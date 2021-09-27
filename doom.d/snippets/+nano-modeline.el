;;; ~/.doom.d/snippets/+nano-modeline.el -*- lexical-binding: t -*-

;;;
;;; Packages
;;;

  ;; Extensions
(use-package! anzu
  :after-call isearch-mode
  :config
  ;; We manage our own modeline segments
  (setq anzu-cons-mode-line-p nil)
  ;; Ensure anzu state is cleared when searches & iedit are done
  (add-hook 'iedit-mode-end-hook #'anzu--reset-status)
  (advice-add #'evil-force-normal-state :before #'anzu--reset-status)
  ;; Fix matches segment mirroring across all buffers
  (mapc #'make-variable-buffer-local
        '(anzu--total-matched
          anzu--current-position
          anzu--state
          anzu--cached-count
          anzu--cached-positions anzu--last-command
          anzu--last-isearch-string anzu--overflow-p)))

(use-package! evil-anzu
  :when (featurep! :editor evil)
  :after-call evil-ex-start-search evil-ex-start-word-search evil-ex-search-activate-highlight
  :config (global-anzu-mode +1))

(use-package! nano-modeline
  ;; Hide both the header-line and mode-line for the dashboard mode
  :hook (+doom-dashboard-mode . +doom-hide-nano-modeline-h)
  ;; Don't explicitly require and start the modeline until we swap to a file
  :hook (doom-first-file . +doom-nano-modeline-init-h)
  :config
  (defun +doom-hide-nano-modeline-h ()
    "Add hook that extends hide-mode-line-mode to work for nano headerline."
    (require 'hide-mode-line)
    (setq hide-mode-line-format " ")
    (message "hide-mode-line-mode %s" hide-mode-line-mode)
    (if (and hide-mode-line-mode
             (eq nano-modeline-position 'top))
        (progn
          (add-hook 'after-change-major-mode-hook #'hide-mode-line-reset nil t)
          (message "before header-line-format %s" header-line-format)
          (setq hide-mode-line--old-format header-line-format
                header-line-format hide-mode-line-format)
          (message "hide-mode-line--old-format %s" hide-mode-line--old-format)
          (message "header-line-format %s" header-line-format))

      (remove-hook 'after-change-major-mode-hook #'hide-mode-line-reset t)
      (setq header-line-format hide-mode-line--old-format
            hide-mode-line--old-format " "))

    (force-mode-line-update))
  (defun +doom-nano-modeline-init-h ()
    (require 'nano-modeline)
    (nano-modeline)))

(after! nano-modeline
  (defun +nano-modeline-visual-bell-fn ()
    "Blink the mode-line red briefly. Set `ring-bell-function' to this to use it."
    (let ((nano-headline--active-cookie (face-remap-add-relative 'nano-modeline-active 'nano-modeline-visual-bell))
          (nano-headline--active-name-cookie (face-remap-add-relative 'nano-modeline-active-name 'nano-modeline-visual-bell))
          (nano-headline--active-primary-cookie (face-remap-add-relative 'nano-modeline-active-primary 'nano-modeline-visual-bell))
          (nano-headline--active-secondary-cookie (face-remap-add-relative 'nano-modeline-active-secondary 'nano-modeline-visual-bell))
          (nano-headline--active-status-RO (face-remap-add-relative 'nano-modeline-active-status-RO 'nano-modeline-visual-bell))
          (nano-headline--active-status-RW (face-remap-add-relative 'nano-modeline-active-status-RW 'nano-modeline-visual-bell))
          (nano-headline--active-status-** (face-remap-add-relative 'nano-modeline-active-status-** 'nano-modeline-visual-bell)))
      (force-mode-line-update)
      (run-with-timer 0.15 nil
                      (lambda (cookies buf)
                        (with-current-buffer buf
                          (dolist (cookie cookies)
                            (face-remap-remove-relative cookie))
                          (force-mode-line-update)))
                      (list nano-headline--active-cookie
                            nano-headline--active-name-cookie
                            nano-headline--active-primary-cookie
                            nano-headline--active-secondary-cookie
                            nano-headline--active-status-RO
                            nano-headline--active-status-RW
                            nano-headline--active-status-**)
                      (current-buffer))))

  (defun +nano-modeline-visual-bell-config ()
    "Enable flashing the mode-line on error."
    (setq ring-bell-function #'+nano-modeline-visual-bell-fn
          visible-bell t))

  (defun +nano-modeline--get-current-window (&optional frame)
    "Get the current window but should exclude the child windows."
    (if (and (fboundp 'frame-parent) (frame-parent frame))
        (frame-selected-window (frame-parent frame))
      (frame-selected-window frame)))

  (defvar +nano-modeline-current-window (+nano-modeline--get-current-window))

  (defun +nano-modeline--active ()
    "Whether is an active window."
    (unless (and (bound-and-true-p mini-frame-frame)
                 (and (frame-live-p mini-frame-frame)
                      (frame-visible-p mini-frame-frame)))
      (and +nano-modeline-current-window
           (eq (+nano-modeline--get-current-window) +nano-modeline-current-window))))

  (defun +nano-modeline-set-selected-window (&rest _)
    "Set `doom-modeline-current-window' appropriately."
    (when-let ((win (+nano-modeline--get-current-window)))
      (unless (or (minibuffer-window-active-p win)
                  (and (bound-and-true-p lv-wnd) (eq lv-wnd win)))
        (setq +nano-modeline-current-window win))))

  (defun +nano-modeline-unset-selected-window ()
    "Unset `doom-modeline-current-window' appropriately."
    (setq +nano-modeline-current-window nil))

  (defun nano-modeline-compose (status name primary secondary)
    "Compose a string with provided information"
    (let* ((char-width    (window-font-width nil 'header-line))
           (window        (get-buffer-window (current-buffer)))
           (active        (+nano-modeline--active))
           (space-up       +0.3)
           (space-down     -0.25)
           (prefix (cond ((string= status "RO")
                          (propertize (if (window-dedicated-p)" -- " " RO ")
                                      'face (if active 'nano-modeline-active-status-RO
                                              'nano-modeline-inactive-status-RO)))
                         ((string= status "**")
                          (propertize (if (window-dedicated-p)" -- " " ** ")
                                      'face (if active 'nano-modeline-active-status-**
                                              'nano-modeline-inactive-status-**)))
                         ((string= status "RW")
                          (propertize (if (window-dedicated-p)" -- " " RW ")
                                      'face (if active 'nano-modeline-active-status-RW
                                              'nano-modeline-inactive-status-RW)))
                         (t (propertize status
                                        'face (if active 'nano-modeline-active-status-**
                                                'nano-modeline-inactive-status-**)))))
           (left (concat
                  (propertize " "  'face (if active 'nano-modeline-active
                                           'nano-modeline-inactive)
                              'display `(raise ,space-up))
                  (propertize name 'face (if active 'nano-modeline-active-name
                                           'nano-modeline-inactive-name))
                  (propertize " "  'face (if active 'nano-modeline-active
                                           'nano-modeline-inactive)
                              'display `(raise ,space-down))
                  (propertize primary 'face (if active 'nano-modeline-active-primary
                                              'nano-modeline-inactive-primary))))
           (right (concat secondary " "))
           (header-line-height (face-attribute 'header-line :height))
           (used-space (+ (length prefix) (length left) (length right)))
           (unused-space (- (window-total-width) used-space))
           ;; Get the width difference between the default font width and the header-line font width
           (char-width-delta (/ (float char-width) (float (window-font-width))))
           ;; Get the adjusted window width based on the header-line font width
           (window-total-width-adjusted (/ (window-total-width) char-width-delta))
           ;; Calculate the available center padding space, rounding up
           (available-width (ceiling (- window-total-width-adjusted (float used-space))))
           ;; Ensure available width is a positive number
           (available-width (max 1 available-width)))
      (let ((filler-string (make-string available-width ?\s)))
        (concat prefix
              left
              (propertize filler-string
                          'face (if active 'nano-modeline-active
                                  'nano-modeline-inactive))
              (propertize right 'face (if active 'nano-modeline-active-secondary
                                        'nano-modeline-inactive-secondary))))))

  ;; Keep the org clock info in the modeline
  (defun nano-modeline-default-mode ()
    (let* ((buffer-name (format-mode-line "%b"))
           (mode-name   (nano-mode-name))
           (branch      (vc-branch))
           (position (format-mode-line "%l:%c")))
      (let ((secondary position))
        ;; Handle setting the secondary
        (when org-mode-line-string
          (setq secondary org-mode-line-string))
        (when (+nano-modeline-anzu)
          (setq secondary (+nano-modeline-anzu)))
        (when (+nano-modeline-evil-substitute-p)
          (setq secondary (+nano-modeline-evil-substitute)))
        ;; Handle setting the buffer name
        (when (string-match " \\*Sidebar\\*" buffer-name)
          (setq buffer-name "Dashboard"))
        (if (string-match " \\*NeoTree\\*" buffer-name)
            (nano-modeline-compose (nano-modeline-status) buffer-name "" "")
          (nano-modeline-compose (nano-modeline-status)
                                 buffer-name
                                 (concat "(" mode-name
                                         (if branch (concat ", "
                                                            (propertize branch 'face 'italic)))
                                         ")")
                                 secondary)))))

  (defun nano-modeline-org-clock-mode ()
    (let ((buffer-name (format-mode-line "%b"))
          (mode-name   (nano-mode-name))
          (branch      (vc-branch))
          (position    (format-mode-line "%l:%c")))

      ;; Use the term mode even when clocked in
      ;; TODO fix the check for
      (if (nano-modeline-vterm-mode-p)
          (nano-modeline-term-mode)
        (nano-modeline-compose (nano-modeline-status)
                               buffer-name
                               (concat "(" mode-name
                                       (if branch (concat ", "
                                                          (propertize branch 'face 'italic)))
                                       ")")
                               org-mode-line-string))))

  (defun +nano-modeline-evil-substitute-p ()
    "Return non-nil when an evil substitution is performed"
    (when (and (bound-and-true-p evil-local-mode)
               (or (assq 'evil-ex-substitute evil-ex-active-highlights-alist)
                   (assq 'evil-ex-global-match evil-ex-active-highlights-alist)
                   (assq 'evil-ex-buffer-match evil-ex-active-highlights-alist)))
      't))

  (defun +nano-modeline-evil-substitute ()
    "Show number of matches for evil-ex substitutions and highlights in real time."
    (when (+nano-modeline-evil-substitute-p)
      (propertize
       (let ((range (if evil-ex-range
                        (cons (car evil-ex-range) (cadr evil-ex-range))
                      (cons (line-beginning-position) (line-end-position))))
             (pattern (car-safe (evil-delimited-arguments evil-ex-argument 2))))
         (if pattern
             (format " %s matches " (how-many pattern (car range) (cdr range)))
           " - ")))))

  (defun +nano-modeline-anzu ()
    "Show the match index and total number thereof.
  Requires `anzu', also `evil-anzu' if using `evil-mode' for compatibility with
  `evil-search'."
    (when (and (bound-and-true-p anzu--state)
               (not (bound-and-true-p iedit-mode)))
      (propertize
       (let ((here anzu--current-position)
             (total anzu--total-matched))
         (cond ((eq anzu--state 'replace-query)
                (format " %d replace " anzu--cached-count))
               ((eq anzu--state 'replace)
                (format " %d/%d " here total))
               (anzu--overflow-p
                (format " %s+ " total))
               (t
                (format " %s/%d " here total)))))))

  (add-hook 'after-make-frame-functions #'+nano-modeline-set-selected-window)
  (add-hook 'buffer-list-update-hook #'+nano-modeline-set-selected-window)
  (add-hook 'window-configuration-change-hook #'+nano-modeline-set-selected-window)
  (add-hook 'window-selection-change-functions #'+nano-modeline-set-selected-window)
  (add-hook 'exwm-workspace-switch-hook #'+nano-modeline-set-selected-window)
  (with-no-warnings
    (if (boundp 'after-focus-change-function)
        (progn
          (defun +nano-modeline-refresh-frame ()
            (setq +nano-modeline-current-window nil)
            (cl-loop for frame in (frame-list)
                     if (eq (frame-focus-state frame) t)
                     return (setq +nano-modeline-current-window
                                  (+nano-modeline--get-current-window frame)))
            (force-mode-line-update))
          (add-function :after after-focus-change-function #'+nano-modeline-refresh-frame))
      (progn
        (add-hook 'focus-in-hook #'+nano-modeline-set-selected-window)
        (add-hook 'focus-out-hook #'+nano-modeline-unset-selected-window))))

  ;; (add-hook 'neotree-mode-hook #'hide-nano-modeline-h)
  (+nano-modeline-visual-bell-config))
