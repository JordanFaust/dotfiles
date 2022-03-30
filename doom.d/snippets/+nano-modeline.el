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
  ;; Show the modeline within mu4e
  :hook (mu4e-headers-mode . +doom-nano-modeline-init-h)
  :config
  (defun +doom-hide-nano-modeline-h ()
    "Add hook that extends hide-mode-line-mode to work for nano headerline."
    (require 'hide-mode-line)
    (setq hide-mode-line-format " ")
    (setq mode-line-format " ")
    (if (and hide-mode-line-mode
             (eq nano-modeline-position 'top))
        (progn
          (add-hook 'after-change-major-mode-hook #'hide-mode-line-reset nil t)
          (setq hide-mode-line--old-format header-line-format
                header-line-format hide-mode-line-format))

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
          (nano-headline--active-status-** (face-remap-add-relative 'nano-modeline-active-status-** 'nano-modeline-visual-bell))
          (nano-headline--active-spacer (face-remap-add-relative 'nano-modeline-active-spacer 'nano-modeline-visual-bell))
          (header-line--active-cookie (face-remap-add-relative 'header-line 'nano-modeline-visual-bell)))
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
                            nano-headline--active-status-**
                            nano-headline--active-spacer
                            header-line--active-cookie)
                      (current-buffer))))

  (defun +nano-modeline-visual-bell-config ()
    "Enable flashing the mode-line on error."
    (setq ring-bell-function #'+nano-modeline-visual-bell-fn
          visible-bell t))

  (+nano-modeline-visual-bell-config)

  ;;--------------------------------------------------------------------------------

  ;;;
  ;;; Window Focus Modeline Updates
  ;;;

  (defun +nano-modeline--get-current-window (&optional frame)
    "Get the current window but should exclude the child windows."
    (if (and (fboundp 'frame-parent) (frame-parent frame))
        (frame-selected-window (frame-parent frame))
      (frame-selected-window frame)))

  (defun +nano-modeline--set-selected-window (&rest _)
    "Set `nano-modeline--selected-window' appropriately."
    (when-let ((win (+nano-modeline--get-current-window)))
      (unless (or (minibuffer-window-active-p win)
                  (and (bound-and-true-p lv-wnd) (eq lv-wnd win)))
        (setq nano-modeline--selected-window win))))

  (defun +nano-modeline--unset-selected-window ()
    "Unset `doom-modeline-current-window' appropriately."
    (setq nano-modeline--selected-window nil))

  (defun +nano-modeline--active ()
    "Check if the current window is the active window."
    (unless (and (bound-and-true-p mini-frame-frame)
                 (and (frame-live-p mini-frame-frame)
                      (frame-visible-p mini-frame-frame)))
      (and nano-modeline--selected-window
           (eq (+nano-modeline--get-current-window) nano-modeline--selected-window))))

  (add-hook 'pre-redisplay-functions #'+nano-modeline--set-selected-window)


  (defvar +nano-modeline--remap-face-cookie nil)
  (defun +nano-modeline--focus()
    "Focus mode-line."
    (when +nano-modeline--remap-face-cookie
      (require 'face-remap)
      (face-remap-remove-relative +nano-modeline--remap-face-cookie)))
  (defun +nano-modeline--unfocus()
    "Unfocus mode-line"
    (setq +nano-modeline--remap-face-cookie
          (face-remap-add-relative 'header-line 'nano-modeline-inactive)))
  (with-no-warnings
    (if (boundp 'after-focus-change-function)
        (progn
          (defun +nano-modeline--focus-change (&rest _)
            (if (frame-focus-state)
                (+nano-modeline--focus)
              (+nano-modeline--unfocus)))
          (advice-add #'handle-switch-frame :after #'+nano-modeline--focus-change)
          (add-function :after after-focus-change-function #'+nano-modeline--focus-change))
      (progn
        (add-hook 'focus-in-hook #'+nano-modeline--focus)
        (add-hook 'focus-out-hook #'+nano-modeline--unfocus))))
  ;; (add-hook 'after-make-frame-functions #'+nano-modeline--set-selected-window)
  ;; (add-hook 'buffer-list-update-hook #'+nano-modeline--focus-change)
  ;; (add-hook 'window-configuration-change-hook #'+nano-modeline--focus-change)
  ;; (add-hook 'window-selection-change-functions #'+nano-modeline--focus-change)
  ;; (add-hook 'exwm-workspace-switch-hook #'+nano-modeline--set-selected-window)
  ;; (with-no-warnings
  ;;   (if (boundp 'after-focus-change-function)
  ;;       (progn
  ;;         (defun +nano-modeline-refresh-frame ()
  ;;           (setq nano-modeline--selected-window nil)
  ;;           (cl-loop for frame in (frame-list)
  ;;                    if (eq (frame-focus-state frame) t)
  ;;                    return (setq nano-modeline--selected-window
  ;;                                 (+nano-modeline--get-current-window frame)))
  ;;           (set-face-background 'header-line (if (+nano-modeline--active) (doom-color 'yellow) (doom-color 'fg)))
  ;;           (force-mode-line-update))
  ;;         (add-function :after after-focus-change-function #'+nano-modeline-refresh-frame))
  ;;     (progn
  ;;       (add-hook 'focus-in-hook #'+nano-modeline--set-selected-window)
  ;;       (add-hook 'focus-out-hook #'+nano-modeline--unset-selected-window))))

  ;; ---------------------------- PATCH ------------------------------------
  ;; See https://github.com/rougier/nano-modeline/pull/23/files

  (defface nano-modeline-active-spacer
    '((t (:inherit nano-modeline-active)))
    "Modeline face for active modeline spacing element"
    :group 'nano-modeline-active)

  (defface nano-modeline-inactive-spacer
    '((t (:inherit nano-modeline-inactive)))
    "Modeline face for inactive spacing element"
    :group 'nano-modeline-inactive)

  (defun nano-modeline-render (prefix name primary secondary &optional status)
    "Compose a string with provided information"
    (let* ((window (get-buffer-window (current-buffer)))
           (name-max-width (- (window-body-width)
                              1
                              (length prefix)
                              1
                              (length primary)
                              5
                              (length secondary)
                              1))
           (name (if (and (stringp name) (> (length name) name-max-width))
                     (format "%s…" (substring name 0 (- name-max-width 1)))
                   name))
           (status (or status (nano-modeline-status)))
           (active (eq window nano-modeline--selected-window))
           (prefix (or prefix (cond ((eq status 'read-only)  "RO")
                                    ((eq status 'read-write) "RW")
                                    ((eq status 'modified)   "**")
                                    (t                       "--"))))

           (prefix-face (cond ((eq status 'read-only) (if active
                                                          'nano-modeline-active-status-RO
                                                        'nano-modeline-inactive-status-RO))
                              ((eq status 'modified) (if active
                                                         'nano-modeline-active-status-**
                                                       'nano-modeline-inactive-status-**))
                              ((eq status 'read-write) (if active
                                                           'nano-modeline-active-status-RW
                                                         'nano-modeline-inactive-status-RW))
                              ((facep status) status)
                              ((listp status) (if active (car status)
                                                (cadr status)))
                              (t (if active 'nano-modeline-active
                                   'nano-modeline-inactive))))
           (left (concat (if (stringp prefix)
                             (concat
                              (propertize (if (window-dedicated-p) "[" " ")
                                          'face `(:inherit ,prefix-face))
                              (propertize (format "%s" prefix)
                                          'face `(:inherit ,prefix-face))
                              (propertize (if (window-dedicated-p) "]" " ")
                                          'face `(:inherit ,prefix-face))))
                         (propertize " " 'display `(raise ,nano-modeline-space-top)
                                     'face (if active 'nano-modeline-active-spacer
                                             'nano-modeline-inactive-spacer))
                         (propertize name 'face (if active 'nano-modeline-active-name
                                                  'nano-modeline-inactive-name))
                         (when (length name)
                           (propertize " " 'face (if active 'nano-modeline-active-spacer
                                                   'nano-modeline-inactive-spacer)))
                         (propertize primary 'face (if active 'nano-modeline-active-primary
                                                     'nano-modeline-inactive-primary))))
           (right (concat (propertize secondary 'face (if active 'nano-modeline-active-secondary
                                                        'nano-modeline-inactive-secondary))
                          (propertize " " 'display `(raise ,nano-modeline-space-bottom)
                                      'face (if active 'nano-modeline-active-spacer
                                              'nano-modeline-inactive-spacer))))
           (right-len (length (format-mode-line right))))
      (concat
        left
        (propertize " " 'display `(space :align-to (- right ,(- right-len 0)))
                    'face (if active 'nano-modeline-active-spacer
                            'nano-modeline-inactive-spacer))
        right)))

  ;;
  ;; Anzu Completions
  ;;

  (defun +nano-modeline-anzu-mode-p ()
    (and (bound-and-true-p anzu--state)
         (not (bound-and-true-p iedit-mode))))

  (defun +nano-modeline--anzu-secondary ()
    "Show the match index and total number thereof.
  Requires `anzu', also `evil-anzu' if using `evil-mode' for compatibility with
  `evil-search'."
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
              (format " %s/%d " here total))))))

  (defun +nano-modeline-anzu-mode ()
    (let ((buffer-name (format-mode-line "%b"))
          (branch      (nano-modeline-vc-branch))
          (matches     (+nano-modeline--anzu-secondary)))
      (nano-modeline-render nil
                            buffer-name
                            (if branch (concat "(" branch ")") "")
                            matches)))


  (pushnew! nano-modeline-mode-formats
            (list 'anzu-mode
                  :mode-p '+nano-modeline-anzu-mode-p
                  :format '+nano-modeline-anzu-mode))

  ;;
  ;; Evil Substitution
  ;;

  (defun +nano-modeline-evil-substitute-mode-p ()
    "Return non-nil when an evil substitution is performed"
    (when (and (bound-and-true-p evil-local-mode)
               (or (assq 'evil-ex-substitute evil-ex-active-highlights-alist)
                   (assq 'evil-ex-global-match evil-ex-active-highlights-alist)
                   (assq 'evil-ex-buffer-match evil-ex-active-highlights-alist)))
      't))

  (defun +nano-modeline--evil-substitute-secondary ()
    "Show number of matches for evil-ex substitutions and highlights in real time."
    (propertize
     (let ((range (if evil-ex-range
                      (cons (car evil-ex-range) (cadr evil-ex-range))
                    (cons (line-beginning-position) (line-end-position))))
           (pattern (car-safe (evil-delimited-arguments evil-ex-argument 2))))
       (if pattern
           (format " %s matches " (how-many pattern (car range) (cdr range)))
         " - "))))

  (defun +nano-modeline-evil-substitute-mode ()
    "Format for the modeline in evil substitute mode."
    (let ((buffer-name (format-mode-line "%b"))
          (branch      (nano-modeline-vc-branch))
          (matches     (+nano-modeline--evil-substitute-secondary)))
      (message "rendering evil substitute")
      (nano-modeline-render nil
                            buffer-name
                            (if branch (concat "(" branch ")") "")
                            matches)))

  (pushnew! nano-modeline-mode-formats
            (list 'evil-substitute
                  :mode-p '+nano-modeline-evil-substitute-mode-p
                  :format '+nano-modeline-evil-substitute-mode)))
