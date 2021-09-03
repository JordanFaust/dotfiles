;;; ~/.doom.d/snippets/+nano-modeline.el -*- lexical-binding: t -*-

(use-package! nano-modeline
  :config
  ;; Set floating look to frames and the modeline
  (setq default-frame-alist
        (append (list
                 '(min-height . 1)  '(height . 45)
                 '(min-width  . 1)  '(width  . 81)
                 '(vertical-scroll-bars . nil)
                 '(internal-border-width . 32)
                 '(left-fringe . 0)
                 '(right-fringe . 0)
                 '(tool-bar-lines . 0)
                 '(menu-bar-lines . 0))))
  ;; Add window dividers for keeping floating headline when using virtical splits
  (setq window-divider-default-right-width 32)
  (setq window-divider-default-places 'right-only)
  (window-divider-mode 1))

(defun hide-nano-modeline-h ()
  "Add hook that extends hide-mode-line-mode to work for nano headerline."
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

(defface nano-modeline-visual-bell '((t :inherit error))
  "Face to use for the mode-line when `+nano-modeline-visual-bell-config' is used."
  :group 'nano-modeline)

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

(after! nano-modeline
  (defun nano-modeline-compose (status name primary secondary)
    "Compose a string with provided information"
    (let* ((char-width    (window-font-width nil 'header-line))
           (window        (get-buffer-window (current-buffer)))
           (active        (eq window nano-modeline--selected-window))
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
           ;; Handle the header-line height being set as an integer or a floating point
           ;; scaler
           (char-width-multiple (if (floatp header-line-height)
                                    header-line-height
                                  (/ header-line-height 100.0)))
           (available-width (- (window-total-width)
                               (length prefix) (length left) (length right)
                               (floor (* char-width-multiple char-width))))
           (available-width (max 1 available-width)))
      (concat prefix
              left
              (propertize (make-string available-width ?\ )
                          'face (if active 'nano-modeline-active
                                  'nano-modeline-inactive))
              (propertize right 'face (if active 'nano-modeline-active-secondary
                                        'nano-modeline-inactive-secondary)))))

  ;; Keep the org clock info in the modeline
  (defun nano-modeline-default-mode ()
    (let ((buffer-name (format-mode-line "%b"))
          (mode-name   (nano-mode-name))
          (branch      (vc-branch)))
      (if (string-match " \\*NeoTree\\*" buffer-name)
          (nano-modeline-compose (nano-modeline-status) buffer-name "" "")
        (nano-modeline-compose (nano-modeline-status)
                               buffer-name
                               (concat "(" mode-name
                                       (if branch (concat ", "
                                                          (propertize branch 'face 'italic)))
                                       ")" )
                               org-mode-line-string))))

  ;; (add-hook 'neotree-mode-hook #'hide-nano-modeline-h)
  (+nano-modeline-visual-bell-config))
