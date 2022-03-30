;;; ~/.doom.d/snippets/+nano-minibuffer.el -*- lexical-binding: t -*-
;; Nicolas .P Rougier emacs configuration - mini-frame configuration
;; ---------------------------------------------------------------------
(require 'vertico)
(require 'marginalia)
(require 'mini-frame)

;; Used to indicate the overlay should be disabled
(defvar +nano-minibuffer--disable-overlay nil)

(defun minibuffer-setup ()
  ;; This prevents the header line to spill over second line
  ;; (let ((inhibit-message t))
  ;;   (toggle-truncate-lines 1))

  (unless +nano-minibuffer--disable-overlay
    (setq enable-recursive-minibuffers t)

    ;; This allows to have a consistent full width (fake) header like
    (setq display-table (make-display-table))
    (set-display-table-slot display-table 'truncation (make-glyph-code ?\ ))
    (set-display-table-slot display-table 'wrap (make-glyph-code ?\ ))
    (setq buffer-display-table display-table)

    (cursor-intangible-mode)
    (let* ((left  (concat (propertize " "
                                      ;; 'face '(nano-modeline-active-status-**)
                                      'face 'nano-modeline-active-status-**
                                      'display '(raise +0.20))
                          (propertize " M-x "
                                      'face 'nano-modeline-active-status-**)
                          (propertize " "
                                      'face 'nano-modeline-active-primary
                                      'display '(raise -0.30))))
           (right (propertize "C-g: abort "
                              'face '(:inherit (nano-modeline-active-primary)
                                      :weight light)))
           (spacer (propertize (make-string (- (window-width)
                                               (length left)
                                               (length right)
                                               1) ?\ )
                               'face 'nano-modeline-active))
           (header (concat left spacer right))
           (overlay (make-overlay (+ (point-min) 0) (+ (point-min) 0))))
      (overlay-put overlay 'before-string
          (concat
           (propertize " " 'display header)
           "\n"
           ;; This provides a vertical gap (half a line) above the prompt.
           (propertize " " 'face `(:extend t)
                       'display '(raise .33)
                       'read-only t 'cursor-intangible t))))))

(add-hook 'minibuffer-setup-hook #'minibuffer-setup)

;; Prefix/Affix the current candidate. From
;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
(defun minibuffer-format-candidate (orig cand prefix suffix index _start)
  (let ((prefix (if (= vertico--index index)
                    "  " "   ")))
    (funcall orig cand prefix suffix index _start)))

(advice-add #'vertico--format-candidate
            :around #'minibuffer-format-candidate)

(after! vertico
  (setq completion-styles '(basic substring partial-completion flex))
  (setq vertico-count 10)
  (setq vertico-count-format nil)
  (setq vertico-grid-separator
        #("  |  " 2 3 (display (space :width (1))
                               face (:background "#ECEFF1"))))
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))

  (defun vertico--prompt-selection ()
    "Highlight the prompt"
    (let ((inhibit-modification-hooks t))
      (set-text-properties (minibuffer-prompt-end) (point-max)
                           '(face (highlight bold))))))

(after! marginalia
  (setq truncate-string-ellipsis "…")
  (setq marginalia--ellipsis "…")
  (setq marginalia-align 'right)
  (setq marginalia-align-offset -1)

  (pushnew! marginalia-command-categories
            '(+default/find-file-under-here . file)
            '(doom/find-file-in-emacsd . project-file)
            '(doom/find-file-in-other-project . project-file)
            '(doom/find-file-in-private-config . file)
            '(doom/describe-active-minor-mode . minor-mode)
            '(flycheck-error-list-set-filter . builtin)
            '(persp-switch-to-buffer . buffer)
            '(projectile-find-file . project-file)
            '(projectile-recentf . project-file)
            '(projectile-switch-to-buffer . buffer)
            '(projectile-switch-project . project-file)))


(after! mini-frame
  (set-face-background 'child-frame-border (doom-color 'bg))
  (setq mini-frame-default-height vertico-count)
  (setq mini-frame-create-lazy t)
  (setq mini-frame-show-parameters 'mini-frame-dynamic-parameters)
  (setq mini-frame-ignore-commands
        '("edebug-eval-expression"
          debugger-eval-expression
          evil-ex))
  (setq mini-frame-internal-border-color (doom-color 'yellow))
  (setq mini-frame-resize t)
  (setq mini-frame-resize-min-height 3)

  (defun mini-frame-dynamic-parameters ()
    (let* ((edges       (window-pixel-edges))      ;; (left top right bottom)
           (body-edges  (window-body-pixel-edges)) ;; (left top right bottom)
           (left   (nth 0 edges))      ;; Take margins into account
           (top    (nth 1 edges)) ;; Drop header line
           (right  (nth 2 edges))      ;; Take margins into account
           (bottom (nth 3 body-edges)) ;; Drop header line
           ;; Figure out how to get this from mini buffer
           (left   (if (eq left-fringe-width 0)
                       (nth 0 edges)
                     (- (nth 0 edges) (frame-parameter nil 'left-fringe))))
           (right  (if (eq right-fringe-width 0)
                       right
                     (+ right (frame-parameter nil 'right-fringe))))
           (fringe-left 0)
           (fringe-right 0)
           (border 1)
           ;; (width (- (frame-pixel-width) (* 2 (+ fringe border))))
           (width (- right left fringe-left fringe-right (* 0 border)))
           (y (- top border)))
      `((left . ,left)
        (top . ,y)
        (alpha . 1.0)
        (width . (text-pixels . ,width))
        (left-fringe . ,fringe-left)
        (right-fringe . ,fringe-right)
        (child-frame-border-width . ,border)
        (internal-border-width . ,border)
        (right-divider-width . 0)
        (scroll-bar-height . 0)
        (border-color . ,(doom-color 'red))
        (foreground-color . ,(doom-color 'yellow))
        (background-color . ,(doom-color 'bg)))))

  ;;;
  ;;; Disable overlay for evil ex commands
  ;;;

  (defadvice! +nano-minibuffer--disable-overlay-evil-ex (&rest _)
    :before 'evil-ex
    (setq +nano-minibuffer--disable-overlay t))

  (defadvice! +nano-minibuffer--enable-overlay-after-evil-ex (&rest _)
    "Re-enable the minibuffer overlay after an evil ex command has completed or has been aborted."
    :after 'evil-ex-execute
    :after 'evil-ex-abort
    (setq +nano-minibuffer--disable-overlay nil)))

(vertico-mode)
(marginalia-mode)
(mini-frame-mode t)

(provide '+nano-minibuffer)
