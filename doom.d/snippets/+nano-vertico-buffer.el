;;; ~/.doom.d/snippets/+nano-vertico-buffer.el -*- lexical-binding: t -*-
;; Nicolas .P Rougier emacs configuration - mini-frame configuration
;; ---------------------------------------------------------------------

;; See https://gist.github.com/rougier/126e358464e12aa28fac5b4f3dd5eb9c

(defvar +nano-vertico-buffer-prefix "Minibuffer"
  "The prefix name of the vertico buffer.")

(defvar +nano-vertico-buffer-window-parameters
  '((+nano-vertico-buffer . t))
  "The default list of window parameters to add to the vertico buffer window.")

;;;
;;; Update the prefix string based on the commands ran
;;;

(defadvice! +nano-vertico-buffer--helpful-prompt-prefix (fn &rest args)
  "Set the minibuffer prefix to the thing being described."
  :around 'helpful--read-symbol
  (when-let* ((helpful-prompt (nth 0 args))
              (match-index (string-match "\\(?1:[[:word:]]*\\)" helpful-prompt))
              (+nano-vertico-buffer-prefix (match-string match-index helpful-prompt)))
    (apply fn args)))

(defadvice! +nano-vertico-buffer--describe-face-prefix (fn &rest args)
  "Set the minibuffer prefix when describing faces."
  :around 'read-face-name
  (let ((+nano-vertico-buffer-prefix "Face"))
    (apply fn args)))

(defadvice! +nano-vertico-buffer--find-file-prefix (fn &rest args)
  "Set the minibuffer prefix when finding files."
  :around 'projectile-find-file
  :around 'find-file-read-args
  (let ((+nano-vertico-buffer-prefix "File"))
    (apply fn args)))

(defadvice! +nano-vertico-buffer--projectile-switch-project-prefix (fn &rest args)
  "Set the minibuffer prefix when switching projects."
  :around 'projectile-switch-project
  (let ((+nano-vertico-buffer-prefix "Project"))
    (apply fn args)))

(defadvice! +nano-vertico-buffer--switch-buffer-prefix (fn &rest args)
  "Set the minibuffer prefix when switching buffers."
  :around 'projectile-switch-to-buffer
  :around '+vertico/switch-workspace-buffer
  (let ((+nano-vertico-buffer-prefix "Buffer"))
    (apply fn args)))

(defadvice! +nano-vertico-buffer--org-roam-find-prefix (fn &rest args)
  "Set the minibuffer prefix when looking up org roam files."
  :around 'org-roam-node-find
  (let ((+nano-vertico-buffer-prefix "Roam (Find)"))
    (apply fn args)))

(defadvice! +nano-vertico-buffer--org-roam-capture-prefix (fn &rest args)
  "Set the minibuffer prefix when creating a new org roam file."
  :around 'org-roam-capture
  (let ((+nano-vertico-buffer-prefix "Roam (New)"))
    (apply fn args)))

(defadvice! +nano-vertico-buffer--project-search-prefix (fn &rest args)
  "Set the minibuffer prefix when searching within a project."
  :around '+default/search-project
  (let ((+nano-vertico-buffer-prefix "Search (Project)"))
    (apply fn args)))

(defadvice! +nano-vertico-buffer--buffer-search-prefix (fn &rest args)
  "Set the minibuffer prefix when searching within a buffer."
  :around '+default/search-buffer
  (let ((+nano-vertico-buffer-prefix "Search (Buffer)"))
    (apply fn args)))

;; Prefix/Affix the current candidate. From
;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
(defun minibuffer-format-candidate (orig cand prefix suffix index start)
  (let ((prefix (if (= vertico--index index)
                    "  " "   ")))
    (funcall orig cand prefix suffix index start)))

(advice-add #'vertico--format-candidate
            :around #'minibuffer-format-candidate)

(after! (vertico vertico-buffer)
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
                           '(face (completions-first-difference bold))))))

(after! vertico-buffer
  ;; Don't use evil keybinds or modes within the minibuffer. This makes C-g
  ;; work without having to hit it twice.
  (evil-set-initial-state 'minibuffer-mode 'emacs)

  ;; Control the initial window size of the buffer created for the vertico-buffer
  (set-popup-rule! "*vertico-buffer*"
    :height 0.25
    :side 'bottom
    :vslot 1
    :parameters +nano-vertico-buffer-window-parameters)

  (defun +nano-vertico-buffer--setup ()
    ;; Rename the buffer to indicate what the completion is for
    (rename-buffer (format "%s" +nano-vertico-buffer-prefix))
    ;; The minibuffer is not 'focused', make it appear like it is
    (face-remap-add-relative 'nano-modeline-inactive :background (doom-color 'yellow))
    (face-remap-add-relative 'nano-modeline-inactive-primary :background (doom-color 'yellow))
    (face-remap-add-relative 'nano-modeline-inactive-secondary :background (doom-color 'yellow))
    (face-remap-add-relative 'nano-modeline-inactive-name :background (doom-color 'yellow))

    ;; Make cursor behave as expected
    (setq enable-recursive-minibuffers t)
    (cursor-intangible-mode)

    ;; Set local buffer styles
    (setq-local
     left-fringe-width 1
     right-fringe-width 1
     left-margin-width 1
     right-margin-width 1
     fringes-outside-margins t))
  (advice-add #'vertico-buffer--setup :after #'+nano-vertico-buffer--setup))

(after! marginalia
  (setq truncate-string-ellipsis "…")
  (setq marginalia--ellipsis "…")
  (setq marginalia-align 'right)
  (setq marginalia-align-offset -1))

;; Enable vertico-buffer mode
(vertico-buffer-mode)
;; Enable marginalia mode
(marginalia-mode)

(provide '+nano-vertico-buffer)
