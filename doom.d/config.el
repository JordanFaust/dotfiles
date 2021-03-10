;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Jordan Faust"
      user-mail-address "jfaust47@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "Source Code Pro for Powerline" :size 20 :weight 'semi-bold))
;; (setq doom-font (font-spec :family "Iosevka Semibold" :size 20 :weight 'semibold))
(setq doom-font (font-spec :family "JetBrains Mono" :size 18 :weight 'bold))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-rouge)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;;
;; Theme Config
;;

(after! doom-themes
  :config
  (setq doom-rouge-brighter-tabs t
        doom-rouge-brighter-comments t
        doom-rouge-padded-modeline t))

(after! doom-modeline
  (setq doom-modeline-bar-width 10
        doom-modeline-height 40
        doom-modeline-buffer-file-name-style 'relative-to-project)
  (doom-modeline-def-modeline 'jfaust
    '(bar matches buffer-info buffer-position word-count selection-info)
    '(misc-info persp-name grip gnus github debug lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker "  "))
  (defun setup-custom-doom-modeline ()
    (doom-modeline-set-modeline 'jfaust 'default))
  (add-hook 'doom-modeline-mode-hook 'setup-custom-doom-modeline))

(after! centaur-tabs
  :config
  (setq centaur-tabs-height 48
        ;; highlight bar
        centaur-tabs-set-bar 'under
        x-underline-at-descent-line t
        ;; close/modified overrides
        centaur-tabs-left-edge-margin nil
        centaur-tabs-set-modified-marker t
        centaur-tabs-close-button "✕"
        centaur-tabs-modified-marker "⬤"
        ;; icon config
        centaur-tabs-set-icons t
        centaur-tabs-gray-out-icons 'buffer
        all-the-icons-color-icons nil
        centaur-tabs-plain-icons t
        centaur-tabs-icon-scale-factor 0.8)
  (centaur-tabs-headline-match)
  (centaur-tabs-group-by-projectile-project))

;;
;; Theme Customization
;;

(custom-theme-set-faces! 'doom-rouge
  `(ivy-posframe :background ,(doom-darken "#0E131D"  0.1)))

;;
;; LSP Configuration
;;

(after! lsp-mode
  :config
  ;; Limit the width of the completion tooltip to allow room for function documentation
  (setq company-tooltip-maximum-width 70))

;;
;; Project Configuration
;;

(after! projectile
  :config
  ;; Prevent projects within the following directory from becoming projectile projects
  ;; * ~/ws/go/pkg/ - Go project dependencies (code navigation jumps)
  ;; * ~/.rustup/ - Rust project dependencies (code navigation jumps)
  ;; * ~/.gems/ - Ruby project dependencies (code navigation jumps)
  ;; * ~/.emacs.d/.local
  (setq projectile-ignored-projects '("/tmp" "~/.rustup/" "~/.gem/" "~/ws/go/pkg/" "~/.emacs.d/.local/"))
  (defun projectile-ignored-project-regexp-function (project-root)
    (cl-loop for project in projectile-ignored-projects
             ;; Ignore the home directory
             when (and (f-descendant-of? project-root (expand-file-name project))
                       (string-match (getenv "HOME") (expand-file-name project)))
             return 't))
  (setq projectile-ignored-project-function #'projectile-ignored-project-regexp-function)
  ;; Ignore the following directories when doing project searches
  ;; * .bundle - Ignore local dependency files in project search and don't cache opened buffers
  (add-to-list 'projectile-globally-ignored-directories ".bundle")
  (add-to-list 'projectile-globally-ignored-directories ".yardoc"))

(after! ivy-posframe
  ;; Force project and symbol search to a the top posframe
  ;; Explicitly set the width of the posframe to prevent possibly violent
  ;; resizing as it async processes results and displays them
  (setf (alist-get t ivy-posframe-display-functions-alist)
        #'ivy-posframe-display-at-frame-top-center)
  (setf (alist-get 'swiper ivy-posframe-display-functions-alist)
        #'ivy-posframe-display-at-frame-top-center)
  (setf (alist-get 'counsel-rg ivy-posframe-display-functions-alist)
        #'ivy-posframe-display-at-frame-top-center)
  (setq ivy-posframe-width 120)
  (setq posframe-arghandler
        (lambda (buffer-or-name key value)
          (or (and (eq key :lines-truncate)
                   (string-match-p
                    "ivy\\|counsel"
                    (if (stringp buffer-or-name)
                        buffer-or-name
                      (buffer-name (get-buffer buffer-or-name))))
                   t)
              value)))
  (setq ivy-posframe-border-width 5
        ivy-posframe-parameters (append ivy-posframe-parameters '((left-fringe . 5)
                                                                  (right-fringe . 5)))))

(after! counsel
  ;; Fix improper handling of error codes from ripgrep on Linux
  (setq counsel-rg-base-command "rg -M 240 --with-filename --no-heading --line-number --color never %s || true"))

;;
;; Neotree Configuration
;;

(after! neotree
  :config
  (defun doom-neo--longest-line ()
    "Calculates the longest line in the neotree buffer"
    (with-current-buffer (neo-global--get-buffer)
      (let ((longest-line 0))
        (save-excursion
          (goto-char (point-min))
          (while (not(eobp))
            (setq longest-line (max longest-line (- (line-end-position) (line-beginning-position))))
            (forward-line)))
        longest-line)))

  (defun neo-window-size-change-function (_)
    "Dynamically set the width of the NEOTREE buffer within the configured bounds"
    (let ((neo-window (neo-global--get-window)))
      (unless (null neo-window)
        ;; Dynamically set the window width within the allowed bounds
        (neo-global--set-window-width
         (let ((width (doom-neo--longest-line)))
           ;; Add 10 to the total width to account for difference in ligature width
           ;; in the calculation
           (pcase width
             ;; Don't allow the window width to be smaller than 30
             ((guard (< width 25)) 25)
             ;; Don't allow the window width to be greater than 55
             ((guard (> width 55)) 55)
             ;; Return the current width if within bounds
             (_ (+ width 1))))))))

  (add-to-list 'window-size-change-functions #'neo-window-size-change-function))

(load! "+ruby")
(load! "+functions")
(load! "+bindings")
