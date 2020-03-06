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
(setq doom-font (font-spec :family "Iosevka Semibold Extended" :size 20 :weight 'semibold))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord)

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

(after! ivy-posframe
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center))))


(after! doom-modeline
  (setq doom-modeline-bar-width 10
        doom-modeline-buffer-file-name-style 'relative-to-project)
  (doom-modeline-def-modeline 'jfaust
    '(bar workspace-name window-number matches buffer-info buffer-position word-count parrot selection-info)
    '(misc-info persp-name grip gnus github debug lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker))
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

;; Use the otf-fira-code-symbol font from yay for the font for pretty code
(setq +pretty-code-iosevka-font-name "Iosevka Nerd Font Mono"
      +pretty-code-fira-font-name "Fira Code Symbol")

(load! "+functions")
(load! "themes/doom-rouge-theme")
(load! "+bindings")
;; FIXME
(setq doom-rouge-brighter-tabs t)
(setq doom-rouge-brighter-comments t)
;; (setq doom-rouge-brighter-modeline t)
(setq doom-rouge-padded-modeline t)
(load-theme 'doom-rouge)
