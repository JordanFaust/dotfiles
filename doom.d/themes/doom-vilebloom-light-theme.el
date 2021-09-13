
;;; doom-vilebloom-light-light-theme.el --- ported from vilebloom -*- lexical-binding: t; no-byte-compile: t; -*-
(require 'doom-themes)

;;
(defgroup doom-vilebloom-light-theme nil
  "Options for the `doom-vilebloom-light' theme."
  :group 'doom-themes)

(defcustom doom-vilebloom-light-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-vilebloom-light-theme
  :type 'boolean)

(defcustom doom-vilebloom-light-brighter-tabs t
  "If non-nil, tabs will a more vivid background color."
  :group 'doom-vilebloom-light-theme
  :type 'boolean)

(defcustom doom-vilebloom-light-comment-bg doom-vilebloom-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-vilebloom-light-theme
  :type 'boolean)

(defcustom doom-vilebloom-light-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-vilebloom-light-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-vilebloom-light
  "A dark theme inspired by the Vilebloom Keycap set."

  ;; name        default   256       16
  ((bg         '("#FAFFF6" nil       nil)) ;; modified
   ;; (bg-alt     '("#F0F4FC" nil       nil))
   (bg-alt     '("#FAFFF6" nil       nil))
   (base0      '("#E8E9EB" "black"   "black"))
   (base1      '("#64727d" "#1e1e1e" "brightblack"))
   (base2      '("#5D636E" "#2e2e2e" "brightblack"))
   (base3      '("#1F2A3F" "#262626" "brightblack"))
   (base4      '("#151D2B" "#3f3f3f" "brightblack"))
   (base5      '("#0E131D" "#64727d" "brightblack"))
   (base6      '("#ff777a" "#6b6b6b" "brightblack"))
   (base7      '("#24455b" "#979797" "brightblack"))
   (base8      '("#A7ACB9" "#dfdfdf" "white"))
   (fg         '("#152733"    "#bbb"    "white"))
   (fg-alt     '("#152733" "#bfbfbf" "brightwhite"))

   (grey       base5)
   (red        '("#ff777a" "#c6797e" "red"))
   (light-red  '("#ff776d" "#DB6E8F" "red"))
   (orange     '("#f6b77c" "#eabe9a" "brightred"))
   (green      '("#d97a9b" "#A3B9A4" "green"))
   (teal       '("#00b490" "#7ea9a9" "brightgreen"))
   (yellow     '("#ffba95" "#F7E3AF" "yellow"))
   (blue       '("#4684ae" "#6e94b9" "brightblue"))
   (dark-blue  '("#4684ae" "#1E6378" "blue"))
   (magenta    '("#325f7d" "#b18bb1" "magenta"))
   (salmon     '("#F9B5AC" "#F9B5AC" "orange"))
   (violet     '("#325f7d" "#5D80AE" "brightmagenta"))
   (cyan       '("#c7bcba" "#88C0D0" "brightcyan"))
   (dark-cyan  '("#c7bcba" "#507681" "cyan"))

   ;; face categories -- required for all themes
   (highlight      base6)
   (vertical-bar   (doom-darken base1 0.2))
   (selection      base0)
   (builtin        (doom-lighten red 0.2))
   (comments       grey)
   (doc-comments   blue)
   (constants      blue)
   (functions      yellow)
   (keywords       magenta)
   (methods        light-red)
   (operators      magenta)
   (type           red)
   (strings        green)
   (variables      red)
   (numbers        orange)
   (region         base0)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-pad
    (when doom-vilebloom-light-padded-modeline
      (if (integerp doom-vilebloom-light-padded-modeline) doom-vilebloom-padded-modeline 4)))

   (tabs-bg (if doom-vilebloom-light-brighter-tabs (doom-darken base6 0.15) bg))
   (tabs-fg (if doom-vilebloom-light-brighter-tabs base8 fg))
   (tabs-bar-bg (if doom-vilebloom-light-brighter-tabs bg red))
   (tabs-marker (if doom-vilebloom-light-brighter-tabs base8 highlight))

   (modeline-fg     nil)
   (modeline-fg-alt base6)
   (modeline-bg base1)
   (modeline-bg-l `(,(doom-darken (car bg) 0.1) ,@(cdr base0)))
   (modeline-bg-inactive   (doom-darken bg 0.1))
   (modeline-bg-inactive-l `(,(car bg) ,@(cdr base1))))


  ;;;; Base theme face overrides
  (((font-lock-comment-face &override) :slant 'italic)
   ((font-lock-keyword-face &override) :slant 'italic)
   (font-lock-preprocessor-face :foreground magenta :slant 'italic)
   (lazy-highlight :background base0)
   ((line-number &override) :foreground (doom-lighten 'base5 0.2))
   ((line-number-current-line &override) :foreground base7)
   ;; (mode-line
   ;;  :background modeline-bg :foreground modeline-fg
   ;;  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line :background bg)
   ;; (mode-line-inactive
   ;;  :background modeline-bg-inactive :foreground modeline-fg-alt
   ;;  :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-inactive :background bg)
   (mode-line-emphasis :foreground highlight)
   (tooltip :background bg-alt :foreground base4)
   (vertical-border :foreground base6)

   ;;;; centuar-tabs
   (centaur-tabs-selected :foreground tabs-fg :background tabs-bg)
   (centaur-tabs-selected-modified :foreground tabs-fg :background tabs-bg)
   (centaur-tabs-unselected-modified :foreground tabs-fg :background bg)
   (centaur-tabs-active-bar-face :background tabs-bar-bg)
   (centaur-tabs-modified-marker-selected :inherit 'centaur-tabs-selected :foreground tabs-marker)
   (centaur-tabs-modified-marker-unselected :inherit 'centaur-tabs-unselected :foreground tabs-marker)

   ;;; nano-modeline
   (nano-modeline-active :foreground fg :background yellow)
   (nano-modeline-active-name :foreground bg :background yellow :weight 'bold)
   (nano-modeline-active-primary :foreground bg :background yellow)
   (nano-modeline-active-secondary :foreground fg :background yellow)
   (nano-modeline-active-status-RO :foreground bg :background red :weight 'bold)
   (nano-modeline-active-status-RW :foreground bg :background red :weight 'bold)
   (nano-modeline-active-status-** :foreground bg :background red :weight 'bold)
   (nano-modeline-inactive :foreground bg :background base0)
   (nano-modeline-inactive-name :foreground bg :background base0 :weight 'bold)
   (nano-modeline-inactive-primary :foreground bg :background base0)
   (nano-modeline-inactive-secondary :foreground fg :background base0)
   (nano-modeline-inactive-status-RO :foreground bg :background red :weight 'bold)
   (nano-modeline-inactive-status-RW :foreground bg :background red :weight 'bold)
   (nano-modeline-inactive-status-** :foreground bg :background red :weight 'bold)
   (nano-modeline-visual-bell :background red :foreground bg)

   (window-divider :background bg :foreground bg)

   ;;;; mu4e
   (mu4e-attach-number-face :foreground yellow)
   (mu4e-cited-1-face :foreground yellow)
   (mu4e-unread-face :foreground base4 :slant 'normal :weight 'bold)
   (mu4e-header-face :foreground base4 :weight 'light)
   (mu4e-header-highlight-face :background violet :foreground bg :weight 'normal)
   ((mu4e-thread-folding-root-unfolded-face &override) :background (doom-lighten blue 0.4) :weight 'normal)
   (mu4e-thread-folding-root-prefix-face :background 'unspecified)
   ((mu4e-thread-folding-child-face &override) :background (doom-lighten blue 0.8) :weight 'light)

   ;;;; Which Key
   (which-key-key-face :foreground red)
   (which-key-separator-face :foreground green)
   (which-key-note-face :foreground blue)
   (which-key-command-discription-face :foreground blue)
   (which-key-group-description-face :foreground yellow)

   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;;; hl-line
   (hl-line :background (doom-lighten bg 0.1))

   ;;;; doom-modeline
   (doom-modeline-project-root-dir :foreground base6)

   ;;;; doom-themes
   (doom-themes-treemacs-root-face :foreground highlight :weight 'ultra-bold :height 1.5)
   (doom-themes-treemacs-file-face :foreground highlight)

   ;;;; ediff <built-in>
   (ediff-fine-diff-A    :background (doom-darken violet 0.4) :weight 'bold)
   (ediff-current-diff-A :background (doom-darken base0 0.25))

   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   ;;;; highlight-symbol
   (highlight-symbol-face :background region :distant-foreground fg-alt)

   ;;;; highlight-thing
   (highlight-thing :background region :distant-foreground fg-alt)

   ;;;; ivy
   (ivy-current-match :background base0 :foreground base4)
   (ivy-minibuffer-match-face-2 :foreground highlight :weight 'extra-bold)

   ;;;; ivy-posframe
   (ivy-posframe :background bg-alt)
   (ivy-posframe-border :background highlight)

   ;;;; magit
   (magit-diff-hunk-heading           :foreground bg                    :background (doom-blend highlight bg 0.3) :extend t)
   (magit-diff-hunk-heading-highlight :foreground bg                    :background highlight :weight 'bold :extend t)
   (magit-section-heading :foreground highlight)

   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))

   ;;;; mic-paren
   ((paren-face-match &override) :foreground red :background base0 :weight 'ultra-bold)
   ((paren-face-mismatch &override) :foreground base3 :background red :weight 'ultra-bold)

   ;;;; neotree
   (neo-root-dir-face :foreground red)

   ;;;; org <built-in>
   (org-hide :foreground hidden)
   (org-agenda-clocking :background bg)

   ;;;; org roam
   (org-roam-title :foreground yellow)

   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))
   (solaire-hl-line-face :background base0)

   ;;;; treemacs
   (treemacs-root-face :foreground highlight :weight 'ultra-bold :height 1.5)
   (treemacs-directory-face :foreground highlight)
   (treemacs-fringe-indicator-face :background red)

   ;;;; vimish-fold
   ((vimish-fold-overlay &override) :inherit 'font-lock-comment-face :background base3 :weight 'light)
   ((vimish-fold-fringe &override)  :foreground teal))

  ;;;; Base theme variable overrides-
  ())

;;; doom-vilebloom-light-theme.el ends here
