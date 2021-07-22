;;; doom-valley-theme.el --- ported from Rouge Theme -*- lexical-binding: t; no-byte-compile: t; -*-
(require 'doom-themes)

;;
(defgroup doom-valley-theme nil
  "Options for the `doom-valley' theme."
  :group 'doom-themes)

(defcustom doom-valley-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-valley-theme
  :type 'boolean)

(defcustom doom-valley-brighter-tabs t
  "If non-nil, tabs will a more vivid background color."
  :group 'doom-valley-theme
  :type 'boolean)

(defcustom doom-valley-comment-bg doom-valley-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-valley-theme
  :type 'boolean)

(defcustom doom-valley-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-valley-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-valley
  "A dark theme ported from VS Code's Rouge."

  ;; name        default   256       16
  ((bg         '("#172030" nil       nil            )) ;; modified
   (bg-alt     '("#172030" nil       nil            ))
   (base0      '("#070A0E" "black"   "black"        ))
   (base1      '("#0E131D" "#1e1e1e" "brightblack"  ))
   (base2      '("#151D2B" "#2e2e2e" "brightblack"  ))
   (base3      '("#1F2A3F" "#262626" "brightblack"  ))
   (base4      '("#5D636E" "#3f3f3f" "brightblack"  ))
   (base5      '("#64727d" "#64727d" "brightblack"  ))
   (base6      '("#e95e6c" "#6b6b6b" "brightblack"  ))
   (base7      '("#E8E9EB" "#979797" "brightblack"  ))
   (base8      '("#F0F4FC" "#dfdfdf" "white"        ))
   (fg         '("#FAFFF6"    "#bbb"    "white"        ))
   (fg-alt     '("#A7ACB9" "#bfbfbf" "brightwhite"  ))

   (grey       base5)
   (red        '("#e95e6c" "#c6797e" "red"          ))
   (light-red  '("#ed6284" "#DB6E8F" "red"          ))
   (orange     '("#ff996b" "#eabe9a" "brightred"    ))
   (green      '("#00954c" "#A3B9A4" "green"        ))
   (teal       '("#00b490" "#7ea9a9" "brightgreen"  ))
   (yellow     '("#ffd081" "#F7E3AF" "yellow"       ))
   (blue       '("#006488" "#6e94b9" "brightblue"   ))
   (dark-blue  '("#006488" "#1E6378" "blue"         ))
   (magenta    '("#BD99FF" "#b18bb1" "magenta"      ))
   (salmon     '("#F9B5AC" "#F9B5AC" "orange"       ))
   (violet     '("#BD99FF" "#5D80AE" "brightmagenta"))
   (cyan       '("#24D1E7" "#88C0D0" "brightcyan"   ))
   (dark-cyan  '("#24D1E7" "#507681" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      base6)
   (vertical-bar   (doom-darken base1 0.2))
   (selection      base4)
   (builtin        (doom-lighten red 0.2))
   (comments       grey)
   (doc-comments   green)
   (constants      blue)
   (functions      yellow)
   (keywords       magenta)
   (methods        light-red)
   (operators      magenta)
   (type           red)
   (strings        green)
   (variables      red)
   (numbers        orange)
   (region         base4)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-pad
    (when doom-valley-padded-modeline
      (if (integerp doom-valley-padded-modeline) doom-valley-padded-modeline 4)))

   (tabs-bg (if doom-valley-brighter-tabs (doom-darken base6 0.15) bg))
   (tabs-fg (if doom-valley-brighter-tabs base8 fg))
   (tabs-bar-bg (if doom-valley-brighter-tabs bg red))
   (tabs-marker (if doom-valley-brighter-tabs base8 highlight))

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
   (lazy-highlight :background base4)
   ((line-number &override) :foreground (doom-lighten 'base5 0.2))
   ((line-number-current-line &override) :foreground base7)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground highlight)
   (tooltip :background base3 :foreground fg-alt)
   (vertical-border :foreground base6)

   ;;;; centuar-tabs
   (centaur-tabs-selected :foreground tabs-fg :background tabs-bg)
   (centaur-tabs-selected-modified :foreground tabs-fg :background tabs-bg)
   (centaur-tabs-unselected-modified :foreground tabs-fg :background bg)
   (centaur-tabs-active-bar-face :background tabs-bar-bg)
   (centaur-tabs-modified-marker-selected :inherit 'centaur-tabs-selected :foreground tabs-marker)
   (centaur-tabs-modified-marker-unselected :inherit 'centaur-tabs-unselected :foreground tabs-marker)

   ;;;; which-key
   (which-key-command-description-face :foreground cyan)

   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-project-root-dir :foreground base6)
   ;;;; doom-themes
   (doom-themes-treemacs-root-face :foreground highlight :weight 'ultra-bold :height 1.2)
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
   (ivy-current-match :background base3)
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
   ((paren-face-match &override) :foreground red :background base3 :weight 'ultra-bold)
   ((paren-face-mismatch &override) :foreground base3 :background red :weight 'ultra-bold)
   ;;;; neotree
   (neo-root-dir-face :foreground red)
   ;;;; org <built-in>
   (org-hide :foreground hidden)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))
   (solaire-hl-line-face :background base3)
   ;;;; treemacs
   (treemacs-root-face :foreground highlight :weight 'ultra-bold :height 1.2)
   (treemacs-directory-face :foreground highlight)
   ;;;; vimish-fold
   ((vimish-fold-overlay &override) :inherit 'font-lock-comment-face :background base3 :weight 'light)
   ((vimish-fold-fringe &override)  :foreground teal))


  ;;;; Base theme variable overrides-
  ())

;;; doom-valley-theme.el ends here
