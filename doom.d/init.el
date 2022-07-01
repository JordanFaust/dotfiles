;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load in.
;; Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find information about all of Doom's modules
;;      and what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c g k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c g d') on a module to browse its
;;      directory (for easy access to its source code).
(doom! :input

       :completion
       (company           ; the ultimate code completion backend
        +auto
        +childframe)
       ;; (ivy               ; a search engine for love and life
       ;;  +childframe
       ;;  +icons)
       (vertico
        +icons)

       :ui
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       ligatures         ; ligatures and symbols to make your code pretty again
       ;; nav-flash         ; blink the current line after jumping
       ;; neotree           ; a project drawer, like NERDTree for vim
       treemacs
       ophints           ; highlight the region an operation acts on
       (popup            ; tame sudden yet inevitable temporary windows
        +all             ; catch all popups that start with an asterix
        +defaults)       ; default popup rules

       (vc-gutter         ; vcs diff in the fringe
        +vc-gutter-default-style nil)
       window-select     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces

       :editor
       (evil +everywhere)  ; come to the dark side, we have cookies
       ;; file-templates      ; auto-snippets for empty files
       ;; fold                ; (nigh) universal code folding
       (format)  ; automated prettiness
       ;; lispy             ; vim for lisp, for people who don't like vim
       ;; multiple-cursors    ; editing in many places at once
       ;; rotate-text         ; cycle region at point between text candidates
       ;; snippets            ; my elves. They type so I don't have to
       parinfer

       :emacs
       dired             ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       ibuffer           ; interactive buffer management
       undo              ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term
       ;;eshell            ; a consistent, cross-platform shell (WIP)
       ;;shell             ; a terminal REPL for Emacs
       ;;term              ; terminals in Emacs
       vterm             ; another terminals in Emacs

       :checkers
       syntax             ; tasing you for every semicolon you forget
       (spell               ; tasing you for misspelling mispelling
        +aspell
        +everywhere)

       :tools
       docker
       (eval +overlay)     ; run code, run (also, repls)
       (lookup             ; helps you navigate your code and documentation
        +docsets)          ; ...or in Dash docsets locally
       (lsp
        +peek)
       magit               ; a git porcelain for Emacs
       make              ; run make tasks from Emacs
       ;; rgb               ; creating color strings
       taskrunner        ; taskrunner for all your projects
       terraform           ; infrastructure as code

       :lang
       (cc
        +lsp)
       data                ; config/data formats
       emacs-lisp          ; drown in parentheses
       (go                 ; the hipster dialect
        +lsp)
       json              ; At least it ain't XML
       ;; (javascript          ; all(hope(abandon(ye(who(enter(here))))))
       ;;  +lsp)
       lua                 ; one-based indices? one-based indices
       markdown            ; writing docs for people to ignore
       (org                ; organize your plain life in plain text
        +dragndrop         ; drag & drop files/images into org buffers
        +pretty
        +present           ; using org-mode for presentations
        +roam2)
       ;; (python           ; beautiful is better than ugly
       ;;  +lsp)
       (ruby               ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
        +lsp)
       (rust               ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
        +lsp)
       sh               ; she sells {ba,z,fi}sh shells on the C xor
       yaml

       :email
       (mu4e +gmail +org)

       :app
       ;;calendar
       ;;(rss +org)        ; emacs as an RSS reader

       :config
       ;;literate
       (default +bindings +smartparens))

(when doom-debug-p
  (require 'benchmark-init)
  (add-hook 'emacs-startup-hook #'benchmark-init/deactivate))
