(setq multi-term-program "/bin/zsh")
(add-hook 'term-mode-hook
          (lambda ()
            (setq term-buffer-maximum-size 10000)
            (setq show-trailing-whitespace nil)
            (setenv "EMACS_PROJECT_ROOT" (projectile-project-root))
            (define-key term-raw-map (kbd "C-y") 'term-paste)
            (add-to-list 'term-unbind-key-list "C-a")))


;; Add important shell env variables
;; (setenv "GOCACHE" "/home/jfaust/work/go/cache/")
(setenv "INSIDE_EMACS" "true")
