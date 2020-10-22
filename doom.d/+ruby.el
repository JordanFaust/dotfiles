;;
;; Swap to enh-ruby-mode
;;

(use-package! enh-ruby-mode
  :when (executable-find "ruby")
  :after ruby-mode
  :config
  ;; If ruby is present, `enh-ruby-mode' provides superior (semantic) syntax
  ;; highlighting so use it instead.
  (advice-add #'ruby-mode :override #'enh-ruby-mode))

(after! enh-ruby-mode
  :config
  (add-hook! '(ruby-mode-local-vars-hook
             enh-ruby-mode-local-vars-hook)
             #'lsp!)

  (map! :localleader
        :map ruby-mode-map
        "[" #'ruby-toggle-block
        "{" #'ruby-toggle-block))

(use-package! rubocop
  :hook (enh-ruby-mode . rubocop-mode)
  :config
  (map! :after ruby-mode
        :localleader
        :prefix ("r" . "rubocop")
        :map rubocop-mode-map
        "f" #'rubocop-check-current-file
        "F" #'rubocop-autocorrect-current-file
        "p" #'rubocop-check-project
        "P" #'rubocop-autocorrect-project))

(use-package! rspec-mode
  :mode ("/\\.rspec\\'" . text-mode)
  :init
  (setq rspec-use-spring-when-possible nil)
  (when (featurep! :editor evil)
    (add-hook 'rspec-mode-hook #'evil-normalize-keymaps))
  :config
  (set-popup-rule! "^\\*\\(rspec-\\)?compilation" :size 0.3 :ttl nil :select t)
  (setq rspec-use-rvm (executable-find "rvm"))
  (map! :localleader
        :prefix ("t" . "testing")
        :map (rspec-verifiable-mode-map rspec-dired-mode-map rspec-mode-map)
        "a" #'rspec-verify-all
        "r" #'rspec-rerun
        :map (rspec-verifiable-mode-map rspec-mode-map)
        "v" #'rspec-verify
        "c" #'rspec-verify-continue
        "l" #'rspec-run-last-failed
        "T" #'rspec-toggle-spec-and-target
        "t" #'rspec-toggle-spec-and-target-find-example
        :map rspec-verifiable-mode-map
        "f" #'rspec-verify-method
        "m" #'rspec-verify-matching
        :map rspec-mode-map
        "s" #'rspec-verify-single
        "e" #'rspec-toggle-example-pendingness
        :map rspec-dired-mode-map
        "v" #'rspec-dired-verify
        "s" #'rspec-dired-verify-single))

(use-package! rake
  :defer t
  :init
  (setq rake-cache-file (concat doom-cache-dir "rake.cache"))
  (setq rake-completion-system 'default)
  (map! :after ruby-mode
        :localleader
        :map (ruby-mode-map enh-ruby-mode-map)
        :prefix ("k" . "rake")
        "k" #'rake
        "r" #'rake-rerun
        "R" #'rake-regenerate-cache
        "f" #'rake-find-task))

(use-package! bundler
  :defer t
  :init
  (map! :after ruby-mode
        :localleader
        :map (ruby-mode-map enh-ruby-mode-map)
        :prefix ("b" . "bundler")
        "c" #'bundle-check
        "C" #'bundle-console
        "i" #'bundle-install
        "u" #'bundle-update
        "e" #'bundle-exec
        "o" #'bundle-open))
