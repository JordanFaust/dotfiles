;; Org mode config

;; (require 'org-bullets)
;; (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; (require 'org-id)
;; (setq org-id-link-to-org-use-id 'create-if-interactive)

;; (setq-default
;;    org-adapt-indentation nil
;;    org-agenda-dim-blocked-tasks nil
;;    org-agenda-inhibit-startup t
;;    org-agenda-skip-unavailable-files nil
;;    org-cycle-include-plain-lists t
;;    org-cycle-separator-lines 1
;;    org-entities-user '(("flat"  "\\flat" nil "" "" "266D" "♭") ("sharp" "\\sharp" nil "" "" "266F" "♯"))
;;    ;; org-ellipsis " ... "
;;    org-fontify-done-headline t
;;    org-fontify-quote-and-verse-blocks t
;;    org-fontify-whole-heading-line t
;;    org-footnote-auto-label 'plain
;;    org-hidden-keywords nil
;;    org-hide-emphasis-markers nil
;;    org-hide-leading-stars t
;;    org-hide-leading-stars-before-indent-mode t
;;    org-image-actual-width nil
;;    org-indent-indentation-per-level 2
;;    org-indent-mode-turns-on-hiding-stars t
;;    org-pretty-entities nil
;;    org-pretty-entities-include-sub-superscripts t
;;    org-priority-faces
;;    `((?a . ,(face-foreground 'error))
;;      (?b . ,(face-foreground 'warning))
;;      (?c . ,(face-foreground 'success)))
;;    org-startup-folded t
;;    org-startup-indented t
;;    org-startup-with-inline-images nil
;;    org-tags-column 0
;;    org-todo-keywords
;;    '((sequence "[ ](t)" "[-](p)" "[?](m)" "|" "[X](d)")
;;      (sequence "TODO(T)" "|" "DONE(D)")
;;      (sequence "NEXT(n)" "ACTIVE(a)" "WAITING(w)" "LATER(l)" "|" "CANCELLED(c)"))
;;    org-use-sub-superscripts '{}
;;    outline-blank-line t)


;; (setq org-capture-templates
;;       '(
;;         ("t" "Todo" entry
;;          (file+headline "~/org/inbox.org" "Tasks")
;;          "* [ ] %?\n%i")
;;         ("n" "Note/Data" entry
;;          (file+headline "~/org/inbox.org" "Notes/Data")
;;           "* %?   \n  %i\n  %u\n  %a")
;;         ("j" "Journal" entry (file+datetree "~/org/journal.org")
;;           "* %?\nEntered on %U\n %i\n %a")
;;         ("J" "Work-Journal" entry (file+datetree "~/org/wjournal.org")
;;           "* %?\nEntered on %U\n %i\n %a")
;;         ))
;; (setq org-log-done 'time)
;; (setq org-agenda-files (list "~/org/inbox.org"
;;                               "~/org/journal.org"
;;                               "~/org/wjournal.org"
;;                               ))
;; (setq org-refile-targets '((nil :maxlevel . 2)
;;                             (org-agenda-files :maxlevel . 2)
;;                             ("~/org/someday.org" :maxlevel . 2)
;;                             ("~/org/templates.org" :maxlevel . 2)
;;                             ("~/org/done.org" :maxlevel . 2)
;;                             ))
;; ;; (setq org-todo-keywords '(
;; ;;                           (sequence "TODO(t!)" "NEXT(n!)" "STARTED(a!)" "WAIT(w@/!)" "OTHERS(o!)" "|" "DONE(d)" "CANCELLED(c)")
;; ;;                           ))
;; (setq org-todo-keyword-faces
;;       (quote (("TODO" :foreground "#C16069" :weight bold)
;;               ("NEXT" :foreground "#86C0D1" :weight bold)
;;               ("DONE" :foreground "#A2BF8A" :weight light)
;;               ("WAITING_FOR" :foreground "indian red" :weight bold)
;;               ("DELEGATED" :foreground "indian red" :weight bold)
;;                                         ; ("HOLD" :foreground "magenta" :weight bold)
;;               ("CANCELLED" :foreground "light green" :weight light))))

;; (defun air-org-skip-subtree-if-priority (priority)
;;   "Skip an agenda subtree if it has a priority of PRIORITY.

;; PRIORITY may be one of the characters ?A, ?B, or ?C."
;;   (let ((subtree-end (save-excursion (org-end-of-subtree t)))
;;         (pri-value (* 1000 (- org-lowest-priority priority)))
;;         (pri-current (org-get-priority (thing-at-point 'line t))))
;;     (if (= pri-value pri-current)
;;         subtree-end
;;       nil)))

;; (defun air-org-skip-subtree-if-habit ()
;;   "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
;;   (let ((subtree-end (save-excursion (org-end-of-subtree t))))
;;     (if (string= (org-entry-get nil "STYLE") "habit")
;;         subtree-end
;;       nil)))

;; (defun air-skip-unless-tag (tag)
;;   "Skip trees that don't have the passed tag"
;;   (let ((subtree-end (save-excursion (org-end-of-subtree t))))
;;     (if (re-search-forward (format "%s" tag) subtree-end t)
;;         nil          ; tag found, do not skip
;;       subtree-end))) ; tag not found, continue after end of subtree

;; ;; Based off of https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
;; (setq org-agenda-block-separator "--------------------------------------------------------------------------------")
;; (setq org-agenda-remove-tags t)
;; (setq org-agenda-custom-commands
;;       '(("c" "Daily agenda and all TODOs"
;;          (
;;           (tags "PRIORITY=\"A\""
;;                 ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
;;                  (org-agenda-overriding-header "High-Priority Tasks:")))
;;           (tags "DEADLINE<=\"<now>\"|SCHEDULED<=\"<now>\""
;;                 ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
;;                  (org-agenda-overriding-header "Overdue Tasks:")))
;;           (agenda "" ((org-agenda-span 'day)
;;                       (org-agenda-use-time-grid t)
;;                       (org-agenda-time-grid
;;                        '((daily today)
;;                          (800 1000 1200 1400 1600 1800 2000)
;;                          "......"
;;                          "----------------"))
;;                       (org-agenda-overriding-header "Today's Schedule:")
;;                       (org-super-agenda-groups
;;                        '((:name "Today"
;;                                 :time-grid t
;;                                 :date today
;;                                 :todo "TODAY"
;;                                 :scheduled today
;;                                 :order 1
;;                                 )))))
;;           (alltodo ""
;;                    ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
;;                                                    (air-org-skip-subtree-if-priority ?A)
;;                                                    (air-skip-unless-tag "NEXT")
;;                                                    (org-agenda-skip-if nil '(scheduled deadline))))
;;                     (org-agenda-overriding-header "Next Tasks:")))
;;           (alltodo ""
;;                    ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
;;                                                    (air-org-skip-subtree-if-priority ?A)
;;                                                    (org-agenda-skip-entry-if 'todo '("NEXT"))
;;                                                    (org-agenda-skip-if nil '(scheduled deadline))))
;;                    (org-agenda-overriding-header "Normal-Priority Tasks:")))
;;           (agenda "" ((org-agenda-ndays 1)
;;                       (org-agenda-use-time-grid nil)
;;                       (org-agenda-overriding-header "Week At A Glance:"))))
;;          ((org-agenda-compact-blocks nil)))))

;; (set-face-attribute 'org-agenda-structure nil
;;                     :foreground "#80A0C2"
;;                     :weight 'ultra-bold)
;; (setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
;; (setq org-refile-use-outline-path 'file)
