;;; ~/.doom.d/snippets/+org/base.el -*- lexical-binding: t -*-

;;;
;;; Packages
;;;

(use-package! org-roam
  :after
  ;; TODO figure out why this is needed
  (org-id-update-id-locations))

;;;
;;; Config
;;;

(setq org-tag-alist
      '((:startgroup . nil)
        ;; Tag work related items
        ("@work" . ?w)
        ;; Tag emacs or other workflow related items
        ("@emacs" . ?e)
        (:endgroup . nil)
        (:startgroup . nil)
        ("@next" . ?n)
        ("@maybe" . ?m)
        ("@unscheduled" . ?u)
        (:endgroup . nil)
        ;; Catch all tag for refile targets
        ("@refile" . ?r)))
;; Set the org directory to the todos directory
(setq org-directory "~/notes/roam/todos")
;; Set the directory for org roam
(setq org-roam-directory "~/notes/roam")
;; Set the directory for dialies
(setq org-roam-dailies-directory "~/notes/roam/daily")
;; Set the directory for image attachements
(setq org-attach-id-dir "~/notes/roam/attachments")

;; Add additional templates for capturing thoughts
(setq org-roam-capture-templates
      '(("n" "default note" plain
         "%?"
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)
        ("p" "project" plain
         (file "~/notes/roam/templates/project-template.org")
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: Project - ${title}\n#+filetags: Project")
         :unnarrowed t)
        ("t" "team" plain
         (file "~/notes/roam/templates/team-template.org")
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: Team - ${title}\n#+filetags: Team")
         :unnarrowed t)
        ("a" "Area" plain
         (file "~/notes/roam/templates/area-template.org")
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: Area - ${title}\n#+filetags: Area")
         :unnarrowed t)
        ("r" "RFDs" plain
         (file "~/notes/roam/templates/rfd-template.org")
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: RFD - ${title}\n#+filetags: Area")
         :unnarrowed t)
        ("w" "1 on 1 Reviews" plain
         (file "~/notes/roam/templates/1on1-template.org")
         :if-new (file+head "one-on-ones/%<%Y>/%<%B>/%<%Y-%m-%d>.org" "#+title: 1on1 - %<%Y-%m-%d>\n#+filetags: 1on1\n#+date: %U\n")
         :unnarrowed t)
        ("d" "daily" plain
         (file "~/notes/roam/templates/daily-template.org")
         :if-new (file+head "daily/%<%Y>/%<%B>/%<%Y-%m-%d>.org" "#+title: Daily - %<%Y-%m-%d>\n#+filetags: Daily")
         :unnarrowed t)
        ("xd" "daily autocomplete" plain
         (file "~/notes/roam/templates/daily-template.org")
         :if-new (file+head "daily/%<%Y>/%<%B>/%<%Y-%m-%d>.org" "#+title: Daily - %<%Y-%m-%d>\n#+filetags: Daily")
         :immediate-finish)
        ("i" "Inbox" entry
         "* TODO %?"
         :if-new (file "~/notes/roam/inbox/work.org")
         :immediate-finish)
        ))

;; Update the title and filetags for daily entries
(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* test"
         ;; "* Metadata\n\n- Date: %t\n\n* Summary\n\n%?* Completed\n\n"
         :if-new (file+head "%<%Y-%m-%d>.org" "#+title: Daily - %<%Y-%m-%d>\n#+filetags: Daily")
         :unnarrowed t)
        ("i" "inbox" entry
         "** TODO %?"
         :if-new (file+olp "~/notes/roam/inbox/work.org" ("Inbox"))
         :unnarowed t)
        ;; Used in workflow to generate dailies that don't exist when refiling archived
        ;; tasks to the day they were completed.
        ("x" "automated" entry
         "* Metadata\n\n- Date: %t\n\n* Summary\n\n* Completed\n\n"
         ;; (file "~/notes/roam/templates/daily-template.org")
         :if-new (file+head "%<%Y-%m-%d>.org" "#+title: Daily - %<%Y-%m-%d>\n#+filetags: Daily")
         :unnarowed t
         :immediate-finish t)))

;;;
;;; Requires
;;;

(load "+org/ui.el")
(load "+org/agenda")
(load "+org/roam-todo")
(load "+org/keybinds")
(load "+org/hooks")
