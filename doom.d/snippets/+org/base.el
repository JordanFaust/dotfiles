;;; ~/.doom.d/snippets/+org/base.el -*- lexical-binding: t -*-

;;;
;;; Packages
;;;

(use-package! org-roam
  :after
  ;; TODO figure out why this is needed
  (org-id-update-id-locations))

;; Vulpea provides a set of utilities for digging deeper into the content
;; of notes in org-roam. All Todos and other information is quickly filtered
;; and operated on in an efficent manner using filetags to only process
;; items in the files we need. This make building more complex rollups
;; and views into the data, todos, and content of all my roam notes.

(use-package! vulpea
  :after-call vulpea-buffer-tags-get vulpea-buffer-tags-set vulpea-buffer-prop-get)

;; Used for icons
(use-package! all-the-icons
  :defer t)

;;;
;;; Config
;;;

(after! org
  ;; Set idle time for clocked tasks to 15 minutes
  (setq org-clock-idle-time 15)
  ;; Define the tag and tag groups
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
  ;; Only show roam backlink buffer when explicitly opened
  (setq +org-roam-open-buffer-on-find-file nil)

  ;;;
  ;;; Org/Org Agenda UI Changes
  ;;;

  ;; Remove TODO keywrods from org-mode (it will still work in agenda)
  (set-ligatures! 'org-mode
    :alist '(("TODO " . "")
             ("NEXT " . "")
             ("PROG " . "")
             ("WAIT " . "")
             ("DONE " . "")
             ("FAIL " . "")))
  ;; Ellipsis configuration
  (setq org-ellipsis " ▼")
  ;; Hide signs like "~" or "_" or "*"
  (setq org-hide-emphasis-markers t)
  ;; Don't align tags (Was 77)
  (setq org-tags-column 0)

  (setq-hook! org-mode
    org-log-done 'time
    org-log-reschedule 'time
    org-image-actual-width nil)

  (after! org-fancy-priorities
    (setq org-priority-faces
        `((65 . ( :foreground ,(doom-color 'light-red) :weight bold))
          (66 . ( :foreground ,(doom-color 'yellow) :weight bold))
          (67 . ( :foreground ,(doom-color 'blue) :weight bold))))
    (setq org-fancy-priorities-list
        `((?A . ,(propertize (format "%s [ SEVERE ]" (all-the-icons-faicon "exclamation-circle" :v-adjust -0.01))))
          (?B . ,(propertize (format "%s [ MEDIUM ]" (all-the-icons-faicon "arrow-circle-up" :v-adjust -0.01))))
          (?C . ,(propertize (format "%s [ NORMAL ]" (all-the-icons-faicon "arrow-circle-down" :v-adjust -0.01)))))))

  ;; Define the icons associated with the category of each headline
  ;; a todo was filed to. Use all the icons where available for each
  ;; icon, centering and slightly increasing the size of the icon.
  (customize-set-value
   'org-agenda-category-icon-alist
    `(("active" ,(list (all-the-icons-material "access_time" :height 1.2)) nil nil :ascent center :mask heuristic)
      ("maybe" ,(list (all-the-icons-material "exposure_plus_1" :height 1.2)) nil nil :ascent center :mask heuristic)
      ("work" ,(list (all-the-icons-material "work" :height 1.2)) nil nil :ascent center :mask heuristic)
      ("chore" ,(list (all-the-icons-material "repeat" :height 1.2)) nil nil :ascent center :mask heuristic)
      ("events" ,(list (all-the-icons-material "event" :height 1.2)) nil nil :ascent center :mask heuristic)
      ("inbox" ,(list (all-the-icons-material "inbox" :height 1.2)) nil nil :ascent center :mask heuristic)
      ("walk" ,(list (all-the-icons-material "directions_walk" :height 1.2)) nil nil :ascent center :mask heuristic)
      ("scheduled" ,(list (all-the-icons-material "schedule" :height 1.2)) nil nil :ascent center :mask heuristic)
      ("home" ,(list (all-the-icons-material "home" :height 1.2)) nil nil :ascent center :mask heuristic)
      ("buy" ,(list (all-the-icons-material "attach_money" :height 1.2)) nil nil :ascent center :mask heuristic)))

  (custom-set-faces!
    ;; Org
    `((org-level-1) :weight normal :height 1.6 :inherit outline-1)
    `((org-level-2) :weight normal :height 1.2 :inherit outline-2)
    ;; Org Agenda
    `((org-agenda-structure) :weight normal :height 350 :foreground ,(doom-color 'red))
    `((org-agenda-date-today) :foreground ,(doom-color 'yellow))
    `((org-agenda-date) :foreground ,(doom-color 'green))
    `((org-time-grid) :foreground ,(doom-color 'fg))
    `((org-agenda-current-time) :foreground ,(doom-color 'yellow))
    ;; Event synced with org-gcal. This is not scheduled
    `(org-agenda-calendar-event :foreground ,(doom-color 'blue)))

  (after! org-superstar
    ;; Every non-TODO headline now have no bullet
    (setq org-superstar-headline-bullets-list '("◯" "∙" "∘" "∘" "∘" "∘" "∘" "∘"))
    (setq org-superstar-leading-bullet ?　)
    ;; Enable custom bullets for TODO items
    (setq org-superstar-special-todo-items t)
    (setq org-superstar-todo-bullet-alist
        '(("TODO" "☐　")
          ("NEXT" "✒　")
          ("PROG" "✰　")
          ("WAIT" "☕　")
          ("FAIL" "✘　")
          ("DONE" "✔　")))

    (org-superstar-restart))

  ;;;
  ;;; Org Capture/Refile UI adjustments
  ;;;

  (defun +org-roam-create-date ()
    "Return the date the note was created."
    (format-time-string "<%Y-%m-%d>"))

  ;; nano-modeline does not play well with this
  (remove-hook! 'org-capture-mode-hook '+org-show-target-in-capture-header-h)
  (set-popup-rule! "^ \\*Org tags*" :height 0.4 :side 'bottom :vslot 1)

  ;; Add additional templates for capturing thoughts
  (setq org-roam-capture-templates
        '(("n" "default note" plain
           "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("p" "project" plain
           (file "~/notes/roam/templates/project-template.org")
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: Project - ${title}\n#+CREATED: %(+org-roam-create-date)\n#+filetags: Project")
           :unnarrowed t)
          ("t" "team" plain
           (file "~/notes/roam/templates/team-template.org")
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: Team - ${title}\n#+CREATED: %(+org-roam-create-date)\n#+filetags: Team")
           :unnarrowed t)
          ("a" "Area" plain
           (file "~/notes/roam/templates/area-template.org")
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: Area - ${title}\n#+CREATED: %(+org-roam-create-date)\n#+filetags: Area")
           :unnarrowed t)
          ("r" "RFDs" plain
           (file "~/notes/roam/templates/rfd-template.org")
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: RFD - ${title}\n#+CREATED: %(+org-roam-create-date)\n#+filetags: Area")
           :unnarrowed t)
          ("w" "1 on 1 Reviews" plain
           (file "~/notes/roam/templates/1on1-template.org")
           :if-new (file+head "one-on-ones/%<%Y>/%<%B>/%<%Y-%m-%d>.org" "#+title: 1on1 - %<%Y-%m-%d>\n#+CREATED: %(+org-roam-create-date)\n#+filetags: 1on1\n")
           :unnarrowed t)
          ("d" "daily" plain
           (file "~/notes/roam/templates/daily-template.org")
           :if-new (file+head "daily/%<%Y>/%<%B>/%<%Y-%m-%d>.org" "#+title: Daily - %<%Y-%m-%d>\n#+CREATED: %(+org-roam-create-date)\n#+filetags: Daily")
           :unnarrowed t)
          ("xd" "daily autocomplete" plain
           (file "~/notes/roam/templates/daily-template.org")
           :if-new (file+head "daily/%<%Y>/%<%B>/%<%Y-%m-%d>.org" "#+title: Daily - %<%Y-%m-%d>\n#+CREATED: %(+org-roam-create-date)\n#+filetags: Daily")
           :immediate-finish)
          ("i" "Inbox" entry
           "* TODO %?"
           :if-new (file "~/notes/roam/inbox/work.org")
           :immediate-finish)))


  ;; Update the title and filetags for daily entries
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* test"
           ;; "* Metadata\n\n- Date: %(+org-roam-create-date)\n\n* Summary\n\n%?* Completed\n\n"
           :if-new (file+head "%<%Y-%m-%d>.org" "#+title: Daily - %<%Y-%m-%d>\n#+filetags: Daily")
           :unnarrowed t)
          ("i" "inbox" entry
           "** TODO %?"
           :if-new (file+olp "~/notes/roam/inbox/work.org" ("Inbox"))
           :unnarowed t)
          ;; Used in workflow to generate dailies that don't exist when refiling archived
          ;; tasks to the day they were completed.
          ("x" "automated" entry
           "* Metadata\n\n- Date: t\n\n* Summary\n\n* Completed\n\n"
           ;; (file "~/notes/roam/templates/daily-template.org")
           :if-new (file+head "%<%Y-%m-%d>.org" "#+title: Daily - %<%Y-%m-%d>\n#+filetags: Daily")
           :unnarowed t
           :immediate-finish t))))

;;;
;;; Requires
;;;

(after! (:or org org-agenda org-roam)
  (require 'autoloads "+org/autoloads")
  (require 'ui "+org/ui.el")
  (require 'agenda "+org/agenda")
  (require 'roam-todo "+org/roam-todo")
  (require 'hooks "+org/hooks")
  (require 'clock "+org/clock")
  (require 'keybinds "+org/keybinds"))

(require 'gcal "+org/gcal")

(provide 'base)
