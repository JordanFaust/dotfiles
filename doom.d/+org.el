;;; ~/.doom.d/+org.el -*- lexical-binding: t -*-

;;;
;;; Org Appearance/Rice
;;;

(use-package! all-the-icons)
(require 'all-the-icons)

(use-package! org-super-agenda)

;; Hide signs like "~" or "_" or "*"
(setq org-hide-emphasis-markers t)

;; Don't align tags (Was 77)
(setq org-tags-column 0)

(setq org-tag-alist
      '((:startgroup . nil)
        ;; Tag work related items
        ("@work" . ?w)
        ;; Tag emacs or other workflow related items
        ("@emacs" . ?e)
        (:endgroup . nil)
        ;; Catch all tag for refile targets
        ("@refile" . ?r)))

;; Increade indentation in org-indent
;; (setq org-indent-indentation-per-level 2)
;; (setq org-indent-boundary-char ? )

;; Update breadcrumbs separator
(setq org-agenda-breadcrumbs-separator " ❱ ")

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

(setq-hook! org-mode
  org-log-done 'time
  org-log-reschedule 'time
  org-image-actual-width nil)

;; Define the icons associated with the category of each headline
;; a todo was filed to. Use all the icons where available for each
;; icon, centering and slightly increasing the size of the icon.
(customize-set-value
 'org-agenda-category-icon-alist
  `(
      ("active" ,(list (all-the-icons-material "code" :height 1.2)) nil nil :ascent center :mask heuristic)
      ("maybe" ,(list (all-the-icons-material "exposure_plus_1" :height 1.2)) nil nil :ascent center :mask heuristic)
      ("work" ,(list (all-the-icons-material "work" :height 1.2)) nil nil :ascent center :mask heuristic)
      ("chore" ,(list (all-the-icons-material "repeat" :height 1.2)) nil nil :ascent center :mask heuristic)
      ("events" ,(list (all-the-icons-material "event" :height 1.2)) nil nil :ascent center :mask heuristic)
      ("inbox" ,(list (all-the-icons-material "inbox" :height 1.2)) nil nil :ascent center :mask heuristic)
      ("walk" ,(list (all-the-icons-material "directions_walk" :height 1.2)) nil nil :ascent center :mask heuristic)
      ("community" ,(list (all-the-icons-material "group" :height 1.2)) nil nil :ascent center :mask heuristic)
      ("idea" ,(list (all-the-icons-material "lightbulb_outline" :height 1.2)) nil nil :ascent center :mask heuristic)
      ("man" ,(list (all-the-icons-material "accessibility" :height 1.2)) nil nil :ascent center :mask heuristic)
      ("scheduled" ,(list (all-the-icons-material "schedule" :height 1.2)) nil nil :ascent center :mask heuristic)
      ("class" ,(list (all-the-icons-material "school" :height 1.2)) nil nil :ascent center :mask heuristic)
      ("plant" ,(list (all-the-icons-faicon "tree" :height 1.2)) nil nil :ascent center :mask heuristic)
      ("check" ,(list (all-the-icons-material "check" :height 1.2)) nil nil :ascent center :mask heuristic)
      ("search" ,(list (all-the-icons-material "search" :height 1.2)) nil nil :ascent center :mask heuristic)
      ("home" ,(list (all-the-icons-material "home" :height 1.2)) nil nil :ascent center :mask heuristic)
      ("buy" ,(list (all-the-icons-material "attach_money" :height 1.2)) nil nil :ascent center :mask heuristic)
      ))

(defun my-org-agenda-format-date-aligned (date)
  "Format a DATE string for display in the daily/weekly agenda, or timeline.
This function makes sure that dates are aligned for easy reading."
  (require 'cal-iso)
  (let* ((dayname (calendar-day-name date 1 nil))
         (day (cadr date))
         (day-of-week (calendar-day-of-week date))
         (month (car date))
         (monthname (calendar-month-name month 1))
         (year (nth 2 date))
         (iso-week (org-days-to-iso-week
                    (calendar-absolute-from-gregorian date))))
         (format " %-2s. %2d %s, %s"
            dayname day monthname year)))


(setq org-agenda-block-separator (string-to-char " "))
(setq org-agenda-format-date 'my-org-agenda-format-date-aligned)
(setq org-agenda-custom-commands
      '(("a" "My Agenda"
         (
          ;; TODO The Todos
          ;; This section contains the set of tasks I am commiting
          ;; to working on today, with a few thrown in that I may
          ;; optionally work on if time allows. This should not
          ;; consistently contain more work then I am able to complete
          ;; and should not have tasks living in this for more then a
          ;; few days. Any tasks that does was to big or not a priority
          ;; and needs to be refiled.
         ;; (todo "+TODO=\"active\"" (
         ;;              ;; TODO Utilize vulpea to generate counts based
         ;;              ;; on the filters used in this section
         ;;              (org-agenda-overriding-header "⚡ Do Today:\n")
         ;;              (org-agenda-sorting-strategy '(priority-down))
         ;;              (org-agenda-remove-tags t)
         ;;              ;; (org-agenda-tags-column)
         ;;              ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))
         ;;              ;; (org-agenda-todo-ignore-scheduled 'future)
         ;;              (org-agenda-prefix-format "   %-2i %-13b")
         ;;              (org-agenda-todo-keyword-format "")
         ;;           ))

          ;; The Inbox
          ;; This section contains all of my unfilled todos or tasks
          ;; that need further rework to more managable tasks.
          (todo "" (
                      (org-agenda-overriding-header "⚡ Do Today:\n")
                      (org-agenda-sorting-strategy '(priority-down))
                      (org-agenda-remove-tags t)
                      ;; (org-agenda-tags-column)
                      ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))
                      (org-agenda-todo-ignore-scheduled 'all)
                      (org-agenda-prefix-format "   %-2i %-13b")
                      (org-agenda-todo-keyword-format "")))

          ;; The Schedule Things
          ;; This contains a brief look that the next few days and
          ;; anything scheduled that needs to be addressed. This section
          ;; takes precedence over any other tasks. Scheduled tasks
          ;; should only be used when absolutely necessary to make sure
          ;; this section provides a clear glance at priority work.
          (agenda "" (
                      (org-agenda-skip-scheduled-if-done t)
                      (org-agenda-skip-timestamp-if-done t)
                      (org-agenda-skip-deadline-if-done t)
                      (org-agenda-start-day "+0d")
                      (org-agenda-span 5)
                      (org-agenda-overriding-header "⚡ Schedule:\n")
                      (org-agenda-remove-tags t)
                      (org-agenda-prefix-format "  %-3i  %-15b%t %s")
                      (org-agenda-todo-keyword-format " ☐ ")
                      (org-agenda-current-time-string "ᐊ┈┈┈┈┈┈┈ Now")
                      (org-agenda-time-grid (quote ((daily today remove-match) (0900 1200 1800 2100) "      " "┈┈┈┈┈┈┈┈┈┈┈┈┈")))
                      ))
         ))))

(custom-set-faces!
  ;; Org
  '((org-level-1) :weight normal :height 1.6 :inherit outline-1)
  '((org-level-2) :weight normal :height 1.2 :inherit outline-2)
  ;; Org Agenda
  '((org-agenda-structure) :weight normal :height 1.2 :foreground "#ff777a" :underline "on")
  '((org-agenda-date-today) :foreground "#ffba95")
  '((org-agenda-date) :foreground "#d97a9b")
  '((org-time-grid) :foreground "#FAFFF6")
  '((org-agenda-current-time) :foreground "#ffba95"))

(setq org-directory "~/notes/roam/todos")

;; (after! org
;;   ;; Set a new popup rule for the Agenda
;;   (set-popup-rule! "\\*Org Agenda\\*" :side 'bottom :size 0.5 :slot 1))

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

;;; Keybinds

(defun +org-my-agenda ()
  "Iteractive command to navigate to My Agenda"
  (interactive)
  (org-agenda nil "a"))

(defun +org-roam-capture-to-inbox ()
  "Interactive command to start inbox capture workflow"
  (interactive)
  (org-roam-capture- :keys "i"
                     :node (org-roam-node-create)
                     :templates org-roam-dailies-capture-templates))

(defun +org-roam-go-to-inbox ()
  "Interactive command to go to the roam inbox"
  (interactive)
  (find-file "~/notes/roam/inbox/work.org"))

(map! :leader :desc "My Agenda"        :nvg "na" '+org-my-agenda)
(map! :leader :desc "Capture to Inbox" :nvg "ni" '+org-roam-capture-to-inbox)
(map! :leader :desc "Goto Inbox"       :nvg "nI" '+org-roam-go-to-inbox)
;; (map! :localleader (:map org-agenda-mode-map :desc "Refile to Todos" :nvg "x" '+org-refile-inbox-to-todos))

;;;
;;; Task Management via Org Roam
;;;

;;;
;;; Vulpea
;;;


;; Vulpea provides a set of utilities for digging deeper into the content
;; of notes in org-roam. All Todos and other information is quickly filtered
;; and operated on in an efficent manner using filetags to only process
;; items in the files we need. This make building more complex rollups
;; and views into the data, todos, and content of all my roam notes.

(use-package! vulpea
  :ensure t)

;; The filetag added to each note that has a TODO
(defconst +org-roam-todo-tag-key "HasTodo")
;; The filetag added to notes involved in the refile process
(defconst +org-roam-todo-refile-tag-key "TodoRefile")
;; The archive headline used to place todos when archiving them
(defconst +org-roam-todo-archive-headline "::* Completed")

(defun +org-roam-daily-current-file ()
  "Build the path to the current dialy file."
  (expand-file-name
   (concat org-roam-dailies-directory "/" (format-time-string "%Y-%m-%d") ".org")))

(defun +org-roam-todo-archive-location ()
  "Build the archive location string for the current daily entry."
  (expand-file-name
   (concat (+org-roam-daily-current-file) +org-roam-todo-archive-headline)))

(defun +org-roam-note-has-todos-p ()
  "Return non-nil if current buffer has any todo entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
  (seq-find                                 ; (3)
   (lambda (type)
     (eq type 'todo))
   (org-element-map                         ; (2)
       (org-element-parse-buffer 'headline) ; (1)
       'headline
     (lambda (h)
       (org-element-property :todo-type h)))))

(defun +org-roam-update-todo-tag ()
    "Update PROJECT tag in the current buffer."
    (when (and (not (active-minibuffer-window))
               (+org-roam-note-p))
      (save-excursion
        (goto-char (point-min))
        (let* ((tags (vulpea-buffer-tags-get))
               (original-tags tags))
          (if (+org-roam-note-has-todos-p)
              (setq tags (cons +org-roam-todo-tag-key tags))
            (setq tags (remove +org-roam-todo-tag-key tags)))
          (unless (eq original-tags tags)
            (apply #'vulpea-buffer-tags-set (seq-uniq tags)))))))

(defun +org-roam-note-p ()
  "Return non-nil if the currently visited buffer is a note."
  (and buffer-file-name
       (string-prefix-p
        (expand-file-name (file-name-as-directory org-roam-directory))
        (file-name-directory buffer-file-name))))

(defun +org-roam-notes-with-tag-key (filter-key)
    "Return a list of note files containing 'project' tag." ;
    (let ((tag-filter (concat "%\"" filter-key "\"%")))
      (message "%s" tag-filter)
      (seq-uniq
       (seq-map
        #'car
        (org-roam-db-query
        [:select [nodes:file]
                :from tags
                :left-join nodes
                :on (= tags:node-id nodes:id)
                :where (like tag $r1)] tag-filter)))))

(defun +org-agena-files-update (&rest _)
  "Update the value of `org-agenda-files'."
  (setq org-agenda-files (+org-roam-notes-with-tag-key +org-roam-todo-tag-key)))

(defun +org-agenda-refile-targets-update (&rest _)
  "Update the targets for a refiling."
  (setq org-refile-targets '(
                             (org-agenda-files :maxlevel . 1)
                             (org-agenda-files :level . 1)
                             (org-agenda-files :tag . "@refile"))))

(defun +org-achive-location-update (&rest _)
  "Dynamically update the archive target location to the current dailies file."

  (save-current-buffer
    ;; Create the current daily template if it doesn't exist
    (if (file-exists-p (+org-roam-daily-current-file))
        (org-roam-capture- :keys "xd"
                           :node (org-roam-node-create)
                           :templates org-roam-capture-templates))
    ;; Update the archive location to the existing or just created daily file
    (setq org-archive-location (+org-roam-todo-archive-location))))

;; Update the tags on files when performing a lookup for a node
;; and before saving the buffer of a new note.
(add-hook 'find-file-hook #'+org-roam-update-todo-tag)
(add-hook 'before-save-hook #'+org-roam-update-todo-tag)

;;; Agenda Specific Advice

;; Lookup and assign all files that have the todo tag to the org-agenda-files
(advice-add 'org-agenda :before #'+org-agena-files-update)
(advice-add 'org-todo-list :before #'+org-agena-files-update)
;; Update the list of org-refile-targets to those at the appropriate headline
;; level and with the appropriate tags
(advice-add 'org-agenda-refile :before #'+org-agenda-refile-targets-update)
;; Refresh the agenda after refiling a task
(advice-add 'org-agenda-refile :after #'+org-my-agenda)

;; Before archiving a task create the daily note if it doesn't already
;; exist and set the daily note as the org-archive-location
(advice-add 'org-archive-subtree :before #'+org-achive-location-update)


;;;
;;; Org Roam
;;;

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
        ("d" "daily" plain
         (file "~/notes/roam/templates/daily-template.org")
         :if-new (file+head "daily/%<%Y-%m-%d>.org" "#+title: Daily - %<%Y-%m-%d>\n#+filetags: Daily")
         :unnarrowed t)
        ("xd" "daily autocomplete" plain
         (file "~/notes/roam/templates/daily-template.org")
         :if-new (file+head "daily/%<%Y-%m-%d>.org" "#+title: Daily - %<%Y-%m-%d>\n#+filetags: Daily")
         :immediate-finish)
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
        ("r" "1 on 1 Reviews" plain
         (file "~/notes/roam/templates/1on1-template.org")
         :if-new (file+head "one-on-ones/%<%Y-%m-%d>.org" "#+title: 1on1 - %<%Y-%m-%d>\n#+filetags: 1on1\n#+date: %U\n")
         :unnarrowed t)
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

;; Resize the faces within the backlink buffer
(defun org-roam-buffer-backlink-appearence-update ()
  (set-face-attribute 'magit-section-heading nil :height 280)
  (set-face-attribute 'org-roam-title nil :height 230))
(add-hook 'org-roam-find-file-hook #'org-roam-buffer-backlink-appearence-update)

(use-package! org-roam
  :after
  ;; TODO figure out why this is needed
  (org-id-update-id-locations))
