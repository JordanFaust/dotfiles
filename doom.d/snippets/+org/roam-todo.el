;;; ~/.doom.d/snippets/+org/ui.el -*- lexical-binding: t -*-

;;;
;;; Packages
;;;

;; Vulpea provides a set of utilities for digging deeper into the content
;; of notes in org-roam. All Todos and other information is quickly filtered
;; and operated on in an efficent manner using filetags to only process
;; items in the files we need. This make building more complex rollups
;; and views into the data, todos, and content of all my roam notes.

(use-package! vulpea
  :ensure t)

;;;
;;; Config
;;;

;; The filetag added to each note that has a TODO
(defconst +org-roam-todo-tag-key "HasTodo")
;; The filetag added to notes involved in the refile process
(defconst +org-roam-todo-refile-tag-key "TodoRefile")
;; The archive headline used to place todos when archiving them
(defconst +org-roam-todo-archive-headline "::* Completed")

(defun +org-roam-daily-current-file ()
  "Build the path to the current dialy file."
  (expand-file-name
   (concat
    org-roam-dailies-directory
    "/"
    (format-time-string "%Y")
    "/"
    (format-time-string "%B")
    "/"
    (format-time-string "%Y-%m-%d") ".org")))

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

(defun +org-agenda-files-update (&rest _)
  "Update the value of `org-agenda-files' used in the Org Agenda views."
  (setq org-agenda-files
        (cons "~/notes/roam/todos/schedule.org"
              (+org-roam-notes-with-tag-key +org-roam-todo-tag-key))))

(defun +org-refile-agenda-files-update (&rest _)
  "Update the value of `org-agenda-files' used in the org refile workflow."
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
    ;; Create the current daily template unless it already exists
    (unless (file-exists-p (+org-roam-daily-current-file))
      (progn
        (message "The daily file does not exist, creating")
        (org-roam-capture- :keys "xd"
                           :node (org-roam-node-create)
                           :templates org-roam-capture-templates)))
    ;; Update the archive location to the daily file for the current day
    (setq org-archive-location (+org-roam-todo-archive-location))))

(defun +org-roam-buffer-backlink-appearence-update ()
  "Resize the faces within the backlink buffer"
  (set-face-attribute 'magit-section-heading nil :height 280)
  (set-face-attribute 'org-roam-title nil :height 230))

(defun +org-capture-add-property-with-date-captured ()
  "Add CREATED property to current item"
  (interactive)
  (org-set-property "CREATED" (format-time-string "%F")))
