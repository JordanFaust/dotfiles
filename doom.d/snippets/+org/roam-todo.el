;;; ~/.doom.d/snippets/+org/ui.el -*- lexical-binding: t -*-

;;;
;;; Packages
;;;

;; Vulpea provides a set of utilities for digging deeper into the content
;; of notes in org-roam. All Todos and other information is quickly filtered
;; and operated on in an efficent manner using filetags to only process
;; items in the files we need. This make building more complex rollups
;; and views into the data, todos, and content of all my roam notes.

(use-package! vulpea)

;;;
;;; Config
;;;

;; Tags are dynamically added to notes based on their content. This allows
;; narrowing and widening the files included in various queries or org
;; workflows by limiting the number of files that are worked on. By
;; leaning on the data within the roam database this is done efficiently
;; even across hundreds and thousands of files.

;; The filetag added to each note that has a TODO
(defconst +org-roam-todo-tag-key "HasTodo")
;; The filetag added indicating that note has a completed TODO
(defconst +org-roam-completed-todo-tag-key "HasCompleted")
;; The filetag added marking a file as archived and no longer
;; used in any queries. This is used on the daily files that
;; that are older then the configured time window.
(defconst +org-roam-archived-note-tag-key "Archived")
(defvar +org-roam-daily-archive-after 45
  "The number of days until a daily note is archived")
;; The filetag used to mark daily files
(defconst +org-roam-daily-note-tag-key "Daily")
;; The filetag added to notes involved in the refile process
(defconst +org-roam-todo-refile-tag-key "TodoRefile")
;; The archive headline used to place todos when archiving them
(defconst +org-roam-todo-archive-headline "::* Completed")
;; The keyword used to capture the date the note was created
(defconst +org-roam-note-created-keyword "CREATED")

(defun +org-roam-format-daily-file (&optional date)
  "Generate the formatted roam daily file path for the current day or the given date."
  (let ((timestamp (or date (decode-time))))
    (concat org-roam-dailies-directory
            "/"
            (format-time-string "%Y" (apply #'encode-time timestamp))
            "/"
            (format-time-string "%B" (apply #'encode-time timestamp))
            "/"
            (format-time-string "%Y-%m-%d" (apply #'encode-time timestamp))
            ".org")))

(defun +org-roam-daily-date-from-file (file)
  "Extract the date of the daily form the filename."
  (when (string-match
         (concat
          org-roam-directory
          "daily"
          "/[[:digit:]]\\{4\\}"
          "/[[:word:]]*/"
          "\\(?1:[[:digit:]]\\{4\\}\-[[:digit:]]\\{2\\}\-[[:digit:]]\\{2\\}\\).org")
         file)
    (match-string 1 file)))

(defun +org-roam-daily-current-file ()
  "Build the path to the current dialy file."
  (expand-file-name (+org-roam-format-daily-file)))

(defun +org-roam-todo-archive-location ()
  "Build the archive location string for the current daily entry."
  (expand-file-name
   (concat (+org-roam-daily-current-file) +org-roam-todo-archive-headline)))

(defun +org-roam-notes-with-tag-key (filter-key)
  "Return a list of note files containing 'project' tag." ;
  (let ((tag-filter (concat "%\"" filter-key "\"%")))
    (seq-uniq
     (seq-map
      #'car
      (org-roam-db-query
       [:select [nodes:file]
               :from tags
               :left-join nodes
               :on (= tags:node-id nodes:id)
               :where (like tag $r1)] tag-filter)))))

;;;
;;; Update Roam Filetags
;;;
;; Whenever a file is loaded into a buffer or saved we update the set
;; of filetags it has. This process only operates on files that are
;; created or visited and does not handle updating files that have not
;; been visited in a long time.

(defun +org-roam-note-has-todos-p ()
  "Return non-nil if current buffer has any todo entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
  (seq-find                                 ; (3)
   (lambda (type)
     (eq type 'todo))
   (org-element-map (org-element-parse-buffer 'headline) 'headline
     (lambda (h)
       (org-element-property :todo-type h)))))

(defun +org-roam-note-has-completed-todos-p ()
  "Return non-nil if current buffer has completed/terminated todo entry.

TODO entries still in TODO state are ignored, meaning this
function returns nil if the current buffer contains only uncompleted
tasks."
  (seq-find
   (lambda (type)
     (member type '(done kill)))
   (org-element-map
       (org-element-parse-buffer 'greater-element)
       'headline
     (lambda (h)
       (org-element-property :todo-type h)))))

(defun +org-roam-note-p ()
  "Return non-nil if the currently visited buffer is a note."
  (and buffer-file-name
       (string-prefix-p
        (expand-file-name (file-name-as-directory org-roam-directory))
        (file-name-directory buffer-file-name))))

(defun +org-roam-update-todo-tag-h ()
    "Update PROJECT tag in the current buffer."
    (when (and (not (active-minibuffer-window))
               (+org-roam-note-p))
      (message "updating roam note tags %s" (buffer-name))
      (save-excursion
        (goto-char (point-min))
        (let* ((tags (seq-uniq (vulpea-buffer-tags-get)))
               (original-tags tags))
          ;; Add the todo tag key if the note has any top level todos
          (message "Evaluating tags on note")
          (if (+org-roam-note-has-todos-p)
              (setq tags (seq-uniq (cons +org-roam-todo-tag-key tags)))
            (setq tags (remove +org-roam-todo-tag-key tags)))
          ;; Add the completed tag key if the notes has any completed todos
          (if (+org-roam-note-has-completed-todos-p)
              (setq tags (seq-uniq (cons +org-roam-completed-todo-tag-key tags)))
            (setq tags (remove +org-roam-completed-todo-tag-key tags)))
          (unless (cl-equalp original-tags tags)
            (message "original %s updated %s" original-tags tags)
            (apply #'vulpea-buffer-tags-set (seq-uniq tags)))))))

;;;
;;; Add Archive Tag
;;;
;; Certain file types will stop being relevant after a window of time.
;; The daily files in particular are an example of this. To improve the
;; performance of querying the roam files add an archive tag to files
;; that are no longer needed in other org workflows. This tag replaces
;; the daily tag that was previously used.

(defun +org-roam-archive-dailies ()
  "Query all non-archived dailies and mark any that are past the archive date as archived.

Daily files are used to capture details from the day and also capture any completed tasks
for the day. These files will build up fast and be a source of slow down for roam. To
prevent any issues we aggressively archive files that are no longer useful. To configured
the amount of time a daily file should be kept configure it via its variable:

(setq +org-roam-daily-archive-after 45)

This sets all dailies to be archived after 45 days.

This function can then be called at a set interval to mark dailies older then this
as archived.

(run-with-timer 0 (* 24 60 60) '+org-roam-archive-dailies)"
  (let ((dailies (+org-roam-notes-with-tag-key +org-roam-daily-note-tag-key))
        (archive-date (decode-time)))
    ;; Get the target archive date
    (cl-decf (nth 3 archive-date) +org-roam-daily-archive-after)
    (dolist (daily dailies)
      (with-temp-buffer
        ;; Prevent hooks from running on the files
        (insert-file-contents-literally daily)
        (save-excursion
          (goto-char (point-min))
          (let ((timestamp (vulpea-buffer-prop-get +org-roam-note-created-keyword))
                (archive-date-timestamp (format-time-string "%Y-%m-%d" (apply #'encode-time archive-date)))
                (tags (vulpea-buffer-tags-get)))
            (when (string-lessp timestamp archive-date-timestamp)
              (setq tags (cons "Archived" (remove "Daily" tags)))
              (apply #'vulpea-buffer-tags-set (seq-uniq tags)))))))))

(defun +org-agenda-files-update-a (&rest _)
  "Update the value of `org-agenda-files' used in the Org Agenda views."
  (setq org-agenda-files
        (cons "~/notes/roam/todos/schedule.org"
              (+org-roam-notes-with-tag-key +org-roam-todo-tag-key))))

(defun +org-refile-agenda-files-update-a (&rest _)
  "Update the value of `org-agenda-files' used in the org refile workflow."
  (setq org-agenda-files (+org-roam-notes-with-tag-key +org-roam-todo-tag-key)))

(defun +org-agenda-refile-targets-update-a (&rest _)
  "Update the targets for a refiling."
  (setq org-refile-targets '(
                             (org-agenda-files :maxlevel . 1)
                             (org-agenda-files :level . 1)
                             (org-agenda-files :tag . "@refile"))))

(defun +org-archive-location-update-a (&rest _)
  "Dynamically update the archive target location to the current dailies file."
  (save-current-buffer
    ;; Create the current daily template unless it already exists
    (unless (file-exists-p (+org-roam-daily-current-file))
      (org-roam-capture- :keys "xd"
                         :node (org-roam-node-create)
                         :templates org-roam-capture-templates))
    ;; Update the archive location to the daily file for the current day
    (setq org-archive-location (+org-roam-todo-archive-location))))

(defun +org-roam-buffer-backlink-appearence-update-h ()
  "Resize the faces within the backlink buffer"
  (set-face-attribute 'magit-section-heading nil :height 280)
  (set-face-attribute 'org-roam-title nil :height 230))

(defun +org-is-todo-capture-p ()
  "Check to see if the current heading is a todo capture."
  (let* ((line-beginning (line-beginning-position))
         (line-end (line-end-position))
         (line (buffer-substring-no-properties line-beginning line-end)))
    (if (string-match-p "^\\(?1:\\*\\* TODO\\)+ .*" line)
        't)))

(defun +org-capture-add-property-with-date-captured-h ()
  "Add CREATED property to current item"
  (interactive)
  (when (eq (+org-is-todo-capture-p) 't)
    (org-set-property +org-roam-note-created-keyword (format-time-string "%F"))))
