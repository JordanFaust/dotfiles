;;; ~/.doom.d/snippets/+org/keybinds.el -*- lexical-binding: t -*-

(defvar +org-screenshot-width "600"
  "Desired width of the captured screenshot.")

;;;###autoload
(defun +org-my-agenda (&rest _)
  "Iteractive command to navigate to My Agenda"
  (interactive "P")
  (org-agenda nil "a"))

;;;###autoload
(defun +org-roam-capture-to-inbox ()
  "Interactive command to start inbox capture workflow"
  (interactive)
  (org-roam-capture- :keys "i"
                     :node (org-roam-node-create)
                     :templates org-roam-dailies-capture-templates))

;;;###autoload
(defun +org-roam-go-to-inbox ()
  "Interactive command to go to the roam inbox"
  (interactive)
  (find-file "~/notes/roam/inbox/work.org"))

;;;###autoload
(defun +org-hide-properties ()
  "Hide all org-mode headline property drawers in buffer. Could be slow if it has a lot of overlays."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "^ *:properties:\n\\( *:.+?:.*\n\\)+ *:end:\n" nil t)
      (let ((ov_this (make-overlay (match-beginning 0) (match-end 0))))
        (overlay-put ov_this 'display "")
        (overlay-put ov_this 'hidden-prop-drawer t))))
  (put 'org-toggle-properties-hide-state 'state 'hidden))

;;;###autoload
(defun +org-show-properties ()
  "Show all org-mode property drawers hidden by org-hide-properties."
  (interactive)
  (remove-overlays (point-min) (point-max) 'hidden-prop-drawer t)
  (put 'org-toggle-properties-hide-state 'state 'shown))

;;;###autoload
(defun +org-toggle-properties ()
  "Toggle visibility of property drawers."
  (interactive)
  (if (eq (get 'org-toggle-properties-hide-state 'state) 'hidden)
      (+org-show-properties)
    (+org-hide-properties)))

;;;###autoload
(defun +org-download-screenshot (width)
  "Capture a screen shot and specify the desired image width in the capture."
  (interactive
   (list (read-string (format "Width [%s]: " +org-screenshot-width) nil nil +org-screenshot-width)))
  (setq org-image-actual-width (string-to-number width))
  (funcall-interactively 'org-download-screenshot nil))

;;;###autoload
(defun +org-weekly-clock-report ()
  "Open the weekly clock report"
  (interactive)
  (require 'org-clock)
  (+org-clockreport-render-ui 'weekly))

;;;###autoload
(defun +org-monthly-clock-report ()
  "Open the monthly clock report."
  (interactive)
  (require 'org-clock)
  (+org-clockreport-render-ui 'monthly))

;;;###autoload
(defun +org-agenda-set-effort (effort)
  "Set the effort property for the current headline"
  (interactive
   (list (read-string (format "Effort [%s]: " +org-current-effort) nil nil +org-current-effort)))
  (setq +org-current-effort effort)
  (org-agenda-check-no-diary)
  (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                       (org-agenda-error)))
         (buffer (marker-buffer hdmarker))
         (pos (marker-position hdmarker))
         (inhibit-read-only t)
         newhead)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
        (widen)
        (goto-char pos)
        (org-show-context 'agenda)
        (funcall-interactively 'org-set-effort nil +org-current-effort)
        (end-of-line 1)
        (setq newhead (org-get-heading)))
      (org-agenda-change-all-lines newhead hdmarker))))

;;;###autoload
(defun +org-agenda-process-inbox-item ()
  "Process a single item in the org-agenda"
  (interactive)
  (org-with-wide-buffer
   (org-agenda-set-tags)
   (org-agenda-priority 'set)
   (call-interactively '+org-agenda-set-effort)
   (org-agenda-refile nil nil t)))

;;;###autoload
(defun +org-my-agenda (&rest _)
  "Iteractive command to navigate to My Agenda"
  (interactive "P")
  ;; TODO fix issue with roam db queries for HasTodo
  (let* ((schedule "~/notes/roam/todos/schedule.org")
         (active-todos (+org-roam-notes-with-tag-key +org-roam-todo-tag-key))
         (org-agenda-files (cons schedule active-todos)))
    (org-agenda nil "a")))


;;;###autoload
(defun +org-roam-capture-to-inbox ()
  "Interactive command to start inbox capture workflow"
  (interactive)
  (org-roam-capture- :keys "i"
                     :node (org-roam-node-create)
                     :templates org-roam-dailies-capture-templates))

;;;###autoload
(defun +org-roam-go-to-inbox ()
  "Interactive command to go to the roam inbox"
  (interactive)
  (find-file "~/notes/roam/inbox/work.org"))

;;; Update Roam Filetags
;; Whenever a file is loaded into a buffer or saved we update the set
;; of filetags it has. This process only operates on files that are
;; created or visited and does not handle updating files that have not
;; been visited in a long time.

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun +org-roam-note-p ()
  "Return non-nil if the currently visited buffer is a note."
  (and buffer-file-name
       (string-prefix-p
        (expand-file-name (file-name-as-directory org-roam-directory))
        (file-name-directory buffer-file-name))))

;;;###autoload
(defun +org-roam-update-todo-tag-h ()
  "Update PROJECT tag in the current buffer."
  (when (and (not (active-minibuffer-window))
             (+org-roam-note-p))
    ;; Explicitly require since a reloaded file with doom reload session
    ;; might need this function before it is incrementally loaded
    (require 'vulpea)
    (save-excursion
      (goto-char (point-min))
      (let* ((tags (seq-uniq (vulpea-buffer-tags-get)))
             (original-tags tags))
        ;; Add the todo tag key if the note has any top level todos
        (message "note has tags %s original %s" tags original-tags)
        (if (+org-roam-note-has-todos-p)
            (setq tags (seq-uniq (cons +org-roam-todo-tag-key tags)))
          (setq tags (remove +org-roam-todo-tag-key tags))
          ;; Add the completed tag key if the notes has any completed todos
          (if (+org-roam-note-has-completed-todos-p)
              (setq tags (seq-uniq (cons +org-roam-completed-todo-tag-key tags)))
            (setq tags (remove +org-roam-completed-todo-tag-key tags)))
          (unless (cl-equalp original-tags tags)
            (message "original %s updated %s" original-tags tags)
            (apply #'vulpea-buffer-tags-set (seq-uniq tags))))))))

;;; Org Gcal

;;;###autoload
(defun +org-gcal--strip-entry-id-on-archive ()
  "Strip the entry-id from the calendar event after it has been archived.

Org GCal is pretty aggressive in searching all agenda files for anything that has an
entry-id property. It considers those files a file that needs updates for the events in it.
This is not desired behavior as anything moved outside of the schedule is an archived meeting.
Stripping the entry-id will prevent org-gcal from considering the file a calendar file."
  (save-excursion
    (org-delete-property "entry-id")))

;;;###autoload
(defun +org-gcal-load-secrets ()
  (interactive)
  (load "~/.doom.d/secrets/config.el"))

;;;###autoload
(defun +org-gcal-sync-calendar ()
  (interactive)
  (require 'org-gcal)
  ;; Check to see if the the TTL has expired on the last run. This is
  ;; persisted across emacs restarts to reduce the number of times it runs.
  (when (or (not (doom-store-member-p 'org-gcal-last-run)))
    (quiet!
     (+org-gcal-load-secrets)
     (org-gcal-fetch))
    ;; Update the store setting the last time it ran and the TTL to our desired sync interval
    (doom-store-put 'org-gcal-last-run (ts-unix (ts-now)) +org-gcal-sync-interval)))

(provide '+org-autoloads)
