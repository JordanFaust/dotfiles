;;; ~/.doom.d/snippets/+org/ui.el -*- lexical-binding: t -*-

;;;
;;; Config
;;;

;;; Org Roam TODOs tag management

;; Update the tags on files when performing a lookup for a node
;; and before saving the buffer of a new note.
(add-hook 'find-file-hook #'+org-roam-update-todo-tag-h)
(add-hook 'before-save-hook #'+org-roam-update-todo-tag-h)

;;; Agenda Specific Changes

;; Lookup and assign all files that have the todo tag to the org-agenda-files
(advice-add 'org-agenda :before #'+org-agenda-files-update-a)
(advice-add 'org-todo-list :before #'+org-agenda-files-update-a)
;; Lookup and assign all files used in the refile workflows
(advice-add 'org-refile :before #'+org-refile-agenda-files-update-a)
(advice-add 'org-agenda-refile :before #'+org-refile-agenda-files-update-a)
;; Update the list of org-refile-targets to those at the appropriate headline
(advice-add 'org-agenda-refile :before #'+org-agenda-refile-targets-update-a)
;; Refresh the agenda after refiling a task
(advice-add 'org-agenda-refile :after #'+org-my-agenda)
;; Refresh the agenda after clocking in/out of a task
(advice-add 'org-agenda-clock-in :after #'+org-my-agenda)
(advice-add 'org-agenda-clock-out :after #'+org-my-agenda)
;; UI adjustments for Org Agenda buffer
(add-hook 'org-agenda-finalize-hook #'+org-set-window-clean-h 100)
;; Change back the headline settings when moving to a different buffer
(add-hook 'window-configuration-change-hook #'+org-undo-window-changes-h)
;; Reset the clean window when the window changes
(add-hook 'window-configuration-change-hook #'+org-reset-margins-after-window-change-h)

;;; Org Archive workflow adjustments

;; Before archiving a task create the daily note if it doesn't already
;; exist and set the daily note as the org-archive-location
(advice-add 'org-archive-subtree :before #'+org-archive-location-update-a)
(advice-add 'org-archive-subtree :after #'save-buffer)

;;; Org Roam Changes

(add-hook 'org-roam-find-file-hook #'+org-roam-buffer-backlink-appearence-update-h)

;;; Org Capture Changes

;; Capture created at timestamp for captured items
(add-hook 'org-capture-before-finalize-hook '+org-capture-add-property-with-date-captured-h)

;;;
;;; Org Clock Advice/Hooks
;;;

;; UI adjustments for Org Clock buffer
(advice-add '+org-weekly-clock-report :after #'+org-set-window-clean-h)
(advice-add '+org-weekly-clock-report :after #'+org-clock-report-close-file-buffers)
(advice-add '+org-monthly-clock-report :after #'+org-set-window-clean-h)
(advice-add '+org-monthly-clock-report :after #'+org-clock-report-close-file-buffers)

;;;
;;; Org GCal Advice/Hooks
;;;

(defadvice! +org-gcal-filter-org-agenda-files-a (&rest _)
  "Only use the schedule file for updating calendar events"
  :before 'org-generic-id-update-id-locations
  (setq org-agenda-files '("~/notes/roam/todos/schedule.org")))

(defadvice! +org-gcal-archive-property-updates-a (&rest _)
  "Strip the entry-id from calendar event headline and add the clock time for the meeting.

Removing the entry-id prevents this event from being updated and the file the meeting is
archived to from being updated when the calendar is synced. A clock entry is also added
that captures the duration of the meeting."
  :before 'org-archive-subtree
  ;;  TODO figure out a way to filter out events that are not for the current day that
  ;;  were archived. Hopefully this issue goes away over time and isn't needed
  (+org-gcal--strip-entry-id-on-archive))

(defadvice! +org-gcal-build-meetings-list-a (&rest _)
  "Build the list of meetings that occur today."
  :after 'org-gcal-fetch
  (+org-gcal--build-meetings-list))

;;;
;;; Timers
;;;

;; Archive daily files that are older then the specified configuration
(run-with-timer 0 (* 24 60 60) '+org-roam-archive-dailies)
;; Sync Google Calendar events
(run-with-timer 0 (* 8 60 60) '+org-gcal-sync-calendar)
