;;; ~/.doom.d/snippets/+org/ui.el -*- lexical-binding: t -*-

;;;
;;; Config
;;;

;;; Org Roam TODOs tag management

;; Update the tags on files when performing a lookup for a node
;; and before saving the buffer of a new note.
(add-hook 'find-file-hook #'+org-roam-update-todo-tag)
(add-hook 'before-save-hook #'+org-roam-update-todo-tag)

;;; Agenda Specific Changes

;; Lookup and assign all files that have the todo tag to the org-agenda-files
(advice-add 'org-agenda :before #'+org-agenda-files-update)
(advice-add 'org-todo-list :before #'+org-agenda-files-update)
;; Lookup and assign all files used in the refile workflows
(advice-add 'org-refile :before #'+org-refile-agenda-files-update)
(advice-add 'org-agenda-refile :before #'+org-refile-agenda-files-update)
;; Update the list of org-refile-targets to those at the appropriate headline
(advice-add 'org-agenda-refile :before #'+org-agenda-refile-targets-update)
;; Refresh the agenda after refiling a task
(advice-add 'org-agenda-refile :after #'+org-my-agenda)
;; UI adjustments for Org Agenda buffer
(add-hook 'org-agenda-finalize-hook #'+org-agenda-finalizer-set-window-clean 100)
;; Change back the headline settings when moving to a different buffer
(add-hook 'window-configuration-change-hook #'+org-agenda-finalizer-undo-window-changes)
;; Reset the clean window when the window changes
(add-hook 'window-configuration-change-hook #'+org-agenda-finalizer-reset-margins-after-window-change)

;;; Org Archive workflow adjustments

;; Before archiving a task create the daily note if it doesn't already
;; exist and set the daily note as the org-archive-location
(advice-add 'org-archive-subtree :before #'+org-achive-location-update)
(advice-add 'org-archive-subtree :after #'save-buffer)

;;; Org Roam Changes

(add-hook 'org-roam-find-file-hook #'+org-roam-buffer-backlink-appearence-update)

;;; Org Capture Changes

;; Capture created at timestamp for captured items
(add-hook 'org-capture-before-finalize-hook '+org-capture-add-property-with-date-captured)

