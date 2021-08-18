;;; ~/.doom.d/snippets/+org/keybinds.el -*- lexical-binding: t -*-

;;;
;;; Packages
;;;


;;;
;;; Config
;;;

(defun +org-my-agenda (&rest _)
  "Iteractive command to navigate to My Agenda"
  (interactive "P")
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

(defun +org-agenda-process-inbox-item ()
  "Process a single item in the org-agenda"
  (interactive)
  (org-with-wide-buffer
   (org-agenda-set-tags)
   (org-agenda-priority)
   (org-agenda-refile nil nil t)))

(map! :leader :desc "My Agenda"        :nvg "na" '+org-my-agenda)
(map! :leader :desc "Capture to Inbox" :nvg "ni" '+org-roam-capture-to-inbox)
(map! :leader :desc "Goto Inbox"       :nvg "nI" '+org-roam-go-to-inbox)
(map! :localleader (:map org-agenda-mode-map :nvg "r" '+org-agenda-process-inbox-item))
