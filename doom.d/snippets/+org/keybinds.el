;;; ~/.doom.d/snippets/+org/keybinds.el -*- lexical-binding: t -*-

(after! org-agenda
  (map!
   :leader
   (:prefix ("n" . "+notes")
    :desc "My Agenda"            :nvg "a" #'+org-my-agenda
    :desc "Capture to Inbox"     :nvg "i" #'+org-roam-capture-to-inbox
    :desc "Goto Inbox"           :nvg "I" #'+org-roam-go-to-inbox
    :desc "Weekly Clock Report"  :nvg "W" #'+org-weekly-clock-report
    :desc "Monthly Clock Report" :nvg "M" #'+org-monthly-clock-report))

  (map! :localleader (:map org-agenda-mode-map :nvg "r" '+org-agenda-process-inbox-item)))

(after! org
  (map!
   :localleader
   :map org-mode-map
   :nvg "h" #'+org-toggle-properties
   :nvg "H" #'+org-toggle-heading
   :nvg "ac" #'+org-donload-screenshot))

(provide '+org-keybinds)
