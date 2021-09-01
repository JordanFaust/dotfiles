;;; ~/.doom.d/snippets/+org/keybinds.el -*- lexical-binding: t -*-

;;;
;;; Packages
;;;


;;;
;;; Config
;;;


(defvar +org-current-effort "1:00"
  "Current effort for agenda items.")

(defvar +org-screenshot-width "600"
  "Desired width of the captured screenshot.")

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

(defun +org-agenda-process-inbox-item ()
  "Process a single item in the org-agenda"
  (interactive)
  (org-with-wide-buffer
   (org-agenda-set-tags)
   (org-agenda-priority)
   (call-interactively '+org-agenda-set-effort)
   (org-agenda-refile nil nil t)))

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

(defun +org-show-properties ()
  "Show all org-mode property drawers hidden by org-hide-properties."
  (interactive)
  (remove-overlays (point-min) (point-max) 'hidden-prop-drawer t)
  (put 'org-toggle-properties-hide-state 'state 'shown))

(defun +org-toggle-properties ()
  "Toggle visibility of property drawers."
  (interactive)
  (if (eq (get 'org-toggle-properties-hide-state 'state) 'hidden)
      (+org-show-properties)
    (+org-hide-properties)))

(defun +org-download-screenshot (width)
  "Capture a screen shot and specify the desired image width in the capture."
  (interactive
   (list (read-string (format "Width [%s]: " +org-screenshot-width) nil nil +org-screenshot-width)))
  (setq org-image-actual-width (string-to-number width))
  (funcall-interactively 'org-download-screenshot nil))

(defun +org-weekly-clock-report ()
  "Open the weekly clock report"
  (interactive)
  (switch-to-buffer (get-buffer-create "*Org Clock Report*"))
  (org-mode)
  (let ((inhibit-read-only 't)
        (weekly-properties '(:scope +org-clock-report-weekly-files :maxlevel 2 :hidefiles 't :stepskip0 't :fileskip0 't)))
    (erase-buffer)
    (setq org-clock-clocktable-default-properties weekly-properties)
    (org-clock-report))
  (read-only-mode 1))

(defun +org-monthly-clock-report ()
  "Open the monthly clock report."
  (interactive)
  (switch-to-buffer (get-buffer-create "*Org Clock Report*"))
  (org-mode)
  (let ((inhibit-read-only 't)
        (monthly-properties '(:scope +org-clock-report-monthly-files :maxlevel 2 :hidefiles 't :stepskip0 't :fileskip0 't)))
    (erase-buffer)
    (setq org-clock-clocktable-default-properties monthly-properties)
    (org-clock-report))
  (read-only-mode 1))

(map! :leader :desc "My Agenda"        :nvg "na" '+org-my-agenda)
(map! :leader :desc "Capture to Inbox" :nvg "ni" '+org-roam-capture-to-inbox)
(map! :leader :desc "Goto Inbox"       :nvg "nI" '+org-roam-go-to-inbox)
(map! :leader :desc "Weekly Clock Report" :nvg "nW" '+org-weekly-clock-report)
(map! :leader :desc "Monthly Clock Report" :nvg "nM" '+org-monthly-clock-report)
(map! :localleader (:map org-mode-map :nvg "h" '+org-toggle-properties))
(map! :localleader (:map org-mode-map :nvg "H" 'org-toggle-heading))
(map! :localleader (:map org-mode-map :nvg "ac" '+org-download-screenshot))
(map! :localleader (:map org-agenda-mode-map :nvg "r" '+org-agenda-process-inbox-item))
