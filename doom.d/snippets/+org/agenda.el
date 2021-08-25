;;; ~/.doom.d/snippets/+org/agenda.el -*- lexical-binding: t -*-

;;;
;;; Packages
;;;

(use-package! org-super-agenda)

;;;
;;; Config
;;;

(defun +org-agenda-format-date-aligned (date)
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

;; Update breadcrumbs separator
(setq org-agenda-breadcrumbs-separator " ❱ ")
;; Add empty space as separator between blocks
(setq org-agenda-block-separator (string-to-char " "))
;; Use custom date format with alignement
(setq org-agenda-format-date '+org-agenda-format-date-aligned)
;; Define custom agendas
(setq org-agenda-custom-commands
      '(("a" "My Agenda"
         (
          ;; This section contains the set of tasks I am commiting
          ;; to working on today, with a few thrown in that I may
          ;; optionally work on if time allows. This should not
          ;; consistently contain more work then I am able to complete
          ;; and should not have tasks living in this for more then a
          ;; few days. Any tasks that does was to big or not a priority
          ;; and needs to be refiled.
          (tags-todo "+@next|+@maybe"
                     (
                      ;; TODO Utilize vulpea to generate counts based
                      ;; on the filters used in this section
                      (org-agenda-overriding-header
                       (format "%s Do Today:\n"
                               (all-the-icons-material "access_time" :height 1.2 :v-adjust -0.1)))
                      ;; (org-agenda-overriding-header "⚡ Do Today:")
                      (org-agenda-sorting-strategy '(priority-down))
                      (org-agenda-remove-tags t)
                      (org-agenda-prefix-format "  %-2i %-8b [%-4e]")
                      (org-agenda-todo-keyword-format "")
                   ))

          ;; The Inbox
          ;; This section contains all of my unfilled todos or tasks
          ;; that need further rework to more managable tasks.
          (tags-todo "+@inbox" (
                      (org-agenda-overriding-header
                       (format "%s Inbox:\n"
                               (all-the-icons-material "mail" :height 1.2 :v-adjust -0.1)))
                      (org-agenda-sorting-strategy '(priority-down))
                      (org-agenda-remove-tags t)
                      (org-agenda-todo-ignore-scheduled 'all)
                      (org-agenda-prefix-format "  %-2i %-8b")
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
                      (org-agenda-span 1)
                      (org-agenda-overriding-header
                       (format "%s Schedule:\n"
                               (all-the-icons-material "today" :height 1.2 :v-adjust -0.1)))
                      (org-agenda-remove-tags t)
                      (org-agenda-prefix-format "%-3i  %-15b%t %s")
                      (org-agenda-todo-keyword-format " ☐ ")
                      (org-agenda-current-time-string "ᐊ┈┈┈┈┈┈┈ Now")
                      (org-agenda-time-grid (quote ((daily today remove-match) (0900 1200 1800 2100) "      " "┈┈┈┈┈┈┈┈┈┈┈┈┈")))))))))


;;;
;;; Agenda Display Functions
;;;

(defun +org-agenda-finalizer-set-window-clean (&optional changes)
  "Make the agenda buffer look better by adjusting faces and disabling modes."
  ;; Remove the modeline
  (setq mode-line-format nil)
  ;; Remove modeline formatting
  (setq header-line-format " ")
  ;; Disable highlight line mode to better mask the different line heights
  (hl-line-mode -1)
  ;; Disable displaying line numbers
  (display-line-numbers-mode -1)
  ;; Increase the height of the modeline
  (set-face-attribute 'header-line nil :background "#00000000" :height 600)
  ;; Add side margin padding
  (set-window-margins (frame-selected-window) 6)
  ;; Force the cursor back to the top of the window
  (goto-char (point-min))
  ;; (when (not (eq changes 'minimal))
    (message "running full changes")
    (+org-agenda-scan-finalized-agenda)
    ;; )
  )

(defun +org-agenda-finalizer-undo-window-changes ()
  "Reset headline changes when leaving the org agenda buffer"
  (unless (string-equal (buffer-name) "*Org Agenda*")
    (set-face-attribute 'header-line nil :height 200)))

(defun +org-agenda-finalizer-reset-margins-after-window-change ()
  "Reset the margins after window configuration has changed for the agenda buffer."
  (when (string-equal (buffer-name) "*Org Agenda*")
    (+org-agenda-finalizer-set-window-clean 'minimal)))

(defun +org-agenda-parse-agenda-line-item (line)
  "Attempt to parse and extract start and end locations of todo components."
  (when (string-match
         (concat
          ;; The whitespace before the breadcrumbs
          "\\(?1:[[:space:]]*\\)"
          ;; The breadcrumbs
          "\\(?2:[[:word:]]*[[:space:]]*❱[[:space:]]*\\)"
          ;; The effort for the task
          " \\(?3:\\[[[:digit:]]*\\:[[:digit:]]*\\]\\)?"
          ;; The TODOs keywords
          "\\(?4:[[:upper:]]*\\)[[:space:]]"
          ;; The Priority
          "\\(?5:\\[\\#[[:upper:]]*\\]\\)?[[:space:]]?"
          ;; The headline of the task
          "\\(?6:.*\\)")
         line)

    (list :whitespace (list :text (match-string 1 line) :start (match-beginning 1) :end (match-end 1))
          :breadcrumbs (list :text (match-string 2 line) :start (match-beginning 2) :end (match-end 2))
          :effort (list :text (match-string 3 line) :start (match-beginning 3) :end (match-end 3))
          :todo-category (list :text (match-string 4 line) :start (match-beginning 4) :end (match-end 4))
          :priority (list :text (match-string 5 line) :start (match-beginning 5) :end (match-end 5))
          :todo (list :text (match-string 6 line) :start (match-beginning 6) :end (match-end 6)))
    )
  )
;; (+org-agenda-parse-formatted-item "     Today ❱  [8:00]TODO [#A] CPO-809 | Task | Create cluster dashboard for cluster components")
;; (+org-agenda-parse-formatted-item "    INBOX ❱ TODO Define How to schedule tasks/todos in org")


(defun +org-agenda-scan-finalized-agenda ()
  "Scan each line of the finalized agenda and add highlighting to the TODOs lines"
  (goto-char (point-max))
  (while (> (point) (point-min))
    (let* ((line-beginning (line-beginning-position))
           (line-end (line-end-position))
           (line (buffer-substring-no-properties line-beginning line-end))
           (properties (+org-agenda-parse-agenda-line-item line))
           (breadcrumbs (plist-get properties :breadcrumbs))
           (effort (plist-get properties :effort))
           (todo (plist-get properties :todo)))
      (message "properties %s" properties)
      (message "breadcrumb properties %s" breadcrumbs)
      (message "line: %s" line)
      (when (plist-get breadcrumbs :text)
        (let* ((start (+ (line-beginning-position (plist-get breadcrumbs :start))))
               (end (+ start (plist-get breadcrumbs :end)))
               (overlay (make-overlay start end)))
          (overlay-put overlay 'face `(:foreground ,(doom-color 'blue)))))
      (when (plist-get effort :text)
        (let* ((start (+ (line-beginning-position (plist-get effort :start))))
               (end (+ start (plist-get effort :end)))
               (overlay (make-overlay start end)))
          (overlay-put overlay 'face `(:foreground ,(doom-color 'grey)))))
      (when (plist-get todo :text)
        (let* ((start (+ (line-beginning-position (plist-get todo :start))))
               (end (+ start (plist-get todo :end)))
               (overlay (make-overlay start end)))
          (overlay-put overlay 'face `(:foreground ,(doom-color 'yellow)))))
      (forward-line -1)))
  )
