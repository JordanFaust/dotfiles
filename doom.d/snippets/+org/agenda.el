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
;;; Org Agenda Overlay UI
;;;

(defun +org-agenda-parse-agenda-line-item (line)
  "Attempt to parse and extract start and end locations of todo components."
  (when (string-match
         (concat
          ;; The whitespace before the breadcrumbs
          "\\(?1:[[:space:]]*\\)"
          ;; The breadcrumbs
          "\\(?2:[[:word:]]*[[:space:]]*❱\\)[[:space:]]*"
          ;; The effort for the task
          " \\(?3:\\[[[:digit:]]*\\:[[:digit:]]*\\]\\)?"
          ;; ;; The TODOs keywords
          ;; "\\(?4:[[:upper:]]*\\)?[[:space:]]?"
          ;; ;; The Priority
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

(defun +org-agenda-create-overlay (start end color)
  "Create an overlay for the given positions with the provided foreground color"
  (let ((overlay (make-overlay start end nil 't 't)))
    (progn
      (overlay-put overlay 'face `(:foreground ,color))
      (overlay-put overlay 'category 'custom-agenda-overlay))))

(defun +org-agenda-scan-finalized-agenda--todo-overlays (line line-beginning properties)
  "Add overlays to the given line containing the todo text of the agenda item."
  (let* ((start (+ line-beginning (plist-get properties :start)))
         (end (+ line-beginning (plist-get properties :end)))
         (overlays (overlays-at start))
         (existing-overlay 'f)
         (color (doom-color 'yellow)))
    (when (car overlays)
      (dolist (overlay overlays)
        ;; If there is an existing overlay and it is not the org-agenda-clocking
        ;; overlay adjust the start position of our overlay
        (when (and (< start (overlay-end overlay))
                   (not (eq (overlay-get overlay 'face) 'org-agenda-clocking)))
          (setq start (overlay-end overlay)))
        ;; If the todo overlay exists record it so we don't double generate
        ;; overlays.
        (when (eq (overlay-get overlay 'category) 'custom-agenda-overlay)
          (setq existing-overlay 't))))
    (when (eq existing-overlay 'f)
      (+org-agenda-create-overlay start end color))))

(defun +org-agenda-scan-finalized-agenda--effort-overlays (line line-beginning properties)
  "Add overlays to the given line containing the effort text of the agenda item."
  (let* ((start (+ line-beginning (plist-get properties :start)))
         (end (+ line-beginning (plist-get properties :end)))
         (overlays (overlays-at start))
         (existing-overlay 'f)
         (color (doom-color 'grey)))
    (when (car overlays)
      (dolist (overlay overlays)
        ;; If the org-agenda-clocking overlay exists change the color
        ;; of the text color
        (when (eq (overlay-get overlay 'face) 'org-agenda-clocking)
          (setq color (doom-color 'fg)))
        ;; If the effort overlay exists record it so we don't double generate
        ;; overlays.
        (when (eq (overlay-get overlay 'category) 'custom-agenda-overlay)
          (setq existing-overlay 't))))
    ;; Create the overlay for the effort text with the given color if
    ;; it doesn't exist
    (when (eq existing-overlay 'f)
      (+org-agenda-create-overlay start end color))))


(defun +org-agenda-scan-finalized-agenda--breadcrumb-overlays (line line-beginning properties)
  "Add overlays to the given line containing the breadcrumbs of the agenda item."
  ;; Subtract 3 from the start to capture the icon prefixed before the breadcrumbs
  (let* ((start (+ line-beginning (plist-get properties :start) -3))
         (end (+ line-beginning (plist-get properties :end)))
         (overlays (overlays-at start))
         (existing-overlay 'f)
         (color (doom-color 'blue)))
    (when (car overlays)
      (dolist (overlay overlays)
        ;; If the effort overlay exists record it so we don't double generate
        ;; overlays.
        (when (eq (overlay-get overlay 'category) 'ccustom-agenda-overlay)
          (setq existing-overlay 't))))
    ;; Create the overlay for the breadcrumbs if it doesn't exist
    (when (eq existing-overlay 'f)
      (+org-agenda-create-overlay start end color))))

(defun +org-agenda-scan-finalized-agenda ()
  "Scan each line of the finalized agenda and add highlighting to the TODOs lines"
  (let ((inhibit-read-only t)
        (buffer-invisibility-spec '(org-link)))
    (save-excursion
      (goto-char (point-max))
      (while (> (point) (point-min))
        (let* ((line-beginning (line-beginning-position))
               (line-end (line-end-position))
               (line (buffer-substring-no-properties line-beginning line-end))
               (properties (+org-agenda-parse-agenda-line-item line))
               (breadcrumbs (plist-get properties :breadcrumbs))
               (effort (plist-get properties :effort))
               (todo (plist-get properties :todo)))
          (when properties
            ;; Add the todo text overlays first since they appear before the other
            ;; components
            (when (plist-get todo :text)
              (+org-agenda-scan-finalized-agenda--todo-overlays line line-beginning todo))
            ;; Add the effort text overlays
            (when (plist-get effort :text)
              (+org-agenda-scan-finalized-agenda--effort-overlays line line-beginning effort))
            ;; Add the breadcrumb overlays
            (when (plist-get breadcrumbs :text)
              (+org-agenda-scan-finalized-agenda--breadcrumb-overlays line line-beginning breadcrumbs)))
          (forward-line -1))))
    (goto-char (point-min))))
