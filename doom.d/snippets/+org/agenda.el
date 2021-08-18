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
(setq org-agenda-block-separator (string-to-char " "))
(setq org-agenda-format-date '+org-agenda-format-date-aligned)
;; (format "%s" (all-the-icons-material "inbox" :height 1.2))
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
                      (org-agenda-prefix-format "  %-2i %-13b")
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
                      (org-agenda-prefix-format "  %-2i %-13b")
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
                      (org-agenda-span 5)
                      (org-agenda-overriding-header
                       (format "%s Schedule:\n"
                               (all-the-icons-material "today" :height 1.2 :v-adjust -0.1)))
                      (org-agenda-remove-tags t)
                      (org-agenda-prefix-format "      %-3i  %-15b%t %s")
                      (org-agenda-todo-keyword-format " ☐ ")
                      (org-agenda-current-time-string "ᐊ┈┈┈┈┈┈┈ Now")
                      (org-agenda-time-grid (quote ((daily today remove-match) (0900 1200 1800 2100) "      " "┈┈┈┈┈┈┈┈┈┈┈┈┈")))))))))
