;;; ~/.doom.d/snippets/+org/clock.el -*- lexical-binding: t -*-

;;;
;;; Packages
;;;

;;;
;;; Config
;;;

;;;
;;; Org Clock Report Generation
;;;
;;
;; Functions for finding files used to generate clock reports. Daily
;; roam files are used as archives for meetings and completed tasks.
;;

(defun +org-clock-report-weekly-files ()
  "Return the set of files that can be used in a clock report.

This set includes Daily files that have not been archived and the current
todo files that have active clocks and tasks."
  (let* ((dailies (+org-roam-notes-with-tag-key +org-roam-daily-note-tag-key))
         (weekly (+org-roam-notes-with-tag-key +org-roam-todo-tag-key)))
    (dolist (daily dailies)
      (let* ((timestamp (+org-roam-daily-date-from-file daily))
            (now (decode-time))
            (start-of-week (copy-sequence now))
            (day-of-week (string-to-number (format-time-string "%w"))))
        (when timestamp
          ;; Set the date of start-of-week to the start of the week. This is done by
          ;; subtracting the current day by the day-of-week representation in format-time-string
          (cl-decf (nth 3 start-of-week) day-of-week)
          (let ((start-of-week-timestamp (format-time-string "%Y-%m-%d" (apply #'encode-time start-of-week))))
            (when (or (string-greaterp timestamp start-of-week-timestamp )
                      (string-equal start-of-week-timestamp timestamp))
              (setq weekly (cons daily weekly)))))))
    weekly))


(defun +org-clock-report-monthly-files ()
  "Return the set of files that can be used in a clock report.

This set includes Daily files that have not been archived and the current
todo files that have active clocks and tasks."
  (let* ((dailies (+org-roam-notes-with-tag-key +org-roam-daily-note-tag-key))
         (monthly (+org-roam-notes-with-tag-key +org-roam-todo-tag-key)))
    (dolist (daily dailies)
      (let* ((timestamp (+org-roam-daily-date-from-file daily))
            (now (decode-time))
            (start-of-month (copy-sequence now))
            (day-of-month (string-to-number (format-time-string "%e"))))
        (when timestamp
          ;; Set the date of start-of-week to the start of the week. This is done by
          ;; subtracting the current day by the day-of-week representation in format-time-string
          (cl-decf (nth 3 start-of-month) day-of-month)
          (let ((start-of-month-timestamp (format-time-string "%Y-%m-%d" (apply #'encode-time start-of-month))))
            (when (or (string-greaterp timestamp start-of-month-timestamp )
                      (string-equal start-of-month-timestamp timestamp))
              (setq monthly (cons daily monthly)))))))
    monthly))

(defun +org-clock-report-close-file-buffers ()
  "Close all open buffers opened generating the clock report.

The org clock report function opens a buffer for every file included in the report. This
closes those buffers if they have not been modified"
  (let ((dailies (+org-roam-notes-with-tag-key +org-roam-daily-note-tag-key)))
    (dolist (daily dailies)
      (let* ((daily-buffer-name (concat (+org-roam-daily-date-from-file daily) ".org"))
             (daily-buffer (get-buffer daily-buffer-name)))
        (when daily-buffer
          (unless (buffer-modified-p daily-buffer)
            (kill-buffer daily-buffer)))))))

;;;
;;; Org Clock Report Config
;;;

(defun +org-clockreport-weekly-properteis ()
  "Return the properties used to generate a weekly report"
  '(:scope +org-clock-report-weekly-files
    :maxlevel 2
    :hidefiles 't
    :stepskip0 't
    :fileskip0 't))

(defun +org-clockreport-monthly-properteis ()
  "Return the properties used to generate a monthly report"
  '(:scope +org-clock-report-monthly-files
    :maxlevel 2
    :hidefiles 't
    :stepskip0 't
    :fileskip0 't))

;;;
;;; Org Clock Report UI
;;;

(defun +org-clockreport-render-ui-header (type)
  "Render the header for the clock report."
  (goto-char (point-min))
  (insert "\n")
  (let* ((header-length (min (window-width) 54))
         (timestamp (format-time-string "%Y-%m-%d"))
         (timestamp-length (length timestamp))
         (icon (all-the-icons-material "date_range" :height 0.75 :v-adjust -0.1))
         (icon-length (length icon))
         (whitespace-padding (- header-length timestamp-length icon-length 2))
         (padding (make-string whitespace-padding ?\s)))
    (insert " " icon "  " (capitalize (format "%s" type)) padding timestamp " ")
    ))

(defun +org-clockreport-render-ui-header-overlay (line line-beginning)
  "Add overlays with the desired text properties to the header."
  (when (string-match
         (concat
          ;; Icon and whitespace right after
          "\\(?1:[[:space:]][[:word:]][[:space:]]\\)"
          ;; Whitespace and the type of calendar
          "\\(?2:[[:space:]][[:word:]]*\\)"
          ;; The whitespace padding
          "\\(?3:[[:space:]]*\\)"
          ;; The timestamp
          "\\(?4:[[:digit:]]\\{4\\}\-[[:digit:]]\\{2\\}\-[[:digit:]]\\{2\\}[[:space:]]\\)")
         line)
    (let ((icon (list :start (+ line-beginning (match-beginning 1)) :end (+ line-beginning (match-end 1))))
          (type (list :start (+ line-beginning (match-beginning 2)) :end (+ line-beginning (match-end 2))))
          (padding (list :start (+ line-beginning (match-beginning 3)) :end (+ line-beginning (match-end 3))))
          (timestamp (list :start (+ line-beginning (match-beginning 4)) :end (+ line-beginning (match-end 4)))))
      (let ((overlay (make-overlay (plist-get icon :start) (plist-get icon :end))))
        (overlay-put overlay 'face `(:background ,(doom-color 'red) :foreground ,(doom-color 'bg) :height 1.3))
        (add-text-properties (plist-get icon :start) (plist-get icon :end) '(display '(raise 0.13))))
      (let ((overlay (make-overlay (plist-get type :start) (plist-get type :end))))
        (overlay-put overlay 'face `(:background ,(doom-color 'yellow) :foreground ,(doom-color 'bg) :height 1.3))
        (add-text-properties (plist-get type :start) (plist-get type :end) '(display '(raise 0.15))))
      (let ((overlay (make-overlay (plist-get padding :start) (plist-get padding :end))))
        (overlay-put overlay 'face `(:background ,(doom-color 'yellow) :foreground ,(doom-color 'bg) :height 2.0))
        (add-text-properties (plist-get padding :start) (plist-get padding :end) '(display '(raise 0.10))))
      (let ((overlay (make-overlay (plist-get timestamp :start) (plist-get timestamp :end))))
        (overlay-put overlay 'face `(:background ,(doom-color 'yellow) :foreground ,(doom-color 'bg) :height 1.3))
        (add-text-properties (plist-get timestamp :start) (plist-get timestamp :end) '(display '(raise 0.15))))
      )))

(defun +org-clockreport-render-ui-hide-source-block (line line-beginning)
  "Hide the org clockreport definition lines."
  (when (string-match "\\(?1:#\\+BEGIN\: .*\$\\|#\\+END\:\$\\|#\\+CAPTION\: .*\$\\)" line)
    (let ((overlay (make-overlay line-beginning (+ line-beginning (match-end 1)))))
      (overlay-put overlay 'invisible 't))))

(defun +org-clockreport-render-overlay-ui ()
  "Move line by line and add overlays to the rendered clock report."
  (let ((inhibit-read-only t)
        (buffer-invisibility-spec '(org-link)))
    (save-excursion
      (goto-char (point-max))
      (while (> (point) (point-min))
        (let* ((line-beginning (line-beginning-position))
               (line-end (line-end-position))
               (line (buffer-substring-no-properties line-beginning line-end)))
          (+org-clockreport-render-ui-header-overlay line line-beginning)
          (+org-clockreport-render-ui-hide-source-block line line-beginning))
        (forward-line -1)))))

(defun +org-clockreport-render-ui (type)
  "Render the clock report and add the finalized overlay UI adjustments.

TYPE specifies the kind of report to generate and can be 'weekly or 'monthly."
  (switch-to-buffer (get-buffer-create "*Org Clock Report*"))
  (org-mode)
  (let ((inhibit-read-only 't)
        (properties org-clock-clocktable-default-properties))
    (erase-buffer)
    (when (eq type 'weekly)
      (setq org-clock-clocktable-default-properties (+org-clockreport-weekly-properteis)))
    (when (eq type 'monthly)
      (setq org-clock-clocktable-default-properties (+org-clockreport-monthly-properteis)))
    (+org-clockreport-render-ui-header type)
    (org-clock-report)
    (+org-clockreport-render-overlay-ui)
    (setq org-clock-clocktable-default-properties properties))
  (read-only-mode 1))
