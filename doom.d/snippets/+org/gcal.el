;;; ~/.doom.d/snippets/+org/gcal.el -*- lexical-binding: t -*-

;;;
;;; Packages
;;;

(use-package! org-gcal
  :defer t
  :init
  (setq org-gcal-client-id "880506599293-ok0t1jlcvv202dkp2f0jffr2a2250t9q.apps.googleusercontent.com")
  (setq org-gcal-fetch-file-alist '(("jordan.faust@procore.com" . "~/notes/roam/todos/schedule.org")))
  (setq org-gcal-remove-cancelled-events 't)
  :config
  (setq org-gcal-recurring-events-mode 'nested)
  (setq org-gcal-down-days 5)
  ;; ;; Setting this to zero causes the event to be archived
  ;; ;; the same day. Setting this to one causes this to be
  ;; ;; archived the following day.
  (setq org-gcal-up-days 0)
  (setq org-generic-id-search-archives nil))

;; ;;;
;; ;;; Config
;; ;;;

;; NOTE: If the calendar data needs to be reset follow the steps mentioned here
;; https://github.com/kidd/org-gcal.el/issues/108#issuecomment-764390131
;;
;; The script is pretty heavy. The thing that needs to happen is deletion of the
;; schedule.org file.

(defun load-secrets ()
  (interactive)
  (load "~/.doom.d/secrets/config.el"))

(defun +org-gcal-sync-calendar ()
  (interactive)
  (load-secrets)
  (org-gcal-fetch))

;; ;; TODO figure out if archiving to daily is worth it for meetings. If this is an issue
;; ;; then using an around advice on org-archive-subtree to find the type of current
;; ;; headline before continuing. If it is a calendar archival the org-archive-location
;; ;; can be modified to point to something like "~/notes/roam/todos/schedule_archive.org"
;; ;;
;; ;; Look into add-advive! :filter-output
;; ;;

;; ;; Look into org-gcal-after-update-entry-functions for adding clock time

(defun +org-gcal--clock-duration ()
  "Use the gcal entry details to generate the start and end date for a clock report."
  (save-excursion
    (let ((elem (org-element-headline-parser (point-max) t))
          (time-and-desc (org-gcal--get-time-and-desc)))
      (when (plist-get time-and-desc :desc)
        (list :start (plist-get time-and-desc :start) :end (plist-get time-and-desc :end))))))


;; (parse-iso8601-time-string "2021-09-06T18:00:00Z")
;; (current-time)
;; (encode-time (iso8601-parse "2021-09-06T18:00:00Z"))
;; (org-format-time-string "%Y-%m-%d %a %H:%M" (parse-iso8601-time-string "2021-09-06T18:00:00Z"))
(defun +org-gcal--add-clock-entry-for-meeting ()
  "Add clocked time for the duration of the meeting."
  ;; Need to wrap another save excursion to jump to current clock and record it.
  ;; Then execute the current sav-excursion which will calculate the clock details
  ;; for the meeting and clockout again. After that is complete the previous clock can
  ;; be resumed.
  (save-excursion
    (let* ((details (+org-gcal--clock-duration))
           (start (parse-iso8601-time-string (plist-get details :start)))
           (formatted-start (org-format-time-string "%Y-%m-%d %a %H:%M" start))
           (end (iso8601-parse (plist-get details :end)))
           (formatted-end (org-format-time-string "%Y-%m-%d %a %H:%M" end)))
      ;; This is breaking the clock. The clock out at the end is not enough.
      (org-clock-find-position org-clock-in-resume)
      (org-insert-time-stamp start t t "CLOCK: ")
      (insert "--")
      (org-insert-time-stamp end t t)
      ;; (setq org-clock-total-time (org-clock-sum-current-item (org-clock-get-sum-start)))
      (insert (format " =>  5:00"))
      (insert "\n")
      (move-marker org-clock-marker (point) (buffer-base-buffer))
      (move-marker org-clock-hd-marker
                   (save-excursion (org-back-to-heading t) (point))
                   (buffer-base-buffer)))))
      ;; (org-clock-out)
      

;; After each calendar sync build a cache of the meetings for the
;; day. Possible bucketed by their start and end time. Then every minute
;; this can be used to lookup if a meeting has started. When it has clock it
;; in (and mark it in the structure). When the meeting has ended clock out.

(defvar +org-gcal--meetings-today (list)
  "The list of gcal meetings left for the day.")
(defun +org-gcal--build-meetings-list ()
  "Parse the schedule and find the current meetings.


Iterate through all scheduled meetings pulled down from gcal and collect
the meetings happening today."
  (with-current-buffer (find-file-noselect "~/notes/roam/todos/schedule.org")
    (goto-char (point-min))
    ;; (org-clock-jump-to-current-clock)
    (let* ((now (decode-time))
           (tomorrow-copy (copy-sequence now)))
      ;; Set tomorrow to the start of the next day
      (cl-letf* (((nth 0 tomorrow-copy) 0)
                 ((nth 1 tomorrow-copy) 0)
                 ((nth 2 tomorrow-copy) 0)
                 ((nth 3 tomorrow-copy) (1+ (nth 3 tomorrow-copy))))
        ;; Reset meeting list
        (setq +org-gcal--meetings-today nil)
        (org-map-entries
         (lambda ()
           (let* ((headline (cadr (org-element-headline-parser (point-max) t)))
                  (time-and-desc (org-gcal--get-time-and-desc))
                  (details (list :title (plist-get headline :title)
                                 :start (plist-get time-and-desc :start)
                                 :end   (plist-get time-and-desc :end)))
                  (start (parse-iso8601-time-string (plist-get time-and-desc :start)))
                  (end (parse-iso8601-time-string (plist-get time-and-desc :end)))
                  (tomorrow (encode-time tomorrow-copy)))
             (when (and (time-less-p (encode-time now) end)
                        (time-less-p end tomorrow)
                        (time-less-p start tomorrow)
                        (not (time-equal-p start end)))
               (if (null +org-gcal--meetings-today)
                   (setq +org-gcal--meetings-today `(,details))
                 (cl-pushnew details +org-gcal--meetings-today :test #'equal-including-properties)))))))
      +org-gcal--meetings-today)))

(defun +org-gcal--strip-entry-id-on-archive ()
  "Strip the entry-id from the calendar event after it has been archived.

Org GCal is pretty aggressive in searching all agenda files for anything that has an
entry-id property. It considers those files a file that needs updates for the events in it.
This is not desired behavior as anything moved outside of the schedule is an archived meeting.
Stripping the entry-id will prevent org-gcal from considering the file a calendar file."
  (save-excursion
    (org-delete-property "entry-id")))
