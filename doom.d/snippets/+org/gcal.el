


(use-package! org-gcal)

(setq org-gcal-recurring-events-mode 'nested
      org-gcal-down-days 14
      org-gcal-up-days 1)

(defun load-secrets ()
  (interactive)
  (load "~/.doom.d/secrets/config.el"))

(defun +org-gcal-sync-calendar ()
  (interactive)
  (load-secrets)
  (org-gcal-fetch))

(setq org-gcal-client-id "880506599293-ok0t1jlcvv202dkp2f0jffr2a2250t9q.apps.googleusercontent.com"
      org-gcal-fetch-file-alist '(("jordan.faust@procore.com" . "~/notes/roam/todos/schedule.org"))
      org-gcal-remove-cancelled-events 't)

(run-with-timer 0 (* 1 60 60) '+org-gcal-sync-calendar)
