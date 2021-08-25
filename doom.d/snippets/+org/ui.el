;;; ~/.doom.d/snippets/+org/ui.el -*- lexical-binding: t -*-

;;;
;;; Packages
;;;


(use-package! all-the-icons)
;; (require 'all-the-icons)

;;;
;;; Config
;;;

;; Remove TODO keywrods from org-mode (it will still work in agenda)
(set-ligatures! 'org-mode
    :alist '(("TODO " . "")
             ("NEXT " . "")
             ("PROG " . "")
             ("WAIT " . "")
             ("DONE " . "")
             ("FAIL " . "")))

;; Ellipsis configuration
(setq org-ellipsis " ▼")

;; Hide signs like "~" or "_" or "*"
(setq org-hide-emphasis-markers t)

;; Don't align tags (Was 77)
(setq org-tags-column 0)

(setq-hook! org-mode
  org-log-done 'time
  org-log-reschedule 'time
  org-image-actual-width nil)

(after! org-fancy-priorities
  (setq org-priority-faces
        `((65 . ( :foreground ,(doom-color 'light-red) :weight bold ))
          (66 . ( :foreground ,(doom-color 'yellow) :weight bold ))
          (67 . ( :foreground ,(doom-color 'blue) :weight bold ))))
  (setq org-fancy-priorities-list
        `((?A . ,(propertize (format "%s [ SEVERE ]" (all-the-icons-faicon "exclamation-circle" :v-adjust -0.01))))
          (?B . ,(propertize (format "%s [ MEDIUM ]" (all-the-icons-faicon "arrow-circle-up" :v-adjust -0.01))))
          (?C . ,(propertize (format "%s [ NORMAL ]" (all-the-icons-faicon "arrow-circle-down" :v-adjust -0.01))))))
  )

;; Define the icons associated with the category of each headline
;; a todo was filed to. Use all the icons where available for each
;; icon, centering and slightly increasing the size of the icon.
(customize-set-value
 'org-agenda-category-icon-alist
  `(
      ("active" ,(list (all-the-icons-material "access_time" :height 1.2)) nil nil :ascent center :mask heuristic)
      ("maybe" ,(list (all-the-icons-material "exposure_plus_1" :height 1.2)) nil nil :ascent center :mask heuristic)
      ("work" ,(list (all-the-icons-material "work" :height 1.2)) nil nil :ascent center :mask heuristic)
      ("chore" ,(list (all-the-icons-material "repeat" :height 1.2)) nil nil :ascent center :mask heuristic)
      ("events" ,(list (all-the-icons-material "event" :height 1.2)) nil nil :ascent center :mask heuristic)
      ("inbox" ,(list (all-the-icons-material "inbox" :height 1.2)) nil nil :ascent center :mask heuristic)
      ("walk" ,(list (all-the-icons-material "directions_walk" :height 1.2)) nil nil :ascent center :mask heuristic)
      ("scheduled" ,(list (all-the-icons-material "schedule" :height 1.2)) nil nil :ascent center :mask heuristic)
      ("home" ,(list (all-the-icons-material "home" :height 1.2)) nil nil :ascent center :mask heuristic)
      ("buy" ,(list (all-the-icons-material "attach_money" :height 1.2)) nil nil :ascent center :mask heuristic)))

(custom-set-faces!
  ;; Org
  `((org-level-1) :weight normal :height 1.6 :inherit outline-1)
  `((org-level-2) :weight normal :height 1.2 :inherit outline-2)
  ;; Org Agenda
  `((org-agenda-structure) :weight normal :height 350 :foreground ,(doom-color 'red))
  `((org-agenda-date-today) :foreground ,(doom-color 'yellow))
  `((org-agenda-date) :foreground ,(doom-color 'green))
  `((org-time-grid) :foreground ,(doom-color 'fg))
  `((org-agenda-current-time) :foreground ,(doom-color 'yellow))
  ;; Event synced with org-gcal. This is not scheduled
  `(org-agenda-calendar-event :foreground ,(doom-color 'blue)))

(after! org-superstar
  ;; Every non-TODO headline now have no bullet
  (setq org-superstar-headline-bullets-list '("◯" "∙" "∘" "∘" "∘" "∘" "∘" "∘"))
  (setq org-superstar-leading-bullet ?　)
  ;; Enable custom bullets for TODO items
  (setq org-superstar-special-todo-items t)
  (setq org-superstar-todo-bullet-alist
        '(("TODO" "☐　")
          ("NEXT" "✒　")
          ("PROG" "✰　")
          ("WAIT" "☕　")
          ("FAIL" "✘　")
          ("DONE" "✔　")))

  (org-superstar-restart))
