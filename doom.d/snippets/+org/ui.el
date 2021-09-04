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

;;;
;;; Org Capture/Refile UI adjustments
;;;

;; nano-modeline does not play well with this
(after! org
  (remove-hook! 'org-capture-mode-hook '+org-show-target-in-capture-header-h)
  (set-popup-rule! "^ \\*Org tags*" :height 0.4 :side 'bottom :vslot 1))

;;;
;;; Agenda/Clock Report Display Functions
;;;

(defun +org-set-window-clean-h (&optional changes)
  "Make the agenda and clock report buffer look better by adjusting faces and disable modes."
  ;; Remove the headline formatting
  (setq header-line-format " ")
  ;; Remove mode line formatting
  (setq mode-line-format nil)
  ;; Increase the height of the modeline
  (set-face-attribute 'header-line nil :background "#00000000" :height 500)
  ;; Turn off mouse highlighting
  (setq mouse-highlight nil)
  ;; Add side margin padding
  (set-window-margins (frame-selected-window) 6)
  ;; Remove fringe mode from the buffer
  (fringe-mode -1)

  ;; When doing a full clean
  (when (not (eq changes 'minimal))
    ;; Disable highlight line mode to better mask the different line heights
    (hl-line-mode -1)
    ;; Disable displaying line numbers
    (display-line-numbers-mode -1)
    ;; Force the cursor back to the top of the window
    (goto-char (point-min))
    ;; Scan and add overlays to the rendered agenda
    (when (string-equal (buffer-name) "*Org Agenda*")
      (+org-agenda-scan-finalized-agenda))))

(defun +org-undo-window-changes-h ()
  "Reset headline changes when leaving the org agenda or org clock report buffer"
  ;; TODO figure out why this is needed instead of using the popup-rule
  (when (string-equal (buffer-name) " *Org tags*")
    (window-resize (get-buffer-window (current-buffer)) 3))
  (unless (or (string-equal (buffer-name) "*Org Agenda*")
              (string-equal (buffer-name) "*Org Clock Report*"))
    (fringe-mode)
    (set-face-attribute 'header-line nil :height 250)
    (setq mouse-highlight 't)))

(defun +org-reset-margins-after-window-change-h ()
  "Reset the margins after window configuration has changed for the agenda or org clock buffer."
  (when (or (string-equal (buffer-name) "*Org Agenda*")
            (string-equal (buffer-name) "*Org Clock Report*"))
    (+org-set-window-clean-h 'minimal)))

;;;
;;; Org Overlay Header
;;;

(defun +org-overlay-header-insert (type icon &optional width)
  "Insert the textual header for the Org Overlay Header.

TYPE is the symbol or string representation of the tile of the Org Overlay Header.
ICON is the icon added to the right hand side of the Org Overlay Header.
WIDTH is the width of the Org Overlay Header."
  (goto-char (point-min))
  (insert "\n")
  (let* ((header-length (or width (min (window-width) 60)))
         (timestamp (format-time-string "%Y-%m-%d"))
         (timestamp-length (length timestamp))
         (icon-length (length icon))
         (whitespace-padding (- header-length timestamp-length icon-length 2))
         (padding (make-string whitespace-padding ?\s)))
    (insert " " icon "  " (capitalize (format "%s" type)) padding timestamp " " "\n")))

(defun +org-overlay-header-text-p (line)
  "Returns true if the given line is the Org Overlay Header line."
  (string-match
   (concat
    ;; Icon and whitespace right after
    "\\(?1:[[:space:]][[:word:]][[:space:]]\\)"
    ;; Whitespace and the type of calendar
    "\\(?2:[[:space:]][[:word:]]*[[:space:]]?[[:word:]]*\\)"
    ;; The whitespace padding
    "\\(?3:[[:space:]]*\\)"
    ;; The timestamp
    "\\(?4:[[:digit:]]\\{4\\}\-[[:digit:]]\\{2\\}\-[[:digit:]]\\{2\\}[[:space:]]\\)")
   line))

(defun +org-overlay-header-render-overlay (line line-beginning height primary accent)
  "Add overlays with the desired text properties to the header.

LINE is the line of text that contains the Org Overlay Header.
LINE BEGINNING is the start of the line within the buffer.
HEIGHT is the height of Org Overlay Header.
PRIMARY is the primary background of Org Overlay Header.
ACCENT is the background for the icon."
  (when (+org-overlay-header-text-p line)
    (let ((icon (list :start (+ line-beginning (match-beginning 1)) :end (+ line-beginning (match-end 1))))
          (type (list :start (+ line-beginning (match-beginning 2)) :end (+ line-beginning (match-end 2))))
          (padding (list :start (+ line-beginning (match-beginning 3)) :end (+ line-beginning (match-end 3))))
          (timestamp (list :start (+ line-beginning (match-beginning 4)) :end (+ line-beginning (match-end 4))))
          (text-height (/ height 1.53)))
      (let ((overlay (make-overlay (plist-get icon :start) (plist-get icon :end))))
        (overlay-put overlay 'face `(:background ,accent :foreground ,(doom-color 'bg) :height ,text-height))
        (add-text-properties (plist-get icon :start) (plist-get icon :end) '(display '(raise 0.13))))
      (let ((overlay (make-overlay (plist-get type :start) (plist-get type :end))))
        (overlay-put overlay 'face `(:background ,primary :foreground ,(doom-color 'bg) :height ,text-height))
        (add-text-properties (plist-get type :start) (plist-get type :end) '(display '(raise 0.15))))
      (let ((overlay (make-overlay (plist-get padding :start) (plist-get padding :end))))
        (overlay-put overlay 'face `(:background ,primary :foreground ,(doom-color 'bg) :height ,height))
        (add-text-properties (plist-get padding :start) (plist-get padding :end) '(display '(raise 0.10))))
      (let ((overlay (make-overlay (plist-get timestamp :start) (plist-get timestamp :end))))
        (overlay-put overlay 'face `(:background ,primary :foreground ,(doom-color 'bg) :height ,text-height))
        (add-text-properties (plist-get timestamp :start) (plist-get timestamp :end) '(display '(raise 0.15)))))))
