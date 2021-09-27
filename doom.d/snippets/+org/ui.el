;;; ~/.doom.d/snippets/+org/ui.el -*- lexical-binding: t -*-


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
  (set-face-attribute 'header-line nil :background (doom-color 'bg) :height 500)
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

(defun +org-agenda-or-clock-buffer-visible-p ()
  "Returns the first buffer belonging to Org in the current visible frame."
  (declare (side-effect-free error-free))
  (->> (window-list (selected-frame))
       (--first (->> it
                     (window-buffer)
                     (buffer-name)
                     (s-starts-with? "*Org")))))

(defun +org-undo-window-changes-h ()
  "Reset headline changes when leaving the org agenda or org clock report buffer"
  (unless (+org-agenda-or-clock-buffer-visible-p)
    (fringe-mode)
    (set-face-attribute 'header-line nil :height 1.45)
    (setq mode-line-format nil)
    (setq mouse-highlight 't)))

(defun +org-reset-margins-after-window-change-h ()
  "Reset the margins after window configuration has changed for the agenda or org clock buffer."
  (when (+org-agenda-or-clock-buffer-visible-p)
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

(provide '+org-ui)
