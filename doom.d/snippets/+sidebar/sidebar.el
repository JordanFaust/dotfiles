;;; -*- lexical-binding: t -*-
;;; ~/.doom.d/snippets/+sidebar/sidebar.el

(defgroup +sidebar nil
  "Sidebar configuration options."
  :group '+sidebar
  :prefix "+sidebar:")

(defcustom +sidebar-position 'left
  "Position of sidebar buffer.

Valid values are
 * left,
 * right."
  :type '(choice (const left)
                 (const right))
  :group '+sidebar)

(defcustom +sidebar-default-init '+sidebar:default-init
  "Default sidebar initialization function."

  :type '(sexp)
  :group 'sidebar)

(defcustom +sidebar-buffer-init-alist nil
  "Default sidebar initialization alist.
This attempts to match the current buffer name to the sidebar buffer initialization
function. This is used to display specific content based on the current buffer name."

  :type '(alist :key-type regexp :value-type sexp)
  :group 'sidebar)

(defgroup +sidebar-window nil
  "Customisations for the behaviour of the sidebar window."
  :group '+sidebar
  :prefix "+sidebar:")

(defcustom +sidebar-width 35
  "Width of the sidebar window."
  :type 'integer
  :group 'sidebar-window)

(defcustom +sidebar-no-delete-other-windows t
  "When non-nil sidebar will have the `no-delete-other-windows' parameter.
This parameter prevents the sidebar window from closing when calling
`delete-other-windows' or when a command like `magit-status' would launch a new
full-screen buffer."
  :type 'boolean
  :group '+sidebar-window)

(defcustom +sidebar-is-never-other-window t
  "When non-nil sidebar will use the `no-other-window' parameter.

In practice it means that sidebar will become invisible to commands like
`other-window' or `evil-window-left'."
  :type 'boolean
  :group '+sidebar-window)

(defcustom +sidebar-window-background-color nil
  "Custom background colours for the sidebar window.
Value must be a cons cell consisting of two colours: first the background of the
sidebar window proper, then a second colour for sidebar' `hl-line' overlay
marking the selected line."
  :type '(cons color color)
  :group '+sidebar-window)

(defvar +sidebar:buffer-name-prefix " *Sidebar Buffer"
  "The prefix for the sidebar buffer.")

(defvar +sidebar:buffer-name " *Sidebar Buffer*"
  "The name of the sidebar buffer")

(defvar +sidebar:buffer nil
  "The buffer containing the sidebar content.")

(defvar-local +sidebar:in-this-buffer nil
  "Non-nil only in buffers meant to show sidebar.
Used to show an error message if someone mistakenly activates `sidebar-mode'.")

(defvar +sidebar:debug t)
;;;
;;; Utility
;;;

(defun +sidebar:log (message &optional args)
  (when +sidebar:debug
    (message message args)))

(defun +sidebar:on-window-config-change ()
  "Collects all tasks that need to run on a window config change."
  (-when-let (w (+sidebar:get-local-window))
    (with-selected-window w
       ;; apparently keeping the hook around can lead to a feeback loop together with helms
       ;; auto-resize mode as seen in https://github.com/Alexander-Miller/treemacs/issues/76
       (let (window-configuration-change-hook)
         (set-window-parameter w 'no-delete-other-windows +sidebar-no-delete-other-windows)
         (set-window-parameter w 'window-side +sidebar-position)
         (set-window-parameter w 'window-slot 0)
         (when +sidebar-is-never-other-window
           (set-window-parameter w 'no-other-window t))))))

(defun +sidebar:set-width (width)
  "Set the width of the sidebar buffer to WIDTH."
  (+sidebar:log "+sidebar:set-width(%s)" width)
  (unless (one-window-p)
    (let ((window-size-fixed)
          (w (max width window-min-width)))
      (cond
       ((> (window-width) w)
        (shrink-window-horizontally  (- (window-width) w)))
       ((< (window-width) w)
        (enlarge-window-horizontally (- w (window-width))))))))

;;;
;;; Sidebar Buffer
;;;

(defun +sidebar:get-local-buffer ()
  "Return the sidebar buffer."
  ;; (declare (side-effect-free t))
  (+sidebar:log "+sidebar:get-local-buffer")
  (and (buffer-live-p +sidebar:buffer) +sidebar:buffer))

(defun +sidebar:get-local-buffer-create ()
  "Get the buffer, creating a new one if needed."
  (+sidebar:log "+sidebar:get-local-buffer-create")
  (or (+sidebar:get-local-buffer)
      (+sidebar:create-buffer)))

(defun +sidebar:create-buffer ()
  "Create and store a new buffer."
  (+sidebar:log "+sidebar:create-buffer")
  (let ((buffer (get-buffer-create +sidebar:buffer-name)))
    (setq +sidebar:buffer buffer)))

(defun +sidebar:setup-buffer ()
  "Create and setup a buffer for sidebar in the right position and size."
  (+sidebar:log "+sidebar:setup-buffer")
  (let ((init-fn (+sidebar:get-init)))
    (-if-let (lv-buffer (-some->
                          (--find (string= " *LV*" (buffer-name (window-buffer it)))
                                  (window-list (selected-frame))))
                        (window-buffer))
        (progn
          ;; workaround for LV windows like spacemacs' transient states preventing
          ;; side windows from popping up right
          ;; see https://github.com/abo-abo/hydra/issues/362
          (setf (buffer-local-value 'window-size-fixed lv-buffer) nil)
          (+sidebar:log "+sidebar:setup-buffer using lv-buffer")
          (+sidebar:popup-window)
          (setf (buffer-local-value 'window-size-fixed lv-buffer) t))
      (+sidebar:log "+sidebar:setup-buffer not using lv-buffer")
      (+sidebar:popup-window))
    (let ((inhibit-read-only t))
      (with-current-buffer +sidebar:buffer
        (+sidebar:log "the init function is %s" init-fn)
        (funcall init-fn)))
    (set-window-dedicated-p (selected-window) t)
    (setq-local +sidebar:in-this-buffer t)
    (let ((window-size-fixed))
      (+sidebar:set-width +sidebar-width))))

(defun +sidebar:on-buffer-kill ()
  "Cleanup to run when a sidebar buffer is killed."
  (+sidebar:log "+sidebar:on-buffer-kill")
  (mac-auto-operator-composition-mode 1)
  (setq +sidebar:buffer nil))

;;;
;;; Sidebar Window/Buffer Visibility
;;;

(defun +sidebar:get-local-window ()
  "Return the window displaying the sidebar buffer in the current frame.
Returns nil if no sidebar buffer is visible."
  (declare (side-effect-free error-free))
  (->> (window-list (selected-frame))
       (--first (->> it
                     (window-buffer)
                     (buffer-name)
                     (s-starts-with? +sidebar:buffer-name-prefix)))))

(defun +sidebar:select-visible-window ()
  "Switch to sidebar buffer, given that it is currently visible."
  (+sidebar:log "+sidebar:select-visible-window")
  (-some--> (+sidebar:get-local-buffer)
            (get-buffer-window)
            (select-window)))

(define-inline +sidebar:select-not-visible-window ()
  "Switch to sidebar buffer, given that it not visible."
  (inline-quote
   (+sidebar:setup-buffer)))

(define-inline +sidebar:current-visibility ()
  "Return whether the current visibility state of the sidebar buffer.
Valid states are 'visible, 'exists and 'none."
  (declare (side-effect-free t))
  (inline-quote
   (cond
    ((+sidebar:get-local-window) 'visible)
    ((+sidebar:get-local-buffer) 'exists)
    (t 'none))))

(defun +sidebar:visible-p ()
  "Returns true if the sidebar is visible."
  (declare (side-effect-free t))
  (member (+sidebar:current-visibility) '(visible)))

;;;
;;; Sidebar Window
;;;

(defun +sidebar:popup-window ()
  "Pop up a side window and buffer for sidebar."
  (+sidebar:log "+sidebar:popup-window")
  (-> (+sidebar:get-local-buffer-create)
    (display-buffer-in-side-window `((side . ,+sidebar-position)))
    (select-window)))


;;;
;;; Sidebar Init
;;;

(defun +sidebar:init ()
  "Create the buffer that will hold the sidebar content."
  (+sidebar:log "+sidebar:init")
  (let ((origin-buffer (current-buffer))
        (visibility (+sidebar:current-visibility)))
    (pcase visibility
      ('visible (+sidebar:select-visible-window))
      ('exists (+sidebar:select-not-visible-window))
      ('none
       (+sidebar:setup-buffer)
       (+sidebar-mode)
       ;; Render the Sidebar content
       ;; (goto-char 2)
       (message "buffer created")))))

;; TODO: Don't use : in variable names and rename this to
;; +sidebar:default-init
(defun +sidebar:default-init ()
  "Default sidebar initialization function."
  (erase-buffer)
  (insert "* Sidebar Buffer\n")
  (org-mode)
  (+sidebar-mode))

(defun +sidebar:get-init (&optional buffer)
  "Get the initialization function for the buffer based on the +sidebar-buffer-alist.

BUFFER is the name of the buffer, defaults to the current buffer."
  (let* ((name (buffer-name buffer))
         (init +sidebar-default-init))
    ;; Find the init function to use for the current buffer. The +sidebar-buffer-alist
    ;; provides a list of regex's to match against the current buffer name and will use
    ;; the associated init function for the first match.
    (message "getting init function for buffer %s" name)
    (message "  alist %s" +sidebar-buffer-init-alist)
    (dolist (buffer-init +sidebar-buffer-init-alist)
      (-when-let* ((buffer-init-regex (car buffer-init))
                   (buffer-init-function (cdr buffer-init))
                   (match-index (string-match buffer-init-regex name)))
        (setq init buffer-init-function)))
    init))

;;;
;;; Sidebar Mode
;;;

;;;###autoload
(defun +sidebar:select-window ()
  "Select the sidebar window if it is visible.
Bring it to the foreground if it is not visible.
Initialise a new sidebar buffer as calling `sidebar' would if there is no
sidebar buffer for this frame."
  (interactive)
  (pcase (+sidebar:current-visibility)
    ('visible (+sidebar:select-visible-window))
    ('exists  (+sidebar:select-not-visible-window))
    ('none    (+sidebar:init))))

;;;###autoload
(defun +sidebar ()
  "Initialise or toggle sidebar.
* If the sidebar window is visible hide it.
* If a sidebar buffer exists, but is not visible show it.
* If no sidebar buffer exists for the current frame create and show it."
  (interactive)
  (pcase (+sidebar:current-visibility)
    ('visible (delete-window (+sidebar:get-local-window)))
    ('exists  (+sidebar:select-window))
    ('none    (+sidebar:init))))

;;;###autoload
(defun +sidebar:open ()
  "Show the sidebar or create it if it is missing.
* If the sidebar window is visible do nothing.
* If a sidebar buffer exists, but is not visible show it.
* If no sidebar buffer exists for the current frame create and show it."
  (interactive)
  (pcase (+sidebar:current-visibility)
    ('exists (+sidebar:select-window))
    ('none   (+sidebar:init))))

;;;###autoload
(defun +sidebar:close ()
  "Close the sidebar.
* If the sidebar window is visible hide it.
* If a sidebar buffer exists, but is not visible do nothing.
* If no sidebar buffer exists for the current frame do nothing."
  (interactive)
  (pcase (+sidebar:current-visibility)
    ('visible (delete-window (+sidebar:get-local-window)))))

;;;###autoload
(define-derived-mode +sidebar-mode org-mode "Sidebar"
  "A major mode for displaying sidebar content."

  (setq buffer-read-only         t
        truncate-lines           t
        indent-tabs-mode         nil
        desktop-save-buffer      nil
        ;; window-size-fixed        (when treemacs--width-is-locked 'width)
        +sidebar:in-this-buffer t)

  (setq cursor-type nil)
  ;; (when (boundp 'evil-treemacs-state-cursor)
  ;;   (with-no-warnings
  ;;     (setq evil-treemacs-state-cursor'(hbar . 0))))

  ;; higher fuzz value makes it less likely to start a mouse drag
  ;; and make a switch to visual state
  (setq-local double-click-fuzz 15)
  (setq-local show-paren-mode nil)
  (setq-local tab-width 1)
  (electric-indent-local-mode -1)
  (visual-line-mode -1)
  (font-lock-mode -1)
  (jit-lock-mode nil)
  (mac-auto-operator-composition-mode -1)
  (buffer-disable-undo)
  ;; fringe indicator must be set up right here, before hl-line-mode, since activating hl-line-mode will
  ;; invoke the movement of the fringe overlay that would otherwise be nil
  ;; (when treemacs-fringe-indicator-mode
  ;;   (treemacs--enable-fringe-indicator))
  (hl-line-mode t)

  ;; needs to run manually the first time treemacs is loaded, since the hook is only added *after*
  ;; the window config was changed to show treemacs
  (unless (member #'+sidebar:on-window-config-change (default-value 'window-configuration-change-hook))
    (+sidebar:on-window-config-change))
  ;; set the parameter immediately so it can take effect when `treemacs' is called programatically
  ;; alongside other window layout chaning commands that might delete it again
  (set-window-parameter (selected-window) 'no-delete-other-windows +sidebar-no-delete-other-windows)

  (setq header-line-format " ")

  (when +sidebar-window-background-color
    (face-remap-add-relative 'default :background (car +sidebar-window-background-color))
    (face-remap-add-relative 'fringe  :background (car +sidebar-window-background-color))
    (face-remap-add-relative 'hl-line :background (cdr +sidebar-window-background-color)))

  (face-remap-add-relative 'italic :slant 'normal :foreground (doom-color 'blue))
  (face-remap-add-relative 'org-link :underline nil :foreground (doom-lighten (doom-color 'blue) 0.2))
  (face-remap-add-relative 'highlight :background (doom-color 'bg))

  (add-hook 'window-configuration-change-hook #'+sidebar:on-window-config-change)
  (add-hook 'kill-buffer-hook #'+sidebar:on-buffer-kill nil t))

(provide '+sidebar-mode)
