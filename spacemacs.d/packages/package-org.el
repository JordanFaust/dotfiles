;; -*- no-byte-compile: t; -*-
;;; lang/org/packages.el

;;
;; Package Init
;;

(defvar +org-dir (expand-file-name "~/org/")
  "The directory where org files are kept.")

;;
;; Autoloads
;;

;;;###autoload
(define-minor-mode +org-pretty-mode
  "TODO"
  :init-value nil
  :lighter " *"
  :group 'evil-org
  (setq org-hide-emphasis-markers +org-pretty-mode)
  (org-toggle-pretty-entities)
  (org-with-silent-modifications
   ;; In case the above un-align tables
   (org-table-map-tables 'org-table-align t)))

;;;###autoload
(defun +org|update-cookies ()
  "Update counts in headlines (aka \"cookies\")."
  (when (and buffer-file-name (file-exists-p buffer-file-name))
    (org-update-statistics-cookies t)))

;;;###autoload
(defun +org/dwim-at-point ()
  "Do-what-I-mean at point.
If on a:
- checkbox list item or todo heading: toggle it.
- clock: update its time.
- headline: toggle latex fragments and inline images underneath.
- footnote definition: jump to the footnote
- table-row or a TBLFM: recalculate the table's formulas
- table-cell: clear it and go into insert mode. If this is a formula cell,
  recaluclate it instead.
- babel-call: execute the source block
- statistics-cookie: update it.
- latex fragment: toggle it.
- link: follow it
- otherwise, refresh all inline images in current tree."
  (interactive)
  (let* ((scroll-pt (window-start))
         (context (org-element-context))
         (type (org-element-type context)))
    ;; skip over unimportant contexts
    (while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
      (setq context (org-element-property :parent context)
            type (org-element-type context)))
    (pcase type
      ((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
       (let ((match (and (org-at-item-checkbox-p) (match-string 1))))
         (org-toggle-checkbox (if (equal match "[ ]") '(16)))))

      (`headline
       (cond ((org-element-property :todo-type context)
              (org-todo
               (if (eq (org-element-property :todo-type context) 'done) 'todo 'done)))
             ((string= "ARCHIVE" (car-safe (org-get-tags)))
              (org-force-cycle-archived))
             (t
              (org-remove-latex-fragment-image-overlays)
              (org-toggle-latex-fragment '(4)))))

      (`clock (org-clock-update-time-maybe))

      (`footnote-definition
       (goto-char (org-element-property :post-affiliated context))
       (call-interactively #'org-footnote-action))

      ((or `planning `timestamp)
       (org-follow-timestamp-link))

      ((or `table `table-row)
       (if (org-at-TBLFM-p)
           (org-table-calc-current-TBLFM)
         (ignore-errors
           (save-excursion
             (goto-char (org-element-property :contents-begin context))
             (org-call-with-arg 'org-table-recalculate (or arg t))))))

      (`table-cell
       (org-table-blank-field)
       (org-table-recalculate)
       (when (and (string-empty-p (string-trim (org-table-get-field)))
                  (bound-and-true-p evil-mode))
         (evil-change-state 'insert)))

      (`babel-call
       (org-babel-lob-execute-maybe))

      (`statistics-cookie
       (save-excursion (org-update-statistics-cookies nil)))

      ((or `src-block `inline-src-block)
       (org-babel-execute-src-block))

      ((or `latex-fragment `latex-environment)
       (org-toggle-latex-fragment))

      (`link
       (let ((path (org-element-property :path (org-element-lineage context '(link) t))))
         (if (and path (image-type-from-file-name path))
             (+org/refresh-inline-images)
           (org-open-at-point))))

      (_ (+org/refresh-inline-images)))
    (set-window-start nil scroll-pt)))

;;;###autoload
(defun +org/indent ()
  "Indent the current item (header or item). Otherwise, forward to
`self-insert-command'."
  (interactive)
  (cond ((org-at-item-p)
         (org-indent-item-tree))
        ((org-at-heading-p)
         (ignore-errors (org-demote)))
        ((org-in-src-block-p t)
         (doom/dumb-indent))
        (t
         (call-interactively #'self-insert-command))))

;;;###autoload
(defun +org/indent-or-next-field-or-yas-expand ()
  "Depending on the context either a) indent the current line, b) go the next
table field or c) run `yas-expand'."
  (interactive)
  (call-interactively
   (cond ((and (bound-and-true-p yas-minor-mode)
               (yas--templates-for-key-at-point))
          #'yas-expand)
         ((org-at-table-p)
          #'org-table-next-field)
         (t
          #'+org/indent))))

;;;###autoload
(defun +org/dedent ()
  "Dedent the current item (header or item). Otherwise, forward to
`self-insert-command'."
  (interactive)
  (cond ((org-at-item-p)
         (org-list-indent-item-generic
          -1 nil
          (save-excursion
            (when (org-region-active-p)
              (goto-char (region-beginning)))
            (org-list-struct))))
        ((org-at-heading-p)
         (ignore-errors (org-promote)))
        (t
         (call-interactively #'self-insert-command))))

;;;###autoload
(defun +org/dedent-or-prev-field ()
  "Depending on the context either dedent the current item or go the previous
table field."
  (interactive)
  (call-interactively
   (if (org-at-table-p)
       #'org-table-previous-field
     #'+org/dedent)))

;;;###autoload
(defun +org/insert-item (direction)
  "Inserts a new heading, table cell or item, depending on the context.
DIRECTION can be 'above or 'below.
I use this instead of `org-insert-item' or `org-insert-heading' which are too
opinionated and perform this simple task incorrectly (e.g. whitespace in the
wrong places)."
  (interactive)
  (let* ((context (org-element-lineage
                   (org-element-context)
                   '(table table-row headline inlinetask item plain-list)
                   t))
         (type (org-element-type context)))
    (cond ((memq type '(item plain-list))
           (let ((marker (org-element-property :bullet context))
                 (pad (save-excursion
                        (back-to-indentation)
                        (- (point) (line-beginning-position)))))
             (pcase direction
               ('below
                (org-end-of-item)
                (goto-char (line-beginning-position))
                (insert (make-string pad 32) (or marker ""))
                (save-excursion (insert "\n")))
               ('above
                (goto-char (line-beginning-position))
                (insert (make-string pad 32) (or marker ""))
                (save-excursion (insert "\n")))))
           (when (org-element-property :checkbox context)
             (insert "[ ] ")))

          ((memq type '(table table-row))
           (pcase direction
             ('below (org-table-insert-row t))
             ('above (org-shiftmetadown))))

          ((memq type '(headline inlinetask))
           (let ((level (if (eq (org-element-type context) 'headline)
                            (org-element-property :level context)
                          1)))
             (pcase direction
               ('below
                (let ((at-eol (= (point) (1- (line-end-position)))))
                  (goto-char (line-end-position))
                  (org-end-of-subtree)
                  (insert (concat "\n"
                                  (when (= level 1)
                                    (if at-eol
                                        (ignore (cl-incf level))
                                      "\n"))
                                  (make-string level ?*)
                                  " "))))
               ('above
                (org-back-to-heading)
                (org-insert-heading)
                (when (= level 1)
                  (save-excursion (insert "\n")))))
             (when (org-element-property :todo-type context)
               (org-todo 'todo))))

          (t (user-error "Not a valid list, heading or table")))

    (when (bound-and-true-p evil-mode)
      (evil-append-line 1))))

;;;###autoload
(defun +org-get-property (name &optional _file) ; TODO Add FILE
  "Get a propery from an org file."
  (save-excursion
    (goto-char 1)
    (re-search-forward (format "^#\\+%s:[ \t]*\\([^\n]+\\)" (upcase name)) nil t)
    (buffer-substring-no-properties (match-beginning 1) (match-end 1))))

;;;###autoload
(defun +org/refresh-inline-images ()
  "Refresh image previews in the current heading/tree."
  (interactive)
  (if (> (length org-inline-image-overlays) 0)
      (org-remove-inline-images)
    (org-display-inline-images
     t t
     (if (org-before-first-heading-p)
         (line-beginning-position)
       (save-excursion (org-back-to-heading) (point)))
     (if (org-before-first-heading-p)
         (line-end-position)
       (save-excursion (org-end-of-subtree) (point))))))

;;;###autoload
(defun +org/toggle-checkbox ()
  "Toggle the presence of a checkbox in the current item."
  (interactive)
  (org-toggle-checkbox '(4)))

;;;###autoload
(defun +org/toggle-fold ()
  "Toggle the local fold at the point (as opposed to cycling through all levels
with `org-cycle'). Also:
  + If in a babel block, removes result blocks.
  + If in a table, realign it, if necessary."
  (interactive)
  (save-excursion
    (org-beginning-of-line)
    (cond ((org-at-table-p)
           (org-table-align))
          ((org-in-src-block-p)
           (org-babel-remove-result))
          ((org-at-heading-p)
           (outline-toggle-children))
          ((org-at-item-p)
           (let ((window-beg (window-start)))
             (org-cycle)
             (set-window-start nil window-beg))))))

;;;###autoload
(defun +org/remove-link ()
  "Unlink the text at point."
  (interactive)
  (unless (org-in-regexp org-bracket-link-regexp 1)
    (user-error "No link at point"))
  (save-excursion
    (let ((remove (list (match-beginning 0) (match-end 0)))
          (description (if (match-end 3)
                           (match-string-no-properties 3)
                         (match-string-no-properties 1))))
      (apply #'delete-region remove)
      (insert description))))


;;
;; Bootstrap
;;

(add-hook! org-mode
  #'(;;doom|disable-line-numbers  ; no line numbers
     org-bullets-mode           ; "prettier" bullets
     org-indent-mode            ; margin-based indentation
     toc-org-enable             ; auto-table of contents
     visual-line-mode           ; line wrapping

     +org|enable-auto-update-cookies
     +org|smartparens-compatibility-config
     +org|unfold-to-2nd-level-or-point
     +org|show-paren-mode-compatibility
     ))

;;
;; Config hooks
;;

(defun +org|unfold-to-2nd-level-or-point ()
  "My version of the 'overview' #+STARTUP option: expand first-level headings.
Expands the first level, but no further. If point was left somewhere deeper,
unfold to point on startup."
  (unless org-agenda-inhibit-startup
    (when (eq org-startup-folded t)
      (outline-hide-sublevels 2))
    (when (outline-invisible-p)
      (ignore-errors
        (save-excursion
          (outline-previous-visible-heading 1)
          (org-show-subtree))))))

(defun +org|smartparens-compatibility-config ()
  "Instruct `smartparens' not to impose itself in org-mode."
  (defun +org-sp-point-in-checkbox-p (_id action _context)
    (when (eq action 'insert)
      (sp--looking-at-p "\\s-*]")))

  ;; make delimiter auto-closing a little more conservative
  (after! smartparens
    (sp-with-modes 'org-mode
      (sp-local-pair "*" nil :unless '(sp-point-after-word-p sp-point-before-word-p sp-point-at-bol-p))
      (sp-local-pair "_" nil :unless '(sp-point-after-word-p sp-point-before-word-p))
      (sp-local-pair "/" nil :unless '(sp-point-after-word-p sp-point-before-word-p +org-sp-point-in-checkbox-p))
      (sp-local-pair "~" nil :unless '(sp-point-after-word-p sp-point-before-word-p))
      (sp-local-pair "=" nil :unless '(sp-point-after-word-p sp-point-before-word-p)))))

(defun +org|enable-auto-update-cookies ()
  "Update statistics cookies when saving or exiting insert mode (`evil-mode')."
  (when (featurep 'evil)
    (add-hook 'evil-insert-state-exit-hook #'+org|update-cookies nil t))
  (add-hook 'before-save-hook #'+org|update-cookies nil t))

(defun +org|show-paren-mode-compatibility ()
  "`show-paren-mode' causes flickering with indentation margins made by
`org-indent-mode', so we simply turn off show-paren-mode altogether."
  (set (make-local-variable 'show-paren-mode) nil))


;;
(defun +org-init-ui ()
  "Configures the UI for `org-mode'."
  (setq-default
   org-adapt-indentation nil
   org-agenda-dim-blocked-tasks nil
   org-agenda-files (directory-files +org-dir t "\\.org$" t)
   org-agenda-inhibit-startup t
   org-agenda-skip-unavailable-files nil
   org-cycle-include-plain-lists t
   org-cycle-separator-lines 1
   org-entities-user '(("flat"  "\\flat" nil "" "" "266D" "♭") ("sharp" "\\sharp" nil "" "" "266F" "♯"))
   ;; org-ellipsis " ... "
   org-fontify-done-headline t
   org-fontify-quote-and-vers_e-blocks t
   org-fontify-whole-heading-line t
   org-footnote-auto-label 'plain
   org-hidden-keywords nil
   org-hide-emphasis-markers nil
   org-hide-leading-stars t
   org-hide-leading-stars-before-indent-mode t
   org-image-actual-width nil
   ;; org-indent-indentation-per-level 2
   org-indent-mode-turns-on-hiding-stars t
   org-pretty-entities nil
   org-pretty-entities-include-sub-superscripts t
   org-priority-faces
   `((?a . ,(face-foreground 'error))
     (?b . ,(face-foreground 'warning))
     (?c . ,(face-foreground 'success)))
   org-startup-folded t
   org-startup-indented t
   org-startup-with-inline-images nil
   org-tags-column 0
   org-todo-keywords
   '(;(sequence "[ ](t)" "[-](p)" "[?](m)" "|" "[X](d)")
     (sequence "TODO(T)" "|" "DONE(D)")
     (sequence "NEXT(n)" "ACTIVE(a)" "WAITING(w)" "LATER(l)" "|" "CANCELLED(c)"))
   org-use-sub-superscripts '{}
   outline-blank-line t

   ;; LaTeX previews are too small and usually render to light backgrounds, so
   ;; this enlargens them and ensures their background (and foreground) match the
   ;; current theme.
   ;; org-preview-latex-image-directory (concat doom-cache-dir "org-latex/")
   org-format-latex-options (plist-put org-format-latex-options :scale 1.5)
   org-format-latex-options
   (plist-put org-format-latex-options
              :background (face-attribute (or (cadr (assq 'default face-remapping-alist))
                                              'default)
                                          :background nil t))
   ;
   ; Org Caputure and Org Agenda Config
   ;

   org-capture-templates
   '(
     ("t" "Todo" entry
       (file+headline "~/org/inbox.org" "Tasks")
       "* [ ] %?\n%i")
     ("p" "Project" entry
       (file+headline "~/org/inbox.org" "Tasks")
       "* TODO Project: %? [/]\n%i")
     ("n" "Note/Data" entry
       (file+headline "~/org/inbox.org" "Notes/Data")
       "* %?   \n  %i\n  %u\n  %a")
     ("j" "Journal" entry (file+datetree "~/org/journal.org")
       "* %?\nEntered on %U\n %i\n %a")
     ("J" "Work-Journal" entry (file+datetree "~/org/wjournal.org")
       "* %?\nEntered on %U\n %i\n %a")
     )
   org-todo-keyword-faces
   (quote (("TODO" :foreground "#C16069" :weight bold)
           ("NEXT" :foreground "#86C0D1" :weight bold)
           ("DONE" :foreground "#A2BF8A" :weight light)
           ("WAITING_FOR" :foreground "indian red" :weight bold)
           ("DELEGATED" :foreground "indian red" :weight bold)
                                     ; ("HOLD" :foreground "magenta" :weight bold)
           ("CANCELLED" :foreground "light green" :weight light)))

   org-refile-targets '((org-agenda-files :maxlevel . 4))
   org-refile-allow-creating-parent-nodes 'confirm
   org-outline-path-complete-in-steps nil ; Refile in a single go
   org-refile-use-outline-path t
   org-enforce-todo-dependencies t

   ; Agenda Customization
   org-agenda-block-separator "--------------------------------------------------------------------------------"
   org-agenda-remove-tags t
   org-level-color-stars-only t
   org-hide-leading-stars t
   org-agenda-custom-commands
   '(("c" "Daily agenda and all TODOs"
       (
       (tags "PRIORITY=\"A\""
             ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
               (org-agenda-overriding-header "High-Priority Tasks:")))
       (tags "DEADLINE<=\"<now>\"|SCHEDULED<=\"<now>\""
             ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
               (org-agenda-overriding-header "Overdue Tasks:")))
       (agenda "" ((org-agenda-span 'day)
                   (org-agenda-use-time-grid t)
                   (org-agenda-time-grid
                     '((daily today)
                       (800 1000 1200 1400 1600 1800 2000)
                       "......"
                       "----------------"))
                   (org-agenda-overriding-header "Today's Schedule:")
                   (org-super-agenda-groups
                     '((:name "Today"
                             :time-grid t
                             :date today
                             :todo "TODAY"
                             :scheduled today
                             :order 1
                             )))))
       (alltodo ""
                 ((org-agenda-skip-function '(or (+org-agenda|skip-subtree-if-habit)
                                                 (+org-agenda|skip-subtree-if-priority ?A)
                                                 (+org-agenda|skip-unless-tag "NEXT")
                                                 (org-agenda-skip-if nil '(scheduled deadline))))
                 (org-agenda-overriding-header "Next Tasks:")))
       (alltodo ""
                 ((org-agenda-skip-function '(or (+org-agenda|skip-subtree-if-habit)
                                                 (+org-agenda|skip-unless-tag "Project")))
                 (org-agenda-overriding-header "Projects:")))
       (alltodo ""
                 ((org-agenda-skip-function '(or (+org-agenda|skip-subtree-if-habit)
                                                 (+org-agenda|skip-subtree-if-priority ?A)
                                                 (org-agenda-skip-entry-if 'todo '("NEXT"))
                                                 (org-agenda-skip-if nil '(regexp "Project"))
                                                 (org-agenda-skip-if nil '(scheduled deadline))))
                 (org-agenda-overriding-header "Normal-Priority Tasks:")))
       (agenda "" ((org-agenda-ndays 1)
                   (org-agenda-use-time-grid nil)
                   (org-agenda-overriding-header "Week At A Glance:"))))
       ((org-agenda-compact-blocks nil))))
   )

  ;; Custom links
  (org-link-set-parameters
   "org"
   :complete (lambda () (+org-link-read-file "org" +org-dir))
   :follow   (lambda (link) (find-file (expand-file-name link +org-dir)))
   :face     (lambda (link)
               (if (file-exists-p (expand-file-name link +org-dir))
                   'org-link
                 'error))))

(defun +org-init-keybinds ()
  "Sets up org-mode and evil keybindings. Tries to fix the idiosyncrasies
between the two."
  )

;;
(defun +org-hacks ()
  "Getting org to behave."
  ;; Don't open separate windows
  (push '(file . find-file) org-link-frame-setup)

  (defun +org|remove-occur-highlights ()
    "Remove org occur highlights on ESC in normal mode."
    (when (and (derived-mode-p 'org-mode)
               org-occur-highlights)
      (org-remove-occur-highlights)))
  (add-hook '+evil-esc-hook #'+org|remove-occur-highlights)

  (after! recentf
    ;; Don't clobber recentf with agenda files
    (defun +org-is-agenda-file (filename)
      (cl-find (file-truename filename) org-agenda-files
               :key #'file-truename
               :test #'equal))
    (add-to-list 'recentf-exclude #'+org-is-agenda-file))
  )

(after! org
  ;; Occasionally, Emacs encounters an error loading the built-in org, aborting
  ;; the load. This results in a broken, partially loaded state. This require
  ;; tries to set it straight.
  (require 'org)

  (defvaralias 'org-directory '+org-dir)

  ;; (org-crypt-use-before-save-magic)
  (+org-init-ui)
  (+org-init-keybinds)
  (+org-hacks))

;;
;; Org Agenda Functions
;;

(defun +org-agenda|skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(defun +org-agenda|skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

(defun +org-agenda|skip-unless-tag (tag)
  "Skip trees that don't have the passed tag"
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (re-search-forward (format "%s" tag) subtree-end t)
        nil          ; tag found, do not skip
      subtree-end))) ; tag not found, continue after end of subtree

;;
;; Org Agenda Config
;;

;; Based off of https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
(setq ; Capture Templates, Refile and Keywords
      )

(provide 'package-org)
