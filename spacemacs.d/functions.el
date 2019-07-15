(defmacro add-hook* (mode &rest body)
  `(add-hook ,mode (lambda () ,@body)))

(defmacro ensure-clone (user project branch)
  (unless (file-exists-p (format "~/.spacemacs.d/%s" project))
    (shell-command (format "cd ~/.spacemacs.d && git clone -b %s git@github.com:%s/%s" branch user project))))

(defun capitalize-previous-word ()
  (interactive)
  (save-excursion
    (backward-word)
    (capitalize-word 1)))

(defun eww-other-window (url)
  (interactive)
  (view-buffer-other-window "*DocBuffer*")
  (eww url))

(defun evil-normal-state-and-save ()
  (interactive)
  (evil-normal-state)
  (save-buffer))

(defun indent-buffer-on-save ()
  (if (member major-mode indent-buffer-modes)
      (spacemacs/indent-region-or-buffer)))

(defun my/helm-exit-minibuffer ()
  (interactive)
  (helm-exit-minibuffer))

(defun require-template (symbol)
  `(require (quote ,symbol)))

(defmacro load-my-packages ()
  (let* ((filter-fn (lambda (x) (not (member x '("." "..")))))
         (strip-ext-fn (lambda (x) (s-left -3 x)))
         (all-files (directory-files "~/.spacemacs.d/packages/"))
         (package-files-ext (-filter filter-fn all-files))
         (package-files (-map strip-ext-fn package-files-ext))
         (package-symbols (-map 'read package-files))
         (package-requires (-map 'require-template package-symbols)))
    `(progn ,@package-requires)))

(defmacro after! (feature &rest forms)
  "A smart wrapper around `with-eval-after-load'. Supresses warnings during
compilation."
  (declare (indent defun) (debug t))
  `(,(if (or (not (bound-and-true-p byte-compile-current-file))
             (if (symbolp feature)
                 (require feature nil :no-error)
               (load feature :no-message :no-error)))
         #'progn
       #'with-no-warnings)
    (with-eval-after-load ',feature ,@forms)))

(defmacro add-hook! (&rest args)
  "A convenience macro for `add-hook'. Takes, in order:
  1. Optional properties :local and/or :append, which will make the hook
     buffer-local or append to the list of hooks (respectively),
  2. The hooks: either an unquoted major mode, an unquoted list of major-modes,
     a quoted hook variable or a quoted list of hook variables. If unquoted, the
     hooks will be resolved by appending -hook to each symbol.
  3. A function, list of functions, or body forms to be wrapped in a lambda.
Examples:
    (add-hook! 'some-mode-hook 'enable-something)
    (add-hook! some-mode '(enable-something and-another))
    (add-hook! '(one-mode-hook second-mode-hook) 'enable-something)
    (add-hook! (one-mode second-mode) 'enable-something)
    (add-hook! :append (one-mode second-mode) 'enable-something)
    (add-hook! :local (one-mode second-mode) 'enable-something)
    (add-hook! (one-mode second-mode) (setq v 5) (setq a 2))
    (add-hook! :append :local (one-mode second-mode) (setq v 5) (setq a 2))
Body forms can access the hook's arguments through the let-bound variable
`args'."
  (declare (indent defun) (debug t))
  (let ((hook-fn 'add-hook)
        append-p local-p)
    (while (keywordp (car args))
      (pcase (pop args)
        (:append (setq append-p t))
        (:local  (setq local-p t))
        (:remove (setq hook-fn 'remove-hook))))
    (let ((hooks (doom--resolve-hooks (pop args)))
          (funcs
           (let ((val (car args)))
             (if (memq (car-safe val) '(quote function))
                 (if (cdr-safe (cadr val))
                     (cadr val)
                   (list (cadr val)))
               (list args))))
          forms)
      (dolist (fn funcs)
        (setq fn (if (symbolp fn)
                     `(function ,fn)
                   `(lambda (&rest _) ,@args)))
        (dolist (hook hooks)
          (push (cond ((eq hook-fn 'remove-hook)
                       `(remove-hook ',hook ,fn ,local-p))
                      (t
                       `(add-hook ',hook ,fn ,append-p ,local-p)))
                forms)))
      `(progn ,@(nreverse forms)))))

(defun doom--resolve-hooks (hooks)
  (let ((quoted-p (eq (car-safe hooks) 'quote)))
    (when quoted-p
      (setq hooks (cadr hooks)))
    (cl-loop with hooks = (if (listp hooks) hooks (list hooks))
             for hook in hooks
             if (eq (car-safe hook) 'quote)
             collect (cadr hook)
             else if quoted-p
             collect hook
             else collect (intern (format "%s-hook" (symbol-name hook))))))

(provide 'functions)
