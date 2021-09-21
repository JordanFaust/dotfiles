;; -*- lexical-binding:t -*-
;; ibuffer configuration

(require 'sidebar-core)

(defun ibuffer-update-header-and-titles-a (format)
  (with-current-buffer "*Ibuffer*"
    (save-excursion
      (let ((inhibit-read-only t))
        ;; Remove header and insert ours
        (goto-char (point-min))
        (search-forward "-\n" nil t)
        (delete-region 1 (point))
        (goto-char (point-min))
        (insert (concat
                 (propertize "\n" 'face '(:height 1.2))
                 (propertize " "  'display `(raise +0.25))
                 (propertize " Buffers list (ibuffer)"
                             'face `(:height 1.1 :foreground ,(doom-color 'blue)))
                 (propertize " " 'display `(raise -0.35))
                 "\n"))
        (insert "")

        ;; Transform titles
        (goto-char (point-min))
        (while (re-search-forward "\\[ \\(.*\\)]" nil t)
          (let* ((title (match-string 0))
                 (property (get-text-property 0 'ibuffer-filter-group-name title)))
            (replace-match "\n")
            (insert (concat
                     (propertize
                      (format "❱  %s " (substring title 2 -2))
                      'ibuffer-filter-group-name property
                      'face `(:height 1.1 :foreground ,(doom-color 'fg)))
                     (propertize
                      (make-string (max 0 (- 30 (length title))) ?-)
                      'face `(:height 1.1 :foreground ,(doom-color 'base4)))
                     "\n"))))))))


(setq ibuffer-saved-filter-groups
       '(("home"
          ("Configuration" (or (filename . ".emacs.d")
                               (filename . "doom.d")))
          ("Org" (or (mode . org-mode)
                     (filename . "OrgMode")))
          ("Code" (or (derived-mode . prog-mode)
                      (mode . ess-mode)
                      (mode . compilation-mode)))
          ("Help" (or (name . "\*Help\*")
                      (name . "\*Apropos\*")
                      (name . "\*info\*"))))))
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-display-summary nil)
(setq ibuffer-use-header-line nil)
(setq ibuffer-eliding-string (propertize "..." 'face `(:foreground ,(doom-color 'fg))))
(setq ibuffer-formats
       '(("  " mark " " (name 24 24 :left :elide) "  " modified)
         (mark " " (name 16 -1) " " filename)))
(setq ibuffer-fontification-alist
      '((1 t all-the-icons-dblue)))

(defun +ibuffer-sidebar-init ()
  (erase-buffer)
  (ibuffer)
  (ibuffer-switch-to-saved-filter-groups "home")
  (ibuffer-auto-mode 1))

(advice-add 'ibuffer-update-title-and-summary :after #'ibuffer-update-header-and-titles-a)

(defun +ibuffer-sidebar-toggle ()
  (interactive)
  (let ((+sidebar-buffer-init-alist '((".*" . +ibuffer-sidebar-init)))
        (buffer (current-buffer)))
    (if (get-buffer "*Ibuffer*")
        (progn
          (kill-buffer "*Ibuffer*")
          (+sidebar:close))
      (+sidebar:open)
      (switch-to-buffer buffer))))

(provide 'sidebar-ibuffer)
