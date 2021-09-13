;;; ~/.doom.d/snippets/+mu4u.el -*- lexical-binding: t -*-

;;;
;;; Packages
;;;

(use-package! mu4e-thread-folding
  :config
  (add-to-list 'mu4e-header-info-custom
               '(:empty . (:name "Empty"
                           :shortname ""
                           :function (lambda (msg) " "))))
  (setq mu4e-headers-fields
        '((:empty          . 2)
          (:account-stripe . 1)
          ;; (:flags          . 6) ; 3 icon flags
          ;; (:mailing-list  .   20)
          (:from-or-to     . 20)
          (:human-date     . 12)
          (:subject        . nil)))

  (setq mu4e-headers-precise-alignment nil)

  ;; Remove the count for threads
  (setq mu4e-thread-folding-root-folded-prefix-string "▸")
  (setq mu4e-thread-folding-root-unfolded-prefix-string "▾")

  (map! :map mu4e-headers-mode-map :nvg "<tab>"     'mu4e-headers-toggle-at-point)
  (map! :map mu4e-headers-mode-map :nvg "<left>"    'mu4e-headers-fold-at-point)
  (map! :map mu4e-headers-mode-map :nvg "<S-left>"  'mu4e-headers-fold-all)
  (map! :map mu4e-headers-mode-map :nvg "<right>"   'mu4e-headers-unfold-at-point)
  (map! :map mu4e-headers-mode-map :nvg "<S-right>" 'mu4e-headers-unfold-all)
  (map! :localleader (:map mu4e-headers-mode-map :nvg "<tab>"     'mu4e-headers-toggle-at-point))
  (map! :localleader (:map mu4e-headers-mode-map :nvg "<left>"    'mu4e-headers-fold-at-point))
  (map! :localleader (:map mu4e-headers-mode-map :nvg "<S-left>"  'mu4e-headers-fold-all))
  (map! :localleader (:map mu4e-headers-mode-map :nvg "<right>"   'mu4e-headers-unfold-at-point))
  (map! :localleader (:map mu4e-headers-mode-map :nvg "<S-right>" 'mu4e-headers-unfold-all))

  ;; (setq mu4e-headers-found-hook '(mu4e-headers-mark-thread))
  (add-hook! 'mu4e-headers-found-hook :append #'mu4e-headers-fold-all)

  (mu4e-thread-folding-mode))



(use-package! per-buffer-theme
  :config
  (setq per-buffer-theme/use-timer t)
  ;; Reduce to 0.5 if causing performance issues
  (setq per-buffer-theme/timer-idle-delay 0.1)
  (setq per-buffer-theme/default-theme 'doom-vilebloom)
  (setq per-buffer-theme/themes-alist nil)
  ;; (setq per-buffer-theme/themes-alist
  ;;       '(((:theme . doom-vilebloom-light)
  ;;          (:buffernames . ("[[:space:]]*Treemacs-Scoped-Buffer-Perspective"))
  ;;          (:modes . (mu4e-headers-mode mu4e:main-mode mu4e-view-mode +sidebar-mode treemacs-mode)))))
  (setq per-buffer-theme/ignored-buffernames-regex '())
  ;; (setq per-buffer-theme/ignored-buffernames-regex
  ;;       (append ido-ignore-buffers '("*info*" "*Warnings*")
  ;;               per-buffer-theme/ignored-buffernames-regex))
  (per-buffer-theme-mode))

;; (use-package! nano-sidebar)

(use-package! mu4e-dashboard)
;;;
;;; Config
;;;

;; Configure using msmtp
(setq sendmail-program "/usr/local/bin/msmtp")
(setq send-mail-function #'smtpmail-send-it)
(setq message-sendmail-extra-arguments '("--read-envelope-from"))
(setq message-send-mail-function #'message-send-mail-with-sendmail)

;; Setup accounts
(set-email-account! "Procore.com"
                    '((mu4e-sent-folder       . "/Procore.com/Sent Mail")
                      (mu4e-drafts-folder     . "/Procore.com/Drafts")
                      (mu4e-trash-folder      . "/Procore.com/Bin")
                      (mu4e-refile-folder     . "/Procore.com/All Mail")
                      (smtpmail-smtp-user     . "jordan.faust@procore.com")
                      (user-mail-address      . "jordan.faust@procore.com") ;; only needed for mu < 1.4
                      (mu4e-compose-signature . "---\nJordan Faust"))
                    t)

(setq +mu4e-header--maildir-colors '(("Procore.com" . all-the-icons-yellow)))
(setq +mu4e-gmail-accounts '(("jordan.faust@procore.com" . "~/Mail/Procore.com")))

;; don't need to run cleanup after indexing for gmail
(setq mu4e-index-cleanup nil)
;; because gmail uses labels as folders we can use lazy check since messages
;; don't really "move"
(setq mu4e-index-lazy-check t)

;; Change the order of colors for account marking
(setq +mu4e-header-colorized-faces
      '(all-the-icons-yellow
        all-the-icons-lblue
        all-the-icons-purple-alt
        all-the-icons-blue-alt
        all-the-icons-purple))

(defun +mu4e-open-inbox ()
  "Launch a view of the mu4e inbox with the sidebar dashboard."
  (interactive)
  (if (or (member major-mode '(+sidebar-mode mu4e-headers-mode mu4e:main-mode mu4e-view-mode))
          (get-buffer "*mu4e-headers*")
          (+sidebar:visible-p))
      (progn
        ;; Close the sidebar
        (+sidebar:close)
        ;; Stop the mu server, freeing the lock on the files, and kill all associated buffers
        (mu4e~stop))
    ;; Start the mu server and open the inbox view
    (mu4e~start (mu4e-headers-search "m:/Procore.com/INBOX"))
    ;; Open the sidebar
    (+sidebar:open)
    ;; Switch back to the mu4e buffer
    (switch-to-buffer "*mu4e-headers*")))

;;;
;;; Requires
;;;

;;;
;;; Keybinds
;;;

(map! :leader :nvg "e" '+mu4e-open-inbox)
