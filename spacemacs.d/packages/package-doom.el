(require 'doom-themes)
(require 'helm-bookmark)
;;; Settings (defaults)
(setq doom-enable-bold t    ; if nil, bolding are universally disabled
      doom-enable-italic t  ; if nil, italics are universally disabled

      ;; doom-one specific settings
      doom-one-brighter-modeline nil
      doom-one-brighter-comments nil

      doom-nord-region-highlight 'frost
      doom-nord-brighter-modeline t
      doom-nord-brighter-comments nil

      doom-nord-padded-modeline t
      )

;; Load the theme (doom-one, doom-dark, etc.)

(defadvice load-theme (before theme-dont-propagate activate)
  (mapcar #'disable-theme custom-enabled-themes))

(if (display-graphic-p)
    (load-theme 'doom-nord t)
  (load-theme 'doom-one t))

;; ;;; OPTIONAL
;; brighter source buffers (that represent files)
;; (add-hook 'find-file-hook 'doom-buffer-mode-maybe)
;; ;; if you use auto-revert-mode
;; (add-hook 'after-revert-hook 'doom-buffer-mode-maybe)
;; ;; you can brighten other buffers (unconditionally) with:
;; (add-hook 'ediff-prepare-buffer-hook 'doom-buffer-mode)


(require 'solaire-mode)

;; brighten buffers (that represent real files)
(add-hook 'change-major-mode-hook #'turn-on-solaire-mode)
;; To enable solaire-mode unconditionally for certain modes:
(add-hook 'ediff-prepare-buffer-hook #'solaire-mode)

;; ...if you use auto-revert-mode, this prevents solaire-mode from turning
;; itself off every time Emacs reverts the file
(add-hook 'after-revert-hook #'turn-on-solaire-mode)

;; highlight the minibuffer when it is activated:
(add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)

;; if the bright and dark background colors are the wrong way around, use this
;; to switch the backgrounds of the `default` and `solaire-default-face` faces.
;; This should be used *after* you load the active theme!
;;
;; NOTE: This is necessary for themes in the doom-themes package!
;; (solaire-mode-swap-bg)

(provide 'package-doom)
