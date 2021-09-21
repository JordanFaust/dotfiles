;;; -*- lexical-binding: t -*-
;;; ~/.doom.d/snippets/+sidebar.el

;;;
;;; Requires
;;;

;; (load! "+sidebar/sidebar.el")
;; (load! "+sidebar/sidebar-ibuffer.el")
(require 'sidebar-core "+sidebar/sidebar-core.el")
(require 'sidebar-ibuffer "+sidebar/sidebar-ibuffer.el")

(provide '+sidebar)
(map! :leader :nvg "os" #'+ibuffer-sidebar-toggle)
