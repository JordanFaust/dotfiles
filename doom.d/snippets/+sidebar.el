;;; -*- lexical-binding: t -*-
;;; ~/.doom.d/snippets/+sidebar.el

;;;
;;; Requires
;;;

(require 'sidebar-core "+sidebar/sidebar-core.el")
(require 'sidebar-ibuffer "+sidebar/sidebar-ibuffer.el")

;; TODO move somewhere better
(map! :leader :nvg "os" #'+ibuffer-sidebar-toggle)

(provide '+sidebar)
