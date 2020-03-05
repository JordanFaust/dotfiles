;;; private/jfaust/+bindings.el -*- lexical-binding: t -*-

(map!
 ;; Window Movements
 "C-h"    #'evil-window-left
 "C-j"    #'evil-window-down
 "C-k"    #'evil-window-up
 "C-l"    #'evil-window-right
 "A-q"    #'delete-window
 "C-`"      #'+popup/toggle
 "<C-tab>"  #'+popup/other

 ;; Code Navigation
 "C-t" #'pop-tag-mark

 ;; Tab Cycling
 :n "g T"  #'centaur-tabs-backward
 :n "g t"  #'centaur-tabs-forward

 ;; Leader Configs
 (:leader
   :desc "Switch to last buffer" :n "SPC" #'wc/switch-to-mru-buffer
   ))
