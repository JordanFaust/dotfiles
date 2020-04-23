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
 (:after persp-mode
   :map evil-normal-state-map
   "C-t" #'pop-tag-mark)

 ;; Tab Cycling
 :n "g T"  #'centaur-tabs-backward
 :n "g t"  #'centaur-tabs-forward
 :n "C-<"  #'centaur-tabs-backward
 :n "C->"  #'centaur-tabs-forward

 ;; Leader Configs
 (:leader
   :desc "Switch to last buffer" :n "SPC" #'evil-switch-to-windows-last-buffer))
