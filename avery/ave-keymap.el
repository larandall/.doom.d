;;; $DOOMDIR/avery/ave-keymap.el -*- lexical-binding: t; -*-
(require 'evil-commands)
;; (require 'workspaces)
;; (require 'lookup)
(require 'general)
(general-auto-unbind-keys)

(map!
 :leader
 "w" nil)
(map!
:nv "H" nil
:nv "J" nil
:nv  "K" nil
:nv  "L" nil)

(map!
  :nv "H" #'evil-window-left
  :nv "J" #'evil-window-down
  :nv "K" #'evil-window-up
  :nv "L" #'evil-window-right
  :g "C-w C-w" #'evil-window-next)
(map!
 :leader
 "d" #'+workspace/close-window-or-workspace
 "D" #'kill-buffer-and-window
 "C" #'kill-current-buffer
 "w" #'save-buffer
 "e" #'treemacs
 "k" #'+lookup/documentation
 "j" #'evil-join
 )
(map!
 :nvi "C-M-RET" #'avery-insert-heading
 )
(map!
 :nvi "C-M-RET" #'avery-insert-heading
 :leader
 "ic" #'avery-insert-heading-and-clock
 "ih" #'avery-insert-heading-and-append
 "ii" #'avery-clock-in
 "im" #'my-start-morningwrite
 "ip" #'my-start-writing-pomodoro
 "iq" #'my-start-quickwrite
 "io" #'my-clock-out)
(provide 'ave-keymap)
