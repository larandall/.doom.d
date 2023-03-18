;;; $DOOMDIR/avery/ave-org.el -*- lexical-binding: t; -*-
;;;;; General org setup

(setq org-directory "~/Dropbox/org/")
(setq org-roam-directory "~/Dropbox/org/roam/")
(setq deft-directory org-roam-directory)
(after! org
  ;; (add-to-list 'org-agenda-files "~/Dropbox/org/roam/")
  ;; (add-to-list 'org-agenda-files "~/Dropbox/org/roam/references")
  ;; (add-to-list 'org-agenda-files "~/Dropbox/org/roam/people")
  ;; (add-to-list 'org-agenda-files "~/Dropbox/Essays/Toward_a_principled_pluralism/notes/")
  ;; (add-to-list 'org-agenda-files "~/Dropbox/Essays/Toward_a_principled_pluralism/logs/")
  ;; (add-to-list 'org-agenda-files "~/Dropbox/Essays/Toward_a_principled_pluralism/")
  )
(after! org
(setq org-todo-keywords '((sequence
           "[_](t)"   ; A task that needs doing
           "[-](s)"   ; Task is in progress
           "|"
           "[X](d)")  ; Task was completed
        (sequence
           "[I](i)"  ; An unconfirmed and unapproved task or notion
           "[P](p)"  ; A project, which usually contains other tasks
           "[W](w)"  ; Something external is holding up this task
           "[H](h)"  ; This task is paused/on hold because of me
            "|"
           "[K](k)") ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "[R](r)"
           "[E](e)"
           "[A](a)"
           "|"
           "[D](D)")
          (sequence
           "|"
           "[O](o)"
           "[Y](y)"
           "[N](n)"))
        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("[E]"  . +org-todo-active)
          ("[A]"  . +org-todo-active)
          ("[H]"  . +org-todo-onhold)
          ("[W]" . +org-todo-onhold)
          ("[P]" . +org-todo-project)
          ("[N]"   . +org-todo-cancel)
          ("[K]" . +org-todo-cancel)))
  )

(after! org
  ;; (setq org-hide-leading-stars nil
  ;;       org-indent-mode-turns-on-hiding-stars nil)
  (setq org-superstar-headline-bullets-list '(
                                              ;; "₽"
                                              "№"
                                              "§"
                                              "¶"
                                              "◊"
                                              ;; "†"
                                              "‡"
                                              )))
(after! org
  (setq
;;;;;; indentation
   org-tags-column -80
   org-adapt-indentation nil
   org-startup-indented nil
;;;;;; insert headings wherever you are
   org-insert-heading-respect-content nil
;;;;;; footnotes
   org-footnote-auto-label 'random)
;;;;;; opening files
  (if (string-equal system-type "gnu/linux")
    (setq org-file-apps
          '(("\\.mm\\'" . default)
            ("\\.x?html?\\'" . default)
            ("\\.pdf\\'" . default)
            ("\\.odt\\'" . "/usr/bin/libreoffice %s")
            (auto-emacs . mode)
            ))))
(use-package! org-checklist
  :after org)
;;;;; Org word count
(use-package! org-wc
  ;; NOTE org-wc currently does not read links properly. You have to manually
  ;; set it to read all links as one word.
  :after org
  :init
  (setq org-wc-default-link-count 'oneword))

;;;;; DONE Org-Inlinetask
(use-package! org-inlinetask
  :after org
  :init
  (setq
   org-inlinetask-min-level 10
   org-export-with-inlinetasks nil))

(defun avery-insert-heading-and-clock ()
  (interactive)
  (org-insert-heading)
  (avery-clock-in)
  (evil-org-append-line 1))

(defun avery-insert-heading-and-append ()
  (interactive)
  (org-insert-heading)
  (if orig-wc
      (avery-clock-in))
  (evil-org-append-line 1))

(defun avery-insert-heading ()
  (interactive)
  (org-insert-heading)
  (if (org-clocking-p)
      (org-clock-in)))

;; ;;;;; DONE get smartparens to work
;; (remove-hook! 'org-load-hook
;;              #'+org-init-smartparens-h)
(provide 'ave-org)

