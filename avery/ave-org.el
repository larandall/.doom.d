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
  (add-to-list 'org-todo-keywords '(sequence
                                    "NOTE(n)"
                                    "|"
                                    "NOTED(N)"))
  (add-to-list 'org-todo-keywords '(sequence
                                    "TOREAD(r)"
                                    "READNG(g)"
                                    "ANNOTT(G)"
                                    "|"
                                    "READ(R)"))
  (add-to-list 'org-todo-keyword-faces '("READNG" . +org-todo-active))
 (add-to-list 'org-todo-keyword-faces '("ANNOTT" . +org-todo-active))
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
   org-tags-column -78   
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
            ("\\.x?html?\\'" . "firefox %s")
            ("\\.pdf\\'" . default)
            ("\\.odt\\'" . "/usr/bin/libreoffice %s")
            (auto-mode . emacs)
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

;; ;;;;; DONE get smartparens to work
;; (remove-hook! 'org-load-hook
;;              #'+org-init-smartparens-h)
(provide 'ave-org)

