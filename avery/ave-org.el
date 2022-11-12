;;; $DOOMDIR/avery/ave-org.el -*- lexical-binding: t; -*-
;;;;; General org setup

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

