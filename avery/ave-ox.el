;;; $DOOMDIR/avery/ave-ox.el -*- lexical-binding: t; -*-
;; My org export settings


;;;; STRT basic requires
  (use-package!  ox-extra
                 :after org
                 :config
  (ox-extras-activate '(ignore-headlines))
(use-package! ox
    :after org)
(use-package! ox-beamer
  :after (ox ox-latex))
(use-package ox-latex
  :after ox)

(provide 'ave-ox)
