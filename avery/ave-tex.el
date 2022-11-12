;;; $DOOMDIR/avery/ave-tex.el -*- lexical-binding: t; -*-

(use-package! latex
  :init
    (add-hook 'LaTeX-mode-hook
            (lambda ()
              (TeX-fold-mode 1)
              (add-hook 'find-file-hook 'TeX-fold-buffer t t)
              (outline-minor-mode 1)
              (outshine-mode)
              (visual-line-mode 1)
              (visual-fill-column-mode -1)
              (display-fill-column-indicator-mode 1)
              (auto-fill-mode 1)))
(add-hook 'evil-insert-state-exit-hook #'(lambda () (if (and(bound-and-true-p TeX-fold-auto)
                                                         (bound-and-true-p TeX-fold-mode))
                                                         (TeX-fold-buffer))))
  (setq TeX-fold-unfold-around-mark t)
  (setq reftex-toc-split-windows-horizontally t)
  (setq reftex-toc-split-windows-fraction 0.25)
  (setq reftex-toc-follow-mode t)
  (setq TeX-fold-auto t)
  (setq TeX-fold-macro-spec-list
        (quote
         (("[mp]"
           ("marginpar"))
          ("[f]"
           ("footnote"))
          ("[fm]"
           ("footnotemark"))
          ("[ft]"
           ("footnotetext"))
          ("[a]"
           ("autocite"))
          ("[A]"
           ("Autocite"))
          ("[C]"
           ("Cite"))
          ("[c]"
           ("cite"))
          ("[tc]"
 ("citet" "textcite" "textcites"))
          ("[pc]"
           ("citep" "parencite" "parencites"))
          ("[ct]"
           ("citetitle"))
          ("[a*]"
           ("autocite*"))
          ("[A*]"
           ("Autocite*"))
          ("[c*]"
           ("cite*"))
          ("[C*]"
           ("Cite*"))
          ("[ct*]"
           ("citetitle*"))
          ("“{1}”"
           ("enquote" "textquote" "blockquote"))
          ("‘{1}’"
           ("enquote*" "textquote*" "blockquote*"))
          ("[l]"
           ("label"))
          ("[r]"
           ("ref" "pageref" "eqref"))
          ("[i]"
           ("index" "glossary"))
          ("[1]:||*"
           ("item"))
          ("..."
           ("dots"))
          ("(C)"
           ("copyright"))
          ("(R)"
           ("textregistered"))
          ("TM"
           ("texttrademark"))
          (1
           ("part" "chapter" "section" "subsection" "subsubsection" "paragraph"
            "subparagraph" "part*" "chapter*" "section*" "subsection*"
            "subsubsection*" "paragraph*" "subparagraph*" "emph" "textit" "textsl"
            "textmd" "textrm" "textsf" "texttt" "textbf" "textsc" "textup")))))
  :config
;;;;; Word count
  (defun latex-word-count ()
    (interactive)
    (with-current-buffer (find-file-noselect (TeX-master-file t))
    (shell-command (concat "/usr/bin/texcount"
                                         " -inc -incbib -total -v0 "
                           (buffer-file-name))))))
(provide 'ave-tex)
