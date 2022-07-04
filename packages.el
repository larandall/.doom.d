;git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here, run 'doom sync' on
;; the command line, then restart Emacs for the changes to take effect.
;; Alternatively, use M-x doom/reload.


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a particular repo, you'll need to specify
;; a `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, for whatever reason,
;; you can do so here with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))
(package! outshine)
(package! xclip)
(package! ox-pandoc)
(package! nord-theme)
(package! doom-themes)
(package! pdf-tools)
(package! vulpea
  :recipe (:host github :repo "d12frosted/vulpea"))
(package! org-roam-bibtex
  :recipe (:host github :repo "org-roam/org-roam-bibtex"))
;; (package! tree-sitter)
;; (package! tree-sitter-langs)
;; When using org-roam via the `+roam` flag
(unpin! org-roam)

;; When using bibtex-completion via the `biblio` module
(unpin! bibtex-completion helm-bibtex ivy-bibtex)
(package! gruvbox-theme)

(package! evil-terminal-cursor-changer)
(package! base16-theme)
(package! el-get)
(package! zotxt)
(package! centered-cursor-mode)
(package! bibtex-completion
  :recipe (:host github :repo "tmalsburg/helm-bibtex"
           :files ("bibtex-completion.el")))
 (package! org-ref
            :recipe (:host github :repo "jkitchin/org-ref"))
;;           :recipe (:host github :repo "larandall/org-ref"
;;                          :branch "fix-citation-export-for-pandoc-and-org"))
(package! toc-org)
(package! sr-speedbar)
(package! pretty-speedbar
   :recipe (:host github :repo "kcyarn/pretty-speedbar"))
;; (package! doom-todo-ivy :recipe (:host github :repo "jsmestad/doom-todo-ivy"))
(package! visual-fill-column)
(package! org-wc
  :recipe (:host github :repo "tesujimath/org-wc"))
;; (package! winum)
