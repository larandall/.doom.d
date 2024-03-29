;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;;; Preliminaries
;; Place your private configuration here!Remember, you do not need to run 'doom
;; sync' after modifying this file!
(use-package server
  :defer 1
:config (unless server-process (server-start)))
(defvar conf-dir (expand-file-name "~/.dotfiles") 
  "Where I put my config files")
(setq conf-dir (expand-file-name "~/Dropbox/Resources/dotfiles/.doom.d/"))
(add-to-list 'load-path (concat conf-dir "avery/"))
(require 'ave-keymap)
(defun wrap-obsolete (orig-fn &rest args)
  (let ((args_ (if (= (length args) 2)
                   (append args (list "0"))
                 args)))
    (apply orig-fn args_)))

(advice-add 'define-obsolete-function-alias :around #'wrap-obsolete)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Avery Randall"
      user-mail-address "l.avery.randall@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family (if (string= system-type "gnu/linux")
                                       "JetBrains Mono"
                                     "Triplicate A Code")
                           :size
                                     (if (and
                                          (> (display-pixel-width) 1921)
                                          (> (display-pixel-height) 1081))
                                         (if (string= system-type "gnu/linux")
                                           ;; (string= system-name "avery-imac")
                                             24
                                         15)
                                       14)
                           ))
;; (setq doom-variable-pitch-font (font-spec :family (if (string= system-type "gnu/linux")
;;                                                       "Triplicate A"
;;                                                     "Triplicate A")
;;                            :size
;;                                      (if (and
;;                                           (> (display-pixel-width) 1921)
;;                                           (> (display-pixel-height) 1081))
;;                                          (if (string= system-type "gnu/linux")
;;                                            ;; (string= system-name "avery-imac")
;;                                              28
;;                                          18)
;;                                        17)
;;                            ))


;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme  ;; 'gruvbox-dark-medium
      'noctilux)

;;; Fill column indicator
(display-fill-column-indicator-mode 1)
;;; Centered Cursor
(global-centered-cursor-mode 1)
(setq-default ccm-vpos-init '(- (ccm-visible-text-lines)
                                (round (ccm-visible-text-lines)1.618)
                                ))
;; (setq-default ccm-vpos-init '(round (ccm-visible-text-lines)1.618))
(setq-default ccm-vpos-inverted 1)
(setq ccm-recenter-at-end-of-file t)
(setq scroll-margin 13)
(xterm-mouse-mode 1)
  ;; (unless (display-graphic-p)
          (require 'evil-terminal-cursor-changer)
          (evil-terminal-cursor-changer-activate)
;; )
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(use-package ave-org)
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

(use-package! org-roam-bibtex
  :after org-roam
  :config
  ;; (require 'org-ref)
  (require 'helm-bibtex)
  (require 'orb-helm)
  (require 'bibtex-completion)
  (setq org-roam-bibtex-preformat-keywords
   '("citekey" "date" "entry-type" "title" "url" "file" "author" "editor" "pdf?" "file" "author-or-editor" "keywords" "year"
     "author-abbrev" "editor-abbrev" "author-or-editor-abbrev"))
  (setq orb-process-file-keyword t
        orb-attached-file-field-extensions '("pdf")
        orb-insert-interface 'helm-bibtex
        orb-insert-generic-candidates-format 'key
        orb-insert-link-description 'citation-org-cite
        orb-roam-ref-format 'org-cite)
  (org-roam-bibtex-mode)
  (setq org-roam-capture-templates
        '(
          ("d" "default" plain
         (file "~/Dropbox/Resources/dotfiles/.doom.d/templates/default.org")
         :if-new
         (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)
          ("p" "person" plain
         (file "~/Dropbox/Resources/dotfiles/.doom.d/templates/people.org")
         :if-new
         (file+head "people/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)
        ("b" "bibliography reference" plain
         (file "~/Dropbox/Resources/dotfiles/.doom.d/templates/references.org")
         :if-new
         (file+head "references/%<%Y%m%d%H%M%S>-${citekey}.org" "#+title: ${title} by ${author} (${date})\n")
         :unnarrowed t)
        ))
  )
  (map!
   :desc "Insert org-roam link"
   "M-C-," #'orb-insert-link
   "M-C-a" #'org-roam-node-insert
   )
;;; tree sitter
;; (use-package! tree-sitter
;;   :config
;;   (require 'tree-sitter-langs)
;;   (global-tree-sitter-mode)
;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;absolute; they are implemented.
;;;; STRT Convert config to doom syntax
;;;; STRT Reorganize config to make navigating easier
;;;; Ace window config
(setq aw-keys '(?y ?h ?e ?a ?t ?n ?r ?r ?p))
;;;; Winum mode config
(use-package! winum
  :commands (winum-mode)
  :config
  (winum-mode))
(map!
 :leader
 "RET" #'jump-to-register
 "1" #'winum-select-window-1
 "2" #'winum-select-window-2
 "3" #'winum-select-window-3
 "4" #'winum-select-window-4
 "5" #'winum-select-window-5
 "6" #'winum-select-window-6
 "7" #'winum-select-window-7
 "8" #'winum-select-window-8
 "9" #'winum-select-window-9
 "0" #'treemacs-select-window)
;;;; hl-todo config
(after! hl-todo
  (setq hl-todo-keyword-faces
        '(;; for tasks and projects that have not yet been started
          ("TODO" warning bold)
          ;; for tasks and projects that have been started
          ("STRT" success bold)
          ("FIXME" error bold)
          ("HACK" font-lock-constant-face bold)
          ("REVIEW" font-lock-keyword-face bold)
          ("NOTE" success bold)
          ("DONE" font-lock-doc-face bold)
          ("DEPRECATED" font-lock-doc-face bold))))
;;;; DONE Keybindings
  (if (string-equal system-type "darwin")
      (setq ns-command-modifier 'meta))
(map! :after evil-org
      :map evil-org-mode-map
      :localleader
 "Ef" #'org-publish-current-file
 "Ep" #'org-publish-current-project
 "Eg" #'send-buffer-professional
 "Ea" #'send-buffer-personal
 "Er" #'export-current-project
 "En" #'export-current-notes)
;;;;; searching
(map!
 :nvm
 "s" #'evil-avy-goto-char-2)
(map! :after evil
      :map evil-normal-state-map
      "zs" #'avery-fill-paragraph
      "zq" #'unfill-paragraph
      "j" #'evil-next-visual-line
      "k" #'evil-previous-visual-line
      "<down>" #'evil-next-visual-line
      "<up>" #'evil-previous-visual-line)
(map! :after evil
      :map evil-visual-state-map
      "j" #'evil-next-visual-line
      "k" #'evil-previous-visual-line
      "<down>" #'evil-next-visual-line
      "<up>" #'evil-previous-visual-line)
(map! :after evil-org
      :map evil-org-mode-map
      :nv
      "zs" #'avery-fill-paragraph)
;;;;; get :q to work properly
(after! evil-ex
  (evil-ex-define-cmd "q[uit]" 'kill-current-buffer))
;;;; DONE Mode line
(setq display-time-format nil)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time-mode 1)
(use-package! doom-modeline
  :init
  (setq doom-modeline-enable-word-count nil))
;; (display-battery-mode -1)
;;;; DONE email setup commented out for now
(use-package! smtpmail
  :after (:any message sendmail)
  :commands smtpmail-send-it)
(use-package! org-mime
  :after (:any message sendmail)
  :commands org-mime-org-buffer-htmlize)
(after! (smtpmail org-mime)
  (setq send-mail-function  'smtpmail-send-it)

  (defun set-email-personal ()
    (interactive)
    (setq send-mail-function    'smtpmail-send-it
          smtpmail-(set-mark )tp-server  "smtp.gmail.com"
          smtpmail-stream-type  'starttls
          smtpmail-smtp-service 587
          smtpmail-smtp-user "l.avery.randall@gmail.com"
          user-mail-address "l.avery.randall@gmail.com"))
  (defun set-email-professional ()
    (interactive)
    (setq send-mail-function    'smtpmail-send-it
          smtpmail-(set-mark )tp-server  "gator3189.hostgator.com"
          smtpmail-stream-type  'ssl
          smtpmail-smtp-service 465
          smtpmail-smtp-user "avery@gmdcustom.com"
          user-mail-address "avery@gmdcustom.com"))
  (defun send-buffer-professional ()
    (interactive)
    (set-email-professional)
    (org-mime-org-buffer-htmlize))
  (defun send-buffer-personal ()
    (interactive)
    (set-email-personal)
    (org-mime-org-buffer-htmlize)))
;;;;; Set up some common mu4e variables (commented out for now)
  ;; (setq mu4e-maildir "~/.mail"
  ;;       mu4e-get-mail-command "offlineimap"
  ;;       mu4e-update-interval 600
  ;;       mu4e-compose-signature-auto-include t
  ;;       mu4e-view-show-images t
  ;;       mu4e-view-show-addresses t)
;;;; DONE Auto saving
  (defun my-save-if-bufferfilename ()
    (if (buffer-file-name)
        (progn
          (save-buffer)
          )
      (message "no file is associated to this buffer: do nothing")))
  (add-hook 'evil-insert-state-exit-hook 'my-save-if-bufferfilename)

;;;; DONE Fullscreen
  (if (string-equal system-type "gnu/linux")
      (add-to-list 'default-frame-alist '(fullscreen . maximized))
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
    ;; (add-to-list 'default-frame-alist '(fullscreen . fullscreen))
    )

;;;; DONE Outshine mode
(use-package! outshine
  :after (outline)
  :init
  (add-hook 'outline-minor-mode-hook 'outshine-mode))
(after! elisp-mode
   (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode))
;;;; DONE doom-todo-ivy setup
;; (use-package! doom-todo-ivy
;;   :config
;;   (setq doom/ivy-task-tags
;;         '(("FIXME" . error)
;;           ("TODO" . warning)
;;           ("STRT" . success)
;;           ("REVIEW" . font-lock-keyword-face)
;;           ("HACK" . font-lock-constant-face)
;;           ("NOTE" . success)
;;           ("DONE" . font-lock-doc-face)
;;           ("DEPRECATED" . font-lock-doc-face)))
;;   :commands
;;   doom/ivy-tasks)
;; key commands
;; (map!
;;  :after doom-todo-ivy
;;  :leader
;;  "pt" #'ivy-magit-todos)
;;;; DONE Latex configurations
(use-package! ave-tex)
;;;; Fine Undo
  (setq evil-want-fine-undo t)
;;;; Fill in org mode
;; (defun my-set-margins ()
;;   "Set margins in current buffer."
;;   (setq left-margin-width 6))

;; (add-hook 'text-mode-hook 'my-set-margins)
;; (use-package! mixed-pitch
;;   :hook (org-mode . mixed-pitch-mode)
;;   :config
;;   ;; (setq mixed-pitch-face 'variable-pitch)
;;   (setq mixed-pitch-set-height 1)
;;   )
(add-hook 'text-mode-hook
          (lambda ()
            (setq fill-column 80)
            ;; Enable automatic line wrapping at fill column
            (auto-fill-mode 1)
            (visual-fill-column-mode -1)
            (display-fill-column-indicator-mode 1)
            (smartparens-mode 1)            
            (show-smartparens-mode -1)))
(use-package! ave-ispell)
(use-package! ave-utils
              :init
  (setq org-fill-by-sentences t)
  (setq avery-wrap-sentences t)
  (setq avery_writinglog "~/Dropbox/logs/writinglog.csv")
            )

;; ;;;;; DONE get smartparens to work
;; (remove-hook! 'org-load-hook
;;              #'+org-init-smartparens-h)

;;;;; Capture frame

(after! org (setq +org-capture-frame-parameters
  `((name . "doom-capture")
    (width . 70)
    (height . 25)
    (transient . t)
    ,(if IS-LINUX '(display . ":1"))
    ,(if IS-MAC '(menu-bar-lines . 1)))))
;;;;;; Reading list
(after! org
  (defvar +org-capture-reading-notes-file
    "Reading_notes.org")
  (defun +org-capture-reading-notes-file ()
      (expand-file-name +org-capture-reading-notes-file org-directory))
  (defun +org-capture-project-reading-notes-file ()
    (+org--capture-local-root +org-capture-reading-notes-file))
  (add-to-list 'org-capture-templates
          '("pr" "Project-local reading notes" entry  ; {project-root}/Reading_notes.org
           (file+headline +org-capture-project-reading-notes-file "Inbox"))
  (add-to-list 'org-capture-templates
          '("r" "Reading notes" entry  ; {project-root}/Reading_notes.org
           (file+headline +org-capture-reading-notes-file "Inbox")))))
;;;;;;  Current file log entry
(after! org
  (add-to-list 'org-capture-templates
        '("1" "Current file log entry" plain
           (file+datetree buffer-file-name)
           "\n\n%? " :clock-in :clock-keep))
;;;;;; Day Sheet
  (add-to-list 'org-capture-templates
               '("d" "Day Sheet" entry (file+datetree "~/Dropbox/Professional/GMD/Day-Sheets.org")
                 "* Day Sheet %<%A %m/%d/%Y> :ignore:\n:PROPERTIES:\n:EXPORT_FILE_NAME: Sheets/%<%m-%d-%Y>\n:END:
Avery %<%A %m/%d/%Y> %^{First PO}%?\n\n%\\1: \n\nGMD on Site:\n\nNon-GMD on Site:\n\nNotes:" :jump-to-captured nil))
;;;;;; Letter
  (add-to-list 'org-capture-templates
               '("l" "letter" entry (file+datetree "~/Dropbox/org/Letters.org")
                 "* Letter to %^{Addressee} %<%A %m/%d/%Y> :ignore:\n:PROPERTIES:\n:EXPORT_FILE_NAME: Letters/%\\1-%<%Y-%m-%d>\n:END:\n%?" :jump-to-captured t))
;;;;;; Journal
  (add-to-list 'org-capture-templates
               '("j" "Journal" plain (file+datetree "~/Dropbox/Personal/Journals/Journal.org")
                 "%?\nEntered on %U\n " :jump-to-captured t))
;;;;;;  Avery todo
  (add-to-list 'org-capture-templates
               '("a" "Avery TODO" entry (file+olp "~/Dropbox/Professional/GMD/Avery-Todo.org" "Tasks" "Current")
                 "* TODO %? \n%i\n %a"))

  ;; (add-to-list 'org-capture-templates
  ;;              '(("p" "Personal TODO" entry (file+olp "~/Dropbox/Agendas/Personal.org" "Inbox")
  ;;                "* TODO %? \n%i\n")))
;;;;;;  bibtex
  (add-to-list 'org-capture-templates
               '("b" "Bibtex" "* READ %?\n\n%a\n\n%:author (%:year): %:title\n   \
         In %:journal, %:pages."))
;;;;;;  reading
  ;; (add-to-list 'org-capture-templates
  ;;       '("u" "Reading" entry
  ;;         (file+headline "~/Dropbox/P/Bib/Readinglist.org" "RLIST Inbox")
  ;;         "** %^{Todo state|READ|FIND|PRINT|NOTES} [#%^{Priority|A|B|C}] New Reading Entry %? %^{BIB_TITLE}p %^{BIB_AUTHOR}p %^{BIB_EDITOR}p %^{BIB_YEAR}p %^{CUSTOM_ID}p %^g
  ;;       :PROPERTIES:
  ;;       :BIB_BTYPE: %^{Entry type|book|article|inbook|bookinbook|incollection|suppbook|phdthesis|proceedings|inproceedings|booklet}
  ;;       :ENTERED_ON: %U %(my-org-bibtex-crossref)
  ;;       :END:" :prepend t :jump-to-captured t))
  )
;;;;; Export

;;;;;; async export
(setq org-export-async-init-file "/Users/avery/Dropbox/Resources/dotfiles/lisp/org-setup.el")
;;;;;; auto export
(setq current-notes-file "~/Dropbox/Writing Projects/Property Project/010_Property-Notes.org")
(setq current-project-file "~/Dropbox/Writing Projects/Property Project/100_Property_Project.org")

(setq avery-export-function 'org-latex-export-to-latex)
(defun avery-export-file-async (&optional file async)
  (interactive)
  (save-excursion
    (let* ((target
            (if file file
              (buffer-file-name)))
           (keep (if (get-file-buffer target)
                     t nil)))
      (set-buffer
       (find-file-noselect target))
      (funcall avery-export-function async)
      ;; clean up
      (unless keep (kill-buffer)))))
(defun export-current-notes ()
  (interactive)
 (avery-export-file-async current-notes-file t))
(defun export-current-project ()
    (interactive)
  (avery-export-file-async current-notes-file t))

;;;;;; default packages
(use-package! ox-latex
  :after (org ox)
  :commands (org-latex-export-as-latex
             org-latex-export-to-latex
             org-latex-export-to-latex-and-open
             org-latex-export-to-pdf
             org-latex-export-to-pdf-and-open)
  :init
  (setq org-latex-default-packages-alist
      '(("AUTO" "inputenc" t
        ("pdflatex"))
       ("T1,T5" "fontenc" t
        ("pdflatex"))
       ("" "csquotes" t)
       ("vietnamese,english" "babel" t)
       (#1="" "graphicx" t)
       (#1# "grffile" t)
       (#1# "longtable" nil)
       (#1# "wrapfig" nil)
       (#1# "rotating" nil)
       ("normalem" "ulem" t)
       (#1# "amsmath" t)
       (#1# "textcomp" t)
       (#1# "amssymb" t)
       (#1# "capt-of" nil)
       (#1# "hyperref" nil))))
;;;;;; Org publish
(use-package! ox-publish
  :after org
  :init
(setq org-publish-project-alist
      '(("Property"
         :base-directory "~/Dropbox/Writing Projects/Property Project/"
         :base-extension "org"
         :recursive t
         :publishing-directory "~/Dropbox/Writing Projects/Property Project/Output/"
         :publishing-function org-latex-publish-to-pdf
         :exclude "\(Output/.*\)\|\(todo.org\)")
        ("Journals"
         :base-directory "~/Dropbox/Personal/Journals/"
         :base-extension "org"
         :publishing-directory "~/Dropbox/Personal/Journals/Pretty/"
         :publishing-function org-latex-publish-to-pdf)
        ("Chapters"
         :base-directory "~/Dropbox/P/Thesis/Chapters/"
         :base-extension "org"
         :publishing-directory "~/Dropbox/P/Thesis/Output/"
         :publishing-function org-latex-publish-to-pdf
         :exclude "^WorkingDraft.org")
        ("Thesis-Simple"
         :base-directory "~/Dropbox/P/Thesis/Chapters/"
         :base-extension "org"
         :publishing-directory "~/Dropbox/P/Thesis/Output/"
         :publishing-function org-latex-publish-to-pdf
         :exclude "[1-5].*")
        ("Chapter-Base"
         :base-directory "~/Dropbox/P/Thesis/Chapters/"
         :base-extension "org"
         :publishing-directory "~/Dropbox/P/Thesis/Chapters/"
         :publishing-function org-latex-publish-to-latex
         :exclude "^WorkingDraft.org")
        ("all"
         :components ("Chapters" "Thesis-Simple"))
        ("Compile-Draft"
         :components ("Chapter-Base" "Thesis-Simple")))))
;;;;;; Ignore headlines
(use-package! ox-extra
  :after org
  :config
  (ox-extras-activate '(ignore-headlines)))
;;;;;; Org Export

(defvar latex-output-force nil
  "Force latexmk to rerun file.")
(setq org-latex-pdf-process '("latexmk -e \"$pdflatex=q/pdflatex -synctex=1 -interaction=nonstopmode/\" -bibtex -jobname=%o%b -pdf %f" "latexmk -c %f"))
(defun my-toggle-latex-output-force ()
  (interactive)
  (if latex-output-force
      (list (setq latex-output-force nil)
            (message "Latex output now set to standard recompile."))
    (setq latex-output-force t)
    (message "Latex output now set to force recompile."))
  (my-establish-pdf-engine))
(defun my-establish-pdf-engine ()
  (if latex-output-force
      (setq org-latex-pdf-process '("latexmk -gg -e \"$pdflatex=q/pdflatex -synctex=1 -interaction=nonstopmode/\" -bibtex -pdf %f"))
    (setq org-latex-pdf-process '("latexmk -e \"$pdflatex=q/pdflatex -synctex=1 -interaction=nonstopmode/\" -bibtex -pdf %f"))))
;;;;;; Org Latex Classes Setup
(after! ox-latex
;;;;;;  Book without parts (book-noparts)
(add-to-list 'org-latex-classes
               '("book-noparts"
                 "\\documentclass{book}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
;;;;;; classicthesis-report
(add-to-list 'org-latex-classes
               '("classicthesis-report"
                 "\\documentclass[]{scrreprt}
 [NO-DEFAULT-PACKAGES]
 [NO-PACKAGES]
"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
;;;;;; Book with nice outline
(add-to-list 'org-latex-classes
             '("book-nice-outline"
               "\\documentclass{book}
\\usepackage{unnumberedtotoc}

\\def\\theequation{\\arabic{equation}}                          % 1
\\def\\theIEEEsubequation{\\theequation\\alph{IEEEsubequation}}  % 1a (used only by IEEEtran's IEEEeqnarray)
\\def\\thesection{\\Roman{section}}                             % I
% V1.7, \\mbox prevents breaks around -
\\def\\thesubsection{\\mbox{\\thesection-\\Alph{subsection}}}     % I-A
% V1.7 use I-A1 format used by the IEEE rather than I-A.1
\\def\\thesubsubsection{\\thesubsection\\arabic{subsubsection}}  % I-A1
\\def\\theparagraph{\\thesubsubsection\\alph{paragraph}}         % I-A1a

% From Heiko Oberdiek. Because of the \\mbox in \\thesubsection, we need to
% tell hyperref to disable the \\mbox command when making PDF bookmarks.
% This done already with hyperref.sty version 6.74o and later, but
% it will not hurt to do it here again for users of older versions.
\\@ifundefined{pdfstringdefPreHook}{\\let\\pdfstringdefPreHook\\@empty}{}%
\\g@addto@macro\\pdfstringdefPreHook{\\let\\mbox\\relax}



% Main text forms (how shown in main text headings)
% V1.6, using \\thesection in \\thesectiondis allows changes
% in the former to automatically appear in the latter
  \\def\\thesectiondis{\\thesection.}                   % I.
  \\def\\thesubsectiondis{\\Alph{subsection}.}          % B.
  \\def\\thesubsubsectiondis{\\arabic{subsubsection})}  % 3)
  \\def\\theparagraphdis{\\alph{paragraph})}            % d)
"
               ("\\addchap{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection*{%s}" . "\\subsection*{%s}")
               ("\\subsubsection*{%s}" . "\\subsubsection*{%s}")))
;;;;;;  Book with unnumbered chapters (book-nonumbers)
 (add-to-list 'org-latex-classes
               '("book-nonumbers"
                 "\\documentclass{book}
\\usepackage{unnumberedtotoc}"
                 ("\\addchap{%s}" . "\\chapter*{%s}")
                 ("\\addsec{%s}" . "\\section*{%s}")
                 ("\\subsection*{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection*{%s}" . "\\subsubsection*{%s}")))
(add-to-list 'org-latex-classes
 '("memoir-book"
   "\\documentclass{memoir}"
   ("\\chapter{%s}" . "\\chapter*{%s}")
   ("\\section{%s}" . "\\section*{%s}")
   ("\\subsection*{%s}" . "\\subsection*{%s}")
   ("\\subsubsection*{%s}" . "\\subsubsection*{%s}")))
;;;;;;  Unnumbered book with parts
(add-to-list 'org-latex-classes
               '("book-withparts-nonumbers"
                 "\\documentclass{book}
\\usepackage{unnumberedtotoc}"
                 ("\\addpart{%s}" . "\\part*{%s}")
                 ("\\addchap{%s}" . "\\chapter*{%s}")
                 ("\\addsec{%s}" . "\\section*{%s}")
                 ("\\subsection*{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection*{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph*{%s}" . "\\paragraph*{%s}")))
;;;;;;  Single chapter with no numbers (chapter-nonumbers)
  (add-to-list 'org-latex-classes
               '("chapter-nonumbers"
                 "\\documentclass{book}
\\usepackage{unnumberedtotoc}"
                 ("\\addsec{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


;;;;;;  Beamer
(add-to-list 'org-latex-classes
             '("beamer"
               "\\documentclass\{beamer\}"
               ("\\section\{%s\}" . "\\section*\{%s\}")
               ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
               ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))
;;;;;;  Beamer Handout
(add-to-list 'org-latex-classes
             '("beamer-handout"
               "\\documentclass\[handout\]\{beamer\}"
               ("\\section\{%s\}" . "\\section*\{%s\}")
               ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
               ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))
;;;;;;  Uggedal-Thesis
(add-to-list 'org-latex-classes
             '("Uggedal-Thesis"
               "\\documentclass[openany, 11pt]{uiothesis}
\\usepackage{color}
\\usepackage{ifoddpage}
\\usepackage{snotez}
\\setsidenotes{text-format =
                \\checkoddpage
        \\ifoddpage
         \\RaggedRight\\footnotesize
        \\else
        \\RaggedLeft\\footnotesize
        \\fi, %footnote=true
}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]
"
               ("\\part{%s}" . "\\part*{%s}")
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph{%s}")))
;;;;;;  Uggedal-Thesis2
(add-to-list 'org-latex-classes
             '("Uggedal-Thesis2"
               "\\documentclass[openany, 11pt]{uiothesis}
\\usepackage{color}
\\usepackage{ifoddpage}
\\usepackage{snotez}
\\setsidenotes{text-format =
                \\checkoddpage
        \\ifoddpage
         \\RaggedRight\\footnotesize
        \\else
        \\RaggedLeft\\footnotesize
        \\fi, %footnote=true
}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]
 "
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph{%s}")))
;;;;;;  Uggedal-Thesis3

(add-to-list 'org-latex-classes
             '("Uggedal-Thesis3"
               "\\documentclass[openany, 11pt]{uiothesis}
\\usepackage{color}
\\usepackage{ifoddpage}
\\usepackage{snotez}
\\setsidenotes{text-format =
                \\checkoddpage
        \\ifoddpage
         \\RaggedRight\\footnotesize
        \\else
        \\RaggedLeft\\footnotesize
        \\fi, %footnote=true
}
 [PACKAGES]
 [NO-DEFAULT-PACKAGES]
 [EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph{%s}")))
                                        ; -Thesis
;;;;;;  Thesis
(add-to-list 'org-latex-classes
             '("Thesis2"
               "\\documentclass[12pt,twoside,openright]{memoir}
\\usepackage{layouts}[2001/04/29]
\\usepackage{glossaries}
\\glstoctrue
\\makeglossaries
\\makeindex
\\stockaiv
\\settrimmedsize{\\stockheight}{\\stockwidth}{*}
\\settrims{0pt}{0pt}
\\setlrmarginsandblock{3cm}{3cm}{*}
\\setulmarginsandblock{3cm}{3cm}{*}
\\setmarginnotes{17pt}{51pt}{\\onelineskip}
\\setheadfoot{\\onelineskip}{2\\onelineskip}
\\setheaderspaces{*}{2\\onelineskip}{*}
\\checkandfixthelayout
\\pagestyle{ruled}
\\headstyles{bringhurst}
\\chapterstyle{thatcher}
\\setlength{\\footmarkwidth}{1.8em}
\\setlength{\\footmarksep}{0em}
\\footmarkstyle{#1\\hfill}
\\addtolength{\\footnotesep}{.5em}
\\usepackage{ifxetex}
\\ifxetex
        \\usepackage{fontspec}
        \\usepackage{xunicode}
        \\defaultfontfeatures{Mapping=tex-text, Numbers={OldStyle}}
        \\setmainfont{Linux Libertine}
        \\setsansfont[Mapping=tex-text]{Linux Biolinum}
        \\setmonofont[Mapping=tex-text,Scale=MatchLowercase]{Linux Libertine Mono O}
        \\usepackage[xetex, colorlinks=true, urlcolor=FireBrick, plainpages=false, pdfpagelabels] {hyperref}
\\else
        \\usepackage[utf8]{inputenc}
        \\usepackage[osf]{mathpazo}
        \\usepackage{courier}
        \\usepackage[T1]{fontenc}
        \\usepackage[colorlinks=true, urlcolor=FireBrick, plainpages=false, pdfpagelabels]{hyperref}
\\fi
\\linespread{1.5}
\\usepackage[german,american]{babel}
\\usepackage{keyval}
\\usepackage{ifthen}
\\usepackage{etoolbox}
\\usepackage[babel=once,english=american]{csquotes}
\\usepackage[notes, strict, backend=bibtex, natbib, bibencoding=inputenc]{/Users/leotr/Documents/MMDMaster/Style_Files/biblatex-chicago}
\\setpnumwidth{2.55em}
\\setrmarg{3.55em}
\\cftsetindents{part}{0em}{3em}
\\cftsetindents{chapter}{0em}{3em}
\\cftsetindents{section}{3em}{3em}
\\cftsetindents{subsection}{4.5em}{3.9em}
\\cftsetindents{subsubsection}{8.4em}{4.8em}
\\cftsetindents{paragraph}{10.7em}{5.7em}
\\cftsetindents{subparagraph}{12.7em}{6.7em}
\\cftsetindents{figure}{0em}{3.0em}
\\cftsetindents{table}{0em}{3.0em}
\\usepackage{fancyvrb}
\\usepackage{graphicx}
\\usepackage{booktabs}
\\usepackage{tabulary}
\\usepackage{xcolor}
\\usepackage{hyperref}
\\usepackage{memhfixc}
\\usepackage{xcolor}
 [NO-DEFAULT-PACKAGES]
 [EXTRA]"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
;;;;;;  ThesisChapters
(add-to-list 'org-latex-classes
             '("Thesischapters"
               "\\usepackage{layouts}[2001/04/29]
\\usepackage{glossaries}
\\glstoctrue
\\makeglossaries
\\makeindex
\\stockaiv
\\settrimmedsize{\\stockheight}{\\stockwidth}{*}
\\settrims{0pt}{0pt}
\\setlrmarginsandblock{3cm}{3cm}{*}
\\setulmarginsandblock{3cm}{3cm}{*}
\\setmarginnotes{17pt}{51pt}{\\onelineskip}
\\setheaderspaces{*}{2\\onelineskip}{*}
\\checkandfixthelayout
\\pagestyle{ruled}
\\headstyles{bringhurst}
\\chapterstyle{thatcher}
\\setlength{\\footmarkwidth}{1.8em}
\\setlength{\\footmarksep}{0em}
\\footmarkstyle{#1\\hfill}
\\addtolength{\\footnotesep}{.5em}
\\usepackage{ifxetex}
\\ifxetex
        \\usepackage{fontspec}
        \\usepackage{xunicode}
        \\defaultfontfeatures{Mapping=tex-text, Numbers={OldStyle}}
        \\setmainfont{Sorts Mill Goudy}
        \\setsansfont[Mapping=tex-text]{Helvetica}
        \\setmonofont[Mapping=tex-text,Scale=MatchLowercase]{Menlo}
        \\usepackage[xetex, colorlinks=true, urlcolor=FireBrick, plainpages=false, pdfpagelabels] {hyperref}
\\else
        \\usepackage[utf8]{inputenc}
        \\usepackage[osf]{mathpazo}
        \\usepackage{courier}
        \\usepackage[T1]{fontenc}
        \\usepackage[colorlinks=true, urlcolor=FireBrick, plainpages=false, pdfpagelabels] {hyperref}
\\fi
\\linespread{1.5}
\\usepackage[german,american]{babel}
\\usepackage{keyval}
\\usepackage{ifthen}
\\usepackage{etoolbox}
\\usepackage[babel=once,english=american]{csquotes}
\\usepackage[notes, strict, backend=bibtex, natbib, bibencoding=inputenc]{/Users/leotr/Documents/MMDMaster/Style_Files/biblatex-chicago}
\\setpnumwidth{2.55em}
\\setrmarg{3.55em}
\\cftsetindents{part}{0em}{3em}
\\cftsetindents{chapter}{0em}{3em}
\\cftsetindents{section}{3em}{3em}
\\cftsetindents{subsection}{4.5em}{3.9em}
\\cftsetindents{subsubsection}{8.4em}{4.8em}
\\cftsetindents{paragraph}{10.7em}{5.7em}
\\cftsetindents{subparagraph}{12.7em}{6.7em}
\\cftsetindents{figure}{0em}{3.0em}
\\cftsetindents{table}{0em}{3.0em}
\\usepackage{fancyvrb}
\\usepackage{graphicx}
\\usepackage{booktabs}
\\usepackage{tabulary}
\\usepackage{xcolor}
\\usepackage{hyperref}
\\usepackage{memhfixc}
\\usepackage{xcolor}"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
;;;;;;  IEEE
(add-to-list 'org-latex-classes
             '("IEEE"
               "\\documentclass{IEEEtran}
"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
;;;;;;  Article
 (add-to-list 'org-latex-classes
             '("article"
               "\\documentclass{article}
\\usepackage[notes, backend=biber, natbib, bibencoding=inputenc, url=false, doi=false, isbn=false]{biblatex-chicago}
\\bibliography{Readinglist.bib, Paperslibrary.bib}
\\usepackage[T1]{fontenc}
\\usepackage{MinionPro}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
;;;;;;  Memoir Article
(add-to-list 'org-latex-classes
             '("Memoir-Article"
               "\\documentclass[12pt,twoside,article]{memoir}
 \\usepackage{layouts}[2001/04/29]
 \\counterwithout{section}{chapter}y
 \\usepackage{glossaries}
 \\glstoctrue
 \\makeglossaries
 \\makeindex
 \\stockaiv
\\settrimmedsize{\\stockheight}{\\stockwidth}{*}
\\settrims{0pt}{0pt}
\\setlrmarginsandblock{3cm}{3cm}{*}
\\setulmarginsandblock{3cm}{3cm}{*}
\\setmarginnotes{17pt}{51pt}{\\onelineskip}
\\setheadfoot{\\onelineskip}{2\\onelineskip}
\\setheaderspaces{*}{2\\onelineskip}{*}
\\checkandfixthelayout
\\pagestyle{ruled}
\\headstyles{bringhurst}
\\chapterstyle{thatcher}
\\setlength{\\footmarkwidth}{1.8em}
\\setlength{\\footmarksep}{0em}
\\footmarkstyle{#1\\hfill}
\\addtolength{\\footnotesep}{.5em}
\\usepackage{ifxetex}
\\ifxetex
        \\usepackage{fontspec}
        \\usepackage{xunicode}
        \\defaultfontfeatures{Mapping=tex-text, Numbers={OldStyle}}
        \\setmainfont{Sorts Mill Goudy}
        \\setsansfont[Mapping=tex-text]{Helvetica}
        \\setmonofont[Mapping=tex-text,Scale=MatchLowercase]{Menlo}
        \\usepackage[xetex, colorlinks=true, urlcolor=FireBrick, plainpages=false, pdfpagelabels] {hyperref}
\\else
        \\usepackage[utf8]{inputenc}
        \\usepackage[osf]{mathpazo}
        \\usepackage{courier}
        \\usepackage[T1]{fontenc}
        \\usepackage[colorlinks=true, urlcolor=FireBrick, plainpages=false, pdfpagelabels] {hyperref}
\\fi
\\linespread{1.5}
\\usepackage[german,american]{babel}
\\usepackage{keyval}
\\usepackage{ifthen}
\\usepackage{etoolbox}
\\usepackage[babel=once,english=american]{csquotes}
\\usepackage[notes, strict, backend=bibtex, natbib, bibencoding=inputenc]{/Users/leotr/Documents/MMDMaster/Style_Files/biblatex-chicago}
\\setpnumwidth{2.55em}
\\setrmarg{3.55em}
\\cftsetindents{part}{0em}{3em}
\\cftsetindents{chapter}{0em}{3em}
\\cftsetindents{section}{3em}{3em}
\\cftsetindents{subsection}{4.5em}{3.9em}
\\cftsetindents{subsubsection}{8.4em}{4.8em}
\\cftsetindents{paragraph}{10.7em}{5.7em}
\\cftsetindents{subparagraph}{12.7em}{6.7em}
\\cftsetindents{figure}{0em}{3.0em}
\\cftsetindents{table}{0em}{3.0em}
\\usepackage{fancyvrb}
\\usepackage{graphicx}
\\usepackage{booktabs}
\\usepackage{tabulary}
\\usepackage{xcolor}
\\usepackage{hyperref}
\\usepackage{memhfixc}
\\usepackage{xcolor}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

(setq org-latex-src-block-backend 'listings)
;;;;;;  Org Article
(add-to-list 'org-latex-classes
             '("org-article"
               "\\documentclass{org-article}
             [NO-DEFAULT-PACKAGES]
             [PACKAGES]
             [EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
;;;;;;  Philosophers Imprint
(add-to-list 'org-latex-classes
             '("philosophersimprint"
               "\\documentclass[noflushend]{philosophersimprint}
\\usepackage{opcit,kantlipsum}
\\usepackage{url}
\\usepackage[breaklinks,colorlinks,linkcolor=black,citecolor=black,
            pagecolor=black,urlcolor=black]{hyperref}
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]

[NO-EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ))

)

;;;; DONE Bibliography
;;;;; Reftex
(use-package! pretty-speedbar)
(setq sr-speedbar-right-side nil)
(use-package! reftex
  :after (:any org latex tex-mode)
  :config
(setq reftex-default-bibliography '("~/Dropbox/bib/My-Library.bib")))
(setq bibtex-completion-bibliography '("~/Dropbox/bib/My-Library.bib"))
;;;;; Org-reg

(use-package! org-ref
  :after org
  :config
  (setq
   ;; org-ref-bibliography-notes "~/Dropbox/zotfiles/notes.org"
      org-ref-default-bibliography '("~/Dropbox/bib/My-Library.bib")
      org-ref-pdf-directory "~/Dropbox/zotfiles/")
  ;; (setf (cdr (assoc 'org-mode bibtex-completion-format-citation-functions)) 'org-ref-format-citation)
  )
(defvar ave/bib
  nil "A list of local bibliographies")
(setq ave/bib '("~/Dropbox/bib/My-Library.bib"))
(use-package! oc
  :after org
  :config
  (setq
   ;; org-ref-bibliography-notes "~/Dropbox/zotfiles/notes.org"
       org-cite-global-bibliography ave/bib
       citar-bibliography ave/bib

)
  (setf (cdr
         (assoc 'org-mode bibtex-completion-format-citation-functions))
        'bibtex-completion-format-citation-org-cite)
)
(use-package! citar
  :after oc
  :config
  (setq
       org-cite-global-bibliography  ave/bib
       )
)


;;;; file register
  (set-register ?j (cons 'file "~/Dropbox/Personal/Journals/Journal.org"))
  (set-register ?c (cons 'file "~/Dropbox/Writing Projects/Buddhistmoralphil.org"))
  (set-register ?b (cons 'file "~/Dropbox/Personal/Journals/Buddhism.Spirituality.org"))
  (set-register ?r (cons 'file "~/Dropbox/Writing Projects/Property Project/101_A_reasonable_pluralism.org"))
  (set-register ?p (cons 'file "~/Dropbox/Essays/An_analysis_of_private_property/An_analysis_of_private_property.tex"))
  (set-register ?s (cons 'file "~/Dropbox/Writing Projects/Poems.org"))
  (set-register ?o (cons 'file "~/Dropbox/Writing Projects/Property Project/Notes/outline.org"))
  (set-register ?l (cons 'file "~/Dropbox/P/Logs/Writinglog.csv"))
  (set-register ?e (cons 'file "~/Dropbox/Resources/dotfiles/.spacemacs"))
  (set-register ?d (cons 'file "~/Dropbox/Professional/GMD/Day-Sheets.org"))
  (set-register ?g (cons 'file "~/Dropbox/Professional/GMD/Knack-Lists.org"))
  (set-register ?t (cons 'file "~/Dropbox/Agendas/Personal.org"))
(set-register ?n (cons 'file "~/Dropbox/org/notes.org"))
  (set-register ?w (cons 'file "~/Dropbox/Writing Projects/Essay-Ideas.org"))
;;;; Custom functions
;;;;; Org-ref <==> Pandoc
  (defun avery-org-ref-to-pandoc ()
    (interactive)
    (if (re-search-forward "\\[\\[MCS:\\(\\(autocites\\)\\|\\(parencites\\)\\)\\]\\[\\((\\|\\*\\)\\]\\]\\[\\[MC:\\(.*?\\)\\(\\]\\[\\)" nil 1)
        (let ((key (match-string-no-properties 5)) replacement)
          (if (match-string 2)
              (setq replacement (concat "[[ACTs:" key "][("))
            (setq replacement (concat "[[PSCs:" key "][(")))
          (let ((orig (match-string-no-properties 0)))
            (if (y-or-n-p (concat "Replace " orig "with " replacement "? "))
                (replace-match replacement))))))
;;;; Insert quote
  (defun current-line-empty-p ()
    (save-excursion
      (beginning-of-line)
      (looking-at "[[:space:]]*$")))
  (defun avery-clean-quote ()
    (interactive)
    (with-temp-buffer
      (evil-paste-after 1)
       (let* ((no-wordbreak
            (replace-regexp-in-string "­[ \n]*" "" (buffer-string)))
           (no-newlines
            (replace-regexp-in-string " *\n *" " " no-wordbreak))
           (no-hidden
               (replace-regexp-in-string "\ufeff\\|\u200b\\|\u200f\\|\u202e\\|\u200e\\|\ufffc" "" no-newlines))
           (no-thinspace
            (replace-regexp-in-string " " " " no-hidden))
           (no-initial-spaces
            (replace-regexp-in-string "\\` *" "" no-thinspace))
           (no-final-spaces
            (replace-regexp-in-string " *\\'" "" no-initial-spaces))
           (convert-en-dash
            (replace-regexp-in-string "–" "--" no-final-spaces))
           (convert-em-dash
            (replace-regexp-in-string "—" "---" convert-en-dash))
           (convert-single-quotes
            (replace-regexp-in-string "‘\\|’\\|’" "'" convert-em-dash))
           (convert-double-quotes
            (replace-regexp-in-string "“\\|”" "\"" convert-single-quotes)))
      (replace-regexp-in-string "" "" convert-double-quotes))))
  (defun avery-clean-quote2 ()
    (interactive)
    (with-temp-buffer
      (evil-paste-after 1)
       (let* ((no-wordbreak
            (replace-regexp-in-string "­[ \n]*" "" (buffer-string)))
           (no-hidden
               (replace-regexp-in-string "\ufeff\\|\u200b\\|\u200f\\|\u202e\\|\u200e\\|\ufffc" "" no-wordbreak))
           (no-thinspace
            (replace-regexp-in-string " " " " no-hidden))
           (no-initial-spaces
            (replace-regexp-in-string "\\` *" "" no-thinspace))
           (no-final-spaces
            (replace-regexp-in-string " *\\'" "" no-initial-spaces))
           (convert-en-dash
            (replace-regexp-in-string "–" "--" no-final-spaces))
           (convert-em-dash
            (replace-regexp-in-string "—" "---" convert-en-dash))
           (convert-single-quotes
            (replace-regexp-in-string "‘\\|’\\|’" "'" convert-em-dash))
           (convert-double-quotes
            (replace-regexp-in-string "“\\|”" "\"" convert-single-quotes)))
      (replace-regexp-in-string "" "" convert-double-quotes))))
  (defun avery-reverse-quotes (quotation)
    (let* ((trick
            (replace-regexp-in-string "\"" "&&&" quotation))
           (step
            (replace-regexp-in-string "'" "\"" trick)))
      (replace-regexp-in-string "&&&" "'" step)))
  (defun avery-insert-quote ()
    (interactive)
    (unless (current-line-empty-p)
      (unless (or (eolp) (save-excursion (looking-at ".? *$")))
        (unless (looking-at " ") (search-forward-regexp " \\|$" (+ 2 (point)) t))
        (save-excursion (newline-and-indent)))
      ;; (let ((evil-auto-indent nil))
      ;;   (evil-open-below 1))
      (unless (looking-at " ") (search-forward-regexp " \\|$" (+ 1 (point)) t))
      (newline))
      (forward-line 0)
    (save-excursion
      (insert "  #+BEGIN_QUOTE\n\n  #+END_QUOTE"))
    (forward-line)
    (beginning-of-line)
    (insert "  " (avery-clean-quote) " ")
    (save-excursion (org-cite-insert nil))
    (org-fill-paragraph))
(defun avery-clean-buffer ()
  (interactive)
  (kill-region (point-min) (point-max))
  (insert (avery-clean-quote2)))
(defun avery-clean-insert ()
  (interactive)
  (insert (avery-clean-quote2)))
  (defun avery-insert-short-quote ()
    (interactive)
    (unless (looking-at " \\|\\w") (forward-char))
    (insert " \"" (avery-reverse-quotes (avery-clean-quote)) "\" ")
    (save-excursion
      (insert " "))
    (org-ref-helm-insert-cite-link)
    (org-fill-paragraph))

  (defun avery-insert-quote-simple ()
    (interactive)
    (unless (looking-at " \\|\\w") (forward-char))
    (insert " \"" (avery-clean-quote) "\" ")
    (fill-paragraph))
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
(map!
 :map evil-org-mode-map

 (:leader
 "iq" nil
 (:prefix "iq"
 "q" #'avery-insert-quote
 "s" #'avery-insert-short-quote
 "r" #'avery-insert-quote-simple)))
  ;; (spacemacs/set-leader-keys-for-major-mode 'org-mode "iq" 'avery-insert-quote)
  ;; (spacemacs/set-leader-keys-for-major-mode 'org-mode "ir" 'avery-insert-short-quote)
  ;; (spacemacs/set-leader-keys-for-major-mode 'org-mode "iu" 'avery-insert-quote-simple)
;;;; agenda files
  ;; (setq org-agenda-files
  ;;        '("~/Dropbox/Personal/Journals/Journal.org"
  ;;        "~/Dropbox/Personal/Journals/Buddhism.Spirituality.org"
  ;;        "~/Dropbox/Writing Projects/Property Project/100_Property_Project.org"
  ;;        "~/Dropbox/Writing Projects/Property Project/TODOs.org"
  ;;        "~/Dropbox/Professional/GMD/Day-Sheets.org"
  ;;        "~/Dropbox/Professional/GMD/Knack-Lists.org"
  ;;        "~/Dropbox/Agendas/Personal.org"
  ;;        "~/Dropbox/Calendars/Avery.org"
  ;;        "~/Dropbox/Calendars/Coparent.org"
  ;;        "~/Dropbox/Calendars/Maria.org"
  ;;        "~/Dropbox/Calendars/Melissa.org"
  ;;        "~/Dropbox/Calendars/MelissaPCC.org"
  ;;        "~/Dropbox/Calendars/bname.org"))
;;;; org-toc
(use-package! toc-org
  :commands (toc-org-insert-toc toc-org-mode)
  :after (any: org markdown-mode)
  :config
  (add-hook 'org-mode-hook 'toc-org-mode)
  ;; enable in markdown, too
  (add-hook 'markdown-mode-hook 'toc-org-mode))
(after! markdown-mode
  (define-key markdown-mode-map (kbd "\C-c\C-o") 'toc-org-markdown-follow-thing-at-point)
(warn "toc-org not found"))
