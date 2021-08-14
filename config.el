;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;;; Preliminaries
;; Place your private configuration here!Remember, you do not need to run 'doom
;; sync' after modifying this file!
(require 'server)
(unless server-process (server-start))
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
(setq doom-font (font-spec :family "JetBrains Mono"
                           :size (if  (and (string= system-type "gnu/linux")
                                           (string= system-name "avery-imac"))
                                     (if (and
                                          (> (display-pixel-width) 1921)
                                          (> (display-pixel-height) 1081))
                                         24
                                       16)
                                   14)))


;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'gruvbox-dark-medium)

;;; Centered Cursor
(global-centered-cursor-mode 1)
(xterm-mouse-mode 1)
  ;; (unless (display-graphic-p)
          (require 'evil-terminal-cursor-changer)
          (evil-terminal-cursor-changer-activate)
;; )
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")
(setq org-roam-directory "~/Dropbox/org/roam/")
(setq deft-directory org-roam-directory)
(after! org
  (add-to-list 'org-agenda-files "~/Dropbox/org/roam/")
  (add-to-list 'org-agenda-files "~/Dropbox/org/roam/references")
  (add-to-list 'org-agenda-files "~/Dropbox/org/roam/people")
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
  (require 'org-ref)
  (require 'helm-bibtex)
  (require 'orb-helm)
  (require 'bibtex-completion)
  (setq org-roam-bibtex-preformat-keywords
   '("citekey" "date" "entry-type" "title" "url" "file" "author" "editor" "pdf?" "file" "author-or-editor" "keywords" "year"
     "author-abbrev" "editor-abbrev" "author-or-editor-abbrev"))
  (setq orb-process-file-keyword t
        orb-file-field-extensions '("pdf")
        orb-insert-interface 'helm-bibtex
        orb-insert-generic-candidates-format 'key
        orb-insert-link-description 'citation)
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
         (file+head "references/%<%Y%m%d%H%M%S>-${citekey}.org" "#+title: ${citekey}: ${title}\n")
         :unnarrowed t)
        ))
  )
  (map!
   :desc "Insert org-roam link"
   "M-C-," #'orb-insert-link
   "M-C-a" #'org-roam-node-insert
   )
;;; tree sitter
(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
;;; vulpea setup
(use-package! vulpea
  :config
  (defun vulpea-project-p ()
  "Return non-nil if current buffer has any todo entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
  (seq-find                                 ; (3)
   (lambda (type)
     (eq type 'todo))
   (org-element-map                         ; (2)
       (org-element-parse-buffer 'headline) ; (1)
       'headline
     (lambda (h)
       (org-element-property :todo-type h)))))

(defun vulpea-project-update-tag ()
    "Update PROJECT tag in the current buffer."
    (when (and (not (active-minibuffer-window))
               (vulpea-buffer-p))
      (save-excursion
        (goto-char (point-min))
        (let* ((tags (vulpea-buffer-tags-get))
               (original-tags tags))
          (if (vulpea-project-p)
              (setq tags (cons "project" tags))
            (setq tags (remove "project" tags)))
          (unless (eq original-tags tags)
            (apply #'vulpea-buffer-tags-set (seq-uniq tags)))))))

(defun vulpea-buffer-p ()
  "Return non-nil if the currently visited buffer is a note."
  (and buffer-file-name
       (string-prefix-p
        (expand-file-name (file-name-as-directory org-roam-directory))
        (file-name-directory buffer-file-name))))

(defun vulpea-project-files ()
    "Return a list of note files containing 'project' tag." ;
    (seq-uniq
     (seq-map
      #'car
      (org-roam-db-query
       [:select [nodes:file]
        :from tags
        :left-join nodes
        :on (= tags:node-id nodes:id)
        :where (like tag (quote "%\"project\"%"))]))))

(defun vulpea-agenda-files-update (&rest _)
  "Update the value of `org-agenda-files'."
  (setq org-agenda-files (vulpea-project-files)))

(add-hook 'find-file-hook #'vulpea-project-update-tag)
(add-hook 'before-save-hook #'vulpea-project-update-tag)

(advice-add 'org-agenda :before #'vulpea-agenda-files-update)
)

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
;; (use-package! winum
;;   :commands (winum-mode)
;;   :config
;;   (winum-mode))
;; (map!
;;  :leader
;;  "RET" #'jump-to-register
;;  "1" #'winum-select-window-1
;;  "2" #'winum-select-window-2
;;  "3" #'winum-select-window-3
;;  "4" #'winum-select-window-4
;;  "5" #'winum-select-window-5
;;  "6" #'winum-select-window-6
;;  "7" #'winum-select-window-7
;;  "8" #'winum-select-window-8
;;  "9" #'winum-select-window-9
;;  "0" #'winum-select-window-0-or-10)
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
 "ef" #'org-publish-current-file
 "ep" #'org-publish-current-project
 "eg" #'send-buffer-professional
 "ea" #'send-buffer-personal
 "er" #'export-current-project
 "en" #'export-current-notes)
;;;;; searching
(map!
 :nvm
 "s" #'evil-avy-goto-char-2)
(map! :map evil-normal-state-map
      "zs" #'avery-fill-paragraph
      "zq" #'unfill-paragraph)
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
    (add-to-list 'default-frame-alist '(fullscreen . fullscreen)))
;;;; STRT basic requires
(use-package! org-checklist
  :after org)
(use-package! ox
    :after org)
(use-package! ox-beamer
  :after (ox ox-latex))
(use-package ox-latex
  :after ox)
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
(use-package! latex
  :init
    (add-hook 'LaTeX-mode-hook
            (lambda ()
              (TeX-fold-mode 1)
              (add-hook 'find-file-hook 'TeX-fold-buffer t t)
              (outline-minor-mode 1)
              (outshine-mode)
              (visual-line-mode 1)
              (visual-fill-column-mode 1)
              (auto-fill-mode -1)))
(add-hook 'evil-insert-state-exit-hook '(lambda () (if (and(bound-and-true-p TeX-fold-auto)
                                                         (bound-and-true-p TeX-fold-mode))
                                                         (TeX-fold-buffer))))
  (setq tex-fold-unfold-around-mark t)
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
;;;; Fine Undo
  (setq evil-want-fine-undo t)
;;;; Fill in org mode
(add-hook 'org-mode-hook
          (lambda ()
            (setq fill-column 80)
            ;; Enable automatic line wrapping at fill column
            (auto-fill-mode 1)
            (visual-fill-column-mode -1)
            (smartparens-mode 1)
            (show-smartparens-mode -1)))
(after!


  org (setq org-tags-column -80))
(defun unfill-paragraph ()
    (interactive)
      (let ((fill-column (point-max)))
        (cond
         ((eq major-mode 'org-mode)
          (org-fill-paragraph))
         ((eq major-mode 'LaTeX-mode)
          (LaTeX-fill-paragraph))
         (t (fill-paragraph)))))
(after!
  org
  (defvar org-fill-by-sentences nil
    "If non-nill fill paragraphs by sentences in org mode")
  (setq org-fill-by-sentences t)
  (defun averys-fill-paragraph-by-sentences (&optional justify)
  "This function fills a paragraph by sentences, principally in org-mode."
  (interactive)
  (save-excursion
  (unfill-paragraph)
  (let ((beg (max (point-min)
                  (org-element-property :begin (org-element-at-point))))
        (end (min (point-max)
                  (org-element-property :end (org-element-at-point))))
        (eos nil)
        (bos nil)
        (isitem nil)
        (next-s nil))
    (save-excursion
      (goto-char beg)
      (save-excursion
        (back-to-indentation)
        (if (> (current-column) 1)
            (setq indented t)
        (forward-char)
        (if (or (equal
             (org-element-type (org-element-at-point))
             'item) (> 0 (current-indentation))) (setq isitem t)))
        (while (< (point) end)
          (org-forward-sentence)
          (unless (looking-back "^[ \\t]*[0-9]*\\.")
          (setq eos (point))
          (setq bos (line-beginning-position))
          ;; (unless isitem
          ;; (fill-region-as-paragraph bos (point) justify))
          (save-excursion
            (forward-char)
            (org-forward-sentence)
            (setq next-s (point)))
          (unless (> next-s end)
            (if isitem
              (let* ((trailing-data
	                   (delete-and-extract-region (point) (line-end-position)))
                    (clean-data (replace-regexp-in-string "^[ \t]*" "" trailing-data)))
		            (save-excursion
                  (org--newline t nil t)
                  (insert clean-data))))
            (delete-horizontal-space)
            (org--newline t nil t))
          (fill-region-as-paragraph bos (point) justify)
          (setq end  (min (point-max)
                  (org-element-property :end (org-element-at-point))))
          (forward-char))
          (forward-char)))))))
  (defun get-indent ()
    (interactive)
    (message (number-to-string (current-indentation))))
  (defun get-oet ()
    (interactive)
    (if (equal
         (org-element-type (org-element-at-point))
         'item) (message "this is an item")
      (message "Not an item")))

 ;;   (defun org-fill-element ( justify)
;;   "Fill element at point, when applicable.

;; This function only applies to comment blocks, comments, example
;; blocks and paragraphs.  Also, as a special case, re-align table
;; when point is at one.

;; If JUSTIFY is non-nil (interactively, with prefix argument),
;; justify as well.  If `sentence-end-double-space' is non-nil, then
;; period followed by one space does not end a sentence, so don't
;; break a line there.  The variable `fill-column' controls the
;; width for filling.

;; For convenience, when point is at a plain list, an item or
;; a footnote definition, try to fill the first paragraph within."
;;   (with-syntax-table org-mode-transpose-word-syntax-table
;;     ;; Move to end of line in order to get the first paragraph within
;;     ;; a plain list or a footnote definition.
;;     (let ((element (save-excursion (end-of-line) (org-element-at-point))))
;;       ;; First check if point is in a blank line at the beginning of
;;       ;; the buffer.  In that case, ignore filling.
;;       (cl-case (org-element-type element)
;; 	;; Use major mode filling function is source blocks.
;; 	(src-block (org-babel-do-key-sequence-in-edit-buffer (kbd "M-q")))
;; 	;; Align Org tables, leave table.el tables as-is.
;; 	(table-row (org-table-align) t)
;; 	(table
;; 	 (when (eq (org-element-property :type element) 'org)
;; 	   (save-excursion
;; 	     (goto-char (org-element-property :post-affiliated element))
;; 	     (org-table-align)))
;; 	 t)
;; 	(paragraph
;; 	 ;; Paragraphs may contain `line-break' type objects.
;;    (if org-fill-by-sentences
;;        (averys-fill-paragraph-by-sentences)
;; 	   (let ((beg (max (point-min)
;; 			 (org-element-property :contents-begin element)))
;; 	       (end (min (point-max)
;; 			 (org-element-property :contents-end element))))
;; 	   ;; Do nothing if point is at an affiliated keyword.
;; 	   (if (< (line-end-position) beg) t
;; 	     ;; Fill paragraph, taking line breaks into account.
;; 	     (save-excursion
;; 	       (goto-char beg)
;; 	       (let ((cuts (list beg)))
;; 		 (while (re-search-forward "\\\\\\\\[ \t]*\n" end t)
;; 		   (when (eq 'line-break
;; 			     (org-element-type
;; 			      (save-excursion (backward-char)
;; 					      (org-element-context))))
;; 		     (push (point) cuts)))
;; 		 (dolist (c (delq end cuts))
;; 		   (fill-region-as-paragraph c end justify)
;; 		   (setq end c))))
;; 	     t))))
;; 	;; Contents of `comment-block' type elements should be
;; 	;; filled as plain text, but only if point is within block
;; 	;; markers.
;; 	(comment-block
;; 	 (let* ((case-fold-search t)
;; 		(beg (save-excursion
;; 		       (goto-char (org-element-property :begin element))
;; 		       (re-search-forward "^[ \t]*#\\+begin_comment" nil t)
;; 		       (forward-line)
;; 		       (point)))
;; 		(end (save-excursion
;; 		       (goto-char (org-element-property :end element))
;; 		       (re-search-backward "^[ \t]*#\\+end_comment" nil t)
;; 		       (line-beginning-position))))
;; 	   (if (or (< (point) beg) (> (point) end)) t
;; 	     (fill-region-as-paragraph
;; 	      (save-excursion (end-of-line)
;; 			      (re-search-backward "^[ \t]*$" beg 'move)
;; 			      (line-beginning-position))
;; 	      (save-excursion (beginning-of-line)
;; 			      (re-search-forward "^[ \t]*$" end 'move)
;; 			      (line-beginning-position))
;; 	      justify))))
;; 	;; Fill comments.
;; 	(comment
;; 	 (let ((begin (org-element-property :post-affiliated element))
;; 	       (end (org-element-property :end element)))
;; 	   (when (and (>= (point) begin) (<= (point) end))
;; 	     (let ((begin (save-excursion
;; 			    (end-of-line)
;; 			    (if (re-search-backward "^[ \t]*#[ \t]*$" begin t)
;; 				(progn (forward-line) (point))
;; 			      begin)))
;; 		   (end (save-excursion
;; 			  (end-of-line)
;; 			  (if (re-search-forward "^[ \t]*#[ \t]*$" end 'move)
;; 			      (1- (line-beginning-position))
;; 			    (skip-chars-backward " \r\t\n")
;; 			    (line-end-position)))))
;; 	       ;; Do not fill comments when at a blank line.
;; 	       (when (> end begin)
;; 		 (let ((fill-prefix
;; 			(save-excursion
;; 			  (beginning-of-line)
;; 			  (looking-at "[ \t]*#")
;; 			  (let ((comment-prefix (match-string 0)))
;; 			    (goto-char (match-end 0))
;; 			    (if (looking-at adaptive-fill-regexp)
;; 				(concat comment-prefix (match-string 0))
;; 			      (concat comment-prefix " "))))))
;; 		   (save-excursion
;; 		     (fill-region-as-paragraph begin end justify))))))
;; 	   t))
;; 	;; Ignore every other element.
;; 	(otherwise t)))))
(defun avery-fill-paragraph (&optional justify region)
  "Fill element at point, when applicable.

This function only applies to comment blocks, comments, example
blocks and paragraphs.  Also, as a special case, re-align table
when point is at one.

For convenience, when point is at a plain list, an item or
a footnote definition, try to fill the first paragraph within.

If JUSTIFY is non-nil (interactively, with prefix argument),
justify as well.  If `sentence-end-double-space' is non-nil, then
period followed by one space does not end a sentence, so don't
break a line there.  The variable `fill-column' controls the
width for filling.

The REGION argument is non-nil if called interactively; in that
case, if Transient Mark mode is enabled and the mark is active,
fill each of the elements in the active region, instead of just
filling the current element."
  (interactive (progn
		 (barf-if-buffer-read-only)
		 (list (when current-prefix-arg 'full) t)))
  (let ((hash (and (not (buffer-modified-p))
		   (org-buffer-hash))))
    (cond
     ((and region transient-mark-mode mark-active
	   (not (eq (region-beginning) (region-end))))
      (let ((origin (point-marker))
	    (start (region-beginning)))
	(unwind-protect
	    (progn
	      (goto-char (region-end))
	      (while (> (point) start)
		(org-backward-paragraph)
		(averys-fill-paragraph-by-sentences justify)))
	  (goto-char origin)
	  (set-marker origin nil))))
     (t (averys-fill-paragraph-by-sentences justify)))
    ;; If we didn't change anything in the buffer (and the buffer was
    ;; previously unmodified), then flip the modification status back
    ;; to "unchanged".
    (when (and hash (equal hash (org-buffer-hash)))
      (set-buffer-modified-p nil)))))
;;;; flyspell in org mode
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(setq ispell-program-name (if (string-equal system-type "gnu/linux") "/usr/bin/hunspell" "/usr/local/bin/aspell"))
(setq ispell-dictionary "american")
(defvar pomodoro-buffer nil)
;;;; DONE Some org setup
;;;;; General org setup
(after! org
  (setq
;;;;;; indentation
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
;;;;; Org word count
(use-package! org-wc
  ;; NOTE org-wc currently does not read links properly. You have to manually
  ;; set it to read all links as one word.
  :after org
  :init
  (setq org-wc-default-link-count 'oneword))
;;;;; DONE get smartparens to work
(remove-hook! 'org-load-hook
             #'+org-init-smartparens-h)
;;;;; DONE Org-Inlinetask
(use-package! org-inlinetask
  :after org
  :init
  (setq
   org-inlinetask-min-level 10
   org-export-with-inlinetasks nil))
;;;;; Work tools for org mode
;;;;;; Tracking time and work
(use-package! org-clock
  :after org
  :commands (org-clock-in org-clock-out org-clocking-buffer))
(after! org
  (defvar avery_writinglog nil)
  (setq avery_writinglog "~/Dropbox/Essays/Toward_a_principled_pluralism/logs/Writinglog.csv")
  (defvar pomodoro-buffer nil)
  (defun my-start-writing-pomodoro ()
    (interactive)
    (my-clock-in)
    (run-with-timer
     1500 nil 'my-clock-out))
  (defvar orig-wc 0)
  (defun my-clock-out-hook ()
    (interactive)
    (with-current-buffer pomodoro-buffer
      (let* ((final-wc (count-words (point-min)(point-max)))
             (added-w (- final-wc orig-wc))
             (time (format-time-string "%d %b %Y %R"))
             (time2 (format-time-string "%d %b %Y, %R"))
             (duration (replace-regexp-in-string " " "" (org-timer nil t))))
        (shell-command (format "notify-send   \"%s\" \"Take a break! you have added %s words in %s\" " time added-w duration))
        (shell-command (format "echo %s, %s, %s, %s >> %s" time2 duration added-w buffer-or-file avery_writinglog))
        )))
  (defun my-clock-out ()
    (interactive)
    (with-current-buffer (org-clocking-buffer)
      (let* ((final-wc (count-words (point-min)(point-max)))
             (added-w (- final-wc orig-wc))
             (buffer-or-file (if (buffer-file-name)
                                 (abbreviate-file-name (buffer-file-name))
                               (buffer-name)))
             (time (format-time-string "%d %b %Y %R"))
             (time2 (format-time-string "%d %b %Y, %R"))
             (duration (replace-regexp-in-string " " "" (org-timer nil t))))
        (shell-command (format "notify-send   \"%s\" \"Take a break! you have added %s words in %s\" " time added-w duration))
        (shell-command (format "echo %s, %s, %s, %s >> %s" time2 duration added-w buffer-or-file avery_writinglog))
        (org-clock-out))))
  (setq notify-method 'notify-via-message)
  (defun my-clock-in-hook ()
    (interactive)
    (setq pomodoro-buffer (org-clocking-buffer))
    (with-current-buffer (org-clocking-buffer)
      (setq orig-wc (count-words (point-min) (point-max)))
      (org-timer-start)))

  (defun my-clock-in()
    (interactive)
    (if (eq major-mode 'org-mode)
        (save-excursion
          (unless (org-at-heading-p)
            (org-previous-visible-heading 1))
          (org-clock-in)
          (with-current-buffer (org-clocking-buffer)
            (setq orig-wc (count-words (point-min) (point-max)))
            (org-timer-start)))
    (message "You need to be in org-for that")))
  (add-hook 'org-pomodoro-finished-hook (lambda () (my-clock-out-hook)))
  (add-hook 'org-pomodoro-started-hook (lambda () (my-clock-in-hook))))
;;;;;; Calendar Set up (commented out)

  ;; (require 'org-gcal)

  ;; (setq org-gcal-client-id "361276232754-eqa9218klpehsc6kf48a8q0loeeam1bk.apps.googleusercontent.com"
  ;;       org-gcal-client-secret "KkeTt-S1iBxrumxTVdxCLWTB"
  ;;       org-gcal-file-alist '(("l.avery.randall@gmail.com" .  "~/Dropbox/Calendars/Avery.org")
  ;;                             ("q87rk33v9ctqnja35sp5bngd08@group.calendar.google.com"
  ;;                              .  "~/Dropbox/Calendars/MelissaPCC.org")
  ;;                             ("melissa.wolfang@gmail.com" . "~/Dropbox/Calendars/Melissa.org")
  ;;                             ("lttoh6g659iiutgc555h3lcveo@group.calendar.google.com" . "~/Dropbox/Calendars/Coparent.org")
  ;;                             ("mariaameliad@gmail.com" . "~/Dropbox/Calendars/Maria.org")
  ;;                             ("leonard.a.randall@gmail.com" . "~/Dropbox/Calendars/bname.org")))
  ;; (defun internet-up-p (&optional host)
  ;;   (= 0 (call-process "ping" nil nil nil "-c" "1" "-W" "1"
  ;;                      (if host host "www.google.com"))))
  ;;  (remove-hook 'after-init-hook (lambda ()  (if (internet-up-p)
  ;;                                             (org-gcal-sync))))
  ;; (remove-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync)))
  ;; ;; (add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync)))
  ;; (remove-hook 'org-capture-after-finalize-hook (lambda () (if (string-equal (file-name-directory (buffer-file-name (buffer-base-buffer))) (expand-file-name "~/Dropbox/Calendars/"))
  ;;                                                           (org-gcal-post-at-point))))
;;;;; DONE Org Capture templates
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
Avery %<%A %m/%d/%Y> %^{First PO}%?\n\n%\\1: \n\nGMD on Site:\n\nNon-GMD on Site:\n\nNotes:" :jump-to-captured t))
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
                 "\\documentclass{scrreprt}
\\KOMAoptions{twoside,%
  titlepage,%
  numbers=noenddot,%
  headinclude,%
  footinclude,%
  cleardoublepage=empty,%
  BCOR=5mm,%
  abstract=true, % Turn off, if you decide to use book class.
  open=right,
  fontsize=11pt,%
}
\\usepackage{classic-config}
 [NO-DEFAULT-PACKAGES]
 [NO-PACKAGES]
 [NO-EXTRA]
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

(setq org-latex-listings t)
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
(use-package! reftex
  :after (:any org latex tex-mode)
  :config
(setq reftex-default-bibliography '("~/Dropbox/Resources/bib/mybib/My-Library.bib")))
(setq bibtex-completion-bibliography '("~/Dropbox/Resources/bib/mybib/My-Library.bib"))
;;;;; Org-reg
(use-package! org-ref
  :after org
  :config
  (setq
   ;; org-ref-bibliography-notes "~/Dropbox/zotfiles/notes.org"
      org-ref-default-bibliography '("~/Dropbox/Resources/bib/mybib/My-Library.bib")
      org-ref-pdf-directory "~/Dropbox/zotfiles/")
  (setf (cdr (assoc 'org-mode bibtex-completion-format-citation-functions)) 'org-ref-format-citation))

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
    (save-excursion (org-ref-helm-insert-cite-link))
    (org-fill-paragraph))
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
