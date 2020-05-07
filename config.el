;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


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
(setq doom-font (font-spec :family "monospace"))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-opera)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


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
;; they are implemented.



;;;; Keybindings
  ;; get rid of those stupid hyper commands
  (if (string-equal system-type "darwin")
      (setq ns-command-modifier 'meta))
;;;;; TODO Get this shit working 
;; (spacemacs/set-leader-keys-for-major-mode 'org-mode
;;     "ef" 'org-publish-current-file
;;     "ep" 'org-publish-current-project
;;     "eg" 'send-buffer-professional
;;     "ea" 'send-buffer-personal
;;     "er" 'export-current-project
;;     "en" 'export-current-notes)
;;;;; get :q to work properly
  (evil-ex-define-cmd "q[uit]" 'spacemacs/kill-this-buffer)
;;;; Mode line
(setq doom-modeline-height 8)
(defun *extra ()
  (when global-mode-string
    (concat (format-mode-line global-mode-string) "   ")))
(defun doom-mode-line (&optional id)
  `(:eval
    (let* ((active (eq (selected-window) mode-line-selected-window))
           (lhs (list (propertize " " 'display (if active mode-line-bar mode-line-inactive-bar))
                      (*flycheck)
                      (*selection-info)
                      ;; (*anzu)
                      ;; (*iedit)
                      " "
                      (*buffer-path)
                      (*buffer-name)
                      " "
                      (*buffer-state)
                      ,(if (eq id 'scratch) '(*buffer-pwd))))
           (rhs (list 
                      (*buffer-encoding-abbrev)
                      (*vc)
                      "  " (*major-mode) "  "
                      (propertize
                       (concat "(%l,%c) " (*buffer-position))
                       'face (if active 'mode-line-2))
                      (*extra)))

           (middle (propertize
                    " " 'display `((space :align-to (- (+ right right-fringe right-margin)
                                                       ,(1+ (string-width (format-mode-line rhs)))))))))
      (list lhs middle rhs))))
(setq display-time-format nil)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time-mode 1)
;; (display-battery-mode -1)
;;;; email setup
(setq user-mail-address "l.avery.randall@gmail.com")
  (setq send-mail-function  'smtpmail-send-it)

  (setq user-full-name "L. Avery Randall")
  (defun set-email-personal ()
    (interactive)
    (setq send-mail-function    'smtpmail-send-it
          smtpmail-smtp-server  "smtp.gmail.com"
          smtpmail-stream-type  'starttls
          smtpmail-smtp-service 587
          smtpmail-smtp-user "l.avery.randall@gmail.com"
          user-mail-address "l.avery.randall@gmail.com"))
  (defun set-email-professional ()
    (interactive)
    (setq send-mail-function    'smtpmail-send-it
          smtpmail-smtp-server  "gator3189.hostgator.com"
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
    (org-mime-org-buffer-htmlize))
  ;; (setq mu4e-account-alist
  ;; )
  ;; (mu4e/mail-account-reset)
 ;;; Set up some common mu4e variables
  ;; (setq mu4e-maildir "~/.mail"
  ;;       mu4e-get-mail-command "offlineimap"
  ;;       mu4e-update-interval 600
  ;;       mu4e-compose-signature-auto-include t
  ;;       mu4e-view-show-images t
  ;;       mu4e-view-show-addresses t)
;;;;; Contexts
;;;; Auto saving
  (defun my-save-if-bufferfilename ()
    (if (buffer-file-name)
        (progn
          (save-buffer)
          )
      (message "no file is associated to this buffer: do nothing")
      ))
  (add-hook 'evil-insert-state-exit-hook 'my-save-if-bufferfilename)
  (add-hook 'evil-insert-state-exit-hook '(lambda () (if (and(bound-and-true-p TeX-fold-auto)
                                                         (bound-and-true-p TeX-fold-mode))
                                                         (TeX-fold-buffer))))
;;;; fix persistent server in macs
  (defun spacemacs/frame-killer ()
    "Kill server buffer and hide the main Emacs window"
    (interactive)
    (condition-case nil
        (delete-frame nil 1)
      (error
       (set-frame-parameter nil 'fullscreen nil)
       (make-frame-invisible nil 1))))
;;;; Fullscreen
  (if (string-equal system-type "gnu/linux")
      (add-to-list 'default-frame-alist '(fullscreen . maximized))
    (add-to-list 'default-frame-alist '(fullscreen . fullscreen)))
;;;; basic requires
  (require 'use-package)
  (require 'helm-bookmark)
  (require 'org)
  (require 'org-checklist)
  (require 'ox)
  (require 'ox-beamer)
  (require 'ox-latex)
  (require 'org-ref)
  ;; (unless (server-running-p) (server-start))
;;;; Outshine mode
  (use-package outshine)
  (add-hook 'outline-minor-mode-hook 'outshine-mode)
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
;;;; doom-todo-ivy setup
(require 'doom-todo-ivy)
  (setq doom/ivy-task-tags
        '(("TODO" . warning)
          ("NEXT" . warning)
          ("REVISE" . error)
          ("FIXME" . error)))
  ;; TODO key combinations
;;;; Latex configurations
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (TeX-fold-mode 1)
              (add-hook 'find-file-hook 'TeX-fold-buffer t t)
              (outline-minor-mode 1)
              (outshine-mode)
              (visual-line-mode 1)
              (visual-fill-column-mode 1)
              (auto-fill-mode -1)))
  
  (setq tex-fold-unfold-around-mark t)
  (setq reftex-toc-split-windows-horizontally t)
  (setq reftex-toc-split-windows-fraction 0.25)
  (setq reftex-toc-follow-mode t)
  (setq TeX-fold-auto t)
;;;;; Word count
  (defun latex-word-count ()
    (interactive)
    (with-current-buffer (find-file-noselect (TeX-master-file t))
    (shell-command (concat "/usr/bin/texcount"
                                         " -inc -incbib -total -v0 "
                           (buffer-file-name)))))
;;;; Fine Undo
  (setq evil-want-fine-undo t)
;;;; Doom theme
  (require 'doom-themes)
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
  ;; may have their own settings.
  ;; (load-theme 'doom-nord t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  ;; (doom-themes-neotree-config)
  (doom-themes-treemacs-config)
;;;; Auto fill in org mode
  (add-hook 'org-mode-hook
            (lambda ()
              ;; Turn off line numbering, it makes org so slow
              ;; (linum-mode -1)
              ;; Set fill column to 79
              (setq fill-column 80)
              ;; Enable automatic line wrapping at fill column
              (auto-fill-mode -1)
              (visual-fill-column-mode t)
              ;; (spacemacs/toggle-visual-line-numbers-on)
              ;; (spacemacs/toggle-visual-line-navigation-on)
              (smartparens-mode t)
              (show-smartparens-mode -1)))
  (setq org-tags-column -76)
  (defun unfill-paragraph ()
    (interactive)
    (let*((init-fc fill-column))
      (setq fill-column 1000000)
      (fill-paragraph)
      (setq fill-column init-fc)))
;;;; flyspell in org mode
  (dolist (hook '(text-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))
  (setq ispell-program-name (if (string-equal system-type "gnu/linux") "/usr/bin/hunspell" "/usr/local/bin/aspell"))
  (defvar pomodoro-buffer nil)
;;;; Some org setup
  (setq org-adapt-indentation nil)
  (setq org-startup-indented t)
;;;;; Org-Inlinetask
  (require 'org-inlinetask)
;;;;; Random footnote labels
  (setq org-footnote-auto-label 'random)
;;;;; make org meta return work properly
  (org-defkey org-mode-map [(meta return)] 'org-meta-return)

;;;;; Work tools for org mode
;;;;;; Tracking time and work
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
        (shell-command (format "growlnotify   \"%s\" \"Take a break! you have added %s words in %s\" " time added-w duration))
        (shell-command (format "echo %s, %s, %s >> %s" time2 duration added-w writinglog))
        )))
  (defun my-clock-out ()
    (interactive)
    (with-current-buffer (org-clocking-buffer)
      (let* ((final-wc (count-words (point-min)(point-max)))
             (added-w (- final-wc orig-wc))
             (time (format-time-string "%d %b %Y %R"))
             (time2 (format-time-string "%d %b %Y, %R"))
             (duration (replace-regexp-in-string " " "" (org-timer nil t))))
        (shell-command (format "growlnotify   \"%s\" \"Take a break! you have added %s words in %s\" " time added-w duration))
        (shell-command (format "echo %s, %s, %s >> %s" time2 duration added-w writinglog))
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
    (org-clock-in)
    (with-current-buffer (org-clocking-buffer)
      (setq orig-wc (count-words (point-min) (point-max)))
      (org-timer-start)))
  (add-hook 'org-pomodoro-finished-hook (lambda () (my-clock-out-hook)))
  (add-hook 'org-pomodoro-started-hook (lambda () (my-clock-in-hook)))
  (defvar writinglog nil)
  (setq writinglog "~/GDrive/P/Logs/Writinglog.csv")
;;;;;; Calendar Set up (commented out)

  ;; (require 'org-gcal)

  ;; (setq org-gcal-client-id "361276232754-eqa9218klpehsc6kf48a8q0loeeam1bk.apps.googleusercontent.com"
  ;;       org-gcal-client-secret "KkeTt-S1iBxrumxTVdxCLWTB"
  ;;       org-gcal-file-alist '(("l.avery.randall@gmail.com" .  "~/GDrive/Calendars/Avery.org")
  ;;                             ("q87rk33v9ctqnja35sp5bngd08@group.calendar.google.com"
  ;;                              .  "~/GDrive/Calendars/MelissaPCC.org")
  ;;                             ("melissa.wolfang@gmail.com" . "~/GDrive/Calendars/Melissa.org")
  ;;                             ("lttoh6g659iiutgc555h3lcveo@group.calendar.google.com" . "~/GDrive/Calendars/Coparent.org")
  ;;                             ("mariaameliad@gmail.com" . "~/GDrive/Calendars/Maria.org")
  ;;                             ("leonard.a.randall@gmail.com" . "~/GDrive/Calendars/bname.org")))
  ;; (defun internet-up-p (&optional host)
  ;;   (= 0 (call-process "ping" nil nil nil "-c" "1" "-W" "1" 
  ;;                      (if host host "www.google.com"))))
  ;;  (remove-hook 'after-init-hook (lambda ()  (if (internet-up-p)
  ;;                                             (org-gcal-sync))))
  ;; (remove-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync)))
  ;; ;; (add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync)))
  ;; (remove-hook 'org-capture-after-finalize-hook (lambda () (if (string-equal (file-name-directory (buffer-file-name (buffer-base-buffer))) (expand-file-name "~/GDrive/Calendars/"))
  ;;                                                           (org-gcal-post-at-point))))
;;;;; Org Capture templates
;;;;;;  Current file log entry
  (add-to-list org-capture-templates
        '("1" "Current file log entry" plain
           (file+datetree buffer-file-name)
           "\n\n%? " :clock-in :clock-keep)
          ;; ("e" "All Day event" entry (file  "~/GDrive/Calendars/Avery.org" )
          ;;  "* %?\n\n%^t\n\n:PROPERTIES:\n\n:END:\n\n")
          ;; ("f" "event" entry (file  "~/GDrive/Calendars/Avery.org" )
          ;;  "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")

          ;; ("g" "Event with end" entry (file  "~/GDrive/Calendars/Avery.org" )
          ;;  "* %?\n\n%^T-%^T\n\n:PROPERTIES:\n\n:END:\n\n")
          ;; ("c" "Coparenting Schedule" entry (file  "~/GDrive/Calendars/Coparent.org" )
          ;;  "* %?\n\n%^t\n\n:PROPERTIES:\n\n:END:\n\n")

          ;; ("m" "Event" entry (file  "~/GDrive/Calendars/Melissa.org" )
          ;;  "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
        ;;   ("w" "Weigh-in" entry
        ;;    (file+headline "~/GDrive/P/Fitness/diet.org" "Daily Logs")
        ;;    "* CAL-IN Diet for day %t
        ;; %^{Weight}p
        ;; | Food / Exercise | Calories | Quantity | Total |
        ;; |-----------------+----------+----------+-------|
        ;; | %?                |          |          |       |
        ;; |-----------------+----------+----------+-------|
        ;; | Total           |          |          |       |
        ;; #+TBLFM: $4=$2*$3;%.0f::$LR4=vsum(@2$4..@-I$4)

        ;; "
        ;;    :prepend t :empty-lines 1)
        ;;   ("W" "Workout and Diet info" plain
        ;;    (file+datetree+prompt
        ;;     "~/GDrive/P/Fitness/Fitness-Log.org")
        ;;    "%?THISISAWORKOUTENTRYDONOTMODIFY" :jump-to-captured t)
        ;;   ("d" "Daily Plan" plain (file+datetree+prompt "~/GDrive/P/GTD/Habits.org")
        ;;    "- %?" :jump-to-captured t)
        ;;   ("s" "Schedule" entry (file+olp "~/GDrive/P/GTD/GTD.org" "Work Schedule")
        ;;    "* %?\n" :jump-to-captured t)
        ;;   ("m" "Meetings" entry (file+olp "~/GDrive/P/GTD/GTD.org" "Meetings, Conferences, and Appointments")
        ;;    "* %?\n" :jump-to-captured t)
        ;;   ("i" "Inbox" entry (file+olp "~/GDrive/P/GTD/GTD.org" "GTD Inbox")
        ;;    "* NEXT %?\n  %i\n  %a" :jump-to-captured nil)

        ;;   ("p" "General Projects" entry (file+olp "~/GDrive/P/GTD/GTD.org" "Projects")
        ;;    "* NEXT %?\n  %i\n  %a" :jump-to-captured t)
        ;;   ("t" "Thesis Inbox" entry (file+olp "~/GDrive/P/GTD/Thesis.org" "Thesis Inbox")
        ;;    "* NEXT %?\n  %i\n  %a" :jump-to-captured nil)
        ;;   ("1" "Chapter 1 tasks" entry (file+olp "~/GDrive/P/GTD/GTD.org" "Chapter 1 Tasks")
        ;;    "* NEXT %?\n  %i\n  %a" :jump-to-captured nil)
        ;;   ("2" "Chapter 2 tasks" entry (file+olp "~/GDrive/P/GTD/GTD.org" "Chapter 2 Tasks")
        ;;    "* NEXT %?\n  %i\n  %a" :jump-to-captured nil)
        ;;   ("3" "Chapter 3 tasks" entry (file+olp "~/GDrive/P/GTD/GTD.org" "Chapter 3 Tasks")
        ;;    "* NEXT %?\n  %i\n  %a" :jump-to-captured nil)
        ;;   ("4" "Chapter 4 tasks" entry (file+olp "~/GDrive/P/GTD/GTD.org" "Chapter 4 Tasks")
        ;;    "* NEXT %?\n  %i\n  %a" :jump-to-captured nil)
        ;;   ("5" "Chapter 5 tasks" entry (file+olp "~/GDrive/P/GTD/GTD.org" "Chapter 5 Tasks")
        ;;    "* NEXT %?\n  %i\n  %a" :jump-to-captured nil)
        ;;   ("P" "Thesis Projects" entry (file+olp "~/GDrive/P/Thesis/Thesis.org" "Thesis Projects")
        ;;    "* NEXT %?\n  %i\n  %a" :jump-to-captured nil)
        ;;   ("l" "Writing Log" plain (file+datetree+prompt "~/GDrive/P/Thesis/writinglog.org")
        ;;    "1. %?" :jump-to-captured nil)
        ;;   ("r" "Work Record" entry (file+datetree "~/GDrive/Professional/Work Records/Work-Record.org")
        ;;    "* %?\nEntered on %U\n ")
        ;;   ("j" "Journal" entry (file+datetree "~/GDrive/Personal/Journals/Journal.org")
        ;;    "* %?\nEntered on %U\n ")
        ;;   ("n" "Note" plain (file+datetree "~/GDrive/P/Writing Projects/Freewriting/Freewriting.org")
        ;;    "Entered on %U\n  %i\n  %a")
        ;;   ("f" "Freewriting" entry (file+datetree "~/GDrive/P/Writing Projects/Freewriting/Freewriting.org")
        ;;    "* %?\n  %i\n" :jump-to-captured t)
        ;;   ("b" "Bibtex" "* READ %?\n\n%a\n\n%:author (%:year): %:title\n   \
        ;;  In %:journal, %:pages.")
        ;;   ("r" "Reading" entry
        ;;    (file+olp "~/GDrive/P/Bib/Readinglist.org" "RLIST Inbox")
        ;;    "** %^{Todo state|READ|FIND|PRINT|NOTES} [#%^{Priority|A|B|C}] New Reading Entry %? %^{BIB_TITLE}p %^{BIB_AUTHOR}p %^{BIB_EDITOR}p %^{BIB_YEAR}p %^{CUSTOM_ID}p %^g
        ;; :PROPERTIES:
        ;; :BIB_BTYPE: %^{Entry type|book|article|inbook|bookinbook|incollection|suppbook|phdthesis|proceedings|inproceedings|booklet}
        ;; :ENTERED_ON: %U %(my-org-bibtex-crossref)
        ;; :END:" :prepend t :jump-to-captured t)
          )

;;;;;;  Day Sheet
  (add-to-list 'org-capture-templates
               '("d" "Day Sheet" entry (file+datetree "~/GDrive/Professional/GMD/Day-Sheets.org")
                 "* Day Sheet %<%A %m/%d/%Y> :ignore:\n:PROPERTIES:\n:EXPORT_FILE_NAME: Sheets/%<%m-%d-%Y>\n:END:
Avery %<%A %m/%d/%Y> %^{First PO}%?\n\n%\\1: \n\nGMD on Site:\n\nNon-GMD on Site:\n\nNotes:" :jump-to-captured t))
  (add-to-list 'org-capture-templates
               '("l" "letter" entry (file+datetree "~/GDrive/Personal/Journals/Letters.org")
                 "* Letter to %^{Addressee} %<%A %m/%d/%Y> :ignore:\n:PROPERTIES:\n:EXPORT_FILE_NAME: Letters/%\\1-%<%Y-%m-%d>\n:END:\n%?" :jump-to-captured t))
;;;;;;  Journal
  (add-to-list 'org-capture-templates
               '("j" "Journal" plain (file+datetree "~/GDrive/Personal/Journals/Journal.org")
                 "%?\nEntered on %U\n " :jump-to-captured t))
;;;;;;  Avery todo
  (add-to-list 'org-capture-templates
               '("a" "Avery TODO" entry (file+olp "~/GDrive/Professional/GMD/Avery-Todo.org" "Tasks" "Current")
                 "* TODO %? \n%i\n %a"))

  (add-to-list 'org-capture-templates
               '("p" "Personal TODO" entry (file+olp "~/GDrive/Agendas/Personal.org" "Inbox")
                 "* TODO %? \n%i\n"))
;;;;;;  Old ones from my thesis
;; (add-to-list 'org-capture-templates
;;         '("w" "Weigh-in" entry
;;           (file+headline "~/GDrive/P/Fitness/diet.org" "Daily Logs")
;;            "* CAL-IN Diet for day %t
;;         %^{Weight}p
;;         | Food / Exercise | Calories | Quantity | Total |
;;         |-----------------+----------+----------+-------|
;;         | %?                |          |          |       |
;;         |-----------------+----------+----------+-------|
;;         | Total           |          |          |       |
;;         #+TBLFM: $4=$2*$3;%.0f::$LR4=vsum(@2$4..@-I$4)

;;         "
;;            :prepend t :empty-lines 1))
;; (add-to-list 'org-capture-templates
;;         '("W" "Workout and Diet info" plain
;;           (file+datetree+prompt
;;            "~/GDrive/P/Fitness/Fitness-Log.org")
;;           "%?THISISAWORKOUTENTRYDONOTMODIFY" :jump-to-captured t))
;; (add-to-list 'org-capture-templates
;;         '("d" "Daily Plan" plain (file+datetree+prompt "~/GDrive/P/GTD/Habits.org")
;;           "- %?" :jump-to-captured t))
;; (add-to-list 'org-capture-templates
;;              '("s" "Schedule" entry (file+olp "~/GDrive/P/GTD/GTD.org" "Work Schedule")
;;                "* %?\n" :jump-to-captured t))
;; (add-to-list 'org-capture-templates
;;              '("m" "Meetings" entry (file+olp "~/GDrive/P/GTD/GTD.org" "Meetings, Conferences, and Appointments")
;;                "* %?\n" :jump-to-captured t))
;; (add-to-list 'org-capture-templates
;;              '("i" "Inbox" entry (file+olp "~/GDrive/P/GTD/GTD.org" "GTD Inbox")
;;                "* NEXT %?\n  %i\n  %a" :jump-to-captured nil))
;; (add-to-list 'org-capture-templates
;;              '("p" "General Projects" entry (file+olp "~/GDrive/P/GTD/GTD.org" "Projects")
;;                "* NEXT %?\n  %i\n  %a" :jump-to-captured t))
;; (add-to-list 'org-capture-templates
;;         '("t" "Thesis Inbox" entry (file+olp "~/GDrive/P/GTD/Thesis.org" "Thesis Inbox")
;;           "* NEXT %?\n  %i\n  %a" :jump-to-captured nil))
;; (add-to-list 'org-capture-templates
;;         '("1" "Chapter 1 tasks" entry (file+olp "~/GDrive/P/GTD/GTD.org" "Chapter 1 Tasks")
;;          "* NEXT %?\n  %i\n  %a" :jump-to-captured nil))
;; (add-to-list 'org-capture-templates
;;         '("2" "Chapter 2 tasks" entry (file+olp "~/GDrive/P/GTD/GTD.org" "Chapter 2 Tasks")
;;           "* NEXT %?\n  %i\n  %a" :jump-to-captured nil))
;; (add-to-list 'org-capture-templates
;;         '("3" "Chapter 3 tasks" entry (file+olp "~/GDrive/P/GTD/GTD.org" "Chapter 3 Tasks")
;;          "* NEXT %?\n  %i\n  %a" :jump-to-captured nil))
;; (add-to-list 'org-capture-templates
;;         '("4" "Chapter 4 tasks" entry (file+olp "~/GDrive/P/GTD/GTD.org" "Chapter 4 Tasks")
;;          "* NEXT %?\n  %i\n  %a" :jump-to-captured nil))
;; (add-to-list 'org-capture-templates
;;         '("5" "Chapter 5 tasks" entry (file+olp "~/GDrive/P/GTD/GTD.org" "Chapter 5 Tasks")
;;          "* NEXT %?\n  %i\n  %a" :jump-to-captured nil))
;; (add-to-list 'org-capture-templates
;;         '("P" "Thesis Projects" entry (file+olp "~/GDrive/P/Thesis/Thesis.org" "Thesis Projects")
;;          "* NEXT %?\n  %i\n  %a" :jump-to-captured nil))
;; (add-to-list 'org-capture-templates
;;         '("l" "Writing Log" plain (file+datetree+prompt "~/GDrive/P/Thesis/writinglog.org")
;;          "1. %?" :jump-to-captured nil))
;; (add-to-list 'org-capture-templates
;;         '("r" "Work Record" entry (file+datetree "~/GDrive/Professional/Work Records/Work-Record.org")
;;           "* %?\nEntered on %U\n "))
;; (add-to-list 'org-capture-templates
;;         '("n" "Note" plain (file+datetree "~/GDrive/P/Writing Projects/Freewriting/Freewriting.org")
;;           "Entered on %U\n  %i\n  %a"))
;; (add-to-list 'org-capture-templates
;;         '("f" "Freewriting" entry (file+datetree "~/GDrive/P/Writing Projects/Freewriting/Freewriting.org")
;;           "* %?\n  %i\n" :jump-to-captured t))
;;;;;;  bibtex
  (add-to-list 'org-capture-templates
               '("b" "Bibtex" "* READ %?\n\n%a\n\n%:author (%:year): %:title\n   \
         In %:journal, %:pages."))
;;;;;;  reading 
  (add-to-list 'org-capture-templates
        '("r" "Reading" entry
          (file+olp "~/GDrive/P/Bib/Readinglist.org" "RLIST Inbox")
          "** %^{Todo state|READ|FIND|PRINT|NOTES} [#%^{Priority|A|B|C}] New Reading Entry %? %^{BIB_TITLE}p %^{BIB_AUTHOR}p %^{BIB_EDITOR}p %^{BIB_YEAR}p %^{CUSTOM_ID}p %^g
        :PROPERTIES:
        :BIB_BTYPE: %^{Entry type|book|article|inbook|bookinbook|incollection|suppbook|phdthesis|proceedings|inproceedings|booklet}
        :ENTERED_ON: %U %(my-org-bibtex-crossref)
        :END:" :prepend t :jump-to-captured t))
;;;;; Reftex and org reftex
  (setq reftex-default-bibliography
        '("/home/avery/GDrive/Resources/bib/mybib/My-Library.bib"))
;;;;; Export

(setq org-export-backends '(html icalender latex md odt pandoc))
(if (string-equal system-type "gnu/linux")
    (setq org-file-apps
          '(("\\.mm\\'" . default)
            ("\\.x?html?\\'" . "firefox %s")
            ("\\.pdf\\'" . default)
            ("\\.odt\\'" . "/usr/bin/libreoffice %s")
            (auto-mode . emacs)
            )))
;;;;;; async export
(setq org-export-async-init-file "/Users/avery/GDrive/Resources/dotfiles/lisp/org-setup.el")
;;;;;; auto export
(setq current-notes-file "~/GDrive/Writing Projects/Property Project/010_Property-Notes.org")
(setq current-project-file "~/GDrive/Writing Projects/Property Project/100_Property_Project.org")

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
       (#1# "hyperref" nil)))
;;;;;; Org publish
(require 'ox-publish)
(setq org-publish-project-alist
      '(("Property"
         :base-directory "~/GDrive/Writing Projects/Property Project/"
         :base-extension "org"
         :recursive t
         :publishing-directory "~/GDrive/Writing Projects/Property Project/Output/"
         :publishing-function org-latex-publish-to-pdf
         :exclude "\(Output/.*\)\|\(todo.org\)")
        ("Journals"
         :base-directory "~/GDrive/Personal/Journals/"
         :base-extension "org"
         :publishing-directory "~/GDrive/Personal/Journals/Pretty/"
         :publishing-function org-latex-publish-to-pdf)
        ("Chapters"
         :base-directory "~/GDrive/P/Thesis/Chapters/"
         :base-extension "org" 
         :publishing-directory "~/GDrive/P/Thesis/Output/" 
         :publishing-function org-latex-publish-to-pdf
         :exclude "^WorkingDraft.org")
        ("Thesis-Simple"
         :base-directory "~/GDrive/P/Thesis/Chapters/"
         :base-extension "org"
         :publishing-directory "~/GDrive/P/Thesis/Output/"
         :publishing-function org-latex-publish-to-pdf
         :exclude "[1-5].*")
        ("Chapter-Base"
         :base-directory "~/GDrive/P/Thesis/Chapters/"
         :base-extension "org"
         :publishing-directory "~/GDrive/P/Thesis/Chapters/"
         :publishing-function org-latex-publish-to-latex
         :exclude "^WorkingDraft.org")
        ("all"
         :components ("Chapters" "Thesis-Simple"))
        ("Compile-Draft"
         :components ("Chapter-Base" "Thesis-Simple"))))
;;;;;; Ignore headlines
(require 'ox-extra)
(ox-extras-activate '(ignore-headlines))
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
;;;;;;  Book with nice outline
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



;;;; Bibliography
(setq reftex-default-bibliography '("~/GDrive/Resources/bib/mybib/My-Library.bib"))

;; see org-ref for use of these variables
(require 'org-ref)
(setq org-ref-bibliography-notes "~/GDrive/zotfiles/notes.org"
      org-ref-default-bibliography '("~/GDrive/Resources/bib/mybib/My-Library.bib")
      org-ref-pdf-directory "~/GDrive/zotfiles/")
(setf (cdr (assoc 'org-mode bibtex-completion-format-citation-functions)) 'org-ref-format-citation)

;;;; file register
  (set-register ?j (cons 'file "~/GDrive/Personal/Journals/Journal.org"))
  (set-register ?c (cons 'file "~/GDrive/Writing Projects/Buddhistmoralphil.org"))
  (set-register ?b (cons 'file "~/GDrive/Personal/Journals/Buddhism.Spirituality.org"))
  (set-register ?r (cons 'file "~/GDrive/Writing Projects/Property Project/101_A_reasonable_pluralism.org"))
  (set-register ?p (cons 'file "~/GDrive/Essays/An_analysis_of_private_property/An_analysis_of_private_property.tex"))
  (set-register ?s (cons 'file "~/GDrive/Writing Projects/Poems.org"))
  (set-register ?o (cons 'file "~/GDrive/Writing Projects/Property Project/Notes/outline.org"))
  (set-register ?l (cons 'file "~/GDrive/P/Logs/Writinglog.csv"))
  (set-register ?e (cons 'file "~/GDrive/Resources/dotfiles/.spacemacs"))
  (set-register ?d (cons 'file "~/GDrive/Professional/GMD/Day-Sheets.org"))
  (set-register ?g (cons 'file "~/GDrive/Professional/GMD/Knack-Lists.org"))
  (set-register ?t (cons 'file "~/GDrive/Agendas/Personal.org"))
  (set-register ?w (cons 'file "~/GDrive/Writing Projects/Essay-Ideas.org"))
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
      (beginning-of-line)
    (save-excursion
      (insert "  #+BEGIN_QUOTE\n\n  #+END_QUOTE"))
    (next-line) 
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
  ;; (spacemacs/set-leader-keys-for-major-mode 'org-mode "iq" 'avery-insert-quote)
  ;; (spacemacs/set-leader-keys-for-major-mode 'org-mode "ir" 'avery-insert-short-quote)
  ;; (spacemacs/set-leader-keys-for-major-mode 'org-mode "iu" 'avery-insert-quote-simple)
  (define-key evil-normal-state-map "zq" 'unfill-paragraph)
;;;; agenda files
  ;; (setq org-agenda-files
  ;;        '("~/GDrive/Personal/Journals/Journal.org"
  ;;        "~/GDrive/Personal/Journals/Buddhism.Spirituality.org"
  ;;        "~/GDrive/Writing Projects/Property Project/100_Property_Project.org"
  ;;        "~/GDrive/Writing Projects/Property Project/TODOs.org"
  ;;        "~/GDrive/Professional/GMD/Day-Sheets.org"
  ;;        "~/GDrive/Professional/GMD/Knack-Lists.org"
  ;;        "~/GDrive/Agendas/Personal.org"
  ;;        "~/GDrive/Calendars/Avery.org"
  ;;        "~/GDrive/Calendars/Coparent.org"
  ;;        "~/GDrive/Calendars/Maria.org"
  ;;        "~/GDrive/Calendars/Melissa.org"
  ;;        "~/GDrive/Calendars/MelissaPCC.org"
  ;;        "~/GDrive/Calendars/bname.org"))
;;;; get cursor to work
;;;; org-toc

  (defun my-set-cursor (spec)
    (if (display-graphic-p)
        (set cursor-type spec)
      (unless (equal cursor-type spec)
        (let ((shape (or (car-safe spec) spec))
              (param))
          (setq param
                (cond ((eq shape 'bar) "6")
                      ((eq shape 'hbar) "4")
                      (t "2")))
          (send-string-to-terminal
           (concat "\e[" param " q"))))))
(if (require 'toc-org nil t)
    (add-hook 'org-mode-hook 'toc-org-mode)

  ;; enable in markdown, too
  (add-hook 'markdown-mode-hook 'toc-org-mode)
  (define-key markdown-mode-map (kbd "\C-c\C-o") 'toc-org-markdown-follow-thing-at-point)
(warn "toc-org not found"))
