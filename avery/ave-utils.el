;;; $DOOMDIR/avery/ave-utils.el -*- lexical-binding: t; -*-
;; a set of utilities and applications that make my life easier.
;; * Paragraph filling

(after!
  org
  (defun get-indent ()
    (interactive)
    (message (number-to-string (current-indentation))))
  (defun get-oet ()
    (interactive)
    (if (equal
         (org-element-type (org-element-at-point))
         'item) (message "this is an item")
      (message "Not an item")))
  (defvar org-fill-by-sentences nil
    "If non-nill fill paragraphs by sentences in org mode")
  (defvar avery-wrap-sentences nil
    "If non-nill `averys-fill-paragraph-by-sentences'wraps filled sentences at
`fill-column'")
(defun unfill-paragraph ()
    (interactive)
      (let ((fill-column (point-max)))
        (cond
         ((eq major-mode 'org-mode)
          (org-fill-paragraph))
         ((eq major-mode 'LaTeX-mode)
          (LaTeX-fill-paragraph))
         (t (fill-paragraph)))))
  (defun averys-fill-paragraph-by-sentences (&optional justify)
  "This function fills a paragraph by sentences, principally in org-mode."
   (interactive)
  (let ((fill-column
    (if avery-wrap-sentences
        fill-column
      (point-max))))
    (averys-fill-paragraph-by-sentences-and-wrap)
    ))
  
(defun averys-fill-paragraph-by-sentences-and-wrap (&optional justify)
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
          (unless (looking-back "^[ \\t]*[0-9]*\\." (line-beginning-position))
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

;; this is just a copy of org-fill-paragraph modified for sentence wrapping.
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
      (set-buffer-modified-p nil))))
)
;; * org-pomodoro functions
;; Work tools for org mode
;; Tracking time and work

(after! org
        (use-package! org-clock
                      :after org
                      :commands (org-clock-in org-clock-out org-clocking-buffer)
                      )
  (defvar avery_writinglog nil)
  (defvar pomodoro-buffer nil)
(defun my-start-20 ()
    (interactive)
    (my-clock-in)
    (run-with-timer
     1200 nil 'my-clock-out))
(defun my-start-morningwrite ()
    (interactive)
    (my-clock-in)
    (run-with-timer
     1800 nil 'my-clock-out))
(defun my-start-quickwrite ()
    (interactive)
    (my-clock-in)
    (run-with-timer
     720 nil 'my-clock-out))
(defun my-start-short-pomodoro ()
    (interactive)
    (my-clock-in)
    (run-with-timer
     240 nil 'my-clock-out))
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
        (org-clock-out)
        (setq orig-wc nil))))
  (setq notify-method 'notify-via-message)
  (defun my-clock-in-hook ()
    (interactive)
    (setq pomodoro-buffer (org-clocking-buffer))
    (with-current-buffer (org-clocking-buffer)
      (setq orig-wc (count-words (point-min) (point-max)))
      (org-timer-start)))

  (defun avery-clock-in()
    (interactive)
    (if orig-wc
        (unless (equal (current-buffer) (org-clocking-buffer))
          (my-clock-out)))
    (if orig-wc
        (org-clock-in)
      (my-clock-in))
    )

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
(provide 'ave-utils)

;; * autocompile

(defun avery-autocompile-files ()
    "Return a list of note files containing 'project' tag." ;
    (seq-uniq
     (seq-map
      #'car
      (org-roam-db-query
       [:select [nodes:file]
        :from tags
        :left-join nodes
        :on (= tags:node-id nodes:id)
        :where (like tag (quote "%\"autocompile\"%"))]))))

(defun avery-autocompile ()
  (interactive)
  (if (equal major-mode 'org-mode)
      (let ((file-tags (vulpea-buffer-tags-get)))
        (if (member "autocompile" file-tags)
            (org-latex-export-to-latex nil nil nil t)))))
(use-package! vulpea)

(add-hook 'after-save-hook #'avery-autocompile)
