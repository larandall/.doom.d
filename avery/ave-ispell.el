;;;; flyspell in org mode
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda ()
            (flyspell-mode 1)
                   )))
;; (add-hook 'text-mode-hook (lambda ()(variable-pitch-mode t)))
(setq ispell-program-name (if (string-equal system-type "gnu/linux") 
                            "/usr/bin/hunspell" 
                            "/usr/local/bin/aspell"))
(setq ispell-dictionary "american")

(provide 'ave-ispell)
