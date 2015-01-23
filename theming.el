(unless (package-installed-p 'monokai-theme)
  (package-install 'monokai-theme))
(unless (package-installed-p 'leuven-theme)
  (package-install 'leuven-theme))

(load-theme 'leuven t)

(defun cycle-themes ()
  "Returns a function that lets you cycle your themes."
  (lexical-let ((themes '#1=(leuven monokai . #1#)))
    (lambda ()
      (interactive)
      ;; Rotates the thme cycle and changes the current theme.
      (load-theme (car (setq themes (cdr themes))) t))))
