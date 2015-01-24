;; Turn off modes
(dolist (mode
	 '(menu-bar-mode                    ; No menu bar
	   tool-bar-mode                    ; No tool bar
	   scroll-bar-mode                  ; No scroll bar
	   blink-cursor-mode                ; Stop blinking cursor
	   global-linum-mode))              ; No line numbers
  (funcall mode 0))
	 
;; Turn on modes
(dolist (mode
	 '(column-number-mode               ; Turn on column numbers
	   show-paren-mode                  ; Show matching parenthesis
	   delete-selection-mode))          ; Replace selected text
  (funcall mode 1))

;; Set values
(setq inhibit-splash-screen t               ; No splash screen
      initial-scratch-message nil)          ; Clean scratch buffer.

(fset 'yes-or-no-p 'y-or-n-p)               ; Answer with y/n

;; ido-mode
(ido-mode t)
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
(defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
(add-hook 'ido-setup-hook 'ido-define-keys)

;; tidy
(defun tidy ()
  "Ident, untabify and unwhitespacify current buffer, or region if active."
  (interactive)
  (let ((beg (if (region-active-p) (region-beginning) (point-min)))
	(end (if (region-active-p) (region-end)       (point-max))))
    (whitespace-cleanup)
    (indent-region beg end nil)
    (untabify beg end)))

;; Use C-x C-f will create directory if not exists
(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir)))))

;; autosaves
(defvar emacs-autosave-directory "~/.emacs.d/autosaves/")
(setq backup-directory-alist
      `((".*" . ,emacs-autosave-directory))
      auto-save-file-name-transforms
      `((".*" ,emacs-autosave-directory t)))

;; font face
(when (member "Consolas" (font-family-list))
  (set-face-attribute 'default nil :font "Consolas-12"))
