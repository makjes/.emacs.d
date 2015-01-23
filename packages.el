;; packages
(require 'cl)
(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

(setq archives-refreshed nil)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("org" . "http://orgmode.org/elpa/")
	("MELPA" . "http://melpa.milkbox.net/packages/")))

(unless (file-exists-p (expand-file-name
			(concat package-user-dir
				"/archives/MELPA/")))
  (package-refresh-contents))

(defun packages-installed-p (packages)
  (loop for p in packages
	when (not (package-installed-p p)) do (return nil)
	finally (return t)))

(defun packages-install (packages)
  (unless (packages-installed-p packages)
    (if (not archives-refreshed)
      ;; check for new packages
      (progn (message "%s" "Emacs is now refreshing its package database...")
      (package-refresh-contents)
      (message "%s" "done.")
      (setq archives-refreshed t))
      (message "%s" "Package database already refreshed."))
    ;; install missing packages
    (dolist (p packages)
      (when (not (package-installed-p p))
        (package-install p)))))

;; setup-files
(load "~/.emacs.d/packages/autocomplete.el")
(load "~/.emacs.d/packages/prettylambda.el")
(load "~/.emacs.d/packages/scheme.el")
(load "~/.emacs.d/packages/multiplecursors.el")
(load "~/.emacs.d/packages/magit.el")
