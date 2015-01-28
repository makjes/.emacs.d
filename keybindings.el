(defvar custom-bindings-map (make-keymap)
  "A keymap for custom bindings.")

(define-key custom-bindings-map (kbd "<C-tab>")   'tidy)                            ; Tidy indent ref: general.el
(define-key custom-bindings-map (kbd "C-x k")     'kill-this-buffer)                ; Kills active buffer
(define-key custom-bindings-map (kbd "C-c .")     (cycle-themes))                   ; Cycle themes ref: theming.el
(define-key custom-bindings-map (kbd "C-<")       'mc/mark-next-like-this)          ; ref: packages/multiplecursors.el
(define-key custom-bindings-map (kbd "C-c C-<")   'mc/mark-all-like-this)           ; ref: packages/multiplecursors.el
(define-key custom-bindings-map (kbd "C->")       'mc/edit-lines)                   ; ref: packages/multiplecursors.el
(define-key custom-bindings-map (kbd "C-x C-g s") 'magit-status)                    ; ref: packages/magit.el
(define-key custom-bindings-map (kbd "C-x C-g l") 'gist-list)                       ; ref: packages/gist.el
(define-key custom-bindings-map (kbd "C-x C-g u") 'gist-region-or-buffer-private)   ; ref: packages/gist.el
(define-key custom-bindings-map (kbd "C-8")       'paredit-wrap-round)              ; Replace C-(. Sucks on NO-keyboards.

(define-minor-mode custom-bindings-mode
  "A mode that activates custom-bindings."
  t nil custom-bindings-map)



