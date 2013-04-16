;;;; region setting

;; expand region
(global-set-key (kbd "C-@") 'er/expand-region)
(global-set-key (kbd "C-M-@") 'er/contract-region)

;; multimark
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-x r e") 'mc/edit-lines)

(eval-after-load "multiple-cursors-core"
  '(progn
     (dolist (command '(delete-cursor-word-or-region
                        paredit-forward-delete
                        paredit-forward-kill-word
                        my/backward-kill-word
                        my/delete-word
                        c-electric-delete-forward
                        cperl-electric-semi))
       (add-to-list 'mc/cmds-to-run-for-all command))))

;; wrap-region
(require 'wrap-region)
(wrap-region-global-mode t)

;; org-mode
(dolist (sign '("*" "_" "/" "+"))
  (wrap-region-add-wrapper sign sign nil 'org-mode))

;; markdown-mode
(dolist (sign '("`" "*"))
  (wrap-region-add-wrapper sign sign nil 'markdown-mode))

;; disable paredit enable mode
(dolist (mode (append '(emacs-lisp-mode scheme-mode lisp-mode clojure-mode)
                      my/autopair-enabled-modes))
  (add-to-list 'wrap-region-except-modes mode))
