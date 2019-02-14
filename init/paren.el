;; show paren
(show-paren-mode 1)
(custom-set-variables
 '(show-paren-delay 0)
 '(show-paren-style 'expression)
 '(parens-require-spaces nil))

;;;; Paredit
(dolist (hook '(emacs-lisp-mode-hook
                lisp-mode-hook
                ielm-mode-hook
                scheme-mode-hook
                inferior-scheme-mode-hook
                clojure-mode-hook
                cider-repl-mode-hook
                sly-mrepl-mode-hook))
  (add-hook hook 'enable-paredit-mode))

(with-eval-after-load 'paredit
  (define-key paredit-mode-map (kbd "C-c C-q") 'paredit-reindent-defun)
  (define-key paredit-mode-map (kbd "C-c C-j") #'eval-print-last-sexp)
  (define-key paredit-mode-map (kbd "M-q") nil)
  (define-key paredit-mode-map (kbd "M-)") #'move-past-close-and-reindent))
