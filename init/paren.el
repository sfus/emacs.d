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

;;;;+ Extra
(defun my/paredit-add-to-previous-list ()
  (interactive)
  (back-to-indentation)
  (paredit-add-to-previous-list)
  (back-to-indentation))

(defun my/paredit-forward-barf-sexp ()
  (interactive)
  (back-to-indentation)
  (paredit-forward-barf-sexp)
  (back-to-indentation))
;;;;+


(with-eval-after-load 'paredit
  (define-key paredit-mode-map (kbd "C-c C-q") 'paredit-reindent-defun)
  (define-key paredit-mode-map (kbd "C-c C-j") #'eval-print-last-sexp)
  (define-key paredit-mode-map (kbd "M-q") nil)
  (define-key paredit-mode-map (kbd "M-)") #'move-past-close-and-reindent)

  ;;;;+ Extra
  (define-key paredit-mode-map (kbd "M-P") 'paredit-splice-sexp-killing-backward)
  (define-key paredit-mode-map (kbd "M-p") 'my/paredit-add-to-previous-list)
  (define-key paredit-mode-map (kbd "M-n") 'my/paredit-forward-barf-sexp)
  ;;;;+
  )

;;;; ParEdit
;; -> http://www.daregada.sakuraweb.com/paredit_tutorial_ja.html

;; C-), C-<right>   : paredit-forward-slurp-sexp
;; C-}, C-<left>    : paredit-forward-barf-sexp
;; M-(              : paredit-wrap-round
;; M-s              : paredit-splice-sexp
;; M-<up>           : paredit-splice-sexp-killing-backward
;; M-r              : paredit-raise-sexp

;;;;;
;;  C-d         paredit-forward-delete
;;  C-j         paredit-newline
;;  C-k         paredit-kill
;;  "           paredit-doublequote
;;  (           paredit-open-round
;;  )           paredit-close-round
;;  ;           paredit-semicolon
;;  [           paredit-open-square
;;  \           paredit-backslash
;;  ]           paredit-close-square
;;  DEL         editutil-paredit-backward-delete
;;  C-(         paredit-backward-slurp-sexp
;;  C-)         paredit-forward-slurp-sexp
;;  C-{         paredit-backward-barf-sexp
;;  C-}         paredit-forward-barf-sexp
;;  <C-M-left>  paredit-backward-slurp-sexp
;;  <C-M-right> paredit-backward-barf-sexp
;;  <C-left>    paredit-forward-barf-sexp
;;  <C-right>   paredit-forward-slurp-sexp
;;  <M-down>    paredit-splice-sexp-killing-forward
;;  <M-up>      paredit-splice-sexp-killing-backward
;;  <delete>    paredit-forward-delete
;;  <deletechar>    paredit-forward-delete
;;  C-c C-q     paredit-reindent-defun
;;  C-M-b       paredit-backward
;;  C-M-d       paredit-forward-down
;;  C-M-f       paredit-forward
;;  C-M-n       paredit-forward-up
;;  C-M-p       paredit-backward-down
;;  C-M-u       paredit-backward-up
;;  M-"         paredit-meta-doublequote
;;  M-(         paredit-wrap-round
;;  M-;         paredit-comment-dwim
;;  M-?         paredit-convolute-sexp
;;  M-J         paredit-join-sexps
;;  M-S         paredit-split-sexp
;;  M-d         paredit-forward-kill-word
;;  M-r         paredit-raise-sexp
;;  M-s         paredit-splice-sexp
;;  M-DEL       paredit-backward-kill-word
;;  ESC <C-left>    paredit-backward-slurp-sexp
;;  ESC <C-right>   paredit-backward-barf-sexp
;;  ESC <down>  paredit-splice-sexp-killing-forward
;;  ESC <up>    paredit-splice-sexp-killing-backward
;;  C-c C-M-l   paredit-recenter-on-sexp
