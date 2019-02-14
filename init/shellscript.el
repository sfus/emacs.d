;;;; sh-mode
;; @eval (find-file-read-only (locate-library "sh-script.el"))

(setq sh-basic-offset 4);; default: 2
(setq sh-indentation 4) ;; default: 4
(eval-after-load "sh-script"
  '(define-key sh-mode-map "\C-m" 'newline-and-indent))

(add-hook 'sh-mode-hook
          (lambda ()
            (setq tab-width 4)
            ))

(add-to-list 'auto-mode-alist '("/\\.\\(bash\\|zsh\\)rc.*$" . sh-mode))
