;; setting for flycheck

(custom-set-variables
 '(flycheck-display-errors-delay 0.5)
 '(flycheck-idle-change-delay 1.0)
 ;;'(flycheck-display-errors-function nil)
 )

(with-eval-after-load 'flycheck
  (define-key flycheck-command-map (kbd "M-g M-n") 'flycheck-next-error)
  (define-key flycheck-command-map (kbd "M-g M-p") 'flycheck-previous-error)
  (define-key flycheck-command-map (kbd "M-g M-l") 'flycheck-list-errors))


;;;;+ Extra

;; Change prefix key
(setq flycheck-keymap-prefix (kbd "C-c 1")) ;; default: "C-c !"

(setq flycheck-checker-error-threshold 999) ;; default: 400

;; enable global-flycheck-mode
(add-hook 'after-init-hook #'global-flycheck-mode)

;; also set key to global-map
(global-set-key (kbd "M-g M-l") 'flycheck-list-errors)

;; flycheck-color-mode-line
;; -> https://github.com/flycheck/flycheck-color-mode-line
(when (require 'flycheck-color-mode-line nil t)
  (with-eval-after-load "flycheck"
    (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
    (custom-set-faces
     '(flycheck-color-mode-line-error-face ((t (:background "maroon"))))
     '(flycheck-color-mode-line-info-face ((t nil)))
     '(flycheck-color-mode-line-running-face ((t nil)))
     '(flycheck-color-mode-line-warning-face ((t nil)))
     )
    ))
;;  (eval-after-load 'flycheck
;;    (flycheck-add-next-checker 'javascript-jshint   ;; default
;;                               'javascript-gjslint) ;; append
;;    )

;; To find load-path, use user's load-path instead of `Emacs -Q'
;; https://emacs.stackexchange.com/questions/10244/flycheck-could-not-find-file-under-load-path
(setq flycheck-emacs-lisp-load-path 'inherit)
