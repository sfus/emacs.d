;; setting for javascript
(custom-set-variables
 '(js-auto-indent-flag nil))

(defun my/js-mode-hook ()
  (setq-local company-backends '(company-tern company-dabbrev)))
(add-hook 'js-mode-hook #'my/js-mode-hook)

;;;;+ Extra
;;; js2-mode
;; -> https://github.com/mooz/js2-mode
;; -> https://sites.google.com/site/shidoinfo/Home/programing-lang/%E9%96%A2%E6%95%B0%E5%9E%8B%E3%83%97%E3%83%AD%E3%82%B0%E3%83%A9%E3%83%9F%E3%83%B3%E3%82%B0%E8%A8%80%E8%AA%9E/ecmascript/javascript-kai-fa-huan-jing/emacs-javascript/js2-mode

(when (require 'js2-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode))

  ;; js2-jsx-mode
  ;; ;; Support for JSX is available via the derived mode `js2-jsx-mode'.  If you
  ;; ;; also want JSX support, use that mode instead:
  ;; (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
  ;; (add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))

  ) ;; js2-mode ends here
;;;;+
