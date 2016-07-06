;; setting for javascript
(custom-set-variables
 '(js-auto-indent-flag nil))

(defun my/js-mode-hook ()
  (setq-local company-backends '(company-tern company-dabbrev))
  (if (string-suffix-p ".json" (buffer-name))
      (setq flycheck-checker 'json-jsonlint)
    (setq flycheck-checker 'javascript-jshint)))
(add-hook 'js-mode-hook 'my/js-mode-hook)
