;;;; Coffee Script
;; (auto-install-from-url "https://raw.github.com/defunkt/coffee-mode/master/coffee-mode.el")
(autoload 'coffee-mode "coffee-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

(custom-set-variables
 '(coffee-tab-width 4))
