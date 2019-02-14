;;; sql
(add-hook 'sql-mode-hook 'sql-highlight-mysql-keywords)

(defun my/sql-mode-hook ()
  (setq sql-indent-offset 2)
  (setq indent-tabs-mode nil)
  (sql-set-product "mysql"))
(add-hook 'sql-mode-hook 'my/sql-mode-hook)

;; sql-upcase
(when (require 'sql-upcase nil t)
  (add-hook 'sql-mode-hook 'sql-upcase-mode)
  (add-hook 'sql-interactive-mode-hook 'sql-upcase-mode)
  (custom-set-variables
   '(sql-upcase-boundary "[\t\n\r ();,]") ;; default: "[\t\n\r ();]"
   ))

;; sql-indent
(when (require 'sql-indent nil t)
  (add-hook 'sql-mode-hook 'sqlind-minor-mode))
