;; coffeescript
(custom-set-variables
 '(coffee-tab-width 2)
 '(coffee-indent-like-python-mode t)
 '(coffee-args-compile '("-c" "-m")))

(defun my/coffee-edit-next-line ()
  (interactive)
  (goto-char (line-end-position))
  (coffee-newline-and-indent))

(with-eval-after-load 'coffee-mode
  (add-hook 'coffee-mode-hook 'my/coffee-mode-hook)
  (add-hook 'coffee-after-compile-hook 'sourcemap-goto-corresponding-point)

  (define-key coffee-mode-map [remap newline-and-indent] 'nil)
  (define-key coffee-mode-map (kbd "C-m") 'nil)
  (define-key coffee-mode-map (kbd "C-<return>") 'coffee-newline-and-indent)
  (define-key coffee-mode-map (kbd "M-o") 'my/coffee-edit-next-line)
  (define-key coffee-mode-map (kbd "C-j") 'coffee-newline-and-indent)

  (smartrep-define-key
      coffee-mode-map "C-c" '(("h" . 'coffee-indent-shift-left)
                              ("l" . 'coffee-indent-shift-right))))

;;;;+ Extra
;; Default Key Bindings
;;
;; Key              Command
;; C-m, Return      Insert newline and indent line
;; C-c C-<, backtab Indent line or region to left
;; C-c C->          Indent line or region to right
;; C-M-a            Move to beginning of defun
;; C-M-e            Move to end of defun
;; C-M-h            Mark this defun
;; A-r, C-c C-k     Compile buffer to JavaScript
;; A-R              Compile content of region to JavaScript
;; A-M-r, C-c C-z   Run CoffeeScript REPL
;; C-c C-l          Send this line to REPL buffer
;; C-c C-r          Send content of region to REPL buffer
;; C-c C-b          Send content of buffer to REPL buffer
;; C-c C-o C-s      Enable coffee-cos-mode
;;;;+
