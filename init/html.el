;; web-mode
(with-eval-after-load 'web-mode
  (add-hook 'web-mode-hook 'my/web-mode-hook)
  ;; remap key
  (define-key web-mode-map (kbd "C-c b b") 'web-mode-block-beginning)
  (define-key web-mode-map (kbd "C-c b e") 'web-mode-block-end)
  (define-key web-mode-map (kbd "C-c b k") 'web-mode-block-kill)
  (define-key web-mode-map (kbd "C-c b n") 'web-mode-block-next)
  (define-key web-mode-map (kbd "C-c b p") 'web-mode-block-previous)
  (define-key web-mode-map (kbd "C-c b s") 'web-mode-block-select)

  (define-key web-mode-map (kbd "C-c e b") 'web-mode-element-beginning)
  (define-key web-mode-map (kbd "C-c e c") 'web-mode-element-clone)
  (define-key web-mode-map (kbd "C-c e d") 'web-mode-element-child)
  (define-key web-mode-map (kbd "C-c e e") 'web-mode-element-end)
  (define-key web-mode-map (kbd "C-c e i") 'web-mode-element-content-select)
  (define-key web-mode-map (kbd "C-c e k") 'web-mode-element-kill)
  (define-key web-mode-map (kbd "C-c e n") 'web-mode-element-next)
  (define-key web-mode-map (kbd "C-c e p") 'web-mode-element-previous)
  (define-key web-mode-map (kbd "C-c e r") 'web-mode-element-rename)
  (define-key web-mode-map (kbd "C-c e s") 'web-mode-element-select)
  (define-key web-mode-map (kbd "C-c e t") 'web-mode-element-traverse)
  (define-key web-mode-map (kbd "C-c e u") 'web-mode-element-parent)

  (define-key web-mode-map (kbd "C-c t b") 'web-mode-tag-beginning)
  (define-key web-mode-map (kbd "C-c t e") 'web-mode-tag-end)
  (define-key web-mode-map (kbd "C-c t m") 'web-mode-tag-match)
  (define-key web-mode-map (kbd "C-c t n") 'web-mode-tag-next)
  (define-key web-mode-map (kbd "C-c t p") 'web-mode-tag-previous)
  (define-key web-mode-map (kbd "C-c t s") 'web-mode-tag-select))

;;(add-to-list 'auto-mode-alist '("\\.html?``'" . web-mode))

(custom-set-variables
 '(web-mode-css-indent-offset 4))

(defun my/web-mode-hook ()
  (local-unset-key (kbd "C-c C-b"))
  (local-unset-key (kbd "C-c C-e"))
  (local-unset-key (kbd "C-c C-t")))

;; html-mode
(defun html-mode-insert-br ()
  (interactive)
  (insert "<br />"))

(with-eval-after-load 'sgml-mode
  (define-key html-mode-map (kbd "C-c b") 'html-mode-insert-br))

;; emmet-coding
(dolist (hook '(sgml-mode-hook html-mode-hook web-mode-hook))
  (add-hook 'hook 'emmet-mode))

;; Preview is disable as default
(custom-set-variables
 '(emmet-preview-default nil)
 '(emmet-indentation 2))


;;;;+ Extra
(setq web-mode-markup-indent-offset 2)
;; (setq web-mode-code-indent-offset 2)
;; (setq web-mode-html-offset   2)
;; (setq web-mode-css-offset    2)
;; (setq web-mode-script-offset 4)
;; (setq web-mode-php-offset    4)
;; (setq web-mode-java-offset   4)
;; (setq web-mode-asp-offset    4)
;; (setq indent-tabs-mode nil)
;; (setq tab-width 2)

(setq web-mode-enable-auto-quoting nil)

;; whitespace
(setq web-mode-enable-whitespace-fontification t)
(setq web-mode-display-table
      (let ((table (make-display-table)))
        ;;(aset table 9  (vector ?\xB7 ?\t)) ;tab
        (aset table 9  (vector ?\xBB ?\t)) ;tab
        ;; (aset table 10 (vector ?\;XXX: B6 ?\n)) ;line feed
        ;; (aset table 32 (vector ?\xB7))
        table))

;; faces
(custom-set-faces
 '(web-mode-doctype-face
   ((t (:foreground "#82AE46"))))                         ; doctype
 '(web-mode-html-tag-face
   ((t (:foreground "#E6B422" :weight bold))))            ; tag
 '(web-mode-html-attr-name-face
   ((t (:foreground "#C97586"))))                         ; attr-name
 '(web-mode-html-attr-value-face
   ((t (:foreground "#82AE46"))))                         ; attr-value
 '(web-mode-comment-face
   ((t (:foreground "#999999"))))                         ; comment
 '(web-mode-server-comment-face
   ((t (:foreground "#999999"))))                         ; comment
 '(web-mode-css-rule-face
   ((t (:foreground "#A0D8EF"))))                         ; css
 '(web-mode-css-pseudo-class-face
   ((t (:foreground "#FF7F00"))))                         ; css pseudo class
 '(web-mode-css-at-rule-face
   ((t (:foreground "#FF7F00"))))                         ; css
 '(web-mode-whitespace-face
   ((t (:foreground "SteelBlue" :underline t :background "dark slate gray"))))
 )
;;;;+
