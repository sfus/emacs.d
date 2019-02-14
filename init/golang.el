;; Go Lang
(custom-set-variables
 '(ac-go-expand-arguments-into-snippets nil)
 '(company-go-insert-arguments nil)
 '(gofmt-command "goimports"))

(with-eval-after-load 'go-mode
  (add-hook 'go-mode-hook 'my/go-mode-hook)
  (add-hook 'go-mode-hook 'go-eldoc-setup)

  (require 'go-autocomplete)
  (require 'go-guru)

  (define-key go-mode-map (kbd "C-c a") 'go-import-add)
  (define-key go-mode-map (kbd "C-c C-a") 'helm-godoc-import)
  (define-key go-mode-map (kbd "C-c C-j") 'go-direx-pop-to-buffer)
  (define-key go-mode-map (kbd "C-c t") 'go-add-tags)
  (define-key go-mode-map (kbd "C-c C-d") 'helm-godoc)
  (define-key go-mode-map (kbd "C-c d") 'progutil-go-gogetdoc)
  (define-key go-mode-map (kbd "C-c p s") 'go-set-project)
  (define-key go-mode-map (kbd "C-c p r") 'go-reset-gopath)
  (define-key go-mode-map (kbd "M-.") 'godef-jump)
  (define-key go-mode-map (kbd "M-,") 'pop-tag-mark)

  (define-key go-mode-map (kbd ":") nil)

  ;;;;+ Extra
  (define-key go-mode-map (kbd "C-c C-r") 'go-remove-unused-imports)

  ;; godef-jump ($ go get -u code.google.com/p/rog-go/exp/cmd/godef)
  (define-key go-mode-map (kbd "M-j") 'godef-jump) ;; default: C-c C-j
  ;;;;+

  (progutil-go-setup))

(defun my/go-mode-hook ()
  ;;(setq-local company-backends '(company-go company-files company-dabbrev))
  (delete 'ac-source-words-in-same-mode-buffers ac-sources)
  (setq compile-command "go test")
  (setq flycheck-go-vet-shadow 'strict)

  ;;;;+ Extra
  (add-hook 'before-save-hook 'gofmt-before-save) ;; run gofmt on save

  (set (make-local-variable 'ac-auto-start) t)
  ;;;;+
  )


;;;; go-mode
;; -> http://qiita.com/senda-akiha/items/8bbdd3e59c51d5619ea7
;;
;; C-c C-a (go-import-add)
;;         (go-remove-unused-imports)
;; C-M-a   (beginning-of-defun)
;; C-M-e   (end-of-defun)
;; C-M-h   (mark-defun)
;; C-x n d (narrow-to-defun)
;;         (go-goto-imports)
;; C-c C-d (godef-describe)
;;         (go-coverage)
