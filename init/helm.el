;;;; helm
(custom-set-variables
 '(helm-input-idle-delay 0)
 '(helm-exit-idle-delay 0)
 '(helm-candidate-number-limit 500)
 '(helm-ag-insert-at-point 'symbol)
 '(helm-find-files-doc-header "")
 '(helm-command-prefix-key nil))

(with-eval-after-load 'helm
  (helm-descbinds-mode +1)

  (define-key helm-map (kbd "C-p")   #'helm-previous-line)
  (define-key helm-map (kbd "C-n")   #'helm-next-line)
  (define-key helm-map (kbd "C-M-n") #'helm-next-source)
  (define-key helm-map (kbd "C-M-p") #'helm-previous-source))

(with-eval-after-load 'helm-files
  (remove-hook 'post-self-insert-hook 'helm-find-files--reset-level-tree)

  (define-key helm-find-files-map (kbd "C-M-u") #'helm-find-files-down-one-level)
  (define-key helm-find-files-map (kbd "C-c C-o") #'helm-ff-run-switch-other-window))


;;;;+ Extra

(with-eval-after-load 'helm
  (define-key helm-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
  ;; (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
  ;; (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "M-C-i")  'helm-select-action)

  (define-key global-map (kbd "C-x b") 'helm-mini)
  (define-key global-map (kbd "C-x C-f") 'helm-find-files)
  (define-key global-map (kbd "M-x") 'helm-M-x)
  (define-key global-map (kbd "M-y") 'helm-show-kill-ring)

  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-set-key (kbd "C-c h o") 'helm-occur)


  (when (executable-find "curl")
    (setq url-retrieve-synchronously t))

  ) ;; helm ends here
