;;;; minor key setting

;; (makunbound 'overriding-minor-mode-map)
(define-minor-mode overriding-minor-mode
  "Most superior minir mode"
  t  ;; default is enable
  "" ;; Not display mode-line
  `((,(kbd "C-M-j") . dabbrev-expand)
    (,(kbd "C-M-i") . my/auto-complete)
    (,(kbd "M-q") . editutil-forward-char)
    (,(kbd "C-M-o") . editutil-other-window)))
