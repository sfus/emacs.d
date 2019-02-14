;;;; GNU gtags
(custom-set-variables
 '(helm-gtags-pulse-at-cursor nil))

(with-eval-after-load 'helm-gtags
  (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
  (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
  (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
  (define-key helm-gtags-mode-map (kbd "M-g M-s") 'helm-gtags-select)
  (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
  (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
  (define-key helm-gtags-mode-map (kbd "C-t") 'helm-gtags-pop-stack))

;;; Enable helm-gtags-mode
;; (dolist (hook '(c-mode-common-hook
;;                 java-mode-hook
;;                 asm-mode-hook
;;                 ))
;;   (add-hook hook 'helm-gtags-mode))


;;;;+ Extra

;;; helm-gtags
;; (when (require 'helm-gtags nil t)
;;   (setq helm-gtags-direct-helm-completing t)    ;; not read in minibuffer
;;   (setq helm-gtags-use-input-at-cursor t)
;;   (setq helm-gtags-suggested-key-mapping t)
;;   (setq helm-gtags-pulse-at-cursor t)           ;; revert to default
;;   (setq helm-gtags-auto-update t)
;;   (define-key helm-gtags-mode-map (kbd "M-T") 'helm-gtags-pop-stack)
;;   (define-key helm-gtags-mode-map (kbd "C-c R") 'helm-gtags-create-tags)
;;   ) ;; helm-gtags


;;; ggtags
;; -> https://github.com/leoliu/ggtags
;; -> https://qiita.com/yoshizow/items/9cc0236ac0249e0638ff
;; -> http://udzura.hatenablog.jp/entry/2016/07/21/142628

(when (require 'ggtags nil t)
  (defun my/gtags-mode-hook ()
    ;; choose ggtags-mode or helm-gtags-mode
    (ggtags-mode 1)
    ;;(helm-gtags-mode 1)

    (add-to-list 'ac-sources 'ac-source-gtags)
    )

  (dolist (hook '(c-mode-common-hook
                  java-mode-hook
                  asm-mode-hook
                  cperl-mode-hook
                  go-mode-hook
                  ))
    (add-hook hook 'my/gtags-mode-hook))

  ;; ;; not create tags files on save (default: t)
  ;; (setq ggtags-update-on-save nil)

  ;; to use helm
  (setq ggtags-completing-read-function nil)

  (define-key ggtags-navigation-map (kbd "C-g") 'ggtags-navigation-mode-done)

  ;; projectile
  (setq projectile-tags-backend 'ggtags)

) ;; ggtags


;;; projectile to use gtags

(when (executable-find "gtags")
  (setq projectile-tags-file-name "GTAGS")
  (setq projectile-tags-command "gtags")
  ) ;; projectile

;; (setenv "GTAGSLIBPATH" (concat
;;       "/Users/udzura/.ghq/github.com/mruby/mruby"
;;       ":" "/Users/udzura/.ghq/git.kernel.org/pub/scm/linux/kernel/git/stable/linux-stable"
;;       ":" "/Users/udzura/.ghq/sourceware.org/git/glibc"
;;       ":" "/Users/udzura/.ghq/git.code.sf.net/p/libcg/libcg"
;;       ":" "/Users/udzura/.ghq/git.kernel.org/pub/scm/linux/kernel/git/morgan/libcap"))
