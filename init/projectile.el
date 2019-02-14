;;;; projectile
;; -> https://github.com/bbatsov/projectile
;; -> http://projectile.readthedocs.io/en/latest/
;; -> https://tuhdo.github.io/helm-projectile.html

(global-unset-key (kbd "C-z"))
(setq projectile-keymap-prefix (kbd "C-z")) ;; default: C-c p

(when (require 'projectile nil t)
  (projectile-mode)
  (setq projectile-mode-line '(:eval (format " Pj[%s]" (projectile-project-name)))) ;; default: (:eval (format " Projectile[%s]" (projectile-project-name)))
  (setq projectile-switch-project-action 'projectile-dired)

  (define-key projectile-command-map (kbd "C-f") 'projectile-find-file-dwim)
  (define-key projectile-command-map (kbd "C-r") 'projectile-recentf)

  (with-eval-after-load "helm"
    (setq projectile-completion-system 'helm))

  (when (require 'helm-projectile nil t)
    (helm-projectile-on)
    (define-key projectile-command-map (kbd "C-h") 'helm-projectile)

    ;;(setq projectile-switch-project-action 'helm-projectile)
    ) ;; helm-projectile ends here

  ) ;; projectile ends here
