;; for lancher
(unless load-file-name
  (cd (getenv "HOME")))

;; Add load path of emacs lisps
(add-to-list 'load-path (concat user-emacs-directory "elisps"))

;; Emacs package system
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; load environment variables
(let ((envs '("PATH" "VIRTUAL_ENV" "GOROOT" "GOPATH")))
  (exec-path-from-shell-copy-envs envs))

;;;; setup theme
(load-theme 'reverse t t)
(enable-theme 'reverse)

;; init-loader
(custom-set-variables
 '(init-loader-show-log-after-init 'error-only))
(init-loader-load (concat user-emacs-directory "init_loader"))
