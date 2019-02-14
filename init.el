;; for launcher (package-initialize)
(unless load-file-name
  (cd (getenv "HOME")))

(require 'cl-lib)

(when load-file-name
  (setq-default user-emacs-directory (file-name-directory load-file-name)))

;;;;+ Extra
;;; add load-path recursively. https://www.emacswiki.org/emacs/LoadPath
(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path)) ;; this uses `default-directory' variable.
;;;;+

(load (concat user-emacs-directory "init-el-get.el"))

;;;;+ Extra
(load (concat user-emacs-directory "init-el-get-extra.el"))

(let ((private-el (concat user-emacs-directory "init-private.el")))
  (when (file-exists-p private-el)
    (load private-el)))

;;; Emacs server
(require 'server)
(defun server-ensure-safe-dir (dir) "Noop" t)
(if (not (file-directory-p server-socket-dir))
    (make-directory server-socket-dir))
(unless (server-running-p)
  (server-start))

;;;;+

;; load environment variables
(custom-set-variables
 '(exec-path-from-shell-check-startup-files nil))
(exec-path-from-shell-copy-envs '("PATH" "VIRTUAL_ENV" "GOROOT" "GOPATH" "EIJIRO_DIR"))

;;;; setup theme
(load-theme 'syohex t t)
(enable-theme 'syohex)

;; init-loader
(custom-set-variables
 '(init-loader-show-log-after-init 'error-only))
(init-loader-load (concat user-emacs-directory "init-loader"))
