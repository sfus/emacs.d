;;; Commentary:
;;; init.el

;;; initial settings
;; GC
(setq-default gc-cons-threshold (* 256 1024 1024)) ;; 256MB

;; cl-lib
(require 'cl-lib)

;; minimum key-binding
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)

;; if `-l' option is set, then change `user-emacs-directory'
(when load-file-name
  (setq-default user-emacs-directory (file-name-directory load-file-name)))

;; add `lisp/' to load-path recursively. https://www.emacswiki.org/emacs/LoadPath
(let ((default-directory (concat user-emacs-directory "lisp/")))
  (if (file-directory-p default-directory)
      (normal-top-level-add-subdirs-to-load-path))) ;; this uses `default-directory' variable.

;; load private setting
(let ((private-el (concat user-emacs-directory "init-private.el")))
  (when (file-exists-p private-el)
    (load private-el)))


;;; package.el
(add-to-list 'load-path (concat user-emacs-directory "elpa/"))
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))


;;; use-package
;; -> https://github.com/jwiegley/use-package/blob
;; -> https://qiita.com/kai2nenobu/items/5dfae3767514584f5220
(when (not (package-installed-p 'use-package))
  (package-install 'use-package))
(setq use-package-enable-imenu-support t) ;; need to be set before (require 'use-package)
(require 'use-package)

;; defer keywords
;; -> https://github.com/jwiegley/use-package#modes-and-interpreters
;; :commands, :bind, :bind*, :bind-keymap, :bind-keymap*, :mode, :interpreter, or :hook

;;; el-get
;; -> https://github.com/edvorg/use-package-el-get
(use-package el-get
  :ensure t
  :config
  (add-to-list 'load-path (concat user-emacs-directory "el-get/el-get"))
  (add-to-list 'el-get-recipe-path (concat user-emacs-directory "el-get/el-get/recipes"))
  (el-get-bundle syohex/emacs-import-popwin :name import-popwin)
  (el-get-bundle sfus/org-screenshot :name org-attach-screenshot)
  (el-get-bundle sfus/text-adjust.el :name text-adjust)
  ) ;; el-get


;;; Load environment variables by exec-path-from-shell
;; -> https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :init
  ;; Change exec-path-from-shell-arguments for the following warning:
  ;;   Warning: exec-path-from-shell execution took 3947ms. See the README for tips on reducing this.
  (setq exec-path-from-shell-arguments '("-l"))
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("PATH" "VIRTUAL_ENV" "GOROOT" "GOPATH" "EIJIRO_DIR")))


;;; init-loader
(use-package init-loader
  :ensure t
  :init
  ;; (setq init-loader-show-log-after-init 'error-only) ;; default: t
  :config
  (init-loader-load (concat user-emacs-directory "init-loader")))


;;; Mac settings
;; synchronize kill-ring and Mac's clipboard
;; http://qiita.com/tstomoki/items/24d63217f797c6929a23
(when (eq system-type 'darwin)
  (defun copy-from-osx ()
    (shell-command-to-string "pbpaste"))
  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))
  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx))


;;; Emacs server for emacsclient
(require 'server)
(defun server-ensure-safe-dir (dir) "Noop" t)
(if (not (file-directory-p server-socket-dir))
    (make-directory server-socket-dir))
(unless (server-running-p)
  (server-start))


;; Report Emacs init time
(defun my/emacs-init-time ()
  "Emacs init time in msec."
  (message "Emacs init time: %.0f [msec]"
           (* 1000 (float-time (time-subtract after-init-time before-init-time)))))
(if load-file-name
    (add-hook 'emacs-startup-hook #'my/emacs-init-time t)
  (add-hook 'after-init-hook #'my/emacs-init-time t))

(defvar my/emacs-init-time-reported nil)
(defadvice server-execute (after my/server-execute activate)
  "Overwrite initial minibuffer message like `When done with this frame, type C-x 5 0'"
  (unless my/emacs-init-time-reported
      (my/emacs-init-time))
  (setq my/emacs-init-time-reported t))
