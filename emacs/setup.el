;; Emacs lisp install file

(require 'cl)

(defvar my/elisp-directory)
(defvar my/init-loader-directory)

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name))
  (setq custom-theme-directory (concat user-emacs-directory "elisps/")))

(setq my/elisp-directory (concat user-emacs-directory "elisps/"))
(unless (file-directory-p my/elisp-directory)
  (make-directory my/elisp-directory))

(setq my/init-loader-directory (concat user-emacs-directory "init_loader/"))
(unless load-file-name
  (unless (file-symlink-p my/init-loader-directory)
    (make-symbolic-link (concat default-directory "init_loader")
                        user-emacs-directory)))

;; first eval this code block
(add-to-list 'load-path my/elisp-directory)

;; check commands
(dolist (cmd '("curl" "cvs"))
  (unless (executable-find cmd)
    (error "Please install %s" cmd)))

;; Emacs package system
(require 'package)
(setq package-user-dir (concat user-emacs-directory "elpa"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(package-refresh-contents)

(defvar my/install-packages
  '(
    ;;;; for auto-complete
    auto-complete fuzzy popup ac-slime pos-tip

    ;;;; highlight
    ace-jump-mode vline col-highlight

    ;;;; editing utilities
    expand-region wrap-region
    undo-tree mark-multiple smartrep
    yasnippet goto-chg

    ;;;; buffer utils
    popwin elscreen yascroll

    ;;;; programming
    ;; haskell
    haskell-mode

    ;; flymake
    flycheck

    ;; clojure
    clojure-mode nrepl

    ;; coffee-script
    coffee-mode

    ;; perl
    cperl-mode

    ;; python
    jedi

    ;; ruby
    ruby-block ruby-compilation ruby-end ruby-interpolation
    ruby-mode ruby-test-mode ruby-tools inf-ruby
    yari

    ;; emacs-lisp
    elisp-slime-nav thingopt

    ;; Common Lisp
    slime paredit

    ;; common utility
    quickrun

    ;;;; markup language
    haml-mode sass-mode htmlize
    markdown-mode markdown-mode+
    scss-mode yaml-mode zencoding-mode

    ;; helm
    helm helm-gtags helm-descbinds helm-themes helm-ag

    ;; git
    magit git-gutter

    ;; directory operation
    direx

    init-loader
))

(dolist (package my/install-packages)
  (unless (package-installed-p package)
    (package-install package)))

(defun my/download-url (url)
  (assert (stringp url))
  (if (zerop (call-process "curl" nil nil nil "-O" url))
      (message "Success Download %s" url)
    (message "Failed Download %s" url)))

(defun my/system (cmd)
  (message "Execute '%s'" cmd)
  (unless (zerop (call-process-shell-command cmd))
    (error "%s is failed!!" cmd)))

(defvar my/nonelpa-packages-url
  '(
    ;; my own utilities
    "https://raw.github.com/syohex/emacs-utils/master/pomodoro.el"
    "https://raw.github.com/syohex/emacs-utils/master/sgit.el"
    "https://raw.github.com/syohex/emacs-utils/master/helm-myutils.el"

    ;; 3rd pirty
    "http://homepage1.nifty.com/bmonkey/emacs/elisp/iman.el"
    "https://github.com/renormalist/emacs-pod-mode/raw/master/pod-mode.el"
    "http://www.emacswiki.org/emacs/download/xs-mode.el"
    "http://static.boinkor.net/lisp/cl-indent-patches.el"
    ))

;; setup nonelpa packages
(let ((default-directory my/elisp-directory))
  (dolist (url my/nonelpa-packages-url)
    (unless (file-exists-p (file-name-nondirectory url))
      (my/download-url url))))

;; rsense
(let ((default-directory my/elisp-directory))
  (when (not (file-directory-p "rsense"))
    (message "Install rsense(Wait a minute)")
    (my/system "curl -O http://cx4a.org/pub/rsense/rsense-0.3.zip")
    (my/system "unzip rsense-0.3.zip")
    (my/system "mv rsense-0.3 rsense")
    (my/system "rm -f rsense-0.3.zip")))

;; w3m
(let ((default-directory my/elisp-directory))
  (when (not (file-directory-p "emacs-w3m"))
    (message "Install emacs-w3m(Wait a minute)")
;;    (my/system "cvs -d :pserver:anonymous@cvs.namazu.org:/storage/cvsroot login")
    (my/system "cvs -d :pserver:anonymous@cvs.namazu.org:/storage/cvsroot co emacs-w3m")
    (let ((default-directory (concat default-directory "emacs-w3m")))
      (my/system "autoconf && ./configure && make"))))

;; Hyperspec
(let ((default-directory my/elisp-directory))
  (when (not (file-directory-p "HyperSpec"))
    (message "Install HyperSpec(Wait a minute)")
    (unless (file-exists-p "HyperSpec-7-0.tar.gz")
      (my/download-url "ftp://ftp.lispworks.com/pub/software_tools/reference/HyperSpec-7-0.tar.gz"))
    (my/system "tar xf HyperSpec-7-0.tar.gz")
    (my/system "rm -f HyperSpec-7-0.tar.gz")))

;; setup theme
(let ((default-directory custom-theme-directory))
  (when (not (file-exists-p "reverse-theme.el"))
    (message "Install reverse theme")
    (my/download-url
     "https://raw.github.com/syohex/emacs-reverse-theme/master/reverse-theme.el")))

(load-theme 'reverse t t)
(enable-theme 'reverse)

(init-loader-load my/init-loader-directory)