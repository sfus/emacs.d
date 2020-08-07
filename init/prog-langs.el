;;; asm-mode
(use-package asm-mode
  :bind (:map asm-mode-map
              ("RET" . newline))
  ) ;; asm-mode


;;; conf-unix-mode
(use-package conf-unix-mode
  :mode ("/\\.gitconfig$" "/\\.gitignore$" "/\\..+rc$")
  ) ;; conf-unix-mode


;;; vimrc-mode
(use-package vimrc-mode
  :ensure t
  :mode "\\.vim\\(rc\\)?\\'\\|vifmrc\\'"
  ) ;; vimrc-mode


;;; yaml-mode
(use-package yaml-mode
  :ensure t
  :mode ("\\.yaml\\'" "\\.yml\\'")
  ) ;; yaml-mode


;;; c-mode
(use-package c-mode
  :mode ("\\.mm\\'" . c++-mode)
  :hook ((c-mode-hook . my/c-mode-hook)
         (c++-mode-hook . my/c-mode-hook)
         (objc-mode-hook  . my/objc-mode-hook))
  :bind (:map c-mode-base-map
              ("C-c C-s" . clang-format-buffer))

  :config
  (defun my/c-mode-hook ()
    (c-set-style "k&r")
    (setq indent-tabs-mode t
          c-basic-offset 8)
    (c-toggle-electric-state -1)
    (setq-local company-backends '(company-clang company-dabbrev)))

  (defun my/objc-mode-hook ()
    (setq c-basic-offset 4)
    (c-toggle-electric-state -1)
    (setq-local company-backends '(company-clang company-dabbrev)))

  ) ;; c-mode


;;; cider for clojure
(use-package cider
  :ensure t
  :hook ((cider-mode-hook  . eldoc-mode)
         (cider-repl-mode-hook . eldoc-mode)
         ;; (cider-mode-hook . ac-cider-setup)
         ;; (cider-repl-mode-hook . ac-cider-setup)
         )
  :bind (:map cider-mode-map
         ("C-M-i" . company-complete)
         ("C-c C-q" . nil))
  :init
  (setq cider-prompt-save-file-on-load 'always-save)
  (setq cider-font-lock-dynamically '(macro core function var))
  (setq nrepl-hide-special-buffers t)

  :config
  (with-eval-after-load 'clj-refactor
    (cljr-add-keybindings-with-prefix "C-c C-m"))

  ) ;; cider


;;; coffee-mode
(use-package coffee-mode
  :ensure t
  :hook ((coffee-mode-hook . my/coffee-mode-hook)
         (coffee-after-compile-hook . sourcemap-goto-corresponding-point))
  :bind (:map coffee-mode-map
              ([remap newline-and-indent] . nil)
              ("C-m" . nil)
              ("C-<return>" . coffee-newline-and-indent)
              ("M-o" . my/coffee-edit-next-line)
              ("C-j" . coffee-newline-and-indent))
  :init
  (setq coffee-tab-width 2)
  (setq coffee-indent-like-python-mode t)
  (setq coffee-args-compile '("-c" "-m"))

  :config
  (defun my/coffee-edit-next-line ()
    (interactive)
    (goto-char (line-end-position))
    (coffee-newline-and-indent))

  (use-package smartrep
    :ensure t
    :config
    (smartrep-define-key
        coffee-mode-map "C-c" '(("h" . 'coffee-indent-shift-left)
                                ("l" . 'coffee-indent-shift-right))))
  ) ;; coffee-mode

;; Default Key Bindings
;;
;; Key              Command
;; C-m, Return      Insert newline and indent line
;; C-c C-<, backtab Indent line or region to left
;; C-c C->          Indent line or region to right
;; C-M-a            Move to beginning of defun
;; C-M-e            Move to end of defun
;; C-M-h            Mark this defun
;; A-r, C-c C-k     Compile buffer to JavaScript
;; A-R              Compile content of region to JavaScript
;; A-M-r, C-c C-z   Run CoffeeScript REPL
;; C-c C-l          Send this line to REPL buffer
;; C-c C-r          Send content of region to REPL buffer
;; C-c C-b          Send content of buffer to REPL buffer
;; C-c C-o C-s      Enable coffee-cos-mode


;;; emacs-lisp-mode
(use-package emacs-lisp-mode
  :mode "Cask\\'"
  :hook ((emacs-lisp-mode . eldoc-mode)
         ;;(emacs-lisp-mode . my/elisp-mode-hook)
         )
  :init
  ;; inhibit Edebug bindings on the C-x C-a key.
  (setq-default edebug-inhibit-emacs-lisp-mode-bindings t)

  ;; (defun my/elisp-mode-hook ()
  ;;   ;;(setq ac-sources '(ac-source-features ac-source-functions ac-source-variables))
  ;;   (setq-local company-backends '(company-elisp (company-dabbrev-code company-keywords) company-dabbrev))
  ;;   (setq-local tab-width 8))

  :config
  ;; word boundary (not treat `-' as word boundary)
  (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)

  ) ;; emacs-lisp-mode


;; ;;; elisp-slime-nav-mode
;; (use-package elisp-slime-nav-mode
;;   :hook (emacs-lisp-mode-hook . ielm-mode-hook)
;;   ) ;; elisp-slime-nav-mode


;;; elixir
(use-package elixir-mode
  :ensure t
  :defer t
  :hook ((elixir-mode-hook . alchemist-mode)
         ;;(elixir-mode-hook . ac-alchemist-setup)
         )
  ) ;; elixir


;;; erlang
(use-package erlang
  :ensure t
  :defer t
  :init
  (setq erlang-electric-commands '(erlang-electric-comma erlang-electric-semicolon))
  (setq erlang-electric-newline-inhibit-list '(erlang-electric-gt))
  (setq erlang-electric-newline-inhibit t)

  :config
  (require 'erlang-start)
  (require 'erlang-flymake)
  ) ;; erlang


;; ;;; distel for erlang
;; ;; -> https://github.com/massemanet/distel
;; ;; -> https://masayuki038.github.io/blog/2013/04/18/erlang-mode-and-distel/
;; (use-package distel
;;   :ensure t
;;   :defer t
;;   :init
;;   (add-to-list 'load-path (concat user-emacs-directory "elisps/distel/elisp/")
;;   :config
;;   ;;(distel-setup)
;;  ) ;; distel


;;; Go
(use-package go-mode
  :ensure t
  :bind (:map go-mode-map
              ("C-c a"      . go-import-add)
              ("C-c C-a"    . helm-godoc-import)
              ("C-c C-j"    . go-direx-pop-to-buffer)
              ("C-c t"      . go-add-tags)
              ("C-c C-d"    . helm-godoc)
              ("C-c d"      . progutil-go-gogetdoc)
              ("C-c p s"    . go-set-project)
              ("C-c p r"    . go-reset-gopath)
              ("M-."        . godef-jump)
              ("M-,"        . pop-tag-mark)
              (":"          . nil)
              ("C-c C-r"    . go-remove-unused-imports)

              ;; godef-jump ($ go get -u code.google.com/p/rog-go/exp/cmd/godef)
              ("M-j"        . godef-jump) ;; default: C-c C-j
              )
  :hook ((go-mode-hook . my/go-mode-hook)
         (go-mode-hook . go-eldoc-setup))
  :init
  (setq ac-go-expand-arguments-into-snippets nil)
  (setq company-go-insert-arguments nil)
  (setq gofmt-command "goimports")

  (defun my/go-mode-hook ()
    ;;(setq-local company-backends '(company-go company-files company-dabbrev))
    (delete 'ac-source-words-in-same-mode-buffers ac-sources)
    (setq compile-command "go test")
    (setq flycheck-go-vet-shadow 'strict)

    (add-hook 'before-save-hook 'gofmt-before-save) ;; run gofmt on save
    (set (make-local-variable 'ac-auto-start) t)
    )

  :config
  (use-package go-autocomplete
    :ensure t)
  (use-package go-guru
    :ensure t)

  ) ;; go

;; go-mode
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


;;; haskell
(use-package haskell-mode
  :ensure t
  :bind (:map haskell-mode-map
              ("C-c C-d"        . helm-editutil-ghc-browse-document)
              ("M-o"            . editutil-edit-next-line-same-column)
              ("TAB"            . haskell-simple-indent)
              ("<backtab>"      . haskell-simple-indent-backtab)
              ([remap newline]  . haskell-simple-indent-newline-same-col)
              ([remap newline-and-indent] . haskell-simple-indent-newline-indent)
              ("C-<return>"     . haskell-simple-indent-newline-indent))
  :hook ((haskell-mode-hook . my/haskell-mode-hook))
  :init
  (defun my/haskell-mode-hook ()
    (turn-on-haskell-doc-mode)
    (turn-on-haskell-indent)

    ;; I don't want to set key bindings
    (ghc-abbrev-init)
    (ghc-type-init)
    (unless ghc-initialized
      (ghc-comp-init)
      (setq ghc-initialized t))

    ;; for auto-complete
    (add-to-list 'ac-sources 'ac-source-ghc-mod))

  :config
  ;; (use-package haskell-simple-indent
  ;;   :ensure t)

  (use-package ghc
    :ensure t
    :init
    (setq ghc-module-command (executable-find "ghc-mod"))
    ) ;; ghc

  ;; Wrap region with block comment
  (defun my/haskell-block-commend-region (start end)
    (interactive "r")
    (save-excursion
      (let (end-marker)
        (goto-char end)
        (setq end-marker (point-marker))
        (goto-char start)
        (insert "{-\n")
        (goto-char (marker-position end-marker))
        (insert "-}"))))

  ) ;; haskell


;;; Java
(use-package java-mode
  :defer t
  :hook (java-mode-hook . my/java-mode-hook)
  :init
  (defun my/java-mode-hook ()
    ;;(setq indent-tabs-mode t c-basic-offset 8)
    (setq indent-tabs-mode nil c-basic-offset 4)
    (c-toggle-electric-state -1))
  ) ;; java


;;; js-mode
(use-package js-mode
  :hook (js-mode-hook . my/js-mode-hook)
  :init
  (setq js-auto-indent-flag nil)
  (setq js-indent-level 2) ;; this variable also affects js2-mode indent level
  (defun my/js-mode-hook ()
    (setq-local company-backends '(company-tern company-dabbrev)))
  ) ;; js-mode


;;; js2-mode
;; -> https://github.com/mooz/js2-mode
;; -> https://sites.google.com/site/shidoinfo/Home/programing-lang/%E9%96%A2%E6%95%B0%E5%9E%8B%E3%83%97%E3%83%AD%E3%82%B0%E3%83%A9%E3%83%9F%E3%83%B3%E3%82%B0%E8%A8%80%E8%AA%9E/ecmascript/javascript-kai-fa-huan-jing/emacs-javascript/js2-mode
(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :init
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode)) ;; (version< emacs-version "27.0")

  (defun my/set-jsx-indentation ()
    (setq-local sgml-basic-offset js2-basic-offset))
  (add-hook 'js2-jsx-mode-hook #'my/set-jsx-indentation)
  ) ;; js2-mode


;;; markdown.el
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :bind (:map markdown-mode-map
              ;;("C-M-i" . auto-complete)
              ("C-M-i" . company-complete)
              ("C-c C-c C-l" . markdown-insert-link)
              ("C-c C-c C-i" . markdown-insert-image)
              ("C-c ." . org-time-stamp)
              :map gfm-mode-map
              ("C-c C-c C-c" . markdown-insert-gfm-code-block)
              ("`" . nil))
  :hook (markdown-mode-hook . my/markdown-mode-hook)
  :init
  (setq markdown-indent-on-enter nil)

  (defun my/markdown-mode-hook ()
    (setq-local company-backends '(company-ispell company-files company-dabbrev))
    (make-local-variable 'electric-pair-pairs)
    (add-to-list 'electric-pair-pairs '(?` . ?`))
    (setq-local tab-width 8))

  ) ;; markdown-mode


;;; perl

;;;; cperl-mode
(add-to-list 'auto-mode-alist
             '("\\.\\(pl\\|pm\\|cgi\\|t\\|psgi\\)\\'" . cperl-mode))
(add-to-list 'auto-mode-alist '("cpanfile\\'" . cperl-mode))
(defalias 'perl-mode 'cperl-mode)

(with-eval-after-load 'cperl-mode
  (cperl-set-style "PerlStyle")
  (setq cperl-auto-newline nil)

  (helm-perldoc:setup)

  ;; bindings
  (define-key cperl-mode-map "\177" nil)
  (define-key cperl-mode-map (kbd ";") nil)
  (define-key cperl-mode-map (kbd ":") nil)
  (define-key cperl-mode-map (kbd "(") nil)
  (define-key cperl-mode-map (kbd "{") nil)
  (define-key cperl-mode-map (kbd "}") nil)
  (define-key cperl-mode-map (kbd "[") nil)

  (define-key cperl-mode-map (kbd "C-c C-d") 'helm-perldoc)
  (define-key cperl-mode-map (kbd "C-c C-r") 'helm-perldoc:history))

(defun my/cperl-imenu-create-index ()
  (let (index)
    (save-excursion
      ;; collect subroutine
      (goto-char (point-min))
      (while (re-search-forward "^\\s-*sub\\s-+\\([^ ]+\\)" nil t)
        (push (cons (format "Function: %s" (match-string 1))
                    (match-beginning 1)) index))

      ;; collect subtest
      (goto-char (point-min))
      (let ((desc-re "^\\s-*subtest\\s-+\\(['\"]\\)\\([^\1\r\n]+\\)\\1"))
        (while (re-search-forward desc-re nil t)
          (push (cons (format "Subtest: %s" (match-string 2))
                      (match-beginning 0)) index)))
      (nreverse index))))

(defun my/cperl-mode-hook ()
  (hs-minor-mode 1)
  ;;;;+ Extra
  (set (make-local-variable 'ac-auto-start) t)
  ;;;;

  ;; my own imenu. cperl imenu is too many information for me
  (setq imenu-create-index-function 'my/cperl-imenu-create-index))

(add-hook 'cperl-mode-hook 'my/cperl-mode-hook)

(custom-set-variables
 '(cperl-indent-parens-as-block t)
 '(cperl-close-paren-offset -4)
 '(cperl-indent-subs-specially nil))
;;;; plantuml.el

;; setting plantuml
(add-to-list 'auto-mode-alist '("\.puml$" . plantuml-mode))

(with-eval-after-load 'plantuml-mode
  (setq plantuml-jar-path (shell-command-to-string "cat `which plantuml` | awk '/plantuml.jar/ { print $(NF - 1)}' | tr -d '\n'"))
  (setq plantuml-output-type "txt")

  (defun my/plantuml-save-and-preview-other-window (&optional arg)
    (interactive "p")
    (save-buffer arg)
    (let ((win (get-buffer-window plantuml-preview-buffer)))
      (if win
          (delete-window win)))
    (plantuml-preview 4))

  (defun my/plantuml-save-and-preview-other-tmux-pane (&optional arg)
    (interactive "p")
    (save-buffer arg)
    (call-process-shell-command (format "tmux split-window -h 'cat %s | plantuml -p | imgcat && read'" buffer-file-name) nil 0))

  (defun my/plantuml-save-and-preview-os-command (&optional arg)
    (interactive "p")
    (save-buffer arg)
    (call-process-shell-command (format "plantuml %s && open %s.png" buffer-file-name (file-name-base buffer-file-name)) nil 0))

  (defun my/plantuml-mode-hook ()
    (define-key plantuml-mode-map (kbd "C-c C-s") 'my/plantuml-save-and-preview-other-window)
    (define-key plantuml-mode-map (kbd "C-c C-c") 'my/plantuml-save-and-preview-other-tmux-pane)
    (define-key plantuml-mode-map (kbd "C-c c") 'my/plantuml-save-and-preview-os-command))

  (add-hook 'plantuml-mode-hook 'my/plantuml-mode-hook)

  )
;;;; python.el

;; python-setting

(defun my/python-mode-hook ()
  (jedi:setup)
  (setq-local company-backends '(company-jedi company-dabbrev))

  ;; flycheck
  (setq flycheck-flake8rc (expand-file-name "~/.config/flake8")))

(with-eval-after-load 'python
  (add-hook 'python-mode-hook 'my/python-mode-hook)

  ;; binding
  (define-key python-mode-map (kbd "C-M-i") 'company-complete)
  (define-key python-mode-map (kbd "C-c C-d") 'helm-pydoc)
  (define-key python-mode-map (kbd "C-c C-h") 'jedi:show-doc)
  (define-key python-mode-map (kbd "C-c C-l") 'jedi:get-in-function-call)

  (smartrep-define-key
      python-mode-map "C-c" '(("h" . 'python-indent-shift-left)
                              ("l" . 'python-indent-shift-right))))

;; jedi
(custom-set-variables
 '(jedi:tooltip-method nil))
;;;; ruby.el

;; setting for ruby
(custom-set-variables
 '(ruby-deep-indent-paren nil)
 '(ruby-insert-encoding-magic-comment nil)
 '(robe-completing-read-func #'helm-robe-completing-read)
 '(robe-highlight-capf-candidates nil))

(with-eval-after-load 'ruby-mode
  (progutil-ruby-setup)

  (add-hook 'ruby-mode-hook 'my/ruby-mode-hook)
  ;; rbenv
  ;;(global-rbenv-mode t) ;; I think it may not be needed

  ;; binding
  (define-key ruby-mode-map (kbd "C-c C-a") 'ruby-beginning-of-block)
  (define-key ruby-mode-map (kbd "C-c C-e") 'ruby-end-of-block)
  (define-key ruby-mode-map (kbd "C-c ?") 'robe-doc)

  ;; disable default bindings
  (dolist (key '("(" ")" "{" "}" "[" "]" "\"" "'"))
    (define-key ruby-mode-map key nil)))

(defun my/ruby-mode-hook ()
  ;; robe
  (robe-mode +1)
  ;; robe-eldoc often display wrong information
  (setq-local eldoc-documentation-function nil)
  ;;(add-to-list 'ac-sources 'ac-source-robe)
  (setq-local company-backends '(company-robe company-dabbrev))

  ;; auto insert `end'
  (ruby-end-mode +1))
;;;; rust.el

(custom-set-variables
 '(rust-format-on-save nil))

(with-eval-after-load 'rust-mode
  (flycheck-rust-setup)

  (define-key rust-mode-map (kbd "C-c C-s") 'rust-format-buffer))

(add-hook 'rust-mode-hook 'racer-mode)
(add-hook 'racer-mode-hook 'eldoc-mode)
;;;; scheme.el

;; setting for scheme
(with-eval-after-load "scheme"
  (require 'cmuscheme)

  (progutil-scheme-setup 'gauche)

  (push '("*scheme*" :stick t) popwin:special-display-config)
  (define-key scheme-mode-map (kbd "C-c C-z") 'run-scheme)
  (define-key scheme-mode-map (kbd "C-c C-c") 'scheme-send-definition)
  (define-key scheme-mode-map (kbd "C-c C-e") 'scheme-send-definition-and-go))

(add-hook 'scheme-mode-hook 'progutil-scheme-hook)
;;;; shell.el

;; Shell/Command utilities

;; direnv
(add-to-list 'auto-mode-alist '("/\\.envrc\\'" . sh-mode))

;; compilation
(custom-set-variables
 '(compile-command "")
 '(compilation-always-kill t)
 '(compilation-message-face nil)
 '(compilation-auto-jump-to-first-error t))

;; comint
(custom-set-variables
 '(comint-input-ignoredups t))

(defun my/colorize-compilation-buffer ()
  (ansi-color-process-output nil))

(with-eval-after-load 'compile
  (add-hook 'compilation-filter-hook 'my/colorize-compilation-buffer))

;; eshell
(custom-set-variables
 '(eshell-banner-message "")
 '(eshell-cmpl-cycle-completions nil)
 '(eshell-hist-ignoredups t)
 '(eshell-scroll-show-maximum-output nil))

(setq-default eshell-path-env (getenv "PATH"))
(global-set-key (kbd "C-\\") 'eshellutil-popup)
;;;; shellscript.el

;;;; sh-mode
;; @eval (find-file-read-only (locate-library "sh-script.el"))

(setq sh-basic-offset 4);; default: 2
(setq sh-indentation 4) ;; default: 4
(eval-after-load "sh-script"
  '(define-key sh-mode-map "\C-m" 'newline-and-indent))

(add-hook 'sh-mode-hook
          (lambda ()
            (setq tab-width 4)
            ))

(add-to-list 'auto-mode-alist '("/\\.\\(bash\\|zsh\\)rc.*$" . sh-mode))
;;;; slime.el



;;; Slime
(use-package sly
  :defer t
  :hook ((slime-mode-hook . set-up-slime-ac)
         (slime-repl-mode-hook . set-up-slime-ac))
  :config
  (require 'ac-slime nil t)
  ) ;; sly




;;; sly
(custom-set-variables
 '(sly-net-coding-system 'utf-8-unix))

(with-eval-after-load 'sly
  (setq inferior-lisp-program "sbcl")
  ;;(setq sly-protocol-version 'ignore)      ;; for clojure

  (defalias 'sly-cleanup 'sly-restart-inferior-lisp)

  ;; hyperspec
  (let ((sly-libdir (concat (file-name-directory (locate-library "sly")) "lib")))
    (add-to-list 'load-path sly-libdir))

  ;; ac-sly
  (add-hook 'sly-mode-hook 'set-up-sly-ac)
  (add-hook 'sly-mrepl-mode-hook 'set-up-sly-ac)

  ;; bindings
  (define-key sly-mode-map (kbd "C-M-i") 'auto-complete)
  (define-key sly-mode-map (kbd "C-c C-d C-a") 'helm-editutil-hyperspec)
  (define-key sly-mode-map (kbd "C-c C-d C-d") 'hyperspec-lookup))

(with-eval-after-load 'sly-mrepl
  (define-key sly-mrepl-mode-map (kbd "TAB") nil)
  (define-key sly-mrepl-mode-map (kbd "C-M-i") 'auto-complete))

(with-eval-after-load 'hyperspec
  (let ((hyperspec-dir (expand-file-name (concat user-emacs-directory "elisps/HyperSpec/"))))
    (setq common-lisp-hyperspec-root (concat "file://" hyperspec-dir)
          common-lisp-hyperspec-symbol-table (concat hyperspec-dir "Data/Map_Sym.txt"))))

(dolist (hook '(lisp-mode-hook))
  (add-hook hook 'sly-mode))

;;;;+ Extra
;; -> https://www.common-lisp.net/project/slime/doc/html/index.html
;; -> http://dev.ariel-networks.com/wp/archives/462
;;;;+
;;;; sql.el

;;; sql
(add-hook 'sql-mode-hook 'sql-highlight-mysql-keywords)

(defun my/sql-mode-hook ()
  (setq sql-indent-offset 2)
  (setq indent-tabs-mode nil)
  (sql-set-product "mysql"))
(add-hook 'sql-mode-hook 'my/sql-mode-hook)

;; sql-upcase
(when (require 'sql-upcase nil t)
  (add-hook 'sql-mode-hook 'sql-upcase-mode)
  (add-hook 'sql-interactive-mode-hook 'sql-upcase-mode)
  (custom-set-variables
   '(sql-upcase-boundary "[\t\n\r ();,]") ;; default: "[\t\n\r ();]"
   ))

;; sql-indent
(when (require 'sql-indent nil t)
  (add-hook 'sql-mode-hook 'sqlind-minor-mode))


;;; text-mode
(use-package text-mode
  :mode "/\\(?:LICENSE\\|Changes\\)\\'"
  :hook ((text-mode-hook . my/text-mode-hook)
         (text-mode-hook . turn-on-font-lock))
  :bind (:map text-mode-map
              ("C-M-i" . company-complete))
  :init
  (defun my/text-mode-hook ()
    (when (string-prefix-p "Changes" (buffer-name))
      (setq-local company-backends '(company-ispell company-files company-dabbrev))
      (flyspell-mode +1)))
  :config
  (remove-hook 'text-mode-hook 'auto-fill-mode-hook)
  ) ;; text-mode


;;;; typescript.el

;; setting for typescript

;; ;; https://blog.shibayu36.org/entry/2015/07/30/165626
;; (require 'typescript-mode)
;; (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

;; ;; https://github.com/ananthakumaran/tide
;; (defun setup-tide-mode ()
;;   (interactive)
;;   (tide-setup)
;;   (flycheck-mode +1)
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   (eldoc-mode +1)
;;   (tide-hl-identifier-mode +1)
;;   ;; company is an optional dependency. You have to
;;   ;; install it separately via package-install
;;   ;; `M-x package-install [ret] company`
;;   (company-mode +1))

;; ;; aligns annotation to the right hand side
;; (setq company-tooltip-align-annotations t)

;; ;; ;; formats the buffer before saving
;; ;; (add-hook 'before-save-hook 'tide-format-before-save)

;; (add-hook 'typescript-mode-hook #'setup-tide-mode)



;; ;;; web-mode
;; (use-package web-mode
;;   :ensure t
;;   :mode "\\.html?``'"
;;   :hook (web-mode-hook . my/web-mode-hook)
;;   :bind (:map web-mode-map
;;               ("C-c b b" . web-mode-block-beginning)
;;               ("C-c b e" . web-mode-block-end)
;;               ("C-c b k" . web-mode-block-kill)
;;               ("C-c b n" . web-mode-block-next)
;;               ("C-c b p" . web-mode-block-previous)
;;               ("C-c b s" . web-mode-block-select)
;;               ("C-c e b" . web-mode-element-beginning)
;;               ("C-c e c" . web-mode-element-clone)
;;               ("C-c e d" . web-mode-element-child)
;;               ("C-c e e" . web-mode-element-end)
;;               ("C-c e i" . web-mode-element-content-select)
;;               ("C-c e k" . web-mode-element-kill)
;;               ("C-c e n" . web-mode-element-next)
;;               ("C-c e p" . web-mode-element-previous)
;;               ("C-c e r" . web-mode-element-rename)
;;               ("C-c e s" . web-mode-element-select)
;;               ("C-c e t" . web-mode-element-traverse)
;;               ("C-c e u" . web-mode-element-parent)
;;               ("C-c t b" . web-mode-tag-beginning)
;;               ("C-c t e" . web-mode-tag-end)
;;               ("C-c t m" . web-mode-tag-match)
;;               ("C-c t n" . web-mode-tag-next)
;;               ("C-c t p" . web-mode-tag-previous)
;;               ("C-c t s" . web-mode-tag-select))
;;
;;   :init
;;   (setq web-mode-css-indent-offset 4)
;;   (setq web-mode-markup-indent-offset 2)
;;   ;; (setq web-mode-code-indent-offset 2)
;;   ;; (setq web-mode-html-offset   2)
;;   ;; (setq web-mode-css-offset    2)
;;   ;; (setq web-mode-script-offset 4)
;;   ;; (setq web-mode-php-offset    4)
;;   ;; (setq web-mode-java-offset   4)
;;   ;; (setq web-mode-asp-offset    4)
;;   ;; (setq indent-tabs-mode nil)
;;   ;; (setq tab-width 2)
;;   (setq web-mode-enable-auto-quoting nil)
;;   (setq web-mode-enable-whitespace-fontification t)
;;   (setq web-mode-display-table
;;         (let ((table (make-display-table)))
;;           ;;(aset table 9  (vector ?\xB7 ?\t)) ;tab
;;           (aset table 9  (vector ?\xBB ?\t)) ;tab
;;           ;; (aset table 10 (vector ?\;XXX: B6 ?\n)) ;line feed
;;           ;; (aset table 32 (vector ?\xB7))
;;           table))
;;
;;   (defun my/web-mode-hook ()
;;     (local-unset-key (kbd "C-c C-b"))
;;     (local-unset-key (kbd "C-c C-e"))
;;     (local-unset-key (kbd "C-c C-t")))
;;   ) ;; web-mode
;;
;;
;; ;;; emmet
;; (use-package emmet-mode
;;   :ensure t
;;   :defer t
;;   :hook ((sgml-mode-hook . emmet-mode)
;;          (html-mode-hook . emmet-mode)
;;          (web-mode-hook . emmet-mode))
;;   :init
;;   ;; Preview is disable as default
;;   (setq emmet-preview-default nil)
;;   (setq emmet-indentation 2)
;;   ) ;; emmet-mode
;;
