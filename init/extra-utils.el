;;; expand-region
;; -> https://github.com/magnars/expand-region.el/blob/master/README.md
;; -> http://d.hatena.ne.jp/syohex/20120117/1326814127
(use-package expand-region
  :ensure t
  :bind (("C-M-<SPC>" . er/expand-region)       ;; default: mark-sexp
         ("C-M-S-<SPC>" . er/contract-region)
         ;; (bind S-F11, S-F12 under terminal env.)
         ("S-<f11>" . er/expand-region)
         ("S-<f12>" . er/contract-region))
  ) ;; expand-region


;;; multiple-cursors
;; -> https://github.com/magnars/multiple-cursors.el/blob/master/README.md
(use-package multiple-cursors
  :ensure t
  :bind (("C-M-." . mc/mark-next-like-this) ;; find-tag-regexp
         ("C-M-," . mc/unmark-next-like-this)
         ("C-M->" . mc/skip-to-next-like-this)
         ("C-M-<" . mc/mark-all-like-this)
         ;; (bind S-F7, S-F8, S-F9, S-F10 under terminal env.)
         ("S-<f7>" . mc/mark-next-like-this)
         ("S-<f8>" . mc/unmark-next-like-this)
         ("S-<f9>" . mc/skip-to-next-like-this)
         ("S-<f10>" . mc/mark-all-like-this))
  ) ;; multiple-cursors


;;; helm
;; -> https://qiita.com/jabberwocky0139/items/86df1d3108e147c69e2c
(use-package helm
  :ensure t
  :bind (("M-x"     . helm-M-x)
         ("M-y"     . helm-show-kill-ring)
         ("C-M-z"   . helm-resume)
         ("C-x b"   . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("C-x C-r" . helm-recentf)
         ("C-x C-l" . helm-mini) ;; default: downcase-region
         ("C-c C-l" . helm-mini)
         ("C-c C-i" . helm-imenu) ;; default: ident-rigidly
         ("C-c o"   . helm-occur)
         ("C-c <SPC>" . helm-all-mark-rings)

         :map helm-map
         ("C-p"   . helm-previous-line)
         ("C-n"   . helm-next-line)
         ("C-M-n" . helm-next-source)
         ("C-M-p" . helm-previous-source)
         ("C-h"   . delete-backward-char)
         ("<tab>" . helm-execute-persistent-action)
         ("C-i"   . helm-execute-persistent-action)
         ("M-C-i" . helm-select-action))

  :init
  (setq helm-input-idle-delay 0)
  (setq helm-exit-idle-delay 0)
  (setq helm-candidate-number-limit 500)
  (setq helm-find-files-doc-header "")
  (setq helm-command-prefix-key "C-c h")
  (setq helm-mini-default-sources '(helm-source-recentf
                                    helm-source-bookmarks
                                    helm-source-buffers-list
                                    helm-source-buffer-not-found))
  :config
  (when (executable-find "curl")
    (setq url-retrieve-synchronously t))

  ;;; helm-files
  (use-package helm-files
    :bind (:map helm-find-files-map
                ("C-M-u" . helm-find-files-down-one-level)
                ("C-c C-o" . helm-ff-run-switch-other-window)
                ("C-h" . delete-backward-char))
    :config
    (remove-hook 'post-self-insert-hook 'helm-find-files--reset-level-tree)
    ) ;; helm-files

  ;;; helm for help
  (use-package help
    :bind (:map help-map
                ("a" . helm-apropos)            ;; default: apropos-command
                ("C-m" . helm-man-woman))       ;; default: view-order-manuals
    ) ;; help for heml

  (use-package org
    :bind (:map org-mode-map
                ("C-c h" . helm-org-in-buffer-headings))
    ) ;; org for helm

  ;; unset M-O to enable cursor keys on console
  (global-unset-key (kbd "M-O"))

  ) ;; helm


;;; helm-config
(use-package helm-config
  :after helm
  :bind-keymap ("C-c h" . helm-command-prefix)
  :bind (:map helm-command-map
              ("o" . helm-occur)
              ("<SPC>" . helm-all-mark-rings))
  ) ;; helm-config


;;; helm-descbinds
(use-package helm-descbinds
  :ensure t
  :after help
  :init
  (helm-descbinds-mode +1)
  ) ;; helm-descbinds


;;; helm-ag
(use-package helm-ag
  :ensure t
  :bind (("M-g ." . helm-do-ag)
         ("M-g ," . helm-ag-pop-stack)
         ("M-g /" . helm-ag-project-root))
  :init
  (setq helm-ag-insert-at-point 'symbol)

  ) ;; helm-ag


;; ;;; emacs-powerline
;; ;; -> https://github.com/jonathanchu/emacs-powerline
;; ;; -> http://blechmusik.hatenablog.jp/entry/2013/12/13/020823
;; ;; -> http://hico-horiuchi.hateblo.jp/entry/20130510/1368201907
;; (use-package powerline
;;   :ensure t
;;   :init
;;   (powerline-default-theme)
;;   :config
;;   (set-face-attribute 'mode-line nil
;;                       :background "dark slate blue"))
;;   ) ;; emacs-powerline


;;; tabbar
(use-package tabbar
  :ensure t
  :config
  ;; enable tabbar
  (call-interactively 'tabbar-mode t)

  ;; hide button
  (when (not window-system)
    (dolist (btn '(tabbar-buffer-home-button
                   tabbar-scroll-left-button
                   tabbar-scroll-right-button))
      (set btn (cons (cons "" nil) (cons "" nil)))))

  ;; disable mouse wheel to change tab (0：enable，-1: disable)
  (call-interactively 'tabbar-mwheel-mode -1)
  (remove-hook 'tabbar-mode-hook      'tabbar-mwheel-follow)
  (remove-hook 'mouse-wheel-mode-hook 'tabbar-mwheel-follow)

  ;; not use tab group (t: enable，nil: disable）
  (setq tabbar-buffer-groups-function nil)

  (with-eval-after-load "elscreen-buffer-group"
    ;; (setq tabbar-buffer-groups-function 'buffer-list)
    ;; (setq elscreen-buffer-group-exclusive t)
    )

  ;; tab separator width
  (setq tabbar-separator '(0.2))

  ;; tab changer
  (global-set-key (kbd "<C-tab>") 'tabbar-forward-tab)
  ;;(global-set-key (kbd "C-q")     'tabbar-backward-tab)

  ;; show `|' if console emacs
  (if (not window-system)
      (setq tabbar-separator-value "|"))

  ;; color (see also custom theme-setting.el)
  (setq tabbar-background-color "gray50")

  ;; hide `*...*' buffers
  ;; -> http://ser1zw.hatenablog.com/entry/2012/12/31/022359

  (defvar my/tabbar-invisible-buffers '("TAGS" "GTAGS" "GRTAGS" "GPATH"))
  (defvar my/tabbar-invisible-buffers-regexp '("^magit\\(-.*\\)?:"
                                               "^CAPTURE\\(-\\d\\)?-\\*notes\\*"
                                               "\\(business\\|private\\|inbox\\|routine\\)\\.org"
                                               "_\\(LOCAL\\|REMOTE\\|BASE\\)_\\d\\{5\\}"))
  (defvar my/tabbar-visible-*-buffers '("*scratch*" "*Messages*" "*notes*" "*Help*"))
  (defun my/tabbar-buffer-list ()
    (interactive)
    (delq nil
          (mapcar #'(lambda (buf)
                      (cond
                       ((seq-find (lambda (name) (string-match (format "^%s\\(<.*>\\)?$" name) (buffer-name buf))) my/tabbar-invisible-buffers) nil)
                       ((seq-find (lambda (regexp) (string-match regexp (buffer-name buf))) my/tabbar-invisible-buffers-regexp) nil)
                       ((member (buffer-name buf) my/tabbar-visible-*-buffers) buf)
                       ((eq (current-buffer) buf) buf)
                       ((char-equal ?* (aref (buffer-name buf) 0)) nil)
                       ((char-equal ?\  (aref (buffer-name buf) 0)) nil)
                       ((buffer-file-name buf) buf)
                       ((buffer-live-p buf) buf)))
                  (buffer-list))))
  (setq tabbar-buffer-list-function 'my/tabbar-buffer-list)

  ;; enable tabbar on persp-mode
  (with-eval-after-load "persp-mode"
    (setq persp-buffer-list-function 'my/tabbar-buffer-list))

  ) ;; tabbar


;;; elscreen
(use-package elscreen
  :ensure t
  :bind (("C-M-t" . elscreen-editutil-clone-only-this-window) ;; default: transpose-sexps
         ("M-z" . elscreen-next) ;; default: zap-to-char
         ("M-t" . elscreen-toggle) ;; default: transpose-words
         ("M-Z" . elscreen-previous)
         ("C-z C-l" . helm-editutil-elscreen))
  :init
  (setq elscreen-prefix-key (kbd "C-z C-z"))
  ;; (setq elscreen-display-screen-number nil)
  (setq elscreen-display-tab nil)
  (setq elscreen-tab-display-kill-screen nil)
  (setq elscreen-mode-to-nickname-alist
        '(("^dired-mode$" . (lambda () (format "Dired(%s/)" (buffer-name))))
          ("^Info-mode$" . (lambda ()
                             (format "Info(%s)" (file-name-nondirectory Info-current-file))))))
  (setq elscreen-buffer-to-nickname-alist
        '(("Minibuf". "")))

  (with-eval-after-load "projectile"
    ;; Change elscreen-prefix "C-z" to "C-z C-z"
    (require 'elscreen)
    (define-key projectile-command-map (kbd "C-z") elscreen-map))

  :config
  (elscreen-start)

  (with-eval-after-load "projectile"
    ;; Change elscreen-prefix "C-z" to "C-z C-z"
    ;;(define-key projectile-command-map (kbd "C-z") elscreen-map)
    (define-key elscreen-map (kbd "C-z") 'elscreen-toggle)
    (define-key elscreen-map (kbd ",") 'elscreen-screen-nickname)
    (define-key elscreen-map (kbd "C") 'elscreen-clone))

  ;; helm-editutil-elscreen
  ;; -> https://github.com/syohex/emacs-editutil/blob/master/helm-editutil.el
  (defun helm-editutil--elscreen-candidates ()
    (cl-loop with sort-func = (lambda (a b) (< (car a) (car b)))
             with screen-list = (cl-copy-list (elscreen-get-screen-to-name-alist))
             with remove-regexp = (format ":?%s:?" (regexp-quote "*helm-elscreen*"))
             for (index . screen-name) in (sort screen-list sort-func)
             collect
             (let ((name (replace-regexp-in-string remove-regexp "" screen-name)))
               (cons (format "[%d] %s" index name) index))))

  (defun helm-editutil--elscreen-kill-screens (_candidate)
    (dolist (screen (helm-marked-candidates))
      (elscreen-goto screen)
      (elscreen-kill)))

  (defvar helm-editutil-source-elscreen
    (helm-build-in-buffer-source "Elscreen"
      :candidates #'helm-editutil--elscreen-candidates
      :action (helm-make-actions
               "Change screen" #'elscreen-goto
               "Kill screen" #'helm-editutil--elscreen-kill-screens)))

  (defun helm-editutil-elscreen ()
    (interactive)
    (require 'elscreen)
    (helm :sources '(helm-editutil-source-elscreen) :buffer "*helm-elscreen*"))


  ;; elscreen-editutil-clone-only-this-window
  ;; -> https://github.com/syohex/emacs-editutil/blob/master/elscreen-editutil.el
  (defun elscreen-editutil-clone-only-this-window ()
    (interactive)
    (call-interactively 'elscreen-clone)
    (delete-other-windows))

  ) ;; elscreen


;;; popwin
(use-package popwin
  :ensure t
  ;;:bind-keymap ("M-g w" . popwin:keymap)
  ;;:bind ("M-g w i" . import-popwin)
  ;; ;; helm binding
  ;; ;; (global-set-key (kbd "C-h l")  'popwin:popup-last-buffer)
  ;; ;; (global-set-key (kbd "C-h SPC") 'popwin:select-popup-window)

  :config
  (popwin-mode 1)

  ;; remove from default config
  (dolist (stuff '("*vc-diff*" "*vc-change-log*"))
    (delete stuff popwin:special-display-config))

  ;; basic
  (push '("*Help*" :stick t :noselect t) popwin:special-display-config)
  (push '("*sdic*") popwin:special-display-config)

  ;; Ruby
  (push '("*ri-doc*" :stick t :height 20) popwin:special-display-config)
  (push '(inf-ruby-mode :stick t :height 20) popwin:special-display-config)

  ;; python
  (push '("*Python*"   :stick t) popwin:special-display-config)
  (push '("*Python Help*" :stick t :height 20) popwin:special-display-config)
  (push '("*jedi:doc*" :stick t :noselect t) popwin:special-display-config)

  ;; Go
  (push '("^\*go-direx:" :position left :width 0.3 :dedicated t :stick t :regexp t)
        popwin:special-display-config)

  ;; flycheck
  (push '(flycheck-error-list-mode :stick t) popwin:special-display-config)

  ;; CoffeeScript
  (push '("*CoffeeREPL*" :stick t) popwin:special-display-config)

  ;; Clojure
  (push '(cider-repl-mode :stick t) popwin:special-display-config)

  ;; Slime
  ;;   Apropos
  (push '("*slime-apropos*") popwin:special-display-config)
  ;;   Macroexpand
  (push '("*slime-macroexpansion*") popwin:special-display-config)
  ;;   Help
  (push '("*slime-description*") popwin:special-display-config)
  ;;   Compilation
  (push '("*slime-compilation*" :noselect t) popwin:special-display-config)
  ;;   Cross-reference
  (push '("*slime-xref*") popwin:special-display-config)
  ;;   Debugger
  (push '(sldb-mode :stick t) popwin:special-display-config)
  ;;   REPL
  (push '(slime-repl-mode) popwin:special-display-config)
  ;;   Connections
  (push '(slime-connection-list-mode) popwin:special-display-config)

  ;; Direx
  (push '(direx:direx-mode :position left :width 40 :dedicated t) popwin:special-display-config)

  ) ;; popwin


;;;; direx
;; -> https://github.com/m2ym/direx-el
(use-package direx
  :ensure t
  :bind (("C-x C-j" . my/dired-jump)
         :map direx:direx-mode-map
         ("C-h" . direx:up-item)
         ("C-g" . quit-window))

  :init
  ;; http://syohex.hatenablog.com/entry/20130202/1359814263
  (defun my/dired-jump ()
    (interactive)
    (cond (current-prefix-arg
           (dired-jump))
          ((not (one-window-p)
                ;; (< (window-width) 100)
                )
           (or (ignore-errors
                 (direx-project:jump-to-project-root) t)
               (direx:jump-to-directory)))
          (t
           (or (ignore-errors
                 (direx-project:jump-to-project-root-other-window) t)
               (direx:jump-to-directory-other-window)))))

  :config
  ;; Disable helm-mode on direx:do-copy-files
  (defadvice direx:do-copy-files
      (around direx:do-copy-files-default activate)
    (let ((helm-mode-p helm-mode))
      (if helm-mode-p (helm-mode -1))
      ad-do-it
      (if helm-mode-p (helm-mode 1))))

  ) ;; direx


;;; anzu
(use-package anzu
  :ensure t
  :bind (("M-%" . anzu-query-replace)
         ("ESC M-%" . anzu-query-replace-at-cursor)
         ("C-x %" . anzu-replace-at-cursor-thing)
         ("M-'" . avy-goto-word-1) ;; default: abbrev-prefix-mark
         :map isearch-mode-map
              ([remap isearch-query-replace] . anzu-isearch-query-replace)
              ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :init
  (setq anzu-mode-lighter "")
  (setq anzu-deactivate-region t)
  (setq anzu-search-threshold 1000)
  (setq anzu-replace-threshold 50)
  (setq anzu-replace-to-string-separator " => ")
  :config
  (global-anzu-mode +1)
  ) ;; anzu


;;; avy
(use-package avy
  :ensure t
  :bind (:map isearch-mode-map
              ("M-a" . avy-isearch))
  :init
  (setq avy-case-fold-search nil)
  ) ;; avy


;;; buffer-move
(use-package buffer-move
  :ensure t
  :bind (("M-g h" . buf-move-left)
         ("M-g j" . buf-move-down)
         ("M-g k" . buf-move-up)
         ("M-g l" . buf-move-right))
  ) ;; buffer-move


;;; smooth-scroll
;; -> https://github.com/k-talo/smooth-scroll.el
;; -> https://www.emacswiki.org/emacs/SmoothScrolling
;; -> https://qiita.com/ShingoFukuyama/items/429199542c38625c5554
(use-package smooth-scroll
  :defer t
  :init
  (setq smooth-scroll/vscroll-step-size 4) ;; default: 2
  :config
  (smooth-scroll-mode t)
  ) ;; smooth-scroll


;; ;;; yascroll
;; (use-package yascroll
;;   :config
;;   (global-yascroll-bar-mode 1)
;;   ;; (custom-set-variables
;;   ;;  '(yascroll:delay-to-hide nil))
;;   ) ;; yascroll


;;; which-key
(use-package which-key
  :ensure t
  :init
  (setq which-key-lighter "")
  (setq which-key-idle-delay 0.5)
  (which-key-mode +1)
  ) ;; which-key


;;; smartrep
(use-package smartrep
  :defer t
  :ensure t
  :config
  (setq smartrep-mode-line-active-bg nil)
  (setq smartrep-mode-line-string-activated "<<< SmartRep >>>")
  ) ;; smartrep


;;; undo-tree
(use-package undo-tree
  :ensure t
  :bind (("C-S-/" . undo-tree-redo) ;; C-S-/ => redo
         ("C-M-_" . undo-tree-redo) ;; C-M-_ => redo (Esc C-/ on console)
         :map undo-tree-map
         ("C-/" . undo-tree-undo)
         ("M-_" . nil))
  :init
  (setq undo-tree-mode-lighter nil) ;; delete minor-mode in mode-line
  :config
  (global-undo-tree-mode)
  ) ;; undo-tree


;;; hippie-expand
;; http://emacs.rubikitch.com/sd1409-migemo-ace-jump-mode-dabbrev/
(use-package hippie-expand
  :defer t
  :init
  (setq hippie-expand-verbose nil)
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-complete-file-name
          try-complete-file-name-partially
          try-expand-dabbrev-all-buffers))
  ) ;; hippie-expand


;;; text-adjust:
;; -> http://www.taiyaki.org/elisp/text-adjust/
;; -> http://d.hatena.ne.jp/rubikitch/20090220/text_adjust
;;      http://www.rubyist.net/~rubikitch/archive/mell.el
;;      http://www.rubyist.net/~rubikitch/archive/text-adjust.el
(use-package text-adjust
  :bind ("C-x M-q" . text-adjust-space)
  :init
  ;; full-width character to ignore
  (setq text-adjust-hankaku-except "　？！＠ー〜、，。．")
  ;; concider margin on text-adjust-fill-region
  (setq adaptive-fill-regexp "[ \t]*")
  (setq adaptive-fill-mode t)
  ;; change rule for TeX
  (setq text-adjust-rule-space
        '((("\\cK\\|\\cC\\|\\cH" "" "[[(0-9a-zA-Z+$]")   " ")
          (("[])/!?0-9a-zA-Z+$]" "" "\\cK\\|\\cC\\|\\cH") " ")))
  ) ;; text-adjust

;;; gist
;; -> https://github.com/defunkt/gist.el
;; git config --global github.user <your-github-user-name>
;; git config --global github.oauth-token <your-personal-access-token-with-gist-scope>
(use-package gist
  :defer t
  :ensure t
  ) ;; gist

;; multi-files gist support (indicated by a '+' in the gist list)
;; improved gist-list buffer, based on tabulated-list.el (same codebase as package.el) New keybindings:
;; g : reload the gist list from server
;; e : edit current gist description
;; k : delete current gist
;; + : add a file to the current gist
;; - : remove a file from the current gist
;; y : print current gist url
;; b : browse current gist
;; * : star gist
;; ^ : unstar gist
;; f : fork gist
;; in-place edition. While viewing a gist file buffer, you can:
;; C-x C-s : save a new version of the gist
;; C-x C-w : rename some file
;; dired integration. From a dired buffer, you can:
;; @ : make a gist out of marked files (with a prefix, make it private)


;;; paredit
;; -> http://www.daregada.sakuraweb.com/paredit_tutorial_ja.html
(use-package paredit
  :ensure t
  :bind (:map paredit-mode-map
              ("C-c C-q" . paredit-reindent-defun)
              ("C-c C-j" . eval-print-last-sexp)
              ("M-q" .  nil)
              ("M-)" . move-past-close-and-reindent)

              ("M-P" . paredit-splice-sexp-killing-backward)
              ("M-p" . my/paredit-add-to-previous-list)
              ("M-n" . my/paredit-forward-barf-sexp)
              )
  :init
  ;; (dolist (hook '(emacs-lisp-mode-hook
  ;;                 lisp-mode-hook
  ;;                 ielm-mode-hook
  ;;                 scheme-mode-hook
  ;;                 inferior-scheme-mode-hook
  ;;                 clojure-mode-hook
  ;;                 cider-repl-mode-hook
  ;;                 sly-mrepl-mode-hook))
  ;;   (add-hook hook 'enable-paredit-mode))

  :config
  (defun my/paredit-add-to-previous-list ()
    (interactive)
    (back-to-indentation)
    (paredit-add-to-previous-list)
    (back-to-indentation))

  (defun my/paredit-forward-barf-sexp ()
    (interactive)
    (back-to-indentation)
    (paredit-forward-barf-sexp)
    (back-to-indentation))

  ) ;; paredit

;; C-), C-<right>   : paredit-forward-slurp-sexp
;; C-}, C-<left>    : paredit-forward-barf-sexp
;; M-(              : paredit-wrap-round
;; M-s              : paredit-splice-sexp
;; M-<up>           : paredit-splice-sexp-killing-backward
;; M-r              : paredit-raise-sexp

;;;;;
;;  C-d         paredit-forward-delete
;;  C-j         paredit-newline
;;  C-k         paredit-kill
;;  "           paredit-doublequote
;;  (           paredit-open-round
;;  )           paredit-close-round
;;  ;           paredit-semicolon
;;  [           paredit-open-square
;;  \           paredit-backslash
;;  ]           paredit-close-square
;;  DEL         editutil-paredit-backward-delete
;;  C-(         paredit-backward-slurp-sexp
;;  C-)         paredit-forward-slurp-sexp
;;  C-{         paredit-backward-barf-sexp
;;  C-}         paredit-forward-barf-sexp
;;  <C-M-left>  paredit-backward-slurp-sexp
;;  <C-M-right> paredit-backward-barf-sexp
;;  <C-left>    paredit-forward-barf-sexp
;;  <C-right>   paredit-forward-slurp-sexp
;;  <M-down>    paredit-splice-sexp-killing-forward
;;  <M-up>      paredit-splice-sexp-killing-backward
;;  <delete>    paredit-forward-delete
;;  <deletechar>    paredit-forward-delete
;;  C-c C-q     paredit-reindent-defun
;;  C-M-b       paredit-backward
;;  C-M-d       paredit-forward-down
;;  C-M-f       paredit-forward
;;  C-M-n       paredit-forward-up
;;  C-M-p       paredit-backward-down
;;  C-M-u       paredit-backward-up
;;  M-"         paredit-meta-doublequote
;;  M-(         paredit-wrap-round
;;  M-;         paredit-comment-dwim
;;  M-?         paredit-convolute-sexp
;;  M-J         paredit-join-sexps
;;  M-S         paredit-split-sexp
;;  M-d         paredit-forward-kill-word
;;  M-r         paredit-raise-sexp
;;  M-s         paredit-splice-sexp
;;  M-DEL       paredit-backward-kill-word
;;  ESC <C-left>    paredit-backward-slurp-sexp
;;  ESC <C-right>   paredit-backward-barf-sexp
;;  ESC <down>  paredit-splice-sexp-killing-forward
;;  ESC <up>    paredit-splice-sexp-killing-backward
;;  C-c C-M-l   paredit-recenter-on-sexp
