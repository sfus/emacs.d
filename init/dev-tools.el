;;; eldoc-mode
(use-package eldoc-mode
  :defer t
  :hook (emacs-lisp-mode-hook ielm-mode-hook)
  :init
  (setq eldoc-idle-delay 0.5)
  (setq eldoc-minor-mode-string "")
  ) ;; eldoc-mode


;;; ediff
;; -> https://www.gnu.org/software/emacs/manual/html_mono/ediff.html
(use-package ediff
  :bind (("C-x D" . ediff-buffers))
  :init
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-diff-options "-twB")

  ;; `Q' => quit and return terminal
  (defun my/ediff-quit-and-kill-terminal (&optional arg)
    (interactive "P")
    (ediff-barf-if-not-control-buffer)
    (let ((ctl-buf (current-buffer))
          (ctl-frm (selected-frame)))
      (set-buffer ctl-buf)
      (ediff-really-quit nil)
      (select-frame ctl-frm)
      (raise-frame ctl-frm))
    (save-buffers-kill-terminal arg))

  ;; `c' / `C' => includes both a/b diffs
  ;; -> https://emacs.stackexchange.com/questions/19339/how-to-use-both-variants-in-ediff
  (defun my/ediff-copy-both-to-C ()
    (interactive)
    (ediff-copy-diff ediff-current-difference nil 'C nil
                     (concat
                      (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                      (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
  (defun my/ediff-copy-both-to-C-reverse ()
    (interactive)
    (ediff-copy-diff ediff-current-difference nil 'C nil
                     (concat
                      (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer)
                      (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer))))

  ;; keymap setup
  (defun my/ediff-mode-keymap-setup ()
    (define-key ediff-mode-map (kbd "Q") 'my/ediff-quit-and-kill-terminal)
    (define-key ediff-mode-map (kbd "c") 'my/ediff-copy-both-to-C)
    (define-key ediff-mode-map (kbd "C") 'my/ediff-copy-both-to-C-reverse))
  (add-hook 'ediff-keymap-setup-hook #'my/ediff-mode-keymap-setup)

 ) ;; ediff


;;; smerge-mode
;; -> http://dev.ariel-networks.com/articles/emacs/part7/
;; -> https://emacs.stackexchange.com/questions/16469/how-to-merge-git-conflicts-in-emacs
(use-package smerge-mode
  :init
  ;; Prefix key
  (setq smerge-command-prefix (kbd "C-c v")) ;; default: C-c ^
  ;; C-c ^ n      smerge-next
  ;; C-c ^ p      smerge-prev
  ;; C-c ^ R      smerge-refine
  ;; C-c ^ m      smerge-keep-mine
  ;; C-c ^ o      smerge-keep-other
  ;; C-c ^ a      smerge-keep-all
  ;; C-c ^ b      smerge-keep-base
  ;; C-c ^ E      smerge-ediff

  ;; ;; whether leave resolved conflict position or not
  ;; (setq smerge-auto-leave nil) ;; default: t
  ) ;; smerge-mode


;;; gdb
;; -> https://www.gnu.org/software/emacs/manual/html_node/emacs/Commands-of-GUD.html
;; -> http://d.hatena.ne.jp/higepon/20090505/p1
;; -> http://murakan.cocolog-nifty.com/blog/2009/01/gud-on-emacs23-.html
;; -> https://gist.github.com/chokkan/5693497
(use-package gud
  :bind (("M-g M-d" . gdb)
         ;; ("M-t" . gud-tbreak) ;; default C-x C-a C-t
         :map gud-minor-mode-map
         ;; like chrome key bindings
         ("<f8>" . gud-cont)        ;; resume
         ("<f10>" . gud-next)       ;; step over
         ("<f11>" . gud-step)       ;; step in
         ("<S-f11>" . gud-finish)   ;; step out
         ("M-\\" . gud-cont)
         ("M-'" . gud-next)
         ("M-;" . gud-step)
         ("M-:" . gud-finish)

         ;; other bindings
         ("C-x <SPC>" . gud-break) ;; revert default binding
         ("M-C-x" . gud-break)
         ("M-C-d" . gud-remove)
         ("M-C-t" . gud-tbreak)
         ("C-c C-c" . gud-run))
  :init
  ;; ;; Change prefix key
  ;;(setq gud-key-prefix "\C-z\C-a") ;; default: "\C-x\C-a"
  (setq gdb-many-windows t)
  ;; https://emacs.stackexchange.com/questions/23953/how-can-i-prevent-gdb-input-output-buffer-from-aggressively-popping-up-in-fram
  (setq gdb-display-io-nopopup t)
  (setq gdb-use-separate-io-buffer nil)
  (setq gud-tooltip-echo-area nil)

  :config
  (with-eval-after-load "elscreen"
    (define-key elscreen-map (kbd "C-d") 'gdb-restore-windows))  ;; C-z C-z C-d
  (gud-tooltip-mode t)
  (gud-def my/gud-break-main "break main" nil "Set breakpoint at main.")

  (defun my/gud-set-clear-breakpoint ()
    (interactive)
    (if (or (buffer-file-name) (eq major-mode 'gdb-assembler-mode))
        (if (or
             (let ((start (- (line-beginning-position) 1))
                   (end (+ (line-end-position) 1)))
               (catch 'breakpoint
                 (dolist (overlay (overlays-in start end))
                   (if (overlay-get overlay 'put-break)
                       (throw 'breakpoint t)))))
             (eq (car (fringe-bitmaps-at-pos)) 'breakpoint))
            (gud-remove nil)
          (gud-break nil))))

  (defun my/gud-kill ()
    "Kill gdb process."
    (interactive)
    (with-current-buffer gud-comint-buffer (comint-skip-input))
    (kill-process (get-buffer-process gud-comint-buffer)))

  ) ;; gud

;; Mac Setting
;; <http://qiita.com/ymotongpoo/items/81d3c945483cae734122>
;; <http://efcl.info/2014/08/29/golang-liteide-debugger/>
;; <http://qiita.com/takahashim/items/204ffa698afe09bd4e28>
;;
;; $ brew install https://raw.github.com/Homebrew/homebrew-dupes/master/gdb.rb
;; $ open -a "Keychain Access.app"
;;   「証明書アシスタント」「証明を作成」を選択し、例えば gdb-cert と名前を付ける。
;;   「自己署名ルート」「コード署名」を選択し、「デフォルトを無効化」にチェックを入れる。
;;   「キーチェーン」は「システム」を選択。
;;    作成された証明書をダブルクリックで開いて、「信頼」を開き、コード署名のところを「常に信頼」に設定。
;; $ sudo vi /System/Library/LaunchDaemons/com.apple.taskgated.plist
;;     <key>ProgramArguments</key>
;;     <array>
;;         <string>/usr/libexec/taskgated</string>
;; -       <string>-s</string>
;; +       <string>-sp</string>
;;     </array>
;; $ sudo codesign -s gdb-cert /usr/local/bin/gdb
;; $ go build -gcflags "-N -l" foo.go


;;; git-commit (included by magit)
(use-package git-commit
  :ensure t
  :init
  (defun my/git-commit-commit-after (_unused)
    (delete-window))
  (defun my/git-commit-mode-hook ()
    (setq-local company-backends '(company-ispell company-files company-dabbrev))
    (flyspell-mode +1)
    )

  (add-hook 'git-commit-mode-hook 'my/git-commit-mode-hook)
  (advice-add 'git-commit-commit :after 'my/git-commit-commit-after)

  :config
  (global-git-commit-mode t)
  ) ;; git-commit


;;; magit
(use-package magit
  :ensure t
  :bind ("M-g M-g" . magit-status)
  :init
  (setq git-commit-fill-column 80)
  (setq git-commit-summary-max-length 72)
  (setq magit-auto-revert-mode-lighter "")
  (setq magit-push-always-verify nil)
  (setq git-commit-summary-max-length 72) ;; default: 50
  (setq git-commit-fill-column 120)       ;; default: 72

  ;; -> https://magit.vc/manual/magit/Automatic-Reverting-of-File_002dVisiting-Buffers.html
  ;; (setq magit-auto-revert-mode nil)     ;; default: t

  :config
  ;; (advice-add 'magit-status :around 'my/magit-status-around)
  ;; (define-key magit-status-mode-map (kbd "q") 'my/magit-quit-session)

  ;; allways require for emacsclient $GIT_EDITOR call
  ;; restrict to open diff window on magit-commit
  (remove-hook 'server-switch-hook 'magit-commit-diff)

  (defun my/magit-status-around (orig-fn &rest args)
    (window-configuration-to-register :magit-fullscreen)
    (apply orig-fn args)
    (delete-other-windows))

  (defun my/magit-quit-session ()
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen)
    (git-gutter:update-all-windows))

  ) ;; magit

;;   C-c C-c  Finish the editing session successfully by returning
;;            with exit code 0.  Git then creates the commit using
;;            the message it finds in the file.
;;
;;   C-c C-k  Aborts the edit editing session by returning with exit
;;            code 1.  Git then aborts the commit.

;;   M-p      Replace the buffer contents with the previous message
;;            from the message ring.  Of course only after storing
;;            the current content there too.
;;
;;   M-n      Replace the buffer contents with the next message from
;;            the message ring, after storing the current content.

;;   C-c C-s  Insert a Signed-off-by header.
;;   C-C C-a  Insert a Acked-by header.
;;   C-c C-t  Insert a Tested-by header.
;;   C-c C-r  Insert a Reviewed-by header.
;;   C-c C-o  Insert a Cc header.
;;   C-c C-p  Insert a Reported-by header.
;;   C-c M-s  Insert a Suggested-by header.


;;; view-mode
(use-package view
  :bind (:map view-mode-map
              ("N" . View-search-last-regexp-backward)
              ("?" . View-search-regexp-backward)
              ("g" . View-goto-line)
              ("w" . forward-word)
              ("W" . forward-symbol)
              ("b" . backward-word)
              ("h" . backward-char)
              ("j" . next-line)
              ("k" . previous-line)
              ("l" . forward-char)
              ("[" . backward-paragraph)
              ("]" . forward-paragraph))
  :init
  (setq view-read-only t)
  (setq view-inhibit-help-message t)
  :config
  ;; empphasize view-mode string on mode-line
  (setcar (cdr (assq 'view-mode minor-mode-alist))
          (if (fboundp 'propertize)
              (list (propertize " View"
                                'face '(:foreground "white" :background "DeepPink1")))
            " View")))


;;; doc-view
(use-package doc-view
  :bind (:map doc-view-mode-map
              ("j" . doc-view-next-line-or-next-page)
              ("k" . doc-view-previous-line-or-previous-page))
  ) ;; doc-view


;;; auto-complete
(use-package auto-complete
  :ensure t
  :bind (("C-M-i" . auto-complete)
         :map ac-complete-mode-map
         ("C-n" . ac-next)
         ("C-p" . ac-previous)
         ("C-s" . ac-isearch)
         :map ac-completing-map
         ("<tab>" . ac-complete)
         ("C-i" . ac-complete)
         :map lisp-interaction-mode-map
         ("C-M-i" . auto-complete)
         :map emacs-lisp-mode-map
         ("C-M-i" . auto-complete))

  :init
  (setq ac-dictionary-directories (concat user-emacs-directory "ac-dict"))
  (setq ac-use-fuzzy t)
  (setq ac-auto-start nil)
  (setq ac-use-menu-map t)
  (setq ac-quick-help-delay 1.0)

  :config
  ;; setting of auto-complete
  (ac-config-default)
  ;;(remove-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)

  ;; Enable auto-complete mode other than default enable modes
  (dolist (mode '(git-commit-mode
                  coffee-mode
                  go-mode
                  cider-repl-mode
                  markdown-mode
                  fundamental-mode
                  org-mode
                  text-mode
                  slime-repl-mode
                  ))
    (add-to-list 'ac-modes mode))
 ) ;; auto-complete


;;; company
(use-package company
  :ensure t
  :bind (("C-M-i" . company-complete)
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-s" . company-filter-candidates)
         ("C-i" . company-complete-selection)
         :map lisp-interaction-mode-map
         ("C-M-i" . company-elisp)
         :map emacs-lisp-mode-map
         ("C-M-i" . company-complete)
         :map company-search-map
         ("C-n" . 'company-select-next)
         ("C-p" . 'company-select-previous))

  :init
  (setq company-selection-wrap-around t)
  (setq company-idle-delay nil)

  :custom
  (global-company-mode +1)

  ;; suppress minibuffer message
  (fset 'company-echo-show #'ignore)
  ) ;; company


;;; projectile
(use-package projectile
  :ensure t
  :bind-keymap* ("C-z" . projectile-command-map)
  :bind (:map projectile-command-map
              ("C-f" . projectile-find-file-dwim)
              ("C-r" . projectile-recentf))
  :init
  ;;(global-unset-key (kbd "C-z"))
  ;;(setq projectile-keymap-prefix (kbd "C-z")) ;; default: C-c p
  (setq projectile-mode-line '(:eval (format " Pj[%s]" (projectile-project-name))))
  (setq projectile-switch-project-action 'projectile-dired)

  :config
  (projectile-mode)

  ;; gtags
  (when (executable-find "gtags")
    (setq projectile-tags-file-name "GTAGS")
    (setq projectile-tags-command "gtags"))

  (use-package helm
    :init
    (setq projectile-completion-system 'helm))

  (use-package helm-projectile
    :defer t
    :ensure t
    :bind (:map projectile-command-map
                ;;("C-h" . helm-projectile)
                )
    :config
    (helm-projectile-on)
    ;;(setq projectile-switch-project-action 'helm-projectile)
    )
  ) ;; projectile


;;; helm-gtags
(use-package helm-gtags
  :ensure t
  ;; :hook ((c-mode-common-hook . helm-gtags-mode)
  ;;        (java-mode-hook . helm-gtags-mode)
  ;;        (asm-mode-hook . helm-gtags-mode))
  :bind (:map helm-gtags-mode-map
              ("M-t" . helm-gtags-find-tag)
              ("M-r" . helm-gtags-find-rtag)
              ("M-s" . helm-gtags-find-symbol)
              ("M-g M-s" . helm-gtags-select)
              ("C-c >" . helm-gtags-next-history)
              ("C-c <" . helm-gtags-previous-history)
              ("C-t" . helm-gtags-pop-stack))
  :init
  (setq helm-gtags-pulse-at-cursor nil)
  ;;   (setq helm-gtags-direct-helm-completing t)    ;; not read in minibuffer
  ;;   (setq helm-gtags-use-input-at-cursor t)
  ;;   (setq helm-gtags-suggested-key-mapping t)
  ;;   (setq helm-gtags-pulse-at-cursor t)           ;; revert to default
  ;;   (setq helm-gtags-auto-update t)
  ;;   (define-key helm-gtags-mode-map (kbd "M-T") 'helm-gtags-pop-stack)
  ;;   (define-key helm-gtags-mode-map (kbd "C-c R") 'helm-gtags-create-tags)
  ) ;; helm-gtags


;;; ggtags
;; -> https://github.com/leoliu/ggtags
;; -> https://qiita.com/yoshizow/items/9cc0236ac0249e0638ff
;; -> http://udzura.hatenablog.jp/entry/2016/07/21/142628
(use-package ggtags
  :ensure t
  :hook ((c-mode-common-hook . my/gtags-mode-hook)
         (java-mode-hook . my/gtags-mode-hook)
         (asm-mode-hook . my/gtags-mode-hook)
         (cperl-mode-hook . my/gtags-mode-hook)
         (go-mode-hook . my/gtags-mode-hook))

  :bind (:map ggtags-navigation-map
              ("C-g" . ggtags-navigation-mode-done))
  :init
  ;; ;; not create tags files on save (default: t)
  ;; (setq ggtags-update-on-save nil)
  ;; to use helm
  (setq ggtags-completing-read-function nil)
  ;; projectile
  (setq projectile-tags-backend 'ggtags)

  :config
  (defun my/gtags-mode-hook ()
    ;; choose ggtags-mode or helm-gtags-mode
    (ggtags-mode 1)
    ;;(helm-gtags-mode 1)
    (add-to-list 'ac-sources 'ac-source-gtags))

  ) ;; ggtags


;;; compile
(use-package compile
  :bind ("M-g r" . recompile)
  ) ;; compile


;;; quickrun
(use-package quickrun
  :ensure t
  :bind (("M-g q" . quickrun)
         :map quickrun--mode-map
         ("C-g" . quit-window))
  ) ;; quickrun


;;; flycheck
(use-package flycheck
  :ensure t
  :hook (after-init-hook . global-flycheck-mode)
  :bind (("M-g M-l" . flycheck-list-errors)
         :map flycheck-command-map
              ("M-g M-n" . flycheck-next-error)
              ("M-g M-p" . flycheck-previous-error)
              ("M-g M-l" . flycheck-list-errors))
  :init
  (setq flycheck-keymap-prefix (kbd "C-c 1")) ;; default: "C-c !"
  (setq flycheck-checker-error-threshold 999) ;; default: 400
  (setq flycheck-display-errors-delay 0.5)
  (setq flycheck-idle-change-delay 1.0)
  ;;(setq flycheck-display-errors-function nil)

  ;; To find load-path, use user's load-path instead of `Emacs -Q'
  ;; https://emacs.stackexchange.com/questions/10244/flycheck-could-not-find-file-under-load-path
  (setq flycheck-emacs-lisp-load-path 'inherit)

  ;;; flycheck-color-mode-line
  ;; -> https://github.com/flycheck/flycheck-color-mode-line
  (use-package flycheck-color-mode-line
    :defer t
    :ensure t
    :hook (flycheck-mode-hook . flycheck-color-mode-line-mode)
    :init
    (setq flycheck-color-mode-line-error-face '((t (:background "maroon"))))
    (setq flycheck-color-mode-line-info-face '((t nil)))
    (setq flycheck-color-mode-line-running-face '((t nil)))
    (setq flycheck-color-mode-line-warning-face '((t nil)))
    ) ;; flycheck-color-mode-line
  ) ;; flycheck


;;; yasnippet
;; -> https://github.com/joaotavora/yasnippet/blob/master/yasnippet.el
(use-package yasnippet
  :ensure t
  :bind ("M-=" . yas-insert-snippet) ;; default: count-words-region
  :hook ((c-mode-hook . yas-minor-mode)
         (c++-mode-hook . yas-minor-mode)
         (java-mode-hook . yas-minor-mode)
         (cperl-mode-hook . yas-minor-mode)
         (emacs-lisp-mode-hook . yas-minor-mode)
         (elixir-mode-hook . yas-minor-mode)
         (html-mode-hook . yas-minor-mode)
         (js-mode-hook . yas-minor-mode)
         (python-mode-hook . yas-minor-mode)
         (ruby-mode-hook . yas-minor-mode)
         (go-mode-hook . yas-minor-mode)
         (sh-mode-hook . yas-minor-mode)
         (markdown-mode-hook . yas-minor-mode)
         (makefile-mode-hook . yas-minor-mode)
         (text-mode . yas-minor-mode))
  :init
  (setq yas-snippet-dirs (list (concat user-emacs-directory "my_snippets")))
  (setq yas-alias-to-yas/prefix-p nil)
  (setq-default yas-verbosity 1)
  ;; Fix ac-define-source of auto-complete-config.el
  (defalias 'yas/expand 'yas-expand)

  :config
  (yas-reload-all)

  ;; utility functions
  (defun my-yas/perl-package-name ()
    (let ((file-path (file-name-sans-extension (buffer-file-name))))
      (if (string-match "lib/\\(.+\\)\\'" file-path)
          (replace-regexp-in-string "/" "::" (match-string 1 file-path))
        (file-name-nondirectory file-path))))

  (defun my-yas/parent-directory ()
    (let ((curdir (directory-file-name (file-name-directory (buffer-file-name)))))
      (file-name-nondirectory curdir)))

  ) ;; yasnippet


;;; yasnippet-snippets
(use-package yasnippet-snippets
  :ensure t
  :bind (:map help-map
              ("y" . yas-describe-tables))
  ) ;; yasnippet-snippets


;;; Editor Config
;; -> https://github.com/editorconfig/editorconfig-emacs
(use-package editorconfig
  :defer t
  :config
  (editorconfig-mode 1)
  ) ;; editorconfig


;;; vc-mode
(use-package vc-mode
  :bind ("C-x v d" . vc-diff)
  :init
  (setq vc-handled-backends '(Git))
  (setq vc-follow-symlinks t)
  (setq auto-revert-check-vc-info t)
  (setq auto-revert-mode-text "")

  :config
  (global-auto-revert-mode 1)
  (remove-hook 'find-file-hooks 'vc-find-file-hook)
  ) ;; vc-mode


;;; git-messenger
(use-package git-messenger
  :ensure t
  :bind ("C-x v m" . git-messenger:popup-message)
  :init
  (setq git-messenger:handled-backends '(git))
  ) ;; git-messenger


;;; git-gutter
(use-package git-gutter
  :ensure t
  :bind (("C-x v u" . git-gutter)
         ("C-x v p" . git-gutter:stage-hunk)
         ("C-x v =" . git-gutter:popup-hunk)
         ("C-x v r" . git-gutter:revert-hunk)
         ("C-x v e" . git-gutter:end-of-hunk)
         ("C-x v SPC" . git-gutter:mark-hunk))
  :init
  (setq git-gutter:update-interval 2)
  (setq git-gutter:handled-backends '(git svn))
  (global-git-gutter-mode +1)

  :config
  (use-package smartrep
    :ensure t
    :config
    (smartrep-define-key
        global-map  "C-x " '(("C-p" . 'git-gutter:previous-hunk) ;; mark-page
                             ("C-n" . 'git-gutter:next-hunk)))) ;; set-goal-column

  ) ;; git-gutter

;; C-x n       git-gutter:next-hunk
;; C-x p       git-gutter:previous-hunk
;;
;; C-x C-p     helm-editutil-git-ls-files
;; C-x v SPC   git-gutter:mark-hunk
;; C-x v =     git-gutter:popup-hunk
;; C-x v e     git-gutter:end-of-hunk
;; C-x v m     git-messenger:popup-message
;; C-x v p     magit-status
;; C-x v r     git-gutter:revert-hunk
;; C-x v u     git-gutter
;; C-x w c     editutil-browse-github-commit
;; C-x w f     editutil-browse-github-file
;; C-x w w     editutil-browse-github
;; M-g M-g     magit-status
