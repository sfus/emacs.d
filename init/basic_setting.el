;; basic configurations
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8-unix)

(global-font-lock-mode +1)

;; basic customize variables
(custom-set-variables
 '(auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
 '(backup-directory-alist `((".*" . ,temporary-file-directory)))
 '(comment-style 'extra-line)
 '(create-lockfiles nil)
 '(delete-auto-save-files t)
 '(find-file-visit-truename t)
 '(imenu-auto-rescan t)
 '(inhibit-startup-screen t)
 '(large-file-warning-threshold (* 25 1024 1024))
 '(package-enable-at-startup nil)
 '(read-file-name-completion-ignore-case t)
 '(set-mark-command-repeat-pop t)
 '(text-quoting-style 'grave)
;; '(user-full-name "Syohei YOSHIDA")
 '(custom-file (concat user-emacs-directory "custom.el")))

;; Frame and cursor looking
(blink-cursor-mode t)
(menu-bar-mode -1)
(line-number-mode 1)
(column-number-mode 1)
(setq-default horizontal-scroll-bar nil)

(when window-system
  (set-scroll-bar-mode 'nil)
  (tool-bar-mode 0))

;; for GC
;;(setq-default gc-cons-threshold (* gc-cons-threshold 10))
(setq-default gc-cons-threshold (* 256 1024 1024)) ;; 256MB

(setq-default echo-keystrokes 0)
;; I never use C-x C-c
(defalias 'exit 'save-buffers-kill-emacs)

;; Don't disable commands
(dolist (cmd '(narrow-to-region upcase-region downcase-region set-goal-column))
  (put cmd 'disabled nil))

(savehist-mode 1)
(save-place-mode +1)

;; indicate last line
(setq-default indicate-empty-lines t
              indicate-buffer-boundaries 'right)

;; not create backup file and not create auto save file
(setq-default backup-inhibited t)

;; not beep
(setq-default ring-bell-function #'ignore)

(fset 'yes-or-no-p #'y-or-n-p)
(setq-default use-dialog-box nil)

(setq-default history-length 500
              history-delete-duplicates t)

(require 'server)
(unless (server-running-p)
  (server-start))

;; which-func
(which-function-mode +1)
(setq-default which-func-unknown "")

;; invisible mouse cursor when editing text
(setq-default make-pointer-invisible t)

;; undo setting
(setq-default undo-no-redo t
              undo-limit 600000
              undo-strong-limit 900000)

;;;; undo-tree
(global-undo-tree-mode)
(define-key undo-tree-map (kbd "C-/") 'undo-tree-undo)
(define-key undo-tree-map (kbd "M-_") 'nil)
;;;;+ Extra
(global-set-key (kbd "C-S-/") 'undo-tree-redo) ;; C-S-/ => redo
(global-set-key (kbd "C-M-_") 'undo-tree-redo) ;; C-M-_ => redo (Esc C-/ on console)
(setq undo-tree-mode-lighter nil) ;; delete minor-mode in mode-line
;;;;+

(setq-default fill-column 80)

;; smart repetition
(require 'smartrep)
(custom-set-variables
 '(smartrep-mode-line-active-bg nil)
 '(smartrep-mode-line-string-activated "<<< SmartRep >>>"))

(add-to-list 'auto-mode-alist '("/\\(?:LICENSE\\|Changes\\)\\'" . text-mode))

(defun my/text-mode-hook ()
  (when (string-prefix-p "Changes" (buffer-name))
    (setq-local company-backends '(company-ispell company-files company-dabbrev))
    (flyspell-mode +1)))
(add-hook 'text-mode-hook 'my/text-mode-hook)

(with-eval-after-load "text-mode"
  (define-key text-mode-map (kbd "C-M-i") 'company-complete))

(custom-set-variables
 '(hippie-expand-verbose nil)
 '(hippie-expand-try-functions-list
   '(try-expand-dabbrev
     try-complete-file-name
     try-complete-file-name-partially
     try-expand-dabbrev-all-buffers)))

(custom-set-variables
 '(which-key-lighter "")
 '(which-key-idle-delay 0.5))
(which-key-mode +1)

(winner-mode +1)


;;;++ Extra
;;; Load my own edit utility functions
(require 'editutil-extra nil t)

(setq garbage-collection-messages t)

;;; Edit default
(setq-default tab-width 4)              ;; tab 幅
(setq-default truncate-partial-width-windows t);; 左右分割時には切り捨てる

;;; Scrolling
(setq scroll-step 1)                    ;; 画面から出たときにスクロールさせる列数
(setq scroll-conservatively 101)        ;; カーソルを再表示する行数の閾値 (100超なら再表示しない)
(setq scroll-margin 2)                  ;; ポイントをウィンドウ上下端に近づけられる行数
(setq fast-but-imprecise-scrolling t)   ;; スクロールを高速化 (25.1以降)

;;; Others
(setq enable-recursive-minibuffers t)   ;; 再帰編集を可能に ;; default: nil
(setq kill-read-only-ok t)              ;; Read Onlyなバッファでもkillでコピー可能に
(setq delete-by-moving-to-trash t)      ;; ごみ箱を有効に
(delete-selection-mode 1)               ;; 選択状態で入力した際に元のテキストを消す

;;; remember-notes
(setq initial-buffer-choice 'remember-notes)
(setq remember-data-file "~/.emacs.d/notes.org");; default: "~/.emacs.d/notes"
(setq remember-notes-initial-major-mode 'org-mode)

;;; edebug
(setq debug-on-error nil)
(setq eval-expression-print-level nil)
(setq eval-expression-print-length nil)
(setq eval-expression-debug-on-error nil)
(setq edebug-print-length 1000)
(setq edebug-print-level 1000)

;; ;;; word boundary
;; (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)
;; (modify-syntax-entry ?- "w" org-mode-syntax-table)
