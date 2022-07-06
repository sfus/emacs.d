;; basic configurations
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8-unix)

;; locale setting (to change English datetime format)
(setenv "LC_TIME" "C")

(global-font-lock-mode +1)
(delete-selection-mode 1) ;; delete region when input with selection

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
 '(garbage-collection-messages t)
 '(gc-cons-threshold 536870912) ;; (500MB) ;; default: 800000 (on 16GB RAM)
 '(gc-cons-percentage 0.3) ;; default: 0.1
 '(custom-file (concat user-emacs-directory "custom.el"))
 '(line-number-display-limit 1000000)
 '(mode-line-frame-identification " ") ;; delete frame name
 ;; scrolling
 '(scroll-step 5)                       ;; basic scroll step (default: 0)
 '(scroll-conservatively 0)             ;; (default: 0) (if over 100, never recenter point)
 '(scroll-preserve-screen-position nil) ;; keep cursor position on scrolling (default: nil)
 '(scroll-margin 2)                     ;; line margin to window edge
 '(next-screen-context-lines 20)        ;; C-v / M-v duplication lines (default: 2)
 '(fast-but-imprecise-scrolling t)      ;; scroll speed up (for 25.1 after)
 ;; others
 '(enable-recursive-minibuffers t)      ;; enable recursive edit in minibuffer
 '(kill-read-only-ok t)                 ;; enable kill to copy even if read only buffer
 '(delete-by-moving-to-trash t)         ;; enable trash
 ;; edebug
 '(debug-on-error nil)
 '(eval-expression-print-level nil)
 '(eval-expression-print-length nil)
 '(eval-expression-debug-on-error nil)
 '(edebug-print-length 1000)
 '(edebug-print-level 1000)
 ;; recentf
 '(recentf-max-saved-items 2000)
 ;;'(recentf-auto-cleanup 600)
 '(recentf-exclude '("recentf" "/elisps/" "\\`/tmp/" "/\\.git/" "/\\.cask/" "/elpa/"
                     "/tmp/" "/el-get/" ".loaddefs.el" "/\\.cpanm/"
                     "\\.mime-example" "\\.ido.last" "woman_cache.el"
                     "\\`/proc/" "\\`/sys/"
                     "CMakeCache.txt" "/bookmarks" ;;"\\.gz$"
                     "COMMIT_EDITMSG" "MERGE_MSG" "git-rebase-todo"
                     "/org/agenda/"))
 )

;; use space
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; Frame and cursor looking
(blink-cursor-mode t)
(menu-bar-mode -1)
(line-number-mode 1)
(column-number-mode 1)
(setq-default horizontal-scroll-bar nil)
(setq-default truncate-partial-width-windows t);; truncate line if window splitted

(when window-system
  (set-scroll-bar-mode 'nil)
  (tool-bar-mode 0))

;; enable mouse on terminal
(xterm-mouse-mode 1)

(setq-default echo-keystrokes 0)
(setq-default fill-column 80)
(defalias 'exit 'save-buffers-kill-emacs)


;; line feed on mode-line
;; -> https://qiita.com/kai2nenobu/items/ddf94c0e5a36919bc6db
(setq eol-mnemonic-dos " (CRLF) ")
(setq eol-mnemonic-unix " (LF) ")
(setq eol-mnemonic-mac " (CR) ")
(setq eol-mnemonic-undecided " (?) ")


;; Don't disable commands
(dolist (cmd '(narrow-to-region upcase-region downcase-region set-goal-column))
  (put cmd 'disabled nil))

;; history
(savehist-mode 1)
(save-place-mode +1)
(winner-mode +1)

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

;; ;; which-func
;; (which-function-mode +1)
;; (setq-default which-func-unknown "")

;; invisible mouse cursor when editing text
(setq-default make-pointer-invisible t)

;; undo setting
(setq-default undo-no-redo t
              undo-limit 600000
              undo-strong-limit 900000)

;; C-x / => toggle-case-fold-search
(if (require 'menu-bar nil t)
    (define-key ctl-x-map "/" 'toggle-case-fold-search))

(setq-default case-fold-search t
              isearch-case-fold-search t)

;; keep case on replace (default: t)
(setq case-replace t)
;; (BEFORE) (AFTER(t))  (AFTER(nil))
;; foo      bar         bar
;; Foo      Bar         bar
;; FOO      BAR         bar

(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; (setq isearch-allow-scroll nil)         ;; not allow scroll on isearch
(setq lazy-highlight-initial-delay 0)   ;; highlight isearch instantly

;; sample characters for `list-face-display'
(setq list-faces-sample-text "漢字ひらがなカタカナabcdefghijklmnOPQRSTUVWXYZ")

;; Run GC every 60 seconds if emacs is idle.
(run-with-idle-timer 60.0 t #'garbage-collect)
