;; ;; Revert C-h to <BS>
;; (global-set-key (kbd "C-h") 'delete-backward-char)

;; C-h to <BS> with untabify
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
;; Can be `untabify' -- turn a tab to many spaces, then delete one space;
;;        `hungry' -- delete all whitespace, both tabs and spaces;
;;        `all' -- delete all whitespace, including tabs, spaces and newlines;
;;        nil -- just delete one character.
(setq backward-delete-char-untabify-method 'untabify) ;; untabify, hungry, all, nil

;; Unbind C-z for other package prefix key
(global-unset-key (kbd "C-z")) ;; default: suspend-frame

(global-set-key [delete] 'delete-char)

;; C-x C-c => save-buffers-kill-terminal for emacsclient on terminal
(defun my/save-buffers-kill-terminal ()
  (interactive)
  (recentf-mode 1)
  (recentf-save-list)
  (setq initial-buffer-choice (buffer-file-name))
  (save-buffers-kill-terminal))
(unless (window-system)
  (define-key ctl-x-map (kbd "C-c") 'my/save-buffers-kill-terminal))

(define-key ctl-x-map ";" 'comment-region)
(define-key ctl-x-map ":" 'uncomment-region) ;; default: C-u C-x ;

;; change help prefix to C-x C-h
(define-key ctl-x-map (kbd "C-h") 'help-command) ;; default: C-h, F1

;; C-M-k => kill-current-buffer (default: kill-sexp)
(define-key global-map (kbd "C-M-k") 'kill-current-buffer)

;; Swap just-one-space (M-SPC) and dabbrev-expand (M-/)
(global-set-key (kbd "M-SPC") 'dabbrev-expand)
(global-set-key (kbd "M-/") 'just-one-space)
(eval-after-load "dabbrev" ;; overwrite dabbrev's keybind
  '(global-set-key (kbd "M-/") 'just-one-space))

;; C-M-o => other-window
(global-set-key (kbd "C-M-o") 'other-window) ;; default: split-line

(global-set-key (kbd "C-x O") 'split-line) ;; default: C-o

;; C-M-g => delete-other-windows (default: C-x 1)
(global-set-key (kbd "C-M-g") 'delete-other-windows)
;; S-F5 => delete-other-windows (M-C-g transfer S-F5 key code for console by key remapping tool)
(global-set-key (kbd "S-<f5>") 'delete-other-windows)

(global-set-key (kbd "C-m") 'newline-and-indent)

;; C-x C-b => ibuffer
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; M-, / M-* => tag jump
(global-set-key (kbd "M-,") 'pop-tag-mark) ;; default: xref-pop-marker-stack (alias)
(global-set-key (kbd "M-*") 'tags-loop-continue)

;; C-x C-m R => revert-buffer
(global-set-key (kbd "C-x RET R") 'revert-buffer)

;; M-g t => toggle-truncate-lines
(global-set-key (kbd "M-g t") 'toggle-truncate-lines)

;; M-g M-t => ff-find-other-file
(global-set-key (kbd "M-g M-t") 'ff-find-other-file)


;;; Help Key Bindings
;; Additional help keymap setting like following lines
;; C-x F (find-function)        ;; default: F1 f
;; C-x K (find-function-on-key) ;; default: F1 k
;; C-x V (find-variable)        ;; default: F1 v
(find-function-setup-keys)


;; ;; Ctrl-q map
;; (defvar my/ctrl-q-map (make-sparse-keymap)
;;   "My original keymap binded to C-q.")
;; (defalias 'my/ctrl-q-prefix my/ctrl-q-map)
;; (define-key global-map (kbd "C-q") 'my/ctrl-q-prefix)
;; (define-key my/ctrl-q-map (kbd "C-q") 'quoted-insert)

;; (define-key my/ctrl-q-map (kbd "C-c") 'column-highlight-mode)
;; (define-key my/ctrl-q-map (kbd "C-a") 'text-scale-adjust)
;; (define-key my/ctrl-q-map (kbd "C-f") 'flyspell-mode)
;; (define-key my/ctrl-q-map (kbd "C-m") 'flycheck-mode)
