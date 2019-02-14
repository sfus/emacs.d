;;;; global key setting
(global-set-key (kbd "M-ESC ESC") 'read-only-mode)
(global-set-key (kbd "M-,") 'pop-tag-mark)
(global-set-key (kbd "M-*") 'tags-loop-continue)
(global-set-key [delete] 'delete-char)
(global-set-key (kbd "C-M-l") 'goto-line)
(global-set-key (kbd "M-=") 'yas-insert-snippet)
(global-set-key (kbd "C-x z") 'zoom-window-zoom)
(global-set-key (kbd "M-'") 'avy-goto-word-1)
(global-set-key (kbd "C-x C-c") popwin:keymap)
(global-set-key (kbd "C-x C-c C-i") 'import-popwin)
(global-set-key (kbd "C-x RET R") 'revert-buffer)
(global-set-key (kbd "C-x j") 'jump-to-register)
(global-set-key (kbd "C-x SPC") 'point-to-register)
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; unset keys
(global-unset-key (kbd "C-x C-n"))

;; helm binding
(global-set-key (kbd "C-M-z") 'helm-resume)
(global-set-key (kbd "C-x C-c C-c") 'helm-M-x)
(global-set-key (kbd "C-M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-h a") 'helm-apropos)
(global-set-key (kbd "C-h m") 'helm-man-woman)
(global-set-key (kbd "C-h l")  'popwin:popup-last-buffer)
(global-set-key (kbd "C-h SPC") 'popwin:select-popup-window)
(global-set-key (kbd "C-x C-i") 'helm-imenu)
(global-set-key (kbd "C-x b") 'helm-buffers-list)

;; Ctrl-q map
(defvar my/ctrl-q-map (make-sparse-keymap)
  "My original keymap binded to C-q.")
(defalias 'my/ctrl-q-prefix my/ctrl-q-map)
(define-key global-map (kbd "C-q") 'my/ctrl-q-prefix)
(define-key my/ctrl-q-map (kbd "C-q") 'quoted-insert)

(define-key my/ctrl-q-map (kbd "C-c") 'column-highlight-mode)
(define-key my/ctrl-q-map (kbd "C-a") 'text-scale-adjust)
(define-key my/ctrl-q-map (kbd "C-f") 'flyspell-mode)
(define-key my/ctrl-q-map (kbd "C-m") 'flycheck-mode)

(smartrep-define-key
    global-map "C-q" '(("-" . 'goto-last-change)
                       ("+" . 'goto-last-change-reverse)))

(smartrep-define-key
    undo-tree-map "C-x" '(("u" . 'undo-tree-undo)
                          ("U" . 'undo-tree-redo)))

;; editutil mappings
(editutil-default-setup)
(global-set-key (kbd "C-x c") ctl-x-4-map)
(global-set-key (kbd "C-x c j") 'dired-jump-other-window)
(global-set-key (kbd "C-x c y") 'clipboard-yank)

;; M-g mapping
(global-set-key (kbd "M-g .") 'helm-ag)
(global-set-key (kbd "M-g ,") 'helm-ag-pop-stack)
(global-set-key (kbd "M-g p") 'helm-ag-project-root)
(global-set-key (kbd "M-g M-f") 'ffap)
(global-set-key (kbd "M-g M-w") 'ffap-copy-string-as-kill)
(global-set-key (kbd "M-g M-t") 'ff-find-other-file)
(global-set-key (kbd "M-g r") #'recompile)
(global-set-key (kbd "M-g q") #'quickrun)

;;; buffer-move
(global-set-key (kbd "M-g h") 'buf-move-left)
(global-set-key (kbd "M-g j") 'buf-move-down)
(global-set-key (kbd "M-g k") 'buf-move-up)
(global-set-key (kbd "M-g l") 'buf-move-right)


;;;;+ Extra
(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key (kbd "C-o") 'open-line)
(global-set-key (kbd "C-M-g") 'delete-other-windows);; default: C-x 1

(with-eval-after-load "quickrun"
  (define-key quickrun--mode-map (kbd "C-g") 'quit-window))

;; Revert into default keybind from syohex's setting (I want to use C-x C-c)
(defun my/save-buffers-kill-terminal ()
  (interactive)
  (setq initial-buffer-choice (buffer-file-name))
  (save-buffers-kill-terminal))
(define-key ctl-x-map (kbd "C-c") 'my/save-buffers-kill-terminal)

;; Change C-h keymap to C-x C-h (help-map)
(define-key help-map (kbd "C-a") 'helm-apropos) ;; default: (about-emacs)
(define-key help-map (kbd "C-m") 'helm-man-woman) ;; default: (view-order-manuals)
(define-key help-map (kbd "C-l") 'popwin:popup-last-buffer)
(define-key help-map (kbd "SPC") 'popwin:select-popup-window)
(with-eval-after-load "popwin"
 (define-key help-map (kbd "C-z") popwin:keymap))

;; ;; Revert C-h to <BS>
;; (global-set-key (kbd "C-h") 'delete-backward-char)

;; C-h to <BS> with untabify
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
;; ;;
;; Can be `untabify' -- turn a tab to many spaces, then delete one space;
;;        `hungry' -- delete all whitespace, both tabs and spaces;
;;        `all' -- delete all whitespace, including tabs, spaces and newlines;
;;        nil -- just delete one character.
(setq backward-delete-char-untabify-method 'hungry) ;; untabify, hungry, all, nil

;; C-M-k => kill-this-buffer (default: kill-sexp)
(define-key global-map (kbd "C-M-k") 'kill-this-buffer)

;; Swap just-one-space (M-SPC) and dabbrev-expand (M-/)
(global-set-key (kbd "M-SPC") 'dabbrev-expand)
(global-set-key (kbd "M-/") 'just-one-space)
(eval-after-load "dabbrev" ;; overwrite dabbrev's keybind
  '(global-set-key (kbd "M-/") 'just-one-space))

;; C-M-o => other-window
(global-set-key (kbd "C-M-o") 'other-window)

;; C-q C-o => window rotation
(with-eval-after-load "evil"
  (global-set-key (kbd "C-q C-o") 'evil-window-rotate-downwards))

;; unset M-O to enable cursor keys on console
(with-eval-after-load "helm"
  (global-unset-key (kbd "M-O")))


;;; ctl-x-map Bindings

;; C-x D => ediff-buffers
(define-key ctl-x-map "D" 'ediff-buffers)

;; change help prefix to C-x C-h
(define-key ctl-x-map (kbd "C-h") 'help-command) ;; default: C-h, F1

(define-key ctl-x-map ";" 'comment-region)
(define-key ctl-x-map ":" 'uncomment-region) ;; default: C-u C-x ;

;; Revert C-x C-q to toggle-read-only (use vc-next-action (C-x v v))
(eval-after-load "vc"
  '(define-key ctl-x-map (kbd "C-q") 'toggle-read-only)) ;; vc-toggle-read-only


;;; Help Key Bindings
;; Additional help keymap setting like following lines
;; C-x F (find-function)        ;; default: F1 f
;; C-x K (find-function-on-key) ;; default: F1 k
;; C-x V (find-variable)        ;; default: F1 v
(find-function-setup-keys)


;;; Additional my/ctrl-q-map
(define-key my/ctrl-q-map (kbd "C-i") 'self-insert-command)
(define-key my/ctrl-q-map (kbd "C-j") 'self-insert-command)
(define-key my/ctrl-q-map (kbd "C-m") 'self-insert-command)


;;; info
(eval-after-load "info"
  '(progn
     (define-key Info-mode-map "j" 'next-line)
     (define-key Info-mode-map "k" 'previous-line)))


;;; Function Key Bindings
;; S-F5 => delete-other-windows (M-C-g transfer S-F5 key code for console by key remapping tool)
(global-set-key (kbd "S-<f5>") 'delete-other-windows)

;; F7: toggle-truncate-lines
(global-set-key [f7] 'toggle-truncate-lines)

;; F8: open *scratch* buffer
(define-key global-map [f8]
  (defun my/switch-to-scratch-buffer ()
    (interactive)
    (switch-to-buffer "*scratch*")))

;; F9: toggle menu bar
(global-set-key [f9] 'menu-bar-mode)

;; F12: kill-this-buffer
(global-set-key [f12] 'kill-this-buffer)
