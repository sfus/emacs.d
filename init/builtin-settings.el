;;; isearch
(use-package isearch
  :bind (:map isearch-mode-map
              ("C-h" . isearch-delete-char)      ;; C-h => delete-char
              ("C-y" . isearch-yank-kill)        ;; C-y => yank for isearch
              ("C-e" . isearch-edit-string)      ;; C-e => edit search string
              ("TAB" . isearch-yank-word))
  :config
  ;; C-d => isearch using next character on cursor
  (defun my-isearch-yank-char ()
    "Pull next character from buffer into search string."
    (interactive)
    (isearch-yank-string
     (save-excursion
       (and (not isearch-forward) isearch-other-end
            (goto-char isearch-other-end))
       (buffer-substring (point) (1+ (point))))))
  (define-key isearch-mode-map (kbd "C-d") 'my-isearch-yank-char)
  ) ;; isearch


;;; browse-url
(use-package browse-url
 :bind (("C-c C-o" . browse-url-at-point)
        ("C-x ," . browse-url-at-point))
 ) ;; browse-url


;;; info
(use-package info
  :bind (:map Info-mode-map
              ("j" . next-line)
              ("k" . previous-line)))


;;; ffap
(use-package ffap
  :bind (("M-g M-f" . ffap)
         ("M-g M-w" . ffap-copy-string-as-kill))
  :init
  (ffap-bindings)
  :config
  ;; add lisp-interaction-mode to ffap-el-mode
  (setq ffap-alist
        (append '((lisp-interaction-mode . ffap-el-mode))
                ffap-alist))
  ) ;; ffap


;;; flyspell
(use-package flyspell
  :bind ("M-$" . flyspell-mode)
  :init
  (setq flyspell-issue-welcome-flag nil)
  (setq flyspell-use-meta-tab nil)
  ) ;; flyspell


;;; ispell
;; M-$ or M-x ispell-region, M-x ispell-buffer
;; -> http://keisanbutsuriya.hateblo.jp/entry/2015/02/10/152543
;; use aspell instead of ispell
;; $ echo "lang en_US" >> ~/.aspell.conf
(use-package ispell
  :defer t
  :init
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")) ;; skip Japanese text
  (setq-default ispell-program-name "aspell")

  ;; dictionary
  ;;(setenv "DICTIONARY" "/usr/share/dict/words")
  (setq ispell-alternate-dictionary "/usr/share/dict/words")

  ;; personal dictionaly       ;; default: "~/.ispell_DICTNAME"
  (setq ispell-personal-dictionary "~/.emacs.d/.ispell_default")

  ;; grep
  (setq ispell-grep-command "grep")
  ) ;; ispell


;;; ac-ispell
;; -> https://github.com/syohex/emacs-ac-ispell
;; -> http://syohex.hatenablog.com/entry/20131123/1385184659
(use-package ac-ispell ;; Completion words longer than 4 characters
  :ensure t
  :bind ("C-x i" . ac-complete-ispell) ;; default: insert-file
  :hook ((git-commit-mode-hook . ac-ispell-ac-setup)
         (mail-mode-hook . ac-ispell-ac-setup)
         (text-mode-hook . ac-ispell-ac-setup))
  :init
  (setq ac-ispell-requires 4)
  :config
  (use-package auto-complete
    :init
    (ac-ispell-setup))
  ) ;; ac-ispell


;;; elec-pair
(use-package elec-pair
  :init
  (defvar my/electric-pair-enabled-modes
    '(c-mode
      c++-mode
      objc-mode
      java-mode
      python-mode
      ruby-mode
      erlang-mode
      elixir-mode
      prolog-mode
      haskell-mode
      inferior-haskell-mode
      sh-mode
      js-mode
      go-mode
      css-mode
      cmake-mode
      coffee-mode
      tuareg-mode
      tuareg-interactive-mode
      cperl-mode
      perl6-mode
      markdown-mode
      org-mode
      gfm-mode
      sql-mode))

  (dolist (mode my/electric-pair-enabled-modes)
    (add-hook (intern (concat (symbol-name mode) "-hook")) #'electric-pair-local-mode))
  ) ;; elec-pair


;;; paren
(use-package paren
  :init
  (show-paren-mode 1)
  (setq show-paren-delay 0)
  (setq show-paren-style 'expression)
  (setq parens-require-spaces nil)
  :config
  ;; decrease show-paren-match face priority
  (setq show-paren-priority -50) ;; default: 1000
  ) ;; paren


;;; abbrev
(use-package abbrev
  :config
  ;; delete mode-line name
  (setcar (cdr (assq 'abbrev-mode minor-mode-alist)) "")
  ) ;; abbrev


;;; page-ext (extention for narrowing)
;; http://emacs.rubikitch.com/page-ext/
(use-package page-ext
  :defer t
  ) ;; page-ext


;;; align
(use-package align
  :defer t
  :config
  (add-to-list 'align-rules-list
               '(camma-assignment
                 (regexp . ",\\( *\\)")
                 (repeat . t)
                 (modes  . '(cperl-mode))))
  ) ;; align


;; ;;; linum
;; (use-package linum
;;   :defer t
;;   :hook ((text-mode-hook
;;           emacs-lisp-mode-hook
;;           c-mode-hook
;;           c++-mode-hook
;;           perl-mode-hook
;;           cperl-mode-hook
;;           ruby-mode-hook
;;           ) . linum-mode)
;;
;;   :init
;;   (if window-system
;;       (setq linum-format "%5d")
;;     (setq linum-format "%5d|"))
;;
;;   :config
;;   ;; (global-linum-mode t)
;;
;;   ) ;; linum


;;; uniquify
(use-package uniquify
  :defer t
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  ) ;; uniquify


;;; bs
(use-package bs
  :defer t
  :config
  (fset 'bs-message-without-log 'ignore)
  ) ;; bs


;;; woman
(use-package woman
 :defer t
 :init
 (setq woman-use-own-frame nil)
 (setq woman-cache-filename (concat user-emacs-directory "woman_cache.el"))
 (setq woman-manpath '("/usr/share/man" "/usr/local/man" "/usr/man")))


;;;; dired
(use-package dired
  :bind (:map dired-mode-map
              ("Q"      . quick-preview-at-point)
              ("C-M-u"  . dired-up-directory)
              ("r"      . wdired-change-to-wdired-mode)
              ("C-h"    . dired-up-directory)
              ([delete] . dired-up-directory)
              ([backspace] . dired-up-directory)
              ("z"      . my/dired-toggle-listing-switches))

  :init
  (setq ls-lisp-dirs-first t)
  (setq dired-dwim-target t)
  (setq dired-auto-revert-buffer t)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)

  :config
  ;; Not create new buffer, if you chenge directory in dired
  (put 'dired-find-alternate-file 'disabled nil)
  ;;(load-library "ls-lisp")

  ;; h => hexl-mode
  (define-key dired-mode-map (kbd "h")
    (lambda () (interactive)
      (hexl-find-file (dired-get-filename))))

  ;; z => toggle to show hidden files
  (let ((match (string-match "a" dired-listing-switches)))
    (if match
        (progn
          (defvar my/dired-listing-switches-nocontain-a
            (concat (substring dired-listing-switches 0 match)
                    (substring dired-listing-switches (1+ match))))
          (defvar my/dired-listing-switches-contain-a dired-listing-switches))
      (defvar my/dired-listing-switches-contain-a (concat dired-listing-switches "a"))
      (defvar my/dired-listing-switches-nocontain-a dired-listing-switches)))
  (defun my/dired-toggle-listing-switches ()
    (interactive)
    (let ((curdir default-directory))
      (if (string-match "a" dired-listing-switches)
          (setq dired-listing-switches my/dired-listing-switches-nocontain-a)
        (setq dired-listing-switches my/dired-listing-switches-contain-a))
      (kill-buffer (current-buffer))
      (dired curdir)))

  ;;; My C-m binding

  ;; use `dired-find-alternate-file' to open directory.   ;; default: 'a'
  ;;     (for not to create extra buffer.)
  ;; use `dired-view-file' to open file.                  ;; default: 'v'
  ;; use `dired-find-file' to open archive file.          ;; default: 'f'
  (defun dired-advertised-find-file* ()
    "In dired, run `dired-find-alternate-file' or run `dired-view-file'.
When this line is a directory, execute `dired-find-alternate-file';
otherwise, execute `dired-view-file'."
    (interactive)
    (cond
     ((file-directory-p (dired-get-filename))
      (if (= 1 (length (get-buffer-window-list (current-buffer))))
          (dired-find-alternate-file)
        (dired-find-file)))
     ((string-match "\\(\\.tar\\(\\.gz\\)?\\|\\.zip\\|\\.lzh\\|\\.jar\\)\\'"
                    (dired-get-filename))
      (dired-find-file)
      (toggle-read-only t))
     (t (dired-view-file))))

  (eval-after-load "dired"
    '(defalias 'dired-advertised-find-file 'dired-advertised-find-file*))
  (define-key dired-mode-map (kbd "\C-m") 'dired-advertised-find-file*)

  ;; use `dired-find-alternate-file' to go up directory (`dired-up-directory')
  (defadvice dired-up-directory (around dired-up-directory* (arg) activate)
    "In dired, run `dired-up-directory' and kill previous buffer."
    (let ((buf (current-buffer)))
      ad-do-it
      (if (and (eq 'dired-mode  major-mode)       ; in dired-mode
               (null arg)                         ; no dired-other-window
               (not (get-buffer-window buf))      ; unless other-window has same dired-buf
               (not (eq (current-buffer) buf)))   ; unless in the root directory
          (kill-buffer buf))))

  ;; view-mode
  (with-eval-after-load 'view
    ;; quit view-mode on C-m, and if in dired, then move cursor to next line.
    ;; (so with above setting, view each file by repeating C-m.)
    (defun View-quit-and-next-line (arg)
      "Quit View mode, and if Dired mode, go to the next line."
      (interactive "p")
      (View-quit)
      (recenter)
      (cond
       ((eq 'dired-mode  major-mode) (dired-next-line arg))
       ((eq 'tar-mode major-mode) (tar-next-line arg))
       ((eq 'archive-mode major-mode) (archive-next-line arg))))

    (define-key view-mode-map (kbd "\C-m") 'View-quit-and-next-line))

  ;; image-mode
  (with-eval-after-load 'image-mode
    ;; ditto on image-mode
    (defun my/image-quit-and-next-line (arg)
      "Quit Image mode, and if Dired mode, go to the next line."
      (interactive "p")
      (kill-this-buffer)
      (recenter)
      (if dired-mode-p (dired-next-line arg)))

    (define-key image-mode-map "\C-m" 'my/image-quit-and-next-line))

  ;; tar-mode
  (with-eval-after-load 'tar-mode
    ;; C-m to view mode on tar-mode
    (define-key tar-mode-map "\C-m" 'tar-view)
    ;; q => kill-buffer (default: quit-window)
    (define-key tar-mode-map "q" 'kill-this-buffer))

  ;; archive-mode
  (with-eval-after-load 'archive-mode
    ;; C-m to view mode on archive-mode.
    (define-key archive-mode-map "\C-m" 'archive-view)
    ;; q => kill-buffer (default: quit-window)
    (define-key archive-mode-map "q" 'kill-this-buffer))


  (use-package dired-k
    :ensure t
    :hook (dired-initial-position-hook  . dired-k)
    :bind (:map dired-mode-map
              ("K" . dired-k))
    ) ;; dired-k

  ) ;; dired

;; (use-package dired-x
;;   :defer t
;;   :bind ("C-x C-j" . dired-jump)
;;   :init
;;   (autoload 'dired-jump "dired-x" nil t)
;;   ) ;; dired-x
