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


;;; org-mode
(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         :map org-mode-map
         ("M-," . org-mark-ring-goto)
         ("C-c t" . org-toggle-link-display)
         ("C-M-<return>" . org-insert-todo-heading)
         ("C-c <tab>" . outline-show-all)
         ("C-m" . org-return-indent) ;; default: C-j
         ("C-j" . org-return) ;; default: C-m
         ("C-c i" . org-indent-mode)
         ("C-c I" . org-attach-screenshot)
         ("C-M-j" . my/org-insert-heading-dwim)
         ;;("M-j" . org-insert-todo-heading)
         ("M-j"   . org-next-visible-heading) ;; default: C-c C-n
         ("M-k"   . org-previous-visible-heading) ;; default: C-c C-p (kill-sentence)
         )
  :init
  (custom-set-variables
   '(org-startup-truncated nil)
   ;;'(org-startup-indented t) ;; default: nil
   ;;'(org-directory (expand-file-name "~/Dropbox/")) ;; default: "~/org"
   ;;'(org-agenda-files (list "~/Tasks/"))
   '(org-agenda-files (list my/org-agenda-root))
   ;;'(org-yank-folded-subtrees nil) ;; default: t
   '(org-return-follows-link t)
   '(org-use-fast-todo-selection t)
   '(org-src-fontify-natively t)
   '(org-goto-auto-isearch nil) ;; default: t
   ;;'(org-default-notes-file (concat org-directory "organizer.org"))
   '(org-default-notes-file  (concat my/org-agenda-root "inbox.org")) ;; default: "~/.notes"
   ;;'(org-default-priority ?A)
   '(org-default-priority ?C) ;; default: ?B
   '(org-lowest-priority ?F) ;; default: ?C
   '(org-imenu-depth 3)
   '(org-todo-keywords
     ;;'((sequence "TODO(t)" "DOING(d)" "|" "DONE(x)" "BLOCKED(b)" "CANCEL(c)" "NO_ACTION(n)"))
     '((sequence "TODO(t)" "NEXT(n)" "DOING(d!)" "BREAK(b)" "|" "DONE(x!)")
       (sequence "WAITING(w!)" "SOMEDAY(s)" "|" "CANCELED(c!)")))
   '(org-todo-keyword-faces
     '(;; ("TODO" . org-warning)
       ;; ("DOING" . (:foreground "orange" :underline t :weight bold))
       ;; ("BLOCKED" . "firebrick1")
       ;; ("DONE" . "green")
       ;; ("CANCEL" . "SteelBlue")
       ;; ("NO_ACTION" . "brown1")
       ("TODO" . org-todo)
       ("NEXT" . "red")
       ("DOING" . (:foreground "orange" :underline t :weight bold))
       ("DONE" . "green")
       ("WAITING" . "firebrick1")
       ("SOMEDAY" . "brown1")
       ("CANCELED" . "SteelBlue")
       ("BREAK" . "grey")
       ))
   '(org-global-properties '(("Effort_ALL" . "0:30 1:00 1:30 2:00 2:30 3:00 3:30 4:00 4:30 0:10")))

   ;;'(org-startup-folded nil) ;; default: t
   ;;'(org-yank-adjusted-subtrees t) ;; default: nil
   ;;; -> https://orgmode.org/manual/Column-attributes.html#Column-attributes
   '(org-columns-default-format "%60ITEM %TODO %3PRIORITY %8EFFORT(Estimate){:} %8CLOCKSUM(Total){:} %8CLOCKSUM_T(Today){:}") ;; default: "%25ITEM %TODO %3PRIORITY %TAGS"
   ;;'(org-columns-default-format "%25ITEM %TODO %3PRIORITY %CLOCKSUM %EFFORT %SCHEDULED %DEADLINE")
   '(org-agenda-columns-add-appointments-to-effort-sum t)
   '(org-agenda-span 'day) ;; default: 'week (change by `d'/`w' or `v')
   '(org-agenda-include-deadlines nil) ;; default: t (toggle by `!')
   '(org-agenda-todo-ignore-scheduled 'future) ;; default: nil (toggle by `@' key by my function)
   '(org-agenda-start-with-log-mode t) ;; default: nil
   '(org-agenda-log-mode-items '(state)) ;; default: '(closed clock)
   '(org-clock-in-switch-to-state "DOING")
   '(org-clock-out-switch-to-state "BREAK")
   '(org-stuck-projects '("+LEVEL=2/-DONE" ("TODO" "NEXT" "DOING" "BREAK" "WAITING") nil ""))
   '(org-refile-targets (mapcar (lambda (category)
                                  (cons (concat my/org-agenda-root category ".org") '(:level . 1)))
                                my/org-agenda-category-list))
   '(system-time-locale "C") ;; default: nil
   '(org-enforce-todo-dependencies t) ;; default: nil, if want to change force, use C-u C-u C-u C-c C-t
   '(org-archive-reversed-order t) ;; default: nil
   ;;'(org-archive-default-command 'org-archive-to-archive-sibling) ;; default: 'org-archive-subtree
   '(org-log-done 'time)
   '(org-log-into-drawer t) ;; https://orgmode.org/manual/Tracking-TODO-state-changes.html
   '(org-clock-into-drawer "CLOCKLOG") ;; https://orgmode.org/manual/Clocking-commands.html

   ;;'(org-log-refile t) ;; default: nil
   '(org-tags-column -120) ;; default: -77
   '(org-agenda-tags-column -120) ;; default: 'auto
   '(org-tag-alist '(("@1st" . ?1) ("@Zone" . ?z) ("@Break" . ?b) ("@Pocket" . ?p)))
   '(org-reverse-note-order t) ;; default: nil
   '(org-capture-templates
     `(("a" "Business #A" entry (file+headline ,(concat my/org-agenda-root my/org-agenda-category-business ".org") "Inbox")
        "** TODO [#A] %? %^G\n" :jump-to-captured t)
       ("b" "Business #B" entry (file+headline ,(concat my/org-agenda-root my/org-agenda-category-business ".org") "Inbox")
        "** TODO [#B] %? %^G\n" :jump-to-captured t)
       ("c" "Business #C" entry (file+headline ,(concat my/org-agenda-root my/org-agenda-category-business ".org") "Inbox")
        "** TODO [#C] %? %^G\n")
       ("d" "Business #D" entry (file+headline ,(concat my/org-agenda-root my/org-agenda-category-business ".org") "Inbox")
        "** TODO [#D] %? %^G\n")
       ("e" "Private #E" entry (file+headline ,(concat my/org-agenda-root my/org-agenda-category-private ".org") "Private Tasks")
        "** TODO [#E] %? \n")
       ("f" "Future #F" entry (file+headline ,(concat my/org-agenda-root my/org-agenda-category-private ".org") "Future Tasks")
        "** SOMEDAY [#F] %? \n")
       ("s" "Schedule" entry (file+headline org-default-notes-file "Scheduled Tasks")
        "** TODO %? %^G\n   SCHEDULED: %^t %^G\n" :jump-to-captured t)
       ("t" "TODO" entry (file+headline org-default-notes-file "Tasks")
        "** TODO %? \n")
       ("[" "Checklist" checkitem (file+headline org-default-notes-file "Todo")
        "- [ ] %? \n")
       ))
   ;; https://qiita.com/takaxp/items/a5a3383d7358c58240d0
   '(org-use-speed-commands t)
   ;; http://zhongweiy.github.io/blog/2016/02/03/solve-error-emacs-not-compiled-with-dbus-support/
   '(org-show-notification-handler (lambda (msg) msg))
   )

  :config
  (autoload 'org-time-stamp "org" nil t)

  ;; word boundary (not treat `-' as word boundary)
  (modify-syntax-entry ?- "w" org-mode-syntax-table)

  ;; function of org-open-at-point
  (setf (cdr (assoc 'file org-link-frame-setup)) #'find-file)

  ;; org-id
  (use-package org-id
    :config
    (custom-set-variables
     '(org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
     )) ;; org-id

  ;; smartrep
  (use-package smartrep
    :ensure t
    :config
    (smartrep-define-key
        org-mode-map "C-c" '(;;("C-h" . 'org-metaleft)
                             ;;("C-j" . 'org-metadown)
                             ;;("C-k" . 'org-metaup)
                             ;;("C-l" . 'org-metaright)
                             ("H" . 'org-shiftleft)
                             ("J" . 'org-shiftdown)
                             ("K" . 'org-shiftup)
                             ("L" . 'org-shiftright)
                             )))

  ;; https://qiita.com/takaxp/items/a5a3383d7358c58240d0
  (dolist (item '(("h" org-speed-move-safe 'org-cycle)
                  ("j" org-speed-move-safe 'org-next-visible-heading) ;; default: org-goto
                  ("k" org-speed-move-safe 'org-previous-visible-heading)
                  ("l" org-speed-move-safe 'org-show-subtree)   ;; default: org-shiftleft
                  ("H" org-speed-move-safe 'org-shiftmetaleft)
                  ("J" org-speed-move-safe 'org-metadown)
                  ("K" org-speed-move-safe 'org-metaup)
                  ("L" org-speed-move-safe 'org-shiftmetaright) ;; default: org-shiftmetaleft
                  ("q" widen)                                   ;; default: C-x n w
                  ("A" org-force-cycle-archived)                ;; default: C-Tab
                  ("$" org-archive-subtree-default-with-confirmation) ;; default: C-c C-x C-a
                  ))
    (add-to-list 'org-speed-commands-user item))

  ;; Emacs tech bible ch.14.2 by rubikitch
  (defun my/org-insert-upheading (org)
    (interactive "P")
    (org-insert-heading arg)
    (cond ((org-on-heading-p) (org-do-promote))
          ((org-at-item-p) (org-indent-item -1))))

  (defun my/org-insert-heading-dwim (arg)
    (interactive "p")
    (case arg
      (4  (org-insert-subheading nil))  ; C-u
      (16 (my/org-insert-upheading  nil))  ; C-u C-u
      ;;(t  (org-insert-heading    nil))
      (t  (org-insert-heading-respect-content    nil))))
  ) ;; org-mode


;;; org-colview
(use-package org-colview
  :bind (:map org-columns-map
              ("e" . org-agenda-set-effort) ;; default: org-columns-edit-value
              ("r" . org-columns-edit-value) ;; default: org-columns-redo (equals `g')
              ("g" . org-agenda-columns)    ;; default: org-columns-redo
              )
  ) ;; org-colview


;;; org-agenda
(use-package org-agenda
  :bind (("M-q" . my/org-agenda-all-todo-one-window)
         :map org-agenda-mode-map
         ("C-c C-c" . org-agenda-set-tags)  ;; default `:', `C-c C-q`
         ("M-q" . org-agenda-redo-all)
         ("j" . org-agenda-next-item) ;; default: org-agenda-goto-date
         ("k" . org-agenda-previous-item) ;; default: org-agenda-capture
         ("h" . my/org-agenda-todo-state-change-left) ;; default: org-agenda-holidays
         ("l" . my/org-agenda-todo-state-change-right) ;; default: org-agenda-log-mode
         ("L" . org-agenda-log-mode) ;; default: org-agenda-recenter
         ("c" . org-agenda-capture) ;; default: org-agenda-goto-calendar
         ("S" . my/org-agenda-postpone-schedule-to-tomorrow) ;; default: org-agenda-sunrise-sunset
         ("@" . my/org-agenda-toggle-future-tasks)
         ("`" . my/org-agenda-toggle-category)
         ("M-j" . org-agenda-priority-down) ;; default: `-'
         ("M-k" . org-agenda-priority-up) ;; default: `+'
         ("\\" . my/org-agenda-columns-toggle) ;; org-agenda-columns: C-c C-x C-c
         ("M-\\" . my/org-agenda-toggle-org-columns-default-format)
         ("M-/" . my/org-agenda-toggle-tag-filter))

  :init
  (defvar my/org-agenda-root "~/Dropbox/org/agenda/")
  (defvar my/org-agenda-category-business "business")
  (defvar my/org-agenda-category-private "private")
  (defvar my/org-agenda-category-list (list my/org-agenda-category-business my/org-agenda-category-private))
  (defvar my/org-agenda-category-index -1)
  (defvar my/org-agenda-series '("DOING" "BREAK" "NEXT" "TODO" "WAITING" "DONE"))
  (defvar my/org-agenda-toggle-columns '(" %8EFFORT(Estimate){:}" " %8CLOCKSUM(Total){:}" "%8CLOCKSUM_T(Today){:}" " %SCHEDULED(Plan)" " %DEADLINE(Due)"))
  (defvar my/org-agenda-tag-filter-list '("@1st" "@Zone" "@Pocket" "@Break"))
  (defvar my/org-agenda-tag-filter-index -1)
  (setq org-habit-graph-column 80)  ;; default: 40
  (setq org-habit-preceding-days 7) ;; default: 21
  (setq org-habit-following-days 7) ;; default: 7

  :config
  (require 'org-habit)
  ;;(add-to-list 'org-speed-commands-user '("i" my/org-pomodoro)) ;; default: (progn (forward-char 1) (call-interactively 'org-insert-heading-respect-content))

  ;; with-eval-after-load 'org-agenda
  (with-eval-after-load 'org-agenda
    (define-key org-agenda-mode-map (kbd "i") 'my/org-pomodoro) ;; default: org-agenda-diary-entry

    ;; prevent current line disappear and keep cursor position on changing effort within column view
    (defadvice org-agenda-set-effort (after my/org-agenda-set-effort-redo activate)
      (if org-agenda-columns-active
          (let ((pos (point)))
            (org-columns-redo)
            (goto-char pos))))
    (defadvice org-agenda-priority-up (after my/org-agenda-priority-up-redo activate)
      (if org-agenda-columns-active
          (let ((pos (point)))
            (org-columns-redo)
            (goto-char pos))))
    (defadvice org-agenda-priority-down (after my/org-agenda-priority-down-redo activate)
      (if org-agenda-columns-active
          (let ((pos (point)))
            (org-columns-redo)
            (goto-char pos))))
    (defadvice org-agenda-todo (after my/org-agenda-todo-redo activate)
      (if org-agenda-columns-active
          (let ((pos (point)))
            (org-columns-redo)
            (goto-char pos)
            (beginning-of-line))))

    ) ;; with-eval-after-load 'org-agenda

  (defun my/org-agenda-todo-state-change-right ()
    (interactive)
    (org-agenda-todo 'right))

  (defun my/org-agenda-todo-state-change-left ()
    (interactive)
    (org-agenda-todo 'left))

  (defun my/org-agenda-postpone-schedule-to-tomorrow ()
    (interactive)
    (org-agenda-schedule nil "+1d"))

  (defun my/org-agenda-toggle-future-tasks ()
    (interactive)
    (setq org-agenda-todo-ignore-scheduled
          (if org-agenda-todo-ignore-scheduled nil 'future))
    (org-agenda-redo-all)
    (message (format "Future tasks inclusion %s" (if org-agenda-todo-ignore-scheduled "off" "on"))))

  (defun my/org-agenda-columns-toggle ()
    (interactive)
    (if org-agenda-columns-active
        (org-columns-quit)
      (org-agenda-columns)))

  (defun my/org-agenda-toggle-org-columns-default-format ()
    (interactive)
    (dolist (item my/org-agenda-toggle-columns)
      (if (string-match item org-columns-default-format)
          (setq org-columns-default-format (replace-regexp-in-string item "" org-columns-default-format))
        (setq org-columns-default-format (concat org-columns-default-format item))))
    (org-agenda-columns))

  (defun my/org-agenda-change-category (&optional category-index)
    (interactive)
    (let ((index (or category-index my/org-agenda-category-index)))
      (cond ((or (< index 0)
                 (<= (length my/org-agenda-category-list) index))
             (setq index -1)
             (org-agenda-filter-show-all-cat)
             (message "Toggle to all categories"))
            (t
             (org-agenda-filter-show-all-cat)
             (let* ((category (nth index my/org-agenda-category-list))
                    (filter (list (concat "+" category))))
               (setq org-agenda-category-filter filter)
               (org-agenda-filter-apply filter 'category)
               (message "Toggle to category: %s" category))))
      (setq my/org-agenda-category-index index)))

  (defun my/org-agenda-toggle-category ()
    (interactive)
    (my/org-agenda-change-category (1+ my/org-agenda-category-index)))

  (defun my/org-agenda-change-tag-filter (&optional tag-filter-index)
    (interactive)
    (let ((index (or tag-filter-index my/org-agenda-tag-filter-index)))
      (cond ((or (< index 0)
                 (<= (length my/org-agenda-tag-filter-list) index))
             (setq index -1)
             (org-agenda-filter-show-all-tag)
             (message "Toggle to show all tags"))
            (t
             (org-agenda-filter-show-all-tag)
             (let* ((tag-filter (nth index my/org-agenda-tag-filter-list))
                    (filter (list (concat "+" tag-filter))))
               (setq org-agenda-tag-filter filter)
               (org-agenda-filter-apply filter 'tag)
               (message "Toggle to tag-filter: %s" tag-filter))))
      (setq my/org-agenda-tag-filter-index index)))

  (defun my/org-agenda-toggle-tag-filter ()
    (interactive)
    (my/org-agenda-change-tag-filter (1+ my/org-agenda-tag-filter-index)))

  (defun my/org-agenda-all-todo-one-window ()
    (interactive)
    (let* ((agenda "*Org Agenda*")
           (buf (get-buffer agenda))
           (win (get-buffer-window agenda)))
      (cond (win
             (select-window win)
             (org-agenda-redo-all))
            (buf
             (delete-other-windows)
             (switch-to-buffer-other-window buf)
             (org-agenda-redo-all))
            (t
             (message "opening org-agenda...")
             (let ((name "Active TODOs and agenda")
                   (series (list (append '((agenda ""))
                                         (mapcar (lambda (label) (list 'todo label)) my/org-agenda-series)
                                         '((stuck))
                                         )))
                   (narrow-window (< (window-width) 100)))
               (org-agenda-run-series name series)
               (if narrow-window
                   (delete-other-windows)))
             (my/org-agenda-change-category my/org-agenda-category-index)))))
  ) ;; org-agenda

  ;;; org-pomodoro
(use-package org-pomodoro
  :after org-agenda
  :ensure t
  :init
  (setq org-pomodoro-format " %s ") ;; default: "Pomodoro~%s"
  (setq org-pomodoro-short-break-format " Short: %s ") ;; default: "Short Break~%s"
  (setq org-pomodoro-long-break-format " Long: %s ") ;; default: "Long Break~%s"
  (setq org-pomodoro-long-break-length 5) ;; default: 20
  (setq org-pomodoro-play-sounds nil)

  :config

  ;; ;; Use OS notification for Pomodoro
  ;; (defun my/oascript-notification (title message)
  ;;   (shell-command (format "osascript -e 'display notification \"%s\" with title \"%s\"'" message title)))
  ;; (defun my/org-pomodoro-started-hook () (my/oascript-notification "org-pomodoro" "Pomodoro Started"))
  ;; (defun my/org-pomodoro-finished-hook () (my/oascript-notification "org-pomodoro" "Pomodoro Finished"))
  ;; (defun my/org-pomodoro-break-finished-hook () (my/oascript-notification "org-pomodoro" "Break Finished"))
  ;; (defun my/org-pomodoro-killed-hook () (my/oascript-notification "org-pomodoro" "Pomodoro Killed"))

  ;; Use JustFocus for Pomodoro
  (defun my/org-pomodoro-launch-app ()
    (let ((async-shell-command-buffer 'rename-buffer))
      (save-window-excursion
        (async-shell-command "afplay -v 0.2 /System/Library/Sounds/Ping.aiff")
        (async-shell-command "open -a /Applications/JustFocus.app/Contents/MacOS/JustFocus")
      )))
  (defun my/org-pomodoro-kill-app ()
    (save-window-excursion
      (shell-command "killall JustFocus")))

  ;; ;; Use Be Focused Pro for Pomodoro
  ;; (defun my/org-pomodoro-launch-app ()
  ;;   (save-window-excursion
  ;;     (async-shell-command "open -a /Applications/Be\\ Focused\\ Pro.app/Contents/MacOS/Be\\ Focused\\ Pro")))
  ;; (defun my/org-pomodoro-kill-app ()
  ;;   (save-window-excursion
  ;;     (shell-command "killall Be\\ Focused\\ Pro")))

  (defun my/org-pomodoro-started-hook ()
    (my/org-pomodoro-kill-app)
    (my/org-pomodoro-launch-app))
  (defun my/org-pomodoro-finished-hook ())
  (defun my/org-pomodoro-break-finished-hook ()
    (org-pomodoro-start :pomodoro)
    (let ((clock (list (car org-clock-history))))
      (if clock
          (org-with-clock-position clock
            (when (string-match "^\\(DOING\\|BREAK\\)" (org-get-heading))
              (org-clock-in-last)
              (org-agenda-redo-all))))))
  (defun my/org-pomodoro-killed-hook ()
    (my/org-pomodoro-kill-app))

  (add-hook 'org-pomodoro-started-hook 'my/org-pomodoro-started-hook)
  (add-hook 'org-pomodoro-finished-hook 'my/org-pomodoro-finished-hook)
  (add-hook 'org-pomodoro-killed-hook 'my/org-pomodoro-killed-hook)

  ;; (add-hook 'org-pomodoro-break-finished-hook 'my/org-pomodoro-break-finished-hook) ;; auto-restart
  ;; (add-hook 'org-pomodoro-break-finished-hook 'my/org-pomodoro-kill-app) ;; not auto-restart

  (defun my/org-pomodoro (arg)
    (interactive "p")
    (if (> arg 1)
        (progn
          (let ((current-prefix-arg (/ arg 4)))
            ;; not auto-restart
            (remove-hook 'org-pomodoro-break-finished-hook 'my/org-pomodoro-break-finished-hook)
            (add-hook 'org-pomodoro-break-finished-hook 'my/org-pomodoro-kill-app)
            (org-pomodoro (list current-prefix-arg))))
      ;; auto-restart
      (remove-hook 'org-pomodoro-break-finished-hook 'my/org-pomodoro-kill-app)
      (add-hook 'org-pomodoro-break-finished-hook 'my/org-pomodoro-break-finished-hook)
      (org-pomodoro (list arg)))
    (org-agenda-redo-all))

  (defadvice org-pomodoro-set (around org-pomodoro-set-lazy (state) activate)
    "Set the org-pomodoro STATE."
    (setq org-pomodoro-state state
          org-pomodoro-end-time
          (cl-case state
            (:pomodoro (time-add (current-time) (* 60 org-pomodoro-length)))
            (:short-break (time-add (current-time) (* 60 org-pomodoro-short-break-length)))
            (:long-break (time-add (current-time) (* 60 org-pomodoro-long-break-length))))
          org-pomodoro-timer (run-with-timer t 10 'org-pomodoro-tick)))

  ;; shorten Org-Agenda mode-line for narrow window
  (defadvice org-agenda-set-mode-name (around my/org-agenda-set-mode-name activate)
    (if (not (org-pomodoro-active-p))
        ad-do-it
      (setq mode-name '("Org-Agenda"))
      (force-mode-line-update)))

  ) ;; org-pomodoro


;;; ox-hugo
;; -> https://ox-hugo.scripter.co/
(use-package ox-hugo
  :ensure t
  :after ox)

;;; org-capture
;; -> https://ox-hugo.scripter.co/doc/org-capture-setup/
;; Populates only the EXPORT_FILE_NAME property in the inserted headline.
(use-package org-capture
  :bind ("C-c c" . org-capture)
  :config
  (defun org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
    (let* (;; http://www.holgerschurig.de/en/emacs-blog-from-org-to-hugo/
           (section (format-time-string "%Y/%m" (org-current-time)))
           ;;(date (format-time-string (org-time-stamp-format :long :inactive) (org-current-time)))
           (date (format-time-string "%Y-%m-%d" (org-current-time)))
           (year (format-time-string "%Y" (org-current-time)))
           (month (format-time-string "%m" (org-current-time)))
           (title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
           (fname (org-hugo-slug title)))
      (mapconcat #'identity
                 `(
                   ,(concat "* TODO " title)
                   ":PROPERTIES:"
                   ,(concat ":EXPORT_FILE_NAME: " fname)
                   ,(concat ":EXPORT_HUGO_SECTION*: " section) ;;;+ Append
                   ,(concat ":EXPORT_DATE: " date) ;Enter current date and time
                   ,(concat ":ATTACH_DIR: " (format "static/img/%s/%s" year month))
                   ,(concat ":EXPORT_HUGO_CUSTOM_FRONT_MATTER: :archives " (format "'(\"%s\")" year)) ;;;+ Append
                   ,(concat ":EXPORT_HUGO_CUSTOM_FRONT_MATTER+: :thumbnail \"img/common/emacs.png\"" ) ;;;+ Append
                   ,(concat ":EXPORT_HUGO_CUSTOM_FRONT_MATTER+: :toc true") ;;;+ Append
                   ":END:"
                   "%?\n")                ;Place the cursor here finally
                 "\n")))

  (add-to-list 'org-capture-templates
               '("h"                ;`org-capture' binding + h
                 "Hugo post"
                 entry
                 ;; It is assumed that below file is present in `org-directory'
                 ;; and that it has a "Blog Ideas" heading. It can even be a
                 ;; symlink pointing to the actual location of all-posts.org!
                 (file+olp "blog.org" "Blog Ideas")
                 (function org-hugo-new-subtree-post-capture-template)))
  ) ;; org-capture
