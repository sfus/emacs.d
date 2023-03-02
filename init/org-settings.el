;;; org-mode
(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         :map org-mode-map
         ("M-," . org-mark-ring-goto)
         ("C-c t" . org-toggle-link-display)
         ("C-c s" . org-sort) ;; default: C-c ^
         ;;("C-M-<return>" . org-insert-todo-heading)
         ("C-c <tab>" . outline-show-all)
         ;;("C-<return>" . org-insert-subheading) ;; default: org-insert-heading-respect-content
         ("C-m" . org-return-indent) ;; default: C-j
         ("C-j" . org-return) ;; default: C-m
         ("C-c i" . org-indent-mode)
         ("C-c I" . org-attach-screenshot)
         ("C-M-j" . my/org-insert-heading-dwim)
         ;;("M-j" . org-insert-todo-heading)
         ("M-j" . org-next-visible-heading) ;; default: C-c C-n
         ("M-k" . org-previous-visible-heading) ;; default: C-c C-p (kill-sentence)
         ("M-p" . org-metaup) ;; default: M-<up>
         ("M-n" . org-metadown) ;; default: M-<down>
         ("C-M-f" . org-metaright) ;; default: M-<right>
         ("C-M-b" . org-metaleft) ;; default: M-<right>
         ("C-c C-x M-i" . my/org-pomodoro)
         )
  :init
  ;; migration for org 9.5 or later
  (unless (boundp 'org-speed-commands-user)
    (defvaralias 'org-speed-commands-user 'org-speed-commands))
  (defvar my/org-agenda-root "~/Dropbox/org/agenda/")
  (defvar my/org-agenda-category-business "business")
  (defvar my/org-agenda-category-private "private")
  (defvar my/org-agenda-category-list (list my/org-agenda-category-business my/org-agenda-category-private))

  (custom-set-variables
   '(org-startup-truncated nil)
   ;;'(org-startup-indented t) ;; default: nil
   ;;'(org-directory (expand-file-name "~/Dropbox/")) ;; default: "~/org"
   ;;'(org-yank-folded-subtrees nil) ;; default: t
   '(org-adapt-indentation t) ;; defalt: nil ;; https://orgmode.org/manual/Hard-indentation.html
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
       ("NEXT" . "hotpink")
       ("DOING" . (:foreground "orange" :underline t :weight bold))
       ("DONE" . "palegreen")
       ("WAITING" . "lightcoral")
       ("SOMEDAY" . "brown1")
       ("CANCELED" . "SteelBlue")
       ("BREAK" . "grey")
       ))
   '(org-use-fast-todo-selection 'expert)
   '(org-priority-faces '((?A . org-warning)
                          (?B . org-todo)
                          (?C . org-priority)
                          (?D . org-default)
                          (?E . org-agenda-dimmed-todo-face)
                          (?F . org-hide)
                          ;; (?C . (:foreground "yellow" :weight 'bold))
                          ))
   '(org-global-properties '(("Effort_ALL" . "0:30 1:00 1:30 2:00 2:30 3:00 3:30 4:00 4:30 0:10")))

   '(org-startup-folded 'content) ;; default: showeverything
   ;;'(org-yank-adjusted-subtrees t) ;; default: nil
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
   '(org-clock-out-remove-zero-time-clocks t)

   ;;'(org-log-refile t) ;; default: nil
   '(org-tags-column -120) ;; default: -77
   '(org-tag-alist '(("@1st" . ?1) ("@zone" . ?z) ("@break" . ?b) ("@pocket" . ?p)))
   '(org-reverse-note-order t) ;; default: nil
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
                             ;;("H" . 'org-shiftleft)
                             ;;("J" . 'org-shiftdown)
                             ;;("K" . 'org-shiftup)
                             ;;("L" . 'org-shiftright)
                             )))

  ;; https://qiita.com/takaxp/items/a5a3383d7358c58240d0
  (defun my/org-todo-cancel-repeat (&optional ARG)
    (when (org-get-repeat)
      (org-cancel-repeater))
    (org-todo ARG))

  (defun my/org-clock-out-and-widen ()
    (interactive)
    (when (org-clocking-p)
      (org-clock-out)
      (let* ((agenda "*Org Agenda*")
             (win (get-buffer-window agenda)))
        (when win
          (save-window-excursion
            (select-window win)
            (org-agenda-redo-all))
          )))
    (widen))

  (dolist (item '(("h" org-speed-move-safe 'org-cycle)
                  ("j" org-speed-move-safe 'org-next-visible-heading) ;; default: org-goto
                  ("k" org-speed-move-safe 'org-previous-visible-heading)
                  ("l" org-speed-move-safe 'org-show-subtree)   ;; default: org-shiftleft
                  ;;("H" org-speed-move-safe 'org-shiftmetaleft)
                  ;;("J" org-speed-move-safe 'org-metadown)
                  ;;("K" org-speed-move-safe 'org-metaup)
                  ;;("L" org-speed-move-safe 'org-shiftmetaright) ;; default: org-shiftmetaleft
                  ;;("q" my/org-clock-out-and-widen)              ;; default: C-x n w
                  ("q" widen)
                  ("A" org-force-cycle-archived)                ;; default: C-Tab
                  ("$" org-archive-subtree-default-with-confirmation) ;; default: C-c C-x C-a
                  ("D" my/org-todo-cancel-repeat "DONE")
                  ))
    (add-to-list 'org-speed-commands-user item))

  ;; ;; type `?' to show speed commands.
  ;; org-speed-commands-user
  ;; (("Outline Navigation")
  ;;  ("n" org-speed-move-safe 'org-next-visible-heading)
  ;;  ("p" org-speed-move-safe 'org-previous-visible-heading)
  ;;  ("f" org-speed-move-safe 'org-forward-heading-same-level)
  ;;  ("b" org-speed-move-safe 'org-backward-heading-same-level)
  ;;  ("F" . org-next-block)
  ;;  ("B" . org-previous-block)
  ;;  ("u" org-speed-move-safe 'outline-up-heading)
  ;;  ("j" . org-goto)
  ;;  ("g" org-refile t)
  ;;  ("Outline Visibility")
  ;;  ("c" . org-cycle)
  ;;  ("C" . org-shifttab)
  ;;  (" " . org-display-outline-path)
  ;;  ("s" . org-narrow-to-subtree)
  ;;  ("=" . org-columns)
  ;;  ("Outline Structure Editing")
  ;;  ("U" . org-metaup)
  ;;  ("D" . org-metadown)
  ;;  ("r" . org-metaright)
  ;;  ("l" . org-metaleft)
  ;;  ("R" . org-shiftmetaright)
  ;;  ("L" . org-shiftmetaleft)
  ;;  ("i" progn
  ;;   (forward-char 1)
  ;;   (call-interactively 'org-insert-heading-respect-content))
  ;;  ("^" . org-sort)
  ;;  ("w" . org-refile)
  ;;  ("a" . org-archive-subtree-default-with-confirmation)
  ;;  ("@" . org-mark-subtree)
  ;;  ("#" . org-toggle-comment)
  ;;  ("Clock Commands")
  ;;  ("I" . org-clock-in)
  ;;  ("O" . org-clock-out)
  ;;  ("Meta Data Editing")
  ;;  ("t" . org-todo)
  ;;  ("," org-priority)
  ;;  ("0" org-priority 32)
  ;;  ("1" org-priority 65)
  ;;  ("2" org-priority 66)
  ;;  ("3" org-priority 67)
  ;;  (":" . org-set-tags-command)
  ;;  ("e" . org-set-effort)
  ;;  ("E" . org-inc-effort)
  ;;  ("W" lambda
  ;;   (m)
  ;;   (interactive "sMinutes before warning: ")
  ;;   (org-entry-put
  ;;    (point)
  ;;    "APPT_WARNTIME" m))
  ;;  ("Agenda Views etc")
  ;;  ("v" . org-agenda)
  ;;  ("/" . org-sparse-tree)
  ;;  ("Misc")
  ;;  ("o" . org-open-at-point)
  ;;  ("?" . org-speed-command-help)
  ;;  ("<" org-agenda-set-restriction-lock 'subtree)
  ;;  (">" org-agenda-remove-restriction-lock))

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


  ;; set schedule time
  (defun my/org-agenda-set-schedule-time (time)
    (org-agenda-schedule nil time))
  (defun my/org-agenda-set-schedule-time-09 () (interactive) (my/org-agenda-set-schedule-time "09:30"))
  (defun my/org-agenda-set-schedule-time-10 () (interactive) (my/org-agenda-set-schedule-time "10:00"))
  (defun my/org-agenda-set-schedule-time-11 () (interactive) (my/org-agenda-set-schedule-time "11:00"))
  (defun my/org-agenda-set-schedule-time-12 () (interactive) (my/org-agenda-set-schedule-time "12:00"))
  (defun my/org-agenda-set-schedule-time-13 () (interactive) (my/org-agenda-set-schedule-time "13:00"))
  (defun my/org-agenda-set-schedule-time-14 () (interactive) (my/org-agenda-set-schedule-time "14:00"))
  (defun my/org-agenda-set-schedule-time-15 () (interactive) (my/org-agenda-set-schedule-time "15:00"))
  (defun my/org-agenda-set-schedule-time-16 () (interactive) (my/org-agenda-set-schedule-time "16:00"))
  (defun my/org-agenda-set-schedule-time-17 () (interactive) (my/org-agenda-set-schedule-time "17:00"))
  (defun my/org-agenda-set-schedule-time-18 () (interactive) (my/org-agenda-set-schedule-time "18:00"))

  ;; https://qiita.com/takaxp/items/a5a3383d7358c58240d0
  (dolist (item '(("9" my/org-agenda-set-schedule-time-09)
                  ("0" my/org-agenda-set-schedule-time-10)
                  ("1" my/org-agenda-set-schedule-time-11)
                  ("2" my/org-agenda-set-schedule-time-12)
                  ("3" my/org-agenda-set-schedule-time-13)
                  ("4" my/org-agenda-set-schedule-time-14)
                  ("5" my/org-agenda-set-schedule-time-15)
                  ("6" my/org-agenda-set-schedule-time-16)
                  ("7" my/org-agenda-set-schedule-time-17)
                  ("8" my/org-agenda-set-schedule-time-18)
                  ))
    (add-to-list 'org-speed-commands-user item))

  ) ;; org-mode

;;; org-clock
(use-package org-clock
  :init
  (require 'cl)

  :config
  ;; https://emacs.stackexchange.com/questions/35708/org-mode-how-to-generate-clock-report-for-hours-worked-each-day
  ;;   #+BEGIN: weekly :tstart "<2020-05-11>" :tend "<now>"
  ;;   #+END:
  ;; `C-c C-c'
  (defun org-dblock-write:weekly (params)
    (cl-flet ((fmttm (tm) (format-time-string (org-time-stamp-format t t) tm)))
      (let ((file (or (plist-get params :file) (buffer-file-name)))
            (start (seconds-to-time
                    (org-matcher-time (plist-get params :tstart))))
            (end (seconds-to-time (org-matcher-time (plist-get params :tend)))))
        (while (time-less-p start end)
          (let ((next-week (time-add start
                                     (date-to-time "1970-01-08T00:00Z")))
                (week-begin (line-beginning-position))
                (week-minutes 0))
            (insert "\nWeekly Table from " (fmttm start) "\n")
            (insert "| Day of Week | Time |\n|-\n")
            (while (time-less-p start next-week)
              (let* ((next-day (time-add start (date-to-time "1970-01-02T00:00Z")))
                     (minutes
                      (with-current-buffer (find-file-noselect file)
                        (cadr (org-clock-get-table-data
                               file
                               (list :maxlevel 0
                                     :tstart (fmttm start)
                                     :tend (fmttm next-day)))))))
                (insert "|" (format-time-string "%a" start)
                        "|" (format "%d" minutes)
                        "|\n")
                (org-table-align)
                (incf week-minutes minutes)
                (setq start next-day)))
            (when (equal week-minutes 0)
              (delete-region week-begin (line-beginning-position))))))))
  ) ;; org-clock

;;; org-colview
(use-package org-colview
  :bind (:map org-columns-map
              ("c" . org-capture)
              ("e" . org-agenda-set-effort) ;; default: org-columns-edit-value
              ("C-m" . org-columns-edit-value) ;; default: org-agenda-switch-to
              ("g" . org-agenda-columns)    ;; default: org-columns-redo
              ("l" . forward-char)
              ("h" . backward-char)
              ("9" . my/org-agenda-set-schedule-time-09)
              ("0" . my/org-agenda-set-schedule-time-10)
              ("1" . my/org-agenda-set-schedule-time-11)
              ("2" . my/org-agenda-set-schedule-time-12)
              ("3" . my/org-agenda-set-schedule-time-13)
              ("4" . my/org-agenda-set-schedule-time-14)
              ("5" . my/org-agenda-set-schedule-time-15)
              ("6" . my/org-agenda-set-schedule-time-16)
              ("7" . my/org-agenda-set-schedule-time-17)
              ("8" . my/org-agenda-set-schedule-time-18)
              )
  ) ;; org-colview


;;; org-agenda
(use-package org-agenda
  :bind (("M-q" . my/org-agenda-all-todo-one-window)
         :map org-agenda-mode-map
         ("C-c C-c" . org-agenda-set-tags)  ;; default `:', `C-c C-q`
         ("M-q" . org-agenda-redo-all)
         ("i" . my/org-agenda-goto-and-clock-in)
         ("j" . org-agenda-next-item) ;; default: org-agenda-goto-date
         ("k" . org-agenda-previous-item) ;; default: org-agenda-capture
         ("h" . my/org-agenda-todo-state-change-left) ;; default: org-agenda-holidays
         ("l" . my/org-agenda-todo-state-change-right) ;; default: org-agenda-log-mode
         ("L" . org-agenda-log-mode) ;; default: org-agenda-recenter
         ("c" . org-agenda-capture) ;; default: org-agenda-goto-calendar
         ("S" . org-agenda-schedule) ;; default: org-agenda-sunrise-sunset
         ("D" . org-agenda-deadline) ;; default: org-agenda-toggle-diary
         ("P" . my/org-agenda-postpone-schedule-to-tomorrow) ;; default: org-agenda-next-item
         ("@" . my/org-agenda-toggle-future-tasks)
         ("`" . my/org-agenda-toggle-category)
         ("M-j" . org-agenda-priority-down) ;; default: `-'
         ("M-k" . org-agenda-priority-up) ;; default: `+'
         ("\\" . my/org-agenda-columns-toggle) ;; org-agenda-columns: C-c C-x C-c
         ("M-\\" . my/org-agenda-toggle-org-columns-default-format)
         ("M-/" . my/org-agenda-toggle-tag-filter)
         ("9" . my/org-agenda-set-schedule-time-09)
         ("0" . my/org-agenda-set-schedule-time-10)
         ("1" . my/org-agenda-set-schedule-time-11)
         ("2" . my/org-agenda-set-schedule-time-12)
         ("3" . my/org-agenda-set-schedule-time-13)
         ("4" . my/org-agenda-set-schedule-time-14)
         ("5" . my/org-agenda-set-schedule-time-15)
         ("6" . my/org-agenda-set-schedule-time-16)
         ("7" . my/org-agenda-set-schedule-time-17)
         ("8" . my/org-agenda-set-schedule-time-18)
         )

  :init
  (defvar my/org-agenda-category-index -1)
  (defvar my/org-agenda-series '("DOING" "BREAK" "NEXT" "TODO" "WAITING" "DONE"))
  (defvar my/org-agenda-toggle-columns '(" %8CLOCKSUM(Total){:}" "%8CLOCKSUM_T(Today){:}" " %22SCHEDULED(Schedule)" " %22DEADLINE(Deadline)"))
  (defvar my/org-agenda-tag-filter-list '("@1st" "@zone" "@pocket" "@break"))
  (defvar my/org-agenda-tag-filter-index -1)

  (custom-set-variables
   ;;'(org-agenda-files (list "~/Tasks/"))
   '(org-agenda-files (list my/org-agenda-root))
   '(org-agenda-tags-column -120) ;; default: 'auto
   '(org-agenda-start-with-clockreport-mode t) ;; default: nil, toggled by `R'
   '(org-agenda-clockreport-parameter-plist '(:link t :maxlevel 2 :fileskip0 t :narrow 60 :formula %)) ;; default: :link t :maxlevel 2
   ;; org-clocktable-defaults
   ;; (:maxlevel 2 :lang "en" :scope file :block nil :wstart 1 :mstart 1 :tstart nil :tend nil :step nil
   ;;  :stepskip0 nil :fileskip0 nil :tags nil :emphasize nil :link nil :narrow 40! :indent t :formula nil
   ;;  :timestamp nil :level nil :tcolumns nil :formatter nil)
   '(org-agenda-span 'day) ;; default: 'week (change by `d'/`w' or `v')
   ;; '(org-agenda-include-deadlines nil) ;; default: t (toggle by `!')
   '(org-agenda-todo-ignore-scheduled 'future) ;; default: nil (toggle by `@' key by my function)
   '(org-agenda-columns-add-appointments-to-effort-sum t)
   ;;'(org-agenda-start-with-log-mode t) ;; default: nil
   '(org-agenda-log-mode-items '(state)) ;; default: '(closed clock)
   ;;; -> https://orgmode.org/manual/Column-attributes.html#Column-attributes
   ;;'(org-columns-default-format "%25ITEM %TODO %3PRIORITY %CLOCKSUM %EFFORT %SCHEDULED %DEADLINE")
   ;;'(org-columns-default-format "%60ITEM %TODO %3PRIORITY %8EFFORT(Estimate){:} %8CLOCKSUM(Total){:} %8CLOCKSUM_T(Today){:}") ;; default: "%25ITEM %TODO %3PRIORITY %TAGS"
   '(org-columns-default-format "%TODO %46ITEM %3PRIORITY %8EFFORT(Estimate){:} %22SCHEDULED(Schedule) %22DEADLINE(Deadline)") ;; default: "%25ITEM %TODO %3PRIORITY %TAGS"
   '(org-habit-graph-column 80)  ;; default: 40
   '(org-habit-preceding-days 7) ;; default: 21
   '(org-habit-following-days 7) ;; default: 7
   '(org-agenda-time-leading-zero t) ;; default: nil
   )

  (add-hook 'org-agenda-mode-hook #'(lambda ()
                                      (setq hl-line-face 'underline)
                                      (hl-line-mode 1)))

  :config
  ;;(require 'org-habit)
  ;;(add-to-list 'org-speed-commands-user '("i" my/org-pomodoro)) ;; default: (progn (forward-char 1) (call-interactively 'org-insert-heading-respect-content))

  ;; with-eval-after-load 'org-agenda
  (with-eval-after-load 'org-agenda
    (define-key org-agenda-mode-map (kbd "M-i") 'my/org-pomodoro)
    (define-key org-mode-map (kbd "M-i") 'my/org-pomodoro)

    ;; prevent current line disappear and keep cursor position on changing effort within column view
    (defun my/org-agenda-org-columns-redo ()
      (if org-agenda-columns-active
          (let ((pos (point)))
            (org-columns-redo)
            (goto-char pos))))
    (advice-add 'org-agenda-set-effort :after #'my/org-agenda-org-columns-redo)
    (advice-add 'org-agenda-priority-up :after #'my/org-agenda-org-columns-redo)
    (advice-add 'org-agenda-priority-down :after #'my/org-agenda-org-columns-redo)

    (defun my/org-agenda-todo-redo ()
      (if org-agenda-columns-active
          (let ((pos (point)))
            (org-columns-redo)
            (goto-char pos)
            (beginning-of-line))))
    (advice-add 'org-agenda-todo :after #'my/org-agenda-todo-redo)


    ;; How to narrow to subtree in org agenda follow mode? - Emacs Stack Exchange
    ;; -> https://emacs.stackexchange.com/questions/17797/how-to-narrow-to-subtree-in-org-agenda-follow-mode
    (advice-add 'org-agenda-goto :after
                (lambda (&rest args)
                  (org-narrow-to-subtree)))

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
             (my/org-agenda-change-category my/org-agenda-category-index)
             (unless org-agenda-follow-mode
               (org-agenda-follow-mode))
             ))))

  (defun my/org-agenda-goto-and-clock-in ()
    (interactive)
    (org-agenda-clock-in)
    (org-agenda-goto)
    (beginning-of-line))

  ;; shorten Org-Agenda mode-line for narrow window
  (defadvice org-agenda-set-mode-name (around my/org-agenda-set-mode-name activate)
    (if (not (org-pomodoro-active-p))
        ad-do-it
      (setq mode-name '("Org-Agenda"))
      (force-mode-line-update)))

  ) ;; org-agenda


;;; org-capture
(use-package org-capture
  :init
  (custom-set-variables
   '(org-capture-templates
     `(("a" "Business #A" entry (file+headline ,(concat my/org-agenda-root my/org-agenda-category-business ".org") "Inbox")
        "** TODO [#A] %? :@zone:\n   SCHEDULED: <%<%Y-%m-%d %a>> DEADLINE: %^t\n" :jump-to-captured t)
       ("b" "Business #B" entry (file+headline ,(concat my/org-agenda-root my/org-agenda-category-business ".org") "Inbox")
        "** TODO [#B] %? :@zone:\n   SCHEDULED: <%<%Y-%m-%d %a>>\n")
       ("c" "Business #C" entry (file+headline ,(concat my/org-agenda-root my/org-agenda-category-business ".org") "Inbox")
        "** TODO [#C] %? :@pocket:\n   SCHEDULED: <%<%Y-%m-%d %a>>\n")
       ("d" "Business #D" entry (file+headline ,(concat my/org-agenda-root my/org-agenda-category-business ".org") "Inbox")
        "** TODO [#D] %? :@break:\n")
       ("e" "Private #E" entry (file+headline ,(concat my/org-agenda-root my/org-agenda-category-private ".org") "Private Tasks")
        "** TODO [#E] %? :@break:\n")
       ("f" "Future #F" entry (file+headline ,(concat my/org-agenda-root my/org-agenda-category-private ".org") "Future Tasks")
        "** SOMEDAY [#F] %?\n")
       ("s" "Schedule" entry (file+headline org-default-notes-file "Scheduled Tasks")
        "** TODO %?\n   SCHEDULED: %^t\n" :jump-to-captured t)
       ("t" "TODO" entry (file+headline org-default-notes-file "Tasks")
        "** TODO %?\n")
       ("[" "Checklist" checkitem (file+headline org-default-notes-file "Todo")
        "- [ ] %? \n")
       ))
   )
  ) ;; org-capture


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
        ;;(async-shell-command "afplay -v 0.2 /System/Library/Sounds/Ping.aiff")
        (async-shell-command "open -a /Applications/JustFocus.app/Contents/MacOS/JustFocus")
      )))
  (defun my/org-pomodoro-kill-app ()
    (save-window-excursion
      (async-shell-command "afplay -v 0.2 /System/Library/Sounds/Purr.aiff")
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
    (cond
     ((eq arg 1)
      (my/org-pomodoro-repeat))
     ((eq arg 4)
      (my/org-pomodoro-no-repeat))
     (t
      (org-pomodoro (list arg)))))

  (defun my/org-pomodoro-repeat ()
    (remove-hook 'org-pomodoro-break-finished-hook 'my/org-pomodoro-kill-app)
    (add-hook 'org-pomodoro-break-finished-hook 'my/org-pomodoro-break-finished-hook)
    (org-pomodoro)
    (org-agenda-redo-all))

  (defun my/org-pomodoro-no-repeat ()
    (remove-hook 'org-pomodoro-break-finished-hook 'my/org-pomodoro-break-finished-hook)
    (add-hook 'org-pomodoro-break-finished-hook 'my/org-pomodoro-kill-app)
    (org-pomodoro)
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

  ) ;; org-pomodoro


;; ;;; org-tree-slide
;; ;; -> https://qiita.com/takaxp/items/8dfb5d34dfcd79f9fa5c
;; ;; -> https://qiita.com/takaxp/items/6b2d1e05e7ce4517274d
;; (use-package org-tree-slide
;;   :ensure t
;;   :bind (("<f8>" . org-tree-slide-mode)
;;          :map org-tree-slide-mode-map
;;          ("<f9>" . org-tree-slide-move-previous-tree)
;;          ("<f10>" . org-tree-slide-move-next-tree))
;;
;;   :config
;;   (org-tree-slide-narrowing-control-profile) ;; ナローイング用基本設定の適用
;;   (setq org-tree-slide-modeline-display 'outside) ;; 高速動作用（推奨）
;;   (setq org-tree-slide-skip-done nil) ;; DONEなタスクも表示する
;;
;;
;;   (when (require 'org-clock nil t)
;;     ;; org-clock-in を拡張
;;     ;; 発動条件1）タスクが DONE になっていないこと（変更可）
;;     ;; 発動条件2）アウトラインレベルが4まで．それ以上に深いレベルでは計測しない（変更可）
;;     (defun my:org-clock-in ()
;;       (setq vc-display-status nil) ;; モードライン節約
;;       (when (and (looking-at (concat "^\\*+ " org-not-done-regexp))
;;                  (memq (org-outline-level) '(1 2 3 4)))
;;         (org-clock-in)))
;;
;;     ;; org-clock-out を拡張
;;     (defun my:org-clock-out ()
;;       ;; (setq vc-display-status t) ;; モードライン節約解除
;;       (when (org-clocking-p)
;;         (org-clock-out)))
;;
;;     ;; org-clock-in をナローイング時に呼び出す．
;;     (add-hook 'org-tree-slide-before-narrow-hook #'my:org-clock-in)
;;
;;     ;; org-clock-out を適切なタイミングで呼び出す．
;;     (add-hook 'org-tree-slide-before-move-next-hook #'my:org-clock-out)
;;     (add-hook 'org-tree-slide-before-move-previous-hook #'my:org-clock-out)
;;     (add-hook 'org-tree-slide-mode-stop-hook #'my:org-clock-out)
;;
;;     ;; 一時的にナローイングを解く時にも計測を止めたい人向け
;;     (add-hook 'org-tree-slide-before-content-view-hook #'my:org-clock-out)
;;
;;     ;; Emacs終了時に org-clock-outし忘れのタスクの時計を止めます
;;     (defun my:org-clock-out-and-save-when-exit ()
;;       "Save buffers and stop clocking when kill emacs."
;;       (when (org-clocking-p)
;;         (org-clock-out)
;;         (save-some-buffers t)))
;;     (add-hook 'kill-emacs-hook #'my:org-clock-out-and-save-when-exit)
;;
;;     )
;;   ) ;; org-tree-slide


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
