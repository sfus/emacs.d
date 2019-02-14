;; org-mode
(global-set-key (kbd "C-x L") 'org-store-link)
;;(global-set-key (kbd "C-x r a") 'org-agenda)

;;;;+ Extra
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(with-eval-after-load 'ox
  (require 'ox-hugo))

(defvar my/org-agenda-root "~/org/agenda/")
(defvar my/org-agenda-category-business "business")
(defvar my/org-agenda-category-private "private")
(defvar my/org-agenda-category-list (list my/org-agenda-category-business my/org-agenda-category-private))
(defvar my/org-agenda-category-index -1)
(defvar my/org-agenda-series '("DOING" "NEXT" "TODO" "WAITING" "DONE"))

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

(defun my/org-agenda-all-todo-one-window ()
  (interactive)
  (let ((name "Active TODOs and agenda")
        (series (list (append '((agenda ""))
                              (mapcar (lambda (label) (list 'todo label)) my/org-agenda-series)
                              '((stuck))
                              )))
        (narrow-window (< (window-width) 100)))
    (org-agenda-run-series name series)
    (if narrow-window
        (delete-other-windows)))
  (my/org-agenda-change-category my/org-agenda-category-index))
(global-set-key (kbd "M-q") 'my/org-agenda-all-todo-one-window)
;;;;+

(autoload 'org-time-stamp "org" nil t)

(custom-set-variables
 '(org-startup-truncated nil)
 ;;'(org-directory (expand-file-name "~/Dropbox/")) ;; default: "~/org"
 ;;'(org-agenda-files (list "~/Tasks/"))
 '(org-agenda-files (list my/org-agenda-root))
 ;;'(org-yank-folded-subtrees nil) ;; default: t
 '(org-return-follows-link t)
 '(org-use-fast-todo-selection t)
 '(org-src-fontify-natively t)
 ;;'(org-default-notes-file (concat org-directory "organizer.org"))
 '(org-default-notes-file  (concat my/org-agenda-root "inbox.org")) ;; default: "~/.notes"
 ;;'(org-default-priority ?A)
 '(org-default-priority ?C) ;; default: ?B
 '(org-lowest-priority ?F) ;; default: ?C
 '(org-imenu-depth 3)
 '(org-todo-keywords
   ;;'((sequence "TODO(t)" "DOING(d)" "|" "DONE(x)" "BLOCKED(b)" "CANCEL(c)" "NO_ACTION(n)"))
   '((sequence "TODO(t)" "NEXT(n)" "DOING(d!)" "|" "DONE(x!)")
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
     ))

 ;;;;+ Extra
 ;;'(org-startup-folded nil) ;; default: t
 ;;'(org-yank-adjusted-subtrees t) ;; default: nil
 '(org-columns-default-format "%25ITEM %TODO %3PRIORITY %CLOCKSUM %EFFORT") ;; default: "%25ITEM %TODO %3PRIORITY %TAGS"
 ;;'(org-columns-default-format "%25ITEM %TODO %3PRIORITY %CLOCKSUM %EFFORT %SCHEDULED %DEADLINE")
 '(org-agenda-span 'day) ;; default: 'week (change by `d'/`w' or `v')
 '(org-agenda-include-deadlines nil) ;; default: t (toggle by `!')
 '(org-agenda-todo-ignore-scheduled 'future) ;; default: nil (toggle by `@' key by my function)
 ;;'(org-agenda-start-with-log-mode t) ;; default: nil
 '(org-clock-in-switch-to-state "DOING")
 '(org-stuck-projects '("+LEVEL=2/-DONE" ("TODO" "NEXT" "DOING" "WAITING") nil ""))
 '(org-refile-targets (mapcar (lambda (category)
                                (cons (concat my/org-agenda-root category ".org") '(:level . 1)))
                              my/org-agenda-category-list))
 '(system-time-locale "C") ;; default: nil
 '(org-enforce-todo-dependencies t) ;; default: nil, if want to change force, use C-u C-u C-u C-c C-t
 '(org-archive-reversed-order t) ;; default: nil
 ;;'(org-archive-default-command 'org-archive-to-archive-sibling) ;; default: 'org-archive-subtree
 '(org-log-done 'time)
 ;;'(org-log-into-drawer t);; https://orgmode.org/manual/Tracking-TODO-state-changes.html
 ;;'(org-log-refile t) ;; default: nil
 '(org-tags-column -120) ;; default: -77
 '(org-reverse-note-order t) ;; default: nil
 '(org-capture-templates
   `(("a" "Business #A" entry (file+headline ,(concat my/org-agenda-root my/org-agenda-category-business ".org") "Inbox")
      "** TODO [#A] %? \n")
     ("b" "Business #B" entry (file+headline ,(concat my/org-agenda-root my/org-agenda-category-business ".org") "Inbox")
      "** TODO [#B] %? \n")
     ("c" "Business #C" entry (file+headline ,(concat my/org-agenda-root my/org-agenda-category-business ".org") "Inbox")
      "** TODO [#C] %? \n")
     ("d" "Business #D" entry (file+headline ,(concat my/org-agenda-root my/org-agenda-category-business ".org") "Inbox")
      "** TODO [#D] %? \n")
     ("e" "Private #E" entry (file+headline ,(concat my/org-agenda-root my/org-agenda-category-private ".org") "Private Tasks")
      "** TODO [#E] %? \n")
     ("f" "Future #F" entry (file+headline ,(concat my/org-agenda-root my/org-agenda-category-private ".org") "Future Tasks")
      "** SOMEDAY [#F] %? \n")
     ("s" "Schedule" entry (file+headline org-default-notes-file "Scheduled Tasks")
      "** TODO %? \n   SCHEDULED: %^t \n")
     ("t" "TODO" entry (file+headline org-default-notes-file "Tasks")
      "** TODO %? \n")
     ("[" "Checklist" checkitem (file+headline org-default-notes-file "Todo")
      "- [ ] %? \n")
     ))
 ;; https://qiita.com/takaxp/items/a5a3383d7358c58240d0
 '(org-use-speed-commands t)
 ;;;;+
 )

(with-eval-after-load 'org
  (org-editutil-setup)
  ;; function of org-open-at-point
  (setf (cdr (assoc 'file org-link-frame-setup)) #'find-file)

  (define-key org-mode-map (kbd "M-,") 'org-mark-ring-goto)
  (define-key org-mode-map (kbd "C-c t") 'org-toggle-link-display)
  (define-key org-mode-map (kbd "C-M-<return>") 'org-insert-todo-heading)
  (define-key org-mode-map (kbd "C-c <tab>") 'outline-show-all)
  ;;;;+ Extra
  (define-key org-mode-map (kbd "C-m") 'org-return-indent) ;; default: C-j
  (define-key org-mode-map (kbd "C-j") 'org-return) ;; default: C-m

  ;; org-agenda
  (defun my/org-agenda-todo-state-change-right ()
    (interactive)
    (org-agenda-todo 'right))

  (defun my/org-agenda-todo-state-change-left ()
    (interactive)
    (org-agenda-todo 'left))

  (defun my/org-agenda-toggle-future-tasks ()
    (interactive)
    (setq org-agenda-todo-ignore-scheduled
          (if org-agenda-todo-ignore-scheduled nil 'future))
    (org-agenda-redo-all)
    (message (format "Future tasks inclusion %s" (if org-agenda-todo-ignore-scheduled "off" "on"))))

  (defun my/org-agenda-toggle-org-columns-default-format ()
    (interactive)
    (dolist (item '(" %CLOCKSUM" " %EFFORT" " %SCHEDULED" " %DEADLINE"))
     (if (string-match item org-columns-default-format)
         (setq org-columns-default-format (replace-regexp-in-string item "" org-columns-default-format))
       (setq org-columns-default-format (concat org-columns-default-format item))))
    (org-columns-redo))

  (with-eval-after-load 'org-agenda
    (define-key org-agenda-mode-map (kbd "j") 'org-agenda-next-item) ;; default: org-agenda-goto-date
    (define-key org-agenda-mode-map (kbd "k") 'org-agenda-previous-item) ;; default: org-agenda-capture
    (define-key org-agenda-mode-map (kbd "h") 'my/org-agenda-todo-state-change-left) ;; default: org-agenda-holidays
    (define-key org-agenda-mode-map (kbd "l") 'my/org-agenda-todo-state-change-right) ;; default: org-agenda-log-mode
    (define-key org-agenda-mode-map (kbd "L") 'org-agenda-log-mode) ;; default: org-agenda-recenter
    (define-key org-agenda-mode-map (kbd "c") 'org-agenda-capture) ;; default: org-agenda-goto-calendar
    (define-key org-agenda-mode-map (kbd "@") 'my/org-agenda-toggle-future-tasks)
    (define-key org-agenda-mode-map (kbd "`") 'my/org-agenda-toggle-category)
    (define-key org-agenda-mode-map (kbd "\\") 'org-agenda-columns) ;; default: C-c C-x C-c
    (define-key org-agenda-mode-map (kbd "M-\\") 'my/org-agenda-toggle-org-columns-default-format)
    ) ;; org-agenda

  (with-eval-after-load 'org-colview
    (define-key org-columns-map (kbd "e") 'org-agenda-set-effort) ;; default: org-columns-edit-value
    (define-key org-columns-map (kbd "r") 'org-columns-edit-value) ;; default: org-columns-redo (equals `g')
    ) ;; org-colview

  ;;;;+

  ;; (smartrep-define-key
  ;;     org-mode-map "C-c" '(("l" . 'org-shiftright)
  ;;                          ("h" . 'org-shiftleft)))

  ;; (smartrep-define-key
  ;;     org-mode-map "C-c" '(("j" . 'org-metadown)
  ;;                          ("k" . 'org-metaup)))

  ;;;;+ Extra
  (smartrep-define-key
      org-mode-map "C-c" '(("h" . 'org-metaleft)
                           ("j" . 'org-metadown)
                           ("k" . 'org-metaup)
                           ("l" . 'org-metaright)
                           ("H" . 'org-shiftleft)
                           ("J" . 'org-shiftdown)
                           ("K" . 'org-shiftup)
                           ("L" . 'org-shiftright)
                           ))

  ;;(require 'org-attach-screenshot)
  (define-key org-mode-map (kbd "C-c i") 'org-attach-screenshot)

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
  (defun org-insert-upheading (org)
    (interactive "P")
    (org-insert-heading arg)
    (cond ((org-on-heading-p) (org-do-promote))
          ((org-at-item-p) (org-indent-item -1))))
  (defun org-insert-heading-dwim (arg)
    (interactive "p")
    (case arg
      (4  (org-insert-subheading nil))  ; C-u
      (16 (org-insert-upheading  nil))  ; C-u C-u
      ;;(t  (org-insert-heading    nil))
      (t  (org-insert-heading-respect-content    nil))))
  ;;(define-key org-mode-map (kbd "<C-return>") 'org-insert-heading-dwim)
  (define-key org-mode-map (kbd "M-j") 'org-insert-heading-dwim)

  ;; others
  (define-key org-mode-map (kbd "C-M-j") 'org-insert-todo-heading)

  (require 'org-habit)

  (when (require 'org-pomodoro nil t)
    (custom-set-variables
     '(org-pomodoro-format " %s ") ;; default: "Pomodoro~%s"
     '(org-pomodoro-short-break-format " Short: %s ") ;; default: "Short Break~%s"
     '(org-pomodoro-long-break-format " Long: %s ") ;; default: "Long Break~%s"
     '(org-pomodoro-long-break-length 5) ;; default: 20
     '(org-pomodoro-play-sounds nil)
     )
    (custom-set-faces
     '(org-pomodoro-mode-line ((t (:bold t :foreground "#FFFFFF" :background "#FC42A0")))) ;; default: (:foreground "tomato1")
     '(org-pomodoro-mode-line-break ((t (:bold t :foreground "#FFFFFF" :background "#17B005")))) ;; default: (:foreground "#2aa198")
     )
    (add-to-list 'org-speed-commands-user '("i" org-pomodoro)) ;; default: (progn (forward-char 1) (call-interactively 'org-insert-heading-respect-content))

    ;; (defun my/oascript-notification (title message)
    ;;   (shell-command (format "osascript -e 'display notification \"%s\" with title \"%s\"'" message title)))
    ;; (defun my/org-pomodoro-started-hook () (my/oascript-notification "org-pomodoro" "Pomodoro Started"))
    ;; (defun my/org-pomodoro-finished-hook () (my/oascript-notification "org-pomodoro" "Pomodoro Finished"))
    ;; (defun my/org-pomodoro-break-finished-hook () (my/oascript-notification "org-pomodoro" "Break Finished"))
    ;; (defun my/org-pomodoro-killed-hook () (my/oascript-notification "org-pomodoro" "Pomodoro Killed"))

    (defun my/org-pomodoro-launch-app ()
      (save-window-excursion
        (async-shell-command "open -a /Applications/JustFocus.app/Contents/MacOS/JustFocus")))
    (defun my/org-pomodoro-kill-app ()
      (save-window-excursion
        (shell-command "killall JustFocus")))
    (defun my/org-pomodoro-started-hook ()
      (my/org-pomodoro-kill-app)
      (my/org-pomodoro-launch-app))
    (defun my/org-pomodoro-finished-hook ())
    (defun my/org-pomodoro-break-finished-hook ()
      (org-pomodoro-start :pomodoro)
      (let ((clock (list (car org-clock-history))))
        (if clock
            (org-with-clock-position clock
              (when (string-match "^DOING" (org-get-heading))
                (org-clock-in-last)
                (org-agenda-redo-all))))))
    (defun my/org-pomodoro-killed-hook ()
      (my/org-pomodoro-kill-app))

    (add-hook 'org-pomodoro-started-hook 'my/org-pomodoro-started-hook)
    (add-hook 'org-pomodoro-finished-hook 'my/org-pomodoro-finished-hook)
    (add-hook 'org-pomodoro-break-finished-hook 'my/org-pomodoro-break-finished-hook)
    (add-hook 'org-pomodoro-killed-hook 'my/org-pomodoro-killed-hook)

    (defadvice org-pomodoro-set (around org-pomodoro-set-lazy (state) activate)
      "Set the org-pomodoro STATE."
      (setq org-pomodoro-state state
            org-pomodoro-end-time
            (cl-case state
              (:pomodoro (time-add (current-time) (* 60 org-pomodoro-length)))
              (:short-break (time-add (current-time) (* 60 org-pomodoro-short-break-length)))
              (:long-break (time-add (current-time) (* 60 org-pomodoro-long-break-length))))
            org-pomodoro-timer (run-with-timer t 10 'org-pomodoro-tick)))

    (with-eval-after-load 'org-agenda
      (define-key org-agenda-mode-map (kbd "i") 'org-pomodoro) ;; default: org-agenda-diary-entry

      ;; shorten Org-Agenda mode-line for narrow window
      (defadvice org-agenda-set-mode-name (around my/org-agenda-set-mode-name activate)
        (if (not (org-pomodoro-active-p))
            ad-do-it
          (setq mode-name '("Org-Agenda"))
          (force-mode-line-update)))

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
      ) ;; org-agenda
    ) ;; org-pomodoro

  ;;;;+
  )

;;;;+ Extra

;; -> https://ox-hugo.scripter.co/doc/org-capture-setup/
;; Populates only the EXPORT_FILE_NAME property in the inserted headline.
(with-eval-after-load 'org-capture
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
                 (function org-hugo-new-subtree-post-capture-template))))


;;;;+
