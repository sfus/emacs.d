;;;; Common VCS setting
(custom-set-variables
 '(auto-revert-check-vc-info t)
 '(auto-revert-mode-text ""))
(global-auto-revert-mode 1)

;; disable vc-mode
(custom-set-variables
 '(vc-handled-backends '(Git))
 '(vc-follow-symlinks t))
(with-eval-after-load 'vc
  '(remove-hook 'find-file-hooks 'vc-find-file-hook))

;; vc
(global-set-key (kbd "C-x v d") 'vc-diff)

;; git-gutter
(global-git-gutter-mode +1)
(global-set-key (kbd "C-x v u") 'git-gutter)
(global-set-key (kbd "C-x v p") 'git-gutter:stage-hunk)
(global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)
(global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)
(global-set-key (kbd "C-x v e") 'git-gutter:end-of-hunk)

;;;;+ Extra
(global-set-key (kbd "C-x v SPC") #'git-gutter:mark-hunk)
;; Add update-hook
(add-to-list 'git-gutter:update-hooks 'after-save-hook)

(custom-set-variables
 '(git-gutter:update-interval 2)
 '(git-gutter:handled-backends '(git svn)))

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
;;;;+

;; (custom-set-variables
;;  '(git-gutter:verbosity 4)
;;  '(git-gutter:modified-sign " ")
;;  '(git-gutter:deleted-sign " "))

(add-hook 'focus-in-hook 'git-gutter:update-all-windows)

(smartrep-define-key
    global-map  "C-x " '(("C-p" . 'git-gutter:previous-hunk) ;; helm-editutil-git-ls-files
                         ("C-n" . 'git-gutter:next-hunk)))

;; magit
(custom-set-variables
 '(git-commit-fill-column 80)
 '(git-commit-summary-max-length 72)
 '(magit-auto-revert-mode-lighter "")
 '(magit-push-always-verify nil))

(defun my/magit-status-around (orig-fn &rest args)
  (window-configuration-to-register :magit-fullscreen)
  (apply orig-fn args)
  (delete-other-windows))

(global-set-key (kbd "M-g M-g") 'magit-status)

(with-eval-after-load 'magit
  (global-git-commit-mode +1)
  ;; (advice-add 'magit-status :around 'my/magit-status-around)

  ;;(define-key magit-status-mode-map (kbd "q") 'my/magit-quit-session)
  )

(defun my/magit-quit-session ()
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen)
  (git-gutter:update-all-windows))

(defun my/git-commit-commit-after (_unused)
  (delete-window))

(defun my/git-commit-mode-hook ()
  (setq-local company-backends '(company-ispell company-files company-dabbrev))
  (flyspell-mode +1))

(with-eval-after-load 'git-commit
  (add-hook 'git-commit-mode-hook 'my/git-commit-mode-hook)
  (advice-add 'git-commit-commit :after 'my/git-commit-commit-after))


;;;;+ Extra
(when (require 'magit nil t) ;; allways require for emacsclient $GIT_EDITOR call
  ;; restrict to open diff window on magit-commit
  (remove-hook 'server-switch-hook 'magit-commit-diff))

;; git-commit (included by magit)
(custom-set-variables
 '(git-commit-summary-max-length 72) ;; default: 50
 '(git-commit-fill-column 120)       ;; default: 72
 )
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

;;;;+

;; git-messenger
(custom-set-variables
 '(git-messenger:handled-backends '(git)))
(global-set-key (kbd "C-x v m") 'git-messenger:popup-message)
