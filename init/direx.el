;;;; direx
;; -> https://github.com/m2ym/direx-el

(when (require 'direx nil t)

  ;; direx with popwin
  (when (require 'popwin nil t)
    (push '(direx:direx-mode :position left :width 40 :dedicated t)
          popwin:special-display-config))

  ;; http://syohex.hatenablog.com/entry/20130202/1359814263
  (when (require 'direx-project nil t)
    (defun my/dired-jump ()
      (interactive)
      (cond (current-prefix-arg
             (dired-jump))
            ((not (one-window-p)
                  ;; (< (window-width) 100)
                  )
             (or (ignore-errors
                   (direx-project:jump-to-project-root) t)
                 (direx:jump-to-directory)))
            (t
             (or (ignore-errors
                   (direx-project:jump-to-project-root-other-window) t)
                 (direx:jump-to-directory-other-window)))))

    (global-set-key (kbd "C-x C-j") 'my/dired-jump)

    ) ;; direx-project

  (define-key direx:direx-mode-map (kbd "C-h") 'direx:up-item)
  (define-key direx:direx-mode-map (kbd "C-g") 'quit-window)

  ;; Disable helm-mode on direx:do-copy-files
  (defadvice direx:do-copy-files
      (around direx:do-copy-files-default activate)
    (let ((helm-mode-p helm-mode))
      (if helm-mode-p (helm-mode -1))
      ad-do-it
      (if helm-mode-p (helm-mode 1))))

  ;; (with-eval-after-load "e2wm"
  ;;  (defadvice my/dired-jump (around popwin:popup-buffer-with-no-e2wm activate)
  ;;    (e2wm:stop-management)
  ;;    ad-do-it
  ;;    ))

  ) ;; direx
;;
;; (define-key map (kbd "R") 'direx:do-rename-file)
;; (define-key map (kbd "C") 'direx:do-copy-files)
;; (define-key map (kbd "D") 'direx:do-delete-files)
;; (define-key map (kbd "+") 'direx:create-directory)
;; (define-key map (kbd "M") 'direx:do-chmod-file)
;; (define-key map (kbd "L") 'direx:do-load-file)
;; (define-key map (kbd "B") 'direx:do-byte-compile-file)
;; (define-key map (kbd "G") 'direx:do-chgrp)
;; (define-key map (kbd "O") 'direx:do-chown)
;; (define-key map (kbd "T") 'direx:do-touch)

;; (define-key map (kbd "n")           'direx:next-item)
;; (define-key map (kbd "C-n")         'direx:next-item)
;; (define-key map (kbd "<down>")      'direx:next-item)
;; (define-key map (kbd "p")           'direx:previous-item)
;; (define-key map (kbd "C-p")         'direx:previous-item)
;; (define-key map (kbd "<up>")        'direx:previous-item)
;; (define-key map (kbd "C-M-n")       'direx:next-sibling-item)
;; (define-key map (kbd "C-M-<down>")  'direx:next-sibling-item)
;; (define-key map (kbd "C-M-p")       'direx:previous-sibling-item)
;; (define-key map (kbd "C-M-<up>")    'direx:previous-sibling-item)
;; (define-key map (kbd "^")           'direx:up-item)
;; (define-key map (kbd "C-M-u")       'direx:up-item)
;; (define-key map (kbd "C-M-<left>")  'direx:up-item)
;; (define-key map (kbd "C-M-d")       'direx:down-item)
;; (define-key map (kbd "C-M-<right>") 'direx:up-item)
;; (define-key map (kbd "e")           'direx:echo-item)
;; (define-key map (kbd "f")           'direx:find-item)
;; (define-key map (kbd "o")           'direx:find-item-other-window)
;; (define-key map (kbd "v")           'direx:view-item)
;; (define-key map (kbd "V")           'direx:view-item-other-window)
;; (define-key map (kbd "C-o")         'direx:display-item)
;; (define-key map (kbd "RET")         'direx:maybe-find-item)
;; (define-key map (kbd "TAB")         'direx:toggle-item)
;; (define-key map (kbd "i")           'direx:toggle-item)
;; (define-key map (kbd "E")           'direx:expand-item-recursively)
;; (define-key map (kbd "g")           'direx:refresh-whole-tree)
;; (define-key map [mouse-1]           'direx:mouse-1)
;; (define-key map [mouse-2]           'direx:mouse-2)
