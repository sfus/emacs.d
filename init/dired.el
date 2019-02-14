;;;; dired
(with-eval-after-load 'dired
  ;; Not create new buffer, if you chenge directory in dired
  (put 'dired-find-alternate-file 'disabled nil)

  (when (executable-find "gls")
    (setq insert-directory-program "gls"))

  (load-library "ls-lisp")

  ;; binding
  (define-key dired-mode-map (kbd "K") 'dired-k)
  (define-key dired-mode-map (kbd "Q") 'quick-preview-at-point)
  (define-key dired-mode-map (kbd "C-M-u") 'dired-up-directory)
  (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

  ;;;;+ Extra
  ;; other bindings
  (define-key dired-mode-map (kbd "\C-h") 'dired-up-directory)
  (define-key dired-mode-map [delete] 'dired-up-directory)
  (define-key dired-mode-map [backspace] 'dired-up-directory)

  ;; h => hexl-mode
  (define-key dired-mode-map (kbd "h")
    (lambda () (interactive)
      (hexl-find-file (dired-get-filename))))

  (add-hook 'dired-initial-position-hook 'dired-k)
  ;;;;+
  )

(with-eval-after-load 'wdired
  (define-key wdired-mode-map (kbd "C-o") 'toggle-input-method))

(custom-set-variables
 '(ls-lisp-dirs-first t)
 '(dired-dwim-target t)
 '(dired-auto-revert-buffer t)
 '(dired-recursive-copies 'always)
 '(dired-recursive-deletes 'always))

(autoload 'dired-jump "dired-x" nil t)
(global-set-key (kbd "C-x C-j") 'dired-jump)


;;;;+ Extra

;;; customize

;; ls option ("l": must)
;;(setq dired-listing-switches "-l") ;; default: "-la"

;; locale setting (to change English datetime format)
(setenv "LC_TIME" "C")

;; Change yes/no to y/n on deletion confirmation
(setq dired-deletion-confirmer 'y-or-n-p) ;; default: 'yes-or-no-p

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
(define-key dired-mode-map "z" 'my/dired-toggle-listing-switches)


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

;;; view-mode
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

;;; image-mode
(with-eval-after-load 'image-mode
  ;; ditto on image-mode
  (defun my/image-quit-and-next-line (arg)
    "Quit Image mode, and if Dired mode, go to the next line."
    (interactive "p")
    (kill-this-buffer)
    (recenter)
    (if dired-mode-p (dired-next-line arg)))

  (define-key image-mode-map "\C-m" 'my/image-quit-and-next-line))

;;; tar-mode
(with-eval-after-load 'tar-mode
 ;; C-m to view mode on tar-mode
 (define-key tar-mode-map "\C-m" 'tar-view)
 ;; q => kill-buffer (default: quit-window)
 (define-key tar-mode-map "q" 'kill-this-buffer))

;;; archive-mode
(with-eval-after-load 'archive-mode
  ;; C-m to view mode on archive-mode.
  (define-key archive-mode-map "\C-m" 'archive-view)
  ;; q => kill-buffer (default: quit-window)
  (define-key archive-mode-map "q" 'kill-this-buffer))
