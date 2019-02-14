;; ediff
(custom-set-variables
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-diff-options "-twB"))

;;;;+ Extra
(custom-set-faces
 '(ediff-current-diff-A ((t (:background "color-28"))))
 '(ediff-current-diff-B ((t (:background "color-28"))))
 '(ediff-current-diff-C ((t (:background "color-28"))))
 '(ediff-even-diff-C ((t (:background "color-23"))))
 '(ediff-odd-diff-C ((t (:background "brightblack"))))
 '(ediff-fine-diff-C ((t (:background "yellow"))))
 '(ediff-even-diff-Ancestor ((t (:background "black"))))
 )

;; `Q' => quit and return terminal
(defun my/ediff-quit-and-kill-terminal (&optional arg)
  (interactive "P")
  (ediff-barf-if-not-control-buffer)
  (let ((ctl-buf (current-buffer))
        (ctl-frm (selected-frame)))
    (set-buffer ctl-buf)
    (ediff-really-quit nil)
    (select-frame ctl-frm)
    (raise-frame ctl-frm))
  (save-buffers-kill-terminal arg))

;; `c' / `C' => includes both a/b diffs
;; -> https://emacs.stackexchange.com/questions/19339/how-to-use-both-variants-in-ediff
(defun my/ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun my/ediff-copy-both-to-C-reverse ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer))))

;; Set ediff-keymap-setup-hook
(defun my/ediff-keymap-setup-hook ()
  (define-key ediff-mode-map (kbd "Q") 'my/ediff-quit-and-kill-terminal)
  (define-key ediff-mode-map "c" 'my/ediff-copy-both-to-C)
  (define-key ediff-mode-map "C" 'my/ediff-copy-both-to-C-reverse))
(add-hook 'ediff-keymap-setup-hook 'my/ediff-keymap-setup-hook)

;;; ediff
;; -> https://www.gnu.org/software/emacs/manual/html_mono/ediff.html

;;; smerge-mode
;; -> http://dev.ariel-networks.com/articles/emacs/part7/
;; -> https://emacs.stackexchange.com/questions/16469/how-to-merge-git-conflicts-in-emacs
;; C-c ^ n      smerge-next
;; C-c ^ p      smerge-prev
;; C-c ^ R      smerge-refine
;; C-c ^ m      smerge-keep-mine
;; C-c ^ o      smerge-keep-other
;; C-c ^ a      smerge-keep-all
;; C-c ^ b      smerge-keep-base
;; C-c ^ E      smerge-ediff

;; Prefix key
(setq smerge-command-prefix (kbd "C-c v"))

;; ;; whether leave resolved conflict position or not
;; (setq smerge-auto-leave nil) ;; default: t

;;;;+
