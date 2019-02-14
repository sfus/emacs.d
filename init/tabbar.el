;;;; tabbar

(when (require 'tabbar nil t)

  ;; enable tabbar
  (call-interactively 'tabbar-mode t)

  ;; hide button
  (when (not window-system)
    (dolist (btn '(tabbar-buffer-home-button
                   tabbar-scroll-left-button
                   tabbar-scroll-right-button))
      (set btn (cons (cons "" nil) (cons "" nil)))))

  ;; disable mouse wheel to change tab (0：enable，-1: disable)
  (call-interactively 'tabbar-mwheel-mode -1)
  (remove-hook 'tabbar-mode-hook      'tabbar-mwheel-follow)
  (remove-hook 'mouse-wheel-mode-hook 'tabbar-mwheel-follow)

  ;; not use tab group (t: enable，nil: disable）
  (setq tabbar-buffer-groups-function nil)

  (with-eval-after-load "elscreen-buffer-group"
    ;; (setq tabbar-buffer-groups-function 'buffer-list)
    ;; (setq elscreen-buffer-group-exclusive t)
    )

  ;; tab separator width
  (setq tabbar-separator '(0.2))

  ;; tab changer
  (global-set-key (kbd "<C-tab>") 'tabbar-forward-tab)
  ;;(global-set-key (kbd "C-q")     'tabbar-backward-tab)

  ;; show `|' if console emacs
  (if (not window-system)
      (setq tabbar-separator-value "|"))

  ;; color
  (set-face-attribute 'tabbar-default nil
                      :foreground "ghost white"
                      :background "gray50"
                      :height 1.0)
  (set-face-attribute 'tabbar-selected nil
                      :foreground "ghost white"
                      :background "dark slate blue"
                      :height 1.0)
  (setq tabbar-background-color "gray50")

  ;; hide `*...*' buffers
  ;; -> http://ser1zw.hatenablog.com/entry/2012/12/31/022359

  (defvar my/tabbar-invisible-buffers '("TAGS" "GTAGS" "GRTAGS" "GPATH"))
  (defvar my/tabbar-invisible-buffers-regexp '("^magit\\(-.*\\)?:"
                                               "^CAPTURE\\(-\\d\\)?-\\*notes\\*"
                                               "\\(business\\|private\\|inbox\\|routine\\)\\.org"
                                               "_\\(LOCAL\\|REMOTE\\|BASE\\)_\\d\\{5\\}"))
  (defvar my/tabbar-visible-*-buffers '("*scratch*" "*Messages*" "*notes*"))
  (defun my/tabbar-buffer-list ()
    (interactive)
    (delq nil
          (mapcar #'(lambda (buf)
                      (cond
                       ((seq-find (lambda (name) (string-match (format "^%s\\(<.*>\\)?$" name) (buffer-name buf))) my/tabbar-invisible-buffers) nil)
                       ((seq-find (lambda (regexp) (string-match regexp (buffer-name buf))) my/tabbar-invisible-buffers-regexp) nil)
                       ((member (buffer-name buf) my/tabbar-visible-*-buffers) buf)
                       ((eq (current-buffer) buf) buf)
                       ((char-equal ?* (aref (buffer-name buf) 0)) nil)
                       ((char-equal ?\  (aref (buffer-name buf) 0)) nil)
                       ((buffer-file-name buf) buf)
                       ((buffer-live-p buf) buf)))
                  (buffer-list))))
  (setq tabbar-buffer-list-function 'my/tabbar-buffer-list)

  ;; enable tabbar on persp-mode
  (with-eval-after-load "persp-mode"
    (setq persp-buffer-list-function 'my/tabbar-buffer-list))

  ) ;; end of tabbar
