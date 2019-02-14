;;;; editing operations
;; Use regexp version as Default
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'anzu-query-replace-regexp)
(global-set-key (kbd "ESC M-%") 'anzu-query-replace-at-cursor)
(global-set-key (kbd "C-x %") 'anzu-replace-at-cursor-thing)
(define-key isearch-mode-map [remap isearch-query-replace]  #'anzu-isearch-query-replace)
(define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp)
(define-key isearch-mode-map (kbd "M-a") 'avy-isearch)

;; ;; electric-mode
;; (custom-set-variables
;;  '(electric-indent-mode nil))

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


;;;;+ Extra

;;; conf-unix-mode
(add-to-list 'auto-mode-alist '("/\\.gitconfig$" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("/\\.gitignore$" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("/\\..+rc$" . conf-unix-mode))
;;; vimrc-mode
(add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'\\|vifmrc\\'" . vimrc-mode))

;;; text-mode
(add-hook 'text-mode-hook 'turn-on-font-lock)
(remove-hook 'text-mode-hook 'auto-fill-mode-hook)

;;; expand-region
;; -> https://github.com/magnars/expand-region.el/blob/master/README.md
;; -> http://d.hatena.ne.jp/syohex/20120117/1326814127

(when (require 'expand-region nil t)
  ;; bind C-M-<SPC> / S-C-M-<SPC> (bind S-F11 / S-F12 under terminal env.)
  (if window-system
      (progn
        (global-set-key (kbd "C-M-<SPC>") 'er/expand-region) ;; default: mark-sexp
        (global-set-key (kbd "C-M-S-<SPC>") 'er/contract-region))
    (global-set-key (kbd "S-<f11>") 'er/expand-region) ;; default: mark-sexp
    (global-set-key (kbd "S-<f12>") 'er/contract-region))
  ) ;; end of expand-region


;;; multiple-cursors
;; -> https://github.com/magnars/multiple-cursors.el/blob/master/README.md
(when (require 'multiple-cursors nil t)
  (if window-system
      (progn
        ;; bind C-M-. / C-M-, / C-M-> / C-M-<
        (global-set-key (kbd "C-M-.") 'mc/mark-next-like-this) ;; find-tag-regexp
        (global-set-key (kbd "C-M-,") 'mc/unmark-next-like-this)
        (global-set-key (kbd "C-M->") 'mc/skip-to-next-like-this)
        (global-set-key (kbd "C-M-<") 'mc/mark-all-like-this))
    ;; (bind S-F7, S-F8, S-F9, S-F10 under terminal env.)
    (global-set-key (kbd "S-<f7>") 'mc/mark-next-like-this)
    (global-set-key (kbd "S-<f8>") 'mc/unmark-next-like-this)
    (global-set-key (kbd "S-<f9>") 'mc/skip-to-next-like-this)
    (global-set-key (kbd "S-<f10>") 'mc/mark-all-like-this))
  ) ;; end of multiple-cursors


;;; Editor Config
;; -> https://github.com/editorconfig/editorconfig-emacs
(when (require 'editorconfig nil t)
  (editorconfig-mode 1)
  ) ;; end of editorconfig


;;; text-adjust:
;; -> http://www.taiyaki.org/elisp/text-adjust/
;; -> http://d.hatena.ne.jp/rubikitch/20090220/text_adjust
;;      http://www.rubyist.net/~rubikitch/archive/mell.el
;;      http://www.rubyist.net/~rubikitch/archive/text-adjust.el
(when (locate-library "text-adjust")
  (require 'text-adjust)
  ;; 無視する全角記号
  (setq text-adjust-hankaku-except "　？！＠ー〜、，。．")
  ;; text-adjust-fill-region 実行時に左マージンを考慮させる
  (setq adaptive-fill-regexp "[ \t]*")
  (setq adaptive-fill-mode t)
  ;; C-x M-q にキー割り当て
  (define-key ctl-x-map "\M-q" 'text-adjust-space)

  ;; 置換ルールを TeX 向けに少し変更
  (setq text-adjust-rule-space
        '((("\\cK\\|\\cC\\|\\cH" "" "[[(0-9a-zA-Z+$]")   " ")
          (("[])/!?0-9a-zA-Z+$]" "" "\\cK\\|\\cC\\|\\cH") " ")))
  )

;; ;;; lispxmp
;; (when (require 'lispxmp nil t)
;;   (define-key emacs-lisp-mode-map (kbd "C-x M-q") 'lispxmp)
;;   ) ;; lispxmp


;;; Keisen
;; -> http://pitecan.com/Keisen/keisen.el
(when (locate-library "keisen")
  ;; Control + Meta + 矢印キーで罫線を引く
  (global-set-key [C-M-right] 'keisen-right-move)
  (global-set-key [C-M-left]  'keisen-left-move)
  (global-set-key [C-M-up]    'keisen-up-move)
  (global-set-key [C-M-down]  'keisen-down-move)

  (autoload 'keisen-up-move "keisen" nil t)
  (autoload 'keisen-down-move "keisen" nil t)
  (autoload 'keisen-left-move "keisen" nil t)
  (autoload 'keisen-right-move "keisen" nil t))


;;; browse-url
(when (locate-library "browse-url")
  (require 'browse-url)

  (global-set-key [mouse-2] 'browse-url-at-mouse) ;; middle click
  (global-set-key (kbd "C-c C-o") 'browse-url-at-point)

  ;; Dired keymap
  (add-hook 'dired-mode-hook
            (lambda ()
              (define-key dired-mode-map "\C-x," 'browse-url-of-dired-file)
              (define-key dired-mode-map [mouse-2] 'browse-url-at-mouse)))

  ) ;; browse-url


;;; page-ext
;; narrow-to-page (C-x n p) の拡張

(when (locate-library "page-ext")
  (require 'page-ext)

  ;; narrowing 時にも pages-directory を全ページに対して行なう
  (defadvice pages-directory
    (around widen activate)
    (save-restriction (widen) ad-do-it))

  ;; pages-directory 情報を移動時に消す
  (defadvice pages-directory-goto
    (around kill-pages-directory activate)
    (let ((cbuf (current-buffer)))
      ad-do-it
      (kill-buffer cbuf)
      (delete-other-windows)))
  (define-key pages-directory-map "\C-m" 'pages-directory-goto)

  ;; pages-directory 時にカーソル行をプレビュー
  (defun pages-directory-view (&optional next)
    "Go to the corresponding line in the pages buffer."
    (interactive)
    (if (or (not pages-buffer) (not (buffer-name pages-buffer))) (other-window 1))
    (if next (forward-line 1) (forward-line -1))
    (if (or (not pages-buffer) (not (buffer-name pages-buffer)))
        (progn (setq pages-buffer nil
                     pages-pos-list nil)
               (error "Buffer in which pages were found is deleted")))
    (beginning-of-line)
    (let* ((pages-number (1- (count-lines (point-min) (point))))
           (pos (nth pages-number pages-pos-list))
           (end-of-directory-p (eobp))
           (narrowing-p  pages-directory-buffer-narrowing-p))
      (pop-to-buffer pages-buffer)
      (widen)
      (if end-of-directory-p (goto-char (point-max))
        (goto-char (marker-position pos)))
      (if narrowing-p (narrow-to-page)))
    (if (or (not pages-buffer) (not (buffer-name pages-buffer))) (other-window 1)))
  (defun pages-directory-view-next ()
    (interactive)
    (pages-directory-view t))
  (define-key pages-directory-map '[up] 'pages-directory-view)
  (define-key pages-directory-map '[down] 'pages-directory-view-next)
  ) ;; page-ext ends here.

;; *Help*
;; C-x C-p C-n (next-page):次のページへ
;; C-x C-p C-p (previous-page):前のページへ
;; C-x C-p C-s (search-pages):カーソル以降で指定した文字のあるページへ移動
;; C-x C-p C-a (add-new-page):新しいページを追加
;; C-x C-p s (sort-pages-buffer):ページをソート
;; C-x C-p C-l (set-page-delimiter):page-delimiter を設定
;; C-x C-p C-d (pages-directory):ページの一覧を表示
;; sort-pages:リージョン内のページをソート
;; what-page:ページの情報を「 Page 27, line 33 」という風に表示


;;; smooth-scroll
;; -> https://github.com/k-talo/smooth-scroll.el
;; -> https://www.emacswiki.org/emacs/SmoothScrolling
;; -> https://qiita.com/ShingoFukuyama/items/429199542c38625c5554
(when (require 'smooth-scroll nil t)
  (smooth-scroll-mode t)
  (setq smooth-scroll/vscroll-step-size 4) ;; default: 2
  )

;; ;;; yascroll
;; (when (require 'yascroll nil t)
;;   (global-yascroll-bar-mode 1)
;;   ;; (custom-set-variables
;;   ;;  '(yascroll:delay-to-hide nil))
;;   ) ;; yascroll

;;; gist
;; -> https://github.com/defunkt/gist.el
;; git config --global github.user <your-github-user-name>
;; git config --global github.oauth-token <your-personal-access-token-with-gist-scope>
(when (require 'gist nil t)

  ) ;; gist

;; multi-files gist support (indicated by a '+' in the gist list)
;; improved gist-list buffer, based on tabulated-list.el (same codebase as package.el) New keybindings:
;; g : reload the gist list from server
;; e : edit current gist description
;; k : delete current gist
;; + : add a file to the current gist
;; - : remove a file from the current gist
;; y : print current gist url
;; b : browse current gist
;; * : star gist
;; ^ : unstar gist
;; f : fork gist
;; in-place edition. While viewing a gist file buffer, you can:
;; C-x C-s : save a new version of the gist
;; C-x C-w : rename some file
;; dired integration. From a dired buffer, you can:
;; @ : make a gist out of marked files (with a prefix, make it private)
