;; configuration of spell check
(custom-set-variables
 '(flyspell-issue-welcome-flag nil)
 '(flyspell-use-meta-tab nil))

(with-eval-after-load 'ispell
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))


;;;;+ Extra

;;; ispell

;; M-$ や M-x ispell-region, M-x ispell-buffer でスペルチェック

;; -> http://keisanbutsuriya.hateblo.jp/entry/2015/02/10/152543
;; use aspell instead of ispell
;; $ echo "lang en_US" >> ~/.aspell.conf
(setq-default ispell-program-name "aspell")

;; 日本語・英語混じりの文書をチェックする際に日本語をスキップする
(eval-after-load "ispell"
  '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))

;; 辞書ファイル
;;(setenv "DICTIONARY" "/usr/share/dict/words")
(setq ispell-alternate-dictionary "/usr/share/dict/words")

;; 個人辞書の在処         ;; default: "~/.ispell_DICTNAME"
(setq ispell-personal-dictionary "~/.emacs.d/.ispell_default")

;; grep
(setq ispell-grep-command "grep") ;; default: "egrep"


;;; ac-ispell
;; -> https://github.com/syohex/emacs-ac-ispell
;; -> http://syohex.hatenablog.com/entry/20131123/1385184659

(when (locate-library "ac-ispell") ;; Completion words longer than 4 characters
  (custom-set-variables
   '(ac-ispell-requires 4))

  (eval-after-load "auto-complete"
        '(progn
           (ac-ispell-setup)))

  (add-hook 'git-commit-mode-hook 'ac-ispell-ac-setup)
  (add-hook 'mail-mode-hook 'ac-ispell-ac-setup)
  (add-hook 'text-mode-hook 'ac-ispell-ac-setup)

  (global-set-key (kbd "C-x i") 'ac-complete-ispell) ;; default: insert-file
  ) ;; ac-ispell ends here


;;; flyspell

;; オフィススイーツによく付属してるような逐次スペルチェック機能．
;; M-x flyspell-mode で起動/終了する．M-TAB でスペル修正．

;; C-x M-$ で flyspell-mode をトグル
(when (commandp 'flyspell-mode)
  (define-key ctl-x-map (kbd "M-$") 'flyspell-mode))


;;; ffap (find file (or url) at point)

;; 通常の find-file に加え、url なら browse-url、ftp なら ange-ftp を実行。
;; 更に各モードに応じてサーチパスからポイント直前のファイル名を探す。
;; ex.) (require 'hoge) の 'hoge 直後で C-x C-f
;; C-x C-f で ffap、 C-u C-x C-f で普通の find-file。
(ffap-bindings)

;; keybind
(define-key help-map (kbd "C-f") 'find-file-at-point) ;; default: (view-emacs-FAQ)

;; lisp-interaction-mode も ffap-el-mode に追加
(setq ffap-alist
      (append '((lisp-interaction-mode . ffap-el-mode))
              ffap-alist))

;; ;; 新規ファイルの場合には確認する
;; (setq ffap-newfile-prompt t)

;; RFC2472 で ffap をすると，日本語訳の RFC を表示できる
(setq ffap-rfc-path "http://www.minokasago.org/labo/RFC/rfc%s-jp.html")
;; ;; こっちは英語の設定
;; (setq ffap-rfc-path "http://www.ring.gr.jp/archives/doc/RFC/rfc%s.txt")

;; ffap でファイル名に以下の文字を含むと， dired に切り替える
;; つまり，*.el を開こうとすると，*.el をマスクとして， dired
;; を実行できる
;; デフォルトは nil なので，*.el というファイルを作ろうとする
(setq ffap-dired-wildcards "*")

;; ;; ftp 時に ping をしないで，いきなり ange-ftp で開く
;; (setq ffap-machine-p-known 'accept) ;; 'ping 'accept 'reject

;; ffap-kpathsea-expand-path で展開するパスの深さ
(setq ffap-kpathsea-depth 5)

;; highlight-completion-mode と干渉するので， URL の時には
;; 補完しないようにする
(defadvice hc-expand-file-name
  (around hc-expand-file-name-del activate)
  (if name
      ad-do-it))
;; completer との競合を防ぐ
(defadvice completer
  (before completer-set-table activate)
   (if (and (stringp (ad-get-arg 1))
            (string= (ad-get-arg 1) 'ffap-read-file-or-url-internal))
      (ad-set-arg 1 'read-file-name-internal)))
