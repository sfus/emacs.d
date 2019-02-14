;;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;********************************************************************
;;; Color Theme
;;********************************************************************
;; -> http://aoe-tk.hatenablog.com/entry/20130210/1360506829

(when (>= emacs-major-version 24)
  (load-theme 'tango-dark t)
  ) ;; end of theme

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;; replace-colorthemes
;; ;; -> https://github.com/emacs-jp/replace-colorthemes
;; (when (require 'standard-ediff-theme nil t)
;;   (load-theme 'standard-ediff t t)
;;   (enable-theme 'standard-ediff)
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 各種フェース色設定

;; フリンジ領域(折り返しのライン) ;; foreground
(set-face-foreground 'fringe "gray")
;; (set-face-foreground 'fringe (face-background 'fringe)) ;; 消す場合

;; フリンジ領域(折り返しのライン) ;; background
;;(set-face-background 'fringe (face-background 'default)) ;; 背景色と同色に
;;(set-face-background 'fringe "antique white")
(set-face-background 'fringe "gray30")

;; show-paren マッチ
(set-face-attribute 'show-paren-match nil
                    :underline nil
                    :background "color-237")
;; show-paren-match face の優先度を下げる
(setq show-paren-priority -50)

;; リージョン色
(set-face-background 'region "brightred")

;; モードライン色
(set-face-attribute 'mode-line nil
                    :foreground "ghost white"      ;; モードラインの文字色の設定
                    :background "dark slate blue"  ;; モードラインの背景色の設定
                    :box nil)

;; powerline がある場合は上書き再定義
(eval-after-load "powerline"
  '(set-face-attribute 'mode-line nil
                       :foreground "ghost white"      ;; モードラインの文字色の設定
                       :background "dark slate blue"  ;; モードラインの背景色の設定
                       :box nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 全角空白、タブ文字、行末空白の表示
;; -> http://homepage1.nifty.com/blankspace/emacs/color.html

(defface my-face-b-1 '((((class color) (background dark)) (:background "gray25"))
                       (t (:background "bisque"))) nil) ;; for background light, etc
(defface my-face-b-2 '((((class color) (background dark)) (:background "black"))
                       (t (:background "antique white"))) nil) ;; for background light, etc
(defface my-face-u-1 '((t (:foreground "SteelBlue" :underline t))) nil)
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)
(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(("　" 0 my-face-b-1 append)
     ("\t" 0 my-face-b-2 append)
     ("[ \t]+$" 0 my-face-u-1 append)
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)


;;********************************************************************
;;; カスタマイズ
;;********************************************************************

;;------------------------------------------------------------------
;;> *カーソル設定*
;;>   - box             : 黒塗りの四角形（従来型）
;;>   - bar             : 細い縦棒
;;>   - caret           : 点滅する box （solid caret）*デフォルト*
;;>   - hairline-caret  : 点滅する bar
;;>   - checkered-caret : 点滅する市松模様の四角形（gray caret）
;;>
;;> カーソルの色（cursor-color）は、
;;> cursor-type が box, bar の場合のみ変更できる。
;;>
;;> カーソルの高さ（cursor-height）は、cursor-type が
;;> caret, checkered-caret, hairline-caret の場合のみ変更できる。
;;> 値は 0-4 の 5 種類で、
;;>  1-4 : フォントの高さの n/4
;;>    0 : アクティブウィンドウの境界の幅
;;------------------------------------------------------------------

;;********************************************************************
;;; Cursor
;;********************************************************************

;; ;; カーソル行にアンダーラインを引く
;; (when (not window-system) ;; コンソール上では見落としやすいので hl-line-mode ON
;;   (defface hlline-face '((t (:underline t))) "*Face used by hl-line.")
;;   (setq hl-line-face 'hlline-face)
;;   (global-hl-line-mode t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; フェース、フォント関連

;; M-x list-face-display のサンプル文字列
(setq list-faces-sample-text "漢字ひらがなカタカナabcdefghijklmnOPQRSTUVWXYZ")

;; カーソル位置のフェースを調べる関数
(defun my/describe-face-at-point ()
  "Return face used at point."
  (interactive)
  (describe-face (get-char-property (point) 'face)))
(global-set-key (kbd "M-g f") 'my/describe-face-at-point)

;; ;; キーボードからフォントを変更する (Meadow2 対応版)
;; ;; ( = Shift+左クリック)
;; (defun my-select-fontset-minibuf ()
;;   (interactive)
;;   (let* ((current-fontset (cdr (assq 'font (frame-parameters))))
;;          (fontset-alist (mapcar (lambda (x) (list x))
;;                                 (cond ((and (featurep 'meadow)
;;                                             (= emacs-major-version 21))
;;                                        (w32-font-list))
;;                                       (t
;;                                        (fontset-list)))))
;;          (new-fontset   (completing-read
;;                          (concat "fontset (now: " current-fontset "): ")
;;                          fontset-alist nil t)))
;;     (if (not (equal new-fontset current-fontset))
;;         (modify-frame-parameters
;;          nil `((font . ,new-fontset))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; モードラインの設定
;; -> http://homepage1.nifty.com/blankspace/emacs/mode-line.html

(when (not (require 'powerline nil t))
  (setq-default mode-line-format
                '("-"
                  mode-line-mule-info
                  mode-line-modified
                  mode-line-frame-identification
                  mode-line-buffer-identification
                  " "             ;; default:  "   "
                  global-mode-string
                  " %[("          ;; default:  "   %[("
                  mode-name
                  mode-line-process
                  minor-mode-alist
                  "%n" ")%]-"
                  (which-func-mode ("" which-func-format "-")) ;; default: "--"
                  (line-number-mode "L%l-")                    ;; default: "L%l--"
                  (column-number-mode "C%c-")                  ;; default: "C%c--"
                  (-3 . "%p")
                  "-%-")))

;; フレーム名を非表示
(setq mode-line-frame-identification " ")
;; メジャーモード名を短縮
(add-hook 'lisp-interaction-mode-hook (lambda () (setq mode-name "Lisp-I")))
(add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "Elisp")))
;; マイナーモード名を削除
(setcar (cdr (assq 'abbrev-mode minor-mode-alist)) "") ;; Abbrev
(setq eldoc-minor-mode-string "") ;; ElDoc
;; マイナーモード名を短縮
(add-hook 'outline-minor-mode-hook
          (lambda () (setcar (cdr (assq 'outline-minor-mode minor-mode-alist)) " Ol")))

;; ;; mode-line の“Encoded-kbd”を消す
;; (eval-after-load "encoded-kb"
;;   '(setcar (cdr (assq 'encoded-kbd-mode minor-mode-alist)) ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; モードラインの設定 (外観設定)

;; 共通外観設定
(line-number-mode t)                    ;; 行番号を表示
(column-number-mode t)                  ;; 桁番号を表示
(setq line-number-display-limit 1000000);; 行数を表示する上限

(setq resize-minibuffer-mode nil)
(setq resize-minibuffer-window-exactly t)
(setq resize-minibuffer-window-max-height 3)

;;; 日付/時刻表示
(setq display-time-24hr-format t)
(setq display-time-format "%m/%d(%a) %R")
(setq display-time-day-and-date t)
(display-time-mode 1) ;; 日付表示ON

;; 書式を変えたいとき
;; (setq display-time-string-forms
;;       '("[" year "/" month "/" day " " 24-hours ":" minutes "]"))
(setq display-time-string-forms "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; その他モードライン設定

;; cp932エンコードの表記変更
(coding-system-put 'cp932 :mnemonic ?P)
(coding-system-put 'cp932-dos :mnemonic ?P)
(coding-system-put 'cp932-unix :mnemonic ?P)
(coding-system-put 'cp932-mac :mnemonic ?P)

;; UTF-8エンコードの表記変更
(coding-system-put 'utf-8 :mnemonic ?U)
(coding-system-put 'utf-8-with-signature :mnemonic ?u)

;; 改行コードの表記追加
(setq eol-mnemonic-dos       ":CRLF ")
(setq eol-mnemonic-mac       ":CR ")
(setq eol-mnemonic-unix      ":LF ")
(setq eol-mnemonic-undecided ":?? ")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; emacs-powerline (モードラインをビジュアルに)
;; -> https://github.com/jonathanchu/emacs-powerline
;; -> http://blechmusik.hatenablog.jp/entry/2013/12/13/020823
;; -> http://hico-horiuchi.hateblo.jp/entry/20130510/1368201907

(when (require 'powerline nil t)
  ;;(setq powerline-color1 "grey22") ;; default: "grey22"
  ;;(setq powerline-color2 "grey40") ;; default: "grey40"
  (if (>= (string-to-number emacs-version) 24.4)
      (powerline-default-theme))
  ) ;; end of emacs-powerline

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; linum: 行数表示

;; 行番号表示
(when (require 'linum nil t)
  ;; ;; 常に linum-mode オン
  ;; (global-linum-mode t)

  ;; ;; 特定のメジャーモードのみ linum-mode オン
  ;; (add-hook 'text-mode-hook 'linum-mode)
  ;; (add-hook 'emacs-lisp-mode-hook 'linum-mode)
  ;; (add-hook 'c-mode-hook 'linum-mode)
  ;; (add-hook 'c++-mode-hook 'linum-mode)
  ;; (add-hook 'perl-mode-hook 'linum-mode)
  ;; (add-hook 'cperl-mode-hook 'linum-mode)
  ;; (add-hook 'ruby-mode-hook 'linum-mode)

  ;; 文字色・サイズ
  (set-face-attribute 'linum nil
                      :foreground "cyan"
                      :height 0.75)

  ;; ;; 行番号フォーマット
  (if window-system
      (setq linum-format "%5d")
    (setq linum-format "%5d|"))

  ;; ;; 行移動を契機に描画
  ;; (defvar linum-line-number 0)
  ;; (declare-function linum-update-current "linum" ())
  ;; (defadvice linum-update-current
  ;;     (around linum-update-current-around activate compile)
  ;;   (unless (= linum-line-number (line-number-at-pos))
  ;;     (setq linum-line-number (line-number-at-pos))
  ;;     ad-do-it
  ;;     ))

  ;; ;; バッファ中の行番号表示の遅延設定
  ;; (setq linum-delay t)
  ;; (defadvice linum-schedule (around linum-schedule-around () activate)
  ;;   (run-with-idle-timer 0.1 nil #'linum-update-current))

  ) ;; end of linum

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mode-line の minor-mode の部分の view-mode の文字を派手にする
(eval-after-load "view"
  '(setcar (cdr (assq 'view-mode minor-mode-alist))
           (if (fboundp 'propertize)
               (list (propertize " View"
                                 'face '(:foreground "white"
                                         :background "DeepPink1")))
             " View")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; nlinum: 行数表示 (軽量版)

;; (when (require 'nlinum nil t)
;;   ;; 常に nlinum-mode オン
;;   (global-nlinum-mode t)

;;   ;; 文字色・サイズ
;;   (set-face-attribute 'linum nil
;;                       :foreground "cyan"
;;                       :height 0.75)

;;   ;; ;; 行番号フォーマット
;;   (if window-system
;;       (setq nlinum-format "%5d")
;;     (setq nlinum-format "%5d|"))

;;   ) ;; end of nlinum
