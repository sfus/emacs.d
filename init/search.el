;;;; setting for searching
;; anzu
(global-anzu-mode +1)
(custom-set-variables
 '(anzu-mode-lighter "")
 '(anzu-deactivate-region t)
 '(anzu-search-threshold 1000)
 '(anzu-replace-threshold 50)
 '(anzu-replace-to-string-separator " => "))

;; avy
(custom-set-variables
 '(avy-case-fold-search nil))


;;;;+ Extra

;;; case-fold
;; -> http://yohshiy.blog.fc2.com/blog-entry-191.html

;; C-x / で toggle-case-fold-search (大文字小文字の区別 有/無)
(if (require 'menu-bar nil t)
    (define-key ctl-x-map "/" 'toggle-case-fold-search))

;; 大文字・小文字を区別しないでサーチ（全般）;; default: t
(setq-default case-fold-search t)

;; 大文字・小文字を区別しないでサーチ（インクリメンタルサーチ）
(setq-default isearch-case-fold-search t)

;; replase 時に大文字小文字を保存する ;; default: t
(setq case-replace t)

;; 変換前   変換後(t)   変換後(nil)
;; foo      bar         bar
;; Foo      Bar         bar
;; FOO      BAR         bar

;; ;; query-replace の時は常に case-fold-search nil (大文字小文字を厳密マッチ)
;; (defadvice query-replace (around replace-ajust
;;                                  activate compile)
;;   "query-replace with case-fold-search:off"
;;   (let ((case-fold-search nil))
;;          ad-do-it))

;; 補完で大文字小文字の区別をしない
(setq completion-ignore-case t)

;; バッファ名の問い合わせで大文字小文字の区別をしない
(setq read-buffer-completion-ignore-case t)

;; ファイル名の問い合わせで大文字小文字の区別をしない
(setq read-file-name-completion-ignore-case t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; isearch

;; インクリメント検索時に縦スクロールを有効化
(setq isearch-allow-scroll nil)

;; isearch ですぐにハイライトする
(setq lazy-highlight-initial-delay 0)

;; C-hで検索文字列を一文字削除
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)

;; ;; C-dで検索文字列を一文字削除
;; (define-key isearch-mode-map (kbd "C-d") 'isearch-delete-char)

;; C-yで検索文字列にヤンク貼り付け
(define-key isearch-mode-map (kbd "C-y") 'isearch-yank-kill)

;; C-eで検索文字列を編集
(define-key isearch-mode-map (kbd "C-e") 'isearch-edit-string)

;; Tabで検索文字列を補完
(define-key isearch-mode-map (kbd "TAB") 'isearch-yank-word)

;; C-gで検索を終了
(define-key isearch-mode-map (kbd "C-g")
  '(lambda () (interactive) (isearch-done)))

;; 日本語の検索文字列をミニバッファに表示
(define-key isearch-mode-map (kbd "<compend>")
  '(lambda () (interactive) (isearch-update)))
(define-key isearch-mode-map (kbd "<kanji>")
  'isearch-toggle-input-method)

;; 普通の isearch にも migemo-isearch-yank-char
;; （直後の1文字を検索文字列に順次追加する） 機能を追加
(defun my-isearch-yank-char ()
  "Pull next character from buffer into search string."
  (interactive)
  (isearch-yank-string
   (save-excursion
     (and (not isearch-forward) isearch-other-end
          (goto-char isearch-other-end))
     (buffer-substring (point) (1+ (point))))))
(define-key isearch-mode-map (kbd "C-d") 'my-isearch-yank-char) ;; \C-d に割り当て
