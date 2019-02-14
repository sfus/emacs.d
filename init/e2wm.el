;;;; e2wm (EmacsでIDEパースペクティブ)
;; -> http://d.hatena.ne.jp/kiwanami/20100528/1275038929
;; -> https://github.com/kiwanami/emacs-window-manager

(when (require 'e2wm nil t)
  (global-set-key (kbd "M-+") 'e2wm:start-management) ; M-+でe2wmを起動

  ;; ;; codeウィンドウ設定
  ;; (setq e2wm:c-code-recipe
  ;;       '(| (:left-max-size 30)         ;; default: 35
  ;;           (- (:upper-size-ratio 0.7)
  ;;              files history)
  ;;           (- (:upper-size-ratio 0.8)  ;; default: 0.7
  ;;              (| (:right-max-size 25)  ;; default: 30
  ;;                 main imenu)
  ;;              sub)))

  (e2wm:add-keymap
   e2wm:pst-minor-mode-keymap
   '(;;("<M-left>"  . e2wm:dp-code)                       ; codeへ変更
     ;;("<M-right>" . e2wm:dp-two)                        ; twoへ変更
     ;;("<M-up>"    . e2wm:dp-doc)                        ; docへ変更
     ;;("<M-down>"  . e2wm:dp-dashboard)                  ; dashboardへ変更
     ;; ("C-M-1"     . e2wm:dp-code)                       ; codeへ変更
     ;; ("C-M-2"     . e2wm:dp-two)                        ; twoへ変更
     ;; ("C-M-3"     . e2wm:dp-doc)                        ; docへ変更
     ;; ("C-M-4"     . e2wm:dp-dashboard)                  ; dashboardへ変更
     ;; ("C-S-p"     . e2wm:pst-history-forward-command)   ; 履歴進む
     ;; ("C-S-n"     . e2wm:pst-history-back-command)      ; 履歴戻る
     ;; ("C-M-s"     . e2wm:my-toggle-sub)                 ; subウィンドウ表示をトグルする
     ("C-M-g"     . e2wm:my-delete-sub-window)          ; subウィンドウを閉じてmainを選択する
     ("prefix L"  . ielm)                               ; ielm を起動する（subで起動する）
     ("M-+"       . e2wm:stop-management)               ; M-+でe2wmを終了
     ) e2wm:prefix-key)

  ;; filesウィンドウキーマップ
  (e2wm:add-keymap
   e2wm:def-plugin-files-mode-map
   '(("C-h"         . e2wm:def-plugin-files-updir-command) ; C-hでも上ディレクトリに
     ("<backspace>" . e2wm:def-plugin-files-updir-command) ; BS でも上ディレクトリに
     ) e2wm:prefix-key)

  ;; infoを起動する
  (e2wm:add-keymap
   e2wm:dp-doc-minor-mode-map
   '(("prefix I" . info))
   e2wm:prefix-key)

  ;; Subウインドウをトグルする
  (defun e2wm:my-toggle-sub ()
    (interactive)
    (e2wm:pst-window-toggle 'sub t 'main))

  ;; SubウィンドウをクローズしてMainウィンドウを選択する
  (defun e2wm:my-delete-sub-window ()
    (interactive)
    (if (wlf:select (e2wm:pst-get-wm) 'sub)
        (delete-window))
    (e2wm:pst-window-select-main))

  ;; mainウィンドウ以外の linum-mode をOffにする
  (defun e2wm:my-linum-mode-off ()
    (interactive)
    (e2wm:dp-code)
    (wlf:select (e2wm:pst-get-wm) 'files)
    (linum-mode -1)
    (wlf:select (e2wm:pst-get-wm) 'history)
    (linum-mode -1)
    (wlf:select (e2wm:pst-get-wm) 'imenu)
    (linum-mode -1)
    (e2wm:pst-window-select-main))
  (add-hook 'e2wm:pst-minor-mode-setup-hook
            'e2wm:my-linum-mode-off)

  ;; e2wm 起動時にウィンドウ幅を広げる
  (when window-system
    (add-hook 'e2wm:pst-minor-mode-setup-hook
              (lambda ()
                (set-frame-width
                 (selected-frame)
                 (+ (cdr (assq 'width (frame-parameters))) 50))))
    (add-hook 'e2wm:pst-minor-mode-abort-hook
              (lambda ()
                (set-frame-width
                 (selected-frame)
                 (- (cdr (assq 'width (frame-parameters))) 50))))

    ;; 終了時にウィンドウ幅を元に戻す
    (add-hook 'kill-emacs-hook
              'e2wm:stop-management)
    ) ;; end of window-system setting


  ) ;; e2wm setting ends here.
