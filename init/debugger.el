;;;; gdb
;; -> https://www.gnu.org/software/emacs/manual/html_node/emacs/Commands-of-GUD.html
;; -> http://d.hatena.ne.jp/higepon/20090505/p1
;; -> http://murakan.cocolog-nifty.com/blog/2009/01/gud-on-emacs23-.html

;; -> https://gist.github.com/chokkan/5693497
(when (require 'gud nil t)

  (global-set-key (kbd "M-g M-d") 'gdb)
  (with-eval-after-load "elscreen"
    (define-key elscreen-map (kbd "C-d") 'gdb-restore-windows)  ;; C-z C-z C-d
    )

  (setq gdb-many-windows t)

  ;; https://emacs.stackexchange.com/questions/23953/how-can-i-prevent-gdb-input-output-buffer-from-aggressively-popping-up-in-fram
  (setq gdb-display-io-nopopup t)
  (setq gdb-use-separate-io-buffer nil)

  (add-hook
   'gdb-mode-hook
   '(lambda ()
      (gud-tooltip-mode t)
      (gud-def my/gud-break-main "break main" nil "Set breakpoint at main.")
      ))
  (setq gud-tooltip-echo-area nil)

  (defun my/gud-set-clear-breakpoint ()
    (interactive)
    (if (or (buffer-file-name) (eq major-mode 'gdb-assembler-mode))
        (if (or
             (let ((start (- (line-beginning-position) 1))
                   (end (+ (line-end-position) 1)))
               (catch 'breakpoint
                 (dolist (overlay (overlays-in start end))
                   (if (overlay-get overlay 'put-break)
                       (throw 'breakpoint t)))))
             (eq (car (fringe-bitmaps-at-pos)) 'breakpoint))
            (gud-remove nil)
          (gud-break nil))))

  (defun my/gud-kill ()
    "Kill gdb process."
    (interactive)
    (with-current-buffer gud-comint-buffer (comint-skip-input))
    (kill-process (get-buffer-process gud-comint-buffer)))

  ;; ;; Change prefix key
  ;;(setq gud-key-prefix "\C-z\C-a") ;; default: "\C-x\C-a"

  ;; like chrome key bindings
  (define-key gud-minor-mode-map (kbd "<f8>") 'gud-cont)        ;; resume
  (define-key gud-minor-mode-map (kbd "<f10>") 'gud-next)       ;; step over
  (define-key gud-minor-mode-map (kbd "<f11>") 'gud-step)       ;; step in
  (define-key gud-minor-mode-map (kbd "<S-f11>") 'gud-finish)   ;; step out
  (define-key gud-minor-mode-map (kbd "M-\\") 'gud-cont)
  (define-key gud-minor-mode-map (kbd "M-'") 'gud-next)
  (define-key gud-minor-mode-map (kbd "M-;") 'gud-step)
  (define-key gud-minor-mode-map (kbd "M-:") 'gud-finish)

  ;; other bindings
  (define-key gud-minor-mode-map (kbd "C-x <SPC>") 'gud-break) ;; revert default binding
  (define-key gud-minor-mode-map (kbd "M-C-x") 'gud-break)
  (define-key gud-minor-mode-map (kbd "M-C-d") 'gud-remove)
  (define-key gud-minor-mode-map (kbd "M-C-t") 'gud-tbreak)
  (define-key gud-minor-mode-map (kbd "C-c C-c") 'gud-run)

  ;;(global-set-key (kbd "M-t") 'gud-tbreak) ;; (transpose-words) default C-x C-a C-t

  ) ;; end of gud

;; Mac Setting
;; <http://qiita.com/ymotongpoo/items/81d3c945483cae734122>
;; <http://efcl.info/2014/08/29/golang-liteide-debugger/>
;; <http://qiita.com/takahashim/items/204ffa698afe09bd4e28>
;;
;; $ brew install https://raw.github.com/Homebrew/homebrew-dupes/master/gdb.rb
;; $ open -a "Keychain Access.app"
;;   「証明書アシスタント」「証明を作成」を選択し、例えば gdb-cert と名前を付ける。
;;   「自己署名ルート」「コード署名」を選択し、「デフォルトを無効化」にチェックを入れる。
;;   「キーチェーン」は「システム」を選択。
;;    作成された証明書をダブルクリックで開いて、「信頼」を開き、コード署名のところを「常に信頼」に設定。
;; $ sudo vi /System/Library/LaunchDaemons/com.apple.taskgated.plist
;;     <key>ProgramArguments</key>
;;     <array>
;;         <string>/usr/libexec/taskgated</string>
;; -       <string>-s</string>
;; +       <string>-sp</string>
;;     </array>
;; $ sudo codesign -s gdb-cert /usr/local/bin/gdb
;; $ go build -gcflags "-N -l" foo.go
