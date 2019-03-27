;;; C-, C-. => change buffers

;; ignore buffer list
(defvar my-ignore-buffer-list
  '("TAGS" "*Help*" "*Compile-Log*" "*compilation*" "*Completions*" "*Bookmark List*"
    "*Shell Command Output*" "*Apropos*" "*Buffer List*" "*WoMan-Log*"
    "*helm-mode-execute-extended-command*" "*helm-mode-iswitchb-buffer*"
    "*helm-mode-describe-variable*" "*helm-mode-describe-function*"
    ))

(defun my-visible-buffer (blst)
  (let ((bufn (buffer-name (car blst))))
    (if (or (= (aref bufn 0) ? ) (member bufn my-ignore-buffer-list))
        (my-visible-buffer (cdr blst)) (car blst))))
(defun my-grub-buffer ()
  (interactive)
  (switch-to-buffer (my-visible-buffer (reverse (buffer-list)))))
(defun my-bury-buffer ()
  (interactive)
  (let ((nbuf (my-visible-buffer (cdr (buffer-list)))))
    (bury-buffer)
    (switch-to-buffer nbuf)))

;; bind C-, / C-. (bind PageUp / PageDown if in terminal)
(if window-system
    (progn
      (defvar my-grub-buffer-key "C-.")
      (defvar my-bury-buffer-key "C-,"))
  (defvar my-grub-buffer-key "<next>")
  (defvar my-bury-buffer-key "<prior>"))

(define-key global-map (kbd my-grub-buffer-key) 'my-grub-buffer)
(define-key global-map (kbd my-bury-buffer-key) 'my-bury-buffer)

;; prevent to override by other mode map
(with-eval-after-load 'flyspell
  (define-key flyspell-mode-map (kbd my-grub-buffer-key) 'my-grub-buffer)
  (define-key flyspell-mode-map (kbd my-bury-buffer-key) 'my-bury-buffer))
(with-eval-after-load 'org-mode
  (define-key org-mode-map (kbd my-grub-buffer-key) 'my-grub-buffer)
  (define-key org-mode-map (kbd my-bury-buffer-key) 'my-bury-buffer))

(with-eval-after-load 'tabbar
  (define-key global-map (kbd my-grub-buffer-key) 'tabbar-forward-tab)
  (define-key global-map (kbd my-bury-buffer-key) 'tabbar-backward-tab)

  (with-eval-after-load 'flyspell
    (define-key flyspell-mode-map (kbd my-grub-buffer-key) 'tabbar-forward-tab)
    (define-key flyspell-mode-map (kbd my-bury-buffer-key) 'tabbar-backward-tab))
  (with-eval-after-load 'org-mode
    (define-key org-mode-map (kbd my-grub-buffer-key) 'tabbar-forward-tab)
    (define-key org-mode-map (kbd my-bury-buffer-key) 'tabbar-backward-tab))
  )

;;------------------------------------------------------------

;;; show white spaces
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

;;------------------------------------------------------------

;;; C-w => resion cut, or preceding word cut if no resion
;; -> http://d.hatena.ne.jp/kiwanami/20091222/1261504543
(defun my/kill-region-or-backward-kill-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (point) (mark))
    (backward-kill-word 1)))
(global-set-key (kbd "C-w") 'my/kill-region-or-backward-kill-word)

;;------------------------------------------------------------

;;; kill word without storing kill-ring
;; -> http://www.emacswiki.org/emacs/BackwardDeleteWord
;; -> http://qiita.com/kizashi1122/items/7028aa19f51823b69277
(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))
(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

;; ;; M-d => kill word without storing kill-ring
;; ;; (comment out to use the following `kill-word-or-delete-horizontal-space')
;; (global-set-key (kbd "M-d") 'delete-word)

;; M-DEL => backward kill word without storing kill-ring
(global-set-key (read-kbd-macro "<M-DEL>") 'backward-delete-word)


;; M-d => delete word, or delete white space if cursor stays in white space
;; -> http://d.hatena.ne.jp/kiwanami/20091222/1261504543
(defun my/kill-word-or-delete-horizontal-space (arg)
  (interactive "p")
  (let ((pos (point)))
    (if (and (not (eobp))
             (= (char-syntax (char-after pos)) 32)
             (= (char-syntax (char-after (1+ pos))) 32))
        (prog1 (delete-horizontal-space)
          (unless (memq (char-after pos) '(\?( \?) \?{ \?} \?[ \?]))
            (insert " ")))
      (delete-word arg) ;; changed from (kill-word arg) to above function
      )))
(global-set-key (kbd "M-d") 'my/kill-word-or-delete-horizontal-space)

;;------------------------------------------------------------

;;; C-M-o => other-window or other-window with dired
(defun my/other-window-or-dired-other-window (arg)
  (interactive "p")
  (let ((one-window (one-window-p)))
    (when one-window
      (if (> (window-width) 120)
          (split-window-right)
        (split-window-below)))
    (unless (>= (prefix-numeric-value current-prefix-arg) 16)
      (other-window arg))
    (if one-window
        (dired "."))))
(global-set-key (kbd "C-M-o") 'my/other-window-or-dired-other-window)

;; C-g on dired => delete-window
(defun my/quit-dired-other-window ()
  (interactive)
  (if (or (not (eq major-mode 'dired-mode))
          (one-window-p))
      (keyboard-quit)
    (kill-this-buffer)
    (delete-window)))
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-g") 'my/quit-dired-other-window))

;;------------------------------------------------------------

;; M-g f => describe face at point
(defun my/describe-face-at-point ()
  "Return face used at point."
  (interactive)
  (describe-face (get-char-property (point) 'face)))
(global-set-key (kbd "M-g f") 'my/describe-face-at-point)

;;------------------------------------------------------------

;; M-g s =>  open *scratch* buffer
(defun my/switch-to-scratch-buffer ()
  (interactive)
  (switch-to-buffer "*scratch*"))
(global-set-key (kbd "M-g s") 'my/switch-to-scratch-buffer)

;;------------------------------------------------------------

;;; Delete file if no contents on save
(defun my-delete-file-if-no-contents ()
  (when (and
         (buffer-file-name (current-buffer))
         (= (point-min) (point-max)))
    (when (y-or-n-p "Delete file and kill buffer?")
      (delete-file
       (buffer-file-name (current-buffer)))
      (kill-buffer (current-buffer)))))
(if (not (memq 'my-delete-file-if-no-contents after-save-hook))
    (setq after-save-hook
          (cons 'my-delete-file-if-no-contents after-save-hook)))

;;------------------------------------------------------------

;;; Delete window on quit-window ('q')  ;; default: C-u q
(defadvice quit-window
  (before quit-and-delete-window (&optional kill window) activate)
  (if (and (null window)
           (not (one-window-p)))
      (setq window (selected-window))))

;;------------------------------------------------------------

;;; Change to read only if following from help
(defadvice help-follow
  (after help-follow-read-only activate)
  (setq buffer-read-only t))

;;------------------------------------------------------------

;;; chmod +x on save script file automatically
;; -> http://namazu.org/~tsuchiya/elisp/#chmod
(defun my/make-file-executable ()
  "Make the file of this buffer executable, when it is a script source."
  (save-restriction
    (widen)
    (if (string= "#!" (buffer-substring-no-properties 1 (min 3 (point-max))))
        (let ((name (buffer-file-name)))
          (or (equal ?. (string-to-char (file-name-nondirectory name)))
              (let ((mode (file-modes name)))
                (set-file-modes name (logior mode (logand (/ mode 4) 73)))
                (message (concat "Wrote " name " (+x)"))))))))
(add-hook 'after-save-hook 'my/make-file-executable)

;;------------------------------------------------------------

;; Suppress recentf auto save message
;; -> https://masutaka.net/chalow/2011-10-30-2.html
;; -> http://keisanbutsuriya.hateblo.jp/entry/2015/02/15/174758
;; -> https://emacs.stackexchange.com/questions/14706/suppress-message-in-minibuffer-when-a-buffer-is-saved
(defmacro with-suppressed-message (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))
(run-with-idle-timer 30 t '(lambda ()          ;; save .recentf per 30 sec
   (with-suppressed-message (recentf-save-list))))
(recentf-mode 1)

;;------------------------------------------------------------

;;; Reopen with sudo with tramp
;; -> http://qiita.com/k_ui/items/d9e03ea9523036970519
(defun my/reopen-with-sudo ()
  "Reopen current buffer-file with sudo using tramp."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (find-alternate-file (concat "/sudo::" file-name))
      (error "Cannot get a file name"))))

;;------------------------------------------------------------

;;; C-M-c => duplicate line (default: exit-recursive-edit)
;; https://github.com/syohex/emacs-editutil/blob/8bd35b8190763cd68bde95a0b766bcb5e6ed6c61/editutil.el#L453-L469
(defun my/editutil-duplicate-thing (n)
  (interactive "p")
  (let ((orig-column (current-column)))
    (save-excursion
      (let ((orig-line (line-number-at-pos))
            (str (if (use-region-p)
                     (buffer-substring (region-beginning) (region-end))
                   (buffer-substring (line-beginning-position)
                                     (line-end-position)))))
        (when (or (not (use-region-p)) (not (bolp)))
          (forward-line 1)
          ;; maybe lastline
          (when (= orig-line (line-number-at-pos))
            (insert "\n")))
        (dotimes (_ (or n 1))
          (insert str "\n"))))
    (move-to-column orig-column)))
(global-set-key (kbd "C-M-c") #'my/editutil-duplicate-thing)

;;------------------------------------------------------------

;;; C-c C-j => Open IntelliJ from Emacs
;; -> https://blog.shibayu36.org/entry/2017/08/07/190421
(defun my/open-by-intellij ()
  (interactive)
  (shell-command
   (format "/Applications/IntelliJ\\ IDEA.app/Contents/MacOS/idea --line %d %s >/dev/null 2>&1"
           (line-number-at-pos)
           (buffer-file-name)))
  (shell-command "open -a /Applications/IntelliJ\\ IDEA.app"))
(define-key global-map (kbd "C-c C-j") 'my/open-by-intellij)

;;------------------------------------------------------------

;;; mode-line encoding
;; -> https://qiita.com/kai2nenobu/items/ddf94c0e5a36919bc6db
(defun my-coding-system-name-mnemonic (coding-system)
  (let* ((base (coding-system-base coding-system))
         (name (symbol-name base)))
    (cond ((string-prefix-p "utf-8" name) "UTF-8")
          ((string-prefix-p "utf-16" name) "UTF-16")
          ((string-prefix-p "utf-7" name) "UTF-7")
          ((string-prefix-p "japanese-shift-jis" name) "SJIS")
          ((string-match "cp\\([0-9]+\\)" name) (match-string 1 name))
          ((string-match "japanese-iso-8bit" name) "EUC")
          (t "???")
          )))

(defun my-coding-system-bom-mnemonic (coding-system)
  (let ((name (symbol-name coding-system)))
    (cond ((string-match "be-with-signature" name) "[BE]")
          ((string-match "le-with-signature" name) "[LE]")
          ((string-match "-with-signature" name) "[BOM]")
          (t ""))))

(defun my-buffer-coding-system-mnemonic ()
  "Return a mnemonic for `buffer-file-coding-system'."
  (let* ((code buffer-file-coding-system)
         (name (my-coding-system-name-mnemonic code))
         (bom (my-coding-system-bom-mnemonic code)))
    (format "%s%s" name bom)))

(setq-default mode-line-mule-info
              (cl-substitute '(:eval (my-buffer-coding-system-mnemonic))
                             "%z" mode-line-mule-info :test 'equal))

;;------------------------------------------------------------

;;; Open Emacs from IntelliJ
;; -> https://developer.atlassian.com/blog/2015/03/emacs-intellij/
;; ~/bin/openinemacs
;; (Fixed window activation from original one)
;;--------------------------------------------------------------------
;;#!/bin/bash
;;
;;file=$1
;;line=$2
;;col=$3
;;/usr/local/bin/emacsclient -n -e \
;;    "(progn
;;
;;       ;; Load the file
;;       (find-file \"$file\")
;;
;;       ;; Jump to the same point as in IntelliJ
;;       ;; Unfortunately, IntelliJ doesn't always supply the values
;;       ;; depending on where the open is invoked from; e.g. keyboard
;;       ;; works, tab context doesn't
;;       (when (not (string= \"\" \"$line\"))
;;         (goto-char (point-min))
;;         (forward-line (1- $2))
;;         (forward-char (1- $3)))
;;
;;       ;; Raise/focus our window; depends on the windowing system
;;       (if (string-equal system-type \"darwin\")
;;         (if window-system
;;             (shell-command \"/usr/bin/osascript -e 'tell application \\\"Emacs\\\" to activate'\")
;;           (shell-command \"/usr/bin/osascript -e 'tell application \\\"iTerm2\\\" to activate'\"))
;;         (raise-frame))
;;
;;       ;; Automatically pick up changes made in IntelliJ
;;       (auto-revert-mode t))"
;;--------------------------------------------------------------------

;; IntelliJ Preferences -> Tools -> External Tools
;; Name: Open In Emacs Advanced
;; Description: Load file in Emacs, Advanced version
;; Tool Settings:
;;   Program: openinemacs
;;   Arguments: $FilePath$ $LineNumber$ $ColumnNumber$
;;   Working directory: $FileDir$
;; Advanced Options:
;;   [x] Synchronize files after execution
;;   [ ] Open console for tool output

;;------------------------------------------------------------

;; C-d on minibuffer line end => delete word to a separator like `/',
;; M-C-d => convert relative path <=> absolute path
;; -> http://ko.meadowy.net/~shirai/diary/20030819.html#p01
(defvar minibuf-shrink-type0-chars '((w3m-input-url-history . (?/ ?+ ?:))
                                     (read-expression-history . (?\) ))
                                     (t . (?/ ?+ ?~ ?:)))
  "*minibuffer-history-variable とセパレータと見なす character の alist。
type0 はセパレータを残すもの。")

(defvar minibuf-shrink-type1-chars '((file-name-history . (?.))
                                     (w3m-input-url-history . (?# ?? ?& ?.))
                                     (t . (?- ?_ ?. ? )))
  "*minibuffer-history-variable とセパレータと見なす character の alist。
type1 はセパレータを消去するもの。")

(defun minibuf-shrink-get-chars (types)
  (or (cdr (assq minibuffer-history-variable types))
      (cdr (assq t types))))

(defun minibuf-shrink (&optional args)
  "point が buffer の最後なら 1 word 消去する。その他の場合は delete-char を起動する。
単語のセパレータは minibuf-shrink-type[01]-chars。"
  (interactive "p")
  (if (/= (if (fboundp 'field-end) (field-end) (point-max)) (point))
      (delete-char args)
    (let ((type0 (minibuf-shrink-get-chars minibuf-shrink-type0-chars))
          (type1 (minibuf-shrink-get-chars minibuf-shrink-type1-chars))
          (count (if (<= args 0) 1 args))
          char)
      (while (not (zerop count))
        (when (memq (setq char (char-before)) type0)
          (delete-char -1)
          (while (eq char (char-before))
            (delete-char -1)))
        (setq count (catch 'detect
                      (while (/= (if (fboundp 'field-beginning)
                                     (field-beginning) (point-min))
                                 (point))
                        (setq char (char-before))
                        (cond
                         ((memq char type0)
                          (throw 'detect (1- count)))
                         ((memq char type1)
                          (delete-char -1)
                          (while (eq char (char-before))
                            (delete-char -1))
                          (throw 'detect (1- count)))
                         (t (delete-char -1))))
                      ;; exit
                      0))))))

(defvar minibuf-expand-filename-original nil)
(defvar minibuf-expand-filename-begin nil)

(defun minibuf-expand-filename (&optional args)
  "file-name-history だったら minibuffer の内容を expand-file-name する。
連続して起動すると元に戻す。C-u 付きだと link を展開する。"
  (interactive "P")
  (when (eq minibuffer-history-variable 'file-name-history)
    (let* ((try-again (eq last-command this-command))
           (beg (cond
                 ;; Emacs21.3.50 + ange-ftp だと2回目に変になる
                 ((and try-again minibuf-expand-filename-begin)
                  minibuf-expand-filename-begin)
                 ((fboundp 'field-beginning) (field-beginning))
                 (t (point-min))))
           (end (if (fboundp 'field-end) (field-end) (point-max)))
           (file (buffer-substring-no-properties beg end))
           (remote (when (string-match "^\\(/[^:/]+:\\)/" file)
                     (match-string 1 file)))
           (home (if (string-match "^\\(/[^:/]+:\\)/" file)
                     (expand-file-name (format "%s~" (match-string 1 file)))
                   (expand-file-name "~"))))
      (unless try-again
        (setq minibuf-expand-filename-begin beg))
      (cond
       ((and args try-again minibuf-expand-filename-original)
        (setq file (file-chase-links (expand-file-name file))))
       (args
        (setq minibuf-expand-filename-original file)
        (setq file (file-chase-links (expand-file-name file))))
       ((and try-again minibuf-expand-filename-original)
        (setq file minibuf-expand-filename-original)
        (setq minibuf-expand-filename-original nil))
       (t
        (setq minibuf-expand-filename-original file)
        (if (string-match (concat "^" (regexp-quote home)) file)
            (if remote
                (setq file (concat remote "~" (substring file (match-end 0))))
              (setq file (concat "~" (substring file (match-end 0)))))
          (setq file (expand-file-name file)))))
      (delete-region beg end)
      (insert file))))

(mapc (lambda (map)
        (define-key map "\C-d" 'minibuf-shrink)
        (define-key map "\M-\C-d" 'minibuf-expand-filename))
      (delq nil (list (and (boundp 'minibuffer-local-map)
                           minibuffer-local-map)
                      (and (boundp 'minibuffer-local-ns-map)
                           minibuffer-local-ns-map)
                      (and (boundp 'minibuffer-local-completion-map)
                           minibuffer-local-completion-map)
                      (and (boundp 'minibuffer-local-must-match-map)
                           minibuffer-local-must-match-map))))

;;------------------------------------------------------------

;; atton/east-asian-ambiguous.el
;; -> https://gist.github.com/atton/9580569

; east asian ambiguous character table
(defun east-asian-ambiguous-characters ()
  '(
    (#x00A1 . #x00A1) (#x00A4 . #x00A4) (#x00A7 . #x00A8)
    (#x00AA . #x00AA) (#x00AD . #x00AE) (#x00B0 . #x00B4)
    (#x00B6 . #x00BA) (#x00BC . #x00BF) (#x00C6 . #x00C6)
    (#x00D0 . #x00D0) (#x00D7 . #x00D8) (#x00DE . #x00E1)
    (#x00E6 . #x00E6) (#x00E8 . #x00EA) (#x00EC . #x00ED)
    (#x00F0 . #x00F0) (#x00F2 . #x00F3) (#x00F7 . #x00FA)
    (#x00FC . #x00FC) (#x00FE . #x00FE) (#x0101 . #x0101)
    (#x0111 . #x0111) (#x0113 . #x0113) (#x011B . #x011B)
    (#x0126 . #x0127) (#x012B . #x012B) (#x0131 . #x0133)
    (#x0138 . #x0138) (#x013F . #x0142) (#x0144 . #x0144)
    (#x0148 . #x014B) (#x014D . #x014D) (#x0152 . #x0153)
    (#x0166 . #x0167) (#x016B . #x016B) (#x01CE . #x01CE)
    (#x01D0 . #x01D0) (#x01D2 . #x01D2) (#x01D4 . #x01D4)
    (#x01D6 . #x01D6) (#x01D8 . #x01D8) (#x01DA . #x01DA)
    (#x01DC . #x01DC) (#x0251 . #x0251) (#x0261 . #x0261)
    (#x02C4 . #x02C4) (#x02C7 . #x02C7) (#x02C9 . #x02CB)
    (#x02CD . #x02CD) (#x02D0 . #x02D0) (#x02D8 . #x02DB)
    (#x02DD . #x02DD) (#x02DF . #x02DF) (#x0300 . #x036F)
    (#x0391 . #x03A9) (#x03B1 . #x03C1) (#x03C3 . #x03C9)
    (#x0401 . #x0401) (#x0410 . #x044F) (#x0451 . #x0451)
    (#x2010 . #x2010) (#x2013 . #x2016) (#x2018 . #x2019)
    (#x201C . #x201D) (#x2020 . #x2022) (#x2024 . #x2027)
    (#x2030 . #x2030) (#x2032 . #x2033) (#x2035 . #x2035)
    (#x203B . #x203B) (#x203E . #x203E) (#x2074 . #x2074)
    (#x207F . #x207F) (#x2081 . #x2084) (#x20AC . #x20AC)
    (#x2103 . #x2103) (#x2105 . #x2105) (#x2109 . #x2109)
    (#x2113 . #x2113) (#x2116 . #x2116) (#x2121 . #x2122)
    (#x2126 . #x2126) (#x212B . #x212B) (#x2153 . #x2154)
    (#x215B . #x215E) (#x2160 . #x216B) (#x2170 . #x2179)
    (#x2190 . #x2199) (#x21B8 . #x21B9) (#x21D2 . #x21D2)
    (#x21D4 . #x21D4) (#x21E7 . #x21E7) (#x2200 . #x2200)
    (#x2202 . #x2203) (#x2207 . #x2208) (#x220B . #x220B)
    (#x220F . #x220F) (#x2211 . #x2211) (#x2215 . #x2215)
    (#x221A . #x221A) (#x221D . #x2220) (#x2223 . #x2223)
    (#x2225 . #x2225) (#x2227 . #x222C) (#x222E . #x222E)
    (#x2234 . #x2237) (#x223C . #x223D) (#x2248 . #x2248)
    (#x224C . #x224C) (#x2252 . #x2252) (#x2260 . #x2261)
    (#x2264 . #x2267) (#x226A . #x226B) (#x226E . #x226F)
    (#x2282 . #x2283) (#x2286 . #x2287) (#x2295 . #x2295)
    (#x2299 . #x2299) (#x22A5 . #x22A5) (#x22BF . #x22BF)
    (#x2312 . #x2312) (#x2460 . #x24E9) (#x24EB . #x254B)
    (#x2550 . #x2573) (#x2580 . #x258F) (#x2592 . #x2595)
    (#x25A0 . #x25A1) (#x25A3 . #x25A9) (#x25B2 . #x25B3)
    (#x25B6 . #x25B7) (#x25BC . #x25BD) (#x25C0 . #x25C1)
    (#x25C6 . #x25C8) (#x25CB . #x25CB) (#x25CE . #x25D1)
    (#x25E2 . #x25E5) (#x25EF . #x25EF) (#x2605 . #x2606)
    (#x2609 . #x2609) (#x260E . #x260F) (#x2614 . #x2615)
    (#x261C . #x261C) (#x261E . #x261E) (#x2640 . #x2640)
    (#x2642 . #x2642) (#x2660 . #x2661) (#x2663 . #x2665)
    (#x2667 . #x266A) (#x266C . #x266D) (#x266F . #x266F)
    (#x273D . #x273D) (#x2776 . #x277F) (#xE000 . #xF8FF)
    (#xFE00 . #xFE0F) (#xFFE0 . #xFFE6) (#xFFFD . #xFFFD)))

; setting function
(defun set-east-asian-ambiguous-width (width)
  (cond ((= emacs-major-version 22) (set-east-asian-ambiguous-width-22 width))
        ((> emacs-major-version 22) (set-east-asian-ambiguous-width-23 width))))

; for emacs 22
(defun set-east-asian-ambiguous-width-22 (width)
  (if (= width 2)
    (utf-translate-cjk-set-unicode-range (east-asian-ambiguous-characters))))

; for over 23 (checked work in emacs 24)
(defun set-east-asian-ambiguous-width-23 (width)
  (while (char-table-parent char-width-table)
         (setq char-width-table (char-table-parent char-width-table)))
  (let ((table (make-char-table nil)))
    (dolist (range (east-asian-ambiguous-characters))
      (set-char-table-range table range width))
    (optimize-char-table table)
    (set-char-table-parent table char-width-table)
    (setq char-width-table table)))

(set-east-asian-ambiguous-width 2)

;;------------------------------------------------------------

;;; macro expand function
;; -> https://emacs-jp.slack.com/archives/C6T2T9H4G/p1548855209448900

;; (defmacro p (form)
;;   "Output expand given FORM."
;;   `(progn
;;      (pp (macroexpand-1 ',form))
;;      nil))

;; (let ((use-package-expand-minimally t))
;;   (p (use-package helm)))
;; ;; => (require 'helm nil nil)

;; (let ((use-package-expand-minimally t))
;;   (p (use-package helm
;;      :bind (("M-x" . helm-M-x)))))
;; ;; => (progn
;; ;;      (unless
;; ;;          (fboundp 'helm-M-x)
;; ;;        (autoload #'helm-M-x "helm" nil t))
;; ;;      (bind-keys :package helm
;; ;;                 ("M-x" . helm-M-x)))

;; (let ((use-package-expand-minimally t))
;;   (p (use-package helm :demand t
;;      :bind (("M-x" . helm-M-x)))))
;; ;; => (progn
;; ;;      (require 'helm nil nil)
;; ;;      (bind-keys :package helm
;; ;;                 ("M-x" . helm-M-x)))
