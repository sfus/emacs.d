;;;; recentf-ext
(custom-set-variables
 '(recentf-max-saved-items 2000)
 '(recentf-auto-cleanup 600)
 '(recentf-exclude '("recentf" "/elpa/" "/elisps/" "\\`/tmp/" "/\\.git/" "/\\.cask/"
                     "/tmp/gomi/" "/el-get/" ".loaddefs.el" "/\\.cpanm/"
                     "\\.mime-example" "\\.ido.last" "woman_cache.el"
                     "\\`/proc/" "\\`/sys/"
                     "CMakeCache.txt" "/bookmarks" "\\.gz$"
                     "COMMIT_EDITMSG" "MERGE_MSG" "git-rebase-todo")))

;;(run-at-time t 600 #'editutil-recentf-save-list)

;;;;+ Extra
;; -> https://masutaka.net/chalow/2011-10-30-2.html
;; -> http://keisanbutsuriya.hateblo.jp/entry/2015/02/15/174758
(defmacro with-suppressed-message (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))
(run-with-idle-timer 30 t '(lambda ()          ;; 30秒ごとに .recentf を保存
   (with-suppressed-message (editutil-recentf-save-list))))
;; (run-with-idle-timer 30 t '(lambda ()          ;; 30秒ごとに .recentf を保存
;;    (with-suppressed-message (recentf-save-list))))
;;;;+

(recentf-mode 1)
