;; setting plantuml
(add-to-list 'auto-mode-alist '("\.puml$" . plantuml-mode))

(with-eval-after-load 'plantuml-mode
  (setq plantuml-jar-path (shell-command-to-string "cat `which plantuml` | awk '/plantuml.jar/ { print $(NF - 1)}' | tr -d '\n'"))
  (setq plantuml-output-type "txt")

  (defun my/plantuml-save-and-preview-other-window (&optional arg)
    (interactive "p")
    (save-buffer arg)
    (let ((win (get-buffer-window plantuml-preview-buffer)))
      (if win
          (delete-window win)))
    (plantuml-preview 4))

  (defun my/plantuml-save-and-preview-other-tmux-pane (&optional arg)
    (interactive "p")
    (save-buffer arg)
    (call-process-shell-command (format "tmux split-window -h 'cat %s | plantuml -p | imgcat && read'" buffer-file-name) nil 0))

  (defun my/plantuml-save-and-preview-os-command (&optional arg)
    (interactive "p")
    (save-buffer arg)
    (call-process-shell-command (format "plantuml %s && open %s.png" buffer-file-name (file-name-base buffer-file-name)) nil 0))

  (defun my/plantuml-mode-hook ()
    (define-key plantuml-mode-map (kbd "C-c C-s") 'my/plantuml-save-and-preview-other-window)
    (define-key plantuml-mode-map (kbd "C-c C-c") 'my/plantuml-save-and-preview-other-tmux-pane)
    (define-key plantuml-mode-map (kbd "C-c c") 'my/plantuml-save-and-preview-os-command))

  (add-hook 'plantuml-mode-hook 'my/plantuml-mode-hook)

  )
