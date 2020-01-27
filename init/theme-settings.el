;;; Theme settings
;; theme
(when (>= emacs-major-version 24)
  (load-theme 'tango-dark t))

;; -> https://github.com/syohex/emacs-syohex-theme/blob/master/syohex-theme.el
(custom-set-faces
 '(button ((t (:underline t :foreground "cyan1"))))
 ;; '(default ((t (:background "black" :foreground "white"))))
 '(cursor ((t (:foreground "white" :background "chartreuse2"))))

 ;; '(region ((t (:background "blue3"))))
 '(region ((t (:background "brightred"))))

 '(font-lock-builtin-face ((t (:foreground "LightSteelBlue"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "chocolate1"))))
 ;; '(font-lock-comment-face ((t (:foreground "chocolate1"))))
 '(font-lock-constant-face ((t (:foreground "Aquamarine"))))
 '(font-lock-doc-face ((t (:foreground "LightSalmon"))))
 '(font-lock-function-name-face ((t (:foreground "LightSkyBlue"))))
 '(font-lock-keyword-face ((t (:foreground "Cyan1"))))
 '(font-lock-negation-char-face ((t (nil))))
 '(font-lock-preprocessor-face ((t (:foreground "LightSteelBlue"))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "#ff1493":weight bold))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "#ff8c00" :weight bold))))
 '(font-lock-string-face ((t (:foreground "LightSalmon"))))
 '(font-lock-type-face ((t (:foreground "PaleGreen"))))
 '(font-lock-variable-name-face ((t (:foreground "LightGoldenrod"))))
 '(font-lock-warning-face ((t (:weight bold :foreground "Pink"))))
 '(minibuffer-prompt ((t (:foreground "cyan"))))

 '(completions-annotations ((t (:underline t))))
 '(completions-common-part ((t (:foreground "white" :background "black"))))
 '(completions-first-difference ((t (:weight bold))))
 '(dired-directory ((t (:foreground "LightSkyBlue"))))
 '(dired-flagged ((t (:weight bold :foreground "Pink"))))
 '(dired-header ((t (:foreground "PaleGreen"))))
 '(dired-ignored ((t (:foreground "grey70"))))
 '(dired-mark ((t (:foreground "Aquamarine"))))
 '(dired-marked ((t (:weight bold :foreground "DarkOrange"))))
 '(dired-perm-write ((t (:foreground "chocolate1"))))
 '(dired-symlink ((t (:foreground "Cyan1"))))
 '(dired-warning ((t (:foreground "Pink" :weight bold))))
 '(error ((t (:foreground "pink" :weight bold))))
 '(escape-glyph ((t (:foreground "cyan"))))
 '(file-name-shadow ((t (:foreground "grey70"))))

 ;;'(fringe ((t (:background "grey10"))))
 '(fringe ((t (:foreground "grey" :background "grey30"))))

 '(glyphless-char ((t (:height 0.6))))
 '(header-line ((t (:box (:line-width -1 :style released-button)
                         :background "grey20" :foreground "grey90" :box nil))))
 '(help-argument-name ((t (nil))))

 ;;'(highlight ((t (:background "darkolivegreen"))))
 '(highlight ((t (:background "magenta" :foreground "white"))))

 '(isearch ((t (:background "palevioletred2" :foreground "brown4"))))
 '(isearch-fail ((t (:background "red4"))))
 '(italic ((t (:underline t))))

 ;;'(lazy-highlight ((t (:background "paleturquoise4"))))
 '(lazy-highlight ((t (:background "grey30" :foreground "white"))))

 '(link ((t (:foreground "cyan1" :underline t))))
 '(link-visited ((t (:underline t :foreground "violet"))))
 '(match ((t (:background "RoyalBlue3"))))
 '(menu ((t (nil))))

 ;;'(mode-line ((t (:background "#333333" :foreground "#cccccd"))))
 '(mode-line ((t (:background "dark slate blue" :foreground "#cccccd"))))

 '(mode-line-buffer-id ((t (:weight bold :foreground "orange"))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((t (:box (:line-width 2 :color "grey40"
                                              :style released-button)))))
 '(mode-line-inactive ((t (:background "grey30" :foreground "grey80"
                                       :box (:line-width -1 :color "grey40" :style nil)
                                       :weight light))))

 '(mouse ((t (nil))))
 '(next-error ((t (:background "blue3"))))
 '(nobreak-space ((t (:foreground "cyan" :underline t))))
 '(query-replace ((t (:foreground "brown4" :background "palevioletred2"))))
 '(scroll-bar ((t (nil))))
 '(secondary-selection ((t (:background "SkyBlue4"))))
 '(shadow ((t (:foreground "grey70"))))
 '(success ((t (:foreground "Green1" :weight bold))))
 '(tool-bar ((t (:background "grey75" :foreground "black"
                             :box (:line-width 1 :style released-button)))))
 '(tooltip ((t (:background "lightyellow" :foreground "black"))))
 '(trailing-whitespace ((t (:background "red1"))))
 '(underline ((t (:underline t))))
 '(vertical-border ((t (nil))))
 '(warning ((t (:foreground "DarkOrange" :weight bold))))
 '(widget-button ((t (:weight bold))))
 '(widget-button-pressed ((t (:foreground "red1"))))
 '(widget-documentation ((t (:foreground "lime green"))))
 '(widget-field ((t (:background "dim gray"))))
 '(widget-inactive ((t (:foreground "grey70"))))

 '(which-func ((t (:foreground "chartreuse1" :weight bold))))
 '(comint-highlight-input ((t (:foreground "grey80" :weight semi-bold))))

 '(flycheck-info ((t (:underline (:style wave :color "green")))))
 '(flycheck-error ((t (:foreground "yellow" :background "red" :weight bold))))
 '(flycheck-warning ((t (:foreground nil :background nil :underline "darkorange"
                                     :weight bold))))
 '(flycheck-error-list-highlight ((t (:background "grey15"))))

 '(diff-added ((t (:background nil :foreground "green" :weight normal))))
 '(diff-removed ((t (:background nil :foreground "firebrick1" :weight normal))))
 '(diff-file-header ((t (:background nil :weight extra-bold))))
 '(diff-refine-added ((t (:background nil :underline "green"))))
 '(diff-refine-removed ((t (:background nil :underline "red"))))
 '(diff-refine-changed ((t (:background nil))))
 '(diff-header ((t (:background nil :weight extra-bold))))
 '(diff-hunk-header ((t (:foreground "turquoise" :weight extra-bold :underline t))))

 '(helm-gtags-file ((t (:foreground "aquamarine1"))))
 '(helm-gtags-lineno ((t (:foreground "IndianRed1" :underline nil))))

 '(cperl-array-face ((t (:background nil :foreground "yellow" :weight normal))))
 '(cperl-hash-face ((t (:foreground "DarkOliveGreen3" :background nil :weight normal))))

 ;;'(show-paren-match ((t (:foreground nil :underline "#ffff00" :weight extra-bold))))
 '(show-paren-match ((t (:background "color-237"))))

 '(org-block ((t (:foreground "green"))))
 '(org-tag ((t (:foreground "GreenYellow"))))
 '(org-checkbox ((t (:foreground "LawnGreen" :weight bold))))
 '(org-warning ((t (:foreground "hotpink" :weight bold))))
 '(org-level-1 ((t (:foreground "hotpink" :weight bold))))
 '(org-level-2 ((t (:foreground "LightGoldenrod" :weight semi-bold))))
 '(org-level-3 ((t (:foreground "LightGreen" :weight semi-bold))))
 '(org-level-4 ((t (:foreground "LightCyan"))))
 '(org-level-5 ((t (:foreground "grey90"))))
 '(org-level-6 ((t (:foreground "grey80"))))
 '(org-level-7 ((t (:foreground "grey70"))))
 '(org-level-8 ((t (:foreground "grey60"))))
 '(org-mode-line-clock ((t :foreground "#cccccd" :weight semi-bold)))
 '(org-priority ((t :foreground "orange" :weight normal)))
 '(org-agenda-dimmed-todo-face ((t :foreground "grey70")))

 '(compilation-error ((t (:underline nil))))
 '(compilation-line-number ((t (:underline t))))
 '(compilation-mode-line-exit ((t (:foreground "SpringGreen1" :weight semi-bold))))
 '(compilation-mode-line-fail ((t (:foreground "IndianRed1" :weight semi-bold))))

 '(eshell-prompt ((t (:foreground "yellow" :weight bold))))

 '(eldoc-highlight-function-argument ((t (:foreground "green" :underline t :weight bold))))
 '(anzu-mode-line ((t (:foreground "yellow" :weight bold))))

 '(git-gutter:deleted ((t (:background "red"))))
 '(git-gutter:modified ((t (:background "magenta"))))

 '(magit-hash ((t (:foreground "white" :weight bold))))
 '(magit-branch ((t (:foreground "yellow" :weight bold :underline t))))
 '(magit-item-highlight ((t (:background "gray3" :weight normal))))

 '(git-rebase-hash ((t (:foreground "yellow"))))

 '(helm-source-header ((t (:background "RoyalBlue4" :weight semi-bold :family nil :height 1.0))))
 '(helm-ff-file ((t (:foreground "white" :background nil))))
 '(helm-ff-directory ((t (:foreground "cyan" :background nil :underline t))))
 '(helm-grep-lineno ((t (:foreground "IndianRed1"))))
 '(helm-moccur-buffer ((t (:foreground "aquamarine1" :underline nil))))

 '(popup-scroll-bar-foreground-face ((t (:background "DarkOrange"))))

 '(company-tooltip ((t (:foreground "black" :background "lightgrey"))))
 '(company-tooltip-common ((t (:foreground "black" :background "lightgrey"))))
 '(company-tooltip-common-selection ((t (:foreground "white" :background "steelblue"))))
 '(company-tooltip-selection ((t (:foreground "black" :background "steelblue"))))
 '(company-preview-common ((t (:background nil :foreground "lightgrey" :underline t))))
 '(company-scrollbar-fg ((t (:background "orange"))))
 '(company-scrollbar-bg ((t (:background "gray40"))))
 '(company-template-field ((t (:background nil :foreground "yellow"))))
 '(company-tooltip-annotation ((t (:background nil :foreground "dimgray"))))

 '(flyspell-duplicate ((t (:foreground "white" :background "orange" :underline nil weight bold))))
 '(flyspell-incorrect ((t (:foreground "white" :background "red" :underline nil :weight bold))))

 '(jedi:highlight-function-argument ((t (:foreground "green"))))

 '(vc-edited-state ((t (:foreground "tomato" :weight bold))))
 '(vc-locally-added-state ((t :foreground "GreenYellow" :weight bold)))

 ;; custom
 '(vc-up-to-date-state ((t (:foreground "cyan" :weight bold))))

 '(magit-diff-added-highlight ((t (:foreground "#ccffcc" :background "#227722"))))
 '(magit-diff-removed-highlight ((t (:foreground "#ffcccc" :background "#772222"))))

 '(linum ((t :foreground "cyan" :height 0.75)))

 '(ediff-current-diff-A ((t (:background "color-28"))))
 '(ediff-current-diff-B ((t (:background "color-28"))))
 '(ediff-current-diff-C ((t (:background "color-28"))))
 '(ediff-even-diff-C ((t (:background "color-23"))))
 '(ediff-odd-diff-C ((t (:background "brightblack"))))
 '(ediff-fine-diff-C ((t (:background "yellow"))))
 '(ediff-even-diff-Ancestor ((t (:background "black"))))

 '(org-pomodoro-mode-line ((t (:bold t :foreground "#FFFFFF" :background "#FC42A0")))) ;; default: (:foreground "tomato1")
 '(org-pomodoro-mode-line-break ((t (:bold t :foreground "#FFFFFF" :background "#17B005")))) ;; default: (:foreground "#2aa198")

 '(web-mode-doctype-face ((t (:foreground "#82AE46"))))                 ; doctype
 '(web-mode-html-tag-face ((t (:foreground "#E6B422" :weight bold))))   ; tag
 '(web-mode-html-attr-name-face ((t (:foreground "#C97586"))))          ; attr-name
 '(web-mode-html-attr-value-face ((t (:foreground "#82AE46"))))         ; attr-value
 '(web-mode-comment-face ((t (:foreground "#999999"))))                 ; comment
 '(web-mode-server-comment-face ((t (:foreground "#999999"))))          ; comment
 '(web-mode-css-rule-face ((t (:foreground "#A0D8EF"))))                ; css
 '(web-mode-css-pseudo-class-face ((t (:foreground "#FF7F00"))))        ; css pseudo class
 '(web-mode-css-at-rule-face ((t (:foreground "#FF7F00"))))             ; css
 '(web-mode-whitespace-face ((t (:foreground "SteelBlue" :underline t :background "dark slate gray"))))

 '(helm-selection ((t (:foreground "white" :background "magenta"))))
 '(helm-ff-dotted-directory ((t (:background "DimGray" :foreground "white"))))

 '(tabbar-default ((t (:foreground "ghost white" :background "gray50" :height 1.0))))
 '(tabbar-selected ((t (:foreground "ghost white" :background "dark slate blue" :height 1.0))))

 '(elscreen-tab-background-face ((t (:background "black" :height 1.0))))
 '(elscreen-tab-control-face ((t (:background "black" :foreground "ghost white" :height 1.0))))
 '(elscreen-tab-current-screen-face ((t (:background "dark slate blue" :foreground "ghost white" :underline t :height 1.0))))
 '(elscreen-tab-other-screen-face ((t (:background "gray50" :foreground "ghost white" :height 1.0))))

 ) ;; custom-set-faces
