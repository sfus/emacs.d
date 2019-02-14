;;;; Window configuration

;; popwin
(require 'popwin)

;; ;; comment out to avoid always open in other window
;; (defvar popwin:special-display-config-backup popwin:special-display-config)
;; (custom-set-variables
;;  '(display-buffer-function 'popwin:display-buffer))

(popwin-mode 1)

;; (custom-set-variables
;;  '(special-display-function 'popwin:special-display-popup-window)
;;  ;;'(special-display-buffer-names '("*Help*"))
;;  '(special-display-regexps '("\*Help\*"))
;;  )

;; remove from default config
(dolist (stuff '("*vc-diff*" "*vc-change-log*"))
  (delete stuff popwin:special-display-config))

;; basic
(push '("*Help*" :stick t :noselect t) popwin:special-display-config)
(push '("*sdic*") popwin:special-display-config)

;; Ruby
(push '("*ri-doc*" :stick t :height 20) popwin:special-display-config)
(push '(inf-ruby-mode :stick t :height 20) popwin:special-display-config)

;; python
(push '("*Python*"   :stick t) popwin:special-display-config)
(push '("*Python Help*" :stick t :height 20) popwin:special-display-config)
(push '("*jedi:doc*" :stick t :noselect t) popwin:special-display-config)

;; Go
(push '("^\*go-direx:" :position left :width 0.3 :dedicated t :stick t :regexp t)
      popwin:special-display-config)

;; flycheck
(push '(flycheck-error-list-mode :stick t) popwin:special-display-config)

;; CoffeeScript
(push '("*CoffeeREPL*" :stick t) popwin:special-display-config)

;; Clojure
(push '(cider-repl-mode :stick t) popwin:special-display-config)


;;;;+ Extra
;; ;;++
;; ;; Direx
;; (push '(direx:direx-mode :position left :width 40 :dedicated t) popwin:special-display-config)

;; Slime
;;   Apropos
(push '("*slime-apropos*") popwin:special-display-config)
;;   Macroexpand
(push '("*slime-macroexpansion*") popwin:special-display-config)
;;   Help
(push '("*slime-description*") popwin:special-display-config)
;;   Compilation
(push '("*slime-compilation*" :noselect t) popwin:special-display-config)
;;   Cross-reference
(push '("*slime-xref*") popwin:special-display-config)
;;   Debugger
(push '(sldb-mode :stick t) popwin:special-display-config)
;;   REPL
(push '(slime-repl-mode) popwin:special-display-config)
;;   Connections
(push '(slime-connection-list-mode) popwin:special-display-config)

;;;;+
