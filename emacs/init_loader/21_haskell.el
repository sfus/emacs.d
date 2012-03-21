;; setting haskell language
(add-to-list 'load-path "~/.cabal/share/ghc-mod")
(setq ghc-module-command "~/.cabal/bin/ghc-mod")

(autoload 'ghc-init "ghc" nil t)
(load "haskell-site-file") ;; load haskell-mode/haskell-site-file.el

(add-to-list 'auto-mode-alist
         '("\\.\\(hs\\|hi\\|gs\\)$" . haskell-mode))

(defun haskell-individual-setup ()
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indent)
  (ghc-init)
  (flymake-mode))

(add-hook 'haskell-mode-hook 'haskell-individual-setup)
