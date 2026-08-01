# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Personal Emacs configuration (Emacs Lisp only, no application code). Requires Emacs 27.1 or higher (`init/theme-settings.el` uses `frame-focus-state`, added in 27.1) and `git`; the local install is 30.2. tree-sitter, `setopt`, and other 29+ APIs are not used.

The repository is designed to work in two ways at once:

- **As `~/.emacs.d`** — `./create_symlink.sh` symlinks `init.el`, `init.sh`, `init-loader/`, `my_snippets/`, `ac-dict/`, `init-el-get.el` into `~/.emacs.d/`.
- **As a self-contained sandbox** — `init.el:19-20` rewrites `user-emacs-directory` to the directory of the loaded `init.el` when started with `-l`. Every path derived from `user-emacs-directory` (snippets, ac-dict, elpa, el-get) therefore resolves inside the repo, so the config can be exercised without touching `~/.emacs.d`.

## Commands

```bash
emacs -Q -l init.el          # canonical way to launch this config (README)
./run.sh                     # same, terminal frame: emacs -nw -q -l init.el
./init.sh                    # headless load from ~/.emacs.d (installs/refreshes packages)
./create_symlink.sh          # install into ~/.emacs.d via symlinks
```

Checking a change:

- Startup errors: `M-x init-loader-show-log` (init-loader captures per-file load errors instead of aborting startup; a broken file can therefore fail silently).
- Startup time is printed automatically by `my/emacs-init-time` (`init.el:114-127`).
- Syntax-check edited files without launching Emacs:
  ```bash
  emacs -Q --batch --eval '(dolist (f (list "init/builtin-settings.el")) (with-temp-buffer (insert-file-contents f) (goto-char (point-min)) (condition-case e (progn (while (ignore-errors (read (current-buffer)) t)) (message "%s OK" f)) (error (message "%s PARSE ERROR: %S" f e)))))'
  ```
- Inspect how a `use-package` block actually expands (useful for `:hook`/`:bind` mistakes):
  ```bash
  emacs -Q --batch --eval '(progn (require (quote use-package)) (prin1 (macroexpand-all (quote (use-package display-line-numbers :defer t :hook ((prog-mode text-mode) . display-line-numbers-mode))))))'
  ```

There is **no test suite, no linter, and no byte-compile step** in this repository. Verification is manual: launch, open representative buffers, check the init-loader log.

## Architecture

### Boot sequence

`init.el` is bootstrap only — it configures nothing mode-specific:

1. GC threshold, `cl-lib`, the `C-h` rebind, `user-emacs-directory` rewrite, recursive `lisp/` load-path (not tracked in the repository; `create_symlink.sh` creates an empty `~/.emacs.d/lisp` for hand-placed elisp).
2. Optional `init-private.el` from `user-emacs-directory`, loaded if present. Not part of this repository and not created by `create_symlink.sh` (in the installed setup it is a hand-made symlink into a separate private dotfiles repository).
3. `package.el` + MELPA, then `use-package` (`use-package-enable-imenu-support` must stay set **before** `(require 'use-package)`).
4. `el-get` (load-path/recipe-path only), `exec-path-from-shell` (copies `PATH`, `VIRTUAL_ENV`, `GOROOT`, `GOPATH`, `EIJIRO_DIR`).
5. `init-loader-load` on `init-loader/`.
6. macOS `pbcopy`/`pbpaste` kill-ring integration, `server-start`, init-time reporting.

### `init-loader/` is a symlink layer — never edit it

Each file in `init-loader/` is a symlink into `init/`, and its numeric prefix decides load order:

| load order | symlink | real file | responsibility |
|---|---|---|---|
| 00 | `00_basic-settings.el` | `init/basic-setting.el` | encoding, `custom-set-variables` baseline (backups, scrolling, history), indentation defaults, frame chrome, `before-save-hook` whitespace trimming |
| 10 | `10_global-keys.el` | `init/global-keys.el` | raw global key remaps only (`global-set-key` / `define-key` on `global-map`, `ctl-x-map`) |
| 20 | `20_builtin-settings.el` | `init/builtin-settings.el` | `use-package` blocks for **built-in** packages (isearch, dired, paren, elec-pair, align, display-line-numbers, uniquify, …) |
| 30 | `30_custom-functions.el` | `init/custom-functions.el` | hand-written `defun`/`defadvice` that depend only on built-ins, each bound inline right below its definition |
| 40 | `40_extra-utils.el` | `init/extra-utils.el` | third-party editing/UI packages (helm, elscreen, popwin, avy, anzu, undo-tree, which-key, paredit, evil, …) |
| 50 | `50_dev-tools.el` | `init/dev-tools.el` | language-**agnostic** dev tooling: magit/vc, ediff, company + auto-complete, projectile, gtags, compile/quickrun, flycheck, yasnippet, editorconfig |
| 60 | `60_prog-langs.el` | `init/prog-langs.el` | per-language major modes and language-specific tooling (Go, C/C++, Perl/cperl, Python, Ruby, Rust, JS/js2, Haskell, Elixir, Erlang, Clojure, Scheme, Common Lisp, YAML, Markdown, PlantUML, SQL, sh, asm, conf, vimrc) plus `lsp-mode`/`dap-mode` (currently wired to Go only) |
| 70 | `70_org-settings.el` | `init/org-settings.el` | org-mode and its ecosystem |
| 99 | `99_theme-settings.el` | `init/theme-settings.el` | theme load + `custom-set-faces` overrides, loaded last so it wins |

Placement rules that matter when adding config:

- Cross-language infrastructure → `dev-tools.el`. Anything meaningful for exactly one language/major-mode → `prog-langs.el`.
- Own elisp with no third-party dependency → `custom-functions.el`. Third-party package setup → `extra-utils.el` (small helper `defun`s tightly coupled to one package live inside that package's block).
- Built-in package → `builtin-settings.el`; external package → `extra-utils.el` / `dev-tools.el` / `prog-langs.el`.

### Package management is split in two, and only one half is wired up

- **package.el / MELPA** via `use-package :ensure t` (~63 occurrences under `init/`). This is what actually installs packages at startup.
- **el-get** via `init-el-get.el` (~114 `el-get-bundle` forms, including `sfus/emacs-editutil`, `sfus/emacs-progutil`, themes, mozc). This half is dormant: **nothing in this repository loads `init-el-get.el`** (`init.el` only adds el-get's load-path), and no el-get checkout directory (`el-get/`) exists, so no `el-get-bundle` has ever run. Every package actually in use comes from package.el. Packages declared in both halves (anzu, elscreen, popwin, undo-tree, avy, paredit, helm) are therefore supplied by package.el, and `init-el-get.el` is best read as a historical inventory.
- Installed packages are not vendored: `elpa/`, `el-get/`, `straight/`, `elisps/`, plus runtime state (`history`, `recentf`, `places`, `ac-comphist.dat`) are gitignored.
- `my_snippets/` (yasnippet, wired at `init/dev-tools.el:538`) and `ac-dict/` (auto-complete, `init/dev-tools.el:288`) are resolved through `user-emacs-directory`, which is why they must be symlinked into `~/.emacs.d` for the installed setup.
- `init-el-get.el` is the merged form of an older two-file split: `init-el-get-extra.el` was folded into it in 2019 (`8522125 Prepare to use use-package`).
- Package downloads go through gnutls, which trusts **only** the files listed in `gnutls-trustfiles` and never the macOS keychain. On a network that terminates TLS with its own CA, `package-install` fails with `Could not create connection to melpa.org:443` while `curl` to the same host succeeds — a working `curl` proves nothing here, because curl reads the keychain and gnutls does not. Diagnose:
  ```bash
  # which trust files gnutls will actually use
  emacs -Q --batch --eval '(progn (require (quote gnutls)) (dolist (f gnutls-trustfiles) (when (file-exists-p f) (message "EXISTS %s" f))))'
  # does that bundle accept the host?
  openssl s_client -connect melpa.org:443 -servername melpa.org </dev/null 2>/dev/null -CAfile /etc/ssl/cert.pem | grep "Verify return code"
  ```
  `Verify return code: 20 (unable to get local issuer certificate)` means the chain is signed by a CA that bundle does not carry — a trust problem, not connectivity. The fix is to add that CA to `gnutls-trustfiles`. Keep the certificate in `certs/` and the setting in `init-private.el`; both are outside version control, so machine-specific trust never reaches this public repository.
- `test_emacs` is a standalone 11-line minimal settings file, unreferenced by anything else in the repo.

## Conventions

Match the surrounding file; the codebase is old enough that two generations of style coexist.

- **Section headers**: `;;; name` above each package/topic block, sometimes followed by reference URLs as comments.
- **`use-package` blocks** are closed with a trailing comment naming the package:
  ```elisp
  ;;; uniquify
  (use-package uniquify
    :defer t
    :config
    (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
    ) ;; uniquify
  ```
- **Naming**: `my/` (current) and `my-` (older) prefixes both appear; use `my/` for new code. Per-language hook functions follow `my/<lang>-mode-hook`.
- **`:hook` must use mode names without the `-hook` suffix.** use-package appends `-hook` when the symbol is unbound or ends in `-mode`, so `:hook (conf-mode-hook . foo)` expands to the non-existent `conf-mode-hook-hook` and silently does nothing for any package that is not already loaded. Write `:hook ((prog-mode text-mode conf-mode) . foo)`. Existing `foo-mode-hook` spellings in this repo happen to work only because those hook variables are preloaded.
- **Keybindings** live next to what they activate: inline `global-set-key` directly under the `defun` in `custom-functions.el`, `:bind` inside the package's own `use-package` block elsewhere, and `global-keys.el` exclusively for raw global remaps.
- **Terminal vs GUI branches** (`window-system`, `(eq system-type 'darwin)`) exist in several places; preserve both branches when editing.
- **Commented-out blocks are intentional history**, not cruft: the alternate Go setup and the TypeScript/tide + web-mode/emmet blocks in `prog-langs.el`, `helm-descbinds` marked `:disabled t`, and the mozc block. Do not delete or re-enable them silently.
- Comments and commit messages are English; user-facing discussion in this repo is Japanese.

## Keybindings that break normal Emacs assumptions

Fundamental keys are remapped; assume nothing about defaults when editing keymaps.

| key | bound to | note |
|---|---|---|
| `C-h` | `backward-delete-char-untabify` | set twice: `init.el:16` and `global-keys.el`; dired and isearch remap it locally too |
| `C-x C-h` | `help-command` | help prefix moved here |
| `C-m` (RET) | `newline-and-indent` | overridden again locally in dired/view/image/tar/archive modes |
| `M-SPC` / `M-/` | `dabbrev-expand` / `just-one-space` | swapped from defaults |
| `C-z` | unbound | default `suspend-frame` removed |
| `C-M-c` | `kill-current-buffer` | |
| `C-M-d` | `my/editutil-duplicate-thing` | |
| `C-M-o` | `other-window` | `split-line` moved to `C-x O` |
| `C-x C-b` | `ibuffer` | |

## Fragile areas

- **Legacy `defadvice`** (not `advice-add`) on `font-lock-mode`, `quit-window`, `help-follow` in `custom-functions.el`, and `defadvice server-execute` in `init.el`. They require the accompanying `ad-enable-advice`/`ad-activate` calls to stay intact.
- `advice-add :around` on `dired-up-directory` (`builtin-settings.el`), `direx:do-copy-files`, and `evil-paste-pop*` — these depend on exact upstream function names.
- Dired's `use-package` block nests several `with-eval-after-load` forms (view, image-mode, tar-mode, archive-mode) that restore `C-m` behavior; rewriting the block tends to drop them.
- `theme-settings.el` mutates the `default` face at runtime on frame focus changes (tmux-style active/inactive dimming) in addition to its static `custom-set-faces` block, so static `default` face edits get overwritten on the next focus event. Faces are raw hex/X11 colors with no abstraction, edited one by one. Active theme: `tokyo-night`.
- `mode-line-mule-info` is patched with a string-level `cl-substitute` on `"%z"` in `custom-functions.el`.
- `cperl-mode` is aliased over `perl-mode`, with a block of deliberately unset default bindings.
- Many features depend on external binaries: `rg` (helm grep), GNU Global (`gtags`/`ggtags`), `gofmt`/`goimports`/`dlv`, `clang-format`, `flake8` (`~/.config/flake8`), `sbcl`, `plantuml`, and macOS-only `pbcopy`/`pbpaste`/`osascript`.
