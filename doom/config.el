;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'modus-operandi)
;;(setq doom-theme 'jetbrains-darcula)
;;(setq doom-theme 'doom-gruvbox)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(after! lsp-clangd
  ;; Set clangd binary path (if not in PATH)
  ;; (setq lsp-clangd-binary-path "/usr/bin/clangd")
  
  ;; clangd arguments
  (setq lsp-clangd-server-args
        '("--background-index"
          "--clang-tidy"
          "--completion-style=detailed"
          "--header-insertion=iwyu"
          "--header-insertion-decorators"))
  
  ;; Enable clangd for C/C++ modes
  (set-lsp-priority! 'clangd 1))

;; Optional: LSP performance tweaks
(after! lsp-mode
  (setq lsp-idle-delay 0.1
        lsp-enable-file-watchers nil
        lsp-enable-folding nil
        lsp-enable-symbol-highlighting t
        lsp-enable-snippet t))

;; Detect if running in tmux
(defun running-in-tmux-p ()
  "Check if Emacs is running inside tmux."
  (getenv "TMUX"))

;; Disable ESC bindings when in tmux
(when (running-in-tmux-p)
  (after! evil
    ;; Remove ESC from key translation
    (define-key key-translation-map [escape] nil)
    (define-key function-key-map [escape] nil)

    ;; Make ESC do nothing or just normal state
    (define-key evil-normal-state-map [escape] 'ignore)
    (define-key evil-insert-state-map [escape] 'evil-normal-state)
    (define-key evil-visual-state-map [escape] 'evil-normal-state)

    ;; Prevent ESC from being Meta
    (setq evil-escape-delay 0)))

;; Map Super+b to toggle the Treemacs project drawer
(map! "M-B" #'+treemacs/toggle)

(after! lsp-faces
  ;; Face for read access (e.g., using a variable)
  (set-face-attribute 'lsp-document-highlight-read nil
                      :weight 'bold      ;; Set the font weight to bold
                      :underline nil)   ;; Explicitly remove the underline

  ;; Face for write access (e.g., defining or assigning a variable)
  (set-face-attribute 'lsp-document-highlight-write nil
                      :weight 'bold      ;; Set the font weight to bold
                      :underline nil))  ;; Explicitly remove the underline

(defun my-force-vertical-split-for-python-advice (orig-fn &rest args)
  "Force `run-python` to open the REPL in a vertical split."
  (let ((split-width-threshold nil)
        (split-height-threshold 0))
    (apply orig-fn args)))

(advice-add #'python-shell-get-or-create-buffer :around #'my-force-vertical-split-for-python-advice)

(after! lsp-mode
  (setq lsp-disabled-clients '(ruff-lsp ruff))
  ;; Ensure pylsp is preferred
  (setq lsp-pylsp-server-command "pylsp"))

;; (after! hl-line)
;; (custom-set-faces!
;;   '(hl-line :background "#87CEEB"))  ; Powder blue

(defun +magit/status-split-right ()
  "Open Magit status in a right split."
  (interactive)
  (select-window (split-window-right))
  (magit-status))
