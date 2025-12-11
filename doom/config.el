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
;; (setq doom-theme 'jetbrains-darcula)
;; (setq doom-theme 'doom-gruvbox)
;; (setq doom-theme nil)
;; (setq doom-theme 'doom-dracula)
;; (setq doom-theme 'doom-spacegrey)
(setq doom-theme 'atom-one-dark)

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
  (setq lsp-clangd-binary-path "/usr/bin/clangd")
  
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


;; Override LSP highlight faces to use bolding instead of underlining.
(custom-set-faces!
  ;; Face for tokens being read
  '(lsp-face-high;; light-read
    :inherit lsp-face-highlight-read
    :underline nil
    :weight bold)

  ;; Face for tokens being written
  '(lsp-face-highlight-write
    :inherit lsp-face-highlight-write
    :underline nil
    :weight bold)

  ;; Other semantic tokens might also need adjustment.
  ;; For example, you may want to apply bold to parameters.
  '(lsp-semantic-token-face-parameter
    :inherit lsp-semantic-token-face-parameter
    :underline nil
    :weight bold))

(defun my-force-vertical-split-for-python-advice (orig-fn &rest args)
  "Force `run-python` to open the REPL in a vertical split."
  (let ((split-width-threshold nil)
        (split-height-threshold 0))
    (apply orig-fn args)))

(advice-add #'python-shell-get-or-create-buffer :around #'my-force-vertical-split-for-python-advice)

;; (after! lsp-mode
;;   (setq lsp-disabled-clients '(ruff-lsp ruff))
;;   ;; Ensure pylsp is preferred
;;   (setq lsp-pylsp-server-command "pylsp"))

;; (after! hl-line)
;; (custom-set-faces!
;;   ;;'(hl-line :background "#87CEEB"))  ; Powder blue
;;   '(hl-line :background "#5c5c5c"))

;; (custom-set-faces
;;  '(vertico-current ((t (:background "#5c5c5c")))))

(defun +magit/status-split-right ()
  "Open Magit status in a right split."
  (interactive)
  (select-window (split-window-right))
  (magit-status))

;; Enable clipboard integration
(setq select-enable-clipboard t)
(setq select-enable-primary t)

;; (after! cc-mode
;;   (setq-hook! '(c-mode-hook c++-mode-hook objc-mode-hook)
;;     +format-on-save-enabled-modes
;;     (remove 'c-mode
;;             (remove 'c++-mode
;;                     (remove 'objc-mode +format-on-save-enabled-modes)))))

(use-package! writeroom-mode
  :commands writeroom-mode
  :config
  (setq writeroom-width 100                    ; Text column width
        writeroom-mode-line t                  ; Keep modeline visible
        writeroom-global-effects nil           ; Don't go fullscreen
        writeroom-fringes-outside-margins nil) ; Keep fringes inside

  ;; Optional: bind to a key
  (map! :leader "t w" #'writeroom-mode))

(defun my/docker-build ()
  "Build project using Docker"
  (interactive)
  (let ((default-directory (projectile-project-root))
        (compilation-buffer-name-function (lambda (_) "*docker-build*")))
    (compile "docker run -v ./:/checkout -it -t ghcr.io/lightmatter-ai/system-software-images/congo-ops-image:latest /checkout/scripts/test-build.sh --xmr-mode-hw kenya_fpga")))

(map! :leader
      :desc "Build with Docker"
      "b b" #'my/docker-build)

(setq ef-themes-disable-other-themes t)

(defun my/format-git-diff ()
  "Format the changes in the last commit using clang-format-diff.py"
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (shell-command "git diff -U0 --no-color --relative HEAD^ | clang-format-diff.py -p1 -i")
    (message "Formatted git diff with clang-format")))

(global-display-fill-column-indicator-mode -1)

(setq fill-column 80)
(require 'fill-column-indicator)

(setq fci-rule-color "green")
(setq fci-rule-character ?|)
(setq fci-rule-use-dashes t)  ;; This gives you dashed lines
(setq fci-dash-pattern 0.5)   ;; Adjust dash density if needed

(add-hook 'prog-mode-hook 'fci-mode)

(defun my/save-buffer-no-formatting ()
  "Save the current buffer without running any formatting hooks.
This bypasses before-save-hook, format-all-mode, and other formatters."
  (interactive)
  (let ((before-save-hook nil)
        (after-save-hook nil))
    ;; Temporarily disable format-all if it's active
    (when (bound-and-true-p format-all-mode)
      (format-all-mode -1)
      (save-buffer)
      (format-all-mode 1))
    ;; If format-all isn't active, just save with hooks disabled
    (unless (bound-and-true-p format-all-mode)
      (save-buffer))))
