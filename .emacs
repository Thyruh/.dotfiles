;;; ~/.emacs --- Clean, opinionated Emacs config
;;; Author: Thyruh
;;; Commentary:
;;; Structured into clear sections.  Safe to paste as your full ~/.emacs.

;;; ------------------------------
;;; Bootstrap packages
;;; ------------------------------

;; Load user customizations (optional)
;;; Code:
(load "~/.emacs-custom" 'noerror 'nomessage)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")))
(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;;; ------------------------------
;;; UI / UX
;;; ------------------------------

(require 'whitespace)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq whitespace-style '(face tabs spaces trailing lines-tail))
(global-whitespace-mode 1)

;; Quieter startup
(setq inhibit-startup-screen t
      initial-scratch-message nil
      initial-buffer-choice t
      use-dialog-box nil)

;; Minimal chrome
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Cursor (filled box, not hollow/rectangle)
(setq-default cursor-type 'box)
(setq-default cursor-in-non-selected-windows 'box)
(setq blink-cursor-mode t
      blink-cursor-interval 0.5)

;; Font per-OS (adjust the size if you want)
(defun rc/get-default-font ()
  (cond
   ((eq system-type 'windows-nt) "Consolas-13")
   ((eq system-type 'gnu/linux)  "Iosevka-20")
   (t "Monospace-14")))
(add-to-list 'default-frame-alist `(font . ,(rc/get-default-font)))

;; Frame transparency (global)
(defconst rc/frame-transparency 85)
(set-frame-parameter (selected-frame) 'alpha `(,rc/frame-transparency . ,rc/frame-transparency))
(add-to-list 'default-frame-alist `(alpha . (,rc/frame-transparency . ,rc/frame-transparency)))

;; Lines, parens, highlight
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)
(show-paren-mode 1)
(setq show-paren-delay 0)
(global-hl-line-mode 1)

;; Tabs & indentation
(setq-default tab-width 3
              indent-tabs-mode nil)

;; Theme
(use-package gruber-darker-theme
  :config (load-theme 'gruber-darker t))

;;; ------------------------------
;;; Core editing helpers
;;; ------------------------------

(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1))

;; Whitespace discipline: show + clean trailing on save
(defun rc/set-up-whitespace-handling ()
  (whitespace-mode 1)
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))
(add-hook 'prog-mode-hook #'rc/set-up-whitespace-handling)
(add-hook 'text-mode-hook #'rc/set-up-whitespace-handling)

(toggle-word-wrap 1)

(use-package multiple-cursors
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)))

;; Fast select helpers
(defun rc/select-word ()
  "Select the current word (symbols count)."
  (interactive)
  (skip-syntax-backward "w_")
  (set-mark (point))
  (skip-syntax-forward "w_"))

(defun rc/select-sentence ()
  "Select current sentence."
  (interactive)
  (backward-sentence 1)
  (set-mark (point))
  (forward-sentence 1))

(defun rc/select-current-line ()
  "Select the whole current line."
  (interactive)
  (move-beginning-of-line 1)
  (set-mark (point))
  (move-end-of-line 1))

;;; ------------------------------
;;; Completion, search, diagnostics
;;; ------------------------------

(use-package ivy :config (ivy-mode 1))
(use-package counsel :after ivy :config (counsel-mode 1))
(use-package company :config (global-company-mode 1))
(use-package flycheck :init (global-flycheck-mode 1))

;; LSP: no auto-formatting; explicit control only
(use-package lsp-mode
  :init
  (setq lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil)
  :hook ((go-mode c-mode c++-mode python-mode rust-mode haskell-mode) . lsp-deferred)
  :commands (lsp lsp-deferred))

(use-package lsp-ui :after lsp-mode :commands lsp-ui-mode)
(use-package lsp-ivy :after (lsp-mode ivy) :commands lsp-ivy-workspace-symbol)

;;; ------------------------------
;;; Tree-sitter (works both on Emacs 28/29)
;;; ------------------------------

;; Prefer external tree-sitter if needed
(use-package tree-sitter :defer t)
(use-package tree-sitter-langs :after tree-sitter :defer t)
(when (require 'tree-sitter nil 'noerror)
  (global-tree-sitter-mode 1)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;;; ------------------------------
;;; Dired
;;; ------------------------------

(require 'dired-x)

(setq dired-listing-switches "-alFhG --group-directories-first"
      dired-dwim-target t
      dired-mouse-drag-files t)

(add-hook 'dired-mode-hook #'dired-omit-mode)
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))

;;; ------------------------------
;;; Compilation workflow
;;; ------------------------------

;; Compilation window at bottom, fixed height
(setq display-buffer-alist
      '(("\\*compilation\\*"
         (display-buffer-reuse-window display-buffer-at-bottom)
         (window-height . 15))))

;; Empty default compile command when using M-x compile
(global-set-key (kbd "C-c C-c") 'compile)
(setq compile-command ""
      compilation-read-command t)

;; Colored output in *compilation*
(require 'ansi-color)
(defun rc/colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point))))
(add-hook 'compilation-filter-hook #'rc/colorize-compilation-buffer)

;; One-key compile of current file into sibling bin/
(defun rc/compile-file-to-bin ()
  "Compile current buffer into bin/ alongside the file.
Supports: .cpp (g++), .go (go build), .hs (ghc)."
  (interactive)
  (unless buffer-file-name (user-error "Buffer not visiting a file"))
  (let* ((file buffer-file-name)
         (dir  (file-name-directory file))
         (bin  (expand-file-name "bin" dir))
         (out  (expand-file-name (file-name-sans-extension (file-name-nondirectory file)) bin)))
    (unless (file-directory-p bin)
      (make-directory bin))
    (cond
     ((string-match-p "\\.cpp$" file)
      (compile (format "g++ -O2 -std=c++20 -o %s %s" (shell-quote-argument out) (shell-quote-argument file))))
     ((string-match-p "\\.go$" file)
      (compile (format "go build -o %s %s" (shell-quote-argument out) (shell-quote-argument file))))
     ((string-match-p "\\.hs$" file)
      (compile (format "ghc --make %s -o %s" (shell-quote-argument file) (shell-quote-argument out))))
     (t (user-error "Unsupported file type: %s" file)))))

(global-set-key (kbd "C-c c") #'rc/compile-file-to-bin)

;;; ------------------------------
;;; Magit
;;; ------------------------------

(use-package magit
  :commands (magit-status magit-log-all)
  :config
  ;; Disable auto-revert inside Magit if you dislike the churn
  (when (fboundp 'magit-auto-revert-mode)
    (magit-auto-revert-mode -1)))

;;; ------------------------------
;;; Quality-of-life keys
;;; ------------------------------

(define-prefix-command 'ralt-map)
(global-set-key (kbd "<Alt_R>") 'ralt-map)

(define-key ralt-map (kbd "g") #'lsp-find-definition)
(define-key ralt-map (kbd "f") #'lsp-ivy-workspace-symbol)

;; Vim-like window cycling
(global-set-key (kbd "C-x C-w") 'other-window)

;; Global escape quits
(global-set-key (kbd "<escape>") #'keyboard-escape-quit)

;; Ivy: quick buffer switch
(global-set-key (kbd "C-,") #'counsel-switch-buffer)

;; Fast selections
(global-set-key (kbd "C-c w") #'rc/select-word)
(global-set-key (kbd "C-c s") #'rc/select-sentence)
(global-set-key (kbd "C-c l") #'rc/select-current-line)

;; Functions for testing
(defun my-left-alt-action ()
  (interactive)
  (message "Left Alt pressed"))

(defun my-right-alt-action ()
  (interactive)
  (message "Right Alt pressed"))

;; Key translations
(define-key key-translation-map (kbd "<Alt_L>") 'my-left-alt-action)
(define-key key-translation-map (kbd "<Alt_R>") 'my-right-alt-action)

;; LSP quick keys (explicit; no guessing)
(global-set-key (kbd "C-c g d") #'lsp-find-definition)
(global-set-key (kbd "C-c g i") #'lsp-ivy-workspace-symbol)

;; Shell on demand
(global-set-key (kbd "C-h l") #'shell) ;; replace default view-lossage

;; Copy region (M-w already does this; keep explicit)
(global-set-key (kbd "M-w") #'copy-region-as-kill)

;; Confirm quit
(setq confirm-kill-emacs #'yes-or-no-p)

;;; ------------------------------
;;; Eval control (safety check)
;;; ------------------------------

(defun rc/eval-buffer-confirm ()
  "Eval buffer only if user types yes."
  (interactive)
  (if (string= (read-string "Type 'yes' to eval buffer: ") "yes")
      (progn (eval-buffer) (message "Buffer evaluated!"))
    (message "Evaluation cancelled.")))
(global-set-key (kbd "C-j") #'rc/eval-buffer-confirm)

;;; ------------------------------
;;; Per-file quick jumps
;;; ------------------------------

(global-set-key (kbd "C-c f") (lambda () (interactive) (find-file "~/.emacs")))
(global-set-key (kbd "C-c r") (lambda () (interactive) (find-file "~/.zshrc")))
(global-set-key (kbd "C-c i") (lambda () (interactive) (find-file "~/.config/i3/config")))
(global-set-key (kbd "C-c v") (lambda () (interactive) (find-file "~/dev/")))

;;; ------------------------------
;;; Saves, locks, backups
;;; ------------------------------

(setq auto-save-default nil
      make-backup-files nil
      create-lockfiles nil)

;;; ------------------------------
;;; Extras you referenced (fixed)
;;; ------------------------------

;; Move text up/down (you bound M-p/M-n earlier but the command wasn't installed)
(use-package move-text
  :config
  (move-text-default-bindings) ; sets M-<up>/<down>; keep your preferred bindings too
  (global-set-key (kbd "M-p") #'move-text-up)
  (global-set-key (kbd "M-n") #'move-text-down))

;;; ------------------------------
;;; Final touches
;;; ------------------------------

;; Keep Custom from polluting this file; write to a separate file instead.
(setq custom-file (expand-file-name "~/.emacs-custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;;; ~/.emacs ends here


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(move-text use-package tree-sitter-ispell tree-sitter-indent tree-sitter-ess-r smartparens rc-mode magit lsp-ui lsp-ivy gruber-darker-theme go-mode flycheck counsel company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
