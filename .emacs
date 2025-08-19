;;; ~/.emacs --- Clean, opinionated Emacs config
;;; Author: Thyruh
;;; Commentary:
;;; Structured into clear sections.  Safe to paste as your full ~/.emacs.

;;; ------------------------------
;;; Bootstrap packages
;;; ------------------------------

;;; Code:

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                        ("gnu"   . "https://elpa.gnu.org/packages/"))
package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;;; ------------------------------
;;; UI / UX
;;; ------------------------------

(set-face-attribute 'default nil :font "JetBrains Mono ExtraBold-18")

(load-theme 'gruber-darker t)

;;; ------------------------------

;;; Whitespace handling
;;; ------------------------------

(require 'whitespace)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Highlight trailing spaces, tabs, and spaces; remove 'lines-tail' to hide dollar signs
(setq whitespace-style '(face tabs spaces trailing))
(global-whitespace-mode 1)

(defun rc/set-up-whitespace-handling ()
  "Enable basic whitespace handling without trailing $ markers."
  (whitespace-mode 1)
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))
(add-hook 'prog-mode-hook #'rc/set-up-whitespace-handling)
(add-hook 'text-mode-hook #'rc/set-up-whitespace-handling)

(setq inhibit-startup-screen t
      initial-scratch-message nil
      initial-buffer-choice t
      use-dialog-box nil)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq-default cursor-type 'box
              cursor-in-non-selected-windows 'box)
(blink-cursor-mode 1)
(setq blink-cursor-interval 0.5)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)
(show-paren-mode 1)
(setq show-paren-delay 0)
(global-hl-line-mode 1)

(setq-default tab-width 4
              indent-tabs-mode nil)

;;; ------------------------------
;;; Core editing helpers
;;; ------------------------------


(setq-default scroll-margin 5
              scroll-conservatively 9999
              scroll-step 1)

(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1))

(toggle-word-wrap 1)

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(defun rc/select-word ()
  "Select word."
  (interactive)
  (skip-syntax-backward "w_")
  (set-mark (point))
  (skip-syntax-forward "w_"))

(defun rc/select-sentence ()
  "Select sentence."
  (interactive)
  (backward-sentence 1)
  (set-mark (point))
  (forward-sentence 1))

(defun scratch-compile ()
  "Compile and run the *scratch* buffer as C++ without leaving files."
  (interactive)
  (if (not (eq (current-buffer) (get-buffer "*scratch*")))
      (message "scratch-compile only works in *scratch* buffer")
    (let* ((tmp (make-temp-file "emacs-cpp-" nil ".cpp"))
           (bin (concat tmp ".out")))
      (write-region (point-min) (point-max) tmp nil 'silent)
      (compile
       (format "g++ -std=c++23 -O2 -Wall %s -o %s && %s && rm -f %s %s"
               tmp bin bin bin tmp)))))

;; Optional: bind in *scratch* only
(with-current-buffer "*scratch*"
  (c++-mode)
  (local-set-key (kbd "C-c C-c") #'scratch-compile))

(defun rc/select-current-line ()
  "Select current line."
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

(use-package lsp-mode
  :init
  (setq lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil)
  :hook ((go-mode c-mode c++-mode python-mode rust-mode haskell-mode) . lsp-deferred)
  :commands (lsp lsp-deferred))

(use-package lsp-ui :after lsp-mode :commands lsp-ui-mode)
(use-package lsp-ivy :after (lsp-mode ivy) :commands lsp-ivy-workspace-symbol)

;;; ------------------------------
;;; Tree-sitter
;;; ------------------------------

(use-package tree-sitter :defer t)
(use-package tree-sitter-langs :after tree-sitter :defer t)
(when (require 'tree-sitter nil 'noerror)
  (global-tree-sitter-mode 1)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;;; ------------------------------
;;; Dired
;;; ------------------------------

(require 'dired-x)

;; Corrected: use setq instead of defvar with multiple variables
(setq dired-listing-switches "-alFhG --group-directories-first"
      dired-dwim-target t
      dired-mouse-drag-files t)

(add-hook 'dired-mode-hook #'dired-omit-mode)
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))



;;; ------------------------------
;;; Compilation workflow
;;; ------------------------------

(setq display-buffer-alist
      '(("\\*compilation\\*"
         (display-buffer-reuse-window display-buffer-at-bottom)
         (window-height . 15))))

(global-set-key (kbd "C-c C-c") 'compile)
(setq compile-command ""
      compilation-read-command t)

(require 'ansi-color)
(defun rc/colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point))))
(add-hook 'compilation-filter-hook #'rc/colorize-compilation-buffer)

(defun rc/compile-file-to-bin ()
  "Automatically compiles .cpp/.go/.hs files to 'bin/' directory."
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
      (compile (format "g++ -O2 -std=c++20 -o %s %s"
                       (shell-quote-argument out)
                       (shell-quote-argument file))))
     ((string-match-p "\\.go$" file)
      (compile (format "go build -o %s %s"
                       (shell-quote-argument out)
                       (shell-quote-argument file))))
     ((string-match-p "\\.hs$" file)
      (compile (format "ghc --make %s -o %s"
                       (shell-quote-argument file)
                       (shell-quote-argument out))))
     (t (user-error "Unsupported file type: %s" file)))))

(global-set-key (kbd "C-c c") #'rc/compile-file-to-bin)

;;; ------------------------------
;;; Magit
;;; ------------------------------

(use-package magit
  :commands (magit-status magit-log-all)
  :config
  (when (fboundp 'magit-auto-revert-mode)
    (magit-auto-revert-mode -1)))

;;; ------------------------------
;;; Quality-of-life keys
;;; ------------------------------

(global-set-key (kbd "M-d") 'backward-delete-char)

(global-set-key (kbd "C-x C-r") 'query-replace)
(global-set-key (kbd "C-x C-w") 'other-window)
(global-set-key (kbd "<escape>") #'keyboard-escape-quit)
(global-set-key (kbd "C-,") #'counsel-switch-buffer)

(global-set-key (kbd "C-c w") #'rc/select-word)
(global-set-key (kbd "C-c s") #'rc/select-sentence)
(global-set-key (kbd "C-c l") #'rc/select-current-line)

(global-set-key (kbd "C-c g d") #'lsp-find-definition)
(global-set-key (kbd "C-c g i") #'lsp-ivy-workspace-symbol)

(global-set-key (kbd "C-h l") #'shell)
(global-set-key (kbd "M-w") #'copy-region-as-kill)
(setq confirm-kill-emacs #'yes-or-no-p)

;;; ------------------------------
;;; Eval control
;;; ------------------------------

(defun rc/eval-buffer ()
  "Evaluate current Emacs Lisp buffer safely."
  (interactive)
  (if (eq major-mode 'emacs-lisp-mode)
      (progn
        (funcall (symbol-function 'eval-buffer))
        (message "Buffer evaluated!"))
    (message "Not an Emacs Lisp buffer.")))

(global-set-key (kbd "C-j") #'rc/eval-buffer)

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
;;; Extras
;;; ------------------------------

(use-package move-text
  :config
  (move-text-default-bindings)
  (global-set-key (kbd "M-p") #'move-text-up)
  (global-set-key (kbd "M-n") #'move-text-down))

;;; ------------------------------
;;; Final touches
;;; ------------------------------

(setq custom-file (expand-file-name "~/.emacs-custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;;; .emacs ends here
