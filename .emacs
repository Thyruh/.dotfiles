;;; ~/.emacs --- Cleaned-up Emacs config
;;; Commentary:
;;; Core setup with minimal customizations and mostly default keybindings.
;;; Code:

;; Load custom file
(load "~/.emacs-custom" 'noerror 'nomessage)

;; Package initialization
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Install use-package if not installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; --------------------
;; Programming modes
;; --------------------
(use-package go-mode :ensure t :hook ((go-mode . lsp-deferred)
                                     (before-save . lsp-format-buffer)
                                     (before-save . lsp-organize-imports)))
(use-package lsp-mode :ensure t :commands lsp)
(use-package lsp-ui :ensure t :commands lsp-ui-mode)
(setq lsp-format-buffer nil)

(use-package company :ensure t :config (add-hook 'after-init-hook 'global-company-mode))
(use-package flycheck :ensure t :init (global-flycheck-mode))
(add-hook 'haskell-mode-hook 'flycheck-mode)

(use-package smartparens :ensure t :config
  (require 'smartparens-config)
  (smartparens-global-mode t))



;; --------------------
;; Interface settings
;; --------------------

(when (> (length (window-list)) 1)
  (delete-other-windows))
(setq initial-buffer-choice t)     ;; Open a blank buffer immediately
(setq inhibit-startup-screen t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(setq-default tab-width 3)
(setq-default indent-tabs-mode nil)
(setq show-paren-delay 0)
(show-paren-mode 1)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)
(blink-cursor-mode 1)
(global-hl-line-mode 1)

;; Make compilation window appear horizontally at the bottom
(setq display-buffer-alist
      '(("\\*compilation\\*"
         (display-buffer-reuse-window display-buffer-at-bottom)
         (window-height . 15))))


(use-package ivy :ensure t :config (ivy-mode 1))
(use-package counsel :ensure t :config (counsel-mode 1))
(global-set-key (kbd "C-,") 'counsel-switch-buffer)

(setq use-dialog-box nil)

;; --------------------
;; Keybindings
;; --------------------
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Compile current file into `bin/`
(defun compile-file-to-bin ()
  "Compile the current file into a bin/ directory next to the file."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (file-dir (file-name-directory file-name))
         (bin-dir (concat (file-name-as-directory file-dir) "bin/"))
         (output-file (concat bin-dir (file-name-sans-extension (file-name-nondirectory file-name)))))
    (unless (file-directory-p bin-dir)
      (make-directory bin-dir))
    (cond
     ((string-match "\\.cpp\\'" file-name)
      (compile (format "g++ -o %s %s" output-file file-name)))
     ((string-match "\\.go\\'" file-name)
      (compile (format "go build -o %s %s" output-file file-name)))
     ((string-match "\\.hs\\'" file-name)
      (compile (format "ghc --make %s -o %s" file-name output-file)))
     (t (error "Unsupported file type")))))
(global-set-key (kbd "C-c c") 'compile-file-to-bin)

;; Evaluate buffer with yes confirmation
(defun eval-buffer-confirm ()
  "Evaluate buffer if user types 'yes'."
  (interactive)
  (let ((resp (read-string "Type 'yes' to evaluate buffer: ")))
    (if (string= resp "yes")
        (progn
          (eval-buffer)
          (message "Buffer evaluated!"))
      (message "Evaluation cancelled."))))
(global-set-key (kbd "C-j") 'eval-buffer-confirm)

;; Jump to config files or programming dir
(global-set-key (kbd "C-c f") (lambda () (interactive) (find-file "~/.emacs")))
(global-set-key (kbd "C-c r") (lambda () (interactive) (find-file "~/.zshrc")))
(global-set-key (kbd "C-c i") (lambda () (interactive) (find-file "~/.config/i3/config")))
(global-set-key (kbd "C-c v") (lambda () (interactive) (find-file "~/personal/Architect/")))

;; Move text
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

;; Duplicate line
(defun duplicate-current-line ()
  "Duplicate the current line."
  (interactive)
  (let ((col (- (point) (line-beginning-position))))
    (save-excursion
      (end-of-line)
      (newline)
      (yank))
    (forward-char col)))
(global-set-key (kbd "C-c d") 'duplicate-current-line)

;; Select region
(defun rc/select-word ()
  "Select the current word."
  (interactive)
  (skip-syntax-backward "w_")
  (set-mark (point))
  (skip-syntax-forward "w_"))

(defun rc/select-sentence ()
  "Select the current sentence."
  (interactive)
  (forward-sentence 1)
  (set-mark (point)))

(defun rc/select-current-line ()
  "Select the whole line."
  (interactive)
  (move-beginning-of-line 1)
  (set-mark (point))
  (move-end-of-line 1))

(global-set-key (kbd "C-c w") 'rc/select-word)
(global-set-key (kbd "C-c s") 'rc/select-sentence)
(global-set-key (kbd "C-c l") 'rc/select-current-line)

;; --------------------
;; Misc
;; --------------------
(set-face-attribute 'default nil :height 180)

(use-package gruber-darker-theme :ensure t :config (load-theme 'gruber-darker t))

(set-frame-parameter (selected-frame) 'alpha '(85 . 85))
(add-to-list 'default-frame-alist '(alpha . (85 . 85)))
(setq-default cursor-type 'bar)
(setq-default cursor-in-non-selected-windows 'box)

(global-set-key (kbd "C-h l") 'shell)
(global-set-key (kbd "M-w") 'copy-region-as-kill)
(setq confirm-kill-emacs 'yes-or-no-p)

;;; .emacs ends here


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(warning-suppress-types '((comp) (comp) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
