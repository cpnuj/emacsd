(require 'package)
(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")))
(package-initialize)

;; refresh package list if it is not already available
(when (not package-archive-contents) (package-refresh-contents))

;; install use-package if it isn't already installed
(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(setq scroll-step 1)
(setq scroll-conservatively  10000)
(blink-cursor-mode 0)
(column-number-mode 1)

(setq auto-save-default nil)
(setq make-backup-files nil)

(setq max-lisp-eval-depth 10000)
(setq max-specpdl-size 10000)

;; save recent opened file
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; evil mode
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-fu))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package helm
  :ensure t
  :bind
  ("M-x" . helm-M-x)
  ("C-x f" . helm-find-files)
  ("C-x b" . helm-buffers-list)
  ("C-x r" . helm-recentf)
  ("C-x s" . helm-do-ag-project-root)
  :config
  (progn
    (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
    (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-z") #'helm-select-action)))

;; git
(use-package magit
  :ensure t)
  
;; GO

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :bind (("M-<right>" . lsp-find-definition))
  :config (progn
	    (setq lsp-enable-file-watchers nil)
	    (setq lsp-prefer-flymake nil)))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config (progn
            ;; disable inline documentation
            (setq lsp-ui-sideline-enable nil)
            ;; disable showing docs on hover at the top of the window
            (setq lsp-ui-doc-enable nil)))

(use-package company
  :ensure t
  :config (progn
            ;; don't add any dely before trying to complete thing being typed
            ;; the call/response to gopls is asynchronous so this should have little
            ;; to no affect on edit latency
            (setq company-idle-delay 0)
            ;; start completing after a single character instead of 3
            (setq company-minimum-prefix-length 1)
            ;; align fields in completions
            (setq company-tooltip-align-annotations t)))

(use-package flycheck
  :ensure t)

(defun go-mode-setting ()
  (setq tab-width 4)
  (setq indent-tabs-mode 1))

(use-package go-mode
  :ensure t
  :bind (("C-x d" . dap-mode))
  :config
  :hook ((go-mode . lsp-deferred)
	 (go-mode . show-paren-mode)
	 (go-mode . go-mode-setting)
	 (before-save . lsp-format-buffer)
	 (before-save . lsp-organize-imports)))

(use-package dap-mode
  :defer t
  :ensure t
  :bind (("C-x h" . dap-hydra))
  :config
  (require 'dap-go)
  (require 'dap-hydra))
