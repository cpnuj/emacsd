(require 'package)
(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")))
(package-initialize)

(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(setq scroll-step 1)
(setq scroll-conservatively  10000)
(blink-cursor-mode 0)

(setq auto-save-default nil)
(setq make-backup-files nil)

(setq max-lisp-eval-depth 10000)
(setq max-specpdl-size 10000)

;; (add-to-list 'default-frame-alist '(background-color . "black"))
;; (set-face-attribute hl-line-face nil :underline t)
;; (set-face-background 'hl-line "black")

;; save recent opened file
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key (kbd "C-x r") 'helm-recentf)

;; cl - Common Lisp Extension
(require 'cl)

;; Add Packages
(defvar my/packages
  '(;; Package control
    use-package
    ;; colorful
    doom-themes
    ;; Vim emu
    evil
    undo-fu
    ;; Helm
    helm
    helm-ag
    ;; For code
    company
    flycheck
    lsp-mode
    go-mode
    haskell-mode
    exec-path-from-shell
    ))

(setq package-selected-packages my/packages)

(defun my/packages-installed-p ()
  (loop for pkg in my/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (my/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg my/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(require 'exec-path-from-shell)
;; Find Executable Path on OS X and Linux
(when (memq window-system '(mac ns x))
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

;; helm configuration
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(add-hook 'helm-after-initialize-hook
          (lambda()
            (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
            (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
            (define-key helm-map (kbd "C-z") #'helm-select-action)
            ))

;; Golang
(require 'go-mode)
(require 'lsp-mode)

(add-hook 'go-mode-hook #'lsp-deferred)

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(add-hook 'go-mode-hook
          (lambda ()
            (setq tab-width 4)
	    (setq indent-tabs-mode 1)
	    (show-paren-mode)
            ))

;; C mode
(add-hook 'c-mode-hook #'lsp-deferred)

;; lsp keybinding
(add-hook 'lsp-mode-hook (lambda ()
			   (local-set-key (kbd "M-<right>") #'lsp-find-definition)
			   ))

;; lsp configure
(add-hook 'lsp-mode-hook (lambda ()
			   (setq lsp-enable-file-watchers nil)
			   ))

;; git
(use-package magit
  :ensure t)

(load-theme 'doom-one-light t)
