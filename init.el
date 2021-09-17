(require 'package)
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

(menu-bar-mode -1)
(tool-bar-mode -1)
;; (fringe-mode 1)
(toggle-scroll-bar -1)
;; (global-hl-line-mode 1)
(setq scroll-step 1)
(setq scroll-conservatively  10000)
(blink-cursor-mode 0)

(setq auto-save-default nil)
(setq make-backup-files nil)

(setq max-lisp-eval-depth 10000)
(setq max-specpdl-size 10000)

(add-to-list 'default-frame-alist '(background-color . "black"))
(global-hl-line-mode 1)
(set-face-attribute hl-line-face nil :underline t)
(set-face-background 'hl-line "black")

;; save recent opened file
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key (kbd "C-x r") 'helm-recentf)

;; cl - Common Lisp Extension
(require 'cl)

;; Add Packages
(defvar my/packages '(
  ;; Vim emu
  evil
  ;; Helm
  helm
  helm-ag
  ;; For code
  company
  lsp-mode
  go-mode
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

;; Open some modes
(evil-mode 1)

;; helm configuration
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(add-hook 'helm-after-initialize-hook
          (lambda()
            (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
            (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
            (define-key helm-map (kbd "C-z") #'helm-select-action)
            ;; (define-key helm-M-x-map (kbd "ESC") 'helm-keyboard-quit)
            ;; (define-key helm-buffer-map (kbd "ESC") 'helm-keyboard-quit)
            ;; (define-key helm-find-files-map (kbd "ESC") 'helm-keyboard-quit)
            ;; (define-key helm-map (kbd "ESC") 'helm-keyboard-quit)
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
            (setq indent-tabs-mode 1)))

;; lsp mode color setting

(defun lsp-headerline-face-conf ()
  (set-face-foreground 'lsp-headerline-breadcrumb-deprecated-face "black")
  (set-face-foreground 'lsp-headerline-breadcrumb-path-error-face "black")
  (set-face-foreground 'lsp-headerline-breadcrumb-path-face "black")
  (set-face-foreground 'lsp-headerline-breadcrumb-path-hint-face "black")
  (set-face-foreground 'lsp-headerline-breadcrumb-path-info-face "black")
  (set-face-foreground 'lsp-headerline-breadcrumb-path-warning-face "black")
  (set-face-foreground 'lsp-headerline-breadcrumb-project-prefix-face "black")
  (set-face-foreground 'lsp-headerline-breadcrumb-separator-face "black")
  (set-face-foreground 'lsp-headerline-breadcrumb-symbols-error-face "black")
  (set-face-foreground 'lsp-headerline-breadcrumb-symbols-face "black")
  (set-face-foreground 'lsp-headerline-breadcrumb-symbols-hint-face "black")
  (set-face-foreground 'lsp-headerline-breadcrumb-symbols-info-face "black")
  (set-face-foreground 'lsp-headerline-breadcrumb-symbols-warning-face "black")
  (set-face-foreground 'lsp-headerline-breadcrumb-unknown-project-prefix-face "black")
  )

(add-hook 'lsp-headerline-breadcrumb-mode-hook #'lsp-headerline-face-conf)
