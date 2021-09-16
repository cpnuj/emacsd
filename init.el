(require 'package)
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

(menu-bar-mode -1)
(tool-bar-mode -1)
(fringe-mode -1)
(toggle-scroll-bar -1)
;; (global-hl-line-mode 1)
(setq scroll-step 1)
(setq scroll-conservatively  10000)

(setq auto-save-default nil)
(setq make-backup-files nil)

;; cl - Common Lisp Extension
(require 'cl)

;; Add Packages
(defvar my/packages '(
  evil
  helm
  helm-ag
  eglot
  go-mode
  ) "Default packages")

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

;; Find Executable Path on OS X
;; when (memq window-system '(mac ns))
;;  (exec-path-from-shell-initialize)

;; Open some modes
(evil-mode 1)

;; helm configuration
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-p") 'helm-find-files)
(add-hook 'helm-after-initialize-hook
          (lambda()
            (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
            (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
            (define-key helm-map (kbd "C-z") #'helm-select-action)
            (define-key helm-buffer-map (kbd "ESC") 'helm-keyboard-quit)
            (define-key helm-M-x-map (kbd "ESC") 'helm-keyboard-quit)
            (define-key helm-find-files-map (kbd "ESC") 'helm-keyboard-quit)
            (define-key helm-map (kbd "ESC") 'helm-keyboard-quit)))

;; xref configuration
(global-set-key (kbd "M-<right>") 'xref-find-definitions)

;; Load go-mode and Eglot
(require 'project)

(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'project-find-functions #'project-find-go-module)

(require 'go-mode)
(require 'eglot)
(add-hook 'go-mode-hook 'eglot-ensure)

;; configuring gopls
(setq-default eglot-workspace-configuration
    '((:gopls .
        ((staticcheck . t)
         (matcher . "CaseSensitive")))))

(setq eglot-ignored-server-capabilites '(:documentHighlightProvider))

(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 4)
            (setq indent-tabs-mode 1)))
