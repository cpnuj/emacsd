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

(use-package helm-ag
  :ensure t)

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
(use-package vdiff-magit
  :ensure t
  :config
  (progn
    (setq vdiff-disable-folding 1)
    (setq vdiff-auto-refine 1)
    (bind-key "C-x h" 'vdiff-hydra/body vdiff-mode-map)
    ))

(use-package magit
  :ensure t
  :config
  (progn
    (define-key magit-mode-map "e" 'vdiff-magit-dwim)
    (define-key magit-mode-map "E" 'vdiff-magit)
    (transient-suffix-put 'magit-dispatch "e" :description "vdiff (dwim)")
    (transient-suffix-put 'magit-dispatch "e" :command 'vdiff-magit-dwim)
    (transient-suffix-put 'magit-dispatch "E" :description "vdiff")
    (transient-suffix-put 'magit-dispatch "E" :command 'vdiff-magit)
    ;; This flag will default to using ediff for merges.
    ;; (setq vdiff-magit-use-ediff-for-merges nil)

    ;; Whether vdiff-magit-dwim runs show variants on hunks.  If non-nil,
    ;; vdiff-magit-show-staged or vdiff-magit-show-unstaged are called based on what
    ;; section the hunk is in.  Otherwise, vdiff-magit-dwim runs vdiff-magit-stage
    ;; when point is on an uncommitted hunk.
    ;; (setq vdiff-magit-dwim-show-on-hunks nil)

    ;; Whether vdiff-magit-show-stash shows the state of the index.
    ;; (setq vdiff-magit-show-stash-with-index t)

    ;; Only use two buffers (working file and index) for vdiff-magit-stage
    (setq vdiff-magit-stage-is-2way 1)
    )
)
  
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
  ;; :bind (("C-x h" . dap-hydra))
  :config (bind-key "C-x h" 'dap-hydra dap-mode-map)
  (require 'dap-go)
  (require 'dap-hydra))

(load-theme 'doom-dark+ t)
