;;; init.el --- My Config

;;; Commentary:

;;; Code:
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(defun add-to-load-path (dir) (add-to-list 'load-path dir))

(defun disable-ui ()
  (progn
    (menu-bar-mode -1)
    (toggle-scroll-bar -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (tooltip-mode -1)
    (setq initial-scratch-message nil)
    (kill-buffer "*scratch*")))

(defun configure-startup ()
  (progn
    (setq inhibit-startup-screen t)
    (setq inhibit-startup-echo-area-message t)))

(defun setup-flat-ui ()
  (custom-theme-set-faces
   'flatui
   '(company-preview ((t (:foreground "#7f8c8d" :background "#ecf0f1"))))
   '(company-preview-common ((t (:foreground "#7f8c8d" :background "#ecf0f1"))))
   '(company-tooltip ((t (:foreground "#2c3e50" :background "#dfe4ea"))))
   '(company-tooltip-common ((t (:foreground "#2c3e50" :background "#dfe4ea"))))
   '(company-tooltip-selection ((t (:background "#f1c40f"))))
   '(company-tooltip-common-selection ((t (:foreground "#2c3e50" :background "#f1c40f"))))
   '(company-scrollbar-bg ((t (:background "#95a5a6"))))
   '(company-scrollbar-fg ((t (:background "#1abc9c"))))
   '(iedit-occurrence ((t (:foreground "#2c3e50" :background "#1abc9c"))))
   ))

(defun setup-font ()
  (set-frame-font "InputCustomSansCompressed-14" nil t))

(defun setup-modeline-font ()
  (set-face-attribute 'mode-line-inactive nil :family "InputCustomMonoCompressed")
  (set-face-attribute 'mode-line nil :family "InputCustomMonoCompressed"))

(defun enable-extra-functionality ()
  (progn
    (put 'narrow-to-region 'disabled nil)
    (put 'upcase-region 'disabled nil)
    (put 'downcase-region 'disabled nil)
    (put 'dired-find-alternate-file 'disabled nil)))

(defun disable-bell ()
  (setq ring-bell-function 'ignore
	visible-bell nil))

(defconst auto-save-path (expand-file-name "~/.emacs.d/auto-save/"))

(defun setup-builtins ()
  (progn
    (prefer-coding-system 'utf-8)
    (setq-default fill-column 80)
    (setq save-interprogram-paste-before-kill t)
    (setq-default sentence-end-double-space nil)
    (setq column-number-mode t)
    (blink-cursor-mode 0)
    (fset 'yes-or-no-p 'y-or-n-p)
    (setq make-backup-files nil)
    (setq use-dialog-box nil)
    (unless (file-exists-p auto-save-path)
      (make-directory auto-save-path t))
    (setq auto-save-file-name-transforms
	  `((".*" ,auto-save-path t)))
    (setq minibuffer-prompt-properties
	  '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))))

(defun init ()
  (disable-ui)
  (setup-builtins)
  (setup-font)
  (configure-startup)
  (enable-extra-functionality)
  (disable-bell)
  (add-hook 'after-init-hook 'setup-modeline-font t nil))

(setq gc-cons-threshold 100000000)

(init)

(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

(use-package diminish
  :ensure t)

(use-package electric
  :config
  (progn
    (electric-indent-mode)
    (electric-pair-mode)))

(use-package paren
  :config
  (show-paren-mode))

(use-package cua-base
  :config
  (cua-mode))

(use-package delsel
  :config
  (delete-selection-mode))

(use-package ido
  :config
  (progn
    (ido-mode)
    (ido-everywhere)))

(use-package autorevert
  :diminish auto-revert-mode)

(use-package eldoc
  :diminish eldoc-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))

(use-package mwheel
  :config
  (progn
    (setq mouse-wheel-progressive-speed nil)
    (setq mouse-wheel-scroll-amount '(2 ((shift) . 1) ((control) . nil)))))

(use-package paradox
  :ensure t)

(use-package anzu
  :ensure t
  :diminish anzu-mode
  :config
  (progn
    (setq anzu-cons-mode-line-p nil)
    (global-anzu-mode)))

(use-package spaceline
  :ensure t
  :init
  (setq ns-use-srgb-colorspace nil)
  :config
  (progn
    (require 'spaceline-config)
    (spaceline-spacemacs-theme)
    (setq powerline-default-separator "wave")
    (setq spaceline-workspace-numbers-unicode t)
    (setq spaceline-window-numbers-unicode t)
    (setq powerline-height 25)
    (setq powerline-text-scale-factor 1.0)
    (spaceline-compile)))

(use-package flatui-theme
  :disabled t
  :ensure t
  :config
  (setup-flat-ui))

(use-package dracula-theme
  :ensure t)

(use-package spacemacs-theme
  :ensure t
  :defer t
  :config
  (load-theme 'spacemacs-dark))

(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :defer 2
  :config
  (setq git-gutter:window-width 2)
  (global-git-gutter-mode))

(use-package projectile
  :ensure t
  :defer 2)

(use-package exec-path-from-shell
  :ensure t
  :config
  (progn
    (exec-path-from-shell-copy-env "GOPATH")
    (exec-path-from-shell-copy-env "PATH")))

(use-package yasnippet
  :ensure t
  :defer t
  :diminish yas-minor-mode)

(use-package flycheck
  :ensure t
  :diminish (flycheck-mode . " ⓢ")
  :config
  (global-flycheck-mode))

(use-package company
  :ensure t
  :diminish (company-mode . " ⓐ")
  :config
  (global-company-mode))

(use-package go-mode
  :ensure t
  :defer t
  :config
  (progn
    (setq gofmt-command "goimports")
    (add-hook 'before-save-hook #'gofmt-before-save)))

(use-package company-go
  :ensure t
  :config
  (eval-after-load 'company '(add-to-list 'company-backends 'company-go)))

(use-package protobuf-mode
  :ensure t)

(use-package flycheck-protobuf
  :ensure t)

(use-package anaconda-mode
  :ensure t
  :defer t
  :config
  (progn
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode)))

(use-package company-anaconda
  :ensure t
  :config
  (eval-after-load "company" '(add-to-list 'company-backends 'company-anaconda)))

(use-package yaml-mode
  :ensure t)

(use-package ensime
  :ensure t
  :init
  (setq ensime-startup-snapshot-notification nil))

(use-package sbt-mode
  :ensure t)

(use-package irony
  :ensure t
  :init
  (progn
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'objc-mode-hook 'irony-mode))
  :config
  (progn
    (setq irony-additional-clang-options (quote ("-std=c++11" "-stdlib=libc++")))))

(use-package company-irony
  :ensure t
  :config
  (progn
    (eval-after-load 'company '(add-to-list 'company-backends 'company-irony))
    (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)))

(use-package company-c-headers
  :ensure t
  :config
  (add-to-list 'company-backends 'company-c-headers))

(use-package clang-format
  :ensure t
  :defer t)

(use-package google-c-style
  :ensure t
  :defer t
  :config
  (progn
    (add-hook 'c-mode-common-hook 'google-set-c-style)
    (add-hook 'c-mode-common-hook 'google-make-newline-indent)))

(use-package js
  :config
  (setq js-indent-level 2))

(use-package js2-mode
  :ensure t
  :mode (("\\.js\\'" . js2-mode) ("\\.jsx?\\'" . js2-jsx-mode))
  :config
  (add-hook 'js2-jsx-mode-hook (setq-local sgml-basic-offset js2-basic-offset)))

(use-package json-mode
  :ensure t)

(use-package eslint-fix
  :ensure t
  :config
  (eval-after-load 'js2-mode
    '(add-hook 'js2-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix)))))

(use-package web-mode
  :ensure t
  :mode ("\\.html?\\'" . web-mode))

(use-package markdown-mode
  :ensure t)

(use-package terraform-mode
  :ensure t
  :config
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))

(use-package hungry-delete
  :ensure t
  :diminish hungry-delete-mode
  :config
  (global-hungry-delete-mode))

(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package iedit
  :ensure t
  :bind ("C-;" . iedit-mode))

(use-package multiple-cursors
  :ensure t)

(use-package avy
  :ensure t
  :bind (("C-:" . avy-move-region)
	 ("C-'" . avy-goto-char-timer))
  :config (avy-setup-default))

(use-package ace-window
  :ensure t
  :bind ("M-p" . ace-window))

(use-package which-key
  :ensure t
  :diminish (which-key-mode . " Ⓚ")
  :config
  (progn
    (which-key-mode)))

(use-package guru-mode
  :ensure t
  :diminish guru-mode
  :config
  (progn
    (add-hook 'prog-mode-hook 'guru-mode)
    (setq guru-warn-only t)))

(use-package whitespace
  :ensure t
  :diminish (global-whitespace-mode . " Ⓦ")
  :diminish (whitespace-mode . " ⓦ")
  :config
  (progn
    (setq whitespace-line-column 80)
    (add-hook 'before-save-hook 'whitespace-cleanup)))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode))

(use-package flx-ido
  :ensure t
  :config
  (progn
    (flx-ido-mode)
    (setq ido-enable-flex-matching t)
    (setq ido-use-faces nil)))

(use-package restclient
  :ensure t)

(use-package magit
  :ensure t)

(use-package window-numbering
  :ensure t
  :config
  (progn
    (defun window-numbering-install-mode-line (&optional position) "Do nothing.")
    (window-numbering-mode)))

(use-package golden-ratio
  :ensure t
  :diminish (golden-ratio-mode . " ⓖ")
  :config
  (progn
    (golden-ratio-mode)
    (setq window-combination-resize t)
    (setq golden-ratio-auto-scale t)
    (setq golden-ratio-extra-commands
	  (append golden-ratio-extra-commands
		  '(ace-window
		    ace-delete-window
		    ace-select-window
		    ace-swap-window
		    ace-maximize-window
		    avy-pop-mark
		    avy-goto-char-timer)))))

(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode))

(use-package selectric-mode
  :ensure t
  :diminish (selectric-mode . "♬")
  :config
  (selectric-mode))

(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :config
  (rainbow-mode))

;; just use show-paren-mode
(use-package highlight-parentheses
  :disabled t
  :ensure t
  :diminish highlight-parentheses-mode
  :init
  (add-hook 'prog-mode-hook #'highlight-parentheses-mode))

(use-package highlight-symbol
  :disabled t
  :ensure t
  :config
  (highlight-symbol-mode))

(use-package highlight-indentation
  :disabled t
  :ensure t
  :config
  (highlight-indentation-mode))

(use-package indent-guide
  :disabled t
  :ensure t
  :diminish indent-guide-mode
  :config
  (indent-guide-global-mode))

(use-package centered-cursor-mode
  :disabled t
  :ensure t
  :diminish (centered-cursor-mode . " ⊝")
  :config
  (progn
    (global-centered-cursor-mode)
    (setq ccm-recenter-at-end-of-file t
	  ccm-ignored-commands '(mouse-drag-region
				 mouse-set-point
				 widget-button-click
				 scroll-bar-toolkit-scroll))))

(use-package diff-hl
  :disabled t
  :ensure t
  :config
  (global-diff-hl-mode))

(use-package fill-column-indicator
  :disabled t
  :ensure t
  :defer 2
  :config
  (fci-mode))

;;; init.el ends here