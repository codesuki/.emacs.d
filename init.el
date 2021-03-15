;;; init.el --- My Config -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
;; from spacemacs
;; less garbage collection during init

;; enable debugging
;(setq debug-on-error t)

;; run command without deleting output from previous run
;; (defun shell-command-print-separator ()
;;   (overlay-put (make-overlay (point-max) (point-max))
;;                'before-string
;;                (propertize "!" 'display
;;                            (list 'left-fringe
;;                                  'right-triangle))))

;; (advice-add 'shell-command--save-pos-or-erase :after 'shell-command-print-separator)

(setq gc-cons-threshold 402653184 gc-cons-percentage 0.6)
(setq read-process-output-max (* 1024 1024))
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 100000000
                  gc-cons-percentage 0.1)))

(setq straight-use-package-by-default t)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; (require 'package)
;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;; ;;(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
;; (package-initialize)

;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))
;; (eval-when-compile (require 'use-package))

(defun add-to-load-path (dir) (add-to-list 'load-path dir))

(defun disable-ui ()
  (set-scroll-bar-mode nil)
  (setq menu-bar-mode nil)
  (tool-bar-mode 0)
  (setq tooltip-mode nil)
  (setq initial-scratch-message nil))

(defun configure-startup ()
  (progn
    (setq inhibit-startup-screen t)
    (setq inhibit-startup-echo-area-message t)))

(defun disable-osx-keys ()
  "Disable keys defined in `ns-win.el'."
  (global-unset-key  [?\s-,])
  (global-unset-key  [?\s-'])
  (global-unset-key  [?\s-`])
  (global-unset-key  [?\s-~])
  (global-unset-key  [?\s--])
  (global-unset-key  [?\s-:])
  (global-unset-key  [?\s-?])
  (global-unset-key  [?\s-^])
  (global-unset-key  [?\s-&])
  (global-unset-key  [?\s-C])
  (global-unset-key  [?\s-D])
  (global-unset-key  [?\s-E])
  (global-unset-key  [?\s-L])
  (global-unset-key  [?\s-M])
  (global-unset-key  [?\s-S])
  (global-unset-key  [?\s-a])
  (global-unset-key  [?\s-c])
  (global-unset-key  [?\s-d])
  (global-unset-key  [?\s-e])
  (global-unset-key  [?\s-f])
  (global-unset-key  [?\s-g])
  (global-unset-key  [?\s-h])
  (global-unset-key  [?\s-H])
  (global-unset-key  [?\M-\s-h])
  (global-unset-key  [?\s-j])
  (global-unset-key  [?\s-k])
  (global-unset-key  [?\s-l])
  (global-unset-key  [?\s-m])
  (global-unset-key  [?\s-o])
  (global-unset-key  [?\s-q])
  (global-unset-key  [?\s-s])
  (global-unset-key  [?\s-t])
  (global-unset-key  [?\s-u])
  (global-unset-key  [?\s-v])
  (global-unset-key  [?\s-w])
  (global-unset-key  [?\s-x])
  (global-unset-key  [?\s-y])
  (global-unset-key  [?\s-z])
  (global-unset-key  [?\s-+])
  (global-unset-key  [?\s-=])
  (global-unset-key  [?\s--])
  (global-unset-key  [?\s-0])
  (global-unset-key  [?\s-|])
  (global-unset-key  [s-kp-bar])
  (global-unset-key  [?\C-\s- ]))

(defun setup-font ()
  "Setup fonts and enable `variable-pitch-mode'."
  ;;(set-frame-font "InputMonoCondensed-14" nil t)
  (add-to-list 'default-frame-alist '(font . "InputMonoCondensed-14"))
  (set-face-attribute 'variable-pitch nil :font "InputSansCompressed-14")
  (add-hook 'text-mode-hook #'variable-pitch-mode)
  (add-hook 'prog-mode-hook #'variable-pitch-mode)
  (add-hook 'protobuf-mode-hook #'variable-pitch-mode))

(defun setup-modeline-font ()
  ;; (set-face-attribute 'mode-line-inactive nil :font "InputMonoCondensed-14")
  ;; (set-face-attribute 'mode-line nil :font "InputMonoCondensed-14")
  )

(defun setup-term-font ()
  '(term ((t (:background "#292b2e" :foreground "#b2b2b2" :family "InputCustomMonoCompressed")))))

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
    (setq-default indent-tabs-mode nil)
    (setq-default show-trailing-whitespace 't)
    (setq-default indicate-empty-lines 't)
    (setq save-interprogram-paste-before-kill t)
    (setq-default sentence-end-double-space nil)
    (setq column-number-mode t)
    (blink-cursor-mode 0)
    (fset 'yes-or-no-p 'y-or-n-p)
    (setq make-backup-files nil)
    (setq use-dialog-box nil)
    (setq confirm-kill-processes nil)
    (setq load-prefer-newer t)
    (unless (file-exists-p auto-save-path)
      (make-directory auto-save-path t))
    (setq auto-save-file-name-transforms
          `((".*" ,auto-save-path t)))
    (setq minibuffer-prompt-properties
          '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))))

;; from https://github.com/purcell/emacs.d/blob/master/lisp/init-windows.el
(defun split-window-func-with-other-buffer (split-function)
  (lambda (&optional arg)
    "Split this window and switch to the new window unless ARG is provided."
    (interactive "P")
    (funcall split-function)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (other-buffer))
      (unless arg
        (select-window target-window)))))

(defun setup-window-splitting ()
      (global-set-key (kbd "C-x 2") (split-window-func-with-other-buffer #'split-window-below))
      (global-set-key (kbd "C-x 3") (split-window-func-with-other-buffer #'split-window-horizontally)))

(defun kill-region-or-backward-kill-word (&optional arg region)
  "`kill-region' if the region is active, otherwise `backward-kill-word'"
  (interactive
   (list (prefix-numeric-value current-prefix-arg) (use-region-p)))
  (if region
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))

(defun setup-kill-backwards-word ()
  (global-set-key (kbd "C-w") 'kill-region-or-backward-kill-word))

(defun move-beginning-of-line-or-indent ()
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))

(defun setup-move-beginning-of-line-or-indent ()
  (global-set-key (kbd "C-a") 'move-beginning-of-line-or-indent))

(defun setup-undo-limits ()
  (setq undo-limit 80000000)
  (setq undo-strong-limit 12000000)
  (setq undo-outer-limit 12000000))

(defun setup-auth ()
  (setq auth-sources
        '(macos-keychain-internet)))

(defun setup-better-capitalize-word ()
  "From https://oremacs.com/2014/12/23/upcase-word-you-silly/."
  (defadvice capitalize-word (before capitalize-word-advice activate)
    (unless (or (looking-back "\\b")
                (bound-and-true-p subword-mode))
      (backward-word)))
  (defadvice upcase-word (before upcase-word-advice activate)
    (unless (looking-back "\\b")
      (backward-word)))
  (defadvice downcase-word (before downcase-word-advice activate)
    (unless (looking-back "\\b")
      (backward-word))))

(defun init ()
  "Init shared settings."
  (setup-builtins)
  (configure-startup)
  (enable-extra-functionality)
  (disable-bell)
  (setup-undo-limits)
  (setup-kill-backwards-word)
  (setup-window-splitting)
  (setup-move-beginning-of-line-or-indent)
  (setup-auth)
  (setup-better-capitalize-word)
  (global-set-key (kbd "M-z") 'zap-up-to-char)
  (global-set-key (kbd "C-?") 'help-command)
  (define-key key-translation-map (kbd "C-h") (kbd "<DEL>")))

(defun init-ui ()
  "Init UI relating settings.
FRAME is received from `after-make-frame-functions'."
  (disable-ui)
  (setup-font)
  (setup-modeline-font)
  (disable-osx-keys))

(defun recompile-all ()
  "Recompile all packages."
  (byte-recompile-directory package-user-dir nil 'force))

(defun my-recentf-cleanup ()
  (setq recentf-list '()))

(init)
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame) (with-selected-frame frame 'init-ui)))
  (init-ui))

(straight-use-package 'use-package)

(use-package el-patch)

(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package diminish)

(use-package electric
  :config
  (progn
    (electric-indent-mode)
    (electric-pair-mode)))

(use-package paren
  :config
  (show-paren-mode)
  (setq show-paren-delay 0))

(use-package cua-base
  :config
  (cua-selection-mode t))

(use-package delsel
  :config
  (delete-selection-mode))

;; (use-package ido
;;   :config
;;   (progn
;;     (ido-mode)
;;     (ido-everywhere)
;;     (setq ido-use-virtual-buffers t)))

(use-package autorevert
  :diminish auto-revert-mode)

(use-package eldoc
  :diminish eldoc-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))

(use-package mwheel
  :straight nil
  :config
  (progn
    (setq mouse-wheel-progressive-speed nil)
    (setq mouse-wheel-scroll-amount '(2 ((shift) . 1) ((control) . nil)))))

(use-package dired-x
  :straight nil)

(use-package ibuffer
  :config
  (progn
    (defalias 'list-buffers 'ibuffer)))

(use-package abbrev
  :straight nil
  :diminish)

(use-package recentf
  :config
  (recentf-mode))

(use-package whitespace
  :diminish (global-whitespace-mode . " Ⓦ")
  :diminish (whitespace-mode . " ⓦ")
  :config
  (progn
    (setq whitespace-line-column 80)
    (add-hook 'before-save-hook 'whitespace-cleanup)))

(use-package gnutls)

(use-package gcmh
  :config
  (gcmh-mode))

(use-package winner
  :config
  (winner-mode))

(use-package imenu-list)

(use-package window-purpose
  :after imenu-list
  :straight (window-purpose :fork (:host github :repo "codesuki/emacs-purpose" :branch "counsel"))
  :config
  (setq purpose-default-action-order 'prefer-same-window)
  ;; (add-to-list 'purpose-user-mode-purposes
  ;;              '((bazel-mode . edit)
  ;;                (terraform-mode . edit))
  ;; (purpose-compile-user-configuration)
  (purpose-mode)
  (require 'window-purpose-x)
  (purpose-x-golden-ratio-setup)
  (purpose-x-popwin-setup)
  (add-to-list 'purpose-special-action-sequences
               '(edit purpose-display-maybe-same-window
                      purpose-display-reuse-window-buffer
                      purpose-display-reuse-window-purpose
                      purpose-display-maybe-other-window
                      purpose-display-maybe-other-frame
                      purpose-display-maybe-pop-up-window
                      purpose-display-maybe-pop-up-frame)))

(use-package paradox
  :defer t
  :config
  (progn
    (paradox-enable)))

(use-package amx
  :config
  (amx-initialize))

(use-package flx)

;; https://github.com/abo-abo/swiper/issues/2052
(defun my-ivy-kill-current ()
  "Save current Ivy candidate to the `kill-ring'."
  (interactive)
  (kill-new (ivy-state-current ivy-last)))

(use-package ivy
  :after amx
  :config
  (setq enable-recursive-minibuffers t)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                (t . ivy--regex-fuzzy)))
  (ivy-mode)
  (global-set-key (kbd "C-x b") 'ivy-switch-buffer)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "M-w") #'my-ivy-kill-current))

(use-package counsel
  :config
  (counsel-mode)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-M-i") 'counsel-company)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char))

(use-package swiper
  :defer t
  :config
  (global-set-key (kbd "C-s") 'swiper))

(use-package goto-chg
  :config
  (global-set-key (kbd "C-\\") 'goto-last-change))

(use-package hungry-delete
  :diminish hungry-delete-mode
  :config
  (add-hook 'minibuffer-setup-hook (lambda () (hungry-delete-mode -1)))
  (global-hungry-delete-mode))

(use-package move-text
  :config
  (move-text-default-bindings))

(defun er/add-go-mode-expansions ()
  "Adds Go-specific expansions for buffers in go-mode"
  (set (make-local-variable 'er/try-expand-list) (append
                                                  '(go-guru-expand-region)
                                                  er/try-expand-list)))

(use-package expand-region
  :bind ("C-=" . er/expand-region)
  :config
  (require 'subword-mode-expansions)
  (er/enable-mode-expansions 'bazel-mode 'er/add-python-mode-expansions)
  ;;  (er/enable-mode-expansions 'go-mode 'er/add-cc-mode-expansions))
  (er/enable-mode-expansions 'go-mode 'er/add-go-mode-expansions))

(use-package multiple-cursors
  :bind (("C-;" . mc/mark-all-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

(use-package avy
  :bind (("C-\"" . avy-move-region)
         ("C-:" . avy-kill-region)
         ("C-'" . avy-goto-char-timer)
         ("M-g g" . avy-goto-line))
  :config
  (setq avy-background t)
  (avy-setup-default))

(use-package ace-window
  :bind ("M-o" . ace-window))

(use-package ace-link
  :config
  (ace-link-setup-default))

(use-package browse-kill-ring
  :disabled t
  :config
  (browse-kill-ring-default-keybindings))

(use-package rainbow-delimiters
  :hook (lisp-mode . rainbow-delimiters-mode))

(use-package anzu
  :diminish anzu-mode
  :config
  (progn
    (setq anzu-cons-mode-line-p nil)
    (global-anzu-mode)))

(use-package kaolin-themes
  :disabled t
  :config
  (load-theme 'kaolin-dark t)
  (kaolin-treemacs-theme))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (load-theme 'doom-one t))

(use-package doom-modeline
  :config
  (setq inhibit-compacting-font-caches t)
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project)
  (setq doom-modeline-github t)
  (setq doom-modeline-github-interval (* 1 60))
  (setq doom-modeline-major-mode-color-icon nil)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-env-version nil)
  (doom-modeline-mode))

(use-package highlight-indent-guides
  :config
  (add-hook 'yaml-mode-hook 'highlight-indent-guides-mode))

(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package treemacs-icons-dired
  :after treemacs dired
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit)

(use-package git-gutter
  :diminish git-gutter-mode
  :defer 2
  :config
  (setq git-gutter:window-width 2)
  (global-git-gutter-mode))

(use-package git-timemachine
  :defer t)

(use-package ripgrep
  :defer t)

(use-package projectile
  :config
  (projectile-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-completion-system 'ivy)
  (setq projectile-globally-ignored-directories
        (append projectile-globally-ignored-directories
                '("*.terraform"
                  "*vendor"))))

(use-package counsel-projectile
  :config
  (setcar (nthcdr 0 counsel-projectile-switch-project-action) 2)
  (counsel-projectile-mode))

(use-package exec-path-from-shell
  :config
  (progn
    (setq exec-path-from-shell-variables '("GOPATH" "PATH" "MANPATH"))
    (setq exec-path-from-shell-arguments nil)
    (exec-path-from-shell-initialize)))

(use-package smartparens)

(use-package editorconfig
  :diminish (editorconfig-mode . " ⓔ")
  :config
  (editorconfig-mode))

(use-package restclient
  :defer t)

(use-package magit
  :defer t
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package forge
  :after magit)

(use-package winum
  :defer t
  :config
  (winum-mode))

(use-package golden-ratio
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
  :config
  (smooth-scrolling-mode))

(use-package rainbow-mode
  :diminish rainbow-mode
  :config
  (rainbow-mode))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode))

(use-package flycheck
  :defer 2
  :diminish (flycheck-mode . " ⓢ")
  :config
  (global-flycheck-mode)
  (setq flycheck-go-golint-executable "golint"))

(use-package flycheck-package
  :config
  (flycheck-package-setup))

;; (flycheck-define-checker protobuf
;;   "A Scala syntax checker using the Scala compiler.

;; See URL `https://www.scala-lang.org/'."
;;   :command ("prototool" "lint" source-original)
;;   :error-patterns
;;   ((error line-start (file-name) ":" line ":" column ":" (message) line-end))
;;   :modes protobuf-mode
;;   :next-checkers 'protobuf-protoc)

(use-package company
  :defer 2
  :diminish (company-mode . " ⓐ")
  :bind (:map company-search-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              :map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :config
  (global-company-mode)
  (setq company-dabbrev-downcase nil))

;; (defun setup-lsp-keymap ()
;;   "Add a keymap to go-mode for lsp commands."
;;   (let ((m (define-prefix-command 'go-lsp-map)))
;;     (define-key m "d" #'lsp-find-declaration)
;;     (define-key m "r" #'lsp-find-references)
;;     (define-key m "t" #'lsp-find-type-definition))

;;   (define-key go-mode-map (kbd "C-c C-l") 'go-lsp-map))

(use-package lsp-mode
  :defer t
  :init (setq lsp-keymap-prefix "C-c C-l")
  :commands lsp
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-symbol-highlighting-skip-current t)
  (setq lsp-enable-links nil)
  (setq lsp-eldoc-render-all t)
  (setq lsp-prefer-capf t)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-enable-file-watchers nil)
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]bazel-.*\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]docs\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]web\\'"))

(use-package lsp-ui
  :defer t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil))

(use-package lsp-java
  :defer t
  :hook ((java-mode . lsp)))

(use-package dockerfile-mode
  :defer t)

(use-package nasm-mode
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.s\\'" . nasm-mode)))

(use-package bazel-mode
  :defer t
  :config
  (progn
    (add-hook 'bazel-mode-hook (lambda () (add-hook 'before-save-hook #'bazel-format nil t)))))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package go-mode
  :defer t
  :config
  (progn
    (setq gofmt-command "goimports")
    ;;(add-hook 'before-save-hook #'gofmt-before-save)
    (add-hook 'go-mode-hook 'subword-mode)
    (add-hook 'go-mode-hook #'lsp-deferred)
    (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
    ;;(add-hook 'go-mode-hook 'setup-lsp-keymap)
    (add-hook 'go-mode-hook (lambda () (local-set-key (kbd "C-.") #'lsp-find-definition)))
    (add-hook 'go-mode-hook (lambda () (local-set-key (kbd "C-,") #'xref-pop-marker-stack)))
    ;;(add-hook 'go-mode-hook (lambda () (define-key go-mode-map (kbd "C-=") #'go-guru-expand-region)))
    (add-hook 'go-mode-hook (lambda () (add-to-list 'flycheck-disabled-checkers 'go-test)))
    (add-hook 'go-mode-hook (lambda () (add-to-list 'flycheck-disabled-checkers 'go-vet)))
    (add-hook 'go-mode-hook (lambda () (add-to-list 'flycheck-disabled-checkers 'go-build)))
    (add-hook 'go-mode-hook (lambda () (add-to-list 'flycheck-disabled-checkers 'go-megacheck)))
    (add-hook 'go-mode-hook (lambda () (add-to-list 'flycheck-disabled-checkers 'go-staticcheck)))))

(use-package go-guru)

(use-package go-add-tags
  :defer t
  :config
  (with-eval-after-load 'go-mode
    (define-key go-mode-map (kbd "C-c t") #'go-add-tags)))

(use-package gotest
  :defer t)

(use-package go-rename
  :defer t)

(use-package protobuf-mode
  :defer t)

(use-package anaconda-mode
  :defer t
  :config
  (progn
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode)))

(use-package company-anaconda
  :defer t
  :config
  (eval-after-load "company" '(add-to-list 'company-backends 'company-anaconda)))

(use-package yaml-mode
  :defer t)

(use-package ensime
  :defer t
;;  :pin melpa-stable
  :init
  (setq ensime-startup-snapshot-notification nil)
  (setq ensime-startup-notification nil)
  (eval-after-load "ensime-mode"
    '(define-key ensime-mode-map (kbd "M-p") nil)))

(use-package sbt-mode
  :defer t)

(defun setup-c-clang-options ()
  (setq irony-additional-clang-options (quote ("-std=c11"))))

(defun setup-cpp-clang-options ()
  (setq irony-additional-clang-options (quote ("-std=c++14" "-stdlib=libc++"))))

(use-package irony
  :hook (c++-mode c-mode objc-mode)
  :config
  (progn
    (add-hook 'c++-mode-hook 'setup-cpp-clang-options)
    (add-hook 'c-mode-hook 'setup-c-clang-options)))

(use-package company-irony
  :defer t
  :config
  (progn
    (eval-after-load 'company '(add-to-list 'company-backends 'company-irony))
    (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)))

(use-package flycheck-irony
  :defer t
  :config
  (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

(use-package company-c-headers
  :defer t
  :config
  (add-to-list 'company-backends 'company-c-headers))

(use-package clang-format
  :defer t
  :config
  (progn
    (setq clang-format-style "llvm")
    (add-hook 'c++-mode-hook (lambda () (add-hook 'before-save-hook 'clang-format-buffer nil t)))
    (add-hook 'c-mode-hook (lambda () (add-hook 'before-save-hook 'clang-format-buffer nil t)))))

(use-package google-c-style
  :defer t
  :config
  (progn
    (add-hook 'c-mode-hook 'google-set-c-style)
    (add-hook 'c-mode-hook 'google-make-newline-indent)
    (add-hook 'c++-mode-hook 'google-set-c-style)
    (add-hook 'c++-mode-hook 'google-make-newline-indent)))

(use-package js
  :defer t
  :config
  (setq js-indent-level 2))

(defun set-jsx-indentation ()
  (setq-local sgml-basic-offset js2-basic-offset))

(use-package js2-mode
  :defer t
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))
    (add-hook 'js2-jsx-mode-hook #'set-jsx-indentation)
    (setq js2-mode-show-strict-warnings nil)
    (setq js2-mode-show-parse-errors nil)))

(use-package add-node-modules-path
  :ensure
  :load-path "add-node-modules-path/"
  :config
  (progn
    (eval-after-load 'js2-mode
      '(add-hook 'js2-mode-hook #'add-node-modules-path))
    (eval-after-load 'web-mode
      '(add-hook 'web-mode-hook #'add-node-modules-path))
    (eval-after-load 'typescript-mode
      '(add-hook 'typescript-mode-hook #'add-node-modules-path))))

(use-package typescript-mode
  :defer t)

(use-package tide
  :defer t
  :diminish tide-mode
  :config
  (progn
    (add-hook 'tide-mode-hook (lambda () (add-hook 'before-save-hook #'tide-format-before-save nil t)))
    (add-hook 'typescript-mode-hook #'tide-setup)
    (add-hook 'web-mode-hook
              (lambda ()
                (when (string-equal "tsx" (file-name-extension buffer-file-name))
                  (progn
                    (tide-setup)
                    (eldoc-mode)
                    (flycheck-add-mode 'typescript-tslint 'web-mode)
                    (flycheck-add-mode 'typescript-tide 'web-mode)))))))

(use-package json-mode
  :defer t)

(use-package jsonnet-mode
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.libsonnet\\'" . jsonnet-mode)))

(use-package eslint-fix
  :defer t
  :config
  (eval-after-load 'js2-mode
    '(add-hook 'js2-mode-hook '(add-hook 'before-save-hook 'eslint-fix nil t))))

(use-package web-mode
  :defer t
  ;:mode ("\\.html?\\'" . web-mode)
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))))

(use-package emmet-mode
  :defer t
  :config
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode))

(use-package markdown-mode
  :defer t)

(use-package terraform-mode
  :defer t
  :config
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))

(use-package swift-mode
  :defer t)

(use-package flycheck-swift3
  :defer t
  :config
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-swift3-setup)))

(use-package flycheck-objc-clang
  :defer t
  :config
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-objc-clang-setup)))

(use-package which-key
  :defer 2
  :diminish (which-key-mode . " Ⓚ")
  :config
  (progn
    (which-key-mode)))

(use-package guru-mode
  :diminish guru-mode
  :hook (prog-mode)
  :config
  (progn
    (setq guru-warn-only t)))

(use-package nand2tetris
  :disabled t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.hdl\\'" . nand2tetris-mode))
  (setq nand2tetris-core-base-dir "~/Development/nand2tetris"))

;; old packages

;; (use-package flx-ido
;;   :disabled t
;;   :config
;;   (progn
;;     (flx-ido-mode)
;;     (setq ido-enable-flex-matching t)
;;     (setq ido-use-faces nil)))

;; ;; switch to rg
;; (use-package ag
;;   :disabled t)

;; ;; switch to treemacs
;; (use-package neotree
;;   :disabled t
;;   :config
;;   (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
;;   (setq neo-window-width 40))

;; (use-package selectric-mode
;;   :disabled t
;;   :load-path "selectric-mode/"
;;   :diminish (selectric-mode . "♬")
;;   :config
;;   (selectric-mode))

;; ;; just use show-paren-mode
;; (use-package highlight-parentheses
;;   :disabled t
;;   :diminish highlight-parentheses-mode
;;   :init
;;   (add-hook 'prog-mode-hook #'highlight-parentheses-mode))

;; (use-package highlight-symbol
;;   :disabled t
;;   :config
;;   (highlight-symbol-mode))

;; (use-package highlight-indentation
;;   :disabled t
;;   :config
;;   (highlight-indentation-mode))

;; (use-package indent-guide
;;   :disabled t
;;   :diminish indent-guide-mode
;;   :config
;;   (indent-guide-global-mode))

;; (use-package centered-cursor-mode
;;   :disabled t
;;   :diminish (centered-cursor-mode . " ⊝")
;;   :config
;;   (progn
;;     (global-centered-cursor-mode)
;;     (setq ccm-recenter-at-end-of-file t
;;           ccm-ignored-commands '(mouse-drag-region
;;                                  mouse-set-point
;;                                  widget-button-click
;;                                  scroll-bar-toolkit-scroll))))

;; (use-package diff-hl
;;   :disabled t
;;   :config
;;   (global-diff-hl-mode))

;; (use-package fill-column-indicator
;;   :disabled t
;;   :defer 2
;;   :config
;;   (fci-mode))

;; (use-package iedit
;;   :disabled true
;;   :bind ("C-;" . iedit-mode))

;; (use-package flatui-theme
;;   :disabled t
;;   :defer t
;;   :config
;;   (setup-flat-ui))

;; (use-package dracula-theme
;;   :disabled t
;;   :defer t)

;; (use-package spacemacs-common
;;   :disabled t)
;;   :config
;;   (if (daemonp)
;;       (add-hook 'after-make-frame-functions
;;                 (lambda (frame)
;;                   (with-selected-frame frame (load-theme 'spacemacs-dark))))
;;     (load-theme 'spacemacs-dark)))


;; (defun init-spaceline ()
;;   (require 'spaceline-config)
;;   (spaceline-spacemacs-theme)
;;                                         ;(set-face-attribute 'mode-line nil :box nil)
;;                                         ;(set-face-attribute 'mode-line-inactive nil :box nil)
;;                                         ;(set-face-attribute 'mode-line-highlight nil :box nil)
;;   (setq powerline-default-separator "bar")
;;   (setq spaceline-workspace-numbers-unicode t)
;;   (setq spaceline-window-numbers-unicode t)
;;   (setq powerline-height 25)
;;   (setq powerline-text-scale-factor 1.0)
;;   (setq powerline-image-apple-rgb t)
;;   (spaceline-compile))

;; (use-package spaceline
;;   :disabled t
;;   :demand t
;;   :config
;;   (if (daemonp)
;;       (add-hook 'after-make-frame-functions
;;                 (lambda (frame)
;;                   (with-selected-frame frame (init-spaceline))))
;;     (init-spaceline)))

;; (use-package spaceline-all-the-icons
;;   :disabled t
;;   :after spaceline
;;   :config
;;   (setq spaceline-all-the-icons-clock-always-visible nil)
;;   (spaceline-all-the-icons--setup-anzu)
;;   (spaceline-all-the-icons--setup-package-updates)
;;   (spaceline-all-the-icons--setup-paradox)
;;   (spaceline-all-the-icons--setup-neotree)
;;   (setq spaceline-all-the-icons-separator-type 'none)
;;   (spaceline-all-the-icons-theme))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(lispy lispi yaml-mode winum which-key web-mode use-package treemacs-projectile treemacs-magit treemacs-icons-dired tide terraform-mode swift-mode smooth-scrolling smartparens ripgrep restclient rainbow-mode rainbow-delimiters protobuf-mode paradox nasm-mode multiple-cursors move-text lsp-ui json-mode js2-mode hungry-delete guru-mode gotest google-c-style golden-ratio go-rename go-guru go-eldoc go-add-tags git-gutter flycheck-swift3 flycheck-package flycheck-objc-clang flycheck-irony flycheck-gometalinter flx expand-region exec-path-from-shell eslint-fix ensime emmet-mode editorconfig doom-themes doom-modeline dockerfile-mode diminish counsel company-lsp company-irony company-go company-c-headers company-anaconda clang-format browse-kill-ring bazel-mode anzu amx add-node-modules-path ace-link)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
