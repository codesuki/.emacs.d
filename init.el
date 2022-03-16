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
(if (and (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (message "Native compilation is available")
  (message "Native complation is *not* available"))
(if (functionp 'json-serialize)
    (message "Native JSON is available")
  (message "Native JSON is *not* available"))

(defvar last-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold 402653184 gc-cons-percentage 0.6 file-name-handler-alist nil)
(setq read-process-output-max (* 1024 1024))
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 100000000
                  gc-cons-percentage 0.1
                  file-name-handler-alist last-file-name-handler-alist)))

(defconst codesuki-local-dir (expand-file-name (convert-standard-filename ".local/") user-emacs-directory))

(defun to-local-path (path)
  (expand-file-name (convert-standard-filename path) codesuki-local-dir))

;;(add-to-list 'native-comp-eln-load-path (to-local-path "eln-cache/"))

;; from doom emacs
;; (defadvice! codesuki--write-to-local-path-a (fn &rest args)
;;   :around #'locate-user-emacs-file
;;   (let ((user-emacs-directory doom-etc-dir))
;;     (apply fn args)))

(setq straight-base-dir codesuki-local-dir)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name ".local/straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://github.com/raxod502/straight.el/blob/af5437f2afd00936c883124d6d3098721c2d306c/bootstrap.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)

(straight-use-package 'use-package)

(defun add-to-load-path (dir) (add-to-list 'load-path dir))

(defun disable-ui ()
  ;; from: doom emacs
  ;; https://github.com/hlissner/doom-emacs/blob/4a6de2419c81d120ce363a2ba189789c7a2424d4/core/core-ui.el#L265
  (push '(menu-bar-lines . 0) default-frame-alist)
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist)
  (setq menu-bar-mode nil)
  (setq tool-bar-mode nil)
  (setq scroll-bar-mode nil)
  ;; (setq tooltip-mode nil)
  (setq initial-scratch-message nil)
  (setq frame-inhibit-implied-resize t))

;; Disable bidirectional text support because we don't need it.
;; Ref: https://200ok.ch/posts/2020-09-29_comprehensive_guide_on_handling_long_lines_in_emacs.html
(setq-default bidi-display-reordering 'left-to-right)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

(setq redisplay-skip-fontification-on-input t)

(defun configure-startup ()
    (setq inhibit-startup-screen t)
    (setq inhibit-startup-echo-area-message t))

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
  (set-face-attribute 'default nil :font "Input Mono Condensed-14" :weight 'medium)
  (set-face-attribute 'fixed-pitch nil :font "Input Mono Condensed-14" :weight 'medium)
  (set-face-attribute 'variable-pitch nil :font "Input Sans Compressed-14" :weight 'medium)
  (add-hook 'text-mode-hook #'variable-pitch-mode)
  (add-hook 'prog-mode-hook #'variable-pitch-mode)
  (add-hook 'protobuf-mode-hook #'variable-pitch-mode))

(defun setup-modeline-font ()
  ;; (set-face-attribute 'mode-line-inactive nil :font "InputMonoCondensed-14")
  ;; (set-face-attribute 'mode-line nil :font "InputMonoCondensed-14")
  )

(defun setup-window-divider ()
  ;; From doom emacs.
  "The native window divider takes a pixel out of the fringe."
  (setq window-divider-default-places t)
  (setq window-divider-default-bottom-width 1)
  (setq window-divider-default-right-width 1)
  (window-divider-mode))

(defun setup-minibuffer ()
  ;; from doom emacs.
  "Keep the cursor out of the read-only portion of the minibuffer."
  ;; Try to keep the cursor out of the read-only portions of the minibuffer.
  (setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(defun setup-term-font ()
  '(term ((t (:background "#292b2e" :foreground "#b2b2b2" :family "InputCustomMonoCompressed")))))

(defun enable-extra-functionality ()
    (put 'narrow-to-region 'disabled nil)
    (put 'upcase-region 'disabled nil)
    (put 'downcase-region 'disabled nil)
    (put 'dired-find-alternate-file 'disabled nil))

(defun disable-bell ()
  (setq ring-bell-function 'ignore
        visible-bell nil))

(defconst auto-save-path (expand-file-name "~/.emacs.d/.local/auto-save/"))
(setq auto-save-list-file-prefix "~/.emacs.d/.local/auto-save-list/.saves-")

(defun setup-builtins ()
  (prefer-coding-system 'utf-8)
  (setq window-combination-resize t)
  (setq-default fill-column 80)
  (setq-default indent-tabs-mode nil)
  (setq-default show-trailing-whitespace 't)
  (setq-default indicate-empty-lines 't)
  (setq save-interprogram-paste-before-kill t)
  (setq-default sentence-end-double-space nil)
  (setq column-number-mode t)
  (blink-cursor-mode 0)
  (setq use-short-answers t)
  (setq make-backup-files nil)
  (setq delete-by-moving-to-trash t)
  (setq use-dialog-box nil)
  (setq confirm-kill-processes nil)
  (setq load-prefer-newer t)
  (unless (file-exists-p auto-save-path)
    (make-directory auto-save-path t))
  (setq auto-save-file-name-transforms
        `((".*" ,auto-save-path t)))
  (setq minibuffer-prompt-properties
        '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))

;; from https://github.com/purcell/emacs.d/blob/master/lisp/init-windows.el
(defmacro codesuki--with-other-buffer (split-function)
  `(defun ,(intern (concat "codesuki--with-other-buffer-" (symbol-name split-function))) (arg)
     "Split this window and switch to the new window unless ARG is provided."
          (interactive "P")
          (funcall ',split-function)
          (let ((target-window (next-window)))
            (set-window-buffer target-window (other-buffer))
            (unless arg
              (select-window target-window)))))

(defun setup-window-splitting ()
      (global-set-key (kbd "C-x 2") (codesuki--with-other-buffer split-window-below))
      (global-set-key (kbd "C-x 3") (codesuki--with-other-buffer split-window-horizontally)))

;; TODO: consider not doing this in comments because we might want to delete whitespace.
(defun codesuki-delete-syntax (arg)
  "Delete characters forward until encountering the end of a syntax element.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-same-syntax arg) (point))))

(defun codesuki-backward-delete-syntax (arg)
  "Delete characters backward until encountering the beginning of a syntax element.
With argument ARG, do this that many times."
  (interactive "p")
  (codesuki-delete-syntax (- arg)))

(defun kill-region-or-backward-delete-syntax (&optional arg region)
  "`kill-region' if the region is active, otherwise
`backward-kill-word'. Also inhibits `backward-kill-word' from
adding to `kill-ring'."
  (interactive
   (list (prefix-numeric-value current-prefix-arg) (use-region-p)))
  (if region
      (kill-region (region-beginning) (region-end))
    (codesuki-backward-delete-syntax arg)))

(defun setup-kill-backwards-word ()
  (global-set-key (kbd "C-w") 'kill-region-or-backward-delete-syntax))

(defun codesuki-kill-line (&optional arg)
  "Behaves like `kill-whole-line' when `arg' is provided."
  (interactive "P")
  (if arg
      ;; C-u defaults to 4, but with `(interactive "P")' we actually get `(4)'.
      ;; `C-u 4' gives us 4. So we know which one was pressed.
      (let ((arg (if (listp arg) 1 arg)))
            (kill-whole-line arg))
        (kill-line arg)))

(keymap-global-set "C-k" 'codesuki-kill-line)

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

;; From https://oremacs.com/2014/12/23/upcase-word-you-silly/
(defun setup-better-capitalize-word ()
  "Changes case functions to start from the beginning of a word
instead of from point."
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

;; from doom emacs
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

(defun setup-folders ()
  ""
  (setq async-byte-compile-log-file (to-local-path "async-bytecomp.log"))
  (setq desktop-dirname (to-local-path "desktop"))
  (setq desktop-base-file-name "autosave")
  (setq desktop-base-lock-name "autosave-lock")
  (setq pcache-directory (to-local-path "pcache/"))
  (setq shared-game-score-directory (to-local-path "shared-game-score/"))
  (setq amx-save-file (to-local-path "amx-save.el"))
  (eval-after-load 'projectile
    `(make-directory ,(to-local-path "projectile/") t))
  (setq projectile-cache-file (to-local-path "projectile/cache.el"))
  (setq projectile-known-projects-file (to-local-path "projectile/known-projects.el"))
  (setq transient-history-file (to-local-path "transient/history.el"))
  (setq transient-levels-file (to-local-path "transient/levels.el"))
  (setq transient-values-file (to-local-path "transient/values.el"))
  (setq forge-database-file (to-local-path "forge/database.sqlite"))
  (setq forge-post-directory (to-local-path "forge/posts/"))
  (setq anaconda-mode-installation-directory (to-local-path "anaconda-mode/"))
  (setq bookmark-default-file (to-local-path "bookmark-default.el"))
  (setq project-list-file (to-local-path "project-list.el"))
  (setq recentf-save-file (to-local-path "recentf-save.el"))
  (setq lsp-server-install-dir (to-local-path "lsp/server/"))
  (setq lsp-session-file (to-local-path "lsp/session.el"))
  (setq lsp-java-workspace-dir (to-local-path "lsp-java/workspace/"))
  (setq dap-breakpoints-file (to-local-path "dap/breakpoints.el"))
  (setq url-cache-directory (to-local-path "url/cache/"))
  (setq url-configuration-directory (to-local-path "url/"))
  (setq url-cookie-file (to-local-path "url/cookies.el"))
  (setq url-history-file(to-local-path "url/history.el"))
  (setq save-place-file (to-local-path "saveplace.el"))
  (setq savehist-file (to-local-path "savehist.el")))


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
  (setup-folders)
  (autoload 'View-scroll-half-page-forward "view")
  (autoload 'View-scroll-half-page-backward "view")
  (keymap-global-set "C-v" 'View-scroll-half-page-forward)
  (keymap-global-set "M-v" 'View-scroll-half-page-backward)
  (keymap-global-set "M-z" 'zap-up-to-char)
  (keymap-global-set "C-?" 'help-command)
  (keymap-global-set "C-h" 'delete-backward-char)
  (keymap-global-set "M-u" 'upcase-dwim)
  (keymap-global-set "M-l" 'downcase-dwim)
  (keymap-global-set "M-c" 'capitalize-dwim)
  ;; This frees M-\ because `cycle-spacing' does both.
  (keymap-global-set "M-SPC" 'cycle-spacing))

(setq custom-file (locate-user-emacs-file "custom.el"))

(defun init-ui ()
  "Init UI relating settings.
FRAME is received from `after-make-frame-functions'."
  (disable-ui)
  (push '(width . 220) default-frame-alist)
  (push '(height . 60) default-frame-alist)
  (setup-font)
  (setup-modeline-font)
  (setup-window-divider)
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

(defvar codesuki--quit-hook nil)

(defvar codesuki--popups nil)

(defun codesuki-quit ()
  "`codesuki-quit' executes `codesuki--quit-hook' until one hook
returns non-nil. If all hooks return nil it executes
`keyboard-quit'."
  (interactive)
  (unless (run-hook-with-args-until-success 'codesuki--quit-hook)
    (keyboard-quit)))

;; We remap keyboard-quit instead of `C-g', because `project-switch-project'
;; directly compares the key-map entry to `keyboard-quit'. If it returns
;; `codesuki-quit' it will not cancel the command.
(global-set-key [remap keyboard-quit] 'codesuki-quit)

;; this function should return nil if nothing is killed t otherwise.
(defun codesuki--kill-popups ()
  ;; We should pop after `kill-buffer' returns `t'. In case of vterm we get
  ;; asked whether or not we want to kill the buffer. If we decline then this
  ;; will remove the buffer from the list and we cannot close it with C-g
  ;; anymore.
  ;;
  ;; Actually it would be OK to never `pop' because `codesuki--delete-popup'
  ;; cleans up once a buffer is killed.
  (let ((buffer (pop codesuki--popups)))
    (when buffer
      ;; `kill-buffer': This function returns `t' if it actually killed the buffer.
      ;; It returns `nil' if the user refuses to confirm or if buffer-or-name was
      ;; already dead.
      (kill-buffer buffer))))

(add-hook 'codesuki--quit-hook 'codesuki--kill-popups)

(defun codesuki--delete-popup ()
  (delete (current-buffer) codesuki--popups))

(defun codesuki--display-buffer-in-side-window (buffer &rest alist)
  (let ((window (display-buffer-in-side-window buffer (car alist))))
    (add-to-list 'codesuki--popups buffer))
  (with-current-buffer buffer
    (add-hook 'kill-buffer-hook #'codesuki--delete-popup nil t)))

;; TODO: make it easier to set window parameters. It's quite repetitive.
(setq display-buffer-alist '(("\\*Help\\*"
                              (codesuki--display-buffer-in-side-window)
                              (window-parameters (mode-line-format . none)))
                             ("\\*Helpful"
                              (codesuki--display-buffer-in-side-window)
                              (window-parameters (mode-line-format . none)))
                             ("\\*info\\*"
                              (codesuki--display-buffer-in-side-window)
                              (side . right))
                              ;; this one does not close on C-g. This was because the debugger was on.
                             ;; ("\\*Backtrace\\*"
                             ;;  (codesuki--display-buffer-in-side-window))
                             ("\\*eldoc\\*"
                              (codesuki--display-buffer-in-side-window))
                             ("\\*Compile-Log\\*"
                              (codesuki--display-buffer-in-side-window))
                             ("\\*Warnings\\*"
                              (codesuki--display-buffer-in-side-window))
                             ("\\*vterm\\*"
                              (codesuki--display-buffer-in-side-window)
                              (window-parameters (mode-line-format . none))))) ;; modeline should be hidden and switch to it

;; built-in but newer versions on elpa
(use-package xref
  :config
  (setq xref-search-program 'ripgrep))

(use-package eldoc
  :diminish eldoc-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))

(use-package project)

;;(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(use-package el-patch
  :straight (el-patch :branch "master"))

(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package diminish)

(use-package electric
  :config
  ;; This prevents the following scenario (| is the point): |defun -> (|)defun
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  (electric-indent-mode)
  (electric-pair-mode))

(use-package paren
  :config
  ;; Interesting, but should set the border to distinguish. Check docs.
  ;; (setq show-paren-context-when-offscreen 'child-frame)
  (setq show-paren-delay 0.1)
  (setq show-paren-when-point-inside-paren t)
  (show-paren-mode))

;; Better rectangle editing. This conflicts with org-mode `C-RET'.
;; (use-package cua-base
;;   :config
;;   (cua-selection-mode t))

(use-package delsel
  :config
  (delete-selection-mode))

(use-package autorevert
  :diminish auto-revert-mode)

(use-package repeat
  :config
  ;; (describe-repeat-maps) to see all possibilities.
  (repeat-mode))

(use-package mwheel
  :straight nil
  :config
  (setq scroll-margin 0)
  (setq scroll-preserve-screen-position 'always)
  (setq scroll-conservatively 101) ;; don't recenter when point moves out of screen
  (setq mouse-wheel-progressive-speed nil)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . hscroll))))

(use-package dired-x
  :straight nil)

(use-package ibuffer
  :config
  (defalias 'list-buffers 'ibuffer))

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
  (setq whitespace-line-column 80)
    (add-hook 'before-save-hook 'whitespace-cleanup))

(use-package gnutls)

(use-package exec-path-from-shell
  :config
  (progn
    (setq exec-path-from-shell-variables '("GOPATH" "PATH" "MANPATH"))
    (setq exec-path-from-shell-arguments nil)
    (exec-path-from-shell-initialize)))

(use-package ispell
  :config
  (setenv "LANG" "en_US.UTF-8")
  (setq ispell-dictionary "en_US-large,de_DE_frami")
  (setq ispell-program-name "hunspell")
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US-large,de_DE_frami")
  (add-hook 'org-mode-hook 'flyspell-mode))

(use-package flyspell
  :defer t
  :config
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil))

(use-package helpful
  :defer t
  :config
  (define-key help-map (kbd "f") #'helpful-callable)
  (define-key help-map (kbd "v") #'helpful-variable)
  (define-key help-map (kbd "k") #'helpful-key))

(defun setup-org-faces ()
  "Make fixed-width what should be fixed-width."
  (dolist (face '(org-block
                  org-table
                  org-formula
                  org-code
                  org-indent
                  ;; org-verbatim
                  ;; org-special-keyword
                  ;; org-meta-line
                  org-checkbox
                  ))
    (let ((current (face-attribute face :inherit)))
      (set-face-attribute face nil :inherit (delete-dups
                                             (append
                                              (if (listp current)
                                                  current
                                                (list current))
                                              '(fixed-pitch)))))))

(use-package japanese-holidays
  :config
  (setq calendar-holidays (append holiday-christian-holidays japanese-holidays)))

(require 'cl-lib)
;; (defun codesuki--org-agenda-inhibit-frame-change (fn &rest args)
;;   "This function prevents `org-agenda` to change the frame."
;;   (cl-letf (((symbol-function 'delete-other-windows) #'ignore)
;;             ((symbol-function 'delete-window) #'ignore))
;;     (apply fn args)))
;; (advice-add 'org-agenda-get-restriction-and-command :around 'codesuki--org-agenda-inhibit-frame-change)
;; (advice-add 'org-capture :around 'codesuki--org-agenda-inhibit-frame-change)


;; ;; For some reason org prevents popups with `org-no-popups`
;; (defun codesuki--org-allow-popups (fn buf &optional norecord)
;;   (switch-to-buffer-other-window buf norecord))
;; (advice-add 'org-switch-to-buffer-other-window :around 'codesuki--org-allow-popups)

(defun codesuki--org-vertically-split-follow-mode-window (fn &rest args)
  "Make `org-agenda-follow-mode` open windows in a new vertical split."
  (let ((split-height-threshold nil))
    (apply fn args)))
(advice-add 'org-agenda-do-context-action :around 'codesuki--org-vertically-split-follow-mode-window)
;; this lets org-agenda pop-up on the right side of the screen when using reorganize-frame
(advice-add 'org-agenda-prepare-window :around 'codesuki--org-vertically-split-follow-mode-window)

(use-package org
  :config
  (add-hook 'org-indent-mode-hook (lambda () (setup-org-faces)))
  (add-hook 'org-agenda-mode-hook (lambda () (setq-local show-trailing-whitespace nil)))
  (setq org-startup-indented t)
  (setq org-agenda-start-with-follow-mode t)
  (setq org-pretty-entities nil)
  (setq org-agenda-window-setup 'reorganize-frame)
  ;; if we use `reorganize-frame` then using the below will cause all other
  ;; windows to be deleted
  ;;(setq org-agenda-window-frame-fractions '(0.5 . 1.0))
  (setq org-agenda-restore-windows-after-quit t)
  (setq org-directory "~/notes")
  (setq org-agenda-files (list "inbox.org" "tickler.org" "projects.org"))
  (setq org-refile-targets '((("~/notes/projects.org") . (:maxlevel . 3))
                             (("~/notes/someday.org") . (:maxlevel . 3))))
  (setq org-agenda-custom-commands
        '(("o" "Overview"
           ((agenda "" ((org-agenda-hide-tags-regexp "@work")))
            (tags-todo "@work" ((org-agenda-overriding-header "@work")
                                (org-agenda-hide-tags-regexp "@work")))
            (tags-todo "@home" ((org-agenda-overriding-header "@home")
                                (org-agenda-hide-tags-regexp "@home")))
           (tags-todo "inbox" ((org-agenda-overriding-header "Inbox")
                               (org-agenda-hide-tags-regexp "inbox"))))
           ((org-agenda-prefix-format '((agenda . " %i %?-12t% s")
                                       (todo . " %i ")
                                       (tags . " %i ")
                                       (search . " %i ")))))))
  (setq org-todo-keywords '((sequence "TODO(t!)" "WAIT(w!)" "DONE(d)")))
  (setq org-log-done 'time)
  (setq org-tags-column 0) ;; alternatively make headings fixed width
  (setq org-capture-templates
        `(("i" "Inbox" entry  (file "inbox.org")
           ,(concat "* TODO %?\n"
                    ":PROPERTIES:\n"
                    ":CREATED: %U\n"
                    ":END:\n"))
          ("m" "Meeting" entry  (file "inbox.org")
           ,(concat "* TODO %^{Name or topic}\n"
                    ":PROPERTIES:\n"
                    ":CREATED: %U\n"
                    ":END:\n"
                    "\n"
                    "Participants: %^{Participants}\n"
                    "\n"
                    "%?"
                    ))
          ("b"
           "bookmark"
           entry
           (file "inbox.org")
           "* %:description\nSource: %:link\n%i\n"
           :immediate-finish t)))
  (keymap-global-set "C-c c" #'org-capture)
  (keymap-global-set "C-c a" #'org-agenda)
  ;; (org-babel-do-load-languages 'org-babel-load-languages '((C . t)))
  )

(defun codesuki--org-capture-to-website ()
  (message "%s" (org-capture-get :annotation))
  (message "%s" org-capture-current-plist)
  (message "%s" org-capture-plist)
  (message "%s" org-store-link-plist)
  (org-capture-put :target (list 'file+headline (nth 1 (org-capture-get :target)) (org-capture-get :annotation)))
  (org-capture-set-target-location))

(use-package org-roam
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (setq org-roam-directory "~/notes/kasten/")
  (setq org-roam-db-location (to-local-path "org-roam.db"))
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

;; maybe move this to display-buffer-alist for consistency
(defun codesuki--elfeed-display-buffer (buffer &optional action)
  (pop-to-buffer buffer '((display-buffer-below-selected) (dedicated 'elfeed) (window-height 100))))

(use-package elfeed
  :config
  (setq elfeed-show-entry-switch 'codesuki--elfeed-display-buffer)
  (setq elfeed-db-directory (to-local-path "elfeed")))

(use-package elfeed-org
  :config
  (setq rmh-elfeed-org-files '("~/notes/feeds.org"))
  (elfeed-org))

(use-package deft
  :config
  (setq deft-directory "~/notes"
        deft-recursive t
        deft-extensions '("org")))

(use-package gcmh
  :config
  (setq gcmh-idle-delay 'auto)
  (setq gcmh-auto-idle-delay-factor 10)
  (gcmh-mode))

(use-package winner
  :config
  (winner-mode))

(use-package unbound)

(use-package vterm
  :config
  (add-hook 'vterm-mode-hook (lambda () (setq-local show-trailing-whitespace nil))))

(use-package goto-chg
  :config
  (keymap-global-set "C-\\" 'goto-last-change))

(use-package hungry-delete
  :diminish hungry-delete-mode
  :config
  (setq-default hungry-delete-chars-to-skip " \t\f\v")
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
  :bind (("C-;" . mc/mark-all-dwim)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

(use-package avy
  ;; Default for M-i is `tab-to-tab-stop', which is useless. Alternatively use C-i which is the same as TAB. Another
  ;; good choice would be C-j for jump. It is `electric-newline-and-maybe-indent'. Turns out using C-j instead of RET
  ;; might be easier because it's on the home row.
  :bind (("M-i" . avy-goto-char-timer)
         ;; ("C-\"" . avy-move-region)
         ;; ("C-:" . avy-kill-region)
         ;; ("s-g" . avy-goto-line)
         )
  :config
  (setq avy-background t)
  (avy-setup-default))

(use-package ace-window
  :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package ace-link
  :config
  (ace-link-setup-default))

(use-package rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

(use-package anzu
  :config
  (setq anzu-cons-mode-line-p nil)
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  (global-anzu-mode))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (setq doom-themes-padded-modeline t)
  (load-theme 'doom-vibrant t)
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :config
  (setq inhibit-compacting-font-caches t)
  (setq all-the-icons-scale-factor 1.0)
  (setq doom-modeline-bar-width 4)
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project)
  (setq doom-modeline-buffer-encoding 'nondefault)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-github nil)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-env-version nil)
  (setq doom-modeline-modal-icon nil)
  (setq doom-modeline-irc nil)
  (doom-modeline-mode))

(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'column)
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

(use-package treemacs-icons-dired
  :after treemacs dired
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit)

(define-fringe-bitmap 'codesuki--vc-fringe-bmp-insert [#b11100000] nil nil '(center repeated))
(define-fringe-bitmap 'codesuki--vc-fringe-bmp-change [#b11100000] nil nil '(center repeated))
(define-fringe-bitmap 'codesuki--vc-fringe-bmp-delete
  [#b10000000
   #b11000000
   #b11100000
   #b11110000] nil nil 'bottom)

;;why does this even work?
(defun codesuki--fringe-bmp-from-type (type pos)
  ""
  (intern (format "codesuki--vc-fringe-bmp-%s" type)))

(use-package diff-hl
  :config
  (fringe-mode '4)
  (set-face-attribute 'diff-hl-insert nil :background nil)
  (set-face-attribute 'diff-hl-delete nil :background nil)
  (set-face-attribute 'diff-hl-change nil :background nil)
  (setq-default fringes-outside-margins t)
  (setq diff-hl-fringe-bmp-function 'codesuki--fringe-bmp-from-type)
  (global-diff-hl-mode))

(use-package git-timemachine
  :defer t)

(use-package ripgrep
  :defer t)

(use-package smartparens)

(use-package editorconfig
  :config
  (editorconfig-mode))

(use-package restclient
  :defer t)

(use-package magit
  :defer t)

(use-package forge
  :after magit)

(use-package winum
  :defer t
  :config
  (winum-mode))

(use-package rainbow-mode
  :diminish rainbow-mode
  :config
  (rainbow-mode))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode))

(use-package flymake
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)
  (flymake-mode))

(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package savehist
  :init
  (savehist-mode))

(use-package saveplace
  :init
  (save-place-mode))

(use-package eglot
  :defer t
  :config
  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c o") 'eglot-code-action-organize-imports)
  (define-key eglot-mode-map (kbd "C-c h") 'eldoc))

(use-package orderless
  :config
  (setq completion-styles '(orderless)))

(defun corfu-enable-in-minibuffer ()
  "Enable Corfu in the minibuffer if `completion-at-point' is bound."
  (when (where-is-internal #'completion-at-point (list (current-local-map)))
    ;; (setq-local corfu-auto nil) Enable/disable auto completion
    (corfu-mode 1)))

(use-package corfu
  :init
  (corfu-global-mode)
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer))

(use-package vertico
  :straight (vertico :includes vertico-directory
                     :files (:defaults "extensions/vertico-directory.el"))
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :init
  (vertico-mode))

(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package consult
  :bind (;; C-c bindings (mode-specific-map)
         ;; ("C-c h" . consult-history)
         ;; ("C-c m" . consult-mode-command)
         ;; ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ;; ("M-#" . consult-register-load)
         ;; ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ;; ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ;; ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ;; ("M-g m" . consult-mark)
         ;; ("M-g k" . consult-global-mark)
         ;; ("M-g i" . consult-imenu)
         ;; ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ;; ("M-s d" . consult-find)
         ;; ("M-s D" . consult-locate)
         ;; ("M-s g" . consult-grep)
         ;; ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ;; ("M-s L" . consult-line-multi)
         ;; ("M-s m" . consult-multi-occur)
         ;; ("M-s k" . consult-keep-lines)
         ;; ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ;; ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi))           ;; needed by consult-line to detect isearch)
  :init
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref))

(defun codesuki-embark-ace-window-file ()
  (interactive)
  (require 'ace-window)
  (let ((aw-dispatch-always t))
    ;;(aw-switch-to-window (aw-select nil))
    (aw-select nil #'aw-switch-to-window)
    (call-interactively #'find-file)))

(defun codesuki-embark-ace-window-buffer ()
  (interactive)
  (require 'ace-window)
  (let ((aw-dispatch-always t))
    ;;(aw-switch-to-window (aw-select nil))
    (aw-select nil #'aw-switch-to-window)
    (call-interactively #'switch-to-buffer)))

(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim))        ;; good alternative: M-.
   ;;("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  (define-key embark-file-map "o" #'codesuki-embark-ace-window-file)
  (define-key embark-buffer-map "o" #'codesuki-embark-ace-window-buffer)
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package emacs
  :init
  (setq mac-command-modifier 'meta)

  ;; flex can become very slow.
  ;; (setq completion-styles '(basic substring partial-completion flex))
  ;; (setq completion-styles '(basic substring partial-completion))
  (setq read-file-name-completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  (setq completion-ignore-case t)

  ;; TAB cycle if there are only few candidates
;;  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

(use-package dockerfile-mode
  :defer t)

(use-package nasm-mode
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.s\\'" . nasm-mode)))

(use-package emacs-bazel-mode
  :defer t
  :straight  '(bazel :type git :host github :repo "bazelbuild/emacs-bazel-mode")
  :commands bazel-mode
  :config
  (setq bazel-buildifier-before-save t))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package go-mode
  :defer t
  :config
  (add-hook 'go-mode-hook #'subword-mode)
  (add-hook 'go-mode-hook #'eglot-ensure)
  (setq gofmt-command "goimports")
  ;;(add-hook 'before-save-hook #'gofmt-before-save)
  ;;(add-hook 'go-mode-hook (lambda () (define-key go-mode-map (kbd "C-=") #'go-guru-expand-region)))
  )

(use-package go-guru)

(use-package go-tag
  :defer t
  :config
  (with-eval-after-load 'go-mode
    (define-key go-mode-map (kbd "C-c t") #'go-add-tags)))

(use-package protobuf-mode
  :defer t)

(use-package anaconda-mode
  :defer t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package yaml-mode
  :defer t)

(use-package google-c-style
  :straight (google-c-style :branch "master")
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
  :config
  (progn
    (eval-after-load 'js2-mode
      '(add-hook 'js2-mode-hook #'add-node-modules-path))
    (eval-after-load 'web-mode
      '(add-hook 'web-mode-hook #'add-node-modules-path))
    (eval-after-load 'typescript-mode
      '(add-hook 'typescript-mode-hook #'add-node-modules-path))))

(use-package typescript-mode
  :init
  (define-derived-mode typescript-tsx-mode typescript-mode "tsx")
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode #'subword-mode)
  (add-hook 'typescript-mode-hook #'eglot-ensure)
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-tsx-mode)))

;; source: https://github.com/emacs-tree-sitter/elisp-tree-sitter/issues/20
(defun tree-sitter-mark-bigger-node ()
  (interactive)
  (let* ((p (point))
         (m (or (mark) p))
         (beg (min p m))
         (end (max p m))
         (root (ts-root-node tree-sitter-tree))
         (node (ts-get-descendant-for-position-range root beg end))
         (node-beg (ts-node-start-position node))
         (node-end (ts-node-end-position node)))
    ;; Node fits the region exactly. Try its parent node instead.
    (when (and (= beg node-beg) (= end node-end))
      (when-let ((node (ts-get-parent node)))
        (setq node-beg (ts-node-start-position node)
              node-end (ts-node-end-position node))))
    (set-mark node-end)
    (goto-char node-beg)))

;; set this to languages that tree-sitter supports (golang, typescript)
;; TODO: check how this is different from go-guru-expand
;; (setq er/try-expand-list '(tree-sitter-mark-bigger-node))

(use-package tree-sitter
  :hook ((typescript-mode . tree-sitter-hl-mode)
         (typescript-tsx-mode . tree-sitter-hl-mode)
         (go-mode . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :after tree-sitter
  :config
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx)))

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

;; (use-package web-mode
;;   :defer t
;;   ;; :mode ("\\.html?\\'" . web-mode)
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode)))

(use-package prettier
  :defer t
  :hook ((typescript-mode . prettier-mode)
         (web-mode . prettier-mode)))

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

(use-package which-key
  :defer 2
  :config
  (which-key-mode)
  (setq which-key-use-C-h-commands nil))

(use-package guru-mode
  :diminish guru-mode
  :hook (prog-mode)
  :config
  (setq guru-warn-only t))

(use-package nand2tetris
  :disabled t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.hdl\\'" . nand2tetris-mode))
  (setq nand2tetris-core-base-dir "~/Development/nand2tetris"))

(load custom-file 'noerror 'nomessage)

;;; init.el ends here
