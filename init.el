;;; init.el --- My Config -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

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

;; Check if Emacs built with the features I definitely want.
(if (and (fboundp 'native-comp-available-p)
	 (native-comp-available-p))
    (message "Native compilation is available")
  (message "Native complation is *not* available"))
(if (functionp 'json-serialize)
    (message "Native JSON is available")
  (message "Native JSON is *not* available"))

;; From Doom Emacs. Speed up startup.
(defvar last-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold 402653184)
(setq gc-cons-percentage 0.6)
(setq file-name-handler-alist nil)
(setq read-process-output-max (* 1024 1024))
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold 100000000
		  gc-cons-percentage 0.1
		  file-name-handler-alist last-file-name-handler-alist)))

;; We configure all kinds of packages to put their data here.
(defconst codesuki-local-dir (expand-file-name (convert-standard-filename ".local/") user-emacs-directory))

(defun to-local-path (path)
  (expand-file-name (convert-standard-filename path) codesuki-local-dir))

;; I like Straight as a package manager because we can use specific commits of
;; packages.
(setq straight-base-dir codesuki-local-dir)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name ".local/straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://github.com/radian-software/straight.el/raw/b1062df10ba4c10ff7a3c61b9e124b3242b11bb2/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)

(straight-use-package 'use-package)

;; I want a clean Emacs after startup.
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)

;; Always use utf-8
(prefer-coding-system 'utf-8)

;; By setting this to true space is allocated equally between all windows
;; instead of just the one getting split.
(setq window-combination-resize t)

;; 80 is good for commit messages and comments. I prefer more in org-mode.
(setq-default fill-column 80)

;; I want to see trailing whitespace, but only when programming. Enabling this
;; everywhere is very noisy in `eww', `org-mode', etc.
  (add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))
;; (setq-default indent-tabs-mode nil)

;; This adds a marker on the fringe for empty lines at the end of the file.
(setq-default indicate-empty-lines 't)

;; This saves the contents of the OS clipboard to the kill-ring before
;; overwriting it when killing.
(setq save-interprogram-paste-before-kill t)

;; I like ending sentences with one space.
(setq-default sentence-end-double-space nil)

;; I like it when I can just keep pressing the same key
(setq next-line-add-newlines nil) ; I didn't like this after all.

;; I want to know on which column I am.
(setq column-number-mode t)

;; It's distracting.
(blink-cursor-mode 0)

;; I prefer y/n over yes/no.
(setq use-short-answers t)

;; Use coreutils ls because it supports --dired
(setq insert-directory-program "gls")

;; TODO: move this
(setq backup-directory-alist '(("." . "~/.emacs.d/.local/backups")))

(defconst auto-save-path (expand-file-name "~/.emacs.d/.local/auto-save/"))

(setq auto-save-list-file-prefix "~/.emacs.d/.local/auto-save-list/.saves-")

(unless (file-exists-p auto-save-path)
  (make-directory auto-save-path t))
(setq auto-save-file-name-transforms
      `((".*" ,auto-save-path t)))

(setq eshell-directory-name "~/.emacs.d/.local/eshell")

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
(setq url-history-file (to-local-path "url/history.el"))
(setq save-place-file (to-local-path "saveplace.el"))
(setq savehist-file (to-local-path "savehist.el"))

;; Don't delete files immediately, put them to trash.
;; TODO: Check if this works on macos.
(setq delete-by-moving-to-trash t)

;; I don't want OS dialog boxes.
(setq use-dialog-box nil)

;; Don't ask about killing processes when I want to exit Emacs.
(setq confirm-kill-processes nil)

;; Load the newer file, e.g. .el is newer than .elc, prefer the .el.
(setq load-prefer-newer t)

;; Lockfiles are only useful if you edit the same file from different Emacs
;; instances.
(setq create-lockfiles nil)

;; Normally the minibuffer prompt is editable which can be annoying.
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))

;; Disable bidirectional text support because we don't need it. This apparently
;; gives a slight speed boost.
;; Ref: https://200ok.ch/posts/2020-09-29_comprehensive_guide_on_handling_long_lines_in_emacs.html
(setq-default bidi-display-reordering 'left-to-right)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Smoother scrolling by skipping syntax highlighting.
(setq redisplay-skip-fontification-on-input t)

;; I want huge undo buffers to feel safe.
(setq undo-limit 80000000)
(setq undo-strong-limit 12000000)
(setq undo-outer-limit 12000000)

;; On Mac we can use the keychain.
(setq auth-sources '(macos-keychain-internet))

;; Don't resize the frame when changing font size, etc.
(setq frame-inhibit-implied-resize t)

;; I prefer scrolling a bit less than the normal `C-v' because I tend to lose
;; track of where I am.
(autoload 'View-scroll-half-page-forward "view")
(autoload 'View-scroll-half-page-backward "view")
(keymap-global-set "C-v" 'View-scroll-half-page-forward)
(keymap-global-set "M-v" 'View-scroll-half-page-backward)

;; I think the default is `zap-to-char', I prefer `zap-up-to-char'.
(keymap-global-set "M-z" 'zap-up-to-char)

;; i want `C-h' to behave like in the terminal.
(keymap-global-set "C-?" 'help-command)
(keymap-global-set "C-h" 'delete-backward-char)

;; Use dwim case functions.
(keymap-global-set "M-u" 'upcase-dwim)
(keymap-global-set "M-l" 'downcase-dwim)
(keymap-global-set "M-c" 'capitalize-dwim)

;; This frees M-\ because `cycle-spacing' does both.
(keymap-global-set "M-SPC" 'cycle-spacing)

;; Keeps `init.el' clean.
(setq custom-file (locate-user-emacs-file "custom.el"))

;; Disable keys defined in `ns-win.el'. To be honest I forgot why I did this,
;; but I think I didn't want to rely on these Mac specific keybindings.
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
(global-unset-key  [?\C-\s- ])

;; I prefer variable pitch fonts.
(set-face-attribute 'default nil :font "Input Sans Compressed-14" :weight 'medium)
(set-face-attribute 'fixed-pitch nil :font "Input Mono Condensed-14" :weight 'medium)
(set-face-attribute 'variable-pitch nil :font "Input Sans Compressed-14" :weight 'medium)
(add-hook 'text-mode-hook #'variable-pitch-mode)
(add-hook 'prog-mode-hook #'variable-pitch-mode)
(add-hook 'protobuf-mode-hook #'variable-pitch-mode)

;; I don't like the borders when I hover over the modeline. It cuts off the
;; text.
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)

;; From doom emacs. The native window divider takes a pixel out of the fringe.
(setq window-divider-default-places t)
(setq window-divider-default-bottom-width 1)
(setq window-divider-default-right-width 1)
(window-divider-mode)

;; Emacs normally disables these 'advanced' commands.
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Let there be silence...
(setq ring-bell-function 'ignore)
(setq visible-bell nil)

;; Use the command key as meta. This is a life saver. I don't know how I used
;; Emacs before.
(setq mac-command-modifier 'meta)

;; These commands adjust completion to just how I like it.
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq completion-ignore-case t)
(setq tab-always-indent 'complete)

;; Check out pulse.
;; (defun pulse-line (&rest _)
;;       "Pulse the current line."
;;       (pulse-momentary-highlight-one-line (point)))

;; (dolist (command '(scroll-up-command scroll-down-command
;;                    recenter-top-bottom other-window))
;;   (advice-add command :after #'pulse-line))

;; (defun setup-term-font ()
;;   '(term ((t (:background "#292b2e" :foreground "#b2b2b2" :family "InputCustomMonoCompressed")))))

;; From https://github.com/purcell/emacs.d/blob/master/lisp/init-windows.el
;; When I open a new split I want it to show `other-buffer' instead of
;; `current-buffer'.
(defmacro codesuki--with-other-buffer (split-function)
  `(defun ,(intern (concat "codesuki--with-other-buffer-" (symbol-name split-function))) (arg)
     "Split this window and switch to the new window unless ARG is provided."
	  (interactive "P")
	  (funcall ',split-function)
	  (let ((target-window (next-window)))
	    (set-window-buffer target-window (other-buffer))
	    (unless arg
	      (select-window target-window)))))

(keymap-global-set "C-x 2" (codesuki--with-other-buffer split-window-below))
(keymap-global-set "C-x 3" (codesuki--with-other-buffer split-window-horizontally))

;; I like using `C-w' to delete (not kill) words backwards. But with programming
;; languages I want it to respect braces. E.g.
;;
;; func something() bool {
;;     int|
;; }
;;
;; With | being the cursor. If I press `C-w' twice I end up here
;;
;; func something()|
;;
;; when I actually just want it to delete the whitespace. Yes, I am also to lazy
;; to press `M-SPC'. TODO: consider not doing this in comments because we might
;; want to delete whitespace.
(defun codesuki-delete-syntax (arg)
  "Delete characters forward until encountering the end of a syntax element.
With argument ARG, do this that many times. Use `words' when
inside a comment."
  (interactive "p")
  (let* ((word-end (progn (save-excursion (forward-word arg) (point))))
	 (syntax-end (progn (save-excursion (forward-same-syntax arg) (point))))
	 (faces (progn
		  (let ((maybe-list (get-text-property word-end 'face)))
		    (if (listp maybe-list)
			maybe-list
		      (list maybe-list))))))
    ;; Leave this in to find other faces.
    (message "%s %s" word-end faces)
    (cond ((memq 'font-lock-comment-face faces)
	   (delete-region (point) word-end))
	  ((> word-end syntax-end)
	   (delete-region (point) word-end))
	  (t
	   (delete-region (point) syntax-end)))))

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

(keymap-set prog-mode-map "C-w" 'kill-region-or-backward-delete-syntax)

;; `C-S-<backspace>' is quite hard to press. I want to try to use `C-u C-k'
;; instead. Numerical arguments still work to delete whole lines.
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
(keymap-global-set "C-S-k" 'kill-line)

;; Sometimes I just want to open a line above the current one. It's possible to
;; do this manually via `C-a' followed by `C-o'.
(defun codesuki-open-line-above (arg)
  (interactive "p")
  (save-excursion
    (beginning-of-line)
    (open-line arg)))

(keymap-global-set "C-S-o" 'codesuki-open-line-above)

;; Instead of pressing `M-m' I prefer to re-use `C-a' to switch between
;; `beginning-of-line' and `back-to-indentation'.
(defun move-beginning-of-line-or-indent ()
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))

(keymap-global-set "C-a" 'move-beginning-of-line-or-indent)

;; I don't like the default behavior of `pop-global-mark' to open the buffer in
;; the current window, if the buffer is actually visible in a window already.
(require 'cl-lib)
(defun codesuki-pop-global-mark ()
  (interactive)
  (cl-letf (((symbol-function 'switch-to-buffer) #'pop-to-buffer))
    (pop-global-mark)))

(global-set-key [remap pop-global-mark] 'codesuki-pop-global-mark)

;; From https://oremacs.com/2014/12/23/upcase-word-you-silly/
;; Normally the case functions behave quite weirdly. They change case starting
;; from the cursor location. Changes case functions to start from the beginning
;; of a word instead of from point.
(defun codesuki--case-advice ()
  (unless (or (looking-back "\\b")
	      (bound-and-true-p subword-mode))
    (backward-word)))

;; (advice-add 'capitalize-word :before 'codesuki--case-advice)
;; (advice-add 'upcase-word :before 'codesuki--case-advice)
;; (advice-add 'downcase-word :before 'codesuki--case-advice)

;; (defadvice capitalize-word (before capitalize-word-advice activate)
;;   (unless (or (looking-back "\\b")
;;	      (bound-and-true-p subword-mode))
;;     (backward-word)))

;; (defadvice upcase-word (before upcase-word-advice activate)
;;   (unless (looking-back "\\b")
;;     (backward-word)))

;; (defadvice downcase-word (before downcase-word-advice activate)
;;   (unless (looking-back "\\b")
;;     (backward-word)))

(defun codesuki-recompile-all ()
  "Recompile all packages."
  (byte-recompile-directory package-user-dir nil 'force))

;; Inspired by Doom Emacs. I want a clever `C-g' for popup handling.
(defvar codesuki--quit-hook nil)

(defun codesuki-quit ()
  "`codesuki-quit' executes `codesuki--quit-hook' until one hook
returns non-nil. If all hooks return nil it executes
`keyboard-quit'."
  (interactive)
  ;; TODO: also close minibuffer.
  (unless (run-hook-with-args-until-success 'codesuki--quit-hook)
    (keyboard-quit)))

;; We remap keyboard-quit instead of `C-g', because `project-switch-project'
;; directly compares the key-map entry to `keyboard-quit'. If it returns
;; `codesuki-quit' it will not cancel the command.
(global-set-key [remap keyboard-quit] 'codesuki-quit)

;; Popup handling. This mostly just leverages `display-buffer-alist'. Using the
;; above `codesuki-quit' I want to be able to close one popup at a time.
;; TODO:
;; - Allow for some buffers to ignore the close command.
;; - The first buffer in a popup decides whether it has a modeline.
;; -- This should be per buffer.
(defvar codesuki--popups nil)

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
			     ("\\*compilation\\*"
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
			     ("\\*prettier errors\\*"
			      (codesuki--display-buffer-in-side-window))
			     ("\\*Flymake diagnostics .*\\*"
			      (codesuki--display-buffer-in-side-window))
			     ("\\*vterm\\*"
			      (codesuki--display-buffer-in-side-window)
			      (window-parameters (mode-line-format . none)))))

;; The default project package of emacs has enough features for me.
;; Commands are bound to `C-x p'.
(use-package project
  :demand t)

;; When using use-package with built-in packages it will pull newer version from
;; elpa.
;; Supports code folding:
;; 'outline-minor-mode' is supported in Xref buffers.
;; You can enable outlining by adding 'outline-minor-mode' to
;;'xref-after-update-hook'.
(use-package xref
  :config
  ;; The choice is between grep and ripgrep. I prefer the latter.
  (setq xref-search-program 'ripgrep))

(use-package eldoc
  :diminish eldoc-mode
  :config
  ;; I want docs.
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))

;;(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Compiles lisp files on load/save.
(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;; `electric' does things like indenting after inserting a new line or inserting
;; matching parentheses.
(use-package electric
  :config
  ;; This prevents the following scenario (| is the point): |defun -> (|)defun
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  (electric-indent-mode)
  (electric-pair-mode))

;; Show my matching parentheses.
(use-package paren
  :config
  ;; Interesting, but should set the border to distinguish. Check docs.
  ;; This shows the opening parenthesis in a little window if it's off-screen.
  (setq show-paren-context-when-offscreen 'child-frame)
  (setq show-paren-delay 0.1)
  (setq show-paren-when-point-inside-paren t)
  (show-paren-mode))

;; Better rectangle editing. This conflicts with org-mode `C-RET'.
;; (use-package cua-base
;;   :config
;;   (cua-selection-mode t))

;; Delete the selection when typing something. Same as most other editors.
(use-package delsel
  :config
  (delete-selection-mode))

;; If a file changes on disk reload it in Emacs.
(use-package autorevert)

(use-package simple
  :straight (:type built-in)
  :init
  (setq set-mark-command-repeat-pop t))

(use-package repeat
  :config
  ;; This mode makes many things easier. E.g. to make a window larger you have
  ;; to repeatedly press `C-x C-}'. With `repeat-mode' from the second time
  ;; `C-}' is enough. (describe-repeat-maps) to see all possibilities.
  (repeat-mode))

(use-package mwheel
  :straight (:type built-in)
  :config
  (setq hscroll-margin 2)
  (setq hscroll-step 1)
  ;; I want it to scroll 1 line at a time.
  (setq scroll-margin 0)
  (setq scroll-preserve-screen-position 'always)
  ;; Don't recenter when point moves out of screen
  (setq scroll-conservatively 101)
  (setq mouse-wheel-progressive-speed nil)
  ;; Pressing shift while scrolling will scroll horizontally.
  (setq mouse-wheel-scroll-amount '(1 ((shift) . hscroll))))

(use-package dired-x
  :straight (:type built-in))

;; `ibuffer' is a nicer buffer list.
(use-package ibuffer
  :config
  (defalias 'list-buffers 'ibuffer))

;; Make recently used files available when switching buffers.
(use-package recentf
  :config
  (recentf-mode))

;; This cleans up trailing whitespace before saving.
(use-package whitespace
  :config
  (setq whitespace-line-column 80)
  (add-hook 'before-save-hook 'whitespace-cleanup))

(use-package gnutls)

;; With this you can go back to previous window layouts via `C-c <left>'
(use-package winner
  :config
  (winner-mode))

(use-package epa-file
  :straight (:type built-in)
  :config
  (epa-file-enable)
  (setq epa-file-select-keys nil))

;; Otherwise those PATH variables are not available in GUI Emacs.
(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-variables '("GOPATH" "PATH" "MANPATH" "JAVA_HOME"))
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

;; I use hunspell because it supports multiple dictionaries.
(use-package ispell
  :config
  (setenv "LANG" "en_US.UTF-8")
  (setq ispell-dictionary "en_US-large,de_DE_frami")
  (setq ispell-program-name "hunspell")
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US-large,de_DE_frami")
  (add-hook 'org-mode-hook 'flyspell-mode))

;; This does spell checking on the fly.
(use-package flyspell
  :defer t
  :config
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil))

;; Shows linter errors in the fringe.
(use-package flymake
  :config
  ;; This removes the mouse overlay.
  (put 'eglot-note 'flymake-overlay-control nil)
  (put 'eglot-warning 'flymake-overlay-control nil)
  (put 'eglot-error 'flymake-overlay-control nil)
  ;; This fixes some error message on startup.
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)
  (flymake-mode))

;; Saves minibuffer command history.
(use-package savehist
  :init
  (savehist-mode))

;; Saves the last location in a buffer. Uexpectedly useful.
(use-package saveplace
  :init
  (save-place-mode))

;; I tend to use `dabbrev-expand' a lot, this way I get corfu's help.
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
	 ("C-M-/" . dabbrev-expand))
  :config
  (setq dabbrev-case-replace nil))

;; The built-in or soon-to-be built-in LSP client. Simple and good.
(use-package eglot
  :after project
  :defer t
  :init
  :config
  (set-face-attribute 'eglot-highlight-symbol-face nil :inherit 'error)
  (fset #'jsonrpc--log-event #'ignore)
  (setf (plist-get eglot-events-buffer-config :size) 0)
  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c o") 'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c h") 'eldoc)
  (define-key eglot-mode-map (kbd "C-c i") 'eglot-find-implementation)
  (setq-default eglot-workspace-configuration
		'(:gopls (
			  :formatting.gofumpt t
			  :completion.usePlaceholders t
			  :diagnostic.vulncheck "Imports"
			  :diagnostic.staticcheck t
			  :build.directoryFilters ["-bazel-bin" "-bazel-out" "-bazel-testlogs" "-bazel-mercari-feature-flags"]))))

(defun codesuki--isearch-exit-other-end (&optional nopush edit)
	  "Exit current search on the other end of the match."
	  (interactive)
	  (funcall #'isearch-done nopush edit)
	  (when isearch-other-end (goto-char isearch-other-end)))

(use-package isearch
  :straight (:type built-in)
  :bind (:map isearch-mode-map
	      ("C-<return>" . codesuki--isearch-exit-other-end)))

;; Depends on https://github.com/casouri/tree-sitter-module
(use-package treesit
  :straight (:type built-in)
  :config
  (setq treesit-extra-load-path '("~/Development/tree-sitter-module/dist")))

;; Nicer display of function/macro/variable description. Actually shows the code
;; and callers.
(use-package helpful
  :defer t
  :config
  (global-set-key [remap describe-function] #'helpful-callable)
  (global-set-key [remap describe-command] #'helpful-command)
  (global-set-key [remap describe-variable] #'helpful-variable)
  (global-set-key [remap describe-key] #'helpful-key)
  (global-set-key [remap describe-symbol] #'helpful-symbol))

;; I use variable pitch fonts but some things like tables need to be fixed width.
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

;; For my org-agenda.
(use-package japanese-holidays
  :config
  (setq calendar-holidays (append holiday-christian-holidays japanese-holidays)))

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
;; this lets org-agenda pop-up on the right side instead of on the bottom when
;; using reorganize-frame.
(advice-add 'org-agenda-prepare-window :around 'codesuki--org-vertically-split-follow-mode-window)

;; "https://towardsdatascience.com/the-experimentation-gap-3f5d374d354c"
;; TODO:
;; - feed to pandoc
;; - download images, replace links
(defun codesuki-url-to-org (url)
  (let* ((buffer (url-retrieve-synchronously url))
	 (html (with-current-buffer buffer
		 (buffer-substring url-http-end-of-headers (point-max))))
	 (dom
	  (with-temp-buffer
	    (insert html)
	    (decode-coding-region (point-min) (point-max) 'utf-8)
	    (libxml-parse-html-region (point-min) (point-max)))))
    (eww-score-readability dom)
    (with-temp-buffer
      (shr-dom-print (eww-highest-readability dom))
      (buffer-string))))

(use-package org
  :config
  ;; In `org-mode' I prefer longer lines.
  (add-hook 'org-mode-hook (lambda () (setq fill-column 120)))
  (add-hook 'org-indent-mode-hook (lambda () (setup-org-faces)))
  ;; `org-indent-mode' hides the stars on the left side which is quite noisy
  ;; with many indent levels.
  (setq org-startup-indented t)
  ;; I want to see the actual file when going through my agenda.
  (setq org-agenda-start-with-follow-mode t)
  ;; This renders things like _this_ already underlined. But it's actually super
  ;; confusing for things like A_Something because Something would turn into a
  ;; subscript.
  (setq org-pretty-entities nil)
  ;; This just so makes the frame setup the way I want it.
  (setq org-agenda-window-setup 'reorganize-frame)
  ;; if we use `reorganize-frame` then using the below will cause all other
  ;; windows to be deleted
  ;;(setq org-agenda-window-frame-fractions '(0.5 . 1.0))
  ;; As the name says, `org-agenda' changes the window setup. Please restore it
  ;; when I am finished.
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
  (setq org-log-into-drawer t)
  ;; When using variable pitch fonts the tags are not aligned. Alternatively
  ;; make headings fixed-width.
  (setq org-tags-column 0)
  (setq org-capture-templates
	`(("i" "Inbox" entry  (file "inbox.org")
	   ,(concat "* TODO %^{What}\n"
		    ":PROPERTIES:\n"
		    ":CREATED: %U\n"
		    ":END:\n"
		    "%?"))
	  ;; For meetings.
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
	  ;; For `org-protocol', to save links from the browser.
	  ("b"
	   "bookmark"
	   entry
	   (file "inbox.org")
	   "* %:description\nSource: %:link\n%i\n"
	   :immediate-finish t)
	  ("j" "Journal" entry  (file+olp+datetree "journal.org")
	   ,(concat "* %^{What}\n"
		    "%?"))))
  ;; Those are recommended and I like them.
  (keymap-global-set "C-c c" #'org-capture)
  (keymap-global-set "C-c a" #'org-agenda)
  ;; (org-babel-do-load-languages 'org-babel-load-languages '((C . t)))
  )

;; This is not done, yet. I was thinking to capture quotes from websites under
;; the same heading (name of website).
(defun codesuki--org-capture-to-website ()
  (message "%s" (org-capture-get :annotation))
  (message "%s" org-capture-current-plist)
  (message "%s" org-capture-plist)
  (message "%s" org-store-link-plist)
  (org-capture-put :target (list 'file+headline (nth 1 (org-capture-get :target)) (org-capture-get :annotation)))
  (org-capture-set-target-location))

;; For my Zettelkasten. Everything here is the default from the readme.
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

;; This allows for viewing `elfeed' entries in a split window. Maybe move this
;; to display-buffer-alist for consistency
(defun codesuki--elfeed-display-buffer (buffer &optional action)
  (pop-to-buffer buffer '((display-buffer-below-selected) (dedicated 'elfeed) (window-height 100))))

(use-package elfeed
  :config
  (setq elfeed-show-entry-switch 'codesuki--elfeed-display-buffer)
  (setq elfeed-db-directory (to-local-path "elfeed")))

;; To be able to manage feeds via org-files.
(use-package elfeed-org
  :config
  (setq rmh-elfeed-org-files '("~/notes/feeds.org"))
  (elfeed-org))

(use-package deft
  :config
  (setq deft-directory "~/notes"
	deft-recursive t
	deft-extensions '("org")))

;; This optimizes the garbage collector.
(use-package gcmh
  :config
  (setq gcmh-idle-delay 'auto)
  (setq gcmh-auto-idle-delay-factor 10)
  (gcmh-mode))

;; A create fuzzy search package.
(use-package orderless
  :config
  (setq completion-styles '(orderless)))

(defun corfu-enable-in-minibuffer ()
  "Enable Corfu in the minibuffer if `completion-at-point' is bound."
  (when (where-is-internal #'completion-at-point (list (current-local-map)))
    ;; (setq-local corfu-auto nil) Enable/disable auto completion
    (corfu-mode 1)))

;; Shows completion candidates in a small popup.
(use-package corfu
  :init
  (setq corfu-preselect-first nil)
  (global-corfu-mode)
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer))

;; Minibuffer completion using `completion-at-point'. Simple and good.
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

;; Shows useful information for minibuffer entries. E.g. extra information for
;; buffers.
(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  ;; The :init configuration is always executed (Not lazy!)
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

;; Enhances default commands like `yank-pop' to show list of candidates in the
;; minibuffer.
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
	 ("M-g i" . consult-imenu)
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

;; These two functions let me choose the window before opening a file or buffer.
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

;; This is a great package. It's like a context sentitive right click menu.
(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim))        ;; good alternative: M-.
  ;;("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
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

;; This package let's me edit grep buffers, so I can grep for a word in files
;; and then edit them all at once.
(use-package wgrep)

;; Shows you all unbound keys.
(use-package unbound)

;; Show possible keys to press and their commands.
;; Note: I am now using embark-prefix-help-command instead.
;; (use-package which-key
;;   :defer 2
;;   :config
;;   (which-key-mode)
;;   (setq which-key-use-C-h-commands nil))

;; The best terminal support for Emacs.
(use-package vterm)

;; `goto-last-change', quite useful.
(use-package goto-chg
  :config
  (keymap-global-set "C-\\" 'goto-last-change))

;; Eagerly deletes whitespace.
(use-package hungry-delete
  :diminish hungry-delete-mode
  :config
  (setq-default hungry-delete-chars-to-skip " \t\f\v")
  (add-hook 'minibuffer-setup-hook (lambda () (hungry-delete-mode -1)))
  (global-hungry-delete-mode))

;; Move lines of text up/down with `M-up', `M-down'.
(use-package move-text
  :config
  (move-text-default-bindings))

;; This allows me to use `go-guru-expand-region' together with `expand-region'.
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
  (er/enable-mode-expansions 'go-mode 'er/add-go-mode-expansions))

;; As the name says. Quite useful.
(use-package multiple-cursors
  :bind (("C-;" . mc/mark-all-dwim)
	 ("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)))

;; Jump where you look at. The real value comes from the dispatch options. Press
;; ? to see them.
(use-package avy
  ;; Default for M-i is `tab-to-tab-stop', which is useless. Alternatively use
  ;; C-i which is the same as TAB. Another good choice would be C-j for jump. It
  ;; is `electric-newline-and-maybe-indent'. Turns out using C-j instead of RET
  ;; might be easier because it's on the home row.
  :bind (("M-i" . avy-goto-char-timer))
  :config
  (setq avy-background t)
  (avy-setup-default))

;; Jump to windows via home row keys.
(use-package ace-window
  :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package ace-link
  :config
  (ace-link-setup-default))

;; Super useful for lisp. Make every parenthesis pair a different color.
(use-package rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

;; Shows how many isearch matches there are in the modeline. Also has useful
;; visualizations for `query-replace'.
(use-package anzu
  :config
  (setq anzu-cons-mode-line-p nil)
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  (global-anzu-mode))

(use-package nerd-icons
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

;; The best theme pack.
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (setq doom-themes-padded-modeline t)
  (load-theme 'doom-vibrant t)
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; Can be slow, but I like it the best. Maybe I'll find the time to build a
;; simple modeline I like.
(use-package doom-modeline
  :config
  (setq inhibit-compacting-font-caches t)
  (setq nerd-icons-scale-factor 1.0)
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

;; The fringe bitmaps from Doom Emacs.
(define-fringe-bitmap 'codesuki--vc-fringe-bmp-insert [#b11100000] nil nil '(center repeated))
(define-fringe-bitmap 'codesuki--vc-fringe-bmp-change [#b11100000] nil nil '(center repeated))
(define-fringe-bitmap 'codesuki--vc-fringe-bmp-delete
  [#b10000000
   #b11000000
   #b11100000
   #b11110000] nil nil 'bottom)

(defun codesuki--fringe-bmp-from-type (type pos)
  (intern (format "codesuki--vc-fringe-bmp-%s" type)))

;; Shows git status in the fringe.
(use-package diff-hl
  :config
  (set-face-attribute 'diff-hl-insert nil :background nil)
  (set-face-attribute 'diff-hl-delete nil :background nil)
  (set-face-attribute 'diff-hl-change nil :background nil)
  (setq-default fringes-outside-margins t)
  (setq diff-hl-fringe-bmp-function 'codesuki--fringe-bmp-from-type)
  (global-diff-hl-mode))

;; Very useful when editing yaml.
(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'column)
  (add-hook 'yaml-mode-hook 'highlight-indent-guides-mode))

;; A directory tree view on the left side. Can also show other things. Actually
;; I never use it.
(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
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
     (treemacs-git-mode 'simple)))
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

;; Show old versions of a file.
(use-package git-timemachine
  :defer t)

;; This has awesome functions for editing list but I've never used them.
(use-package smartparens)

;; Editorconfig. Not using it anymore.
(use-package editorconfig
  :defer t
  :config
  (editorconfig-mode))

;; For sending HTTP requests. Can write them in a buffer and save it.
(use-package restclient
  :defer t)

;; Very nice Git UI although I mainly use the commandline.
(use-package magit
  :defer t)

(use-package forge
  :after magit)

;; Show HTML colors codes in that specific color.
(use-package rainbow-mode
  :diminish rainbow-mode
  :config
  (rainbow-mode))

;; I've rarely used snippets so far, but they seem useful.
(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode))

;; Collection of great snippets.
(use-package doom-snippets
  :after yasnippet
  :straight (doom-snippets :type git :host github :repo "hlissner/doom-snippets" :files ("*.el" "*")))

;; Keeps track of which commands are used the most.
(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(defun codesuki--organize-imports-and-format ()
  ;; check if organize imports is available. or just ignore error.
  (condition-case nil
      (eglot-code-action-organize-imports (point-min))
    (error nil))
  (eglot-format-buffer))

(defun codesuki--configure-gopls-env ()
  (make-local-variable 'process-environment)
  (add-to-list 'process-environment "GOPACKAGESDRIVER=./tools/gopackagesdriver.sh")
  )

;; Go support.
(use-package go-ts-mode
  :config
  ;; `subword-mode' is great for CamelCase.
  (add-hook 'go-ts-mode-hook #'subword-mode)
  ;; I want LSP.
  (add-hook 'go-ts-mode-hook #'eglot-ensure)
  (add-hook 'go-ts-mode-hook #'codesuki--configure-gopls-env)
  ;; Format before saving
  (add-hook 'go-ts-mode-hook #'(lambda () (add-hook 'before-save-hook #'codesuki--organize-imports-and-format -10 t))))

;; (use-package go-mode
;;   :defer t
;;   :config
;;   ;; `subword-mode' is great for CamelCase.
;;   (add-hook 'go-mode-hook #'subword-mode)
;;   ;; I want LSP.
;;   (add-hook 'go-mode-hook #'eglot-ensure)
;;   ;; Use goimports to format the file. It also fixes up imports.
;;   (setq gofmt-command "goimports")
;;   (add-hook 'go-mode-hook #'(lambda () (add-hook 'before-save-hook #'codesuki--organize-imports-and-format nil t)))
;;   (add-hook 'go-mode-hook #'codesuki--configure-gopls-env))

;; ;; Provides useful things like jump to definition, list callers, etc. I wonder
;; ;; when this will be superseeded by LSP.
;; (use-package go-guru)

;; ;; Rarely using this. Adds tags to structs.
;; (use-package go-tag
;;   :defer t
;;   :config
;;   (with-eval-after-load 'go-mode
;;     (define-key go-mode-map (kbd "C-c t") #'go-add-tags)))

;; Bazel support.
(use-package emacs-bazel-mode
  :defer t
  :straight  '(bazel :type git :host github :repo "bazelbuild/emacs-bazel-mode")
  :commands bazel-mode
  :custom
  (bazel-buildifier-before-save t "Run buildifier before saving."))


;; Dockerfile support.
(use-package dockerfile-mode
  :defer t)

;; Assembler support.
(use-package nasm-mode
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.s\\'" . nasm-mode)))

;; Protobuf support.
(use-package protobuf-mode
  :defer t)

;; Yaml support.
(use-package yaml-mode
  :defer t)

;; (use-package json-mode
;;   :defer t)

(use-package jsonnet-mode
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.libsonnet\\'" . jsonnet-mode)))

(use-package cue-mode
   :straight  '(cue-mode :type git :host github :repo "russell/cue-mode"))

;; Many projects use this. So I am keeping it.
(use-package google-c-style
  :straight (google-c-style :branch "master")
  :defer t
  :config
  (progn
    (add-hook 'c-mode-hook 'google-set-c-style)
    (add-hook 'c-mode-hook 'google-make-newline-indent)
    (add-hook 'c++-mode-hook 'google-set-c-style)
    (add-hook 'c++-mode-hook 'google-make-newline-indent)))

;; JS support is built-in.
(use-package js
  :defer t
  :config
  (setq js-indent-level 2))

;; (defun set-jsx-indentation ()
;;   (setq-local sgml-basic-offset js2-basic-offset))

;; ;; This mode was a bit better than `js-mode', but I am not sure this is still
;; ;; correct.
;; (use-package js2-mode
;;   :defer t
;;   :config
;;   (progn
;;     (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;;     (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))
;;     (add-hook 'js2-jsx-mode-hook #'set-jsx-indentation)
;;     (setq js2-mode-show-strict-warnings nil)
;;     (setq js2-mode-show-parse-errors nil)))

;; Allows to use HTML, CSS, JS in the same file.
;; (use-package web-mode
;;   :defer t
;;   ;; :mode ("\\.html?\\'" . web-mode)
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode)))

;; My package to have binaries in `node_modules' available.
(use-package add-node-modules-path
  :config
  (eval-after-load 'js2-mode
    '(add-hook 'js2-mode-hook #'add-node-modules-path))
  (eval-after-load 'web-mode
    '(add-hook 'web-mode-hook #'add-node-modules-path))
  (eval-after-load 'typescript-mode
    '(add-hook 'typescript-mode-hook #'add-node-modules-path))
  (eval-after-load 'typescript-ts-mode
    '(add-hook 'typescript-mode-hook #'add-node-modules-path))
  (eval-after-load 'tsx-ts-mode
    '(add-hook 'typescript-mode-hook #'add-node-modules-path)))

;; Typescript support.
;; (use-package typescript-mode
;;   :init
;;   ;; This is to support `tree-sitter'. Why do I want `tree-sitter'? Because
;;   ;; `typescript-mode' cannot properly highlight TSX.
;;   (define-derived-mode typescript-tsx-mode typescript-mode "tsx")
;;   :config
;;   (setq typescript-indent-level 2)
;;   (add-hook 'typescript-mode-hook #'subword-mode)
;;   (add-hook 'typescript-mode-hook #'eglot-ensure)
;;   (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-tsx-mode)))

;; ;; This adds indent support to typescript/css/json via tree sitter.
;; (use-package tsi
;;   :straight '(tsi :type git :host github :repo "orzechowskid/tsi.el")
;;   :init
;;   (add-hook 'typescript-mode-hook #'tsi-typescript-mode))

;; I want to replace `expand-region' with this.
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

;; Provides an AST for languages. Mainly used for better syntax highlighting,
;; but has promise for more.
;; (use-package tree-sitter
;;   :init
;;   (global-tree-sitter-mode)
;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; (use-package tree-sitter-langs
;;   :after tree-sitter
;;   :config
;;   (tree-sitter-require 'tsx)
;;   (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx)))

(use-package typescript-ts-mode
  :config
  (add-hook 'typescript-ts-mode-hook #'eglot-ensure)
  (add-hook 'tsx-ts-mode-hook #'eglot-ensure)
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
		 '((tsx-ts-mode typescript-ts-mode) . ("typescript-language-server" "--stdio")))))


;; The popular JS formatting thing.
(use-package prettier
  :defer t
  :hook ((typescript-ts-mode . prettier-mode)
	 (web-mode . prettier-mode)))

;; This is a mode to quickly write HTML. I forgot how it works, but I like it.
(use-package emmet-mode
  :defer t
  :config
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode))

;; Support for markdown.
(use-package markdown-mode
  :defer t)

;; Terraform support.
(use-package terraform-mode
  :defer t
  :config
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))

(use-package groovy-mode
  :defer t)

;; Swift support.
(use-package swift-mode
  :defer t)

;; For the nand2tetris course.
(use-package nand2tetris
  :disabled t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.hdl\\'" . nand2tetris-mode))
  (setq nand2tetris-core-base-dir "~/Development/nand2tetris"))

(load custom-file 'noerror 'nomessage)

;;; init.el ends here
