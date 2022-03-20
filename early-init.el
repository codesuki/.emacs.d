(startup-redirect-eln-cache ".local/eln-cache/")
(setq package-enable-at-startup nil)

;; From: doom emacs
;; https://github.com/hlissner/doom-emacs/blob/4a6de2419c81d120ce363a2ba189789c7a2424d4/core/core-ui.el#L265
;; Disable all UI elements.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq menu-bar-mode nil)
(setq tool-bar-mode nil)
(setq scroll-bar-mode nil)

;; Let's start in something that can house 2 windows side by side.
(push '(width . 220) default-frame-alist)
(push '(height . 60) default-frame-alist)
