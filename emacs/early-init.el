; derived from:
; https://arne.me/articles/emacs-from-scratch-part-one-foundations

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

; we use straight.el, so disable package.el
(setq package-enable-at-startup nil)
