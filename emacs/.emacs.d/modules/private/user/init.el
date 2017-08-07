;;; private/user/init.el -*- lexical-binding: t; -*-


(after! doom-themes
  ;; fedora: sudo dnf install mozilla-fira-mono-fonts
  ;; Since Fira Mono doesn't have an italicized variant, highlight it instead
  (set-face-attribute 'italic nil
                      ;; The value is in 1/10pt, so 100 will give you 10pt, etc.
                      ;; via: http://stackoverflow.com/a/296316/87207
                      :height 100
                      :weight 'ultra-light
                      :foreground "#ffffff"
                      :background (doom-color 'current-line)))


; the packages that you install with package.el are activated
;   by default after your .emacs is loaded
; via: http://stackoverflow.com/questions/11127109/emacs-24-package-system-initialization-problems/11140619#11140619
(package-initialize t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
    (package-install 'use-package))


(use-package evil-magit
  :ensure t
  :bind (("C-x i" . magit-status))
  :defer t)


(use-package highlight-parentheses
  :ensure t
  :config
  (progn
    (require 'highlight-parentheses)
    (define-globalized-minor-mode global-highlight-parentheses-mode
      highlight-parentheses-mode
      (lambda ()
        (highlight-parentheses-mode t)))
    (global-highlight-parentheses-mode t)))


(use-package whitespace
  :ensure t
  :config
  (progn
    (autoload 'global-whitespace-mode "whitespace" "Toggle whitespace visualization." t)
    (global-whitespace-mode t)))


(use-package clojure-mode
  :ensure t)


(use-package parinfer
  :ensure t
  :bind
  (("C-," . parinfer-toggle-mode))
  :config
  (progn
    (setq parinfer-extensions
          '(defaults       ; should be included.
             ;; rainbow-delimiters package no longer available?
             ;;pretty-parens  ; different paren styles for different modes.
             evil           ; If you use Evil.
             paredit        ; Introduce some paredit commands.
             smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
             smart-yank))   ; Yank behavior depend on mode.
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)))
