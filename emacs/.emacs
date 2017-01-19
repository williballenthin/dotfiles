(let ((default-directory  "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

;;;;;;;;;;;;;;;;   PACKAGES   ;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ; load package manager
(require 'package)
(setq package-enable-at-startup nil)
(push '("marmalade" . "https://marmalade-repo.org/packages/")
        package-archives )
(push '("melpa" . "http://melpa.milkbox.net/packages/")
        package-archives)
(push '("gnu" . "http://elpa.gnu.org/packages/")
        package-archives)

; the packages that you install with package.el are activated
;   by default after your .emacs is loaded
; via: http://stackoverflow.com/questions/11127109/emacs-24-package-system-initialization-problems/11140619#11140619
(package-initialize t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)


(use-package evil
  :ensure t
  :config
  (progn
    (evil-mode 1)
    ;; swap buffers using willi's favorite VIM mapping
    (define-key evil-normal-state-map "J" 'next-buffer)
    (define-key evil-normal-state-map "K" 'previous-buffer)
    ;; note, the opposite is `evil-insert-state-map`)
    (define-key evil-normal-state-map ";" 'ido-switch-buffer)))

(use-package evil-magit
  :ensure t
  :bind (("C-x i" . magit-status))
  :defer t)

(use-package highlight-symbol
  :ensure t
  :bind (("C-<f3>" . highlight-symbol)
         ("<f3>" . highlight-symbol-next)
         ("S-<f3>" . highlight-symbol-prev)
         ("M-<f3>" . highlight-symbol-query-replace))
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

(use-package ido
  :ensure t
  :config (ido-mode t))

(use-package whitespace
  :ensure t
  :config
  (autoload 'global-whitespace-mode "whitespace" "Toggle whitespace visualization." t)
  (add-hook 'after-change-major-mode-hook
            '(lambda ()
               (setq-default indent-tabs-mode nil)
               (setq c-basic-indent 2)
               (setq tab-width 2)
               (setq indent-tabs-mode nil)
               (setq show-trailing-whitespace t)))


(use-package zenburn-theme
  :ensure t
  :config (load-theme 'zenburn t))

(use-package clojure-mode
  :ensure t)

(use-package parinfer
  :ensure t
  :config
  (progn
    (setq parinfer-extensions
          '(defaults       ; should be included.
             pretty-parens  ; different paren styles for different modes.
             evil           ; If you use Evil.
             paredit        ; Introduce some paredit commands.
             smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
             smart-yank))   ; Yank behavior depend on mode.
    (dolist (hook '(clojure-mode-hook
                    emacs-lisp-mode-hook
                    common-lisp-mode-hook
                    scheme-mode-hook
                    lisp-mode-hook))
      (add-hook hook #'parinfer-mode))))

(use-package company
  :ensure t
  :config (global-company-mode))

(use-package org
  :ensure t
  :init
  (progn 
    (setq org-todo-keywords
          '((sequence "TODO(t)" "WORKING(w!)" "DONE(d!)")))

    (setq org-capture-templates
          (quote (
                  ("t" "Todo" entry
                   (file+datetree "~/oh/todo.org")
                   "* TODO %^{Thing} %^g
                    :PROPERTIES:
                    :ADDED: %U
                    :LOGGING: TODO(t!) WORKING(w!) DONE(d!)
                    :END:
                    DEADLINE: %(org-insert-time-stamp (org-read-date nil t))
                    %?")

                  ("i" "Time in" entry
                   (file+datetree "~/oh/todo.org")
                   "* %^{Activity} :TIME:%^g
                    :PROPERTIES:
                    :ADDED: %U
                    :BILLCODE: %^{billcode}
                    :END:"
                                        ; clock-in: start the clock
                                        ; clock-keep: don't stop the clock when commiting entry
                                        ; immediate-finish: don't prompt for finalization of entry
                   :clock-in t :clock-keep t :immediate-finish t)

                  ("e" "Event" entry
                   (file+datetree "~/oh/todo.org")
                   "* event: %^{Thing}
                    :PROPERTIES:
                    :ADDED: %U
                    :END:
                    date: %(org-insert-time-stamp (org-read-date nil t))")

                  ("c" "Quick capture" entry
                   (file+headline "~/oh/todo.org" "Inbox")
                   "* %^{Thing} %^g
                    :PROPERTIES:
                    :ADDED: %U
                    :END:")

                  ("p" "Expense" entry
                   (file+datetree "~/oh/todo.org")
                   "* TODO Expense: %^{expense} %^g
                    :PROPERTIES:
                    :ADDED: %U
                    :BILLCODE: %^{billcode}
                    :END:
                    total: %^{total}")

                  )))

    (setq org-agenda-files '("~/oh/todo.org"))

    ;; the following from: http://stackoverflow.com/a/25089985/87207
                                        ;(setq org-completion-use-ido t)
                                        ; because we use ido, no need to go in steps
                                        ;(setq org-outline-path-complete-in-steps t)
    (setq org-refile-targets '((org-agenda-files :maxlevel . 4)))
                                        ; nil: add entries at the end of lists
    (setq org-reverse-note-order nil)


    ;; iteratively select the tree path
    ;; the following from: http://stackoverflow.com/a/26682891/87207
                                        ; doc: Non-nil means provide refile targets as paths.
    (setq org-refile-use-outline-path t)
    (setq org-outline-path-complete-in-steps t)))

(use-package linum
  :ensure t
  :config
  (dolist (hook '(python-mode-hook
                  go-mode-hook
                  c-mode-hook
                  clojure-mode-hook))
    (add-hook hook #'linum-mode)))


;;;;;;;;;;;;;;;;   INTERFACE   ;;;;;;;;;;;;;;;;;;;;;;;;;

;; remove some chrome
(menu-bar-mode 0)
(tool-bar-mode 0)

; The value is in 1/10pt, so 100 will give you 10pt, etc.
; via: http://stackoverflow.com/a/296316/87207
(set-face-attribute 'default nil :height 100)


;(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
; '(package-selected-packages
;   (quote
;    (async bind-key dash diminish evil git-commit goto-chg magit magit-popup undo-tree with-editor zenburn-theme use-package parinfer highlight-symbol highlight-parentheses goto-last-change evil-magit))))
;(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (parinfer zenburn-theme highlight-parentheses evil-magit highlight-symbol evil-leader evil bind-key diminish use-package))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


