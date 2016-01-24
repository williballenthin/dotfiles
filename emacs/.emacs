; package-install color-theme
; package-install jedi
; package-install highlight-symbol
; package-install highlight-parenthesis
; package-install go-mode
; package-install go-eldoc
; package-install go-autocomplete
; package-install flymake-go
; package-install flycheck
; package-install evil
; package-install ac-python
; package-install magit
; package-install evil-magit
; package-install hs-minor-mode 
;
;; clojure stuff
; package-install slime
; package-install cider
; package-install clojure-mode
; package-install paredit


;;;;;;;;;;;;;;;;   PACKAGES   ;;;;;;;;;;;;;;;;;;;;;;;;;
; load package manager
(require 'package)
  (push '("marmalade" . "http://marmalade-repo.org/packages/")
                package-archives )
  (push '("melpa" . "http://melpa.milkbox.net/packages/")
                package-archives)

; the packages that you install with package.el are activated
;   by default after your .emacs is loaded
; via: http://stackoverflow.com/questions/11127109/emacs-24-package-system-initialization-problems/11140619#11140619
(setq package-enable-at-startup nil)
(package-initialize)

;;;;;;;;;;;;;;;;   KEYBINDINGS   ;;;;;;;;;;;;;;;;;;;;;;;;;

;; vim keybindings
(require 'evil)
(evil-mode 1)

;; swap buffers using willi's favorite VIM mapping
(define-key evil-normal-state-map "J" 'next-buffer)
(define-key evil-normal-state-map "K" 'previous-buffer)
;; note, the opposite is `evil-insert-state-map`)
(define-key evil-normal-state-map ";" 'ido-switch-buffer)

;; highlight-symbol mode
(global-set-key [(control f3)] 'highlight-symbol)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)


;; invoke org-capture
(global-set-key (kbd "<f6>") 'org-capture)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;;;;;;;;;;;;;;;;   UI   ;;;;;;;;;;;;;;;;;;;;;;;;;

;; remove some chrome
(menu-bar-mode 0)
(tool-bar-mode 0)

;; show line numbers
(add-hook 'python-mode-hook       (lambda () (linum-mode)))
(add-hook 'go-mode-hook       (lambda () (linum-mode)))
(add-hook 'c-mode-common-hook     (lambda () (linum-mode)))

;; highlight the symbol under the cursor
(require 'highlight-symbol)

;; highlight matching parentheses
(require 'highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

;; IDO mode for interactively doing things with buffers
(require 'ido)
(ido-mode t)

;; i'd like to use git-gutter, but it doesn't work with flychecker,
;;   which also uses the fringe/gutter buffer
;(require 'git-gutter-fringe+)

;; spaces, not tabs!
(require 'whitespace)
(autoload 'global-whitespace-mode "whitespace" "Toggle whitespace visualization." t)
(add-hook 'after-change-major-mode-hook
          '(lambda ()
         (setq-default indent-tabs-mode nil)
         (setq c-basic-indent 2)
         (setq tab-width 2)
         (setq indent-tabs-mode nil)))


;; themeing stuff
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("6a9606327ecca6e772fba6ef46137d129e6d1888dcfc65d0b9b27a7a00a4af20" default)))
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(load-theme 'zenburn t)

;;;;;;;;;;;;;;;;   GIT        ;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'evil-magit)
(global-set-key (kbd "C-x g") 'magit-status)

;;;;;;;;;;;;;;;;   ORG-MODE   ;;;;;;;;;;;;;;;;;;;;;;;;;
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
(setq org-outline-path-complete-in-steps t)


;;;;;;;;;;;;;;;;   GOLANG STUFF   ;;;;;;;;;;;;;;;;;;;;;;;;;
(setenv "GOPATH" "/home/willi/go")
(setenv "PATH" (concat (getenv "PATH") ":" (concat (getenv "GOPATH") "/bin")))
;; exec-path is like PATH for emacs
(setq exec-path (append exec-path (list (concat (getenv "GOPATH") "/bin"))))

; `go get -u github.com/nsf/gocode`
(require 'go-mode)  ; install via packages.el

(require 'go-eldoc)  ; install via packages.el
(add-hook 'go-mode-hook 'go-eldoc-setup)

; `go get -u github.com/dougm/goflymake`
(add-to-list 'load-path (concat (getenv "GOPATH") "/src/github.com/dougm/goflymake"))
;(require 'go-flymake)  ; install via packages.el
(add-hook 'before-save-hook 'gofmt-before-save)

;; via: https://github.com/nsf/gocode
(require 'auto-complete)
(require 'go-autocomplete)  ;; install via packages.el
(require 'auto-complete-config)
(ac-config-default)


(add-hook 'go-mode-hook
          (lambda ()
            (global-set-key (kbd "C-c s") 'hs-show-block)
            (global-set-key (kbd "C-c S") 'hs-show-all)
            (global-set-key (kbd "C-c h") 'hs-hide-block)
            (global-set-key (kbd "C-c H") 'hs-hide-all)
            (hs-minor-mode t)))

;;;;;;;;;;;;;;;;   PYTHON STUFF   ;;;;;;;;;;;;;;;;;;;;;;;;;
; `sudo pip install pylint`
(require 'flycheck)  ; install via packages.el
(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'jedi)
; then do: M-x jedi:install-server
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

; The value is in 1/10pt, so 100 will give you 10pt, etc. 
; via: http://stackoverflow.com/a/296316/87207
(set-face-attribute 'default nil :height 100)