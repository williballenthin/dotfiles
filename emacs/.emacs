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


;;;;;;;;;;;;;;;;   ORG-MODE   ;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-capture-templates
    (quote (
        ("t" "Todo" entry
            (file+datetree "~/oh/todo.org")
              "* TODO %^{Thing} %^g
:PROPERTIES:
:ADDED: %U
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
SCHEDULED: %(org-insert-time-stamp (org-read-date nil t))")

        ("c" "Quick capture" entry
            (file+headline "~/oh/todo.org" "Inbox")
             "* %^{Thing} %^g
:PROPERTIES:
:ADDED: %U
:END:")

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
; `go get -u github.com/nsf/gocode`
(require 'go-mode)  ; install via packages.el

(require 'go-eldoc)  ; install via packages.el
(add-hook 'go-mode-hook 'go-eldoc-setup)

; `go get -u github.com/dougm/goflymake`
(add-to-list 'load-path (concat (getenv "GOPATH") "/src/github.com/dougm/goflymake"))
(require 'go-flymake)  ; install via packages.el

(setenv "PATH" (concat (getenv "PATH") ":" (concat (getenv "GOPATH") "~/code/go/bin/")))

;;;;;;;;;;;;;;;;   PYTHON STUFF   ;;;;;;;;;;;;;;;;;;;;;;;;;
; `sudo pip install pylint`
(require 'flycheck)  ; install via packages.el
(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'jedi)
; then do: M-x jedi:install-server
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
