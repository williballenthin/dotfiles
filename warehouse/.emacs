(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(add-to-list 'load-path "~/.emacs.d/")
;(add-to-list 'load-path "/opt/slime/")

(load-file "~/.emacs.d/color-theme-solarized.el")
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-solarized-dark)))


(require 'clojure-mode)
;(require 'clojurescript-mode)
;(require 'clojure-test-mode)

;(add-hook 'clojure-mode-hook
;(lambda ()
;  (setq inferior-lisp-program "lein repl")))

;(require 'slime)
;(add-hook 'lisp-mode-hook          (lambda () (slime-mode t)))
;(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
;(setq inferior-lisp-program "/usr/local/bin/lein repl")
;(add-hook 'clojure-mode-hook       (lambda () (inferior-slime-mode t)))


(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'scheme-mode-hook           (lambda () (paredit-mode +1)))
(add-hook 'clojure-mode-hook          (lambda () (paredit-mode +1)))
;(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'clojure-mode-hook    'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook       'turn-on-eldoc-mode)

;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
;(defun override-slime-repl-bindings-with-paredit ()
;  (define-key slime-repl-mode-map
;    (read-kbd-macro paredit-backward-delete-key) nil))
;(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)


(load-file "~/.emacs.d/flymake-cursor.el")

(when (load "flymake" t)
  (defun flymake-pycheckers-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "/usr/local/bin/pycheckers" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks '("\\.py\\'" flymake-pycheckers-init)))

(add-hook 'python-mode-hook 'flymake-mode)


(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))


(require 'flymake-lua)
(add-hook 'lua-mode-hook 'flymake-lua-load)



(add-to-list 'load-path "~/.emacs.d/autocomplete-mode/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/autocomplete-mode//ac-dict")
(ac-config-default)




(add-to-list 'load-path "~/.emacs.d/auto-complete-clang-async/")
(require 'auto-complete-clang-async)
(defun ac-cc-mode-setup ()
  (setq ac-clang-complete-executable "~/.emacs.d/auto-complete-clang-async/clang-complete")
  (setq ac-sources '(ac-source-clang-async))
  (ac-clang-launch-completion-process))

(defun my-ac-config ()
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))

(my-ac-config)



(defun duplicate-line ()
  (interactive)
  (beginning-of-line)
  (kill-line)
  (yank)
  (insert "
")
  (yank)
  (message "Duplicated line."))


(defun prettify-buffer ()
  (interactive)
  (indent-for-tab-command)
  (message "Prettified buffer."))

(defun clean-whitespace-buffer ()
  "Untabifies, removes trailing whitespace"
  (interactive)
  (save-excursion
    (untabify (point-min) (point-max))
    (replace-regexp "[  ]+$" "" nil (point-min) (point-max))
    (delete-trailing-whitespace))
  (message "Cleaned region."))

;; Personal keymap
(define-prefix-command 'willi-keymap)
(global-set-key (kbd "C-2") 'willi-keymap)
(define-key willi-keymap (kbd "C-*") 'duplicate-line)
(define-key willi-keymap (kbd "C-p") 'prettify-buffer)
(define-key willi-keymap (kbd "C-w") 'clean-whitespace-buffer)
(define-key willi-keymap (kbd "C-c") 'compile)


