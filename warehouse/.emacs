;;; .emacs for Willi Ballenthin <williballenthin.com>
;;; Things to learn and remember:
;;;   - personal key map: C-2
;;;   - workgroup mode: C-3

(defun set-env-var-from-shell (var)
  "Set up Emacs' environment variable to match that used by the user's shell."
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n\-]*$" "" (shell-command-to-string (concat "$SHELL --login -i -c 'echo $" var "'")))))
    (setenv var path-from-shell)))


(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

  This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n\-]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)
(set-env-var-from-shell "GOPATH")

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(el-get 'sync)






(menu-bar-mode 0)
(tool-bar-mode 0)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(wg-switch-on-load nil)
 '(workgroups-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )



(add-to-list 'load-path "~/.emacs.d/")
;(add-to-list 'load-path "/opt/slime/")



(require 'color-theme-solarized)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-solarized-dark)
;     (color-theme-tty-dark)
     ))






;; A bunch of stuff for Lisp hacking
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



(add-to-list 'load-path "/home/willi/hg/go/misc/emacs/" t)
(require 'go-mode-load)



(require 'moonscript-mode)



;; Flymake mode for syntax checking
(require 'go-flymake)
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
(add-hook 'go-mode-hook 'flymake-mode)



;; Autocomplete minor mode
(add-to-list 'load-path "~/.emacs.d/autocomplete-mode/")
(require 'go-autocomplete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/autocomplete-mode//ac-dict")
(ac-config-default)


(setq jedi:setup-keys t)
(autoload 'jedi:setup "jedi" "Jedi Autocomplete" t)
(add-hook 'python-mode-hook 'jedi:setup)




;; Lua stuff
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(require 'flymake-lua)
(add-hook 'lua-mode-hook 'flymake-lua-load)

(eval-after-load "auto-complete" '(add-to-list 'ac-modes 'lua-mode))







;; Autocomplete for C code via Clang
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



;; Java stuff
(add-hook 'java-mode-hook 'flymake-mode-on)
(defun my-java-flymake-init ()
  (list "javac" (list (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-with-folder-structure))))
(add-to-list 'flymake-allowed-file-name-masks '("\\.java$" my-java-flymake-init flymake-simple-cleanup))


;; Python display
(add-hook 'python-mode-hook       (lambda () (linum-mode)))
(add-hook 'c-mode-common-hook     (lambda () (linum-mode)))





;; Workgroup minor mode
(require 'workgroups)
(setq wg-prefix-key (kbd "C-3"))
(workgroups-mode 1)
(wg-load "~/.emacs.d/workgroups")




;; IDO mode
(require 'ido)
(ido-mode t)



;; Highlighter minor mode
(require 'highlighter-minor-mode)


;; spaces, not tabs!
(require 'whitespace)
(autoload 'global-whitespace-mode           "whitespace" "Toggle whitespace visualization."        t)
(add-hook 'after-change-major-mode-hook 
          '(lambda () 
	     (setq-default indent-tabs-mode nil)
	     (setq c-basic-indent 2)
	     (setq tab-width 2)
	     (setq indent-tabs-mode nil)))

;; Custom code by Willi, for Willi
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

(defun replace-all-occurances (word replacement)
  "replace-all-occurances replaces all occurances of `word` 
   with `replacement` in the current buffer."
  (save-excursion
    (beginning-of-buffer)
    (replace-string word replacement)))

(defun replace-current-word (replacement)
  "replace-current-word replaces all occurances of the 
   current word with `replacement`"
  (interactive "sReplacement:")
  (replace-all-occurances (current-word) replacement))

;; Personal keymap
(define-prefix-command 'willi-keymap)
(global-set-key (kbd "C-2") 'willi-keymap)
(define-key willi-keymap (kbd "C-*") 'duplicate-line)
(define-key willi-keymap (kbd "C-p") 'prettify-buffer)
(define-key willi-keymap (kbd "C-w") 'clean-whitespace-buffer)
(define-key willi-keymap (kbd "C-c") 'compile)
(define-key willi-keymap (kbd "C-r") 'replace-current-word)


(require 'willi-local)
;(set-face-attribute 'default nil :height 140)
