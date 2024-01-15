; some configuration derived from:
; https://arne.me/articles/emacs-from-scratch-part-one-foundations

(tool-bar-mode -1)             ; Hide the outdated icons
(scroll-bar-mode -1)           ; Hide the always-visible scrollbar
(setq inhibit-splash-screen t) ; Remove the "Welcome to GNU Emacs" splash screen
(setq use-file-dialog nil)      ; Ask for textual confirmation instead of GUI

; via https://github.com/radian-software/straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
    (expand-file-name
      "straight/repos/straight.el/bootstrap.el"
      (or (bound-and-true-p straight-base-dir)
        user-emacs-directory)))
    (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
       'silent 'inhibit-cookies)
    (goto-char (point-max))
    (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

; use-package for tidier specification and better performance
(straight-use-package 'use-package)

; make use-package use straight.el by default 
(setq straight-use-package-by-default t)
; always :defer t for lazy loading
(setq use-package-always-defer t)

(use-package gcmh
  :demand
  :config
  (gcmh-mode 1))

; local configuration
(use-package emacs
  :init
  ; remove default scratch message/help text
  (setq initial-scratch-message nil)
  (defun display-startup-echo-area-message ()
    (message ""))
  ; enable y/n instead of only yes/no
  (defalias 'yes-or-no-p 'y-or-n-p)
  ; make everything utf-8
  (set-charset-priority 'unicode)
  (setq locale-coding-system 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))
  ; use spaces by default
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  ; correct keybindings on macOS
  (when (eq system-type 'darwin)
    (setq mac-command-modifier 'super)
    (setq mac-option-modifier 'meta)
    (setq mac-control-modifier 'control))
  ; relative line numbers in prog mode
  (defun ab/enable-line-numbers ()
    "Enable relative line numbers"
    (interactive)
    (display-line-numbers-mode)
    (setq display-line-numbers 'relative))
  (add-hook 'prog-mode-hook #'ab/enable-line-numbers)
  ; clipboard on wayland
  (when (display-graphic-p)
    (progn
        (setq wl-copy-process nil)
        (defun wl-copy (text)
            (setq wl-copy-process (make-process :name "wl-copy"
                                                :buffer nil
                                                :command '("wl-copy" "-f" "-n")
                                                :connection-type 'pipe))
            (process-send-string wl-copy-process text)
            (process-send-eof wl-copy-process))
        (defun wl-paste ()
            (if (and wl-copy-process (process-live-p wl-copy-process))
                nil ; should return nil if we're the current paste owner
                (shell-command-to-string "wl-paste -n | tr -d \\r")))
        (setq interprogram-cut-function 'wl-copy)
        (setq interprogram-paste-function 'wl-paste)))
  ; escape should exit menus
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  ; enable the mouse under the terminal
  (when (not (display-graphic-p))
    (xterm-mouse-mode))
  ; mark active window modeline
  ; via: https://irreal.org/blog/?p=11874
  (set-face-attribute 'mode-line nil
    ; using doom-one base4 for bg color
    :background "#3f444a" :box '(:line-width 1 :color "black"))
  ; tab bar for window management
  ; hide the tab bar when it has only one tab, and show it again when more tabs are created.
  (tab-bar-mode)
  (setq tab-bar-show 1)
  ; don't create backup files
  (setq make-backup-files nil)

  :bind
  (("M-x" . counsel-M-x))
)

(use-package evil
  :demand ; No lazy loading
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1))

(use-package which-key
  :demand
  :init
  ; Open after .5s instead of 1s
  (setq which-key-idle-delay 0.5)
  :config
  (which-key-mode))

;; for consel-M-x
(use-package counsel)

;; for remembering recently used commands
(use-package smex)

(use-package general
  :demand
  :config
  (general-evil-setup)

  (general-define-key
    :states '(normal emacs)
    ";" 'counsel-switch-buffer

    "J" 'other-window
    "K" '(lambda () (interactive) (other-window -1))

    "L" 'tab-bar-switch-to-next-tab
    "H" 'tab-bar-switch-to-prev-tab)

  (general-create-definer leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (leader-keys
    "x" '(counsel-M-x :which-key "execute command")
    "r" '((lambda () (interactive) (restart-emacs)) :which-key "restart emacs")
    "i" '((lambda () (interactive) (find-file user-init-file)) :which-key "open init file")

    ;; Buffer
    "b" '(:ignore t :which-key "buffer")
    ;; Don't show an error because SPC b ESC is undefined, just abort
    "b <escape>" '(keyboard-escape-quit :which-key t)
    "bd"  'kill-current-buffer
    "bl" '(counsel-switch-buffer :which-key "list buffers")
     "f" '(:ignore t :which-key "file system")
    "f <escape>" '(keyboard-escape-quit :which-key t)
    "fs" 'dirvish
    ;; buffer/window/tab management
    "a" '(:ignore t :which-key "window")
    "a <escape>" '(keyboard-escape-quit :which-key t)
    "a-" 'split-window-below
    "a|" 'split-window-right

    "ag" 'switch-to-buffer-other-tab

    "aj" '((lambda () (interactive) (other-window 1)) :which-key "next window")
    "ak" '((lambda () (interactive) (other-window -1)) :which-key "prev window")

    "al" 'tab-bar-switch-to-next-tab
    "ah" 'tab-bar-switch-to-prev-tab

    "adb" 'kill-current-buffer
    "adw" 'delete-window

    "atj" 'tab-bar-switch-to-next-tab
    "atk" 'tab-bar-switch-to-prev-tab
  ))
 
(use-package projectile
  :demand
  :general
  (leader-keys
    :states 'normal
    "SPC" '(projectile-find-file :which-key "find file")

    ;; Buffers
    "b b" '(projectile-switch-to-buffer :which-key "switch buffer")

    ;; Projects
    "p" '(:ignore t :which-key "projects")
    "p <escape>" '(keyboard-escape-quit :which-key t)
    "p p" '(projectile-switch-project :which-key "switch project")
    "p a" '(projectile-add-known-project :which-key "add project")
    "p r" '(projectile-remove-known-project :which-key "remove project"))
  :init
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy))

(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode))

(use-package magit
  :general
  (leader-keys
    "g" '(:ignore t :which-key "git")
    "g <escape>" '(keyboard-escape-quit :which-key t)
    "g g" '(magit-status :which-key "status")
    "g l" '(magit-log :which-key "log"))
  (general-nmap
    "<escape>" #'transient-quit-one))

(use-package evil-collection
  :after evil
  :demand
  :config
  (evil-collection-init))

(use-package diff-hl
  :init
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode))

(use-package vterm)

(use-package vterm-toggle
  :general
  (leader-keys
    "'" '(vterm-toggle :which-key "terminal")))

(use-package evil-nerd-commenter
  :general
  (general-nvmap
    "gc" 'evilnc-comment-operator))

(use-package dirvish
  :init
  ; clipboard on wayland
  (if (display-graphic-p)
    (setq dirvish-attributes
        ;'(vc-state subtree-state collapse all-the-icons git-msg file-time file-size))
        '(vc-state subtree-state collapse git-msg file-time file-size))
    (setq dirvish-attributes
          '(vc-state subtree-state collapse git-msg file-time file-size)))
  :config
  (dirvish-override-dired-mode)

  (leader-keys
    "f" '(:ignore t :which-key "file system")
    "f <escape>" '(keyboard-escape-quit :which-key t)
    "fd" 'dirvish-fd
  ))

(use-package doom-themes
  :demand
  :config
  (load-theme 'doom-one t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

; setup: M-x nerd-icons-install-fonts
(use-package nerd-icons)

(use-package symbol-overlay)
