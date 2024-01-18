;; some configuration derived from:
;; https://arne.me/articles/emacs-from-scratch-part-one-foundations

(tool-bar-mode -1)             ;; Hide the outdated icons
(scroll-bar-mode -1)           ;; Hide the always-visible scrollbar
(setq inhibit-splash-screen t) ;; Remove the "Welcome to GNU Emacs" splash screen
(setq use-file-dialog nil)      ;; Ask for textual confirmation instead of GUI

;; via https://github.com/radian-software/straight.el
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

;; use-package for tidier specification and better performance
(straight-use-package 'use-package)

;; make use-package use straight.el by default 
(setq straight-use-package-by-default t)
;; always :defer t for lazy loading
(setq use-package-always-defer t)

(use-package gcmh
  :demand
  :config
  (gcmh-mode 1))

;; local configuration
(use-package emacs
  :init
  ;; ---------------------------------------------------------------------
  ;; startup
  ;; remove default scratch message/help text
  (setq initial-scratch-message nil)
  (defun display-startup-echo-area-message ()
    (message ""))
  ;; ---------------------------------------------------------------------
  ;; content
  (progn
    ;; make everything utf-8
    (set-charset-priority 'unicode)
    (setq locale-coding-system 'utf-8
            coding-system-for-read 'utf-8
            coding-system-for-write 'utf-8)
    (set-terminal-coding-system 'utf-8)
    (set-keyboard-coding-system 'utf-8)
    (set-selection-coding-system 'utf-8)
    (prefer-coding-system 'utf-8)
    (setq default-process-coding-system '(utf-8-unix . utf-8-unix))
    ;; use spaces by default
    (setq-default indent-tabs-mode nil)
    (setq-default tab-width 2))
  ;; ---------------------------------------------------------------------
  ;; input (keyboard and  mice)
  (progn
    ;; correct keybindings on macOS
    (when (eq system-type 'darwin)
        (setq mac-command-modifier 'super)
        (setq mac-option-modifier 'meta)
        (setq mac-control-modifier 'control))
    ;; clipboard on wayland
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
                    nil ;; should return nil if we're the current paste owner
                    (shell-command-to-string "wl-paste -n | tr -d \\r")))
            (setq interprogram-cut-function 'wl-copy)
            (setq interprogram-paste-function 'wl-paste)))
    ;; escape should exit menus
    (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
    ;; enable y/n instead of only yes/no
    (defalias 'yes-or-no-p 'y-or-n-p)
    ;; enable the mouse under the terminal
    (when (not (display-graphic-p))
        (xterm-mouse-mode)))
  ;; ---------------------------------------------------------------------
  ;; window management
  (progn
    ;; show open buffers in tabs.
    ;; note, this is different from tab-bar-mode,
    ;; which puts frame layouts in their own tab
    (tab-line-mode 1)

    ;; better window handling defaults: use current window.
    ;; via: https://github.com/nex3/perspective-el/blob/c8c3383/README.md#some-musings-on-emacs-window-layouts
    ;;
    ;; tell display-buffer to reuse existing windows as much as possible, including in other frames.
    (setq display-buffer-base-action
        '((display-buffer-reuse-window display-buffer-same-window)
          (reusable-frames . t)))
    ;; prevent splits by telling display-buffer to switch to the target buffer in the current window.
    (setq even-window-sizes nil))
  ;; ---------------------------------------------------------------------
  ;; file handling
  (progn
    ;; don't create backup files
    (setq make-backup-files nil)
    ;; remember recent files for convenience.
    ;; see also: keybinding: <leader>fr
    (recentf-mode 1)
    (setq recentf-max-menu-items 25)
    (setq recentf-max-saved-items 25))
  ;; ---------------------------------------------------------------------
  ;; theming
  (progn
    (require-theme 'modus-themes)
    (setq modus-themes-italic-constructs t
          modus-themes-bold-constructs t)
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme 'modus-operandi :no-confirm)
    (when (display-graphic-p)
      (progn
        ;; this has to come after modus theme loading, for some reason.
        (set-face-attribute 'default nil :family "Iosevka")
        (set-face-attribute 'variable-pitch nil :family "Iosevka Aile")))
    ;; mark active window modeline
    ;; via: https://irreal.org/blog/?p=11874
    (set-face-attribute 'mode-line nil
        :background "#bebebe" :box '(:line-width 1 :color "black"))
    ;; relative line numbers in prog mode
    (defun ab/enable-line-numbers ()
        "Enable relative line numbers"
        (interactive)
        (display-line-numbers-mode)
        (setq display-line-numbers 'relative))
    (add-hook 'prog-mode-hook #'ab/enable-line-numbers)
    ;; org-modern
    (setq
        ;; Edit settings
        org-auto-align-tags nil
        org-tags-column 0
        org-catch-invisible-edits 'show-and-error
        org-special-ctrl-a/e t
        org-insert-heading-respect-content t

        ;; Org styling, hide markup etc.
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-ellipsis "…"

        ;; Agenda styling
        org-agenda-tags-column 0
        org-agenda-block-separator ?─
        org-agenda-time-grid
        '((daily today require-timed)
        (800 1000 1200 1400 1600 1800 2000)
        " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-current-time-string
        "◀── now ─────────────────────────────────────────────────"))
  ;; ssh is faster than default scp for small files
  (setq tramp-default-method "ssh")

  :bind
  (("M-x" . counsel-M-x)))

(use-package evil
  :demand ;; No lazy loading
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1))

(use-package which-key
  :demand
  :init
  ;; Open after .5s instead of 1s
  (setq which-key-idle-delay 0.5)
  :config
  (which-key-mode))

;; for counsel-M-x
(use-package counsel
  :init (counsel-mode))

;; for remembering recently used commands
(use-package smex)

(use-package general
  :demand
  :config
  (general-evil-setup)

  (general-define-key
    :states '(normal emacs)
    "J" 'other-window
    "K" '(lambda () (interactive) (other-window -1)))

  (general-create-definer leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (leader-keys
    "x" '(counsel-M-x :which-key "execute command")
    "r" '((lambda () (interactive) (restart-emacs)) :which-key "restart emacs")
    "i" '((lambda () (interactive) (find-file "~/.dotfiles/emacs/init.el")) :which-key "open init file")

    ;; Buffer
    "b" '(:ignore t :which-key "buffer")
    "bd"  'kill-current-buffer
    "bl" '(persp-counsel-switch-buffer :which-key "list buffers")
    "f"  '(:ignore t :which-key "file system")
    "ff" 'find-file
    "fs" 'dirvish
    "fq" 'dirvish-quit
    "fr" 'counsel-recentf
    ;; buffer/window/tab management
    "a" '(:ignore t :which-key "window")
    "a-" 'split-window-below
    "a|" 'split-window-right

    "aj" '((lambda () (interactive) (other-window 1)) :which-key "next window")
    "ak" '((lambda () (interactive) (other-window -1)) :which-key "prev window")
    "awd" 'delete-window
    "abd" 'kill-buffer
  ))
 
(use-package projectile
  :demand
  :general
  (leader-keys
    :states 'normal
    "SPC" '(projectile-find-file :which-key "find file")
    ;; notable keybindings
    ;;
    ;;   b - projectile-switch-to-buffer
    ;;   p - projectile-switch-project
    ;;       projectile-add-known-project
    "p" '(:keymap projectile-command-map :which-key "projectile"))
  :init
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy))

;; use evil keybindings:
;;   <leader> ap  - "perspective"
;;   o            - "open" perspective
;;   s            - "save"
;;   d            - "close"
;;   l            - list, but use H/L or atj/atk for this
(use-package perspective
  :bind
  ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
  :init
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))  ; pick your own prefix key here
  :init
  (persp-mode)
  (general-define-key
    :states '(normal emacs)
    ";" 'persp-counsel-switch-buffer

    "L" 'persp-next
    "H" 'persp-prev)

  (leader-keys
    "bl" 'persp-counsel-switch-buffer
    "apo" 'persp-switch  ; switch or open
    "aps" 'persp-state-save
    "apd" 'persp-kill
    "apn" 'persp-rename
    "apl" 'persp-counsel-switch-buffer)
  (setq persp-state-default-file "~/.local/state/emacs/perspective.el")
  (add-hook 'kill-emacs-hook #'persp-state-save)
  ;; restore perspective upon reload
  (persp-state-restore persp-state-default-file))

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
  :config
  (global-diff-hl-mode))

(use-package vterm
  :init
  ;; we're using a light theme globally in emacs
  ;; so bat should use a compatible theme for its text output.
  (setq vterm-environment '("BAT_THEME=Coldark-Cold"))
  ;; don't confirm killing vterm subprocess on exit/restart
  (setq confirm-kill-processes nil))

(use-package vterm-toggle
  :general
  (leader-keys
    "'" '(vterm-toggle :which-key "terminal")))

(use-package multi-vterm
  :ensure t)

(use-package evil-nerd-commenter
  :general
  (leader-keys
    "gc" 'evilnc-comment-operator))

(use-package dirvish
  :init
  ;; clipboard on wayland
  (if (display-graphic-p)
    (setq dirvish-attributes
        ;'(vc-state subtree-state collapse all-the-icons git-msg file-time file-size))
        '(vc-state subtree-state collapse git-msg file-time file-size))
    (setq dirvish-attributes
          '(vc-state subtree-state collapse git-msg file-time file-size)))
  (dirvish-override-dired-mode)
  :config

  (leader-keys
    "f" '(:ignore t :which-key "file system")
    "fd" 'dirvish-fd
  ))

(use-package org-modern
  :init
  (with-eval-after-load 'org (global-org-modern-mode)))

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  (setq doom-modeline-icon (display-graphic-p)))

(when (display-graphic-p)
  ;; setup: M-x nerd-icons-install-fonts
  (use-package nerd-icons))

(use-package symbol-overlay)

(use-package company
  :hook
  (prog-mode . company-mode)

  :bind
  (:map company-active-map
    ("<tab>" . company-complete-selection))

  :custom
  (company-minimum-prefix-length 3)
  (company-idle-delay 0.1))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda () (require 'lsp-pyright))))

(use-package lsp-mode
  :after
  lsp-pyright
  :config
  (general-def 'normal lsp-mode :definer 'minor-mode
      "SPC l" lsp-command-map)
  :hook ((prog-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :custom
  (help-at-pt-timer-delay 0.9)
  (help-at-pt-display-when-idle '(flymake-overlay)))

(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package tree-sitter
  :ensure t
  :config
  ;; activate tree-sitter on any buffer containing code for which it has a parser available
  (global-tree-sitter-mode)
  ;; you can easily see the difference tree-sitter-hl-mode makes for python, ts or tsx
  ;; by switching on and off
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter;
  :config
  (tree-sitter-langs-install-grammars))
