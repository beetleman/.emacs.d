;;; -*- lexical-binding: t; -*-
;;; init.el --- Mateusz's Emacs configuration
;;
;; Copyright (c) 2017-2021 Mateusz P.J.
;;
;; Author: Mateusz Probachta-Jeżowski <mateusz.probachta@gmail.com>
;; URL: https://github.com/beetleman/.emacs.d
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is my personal Emacs configuration
;; based on https://github.com/bbatsov/emacs.d/blob/master/init.el

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;; utils

(defmacro comment (&rest body))

;; turn off GC during star up
(setq gc-cons-threshold most-positive-fixnum)

(setq inhibit-compacting-font-caches t)

(setq user-full-name "Mateusz Jeżowski"
      user-mail-address "mateusz.probachta@gmail.com")


(setq read-process-output-max (* 1024 1024)) ; 1MB

;; Always load newest byte code
;; (setq load-prefer-newer t)

;; Optimization
(setq idle-update-delay 1.0)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Title
(setq frame-title-format '("🐐 - %b")
      icon-title-format frame-title-format)

;; fix MacOS
;; key bindings
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta))

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Suppress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-full-name
      inhibit-default-init t
      initial-scratch-message nil)
(menu-bar-mode -1)
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

;; and show this majestic creature!
(setq initial-scratch-message
      ";;
;;             ❤ Happy Hacking ❤
;;       _))
;;      > *\\     _~
;;      `;'\\\\__-' \\_
;;         | )  _ \\ \\
;;        / / ``   w w
;;       w w
") ;; ejm97


(setq window-resize-pixelwise t
      frame-resize-pixelwise t)

;; fix C-z
(global-unset-key (kbd "C-z"))

(global-set-key (kbd "C-z C-z") 'beetleman--suspend-frame)
(global-set-key (kbd "C-x C-z") 'beetleman--suspend-frame)

(defun beetleman--suspend-frame ()
  "In a GUI environment, do nothing; otherwise `suspend-frame'."
  (interactive)
  (if (display-graphic-p)
      (message "suspend-frame disabled for graphical displays.")
    (suspend-frame)))

;; always require final \n
(setq require-final-newline t)

;; enable y/n answers
(setq use-short-answers t)

;; disable tooltips
(tooltip-mode -1)

;; recursive minibufers
(setq enable-recursive-minibuffers t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Permanently indent with spaces, never with TABs
(setq-default c-basic-offset   4
              tab-width        4
              indent-tabs-mode nil)

;; setup font settings
(if (eq system-type 'darwin) ;; mac specific settings
    (add-to-list 'default-frame-alist '(font . "Aporetic Sans Mono-14"))
  (add-to-list 'default-frame-alist '(font . "Aporetic Sans Mono-11")))

(defun beetleman--set-emoji-font (frame)
  "Adjust the font settings of FRAME so Emacs can display: ✨🍆✨."
  (set-fontset-font t 'symbol "Noto Color Emoji" frame)
  (set-fontset-font t 'symbol "Symbola" frame 'append))


(beetleman--set-emoji-font nil)
(add-hook 'after-make-frame-functions 'beetleman--set-emoji-font)

;; setup ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; setup windows dividers
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)

;; setup emacs window managment options
;; https://www.masteringemacs.org/article/demystifying-emacs-window-manager
(setq switch-to-buffer-in-dedicated-window 'pop
      switch-to-buffer-obey-display-actions t)

;; my functions
(require 'ansi-color)
(defun beetleman--display-ansi-colors ()
  "Display colors in buffer which with color escaped."
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

;; SETUP PACKAGES
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; keep the installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)

;; upgrade buildin packages
(setopt package-install-upgrade-built-in t)

;; update the package metadata is the local cache is missing
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))


(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-expand-minimally t
      use-package-enable-imenu-support t)

(require 'use-package)


;;; BUILT-IN PACKAGES

(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode)
  :config
  (setopt auto-revert-avoid-polling t)
  (setopt auto-revert-interval 5)
  (setopt auto-revert-check-vc-info t))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode))

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(use-package recentf
  :ensure nil
  :init
  (recentf-mode 1))

(use-package server
  :ensure nil
  :defer 1
  :config
  (require 'server)
  (unless (server-running-p) (server-start)))

(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-alh")
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-dwim-target t)
  :config
  (setq delete-by-moving-to-trash t))

(use-package dired
  :ensure nil
  :if (string= system-type "darwin")
  :custom
  (insert-directory-program "/opt/homebrew/bin/gls"))

(use-package project
  :ensure nil
  :config
  (setq project-vc-extra-root-markers '(".project.el" "workspace.edn" "go.mod")))

(use-package flyspell
  :ensure nil
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :config
  (when (executable-find "aspell")
    (setq ispell-program-name "aspell")))

(use-package electric-pair
  :ensure nil
  :hook (after-init . electric-pair-mode))

(use-package hl-line
  :ensure nil
  :hook ((after-init . global-hl-line-mode)
         ((dashboard-mode eshell-mode shell-mode term-mode vterm-mode) .
          (lambda () (setq-local global-hl-line-mode nil)))))

(use-package winner
  :ensure nil
  :hook (after-init . winner-mode))


(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :init (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold))))

;;; THIRD-PARTY PACKAGES

(comment
 (use-package catppuccin-theme
   :demand t
   :custom-face
   (xref-match ((t (:inherit region))))
   :config
   (mapc #'disable-theme custom-known-themes)
   (setq catppuccin-flavor 'mocha) ;; or 'latte, 'macchiato, 'frappe or 'mocha
   (catppuccin-reload)))

(use-package modus-themes
  :demand t
  :config
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))

  (setq modus-themes-common-palette-overrides
        `((bg-paren-match bg-magenta-intense)  ;; make matched parens more visable
          (bg-mode-line-active bg-cyan-subtle) ;; highlight current buffer mode-line
          ,@modus-themes-preset-overrides-faint)) ;; use less distracting colors

  ;; Load the theme of your choice.
  (modus-themes-load-theme 'modus-vivendi-tinted)

  :bind ("<f5>" . #'modus-themes-toggle))

(use-package page-break-lines
  :hook (after-init . global-page-break-lines-mode))

(use-package meow
  :demand t
  :custom-face
  (meow-beacon-fake-selection ((t (:inherit region :underline t))))
  :config
  (setq meow-replace-state-name-list '((normal . "<N>")
                                       (motion . "<M>")
                                       (keypad . "<K>")
                                       (insert . "<I>")
                                       (beacon . "<B>"))
        meow-use-clipboard t)
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-define-key
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)
   '("u" . meow-universal-argument))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("P" . meow-yank-pop)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)
   ;; documentation
   '("?" . "C-c D"))
  (meow-global-mode 1)
  
  (use-package meow-tree-sitter
    :init
    (meow-tree-sitter-register-defaults)))

(use-package nerd-icons
  :config
  (add-to-list 'nerd-icons-extension-icon-alist
               '("tmpl" nerd-icons-devicon "nf-dev-html5" :face nerd-icons-lyellow))
  (add-to-list 'nerd-icons-extension-icon-alist
               '("asd" nerd-icons-sucicon "nf-custom-common_lisp" :face nerd-icons-lorange))
  (add-to-list 'nerd-icons-extension-icon-alist
               '("envrc" nerd-icons-faicon "nf-fa-cogs" :face nerd-icons-lred))
  (add-to-list 'nerd-icons-extension-icon-alist
               '("edn" nerd-icons-devicon "nf-dev-clojure" :face nerd-icons-purple)))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;; Group ibuffer's list by project
(use-package ibuffer-project
  :hook (ibuffer . (lambda ()
                     "Group ibuffer's list by project."
                     (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
                     (unless (eq ibuffer-sorting-mode 'project-file-relative)
                       (ibuffer-do-sort-by-project-file-relative))))
  :init (setq ibuffer-project-use-cache t))

;; treesiter
(use-package treesit-auto
  :commands (global-treesit-auto-mode)
  :hook (after-init . global-treesit-auto-mode)
  :custom
  (treesit-auto-install 'prompt)
  (treesit-auto-langs '(python
                        java
                        rust
                        go
                        gomod
                        nix
                        yaml
                        js
                        typescript
                        bash
                        tsx
                        markdown
                        css
                        html
                        sql)))

;; Environment
(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize)
  ;; CLI tools installed by Mise
  (let ((shims (expand-file-name"~/.local/share/mise/shims")))
    (setenv "PATH" (concat shims
                           ":"
                           (getenv "PATH")))
    (setq exec-path `(,shims ,@exec-path))))

(use-package mode-line-bell
  :hook (after-init . mode-line-bell-mode))

(use-package ws-butler
  :pin nongnu
  :custom
  (ws-butler-keep-whitespace-before-point nil)
  (ws-butler-convert-leading-tabs-or-spaces t)
  :hook ((prog-mode . ws-butler-mode)
	     (markdown-mode . ws-butler-mode)
	     (org-mode . ws-butler-mode)))

(use-package so-long
  :hook (after-init . global-so-long-mode))

;; for emacs profiling
(use-package esup
  :commands (esup)
  :config
  (setq esup-depth 0))

(use-package fancy-compilation
  :commands (fancy-compilation-mode)
  :custom
  (fancy-compilation-override-colors nil)
  (fancy-compilation-quiet-prelude nil)
  :init
  (with-eval-after-load 'compile
    (fancy-compilation-mode)))

(use-package treemacs
  :commands (treemacs-follow-mode
             treemacs-filewatch-mode
             treemacs-git-mode)
  :custom
  (treemacs-read-string-input 'from-minibuffer)
  (treemacs-git-mode 'deferred)
  (treemacs-collapse-dirs 7)
  (treemacs-deferred-git-apply-delay 1)
  (treemacs-move-files-by-mouse-dragging nil)
  :custom-face
  (cfrs-border-color ((t (:inherit posframe-border))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("<f9>"      . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)

  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))

  (use-package treemacs-nerd-icons
    :demand t
    :custom-face
    (treemacs-nerd-icons-root-face ((t (:inherit nerd-icons-dsilver :height 1.3))))
    (treemacs-nerd-icons-file-face ((t (:inherit nerd-icons-dsilver))))
    :config (treemacs-load-theme "nerd-icons"))

  (use-package treemacs-magit
    :demand t))

(use-package nerd-icons-dired
  :custom-face
  (nerd-icons-dired-dir-face ((t (:inherit nerd-icons-dsilver :foreground unspecified))))
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package dired-quick-sort
  :after dired
  :init
  (dired-quick-sort-setup))

(use-package trashed
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package ace-window
  :hook (after-init . ace-window-posframe-mode)
  :bind (("M-o" . ace-window)))

(use-package which-key
  :config
  (which-key-mode +1))

(use-package vundo
  :bind (("C-c v" . vundo)))

(use-package git-modes)

(use-package magit
  :custom
  (magit-log-margin '(t "%D" magit-log-margin-width t 18))
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-diff-refine-hunk t)
  :bind
  ("C-x g" . magit-status))

(with-eval-after-load 'project
  (define-key project-prefix-map "m" #'magit-project-status)
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t))

(use-package diff-hl
  :custom
  (diff-hl-draw-borders nil)
  :hook ((after-init . global-diff-hl-mode)
         (after-init . global-diff-hl-show-hunk-mouse-mode)
         (dired-mode . diff-hl-dired-mode))
  :config
  ;; (diff-hl-flydiff-mode 1) ; to slow
  (setq-default fringes-outside-margins t)
  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-component-separator #'orderless-escapable-split-on-space))

;; Enable vertico

(use-package vertico
  :hook (after-init . vertico-mode))

(use-package nerd-icons-completion
  :hook (vertico-mode . nerd-icons-completion-mode))

(use-package marginalia
  :hook (after-init . marginalia-mode))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil ;; part of vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-repeat
  :after vertico
  :ensure nil ;; part of vertico
  :bind ("<f6>" . vertico-repeat)
  :hook (minibuffer-setup . vertico-repeat-save))

(use-package super-save
  :init
  (super-save-mode +1))

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c i" . consult-imenu)
         ;; ("C-c k" . consult-kmacro) ; conflict with meow
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("C-s" . consult-line)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  )


(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         :map minibuffer-local-map
         ("C-c C-c" . embark-collect)
         ("C-c C-e" . embark-export)))

;; The `embark-consult' package is glue code to tie together `embark'
;; and `consult'.
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
  :bind (:map grep-mode-map
              ("e" . wgrep-change-to-wgrep-mode)
              ("C-x C-q" . wgrep-change-to-wgrep-mode)
              ("C-c C-c" . wgrep-finish-edit)))

(use-package puni
  :hook (prog-mode . puni-mode)
  :bind (("C-<right>" . puni-slurp-forward)
         ("C-<left>" . puni-barf-forward)
         ("C-M-<left>" . puni-slurp-backward)
         ("C-M-<right>" . puni-barf-backward)))

(use-package eldoc
  :hook (prog-mode . eldoc-mode)
  :bind (("C-c d" . eldoc))
  :config
  (use-package eldoc-box
    :custom
    (eldoc-box-lighter nil)
    (eldoc-box-only-multi-line t)
    (eldoc-box-clear-with-C-g t)
    :bind (("C-c D" . eldoc-box-help-at-point))))

(use-package string-inflection
  :bind ("C-c f" . string-inflection-all-cycle))

(use-package subword
  :hook (prog-mode . subword-mode))

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode))

(use-package yasnippet-snippets)

(use-package yasnippet-capf
  :after cape
  :init (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(use-package hl-todo
  :hook ((after-init . global-hl-todo-mode))
  :custom-face
  (hl-todo ((t (:inherit default :height 0.9 :width condensed :weight bold :underline nil :inverse-video t))))
  :init (setq hl-todo-require-punctuation t
              hl-todo-highlight-punctuation ":"))

(use-package anzu
  :bind
  (([remap query-replace] . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp))
  :hook
  (after-init . global-anzu-mode))

(use-package envrc
  :if (executable-find "direnv")
  :hook (after-init . envrc-global-mode)
  :bind (:map envrc-mode-map
              ("C-c d" . envrc-command-map)))

;; fish configuration:
;; if [ "$INSIDE_EMACS" = vterm ]; and [ -n $EMACS_VTERM_PATH ]; and [ -f $EMACS_VTERM_PATH/etc/emacs-vterm-bash.sh ]
;;     source "$EMACS_VTERM_PATH/etc/emacs-vterm.fish"
;;     alias ee="emacsclient -n"
;;     alias dired="emacsclient --eval '(dired-jump)'"
;; end

(use-package vterm
  :custom
  (vterm-ignore-blink-cursor nil)
  (vterm-copy-exclude-prompt t)
  (vterm-always-compile-modul t)
  (vterm-max-scrollback 100000)
  :preface
  (defun beetleman--vterm--delayed-redraw (&rest args)
    (meow--update-cursor))
  :config
  (advice-add 'vterm--delayed-redraw :after #'beetleman--vterm--delayed-redraw))

(use-package multi-vterm
  :commands (multi-vterm-project multi-vterm multi-vterm-next multi-vterm-prev)
  :config
  (with-eval-after-load 'project
    (add-to-list 'project-kill-buffer-conditions  '(major-mode . vterm-mode)))
  :custom
  (multi-vterm-buffer-name "vterm")
  :bind (([remap project-shell] . multi-vterm-project)
         ("C-c t t" . multi-vterm-project)
         ("C-c t T" . multi-vterm)
         ("C-c t n" . multi-vterm-next)
         ("C-c t p" . multi-vterm-prev)))

(use-package editorconfig
  :init
  (editorconfig-mode 1))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-preview-current nil)
  (corfu-auto-delay 0.2)
  (corfu-popupinfo-delay '(0.4 . 0.2))
  (corfu-cycle t)
  (corfu-on-exact-match nil)
  (corfu-preselect 'valid)
  :custom-face
  (corfu-border ((t (:inherit region :background unspecified))))
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :hook ((after-init . global-corfu-mode)
         (global-corfu-mode . corfu-popupinfo-mode))
  :config
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)

  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

;; A few more useful configurations...
(use-package emacs
  :init
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete))

(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))

;; Org

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;; Languages


(use-package csv-mode
  :hook ((csv-mode . csv-align-mode)
         (csv-mode . csv-guess-set-separator)))

(use-package yaml-mode)

(use-package dockerfile-mode)
(use-package php-mode)
(use-package nix-mode)
(use-package kotlin-mode)

;; groovy / gradle

(use-package groovy-mode
  :mode (("\\.gradle?$" . groovy-mode)
         ("\\.groovy?$" . groovy-mode)))

;; OCaml

(defun shell-cmd (cmd)
  "Returns the stdout output of a shell command or nil if the command returned
     an error"
  (car (ignore-errors (apply 'process-lines (split-string cmd)))))

(setq opam-p (shell-cmd "which opam"))

(if opam-p
    (dolist (var (car (read-from-string (shell-command-to-string "opam config env --sexp"))))
      (setenv (car var) (cadr var))))

(use-package caml)

(use-package tuareg
  :mode ("\\.ml[ily]?$" . tuareg-mode))

(use-package merlin
  :custom
  (merlin-completion-with-doc t)
  :bind (:map merlin-mode-map
              ("M-." . merlin-locate)
              ("M-," . merlin-pop-stack)
              ("M-?" . merlin-occurrences)
              ("C-c C-j" . merlin-jump)
              ("C-c i" . merlin-locate-ident)
              ("C-c C-e" . merlin-iedit-occurrences))
  :hook ((tuareg-mode caml-mode) . merlin-mode))

(use-package utop
  :custom
  (utop-edit-command nil)
  :hook
  (tuareg-mode . (lambda ()
                   (setq utop-command "utop -emacs")
                   (utop-minor-mode))))


;; Setup rust

(use-package rust-mode
  :mode (("\\.rs$" . rust-mode)))

;; Setup Golang

(use-package go-mode
  :config
  (use-package go-dlv)
  (use-package go-fill-struct)
  (use-package go-impl)

  (use-package go-tag
    :bind (:map go-mode-map
                ("C-c c a" . go-tag-add)
                ("C-c c r" . go-tag-remove))
    :init (setq go-tag-args (list "-transform" "camelcase")))

  (use-package go-gen-test
    :bind (:map go-mode-map
                ("C-c c g" . go-gen-test-dwim)))

  (use-package gotest
    :bind (:map go-mode-map
                ("C-c c f" . go-test-current-file)
                ("C-c c t" . go-test-current-test)
                ("C-c c j" . go-test-current-project)
                ("C-c c b" . go-test-current-benchmark)
                ("C-c c c" . go-test-current-coverage)
                ("C-c c x" . go-run))))

;; Setup clojure

(use-package clojure-mode
  :mode (("\\.clj$" . clojure-mode)
         ("\\.cljs$" . clojurescript-mode)
         ("\\.cljc$" . clojurec-mode)
         ("\\.edn$" . clojure-mode)
         ("\\.bb$" . clojure-mode)))


(use-package clj-refactor)

(defun portal.api/open ()
  (interactive)
  (cider-nrepl-sync-request:eval
   "(do
(ns dev)
(def portal ((requiring-resolve 'portal.api/open)
{:theme :portal.colors/nord-light
                            :name (-> (System/getProperty \"user.dir\")
                                      (clojure.string/split #\"/\")
                                      last)}))
(add-tap (requiring-resolve 'portal.api/submit))
)"))

(defun portal.api/clear ()
  (interactive)
  (cider-nrepl-sync-request:eval "(portal.api/clear)"))


(use-package cider
  :hook ((cider-mode . clj-refactor-mode)
         (cider-mode . eldoc-mode))
  :bind (:map clojure-mode-map
              ("<f8>" . #'portal.api/open)
              ("<f7>" . #'portal.api/clear))
  :custom
  (cider-repl-display-help-banner nil)
  (cider-repl-result-prefix ";; => ")
  (clojure-toplevel-inside-comment-form t)
  (cider-repl-history-size 1000)
  (cider-repl-history-file ".cider-repl-history")
  (cider-repl-buffer-size-limit nil)
  (cider-enrich-classpath t)
  :config
  (cider-auto-test-mode 1))

(use-package cider-eval-sexp-fu)


;; Cucumber and Gherkin Syntax
(use-package feature-mode
  :mode ("\\.feature\\'" . feature-mode))

;; Common Lisp
(use-package sly
  :config
  (sly-symbol-completion-mode -1)
  (setq sly-net-coding-system 'utf-8-unix
        sly-complete-symbol-function 'sly-flex-completions ;;'sly-simple-completions
        sly-lisp-implementations '((qlot ("qlot" "exec" "sbcl") :coding-system utf-8-unix)
                                   (sbcl ("sbcl") :coding-system utf-8-unix))))

;; Setup Caddyfile

(use-package caddyfile-mode
  :mode (("Caddyfile\\'" . caddyfile-mode)
         ("caddy\\.conf\\'" . caddyfile-mode)))


;; Setup HCL

(use-package hcl-mode
  :mode ("\\.nomad\\'" . hcl-mode))


;; setup teraform

(use-package terraform-mode
  :mode ("\\.tf\\'" . terraform-mode))


;; Setup markdown

(use-package markdown-mode)


;; setup webmode

(use-package web-mode
  :mode (("\\.vue\\'" . web-mode)
         ("\\.html\\'" . web-mode)
         ("\\.htm\\'" . web-mode)
         ("\\.tmpl\\'" . web-mode)
         ("\\.ejs\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-engine-detection t))

;; fish

(use-package fish-mode
  :preface
  (defun beetleman--format-fish-on-save ()
    (add-hook 'before-save-hook 'fish_indent-before-save))
  :hook
  (fish-mode . beetleman--format-fish-on-save))

;; TS

(use-package typescript-mode)

;; JS

(use-package add-node-modules-path
  :hook (((js-mode typescript-mode) . add-node-modules-path)))

;; json
;; use jsonian instead json-mode because json-mode derived from js-mode
;; and it make eglot to use js/ts lsp server for json if one is active
(use-package jsonian
  :mode ("\\.json\\'" . jsonian-mode))

;; OpenIA

(use-package gptel)

;; LSP

(use-package eglot
  :after yasnippet
  :commands (eglot eglot-ensure)
  :preface
  :hook (((markdown-mode
           markdown-ts-mode
           yaml-mode
           yaml-ts-mode
           clojure-mode
           clojurescript-mode
           typescript-mode
           tsx-ts-mode
           typescript-ts-mode
           js-mode
           js-ts-mode
           python-mode
           python-ts-mode
           nxml-mode
           web-mode
           css-mode
           css-ts-mode
           jsonian-mode
           go-mode
           go-ts-mode
           bash-ts-mode
           sh-mode)
          . eglot-ensure))
  :bind (("C-c e r" . eglot-rename)
         ("C-c e i" . eglot-code-action-organize-imports)
         ("C-c e e" . eglot-code-action-extract)
         ("C-c e q" . eglot-code-action-quickfix)
         ("C-c e a" . eglot-code-actions)
         ("C-c e f" . eglot-format))
  :init
  (setq eglot-autoshutdown t
        eglot-report-progress t
        eglot-send-changes-idle-time 0.5
        eglot-max-dir-watched 1000)
  (cl-defmethod eglot-register-capability :around
    (server (method (eql workspace/didChangeWatchedFiles)) id &key watchers)
    (condition-case err
        (let* ((dirs (length (delete-dups (mapcar #'file-name-directory
                                                  (project-files
                                                   (eglot--project server))))))
               (enabled (< dirs eglot-max-dir-watched)))
          (if enabled
              (progn
                (message "Enabling workspace/didChangeWatchedFiles capability. (Dirs: %s)" dirs)
                (cl-call-next-method))
            (progn
              (message "Disabling workspace/didChangeWatchedFiles capability. (Dirs: %s)" dirs)
              (eglot-unregister-capability server method id)
              nil)))
      (error
       (warn "Caught an error: %s" err)
       (cl-call-next-method))))
  :config
  (use-package consult-eglot
    :bind (:map eglot-mode-map
                ("C-M-." . consult-eglot-symbols)))

  (setq eglot-connect-timeout 300) ;; 5m
  (setq eglot-sync-connect 60)
  (setf (plist-get eglot-events-buffer-config :size) 0)
  (let* ((json-object-type 'plist)
         (json-array-type  'vector)
         (json-key-type    'keyword)
         (json-schemas     (plist-get (json-read-file "~/.emacs.d/data/catalog.json") :schemas)))
    (setq-default eglot-workspace-configuration
                  `(:gopls
                    (:staticcheck t
                                  :usePlaceholders t)
                    ;; https://github.com/microsoft/vscode/blob/main/extensions/json-language-features/server/README.md
                    :json
                    (:validate (:enable t)
                               :schemas ,json-schemas)
                    :yaml (:schemas ,json-schemas))))
  (setf eglot-server-programs
        `(,@eglot-server-programs
          (jsonian-mode . ("vscode-json-language-server" "--stdio" :initializationOptions (:provideFormatter t)))
          (web-mode . ,(eglot-alternatives '(("vscode-html-language-server" "--stdio")
                                             ("html-languageserver" "--stdio"))))
          (nxml-mode . ("java"
                        "-jar"
                        ,(expand-file-name "~/.emacs.d/share/lemminx/org.eclipse.lemminx-uber.jar")))))
  ;; Emacs LSP booster
  (when (executable-find "emacs-lsp-booster")
    (unless (package-installed-p 'eglot-booster)
      (and (fboundp #'package-vc-install)
           (package-vc-install "https://github.com/jdtsmith/eglot-booster")))
    (use-package eglot-booster
      :ensure nil
      :autoload eglot-booster-mode
      :init (eglot-booster-mode 1))))

(use-package dape
  :config
  (add-to-list 'dape-configs
               `(jdtls
                 modes (java-mode java-ts-mode)
                 fn (lambda (config)
                      (with-current-buffer
                          (find-file-noselect (expand-file-name (plist-get config :program)
                                                                (project-root (project-current))))
                        (thread-first
                          config
                          (plist-put 'hostname "localhost")
                          (plist-put 'port (eglot-execute-command (eglot-current-server)
                                                                  "vscode.java.startDebugSession" nil))
                          (plist-put :projectName (project-name (project-current))))))
                 :program dape-buffer-default
                 :request "attach"
                 :hostname "localhost"
                 :port 8000))
  (add-to-list 'dape-configs
               `(jdtls
                 modes (java-mode java-ts-mode)
                 fn (lambda (config)
                      (with-current-buffer
                          (find-file-noselect (expand-file-name (plist-get config :program)
                                                                (project-root (project-current))))
                        (thread-first
                          config
                          (plist-put 'hostname "localhost")
                          (plist-put 'port (eglot-execute-command (eglot-current-server)
                                                                  "vscode.java.startDebugSession" nil))
                          (plist-put :projectName (project-name (project-current))))))
                 :program dape-buffer-default
                 :request "attach"
                 :hostname "localhost"
                 :port 8000)))

(use-package jarchive
  :after eglot
  :config
  (jarchive-setup))

(use-package eglot-java
  :custom
  (eglot-java-eclipse-jdt-args '("-Xmx4G"
                                 "--add-modules=ALL-SYSTEM"
                                 "--add-opens"
                                 "java.base/java.util=ALL-UNNAMED"
                                 "--add-opens"
                                 "java.base/java.lang=ALL-UNNAMED"))
  :hook ((java-mode
          java-ts-mode)
         . eglot-java-mode)
  :preface
  (defun beetleman--eglot-java-init-opts (server eglot-java-eclipse-jdt)
    "Custom options that will be merged with any default settings."
    ;; download from https://repo1.maven.org/maven2/com/microsoft/java/com.microsoft.java.debug.plugin/
    `(:bundles
      [,(expand-file-name "~/.emacs.d/share/dape/com.microsoft.java.debug.plugin.jar")]))
  :config
  (setq eglot-java-user-init-opts-fn 'beetleman--eglot-java-init-opts)
  (let ((java-env (getenv "EGLOT_JAVA_JAVA_PROGRAM")))
    (when java-env
      (let ((java-executable (executable-find java-env)))
        (when java-executable
          (setq eglot-java-java-program java-executable))))))

(use-package apheleia
  :hook ((java-mode
          java-ts-mode
          sql-mode
          sql-ts-mode
          clojure-mode
          clojurescript-mode
          web-mode
          emacs-lisp-mode
          sly-mode)
         . apheleia-mode)
  :config
  (push (lambda ()
          (string= "false"
                   (getenv "EMACS_AUTOFORMAT")))
        apheleia-skip-functions))

;; code navigation
(use-package breadcrumb
  :custom
  (breadcrumb-project-max-length 0.4)
  (breadcrumb-imenu-max-length 0.4)
  :hook (after-init . breadcrumb-mode))

;; setup modeline

(use-package doom-modeline
  :custom
  (doom-modeline-vcs-max-length 15)
  (doom-modeline-project-detection 'project)
  (doom-modeline-minor-modes t)
  (doom-modeline-buffer-encoding 'nondefault)
  :hook (after-init . doom-modeline-mode)
  :config
  (setq x-underline-at-descent-line t))

(use-package minions
  :hook (doom-modeline-mode . minions-mode))


(use-package hide-mode-line
  :hook (((treemacs-mode
           eshell-mode
           shell-mode
           term-mode
           vterm-mode
           embark-collect-mode
           lsp-ui-imenu-mode
           pdf-annot-list-mode)
          . turn-on-hide-mode-line-mode)
         (dired-mode . (lambda()
                         (and (bound-and-true-p hide-mode-line-mode)
                              (turn-off-hide-mode-line-mode))))))

;; poppler
(use-package popper
  :preface
  (defun beetleman--fit-window-height (win)
    "Determine the height of popup window WIN by fitting it to the buffer's content."
    (fit-window-to-buffer
     win
     (floor (frame-height) 2)
     (floor (frame-height) 3)))
  (defun beetleman--popper-display-function (buffer &optional alist)
    (let ((window (display-buffer-in-direction buffer
                                               (append alist
                                                       `(;;(direction . bottom)
                                                         (direction . down)
                                                         (window-height . ,#'beetleman--fit-window-height))))))
      (select-window window)))
  :custom
  (popper-group-function #'popper-group-by-directory)
  (popper-echo-dispatch-actions t)
  (popper-display-function #'beetleman--popper-display-function)
  :bind (:map popper-mode-map
              ("C-`"  . popper-toggle)
              ("C-<tab>" . popper-cycle)
              ("C-~" . popper-toggle-type))
  :hook (emacs-startup . popper-echo-mode)
  :init
  (setq popper-reference-buffers
        '("^\\*eldoc.*\\*$"
          "\\*Compile-Log\\*$"
          "\\*cider-repl.*\\*$"

          comint-mode
          compilation-mode
          help-mode
          helpful-mode
          tabulated-list-mode
          Buffer-menu-mode
          cider-repl-mode
          sly-mrepl-mode
          flymake-diagnostics-buffer-mode

          "^\\*.*eshell.*\\*.*$"
          "^\\*.*shell.*\\*.*$"
          "^\\*.*terminal.*\\*.*$"
          "^\\*.*vterm[inal]*.*\\*.*$"))

  (with-eval-after-load 'doom-modeline
    (setq popper-mode-line
          '(:eval (let ((face (if (doom-modeline--active)
                                  'doom-modeline-emphasis
                                'doom-modeline)))
                    (if (and (icons-displayable-p)
                             (bound-and-true-p doom-modeline-icon)
                             (bound-and-true-p doom-modeline-mode))
                        (format " %s "
                                (nerd-icons-octicon "nf-oct-pin" :face face))
                      (propertize " POP " 'face face)))))))


;; reset GC
(use-package gcmh
  :hook (emacs-startup . gcmh-mode)
  :init
  (setq gcmh-idle-delay             'auto  ; default is 15s
        gcmh-auto-idle-delay-factor 30
        gcmh-high-cons-threshold    #x3000000)) ; 48mb

;; config changes made through the customize UI will be stored here

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
