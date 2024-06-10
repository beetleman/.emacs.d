;;; -*- lexical-binding: t; -*-
;;; init.el --- Mateusz's Emacs configuration
;;
;; Copyright (c) 2017-2021 Mateusz P.
;;
;; Author: Mateusz Probachta <mateusz.probachta@gmail.com>
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

;; turn off GC during star up
(setq gc-cons-threshold most-positive-fixnum)

(setq inhibit-compacting-font-caches t)

(setq user-full-name "Mateusz Probachta-Jeżowski"
      user-mail-address "mateusz.probachta@gmail.com")

;; Always load newest byte code
;; (setq load-prefer-newer t)


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

;; replace selected section if start typing
(delete-selection-mode 1)


;; hide anoying UI
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (and (fboundp 'menu-bar-mode)
	   (not (eq system-type 'darwin)))
  (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(setq inhibit-startup-screen t)
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

(global-set-key (kbd "C-z C-z") 'beetleman-suspend-frame)
(global-set-key (kbd "C-x C-z") 'beetleman-suspend-frame)

(defun beetleman-suspend-frame ()
  "In a GUI environment, do nothing; otherwise `suspend-frame'."
  (interactive)
  (if (display-graphic-p)
      (message "suspend-frame disabled for graphical displays.")
    (suspend-frame)))

;; always require final \n
(setq require-final-newline t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Permanently indent with spaces, never with TABs
(setq-default c-basic-offset   4
              tab-width        4
              indent-tabs-mode nil)

;; setup font settings
(if (eq system-type 'darwin) ;; mac specific settings
    (add-to-list 'default-frame-alist '(font . "Iosevka Fixed SS08-13:width=expanded"))
  (add-to-list 'default-frame-alist '(font . "Iosevka Fixed SS08-11:width=expanded")))

(defun beetleman:set-emoji-font (frame)
  "Adjust the font settings of FRAME so Emacs can display: ✨🍆✨."
  (set-fontset-font t 'symbol "Noto Color Emoji" frame)
  (set-fontset-font t 'symbol "Symbola" frame 'append))


(beetleman:set-emoji-font nil)
(add-hook 'after-make-frame-functions 'beetleman:set-emoji-font)

;; server
(require 'server)
(unless (server-running-p) (server-start))

;; vc-mode
(setq auto-revert-check-vc-info t)

;; setup ediff

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; my functions
(require 'ansi-color)
(defun display-ansi-colors ()
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

(require 'use-package)
(setq use-package-always-ensure t)


;;; BUILT-IN PACKAGES

(use-package project
  :ensure nil
  :config
  (setq project-vc-extra-root-markers '(".project.el" "workspace.edn")))

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


;;; THIRD-PARTY PACKAGES

(use-package meow
  :config
  (setq meow-replace-state-name-list '((normal . "<N>")
				       (motion . "<M>")
				       (keypad . "<K>")
				       (insert . "<I>")
				       (beacon . "<B>")))
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
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
   '("?" . meow-cheatsheet))
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
   '("<escape>" . ignore))
  (meow-global-mode 1))

(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

(use-package mode-line-bell
  :hook (after-init . mode-line-bell-mode))

(use-package ws-butler
  :custom
  (ws-butler-convert-leading-tabs-or-spaces t)
  :hook ((prog-mode . ws-butler-mode)
	     (markdown-mode . ws-butler-mode)
	     (org-mode . ws-butler-mode)))

;; for emacs profiling
(use-package esup
  :commands (esup)
  :config
  (setq esup-depth 0))

(use-package treemacs
  :custom
  (treemacs-read-string-input 'from-minibuffer)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("<f9>"      . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-nerd-icons
  :custom-face
  ;; (treemacs-nerd-icons-root-face ((t (:inherit nerd-icons-green :height 1.3))))
  (treemacs-nerd-icons-root-face ((t (:inherit nerd-icons-dsilver))))
  (treemacs-nerd-icons-file-face ((t (:inherit nerd-icons-dsilver))))
  :config (treemacs-load-theme "nerd-icons"))

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package nerd-icons-dired
  :custom-face
  (nerd-icons-dired-dir-face ((t (:inherit nerd-icons-dsilver :foreground unspecified))))
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package dired-quick-sort
  :after dired
  :config
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired t
          insert-directory-program "/opt/homebrew/bin/gls"))
  (dired-quick-sort-setup))

(use-package modus-themes
  :demand t
  :config
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs nil
        modus-themes-bold-constructs nil
	modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))

  ;; Maybe define some palette overrides, such as by using our presets
  (setq modus-themes-common-palette-overrides
        `((bg-paren-match bg-magenta-intense)
	  (bg-mode-line-active bg-cyan-subtle)
          ,@modus-themes-preset-overrides-faint))

  ;; Load the theme of your choice.
  (modus-themes-load-theme 'modus-vivendi-tinted)

  :bind ("<f5>" . #'modus-themes-toggle))

;; (use-package popper
;;   :bind (("C-`"   . popper-toggle-latest)
;;          ("C-~"   . popper-cycle)
;;          ("C-M-`" . popper-toggle-type))
;;   :init
;;   (setq popper-reference-buffers
;;         '("^\\*cider-error*"
;; 	  "^\\*cider-repl"
;; 	  "^\\*cider-repl-history"
;; 	  compilation-mode))
;;   (popper-mode +1)
;;   (popper-echo-mode +1))

(use-package ace-window
  :bind (("M-o" . ace-window)))

(use-package which-key
  :config
  (which-key-mode +1))

(use-package vundo
  :ensure t
  :bind (("C-c v" . vundo)))

(use-package magit
  :custom
  (magit-log-margin '(t "%D" magit-log-margin-width t 18))
  :bind
  ("C-x g" . magit-status))

(use-package diff-hl
  :custom
  (diff-hl-margin-symbols-alist '((insert . "+")
				  (delete . "-")
				  (change . "~")
				  (unknown . "?")
				  (ignored . "i")))
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
	 (magit-post-refresh . diff-hl-magit-post-refresh))
  :init
  (global-diff-hl-mode)
  (diff-hl-margin-mode))


;; Optionally use the `orderless' completion style.
(use-package orderless
  ;; :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  ;; (setq completion-styles '(orderless basic)
  ;;       completion-category-defaults nil
  ;;       completion-category-overrides '((file (styles . (partial-completion)))))
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-component-separator #'orderless-escapable-split-on-space))

;; Enable vertico

(use-package vertico
  :hook (after-init . vertico-mode))

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
  :config
  (super-save-mode +1))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

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

(use-package marginalia
  :hook (after-init . marginalia-mode))

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
  :ensure t
  :bind (:map grep-mode-map
              ("e" . wgrep-change-to-wgrep-mode)
              ("C-x C-q" . wgrep-change-to-wgrep-mode)
              ("C-c C-c" . wgrep-finish-edit)))

(use-package nerd-icons-completion
  :after marginalia
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode))

(use-package puni
  :hook (prog-mode . puni-mode)
  :bind (("C-<right>" . puni-slurp-forward)
         ("C-<left>" . puni-barf-forward)
	 ("C-M-<left>" . puni-slurp-backward)
	 ("C-M-<right>" . puni-barf-backward)))

(use-package eldoc
  :hook (prog-mode . eldoc-mode)
  :bind (("C-c d" . eldoc)))

(use-package subword
  :hook (prog-mode . subword-mode))


(use-package yasnippet
  :hook ((prog-mode . yas-minor-mode)
	 (org-mode . yas-minor-mode)
	 (yaml-mode . yas-minor-mode)
	 (markdown-mode . yas-minor-mode)))

(use-package yasnippet-snippets
  :after (yasnippet)
  :config
  (yas-reload-all))

(use-package yasnippet-capf
  :after (yasnippet)
  :init (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(use-package hl-todo
  :hook ((prog-mode . hl-todo-mode)
	 (yaml-mode . hl-todo-mode)
	 (markdown-mode . yas-minor-mode))
  :config
  (setq hl-todo-highlight-punctuation ":"
	hl-todo-keyword-faces
	'(("TODO"   . "#FF0000")
	  ("FIXME"  . "#FF0000")
	  ("DEBUG"  . "#A020F0")
	  ("GOTCHA" . "#FF4500")
	  ("STUB"   . "#1E90FF"))))


(use-package anzu
  :bind
  (([remap query-replace] . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp))
  :hook
  (after-init . global-anzu-mode))

(use-package envrc
  :defer 2
  :if (executable-find "direnv")
  :bind (:map envrc-mode-map
              ("C-c d" . envrc-command-map))
  :config (envrc-global-mode))

(use-package eat
  :custom
  (eat-term-scrollback-size 10000)
  (eat-enable-yank-to-terminal t)
  :bind (("C-c t" . eat-project-other-window)
	 ("C-c T" . eat)))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-preview-current nil)
  (corfu-echo-mode t)
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  :bind
  (:map corfu-map ("M-?" . corfu-insert-separator))
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))
  :init
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1)
  (corfu-history-mode 1))

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; A few more useful configurations...
(use-package emacs
  :init
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete))

;; Add extensions
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)

  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))


(use-package format-all)

;; Org

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;; Languages


(use-package yaml-mode)

(use-package dockerfile-mode)
(use-package php-mode
  :defer t)

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

;; Setup clojure

;; (use-package flycheck-clj-kondo
;;   :after (clojure-mode))


(use-package clojure-mode
  :mode (("\\.clj$" . clojure-mode)
	 ("\\.cljs$" . clojurescript-mode)
	 ("\\.cljc$" . clojurec-mode)
	 ("\\.edn$" . clojure-mode)
	 ("\\.bb$" . clojure-mode)))


(use-package clj-refactor
  :defer t)


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
	inferior-lisp-program "sbcl"))

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


;; SQL

(use-package sqlformat)

;; TS

(use-package typescript-mode)

;; JS

(use-package add-node-modules-path
  :hook (((js-mode typescript-mode) . add-node-modules-path)))

(use-package prettier-js
  :hook (((js-mode typescript-mode web-mode) . prettier-js-mode)))

;; LSP

(use-package eglot
  :commands (eglot eglot-ensure)
  :hook ((js-mode . eglot-ensure)
         (web-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (clojure-mode . eglot-ensure)
         (clojurescript-mode . eglot-ensure)
         (clojurec-mode . eglot-ensure)
         (sh-mode . eglot-ensure)
         (yaml-mode . eglot-ensure))
  :bind (("C-c r" . eglot-rename)
         ("C-c f" . eglot-format))
  ;; :init
  ;; (defface eglot-diagnostic-tag-deprecated-face
  ;;   '((t . (:inherit flymake-warning)))
  ;;   "Face used to render deprecated or obsolete code.")
  ;; (defface eglot-diagnostic-tag-unnecessary-face
  ;;   '((t . (:inherit flymake-warning)))
  ;;   "Face used to render unused or unnecessary code.")
  :config

  (setq eglot-connect-timeout 300) ;; 5m
  (setf (plist-get eglot-events-buffer-config :size) 0)
  (add-to-list 'eglot-server-programs
               `(web-mode . ,(eglot-alternatives '(("vscode-html-language-server" "--stdio")
                                                   ("html-languageserver" "--stdio"))))))

;; (use-package apheleia
;;   ;; for formating after save file
;;   :hook ((clojure-mode . apheleia-mode)
;; 	 (clojurescript-mode . apheleia-mode)
;;          (clojurec-mode . apheleia-mode))
;;   :config
;;   (setf (alist-get 'cljfmt apheleia-formatters)
;;         '("cljfmt" "fix" "-"))
;;   (setf (alist-get 'clojure-mode apheleia-mode-alist)
;; 	'(cljfmt)))

;; setup modeline

(use-package doom-modeline
  :custom
  (doom-modeline-vcs-max-length 15)
  (doom-modeline-project-detection 'project)
  :hook (after-init . doom-modeline-mode)
  :config
  (setq x-underline-at-descent-line t))

;; reset GC
(use-package gcmh
  :init
  (setq gcmh-idle-delay             'auto  ; default is 15s
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold    (* 16 1024 1024))  ; 16mb
  (gcmh-mode 1))

;; config changes made through the customize UI will be stored here

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
