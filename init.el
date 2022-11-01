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


(setq user-full-name "Mateusz Probachta"
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
      ";;                        _))
;;  ❤️ Happy Hacking ❤️  > *\\     _~
;;                       `;'\\\\__-' \\_
;;                          | )  _ \\ \\
;;                         / / ``   w w
;;                        w w
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

;; setup font settings
;; (add-to-list 'default-frame-alist '(font . "Monaco-12"))
(if (eq system-type 'darwin) ;; mac specific settings
    (add-to-list 'default-frame-alist '(font . "IBM Plex Mono-12"))
  (add-to-list 'default-frame-alist '(font . "IBM Plex Mono-10")))
;; (add-to-list 'default-frame-alist '(font . "Iosevka Fixed SS07 Extended"))
;; (add-to-list 'default-frame-alist '(font . "Iosevka Fixed SS07-10"))
;; (add-to-list 'default-frame-alist '(cursor-color . "magenta"))
;; (set-face-attribute 'default t :font (font-spec :name "Iosevka Fixed SS07" :size 10 :style "Extended"))
;; (set-face-attribute 'default t
;; 		    :font "Iosevka Fixed SS07-10"
;;                     :width 'expanded)



(defun --set-emoji-font (frame)
  "Adjust the font settings of FRAME so Emacs can display: ✨🍆✨."
  (set-fontset-font t 'symbol "Noto Color Emoji" frame)
  (set-fontset-font t 'symbol "Symbola" frame 'append))


(--set-emoji-font nil)
(add-hook 'after-make-frame-functions '--set-emoji-font)


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

;; update the package metadata is the local cache is missing
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)


;;; BUILT-IN PACKAGES

(use-package hl-line
  :config
  (global-hl-line-mode +1))


(use-package flyspell
  :hook ((text-mode . flyspell-mode)
	 (prog-mode . flyspell-prog-mode))
  :config
  (when (executable-find "aspell")
    (setq ispell-program-name "aspell")))


;;; THIRD-PARTY PACKAGES


(use-package ws-butler
  :hook ((prog-mode . ws-butler-mode)
	 (markdown-mode . ws-butler-mode)
	 (org-mode . ws-butler-mode)))

;; for emacs profiling
(use-package esup
  :commands (esup)
  :config
  (setq esup-depth 0))


(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package all-the-icons)

(use-package doom-themes
  :custom
  (doom-themes-padded-modeline 1)
  :init
  (defvar beetleman/theme-dark 'doom-gruvbox)
  (defvar beetleman/theme-light 'doom-gruvbox-light)

  (load-theme beetleman/theme-dark t)
  (doom-themes-org-config)
  (doom-themes-treemacs-config)
  (doom-themes-visual-bell-config)

  :config
  (defun beetleman/themes-toggle ()
    (interactive)
    (let ((current-theme (car custom-enabled-themes)))
      (cond
       ((eq current-theme beetleman/theme-dark)
	(load-theme beetleman/theme-light t))
       ((eq current-theme beetleman/theme-light)
	(load-theme beetleman/theme-dark t))
       (t (load-theme beetleman/theme-dark t)))))
  :bind ("<f5>" . beetleman/themes-toggle))

(use-package popper
  :bind (("C-`"   . popper-toggle-latest)
         ("C-~"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("^\\*cider-error*"
	  "^\\*cider-repl"
	  "^\\*cider-repl-history"
	  compilation-mode)
	popper-group-function #'popper-group-by-perspective)
  (popper-mode +1)
  (popper-echo-mode +1))


(use-package flycheck
  :init (global-flycheck-mode))

(use-package flycheck-inline
  :after flycheck
  :hook (flycheck-mode . flycheck-inline-mode))


(use-package ace-window
  :bind (("M-o" . ace-window)))


(use-package crux
  :bind (("C-c o" . crux-open-with)
	 ("C-c n" . crux-cleanup-buffer-or-region)
	 ("C-c f" . crux-recentf-find-file)
	 ("C-M-z" . crux-indent-defun)
	 ("C-c u" . crux-view-url)
	 ("C-c e" . crux-eval-and-replace)
	 ("C-c w" . crux-swap-windows)
	 ("C-c D" . crux-delete-file-and-buffer)
	 ("C-c r" . crux-rename-buffer-and-file)
	 ("C-c t" . crux-visit-term-buffer)
	 ("C-c k" . crux-kill-other-buffers)
	 ("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
	 ("C-c I" . crux-find-user-init-file)
	 ("C-c S" . crux-find-shell-init-file)
	 ("s-r" . crux-recentf-find-file)
	 ("s-j" . crux-top-join-line)
	 ("C-^" . crux-top-join-line)
	 ("s-k" . crux-kill-whole-line)
	 ("C-<backspace>" . crux-kill-line-backwards)
	 ([remap move-beginning-of-line] . crux-move-beginning-of-line)
	 ([(shift return)] . crux-smart-open-line)
	 ([(control shift return)] . crux-smart-open-line-above)
	 ([remap kill-whole-line] . crux-kill-whole-line)
	 ("C-c s" . crux-ispell-word-then-abbrev)))

(use-package diff-hl
  :config
  (global-diff-hl-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package which-key
  :config
  (which-key-mode +1))

(use-package undo-tree
  :config
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
	`((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode +1))


(use-package magit
  :bind
  ("C-x g" . magit-status))

(use-package diff-hl
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :init
  (global-diff-hl-mode)
  (diff-hl-margin-mode))


(use-package ivy
  :bind
  (("C-s" . swiper)
   ("C-c C-r" . ivy-resume)
   ("<f6>" . ivy-resume))
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (ivy-mode 1))


(use-package counsel
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("<f1> f" . counsel-describe-function)
   ("<f1> v" . counsel-describe-variable)
   ("<f1> l" . counsel-find-library)
   ("<f2> i" . counsel-info-lookup-symbol)
   ("<f2> u" . counsel-unicode-char)
   ("C-c g" . counsel-git)
   ("C-c k" . counsel-ag)
   ("C-x l" . counsel-locate)
   ("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-p" . ivy-next-line)
   :map read-expression-map
   ("C-r" . counsel-expression-history))
  :config
  (when (fboundp 'ido-mode)
    (ido-mode -1))
  (ivy-mode 1))


(use-package prescient
  :after counsel
  :config
  (prescient-persist-mode 1))

(use-package ivy-prescient
  :after prescient
  :config
  (ivy-prescient-mode 1))


(use-package ivy-xref
  :init
  ;; xref initialization is different in Emacs 27 - there are two different
  ;; variables which can be set rather than just one
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
  ;; commands other than xref-find-definitions (e.g. project-find-regexp)
  ;; as well
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))


(use-package projectile
  :hook (after-init . projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-enable-caching t))


(use-package perspective
  :after (projectile)
  :hook (after-init . persp-mode)
  :bind (("C-x b" . persp-switch-to-buffer*)
         ("C-x k" . persp-kill-buffer*))
  :custom
  (persp-modestring-short t)
  (persp-mode-prefix-key (kbd "C-x x"))
  (persp-initial-frame-name "Main"))


(use-package persp-projectile
  :after (perspective projectile))


(use-package treemacs
  :config
  ;;(treemacs-resize-icons 44)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))

  (treemacs-hide-gitignored-files-mode nil)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("<f9>"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode 1))


(use-package smartparens
  :hook (prog-mode . smartparens-strict-mode)
  :config
  (require 'smartparens-config)
  (sp-use-paredit-bindings))

(use-package eldoc
  :hook (prog-mode . eldoc-mode))

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


(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
	hl-todo-keyword-faces
	'(("TODO"   . "#FF0000")
	  ("FIXME"  . "#FF0000")
	  ("DEBUG"  . "#A020F0")
	  ("GOTCHA" . "#FF4500")
	  ("STUB"   . "#1E90FF"))))

(use-package multiple-cursors
  :bind (("C-|" . mc/edit-lines)
	 ("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-c C->" . mc/mark-all-like-this)))


(use-package anzu
  :bind
  (([remap query-replace] . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp))
  :hook
  (after-init . global-anzu-mode))


(use-package direnv
  :config
  (direnv-mode))


(use-package editorconfig
  :config
  (editorconfig-mode 1))


(use-package company
  :bind (("M-i" . company-complete)
	 :map company-active-map
	 ("C-n" . company-select-next)
	 ("C-p" . company-select-previous))
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.5)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t))


(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))


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

(use-package caml
  :ensure t)

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

(use-package flycheck-clj-kondo
  :after (clojure-mode))


(use-package clojure-mode
  :mode (("\\.clj$" . clojure-mode)
	 ("\\.cljs$" . clojurescript-mode)
	 ("\\.cljc$" . clojurec-mode)
	 ("\\.edn$" . clojure-mode)
	 ("\\.bb$" . clojure-mode)))


(use-package clj-refactor
  :defer t)

(use-package cider
  :hook ((cider-mode . clj-refactor-mode)
	 (cider-repl-mode . company-mode)
	 (cider-repl-mode . smartparens-strict-mode)
	 (cider-mode . eldoc-mode))
  :custom
  (cider-repl-display-help-banner nil)
  (cider-repl-result-prefix ";; => ")
  (clojure-toplevel-inside-comment-form t)
  (cider-repl-history-size 1000)
  (cider-repl-history-file ".cider-repl-history")
  (cider-repl-buffer-size-limit 1000)
  :config
  (cider-auto-test-mode 1))

(use-package cider-eval-sexp-fu)

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

(use-package markdown-mode
  :hook ((cider-repl-mode . smartparens-strict-mode)))

;; setup webmode

(use-package web-mode
  :mode ("\\.vue\\'" . web-mode))


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

(setq lsp-keymap-prefix "s-l")

(use-package lsp-mode
  :hook ((js-mode . lsp)
	 (web-mode . lsp)
	 (typescript-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)


;; setup modeline
(use-package minions
  :custom
  (minions-prominent-modes '(flycheck-mode pyvenv-mode))
  (minions-mode-line-lighter "🏳️‍🌈")
  :config
  (minions-mode 1))

;; reset GC
(use-package gcmh
  :init
  (gcmh-mode 1))

;; config changes made through the customize UI will be stored here

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
