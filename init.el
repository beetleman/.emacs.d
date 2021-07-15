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

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; hide anoying UI
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(setq inhibit-startup-screen t)
;; and show this majestic creature!
(setq initial-scratch-message
";;        ❤️ Happy Hacking ❤️
;;             _     _
;;            (')-=-(')
;;          __(   \"   )__
;;         / _/'-----'\\_ \\
;;      ___\\\\ \\\\     // //___
;;      >____)/_\\---/_\\(____<
") ;; Art by Joan Stark

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
(add-to-list 'default-frame-alist '(font . "Hack-10"))
; (add-to-list 'default-frame-alist '(cursor-color . "magenta"))

(defun --set-emoji-font (frame)
  "Adjust the font settings of FRAME so Emacs can display: ✨🍆✨."
  (set-fontset-font t 'symbol "Noto Color Emoji" frame)
  (set-fontset-font t 'symbol "Symbola" frame 'append))


(--set-emoji-font nil)
(add-hook 'after-make-frame-functions '--set-emoji-font)


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
  (when (executable-find "hunspell")
    (setq-default ispell-program-name "hunspell")))


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


(use-package all-the-icons)


(use-package doom-themes
  :init
  (defvar beetleman/theme-dark 'doom-solarized-dark)
  (defvar beetleman/theme-light 'doom-solarized-light)
    ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  (load-theme beetleman/theme-dark t)
  (doom-themes-org-config)
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


;; (use-package modus-themes
;;   :init
;;   (modus-themes-load-themes)
;;   :config
;;   (modus-themes-load-vivendi)
;;   :bind ("<f5>" . modus-themes-toggle))



(use-package flycheck
  :init (global-flycheck-mode))


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


(use-package swiper
  :bind
  ("C-s" . swiper))

(use-package ivy
  :bind
  (("C-c C-r" . ivy-resume)
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
  :bind-keymap
  ("C-c p" . projectile-command-map))


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
  :hook (prog-mode . yas-minor-mode))


(use-package yasnippet-snippets
  :after (yasnippet)
  :init
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

;; Org

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;; Languages

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


(use-package clojure-mode)


(use-package clj-refactor
  :defer t)


(use-package cider
  :hook ((cider-mode . clj-refactor-mode)
	 (cider-repl-mode . company-mode)
	 (cider-repl-mode . smartparens-strict-mode)
	 (cider-mode . eldoc-mode))
  :config
  (cider-auto-test-mode 1))

(use-package cider-eval-sexp-fu)


(use-package zprint-mode
  :hook (clojure-mode clojurescript-mode))

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

;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)


;; setup modeline

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-vcs-max-length 25))

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
