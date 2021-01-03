;;; init.el --- Mateusz's Emacs configuration
;;
;; Copyright (c) 2017-2021 Mateusz Batsov
;;
;; Author: Mateusz Probachta <mateusz.probachta@gmail.com>
;; URL: https://github.com/beetleman/.emacs.d
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is my personal Emacs configuration.  Simple and primitive as me.

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

(setq user-full-name "Mateusz Probachta"
      user-mail-address "mateusz.probachta@gmail.com")

;; Always load newest byte code
;; (setq load-prefer-newer t)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

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
(add-to-list 'default-frame-alist '(cursor-color . "magenta"))

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

;; for emacs profiling
(use-package esup
  :commands (esup)
  :config
  (setq esup-depth 0))


(use-package all-the-icons)

(use-package doom-themes
  :config
  (load-theme 'doom-nord t)
  (doom-themes-org-config)

  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    ;; for ligth "#f9f2d9"
    ;; for dark "#002b36"
    (set-face-attribute 'mode-line-inactive nil
			:background (face-attribute 'hl-line :background))))


(use-package flycheck
  :init (global-flycheck-mode))


(use-package ace-window
  :bind (("M-o" . ace-window)))


(use-package super-save
  :config
  ;; add integration with ace-window
  (add-to-list 'super-save-triggers 'ace-window)
  (super-save-mode +1))

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
  :hook ((prog-mode . smartparens-strict-mode)
         (cider-repl-mode . smartparens-strict-mode)
         (markdown-mode . smartparens-strict-mode))
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
  :init
  (setq company-idle-delay 0.5)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  :config
  (global-company-mode))

;; Org

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;; Languages

(use-package dockerfile-mode)
(use-package php-mode
  :defer t)


;; Setup clojure

(use-package flycheck-clj-kondo
  :after (clojure-mode))

(use-package cider
  :hook ((cider-mode . clj-refactor-mode)
         (cider-mode . smartparens-mode)
         (cider-mode . eldoc-mode))
  :config
  (cider-auto-test-mode 1))

(use-package cider-eval-sexp-fu)

(use-package clj-refactor
  :defer t)

(use-package zprint-mode
  :hook (clojure-mode clojurescript-mode))


;; setup modeline

(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))


(use-package minions
  :custom
  (minions-direct '(overwrite-mode flycheck-mode))
  :config (minions-mode 1))


;; config changes made through the customize UI will be stored here

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
