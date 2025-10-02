;; -*- coding: utf-8; lexical-binding: t; -*-
;; run it in terminal:
;; `emacs -Q --script fix-elpa.el`

(setq package-check-signature nil)

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-refresh-contents)
(package-install 'gnu-elpa-keyring-update)
(gnu-elpa-keyring-update)

(setq package-check-signature "allow-unsigned")
