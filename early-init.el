;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-

(setq gc-cons-threshold most-positive-fixnum)
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

;; I handle package init in `init.el' file
(setq package-enable-at-startup nil)

(prefer-coding-system 'utf-8)

(setq frame-inhibit-implied-resize t)

;; Disable not used visual elements
(setq default-frame-alist `(;; You can turn off scroll bars by uncommenting these lines:
                            (vertical-scroll-bars . nil)
                            (horizontal-scroll-bars . nil)
                            (menu-bar-lines . 0)
                            (tool-bar-lines . 0)
                            ;; Setting the face in here prevents flashes of
                            ;; color as the theme gets activated
                            (background-color . "#0D0E1C")
                            (foreground-color . "#FFFFFF")
                            ,@default-frame-alist))

(when (featurep 'ns)
  (push '(ns-appearance . dark) default-frame-alist))

(setq-default mode-line-format nil)
(setq ns-use-proxy-icon nil)
(setq frame-title-format nil)
