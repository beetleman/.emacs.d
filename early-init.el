;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-

(setq gc-cons-threshold most-positive-fixnum)

;; I handle package init in `init.el' file
(setq package-enable-at-startup nil)

(prefer-coding-system 'utf-8)

(setq frame-inhibit-implied-resize t)

;; Disable not used visual elements
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))
(setq-default mode-line-format nil)
