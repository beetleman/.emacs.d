;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-

;;; Startup performance

;; Disable GC during startup; `gcmh' takes over on `emacs-startup-hook'.
(setq gc-cons-threshold most-positive-fixnum)

;; Skip file-name handlers (TRAMP, EPA, ...) while init files load; keep only
;; jka-compr so compressed *.el.gz sources still work.  Restored after startup.
;; Caveat: remote/.gpg files passed on the command line open before restore.
(defvar beetleman--file-name-handler-alist file-name-handler-alist
  "`file-name-handler-alist' saved before startup.")
(unless (daemonp)
  (setq file-name-handler-alist
        (let ((gz (rassq 'jka-compr-handler file-name-handler-alist)))
          (and gz (list gz))))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq file-name-handler-alist
                    (delete-dups (append file-name-handler-alist
                                         beetleman--file-name-handler-alist))))
            101))

;; Emacs 31: cache `load-path' directory listings to speed up `require'.
(when (fboundp 'load-path-filter-cache-directory-files)
  (setq load-path-filter-function #'load-path-filter-cache-directory-files))

(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)
;; Emacs 31: don't start async native compilation while on battery.
(setq native-comp-async-on-battery-power nil)

;; I handle package init in `init.el' file
(setq package-enable-at-startup nil)

(prefer-coding-system 'utf-8)

;;; Frames, redisplay & startup UI (before the first frame is drawn)

(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      inhibit-compacting-font-caches t
      use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      auto-mode-case-fold nil)

;; Cheaper redisplay: no bidirectional reordering for LTR-only text.
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Avoid a white flash before the theme loads.
(push '(background-color . "#000000") initial-frame-alist)
(push '(foreground-color . "#ffffff") initial-frame-alist)

;; Disable not used visual elements
(setq default-frame-alist `(;; You can turn off scroll bars by uncommenting these lines:
                            (vertical-scroll-bars . nil)
                            (horizontal-scroll-bars . nil)
                            (menu-bar-lines . 0)
                            (tool-bar-lines . 0)
                            ,@default-frame-alist))

(when (featurep 'ns)
  (push '(ns-appearance . dark) default-frame-alist))

(setq-default mode-line-format nil)
(setq ns-use-proxy-icon nil)
(setq frame-title-format nil)

(if (eq system-type 'darwin)
    (set-face-attribute 'default nil :family "Iosevka" :height 140)
  (set-face-attribute 'default nil :family "Iosevka" :height 110))
(set-face-attribute 'variable-pitch nil :family "Iosevka Aile" :height 1.0)
(set-face-attribute 'fixed-pitch nil :family "Iosevka" :height 1.0)

(set-fontset-font t 'symbol "Noto Color Emoji" nil)
(set-fontset-font t 'symbol "Symbola" nil 'append)
