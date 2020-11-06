(use-package better-defaults
  :init
  (require 'better-defaults))


(when (executable-find "hunspell")
  (setq-default ispell-program-name "hunspell"))

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))


(use-package anzu
  :config
  (global-anzu-mode t)
  :bind
  (("<remap> <query-replace>" . 'anzu-query-replace)
   ("<remap> <query-replace-regexp>" . 'anzu-query-replace-regexp)))


(use-package undo-tree
  :init
  (global-undo-tree-mode 1))

(use-package flycheck
  :init (global-flycheck-mode))


(use-package crux
  :bind (("M-o" . crux-other-window-or-switch-buffer)))

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024 3)) ;; 3mb

(setq inhibit-startup-screen t)
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

(provide 'setup-common)
