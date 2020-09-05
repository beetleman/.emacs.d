(use-package better-defaults
  :defer t
  :init
  (require 'better-defaults))


(when (executable-find "hunspell")
  (setq-default ispell-program-name "hunspell"))

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(use-package anzu
  :defer t
  :init
  (global-anzu-mode 1)
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp))

(use-package undo-tree
  :defer t
  :init
  (global-undo-tree-mode 1))

(use-package flycheck
  :init (global-flycheck-mode))

(global-auto-revert-mode 1)
(winner-mode 1)
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024 3)) ;; 3mb
;; (setq x-select-enable-clipboard-manager t)
;; (setq x-select-enable-clipboard t)

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
