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


(use-package ace-window
  :bind (("M-o" . ace-window)))


(global-unset-key (kbd "C-z"))

(global-set-key (kbd "C-z C-z") 'my-suspend-frame)
(global-set-key (kbd "C-x C-z") 'my-suspend-frame)

(defun my-suspend-frame ()
  "In a GUI environment, do nothing; otherwise `suspend-frame'."
  (interactive)
  (if (display-graphic-p)
      (message "suspend-frame disabled for graphical displays.")
    (suspend-frame)))

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
