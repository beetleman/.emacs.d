(use-package better-defaults
  :defer t
  :ensure t
  :init
  (require 'better-defaults))


(when (executable-find "hunspell")
  (setq-default ispell-program-name "hunspell"))

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(use-package dashboard
  :ensure t
  :config
  (setq dashboard-items '((projects . 5)
                          (recents . 5)
                          (agenda . 5)
                          (bookmarks . 5)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-startup-banner 'logo)
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

(use-package anzu
  :defer t
  :ensure t
  :init
  (global-anzu-mode 1)
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp))

(use-package undo-tree
  :defer t
  :ensure t
  :init
  (global-undo-tree-mode 1))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(global-auto-revert-mode 1)
(winner-mode 1)
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024 3)) ;; 3mb
;; (setq x-select-enable-clipboard-manager t)
;; (setq x-select-enable-clipboard t)


(provide 'setup-common)
