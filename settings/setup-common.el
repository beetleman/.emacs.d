(use-package better-defaults
  :defer t
  :ensure t
  :init
  (require 'better-defaults))


(when (executable-find "hunspell")
  (setq-default ispell-program-name "hunspell")
  (setq ispell-really-hunspell t))

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

(global-auto-revert-mode 1)
(winner-mode 1)
(setq x-select-enable-clipboard-manager  nil)



(provide 'setup-common)
