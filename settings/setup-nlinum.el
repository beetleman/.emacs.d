(require 'use-package)
(use-package nlinum-relative
  :ensure t
  :config
  (nlinum-relative-setup-evil)
  (add-hook 'prog-mode-hook 'nlinum-relative-mode)
  (setq nlinum-relative-redisplay-delay 0.1)
  (setq nlinum-relative-current-symbol "->")
  (setq nlinum-relative-offset 1))
(provide 'setup-nlinum)
