(require 'use-package)
(use-package nlinum-relative
    :config
    (nlinum-relative-setup-evil)
    (add-hook 'prog-mode-hook 'nlinum-relative-mode)
    (setq nlinum-relative-redisplay-delay 0)
    (setq nlinum-relative-current-symbol "->")
    (setq nlinum-relative-offset 0))
(provide 'my-nlinum)
