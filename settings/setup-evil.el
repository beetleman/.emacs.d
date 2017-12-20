(require 'use-package)
(use-package evil
  :ensure t
  :init
  (evil-mode 1)
  (use-package evil-surround
    :ensure t
    :init
    (global-evil-surround-mode 1))
  (use-package evil-matchit
    :ensure t
    :init
    (global-evil-matchit-mode 1)))
(provide 'setup-evil)
