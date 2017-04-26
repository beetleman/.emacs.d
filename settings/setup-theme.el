(require 'use-package)
(use-package jbeans-theme
  :defer t
  :ensure t
  :init
  (load-theme 'jbeans t)
  (global-hl-line-mode)
  (add-to-list 'default-frame-alist '(font . "mononoki-12")))

(provide 'setup-theme)
