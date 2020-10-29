(require 'use-package)

(use-package web-mode
  :mode ("\\.js[x]?\\'" "\\.vue\\'")
  :config
  (setq web-mode-content-types-alist
  '(("jsx"  . "\\.js[x]?\\'"))))

(provide 'setup-js)
