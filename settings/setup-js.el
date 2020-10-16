(require 'use-package)

(use-package add-node-modules-path
  :hook (js-mode . add-node-modules-path))

(use-package prettier-js
  :hook (js-mode . prettier-js-mode))

(use-package vue-mode
  :mode "\\.vue\\'"
  :init
  (setq mmm-submode-decoration-level 0))


(use-package web-mode
  :mode "\\.jsx\\'")


(provide 'setup-js)
