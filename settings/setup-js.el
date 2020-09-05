(require 'use-package)

(use-package add-node-modules-path
  :hook (js-mode . add-node-modules-path))

(use-package prettier-js
  :hook (js-mode . prettier-js-mode))

(use-package vue-mode
  :mode "\\.vue\\'"
  :init
  (add-hook 'mmm-mode-hook
            (lambda ()
              (set-face-background 'mmm-default-submode-face nil))))

(provide 'setup-js)
