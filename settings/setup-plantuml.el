(require 'use-package)

(use-package plantuml-mode
  :ensure t
  :mode "\\.plantuml\\'"
  :init
  (setq plantuml-default-exec-mode 'executable)
  (setq plantuml-output-type "svg"))


(provide 'setup-plantuml)
