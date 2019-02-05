(require 'use-package)



(use-package sane-term
  :ensure t
  :bind (("C-x t" . sane-term)
         ([f5] . sane-term-create))
  :config
  (defun config-term ()
    (yas-minor-mode -1)
    (define-key term-raw-map (kbd "M-o") 'ace-window)
    (define-key term-raw-map (kbd "M-[") 'sane-term-next)
    (define-key term-raw-map (kbd "M-]") 'sane-term-prev)
    (setq show-trailing-whitespace nil))

  (add-hook 'term-mode-hook 'config-term))

(provide 'setup-term)
