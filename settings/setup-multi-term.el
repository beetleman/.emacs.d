(require 'use-package)

(use-package multi-term
  :ensure t
  :init
  (global-set-key [f5] 'multi-term)
  :config
  (defun config-term ()
      (yas-minor-mode -1)

      (define-key term-raw-map (kbd "M-o") 'ace-window)
      (define-key term-mode-map (kbd "C-c C-k")
        'term-char-mode)
      (define-key term-raw-map (kbd "C-c C-j")
        'term-line-mode)

      (setq term-buffer-maximum-size 0)  ;; means no limitation.
      (add-to-list 'term-bind-key-alist '("M-[" . multi-term-prev))
      (add-to-list 'term-bind-key-alist '("M-]" . multi-term-next))
      (setq show-trailing-whitespace nil))

  (add-hook 'term-mode-hook 'config-term))

(provide 'setup-multi-term)
