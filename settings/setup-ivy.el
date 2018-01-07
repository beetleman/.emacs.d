(require 'use-package)

(use-package smex
  :ensure t)

(use-package swiper
  :ensure t
  :bind
  ("C-s" . swiper))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :bind
  (("C-c C-r" . ivy-resume)
   ("<f6>" . ivy-resume))
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (ivy-mode 1))

(use-package counsel
  :ensure t
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("<f1> f" . counsel-describe-function)
   ("<f1> v" . counsel-describe-variable)
   ("<f1> l" . counsel-find-library)
   ("<f2> i" . counsel-info-lookup-symbol)
   ("<f2> u" . counsel-unicode-char)
   ("C-c g" . counsel-git)
   ("C-c k" . counsel-ag)
   ("C-x l" . counsel-locate)
   ("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-p" . ivy-next-line)
   :map read-expression-map
   ("C-r" . counsel-expression-history))
  :config
  (when (fboundp 'ido-mode)
    (ido-mode -1))
  (ivy-mode 1))

(provide 'setup-ivy)
