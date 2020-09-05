(require 'use-package)

(use-package smex)

(use-package swiper
  :bind
  ("C-s" . swiper))

(use-package ivy
  :diminish ivy-mode
  :bind
  (("C-c C-r" . ivy-resume)
   ("<f6>" . ivy-resume))
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (ivy-mode 1))

(use-package counsel
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


(use-package ivy-xref
  :init
  ;; xref initialization is different in Emacs 27 - there are two different
  ;; variables which can be set rather than just one
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
  ;; commands other than xref-find-definitions (e.g. project-find-regexp)
  ;; as well
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(provide 'setup-ivy)
