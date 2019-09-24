;; -*- lexical-binding: t; -*-
(require 'use-package)


(use-package smartparens-config
  :ensure smartparens
  :diminish smartparens-mode
  :config
  (smartparens-global-mode t)
  (sp-use-smartparens-bindings)
  (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
  (add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode))

(use-package eldoc
  :diminish eldoc
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'eldoc-mode))

(use-package subword
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'subword-mode))

(use-package yasnippet
  :ensure t
  :diminish yasnippet
  :init
  (yas-global-mode)
  :config
  (use-package yasnippet-snippets
    :ensure t
    :init
    (yas-reload-all)))

(use-package whitespace
  :ensure t
  :init
  (add-hook 'before-save-hook 'whitespace-cleanup))


(use-package flymake-quickdef
  :ensure t
  :init
  (flymake-quickdef-backend flymake-check-joker
                            :pre-let ((joker-exec (executable-find "joker")))
                            :pre-check (unless joker-exec (error "Cannot find joker executable"))
                            :write-type 'file
                            :proc-form (list joker-exec "--lint" fmqd-temp-file)
                            :search-regexp "^.+:\\([[:digit:]]+\\):\\([[:digit:]]+\\): Parse \\([[:alpha:]]+\\): \\(.+\\)$"
                            :prep-diagnostic
                            (let* ((lnum (string-to-number (match-string 1)))
                                   (lcol (string-to-number (match-string 2)))
                                   (severity (match-string 3))
                                   (msg (match-string 4))
                                   (pos (flymake-diag-region fmqd-source lnum lcol))
                                   (beg (car pos))
                                   (end (cdr pos))
                                   (type (cond
                                          ((string= severity "error") :error)
                                          ((string= severity "warning") :warning)
                                          ((string= severity "Exception") :error)
                                          (t :note))))
                              (list fmqd-source beg end type msg)))

  (defun clojure-setup-flymake-backend ()
    (flymake-mode)
    (add-hook 'flymake-diagnostic-functions 'flymake-check-joker nil t))

  (add-hook 'clojure-mode-hook 'clojure-setup-flymake-backend))


(provide 'setup-programing)
