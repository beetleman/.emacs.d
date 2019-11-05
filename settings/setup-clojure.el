;; -*- lexical-binding: t; -*-

(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljc\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\.edn\\'" . clojure-mode)))

(use-package cider
  :ensure t
  :defer t
  :init
  (cider-auto-test-mode 1)
  (add-hook 'cider-mode-hook #'clj-refactor-mode)
  (add-hook 'cider-mode-hook #'smartparens-mode)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  :diminish subword-mode)

(use-package cider-eval-sexp-fu
  :ensure t
  :defer t)

(use-package clj-refactor
  :defer t
  :ensure t
  :diminish clj-refactor-mode)

(use-package flymake-quickdef
  :ensure t
  :init
  (setf flymake-check-joker-regexp
        "^.+:\\([[:digit:]]+\\):\\([[:digit:]]+\\): \\([[:alpha:]\\ ]+\\): \\(.+\\)$")

  (defun severity-to-type (severity)
    (cond
     ((string= severity "Read error") :error)

     ((string= severity "Parse warning") :warning)
     ((string= severity "Parse error") :error)

     ((string= severity "Exception") :error)

     (t :note)))

  (defmacro defn-joker-backend (name dialect)
    `(flymake-quickdef-backend ,name
       :pre-let ((joker-exec (executable-find "joker")))
       :pre-check (unless joker-exec (error "Cannot find joker executable"))
       :write-type 'pipe
       :proc-form (list joker-exec "--lint" "--dialect" ,dialect "-")
       :search-regexp flymake-check-joker-regexp
       :prep-diagnostic
       (let* ((lnum (string-to-number (match-string 1)))
              (lcol (string-to-number (match-string 2)))
              (severity (match-string 3))
              (msg (match-string 4))
              (pos (flymake-diag-region fmqd-source lnum lcol))
              (beg (car pos))
              (end (cdr pos))
              (type (severity-to-type severity)))
         (list fmqd-source beg end type msg))))

  (defn-joker-backend flymake-check-joker-clj "clj")
  (defn-joker-backend flymake-check-joker-edn "edn")
  (defn-joker-backend flymake-check-joker-cljs "cljs")

  (defun setup-flymake-backend-clj ()
    (let ((ext (file-name-extension (buffer-file-name))))
      (when (not (string= "cljs" ext))
        (flymake-mode 1)
        (if (string= "edn" ext)
            (add-hook 'flymake-diagnostic-functions 'flymake-check-joker-edn nil t)
          (add-hook 'flymake-diagnostic-functions 'flymake-check-joker-clj nil t)))))

  (defun setup-flymake-backend-cljs ()
    (flymake-mode 1)
    (add-hook 'flymake-diagnostic-functions 'flymake-check-joker-cljs nil t))

  (add-hook 'clojure-mode-hook 'setup-flymake-backend-clj)
  (add-hook 'clojurescript-mode-hook 'setup-flymake-backend-cljs))

(provide 'setup-clojure)
