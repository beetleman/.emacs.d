(defun clojure-mode-config (hook)
  (add-hook hook #'subword-mode)           
  (add-hook hook #'smartparens-mode)       
  (add-hook hook #'rainbow-delimiters-mode)
  (add-hook hook #'eldoc-mode)             
  (add-hook hook #'idle-highlight-mode))


(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljc\\'" . clojure-mode)
         ("\\.cljs\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode))
  :init
  (clojure-mode-config 'clojure-mode-hook))


(use-package cider-repl-mode
  :init
  (clojure-mode-config 'cider-repl-mode-hook))

(use-package cider
  :ensure t
  :defer t
  :init (add-hook 'cider-mode-hook #'clj-refactor-mode)
  :diminish subword-mode
  :config
  (setq nrepl-log-messages t                  
        cider-repl-display-in-current-window t
        cider-repl-use-clojure-font-lock t    
        cider-prompt-save-file-on-load 'always-save
        cider-font-lock-dynamically '(macro core function var)
        nrepl-hide-special-buffers t            
        cider-overlays-use-font-lock t
        cider-cljs-lein-repl "(do (require 'figwheel-sidecar.repl-api)
                                  (figwheel-sidecar.repl-api/start-figwheel!)
                                  (figwheel-sidecar.repl-api/cljs-repl))")         
  (cider-repl-toggle-pretty-printing))




(use-package cider-eval-sexp-fu
  :defer t)

(use-package clj-refactor
  :defer t
  :ensure t
  :diminish clj-refactor-mode
  :config (cljr-add-keybindings-with-prefix "C-c C-m"))

(use-package smartparens
  :defer t
  :ensure t
  :diminish smartparens-mode
  :init
  (setq sp-override-key-bindings
        '(("C-<right>" . nil)
          ("C-<left>" . nil)
          ("C-)" . sp-forward-slurp-sexp)
          ("M-<backspace>" . nil)
          ("C-(" . sp-forward-barf-sexp)))
  :config
  (use-package smartparens-config)
  (sp-use-smartparens-bindings)
  (sp--update-override-key-bindings)
  :commands (smartparens-mode show-smartparens-mode))

(provide 'my-clojure)
