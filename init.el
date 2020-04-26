;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Settings
(defvar settings-dir
  (expand-file-name "settings" user-emacs-directory))
(add-to-list 'load-path settings-dir)

(require 'settings)

;; force server socket file location
; (setq server-socket-dir "~/.emacs.d/server")

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote nil))
 '(delete-selection-mode nil)
 '(package-selected-packages
   (quote
    (zprint-mode yasnippet-snippets ws-butler which-key use-package undo-tree smex smartparens smart-mode-line slime-company sane-term rjsx-mode reason-mode rainbow-mode pyenv-mode py-isort prettier-js pipenv org-bullets neotree magit lsp-mode geiser flycheck-clj-kondo eziam-theme emmet-mode editorconfig diminish diff-hl dashboard counsel-projectile company-restclient company-quickhelp clj-refactor cider-eval-sexp-fu cargo blacken better-defaults anzu all-the-icons add-node-modules-path ace-window)))
 '(safe-local-variable-values
   (quote
    ((zprint-mode)
     (cider-clojure-cli-global-options . "-A:dev")
     (cider-clojure-cli-global-options . "-A:rebl")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
