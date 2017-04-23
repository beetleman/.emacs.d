(progn
  (require 'package)
  (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

  (defvar my-packages '(better-defaults 
                        use-package
                        diminish
                        scpaste))

  (package-initialize t)
  (defvar packages-list-updated nil)
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (when (not packages-list-updated)
        (setq packages-list-updated t)
        (package-refresh-contents))
      (package-install p))))

(provide 'setup-packages)
