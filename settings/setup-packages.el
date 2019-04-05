(progn
  (require 'package)
  (package-initialize t)

  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/"))

  (defvar my-packages '(use-package
                        diminish))

  (defvar packages-list-updated nil)
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (when (not packages-list-updated)
        (setq packages-list-updated t)
        (package-refresh-contents))
      (package-install p))))

(provide 'setup-packages)
