(require 'use-package)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "/usr/bin/markdown_py-3 -x gfm")
  (let ((gfm-css (file-truename "./setup-markdown.css")))
    (when (file-exists-p gfm-css)
      (setq markdown-css-paths `(,gfm-css)))))

(provide 'setup-markdown)
