(require 'use-package)

(use-package multiple-cursors
  :bind (("C-|" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)))

(provide 'setup-multiple-cursors)
