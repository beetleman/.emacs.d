(use-package restclient
  :ensure t
  :init
  (use-package company-restclient
    :ensure t
    :init
    (add-to-list 'company-backends 'company-restclient)))


(provide 'setup-restclient)
