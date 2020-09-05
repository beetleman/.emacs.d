(use-package restclient)
(use-package company-restclient
  :after (restclient)
  :init
  (add-to-list 'company-backends 'company-restclient))


(provide 'setup-restclient)
