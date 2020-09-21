(use-package company
  :bind (("M-i" . company-complete)
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :init
  (setq company-idle-delay 0.5)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  :config (global-company-mode t))

(use-package company-quickhelp
  :init
  (setq company-quickhelp-delay nil)
  :config
  (company-quickhelp-mode))

(provide 'setup-company)
