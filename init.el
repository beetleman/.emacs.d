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
