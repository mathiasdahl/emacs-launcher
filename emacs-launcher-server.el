;; Use our own server file for emacsclient so that we don't interfer
;; with other Emacs instances

(setq server-name "emacs-launcher-server")
(setq server-auth-dir emacs-launcher-installation-directory)
(server-start)
