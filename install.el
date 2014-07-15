
(defvar emacs-launcher-source-directory (file-name-directory load-file-name))

(defun emacs-launcher-install ()
  (if (not emacs-launcher-source-directory)
      (message "You must use M-x load-file to use this installer!")
    (when (y-or-n-p "This will setup Emacs Launcher. Do you want to continue?")
      (message "Installing...")
      (let* ((installation-directory
              (file-name-as-directory
               (read-directory-name "Where do you want to install Emacs Launcher: "))))
        (if (not (file-exists-p installation-directory))
            (if (not (y-or-n-p "Installation directory does not exist. Do you want to create it?"))
                (error "Installation aborted.")
              (make-directory installation-directory)))
	(dolist (file (directory-files emacs-launcher-source-directory t "\\(\\.el\\|\\.txt\\|^.\\(clips\\|commands\\|common-folders\\|common-urls\\)\\)$"))
	  (copy-file file installation-directory t))
        (emacs-launcher-install-create-emacs-launcher-script installation-directory)
        (emacs-launcher-install-create-runemacs-script installation-directory)
        (when (y-or-n-p 
"Installation done. Make sure you read the README.txt file for
further instructions on how to use the launcher.

Do you want to open the installation folder (recommended)?")
          (w32-shell-execute "Open" installation-directory))))))

(defun emacs-launcher-install-create-runemacs-script (installation-directory)
  (with-temp-file (format "%s/runemacs.cmd" installation-directory)
    (insert (format
             "@echo off
%srunemacs.exe -Q --load %semacs-launcher.el"
             (replace-regexp-in-string "/" "\\\\" invocation-directory)
             (replace-regexp-in-string "/" "\\\\" installation-directory)
             ))))

(defun emacs-launcher-install-create-emacs-launcher-script (installation-directory)
  (let ((w32-installation-directory
        (replace-regexp-in-string "/" "\\\\" installation-directory)))
    (with-temp-file (format "%semacs-launcher.cmd" installation-directory)
      (insert (format
             "@echo off
%semacsclient -q -n -f %semacs-launcher-server --eval (emacs-launcher) -a \"%srunemacs.cmd\""
             (replace-regexp-in-string "/" "\\\\" invocation-directory)
             w32-installation-directory
             w32-installation-directory
             )))))

(emacs-launcher-install)
