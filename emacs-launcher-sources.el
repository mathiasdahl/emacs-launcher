(defvar anything-c-source-common-folders
  '((name . "Common Folders")
    (candidates . (lambda ()
                    (with-temp-buffer
                      (insert-file-contents
		       (format "%s.common-folders"
			       emacs-launcher-installation-directory))
                      (split-string (buffer-substring-no-properties
                                     (point-min) (point-max)) "\n" t))))
    (action . (("Open with Windows Explorer" . folder-open-explorer)
               ("Open command window in folder" . folder-open-explorer)))))

(defvar anything-c-source-urls
 '((name . "Web Sites")
   (candidates . url-names)
   (action . (("Open URL" . open-common-url-from-name)
              ("Edit and Open URL" . edit-and-open-common-url-from-name)))))

;; Alternate version with flexible matching
(defvar anything-c-source-urls
 '((name . "Web Sites")
   (candidates . url-names)
   (match . (anything-c-flexible-match))
   (action . (("Open URL" . open-common-url-from-name)
              ("Edit and Open URL" . edit-and-open-common-url-from-name)))))

(defvar anything-c-source-commands
 '((name . "Commands")
   (candidates . (lambda () (mapcar 'car (list-commands))))
   (action . (("Execute command" . run-command)))))

(defvar anything-c-source-clips
 '((name . "Clips")
   (candidates . (lambda () (mapcar 'car (get-clips))))
   (action . (("Copy clip to clipboard" . get-clip)))))

(defvar anything-c-source-bookmarks
 '((name . "Bookmarks in Emacs")
   (init . (lambda () (require 'bookmark)))
   (candidates . bookmark-all-names)
   (action . (("Open" . open-bookmark)
	      ("Edit and open" . edit-and-open-bookmark)))))

(defvar anything-c-source-special
 '((name . "Special Commands")
   (candidates . ("Cancel" "Quit"))
   (action . (("Cancel" .
               (lambda (x)
                 (cond ((string= x "Cancel")
                        (message "Canceled" x))
                       ((string= x "Quit")
                        (let ((kill-emacs-hook nil))
                          (kill-emacs))))))))))
