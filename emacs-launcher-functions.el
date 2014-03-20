(defun my-browser (url &optional new-window)
  (w32-shell-execute "open" url))

(setq browse-url-browser-function 'my-browser)

(defun get-common-url-from-name (name)
  "Get URL from NAME."
  (let ((url (first (rassoc name (url-urls-names)))))
    (if (string-match "^\\(https?://\\|mailto:\\)" url)
        url
      (concat "http://" url))))

(defun open-common-url-from-name (name)
  "Open URL from NAME."
  (browse-url (get-common-url-from-name name)))

(defun edit-and-open-common-url-from-name (name)
  "Edit and open URL from NAME."
  (browse-url (read-string "URL: " (get-common-url-from-name name))))

(defun terminal-run (command &rest args)
  "Run COMMAND with optional ARGS in terminal."
  (apply 'start-process (append (list "rxvt" "*rxvt*" "rxvt" "-e" command) args)))

(defun run (command &rest args)
  "Run COMMAND with optional ARGS."
  (when (and command (not (string= command "")))
    ;; w32-shell-execute only supports parameters as one string, not
    ;; several, so we must concat multiple parameters into one string.
    (apply 'w32-shell-execute (append (list "open" command (mapconcat 'identity args " "))))))

(defun get-clip-if-match (regexp)
  (let ((clip (current-kill 0)))
    (if (string-match regexp clip)
	(match-string 0 clip)
      nil)))

(defun run-command (name)
  "Run pre-defined command NAME.  See `list-commands'."
  (let ((stuff (cdr (assoc name (list-commands)))))
    (cond ((stringp stuff)
           (run stuff))
          ((eq (car stuff) 'lambda)
           (funcall stuff))
          ((and (listp stuff)
                (eq 'clip (car stuff)))
           (kill-new (cadr stuff)))
           ((and (listp stuff)
                (stringp (car stuff))
                (string-match "https?://.*%s" (car stuff))
                (stringp (cadr stuff)))
	   (let ((default (if (nth 2 stuff)
			      (get-clip-if-match (nth 2 stuff)))))
	     (browse-url (format (nth 0 stuff) (read-string 
						(if default
						    (format "%s [%s]: " (nth 1 stuff) default)
						  (format "%s: " (nth 1 stuff)))
						nil nil default)))))
          ((and (listp stuff)
                (stringp (car stuff)))
           (apply 'run (append (list (car stuff)) (cdr stuff))))
          ((and (listp stuff)
                (eq (car stuff) 'tr))
           (apply 'terminal-run (cadr stuff) (cddr stuff)))
          (t
           (error "Don't know how to run %s" name)))
    (with-temp-buffer
      (insert name "\n")
      (append-to-file
       (point-min) (point-max)
       (format "%s.anything-launcher-history" emacs-launcher-installation-directory)))))

(defun add-file-from-drag-and-drop ()
  (interactive)
  (emacs-launcher-file-dropper
   (lambda (file)
     (let ((command-file (format "%s.commands" emacs-launcher-installation-directory))
           (command-name (read-string "Command name: " file)))
       (with-temp-file command-file
         (insert-file-contents command-file)
         (goto-char (point-max))
         (backward-sexp)
         (forward-sexp)
         (backward-char)
         (insert (format "\n(\"%s\" . \"%s\")\n"
                         (replace-regexp-in-string "\\\\" "\\\\\\\\"
                                                   command-name)
                         (replace-regexp-in-string "\\\\" "\\\\\\\\" file))))
       (message "Command added.")))))

(defun get-page-title (url)
  (save-excursion
    (set-buffer (url-retrieve-synchronously url))
    (goto-char (point-min))
    (if (search-forward-regexp "<title>\\([^<]+\\)</title>" nil t)
        (match-string 1)
      "No title")))

(defun url-lines ()
  "List URL lines."
  (with-temp-buffer
    (insert-file-contents (format "%s.common-urls"
				  emacs-launcher-installation-directory))
    (split-string (buffer-substring-no-properties
                   (point-min) (point-max)) "\n" t)))

(defun url-urls-names ()
  "Extract URLs and their names."
  (mapcar
   (lambda (x)
     (let ((y (split-string x "|")))
       (cons (first y) (second y))))
   (url-lines)))

(defun url-names ()
  "Extract URL names."
  (mapcar
   (lambda (x)
     (second
      (split-string x "|")))
   (url-lines)))

(defun list-commands ()
  "Various commands.

Commands are read from the file defined by the .commands files
which should contain an list of different items:

A simple string runs that command.

A list of strings run the first string as the command and uses
the rest as arguments.

A lambda results in a `funcall' of that lambda.

A list starting with `tr' will run the strings as a command in a
terminal."

  (with-temp-buffer
    (insert-file-contents
     (format "%s.commands" emacs-launcher-installation-directory))
    (read (current-buffer))))

(defun get-clips ()
  (let ((clips
         (with-temp-buffer
           (insert-file-contents
	    (format "%s.clips" emacs-launcher-installation-directory))
           (read (current-buffer)))))
    clips))

(defun get-clip (clip)
  (kill-new (cdr (assoc clip (get-clips)))))

(defun add-clip (name text)
  (let ((clips (get-clips)))
    (add-to-list 'clips (cons name text))
    (with-temp-file ".clips"
      (insert (prin1-to-string clips)))))

(defun truncate-string (stuff len)
  (substring stuff 0 (min len (length stuff))))

(defun slash-to-backslash (text)
  (substitute ?\\ ?/ text))

(defun open-bookmark (bookmark)
  (w32-shell-execute "open" (slash-to-backslash (bookmark-get-filename bookmark))))

(defun edit-and-open-bookmark (bookmark)
  (w32-shell-execute 
   "open" (read-file-name "Open: " (slash-to-backslash (bookmark-get-filename bookmark)))))

(defun anything-c-flexible-match (candidate)
  "Flexible/fuzzy matching"
  (let ((flexible-regexp (mapconcat #'regexp-quote (split-string anything-pattern "") ".*")))
    (string-match flexible-regexp (file-name-nondirectory candidate))))

(defun hide-launcher ()
  (interactive)
  (suspend-frame))

(defun anything-url-with-prompt (url prompt)
  (browse-url (format url (read-string prompt))))

(defun folder-open-explorer (folder)
  (w32-shell-execute "open" folder))

(defun folder-open-cmd (folder)
  (w32-shell-execute "open" "cmd" (format "/k cd /d %s" folder)))

