
(defvar emacs-launcher-file-dropper-frame nil
  "Frame for dropping files")

(defvar emacs-launcher-file-dropper-drop-function 'message)

(defun emacs-launcher-file-dropper-drag-n-drop (event)
  "Run file-dropper on the files listed in the drag-n-drop event."
  (interactive "e")
  (save-excursion
    ;; Make sure the drop target has positive co-ords before setting
    ;; the selected frame - otherwise it won't work.
    (let* ((window (posn-window (event-start event)))
           (coords (posn-x-y (event-start event)))
           (x (car coords))
           (y (cdr coords)))
      (mapcar emacs-launcher-file-dropper-drop-function (car (cdr (cdr event)))))))

(defvar emacs-launcher-file-dropper-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m [drag-n-drop] 'emacs-launcher-file-dropper-drag-n-drop)
    (define-key m "q" 'emacs-launcher-file-dropper-delete-frame)
    m)
  "Keymap for emacs-launcher-file-dropper-mode")

(defun emacs-launcher-file-dropper-delete-frame ()
  (interactive)
  (kill-buffer "*emacs-launcher-file-dropper-drag-n-drop*")
  (delete-frame emacs-launcher-file-dropper-frame))

(defun emacs-launcher-file-dropper-mode ()
  "Mode for dropping files for file-dropper to swallow
\\{emacs-launcher-file-dropper-mode-map}"
  (interactive)  
  (kill-all-local-variables)  
  (setq major-mode 'emacs-launcher-file-dropper-mode)
  (setq mode-name "file-dropper")
  (setq mode-line-format nil)
  (force-mode-line-update)
  (use-local-map emacs-launcher-file-dropper-mode-map)    
  (run-hooks 'emacs-launcher-file-dropper-mode-hook))

(defun emacs-launcher-file-dropper-create-and-raise-frame ()
  (if (or
       (not emacs-launcher-file-dropper-frame)
       (not (frame-live-p emacs-launcher-file-dropper-frame)))
      (setq emacs-launcher-file-dropper-frame 
            (make-frame
             '((name . "emacs-launcher-file-dropper-drag-n-drop-frame")
               (minibuffer . t)
               (user-position . t)
               (width . 100)
               (height . 6)
               (vertical-scroll-bars . nil)
               (menu-bar-lines . 0)))))  
  (raise-frame emacs-launcher-file-dropper-frame)
  (select-frame emacs-launcher-file-dropper-frame))

(defun emacs-launcher-file-dropper (function)
  (interactive)
  (setq emacs-launcher-file-dropper-drop-function function)
  (emacs-launcher-file-dropper-create-and-raise-frame)
  (if (not (string-equal (buffer-name) "*emacs-launcher-file-dropper-drag-n-drop*"))
      (switch-to-buffer  "*emacs-launcher-file-dropper-drag-n-drop*"))  
  (setq buffer-read-only nil)
  (kill-region (point-min) (point-max))
  (insert "\nDrop your files here to add new commands to your .command file. The command will be the file you dropped. You will be asked to provide a command name which by default is the same name as the name of the file that was dropped.\nTo quit, close window or type `q'\n")
  (forward-line -1)
  (fill-paragraph)
  (recenter)
  (end-of-line)
  (backward-char 2)
  (setq buffer-read-only t)
  (emacs-launcher-file-dropper-mode))

(provide 'emacs-launcher-file-dropper)
