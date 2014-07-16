(set-frame-name "Emacs Launcher")
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Setup some text for the user to look at in the *scratch* buffer if things fail...

(erase-buffer)

(insert "

Hi!

Don't be afraid...

If the launcher fails to hide this window, just minimize it,
close it (i.e kill Emacs), or type one of the following keys:

h - hide launcher
o - open launcher and then hide
q - quit launcher

")

(toggle-read-only)

;; Some useful keys to have in the scratch buffer
(local-set-key (kbd "o") 'emacs-launcher)
(local-set-key (kbd "h") 'hide-launcher)
(local-set-key (kbd "q") 'save-buffers-kill-terminal)

;; So that we can paste stuff at the anything prompt
(global-set-key (kbd "C-v") 'yank)

;;(add-to-list 'load-path "c:/Users/mdahse/Dropbox/work/home/newlisp")

;; Set some beautiful colors...
(set-background-color "black")
(set-foreground-color "green")
(set-cursor-color "red")

(set-face-attribute 'default (selected-frame)
			  :width 'normal
			  :weight 'normal
			  :slant 'normal)

;; Change some faces to match the colors better.
(set-face-attribute 'highlight nil :background "chocolate")

(defun setup-frame ()
  (modify-frame-parameters
   emacs-launcher-frame
   '((width . 180)
     (height . 30)
     (top . 200)
     (left . 200))))

(setup-frame)
