;; Not for me, but some might like to be able to start commands with the number keys
;; (setq anything-enable-digit-shortcuts t)

;; Make sure some sources do not take up too much space
(setq anything-candidate-number-limit 10)

;; Use the full Emacs window for the list of commands
(setq anything-samewindow t)

(defface anything-header 
    '((t (:background "black" :foreground "yellow" :weight bold))) 
    "Face for header lines in the anything buffer." :group 'anything)
