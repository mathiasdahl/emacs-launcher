
;;; emacs-launcher.el --- Emacs-based launcher

;; Copyright (C) 2009, 2010, 2011, 2012, 2013, 2014 Mathias Dahl

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(defvar emacs-launcher-installation-directory
  (file-name-directory load-file-name))

(defvar emacs-launcher-frame (selected-frame))

;;(require 'cl)

(load-file "emacs-launcher-server.el")
(load-file "emacs-launcher-looks.el")
(load-file "emacs-launcher-file-dropper.el")
(load-file "emacs-launcher-functions.el")
(load-file "anything.el")
(load-file "emacs-launcher-anything-settings.el")
(load-file "emacs-launcher-sources.el")

(defun emacs-launcher ()
  "Main launcher."
  (interactive)
  (let ((anything-sources
         (list
          anything-c-source-special
          anything-c-source-commands
          anything-c-source-bookmarks
          anything-c-source-clips
          anything-c-source-common-folders
          anything-c-source-urls)))
        (call-interactively 'anything))
  (iconify-frame emacs-launcher-frame))

;; Go go go!
(emacs-launcher)

(provide 'emacs-launcher)

;;; emacs-launcher.el ends here

