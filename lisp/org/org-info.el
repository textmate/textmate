;;; org-info.el --- Support for links to Info nodes from within Org-Mode

;; Copyright (C) 2004-2012 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file implements links to Info nodes from within Org-mode.
;; Org-mode loads this module by default - if this is not what you want,
;; configure the variable `org-modules'.

;;; Code:

(require 'org)

;; Declare external functions and variables

(declare-function Info-find-node "info" (filename nodename
						  &optional no-going-back))
(defvar Info-current-file)
(defvar Info-current-node)

;; Install the link type
(org-add-link-type "info" 'org-info-open)
(add-hook 'org-store-link-functions 'org-info-store-link)

;; Implementation
(defun org-info-store-link ()
  "Store a link to an Info file and node."
  (when (eq major-mode 'Info-mode)
    (let (link desc)
      (setq link (org-make-link "info:"
				(file-name-nondirectory Info-current-file)
				"#" Info-current-node))
      (setq desc (concat (file-name-nondirectory Info-current-file)
			 "#" Info-current-node))
      (org-store-link-props :type "info" :file Info-current-file
			    :node Info-current-node
			    :link link :desc desc)
      link)))

(defun org-info-open (path)
  "Follow an Info file and node link specified by PATH."
  (org-info-follow-link path))


(defun org-info-follow-link (name)
  "Follow an Info file and node link specified by NAME."
  (if (or (string-match "\\(.*\\)[#:]:?\\(.*\\)" name)
          (string-match "\\(.*\\)" name))
      (progn
	(require 'info)
        (if (match-string 2 name) ; If there isn't a node, choose "Top"
            (Info-find-node (match-string 1 name) (match-string 2 name))
          (Info-find-node (match-string 1 name) "Top")))
    (message "Could not open: %s" name)))

(provide 'org-info)

;;; org-info.el ends here
