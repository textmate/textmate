;;; org-docview.el --- support for links to doc-view-mode buffers

;; Copyright (C) 2009-2012  Free Software Foundation, Inc.

;; Author: Jan Böcker <jan.boecker at jboecker dot de>
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

;; This file implements links to open files in doc-view-mode.
;; Org-mode loads this module by default - if this is not what you want,
;; configure the variable `org-modules'.

;; The links take the form
;;
;;    docview:<file path>::<page number>
;;
;; for example: [[docview:~/.elisp/org/doc/org.pdf::1][Org-Mode Manual]]
;;
;; Autocompletion for inserting links is supported; you will be
;; prompted for a file and a page number.
;;
;; If you use org-store-link in a doc-view mode buffer, the stored
;; link will point to the current page.

;;; Code:


(require 'org)

(declare-function doc-view-goto-page "ext:doc-view" (page))
(declare-function image-mode-window-get "ext:image-mode"
		  (prop &optional winprops))

(autoload 'doc-view-goto-page "doc-view")

(org-add-link-type "docview" 'org-docview-open)
(add-hook 'org-store-link-functions 'org-docview-store-link)

(defun org-docview-open (link)
  (when (string-match "\\(.*\\)::\\([0-9]+\\)$"  link)
    (let* ((path (match-string 1 link))
	   (page (string-to-number (match-string 2 link))))
      (org-open-file path 1) ;; let org-mode open the file (in-emacs = 1)
      ;; to ensure org-link-frame-setup is respected
      (doc-view-goto-page page)
      )))

(defun org-docview-store-link ()
  "Store a link to a docview buffer."
  (when (eq major-mode 'doc-view-mode)
    ;; This buffer is in doc-view-mode
    (let* ((path buffer-file-name)
	   (page (image-mode-window-get 'page))
	   (link (concat "docview:" path "::" (number-to-string page)))
	   (description ""))
      (org-store-link-props
       :type "docview"
       :link link
       :description path))))

(defun org-docview-complete-link ()
  "Use the existing file name completion for file.
Links to get the file name, then ask the user for the page number
and append it."
  (concat (replace-regexp-in-string "^file:" "docview:" (org-file-complete-link))
	  "::"
	  (read-from-minibuffer "Page:" "1")))


(provide 'org-docview)

;;; org-docview.el ends here
