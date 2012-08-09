;;; nndir.el --- single directory newsgroup access for Gnus

;; Copyright (C) 1995-2012 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news

;; This file is part of GNU Emacs.

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

;;; Commentary:

;;; Code:

(require 'nnheader)
(require 'nnmh)
(require 'nnml)
(require 'nnoo)
(eval-when-compile (require 'cl))

(nnoo-declare nndir
  nnml nnmh)

(defvoo nndir-directory nil
  "Where nndir will look for groups."
  nnml-current-directory nnmh-current-directory)

(defvoo nndir-nov-is-evil nil
  "*Non-nil means that nndir will never retrieve NOV headers."
  nnml-nov-is-evil)



(defvoo nndir-current-group "" nil nnml-current-group nnmh-current-group)
(defvoo nndir-top-directory nil nil nnml-directory nnmh-directory)
(defvoo nndir-get-new-mail nil nil nnml-get-new-mail nnmh-get-new-mail)

(defvoo nndir-status-string "" nil nnmh-status-string)
(defconst nndir-version "nndir 1.0")



;;; Interface functions.

(nnoo-define-basics nndir)

(deffoo nndir-open-server (server &optional defs)
  (setq nndir-directory
	(or (cadr (assq 'nndir-directory defs))
	    server))
  (unless (assq 'nndir-directory defs)
    (push `(nndir-directory ,server) defs))
  (push `(nndir-current-group
	  ,(file-name-nondirectory (directory-file-name nndir-directory)))
	defs)
  (push `(nndir-top-directory
	  ,(file-name-directory (directory-file-name nndir-directory)))
	defs)
  (nnoo-change-server 'nndir server defs)
  (let (err)
    (cond
     ((not (condition-case arg
	       (file-exists-p nndir-directory)
	     (ftp-error (setq err (format "%s" arg)))))
      (nndir-close-server)
      (nnheader-report
       'nndir (or err "No such file or directory: %s" nndir-directory)))
     ((not (file-directory-p (file-truename nndir-directory)))
      (nndir-close-server)
      (nnheader-report 'nndir "Not a directory: %s" nndir-directory))
     (t
      (nnheader-report 'nndir "Opened server %s using directory %s"
		       server nndir-directory)
      t))))

(nnoo-map-functions nndir
  (nnml-retrieve-headers 0 nndir-current-group 0 0)
  (nnml-request-article 0 nndir-current-group 0 0)
  (nnmh-request-group nndir-current-group 0 0)
  (nnml-close-group nndir-current-group 0)
  (nnml-request-list (nnoo-current-server 'nndir) nndir-directory)
  (nnml-request-newsgroups (nnoo-current-server 'nndir) nndir-directory))

(provide 'nndir)

;;; nndir.el ends here
