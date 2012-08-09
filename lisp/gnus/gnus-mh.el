;;; gnus-mh.el --- mh-e interface for Gnus

;; Copyright (C) 1994-2012 Free Software Foundation, Inc.

;; Author: Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>
;;	Lars Magne Ingebrigtsen <larsi@gnus.org>
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

;;; Send mail using mh-e.

;; The following mh-e interface is all cooperative works of
;; tanaka@flab.fujitsu.CO.JP (TANAKA Hiroshi), kawabe@sra.CO.JP
;; (Yoshikatsu Kawabe), and shingu@casund.cpr.canon.co.jp (Toshiaki
;; SHINGU).

;;; Code:

(require 'gnus)
(require 'mh-e)
(require 'mh-comp)
(require 'gnus-msg)
(require 'gnus-sum)

(defvar mh-lib-progs)

(defun gnus-summary-save-article-folder (&optional arg)
  "Append the current article to an mh folder.
If N is a positive number, save the N next articles.
If N is a negative number, save the N previous articles.
If N is nil and any articles have been marked with the process mark,
save those articles instead."
  (interactive "P")
  (require 'gnus-art)
  (let ((gnus-default-article-saver 'gnus-summary-save-in-folder))
    (gnus-summary-save-article arg)))

(defun gnus-summary-save-in-folder (&optional folder)
  "Save this article to MH folder (using `rcvstore' in MH library).
Optional argument FOLDER specifies folder name."
  ;; Thanks to yuki@flab.Fujitsu.JUNET and ohm@kaba.junet.
  (mh-find-path)
  (let ((folder
	 (cond ((and (eq folder 'default)
		     gnus-newsgroup-last-folder)
		gnus-newsgroup-last-folder)
	       (folder folder)
	       (t (mh-prompt-for-folder
		   "Save article in"
		   (funcall gnus-folder-save-name gnus-newsgroup-name
			    gnus-current-headers gnus-newsgroup-last-folder)
		   t))))
	(errbuf (gnus-get-buffer-create " *Gnus rcvstore*"))
	;; Find the rcvstore program.
	(exec-path (cond
		    ((and (boundp 'mh-lib-progs) mh-lib-progs)
		     (cons mh-lib-progs exec-path))
		    (mh-lib (cons mh-lib exec-path))
		    (t exec-path))))
    (with-current-buffer gnus-original-article-buffer
      (save-restriction
	(widen)
	(unwind-protect
	    (call-process-region
	     (point-min) (point-max) "rcvstore" nil errbuf nil folder)
	  (set-buffer errbuf)
	  (if (zerop (buffer-size))
	      (message "Article saved in folder: %s" folder)
	    (message "%s" (buffer-string)))
	  (kill-buffer errbuf))))
    (setq gnus-newsgroup-last-folder folder)))

(defun gnus-Folder-save-name (newsgroup headers &optional last-folder)
  "Generate folder name from NEWSGROUP, HEADERS, and optional LAST-FOLDER.
If variable `gnus-use-long-file-name' is nil, it is +News.group.
Otherwise, it is like +news/group."
  (or last-folder
      (concat "+"
	      (if gnus-use-long-file-name
		  (gnus-capitalize-newsgroup newsgroup)
		(gnus-newsgroup-directory-form newsgroup)))))

(defun gnus-folder-save-name (newsgroup headers &optional last-folder)
  "Generate folder name from NEWSGROUP, HEADERS, and optional LAST-FOLDER.
If variable `gnus-use-long-file-name' is nil, it is +news.group.
Otherwise, it is like +news/group."
  (or last-folder
      (concat "+"
	      (if gnus-use-long-file-name
		  newsgroup
		(gnus-newsgroup-directory-form newsgroup)))))

(provide 'gnus-mh)

;;; gnus-mh.el ends here
