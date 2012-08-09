;;; gulp.el --- ask for updates for Lisp packages

;; Copyright (C) 1996, 2001-2012 Free Software Foundation, Inc.

;; Author: Sam Shteingold <shteingd@math.ucla.edu>
;; Maintainer: FSF
;; Keywords: maint

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

;; Search the emacs/{version}/lisp directory for *.el files, extract the
;; name of the author or maintainer and send him e-mail requesting
;; update.

;;; Code:
(defgroup gulp nil
  "Ask for updates for Lisp packages."
  :prefix "-"
  :group 'maint)

(defcustom gulp-discard "^;+ *Maintainer: *FSF *$"
  "The regexp matching the packages not requiring the request for updates."
  :type 'regexp
  :group 'gulp)

(defcustom gulp-tmp-buffer "*gulp*"
  "The name of the temporary buffer."
  :type 'string
  :group 'gulp)

(defcustom gulp-max-len 2000
  "Distance into a Lisp source file to scan for keywords."
  :type 'integer
  :group 'gulp)

(defcustom gulp-request-header
  (concat
   "This message was created automatically.
I'm going to start pretesting a new version of GNU Emacs soon, so I'd
like to ask if you have any updates for the Emacs packages you work on.
You're listed as the maintainer of the following package(s):\n\n")
  "The starting text of a gulp message."
  :type 'string
  :group 'gulp)

(defcustom gulp-request-end
  (concat
   "\nIf you have any changes since the version in the previous release ("
   (format "%d.%d" emacs-major-version emacs-minor-version)
   "),
please send them to me ASAP.

Please don't send the whole file.  Instead, please send a patch made with
`diff -c' that shows precisely the changes you would like me to install.
Also please include itemized change log entries for your changes;
please use lisp/ChangeLog as a guide for the style and for what kinds
of information to include.

Thanks.")
  "The closing text in a gulp message."
  :type 'string
  :group 'gulp)

(declare-function mail-subject "sendmail" ())
(declare-function mail-send "sendmail" ())

(defun gulp-send-requests (dir &optional time)
  "Send requests for updates to the authors of Lisp packages in directory DIR.
For each maintainer, the message consists of `gulp-request-header',
followed by the list of packages (with modification times if the optional
prefix argument TIME is non-nil), concluded with `gulp-request-end'.

You can't edit the messages, but you can confirm whether to send each one.

The list of addresses for which you decided not to send mail
is left in the `*gulp*' buffer at the end."
  (interactive "DRequest updates for Lisp directory: \nP")
  (with-current-buffer (get-buffer-create gulp-tmp-buffer)
    (let ((m-p-alist (gulp-create-m-p-alist
		      (directory-files dir nil "^[^=].*\\.el$" t)
		      dir))
	  ;; Temporarily inhibit undo in the *gulp* buffer.
	  (buffer-undo-list t)
	  mail-setup-hook msg node)
      (setq m-p-alist
	    (sort m-p-alist
		  (function (lambda (a b)
			      (string< (car a) (car b))))))
      (while (setq node (car m-p-alist))
	(setq msg (gulp-create-message (cdr node) time))
	(setq mail-setup-hook
	      (lambda ()
		(mail-subject)
		(insert "It's time for Emacs updates again")
		(goto-char (point-max))
		(insert msg)))
	(mail nil (car node))
	(goto-char (point-min))
	(if (y-or-n-p "Send? ") (mail-send)
	  (kill-this-buffer)
	  (set-buffer gulp-tmp-buffer)
	  (insert (format "%s\n\n" node)))
	(setq m-p-alist (cdr m-p-alist))))
    (set-buffer gulp-tmp-buffer)
    (setq buffer-undo-list nil)))


(defun gulp-create-message (rec time)
  "Return the message string for REC, which is a list like (FILE TIME)."
  (let (node (str gulp-request-header))
    (while (setq node (car rec))
      (setq str (concat str "\t" (car node)
			(if time (concat "\tLast modified:\t" (cdr node)))
			"\n"))
      (setq rec (cdr rec)))
    (concat str gulp-request-end)))


(defun gulp-create-m-p-alist (flist dir)
  "Create the maintainer/package alist for files in FLIST in DIR.
That is a list of elements, each of the form (MAINTAINER PACKAGES...)."
  (save-excursion
    (let (mplist filen node mnt-tm mnt tm fl-tm)
      (get-buffer-create gulp-tmp-buffer)
      (set-buffer gulp-tmp-buffer)
      (setq buffer-undo-list t)
      (while flist
	(setq fl-tm (gulp-maintainer (setq filen (car flist)) dir))
	(if (setq tm (cdr fl-tm) mnt (car fl-tm));; there is a definite maintainer
	    (if (setq node (assoc mnt mplist));; this is not a new maintainer
		(setq mplist (cons (cons mnt (cons (cons filen tm) (cdr node)))
				   (delete node mplist)))
	      (setq mplist (cons (list mnt (cons filen (cdr fl-tm))) mplist))))
	(setq flist (cdr flist)))
      (erase-buffer)
      mplist)))

(defun gulp-maintainer (filenm dir)
  "Return a list (MAINTAINER TIMESTAMP) for the package FILENM in directory DIR."
  (save-excursion
    (let* ((fl (expand-file-name filenm dir)) mnt
	   (timest (format-time-string "%Y-%m-%d %a %T %Z"
				       (elt (file-attributes fl) 5))))
      (set-buffer gulp-tmp-buffer)
      (erase-buffer)
      (insert-file-contents fl nil 0 gulp-max-len)
      (goto-char 1)
      (if (re-search-forward gulp-discard nil t)
	  (setq mnt nil) ;; do nothing, return nil
	(goto-char 1)
	(if (and (re-search-forward "^;+ *Maintainer: \\(.*\\)$" nil t)
		 (> (length (setq mnt (match-string 1))) 0))
	    () ;; found!
	  (goto-char 1)
	  (if (re-search-forward "^;+ *Author: \\(.*\\)$" nil t)
	      (setq mnt (match-string 1))))
	(if (= (length mnt) 0) (setq mnt nil))) ;; "^;; Author: $" --> nil
      (cons mnt timest))))

(provide 'gulp)

;;; gulp.el ends here
