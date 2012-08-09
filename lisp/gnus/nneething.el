;;; nneething.el --- arbitrary file access for Gnus

;; Copyright (C) 1995-2012 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;;	Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>
;; Keywords: news, mail

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

(eval-when-compile (require 'cl))

(require 'mailcap)
(require 'nnheader)
(require 'nnmail)
(require 'nnoo)
(require 'gnus-util)

(nnoo-declare nneething)

(defvoo nneething-map-file-directory
  (nnheader-concat gnus-directory ".nneething/")
  "Where nneething stores the map files.")

(defvoo nneething-map-file ".nneething"
  "Name of the map files.")

(defvoo nneething-exclude-files nil
  "Regexp saying what files to exclude from the group.
If this variable is nil, no files will be excluded.")

(defvoo nneething-include-files nil
  "Regexp saying what files to include in the group.
If this variable is non-nil, only files matching this regexp will be
included.")



;;; Internal variables.

(defconst nneething-version "nneething 1.0"
  "nneething version.")

(defvoo nneething-current-directory nil
  "Current news group directory.")

(defvoo nneething-status-string "")

(defvoo nneething-work-buffer " *nneething work*")

(defvoo nneething-group nil)
(defvoo nneething-map nil)
(defvoo nneething-read-only nil)
(defvoo nneething-active nil)
(defvoo nneething-address nil)



;;; Interface functions.

(nnoo-define-basics nneething)

(deffoo nneething-retrieve-headers (articles &optional group server fetch-old)
  (nneething-possibly-change-directory group)

  (with-current-buffer nntp-server-buffer
    (erase-buffer)
    (let* ((number (length articles))
	   (count 0)
	   (large (and (numberp nnmail-large-newsgroup)
		       (> number nnmail-large-newsgroup)))
	   article file)

      (if (stringp (car articles))
	  'headers

	(while (setq article (pop articles))
	  (setq file (nneething-file-name article))

	  (when (and (file-exists-p file)
		     (or (file-directory-p file)
			 (not (zerop (nnheader-file-size file)))))
	    (insert (format "221 %d Article retrieved.\n" article))
	    (nneething-insert-head file)
	    (insert ".\n"))

	  (incf count)

	  (and large
	       (zerop (% count 20))
	       (nnheader-message 5 "nneething: Receiving headers... %d%%"
				 (/ (* count 100) number))))

	(when large
	  (nnheader-message 5 "nneething: Receiving headers...done"))

	(nnheader-fold-continuation-lines)
	'headers))))

(deffoo nneething-request-article (id &optional group server buffer)
  (nneething-possibly-change-directory group)
  (let ((file (unless (stringp id)
		(nneething-file-name id)))
	(nntp-server-buffer (or buffer nntp-server-buffer)))
    (and (stringp file)		   ; We did not request by Message-ID.
	 (file-exists-p file)		; The file exists.
	 (not (file-directory-p file))	; It's not a dir.
	 (save-excursion
	   (let ((nnmail-file-coding-system 'binary))
	     (nnmail-find-file file))	; Insert the file in the nntp buf.
	   (unless (nnheader-article-p)	; Either it's a real article...
	     (let ((type
		    (unless (file-directory-p file)
		      (or (cdr (assoc (concat "." (file-name-extension file))
				      mailcap-mime-extensions))
			  "text/plain")))
		   (charset
		    (mm-detect-mime-charset-region (point-min) (point-max)))
		   (encoding))
	       (unless (string-match "\\`text/" type)
		 (base64-encode-region (point-min) (point-max))
		 (setq encoding "base64"))
	       (goto-char (point-min))
	       (nneething-make-head file (current-buffer)
				    nil type charset encoding))
	     (insert "\n"))
	   t))))

(deffoo nneething-request-group (group &optional server dont-check info)
  (nneething-possibly-change-directory group server)
  (unless dont-check
    (nneething-create-mapping)
    (if (> (car nneething-active) (cdr nneething-active))
	(nnheader-insert "211 0 1 0 %s\n" group)
      (nnheader-insert
       "211 %d %d %d %s\n"
       (- (1+ (cdr nneething-active)) (car nneething-active))
       (car nneething-active) (cdr nneething-active)
       group)))
  t)

(deffoo nneething-request-list (&optional server dir)
  (nnheader-report 'nneething "LIST is not implemented."))

(deffoo nneething-request-newgroups (date &optional server)
  (nnheader-report 'nneething "NEWSGROUPS is not implemented."))

(deffoo nneething-request-type (group &optional article)
  'unknown)

(deffoo nneething-close-group (group &optional server)
  (setq nneething-current-directory nil)
  t)

(deffoo nneething-open-server (server &optional defs)
  (nnheader-init-server-buffer)
  (if (nneething-server-opened server)
      t
    (unless (assq 'nneething-address defs)
      (setq defs (append defs (list (list 'nneething-address server)))))
    (nnoo-change-server 'nneething server defs)))


;;; Internal functions.

(defun nneething-possibly-change-directory (group &optional server)
  (when (and server
	     (not (nneething-server-opened server)))
    (nneething-open-server server))
  (when (and group
	     (not (equal nneething-group group)))
    (setq nneething-group group)
    (setq nneething-map nil)
    (setq nneething-active (cons 1 0))
    (nneething-create-mapping)))

(defun nneething-map-file ()
  ;; We make sure that the .nneething directory exists.
  (gnus-make-directory nneething-map-file-directory)
  ;; We store it in a special directory under the user's home dir.
  (concat (file-name-as-directory nneething-map-file-directory)
	  nneething-group nneething-map-file))

(defun nneething-create-mapping ()
  ;; Read nneething-active and nneething-map.
  (when (file-exists-p nneething-address)
    (let ((map-file (nneething-map-file))
	  (files (directory-files nneething-address))
	  touched map-files)
      (when (file-exists-p map-file)
	(ignore-errors
	  (load map-file nil t t)))
      (unless nneething-active
	(setq nneething-active (cons 1 0)))
      ;; Old nneething had a different map format.
      (when (and (cdar nneething-map)
		 (atom (cdar nneething-map)))
	(setq nneething-map
	      (mapcar (lambda (n)
			(list (cdr n) (car n)
			      (nth 5 (file-attributes
				      (nneething-file-name (car n))))))
		      nneething-map)))
      ;; Remove files matching the exclusion regexp.
      (when nneething-exclude-files
	(let ((f files)
	      prev)
	  (while f
	    (if (string-match nneething-exclude-files (car f))
		(if prev (setcdr prev (cdr f))
		  (setq files (cdr files)))
	      (setq prev f))
	    (setq f (cdr f)))))
      ;; Remove files not matching the inclusion regexp.
      (when nneething-include-files
	(let ((f files)
	      prev)
	  (while f
	    (if (not (string-match nneething-include-files (car f)))
		(if prev (setcdr prev (cdr f))
		  (setq files (cdr files)))
	      (setq prev f))
	    (setq f (cdr f)))))
      ;; Remove deleted files from the map.
      (let ((map nneething-map)
	    prev)
	(while map
	  (if (and (member (cadr (car map)) files)
		  ;; We also remove files that have changed mod times.
		   (equal (nth 5 (file-attributes
				  (nneething-file-name (cadr (car map)))))
			  (cadr (cdar map))))
	      (progn
		(push (cadr (car map)) map-files)
		(setq prev map))
	    (setq touched t)
	    (if prev
		(setcdr prev (cdr map))
	      (setq nneething-map (cdr nneething-map))))
	  (setq map (cdr map))))
      ;; Find all new files and enter them into the map.
      (while files
	(unless (member (car files) map-files)
	  ;; This file is not in the map, so we enter it.
	  (setq touched t)
	  (setcdr nneething-active (1+ (cdr nneething-active)))
	  (push (list (cdr nneething-active) (car files)
		      (nth 5 (file-attributes
			      (nneething-file-name (car files)))))
		nneething-map))
	(setq files (cdr files)))
      (when (and touched
		 (not nneething-read-only))
	(with-temp-file map-file
	  (insert "(setq nneething-map '")
	  (gnus-prin1 nneething-map)
	  (insert ")\n(setq nneething-active '")
	  (gnus-prin1 nneething-active)
	  (insert ")\n"))))))

(defun nneething-insert-head (file)
  "Insert the head of FILE."
  (when (nneething-get-head file)
    (insert-buffer-substring nneething-work-buffer)
    (goto-char (point-max))))

(defun nneething-encode-file-name (file &optional coding-system)
  "Encode the name of the FILE in CODING-SYSTEM."
  (let ((pos 0) buf)
    (setq file (mm-encode-coding-string
		file (or coding-system nnmail-pathname-coding-system)))
    (while (string-match "[^-0-9a-zA-Z_:/.]" file pos)
      (setq buf (cons (format "%%%02x" (aref file (match-beginning 0)))
		      (cons (substring file pos (match-beginning 0)) buf))
	    pos (match-end 0)))
    (apply (function concat)
	   (nreverse (cons (substring file pos) buf)))))

(defun nneething-decode-file-name (file &optional coding-system)
  "Decode the name of the FILE is encoded in CODING-SYSTEM."
  (let ((pos 0) buf)
    (while (string-match "%\\([0-9a-fA-F][0-9a-fA-F]\\)" file pos)
      (setq buf (cons (string (string-to-number (match-string 1 file) 16))
		      (cons (substring file pos (match-beginning 0)) buf))
	    pos (match-end 0)))
    (mm-decode-coding-string
     (apply (function concat)
	    (nreverse (cons (substring file pos) buf)))
     (or coding-system nnmail-pathname-coding-system))))

(defun nneething-get-file-name (id)
  "Extract the file name from the message ID string."
  (when (string-match "\\`<nneething-\\([^@]+\\)@.*>\\'" id)
    (nneething-decode-file-name (match-string 1 id))))

(defun nneething-make-head (file &optional buffer extra-msg
				 mime-type mime-charset mime-encoding)
  "Create a head by looking at the file attributes of FILE."
  (let ((atts (file-attributes file)))
    (insert
     "Subject: " (file-name-nondirectory file) (or extra-msg "") "\n"
     "Message-ID: <nneething-" (nneething-encode-file-name file)
     "@" (system-name) ">\n"
     (if (equal '(0 0) (nth 5 atts)) ""
       (concat "Date: " (current-time-string (nth 5 atts)) "\n"))
     (or (when buffer
	   (with-current-buffer buffer
	     (when (re-search-forward "<[a-zA-Z0-9_]@[-a-zA-Z0-9_]>" 1000 t)
	       (concat "From: " (match-string 0) "\n"))))
	 (nneething-from-line (nth 2 atts) file))
     (if (> (string-to-number (int-to-string (nth 7 atts))) 0)
	 (concat "Chars: " (int-to-string (nth 7 atts)) "\n")
       "")
     (if buffer
	 (with-current-buffer buffer
	   (concat "Lines: " (int-to-string
			      (count-lines (point-min) (point-max)))
		   "\n"))
       "")
     (if mime-type
	 (concat "Content-Type: " mime-type
		 (if mime-charset
		     (concat "; charset="
			     (if (stringp mime-charset)
				 mime-charset
			       (symbol-name mime-charset)))
		   "")
		 (if mime-encoding
		     (concat "\nContent-Transfer-Encoding: " mime-encoding)
		   "")
		 "\nMIME-Version: 1.0\n")
       ""))))

(defun nneething-from-line (uid &optional file)
  "Return a From header based of UID."
  (let* ((login (condition-case nil
		    (user-login-name uid)
		  (error
		   (cond ((= uid (user-uid)) (user-login-name))
			 ((zerop uid) "root")
			 (t (int-to-string uid))))))
	 (name (condition-case nil
		   (user-full-name uid)
		 (error
		  (cond ((= uid (user-uid)) (user-full-name))
			((zerop uid) "Ms. Root")))))
	 (host (if  (string-match "\\`/[^/@]*@\\([^:/]+\\):" file)
		   (prog1
		       (substring file
				  (match-beginning 1)
				  (match-end 1))
		     (when (string-match
			    "/\\(users\\|home\\)/\\([^/]+\\)/" file)
		       (setq login (substring file
					      (match-beginning 2)
					      (match-end 2))
			     name nil)))
		 (system-name))))
    (concat "From: " login "@" host
	    (if name (concat " (" name ")") "") "\n")))

(defun nneething-get-head (file)
  "Either find the head in FILE or make a head for FILE."
  (with-current-buffer (get-buffer-create nneething-work-buffer)
    (setq case-fold-search nil)
    (buffer-disable-undo)
    (erase-buffer)
    (cond
     ((not (file-exists-p file))
      ;; The file do not exist.
      nil)
     ((or (file-directory-p file)
	  (file-symlink-p file))
      ;; It's a dir, so we fudge a head.
      (nneething-make-head file) t)
     (t
      ;; We examine the file.
      (condition-case ()
	  (progn
	    (nnheader-insert-head file)
	    (if (nnheader-article-p)
		(delete-region
		 (progn
		   (goto-char (point-min))
		   (or (and (search-forward "\n\n" nil t)
			    (1- (point)))
		       (point-max)))
		 (point-max))
	      (goto-char (point-min))
	      (nneething-make-head file (current-buffer))
	      (delete-region (point) (point-max))))
	(file-error
	 (nneething-make-head file (current-buffer) " (unreadable)")))
      t))))

(defun nneething-file-name (article)
  "Return the file name of ARTICLE."
  (let ((dir (file-name-as-directory nneething-address))
	fname)
    (if (numberp article)
	(if (setq fname (cadr (assq article nneething-map)))
	    (expand-file-name fname dir)
	  (make-temp-name (expand-file-name "nneething" dir)))
      (expand-file-name article dir))))

(provide 'nneething)

;;; nneething.el ends here
