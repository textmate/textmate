;;; esh-util.el --- general utilities

;; Copyright (C) 1999-2012  Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>

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

(defgroup eshell-util nil
  "This is general utility code, meant for use by Eshell itself."
  :tag "General utilities"
  :group 'eshell)

;;; User Variables:

(defcustom eshell-stringify-t t
  "If non-nil, the string representation of t is 't'.
If nil, t will be represented only in the exit code of the function,
and not printed as a string.  This causes Lisp functions to behave
similarly to external commands, as far as successful result output."
  :type 'boolean
  :group 'eshell-util)

(defcustom eshell-group-file "/etc/group"
  "If non-nil, the name of the group file on your system."
  :type '(choice (const :tag "No group file" nil) file)
  :group 'eshell-util)

(defcustom eshell-passwd-file "/etc/passwd"
  "If non-nil, the name of the passwd file on your system."
  :type '(choice (const :tag "No passwd file" nil) file)
  :group 'eshell-util)

(defcustom eshell-hosts-file "/etc/hosts"
  "The name of the /etc/hosts file."
  :type '(choice (const :tag "No hosts file" nil) file)
  :group 'eshell-util)

(defcustom eshell-handle-errors t
  "If non-nil, Eshell will handle errors itself.
Setting this to nil is offered as an aid to debugging only."
  :type 'boolean
  :group 'eshell-util)

(defcustom eshell-private-file-modes 384 ; umask 177
  "The file-modes value to use for creating \"private\" files."
  :type 'integer
  :group 'eshell-util)

(defcustom eshell-private-directory-modes 448 ; umask 077
  "The file-modes value to use for creating \"private\" directories."
  :type 'integer
  :group 'eshell-util)

(defcustom eshell-tar-regexp
  "\\.t\\(ar\\(\\.\\(gz\\|bz2\\|xz\\|Z\\)\\)?\\|gz\\|a[zZ]\\|z2\\)\\'"
  "Regular expression used to match tar file names."
  :version "24.1"			; added xz
  :type 'regexp
  :group 'eshell-util)

(defcustom eshell-convert-numeric-arguments t
  "If non-nil, converting arguments of numeric form to Lisp numbers.
Numeric form is tested using the regular expression
`eshell-number-regexp'.

NOTE: If you find that numeric conversions are interfering with the
specification of filenames (for example, in calling `find-file', or
some other Lisp function that deals with files, not numbers), add the
following in your .emacs file:

  (put 'find-file 'eshell-no-numeric-conversions t)

Any function with the property `eshell-no-numeric-conversions' set to
a non-nil value, will be passed strings, not numbers, even when an
argument matches `eshell-number-regexp'."
  :type 'boolean
  :group 'eshell-util)

(defcustom eshell-number-regexp "-?\\([0-9]*\\.\\)?[0-9]+\\(e[-0-9.]+\\)?"
  "Regular expression used to match numeric arguments.
If `eshell-convert-numeric-arguments' is non-nil, and an argument
matches this regexp, it will be converted to a Lisp number, using the
function `string-to-number'."
  :type 'regexp
  :group 'eshell-util)

(defcustom eshell-ange-ls-uids nil
  "List of user/host/id strings, used to determine remote ownership."
  :type '(repeat (cons :tag "Host for User/UID map"
		       (string :tag "Hostname")
		       (repeat (cons :tag "User/UID List"
				     (string :tag "Username")
				     (repeat :tag "UIDs" string)))))
  :group 'eshell-util)

;;; Internal Variables:

(defvar eshell-group-names nil
  "A cache to hold the names of groups.")

(defvar eshell-group-timestamp nil
  "A timestamp of when the group file was read.")

(defvar eshell-user-names nil
  "A cache to hold the names of users.")

(defvar eshell-user-timestamp nil
  "A timestamp of when the user file was read.")

(defvar eshell-host-names nil
  "A cache the names of frequently accessed hosts.")

(defvar eshell-host-timestamp nil
  "A timestamp of when the hosts file was read.")

;;; Functions:

(defsubst eshell-under-windows-p ()
  "Return non-nil if we are running under MS-DOS/Windows."
  (memq system-type '(ms-dos windows-nt)))

(defmacro eshell-condition-case (tag form &rest handlers)
  "If `eshell-handle-errors' is non-nil, this is `condition-case'.
Otherwise, evaluates FORM with no error handling."
  (declare (indent 2))
  (if eshell-handle-errors
      `(condition-case ,tag
	   ,form
	 ,@handlers)
    form))

(defun eshell-find-delimiter
  (open close &optional bound reverse-p backslash-p)
  "From point, find the CLOSE delimiter corresponding to OPEN.
The matching is bounded by BOUND.
If REVERSE-P is non-nil, process the region backwards.
If BACKSLASH-P is non-nil, and OPEN and CLOSE are the same character,
then quoting is done by a backslash, rather than a doubled delimiter."
  (save-excursion
    (let ((depth 1)
	  (bound (or bound (point-max))))
      (if (if reverse-p
	      (eq (char-before) close)
	    (eq (char-after) open))
	  (forward-char (if reverse-p -1 1)))
      (while (and (> depth 0)
		  (funcall (if reverse-p '> '<) (point) bound))
	(let ((c (if reverse-p (char-before) (char-after))) nc)
	  (cond ((and (not reverse-p)
		      (or (not (eq open close))
			  backslash-p)
		      (eq c ?\\)
		      (setq nc (char-after (1+ (point))))
		      (or (eq nc open) (eq nc close)))
		 (forward-char 1))
		((and reverse-p
		      (or (not (eq open close))
			  backslash-p)
		      (or (eq c open) (eq c close))
		      (eq (char-before (1- (point)))
			  ?\\))
		 (forward-char -1))
		((eq open close)
		 (if (eq c open)
		     (if (and (not backslash-p)
			      (eq (if reverse-p
				      (char-before (1- (point)))
				    (char-after (1+ (point)))) open))
			 (forward-char (if reverse-p -1 1))
		       (setq depth (1- depth)))))
		((= c open)
		 (setq depth (+ depth (if reverse-p -1 1))))
		((= c close)
		 (setq depth (+ depth (if reverse-p 1 -1))))))
	(forward-char (if reverse-p -1 1)))
      (if (= depth 0)
	  (if reverse-p (point) (1- (point)))))))

(defun eshell-convert (string)
  "Convert STRING into a more native looking Lisp object."
  (if (not (stringp string))
      string
    (let ((len (length string)))
      (if (= len 0)
	  string
	(if (eq (aref string (1- len)) ?\n)
	    (setq string (substring string 0 (1- len))))
	(if (string-match "\n" string)
	    (split-string string "\n")
	  (if (and eshell-convert-numeric-arguments
		   (string-match
		    (concat "\\`\\s-*" eshell-number-regexp
			    "\\s-*\\'") string))
	      (string-to-number string)
	    string))))))

(defun eshell-sublist (l &optional n m)
  "Return from LIST the N to M elements.
If N or M is nil, it means the end of the list."
  (let* ((a (copy-sequence l))
	 result)
    (if (and m (consp (nthcdr m a)))
	(setcdr (nthcdr m a) nil))
    (if n
	(setq a (nthcdr n a))
      (setq n (1- (length a))
	    a (last a)))
    a))

(defvar eshell-path-env (getenv "PATH")
  "Content of $PATH.
It might be different from \(getenv \"PATH\"\), when
`default-directory' points to a remote host.")

(defun eshell-parse-colon-path (path-env)
  "Split string with `parse-colon-path'.
Prepend remote identification of `default-directory', if any."
  (let ((remote (file-remote-p default-directory)))
    (if remote
	(mapcar
	 (lambda (x) (concat remote x))
	 (parse-colon-path path-env))
      (parse-colon-path path-env))))

(defun eshell-split-path (path)
  "Split a path into multiple subparts."
  (let ((len (length path))
	(i 0) (li 0)
	parts)
    (if (and (eshell-under-windows-p)
	     (> len 2)
	     (eq (aref path 0) ?/)
	     (eq (aref path 1) ?/))
	(setq i 2))
    (while (< i len)
      (if (and (eq (aref path i) ?/)
	       (not (get-text-property i 'escaped path)))
	  (setq parts (cons (if (= li i) "/"
			      (substring path li (1+ i))) parts)
		li (1+ i)))
      (setq i (1+ i)))
    (if (< li i)
	(setq parts (cons (substring path li i) parts)))
    (if (and (eshell-under-windows-p)
	     (string-match "\\`[A-Za-z]:\\'" (car (last parts))))
	(setcar (last parts) (concat (car (last parts)) "/")))
    (nreverse parts)))

(defun eshell-to-flat-string (value)
  "Make value a string.  If separated by newlines change them to spaces."
  (let ((text (eshell-stringify value)))
    (if (string-match "\n+\\'" text)
	(setq text (replace-match "" t t text)))
    (while (string-match "\n+" text)
      (setq text (replace-match " " t t text)))
    text))

(defmacro eshell-for (for-var for-list &rest forms)
  "Iterate through a list."
  (declare (indent 2))
  `(let ((list-iter ,for-list))
     (while list-iter
       (let ((,for-var (car list-iter)))
	 ,@forms)
       (setq list-iter (cdr list-iter)))))


(make-obsolete 'eshell-for 'dolist "24.1")

(defun eshell-flatten-list (args)
  "Flatten any lists within ARGS, so that there are no sublists."
  (let ((new-list (list t)))
    (dolist (a args)
      (if (and (listp a)
	       (listp (cdr a)))
	  (nconc new-list (eshell-flatten-list a))
	(nconc new-list (list a))))
    (cdr new-list)))

(defun eshell-uniqify-list (l)
  "Remove occurring multiples in L.  You probably want to sort first."
  (let ((m l))
    (while m
      (while (and (cdr m)
		  (string= (car m)
			   (cadr m)))
	(setcdr m (cddr m)))
      (setq m (cdr m))))
  l)

(defun eshell-stringify (object)
  "Convert OBJECT into a string value."
  (cond
   ((stringp object) object)
   ((and (listp object)
	 (not (eq object nil)))
    (let ((string (pp-to-string object)))
      (substring string 0 (1- (length string)))))
   ((numberp object)
    (number-to-string object))
   (t
    (unless (and (eq object t)
		 (not eshell-stringify-t))
      (pp-to-string object)))))

(defsubst eshell-stringify-list (args)
  "Convert each element of ARGS into a string value."
  (mapcar 'eshell-stringify args))

(defsubst eshell-flatten-and-stringify (&rest args)
  "Flatten and stringify all of the ARGS into a single string."
  (mapconcat 'eshell-stringify (eshell-flatten-list args) " "))

(defsubst eshell-directory-files (regexp &optional directory)
  "Return a list of files in the given DIRECTORY matching REGEXP."
  (directory-files (or directory default-directory)
		   directory regexp))

(defun eshell-regexp-arg (prompt)
  "Return list of regexp and prefix arg using PROMPT."
  (let* (;; Don't clobber this.
	 (last-command last-command)
	 (regexp (read-from-minibuffer prompt nil nil nil
				       'minibuffer-history-search-history)))
    (list (if (string-equal regexp "")
	      (setcar minibuffer-history-search-history
		      (nth 1 minibuffer-history-search-history))
	    regexp)
	  (prefix-numeric-value current-prefix-arg))))

(defun eshell-printable-size (filesize &optional human-readable
				       block-size use-colors)
  "Return a printable FILESIZE."
  (let ((size (float (or filesize 0))))
    (if human-readable
	(if (< size human-readable)
	    (if (= (round size) 0)
		"0"
	      (if block-size
		  "1.0k"
		(format "%.0f" size)))
	  (setq size (/ size human-readable))
	  (if (< size human-readable)
	      (if (<= size 9.94)
		  (format "%.1fk" size)
		(format "%.0fk" size))
	    (setq size (/ size human-readable))
	    (if (< size human-readable)
		(let ((str (if (<= size 9.94)
			       (format "%.1fM" size)
			     (format "%.0fM" size))))
		  (if use-colors
		      (put-text-property 0 (length str)
					 'face 'bold str))
		  str)
	      (setq size (/ size human-readable))
	      (if (< size human-readable)
		  (let ((str (if (<= size 9.94)
				 (format "%.1fG" size)
			       (format "%.0fG" size))))
		    (if use-colors
			(put-text-property 0 (length str)
					   'face 'bold-italic str))
		    str)))))
      (if block-size
	  (setq size (/ size block-size)))
      (format "%.0f" size))))

(defun eshell-winnow-list (entries exclude &optional predicates)
  "Pare down the ENTRIES list using the EXCLUDE regexp, and PREDICATES.
The original list is not affected.  If the result is only one element
long, it will be returned itself, rather than returning a one-element
list."
  (let ((flist (list t))
	valid p listified)
    (unless (listp entries)
      (setq entries (list entries)
	    listified t))
    (dolist (entry entries)
      (unless (and exclude (string-match exclude entry))
	(setq p predicates valid (null p))
	(while p
	  (if (funcall (car p) entry)
	      (setq valid t)
	    (setq p nil valid nil))
	  (setq p (cdr p)))
	(when valid
	  (nconc flist (list entry)))))
    (if listified
	(cadr flist)
      (cdr flist))))

(defsubst eshell-redisplay ()
  "Allow Emacs to redisplay buffers."
  ;; for some strange reason, Emacs 21 is prone to trigger an
  ;; "args out of range" error in `sit-for', if this function
  ;; runs while point is in the minibuffer and the users attempt
  ;; to use completion.  Don't ask me.
  (condition-case nil
      (sit-for 0 0)
    (error nil)))

(defun eshell-read-passwd-file (file)
  "Return an alist correlating gids to group names in FILE."
  (let (names)
    (when (file-readable-p file)
      (with-temp-buffer
	(insert-file-contents file)
	(goto-char (point-min))
	(while (not (eobp))
	  (let* ((fields
		  (split-string (buffer-substring
				 (point) (progn (end-of-line)
						(point))) ":")))
	    (if (and (and fields (nth 0 fields) (nth 2 fields))
		     (not (assq (string-to-number (nth 2 fields)) names)))
		(setq names (cons (cons (string-to-number (nth 2 fields))
					(nth 0 fields))
				  names))))
	  (forward-line))))
    names))

(defun eshell-read-passwd (file result-var timestamp-var)
  "Read the contents of /etc/passwd for user names."
  (if (or (not (symbol-value result-var))
	  (not (symbol-value timestamp-var))
	  (time-less-p
	   (symbol-value timestamp-var)
	   (nth 5 (file-attributes file))))
      (progn
	(set result-var (eshell-read-passwd-file file))
	(set timestamp-var (current-time))))
  (symbol-value result-var))

(defun eshell-read-group-names ()
  "Read the contents of /etc/group for group names."
  (if eshell-group-file
      (eshell-read-passwd eshell-group-file 'eshell-group-names
			  'eshell-group-timestamp)))

(defsubst eshell-group-id (name)
  "Return the user id for user NAME."
  (car (rassoc name (eshell-read-group-names))))

(defsubst eshell-group-name (gid)
  "Return the group name for the given GID."
  (cdr (assoc gid (eshell-read-group-names))))

(defun eshell-read-user-names ()
  "Read the contents of /etc/passwd for user names."
  (if eshell-passwd-file
      (eshell-read-passwd eshell-passwd-file 'eshell-user-names
			  'eshell-user-timestamp)))

(defsubst eshell-user-id (name)
  "Return the user id for user NAME."
  (car (rassoc name (eshell-read-user-names))))

(defalias 'eshell-user-name 'user-login-name)

(defun eshell-read-hosts-file (filename)
  "Read in the hosts from the /etc/hosts file."
  (let (hosts)
    (with-temp-buffer
      (insert-file-contents eshell-hosts-file)
      (goto-char (point-min))
      (while (re-search-forward
	      "^\\([^#[:space:]]+\\)\\s-+\\(\\S-+\\)\\(\\s-*\\(\\S-+\\)\\)?" nil t)
	(if (match-string 1)
	    (add-to-list 'hosts (match-string 1)))
	(if (match-string 2)
	    (add-to-list 'hosts (match-string 2)))
	(if (match-string 4)
	    (add-to-list 'hosts (match-string 4)))))
    (sort hosts 'string-lessp)))

(defun eshell-read-hosts (file result-var timestamp-var)
  "Read the contents of /etc/passwd for user names."
  (if (or (not (symbol-value result-var))
	  (not (symbol-value timestamp-var))
	  (time-less-p
	   (symbol-value timestamp-var)
	   (nth 5 (file-attributes file))))
      (progn
	(set result-var (eshell-read-hosts-file file))
	(set timestamp-var (current-time))))
  (symbol-value result-var))

(defun eshell-read-host-names ()
  "Read the contents of /etc/hosts for host names."
  (if eshell-hosts-file
      (eshell-read-hosts eshell-hosts-file 'eshell-host-names
			 'eshell-host-timestamp)))

(and (featurep 'xemacs)
     (not (fboundp 'subst-char-in-string))
     (defun subst-char-in-string (fromchar tochar string &optional inplace)
       "Replace FROMCHAR with TOCHAR in STRING each time it occurs.
Unless optional argument INPLACE is non-nil, return a new string."
       (let ((i (length string))
	     (newstr (if inplace string (copy-sequence string))))
	 (while (> i 0)
	   (setq i (1- i))
	   (if (eq (aref newstr i) fromchar)
	       (aset newstr i tochar)))
	 newstr)))

(defsubst eshell-copy-environment ()
  "Return an unrelated copy of `process-environment'."
  (mapcar 'concat process-environment))

(defun eshell-subgroups (groupsym)
  "Return all of the subgroups of GROUPSYM."
  (let ((subgroups (get groupsym 'custom-group))
	(subg (list t)))
    (while subgroups
      (if (eq (cadr (car subgroups)) 'custom-group)
	  (nconc subg (list (caar subgroups))))
      (setq subgroups (cdr subgroups)))
    (cdr subg)))

(defmacro eshell-with-file-modes (modes &rest forms)
  "Evaluate, with file-modes set to MODES, the given FORMS."
  `(let ((modes (default-file-modes)))
     (set-default-file-modes ,modes)
     (unwind-protect
	 (progn ,@forms)
       (set-default-file-modes modes))))

(defmacro eshell-with-private-file-modes (&rest forms)
  "Evaluate FORMS with private file modes set."
  `(eshell-with-file-modes ,eshell-private-file-modes ,@forms))

(defsubst eshell-make-private-directory (dir &optional parents)
  "Make DIR with file-modes set to `eshell-private-directory-modes'."
  (eshell-with-file-modes eshell-private-directory-modes
			  (make-directory dir parents)))

(defsubst eshell-substring (string sublen)
  "Return the beginning of STRING, up to SUBLEN bytes."
  (if string
      (if (> (length string) sublen)
	  (substring string 0 sublen)
	string)))

(and (featurep 'xemacs)
     (not (fboundp 'directory-files-and-attributes))
     (defun directory-files-and-attributes (directory &optional full match nosort id-format)
    "Return a list of names of files and their attributes in DIRECTORY.
There are three optional arguments:
If FULL is non-nil, return absolute file names.  Otherwise return names
 that are relative to the specified directory.
If MATCH is non-nil, mention only file names that match the regexp MATCH.
If NOSORT is non-nil, the list is not sorted--its order is unpredictable.
 NOSORT is useful if you plan to sort the result yourself."
    (let ((directory (expand-file-name directory)) ange-cache)
      (mapcar
       (function
	(lambda (file)
	  (cons file (eshell-file-attributes (expand-file-name file directory)))))
       (directory-files directory full match nosort)))))

(defvar ange-cache)

(defun eshell-directory-files-and-attributes (dir &optional full match nosort id-format)
  "Make sure to use the handler for `directory-file-and-attributes'."
  (let* ((dir (expand-file-name dir)))
    (if (string-equal (file-remote-p dir 'method) "ftp")
	(let ((files (directory-files dir full match nosort)))
	  (mapcar
	   (lambda (file)
	     (cons file (eshell-file-attributes (expand-file-name file dir))))
	   files))
      (directory-files-and-attributes dir full match nosort id-format))))

(defun eshell-current-ange-uids ()
  (if (string-match "/\\([^@]+\\)@\\([^:]+\\):" default-directory)
      (let* ((host (match-string 2 default-directory))
	     (user (match-string 1 default-directory))
	     (host-users (assoc host eshell-ange-ls-uids)))
	(when host-users
	  (setq host-users (cdr host-users))
	  (cdr (assoc user host-users))))))

;; Add an autoload for parse-time-string
(if (and (not (fboundp 'parse-time-string))
	 (locate-library "parse-time"))
    (autoload 'parse-time-string "parse-time"))

(eval-when-compile
  (require 'ange-ftp nil t)
  (require 'tramp nil t))

(defun eshell-parse-ange-ls (dir)
  (let ((ange-ftp-name-format
	 (list (nth 0 tramp-file-name-structure)
	       (nth 3 tramp-file-name-structure)
	       (nth 2 tramp-file-name-structure)
	       (nth 4 tramp-file-name-structure)))
	;; ange-ftp uses `ange-ftp-ftp-name-arg' and `ange-ftp-ftp-name-res'
	;; for optimization in `ange-ftp-ftp-name'. If Tramp wasn't active,
	;; there could be incorrect values from previous calls in case the
	;; "ftp" method is used in the Tramp file name. So we unset
	;; those values.
	(ange-ftp-ftp-name-arg "")
	(ange-ftp-ftp-name-res nil)
	entry)
    (with-temp-buffer
      (insert (ange-ftp-ls dir "-la" nil))
      (goto-char (point-min))
      (if (looking-at "^total [0-9]+$")
	  (forward-line 1))
      ;; Some systems put in a blank line here.
      (if (eolp) (forward-line 1))
      (while (looking-at
	      `,(concat "\\([dlscb-][rwxst-]+\\)"
			"\\s-*" "\\([0-9]+\\)" "\\s-+"
			"\\(\\S-+\\)" "\\s-+"
			"\\(\\S-+\\)" "\\s-+"
			"\\([0-9]+\\)" "\\s-+" "\\(.*\\)"))
	(let* ((perms (match-string 1))
	       (links (string-to-number (match-string 2)))
	       (user (match-string 3))
	       (group (match-string 4))
	       (size (string-to-number (match-string 5)))
	       (name (ange-ftp-parse-filename))
	       (mtime
		(if (fboundp 'parse-time-string)
		    (let ((moment (parse-time-string
				   (match-string 6))))
		      (if (nth 0 moment)
			  (setcar (nthcdr 5 moment)
				  (nth 5 (decode-time (current-time))))
			(setcar (nthcdr 0 moment) 0)
			(setcar (nthcdr 1 moment) 0)
			(setcar (nthcdr 2 moment) 0))
		      (apply 'encode-time moment))
		  (ange-ftp-file-modtime (expand-file-name name dir))))
	       symlink)
	  (if (string-match "\\(.+\\) -> \\(.+\\)" name)
	      (setq symlink (match-string 2 name)
		    name (match-string 1 name)))
	  (setq entry
		(cons
		 (cons name
		       (list (if (eq (aref perms 0) ?d)
				 t
			       symlink)
			     links user group
			     nil mtime nil
			     size perms nil nil)) entry)))
	(forward-line)))
    entry))

(defun eshell-file-attributes (file &optional id-format)
  "Return the attributes of FILE, playing tricks if it's over ange-ftp.
The optional argument ID-FORMAT specifies the preferred uid and
gid format.  Valid values are 'string and 'integer, defaulting to
'integer.  See `file-attributes'."
  (let* ((file (expand-file-name file))
	 entry)
    (if (string-equal (file-remote-p file 'method) "ftp")
	(let ((base (file-name-nondirectory file))
	      (dir (file-name-directory file)))
	  (if (string-equal "" base) (setq base "."))
	  (if (boundp 'ange-cache)
	      (setq entry (cdr (assoc base (cdr (assoc dir ange-cache))))))
	  (unless entry
	    (setq entry (eshell-parse-ange-ls dir))
	    (if (boundp 'ange-cache)
		(setq ange-cache
		      (cons (cons dir entry)
			    ange-cache)))
	    (if entry
		(let ((fentry (assoc base (cdr entry))))
		  (if fentry
		      (setq entry (cdr fentry))
		    (setq entry nil)))))
	  entry)
      (file-attributes file id-format))))

(defalias 'eshell-copy-tree 'copy-tree)

(defsubst eshell-processp (proc)
  "If the `processp' function does not exist, PROC is not a process."
  (and (fboundp 'processp) (processp proc)))

; (defun eshell-copy-file
;   (file newname &optional ok-if-already-exists keep-date)
;   "Copy FILE to NEWNAME.  See docs for `copy-file'."
;   (let (copied)
;     (if (string-match "\\`\\([^:]+\\):\\(.*\\)" file)
;	(let ((front (match-string 1 file))
;	      (back (match-string 2 file))
;	      buffer)
;	  (if (and front (string-match eshell-tar-regexp front)
;		     (setq buffer (find-file-noselect front)))
;	    (with-current-buffer buffer
;	      (goto-char (point-min))
;	      (if (re-search-forward (concat " " (regexp-quote back)
;					     "$") nil t)
;		  (progn
;		    (tar-copy (if (file-directory-p newname)
;				  (expand-file-name
;				   (file-name-nondirectory back) newname)
;				newname))
;		    (setq copied t))
;		(error "%s not found in tar file %s" back front))))))
;     (unless copied
;       (copy-file file newname ok-if-already-exists keep-date))))

; (defun eshell-file-attributes (filename)
;   "Return a list of attributes of file FILENAME.
; See the documentation for `file-attributes'."
;   (let (result)
;     (when (string-match "\\`\\([^:]+\\):\\(.*\\)\\'" filename)
;       (let ((front (match-string 1 filename))
;	    (back (match-string 2 filename))
;	    buffer)
;	(when (and front (string-match eshell-tar-regexp front)
;		   (setq buffer (find-file-noselect front)))
;	  (with-current-buffer buffer
;	    (goto-char (point-min))
;	    (when (re-search-forward (concat " " (regexp-quote back)
;					     "\\s-*$") nil t)
;	      (let* ((descrip (tar-current-descriptor))
;		     (tokens (tar-desc-tokens descrip)))
;		(setq result
;		      (list
;		       (cond
;			((eq (tar-header-link-type tokens) 5)
;			 t)
;			((eq (tar-header-link-type tokens) t)
;			 (tar-header-link-name tokens)))
;		       1
;		       (tar-header-uid tokens)
;		       (tar-header-gid tokens)
;		       (tar-header-date tokens)
;		       (tar-header-date tokens)
;		       (tar-header-date tokens)
;		       (tar-header-size tokens)
;		       (concat
;			(cond
;			 ((eq (tar-header-link-type tokens) 5) "d")
;			 ((eq (tar-header-link-type tokens) t) "l")
;			 (t "-"))
;			(tar-grind-file-mode (tar-header-mode tokens)
;					     (make-string 9 ? ) 0))
;		       nil nil nil))))))))
;     (or result
;	(file-attributes filename))))

(provide 'esh-util)

;;; esh-util.el ends here
