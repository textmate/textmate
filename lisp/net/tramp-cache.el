;;; tramp-cache.el --- file information caching for Tramp

;; Copyright (C) 2000, 2005-2012 Free Software Foundation, Inc.

;; Author: Daniel Pittman <daniel@inanna.danann.net>
;;         Michael Albinus <michael.albinus@gmx.de>
;; Keywords: comm, processes
;; Package: tramp

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

;; An implementation of information caching for remote files.

;; Each connection, identified by a vector [method user host
;; localname] or by a process, has a unique cache. We distinguish 3
;; kind of caches, depending on the key:
;;
;; - localname is NIL.  This are reusable properties.  Examples:
;;   "remote-shell" identifies the POSIX shell to be called on the
;;   remote host, or "perl" is the command to be called on the remote
;;   host when starting a Perl script.  These properties are saved in
;;   the file `tramp-persistency-file-name'.
;;
;; - localname is a string.  This are temporary properties, which are
;;   related to the file localname is referring to.  Examples:
;;   "file-exists-p" is t or nile, depending on the file existence, or
;;   "file-attributes" caches the result of the function
;;  `file-attributes'.
;;
;; - The key is a process.  This are temporary properties related to
;;   an open connection.  Examples: "scripts" keeps shell script
;;   definitions already sent to the remote shell, "last-cmd-time" is
;;   the time stamp a command has been sent to the remote process.

;;; Code:

(require 'tramp)
(autoload 'time-stamp-string "time-stamp")

;;; -- Cache --

;;;###tramp-autoload
(defvar tramp-cache-data (make-hash-table :test 'equal)
  "Hash table for remote files properties.")

(defcustom tramp-persistency-file-name
  (cond
   ;; GNU Emacs.
   ((and (fboundp 'locate-user-emacs-file))
    (expand-file-name (tramp-compat-funcall 'locate-user-emacs-file "tramp")))
   ((and (boundp 'user-emacs-directory)
	 (stringp (symbol-value 'user-emacs-directory))
	 (file-directory-p (symbol-value 'user-emacs-directory)))
    (expand-file-name "tramp" (symbol-value 'user-emacs-directory)))
   ((and (not (featurep 'xemacs)) (file-directory-p "~/.emacs.d/"))
    "~/.emacs.d/tramp")
   ;; XEmacs.
   ((and (boundp 'user-init-directory)
	 (stringp (symbol-value 'user-init-directory))
	 (file-directory-p (symbol-value 'user-init-directory)))
    (expand-file-name "tramp" (symbol-value 'user-init-directory)))
   ((and (featurep 'xemacs) (file-directory-p "~/.xemacs/"))
    "~/.xemacs/tramp")
   ;; For users without `~/.emacs.d/' or `~/.xemacs/'.
   (t "~/.tramp"))
  "File which keeps connection history for Tramp connections."
  :group 'tramp
  :type 'file)

(defvar tramp-cache-data-changed nil
  "Whether persistent cache data have been changed.")

;;;###tramp-autoload
(defun tramp-get-file-property (vec file property default)
  "Get the PROPERTY of FILE from the cache context of VEC.
Returns DEFAULT if not set."
  ;; Unify localname.
  (setq vec (copy-sequence vec))
  (aset vec 3 (tramp-run-real-handler 'directory-file-name (list file)))
  (let* ((hash (or (gethash vec tramp-cache-data)
		   (puthash vec (make-hash-table :test 'equal)
			    tramp-cache-data)))
	 (value (when (hash-table-p hash) (gethash property hash))))
    (if
	;; We take the value only if there is any, and
	;; `remote-file-name-inhibit-cache' indicates that it is still
	;; valid.  Otherwise, DEFAULT is set.
	(and (consp value)
	     (or (null remote-file-name-inhibit-cache)
		 (and (integerp remote-file-name-inhibit-cache)
		      (<=
		       (tramp-time-diff (current-time) (car value))
		       remote-file-name-inhibit-cache))
		 (and (consp remote-file-name-inhibit-cache)
		      (tramp-time-less-p
		       remote-file-name-inhibit-cache (car value)))))
	(setq value (cdr value))
      (setq value default))

    (tramp-message vec 8 "%s %s %s" file property value)
    (when (>= tramp-verbose 10)
      (let* ((var (intern (concat "tramp-cache-get-count-" property)))
	     (val (or (ignore-errors (symbol-value var)) 0)))
	(set var (1+ val))))
    value))

;;;###tramp-autoload
(defun tramp-set-file-property (vec file property value)
  "Set the PROPERTY of FILE to VALUE, in the cache context of VEC.
Returns VALUE."
  ;; Unify localname.
  (setq vec (copy-sequence vec))
  (aset vec 3 (tramp-run-real-handler 'directory-file-name (list file)))
  (let ((hash (or (gethash vec tramp-cache-data)
		  (puthash vec (make-hash-table :test 'equal)
			   tramp-cache-data))))
    ;; We put the timestamp there.
    (puthash property (cons (current-time) value) hash)
    (tramp-message vec 8 "%s %s %s" file property value)
    (when (>= tramp-verbose 10)
      (let* ((var (intern (concat "tramp-cache-set-count-" property)))
	     (val (or (ignore-errors (symbol-value var)) 0)))
	(set var (1+ val))))
    value))

;;;###tramp-autoload
(defmacro with-file-property (vec file property &rest body)
  "Check in Tramp cache for PROPERTY, otherwise execute BODY and set cache.
FILE must be a local file name on a connection identified via VEC."
  `(if (file-name-absolute-p ,file)
      (let ((value (tramp-get-file-property ,vec ,file ,property 'undef)))
	(when (eq value 'undef)
	  ;; We cannot pass @body as parameter to
	  ;; `tramp-set-file-property' because it mangles our
	  ;; debug messages.
	  (setq value (progn ,@body))
	  (tramp-set-file-property ,vec ,file ,property value))
	value)
     ,@body))

;;;###tramp-autoload
(put 'with-file-property 'lisp-indent-function 3)
(put 'with-file-property 'edebug-form-spec t)
(tramp-compat-font-lock-add-keywords
 'emacs-lisp-mode '("\\<with-file-property\\>"))

;;;###tramp-autoload
(defun tramp-flush-file-property (vec file)
  "Remove all properties of FILE in the cache context of VEC."
  ;; Remove file property of symlinks.
  (let ((truename (tramp-get-file-property vec file "file-truename" nil)))
    (when (and (stringp truename)
	       (not (string-equal file truename)))
      (tramp-flush-file-property vec truename)))
  ;; Unify localname.
  (setq vec (copy-sequence vec))
  (aset vec 3 (tramp-run-real-handler 'directory-file-name (list file)))
  (tramp-message vec 8 "%s" file)
  (remhash vec tramp-cache-data))

;;;###tramp-autoload
(defun tramp-flush-directory-property (vec directory)
  "Remove all properties of DIRECTORY in the cache context of VEC.
Remove also properties of all files in subdirectories."
  (let ((directory (tramp-run-real-handler
		    'directory-file-name (list directory))))
  (tramp-message vec 8 "%s" directory)
    (maphash
     (lambda (key value)
       (when (and (stringp (tramp-file-name-localname key))
		  (string-match directory (tramp-file-name-localname key)))
	 (remhash key tramp-cache-data)))
     tramp-cache-data)))

;; Reverting or killing a buffer should also flush file properties.
;; They could have been changed outside Tramp.  In eshell, "ls" would
;; not show proper directory contents when a file has been copied or
;; deleted before.
(defun tramp-flush-file-function ()
  "Flush all Tramp cache properties from `buffer-file-name'."
  (let ((bfn (if (stringp (buffer-file-name))
		 (buffer-file-name)
	       default-directory)))
    (when (tramp-tramp-file-p bfn)
      (with-parsed-tramp-file-name bfn nil
	(tramp-flush-file-property v localname)))))

(add-hook 'before-revert-hook 'tramp-flush-file-function)
(add-hook 'eshell-pre-command-hook 'tramp-flush-file-function)
(add-hook 'kill-buffer-hook 'tramp-flush-file-function)
(add-hook 'tramp-cache-unload-hook
	  (lambda ()
	    (remove-hook 'before-revert-hook
			 'tramp-flush-file-function)
	    (remove-hook 'eshell-pre-command-hook
			 'tramp-flush-file-function)
	    (remove-hook 'kill-buffer-hook
			 'tramp-flush-file-function)))

;;; -- Properties --

;;;###tramp-autoload
(defun tramp-get-connection-property (key property default)
  "Get the named PROPERTY for the connection.
KEY identifies the connection, it is either a process or a vector.
If the value is not set for the connection, returns DEFAULT."
  ;; Unify key by removing localname from vector.  Work with a copy in
  ;; order to avoid side effects.
  (when (vectorp key)
    (setq key (copy-sequence key))
    (aset key 3 nil))
  (let* ((hash (gethash key tramp-cache-data))
	 (value (if (hash-table-p hash)
		    (gethash property hash default)
		  default)))
    (tramp-message key 7 "%s %s" property value)
    value))

;;;###tramp-autoload
(defun tramp-set-connection-property (key property value)
  "Set the named PROPERTY of a connection to VALUE.
KEY identifies the connection, it is either a process or a vector.
PROPERTY is set persistent when KEY is a vector."
  ;; Unify key by removing localname from vector.  Work with a copy in
  ;; order to avoid side effects.
  (when (vectorp key)
    (setq key (copy-sequence key))
    (aset key 3 nil))
  (let ((hash (or (gethash key tramp-cache-data)
		  (puthash key (make-hash-table :test 'equal)
			   tramp-cache-data))))
    (puthash property value hash)
    (setq tramp-cache-data-changed t)
    (tramp-message key 7 "%s %s" property value)
    value))

;;;###tramp-autoload
(defmacro with-connection-property (key property &rest body)
  "Check in Tramp for property PROPERTY, otherwise executes BODY and set."
  `(let ((value (tramp-get-connection-property ,key ,property 'undef)))
    (when (eq value 'undef)
      ;; We cannot pass ,@body as parameter to
      ;; `tramp-set-connection-property' because it mangles our debug
      ;; messages.
      (setq value (progn ,@body))
      (tramp-set-connection-property ,key ,property value))
    value))

;;;###tramp-autoload
(put 'with-connection-property 'lisp-indent-function 2)
(put 'with-connection-property 'edebug-form-spec t)
(tramp-compat-font-lock-add-keywords
 'emacs-lisp-mode '("\\<with-connection-property\\>"))

;;;###tramp-autoload
(defun tramp-flush-connection-property (key)
  "Remove all properties identified by KEY.
KEY identifies the connection, it is either a process or a vector."
  ;; Unify key by removing localname from vector.  Work with a copy in
  ;; order to avoid side effects.
  (when (vectorp key)
    (setq key (copy-sequence key))
    (aset key 3 nil))
  (tramp-message
   key 7 "%s %s" key
   (let ((hash (gethash key tramp-cache-data))
	 properties)
     (if (hash-table-p hash)
	 (maphash
	  (lambda (x y) (add-to-list 'properties x 'append))
	  (gethash key tramp-cache-data)))
     properties))
  (setq tramp-cache-data-changed t)
  (remhash key tramp-cache-data))

;;;###tramp-autoload
(defun tramp-cache-print (table)
  "Print hash table TABLE."
  (when (hash-table-p table)
    (let (result)
      (maphash
       (lambda (key value)
	 (let ((tmp (format
		     "(%s %s)"
		     (if (processp key)
			 (prin1-to-string (prin1-to-string key))
		       (prin1-to-string key))
		     (if (hash-table-p value)
			 (tramp-cache-print value)
		       (if (bufferp value)
			   (prin1-to-string (prin1-to-string value))
			 (prin1-to-string value))))))
	   (setq result (if result (concat result " " tmp) tmp))))
       table)
      result)))

;;;###tramp-autoload
(defun tramp-list-connections ()
  "Return a list of all known connection vectors according to `tramp-cache'."
    (let (result)
      (maphash
       (lambda (key value)
	 (when (and (vectorp key) (null (aref key 3)))
	   (add-to-list 'result key)))
       tramp-cache-data)
      result))

(defun tramp-dump-connection-properties ()
  "Write persistent connection properties into file `tramp-persistency-file-name'."
  ;; We shouldn't fail, otherwise (X)Emacs might not be able to be closed.
  (ignore-errors
    (when (and (hash-table-p tramp-cache-data)
	       (not (zerop (hash-table-count tramp-cache-data)))
	       tramp-cache-data-changed
	       (stringp tramp-persistency-file-name))
      (let ((cache (copy-hash-table tramp-cache-data)))
	;; Remove temporary data.  If there is the key "login-as", we
	;; don't save either, because all other properties might
	;; depend on the login name, and we want to give the
	;; possibility to use another login name later on.
	(maphash
	 (lambda (key value)
	   (if (and (vectorp key)
		    (not (tramp-file-name-localname key))
		    (not (gethash "login-as" value)))
	       (progn
		 (remhash "process-name" value)
		 (remhash "process-buffer" value)
		 (remhash "first-password-request" value))
	     (remhash key cache)))
	 cache)
	;; Dump it.
	(with-temp-buffer
	  (insert
	   ";; -*- emacs-lisp -*-"
	   ;; `time-stamp-string' might not exist in all (X)Emacs flavors.
	   (condition-case nil
	       (progn
		 (format
		  " <%s %s>\n"
		  (time-stamp-string "%02y/%02m/%02d %02H:%02M:%02S")
		  tramp-persistency-file-name))
	     (error "\n"))
	   ";; Tramp connection history.  Don't change this file.\n"
	   ";; You can delete it, forcing Tramp to reapply the checks.\n\n"
	   (with-output-to-string
	     (pp (read (format "(%s)" (tramp-cache-print cache))))))
	  (write-region
	   (point-min) (point-max) tramp-persistency-file-name))))))

(unless noninteractive
  (add-hook 'kill-emacs-hook 'tramp-dump-connection-properties))
(add-hook 'tramp-cache-unload-hook
	  (lambda ()
	    (remove-hook 'kill-emacs-hook
			 'tramp-dump-connection-properties)))

;;;###tramp-autoload
(defun tramp-parse-connection-properties (method)
  "Return a list of (user host) tuples allowed to access for METHOD.
This function is added always in `tramp-get-completion-function'
for all methods.  Resulting data are derived from connection history."
  (let (res)
    (maphash
     (lambda (key value)
       (if (and (vectorp key)
		(string-equal method (tramp-file-name-method key))
		(not (tramp-file-name-localname key)))
	   (push (list (tramp-file-name-user key)
		       (tramp-file-name-host key))
		 res)))
     tramp-cache-data)
    res))

;; Read persistent connection history.
(when (and (stringp tramp-persistency-file-name)
	   (zerop (hash-table-count tramp-cache-data))
	   ;; When "emacs -Q" has been called, both variables are nil.
	   ;; We do not load the persistency file then, in order to
	   ;; have a clean test environment.
	   (or (and (boundp 'init-file-user) (symbol-value 'init-file-user))
	       (and (boundp 'site-run-file) (symbol-value 'site-run-file))))
  (condition-case err
      (with-temp-buffer
	(insert-file-contents tramp-persistency-file-name)
	(let ((list (read (current-buffer)))
	      element key item)
	  (while (setq element (pop list))
	    (setq key (pop element))
	    (while (setq item (pop element))
	      (tramp-set-connection-property key (pop item) (car item)))))
	(setq tramp-cache-data-changed nil))
    (file-error
     ;; Most likely because the file doesn't exist yet.  No message.
     (clrhash tramp-cache-data))
    (error
     ;; File is corrupted.
     (message "Tramp persistency file '%s' is corrupted: %s"
	      tramp-persistency-file-name (error-message-string err))
     (clrhash tramp-cache-data))))

(add-hook 'tramp-unload-hook
	  (lambda ()
	    (unload-feature 'tramp-cache 'force)))

(provide 'tramp-cache)

;;; tramp-cache.el ends here
