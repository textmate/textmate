;;; mail-source.el --- functions for fetching mail

;; Copyright (C) 1999-2012  Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
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

;; For Emacs <22.2 and XEmacs.
(eval-and-compile
  (unless (fboundp 'declare-function) (defmacro declare-function (&rest r))))

(require 'format-spec)
(eval-when-compile
  (require 'cl)
  (require 'imap))
(autoload 'auth-source-search "auth-source")
(autoload 'pop3-movemail "pop3")
(autoload 'pop3-get-message-count "pop3")
(autoload 'nnheader-cancel-timer "nnheader")
(require 'mm-util)
(require 'message) ;; for `message-directory'

(defvar display-time-mail-function)

(defgroup mail-source nil
  "The mail-fetching library."
  :version "21.1"
  :group 'gnus)

;; Define these at compile time to avoid dragging in imap always.
(defconst mail-source-imap-authenticators
  (eval-when-compile
    (mapcar (lambda (a)
	      (list 'const (car a)))
     imap-authenticator-alist)))
(defconst mail-source-imap-streams
  (eval-when-compile
    (mapcar (lambda (a)
	      (list 'const (car a)))
     imap-stream-alist)))

(defcustom mail-sources '((file))
  "Where the mail backends will look for incoming mail.
This variable is a list of mail source specifiers.
See Info node `(gnus)Mail Source Specifiers'."
  :group 'mail-source
  :version "23.1" ;; No Gnus
  :link '(custom-manual "(gnus)Mail Source Specifiers")
  :type `(choice
	  (const :tag "None" nil)
	  (repeat :tag "List"
	   (choice :format "%[Value Menu%] %v"
		   :value (file)
		   (cons :tag "Group parameter `mail-source'"
			 (const :format "" group))
		   (cons :tag "Spool file"
			 (const :format "" file)
			 (checklist :tag "Options" :greedy t
				    (group :inline t
					   (const :format "" :value :path)
					   file)))
		   (cons :tag "Several files in a directory"
			 (const :format "" directory)
			 (checklist :tag "Options" :greedy t
				    (group :inline t
					   (const :format "" :value :path)
					   (directory :tag "Path"))
				    (group :inline t
					   (const :format "" :value :suffix)
					   (string :tag "Suffix"))
				    (group :inline t
					   (const :format "" :value :predicate)
					   (function :tag "Predicate"))
				    (group :inline t
					   (const :format "" :value :prescript)
					   (choice :tag "Prescript"
						   :value nil
						   (string :format "%v")
						   (function :format "%v")))
				    (group :inline t
					   (const :format "" :value :postscript)
					   (choice :tag "Postscript"
						   :value nil
						   (string :format "%v")
						   (function :format "%v")))
				    (group :inline t
					   (const :format "" :value :plugged)
					   (boolean :tag "Plugged"))))
		   (cons :tag "POP3 server"
			 (const :format "" pop)
			 (checklist :tag "Options" :greedy t
				    (group :inline t
					   (const :format "" :value :server)
					   (string :tag "Server"))
				    (group :inline t
					   (const :format "" :value :port)
					   (choice :tag "Port"
						   :value "pop3"
						   (integer :format "%v")
						   (string :format "%v")))
				    (group :inline t
					   (const :format "" :value :user)
					   (string :tag "User"))
				    (group :inline t
					   (const :format "" :value :password)
					   (string :tag "Password"))
				    (group :inline t
					   (const :format "" :value :program)
					   (string :tag "Program"))
				    (group :inline t
					   (const :format "" :value :prescript)
					   (choice :tag "Prescript"
						   :value nil
						   (string :format "%v")
						   (function :format "%v")
						   (const :tag "None" nil)))
				    (group :inline t
					   (const :format "" :value :postscript)
					   (choice :tag "Postscript"
						   :value nil
						   (string :format "%v")
						   (function :format "%v")
						   (const :tag "None" nil)))
				    (group :inline t
					   (const :format "" :value :function)
					   (function :tag "Function"))
				    (group :inline t
					   (const :format ""
						  :value :authentication)
					   (choice :tag "Authentication"
						   :value apop
						   (const password)
						   (const apop)))
				    (group :inline t
					   (const :format "" :value :plugged)
					   (boolean :tag "Plugged"))
				    (group :inline t
					   (const :format "" :value :stream)
					   (choice :tag "Stream"
						   :value nil
						   (const :tag "Clear" nil)
						   (const starttls)
						   (const :tag "SSL/TLS" ssl)))))
		   (cons :tag "Maildir (qmail, postfix...)"
			 (const :format "" maildir)
			 (checklist :tag "Options" :greedy t
				    (group :inline t
					   (const :format "" :value :path)
					   (directory :tag "Path"))
				    (group :inline t
					   (const :format "" :value :plugged)
					   (boolean :tag "Plugged"))))
		   (cons :tag "IMAP server"
			 (const :format "" imap)
			 (checklist :tag "Options" :greedy t
				    (group :inline t
					   (const :format "" :value :server)
					   (string :tag "Server"))
				    (group :inline t
					   (const :format "" :value :port)
					   (choice :tag "Port"
						   :value 143
						   integer string))
				    (group :inline t
					   (const :format "" :value :user)
					   (string :tag "User"))
				    (group :inline t
					   (const :format "" :value :password)
					   (string :tag "Password"))
				    (group :inline t
					   (const :format "" :value :stream)
					   (choice :tag "Stream"
						   :value network
						   ,@mail-source-imap-streams))
				    (group :inline t
					   (const :format "" :value :program)
					   (string :tag "Program"))
				    (group :inline t
					   (const :format ""
						  :value :authenticator)
					   (choice :tag "Authenticator"
						   :value login
						   ,@mail-source-imap-authenticators))
				    (group :inline t
					   (const :format "" :value :mailbox)
					   (string :tag "Mailbox"
						   :value "INBOX"))
				    (group :inline t
					   (const :format "" :value :predicate)
					   (string :tag "Predicate"
						   :value "UNSEEN UNDELETED"))
				    (group :inline t
					   (const :format "" :value :fetchflag)
					   (string :tag "Fetchflag"
						   :value  "\\Deleted"))
				    (group :inline t
					   (const :format ""
						  :value :dontexpunge)
					   (boolean :tag "Dontexpunge"))
				    (group :inline t
					   (const :format "" :value :plugged)
					   (boolean :tag "Plugged"))))))))

(defcustom mail-source-ignore-errors nil
  "*Ignore errors when querying mail sources.
If nil, the user will be prompted when an error occurs.  If non-nil,
the error will be ignored."
  :version "22.1"
  :group 'mail-source
  :type 'boolean)

(defcustom mail-source-primary-source nil
  "*Primary source for incoming mail.
If non-nil, this maildrop will be checked periodically for new mail."
  :group 'mail-source
  :type 'sexp)

(defcustom mail-source-flash t
  "*If non-nil, flash periodically when mail is available."
  :group 'mail-source
  :type 'boolean)

(defcustom mail-source-crash-box "~/.emacs-mail-crash-box"
  "File where mail will be stored while processing it."
  :group 'mail-source
  :type 'file)

(defcustom mail-source-directory message-directory
  "Directory where incoming mail source files (if any) will be stored."
  :group 'mail-source
  :type 'directory)

(defcustom mail-source-default-file-modes 384
  "Set the mode bits of all new mail files to this integer."
  :group 'mail-source
  :type 'integer)

(defcustom mail-source-delete-incoming
  10 ;; development versions
  ;; 2 ;; released versions
  "If non-nil, delete incoming files after handling.
If t, delete immediately, if nil, never delete.  If a positive number, delete
files older than number of days.

Removing of old files happens in `mail-source-callback', i.e. no
old incoming files will be deleted unless you receive new mail.
You may also set this variable to nil and call
`mail-source-delete-old-incoming' interactively."
  :group 'mail-source
  :version "22.2" ;; No Gnus / Gnus 5.10.10 (default changed)
  :type '(choice (const :tag "immediately" t)
		 (const :tag "never" nil)
		 (integer :tag "days")))

(defcustom mail-source-delete-old-incoming-confirm nil
  "If non-nil, ask for confirmation before deleting old incoming files.
This variable only applies when `mail-source-delete-incoming' is a positive
number."
  :version "22.2" ;; No Gnus / Gnus 5.10.10 (default changed)
  :group 'mail-source
  :type 'boolean)

(defcustom mail-source-incoming-file-prefix "Incoming"
  "Prefix for file name for storing incoming mail"
  :group 'mail-source
  :type 'string)

(defcustom mail-source-report-new-mail-interval 5
  "Interval in minutes between checks for new mail."
  :group 'mail-source
  :type 'number)

(defcustom mail-source-idle-time-delay 5
  "Number of idle seconds to wait before checking for new mail."
  :group 'mail-source
  :type 'number)

(defcustom mail-source-movemail-program nil
  "If non-nil, name of program for fetching new mail."
  :version "22.1"
  :group 'mail-source
  :type '(choice (const nil) string))

;;; Internal variables.

(defvar mail-source-string ""
  "A dynamically bound string that says what the current mail source is.")

(defvar mail-source-new-mail-available nil
  "Flag indicating when new mail is available.")

(eval-and-compile
  (defvar mail-source-common-keyword-map
    '((:plugged))
    "Mapping from keywords to default values.
Common keywords should be listed here.")

  (defvar mail-source-keyword-map
    '((file
       (:prescript)
       (:prescript-delay)
       (:postscript)
       (:path (or (getenv "MAIL")
		  (expand-file-name (user-login-name) rmail-spool-directory))))
      (directory
       (:prescript)
       (:prescript-delay)
       (:postscript)
       (:path)
       (:suffix ".spool")
       (:predicate identity))
      (pop
       (:prescript)
       (:prescript-delay)
       (:postscript)
       ;; note server and port need to come before user and password
       (:server (getenv "MAILHOST"))
       (:port 110)
       (:user (or (user-login-name) (getenv "LOGNAME") (getenv "USER")))
       (:program)
       (:function)
       (:password)
       (:authentication password)
       (:stream nil))
      (maildir
       (:path (or (getenv "MAILDIR") "~/Maildir/"))
       (:subdirs ("cur" "new"))
       (:function))
      (imap
       ;; note server and port need to come before user and password
       (:server (getenv "MAILHOST"))
       (:port)
       (:stream)
       (:program)
       (:authentication)
       (:user (or (user-login-name) (getenv "LOGNAME") (getenv "USER")))
       (:password)
       (:mailbox "INBOX")
       (:predicate "UNSEEN UNDELETED")
       (:fetchflag "\\Deleted")
       (:prescript)
       (:prescript-delay)
       (:postscript)
       (:dontexpunge)))
    "Mapping from keywords to default values.
All keywords that can be used must be listed here."))

(defvar mail-source-fetcher-alist
  '((file mail-source-fetch-file)
    (directory mail-source-fetch-directory)
    (pop mail-source-fetch-pop)
    (maildir mail-source-fetch-maildir)
    (imap mail-source-fetch-imap))
  "A mapping from source type to fetcher function.")

(defvar mail-source-password-cache nil)

(defvar mail-source-plugged t)

;;; Functions

(eval-and-compile
  (defun mail-source-strip-keyword (keyword)
    "Strip the leading colon off the KEYWORD."
    (intern (substring (symbol-name keyword) 1))))

;; generate a list of variable names paired with nil values
;; suitable for usage in a `let' form
(eval-and-compile
  (defun mail-source-bind-1 (type)
    (let* ((defaults (cdr (assq type mail-source-keyword-map)))
	   default bind)
      (while (setq default (pop defaults))
	(push (list (mail-source-strip-keyword (car default))
		    nil)
	      bind))
      bind)))

(defmacro mail-source-bind (type-source &rest body)
  "Return a `let' form that binds all variables in source TYPE.
TYPE-SOURCE is a list where the first element is the TYPE, and
the second variable is the SOURCE.
At run time, the mail source specifier SOURCE will be inspected,
and the variables will be set according to it.  Variables not
specified will be given default values.

The user and password will be loaded from the auth-source values
if those are available.  They override the original user and
password in a second `let' form.

After this is done, BODY will be executed in the scope
of the second `let' form.

The variables bound and their default values are described by
the `mail-source-keyword-map' variable."
  `(let* ,(mail-source-bind-1 (car type-source))
     (mail-source-set-1 ,(cadr type-source))
     ,@body))

(put 'mail-source-bind 'lisp-indent-function 1)
(put 'mail-source-bind 'edebug-form-spec '(sexp body))

(defun mail-source-set-1 (source)
  (let* ((type (pop source))
         (defaults (cdr (assq type mail-source-keyword-map)))
         (search '(:max 1))
         found default value keyword auth-info user-auth pass-auth)

    ;; append to the search the useful info from the source and the defaults:
    ;; user, host, and port

    ;; the msname is the mail-source parameter
    (dolist (msname '(:server :user :port))
      ;; the asname is the auth-source parameter
      (let* ((asname (case msname
                       (:server :host)  ; auth-source uses :host
                       (t msname)))
             ;; this is the mail-source default
             (msdef1 (or (plist-get source msname)
                         (nth 1 (assoc msname defaults))))
             ;; ...evaluated
             (msdef (mail-source-value msdef1)))
        (setq search (append (list asname
                                   (if msdef msdef t))
                             search))))
    ;; if the port is unknown yet, get it from the mail-source type
    (unless (plist-get search :port)
      (setq search (append (list :port (symbol-name type)))))

    (while (setq default (pop defaults))
      ;; for each default :SYMBOL, set SYMBOL to the plist value for :SYMBOL
      ;; using `mail-source-value' to evaluate the plist value
      (set (mail-source-strip-keyword (setq keyword (car default)))
           ;; note the following reasons for this structure:
           ;; 1) the auth-sources user and password override everything
           ;; 2) it avoids macros, so it's cleaner
           ;; 3) it falls through to the mail-sources and then default values
           (cond
            ((and
             (eq keyword :user)
             (setq user-auth (plist-get
                              ;; cache the search result in `found'
                              (or found
                                  (setq found (nth 0 (apply 'auth-source-search
                                                            search))))
                              :user)))
             user-auth)
            ((and
              (eq keyword :password)
              (setq pass-auth (plist-get
                               ;; cache the search result in `found'
                               (or found
                                   (setq found (nth 0 (apply 'auth-source-search
                                                             search))))
                               :secret)))
             ;; maybe set the password to the return of the :secret function
             (if (functionp pass-auth)
                 (setq pass-auth (funcall pass-auth))
               pass-auth))
            (t (if (setq value (plist-get source keyword))
                 (mail-source-value value)
               (mail-source-value (cadr default)))))))))

(eval-and-compile
  (defun mail-source-bind-common-1 ()
    (let* ((defaults mail-source-common-keyword-map)
	   default bind)
      (while (setq default (pop defaults))
	(push (list (mail-source-strip-keyword (car default))
		    nil)
	      bind))
      bind)))

(defun mail-source-set-common-1 (source)
  (let* ((type (pop source))
	 (defaults mail-source-common-keyword-map)
	 (defaults-1 (cdr (assq type mail-source-keyword-map)))
	 default value keyword)
    (while (setq default (pop defaults))
      (set (mail-source-strip-keyword (setq keyword (car default)))
	   (if (setq value (plist-get source keyword))
	       (mail-source-value value)
	     (if (setq value (assq  keyword defaults-1))
		 (mail-source-value (cadr value))
	       (mail-source-value (cadr default))))))))

(defmacro mail-source-bind-common (source &rest body)
  "Return a `let' form that binds all common variables.
See `mail-source-bind'."
  `(let ,(mail-source-bind-common-1)
     (mail-source-set-common-1 source)
     ,@body))

(put 'mail-source-bind-common 'lisp-indent-function 1)
(put 'mail-source-bind-common 'edebug-form-spec '(sexp body))

(defun mail-source-value (value)
  "Return the value of VALUE."
  (cond
   ;; String
   ((stringp value)
    value)
   ;; Function
   ((and (listp value) (symbolp (car value)) (fboundp (car value)))
    (eval value))
   ;; Just return the value.
   (t
    value)))

(autoload 'nnheader-message "nnheader")

(defun mail-source-fetch (source callback &optional method)
  "Fetch mail from SOURCE and call CALLBACK zero or more times.
CALLBACK will be called with the name of the file where (some of)
the mail from SOURCE is put.
Return the number of files that were found."
  (mail-source-bind-common source
    (if (or mail-source-plugged plugged)
	(save-excursion
	  ;; Special-case the `file' handler since it's so common and
	  ;; just adds noise.
	  (when (or (not (eq (car source) 'file))
		    (mail-source-bind (file source)
		      (file-exists-p path)))
	    (nnheader-message 4 "%sReading incoming mail from %s..."
			      (if method
				  (format "%s: " method)
				"")
			      (car source)))
	  (let ((function (cadr (assq (car source) mail-source-fetcher-alist)))
		(found 0))
	    (unless function
	      (error "%S is an invalid mail source specification" source))
	    ;; If there's anything in the crash box, we do it first.
	    (when (file-exists-p mail-source-crash-box)
	      (message "Processing mail from %s..." mail-source-crash-box)
	      (setq found (mail-source-callback
			   callback mail-source-crash-box))
	      (mail-source-delete-crash-box))
	    (+ found
	       (if (or debug-on-quit debug-on-error)
		   (funcall function source callback)
		 (condition-case err
		     (funcall function source callback)
		   (error
		    (if (and (not mail-source-ignore-errors)
			     (not
			      (yes-or-no-p
			       (format "Mail source %s error (%s).  Continue? "
				       (if (memq ':password source)
					   (let ((s (copy-sequence source)))
					     (setcar (cdr (memq ':password s))
						     "********")
					     s)
					 source)
				       (cadr err)))))
		      (error "Cannot get new mail"))
		    0)))))))))

(declare-function gnus-message "gnus-util" (level &rest args))

(defun mail-source-delete-old-incoming (&optional age confirm)
  "Remove incoming files older than AGE days.
If CONFIRM is non-nil, ask for confirmation before removing a file."
  (interactive "P")
  (require 'gnus-util)
  (let* ((high2days (/ 65536.0 60 60 24));; convert high bits to days
	 (low2days  (/ 1.0 65536.0))     ;; convert low bits to days
	 (diff (if (natnump age) age 30));; fallback, if no valid AGE given
	 currday files)
    (setq files (directory-files
		 mail-source-directory t
		 (concat "\\`"
			 (regexp-quote mail-source-incoming-file-prefix)))
	  currday (* (car (current-time)) high2days)
	  currday (+ currday (* low2days (nth 1 (current-time)))))
    (while files
      (let* ((ffile (car files))
	     (bfile (gnus-replace-in-string
		     ffile "\\`.*/\\([^/]+\\)\\'" "\\1"))
	     (filetime (nth 5 (file-attributes ffile)))
	     (fileday (* (car filetime) high2days))
	     (fileday (+ fileday (* low2days (nth 1 filetime)))))
	(setq files (cdr files))
	(when (and (> (- currday fileday) diff)
		   (if confirm
		       (y-or-n-p
			(format "\
Delete old (> %s day(s)) incoming mail file `%s'? " diff bfile))
		     (gnus-message 8 "\
Deleting old (> %s day(s)) incoming mail file `%s'." diff bfile)
		     t))
	  (delete-file ffile))))))

(defun mail-source-callback (callback info)
  "Call CALLBACK on the mail file.  Pass INFO on to CALLBACK."
  (if (or (not (file-exists-p mail-source-crash-box))
	  (zerop (nth 7 (file-attributes mail-source-crash-box))))
      (progn
	(when (file-exists-p mail-source-crash-box)
	  (delete-file mail-source-crash-box))
	0)
    (funcall callback mail-source-crash-box info)))

(autoload 'gnus-float-time "gnus-util")

(defvar mail-source-incoming-last-checked-time nil)

(defun mail-source-delete-crash-box ()
  (when (file-exists-p mail-source-crash-box)
    ;; Delete or move the incoming mail out of the way.
    (if (eq mail-source-delete-incoming t)
	(delete-file mail-source-crash-box)
      (let ((incoming
	     (mm-make-temp-file
	      (expand-file-name
	       mail-source-incoming-file-prefix
	       mail-source-directory))))
	(unless (file-exists-p (file-name-directory incoming))
	  (make-directory (file-name-directory incoming) t))
	(rename-file mail-source-crash-box incoming t)
	;; remove old incoming files?
	(when (natnump mail-source-delete-incoming)
	  ;; Don't check for old incoming files more than once per day to
	  ;; save a lot of file accesses.
	  (when (or (null mail-source-incoming-last-checked-time)
		    (> (gnus-float-time
			(time-since mail-source-incoming-last-checked-time))
		       (* 24 60 60)))
	    (setq mail-source-incoming-last-checked-time (current-time))
	    (mail-source-delete-old-incoming
	     mail-source-delete-incoming
	     mail-source-delete-old-incoming-confirm)))))))

(defun mail-source-movemail (from to)
  "Move FROM to TO using movemail."
  (if (not (file-writable-p to))
      (error "Can't write to crash box %s.  Not moving mail" to)
    (let ((to (file-truename (expand-file-name to)))
	  errors result)
      (setq to (file-truename to)
	    from (file-truename from))
      ;; Set TO if have not already done so, and rename or copy
      ;; the file FROM to TO if and as appropriate.
      (cond
       ((file-exists-p to)
	;; The crash box exists already.
	t)
       ((not (file-exists-p from))
	;; There is no inbox.
	(setq to nil))
       ((zerop (nth 7 (file-attributes from)))
	;; Empty file.
	(setq to nil))
       (t
	;; If getting from mail spool directory, use movemail to move
	;; rather than just renaming, so as to interlock with the
	;; mailer.
	(unwind-protect
	    (save-excursion
	      (setq errors (generate-new-buffer " *mail source loss*"))
	      (let ((default-directory "/"))
		(setq result
		      (apply
		       'call-process
		       (append
			(list
			 (or mail-source-movemail-program
			     (expand-file-name "movemail" exec-directory))
			 nil errors nil from to)))))
	      (when (file-exists-p to)
		(set-file-modes to mail-source-default-file-modes))
	      (if (and (or (not (buffer-modified-p errors))
			   (zerop (buffer-size errors)))
		       (and (numberp result)
			    (zerop result)))
		  ;; No output => movemail won.
		  t
		(set-buffer errors)
		;; There may be a warning about older revisions.  We
		;; ignore that.
		(goto-char (point-min))
		(if (search-forward "older revision" nil t)
		    t
		  ;; Probably a real error.
		  (subst-char-in-region (point-min) (point-max) ?\n ?\  )
		  (goto-char (point-max))
		  (skip-chars-backward " \t")
		  (delete-region (point) (point-max))
		  (goto-char (point-min))
		  (when (looking-at "movemail: ")
		    (delete-region (point-min) (match-end 0)))
		  ;; Result may be a signal description string.
		  (unless (yes-or-no-p
			   (format "movemail: %s (%s return).  Continue? "
				   (buffer-string) result))
		    (error "%s" (buffer-string)))
		  (setq to nil)))))))
      (when (and errors
		 (buffer-name errors))
	(kill-buffer errors))
      ;; Return whether we moved successfully or not.
      to)))

(defun mail-source-movemail-and-remove (from to)
  "Move FROM to TO using movemail, then remove FROM if empty."
  (or (not (mail-source-movemail from to))
      (not (zerop (nth 7 (file-attributes from))))
      (delete-file from)))

(defun mail-source-fetch-with-program (program)
  (eq 0 (call-process shell-file-name nil nil nil
		      shell-command-switch program)))

(defun mail-source-run-script (script spec &optional delay)
  (when script
    (if (functionp script)
	(funcall script)
      (mail-source-call-script
       (format-spec script spec))))
  (when delay
    (sleep-for delay)))

(defun mail-source-call-script (script)
  (let ((background nil)
	(stderr (get-buffer-create " *mail-source-stderr*"))
	result)
    (when (string-match "& *$" script)
      (setq script (substring script 0 (match-beginning 0))
	    background 0))
    (setq result
	  (call-process shell-file-name nil background nil
			shell-command-switch script))
    (when (and result
	       (not (zerop result)))
      (set-buffer stderr)
      (message "Mail source error: %s" (buffer-string)))
    (kill-buffer stderr)))

;;;
;;; Different fetchers
;;;

(defun mail-source-fetch-file (source callback)
  "Fetcher for single-file sources."
  (mail-source-bind (file source)
    (mail-source-run-script
     prescript (format-spec-make ?t mail-source-crash-box)
     prescript-delay)
    (let ((mail-source-string (format "file:%s" path)))
      (if (mail-source-movemail path mail-source-crash-box)
	  (prog1
	      (mail-source-callback callback path)
	    (mail-source-run-script
	     postscript (format-spec-make ?t mail-source-crash-box))
	    (mail-source-delete-crash-box))
	0))))

(defun mail-source-fetch-directory (source callback)
  "Fetcher for directory sources."
  (mail-source-bind (directory source)
    (mail-source-run-script
     prescript (format-spec-make ?t path) prescript-delay)
    (let ((found 0)
	  (mail-source-string (format "directory:%s" path)))
      (dolist (file (directory-files
		     path t (concat (regexp-quote suffix) "$")))
	(when (and (file-regular-p file)
		   (funcall predicate file)
		   (mail-source-movemail file mail-source-crash-box))
	  (incf found (mail-source-callback callback file))
	  (mail-source-run-script postscript (format-spec-make ?t path))
	  (mail-source-delete-crash-box)))
      found)))

(defun mail-source-fetch-pop (source callback)
  "Fetcher for single-file sources."
  (mail-source-bind (pop source)
    ;; fixme: deal with stream type in format specs
    (mail-source-run-script
     prescript
     (format-spec-make ?p password ?t mail-source-crash-box
		       ?s server ?P port ?u user)
     prescript-delay)
    (let ((from (format "%s:%s:%s" server user port))
	  (mail-source-string (format "pop:%s@%s" user server))
	  result)
      (when (eq authentication 'password)
	(setq password
	      (or password
		  (cdr (assoc from mail-source-password-cache))
		  (read-passwd
		   (format "Password for %s at %s: " user server)))))
      (when server
	(setenv "MAILHOST" server))
      (setq result
	    (cond
	     (program
	      (mail-source-fetch-with-program
	       (format-spec
		program
		(format-spec-make ?p password ?t mail-source-crash-box
				  ?s server ?P port ?u user))))
	     (function
	      (funcall function mail-source-crash-box))
	     ;; The default is to use pop3.el.
	     (t
	      (require 'pop3)
	      (let ((pop3-password password)
		    (pop3-maildrop user)
		    (pop3-mailhost server)
		    (pop3-port port)
		    (pop3-authentication-scheme
		     (if (eq authentication 'apop) 'apop 'pass))
		    (pop3-stream-type stream))
		(if (or debug-on-quit debug-on-error)
		    (save-excursion (pop3-movemail mail-source-crash-box))
		  (condition-case err
		      (save-excursion (pop3-movemail mail-source-crash-box))
		    (error
		     ;; We nix out the password in case the error
		     ;; was because of a wrong password being given.
		     (setq mail-source-password-cache
			   (delq (assoc from mail-source-password-cache)
				 mail-source-password-cache))
		     (signal (car err) (cdr err)))))))))
      (if result
	  (progn
	    (when (eq authentication 'password)
	      (unless (assoc from mail-source-password-cache)
		(push (cons from password) mail-source-password-cache)))
	    (prog1
		(mail-source-callback callback server)
	      ;; Update display-time's mail flag, if relevant.
	      (if (equal source mail-source-primary-source)
		  (setq mail-source-new-mail-available nil))
	      (mail-source-run-script
	       postscript
	       (format-spec-make ?p password ?t mail-source-crash-box
				 ?s server ?P port ?u user))
	      (mail-source-delete-crash-box)))
	;; We nix out the password in case the error
	;; was because of a wrong password being given.
	(setq mail-source-password-cache
	      (delq (assoc from mail-source-password-cache)
		    mail-source-password-cache))
	0))))

(defun mail-source-check-pop (source)
  "Check whether there is new mail."
  (mail-source-bind (pop source)
    (let ((from (format "%s:%s:%s" server user port))
	  (mail-source-string (format "pop:%s@%s" user server))
	  result)
      (when (eq authentication 'password)
	(setq password
	      (or password
		  (cdr (assoc from mail-source-password-cache))
		  (read-passwd
		   (format "Password for %s at %s: " user server))))
	(unless (assoc from mail-source-password-cache)
	  (push (cons from password) mail-source-password-cache)))
      (when server
	(setenv "MAILHOST" server))
      (setq result
	    (cond
	     ;; No easy way to check whether mail is waiting for these.
	     (program)
	     (function)
	     ;; The default is to use pop3.el.
	     (t
	      (require 'pop3)
	      (let ((pop3-password password)
		    (pop3-maildrop user)
		    (pop3-mailhost server)
		    (pop3-port port)
		    (pop3-authentication-scheme
		     (if (eq authentication 'apop) 'apop 'pass)))
		(if (or debug-on-quit debug-on-error)
		    (save-excursion (pop3-get-message-count))
		  (condition-case err
		      (save-excursion (pop3-get-message-count))
		    (error
		     ;; We nix out the password in case the error
		     ;; was because of a wrong password being given.
		     (setq mail-source-password-cache
			   (delq (assoc from mail-source-password-cache)
				 mail-source-password-cache))
		     (signal (car err) (cdr err)))))))))
      (if result
	  ;; Inform display-time that we have new mail.
	  (setq mail-source-new-mail-available (> result 0))
	;; We nix out the password in case the error
	;; was because of a wrong password being given.
	(setq mail-source-password-cache
	      (delq (assoc from mail-source-password-cache)
		    mail-source-password-cache)))
      result)))

(defun mail-source-touch-pop ()
  "Open and close a POP connection shortly.
POP server should be defined in `mail-source-primary-source' (which is
preferred) or `mail-sources'.  You may use it for the POP-before-SMTP
authentication.  To do that, you need to set the
`message-send-mail-function' variable as `message-smtpmail-send-it'
and put the following line in your ~/.gnus.el file:

\(add-hook 'message-send-mail-hook 'mail-source-touch-pop)

See the Gnus manual for details."
  (let ((sources (if mail-source-primary-source
		     (list mail-source-primary-source)
		   mail-sources)))
    (while sources
      (if (eq 'pop (car (car sources)))
	  (mail-source-check-pop (car sources)))
      (setq sources (cdr sources)))))

(defun mail-source-new-mail-p ()
  "Handler for `display-time' to indicate when new mail is available."
  ;; Flash (ie. ring the visible bell) if mail is available.
  (if (and mail-source-flash mail-source-new-mail-available)
      (let ((visible-bell t))
	(ding)))
  ;; Only report flag setting; flag is updated on a different schedule.
  mail-source-new-mail-available)


(defvar mail-source-report-new-mail nil)
(defvar mail-source-report-new-mail-timer nil)
(defvar mail-source-report-new-mail-idle-timer nil)

(defun mail-source-start-idle-timer ()
  ;; Start our idle timer if necessary, so we delay the check until the
  ;; user isn't typing.
  (unless mail-source-report-new-mail-idle-timer
    (setq mail-source-report-new-mail-idle-timer
	  (run-with-idle-timer
	   mail-source-idle-time-delay
	   nil
	   (lambda ()
	     (unwind-protect
		 (mail-source-check-pop mail-source-primary-source)
	       (setq mail-source-report-new-mail-idle-timer nil)))))
    ;; Since idle timers created when Emacs is already in the idle
    ;; state don't get activated until Emacs _next_ becomes idle, we
    ;; need to force our timer to be considered active now.  We do
    ;; this by being naughty and poking the timer internals directly
    ;; (element 0 of the vector is nil if the timer is active).
    (aset mail-source-report-new-mail-idle-timer 0 nil)))

(defun mail-source-report-new-mail (arg)
  "Toggle whether to report when new mail is available.
This only works when `display-time' is enabled."
  (interactive "P")
  (if (not mail-source-primary-source)
      (error "Need to set `mail-source-primary-source' to check for new mail"))
  (let ((on (if (null arg)
		(not mail-source-report-new-mail)
	      (> (prefix-numeric-value arg) 0))))
    (setq mail-source-report-new-mail on)
    (and mail-source-report-new-mail-timer
	 (nnheader-cancel-timer mail-source-report-new-mail-timer))
    (and mail-source-report-new-mail-idle-timer
	 (nnheader-cancel-timer mail-source-report-new-mail-idle-timer))
    (setq mail-source-report-new-mail-timer nil)
    (setq mail-source-report-new-mail-idle-timer nil)
    (if on
	(progn
	  (require 'time)
	  ;; display-time-mail-function is an Emacs feature.
	  (setq display-time-mail-function #'mail-source-new-mail-p)
	  ;; Set up the main timer.
	  (setq mail-source-report-new-mail-timer
		(run-at-time
		 (* 60 mail-source-report-new-mail-interval)
		 (* 60 mail-source-report-new-mail-interval)
		 #'mail-source-start-idle-timer))
	  ;; When you get new mail, clear "Mail" from the mode line.
	  (add-hook 'nnmail-post-get-new-mail-hook
		    'display-time-event-handler)
	  (message "Mail check enabled"))
      (setq display-time-mail-function nil)
      (remove-hook 'nnmail-post-get-new-mail-hook
		   'display-time-event-handler)
      (message "Mail check disabled"))))

(defun mail-source-fetch-maildir (source callback)
  "Fetcher for maildir sources."
  (mail-source-bind (maildir source)
    (let ((found 0)
	  mail-source-string)
      (unless (string-match "/$" path)
	(setq path (concat path "/")))
      (dolist (subdir subdirs)
	(when (file-directory-p (concat path subdir))
	  (setq mail-source-string (format "maildir:%s%s" path subdir))
	  (dolist (file (directory-files (concat path subdir) t))
	    (when (and (not (file-directory-p file))
		       (not (if function
				;; `function' should return nil if successful.
				(funcall function file mail-source-crash-box)
			      (let ((coding-system-for-write
				     mm-text-coding-system)
				    (coding-system-for-read
				     mm-text-coding-system))
				(with-temp-file mail-source-crash-box
				  (insert-file-contents file)
				  (goto-char (point-min))
;;;				  ;; Unix mail format
;;;				  (unless (looking-at "\n*From ")
;;;				    (insert "From maildir "
;;;					    (current-time-string) "\n"))
;;;				  (while (re-search-forward "^From " nil t)
;;;				    (replace-match ">From "))
;;;				  (goto-char (point-max))
;;;				  (insert "\n\n")
				  ;; MMDF mail format
				  (insert "\001\001\001\001\n"))
				(delete-file file)
				nil))))
	      (incf found (mail-source-callback callback file))
	      (mail-source-delete-crash-box)))))
      found)))

(autoload 'imap-open "imap")
(autoload 'imap-authenticate "imap")
(autoload 'imap-mailbox-select "imap")
(autoload 'imap-mailbox-unselect "imap")
(autoload 'imap-mailbox-close "imap")
(autoload 'imap-search "imap")
(autoload 'imap-fetch "imap")
(autoload 'imap-close "imap")
(autoload 'imap-error-text "imap")
(autoload 'imap-message-flags-add "imap")
(autoload 'imap-list-to-message-set "imap")
(autoload 'imap-range-to-message-set "imap")
(autoload 'nnheader-ms-strip-cr "nnheader")

(autoload 'gnus-compress-sequence "gnus-range")

(defvar mail-source-imap-file-coding-system 'binary
  "Coding system for the crashbox made by `mail-source-fetch-imap'.")

;; Autoloads will bring in imap before this is called.
(declare-function imap-capability "imap" (&optional identifier buffer))

(defun mail-source-fetch-imap (source callback)
  "Fetcher for imap sources."
  (mail-source-bind (imap source)
    (mail-source-run-script
     prescript (format-spec-make ?p password ?t mail-source-crash-box
				 ?s server ?P port ?u user)
     prescript-delay)
    (let ((from (format "%s:%s:%s" server user port))
	  (found 0)
	  (buf (generate-new-buffer " *imap source*"))
	  (mail-source-string (format "imap:%s:%s" server mailbox))
	  (imap-shell-program (or (list program) imap-shell-program))
	  remove)
      (if (and (imap-open server port stream authentication buf)
	       (imap-authenticate
		user (or (cdr (assoc from mail-source-password-cache))
			 password) buf)
	       (imap-mailbox-select mailbox nil buf))
	  (let ((coding-system-for-write mail-source-imap-file-coding-system)
		str)
	    (with-temp-file mail-source-crash-box
	      ;; Avoid converting 8-bit chars from inserted strings to
	      ;; multibyte.
	      (mm-disable-multibyte)
	      ;; remember password
	      (with-current-buffer buf
		(when (and imap-password
			   (not (assoc from mail-source-password-cache)))
		  (push (cons from imap-password) mail-source-password-cache)))
	      ;; if predicate is nil, use all uids
	      (dolist (uid (imap-search (or predicate "1:*") buf))
		(when (setq str
			    (if (imap-capability 'IMAP4rev1 buf)
				(caddar (imap-fetch uid "BODY.PEEK[]"
						    'BODYDETAIL nil buf))
			      (imap-fetch uid "RFC822.PEEK" 'RFC822 nil buf)))
		  (push uid remove)
		  (insert "From imap " (current-time-string) "\n")
		  (save-excursion
		    (insert str "\n\n"))
		  (while (let ((case-fold-search nil))
			   (re-search-forward "^From " nil t))
		    (replace-match ">From "))
		  (goto-char (point-max))))
	      (nnheader-ms-strip-cr))
	    (incf found (mail-source-callback callback server))
	    (mail-source-delete-crash-box)
	    (when (and remove fetchflag)
	      (setq remove (nreverse remove))
	      (imap-message-flags-add
	       (imap-range-to-message-set (gnus-compress-sequence remove))
	       fetchflag nil buf))
	    (if dontexpunge
		(imap-mailbox-unselect buf)
	      (imap-mailbox-close nil buf))
	    (imap-close buf))
	(imap-close buf)
	;; We nix out the password in case the error
	;; was because of a wrong password being given.
	(setq mail-source-password-cache
	      (delq (assoc from mail-source-password-cache)
		    mail-source-password-cache))
	(error "IMAP error: %s" (imap-error-text buf)))
      (kill-buffer buf)
      (mail-source-run-script
       postscript
       (format-spec-make ?p password ?t mail-source-crash-box
			 ?s server ?P port ?u user))
      found)))

(provide 'mail-source)

;;; mail-source.el ends here
