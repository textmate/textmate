;;; tramp-compat.el --- Tramp compatibility functions

;; Copyright (C) 2007-2012 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>
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

;; Tramp's main Emacs version for development is Emacs 24.  This
;; package provides compatibility functions for Emacs 22, Emacs 23,
;; XEmacs 21.4+ and SXEmacs 22.

;;; Code:

(require 'tramp-loaddefs)

(eval-when-compile

  ;; Pacify byte-compiler.
  (require 'cl))

(eval-and-compile

  (require 'advice)
  (require 'custom)
  (require 'format-spec)
  (require 'shell)

  ;; As long as password.el is not part of (X)Emacs, it shouldn't be
  ;; mandatory.
  (if (featurep 'xemacs)
      (load "password" 'noerror)
    (or (require 'password-cache nil 'noerror)
	(require 'password nil 'noerror))) ; Part of contrib.

  ;; auth-source is relatively new.
  (if (featurep 'xemacs)
      (load "auth-source" 'noerror)
    (require 'auth-source nil 'noerror))

  ;; Load the appropriate timer package.
  (if (featurep 'xemacs)
      (require 'timer-funcs)
    (require 'timer))

  ;; We check whether `start-file-process' is bound.
  (unless (fboundp 'start-file-process)

    ;; tramp-util offers integration into other (X)Emacs packages like
    ;; compile.el, gud.el etc.  Not necessary in Emacs 23.
    (eval-after-load "tramp"
      '(require 'tramp-util))

    ;; Make sure that we get integration with the VC package.  When it
    ;; is loaded, we need to pull in the integration module.  Not
    ;; necessary in Emacs 23.
    (eval-after-load "vc"
      (eval-after-load "tramp"
	'(require 'tramp-vc))))

  ;; Avoid byte-compiler warnings if the byte-compiler supports this.
  ;; Currently, XEmacs supports this.
  (when (featurep 'xemacs)
    (unless (boundp 'byte-compile-default-warnings)
      (defvar byte-compile-default-warnings nil))
    (delq 'unused-vars byte-compile-default-warnings))

  ;; `last-coding-system-used' is unknown in XEmacs.
  (unless (boundp 'last-coding-system-used)
    (defvar last-coding-system-used nil))

  ;; `directory-sep-char' is an obsolete variable in Emacs.  But it is
  ;; used in XEmacs, so we set it here and there.  The following is
  ;; needed to pacify Emacs byte-compiler.
  ;; Note that it was removed altogether in Emacs 24.1.
  (when (boundp 'directory-sep-char)
    (defvar byte-compile-not-obsolete-var nil)
    (setq byte-compile-not-obsolete-var 'directory-sep-char)
    ;; Emacs 23.2.
    (defvar byte-compile-not-obsolete-vars nil)
    (setq byte-compile-not-obsolete-vars '(directory-sep-char)))

  ;; `remote-file-name-inhibit-cache' has been introduced with Emacs 24.1.
  ;; Besides `t', `nil', and integer, we use also timestamps (as
  ;; returned by `current-time') internally.
  (defvar remote-file-name-inhibit-cache nil)

  ;; For not existing functions, or functions with a changed argument
  ;; list, there are compiler warnings.  We want to avoid them in
  ;; cases we know what we do.
  (defmacro tramp-compat-funcall (function &rest arguments)
    (if (featurep 'xemacs)
	`(funcall (symbol-function ,function) ,@arguments)
      `(when (or (subrp ,function) (functionp ,function))
	 (with-no-warnings (funcall ,function ,@arguments)))))

  ;; `set-buffer-multibyte' comes from Emacs Leim.
  (unless (fboundp 'set-buffer-multibyte)
    (defalias 'set-buffer-multibyte 'ignore))

  ;; The following functions cannot be aliases of the corresponding
  ;; `tramp-handle-*' functions, because this would bypass the locking
  ;; mechanism.

  ;; `file-remote-p' has been introduced with Emacs 22.  The version
  ;; of XEmacs is not a magic file name function (yet); this is
  ;; corrected in tramp-util.el.  Here it is sufficient if the
  ;; function exists.
  (unless (fboundp 'file-remote-p)
    (defalias 'file-remote-p
      (lambda (file &optional identification connected)
	(when (tramp-tramp-file-p file)
	  (tramp-file-name-handler
	   'file-remote-p file identification connected)))))

  ;; `process-file' does not exist in XEmacs.
  (unless (fboundp 'process-file)
    (defalias 'process-file
      (lambda (program &optional infile buffer display &rest args)
	(when (tramp-tramp-file-p default-directory)
	  (apply
	   'tramp-file-name-handler
	   'process-file program infile buffer display args)))))

  ;; `start-file-process' is new in Emacs 23.
  (unless (fboundp 'start-file-process)
    (defalias 'start-file-process
      (lambda (name buffer program &rest program-args)
	(when (tramp-tramp-file-p default-directory)
	  (apply
	   'tramp-file-name-handler
	   'start-file-process name buffer program program-args)))))

  ;; `set-file-times' is also new in Emacs 23.
  (unless (fboundp 'set-file-times)
    (defalias 'set-file-times
      (lambda (filename &optional time)
	(when (tramp-tramp-file-p filename)
	  (tramp-file-name-handler
	   'set-file-times filename time)))))

  ;; We currently use "[" and "]" in the filename format for IPv6
  ;; hosts of GNU Emacs.  This means that Emacs wants to expand
  ;; wildcards if `find-file-wildcards' is non-nil, and then barfs
  ;; because no expansion could be found.  We detect this situation
  ;; and do something really awful: we have `file-expand-wildcards'
  ;; return the original filename if it can't expand anything.  Let's
  ;; just hope that this doesn't break anything else.
  ;; It is not needed anymore since GNU Emacs 23.2.
  (unless (or (featurep 'xemacs)
	      ;; `featurep' has only one argument in XEmacs.
	      (funcall 'featurep 'files 'remote-wildcards))
    (defadvice file-expand-wildcards
      (around tramp-advice-file-expand-wildcards activate)
      (let ((name (ad-get-arg 0)))
	;; If it's a Tramp file, look if wildcards need to be expanded
	;; at all.
	(if (and
	     (tramp-tramp-file-p name)
	     (not (string-match
		   "[[*?]" (tramp-compat-funcall
			    'file-remote-p name 'localname))))
	    (setq ad-return-value (list name))
	  ;; Otherwise, just run the original function.
	  ad-do-it)))
    (add-hook
     'tramp-unload-hook
     (lambda ()
       (ad-remove-advice
	'file-expand-wildcards 'around 'tramp-advice-file-expand-wildcards)
       (ad-activate 'file-expand-wildcards)))))

;; `with-temp-message' does not exists in XEmacs.
(if (fboundp 'with-temp-message)
    (defalias 'tramp-compat-with-temp-message 'with-temp-message)
  (defmacro tramp-compat-with-temp-message (message &rest body)
    "Display MESSAGE temporarily if non-nil while BODY is evaluated."
    `(progn ,@body)))

;; `font-lock-add-keywords' does not exist in XEmacs.
(defun tramp-compat-font-lock-add-keywords (mode keywords &optional how)
  "Add highlighting KEYWORDS for MODE."
  (ignore-errors
    (tramp-compat-funcall 'font-lock-add-keywords mode keywords how)))

(defsubst tramp-compat-temporary-file-directory ()
  "Return name of directory for temporary files (compat function).
For Emacs, this is the variable `temporary-file-directory', for XEmacs
this is the function `temp-directory'."
  (cond
   ((boundp 'temporary-file-directory) (symbol-value 'temporary-file-directory))
   ((fboundp 'temp-directory) (tramp-compat-funcall 'temp-directory))
   ((let ((d (getenv "TEMP"))) (and d (file-directory-p d)))
    (file-name-as-directory (getenv "TEMP")))
   ((let ((d (getenv "TMP"))) (and d (file-directory-p d)))
    (file-name-as-directory (getenv "TMP")))
   ((let ((d (getenv "TMPDIR"))) (and d (file-directory-p d)))
    (file-name-as-directory (getenv "TMPDIR")))
   ((file-exists-p "c:/temp") (file-name-as-directory "c:/temp"))
   (t (message (concat "Neither `temporary-file-directory' nor "
		       "`temp-directory' is defined -- using /tmp."))
      (file-name-as-directory "/tmp"))))

;; `make-temp-file' exists in Emacs only.  On XEmacs, we use our own
;; implementation with `make-temp-name', creating the temporary file
;; immediately in order to avoid a security hole.
(defsubst tramp-compat-make-temp-file (filename &optional dir-flag)
  "Create a temporary file (compat function).
Add the extension of FILENAME, if existing."
  (let* (file-name-handler-alist
	 (prefix (expand-file-name
		  (symbol-value 'tramp-temp-name-prefix)
		  (tramp-compat-temporary-file-directory)))
	 (extension (file-name-extension filename t))
	 result)
    (condition-case nil
	(setq result
	      (tramp-compat-funcall 'make-temp-file prefix dir-flag extension))
      (error
       ;; We use our own implementation, taken from files.el.
       (while
	   (condition-case ()
	       (progn
		 (setq result (concat (make-temp-name prefix) extension))
		 (if dir-flag
		     (make-directory result)
		   (write-region "" nil result nil 'silent))
		 nil)
	     (file-already-exists t))
	 ;; The file was somehow created by someone else between
	 ;; `make-temp-name' and `write-region', let's try again.
	 nil)))
    result))

;; `most-positive-fixnum' does not exist in XEmacs.
(defsubst tramp-compat-most-positive-fixnum ()
  "Return largest positive integer value (compat function)."
  (cond
   ((boundp 'most-positive-fixnum) (symbol-value 'most-positive-fixnum))
   ;; Default value in XEmacs.
   (t 134217727)))

(defun tramp-compat-decimal-to-octal (i)
  "Return a string consisting of the octal digits of I.
Not actually used.  Use `(format \"%o\" i)' instead?"
  (cond ((< i 0) (error "Cannot convert negative number to octal"))
        ((not (integerp i)) (error "Cannot convert non-integer to octal"))
        ((zerop i) "0")
        (t (concat (tramp-compat-decimal-to-octal (/ i 8))
                   (number-to-string (% i 8))))))

;; Kudos to Gerd Moellmann for this suggestion.
(defun tramp-compat-octal-to-decimal (ostr)
  "Given a string of octal digits, return a decimal number."
  (let ((x (or ostr "")))
    ;; `save-match' is in `tramp-mode-string-to-int' which calls this.
    (unless (string-match "\\`[0-7]*\\'" x)
      (error "Non-octal junk in string `%s'" x))
    (string-to-number ostr 8)))

;; ID-FORMAT does not exists in XEmacs.
(defun tramp-compat-file-attributes (filename &optional id-format)
  "Like `file-attributes' for Tramp files (compat function)."
  (cond
   ((or (null id-format) (eq id-format 'integer))
    (file-attributes filename))
   ((tramp-tramp-file-p filename)
    (tramp-file-name-handler 'file-attributes filename id-format))
   (t (condition-case nil
	  (tramp-compat-funcall 'file-attributes filename id-format)
	(wrong-number-of-arguments (file-attributes filename))))))

;; PRESERVE-UID-GID does not exist in XEmacs.
;; PRESERVE-SELINUX-CONTEXT has been introduced with Emacs 24.1.
(defun tramp-compat-copy-file
  (filename newname &optional ok-if-already-exists keep-date
	    preserve-uid-gid preserve-selinux-context)
  "Like `copy-file' for Tramp files (compat function)."
  (cond
   (preserve-selinux-context
    (tramp-compat-funcall
     'copy-file filename newname ok-if-already-exists keep-date
     preserve-uid-gid preserve-selinux-context))
   (preserve-uid-gid
    (tramp-compat-funcall
     'copy-file filename newname ok-if-already-exists keep-date
     preserve-uid-gid))
   (t
    (copy-file filename newname ok-if-already-exists keep-date))))

;; `copy-directory' is a new function in Emacs 23.2.  Implementation
;; is taken from there.
(defun tramp-compat-copy-directory
  (directory newname &optional keep-time parents)
  "Make a copy of DIRECTORY (compat function)."
  (if (fboundp 'copy-directory)
      (tramp-compat-funcall 'copy-directory directory newname keep-time parents)

    ;; If `default-directory' is a remote directory, make sure we find
    ;; its `copy-directory' handler.
    (let ((handler (or (find-file-name-handler directory 'copy-directory)
		       (find-file-name-handler newname 'copy-directory))))
      (if handler
	  (funcall handler 'copy-directory directory newname keep-time parents)

	;; Compute target name.
	(setq directory (directory-file-name (expand-file-name directory))
	      newname   (directory-file-name (expand-file-name newname)))
	(if (and (file-directory-p newname)
		 (not (string-equal (file-name-nondirectory directory)
				    (file-name-nondirectory newname))))
	    (setq newname
		  (expand-file-name
		   (file-name-nondirectory directory) newname)))
	(if (not (file-directory-p newname)) (make-directory newname parents))

	;; Copy recursively.
	(mapc
	 (lambda (file)
	   (if (file-directory-p file)
	       (tramp-compat-copy-directory file newname keep-time parents)
	     (copy-file file newname t keep-time)))
	 ;; We do not want to delete "." and "..".
	 (directory-files
	  directory 'full "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*"))

	;; Set directory attributes.
	(set-file-modes newname (file-modes directory))
	(if keep-time
	    (set-file-times newname (nth 5 (file-attributes directory))))))))

;; TRASH has been introduced with Emacs 24.1.
(defun tramp-compat-delete-file (filename &optional trash)
  "Like `delete-file' for Tramp files (compat function)."
  (condition-case nil
      (tramp-compat-funcall 'delete-file filename trash)
    ;; This Emacs version does not support the TRASH flag.
    (wrong-number-of-arguments
     (let ((delete-by-moving-to-trash
	    (and (boundp 'delete-by-moving-to-trash)
		 (symbol-value 'delete-by-moving-to-trash)
		 trash)))
       (delete-file filename)))))

;; RECURSIVE has been introduced with Emacs 23.2.
(defun tramp-compat-delete-directory (directory &optional recursive)
  "Like `delete-directory' for Tramp files (compat function)."
  (if (null recursive)
      (delete-directory directory)
    (condition-case nil
	(tramp-compat-funcall 'delete-directory directory recursive)
      ;; This Emacs version does not support the RECURSIVE flag.  We
      ;; use the implementation from Emacs 23.2.
      (wrong-number-of-arguments
       (setq directory (directory-file-name (expand-file-name directory)))
       (if (not (file-symlink-p directory))
	   (mapc (lambda (file)
		   (if (eq t (car (file-attributes file)))
		       (tramp-compat-delete-directory file recursive)
		     (delete-file file)))
		 (directory-files
		  directory 'full "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*")))
       (delete-directory directory)))))

;; `number-sequence' does not exist in XEmacs.  Implementation is
;; taken from Emacs 23.
(defun tramp-compat-number-sequence (from &optional to inc)
  "Return a sequence of numbers from FROM to TO as a list (compat function)."
  (if (or (subrp 'number-sequence) (symbol-file 'number-sequence))
      (tramp-compat-funcall 'number-sequence from to inc)
    (if (or (not to) (= from to))
	(list from)
      (or inc (setq inc 1))
      (when (zerop inc) (error "The increment can not be zero"))
      (let (seq (n 0) (next from))
	(if (> inc 0)
	    (while (<= next to)
	      (setq seq (cons next seq)
		    n (1+ n)
		    next (+ from (* n inc))))
	  (while (>= next to)
	    (setq seq (cons next seq)
		  n (1+ n)
		  next (+ from (* n inc)))))
	(nreverse seq)))))

(defun tramp-compat-split-string (string pattern)
  "Like `split-string' but omit empty strings.
In Emacs, (split-string \"/foo/bar\" \"/\") returns (\"foo\" \"bar\").
This is, the first, empty, element is omitted.  In XEmacs, the first
element is not omitted."
  (delete "" (split-string string pattern)))

(defun tramp-compat-call-process
  (program &optional infile destination display &rest args)
  "Calls `call-process' on the local host.
This is needed because for some Emacs flavors Tramp has
defadvised `call-process' to behave like `process-file'.  The
Lisp error raised when PROGRAM is nil is trapped also, returning 1."
  (let ((default-directory
	  (if (file-remote-p default-directory)
	      (tramp-compat-temporary-file-directory)
	    default-directory)))
    (if (executable-find program)
	(apply 'call-process program infile destination display args)
      1)))

(defun tramp-compat-process-running-p (process-name)
  "Returns `t' if system process PROCESS-NAME is running for `user-login-name'."
  (when (stringp process-name)
    (cond
     ;; GNU Emacs 22 on w32.
     ((fboundp 'w32-window-exists-p)
      (tramp-compat-funcall 'w32-window-exists-p process-name process-name))

     ;; GNU Emacs 23.
     ((and (fboundp 'list-system-processes) (fboundp 'process-attributes))
      (let (result)
	(dolist (pid (tramp-compat-funcall 'list-system-processes) result)
	  (let ((attributes (tramp-compat-funcall 'process-attributes pid)))
	    (when (and (string-equal
                        (cdr (assoc 'user attributes)) (user-login-name))
                       (let ((comm (cdr (assoc 'comm attributes))))
                         ;; The returned command name could be truncated
                         ;; to 15 characters.  Therefore, we cannot check
                         ;; for `string-equal'.
                         (and comm (string-match
                                    (concat "^" (regexp-quote comm))
                                    process-name))))
	      (setq result t))))))

     ;; Fallback, if there is no Lisp support yet.
     (t (let ((default-directory
		(if (file-remote-p default-directory)
		    (tramp-compat-temporary-file-directory)
		  default-directory))
	      (unix95 (getenv "UNIX95"))
	      result)
	  (setenv "UNIX95" "1")
	  (when (member
		 (user-login-name)
		 (tramp-compat-split-string
		  (shell-command-to-string
		   (format "ps -C %s -o user=" process-name))
		  "[ \f\t\n\r\v]+"))
	    (setq result t))
	  (setenv "UNIX95" unix95)
	  result)))))

;; The following functions do not exist in XEmacs.  We ignore this;
;; they are used for checking a remote tty.
(defun tramp-compat-process-get (process propname)
  "Return the value of PROCESS' PROPNAME property.
This is the last value stored with `(process-put PROCESS PROPNAME VALUE)'."
  (ignore-errors (tramp-compat-funcall 'process-get process propname)))

(defun tramp-compat-process-put (process propname value)
  "Change PROCESS' PROPNAME property to VALUE.
It can be retrieved with `(process-get PROCESS PROPNAME)'."
  (ignore-errors (tramp-compat-funcall 'process-put process propname value)))

(defun tramp-compat-set-process-query-on-exit-flag (process flag)
  "Specify if query is needed for process when Emacs is exited.
If the second argument flag is non-nil, Emacs will query the user before
exiting if process is running."
  (if (fboundp 'set-process-query-on-exit-flag)
      (tramp-compat-funcall 'set-process-query-on-exit-flag process flag)
    (tramp-compat-funcall 'process-kill-without-query process flag)))

;; There exist different implementations for this function.
(defun tramp-compat-coding-system-change-eol-conversion (coding-system eol-type)
  "Return a coding system like CODING-SYSTEM but with given EOL-TYPE.
EOL-TYPE can be one of `dos', `unix', or `mac'."
  (cond ((fboundp 'coding-system-change-eol-conversion)
         (tramp-compat-funcall
	  'coding-system-change-eol-conversion coding-system eol-type))
        ((fboundp 'subsidiary-coding-system)
         (tramp-compat-funcall
	  'subsidiary-coding-system coding-system
	  (cond ((eq eol-type 'dos) 'crlf)
		((eq eol-type 'unix) 'lf)
		((eq eol-type 'mac) 'cr)
		(t
		 (error "Unknown EOL-TYPE `%s', must be %s"
			eol-type
			"`dos', `unix', or `mac'")))))
        (t (error "Can't change EOL conversion -- is MULE missing?"))))

(add-hook 'tramp-unload-hook
	  (lambda ()
	    (unload-feature 'tramp-compat 'force)))

(provide 'tramp-compat)

;;; TODO:

;;; tramp-compat.el ends here
