;;; tramp-smb.el --- Tramp access functions for SMB servers

;; Copyright (C) 2002-2012 Free Software Foundation, Inc.

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

;; Access functions for SMB servers like SAMBA or M$ Windows from Tramp.

;;; Code:

(eval-when-compile (require 'cl))	; block, return
(require 'tramp)

;; Define SMB method ...
;;;###tramp-autoload
(defconst tramp-smb-method "smb"
  "*Method to connect SAMBA and M$ SMB servers.")

;; ... and add it to the method list.
;;;###tramp-autoload
(unless (memq system-type '(cygwin windows-nt))
  (add-to-list 'tramp-methods
    `(,tramp-smb-method
      ;; We define an empty command, because `tramp-smb-call-winexe'
      ;; opens already the powershell.  Used in `tramp-handle-shell-command'.
      (tramp-remote-shell "")
      ;; This is just a guess.  We don't know whether the share "$C"
      ;; is available for public use, and whether the user has write
      ;; access.
      (tramp-tmpdir "/C$/Temp"))))

;; Add a default for `tramp-default-method-alist'. Rule: If there is
;; a domain in USER, it must be the SMB method.
;;;###tramp-autoload
(add-to-list 'tramp-default-method-alist
	     `(nil ,tramp-prefix-domain-regexp ,tramp-smb-method))

;; Add a default for `tramp-default-user-alist'. Rule: For the SMB method,
;; the anonymous user is chosen.
;;;###tramp-autoload
(add-to-list 'tramp-default-user-alist
	     `(,(concat "\\`" tramp-smb-method "\\'") nil nil))

;; Add completion function for SMB method.
;;;###tramp-autoload
(eval-after-load 'tramp
  '(tramp-set-completion-function
    tramp-smb-method
    '((tramp-parse-netrc "~/.netrc"))))

(defcustom tramp-smb-program "smbclient"
  "*Name of SMB client to run."
  :group 'tramp
  :type 'string)

(defcustom tramp-smb-conf "/dev/null"
  "*Path of the smb.conf file.
If it is nil, no smb.conf will be added to the `tramp-smb-program'
call, letting the SMB client use the default one."
  :group 'tramp
  :type '(choice (const nil) (file :must-match t)))

(defvar tramp-smb-version nil
  "*Version string of the SMB client.")

(defconst tramp-smb-prompt "^smb: .+> \\|^\\s-+Server\\s-+Comment$"
  "Regexp used as prompt in smbclient.")

(defconst tramp-smb-errors
  (mapconcat
   'identity
   `(;; Connection error / timeout / unknown command.
     "Connection\\( to \\S-+\\)? failed"
     "Read from server failed, maybe it closed the connection"
     "Call timed out: server did not respond"
     "\\S-+: command not found"
     "Server doesn't support UNIX CIFS calls"
     ,(regexp-opt
       '(;; Samba.
	 "ERRDOS"
	 "ERRHRD"
	 "ERRSRV"
	 "ERRbadfile"
	 "ERRbadpw"
	 "ERRfilexists"
	 "ERRnoaccess"
	 "ERRnomem"
	 "ERRnosuchshare"
	 ;; Windows 4.0 (Windows NT), Windows 5.0 (Windows 2000),
	 ;; Windows 5.1 (Windows XP), Windows 5.2 (Windows Server 2003),
	 ;; Windows 6.0 (Windows Vista), Windows 6.1 (Windows 7).
	 "NT_STATUS_ACCESS_DENIED"
	 "NT_STATUS_ACCOUNT_LOCKED_OUT"
	 "NT_STATUS_BAD_NETWORK_NAME"
	 "NT_STATUS_CANNOT_DELETE"
	 "NT_STATUS_CONNECTION_REFUSED"
	 "NT_STATUS_DIRECTORY_NOT_EMPTY"
	 "NT_STATUS_DUPLICATE_NAME"
	 "NT_STATUS_FILE_IS_A_DIRECTORY"
	 "NT_STATUS_IMAGE_ALREADY_LOADED"
	 "NT_STATUS_IO_TIMEOUT"
	 "NT_STATUS_LOGON_FAILURE"
	 "NT_STATUS_NETWORK_ACCESS_DENIED"
	 "NT_STATUS_NOT_IMPLEMENTED"
	 "NT_STATUS_NO_SUCH_FILE"
	 "NT_STATUS_NO_SUCH_USER"
	 "NT_STATUS_OBJECT_NAME_COLLISION"
	 "NT_STATUS_OBJECT_NAME_INVALID"
	 "NT_STATUS_OBJECT_NAME_NOT_FOUND"
	 "NT_STATUS_SHARING_VIOLATION"
	 "NT_STATUS_TRUSTED_RELATIONSHIP_FAILURE"
	 "NT_STATUS_UNSUCCESSFUL"
	 "NT_STATUS_WRONG_PASSWORD")))
   "\\|")
  "Regexp for possible error strings of SMB servers.
Used instead of analyzing error codes of commands.")

(defconst tramp-smb-actions-with-share
  '((tramp-smb-prompt tramp-action-succeed)
    (tramp-password-prompt-regexp tramp-action-password)
    (tramp-wrong-passwd-regexp tramp-action-permission-denied)
    (tramp-smb-errors tramp-action-permission-denied)
    (tramp-process-alive-regexp tramp-action-process-alive))
  "List of pattern/action pairs.
This list is used for login to SMB servers.

See `tramp-actions-before-shell' for more info.")

(defconst tramp-smb-actions-without-share
  '((tramp-password-prompt-regexp tramp-action-password)
    (tramp-wrong-passwd-regexp tramp-action-permission-denied)
    (tramp-smb-errors tramp-action-permission-denied)
    (tramp-process-alive-regexp tramp-action-out-of-band))
  "List of pattern/action pairs.
This list is used for login to SMB servers.

See `tramp-actions-before-shell' for more info.")

;; New handlers should be added here.
(defconst tramp-smb-file-name-handler-alist
  '(
    ;; `access-file' performed by default handler.
    (add-name-to-file . tramp-smb-handle-add-name-to-file)
    ;; `byte-compiler-base-file-name' performed by default handler.
    (copy-directory . tramp-smb-handle-copy-directory)
    (copy-file . tramp-smb-handle-copy-file)
    (delete-directory . tramp-smb-handle-delete-directory)
    (delete-file . tramp-smb-handle-delete-file)
    ;; `diff-latest-backup-file' performed by default handler.
    (directory-file-name . tramp-handle-directory-file-name)
    (directory-files . tramp-smb-handle-directory-files)
    (directory-files-and-attributes
     . tramp-handle-directory-files-and-attributes)
    (dired-call-process . ignore)
    (dired-compress-file . ignore)
    (dired-uncache . tramp-handle-dired-uncache)
    (expand-file-name . tramp-smb-handle-expand-file-name)
    (file-accessible-directory-p . tramp-smb-handle-file-directory-p)
    (file-attributes . tramp-smb-handle-file-attributes)
    (file-directory-p .  tramp-smb-handle-file-directory-p)
    (file-executable-p . tramp-handle-file-exists-p)
    (file-exists-p . tramp-handle-file-exists-p)
    (file-local-copy . tramp-smb-handle-file-local-copy)
    (file-modes . tramp-handle-file-modes)
    (file-name-all-completions . tramp-smb-handle-file-name-all-completions)
    (file-name-as-directory . tramp-handle-file-name-as-directory)
    (file-name-completion . tramp-handle-file-name-completion)
    (file-name-directory . tramp-handle-file-name-directory)
    (file-name-nondirectory . tramp-handle-file-name-nondirectory)
    ;; `file-name-sans-versions' performed by default handler.
    (file-newer-than-file-p . tramp-handle-file-newer-than-file-p)
    (file-ownership-preserved-p . ignore)
    (file-readable-p . tramp-handle-file-exists-p)
    (file-regular-p . tramp-handle-file-regular-p)
    (file-remote-p . tramp-handle-file-remote-p)
    ;; `file-selinux-context' performed by default handler.
    (file-symlink-p . tramp-handle-file-symlink-p)
    ;; `file-truename' performed by default handler.
    (file-writable-p . tramp-smb-handle-file-writable-p)
    (find-backup-file-name . tramp-handle-find-backup-file-name)
    ;; `find-file-noselect' performed by default handler.
    ;; `get-file-buffer' performed by default handler.
    (insert-directory . tramp-smb-handle-insert-directory)
    (insert-file-contents . tramp-handle-insert-file-contents)
    (load . tramp-handle-load)
    (make-directory . tramp-smb-handle-make-directory)
    (make-directory-internal . tramp-smb-handle-make-directory-internal)
    (make-symbolic-link . tramp-smb-handle-make-symbolic-link)
    (rename-file . tramp-smb-handle-rename-file)
    (set-file-modes . tramp-smb-handle-set-file-modes)
    ;; `set-file-selinux-context' performed by default handler.
    (set-file-times . ignore)
    (set-visited-file-modtime . ignore)
    (shell-command . ignore)
    (substitute-in-file-name . tramp-smb-handle-substitute-in-file-name)
    (unhandled-file-name-directory . tramp-handle-unhandled-file-name-directory)
    (vc-registered . ignore)
    (verify-visited-file-modtime . ignore)
    (write-region . tramp-smb-handle-write-region)
)
  "Alist of handler functions for Tramp SMB method.
Operations not mentioned here will be handled by the default Emacs primitives.")

;;;###tramp-autoload
(defsubst tramp-smb-file-name-p (filename)
  "Check if it's a filename for SMB servers."
  (let ((v (tramp-dissect-file-name filename)))
    (string= (tramp-file-name-method v) tramp-smb-method)))

;;;###tramp-autoload
(defun tramp-smb-file-name-handler (operation &rest args)
  "Invoke the SMB related OPERATION.
First arg specifies the OPERATION, second arg is a list of arguments to
pass to the OPERATION."
  (let ((fn (assoc operation tramp-smb-file-name-handler-alist)))
    (if fn
	(save-match-data (apply (cdr fn) args))
      (tramp-run-real-handler operation args))))

;;;###tramp-autoload
(unless (memq system-type '(cygwin windows-nt))
  (add-to-list 'tramp-foreign-file-name-handler-alist
	       (cons 'tramp-smb-file-name-p 'tramp-smb-file-name-handler)))


;; File name primitives.

(defun tramp-smb-handle-add-name-to-file
  (filename newname &optional ok-if-already-exists)
  "Like `add-name-to-file' for Tramp files."
  (unless (tramp-equal-remote filename newname)
    (with-parsed-tramp-file-name
	(if (tramp-tramp-file-p filename) filename newname) nil
      (tramp-error
       v 'file-error
       "add-name-to-file: %s"
       "only implemented for same method, same user, same host")))
  (with-parsed-tramp-file-name filename v1
    (with-parsed-tramp-file-name newname v2
      (when (file-directory-p filename)
	(tramp-error
	 v2 'file-error
	 "add-name-to-file: %s must not be a directory" filename))
      (when (and (not ok-if-already-exists)
		 (file-exists-p newname)
		 (not (numberp ok-if-already-exists))
		 (y-or-n-p
		  (format
		   "File %s already exists; make it a new name anyway? "
		   newname)))
	(tramp-error
	 v2 'file-error
	 "add-name-to-file: file %s already exists" newname))
      ;; We must also flush the cache of the directory, because
      ;; `file-attributes' reads the values from there.
      (tramp-flush-file-property v2 (file-name-directory v2-localname))
      (tramp-flush-file-property v2 v2-localname)
      (unless
	  (tramp-smb-send-command
	   v1
	   (format
	    "%s \"%s\" \"%s\""
	    (if (tramp-smb-get-cifs-capabilities v1) "link" "hardlink")
	    (tramp-smb-get-localname v1)
	    (tramp-smb-get-localname v2)))
	(tramp-error
	 v2 'file-error
	 "error with add-name-to-file, see buffer `%s' for details"
	 (buffer-name))))))

(defun tramp-smb-handle-copy-directory
  (dirname newname &optional keep-date parents copy-contents)
  "Like `copy-directory' for Tramp files.  KEEP-DATE is not handled."
  (setq dirname (expand-file-name dirname)
	newname (expand-file-name newname))
  (let ((t1 (tramp-tramp-file-p dirname))
	(t2 (tramp-tramp-file-p newname)))
    (with-parsed-tramp-file-name (if t1 dirname newname) nil
      (cond
       ;; We must use a local temporary directory.
       ((and t1 t2)
	(let ((tmpdir
	       (make-temp-name
		(expand-file-name
		 tramp-temp-name-prefix
		 (tramp-compat-temporary-file-directory)))))
	  (unwind-protect
	      (progn
		(tramp-compat-copy-directory dirname tmpdir keep-date parents)
		(tramp-compat-copy-directory tmpdir newname keep-date parents))
	    (tramp-compat-delete-directory tmpdir 'recursive))))

       ;; We can copy recursively.
       ((or t1 t2)
	(let ((prompt (tramp-smb-send-command v "prompt"))
	      (recurse (tramp-smb-send-command v "recurse")))
	  (unless (file-directory-p newname)
	    (make-directory newname parents))
	  (unwind-protect
	      (unless
		  (and
		   prompt recurse
		   (tramp-smb-send-command
		    v (format "cd \"%s\"" (tramp-smb-get-localname v)))
		   (tramp-smb-send-command
		    v (format "lcd \"%s\"" (if t1 newname dirname)))
		   (if t1
		       (tramp-smb-send-command v "mget *")
		     (tramp-smb-send-command v "mput *")))
		;; Error.
		(with-current-buffer (tramp-get-connection-buffer v)
		  (goto-char (point-min))
		  (search-forward-regexp tramp-smb-errors nil t)
		  (tramp-error
		   v 'file-error
		   "%s `%s'" (match-string 0) (if t1 dirname newname))))
	    ;; Go home.
	    (tramp-smb-send-command
	     v (format
		"cd %s" (if (tramp-smb-get-cifs-capabilities v) "/" "\\")))
	    ;; Toggle prompt and recurse OFF.
	    (if prompt (tramp-smb-send-command v "prompt"))
	    (if recurse (tramp-smb-send-command v "recurse")))))

       ;; We must do it file-wise.
       (t
	(tramp-run-real-handler
	 'copy-directory (list dirname newname keep-date parents)))))))

(defun tramp-smb-handle-copy-file
  (filename newname &optional ok-if-already-exists keep-date
	    preserve-uid-gid preserve-selinux-context)
  "Like `copy-file' for Tramp files.
KEEP-DATE is not handled in case NEWNAME resides on an SMB server.
PRESERVE-UID-GID and PRESERVE-SELINUX-CONTEXT are completely ignored."
  (setq filename (expand-file-name filename)
	newname (expand-file-name newname))
  (tramp-with-progress-reporter
      (tramp-dissect-file-name (if (file-remote-p filename) filename newname))
      0 (format "Copying %s to %s" filename newname)

    (let ((tmpfile (file-local-copy filename)))

      (if tmpfile
	  ;; Remote filename.
	  (condition-case err
	      (rename-file tmpfile newname ok-if-already-exists)
	    ((error quit)
	     (delete-file tmpfile)
	     (signal (car err) (cdr err))))

	;; Remote newname.
	(when (file-directory-p newname)
	  (setq newname
		(expand-file-name (file-name-nondirectory filename) newname)))

	(with-parsed-tramp-file-name newname nil
	  (when (and (not ok-if-already-exists)
		     (file-exists-p newname))
	    (tramp-error v 'file-already-exists newname))

	  ;; We must also flush the cache of the directory, because
	  ;; `file-attributes' reads the values from there.
	  (tramp-flush-file-property v (file-name-directory localname))
	  (tramp-flush-file-property v localname)
	  (unless (tramp-smb-get-share v)
	    (tramp-error
	     v 'file-error "Target `%s' must contain a share name" newname))
	  (unless (tramp-smb-send-command
		   v (format "put \"%s\" \"%s\""
			     filename (tramp-smb-get-localname v)))
	    (tramp-error v 'file-error "Cannot copy `%s'" filename))))))

  ;; KEEP-DATE handling.
  (when keep-date (set-file-times newname (nth 5 (file-attributes filename)))))

(defun tramp-smb-handle-delete-directory (directory &optional recursive)
  "Like `delete-directory' for Tramp files."
  (setq directory (directory-file-name (expand-file-name directory)))
  (when (file-exists-p directory)
    (if recursive
	(mapc
	 (lambda (file)
	   (if (file-directory-p file)
	       (tramp-compat-delete-directory file recursive)
	     (delete-file file)))
	 ;; We do not want to delete "." and "..".
	 (directory-files
	  directory 'full "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*")))

    (with-parsed-tramp-file-name directory nil
      ;; We must also flush the cache of the directory, because
      ;; `file-attributes' reads the values from there.
      (tramp-flush-file-property v (file-name-directory localname))
      (tramp-flush-directory-property v localname)
      (unless (tramp-smb-send-command
	       v (format
		  "%s \"%s\""
		  (if (tramp-smb-get-cifs-capabilities v) "posix_rmdir" "rmdir")
		  (tramp-smb-get-localname v)))
	;; Error.
	(with-current-buffer (tramp-get-connection-buffer v)
	  (goto-char (point-min))
	  (search-forward-regexp tramp-smb-errors nil t)
	  (tramp-error
	   v 'file-error "%s `%s'" (match-string 0) directory))))))

(defun tramp-smb-handle-delete-file (filename &optional trash)
  "Like `delete-file' for Tramp files."
  (setq filename (expand-file-name filename))
  (when (file-exists-p filename)
    (with-parsed-tramp-file-name filename nil
      ;; We must also flush the cache of the directory, because
      ;; `file-attributes' reads the values from there.
      (tramp-flush-file-property v (file-name-directory localname))
      (tramp-flush-file-property v localname)
      (unless (tramp-smb-send-command
	       v (format
		  "%s \"%s\""
		  (if (tramp-smb-get-cifs-capabilities v) "posix_unlink" "rm")
		  (tramp-smb-get-localname v)))
	;; Error.
	(with-current-buffer (tramp-get-connection-buffer v)
	  (goto-char (point-min))
	  (search-forward-regexp tramp-smb-errors nil t)
	  (tramp-error
	   v 'file-error "%s `%s'" (match-string 0) filename))))))

(defun tramp-smb-handle-directory-files
  (directory &optional full match nosort)
  "Like `directory-files' for Tramp files."
  (let ((result (mapcar 'directory-file-name
			(file-name-all-completions "" directory))))
    ;; Discriminate with regexp.
    (when match
      (setq result
	    (delete nil
		    (mapcar (lambda (x) (when (string-match match x) x))
			    result))))
    ;; Append directory.
    (when full
      (setq result
	    (mapcar
	     (lambda (x) (expand-file-name x directory))
	     result)))
    ;; Sort them if necessary.
    (unless nosort (setq result (sort result 'string-lessp)))
    ;; That's it.
    result))

(defun tramp-smb-handle-expand-file-name (name &optional dir)
  "Like `expand-file-name' for Tramp files."
  ;; If DIR is not given, use DEFAULT-DIRECTORY or "/".
  (setq dir (or dir default-directory "/"))
  ;; Unless NAME is absolute, concat DIR and NAME.
  (unless (file-name-absolute-p name)
    (setq name (concat (file-name-as-directory dir) name)))
  ;; If NAME is not a Tramp file, run the real handler.
  (if (not (tramp-tramp-file-p name))
      (tramp-run-real-handler 'expand-file-name (list name nil))
    ;; Dissect NAME.
    (with-parsed-tramp-file-name name nil
      ;; Tilde expansion if necessary.  We use the user name as share,
      ;; which is often the case in domains.
      (when (string-match "\\`/?~\\([^/]*\\)" localname)
	(setq localname
	      (replace-match
	       (if (zerop (length (match-string 1 localname)))
		   (tramp-file-name-real-user v)
		 (match-string 1 localname))
	       nil nil localname)))
      ;; Make the file name absolute.
      (unless (tramp-run-real-handler 'file-name-absolute-p (list localname))
	(setq localname (concat "/" localname)))
      ;; No tilde characters in file name, do normal
      ;; `expand-file-name' (this does "/./" and "/../").
      (tramp-make-tramp-file-name
       method user host
       (tramp-run-real-handler 'expand-file-name (list localname))))))

(defun tramp-smb-handle-file-attributes (filename &optional id-format)
  "Like `file-attributes' for Tramp files."
  (unless id-format (setq id-format 'integer))
  (ignore-errors
    (with-parsed-tramp-file-name filename nil
      (with-file-property v localname (format "file-attributes-%s" id-format)
	(if (and (tramp-smb-get-share v) (tramp-smb-get-stat-capability v))
	    (tramp-smb-do-file-attributes-with-stat v id-format)
	  ;; Reading just the filename entry via "dir localname" is not
	  ;; possible, because when filename is a directory, some
	  ;; smbclient versions return the content of the directory, and
	  ;; other versions don't.  Therefore, the whole content of the
	  ;; upper directory is retrieved, and the entry of the filename
	  ;; is extracted from.
	  (let* ((entries (tramp-smb-get-file-entries
			   (file-name-directory filename)))
		 (entry (assoc (file-name-nondirectory filename) entries))
		 (uid (if (equal id-format 'string) "nobody" -1))
		 (gid (if (equal id-format 'string) "nogroup" -1))
		 (inode (tramp-get-inode v))
		 (device (tramp-get-device v)))

	    ;; Check result.
	    (when entry
	      (list (and (string-match "d" (nth 1 entry))
			 t)        ;0 file type
		    -1	           ;1 link count
		    uid	           ;2 uid
		    gid	           ;3 gid
		    '(0 0)	   ;4 atime
		    (nth 3 entry)  ;5 mtime
		    '(0 0)	   ;6 ctime
		    (nth 2 entry)  ;7 size
		    (nth 1 entry)  ;8 mode
		    nil	           ;9 gid weird
		    inode	   ;10 inode number
		    device)))))))) ;11 file system number

(defun tramp-smb-do-file-attributes-with-stat (vec &optional id-format)
  "Implement `file-attributes' for Tramp files using stat command."
  (tramp-message
   vec 5 "file attributes with stat: %s" (tramp-file-name-localname vec))
  (with-current-buffer (tramp-get-buffer vec)
    (let* (size id link uid gid atime mtime ctime mode inode)
      (when (tramp-smb-send-command
	     vec (format "stat \"%s\"" (tramp-smb-get-localname vec)))

	;; Loop the listing.
	(goto-char (point-min))
	(unless (re-search-forward tramp-smb-errors nil t)
	  (while (not (eobp))
	    (cond
	     ((looking-at
	       "Size:\\s-+\\([0-9]+\\)\\s-+Blocks:\\s-+[0-9]+\\s-+\\(\\w+\\)")
	      (setq size (string-to-number (match-string 1))
		    id (if (string-equal "directory" (match-string 2)) t
			 (if (string-equal "symbolic" (match-string 2)) ""))))
	     ((looking-at
	       "Inode:\\s-+\\([0-9]+\\)\\s-+Links:\\s-+\\([0-9]+\\)")
	      (setq inode (string-to-number (match-string 1))
		    link (string-to-number (match-string 2))))
	     ((looking-at
	       "Access:\\s-+([0-9]+/\\(\\S-+\\))\\s-+Uid:\\s-+\\([0-9]+\\)\\s-+Gid:\\s-+\\([0-9]+\\)")
	      (setq mode (match-string 1)
		    uid (if (equal id-format 'string) (match-string 2)
			  (string-to-number (match-string 2)))
		    gid (if (equal id-format 'string) (match-string 3)
			  (string-to-number (match-string 3)))))
	     ((looking-at
	       "Access:\\s-+\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)\\s-+\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)")
	      (setq atime
		    (encode-time
		     (string-to-number (match-string 6)) ;; sec
		     (string-to-number (match-string 5)) ;; min
		     (string-to-number (match-string 4)) ;; hour
		     (string-to-number (match-string 3)) ;; day
		     (string-to-number (match-string 2)) ;; month
		     (string-to-number (match-string 1))))) ;; year
	     ((looking-at
	       "Modify:\\s-+\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)\\s-+\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)")
	      (setq mtime
		    (encode-time
		     (string-to-number (match-string 6)) ;; sec
		     (string-to-number (match-string 5)) ;; min
		     (string-to-number (match-string 4)) ;; hour
		     (string-to-number (match-string 3)) ;; day
		     (string-to-number (match-string 2)) ;; month
		     (string-to-number (match-string 1))))) ;; year
	     ((looking-at
	       "Change:\\s-+\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)\\s-+\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)")
	      (setq ctime
		    (encode-time
		     (string-to-number (match-string 6)) ;; sec
		     (string-to-number (match-string 5)) ;; min
		     (string-to-number (match-string 4)) ;; hour
		     (string-to-number (match-string 3)) ;; day
		     (string-to-number (match-string 2)) ;; month
		     (string-to-number (match-string 1)))))) ;; year
	    (forward-line))
	  ;; Return the result.
	  (list id link uid gid atime mtime ctime size mode nil inode
		(tramp-get-device vec)))))))

(defun tramp-smb-handle-file-directory-p (filename)
  "Like `file-directory-p' for Tramp files."
  (and (file-exists-p filename)
       (eq ?d (aref (nth 8 (file-attributes filename)) 0))))

(defun tramp-smb-handle-file-local-copy (filename)
  "Like `file-local-copy' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (unless (file-exists-p filename)
      (tramp-error
       v 'file-error
       "Cannot make local copy of non-existing file `%s'" filename))
    (let ((tmpfile (tramp-compat-make-temp-file filename)))
      (tramp-with-progress-reporter
	  v 3 (format "Fetching %s to tmp file %s" filename tmpfile)
	(unless (tramp-smb-send-command
		 v (format "get \"%s\" \"%s\""
			   (tramp-smb-get-localname v) tmpfile))
	  ;; Oops, an error.  We shall cleanup.
	  (delete-file tmpfile)
	  (tramp-error
	   v 'file-error "Cannot make local copy of file `%s'" filename)))
      tmpfile)))

;; This function should return "foo/" for directories and "bar" for
;; files.
(defun tramp-smb-handle-file-name-all-completions (filename directory)
  "Like `file-name-all-completions' for Tramp files."
  (all-completions
   filename
   (with-parsed-tramp-file-name directory nil
     (with-file-property v localname "file-name-all-completions"
       (save-match-data
	 (let ((entries (tramp-smb-get-file-entries directory)))
	   (mapcar
	    (lambda (x)
	      (list
	       (if (string-match "d" (nth 1 x))
		   (file-name-as-directory (nth 0 x))
		 (nth 0 x))))
	    entries)))))))

(defun tramp-smb-handle-file-writable-p (filename)
  "Like `file-writable-p' for Tramp files."
  (if (file-exists-p filename)
      (string-match "w" (or (nth 8 (file-attributes filename)) ""))
    (let ((dir (file-name-directory filename)))
      (and (file-exists-p dir)
	   (file-writable-p dir)))))

(defun tramp-smb-handle-insert-directory
  (filename switches &optional wildcard full-directory-p)
  "Like `insert-directory' for Tramp files."
  (setq filename (expand-file-name filename))
  (if full-directory-p
      ;; Called from `dired-add-entry'.
      (setq filename (file-name-as-directory filename))
    (setq filename (directory-file-name filename)))
  (with-parsed-tramp-file-name filename nil
    (save-match-data
      (let ((base (file-name-nondirectory filename))
	    ;; We should not destroy the cache entry.
	    (entries (copy-sequence
		      (tramp-smb-get-file-entries
		       (file-name-directory filename)))))

	(when wildcard
	  (string-match "\\." base)
	  (setq base (replace-match "\\\\." nil nil base))
	  (string-match "\\*" base)
	  (setq base (replace-match ".*" nil nil base))
	  (string-match "\\?" base)
	  (setq base (replace-match ".?" nil nil base)))

	;; Filter entries.
	(setq entries
	      (delq
	       nil
	       (if (or wildcard (zerop (length base)))
		   ;; Check for matching entries.
		   (mapcar
		    (lambda (x)
		      (when (string-match
			     (format "^%s" base) (nth 0 x))
			x))
		    entries)
		 ;; We just need the only and only entry FILENAME.
		 (list (assoc base entries)))))

	;; Sort entries.
	(setq entries
	      (sort
	       entries
	       (lambda (x y)
		 (if (string-match "t" switches)
		     ;; Sort by date.
		     (tramp-time-less-p (nth 3 y) (nth 3 x))
		   ;; Sort by name.
		   (string-lessp (nth 0 x) (nth 0 y))))))

	;; Handle "-F" switch.
	(when (string-match "F" switches)
	  (mapc
	   (lambda (x)
	     (when (not (zerop (length (car x))))
	       (cond
		((char-equal ?d (string-to-char (nth 1 x)))
		 (setcar x (concat (car x) "/")))
		((char-equal ?x (string-to-char (nth 1 x)))
		 (setcar x (concat (car x) "*"))))))
	   entries))

	;; Print entries.
	(mapc
	 (lambda (x)
	   (when (not (zerop (length (nth 0 x))))
	     (let ((attr
		    (when (tramp-smb-get-stat-capability v)
		      (ignore-errors
			(file-attributes filename 'string)))))
	       (insert
		(format
		 "%10s %3d %-8s %-8s %8s %s "
		 (or (nth 8 attr) (nth 1 x)) ; mode
		 (or (nth 1 attr) 1) ; inode
		 (or (nth 2 attr) "nobody") ; uid
		 (or (nth 3 attr) "nogroup") ; gid
		 (or (nth 7 attr) (nth 2 x)) ; size
		 (format-time-string
		  (if (tramp-time-less-p
		       (tramp-time-subtract (current-time) (nth 3 x))
		       tramp-half-a-year)
		      "%b %e %R"
		    "%b %e  %Y")
		  (nth 3 x)))) ; date
	       ;; We mark the file name.  The inserted name could be
	       ;; from somewhere else, so we use the relative file
	       ;; name of `default-directory'.
	       (let ((start (point)))
		 (insert
		  (format
		   "%s\n"
		   (file-relative-name
		    (expand-file-name
		     (nth 0 x) (file-name-directory filename)))))
		 (put-text-property start (1- (point)) 'dired-filename t))
	       (forward-line)
	       (beginning-of-line))))
	 entries)))))

(defun tramp-smb-handle-make-directory (dir &optional parents)
  "Like `make-directory' for Tramp files."
  (setq dir (directory-file-name (expand-file-name dir)))
  (unless (file-name-absolute-p dir)
    (setq dir (expand-file-name dir default-directory)))
  (with-parsed-tramp-file-name dir nil
    (save-match-data
      (let* ((ldir (file-name-directory dir)))
	;; Make missing directory parts.
	(when (and parents
		   (tramp-smb-get-share v)
		   (not (file-directory-p ldir)))
	  (make-directory ldir parents))
	;; Just do it.
	(when (file-directory-p ldir)
	  (make-directory-internal dir))
	(unless (file-directory-p dir)
	  (tramp-error v 'file-error "Couldn't make directory %s" dir))))))

(defun tramp-smb-handle-make-directory-internal (directory)
  "Like `make-directory-internal' for Tramp files."
  (setq directory (directory-file-name (expand-file-name directory)))
  (unless (file-name-absolute-p directory)
    (setq directory (expand-file-name directory default-directory)))
  (with-parsed-tramp-file-name directory nil
    (save-match-data
      (let* ((file (tramp-smb-get-localname v)))
	(when (file-directory-p (file-name-directory directory))
	  (tramp-smb-send-command
	   v
	   (if (tramp-smb-get-cifs-capabilities v)
	       (format
		"posix_mkdir \"%s\" %s"
		file (tramp-compat-decimal-to-octal (default-file-modes)))
	     (format "mkdir \"%s\"" file)))
	  ;; We must also flush the cache of the directory, because
	  ;; `file-attributes' reads the values from there.
	  (tramp-flush-file-property v (file-name-directory localname))
	  (tramp-flush-file-property v localname))
	(unless (file-directory-p directory)
	  (tramp-error
	   v 'file-error "Couldn't make directory %s" directory))))))

(defun tramp-smb-handle-make-symbolic-link
  (filename linkname &optional ok-if-already-exists)
  "Like `make-symbolic-link' for Tramp files.
If LINKNAME is a non-Tramp file, it is used verbatim as the target of
the symlink.  If LINKNAME is a Tramp file, only the localname component is
used as the target of the symlink.

If LINKNAME is a Tramp file and the localname component is relative, then
it is expanded first, before the localname component is taken.  Note that
this can give surprising results if the user/host for the source and
target of the symlink differ."
  (unless (tramp-equal-remote filename linkname)
    (with-parsed-tramp-file-name
	(if (tramp-tramp-file-p filename) filename linkname) nil
      (tramp-error
       v 'file-error
       "make-symbolic-link: %s"
       "only implemented for same method, same user, same host")))
  (with-parsed-tramp-file-name filename v1
    (with-parsed-tramp-file-name linkname v2
      (when (file-directory-p filename)
	(tramp-error
	 v2 'file-error
	 "make-symbolic-link: %s must not be a directory" filename))
      (when (and (not ok-if-already-exists)
		 (file-exists-p linkname)
		 (not (numberp ok-if-already-exists))
		 (y-or-n-p
		  (format
		   "File %s already exists; make it a new name anyway? "
		   linkname)))
	(tramp-error
	 v2 'file-error
	 "make-symbolic-link: file %s already exists" linkname))
      (unless (tramp-smb-get-cifs-capabilities v1)
	(tramp-error v2 'file-error "make-symbolic-link not supported"))
      ;; We must also flush the cache of the directory, because
      ;; `file-attributes' reads the values from there.
      (tramp-flush-file-property v2 (file-name-directory v2-localname))
      (tramp-flush-file-property v2 v2-localname)
      (unless
	  (tramp-smb-send-command
	   v1
	   (format
	    "symlink \"%s\" \"%s\""
	    (tramp-smb-get-localname v1)
	    (tramp-smb-get-localname v2)))
	(tramp-error
	 v2 'file-error
	 "error with make-symbolic-link, see buffer `%s' for details"
	 (buffer-name))))))

(defun tramp-smb-handle-rename-file
  (filename newname &optional ok-if-already-exists)
  "Like `rename-file' for Tramp files."
  (setq filename (expand-file-name filename)
	newname (expand-file-name newname))
  (tramp-with-progress-reporter
      (tramp-dissect-file-name (if (file-remote-p filename) filename newname))
      0 (format "Renaming %s to %s" filename newname)

    (let ((tmpfile (file-local-copy filename)))

      (if tmpfile
	  ;; Remote filename.
	  (condition-case err
	      (rename-file tmpfile newname ok-if-already-exists)
	    ((error quit)
	     (delete-file tmpfile)
	     (signal (car err) (cdr err))))

	;; Remote newname.
	(when (file-directory-p newname)
	  (setq newname (expand-file-name
			 (file-name-nondirectory filename) newname)))

	(with-parsed-tramp-file-name newname nil
	  (when (and (not ok-if-already-exists)
		     (file-exists-p newname))
	    (tramp-error v 'file-already-exists newname))
	  ;; We must also flush the cache of the directory, because
	  ;; `file-attributes' reads the values from there.
	  (tramp-flush-file-property v (file-name-directory localname))
	  (tramp-flush-file-property v localname)
	  (unless (tramp-smb-send-command
		   v (format "put %s \"%s\""
			     filename (tramp-smb-get-localname v)))
	    (tramp-error v 'file-error "Cannot rename `%s'" filename)))))

    (delete-file filename)))

(defun tramp-smb-handle-set-file-modes (filename mode)
  "Like `set-file-modes' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (when (tramp-smb-get-cifs-capabilities v)
      (tramp-flush-file-property v localname)
      (unless (tramp-smb-send-command
	       v (format "chmod \"%s\" %s"
			 (tramp-smb-get-localname v)
			 (tramp-compat-decimal-to-octal mode)))
	(tramp-error
	 v 'file-error "Error while changing file's mode %s" filename)))))

(defun tramp-smb-handle-substitute-in-file-name (filename)
  "Like `handle-substitute-in-file-name' for Tramp files.
\"//\" substitutes only in the local filename part.  Catches
errors for shares like \"C$/\", which are common in Microsoft Windows."
  (with-parsed-tramp-file-name filename nil
    ;; Ignore in LOCALNAME everything before "//".
    (when (and (stringp localname) (string-match ".+?/\\(/\\|~\\)" localname))
      (setq filename
	    (concat (file-remote-p filename)
		    (replace-match "\\1" nil nil localname)))))
  (condition-case nil
      (tramp-run-real-handler 'substitute-in-file-name (list filename))
    (error filename)))

(defun tramp-smb-handle-write-region
  (start end filename &optional append visit lockname confirm)
  "Like `write-region' for Tramp files."
  (setq filename (expand-file-name filename))
  (with-parsed-tramp-file-name filename nil
    (unless (eq append nil)
      (tramp-error
	 v 'file-error "Cannot append to file using Tramp (`%s')" filename))
    ;; XEmacs takes a coding system as the seventh argument, not `confirm'.
    (when (and (not (featurep 'xemacs))
	       confirm (file-exists-p filename))
      (unless (y-or-n-p (format "File %s exists; overwrite anyway? "
				filename))
	(tramp-error v 'file-error "File not overwritten")))
    ;; We must also flush the cache of the directory, because
    ;; `file-attributes' reads the values from there.
    (tramp-flush-file-property v (file-name-directory localname))
    (tramp-flush-file-property v localname)
    (let ((curbuf (current-buffer))
	  (tmpfile (tramp-compat-make-temp-file filename)))
      ;; We say `no-message' here because we don't want the visited file
      ;; modtime data to be clobbered from the temp file.  We call
      ;; `set-visited-file-modtime' ourselves later on.
      (tramp-run-real-handler
       'write-region
       (if confirm ; don't pass this arg unless defined for backward compat.
	   (list start end tmpfile append 'no-message lockname confirm)
	 (list start end tmpfile append 'no-message lockname)))

      (tramp-with-progress-reporter
	  v 3 (format "Moving tmp file %s to %s" tmpfile filename)
	(unwind-protect
	    (unless (tramp-smb-send-command
		     v (format "put %s \"%s\""
			       tmpfile (tramp-smb-get-localname v)))
	      (tramp-error v 'file-error "Cannot write `%s'" filename))
	  (delete-file tmpfile)))

      (unless (equal curbuf (current-buffer))
	(tramp-error
	 v 'file-error
	 "Buffer has changed from `%s' to `%s'" curbuf (current-buffer)))
      (when (eq visit t)
	(set-visited-file-modtime)))))


;; Internal file name functions.

(defun tramp-smb-get-share (vec)
  "Returns the share name of LOCALNAME."
  (save-match-data
    (let ((localname (tramp-file-name-localname vec)))
      (when (string-match "^/?\\([^/]+\\)/" localname)
	(match-string 1 localname)))))

(defun tramp-smb-get-localname (vec)
  "Returns the file name of LOCALNAME.
If VEC has no cifs capabilities, exchange \"/\" by \"\\\\\"."
  (save-match-data
    (let ((localname (tramp-file-name-localname vec)))
      (setq
       localname
       (if (string-match "^/?[^/]+\\(/.*\\)" localname)
	   ;; There is a share, separated by "/".
	   (if (not (tramp-smb-get-cifs-capabilities vec))
	       (mapconcat
		(lambda (x) (if (equal x ?/) "\\" (char-to-string x)))
		(match-string 1 localname) "")
	     (match-string 1 localname))
	 ;; There is just a share.
	 (if (string-match "^/?\\([^/]+\\)$" localname)
	     (match-string 1 localname)
	   "")))

      ;; Sometimes we have discarded `substitute-in-file-name'.
      (when (string-match "\\(\\$\\$\\)\\(/\\|$\\)" localname)
	(setq localname (replace-match "$" nil nil localname 1)))

      localname)))

;; Share names of a host are cached. It is very unlikely that the
;; shares do change during connection.
(defun tramp-smb-get-file-entries (directory)
  "Read entries which match DIRECTORY.
Either the shares are listed, or the `dir' command is executed.
Result is a list of (LOCALNAME MODE SIZE MONTH DAY TIME YEAR)."
  (with-parsed-tramp-file-name (file-name-as-directory directory) nil
    (setq localname (or localname "/"))
    (with-file-property v localname "file-entries"
      (with-current-buffer (tramp-get-buffer v)
	(let* ((share (tramp-smb-get-share v))
	       (cache (tramp-get-connection-property v "share-cache" nil))
	       res entry)

	  (if (and (not share) cache)
	      ;; Return cached shares.
	      (setq res cache)

	    ;; Read entries.
	    (if share
		(tramp-smb-send-command
		 v (format "dir \"%s*\"" (tramp-smb-get-localname v)))
	      ;; `tramp-smb-maybe-open-connection' lists also the share names.
	      (tramp-smb-maybe-open-connection v))

	    ;; Loop the listing.
	    (goto-char (point-min))
	    (if (re-search-forward tramp-smb-errors nil t)
		(tramp-error v 'file-error "%s `%s'" (match-string 0) directory)
	      (while (not (eobp))
		(setq entry (tramp-smb-read-file-entry share))
		(forward-line)
		(when entry (add-to-list 'res entry))))

	    ;; Cache share entries.
	    (unless share
	      (tramp-set-connection-property v "share-cache" res)))

	  ;; Add directory itself.
	  (add-to-list 'res '("" "drwxrwxrwx" 0 (0 0)))

	  ;; There's a very strange error (debugged with XEmacs 21.4.14)
	  ;; If there's no short delay, it returns nil.  No idea about.
	  (when (featurep 'xemacs) (sleep-for 0.01))

	  ;; Return entries.
	  (delq nil res))))))

;; Return either a share name (if SHARE is nil), or a file name.
;;
;; If shares are listed, the following format is expected:
;;
;; Disk|                                  - leading spaces
;; [^|]+|                                 - share name, 14 char
;; .*                                     - comment
;;
;; Entries provided by smbclient DIR aren't fully regular.
;; They should have the format
;;
;; \s-\{2,2}                              - leading spaces
;; \S-\(.*\S-\)\s-*                       - file name, 30 chars, left bound
;; \s-+[ADHRSV]*                          - permissions, 7 chars, right bound
;; \s-                                    - space delimiter
;; \s-+[0-9]+                             - size, 8 chars, right bound
;; \s-\{2,2\}                             - space delimiter
;; \w\{3,3\}                              - weekday
;; \s-                                    - space delimiter
;; \w\{3,3\}                              - month
;; \s-                                    - space delimiter
;; [ 12][0-9]                             - day
;; \s-                                    - space delimiter
;; [0-9]\{2,2\}:[0-9]\{2,2\}:[0-9]\{2,2\} - time
;; \s-                                    - space delimiter
;; [0-9]\{4,4\}                           - year
;;
;; samba/src/client.c (http://samba.org/doxygen/samba/client_8c-source.html)
;; has function display_finfo:
;;
;;   d_printf("  %-30s%7.7s %8.0f  %s",
;;            finfo->name,
;;            attrib_string(finfo->mode),
;;            (double)finfo->size,
;;            asctime(LocalTime(&t)));
;;
;; in Samba 1.9, there's the following code:
;;
;;   DEBUG(0,("  %-30s%7.7s%10d  %s",
;;  	   CNV_LANG(finfo->name),
;;	   attrib_string(finfo->mode),
;;	   finfo->size,
;;	   asctime(LocalTime(&t))));
;;
;; Problems:
;; * Modern regexp constructs, like spy groups and counted repetitions, aren't
;;   available in older Emacsen.
;; * The length of constructs (file name, size) might exceed the default.
;; * File names might contain spaces.
;; * Permissions might be empty.
;;
;; So we try to analyze backwards.
(defun tramp-smb-read-file-entry (share)
  "Parse entry in SMB output buffer.
If SHARE is result, entries are of type dir. Otherwise, shares are listed.
Result is the list (LOCALNAME MODE SIZE MTIME)."
;; We are called from `tramp-smb-get-file-entries', which sets the
;; current buffer.
  (let ((line (buffer-substring (point) (point-at-eol)))
	localname mode size month day hour min sec year mtime)

    (if (not share)

	;; Read share entries.
	(when (string-match "^Disk|\\([^|]+\\)|" line)
	  (setq localname (match-string 1 line)
		mode "dr-xr-xr-x"
		size 0))

      ;; Real listing.
      (block nil

	;; year.
	(if (string-match "\\([0-9]+\\)$" line)
	    (setq year (string-to-number (match-string 1 line))
		  line (substring line 0 -5))
	  (return))

	;; time.
	(if (string-match "\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)$" line)
	    (setq hour (string-to-number (match-string 1 line))
		  min  (string-to-number (match-string 2 line))
		  sec  (string-to-number (match-string 3 line))
		  line (substring line 0 -9))
	  (return))

	;; day.
	(if (string-match "\\([0-9]+\\)$" line)
	    (setq day  (string-to-number (match-string 1 line))
		  line (substring line 0 -3))
	  (return))

	;; month.
	(if (string-match "\\(\\w+\\)$" line)
	    (setq month (match-string 1 line)
		  line  (substring line 0 -4))
	  (return))

	;; weekday.
	(if (string-match "\\(\\w+\\)$" line)
	    (setq line (substring line 0 -5))
	  (return))

	;; size.
	(if (string-match "\\([0-9]+\\)$" line)
	    (let ((length (- (max 10 (1+ (length (match-string 1 line)))))))
	      (setq size (string-to-number (match-string 1 line)))
	      (when (string-match "\\([ADHRSV]+\\)" (substring line length))
		(setq length (+ length (match-end 0))))
	      (setq line (substring line 0 length)))
	  (return))

	;; mode: ARCH, DIR, HIDDEN, RONLY, SYSTEM, VOLID.
	(if (string-match "\\([ADHRSV]+\\)?$" line)
	    (setq
	     mode (or (match-string 1 line) "")
	     mode (save-match-data (format
		    "%s%s"
		    (if (string-match "D" mode) "d" "-")
		    (mapconcat
		     (lambda (x) "") "    "
		     (concat "r" (if (string-match "R" mode) "-" "w") "x"))))
	     line (substring line 0 -6))
	  (return))

	;; localname.
	(if (string-match "^\\s-+\\(\\S-\\(.*\\S-\\)?\\)\\s-*$" line)
	    (setq localname (match-string 1 line))
	  (return))))

    (when (and localname mode size)
      (setq mtime
	    (if (and sec min hour day month year)
		(encode-time
		 sec min hour day
		 (cdr (assoc (downcase month) tramp-parse-time-months))
		 year)
	      '(0 0)))
      (list localname mode size mtime))))

(defun tramp-smb-get-cifs-capabilities (vec)
  "Check, whether the SMB server supports POSIX commands."
  ;; When we are not logged in yet, we return nil.
  (if (let ((p (tramp-get-connection-process vec)))
	(and p (processp p) (memq (process-status p) '(run open))))
      (with-connection-property
	  (tramp-get-connection-process vec) "cifs-capabilities"
	(save-match-data
	  (when (tramp-smb-send-command vec "posix")
	    (with-current-buffer (tramp-get-buffer vec)
	      (goto-char (point-min))
	      (when
		  (re-search-forward "Server supports CIFS capabilities" nil t)
		(member
		 "pathnames"
		 (split-string
		  (buffer-substring (point) (point-at-eol)) nil t)))))))))

(defun tramp-smb-get-stat-capability (vec)
  "Check, whether the SMB server supports the STAT command."
  ;; When we are not logged in yet, we return nil.
  (if (let ((p (tramp-get-connection-process vec)))
	(and p (processp p) (memq (process-status p) '(run open))))
      (with-connection-property
	  (tramp-get-connection-process vec) "stat-capability"
	(tramp-smb-send-command vec "stat ."))))


;; Connection functions.

(defun tramp-smb-send-command (vec command)
  "Send the COMMAND to connection VEC.
Returns nil if there has been an error message from smbclient."
  (tramp-smb-maybe-open-connection vec)
  (tramp-message vec 6 "%s" command)
  (tramp-send-string vec command)
  (tramp-smb-wait-for-output vec))

(defun tramp-smb-maybe-open-connection (vec)
  "Maybe open a connection to HOST, log in as USER, using `tramp-smb-program'.
Does not do anything if a connection is already open, but re-opens the
connection if a previous connection has died for some reason."
  (let* ((share (tramp-smb-get-share vec))
	 (buf (tramp-get-buffer vec))
	 (p (get-buffer-process buf)))

    ;; Check whether we still have the same smbclient version.
    ;; Otherwise, we must delete the connection cache, because
    ;; capabilities migh have changed.
    (unless (processp p)
      (let ((default-directory (tramp-compat-temporary-file-directory))
	    (command (concat tramp-smb-program " -V")))

	(unless tramp-smb-version
	  (unless (executable-find tramp-smb-program)
	    (tramp-error
	     vec 'file-error
	     "Cannot find command %s in %s" tramp-smb-program exec-path))
	  (setq tramp-smb-version (shell-command-to-string command))
	  (tramp-message vec 6 command)
	  (tramp-message vec 6 "\n%s" tramp-smb-version)
	  (if (string-match "[ \t\n\r]+\\'" tramp-smb-version)
	      (setq tramp-smb-version
		    (replace-match "" nil nil tramp-smb-version))))

	(unless (string-equal
		 tramp-smb-version
		 (tramp-get-connection-property
		  vec "smbclient-version" tramp-smb-version))
	  (tramp-flush-directory-property vec "")
	  (tramp-flush-connection-property vec))

	(tramp-set-connection-property
	 vec "smbclient-version" tramp-smb-version)))

    ;; If too much time has passed since last command was sent, look
    ;; whether there has been an error message; maybe due to
    ;; connection timeout.
    (with-current-buffer buf
      (goto-char (point-min))
      (when (and (> (tramp-time-diff
		     (current-time)
		     (tramp-get-connection-property
		      p "last-cmd-time" '(0 0 0)))
		    60)
		 p (processp p) (memq (process-status p) '(run open))
		 (re-search-forward tramp-smb-errors nil t))
	(delete-process p)
	(setq p nil)))

    ;; Check whether it is still the same share.
    (unless
	(and p (processp p) (memq (process-status p) '(run open))
	     (string-equal
	      share
	      (tramp-get-connection-property p "smb-share" "")))

      (save-match-data
	;; There might be unread output from checking for share names.
	(when buf (with-current-buffer buf (erase-buffer)))
	(when (and p (processp p)) (delete-process p))

	(let* ((user      (tramp-file-name-user vec))
	       (host      (tramp-file-name-host vec))
	       (real-user (tramp-file-name-real-user vec))
	       (real-host (tramp-file-name-real-host vec))
	       (domain    (tramp-file-name-domain vec))
	       (port      (tramp-file-name-port vec))
	       args)

	  (if share
	      (setq args (list (concat "//" real-host "/" share)))
	    (setq args (list "-g" "-L" real-host )))

	  (if (not (zerop (length real-user)))
	      (setq args (append args (list "-U" real-user)))
	    (setq args (append args (list "-N"))))

	  (when domain (setq args (append args (list "-W" domain))))
	  (when port   (setq args (append args (list "-p" port))))
	  (when tramp-smb-conf
	    (setq args (append args (list "-s" tramp-smb-conf))))

	  ;; OK, let's go.
	  (tramp-with-progress-reporter
	      vec 3
	      (format "Opening connection for //%s%s/%s"
		      (if (not (zerop (length user))) (concat user "@") "")
		      host (or share ""))

	    (let* ((coding-system-for-read nil)
		   (process-connection-type tramp-process-connection-type)
		   (p (let ((default-directory
			      (tramp-compat-temporary-file-directory)))
			(apply #'start-process
			       (tramp-buffer-name vec) (tramp-get-buffer vec)
			       tramp-smb-program args))))

	      (tramp-message
	       vec 6 "%s" (mapconcat 'identity (process-command p) " "))
	      (tramp-compat-set-process-query-on-exit-flag p nil)

	      ;; Set variables for computing the prompt for reading password.
	      (setq tramp-current-method tramp-smb-method
		    tramp-current-user user
		    tramp-current-host host)

	      ;; Play login scenario.
	      (tramp-process-actions
	       p vec nil
	       (if share
		   tramp-smb-actions-with-share
		 tramp-smb-actions-without-share))

	      ;; Check server version.
	      (with-current-buffer (tramp-get-connection-buffer vec)
		(goto-char (point-min))
		(search-forward-regexp
		 "Domain=\\[[^]]*\\] OS=\\[[^]]*\\] Server=\\[[^]]*\\]" nil t)
		(let ((smbserver-version (match-string 0)))
		  (unless
		      (string-equal
		       smbserver-version
		       (tramp-get-connection-property
			vec "smbserver-version" smbserver-version))
		    (tramp-flush-directory-property vec "")
		    (tramp-flush-connection-property vec))
		  (tramp-set-connection-property
		   vec "smbserver-version" smbserver-version)))

	      ;; Set chunksize.  Otherwise, `tramp-send-string' might
	      ;; try it itself.
	      (tramp-set-connection-property p "smb-share" share)
	      (tramp-set-connection-property
	       p "chunksize" tramp-chunksize))))))))

;; We don't use timeouts.  If needed, the caller shall wrap around.
(defun tramp-smb-wait-for-output (vec)
  "Wait for output from smbclient command.
Returns nil if an error message has appeared."
  (with-current-buffer (tramp-get-buffer vec)
    (let ((p (get-buffer-process (current-buffer)))
	  (found (progn (goto-char (point-min))
			(re-search-forward tramp-smb-prompt nil t)))
	  (err   (progn (goto-char (point-min))
			(re-search-forward tramp-smb-errors nil t)))
	  buffer-read-only)

      ;; Algorithm: get waiting output.  See if last line contains
      ;; `tramp-smb-prompt' sentinel or `tramp-smb-errors' strings.
      ;; If not, wait a bit and again get waiting output.
      (while (and (not found) (not err) (memq (process-status p) '(run open)))

	;; Accept pending output.
	(tramp-accept-process-output p)

	;; Search for prompt.
	(goto-char (point-min))
	(setq found (re-search-forward tramp-smb-prompt nil t))

	;; Search for errors.
	(goto-char (point-min))
	(setq err (re-search-forward tramp-smb-errors nil t)))

      ;; When the process is still alive, read pending output.
      (while (and (not found) (memq (process-status p) '(run open)))

	;; Accept pending output.
	(tramp-accept-process-output p)

	;; Search for prompt.
	(goto-char (point-min))
	(setq found (re-search-forward tramp-smb-prompt nil t)))

      ;; Return value is whether no error message has appeared.
      (tramp-message vec 6 "\n%s" (buffer-string))
      (not err))))

(add-hook 'tramp-unload-hook
	  (lambda ()
	    (unload-feature 'tramp-smb 'force)))

(provide 'tramp-smb)

;;; TODO:

;; * Error handling in case password is wrong.
;; * Return more comprehensive file permission string.
;; * Try to remove the inclusion of dummy "" directory.  Seems to be at
;;   several places, especially in `tramp-smb-handle-insert-directory'.
;; * (RMS) Use unwind-protect to clean up the state so as to make the state
;;   regular again.
;; * Ignore case in file names.

;;; tramp-smb.el ends here
