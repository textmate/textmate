;;; vc-sccs.el --- support for SCCS version-control

;; Copyright (C) 1992-2012  Free Software Foundation, Inc.

;; Author:     FSF (see vc.el for full credits)
;; Maintainer: Andre Spiegel <spiegel@gnu.org>
;; Package: vc

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

;; Proper function of the SCCS diff commands requires the shellscript vcdiff
;; to be installed somewhere on Emacs's path for executables.
;;

;;; Code:

(eval-when-compile
  (require 'vc))

;;;
;;; Customization options
;;;

;; ;; Maybe a better solution is to not use "get" but "sccs get".
;; (defcustom vc-sccs-path
;;   (let ((path ()))
;;     (dolist (dir '("/usr/sccs" "/usr/lib/sccs" "/usr/libexec/sccs"))
;;       (if (file-directory-p dir)
;;           (push dir path)))
;;     path)
;;   "List of extra directories to search for SCCS commands."
;;   :type '(repeat directory)
;;   :group 'vc)

(defgroup vc-sccs nil
  "VC SCCS backend."
  :version "24.1"
  :group 'vc)

(defcustom vc-sccs-register-switches nil
  "Switches for registering a file in SCCS.
A string or list of strings passed to the checkin program by
\\[vc-register].  If nil, use the value of `vc-register-switches'.
If t, use no switches."
  :type '(choice (const :tag "Unspecified" nil)
		 (const :tag "None" t)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List" :value ("") string))
  :version "21.1"
  :group 'vc-sccs)

(defcustom vc-sccs-diff-switches nil
  "String or list of strings specifying switches for SCCS diff under VC.
If nil, use the value of `vc-diff-switches'.  If t, use no switches."
  :type '(choice (const :tag "Unspecified" nil)
		 (const :tag "None" t)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List" :value ("") string))
  :version "21.1"
  :group 'vc-sccs)

(defcustom vc-sccs-header '("%W%")
  "Header keywords to be inserted by `vc-insert-headers'."
  :type '(repeat string)
  :version "24.1"     ; no longer consult the obsolete vc-header-alist
  :group 'vc-sccs)

;;;###autoload
(defcustom vc-sccs-master-templates
  (purecopy '("%sSCCS/s.%s" "%ss.%s" vc-sccs-search-project-dir))
  "Where to look for SCCS master files.
For a description of possible values, see `vc-check-master-templates'."
  :type '(choice (const :tag "Use standard SCCS file names"
			("%sSCCS/s.%s" "%ss.%s" vc-sccs-search-project-dir))
		 (repeat :tag "User-specified"
			 (choice string
				 function)))
  :version "21.1"
  :group 'vc-sccs)


;;;
;;; Internal variables
;;;

(defconst vc-sccs-name-assoc-file "VC-names")


;;; Properties of the backend

(defun vc-sccs-revision-granularity () 'file)
(defun vc-sccs-checkout-model (files) 'locking)

;;;
;;; State-querying functions
;;;

;; The autoload cookie below places vc-sccs-registered directly into
;; loaddefs.el, so that vc-sccs.el does not need to be loaded for
;; every file that is visited.  The definition is repeated below
;; so that Help and etags can find it.

;;;###autoload (defun vc-sccs-registered(f) (vc-default-registered 'SCCS f))
(defun vc-sccs-registered (f) (vc-default-registered 'SCCS f))

(defun vc-sccs-state (file)
  "SCCS-specific function to compute the version control state."
  (if (not (vc-sccs-registered file))
      'unregistered
    (with-temp-buffer
      (if (vc-insert-file (vc-sccs-lock-file file))
	  (let* ((locks (vc-sccs-parse-locks))
		 (working-revision (vc-working-revision file))
		 (locking-user (cdr (assoc working-revision locks))))
	    (if (not locking-user)
		(if (vc-workfile-unchanged-p file)
		    'up-to-date
		  'unlocked-changes)
	      (if (string= locking-user (vc-user-login-name file))
		  'edited
		locking-user)))
	'up-to-date))))

(defun vc-sccs-state-heuristic (file)
  "SCCS-specific state heuristic."
  (if (not (vc-mistrust-permissions file))
      ;;   This implementation assumes that any file which is under version
      ;; control and has -rw-r--r-- is locked by its owner.  This is true
      ;; for both RCS and SCCS, which keep unlocked files at -r--r--r--.
      ;; We have to be careful not to exclude files with execute bits on;
      ;; scripts can be under version control too.  Also, we must ignore the
      ;; group-read and other-read bits, since paranoid users turn them off.
      (let* ((attributes  (file-attributes file 'string))
             (owner-name  (nth 2 attributes))
             (permissions (nth 8 attributes)))
	(if (string-match ".r-..-..-." permissions)
            'up-to-date
          (if (string-match ".rw..-..-." permissions)
              (if (file-ownership-preserved-p file)
                  'edited
                owner-name)
            ;; Strange permissions.
            ;; Fall through to real state computation.
            (vc-sccs-state file))))
    (vc-sccs-state file)))

(defun vc-sccs-dir-status (dir update-function)
  ;; FIXME: this function should be rewritten, using `vc-expand-dirs'
  ;; is not TRTD because it returns files from multiple backends.
  ;; It should also return 'unregistered files.

  ;; Doing lots of individual VC-state calls is painful, but
  ;; there is no better option in SCCS-land.
  (let ((flist (vc-expand-dirs (list dir)))
	(result nil))
    (dolist (file flist)
      (let ((state (vc-state file))
	    (frel (file-relative-name file)))
	(when (and (eq (vc-backend file) 'SCCS)
		   (not (eq state 'up-to-date)))
	  (push (list frel state) result))))
    (funcall update-function result)))

(defun vc-sccs-working-revision (file)
  "SCCS-specific version of `vc-working-revision'."
  (with-temp-buffer
    ;; The working revision is always the latest revision number.
    ;; To find this number, search the entire delta table,
    ;; rather than just the first entry, because the
    ;; first entry might be a deleted ("R") revision.
    (vc-insert-file (vc-name file) "^\001e\n\001[^s]")
    (vc-parse-buffer "^\001d D \\([^ ]+\\)" 1)))

(defun vc-sccs-workfile-unchanged-p (file)
  "SCCS-specific implementation of `vc-workfile-unchanged-p'."
  (zerop (apply 'vc-do-command "*vc*" 1 "vcdiff" (vc-name file)
                (list "--brief" "-q"
                      (concat "-r" (vc-working-revision file))))))


;;;
;;; State-changing functions
;;;

(defun vc-sccs-do-command (buffer okstatus command file-or-list &rest flags)
  ;; (let ((load-path (append vc-sccs-path load-path)))
  ;;   (apply 'vc-do-command buffer okstatus command file-or-list flags))
  (apply 'vc-do-command (or buffer "*vc*") okstatus "sccs" file-or-list command flags))

(defun vc-sccs-create-repo ()
  "Create a new SCCS repository."
  ;; SCCS is totally file-oriented, so all we have to do is make the directory
  (make-directory "SCCS"))

(defun vc-sccs-register (files &optional rev comment)
  "Register FILES into the SCCS version-control system.
REV is the optional revision number for the file.  COMMENT can be used
to provide an initial description of FILES.
Passes either `vc-sccs-register-switches' or `vc-register-switches'
to the SCCS command.

Automatically retrieve a read-only version of the files with keywords
expanded if `vc-keep-workfiles' is non-nil, otherwise, delete the workfile."
  (dolist (file files)
    (let* ((dirname (or (file-name-directory file) ""))
	   (basename (file-name-nondirectory file))
	   (project-file (vc-sccs-search-project-dir dirname basename)))
      (let ((vc-name
	     (or project-file
		 (format (car vc-sccs-master-templates) dirname basename))))
	(apply 'vc-sccs-do-command nil 0 "admin" vc-name
	       (and rev (not (string= rev "")) (concat "-r" rev))
	       "-fb"
	       (concat "-i" (file-relative-name file))
	       (and comment (concat "-y" comment))
	       (vc-switches 'SCCS 'register)))
      (delete-file file)
      (if vc-keep-workfiles
	  (vc-sccs-do-command nil 0 "get" (vc-name file))))))

(defun vc-sccs-responsible-p (file)
  "Return non-nil if SCCS thinks it would be responsible for registering FILE."
  ;; TODO: check for all the patterns in vc-sccs-master-templates
  (or (file-directory-p (expand-file-name "SCCS" (file-name-directory file)))
      (stringp (vc-sccs-search-project-dir (or (file-name-directory file) "")
					   (file-name-nondirectory file)))))

(defun vc-sccs-checkin (files rev comment)
  "SCCS-specific version of `vc-backend-checkin'."
  (dolist (file (vc-expand-dirs files))
    (apply 'vc-sccs-do-command nil 0 "delta" (vc-name file)
	   (if rev (concat "-r" rev))
	   (concat "-y" comment)
	   (vc-switches 'SCCS 'checkin))
    (if vc-keep-workfiles
	(vc-sccs-do-command nil 0 "get" (vc-name file)))))

(defun vc-sccs-find-revision (file rev buffer)
  (apply 'vc-sccs-do-command
	 buffer 0 "get" (vc-name file)
	 "-s" ;; suppress diagnostic output
	 "-p"
	 (and rev
	      (concat "-r"
		      (vc-sccs-lookup-triple file rev)))
	 (vc-switches 'SCCS 'checkout)))

(defun vc-sccs-checkout (file &optional editable rev)
  "Retrieve a copy of a saved revision of SCCS controlled FILE.
If FILE is a directory, all version-controlled files beneath are checked out.
EDITABLE non-nil means that the file should be writable and
locked.  REV is the revision to check out."
  (if (file-directory-p file)
      (mapc 'vc-sccs-checkout (vc-expand-dirs (list file)))
    (let ((file-buffer (get-file-buffer file))
	  switches)
      (message "Checking out %s..." file)
      (save-excursion
	;; Change buffers to get local value of vc-checkout-switches.
	(if file-buffer (set-buffer file-buffer))
	(setq switches (vc-switches 'SCCS 'checkout))
	;; Save this buffer's default-directory
	;; and use save-excursion to make sure it is restored
	;; in the same buffer it was saved in.
	(let ((default-directory default-directory))
	  (save-excursion
	    ;; Adjust the default-directory so that the check-out creates
	    ;; the file in the right place.
	    (setq default-directory (file-name-directory file))

	    (and rev (or (string= rev "")
			 (not (stringp rev)))
		 (setq rev nil))
	    (apply 'vc-sccs-do-command nil 0 "get" (vc-name file)
		   (if editable "-e")
		   (and rev (concat "-r" (vc-sccs-lookup-triple file rev)))
		   switches))))
      (message "Checking out %s...done" file))))

(defun vc-sccs-rollback (files)
  "Roll back, undoing the most recent checkins of FILES.  Directories
are expanded to all version-controlled subfiles."
  (setq files (vc-expand-dirs files))
  (if (not files)
      (error "SCCS backend doesn't support directory-level rollback"))
  (dolist (file files)
	  (let ((discard (vc-working-revision file)))
	    (if (null (yes-or-no-p (format "Remove version %s from %s history? "
					   discard file)))
		(error "Aborted"))
	    (message "Removing revision %s from %s..." discard file)
	    (vc-sccs-do-command nil 0 "rmdel"
                                (vc-name file) (concat "-r" discard))
	    (vc-sccs-do-command nil 0 "get" (vc-name file) nil))))

(defun vc-sccs-revert (file &optional contents-done)
  "Revert FILE to the version it was based on. If FILE is a directory,
revert all subfiles."
  (if (file-directory-p file)
      (mapc 'vc-sccs-revert (vc-expand-dirs (list file)))
    (vc-sccs-do-command nil 0 "unget" (vc-name file))
    (vc-sccs-do-command nil 0 "get" (vc-name file))
    ;; Checking out explicit revisions is not supported under SCCS, yet.
    ;; We always "revert" to the latest revision; therefore
    ;; vc-working-revision is cleared here so that it gets recomputed.
    (vc-file-setprop file 'vc-working-revision nil)))

(defun vc-sccs-steal-lock (file &optional rev)
  "Steal the lock on the current workfile for FILE and revision REV."
  (if (file-directory-p file)
      (mapc 'vc-sccs-steal-lock (vc-expand-dirs (list file)))
    (vc-sccs-do-command nil 0 "unget"
			(vc-name file) "-n" (if rev (concat "-r" rev)))
    (vc-sccs-do-command nil 0 "get"
			(vc-name file) "-g" (if rev (concat "-r" rev)))))

(defun vc-sccs-modify-change-comment (files rev comment)
  "Modify (actually, append to) the change comments for FILES on a specified REV."
  (dolist (file (vc-expand-dirs files))
    (vc-sccs-do-command nil 0 "cdc" (vc-name file)
                        (concat "-y" comment) (concat "-r" rev))))


;;;
;;; History functions
;;;

(defun vc-sccs-print-log (files buffer &optional shortlog start-revision-ignored limit)
  "Get change log associated with FILES."
  (setq files (vc-expand-dirs files))
  (vc-sccs-do-command buffer 0 "prs" (mapcar 'vc-name files))
  (when limit 'limit-unsupported))

(defun vc-sccs-diff (files &optional oldvers newvers buffer)
  "Get a difference report using SCCS between two filesets."
  (setq files (vc-expand-dirs files))
  (setq oldvers (vc-sccs-lookup-triple (car files) oldvers))
  (setq newvers (vc-sccs-lookup-triple (car files) newvers))
  (apply 'vc-do-command (or buffer "*vc-diff*")
	 1 "vcdiff" (mapcar 'vc-name (vc-expand-dirs files))
         (append (list "-q"
                       (and oldvers (concat "-r" oldvers))
                       (and newvers (concat "-r" newvers)))
                 (vc-switches 'SCCS 'diff))))


;;;
;;; Tag system.  SCCS doesn't have tags, so we simulate them by maintaining
;;; our own set of name-to-revision mappings.
;;;

(defun vc-sccs-create-tag (dir name branchp)
  (when branchp
    (error "SCCS backend does not support module branches"))
  (let ((result (vc-tag-precondition dir)))
    (if (stringp result)
	(error "File %s is not up-to-date" result)
      (vc-file-tree-walk
       dir
       (lambda (f)
	 (vc-sccs-add-triple name f (vc-working-revision f)))))))


;;;
;;; Miscellaneous
;;;

(defun vc-sccs-previous-revision (file rev)
  (vc-call-backend 'RCS 'previous-revision file rev))

(defun vc-sccs-next-revision (file rev)
  (vc-call-backend 'RCS 'next-revision file rev))

(defun vc-sccs-check-headers ()
  "Check if the current file has any headers in it."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward  "%[A-Z]%" nil t)))

(defun vc-sccs-rename-file (old new)
  ;; Move the master file (using vc-rcs-master-templates).
  (vc-rename-master (vc-name old) new vc-sccs-master-templates)
  ;; Update the tag file.
  (with-current-buffer
      (find-file-noselect
       (expand-file-name vc-sccs-name-assoc-file
			 (file-name-directory (vc-name old))))
    (goto-char (point-min))
    ;; (replace-regexp (concat ":" (regexp-quote old) "$") (concat ":" new))
    (while (re-search-forward (concat ":" (regexp-quote old) "$") nil t)
      (replace-match (concat ":" new) nil nil))
    (basic-save-buffer)
    (kill-buffer (current-buffer))))

(defun vc-sccs-find-file-hook ()
  ;; If the file is locked by some other user, make
  ;; the buffer read-only.  Like this, even root
  ;; cannot modify a file that someone else has locked.
  (and (stringp (vc-state buffer-file-name 'SCCS))
       (setq buffer-read-only t)))


;;;
;;; Internal functions
;;;

;; This function is wrapped with `progn' so that the autoload cookie
;; copies the whole function itself into loaddefs.el rather than just placing
;; a (autoload 'vc-sccs-search-project-dir "vc-sccs") which would not
;; help us avoid loading vc-sccs.
;;;###autoload
(progn (defun vc-sccs-search-project-dir (dirname basename)
  "Return the name of a master file in the SCCS project directory.
Does not check whether the file exists but returns nil if it does not
find any project directory."
  (let ((project-dir (getenv "PROJECTDIR")) dirs dir)
    (when project-dir
      (if (file-name-absolute-p project-dir)
	  (setq dirs '("SCCS" ""))
	(setq dirs '("src/SCCS" "src" "source/SCCS" "source"))
	(setq project-dir (expand-file-name (concat "~" project-dir))))
      (while (and (not dir) dirs)
	(setq dir (expand-file-name (car dirs) project-dir))
	(unless (file-directory-p dir)
	  (setq dir nil)
	  (setq dirs (cdr dirs))))
      (and dir (expand-file-name (concat "s." basename) dir))))))

(defun vc-sccs-lock-file (file)
  "Generate lock file name corresponding to FILE."
  (let ((master (vc-name file)))
    (and
     master
     (string-match "\\(.*/\\)\\(s\\.\\)\\(.*\\)" master)
     (replace-match "p." t t master 2))))

(defun vc-sccs-parse-locks ()
  "Parse SCCS locks in current buffer.
The result is a list of the form ((REVISION . USER) (REVISION . USER) ...)."
  (let (master-locks)
    (goto-char (point-min))
    (while (re-search-forward "^\\([0-9.]+\\) [0-9.]+ \\([^ ]+\\) .*\n?"
			      nil t)
      (setq master-locks
	    (cons (cons (match-string 1) (match-string 2)) master-locks)))
    ;; FIXME: is it really necessary to reverse ?
    (nreverse master-locks)))

(defun vc-sccs-add-triple (name file rev)
  (with-current-buffer
      (find-file-noselect
       (expand-file-name vc-sccs-name-assoc-file
			 (file-name-directory (vc-name file))))
    (goto-char (point-max))
    (insert name "\t:\t" file "\t" rev "\n")
    (basic-save-buffer)
    (kill-buffer (current-buffer))))

(defun vc-sccs-lookup-triple (file name)
  "Return the numeric revision corresponding to a named tag of FILE.
If NAME is nil or a revision number string it's just passed through."
  (if (or (null name)
	  (let ((firstchar (aref name 0)))
	    (and (>= firstchar ?0) (<= firstchar ?9))))
      name
    (with-temp-buffer
      (vc-insert-file
       (expand-file-name vc-sccs-name-assoc-file
			 (file-name-directory (vc-name file))))
      (vc-parse-buffer (concat name "\t:\t" file "\t\\(.+\\)") 1))))

(provide 'vc-sccs)

;;; vc-sccs.el ends here
