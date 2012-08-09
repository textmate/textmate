;;; vc-rcs.el --- support for RCS version-control

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

;; See vc.el

;; Some features will not work with ancient RCS versions.  Where
;; appropriate, VC finds out which version you have, and allows or
;; disallows those features.

;; You can support the RCS -x option by customizing vc-rcs-master-templates.

;;; Code:

;;;
;;; Customization options
;;;

(eval-when-compile
  (require 'cl)
  (require 'vc))

(defgroup vc-rcs nil
  "VC RCS backend."
  :version "24.1"
  :group 'vc)

(defcustom vc-rcs-release nil
  "The release number of your RCS installation, as a string.
If nil, VC itself computes this value when it is first needed."
  :type '(choice (const :tag "Auto" nil)
		 (string :tag "Specified")
		 (const :tag "Unknown" unknown))
  :group 'vc-rcs)

(defcustom vc-rcs-register-switches nil
  "Switches for registering a file in RCS.
A string or list of strings passed to the checkin program by
\\[vc-register].  If nil, use the value of `vc-register-switches'.
If t, use no switches."
  :type '(choice (const :tag "Unspecified" nil)
		 (const :tag "None" t)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List" :value ("") string))
  :version "21.1"
  :group 'vc-rcs)

(defcustom vc-rcs-diff-switches nil
  "String or list of strings specifying switches for RCS diff under VC.
If nil, use the value of `vc-diff-switches'.  If t, use no switches."
  :type '(choice (const :tag "Unspecified" nil)
                 (const :tag "None" t)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List" :value ("") string))
  :version "21.1"
  :group 'vc-rcs)

(defcustom vc-rcs-header '("\$Id\$")
  "Header keywords to be inserted by `vc-insert-headers'."
  :type '(repeat string)
  :version "24.1"     ; no longer consult the obsolete vc-header-alist
  :group 'vc-rcs)

(defcustom vc-rcsdiff-knows-brief nil
  "Indicates whether rcsdiff understands the --brief option.
The value is either `yes', `no', or nil.  If it is nil, VC tries
to use --brief and sets this variable to remember whether it worked."
  :type '(choice (const :tag "Work out" nil) (const yes) (const no))
  :group 'vc-rcs)

;;;###autoload
(defcustom vc-rcs-master-templates
  (purecopy '("%sRCS/%s,v" "%s%s,v" "%sRCS/%s"))
  "Where to look for RCS master files.
For a description of possible values, see `vc-check-master-templates'."
  :type '(choice (const :tag "Use standard RCS file names"
			'("%sRCS/%s,v" "%s%s,v" "%sRCS/%s"))
		 (repeat :tag "User-specified"
			 (choice string
				 function)))
  :version "21.1"
  :group 'vc-rcs)


;;; Properties of the backend

(defun vc-rcs-revision-granularity () 'file)

(defun vc-rcs-checkout-model (files)
  "RCS-specific version of `vc-checkout-model'."
  (let ((file (if (consp files) (car files) files))
        result)
    (when vc-consult-headers
      (vc-file-setprop file 'vc-checkout-model nil)
      (vc-rcs-consult-headers file)
      (setq result (vc-file-getprop file 'vc-checkout-model)))
    (or result
        (progn (vc-rcs-fetch-master-state file)
               (vc-file-getprop file 'vc-checkout-model)))))

;;;
;;; State-querying functions
;;;

;; The autoload cookie below places vc-rcs-registered directly into
;; loaddefs.el, so that vc-rcs.el does not need to be loaded for
;; every file that is visited.
;;;###autoload
(progn
(defun vc-rcs-registered (f) (vc-default-registered 'RCS f)))

(defun vc-rcs-state (file)
  "Implementation of `vc-state' for RCS."
  (if (not (vc-rcs-registered file))
      'unregistered
    (or (boundp 'vc-rcs-headers-result)
	(and vc-consult-headers
	     (vc-rcs-consult-headers file)))
    (let ((state
	   ;; vc-working-revision might not be known; in that case the
	   ;; property is nil.  vc-rcs-fetch-master-state knows how to
	   ;; handle that.
	   (vc-rcs-fetch-master-state file
				      (vc-file-getprop file
						       'vc-working-revision))))
      (if (not (eq state 'up-to-date))
	  state
	(if (vc-workfile-unchanged-p file)
	    'up-to-date
	  (if (eq (vc-rcs-checkout-model (list file)) 'locking)
	      'unlocked-changes
	    'edited))))))

(defun vc-rcs-state-heuristic (file)
  "State heuristic for RCS."
  (let (vc-rcs-headers-result)
    (if (and vc-consult-headers
             (setq vc-rcs-headers-result
                   (vc-rcs-consult-headers file))
             (eq vc-rcs-headers-result 'rev-and-lock))
        (let ((state (vc-file-getprop file 'vc-state)))
          ;; If the headers say that the file is not locked, the
          ;; permissions can tell us whether locking is used for
          ;; the file or not.
          (if (and (eq state 'up-to-date)
                   (not (vc-mistrust-permissions file))
                   (file-exists-p file))
              (cond
               ((string-match ".rw..-..-." (nth 8 (file-attributes file)))
                (vc-file-setprop file 'vc-checkout-model 'implicit)
		(setq state
		      (if (vc-rcs-workfile-is-newer file)
			  'edited
			'up-to-date)))
               ((string-match ".r-..-..-." (nth 8 (file-attributes file)))
                (vc-file-setprop file 'vc-checkout-model 'locking))))
          state)
      (if (not (vc-mistrust-permissions file))
          (let* ((attributes  (file-attributes file 'string))
                 (owner-name  (nth 2 attributes))
                 (permissions (nth 8 attributes)))
            (cond ((and permissions (string-match ".r-..-..-." permissions))
                   (vc-file-setprop file 'vc-checkout-model 'locking)
                   'up-to-date)
                  ((and permissions (string-match ".rw..-..-." permissions))
		   (if (eq (vc-rcs-checkout-model file) 'locking)
		       (if (file-ownership-preserved-p file)
			   'edited
			 owner-name)
		     (if (vc-rcs-workfile-is-newer file)
			 'edited
		       'up-to-date)))
                  (t
                   ;; Strange permissions.  Fall through to
                   ;; expensive state computation.
                   (vc-rcs-state file))))
        (vc-rcs-state file)))))

(defun vc-rcs-dir-status (dir update-function)
  ;; FIXME: this function should be rewritten or `vc-expand-dirs'
  ;; should be changed to take a backend parameter.  Using
  ;; `vc-expand-dirs' is not TRTD because it returns files from
  ;; multiple backends.  It should also return 'unregistered files.

  ;; Doing individual vc-state calls is painful but there
  ;; is no better way in RCS-land.
  (let ((flist (vc-expand-dirs (list dir)))
	(result nil))
    (dolist (file flist)
      (let ((state (vc-state file))
	    (frel (file-relative-name file)))
	(when (and (eq (vc-backend file) 'RCS)
		   (not (eq state 'up-to-date)))
	  (push (list frel state) result))))
    (funcall update-function result)))

(defun vc-rcs-working-revision (file)
  "RCS-specific version of `vc-working-revision'."
  (or (and vc-consult-headers
           (vc-rcs-consult-headers file)
           (vc-file-getprop file 'vc-working-revision))
      (progn
        (vc-rcs-fetch-master-state file)
        (vc-file-getprop file 'vc-working-revision))))

(defun vc-rcs-latest-on-branch-p (file &optional version)
  "Return non-nil if workfile version of FILE is the latest on its branch.
When VERSION is given, perform check for that version."
  (unless version (setq version (vc-working-revision file)))
  (with-temp-buffer
    (string= version
	     (if (vc-rcs-trunk-p version)
		 (progn
		   ;; Compare VERSION to the head version number.
		   (vc-insert-file (vc-name file) "^[0-9]")
		   (vc-parse-buffer "^head[ \t\n]+\\([^;]+\\);" 1))
	       ;; If we are not on the trunk, we need to examine the
	       ;; whole current branch.
	       (vc-insert-file (vc-name file) "^desc")
	       (vc-rcs-find-most-recent-rev (vc-branch-part version))))))

(defun vc-rcs-workfile-unchanged-p (file)
  "RCS-specific implementation of `vc-workfile-unchanged-p'."
  ;; Try to use rcsdiff --brief.  If rcsdiff does not understand that,
  ;; do a double take and remember the fact for the future
  (let* ((version (concat "-r" (vc-working-revision file)))
         (status (if (eq vc-rcsdiff-knows-brief 'no)
                     (vc-do-command "*vc*" 1 "rcsdiff" file version)
                   (vc-do-command "*vc*" 2 "rcsdiff" file "--brief" version))))
    (if (eq status 2)
        (if (not vc-rcsdiff-knows-brief)
            (setq vc-rcsdiff-knows-brief 'no
                  status (vc-do-command "*vc*" 1 "rcsdiff" file version))
          (error "rcsdiff failed"))
      (if (not vc-rcsdiff-knows-brief) (setq vc-rcsdiff-knows-brief 'yes)))
    ;; The workfile is unchanged if rcsdiff found no differences.
    (zerop status)))


;;;
;;; State-changing functions
;;;

(defun vc-rcs-create-repo ()
  "Create a new RCS repository."
  ;; RCS is totally file-oriented, so all we have to do is make the directory.
  (make-directory "RCS"))

(defun vc-rcs-register (files &optional rev comment)
  "Register FILES into the RCS version-control system.
REV is the optional revision number for the files.  COMMENT can be used
to provide an initial description for each FILES.
Passes either `vc-rcs-register-switches' or `vc-register-switches'
to the RCS command.

Automatically retrieve a read-only version of the file with keywords
expanded if `vc-keep-workfiles' is non-nil, otherwise, delete the workfile."
  (let (subdir name)
    ;; When REV is specified, we need to force using "-t-".
    (when rev (unless comment (setq comment "")))
    (dolist (file files)
      (and (not (file-exists-p
		 (setq subdir (expand-file-name "RCS"
						(file-name-directory file)))))
	   (not (directory-files (file-name-directory file)
				 nil ".*,v$" t))
	   (yes-or-no-p "Create RCS subdirectory? ")
	   (make-directory subdir))
      (apply 'vc-do-command "*vc*" 0 "ci" file
	     ;; if available, use the secure registering option
	     (and (vc-rcs-release-p "5.6.4") "-i")
	     (concat (if vc-keep-workfiles "-u" "-r") rev)
	     (and comment (concat "-t-" comment))
	     (vc-switches 'RCS 'register))
      ;; parse output to find master file name and workfile version
      (with-current-buffer "*vc*"
	(goto-char (point-min))
	(if (not (setq name
		       (if (looking-at (concat "^\\(.*\\)  <--	"
					       (file-name-nondirectory file)))
			   (match-string 1))))
	    ;; if we couldn't find the master name,
	    ;; run vc-rcs-registered to get it
	    ;; (will be stored into the vc-name property)
	    (vc-rcs-registered file)
	  (vc-file-setprop file 'vc-name
			   (if (file-name-absolute-p name)
			       name
			     (expand-file-name
			      name
			      (file-name-directory file))))))
      (vc-file-setprop file 'vc-working-revision
		       (if (re-search-forward
			    "^initial revision: \\([0-9.]+\\).*\n"
			    nil t)
			   (match-string 1))))))

(defun vc-rcs-responsible-p (file)
  "Return non-nil if RCS thinks it would be responsible for registering FILE."
  ;; TODO: check for all the patterns in vc-rcs-master-templates
  (file-directory-p (expand-file-name "RCS"
                                      (if (file-directory-p file)
                                          file
                                        (file-name-directory file)))))

(defun vc-rcs-receive-file (file rev)
  "Implementation of receive-file for RCS."
  (let ((checkout-model (vc-rcs-checkout-model (list file))))
    (vc-rcs-register file rev "")
    (when (eq checkout-model 'implicit)
      (vc-rcs-set-non-strict-locking file))
    (vc-rcs-set-default-branch file (concat rev ".1"))))

(defun vc-rcs-unregister (file)
  "Unregister FILE from RCS.
If this leaves the RCS subdirectory empty, ask the user
whether to remove it."
  (let* ((master (vc-name file))
	 (dir (file-name-directory master))
	 (backup-info (find-backup-file-name master)))
    (if (not backup-info)
	(delete-file master)
      (rename-file master (car backup-info) 'ok-if-already-exists)
      (dolist (f (cdr backup-info)) (ignore-errors (delete-file f))))
    (and (string= (file-name-nondirectory (directory-file-name dir)) "RCS")
	 ;; check whether RCS dir is empty, i.e. it does not
	 ;; contain any files except "." and ".."
	 (not (directory-files dir nil
			       "^\\([^.]\\|\\.[^.]\\|\\.\\.[^.]\\).*"))
	 (yes-or-no-p (format "Directory %s is empty; remove it? " dir))
	 (delete-directory dir))))

(defun vc-rcs-checkin (files rev comment)
  "RCS-specific version of `vc-backend-checkin'."
  (let ((switches (vc-switches 'RCS 'checkin)))
    ;; Now operate on the files
    (dolist (file (vc-expand-dirs files))
      (let ((old-version (vc-working-revision file)) new-version
	    (default-branch (vc-file-getprop file 'vc-rcs-default-branch)))
	;; Force branch creation if an appropriate
	;; default branch has been set.
	(and (not rev)
	     default-branch
	     (string-match (concat "^" (regexp-quote old-version) "\\.")
			   default-branch)
	     (setq rev default-branch)
	     (setq switches (cons "-f" switches)))
	(if (and (not rev) old-version)
	    (setq rev (vc-branch-part old-version)))
	(apply 'vc-do-command "*vc*" 0 "ci" (vc-name file)
	       ;; if available, use the secure check-in option
	       (and (vc-rcs-release-p "5.6.4") "-j")
	       (concat (if vc-keep-workfiles "-u" "-r") rev)
	       (concat "-m" comment)
	       switches)
	(vc-file-setprop file 'vc-working-revision nil)

	;; determine the new workfile version
	(set-buffer "*vc*")
	(goto-char (point-min))
	(when (or (re-search-forward
		   "new revision: \\([0-9.]+\\);" nil t)
		  (re-search-forward
		   "reverting to previous revision \\([0-9.]+\\)" nil t))
	  (setq new-version (match-string 1))
	  (vc-file-setprop file 'vc-working-revision new-version))

	;; if we got to a different branch, adjust the default
	;; branch accordingly
	(cond
	 ((and old-version new-version
	       (not (string= (vc-branch-part old-version)
			     (vc-branch-part new-version))))
	  (vc-rcs-set-default-branch file
				     (if (vc-rcs-trunk-p new-version) nil
				       (vc-branch-part new-version)))
	  ;; If this is an old (pre-1992!) RCS release, we might have
	  ;; to remove a remaining lock.
	  (if (not (vc-rcs-release-p "5.6.2"))
	      ;; exit status of 1 is also accepted.
	      ;; It means that the lock was removed before.
	      (vc-do-command "*vc*" 1 "rcs" (vc-name file)
			     (concat "-u" old-version)))))))))

(defun vc-rcs-find-revision (file rev buffer)
  (apply 'vc-do-command
	 (or buffer "*vc*") 0 "co" (vc-name file)
	 "-q" ;; suppress diagnostic output
	 (concat "-p" rev)
	 (vc-switches 'RCS 'checkout)))

(defun vc-rcs-checkout (file &optional editable rev)
  "Retrieve a copy of a saved version of FILE.  If FILE is a directory,
attempt the checkout for all registered files beneath it."
  (if (file-directory-p file)
      (mapc 'vc-rcs-checkout (vc-expand-dirs (list file)))
    (let ((file-buffer (get-file-buffer file))
	  switches)
      (message "Checking out %s..." file)
      (save-excursion
	;; Change buffers to get local value of vc-checkout-switches.
	(if file-buffer (set-buffer file-buffer))
	(setq switches (vc-switches 'RCS 'checkout))
	;; Save this buffer's default-directory
	;; and use save-excursion to make sure it is restored
	;; in the same buffer it was saved in.
	(let ((default-directory default-directory))
	  (save-excursion
	    ;; Adjust the default-directory so that the check-out creates
	    ;; the file in the right place.
	    (setq default-directory (file-name-directory file))
	    (let (new-version)
	      ;; if we should go to the head of the trunk,
	      ;; clear the default branch first
	      (and rev (string= rev "")
		   (vc-rcs-set-default-branch file nil))
	      ;; now do the checkout
	      (apply 'vc-do-command
		     "*vc*" 0 "co" (vc-name file)
		     ;; If locking is not strict, force to overwrite
		     ;; the writable workfile.
		     (if (eq (vc-rcs-checkout-model (list file)) 'implicit) "-f")
		     (if editable "-l")
		     (if (stringp rev)
			 ;; a literal revision was specified
			 (concat "-r" rev)
		       (let ((workrev (vc-working-revision file)))
			 (if workrev
			     (concat "-r"
				     (if (not rev)
					 ;; no revision specified:
					 ;; use current workfile version
					 workrev
				       ;; REV is t ...
				       (if (not (vc-rcs-trunk-p workrev))
					   ;; ... go to head of current branch
					   (vc-branch-part workrev)
					 ;; ... go to head of trunk
					 (vc-rcs-set-default-branch file
                                                                  nil)
                                       ""))))))
		   switches)
	    ;; determine the new workfile version
	    (with-current-buffer "*vc*"
	      (setq new-version
		    (vc-parse-buffer "^revision \\([0-9.]+\\).*\n" 1)))
	    (vc-file-setprop file 'vc-working-revision new-version)
	    ;; if necessary, adjust the default branch
	    (and rev (not (string= rev ""))
		 (vc-rcs-set-default-branch
		  file
		  (if (vc-rcs-latest-on-branch-p file new-version)
		      (if (vc-rcs-trunk-p new-version) nil
			(vc-branch-part new-version))
		    new-version)))))
	(message "Checking out %s...done" file))))))

(defun vc-rcs-rollback (files)
  "Roll back, undoing the most recent checkins of FILES.  Directories are
expanded to all registered subfiles in them."
  (if (not files)
      (error "RCS backend doesn't support directory-level rollback"))
  (dolist (file (vc-expand-dirs files))
	  (let* ((discard (vc-working-revision file))
		 (previous (if (vc-rcs-trunk-p discard) "" (vc-branch-part discard)))
		 (config (current-window-configuration))
		 (done nil))
	    (if (null (yes-or-no-p (format "Remove version %s from %s history? "
					   discard file)))
		(error "Aborted"))
	    (message "Removing revision %s from %s." discard file)
	    (vc-do-command "*vc*" 0 "rcs" (vc-name file) (concat "-o" discard))
	    ;; Check out the most recent remaining version.  If it
	    ;; fails, because the whole branch got deleted, do a
	    ;; double-take and check out the version where the branch
	    ;; started.
	    (while (not done)
	      (condition-case err
		  (progn
		    (vc-do-command "*vc*" 0 "co" (vc-name file) "-f"
				   (concat "-u" previous))
		    (setq done t))
		(error (set-buffer "*vc*")
		       (goto-char (point-min))
		       (if (search-forward "no side branches present for" nil t)
			   (progn (setq previous (vc-branch-part previous))
				  (vc-rcs-set-default-branch file previous)
				  ;; vc-do-command popped up a window with
				  ;; the error message.  Get rid of it, by
				  ;; restoring the old window configuration.
				  (set-window-configuration config))
			 ;; No, it was some other error: re-signal it.
			 (signal (car err) (cdr err)))))))))

(defun vc-rcs-revert (file &optional contents-done)
  "Revert FILE to the version it was based on.  If FILE is a directory,
revert all registered files beneath it."
  (if (file-directory-p file)
      (mapc 'vc-rcs-revert (vc-expand-dirs (list file)))
    (vc-do-command "*vc*" 0 "co" (vc-name file) "-f"
		   (concat (if (eq (vc-state file) 'edited) "-u" "-r")
			   (vc-working-revision file)))))

(defun vc-rcs-merge (file first-version &optional second-version)
  "Merge changes into current working copy of FILE.
The changes are between FIRST-VERSION and SECOND-VERSION."
  (vc-do-command "*vc*" 1 "rcsmerge" (vc-name file)
		 "-kk"			; ignore keyword conflicts
		 (concat "-r" first-version)
		 (if second-version (concat "-r" second-version))))

(defun vc-rcs-steal-lock (file &optional rev)
  "Steal the lock on the current workfile for FILE and revision REV.
If FILE is a directory, steal the lock on all registered files beneath it.
Needs RCS 5.6.2 or later for -M."
  (if (file-directory-p file)
      (mapc 'vc-rcs-steal-lock (vc-expand-dirs (list file)))
    (vc-do-command "*vc*" 0 "rcs" (vc-name file) "-M" (concat "-u" rev))
    ;; Do a real checkout after stealing the lock, so that we see
    ;; expanded headers.
    (vc-do-command "*vc*" 0 "co" (vc-name file) "-f" (concat "-l" rev))))

(defun vc-rcs-modify-change-comment (files rev comment)
  "Modify the change comments change on FILES on a specified REV.  If FILE is a
directory the operation is applied to all registered files beneath it."
  (dolist (file (vc-expand-dirs files))
    (vc-do-command "*vc*" 0 "rcs" (vc-name file)
		   (concat "-m" rev ":" comment))))


;;;
;;; History functions
;;;

(defun vc-rcs-print-log-cleanup ()
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (forward-line -1)
    (while (looking-at "=*\n")
      (delete-char (- (match-end 0) (match-beginning 0)))
      (forward-line -1))
    (goto-char (point-min))
    (when (looking-at "[\b\t\n\v\f\r ]+")
      (delete-char (- (match-end 0) (match-beginning 0))))))

(defun vc-rcs-print-log (files buffer &optional shortlog start-revision-ignored limit)
  "Get change log associated with FILE.  If FILE is a
directory the operation is applied to all registered files beneath it."
  (vc-do-command (or buffer "*vc*") 0 "rlog" (mapcar 'vc-name (vc-expand-dirs files)))
  (with-current-buffer (or buffer "*vc*")
    (vc-rcs-print-log-cleanup))
  (when limit 'limit-unsupported))

(defun vc-rcs-diff (files &optional oldvers newvers buffer)
  "Get a difference report using RCS between two sets of files."
  (apply 'vc-do-command (or buffer "*vc-diff*")
	 1		;; Always go synchronous, the repo is local
	 "rcsdiff" (vc-expand-dirs files)
         (append (list "-q"
                       (and oldvers (concat "-r" oldvers))
                       (and newvers (concat "-r" newvers)))
                 (vc-switches 'RCS 'diff))))

(defun vc-rcs-comment-history (file)
  "Return a string with all log entries stored in BACKEND for FILE."
  (with-current-buffer "*vc*"
    ;; Has to be written this way, this function is used by the CVS backend too
    (vc-call-backend (vc-backend file) 'print-log (list file))
    ;; Remove cruft
    (let ((separator (concat "^-+\nrevision [0-9.]+\ndate: .*\n"
			     "\\(branches: .*;\n\\)?"
			     "\\(\\*\\*\\* empty log message \\*\\*\\*\n\\)?")))
      (goto-char (point-max)) (forward-line -1)
      (while (looking-at "=*\n")
	(delete-char (- (match-end 0) (match-beginning 0)))
	(forward-line -1))
      (goto-char (point-min))
      (if (looking-at "[\b\t\n\v\f\r ]+")
	  (delete-char (- (match-end 0) (match-beginning 0))))
      (goto-char (point-min))
      (re-search-forward separator nil t)
      (delete-region (point-min) (point))
      (while (re-search-forward separator nil t)
	(delete-region (match-beginning 0) (match-end 0))))
    ;; Return the de-crufted comment list
    (buffer-string)))

(defun vc-rcs-annotate-command (file buffer &optional revision)
  "Annotate FILE, inserting the results in BUFFER.
Optional arg REVISION is a revision to annotate from."
  (vc-setup-buffer buffer)
  ;; Aside from the "head revision on the trunk", the instructions for
  ;; each revision on the trunk are an ordered list of kill and insert
  ;; commands necessary to go from the chronologically-following
  ;; revision to this one.  That is, associated with revision N are
  ;; edits that applied to revision N+1 would result in revision N.
  ;;
  ;; On a branch, however, (some) things are inverted: the commands
  ;; listed are those necessary to go from the chronologically-preceding
  ;; revision to this one.  That is, associated with revision N are
  ;; edits that applied to revision N-1 would result in revision N.
  ;;
  ;; So, to get per-line history info, we apply reverse-chronological
  ;; edits, starting with the head revision on the trunk, all the way
  ;; back through the initial revision (typically "1.1" or similar),
  ;; then apply forward-chronological edits -- keeping track of which
  ;; revision is associated with each inserted line -- until we reach
  ;; the desired revision for display (which may be either on the trunk
  ;; or on a branch).
  (let* ((tree (with-temp-buffer
                 (insert-file-contents (vc-rcs-registered file))
                 (vc-rcs-parse)))
         (revisions (cdr (assq 'revisions tree)))
         ;; The revision N whose instructions we currently are processing.
         (cur (cdr (assq 'head (cdr (assq 'headers tree)))))
         ;; Alist from the parse tree for N.
         (meta (cdr (assoc cur revisions)))
         ;; Point and temporary string, respectively.
         p s
         ;; "Next-branch list".  Nil means the desired revision to
         ;; display lives on the trunk.  Non-nil means it lives on a
         ;; branch, in which case the value is a list of revision pairs
         ;; (PARENT . CHILD), the first PARENT being on the trunk, that
         ;; links each series of revisions in the path from the initial
         ;; revision to the desired revision to display.
         nbls
         ;; "Path-accumulate-predicate plus revision/date/author".
         ;; Until set, forward-chronological edits are not accumulated.
         ;; Once set, its value (updated every revision) is used for
         ;; the text property `:vc-rcs-r/d/a' for inserts during
         ;; processing of forward-chronological instructions for N.
         ;; See internal func `r/d/a'.
         prda
         ;; List of forward-chronological instructions, each of the
         ;; form: (POS . ACTION), where POS is a buffer position.  If
         ;; ACTION is a string, it is inserted, otherwise it is taken as
         ;; the number of characters to be deleted.
         path
         ;; N+1.  When `cur' is "", this is the initial revision.
         pre)
    (unless revision
      (setq revision cur))
    (unless (assoc revision revisions)
      (error "No such revision: %s" revision))
    ;; Find which branches (if any) must be included in the edits.
    (let ((par revision)
          bpt kids)
      (while (setq bpt (vc-branch-part par)
                   par (vc-branch-part bpt))
        (setq kids (cdr (assq 'branches (cdr (assoc par revisions)))))
        ;; A branchpoint may have multiple children.  Find the right one.
        (while (not (string= bpt (vc-branch-part (car kids))))
          (setq kids (cdr kids)))
        (push (cons par (car kids)) nbls)))
    ;; Start with the full text.
    (set-buffer buffer)
    (insert (cdr (assq 'text meta)))
    ;; Apply reverse-chronological edits on the trunk, computing and
    ;; accumulating forward-chronological edits after some point, for
    ;; later.
    (flet ((r/d/a () (vector pre
                             (cdr (assq 'date meta))
                             (cdr (assq 'author meta)))))
      (while (when (setq pre cur cur (cdr (assq 'next meta)))
               (not (string= "" cur)))
        (setq
         ;; Start accumulating the forward-chronological edits when N+1
         ;; on the trunk is either the desired revision to display, or
         ;; the appropriate branchpoint for it.  Do this before
         ;; updating `meta' since `r/d/a' uses N+1's `meta' value.
         prda (when (or prda (string= (if nbls (caar nbls) revision) pre))
                (r/d/a))
         meta (cdr (assoc cur revisions)))
        ;; Edits in the parse tree specify a line number (in the buffer
        ;; *BEFORE* editing occurs) to start from, but line numbers
        ;; change as a result of edits.  To DTRT, we apply edits in
        ;; order of descending buffer position so that edits further
        ;; down in the buffer occur first w/o corrupting specified
        ;; buffer positions of edits occurring towards the beginning of
        ;; the buffer.  In this way we avoid using markers.  A pleasant
        ;; property of this approach is ability to push instructions
        ;; onto `path' directly, w/o need to maintain rev boundaries.
        (dolist (insn (cdr (assq :insn meta)))
          (goto-char (point-min))
          (forward-line (1- (pop insn)))
          (setq p (point))
          (case (pop insn)
            (k (setq s (buffer-substring-no-properties
                        p (progn (forward-line (car insn))
                                 (point))))
               (when prda
                 (push `(,p . ,(propertize s :vc-rcs-r/d/a prda)) path))
               (delete-region p (point)))
            (i (setq s (car insn))
               (when prda
                 (push `(,p . ,(length s)) path))
               (insert s)))))
      ;; For the initial revision, setting `:vc-rcs-r/d/a' directly is
      ;; equivalent to pushing an insert instruction (of the entire buffer
      ;; contents) onto `path' then erasing the buffer, but less wasteful.
      (put-text-property (point-min) (point-max) :vc-rcs-r/d/a (r/d/a))
      ;; Now apply the forward-chronological edits for the trunk.
      (dolist (insn path)
        (goto-char (pop insn))
        (if (stringp insn)
            (insert insn)
          (delete-char insn)))
      ;; Now apply the forward-chronological edits (directly from the
      ;; parse-tree) for the branch(es), if necessary.  We re-use vars
      ;; `pre' and `meta' for the sake of internal func `r/d/a'.
      (while nbls
        (setq pre (cdr (pop nbls)))
        (while (progn
                 (setq meta (cdr (assoc pre revisions))
                       prda nil)
                 (dolist (insn (cdr (assq :insn meta)))
                   (goto-char (point-min))
                   (forward-line (1- (pop insn)))
                   (case (pop insn)
                     (k (delete-region
                         (point) (progn (forward-line (car insn))
                                        (point))))
                     (i (insert (propertize
                                 (car insn)
                                 :vc-rcs-r/d/a
                                 (or prda (setq prda (r/d/a))))))))
                 (prog1 (not (string= (if nbls (caar nbls) revision) pre))
                   (setq pre (cdr (assq 'next meta)))))))))
  ;; Lastly, for each line, insert at bol nicely-formatted history info.
  ;; We do two passes to collect summary information used to minimize
  ;; the annotation's usage of screen real-estate: (1) Consider rendered
  ;; width of revision plus author together as a unit; and (2) Omit
  ;; author entirely if all authors are the same as the user.
  (let ((ht (make-hash-table :test 'eq))
        (me (user-login-name))
        (maxw 0)
        (all-me t)
        rda w a)
    (goto-char (point-max))
    (while (not (bobp))
      (forward-line -1)
      (setq rda (get-text-property (point) :vc-rcs-r/d/a))
      (unless (gethash rda ht)
        (setq a (aref rda 2)
              all-me (and all-me (string= a me)))
        (puthash rda (setq w (+ (length (aref rda 0))
                                (length a)))
                 ht)
        (setq maxw (max w maxw))))
    (let ((padding (make-string maxw 32)))
      (flet ((pad (w) (substring-no-properties padding w))
             (render (rda &rest ls)
                     (propertize
                      (apply 'concat
                             (format-time-string "%Y-%m-%d" (aref rda 1))
                             "  "
                             (aref rda 0)
                             ls)
                      :vc-annotate-prefix t
                      :vc-rcs-r/d/a rda)))
        (maphash
         (if all-me
             (lambda (rda w)
               (puthash rda (render rda (pad w) ": ") ht))
           (lambda (rda w)
             (puthash rda (render rda " " (pad w) " " (aref rda 2) ": ") ht)))
         ht)))
    (while (not (eobp))
      (insert (gethash (get-text-property (point) :vc-rcs-r/d/a) ht))
      (forward-line 1))))

(declare-function vc-annotate-convert-time "vc-annotate" (time))

(defun vc-rcs-annotate-current-time ()
  "Return the current time, based at midnight of the current day, and
encoded as fractional days."
  (vc-annotate-convert-time
   (apply 'encode-time 0 0 0 (nthcdr 3 (decode-time (current-time))))))

(defun vc-rcs-annotate-time ()
  "Return the time of the next annotation (as fraction of days)
systime, or nil if there is none.  Also, reposition point."
  (unless (eobp)
    (prog1 (vc-annotate-convert-time
            (aref (get-text-property (point) :vc-rcs-r/d/a) 1))
      (goto-char (next-single-property-change (point) :vc-annotate-prefix)))))

(defun vc-rcs-annotate-extract-revision-at-line ()
  (aref (get-text-property (point) :vc-rcs-r/d/a) 0))


;;;
;;; Tag system
;;;

(defun vc-rcs-create-tag (dir name branchp)
  (when branchp
    (error "RCS backend does not support module branches"))
  (let ((result (vc-tag-precondition dir)))
    (if (stringp result)
	(error "File %s is not up-to-date" result)
      (vc-file-tree-walk
       dir
       (lambda (f)
	 (vc-do-command "*vc*" 0 "rcs" (vc-name f) (concat "-n" name ":")))))))


;;;
;;; Miscellaneous
;;;

(defun vc-rcs-trunk-p (rev)
  "Return t if REV is a revision on the trunk."
  (not (eq nil (string-match "\\`[0-9]+\\.[0-9]+\\'" rev))))

(defun vc-rcs-minor-part (rev)
  "Return the minor revision number of a revision number REV."
  (string-match "[0-9]+\\'" rev)
  (substring rev (match-beginning 0) (match-end 0)))

(defun vc-rcs-previous-revision (file rev)
  "Return the revision number immediately preceding REV for FILE,
or nil if there is no previous revision.  This default
implementation works for MAJOR.MINOR-style revision numbers as
used by RCS and CVS."
  (let ((branch (vc-branch-part rev))
        (minor-num (string-to-number (vc-rcs-minor-part rev))))
    (when branch
      (if (> minor-num 1)
          ;; revision does probably not start a branch or release
          (concat branch "." (number-to-string (1- minor-num)))
        (if (vc-rcs-trunk-p rev)
            ;; we are at the beginning of the trunk --
            ;; don't know anything to return here
            nil
          ;; we are at the beginning of a branch --
          ;; return revision of starting point
          (vc-branch-part branch))))))

(defun vc-rcs-next-revision (file rev)
  "Return the revision number immediately following REV for FILE,
or nil if there is no next revision.  This default implementation
works for MAJOR.MINOR-style revision numbers as used by RCS
and CVS."
  (when (not (string= rev (vc-working-revision file)))
    (let ((branch (vc-branch-part rev))
	  (minor-num (string-to-number (vc-rcs-minor-part rev))))
      (concat branch "." (number-to-string (1+ minor-num))))))

(defun vc-rcs-update-changelog (files)
  "Default implementation of update-changelog.
Uses `rcs2log' which only works for RCS and CVS."
  ;; FIXME: We (c|sh)ould add support for cvs2cl
  (let ((odefault default-directory)
	(changelog (find-change-log))
	;; Presumably not portable to non-Unixy systems, along with rcs2log:
	(tempfile (make-temp-file
		   (expand-file-name "vc"
				     (or small-temporary-file-directory
					 temporary-file-directory))))
        (login-name (or user-login-name
                        (format "uid%d" (number-to-string (user-uid)))))
	(full-name (or add-log-full-name
		       (user-full-name)
		       (user-login-name)
		       (format "uid%d" (number-to-string (user-uid)))))
	(mailing-address (or add-log-mailing-address
			     user-mail-address)))
    (find-file-other-window changelog)
    (barf-if-buffer-read-only)
    (vc-buffer-sync)
    (undo-boundary)
    (goto-char (point-min))
    (push-mark)
    (message "Computing change log entries...")
    (message "Computing change log entries... %s"
	     (unwind-protect
		 (progn
		   (setq default-directory odefault)
		   (if (eq 0 (apply 'call-process
                                    (expand-file-name "rcs2log"
                                                      exec-directory)
                                    nil (list t tempfile) nil
                                    "-c" changelog
                                    "-u" (concat login-name
                                                 "\t" full-name
                                                 "\t" mailing-address)
                                    (mapcar
                                     (lambda (f)
                                       (file-relative-name
					(expand-file-name f odefault)))
                                     files)))
                       "done"
		     (pop-to-buffer (get-buffer-create "*vc*"))
		     (erase-buffer)
		     (insert-file-contents tempfile)
		     "failed"))
	       (setq default-directory (file-name-directory changelog))
	       (delete-file tempfile)))))

(defun vc-rcs-check-headers ()
  "Check if the current file has any headers in it."
  (save-excursion
    (goto-char (point-min))
         (re-search-forward "\\$[A-Za-z\300-\326\330-\366\370-\377]+\
\\(: [\t -#%-\176\240-\377]*\\)?\\$" nil t)))

(defun vc-rcs-clear-headers ()
  "Implementation of vc-clear-headers for RCS."
  (let ((case-fold-search nil))
    (goto-char (point-min))
    (while (re-search-forward
            (concat "\\$\\(Author\\|Date\\|Header\\|Id\\|Locker\\|Name\\|"
                    "RCSfile\\|Revision\\|Source\\|State\\): [^$\n]+\\$")
            nil t)
      (replace-match "$\\1$"))))

(defun vc-rcs-rename-file (old new)
  ;; Just move the master file (using vc-rcs-master-templates).
  (vc-rename-master (vc-name old) new vc-rcs-master-templates))

(defun vc-rcs-find-file-hook ()
  ;; If the file is locked by some other user, make
  ;; the buffer read-only.  Like this, even root
  ;; cannot modify a file that someone else has locked.
  (and (stringp (vc-state buffer-file-name 'RCS))
       (setq buffer-read-only t)))


;;;
;;; Internal functions
;;;

(defun vc-rcs-workfile-is-newer (file)
  "Return non-nil if FILE is newer than its RCS master.
This likely means that FILE has been changed with respect
to its master version."
  (let ((file-time (nth 5 (file-attributes file)))
	(master-time (nth 5 (file-attributes (vc-name file)))))
    (or (> (nth 0 file-time) (nth 0 master-time))
	(and (= (nth 0 file-time) (nth 0 master-time))
	     (> (nth 1 file-time) (nth 1 master-time))))))

(defun vc-rcs-find-most-recent-rev (branch)
  "Find most recent revision on BRANCH."
  (goto-char (point-min))
  (let ((latest-rev -1) value)
    (while (re-search-forward (concat "^\\(" (regexp-quote branch)
				      "\\.\\([0-9]+\\)\\)\ndate[ \t]+[0-9.]+;")
			      nil t)
      (let ((rev (string-to-number (match-string 2))))
	(when (< latest-rev rev)
	  (setq latest-rev rev)
	  (setq value (match-string 1)))))
    (or value
	(vc-branch-part branch))))

(defun vc-rcs-fetch-master-state (file &optional working-revision)
  "Compute the master file's idea of the state of FILE.
If a WORKING-REVISION is given, compute the state of that version,
otherwise determine the workfile version based on the master file.
This function sets the properties `vc-working-revision' and
`vc-checkout-model' to their correct values, based on the master
file."
  (with-temp-buffer
    (if (or (not (vc-insert-file (vc-name file) "^[0-9]"))
            (progn (goto-char (point-min))
                   (not (looking-at "^head[ \t\n]+[^;]+;$"))))
        (error "File %s is not an RCS master file" (vc-name file)))
    (let ((workfile-is-latest nil)
	  (default-branch (vc-parse-buffer "^branch[ \t\n]+\\([^;]*\\);" 1)))
      (vc-file-setprop file 'vc-rcs-default-branch default-branch)
      (unless working-revision
	;; Workfile version not known yet.  Determine that first.  It
	;; is either the head of the trunk, the head of the default
	;; branch, or the "default branch" itself, if that is a full
	;; revision number.
	(cond
	 ;; no default branch
	 ((or (not default-branch) (string= "" default-branch))
	  (setq working-revision
		(vc-parse-buffer "^head[ \t\n]+\\([^;]+\\);" 1))
	  (setq workfile-is-latest t))
	 ;; default branch is actually a revision
	 ((string-match "^[0-9]+\\.[0-9]+\\(\\.[0-9]+\\.[0-9]+\\)*$"
			default-branch)
	  (setq working-revision default-branch))
	 ;; else, search for the head of the default branch
	 (t (vc-insert-file (vc-name file) "^desc")
	    (setq working-revision
		  (vc-rcs-find-most-recent-rev default-branch))
	    (setq workfile-is-latest t)))
	(vc-file-setprop file 'vc-working-revision working-revision))
      ;; Check strict locking
      (goto-char (point-min))
      (vc-file-setprop file 'vc-checkout-model
		       (if (re-search-forward ";[ \t\n]*strict;" nil t)
			   'locking 'implicit))
      ;; Compute state of workfile version
      (goto-char (point-min))
      (let ((locking-user
	     (vc-parse-buffer (concat "^locks[ \t\n]+[^;]*[ \t\n]+\\([^:]+\\):"
				      (regexp-quote working-revision)
				      "[^0-9.]")
			      1)))
	(cond
	 ;; not locked
	 ((not locking-user)
          (if (or workfile-is-latest
                  (vc-rcs-latest-on-branch-p file working-revision))
              ;; workfile version is latest on branch
              'up-to-date
            ;; workfile version is not latest on branch
            'needs-update))
	 ;; locked by the calling user
	 ((and (stringp locking-user)
	       (string= locking-user (vc-user-login-name file)))
          ;; Don't call `vc-rcs-checkout-model' to avoid inf-looping.
	  (if (or (eq (vc-file-getprop file 'vc-checkout-model) 'locking)
		  workfile-is-latest
		  (vc-rcs-latest-on-branch-p file working-revision))
	      'edited
	    ;; Locking is not used for the file, but the owner does
	    ;; have a lock, and there is a higher version on the current
	    ;; branch.  Not sure if this can occur, and if it is right
	    ;; to use `needs-merge' in this case.
	    'needs-merge))
	 ;; locked by somebody else
	 ((stringp locking-user)
	  locking-user)
	 (t
	  (error "Error getting state of RCS file")))))))

(defun vc-rcs-consult-headers (file)
  "Search for RCS headers in FILE, and set properties accordingly.

Returns: nil            if no headers were found
         'rev           if a workfile revision was found
         'rev-and-lock  if revision and lock info was found"
  (cond
   ((not (get-file-buffer file)) nil)
   ((let (status version locking-user)
      (with-current-buffer (get-file-buffer file)
        (save-excursion
          (goto-char (point-min))
          (cond
           ;; search for $Id or $Header
           ;; -------------------------
           ;; The `\ 's below avoid an RCS 5.7 bug when checking in this file.
           ((or (and (search-forward "$Id\ : " nil t)
                     (looking-at "[^ ]+ \\([0-9.]+\\) "))
                (and (progn (goto-char (point-min))
                            (search-forward "$Header\ : " nil t))
                     (looking-at "[^ ]+ \\([0-9.]+\\) ")))
            (goto-char (match-end 0))
            ;; if found, store the revision number ...
            (setq version (match-string-no-properties 1))
            ;; ... and check for the locking state
            (cond
             ((looking-at
               (concat "[0-9]+[/-][01][0-9][/-][0-3][0-9] "              ; date
                 "[0-2][0-9]:[0-5][0-9]+:[0-6][0-9]+\\([+-][0-9:]+\\)? " ; time
                       "[^ ]+ [^ ]+ "))                        ; author & state
              (goto-char (match-end 0)) ; [0-6] in regexp handles leap seconds
              (cond
               ;; unlocked revision
               ((looking-at "\\$")
                (setq locking-user 'none)
                (setq status 'rev-and-lock))
               ;; revision is locked by some user
               ((looking-at "\\([^ ]+\\) \\$")
                (setq locking-user (match-string-no-properties 1))
                (setq status 'rev-and-lock))
               ;; everything else: false
               (nil)))
             ;; unexpected information in
             ;; keyword string --> quit
             (nil)))
           ;; search for $Revision
           ;; --------------------
           ((re-search-forward (concat "\\$"
                                       "Revision: \\([0-9.]+\\) \\$")
                               nil t)
            ;; if found, store the revision number ...
            (setq version (match-string-no-properties 1))
            ;; and see if there's any lock information
            (goto-char (point-min))
            (if (re-search-forward (concat "\\$" "Locker:") nil t)
                (cond ((looking-at " \\([^ ]+\\) \\$")
                       (setq locking-user (match-string-no-properties 1))
                       (setq status 'rev-and-lock))
                      ((looking-at " *\\$")
                       (setq locking-user 'none)
                       (setq status 'rev-and-lock))
                      (t
                       (setq locking-user 'none)
                       (setq status 'rev-and-lock)))
              (setq status 'rev)))
           ;; else: nothing found
           ;; -------------------
           (t nil))))
     (if status (vc-file-setprop file 'vc-working-revision version))
     (and (eq status 'rev-and-lock)
	  (vc-file-setprop file 'vc-state
			   (cond
			    ((eq locking-user 'none) 'up-to-date)
			    ((string= locking-user (vc-user-login-name file))
                             'edited)
			    (t locking-user)))
	  ;; If the file has headers, we don't want to query the
	  ;; master file, because that would eliminate all the
	  ;; performance gain the headers brought us.  We therefore
	  ;; use a heuristic now to find out whether locking is used
	  ;; for this file.  If we trust the file permissions, and the
	  ;; file is not locked, then if the file is read-only we
          ;; assume that locking is used for the file, otherwise
          ;; locking is not used.
	  (not (vc-mistrust-permissions file))
	  (vc-up-to-date-p file)
	  (if (string-match ".r-..-..-." (nth 8 (file-attributes file)))
	      (vc-file-setprop file 'vc-checkout-model 'locking)
	    (vc-file-setprop file 'vc-checkout-model 'implicit)))
     status))))

(defun vc-release-greater-or-equal (r1 r2)
  "Compare release numbers, represented as strings.
Release components are assumed cardinal numbers, not decimal fractions
\(5.10 is a higher release than 5.9\).  Omitted fields are considered
lower \(5.6.7 is earlier than 5.6.7.1\).  Comparison runs till the end
of the string is found, or a non-numeric component shows up \(5.6.7 is
earlier than \"5.6.7 beta\", which is probably not what you want in
some cases\).  This code is suitable for existing RCS release numbers.
CVS releases are handled reasonably, too \(1.3 < 1.4* < 1.5\)."
  (let (v1 v2 i1 i2)
    (catch 'done
      (or (and (string-match "^\\.?\\([0-9]+\\)" r1)
	       (setq i1 (match-end 0))
	       (setq v1 (string-to-number (match-string 1 r1)))
	       (or (and (string-match "^\\.?\\([0-9]+\\)" r2)
			(setq i2 (match-end 0))
			(setq v2 (string-to-number (match-string 1 r2)))
			(if (> v1 v2) (throw 'done t)
			  (if (< v1 v2) (throw 'done nil)
			    (throw 'done
				   (vc-release-greater-or-equal
				    (substring r1 i1)
				    (substring r2 i2)))))))
		   (throw 'done t)))
	  (or (and (string-match "^\\.?\\([0-9]+\\)" r2)
		   (throw 'done nil))
	      (throw 'done t)))))

(defun vc-rcs-release-p (release)
  "Return t if we have RELEASE or better."
  (let ((installation (vc-rcs-system-release)))
    (if (and installation
	     (not (eq installation 'unknown)))
	(vc-release-greater-or-equal installation release))))

(defun vc-rcs-system-release ()
  "Return the RCS release installed on this system, as a string.
Return symbol `unknown' if the release cannot be deducted.  The user can
override this using variable `vc-rcs-release'.

If the user has not set variable `vc-rcs-release' and it is nil,
variable `vc-rcs-release' is set to the returned value."
  (or vc-rcs-release
      (setq vc-rcs-release
	    (or (and (zerop (vc-do-command "*vc*" nil "rcs" nil "-V"))
		     (with-current-buffer (get-buffer "*vc*")
		       (vc-parse-buffer "^RCS version \\([0-9.]+ *.*\\)" 1)))
		'unknown))))

(defun vc-rcs-set-non-strict-locking (file)
  (vc-do-command "*vc*" 0 "rcs" file "-U")
  (vc-file-setprop file 'vc-checkout-model 'implicit)
  (set-file-modes file (logior (file-modes file) 128)))

(defun vc-rcs-set-default-branch (file branch)
  (vc-do-command "*vc*" 0 "rcs" (vc-name file) (concat "-b" branch))
  (vc-file-setprop file 'vc-rcs-default-branch branch))

(defun vc-rcs-parse (&optional buffer)
  "Parse current buffer, presumed to be in RCS-style masterfile format.
Optional arg BUFFER specifies another buffer to parse.  Return an alist
of two elements, w/ keys `headers' and `revisions' and values in turn
sub-alists.  For `headers', the values unless otherwise specified are
strings and the keys are:

  desc     -- description
  head     -- latest revision
  branch   -- the branch the \"head revision\" lies on;
              absent if the head revision lies on the trunk
  access   -- ???
  symbols  -- sub-alist of (SYMBOL . REVISION) elements
  locks    -- if file is checked out, something like \"ttn:1.7\"
  strict   -- t if \"strict locking\" is in effect, otherwise nil
  comment  -- may be absent; typically something like \"# \" or \"; \"
  expand   -- may be absent; ???

For `revisions', the car is REVISION (string), the cdr a sub-alist,
with string values (unless otherwise specified) and keys:

  date     -- a time value (like that returned by `encode-time'); as a
              special case, a year value less than 100 is augmented by 1900
  author   -- username
  state    -- typically \"Exp\" or \"Rel\"
  branches -- list of revisions that begin branches from this revision
  next     -- on the trunk: the chronologically-preceding revision, or \"\";
              on a branch: the chronologically-following revision, or \"\"
  log      -- change log entry
  text     -- for the head revision on the trunk, the body of the file;
              other revisions have `:insn' instead
  :insn    -- for non-head revisions, a list of parsed instructions
              in one of two forms, in both cases START meaning \"first
              go to line START\":
               - `(START k COUNT)' -- kill COUNT lines
               - `(START i TEXT)'  -- insert TEXT (a string)
              The list is in descending order by START.

The `:insn' key is a keyword to distinguish it as a vc-rcs.el extension."
  (setq buffer (get-buffer (or buffer (current-buffer))))
  (set-buffer buffer)
  ;; An RCS masterfile can be viewed as containing four regular (for the
  ;; most part) sections: (a) the "headers", (b) the "rev headers", (c)
  ;; the "description" and (d) the "rev bodies", in that order.  In the
  ;; returned alist (see docstring), elements from (b) and (d) are
  ;; combined pairwise to form the "revisions", while those from (a) and
  ;; (c) are simply combined to form the "headers".
  ;;
  ;; Loosely speaking, each section contains a series of alternating
  ;; "tags" and "printed representations".  In the (b) and (d), many
  ;; such series can appear, and a revision number on a line by itself
  ;; precedes the series of tags and printed representations associated
  ;; with it.
  ;;
  ;; In (a) and (b), the printed representations (with the exception of
  ;; the `comment' tag in the headers) terminate with a semicolon, which
  ;; is NOT part of the "value" finally associated with the tag.  All
  ;; other printed representations are in "@@-format"; there is an "@",
  ;; the middle part (to be translated into the value), another "@" and
  ;; a newline.  Each "@@" in the middle part indicates the position of
  ;; a single "@" (and consequently the requirement of an additional
  ;; initial step when translating to the value).
  ;;
  ;; Parser state includes vars that collect parts of the return value...
  (let ((desc nil) (headers nil) (revs nil)
        ;; ... as well as vars that support a single-pass, tag-assisted,
        ;; minimal-data-copying scan.  Basically -- skirting around the
        ;; grouping by revision required in (b) and (d) -- we repeatedly
        ;; and context-sensitively read a tag (that MUST be present),
        ;; determine the bounds of the printed representation, translate
        ;; it into a value, and push the tag plus value onto one of the
        ;; collection vars.  Finally, we return the parse tree
        ;; incorporating the values of the collection vars (see "rv").
        ;;
        ;; A symbol or string to keep track of context (for error messages).
        context
        ;; A symbol, the current tag.
        tok
        ;; Region (begin and end buffer positions) of the printed
        ;; representation for the current tag.
        b e
        ;; A list of buffer positions where "@@" can be found within the
        ;; printed representation region.  For each location, we push two
        ;; elements onto the list, 1+ and 2+ the location, respectively,
        ;; with the 2+ appearing at the head.  In this way, the expression
        ;;   `(,e ,@@-holes ,b)
        ;; describes regions that can be concatenated (in reverse order)
        ;; to "de-@@-format" the printed representation as the first step
        ;; to translating it into some value.  See internal func `gather'.
        @-holes)
    (flet ((sw () (skip-chars-forward " \t\n")) ; i.e., `[:space:]'
           (at (tag) (save-excursion (eq tag (read buffer))))
           (to-eol () (buffer-substring-no-properties
                       (point) (progn (forward-line 1)
                                      (1- (point)))))
           (to-semi () (setq b (point)
                             e (progn (search-forward ";")
                                      (1- (point)))))
           (to-one@ () (setq @-holes nil
                             b (progn (search-forward "@") (point))
                             e (progn (while (and (search-forward "@")
                                                  (= ?@ (char-after))
                                                  (progn
                                                    (push (point) @-holes)
                                                    (forward-char 1)
                                                    (push (point) @-holes))))
                                      (1- (point)))))
           (tok+val (set-b+e name &optional proc)
                    (unless (eq name (setq tok (read buffer)))
                      (error "Missing `%s' while parsing %s" name context))
                    (sw)
                    (funcall set-b+e)
                    (cons tok (if proc
                                  (funcall proc)
                                (buffer-substring-no-properties b e))))
           (k-semi (name &optional proc) (tok+val 'to-semi name proc))
           (gather () (let ((pairs `(,e ,@@-holes ,b))
                            acc)
                        (while pairs
                          (push (buffer-substring-no-properties
                                 (cadr pairs) (car pairs))
                                acc)
                          (setq pairs (cddr pairs)))
                        (apply 'concat acc)))
           (k-one@ (name &optional later) (tok+val 'to-one@ name
                                                   (if later
                                                       (lambda () t)
                                                     'gather))))
      (save-excursion
        (goto-char (point-min))
        ;; headers
        (setq context 'headers)
        (flet ((hpush (name &optional proc)
                      (push (k-semi name proc) headers)))
          (hpush 'head)
          (when (at 'branch)
            (hpush 'branch))
          (hpush 'access)
          (hpush 'symbols
                 (lambda ()
                   (mapcar (lambda (together)
                             (let ((two (split-string together ":")))
                               (setcar two (intern (car two)))
                               (setcdr two (cadr two))
                               two))
                           (split-string
                            (buffer-substring-no-properties b e)))))
          (hpush 'locks))
        (push `(strict . ,(when (at 'strict)
                            (search-forward ";")
                            t))
              headers)
        (when (at 'comment)
          (push (k-one@ 'comment) headers)
          (search-forward ";"))
        (when (at 'expand)
          (push (k-one@ 'expand) headers)
          (search-forward ";"))
        (setq headers (nreverse headers))
        ;; rev headers
        (sw) (setq context 'rev-headers)
        (while (looking-at "[0-9]")
          (push `(,(to-eol)
                  ,(k-semi 'date
                           (lambda ()
                             (let ((ls (mapcar 'string-to-number
                                               (split-string
                                                (buffer-substring-no-properties
                                                 b e)
                                                "\\."))))
                               ;; Hack the year -- verified to be the
                               ;; same algorithm used in RCS 5.7.
                               (when (< (car ls) 100)
                                 (setcar ls (+ 1900 (car ls))))
                               (apply 'encode-time (nreverse ls)))))
                  ,@(mapcar 'k-semi '(author state))
                  ,(k-semi 'branches
                           (lambda ()
                             (split-string
                              (buffer-substring-no-properties b e))))
                  ,(k-semi 'next))
                revs)
          (sw))
        (setq revs (nreverse revs))
        ;; desc
        (sw) (setq context 'desc
                   desc (k-one@ 'desc))
        ;; rev bodies
        (let (acc
              ;; Element of `revs' that initially holds only header info.
              ;; "Pairwise combination" occurs when we add body info.
              rev
              ;; Components of the editing commands (aside from the actual
              ;; text) that comprise the `text' printed representations
              ;; (not including the "head" revision).
              cmd start act
              ;; Ascending (reversed) `@-holes' which the internal func
              ;; `incg' pops to effect incremental gathering.
              asc
              ;; Function to extract text (for the `a' command), either
              ;; `incg' or `buffer-substring-no-properties'.  (This is
              ;; for speed; strictly speaking, it is sufficient to use
              ;; only the former since it behaves identically to the
              ;; latter in the absence of "@@".)
              sub)
          (flet ((incg (beg end) (let ((b beg) (e end) @-holes)
                                   (while (and asc (< (car asc) e))
                                     (push (pop asc) @-holes))
                                   ;; Self-deprecate when work is done.
                                   ;; Folding many dimensions into one.
                                   ;; Thanks B.Mandelbrot, for complex sum.
                                   ;; O beauteous math! --the Unvexed Bum
                                   (unless asc
                                     (setq sub 'buffer-substring-no-properties))
                                   (gather))))
            (while (and (sw)
                        (not (eobp))
                        (setq context (to-eol)
                              rev (or (assoc context revs)
                                      (error "Rev `%s' has body but no head"
                                             context))))
              (push (k-one@ 'log) (cdr rev))
              ;; For rev body `text' tags, delay translation slightly...
              (push (k-one@ 'text t) (cdr rev))
              ;; ... until we decide which tag and value is appropriate to
              ;; collect.  For the "head" revision, compute the value of the
              ;; `text' printed representation by simple `gather'.  For all
              ;; other revisions, replace the `text' tag+value with `:insn'
              ;; plus value, always scanning in-place.
              (if (string= context (cdr (assq 'head headers)))
                  (setcdr (cadr rev) (gather))
                (if @-holes
                    (setq asc (nreverse @-holes)
                          sub 'incg)
                  (setq sub 'buffer-substring-no-properties))
                (goto-char b)
                (setq acc nil)
                (while (< (point) e)
                  (forward-char 1)
                  (setq cmd (char-before)
                        start (read (current-buffer))
                        act (read (current-buffer)))
                  (forward-char 1)
                  (push (case cmd
                          (?d
                           ;; `d' means "delete lines".
                           ;; For Emacs spirit, we use `k' for "kill".
                           `(,start k ,act))
                          (?a
                           ;; `a' means "append after this line" but
                           ;; internally we normalize it so that START
                           ;; specifies the actual line for insert, thus
                           ;; requiring less hair in the realization algs.
                           ;; For Emacs spirit, we use `i' for "insert".
                           `(,(1+ start) i
                             ,(funcall sub (point) (progn (forward-line act)
                                                          (point)))))
                          (t (error "Bad command `%c' in `text' for rev `%s'"
                                    cmd context)))
                        acc))
                (goto-char (1+ e))
                (setcar (cdr rev) (cons :insn acc)))))))
      ;; rv
      `((headers ,desc ,@headers)
        (revisions ,@revs)))))

(provide 'vc-rcs)

;;; vc-rcs.el ends here
