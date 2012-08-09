;;; vc-svn.el --- non-resident support for Subversion version-control

;; Copyright (C) 2003-2012  Free Software Foundation, Inc.

;; Author:      FSF (see vc.el for full credits)
;; Maintainer:  Stefan Monnier <monnier@gnu.org>
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

;; Sync'd with Subversion's vc-svn.el as of revision 5801. but this version
;; has been extensively modified since to handle filesets.

;;; Code:

(eval-when-compile
  (require 'vc))

;; Clear up the cache to force vc-call to check again and discover
;; new functions when we reload this file.
(put 'SVN 'vc-functions nil)

;;;
;;; Customization options
;;;

(defgroup vc-svn nil
  "VC Subversion (svn) backend."
  :version "24.1"
  :group 'vc)

;; FIXME there is also svnadmin.
(defcustom vc-svn-program "svn"
  "Name of the SVN executable."
  :type 'string
  :group 'vc-svn)

(defcustom vc-svn-global-switches nil
  "Global switches to pass to any SVN command."
  :type '(choice (const :tag "None" nil)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List"
			 :value ("")
			 string))
  :version "22.1"
  :group 'vc-svn)

(defcustom vc-svn-register-switches nil
  "Switches for registering a file into SVN.
A string or list of strings passed to the checkin program by
\\[vc-register].  If nil, use the value of `vc-register-switches'.
If t, use no switches."
  :type '(choice (const :tag "Unspecified" nil)
		 (const :tag "None" t)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List" :value ("") string))
  :version "22.1"
  :group 'vc-svn)

(defcustom vc-svn-diff-switches
  t			   ;`svn' doesn't support common args like -c or -b.
  "String or list of strings specifying extra switches for svn diff under VC.
If nil, use the value of `vc-diff-switches' (or `diff-switches'),
together with \"-x --diff-cmd=\"`diff-command' (since 'svn diff'
does not support the default \"-c\" value of `diff-switches').
If you want to force an empty list of arguments, use t."
  :type '(choice (const :tag "Unspecified" nil)
		 (const :tag "None" t)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List"
			 :value ("")
			 string))
  :version "22.1"
  :group 'vc-svn)

(defcustom vc-svn-header '("\$Id\$")
  "Header keywords to be inserted by `vc-insert-headers'."
  :version "24.1"     ; no longer consult the obsolete vc-header-alist
  :type '(repeat string)
  :group 'vc-svn)

;; We want to autoload it for use by the autoloaded version of
;; vc-svn-registered, but we want the value to be compiled at startup, not
;; at dump time.
;; ;;;###autoload
(defconst vc-svn-admin-directory
  (cond ((and (memq system-type '(cygwin windows-nt ms-dos))
	      (getenv "SVN_ASP_DOT_NET_HACK"))
	 "_svn")
	(t ".svn"))
  "The name of the \".svn\" subdirectory or its equivalent.")

;;; Properties of the backend

(defun vc-svn-revision-granularity () 'repository)
(defun vc-svn-checkout-model (files) 'implicit)

;;;
;;; State-querying functions
;;;

;;; vc-svn-admin-directory is generally not defined when the
;;; autoloaded function is called.

;;;###autoload (defun vc-svn-registered (f)
;;;###autoload   (let ((admin-dir (cond ((and (eq system-type 'windows-nt)
;;;###autoload                                (getenv "SVN_ASP_DOT_NET_HACK"))
;;;###autoload                           "_svn")
;;;###autoload                          (t ".svn"))))
;;;###autoload     (when (vc-find-root f admin-dir)
;;;###autoload       (load "vc-svn")
;;;###autoload       (vc-svn-registered f))))

(defun vc-svn-registered (file)
  "Check if FILE is SVN registered."
  (when (vc-svn-root file)
    (with-temp-buffer
      (cd (file-name-directory file))
      (let* (process-file-side-effects
	     (status
             (condition-case nil
                 ;; Ignore all errors.
                 (vc-svn-command t t file "status" "-v")
               ;; Some problem happened.  E.g. We can't find an `svn'
               ;; executable.  We used to only catch `file-error' but when
               ;; the process is run on a remote host via Tramp, the error
               ;; is only reported via the exit status which is turned into
               ;; an `error' by vc-do-command.
               (error nil))))
        (when (eq 0 status)
	  (let ((parsed (vc-svn-parse-status file)))
	    (and parsed (not (memq parsed '(ignored unregistered))))))))))

(defun vc-svn-state (file &optional localp)
  "SVN-specific version of `vc-state'."
  (let (process-file-side-effects)
    (setq localp (or localp (vc-stay-local-p file 'SVN)))
    (with-temp-buffer
      (cd (file-name-directory file))
      (vc-svn-command t 0 file "status" (if localp "-v" "-u"))
      (vc-svn-parse-status file))))

(defun vc-svn-state-heuristic (file)
  "SVN-specific state heuristic."
  (vc-svn-state file 'local))

;; FIXME it would be better not to have the "remote" argument,
;; but to distinguish the two output formats based on content.
(defun vc-svn-after-dir-status (callback &optional remote)
  (let ((state-map '((?A . added)
                     (?C . conflict)
                     (?I . ignored)
                     (?M . edited)
                     (?D . removed)
                     (?R . removed)
                     (?? . unregistered)
                     ;; This is what vc-svn-parse-status does.
                     (?~ . edited)))
	(re (if remote "^\\(.\\)\\(.\\).....? \\([ *]\\) +\\(?:[-0-9]+\\)?   \\(.*\\)$"
	      ;; Subexp 3 is a dummy in this case, so the numbers match.
	      "^\\(.\\)\\(.\\)...\\(.\\) \\(.*\\)$"))
       result)
    (goto-char (point-min))
    (while (re-search-forward re nil t)
      (let ((state (cdr (assq (aref (match-string 1) 0) state-map)))
            (propstat (cdr (assq (aref (match-string 2) 0) state-map)))
            (filename (if (memq system-type '(windows-nt ms-dos))
                          (replace-regexp-in-string "\\\\" "/" (match-string 4))
                        (match-string 4))))
        (and (memq propstat '(conflict edited))
             (not (eq state 'conflict)) ; conflict always wins
             (setq state propstat))
	(and remote (string-equal (match-string 3) "*")
	     ;; FIXME are there other possible combinations?
	     (cond ((eq state 'edited) (setq state 'needs-merge))
		   ((not state) (setq state 'needs-update))))
	(when (and state (not (string= "." filename)))
         (setq result (cons (list filename state) result)))))
    (funcall callback result)))

(defun vc-svn-dir-status (dir callback)
  "Run 'svn status' for DIR and update BUFFER via CALLBACK.
CALLBACK is called as (CALLBACK RESULT BUFFER), where
RESULT is a list of conses (FILE . STATE) for directory DIR."
  ;; FIXME should this rather be all the files in dir?
  ;; FIXME: the vc-stay-local-p logic below is disabled, it ends up
  ;; calling synchronously (vc-svn-registered DIR) => calling svn status -v DIR
  ;; which is VERY SLOW for big trees and it makes emacs
  ;; completely unresponsive during that time.
  (let* ((local (and nil (vc-stay-local-p dir 'SVN)))
	 (remote (or t (not local) (eq local 'only-file))))
    (vc-svn-command (current-buffer) 'async nil "status"
		    (if remote "-u"))
  (vc-exec-after
     `(vc-svn-after-dir-status (quote ,callback) ,remote))))

(defun vc-svn-dir-status-files (dir files default-state callback)
  (apply 'vc-svn-command (current-buffer) 'async nil "status" files)
  (vc-exec-after
   `(vc-svn-after-dir-status (quote ,callback))))

(defun vc-svn-dir-extra-headers (dir)
  "Generate extra status headers for a Subversion working copy."
  (let (process-file-side-effects)
    (vc-svn-command "*vc*" 0 nil "info"))
  (let ((repo
	 (save-excursion
	   (and (progn
		  (set-buffer "*vc*")
		  (goto-char (point-min))
		  (re-search-forward "Repository Root: *\\(.*\\)" nil t))
		(match-string 1)))))
    (concat
     (cond (repo
	    (concat
	     (propertize "Repository : " 'face 'font-lock-type-face)
	     (propertize repo 'face 'font-lock-variable-name-face)))
	   (t "")))))

(defun vc-svn-working-revision (file)
  "SVN-specific version of `vc-working-revision'."
  ;; There is no need to consult RCS headers under SVN, because we
  ;; get the workfile version for free when we recognize that a file
  ;; is registered in SVN.
  (vc-svn-registered file)
  (vc-file-getprop file 'vc-working-revision))

;; vc-svn-mode-line-string doesn't exist because the default implementation
;; works just fine.

(defun vc-svn-previous-revision (file rev)
  (let ((newrev (1- (string-to-number rev))))
    (when (< 0 newrev)
      (number-to-string newrev))))

(defun vc-svn-next-revision (file rev)
  (let ((newrev (1+ (string-to-number rev))))
    ;; The "working revision" is an uneasy conceptual fit under Subversion;
    ;; we use it as the upper bound until a better idea comes along.  If the
    ;; workfile version W coincides with the tree's latest revision R, then
    ;; this check prevents a "no such revision: R+1" error.  Otherwise, it
    ;; inhibits showing of W+1 through R, which could be considered anywhere
    ;; from gracious to impolite.
    (unless (< (string-to-number (vc-file-getprop file 'vc-working-revision))
               newrev)
      (number-to-string newrev))))


;;;
;;; State-changing functions
;;;

(defun vc-svn-create-repo ()
  "Create a new SVN repository."
  (vc-do-command "*vc*" 0 "svnadmin" '("create" "SVN"))
  (vc-svn-command "*vc*" 0 "." "checkout"
                  (concat "file://" default-directory "SVN")))

(defun vc-svn-register (files &optional rev comment)
  "Register FILES into the SVN version-control system.
The COMMENT argument is ignored  This does an add but not a commit.
Passes either `vc-svn-register-switches' or `vc-register-switches'
to the SVN command."
  (apply 'vc-svn-command nil 0 files "add" (vc-switches 'SVN 'register)))

(defun vc-svn-root (file)
  (vc-find-root file vc-svn-admin-directory))

(defalias 'vc-svn-responsible-p 'vc-svn-root)

(defalias 'vc-svn-could-register 'vc-svn-root
  "Return non-nil if FILE could be registered in SVN.
This is only possible if SVN is responsible for FILE's directory.")

(defun vc-svn-checkin (files rev comment &optional extra-args-ignored)
  "SVN-specific version of `vc-backend-checkin'."
  (if rev (error "Committing to a specific revision is unsupported in SVN"))
  (let ((status (apply
                 'vc-svn-command nil 1 files "ci"
                 (nconc (list "-m" comment) (vc-switches 'SVN 'checkin)))))
    (set-buffer "*vc*")
    (goto-char (point-min))
    (unless (equal status 0)
      ;; Check checkin problem.
      (cond
       ((search-forward "Transaction is out of date" nil t)
        (mapc (lambda (file) (vc-file-setprop file 'vc-state 'needs-merge))
	      files)
        (error (substitute-command-keys
                (concat "Up-to-date check failed: "
                        "type \\[vc-next-action] to merge in changes"))))
       (t
        (pop-to-buffer (current-buffer))
        (goto-char (point-min))
        (shrink-window-if-larger-than-buffer)
        (error "Check-in failed"))))
    ;; Update file properties
    ;; (vc-file-setprop
    ;;  file 'vc-working-revision
    ;;  (vc-parse-buffer "^\\(new\\|initial\\) revision: \\([0-9.]+\\)" 2))
    ))

(defun vc-svn-find-revision (file rev buffer)
  "SVN-specific retrieval of a specified version into a buffer."
  (let (process-file-side-effects)
    (apply 'vc-svn-command
	   buffer 0 file
	   "cat"
	   (and rev (not (string= rev ""))
		(concat "-r" rev))
	   (vc-switches 'SVN 'checkout))))

(defun vc-svn-checkout (file &optional editable rev)
  (message "Checking out %s..." file)
  (with-current-buffer (or (get-file-buffer file) (current-buffer))
    (vc-svn-update file editable rev (vc-switches 'SVN 'checkout)))
  (vc-mode-line file 'SVN)
  (message "Checking out %s...done" file))

(defun vc-svn-update (file editable rev switches)
  (if (and (file-exists-p file) (not rev))
      ;; If no revision was specified, there's nothing to do.
      nil
    ;; Check out a particular version (or recreate the file).
    (vc-file-setprop file 'vc-working-revision nil)
    (apply 'vc-svn-command nil 0 file
	   "update"
	   (cond
	    ((null rev) "-rBASE")
	    ((or (eq rev t) (equal rev "")) nil)
	    (t (concat "-r" rev)))
	   switches)))

(defun vc-svn-delete-file (file)
  (vc-svn-command nil 0 file "remove"))

(defun vc-svn-rename-file (old new)
  (vc-svn-command nil 0 new "move" (file-relative-name old)))

(defun vc-svn-revert (file &optional contents-done)
  "Revert FILE to the version it was based on."
  (unless contents-done
    (vc-svn-command nil 0 file "revert")))

(defun vc-svn-merge (file first-version &optional second-version)
  "Merge changes into current working copy of FILE.
The changes are between FIRST-VERSION and SECOND-VERSION."
  (vc-svn-command nil 0 file
                 "merge"
		 "-r" (if second-version
			(concat first-version ":" second-version)
		      first-version))
  (vc-file-setprop file 'vc-state 'edited)
  (with-current-buffer (get-buffer "*vc*")
    (goto-char (point-min))
    (if (looking-at "C  ")
        1				; signal conflict
      0)))				; signal success

(defun vc-svn-merge-news (file)
  "Merge in any new changes made to FILE."
  (message "Merging changes into %s..." file)
  ;; (vc-file-setprop file 'vc-working-revision nil)
  (vc-file-setprop file 'vc-checkout-time 0)
  (vc-svn-command nil 0 file "update")
  ;; Analyze the merge result reported by SVN, and set
  ;; file properties accordingly.
  (with-current-buffer (get-buffer "*vc*")
    (goto-char (point-min))
    ;; get new working revision
    (if (re-search-forward
	 "^\\(Updated to\\|At\\) revision \\([0-9]+\\)" nil t)
	(vc-file-setprop file 'vc-working-revision (match-string 2))
      (vc-file-setprop file 'vc-working-revision nil))
    ;; get file status
    (goto-char (point-min))
    (prog1
        (if (looking-at "At revision")
            0 ;; there were no news; indicate success
          (if (re-search-forward
               ;; Newer SVN clients have 3 columns of chars (one for the
               ;; file's contents, then second for its properties, and the
               ;; third for lock-grabbing info), before the 2 spaces.
               ;; We also used to match the filename in column 0 without any
               ;; meta-info before it, but I believe this can never happen.
               (concat "^\\(\\([ACGDU]\\)\\(.[B ]\\)?  \\)"
                       (regexp-quote (file-name-nondirectory file)))
               nil t)
              (cond
               ;; Merge successful, we are in sync with repository now
               ((string= (match-string 2) "U")
                (vc-file-setprop file 'vc-state 'up-to-date)
                (vc-file-setprop file 'vc-checkout-time
                                 (nth 5 (file-attributes file)))
                0);; indicate success to the caller
               ;; Merge successful, but our own changes are still in the file
               ((string= (match-string 2) "G")
                (vc-file-setprop file 'vc-state 'edited)
                0);; indicate success to the caller
               ;; Conflicts detected!
               (t
                (vc-file-setprop file 'vc-state 'edited)
                1);; signal the error to the caller
               )
            (pop-to-buffer "*vc*")
            (error "Couldn't analyze svn update result")))
      (message "Merging changes into %s...done" file))))

(defun vc-svn-modify-change-comment (files rev comment)
  "Modify the change comments for a specified REV.
You must have ssh access to the repository host, and the directory Emacs
uses locally for temp files must also be writable by you on that host.
This is only supported if the repository access method is either file://
or svn+ssh://."
  (let (tempfile host remotefile directory fileurl-p)
    (with-temp-buffer
      (vc-svn-command (current-buffer) 0 nil "info")
      (goto-char (point-min))
      (unless (re-search-forward "Repository Root: \\(file://\\(/.*\\)\\)\\|\\(svn\\+ssh://\\([^/]+\\)\\(/.*\\)\\)" nil t)
	(error "Repository information is unavailable"))
      (if (match-string 1)
	  (progn
	    (setq fileurl-p t)
	    (setq directory (match-string 2)))
	(setq host (match-string 4))
	(setq directory (match-string 5))
	(setq remotefile (concat host ":" tempfile))))
    (with-temp-file (setq tempfile (make-temp-file user-mail-address))
      (insert comment))
    (if fileurl-p
	;; Repository Root is a local file.
	(progn
	  (unless (vc-do-command
		   "*vc*" 0 "svnadmin" nil
		   "setlog" "--bypass-hooks" directory
		   "-r" rev (format "%s" tempfile))
	    (error "Log edit failed"))
	  (delete-file tempfile))

      ;; Remote repository, using svn+ssh.
      (unless (vc-do-command "*vc*" 0 "scp" nil "-q" tempfile remotefile)
	(error "Copy of comment to %s failed" remotefile))
      (unless (vc-do-command
	       "*vc*" 0 "ssh" nil "-q" host
	       (format "svnadmin setlog --bypass-hooks %s -r %s %s; rm %s"
		       directory rev tempfile tempfile))
	(error "Log edit failed")))))

;;;
;;; History functions
;;;

(defvar log-view-per-file-logs)

(define-derived-mode vc-svn-log-view-mode log-view-mode "SVN-Log-View"
  (require 'add-log)
  (set (make-local-variable 'log-view-per-file-logs) nil))

(defun vc-svn-print-log (files buffer &optional shortlog start-revision limit)
  "Get change log(s) associated with FILES."
  (save-current-buffer
    (vc-setup-buffer buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (if files
	  (dolist (file files)
		  (insert "Working file: " file "\n")
		  (apply
		   'vc-svn-command
		   buffer
		   'async
		   ;; (if (and (= (length files) 1) (vc-stay-local-p file 'SVN)) 'async 0)
		   (list file)
		   "log"
		   (append
		    (list
		     (if start-revision
			 (format "-r%s" start-revision)
		       ;; By default Subversion only shows the log up to the
		       ;; working revision, whereas we also want the log of the
		       ;; subsequent commits.  At least that's what the
		       ;; vc-cvs.el code does.
		       "-rHEAD:0"))
		    (when limit (list "--limit" (format "%s" limit))))))
	;; Dump log for the entire directory.
	(apply 'vc-svn-command buffer 0 nil "log"
	       (append
		(list
		 (if start-revision (format "-r%s" start-revision) "-rHEAD:0"))
		(when limit (list "--limit" (format "%s" limit)))))))))

(defun vc-svn-diff (files &optional oldvers newvers buffer)
  "Get a difference report using SVN between two revisions of fileset FILES."
  (and oldvers
       (not newvers)
       files
       (catch 'no
	 (dolist (f files)
	   (or (equal oldvers (vc-working-revision f))
	       (throw 'no nil)))
	 t)
       ;; Use nil rather than the current revision because svn handles
       ;; it better (i.e. locally).  Note that if _any_ of the files
       ;; has a different revision, we fetch the lot, which is
       ;; obviously sub-optimal.
       (setq oldvers nil))
  (let* ((switches
	    (if vc-svn-diff-switches
		(vc-switches 'SVN 'diff)
	      (list (concat "--diff-cmd=" diff-command) "-x"
		    (mapconcat 'identity (vc-switches nil 'diff) " "))))
	   (async (and (not vc-disable-async-diff)
                       (vc-stay-local-p files 'SVN)
		       (or oldvers newvers)))) ; Svn diffs those locally.
      (apply 'vc-svn-command buffer
	     (if async 'async 0)
	     files "diff"
	     (append
	      switches
	      (when oldvers
		(list "-r" (if newvers (concat oldvers ":" newvers)
			     oldvers)))))
      (if async 1		      ; async diff => pessimistic assumption
	;; For some reason `svn diff' does not return a useful
	;; status w.r.t whether the diff was empty or not.
	(buffer-size (get-buffer buffer)))))

;;;
;;; Tag system
;;;

(defun vc-svn-create-tag (dir name branchp)
  "Assign to DIR's current revision a given NAME.
If BRANCHP is non-nil, the name is created as a branch (and the current
workspace is immediately moved to that new branch).
NAME is assumed to be a URL."
  (vc-svn-command nil 0 dir "copy" name)
  (when branchp (vc-svn-retrieve-tag dir name nil)))

(defun vc-svn-retrieve-tag (dir name update)
  "Retrieve a tag at and below DIR.
NAME is the name of the tag; if it is empty, do a `svn update'.
If UPDATE is non-nil, then update (resynch) any affected buffers.
NAME is assumed to be a URL."
  (vc-svn-command nil 0 dir "switch" name)
  ;; FIXME: parse the output and obey `update'.
  )

;;;
;;; Miscellaneous
;;;

;; Subversion makes backups for us, so don't bother.
;; (defun vc-svn-make-version-backups-p (file)
;;   "Return non-nil if version backups should be made for FILE."
;;  (vc-stay-local-p file 'SVN))

(defun vc-svn-check-headers ()
  "Check if the current file has any headers in it."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "\\$[A-Za-z\300-\326\330-\366\370-\377]+\
\\(: [\t -#%-\176\240-\377]*\\)?\\$" nil t)))


;;;
;;; Internal functions
;;;

(defun vc-svn-command (buffer okstatus file-or-list &rest flags)
  "A wrapper around `vc-do-command' for use in vc-svn.el.
The difference to vc-do-command is that this function always invokes `svn',
and that it passes \"--non-interactive\" and `vc-svn-global-switches' to
it before FLAGS."
  ;; Might be nice if svn defaulted to non-interactive if stdin not tty.
  ;; http://svn.haxx.se/dev/archive-2008-05/0762.shtml
  ;; http://svn.haxx.se/dev/archive-2009-04/0094.shtml
  ;; Maybe newer ones do?
  (or (member "--non-interactive"
              (setq flags (if (stringp vc-svn-global-switches)
             (cons vc-svn-global-switches flags)
                            (append vc-svn-global-switches flags))))
      (setq flags (cons "--non-interactive" flags)))
  (apply 'vc-do-command (or buffer "*vc*") okstatus vc-svn-program file-or-list
         flags))

(defun vc-svn-repository-hostname (dirname)
  (with-temp-buffer
    (let (process-file-side-effects)
      (vc-svn-command t t dirname "info" "--xml"))
    (goto-char (point-min))
    (when (re-search-forward "<url>\\(.*\\)</url>" nil t)
      ;; This is not a hostname but a URL.  This may actually be considered
      ;; as a feature since it allows vc-svn-stay-local to specify different
      ;; behavior for different modules on the same server.
      (match-string 1))))

(defun vc-svn-resolve-when-done ()
  "Call \"svn resolved\" if the conflict markers have been removed."
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "^<<<<<<< " nil t)
      (vc-svn-command nil 0 buffer-file-name "resolved")
      ;; Remove the hook so that it is not called multiple times.
      (remove-hook 'after-save-hook 'vc-svn-resolve-when-done t))))

;; Inspired by vc-arch-find-file-hook.
(defun vc-svn-find-file-hook ()
  (when (eq ?C (vc-file-getprop buffer-file-name 'vc-svn-status))
    ;; If the file is marked as "conflicted", then we should try and call
    ;; "svn resolved" when applicable.
    (if (save-excursion
          (goto-char (point-min))
          (re-search-forward "^<<<<<<< " nil t))
        ;; There are conflict markers.
        (progn
          (smerge-start-session)
          (add-hook 'after-save-hook 'vc-svn-resolve-when-done nil t))
      ;; There are no conflict markers.  This is problematic: maybe it means
      ;; the conflict has been resolved and we should immediately call "svn
      ;; resolved", or it means that the file's type does not allow Svn to
      ;; use conflict markers in which case we don't really know what to do.
      ;; So let's just punt for now.
      nil)
    (message "There are unresolved conflicts in this file")))

(defun vc-svn-parse-status (&optional filename)
  "Parse output of \"svn status\" command in the current buffer.
Set file properties accordingly.  Unless FILENAME is non-nil, parse only
information about FILENAME and return its status."
  (let (file status propstat)
    (goto-char (point-min))
    (while (re-search-forward
            ;; Ignore the files with status X.
	    "^\\(?:\\?\\|[ ACDGIMR!~][ MC][ L][ +][ S]..\\([ *]\\) +\\([-0-9]+\\) +\\([0-9?]+\\) +\\([^ ]+\\)\\) +" nil t)
      ;; If the username contains spaces, the output format is ambiguous,
      ;; so don't trust the output's filename unless we have to.
      (setq file (or filename
                     (expand-file-name
                      (buffer-substring (point) (line-end-position)))))
      (setq status (char-after (line-beginning-position))
            ;; Status of the item's properties ([ MC]).
            propstat (char-after (1+ (line-beginning-position))))
      (if (eq status ??)
	  (vc-file-setprop file 'vc-state 'unregistered)
	;; Use the last-modified revision, so that searching in vc-print-log
	;; output works.
	(vc-file-setprop file 'vc-working-revision (match-string 3))
        ;; Remember Svn's own status.
        (vc-file-setprop file 'vc-svn-status status)
	(vc-file-setprop
	 file 'vc-state
	 (cond
	  ((and (eq status ?\ ) (eq propstat ?\ ))
	   (if (eq (char-after (match-beginning 1)) ?*)
	       'needs-update
             (vc-file-setprop file 'vc-checkout-time
                              (nth 5 (file-attributes file)))
	     'up-to-date))
	  ((eq status ?A)
	   ;; If the file was actually copied, (match-string 2) is "-".
	   (vc-file-setprop file 'vc-working-revision "0")
	   (vc-file-setprop file 'vc-checkout-time 0)
	   'added)
	  ;; Conflict in contents or properties.
	  ((or (eq status ?C) (eq propstat ?C))
	   (vc-file-setprop file 'vc-state 'conflict))
	  ;; Modified contents or properties.
	  ((or (eq status ?M) (eq propstat ?M))
	   (if (eq (char-after (match-beginning 1)) ?*)
	       'needs-merge
	     'edited))
	  ((eq status ?I)
	   (vc-file-setprop file 'vc-state 'ignored))
	  ((memq status '(?D ?R))
	   (vc-file-setprop file 'vc-state 'removed))
	  (t 'edited)))))
    (when filename (vc-file-getprop filename 'vc-state))))

(defun vc-svn-valid-symbolic-tag-name-p (tag)
  "Return non-nil if TAG is a valid symbolic tag name."
  ;; According to the SVN manual, a valid symbolic tag must start with
  ;; an uppercase or lowercase letter and can contain uppercase and
  ;; lowercase letters, digits, `-', and `_'.
  (and (string-match "^[a-zA-Z]" tag)
       (not (string-match "[^a-z0-9A-Z-_]" tag))))

(defun vc-svn-valid-revision-number-p (tag)
  "Return non-nil if TAG is a valid revision number."
  (and (string-match "^[0-9]" tag)
       (not (string-match "[^0-9]" tag))))

;; Support for `svn annotate'

(defun vc-svn-annotate-command (file buf &optional rev)
  (vc-svn-command buf 'async file "annotate" (if rev (concat "-r" rev))))

(defun vc-svn-annotate-time-of-rev (rev)
  ;; Arbitrarily assume 10 commits per day.
  (/ (string-to-number rev) 10.0))

(defvar vc-annotate-parent-rev)

(defun vc-svn-annotate-current-time ()
  (vc-svn-annotate-time-of-rev vc-annotate-parent-rev))

(defconst vc-svn-annotate-re "[ \t]*\\([0-9]+\\)[ \t]+[^\t ]+ ")

(defun vc-svn-annotate-time ()
  (when (looking-at vc-svn-annotate-re)
    (goto-char (match-end 0))
    (vc-svn-annotate-time-of-rev (match-string 1))))

(defun vc-svn-annotate-extract-revision-at-line ()
  (save-excursion
    (beginning-of-line)
    (if (looking-at vc-svn-annotate-re) (match-string 1))))

(defun vc-svn-revision-table (files)
  (let ((vc-svn-revisions '()))
    (with-current-buffer "*vc*"
      (vc-svn-command nil 0 files "log" "-q")
      (goto-char (point-min))
      (forward-line)
      (let ((start (point-min))
            (loglines (buffer-substring-no-properties (point-min)
                                                      (point-max))))
        (while (string-match "^r\\([0-9]+\\) " loglines)
          (push (match-string 1 loglines) vc-svn-revisions)
          (setq start (+ start (match-end 0)))
          (setq loglines (buffer-substring-no-properties start (point-max)))))
    vc-svn-revisions)))

(provide 'vc-svn)

;;; vc-svn.el ends here
