;;; vc-mcvs.el --- VC backend for the Meta-CVS version-control system

;; Copyright (C) 2003-2012  Free Software Foundation, Inc.

;; Author:      FSF (see vc.el for full credits)
;; Maintainer:  None
;; Obsolete-since: 23.1

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

;; ********** READ THIS! **********
;;
;; This file apparently does not work with the new (as of Emacs 23)
;; VC code.  Use at your own risk.  Please contact emacs-devel if you
;; can maintain this file and update it to work correctly.
;;
;; ********** READ THIS! **********

;; The home page of the Meta-CVS version control system is at
;;
;;      http://users.footprints.net/~kaz/mcvs.html
;;
;; This is derived from vc-cvs.el as follows:
;; - cp vc-cvs.el vc-mcvs.el
;; - Replace CVS/ with MCVS/CVS/
;; - Replace 'CVS with 'MCVS
;; - Replace -cvs- with -mcvs-
;; - Replace most of the rest of CVS to Meta-CVS
;;
;; Then of course started the hacking.  Only a small part of the code
;; has been touched and not much more than that was tested, so if
;; you bump into a bug, don't be surprised: just report it to me.
;;
;; What has been partly tested:
;; - C-x v v to start editing a file that was checked out with CVSREAD on.
;; - C-x v v to commit a file
;; - C-x v =
;; - C-x v l
;; - C-x v i
;; - C-x v g
;; - M-x vc-rename-file RET

;;; Bugs:

;; - Retrieving tags doesn't filter `cvs update' output and thus
;;   parses bogus filenames.  Don't know if it harms.

;;; Code:

(eval-when-compile (require 'vc))
(require 'vc-cvs)

;;;
;;; Customization options
;;;

(defcustom vc-mcvs-global-switches nil
  "Global switches to pass to any Meta-CVS command."
  :type '(choice (const :tag "None" nil)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List" :value ("") string))
  :version "22.1"
  :group 'vc)

(defcustom vc-mcvs-register-switches nil
  "Switches for registering a file into Meta-CVS.
A string or list of strings passed to the checkin program by
\\[vc-register].  If nil, use the value of `vc-register-switches'.
If t, use no switches."
  :type '(choice (const :tag "Unspecified" nil)
		 (const :tag "None" t)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List" :value ("") string))
  :version "22.1"
  :group 'vc)

(defcustom vc-mcvs-diff-switches nil
  "String or list of strings specifying switches for Meta-CVS diff under VC.
If nil, use the value of `vc-diff-switches'.  If t, use no switches."
  :type '(choice (const :tag "Unspecified" nil)
		 (const :tag "None" t)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List" :value ("") string))
  :version "22.1"
  :group 'vc)

(defcustom vc-mcvs-header vc-cvs-header
  "Header keywords to be inserted by `vc-insert-headers'."
  :version "24.1"     ; no longer consult the obsolete vc-header-alist
  :type '(repeat string)
  :group 'vc)

(defcustom vc-mcvs-use-edit vc-cvs-use-edit
  "Non-nil means to use `cvs edit' to \"check out\" a file.
This is only meaningful if you don't use the implicit checkout model
\(i.e. if you have $CVSREAD set)."
  :type 'boolean
  :version "22.1"
  :group 'vc)

;;; Properties of the backend

(defalias 'vc-mcvs-revision-granularity 'vc-cvs-revision-granularity)
(defalias 'vc-mcvs-checkout-model 'vc-cvs-checkout-model)

;;;
;;; State-querying functions
;;;

;;;###autoload (defun vc-mcvs-registered (file)
;;;###autoload   (if (vc-find-root file "MCVS/CVS")
;;;###autoload       (progn
;;;###autoload         (load "vc-mcvs")
;;;###autoload         (vc-mcvs-registered file))))

(defun vc-mcvs-root (file)
  "Return the root directory of a Meta-CVS project, if any."
  (or (vc-file-getprop file 'mcvs-root)
      (vc-file-setprop file 'mcvs-root (vc-find-root file "MCVS/CVS"))))

(defun vc-mcvs-read (file)
  (if (file-readable-p file)
      (with-temp-buffer
	(insert-file-contents file)
	(goto-char (point-min))
	(read (current-buffer)))))

(defun vc-mcvs-map-file (dir file)
  (let ((map (vc-mcvs-read (expand-file-name "MCVS/MAP" dir)))
	inode)
    (dolist (x map inode)
      (if (equal (nth 2 x) file) (setq inode (nth 1 x))))))

(defun vc-mcvs-registered (file)
  (let (root inode cvsfile)
    (when (and (setq root (vc-mcvs-root file))
	       (setq inode (vc-mcvs-map-file
			    root (file-relative-name file root))))
      (vc-file-setprop file 'mcvs-inode inode)
      ;; Avoid calling `mcvs diff' in vc-workfile-unchanged-p.
      (vc-file-setprop file 'vc-checkout-time
		       (if (vc-cvs-registered
			    (setq cvsfile (expand-file-name inode root)))
			   (vc-file-getprop cvsfile 'vc-checkout-time)
			 ;; The file might not be registered yet because
			 ;; of lazy-adding.
			 0))
      t)))

(defun vc-mcvs-state (file)
  ;; This would assume the Meta-CVS sandbox is synchronized.
  ;; (vc-mcvs-cvs state file))
  "Meta-CVS-specific version of `vc-state'."
  (if (vc-stay-local-p file)
      (let ((state (vc-file-getprop file 'vc-state)))
        ;; If we should stay local, use the heuristic but only if
        ;; we don't have a more precise state already available.
	(if (memq state '(up-to-date edited))
	    (vc-mcvs-state-heuristic file)
	  state))
    (with-temp-buffer
      (setq default-directory (vc-mcvs-root file))
      (vc-mcvs-command t 0 file "status")
      (vc-cvs-parse-status t))))


(defalias 'vc-mcvs-state-heuristic 'vc-cvs-state-heuristic)

(defun vc-mcvs-working-revision (file)
  (vc-cvs-working-revision
   (expand-file-name (vc-file-getprop file 'mcvs-inode)
		     (vc-file-getprop file 'mcvs-root))))

;;;
;;; State-changing functions
;;;

(defun vc-mcvs-register (files &optional rev comment)
  "Register FILES into the Meta-CVS version-control system.
COMMENT can be used to provide an initial description of FILE.
Passes either `vc-mcvs-register-switches' or `vc-register-switches'
to the Meta-CVS command."
  ;; FIXME: multiple-file case should be made to work.
  (if (> (length files) 1) (error "Registering filesets is not yet supported"))
  (let* ((file (car files))
	 (filename (file-name-nondirectory file))
	 (extpos (string-match "\\." filename))
	 (ext (if extpos (substring filename (1+ extpos))))
	 (root (vc-mcvs-root file))
	 (types-file (expand-file-name "MCVS/TYPES" root))
	 (map-file (expand-file-name "MCVS/MAP" root))
	 (types (vc-mcvs-read types-file)))
    ;; Make sure meta files like MCVS/MAP are not read-only (happens with
    ;; CVSREAD) since Meta-CVS doesn't pay attention to it at all and goes
    ;; belly-up.
    (unless (file-writable-p map-file)
      (vc-checkout map-file t))
    (unless (or (file-writable-p types-file) (not (file-exists-p types-file)))
      (vc-checkout types-file t))
    ;; Make sure the `mcvs add' will not fire up the CVSEDITOR
    ;; to add a rule for the given file's extension.
    (when (and ext (not (assoc ext types)))
      (let ((type (completing-read "Type to use (default): "
				   '("default" "name-only" "keep-old"
				     "binary" "value-only")
				   nil t nil nil "default")))
	(push (list ext (make-symbol (upcase (concat ":" type)))) types)
	(setq types (sort types (lambda (x y) (string< (car x) (car y)))))
	(with-current-buffer (find-file-noselect types-file)
	  (erase-buffer)
	  (pp types (current-buffer))
	  (save-buffer)
	  (unless (get-buffer-window (current-buffer) t)
	    (kill-buffer (current-buffer))))))
    ;; Now do the ADD.
    (prog1 (apply 'vc-mcvs-command nil 0 file
                  "add"
                  (and comment (string-match "[^\t\n ]" comment)
                       (concat "-m" comment))
                  (vc-switches 'MCVS 'register))
      ;; I'm not sure exactly why, but if we don't setup the inode and root
      ;; prop of the file, things break later on in vc-mode-line that
      ;; ends up calling vc-mcvs-working-revision.
      ;; We also need to set vc-checkout-time so that vc-workfile-unchanged-p
      ;; doesn't try to call `mcvs diff' on the file.
      (vc-mcvs-registered file))))

(defalias 'vc-mcvs-responsible-p 'vc-mcvs-root
  "Return non-nil if CVS thinks it is responsible for FILE.")

(defalias 'vc-cvs-could-register 'vc-cvs-responsible-p
  "Return non-nil if FILE could be registered in Meta-CVS.
This is only possible if Meta-CVS is responsible for FILE's directory.")

(defun vc-mcvs-checkin (files rev comment)
  "Meta-CVS-specific version of `vc-backend-checkin'."
  (unless (or (not rev) (vc-mcvs-valid-revision-number-p rev))
    (if (not (vc-mcvs-valid-symbolic-tag-name-p rev))
	(error "%s is not a valid symbolic tag name" rev)
      ;; If the input revision is a valid symbolic tag name, we create it
      ;; as a branch, commit and switch to it.
      ;; This file-specific form of branching is deprecated.
      ;; We can't use `mcvs branch' and `mcvs switch' because they cannot
      ;; be applied just to this one file.
      (apply 'vc-mcvs-command nil 0 files "tag" "-b" (list rev))
      (apply 'vc-mcvs-command nil 0 files "update" "-r" (list rev))
      (mapc (lambda (file) (vc-file-setprop file 'vc-mcvs-sticky-tag rev))
	    files)
      (setq rev nil)))
  ;; This commit might cvs-commit several files (e.g. MAP and TYPES)
  ;; so using numbered revs here is dangerous and somewhat meaningless.
  (when rev (error "Cannot commit to a specific revision number"))
  (let ((status (apply 'vc-mcvs-command nil 1 files
		       "ci" "-m" comment
		       (vc-switches 'MCVS 'checkin))))
    (set-buffer "*vc*")
    (goto-char (point-min))
    (when (not (zerop status))
      ;; Check checkin problem.
      (cond
       ((re-search-forward "Up-to-date check failed" nil t)
	(mapc (lambda (file) (vc-file-setprop file 'vc-state 'needs-merge))
	      files)
        (error "%s" (substitute-command-keys
                (concat "Up-to-date check failed: "
                        "type \\[vc-next-action] to merge in changes"))))
       (t
        (pop-to-buffer (current-buffer))
        (goto-char (point-min))
        (shrink-window-if-larger-than-buffer)
        (error "Check-in failed"))))
    ;; Single-file commit?  Then update the revision by parsing the buffer.
    ;; Otherwise we can't necessarily tell what goes with what; clear
    ;; its properties so they have to be refetched.
    (if (= (length files) 1)
	(vc-file-setprop
	 (car files) 'vc-working-revision
	 (vc-parse-buffer "^\\(new\\|initial\\) revision: \\([0-9.]+\\)" 2))
      (mapc (lambda (file) (vc-file-clearprops file)) files))
    ;; Anyway, forget the checkout model of the file, because we might have
    ;; guessed wrong when we found the file.  After commit, we can
    ;; tell it from the permissions of the file (see
    ;; vc-mcvs-checkout-model).
    (mapc (lambda (file) (vc-file-setprop file 'vc-checkout-model nil))
	    files)

    ;; if this was an explicit check-in (does not include creation of
    ;; a branch), remove the sticky tag.
    (if (and rev (not (vc-mcvs-valid-symbolic-tag-name-p rev)))
	(vc-mcvs-command nil 0 files "update" "-A"))))

(defun vc-mcvs-find-revision (file rev buffer)
  (apply 'vc-mcvs-command
	 buffer 0 file
	 "-Q"				; suppress diagnostic output
	 "update"
	 (and rev (not (string= rev ""))
	      (concat "-r" rev))
	 "-p"
	 (vc-switches 'MCVS 'checkout)))

(defun vc-mcvs-checkout (file &optional editable rev)
  (message "Checking out %s..." file)
  (with-current-buffer (or (get-file-buffer file) (current-buffer))
    (vc-mcvs-update file editable rev (vc-switches 'MCVS 'checkout)))
  (vc-mode-line file)
  (message "Checking out %s...done" file))

(defun vc-mcvs-update (file editable rev switches)
  (if (and (file-exists-p file) (not rev))
      ;; If no revision was specified, just make the file writable
      ;; if necessary (using `cvs-edit' if requested).
      (and editable (not (eq (vc-mcvs-checkout-model (list file)) 'implicit))
	   (if vc-mcvs-use-edit
	       (vc-mcvs-command nil 0 file "edit")
	     (set-file-modes file (logior (file-modes file) 128))
	     (if (equal file buffer-file-name) (toggle-read-only -1))))
    ;; Check out a particular revision (or recreate the file).
    (vc-file-setprop file 'vc-working-revision nil)
    (apply 'vc-mcvs-command nil 0 file
	   (if editable "-w")
	   "update"
	   ;; default for verbose checkout: clear the sticky tag so
	   ;; that the actual update will get the head of the trunk
	   (if (or (not rev) (string= rev ""))
	       "-A"
	     (concat "-r" rev))
	   switches)))

(defun vc-mcvs-rename-file (old new)
  (vc-mcvs-command nil 0 new "move" (file-relative-name old)))

(defun vc-mcvs-revert (file &optional contents-done)
  "Revert FILE to the working revision it was based on."
  (vc-default-revert 'MCVS file contents-done)
  (unless (eq (vc-mcvs-checkout-model (list file)) 'implicit)
    (if vc-mcvs-use-edit
        (vc-mcvs-command nil 0 file "unedit")
      ;; Make the file read-only by switching off all w-bits
      (set-file-modes file (logand (file-modes file) 3950)))))

(defun vc-mcvs-merge (file first-revision &optional second-revision)
  "Merge changes into current working copy of FILE.
The changes are between FIRST-REVISION and SECOND-REVISION."
  (vc-mcvs-command nil 0 file
		   "update" "-kk"
		   (concat "-j" first-revision)
		   (concat "-j" second-revision))
  (vc-file-setprop file 'vc-state 'edited)
  (with-current-buffer (get-buffer "*vc*")
    (goto-char (point-min))
    (if (re-search-forward "conflicts during merge" nil t)
        1				; signal error
      0)))				; signal success

(defun vc-mcvs-merge-news (file)
  "Merge in any new changes made to FILE."
  (message "Merging changes into %s..." file)
  ;; (vc-file-setprop file 'vc-working-revision nil)
  (vc-file-setprop file 'vc-checkout-time 0)
  (vc-mcvs-command nil 0 file "update")
  ;; Analyze the merge result reported by Meta-CVS, and set
  ;; file properties accordingly.
  (with-current-buffer (get-buffer "*vc*")
    (goto-char (point-min))
    ;; get new working revision
    (if (re-search-forward
	 "^Merging differences between [0-9.]* and \\([0-9.]*\\) into" nil t)
	(vc-file-setprop file 'vc-working-revision (match-string 1))
      (vc-file-setprop file 'vc-working-revision nil))
    ;; get file status
    (prog1
        (if (eq (buffer-size) 0)
            0 ;; there were no news; indicate success
          (if (re-search-forward
               (concat "^\\([CMUP] \\)?"
                       ".*"
                       "\\( already contains the differences between \\)?")
               nil t)
              (cond
               ;; Merge successful, we are in sync with repository now
               ((or (match-string 2)
                    (string= (match-string 1) "U ")
                    (string= (match-string 1) "P "))
                (vc-file-setprop file 'vc-state 'up-to-date)
                (vc-file-setprop file 'vc-checkout-time
                                 (nth 5 (file-attributes file)))
                0);; indicate success to the caller
               ;; Merge successful, but our own changes are still in the file
               ((string= (match-string 1) "M ")
                (vc-file-setprop file 'vc-state 'edited)
                0);; indicate success to the caller
               ;; Conflicts detected!
               (t
                (vc-file-setprop file 'vc-state 'edited)
                1);; signal the error to the caller
               )
            (pop-to-buffer "*vc*")
            (error "Couldn't analyze mcvs update result")))
      (message "Merging changes into %s...done" file))))

(defun vc-mcvs-modify-change-comment (files rev comment)
  "Modify the change comments for FILES on a specified REV.
Will fail unless you have administrative privileges on the repo."
  (vc-mcvs-command nil 0 files "rcs" (concat "-m" comment ":" rev)))


;;;
;;; History functions
;;;

(defun vc-mcvs-print-log (files &optional buffer)
  "Get change log associated with FILES."
  (let ((default-directory (vc-mcvs-root (car files))))
    ;; Run the command from the root dir so that `mcvs filt' returns
    ;; valid relative names.
    (vc-mcvs-command
     buffer
     (if (vc-stay-local-p files) 'async 0)
     files "log")))

(defun vc-mcvs-diff (files &optional oldvers newvers buffer)
  "Get a difference report using Meta-CVS between two revisions of FILES."
    (let* ((async (and (not vc-disable-async-diff)
                       (vc-stay-local-p files)))
	   ;; Run the command from the root dir so that `mcvs filt' returns
	   ;; valid relative names.
	   (default-directory (vc-mcvs-root (car files)))
	   (status
	    (apply 'vc-mcvs-command (or buffer "*vc-diff*")
		   (if async 'async 1)
		   files "diff"
		   (and oldvers (concat "-r" oldvers))
		   (and newvers (concat "-r" newvers))
		   (vc-switches 'MCVS 'diff))))
      (if async 1 status)))	       ; async diff, pessimistic assumption.

(defun vc-mcvs-annotate-command (file buffer &optional revision)
  "Execute \"mcvs annotate\" on FILE, inserting the contents in BUFFER.
Optional arg REVISION is a revision to annotate from."
  (vc-mcvs-command
   buffer
   (if (vc-stay-local-p file) 'async 0)
   file "annotate" (if revision (concat "-r" revision)))
  (with-current-buffer buffer
    (goto-char (point-min))
    (re-search-forward "^[0-9]")
    (delete-region (point-min) (1- (point)))))

(defalias 'vc-mcvs-annotate-current-time 'vc-cvs-annotate-current-time)
(defalias 'vc-mcvs-annotate-time 'vc-cvs-annotate-time)

;;;
;;; Tag system
;;;

(defun vc-mcvs-create-tag (dir name branchp)
  "Assign to DIR's current revision a given NAME.
If BRANCHP is non-nil, the name is created as a branch (and the current
workspace is immediately moved to that new branch)."
  (if (not branchp)
      (vc-mcvs-command nil 0 dir "tag" "-c" name)
    (vc-mcvs-command nil 0 dir "branch" name)
    (vc-mcvs-command nil 0 dir "switch" name)))

(defun vc-mcvs-retrieve-tag (dir name update)
  "Retrieve a tag at and below DIR.
NAME is the name of the tag; if it is empty, do a `cvs update'.
If UPDATE is non-nil, then update (resynch) any affected buffers."
  (with-current-buffer (get-buffer-create "*vc*")
    (let ((default-directory dir)
	  (sticky-tag))
      (erase-buffer)
      (if (or (not name) (string= name ""))
	  (vc-mcvs-command t 0 nil "update")
	(vc-mcvs-command t 0 nil "update" "-r" name)
	(setq sticky-tag name))
      (when update
	(goto-char (point-min))
	(while (not (eobp))
	  (if (looking-at "\\([CMUP]\\) \\(.*\\)")
	      (let* ((file (expand-file-name (match-string 2) dir))
		     (state (match-string 1))
		     (buffer (find-buffer-visiting file)))
		(when buffer
		  (cond
		   ((or (string= state "U")
			(string= state "P"))
		    (vc-file-setprop file 'vc-state 'up-to-date)
		    (vc-file-setprop file 'vc-working-revision nil)
		    (vc-file-setprop file 'vc-checkout-time
				     (nth 5 (file-attributes file))))
		   ((or (string= state "M")
			(string= state "C"))
		    (vc-file-setprop file 'vc-state 'edited)
		    (vc-file-setprop file 'vc-working-revision nil)
		    (vc-file-setprop file 'vc-checkout-time 0)))
		  (vc-file-setprop file 'vc-mcvs-sticky-tag sticky-tag)
		  (vc-resynch-buffer file t t))))
	  (forward-line 1))))))


;;;
;;; Miscellaneous
;;;

(defalias 'vc-mcvs-make-version-backups-p 'vc-stay-local-p
  "Return non-nil if version backups should be made for FILE.")
(defalias 'vc-mcvs-check-headers 'vc-cvs-check-headers)


;;;
;;; Internal functions
;;;

(defun vc-mcvs-command (buffer okstatus file &rest flags)
  "A wrapper around `vc-do-command' for use in vc-mcvs.el.
The difference to vc-do-command is that this function always invokes `mcvs',
and that it passes `vc-mcvs-global-switches' to it before FLAGS."
  (let ((args (append '("--error-terminate")
		      (if (stringp vc-mcvs-global-switches)
			  (cons vc-mcvs-global-switches flags)
			(append vc-mcvs-global-switches flags)))))
    (if (not (member (car flags) '("diff" "log" "status")))
	;; No need to filter: do it the easy way.
	(apply 'vc-do-command (or buffer "*vc*") okstatus "mcvs" file args)
      ;; We need to filter the output.
      ;; The output of the filter uses filenames relative to the root,
      ;; so we need to change the default-directory.
      ;; (assert (equal default-directory (vc-mcvs-root file)))
      (vc-do-command
       (or buffer "*vc*") okstatus "sh" nil "-c"
       (concat "mcvs "
	       (mapconcat
		'shell-quote-argument
		(append (remq nil args)
			(if file (list (file-relative-name file))))
		" ")
	       " | mcvs filt")))))

(defun vc-mcvs-repository-hostname (dirname)
  (vc-cvs-repository-hostname (vc-mcvs-root dirname)))

(defun vc-mcvs-dir-state-heuristic (dir)
  "Find the Meta-CVS state of all files in DIR, using only local information."
  (with-temp-buffer
    (vc-cvs-get-entries dir)
    (goto-char (point-min))
    (while (not (eobp))
      ;; Meta-MCVS-removed files are not taken under VC control.
      (when (looking-at "/\\([^/]*\\)/[^/-]")
	(let ((file (expand-file-name (match-string 1) dir)))
	  (unless (vc-file-getprop file 'vc-state)
	    (vc-cvs-parse-entry file t))))
      (forward-line 1))))

(defalias 'vc-mcvs-valid-symbolic-tag-name-p 'vc-cvs-valid-symbolic-tag-name-p)
(defalias 'vc-mcvs-valid-revision-number-p 'vc-cvs-valid-revision-number-p)

(provide 'vc-mcvs)

;; ********** READ THIS! **********
;;
;; This file apparently does not work with the new (as of Emacs 23)
;; VC code.  Use at your own risk.  Please contact emacs-devel if you
;; can maintain this file and update it to work correctly.
;;
;; ********** READ THIS! **********

;;; vc-mcvs.el ends here
