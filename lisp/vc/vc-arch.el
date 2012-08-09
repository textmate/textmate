;;; vc-arch.el --- VC backend for the Arch version-control system

;; Copyright (C) 2004-2012  Free Software Foundation, Inc.

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

;; The home page of the Arch version control system is at
;;
;;      http://www.gnuarch.org/
;;
;; This is derived from vc-mcvs.el as follows:
;; - cp vc-mcvs.el vc-arch.el and then M-% mcvs RET arch RET
;;
;; Then of course started the hacking.
;;
;; What has been partly tested:
;; - Open a file.
;; - C-x v =  without any prefix arg.
;; - C-x v v  to commit a change to a single file.

;; Bugs:

;; - *vc-log*'s initial content lacks the `Summary:' lines.
;; - All files under the tree are considered as "under Arch's control"
;;   without regards to =tagging-method and such.
;; - Files are always considered as `edited'.
;; - C-x v l does not work.
;; - C-x v i does not work.
;; - C-x v ~ does not work.
;; - C-x v u does not work.
;; - C-x v s does not work.
;; - C-x v r does not work.
;; - VC directory listings do not work.
;; - And more...

;;; Code:

(eval-when-compile (require 'vc) (require 'cl))

;;; Properties of the backend

(defun vc-arch-revision-granularity () 'repository)
(defun vc-arch-checkout-model (files) 'implicit)

;;;
;;; Customization options
;;;

(defgroup vc-arch nil
  "VC Arch backend."
  :version "24.1"
  :group 'vc)

;; It seems Arch diff does not accept many options, so this is not
;; very useful.  It exists mainly so that the VC backends are all
;; consistent with regards to their treatment of diff switches.
(defcustom vc-arch-diff-switches t
  "String or list of strings specifying switches for Arch diff under VC.
If nil, use the value of `vc-diff-switches'.  If t, use no switches."
  :type '(choice (const :tag "Unspecified" nil)
		 (const :tag "None" t)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List" :value ("") string))
  :version "23.1"
  :group 'vc-arch)

(define-obsolete-variable-alias 'vc-arch-command 'vc-arch-program "23.1")

(defcustom vc-arch-program
  (let ((candidates '("tla" "baz")))
    (while (and candidates (not (executable-find (car candidates))))
      (setq candidates (cdr candidates)))
    (or (car candidates) "tla"))
  "Name of the Arch executable."
  :type 'string
  :group 'vc-arch)

;; Clear up the cache to force vc-call to check again and discover
;; new functions when we reload this file.
(put 'Arch 'vc-functions nil)

;;;###autoload (defun vc-arch-registered (file)
;;;###autoload   (if (vc-find-root file "{arch}/=tagging-method")
;;;###autoload       (progn
;;;###autoload         (load "vc-arch")
;;;###autoload         (vc-arch-registered file))))

(defun vc-arch-add-tagline ()
  "Add an `arch-tag' to the end of the current file."
  (interactive)
  (comment-normalize-vars)
  (goto-char (point-max))
  (forward-comment -1)
  (skip-chars-forward " \t\n")
  (cond
   ((not (bolp)) (insert "\n\n"))
   ((not (eq ?\n (char-before (1- (point))))) (insert "\n")))
  (let ((beg (point))
	(idfile (and buffer-file-name
		     (expand-file-name
		      (concat ".arch-ids/"
			      (file-name-nondirectory buffer-file-name)
			      ".id")
		      (file-name-directory buffer-file-name)))))
    (insert "arch-tag: ")
    (if (and idfile (file-exists-p idfile))
	;; If the file is unreadable, we do want to get an error here.
	(progn
	  (insert-file-contents idfile)
	  (forward-line 1)
	  (delete-file idfile))
      (condition-case nil
	  (call-process "uuidgen" nil t)
	(file-error (insert (format "%s <%s> %s"
				    (current-time-string)
				    user-mail-address
				    (+ (nth 2 (current-time))
				       (buffer-size)))))))
    (comment-region beg (point))))

(defconst vc-arch-tagline-re "^\\W*arch-tag:[ \t]*\\(.*[^ \t\n]\\)")

(defmacro vc-with-current-file-buffer (file &rest body)
  (declare (indent 2) (debug t))
  `(let ((-kill-buf- nil)
         (-file- ,file))
     (with-current-buffer (or (find-buffer-visiting -file-)
                              (setq -kill-buf- (generate-new-buffer " temp")))
       ;; Avoid find-file-literally since it can do many undesirable extra
       ;; things (among which, call us back into an infinite loop).
       (if -kill-buf- (insert-file-contents -file-))
       (unwind-protect
           (progn ,@body)
         (if (buffer-live-p -kill-buf-) (kill-buffer -kill-buf-))))))

(defun vc-arch-file-source-p (file)
  "Can return nil, `maybe' or a non-nil value.
Only the value `maybe' can be trusted :-(."
  ;; FIXME: Check the tag and name of parent dirs.
  (unless (string-match "\\`[,+]" (file-name-nondirectory file))
    (or (string-match "\\`{arch}/"
		      (file-relative-name file (vc-arch-root file)))
	(file-exists-p
	 ;; Check the presence of an ID file.
	 (expand-file-name
	  (concat ".arch-ids/" (file-name-nondirectory file) ".id")
	  (file-name-directory file)))
	;; Check the presence of a tagline.
	(vc-with-current-file-buffer file
	  (save-excursion
	    (goto-char (point-max))
	    (or (re-search-backward vc-arch-tagline-re (- (point) 1000) t)
		(progn
		  (goto-char (point-min))
		  (re-search-forward vc-arch-tagline-re (+ (point) 1000) t)))))
	;; FIXME: check =tagging-method to see whether untagged files might
	;; be source or not.
	(with-current-buffer
	    (find-file-noselect (expand-file-name "{arch}/=tagging-method"
						  (vc-arch-root file)))
	  (let ((untagged-source t))	;Default is `names'.
	    (save-excursion
	      (goto-char (point-min))
	      (if (re-search-forward "^[ \t]*\\(\\(tagline\\|implicit\\|names\\)\\|explicit\\)" nil t)
		  (setq untagged-source (match-end 2)))
	      (if (re-search-forward "^[ \t]*untagged-source[ \t]+\\(\\(source\\)\\|precious\\|backup\\|junk\\|unrecognized\\)" nil t)
		  (setq untagged-source (match-end 2))))
	    (if untagged-source 'maybe))))))

(defun vc-arch-file-id (file)
  ;; Don't include the kind of ID this is because it seems to be too messy.
  (let ((idfile (expand-file-name
		 (concat ".arch-ids/" (file-name-nondirectory file) ".id")
		 (file-name-directory file))))
    (if (file-exists-p idfile)
	(with-temp-buffer
	  (insert-file-contents idfile)
	  (looking-at ".*[^ \n\t]")
	  (match-string 0))
      (with-current-buffer (find-file-noselect file)
	(save-excursion
	  (goto-char (point-max))
	  (if (or (re-search-backward vc-arch-tagline-re (- (point) 1000) t)
		  (progn
		    (goto-char (point-min))
		    (re-search-forward vc-arch-tagline-re (+ (point) 1000) t)))
	      (match-string 1)
	    (concat "./" (file-relative-name file (vc-arch-root file)))))))))

(defun vc-arch-tagging-method (file)
  (with-current-buffer
      (find-file-noselect
       (expand-file-name "{arch}/=tagging-method" (vc-arch-root file)))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward
	   "^[ \t]*\\(tagline\\|implicit\\|names\\|explicit\\)" nil t)
	  (intern (match-string 1))
	'names))))

(defun vc-arch-root (file)
  "Return the root directory of an Arch project, if any."
  (or (vc-file-getprop file 'arch-root)
      ;; Check the =tagging-method, in case someone naively manually
      ;; creates a {arch} directory somewhere.
      (let ((root (vc-find-root file "{arch}/=tagging-method")))
	(when root
	  (vc-file-setprop
	   file 'arch-root root)))))

(defun vc-arch-register (files &optional rev comment)
  (if rev (error "Explicit initial revision not supported for Arch"))
  (dolist (file files)
    (let ((tagmet (vc-arch-tagging-method file)))
      (if (and (memq tagmet '(tagline implicit)) comment-start)
	  (with-current-buffer (find-file-noselect file)
	    (if (buffer-modified-p)
		(error "Save %s first" (buffer-name)))
	    (vc-arch-add-tagline)
	    (save-buffer)))))
  (vc-arch-command nil 0 files "add"))

(defun vc-arch-registered (file)
  ;; Don't seriously check whether it's source or not.  Checking would
  ;; require running TLA, so it's better to not do it, so it also works if
  ;; TLA is not installed.
  (and (vc-arch-root file)
       (vc-arch-file-source-p file)))

(defun vc-arch-default-version (file)
  (or (vc-file-getprop (vc-arch-root file) 'arch-default-version)
      (let* ((root (vc-arch-root file))
	     (f (expand-file-name "{arch}/++default-version" root)))
	(if (file-readable-p f)
	    (vc-file-setprop
	     root 'arch-default-version
	     (with-temp-buffer
	       (insert-file-contents f)
	       ;; Strip the terminating newline.
	       (buffer-substring (point-min) (1- (point-max)))))))))

(defun vc-arch-workfile-unchanged-p (file)
  "Stub: arch workfiles are always considered to be in a changed state,"
  nil)

(defun vc-arch-state (file)
  ;; There's no checkout operation and merging is not done from VC
  ;; so the only operation that's state dependent that VC supports is commit
  ;; which is only activated if the file is `edited'.
  (let* ((root (vc-arch-root file))
	 (ver (vc-arch-default-version file))
	 (pat (concat "\\`" (subst-char-in-string ?/ ?% ver)))
	 (dir (expand-file-name ",,inode-sigs/"
				(expand-file-name "{arch}" root)))
	 (sigfile nil))
    (dolist (f (if (file-directory-p dir) (directory-files dir t pat)))
      (if (or (not sigfile) (file-newer-than-file-p f sigfile))
	  (setq sigfile f)))
    (if (not sigfile)
	'edited				;We know nothing.
      (let ((id (vc-arch-file-id file)))
	(setq id (replace-regexp-in-string "[ \t]" "_" id))
	(with-current-buffer (find-file-noselect sigfile)
	  (goto-char (point-min))
	  (while (and (search-forward id nil 'move)
		      (save-excursion
			(goto-char (- (match-beginning 0) 2))
			;; For `names', the lines start with `?./foo/bar'.
			;; For others there's 2 chars before the ./foo/bar.
			(or (not (or (bolp) (looking-at "\n?")))
			    ;; Ignore E_ entries used for foo.id files.
			    (looking-at "E_")))))
	  (if (eobp)
	      ;; ID not found.
	      (if (equal (file-name-nondirectory sigfile)
			 (subst-char-in-string
			  ?/ ?% (vc-arch-working-revision file)))
		  'added
		;; Might be `added' or `up-to-date' as well.
		;; FIXME: Check in the patch logs to find out.
		'edited)
	    ;; Found the ID, let's check the inode.
	    (if (not (re-search-forward
		      "\t.*mtime=\\([0-9]+\\):size=\\([0-9]+\\)"
		      (line-end-position) t))
		;; Buh?  Unexpected format.
		'edited
	      (let ((ats (file-attributes file)))
		(if (and (eq (nth 7 ats) (string-to-number (match-string 2)))
			 (equal (format-time-string "%s" (nth 5 ats))
				(match-string 1)))
		    'up-to-date
		  'edited)))))))))

(defun vc-arch-dir-status (dir callback)
  "Run 'tla inventory' for DIR and pass results to CALLBACK.
CALLBACK expects (ENTRIES &optional MORE-TO-COME); see
`vc-dir-refresh'."
  (let ((default-directory dir))
    (vc-arch-command t 'async nil "changes"))
  ;; The updating could be done asynchronously.
  (vc-exec-after
   `(vc-arch-after-dir-status ',callback)))

(defun vc-arch-after-dir-status (callback)
  (let* ((state-map '(("M " . edited)
		      ("Mb" . edited)	;binary
		      ("D " . removed)
		      ("D/" . removed)	;directory
		      ("A " . added)
		      ("A/" . added)	;directory
		      ("=>" . renamed)
		      ("/>" . renamed)	;directory
		      ("lf" . symlink-to-file)
		      ("fl" . file-to-symlink)
		      ("--" . permissions-changed)
		      ("-/" . permissions-changed) ;directory
		      ))
	 (state-map-regexp (regexp-opt (mapcar 'car state-map) t))
	 (entry-regexp (concat "^" state-map-regexp " \\(.*\\)$"))
	 result)
    (goto-char (point-min))
    ;;(message "Got %s" (buffer-string))
    (while (re-search-forward entry-regexp nil t)
      (let* ((state-string (match-string 1))
	     (state (cdr (assoc state-string state-map)))
	     (filename (match-string 2)))
	(push (list filename state) result)))

    (funcall callback result nil)))

(defun vc-arch-working-revision (file)
  (let* ((root (expand-file-name "{arch}" (vc-arch-root file)))
	 (defbranch (vc-arch-default-version file)))
    (when (and defbranch (string-match "\\`\\(.+@[^/\n]+\\)/\\(\\(\\(.*?\\)\\(?:--.*\\)?\\)--.*\\)\\'" defbranch))
      (let* ((archive (match-string 1 defbranch))
	     (category (match-string 4 defbranch))
	     (branch (match-string 3 defbranch))
	     (version (match-string 2 defbranch))
	     (sealed nil) (rev-nb 0)
	     (rev nil)
	     logdir tmp)
	(setq logdir (expand-file-name category root))
	(setq logdir (expand-file-name branch logdir))
	(setq logdir (expand-file-name version logdir))
	(setq logdir (expand-file-name archive logdir))
	(setq logdir (expand-file-name "patch-log" logdir))
	(dolist (file (if (file-directory-p logdir) (directory-files logdir)))
	  ;; Revision names go: base-0, patch-N, version-0, versionfix-M.
	  (when (and (eq (aref file 0) ?v) (not sealed))
	    (setq sealed t rev-nb 0))
	  (if (and (string-match "-\\([0-9]+\\)\\'" file)
		   (setq tmp (string-to-number (match-string 1 file)))
		   (or (not sealed) (eq (aref file 0) ?v))
		   (>= tmp rev-nb))
	      (setq rev-nb tmp rev file)))
	;; Use "none-000" if the tree hasn't yet been committed on the
	;; default branch.  We'll then get "Arch:000[branch]" on the mode-line.
	(concat defbranch "--" (or rev "none-000"))))))


(defcustom vc-arch-mode-line-rewrite
  '(("\\`.*--\\(.*--.*\\)--\\(v?\\).*-\\([0-9]+\\)\\'" . "\\2\\3[\\1]"))
  "Rewrite rules to shorten Arch's revision names on the mode-line."
  :type '(repeat (cons regexp string))
  :group 'vc-arch)

(defun vc-arch-mode-line-string (file)
  "Return string for placement in modeline by `vc-mode-line' for FILE."
  (let ((rev (vc-working-revision file)))
    (dolist (rule vc-arch-mode-line-rewrite)
      (if (string-match (car rule) rev)
	  (setq rev (replace-match (cdr rule) t nil rev))))
    (format "Arch%c%s"
	    (case (vc-state file)
	      ((up-to-date needs-update) ?-)
	      (added ?@)
	      (t ?:))
	    rev)))

(defun vc-arch-diff3-rej-p (rej)
  (let ((attrs (file-attributes rej)))
    (and attrs (< (nth 7 attrs) 60)
	 (with-temp-buffer
	   (insert-file-contents rej)
	   (goto-char (point-min))
	   (looking-at "Conflicts occurred, diff3 conflict markers left in file\\.")))))

(defun vc-arch-delete-rej-if-obsolete ()
  "For use in `after-save-hook'."
  (save-excursion
    (let ((rej (concat buffer-file-name ".rej")))
      (when (and buffer-file-name (vc-arch-diff3-rej-p rej))
	(unless (re-search-forward "^<<<<<<< " nil t)
	  ;; The .rej file is obsolete.
	  (condition-case nil (delete-file rej) (error nil))
	  ;; Remove the hook so that it is not called multiple times.
	  (remove-hook 'after-save-hook 'vc-arch-delete-rej-if-obsolete t))))))

(defun vc-arch-find-file-hook ()
  (let ((rej (concat buffer-file-name ".rej")))
    (when (and buffer-file-name (file-exists-p rej))
      (if (vc-arch-diff3-rej-p rej)
	  (save-excursion
	    (goto-char (point-min))
	    (if (not (re-search-forward "^<<<<<<< " nil t))
		;; The .rej file is obsolete.
		(condition-case nil (delete-file rej) (error nil))
	      (smerge-mode 1)
	      (add-hook 'after-save-hook
			'vc-arch-delete-rej-if-obsolete nil t)
	      (message "There are unresolved conflicts in this file")))
	(message "There are unresolved conflicts in %s"
		 (file-name-nondirectory rej))))))

(defun vc-arch-checkin (files rev comment)
  (if rev (error "Committing to a specific revision is unsupported"))
  ;; FIXME: This implementation probably only works for singleton filesets
  (let ((summary (file-relative-name (car files) (vc-arch-root (car files)))))
    ;; Extract a summary from the comment.
    (when (or (string-match "\\`Summary:[ \t]*\\(.*[^ \t\n]\\)\\([ \t]*\n\\)*" comment)
	      (string-match "\\`[ \t]*\\(.*[^ \t\n]\\)[ \t]*\\(\n?\\'\\|\n\\([ \t]*\n\\)+\\)" comment))
      (setq summary (match-string 1 comment))
      (setq comment (substring comment (match-end 0))))
    (vc-arch-command nil 0 files "commit" "-s" summary "-L" comment "--"
		     (vc-switches 'Arch 'checkin))))

(defun vc-arch-diff (files &optional oldvers newvers buffer)
  "Get a difference report using Arch between two versions of FILES."
  ;; FIXME: This implementation only works for singleton filesets.  To make
  ;; it work for more cases, we have to either call `file-diffs' manually on
  ;; each and every `file' in the fileset, or use `changes --diffs' (and
  ;; variants) and maybe filter the output with `filterdiff' to only include
  ;; the files in which we're interested.
  (let ((file (car files)))
    (if (and newvers
             (vc-up-to-date-p file)
             (equal newvers (vc-working-revision file)))
        ;; Newvers is the base revision and the current file is unchanged,
        ;; so we can diff with the current file.
        (setq newvers nil))
    (if newvers
        (error "Diffing specific revisions not implemented")
      (let* (process-file-side-effects
	     (async (not vc-disable-async-diff))
             ;; Run the command from the root dir.
             (default-directory (vc-arch-root file))
             (status
              (vc-arch-command
               (or buffer "*vc-diff*")
               (if async 'async 1)
               nil "file-diffs"
               (vc-switches 'Arch 'diff)
               (file-relative-name file)
               (if (equal oldvers (vc-working-revision file))
                   nil
                 oldvers))))
        (if async 1 status)))))	       ; async diff, pessimistic assumption.

(defun vc-arch-delete-file (file)
  (vc-arch-command nil 0 file "rm"))

(defun vc-arch-rename-file (old new)
  (vc-arch-command nil 0 new "mv" (file-relative-name old)))

(defalias 'vc-arch-responsible-p 'vc-arch-root)

(defun vc-arch-command (buffer okstatus file &rest flags)
  "A wrapper around `vc-do-command' for use in vc-arch.el."
  (apply 'vc-do-command (or buffer "*vc*") okstatus vc-arch-program file flags))

(defun vc-arch-init-revision () nil)

;;; Completion of versions and revisions.

(defun vc-arch--version-completion-table (root string)
  (delq nil
	(mapcar
	 (lambda (d)
	   (when (string-match "/\\([^/]+\\)/\\([^/]+\\)\\'" d)
	     (concat (match-string 2 d) "/" (match-string 1 d))))
	 (let ((default-directory root))
	   (file-expand-wildcards
	    (concat "*/*/"
		    (if (string-match "/" string)
			(concat (substring string (match-end 0))
				"*/" (substring string 0 (match-beginning 0)))
		      (concat "*/" string))
		    "*"))))))

(defun vc-arch-revision-completion-table (files)
  (lexical-let ((files files))
    (lambda (string pred action)
      ;; FIXME: complete revision patches as well.
      (let* ((root (expand-file-name "{arch}" (vc-arch-root (car files))))
             (table (vc-arch--version-completion-table root string)))
	(complete-with-action action table string pred)))))

;;; Trimming revision libraries.

;; This code is not directly related to VC and there are many variants of
;; this functionality available as scripts, but I like this version better,
;; so maybe others will like it too.

(defun vc-arch-trim-find-least-useful-rev (revs)
  (let* ((first (pop revs))
         (second (pop revs))
         (third (pop revs))
         ;; We try to give more importance to recent revisions.  The idea is
         ;; that it's OK if checking out a revision 1000-patch-old is ten
         ;; times slower than checking out a revision 100-patch-old.  But at
         ;; the same time a 2-patch-old rev isn't really ten times more
         ;; important than a 20-patch-old, so we use an arbitrary constant
         ;; "100" to reduce this effect for recent revisions.  Making this
         ;; constant a float has the side effect of causing the subsequent
         ;; computations to be done as floats as well.
         (max (+ 100.0 (car (or (car (last revs)) third))))
         (cost (lambda () (/ (- (car third) (car first)) (- max (car second)))))
         (minrev second)
         (mincost (funcall cost)))
    (while revs
      (setq first second)
      (setq second third)
      (setq third (pop revs))
      (when (< (funcall cost) mincost)
        (setq minrev second)
        (setq mincost (funcall cost))))
    minrev))

(defun vc-arch-trim-make-sentinel (revs)
  (if (null revs) (lambda (proc msg) (message "VC-Arch trimming ... done"))
    (lexical-let ((revs revs))
      (lambda (proc msg)
        (message "VC-Arch trimming %s..." (file-name-nondirectory (car revs)))
        (rename-file (car revs) (concat (car revs) "*rm*"))
       (setq proc (start-process "vc-arch-trim" nil
                                  "rm" "-rf" (concat (car revs) "*rm*")))
        (set-process-sentinel proc (vc-arch-trim-make-sentinel (cdr revs)))))))

(defun vc-arch-trim-one-revlib (dir)
  "Delete half of the revisions in the revision library."
  (interactive "Ddirectory: ")
  (let ((garbage (directory-files dir 'full "\\`,," 'nosort)))
    (when garbage
      (funcall (vc-arch-trim-make-sentinel garbage) nil nil)))
  (let ((revs
         (sort (delq nil
                     (mapcar
                      (lambda (f)
                        (when (string-match "-\\([0-9]+\\)\\'" f)
                          (cons (string-to-number (match-string 1 f)) f)))
                      (directory-files dir nil nil 'nosort)))
               'car-less-than-car))
        (subdirs nil))
    (when (cddr revs)
      (dotimes (i (/ (length revs) 2))
        (let ((minrev (vc-arch-trim-find-least-useful-rev revs)))
          (setq revs (delq minrev revs))
          (push minrev subdirs)))
      (funcall (vc-arch-trim-make-sentinel
                (mapcar (lambda (x) (expand-file-name (cdr x) dir)) subdirs))
               nil nil))))

(defun vc-arch-trim-revlib ()
  "Delete half of the revisions in the revision library."
  (interactive)
  (let ((rl-dir (with-output-to-string
                  (call-process vc-arch-program nil standard-output nil
                                "my-revision-library"))))
    (while (string-match "\\(.*\\)\n" rl-dir)
      (let ((dir (match-string 1 rl-dir)))
        (setq rl-dir
              (if (and (file-directory-p dir) (file-writable-p dir))
                  dir
                (substring rl-dir (match-end 0))))))
    (unless (file-writable-p rl-dir)
      (error "No writable revlib directory found"))
    (message "Revlib at %s" rl-dir)
    (let* ((archives (directory-files rl-dir 'full "[^.]\\|..."))
           (categories
            (apply 'append
                   (mapcar (lambda (dir)
                             (when (file-directory-p dir)
                               (directory-files dir 'full "[^.]\\|...")))
                           archives)))
           (branches
            (apply 'append
                   (mapcar (lambda (dir)
                             (when (file-directory-p dir)
                               (directory-files dir 'full "[^.]\\|...")))
                           categories)))
           (versions
            (apply 'append
                   (mapcar (lambda (dir)
                             (when (file-directory-p dir)
                               (directory-files dir 'full "--.*--")))
                           branches))))
      (mapc 'vc-arch-trim-one-revlib versions))
    ))

(defvar vc-arch-extra-menu-map
  (let ((map (make-sparse-keymap)))
    (define-key map [add-tagline]
      '(menu-item "Add tagline" vc-arch-add-tagline))
    map))

(defun vc-arch-extra-menu () vc-arch-extra-menu-map)


;;; Less obvious implementations.

(defun vc-arch-find-revision (file rev buffer)
  (let ((out (make-temp-file "vc-out")))
    (unwind-protect
        (progn
          (with-temp-buffer
            (vc-arch-command (current-buffer) 1 nil "file-diffs" file rev)
            (call-process-region (point-min) (point-max)
                                 "patch" nil nil nil "-R" "-o" out file))
          (with-current-buffer buffer
            (insert-file-contents out)))
      (delete-file out))))

(provide 'vc-arch)

;;; vc-arch.el ends here
