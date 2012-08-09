;;; vc-git.el --- VC backend for the git version control system

;; Copyright (C) 2006-2012 Free Software Foundation, Inc.

;; Author: Alexandre Julliard <julliard@winehq.org>
;; Keywords: vc tools
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

;; This file contains a VC backend for the git version control
;; system.
;;

;;; Installation:

;; To install: put this file on the load-path and add Git to the list
;; of supported backends in `vc-handled-backends'; the following line,
;; placed in your ~/.emacs, will accomplish this:
;;
;;     (add-to-list 'vc-handled-backends 'Git)

;;; Todo:
;;  - check if more functions could use vc-git-command instead
;;     of start-process.
;;  - changelog generation

;; Implement the rest of the vc interface. See the comment at the
;; beginning of vc.el. The current status is:
;; ("??" means: "figure out what to do about it")
;;
;; FUNCTION NAME                                   STATUS
;; BACKEND PROPERTIES
;; * revision-granularity                          OK
;; STATE-QUERYING FUNCTIONS
;; * registered (file)                             OK
;; * state (file)                                  OK
;; - state-heuristic (file)                        NOT NEEDED
;; * working-revision (file)                       OK
;; - latest-on-branch-p (file)                     NOT NEEDED
;; * checkout-model (files)                        OK
;; - workfile-unchanged-p (file)                   OK
;; - mode-line-string (file)                       OK
;; STATE-CHANGING FUNCTIONS
;; * create-repo ()                                OK
;; * register (files &optional rev comment)        OK
;; - init-revision (file)                          NOT NEEDED
;; - responsible-p (file)                          OK
;; - could-register (file)                         NOT NEEDED, DEFAULT IS GOOD
;; - receive-file (file rev)                       NOT NEEDED
;; - unregister (file)                             OK
;; * checkin (files rev comment)                   OK
;; * find-revision (file rev buffer)               OK
;; * checkout (file &optional editable rev)        OK
;; * revert (file &optional contents-done)         OK
;; - rollback (files)                              COULD BE SUPPORTED
;; - merge (file rev1 rev2)                   It would be possible to merge
;;                                          changes into a single file, but
;;                                          when committing they wouldn't
;;                                          be identified as a merge
;;                                          by git, so it's probably
;;                                          not a good idea.
;; - merge-news (file)                     see `merge'
;; - steal-lock (file &optional revision)          NOT NEEDED
;; HISTORY FUNCTIONS
;; * print-log (files buffer &optional shortlog start-revision limit)   OK
;; - log-view-mode ()                              OK
;; - show-log-entry (revision)                     OK
;; - comment-history (file)                        ??
;; - update-changelog (files)                      COULD BE SUPPORTED
;; * diff (file &optional rev1 rev2 buffer)        OK
;; - revision-completion-table (files)             OK
;; - annotate-command (file buf &optional rev)     OK
;; - annotate-time ()                              OK
;; - annotate-current-time ()                      NOT NEEDED
;; - annotate-extract-revision-at-line ()          OK
;; TAG SYSTEM
;; - create-tag (dir name branchp)                 OK
;; - retrieve-tag (dir name update)                OK
;; MISCELLANEOUS
;; - make-version-backups-p (file)                 NOT NEEDED
;; - repository-hostname (dirname)                 NOT NEEDED
;; - previous-revision (file rev)                  OK
;; - next-revision (file rev)                      OK
;; - check-headers ()                              COULD BE SUPPORTED
;; - clear-headers ()                              NOT NEEDED
;; - delete-file (file)                            OK
;; - rename-file (old new)                         OK
;; - find-file-hook ()                             NOT NEEDED

(eval-when-compile
  (require 'cl)
  (require 'vc)
  (require 'vc-dir)
  (require 'grep))

(defgroup vc-git nil
  "VC Git backend."
  :version "24.1"
  :group 'vc)

(defcustom vc-git-diff-switches t
  "String or list of strings specifying switches for Git diff under VC.
If nil, use the value of `vc-diff-switches'.  If t, use no switches."
  :type '(choice (const :tag "Unspecified" nil)
		 (const :tag "None" t)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List" :value ("") string))
  :version "23.1"
  :group 'vc-git)

(defcustom vc-git-program "git"
  "Name of the Git executable (excluding any arguments)."
  :version "24.1"
  :type 'string
  :group 'vc-git)

(defcustom vc-git-root-log-format
  '("%d%h..: %an %ad %s"
    ;; The first shy group matches the characters drawn by --graph.
    ;; We use numbered groups because `log-view-message-re' wants the
    ;; revision number to be group 1.
    "^\\(?:[*/\\| ]+ \\)?\\(?2: ([^)]+)\\)?\\(?1:[0-9a-z]+\\)..: \
\\(?3:.*?\\)[ \t]+\\(?4:[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)"
    ((1 'log-view-message-face)
     (2 'change-log-list nil lax)
     (3 'change-log-name)
     (4 'change-log-date)))
  "Git log format for `vc-print-root-log'.
This should be a list (FORMAT REGEXP KEYWORDS), where FORMAT is a
format string (which is passed to \"git log\" via the argument
\"--pretty=tformat:FORMAT\"), REGEXP is a regular expression
matching the resulting Git log output, and KEYWORDS is a list of
`font-lock-keywords' for highlighting the Log View buffer."
  :type '(list string string (repeat sexp))
  :group 'vc-git
  :version "24.1")

(defvar vc-git-commits-coding-system 'utf-8
  "Default coding system for git commits.")

;; History of Git commands.
(defvar vc-git-history nil)

;;; BACKEND PROPERTIES

(defun vc-git-revision-granularity () 'repository)
(defun vc-git-checkout-model (files) 'implicit)

;;; STATE-QUERYING FUNCTIONS

;;;###autoload (defun vc-git-registered (file)
;;;###autoload   "Return non-nil if FILE is registered with git."
;;;###autoload   (if (vc-find-root file ".git")       ; Short cut.
;;;###autoload       (progn
;;;###autoload         (load "vc-git")
;;;###autoload         (vc-git-registered file))))

(defun vc-git-registered (file)
  "Check whether FILE is registered with git."
  (let ((dir (vc-git-root file)))
    (when dir
      (with-temp-buffer
	(let* (process-file-side-effects
	       ;; Do not use the `file-name-directory' here: git-ls-files
	       ;; sometimes fails to return the correct status for relative
	       ;; path specs.
	       ;; See also: http://marc.info/?l=git&m=125787684318129&w=2
	       (name (file-relative-name file dir))
	       (str (ignore-errors
		     (cd dir)
		     (vc-git--out-ok "ls-files" "-c" "-z" "--" name)
		     ;; If result is empty, use ls-tree to check for deleted
                     ;; file.
		     (when (eq (point-min) (point-max))
		       (vc-git--out-ok "ls-tree" "--name-only" "-z" "HEAD"
                                       "--" name))
		     (buffer-string))))
	  (and str
	       (> (length str) (length name))
	       (string= (substring str 0 (1+ (length name)))
			(concat name "\0"))))))))

(defun vc-git--state-code (code)
  "Convert from a string to a added/deleted/modified state."
  (case (string-to-char code)
    (?M 'edited)
    (?A 'added)
    (?D 'removed)
    (?U 'edited)     ;; FIXME
    (?T 'edited)))   ;; FIXME

(defun vc-git-state (file)
  "Git-specific version of `vc-state'."
  ;; FIXME: This can't set 'ignored or 'conflict yet
  ;; The 'ignored state could be detected with `git ls-files -i -o
  ;; --exclude-standard` It also can't set 'needs-update or
  ;; 'needs-merge. The rough equivalent would be that upstream branch
  ;; for current branch is in fast-forward state i.e. current branch
  ;; is direct ancestor of corresponding upstream branch, and the file
  ;; was modified upstream.  But we can't check that without a network
  ;; operation.
  (if (not (vc-git-registered file))
      'unregistered
    (let ((diff (vc-git--run-command-string
                 file "diff-index" "-p" "--raw" "-z" "HEAD" "--")))
      (if (and diff
	       (string-match ":[0-7]\\{6\\} [0-7]\\{6\\} [0-9a-f]\\{40\\} [0-9a-f]\\{40\\} \\([ADMUT]\\)\0[^\0]+\0\\(.*\n.\\)?"
			     diff))
          (let ((diff-letter (match-string 1 diff)))
            (if (not (match-beginning 2))
                ;; Empty diff: file contents is the same as the HEAD
                ;; revision, but timestamps are different (eg, file
                ;; was "touch"ed).  Update timestamp in index:
                (prog1 'up-to-date
                  (vc-git--call nil "add" "--refresh" "--"
                                (file-relative-name file)))
              (vc-git--state-code diff-letter)))
	(if (vc-git--empty-db-p) 'added 'up-to-date)))))

(defun vc-git-working-revision (file)
  "Git-specific version of `vc-working-revision'."
  (let* (process-file-side-effects
	 (str (with-output-to-string
		(with-current-buffer standard-output
		  (vc-git--out-ok "symbolic-ref" "HEAD")))))
    (if (string-match "^\\(refs/heads/\\)?\\(.+\\)$" str)
        (match-string 2 str)
      str)))

(defun vc-git-workfile-unchanged-p (file)
  (eq 'up-to-date (vc-git-state file)))

(defun vc-git-mode-line-string (file)
  "Return string for placement into the modeline for FILE."
  (let* ((branch (vc-git-working-revision file))
         (def-ml (vc-default-mode-line-string 'Git file))
         (help-echo (get-text-property 0 'help-echo def-ml)))
    (if (zerop (length branch))
        (propertize
         (concat def-ml "!")
         'help-echo (concat help-echo "\nNo current branch (detached HEAD)"))
      (propertize def-ml
                  'help-echo (concat help-echo "\nCurrent branch: " branch)))))

(defstruct (vc-git-extra-fileinfo
            (:copier nil)
            (:constructor vc-git-create-extra-fileinfo
                          (old-perm new-perm &optional rename-state orig-name))
            (:conc-name vc-git-extra-fileinfo->))
  old-perm new-perm   ;; Permission flags.
  rename-state        ;; Rename or copy state.
  orig-name)          ;; Original name for renames or copies.

(defun vc-git-escape-file-name (name)
  "Escape a file name if necessary."
  (if (string-match "[\n\t\"\\]" name)
      (concat "\""
              (mapconcat (lambda (c)
                   (case c
                     (?\n "\\n")
                     (?\t "\\t")
                     (?\\ "\\\\")
                     (?\" "\\\"")
                     (t (char-to-string c))))
                 name "")
              "\"")
    name))

(defun vc-git-file-type-as-string (old-perm new-perm)
  "Return a string describing the file type based on its permissions."
  (let* ((old-type (lsh (or old-perm 0) -9))
	 (new-type (lsh (or new-perm 0) -9))
	 (str (case new-type
		(?\100  ;; File.
		 (case old-type
		   (?\100 nil)
		   (?\120 "   (type change symlink -> file)")
		   (?\160 "   (type change subproject -> file)")))
		 (?\120  ;; Symlink.
		  (case old-type
		    (?\100 "   (type change file -> symlink)")
		    (?\160 "   (type change subproject -> symlink)")
		    (t "   (symlink)")))
		  (?\160  ;; Subproject.
		   (case old-type
		     (?\100 "   (type change file -> subproject)")
		     (?\120 "   (type change symlink -> subproject)")
		     (t "   (subproject)")))
                  (?\110 nil)  ;; Directory (internal, not a real git state).
		  (?\000  ;; Deleted or unknown.
		   (case old-type
		     (?\120 "   (symlink)")
		     (?\160 "   (subproject)")))
		  (t (format "   (unknown type %o)" new-type)))))
    (cond (str (propertize str 'face 'font-lock-comment-face))
          ((eq new-type ?\110) "/")
          (t ""))))

(defun vc-git-rename-as-string (state extra)
  "Return a string describing the copy or rename associated with INFO,
or an empty string if none."
  (let ((rename-state (when extra
			(vc-git-extra-fileinfo->rename-state extra))))
    (if rename-state
        (propertize
         (concat "   ("
                 (if (eq rename-state 'copy) "copied from "
                   (if (eq state 'added) "renamed from "
                     "renamed to "))
                 (vc-git-escape-file-name
                  (vc-git-extra-fileinfo->orig-name extra))
                 ")")
         'face 'font-lock-comment-face)
      "")))

(defun vc-git-permissions-as-string (old-perm new-perm)
  "Format a permission change as string."
  (propertize
   (if (or (not old-perm)
           (not new-perm)
           (eq 0 (logand ?\111 (logxor old-perm new-perm))))
       "  "
     (if (eq 0 (logand ?\111 old-perm)) "+x" "-x"))
  'face 'font-lock-type-face))

(defun vc-git-dir-printer (info)
  "Pretty-printer for the vc-dir-fileinfo structure."
  (let* ((isdir (vc-dir-fileinfo->directory info))
	 (state (if isdir "" (vc-dir-fileinfo->state info)))
         (extra (vc-dir-fileinfo->extra info))
         (old-perm (when extra (vc-git-extra-fileinfo->old-perm extra)))
         (new-perm (when extra (vc-git-extra-fileinfo->new-perm extra))))
    (insert
     "  "
     (propertize (format "%c" (if (vc-dir-fileinfo->marked info) ?* ? ))
                 'face 'font-lock-type-face)
     "  "
     (propertize
      (format "%-12s" state)
      'face (cond ((eq state 'up-to-date) 'font-lock-builtin-face)
		  ((eq state 'missing) 'font-lock-warning-face)
		  (t 'font-lock-variable-name-face))
      'mouse-face 'highlight)
     "  " (vc-git-permissions-as-string old-perm new-perm)
     "    "
     (propertize (vc-git-escape-file-name (vc-dir-fileinfo->name info))
                 'face (if isdir 'font-lock-comment-delimiter-face
                         'font-lock-function-name-face)
		 'help-echo
		 (if isdir
		     "Directory\nVC operations can be applied to it\nmouse-3: Pop-up menu"
		   "File\nmouse-3: Pop-up menu")
		 'keymap vc-dir-filename-mouse-map
		 'mouse-face 'highlight)
     (vc-git-file-type-as-string old-perm new-perm)
     (vc-git-rename-as-string state extra))))

(defun vc-git-after-dir-status-stage (stage files update-function)
  "Process sentinel for the various dir-status stages."
  (let (next-stage result)
    (goto-char (point-min))
    (case stage
      (update-index
       (setq next-stage (if (vc-git--empty-db-p) 'ls-files-added
                          (if files 'ls-files-up-to-date 'diff-index))))
      (ls-files-added
       (setq next-stage 'ls-files-unknown)
       (while (re-search-forward "\\([0-7]\\{6\\}\\) [0-9a-f]\\{40\\} 0\t\\([^\0]+\\)\0" nil t)
         (let ((new-perm (string-to-number (match-string 1) 8))
               (name (match-string 2)))
           (push (list name 'added (vc-git-create-extra-fileinfo 0 new-perm))
                 result))))
      (ls-files-up-to-date
       (setq next-stage 'diff-index)
       (while (re-search-forward "\\([0-7]\\{6\\}\\) [0-9a-f]\\{40\\} 0\t\\([^\0]+\\)\0" nil t)
         (let ((perm (string-to-number (match-string 1) 8))
               (name (match-string 2)))
           (push (list name 'up-to-date
                       (vc-git-create-extra-fileinfo perm perm))
                 result))))
      (ls-files-unknown
       (when files (setq next-stage 'ls-files-ignored))
       (while (re-search-forward "\\([^\0]*?\\)\0" nil t 1)
         (push (list (match-string 1) 'unregistered
                     (vc-git-create-extra-fileinfo 0 0))
               result)))
      (ls-files-ignored
       (while (re-search-forward "\\([^\0]*?\\)\0" nil t 1)
         (push (list (match-string 1) 'ignored
                     (vc-git-create-extra-fileinfo 0 0))
               result)))
      (diff-index
       (setq next-stage 'ls-files-unknown)
       (while (re-search-forward
               ":\\([0-7]\\{6\\}\\) \\([0-7]\\{6\\}\\) [0-9a-f]\\{40\\} [0-9a-f]\\{40\\} \\(\\([ADMUT]\\)\0\\([^\0]+\\)\\|\\([CR]\\)[0-9]*\0\\([^\0]+\\)\0\\([^\0]+\\)\\)\0"
               nil t 1)
         (let ((old-perm (string-to-number (match-string 1) 8))
               (new-perm (string-to-number (match-string 2) 8))
               (state (or (match-string 4) (match-string 6)))
               (name (or (match-string 5) (match-string 7)))
               (new-name (match-string 8)))
           (if new-name  ; Copy or rename.
               (if (eq ?C (string-to-char state))
                   (push (list new-name 'added
                               (vc-git-create-extra-fileinfo old-perm new-perm
                                                             'copy name))
                         result)
                 (push (list name 'removed
                             (vc-git-create-extra-fileinfo 0 0
                                                           'rename new-name))
                       result)
                 (push (list new-name 'added
                             (vc-git-create-extra-fileinfo old-perm new-perm
                                                           'rename name))
                       result))
             (push (list name (vc-git--state-code state)
                         (vc-git-create-extra-fileinfo old-perm new-perm))
                   result))))))
    (when result
      (setq result (nreverse result))
      (when files
        (dolist (entry result) (setq files (delete (car entry) files)))
        (unless files (setq next-stage nil))))
    (when (or result (not next-stage))
      (funcall update-function result next-stage))
    (when next-stage
      (vc-git-dir-status-goto-stage next-stage files update-function))))

(defun vc-git-dir-status-goto-stage (stage files update-function)
  (erase-buffer)
  (case stage
    (update-index
     (if files
         (vc-git-command (current-buffer) 'async files "add" "--refresh" "--")
       (vc-git-command (current-buffer) 'async nil
                       "update-index" "--refresh")))
    (ls-files-added
     (vc-git-command (current-buffer) 'async files
                     "ls-files" "-z" "-c" "-s" "--"))
    (ls-files-up-to-date
     (vc-git-command (current-buffer) 'async files
                     "ls-files" "-z" "-c" "-s" "--"))
    (ls-files-unknown
     (vc-git-command (current-buffer) 'async files
                     "ls-files" "-z" "-o" "--directory"
                     "--no-empty-directory" "--exclude-standard" "--"))
    (ls-files-ignored
     (vc-git-command (current-buffer) 'async files
                     "ls-files" "-z" "-o" "-i" "--directory"
                     "--no-empty-directory" "--exclude-standard" "--"))
    ;; --relative added in Git 1.5.5.
    (diff-index
     (vc-git-command (current-buffer) 'async files
                     "diff-index" "--relative" "-z" "-M" "HEAD" "--")))
  (vc-exec-after
   `(vc-git-after-dir-status-stage ',stage  ',files ',update-function)))

(defun vc-git-dir-status (dir update-function)
  "Return a list of (FILE STATE EXTRA) entries for DIR."
  ;; Further things that would have to be fixed later:
  ;; - how to handle unregistered directories
  ;; - how to support vc-dir on a subdir of the project tree
  (vc-git-dir-status-goto-stage 'update-index nil update-function))

(defun vc-git-dir-status-files (dir files default-state update-function)
  "Return a list of (FILE STATE EXTRA) entries for FILES in DIR."
  (vc-git-dir-status-goto-stage 'update-index files update-function))

(defvar vc-git-stash-map
  (let ((map (make-sparse-keymap)))
    ;; Turn off vc-dir marking
    (define-key map [mouse-2] 'ignore)

    (define-key map [down-mouse-3] 'vc-git-stash-menu)
    (define-key map "\C-k" 'vc-git-stash-delete-at-point)
    (define-key map "=" 'vc-git-stash-show-at-point)
    (define-key map "\C-m" 'vc-git-stash-show-at-point)
    (define-key map "A" 'vc-git-stash-apply-at-point)
    (define-key map "P" 'vc-git-stash-pop-at-point)
    (define-key map "S" 'vc-git-stash-snapshot)
    map))

(defvar vc-git-stash-menu-map
  (let ((map (make-sparse-keymap "Git Stash")))
    (define-key map [de]
      '(menu-item "Delete Stash" vc-git-stash-delete-at-point
		  :help "Delete the current stash"))
    (define-key map [ap]
      '(menu-item "Apply Stash" vc-git-stash-apply-at-point
		  :help "Apply the current stash and keep it in the stash list"))
    (define-key map [po]
      '(menu-item "Apply and Remove Stash (Pop)" vc-git-stash-pop-at-point
		  :help "Apply the current stash and remove it"))
    (define-key map [sh]
      '(menu-item "Show Stash" vc-git-stash-show-at-point
		  :help "Show the contents of the current stash"))
    map))

(defun vc-git-dir-extra-headers (dir)
  (let ((str (with-output-to-string
               (with-current-buffer standard-output
                 (vc-git--out-ok "symbolic-ref" "HEAD"))))
	(stash (vc-git-stash-list))
	(stash-help-echo "Use M-x vc-git-stash to create stashes.")
	branch remote remote-url)
    (if (string-match "^\\(refs/heads/\\)?\\(.+\\)$" str)
	(progn
	  (setq branch (match-string 2 str))
	  (setq remote
		(with-output-to-string
		  (with-current-buffer standard-output
		    (vc-git--out-ok "config"
                                    (concat "branch." branch ".remote")))))
	  (when (string-match "\\([^\n]+\\)" remote)
	    (setq remote (match-string 1 remote)))
	  (when remote
	    (setq remote-url
		  (with-output-to-string
		    (with-current-buffer standard-output
		      (vc-git--out-ok "config"
                                      (concat "remote." remote ".url"))))))
	  (when (string-match "\\([^\n]+\\)" remote-url)
	    (setq remote-url (match-string 1 remote-url))))
      (setq branch "not (detached HEAD)"))
    ;; FIXME: maybe use a different face when nothing is stashed.
    (concat
     (propertize "Branch     : " 'face 'font-lock-type-face)
     (propertize branch
		 'face 'font-lock-variable-name-face)
     (when remote
       (concat
	"\n"
	(propertize "Remote     : " 'face 'font-lock-type-face)
	(propertize remote-url
		    'face 'font-lock-variable-name-face)))
     "\n"
     (if stash
       (concat
	(propertize "Stash      :\n" 'face 'font-lock-type-face
		    'help-echo stash-help-echo)
	(mapconcat
	 (lambda (x)
	   (propertize x
		       'face 'font-lock-variable-name-face
		       'mouse-face 'highlight
		       'help-echo "mouse-3: Show stash menu\nRET: Show stash\nA: Apply stash\nP: Apply and remove stash (pop)\nC-k: Delete stash"
		       'keymap vc-git-stash-map))
	 stash "\n"))
       (concat
	(propertize "Stash      : " 'face 'font-lock-type-face
		    'help-echo stash-help-echo)
	(propertize "Nothing stashed"
		    'help-echo stash-help-echo
		    'face 'font-lock-variable-name-face))))))

(defun vc-git-branches ()
  "Return the existing branches, as a list of strings.
The car of the list is the current branch."
  (with-temp-buffer
    (call-process vc-git-program nil t nil "branch")
    (goto-char (point-min))
    (let (current-branch branches)
      (while (not (eobp))
	(when (looking-at "^\\([ *]\\) \\(.+\\)$")
	  (if (string-equal (match-string 1) "*")
	      (setq current-branch (match-string 2))
	    (push (match-string 2) branches)))
	(forward-line 1))
      (cons current-branch (nreverse branches)))))

;;; STATE-CHANGING FUNCTIONS

(defun vc-git-create-repo ()
  "Create a new Git repository."
  (vc-git-command nil 0 nil "init"))

(defun vc-git-register (files &optional rev comment)
  "Register FILES into the git version-control system."
  (let (flist dlist)
    (dolist (crt files)
      (if (file-directory-p crt)
	  (push crt dlist)
	(push crt flist)))
    (when flist
      (vc-git-command nil 0 flist "update-index" "--add" "--"))
    (when dlist
      (vc-git-command nil 0 dlist "add"))))

(defalias 'vc-git-responsible-p 'vc-git-root)

(defun vc-git-unregister (file)
  (vc-git-command nil 0 file "rm" "-f" "--cached" "--"))

(declare-function log-edit-extract-headers "log-edit" (headers string))

(defun vc-git-checkin (files rev comment)
  (let ((coding-system-for-write vc-git-commits-coding-system))
    (apply 'vc-git-command nil 0 files
	   (nconc (list "commit" "-m")
                  (log-edit-extract-headers '(("Author" . "--author")
					      ("Date" . "--date"))
                                            comment)
                  (list "--only" "--")))))

(defun vc-git-find-revision (file rev buffer)
  (let* (process-file-side-effects
	 (coding-system-for-read 'binary)
	 (coding-system-for-write 'binary)
	 (fullname
	  (let ((fn (vc-git--run-command-string
		     file "ls-files" "-z" "--full-name" "--")))
	    ;; ls-files does not return anything when looking for a
	    ;; revision of a file that has been renamed or removed.
	    (if (string= fn "")
		(file-relative-name file (vc-git-root default-directory))
	      (substring fn 0 -1)))))
    (vc-git-command
     buffer 0
     nil
     "cat-file" "blob" (concat (if rev rev "HEAD") ":" fullname))))

(defun vc-git-checkout (file &optional editable rev)
  (vc-git-command nil 0 file "checkout" (or rev "HEAD")))

(defun vc-git-revert (file &optional contents-done)
  "Revert FILE to the version stored in the git repository."
  (if contents-done
      (vc-git-command nil 0 file "update-index" "--")
    (vc-git-command nil 0 file "reset" "-q" "--")
    (vc-git-command nil nil file "checkout" "-q" "--")))

(defun vc-git-pull (prompt)
  "Pull changes into the current Git branch.
Normally, this runs \"git pull\".  If PROMPT is non-nil, prompt
for the Git command to run."
  (let* ((root (vc-git-root default-directory))
	 (buffer (format "*vc-git : %s*" (expand-file-name root)))
	 (command "pull")
	 (git-program vc-git-program)
	 args)
    ;; If necessary, prompt for the exact command.
    (when prompt
      (setq args (split-string
		  (read-shell-command "Git pull command: "
                                      (format "%s pull" git-program)
				      'vc-git-history)
		  " " t))
      (setq git-program (car  args)
	    command     (cadr args)
	    args        (cddr args)))
    (apply 'vc-do-async-command buffer root git-program command args)
    (vc-set-async-update buffer)))

(defun vc-git-merge-branch ()
  "Merge changes into the current Git branch.
This prompts for a branch to merge from."
  (let* ((root (vc-git-root default-directory))
	 (buffer (format "*vc-git : %s*" (expand-file-name root)))
	 (branches (cdr (vc-git-branches)))
	 (merge-source
	  (completing-read "Merge from branch: "
			   (if (or (member "FETCH_HEAD" branches)
				   (not (file-readable-p
					 (expand-file-name ".git/FETCH_HEAD"
							   root))))
			       branches
			     (cons "FETCH_HEAD" branches))
			   nil t)))
    (apply 'vc-do-async-command buffer root vc-git-program "merge"
	   (list merge-source))
    (vc-set-async-update buffer)))

;;; HISTORY FUNCTIONS

(defun vc-git-print-log (files buffer &optional shortlog start-revision limit)
  "Get change log associated with FILES.
Note that using SHORTLOG requires at least Git version 1.5.6,
for the --graph option."
  (let ((coding-system-for-read vc-git-commits-coding-system))
    ;; `vc-do-command' creates the buffer, but we need it before running
    ;; the command.
    (vc-setup-buffer buffer)
    ;; If the buffer exists from a previous invocation it might be
    ;; read-only.
    (let ((inhibit-read-only t))
      (with-current-buffer
          buffer
	(apply 'vc-git-command buffer
	       'async files
	       (append
		'("log" "--no-color")
		(when shortlog
		  `("--graph" "--decorate" "--date=short"
                    ,(format "--pretty=tformat:%s"
			     (car vc-git-root-log-format))
		    "--abbrev-commit"))
		(when limit (list "-n" (format "%s" limit)))
		(when start-revision (list start-revision))
		'("--")))))))

(defun vc-git-log-outgoing (buffer remote-location)
  (interactive)
  (vc-git-command
   buffer 0 nil
   "log"
   "--no-color" "--graph" "--decorate" "--date=short"
   (format "--pretty=tformat:%s" (car vc-git-root-log-format))
   "--abbrev-commit"
   (concat (if (string= remote-location "")
	       "@{upstream}"
	     remote-location)
	   "..HEAD")))

(defun vc-git-log-incoming (buffer remote-location)
  (interactive)
  (vc-git-command nil 0 nil "fetch")
  (vc-git-command
   buffer 0 nil
   "log"
   "--no-color" "--graph" "--decorate" "--date=short"
   (format "--pretty=tformat:%s" (car vc-git-root-log-format))
   "--abbrev-commit"
   (concat "HEAD.." (if (string= remote-location "")
			"@{upstream}"
		      remote-location))))

(defvar log-view-message-re)
(defvar log-view-file-re)
(defvar log-view-font-lock-keywords)
(defvar log-view-per-file-logs)
(defvar log-view-expanded-log-entry-function)

(define-derived-mode vc-git-log-view-mode log-view-mode "Git-Log-View"
  (require 'add-log) ;; We need the faces add-log.
  ;; Don't have file markers, so use impossible regexp.
  (set (make-local-variable 'log-view-file-re) "\\`a\\`")
  (set (make-local-variable 'log-view-per-file-logs) nil)
  (set (make-local-variable 'log-view-message-re)
       (if (not (eq vc-log-view-type 'long))
	   (cadr vc-git-root-log-format)
	 "^commit *\\([0-9a-z]+\\)"))
  ;; Allow expanding short log entries
  (when (eq vc-log-view-type 'short)
    (setq truncate-lines t)
    (set (make-local-variable 'log-view-expanded-log-entry-function)
	 'vc-git-expanded-log-entry))
  (set (make-local-variable 'log-view-font-lock-keywords)
       (if (not (eq vc-log-view-type 'long))
	   (list (cons (nth 1 vc-git-root-log-format)
		       (nth 2 vc-git-root-log-format)))
	 (append
	  `((,log-view-message-re (1 'change-log-acknowledgement)))
	  ;; Handle the case:
	  ;; user: foo@bar
	  '(("^Author:[ \t]+\\([A-Za-z0-9_.+-]+@[A-Za-z0-9_.-]+\\)"
	     (1 'change-log-email))
	    ;; Handle the case:
	    ;; user: FirstName LastName <foo@bar>
	    ("^Author:[ \t]+\\([^<(]+?\\)[ \t]*[(<]\\([A-Za-z0-9_.+-]+@[A-Za-z0-9_.-]+\\)[>)]"
	     (1 'change-log-name)
	     (2 'change-log-email))
	    ("^ +\\(?:\\(?:[Aa]cked\\|[Ss]igned-[Oo]ff\\)-[Bb]y:\\)[ \t]+\\([A-Za-z0-9_.+-]+@[A-Za-z0-9_.-]+\\)"
	     (1 'change-log-name))
	    ("^ +\\(?:\\(?:[Aa]cked\\|[Ss]igned-[Oo]ff\\)-[Bb]y:\\)[ \t]+\\([^<(]+?\\)[ \t]*[(<]\\([A-Za-z0-9_.+-]+@[A-Za-z0-9_.-]+\\)[>)]"
	     (1 'change-log-name)
	     (2 'change-log-email))
	    ("^Merge: \\([0-9a-z]+\\) \\([0-9a-z]+\\)"
	     (1 'change-log-acknowledgement)
	     (2 'change-log-acknowledgement))
	    ("^Date:   \\(.+\\)" (1 'change-log-date))
	    ("^summary:[ \t]+\\(.+\\)" (1 'log-view-message)))))))


(defun vc-git-show-log-entry (revision)
  "Move to the log entry for REVISION.
REVISION may have the form BRANCH, BRANCH~N,
or BRANCH^ (where \"^\" can be repeated)."
  (goto-char (point-min))
  (prog1
      (when revision
        (search-forward
         (format "\ncommit %s" revision) nil t
         (cond ((string-match "~\\([0-9]\\)\\'" revision)
                (1+ (string-to-number (match-string 1 revision))))
               ((string-match "\\^+\\'" revision)
                (1+ (length (match-string 0 revision))))
               (t nil))))
    (beginning-of-line)))

(defun vc-git-expanded-log-entry (revision)
  (with-temp-buffer
    (apply 'vc-git-command t nil nil (list "log" revision "-1"))
    (goto-char (point-min))
    (unless (eobp)
      ;; Indent the expanded log entry.
      (indent-region (point-min) (point-max) 2)
      (buffer-string))))

(defun vc-git-diff (files &optional rev1 rev2 buffer)
  "Get a difference report using Git between two revisions of FILES."
  (let (process-file-side-effects)
    (apply #'vc-git-command (or buffer "*vc-diff*") 1 files
	   (if (and rev1 rev2) "diff-tree" "diff-index")
	   "--exit-code"
	   (append (vc-switches 'git 'diff)
		   (list "-p" (or rev1 "HEAD") rev2 "--")))))

(defun vc-git-revision-table (files)
  ;; What about `files'?!?  --Stef
  (let (process-file-side-effects
	(table (list "HEAD")))
    (with-temp-buffer
      (vc-git-command t nil nil "for-each-ref" "--format=%(refname)")
      (goto-char (point-min))
      (while (re-search-forward "^refs/\\(heads\\|tags\\|remotes\\)/\\(.*\\)$"
                                nil t)
        (push (match-string 2) table)))
    table))

(defun vc-git-revision-completion-table (files)
  (lexical-let ((files files)
                table)
    (setq table (lazy-completion-table
                 table (lambda () (vc-git-revision-table files))))
    table))

(defun vc-git-annotate-command (file buf &optional rev)
  (let ((name (file-relative-name file)))
    (vc-git-command buf 'async nil "blame" "--date=iso" "-C" "-C" rev "--" name)))

(declare-function vc-annotate-convert-time "vc-annotate" (time))

(defun vc-git-annotate-time ()
  (and (re-search-forward "[0-9a-f]+[^()]+(.* \\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\) \\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\) \\([-+0-9]+\\) +[0-9]+) " nil t)
       (vc-annotate-convert-time
        (apply #'encode-time (mapcar (lambda (match)
                                       (string-to-number (match-string match)))
                                     '(6 5 4 3 2 1 7))))))

(defun vc-git-annotate-extract-revision-at-line ()
  (save-excursion
    (move-beginning-of-line 1)
    (when (looking-at "\\([0-9a-f^][0-9a-f]+\\) \\(\\([^(]+\\) \\)?")
      (let ((revision (match-string-no-properties 1)))
	(if (match-beginning 2)
	    (let ((fname (match-string-no-properties 3)))
	      ;; Remove trailing whitespace from the file name.
	      (when (string-match " +\\'" fname)
		(setq fname (substring fname 0 (match-beginning 0))))
	      (cons revision
		    (expand-file-name fname (vc-git-root default-directory))))
	  revision)))))

;;; TAG SYSTEM

(defun vc-git-create-tag (dir name branchp)
  (let ((default-directory dir))
    (and (vc-git-command nil 0 nil "update-index" "--refresh")
         (if branchp
             (vc-git-command nil 0 nil "checkout" "-b" name)
           (vc-git-command nil 0 nil "tag" name)))))

(defun vc-git-retrieve-tag (dir name update)
  (let ((default-directory dir))
    (vc-git-command nil 0 nil "checkout" name)
    ;; FIXME: update buffers if `update' is true
    ))


;;; MISCELLANEOUS

(defun vc-git-previous-revision (file rev)
  "Git-specific version of `vc-previous-revision'."
  (if file
      (let* ((fname (file-relative-name file))
             (prev-rev (with-temp-buffer
                         (and
                          (vc-git--out-ok "rev-list" "-2" rev "--" fname)
                          (goto-char (point-max))
                          (bolp)
                          (zerop (forward-line -1))
                          (not (bobp))
                          (buffer-substring-no-properties
                           (point)
                           (1- (point-max)))))))
        (or (vc-git-symbolic-commit prev-rev) prev-rev))
    (with-temp-buffer
      (and
       (vc-git--out-ok "rev-parse" (concat rev "^"))
       (buffer-substring-no-properties (point-min) (+ (point-min) 40))))))

(defun vc-git-next-revision (file rev)
  "Git-specific version of `vc-next-revision'."
  (let* ((default-directory (file-name-directory
			     (expand-file-name file)))
         (file (file-name-nondirectory file))
         (current-rev
          (with-temp-buffer
            (and
             (vc-git--out-ok "rev-list" "-1" rev "--" file)
             (goto-char (point-max))
             (bolp)
             (zerop (forward-line -1))
             (bobp)
             (buffer-substring-no-properties
              (point)
              (1- (point-max))))))
         (next-rev
          (and current-rev
               (with-temp-buffer
                 (and
                  (vc-git--out-ok "rev-list" "HEAD" "--" file)
                  (goto-char (point-min))
                  (search-forward current-rev nil t)
                  (zerop (forward-line -1))
                  (buffer-substring-no-properties
                   (point)
                   (progn (forward-line 1) (1- (point)))))))))
    (or (vc-git-symbolic-commit next-rev) next-rev)))

(defun vc-git-delete-file (file)
  (vc-git-command nil 0 file "rm" "-f" "--"))

(defun vc-git-rename-file (old new)
  (vc-git-command nil 0 (list old new) "mv" "-f" "--"))

(defvar vc-git-extra-menu-map
  (let ((map (make-sparse-keymap)))
    (define-key map [git-grep]
      '(menu-item "Git grep..." vc-git-grep
		  :help "Run the `git grep' command"))
    (define-key map [git-sn]
      '(menu-item "Stash a Snapshot" vc-git-stash-snapshot
		  :help "Stash the current state of the tree and keep the current state"))
    (define-key map [git-st]
      '(menu-item "Create Stash..." vc-git-stash
		  :help "Stash away changes"))
    (define-key map [git-ss]
      '(menu-item "Show Stash..." vc-git-stash-show
		  :help "Show stash contents"))
    map))

(defun vc-git-extra-menu () vc-git-extra-menu-map)

(defun vc-git-extra-status-menu () vc-git-extra-menu-map)

(defun vc-git-root (file)
  (vc-find-root file ".git"))

;; Derived from `lgrep'.
(defun vc-git-grep (regexp &optional files dir)
  "Run git grep, searching for REGEXP in FILES in directory DIR.
The search is limited to file names matching shell pattern FILES.
FILES may use abbreviations defined in `grep-files-aliases', e.g.
entering `ch' is equivalent to `*.[ch]'.

With \\[universal-argument] prefix, you can edit the constructed shell command line
before it is executed.
With two \\[universal-argument] prefixes, directly edit and run `grep-command'.

Collect output in a buffer.  While git grep runs asynchronously, you
can use \\[next-error] (M-x next-error), or \\<grep-mode-map>\\[compile-goto-error] \
in the grep output buffer,
to go to the lines where grep found matches.

This command shares argument histories with \\[rgrep] and \\[grep]."
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((equal current-prefix-arg '(16))
       (list (read-from-minibuffer "Run: " "git grep"
				   nil nil 'grep-history)
	     nil))
      (t (let* ((regexp (grep-read-regexp))
		(files (grep-read-files regexp))
		(dir (read-directory-name "In directory: "
					  nil default-directory t)))
	   (list regexp files dir))))))
  (require 'grep)
  (when (and (stringp regexp) (> (length regexp) 0))
    (let ((command regexp))
      (if (null files)
	  (if (string= command "git grep")
	      (setq command nil))
	(setq dir (file-name-as-directory (expand-file-name dir)))
	(setq command
	      (grep-expand-template "git grep -n -e <R> -- <F>"
                                    regexp files))
	(when command
	  (if (equal current-prefix-arg '(4))
	      (setq command
		    (read-from-minibuffer "Confirm: "
					  command nil nil 'grep-history))
	    (add-to-history 'grep-history command))))
      (when command
	(let ((default-directory dir)
	      (compilation-environment (cons "PAGER=" compilation-environment)))
	  ;; Setting process-setup-function makes exit-message-function work
	  ;; even when async processes aren't supported.
	  (compilation-start command 'grep-mode))
	(if (eq next-error-last-buffer (current-buffer))
	    (setq default-directory dir))))))

(defun vc-git-stash (name)
  "Create a stash."
  (interactive "sStash name: ")
  (let ((root (vc-git-root default-directory)))
    (when root
      (vc-git--call nil "stash" "save" name)
      (vc-resynch-buffer root t t))))

(defun vc-git-stash-show (name)
  "Show the contents of stash NAME."
  (interactive "sStash name: ")
  (vc-setup-buffer "*vc-git-stash*")
  (vc-git-command "*vc-git-stash*" 'async nil "stash" "show" "-p" name)
  (set-buffer "*vc-git-stash*")
  (diff-mode)
  (setq buffer-read-only t)
  (pop-to-buffer (current-buffer)))

(defun vc-git-stash-apply (name)
  "Apply stash NAME."
  (interactive "sApply stash: ")
  (vc-git-command "*vc-git-stash*" 0 nil "stash" "apply" "-q" name)
  (vc-resynch-buffer (vc-git-root default-directory) t t))

(defun vc-git-stash-pop (name)
  "Pop stash NAME."
  (interactive "sPop stash: ")
  (vc-git-command "*vc-git-stash*" 0 nil "stash" "pop" "-q" name)
  (vc-resynch-buffer (vc-git-root default-directory) t t))

(defun vc-git-stash-snapshot ()
  "Create a stash with the current tree state."
  (interactive)
  (vc-git--call nil "stash" "save"
		(let ((ct (current-time)))
		  (concat
		   (format-time-string "Snapshot on %Y-%m-%d" ct)
		   (format-time-string " at %H:%M" ct))))
  (vc-git-command "*vc-git-stash*" 0 nil "stash" "apply" "-q" "stash@{0}")
  (vc-resynch-buffer (vc-git-root default-directory) t t))

(defun vc-git-stash-list ()
  (delete
   ""
   (split-string
    (replace-regexp-in-string
     "^stash@" "             " (vc-git--run-command-string nil "stash" "list"))
    "\n")))

(defun vc-git-stash-get-at-point (point)
  (save-excursion
    (goto-char point)
    (beginning-of-line)
    (if (looking-at "^ +\\({[0-9]+}\\):")
	(match-string 1)
      (error "Cannot find stash at point"))))

(defun vc-git-stash-delete-at-point ()
  (interactive)
  (let ((stash (vc-git-stash-get-at-point (point))))
    (when (y-or-n-p (format "Remove stash %s ? " stash))
      (vc-git--run-command-string nil "stash" "drop" (format "stash@%s" stash))
      (vc-dir-refresh))))

(defun vc-git-stash-show-at-point ()
  (interactive)
  (vc-git-stash-show (format "stash@%s" (vc-git-stash-get-at-point (point)))))

(defun vc-git-stash-apply-at-point ()
  (interactive)
  (vc-git-stash-apply (format "stash@%s" (vc-git-stash-get-at-point (point)))))

(defun vc-git-stash-pop-at-point ()
  (interactive)
  (vc-git-stash-pop (format "stash@%s" (vc-git-stash-get-at-point (point)))))

(defun vc-git-stash-menu (e)
  (interactive "e")
  (vc-dir-at-event e (popup-menu vc-git-stash-menu-map e)))


;;; Internal commands

(defun vc-git-command (buffer okstatus file-or-list &rest flags)
  "A wrapper around `vc-do-command' for use in vc-git.el.
The difference to vc-do-command is that this function always invokes
`vc-git-program'."
  (apply 'vc-do-command (or buffer "*vc*") okstatus vc-git-program
         file-or-list flags))

(defun vc-git--empty-db-p ()
  "Check if the git db is empty (no commit done yet)."
  (let (process-file-side-effects)
    (not (eq 0 (vc-git--call nil "rev-parse" "--verify" "HEAD")))))

(defun vc-git--call (buffer command &rest args)
  ;; We don't need to care the arguments.  If there is a file name, it
  ;; is always a relative one.  This works also for remote
  ;; directories.  We enable `inhibit-null-byte-detection', otherwise
  ;; Tramp's eol conversion might be confused.
  (let ((inhibit-null-byte-detection t)
	(process-environment (cons "PAGER=" process-environment)))
    (apply 'process-file vc-git-program nil buffer nil command args)))

(defun vc-git--out-ok (command &rest args)
  (zerop (apply 'vc-git--call '(t nil) command args)))

(defun vc-git--run-command-string (file &rest args)
  "Run a git command on FILE and return its output as string.
FILE can be nil."
  (let* ((ok t)
         (str (with-output-to-string
                (with-current-buffer standard-output
                  (unless (apply 'vc-git--out-ok
				 (if file
				     (append args (list (file-relative-name
							 file)))
				   args))
                    (setq ok nil))))))
    (and ok str)))

(defun vc-git-symbolic-commit (commit)
  "Translate COMMIT string into symbolic form.
Returns nil if not possible."
  (and commit
       (let ((name (with-temp-buffer
                     (and
                      (vc-git--out-ok "name-rev" "--name-only" commit)
                      (goto-char (point-min))
                      (= (forward-line 2) 1)
                      (bolp)
                      (buffer-substring-no-properties (point-min)
                                                      (1- (point-max)))))))
         (and name (not (string= name "undefined")) name))))

(provide 'vc-git)

;;; vc-git.el ends here
