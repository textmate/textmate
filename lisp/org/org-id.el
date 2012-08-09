;;; org-id.el --- Global identifiers for Org-mode entries
;;
;; Copyright (C) 2008-2012 Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;;
;; This file is part of GNU Emacs.
;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file implements globally unique identifiers for Org-mode entries.
;; Identifiers are stored in the entry as an :ID: property.  Functions
;; are provided that create and retrieve such identifiers, and that find
;; entries based on the identifier.

;; Identifiers consist of a prefix (default "Org" given by the variable
;; `org-id-prefix') and a unique part that can be created by a number
;; of different methods, see the variable `org-id-method'.
;; Org has a builtin method that uses a compact encoding of the creation
;; time of the ID, with microsecond accuracy.  This virtually
;; guarantees globally unique identifiers, even if several people are
;; creating IDs at the same time in files that will eventually be used
;; together.
;;
;; By default Org uses UUIDs as global unique identifiers.
;;
;; This file defines the following API:
;;
;; org-id-get-create
;;        Create an ID for the entry at point if it does not yet have one.
;;        Returns the ID (old or new).  This function can be used
;;        interactively, with prefix argument the creation of a new ID is
;;        forced, even if there was an old one.
;;
;; org-id-get
;;        Get the ID property of an entry.  Using appropriate arguments
;;        to the function, it can also create the ID for this entry.
;;
;; org-id-goto
;;        Command to go to a specific ID, this command can be used
;;        interactively.
;;
;; org-id-get-with-outline-path-completion
;;        Retrieve the ID of an entry, using outline path completion.
;;        This function can work for multiple files.
;;
;; org-id-get-with-outline-drilling
;;        Retrieve the ID of an entry, using outline path completion.
;;        This function only works for the current file.
;;
;; org-id-find
;;        Find the location of an entry with specific id.
;;

;;; Code:

(require 'org)

(declare-function message-make-fqdn "message" ())
(declare-function org-pop-to-buffer-same-window
		  "org-compat" (&optional buffer-or-name norecord label))

;;; Customization

(defgroup org-id nil
  "Options concerning global entry identifiers in Org-mode."
  :tag "Org ID"
  :group 'org)

(defcustom org-id-uuid-program "uuidgen"
  "The uuidgen program."
  :group 'org-id
  :type 'string)

(defcustom org-id-method 'uuid
  "The method that should be used to create new IDs.

An ID will consist of the optional prefix specified in `org-id-prefix',
and a unique part created by the method this variable specifies.

Allowed values are:

org        Org's own internal method, using an encoding of the current time to
           microsecond accuracy, and optionally the current domain of the
           computer.  See the variable `org-id-include-domain'.

uuid       Create random (version 4) UUIDs.  If the program defined in
           `org-id-uuid-program' is available it is used to create the ID.
           Otherwise an internal functions is used."
  :group 'org-id
  :type '(choice
	  (const :tag "Org's internal method" org)
	  (const :tag "external: uuidgen" uuid)))

(defcustom org-id-prefix nil
  "The prefix for IDs.

This may be a string, or it can be nil to indicate that no prefix is required.
When a string, the string should have no space characters as IDs are expected
to have no space characters in them."
  :group 'org-id
  :type '(choice
	  (const :tag "No prefix")
	  (string :tag "Prefix")))

(defcustom org-id-include-domain nil
  "Non-nil means add the domain name to new IDs.
This ensures global uniqueness of IDs, and is also suggested by
RFC 2445 in combination with RFC 822.  This is only relevant if
`org-id-method' is `org'.  When uuidgen is used, the domain will never
be added.
The default is to not use this because we have no really good way to get
the true domain, and Org entries will normally not be shared with enough
people to make this necessary."
  :group 'org-id
  :type 'boolean)

(defcustom org-id-track-globally t
  "Non-nil means track IDs through files, so that links work globally.
This work by maintaining a hash table for IDs and writing this table
to disk when exiting Emacs.  Because of this, it works best if you use
a single Emacs process, not many.

When nil, IDs are not tracked.  Links to IDs will still work within
a buffer, but not if the entry is located in another file.
IDs can still be used if the entry with the id is in the same file as
the link."
  :group 'org-id
  :type 'boolean)

(defcustom org-id-locations-file (convert-standard-filename
				  "~/.emacs.d/.org-id-locations")
  "The file for remembering in which file an ID was defined.
This variable is only relevant when `org-id-track-globally' is set."
  :group 'org-id
  :type 'file)

(defvar org-id-locations nil
  "List of files with IDs in those files.")

(defvar org-id-files nil
  "List of files that contain IDs.")

(defcustom org-id-extra-files 'org-agenda-text-search-extra-files
  "Files to be searched for IDs, besides the agenda files.
When Org reparses files to remake the list of files and IDs it is tracking,
it will normally scan the agenda files, the archives related to agenda files,
any files that are listed as ID containing in the current register, and
any Org-mode files currently visited by Emacs.
You can list additional files here.
This variable is only relevant when `org-id-track-globally' is set."
  :group 'org-id
  :type
  '(choice
    (symbol :tag "Variable")
    (repeat :tag "List of files"
	    (file))))

(defcustom org-id-search-archives t
  "Non-nil means search also the archive files of agenda files for entries.
This is a possibility to reduce overhead, but it means that entries moved
to the archives can no longer be found by ID.
This variable is only relevant when `org-id-track-globally' is set."
  :group 'org-id
  :type 'boolean)

;;; The API functions

;;;###autoload
(defun org-id-get-create (&optional force)
  "Create an ID for the current entry and return it.
If the entry already has an ID, just return it.
With optional argument FORCE, force the creation of a new ID."
  (interactive "P")
  (when force
    (org-entry-put (point) "ID" nil))
  (org-id-get (point) 'create))

;;;###autoload
(defun org-id-copy ()
  "Copy the ID of the entry at point to the kill ring.
Create an ID if necessary."
  (interactive)
  (org-kill-new (org-id-get nil 'create)))

;;;###autoload
(defun org-id-get (&optional pom create prefix)
  "Get the ID property of the entry at point-or-marker POM.
If POM is nil, refer to the entry at point.
If the entry does not have an ID, the function returns nil.
However, when CREATE is non nil, create an ID if none is present already.
PREFIX will be passed through to `org-id-new'.
In any case, the ID of the entry is returned."
  (org-with-point-at pom
    (let ((id (org-entry-get nil "ID")))
      (cond
       ((and id (stringp id) (string-match "\\S-" id))
	id)
       (create
	(setq id (org-id-new prefix))
	(org-entry-put pom "ID" id)
	(org-id-add-location id (buffer-file-name (buffer-base-buffer)))
	id)
       (t nil)))))

;;;###autoload
(defun org-id-get-with-outline-path-completion (&optional targets)
  "Use outline-path-completion to retrieve the ID of an entry.
TARGETS may be a setting for `org-refile-targets' to define the eligible
headlines.  When omitted, all headlines in all agenda files are
eligible.
It returns the ID of the entry.  If necessary, the ID is created."
  (let* ((org-refile-targets (or targets '((nil . (:maxlevel . 10)))))
	 (org-refile-use-outline-path
	  (if (caar org-refile-targets) 'file t))
	 (org-refile-target-verify-function nil)
	 (spos (org-refile-get-location "Entry"))
	 (pom (and spos (move-marker (make-marker) (nth 3 spos)
				     (get-file-buffer (nth 1 spos))))))
    (prog1 (org-id-get pom 'create)
      (move-marker pom nil))))

;;;###autoload
(defun org-id-get-with-outline-drilling (&optional targets)
  "Use an outline-cycling interface to retrieve the ID of an entry.
This only finds entries in the current buffer, using `org-get-location'.
It returns the ID of the entry.  If necessary, the ID is created."
  (let* ((spos (org-get-location (current-buffer) org-goto-help))
	 (pom (and spos (move-marker (make-marker) (car spos)))))
    (prog1 (org-id-get pom 'create)
      (move-marker pom nil))))

;;;###autoload
(defun org-id-goto (id)
  "Switch to the buffer containing the entry with id ID.
Move the cursor to that entry in that buffer."
  (interactive "sID: ")
  (let ((m (org-id-find id 'marker)))
    (unless m
      (error "Cannot find entry with ID \"%s\"" id))
    (org-pop-to-buffer-same-window (marker-buffer m))
    (goto-char m)
    (move-marker m nil)
    (org-show-context)))

;;;###autoload
(defun org-id-find (id &optional markerp)
  "Return the location of the entry with the id ID.
The return value is a cons cell (file-name . position), or nil
if there is no entry with that ID.
With optional argument MARKERP, return the position as a new marker."
  (cond
   ((symbolp id) (setq id (symbol-name id)))
   ((numberp id) (setq id (number-to-string id))))
  (let ((file (org-id-find-id-file id))
	org-agenda-new-buffers where)
    (when file
      (setq where (org-id-find-id-in-file id file markerp)))
    (unless where
      (org-id-update-id-locations)
      (setq file (org-id-find-id-file id))
      (when file
	(setq where (org-id-find-id-in-file id file markerp))))
    where))

;;; Internal functions

;; Creating new IDs

(defun org-id-new (&optional prefix)
  "Create a new globally unique ID.

An ID consists of two parts separated by a colon:
- a prefix
- a unique part that will be created according to `org-id-method'.

PREFIX can specify the prefix, the default is given by the variable
`org-id-prefix'.  However, if PREFIX is the symbol `none', don't use any
prefix even if `org-id-prefix' specifies one.

So a typical ID could look like \"Org:4nd91V40HI\"."
  (let* ((prefix (if (eq prefix 'none)
		     ""
		   (concat (or prefix org-id-prefix) ":")))
	 unique)
    (if (equal prefix ":") (setq prefix ""))
    (cond
     ((memq org-id-method '(uuidgen uuid))
      (setq unique (org-trim (shell-command-to-string org-id-uuid-program)))
      (unless (org-uuidgen-p unique)
	(setq unique (org-id-uuid))))
     ((eq org-id-method 'org)
      (let* ((etime (org-id-reverse-string (org-id-time-to-b36)))
	     (postfix (if org-id-include-domain
			  (progn
			    (require 'message)
			    (concat "@" (message-make-fqdn))))))
	(setq unique (concat etime postfix))))
     (t (error "Invalid `org-id-method'")))
    (concat prefix unique)))

(defun org-id-uuid ()
  "Return string with random (version 4) UUID."
  (let ((rnd (md5 (format "%s%s%s%s%s%s%s"
			  (random t)
			  (current-time)
			  (user-uid)
			  (emacs-pid)
			  (user-full-name)
			  user-mail-address
			  (recent-keys)))))
    (format "%s-%s-4%s-%s%s-%s"
	    (substring rnd 0 8)
	    (substring rnd 8 12)
	    (substring rnd 13 16)
	    (format "%x"
		    (logior
		     #b10000000
		     (logand
		      #b10111111
		      (string-to-number
		       (substring rnd 16 18) 16))))
	    (substring rnd 18 20)
	    (substring rnd 20 32))))

(defun org-id-reverse-string (s)
  (mapconcat 'char-to-string (nreverse (string-to-list s)) ""))

(defun org-id-int-to-b36-one-digit (i)
  "Turn an integer between 0 and 61 into a single character 0..9, A..Z, a..z."
  (cond
   ((< i 10) (+ ?0 i))
   ((< i 36) (+ ?a i -10))
   (t (error "Larger that 35"))))

(defun org-id-b36-to-int-one-digit (i)
  "Turn a character 0..9, A..Z, a..z into a number 0..61.
The input I may be a character, or a single-letter string."
  (and (stringp i) (setq i (string-to-char i)))
  (cond
   ((and (>= i ?0) (<= i ?9)) (- i ?0))
   ((and (>= i ?a) (<= i ?z)) (+ (- i ?a) 10))
   (t (error "Invalid b36 letter"))))

(defun org-id-int-to-b36 (i &optional length)
  "Convert an integer to a base-36 number represented as a string."
  (let ((s ""))
    (while (> i 0)
      (setq s (concat (char-to-string
		       (org-id-int-to-b36-one-digit (mod i 36))) s)
	    i (/ i 36)))
    (setq length (max 1 (or length 1)))
    (if (< (length s) length)
	(setq s (concat (make-string (- length (length s)) ?0) s)))
    s))

(defun org-id-b36-to-int (s)
  "Convert a base-36 string into the corresponding integer."
  (let ((r 0))
    (mapc (lambda (i) (setq r (+ (* r 36) (org-id-b36-to-int-one-digit i))))
	  s)
    r))

(defun org-id-time-to-b36 (&optional time)
  "Encode TIME as a 10-digit string.
This string holds the time to micro-second accuracy, and can be decoded
using `org-id-decode'."
  (setq time (or time (current-time)))
  (concat (org-id-int-to-b36 (nth 0 time) 4)
	  (org-id-int-to-b36 (nth 1 time) 4)
	  (org-id-int-to-b36 (or (nth 2 time) 0) 4)))

(defun org-id-decode (id)
  "Split ID into the prefix and the time value that was used to create it.
The return value is (prefix . time) where PREFIX is nil or a string,
and time is the usual three-integer representation of time."
  (let (prefix time parts)
    (setq parts (org-split-string id ":"))
    (if (= 2 (length parts))
	(setq prefix (car parts) time (nth 1 parts))
      (setq prefix nil time (nth 0 parts)))
    (setq time (org-id-reverse-string time))
    (setq time (list (org-id-b36-to-int (substring time 0 4))
		     (org-id-b36-to-int (substring time 4 8))
		     (org-id-b36-to-int (substring time 8 12))))
    (cons prefix time)))

;; Storing ID locations (files)

(defun org-id-update-id-locations (&optional files)
  "Scan relevant files for IDs.
Store the relation between files and corresponding IDs.
This will scan all agenda files, all associated archives, and all
files currently mentioned in `org-id-locations'.
When FILES is given, scan these files instead.
When CHECK is given, prepare detailed information about duplicate IDs."
  (interactive)
  (if (not org-id-track-globally)
      (error "Please turn on `org-id-track-globally' if you want to track IDs")
    (let* ((org-id-search-archives
	    (or org-id-search-archives
		(and (symbolp org-id-extra-files)
		     (symbol-value org-id-extra-files)
		     (member 'agenda-archives org-id-extra-files))))
	   (files
	    (or files
		(append
		 ;; Agenda files and all associated archives
		 (org-agenda-files t org-id-search-archives)
		 ;; Explicit extra files
		 (if (symbolp org-id-extra-files)
		     (symbol-value org-id-extra-files)
		   org-id-extra-files)
	      ;; Files associated with live org-mode buffers
		 (delq nil
		       (mapcar (lambda (b)
				 (with-current-buffer b
				   (and (eq major-mode 'org-mode) (buffer-file-name))))
			       (buffer-list)))
		 ;; All files known to have IDs
		 org-id-files)))
	   org-agenda-new-buffers
	   file nfiles tfile ids reg found id seen (ndup 0))
      (when (member 'agenda-archives files)
	(setq files (delq 'agenda-archives (copy-sequence files))))
      (setq nfiles (length files))
      (while (setq file (pop files))
	(message "Finding ID locations (%d/%d files): %s"
		 (- nfiles (length files)) nfiles file)
	(setq tfile (file-truename file))
	(when (and (file-exists-p file) (not (member tfile seen)))
	  (push tfile seen)
	  (setq ids nil)
	  (with-current-buffer (org-get-agenda-file-buffer file)
	    (save-excursion
	      (save-restriction
		(widen)
		(goto-char (point-min))
		(while (re-search-forward "^[ \t]*:ID:[ \t]+\\(\\S-+\\)[ \t]*$"
					  nil t)
		  (setq id (org-match-string-no-properties 1))
		  (if (member id found)
		      (progn
			(message "Duplicate ID \"%s\", also in file %s"
				 id (or (car (delq
					      nil
					      (mapcar
					       (lambda (x)
						 (if (member id (cdr x))
						     (car x)))
					       reg)))
					(buffer-file-name)))
			(when (= ndup 0)
			  (ding)
			  (sit-for 2))
			(setq ndup (1+ ndup)))
		    (push id found)
		    (push id ids)))
		(push (cons (abbreviate-file-name file) ids) reg))))))
      (org-release-buffers org-agenda-new-buffers)
      (setq org-agenda-new-buffers nil)
      (setq org-id-locations reg)
      (setq org-id-files (mapcar 'car org-id-locations))
      (org-id-locations-save) ;; this function can also handle the alist form
      ;; now convert to a hash
      (setq org-id-locations (org-id-alist-to-hash org-id-locations))
      (if (> ndup 0)
	  (message "WARNING: %d duplicate IDs found, check *Messages* buffer" ndup)
	(message "%d unique files scanned for IDs" (length org-id-files)))
      org-id-locations)))

(defun org-id-locations-save ()
  "Save `org-id-locations' in `org-id-locations-file'."
  (when (and org-id-track-globally org-id-locations)
    (let ((out (if (hash-table-p org-id-locations)
		   (org-id-hash-to-alist org-id-locations)
		 org-id-locations)))
      (with-temp-file org-id-locations-file
	(print out (current-buffer))))))

(defun org-id-locations-load ()
  "Read the data from `org-id-locations-file'."
  (setq org-id-locations nil)
  (when org-id-track-globally
    (with-temp-buffer
      (condition-case nil
	  (progn
	    (insert-file-contents-literally org-id-locations-file)
	    (goto-char (point-min))
	    (setq org-id-locations (read (current-buffer))))
	(error
	 (message "Could not read org-id-values from %s. Setting it to nil."
		  org-id-locations-file))))
    (setq org-id-files (mapcar 'car org-id-locations))
    (setq org-id-locations (org-id-alist-to-hash org-id-locations))))

(defun org-id-add-location (id file)
  "Add the ID with location FILE to the database of ID locations."
  ;; Only if global tracking is on, and when the buffer has a file
  (when (and org-id-track-globally id file)
    (unless org-id-locations (org-id-locations-load))
    (puthash id (abbreviate-file-name file) org-id-locations)
    (add-to-list 'org-id-files (abbreviate-file-name file))))

(unless noninteractive
  (add-hook 'kill-emacs-hook 'org-id-locations-save))

(defun org-id-hash-to-alist (hash)
  "Turn an org-id hash into an alist, so that it can be written to a file."
  (let (res x)
    (maphash
     (lambda (k v)
       (if (setq x (member v res))
	   (setcdr x (cons k (cdr x)))
	 (push (list v k) res)))
     hash)
    res))

(defun org-id-alist-to-hash (list)
  "Turn an org-id location list into a hash table."
  (let ((res (make-hash-table
	      :test 'equal
	      :size (apply '+ (mapcar 'length list))))
	f)
    (mapc
     (lambda (x)
       (setq f (car x))
       (mapc (lambda (i) (puthash i f res)) (cdr x)))
     list)
    res))

(defun org-id-paste-tracker (txt &optional buffer-or-file)
  "Update any IDs in TXT and assign BUFFER-OR-FILE to them."
  (when org-id-track-globally
    (save-match-data
      (setq buffer-or-file (or buffer-or-file (current-buffer)))
      (when (bufferp buffer-or-file)
	(setq buffer-or-file (or (buffer-base-buffer buffer-or-file)
				 buffer-or-file))
	(setq buffer-or-file (buffer-file-name buffer-or-file)))
      (when buffer-or-file
	(let ((fname (abbreviate-file-name buffer-or-file))
	      (s 0))
	  (while (string-match "^[ \t]*:ID:[ \t]+\\([^ \t\n\r]+\\)" txt s)
	    (setq s (match-end 0))
	    (org-id-add-location (match-string 1 txt) fname)))))))

;; Finding entries with specified id

;;;###autoload
(defun org-id-find-id-file (id)
  "Query the id database for the file in which this ID is located."
  (unless org-id-locations (org-id-locations-load))
  (or (and org-id-locations
	   (hash-table-p org-id-locations)
	   (gethash id org-id-locations))
      ;; ball back on current buffer
      (buffer-file-name (or (buffer-base-buffer (current-buffer))
			    (current-buffer)))))

(defun org-id-find-id-in-file (id file &optional markerp)
  "Return the position of the entry ID in FILE.
If that files does not exist, or if it does not contain this ID,
return nil.
The position is returned as a cons cell (file-name . position).  With
optional argument MARKERP, return the position as a new marker."
  (let (org-agenda-new-buffers buf pos)
    (cond
     ((not file) nil)
     ((not (file-exists-p file)) nil)
     (t (with-current-buffer (setq buf (org-get-agenda-file-buffer file))
	  (setq pos (org-find-entry-with-id id))
	  (when pos
	    (if markerp
		(move-marker (make-marker) pos buf)
	      (cons file pos))))))))

;; id link type

;; Calling the following function is hard-coded into `org-store-link',
;; so we do have to add it to `org-store-link-functions'.

;;;###autoload
(defun org-id-store-link ()
  "Store a link to the current entry, using its ID."
  (interactive)
  (when (and (buffer-file-name (buffer-base-buffer)) (eq major-mode 'org-mode))
    (let* ((link (org-make-link "id:" (org-id-get-create)))
	   (case-fold-search nil)
	   (desc (save-excursion
		   (org-back-to-heading t)
		   (or (and (looking-at org-complex-heading-regexp)
			    (if (match-end 4)
				(match-string 4)
			      (match-string 0)))
		       link))))
      (org-store-link-props :link link :description desc :type "id")
      link)))

(defun org-id-open (id)
  "Go to the entry with id ID."
  (org-mark-ring-push)
  (let ((m (org-id-find id 'marker))
	cmd)
    (unless m
      (error "Cannot find entry with ID \"%s\"" id))
    ;; Use a buffer-switching command in analogy to finding files
    (setq cmd
	  (or
	   (cdr
	    (assq
	     (cdr (assq 'file org-link-frame-setup))
	     '((find-file . switch-to-buffer)
	       (find-file-other-window . switch-to-buffer-other-window)
	       (find-file-other-frame . switch-to-buffer-other-frame))))
	   'switch-to-buffer-other-window))
    (if (not (equal (current-buffer) (marker-buffer m)))
	(funcall cmd (marker-buffer m)))
    (goto-char m)
    (move-marker m nil)
    (org-show-context)))

(org-add-link-type "id" 'org-id-open)

(provide 'org-id)

;;; org-id.el ends here
