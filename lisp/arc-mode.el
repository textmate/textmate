;;; arc-mode.el --- simple editing of archives

;; Copyright (C) 1995, 1997-1998, 2001-2012  Free Software Foundation, Inc.

;; Author: Morten Welinder <terra@gnu.org>
;; Keywords: files archives msdog editing major-mode
;; Favorite-brand-of-beer: None, I hate beer.

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

;; NAMING: "arc" is short for "archive" and does not refer specifically
;; to files whose name end in ".arc"
;;
;; This code does not decode any files internally, although it does
;; understand the directory level of the archives.  For this reason,
;; you should expect this code to need more fiddling than tar-mode.el
;; (although it at present has fewer bugs :-)  In particular, I have
;; not tested this under Ms-Dog myself.
;; -------------------------------------
;; INTERACTION: arc-mode.el should play together with
;;
;; * ange-ftp.el: Remote archives (i.e., ones that ange-ftp has brought
;;                to you) are handled by doing all updates on a local
;;                copy.  When you make changes to a remote file the
;;                changes will first take effect when the archive buffer
;;                is saved.  You will be warned about this.
;;
;; * dos-fns.el:  (Part of Emacs 19).  You get automatic ^M^J <--> ^J
;;                conversion.
;;
;; arc-mode.el does not work well with crypt++.el; for the archives as
;; such this could be fixed (but wouldn't be useful) by declaring such
;; archives to be "remote".  For the members this is a general Emacs
;; problem that 19.29's file formats may fix.
;; -------------------------------------
;; ARCHIVE TYPES: Currently only the archives below are handled, but the
;; structure for handling just about anything is in place.
;;
;;			Arc	Lzh	Zip	Zoo	Rar	7z
;;			--------------------------------------------
;; View listing		Intern	Intern	Intern	Intern	Y	Y
;; Extract member	Y	Y	Y	Y	Y	Y
;; Save changed member	Y	Y	Y	Y	N	Y
;; Add new member	N	N	N	N	N	N
;; Delete member	Y	Y	Y	Y	N	Y
;; Rename member	Y	Y	N	N	N	N
;; Chmod		-	Y	Y	-	N	N
;; Chown		-	Y	-	-	N	N
;; Chgrp		-	Y	-	-	N	N
;;
;; Special thanks to Bill Brodie <wbrodie@panix.com> for very useful tips
;; on the first released version of this package.
;;
;; This code is partly based on tar-mode.el from Emacs.
;; -------------------------------------
;; ARCHIVE STRUCTURES:
;; (This is mostly for myself.)
;;
;; ARC         A series of (header,file).  No interactions among members.
;;
;; LZH         A series of (header,file).  Headers are checksummed.  No
;;             interaction among members.
;;             Headers come in three flavors called level 0, 1 and 2 headers.
;;             Level 2 header is free of DOS specific restrictions and most
;;             prevalently used.  Also level 1 and 2 headers consist of base
;;             and extension headers.  For more details see
;;             http://homepage1.nifty.com/dangan/en/Content/Program/Java/jLHA/Notes/Notes.html
;;             http://www.osirusoft.com/joejared/lzhformat.html
;;
;; ZIP         A series of (lheader,fil) followed by a "central directory"
;;             which is a series of (cheader) followed by an end-of-
;;             central-dir record possibly followed by junk.  The e-o-c-d
;;             links to c-d.  cheaders link to lheaders which are basically
;;             cut-down versions of the cheaders.
;;
;; ZOO         An archive header followed by a series of (header,file).
;;             Each member header points to the next.  The archive is
;;             terminated by a bogus header with a zero next link.
;; -------------------------------------
;; HOOKS: `foo' means one of the supported archive types.
;;
;; archive-mode-hook
;; archive-foo-mode-hook
;; archive-extract-hooks

;;; Code:

;; -------------------------------------------------------------------------
;;; Section: Configuration.

(defgroup archive nil
  "Simple editing of archives."
  :group 'data)

(defgroup archive-arc nil
  "ARC-specific options to archive."
  :group 'archive)

(defgroup archive-lzh nil
  "LZH-specific options to archive."
  :group 'archive)

(defgroup archive-zip nil
  "ZIP-specific options to archive."
  :group 'archive)

(defgroup archive-zoo nil
  "ZOO-specific options to archive."
  :group 'archive)

(defcustom archive-tmpdir
  ;; make-temp-name is safe here because we use this name
  ;; to create a directory.
  (make-temp-name
   (expand-file-name (if (eq system-type 'ms-dos) "ar" "archive.tmp")
		     temporary-file-directory))
  "Directory for temporary files made by `arc-mode.el'."
  :type 'directory
  :group 'archive)

(defcustom archive-remote-regexp "^/[^/:]*[^/:.]:"
  "Regexp recognizing archive files names that are not local.
A non-local file is one whose file name is not proper outside Emacs.
A local copy of the archive will be used when updating."
  :type 'regexp
  :group 'archive)

(defcustom archive-extract-hooks nil
  "Hooks to run when an archive member has been extracted."
  :type 'hook
  :group 'archive)
;; ------------------------------
;; Arc archive configuration

;; We always go via a local file since there seems to be no reliable way
;; to extract to stdout without junk getting added.
(defcustom archive-arc-extract
  '("arc" "x")
  "Program and its options to run in order to extract an arc file member.
Extraction should happen to the current directory.  Archive and member
name will be added."
  :type '(list (string :tag "Program")
		(repeat :tag "Options"
			:inline t
			(string :format "%v")))
  :group 'archive-arc)

(defcustom archive-arc-expunge
  '("arc" "d")
  "Program and its options to run in order to delete arc file members.
Archive and member names will be added."
  :type '(list (string :tag "Program")
		(repeat :tag "Options"
			:inline t
			(string :format "%v")))
  :group 'archive-arc)

(defcustom archive-arc-write-file-member
  '("arc" "u")
  "Program and its options to run in order to update an arc file member.
Archive and member name will be added."
  :type '(list (string :tag "Program")
		(repeat :tag "Options"
			:inline t
			(string :format "%v")))
  :group 'archive-arc)
;; ------------------------------
;; Lzh archive configuration

(defcustom archive-lzh-extract
  '("lha" "pq")
  "Program and its options to run in order to extract an lzh file member.
Extraction should happen to standard output.  Archive and member name will
be added."
  :type '(list (string :tag "Program")
		(repeat :tag "Options"
			:inline t
			(string :format "%v")))
  :group 'archive-lzh)

(defcustom archive-lzh-expunge
  '("lha" "d")
  "Program and its options to run in order to delete lzh file members.
Archive and member names will be added."
  :type '(list (string :tag "Program")
		(repeat :tag "Options"
			:inline t
			(string :format "%v")))
  :group 'archive-lzh)

(defcustom archive-lzh-write-file-member
  '("lha" "a")
  "Program and its options to run in order to update an lzh file member.
Archive and member name will be added."
  :type '(list (string :tag "Program")
		(repeat :tag "Options"
			:inline t
			(string :format "%v")))
  :group 'archive-lzh)
;; ------------------------------
;; Zip archive configuration

(defcustom archive-zip-extract
  (cond ((executable-find "unzip")   '("unzip" "-qq" "-c"))
	((executable-find "7z")      '("7z" "x" "-so"))
	((executable-find "pkunzip") '("pkunzip" "-e" "-o-"))
	(t                           '("unzip" "-qq" "-c")))
  "Program and its options to run in order to extract a zip file member.
Extraction should happen to standard output.  Archive and member name will
be added."
  :type '(list (string :tag "Program")
	       (repeat :tag "Options"
		       :inline t
		       (string :format "%v")))
  :group 'archive-zip)

;; For several reasons the latter behavior is not desirable in general.
;; (1) It uses more disk space.  (2) Error checking is worse or non-
;; existent.  (3) It tends to do funny things with other systems' file
;; names.

(defcustom archive-zip-expunge
  (cond ((executable-find "zip")     '("zip" "-d" "-q"))
	((executable-find "7z")      '("7z" "d"))
	((executable-find "pkzip")   '("pkzip" "-d"))
	(t                           '("zip" "-d" "-q")))
  "Program and its options to run in order to delete zip file members.
Archive and member names will be added."
  :type '(list (string :tag "Program")
	       (repeat :tag "Options"
		       :inline t
		       (string :format "%v")))
  :group 'archive-zip)

(defcustom archive-zip-update
  (cond ((executable-find "zip")     '("zip" "-q"))
	((executable-find "7z")      '("7z" "u"))
	((executable-find "pkzip")   '("pkzip" "-u" "-P"))
	(t                           '("zip" "-q")))
  "Program and its options to run in order to update a zip file member.
Options should ensure that specified directory will be put into the zip
file.  Archive and member name will be added."
  :type '(list (string :tag "Program")
	       (repeat :tag "Options"
		       :inline t
		       (string :format "%v")))
  :group 'archive-zip)

(defcustom archive-zip-update-case
  (cond ((executable-find "zip")     '("zip" "-q" "-k"))
	((executable-find "7z")      '("7z" "u"))
	((executable-find "pkzip")   '("pkzip" "-u" "-P"))
	(t                           '("zip" "-q" "-k")))
  "Program and its options to run in order to update a case fiddled zip member.
Options should ensure that specified directory will be put into the zip file.
Archive and member name will be added."
  :type '(list (string :tag "Program")
	       (repeat :tag "Options"
		       :inline t
		       (string :format "%v")))
  :group 'archive-zip)

(defcustom archive-zip-case-fiddle t
  "If non-nil then zip file members may be down-cased.
This case fiddling will only happen for members created by a system
that uses caseless file names."
  :type 'boolean
  :group 'archive-zip)
;; ------------------------------
;; Zoo archive configuration

(defcustom archive-zoo-extract
  '("zoo" "xpq")
  "Program and its options to run in order to extract a zoo file member.
Extraction should happen to standard output.  Archive and member name will
be added."
  :type '(list (string :tag "Program")
		(repeat :tag "Options"
			:inline t
			(string :format "%v")))
  :group 'archive-zoo)

(defcustom archive-zoo-expunge
  '("zoo" "DqPP")
  "Program and its options to run in order to delete zoo file members.
Archive and member names will be added."
  :type '(list (string :tag "Program")
		(repeat :tag "Options"
			:inline t
			(string :format "%v")))
  :group 'archive-zoo)

(defcustom archive-zoo-write-file-member
  '("zoo" "a")
  "Program and its options to run in order to update a zoo file member.
Archive and member name will be added."
  :type '(list (string :tag "Program")
		(repeat :tag "Options"
			:inline t
			(string :format "%v")))
  :group 'archive-zoo)
;; ------------------------------
;; 7z archive configuration

(defcustom archive-7z-extract
  '("7z" "x" "-so")
  "Program and its options to run in order to extract a 7z file member.
Extraction should happen to standard output.  Archive and member name will
be added."
  :version "24.1"
  :type '(list (string :tag "Program")
	       (repeat :tag "Options"
		       :inline t
		       (string :format "%v")))
  :group 'archive-7z)

(defcustom archive-7z-expunge
  '("7z" "d")
  "Program and its options to run in order to delete 7z file members.
Archive and member names will be added."
  :version "24.1"
  :type '(list (string :tag "Program")
	       (repeat :tag "Options"
		       :inline t
		       (string :format "%v")))
  :group 'archive-7z)

(defcustom archive-7z-update
  '("7z" "u")
  "Program and its options to run in order to update a 7z file member.
Options should ensure that specified directory will be put into the 7z
file.  Archive and member name will be added."
  :version "24.1"
  :type '(list (string :tag "Program")
	       (repeat :tag "Options"
		       :inline t
		       (string :format "%v")))
  :group 'archive-7z)

;; -------------------------------------------------------------------------
;;; Section: Variables

(defvar archive-subtype nil "Symbol describing archive type.")
(defvar archive-file-list-start nil "Position of first contents line.")
(defvar archive-file-list-end nil "Position just after last contents line.")
(defvar archive-proper-file-start nil "Position of real archive's start.")
(defvar archive-read-only nil "Non-nil if the archive is read-only on disk.")
(defvar archive-local-name nil "Name of local copy of remote archive.")
(defvar archive-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map " " 'archive-next-line)
    (define-key map "a" 'archive-alternate-display)
    ;;(define-key map "c" 'archive-copy)
    (define-key map "d" 'archive-flag-deleted)
    (define-key map "\C-d" 'archive-flag-deleted)
    (define-key map "e" 'archive-extract)
    (define-key map "f" 'archive-extract)
    (define-key map "\C-m" 'archive-extract)
    (define-key map "m" 'archive-mark)
    (define-key map "n" 'archive-next-line)
    (define-key map "\C-n" 'archive-next-line)
    (define-key map [down] 'archive-next-line)
    (define-key map "o" 'archive-extract-other-window)
    (define-key map "p" 'archive-previous-line)
    (define-key map "\C-p" 'archive-previous-line)
    (define-key map [up] 'archive-previous-line)
    (define-key map "r" 'archive-rename-entry)
    (define-key map "u" 'archive-unflag)
    (define-key map "\M-\C-?" 'archive-unmark-all-files)
    (define-key map "v" 'archive-view)
    (define-key map "x" 'archive-expunge)
    (define-key map "\177" 'archive-unflag-backwards)
    (define-key map "E" 'archive-extract-other-window)
    (define-key map "M" 'archive-chmod-entry)
    (define-key map "G" 'archive-chgrp-entry)
    (define-key map "O" 'archive-chown-entry)
    ;; Let mouse-1 follow the link.
    (define-key map [follow-link] 'mouse-face)

    (if (fboundp 'command-remapping)
        (progn
          (define-key map [remap advertised-undo] 'archive-undo)
          (define-key map [remap undo] 'archive-undo))
      (substitute-key-definition 'advertised-undo 'archive-undo map global-map)
      (substitute-key-definition 'undo 'archive-undo map global-map))

    (define-key map
      (if (featurep 'xemacs) 'button2 [mouse-2]) 'archive-extract)

    (if (featurep 'xemacs)
        ()				; out of luck

      (define-key map [menu-bar immediate]
        (cons "Immediate" (make-sparse-keymap "Immediate")))
      (define-key map [menu-bar immediate alternate]
        '(menu-item "Alternate Display" archive-alternate-display
          :enable (boundp (archive-name "alternate-display"))
          :help "Toggle alternate file info display"))
      (define-key map [menu-bar immediate view]
        '(menu-item "View This File" archive-view
          :help "Display file at cursor in View Mode"))
      (define-key map [menu-bar immediate display]
        '(menu-item "Display in Other Window" archive-display-other-window
          :help "Display file at cursor in another window"))
      (define-key map [menu-bar immediate find-file-other-window]
        '(menu-item "Find in Other Window" archive-extract-other-window
          :help "Edit file at cursor in another window"))
      (define-key map [menu-bar immediate find-file]
        '(menu-item "Find This File" archive-extract
          :help "Extract file at cursor and edit it"))

      (define-key map [menu-bar mark]
        (cons "Mark" (make-sparse-keymap "Mark")))
      (define-key map [menu-bar mark unmark-all]
        '(menu-item "Unmark All" archive-unmark-all-files
          :help "Unmark all marked files"))
      (define-key map [menu-bar mark deletion]
        '(menu-item "Flag" archive-flag-deleted
          :help "Flag file at cursor for deletion"))
      (define-key map [menu-bar mark unmark]
        '(menu-item "Unflag" archive-unflag
          :help "Unmark file at cursor"))
      (define-key map [menu-bar mark mark]
        '(menu-item "Mark" archive-mark
          :help "Mark file at cursor"))

      (define-key map [menu-bar operate]
        (cons "Operate" (make-sparse-keymap "Operate")))
      (define-key map [menu-bar operate chown]
        '(menu-item "Change Owner..." archive-chown-entry
          :enable (fboundp (archive-name "chown-entry"))
          :help "Change owner of marked files"))
      (define-key map [menu-bar operate chgrp]
        '(menu-item "Change Group..." archive-chgrp-entry
          :enable (fboundp (archive-name "chgrp-entry"))
          :help "Change group ownership of marked files"))
      (define-key map [menu-bar operate chmod]
        '(menu-item "Change Mode..." archive-chmod-entry
          :enable (fboundp (archive-name "chmod-entry"))
          :help "Change mode (permissions) of marked files"))
      (define-key map [menu-bar operate rename]
        '(menu-item "Rename to..." archive-rename-entry
          :enable (fboundp (archive-name "rename-entry"))
          :help "Rename marked files"))
      ;;(define-key map [menu-bar operate copy]
      ;;  '(menu-item "Copy to..." archive-copy))
      (define-key map [menu-bar operate expunge]
        '(menu-item "Expunge Marked Files" archive-expunge
          :help "Delete all flagged files from archive"))
      map))
  "Local keymap for archive mode listings.")
(defvar archive-file-name-indent nil "Column where file names start.")

(defvar archive-remote nil "Non-nil if the archive is outside file system.")
(make-variable-buffer-local 'archive-remote)
(put 'archive-remote 'permanent-local t)

(defvar archive-member-coding-system nil "Coding-system of archive member.")
(make-variable-buffer-local 'archive-member-coding-system)

(defvar archive-alternate-display nil
  "Non-nil when alternate information is shown.")
(make-variable-buffer-local 'archive-alternate-display)
(put 'archive-alternate-display 'permanent-local t)

(defvar archive-superior-buffer nil "In archive members, points to archive.")
(put 'archive-superior-buffer 'permanent-local t)

(defvar archive-subfile-mode nil "Non-nil in archive member buffers.")
(make-variable-buffer-local 'archive-subfile-mode)
(put 'archive-subfile-mode 'permanent-local t)

(defvar archive-file-name-coding-system nil)
(make-variable-buffer-local 'archive-file-name-coding-system)
(put 'archive-file-name-coding-system 'permanent-local t)

(defvar archive-files nil
  "Vector of file descriptors.
Each descriptor is a vector of the form
 [EXT-FILE-NAME INT-FILE-NAME CASE-FIDDLED MODE ...]")
(make-variable-buffer-local 'archive-files)

;; -------------------------------------------------------------------------
;;; Section: Support functions.

(eval-when-compile
  (defsubst byte-after (pos)
    "Like char-after but an eight-bit char is converted to unibyte."
    (multibyte-char-to-unibyte (char-after pos)))
  (defsubst insert-unibyte (&rest args)
    "Like insert but don't make unibyte string and eight-bit char multibyte."
    (dolist (elt args)
      (if (integerp elt)
	  (insert (if (< elt 128) elt (decode-char 'eight-bit elt)))
	(insert (string-to-multibyte elt)))))
  )

(defsubst archive-name (suffix)
  (intern (concat "archive-" (symbol-name archive-subtype) "-" suffix)))

(defun archive-l-e (str &optional len float)
  "Convert little endian string/vector STR to integer.
Alternatively, STR may be a buffer position in the current buffer
in which case a second argument, length LEN, should be supplied.
FLOAT, if non-nil, means generate and return a float instead of an integer
\(use this for numbers that can overflow the Emacs integer)."
  (if (stringp str)
      (setq len (length str))
    (setq str (buffer-substring str (+ str len))))
  (setq str (string-as-unibyte str))
  (let ((result 0)
        (i 0))
    (while (< i len)
      (setq i (1+ i)
            result (+ (if float (* result 256.0) (ash result 8))
		      (aref str (- len i)))))
    result))

(defun archive-int-to-mode (mode)
  "Turn an integer like 0700 (i.e., 448) into a mode string like -rwx------."
  ;; FIXME: merge with tar-grind-file-mode.
  (string
    (if (zerop (logand  8192 mode))
	(if (zerop (logand 16384 mode)) ?- ?d)
      ?c) ; completeness
    (if (zerop (logand   256 mode)) ?- ?r)
    (if (zerop (logand   128 mode)) ?- ?w)
    (if (zerop (logand    64 mode))
	(if (zerop (logand  1024 mode)) ?- ?S)
      (if (zerop (logand  1024 mode)) ?x ?s))
    (if (zerop (logand    32 mode)) ?- ?r)
    (if (zerop (logand    16 mode)) ?- ?w)
    (if (zerop (logand     8 mode))
	(if (zerop (logand  2048 mode)) ?- ?S)
      (if (zerop (logand  2048 mode)) ?x ?s))
    (if (zerop (logand     4 mode)) ?- ?r)
    (if (zerop (logand     2 mode)) ?- ?w)
    (if (zerop (logand     1 mode)) ?- ?x)))

(defun archive-calc-mode (oldmode newmode &optional error)
  "From the integer OLDMODE and the string NEWMODE calculate a new file mode.
NEWMODE may be an octal number including a leading zero in which case it
will become the new mode.\n
NEWMODE may also be a relative specification like \"og-rwx\" in which case
OLDMODE will be modified accordingly just like chmod(2) would have done.\n
If optional third argument ERROR is non-nil an error will be signaled if
the mode is invalid.  If ERROR is nil then nil will be returned."
  (cond ((string-match "^0[0-7]*$" newmode)
	 (let ((result 0)
	       (len (length newmode))
	       (i 1))
	   (while (< i len)
	     (setq result (+ (lsh result 3) (aref newmode i) (- ?0))
		   i (1+ i)))
	   (logior (logand oldmode 65024) result)))
	((string-match "^\\([agou]+\\)\\([---+=]\\)\\([rwxst]+\\)$" newmode)
	 (let ((who 0)
	       (result oldmode)
	       (op (aref newmode (match-beginning 2)))
	       (bits 0)
	       (i (match-beginning 3)))
	   (while (< i (match-end 3))
	     (let ((rwx (aref newmode i)))
	       (setq bits (logior bits (cond ((= rwx ?r)  292)
					     ((= rwx ?w)  146)
					     ((= rwx ?x)   73)
					     ((= rwx ?s) 3072)
					     ((= rwx ?t)  512)))
		     i (1+ i))))
	   (while (< who (match-end 1))
	     (let* ((whoc (aref newmode who))
		    (whomask (cond ((= whoc ?a) 4095)
				   ((= whoc ?u) 1472)
				   ((= whoc ?g) 2104)
				   ((= whoc ?o)    7))))
	       (if (= op ?=)
		   (setq result (logand result (lognot whomask))))
	       (if (= op ?-)
		   (setq result (logand result (lognot (logand whomask bits))))
		 (setq result (logior result (logand whomask bits)))))
	     (setq who (1+ who)))
	   result))
	(t
	 (if error
	     (error "Invalid mode specification: %s" newmode)))))

(defun archive-dosdate (date)
  "Stringify dos packed DATE record."
  (let ((year (+ 1980 (logand (ash date -9) 127)))
        (month (logand (ash date -5) 15))
        (day (logand date 31)))
    (if (or (> month 12) (< month 1))
        ""
      (format "%2d-%s-%d"
              day
              (aref ["Jan" "Feb" "Mar" "Apr" "May" "Jun"
                     "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"] (1- month))
              year))))

(defun archive-dostime (time)
  "Stringify dos packed TIME record."
  (let ((hour (logand (ash time -11) 31))
        (minute (logand (ash time -5) 63))
        (second (* 2 (logand time 31)))) ; 2 seconds resolution
    (format "%02d:%02d:%02d" hour minute second)))

(defun archive-unixdate (low high)
  "Stringify Unix (LOW HIGH) date."
  (let ((str (current-time-string (cons high low))))
    (format "%s-%s-%s"
	    (substring str 8 10)
	    (substring str 4 7)
	    (substring str 20 24))))

(defun archive-unixtime (low high)
  "Stringify Unix (LOW HIGH) time."
  (let ((str (current-time-string (cons high low))))
    (substring str 11 19)))

(defun archive-get-lineno ()
  (if (>= (point) archive-file-list-start)
      (count-lines archive-file-list-start
		   (line-beginning-position))
    0))

(defun archive-get-descr (&optional noerror)
  "Return the descriptor vector for file at point.
Does not signal an error if optional argument NOERROR is non-nil."
  (let ((no (archive-get-lineno)))
    (if (and (>= (point) archive-file-list-start)
             (< no (length archive-files)))
	(let ((item (aref archive-files no)))
	  (if (vectorp item)
	      item
	    (if (not noerror)
		(error "Entry is not a regular member of the archive"))))
      (if (not noerror)
          (error "Line does not describe a member of the archive")))))
;; -------------------------------------------------------------------------
;;; Section: the mode definition

;;;###autoload
(defun archive-mode (&optional force)
  "Major mode for viewing an archive file in a dired-like way.
You can move around using the usual cursor motion commands.
Letters no longer insert themselves.
Type `e' to pull a file out of the archive and into its own buffer;
or click mouse-2 on the file's line in the archive mode buffer.

If you edit a sub-file of this archive (as with the `e' command) and
save it, the contents of that buffer will be saved back into the
archive.

\\{archive-mode-map}"
  ;; This is not interactive because you shouldn't be turning this
  ;; mode on and off.  You can corrupt things that way.
  (if (zerop (buffer-size))
      ;; At present we cannot create archives from scratch
      (funcall (or (default-value 'major-mode) 'fundamental-mode))
    (if (and (not force) archive-files) nil
      (let* ((type (archive-find-type))
	     (typename (capitalize (symbol-name type))))
	(kill-all-local-variables)
	(make-local-variable 'archive-subtype)
	(setq archive-subtype type)

	;; Buffer contains treated image of file before the file contents
	(make-local-variable 'revert-buffer-function)
	(setq revert-buffer-function 'archive-mode-revert)
	(auto-save-mode 0)

	;; Remote archives are not written by a hook.
	(if archive-remote nil
	  (add-hook 'write-contents-functions 'archive-write-file nil t))

	(make-local-variable 'require-final-newline)
	(setq require-final-newline nil)
	(make-local-variable 'local-enable-local-variables)
	(setq local-enable-local-variables nil)

	;; Prevent loss of data when saving the file.
	(make-local-variable 'file-precious-flag)
	(setq file-precious-flag t)

	(make-local-variable 'archive-read-only)
	;; Archives which are inside other archives and whose
	;; names are invalid for this OS, can't be written.
	(setq archive-read-only
	      (or (not (file-writable-p (buffer-file-name)))
		  (and archive-subfile-mode
		       (string-match file-name-invalid-regexp
				     (aref archive-subfile-mode 0)))))

	;; Should we use a local copy when accessing from outside Emacs?
	(make-local-variable 'archive-local-name)

	;; An archive can contain another archive whose name is invalid
	;; on local filesystem.  Treat such archives as remote.
	(or archive-remote
	    (setq archive-remote
		  (or (string-match archive-remote-regexp (buffer-file-name))
		      (string-match file-name-invalid-regexp
				    (buffer-file-name)))))

	(setq major-mode 'archive-mode)
	(setq mode-name (concat typename "-Archive"))
	;; Run archive-foo-mode-hook and archive-mode-hook
	(run-mode-hooks (archive-name "mode-hook") 'archive-mode-hook)
	(use-local-map archive-mode-map))

      (make-local-variable 'archive-proper-file-start)
      (make-local-variable 'archive-file-list-start)
      (make-local-variable 'archive-file-list-end)
      (make-local-variable 'archive-file-name-indent)
      (setq archive-file-name-coding-system
	    (or file-name-coding-system
		default-file-name-coding-system
		locale-coding-system))
      (if (default-value 'enable-multibyte-characters)
	  (set-buffer-multibyte 'to))
      (archive-summarize nil)
      (setq buffer-read-only t))))

;; Archive mode is suitable only for specially formatted data.
(put 'archive-mode 'mode-class 'special)

(let ((item1 '(archive-subfile-mode " Archive")))
  (or (member item1 minor-mode-alist)
      (setq minor-mode-alist (cons item1 minor-mode-alist))))
;; -------------------------------------------------------------------------
(defun archive-find-type ()
  (widen)
  (goto-char (point-min))
  ;; The funny [] here make it unlikely that the .elc file will be treated
  ;; as an archive by other software.
  (let (case-fold-search)
    (cond ((looking-at "\\(PK00\\)?[P]K\003\004") 'zip)
	  ((looking-at "..-l[hz][0-9ds]-") 'lzh)
	  ((looking-at "....................[\334]\247\304\375") 'zoo)
	  ((and (looking-at "\C-z")	; signature too simple, IMHO
		(string-match "\\.[aA][rR][cC]$"
			      (or buffer-file-name (buffer-name))))
	   'arc)
          ;; This pattern modeled on the BSD/GNU+Linux `file' command.
          ;; Have seen capital "LHA's", and file has lower case "LHa's" too.
          ;; Note this regexp is also in archive-exe-p.
          ((looking-at "MZ\\(.\\|\n\\)\\{34\\}LH[aA]'s SFX ") 'lzh-exe)
          ((looking-at "Rar!") 'rar)
          ((looking-at "!<arch>\n") 'ar)
          ((and (looking-at "MZ")
                (re-search-forward "Rar!" (+ (point) 100000) t))
           'rar-exe)
	  ((looking-at "7z\274\257\047\034") '7z)
	  (t (error "Buffer format not recognized")))))
;; -------------------------------------------------------------------------

(defun archive-desummarize ()
  (let ((inhibit-read-only t)
        (modified (buffer-modified-p)))
    (widen)
    (delete-region (point-min) archive-proper-file-start)
    (restore-buffer-modified-p modified)))


(defun archive-summarize (&optional shut-up)
  "Parse the contents of the archive file in the current buffer.
Place a dired-like listing on the front;
then narrow to it, so that only that listing
is visible (and the real data of the buffer is hidden).
Optional argument SHUT-UP, if non-nil, means don't print messages
when parsing the archive."
  (widen)
  (let ((inhibit-read-only t))
    (setq archive-proper-file-start (copy-marker (point-min) t))
    (set (make-local-variable 'change-major-mode-hook) 'archive-desummarize)
    (or shut-up
	(message "Parsing archive file..."))
    (buffer-disable-undo (current-buffer))
    (setq archive-files (funcall (archive-name "summarize")))
    (or shut-up
	(message "Parsing archive file...done."))
    (setq archive-proper-file-start (point-marker))
    (narrow-to-region (point-min) (point))
    (set-buffer-modified-p nil)
    (buffer-enable-undo))
  (goto-char archive-file-list-start)
  (archive-next-line 0))

(defun archive-resummarize ()
  "Recreate the contents listing of an archive."
  (let ((no (archive-get-lineno)))
    (archive-desummarize)
    (archive-summarize t)
    (goto-char archive-file-list-start)
    (archive-next-line no)))

(defun archive-summarize-files (files)
  "Insert a description of a list of files annotated with proper mouse face."
  (setq archive-file-list-start (point-marker))
  (setq archive-file-name-indent (if files (aref (car files) 1) 0))
  ;; We don't want to do an insert for each element since that takes too
  ;; long when the archive -- which has to be moved in memory -- is large.
  (insert
   (apply
    (function concat)
    (mapcar
     (lambda (fil)
       ;; Using `concat' here copies the text also, so we can add
       ;; properties without problems.
       (let ((text (concat (aref fil 0) "\n")))
         (if (featurep 'xemacs)
             ()                         ; out of luck
           (add-text-properties
            (aref fil 1) (aref fil 2)
            '(mouse-face highlight
              help-echo "mouse-2: extract this file into a buffer")
            text))
         text))
     files)))
  (setq archive-file-list-end (point-marker)))

(defun archive-alternate-display ()
  "Toggle alternative display.
To avoid very long lines archive mode does not show all information.
This function changes the set of information shown for each files."
  (interactive)
  (setq archive-alternate-display (not archive-alternate-display))
  (archive-resummarize))
;; -------------------------------------------------------------------------
;;; Section: Local archive copy handling

(defun archive-unique-fname (fname dir)
  "Make sure a file FNAME can be created uniquely in directory DIR.

If FNAME can be uniquely created in DIR, it is returned unaltered.
If FNAME is something our underlying filesystem can't grok, or if another
file by that name already exists in DIR, a unique new name is generated
using `make-temp-file', and the generated name is returned."
  (let ((fullname (expand-file-name fname dir))
	(alien (string-match file-name-invalid-regexp fname))
	(tmpfile
	 (expand-file-name
	  (if (if (fboundp 'msdos-long-file-names)
		  (not (msdos-long-file-names)))
	      "am"
	    "arc-mode.")
	  dir)))
    (if (or alien (file-exists-p fullname))
	(progn
	  ;; Make sure all the leading directories in
	  ;; archive-local-name exist under archive-tmpdir, so that
	  ;; the directory structure recorded in the archive is
	  ;; reconstructed in the temporary directory.
	  (make-directory (file-name-directory tmpfile) t)
	  (make-temp-file tmpfile))
      ;; Make sure all the leading directories in `fullname' exist
      ;; under archive-tmpdir.  This is necessary for nested archives
      ;; (`archive-extract' sets `archive-remote' to t in case
      ;; an archive occurs inside another archive).
      (make-directory (file-name-directory fullname) t)
      fullname)))

(defun archive-maybe-copy (archive)
  (let ((coding-system-for-write 'no-conversion))
    (if archive-remote
	(let ((start (point-max))
	      ;; Sometimes ARCHIVE is invalid while its actual name, as
	      ;; recorded in its parent archive, is not.  For example, an
	      ;; archive bar.zip inside another archive foo.zip gets a name
	      ;; "foo.zip:bar.zip", which is invalid on DOS/Windows.
	      ;; So use the actual name if available.
	      (archive-name
	       (or (and archive-subfile-mode (aref archive-subfile-mode 0))
		   archive)))
	  (setq archive-local-name
		(archive-unique-fname archive-name archive-tmpdir))
	  (save-restriction
	    (widen)
	    (write-region start (point-max) archive-local-name nil 'nomessage))
	  archive-local-name)
      (if (buffer-modified-p) (save-buffer))
      archive)))

(defun archive-maybe-update (unchanged)
  (if archive-remote
      (let ((name archive-local-name)
	    (modified (buffer-modified-p))
	    (coding-system-for-read 'no-conversion)
	    (lno (archive-get-lineno))
	    (inhibit-read-only t))
	(if unchanged nil
	  (setq archive-files nil)
	  (erase-buffer)
	  (insert-file-contents name)
	  (archive-mode t)
	  (goto-char archive-file-list-start)
	  (archive-next-line lno))
	(archive-delete-local name)
	(if (not unchanged)
	    (message
	     "Buffer `%s' must be saved for changes to take effect"
	     (buffer-name (current-buffer))))
	(set-buffer-modified-p (or modified (not unchanged))))))

(defun archive-delete-local (name)
  "Delete file NAME and its parents up to and including `archive-tmpdir'."
  (let ((again t)
	(top (directory-file-name (file-name-as-directory archive-tmpdir))))
    (condition-case nil
	(delete-file name)
      (error nil))
    (while again
      (setq name (directory-file-name (file-name-directory name)))
      (condition-case nil
	  (delete-directory name)
	(error nil))
      (if (string= name top) (setq again nil)))))
;; -------------------------------------------------------------------------
;;; Section: Member extraction

(defun archive-try-jka-compr ()
  (when (and auto-compression-mode
             (jka-compr-get-compression-info buffer-file-name))
    (let* ((basename (file-name-nondirectory buffer-file-name))
           (tmpname (if (string-match ":\\([^:]+\\)\\'" basename)
                        (match-string 1 basename) basename))
           (tmpfile (make-temp-file (file-name-sans-extension tmpname)
                                    nil
                                    (file-name-extension tmpname 'period))))
      (unwind-protect
          (progn
            (let ((coding-system-for-write 'no-conversion)
                  ;; Don't re-compress this data just before decompressing it.
                  (jka-compr-inhibit t))
              (write-region (point-min) (point-max) tmpfile nil 'quiet))
            (erase-buffer)
            (let ((coding-system-for-read 'no-conversion))
              (insert-file-contents tmpfile)))
        (delete-file tmpfile)))))

(defun archive-file-name-handler (op &rest args)
  (or (eq op 'file-exists-p)
      (let ((file-name-handler-alist nil))
	(apply op args))))

(defun archive-set-buffer-as-visiting-file (filename)
  "Set the current buffer as if it were visiting FILENAME."
  (save-excursion
    (goto-char (point-min))
    (let ((buffer-undo-list t)
	  (coding
	   (or coding-system-for-read
	       (and set-auto-coding-function
		    (save-excursion
		      (funcall set-auto-coding-function
			       filename (- (point-max) (point-min)))))
	       ;; dos-w32.el defines the function
	       ;; find-buffer-file-type-coding-system for DOS/Windows
	       ;; systems which preserves the coding-system of existing files.
	       ;; (That function is called via file-coding-system-alist.)
	       ;; Here, we want it to act as if the extracted file existed.
	       ;; The following let-binding of file-name-handler-alist forces
	       ;; find-file-not-found-set-buffer-file-coding-system to ignore
	       ;; the file's name (see dos-w32.el).
	       (let ((file-name-handler-alist
		      '(("" . archive-file-name-handler))))
		 (car (find-operation-coding-system
		       'insert-file-contents
		       (cons filename (current-buffer)) t))))))
      (unless (or coding-system-for-read
                  enable-multibyte-characters)
        (setq coding
              (coding-system-change-text-conversion coding 'raw-text)))
      (unless (memq coding '(nil no-conversion))
        (decode-coding-region (point-min) (point-max) coding)
	(setq last-coding-system-used coding))
      (set-buffer-modified-p nil)
      (kill-local-variable 'buffer-file-coding-system)
      (after-insert-file-set-coding (- (point-max) (point-min))))))

(define-obsolete-function-alias 'archive-mouse-extract 'archive-extract "22.1")

(defun archive-extract (&optional other-window-p event)
  "In archive mode, extract this entry of the archive into its own buffer."
  (interactive (list nil last-input-event))
  (if event (posn-set-point (event-end event)))
  (let* ((view-p (eq other-window-p 'view))
	 (descr (archive-get-descr))
         (ename (aref descr 0))
         (iname (aref descr 1))
         (archive-buffer (current-buffer))
         (arcdir default-directory)
         (archive (buffer-file-name))
         (arcname (file-name-nondirectory archive))
         (bufname (concat (file-name-nondirectory iname) " (" arcname ")"))
         (extractor (archive-name "extract"))
	 ;; Members with file names which aren't valid for the
	 ;; underlying filesystem, are treated as read-only.
         (read-only-p (or archive-read-only
			  view-p
			  (string-match file-name-invalid-regexp ename)))
	 (arcfilename (expand-file-name (concat arcname ":" iname)))
         (buffer (get-buffer bufname))
         (just-created nil)
	 (file-name-coding archive-file-name-coding-system))
      (if (and buffer
	       (string= (buffer-file-name buffer) arcfilename))
          nil
	(setq archive (archive-maybe-copy archive))
	(setq bufname (generate-new-buffer-name bufname))
        (setq buffer (get-buffer-create bufname))
        (setq just-created t)
        (with-current-buffer buffer
          (setq buffer-file-name arcfilename)
          (setq buffer-file-truename
                (abbreviate-file-name buffer-file-name))
          ;; Set the default-directory to the dir of the superior buffer.
          (setq default-directory arcdir)
          (make-local-variable 'archive-superior-buffer)
          (setq archive-superior-buffer archive-buffer)
          (add-hook 'write-file-functions 'archive-write-file-member nil t)
          (setq archive-subfile-mode descr)
	  (setq archive-file-name-coding-system file-name-coding)
	  (if (and
	       (null
		(let (;; We may have to encode the file name argument for
		      ;; external programs.
		      (coding-system-for-write
		       (and enable-multibyte-characters
			    archive-file-name-coding-system))
		      ;; We read an archive member by no-conversion at
		      ;; first, then decode appropriately by calling
		      ;; archive-set-buffer-as-visiting-file later.
		      (coding-system-for-read 'no-conversion))
		  (condition-case err
		      (if (fboundp extractor)
			  (funcall extractor archive ename)
			(archive-*-extract archive ename
					   (symbol-value extractor)))
		    (error
		     (ding (message "%s" (error-message-string err)))
		     nil))))
	       just-created)
	      (progn
		(set-buffer-modified-p nil)
		(kill-buffer buffer))
            (archive-try-jka-compr)     ;Pretty ugly hack :-(
	    (archive-set-buffer-as-visiting-file ename)
	    (goto-char (point-min))
	    (rename-buffer bufname)
	    (setq buffer-read-only read-only-p)
	    (setq buffer-undo-list nil)
	    (set-buffer-modified-p nil)
	    (setq buffer-saved-size (buffer-size))
	    (normal-mode)
	    ;; Just in case an archive occurs inside another archive.
	    (when (derived-mode-p 'archive-mode)
              (setq archive-remote t)
              (if read-only-p (setq archive-read-only t))
              ;; We will write out the archive ourselves if it is
              ;; part of another archive.
              (remove-hook 'write-contents-functions 'archive-write-file t))
            (run-hooks 'archive-extract-hooks)
	    (if archive-read-only
		(message "Note: altering this archive is not implemented."))))
	(archive-maybe-update t))
      (or (not (buffer-name buffer))
          (cond
           (view-p
	    (view-buffer buffer (and just-created 'kill-buffer-if-not-modified)))
           ((eq other-window-p 'display) (display-buffer buffer))
           (other-window-p (switch-to-buffer-other-window buffer))
           (t (switch-to-buffer buffer))))))

(defun archive-*-extract (archive name command)
  (let* ((default-directory (file-name-as-directory archive-tmpdir))
	 (tmpfile (expand-file-name (file-name-nondirectory name)
				    default-directory))
	 exit-status success)
    (make-directory (directory-file-name default-directory) t)
    (setq exit-status
	  (apply 'call-process
		 (car command)
		 nil
		 nil
		 nil
		 (append (cdr command) (list archive name))))
    (cond ((and (numberp exit-status) (zerop exit-status))
	   (if (not (file-exists-p tmpfile))
	       (ding (message "`%s': no such file or directory" tmpfile))
	     (insert-file-contents tmpfile)
	     (setq success t)))
	  ((numberp exit-status)
	   (ding
	    (message "`%s' exited with status %d" (car command) exit-status)))
	  ((stringp exit-status)
	   (ding (message "`%s' aborted: %s" (car command) exit-status)))
	  (t
	   (ding (message "`%s' failed" (car command)))))
    (archive-delete-local tmpfile)
    success))

(defun archive-extract-by-stdout (archive name command &optional stderr-file)
  (apply 'call-process
	 (car command)
	 nil
	 (if stderr-file (list t stderr-file) t)
	 nil
	 (append (cdr command) (list archive name))))

(defun archive-extract-other-window ()
  "In archive mode, find this member in another window."
  (interactive)
  (archive-extract t))

(defun archive-display-other-window ()
  "In archive mode, display this member in another window."
  (interactive)
  (archive-extract 'display))

(defun archive-view ()
  "In archive mode, view the member on this line."
  (interactive)
  (archive-extract 'view))

(defun archive-add-new-member (arcbuf name)
  "Add current buffer to the archive in ARCBUF naming it NAME."
  (interactive
   (list (get-buffer
	  (read-buffer "Buffer containing archive: "
		       ;; Find first archive buffer and suggest that
		       (let ((bufs (buffer-list)))
			 (while (and bufs
                                     (not (with-current-buffer (car bufs)
                                            (derived-mode-p 'archive-mode))))
                           (setq bufs (cdr bufs)))
			 (if bufs
			     (car bufs)
			   (error "There are no archive buffers")))
		       t))
	 (read-string "File name in archive: "
		      (if buffer-file-name
			  (file-name-nondirectory buffer-file-name)
			""))))
  (with-current-buffer arcbuf
    (or (derived-mode-p 'archive-mode)
	(error "Buffer is not an archive buffer"))
    (if archive-read-only
	(error "Archive is read-only")))
  (if (eq arcbuf (current-buffer))
      (error "An archive buffer cannot be added to itself"))
  (if (string= name "")
      (error "Archive members may not be given empty names"))
  (let ((func (with-current-buffer arcbuf
                (archive-name "add-new-member")))
	(membuf (current-buffer)))
    (if (fboundp func)
	(with-current-buffer arcbuf
	  (funcall func buffer-file-name membuf name))
      (error "Adding a new member is not supported for this archive type"))))
;; -------------------------------------------------------------------------
;;; Section: IO stuff

(defun archive-write-file-member ()
  (save-excursion
    (save-restriction
      (message "Updating archive...")
      (widen)
      (let ((writer  (with-current-buffer archive-superior-buffer
                       (archive-name "write-file-member")))
	    (archive (with-current-buffer archive-superior-buffer
                       (archive-maybe-copy (buffer-file-name)))))
	(if (fboundp writer)
	    (funcall writer archive archive-subfile-mode)
	  (archive-*-write-file-member archive
				       archive-subfile-mode
				       (symbol-value writer)))
	(set-buffer-modified-p nil)
	(message "Updating archive...done"))
      (set-buffer archive-superior-buffer)
      (if (not archive-remote) (revert-buffer) (archive-maybe-update nil))))
  ;; Restore the value of last-coding-system-used, so that basic-save-buffer
  ;; won't reset the coding-system of this archive member.
  (if (local-variable-p 'archive-member-coding-system)
      (setq last-coding-system-used archive-member-coding-system))
  t)

(defun archive-*-write-file-member (archive descr command)
  (let* ((ename (aref descr 0))
         (tmpfile (expand-file-name ename archive-tmpdir))
         (top (directory-file-name (file-name-as-directory archive-tmpdir)))
	 (default-directory (file-name-as-directory top)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory tmpfile) t)
	  ;; If the member is itself an archive, write it without
	  ;; the dired-like listing we created.
	  (if (eq major-mode 'archive-mode)
	      (archive-write-file tmpfile)
	    (write-region nil nil tmpfile nil 'nomessage))
	  ;; basic-save-buffer needs last-coding-system-used to have
	  ;; the value used to write the file, so save it before any
	  ;; further processing clobbers it (we restore it in
	  ;; archive-write-file-member, above).
	  (setq archive-member-coding-system last-coding-system-used)
	  (if (aref descr 3)
	      ;; Set the file modes, but make sure we can read it.
	      (set-file-modes tmpfile (logior ?\400 (aref descr 3))))
	  (setq ename
		(encode-coding-string ename archive-file-name-coding-system))
          (let* ((coding-system-for-write 'no-conversion)
		 (exitcode (apply 'call-process
				  (car command)
				  nil
				  nil
				  nil
				  (append (cdr command)
					  (list archive ename)))))
            (or (zerop exitcode)
		(error "Updating was unsuccessful (%S)" exitcode))))
      (archive-delete-local tmpfile))))

(defun archive-write-file (&optional file)
  (save-excursion
    (let ((coding-system-for-write 'no-conversion))
      (write-region archive-proper-file-start (point-max)
		    (or file buffer-file-name) nil t)
      (set-buffer-modified-p nil))
    t))
;; -------------------------------------------------------------------------
;;; Section: Marking and unmarking.

(defun archive-flag-deleted (p &optional type)
  "In archive mode, mark this member to be deleted from the archive.
With a prefix argument, mark that many files."
  (interactive "p")
  (or type (setq type ?D))
  (beginning-of-line)
  (let ((sign (if (>= p 0) +1 -1))
	(modified (buffer-modified-p))
        (inhibit-read-only t))
    (while (not (zerop p))
      (if (archive-get-descr t)
          (progn
            (delete-char 1)
            (insert type)))
      (forward-line sign)
      (setq p (- p sign)))
    (restore-buffer-modified-p modified))
  (archive-next-line 0))

(defun archive-unflag (p)
  "In archive mode, un-mark this member if it is marked to be deleted.
With a prefix argument, un-mark that many files forward."
  (interactive "p")
  (archive-flag-deleted p ?\s))

(defun archive-unflag-backwards (p)
  "In archive mode, un-mark this member if it is marked to be deleted.
With a prefix argument, un-mark that many members backward."
  (interactive "p")
  (archive-flag-deleted (- p) ?\s))

(defun archive-unmark-all-files ()
  "Remove all marks."
  (interactive)
  (let ((modified (buffer-modified-p))
	(inhibit-read-only t))
    (save-excursion
      (goto-char archive-file-list-start)
      (while (< (point) archive-file-list-end)
        (or (= (following-char) ?\s)
            (progn (delete-char 1) (insert ?\s)))
        (forward-line 1)))
    (restore-buffer-modified-p modified)))

(defun archive-mark (p)
  "In archive mode, mark this member for group operations.
With a prefix argument, mark that many members.
Use \\[archive-unmark-all-files] to remove all marks."
  (interactive "p")
  (archive-flag-deleted p ?*))

(defun archive-get-marked (mark &optional default)
  (let (files)
    (save-excursion
      (goto-char archive-file-list-start)
      (while (< (point) archive-file-list-end)
        (if (= (following-char) mark)
	    (setq files (cons (archive-get-descr) files)))
        (forward-line 1)))
    (or (nreverse files)
	(and default
	     (list (archive-get-descr))))))
;; -------------------------------------------------------------------------
;;; Section: Operate

(defun archive-next-line (p)
  (interactive "p")
  (forward-line p)
  (or (eobp)
      (forward-char archive-file-name-indent)))

(defun archive-previous-line (p)
  (interactive "p")
  (archive-next-line (- p)))

(defun archive-chmod-entry (new-mode)
  "Change the protection bits associated with all marked or this member.
The new protection bits can either be specified as an octal number or
as a relative change like \"g+rw\" as for chmod(2)."
  (interactive "sNew mode (octal or relative): ")
  (if archive-read-only (error "Archive is read-only"))
  (let ((func (archive-name "chmod-entry")))
    (if (fboundp func)
	(progn
	  (funcall func new-mode (archive-get-marked ?* t))
	  (archive-resummarize))
      (error "Setting mode bits is not supported for this archive type"))))

(defun archive-chown-entry (new-uid)
  "Change the owner of all marked or this member."
  (interactive "nNew uid: ")
  (if archive-read-only (error "Archive is read-only"))
  (let ((func (archive-name "chown-entry")))
    (if (fboundp func)
	(progn
	  (funcall func new-uid (archive-get-marked ?* t))
	  (archive-resummarize))
      (error "Setting owner is not supported for this archive type"))))

(defun archive-chgrp-entry (new-gid)
  "Change the group of all marked or this member."
  (interactive "nNew gid: ")
  (if archive-read-only (error "Archive is read-only"))
  (let ((func (archive-name "chgrp-entry")))
    (if (fboundp func)
	(progn
	  (funcall func new-gid (archive-get-marked ?* t))
	  (archive-resummarize))
      (error "Setting group is not supported for this archive type"))))

(defun archive-expunge ()
  "Do the flagged deletions."
  (interactive)
  (let (files)
    (save-excursion
      (goto-char archive-file-list-start)
      (while (< (point) archive-file-list-end)
        (if (= (following-char) ?D)
	    (setq files (cons (aref (archive-get-descr) 0) files)))
        (forward-line 1)))
    (setq files (nreverse files))
    (and files
	 (or (not archive-read-only)
	     (error "Archive is read-only"))
	 (or (yes-or-no-p (format "Really delete %d member%s? "
				  (length files)
				  (if (null (cdr files)) "" "s")))
	     (error "Operation aborted"))
	 (let ((archive (archive-maybe-copy (buffer-file-name)))
	       (expunger (archive-name "expunge")))
	   (if (fboundp expunger)
	       (funcall expunger archive files)
	     (archive-*-expunge archive files (symbol-value expunger)))
	   (archive-maybe-update nil)
	   (if archive-remote
	       (archive-resummarize)
	     (revert-buffer))))))

(defun archive-*-expunge (archive files command)
  (apply 'call-process
	 (car command)
	 nil
	 nil
	 nil
	 (append (cdr command) (cons archive files))))

(defun archive-rename-entry (newname)
  "Change the name associated with this entry in the archive file."
  (interactive "sNew name: ")
  (if archive-read-only (error "Archive is read-only"))
  (if (string= newname "")
      (error "Archive members may not be given empty names"))
  (let ((func (archive-name "rename-entry"))
	(descr (archive-get-descr)))
    (if (fboundp func)
        (progn
	  (funcall func
		   (encode-coding-string newname
					 archive-file-name-coding-system)
		   descr)
	  (archive-resummarize))
      (error "Renaming is not supported for this archive type"))))

;; Revert the buffer and recompute the dired-like listing.
(defun archive-mode-revert (&optional _no-auto-save _no-confirm)
  (let ((no (archive-get-lineno)))
    (setq archive-files nil)
    (let ((revert-buffer-function nil)
	  (coding-system-for-read 'no-conversion))
      (revert-buffer t t))
    (archive-mode)
    (goto-char archive-file-list-start)
    (archive-next-line no)))

(defun archive-undo ()
  "Undo in an archive buffer.
This doesn't recover lost files, it just undoes changes in the buffer itself."
  (interactive)
  (let ((inhibit-read-only t))
    (undo)))
;; -------------------------------------------------------------------------
;;; Section: Arc Archives

(defun archive-arc-summarize ()
  (let ((p 1)
	(totalsize 0)
	(maxlen 8)
        files
	visual)
    (while (and (< (+ p 29) (point-max))
		(= (byte-after p) ?\C-z)
		(> (byte-after (1+ p)) 0))
      (let* ((namefld (buffer-substring (+ p 2) (+ p 2 13)))
	     (fnlen   (or (string-match "\0" namefld) 13))
	     (efnname (decode-coding-string (substring namefld 0 fnlen)
					    archive-file-name-coding-system))
	     ;; Convert to float to avoid overflow for very large files.
             (csize   (archive-l-e (+ p 15) 4 'float))
             (moddate (archive-l-e (+ p 19) 2))
             (modtime (archive-l-e (+ p 21) 2))
             (ucsize  (archive-l-e (+ p 25) 4 'float))
	     (fiddle  (string= efnname (upcase efnname)))
             (ifnname (if fiddle (downcase efnname) efnname))
             (text    (format "  %8.0f  %-11s  %-8s  %s"
                              ucsize
                              (archive-dosdate moddate)
                              (archive-dostime modtime)
                              ifnname)))
        (setq maxlen (max maxlen fnlen)
	      totalsize (+ totalsize ucsize)
	      visual (cons (vector text
				   (- (length text) (length ifnname))
				   (length text))
			   visual)
	      files (cons (vector efnname ifnname fiddle nil (1- p))
                          files)
	      ;; p needs to stay an integer, since we use it in char-after
	      ;; above.  Passing through `round' limits the compressed size
	      ;; to most-positive-fixnum, but if the compressed size exceeds
	      ;; that, we cannot visit the archive anyway.
              p (+ p 29 (round csize)))))
    (goto-char (point-min))
    (let ((dash (concat "- --------  -----------  --------  "
			(make-string maxlen ?-)
			"\n")))
      (insert "M   Length  Date         Time      File\n"
	      dash)
      (archive-summarize-files (nreverse visual))
      (insert dash
	      (format "  %8.0f                         %d file%s"
		      totalsize
		      (length files)
		      (if (= 1 (length files)) "" "s"))
	      "\n"))
    (apply 'vector (nreverse files))))

(defun archive-arc-rename-entry (newname descr)
  (if (string-match "[:\\\\/]" newname)
      (error "File names in arc files must not contain a directory component"))
  (if (> (length newname) 12)
      (error "File names in arc files are limited to 12 characters"))
  (let ((name (concat newname (substring "\0\0\0\0\0\0\0\0\0\0\0\0\0"
					 (length newname))))
	(inhibit-read-only t))
    (save-restriction
      (save-excursion
	(widen)
	(goto-char (+ archive-proper-file-start (aref descr 4) 2))
	(delete-char 13)
	(insert-unibyte name)))))
;; -------------------------------------------------------------------------
;;; Section: Lzh Archives

(defun archive-lzh-summarize (&optional start)
  (let ((p (or start 1)) ;; 1 for .lzh, something further on for .exe
	(totalsize 0)
	(maxlen 8)
        files
	visual)
    (while (progn (goto-char p)		;beginning of a base header.
		  (looking-at "\\(.\\|\n\\)\\(.\\|\n\\)-l[hz][0-9ds]-"))
      (let* ((hsize   (byte-after p))	;size of the base header (level 0 and 1)
	     ;; Convert to float to avoid overflow for very large files.
	     (csize   (archive-l-e (+ p 7) 4 'float)) ;size of a compressed file to follow (level 0 and 2),
					;size of extended headers + the compressed file to follow (level 1).
             (ucsize  (archive-l-e (+ p 11) 4 'float))	;size of an uncompressed file.
	     (time1   (archive-l-e (+ p 15) 2))	;date/time (MSDOS format in level 0, 1 headers
	     (time2   (archive-l-e (+ p 17) 2))	;and UNIX format in level 2 header.)
	     (hdrlvl  (byte-after (+ p 20))) ;header level
	     thsize		;total header size (base + extensions)
	     fnlen efnname osid fiddle ifnname width p2
	     neh	;beginning of next extension header (level 1 and 2)
	     mode modestr uid gid text dir prname
	     gname uname modtime moddate)
	(if (= hdrlvl 3) (error "can't handle lzh level 3 header type"))
	(when (or (= hdrlvl 0) (= hdrlvl 1))
	  (setq fnlen   (byte-after (+ p 21))) ;filename length
	  (setq efnname (let ((str (buffer-substring (+ p 22) (+ p 22 fnlen))))	;filename from offset 22
			(decode-coding-string
			 str archive-file-name-coding-system)))
	  (setq p2      (+ p 22 fnlen))) ;
	(if (= hdrlvl 1)
            (setq neh (+ p2 3))         ;specific to level 1 header
	  (if (= hdrlvl 2)
              (setq neh (+ p 24))))     ;specific to level 2 header
	(if neh		;if level 1 or 2 we expect extension headers to follow
	    (let* ((ehsize (archive-l-e neh 2))	;size of the extension header
		   (etype (byte-after (+ neh 2)))) ;extension type
	      (while (not (= ehsize 0))
		  (cond
		 ((= etype 1)	;file name
		  (let ((i (+ neh 3)))
		    (while (< i (+ neh ehsize))
		      (setq efnname (concat efnname (char-to-string (byte-after i))))
		      (setq i (1+ i)))))
		 ((= etype 2)	;directory name
		  (let ((i (+ neh 3)))
		    (while (< i (+ neh ehsize))
				    (setq dir (concat dir
						       (if (= (byte-after i)
							      255)
							   "/"
							 (char-to-string
							  (char-after i)))))
				    (setq i (1+ i)))))
		 ((= etype 80)		;Unix file permission
		  (setq mode (archive-l-e (+ neh 3) 2)))
		 ((= etype 81)		;UNIX file group/user ID
		  (progn (setq uid (archive-l-e (+ neh 3) 2))
			 (setq gid (archive-l-e (+ neh 5) 2))))
		 ((= etype 82)		;UNIX file group name
		  (let ((i (+ neh 3)))
		    (while (< i (+ neh ehsize))
		      (setq gname (concat gname (char-to-string (char-after i))))
		      (setq i (1+ i)))))
		 ((= etype 83)		;UNIX file user name
		  (let ((i (+ neh 3)))
		    (while (< i (+ neh ehsize))
		      (setq uname (concat uname (char-to-string (char-after i))))
		      (setq i (1+ i)))))
		   )
		(setq neh (+ neh ehsize))
		(setq ehsize (archive-l-e neh 2))
		(setq etype (byte-after (+ neh 2))))
	      ;;get total header size for level 1 and 2 headers
	      (setq thsize (- neh p))))
	(if (= hdrlvl 0)  ;total header size
	    (setq thsize hsize))
        ;; OS ID field not present in level 0 header, use code 0 "generic"
        ;; in that case as per lha program header.c get_header()
	(setq osid (cond ((= hdrlvl 0)  0)
                         ((= hdrlvl 1)  (char-after (+ p 22 fnlen 2)))
                         ((= hdrlvl 2)  (char-after (+ p 23)))))
        ;; Filename fiddling must follow the lha program, otherwise the name
        ;; passed to "lha pq" etc won't match (which for an extract silently
        ;; results in no output).  As of version 1.14i it goes from the OS ID,
        ;; - For 'M' MSDOS: msdos_to_unix_filename() downcases always, and
        ;;   converts "\" to "/".
        ;; - For 0 generic: generic_to_unix_filename() downcases if there's
        ;;   no lower case already present, and converts "\" to "/".
        ;; - For 'm' MacOS: macos_to_unix_filename() changes "/" to ":" and
        ;;   ":" to "/"
	(setq fiddle (cond ((= ?M osid) t)
                           ((= 0 osid)  (string= efnname (upcase efnname)))))
	(setq ifnname (if fiddle (downcase efnname) efnname))
	(setq prname (if dir (concat dir ifnname) ifnname))
	(setq width (if prname (string-width prname) 0))
	(setq modestr (if mode (archive-int-to-mode mode) "??????????"))
	(setq moddate (if (= hdrlvl 2)
			  (archive-unixdate time1 time2) ;level 2 header in UNIX format
			(archive-dosdate time2))) ;level 0 and 1 header in DOS format
	(setq modtime (if (= hdrlvl 2)
			  (archive-unixtime time1 time2)
			(archive-dostime time1)))
	(setq text    (if archive-alternate-display
			  (format "  %8.0f  %5S  %5S  %s"
				  ucsize
				  (or uid "?")
				  (or gid "?")
				  ifnname)
			(format "  %10s  %8.0f  %-11s  %-8s  %s"
				modestr
				ucsize
				moddate
				modtime
				prname)))
        (setq maxlen (max maxlen width)
	      totalsize (+ totalsize ucsize)
	      visual (cons (vector text
				   (- (length text) (length prname))
				   (length text))
			   visual)
	      files (cons (vector prname ifnname fiddle mode (1- p))
                          files))
	(cond ((= hdrlvl 1)
	       ;; p needs to stay an integer, since we use it in goto-char
	       ;; above.  Passing through `round' limits the compressed size
	       ;; to most-positive-fixnum, but if the compressed size exceeds
	       ;; that, we cannot visit the archive anyway.
	       (setq p (+ p hsize 2 (round csize))))
	      ((or (= hdrlvl 2) (= hdrlvl 0))
	       (setq p (+ p thsize 2 (round csize)))))
	))
    (goto-char (point-min))
    (let ((dash (concat (if archive-alternate-display
			    "- --------  -----  -----  "
			  "- ----------  --------  -----------  --------  ")
			(make-string maxlen ?-)
			"\n"))
	  (header (if archive-alternate-display
		       "M   Length    Uid    Gid  File\n"
		    "M   Filemode    Length  Date         Time      File\n"))
	  (sumline (if archive-alternate-display
		       "  %8.0f                %d file%s"
		     "              %8.0f                         %d file%s")))
      (insert header dash)
      (archive-summarize-files (nreverse visual))
      (insert dash
	      (format sumline
		      totalsize
		      (length files)
		      (if (= 1 (length files)) "" "s"))
	      "\n"))
    (apply 'vector (nreverse files))))

(defconst archive-lzh-alternate-display t)

(defun archive-lzh-extract (archive name)
  (archive-extract-by-stdout archive name archive-lzh-extract))

(defun archive-lzh-resum (p count)
  (let ((sum 0))
    (while (> count 0)
      (setq count (1- count)
	    sum (+ sum (byte-after p))
	    p (1+ p)))
    (logand sum 255)))

(defun archive-lzh-rename-entry (newname descr)
  (save-restriction
    (save-excursion
      (widen)
      (let* ((p        (+ archive-proper-file-start (aref descr 4)))
	     (oldhsize (byte-after p))
	     (oldfnlen (byte-after (+ p 21)))
	     (newfnlen (length newname))
	     (newhsize (+ oldhsize newfnlen (- oldfnlen)))
	     (inhibit-read-only t))
	(if (> newhsize 255)
	    (error "The file name is too long"))
	(goto-char (+ p 21))
	(delete-char (1+ oldfnlen))
	(insert-unibyte newfnlen newname)
	(goto-char p)
	(delete-char 2)
	(insert-unibyte newhsize (archive-lzh-resum p newhsize))))))

(defun archive-lzh-ogm (newval files errtxt ofs)
  (save-excursion
    (save-restriction
      (widen)
      (dolist (fil files)
	(let* ((p (+ archive-proper-file-start (aref fil 4)))
	       (hsize   (byte-after p))
	       (fnlen   (byte-after (+ p 21)))
	       (p2      (+ p 22 fnlen))
	       (creator (if (>= (- hsize fnlen) 24) (byte-after (+ p2 2)) 0))
	       (inhibit-read-only t))
	  (if (= creator ?U)
	      (progn
		(or (numberp newval)
		    (setq newval (funcall newval (archive-l-e (+ p2 ofs) 2))))
		(goto-char (+ p2 ofs))
		(delete-char 2)
		(insert-unibyte (logand newval 255) (lsh newval -8))
		(goto-char (1+ p))
		(delete-char 1)
		(insert-unibyte (archive-lzh-resum (1+ p) hsize)))
	    (message "Member %s does not have %s field"
		     (aref fil 1) errtxt)))))))

(defun archive-lzh-chown-entry (newuid files)
  (archive-lzh-ogm newuid files "an uid" 10))

(defun archive-lzh-chgrp-entry (newgid files)
  (archive-lzh-ogm newgid files "a gid" 12))

(defun archive-lzh-chmod-entry (newmode files)
  (archive-lzh-ogm
   ;; This should work even though newmode will be dynamically accessed.
   (lambda (old) (archive-calc-mode old newmode t))
   files "a unix-style mode" 8))

;; -------------------------------------------------------------------------
;;; Section: Lzh Self-Extracting .exe Archives
;;
;; No support for modifying these files.  It looks like the lha for unix
;; program (as of version 1.14i) can't create or retain the DOS exe part.
;; If you do an "lha a" on a .exe for instance it renames and writes to a
;; plain .lzh.

(defun archive-lzh-exe-summarize ()
  "Summarize the contents of an LZH self-extracting exe, for `archive-mode'."

  ;; Skip the initial executable code part and apply archive-lzh-summarize
  ;; to the archive part proper.  The "-lh5-" etc regexp here for the start
  ;; is the same as in archive-find-type.
  ;;
  ;; The lha program (version 1.14i) does this in skip_msdos_sfx1_code() by
  ;; a similar scan.  It looks for "..-l..-" plus for level 0 or 1 a test of
  ;; the header checksum, or level 2 a test of the "attribute" and size.
  ;;
  (re-search-forward "..-l[hz][0-9ds]-" nil)
  (archive-lzh-summarize (match-beginning 0)))

;; `archive-lzh-extract' runs "lha pq", and that works for .exe as well as
;; .lzh files
(defalias 'archive-lzh-exe-extract 'archive-lzh-extract
  "Extract a member from an LZH self-extracting exe, for `archive-mode'.")

;; -------------------------------------------------------------------------
;;; Section: Zip Archives

(defun archive-zip-summarize ()
  (goto-char (- (point-max) (- 22 18)))
  (search-backward-regexp "[P]K\005\006")
  (let ((p (+ (point-min) (archive-l-e (+ (point) 16) 4)))
        (maxlen 8)
	(totalsize 0)
        files
	visual)
    (while (string= "PK\001\002" (buffer-substring p (+ p 4)))
      (let* ((creator (byte-after (+ p 5)))
	     ;; (method  (archive-l-e (+ p 10) 2))
             (modtime (archive-l-e (+ p 12) 2))
             (moddate (archive-l-e (+ p 14) 2))
	     ;; Convert to float to avoid overflow for very large files.
             (ucsize  (archive-l-e (+ p 24) 4 'float))
             (fnlen   (archive-l-e (+ p 28) 2))
             (exlen   (archive-l-e (+ p 30) 2))
             (fclen   (archive-l-e (+ p 32) 2))
             (lheader (archive-l-e (+ p 42) 4))
             (efnname (let ((str (buffer-substring (+ p 46) (+ p 46 fnlen))))
			(decode-coding-string
			 str archive-file-name-coding-system)))
	     (isdir   (and (= ucsize 0)
			   (string= (file-name-nondirectory efnname) "")))
	     (mode    (cond ((memq creator '(2 3)) ; Unix
			     (archive-l-e (+ p 40) 2))
			    ((memq creator '(0 5 6 7 10 11 15)) ; Dos etc.
			     (logior ?\444
				     (if isdir (logior 16384 ?\111) 0)
				     (if (zerop
					  (logand 1 (byte-after (+ p 38))))
					 ?\222 0)))
			    (t nil)))
	     (modestr (if mode (archive-int-to-mode mode) "??????????"))
	     (fiddle  (and archive-zip-case-fiddle
			   (not (not (memq creator '(0 2 4 5 9))))
			   (string= (upcase efnname) efnname)))
             (ifnname (if fiddle (downcase efnname) efnname))
	     (width (string-width ifnname))
             (text    (format "  %10s  %8.0f  %-11s  %-8s  %s"
			      modestr
                              ucsize
                              (archive-dosdate moddate)
                              (archive-dostime modtime)
                              ifnname)))
        (setq maxlen (max maxlen width)
	      totalsize (+ totalsize ucsize)
	      visual (cons (vector text
				   (- (length text) (length ifnname))
				   (length text))
			   visual)
	      files (cons (if isdir
			      nil
			    (vector efnname ifnname fiddle mode
				    (list (1- p) lheader)))
                          files)
              p (+ p 46 fnlen exlen fclen))))
    (goto-char (point-min))
    (let ((dash (concat "- ----------  --------  -----------  --------  "
			(make-string maxlen ?-)
			"\n")))
      (insert "M Filemode      Length  Date         Time      File\n"
	      dash)
      (archive-summarize-files (nreverse visual))
      (insert dash
	      (format "              %8.0f                         %d file%s"
		      totalsize
		      (length files)
		      (if (= 1 (length files)) "" "s"))
	      "\n"))
    (apply 'vector (nreverse files))))

(defun archive-zip-extract (archive name)
  (cond
   ((member-ignore-case (car archive-zip-extract) '("pkunzip" "pkzip"))
    (archive-*-extract archive name archive-zip-extract))
   ((equal (car archive-zip-extract) "7z")
    (let ((archive-7z-extract archive-zip-extract))
      (archive-7z-extract archive name)))
   (t
    (archive-extract-by-stdout
     archive
     ;; unzip expands wildcards in NAME, so we need to quote it.  But
     ;; not on DOS/Windows, since that fails extraction on those
     ;; systems (unless w32-quote-process-args is nil), and file names
     ;; with wildcards in zip archives don't work there anyway.
     ;; FIXME: Does pkunzip need similar treatment?
     (if (and (or (not (memq system-type '(windows-nt ms-dos)))
		  (and (boundp 'w32-quote-process-args)
		       (null w32-quote-process-args)))
	      (equal (car archive-zip-extract) "unzip"))
	 (shell-quote-argument name)
       name)
     archive-zip-extract))))

(defun archive-zip-write-file-member (archive descr)
  (archive-*-write-file-member
   archive
   descr
   (if (aref descr 2) archive-zip-update-case archive-zip-update)))

(defun archive-zip-chmod-entry (newmode files)
  (save-restriction
    (save-excursion
      (widen)
      (dolist (fil files)
	(let* ((p (+ archive-proper-file-start (car (aref fil 4))))
	       (creator (byte-after (+ p 5)))
	       (oldmode (aref fil 3))
	       (newval  (archive-calc-mode oldmode newmode t))
	       (inhibit-read-only t))
	  (cond ((memq creator '(2 3)) ; Unix
		 (goto-char (+ p 40))
		 (delete-char 2)
		 (insert-unibyte (logand newval 255) (lsh newval -8)))
		((memq creator '(0 5 6 7 10 11 15)) ; Dos etc.
		 (goto-char (+ p 38))
		 (insert-unibyte (logior (logand (byte-after (point)) 254)
					 (logand (logxor 1 (lsh newval -7)) 1)))
		 (delete-char 1))
		(t (message "Don't know how to change mode for this member"))))
        ))))
;; -------------------------------------------------------------------------
;;; Section: Zoo Archives

(defun archive-zoo-summarize ()
  (let ((p (1+ (archive-l-e 25 4)))
        (maxlen 8)
	(totalsize 0)
        files
	visual)
    (while (and (string= "\334\247\304\375" (buffer-substring p (+ p 4)))
		(> (archive-l-e (+ p 6) 4) 0))
      (let* ((next    (1+ (archive-l-e (+ p 6) 4)))
             (moddate (archive-l-e (+ p 14) 2))
             (modtime (archive-l-e (+ p 16) 2))
	     ;; Convert to float to avoid overflow for very large files.
             (ucsize  (archive-l-e (+ p 20) 4 'float))
	     (namefld (buffer-substring (+ p 38) (+ p 38 13)))
	     (dirtype (byte-after (+ p 4)))
	     (lfnlen  (if (= dirtype 2) (byte-after (+ p 56)) 0))
	     (ldirlen (if (= dirtype 2) (byte-after (+ p 57)) 0))
	     (fnlen   (or (string-match "\0" namefld) 13))
	     (efnname (let ((str
			     (concat
			      (if (> ldirlen 0)
				  (concat (buffer-substring
					   (+ p 58 lfnlen)
					   (+ p 58 lfnlen ldirlen -1))
					  "/")
				"")
			      (if (> lfnlen 0)
				  (buffer-substring (+ p 58)
						    (+ p 58 lfnlen -1))
				(substring namefld 0 fnlen)))))
			(decode-coding-string
			 str archive-file-name-coding-system)))
	     (fiddle  (and (= lfnlen 0) (string= efnname (upcase efnname))))
             (ifnname (if fiddle (downcase efnname) efnname))
	     (width (string-width ifnname))
             (text    (format "  %8.0f  %-11s  %-8s  %s"
                              ucsize
                              (archive-dosdate moddate)
                              (archive-dostime modtime)
                              ifnname)))
        (setq maxlen (max maxlen width)
	      totalsize (+ totalsize ucsize)
	      visual (cons (vector text
				   (- (length text) (length ifnname))
				   (length text))
			   visual)
	      files (cons (vector efnname ifnname fiddle nil (1- p))
                          files)
              p next)))
    (goto-char (point-min))
    (let ((dash (concat "- --------  -----------  --------  "
			(make-string maxlen ?-)
			"\n")))
      (insert "M   Length  Date         Time      File\n"
	      dash)
      (archive-summarize-files (nreverse visual))
      (insert dash
	      (format "  %8.0f                         %d file%s"
		      totalsize
		      (length files)
		      (if (= 1 (length files)) "" "s"))
	      "\n"))
    (apply 'vector (nreverse files))))

(defun archive-zoo-extract (archive name)
  (archive-extract-by-stdout archive name archive-zoo-extract))

;; -------------------------------------------------------------------------
;;; Section: Rar Archives

(defun archive-rar-summarize (&optional file)
  ;; File is used internally for `archive-rar-exe-summarize'.
  (unless file (setq file buffer-file-name))
  (let* ((copy (file-local-copy file))
         (maxname 10)
         (maxsize 5)
         (files ()))
    (with-temp-buffer
      (call-process "unrar-free" nil t nil "--list" (or file copy))
      (if copy (delete-file copy))
      (goto-char (point-min))
      (re-search-forward "^-+\n")
      (while (looking-at (concat " \\(.*\\)\n" ;Name.
                                 ;; Size ; Packed.
                                 " +\\([0-9]+\\) +[0-9]+"
                                 ;; Ratio ; Date'
                                 " +\\([0-9%]+\\) +\\([-0-9]+\\)"
                                 ;; Time ; Attr.
                                 " +\\([0-9:]+\\) +[^ \n]\\{6,10\\}"
                                 ;; CRC; Meth ; Var.
                                 " +[0-9A-F]+ +[^ \n]+ +[0-9.]+\n"))
        (goto-char (match-end 0))
        (let ((name (match-string 1))
              (size (match-string 2)))
          (if (> (length name) maxname) (setq maxname (length name)))
          (if (> (length size) maxsize) (setq maxsize (length size)))
          (push (vector name name nil nil
                        ;; Size, Ratio.
                        size (match-string 3)
                        ;; Date, Time.
                        (match-string 4) (match-string 5))
                files))))
    (setq files (nreverse files))
    (goto-char (point-min))
    (let* ((format (format " %%s %%s  %%%ds %%5s  %%s" maxsize))
           (sep (format format "--------" "-----" (make-string maxsize ?-)
                        "-----" ""))
           (column (length sep)))
      (insert (format format "  Date  " "Time " "Size " "Ratio" " Filename") "\n")
      (insert sep (make-string maxname ?-) "\n")
      (archive-summarize-files (mapcar (lambda (desc)
                                         (let ((text
                                                (format format
                                                         (aref desc 6)
                                                         (aref desc 7)
                                                         (aref desc 4)
                                                         (aref desc 5)
                                                         (aref desc 1))))
                                           (vector text
                                                   column
                                                   (length text))))
                                       files))
      (insert sep (make-string maxname ?-) "\n")
      (apply 'vector files))))

(defun archive-rar-extract (archive name)
  ;; unrar-free seems to have no way to extract to stdout or even to a file.
  (if (file-name-absolute-p name)
      ;; The code below assumes the name is relative and may do undesirable
      ;; things otherwise.
      (error "Can't extract files with non-relative names")
    (let ((dest (make-temp-file "arc-rar" 'dir)))
      (unwind-protect
          (progn
            (call-process "unrar-free" nil nil nil
                          "--extract" archive name dest)
            (insert-file-contents-literally (expand-file-name name dest)))
        (delete-file (expand-file-name name dest))
        (while (file-name-directory name)
          (setq name (directory-file-name (file-name-directory name)))
          (delete-directory (expand-file-name name dest)))
        (delete-directory dest)))))

;;; Section: Rar self-extracting .exe archives.

(defun archive-rar-exe-summarize ()
  (let ((tmpfile (make-temp-file "rarexe")))
    (unwind-protect
        (progn
          (goto-char (point-min))
          (re-search-forward "Rar!")
          (write-region (match-beginning 0) (point-max) tmpfile)
          (archive-rar-summarize tmpfile))
      (delete-file tmpfile))))

(defun archive-rar-exe-extract (archive name)
  (let* ((tmpfile (make-temp-file "rarexe"))
         (buf (find-buffer-visiting archive))
         (tmpbuf (unless buf (generate-new-buffer " *rar-exe*"))))
    (unwind-protect
        (progn
          (with-current-buffer (or buf tmpbuf)
            (save-excursion
              (save-restriction
                (if buf
                    ;; point-max unwidened is assumed to be the end of the
                    ;; summary text and the beginning of the actual file data.
                    (progn (goto-char (point-max)) (widen))
                  (insert-file-contents-literally archive)
                  (goto-char (point-min)))
                (re-search-forward "Rar!")
                (write-region (match-beginning 0) (point-max) tmpfile))))
          (archive-rar-extract tmpfile name))
      (if tmpbuf (kill-buffer tmpbuf))
      (delete-file tmpfile))))

;; -------------------------------------------------------------------------
;;; Section: 7z Archives

(defun archive-7z-summarize ()
  (let ((maxname 10)
	(maxsize 5)
	(file buffer-file-name)
	(files ()))
    (with-temp-buffer
      (call-process "7z" nil t nil "l" "-slt" file)
      (goto-char (point-min))
      ;; Four dashes start the meta info section that should be skipped.
      ;; Archive members start with more than four dashes.
      (re-search-forward "^-----+\n")
      (while (re-search-forward "^Path = \\(.*\\)\n" nil t)
        (goto-char (match-end 0))
        (let ((name (match-string 1))
              (size (save-excursion
		      (and (re-search-forward "^Size = \\(.*\\)\n")
			   (match-string 1))))
	      (time (save-excursion
		      (and (re-search-forward "^Modified = \\(.*\\)\n")
			   (match-string 1)))))
          (if (> (length name) maxname) (setq maxname (length name)))
          (if (> (length size) maxsize) (setq maxsize (length size)))
          (push (vector name name nil nil time nil nil size)
                files))))
    (setq files (nreverse files))
    (goto-char (point-min))
    (let* ((format (format " %%%ds %%s %%s" maxsize))
           (sep (format format (make-string maxsize ?-) "-------------------" ""))
           (column (length sep)))
      (insert (format format "Size " "Date       Time    " " Filename") "\n")
      (insert sep (make-string maxname ?-) "\n")
      (archive-summarize-files (mapcar (lambda (desc)
                                         (let ((text
                                                (format format
							(aref desc 7)
							(aref desc 4)
							(aref desc 1))))
                                           (vector text
                                                   column
                                                   (length text))))
                                       files))
      (insert sep (make-string maxname ?-) "\n")
      (apply 'vector files))))

(defun archive-7z-extract (archive name)
  (let ((tmpfile (make-temp-file "7z-stderr")))
    ;; 7z doesn't provide a `quiet' option to suppress non-essential
    ;; stderr messages.  So redirect stderr to a temp file and display it
    ;; in the echo area when it contains error messages.
    (prog1 (archive-extract-by-stdout
	    archive name archive-7z-extract tmpfile)
      (with-temp-buffer
	(insert-file-contents tmpfile)
	(unless (search-forward "Everything is Ok" nil t)
	  (message "%s" (buffer-string)))
	(delete-file tmpfile)))))

(defun archive-7z-write-file-member (archive descr)
  (archive-*-write-file-member
   archive
   descr
   archive-7z-update))

;; -------------------------------------------------------------------------
;;; Section `ar' archives.

;; TODO: we currently only handle the basic format of ar archives,
;; not the GNU nor the BSD extensions.  As it turns out, this is sufficient
;; for .deb packages.

(autoload 'tar-grind-file-mode "tar-mode")

(defconst archive-ar-file-header-re
  "\\(.\\{16\\}\\)\\([ 0-9]\\{12\\}\\)\\([ 0-9]\\{6\\}\\)\\([ 0-9]\\{6\\}\\)\\([ 0-7]\\{8\\}\\)\\([ 0-9]\\{10\\}\\)`\n")

(defun archive-ar-summarize ()
  ;; File is used internally for `archive-rar-exe-summarize'.
  (let* ((maxname 10)
         (maxtime 16)
         (maxuser 5)
         (maxgroup 5)
         (maxmode 8)
         (maxsize 5)
         (files ()))
    (goto-char (point-min))
    (search-forward "!<arch>\n")
    (while (looking-at archive-ar-file-header-re)
      (let ((name (match-string 1))
            extname
            ;; Emacs will automatically use float here because those
            ;; timestamps don't fit in our ints.
            (time (string-to-number (match-string 2)))
            (user (match-string 3))
            (group (match-string 4))
            (mode (string-to-number (match-string 5) 8))
            (size (string-to-number (match-string 6))))
        ;; Move to the beginning of the data.
        (goto-char (match-end 0))
        (setq time
              (format-time-string
               "%Y-%m-%d %H:%M"
               (let ((high (truncate (/ time 65536))))
                 (list high (truncate (- time (* 65536.0 high)))))))
        (setq extname
              (cond ((equal name "//              ")
                     (propertize ".<ExtNamesTable>." 'face 'italic))
                    ((equal name "/               ")
                     (propertize ".<LookupTable>." 'face 'italic))
                    ((string-match "/? *\\'" name)
                     (substring name 0 (match-beginning 0)))))
        (setq user (substring user 0 (string-match " +\\'" user)))
        (setq group (substring group 0 (string-match " +\\'" group)))
        (setq mode (tar-grind-file-mode mode))
        ;; Move to the end of the data.
        (forward-char size) (if (eq ?\n (char-after)) (forward-char 1))
        (setq size (number-to-string size))
        (if (> (length name) maxname) (setq maxname (length name)))
        (if (> (length time) maxtime) (setq maxtime (length time)))
        (if (> (length user) maxuser) (setq maxuser (length user)))
        (if (> (length group) maxgroup) (setq maxgroup (length group)))
        (if (> (length mode) maxmode) (setq maxmode (length mode)))
        (if (> (length size) maxsize) (setq maxsize (length size)))
        (push (vector name extname nil mode
                      time user group size)
              files)))
    (setq files (nreverse files))
    (goto-char (point-min))
    (let* ((format (format "%%%ds %%%ds/%%-%ds  %%%ds %%%ds %%s"
                           maxmode maxuser maxgroup maxsize maxtime))
           (sep (format format (make-string maxmode ?-)
                         (make-string maxuser ?-)
                          (make-string maxgroup ?-)
                           (make-string maxsize ?-)
                           (make-string maxtime ?-) ""))
           (column (length sep)))
      (insert (format format "  Mode  " "User" "Group" " Size "
                      "      Date      " "Filename")
              "\n")
      (insert sep (make-string maxname ?-) "\n")
      (archive-summarize-files (mapcar (lambda (desc)
                                         (let ((text
                                                (format format
                                                         (aref desc 3)
                                                         (aref desc 5)
                                                         (aref desc 6)
                                                         (aref desc 7)
                                                         (aref desc 4)
                                                         (aref desc 1))))
                                           (vector text
                                                   column
                                                   (length text))))
                                       files))
      (insert sep (make-string maxname ?-) "\n")
      (apply 'vector files))))

(defun archive-ar-extract (archive name)
  (let ((destbuf (current-buffer))
        (archivebuf (find-file-noselect archive))
        (from nil) size)
    (with-current-buffer archivebuf
      (save-restriction
        ;; We may be in archive-mode or not, so either with or without
        ;; narrowing and with or without a prepended summary.
        (save-excursion
          (widen)
          (search-forward "!<arch>\n")
          (while (and (not from) (looking-at archive-ar-file-header-re))
            (let ((this (match-string 1)))
              (setq size (string-to-number (match-string 6)))
              (goto-char (match-end 0))
              (if (equal name this)
                  (setq from (point))
                ;; Move to the end of the data.
                (forward-char size) (if (eq ?\n (char-after)) (forward-char 1)))))
          (when from
            (set-buffer-multibyte nil)
            (with-current-buffer destbuf
              ;; Do it within the `widen'.
              (insert-buffer-substring archivebuf from (+ from size)))
            (set-buffer-multibyte 'to)
            ;; Inform the caller that the call succeeded.
            t))))))

;; -------------------------------------------------------------------------
;; This line was a mistake; it is kept now for compatibility.
;; rms  15 Oct 98
(provide 'archive-mode)

(provide 'arc-mode)

;;; arc-mode.el ends here
