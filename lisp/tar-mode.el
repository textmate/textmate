;;; tar-mode.el --- simple editing of tar files from GNU Emacs

;; Copyright (C) 1990-1991, 1993-2012 Free Software Foundation, Inc.

;; Author: Jamie Zawinski <jwz@lucid.com>
;; Maintainer: FSF
;; Created: 04 Apr 1990
;; Keywords: unix

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

;; This package attempts to make dealing with Unix 'tar' archives easier.
;; When this code is loaded, visiting a file whose name ends in '.tar' will
;; cause the contents of that archive file to be displayed in a Dired-like
;; listing.  It is then possible to use the customary Dired keybindings to
;; extract sub-files from that archive, either by reading them into their own
;; editor buffers, or by copying them directly to arbitrary files on disk.
;; It is also possible to delete sub-files from within the tar file and write
;; the modified archive back to disk, or to edit sub-files within the archive
;; and re-insert the modified files into the archive.  See the documentation
;; string of tar-mode for more info.

;; This code now understands the extra fields that GNU tar adds to tar files.

;; This interacts correctly with "uncompress.el" in the Emacs library,
;; which you get with
;;
;;  (autoload 'uncompress-while-visiting "uncompress")
;;  (setq auto-mode-alist (cons '("\\.Z$" . uncompress-while-visiting)
;;			   auto-mode-alist))
;;
;; Do not attempt to use tar-mode.el with crypt.el, you will lose.

;;    ***************   TO DO   ***************
;;
;; o  chmod should understand "a+x,og-w".
;;
;; o  It's not possible to add a NEW file to a tar archive; not that
;;    important, but still...
;;
;; o  The code is less efficient that it could be - in a lot of places, I
;;    pull a 512-character string out of the buffer and parse it, when I could
;;    be parsing it in place, not garbaging a string.  Should redo that.
;;
;; o  I'd like a command that searches for a string/regexp in every subfile
;;    of an archive, where <esc> would leave you in a subfile-edit buffer.
;;    (Like the Meta-R command of the Zmacs mail reader.)
;;
;; o  Sometimes (but not always) reverting the tar-file buffer does not
;;    re-grind the listing, and you are staring at the binary tar data.
;;    Typing 'g' again immediately after that will always revert and re-grind
;;    it, though.  I have no idea why this happens.
;;
;; o  Tar-mode interacts poorly with crypt.el and zcat.el because the tar
;;    write-file-hook actually writes the file.  Instead it should remove the
;;    header (and conspire to put it back afterwards) so that other write-file
;;    hooks which frob the buffer have a chance to do their dirty work.  There
;;    might be a problem if the tar write-file-hook does not come *first* on
;;    the list.
;;
;; o  Block files, sparse files, continuation files, and the various header
;;    types aren't editable.  Actually I don't know that they work at all.

;; Rationale:

;; Why does tar-mode edit the file itself instead of using tar?

;; That means that you can edit tar files which you don't have room for
;; on your local disk.

;; I don't know about recent features in gnu tar, but old versions of tar
;; can't replace a file in the middle of a tar file with a new version.
;; Tar-mode can.  I don't think tar can do things like chmod the subfiles.
;; An implementation which involved unpacking and repacking the file into
;; some scratch directory would be very wasteful, and wouldn't be able to
;; preserve the file owners.

;;; Bugs:

;; - Rename on ././@LongLink files
;; - Revert confirmation displays the raw data temporarily.

;;; Code:

(eval-when-compile (require 'cl))

(defgroup tar nil
  "Simple editing of tar files."
  :prefix "tar-"
  :group 'data)

(defcustom tar-anal-blocksize 20
  "The blocksize of tar files written by Emacs, or nil, meaning don't care.
The blocksize of a tar file is not really the size of the blocks; rather, it is
the number of blocks written with one system call.  When tarring to a tape,
this is the size of the *tape* blocks, but when writing to a file, it doesn't
matter much.  The only noticeable difference is that if a tar file does not
have a blocksize of 20, tar will tell you that; all this really controls is
how many null padding bytes go on the end of the tar file."
  :type '(choice integer (const nil))
  :group 'tar)

(defcustom tar-update-datestamp nil
  "Non-nil means Tar mode should play fast and loose with sub-file datestamps.
If this is true, then editing and saving a tar file entry back into its
tar file will update its datestamp.  If false, the datestamp is unchanged.
You may or may not want this - it is good in that you can tell when a file
in a tar archive has been changed, but it is bad for the same reason that
editing a file in the tar archive at all is bad - the changed version of
the file never exists on disk."
  :type 'boolean
  :group 'tar)

(defcustom tar-mode-show-date nil
  "Non-nil means Tar mode should show the date/time of each subfile.
This information is useful, but it takes screen space away from file names."
  :type 'boolean
  :group 'tar)

(defvar tar-parse-info nil)
(defvar tar-superior-buffer nil)
(defvar tar-superior-descriptor nil)
(defvar tar-file-name-coding-system nil)

(put 'tar-superior-buffer 'permanent-local t)
(put 'tar-superior-descriptor 'permanent-local t)

;; The Tar data is made up of bytes and better manipulated as bytes
;; and can be very large, so insert/delete can be costly.  The summary we
;; want to display may contain non-ascii chars, of course, so we'd like it
;; to be multibyte.  We used to keep both in the same buffer and switch
;; from/to uni/multibyte.  But this had several downsides:
;; - set-buffer-multibyte has an O(N^2) worst case that tends to be triggered
;;   here, so it gets atrociously slow on large Tar files.
;; - need to widen/narrow the buffer to show/hide the raw data, and need to
;;   maintain a tar-header-offset that keeps track of the boundary between
;;   the two.
;; - can't use markers because they're not preserved by set-buffer-multibyte.
;; So instead, we now keep the two pieces of data in separate buffers, and
;; use the new buffer-swap-text primitive when we need to change which data
;; is associated with "the" buffer.
(defvar tar-data-buffer nil "Buffer that holds the actual raw tar bytes.")
(make-variable-buffer-local 'tar-data-buffer)

(defvar tar-data-swapped nil
  "If non-nil, `tar-data-buffer' indeed holds raw tar bytes.")
(make-variable-buffer-local 'tar-data-swapped)

(defun tar-data-swapped-p ()
  "Return non-nil if the tar-data is in `tar-data-buffer'."
  (and (buffer-live-p tar-data-buffer)
       ;; Sanity check to try and make sure tar-data-swapped tracks the swap
       ;; state correctly: the raw data is expected to be always larger than
       ;; the summary.
       (progn
	 (assert (or (= (buffer-size tar-data-buffer) (buffer-size))
                     (eq tar-data-swapped
                         (> (buffer-size tar-data-buffer) (buffer-size)))))
	 tar-data-swapped)))

(defun tar-swap-data ()
  "Swap buffer contents between current buffer and `tar-data-buffer'.
Preserve the modified states of the buffers and set `buffer-swapped-with'."
  (let ((data-buffer-modified-p (buffer-modified-p tar-data-buffer))
	(current-buffer-modified-p (buffer-modified-p)))
    (buffer-swap-text tar-data-buffer)
    (setq tar-data-swapped (not tar-data-swapped))
    (restore-buffer-modified-p data-buffer-modified-p)
    (with-current-buffer tar-data-buffer
      (restore-buffer-modified-p current-buffer-modified-p))))

;;; down to business.

(defstruct (tar-header
            (:constructor nil)
            (:type vector)
            :named
            (:constructor
             make-tar-header (data-start name mode uid gid size date checksum
                              link-type link-name magic uname gname dmaj dmin)))
  data-start name mode uid gid size date checksum link-type link-name
  magic uname gname dmaj dmin
  ;; Start of the header can be nil (meaning it's 512 bytes before data-start)
  ;; or a marker (in case the header uses LongLink thingies).
  header-start)

(defconst tar-name-offset 0)
(defconst tar-mode-offset (+ tar-name-offset 100))
(defconst tar-uid-offset  (+ tar-mode-offset 8))
(defconst tar-gid-offset  (+ tar-uid-offset 8))
(defconst tar-size-offset (+ tar-gid-offset 8))
(defconst tar-time-offset (+ tar-size-offset 12))
(defconst tar-chk-offset  (+ tar-time-offset 12))
(defconst tar-linkp-offset (+ tar-chk-offset 8))
(defconst tar-link-offset (+ tar-linkp-offset 1))
;;; GNU-tar specific slots.
(defconst tar-magic-offset (+ tar-link-offset 100))
(defconst tar-uname-offset (+ tar-magic-offset 8))
(defconst tar-gname-offset (+ tar-uname-offset 32))
(defconst tar-dmaj-offset (+ tar-gname-offset 32))
(defconst tar-dmin-offset (+ tar-dmaj-offset 8))
(defconst tar-prefix-offset (+ tar-dmin-offset 8))
(defconst tar-end-offset (+ tar-prefix-offset 155))

(defun tar-roundup-512 (s)
  "Round S up to the next multiple of 512."
  (ash (ash (+ s 511) -9) 9))

(defun tar-header-block-tokenize (pos coding)
  "Return a `tar-header' structure.
This is a list of name, mode, uid, gid, size,
write-date, checksum, link-type, and link-name."
  (if (> (+ pos 512) (point-max)) (error "Malformed Tar header"))
  (assert (zerop (mod (- pos (point-min)) 512)))
  (assert (not enable-multibyte-characters))
  (let ((string (buffer-substring pos (setq pos (+ pos 512)))))
    (when      ;(some 'plusp string)		 ; <-- oops, massive cycle hog!
        (or (not (= 0 (aref string 0))) ; This will do.
            (not (= 0 (aref string 101))))
      (let* ((name-end tar-mode-offset)
             (link-end (1- tar-magic-offset))
             (uname-end (1- tar-gname-offset))
             (gname-end (1- tar-dmaj-offset))
             (link-p (aref string tar-linkp-offset))
             (magic-str (substring string tar-magic-offset
				   ;; The magic string is actually 6bytes
				   ;; of magic string plus 2bytes of version
				   ;; which we here ignore.
                                   (- tar-uname-offset 2)))
	     ;; The magic string is "ustar\0" for POSIX format, and
	     ;; "ustar " for GNU Tar's format.
             (uname-valid-p (car (member magic-str '("ustar " "ustar\0"))))
             name linkname
             (nulsexp   "[^\000]*\000"))
        (when (string-match nulsexp string tar-name-offset)
          (setq name-end (min name-end (1- (match-end 0)))))
        (when (string-match nulsexp string tar-link-offset)
          (setq link-end (min link-end (1- (match-end 0)))))
        (when (string-match nulsexp string tar-uname-offset)
          (setq uname-end (min uname-end (1- (match-end 0)))))
        (when (string-match nulsexp string tar-gname-offset)
          (setq gname-end (min gname-end (1- (match-end 0)))))
        (setq name (substring string tar-name-offset name-end)
              link-p (if (or (= link-p 0) (= link-p ?0))
                         nil
                       (- link-p ?0)))
        (setq linkname (substring string tar-link-offset link-end))
        (when (and (equal uname-valid-p "ustar\0")
                   (string-match nulsexp string tar-prefix-offset)
                   (> (match-end 0) (1+ tar-prefix-offset)))
          (setq name (concat (substring string tar-prefix-offset
                                        (1- (match-end 0)))
                             "/" name)))
        (if (default-value 'enable-multibyte-characters)
            (setq name
                  (decode-coding-string name coding)
                  linkname
                  (decode-coding-string linkname coding)))
        (if (and (null link-p) (string-match "/\\'" name))
            (setq link-p 5))            ; directory

        (if (and (equal name "././@LongLink")
                 ;; Supposedly @LongLink is only used for GNUTAR
                 ;; format (i.e. "ustar ") but some POSIX Tar files
                 ;; (with "ustar\0") have been seen using it as well.
                 (member magic-str '("ustar " "ustar\0")))
            ;; This is a GNU Tar long-file-name header.
            (let* ((size (tar-parse-octal-integer
                          string tar-size-offset tar-time-offset))
                   ;; -1 so as to strip the terminating 0 byte.
		   (name (decode-coding-string
			  (buffer-substring pos (+ pos size -1)) coding))
                   (descriptor (tar-header-block-tokenize
                                (+ pos (tar-roundup-512 size))
				coding)))
              (cond
               ((eq link-p (- ?L ?0))      ;GNUTYPE_LONGNAME.
                (setf (tar-header-name descriptor) name))
               ((eq link-p (- ?K ?0))      ;GNUTYPE_LONGLINK.
                (setf (tar-header-link-name descriptor) name))
               (t
                (message "Unrecognized GNU Tar @LongLink format")))
              (setf (tar-header-header-start descriptor)
                    (copy-marker (- pos 512) t))
              descriptor)

          (make-tar-header
           (copy-marker pos nil)
           name
           (tar-parse-octal-integer string tar-mode-offset tar-uid-offset)
           (tar-parse-octal-integer string tar-uid-offset tar-gid-offset)
           (tar-parse-octal-integer string tar-gid-offset tar-size-offset)
           (tar-parse-octal-integer string tar-size-offset tar-time-offset)
           (tar-parse-octal-long-integer string tar-time-offset tar-chk-offset)
           (tar-parse-octal-integer string tar-chk-offset tar-linkp-offset)
           link-p
           linkname
           uname-valid-p
           (when uname-valid-p
             (decode-coding-string
              (substring string tar-uname-offset uname-end) coding))
           (when uname-valid-p
             (decode-coding-string
              (substring string tar-gname-offset gname-end) coding))
           (tar-parse-octal-integer string tar-dmaj-offset tar-dmin-offset)
           (tar-parse-octal-integer string tar-dmin-offset tar-prefix-offset)
           ))))))

;; Pseudo-field.
(defun tar-header-data-end (descriptor)
  (let* ((data-start (tar-header-data-start descriptor))
         (link-type (tar-header-link-type descriptor))
         (size (tar-header-size descriptor))
         (fudge (cond
                 ;; Foo.  There's an extra empty block after these.
                 ((memq link-type '(20 55)) 512)
                 (t 0))))
    (+ data-start fudge
       (if (and (null link-type) (> size 0))
           (tar-roundup-512 size)
         0))))

(defun tar-parse-octal-integer (string &optional start end)
  (if (null start) (setq start 0))
  (if (null end) (setq end (length string)))
  (if (= (aref string start) 0)
      0
    (let ((n 0))
      (while (< start end)
	(setq n (if (< (aref string start) ?0) n
		  (+ (* n 8) (- (aref string start) ?0)))
	      start (1+ start)))
      n)))

(defun tar-parse-octal-long-integer (string &optional start end)
  (if (null start) (setq start 0))
  (if (null end) (setq end (length string)))
  (if (= (aref string start) 0)
      (list 0 0)
    (let ((lo 0)
	  (hi 0))
      (while (< start end)
	(if (>= (aref string start) ?0)
	    (setq lo (+ (* lo 8) (- (aref string start) ?0))
		  hi (+ (* hi 8) (ash lo -16))
		  lo (logand lo 65535)))
	(setq start (1+ start)))
      (list hi lo))))

(defun tar-parse-octal-integer-safe (string)
  (if (zerop (length string)) (error "empty string"))
  (mapc (lambda (c)
	  (if (or (< c ?0) (> c ?7))
	      (error "`%c' is not an octal digit" c)))
	string)
  (tar-parse-octal-integer string))


(defun tar-header-block-checksum (string)
  "Compute and return a tar-acceptable checksum for this block."
  (assert (not (multibyte-string-p string)))
  (let* ((chk-field-start tar-chk-offset)
	 (chk-field-end (+ chk-field-start 8))
	 (sum 0)
	 (i 0))
    ;; Add up all of the characters except the ones in the checksum field.
    ;; Add that field as if it were filled with spaces.
    (while (< i chk-field-start)
      (setq sum (+ sum (aref string i))
	    i (1+ i)))
    (setq i chk-field-end)
    (while (< i 512)
      (setq sum (+ sum (aref string i))
	    i (1+ i)))
    (+ sum (* 32 8))))

(defun tar-header-block-check-checksum (hblock desired-checksum file-name)
  "Beep and print a warning if the checksum doesn't match."
  (if (not (= desired-checksum (tar-header-block-checksum hblock)))
      (progn (beep) (message "Invalid checksum for file %s!" file-name))))

(defun tar-clip-time-string (time)
  (let ((str (current-time-string time)))
    (concat " " (substring str 4 16) (substring str 19 24))))

(defun tar-grind-file-mode (mode)
  "Construct a `-rw--r--r--' string indicating MODE.
MODE should be an integer which is a file mode value."
  (string
   (if (zerop (logand 256 mode)) ?- ?r)
   (if (zerop (logand 128 mode)) ?- ?w)
   (if (zerop (logand 2048 mode))
       (if (zerop (logand  64 mode)) ?- ?x)
     (if (zerop (logand  64 mode)) ?S ?s))
   (if (zerop (logand  32 mode)) ?- ?r)
   (if (zerop (logand  16 mode)) ?- ?w)
   (if (zerop (logand 1024 mode))
       (if (zerop (logand   8 mode)) ?- ?x)
     (if (zerop (logand   8 mode)) ?S ?s))
   (if (zerop (logand   4 mode)) ?- ?r)
   (if (zerop (logand   2 mode)) ?- ?w)
   (if (zerop (logand 512 mode))
       (if (zerop (logand   1 mode)) ?- ?x)
     (if (zerop (logand   1 mode)) ?T ?t))))

(defun tar-header-block-summarize (tar-hblock &optional mod-p)
  "Return a line similar to the output of `tar -vtf'."
  (let ((name (tar-header-name tar-hblock))
	(mode (tar-header-mode tar-hblock))
	(uid (tar-header-uid tar-hblock))
	(gid (tar-header-gid tar-hblock))
	(uname (tar-header-uname tar-hblock))
	(gname (tar-header-gname tar-hblock))
	(size (tar-header-size tar-hblock))
	(time (tar-header-date tar-hblock))
	;; (ck (tar-header-checksum tar-hblock))
	(type (tar-header-link-type tar-hblock))
	(link-name (tar-header-link-name tar-hblock)))
    (format "%c%c%s %7s/%-7s %7s%s %s%s"
	    (if mod-p ?* ? )
	    (cond ((or (eq type nil) (eq type 0)) ?-)
		  ((eq type 1) ?h)	; link
		  ((eq type 2) ?l)	; symlink
		  ((eq type 3) ?c)	; char special
		  ((eq type 4) ?b)	; block special
		  ((eq type 5) ?d)	; directory
		  ((eq type 6) ?p)	; FIFO/pipe
		  ((eq type 20) ?*)	; directory listing
		  ((eq type 28) ?L)	; next has longname
		  ((eq type 29) ?M)	; multivolume continuation
		  ((eq type 35) ?S)	; sparse
		  ((eq type 38) ?V)	; volume header
		  ((eq type 55) ?H)	; extended pax header
		  (t ?\s)
		  )
	    (tar-grind-file-mode mode)
	    (if (= 0 (length uname)) uid uname)
	    (if (= 0 (length gname)) gid gname)
	    size
	    (if tar-mode-show-date (tar-clip-time-string time) "")
	    (propertize name
			'mouse-face 'highlight
			'help-echo "mouse-2: extract this file into a buffer")
	    (if (or (eq type 1) (eq type 2))
		(concat (if (= type 1) " ==> " " --> ") link-name)
	      ""))))

(defun tar-untar-buffer ()
  "Extract all archive members in the tar-file into the current directory."
  (interactive)
  ;; FIXME: make it work even if we're not in tar-mode.
  (let ((descriptors tar-parse-info))   ;Read the var in its buffer.
    (with-current-buffer
        (if (tar-data-swapped-p) tar-data-buffer (current-buffer))
      (set-buffer-multibyte nil)          ;Hopefully, a no-op.
      (dolist (descriptor descriptors)
        (let* ((name (tar-header-name descriptor))
               (dir (if (eq (tar-header-link-type descriptor) 5)
                        name
                      (file-name-directory name)))
               (start (tar-header-data-start descriptor))
               (end (+ start (tar-header-size descriptor))))
          (unless (file-directory-p name)
            (message "Extracting %s" name)
            (if (and dir (not (file-exists-p dir)))
                (make-directory dir t))
            (unless (file-directory-p name)
	      (let ((coding-system-for-write 'no-conversion))
		(write-region start end name)))
            (set-file-modes name (tar-header-mode descriptor))))))))

(defun tar-summarize-buffer ()
  "Parse the contents of the tar file in the current buffer."
  (assert (tar-data-swapped-p))
  (let* ((modified (buffer-modified-p))
         (result '())
         (pos (point-min))
	 (coding tar-file-name-coding-system)
         (progress-reporter
          (with-current-buffer tar-data-buffer
            (make-progress-reporter "Parsing tar file..."
                                    (point-min) (point-max))))
         descriptor)
    (with-current-buffer tar-data-buffer
      (while (and (< pos (point-max))
                  (setq descriptor (tar-header-block-tokenize pos coding)))
        (let ((size (tar-header-size descriptor)))
          (if (< size 0)
              (error "%s has size %s - corrupted"
                     (tar-header-name descriptor) size)))
        ;;
        ;; This is just too slow.  Don't really need it anyway....
        ;;(tar-header-block-check-checksum
        ;;  hblock (tar-header-block-checksum hblock)
        ;;  (tar-header-name descriptor))

        (push descriptor result)
        (setq pos (tar-header-data-end descriptor))
        (progress-reporter-update progress-reporter pos)))

    (set (make-local-variable 'tar-parse-info) (nreverse result))
    ;; A tar file should end with a block or two of nulls,
    ;; but let's not get a fatal error if it doesn't.
    (if (null descriptor)
        (progress-reporter-done progress-reporter)
      (message "Warning: premature EOF parsing tar file"))
    (goto-char (point-min))
    (let ((inhibit-read-only t)
          (total-summaries
           (mapconcat 'tar-header-block-summarize tar-parse-info "\n")))
      (insert total-summaries "\n"))
    (goto-char (point-min))
    (restore-buffer-modified-p modified)))

(defvar tar-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map " " 'tar-next-line)
    (define-key map "C" 'tar-copy)
    (define-key map "d" 'tar-flag-deleted)
    (define-key map "\^D" 'tar-flag-deleted)
    (define-key map "e" 'tar-extract)
    (define-key map "f" 'tar-extract)
    (define-key map "\C-m" 'tar-extract)
    (define-key map [mouse-2] 'tar-mouse-extract)
    (define-key map "g" 'revert-buffer)
    (define-key map "n" 'tar-next-line)
    (define-key map "\^N" 'tar-next-line)
    (define-key map [down] 'tar-next-line)
    (define-key map "o" 'tar-extract-other-window)
    (define-key map "p" 'tar-previous-line)
    (define-key map "\^P" 'tar-previous-line)
    (define-key map [up] 'tar-previous-line)
    (define-key map "R" 'tar-rename-entry)
    (define-key map "u" 'tar-unflag)
    (define-key map "v" 'tar-view)
    (define-key map "x" 'tar-expunge)
    (define-key map "\177" 'tar-unflag-backwards)
    (define-key map "E" 'tar-extract-other-window)
    (define-key map "M" 'tar-chmod-entry)
    (define-key map "G" 'tar-chgrp-entry)
    (define-key map "O" 'tar-chown-entry)
    ;; Let mouse-1 follow the link.
    (define-key map [follow-link] 'mouse-face)

    ;; Make menu bar items.

    ;; Get rid of the Edit menu bar item to save space.
    (define-key map [menu-bar edit] 'undefined)

    (define-key map [menu-bar immediate]
      (cons "Immediate" (make-sparse-keymap "Immediate")))

    (define-key map [menu-bar immediate view]
      '("View This File" . tar-view))
    (define-key map [menu-bar immediate display]
      '("Display in Other Window" . tar-display-other-window))
    (define-key map [menu-bar immediate find-file-other-window]
      '("Find in Other Window" . tar-extract-other-window))
    (define-key map [menu-bar immediate find-file]
      '("Find This File" . tar-extract))

    (define-key map [menu-bar mark]
      (cons "Mark" (make-sparse-keymap "Mark")))

    (define-key map [menu-bar mark unmark-all]
      '("Unmark All" . tar-clear-modification-flags))
    (define-key map [menu-bar mark deletion]
      '("Flag" . tar-flag-deleted))
    (define-key map [menu-bar mark unmark]
      '("Unflag" . tar-unflag))

    (define-key map [menu-bar operate]
      (cons "Operate" (make-sparse-keymap "Operate")))

    (define-key map [menu-bar operate chown]
      '("Change Owner..." . tar-chown-entry))
    (define-key map [menu-bar operate chgrp]
      '("Change Group..." . tar-chgrp-entry))
    (define-key map [menu-bar operate chmod]
      '("Change Mode..." . tar-chmod-entry))
    (define-key map [menu-bar operate rename]
      '("Rename to..." . tar-rename-entry))
    (define-key map [menu-bar operate copy]
      '("Copy to..." . tar-copy))
    (define-key map [menu-bar operate expunge]
      '("Expunge Marked Files" . tar-expunge))
    
    map)
  "Local keymap for Tar mode listings.")


;; tar mode is suitable only for specially formatted data.
(put 'tar-mode 'mode-class 'special)
(put 'tar-subfile-mode 'mode-class 'special)

(defun tar-change-major-mode-hook ()
  ;; Bring the actual Tar data back into the main buffer.
  (when (tar-data-swapped-p) (tar-swap-data))
  ;; Throw away the summary.
  (when (buffer-live-p tar-data-buffer) (kill-buffer tar-data-buffer)))

(defun tar-mode-kill-buffer-hook ()
  (if (buffer-live-p tar-data-buffer) (kill-buffer tar-data-buffer)))

;;;###autoload
(define-derived-mode tar-mode special-mode "Tar"
  "Major mode for viewing a tar file as a dired-like listing of its contents.
You can move around using the usual cursor motion commands.
Letters no longer insert themselves.
Type `e' to pull a file out of the tar file and into its own buffer;
or click mouse-2 on the file's line in the Tar mode buffer.
Type `c' to copy an entry from the tar file into another file on disk.

If you edit a sub-file of this archive (as with the `e' command) and
save it with \\[save-buffer], the contents of that buffer will be
saved back into the tar-file buffer; in this way you can edit a file
inside of a tar archive without extracting it and re-archiving it.

See also: variables `tar-update-datestamp' and `tar-anal-blocksize'.
\\{tar-mode-map}"
  (and buffer-file-name
       (file-writable-p buffer-file-name)
       (setq buffer-read-only nil))    ; undo what `special-mode' did
  (make-local-variable 'tar-parse-info)
  (set (make-local-variable 'require-final-newline) nil) ; binary data, dude...
  (set (make-local-variable 'local-enable-local-variables) nil)
  (set (make-local-variable 'next-line-add-newlines) nil)
  (set (make-local-variable 'tar-file-name-coding-system)
       (or file-name-coding-system
	   default-file-name-coding-system
	   locale-coding-system))
  ;; Prevent loss of data when saving the file.
  (set (make-local-variable 'file-precious-flag) t)
  (buffer-disable-undo)
  (widen)
  ;; Now move the Tar data into an auxiliary buffer, so we can use the main
  ;; buffer for the summary.
  (assert (not (tar-data-swapped-p)))
  (set (make-local-variable 'revert-buffer-function) 'tar-mode-revert)
  ;; We started using write-contents-functions, but this hook is not
  ;; used during auto-save, so we now use
  ;; write-region-annotate-functions which hooks at a lower-level.
  (add-hook 'write-region-annotate-functions 'tar-write-region-annotate nil t)
  (add-hook 'kill-buffer-hook 'tar-mode-kill-buffer-hook nil t)
  (add-hook 'change-major-mode-hook 'tar-change-major-mode-hook nil t)
  ;; Tar data is made of bytes, not chars.
  (set-buffer-multibyte nil)            ;Hopefully a no-op.
  (set (make-local-variable 'tar-data-buffer)
       (generate-new-buffer (format " *tar-data %s*"
                                    (file-name-nondirectory
                                     (or buffer-file-name (buffer-name))))))
  (condition-case err
      (progn
        (tar-swap-data)
        (tar-summarize-buffer)
        (tar-next-line 0))
    (error
     ;; If summarizing caused an error, then maybe the buffer doesn't contain
     ;; tar data.  Rather than show a mysterious empty buffer, let's
     ;; revert to fundamental-mode.
     (fundamental-mode)
     (signal (car err) (cdr err)))))


(define-minor-mode tar-subfile-mode
  "Minor mode for editing an element of a tar-file.
With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil.  This mode arranges for \"saving\" this
buffer to write the data into the tar-file buffer that it came
from.  The changes will actually appear on disk when you save the
tar-file's buffer."
  ;; Don't do this, because it is redundant and wastes mode line space.
  ;; :lighter " TarFile"
  nil nil nil
  (or (and (boundp 'tar-superior-buffer) tar-superior-buffer)
      (error "This buffer is not an element of a tar file"))
  (cond (tar-subfile-mode
	 (add-hook 'write-file-functions 'tar-subfile-save-buffer nil t)
	 ;; turn off auto-save.
	 (auto-save-mode -1)
	 (setq buffer-auto-save-file-name nil))
	(t
	 (remove-hook 'write-file-functions 'tar-subfile-save-buffer t))))


;; Revert the buffer and recompute the dired-like listing.
(defun tar-mode-revert (&optional no-auto-save no-confirm)
  (unwind-protect
      (let ((revert-buffer-function nil))
        (if (tar-data-swapped-p) (tar-swap-data))
        ;; FIXME: If we ask for confirmation, the user will be temporarily
        ;; looking at the raw data.
        (revert-buffer no-auto-save no-confirm 'preserve-modes)
        ;; Recompute the summary.
        (if (buffer-live-p tar-data-buffer) (kill-buffer tar-data-buffer))
        (tar-mode))
    (unless (tar-data-swapped-p) (tar-swap-data))))


(defun tar-next-line (arg)
  "Move cursor vertically down ARG lines and to the start of the filename."
  (interactive "p")
  (forward-line arg)
  (goto-char (or (next-single-property-change (point) 'mouse-face) (point))))

(defun tar-previous-line (arg)
  "Move cursor vertically up ARG lines and to the start of the filename."
  (interactive "p")
  (tar-next-line (- arg)))

(defun tar-current-descriptor (&optional noerror)
  "Return the tar-descriptor of the current line, or signals an error."
  ;; I wish lines had plists, like in ZMACS...
  (or (nth (count-lines (point-min) (line-beginning-position))
	   tar-parse-info)
      (if noerror
	  nil
	  (error "This line does not describe a tar-file entry"))))

(defun tar-get-descriptor ()
  (let* ((descriptor (tar-current-descriptor))
	 (size (tar-header-size descriptor))
	 (link-p (tar-header-link-type descriptor)))
    (if link-p
	(error "This is %s, not a real file"
	       (cond ((eq link-p 5) "a directory")
		     ((eq link-p 20) "a tar directory header")
		     ((eq link-p 28) "a next has longname")
		     ((eq link-p 29) "a multivolume-continuation")
		     ((eq link-p 35) "a sparse entry")
		     ((eq link-p 38) "a volume header")
		     ((eq link-p 55) "an extended pax header")
		     (t "a link"))))
    (if (zerop size) (message "This is a zero-length file"))
    descriptor))

(defun tar-mouse-extract (event)
  "Extract a file whose tar directory line you click on."
  (interactive "e")
  (with-current-buffer (window-buffer (posn-window (event-end event)))
    (save-excursion
      (goto-char (posn-point (event-end event)))
      ;; Just make sure this doesn't get an error.
      (tar-get-descriptor)))
  (select-window (posn-window (event-end event)))
  (goto-char (posn-point (event-end event)))
  (tar-extract))

(defun tar-file-name-handler (op &rest args)
  "Helper function for `tar-extract'."
  (or (eq op 'file-exists-p)
      (let ((file-name-handler-alist nil))
	(apply op args))))

(defun tar-extract (&optional other-window-p)
  "In Tar mode, extract this entry of the tar file into its own buffer."
  (interactive)
  (let* ((view-p (eq other-window-p 'view))
	 (descriptor (tar-get-descriptor))
	 (name (tar-header-name descriptor))
	 (size (tar-header-size descriptor))
	 (start (tar-header-data-start descriptor))
	 (end (+ start size)))
    (let* ((tar-buffer (current-buffer))
	   (tarname (buffer-name))
	   (bufname (concat (file-name-nondirectory name)
			    " ("
			     tarname
			     ")"))
	   (read-only-p (or buffer-read-only view-p))
	   (new-buffer-file-name (expand-file-name
				  ;; `:' is not allowed on Windows
                                  (concat tarname "!"
                                          (if (string-match "/" name)
                                              name
                                            ;; Make sure `name' contains a /
                                            ;; so set-auto-mode doesn't try
                                            ;; to look at `tarname' for hints.
                                            (concat "./" name)))))
	   (buffer (get-file-buffer new-buffer-file-name))
	   (just-created nil)
	   undo-list)
      (unless buffer
	(setq buffer (generate-new-buffer bufname))
	(with-current-buffer buffer
	  (setq undo-list buffer-undo-list
		buffer-undo-list t))
	(setq bufname (buffer-name buffer))
	(setq just-created t)
	(with-current-buffer tar-data-buffer
          (let (coding)
            (narrow-to-region start end)
            (goto-char start)
            (setq coding (or coding-system-for-read
                             (and set-auto-coding-function
                                  (funcall set-auto-coding-function
                                           name (- end start)))
                             ;; The following binding causes
                             ;; find-buffer-file-type-coding-system
                             ;; (defined on dos-w32.el) to act as if
                             ;; the file being extracted existed, so
                             ;; that the file's contents' encoding and
                             ;; EOL format are auto-detected.
                             (let ((file-name-handler-alist
                                    '(("" . tar-file-name-handler))))
                               (car (find-operation-coding-system
                                     'insert-file-contents
                                     (cons name (current-buffer)) t)))))
            (if (or (not coding)
                    (eq (coding-system-type coding) 'undecided))
                (setq coding (detect-coding-region start end t)))
            (if (and (default-value 'enable-multibyte-characters)
                     (coding-system-get coding :for-unibyte))
                (with-current-buffer buffer
                  (set-buffer-multibyte nil)))
            (widen)
            (decode-coding-region start end coding buffer)))
        (with-current-buffer buffer
          (goto-char (point-min))
          (setq buffer-file-name new-buffer-file-name)
          (setq buffer-file-truename
                (abbreviate-file-name buffer-file-name))
          ;; Force buffer-file-coding-system to what
          ;; decode-coding-region actually used.
          (set-buffer-file-coding-system last-coding-system-used t)
          ;; Set the default-directory to the dir of the
          ;; superior buffer.
          (setq default-directory
                (with-current-buffer tar-buffer
                  default-directory))
          (rename-buffer bufname)
          (set-buffer-modified-p nil)
          (setq buffer-undo-list undo-list)
          (normal-mode)  ; pick a mode.
          (set (make-local-variable 'tar-superior-buffer) tar-buffer)
          (set (make-local-variable 'tar-superior-descriptor) descriptor)
          (setq buffer-read-only read-only-p)
          (tar-subfile-mode 1)))
      (cond
       (view-p
	(view-buffer buffer (and just-created 'kill-buffer-if-not-modified)))
       ((eq other-window-p 'display) (display-buffer buffer))
       (other-window-p (switch-to-buffer-other-window buffer))
       (t (switch-to-buffer buffer))))))


(defun tar-extract-other-window ()
  "In Tar mode, find this entry of the tar file in another window."
  (interactive)
  (tar-extract t))

(defun tar-display-other-window ()
  "In Tar mode, display this entry of the tar file in another window."
  (interactive)
  (tar-extract 'display))

(defun tar-view ()
  "In Tar mode, view the tar file entry on this line."
  (interactive)
  (tar-extract 'view))


(defun tar-read-file-name (&optional prompt)
  "Read a file name with this line's entry as the default."
  (or prompt (setq prompt "Copy to: "))
  (let* ((default-file (expand-file-name
			(tar-header-name (tar-current-descriptor))))
	 (target (expand-file-name
		  (read-file-name prompt
				  (file-name-directory default-file)
				  default-file nil))))
    (if (or (string= "" (file-name-nondirectory target))
	    (file-directory-p target))
	(setq target (concat (if (string-match "/$" target)
				 (substring target 0 (1- (match-end 0)))
				 target)
			     "/"
			     (file-name-nondirectory default-file))))
    target))


(defun tar-copy (&optional to-file)
  "In Tar mode, extract this entry of the tar file into a file on disk.
If TO-FILE is not supplied, it is prompted for, defaulting to the name of
the current tar-entry."
  (interactive (list (tar-read-file-name)))
  (let* ((descriptor (tar-get-descriptor))
	 (name (tar-header-name descriptor))
	 (size (tar-header-size descriptor))
	 (start (tar-header-data-start descriptor))
	 (end (+ start size))
	 (inhibit-file-name-handlers inhibit-file-name-handlers)
	 (inhibit-file-name-operation inhibit-file-name-operation))
    (with-current-buffer
	(if (tar-data-swapped-p) tar-data-buffer (current-buffer))
      ;; Inhibit compressing a subfile again if *both* name and
      ;; to-file are handled by jka-compr
      (if (and (eq (find-file-name-handler name 'write-region)
		   'jka-compr-handler)
	       (eq (find-file-name-handler to-file 'write-region)
		   'jka-compr-handler))
	  (setq inhibit-file-name-handlers
		(cons 'jka-compr-handler
		      (and (eq inhibit-file-name-operation 'write-region)
			   inhibit-file-name-handlers))
		inhibit-file-name-operation 'write-region))
      (let ((coding-system-for-write 'no-conversion))
	(write-region start end to-file nil nil nil t)))
    (message "Copied tar entry %s to %s" name to-file)))

(defun tar-flag-deleted (p &optional unflag)
  "In Tar mode, mark this sub-file to be deleted from the tar file.
With a prefix argument, mark that many files."
  (interactive "p")
  (beginning-of-line)
  (dotimes (i (abs p))
    (if (tar-current-descriptor unflag) ; barf if we're not on an entry-line.
	(progn
	  (delete-char 1)
	  (insert (if unflag " " "D"))))
    (forward-line (if (< p 0) -1 1)))
  (if (eobp) nil (forward-char 36)))

(defun tar-unflag (p)
  "In Tar mode, un-mark this sub-file if it is marked to be deleted.
With a prefix argument, un-mark that many files forward."
  (interactive "p")
  (tar-flag-deleted p t))

(defun tar-unflag-backwards (p)
  "In Tar mode, un-mark this sub-file if it is marked to be deleted.
With a prefix argument, un-mark that many files backward."
  (interactive "p")
  (tar-flag-deleted (- p) t))


(defun tar-expunge-internal ()
  "Expunge the tar-entry specified by the current line."
  (let ((descriptor (tar-current-descriptor)))
    ;;
    ;; delete the current line...
    (delete-region (line-beginning-position) (line-beginning-position 2))
    ;;
    ;; delete the data pointer...
    (setq tar-parse-info (delq descriptor tar-parse-info))
    ;;
    ;; delete the data from inside the file...
    (with-current-buffer tar-data-buffer
      (delete-region (or (tar-header-header-start descriptor)
                         (- (tar-header-data-start descriptor) 512))
                     (tar-header-data-end descriptor)))))


(defun tar-expunge (&optional noconfirm)
  "In Tar mode, delete all the archived files flagged for deletion.
This does not modify the disk image; you must save the tar file itself
for this to be permanent."
  (interactive)
  (if (or noconfirm
	  (y-or-n-p "Expunge files marked for deletion? "))
      (let ((n 0))
	(save-excursion
	  (goto-char (point-min))
	  (while (not (eobp))
	    (if (looking-at "D")
		(progn (tar-expunge-internal)
		       (setq n (1+ n)))
		(forward-line 1)))
	  ;; after doing the deletions, add any padding that may be necessary.
	  (tar-pad-to-blocksize))
	(if (zerop n)
	    (message "Nothing to expunge.")
	    (message "%s files expunged.  Be sure to save this buffer." n)))))


(defun tar-clear-modification-flags ()
  "Remove the stars at the beginning of each line."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (if (not (eq (following-char) ?\s))
	  (progn (delete-char 1) (insert " ")))
      (forward-line 1))))


(defun tar-chown-entry (new-uid)
  "Change the user-id associated with this entry in the tar file.
If this tar file was written by GNU tar, then you will be able to edit
the user id as a string; otherwise, you must edit it as a number.
You can force editing as a number by calling this with a prefix arg.
This does not modify the disk image; you must save the tar file itself
for this to be permanent."
  (interactive
   (list
    (let ((descriptor (tar-current-descriptor)))
      (if (or current-prefix-arg
              (not (tar-header-magic descriptor)))
          (read-number
           "New UID number: "
           (format "%s" (tar-header-uid descriptor)))
        (read-string "New UID string: " (tar-header-uname descriptor))))))
  (cond ((stringp new-uid)
	 (setf (tar-header-uname (tar-current-descriptor)) new-uid)
	 (tar-alter-one-field tar-uname-offset
                              (concat (encode-coding-string
                                       new-uid tar-file-name-coding-system)
                                      "\000")))
	(t
	 (setf (tar-header-uid (tar-current-descriptor)) new-uid)
	 (tar-alter-one-field tar-uid-offset
	   (concat (substring (format "%6o" new-uid) 0 6) "\000 ")))))


(defun tar-chgrp-entry (new-gid)
  "Change the group-id associated with this entry in the tar file.
If this tar file was written by GNU tar, then you will be able to edit
the group id as a string; otherwise, you must edit it as a number.
You can force editing as a number by calling this with a prefix arg.
This does not modify the disk image; you must save the tar file itself
for this to be permanent."
  (interactive
   (list
    (let ((descriptor (tar-current-descriptor)))
      (if (or current-prefix-arg
              (not (tar-header-magic descriptor)))
          (read-number
           "New GID number: "
           (format "%s" (tar-header-gid descriptor)))
        (read-string "New GID string: " (tar-header-gname descriptor))))))
  (cond ((stringp new-gid)
	 (setf (tar-header-gname (tar-current-descriptor)) new-gid)
	 (tar-alter-one-field tar-gname-offset
                              (concat (encode-coding-string
                                       new-gid tar-file-name-coding-system)
                                      "\000")))
	(t
	 (setf (tar-header-gid (tar-current-descriptor)) new-gid)
	 (tar-alter-one-field tar-gid-offset
	   (concat (substring (format "%6o" new-gid) 0 6) "\000 ")))))

(defun tar-rename-entry (new-name)
  "Change the name associated with this entry in the tar file.
This does not modify the disk image; you must save the tar file itself
for this to be permanent."
  (interactive
    (list (read-string "New name: "
	    (tar-header-name (tar-current-descriptor)))))
  (if (string= "" new-name) (error "zero length name"))
  (let ((encoded-new-name (encode-coding-string new-name
						tar-file-name-coding-system))
        (descriptor (tar-current-descriptor))
        (prefix nil))
    (when (tar-header-header-start descriptor)
      ;; FIXME: Make it work for ././@LongLink.
      (error "Rename with @LongLink format is not implemented"))

    (when (and (> (length encoded-new-name) 98)
               (string-match "/" encoded-new-name
			     (- (length encoded-new-name) 99))
	       (< (match-beginning 0) 155))
      (unless (equal (tar-header-magic descriptor) "ustar\0")
        (tar-alter-one-field tar-magic-offset (concat "ustar\0" "00")))
      (setq prefix (substring encoded-new-name 0 (match-beginning 0)))
      (setq encoded-new-name (substring encoded-new-name (match-end 0))))

    (if (> (length encoded-new-name) 98) (error "name too long"))
    (setf (tar-header-name descriptor) new-name)
    (tar-alter-one-field 0
     (substring (concat encoded-new-name (make-string 99 0)) 0 99))
    (if prefix
        (tar-alter-one-field tar-prefix-offset
         (substring (concat prefix (make-string 155 0)) 0 155)))))


(defun tar-chmod-entry (new-mode)
  "Change the protection bits associated with this entry in the tar file.
This does not modify the disk image; you must save the tar file itself
for this to be permanent."
  (interactive (list (tar-parse-octal-integer-safe
		       (read-string "New protection (octal): "))))
  (setf (tar-header-mode (tar-current-descriptor)) new-mode)
  (tar-alter-one-field tar-mode-offset
    (concat (substring (format "%6o" new-mode) 0 6) "\000 ")))


(defun tar-alter-one-field (data-position new-data-string &optional descriptor)
  (unless descriptor (setq descriptor (tar-current-descriptor)))
  ;;
  ;; update the header-line.
  (let ((col (current-column)))
    (delete-region (line-beginning-position)
                   (prog2 (forward-line 1)
                       (point)
                     ;; Insert the new text after the old, before deleting,
                     ;; to preserve markers such as the window start.
                     (insert (tar-header-block-summarize descriptor) "\n")))
    (forward-line -1) (move-to-column col))

  (assert (tar-data-swapped-p))
  (with-current-buffer tar-data-buffer
    (let* ((start (- (tar-header-data-start descriptor) 512)))
        ;;
        ;; delete the old field and insert a new one.
        (goto-char (+ start data-position))
        (delete-region (point) (+ (point) (length new-data-string))) ; <--
        (assert (not (or enable-multibyte-characters
                         (multibyte-string-p new-data-string))))
        (insert new-data-string)
        ;;
        ;; compute a new checksum and insert it.
        (let ((chk (tar-header-block-checksum
		  (buffer-substring start (+ start 512)))))
	(goto-char (+ start tar-chk-offset))
	(delete-region (point) (+ (point) 8))
	(insert (format "%6o\0 " chk))
	(setf (tar-header-checksum descriptor) chk)
	;;
	;; ok, make sure we didn't botch it.
	(tar-header-block-check-checksum
	 (buffer-substring start (+ start 512))
	 chk (tar-header-name descriptor))
	))))


(defun tar-octal-time (timeval)
  ;; Format a timestamp as 11 octal digits.  Ghod, I hope this works...
  (let ((hibits (car timeval)) (lobits (car (cdr timeval))))
    (format "%05o%01o%05o"
	    (lsh hibits -2)
	    (logior (lsh (logand 3 hibits) 1)
		    (if (> (logand lobits 32768) 0) 1 0))
	    (logand 32767 lobits)
	    )))

(defun tar-subfile-save-buffer ()
  "In tar subfile mode, save this buffer into its parent tar-file buffer.
This doesn't write anything to disk; you must save the parent tar-file buffer
to make your changes permanent."
  (interactive)
  (if (not (and (boundp 'tar-superior-buffer) tar-superior-buffer))
      (error "This buffer has no superior tar file buffer"))
  (if (not (and (boundp 'tar-superior-descriptor) tar-superior-descriptor))
      (error "This buffer doesn't have an index into its superior tar file!"))
  (let ((subfile (current-buffer))
        (coding buffer-file-coding-system)
        (descriptor tar-superior-descriptor)
        subfile-size)
    (with-current-buffer tar-superior-buffer
      (let* ((start (tar-header-data-start descriptor))
             (size (tar-header-size descriptor))
             (head (memq descriptor tar-parse-info)))
        (if (not head)
            (error "Can't find this tar file entry in its parent tar file!"))
        (with-current-buffer tar-data-buffer
          ;; delete the old data...
          (let* ((data-start start)
                 (data-end (+ data-start (tar-roundup-512 size))))
            (narrow-to-region data-start data-end)
            (delete-region (point-min) (point-max))
            ;; insert the new data...
            (goto-char data-start)
            (let ((dest (current-buffer)))
              (with-current-buffer subfile
                (save-restriction
                  (widen)
                  (encode-coding-region (point-min) (point-max) coding dest))))
            (setq subfile-size (- (point-max) (point-min)))
            ;;
            ;; pad the new data out to a multiple of 512...
            (let ((subfile-size-pad (tar-roundup-512 subfile-size)))
              (goto-char (point-max))
              (insert (make-string (- subfile-size-pad subfile-size) 0))
              ;;
              ;; update the data of this files...
              (setf (tar-header-size descriptor) subfile-size)
              ;;
              ;; Update the size field in the header block.
              (widen))))
        ;;
        ;; alter the descriptor-line and header
        ;;
        (let ((position (- (length tar-parse-info) (length head))))
          (goto-char (point-min))
          (forward-line position)
	  (tar-alter-one-field tar-size-offset (format "%11o " subfile-size))
	  ;;
	  ;; Maybe update the datestamp.
	  (when tar-update-datestamp
	    (tar-alter-one-field tar-time-offset
				 (concat (tar-octal-time (current-time)) " "))))
        ;; After doing the insertion, add any necessary final padding.
        (tar-pad-to-blocksize))
      (set-buffer-modified-p t)         ; mark the tar file as modified
      (tar-next-line 0))
    (set-buffer-modified-p nil)       ; mark the tar subfile as unmodified
    (message "Saved into tar-buffer `%s'.  Be sure to save that buffer!"
             (buffer-name tar-superior-buffer))
    ;; Prevent basic-save-buffer from changing our coding-system.
    (setq last-coding-system-used buffer-file-coding-system)
    ;; Prevent ordinary saving from happening.
    t))


;; When this function is called, it is sure that the buffer is unibyte.
(defun tar-pad-to-blocksize ()
  "If we are being anal about tar file blocksizes, fix up the current buffer.
Leaves the region wide."
  (if (null tar-anal-blocksize)
      nil
    (let* ((last-desc (nth (1- (length tar-parse-info)) tar-parse-info))
	   (start (tar-header-data-start last-desc))
	   (link-p (tar-header-link-type last-desc))
	   (size (if link-p 0 (tar-header-size last-desc)))
	   (data-end (+ start size))
	   (bbytes (ash tar-anal-blocksize 9))
	   (pad-to (+ bbytes (* bbytes (/ (- data-end (point-min)) bbytes)))))
      ;; If the padding after the last data is too long, delete some;
      ;; else insert some until we are padded out to the right number of blocks.
      ;;
      (with-current-buffer tar-data-buffer
        (let ((goal-end (+ (point-min) pad-to)))
          (if (> (point-max) goal-end)
              (delete-region goal-end (point-max))
            (goto-char (point-max))
            (insert (make-string (- goal-end (point-max)) ?\0))))))))


;; Used in write-region-annotate-functions to write tar-files out correctly.
(defun tar-write-region-annotate (start _end)
  ;; When called from write-file (and auto-save), `start' is nil.
  ;; When called from M-x write-region, we assume the user wants to save
  ;; (part of) the summary, not the tar data.
  (unless (or start (not (tar-data-swapped-p)))
  (tar-clear-modification-flags)
    (set-buffer tar-data-buffer)
    nil))

(provide 'tar-mode)

;;; tar-mode.el ends here
