;;; jka-compr.el --- reading/writing/loading compressed files

;; Copyright (C) 1993-1995, 1997, 1999-2012 Free Software Foundation, Inc.

;; Author: jka@ece.cmu.edu (Jay K. Adams)
;; Maintainer: FSF
;; Keywords: data

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

;; This package implements low-level support for reading, writing,
;; and loading compressed files.  It hooks into the low-level file
;; I/O functions (including write-region and insert-file-contents) so
;; that they automatically compress or uncompress a file if the file
;; appears to need it (based on the extension of the file name).
;; Packages like Rmail, VM, GNUS, and Info should be able to work
;; with compressed files without modification.


;; INSTRUCTIONS:
;;
;; To use jka-compr, invoke the command `auto-compression-mode' (which
;; see), or customize the variable of the same name.  Its operation
;; should be transparent to the user (except for messages appearing when
;; a file is being compressed or uncompressed).
;;
;; The variable, jka-compr-compression-info-list can be used to
;; customize jka-compr to work with other compression programs.
;; The default value of this variable allows jka-compr to work with
;; Unix compress and gzip.
;;
;; If you don't want messages about compressing and decompressing
;; to show up in the echo area, you can set the compress-msg and
;; decompress-msg fields of the jka-compr-compression-info-list to
;; nil.


;; APPLICATION NOTES:
;;
;; crypt++
;;   jka-compr can coexist with crypt++ if you take all the decompression
;;   entries out of the crypt-encoding-list.  Clearly problems will arise if
;;   you have two programs trying to compress/decompress files.  jka-compr
;;   will not "work with" crypt++ in the following sense: you won't be able to
;;   decode encrypted compressed files--that is, files that have been
;;   compressed then encrypted (in that order).  Theoretically, crypt++ and
;;   jka-compr could properly handle a file that has been encrypted then
;;   compressed, but there is little point in trying to compress an encrypted
;;   file.
;;


;; ACKNOWLEDGMENTS
;;
;; jka-compr is a V19 adaptation of jka-compr for V18 of Emacs.  Many people
;; have made helpful suggestions, reported bugs, and even fixed bugs in
;; jka-compr.  I recall the following people as being particularly helpful.
;;
;;   Jean-loup Gailly
;;   David Hughes
;;   Richard Pieri
;;   Daniel Quinlan
;;   Chris P. Ross
;;   Rick Sladkey
;;
;; Andy Norman's ange-ftp was the inspiration for the original jka-compr for
;; Version 18 of Emacs.
;;
;; After I had made progress on the original jka-compr for V18, I learned of a
;; package written by Kazushi Jam Marukawa, called jam-zcat, that did exactly
;; what I was trying to do.  I looked over the jam-zcat source code and
;; probably got some ideas from it.
;;

;;; Code:

(require 'jka-cmpr-hook)

(defcustom jka-compr-shell "sh"
  "Shell to be used for calling compression programs.
NOTE: Not used in MS-DOS and Windows systems."
  :type 'string
  :group 'jka-compr)

(defvar jka-compr-use-shell
  (not (memq system-type '(ms-dos windows-nt))))

(defvar jka-compr-really-do-compress nil
  "Non-nil in a buffer whose visited file was uncompressed on visiting it.
This means compress the data on writing the file, even if the
data appears to be compressed already.")
(make-variable-buffer-local 'jka-compr-really-do-compress)
(put 'jka-compr-really-do-compress 'permanent-local t)


(put 'compression-error 'error-conditions '(compression-error file-error error))


(defvar jka-compr-acceptable-retval-list '(0 2 141))


(defun jka-compr-error (prog args infile message &optional errfile)

  (let ((errbuf (get-buffer-create " *jka-compr-error*")))
    (with-current-buffer errbuf
      (widen) (erase-buffer)
      (insert (format "Error while executing \"%s %s < %s\"\n\n"
		      prog
		      (mapconcat 'identity args " ")
		      infile))

      (and errfile
	   (insert-file-contents errfile)))
     (display-buffer errbuf))

  (signal 'compression-error
	  (list "Opening input file" (format "error %s" message) infile)))


(defcustom jka-compr-dd-program "/bin/dd"
  "How to invoke `dd'."
  :type 'string
  :group 'jka-compr)


(defvar jka-compr-dd-blocksize 256)


(defun jka-compr-partial-uncompress (prog message args infile beg len)
  "Call program PROG with ARGS args taking input from INFILE.
Fourth and fifth args, BEG and LEN, specify which part of the output
to keep: LEN chars starting BEG chars from the beginning."
  (let ((start (point))
	(prefix beg))
    (if (and jka-compr-use-shell jka-compr-dd-program)
	;; Put the uncompression output through dd
	;; to discard the part we don't want.
	(let ((skip (/ beg jka-compr-dd-blocksize))
	      (err-file (jka-compr-make-temp-name))
	      ;; call-process barfs if default-directory is inaccessible.
	      (default-directory
		(if (and default-directory
			 (file-accessible-directory-p default-directory))
		    default-directory
		  (file-name-directory infile)))
	      count)
	  ;; Update PREFIX based on the text that we won't read in.
	  (setq prefix (- beg (* skip jka-compr-dd-blocksize))
		count (and len (1+ (/ (+ len prefix) jka-compr-dd-blocksize))))
	  (unwind-protect
	      (or (memq (call-process
			 jka-compr-shell infile t nil "-c"
			 ;; Windows shells need the program file name
			 ;; after the pipe symbol be quoted if they use
			 ;; forward slashes as directory separators.
			 (format
			  "%s %s 2> %s | \"%s\" bs=%d skip=%d %s 2> %s"
			  prog
			  (mapconcat 'identity args " ")
			  err-file
			  jka-compr-dd-program
			  jka-compr-dd-blocksize
			  skip
			  ;; dd seems to be unreliable about
			  ;; providing the last block.  So, always
			  ;; read one more than you think you need.
			  (if count (format "count=%d" (1+ count)) "")
			  null-device))
			jka-compr-acceptable-retval-list)
		  (jka-compr-error prog args infile message err-file))
	    (delete-file err-file)))

      ;; Run the uncompression program directly.
      ;; We get the whole file and must delete what we don't want.
      (jka-compr-call-process prog message infile t nil args))

    ;; Delete the stuff after what we want, if there is any.
    (and
     len
     (< (+ start prefix len) (point))
     (delete-region (+ start prefix len) (point)))

    ;; Delete the stuff before what we want.
    (delete-region start (+ start prefix))))


(defun jka-compr-call-process (prog message infile output temp args)
  ;; call-process barfs if default-directory is inaccessible.
  (let ((default-directory
	  (if (and default-directory
		   (not (file-remote-p default-directory))
		   (file-accessible-directory-p default-directory))
	      default-directory
	    (file-name-directory infile))))
    (if jka-compr-use-shell
	(let ((err-file (jka-compr-make-temp-name))
	      (coding-system-for-read (or coding-system-for-read 'undecided))
	      (coding-system-for-write 'no-conversion))
	  (unwind-protect
	      (or (memq
		   (call-process jka-compr-shell infile
				 (if (stringp output) nil output)
				 nil
				 "-c"
				 (format "%s %s 2> %s %s"
					 prog
					 (mapconcat 'identity args " ")
					 err-file
					 (if (stringp output)
					     (concat "> " output)
					   "")))
		   jka-compr-acceptable-retval-list)
		  (jka-compr-error prog args infile message err-file))
	    (delete-file err-file)))
      (or (eq 0
	      (apply 'call-process
		     prog infile (if (stringp output) temp output)
		     nil args))
	  (jka-compr-error prog args infile message))
      (and (stringp output)
	   (with-current-buffer temp
	     (write-region (point-min) (point-max) output)
	     (erase-buffer))))))


;; Support for temp files.  Much of this was inspired if not lifted
;; from ange-ftp.

(defcustom jka-compr-temp-name-template
  (expand-file-name "jka-com" temporary-file-directory)
  "Prefix added to all temp files created by jka-compr.
There should be no more than seven characters after the final `/'."
  :type 'string
  :group 'jka-compr)

(defun jka-compr-make-temp-name (&optional _local-copy)
  "This routine will return the name of a new file."
  (make-temp-file jka-compr-temp-name-template))

(defun jka-compr-write-region (start end file &optional append visit)
  (let* ((filename (expand-file-name file))
	 (visit-file (if (stringp visit) (expand-file-name visit) filename))
	 (info (jka-compr-get-compression-info visit-file))
	 (magic (and info (jka-compr-info-file-magic-bytes info))))

    ;; If we uncompressed this file when visiting it,
    ;; then recompress it when writing it
    ;; even if the contents look compressed already.
    (if (and jka-compr-really-do-compress
             (or (null start)
                 (= (- end start) (buffer-size))))
	(setq magic nil))

    (if (and info
	     ;; If the contents to be written out
	     ;; are properly compressed already,
	     ;; don't try to compress them over again.
	     (not (and magic
		       (equal (if (stringp start)
				  (substring start 0 (min (length start)
							  (length magic)))
                                (let* ((from (or start (point-min)))
                                       (to (min (or end (point-max))
                                                (+ from (length magic)))))
                                  (buffer-substring from to)))
			      magic))))
	(let ((can-append (jka-compr-info-can-append info))
	      (compress-program (jka-compr-info-compress-program info))
	      (compress-message (jka-compr-info-compress-message info))
	      (compress-args (jka-compr-info-compress-args info))
	      (base-name (file-name-nondirectory visit-file))
	      temp-file temp-buffer
	      ;; we need to leave `last-coding-system-used' set to its
	      ;; value after calling write-region the first time, so
	      ;; that `basic-save-buffer' sees the right value.
	      (coding-system-used last-coding-system-used))

          (or compress-program
              (error "No compression program defined"))

	  (setq temp-buffer (get-buffer-create " *jka-compr-wr-temp*"))
	  (with-current-buffer temp-buffer
	    (widen) (erase-buffer))

	  (if (and append
		   (not can-append)
		   (file-exists-p filename))

	      (let* ((local-copy (file-local-copy filename))
		     (local-file (or local-copy filename)))

		(setq temp-file local-file))

	    (setq temp-file (jka-compr-make-temp-name)))

	  (and
	   compress-message
	   jka-compr-verbose
	   (message "%s %s..." compress-message base-name))

	  (jka-compr-run-real-handler 'write-region
				      (list start end temp-file t 'dont))
	  ;; save value used by the real write-region
	  (setq coding-system-used last-coding-system-used)

	  ;; Here we must read the output of compress program as is
	  ;; without any code conversion.
	  (let ((coding-system-for-read 'no-conversion))
	    (jka-compr-call-process compress-program
				    (concat compress-message
					    " " base-name)
				    temp-file
				    temp-buffer
				    nil
				    compress-args))

	  (with-current-buffer temp-buffer
	    (let ((coding-system-for-write 'no-conversion))
	      (if (memq system-type '(ms-dos windows-nt))
		  (setq buffer-file-type t) )
	      (jka-compr-run-real-handler 'write-region
					  (list (point-min) (point-max)
						filename
						(and append can-append) 'dont))
	      (erase-buffer)) )

	  (delete-file temp-file)

	  (and
	   compress-message
	   jka-compr-verbose
	   (message "%s %s...done" compress-message base-name))

	  (cond
	   ((eq visit t)
	    (setq buffer-file-name filename)
	    (setq jka-compr-really-do-compress t)
	    (set-visited-file-modtime))
	   ((stringp visit)
	    (setq buffer-file-name visit)
	    (let ((buffer-file-name filename))
	      (set-visited-file-modtime))))

	  (and (or (eq visit t)
		   (eq visit nil)
		   (stringp visit))
	       (message "Wrote %s" visit-file))

	  ;; ensure `last-coding-system-used' has an appropriate value
	  (setq last-coding-system-used coding-system-used)

	  nil)

      (jka-compr-run-real-handler 'write-region
				  (list start end filename append visit)))))


(defun jka-compr-insert-file-contents (file &optional visit beg end replace)
  (barf-if-buffer-read-only)

  (and (or beg end)
       visit
       (error "Attempt to visit less than an entire file"))

  (let* ((filename (expand-file-name file))
	 (info (jka-compr-get-compression-info filename)))

    (if (not info)

	(jka-compr-run-real-handler 'insert-file-contents
                                    (list file visit beg end replace))

      (let ((uncompress-message (jka-compr-info-uncompress-message info))
            (uncompress-program (jka-compr-info-uncompress-program info))
            (uncompress-args (jka-compr-info-uncompress-args info))
            (base-name (file-name-nondirectory filename))
            (notfound nil)
            (local-copy
             (jka-compr-run-real-handler 'file-local-copy (list filename)))
            local-file
            size start)

        (setq local-file (or local-copy filename))

        (and
         visit
         (setq buffer-file-name filename))

        (unwind-protect               ; to make sure local-copy gets deleted

            (progn

              (and
               uncompress-message
	       jka-compr-verbose
               (message "%s %s..." uncompress-message base-name))

              (condition-case error-code

                  (let ((coding-system-for-read 'no-conversion))
                    (if replace
                        (goto-char (point-min)))
                    (setq start (point))
                    (if (or beg end)
                        (jka-compr-partial-uncompress uncompress-program
                                                      (concat uncompress-message
                                                              " " base-name)
                                                      uncompress-args
                                                      local-file
                                                      (or beg 0)
                                                      (if (and beg end)
                                                          (- end beg)
                                                        end))
                      ;; If visiting, bind off buffer-file-name so that
                      ;; file-locking will not ask whether we should
                      ;; really edit the buffer.
                      (let ((buffer-file-name
                             (if visit nil buffer-file-name)))
                        (jka-compr-call-process uncompress-program
                                                (concat uncompress-message
                                                        " " base-name)
                                                local-file
                                                t
                                                nil
                                                uncompress-args)))
                    (setq size (- (point) start))
                    (if replace
                        (delete-region (point) (point-max)))
                    (goto-char start))
                (error
                 ;; If the file we wanted to uncompress does not exist,
                 ;; handle that according to VISIT as `insert-file-contents'
                 ;; would, maybe signaling the same error it normally would.
                 (if (and (eq (car error-code) 'file-error)
                          (eq (nth 3 error-code) local-file))
                     (if visit
                         (setq notfound error-code)
                       (signal 'file-error
                               (cons "Opening input file"
                                     (nthcdr 2 error-code))))
                   ;; If the uncompression program can't be found,
                   ;; signal that as a non-file error
                   ;; so that find-file-noselect-1 won't handle it.
                   (if (and (eq (car error-code) 'file-error)
                            (equal (cadr error-code) "Searching for program"))
                       (error "Uncompression program `%s' not found"
                              (nth 3 error-code)))
                   (signal (car error-code) (cdr error-code))))))

          (and
           local-copy
           (file-exists-p local-copy)
           (delete-file local-copy)))

        (unless notfound
          (decode-coding-inserted-region
           (point) (+ (point) size)
           (jka-compr-byte-compiler-base-file-name file)
           visit beg end replace))

        (and
         visit
         (progn
           (unlock-buffer)
           (setq buffer-file-name filename)
           (setq jka-compr-really-do-compress t)
           (set-visited-file-modtime)))

        (and
         uncompress-message
	 jka-compr-verbose
         (message "%s %s...done" uncompress-message base-name))

        (and
         visit
         notfound
         (signal 'file-error
                 (cons "Opening input file" (nth 2 notfound))))

        ;; This is done in insert-file-contents after we return.
        ;; That is a little weird, but better to go along with it now
        ;; than to change it now.

        ;; ;; Run the functions that insert-file-contents would.
        ;; (let ((p after-insert-file-functions)
        ;;       (insval size))
        ;;   (while p
        ;;     (setq insval (funcall (car p) size))
        ;;     (if insval
        ;;         (progn
        ;;           (or (integerp insval)
        ;;       	(signal 'wrong-type-argument
        ;;       		(list 'integerp insval)))
        ;;           (setq size insval)))
        ;;     (setq p (cdr p))))

        (or (jka-compr-info-compress-program info)
            (message "You can't save this buffer because compression program is not defined"))

        (list filename size)))))


(defun jka-compr-file-local-copy (file)
  (let* ((filename (expand-file-name file))
	 (info (jka-compr-get-compression-info filename)))

    (if info

	(let ((uncompress-message (jka-compr-info-uncompress-message info))
	      (uncompress-program (jka-compr-info-uncompress-program info))
	      (uncompress-args (jka-compr-info-uncompress-args info))
	      (base-name (file-name-nondirectory filename))
	      (local-copy
	       (jka-compr-run-real-handler 'file-local-copy (list filename)))
	      (temp-file (jka-compr-make-temp-name t))
	      (temp-buffer (get-buffer-create " *jka-compr-flc-temp*"))
	      local-file)

	  (setq local-file (or local-copy filename))

	  (unwind-protect

	      (with-current-buffer temp-buffer

		(and
		 uncompress-message
		 jka-compr-verbose
		 (message "%s %s..." uncompress-message base-name))

		;; Here we must read the output of uncompress program
		;; and write it to TEMP-FILE without any code
		;; conversion.  An appropriate code conversion (if
		;; necessary) is done by the later I/O operation
		;; (e.g. load).
		(let ((coding-system-for-read 'no-conversion)
		      (coding-system-for-write 'no-conversion))

		  (jka-compr-call-process uncompress-program
					  (concat uncompress-message
						  " " base-name)
					  local-file
					  t
					  nil
					  uncompress-args)

		  (and
		   uncompress-message
		   jka-compr-verbose
		   (message "%s %s...done" uncompress-message base-name))

		  (write-region
		   (point-min) (point-max) temp-file nil 'dont)))

	    (and
	     local-copy
	     (file-exists-p local-copy)
	     (delete-file local-copy))

	    (kill-buffer temp-buffer))

	  temp-file)

      (jka-compr-run-real-handler 'file-local-copy (list filename)))))


;; Support for loading compressed files.
(defun jka-compr-load (file &optional noerror nomessage _nosuffix)
  "Documented as original."

  (let* ((local-copy (jka-compr-file-local-copy file))
	 (load-file (or local-copy file)))

    (unwind-protect

	(let (inhibit-file-name-operation
	      inhibit-file-name-handlers)
	  (or nomessage
	      (message "Loading %s..." file))

	  (let ((load-force-doc-strings t))
	    (load load-file noerror t t))
	  (or nomessage
	      (message "Loading %s...done." file))
	  ;; Fix up the load history to point at the right library.
	  (let ((l (or (assoc load-file load-history)
		       ;; On MS-Windows, if load-file is in
		       ;; temporary-file-directory, it will look like
		       ;; "c:/DOCUME~1/USER/LOCALS~1/foo", whereas
		       ;; readevalloop will record its truename in
		       ;; load-history.  Therefore try truename if the
		       ;; original name is not in load-history.
		       (assoc (file-truename load-file) load-history))))
	    ;; Remove .gz and .elc?.
	    (while (file-name-extension file)
	      (setq file (file-name-sans-extension file)))
	    (setcar l file)))

      (delete-file local-copy))

    t))

(defun jka-compr-byte-compiler-base-file-name (file)
  (let ((info (jka-compr-get-compression-info file)))
    (if (and info (jka-compr-info-strip-extension info))
	(save-match-data
	  (substring file 0 (string-match (jka-compr-info-regexp info) file)))
      file)))

(put 'write-region 'jka-compr 'jka-compr-write-region)
(put 'insert-file-contents 'jka-compr 'jka-compr-insert-file-contents)
(put 'file-local-copy 'jka-compr 'jka-compr-file-local-copy)
(put 'load 'jka-compr 'jka-compr-load)
(put 'byte-compiler-base-file-name 'jka-compr
     'jka-compr-byte-compiler-base-file-name)

;;;###autoload
(defvar jka-compr-inhibit nil
  "Non-nil means inhibit automatic uncompression temporarily.
Lisp programs can bind this to t to do that.
It is not recommended to set this variable permanently to anything but nil.")

;;;###autoload
(defun jka-compr-handler (operation &rest args)
  (save-match-data
    (let ((jka-op (get operation 'jka-compr)))
      (if (and jka-op (not jka-compr-inhibit))
	  (apply jka-op args)
	(jka-compr-run-real-handler operation args)))))

;; If we are given an operation that we don't handle,
;; call the Emacs primitive for that operation,
;; and manipulate the inhibit variables
;; to prevent the primitive from calling our handler again.
(defun jka-compr-run-real-handler (operation args)
  (let ((inhibit-file-name-handlers
	 (cons 'jka-compr-handler
	       (and (eq inhibit-file-name-operation operation)
		    inhibit-file-name-handlers)))
	(inhibit-file-name-operation operation))
    (apply operation args)))

;;;###autoload
(defun jka-compr-uninstall ()
  "Uninstall jka-compr.
This removes the entries in `file-name-handler-alist' and `auto-mode-alist'
and `inhibit-local-variables-suffixes' that were added
by `jka-compr-installed'."
  ;; Delete from inhibit-local-variables-suffixes what jka-compr-install added.
  (mapc
     (function (lambda (x)
		 (and (jka-compr-info-strip-extension x)
		      (setq inhibit-local-variables-suffixes
			    (delete (jka-compr-info-regexp x)
				    inhibit-local-variables-suffixes)))))
     jka-compr-compression-info-list--internal)

  (let* ((fnha (cons nil file-name-handler-alist))
	 (last fnha))

    (while (cdr last)
      (if (eq (cdr (car (cdr last))) 'jka-compr-handler)
	  (setcdr last (cdr (cdr last)))
	(setq last (cdr last))))

    (setq file-name-handler-alist (cdr fnha)))

  (let* ((ama (cons nil auto-mode-alist))
	 (last ama)
	 entry)

    (while (cdr last)
      (setq entry (car (cdr last)))
      (if (or (member entry jka-compr-mode-alist-additions--internal)
	      (and (consp (cdr entry))
		   (eq (nth 2 entry) 'jka-compr)))
	  (setcdr last (cdr (cdr last)))
	(setq last (cdr last))))

    (setq auto-mode-alist (cdr ama)))

  (while jka-compr-added-to-file-coding-system-alist
    (setq file-coding-system-alist
          (delq (car (member (pop jka-compr-added-to-file-coding-system-alist)
                             file-coding-system-alist))
                file-coding-system-alist)))

  ;; Remove the suffixes that were added by jka-compr.
  (dolist (suff jka-compr-load-suffixes--internal)
    (setq load-file-rep-suffixes (delete suff load-file-rep-suffixes)))

  (setq jka-compr-compression-info-list--internal nil
	jka-compr-mode-alist-additions--internal nil
	jka-compr-load-suffixes--internal nil))

(provide 'jka-compr)

;;; jka-compr.el ends here
