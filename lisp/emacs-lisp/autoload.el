;; autoload.el --- maintain autoloads in loaddefs.el

;; Copyright (C) 1991-1997, 2001-2012  Free Software Foundation, Inc.

;; Author: Roland McGrath <roland@gnu.org>
;; Keywords: maint
;; Package: emacs

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

;; This code helps GNU Emacs maintainers keep the loaddefs.el file up to
;; date.  It interprets magic cookies of the form ";;;###autoload" in
;; lisp source files in various useful ways.  To learn more, read the
;; source; if you're going to use this, you'd better be able to.

;;; Code:

(require 'lisp-mode)			;for `doc-string-elt' properties.
(require 'help-fns)			;for help-add-fundoc-usage.
(eval-when-compile (require 'cl))

(defvar generated-autoload-file nil
  "File into which to write autoload definitions.
A Lisp file can set this in its local variables section to make
its autoloads go somewhere else.

If this is a relative file name, the directory is determined as
follows:
 - If a Lisp file defined `generated-autoload-file' as a
   file-local variable, use its containing directory.
 - Otherwise use the \"lisp\" subdirectory of `source-directory'.

The autoload file is assumed to contain a trailer starting with a
FormFeed character.")
;;;###autoload
(put 'generated-autoload-file 'safe-local-variable 'stringp)

(defvar generated-autoload-load-name nil
  "Load name for `autoload' statements generated from autoload cookies.
If nil, this defaults to the file name, sans extension.")
;;;###autoload
(put 'generated-autoload-load-name 'safe-local-variable 'stringp)

;; This feels like it should be a defconst, but MH-E sets it to
;; ";;;###mh-autoload" for the autoloads that are to go into mh-loaddefs.el.
(defvar generate-autoload-cookie ";;;###autoload"
  "Magic comment indicating the following form should be autoloaded.
Used by \\[update-file-autoloads].  This string should be
meaningless to Lisp (e.g., a comment).

This string is used:

\;;;###autoload
\(defun function-to-be-autoloaded () ...)

If this string appears alone on a line, the following form will be
read and an autoload made for it.  If there is further text on the line,
that text will be copied verbatim to `generated-autoload-file'.")

(defvar autoload-excludes nil
  "If non-nil, list of absolute file names not to scan for autoloads.")

(defconst generate-autoload-section-header "\f\n;;;### "
  "String that marks the form at the start of a new file's autoload section.")

(defconst generate-autoload-section-trailer "\n;;;***\n"
  "String which indicates the end of the section of autoloads for a file.")

(defconst generate-autoload-section-continuation ";;;;;; "
  "String to add on each continuation of the section header form.")

(defvar autoload-modified-buffers)      ;Dynamically scoped var.

(defun make-autoload (form file)
  "Turn FORM into an autoload or defvar for source file FILE.
Returns nil if FORM is not a special autoload form (i.e. a function definition
or macro definition or a defcustom)."
  (let ((car (car-safe form)) expand)
    (cond
     ;; For complex cases, try again on the macro-expansion.
     ((and (memq car '(easy-mmode-define-global-mode define-global-minor-mode
		       define-globalized-minor-mode
		       easy-mmode-define-minor-mode define-minor-mode))
	   (setq expand (let ((load-file-name file)) (macroexpand form)))
	   (eq (car expand) 'progn)
	   (memq :autoload-end expand))
      (let ((end (memq :autoload-end expand)))
	;; Cut-off anything after the :autoload-end marker.
	(setcdr end nil)
	(cons 'progn
	      (mapcar (lambda (form) (make-autoload form file))
		      (cdr expand)))))

     ;; For special function-like operators, use the `autoload' function.
     ((memq car '(defun define-skeleton defmacro define-derived-mode
                   define-compilation-mode define-generic-mode
		   easy-mmode-define-global-mode define-global-minor-mode
		   define-globalized-minor-mode
		   easy-mmode-define-minor-mode define-minor-mode
		   defun* defmacro* define-overloadable-function))
      (let* ((macrop (memq car '(defmacro defmacro*)))
	     (name (nth 1 form))
	     (args (case car
                     ((defun defmacro defun* defmacro*
                        define-overloadable-function) (nth 2 form))
                     ((define-skeleton) '(&optional str arg))
                     ((define-generic-mode define-derived-mode
                        define-compilation-mode) nil)
                     (t)))
	     (body (nthcdr (get car 'doc-string-elt) form))
	     (doc (if (stringp (car body)) (pop body))))
	(when (listp args)
	  ;; Add the usage form at the end where describe-function-1
	  ;; can recover it.
	  (setq doc (help-add-fundoc-usage doc args)))
        (let ((exp
               ;; `define-generic-mode' quotes the name, so take care of that
               (list 'autoload (if (listp name) name (list 'quote name))
                     file doc
                     (or (and (memq car '(define-skeleton define-derived-mode
                                           define-generic-mode
                                           easy-mmode-define-global-mode
                                           define-global-minor-mode
                                           define-globalized-minor-mode
                                           easy-mmode-define-minor-mode
                                           define-minor-mode)) t)
                         (eq (car-safe (car body)) 'interactive))
                     (if macrop (list 'quote 'macro) nil))))
          (when macrop
            ;; Special case to autoload some of the macro's declarations.
            (let ((decls (nth (if (stringp (nth 3 form)) 4 3) form))
                  (exps '()))
              (when (eq (car-safe decls) 'declare)
                ;; FIXME: We'd like to reuse macro-declaration-function,
                ;; but we can't since it doesn't return anything.
                (dolist (decl decls)
                  (case (car-safe decl)
                    (indent
                     (push `(put ',name 'lisp-indent-function ',(cadr decl))
                           exps))
                    (doc-string
                     (push `(put ',name 'doc-string-elt ',(cadr decl)) exps))))
                (when exps
                  (setq exp `(progn ,exp ,@exps))))))
          exp)))

     ;; For defclass forms, use `eieio-defclass-autoload'.
     ((eq car 'defclass)
      (let ((name (nth 1 form))
	    (superclasses (nth 2 form))
	    (doc (nth 4 form)))
	(list 'eieio-defclass-autoload (list 'quote name)
	      (list 'quote superclasses) file doc)))

     ;; Convert defcustom to less space-consuming data.
     ((eq car 'defcustom)
      (let ((varname (car-safe (cdr-safe form)))
	    (init (car-safe (cdr-safe (cdr-safe form))))
	    (doc (car-safe (cdr-safe (cdr-safe (cdr-safe form)))))
	    ;; (rest (cdr-safe (cdr-safe (cdr-safe (cdr-safe form)))))
	    )
	`(progn
	   (defvar ,varname ,init ,doc)
	   (custom-autoload ',varname ,file
                            ,(condition-case nil
                                 (null (cadr (memq :set form)))
                               (error nil))))))

     ((eq car 'defgroup)
      ;; In Emacs this is normally handled separately by cus-dep.el, but for
      ;; third party packages, it can be convenient to explicitly autoload
      ;; a group.
      (let ((groupname (nth 1 form)))
        `(let ((loads (get ',groupname 'custom-loads)))
           (if (member ',file loads) nil
             (put ',groupname 'custom-loads (cons ',file loads))))))

     ;; nil here indicates that this is not a special autoload form.
     (t nil))))

;; Forms which have doc-strings which should be printed specially.
;; A doc-string-elt property of ELT says that (nth ELT FORM) is
;; the doc-string in FORM.
;; Those properties are now set in lisp-mode.el.

(defun autoload-find-generated-file ()
  "Visit the autoload file for the current buffer, and return its buffer.
If a buffer is visiting the desired autoload file, return it."
  (let ((enable-local-variables :safe))
    ;; We used to use `raw-text' to read this file, but this causes
    ;; problems when the file contains non-ASCII characters.
    (find-file-noselect
     (autoload-ensure-default-file (autoload-generated-file)))))

(defun autoload-generated-file ()
  (expand-file-name generated-autoload-file
                    ;; File-local settings of generated-autoload-file should
                    ;; be interpreted relative to the file's location,
                    ;; of course.
                    (if (not (local-variable-p 'generated-autoload-file))
                        (expand-file-name "lisp" source-directory))))


(defun autoload-read-section-header ()
  "Read a section header form.
Since continuation lines have been marked as comments,
we must copy the text of the form and remove those comment
markers before we call `read'."
  (save-match-data
    (let ((beginning (point))
	  string)
      (forward-line 1)
      (while (looking-at generate-autoload-section-continuation)
	(forward-line 1))
      (setq string (buffer-substring beginning (point)))
      (with-current-buffer (get-buffer-create " *autoload*")
	(erase-buffer)
	(insert string)
	(goto-char (point-min))
	(while (search-forward generate-autoload-section-continuation nil t)
	  (replace-match " "))
	(goto-char (point-min))
	(read (current-buffer))))))

(defvar autoload-print-form-outbuf nil
  "Buffer which gets the output of `autoload-print-form'.")

(defun autoload-print-form (form)
  "Print FORM such that `make-docfile' will find the docstrings.
The variable `autoload-print-form-outbuf' specifies the buffer to
put the output in."
  (cond
   ;; If the form is a sequence, recurse.
   ((eq (car form) 'progn) (mapcar 'autoload-print-form (cdr form)))
   ;; Symbols at the toplevel are meaningless.
   ((symbolp form) nil)
   (t
    (let ((doc-string-elt (get (car-safe form) 'doc-string-elt))
	  (outbuf autoload-print-form-outbuf))
      (if (and doc-string-elt (stringp (nth doc-string-elt form)))
	  ;; We need to hack the printing because the
	  ;; doc-string must be printed specially for
	  ;; make-docfile (sigh).
	  (let* ((p (nthcdr (1- doc-string-elt) form))
		 (elt (cdr p)))
	    (setcdr p nil)
	    (princ "\n(" outbuf)
	    (let ((print-escape-newlines t)
                  (print-quoted t)
		  (print-escape-nonascii t))
	      (dolist (elt form)
		(prin1 elt outbuf)
		(princ " " outbuf)))
	    (princ "\"\\\n" outbuf)
	    (let ((begin (with-current-buffer outbuf (point))))
	      (princ (substring (prin1-to-string (car elt)) 1)
		     outbuf)
	      ;; Insert a backslash before each ( that
	      ;; appears at the beginning of a line in
	      ;; the doc string.
	      (with-current-buffer outbuf
		(save-excursion
		  (while (re-search-backward "\n[[(]" begin t)
		    (forward-char 1)
		    (insert "\\"))))
	      (if (null (cdr elt))
		  (princ ")" outbuf)
		(princ " " outbuf)
		(princ (substring (prin1-to-string (cdr elt)) 1)
		       outbuf))
	      (terpri outbuf)))
	(let ((print-escape-newlines t)
              (print-quoted t)
	      (print-escape-nonascii t))
	  (print form outbuf)))))))

(defun autoload-rubric (file &optional type feature)
  "Return a string giving the appropriate autoload rubric for FILE.
TYPE (default \"autoloads\") is a string stating the type of
information contained in FILE.  If FEATURE is non-nil, FILE
will provide a feature.  FEATURE may be a string naming the
feature, otherwise it will be based on FILE's name.

At present, a feature is in fact always provided, but this should
not be relied upon."
  (let ((basename (file-name-nondirectory file)))
    (concat ";;; " basename
	    " --- automatically extracted " (or type "autoloads") "\n"
	    ";;\n"
	    ";;; Code:\n\n"
	    "\n"
	    ;; This is used outside of autoload.el, eg cus-dep, finder.
	    "(provide '"
	    (if (stringp feature)
		feature
	      (file-name-sans-extension basename))
	    ")\n"
	    ";; Local Variables:\n"
	    ";; version-control: never\n"
	    ";; no-byte-compile: t\n"
	    ";; no-update-autoloads: t\n"
	    ";; coding: utf-8\n"
	    ";; End:\n"
	    ";;; " basename
	    " ends here\n")))

(defun autoload-ensure-default-file (file)
  "Make sure that the autoload file FILE exists and if not create it."
  (unless (file-exists-p file)
    (write-region (autoload-rubric file) nil file))
  file)

(defun autoload-insert-section-header (outbuf autoloads load-name file time)
  "Insert the section-header line,
which lists the file name and which functions are in it, etc."
  (insert generate-autoload-section-header)
  (prin1 (list 'autoloads autoloads load-name file time)
	 outbuf)
  (terpri outbuf)
  ;; Break that line at spaces, to avoid very long lines.
  ;; Make each sub-line into a comment.
  (with-current-buffer outbuf
    (save-excursion
      (forward-line -1)
      (while (not (eolp))
	(move-to-column 64)
	(skip-chars-forward "^ \n")
	(or (eolp)
	    (insert "\n" generate-autoload-section-continuation))))))

(defun autoload-find-file (file)
  "Fetch file and put it in a temp buffer.  Return the buffer."
  ;; It is faster to avoid visiting the file.
  (setq file (expand-file-name file))
  (with-current-buffer (get-buffer-create " *autoload-file*")
    (kill-all-local-variables)
    (erase-buffer)
    (setq buffer-undo-list t
          buffer-read-only nil)
    (emacs-lisp-mode)
    (setq default-directory (file-name-directory file))
    (insert-file-contents file nil)
    (let ((enable-local-variables :safe))
      (hack-local-variables))
    (current-buffer)))

(defvar no-update-autoloads nil
  "File local variable to prevent scanning this file for autoload cookies.")

(defun autoload-file-load-name (file)
  "Compute the name that will be used to load FILE."
  ;; OUTFILE should be the name of the global loaddefs.el file, which
  ;; is expected to be at the root directory of the files we're
  ;; scanning for autoloads and will be in the `load-path'.
  (let* ((outfile (default-value 'generated-autoload-file))
         (name (file-relative-name file (file-name-directory outfile)))
         (names '())
         (dir (file-name-directory outfile)))
    ;; If `name' has directory components, only keep the
    ;; last few that are really needed.
    (while name
      (setq name (directory-file-name name))
      (push (file-name-nondirectory name) names)
      (setq name (file-name-directory name)))
    (while (not name)
      (cond
       ((null (cdr names)) (setq name (car names)))
       ((file-exists-p (expand-file-name "subdirs.el" dir))
        ;; FIXME: here we only check the existence of subdirs.el,
        ;; without checking its content.  This makes it generate wrong load
        ;; names for cases like lisp/term which is not added to load-path.
        (setq dir (expand-file-name (pop names) dir)))
       (t (setq name (mapconcat 'identity names "/")))))
    (if (string-match "\\.elc?\\(\\.\\|\\'\\)" name)
        (substring name 0 (match-beginning 0))
      name)))

(defun generate-file-autoloads (file)
  "Insert at point a loaddefs autoload section for FILE.
Autoloads are generated for defuns and defmacros in FILE
marked by `generate-autoload-cookie' (which see).
If FILE is being visited in a buffer, the contents of the buffer
are used.
Return non-nil in the case where no autoloads were added at point."
  (interactive "fGenerate autoloads for file: ")
  (let ((generated-autoload-file buffer-file-name))
    (autoload-generate-file-autoloads file (current-buffer))))

(defvar print-readably)

;; When called from `generate-file-autoloads' we should ignore
;; `generated-autoload-file' altogether.  When called from
;; `update-file-autoloads' we don't know `outbuf'.  And when called from
;; `update-directory-autoloads' it's in between: we know the default
;; `outbuf' but we should obey any file-local setting of
;; `generated-autoload-file'.
(defun autoload-generate-file-autoloads (file &optional outbuf outfile)
  "Insert an autoload section for FILE in the appropriate buffer.
Autoloads are generated for defuns and defmacros in FILE
marked by `generate-autoload-cookie' (which see).
If FILE is being visited in a buffer, the contents of the buffer are used.
OUTBUF is the buffer in which the autoload statements should be inserted.
If OUTBUF is nil, it will be determined by `autoload-generated-file'.

If provided, OUTFILE is expected to be the file name of OUTBUF.
If OUTFILE is non-nil and FILE specifies a `generated-autoload-file'
different from OUTFILE, then OUTBUF is ignored.

Return non-nil if and only if FILE adds no autoloads to OUTFILE
\(or OUTBUF if OUTFILE is nil)."
  (catch 'done
    (let ((autoloads-done '())
	  load-name
          (print-length nil)
	  (print-level nil)
          (print-readably t)           ; This does something in Lucid Emacs.
          (float-output-format nil)
          (visited (get-file-buffer file))
          (otherbuf nil)
          (absfile (expand-file-name file))
          ;; nil until we found a cookie.
          output-start ostart)
      (with-current-buffer (or visited
                               ;; It is faster to avoid visiting the file.
                               (autoload-find-file file))
        ;; Obey the no-update-autoloads file local variable.
        (unless no-update-autoloads
          (message "Generating autoloads for %s..." file)
	  (setq load-name
		(if (stringp generated-autoload-load-name)
		    generated-autoload-load-name
		  (autoload-file-load-name absfile)))
          (when (and outfile
                     (not
		      (if (memq system-type '(ms-dos windows-nt))
			  (equal (downcase outfile)
				 (downcase (autoload-generated-file)))
			(equal outfile (autoload-generated-file)))))
            (setq otherbuf t))
          (save-excursion
            (save-restriction
              (widen)
              (goto-char (point-min))
              (while (not (eobp))
                (skip-chars-forward " \t\n\f")
                (cond
                 ((looking-at (regexp-quote generate-autoload-cookie))
                  ;; If not done yet, figure out where to insert this text.
                  (unless output-start
                    (let ((outbuf
                           (or (if otherbuf
                                   ;; A file-local setting of
                                   ;; autoload-generated-file says we
                                   ;; should ignore OUTBUF.
                                   nil
                                 outbuf)
                               (autoload-find-destination absfile load-name)
                               ;; The file has autoload cookies, but they're
                               ;; already up-to-date. If OUTFILE is nil, the
                               ;; entries are in the expected OUTBUF,
                               ;; otherwise they're elsewhere.
                               (throw 'done otherbuf))))
                      (with-current-buffer outbuf
                        (setq output-start (point-marker)
                              ostart (point)))))
                  (search-forward generate-autoload-cookie)
                  (skip-chars-forward " \t")
                  (if (eolp)
                      (condition-case err
                          ;; Read the next form and make an autoload.
                          (let* ((form (prog1 (read (current-buffer))
                                         (or (bolp) (forward-line 1))))
                                 (autoload (make-autoload form load-name)))
                            (if autoload
                                (push (nth 1 form) autoloads-done)
                              (setq autoload form))
                            (let ((autoload-print-form-outbuf
                                   (marker-buffer output-start)))
                              (autoload-print-form autoload)))
                        (error
                         (message "Autoload cookie error in %s:%s %S"
                                  file (count-lines (point-min) (point)) err)))

                    ;; Copy the rest of the line to the output.
                    (princ (buffer-substring
                            (progn
                              ;; Back up over whitespace, to preserve it.
                              (skip-chars-backward " \f\t")
                              (if (= (char-after (1+ (point))) ? )
                                  ;; Eat one space.
                                  (forward-char 1))
                              (point))
                            (progn (forward-line 1) (point)))
                           (marker-buffer output-start))))
                 ((looking-at ";")
                  ;; Don't read the comment.
                  (forward-line 1))
                 (t
                  (forward-sexp 1)
                  (forward-line 1))))))

          (when output-start
            (let ((secondary-autoloads-file-buf
                   (if otherbuf (current-buffer))))
              (with-current-buffer (marker-buffer output-start)
                (save-excursion
                  ;; Insert the section-header line which lists the file name
                  ;; and which functions are in it, etc.
                  (assert (= ostart output-start))
                  (goto-char output-start)
                  (let ((relfile (file-relative-name absfile)))
                    (autoload-insert-section-header
                     (marker-buffer output-start)
                     autoloads-done load-name relfile
                     (if secondary-autoloads-file-buf
                         ;; MD5 checksums are much better because they do not
                         ;; change unless the file changes (so they'll be
                         ;; equal on two different systems and will change
                         ;; less often than time-stamps, thus leading to fewer
                         ;; unneeded changes causing spurious conflicts), but
                         ;; using time-stamps is a very useful optimization,
                         ;; so we use time-stamps for the main autoloads file
                         ;; (loaddefs.el) where we have special ways to
                         ;; circumvent the "random change problem", and MD5
                         ;; checksum in secondary autoload files where we do
                         ;; not need the time-stamp optimization because it is
                         ;; already provided by the primary autoloads file.
                         (md5 secondary-autoloads-file-buf
                              ;; We'd really want to just use
                              ;; `emacs-internal' instead.
                              nil nil 'emacs-mule-unix)
                       (nth 5 (file-attributes relfile))))
                    (insert ";;; Generated autoloads from " relfile "\n")))
                (insert generate-autoload-section-trailer))))
          (message "Generating autoloads for %s...done" file))
        (or visited
            ;; We created this buffer, so we should kill it.
            (kill-buffer (current-buffer))))
      (or (not output-start)
          ;; If the entries were added to some other buffer, then the file
          ;; doesn't add entries to OUTFILE.
          otherbuf))))

(defun autoload-save-buffers ()
  (while autoload-modified-buffers
    (with-current-buffer (pop autoload-modified-buffers)
      (let ((version-control 'never))
	(save-buffer)))))

;;;###autoload
(defun update-file-autoloads (file &optional save-after outfile)
  "Update the autoloads for FILE.
If prefix arg SAVE-AFTER is non-nil, save the buffer too.

If FILE binds `generated-autoload-file' as a file-local variable,
autoloads are written into that file.  Otherwise, the autoloads
file is determined by OUTFILE.  If called interactively, prompt
for OUTFILE; if called from Lisp with OUTFILE nil, use the
existing value of `generated-autoload-file'.

Return FILE if there was no autoload cookie in it, else nil."
  (interactive (list (read-file-name "Update autoloads for file: ")
		     current-prefix-arg
		     (read-file-name "Write autoload definitions to file: ")))
  (let* ((generated-autoload-file (or outfile generated-autoload-file))
	 (autoload-modified-buffers nil)
         (no-autoloads (autoload-generate-file-autoloads file)))
    (if autoload-modified-buffers
        (if save-after (autoload-save-buffers))
      (if (called-interactively-p 'interactive)
          (message "Autoload section for %s is up to date." file)))
    (if no-autoloads file)))

(defun autoload-find-destination (file load-name)
  "Find the destination point of the current buffer's autoloads.
FILE is the file name of the current buffer.
Returns a buffer whose point is placed at the requested location.
Returns nil if the file's autoloads are uptodate, otherwise
removes any prior now out-of-date autoload entries."
  (catch 'up-to-date
    (let* ((buf (current-buffer))
           (existing-buffer (if buffer-file-name buf))
           (found nil))
      (with-current-buffer (autoload-find-generated-file)
        ;; This is to make generated-autoload-file have Unix EOLs, so
        ;; that it is portable to all platforms.
        (or (eq 0 (coding-system-eol-type buffer-file-coding-system))
	    (set-buffer-file-coding-system 'unix))
        (or (> (buffer-size) 0)
            (error "Autoloads file %s lacks boilerplate" buffer-file-name))
        (or (file-writable-p buffer-file-name)
            (error "Autoloads file %s is not writable" buffer-file-name))
        (widen)
        (goto-char (point-min))
        ;; Look for the section for LOAD-NAME.
        (while (and (not found)
                    (search-forward generate-autoload-section-header nil t))
          (let ((form (autoload-read-section-header)))
            (cond ((string= (nth 2 form) load-name)
                   ;; We found the section for this file.
                   ;; Check if it is up to date.
                   (let ((begin (match-beginning 0))
                         (last-time (nth 4 form))
                         (file-time (nth 5 (file-attributes file))))
                     (if (and (or (null existing-buffer)
                                  (not (buffer-modified-p existing-buffer)))
                              (or
                               ;; last-time is the time-stamp (specifying
                               ;; the last time we looked at the file) and
                               ;; the file hasn't been changed since.
                               (and (listp last-time) (= (length last-time) 2)
                                    (not (time-less-p last-time file-time)))
                               ;; last-time is an MD5 checksum instead.
                               (and (stringp last-time)
                                    (equal last-time
                                           (md5 buf nil nil 'emacs-mule)))))
                         (throw 'up-to-date nil)
                       (autoload-remove-section begin)
                       (setq found t))))
                  ((string< load-name (nth 2 form))
                   ;; We've come to a section alphabetically later than
                   ;; LOAD-NAME.  We assume the file is in order and so
                   ;; there must be no section for LOAD-NAME.  We will
                   ;; insert one before the section here.
                   (goto-char (match-beginning 0))
                   (setq found t)))))
        (or found
            (progn
              ;; No later sections in the file.  Put before the last page.
              (goto-char (point-max))
              (search-backward "\f" nil t)))
        (unless (memq (current-buffer) autoload-modified-buffers)
          (push (current-buffer) autoload-modified-buffers))
        (current-buffer)))))

(defun autoload-remove-section (begin)
  (goto-char begin)
  (search-forward generate-autoload-section-trailer)
  (delete-region begin (point)))

;;;###autoload
(defun update-directory-autoloads (&rest dirs)
  "Update autoload definitions for Lisp files in the directories DIRS.
In an interactive call, you must give one argument, the name of a
single directory.  In a call from Lisp, you can supply multiple
directories as separate arguments, but this usage is discouraged.

The function does NOT recursively descend into subdirectories of the
directory or directories specified.

In an interactive call, prompt for a default output file for the
autoload definitions, and temporarily bind the variable
`generated-autoload-file' to this value.  When called from Lisp,
use the existing value of `generated-autoload-file'.  If any Lisp
file binds `generated-autoload-file' as a file-local variable,
write its autoloads into the specified file instead."
  (interactive "DUpdate autoloads from directory: ")
  (let* ((files-re (let ((tmp nil))
		     (dolist (suf (get-load-suffixes)
				  (concat "^[^=.].*" (regexp-opt tmp t) "\\'"))
		       (unless (string-match "\\.elc" suf) (push suf tmp)))))
	 (files (apply 'nconc
		       (mapcar (lambda (dir)
				 (directory-files (expand-file-name dir)
						  t files-re))
			       dirs)))
         (done ())
	 (this-time (current-time))
         ;; Files with no autoload cookies or whose autoloads go to other
         ;; files because of file-local autoload-generated-file settings.
	 (no-autoloads nil)
         (autoload-modified-buffers nil)
	 (generated-autoload-file
	  (if (called-interactively-p 'interactive)
	      (read-file-name "Write autoload definitions to file: ")
	    generated-autoload-file)))

    (with-current-buffer (autoload-find-generated-file)
      (save-excursion
	;; Canonicalize file names and remove the autoload file itself.
	(setq files (delete (file-relative-name buffer-file-name)
			    (mapcar 'file-relative-name files)))

	(goto-char (point-min))
	(while (search-forward generate-autoload-section-header nil t)
	  (let* ((form (autoload-read-section-header))
		 (file (nth 3 form)))
	    (cond ((and (consp file) (stringp (car file)))
		   ;; This is a list of files that have no autoload cookies.
		   ;; There shouldn't be more than one such entry.
		   ;; Remove the obsolete section.
		   (autoload-remove-section (match-beginning 0))
		   (let ((last-time (nth 4 form)))
		     (dolist (file file)
		       (let ((file-time (nth 5 (file-attributes file))))
			 (when (and file-time
				    (not (time-less-p last-time file-time)))
			   ;; file unchanged
			   (push file no-autoloads)
			   (setq files (delete file files)))))))
		  ((not (stringp file)))
		  ((or (not (file-exists-p file))
                       ;; Remove duplicates as well, just in case.
                       (member file done)
                       ;; If the file is actually excluded.
                       (member (expand-file-name file) autoload-excludes))
                   ;; Remove the obsolete section.
		   (autoload-remove-section (match-beginning 0)))
		  ((not (time-less-p (nth 4 form)
                                     (nth 5 (file-attributes file))))
		   ;; File hasn't changed.
		   nil)
		  (t
                   (autoload-remove-section (match-beginning 0))
                   (if (autoload-generate-file-autoloads
                        ;; Passing `current-buffer' makes it insert at point.
                        file (current-buffer) buffer-file-name)
                       (push file no-autoloads))))
            (push file done)
	    (setq files (delete file files)))))
      ;; Elements remaining in FILES have no existing autoload sections yet.
      (dolist (file files)
        (cond
         ((member (expand-file-name file) autoload-excludes) nil)
         ;; Passing nil as second argument forces
         ;; autoload-generate-file-autoloads to look for the right
         ;; spot where to insert each autoloads section.
         ((autoload-generate-file-autoloads file nil buffer-file-name)
          (push file no-autoloads))))

      (when no-autoloads
	;; Sort them for better readability.
	(setq no-autoloads (sort no-autoloads 'string<))
	;; Add the `no-autoloads' section.
	(goto-char (point-max))
	(search-backward "\f" nil t)
	(autoload-insert-section-header
	 (current-buffer) nil nil no-autoloads this-time)
	(insert generate-autoload-section-trailer))

      (let ((version-control 'never))
	(save-buffer))
      ;; In case autoload entries were added to other files because of
      ;; file-local autoload-generated-file settings.
      (autoload-save-buffers))))

(define-obsolete-function-alias 'update-autoloads-from-directories
    'update-directory-autoloads "22.1")

(defvar autoload-make-program (or (getenv "MAKE") "make")
  "Name of the make program in use during the Emacs build process.")

;;;###autoload
(defun batch-update-autoloads ()
  "Update loaddefs.el autoloads in batch mode.
Calls `update-directory-autoloads' on the command line arguments.
Definitions are written to `generated-autoload-file' (which
should be non-nil)."
  ;; For use during the Emacs build process only.
  ;; Exclude those files that are preloaded on ALL platforms.
  ;; These are the ones in loadup.el where "(load" is at the start
  ;; of the line (crude, but it works).
  (unless autoload-excludes
    (let ((default-directory (file-name-directory generated-autoload-file))
	  file)
      (when (file-readable-p "loadup.el")
	(with-temp-buffer
	  (insert-file-contents "loadup.el")
	  (while (re-search-forward "^(load \"\\([^\"]+\\)\"" nil t)
	    (setq file (match-string 1))
	    (or (string-match "\\.el\\'" file)
		(setq file (format "%s.el" file)))
	    (or (string-match "\\`site-" file)
		(push (expand-file-name file) autoload-excludes)))))))
  (let ((args command-line-args-left))
    (setq command-line-args-left nil)
    (apply 'update-directory-autoloads args)))

(provide 'autoload)

;;; autoload.el ends here
