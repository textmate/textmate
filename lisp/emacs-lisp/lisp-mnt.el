;;; lisp-mnt.el --- utility functions for Emacs Lisp maintainers

;; Copyright (C) 1992, 1994, 1997, 2000-2012 Free Software Foundation, Inc.

;; Author: Eric S. Raymond <esr@snark.thyrsus.com>
;; Maintainer: FSF
;; Created: 14 Jul 1992
;; Keywords: docs
;; X-Bogus-Bureaucratic-Cruft: Gruad will get you if you don't watch out!

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

;; This library adds some services to Emacs-Lisp editing mode.
;;
;; First, it knows about the header conventions for library packages.
;; One entry point supports generating synopses from a library directory.
;; Another can be used to check for missing headers in library files.
;;
;; Another entry point automatically addresses bug mail to a package's
;; maintainer or author.

;; This file can be loaded by your emacs-lisp-mode-hook.  Have it
;; (require 'lisp-mnt)

;; This file is an example of the header conventions.  Note the following
;; features:
;;
;;    * Header line --- makes it possible to extract a one-line summary of
;; the package's uses automatically for use in library synopses, KWIC
;; indexes and the like.
;;
;;    Format is three semicolons, followed by the filename, followed by
;; three dashes, followed by the summary.  All fields space-separated.
;;
;;    * A blank line
;;
;;    * Copyright line, which looks more or less like this:
;;
;;       ;; Copyright (C) 1999, 2000, 2001 Free Software Foundation, Inc.
;;
;;    * A blank line
;;
;;    * Author line --- contains the name and net address of at least
;; the principal author.
;;
;;    If there are multiple authors, they should be listed on continuation
;; lines led by ;;<TAB>, like this:
;;
;; ;; Author: Ashwin Ram <Ram-Ashwin@cs.yale.edu>
;; ;;	Dave Sill <de5@ornl.gov>
;; ;;	David Lawrence <tale@pawl.rpi.edu>
;; ;;	Noah Friedman <friedman@ai.mit.edu>
;; ;;	Joe Wells <jbw@maverick.uswest.com>
;; ;;	Dave Brennan <brennan@hal.com>
;; ;;	Eric Raymond <esr@snark.thyrsus.com>
;;
;; This field may have some special values; notably "FSF", meaning
;; "Free Software Foundation".
;;
;;    * Maintainer line --- should be a single name/address as in the Author
;; line, or an address only, or the string "FSF".  If there is no maintainer
;; line, the person(s) in the Author field are presumed to be it.
;;    The idea behind these two fields is to be able to write a Lisp function
;; that does "send mail to the author" without having to mine the name out by
;; hand.  Please be careful about surrounding the network address with <> if
;; there's also a name in the field.
;;
;;    * Created line --- optional, gives the original creation date of the
;; file.  For historical interest, basically.
;;
;;    * Version line --- intended to give the reader a clue if they're looking
;; at a different version of the file than the one they're accustomed to.  This
;; may be an RCS or SCCS header.
;;
;;    * Adapted-By line --- this is for FSF's internal use.  The person named
;; in this field was the one responsible for installing and adapting the
;; package for the distribution.  (This file doesn't have one because the
;; author *is* one of the maintainers.)
;;
;;    * Keywords line --- used by the finder code for finding Emacs
;; Lisp code related to a topic.
;;
;;    * X-Bogus-Bureaucratic-Cruft line --- this is a joke and an example
;; of a comment header.  Headers starting with `X-' should never be used
;; for any real purpose; this is the way to safely add random headers
;; without invoking the wrath of any program.
;;
;;    * Commentary line --- enables Lisp code to find the developer's and
;; maintainers' explanations of the package internals.
;;
;;    * Change log line --- optional, exists to terminate the commentary
;; section and start a change-log part, if one exists.
;;
;;    * Code line --- exists so Lisp can know where commentary and/or
;; change-log sections end.
;;
;;    * Footer line --- marks end-of-file so it can be distinguished from
;; an expanded formfeed or the results of truncation.

;;; Change Log:

;; Tue Jul 14 23:44:17 1992	ESR
;;	* Created.

;;; Code:

;;; Variables:

(defgroup lisp-mnt nil
  "Utility functions for Emacs Lisp maintainers."
  :prefix "lm-"
  :group 'maint)

;; At least some of these defcustoms should probably be defconsts,
;; since they define, or are defined by, the header format.  -- fx

(defcustom lm-header-prefix "^;+[ \t]+\\(@(#)\\)?[ \t]*\\$?"
  "Prefix that is ignored before the tag.
For example, you can write the 1st line synopsis string and headers like this
in your Lisp package:

   ;; @(#) package.el -- package description
   ;;
   ;; @(#) $Maintainer:   Person Foo Bar $

The @(#) construct is used by unix what(1) and
then $identifier: doc string $ is used by GNU ident(1)"
  :type 'regexp
  :group 'lisp-mnt)

(defcustom lm-copyright-prefix "^\\(;+[ \t]\\)+Copyright (C) "
  "Prefix that is ignored before the dates in a copyright.
Leading comment characters and whitespace should be in regexp group 1."
  :type 'regexp
  :group 'lisp-mnt)

(defcustom lm-comment-column 16
  "Column used for placing formatted output."
  :type 'integer
  :group 'lisp-mnt)

(defcustom lm-any-header ".*"
  "Regexp which matches start of any section."
  :type 'regexp
  :group 'lisp-mnt)

(defcustom lm-commentary-header "Commentary\\|Documentation"
  "Regexp which matches start of documentation section."
  :type 'regexp
  :group 'lisp-mnt)

(defcustom lm-history-header "Change ?Log\\|History"
  "Regexp which matches the start of code log section."
  :type 'regexp
  :group 'lisp-mnt)

;;; Functions:

;; These functions all parse the headers of the current buffer

(defun lm-get-header-re (header &optional mode)
  "Return regexp for matching HEADER.
If called with optional MODE and with value `section',
return section regexp instead."
  (if (eq mode 'section)
      (concat "^;;;;* \\(" header "\\):[ \t]*$")
    (concat lm-header-prefix "\\(" header "\\)[ \t]*:[ \t]*")))

(defun lm-get-package-name ()
  "Return package name by looking at the first line."
  (save-excursion
    (goto-char (point-min))
    (if (and (looking-at (concat lm-header-prefix))
	     (progn (goto-char (match-end 0))
		    (looking-at "\\([^\t ]+\\)")
		    (match-end 1)))
	(match-string-no-properties 1))))

(defun lm-section-start (header &optional after)
  "Return the buffer location of a given section start marker.
The HEADER is the section mark string to search for.
If AFTER is non-nil, return the location of the next line.
If the given section does not exist, return nil."
  (save-excursion
    (let ((case-fold-search t))
      (goto-char (point-min))
      (if (re-search-forward (lm-get-header-re header 'section) nil t)
          (line-beginning-position (if after 2))))))
(defalias 'lm-section-mark 'lm-section-start)

(defun lm-section-end (header)
  "Return the buffer location of the end of a given section.
The HEADER is the section string marking the beginning of the
section.  If the given section does not exist, return nil.

The end of the section is defined as the beginning of the next
section of the same level or lower.  The function
`lisp-outline-level' is used to compute the level of a section.
If no such section exists, return the end of the buffer."
  (require 'outline)   ;; for outline-regexp.
  (let ((start (lm-section-start header)))
    (when start
      (save-excursion
        (goto-char start)
        (let ((level (lisp-outline-level))
              (case-fold-search t)
              next-section-found)
          (beginning-of-line 2)
          (while (and (setq next-section-found
                            (re-search-forward
                             (lm-get-header-re lm-any-header 'section)
                             nil t))
                      (> (save-excursion
                           (beginning-of-line)
                           (lisp-outline-level))
                         level)))
          (if next-section-found
              (line-beginning-position)
            (point-max)))))))

(defsubst lm-code-start ()
  "Return the buffer location of the `Code' start marker."
  (lm-section-start "Code"))
(defalias 'lm-code-mark 'lm-code-start)

(defsubst lm-commentary-start ()
  "Return the buffer location of the `Commentary' start marker."
  (lm-section-start lm-commentary-header))
(defalias 'lm-commentary-mark 'lm-commentary-start)

(defsubst lm-commentary-end ()
  "Return the buffer location of the `Commentary' section end."
  (lm-section-end lm-commentary-header))

(defsubst lm-history-start ()
  "Return the buffer location of the `History' start marker."
  (lm-section-start lm-history-header))
(defalias 'lm-history-mark 'lm-history-start)

(defsubst lm-copyright-mark ()
  "Return the buffer location of the `Copyright' line."
  (save-excursion
    (let ((case-fold-search t))
      (goto-char (point-min))
      (if (re-search-forward lm-copyright-prefix nil t)
	  (point)))))

(defun lm-header (header)
  "Return the contents of the header named HEADER."
  (goto-char (point-min))
  (let ((case-fold-search t))
    (when (and (re-search-forward (lm-get-header-re header) (lm-code-mark) t)
	       ;;   RCS ident likes format "$identifier: data$"
	       (looking-at
		(if (save-excursion
		      (skip-chars-backward "^$" (match-beginning 0))
		      (= (point) (match-beginning 0)))
		    "[^\n]+" "[^$\n]+")))
      (match-string-no-properties 0))))

(defun lm-header-multiline (header)
  "Return the contents of the header named HEADER, with continuation lines.
The returned value is a list of strings, one per line."
  (save-excursion
    (goto-char (point-min))
    (let ((res (lm-header header)))
      (when res
	(setq res (list res))
	(forward-line 1)
	(while (and (or (looking-at (concat lm-header-prefix "[\t ]+"))
			(and (not (looking-at
				   (lm-get-header-re "\\sw\\(\\sw\\|\\s_\\)*")))
			     (looking-at lm-header-prefix)))
		    (goto-char (match-end 0))
		    (looking-at ".+"))
	  (setq res (cons (match-string-no-properties 0) res))
	  (forward-line 1)))
      (nreverse res))))

;; These give us smart access to the header fields and commentary

(defmacro lm-with-file (file &rest body)
  "Execute BODY in a buffer containing the contents of FILE.
If FILE is nil, execute BODY in the current buffer."
  (declare (indent 1) (debug t))
  (let ((filesym (make-symbol "file")))
    `(let ((,filesym ,file))
       (if ,filesym
	   (with-temp-buffer
	     (insert-file-contents ,filesym)
	     (emacs-lisp-mode)
	     ,@body)
	 (save-excursion
	   ;; Switching major modes is too drastic, so just switch
	   ;; temporarily to the Emacs Lisp mode syntax table.
	   (with-syntax-table emacs-lisp-mode-syntax-table
	     ,@body))))))

;; Fixme: Probably this should be amalgamated with copyright.el; also
;; we need a check for ranges in copyright years.

(defun lm-crack-copyright (&optional file)
  "Return the copyright holder, and a list of copyright years.
Use the current buffer if FILE is nil.
Return argument is of the form (\"HOLDER\" \"YEAR1\" ... \"YEARN\")"
  (lm-with-file file
    (goto-char (lm-copyright-mark))
    (let ((holder nil)
	  (years nil)
	  (start (point))
	  (end (line-end-position)))
      ;; Cope with multi-line copyright `lines'.  Assume the second
      ;; line is indented (with the same commenting style).
      (save-excursion
	(beginning-of-line 2)
	(let ((str (concat (match-string-no-properties 1) "[ \t]+")))
	  (beginning-of-line)
	  (while (looking-at str)
	    (setq end (line-end-position))
	    (beginning-of-line 2))))
      ;; Make a single line and parse that.
      (let ((buff (current-buffer)))
	(with-temp-buffer
	  (insert-buffer-substring buff start end)
	  (goto-char (point-min))
	  (while (re-search-forward "^;+[ \t]+" nil t)
	    (replace-match ""))
	  (goto-char (point-min))
	  (while (re-search-forward " *\n" nil t)
	    (replace-match " "))
	  (goto-char (point-min))
	  (while (re-search-forward "\\([0-9]+\\),? +" nil t)
	    (setq years (cons (match-string-no-properties 1) years)))
	  (if (looking-at ".*$")
	      (setq holder (match-string-no-properties 0)))))
      (cons holder (nreverse years)))))

(defun lm-summary (&optional file)
  "Return the one-line summary of file FILE, or current buffer if FILE is nil."
  (lm-with-file file
    (goto-char (point-min))
    (if (and (looking-at lm-header-prefix)
	     (progn (goto-char (match-end 0))
		    (looking-at "[^ ]+[ \t]+--+[ \t]+\\(.*\\)")))
	(let ((summary (match-string-no-properties 1)))
	  ;; Strip off -*- specifications.
	  (if (string-match "[ \t]*-\\*-.*-\\*-" summary)
	      (substring summary 0 (match-beginning 0))
	    summary)))))

(defun lm-crack-address (x)
  "Split up an email address X into full name and real email address.
The value is a cons of the form (FULLNAME . ADDRESS)."
  (cond ((string-match "\\(.+\\) [(<]\\(\\S-+@\\S-+\\)[>)]" x)
	 (cons (match-string 1 x)
	       (match-string 2 x)))
	((string-match "\\(\\S-+@\\S-+\\) [(<]\\(.*\\)[>)]" x)
	 (cons (match-string 2 x)
	       (match-string 1 x)))
	((string-match "\\S-+@\\S-+" x)
	 (cons nil x))
	(t
	 (cons x nil))))

(defun lm-authors (&optional file)
  "Return the author list of file FILE, or current buffer if FILE is nil.
Each element of the list is a cons; the car is the full name,
the cdr is an email address."
  (lm-with-file file
    (let ((authorlist (lm-header-multiline "author")))
      (mapcar 'lm-crack-address authorlist))))

(defun lm-maintainer (&optional file)
  "Return the maintainer of file FILE, or current buffer if FILE is nil.
The return value has the form (NAME . ADDRESS)."
  (lm-with-file file
    (let ((maint (lm-header "maintainer")))
      (if maint
	  (lm-crack-address maint)
	(car (lm-authors))))))

(defun lm-creation-date (&optional file)
  "Return the created date given in file FILE, or current buffer if FILE is nil."
  (lm-with-file file
    (lm-header "created")))

(defun lm-last-modified-date (&optional file iso-date)
  "Return the modify-date given in file FILE, or current buffer if FILE is nil.
ISO-DATE non-nil means return the date in ISO 8601 format."
  (lm-with-file file
    (when (progn (goto-char (point-min))
		 (re-search-forward
		  "\\$[I]d: [^ ]+ [^ ]+ \\([^/]+\\)/\\([^/]+\\)/\\([^ ]+\\) "
		  (lm-code-mark) t))
      (let ((dd (match-string 3))
	    (mm (match-string 2))
	    (yyyy (match-string 1)))
	(if iso-date
	    (format "%s-%s-%s" yyyy mm dd)
	  (format "%s %s %s"
		  dd
		  (nth (string-to-number mm)
		       '("" "Jan" "Feb" "Mar" "Apr" "May" "Jun"
			 "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
		  yyyy))))))

(defun lm-version (&optional file)
  "Return the version listed in file FILE, or current buffer if FILE is nil.
This can be found in an RCS or SCCS header."
  (lm-with-file file
    (or (lm-header "version")
	(let ((header-max (lm-code-mark)))
	  (goto-char (point-min))
	  (cond
	   ;; Look for an RCS header
	   ((re-search-forward "\\$[I]d: [^ ]+ \\([^ ]+\\) " header-max t)
	    (match-string-no-properties 1))
	   ((re-search-forward "\\$Revision: +\\([^ ]+\\) " header-max t)
	    (match-string-no-properties 1))
	   ;; Look for an SCCS header
	   ((re-search-forward
	     (concat
	      (regexp-quote "@(#)")
	      (regexp-quote (file-name-nondirectory (buffer-file-name)))
	      "\t\\([012345679.]*\\)")
	     header-max t)
	    (match-string-no-properties 1)))))))

(defun lm-keywords (&optional file)
  "Return the keywords given in file FILE, or current buffer if FILE is nil.
The return is a `downcase'-ed string, or nil if no keywords
header.  Multi-line keywords are joined up with a space between
each line."
  (lm-with-file file
    (let ((keywords (lm-header-multiline "keywords")))
      (and keywords
	   (mapconcat 'downcase keywords " ")))))

(defun lm-keywords-list (&optional file)
  "Return list of keywords given in file FILE."
  (let ((keywords (lm-keywords file)))
    (if keywords
	(if (string-match-p "," keywords)
	    (split-string keywords ",[ \t\n]*" t)
	  (split-string keywords "[ \t\n]+" t)))))

(defvar finder-known-keywords)
(defun lm-keywords-finder-p (&optional file)
  "Return non-nil if any keywords in FILE are known to finder."
  (require 'finder)
  (let ((keys (lm-keywords-list file)))
    (catch 'keyword-found
      (while keys
	(if (assoc (intern (car keys)) finder-known-keywords)
	    (throw 'keyword-found t))
	(setq keys (cdr keys)))
      nil)))

(defun lm-adapted-by (&optional file)
  "Return the adapted-by names in file FILE, or current buffer if FILE is nil.
This is the name of the person who cleaned up this package for
distribution."
  (lm-with-file file
    (lm-header "adapted-by")))

(defun lm-commentary (&optional file)
  "Return the commentary in file FILE, or current buffer if FILE is nil.
Return the value as a string.  In the file, the commentary
section starts with the tag `Commentary' or `Documentation' and
ends just before the next section.  If the commentary section is
absent, return nil."
  (lm-with-file file
    (let ((start (lm-commentary-start)))
      (when start
        (buffer-substring-no-properties start (lm-commentary-end))))))

;;; Verification and synopses

(defun lm-insert-at-column (col &rest strings)
  "Insert, at column COL, list of STRINGS."
  (if (> (current-column) col) (insert "\n"))
  (move-to-column col t)
  (apply 'insert strings))

(defun lm-verify (&optional file showok verbose non-fsf-ok)
  "Check that the current buffer (or FILE if given) is in proper format.
If FILE is a directory, recurse on its files and generate a report in a
temporary buffer.  In that case, the optional argument SHOWOK
says display \"OK\" in temp buffer for files that have no problems.

Optional argument VERBOSE specifies verbosity level.
Optional argument NON-FSF-OK if non-nil means a non-FSF
copyright notice is allowed."
  (interactive (list nil nil t))
  (let* ((ret (and verbose "Ok"))
	 name)
    (if (and file (file-directory-p file))
	(setq ret
	      (with-temp-buffer
		(dolist (f (directory-files file nil "\\.el\\'")
			   (buffer-string))
		  (when (file-regular-p f)
		    (let ((status (lm-verify f)))
		      (insert f ":")
		      (if status
			  (lm-insert-at-column lm-comment-column status
					       "\n")
			(if showok
			    (lm-insert-at-column lm-comment-column
						 "OK\n"))))))))
      (lm-with-file file
	(setq name (lm-get-package-name))
	(setq ret
	      (cond
	       ((null name)
		"Can't find package name")
	       ((not (lm-authors))
		"`Author:' tag missing")
	       ((not (lm-maintainer))
		"`Maintainer:' tag missing")
	       ((not (lm-summary))
		"Can't find the one-line summary description")
	       ((not (lm-keywords))
		"`Keywords:' tag missing")
	       ((not (lm-keywords-finder-p))
		"`Keywords:' has no valid finder keywords (see `finder-known-keywords')")
	       ((not (lm-commentary-mark))
		"Can't find a 'Commentary' section marker")
	       ((not (lm-history-mark))
		"Can't find a 'History' section marker")
	       ((not (lm-code-mark))
		"Can't find a 'Code' section marker")
	       ((progn
		  (goto-char (point-max))
		  (not
		   (re-search-backward
		    (concat "^;;;[ \t]+" name "[ \t]+ends here[ \t]*$"
			    "\\|^;;;[ \t]+ End of file[ \t]+" name)
		    nil t)))
		"Can't find the footer line")
	       ((not (and (lm-copyright-mark) (lm-crack-copyright)))
		"Can't find a valid copyright notice")
	       ((not (or non-fsf-ok
			 (string-match "Free Software Foundation"
				       (car (lm-crack-copyright)))))
		"Copyright holder is not the Free Software Foundation")
	       (t
		ret)))))
    (if verbose
	(message "%s" ret))
    ret))

(defun lm-synopsis (&optional file showall)
  "Generate a synopsis listing for the buffer or the given FILE if given.
If FILE is a directory, recurse on its files and generate a report in
a temporary buffer.  If SHOWALL is non-nil, also generate a line for files
which do not include a recognizable synopsis."
  (interactive
   (list
    (read-file-name "Synopsis for (file or dir): ")))

  (if (and file (file-directory-p file))
      (with-output-to-temp-buffer "*Synopsis*"
        (set-buffer standard-output)
        (dolist (f (directory-files file nil ".*\\.el\\'"))
          (let ((syn (lm-synopsis (expand-file-name f file))))
            (when (or syn showall)
              (insert f ":")
              (lm-insert-at-column lm-comment-column (or syn "NA") "\n")))))
    (save-excursion
      (let ((must-kill (and file (not (get-file-buffer file)))))
        (when file (find-file file))
        (prog1
            (if (called-interactively-p 'interactive)
                (message "%s" (lm-summary))
              (lm-summary))
          (when must-kill (kill-buffer (current-buffer))))))))

(defvar report-emacs-bug-address)

(defun lm-report-bug (topic)
  "Report a bug in the package currently being visited to its maintainer.
Prompts for bug subject TOPIC.  Leaves you in a mail buffer."
  (interactive "sBug Subject: ")
  (require 'emacsbug)
  (let ((package (lm-get-package-name))
	(addr (lm-maintainer))
	(version (lm-version)))
    (compose-mail (if addr
		      (concat (car addr) " <" (cdr addr) ">")
		    report-emacs-bug-address)
		  topic)
    (goto-char (point-max))
    (insert "\nIn " package)
    (if version
	(insert " version " version))
    (newline 2)
    (message "%s"
     (substitute-command-keys "Type \\[mail-send] to send bug report."))))

(provide 'lisp-mnt)

;;; lisp-mnt.el ends here
