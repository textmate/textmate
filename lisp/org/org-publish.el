;;; org-publish.el --- publish related org-mode files as a website
;; Copyright (C) 2006-2012 Free Software Foundation, Inc.

;; Author: David O'Toole <dto@gnu.org>
;; Maintainer: Carsten Dominik <carsten DOT dominik AT gmail DOT com>
;; Keywords: hypermedia, outlines, wp

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

;;; Commentary:

;; This program allow configurable publishing of related sets of
;; Org-mode files as a complete website.
;;
;; org-publish.el can do the following:
;;
;; + Publish all one's org-files to HTML or PDF
;; + Upload HTML, images, attachments and other files to a web server
;; + Exclude selected private pages from publishing
;; + Publish a clickable sitemap of pages
;; + Manage local timestamps for publishing only changed files
;; + Accept plugin functions to extend range of publishable content
;;
;; Documentation for publishing is in the manual.

;;; Code:


(eval-when-compile
  (require 'cl))
(require 'org)
(require 'org-exp)
(require 'format-spec)

(eval-and-compile
  (unless (fboundp 'declare-function)
    (defmacro declare-function (fn file &optional arglist fileonly))))

(defvar org-publish-initial-buffer nil
  "The buffer `org-publish' has been called from.")

(defvar org-publish-temp-files nil
  "Temporary list of files to be published.")

;; Here, so you find the variable right before it's used the first time:
(defvar org-publish-cache nil
  "This will cache timestamps and titles for files in publishing projects.
Blocks could hash sha1 values here.")

(defgroup org-publish nil
  "Options for publishing a set of Org-mode and related files."
  :tag "Org Publishing"
  :group 'org)

(defcustom org-publish-project-alist nil
  "Association list to control publishing behavior.
Each element of the alist is a publishing 'project.'  The CAR of
each element is a string, uniquely identifying the project.  The
CDR of each element is in one of the following forms:

1. A well-formed property list with an even number of elements, alternating
   keys and values, specifying parameters for the publishing process.

     (:property value :property value ... )

2. A meta-project definition, specifying of a list of sub-projects:

     (:components (\"project-1\" \"project-2\" ...))

When the CDR of an element of org-publish-project-alist is in
this second form, the elements of the list after :components are
taken to be components of the project, which group together files
requiring different publishing options.  When you publish such a
project with \\[org-publish], the components all publish.

When a property is given a value in org-publish-project-alist, its
setting overrides the value of the corresponding user variable
\(if any) during publishing.  However, options set within a file
override everything.

Most properties are optional, but some should always be set:

  :base-directory        Directory containing publishing source files
  :base-extension        Extension (without the dot!) of source files.
                         This can be a regular expression.  If not given,
                         \"org\" will be used as default extension.
  :publishing-directory  Directory (possibly remote) where output
                         files will be published

The :exclude property may be used to prevent certain files from
being published.  Its value may be a string or regexp matching
file names you don't want to be published.

The :include property may be used to include extra files.  Its
value may be a list of filenames to include. The filenames are
considered relative to the base directory.

When both :include and :exclude properties are given values, the
exclusion step happens first.

One special property controls which back-end function to use for
publishing files in the project.  This can be used to extend the
set of file types publishable by org-publish, as well as the set
of output formats.

  :publishing-function     Function to publish file.  The default is
                           `org-publish-org-to-html', but other
                           values are possible.  May also be a
                           list of functions, in which case
                           each function in the list is invoked
                           in turn.

Another property allows you to insert code that prepares a
project for publishing.  For example, you could call GNU Make on a
certain makefile, to ensure published files are built up to date.

  :preparation-function   Function to be called before publishing
                          this project.  This may also be a list
                          of functions.
  :completion-function    Function to be called after publishing
                          this project.  This may also be a list
                          of functions.

Some properties control details of the Org publishing process,
and are equivalent to the corresponding user variables listed in
the right column.  See the documentation for those variables to
learn more about their use and default values.

  :language              `org-export-default-language'
  :headline-levels       `org-export-headline-levels'
  :section-numbers       `org-export-with-section-numbers'
  :table-of-contents     `org-export-with-toc'
  :emphasize             `org-export-with-emphasize'
  :sub-superscript       `org-export-with-sub-superscripts'
  :TeX-macros            `org-export-with-TeX-macros'
  :fixed-width           `org-export-with-fixed-width'
  :tables                `org-export-with-tables'
  :table-auto-headline   `org-export-highlight-first-table-line'
  :style                 `org-export-html-style'
  :convert-org-links     `org-export-html-link-org-files-as-html'
  :inline-images         `org-export-html-inline-images'
  :expand-quoted-html    `org-export-html-expand'
  :timestamp             `org-export-html-with-timestamp'
  :publishing-directory  `org-export-publishing-directory'
  :html-preamble         `org-export-html-preamble'
  :html-postamble        `org-export-html-postamble'
  :author                `user-full-name'
  :email                 `user-mail-address'

The following properties may be used to control publishing of a
sitemap of files or summary page for a given project.

  :auto-sitemap           Whether to publish a sitemap during
                         `org-publish-current-project' or `org-publish-all'.
  :sitemap-filename      Filename for output of sitemap.  Defaults
                         to 'sitemap.org' (which becomes 'sitemap.html').
  :sitemap-title         Title of sitemap page.  Defaults to name of file.
  :sitemap-function      Plugin function to use for generation of sitemap.
                         Defaults to `org-publish-org-sitemap', which
                         generates a plain list of links to all files
                         in the project.
  :sitemap-style         Can be `list' (sitemap is just an itemized list
                         of the titles of the files involved) or
                         `tree' (the directory structure of the source
                         files is reflected in the sitemap).  Defaults to
                         `tree'.
  :sitemap-sans-extension Remove extension from sitemap's
                           filenames.  Useful to have cool
                           URIs (see
                           http://www.w3.org/Provider/Style/URI).
                           Defaults to nil.

  If you create a sitemap file, adjust the sorting like this:

  :sitemap-sort-folders    Where folders should appear in the sitemap.
                           Set this to `first' (default) or `last' to
                           display folders first or last, respectively.
                           Any other value will mix files and folders.
  :sitemap-sort-files      The site map is normally sorted alphabetically.
                           You can change this behaviour setting this to
                           `chronologically', `anti-chronologically' or nil.
  :sitemap-ignore-case     Should sorting be case-sensitive?  Default nil.

The following properties control the creation of a concept index.

  :makeindex             Create a concept index.

Other properties affecting publication.

  :body-only              Set this to 't' to publish only the body of the
                         documents, excluding everything outside and
                         including the <body> tags in HTML, or
                         \begin{document}..\end{document} in LaTeX."
  :group 'org-publish
  :type 'alist)

(defcustom org-publish-use-timestamps-flag t
  "Non-nil means use timestamp checking to publish only changed files.
When nil, do no timestamp checking and always publish all files."
  :group 'org-publish
  :type 'boolean)

(defcustom org-publish-timestamp-directory (convert-standard-filename
					    "~/.org-timestamps/")
  "Name of directory in which to store publishing timestamps."
  :group 'org-publish
  :type 'directory)

(defcustom org-publish-list-skipped-files t
  "Non-nil means show message about files *not* published."
  :group 'org-publish
  :type 'boolean)

(defcustom org-publish-before-export-hook nil
  "Hook run before export on the Org file.
The hook may modify the file in arbitrary ways before publishing happens.
The original version of the buffer will be restored after publishing."
  :group 'org-publish
  :type 'hook)

(defcustom org-publish-after-export-hook nil
  "Hook run after export on the exported buffer.
Any changes made by this hook will be saved."
  :group 'org-publish
  :type 'hook)

(defcustom org-publish-sitemap-sort-files 'alphabetically
  "How sitemaps files should be sorted by default?
Possible values are `alphabetically', `chronologically', `anti-chronologically' and nil.
If `alphabetically', files will be sorted alphabetically.
If `chronologically', files will be sorted with older modification time first.
If `anti-chronologically', files will be sorted with newer modification time first.
nil won't sort files.

You can overwrite this default per project in your
`org-publish-project-alist', using `:sitemap-sort-files'."
  :group 'org-publish
  :version "24.1"
  :type 'symbol)

(defcustom org-publish-sitemap-sort-folders 'first
  "A symbol, denoting if folders are sorted first in sitemaps.
Possible values are `first', `last', and nil.
If `first', folders will be sorted before files.
If `last', folders are sorted to the end after the files.
Any other value will not mix files and folders.

You can overwrite this default per project in your
`org-publish-project-alist', using `:sitemap-sort-folders'."
  :group 'org-publish
  :version "24.1"
  :type 'symbol)

(defcustom org-publish-sitemap-sort-ignore-case nil
  "Sort sitemaps case insensitively by default?

You can overwrite this default per project in your
`org-publish-project-alist', using `:sitemap-ignore-case'."
  :group 'org-publish
  :version "24.1"
  :type 'boolean)

(defcustom org-publish-sitemap-date-format "%Y-%m-%d"
  "Format for `format-time-string' which is used to print a date
in the sitemap."
  :group 'org-publish
  :version "24.1"
  :type 'string)

(defcustom org-publish-sitemap-file-entry-format "%t"
  "How a sitemap file entry is formatted.
You could use brackets to delimit on what part the link will be.

%t is the title.
%a is the author.
%d is the date formatted using `org-publish-sitemap-date-format'."
  :group 'org-publish
  :version "24.1"
  :type 'string)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sanitize-plist (FIXME why?)

(defun org-publish-sanitize-plist (plist)
  ;; FIXME document
  (mapcar (lambda (x)
	    (or (cdr (assq x '((:index-filename . :sitemap-filename)
			       (:index-title . :sitemap-title)
			       (:index-function . :sitemap-function)
			       (:index-style . :sitemap-style)
			       (:auto-index . :auto-sitemap))))
		x))
	  plist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Timestamp-related functions

(defun org-publish-timestamp-filename (filename &optional pub-dir pub-func)
  "Return path to timestamp file for filename FILENAME."
  (setq filename (concat filename "::" (or pub-dir "") "::"
			 (format "%s" (or pub-func ""))))
  (concat "X" (if (fboundp 'sha1) (sha1 filename) (md5 filename))))

(defun org-publish-needed-p (filename &optional pub-dir pub-func true-pub-dir)
  "Return t if FILENAME should be published in PUB-DIR using PUB-FUNC.
TRUE-PUB-DIR is where the file will truly end up.  Currently we are not using
this - maybe it can eventually be used to check if the file is present at
the target location, and how old it is.  Right now we cannot do this, because
we do not know under what file name the file will be stored - the publishing
function can still decide about that independently."
  (let ((rtn
	 (if org-publish-use-timestamps-flag
	     (org-publish-cache-file-needs-publishing
	      filename pub-dir pub-func)
	   ;; don't use timestamps, always return t
	   t)))
    (if rtn
	(message "Publishing file %s using `%s'" filename pub-func)
      (when org-publish-list-skipped-files
	(message   "Skipping unmodified file %s" filename)))
    rtn))

(defun org-publish-update-timestamp (filename &optional pub-dir pub-func)
  "Update publishing timestamp for file FILENAME.
If there is no timestamp, create one."
  (let ((key (org-publish-timestamp-filename filename pub-dir pub-func))
	(stamp (org-publish-cache-ctime-of-src filename)))
    (org-publish-cache-set key stamp)))

(defun org-publish-remove-all-timestamps ()
  "Remove all files in the timestamp directory."
  (let ((dir org-publish-timestamp-directory)
	files)
    (when (and (file-exists-p dir)
	       (file-directory-p dir))
      (mapc 'delete-file (directory-files dir 'full "[^.]\\'"))
      (org-publish-reset-cache))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compatibility aliases

;; Delete-dups is not in Emacs <22
(if (fboundp 'delete-dups)
    (defalias 'org-publish-delete-dups 'delete-dups)
  (defun org-publish-delete-dups (list)
    "Destructively remove `equal' duplicates from LIST.
Store the result in LIST and return it.  LIST must be a proper list.
Of several `equal' occurrences of an element in LIST, the first
one is kept.

This is a compatibility function for Emacsen without `delete-dups'."
    ;; Code from `subr.el' in Emacs 22:
    (let ((tail list))
      (while tail
	(setcdr tail (delete (car tail) (cdr tail)))
	(setq tail (cdr tail))))
    list))

(declare-function org-publish-delete-dups "org-publish" (list))
(declare-function find-lisp-find-files "find-lisp" (directory regexp))
(declare-function org-pop-to-buffer-same-window
		  "org-compat" (&optional buffer-or-name norecord label))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Getting project information out of org-publish-project-alist

(defun org-publish-expand-projects (projects-alist)
  "Expand projects in PROJECTS-ALIST.
This splices all the components into the list."
  (let ((rest projects-alist) rtn p components)
    (while (setq p (pop rest))
      (if (setq components (plist-get (cdr p) :components))
	  (setq rest (append
		      (mapcar (lambda (x) (assoc x org-publish-project-alist))
			      components)
		      rest))
	(push p rtn)))
    (nreverse (org-publish-delete-dups (delq nil rtn)))))

(defvar org-sitemap-sort-files)
(defvar org-sitemap-sort-folders)
(defvar org-sitemap-ignore-case)
(defvar org-sitemap-requested)
(defvar org-sitemap-date-format)
(defvar org-sitemap-file-entry-format)
(defun org-publish-compare-directory-files (a b)
  "Predicate for `sort', that sorts folders and files for sitemap."
  (let ((retval t))
    (when (or org-sitemap-sort-files org-sitemap-sort-folders)
      ;; First we sort files:
      (when org-sitemap-sort-files
	(cond ((equal org-sitemap-sort-files 'alphabetically)
	       (let* ((adir (file-directory-p a))
		      (aorg (and (string-match "\\.org$" a) (not adir)))
		      (bdir (file-directory-p b))
		      (borg (and (string-match "\\.org$" b) (not bdir)))
		      (A (if aorg
			     (concat (file-name-directory a)
				     (org-publish-find-title a)) a))
		      (B (if borg
			     (concat (file-name-directory b)
				     (org-publish-find-title b)) b)))
		 (setq retval (if org-sitemap-ignore-case
				  (not (string-lessp (upcase B) (upcase A)))
				(not (string-lessp B A))))))
		((or (equal org-sitemap-sort-files 'chronologically)
		     (equal org-sitemap-sort-files 'anti-chronologically))
		 (let* ((adate (org-publish-find-date a))
			(bdate (org-publish-find-date b))
			(A (+ (lsh (car adate) 16) (cadr adate)))
			(B (+ (lsh (car bdate) 16) (cadr bdate))))
		   (setq retval (if (equal org-sitemap-sort-files 'chronologically)
				    (<= A B)
				  (>= A B)))))))
      ;; Directory-wise wins:
      (when org-sitemap-sort-folders
        ;; a is directory, b not:
        (cond
         ((and (file-directory-p a) (not (file-directory-p b)))
          (setq retval (equal org-sitemap-sort-folders 'first)))
          ;; a is not a directory, but b is:
         ((and (not (file-directory-p a)) (file-directory-p b))
          (setq retval (equal org-sitemap-sort-folders 'last))))))
    retval))

(defun org-publish-get-base-files-1 (base-dir &optional recurse match skip-file skip-dir)
  "Set `org-publish-temp-files' with files from BASE-DIR directory.
If RECURSE is non-nil, check BASE-DIR recursively.  If MATCH is
non-nil, restrict this list to the files matching the regexp
MATCH.  If SKIP-FILE is non-nil, skip file matching the regexp
SKIP-FILE.  If SKIP-DIR is non-nil, don't check directories
matching the regexp SKIP-DIR when recursing through BASE-DIR."
  (mapc (lambda (f)
	  (let ((fd-p (file-directory-p f))
		(fnd (file-name-nondirectory f)))
	    (if (and fd-p recurse
		     (not (string-match "^\\.+$" fnd))
		     (if skip-dir (not (string-match skip-dir fnd)) t))
		(org-publish-get-base-files-1 f recurse match skip-file skip-dir)
	      (unless (or fd-p ;; this is a directory
			  (and skip-file (string-match skip-file fnd))
			  (not (file-exists-p (file-truename f)))
			  (not (string-match match fnd)))

		(pushnew f org-publish-temp-files)))))
	(if org-sitemap-requested
	    (sort (directory-files base-dir t (unless recurse match))
		  'org-publish-compare-directory-files)
	  (directory-files base-dir t (unless recurse match)))))

(defun org-publish-get-base-files (project &optional exclude-regexp)
  "Return a list of all files in PROJECT.
If EXCLUDE-REGEXP is set, this will be used to filter out
matching filenames."
  (let* ((project-plist (cdr project))
	 (base-dir (file-name-as-directory
		    (plist-get project-plist :base-directory)))
	 (include-list (plist-get project-plist :include))
	 (recurse (plist-get project-plist :recursive))
	 (extension (or (plist-get project-plist :base-extension) "org"))
	 ;; sitemap-... variables are dynamically scoped for
	 ;; org-publish-compare-directory-files:
	 (org-sitemap-requested
	  (plist-get project-plist :auto-sitemap))
	 (sitemap-filename
	  (or (plist-get project-plist :sitemap-filename)
	      "sitemap.org"))
	 (org-sitemap-sort-folders
	  (if (plist-member project-plist :sitemap-sort-folders)
	      (plist-get project-plist :sitemap-sort-folders)
	    org-publish-sitemap-sort-folders))
	 (org-sitemap-sort-files
	  (cond ((plist-member project-plist :sitemap-sort-files)
		 (plist-get project-plist :sitemap-sort-files))
		;; For backward compatibility:
		((plist-member project-plist :sitemap-alphabetically)
		 (if (plist-get project-plist :sitemap-alphabetically)
		     'alphabetically nil))
		(t org-publish-sitemap-sort-files)))
	 (org-sitemap-ignore-case
	  (if (plist-member project-plist :sitemap-ignore-case)
	      (plist-get project-plist :sitemap-ignore-case)
	    org-publish-sitemap-sort-ignore-case))
	 (match (if (eq extension 'any)
                    "^[^\\.]"
		  (concat "^[^\\.].*\\.\\(" extension "\\)$"))))
    ;; Make sure `org-sitemap-sort-folders' has an accepted value
    (unless (memq org-sitemap-sort-folders '(first last))
      (setq org-sitemap-sort-folders nil))

    (setq org-publish-temp-files nil)
    (if org-sitemap-requested
	(pushnew (expand-file-name (concat base-dir sitemap-filename))
		  org-publish-temp-files))
    (org-publish-get-base-files-1 base-dir recurse match
				  ;; FIXME distinguish exclude regexp
				  ;; for skip-file and skip-dir?
				  exclude-regexp exclude-regexp)
    (mapc (lambda (f)
	    (pushnew
	     (expand-file-name (concat base-dir f))
	     org-publish-temp-files))
	  include-list)
    org-publish-temp-files))

(defun org-publish-get-project-from-filename (filename &optional up)
  "Return the project that FILENAME belongs to."
  (let* ((filename (expand-file-name filename))
	 project-name)

    (catch 'p-found
      (dolist (prj org-publish-project-alist)
	(unless (plist-get (cdr prj) :components)
	  ;; [[info:org:Selecting%20files]] shows how this is supposed to work:
	  (let* ((r (plist-get (cdr prj) :recursive))
		 (b (expand-file-name (file-name-as-directory
				       (plist-get (cdr prj) :base-directory))))
		 (x (or (plist-get (cdr prj) :base-extension) "org"))
		 (e (plist-get (cdr prj) :exclude))
		 (i (plist-get (cdr prj) :include))
		 (xm (concat "^" b (if r ".+" "[^/]+") "\\.\\(" x "\\)$")))
	    (when
		(or
		   (and
		  i (member filename
			    (mapcar
			     (lambda (file) (expand-file-name file b))
			     i)))
		   (and
		    (not (and e (string-match e filename)))
		    (string-match xm filename)))
	      (setq project-name (car prj))
	      (throw 'p-found project-name))))))
    (when up
      (dolist (prj org-publish-project-alist)
	(if (member project-name (plist-get (cdr prj) :components))
	    (setq project-name (car prj)))))
    (assoc project-name org-publish-project-alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pluggable publishing back-end functions

(defun org-publish-org-to (format plist filename pub-dir)
  "Publish an org file to FORMAT.
PLIST is the property list for the given project.
FILENAME is the filename of the org file to be published.
PUB-DIR is the publishing directory."
  (require 'org)
  (unless (file-exists-p pub-dir)
    (make-directory pub-dir t))
  (let ((visiting (find-buffer-visiting filename)))
    (save-excursion
      (org-pop-to-buffer-same-window (or visiting (find-file filename)))
      (let* ((plist (cons :buffer-will-be-killed (cons t plist)))
	     (init-buf (current-buffer))
	     (init-point (point))
	     (init-buf-string (buffer-string))
	     export-buf-or-file)
	;; run hooks before exporting
	(run-hooks 'org-publish-before-export-hook)
	;; export the possibly modified buffer
	(setq export-buf-or-file
	      (funcall (intern (concat "org-export-as-" format))
		       (plist-get plist :headline-levels)
		       nil plist nil
		       (plist-get plist :body-only)
		       pub-dir))
	(when (and (bufferp export-buf-or-file)
		   (buffer-live-p export-buf-or-file))
	  (set-buffer export-buf-or-file)
	  ;; run hooks after export and save export
	  (progn (run-hooks 'org-publish-after-export-hook)
		 (if (buffer-modified-p) (save-buffer)))
	  (kill-buffer export-buf-or-file))
	;; maybe restore buffer's content
	(set-buffer init-buf)
	(when (buffer-modified-p init-buf)
	  (erase-buffer)
	  (insert init-buf-string)
	  (save-buffer)
	  (goto-char init-point))
	(unless visiting
	  (kill-buffer init-buf))))))

(defmacro org-publish-with-aux-preprocess-maybe (&rest body)
  "Execute BODY with a modified hook to preprocess for index."
  `(let ((org-export-preprocess-after-headline-targets-hook
	 (if (plist-get project-plist :makeindex)
	     (cons 'org-publish-aux-preprocess
		   org-export-preprocess-after-headline-targets-hook)
	   org-export-preprocess-after-headline-targets-hook)))
     ,@body))
(def-edebug-spec org-publish-with-aux-preprocess-maybe (body))

(defvar project-plist)
(defun org-publish-org-to-latex (plist filename pub-dir)
  "Publish an org file to LaTeX.
See `org-publish-org-to' to the list of arguments."
  (org-publish-with-aux-preprocess-maybe
   (org-publish-org-to "latex" plist filename pub-dir)))

(defun org-publish-org-to-pdf (plist filename pub-dir)
  "Publish an org file to PDF (via LaTeX).
See `org-publish-org-to' to the list of arguments."
  (org-publish-with-aux-preprocess-maybe
   (org-publish-org-to "pdf" plist filename pub-dir)))

(defun org-publish-org-to-html (plist filename pub-dir)
  "Publish an org file to HTML.
See `org-publish-org-to' to the list of arguments."
  (org-publish-with-aux-preprocess-maybe
    (org-publish-org-to "html" plist filename pub-dir)))

(defun org-publish-org-to-org (plist filename pub-dir)
  "Publish an org file to HTML.
See `org-publish-org-to' to the list of arguments."
  (org-publish-org-to "org" plist filename pub-dir))

(defun org-publish-org-to-ascii (plist filename pub-dir)
  "Publish an org file to ASCII.
See `org-publish-org-to' to the list of arguments."
  (org-publish-with-aux-preprocess-maybe
    (org-publish-org-to "ascii" plist filename pub-dir)))

(defun org-publish-org-to-latin1 (plist filename pub-dir)
  "Publish an org file to Latin-1.
See `org-publish-org-to' to the list of arguments."
  (org-publish-with-aux-preprocess-maybe
    (org-publish-org-to "latin1" plist filename pub-dir)))

(defun org-publish-org-to-utf8 (plist filename pub-dir)
  "Publish an org file to UTF-8.
See `org-publish-org-to' to the list of arguments."
  (org-publish-with-aux-preprocess-maybe
    (org-publish-org-to "utf8" plist filename pub-dir)))

(defun org-publish-attachment (plist filename pub-dir)
  "Publish a file with no transformation of any kind.
See `org-publish-org-to' to the list of arguments."
  ;; make sure eshell/cp code is loaded
  (unless (file-directory-p pub-dir)
    (make-directory pub-dir t))
  (or (equal (expand-file-name (file-name-directory filename))
	     (file-name-as-directory (expand-file-name pub-dir)))
      (copy-file filename
		 (expand-file-name (file-name-nondirectory filename) pub-dir)
		 t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Publishing files, sets of files, and indices

(defun org-publish-file (filename &optional project no-cache)
  "Publish file FILENAME from PROJECT.
If NO-CACHE is not nil, do not initialize org-publish-cache and
write it to disk.  This is needed, since this function is used to
publish single files, when entire projects are published.
See `org-publish-projects'."
  (let* ((project
	  (or project
	      (or (org-publish-get-project-from-filename filename)
		  (error "File %s not part of any known project"
			 (abbreviate-file-name filename)))))
	 (project-plist (cdr project))
	 (ftname (expand-file-name filename))
	 (publishing-function
	  (or (plist-get project-plist :publishing-function)
	      'org-publish-org-to-html))
	 (base-dir
	  (file-name-as-directory
	   (expand-file-name
	    (or (plist-get project-plist :base-directory)
		(error "Project %s does not have :base-directory defined"
		       (car project))))))
	 (pub-dir
	  (file-name-as-directory
	   (file-truename
	    (or (eval (plist-get project-plist :publishing-directory))
		(error "Project %s does not have :publishing-directory defined"
		       (car project))))))
	 tmp-pub-dir)

    (unless no-cache
      (org-publish-initialize-cache (car project)))

    (setq tmp-pub-dir
	  (file-name-directory
	   (concat pub-dir
		   (and (string-match (regexp-quote base-dir) ftname)
			(substring ftname (match-end 0))))))
    (if (listp publishing-function)
	;; allow chain of publishing functions
	(mapc (lambda (f)
		(when (org-publish-needed-p filename pub-dir f tmp-pub-dir)
		  (funcall f project-plist filename tmp-pub-dir)
		  (org-publish-update-timestamp filename pub-dir f)))
	      publishing-function)
      (when (org-publish-needed-p filename pub-dir publishing-function
				  tmp-pub-dir)
	(funcall publishing-function project-plist filename tmp-pub-dir)
	(org-publish-update-timestamp
	 filename pub-dir publishing-function)))
    (unless no-cache (org-publish-write-cache-file))))

(defun org-publish-projects (projects)
  "Publish all files belonging to the PROJECTS alist.
If :auto-sitemap is set, publish the sitemap too.
If :makeindex is set, also produce a file theindex.org."
  (mapc
   (lambda (project)
     ;; Each project uses its own cache file:
     (org-publish-initialize-cache (car project))
     (let*
	 ((project-plist (cdr project))
	  (exclude-regexp (plist-get project-plist :exclude))
	  (sitemap-p (plist-get project-plist :auto-sitemap))
	  (sitemap-filename (or (plist-get project-plist :sitemap-filename)
				"sitemap.org"))
	  (sitemap-function (or (plist-get project-plist :sitemap-function)
				'org-publish-org-sitemap))
	  (org-sitemap-date-format (or (plist-get project-plist :sitemap-date-format)
				   org-publish-sitemap-date-format))
	  (org-sitemap-file-entry-format (or (plist-get project-plist :sitemap-file-entry-format)
					 org-publish-sitemap-file-entry-format))
	  (preparation-function (plist-get project-plist :preparation-function))
	  (completion-function (plist-get project-plist :completion-function))
	  (files (org-publish-get-base-files project exclude-regexp)) file)
       (when preparation-function (run-hooks 'preparation-function))
       (if sitemap-p (funcall sitemap-function project sitemap-filename))
       (while (setq file (pop files))
	 (org-publish-file file project t))
       (when (plist-get project-plist :makeindex)
	 (org-publish-index-generate-theindex
	  (plist-get project-plist :base-directory))
	 (org-publish-file (expand-file-name
			    "theindex.org"
			    (plist-get project-plist :base-directory))
			   project t))
       (when completion-function (run-hooks 'completion-function))
     (org-publish-write-cache-file)))
   (org-publish-expand-projects projects)))

(defun org-publish-org-sitemap (project &optional sitemap-filename)
  "Create a sitemap of pages in set defined by PROJECT.
Optionally set the filename of the sitemap with SITEMAP-FILENAME.
Default for SITEMAP-FILENAME is 'sitemap.org'."
  (let* ((project-plist (cdr project))
	 (dir (file-name-as-directory
	       (plist-get project-plist :base-directory)))
	 (localdir (file-name-directory dir))
	 (indent-str (make-string 2 ?\ ))
	 (exclude-regexp (plist-get project-plist :exclude))
	 (files (nreverse (org-publish-get-base-files project exclude-regexp)))
	 (sitemap-filename (concat dir (or sitemap-filename "sitemap.org")))
	 (sitemap-title (or (plist-get project-plist :sitemap-title)
			  (concat "Sitemap for project " (car project))))
	 (sitemap-style (or (plist-get project-plist :sitemap-style)
			  'tree))
	 (sitemap-sans-extension (plist-get project-plist :sitemap-sans-extension))
	 (visiting (find-buffer-visiting sitemap-filename))
	 (ifn (file-name-nondirectory sitemap-filename))
	 file sitemap-buffer)
    (with-current-buffer (setq sitemap-buffer
			       (or visiting (find-file sitemap-filename)))
      (erase-buffer)
      (insert (concat "#+TITLE: " sitemap-title "\n\n"))
      (while (setq file (pop files))
	(let ((fn (file-name-nondirectory file))
	      (link (file-relative-name file dir))
	      (oldlocal localdir))
	  (when sitemap-sans-extension
	    (setq link (file-name-sans-extension link)))
	  ;; sitemap shouldn't list itself
	  (unless (equal (file-truename sitemap-filename)
			 (file-truename file))
	    (if (eq sitemap-style 'list)
		(message "Generating list-style sitemap for %s" sitemap-title)
	      (message "Generating tree-style sitemap for %s" sitemap-title)
	      (setq localdir (concat (file-name-as-directory dir)
				     (file-name-directory link)))
	      (unless (string= localdir oldlocal)
		(if (string= localdir dir)
		    (setq indent-str (make-string 2 ?\ ))
		  (let ((subdirs
			 (split-string
			  (directory-file-name
			   (file-name-directory
			    (file-relative-name localdir dir))) "/"))
			(subdir "")
			(old-subdirs (split-string
				      (file-relative-name oldlocal dir) "/")))
		    (setq indent-str (make-string 2 ?\ ))
		    (while (string= (car old-subdirs) (car subdirs))
		      (setq indent-str (concat indent-str (make-string 2 ?\ )))
		      (pop old-subdirs)
		      (pop subdirs))
		    (dolist (d subdirs)
		      (setq subdir (concat subdir d "/"))
		      (insert (concat indent-str " + " d "\n"))
		      (setq indent-str (make-string
					(+ (length indent-str) 2) ?\ )))))))
	    ;; This is common to 'flat and 'tree
	    (let ((entry
		   (org-publish-format-file-entry org-sitemap-file-entry-format
						  file project-plist))
		  (regexp "\\(.*\\)\\[\\([^][]+\\)\\]\\(.*\\)"))
	      (cond ((string-match-p regexp entry)
		     (string-match regexp entry)
		     (insert (concat indent-str " + " (match-string 1 entry)
				     "[[file:" link "]["
				     (match-string 2 entry)
				     "]]" (match-string 3 entry) "\n")))
		    (t
		     (insert (concat indent-str " + [[file:" link "]["
				     entry
				     "]]\n"))))))))
      (save-buffer))
    (or visiting (kill-buffer sitemap-buffer))))

(defun org-publish-format-file-entry (fmt file project-plist)
  (format-spec fmt
	     `((?t . ,(org-publish-find-title file t))
	       (?d . ,(format-time-string org-sitemap-date-format
					  (org-publish-find-date file)))
	       (?a . ,(or (plist-get project-plist :author) user-full-name)))))

(defun org-publish-find-title (file &optional reset)
  "Find the title of FILE in project."
  (or
   (and (not reset) (org-publish-cache-get-file-property file :title nil t))
   (let* ((visiting (find-buffer-visiting file))
	  (buffer (or visiting (find-file-noselect file)))
	  title)
     (with-current-buffer buffer
       (let* ((opt-plist (org-combine-plists (org-default-export-plist)
					     (org-infile-export-plist))))
	 (setq title
	       (or (plist-get opt-plist :title)
		   (and (not
			 (plist-get opt-plist :skip-before-1st-heading))
			(org-export-grab-title-from-buffer))
		   (file-name-nondirectory (file-name-sans-extension file))))))
     (unless visiting
       (kill-buffer buffer))
     (org-publish-cache-set-file-property file :title title)
     title)))

(defun org-publish-find-date (file)
  "Find the date of FILE in project.
If FILE provides a #+date keyword use it else use the file
system's modification time.

It returns time in `current-time' format."
  (let ((visiting (find-buffer-visiting file)))
    (save-excursion
      (org-pop-to-buffer-same-window (or visiting (find-file-noselect file nil t)))
      (let* ((plist (org-infile-export-plist))
	     (date (plist-get plist :date)))
	(unless visiting
	  (kill-buffer (current-buffer)))
	(if date
	    (org-time-string-to-time date)
	  (when (file-exists-p file)
	    (nth 5 (file-attributes file))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interactive publishing functions

;;;###autoload
(defalias 'org-publish-project 'org-publish)

;;;###autoload
(defun org-publish (project &optional force)
  "Publish PROJECT."
  (interactive
   (list
    (assoc (org-icompleting-read
	    "Publish project: "
	    org-publish-project-alist nil t)
	   org-publish-project-alist)
    current-prefix-arg))
  (setq org-publish-initial-buffer (current-buffer))
  (save-window-excursion
    (let* ((org-publish-use-timestamps-flag
	    (if force nil org-publish-use-timestamps-flag)))
      (org-publish-projects
       (if (stringp project)
	   ;; If this function is called in batch mode,
	   ;; project is still a string here.
	   (list (assoc project org-publish-project-alist))
	   (list project))))))

;;;###autoload
(defun org-publish-all (&optional force)
  "Publish all projects.
With prefix argument, remove all files in the timestamp
directory and force publishing all files."
  (interactive "P")
  (when force
    (org-publish-remove-all-timestamps))
  (save-window-excursion
    (let ((org-publish-use-timestamps-flag
	   (if force nil org-publish-use-timestamps-flag)))
      (org-publish-projects org-publish-project-alist))))


;;;###autoload
(defun org-publish-current-file (&optional force)
  "Publish the current file.
With prefix argument, force publish the file."
  (interactive "P")
  (save-window-excursion
    (let ((org-publish-use-timestamps-flag
	   (if force nil org-publish-use-timestamps-flag)))
      (org-publish-file (buffer-file-name)))))

;;;###autoload
(defun org-publish-current-project (&optional force)
  "Publish the project associated with the current file.
With a prefix argument, force publishing of all files in
the project."
  (interactive "P")
  (save-window-excursion
    (let ((project (org-publish-get-project-from-filename (buffer-file-name) 'up))
	  (org-publish-use-timestamps-flag
	   (if force nil org-publish-use-timestamps-flag)))
      (if (not project)
	  (error "File %s is not part of any known project" (buffer-file-name)))
      ;; FIXME: force is not used here?
      (org-publish project))))


;;; Index generation

(defun org-publish-aux-preprocess ()
  "Find index entries and write them to an .orgx file."
  (let ((case-fold-search t)
	entry index target)
    (goto-char (point-min))
    (while
	(and
	 (re-search-forward "^[ \t]*#\\+index:[ \t]*\\(.*?\\)[ \t]*$" nil t)
	 (> (match-end 1) (match-beginning 1)))
      (setq entry (match-string 1))
      (when (eq org-export-current-backend 'latex)
	(replace-match (format "\\index{%s}" entry) t t))
      (save-excursion
	(ignore-errors (org-back-to-heading t))
	(setq target (get-text-property (point) 'target))
	(setq target (or (cdr (assoc target org-export-preferred-target-alist))
			 (cdr (assoc target org-export-id-target-alist))
			 target ""))
	(push (cons entry target) index)))
    (with-temp-file
	(concat
	 (file-name-directory org-current-export-file) "."
	 (file-name-sans-extension
	  (file-name-nondirectory org-current-export-file)) ".orgx")
      (dolist (entry (nreverse index))
	(insert (format "INDEX: (%s) %s\n" (cdr entry) (car entry)))))))

(defun org-publish-index-generate-theindex (directory)
  "Generate the index from all .orgx files in DIRECTORY."
  (require 'find-lisp)
  (let* ((fulldir (file-name-as-directory
		   (expand-file-name directory)))
	 (full-files (find-lisp-find-files directory "\\.orgx\\'"))
	 (re (concat "\\`" fulldir))
	 (files (mapcar (lambda (f) (if (string-match re f)
					(substring f (match-end 0))
				      f))
			full-files))
	 (default-directory directory)
	 index origfile buf target entry ibuffer
	 main last-main letter last-letter file sub link tgext)
    ;; `files' contains the list of relative file names
    (dolist (file files)
      (setq origfile
	    (concat (file-name-directory file)
		    (substring (file-name-nondirectory file) 1 -1)))
      (setq buf (find-file-noselect file))
      (with-current-buffer buf
	(goto-char (point-min))
	(while (re-search-forward "^INDEX: (\\(.*?\\)) \\(.*\\)" nil t)
	  (setq target (match-string 1)
		entry (match-string 2))
	  (push (list entry origfile target) index)))
      (kill-buffer buf))
    (setq index (sort index (lambda (a b) (string< (downcase (car a))
						   (downcase (car b))))))
    (setq ibuffer (find-file-noselect (expand-file-name "theindex.inc" directory)))
    (with-current-buffer ibuffer
      (erase-buffer)
      (insert "* Index\n")
      (setq last-letter nil)
      (dolist (idx index)
	(setq entry (car idx) file (nth 1 idx) target (nth 2 idx))
	(if (and (stringp target) (string-match "\\S-" target))
	    (setq tgext (concat "::#" target))
	  (setq tgext ""))
	(setq letter (upcase (substring entry 0 1)))
	(when (not (equal letter last-letter))
	  (insert "** " letter "\n")
	  (setq last-letter letter))
	(if (string-match "!" entry)
	    (setq main (substring entry 0 (match-beginning 0))
		  sub (substring entry (match-end 0)))
	  (setq main nil sub nil last-main nil))
	(when (and main (not (equal main last-main)))
	  (insert "   - " main "\n")
	  (setq last-main main))
	(setq link (concat "[[file:" file tgext "]"
			   "[" (or sub entry) "]]"))
	(if (and main sub)
	    (insert "     - " link "\n")
	  (insert "   - " link "\n")))
      (save-buffer))
    (kill-buffer ibuffer)
    ;; Create theindex.org if it doesn't exist already
    (let ((index-file (expand-file-name "theindex.org" directory)))
      (unless (file-exists-p index-file)
       (setq ibuffer (find-file-noselect index-file))
       (with-current-buffer ibuffer
         (erase-buffer)
         (insert "\n\n#+include: \"theindex.inc\"\n\n")
         (save-buffer))
       (kill-buffer ibuffer)))))

;; Caching functions:

(defun org-publish-write-cache-file (&optional free-cache)
  "Write `org-publish-cache' to file.
If FREE-CACHE, empty the cache."
  (unless org-publish-cache
    (error "%s" "`org-publish-write-cache-file' called, but no cache present"))

  (let ((cache-file (org-publish-cache-get ":cache-file:")))
    (unless cache-file
      (error
       "%s" "Cannot find cache-file name in `org-publish-write-cache-file'"))
    (with-temp-file cache-file
      (let ((print-level nil)
	    (print-length nil))
	(insert "(setq org-publish-cache (make-hash-table :test 'equal :weakness nil :size 100))\n")
	(maphash (lambda (k v)
		   (insert
		    (format (concat "(puthash %S "
				    (if (or (listp v) (symbolp v))
					"'" "")
				    "%S org-publish-cache)\n") k v)))
		 org-publish-cache)))
    (when free-cache (org-publish-reset-cache))))

(defun org-publish-initialize-cache (project-name)
  "Initialize the projects cache if not initialized yet and return it."

  (unless project-name
    (error "%s%s" "Cannot initialize `org-publish-cache' without projects name"
	   " in `org-publish-initialize-cache'"))

  (unless (file-exists-p org-publish-timestamp-directory)
    (make-directory org-publish-timestamp-directory t))
  (if (not (file-directory-p org-publish-timestamp-directory))
      (error "Org publish timestamp: %s is not a directory"
	     org-publish-timestamp-directory))

  (unless (and org-publish-cache
	       (string= (org-publish-cache-get ":project:") project-name))
    (let* ((cache-file (concat
			(expand-file-name org-publish-timestamp-directory)
			project-name
			".cache"))
	   (cexists (file-exists-p cache-file)))

      (when org-publish-cache
	(org-publish-reset-cache))

      (if cexists
	  (load-file cache-file)
	(setq org-publish-cache
	      (make-hash-table :test 'equal :weakness nil :size 100))
	(org-publish-cache-set ":project:" project-name)
	(org-publish-cache-set ":cache-file:" cache-file))
      (unless cexists (org-publish-write-cache-file nil))))
  org-publish-cache)

(defun org-publish-reset-cache ()
  "Empty org-publish-cache and reset it nil."
  (message "%s" "Resetting org-publish-cache")
  (if (hash-table-p org-publish-cache)
      (clrhash org-publish-cache))
  (setq org-publish-cache nil))

(defun org-publish-cache-file-needs-publishing (filename &optional pub-dir pub-func)
  "Check the timestamp of the last publishing of FILENAME.
Return `t', if the file needs publishing.  The function also
checks if any included files have been more recently published,
so that the file including them will be republished as well."
  (unless org-publish-cache
    (error "%s" "`org-publish-cache-file-needs-publishing' called, but no cache present"))
  (let* ((key (org-publish-timestamp-filename filename pub-dir pub-func))
	 (pstamp (org-publish-cache-get key))
	 (visiting (find-buffer-visiting filename))
	 included-files-ctime buf)

    (when (equal (file-name-extension filename) "org")
      (setq buf (find-file (expand-file-name filename)))
      (with-current-buffer buf
	(goto-char (point-min))
	(while (re-search-forward "^#\\+INCLUDE:[ \t]+\"?\\([^ \t\n\r\"]*\\)\"?[ \t]*.*$" nil t)
	  (let* ((included-file (expand-file-name (match-string 1))))
	    (add-to-list 'included-files-ctime
			 (org-publish-cache-ctime-of-src included-file) t))))
      ;; FIXME don't kill current buffer
      (unless visiting (kill-buffer buf)))
    (if (null pstamp)
	t
      (let ((ctime (org-publish-cache-ctime-of-src filename)))
	(or (< pstamp ctime)
	    (when included-files-ctime
	      (not (null (delq nil (mapcar (lambda(ct) (< ctime ct))
					   included-files-ctime))))))))))

(defun org-publish-cache-set-file-property (filename property value &optional project-name)
  "Set the VALUE for a PROPERTY of file FILENAME in publishing cache to VALUE.
Use cache file of PROJECT-NAME.  If the entry does not exist, it will be
created.  Return VALUE."
  ;; Evtl. load the requested cache file:
  (if project-name (org-publish-initialize-cache project-name))
  (let ((pl (org-publish-cache-get filename)))
    (if pl
	(progn
	  (plist-put pl property value)
	  value)
      (org-publish-cache-get-file-property
       filename property value nil project-name))))

(defun org-publish-cache-get-file-property
  (filename property &optional default no-create project-name)
  "Return the value for a PROPERTY of file FILENAME in publishing cache.
Use cache file of PROJECT-NAME. Return the value of that PROPERTY or
DEFAULT, if the value does not yet exist.
If the entry will be created, unless NO-CREATE is not nil."
  ;; Evtl. load the requested cache file:
  (if project-name (org-publish-initialize-cache project-name))
  (let ((pl (org-publish-cache-get filename))
	(retval nil))
    (if pl
	(if (plist-member pl property)
	    (setq retval (plist-get pl property))
	  (setq retval default))
      ;; no pl yet:
      (unless no-create
	(org-publish-cache-set filename (list property default)))
      (setq retval default))
    retval))

(defun org-publish-cache-get (key)
  "Return the value stored in `org-publish-cache' for key KEY.
Returns nil, if no value or nil is found, or the cache does not
exist."
  (unless org-publish-cache
    (error "%s" "`org-publish-cache-get' called, but no cache present"))
  (gethash key org-publish-cache))

(defun org-publish-cache-set (key value)
  "Store KEY VALUE pair in `org-publish-cache'.
Returns value on success, else nil."
  (unless org-publish-cache
    (error "%s" "`org-publish-cache-set' called, but no cache present"))
  (puthash key value org-publish-cache))

(defun org-publish-cache-ctime-of-src (filename)
  "Get the FILENAME ctime as an integer."
  (let* ((symlink-maybe (or (file-symlink-p filename) filename))
	 (src-attr (file-attributes (if (file-name-absolute-p symlink-maybe)
					symlink-maybe
				      (expand-file-name
				       symlink-maybe
				       (file-name-directory filename))))))
    (+
     (lsh (car (nth 5 src-attr)) 16)
     (cadr (nth 5 src-attr)))))

(provide 'org-publish)

;;; org-publish.el ends here
