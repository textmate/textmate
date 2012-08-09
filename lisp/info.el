;; info.el --- info package for Emacs

;; Copyright (C) 1985-1986, 1992-2012  Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: help

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

;; Note that nowadays we expect Info files to be made using makeinfo.
;; In particular we make these assumptions:
;;  - a menu item MAY contain colons but not colon-space ": "
;;  - a menu item ending with ": " (but not ":: ") is an index entry
;;  - a node name MAY NOT contain a colon
;; This distinction is to support indexing of computer programming
;; language terms that may contain ":" but not ": ".

;;; Code:

(eval-when-compile (require 'cl))

(defgroup info nil
  "Info subsystem."
  :group 'help
  :group 'docs)


(defvar Info-history nil
  "Stack of Info nodes user has visited.
Each element of the stack is a list (FILENAME NODENAME BUFFERPOS).")

(defvar Info-history-forward nil
  "Stack of Info nodes user has visited with `Info-history-back' command.
Each element of the stack is a list (FILENAME NODENAME BUFFERPOS).")

(defvar Info-history-list nil
  "List of all Info nodes user has visited.
Each element of the list is a list (FILENAME NODENAME).")

(defcustom Info-history-skip-intermediate-nodes t
  "Non-nil means don't record intermediate Info nodes to the history.
Intermediate Info nodes are nodes visited by Info internally in the process of
searching the node to display.  Intermediate nodes are not presented
to the user."
  :type 'boolean
  :group 'info
  :version "24.1")

(defcustom Info-enable-edit nil
  "Non-nil means the \\<Info-mode-map>\\[Info-edit] command in Info can edit the current node.
This is convenient if you want to write Info files by hand.
However, we recommend that you not do this.
It is better to write a Texinfo file and generate the Info file from that,
because that gives you a printed manual as well."
  :type 'boolean
  :group 'info)

(defvar Info-enable-active-nodes nil
  "Non-nil allows Info to execute Lisp code associated with nodes.
The Lisp code is executed when the node is selected.")
(put 'Info-enable-active-nodes 'risky-local-variable t)

(defface info-node
  '((((class color) (background light)) :foreground "brown" :weight bold :slant italic)
    (((class color) (background dark)) :foreground "white" :weight bold :slant italic)
    (t :weight bold :slant italic))
  "Face for Info node names."
  :group 'info)

(defface info-title-1
  '((((type tty pc) (class color) (background light))
     :foreground "green" :weight bold)
    (((type tty pc) (class color) (background dark))
     :foreground "yellow" :weight bold)
    (t :height 1.2 :inherit info-title-2))
  "Face for info titles at level 1."
  :group 'info)
(define-obsolete-face-alias 'Info-title-1-face 'info-title-1 "22.1")

(defface info-title-2
  '((((type tty pc) (class color)) :foreground "lightblue" :weight bold)
    (t :height 1.2 :inherit info-title-3))
  "Face for info titles at level 2."
  :group 'info)
(define-obsolete-face-alias 'Info-title-2-face 'info-title-2 "22.1")

(defface info-title-3
  '((((type tty pc) (class color)) :weight bold)
    (t :height 1.2 :inherit info-title-4))
  "Face for info titles at level 3."
  :group 'info)
(define-obsolete-face-alias 'Info-title-3-face 'info-title-3 "22.1")

(defface info-title-4
  '((((type tty pc) (class color)) :weight bold)
    (t :weight bold :inherit variable-pitch))
  "Face for info titles at level 4."
  :group 'info)
(define-obsolete-face-alias 'Info-title-4-face 'info-title-4 "22.1")

(defface info-menu-header
  '((((type tty pc))
     :underline t
     :weight bold)
    (t
     :inherit variable-pitch
     :weight bold))
  "Face for headers in Info menus."
  :group 'info)

(defface info-menu-star
  '((((class color)) :foreground "red1")
    (t :underline t))
  "Face for every third `*' in an Info menu."
  :group 'info)
(define-obsolete-face-alias 'info-menu-5 'info-menu-star "22.1")

(defface info-xref
  '((t :inherit link))
  "Face for unvisited Info cross-references."
  :group 'info)

(defface info-xref-visited
  '((t :inherit (link-visited info-xref)))
  "Face for visited Info cross-references."
  :version "22.1"
  :group 'info)

(defcustom Info-fontify-visited-nodes t
  "Non-nil to fontify references to visited nodes in `info-xref-visited' face."
  :version "22.1"
  :type 'boolean
  :group 'info)

(defcustom Info-fontify-maximum-menu-size 100000
  "Maximum size of menu to fontify if `font-lock-mode' is non-nil.
Set to nil to disable node fontification."
  :type 'integer
  :group 'info)

(defcustom Info-use-header-line t
  "Non-nil means to put the beginning-of-node links in an Emacs header-line.
A header-line does not scroll with the rest of the buffer."
  :type 'boolean
  :group 'info)

(defface info-header-xref
  '((t :inherit info-xref))
  "Face for Info cross-references in a node header."
  :group 'info)

(defface info-header-node
  '((t :inherit info-node))
  "Face for Info nodes in a node header."
  :group 'info)

(defvar Info-directory-list nil
  "List of directories to search for Info documentation files.
If nil, meaning not yet initialized, Info uses the environment
variable INFOPATH to initialize it, or `Info-default-directory-list'
if there is no INFOPATH variable in the environment, or the
concatenation of the two if INFOPATH ends with a `path-separator'.

When `Info-directory-list' is initialized from the value of
`Info-default-directory-list', and Emacs is installed in one of the
standard directories, the directory of Info files that come with Emacs
is put last (so that local Info files override standard ones).

When `Info-directory-list' is initialized from the value of
`Info-default-directory-list', and Emacs is not installed in one
of the standard directories, the first element of the resulting
list is the directory where Emacs installs the Info files that
come with it.  This is so that Emacs's own manual, which suits the
version of Emacs you are using, will always be found first.  This
is useful when you install an experimental version of Emacs without
removing the standard installation.

If you want to override the order of directories in
`Info-default-directory-list', set INFOPATH in the environment.

If you run the Emacs executable from the `src' directory in the Emacs
source tree, and INFOPATH is not defined, the `info' directory in the
source tree is used as the first element of `Info-directory-list', in
place of the installation Info directory.  This is useful when you run
a version of Emacs without installing it.")

(defcustom Info-additional-directory-list nil
  "List of additional directories to search for Info documentation files.
These directories are searched after those in `Info-directory-list'."
  :type '(repeat directory)
  :group 'info)

(defcustom Info-scroll-prefer-subnodes nil
  "If non-nil, \\<Info-mode-map>\\[Info-scroll-up] in a menu visits subnodes.

If this is non-nil, and you scroll far enough in a node that its menu
appears on the screen, the next \\<Info-mode-map>\\[Info-scroll-up]
moves to a subnode indicated by the following menu item.  This means
that you visit a subnode before getting to the end of the menu.

Setting this option to nil results in behavior similar to the stand-alone
Info reader program, which visits the first subnode from the menu only
when you hit the end of the current node."
  :version "22.1"
  :type 'boolean
  :group 'info)

(defcustom Info-hide-note-references t
  "If non-nil, hide the tag and section reference in *note and * menu items.
If value is non-nil but not `hide', also replaces the \"*note\" with \"see\".
If value is non-nil but not t or `hide', the reference section is still shown.
`nil' completely disables this feature.  If this is non-nil, you might
want to set `Info-refill-paragraphs'."
  :version "22.1"
  :type '(choice (const :tag "No hiding" nil)
		 (const :tag "Replace tag and hide reference" t)
		 (const :tag "Hide tag and reference" hide)
		 (other :tag "Only replace tag" tag))
  :set (lambda (sym val)
	 (set sym val)
	 (dolist (buffer (buffer-list))
	   (with-current-buffer buffer
	     (when (eq major-mode 'Info-mode)
	       (revert-buffer t t)))))
  :group 'info)

(defcustom Info-refill-paragraphs nil
  "If non-nil, attempt to refill paragraphs with hidden references.
This refilling may accidentally remove explicit line breaks in the Info
file, so be prepared for a few surprises if you enable this feature.
This only has an effect if `Info-hide-note-references' is non-nil."
  :version "22.1"
  :type 'boolean
  :group 'info)

(defcustom Info-breadcrumbs-depth 4
  "Depth of breadcrumbs to display.
0 means do not display breadcrumbs."
  :version "23.1"
  :type 'integer
  :group 'info)

(defcustom Info-search-whitespace-regexp "\\s-+"
  "If non-nil, regular expression to match a sequence of whitespace chars.
This applies to Info search for regular expressions.
You might want to use something like \"[ \\t\\r\\n]+\" instead.
In the Customization buffer, that is `[' followed by a space,
a tab, a carriage return (control-M), a newline, and `]+'."
  :type 'regexp
  :group 'info)

(defcustom Info-isearch-search t
  "If non-nil, isearch in Info searches through multiple nodes.
Before leaving the initial Info node, where isearch was started,
it fails once with the error message [initial node], and with
subsequent C-s/C-r continues through other nodes without failing
with this error message in other nodes.  When isearch fails for
the rest of the manual, it wraps around the whole manual and
restarts the search from the top/final node depending on
search direction.

Setting this option to nil restores the default isearch behavior
with wrapping around the current Info node."
  :version "22.1"
  :type 'boolean
  :group 'info)

(defvar Info-isearch-initial-node nil)
(defvar Info-isearch-initial-history nil)
(defvar Info-isearch-initial-history-list nil)

(defcustom Info-mode-hook
  ;; Try to obey obsolete Info-fontify settings.
  (unless (and (boundp 'Info-fontify) (null Info-fontify))
    '(turn-on-font-lock))
  "Hooks run when `Info-mode' is called."
  :type 'hook
  :group 'info)

(defcustom Info-selection-hook nil
  "Hooks run when `Info-select-node' is called."
  :type 'hook
  :group 'info)

(defvar Info-edit-mode-hook nil
  "Hooks run when `Info-edit-mode' is called.")

(defvar Info-current-file nil
  "Info file that Info is now looking at, or nil.
This is the name that was specified in Info, not the actual file name.
It doesn't contain directory names or file name extensions added by Info.")

(defvar Info-current-subfile nil
  "Info subfile that is actually in the *info* buffer now.
It is nil if current Info file is not split into subfiles.")

(defvar Info-current-node nil
  "Name of node that Info is now looking at, or nil.")

(defvar Info-tag-table-marker nil
  "Marker pointing at beginning of current Info file's tag table.
Marker points nowhere if file has no tag table.")

(defvar Info-tag-table-buffer nil
  "Buffer used for indirect tag tables.")

(defvar Info-current-file-completions nil
  "Cached completion list for current Info file.")

(defvar Info-file-supports-index-cookies nil
  "Non-nil if current Info file supports index cookies.")

(defvar Info-file-supports-index-cookies-list nil
  "List of Info files with information about index cookies support.
Each element of the list is a list (FILENAME SUPPORTS-INDEX-COOKIES)
where SUPPORTS-INDEX-COOKIES can be either t or nil.")

(defvar Info-index-alternatives nil
  "List of possible matches for last `Info-index' command.")

(defvar Info-point-loc nil
  "Point location within a selected node.
If string, the point is moved to the proper occurrence of the
name of the followed cross reference within a selected node.
If number, the point is moved to the corresponding line.")

(defvar Info-standalone nil
  "Non-nil if Emacs was started solely as an Info browser.")

(defvar Info-virtual-files nil
  "List of definitions of virtual Info files.
Each element of the list has the format (FILENAME (OPERATION . HANDLER) ...)
where FILENAME is a regexp that matches a class of virtual Info file names.
It should be carefully chosen to not cause file name clashes with
existing file names.  OPERATION is one of the following operation
symbols `find-file', `find-node', `toc-nodes' that define what HANDLER
function to call instead of calling the default corresponding function
to override it.")

(defvar Info-virtual-nodes nil
  "List of definitions of virtual Info nodes.
Each element of the list has the format (NODENAME (OPERATION . HANDLER) ...)
where NODENAME is a regexp that matches a class of virtual Info node names.
It should be carefully chosen to not cause node name clashes with
existing node names.  OPERATION is one of the following operation
symbols `find-node' that define what HANDLER function to call instead
of calling the default corresponding function to override it.")

(defvar Info-current-node-virtual nil
  "Non-nil if the current Info node is virtual.")

(defun Info-virtual-file-p (filename)
  "Check if Info file FILENAME is virtual."
  (Info-virtual-fun 'find-file filename nil))

(defun Info-virtual-fun (op filename nodename)
  "Find a function that handles operations on virtual manuals.
OP is an operation symbol (`find-file', `find-node' or `toc-nodes'),
FILENAME is a virtual Info file name, NODENAME is a virtual Info
node name.  Return a function found either in `Info-virtual-files'
or `Info-virtual-nodes'."
  (or (and (stringp filename) ; some legacy code can still use a symbol
	   (cdr-safe (assoc op (assoc-default filename
					      Info-virtual-files
					      'string-match))))
      (and (stringp nodename) ; some legacy code can still use a symbol
	   (cdr-safe (assoc op (assoc-default nodename
					      Info-virtual-nodes
					      'string-match))))))

(defun Info-virtual-call (virtual-fun &rest args)
  "Call a function that handles operations on virtual manuals."
  (when (functionp virtual-fun)
    (or (apply virtual-fun args) t)))


(defvar Info-suffix-list
  ;; The MS-DOS list should work both when long file names are
  ;; supported (Windows 9X), and when only 8+3 file names are available.
  (if (eq system-type 'ms-dos)
      '( (".gz"       . "gunzip")
	 (".z"        . "gunzip")
	 (".bz2"      . ("bzip2" "-dc"))
	 (".inz"      . "gunzip")
	 (".igz"      . "gunzip")
	 (".info.Z"   . "gunzip")
	 (".info.gz"  . "gunzip")
	 ("-info.Z"   . "gunzip")
	 ("-info.gz"  . "gunzip")
	 ("/index.gz" . "gunzip")
	 ("/index.z"  . "gunzip")
	 (".inf"      . nil)
	 (".info"     . nil)
	 ("-info"     . nil)
	 ("/index"    . nil)
	 (""          . nil))
    '( (".info.Z"    . "uncompress")
       (".info.Y"    . "unyabba")
       (".info.gz"   . "gunzip")
       (".info.z"    . "gunzip")
       (".info.bz2"  . ("bzip2" "-dc"))
       (".info.xz"   . "unxz")
       (".info"      . nil)
       ("-info.Z"    . "uncompress")
       ("-info.Y"    . "unyabba")
       ("-info.gz"   . "gunzip")
       ("-info.bz2"  . ("bzip2" "-dc"))
       ("-info.z"    . "gunzip")
       ("-info.xz"   . "unxz")
       ("-info"      . nil)
       ("/index.Z"   . "uncompress")
       ("/index.Y"   . "unyabba")
       ("/index.gz"  . "gunzip")
       ("/index.z"   . "gunzip")
       ("/index.bz2" . ("bzip2" "-dc"))
       ("/index.xz"  . "unxz")
       ("/index"     . nil)
       (".Z"         . "uncompress")
       (".Y"         . "unyabba")
       (".gz"        . "gunzip")
       (".z"         . "gunzip")
       (".bz2"       . ("bzip2" "-dc"))
       (".xz"        . "unxz")
       (""           . nil)))
  "List of file name suffixes and associated decoding commands.
Each entry should be (SUFFIX . STRING); the file is given to
the command as standard input.

STRING may be a list of strings.  In that case, the first element is
the command name, and the rest are arguments to that command.

If STRING is nil, no decoding is done.
Because the SUFFIXes are tried in order, the empty string should
be last in the list.")

;; Concatenate SUFFIX onto FILENAME.  SUFFIX should start with a dot.
;; First, on MS-DOS with no long file names support, delete some of
;; the extension in FILENAME to make room.
(defun info-insert-file-contents-1 (filename suffix lfn)
  (if lfn	; long file names are supported
      (concat filename suffix)
    (let* ((sans-exts (file-name-sans-extension filename))
	   ;; How long is the extension in FILENAME (not counting the dot).
	   (ext-len (max 0 (- (length filename) (length sans-exts) 1)))
	   ext-left)
      ;; SUFFIX starts with a dot.  If FILENAME already has one,
      ;; get rid of the one in SUFFIX (unless suffix is empty).
      (or (and (<= ext-len 0)
	       (not (eq (aref filename (1- (length filename))) ?.)))
	  (= (length suffix) 0)
	  (setq suffix (substring suffix 1)))
      ;; How many chars of that extension should we keep?
      (setq ext-left (min ext-len (max 0 (- 3 (length suffix)))))
      ;; Get rid of the rest of the extension, and add SUFFIX.
      (concat (substring filename 0 (- (length filename)
				       (- ext-len ext-left)))
	      suffix))))

(defun info-file-exists-p (filename)
  (and (file-exists-p filename)
       (not (file-directory-p filename))))

(defun info-insert-file-contents (filename &optional visit)
  "Insert the contents of an Info file in the current buffer.
Do the right thing if the file has been compressed or zipped."
  (let* ((tail Info-suffix-list)
	 (jka-compr-verbose nil)
	 (lfn (if (fboundp 'msdos-long-file-names)
		  (msdos-long-file-names)
		t))
	 (check-short (and (fboundp 'msdos-long-file-names)
			   lfn))
	 fullname decoder done)
    (if (info-file-exists-p filename)
	;; FILENAME exists--see if that name contains a suffix.
	;; If so, set DECODE accordingly.
	(progn
	  (while (and tail
		      (not (string-match
			    (concat (regexp-quote (car (car tail))) "$")
			    filename)))
	    (setq tail (cdr tail)))
	  (setq fullname filename
		decoder (cdr (car tail))))
      ;; Try adding suffixes to FILENAME and see if we can find something.
      (while (and tail (not done))
	(setq fullname (info-insert-file-contents-1 filename
						    (car (car tail)) lfn))
	(if (info-file-exists-p fullname)
	    (setq done t
		  ;; If we found a file with a suffix, set DECODER
		  ;; according to the suffix.
		  decoder (cdr (car tail)))
	  ;; When the MS-DOS port runs on Windows, we need to check
	  ;; the short variant of a long file name as well.
	  (when check-short
	    (setq fullname (info-insert-file-contents-1 filename
							(car (car tail)) nil))
	    (if (info-file-exists-p fullname)
		(setq done t
		      decoder (cdr (car tail))))))
	(setq tail (cdr tail)))
      (or tail
	  (error "Can't find %s or any compressed version of it" filename)))
    ;; check for conflict with jka-compr
    (if (and (jka-compr-installed-p)
	     (jka-compr-get-compression-info fullname))
	(setq decoder nil))
    (if decoder
	(progn
	  (insert-file-contents-literally fullname visit)
	  (let ((inhibit-read-only t)
		(coding-system-for-write 'no-conversion)
		(inhibit-null-byte-detection t) ; Index nodes include null bytes
		(default-directory (or (file-name-directory fullname)
				       default-directory)))
	    (or (consp decoder)
		(setq decoder (list decoder)))
	    (apply 'call-process-region (point-min) (point-max)
		   (car decoder) t t nil (cdr decoder))))
      (let ((inhibit-null-byte-detection t)) ; Index nodes include null bytes
	(insert-file-contents fullname visit)))))

(defun Info-file-supports-index-cookies (&optional file)
  "Return non-nil value if FILE supports Info index cookies.
Info index cookies were first introduced in 4.7, and all later
makeinfo versions output them in index nodes, so we can rely
solely on the makeinfo version.  This function caches the information
in `Info-file-supports-index-cookies-list'."
  (or file (setq file Info-current-file))
  (or (assoc file Info-file-supports-index-cookies-list)
      ;; Skip virtual Info files
      (and (or (not (stringp file))
	       (Info-virtual-file-p file))
           (setq Info-file-supports-index-cookies-list
		 (cons (cons file nil) Info-file-supports-index-cookies-list)))
      (save-excursion
	(let ((found nil))
	  (goto-char (point-min))
	  (condition-case ()
	      (if (and (re-search-forward
			"makeinfo[ \n]version[ \n]\\([0-9]+.[0-9]+\\)"
			(line-beginning-position 4) t)
		       (not (version< (match-string 1) "4.7")))
		  (setq found t))
	    (error nil))
	  (setq Info-file-supports-index-cookies-list
		(cons (cons file found) Info-file-supports-index-cookies-list)))))
  (cdr (assoc file Info-file-supports-index-cookies-list)))


(defun Info-default-dirs ()
  (let ((source (expand-file-name "info/" source-directory))
	(sibling (if installation-directory
		     (expand-file-name "info/" installation-directory)
		   (if invocation-directory
		       (let ((infodir (expand-file-name
				       "../share/info/"
				       invocation-directory)))
			 (if (file-exists-p infodir)
			     infodir
			   (setq infodir (expand-file-name
					  "../../../share/info/"
					  invocation-directory))
			   (and (file-exists-p infodir)
				infodir))))))
	alternative)
    (setq alternative
	  (if (and sibling (file-exists-p sibling))
	      ;; Uninstalled, Emacs builddir != srcdir.
	      sibling
	    ;; Uninstalled, builddir == srcdir
	    source))
    (if (or (member alternative Info-default-directory-list)
	    ;; On DOS/NT, we use movable executables always,
	    ;; and we must always find the Info dir at run time.
	    (if (memq system-type '(ms-dos windows-nt))
		nil
	      ;; Use invocation-directory for Info
	      ;; only if we used it for exec-directory also.
	      (not (string= exec-directory
			    (expand-file-name "lib-src/"
					      installation-directory))))
	    (not (file-exists-p alternative)))
	Info-default-directory-list
      ;; `alternative' contains the Info files that came with this
      ;; version, so we should look there first.  `Info-insert-dir'
      ;; currently expects to find `alternative' first on the list.
      (cons alternative
	    ;; Don't drop the last part, it might contain non-Emacs stuff.
	    ;; (reverse (cdr (reverse
	    Info-default-directory-list)))) ;; )))

(defun info-initialize ()
  "Initialize `Info-directory-list', if that hasn't been done yet."
  (unless Info-directory-list
    (let ((path (getenv "INFOPATH"))
	  (sep (regexp-quote path-separator)))
      (setq Info-directory-list
	    (prune-directory-list
	     (if path
		 (if (string-match-p (concat sep "\\'") path)
		     (append (split-string (substring path 0 -1) sep)
			     (Info-default-dirs))
		   (split-string path sep))
	       (Info-default-dirs)))))))

;;;###autoload
(defun info-other-window (&optional file-or-node)
  "Like `info' but show the Info buffer in another window."
  (interactive (if current-prefix-arg
		   (list (read-file-name "Info file name: " nil nil t))))
  (info-setup file-or-node (switch-to-buffer-other-window "*info*")))

;;;###autoload (put 'info 'info-file (purecopy "emacs"))
;;;###autoload
(defun info (&optional file-or-node buffer)
  "Enter Info, the documentation browser.
Optional argument FILE-OR-NODE specifies the file to examine;
the default is the top-level directory of Info.
Called from a program, FILE-OR-NODE may specify an Info node of the form
\"(FILENAME)NODENAME\".
Optional argument BUFFER specifies the Info buffer name;
the default buffer name is *info*.  If BUFFER exists,
just switch to BUFFER.  Otherwise, create a new buffer
with the top-level Info directory.

In interactive use, a non-numeric prefix argument directs
this command to read a file name from the minibuffer.
A numeric prefix argument selects an Info buffer with the prefix number
appended to the Info buffer name.

The search path for Info files is in the variable `Info-directory-list'.
The top-level Info directory is made by combining all the files named `dir'
in all the directories in that path.

See a list of available Info commands in `Info-mode'."
  (interactive (list
                (if (and current-prefix-arg (not (numberp current-prefix-arg)))
                    (read-file-name "Info file name: " nil nil t))
                (if (numberp current-prefix-arg)
                    (format "*info*<%s>" current-prefix-arg))))
  (info-setup file-or-node
	      (pop-to-buffer-same-window (or buffer "*info*"))))

(defun info-setup (file-or-node buffer)
  "Display Info node FILE-OR-NODE in BUFFER."
  (if (and buffer (not (eq major-mode 'Info-mode)))
      (Info-mode))
  (if file-or-node
      ;; If argument already contains parentheses, don't add another set
      ;; since the argument will then be parsed improperly.  This also
      ;; has the added benefit of allowing node names to be included
      ;; following the parenthesized filename.
      (Info-goto-node
       (if (and (stringp file-or-node) (string-match "(.*)" file-or-node))
           file-or-node
         (concat "(" file-or-node ")")))
    (if (and (zerop (buffer-size))
	     (null Info-history))
	;; If we just created the Info buffer, go to the directory.
	(Info-directory))))

;;;###autoload
(defun info-emacs-manual ()
  "Display the Emacs manual in Info mode."
  (interactive)
  (info "emacs"))

;;;###autoload
(defun info-standalone ()
  "Run Emacs as a standalone Info reader.
Usage:  emacs -f info-standalone [filename]
In standalone mode, \\<Info-mode-map>\\[Info-exit] exits Emacs itself."
  (setq Info-standalone t)
  (if (and command-line-args-left
	   (not (string-match "^-" (car command-line-args-left))))
      (condition-case err
	  (progn
	    (info (car command-line-args-left))
	    (setq command-line-args-left (cdr command-line-args-left)))
	(error (send-string-to-terminal
		(format "%s\n" (if (eq (car-safe err) 'error)
				   (nth 1 err) err)))
	       (save-buffers-kill-emacs)))
    (info)))

;; See if the accessible portion of the buffer begins with a node
;; delimiter, and the node header line which follows matches REGEXP.
;; Typically, this test will be followed by a loop that examines the
;; rest of the buffer with (search-forward "\n\^_"), and it's a pity
;; to have the overhead of this special test inside the loop.

;; This function changes match-data, but supposedly the caller might
;; want to use the results of re-search-backward.

;; The return value is the value of point at the beginning of matching
;; REGEXP, if the function succeeds, nil otherwise.
(defun Info-node-at-bob-matching (regexp)
  (and (bobp)				; are we at beginning of buffer?
       (looking-at "\^_")		; does it begin with node delimiter?
       (let (beg)
	 (forward-line 1)
	 (setq beg (point))
	 (forward-line 1)		; does the line after delimiter match REGEXP?
	 (re-search-backward regexp beg t))))

(defun Info-find-file (filename &optional noerror)
  "Return expanded FILENAME, or t if FILENAME is \"dir\".
Optional second argument NOERROR, if t, means if file is not found
just return nil (no error)."
  ;; Convert filename to lower case if not found as specified.
  ;; Expand it.
  (cond
   ((Info-virtual-call
     (Info-virtual-fun 'find-file filename nil)
     filename noerror))
   ((stringp filename)
    (let (temp temp-downcase found)
      (setq filename (substitute-in-file-name filename))
      (let ((dirs (if (string-match "^\\./" filename)
		      ;; If specified name starts with `./'
		      ;; then just try current directory.
		      '("./")
		    (if (file-name-absolute-p filename)
			;; No point in searching for an
			;; absolute file name
			'(nil)
		      (if Info-additional-directory-list
			  (append Info-directory-list
				  Info-additional-directory-list)
			Info-directory-list)))))
	;; Fall back on the installation directory if we can't find
	;; the info node anywhere else.
	(when installation-directory
	  (setq dirs (append dirs (list (expand-file-name
					 "info" installation-directory)))))
	;; Search the directory list for file FILENAME.
	(while (and dirs (not found))
	  (setq temp (expand-file-name filename (car dirs)))
	  (setq temp-downcase
		(expand-file-name (downcase filename) (car dirs)))
	  ;; Try several variants of specified name.
	  (let ((suffix-list Info-suffix-list)
		(lfn (if (fboundp 'msdos-long-file-names)
			 (msdos-long-file-names)
		       t)))
	    (while (and suffix-list (not found))
	      (cond ((info-file-exists-p
		      (info-insert-file-contents-1
		       temp (car (car suffix-list)) lfn))
		     (setq found temp))
		    ((info-file-exists-p
		      (info-insert-file-contents-1
		       temp-downcase (car (car suffix-list)) lfn))
		     (setq found temp-downcase))
		    ((and (fboundp 'msdos-long-file-names)
			  lfn
			  (info-file-exists-p
			   (info-insert-file-contents-1
			    temp (car (car suffix-list)) nil)))
		     (setq found temp)))
	      (setq suffix-list (cdr suffix-list))))
	  (setq dirs (cdr dirs))))
      (if found
	  (setq filename found)
	(if noerror
	    (setq filename nil)
	  (error "Info file %s does not exist" filename)))
      filename))))

(defun Info-find-node (filename nodename &optional no-going-back)
  "Go to an Info node specified as separate FILENAME and NODENAME.
NO-GOING-BACK is non-nil if recovering from an error in this function;
it says do not attempt further (recursive) error recovery."
  (info-initialize)
  (setq filename (Info-find-file filename))
  ;; Go into Info buffer.
  (or (eq major-mode 'Info-mode) (switch-to-buffer "*info*"))
  ;; Record the node we are leaving, if we were in one.
  (and (not no-going-back)
       Info-current-file
       (push (list Info-current-file Info-current-node (point))
             Info-history))
  (Info-find-node-2 filename nodename no-going-back))

;;;###autoload
(defun Info-on-current-buffer (&optional nodename)
  "Use Info mode to browse the current Info buffer.
With a prefix arg, this queries for the node name to visit first;
otherwise, that defaults to `Top'."
  (interactive
   (list (if current-prefix-arg
	     (completing-read "Node name: " (Info-build-node-completions)
			      nil t "Top"))))
  (unless nodename (setq nodename "Top"))
  (info-initialize)
  (Info-mode)
  (set (make-local-variable 'Info-current-file)
       (or buffer-file-name
	   ;; If called on a non-file buffer, make a fake file name.
	   (concat default-directory (buffer-name))))
  (Info-find-node-2 nil nodename))

(defun Info-revert-find-node (filename nodename)
  "Go to an Info node FILENAME and NODENAME, re-reading disk contents.
When *info* is already displaying FILENAME and NODENAME, the window position
is preserved, if possible."
  (or (eq major-mode 'Info-mode) (switch-to-buffer "*info*"))
  (let ((old-filename Info-current-file)
	(old-nodename Info-current-node)
	(window-selected (eq (selected-window) (get-buffer-window)))
	(pcolumn      (current-column))
	(pline        (count-lines (point-min) (line-beginning-position)))
	(wline        (count-lines (point-min) (window-start)))
	(new-history  (and Info-current-file
			   (list Info-current-file Info-current-node (point)))))
    ;; When `Info-current-file' is nil, `Info-find-node-2' rereads the file.
    (setq Info-current-file nil)
    (Info-find-node filename nodename)
    (if (and (equal old-filename Info-current-file)
	     (equal old-nodename Info-current-node))
	(progn
	  ;; note goto-line is no good, we want to measure from point-min
	  (when window-selected
	    (goto-char (point-min))
	    (forward-line wline)
	    (set-window-start (selected-window) (point)))
	  (goto-char (point-min))
	  (forward-line pline)
	  (move-to-column pcolumn))
      ;; only add to the history when coming from a different file+node
      (if new-history
	  (setq Info-history (cons new-history Info-history))))))

(defun Info-revert-buffer-function (_ignore-auto noconfirm)
  (when (or noconfirm (y-or-n-p "Revert info buffer? "))
    (Info-revert-find-node Info-current-file Info-current-node)
    (message "Reverted %s" Info-current-file)))

(defun Info-find-in-tag-table-1 (marker regexp case-fold)
  "Find a node in a tag table.
MARKER specifies the buffer and position to start searching at.
REGEXP is a regular expression matching nodes or references.  Its first
group should match `Node:' or `Ref:'.
CASE-FOLD t means search for a case-insensitive match.
If a match was found, value is a list (FOUND-ANCHOR POS MODE), where
FOUND-ANCHOR is non-nil if a `Ref:' was matched, POS is the position
where the match was found, and MODE is `major-mode' of the buffer in
which the match was found."
  (let ((case-fold-search case-fold))
    (with-current-buffer (marker-buffer marker)
      (goto-char marker)

      ;; Search tag table
      (beginning-of-line)
      (when (re-search-forward regexp nil t)
	(list (string-equal "Ref:" (match-string 1))
	      (+ (point-min) (read (current-buffer)))
	      major-mode)))))

(defun Info-find-in-tag-table (marker regexp)
  "Find a node in a tag table.
MARKER specifies the buffer and position to start searching at.
REGEXP is a regular expression matching nodes or references.  Its first
group should match `Node:' or `Ref:'.
If a match was found, value is a list (FOUND-ANCHOR POS MODE), where
FOUND-ANCHOR is non-nil if a `Ref:' was matched, POS is the position
where the match was found, and MODE is `major-mode' of the buffer in
which the match was found.
This function tries to find a case-sensitive match first, then a
case-insensitive match is tried."
  (let ((result (Info-find-in-tag-table-1 marker regexp nil)))
    (when (null (car result))
      (setq result (Info-find-in-tag-table-1 marker regexp t)))
    result))

(defun Info-find-node-in-buffer-1 (regexp case-fold)
  "Find a node or anchor in the current buffer.
REGEXP is a regular expression matching nodes or references.  Its first
group should match `Node:' or `Ref:'.
CASE-FOLD t means search for a case-insensitive match.
Value is the position at which a match was found, or nil if not found."
  (let ((case-fold-search case-fold)
	found)
    (save-excursion
      (if (Info-node-at-bob-matching regexp)
          (setq found (point))
        (while (and (not found)
                    (search-forward "\n\^_" nil t))
          (forward-line 1)
          (let ((beg (point)))
            (forward-line 1)
            (if (re-search-backward regexp beg t)
                (setq found (line-beginning-position)))))))
    found))

(defun Info-find-node-in-buffer (regexp)
  "Find a node or anchor in the current buffer.
REGEXP is a regular expression matching nodes or references.  Its first
group should match `Node:' or `Ref:'.
Value is the position at which a match was found, or nil if not found.
This function looks for a case-sensitive match first.  If none is found,
a case-insensitive match is tried."
  (or (Info-find-node-in-buffer-1 regexp nil)
      (Info-find-node-in-buffer-1 regexp t)))

(defun Info-find-node-2 (filename nodename &optional no-going-back)
  (buffer-disable-undo (current-buffer))
  (or (eq major-mode 'Info-mode)
      (Info-mode))
  (widen)
  (setq Info-current-node nil)
  (unwind-protect
      (let ((case-fold-search t)
	    (virtual-fun (Info-virtual-fun 'find-node
					   (or filename Info-current-file)
					   nodename))
	    anchorpos)
	(cond
	 ((functionp virtual-fun)
	  (let ((filename (or filename Info-current-file)))
	    (setq buffer-read-only nil)
	    (setq Info-current-file filename
		  Info-current-subfile nil
		  Info-current-file-completions nil
		  buffer-file-name nil)
	    (erase-buffer)
	    (Info-virtual-call virtual-fun filename nodename no-going-back)
	    (set-marker Info-tag-table-marker nil)
	    (setq buffer-read-only t)
	    (set-buffer-modified-p nil)
	    (set (make-local-variable 'Info-current-node-virtual) t)))
	 ((not (and
		;; Reread a file when moving from a virtual node.
		(not Info-current-node-virtual)
		(or (null filename)
		    (equal Info-current-file filename))))
	  ;; Switch files if necessary
	  (let ((inhibit-read-only t))
	    (when Info-current-node-virtual
	      ;; When moving from a virtual node.
	      (set (make-local-variable 'Info-current-node-virtual) nil)
	      (if (null filename)
		  (setq filename Info-current-file)))
	    (setq Info-current-file nil
		  Info-current-subfile nil
		  Info-current-file-completions nil
		  buffer-file-name nil)
	    (erase-buffer)
	    (info-insert-file-contents filename nil)
	    (setq default-directory (file-name-directory filename))
	    (set-buffer-modified-p nil)
	    (set (make-local-variable 'Info-file-supports-index-cookies)
		 (Info-file-supports-index-cookies filename))

	    ;; See whether file has a tag table.  Record the location if yes.
	    (goto-char (point-max))
	    (forward-line -8)
	    ;; Use string-equal, not equal, to ignore text props.
	    (if (not (or (string-equal nodename "*")
			 (not
			  (search-forward "\^_\nEnd tag table\n" nil t))))
		(let (pos)
		  ;; We have a tag table.  Find its beginning.
		  ;; Is this an indirect file?
		  (search-backward "\nTag table:\n")
		  (setq pos (point))
		  (if (save-excursion
			(forward-line 2)
			(looking-at "(Indirect)\n"))
		      ;; It is indirect.  Copy it to another buffer
		      ;; and record that the tag table is in that buffer.
		      (let ((buf (current-buffer))
			    (tagbuf
			     (or Info-tag-table-buffer
				 (generate-new-buffer " *info tag table*"))))
			(setq Info-tag-table-buffer tagbuf)
			(with-current-buffer tagbuf
			  (buffer-disable-undo (current-buffer))
			  (setq case-fold-search t)
			  (erase-buffer)
			  (insert-buffer-substring buf))
			(set-marker Info-tag-table-marker
				    (match-end 0) tagbuf))
		    (set-marker Info-tag-table-marker pos)))
	      (set-marker Info-tag-table-marker nil))
	    (setq Info-current-file filename)
	    )))

        ;; Use string-equal, not equal, to ignore text props.
        (if (string-equal nodename "*")
            (progn (setq Info-current-node nodename)
                   (Info-set-mode-line))
          ;; Possibilities:
          ;;
          ;; 1. Anchor found in tag table
          ;; 2. Anchor *not* in tag table
          ;;
          ;; 3. Node found in tag table
          ;; 4. Node *not* found in tag table, but found in file
          ;; 5. Node *not* in tag table, and *not* in file
          ;;
          ;; *Or* the same, but in an indirect subfile.

          ;; Search file for a suitable node.
	  (let ((guesspos (point-min))
		(regexp (concat "\\(Node:\\|Ref:\\) *\\("
				(if (stringp nodename)
				    (regexp-quote nodename)
				  "")
				"\\) *[,\t\n\177]")))

	    (catch 'foo

	      ;; First, search a tag table, if any
	      (when (marker-position Info-tag-table-marker)
		(let* ((m Info-tag-table-marker)
		       (found (Info-find-in-tag-table m regexp)))

		  (when found
		    ;; FOUND is (ANCHOR POS MODE).
		    (setq guesspos (nth 1 found))

		    ;; If this is an indirect file, determine which
		    ;; file really holds this node and read it in.
		    (unless (eq (nth 2 found) 'Info-mode)
		      ;; Note that the current buffer must be the
		      ;; *info* buffer on entry to
		      ;; Info-read-subfile.  Thus the hackery above.
		      (setq guesspos (Info-read-subfile guesspos)))

		    ;; Handle anchor
		    (when (nth 0 found)
		      (goto-char (setq anchorpos guesspos))
		      (throw 'foo t)))))

	      ;; Else we may have a node, which we search for:
	      (goto-char (max (point-min)
			      (- (byte-to-position guesspos) 1000)))

	      ;; Now search from our advised position (or from beg of
	      ;; buffer) to find the actual node.  First, check
	      ;; whether the node is right where we are, in case the
	      ;; buffer begins with a node.
	      (let ((pos (Info-find-node-in-buffer regexp)))
		(when pos
		  (goto-char pos)
		  (throw 'foo t)))

              (when (string-match "\\([^.]+\\)\\." nodename)
                (let (Info-point-loc)
                  (Info-find-node-2
                   filename (match-string 1 nodename) no-going-back))
                (widen)
                (throw 'foo t))

              ;; No such anchor in tag table or node in tag table or file
              (error "No such node or anchor: %s" nodename))

	    (Info-select-node)
	    (goto-char (point-min))
	    (forward-line 1)		       ; skip header line
	    ;; (when (> Info-breadcrumbs-depth 0) ; skip breadcrumbs line
	    ;;   (forward-line 1))

	    (cond (anchorpos
                   (let ((new-history (list Info-current-file
                                            (substring-no-properties nodename))))
                     ;; Add anchors to the history too
                     (setq Info-history-list
                           (cons new-history
                                 (remove new-history Info-history-list))))
                   (goto-char anchorpos))
                  ((numberp Info-point-loc)
                   (forward-line (- Info-point-loc 2))
                   (setq Info-point-loc nil))
		  ((stringp Info-point-loc)
		   (Info-find-index-name Info-point-loc)
		   (setq Info-point-loc nil))))))
    ;; If we did not finish finding the specified node,
    ;; go back to the previous one.
    (or Info-current-node no-going-back (null Info-history)
        (let ((hist (car Info-history)))
          (setq Info-history (cdr Info-history))
          (Info-find-node (nth 0 hist) (nth 1 hist) t)
          (goto-char (nth 2 hist))))))

;; Cache the contents of the (virtual) dir file, once we have merged
;; it for the first time, so we can save time subsequently.
(defvar Info-dir-contents nil)

;; Cache for the directory we decided to use for the default-directory
;; of the merged dir text.
(defvar Info-dir-contents-directory nil)

;; Record the file attributes of all the files from which we
;; constructed Info-dir-contents.
(defvar Info-dir-file-attributes nil)

(defvar Info-dir-file-name nil)

;; Construct the Info directory node by merging the files named `dir'
;; from various directories.  Set the *info* buffer's
;; default-directory to the first directory we actually get any text
;; from.
(defun Info-insert-dir ()
  (if (and Info-dir-contents Info-dir-file-attributes
	   ;; Verify that none of the files we used has changed
	   ;; since we used it.
	   (eval (cons 'and
		       (mapcar (lambda (elt)
				 (let ((curr (file-attributes
					      ;; Handle symlinks
					      (file-truename (car elt)))))

				   ;; Don't compare the access time.
				   (if curr (setcar (nthcdr 4 curr) 0))
				   (setcar (nthcdr 4 (cdr elt)) 0)
				   (equal (cdr elt) curr)))
			       Info-dir-file-attributes))))
      (progn
	(insert Info-dir-contents)
	(goto-char (point-min)))
    (let ((dirs (if Info-additional-directory-list
		    (append Info-directory-list
			    Info-additional-directory-list)
		  Info-directory-list))
	  (dir-file-attrs nil)
	  ;; Bind this in case the user sets it to nil.
	  (case-fold-search t)
	  ;; This is set non-nil if we find a problem in some input files.
	  problems
	  buffers buffer others nodes dirs-done)

      ;; Search the directory list for the directory file.
      (while dirs
	(let ((truename (file-truename (expand-file-name (car dirs)))))
	  (or (member truename dirs-done)
	      (member (directory-file-name truename) dirs-done)
	      ;; Try several variants of specified name.
	      ;; Try upcasing, appending `.info', or both.
	      (let* (file
		     (attrs
		      (or
		       (progn (setq file (expand-file-name "dir" truename))
			      (file-attributes file))
		       (progn (setq file (expand-file-name "DIR" truename))
			      (file-attributes file))
		       (progn (setq file (expand-file-name "dir.info" truename))
			      (file-attributes file))
		       (progn (setq file (expand-file-name "DIR.INFO" truename))
			      (file-attributes file))
		       ;; Shouldn't really happen, but sometimes does,
		       ;; eg on Debian systems with buggy packages;
		       ;; so may as well try it.
		       ;; http://lists.gnu.org/archive/html/emacs-devel/2012-03/msg00005.html
		       (progn (setq file (expand-file-name "dir.gz" truename))
			      (file-attributes file)))))
		(setq dirs-done
		      (cons truename
			    (cons (directory-file-name truename)
				  dirs-done)))
		(if attrs
		    (with-current-buffer (generate-new-buffer " info dir")
		      (or buffers
			  (message "Composing main Info directory..."))
		      (condition-case nil
			  ;; Index nodes include null bytes.  DIR
			  ;; files should not have indices, but who
			  ;; knows...
			  (let ((inhibit-null-byte-detection t))
			    (insert-file-contents file)
			    (set (make-local-variable 'Info-dir-file-name)
				 file)
			    (push (current-buffer) buffers)
			    (push (cons file attrs) dir-file-attrs))
			(error (kill-buffer (current-buffer))))))))
	  (unless (cdr dirs)
	    (set (make-local-variable 'Info-dir-contents-directory)
		 (file-name-as-directory (car dirs))))
	  (setq dirs (cdr dirs))))

      (or buffers
	  (error "Can't find the Info directory node"))

      ;; Distinguish the dir file that comes with Emacs from all the
      ;; others.  Yes, that is really what this is supposed to do.
      ;; The definition of `Info-directory-list' puts it first on that
      ;; list and so last in `buffers' at this point.
      (setq buffer (car (last buffers))
	    others (delq buffer buffers))

      ;; Insert the entire original dir file as a start; note that we've
      ;; already saved its default directory to use as the default
      ;; directory for the whole concatenation.
      (save-excursion (insert-buffer-substring buffer))

      ;; Look at each of the other buffers one by one.
      (dolist (other others)
	(let (this-buffer-nodes)
	  ;; In each, find all the menus.
	  (with-current-buffer other
	    (goto-char (point-min))
	    ;; Find each menu, and add an elt to NODES for it.
	    (while (re-search-forward "^\\* Menu:" nil t)
	      (while (and (zerop (forward-line 1)) (eolp)))
	      (let ((beg (point))
		    nodename end)
		(re-search-backward "^\^_")
		(search-forward "Node: ")
		(setq nodename (Info-following-node-name))
		(search-forward "\n\^_" nil 'move)
		(beginning-of-line)
		(setq end (point))
		(push (list nodename other beg end) this-buffer-nodes)))
	    (if (assoc-string "top" this-buffer-nodes t)
		(setq nodes (nconc this-buffer-nodes nodes))
	      (setq problems t)
	      (message "No `top' node in %s" Info-dir-file-name)))))
      ;; Add to the main menu a menu item for each other node.
      (re-search-forward "^\\* Menu:")
      (forward-line 1)
      (let ((menu-items '("top"))
	    (end (save-excursion (search-forward "\^_" nil t) (point))))
	(dolist (node nodes)
	  (let ((nodename (car node)))
	    (save-excursion
	      (or (member (downcase nodename) menu-items)
		  (re-search-forward (concat "^\\* +"
					     (regexp-quote nodename)
					     "::")
				     end t)
		  (progn
		    (insert "* " nodename "::" "\n")
		    (push nodename menu-items)))))))
      ;; Now take each node of each of the other buffers
      ;; and merge it into the main buffer.
      (dolist (node nodes)
	(let ((case-fold-search t)
	      (nodename (car node)))
	  (goto-char (point-min))
	  ;; Find the like-named node in the main buffer.
	  (if (re-search-forward (concat "^\^_.*\n.*Node: "
					 (regexp-quote nodename)
					 "[,\n\t]")
				 nil t)
	      (progn
		(search-forward "\n\^_" nil 'move)
		(beginning-of-line)
		(insert "\n"))
	    ;; If none exists, add one.
	    (goto-char (point-max))
	    (insert "\^_\nFile: dir\tNode: " nodename "\n\n* Menu:\n\n"))
	  ;; Merge the text from the other buffer's menu
	  ;; into the menu in the like-named node in the main buffer.
	  (apply 'insert-buffer-substring (cdr node))))
      (Info-dir-remove-duplicates)
      ;; Kill all the buffers we just made, including the special one excised.
      (mapc 'kill-buffer (cons buffer buffers))
      (goto-char (point-min))
      (if problems
	  (message "Composing main Info directory...problems encountered, see `*Messages*'")
	(message "Composing main Info directory...done"))
      (set (make-local-variable 'Info-dir-contents) (buffer-string))
      (set (make-local-variable 'Info-dir-file-attributes) dir-file-attrs)))
  (setq default-directory Info-dir-contents-directory))

(defvar Info-streamline-headings
  '(("Emacs" . "Emacs")
    ("Programming" . "Programming")
    ("Libraries" . "Libraries")
    ("World Wide Web\\|Net Utilities" . "Net Utilities"))
  "List of elements (RE . NAME) to merge headings matching RE to NAME.")

(defun Info-dir-remove-duplicates ()
  (let (limit)
    (goto-char (point-min))
    ;; Remove duplicate headings in the same menu.
    (while (search-forward "\n* Menu:" nil t)
      (setq limit (save-excursion (search-forward "\n\^_" nil t)))
      ;; Look for the next heading to unify.
      (while (re-search-forward "^\\(\\w.*\\)\n\\*" limit t)
	(let ((name (match-string 1))
	      (start (match-beginning 0))
	      (entries nil) re)
	  ;; Check whether this heading should be streamlined.
	  (save-match-data
	    (dolist (x Info-streamline-headings)
	      (when (string-match (car x) name)
		(setq name (cdr x))
		(setq re (car x)))))
	  (if re (replace-match name t t nil 1))
	  (goto-char (if (re-search-forward "^[^* \n\t]" limit t)
			 (match-beginning 0)
		       (or limit (point-max))))
	  ;; Look for other headings of the same category and merge them.
	  (save-excursion
	    (while (re-search-forward "^\\(\\w.*\\)\n\\*" limit t)
	      (when (if re (save-match-data (string-match re (match-string 1)))
		      (equal name (match-string 1)))
		(forward-line 0)
		;; Delete redundant heading.
		(delete-region (match-beginning 0) (point))
		;; Push the entries onto `text'.
		(push
		 (delete-and-extract-region
		  (point)
		  (if (re-search-forward "^[^* \n\t]" nil t)
		      (match-beginning 0)
		    (or limit (point-max))))
		 entries)
		(forward-line 0))))
	  ;; Insert the entries just found.
	  (while (= (line-beginning-position 0) (1- (point)))
	    (backward-char))
	  (dolist (entry (nreverse entries))
	    (insert entry)
	    (while (= (line-beginning-position 0) (1- (point)))
	      (delete-region (1- (point)) (point))))

	  ;; Now remove duplicate entries under the same heading.
	  (let (seen)
	    (save-restriction
	      (narrow-to-region start (point))
	      (goto-char (point-min))
	      (while (re-search-forward "^* \\([^:\n]+:\\(:\\|[^.\n]+\\).\\)" nil 'move)
		;; Fold case straight away; `member-ignore-case' here wasteful.
		(let ((x (downcase (match-string 1))))
		  (if (member x seen)
		      (delete-region
		       (match-beginning 0)
		       (if (re-search-forward "^[^ \t]" nil 'move)
			   (goto-char (match-beginning 0))
			 (point-max)))
		    (push x seen)))))))))))

;; Note that on entry to this function the current-buffer must be the
;; *info* buffer; not the info tags buffer.
(defun Info-read-subfile (nodepos)
  ;; NODEPOS is either a position (in the Info file as a whole,
  ;; not relative to a subfile) or the name of a subfile.
  (let (lastfilepos
	lastfilename)
    (if (numberp nodepos)
	(with-current-buffer (marker-buffer Info-tag-table-marker)
	  (goto-char (point-min))
	  (or (looking-at "\^_")
	      (search-forward "\n\^_"))
	  (forward-line 2)
	  (catch 'foo
	    (while (not (looking-at "\^_"))
	      (if (not (eolp))
		  (let ((beg (point))
			thisfilepos thisfilename)
		    (search-forward ": ")
		    (setq thisfilename  (buffer-substring beg (- (point) 2)))
		    (setq thisfilepos (+ (point-min) (read (current-buffer))))
		    ;; read in version 19 stops at the end of number.
		    ;; Advance to the next line.
		    (forward-line 1)
		    (if (> thisfilepos nodepos)
			(throw 'foo t))
		    (setq lastfilename thisfilename)
		    (setq lastfilepos thisfilepos))
		(forward-line 1)))))
      (setq lastfilename nodepos)
      (setq lastfilepos 0))
    ;; Assume previous buffer is in Info-mode.
    ;; (set-buffer (get-buffer "*info*"))
    (or (equal Info-current-subfile lastfilename)
	(let ((inhibit-read-only t))
	  (setq buffer-file-name nil)
	  (widen)
	  (erase-buffer)
	  (info-insert-file-contents lastfilename)
	  (set-buffer-modified-p nil)
	  (setq Info-current-subfile lastfilename)))
    ;; Widen in case we are in the same subfile as before.
    (widen)
    (goto-char (point-min))
    (if (looking-at "\^_")
	(forward-char 1)
      (search-forward "\n\^_"))
    (if (numberp nodepos)
	(+ (- nodepos lastfilepos) (point)))))

(defun Info-unescape-quotes (value)
  "Unescape double quotes and backslashes in VALUE."
  (let ((start 0)
	(unquote value))
    (while (string-match "[^\\\"]*\\(\\\\\\)[\\\\\"]" unquote start)
      (setq unquote (replace-match "" t t unquote 1))
      (setq start (- (match-end 0) 1)))
    unquote))

;; As of Texinfo 4.6, makeinfo writes constructs like
;;   \0\h[image param=value ...\h\0]
;; into the Info file for handling images.
(defun Info-split-parameter-string (parameter-string)
  "Return alist of (\"KEY\" . \"VALUE\") from PARAMETER-STRING.
PARAMETER-STRING is a whitespace separated list of KEY=VALUE pairs.
If VALUE contains whitespace or double quotes, it must be quoted
in double quotes and any double quotes or backslashes must be
escaped (\\\",\\\\)."
  (let ((start 0)
	(parameter-alist))
    (while (string-match
	    "\\s *\\([^=]+\\)=\\(?:\\([^\\s \"]+\\)\\|\\(?:\"\\(\\(?:[^\\\"]\\|\\\\[\\\\\"]\\)*\\)\"\\)\\)"
	    parameter-string start)
      (setq start (match-end 0))
      (push (cons (match-string 1 parameter-string)
		  (or (match-string 2 parameter-string)
		      (Info-unescape-quotes
		       (match-string 3 parameter-string))))
	    parameter-alist))
    parameter-alist))

(defun Info-display-images-node ()
  "Display images in current node."
  (save-excursion
    (let ((inhibit-read-only t)
	  (case-fold-search t))
      (goto-char (point-min))
      (while (re-search-forward
	      "\\(\0\b[[]image\\(\\(?:[^\b]\\|[^\0]+\b\\)*\\)\0\b[]]\\)"
	      nil t)
	(let* ((start (match-beginning 1))
	       (parameter-alist (Info-split-parameter-string (match-string 2)))
               (src (cdr (assoc-string "src" parameter-alist))))
          (if (display-images-p)
              (let* ((image-file (if src (if (file-name-absolute-p src) src
                                           (concat default-directory src))
                                   ""))
                     (image (if (file-exists-p image-file)
                                (create-image image-file)
                              "[broken image]")))
                (if (not (get-text-property start 'display))
                    (add-text-properties
                     start (point) `(display ,image rear-nonsticky (display)))))
            ;; text-only display, show alternative text if provided, or
            ;; otherwise a clue that there's meant to be a picture
            (delete-region start (point))
            (insert (or (cdr (assoc-string "text" parameter-alist))
                        (cdr (assoc-string "alt" parameter-alist))
                        (and src
                             (concat "[image:" src "]"))
                        "[image]"))))))
    (set-buffer-modified-p nil)))

;; Texinfo 4.7 adds cookies of the form ^@^H[NAME CONTENTS ^@^H].
;; Hide any construct of the general form ^@[^@-^_][ ...  ^@[^@-^_]],
;; including one optional trailing newline.
(defun Info-hide-cookies-node ()
  "Hide unrecognized cookies in current node."
  (save-excursion
    (let ((inhibit-read-only t)
	  (case-fold-search t))
      (goto-char (point-min))
      (while (re-search-forward
	      "\\(\0[\0-\37][[][^\0]*\0[\0-\37][]]\n?\\)"
	      nil t)
	(let* ((start (match-beginning 1)))
	  (if (and (not (get-text-property start 'invisible))
		   (not (get-text-property start 'display)))
	      (put-text-property start (point) 'invisible t)))))
    (set-buffer-modified-p nil)))

(defun Info-select-node ()
  "Select the Info node that point is in."
  ;; Bind this in case the user sets it to nil.
  (let ((case-fold-search t))
    (save-excursion
      ;; Find beginning of node.
      (if (search-backward "\n\^_" nil 'move)
	  (forward-line 2)
	(if (looking-at "\^_")
	    (forward-line 1)
	  (signal 'search-failed (list "\n\^_"))))
      ;; Get nodename spelled as it is in the node.
      (re-search-forward "Node:[ \t]*")
      (setq Info-current-node
	    (buffer-substring-no-properties (point)
					    (progn
					      (skip-chars-forward "^,\t\n")
					      (point))))
      (Info-set-mode-line)
      ;; Find the end of it, and narrow.
      (beginning-of-line)
      (let (active-expression)
	;; Narrow to the node contents
	(narrow-to-region (point)
			  (if (re-search-forward "\n[\^_\f]" nil t)
			      (prog1
				  (1- (point))
				(if (looking-at "[\n\^_\f]*execute: ")
				    (progn
				      (goto-char (match-end 0))
				      (setq active-expression
					    (read (current-buffer))))))
			    (point-max)))
	(if Info-enable-active-nodes (eval active-expression))
	;; Add a new unique history item to full history list
	(let ((new-history (list Info-current-file Info-current-node)))
	  (setq Info-history-list
		(cons new-history (remove new-history Info-history-list)))
	  (setq Info-history-forward nil))
	(if (not (eq Info-fontify-maximum-menu-size nil))
            (Info-fontify-node))
	(Info-display-images-node)
	(Info-hide-cookies-node)
	(run-hooks 'Info-selection-hook)))))

(defvar Info-mode-line-node-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'Info-mouse-scroll-up)
    (define-key map [mode-line mouse-3] 'Info-mouse-scroll-down)
    map)
  "Keymap to put on the Info node name in the mode line.")

(defun Info-set-mode-line ()
  (setq mode-line-buffer-identification
	(nconc (propertized-buffer-identification "%b")
	       (list
		(concat
		 " ("
		 (if (stringp Info-current-file)
		     (replace-regexp-in-string
		      "%" "%%" (file-name-nondirectory Info-current-file))
		   (format "*%S*" Info-current-file))
		 ") "
		 (if Info-current-node
		     (propertize (replace-regexp-in-string
				  "%" "%%" Info-current-node)
				 'face 'mode-line-buffer-id
				 'help-echo
				 "mouse-1: scroll forward, mouse-3: scroll back"
				 'mouse-face 'mode-line-highlight
				 'local-map Info-mode-line-node-keymap)
		   ""))))))

;; Go to an Info node specified with a filename-and-nodename string
;; of the sort that is found in pointers in nodes.

;; Don't autoload this function: the correct entry point for other packages
;; to use is `info'.  --Stef
;; ;;;###autoload
(defun Info-goto-node (nodename &optional fork)
  "Go to Info node named NODENAME.  Give just NODENAME or (FILENAME)NODENAME.
If NODENAME is of the form (FILENAME)NODENAME, the node is in the Info file
FILENAME; otherwise, NODENAME should be in the current Info file (or one of
its sub-files).
Completion is available, but only for node names in the current Info file.
If FORK is non-nil (interactively with a prefix arg), show the node in
a new Info buffer.
If FORK is a string, it is the name to use for the new buffer."
  (interactive (list (Info-read-node-name "Go to node: ") current-prefix-arg))
  (info-initialize)
  (if fork
      (set-buffer
       (clone-buffer (concat "*info-" (if (stringp fork) fork nodename) "*") t)))
  (let (filename)
    (string-match "\\s *\\((\\s *\\([^\t)]*\\)\\s *)\\s *\\|\\)\\(.*\\)"
		  nodename)
    (setq filename (if (= (match-beginning 1) (match-end 1))
		       ""
		     (match-string 2 nodename))
	  nodename (match-string 3 nodename))
    (let ((trim (string-match "\\s +\\'" filename)))
      (if trim (setq filename (substring filename 0 trim))))
    (let ((trim (string-match "\\s +\\'" nodename)))
      (if trim (setq nodename (substring nodename 0 trim))))
    (if transient-mark-mode (deactivate-mark))
    (Info-find-node (if (equal filename "") nil filename)
		    (if (equal nodename "") "Top" nodename))))

(defvar Info-read-node-completion-table)

(defun Info-read-node-name-2 (dirs suffixes string pred action)
  "Internal function used to complete Info node names.
Return a completion table for Info files---the FILENAME part of a
node named \"(FILENAME)NODENAME\".  DIRS is a list of Info
directories to search if FILENAME is not absolute; SUFFIXES is a
list of valid filename suffixes for Info files.  See
`try-completion' for a description of the remaining arguments."
  (setq suffixes (remove "" suffixes))
  (when (file-name-absolute-p string)
    (setq dirs (list (file-name-directory string))))
  (let ((names nil)
        (suffix (concat (regexp-opt suffixes t) "\\'"))
        (string-dir (file-name-directory string)))
    (dolist (dir dirs)
      (unless dir
	(setq dir default-directory))
      (if string-dir (setq dir (expand-file-name string-dir dir)))
      (when (file-directory-p dir)
	(dolist (file (file-name-all-completions
		       (file-name-nondirectory string) dir))
	  ;; If the file name has no suffix or a standard suffix,
	  ;; include it.
	  (and (or (null (file-name-extension file))
		   (string-match suffix file))
	       ;; But exclude subfiles of split Info files.
	       (not (string-match "-[0-9]+\\'" file))
	       ;; And exclude backup files.
	       (not (string-match "~\\'" file))
	       (push (if string-dir (concat string-dir file) file) names))
	  ;; If the file name ends in a standard suffix,
	  ;; add the unsuffixed name as a completion option.
	  (when (string-match suffix file)
	    (setq file (substring file 0 (match-beginning 0)))
	    (push (if string-dir (concat string-dir file) file) names)))))
    (complete-with-action action names string pred)))

(defun Info-read-node-name-1 (string predicate code)
  "Internal function used by `Info-read-node-name'.
See `completing-read' for a description of arguments and usage."
  (cond
   ;; First complete embedded file names.
   ((string-match "\\`([^)]*\\'" string)
    (completion-table-with-context
     "("
     (apply-partially 'completion-table-with-terminator ")"
                      (apply-partially 'Info-read-node-name-2
                                       Info-directory-list
                                       (mapcar 'car Info-suffix-list)))
     (substring string 1)
     predicate
     code))
   ;; If a file name was given, then any node is fair game.
   ((string-match "\\`(" string)
    (cond
     ((eq code nil) string)
     ((eq code t) nil)
     (t t)))
   ;; Otherwise use Info-read-node-completion-table.
   (t (complete-with-action
       code Info-read-node-completion-table string predicate))))

;; Arrange to highlight the proper letters in the completion list buffer.
(defun Info-read-node-name (prompt)
  "Read an Info node name with completion, prompting with PROMPT.
A node name can have the form \"NODENAME\", referring to a node
in the current Info file, or \"(FILENAME)NODENAME\"."
  (let* ((completion-ignore-case t)
	 (Info-read-node-completion-table (Info-build-node-completions))
	 (nodename (completing-read prompt 'Info-read-node-name-1 nil t)))
    (if (equal nodename "")
	(Info-read-node-name prompt)
      nodename)))

(defun Info-build-node-completions ()
  (or Info-current-file-completions
      (let ((compl nil)
	    ;; Bind this in case the user sets it to nil.
	    (case-fold-search t)
	    (node-regexp "Node: *\\([^,\n]*\\) *[,\n\t]"))
	(save-excursion
	  (save-restriction
	    (or Info-tag-table-marker
		(error "No Info tags found"))
	    (if (marker-buffer Info-tag-table-marker)
		(let ((marker Info-tag-table-marker))
		  (set-buffer (marker-buffer marker))
		  (widen)
		  (goto-char marker)
		  (while (re-search-forward "\n\\(Node\\|Ref\\): \\(.*\\)\177" nil t)
		    (setq compl
			  (cons (list (match-string-no-properties 2))
				compl))))
	      (widen)
	      (goto-char (point-min))
	      ;; If the buffer begins with a node header, process that first.
	      (if (Info-node-at-bob-matching node-regexp)
		  (setq compl (list (match-string-no-properties 1))))
	      ;; Now for the rest of the nodes.
	      (while (search-forward "\n\^_" nil t)
		(forward-line 1)
		(let ((beg (point)))
		  (forward-line 1)
		  (if (re-search-backward node-regexp beg t)
		      (setq compl
			    (cons (list (match-string-no-properties 1))
				  compl))))))))
	(setq compl (cons '("*") compl))
	(set (make-local-variable 'Info-current-file-completions) compl))))

(defun Info-restore-point (hl)
  "If this node has been visited, restore the point value when we left."
  (while hl
    (if (and (equal (nth 0 (car hl)) Info-current-file)
	     ;; Use string-equal, not equal, to ignore text props.
	     (string-equal (nth 1 (car hl)) Info-current-node))
	(progn
	  (goto-char (nth 2 (car hl)))
	  (setq hl nil))		;terminate the while at next iter
      (setq hl (cdr hl)))))

(defvar Info-search-history nil
  "The history list for `Info-search'.")

(defvar Info-search-case-fold nil
  "The value of `case-fold-search' from previous `Info-search' command.")

(defun Info-search (regexp &optional bound _noerror _count direction)
  "Search for REGEXP, starting from point, and select node it's found in.
If DIRECTION is `backward', search in the reverse direction."
  (interactive (list (read-string
		      (if Info-search-history
			  (format "Regexp search%s (default %s): "
				  (if case-fold-search "" " case-sensitively")
				  (car Info-search-history))
			(format "Regexp search%s: "
				(if case-fold-search "" " case-sensitively")))
		      nil 'Info-search-history)))
  (deactivate-mark)
  (when (equal regexp "")
    (setq regexp (car Info-search-history)))
  (when regexp
    (let (found beg-found give-up
	  (backward (eq direction 'backward))
	  (onode Info-current-node)
	  (ofile Info-current-file)
	  (opoint (point))
	  (opoint-min (point-min))
	  (opoint-max (point-max))
	  (ostart (window-start))
	  (osubfile Info-current-subfile))
      (setq Info-search-case-fold case-fold-search)
      (save-excursion
	(save-restriction
	  (widen)
	  (when backward
	    ;; Hide Info file header for backward search
	    (narrow-to-region (save-excursion
				(goto-char (point-min))
				(search-forward "\n\^_")
				(1- (point)))
			      (point-max)))
	  (while (and (not give-up)
		      (or (null found)
			  (not (funcall isearch-filter-predicate beg-found found))))
	    (let ((search-spaces-regexp
		   (if (or (not isearch-mode) isearch-regexp)
		       Info-search-whitespace-regexp)))
	      (if (if backward
		      (re-search-backward regexp bound t)
		    (re-search-forward regexp bound t))
		  (setq found (point) beg-found (if backward (match-end 0)
						  (match-beginning 0)))
		(setq give-up t))))))

      (when (and isearch-mode Info-isearch-search
		 (not Info-isearch-initial-node)
		 (not bound)
		 (or give-up (and found (not (and (> found opoint-min)
						  (< found opoint-max))))))
	(signal 'search-failed (list regexp "initial node")))

      ;; If no subfiles, give error now.
      (if give-up
	  (if (null Info-current-subfile)
	      (if isearch-mode
		  (signal 'search-failed (list regexp "end of manual"))
		(let ((search-spaces-regexp
		       (if (or (not isearch-mode) isearch-regexp)
			   Info-search-whitespace-regexp)))
		  (if backward
		      (re-search-backward regexp)
		    (re-search-forward regexp))))
	    (setq found nil)))

      (if (and bound (not found))
	  (signal 'search-failed (list regexp)))

      (unless (or found bound)
	(unwind-protect
	    ;; Try other subfiles.
	    (let ((list ()))
	      (with-current-buffer (marker-buffer Info-tag-table-marker)
		(goto-char (point-min))
		(search-forward "\n\^_\nIndirect:")
		(save-restriction
		  (narrow-to-region (point)
				    (progn (search-forward "\n\^_")
					   (1- (point))))
		  (goto-char (point-min))
		  ;; Find the subfile we just searched.
		  (search-forward (concat "\n" osubfile ": "))
		  ;; Skip that one.
		  (forward-line (if backward 0 1))
		  (if backward (forward-char -1))
		  ;; Make a list of all following subfiles.
		  ;; Each elt has the form (VIRT-POSITION . SUBFILENAME).
		  (while (not (if backward (bobp) (eobp)))
		    (if backward
		        (re-search-backward "\\(^.*\\): [0-9]+$")
		      (re-search-forward "\\(^.*\\): [0-9]+$"))
		    (goto-char (+ (match-end 1) 2))
		    (setq list (cons (cons (+ (point-min)
					      (read (current-buffer)))
					   (match-string-no-properties 1))
				     list))
		    (goto-char (if backward
                                   (1- (match-beginning 0))
                                 (1+ (match-end 0)))))
		  ;; Put in forward order
		  (setq list (nreverse list))))
	      (while list
		(message "Searching subfile %s..." (cdr (car list)))
		(Info-read-subfile (car (car list)))
		(when backward
		  ;; Hide Info file header for backward search
		  (narrow-to-region (save-excursion
				      (goto-char (point-min))
				      (search-forward "\n\^_")
				      (1- (point)))
				    (point-max))
		  (goto-char (point-max)))
		(setq list (cdr list))
		(setq give-up nil found nil)
		(while (and (not give-up)
			    (or (null found)
				(not (funcall isearch-filter-predicate beg-found found))))
		  (let ((search-spaces-regexp
			 (if (or (not isearch-mode) isearch-regexp)
			     Info-search-whitespace-regexp)))
		    (if (if backward
			    (re-search-backward regexp nil t)
			  (re-search-forward regexp nil t))
			(setq found (point) beg-found (if backward (match-end 0)
							(match-beginning 0)))
		      (setq give-up t))))
		(if give-up
		    (setq found nil))
		(if found
		    (setq list nil)))
	      (if found
		  (message "")
		(signal 'search-failed (if isearch-mode
					   (list regexp "end of manual")
					 (list regexp)))))
	  (if (not found)
	      (progn (Info-read-subfile osubfile)
		     (goto-char opoint)
		     (Info-select-node)
		     (set-window-start (selected-window) ostart)))))

      (if (and (string= osubfile Info-current-subfile)
               (> found opoint-min)
               (< found opoint-max))
          ;; Search landed in the same node
          (goto-char found)
        (widen)
        (goto-char found)
        (save-match-data (Info-select-node)))

      ;; Use string-equal, not equal, to ignore text props.
      (or (and (string-equal onode Info-current-node)
	       (equal ofile Info-current-file))
          (and isearch-mode isearch-wrapped
	       (eq opoint (if isearch-forward opoint-min opoint-max)))
	  (setq Info-history (cons (list ofile onode opoint)
				   Info-history))))))

(defun Info-search-case-sensitively ()
  "Search for a regexp case-sensitively."
  (interactive)
  (let ((case-fold-search nil))
    (call-interactively 'Info-search)))

(defun Info-search-next ()
  "Search for next regexp from a previous `Info-search' command."
  (interactive)
  (let ((case-fold-search Info-search-case-fold))
    (if Info-search-history
        (Info-search (car Info-search-history))
      (call-interactively 'Info-search))))

(defun Info-search-backward (regexp &optional bound noerror count)
  "Search for REGEXP in the reverse direction."
  (interactive (list (read-string
		      (if Info-search-history
			  (format "Regexp search%s backward (default %s): "
				  (if case-fold-search "" " case-sensitively")
				  (car Info-search-history))
			(format "Regexp search%s backward: "
				(if case-fold-search "" " case-sensitively")))
		      nil 'Info-search-history)))
  (Info-search regexp bound noerror count 'backward))

(defun Info-isearch-search ()
  (if Info-isearch-search
      (lambda (string &optional bound noerror count)
	(if isearch-word
	    (Info-search (concat "\\b" (replace-regexp-in-string
					"\\W+" "\\W+"
					(replace-regexp-in-string
					 "^\\W+\\|\\W+$" "" string)
					nil t)
				 ;; Lax version of word search
				 (if (or isearch-nonincremental
					 (eq (length string)
					     (length (isearch-string-state
						      (car isearch-cmds)))))
				     "\\b"))
			 bound noerror count
			 (unless isearch-forward 'backward))
	  (Info-search (if isearch-regexp string (regexp-quote string))
		       bound noerror count
		       (unless isearch-forward 'backward)))
	(point))
    (let ((isearch-search-fun-function nil))
      (isearch-search-fun))))

(defun Info-isearch-wrap ()
  (if Info-isearch-search
      (if Info-isearch-initial-node
	  (progn
	    (if isearch-forward (Info-top-node) (Info-final-node))
	    (goto-char (if isearch-forward (point-min) (point-max))))
	(setq Info-isearch-initial-node Info-current-node)
	(setq isearch-wrapped nil))
    (goto-char (if isearch-forward (point-min) (point-max)))))

(defun Info-isearch-push-state ()
  `(lambda (cmd)
     (Info-isearch-pop-state cmd ',Info-current-file ',Info-current-node)))

(defun Info-isearch-pop-state (_cmd file node)
  (or (and (equal Info-current-file file)
           (equal Info-current-node node))
      (progn (Info-find-node file node) (sit-for 0))))

(defun Info-isearch-start ()
  (setq Info-isearch-initial-node
	;; Don't stop at initial node for nonincremental search.
	;; Otherwise this variable is set after first search failure.
	(and isearch-nonincremental Info-current-node))
  (setq Info-isearch-initial-history      Info-history
	Info-isearch-initial-history-list Info-history-list)
  (add-hook 'isearch-mode-end-hook 'Info-isearch-end nil t))

(defun Info-isearch-end ()
  ;; Remove intermediate nodes (visited while searching)
  ;; from the history.  Add only the last node (where Isearch ended).
  (if (> (length Info-history)
	 (length Info-isearch-initial-history))
      (setq Info-history
	    (nthcdr (- (length Info-history)
		       (length Info-isearch-initial-history)
		       1)
		    Info-history)))
  (if (> (length Info-history-list)
	 (length Info-isearch-initial-history-list))
      (setq Info-history-list
	    (cons (car Info-history-list)
		  Info-isearch-initial-history-list)))
  (remove-hook 'isearch-mode-end-hook  'Info-isearch-end t))

(defun Info-isearch-filter (beg-found found)
  "Test whether the current search hit is a visible useful text.
Return non-nil if the text from BEG-FOUND to FOUND is visible
and is not in the header line or a tag table."
  (save-match-data
    (let ((backward (< found beg-found)))
      (not
       (or
	(and (not (eq search-invisible t))
	     (if backward
		 (or (text-property-not-all found beg-found 'invisible nil)
		     (text-property-not-all found beg-found 'display nil))
	       (or (text-property-not-all beg-found found 'invisible nil)
		   (text-property-not-all beg-found found 'display nil))))
	;; Skip node header line
	(and (save-excursion (forward-line -1)
			     (looking-at "\^_"))
	     (forward-line (if backward -1 1)))
	;; Skip Tag Table node
	(save-excursion
	  (and (search-backward "\^_" nil t)
	       (looking-at
		"\^_\n\\(Tag Table\\|Local Variables\\)"))))))))


(defun Info-extract-pointer (name &optional errorname)
  "Extract the value of the node-pointer named NAME.
If there is none, use ERRORNAME in the error message;
if ERRORNAME is nil, just return nil."
  ;; Bind this in case the user sets it to nil.
  (let ((case-fold-search t))
    (save-excursion
      (goto-char (point-min))
      (let ((bound (point)))
	(forward-line 1)
	(cond ((re-search-backward
		(concat name ":" (Info-following-node-name-re)) bound t)
	       (match-string-no-properties 1))
	      ((not (eq errorname t))
	       (error "Node has no %s"
		      (capitalize (or errorname name)))))))))

(defun Info-following-node-name-re (&optional allowedchars)
  "Return a regexp matching a node name.
ALLOWEDCHARS, if non-nil, goes within [...] to make a regexp
saying which chars may appear in the node name.
Submatch 1 is the complete node name.
Submatch 2 if non-nil is the parenthesized file name part of the node name.
Submatch 3 is the local part of the node name.
End of submatch 0, 1, and 3 are the same, so you can safely concat."
  (concat "[ \t\n]*"			;Skip leading space.
	  "\\(\\(([^)]+)\\)?"	;Node name can start with a file name.
	  "\\([" (or allowedchars "^,\t\n") "]*" ;Any number of allowed chars.
	  "[" (or allowedchars "^,\t\n") " ]" ;The last char can't be a space.
	  "\\|\\)\\)"))			      ;Allow empty node names.

;;; For compatibility; other files have used this name.
(defun Info-following-node-name ()
  (and (looking-at (Info-following-node-name-re))
       (match-string-no-properties 1)))

(defun Info-next ()
  "Go to the next node of this node."
  (interactive)
  ;; In case another window is currently selected
  (save-window-excursion
    (or (eq major-mode 'Info-mode) (switch-to-buffer "*info*"))
    (Info-goto-node (Info-extract-pointer "next"))))

(defun Info-prev ()
  "Go to the previous node of this node."
  (interactive)
  ;; In case another window is currently selected
  (save-window-excursion
    (or (eq major-mode 'Info-mode) (switch-to-buffer "*info*"))
    (Info-goto-node (Info-extract-pointer "prev[ious]*" "previous"))))

(defun Info-up (&optional same-file)
  "Go to the superior node of this node.
If SAME-FILE is non-nil, do not move to a different Info file."
  (interactive)
  ;; In case another window is currently selected
  (save-window-excursion
    (or (eq major-mode 'Info-mode) (switch-to-buffer "*info*"))
    (let ((old-node Info-current-node)
	  (old-file Info-current-file)
	  (node (Info-extract-pointer "up")) p)
      (and same-file
	   (string-match "^(" node)
	   (error "Up node is in another Info file"))
      (Info-goto-node node)
      (setq p (point))
      (goto-char (point-min))
      (if (and (stringp old-file)
	       (search-forward "\n* Menu:" nil t)
	       (re-search-forward
		(if (string-equal old-node "Top")
		    (concat "\n\\*[^:]+: +(" (file-name-nondirectory old-file) ")")
		  (concat "\n\\* +\\(" (regexp-quote old-node)
			  ":\\|[^:]+: +" (regexp-quote old-node) "\\)"))
		nil t))
	  (progn (beginning-of-line) (if (looking-at "^\\* ") (forward-char 2)))
	(goto-char p)
	(Info-restore-point Info-history)))))

(defun Info-history-back ()
  "Go back in the history to the last node visited."
  (interactive)
  (or Info-history
      (error "This is the first Info node you looked at"))
  (let ((history-forward
	 (cons (list Info-current-file Info-current-node (point))
	       Info-history-forward))
	filename nodename opoint)
    (setq filename (car (car Info-history)))
    (setq nodename (car (cdr (car Info-history))))
    (setq opoint (car (cdr (cdr (car Info-history)))))
    (setq Info-history (cdr Info-history))
    (Info-find-node filename nodename)
    (setq Info-history (cdr Info-history))
    (setq Info-history-forward history-forward)
    (goto-char opoint)))

(defalias 'Info-last 'Info-history-back)

(defun Info-history-forward ()
  "Go forward in the history of visited nodes."
  (interactive)
  (or Info-history-forward
      (error "This is the last Info node you looked at"))
  (let ((history-forward (cdr Info-history-forward))
	filename nodename opoint)
    (setq filename (car (car Info-history-forward)))
    (setq nodename (car (cdr (car Info-history-forward))))
    (setq opoint (car (cdr (cdr (car Info-history-forward)))))
    (Info-find-node filename nodename)
    (setq Info-history-forward history-forward)
    (goto-char opoint)))

(add-to-list 'Info-virtual-files
	     '("\\`dir\\'"
	       (toc-nodes . Info-directory-toc-nodes)
	       (find-file . Info-directory-find-file)
	       (find-node . Info-directory-find-node)
	       ))

(defun Info-directory-toc-nodes (filename)
  "Directory-specific implementation of `Info-toc-nodes'."
  `(,filename
    ("Top" nil nil nil)))

(defun Info-directory-find-file (filename &optional _noerror)
  "Directory-specific implementation of `Info-find-file'."
  filename)

(defun Info-directory-find-node (_filename _nodename &optional _no-going-back)
  "Directory-specific implementation of `Info-find-node-2'."
  (Info-insert-dir))

;;;###autoload
(defun Info-directory ()
  "Go to the Info directory node."
  (interactive)
  (Info-find-node "dir" "top"))

(add-to-list 'Info-virtual-files
	     '("\\`\\*History\\*\\'"
	       (toc-nodes . Info-history-toc-nodes)
	       (find-file . Info-history-find-file)
	       (find-node . Info-history-find-node)
	       ))

(defun Info-history-toc-nodes (filename)
  "History-specific implementation of `Info-toc-nodes'."
  `(,filename
    ("Top" nil nil nil)))

(defun Info-history-find-file (filename &optional _noerror)
  "History-specific implementation of `Info-find-file'."
  filename)

(defun Info-history-find-node (filename nodename &optional _no-going-back)
  "History-specific implementation of `Info-find-node-2'."
  (insert (format "\n\^_\nFile: %s,  Node: %s,  Up: (dir)\n\n"
		  (or filename Info-current-file) nodename))
  (insert "Recently Visited Nodes\n")
  (insert "**********************\n\n")
  (insert "* Menu:\n\n")
  (let ((hl (remove '("*History*" "Top") Info-history-list)))
    (while hl
      (let ((file (nth 0 (car hl)))
	    (node (nth 1 (car hl))))
	(if (stringp file)
	    (insert "* " node ": ("
		    (propertize (or (file-name-directory file) "") 'invisible t)
		    (file-name-nondirectory file)
		    ")" node ".\n")))
      (setq hl (cdr hl)))))

(defun Info-history ()
  "Go to a node with a menu of visited nodes."
  (interactive)
  (Info-find-node "*History*" "Top")
  (Info-next-reference)
  (Info-next-reference))

(add-to-list 'Info-virtual-nodes
	     '("\\`\\*TOC\\*\\'"
	       (find-node . Info-toc-find-node)
	       ))

(defun Info-toc-find-node (filename nodename &optional _no-going-back)
  "Toc-specific implementation of `Info-find-node-2'."
  (let* ((curr-file (substring-no-properties (or filename Info-current-file)))
	 (curr-node (substring-no-properties (or nodename Info-current-node)))
	 (node-list (Info-toc-nodes curr-file)))
    (insert (format "\n\^_\nFile: %s,  Node: %s,  Up: Top\n\n"
		    curr-file curr-node))
    (insert "Table of Contents\n")
    (insert "*****************\n\n")
    (insert "*Note Top::\n")
    (Info-toc-insert
     (nth 3 (assoc "Top" node-list))	; get Top nodes
     node-list 0 curr-file)
    (unless (bobp)
      (let ((Info-hide-note-references 'hide)
	    (Info-fontify-visited-nodes nil))
	(setq Info-current-file filename Info-current-node "*TOC*")
	(goto-char (point-min))
	(narrow-to-region (or (re-search-forward "\n[\^_\f]\n" nil t)
			      (point-min))
			  (point-max))
	(Info-fontify-node)
	(widen)))))

(defun Info-toc ()
  "Go to a node with table of contents of the current Info file.
Table of contents is created from the tree structure of menus."
  (interactive)
  (Info-find-node Info-current-file "*TOC*")
  (let ((prev-node (nth 1 (car Info-history))) p)
    (goto-char (point-min))
    (if (setq p (search-forward (concat "*Note " prev-node ":") nil t))
	(setq p (- p (length prev-node) 2)))
    (goto-char (or p (point-min)))))

(defun Info-toc-insert (nodes node-list level curr-file)
  "Insert table of contents with references to nodes."
  (let ((section "Top"))
    (while nodes
      (let ((node (assoc (car nodes) node-list)))
        (unless (member (nth 2 node) (list nil section))
          (insert (setq section (nth 2 node)) "\n"))
        (insert (make-string level ?\t))
        (insert "*Note " (car nodes) ":: \n")
        (Info-toc-insert (nth 3 node) node-list (1+ level) curr-file)
        (setq nodes (cdr nodes))))))

(defun Info-toc-build (file)
  "Build table of contents from menus of Info FILE and its subfiles."
  (with-temp-buffer
    (let* ((file (and (stringp file) (Info-find-file file)))
           (default-directory (or (and (stringp file)
                                       (file-name-directory file))
                                  default-directory))
           (main-file (and (stringp file) file))
           (sections '(("Top" "Top")))
           nodes subfiles)
      (while (or main-file subfiles)
        ;; (or main-file (message "Searching subfile %s..." (car subfiles)))
        (erase-buffer)
        (info-insert-file-contents (or main-file (car subfiles)))
        (goto-char (point-min))
        (while (and (search-forward "\n\^_\nFile:" nil 'move)
                    (search-forward "Node: " nil 'move))
          (let* ((nodename (substring-no-properties (Info-following-node-name)))
		 (bound (- (or (save-excursion (search-forward "\n\^_" nil t))
			       (point-max)) 2))
		 (upnode (and (re-search-forward
			       (concat "Up:" (Info-following-node-name-re))
			       bound t)
			      (match-string-no-properties 1)))
		 (section "Top")
		 menu-items)
	    (when (and upnode (string-match "(" upnode)) (setq upnode nil))
            (when (and (not (Info-index-node nodename file))
                       (re-search-forward "^\\* Menu:" bound t))
              (forward-line 1)
              (beginning-of-line)
              (setq bound (or (and (equal nodename "Top")
                                   (save-excursion
                                     (re-search-forward
                                      "^[ \t-]*The Detailed Node Listing" nil t)))
                              bound))
              (while (< (point) bound)
                (cond
                 ;; Menu item line
                 ((looking-at "^\\* +[^:]+:")
                  (beginning-of-line)
                  (forward-char 2)
                  (let ((menu-node-name (substring-no-properties
                                         (Info-extract-menu-node-name))))
                    (setq menu-items (cons menu-node-name menu-items))
                    (if (equal nodename "Top")
                        (setq sections
                              (cons (list menu-node-name section) sections)))))
                 ;; Other non-empty strings in the Top node are section names
                 ((and (equal nodename "Top")
                       (looking-at "^\\([^ \t\n*=.-][^:\n]*\\)"))
                  (setq section (match-string-no-properties 1))))
                (forward-line 1)
                (beginning-of-line)))
            (setq nodes (cons (list nodename upnode
                                    (cadr (assoc nodename sections))
                                    (nreverse menu-items))
                              nodes))
            (goto-char bound)))
        (if main-file
            (save-excursion
              (goto-char (point-min))
              (if (search-forward "\n\^_\nIndirect:" nil t)
                  (let ((bound (save-excursion (search-forward "\n\^_" nil t))))
                    (while (re-search-forward "^\\(.*\\): [0-9]+$" bound t)
                      (setq subfiles (cons (match-string-no-properties 1)
                                           subfiles)))))
              (setq subfiles (nreverse subfiles)
                    main-file nil))
          (setq subfiles (cdr subfiles))))
      (message "")
      (nreverse nodes))))

(defvar Info-toc-nodes nil
  "Alist of cached parent-children node information in visited Info files.
Each element is (FILE (NODE-NAME PARENT SECTION CHILDREN) ...)
where PARENT is the parent node extracted from the Up pointer,
SECTION is the section name in the Top node where this node is placed,
CHILDREN is a list of child nodes extracted from the node menu.")

(defun Info-toc-nodes (filename)
  "Return a node list of Info FILENAME with parent-children information.
This information is cached in the variable `Info-toc-nodes' with the help
of the function `Info-toc-build'."
  (cond
   ((Info-virtual-call
     (Info-virtual-fun 'toc-nodes (or filename Info-current-file) nil)
     filename))
   (t
    (or filename (setq filename Info-current-file))
    (or (assoc filename Info-toc-nodes)
	;; Skip virtual Info files
	(and (or (not (stringp filename))
		 (Info-virtual-file-p filename))
	     (push (cons filename nil) Info-toc-nodes))
	;; Scan the entire manual and cache the result in Info-toc-nodes
	(let ((nodes (Info-toc-build filename)))
	  (push (cons filename nodes) Info-toc-nodes)
	  nodes)
	;; If there is an error, still add nil to the cache
	(push (cons filename nil) Info-toc-nodes))
    (cdr (assoc filename Info-toc-nodes)))))


(defun Info-follow-reference (footnotename &optional fork)
  "Follow cross reference named FOOTNOTENAME to the node it refers to.
FOOTNOTENAME may be an abbreviation of the reference name.
If FORK is non-nil (interactively with a prefix arg), show the node in
a new Info buffer.  If FORK is a string, it is the name to use for the
new buffer."
  (interactive
   (let ((completion-ignore-case t)
	 (case-fold-search t)
	 completions default alt-default (start-point (point)) str i bol eol)
     (save-excursion
       ;; Store end and beginning of line.
       (setq eol (line-end-position)
             bol (line-beginning-position))
       (goto-char (point-min))
       (while (re-search-forward "\\*note[ \n\t]+\\([^:]*\\):" nil t)
	 (setq str (match-string-no-properties 1))
	 ;; See if this one should be the default.
	 (and (null default)
	      (<= (match-beginning 0) start-point)
	      (<= start-point (point))
	      (setq default t))
	 ;; See if this one should be the alternate default.
	 (and (null alt-default)
	      (and (<= bol (match-beginning 0))
		   (<= (point) eol))
	      (setq alt-default t))
	 (setq i 0)
	 (while (setq i (string-match "[ \n\t]+" str i))
	   (setq str (concat (substring str 0 i) " "
			     (substring str (match-end 0))))
	   (setq i (1+ i)))
	 ;; Record as a completion and perhaps as default.
	 (if (eq default t) (setq default str))
	 (if (eq alt-default t) (setq alt-default str))
	 ;; Don't add this string if it's a duplicate.
	 (or (assoc-string str completions t)
	     (push str completions))))
     ;; If no good default was found, try an alternate.
     (or default
	 (setq default alt-default))
     ;; If only one cross-reference found, then make it default.
     (if (eq (length completions) 1)
         (setq default (car completions)))
     (if completions
	 (let ((input (completing-read (if default
					   (concat
					    "Follow reference named (default "
					    default "): ")
					 "Follow reference named: ")
				       completions nil t)))
	   (list (if (equal input "")
		     default input) current-prefix-arg))
       (error "No cross-references in this node"))))

  (unless footnotename
    (error "No reference was specified"))

  (let (target i (str (concat "\\*note " (regexp-quote footnotename)))
	       (case-fold-search t))
    (while (setq i (string-match " " str i))
      (setq str (concat (substring str 0 i) "[ \t\n]+" (substring str (1+ i))))
      (setq i (+ i 6)))
    (save-excursion
      ;; Move point to the beginning of reference if point is on reference
      (or (looking-at "\\*note[ \n\t]+")
          (and (looking-back "\\*note[ \n\t]+")
               (goto-char (match-beginning 0)))
          (if (and (save-excursion
                     (goto-char (+ (point) 5)) ; skip a possible *note
                     (re-search-backward "\\*note[ \n\t]+" nil t)
                     (looking-at str))
                   (<= (point) (match-end 0)))
              (goto-char (match-beginning 0))))
      ;; Go to the reference closest to point
      (let ((next-ref (save-excursion (and (re-search-forward str nil t)
                                           (+ (match-beginning 0) 5))))
            (prev-ref (save-excursion (and (re-search-backward str nil t)
                                           (+ (match-beginning 0) 5)))))
        (goto-char (cond ((and next-ref prev-ref)
                          (if (< (abs (- next-ref (point)))
                                 (abs (- prev-ref (point))))
                              next-ref prev-ref))
                         ((or next-ref prev-ref))
                         ((error "No cross-reference named %s" footnotename))))
        (setq target (Info-extract-menu-node-name t))))
    (while (setq i (string-match "[ \t\n]+" target i))
      (setq target (concat (substring target 0 i) " "
			   (substring target (match-end 0))))
      (setq i (+ i 1)))
    (Info-goto-node target fork)))

(defconst Info-menu-entry-name-re "\\(?:[^:]\\|:[^:,.;() \t\n]\\)*"
  ;; We allow newline because this is also used in Info-follow-reference,
  ;; where the xref name might be wrapped over two lines.
  "Regexp that matches a menu entry name upto but not including the colon.
Because of ambiguities, this should be concatenated with something like
`:' and `Info-following-node-name-re'.")

(defun Info-extract-menu-node-name (&optional multi-line index-node)
  (skip-chars-forward " \t\n")
  (when (looking-at (concat Info-menu-entry-name-re ":\\(:\\|"
			    (Info-following-node-name-re
                             (cond
                              (index-node "^,\t\n")
                              (multi-line "^.,\t")
                              (t          "^.,\t\n")))
                            "\\)"
                            (if index-node
                                "\\.\\(?:[ \t\n]+(line +\\([0-9]+\\))\\)?"
                              "")))
    (if index-node
        (setq Info-point-loc
              (if (match-beginning 5)
                  (string-to-number (match-string 5))
                (buffer-substring-no-properties
		 (match-beginning 0) (1- (match-beginning 1)))))
;;; Uncomment next line to use names of cross-references in non-index nodes:
;;;       (setq Info-point-loc
;;;             (buffer-substring (match-beginning 0) (1- (match-beginning 1))))
      )
    (replace-regexp-in-string
     "[ \n]+" " "
     (or (and (not (equal (match-string-no-properties 2) ""))
	      (match-string-no-properties 2))
	 ;; If the node name is the menu entry name (using `entry::').
	 (buffer-substring-no-properties
	  (match-beginning 0) (1- (match-beginning 1)))))))

;; No one calls this.
;;(defun Info-menu-item-sequence (list)
;;  (while list
;;    (Info-menu (car list))
;;    (setq list (cdr list))))

(defvar Info-complete-menu-buffer)
(defvar Info-complete-next-re nil)
(defvar Info-complete-nodes nil)
(defvar Info-complete-cache nil)

(defconst Info-node-spec-re
  (concat (Info-following-node-name-re "^.,:") "[,:.]")
  "Regexp to match the text after a : until the terminating `.'.")

(defun Info-complete-menu-item (string predicate action)
  ;; This uses two dynamically bound variables:
  ;; - `Info-complete-menu-buffer' which contains the buffer in which
  ;; is the menu of items we're trying to complete.
  ;; - `Info-complete-next-re' which, if non-nil, indicates that we should
  ;; also look for menu items in subsequent nodes as long as those
  ;; nodes' names match `Info-complete-next-re'.  This feature is currently
  ;; not used.
  ;; - `Info-complete-nodes' which, if non-nil, indicates that we should
  ;; also look for menu items in these nodes.  This feature is currently
  ;; only used for completion in Info-index.

  ;; Note that `Info-complete-menu-buffer' could be current already,
  ;; so we want to save point.
  (with-current-buffer Info-complete-menu-buffer
    (save-excursion
      (let ((completion-ignore-case t)
            (case-fold-search t)
            (orignode Info-current-node)
            nextnode)
        (goto-char (point-min))
        (search-forward "\n* Menu:")
        (cond
         ((eq (car-safe action) 'boundaries) nil)
         ((eq action 'lambda)
          (re-search-forward
           (concat "\n\\* +" (regexp-quote string) ":") nil t))
         (t
          (let ((pattern (concat "\n\\* +\\("
                                 (regexp-quote string)
                                 Info-menu-entry-name-re "\\):"
                                 Info-node-spec-re))
                completions
                (complete-nodes Info-complete-nodes))
            ;; Check the cache.
            (if (and (equal (nth 0 Info-complete-cache) Info-current-file)
                     (equal (nth 1 Info-complete-cache) Info-current-node)
                     (equal (nth 2 Info-complete-cache) Info-complete-next-re)
                     (equal (nth 5 Info-complete-cache) Info-complete-nodes)
                     (let ((prev (nth 3 Info-complete-cache)))
                       (eq t (compare-strings string 0 (length prev)
                                              prev 0 nil t))))
                ;; We can reuse the previous list.
                (setq completions (nth 4 Info-complete-cache))
              ;; The cache can't be used.
              (while
                  (progn
                    (while (re-search-forward pattern nil t)
                      (push (match-string-no-properties 1)
                            completions))
                    ;; Check subsequent nodes if applicable.
                    (or (and Info-complete-next-re
                             (setq nextnode (Info-extract-pointer "next" t))
                             (string-match Info-complete-next-re nextnode))
                        (and complete-nodes
                             (setq complete-nodes (cdr complete-nodes)
                                   nextnode (car complete-nodes)))))
                (Info-goto-node nextnode))
              ;; Go back to the start node (for the next completion).
              (unless (equal Info-current-node orignode)
                (Info-goto-node orignode))
              ;; Update the cache.
              (set (make-local-variable 'Info-complete-cache)
		   (list Info-current-file Info-current-node
			 Info-complete-next-re string completions
			 Info-complete-nodes)))
            (complete-with-action action completions string predicate))))))))


(defun Info-menu (menu-item &optional fork)
  "Go to the node pointed to by the menu item named (or abbreviated) MENU-ITEM.
The menu item should one of those listed in the current node's menu.
Completion is allowed, and the default menu item is the one point is on.
If FORK is non-nil (interactively with a prefix arg), show the node in
a new Info buffer.  If FORK is a string, it is the name to use for the
new buffer."
  (interactive
   (let (;; If point is within a menu item, use that item as the default
	 (default nil)
	 (p (point))
	 beg
	 (case-fold-search t))
     (save-excursion
       (goto-char (point-min))
       (if (not (search-forward "\n* menu:" nil t))
	   (error "No menu in this node"))
       (setq beg (point))
       (and (< (point) p)
	    (save-excursion
	      (goto-char p)
	      (end-of-line)
	      (if (re-search-backward (concat "\n\\* +\\("
					      Info-menu-entry-name-re
					      "\\):") beg t)
		  (setq default (match-string-no-properties 1))))))
     (let ((item nil))
       (while (null item)
	 (setq item (let ((completion-ignore-case t)
			  (Info-complete-menu-buffer (current-buffer)))
		      (completing-read (if default
					   (format "Menu item (default %s): "
						   default)
					 "Menu item: ")
				       'Info-complete-menu-item nil t)))
	 ;; we rely on the fact that completing-read accepts an input
	 ;; of "" even when the require-match argument is true and ""
	 ;; is not a valid possibility
	 (if (string= item "")
	     (if default
		 (setq item default)
	       ;; ask again
	       (setq item nil))))
       (list item current-prefix-arg))))
  ;; there is a problem here in that if several menu items have the same
  ;; name you can only go to the node of the first with this command.
  (Info-goto-node (Info-extract-menu-item menu-item)
		  (and fork
		       (if (stringp fork) fork menu-item))))

(defun Info-extract-menu-item (menu-item)
  (setq menu-item (regexp-quote menu-item))
  (let ((case-fold-search t))
    (save-excursion
      (let ((case-fold-search t))
	(goto-char (point-min))
	(or (search-forward "\n* menu:" nil t)
	    (error "No menu in this node"))
	(or (re-search-forward (concat "\n\\* +" menu-item ":") nil t)
	    (re-search-forward (concat "\n\\* +" menu-item) nil t)
	    (error "No such item in menu"))
	(beginning-of-line)
	(forward-char 2)
	(Info-extract-menu-node-name nil (Info-index-node))))))

;; If COUNT is nil, use the last item in the menu.
(defun Info-extract-menu-counting (count &optional no-detail)
  (let ((case-fold-search t))
    (save-excursion
      (let ((case-fold-search t)
	    (bound (when (and no-detail
			      (re-search-forward
			       "^[ \t-]*The Detailed Node Listing" nil t))
		     (match-beginning 0))))
	(goto-char (point-min))
	(or (search-forward "\n* menu:" bound t)
	    (error "No menu in this node"))
	(if count
	    (or (search-forward "\n* " bound t count)
		(error "Too few items in menu"))
	  (while (search-forward "\n* " bound t)
	    nil))
	(Info-extract-menu-node-name nil (Info-index-node))))))

(defun Info-nth-menu-item ()
  "Go to the node of the Nth menu item.
N is the digit argument used to invoke this command."
  (interactive)
  (Info-goto-node
   (Info-extract-menu-counting
    (- (aref (this-command-keys) (1- (length (this-command-keys)))) ?0))))

(defun Info-top-node ()
  "Go to the Top node of this file."
  (interactive)
  (Info-goto-node "Top"))

(defun Info-final-node ()
  "Go to the final node in this file."
  (interactive)
  (Info-goto-node "Top")
  (let ((Info-history nil)
	(case-fold-search t))
    ;; Go to the last node in the menu of Top.  But don't delve into
    ;; detailed node listings.
    (Info-goto-node (Info-extract-menu-counting nil t))
    ;; If the last node in the menu is not last in pointer structure,
    ;; move forward (but not down- or upward - see bug#1116) until we
    ;; can't go any farther.
    (while (Info-forward-node t t t) nil)
    ;; Then keep moving down to last subnode, unless we reach an index.
    (while (and (not (Info-index-node))
		(save-excursion (search-forward "\n* Menu:" nil t)))
      (Info-goto-node (Info-extract-menu-counting nil)))))

(defun Info-forward-node (&optional not-down not-up no-error)
  "Go forward one node, considering all nodes as forming one sequence."
  (interactive)
  (goto-char (point-min))
  (forward-line 1)
  (let ((case-fold-search t))
    ;; three possibilities, in order of priority:
    ;;     1. next node is in a menu in this node (but not in an index)
    ;;     2. next node is next at same level
    ;;     3. next node is up and next
    (cond ((and (not not-down)
		(save-excursion (search-forward "\n* menu:" nil t))
		(not (Info-index-node)))
	   (Info-goto-node (Info-extract-menu-counting 1))
	   t)
	  ((save-excursion (search-backward "next:" nil t))
	   (Info-next)
	   t)
	  ((and (not not-up)
		(save-excursion (search-backward "up:" nil t))
		;; Use string-equal, not equal, to ignore text props.
		(not (string-equal (downcase (Info-extract-pointer "up"))
				   "top")))
	   (let ((old-node Info-current-node))
	     (Info-up)
	     (let ((old-history Info-history)
		   success)
	       (unwind-protect
		   (setq success (Info-forward-node t nil no-error))
		 (or success (Info-goto-node old-node)))
	       (if Info-history-skip-intermediate-nodes
		   (setq Info-history old-history)))))
	  (no-error nil)
	  (t (error "No pointer forward from this node")))))

(defun Info-backward-node ()
  "Go backward one node, considering all nodes as forming one sequence."
  (interactive)
  (let ((prevnode (Info-extract-pointer "prev[ious]*" t))
	(upnode (Info-extract-pointer "up" t))
	(case-fold-search t))
    (cond ((and upnode (string-match "(" upnode))
	   (error "First node in file"))
	  ((and upnode (or (null prevnode)
			   ;; Use string-equal, not equal,
			   ;; to ignore text properties.
			   (string-equal (downcase prevnode)
					 (downcase upnode))))
	   (Info-up))
	  (prevnode
	   ;; If we move back at the same level,
	   ;; go down to find the last subnode*.
	   (Info-prev)
	   (let ((old-history Info-history))
	     (while (and (not (Info-index-node))
			 (save-excursion (search-forward "\n* Menu:" nil t)))
	       (Info-goto-node (Info-extract-menu-counting nil)))
	     (if Info-history-skip-intermediate-nodes
		 (setq Info-history old-history))))
	  (t
	   (error "No pointer backward from this node")))))

(defun Info-exit ()
  "Exit Info by selecting some other buffer."
  (interactive)
  (if Info-standalone
      (save-buffers-kill-emacs)
    (quit-window)))

(defun Info-next-menu-item ()
  "Go to the node of the next menu item."
  (interactive)
  ;; Bind this in case the user sets it to nil.
  (let* ((case-fold-search t)
	 (node
	  (save-excursion
	    (forward-line -1)
	    (search-forward "\n* menu:" nil t)
	    (and (search-forward "\n* " nil t)
		 (Info-extract-menu-node-name)))))
    (if node (Info-goto-node node)
      (error "No more items in menu"))))

(defun Info-last-menu-item ()
  "Go to the node of the previous menu item."
  (interactive)
  (save-excursion
    (forward-line 1)
    ;; Bind this in case the user sets it to nil.
    (let* ((case-fold-search t)
	   (beg (save-excursion
		  (and (search-backward "\n* menu:" nil t)
		       (point)))))
      (or (and beg (search-backward "\n* " beg t))
	  (error "No previous items in menu")))
    (Info-goto-node (save-excursion
		      (goto-char (match-end 0))
		      (Info-extract-menu-node-name)))))

(defmacro Info-no-error (&rest body)
  (list 'condition-case nil (cons 'progn (append body '(t))) '(error nil)))

(defun Info-next-preorder ()
  "Go to the next subnode or the next node, or go up a level."
  (interactive)
  (cond ((Info-no-error (Info-next-menu-item)))
	((Info-no-error (Info-next)))
	((Info-no-error (Info-up t))
	 ;; Since we have already gone thru all the items in this menu,
	 ;; go up to the end of this node.
	 (goto-char (point-max))
	 ;; Since logically we are done with the node with that menu,
	 ;; move on from it.  But don't add intermediate nodes
	 ;; to the history on recursive calls.
	 (let ((old-history Info-history))
	   (Info-next-preorder)
	   (if Info-history-skip-intermediate-nodes
	       (setq Info-history old-history))))
	(t
	 (error "No more nodes"))))

(defun Info-last-preorder ()
  "Go to the last node, popping up a level if there is none."
  (interactive)
  (cond ((and Info-scroll-prefer-subnodes
	      (Info-no-error
	       (Info-last-menu-item)
	       ;; If we go down a menu item, go to the end of the node
	       ;; so we can scroll back through it.
	       (goto-char (point-max))))
	 ;; Keep going down, as long as there are nested menu nodes.
	 (let ((old-history Info-history))
	   (while (Info-no-error
		   (Info-last-menu-item)
		   ;; If we go down a menu item, go to the end of the node
		   ;; so we can scroll back through it.
		   (goto-char (point-max))))
	   (if Info-history-skip-intermediate-nodes
	       (setq Info-history old-history)))
	 (recenter -1))
	((and (Info-no-error (Info-extract-pointer "prev"))
	      (not (equal (Info-extract-pointer "up")
			  (Info-extract-pointer "prev"))))
	 (Info-no-error (Info-prev))
	 (goto-char (point-max))
	 (let ((old-history Info-history))
	   (while (Info-no-error
		   (Info-last-menu-item)
		   ;; If we go down a menu item, go to the end of the node
		   ;; so we can scroll back through it.
		   (goto-char (point-max))))
	   (if Info-history-skip-intermediate-nodes
	       (setq Info-history old-history)))
	 (recenter -1))
	((Info-no-error (Info-up t))
	 (goto-char (point-min))
	 (let ((case-fold-search t))
	   (or (search-forward "\n* Menu:" nil t)
	       (goto-char (point-max)))))
	(t (error "No previous nodes"))))

(defun Info-scroll-up ()
  "Scroll one screenful forward in Info, considering all nodes as one sequence.
Once you scroll far enough in a node that its menu appears on the screen
but after point, the next scroll moves into its first subnode, unless
`Info-scroll-prefer-subnodes' is nil.

When you scroll past the end of a node, that goes to the next node if
`Info-scroll-prefer-subnodes' is non-nil and to the first subnode otherwise;
if this node has no successor, it moves to the parent node's successor,
and so on.  If `Info-scroll-prefer-subnodes' is non-nil and point is inside
the menu of a node, it moves to subnode indicated by the following menu
item.  (That case won't normally result from this command, but can happen
in other ways.)"

  (interactive)
  (if (or (< (window-start) (point-min))
	  (> (window-start) (point-max)))
      (set-window-start (selected-window) (point)))
  (let* ((case-fold-search t)
	 (virtual-end (save-excursion
			(goto-char (point-min))
			(if (and Info-scroll-prefer-subnodes
				 (search-forward "\n* Menu:" nil t))
			    (point)
			  (point-max)))))
    (if (or (< virtual-end (window-start))
	    (pos-visible-in-window-p virtual-end))
	(cond
	 (Info-scroll-prefer-subnodes (Info-next-preorder))
	 ((Info-no-error (Info-goto-node (Info-extract-menu-counting 1))))
	 (t (Info-next-preorder)))
      (scroll-up))))

(defun Info-mouse-scroll-up (e)
  "Scroll one screenful forward in Info, using the mouse.
See `Info-scroll-up'."
  (interactive "e")
  (save-selected-window
    (if (eventp e)
	(select-window (posn-window (event-start e))))
    (Info-scroll-up)))

(defun Info-scroll-down ()
  "Scroll one screenful back in Info, considering all nodes as one sequence.
If point is within the menu of a node, and `Info-scroll-prefer-subnodes'
is non-nil, this goes to its last subnode.  When you scroll past the
beginning of a node, that goes to the previous node or back up to the
parent node."
  (interactive)
  (if (or (< (window-start) (point-min))
	  (> (window-start) (point-max)))
      (set-window-start (selected-window) (point)))
  (let* ((case-fold-search t)
	 (current-point (point))
	 (virtual-end
	  (and Info-scroll-prefer-subnodes
	       (save-excursion
		 (setq current-point (line-beginning-position))
		 (goto-char (point-min))
		 (search-forward "\n* Menu:" current-point t)))))
    (if (or virtual-end
	    (pos-visible-in-window-p (point-min) nil t))
	(Info-last-preorder)
      (scroll-down))))

(defun Info-mouse-scroll-down (e)
  "Scroll one screenful backward in Info, using the mouse.
See `Info-scroll-down'."
  (interactive "e")
  (save-selected-window
    (if (eventp e)
	(select-window (posn-window (event-start e))))
    (Info-scroll-down)))

(defun Info-next-reference (&optional recur)
  "Move cursor to the next cross-reference or menu item in the node."
  (interactive)
  (let ((pat "\\*note[ \n\t]+\\([^:]+\\):\\|^\\* .*:\\|[hf]t?tps?://")
	(old-pt (point))
	(case-fold-search t))
    (or (eobp) (forward-char 1))
    (or (re-search-forward pat nil t)
	(progn
	  (goto-char (point-min))
	  (or (re-search-forward pat nil t)
	      (progn
		(goto-char old-pt)
		(error "No cross references in this node")))))
    (goto-char (or (match-beginning 1) (match-beginning 0)))
    (if (looking-at "\\* Menu:")
	(if recur
	    (error "No cross references in this node")
	  (Info-next-reference t))
      (if (looking-at "^\\* ")
	  (forward-char 2)))))

(defun Info-prev-reference (&optional recur)
  "Move cursor to the previous cross-reference or menu item in the node."
  (interactive)
  (let ((pat "\\*note[ \n\t]+\\([^:]+\\):\\|^\\* .*:\\|[hf]t?tps?://")
	(old-pt (point))
	(case-fold-search t))
    (or (re-search-backward pat nil t)
	(progn
	  (goto-char (point-max))
	  (or (re-search-backward pat nil t)
	      (progn
		(goto-char old-pt)
		(error "No cross references in this node")))))
    (goto-char (or (match-beginning 1) (match-beginning 0)))
    (if (looking-at "\\* Menu:")
	(if recur
	    (error "No cross references in this node")
	  (Info-prev-reference t))
      (if (looking-at "^\\* ")
	  (forward-char 2)))))

(defvar Info-index-nodes nil
  "Alist of cached index node names of visited Info files.
Each element has the form (INFO-FILE INDEX-NODE-NAMES-LIST).")

(defun Info-index-nodes (&optional file)
  "Return a list of names of all index nodes in Info FILE.
If FILE is omitted, it defaults to the current Info file.
First look in a list of cached index node names.  Then scan Info
file and its subfiles for nodes with the index cookie.  Then try
to find index nodes starting from the first node in the top level
menu whose name contains the word \"Index\", plus any immediately
following nodes whose names also contain the word \"Index\"."
  (or file (setq file Info-current-file))
  (or (assoc file Info-index-nodes)
      ;; Skip virtual Info files
      (and (or (not (stringp file))
	       (Info-virtual-file-p file))
           (setq Info-index-nodes (cons (cons file nil) Info-index-nodes)))
      (if (Info-file-supports-index-cookies file)
	  ;; Find nodes with index cookie
	  (let* ((default-directory (or (and (stringp file)
					     (file-name-directory
					      (setq file (Info-find-file file))))
					default-directory))
		 Info-history Info-history-list Info-fontify-maximum-menu-size
		 (main-file file) subfiles nodes)
	    (condition-case nil
		(with-temp-buffer
		  (while (or main-file subfiles)
		    (erase-buffer)
		    (info-insert-file-contents (or main-file (car subfiles)))
		    (goto-char (point-min))
		    (while (search-forward "\0\b[index\0\b]" nil 'move)
		      (save-excursion
			(re-search-backward "^\^_")
			(search-forward "Node: ")
			(setq nodes (cons (Info-following-node-name) nodes))))
		    (if main-file
			(save-excursion
			  (goto-char (point-min))
			  (if (search-forward "\n\^_\nIndirect:" nil t)
			      (let ((bound (save-excursion (search-forward "\n\^_" nil t))))
				(while (re-search-forward "^\\(.*\\): [0-9]+$" bound t)
				  (setq subfiles (cons (match-string-no-properties 1)
						       subfiles)))))
			  (setq subfiles (nreverse subfiles)
				main-file nil))
		      (setq subfiles (cdr subfiles)))))
	      (error nil))
	    (if nodes
		(setq nodes (nreverse nodes)
		      Info-index-nodes (cons (cons file nodes) Info-index-nodes)))
	    nodes)
	;; Else find nodes with the word "Index" in the node name
	(let ((case-fold-search t)
	      Info-history Info-history-list Info-fontify-maximum-menu-size Info-point-loc
	      nodes node)
	  (condition-case nil
	      (with-temp-buffer
		(Info-mode)
		(Info-find-node file "Top")
		(when (and (search-forward "\n* menu:" nil t)
			   (re-search-forward "\n\\* \\(.*\\<Index\\>\\)" nil t))
		  (goto-char (match-beginning 1))
		  (setq nodes (list (Info-extract-menu-node-name)))
		  (Info-goto-node (car nodes))
		  (while (and (setq node (Info-extract-pointer "next" t))
			      (string-match "\\<Index\\>" node))
		    (push node nodes)
		    (Info-goto-node node))))
	    (error nil))
	  (if nodes
	      (setq nodes (nreverse nodes)
		    Info-index-nodes (cons (cons file nodes) Info-index-nodes)))
	  nodes))
      ;; If file has no index nodes, still add it to the cache
      (setq Info-index-nodes (cons (cons file nil) Info-index-nodes)))
  (cdr (assoc file Info-index-nodes)))

(defun Info-index-node (&optional node file)
  "Return non-nil value if NODE is an index node.
If NODE is nil, check the current Info node.
If FILE is nil, check the current Info file."
  (or file (setq file Info-current-file))
  (if (and (or (and node (not (equal node Info-current-node)))
	       (assoc file Info-index-nodes))
	   (not Info-current-node-virtual))
      (member (or node Info-current-node) (Info-index-nodes file))
    ;; Don't search all index nodes if request is only for the current node
    ;; and file is not in the cache of index nodes
    (save-match-data
      (if (Info-file-supports-index-cookies file)
	  (save-excursion
	    (goto-char (+ (or (save-excursion
				(search-backward "\n\^_" nil t))
			      (point-min)) 2))
	    (search-forward "\0\b[index\0\b]"
			    (or (save-excursion
				  (search-forward "\n\^_" nil t))
				(point-max)) t))
	(string-match "\\<Index\\>" (or node Info-current-node ""))))))

(defun Info-goto-index ()
  "Go to the first index node."
  (let ((node (car (Info-index-nodes))))
    (or node (error "No index"))
    (Info-goto-node node)))

;;;###autoload
(defun Info-index (topic)
  "Look up a string TOPIC in the index for this manual and go to that entry.
If there are no exact matches to the specified topic, this chooses
the first match which is a case-insensitive substring of a topic.
Use the \\<Info-mode-map>\\[Info-index-next] command to see the other matches.
Give an empty topic name to go to the Index node itself."
  (interactive
   (list
    (let ((completion-ignore-case t)
	  (Info-complete-menu-buffer (clone-buffer))
	  (Info-complete-nodes (Info-index-nodes))
	  (Info-history-list nil))
      (if (equal Info-current-file "dir")
	  (error "The Info directory node has no index; use m to select a manual"))
      (unwind-protect
	  (with-current-buffer Info-complete-menu-buffer
	    (Info-goto-index)
	    (completing-read "Index topic: " 'Info-complete-menu-item))
	(kill-buffer Info-complete-menu-buffer)))))
  (if (equal Info-current-file "dir")
      (error "The Info directory node has no index; use m to select a manual"))
  ;; Strip leading colon in topic; index format does not allow them.
  (if (and (stringp topic)
	   (> (length topic) 0)
	   (= (aref topic 0) ?:))
      (setq topic (substring topic 1)))
  (let ((orignode Info-current-node)
	(pattern (format "\n\\* +\\([^\n]*%s[^\n]*\\):[ \t]+\\([^\n]*\\)\\.\\(?:[ \t\n]*(line +\\([0-9]+\\))\\)?"
			 (regexp-quote topic)))
	node (nodes (Info-index-nodes))
	(ohist-list Info-history-list)
	(case-fold-search t))
    (Info-goto-index)
    (or (equal topic "")
	(let ((matches nil)
	      (exact nil)
	      ;; We bind Info-history to nil for internal node-switches so
	      ;; that we don't put junk in the history.  In the first
	      ;; Info-goto-index call, above, we do update the history
	      ;; because that is what the user's previous node choice into it.
	      (Info-history nil)
	      found)
	  (while
	      (progn
		(goto-char (point-min))
		(while (re-search-forward pattern nil t)
		  (push (list (match-string-no-properties 1)
			      (match-string-no-properties 2)
			      Info-current-node
			      (string-to-number (concat "0"
							(match-string 3))))
			matches))
		(setq nodes (cdr nodes) node (car nodes)))
	    (Info-goto-node node))
	  (or matches
	      (progn
		(Info-goto-node orignode)
		(error "No `%s' in index" topic)))
	  ;; Here it is a feature that assoc is case-sensitive.
	  (while (setq found (assoc topic matches))
	    (setq exact (cons found exact)
		  matches (delq found matches)))
          (setq Info-history-list ohist-list)
	  (setq Info-index-alternatives (nconc exact (nreverse matches)))
	  (Info-index-next 0)))))

(defun Info-index-next (num)
  "Go to the next matching index item from the last \\<Info-mode-map>\\[Info-index] command."
  (interactive "p")
  (or Info-index-alternatives
      (error "No previous `i' command"))
  (while (< num 0)
    (setq num (+ num (length Info-index-alternatives))))
  (while (> num 0)
    (setq Info-index-alternatives
	  (nconc (cdr Info-index-alternatives)
		 (list (car Info-index-alternatives)))
	  num (1- num)))
  (Info-goto-node (nth 1 (car Info-index-alternatives)))
  (if (> (nth 3 (car Info-index-alternatives)) 0)
      ;; Forward 2 lines less because `Info-find-node-2' initially
      ;; puts point to the 2nd line.
      (forward-line (- (nth 3 (car Info-index-alternatives)) 2))
    (forward-line 3)			; don't search in headers
    (let ((name (car (car Info-index-alternatives))))
      (Info-find-index-name name)))
  (message "Found `%s' in %s.  %s"
	   (car (car Info-index-alternatives))
	   (nth 2 (car Info-index-alternatives))
	   (if (cdr Info-index-alternatives)
	       (format "(%s total; use `%s' for next)"
		       (length Info-index-alternatives)
		       (key-description (where-is-internal
					 'Info-index-next overriding-local-map
					 t)))
	     "(Only match)")))

(defun Info-find-index-name (name)
  "Move point to the place within the current node where NAME is defined."
  (let ((case-fold-search t))
    (if (or (re-search-forward (format
				"[a-zA-Z]+: %s\\( \\|$\\)"
				(regexp-quote name)) nil t)
	    ;; Find a function definition with a return type.
	    (re-search-forward (format
                                "[a-zA-Z]+: [a-zA-Z0-9_ *&]+ %s\\( \\|$\\)"
                                (regexp-quote name)) nil t)
	    (search-forward (format "`%s'" name) nil t)
	    (and (string-match "\\`.*\\( (.*)\\)\\'" name)
		 (search-forward
		  (format "`%s'" (substring name 0 (match-beginning 1)))
		  nil t))
	    (search-forward name nil t)
	    ;; Try again without the " <1>" makeinfo can append
            (and (string-match "\\`\\(.*\\) <[0-9]+>\\'" name)
                 (Info-find-index-name (match-string 1 name))))
	(progn (beginning-of-line) t)  ;; non-nil for recursive call
      (goto-char (point-min)))))

(add-to-list 'Info-virtual-nodes
	     '("\\`\\*Index.*\\*\\'"
	       (find-node . Info-virtual-index-find-node)
	       (slow . t)
	       ))

(defvar Info-virtual-index-nodes nil
  "Alist of cached matched index search nodes.
Each element is ((FILENAME . TOPIC) MATCHES) where
FILENAME is the file name of the manual,
TOPIC is the search string given as an argument to `Info-virtual-index',
MATCHES is a list of index matches found by `Info-index'.")

(defun Info-virtual-index-find-node (filename nodename &optional _no-going-back)
  "Index-specific implementation of `Info-find-node-2'."
  ;; Generate Index-like menu of matches
  (if (string-match "^\\*Index for `\\(.+\\)'\\*$" nodename)
      ;; Generate Index-like menu of matches
      (let* ((topic (match-string 1 nodename))
	     (matches (cdr (assoc (cons (or filename Info-current-file) topic)
				  Info-virtual-index-nodes))))
	(insert (format "\n\^_\nFile: %s,  Node: %s,  Up: *Index*\n\n"
			(or filename Info-current-file) nodename))
	(insert "Info Virtual Index\n")
	(insert "******************\n\n")
	(insert "Index entries that match `" topic "':\n\n")
	(insert "\0\b[index\0\b]\n")
	(if (null matches)
	    (insert "No matches found.\n")
	  (insert "* Menu:\n\n")
	  (dolist (entry matches)
	    (insert (format "* %-38s %s.%s\n"
			    (format "%s [%s]:" (nth 0 entry) (nth 2 entry))
			    (nth 1 entry)
			    (if (nth 3 entry)
				(format " (line %s)" (nth 3 entry))
			      ""))))))
    ;; Else, Generate a list of previous search results
    (let ((nodes (reverse Info-virtual-index-nodes)))
      (insert (format "\n\^_\nFile: %s,  Node: %s,  Up: Top\n\n"
		      (or filename Info-current-file) nodename))
      (insert "Info Virtual Index\n")
      (insert "******************\n\n")
      (insert "This is a list of search results produced by\n"
	      "`Info-virtual-index' for the current manual.\n\n")
      (insert "* Menu:\n\n")
      (dolist (nodeinfo nodes)
	(when (equal (car (nth 0 nodeinfo)) (or filename Info-current-file))
	  (insert
	   (format "* %-20s %s.\n"
		   (format "*Index for `%s'*::" (cdr (nth 0 nodeinfo)))
		   (cdr (nth 0 nodeinfo)))))))))

(defun Info-virtual-index (topic)
  "Show a node with all lines in the index containing a string TOPIC.
Like `Info-index' but displays a node with index search results.
Give an empty topic name to go to the node with links to previous
search results."
  ;; `interactive' is a copy from `Info-index'
  (interactive
   (list
    (let ((completion-ignore-case t)
	  (Info-complete-menu-buffer (clone-buffer))
	  (Info-complete-nodes (Info-index-nodes))
	  (Info-history-list nil))
      (if (equal Info-current-file "dir")
	  (error "The Info directory node has no index; use m to select a manual"))
      (unwind-protect
	  (with-current-buffer Info-complete-menu-buffer
	    (Info-goto-index)
	    (completing-read "Index topic: " 'Info-complete-menu-item))
	(kill-buffer Info-complete-menu-buffer)))))
  (if (equal topic "")
      (Info-find-node Info-current-file "*Index*")
    (unless (assoc (cons Info-current-file topic) Info-virtual-index-nodes)
      (let ((orignode Info-current-node)
	    (ohist-list Info-history-list))
	;; Reuse `Info-index' to set `Info-index-alternatives'.
	(Info-index topic)
	(push (cons (cons Info-current-file topic) Info-index-alternatives)
	      Info-virtual-index-nodes)
	;; Clean up unnecessary side-effects of `Info-index'.
	(setq Info-history-list ohist-list)
	(Info-goto-node orignode)
	(message "")))
    (Info-find-node Info-current-file (format "*Index for `%s'*" topic))))

(add-to-list 'Info-virtual-files
	     '("\\`\\*Apropos\\*\\'"
	       (toc-nodes . Info-apropos-toc-nodes)
	       (find-file . Info-apropos-find-file)
	       (find-node . Info-apropos-find-node)
	       (slow . t)
	       ))

(defvar Info-apropos-file "*Apropos*"
  "Info file name of the virtual manual for matches of `info-apropos'.")

(defvar Info-apropos-nodes nil
  "Alist of cached apropos matched nodes.
Each element is (NODENAME STRING MATCHES) where
NODENAME is the name of the node that holds the search result,
STRING is the search string given as an argument to `info-apropos',
MATCHES is a list of index matches found by `Info-apropos-matches'.")

(defun Info-apropos-toc-nodes (filename)
  "Apropos-specific implementation of `Info-toc-nodes'."
  (let ((nodes (mapcar 'car (reverse Info-apropos-nodes))))
    `(,filename
      ("Top" nil nil ,nodes)
      ,@(mapcar (lambda (node) `(,node "Top" nil nil)) nodes))))

(defun Info-apropos-find-file (filename &optional _noerror)
  "Apropos-specific implementation of `Info-find-file'."
  filename)

(defun Info-apropos-find-node (_filename nodename &optional _no-going-back)
  "Apropos-specific implementation of `Info-find-node-2'."
  (if (equal nodename "Top")
      ;; Generate Top menu
      (let ((nodes (reverse Info-apropos-nodes)))
	(insert (format "\n\^_\nFile: %s,  Node: %s,  Up: (dir)\n\n"
			Info-apropos-file nodename))
	(insert "Apropos Index\n")
	(insert "*************\n\n")
	(insert "This is a list of search results produced by `info-apropos'.\n\n")
	(insert "* Menu:\n\n")
	(dolist (nodeinfo nodes)
	  (insert (format "* %-20s %s.\n"
			  (format "%s::" (nth 0 nodeinfo))
			  (nth 1 nodeinfo)))))
    ;; Else, Generate Index-like menu of matches
    (let* ((nodeinfo (assoc nodename Info-apropos-nodes))
	   (matches (nth 2 nodeinfo)))
      (when matches
	(insert (format "\n\^_\nFile: %s,  Node: %s,  Up: Top\n\n"
			Info-apropos-file nodename))
	(insert "Apropos Index\n")
	(insert "*************\n\n")
	(insert "Index entries that match `" (nth 1 nodeinfo) "':\n\n")
	(insert "\0\b[index\0\b]\n")
	(if (eq matches t)
	    (insert "No matches found.\n")
	  (insert "* Menu:\n\n")
	  (dolist (entry matches)
	    (insert (format "* %-38s (%s)%s.%s\n"
			    (format "%s [%s]:" (nth 1 entry) (nth 0 entry))
			    (nth 0 entry)
			    (nth 2 entry)
			    (if (nth 3 entry)
				(format " (line %s)" (nth 3 entry))
			      "")))))))))

(defun Info-apropos-matches (string)
  "Collect STRING matches from all known Info files on your system.
Return a list of matches where each element is in the format
\((FILENAME INDEXTEXT NODENAME LINENUMBER))."
  (unless (string= string "")
    (let ((pattern (format "\n\\* +\\([^\n]*%s[^\n]*\\):[ \t]+\\([^\n]+\\)\\.\\(?:[ \t\n]*(line +\\([0-9]+\\))\\)?"
			   (regexp-quote string)))
	  (ohist Info-history)
	  (ohist-list Info-history-list)
	  (current-node Info-current-node)
	  (current-file Info-current-file)
	  manuals matches node nodes)
      (let ((Info-fontify-maximum-menu-size nil))
	(Info-directory)
	;; current-node and current-file are nil when they invoke info-apropos
	;; as the first Info command, i.e. info-apropos loads info.el.  In that
	;; case, we use (DIR)Top instead, to avoid signaling an error after
	;; the search is complete.
	(when (null current-node)
	  (setq current-file Info-current-file)
	  (setq current-node Info-current-node))
	(message "Searching indices...")
	(goto-char (point-min))
	(re-search-forward "\\* Menu: *\n" nil t)
	(while (re-search-forward "\\*.*: *(\\([^)]+\\))" nil t)
	  ;; add-to-list makes sure we don't have duplicates in `manuals',
	  ;; so that the following dolist loop runs faster.
	  (add-to-list 'manuals (match-string 1)))
	(dolist (manual (nreverse manuals))
	  (message "Searching %s" manual)
	  (condition-case err
	      (if (setq nodes (Info-index-nodes (Info-find-file manual)))
                  (save-excursion
                    (Info-find-node manual (car nodes))
                    (while
                        (progn
                          (goto-char (point-min))
                          (while (re-search-forward pattern nil t)
			    (setq matches
				  (cons (list manual
					      (match-string-no-properties 1)
					      (match-string-no-properties 2)
					      (match-string-no-properties 3))
					matches)))
                          (setq nodes (cdr nodes) node (car nodes)))
                      (Info-goto-node node))))
	    (error
	     (message "%s" (if (eq (car-safe err) 'error)
			       (nth 1 err) err))
	     (sit-for 1 t)))))
      (Info-find-node current-file current-node)
      (setq Info-history ohist
	    Info-history-list ohist-list)
      (message "Searching indices...done")
      (or (nreverse matches) t))))

;;;###autoload
(defun info-apropos (string)
  "Grovel indices of all known Info files on your system for STRING.
Build a menu of the possible matches."
  (interactive "sIndex apropos: ")
  (if (equal string "")
      (Info-find-node Info-apropos-file "Top")
    (let* ((nodes Info-apropos-nodes) nodename)
      (while (and nodes (not (equal string (nth 1 (car nodes)))))
	(setq nodes (cdr nodes)))
      (if nodes
	  (Info-find-node Info-apropos-file (car (car nodes)))
	(setq nodename (format "Index for `%s'" string))
	(push (list nodename string (Info-apropos-matches string))
	      Info-apropos-nodes)
	(Info-find-node Info-apropos-file nodename)))))

(add-to-list 'Info-virtual-files
	     '("\\`\\*Finder.*\\*\\'"
	       (find-file . Info-finder-find-file)
	       (find-node . Info-finder-find-node)
	       ))

(defvar Info-finder-file "*Finder*"
  "Info file name of the virtual Info keyword finder manual.")

(defun Info-finder-find-file (filename &optional _noerror)
  "Finder-specific implementation of `Info-find-file'."
  filename)

(defvar finder-known-keywords)
(declare-function find-library-name "find-func" (library))
(declare-function finder-unknown-keywords "finder" ())
(declare-function lm-commentary "lisp-mnt" (&optional file))
(defvar finder-keywords-hash)
(defvar package--builtins)		; finder requires package

(defun Info-finder-find-node (_filename nodename &optional _no-going-back)
  "Finder-specific implementation of `Info-find-node-2'."
  (require 'finder)
  (cond
   ((equal nodename "Top")
    ;; Display Top menu with descriptions of the keywords
    (insert (format "\n\^_\nFile: %s,  Node: %s,  Up: (dir)\n\n"
		    Info-finder-file nodename))
    (insert "Finder Keywords\n")
    (insert "***************\n\n")
    (insert "* Menu:\n\n")
    (dolist (assoc (append '((all . "All package info")
			     (unknown . "Unknown keywords"))
			   finder-known-keywords))
      (let ((keyword (car assoc)))
	(insert (format "* %s %s.\n"
			(concat (symbol-name keyword) ": "
				"Keyword " (symbol-name keyword) ".")
			(cdr assoc))))))
   ((equal nodename "Keyword unknown")
    ;; Display unknown keywords
    (insert (format "\n\^_\nFile: %s,  Node: %s,  Up: Top\n\n"
		    Info-finder-file nodename))
    (insert "Finder Unknown Keywords\n")
    (insert "***********************\n\n")
    (insert "* Menu:\n\n")
    (mapc
     (lambda (assoc)
       (insert (format "* %-14s %s.\n"
		       (concat (symbol-name (car assoc)) ": "
			       "Keyword " (symbol-name (car assoc)) ".")
		       (cdr assoc))))
     (finder-unknown-keywords)))
   ((equal nodename "Keyword all")
    ;; Display all package info.
    (insert (format "\n\^_\nFile: %s,  Node: %s,  Up: Top\n\n"
		    Info-finder-file nodename))
    (insert "Finder Package Info\n")
    (insert "*******************\n\n")
    (insert "* Menu:\n\n")
    (let (desc)
      (dolist (package package--builtins)
	(setq desc (cdr-safe package))
	(when (vectorp desc)
	  (insert (format "* %-16s %s.\n"
			  (concat (symbol-name (car package)) "::")
			  (aref desc 2)))))))
   ((string-match "\\`Keyword " nodename)
    (setq nodename (substring nodename (match-end 0)))
    ;; Display packages that match the keyword
    ;; or the list of keywords separated by comma.
    (insert (format "\n\^_\nFile: %s,  Node: Keyword %s,  Up: Top\n\n"
		    Info-finder-file nodename))
    (insert "Finder Packages\n")
    (insert "***************\n\n")
    (insert
     "The following packages match the keyword `" nodename "':\n\n")
    (insert "* Menu:\n\n")
    (let ((keywords
	   (mapcar 'intern (if (string-match-p "," nodename)
			       (split-string nodename ",[ \t\n]*" t)
			     (list nodename))))
	  hits desc)
      (dolist (keyword keywords)
	(push (copy-tree (gethash keyword finder-keywords-hash)) hits))
      (setq hits (delete-dups (apply 'append hits)))
      (dolist (package hits)
	(setq desc (cdr-safe (assq package package--builtins)))
	(when (vectorp desc)
	  (insert (format "* %-16s %s.\n"
			  (concat (symbol-name package) "::")
			  (aref desc 2)))))))
   (t
    ;; Display commentary section
    (insert (format "\n\^_\nFile: %s,  Node: %s,  Up: Top\n\n"
		    Info-finder-file nodename))
    (insert "Finder Commentary\n")
    (insert "*****************\n\n")
    (insert
     "Commentary section of the package `" nodename "':\n\n")
    (let ((str (lm-commentary (find-library-name nodename))))
      (if (null str)
	  (insert "Can't find any Commentary section\n\n")
	(insert
	 (with-temp-buffer
	   (insert str)
	   (goto-char (point-min))
	   (delete-blank-lines)
	   (goto-char (point-max))
	   (delete-blank-lines)
	   (goto-char (point-min))
	   (while (re-search-forward "^;+ ?" nil t)
	     (replace-match "" nil nil))
	   (buffer-string))))))))

;;;###autoload
(defun info-finder (&optional keywords)
  "Display descriptions of the keywords in the Finder virtual manual.
In interactive use, a prefix argument directs this command to read
a list of keywords separated by comma.  After that, it displays a node
with a list of packages that contain all specified keywords."
  (interactive
   (when current-prefix-arg
     (require 'finder)
     (list
      (completing-read-multiple
       "Keywords (separated by comma): "
       (mapcar 'symbol-name (mapcar 'car (append finder-known-keywords
						 (finder-unknown-keywords))))
       nil t))))
  (require 'finder)
  (if keywords
      (Info-find-node Info-finder-file (mapconcat 'identity keywords ", "))
    (Info-find-node Info-finder-file "Top")))


(defun Info-undefined ()
  "Make command be undefined in Info."
  (interactive)
  (ding))

(defun Info-help ()
  "Enter the Info tutorial."
  (interactive)
  (delete-other-windows)
  (Info-find-node "info"
		  (if (< (window-height) 23)
		      "Help-Small-Screen"
		    "Help")))

(defun Info-summary ()
  "Display a brief summary of all Info commands."
  (interactive)
  (save-window-excursion
    (switch-to-buffer "*Help*")
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert (documentation 'Info-mode))
    (help-mode)
    (goto-char (point-min))
    (let (ch flag)
      (while (progn (setq flag (not (pos-visible-in-window-p (point-max))))
		    (message (if flag "Type Space to see more"
			       "Type Space to return to Info"))
		    (if (not (eq ?\s (setq ch (read-event))))
			(progn (setq unread-command-events (list ch)) nil)
		      flag))
	(scroll-up)))
    (bury-buffer "*Help*")))

(defun Info-get-token (pos start all &optional errorstring)
  "Return the token around POS.
POS must be somewhere inside the token.
START is a regular expression which will match the
    beginning of the tokens delimited string.
ALL is a regular expression with a single
    parenthesized subpattern which is the token to be
    returned.  E.g. '{\(.*\)}' would return any string
    enclosed in braces around POS.
ERRORSTRING optional fourth argument, controls action on no match:
    nil: return nil
    t: beep
    a string: signal an error, using that string."
  (let ((case-fold-search t))
    (save-excursion
      (goto-char pos)
      ;; First look for a match for START that goes across POS.
      (while (and (not (bobp)) (> (point) (- pos (length start)))
		  (not (looking-at start)))
	(forward-char -1))
      ;; If we did not find one, search back for START
      ;; (this finds only matches that end at or before POS).
      (or (looking-at start)
	  (progn
	    (goto-char pos)
	    (re-search-backward start (max (point-min) (- pos 200)) 'yes)))
      (let (found)
	(while (and (re-search-forward all (min (point-max) (+ pos 200)) 'yes)
		    (not (setq found (and (<= (match-beginning 0) pos)
					  (> (match-end 0) pos))))))
	(if (and found (<= (match-beginning 0) pos)
		 (> (match-end 0) pos))
	    (match-string-no-properties 1)
	  (cond ((null errorstring)
		 nil)
		((eq errorstring t)
		 (beep)
		 nil)
		(t
		 (error "No %s around position %d" errorstring pos))))))))

(defun Info-mouse-follow-nearest-node (click)
  "\\<Info-mode-map>Follow a node reference near point.
Like \\[Info-menu], \\[Info-follow-reference], \\[Info-next], \\[Info-prev] or \\[Info-up] command, depending on where you click.
At end of the node's text, moves to the next node, or up if none."
  (interactive "e")
  (mouse-set-point click)
  (and (not (Info-follow-nearest-node))
       (save-excursion (forward-line 1) (eobp))
       (Info-next-preorder)))

(defun Info-follow-nearest-node (&optional fork)
  "Follow a node reference near point.
If point is on a reference, follow that reference.  Otherwise,
if point is in a menu item description, follow that menu item.

If FORK is non-nil (interactively with a prefix arg), show the node in
a new Info buffer.
If FORK is a string, it is the name to use for the new buffer."
  (interactive "P")
  (or (Info-try-follow-nearest-node fork)
      (when (save-excursion
	      (search-backward "\n* menu:" nil t))
	(save-excursion
	  (beginning-of-line)
	  (while (not (or (bobp) (looking-at "[^ \t]\\|[ \t]*$")))
	    (beginning-of-line 0))
	  (when (looking-at "\\* +\\([^\t\n]*\\):")
	    (Info-goto-node
	     (Info-extract-menu-item (match-string-no-properties 1)) fork)
	    t)))
      (and (eq this-command 'Info-mouse-follow-nearest-node)
	   ;; Don't raise an error when mouse-1 is bound to this - it's
	   ;; often used to simply select the window or frame.
	   (eq 'mouse-1 (event-basic-type last-input-event)))
      (error "Point neither on reference nor in menu item description")))

;; Common subroutine.
(defun Info-try-follow-nearest-node (&optional fork)
  "Follow a node reference near point.  Return non-nil if successful.
If FORK is non-nil, it is passed to `Info-goto-node'."
  (let (node)
    (cond
     ((setq node (Info-get-token (point) "[hf]t?tps?://"
				 "\\([hf]t?tps?://[^ \t\n\"`({<>})']+\\)"))
      (browse-url node)
      (setq node t))
     ((setq node (Info-get-token (point) "\\*note[ \n\t]+"
				 "\\*note[ \n\t]+\\([^:]*\\):\\(:\\|[ \n\t]*(\\)?"))
      (Info-follow-reference node fork))
     ;; menu item: node name
     ((setq node (Info-get-token (point) "\\* +" "\\* +\\([^:]*\\)::"))
      (Info-goto-node node fork))
     ;; menu item: node name or index entry
     ((Info-get-token (point) "\\* +" "\\* +\\(.*\\): ")
      (beginning-of-line)
      (forward-char 2)
      (setq node (Info-extract-menu-node-name nil (Info-index-node)))
      (Info-goto-node node fork))
     ((setq node (Info-get-token (point) "Up: " "Up: \\([^,\n\t]*\\)"))
      (Info-goto-node node fork))
     ((setq node (Info-get-token (point) "Next: " "Next: \\([^,\n\t]*\\)"))
      (Info-goto-node node fork))
     ((setq node (Info-get-token (point) "File: " "File: \\([^,\n\t]*\\)"))
      (Info-goto-node "Top" fork))
     ((setq node (Info-get-token (point) "Prev: " "Prev: \\([^,\n\t]*\\)"))
      (Info-goto-node node fork)))
    node))

(defun Info-mouse-follow-link (click)
  "Follow a link where you click."
  (interactive "e")
  (let* ((position (event-start click))
	 (posn-string (and position (posn-string position)))
	 (string (car-safe posn-string))
	 (string-pos (cdr-safe posn-string))
	 (link-args (and string string-pos
			 (get-text-property string-pos 'link-args string))))
    (when link-args
      (Info-goto-node link-args))))


(defvar Info-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map "." 'beginning-of-buffer)
    (define-key map " " 'Info-scroll-up)
    (define-key map "\C-m" 'Info-follow-nearest-node)
    (define-key map "\t" 'Info-next-reference)
    (define-key map "\e\t" 'Info-prev-reference)
    (define-key map [backtab] 'Info-prev-reference)
    (define-key map "1" 'Info-nth-menu-item)
    (define-key map "2" 'Info-nth-menu-item)
    (define-key map "3" 'Info-nth-menu-item)
    (define-key map "4" 'Info-nth-menu-item)
    (define-key map "5" 'Info-nth-menu-item)
    (define-key map "6" 'Info-nth-menu-item)
    (define-key map "7" 'Info-nth-menu-item)
    (define-key map "8" 'Info-nth-menu-item)
    (define-key map "9" 'Info-nth-menu-item)
    (define-key map "0" 'undefined)
    (define-key map "?" 'Info-summary)
    (define-key map "]" 'Info-forward-node)
    (define-key map "[" 'Info-backward-node)
    (define-key map "<" 'Info-top-node)
    (define-key map ">" 'Info-final-node)
    (define-key map "b" 'beginning-of-buffer)
    (put 'beginning-of-buffer :advertised-binding "b")
    (define-key map "d" 'Info-directory)
    (define-key map "e" 'end-of-buffer)
    (define-key map "f" 'Info-follow-reference)
    (define-key map "g" 'Info-goto-node)
    (define-key map "h" 'Info-help)
    (define-key map "i" 'Info-index)
    (define-key map "I" 'Info-virtual-index)
    (define-key map "l" 'Info-history-back)
    (define-key map "L" 'Info-history)
    (define-key map "m" 'Info-menu)
    (define-key map "n" 'Info-next)
    (define-key map "p" 'Info-prev)
    (define-key map "q" 'Info-exit)
    (define-key map "r" 'Info-history-forward)
    (define-key map "s" 'Info-search)
    (define-key map "S" 'Info-search-case-sensitively)
    (define-key map "\M-n" 'clone-buffer)
    (define-key map "t" 'Info-top-node)
    (define-key map "T" 'Info-toc)
    (define-key map "u" 'Info-up)
    ;; `w' for consistency with `dired-copy-filename-as-kill'.
    (define-key map "w" 'Info-copy-current-node-name)
    (define-key map "c" 'Info-copy-current-node-name)
    ;; `^' for consistency with `dired-up-directory'.
    (define-key map "^" 'Info-up)
    (define-key map "," 'Info-index-next)
    (define-key map "\177" 'Info-scroll-down)
    (define-key map [mouse-2] 'Info-mouse-follow-nearest-node)
    (define-key map [follow-link] 'mouse-face)
    map)
  "Keymap containing Info commands.")


(defun Info-check-pointer (item)
  "Non-nil if ITEM is present in this node."
  (condition-case nil
      (Info-extract-pointer item)
    (error nil)))

(easy-menu-define
 Info-mode-menu Info-mode-map
 "Menu for Info files."
 '("Info"
   ["Up" Info-up :active (Info-check-pointer "up")
    :help "Go up in the Info tree"]
   ["Next" Info-next :active (Info-check-pointer "next")
    :help "Go to the next node"]
   ["Previous" Info-prev :active (Info-check-pointer "prev[ious]*")
    :help "Go to the previous node"]
   ["Backward" Info-backward-node
    :help "Go backward one node, considering all as a sequence"]
   ["Forward" Info-forward-node
    :help "Go forward one node, considering all as a sequence"]
   ["Beginning" beginning-of-buffer
    :help "Go to beginning of this node"]
   ["Top" Info-top-node
    :help "Go to top node of file"]
   ["Final Node" Info-final-node
    :help "Go to final node in this file"]
   ("Menu Item" ["You should never see this" report-emacs-bug t])
   ("Reference" ["You should never see this" report-emacs-bug t])
   ["Search..." Info-search
    :help "Search for regular expression in this Info file"]
   ["Search Next" Info-search-next
    :help "Search for another occurrence of regular expression"]
   ["Go to Node..." Info-goto-node
    :help "Go to a named node"]
   ["Back in history" Info-history-back :active Info-history
    :help "Go back in history to the last node you were at"]
   ["Forward in history" Info-history-forward :active Info-history-forward
    :help "Go forward in history"]
   ["History" Info-history :active Info-history-list
    :help "Go to menu of visited nodes"]
   ["Table of Contents" Info-toc
    :help "Go to table of contents"]
   ("Index"
    ["Lookup a String..." Info-index
     :help "Look for a string in the index items"]
    ["Next Matching Item" Info-index-next :active Info-index-alternatives
     :help "Look for another occurrence of previous item"]
    ["Lookup a string and display index of results..." Info-virtual-index
     :help "Look for a string in the index items and display node with results"]
    ["Lookup a string in all indices..." info-apropos
     :help "Look for a string in the indices of all manuals"])
   ["Copy Node Name" Info-copy-current-node-name
    :help "Copy the name of the current node into the kill ring"]
   ["Clone Info buffer" clone-buffer
    :help "Create a twin copy of the current Info buffer."]
   ["Exit" Info-exit :help "Stop reading Info"]))


(defvar info-tool-bar-map
  (let ((map (make-sparse-keymap)))
    (tool-bar-local-item-from-menu 'Info-history-back "left-arrow" map Info-mode-map
				   :rtl "right-arrow"
				   :label "Back"
				   :vert-only t)
    (tool-bar-local-item-from-menu 'Info-history-forward "right-arrow" map Info-mode-map
				   :rtl "left-arrow"
				   :label "Forward"
				   :vert-only t)
    (define-key-after map [separator-1] menu-bar-separator)
    (tool-bar-local-item-from-menu 'Info-prev "prev-node" map Info-mode-map
				   :rtl "next-node")
    (tool-bar-local-item-from-menu 'Info-next "next-node" map Info-mode-map
				   :rtl "prev-node")
    (tool-bar-local-item-from-menu 'Info-up "up-node" map Info-mode-map
				   :vert-only t)
    (define-key-after map [separator-2] menu-bar-separator)
    (tool-bar-local-item-from-menu 'Info-top-node "home" map Info-mode-map
				   :vert-only t)
    (tool-bar-local-item-from-menu 'Info-goto-node "jump-to" map Info-mode-map)
    (define-key-after map [separator-3] menu-bar-separator)
    (tool-bar-local-item-from-menu 'Info-index "index" map Info-mode-map
				   :label "Index")
    (tool-bar-local-item-from-menu 'Info-search "search" map Info-mode-map
				   :vert-only t)
    (tool-bar-local-item-from-menu 'Info-exit "exit" map Info-mode-map
				   :vert-only t)
    map))

(defvar Info-menu-last-node nil)
;; Last node the menu was created for.
;; Value is a list, (FILE-NAME NODE-NAME).

(defun Info-menu-update ()
  "Update the Info menu for the current node."
  (condition-case nil
      (if (or (not (eq major-mode 'Info-mode))
	      (equal (list Info-current-file Info-current-node)
		     Info-menu-last-node))
	  ()
	;; Update menu menu.
	(let* ((Info-complete-menu-buffer (current-buffer))
	       (items (nreverse (condition-case nil
				    (Info-complete-menu-item "" nil t)
				  (error nil))))
	       entries current
	       (number 0))
	  (while (and items (< number 9))
	    (setq current (car items)
		  items (cdr items)
		  number (1+ number))
	    (setq entries (cons `[,current
				  (Info-menu ,current)
				  :keys ,(format "%d" number)]
				entries)))
	  (if items
	      (setq entries (cons ["Other..." Info-menu t] entries)))
	  (or entries
	      (setq entries (list ["No menu" nil nil] nil :active)))
	  (easy-menu-change '("Info") "Menu Item" (nreverse entries)))
	;; Update reference menu.  Code stolen from `Info-follow-reference'.
	(let ((items nil)
	      str i entries current
	      (number 0)
	      (case-fold-search t))
	  (save-excursion
	    (goto-char (point-min))
	    (while (re-search-forward "\\*note[ \n\t]+\\([^:]*\\):" nil t)
	      (setq str (match-string 1))
	      (setq i 0)
	      (while (setq i (string-match "[ \n\t]+" str i))
		(setq str (concat (substring str 0 i) " "
				  (substring str (match-end 0))))
		(setq i (1+ i)))
	      (setq items
		    (cons str items))))
	  (while (and items (< number 9))
	    (setq current (car items)
		  items (cdr items)
		  number (1+ number))
	    (setq entries (cons `[,current
				  (Info-follow-reference ,current)
				  t]
				entries)))
	  (if items
	      (setq entries (cons ["Other..." Info-follow-reference t]
				  entries)))
	  (or entries
	      (setq entries (list ["No references" nil nil] nil :active)))
	  (easy-menu-change '("Info") "Reference" (nreverse entries)))
	;; Update last seen node.
	(setq Info-menu-last-node (list Info-current-file Info-current-node)))
    ;; Try to avoid entering infinite beep mode in case of errors.
    (error (ding))))


(defun Info-copy-current-node-name (&optional arg)
  "Put the name of the current Info node into the kill ring.
The name of the Info file is prepended to the node name in parentheses.
With a zero prefix arg, put the name inside a function call to `info'."
  (interactive "P")
  (unless Info-current-node
    (error "No current Info node"))
  (let ((node (if (stringp Info-current-file)
		  (concat "(" (file-name-nondirectory Info-current-file) ") "
			  Info-current-node))))
    (if (zerop (prefix-numeric-value arg))
        (setq node (concat "(info \"" node "\")")))
    (unless (stringp Info-current-file)
      (setq node (format "(Info-find-node '%S '%S)"
			 Info-current-file Info-current-node)))
    (kill-new node)
    (message "%s" node)))


;; Info mode is suitable only for specially formatted data.
(put 'Info-mode 'mode-class 'special)
(put 'Info-mode 'no-clone-indirect t)

(defvar tool-bar-map)
(defvar bookmark-make-record-function)

(defvar Info-mode-syntax-table
  (let ((st (copy-syntax-table text-mode-syntax-table)))
    ;; Use punctuation syntax for apostrophe because of
    ;; extensive use of quotes like `this' in Info manuals.
    (modify-syntax-entry ?' "." st)
    st)
  "Syntax table used in `Info-mode'.")

;; Autoload cookie needed by desktop.el
;;;###autoload
(define-derived-mode Info-mode nil "Info"
  "Info mode provides commands for browsing through the Info documentation tree.
Documentation in Info is divided into \"nodes\", each of which discusses
one topic and contains references to other nodes which discuss related
topics.  Info has commands to follow the references and show you other nodes.

\\<Info-mode-map>\
\\[Info-help]	Invoke the Info tutorial.
\\[Info-exit]	Quit Info: reselect previously selected buffer.

Selecting other nodes:
\\[Info-mouse-follow-nearest-node]
	Follow a node reference you click on.
	  This works with menu items, cross references, and
	  the \"next\", \"previous\" and \"up\", depending on where you click.
\\[Info-follow-nearest-node]	Follow a node reference near point, like \\[Info-mouse-follow-nearest-node].
\\[Info-next]	Move to the \"next\" node of this node.
\\[Info-prev]	Move to the \"previous\" node of this node.
\\[Info-up]	Move \"up\" from this node.
\\[Info-menu]	Pick menu item specified by name (or abbreviation).
	  Picking a menu item causes another node to be selected.
\\[Info-directory]	Go to the Info directory node.
\\[Info-top-node]	Go to the Top node of this file.
\\[Info-final-node]	Go to the final node in this file.
\\[Info-backward-node]	Go backward one node, considering all nodes as forming one sequence.
\\[Info-forward-node]	Go forward one node, considering all nodes as forming one sequence.
\\[Info-next-reference]	Move cursor to next cross-reference or menu item.
\\[Info-prev-reference]	Move cursor to previous cross-reference or menu item.
\\[Info-follow-reference]	Follow a cross reference.  Reads name of reference.
\\[Info-history-back]	Move back in history to the last node you were at.
\\[Info-history-forward]	Move forward in history to the node you returned from after using \\[Info-history-back].
\\[Info-history]	Go to menu of visited nodes.
\\[Info-toc]	Go to table of contents of the current Info file.

Moving within a node:
\\[Info-scroll-up]	Normally, scroll forward a full screen.
	  Once you scroll far enough in a node that its menu appears on the
	  screen but after point, the next scroll moves into its first
	  subnode.  When after all menu items (or if there is no menu),
	  move up to the parent node.
\\[Info-scroll-down]	Normally, scroll backward.  If the beginning of the buffer is
	  already visible, try to go to the previous menu entry, or up
	  if there is none.
\\[beginning-of-buffer]	Go to beginning of node.

Advanced commands:
\\[Info-search]	Search through this Info file for specified regexp,
	  and select the node in which the next occurrence is found.
\\[Info-search-case-sensitively]	Search through this Info file for specified regexp case-sensitively.
\\[isearch-forward], \\[isearch-forward-regexp]	Use Isearch to search through multiple Info nodes.
\\[Info-index]	Search for a topic in this manual's Index and go to index entry.
\\[Info-index-next]	(comma) Move to the next match from a previous \\<Info-mode-map>\\[Info-index] command.
\\[Info-virtual-index]	Look for a string and display the index node with results.
\\[info-apropos]	Look for a string in the indices of all manuals.
\\[Info-goto-node]	Move to node specified by name.
	  You may include a filename as well, as (FILENAME)NODENAME.
1 .. 9	Pick first ... ninth item in node's menu.
	  Every third `*' is highlighted to help pick the right number.
\\[Info-copy-current-node-name]	Put name of current Info node in the kill ring.
\\[clone-buffer]	Select a new cloned Info buffer in another window.
\\[universal-argument] \\[info]	Move to new Info file with completion.
\\[universal-argument] N \\[info]	Select Info buffer with prefix number in the name *info*<N>."
  :syntax-table Info-mode-syntax-table
  :abbrev-table text-mode-abbrev-table
  (setq tab-width 8)
  (add-hook 'activate-menubar-hook 'Info-menu-update nil t)
  (setq case-fold-search t)
  (setq buffer-read-only t)
  (make-local-variable 'Info-current-file)
  (make-local-variable 'Info-current-subfile)
  (make-local-variable 'Info-current-node)
  (set (make-local-variable 'Info-tag-table-marker) (make-marker))
  (set (make-local-variable 'Info-tag-table-buffer) nil)
  (make-local-variable 'Info-history)
  (make-local-variable 'Info-history-forward)
  (make-local-variable 'Info-index-alternatives)
  (if Info-use-header-line    ; do not override global header lines
      (setq header-line-format
 	    '(:eval (get-text-property (point-min) 'header-line))))
  (set (make-local-variable 'tool-bar-map) info-tool-bar-map)
  ;; This is for the sake of the invisible text we use handling titles.
  (set (make-local-variable 'line-move-ignore-invisible) t)
  (set (make-local-variable 'desktop-save-buffer)
       'Info-desktop-buffer-misc-data)
  (set (make-local-variable 'widen-automatically) nil)
  (add-hook 'kill-buffer-hook 'Info-kill-buffer nil t)
  (add-hook 'clone-buffer-hook 'Info-clone-buffer nil t)
  (add-hook 'change-major-mode-hook 'font-lock-defontify nil t)
  (add-hook 'isearch-mode-hook 'Info-isearch-start nil t)
  (set (make-local-variable 'isearch-search-fun-function)
       'Info-isearch-search)
  (set (make-local-variable 'isearch-wrap-function)
       'Info-isearch-wrap)
  (set (make-local-variable 'isearch-push-state-function)
       'Info-isearch-push-state)
  (set (make-local-variable 'isearch-filter-predicate)
       'Info-isearch-filter)
  (set (make-local-variable 'search-whitespace-regexp)
       Info-search-whitespace-regexp)
  (set (make-local-variable 'revert-buffer-function)
       'Info-revert-buffer-function)
  (Info-set-mode-line)
  (set (make-local-variable 'bookmark-make-record-function)
       'Info-bookmark-make-record))

;; When an Info buffer is killed, make sure the associated tags buffer
;; is killed too.
(defun Info-kill-buffer ()
  (and (eq major-mode 'Info-mode)
       Info-tag-table-buffer
       (kill-buffer Info-tag-table-buffer)))

;; Placed on `clone-buffer-hook'.
(defun Info-clone-buffer ()
  (when (bufferp Info-tag-table-buffer)
    (setq Info-tag-table-buffer
	  (with-current-buffer Info-tag-table-buffer (clone-buffer))))
  (let ((m Info-tag-table-marker))
    (when (markerp m)
      (setq Info-tag-table-marker
	    (if (and (marker-position m) (bufferp Info-tag-table-buffer))
		(with-current-buffer Info-tag-table-buffer
		  (copy-marker (marker-position m)))
	      (make-marker))))))

(defvar Info-edit-map (let ((map (make-sparse-keymap)))
			(set-keymap-parent map text-mode-map)
			(define-key map "\C-c\C-c" 'Info-cease-edit)
			map)
  "Local keymap used within `e' command of Info.")

;; Info-edit mode is suitable only for specially formatted data.
(put 'Info-edit-mode 'mode-class 'special)

(defun Info-edit-mode ()
  "Major mode for editing the contents of an Info node.
Like text mode with the addition of `Info-cease-edit'
which returns to Info mode for browsing.
\\{Info-edit-map}"
  (use-local-map Info-edit-map)
  (setq major-mode 'Info-edit-mode)
  (setq mode-name "Info Edit")
  (kill-local-variable 'mode-line-buffer-identification)
  (setq buffer-read-only nil)
  (force-mode-line-update)
  (buffer-enable-undo (current-buffer))
  (run-mode-hooks 'Info-edit-mode-hook))

(defun Info-edit ()
  "Edit the contents of this Info node.
Allowed only if variable `Info-enable-edit' is non-nil."
  (interactive)
  (or Info-enable-edit
      (error "Editing Info nodes is not enabled"))
  (Info-edit-mode)
  (message "%s" (substitute-command-keys
		 "Editing: Type \\<Info-edit-map>\\[Info-cease-edit] to return to info")))

(defun Info-cease-edit ()
  "Finish editing Info node; switch back to Info proper."
  (interactive)
  ;; Do this first, so nothing has changed if user C-g's at query.
  (and (buffer-modified-p)
       (y-or-n-p "Save the file? ")
       (save-buffer))
  (use-local-map Info-mode-map)
  (setq major-mode 'Info-mode)
  (setq mode-name "Info")
  (Info-set-mode-line)
  (setq buffer-read-only t)
  (force-mode-line-update)
  (and (marker-position Info-tag-table-marker)
       (buffer-modified-p)
       (message "Tags may have changed.  Use Info-tagify if necessary")))

(defvar Info-file-list-for-emacs
  '("ediff" "eudc" "forms" "gnus" "info" ("Info" . "info") ("mh" . "mh-e")
    "sc" "message" ("dired" . "dired-x") "viper" "vip" "idlwave"
    ("c" . "ccmode") ("c++" . "ccmode") ("objc" . "ccmode")
    ("java" . "ccmode") ("idl" . "ccmode") ("pike" . "ccmode")
    ("skeleton" . "autotype") ("auto-insert" . "autotype")
    ("copyright" . "autotype") ("executable" . "autotype")
    ("time-stamp" . "autotype") ("quickurl" . "autotype")
    ("tempo" . "autotype") ("hippie-expand" . "autotype")
    ("cvs" . "pcl-cvs") ("ada" . "ada-mode") "calc"
    ("calcAlg" . "calc") ("calcDigit" . "calc") ("calcVar" . "calc")
    "ebrowse" "eshell" "cl" "reftex" "speedbar" "widget" "woman"
    ("mail-header" . "emacs-mime") ("mail-content" . "emacs-mime")
    ("mail-encode" . "emacs-mime") ("mail-decode" . "emacs-mime")
    ("rfc2045" . "emacs-mime")
    ("rfc2231" . "emacs-mime")  ("rfc2047" . "emacs-mime")
    ("rfc2045" . "emacs-mime") ("rfc1843" . "emacs-mime")
    ("ietf-drums" . "emacs-mime")  ("quoted-printable" . "emacs-mime")
    ("binhex" . "emacs-mime") ("uudecode" . "emacs-mime")
    ("mailcap" . "emacs-mime") ("mm" . "emacs-mime")
    ("mml" . "emacs-mime"))
  "List of Info files that describe Emacs commands.
An element can be a file name, or a list of the form (PREFIX . FILE)
where PREFIX is a name prefix and FILE is the file to look in.
If the element is just a file name, the file name also serves as the prefix.")

(defun Info-find-emacs-command-nodes (command)
  "Return a list of locations documenting COMMAND.
The `info-file' property of COMMAND says which Info manual to search.
If COMMAND has no property, the variable `Info-file-list-for-emacs'
defines heuristics for which Info manual to try.
The locations are of the format used in `Info-history', i.e.
\(FILENAME NODENAME BUFFERPOS), where BUFFERPOS is the line number
in the first element of the returned list (which is treated specially in
`Info-goto-emacs-command-node'), and 0 for the rest elements of a list."
  (let ((where '()) line-number
	(cmd-desc (concat "^\\* +" (regexp-quote (symbol-name command))
			  "\\( <[0-9]+>\\)?:\\s *\\(.*\\)\\."
			  "\\(?:[ \t\n]+(line +\\([0-9]+\\))\\)?"))
	(info-file "emacs"))		;default
    ;; Determine which Info file this command is documented in.
    (if (get command 'info-file)
	(setq info-file (get command 'info-file))
      ;; If it doesn't say explicitly, test its name against
      ;; various prefixes that we know.
      (let ((file-list Info-file-list-for-emacs))
	(while file-list
	  (let* ((elt (car file-list))
		 (name (if (consp elt)
			   (car elt)
			 elt))
		 (file (if (consp elt) (cdr elt) elt))
		 (case-fold-search nil)
		 (regexp (concat "\\`" (regexp-quote name)
				 "\\(\\'\\|-\\)")))
	    (if (string-match regexp (symbol-name command))
		(setq info-file file file-list nil))
	    (setq file-list (cdr file-list))))))
    (Info-find-node info-file "Top")
    ;; Bind Info-history to nil, to prevent the index nodes from
    ;; getting into the node history.
    (let ((Info-history nil)
          (Info-history-list nil)
	  node (nodes (Info-index-nodes)))
      (Info-goto-node (car nodes))
      (while
	  (progn
	    (goto-char (point-min))
	    (while (re-search-forward cmd-desc nil t)
	      (setq where
		    (cons (list Info-current-file
				(match-string-no-properties 2)
				0)
			  where))
	      (setq line-number (and (match-beginning 3)
				     (string-to-number (match-string 3)))))
	    (and (setq nodes (cdr nodes) node (car nodes))))
	(Info-goto-node node)))
    (if (and line-number where)
	(cons (list (nth 0 (car where)) (nth 1 (car where)) line-number)
	      (cdr where))
      where)))

;;;###autoload (put 'Info-goto-emacs-command-node 'info-file (purecopy "emacs"))
;;;###autoload
(defun Info-goto-emacs-command-node (command)
  "Go to the Info node in the Emacs manual for command COMMAND.
The command is found by looking up in Emacs manual's indices
or in another manual found via COMMAND's `info-file' property or
the variable `Info-file-list-for-emacs'.
COMMAND must be a symbol or string."
  (interactive "CFind documentation for command: ")
  ;; If command is given as a string, convert it to a symbol.
  (if (stringp command)
      (setq command (intern command)))
  (or (commandp command)
      (signal 'wrong-type-argument (list 'commandp command)))
  (let ((where (Info-find-emacs-command-nodes command)))
    (if where
	(let ((num-matches (length where)))
	  ;; Get Info running, and pop to it in another window.
	  (save-window-excursion
	    (info))
	  (or (eq major-mode 'Info-mode) (pop-to-buffer "*info*"))
	  ;; Bind Info-history to nil, to prevent the last Index node
	  ;; visited by Info-find-emacs-command-nodes from being
	  ;; pushed onto the history.
	  (let ((Info-history nil) (Info-history-list nil)
		(line-number (nth 2 (car where))))
	    (Info-find-node (nth 0 (car where)) (nth 1 (car where)))
	    (if (and (integerp line-number) (> line-number 0))
		(forward-line (1- line-number))))
	  (if (> num-matches 1)
	      (progn
		;; (car where) will be pushed onto Info-history
		;; when/if they go to another node.  Put the other
		;; nodes that were found on the history.
		(setq Info-history (nconc (cdr where) Info-history))
		(message "Found %d other entr%s.  Use %s to see %s."
			 (1- num-matches)
			 (if (> num-matches 2) "ies" "y")
			 (substitute-command-keys "\\[Info-history-back]")
			 (if (> num-matches 2) "them" "it")))))
      (error "Couldn't find documentation for %s" command))))

;;;###autoload (put 'Info-goto-emacs-key-command-node 'info-file (purecopy "emacs"))
;;;###autoload
(defun Info-goto-emacs-key-command-node (key)
  "Go to the node in the Emacs manual which describes the command bound to KEY.
KEY is a string.
Interactively, if the binding is `execute-extended-command', a command is read.
The command is found by looking up in Emacs manual's indices
or in another manual found via COMMAND's `info-file' property or
the variable `Info-file-list-for-emacs'."
  (interactive "kFind documentation for key: ")
  (let ((command (key-binding key)))
    (cond ((null command)
	   (message "%s is undefined" (key-description key)))
	  ((and (called-interactively-p 'interactive)
		(eq command 'execute-extended-command))
	   (Info-goto-emacs-command-node
	    (read-command "Find documentation for command: ")))
	  (t
	   (Info-goto-emacs-command-node command)))))

(defvar Info-next-link-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [header-line mouse-1] 'Info-next)
    (define-key keymap [header-line mouse-2] 'Info-next)
    (define-key keymap [header-line down-mouse-1] 'ignore)
    (define-key keymap [mouse-2] 'Info-next)
    (define-key keymap [follow-link] 'mouse-face)
    keymap)
  "Keymap to put on the Next link in the text or the header line.")

(defvar Info-prev-link-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [header-line mouse-1] 'Info-prev)
    (define-key keymap [header-line mouse-2] 'Info-prev)
    (define-key keymap [header-line down-mouse-1] 'ignore)
    (define-key keymap [mouse-2] 'Info-prev)
    (define-key keymap [follow-link] 'mouse-face)
    keymap)
  "Keymap to put on the Prev link in the text or the header line.")

(defvar Info-up-link-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [header-line mouse-1] 'Info-up)
    (define-key keymap [header-line mouse-2] 'Info-up)
    (define-key keymap [header-line down-mouse-1] 'ignore)
    (define-key keymap [mouse-2] 'Info-up)
    (define-key keymap [follow-link] 'mouse-face)
    keymap)
  "Keymap to put on the Up link in the text or the header line.")

(defvar Info-link-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [header-line mouse-1] 'Info-mouse-follow-link)
    (define-key keymap [header-line mouse-2] 'Info-mouse-follow-link)
    (define-key keymap [header-line down-mouse-1] 'ignore)
    (define-key keymap [mouse-2] 'Info-mouse-follow-link)
    (define-key keymap [follow-link] 'mouse-face)
    keymap)
  "Keymap to put on the link in the text or the header line.")

(defun Info-breadcrumbs ()
  (let ((nodes (Info-toc-nodes Info-current-file))
	(node Info-current-node)
        (crumbs ())
        (depth Info-breadcrumbs-depth)
	line)

    ;; Get ancestors from the cached parent-children node info
    (while (and (not (equal "Top" node)) (> depth 0))
      (setq node (nth 1 (assoc node nodes)))
      (if node (push node crumbs))
      (setq depth (1- depth)))

    ;; Add bottom node.
    (when Info-use-header-line
      ;; Let it disappear if crumbs is nil.
      (nconc crumbs (list Info-current-node)))
    (when (or Info-use-header-line crumbs)
      ;; Add top node (and continuation if needed).
      (setq crumbs
	    (cons "Top" (if (member (pop crumbs) '(nil "Top"))
			    crumbs (cons nil crumbs))))
      ;; Eliminate duplicate.
      (forward-line 1)
      (dolist (node crumbs)
	(let ((text
	       (if (not (equal node "Top")) node
		 (format "(%s)Top"
			 (if (stringp Info-current-file)
			     (file-name-nondirectory Info-current-file)
			   ;; Some legacy code can still use a symbol.
			   Info-current-file)))))
	  (setq line (concat
		      line
		      (if (null line) "" " > ")
		      (cond
		       ((null node) "...")
		       ((equal node Info-current-node)
			;; No point linking to ourselves.
			(propertize text 'font-lock-face 'info-header-node))
		       (t
			(propertize text
				    'mouse-face 'highlight
				    'font-lock-face 'info-header-xref
				    'help-echo "mouse-2: Go to node"
				    'keymap Info-link-keymap
				    'link-args text)))))))
      (setq line (concat line "\n")))
    ;; (font-lock-append-text-property 0 (length line)
    ;; 				    'font-lock-face 'header-line line)
    line))

(defun Info-fontify-node ()
  "Fontify the node."
  (save-excursion
    (let* ((inhibit-read-only t)
           (case-fold-search t)
           paragraph-markers
           (not-fontified-p ; the node hasn't already been fontified
            (not (let ((where (next-single-property-change (point-min)
							   'font-lock-face)))
                   (and where (not (= where (point-max)))))))
           (fontify-visited-p ; visited nodes need to be re-fontified
            (and Info-fontify-visited-nodes
                 ;; Don't take time to refontify visited nodes in huge nodes
		 Info-fontify-maximum-menu-size
                 (< (- (point-max) (point-min)) Info-fontify-maximum-menu-size)))
           rbeg rend)

      ;; Fontify header line
      (goto-char (point-min))
      (when (and not-fontified-p (looking-at "^\\(File: [^,: \t]+,?[ \t]+\\)?"))
        (goto-char (match-end 0))
        (while (looking-at "[ \t]*\\([^:, \t\n]+\\):[ \t]+\\([^:,\t\n]+\\),?")
          (goto-char (match-end 0))
          (let* ((nbeg (match-beginning 2))
                 (nend (match-end 2))
                 (tbeg (match-beginning 1))
                 (tag (match-string 1)))
            (if (string-equal (downcase tag) "node")
                (put-text-property nbeg nend 'font-lock-face 'info-header-node)
              (put-text-property nbeg nend 'font-lock-face 'info-header-xref)
              (put-text-property tbeg nend 'mouse-face 'highlight)
              (put-text-property tbeg nend
                                 'help-echo
                                 (concat "mouse-2: Go to node "
                                         (buffer-substring nbeg nend)))
              ;; Always set up the text property keymap.
              ;; It will either be used in the buffer
              ;; or copied in the header line.
              (put-text-property
	       tbeg nend 'keymap
	       (cond
		((string-equal (downcase tag) "prev") Info-prev-link-keymap)
		((string-equal (downcase tag) "next") Info-next-link-keymap)
		((string-equal (downcase tag) "up"  ) Info-up-link-keymap))))))

        ;; (when (> Info-breadcrumbs-depth 0)
        ;;   (insert (Info-breadcrumbs)))

        ;; Treat header line.
        (when Info-use-header-line
          (goto-char (point-min))
          (let* ((header-end (line-end-position))
                 (header
                  ;; If we find neither Next: nor Prev: link, show the entire
                  ;; node header.  Otherwise, don't show the File: and Node:
                  ;; parts, to avoid wasting precious space on information that
                  ;; is available in the mode line.
                  (if (re-search-forward
                       "\\(next\\|up\\|prev[ious]*\\): "
                       header-end t)
                      (progn
                        (goto-char (match-beginning 1))
                        (buffer-substring (point) header-end))
                    (if (re-search-forward "node:[ \t]*[^ \t]+[ \t]*"
                                           header-end t)
                        (concat "No next, prev or up links  --  "
                                (buffer-substring (point) header-end))
                      (buffer-substring (point) header-end)))))
            (put-text-property (point-min) (1+ (point-min))
                               'header-line
			       (replace-regexp-in-string
				"%"
				;; Preserve text properties on duplicated `%'.
				(lambda (s) (concat s s)) header))
            ;; Hide the part of the first line
            ;; that is in the header, if it is just part.
            (cond
             ((> Info-breadcrumbs-depth 0)
	      (let ((ov (make-overlay (point-min) (1+ header-end))))
		(overlay-put ov 'display (Info-breadcrumbs))
		(overlay-put ov 'evaporate t)))
             ((not (bobp))
              ;; Hide the punctuation at the end, too.
              (skip-chars-backward " \t,")
              (put-text-property (point) header-end 'invisible t))))))

      ;; Fontify titles
      (goto-char (point-min))
      (when (and font-lock-mode not-fontified-p)
        (while (and (re-search-forward "\n\\([^ \t\n].+\\)\n\\(\\*\\*+\\|==+\\|--+\\|\\.\\.+\\)$"
                                       nil t)
                    ;; Only consider it as an underlined title if the ASCII
                    ;; underline has the same size as the text.  A typical
                    ;; counter example is when a continuation "..." is alone
                    ;; on a line.
                    (= (string-width (match-string 1))
                       (string-width (match-string 2))))
          (let* ((c (preceding-char))
                 (face
                  (cond ((= c ?*) 'info-title-1)
                        ((= c ?=) 'info-title-2)
                        ((= c ?-) 'info-title-3)
                        (t        'info-title-4))))
            (put-text-property (match-beginning 1) (match-end 1)
                               'font-lock-face face))
          ;; This is a serious problem for trying to handle multiple
          ;; frame types at once.  We want this text to be invisible
          ;; on frames that can display the font above.
          (when (memq (framep (selected-frame)) '(x pc w32 ns))
            (add-text-properties (1- (match-beginning 2)) (match-end 2)
                                 '(invisible t front-sticky nil rear-nonsticky t)))))

      ;; Fontify cross references
      (goto-char (point-min))
      (when (or not-fontified-p fontify-visited-p)
        (while (re-search-forward "\\(\\*Note[ \n\t]+\\)\\([^:]*\\)\\(:[ \t]*\\([^.,:(]*\\)\\(\\(([^)]*)\\)[^.,:]*\\)?[,:]?\n?\\)" nil t)
          (let ((start (match-beginning 0))
                (next (point))
                other-tag)
            (when not-fontified-p
              (when Info-hide-note-references
                (when (and (not (eq Info-hide-note-references 'hide))
                           (> (line-number-at-pos) 4)) ; Skip breadcrumbs
                  ;; *Note is often used where *note should have been
                  (goto-char start)
                  (skip-syntax-backward " ")
		  (when (memq (char-before) '(?\( ?\[ ?\{))
		    ;; Check whether the paren is preceded by
		    ;; an end of sentence
		    (skip-syntax-backward " ("))
                  (setq other-tag
			(cond ((save-match-data (looking-back "\\<see"))
			       "")
			      ((save-match-data (looking-back "\\<in"))
			       "")
			      ((memq (char-before) '(nil ?\. ?! ??))
                               "See ")
			      ((save-match-data
				 (save-excursion
				   (search-forward "\n\n" start t)))
			       "See ")
			      (t "see "))))
                (goto-char next)
                (add-text-properties
                 (match-beginning 1)
                 (or (save-match-data
                       ;; Don't hide \n after *Note
                       (let ((start1 (match-beginning 1)))
                         (if (string-match "\n" (match-string 1))
                             (+ start1 (match-beginning 0)))))
                     (match-end 1))
                 (if other-tag
                     `(display ,other-tag front-sticky nil rear-nonsticky t)
                   '(invisible t front-sticky nil rear-nonsticky t))))
              (add-text-properties
               (match-beginning 2) (match-end 2)
               (list
                'help-echo (if (or (match-end 5)
                                   (not (equal (match-string 4) "")))
                               (concat "mouse-2: go to " (or (match-string 5)
                                                             (match-string 4)))
                             "mouse-2: go to this node")
                'mouse-face 'highlight)))
            (when (or not-fontified-p fontify-visited-p)
              (setq rbeg (match-beginning 2)
                    rend (match-end 2))
              (put-text-property
               rbeg rend
               'font-lock-face
               ;; Display visited nodes in a different face
               (if (and Info-fontify-visited-nodes
                        (save-match-data
                          (let* ((node (replace-regexp-in-string
                                        "^[ \t]+" ""
                                        (replace-regexp-in-string
                                         "[ \t\n]+" " "
                                         (or (match-string-no-properties 5)
                                             (and (not (equal (match-string 4) ""))
                                                  (match-string-no-properties 4))
                                             (match-string-no-properties 2)))))
				 (external-link-p
				  (string-match "(\\([^)]+\\))\\([^)]*\\)" node))
                                 (file (if external-link-p
					   (file-name-nondirectory
					    (match-string-no-properties 1 node))
					 Info-current-file))
                                 (hl Info-history-list)
                                 res)
                            (if external-link-p
				(setq node (if (equal (match-string 2 node) "")
                                               "Top"
                                             (match-string-no-properties 2 node))))
			    (while hl
			      (if (and (string-equal node (nth 1 (car hl)))
				       (equal file
					      (if (and external-link-p
						       (stringp (caar hl)))
						  (file-name-nondirectory
						   (caar hl))
						(caar hl))))
				  (setq res (car hl) hl nil)
				(setq hl (cdr hl))))
                            res))) 'info-xref-visited 'info-xref))
              ;; For multiline ref, unfontify newline and surrounding whitespace
              (save-excursion
                (goto-char rbeg)
                (save-match-data
                  (while (re-search-forward "\\s-*\n\\s-*" rend t nil)
                    (remove-text-properties (match-beginning 0)
                                            (match-end 0)
                                            '(font-lock-face t))))))
            (when not-fontified-p
              (when (memq Info-hide-note-references '(t hide))
                (add-text-properties (match-beginning 3) (match-end 3)
                                     '(invisible t front-sticky nil rear-nonsticky t))
                ;; Unhide the file name of the external reference in parens
                (if (and (match-string 6) (not (eq Info-hide-note-references 'hide)))
                    (remove-text-properties (match-beginning 6) (match-end 6)
                                            '(invisible t front-sticky nil rear-nonsticky t)))
                ;; Unhide newline because hidden newlines cause too long lines
                (save-match-data
                  (let ((beg3 (match-beginning 3))
                        (end3 (match-end 3)))
                    (if (and (string-match "\n[ \t]*" (match-string 3))
                             (not (save-match-data
                                    (save-excursion
                                      (goto-char (1+ end3))
                                      (looking-at "[.)]*$")))))
                        (remove-text-properties (+ beg3 (match-beginning 0))
                                                (+ beg3 (match-end 0))
                                                '(invisible t front-sticky nil rear-nonsticky t))))))
              (when (and Info-refill-paragraphs Info-hide-note-references)
                (push (set-marker (make-marker) start)
                      paragraph-markers))))))

      ;; Refill paragraphs (experimental feature)
      (when (and not-fontified-p
                 Info-refill-paragraphs
                 paragraph-markers)
        (let ((fill-nobreak-invisible t)
              (fill-individual-varying-indent nil)
              (paragraph-start "\f\\|[ \t]*[-*]\\|[ \t]*$")
              (paragraph-separate ".*\\.[ \t]*\n[ \t]\\|[ \t]*[-*]\\|[ \t\f]*$")
              (adaptive-fill-mode nil))
          (goto-char (point-max))
          (dolist (m paragraph-markers)
            (when (< m (point))
              (goto-char m)
              (beginning-of-line)
              (let ((beg (point)))
                (when (zerop (forward-paragraph))
                  (fill-individual-paragraphs beg (point) nil nil)
                  (goto-char beg))))
            (set-marker m nil))))

      ;; Fontify menu items
      (goto-char (point-min))
      (when (and (or not-fontified-p fontify-visited-p)
                 (search-forward "\n* Menu:" nil t)
                 ;; Don't take time to annotate huge menus
		 Info-fontify-maximum-menu-size
                 (< (- (point-max) (point)) Info-fontify-maximum-menu-size))
        (let ((n 0)
              cont)
          (while (re-search-forward
                  (concat "^\\* Menu:\\|\\(?:^\\* +\\(" Info-menu-entry-name-re "\\)\\(:"
                          Info-node-spec-re "\\([ \t]*\\)\\)\\)")
                  nil t)
	    (when (match-beginning 1)
	      (when not-fontified-p
		(setq n (1+ n))
		(if (and (<= n 9) (zerop (% n 3))) ; visual aids to help with 1-9 keys
		    (put-text-property (match-beginning 0)
				       (1+ (match-beginning 0))
				       'font-lock-face 'info-menu-star)))
	      (when not-fontified-p
		(add-text-properties
		 (match-beginning 1) (match-end 1)
		 (list
		  'help-echo (if (and (match-end 3)
				      (not (equal (match-string 3) "")))
				 (concat "mouse-2: go to " (match-string 3))
			       "mouse-2: go to this node")
		  'mouse-face 'highlight)))
	      (when (or not-fontified-p fontify-visited-p)
		(put-text-property
		 (match-beginning 1) (match-end 1)
                 'font-lock-face
                 ;; Display visited menu items in a different face
                 (if (and Info-fontify-visited-nodes
                          (save-match-data
                            (let* ((node (if (equal (match-string 3) "")
					     (match-string-no-properties 1)
					   (match-string-no-properties 3)))
				   (external-link-p
				    (string-match "(\\([^)]+\\))\\([^)]*\\)" node))
				   (file (if external-link-p
					     (file-name-nondirectory
					      (match-string-no-properties 1 node))
					   Info-current-file))
				   (hl Info-history-list)
				   res)
                              (if external-link-p
                                  (setq node (if (equal (match-string 2 node) "")
                                                 "Top"
                                               (match-string-no-properties 2 node))))
			      (while hl
				(if (and (string-equal node (nth 1 (car hl)))
					 (equal file
						(if (and external-link-p
							 (stringp (caar hl)))
						    (file-name-nondirectory
						     (caar hl))
						  (caar hl))))
				    (setq res (car hl) hl nil)
				  (setq hl (cdr hl))))
                              res))) 'info-xref-visited 'info-xref)))
	      (when (and not-fontified-p
			 (memq Info-hide-note-references '(t hide))
			 (not (Info-index-node)))
		(put-text-property (match-beginning 2) (1- (match-end 6))
				   'invisible t)
		;; Unhide the file name in parens
		(if (and (match-end 4) (not (eq (char-after (match-end 4)) ?.)))
		    (remove-text-properties (match-beginning 4) (match-end 4)
					    '(invisible t)))
		;; We need a stretchable space like :align-to but with
		;; a minimum value.
		(put-text-property (1- (match-end 6)) (match-end 6) 'display
				   (if (>= 22 (- (match-end 1)
						 (match-beginning 0)))
				       '(space :align-to 24)
				     '(space :width 2)))
		(setq cont (looking-at "."))
		(while (and (= (forward-line 1) 0)
			    (looking-at "\\([ \t]+\\)[^*\n]"))
		  (put-text-property (match-beginning 1) (1- (match-end 1))
				     'invisible t)
		  (put-text-property (1- (match-end 1)) (match-end 1)
				     'display
				     (if cont
					 '(space :align-to 26)
				       '(space :align-to 24)))
		  (setq cont t)))))))

      ;; Fontify menu headers
      ;; Add the face `info-menu-header' to any header before a menu entry
      (goto-char (point-min))
      (when (and not-fontified-p (re-search-forward "^\\* Menu:" nil t))
        (put-text-property (match-beginning 0) (match-end 0)
                           'font-lock-face 'info-menu-header)
        (while (re-search-forward "\n\n\\([^*\n ].*\\)\n\n?[*]" nil t)
          (put-text-property (match-beginning 1) (match-end 1)
                             'font-lock-face 'info-menu-header)))

      ;; Hide index line numbers
      (goto-char (point-min))
      (when (and not-fontified-p (Info-index-node))
        (while (re-search-forward "[ \t\n]*(line +[0-9]+)" nil t)
          (put-text-property (match-beginning 0) (match-end 0)
                             'invisible t)))

      ;; Fontify http and ftp references
      (goto-char (point-min))
      (when not-fontified-p
        (while (re-search-forward "\\(https?\\|ftp\\)://[^ \t\n\"`({<>})']+"
                                  nil t)
          (add-text-properties (match-beginning 0) (match-end 0)
                               '(font-lock-face info-xref
                                 mouse-face highlight
                                 help-echo "mouse-2: go to this URL"))))

      (set-buffer-modified-p nil))))

;;; Speedbar support:
;; These functions permit speedbar to display the "tags" in the
;; current Info node.
(eval-when-compile (require 'speedbar))

(defvar Info-speedbar-key-map nil
  "Keymap used when in the Info display mode.")

(defun Info-install-speedbar-variables ()
  "Install those variables used by speedbar to enhance Info."
  (if Info-speedbar-key-map
      nil
    (setq Info-speedbar-key-map (speedbar-make-specialized-keymap))

    ;; Basic tree features
    (define-key Info-speedbar-key-map "e" 'speedbar-edit-line)
    (define-key Info-speedbar-key-map "\C-m" 'speedbar-edit-line)
    (define-key Info-speedbar-key-map "+" 'speedbar-expand-line)
    (define-key Info-speedbar-key-map "-" 'speedbar-contract-line)
    )

  (speedbar-add-expansion-list '("Info" Info-speedbar-menu-items
				 Info-speedbar-key-map
				 Info-speedbar-hierarchy-buttons)))

(defvar Info-speedbar-menu-items
  '(["Browse Node" speedbar-edit-line t]
    ["Expand Node" speedbar-expand-line
     (save-excursion (beginning-of-line)
		     (looking-at "[0-9]+: *.\\+. "))]
    ["Contract Node" speedbar-contract-line
     (save-excursion (beginning-of-line)
		     (looking-at "[0-9]+: *.-. "))]
    )
  "Additional menu-items to add to speedbar frame.")

;; Make sure our special speedbar major mode is loaded
(if (featurep 'speedbar)
    (Info-install-speedbar-variables)
  (add-hook 'speedbar-load-hook 'Info-install-speedbar-variables))

;;; Info hierarchy display method
;;;###autoload
(defun Info-speedbar-browser ()
  "Initialize speedbar to display an Info node browser.
This will add a speedbar major display mode."
  (interactive)
  (require 'speedbar)
  ;; Make sure that speedbar is active
  (speedbar-frame-mode 1)
  ;; Now, throw us into Info mode on speedbar.
  (speedbar-change-initial-expansion-list "Info")
  )

(defun Info-speedbar-hierarchy-buttons (_directory depth &optional node)
  "Display an Info directory hierarchy in speedbar.
DIRECTORY is the current directory in the attached frame.
DEPTH is the current indentation depth.
NODE is an optional argument that is used to represent the
specific node to expand."
  (if (and (not node)
	   (save-excursion (goto-char (point-min))
			   (let ((case-fold-search t))
			     (looking-at "Info Nodes:"))))
      ;; Update our "current node" maybe?
      nil
    ;; We cannot use the generic list code, that depends on all leaves
    ;; being known at creation time.
    (if (not node)
	(speedbar-with-writable (insert "Info Nodes:\n")))
    (let ((completions nil))
      (speedbar-select-attached-frame)
      (save-window-excursion
	(setq completions
	      (Info-speedbar-fetch-file-nodes (or node '"(dir)top"))))
      (select-frame (speedbar-current-frame))
      (if completions
	  (speedbar-with-writable
	   (dolist (completion completions)
	     (speedbar-make-tag-line 'bracket ?+ 'Info-speedbar-expand-node
				     (cdr completion)
				     (car completion)
				     'Info-speedbar-goto-node
				     (cdr completion)
				     'info-xref depth))
	   t)
	nil))))

(defun Info-speedbar-goto-node (_text node _indent)
  "When user clicks on TEXT, go to an info NODE.
The INDENT level is ignored."
  (speedbar-select-attached-frame)
  (let* ((buff (or (get-buffer "*info*")
		   (progn (info) (get-buffer "*info*"))))
	 (bwin (get-buffer-window buff 0)))
    (if bwin
	(progn
	  (select-window bwin)
	  (raise-frame (window-frame bwin)))
      (if speedbar-power-click
	  (switch-to-buffer-other-frame buff)
	(speedbar-select-attached-frame)
	(switch-to-buffer buff)))
    (if (not (string-match "^(\\([^)]+\\))\\([^.]+\\)$" node))
	(error "Invalid node %s" node)
      (Info-find-node (match-string 1 node) (match-string 2 node))
      ;; If we do a find-node, and we were in info mode, restore
      ;; the old default method.  Once we are in info mode, it makes
      ;; sense to return to whatever method the user was using before.
      (if (string= speedbar-initial-expansion-list-name "Info")
	  (speedbar-change-initial-expansion-list
	   speedbar-previously-used-expansion-list-name)))))

(defun Info-speedbar-expand-node (text token indent)
  "Expand the node the user clicked on.
TEXT is the text of the button we clicked on, a + or - item.
TOKEN is data related to this node (NAME . FILE).
INDENT is the current indentation depth."
  (cond ((string-match "+" text)	;we have to expand this file
	 (speedbar-change-expand-button-char ?-)
	 (if (speedbar-with-writable
	      (save-excursion
		(end-of-line) (forward-char 1)
		(Info-speedbar-hierarchy-buttons nil (1+ indent) token)))
	     (speedbar-change-expand-button-char ?-)
	   (speedbar-change-expand-button-char ??)))
	((string-match "-" text)	;we have to contract this node
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent))
	(t (error "Ooops... not sure what to do")))
  (speedbar-center-buffer-smartly))

(defun Info-speedbar-fetch-file-nodes (nodespec)
  "Fetch the subnodes from the info NODESPEC.
NODESPEC is a string of the form: (file)node."
  ;; Set up a buffer we can use to fake-out Info.
  (with-current-buffer (get-buffer-create " *info-browse-tmp*")
    (if (not (equal major-mode 'Info-mode))
	(Info-mode))
    ;; Get the node into this buffer
    (if (not (string-match "^(\\([^)]+\\))\\([^.]+\\)$" nodespec))
	(error "Invalid node specification %s" nodespec)
      (Info-find-node (match-string 1 nodespec) (match-string 2 nodespec)))
    ;; Scan the created buffer
    (goto-char (point-min))
    (let ((completions nil)
	  (case-fold-search t)
	  (thisfile (progn (string-match "^(\\([^)]+\\))" nodespec)
			   (match-string 1 nodespec))))
      ;; Always skip the first one...
      (re-search-forward "\n\\* \\([^:\t\n]*\\):" nil t)
      (while (re-search-forward "\n\\* \\([^:\t\n]*\\):" nil t)
	(let ((name (match-string 1)))
	  (push (cons name
		      (if (looking-at " *\\(([^)]+)[^.\n]+\\)\\.")
			  (match-string 1)
			(if (looking-at " *\\(([^)]+)\\)\\.")
			    (concat (match-string 1) "Top")
			  (concat "(" thisfile ")"
				  (if (looking-at " \\([^.]+\\).")
				      (match-string 1)
				    name)))))
		completions)))
      (nreverse completions))))

;;; Info mode node listing
;; This is called by `speedbar-add-localized-speedbar-support'
(defun Info-speedbar-buttons (_buffer)
  "Create a speedbar display to help navigation in an Info file.
BUFFER is the buffer speedbar is requesting buttons for."
  (if (save-excursion (goto-char (point-min))
		      (let ((case-fold-search t))
			(not (looking-at "Info Nodes:"))))
      (erase-buffer))
  (Info-speedbar-hierarchy-buttons nil 0))

(dolist (mess '("^First node in file$"
		"^No `.*' in index$"
		"^No cross-reference named"
		"^No cross.references in this node$"
		"^No current Info node$"
		"^No menu in this node$"
		"^No more items in menu$"
		"^No more nodes$"
		"^No pointer \\(?:forward\\|backward\\) from this node$"
		"^No previous `i' command$"
		"^No previous items in menu$"
		"^No previous nodes$"
		"^No such item in menu$"
		"^No such node or anchor"
		"^Node has no"
		"^Point neither on reference nor in menu item description$"
		"^This is the \\(?:first\\|last\\) Info node you looked at$"
		search-failed))
  (add-to-list 'debug-ignored-errors mess))

;;;;  Desktop support

(defun Info-desktop-buffer-misc-data (_desktop-dirname)
  "Auxiliary information to be saved in desktop file."
  (list Info-current-file
	Info-current-node
	;; Additional data as an association list.
	(delq nil (list
		   (and Info-history
			(cons 'history Info-history))
		   (and (Info-virtual-fun
			 'slow Info-current-file Info-current-node)
			(cons 'slow t))))))

(defun Info-restore-desktop-buffer (_desktop-buffer-file-name
                                    desktop-buffer-name
                                    desktop-buffer-misc)
  "Restore an Info buffer specified in a desktop file."
  (let* ((file (nth 0 desktop-buffer-misc))
	 (node (nth 1 desktop-buffer-misc))
	 (data (nth 2 desktop-buffer-misc))
	 (hist (assq 'history data))
	 (slow (assq 'slow data)))
    ;; Don't restore nodes slow to regenerate.
    (unless slow
      (when (and file node)
	(when desktop-buffer-name
	  (set-buffer (get-buffer-create desktop-buffer-name))
	  (Info-mode))
	(Info-find-node file node)
	(when hist
	  (setq Info-history (cdr hist)))
	(current-buffer)))))

(add-to-list 'desktop-buffer-mode-handlers
	     '(Info-mode . Info-restore-desktop-buffer))

;;;; Bookmark support
(declare-function bookmark-make-record-default
                  "bookmark" (&optional no-file no-context posn))
(declare-function bookmark-prop-get "bookmark" (bookmark prop))
(declare-function bookmark-default-handler "bookmark" (bmk))
(declare-function bookmark-get-bookmark-record "bookmark" (bmk))

(defun Info-bookmark-make-record ()
  "This implements the `bookmark-make-record-function' type (which see)
for Info nodes."
  `(,Info-current-node
    ,@(bookmark-make-record-default 'no-file)
    (filename . ,Info-current-file)
    (info-node . ,Info-current-node)
    (handler . Info-bookmark-jump)))

;;;###autoload
(defun Info-bookmark-jump (bmk)
  "This implements the `handler' function interface for the record
type returned by `Info-bookmark-make-record', which see."
  (let* ((file                   (bookmark-prop-get bmk 'filename))
         (info-node              (bookmark-prop-get bmk 'info-node))
         (buf (save-window-excursion    ;FIXME: doesn't work with frames!
                (Info-find-node file info-node) (current-buffer))))
    ;; Use bookmark-default-handler to move to the appropriate location
    ;; within the node.
    (bookmark-default-handler
     `("" (buffer . ,buf) . ,(bookmark-get-bookmark-record bmk)))))


;;;###autoload
(defun info-display-manual (manual)
  "Go to Info buffer that displays MANUAL, creating it if none already exists."
  (interactive "sManual name: ")
  (let ((blist (buffer-list))
	(manual-re (concat "\\(/\\|\\`\\)" manual "\\(\\.\\|\\'\\)"))
	(case-fold-search t)
	found)
    (dolist (buffer blist)
      (with-current-buffer buffer
	(when (and (eq major-mode 'Info-mode)
		   (stringp Info-current-file)
		   (string-match manual-re Info-current-file))
	  (setq found buffer
		blist nil))))
    (if found
	(switch-to-buffer found)
      (info-initialize)
      (info (Info-find-file manual)))))

(provide 'info)

;;; info.el ends here
