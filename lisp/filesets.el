;;; filesets.el --- handle group of files

;; Copyright (C) 2002-2012 Free Software Foundation, Inc.

;; Author: Thomas Link <sanobast-emacs@yahoo.de>
;; Maintainer: FSF
;; Keywords: filesets convenience

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

;;; Code:

(defvar filesets-version "1.8.4")
(defvar filesets-homepage
  "http://members.a1.net/t.link/CompEmacsFilesets.html")

;;; Commentary:

;; Define filesets, which can be opened or saved with the power of one or
;; two mouse clicks only.  A fileset is either a list of files, a file
;; pattern, a base directory and a search pattern (for files), or an
;; inclusion group (i.e. a base file including other files).

;; Usage:
;; 1. Put (require 'filesets) and (filesets-init) in your .emacs file.
;; 2. Type ;; M-x filesets-edit or choose "Edit Filesets" from the menu.
;; 3. Save your customizations.

;; Caveat: Fileset names have to be unique.

;; Filesets.el adds a nifty filesets menu to your menubar.  If you change
;; your filesets on the fly, don't forget to select "Save Filesets" from
;; the menu.

;; Pressing on the first item in the submenu will open all files at once.
;; Define your own function, e.g. browse-url, for opening a fileset's
;; files.  Or define external viewers for opening files with other
;; programs.  See `filesets-external-viewers'.

;; BTW, if you close a fileset, files, which have been changed, will
;; be silently saved.  Change this behavior by setting
;; `filesets-save-buffer-fn'.

;;; Supported modes for inclusion groups (`filesets-ingroup-patterns'):
;; - Elisp
;; - Emacs-Wiki (simple names only)
;; - LaTeX



;;; Known bugs:


;;; To do:

;;- better handling of different customization scenarios

;; Data gathering should be better separated from building the menu
;; so that one could (1) use filesets without installing the menu
;; and (2) create new "frontends" to speedbar and others.

;; The functionality to call external viewers should be isolated in
;; an extra package and possibly integrated with the MIME
;; handling.

;;; Credits:

;; Helpful suggestions (but no significant code) were contributed by

;;- Christoph Conrad (at gmx de)
;;- Christian Ohler (at Informatik Uni-Oldenburg DE)
;;- Richard Stallman aka RMS (at gnu org)
;;- Per Abrahamsen aka abraham (at dina kvl dk)


;;; Code:

(eval-when-compile
  (require 'cl))


;;; Some variables

(defvar filesets-menu-cache nil
  "The whole filesets menu.")
(defvar filesets-cache-version nil
  "Filesets' cached version number.")
(defvar filesets-cache-hostname nil
  "Filesets' cached system name.")

(defvar filesets-ingroup-cache nil
  "A plist containing files and their ingroup data.")
(defvar filesets-ingroup-files nil
  "List of files already processed when searching for included files.")

(defvar filesets-has-changed-flag t
  "Non-nil means some fileset definition has changed.")
(defvar filesets-submenus nil
  "An association list with filesets menu data.")
(defvar filesets-updated-buffers nil
  "A list of buffers with updated menu bars.")
(defvar filesets-menu-use-cached-flag nil
  "Use cached data.  See `filesets-menu-ensure-use-cached' for details.")
(defvar filesets-update-cache-file-flag nil
  "Non-nil means the cache needs updating.")
(defvar filesets-ignore-next-set-default nil
  "List of custom variables for which the next `set-default' will be ignored.")

(defvar filesets-output-buffer-flag nil
  "Non-nil means the current buffer is an output buffer created by filesets.
Is buffer local variable.")

(defvar filesets-verbosity 1
  "An integer defining the level of verbosity.
0 means no messages at all.")

(defvar filesets-menu-ensure-use-cached
  (and (featurep 'xemacs)
       (if (fboundp 'emacs-version>=)
	   (not (emacs-version>= 21 5))))
  "Make sure (X)Emacs uses filesets' cache.

Well, if you use XEmacs (prior to 21.5?) custom.el is loaded after
init.el.  This means that settings saved in the cache file (see
`filesets-menu-cache-file') will be overwritten by custom.el.  In order
to ensure the use of the cache file, set this variable to t -- which is
the default for XEmacs prior to 21.5.  If you want to change this value
put \"(setq filesets-menu-ensure-use-cached VALUE)\" into your startup
file -- before loading filesets.el.

So, when should you think about setting this value to t? If filesets.el
is loaded before user customizations.  Thus, if (require 'filesets)
precedes the `custom-set-variables' command or, for XEmacs, if init.el
is loaded before custom.el, set this variable to t.")


;;; utils
(defun filesets-filter-list (lst cond-fn)
  "Remove all elements not conforming to COND-FN from list LST.
COND-FN takes one argument: the current element."
;  (remove* 'dummy lst :test (lambda (dummy elt)
;			      (not (funcall cond-fn elt)))))
  (let ((rv nil))
    (dolist (elt lst rv)
      (when (funcall cond-fn elt)
	(setq rv (append rv (list elt)))))))

(defun filesets-ormap (fsom-pred lst)
  "Return the tail of LST for the head of which FSOM-PRED is non-nil."
  (let ((fsom-lst lst)
	(fsom-rv nil))
    (while (and (not (null fsom-lst))
		(null fsom-rv))
      (if (funcall fsom-pred (car fsom-lst))
	  (setq fsom-rv fsom-lst)
	(setq fsom-lst (cdr fsom-lst))))
    fsom-rv))

(defun filesets-some (fss-pred fss-lst)
  "Return non-nil if FSS-PRED is non-nil for any element of FSS-LST.
Like `some', return the first value of FSS-PRED that is non-nil."
  (catch 'exit
    (dolist (fss-this fss-lst nil)
      (let ((fss-rv (funcall fss-pred fss-this)))
	(when fss-rv
	  (throw 'exit fss-rv))))))
;(fset 'filesets-some 'some) ;; or use the cl function

(defun filesets-member (fsm-item fsm-lst &rest fsm-keys)
  "Find the first occurrence of FSM-ITEM in FSM-LST.
It is supposed to work like cl's `member*'.  At the moment only the :test
key is supported."
  (let ((fsm-test (or (plist-get fsm-keys ':test)
		      (function equal))))
    (filesets-ormap (lambda (fsm-this)
		      (funcall fsm-test fsm-item fsm-this))
		    fsm-lst)))
;(fset 'filesets-member 'member*) ;; or use the cl function

(defun filesets-sublist (lst beg &optional end)
  "Get the sublist of LST from BEG to END - 1."
  (let ((rv  nil)
	(i   beg)
	(top (or end
		 (length lst))))
    (while (< i top)
      (setq rv (append rv (list (nth i lst))))
      (setq i (+ i 1)))
    rv))

(defun filesets-select-command (cmd-list)
  "Select one command from CMD-LIST -- a string with space separated names."
  (let ((this (shell-command-to-string
	       (format "which --skip-alias %s 2> /dev/null | head -n 1"
		       cmd-list))))
    (if (equal this "")
	nil
      (file-name-nondirectory (substring this 0 (- (length this) 1))))))

(defun filesets-which-command (cmd)
  "Call \"which CMD\"."
  (shell-command-to-string (format "which %s" cmd)))

(defun filesets-which-command-p (cmd)
  "Call \"which CMD\" and return non-nil if the command was found."
  (when (string-match (format "\\(/[^/]+\\)?/%s" cmd)
		      (filesets-which-command cmd))
    cmd))

(defun filesets-message (level &rest args)
  "Show a message only if LEVEL is greater or equal then `filesets-verbosity'."
  (when (<= level (abs filesets-verbosity))
    (apply 'message args)))


;;; config file
(defun filesets-save-config ()
  "Save filesets' customizations."
  (interactive)
  (customize-save-customized))

(defun filesets-reset-fileset (&optional fileset no-cache)
  "Reset the cached values for one or all filesets."
  (if fileset
      (setq filesets-submenus (lax-plist-put filesets-submenus fileset nil))
    (setq filesets-submenus nil))
  (setq filesets-has-changed-flag t)
  (setq filesets-update-cache-file-flag (or filesets-update-cache-file-flag
					    (not no-cache))))

(defun filesets-set-config (fileset var val)
  "Set-default wrapper function."
  (filesets-reset-fileset fileset)
  (set-default var val))
;  (customize-set-variable var val))
;  (filesets-build-menu))

;; It seems this is a workaround for the XEmacs issue described in the
;; doc-string of filesets-menu-ensure-use-cached.  Under Emacs this is
;; essentially just `set-default'.
(defun filesets-set-default (sym val &optional init-flag)
  "Set-default wrapper function used in conjunction with `defcustom'.
If SYM is in the list `filesets-ignore-next-set-default', delete
it from that list, and return nil.  Otherwise, set the value of
SYM to VAL and return t.  If INIT-FLAG is non-nil, set with
`custom-initialize-set', otherwise with `set-default'."
  (let ((ignore-flag (member sym filesets-ignore-next-set-default)))
    (if ignore-flag
	(setq filesets-ignore-next-set-default
	      (delete sym filesets-ignore-next-set-default))
      (if init-flag
	  (custom-initialize-set sym val)
	(set-default sym val)))
    (not ignore-flag)))

(defun filesets-set-default! (sym val)
  "Call `filesets-set-default' and reset cached data (i.e. rebuild menu)."
  (when (filesets-set-default sym val)
    (filesets-reset-fileset)))

(defun filesets-set-default+ (sym val)
  "Call `filesets-set-default' and reset filesets' standard menu."
  (when (filesets-set-default sym val)
    (setq filesets-has-changed-flag t)))
;    (filesets-reset-fileset nil t)))

(defvar filesets-data)

(defun filesets-data-set-default (sym val)
  "Set the default for `filesets-data'."
  (if filesets-menu-use-cached-flag
      (setq filesets-menu-use-cached-flag nil)
    (when (default-boundp 'filesets-data)
      (let ((modified-filesets
	     (filesets-filter-list val
				   (lambda (x)
				     (let ((name (car x))
					   (data (cdr x)))
				       (let ((elt (assoc name filesets-data)))
					 (or (not elt)
					     (not (equal data (cdr elt))))))))))
	(dolist (x modified-filesets)
	  (filesets-reset-fileset (car x))))))
  (filesets-set-default sym val))

;;; configuration
(defgroup filesets nil
  "The fileset swapper."
  :prefix "filesets-"
  :group 'convenience
  :version "22.1")

(defcustom filesets-menu-name "Filesets"
  "Filesets' menu name."
  :set (function filesets-set-default)
  :type 'string
  :group 'filesets)

(defcustom filesets-menu-path '("File")	; cf recentf-menu-path
  "The menu under which the filesets menu should be inserted.
See `add-submenu' for documentation."
  :set (function filesets-set-default)
  :type '(choice (const :tag "Top Level" nil)
		 (sexp :tag "Menu Path"))
  :version "23.1"			; was nil
  :group 'filesets)

(defcustom filesets-menu-before "Open File..." ; cf recentf-menu-before
  "The name of a menu before which this menu should be added.
See `add-submenu' for documentation."
  :set (function filesets-set-default)
  :type '(choice (string :tag "Name")
                 (const :tag "Last" nil))
  :version "23.1"			; was "File"
  :group 'filesets)

(defcustom filesets-menu-in-menu nil
  "Use that instead of `current-menubar' as the menu to change.
See `add-submenu' for documentation."
  :set (function filesets-set-default)
  :type 'sexp
  :group 'filesets)

(defcustom filesets-menu-shortcuts-flag t
  "Non-nil means to prepend menus with hopefully unique shortcuts."
  :set (function filesets-set-default!)
  :type 'boolean
  :group 'filesets)

(defcustom filesets-menu-shortcuts-marker "%_"
  "String for marking menu shortcuts."
  :set (function filesets-set-default!)
  :type 'string
  :group 'filesets)

;;(defcustom filesets-menu-cnvfp-flag nil
;;  "Non-nil means show \"Convert :pattern to :files\" entry for :pattern menus."
;;  :set (function filesets-set-default!)
;;  :type 'boolean
;;  :group 'filesets)

(defcustom filesets-menu-cache-file
  (locate-user-emacs-file "filesets-cache.el")
  "File to be used for saving the filesets menu between sessions.
Set this to \"\", to disable caching of menus.
Don't forget to check out `filesets-menu-ensure-use-cached'."
  :set (function filesets-set-default)
  :type 'file
  :group 'filesets)
(put 'filesets-menu-cache-file 'risky-local-variable t)

(defcustom filesets-menu-cache-contents
  '(filesets-be-docile-flag
    filesets-submenus
    filesets-menu-cache
    filesets-ingroup-cache)
  "Stuff we want to save in `filesets-menu-cache-file'.

Possible uses: don't save configuration data in the main startup files
but in filesets's own cache.  In this case add `filesets-data' to this
list.

There is a second reason for putting `filesets-data' on this list.  If
you frequently add and remove buffers on the fly to :files filesets, you
don't need to save your customizations if `filesets-data' is being
mirrored in the cache file.  In this case the version in the cache file
is the current one, and the version in your startup file will be
silently updated later on.

If you want caching to work properly, at least `filesets-submenus',
`filesets-menu-cache', and `filesets-ingroup-cache' should be in this
list.

Don't forget to check out `filesets-menu-ensure-use-cached'."
  :set (function filesets-set-default)
  :type '(repeat
	  (choice :tag "Variable"
		  (const :tag "filesets-submenus"
			 :value filesets-submenus)
		  (const :tag "filesets-menu-cache"
			 :value filesets-menu-cache)
		  (const :tag "filesets-ingroup-cache"
			 :value filesets-ingroup-cache)
		  (const :tag "filesets-data"
			 :value filesets-data)
		  (const :tag "filesets-external-viewers"
			 :value filesets-external-viewers)
		  (const :tag "filesets-ingroup-patterns"
			 :value filesets-ingroup-patterns)
		  (const :tag "filesets-be-docile-flag"
			 :value filesets-be-docile-flag)
		  (sexp :tag "Other" :value nil)))
  :group 'filesets)

(defcustom filesets-cache-fill-content-hooks nil
  "Hooks to run when writing the contents of filesets' cache file.

The hook is called with the cache file as current buffer and the cursor
at the last position.  I.e. each hook has to make sure that the cursor is
at the last position.

Possible uses: If you don't want to save `filesets-data' in your normal
configuration file, you can add a something like this

	\(lambda ()
	      \(insert (format \"(setq-default filesets-data '%S)\"
			      filesets-data))
	      \(newline 2))

to this hook.

Don't forget to check out `filesets-menu-ensure-use-cached'."
  :set (function filesets-set-default)
  :type 'hook
  :group 'filesets)

(defcustom filesets-cache-hostname-flag nil
  "Non-nil means cache the hostname.
If the current name differs from the cached one,
rebuild the menu and create a new cache file."
  :set (function filesets-set-default)
  :type 'boolean
  :group 'filesets)

(defcustom filesets-cache-save-often-flag nil
  "Non-nil means save buffer on every change of the filesets menu.
If this variable is set to nil and if Emacs crashes, the cache and
filesets-data could get out of sync.  Set this to t if this happens from
time to time or if the fileset cache causes troubles."
  :set (function filesets-set-default)
  :type 'boolean
  :group 'filesets)

(defcustom filesets-max-submenu-length 25
  "Maximum length of submenus.
Set this value to 0 to turn menu splitting off.  BTW, parts of submenus
will not be rewrapped if their length exceeds this value."
  :set (function filesets-set-default)
  :type 'integer
  :group 'filesets)

(defcustom filesets-max-entry-length 50
  "Truncate names of split submenus to this length."
  :set (function filesets-set-default)
  :type 'integer
  :group 'filesets)

(defcustom filesets-browse-dir-function 'dired
  "A function or command used for browsing directories.
When using an external command, \"%s\" will be replaced with the
directory's name.

Note: You have to manually rebuild the menu if you change this value."
  :set (function filesets-set-default)
  :type '(choice :tag "Function:"
		 (const :tag "dired"
			:value dired)
		 (list :tag "Command"
		       :value ("" "%s")
		       (string :tag "Name")
		       (string :tag "Arguments"))
		 (function :tag "Function"
			   :value nil))
  :group 'filesets)

(defcustom filesets-open-file-function 'filesets-find-or-display-file
  "The function used for opening files.

`filesets-find-or-display-file' ... Filesets' default function for
visiting files.  This function checks if an external viewer is defined
for a specific file type.  Either this viewer, if defined, or
`find-file' will be used to visit a file.

`filesets-find-file' ... An alternative function that always uses
`find-file'.  If `filesets-be-docile-flag' is true, a file, which isn't
readable, will not be opened.

Caveat: Changes will take effect only after rebuilding the menu."
  :set (function filesets-set-default)
  :type '(choice :tag "Function:"
		 (const :tag "filesets-find-or-display-file"
			:value filesets-find-or-display-file)
		 (const :tag "filesets-find-file"
			:value filesets-find-file)
		 (function :tag "Function"
			   :value nil))
  :group 'filesets)

(defcustom filesets-save-buffer-function 'save-buffer
  "The function used to save a buffer.
Caveat: Changes will take effect after rebuilding the menu."
  :set (function filesets-set-default)
  :type '(choice :tag "Function:"
		 (const :tag "save-buffer"
			:value save-buffer)
		 (function :tag "Function"
			   :value nil))
  :group 'filesets)

(defcustom filesets-find-file-delay
  (if (and (featurep 'xemacs) gutter-buffers-tab-visible-p)
      0.5
    0)
  "Delay before calling `find-file'.
This is for calls via `filesets-find-or-display-file'
or `filesets-find-file'.

Set this to 0, if you don't use XEmacs's buffer tabs."
  :set (function filesets-set-default)
  :type 'number
  :group 'filesets)

(defcustom filesets-be-docile-flag nil
  "Non-nil means don't complain if a file or a directory doesn't exist.
This is useful if you want to use the same startup files in different
computer environments."
  :set (function filesets-set-default)
  :type 'boolean
  :group 'filesets)

(defcustom filesets-sort-menu-flag t
  "Non-nil means sort the filesets menu alphabetically."
  :set (function filesets-set-default)
  :type 'boolean
  :group 'filesets)

(defcustom filesets-sort-case-sensitive-flag t
  "Non-nil means sorting of the filesets menu is case sensitive."
  :set (function filesets-set-default)
  :type 'boolean
  :group 'filesets)

(defcustom filesets-tree-max-level 3
  "Maximum scan depth for directory trees.
A :tree fileset is defined by a base directory the contents of which
will be recursively added to the menu.  `filesets-tree-max-level' tells up
to which level the directory structure should be scanned/listed,
i.e. how deep the menu should be.  Try something like

	\(\"HOME -- only one level\"
	 \(:tree \"~\" \"^[^.].*[^~]$\")
	 \(:tree-max-level 1)
	 \(:filter-dirs-flag t))
	\(\"HOME -- up to 3 levels\"
	 \(:tree \"~\" \"^[^.].*[^~]$\")
	 \(:tree-max-level 3)
	 \(:filter-dirs-flag t))

and it should become clear what this option is about.  In any case,
including directory trees to the menu can take a lot of memory."
  :set (function filesets-set-default)
  :type 'integer
  :group 'filesets)

(defcustom filesets-commands
  `(("Isearch"
     multi-isearch-files
     (filesets-cmd-isearch-getargs))
    ("Isearch (regexp)"
     multi-isearch-files-regexp
     (filesets-cmd-isearch-getargs))
    ("Query Replace"
     perform-replace
     (filesets-cmd-query-replace-getargs))
    ("Query Replace (regexp)"
     perform-replace
     (filesets-cmd-query-replace-regexp-getargs))
    ("Grep <<selection>>"
     "grep"
     ("-n " filesets-get-quoted-selection " " "<<file-name>>"))
    ("Run Shell Command"
     filesets-cmd-shell-command
     (filesets-cmd-shell-command-getargs)))
  "Commands to run on filesets.
An association list of names, functions, and an argument list (or a
function that returns one) to be run on a filesets' files.

The argument <file-name> or <<file-name>> (quoted) will be replaced with
the filename."
  :set (function filesets-set-default+)
  :type '(repeat :tag "Commands"
		 (list :tag "Definition" :value ("")
		       (string "Name")
		       (choice :tag "Command"
			       (string :tag "String")
			       (function :tag "Function"))
		       (repeat :tag "Argument List"
			       (choice :tag "Arguments"
				       (sexp :tag "Sexp"
					     :value nil)
				       (string :tag "File Name"
					       :value "<file-name>")
				       (string :tag "Quoted File Name"
					       :value "<<file-name>>")
				       (function :tag "Function"
						 :value nil)))))
  :group 'filesets)
(put 'filesets-commands 'risky-local-variable t)

(defcustom filesets-external-viewers
  (let
      ;; ((ps-cmd  (or (and (boundp 'my-ps-viewer) my-ps-viewer)
      ;;    	    (filesets-select-command "ggv gv")))
      ;;  (pdf-cmd (or (and (boundp 'my-ps-viewer) my-pdf-viewer)
      ;;    	    (filesets-select-command "xpdf acroread")))
      ;;  (dvi-cmd (or (and (boundp 'my-ps-viewer) my-dvi-viewer)
      ;;    	    (filesets-select-command "xdvi tkdvi")))
      ;;  (doc-cmd (or (and (boundp 'my-ps-viewer) my-doc-viewer)
      ;;    	    (filesets-select-command "antiword")))
      ;;  (pic-cmd (or (and (boundp 'my-ps-viewer) my-pic-viewer)
      ;;    	    (filesets-select-command "gqview ee display"))))
      ((ps-cmd  "ggv")
       (pdf-cmd "xpdf")
       (dvi-cmd "xdvi")
       (doc-cmd "antiword")
       (pic-cmd "gqview"))
    `(("^.+\\..?html?$" browse-url
       ((:ignore-on-open-all t)))
      ("^.+\\.pdf$" ,pdf-cmd
       ((:ignore-on-open-all t)
	(:ignore-on-read-text t)
	(:constraint-flag ,pdf-cmd)))
      ("^.+\\.e?ps\\(.gz\\)?$" ,ps-cmd
       ((:ignore-on-open-all t)
	(:ignore-on-read-text t)
	(:constraint-flag ,ps-cmd)))
      ("^.+\\.dvi$" ,dvi-cmd
       ((:ignore-on-open-all t)
	(:ignore-on-read-text t)
	(:constraint-flag ,dvi-cmd)))
      ("^.+\\.doc$" ,doc-cmd
       ((:capture-output t)
	(:ignore-on-read-text t)
	(:constraint-flag ,doc-cmd)))
      ("^.+\\.\\(tiff\\|xpm\\|gif\\|pgn\\)$" ,pic-cmd
       ((:ignore-on-open-all t)
	(:ignore-on-read-text t)
	(:constraint-flag ,pic-cmd)))))
  "Association list of file patterns and external viewers for use with
`filesets-find-or-display-file'.

Has the form ((FILE-PATTERN VIEWER PROPERTIES) ...), VIEWER being either a
function or a command name as string.

Properties is an association list determining filesets' behavior in
several conditions.  Choose one from this list:

:ignore-on-open-all ... Don't open files of this type automatically --
i.e. on open-all-files-events or when running commands

:capture-output ... capture an external viewer output

:constraintp FUNCTION ... use this viewer only if FUNCTION returns non-nil

:constraint-flag SEXP ... use this viewer only if SEXP evaluates to non-nil

:open-hook HOOK ... run hooks after spawning the viewer -- mainly useful
in conjunction with :capture-output

:args (FORMAT-STRING or SYMBOL or FUNCTION) ... a list of arguments
\(defaults to (list \"%S\")) when using shell commands

Avoid modifying this variable and achieve minor speed-ups by setting the
variables my-ps-viewer, my-pdf-viewer, my-dvi-viewer, my-pic-viewer.

In order to view pdf or rtf files in an Emacs buffer, you could use these:


      \(\"^.+\\\\.pdf\\\\'\" \"pdftotext\"
       \((:capture-output t)
	\(:args (\"%S - | fmt -w \" window-width))
	\(:ignore-on-read-text t)
	\(:constraintp (lambda ()
			\(and \(filesets-which-command-p \"pdftotext\")
			     \(filesets-which-command-p \"fmt\"))))))
      \(\"^.+\\\\.rtf\\\\'\" \"rtf2htm\"
       \((:capture-output t)
	\(:args (\"%S 2> /dev/null | w3m -dump -T text/html\"))
	\(:ignore-on-read-text t)
	\(:constraintp (lambda ()
			\(and (filesets-which-command-p \"rtf2htm\")
			     \(filesets-which-command-p \"w3m\"))))))"
  :set (function filesets-set-default)
  :type '(repeat :tag "Viewer"
		 (list :tag "Definition"
		       :value ("^.+\\.suffix$" "")
		       (regexp :tag "Pattern")
		       (choice :tag "Viewer"
			       (symbol :tag "Function" :value nil)
			       (string :tag "Program" :value ""))
		       (repeat :tag "Properties"
			       (choice
				(list :tag ":constraintp"
				      :value (:constraintp)
				      (const :format ""
					     :value :constraintp)
				      (function :tag "Function"))
				(list :tag ":constraint-flag"
				      :value (:constraint-flag)
				      (const :format ""
					     :value :constraint-flag)
				      (sexp :tag "Symbol"))
				(list :tag ":ignore-on-open-all"
				      :value (:ignore-on-open-all t)
				      (const  :format ""
					      :value :ignore-on-open-all)
				      (boolean :tag "Boolean"))
				(list :tag ":ignore-on-read-text"
				      :value (:ignore-on-read-text t)
				      (const  :format ""
					      :value :ignore-on-read-text)
				      (boolean :tag "Boolean"))
				(list :tag ":args"
				      :value (:args)
				      (const :format ""
					     :value :args)
				      (repeat :tag "List"
					      (choice :tag "Arguments"
						      (string :tag "String"
							      :value "")
						      (symbol :tag "Symbol"
							      :value nil)
						      (function :tag "Function"
								:value nil))))
				(list :tag ":open-hook"
				      :value (:open-hook)
				      (const :format ""
					     :value :open-hook)
				      (hook :tag "Hook"))
;				(list :tag ":close-hook"
;				      :value (:close-hook)
;				      (const :format ""
;					     :value :close-hook)
;				      (hook :tag "Hook"))
				(list :tag ":capture-output"
				      :value (:capture-output t)
				      (const  :format ""
					      :value :capture-output)
				      (boolean :tag "Boolean"))))))
  :group 'filesets)
(put 'filesets-external-viewers 'risky-local-variable t)

(defcustom filesets-ingroup-patterns
  '(("^.+\\.tex$" t
     (((:name "Package")
       (:pattern "\\\\usepackage\\W*\\(\\[[^\]]*\\]\\W*\\)?{\\W*\\(.+\\)\\W*}")
       (:match-number 2)
       (:stub-flag t)
       (:get-file-name (lambda (master file)
			 (filesets-which-file master
					      (concat file ".sty")
					      (filesets-convert-path-list
					       (or (getenv "MY_TEXINPUTS")
						   (getenv "TEXINPUTS")))))))
      ((:name "Include")
       (:pattern "\\\\include\\W*{\\W*\\(.+\\)\\W*}")
       (:get-file-name (lambda (master file)
			 (filesets-which-file master
					      (concat file ".tex")
					      (filesets-convert-path-list
					       (or (getenv "MY_TEXINPUTS")
						   (getenv "TEXINPUTS"))))))
       (:scan-depth 5))
      ((:name "Input")
       (:pattern "\\\\input\\W*{\\W*\\(.+\\)\\W*}")
       (:stubp (lambda (a b) (not (filesets-files-in-same-directory-p a b))))
       (:get-file-name (lambda (master file)
			 (filesets-which-file master
					      (concat file ".tex")
					      (filesets-convert-path-list
					       (or (getenv "MY_TEXINPUTS")
						   (getenv "TEXINPUTS"))))))
       (:scan-depth 5))
      ((:name "Bibliography")
       (:pattern "\\\\bibliography\\W*{\\W*\\(.+\\)\\W*}")
       (:get-file-name (lambda (master file)
			 (filesets-which-file master
					      (concat file ".bib")
					      (filesets-convert-path-list
					       (or (getenv "MY_BIBINPUTS")
						   (getenv "BIBINPUTS")))))))))
    ("^.+\\.el$" t
     (((:name "Require")
       (:pattern "(require\\W+'\\(.+\\))")
       (:stubp (lambda (a b) (not (filesets-files-in-same-directory-p a b))))
       (:get-file-name (lambda (master file)
			 (filesets-which-file master
					      (concat file ".el")
					      load-path))))
      ((:name "Load")
       (:pattern "(load\\(-library\\)?\\W+\"\\(.+\\)\")")
       (:match-number 2)
       (:get-file-name (lambda (master file)
			 (filesets-which-file master file load-path))))))
    ("^\\([A-ZÄÖÜ][a-zäöüß]+\\([A-ZÄÖÜ][a-zäöüß]+\\)+\\)$" t
     (((:pattern "\\<\\([A-ZÄÖÜ][a-zäöüß]+\\([A-ZÄÖÜ][a-zäöüß]+\\)+\\)\\>")
       (:scan-depth 5)
       (:stubp (lambda (a b) (not (filesets-files-in-same-directory-p a b))))
       (:case-sensitive t)
       (:get-file-name (lambda (master file)
			 (filesets-which-file
			  master
			  file
			  (if (boundp 'emacs-wiki-directories)
			      emacs-wiki-directories
			    nil))))))))

  "Inclusion group definitions.

Define how to find included file according to a file's mode (being
defined by a file pattern).

A valid entry has the form (FILE-PATTERN REMOVE-DUPLICATES-FLAG
CMD-DEF1 ...), CMD-DEF1 being a plist containing the fields :pattern
\(mandatory), :name, :get-file-name, :match-number, :scan-depth,
:preprocess, :case-sensitive.

File Pattern ... A regexp matching the file's name for which the
following rules should be applied.

Remove Duplicates ... If t, only the first occurrence of an included
file is retained.  (See below for a full explanation.)

:name STRING ... This pattern's name.

:pattern REGEXP ... A regexp matching the command.  This regexp has to
include a group that holds the name of the included file.

:get-file-name FUNCTION (default: `filesets-which-file') ... A function
that takes two arguments (the path of the master file and the name
of the included file) and returns a valid path or nil -- if the
subfile can't be found.

:match-number INTEGER (default: 1) ... The number of the match/group
in the pattern holding the subfile's name.  0 refers the whole
match, 1 to the first group.

:stubp FUNCTION ... If (FUNCTION MASTER INCLUDED-FILE) returns non-nil,
INCLUDED-FILE is a stub -- see below.

:stub-flag ... Files of this type are stubs -- see below.

:scan-depth INTEGER (default: 0) ... Whether included files should be
rescanned.  Set this to 0 to disable re-scanning of included file.

:preprocess FUNCTION ... A function modifying a buffer holding the
master file so that pattern matching becomes easier.  This is usually
used to narrow a buffer to the relevant region.  This function could also
be destructive and simply delete non-relevant text.

:case-sensitive BOOLEAN (default: nil) ... Whether a pattern is
case-sensitive or not.


Stubs:

First, a stub is a file that shows up in the menu but will not be
included in an ingroup's file listing -- i.e. filesets will never
operate on this file automatically.  Secondly, in opposition to normal
files stubs are not scanned for new inclusion groups.  This is useful if
you want to have quick access to library headers.

In the menu, an asterisk is appended to the stub's name.


Remove Duplicates:

E.g. File A and file B refer to file X; X refers to A.  If
you choose not to remove duplicates the tree would look like:

    M + A - X - A ...
        B - X - A ...

As you can see, there is some chance that you run in circles.
Nevertheless, up to some degree this could still be what you want.

With duplicates removed, it would be:

    M + A - X
        B"
  :set (function filesets-set-default)
  :type '(repeat
	  :tag "Include"
	  (list
	   :tag "Definition" :value ("^.+\\.suffix$" t)
	   (regexp :tag "File Pattern" :value "^.+\\.suffix$")
	   (boolean :tag "Remove Duplicates" :value t)
	   (repeat :tag "Commands"
		   (repeat :tag "Command"
			   (choice
			    :tag "Definition"
			    (list :tag ":name"
				  :value (:name "")
				  (const :format "" :value :name)
				  (string :tag "String"))
			    (list :tag ":pattern"
				  :value (:pattern "\\<CMD\\W*\\(.+\\)\\>")
				  (const :format "" :value :pattern)
				  (regexp :tag "RegExp"))
			    (list :tag ":get-file-name"
				  :value (:get-file-name)
				  (const :format "" :value :get-file-name)
				  (function :tag "Function"))
			    (list :tag ":match-number"
				  :value (:match-number 1)
				  (const :format "" :value :match-number)
				  (integer :tag "Integer"))
			    (list :tag ":stub-flag"
				  :value (:stub-flag t)
				  (const :format "" :value :stub-flag)
				  (boolean :tag "Boolean"))
			    (list :tag ":stubp"
				  :value (:stubp)
				  (const :format "" :value :stubp)
				  (function :tag "Function"))
			    (list :tag ":scan-depth"
				  :value (:scan-depth 0)
				  (const :format "" :value :scan-depth)
				  (integer :tag "Integer"))
			    (list :tag ":case-sensitive"
				  :value (:case-sensitive)
				  (const :format "" :value :case-sensitive)
				  (boolean :tag "Boolean"))
			    (list :tag ":preprocess"
				  :value (:preprocess)
				  (const :format "" :value :preprocess)
				  (function :tag "Function")))))))
  :group 'filesets)
(put 'filesets-ingroup-patterns 'risky-local-variable t)

(defcustom filesets-data nil
  "Fileset definitions.

A fileset is either a list of files, a file pattern, a base directory
and a search pattern (for files), or a base file.  Changes to this
variable will take effect after rebuilding the menu.

Caveat: Fileset names have to be unique.

Example definition:
      '\(\(\"My Wiki\"
	 \(:ingroup \"~/Etc/My-Wiki/WikiContents\"))
	\(\"My Homepage\"
	 \(:pattern \"~/public_html/\" \"^.+\\\\.html$\")
	 \(:open filesets-find-file))
	\(\"User Configuration\"
	 \(:files \"~/.xinitrc\"
		 \"~/.bashrc\"
		 \"~/.bash_profile\"))
	\(\"HOME\"
	 \(:tree \"~\" \"^[^.].*[^~]$\")
	 \(:filter-dirs-flag t)))

`filesets-data' is a list of (NAME-AS-STRING . DEFINITION), DEFINITION
being an association list with the fields:

:files FILE-1 .. FILE-N ... a list of files belonging to a fileset

:ingroup FILE-NAME ... an inclusion group's base file.

:tree ROOT-DIR PATTERN ... a base directory and a file pattern

:pattern DIR PATTERN ... a base directory and a regexp matching
                         files in that directory.  Usually,
                         PATTERN has the form '^REGEXP$'.  Unlike
                         :tree, this form does not descend
                         recursively into subdirectories.

:filter-dirs-flag BOOLEAN ... is only used in conjunction with :tree.

:tree-max-level INTEGER ... recurse into directories this many levels
\(see `filesets-tree-max-level' for a full explanation)

:dormant-flag BOOLEAN ... non-nil means don't show this item in the
menu; dormant filesets can still be manipulated via commands available
from the minibuffer -- e.g. `filesets-open', `filesets-close', or
`filesets-run-cmd'

:dormant-p FUNCTION ... a function returning :dormant-flag

:open FUNCTION ... the function used to open file belonging to this
fileset.  The function takes a file name as argument

:save FUNCTION ... the function used to save file belonging to this
fileset; it takes no arguments, but works on the current buffer.

Either :files, :pattern, :tree, or :ingroup must be supplied.  :files
overrules :tree, :tree overrules :pattern, :pattern overrules :ingroup,
i.e. these tags are mutually exclusive.  The fields :open and :save are
optional.

In conjunction with the :tree tag, :save is void.  :open refers to the
function used for opening files in a directory, not for opening the
directory.  For browsing directories, `filesets-browse-dir-function' is used.

Before using :ingroup, make sure that the file type is already
defined in `filesets-ingroup-patterns'."
  :group 'filesets
  :set (function filesets-data-set-default)
  :type '(repeat
	  (cons :tag "Fileset"
		(string :tag "Name" :value "")
		(repeat :tag "Data"
			(choice
			 :tag "Type" :value nil
			 (list :tag "Pattern"
			       :value (:pattern "~/"  "^.+\\.suffix$")
			       (const :format "" :value :pattern)
			       (directory :tag "Dir")
			       (regexp :tag "Pattern"))
			 (cons :tag "Files"
			       :value (:files)
			       (const :format "" :value :files)
			       (repeat :tag "Files" file))
			 (list :tag "Single File"
			       :value (:file "~/")
			       (const :format "" :value :file)
			       (file :tag "File"))
			 (list :tag "Inclusion group"
			       :value (:ingroup "~/")
			       (const :format "" :value :ingroup)
			       (file :tag "File" :value "~/"))
			 (list :tag "Directory Tree"
			       :value (:tree "~/"  "^.+\\.suffix$")
			       (const :format "" :value :tree)
			       (directory :tag "Dir")
			       (regexp :tag "Pattern"))
			 (list :tag "Filter directories"
			       :value (:filter-dirs-flag)
			       (const :format "" :value :filter-dirs-flag)
			       (boolean :tag "Boolean" :value nil))
			 (list :tag "Scanning depth"
			       :value (:tree-max-level 3)
			       (const :format "" :value :tree-max-level)
			       (integer :tag "Integer"))
			 (list :tag "Verbosity"
			       :value (:verbosity 1)
			       (const :format "" :value :verbosity)
			       (integer :tag "Integer"))
			 (list :tag "Conceal fileset (Flag)"
			       :value (:dormant-flag)
			       (const :format "" :value :dormant-flag)
			       (boolean :tag "Boolean"))
			 (list :tag "Conceal fileset (Function)"
			       :value (:dormant-p)
			       (const :format "" :value :dormant-p)
			       (function :tag "Function"))
			 (list :tag "Save function"
			       :value (:save)
			       (const :format "" :value :save)
			       (function :tag "Function"))
			 (list :tag "Open function"
			       :value (:open)
			       (const :format "" :value :open)
			       (function :tag "Function")))))))
(put 'filesets-data 'risky-local-variable t)


(defcustom filesets-query-user-limit 15
  "Query the user before opening a fileset with that many files."
  :set (function filesets-set-default)
  :type 'integer
  :group 'filesets)

;;; Emacs compatibility
(eval-and-compile
  (if (featurep 'xemacs)
      (fset 'filesets-error 'error)

    (require 'easymenu)

    (defun filesets-error (class &rest args)
      "`error' wrapper."
      (error "%s" (mapconcat 'identity args " ")))

    ))

(defun filesets-filter-dir-names (lst &optional negative)
  "Remove non-directory names from a list of strings.
If NEGATIVE is non-nil, remove all directory names."
  (filesets-filter-list lst
			(lambda (x)
			  (and (not (string-match "^\\.+/$" x))
			       (if negative
				   (not (string-match "[:/\\]$" x))
				 (string-match "[:/\\]$" x))))))

(defun filesets-conditional-sort (lst &optional access-fn)
  "Return a sorted copy of LST, LST being a list of strings.
If `filesets-sort-menu-flag' is nil, return LST itself.

ACCESS-FN ... function to get the string value of LST's elements."
  (if filesets-sort-menu-flag
      (let* ((fni (or access-fn
		      (function identity)))
	     (fn (if filesets-sort-case-sensitive-flag
		     (lambda (a b)
		       (string< (funcall fni a)
				(funcall fni b)))
		   (lambda (a b)
		     (string< (upcase (funcall fni a))
			      (upcase (funcall fni b)))))))
	(sort (copy-sequence lst) fn))
    lst))

(defun filesets-directory-files (dir &optional
				     pattern what full-flag match-dirs-flag)
  "Get WHAT (:files or :dirs) in DIR.
If PATTERN is provided return only those entries matching this
regular expression.
If MATCH-DIRS-FLAG is non-nil, also match directory entries.
Return full path if FULL-FLAG is non-nil."
  (filesets-message 2 "Filesets: scanning %S" dir)
  (cond
   ((file-exists-p dir)
    (let ((files nil)
	  (dirs  nil))
      (dolist (this (file-name-all-completions "" dir))
	(cond
	 ((string-match "^\\.+/$" this)
	  nil)
	 ((string-match "[:/\\]$" this)
	  (when (or (not match-dirs-flag)
		    (not pattern)
		    (string-match pattern this))
	    (filesets-message 5 "Filesets: matched dir %S with pattern %S"
			      this pattern)
	    (setq dirs (cons this dirs))))
	 (t
	  (when (or (not pattern)
		    (string-match pattern this))
	    (filesets-message 5 "Filesets: matched file %S with pattern %S"
			      this pattern)
	    (setq files (cons (if full-flag
				  (concat (file-name-as-directory dir) this)
				this)
			      files))))))
      (cond
       ((equal what ':dirs)
	(filesets-conditional-sort dirs))
       ((equal what ':files)
	(filesets-conditional-sort files))
       (t
	(append (filesets-conditional-sort files)
		(filesets-conditional-sort dirs))))))
   (filesets-be-docile-flag
    (filesets-message 1 "Filesets: %S doesn't exist" dir)
    nil)
   (t
    (filesets-error 'error "Filesets: " dir " does not exist"))))

(defun filesets-quote (txt)
  "Return TXT in quotes."
  (concat "\"" txt "\""))

(defun filesets-get-selection ()
  "Get the text between mark and point -- i.e. the selection or region."
  (let ((m (mark))
	(p (point)))
    (if m
	(buffer-substring (min m p) (max m p))
      (filesets-error 'error "No selection."))))

(defun filesets-get-quoted-selection ()
  "Return the currently selected text in quotes."
  (filesets-quote (filesets-get-selection)))

(defun filesets-get-shortcut (n)
  "Create menu shortcuts based on number N."
  (let ((n (mod (- n 1) 51)))
    (cond
     ((not filesets-menu-shortcuts-flag)
      "")
     ((<= n 9)
      (concat (number-to-string n) " "))
     ((<= n 35)
      (format "%c " (+ 87 n)))
     ((<= n 51)
      (format "%c " (+ -3 n))))))

(defun filesets-files-equalp (a b)
  "Compare two filenames A and B after expansion."
  (equal (expand-file-name a) (expand-file-name b)))

(defun filesets-files-in-same-directory-p (a b)
  "Compare two filenames A and B after expansion."
  (let ((ad (file-name-directory (expand-file-name a)))
	(bd (file-name-directory (expand-file-name b))))
    (equal ad bd)))

(defun filesets-convert-path-list (string)
  "Return a path-list given as STRING as list."
  (if string
      (mapcar (lambda (x) (file-name-as-directory x))
	      (split-string string path-separator))
    nil))

(defun filesets-which-file (master filename &optional path-list)
  "Search for a FILENAME relative to a MASTER file in PATH-LIST."
  (let ((f (concat (file-name-directory master)
		   filename)))
    (if (file-exists-p f)
	f
      (filesets-some
       (lambda (dir)
	 (let ((dir (file-name-as-directory dir))
	       (files (if (file-exists-p dir)
			  (filesets-directory-files dir nil ':files)
			nil)))
	   (filesets-some (lambda (file)
			    (if (equal filename (file-name-nondirectory file))
				(concat dir file)
			      nil))
			  files)))
       path-list))))


(defun filesets-eviewer-get-props (entry)
  "Get ENTRY's (representing an external viewer) properties."
  (nth 2 entry))

(defun filesets-eviewer-constraint-p (entry)
  (let* ((props           (filesets-eviewer-get-props entry))
	 (constraint      (assoc ':constraintp props))
	 (constraint-flag (assoc ':constraint-flag props)))
    (cond
     (constraint
      (funcall (cadr constraint)))
     (constraint-flag
      (eval (cadr constraint-flag)))
     (t
      t))))

(defun filesets-get-external-viewer (file)
  "Find an external viewer for FILE."
  (let ((filename (file-name-nondirectory file)))
    (filesets-some
     (lambda (entry)
       (when (and (string-match (nth 0 entry) filename)
		  (filesets-eviewer-constraint-p entry))
	 entry))
     filesets-external-viewers)))

(defun filesets-get-external-viewer-by-name (name)
  "Get the external viewer definition called NAME."
  (when name
    (filesets-some
     (lambda (entry)
       (when (and (string-equal (nth 1 entry) name)
		  (filesets-eviewer-constraint-p entry))
	 entry))
     filesets-external-viewers)))

(defun filesets-filetype-property (filename event &optional entry)
  "Return non-nil if a file of a specific type has special flags/tags.

Events (corresponding tag):

on-open-all (:ignore-on-open-all) ... Exclude files of this when opening
a fileset

on-grep (:ignore-on-read-text) ... Exclude files of this when running
the \"Grep <<selection>>\" command

on-capture-output (:capture-output) ... Capture output of an external viewer

on-ls ... Not used

on-cmd ... Not used

on-close-all ... Not used"
  (let ((def (filesets-eviewer-get-props
	      (or entry
		  (filesets-get-external-viewer filename)))))
    (filesets-alist-get def
			(case event
			  ((on-open-all)       ':ignore-on-open-all)
			  ((on-grep)           ':ignore-on-read-text)
			  ((on-cmd) nil)
			  ((on-close-all) nil))
			nil t)))

(defun filesets-filetype-get-prop (property filename &optional entry)
  "Return PROPERTY for filename -- use ENTRY if provided."
  (let ((def (filesets-eviewer-get-props
	      (or entry
		  (filesets-get-external-viewer filename)))))
    (when def
      (filesets-alist-get def property nil t))))

(defun filesets-reset-filename-on-change ()
  "Reset a buffer's filename if the buffer is being modified."
  (when filesets-output-buffer-flag
    (set-visited-file-name nil t)))

(defun filesets-spawn-external-viewer (file &optional ev-entry)
  "Start an external viewer for FILE.
Use the viewer defined in EV-ENTRY (a valid element of
`filesets-external-viewers') if provided."
  (let* ((file     (expand-file-name file))
	 (entry    (or ev-entry
		       (filesets-get-external-viewer file))))
    (if entry
	(let* ((vwr  (cadr entry))
	       (co-flag (filesets-filetype-get-prop ':capture-output file entry))
	       (oh   (filesets-filetype-get-prop ':open-hook file entry))
	       (args (let ((fmt (filesets-filetype-get-prop ':args file entry)))
		       (if fmt
			   (let ((rv ""))
			     (dolist (this fmt rv)
			       (setq rv (concat rv
						(cond
						 ((stringp this)
						  (format this file))
						 ((and (symbolp this)
						       (fboundp this))
						  (format "%S" (funcall this)))
						 (t
						  (format "%S" this)))))))
			 (format "%S" file))))
	       (output
		(cond
		 ((and (functionp vwr) co-flag)
		  (funcall vwr file))
		 ((functionp vwr)
		  (funcall vwr file)
		  nil)
		 (co-flag
		  (shell-command-to-string (format "%s %s" vwr args)))
		 (t
		  (shell-command (format "%s %s&" vwr args))
		  nil))))
	  (if co-flag
	      (progn
		(switch-to-buffer (format "Filesets: %s %s" vwr file))
		(insert output)
		(make-local-variable 'filesets-output-buffer-flag)
		(setq filesets-output-buffer-flag t)
		(set-visited-file-name file t)
		(when oh
		  (run-hooks 'oh))
		(set-buffer-modified-p nil)
		(setq buffer-read-only t)
		(goto-char (point-min)))
	    (when oh
	      (run-hooks 'oh))))
      (filesets-error 'error
		      "Filesets: general error when spawning external viewer"))))

(defun filesets-find-file (file)
  "Call `find-file' after a possible delay (see `filesets-find-file-delay').
If `filesets-be-docile-flag' is true, a file, which isn't readable, will
not be opened."
;  (sleep-for filesets-find-file-delay)
  (when (or (file-readable-p file)
	    (not filesets-be-docile-flag))
    (sit-for filesets-find-file-delay)
    (find-file file)))

(defun filesets-find-or-display-file (&optional file viewer)
  "Visit FILE using an external VIEWER or open it in an Emacs buffer."
  (interactive)
  (let* ((file (or file
		   (read-file-name "Find file: " nil nil viewer)))
	 (external-viewer-def (or
			       (filesets-get-external-viewer-by-name viewer)
			       (filesets-get-external-viewer file))))
    (filesets-message 3 "Filesets: view %S using %s" file external-viewer-def)
    (if external-viewer-def
	(filesets-spawn-external-viewer file external-viewer-def)
      (filesets-find-file file))))

(defun filesets-find-file-using ()
  "Select a viewer and call `filesets-find-or-display-file'."
  (interactive)
  (let* ((lst (mapcar (lambda (this)
			(let ((a (cadr this)))
			  (list (format "%s" a) a)))
		      filesets-external-viewers))
	 (viewer (completing-read "Using viewer: " lst nil t)))
    (when viewer
      (filesets-find-or-display-file nil (cadr (assoc viewer lst))))))

(defun filesets-browser-name ()
  "Get the directory browser's name as defined in `filesets-browse-dir-function'."
  (cond
   ((listp filesets-browse-dir-function)
    (car filesets-browse-dir-function))
   (t
    filesets-browse-dir-function)))

(defun filesets-browse-dir (dir)
  "Browse DIR using `filesets-browse-dir-function'."
  (if (functionp filesets-browse-dir-function)
      (funcall filesets-browse-dir-function dir)
    (let ((name (car filesets-browse-dir-function))
	  (args (format (cadr filesets-browse-dir-function) (expand-file-name dir))))
      (with-temp-buffer
	(start-process (concat "Filesets:" name)
		       "*Filesets external directory browser*"
		       name args)))))

(defun filesets-get-fileset-name (something)
  "Get SOMETHING's name (Don't ask)."
  (cond
   ((listp something)
    (car something))
   (t
    something)))

(defun filesets-data-get-name (entry)
  "Access to `filesets-data'.  Get the ENTRY's name."
  (car entry))

(defun filesets-data-get-data (entry)
  "Access to `filesets-data'.  Get the ENTRY's data section."
  (cdr entry))

(defun filesets-alist-get (alist key &optional default carp)
  "Get KEY's value in the association list ALIST.
Return DEFAULT if not found.  Return (car VALUE) if CARP is non-nil."
  (let ((elt (assoc key alist)))
    (cond
      (elt
       (if carp
	   (cadr elt)
	 (cdr elt)))
      (default default)
      (t nil))))

(defun filesets-data-get (entry key &optional default carp)
  "Extract the value for KEY in the data part of fileset ENTRY.
Return DEFAULT if not found.  Return (car VALUE) if CARP is non-nil."
  (filesets-alist-get (filesets-data-get-data entry) key default carp))

(defun filesets-data-set (entry key value)
  "Set the VALUE for KEY in the data part of fileset ENTRY."
  (let* ((alist (filesets-data-get-data entry))
	 (elt (assoc key alist)))
    (if elt
	(setcdr elt value)
      (setcdr entry (cons (cons key value) alist)))))

(defun filesets-entry-mode (entry)
  "Return fileset ENTRY's mode: :files, :file, :tree, :pattern, or :ingroup.
See `filesets-data'."
  (let ((data (filesets-data-get-data entry)))
    (filesets-some
     (lambda (x)
       (if (assoc x data)
	   x))
     '(:files :tree :pattern :ingroup :file))))

(defun filesets-entry-get-open-fn (fileset-name &optional fileset-entry)
  "Get the open-function for FILESET-NAME.
Use FILESET-ENTRY for finding the open function, if provided."
  (filesets-data-get (or fileset-entry
			 (filesets-get-fileset-from-name fileset-name))
		     ':open filesets-open-file-function t))

(defun filesets-entry-get-save-fn (fileset-name &optional fileset-entry)
  "Get the save-function for FILESET-NAME.
Use FILESET-ENTRY for finding the save function, if provided."
  (filesets-data-get (or fileset-entry
			 (filesets-get-fileset-from-name fileset-name))
		     ':save filesets-save-buffer-function t))

(defun filesets-entry-get-files (entry)
  "Get the file list for fileset ENTRY."
  (filesets-data-get entry ':files))

(defun filesets-entry-set-files (entry data &optional anyways)
  "Set the file list for fileset ENTRY."
  (let ((files (filesets-entry-get-files entry)))
    (if (or anyways files)
	(filesets-data-set entry ':files data))))

(defun filesets-entry-get-verbosity (entry)
  "Get verbosity level for fileset ENTRY."
  (filesets-data-get entry ':verbosity 1 t))

(defun filesets-entry-get-file (entry)
  "Get the single file for fileset ENTRY."
  (filesets-data-get entry ':file nil t))

(defun filesets-entry-get-pattern (entry)
  "Get the base directory + file pattern for fileset ENTRY."
;  (filesets-data-get entry ':pattern nil t))
  (filesets-data-get entry ':pattern))

(defun filesets-entry-get-pattern--pattern (list)
  "Get the file pattern for LIST."
  (if (= (length list) 1) ;; for compatibility with filesets < v1.5.5
      (file-name-nondirectory (car list))
    (cadr list)))

(defun filesets-entry-get-pattern--dir (list)
  "Get a file pattern's base directory for LIST."
  (if (= (length list) 1) ;; for compatibility with filesets < v1.5.5
      (file-name-directory (car list))
    (car list)))

(defun filesets-entry-get-tree (entry)
  "Get the tree pattern for fileset ENTRY."
  (filesets-data-get entry ':tree))

(defun filesets-entry-get-dormant-flag (entry)
  "Get dormant flag for fileset ENTRY."
  (let ((fn (filesets-data-get entry ':dormant-p nil t)))
    (if fn
	(funcall fn)
      (filesets-data-get entry ':dormant-flag nil t))))

(defun filesets-entry-get-filter-dirs-flag (entry)
  "Get filter-dirs-flag for fileset ENTRY."
  (filesets-data-get entry ':filter-dirs-flag nil t))

(defun filesets-entry-get-tree-max-level (entry)
  "Get maximal tree scanning depth for fileset ENTRY."
  (filesets-data-get entry ':tree-max-level nil t))

(defun filesets-entry-get-master (entry)
  "Get the base file for fileset ENTRY."
  (filesets-data-get entry ':ingroup nil t))

(defun filesets-file-open (open-function file-name &optional fileset-name)
  "Open FILE-NAME using OPEN-FUNCTION.
If OPEN-FUNCTION is nil, its value will be deduced from FILESET-NAME."
  (let ((open-function (or open-function
			   (filesets-entry-get-open-fn fileset-name))))
    (if (file-readable-p file-name)
	(funcall open-function file-name)
      (message "Filesets: Couldn't open `%s'" file-name))))

(defun filesets-file-close (save-function buffer)
  "Close BUFFER.
First, save the buffer's contents using SAVE-FUNCTION.  Then, kill buffer
if `buffer-modified-p' returns nil.

SAVE-FUNCTION takes no argument, but works on the current buffer."
  (with-current-buffer buffer
    (if (buffer-modified-p)
	(funcall save-function))
    (if (not (buffer-modified-p))
	(kill-buffer buffer))))

(defun filesets-get-fileset-from-name (name &optional mode)
  "Get fileset definition for NAME."
  (case mode
    ((:ingroup :tree)
     name)
    (t
     (assoc name filesets-data))))


;;; commands
(defun filesets-cmd-get-def (cmd-name)
  "Get `filesets-commands' entry for CMD-NAME."
  (assoc cmd-name filesets-commands))

(defun filesets-cmd-get-args (cmd-name)
  (let ((args (let ((def (filesets-cmd-get-def cmd-name)))
		(nth 2 def)))
	(rv nil))
    (dolist (this args rv)
      (cond
       ((and (symbolp this) (fboundp this))
	(let ((x (funcall this)))
	  (setq rv (append rv (if (listp x) x (list x))))))
       (t
	(setq rv (append rv (list this))))))))

(defun filesets-cmd-get-fn (cmd-name)
  (let ((def (filesets-cmd-get-def cmd-name)))
    (nth 1 def)))

(defun filesets-cmd-show-result (cmd output)
  "Show OUTPUT of CMD (a shell command)."
  (pop-to-buffer "*Filesets: Shell Command Output*")
  (with-no-warnings
   (end-of-buffer))
  (insert "*** ")
  (insert cmd)
  (newline)
  (insert output)
  (newline))

(defun filesets-run-cmd--repl-fn (arg &optional format-fn)
  "Helper function for `filesets-run-cmd'.  Apply FORMAT-FN to arg.
Replace <file-name> or <<file-name>> with filename."
  (funcall format-fn (cond
		      ((equal arg "<file-name>")
		       (buffer-file-name))
		      ((equal arg "<<file-name>>")
		       (shell-quote-argument (buffer-file-name)))
		      (t
		       arg))))

(defun filesets-run-cmd (&optional cmd-name fileset mode)
  "Run CMD-NAME (see `filesets-commands') on FILESET."
  (interactive)
  (let* ((cmd-name (or cmd-name
		       (completing-read "Select command: " filesets-commands
					nil t)))
	 (name  (or fileset
		    (completing-read "Select fileset: " filesets-data nil t))))
    (when (and cmd-name name)
      (let* ((event (if (equal cmd-name "Grep <<selection>>")
		       'on-grep
		      'on-cmd))
	     (files (if (and fileset
			     (or (equal mode ':ingroup)
				 (equal mode ':tree)))
			(filesets-get-filelist fileset mode event)
		     (filesets-get-filelist
		      (filesets-get-fileset-from-name name)
		      mode event))))
	(when files
	  (let ((fn   (filesets-cmd-get-fn cmd-name))
		(args (filesets-cmd-get-args cmd-name)))
	    (if (memq fn '(multi-isearch-files multi-isearch-files-regexp))
		(apply fn args)
	      (dolist (this files nil)
		(save-excursion
		  (save-restriction
		    (let ((buffer (filesets-find-file this)))
		      (when buffer
			(goto-char (point-min))
			(progn
			  (cond
			   ((stringp fn)
			    (let* ((args
				    (let ((txt ""))
				      (dolist (this args txt)
					(setq txt
					      (concat txt
						      (filesets-run-cmd--repl-fn
						       this
						       (lambda (this)
							 (if (equal txt "") "" " ")
							 (format "%s" this))))))))
				   (cmd (concat fn " " args)))
			      (filesets-cmd-show-result
			       cmd (shell-command-to-string cmd))))
			   ((symbolp fn)
			    (let ((args
				   (let ((argl nil))
				     (dolist (this args argl)
				       (setq argl
					     (append argl
						     (filesets-run-cmd--repl-fn
						      this
						      'list)))))))
			      (apply fn args)))))))))))))))))

(defun filesets-get-cmd-menu ()
  "Create filesets command menu."
  `("+ Commands"
    . ,(mapcar (lambda (this)
		 (let ((name (car this)))
		   `[,name (filesets-run-cmd ,name)]))
	       filesets-commands)))


;;; sample commands
(defun filesets-cmd-query-replace-getargs ()
  "Get arguments for `query-replace' and `query-replace-regexp'."
  (let ((common (query-replace-read-args "Filesets query replace" nil t)))
    (list (nth 0 common) (nth 1 common) t nil (nth 2 common) nil
	  multi-query-replace-map)))

(defun filesets-cmd-query-replace-regexp-getargs ()
  "Get arguments for `query-replace' and `query-replace-regexp'."
  (let ((common (query-replace-read-args "Filesets query replace" t t)))
    (list (nth 0 common) (nth 1 common) t t (nth 2 common) nil
	  multi-query-replace-map)))

(defun filesets-cmd-isearch-getargs ()
  "Get arguments for `multi-isearch-files' and `multi-isearch-files-regexp'."
  (and (boundp 'files) (list files)))

(defun filesets-cmd-shell-command-getargs ()
  "Get arguments for `filesets-cmd-shell-command'."
  (let* ((arg (read-string "Shell command (%s = file): "
				   "%s"
				   'shell-command-history)))
    arg))

(defun filesets-cmd-shell-command (txt)
  "Wrapper function for `shell-command'."
  (let ((ok (if (buffer-modified-p)
		(let ((ok (y-or-n-p "Save buffer? ")))
		  (when ok
		    (save-buffer))
		  ok)
	      t)))
    (when ok
      (let ((cmd (format txt (shell-quote-argument (buffer-file-name)))))
	(message "Filesets: %s" cmd)
	(filesets-cmd-show-result cmd
				  (shell-command-to-string cmd))))))


;;; body
(defun filesets-get-filelist (entry &optional mode event)
  "Get all files for fileset ENTRY.
Assume MODE (see `filesets-entry-mode'), if provided."
  (let* ((mode (or mode
		   (filesets-entry-mode entry)))
	 (fl (case mode
	       ((:files)
		(filesets-entry-get-files entry))
	       ((:file)
		(list (filesets-entry-get-file entry)))
	       ((:ingroup)
		(let ((entry (expand-file-name
			      (if (stringp entry)
				  entry
				(filesets-entry-get-master entry)))))
		  (cons entry (filesets-ingroup-cache-get entry))))
	       ((:tree)
		(let ((dir  (nth 0 entry))
		      (patt (nth 1 entry)))
		  (filesets-directory-files dir patt ':files t)))
	       ((:pattern)
		(let ((dirpatt (filesets-entry-get-pattern entry)))
		  (if dirpatt
		      (let ((dir (filesets-entry-get-pattern--dir dirpatt))
			    (patt (filesets-entry-get-pattern--pattern dirpatt)))
			;;(filesets-message 3 "Filesets: scanning %s" dirpatt)
			(filesets-directory-files dir patt ':files t))
		    ;; (message "Filesets: malformed entry: %s" entry)))))))
		    (filesets-error 'error "Filesets: malformed entry: "
				    entry)))))))
    (filesets-filter-list fl
			  (lambda (file)
			    (not (filesets-filetype-property file event))))))

(defun filesets-open (&optional mode name lookup-name)
  "Open the fileset called NAME.
Use LOOKUP-NAME for searching additional data if provided."
  (interactive)
  (let* ((name (or name
		   (completing-read "Open fileset: " filesets-data nil t)))
	 (fileset (filesets-get-fileset-from-name name mode))
	 (lookup-fs (if lookup-name
			(filesets-get-fileset-from-name lookup-name)
		      fileset))
	 (mode (or mode (filesets-entry-mode lookup-fs))))
    (if fileset
	(let* ((files         (filesets-get-filelist fileset mode 'on-open-all))
	       (n             (length files))
	       (open-function (filesets-entry-get-open-fn nil lookup-fs)))
	  (if (or (<= n filesets-query-user-limit)
		  (y-or-n-p (format "Filesets: Open all %d files in %s? "
				    n name)))
	      (dolist (this files nil)
		(filesets-file-open open-function this))
	    (message "Filesets: cancelled")))
      (filesets-error 'error "Filesets: Unknown fileset: " name))))

(defun filesets-close (&optional mode name lookup-name)
  "Close all buffers belonging to the fileset called NAME.
Use LOOKUP-NAME for deducing the save-function, if provided."
  (interactive)
  (let* ((name (or name
		   (completing-read "Close fileset: " filesets-data nil t)))
	 (fileset (filesets-get-fileset-from-name name mode))
	 (lookup-fs (if lookup-name
			(filesets-get-fileset-from-name lookup-name)
		      fileset))
	 (mode (or mode (filesets-entry-mode lookup-fs))))
    (if fileset
	(let ((files         (filesets-get-filelist fileset mode 'on-close-all))
	      (save-function (filesets-entry-get-save-fn nil lookup-fs)))
	  (dolist (file-name files nil)
	    (let* ((buffer (get-file-buffer file-name)))
	      (if buffer
		  (filesets-file-close save-function buffer)))))
;      (message "Filesets: Unknown fileset: `%s'" name))))
      (filesets-error 'error "Filesets: Unknown fileset: " name))))

(defun filesets-add-buffer (&optional name buffer)
  "Add BUFFER (or current buffer) to the fileset called NAME.
User will be queried, if no fileset name is provided."
  (interactive)
  (let* ((buffer (or buffer
		     (current-buffer)))
	 (name   (or name
		     (completing-read
		      (format "Add '%s' to fileset: " buffer)
		      filesets-data nil)))
         (entry  (or (assoc name filesets-data)
                     (when (y-or-n-p
                            (format "Fileset %s does not exist. Create it? "
                                    name))
                       (progn
      (add-to-list 'filesets-data (list name '(:files)))
      (message
       "Fileset %s created.  Call `M-x filesets-save-config' to save."
       name)
      (car filesets-data))))))
    (if entry
	(let* ((files  (filesets-entry-get-files entry))
	       (this   (buffer-file-name buffer))
	       (inlist (filesets-member this files
					:test 'filesets-files-equalp)))
	  (cond
	   (inlist
	    (message "Filesets: '%s' is already in '%s'" this name))
	   ((and (equal (filesets-entry-mode entry) ':files)
		 this)
	    (filesets-entry-set-files entry (cons this files) t)
	    (filesets-set-config name 'filesets-data filesets-data))
	   (t
	    (message "Filesets: Can't add '%s' to fileset '%s'" this name)))))))

(defun filesets-remove-buffer (&optional name buffer)
  "Remove BUFFER (or current buffer) to fileset NAME.
User will be queried, if no fileset name is provided."
  (interactive)
  (let* ((buffer (or buffer
		     (current-buffer)))
	 (name   (or name
		     (completing-read
		      (format "Remove '%s' from fileset: " buffer)
		      filesets-data nil t)))
		 (entry (assoc name filesets-data)))
    (if entry
	(let* ((files  (filesets-entry-get-files entry))
	       (this   (buffer-file-name buffer))
	       (inlist (filesets-member this files
					:test 'filesets-files-equalp)))
	  ;;(message "%s %s %s" files this inlist)
	  (if (and files this inlist)
	      (let ((new (list (cons ':files (delete (car inlist) files)))))
		(setcdr entry new)
		(filesets-set-config name 'filesets-data filesets-data))
	    (message "Filesets: Can't remove '%s' from fileset '%s'"
		     this
		     name))))))

(defun filesets-convert-patterns (name)
  "Change fileset NAME's mode from :pattern to :files."
  (interactive)
  (let ((entry (assoc name filesets-data)))
    (if entry
	(let ((pattern  (filesets-entry-get-pattern entry))
	      (patfiles (filesets-get-filelist entry ':pattern)))
	  (if pattern
	      (progn
		(filesets-entry-set-files entry patfiles t)
		(filesets-set-config name 'filesets-data filesets-data)))))))

(defun filesets-edit ()
  "Customize `filesets-data'."
  (interactive)
  (customize-variable 'filesets-data))

(defun filesets-customize ()
  "Customize the filesets group."
  (interactive)
  (customize-group 'filesets))

(defun filesets-info ()
  "Display filesets's version information."
  (interactive)
  (if (y-or-n-p (format "Filesets v%s: visit homepage? " filesets-version))
      (filesets-goto-homepage)))

(defun filesets-goto-homepage ()
  "Show filesets's homepage."
  (interactive)
  (browse-url filesets-homepage))

(defun filesets-remake-shortcut (count submenu)
  "Remake a submenu's shortcut when wrapping long menus."
  (let* ((name (concat (filesets-get-shortcut count)
		       (substring (elt submenu 0) 2))))
    (if (listp submenu)
	(cons name (cdr submenu))
      (apply 'vector (list name (cdr (append submenu nil)))))))
;      (vconcat `[,name] (subseq submenu 1)))))

(defun filesets-wrap-submenu (submenu-body)
  "Split long submenus."
  (let ((bl (length submenu-body)))
    (if (or (= filesets-max-submenu-length 0)
	    (<= bl filesets-max-submenu-length))
	submenu-body
      (let* ((result  nil)
	     (factor (ceiling (/ (float bl)
				 filesets-max-submenu-length))))
	(do ((data  submenu-body (cdr data))
	     (n     1            (+ n 1))
	     (count 0            (+ count factor)))
	    ((or (> count bl)
		 (null data)))
;	  (let ((sl (subseq submenu-body count
	  (let ((sl (filesets-sublist submenu-body count
				      (let ((x (+ count factor)))
					(if (>= bl x)
					    x
					  nil)))))
	    (when sl
	      (setq result
		    (append
		     result
		     (if (= (length sl) 1)
			 (if filesets-menu-shortcuts-flag
			     (list (filesets-remake-shortcut n (car sl)))
			   sl)
		       `((,(concat
			    (filesets-get-shortcut n)
			    (let ((rv ""))
			      (do ((x sl (cdr x)))
				  ((null x))
				(let ((y (concat (elt (car x) 0)
						 (if (null (cdr x))
						     ""
						   ", "))))
				  (setq rv
					(concat
					 rv
					 (if filesets-menu-shortcuts-flag
					     (substring y 2)
					   y)))))
			      (if (> (length rv)
				     filesets-max-entry-length)
				  (concat
				   (substring rv 0 filesets-max-entry-length)
				   " ...")
				rv)))
			  ,@sl))))))))
	result))))

(defun filesets-get-menu-epilog (something &optional
					   mode lookup-name rebuild-flag)
  "Get submenu epilog for SOMETHING (usually a fileset).
If mode is :tree or :ingroup, SOMETHING is some weird construct and
LOOKUP-NAME is used as lookup name for retrieving fileset specific settings."
  (case mode
    ((:tree)
     `("---"
       ["Close all files" (filesets-close ',mode ',something ',lookup-name)]
       ["Run Command"     (filesets-run-cmd nil ',something ',mode)]
       [,(format "Browse with `%s'" (filesets-browser-name))
	(filesets-browse-dir ',(car something))]
       ,@(when rebuild-flag
	   `(["Rebuild this submenu"
	      (filesets-rebuild-this-submenu ',lookup-name)]))))
    ((:ingroup)
     `("---"
       ["Close all files" (filesets-close ',mode ',something ',lookup-name)]
       ["Run Command"     (filesets-run-cmd nil ',something ',mode)]
       ,@(when rebuild-flag
	   `(["Rebuild this submenu"
	      (filesets-rebuild-this-submenu ',lookup-name)]))))
    ((:pattern)
     `("---"
       ["Close all files" (filesets-close ',mode ',something)]
       ["Run Command"     (filesets-run-cmd nil ',something ',mode)]
       [,(format "Browse with `%s'" (filesets-browser-name))
	,(list 'filesets-browse-dir
	       (filesets-entry-get-pattern--dir
		(filesets-entry-get-pattern
		 (filesets-get-fileset-from-name something ':pattern))))]
;       [,(concat (if filesets-menu-shortcuts-flag
;		     (concat "Con" filesets-menu-shortcuts-marker "vert")
;		     "Convert")
;		 " :pattern to :files")
;	,(list (function filesets-convert-patterns) something)]
       ,@(when rebuild-flag
	   `(["Rebuild this submenu"
	      (filesets-rebuild-this-submenu ',lookup-name)]))))
    ((:files)
     `("---"
       [,(concat "Close all files") (filesets-close ',mode ',something)]
       ["Run Command"               (filesets-run-cmd nil ',something ',mode)]
       ["Add current buffer"
        (filesets-add-buffer ',something (current-buffer))]
       ["Remove current buffer"
	(filesets-remove-buffer ',something (current-buffer))]
       ,@(when rebuild-flag
	   `(["Rebuild this submenu"
	      (filesets-rebuild-this-submenu ',lookup-name)]))))
    (t
     (filesets-error 'error "Filesets: malformed definition of " something))))

(defun filesets-ingroup-get-data (master pos &optional fun)
  "Access to `filesets-ingroup-patterns'.  Extract data section."
  (let ((masterfile (file-name-nondirectory master))
	(fn (or fun (lambda (a b)
		      (and (stringp a)
			   (stringp b)
			   (string-match a b))))))
    (filesets-some (lambda (x)
		     (if (funcall fn (car x) masterfile)
			 (nth pos x)
		       nil))
		   filesets-ingroup-patterns)))

(defun filesets-ingroup-get-pattern (master)
  "Access to `filesets-ingroup-patterns'.  Extract patterns."
  (filesets-ingroup-get-data master 2))

(defun filesets-ingroup-get-remdupl-p (master)
  "Access to `filesets-ingroup-patterns'.  Extract remove-duplicates-flag."
  (filesets-ingroup-get-data master 1))

(defun filesets-ingroup-collect-finder (patt case-sensitivep)
  "Helper function for `filesets-ingroup-collect'.  Find pattern PATT."
  (let ((cfs case-fold-search)
	(rv  (progn
	       (setq case-fold-search (not case-sensitivep))
	       (re-search-forward patt nil t))))
    (setq case-fold-search cfs)
    rv))

(defun filesets-ingroup-cache-get (master)
  "Access to `filesets-ingroup-cache'."
  (lax-plist-get filesets-ingroup-cache master))

(defun filesets-ingroup-cache-put (master file)
  "Access to `filesets-ingroup-cache'."
  (let* ((emaster (expand-file-name master))
	 (this    (if file
		      (cons file (filesets-ingroup-cache-get emaster))
		    nil)))
    (setq filesets-ingroup-cache
	  (lax-plist-put filesets-ingroup-cache emaster this))))

(defun filesets-ingroup-collect-files (fs &optional remdupl-flag master depth)
  "Helper function for `filesets-ingroup-collect'.  Collect file names."
  (let* ((master       (or master
			   (filesets-entry-get-master fs)))
	 (remdupl-flag (or remdupl-flag
			   (filesets-ingroup-get-remdupl-p master))))
    (filesets-ingroup-cache-put master nil)
    (filesets-message 2 "Filesets: parsing %S" master)
    (let ((cmdpatts (filesets-ingroup-get-pattern master))
	  (count    0)
	  (rv       nil))
      (if cmdpatts
	  (dolist (this-def cmdpatts rv)
	    (let* ((this-patt (filesets-alist-get this-def ':pattern nil t))
		   (this-name (filesets-alist-get this-def ':name "" t))
		   (this-pp   (filesets-alist-get this-def ':preprocess nil t))
		   (this-mn   (filesets-alist-get this-def ':match-number 1 t))
		   (this-sd   (or depth
				  (filesets-alist-get this-def ':scan-depth 0 t)))
		   (this-csp  (filesets-alist-get this-def ':case-sensitive nil t))
		   (this-fn   (filesets-alist-get
			       this-def ':get-file-name 'filesets-which-file t))
		   (this-stubp (filesets-alist-get this-def ':stubp nil t))
		   (this-stub-flag (filesets-alist-get this-def ':stub-flag nil t))
		   (flist     nil)
		   (lst      nil))
	      (cond
	       ((not this-patt)
		(filesets-error 'error "Filesets: malformed :ingroup definition "
				this-def))
	       ((< this-sd 0)
		nil)
	       (t
		(with-temp-buffer
		  (insert-file-contents master)
		  (goto-char (point-min))
		  (when this-pp
		    (funcall this-pp))
		  (while (filesets-ingroup-collect-finder this-patt this-csp)
		    (let* ((txt (match-string this-mn))
			   (f   (funcall this-fn master txt)))
		      (when (and f
				 (not (member f flist))
				 (or (not remdupl-flag)
				     (not (filesets-member
					   f filesets-ingroup-files
					   :test 'filesets-files-equalp))))
			(let ((no-stub-flag
			       (and (not this-stub-flag)
				    (if this-stubp
					(not (funcall this-stubp master f))
				      t))))
			  (setq count (+ count 1))
			  (setq flist (cons f flist))
			  (setq filesets-ingroup-files
				(cons f filesets-ingroup-files))
			  (when no-stub-flag
			    (filesets-ingroup-cache-put master f))
			  (setq lst (append lst (list f))))))))
		(when lst
		  (setq rv
			(nconc rv
			       (mapcar (lambda (this)
					 `((,this ,this-name)
					   ,@(filesets-ingroup-collect-files
					      fs remdupl-flag this
					      (- this-sd 1))))
				       lst))))))))
	(filesets-message 2 "Filesets: no patterns defined for %S" master)))))

(defun filesets-ingroup-collect-build-menu (fs flist &optional other-count)
  "Helper function for `filesets-ingroup-collect'.  Build the menu.
FS is a fileset's name.  FLIST is a list returned by
`filesets-ingroup-collect-files'."
  (if (null flist)
      nil
    (let ((count 0)
	  (fsn    fs)
	  (rv     nil))
      (dolist (this flist rv)
	(setq count (+ count 1))
	(let* ((def    (if (listp this) (car this) (list this "")))
	       (files  (if (listp this) (cdr this) nil))
	       (master (nth 0 def))
	       (name   (nth 1 def))
	       (nm     (concat (filesets-get-shortcut (if (or (not other-count) files)
							  count other-count))
			       (if (or (null name) (equal name ""))
				   ""
				 (format "%s: " name))
			       (file-name-nondirectory master))))
	  (setq rv
		(append rv
			(if files
			    `((,nm
			       [,(concat "Inclusion Group: "
					 (file-name-nondirectory master))
				(filesets-open ':ingroup ',master ',fsn)]
			       "---"
			       [,master (filesets-file-open nil ',master ',fsn)]
			       "---"
			       ,@(let ((count 0))
				   (mapcar
				    (lambda (this)
				      (setq count (+ count 1))
				      (let ((ff (filesets-ingroup-collect-build-menu
						 fs (list this) count)))
					(if (= (length ff) 1)
					    (car ff)
					  ff)))
				    files))
			       ,@(filesets-get-menu-epilog master ':ingroup fsn)))
			  `([,nm (filesets-file-open nil ',master ',fsn)])))))))))

(defun filesets-ingroup-collect (fs remdupl-flag master)
  "Collect names of included files and build submenu."
  (filesets-ingroup-cache-put master nil)
  (filesets-message 2 "Filesets: parsing %S" master)
  (filesets-ingroup-collect-build-menu
   fs
   (filesets-ingroup-collect-files fs remdupl-flag master)))

(defun filesets-build-ingroup-submenu (lookup-name master)
  "Build a :ingroup submenu for file MASTER."
  (if (file-readable-p master)
      (let ((remdupl-flag  (filesets-ingroup-get-remdupl-p master)))
	(setq filesets-ingroup-files (list master))
	(filesets-ingroup-collect lookup-name remdupl-flag master))
    (if filesets-be-docile-flag
	(progn
	  (message "Filesets: can't parse %s" master)
	  nil)
      (filesets-error 'error "Filesets: can't parse " master))))

(defun filesets-build-dir-submenu-now (level depth entry lookup-name dir patt fd
					     &optional rebuild-flag)
  "Helper function for `filesets-build-dir-submenu'."
  ;;(filesets-message 3 "Filesets: scanning %s" dir)
  (if (or (= depth 0)
	  (< level depth))
      (let* ((dir       (file-name-as-directory dir))
	     (header    `([,(concat "Tree: "
				    (if (= level 0)
					dir
				      (concat ".../"
					      (file-name-as-directory
					       (file-name-nondirectory
						(directory-file-name dir))))))
			   ,(list (function filesets-open)
				  ':tree
				  `(quote (,dir ,patt))
				  lookup-name)]
			  "---"))
	     (dirlist   (filesets-directory-files dir patt nil nil fd))
	     (subdirs   (filesets-filter-dir-names dirlist))
	     (count     0)
	     (dirsmenu  (mapcar
			 (lambda (x)
			   (setq count (+ count 1))
			   (let* ((x  (file-name-as-directory x))
				  (xx (concat dir x))
				  (dd (filesets-build-dir-submenu-now
				       (+ level 1) depth entry
				       lookup-name xx patt fd))
				  (nm (concat (filesets-get-shortcut count)
					      x)))
			     (if dd
				 `(,nm ,@dd)
			       `[,nm ,(list 'filesets-browse-dir xx)])))
			 subdirs))
	     (files     (filesets-filter-dir-names dirlist t))
	     (filesmenu (mapcar (lambda (x)
				  (setq count (+ count 1))
				  `[,(concat (filesets-get-shortcut count)
					     x)
				    (filesets-file-open nil
							(quote ,(concat dir x))
							(quote ,lookup-name))])
				files)))
	(append header
		(filesets-wrap-submenu
		 (append
		  dirsmenu
		  filesmenu))
		(filesets-get-menu-epilog `(,dir ,patt) ':tree
					  lookup-name rebuild-flag)))
    nil))

(defun filesets-build-dir-submenu (entry lookup-name dir patt)
  "Build a :tree submenu named LOOKUP-NAME with base directory DIR including
all files matching PATT for filesets ENTRY."
  (let ((fd (filesets-entry-get-filter-dirs-flag entry))
	(depth (or (filesets-entry-get-tree-max-level entry)
		   filesets-tree-max-level)))
    (filesets-build-dir-submenu-now 0 depth entry lookup-name dir patt fd t)))

(defun filesets-build-submenu (count lookup-name entry)
  "Build submenu for the fileset ENTRY named LOOKUP-NAME.
Construct a shortcut from COUNT."
  (let ((lookup-name (or lookup-name
			 (filesets-data-get-name entry))))
    (message "Filesets: %s" lookup-name)
    (let ((mode (filesets-entry-mode entry))
	  (filesets-verbosity (filesets-entry-get-verbosity entry))
	  (this-lookup-name (concat (filesets-get-shortcut count)
				    lookup-name)))
      (case mode
	((:file)
	 (let* ((file (filesets-entry-get-file entry)))
	   `[,this-lookup-name
	     (filesets-file-open nil ',file ',lookup-name)]))
	(t
	 `(,this-lookup-name
	   ,@(case mode
	       ((:pattern)
		(let* ((files    (filesets-get-filelist entry mode 'on-ls))
		       (dirpatt  (filesets-entry-get-pattern entry))
		       (pattname (apply 'concat (cons "Pattern: " dirpatt)))
		       (count   0))
		  ;;(filesets-message 3 "Filesets: scanning %S" pattname)
		  `([,pattname
		     ,(list (function filesets-open) mode lookup-name)]
		    "---"
		    ,@(filesets-wrap-submenu
		       (mapcar
			(lambda (x)
			  (setq count (+ count 1))
			  `[,(concat (filesets-get-shortcut count)
				     (file-name-nondirectory x))
			    (filesets-file-open nil ',x ',lookup-name)])
			files))
		    ,@(filesets-get-menu-epilog lookup-name mode
						lookup-name t))))
	       ((:ingroup)
		(let* ((master (filesets-entry-get-master entry)))
		  ;;(filesets-message 3 "Filesets: parsing %S" master)
		  `([,(concat "Inclusion Group: "
			      (file-name-nondirectory master))
		     (filesets-open ',mode ',master ',lookup-name)]
		    "---"
		    [,master (filesets-file-open nil ',master ',lookup-name)]
		    "---"
		    ,@(filesets-wrap-submenu
		       (filesets-build-ingroup-submenu lookup-name master))
		    ,@(filesets-get-menu-epilog master mode lookup-name t))))
	       ((:tree)
		(let* ((dirpatt (filesets-entry-get-tree entry))
		       (dir     (car dirpatt))
		       (patt    (cadr dirpatt)))
		  (filesets-build-dir-submenu entry lookup-name dir patt)))
	       ((:files)
		(let ((files (filesets-get-filelist entry mode 'on-open-all))
		      (count 0))
		  `([,(concat "Files: " lookup-name)
		     (filesets-open ',mode ',lookup-name)]
		    "---"
		    ,@(filesets-wrap-submenu
		       (mapcar
			(lambda (x)
			  (setq count (+ count 1))
			  `[,(concat (filesets-get-shortcut count)
				     (file-name-nondirectory x))
			    (filesets-file-open nil ',x ',lookup-name)])
			(filesets-conditional-sort
			 files
			 (function file-name-nondirectory))))
		    ,@(filesets-get-menu-epilog lookup-name mode
						lookup-name t)))))))))))

(defun filesets-remove-from-ubl (&optional buffer)
  "BUFFER or current buffer require update of the filesets menu."
  (let ((b (or buffer
	       (current-buffer))))
    (if (member b filesets-updated-buffers)
	(setq filesets-updated-buffers
	      (delete b filesets-updated-buffers)))))

(defun filesets-build-menu-now (from-scratch-flag)
  "Update the filesets menu.
Build all new if FROM-SCRATCH-FLAG is non-nil.  (To really build from the
bottom up, set `filesets-submenus' to nil, first.)"
  (when (or from-scratch-flag
	    filesets-has-changed-flag
	    (not filesets-menu-cache))
    (setq filesets-menu-cache nil)
    (setq filesets-has-changed-flag nil)
    (setq filesets-updated-buffers nil)
    (setq filesets-update-cache-file-flag t)
    (do ((data  (filesets-conditional-sort filesets-data (function car))
		(cdr data))
	 (count 1 (+ count 1)))
	((null data))
      (let* ((this    (car data))
	     (name    (filesets-data-get-name this))
	     (cached  (lax-plist-get filesets-submenus name))
	     (submenu (or cached
			  (filesets-build-submenu count name this))))
	(unless cached
	  (setq filesets-submenus
		(lax-plist-put filesets-submenus name submenu)))
	(unless (filesets-entry-get-dormant-flag this)
	  (setq filesets-menu-cache
		(append filesets-menu-cache (list submenu))))))
    (when filesets-cache-save-often-flag
      (filesets-menu-cache-file-save-maybe)))
  (let ((cb (current-buffer)))
    (when (not (member cb filesets-updated-buffers))
      (add-submenu
       filesets-menu-path
       `(,filesets-menu-name
	 ("# Filesets"
	  ["Edit Filesets"   filesets-edit]
	  ["Save Filesets"   filesets-save-config]
	  ["Save Menu Cache" filesets-menu-cache-file-save]
	  ["Rebuild Menu"    filesets-build-menu]
	  ["Customize"       filesets-customize]
	  ["About"           filesets-info])
	 ,(filesets-get-cmd-menu)
	 "---"
	 ,@filesets-menu-cache)
       filesets-menu-before
       filesets-menu-in-menu)
      (setq filesets-updated-buffers
	    (cons cb filesets-updated-buffers))
      ;; This wipes out other messages in the echo area.
      ;; (message nil)
      ;;(message "Filesets updated: %s" cb)
      )))

(defun filesets-build-menu-maybe ()
  "Update the filesets menu."
  (interactive)
  (filesets-build-menu-now nil))

(defun filesets-build-menu ()
  "Force rebuild of the filesets menu."
  (interactive)
  ;(setq filesets-submenus nil)
  (filesets-reset-fileset)
  (filesets-build-menu-now t)
  (filesets-menu-cache-file-save-maybe))

(defun filesets-rebuild-this-submenu (fileset)
  "Force rebuild of FILESET submenu."
  (filesets-reset-fileset fileset)
  (filesets-build-menu-now t))

(defun filesets-menu-cache-file-save-maybe (&optional simply-do-it)
  "Write filesets' cache file.
If SIMPLY-DO-IT is non-nil, the cache file will be written no matter if
fileset thinks this is necessary or not."
  (when (and (not (equal filesets-menu-cache-file ""))
	     (or simply-do-it
		 filesets-update-cache-file-flag))
    (when (file-exists-p filesets-menu-cache-file)
      (delete-file filesets-menu-cache-file))
    ;;(message "Filesets: saving menu cache")
    (with-temp-buffer
      (dolist (this filesets-menu-cache-contents)
	(if (get this 'custom-type)
	    (progn
	      (insert (format "(setq-default %s '%S)" this (eval this)))
	      (when filesets-menu-ensure-use-cached
		(newline)
		(insert (format "(setq %s (cons '%s %s))"
				'filesets-ignore-next-set-default
				this
				'filesets-ignore-next-set-default))))
	  (insert (format "(setq %s '%S)" this (eval this))))
	(newline 2))
      (insert (format "(setq filesets-cache-version %S)" filesets-version))
      (newline 2)
      (when filesets-cache-hostname-flag
	(insert (format "(setq filesets-cache-hostname %S)" (system-name)))
	(newline 2))
      (run-hooks 'filesets-cache-fill-content-hooks)
      (write-file filesets-menu-cache-file))
    (setq filesets-has-changed-flag nil)
    (setq filesets-update-cache-file-flag nil)))

(defun filesets-menu-cache-file-save ()
  "Save filesets' menu cache file."
  (interactive)
  (filesets-menu-cache-file-save-maybe t))

(defun filesets-update-cleanup ()
  "Rebuild the menu and save the cache file after updating user data."
  (interactive)
  (message "Filesets v%s: updating menu & cache from version %s"
	   filesets-version (or filesets-cache-version "???"))
  (filesets-build-menu)
  (filesets-menu-cache-file-save-maybe)
  (filesets-menu-cache-file-load))

(defun filesets-update-pre010505 ()
  (let ((msg
"Filesets: manual editing of user data required!

Filesets has detected that you were using an older version before,
which requires some manual updating. Type 'y' for editing the startup
file now.

The layout of `filesets-data' has changed. Please delete your cache file
and edit your startup file as shown below:

1. `filesets-data': Edit all :pattern filesets in your startup file and
transform all entries as shown in this example:

   	\(\"Test\" (:pattern \"~/dir/^pattern$\"))
	--> \(\"Test\" (:pattern \"~/dir/\" \"^pattern$\"))

2. `filesets-data': Change all occurrences of \":document\" to \":ingroup\":

      \(\(\"Test\" \(:document \"~/dir/file\"))
      --> \(\(\"Test\" \(:ingroup \"~/dir/file\"))

3. `filesets-subdocument-patterns': If you already modified the variable
previously called `filesets-subdocument-patterns', change its name to
`filesets-ingroup-patterns'.

4. `filesets-menu-cache-contents': If you already modified this
variable, change the entry `filesets-subdocument--cache' to
`filesets-ingroup-cache'.

5. Type M-x filesets-update-cleanup and restart Emacs.

We apologize for the inconvenience."))
    (let* ((cf (or custom-file user-init-file)))
      (switch-to-buffer-other-frame "*Filesets update*")
      (insert msg)
      (when (y-or-n-p (format "Edit startup (%s) file now? " cf))
	(find-file-other-window cf))
      (filesets-error 'error msg))))

(defun filesets-update (cached-version)
  "Do some cleanup after updating filesets.el."
  (cond
   ((or (not cached-version)
	(string< cached-version "1.5.5")
	(boundp 'filesets-subdocument-patterns))
    (filesets-update-pre010505)))
  (filesets-update-cleanup))

(defun filesets-menu-cache-file-load ()
  "Load filesets' menu cache file."
  (cond
   ((and (not (equal filesets-menu-cache-file ""))
	 (file-readable-p filesets-menu-cache-file))
    (load-file filesets-menu-cache-file)
    (if (and (equal filesets-cache-version filesets-version)
	     (if filesets-cache-hostname-flag
		 (equal filesets-cache-hostname (system-name))
	       t))
	(progn
	  (setq filesets-update-cache-file-flag nil)
	  t)
      (filesets-update filesets-cache-version)))
   (t
    (setq filesets-update-cache-file-flag t)
    nil)))

(defun filesets-exit ()
  (filesets-menu-cache-file-save-maybe))

;;;###autoload
(defun filesets-init ()
  "Filesets initialization.
Set up hooks, load the cache file -- if existing -- and build the menu."
  (add-hook (if (featurep 'xemacs) 'activate-menubar-hook 'menu-bar-update-hook)
	    (function filesets-build-menu-maybe))
  (add-hook 'kill-buffer-hook (function filesets-remove-from-ubl))
  (add-hook 'first-change-hook (function filesets-reset-filename-on-change))
  (add-hook 'kill-emacs-hook (function filesets-exit))
  (if (filesets-menu-cache-file-load)
      (progn
	(filesets-build-menu-maybe)
	;;Well, normally when we use XEmacs <= 21.4, custom.el is loaded
	;;after init.el.  This more or less ignores the next
	;;`filesets-data-set-default'
	(if filesets-menu-ensure-use-cached
	    (setq filesets-menu-use-cached-flag t)))
    (filesets-build-menu)))


(provide 'filesets)

;; Local Variables:
;; sentence-end-double-space:t
;; End:

;;; filesets.el ends here
