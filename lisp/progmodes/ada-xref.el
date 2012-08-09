;; ada-xref.el --- for lookup and completion in Ada mode

;; Copyright (C) 1994-2012  Free Software Foundation, Inc.

;; Author: Markus Heritsch <Markus.Heritsch@studbox.uni-stuttgart.de>
;;      Rolf Ebert <ebert@inf.enst.fr>
;;      Emmanuel Briot <briot@gnat.com>
;; Maintainer: Stephen Leake <stephen_leake@stephe-leake.org>
;; Keywords: languages ada xref
;; Package: ada-mode

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
;;; This Package provides a set of functions to use the output of the
;;; cross reference capabilities of the GNAT Ada compiler
;;; for lookup and completion in Ada mode.
;;;
;;; If a file *.`adp' exists in the ada-file directory, then it is
;;; read for configuration information.  It is read only the first
;;; time a cross-reference is asked for, and is not read later.

;;; You need Emacs >= 20.2 to run this package


;;; History:
;;

;;; Code:

;; ----- Requirements -----------------------------------------------------

(require 'compile)
(require 'comint)
(require 'find-file)
(require 'ada-mode)

;; ------ User variables
(defcustom ada-xref-other-buffer t
  "*If nil, always display the cross-references in the same buffer.
Otherwise create either a new buffer or a new frame."
  :type 'boolean :group 'ada)

(defcustom ada-xref-create-ali nil
  "*If non-nil, run gcc whenever the cross-references are not up-to-date.
If nil, the cross-reference mode never runs gcc."
  :type 'boolean :group 'ada)

(defcustom ada-xref-confirm-compile nil
  "*If non-nil, ask for confirmation before compiling or running the application."
  :type 'boolean :group 'ada)

(defcustom ada-krunch-args "0"
  "*Maximum number of characters for filenames created by `gnatkr'.
Set to 0, if you don't use crunched filenames.  This should be a string."
  :type 'string :group 'ada)

(defcustom ada-gnat-cmd "gnat"
  "Default GNAT project file parser.
Will be run with args \"list -v -Pfile.gpr\".
Default is standard GNAT distribution; alternate \"gnatpath\"
is faster, available from Ada mode web site."
  :type 'string :group 'ada)

(defcustom ada-gnatls-args '("-v")
  "*Arguments to pass to `gnatls' to find location of the runtime.
Typical use is to pass `--RTS=soft-floats' on some systems that support it.

You can also add `-I-' if you do not want the current directory to be included.
Otherwise, going from specs to bodies and back will first look for files in the
current directory.  This only has an impact if you are not using project files,
but only ADA_INCLUDE_PATH."
  :type '(repeat string) :group 'ada)

(defcustom ada-prj-default-comp-opt "-gnatq -gnatQ"
  "Default compilation options."
  :type 'string :group 'ada)

(defcustom ada-prj-default-bind-opt ""
  "Default binder options."
  :type 'string :group 'ada)

(defcustom ada-prj-default-link-opt ""
  "Default linker options."
  :type 'string :group 'ada)

(defcustom ada-prj-default-gnatmake-opt "-g"
  "Default options for `gnatmake'."
  :type 'string :group 'ada)

(defcustom ada-prj-default-gpr-file ""
  "Default GNAT project file.
If non-empty, this file is parsed to set the source and object directories for
the Ada mode project."
  :type 'string :group 'ada)

(defcustom ada-prj-ada-project-path-sep
  (cond ((boundp 'path-separator) path-separator) ; 20.3+
	((memq system-type '(windows-nt ms-dos)) ";")
	(t ":"))
  "Default separator for ada_project_path project variable."
  :type 'string :group 'ada)

(defcustom ada-prj-gnatfind-switches "-rf"
  "Default switches to use for `gnatfind'.
You should modify this variable, for instance to add `-a', if you are working
in an environment where most ALI files are write-protected.
The command `gnatfind' is used every time you choose the menu
\"Show all references\"."
  :type 'string :group 'ada)

(defcustom ada-prj-default-check-cmd
  (concat "${cross_prefix}gnatmake -u -c -gnatc ${gnatmake_opt} ${full_current}"
	  " -cargs ${comp_opt}")
  "*Default command to be used to compile a single file.
Emacs will substitute the current filename for ${full_current}, or add
the filename at the end.  This is the same syntax as in the project file."
  :type 'string :group 'ada)

(defcustom ada-prj-default-comp-cmd
  (concat "${cross_prefix}gnatmake -u -c ${gnatmake_opt} ${full_current} -cargs"
	  " ${comp_opt}")
  "*Default command to be used to compile a single file.
Emacs will substitute the current filename for ${full_current}, or add
the filename at the end.  This is the same syntax as in the project file."
  :type 'string :group 'ada)

(defcustom ada-prj-default-debugger "${cross_prefix}gdb"
  "*Default name of the debugger."
  :type 'string :group 'ada)

(defcustom ada-prj-default-make-cmd
  (concat "${cross_prefix}gnatmake -o ${main} ${main} ${gnatmake_opt} "
	  "-cargs ${comp_opt} -bargs ${bind_opt} -largs ${link_opt}")
  "*Default command to be used to compile the application.
This is the same syntax as in the project file."
  :type 'string :group 'ada)

(defcustom ada-prj-default-project-file ""
  "*Name of the current project file.
Emacs will not try to use the search algorithm to find the project file if
this string is not empty.  It is set whenever a project file is found."
  :type '(file :must-match t) :group 'ada)

(defcustom ada-gnatstub-opts "-q -I${src_dir}"
  "*Options to pass to `gnatsub' to generate the body of a package.
This has the same syntax as in the project file (with variable substitution)."
  :type 'string :group 'ada)

(defcustom ada-always-ask-project nil
  "*If nil, use default values when no project file was found.
Otherwise, ask the user for the name of the project file to use."
  :type 'boolean :group 'ada)

(defconst ada-on-ms-windows (memq system-type '(windows-nt))
  "True if we are running on Windows.")

(defcustom ada-tight-gvd-integration nil
  "*If non-nil, a new Emacs frame will be swallowed in GVD when debugging.
If GVD is not the debugger used, nothing happens."
  :type 'boolean :group 'ada)

(defcustom ada-xref-search-with-egrep t
  "*If non-nil, use egrep to find the possible declarations for an entity.
This alternate method is used when the exact location was not found in the
information provided by GNAT.  However, it might be expensive if you have a lot
of sources, since it will search in all the files in your project."
  :type 'boolean :group 'ada)

(defvar ada-load-project-hook nil
  "Hook that is run when loading a project file.
Each function in this hook takes one argument FILENAME, that is the name of
the project file to load.
This hook should be used to support new formats for the project files.

If the function can load the file with the given filename, it should create a
buffer that contains a conversion of the file to the standard format of the
project files, and return that buffer.  (The usual \"src_dir=\" or \"obj_dir=\"
lines.)  It should return nil if it doesn't know how to convert that project
file.")


;; ------- Nothing to be modified by the user below this
(defvar ada-last-prj-file ""
  "Name of the last project file entered by the user.")

(defconst ada-prj-file-extension ".adp"
  "The extension used for project files.")

(defvar ada-xref-runtime-library-specs-path '()
  "Directories where the specs for the standard library is found.
This is used for cross-references.")

(defvar ada-xref-runtime-library-ali-path '()
  "Directories where the ali for the standard library is found.
This is used for cross-references.")

(defvar ada-xref-pos-ring '()
  "List of positions selected by the cross-references functions.
Used to go back to these positions.")

(defvar ada-cd-command
  (if (string-match "cmdproxy.exe" shell-file-name)
      "cd /d"
    "cd")
  "Command to use to change to a specific directory.
On Windows systems using `cmdproxy.exe' as the shell,
we need to use `/d' or the drive is never changed.")

(defvar ada-command-separator (if ada-on-ms-windows " && " "\n")
  "Separator to use between multiple commands to `compile' or `start-process'.
`cmdproxy.exe' doesn't recognize multiple-line commands, so we have to use
\"&&\" for now.")

(defconst ada-xref-pos-ring-max 16
  "Number of positions kept in the list `ada-xref-pos-ring'.")

(defvar ada-operator-re
  "\\+\\|-\\|/\\|\\*\\*\\|\\*\\|=\\|&\\|abs\\|mod\\|rem\\|and\\|not\\|or\\|xor\\|<=\\|<\\|>=\\|>"
  "Regexp to match for operators.")

(defvar ada-xref-project-files '()
  "Associative list of project files with properties.
It has the format: (project project ...)
A project has the format: (project-file . project-plist)
\(See 'apropos plist' for operations on property lists).
See `ada-default-prj-properties' for the list of valid properties.
The current project is retrieved with `ada-xref-current-project'.
Properties are retrieved with `ada-xref-get-project-field', set with
`ada-xref-set-project-field'.  If project properties are accessed with no
project file, a (nil . default-properties) entry is created.")


;; ----- Identlist manipulation -------------------------------------------
;; An identlist is a vector that is used internally to reference an identifier
;; To facilitate its use, we provide the following macros

(defmacro ada-make-identlist () (make-vector 8 nil))
(defmacro ada-name-of   (identlist)    (list 'aref identlist 0))
(defmacro ada-line-of   (identlist)    (list 'aref identlist 1))
(defmacro ada-column-of (identlist)    (list 'aref identlist 2))
(defmacro ada-file-of   (identlist)    (list 'aref identlist 3))
(defmacro ada-ali-index-of    (identlist) (list 'aref identlist 4))
(defmacro ada-declare-file-of (identlist) (list 'aref identlist 5))
(defmacro ada-references-of   (identlist) (list 'aref identlist 6))
(defmacro ada-on-declaration  (identlist) (list 'aref identlist 7))

(defmacro ada-set-name         (identlist name) (list 'aset identlist 0 name))
(defmacro ada-set-line         (identlist line) (list 'aset identlist 1 line))
(defmacro ada-set-column       (identlist col)  (list 'aset identlist 2 col))
(defmacro ada-set-file         (identlist file) (list 'aset identlist 3 file))
(defmacro ada-set-ali-index   (identlist index) (list 'aset identlist 4 index))
(defmacro ada-set-declare-file (identlist file) (list 'aset identlist 5 file))
(defmacro ada-set-references   (identlist ref)  (list 'aset identlist 6 ref))
(defmacro ada-set-on-declaration (ident value) (list 'aset ident 7 value))

(defsubst ada-get-ali-buffer (file)
  "Read the ali file FILE into a new buffer, and return the buffer's name."
  (find-file-noselect (ada-get-ali-file-name file)))


;; -----------------------------------------------------------------------

(defun ada-quote-cmd (cmd)
  "Duplicate all `\\' characters in CMD so that it can be passed to `compile'."
  (mapconcat 'identity (split-string cmd "\\\\") "\\\\"))

(defun ada-find-executable (exec-name)
  "Find the full path to the executable file EXEC-NAME.
If not found, throw an error.
On Windows systems, this will properly handle .exe extension as well."
  (let ((result (or (ada-find-file-in-dir exec-name exec-path)
		    (ada-find-file-in-dir (concat exec-name ".exe") exec-path))))
    (if result
	result
      (error "'%s' not found in path" exec-name))))

(defun ada-initialize-runtime-library (cross-prefix)
  "Initialize the variables for the runtime library location.
CROSS-PREFIX is the prefix to use for the `gnatls' command."
  (let ((gnatls
	 (condition-case nil
	     ;; if gnatls not found, just give up (may not be using GNAT)
	     (ada-find-executable (concat cross-prefix "gnatls"))
	   (error nil))))
    (if gnatls
	(save-excursion
	  (setq ada-xref-runtime-library-specs-path '()
		ada-xref-runtime-library-ali-path   '())
	  (set-buffer (get-buffer-create "*gnatls*"))
	  (widen)
	  (erase-buffer)
	  ;;  Even if we get an error, delete the *gnatls* buffer
	  (unwind-protect
	      (let ((status (apply 'call-process gnatls (append '(nil t nil) ada-gnatls-args))))
		(goto-char (point-min))

		;; Since we didn't provide all the inputs gnatls expects, it returns status 4
		(if (/= 4 status)
		    (error (buffer-substring (point) (line-end-position))))

		;;  Source path

		(search-forward "Source Search Path:")
		(forward-line 1)
		(while (not (looking-at "^$"))
		  (back-to-indentation)
		  (if (looking-at "<Current_Directory>")
		      (add-to-list 'ada-xref-runtime-library-specs-path  ".")
		    (add-to-list 'ada-xref-runtime-library-specs-path
				 (buffer-substring-no-properties
				  (point)
				  (point-at-eol))))
		  (forward-line 1))

		;;  Object path

		(search-forward "Object Search Path:")
		(forward-line 1)
		(while (not (looking-at "^$"))
		  (back-to-indentation)
		  (if (looking-at "<Current_Directory>")
		      (add-to-list 'ada-xref-runtime-library-ali-path ".")
		    (add-to-list 'ada-xref-runtime-library-ali-path
				 (buffer-substring-no-properties
				  (point)
				  (point-at-eol))))
		  (forward-line 1))
		)
	    (kill-buffer nil))))

    (set 'ada-xref-runtime-library-specs-path
	 (reverse ada-xref-runtime-library-specs-path))
    (set 'ada-xref-runtime-library-ali-path
	 (reverse ada-xref-runtime-library-ali-path))
    ))

(defun ada-gnat-parse-gpr (plist gpr-file)
  "Set gpr_file, src_dir and obj_dir properties in PLIST by parsing GPR-FILE.
Return new value of PLIST.
GPR_FILE must be full path to file, normalized.
src_dir, obj_dir will include compiler runtime.
Assumes environment variable ADA_PROJECT_PATH is set properly."
  (with-current-buffer (get-buffer-create "*gnatls*")
    (erase-buffer)

    ;; this can take a long time; let the user know what's up
    (message "Parsing %s ..." gpr-file)

    ;;  Even if we get an error, delete the *gnatls* buffer
    (unwind-protect
	(let* ((cross-prefix (plist-get plist 'cross_prefix))
	       (gnat (concat cross-prefix ada-gnat-cmd))
	       ;; Putting quotes around gpr-file confuses gnatpath on Lynx; not clear why
	       (gpr-opt (concat "-P" gpr-file))
	       (src-dir '())
	       (obj-dir '())
	       (status (call-process gnat nil t nil "list" "-v" gpr-opt)))
	  (goto-char (point-min))

	  (if (/= 0 status)
	      (error (buffer-substring (point) (line-end-position))))

	  ;;  Source path

	  (search-forward "Source Search Path:")
	  (forward-line 1) ; first directory in list
	  (while (not (looking-at "^$")) ; terminate on blank line
	    (back-to-indentation) ; skip whitespace
	    (add-to-list 'src-dir
                         (if (looking-at "<Current_Directory>")
                             default-directory
			   (expand-file-name
			    (buffer-substring-no-properties
			     (point) (line-end-position)))))
	    (forward-line 1))

	  ;;  Object path

	  (search-forward "Object Search Path:")
	  (forward-line 1)
	  (while (not (looking-at "^$"))
	    (back-to-indentation)
	    (add-to-list 'obj-dir
                         (if (looking-at "<Current_Directory>")
                             default-directory
			   (expand-file-name
			    (buffer-substring-no-properties
			     (point) (line-end-position)))))
	    (forward-line 1))

	  ;; Set properties
	  (setq plist (plist-put plist 'gpr_file gpr-file))
	  (setq plist (plist-put plist 'src_dir src-dir))
	  (plist-put plist 'obj_dir obj-dir)
	  )
      (kill-buffer nil)
      (message "Parsing %s ... done" gpr-file)
      )
    ))

(defun ada-treat-cmd-string (cmd-string)
  "Replace variable references ${var} in CMD-STRING with the appropriate value.
Also replace standard environment variables $var.
Assumes project exists.
As a special case, ${current} is replaced with the name of the current
file, minus extension but with directory, and ${full_current} is
replaced by the name including the extension."

  (while (string-match "\\(-[^-\$IO]*[IO]\\)?\${\\([^}]+\\)}" cmd-string)
    (let (value
	  (name (match-string 2 cmd-string)))
      (cond
       ((string= name "current")
	(setq value (file-name-sans-extension (buffer-file-name))))
       ((string= name "full_current")
	(setq value (buffer-file-name)))
       (t
	(save-match-data
	  (setq value (ada-xref-get-project-field (intern name))))))

      ;; Check if there is an environment variable with the same name
      (if (null value)
	  (if (not (setq value (getenv name)))
	      (message "%s" (concat "No project or environment variable " name " found"))))

      (cond
       ((null value)
	(setq cmd-string (replace-match "" t t cmd-string)))
       ((stringp value)
	(setq cmd-string (replace-match value t t cmd-string)))
       ((listp value)
	(let ((prefix (match-string 1 cmd-string)))
	  (setq cmd-string (replace-match
			    (mapconcat (lambda(x) (concat prefix x)) value " ")
			    t t cmd-string)))))
      ))
  (substitute-in-file-name cmd-string))


(defun ada-xref-get-project-field (field)
  "Extract the value of FIELD from the current project file.
Project variables are substituted.

Note that for src_dir and obj_dir, you should rather use
`ada-xref-get-src-dir-field' or `ada-xref-get-obj-dir-field'
which will in addition return the default paths."

  (let* ((project-plist (cdr (ada-xref-current-project)))
	 (value (plist-get project-plist field)))

    (cond
     ((eq field 'gnatmake_opt)
      (let ((gpr-file (plist-get project-plist 'gpr_file)))
	(if (not (string= gpr-file ""))
	    (setq value (concat "-P\"" gpr-file "\" " value)))))

     ;; FIXME: check for src_dir, obj_dir here, rather than requiring user to do it
     (t
      nil))

    ;; Substitute the ${...} constructs in all the strings, including
    ;; inside lists
    (cond
     ((stringp value)
      (ada-treat-cmd-string value))
     ((null value)
      nil)
     ((listp value)
      (mapcar (lambda(x) (if x (ada-treat-cmd-string x) x)) value))
     (t
      value)
     )
  ))

(defun ada-xref-get-src-dir-field ()
  "Return the full value for src_dir, including the default directories.
All the directories are returned as absolute directories."

  (let ((build-dir (ada-xref-get-project-field 'build_dir)))
    (append
     ;; Add ${build_dir} in front of the path
     (list build-dir)

     (ada-get-absolute-dir-list (ada-xref-get-project-field 'src_dir)
				build-dir)

     ;; Add the standard runtime at the end
     ada-xref-runtime-library-specs-path)))

(defun ada-xref-get-obj-dir-field ()
  "Return the full value for obj_dir, including the default directories.
All the directories are returned as absolute directories."

  (let ((build-dir (ada-xref-get-project-field 'build_dir)))
    (append
     ;; Add ${build_dir} in front of the path
     (list build-dir)

     (ada-get-absolute-dir-list (ada-xref-get-project-field 'obj_dir)
				build-dir)

     ;; Add the standard runtime at the end
     ada-xref-runtime-library-ali-path)))

(defun ada-xref-set-project-field (field value)
  "Set FIELD to VALUE in current project.  Assumes project exists."
  ;; same algorithm to find project-plist as ada-xref-current-project
  (let* ((file-name (ada-xref-current-project-file))
	 (project-plist (cdr (assoc file-name ada-xref-project-files))))

    (setq project-plist (plist-put project-plist field value))
    (setcdr (assoc file-name ada-xref-project-files) project-plist)))

(defun ada-xref-update-project-menu ()
  "Update the menu Ada->Project, with the list of available project files."
  ;; Create the standard items.
  (let ((submenu
	 `("Project"
	   ["Load..." ada-set-default-project-file t]
	   ["New..."  ada-prj-new t]
	   ["Edit..." ada-prj-edit t]
	   "---"
	   ;;  Add the project files
	   ,@(mapcar
	      (lambda (x)
		(let* ((name (or (car x) "<default>"))
		       (command `(lambda ()
				   "Select the current project file."
				   (interactive)
				   (ada-select-prj-file ,name))))
		  (vector
		   (file-name-nondirectory name)
		   command
		   :button (cons
			    :toggle
			    (equal ada-prj-default-project-file
				   (car x))
			    ))))

	      (or ada-xref-project-files '(nil))))))

    (easy-menu-add-item ada-mode-menu '() submenu)))


;;-------------------------------------------------------------
;;--  Searching a file anywhere on the source path.
;;--
;;--  The following functions provide support for finding a file anywhere
;;--  on the source path, without providing an explicit directory.
;;--  They also provide file name completion in the minibuffer.
;;--
;;--  Public subprograms:  ada-find-file
;;--
;;-------------------------------------------------------------

(defun ada-do-file-completion (string predicate flag)
  "Completion function when reading a file from the minibuffer.
Completion is attempted in all the directories in the source path,
as defined in the project file."
  ;; FIXME: doc arguments

  ;; This function is not itself interactive, but it is called as part
  ;; of the prompt of interactive functions, so we require a project
  ;; file.
  (ada-require-project-file)
  (let (list
	(dirs (ada-xref-get-src-dir-field)))

    (while dirs
      (if (file-directory-p (car dirs))
	  (set 'list (append list (file-name-all-completions string (car dirs)))))
      (set 'dirs (cdr dirs)))
    (cond ((equal flag 'lambda)
	   (assoc string list))
	  (flag
	   list)
	  (t
	   (try-completion string
			   (mapcar (lambda (x) (cons x 1)) list)
		      predicate)))))

;;;###autoload
(defun ada-find-file (filename)
  "Open FILENAME, from anywhere in the source path.
Completion is available."
  (interactive
   (list (completing-read "File: " 'ada-do-file-completion)))
  (let ((file (ada-find-src-file-in-dir filename)))
    (if file
	(find-file file)
      (error "%s not found in src_dir" filename))))


;; ----- Utilities -------------------------------------------------

(defun ada-require-project-file ()
  "If the current project does not exist, load or create a default one.
Should only be called from interactive functions."
  (if (string= "" ada-prj-default-project-file)
      (ada-reread-prj-file (ada-prj-find-prj-file t))))

(defun ada-xref-current-project-file ()
  "Return the current project file name; never nil.
Call `ada-require-project-file' first if a project must exist."
  (if (not (string= "" ada-prj-default-project-file))
      ada-prj-default-project-file
    (ada-prj-find-prj-file t)))

(defun ada-xref-current-project ()
  "Return the current project.
Call `ada-require-project-file' first to ensure a project exists."
  (let ((file-name (ada-xref-current-project-file)))
    (assoc file-name ada-xref-project-files)))

(defun ada-show-current-project ()
  "Display current project file name in message buffer."
  (interactive)
  (message (ada-xref-current-project-file)))

(defun ada-show-current-main ()
  "Display current main file name in message buffer."
  (interactive)
  (message "ada-mode main: %s" (ada-xref-get-project-field 'main)))

(defun ada-xref-push-pos (filename position)
  "Push (FILENAME, POSITION) on the position ring for cross-references."
  (setq ada-xref-pos-ring (cons (list position filename) ada-xref-pos-ring))
  (if (> (length ada-xref-pos-ring) ada-xref-pos-ring-max)
      (setcdr (nthcdr (1- ada-xref-pos-ring-max) ada-xref-pos-ring) nil)))

(defun ada-xref-goto-previous-reference ()
  "Go to the previous cross-reference we were on."
  (interactive)
  (if ada-xref-pos-ring
      (let ((pos (car ada-xref-pos-ring)))
	(setq ada-xref-pos-ring (cdr ada-xref-pos-ring))
	(find-file (car (cdr pos)))
	(goto-char (car pos)))))

(defun ada-convert-file-name (name)
  "Convert from NAME to a name that can be used by the compilation commands.
This is overridden on VMS to convert from VMS filenames to Unix filenames."
  name)
;; FIXME: use convert-standard-filename instead

(defun ada-set-default-project-file (file)
  "Set FILE as the current project file."
  (interactive "fProject file:")
  (ada-parse-prj-file file)
  (ada-select-prj-file file))

;; ------ Handling the project file -----------------------------

(defun ada-prj-find-prj-file (&optional no-user-question)
  "Find the project file associated with the current buffer.
If the buffer is not in Ada mode, or not associated with a file,
return `ada-prj-default-project-file'.  Otherwise, search for a file with
the same base name as the Ada file, but extension given by
`ada-prj-file-extension' (default .adp).  If not found, search for *.adp
in the current directory; if several are found, and NO-USER-QUESTION
is non-nil, prompt the user to select one.  If none are found, return
'default.adp'."

  (let (selected)

    (if (not (and (derived-mode-p 'ada-mode)
		  buffer-file-name))

	;;  Not in an Ada buffer, or current buffer not associated
	;;  with a file (for instance an emerge buffer)
	(setq selected nil)

      ;;  other cases: use a more complex algorithm

      (let* ((current-file (buffer-file-name))
	     (first-choice (concat
			    (file-name-sans-extension current-file)
			    ada-prj-file-extension))
	     (dir          (file-name-directory current-file))

	     (prj-files    (directory-files
			    dir t
			    (concat ".*" (regexp-quote
					  ada-prj-file-extension) "$")))
	     (choice       nil))

	(cond

	 ((file-exists-p first-choice)
	  ;; filename.adp
	  (set 'selected first-choice))

	 ((= (length prj-files) 1)
	  ;; Exactly one project file was found in the current directory
	  (set 'selected (car prj-files)))

	 ((and (> (length prj-files) 1) (not no-user-question))
	  ;;  multiple project files in current directory, ask the user
	  (save-window-excursion
	    (with-output-to-temp-buffer "*choice list*"
	      (princ "There are more than one possible project file.\n")
	      (princ "Which one should we use ?\n\n")
	      (princ "  no.   file name  \n")
	      (princ "  ---   ------------------------\n")
	      (let ((counter 1))
		(while (<= counter (length prj-files))
		  (princ (format "  %2d)    %s\n"
				 counter
				 (nth (1- counter) prj-files)))
		  (setq counter (1+ counter))

		  ))) ; end of with-output-to ...
	    (setq choice nil)
	    (while (or
		    (not choice)
		    (not (integerp choice))
		    (< choice 1)
		    (> choice (length prj-files)))
	      (setq choice (string-to-number
			    (read-from-minibuffer "Enter No. of your choice: "))))
	    (set 'selected (nth (1- choice) prj-files))))

	 ((= (length prj-files) 0)
	  ;; No project file in the current directory; ask user
	  (unless (or no-user-question (not ada-always-ask-project))
	    (setq ada-last-prj-file
		  (read-file-name
		   (concat "project file [" ada-last-prj-file "]:")
		   nil ada-last-prj-file))
	    (unless (string= ada-last-prj-file "")
	      (set 'selected ada-last-prj-file))))
	 )))

    (or selected "default.adp")
    ))

(defun ada-default-prj-properties ()
  "Return the default project properties list with the current buffer as main."

  (let ((file (buffer-file-name nil)))
    (list
     ;; variable name alphabetical order
     'ada_project_path (or (getenv "ADA_PROJECT_PATH") "")
     'ada_project_path_sep  ada-prj-ada-project-path-sep
     'bind_opt        ada-prj-default-bind-opt
     'build_dir       default-directory
     'casing          (if (listp ada-case-exception-file)
			  ada-case-exception-file
			(list ada-case-exception-file))
     'check_cmd       (list ada-prj-default-check-cmd) ;; FIXME: should not a list
     'comp_cmd        (list ada-prj-default-comp-cmd) ;; FIXME: should not a list
     'comp_opt        ada-prj-default-comp-opt
     'cross_prefix    ""
     'debug_cmd       (concat ada-prj-default-debugger
			      " ${main}" (if ada-on-ms-windows ".exe")) ;; FIXME: don't need .exe?
     'debug_post_cmd  (list nil)
     'debug_pre_cmd   (list (concat ada-cd-command " ${build_dir}"))
     'gnatmake_opt    ada-prj-default-gnatmake-opt
     'gnatfind_opt    ada-prj-gnatfind-switches
     'gpr_file        ada-prj-default-gpr-file
     'link_opt        ada-prj-default-link-opt
     'main            (if file
			  (file-name-nondirectory
			   (file-name-sans-extension file))
			"")
     'make_cmd        (list ada-prj-default-make-cmd) ;; FIXME: should not a list
     'obj_dir         (list ".")
     'remote_machine  ""
     'run_cmd         (list (concat "./${main}" (if ada-on-ms-windows ".exe")))
     ;; FIXME: should not a list
     ;; FIXME: don't need .exe?
     'src_dir         (list ".")
     )))

(defun ada-parse-prj-file (prj-file)
  "Read PRJ-FILE, set project properties in `ada-xref-project-files'."
  (let ((project (ada-default-prj-properties)))

    (setq prj-file (expand-file-name prj-file))
    (if (string= (file-name-extension prj-file) "gpr")
	(set 'project (ada-gnat-parse-gpr project prj-file))

      (set 'project (ada-parse-prj-file-1 prj-file project))
      )

    ;; Store the project properties
    (if (assoc prj-file ada-xref-project-files)
	(setcdr (assoc prj-file ada-xref-project-files) project)
      (add-to-list 'ada-xref-project-files (cons prj-file project)))

    (ada-xref-update-project-menu)
    ))

(defun ada-parse-prj-file-1 (prj-file project)
  "Parse the Ada mode project file PRJ-FILE, set project properties in PROJECT.
Return new value of PROJECT."
  (let ((ada-buffer (current-buffer))
	;; fields that are lists or otherwise require special processing
	ada_project_path casing comp_cmd check_cmd
	debug_pre_cmd debug_post_cmd gpr_file make_cmd obj_dir src_dir run_cmd)

    ;; Give users a chance to use compiler-specific project file formats
    (let ((buffer (run-hook-with-args-until-success
		   'ada-load-project-hook prj-file)))
      (unless buffer
	;; we load the project file with no warnings; if it does not
	;; exist, we stay in the Ada buffer; no project variable
	;; settings will be found. That works for the default
	;; "default.adp", which does not exist as a file.
	(setq buffer (find-file-noselect prj-file nil)))
      (set-buffer buffer))

    (widen)
    (goto-char (point-min))

    ;; process each line
    (while (not (eobp))

      ;; ignore lines that don't have the format "name=value", put
      ;; 'name', 'value' in match-string.
      (if (looking-at "^\\([^=\n]+\\)=\\(.*\\)")
	  (cond
	   ;; FIXME: strip trailing spaces
	   ;; variable name alphabetical order
	   ((string= (match-string 1) "ada_project_path")
	    (add-to-list 'ada_project_path
			 (expand-file-name
			  (substitute-in-file-name (match-string 2)))))

	   ((string= (match-string 1) "build_dir")
	    (set 'project
		 (plist-put project 'build_dir
			    (file-name-as-directory (match-string 2)))))

	   ((string= (match-string 1) "casing")
	    (add-to-list 'casing
			 (expand-file-name (substitute-in-file-name (match-string 2)))))

	   ((string= (match-string 1) "check_cmd")
	    (add-to-list 'check_cmd (match-string 2)))

	   ((string= (match-string 1) "comp_cmd")
	    (add-to-list 'comp_cmd (match-string 2)))

	   ((string= (match-string 1) "debug_post_cmd")
	    (add-to-list 'debug_post_cmd (match-string 2)))

	   ((string= (match-string 1) "debug_pre_cmd")
	    (add-to-list 'debug_pre_cmd (match-string 2)))

	   ((string= (match-string 1) "gpr_file")
	    ;; expand now; path is relative to Emacs project file
	    (setq gpr_file (expand-file-name (match-string 2))))

	   ((string= (match-string 1) "make_cmd")
	    (add-to-list 'make_cmd (match-string 2)))

	   ((string= (match-string 1) "obj_dir")
	    (add-to-list 'obj_dir
			 (file-name-as-directory
			  (expand-file-name (match-string 2)))))

	   ((string= (match-string 1) "run_cmd")
	    (add-to-list 'run_cmd (match-string 2)))

	   ((string= (match-string 1) "src_dir")
	    (add-to-list 'src_dir
			 (file-name-as-directory
			  (expand-file-name (match-string 2)))))

	   (t
	    ;; any other field in the file is just copied
	    (set 'project (plist-put project
				     (intern (match-string 1))
				     (match-string 2))))))

      (forward-line 1))

    ;; done reading file

    ;; back to the user buffer
    (set-buffer ada-buffer)

    ;; process accumulated lists
    (if ada_project_path
	(let ((sep (plist-get project 'ada_project_path_sep)))
	  (setq ada_project_path (reverse ada_project_path))
	  (setq ada_project_path (mapconcat 'identity ada_project_path sep))
	  (set 'project (plist-put project 'ada_project_path ada_project_path))
	  ;; env var needed now for ada-gnat-parse-gpr
	  (setenv "ADA_PROJECT_PATH" ada_project_path)))

    (if debug_post_cmd (set 'project (plist-put project 'debug_post_cmd (reverse debug_post_cmd))))
    (if debug_pre_cmd (set 'project (plist-put project 'debug_pre_cmd (reverse debug_pre_cmd))))
    (if casing (set 'project (plist-put project 'casing (reverse casing))))
    (if check_cmd (set 'project (plist-put project 'check_cmd (reverse check_cmd))))
    (if comp_cmd (set 'project (plist-put project 'comp_cmd (reverse comp_cmd))))
    (if make_cmd (set 'project (plist-put project 'make_cmd (reverse make_cmd))))
    (if run_cmd (set 'project (plist-put project 'run_cmd (reverse run_cmd))))

    (if gpr_file
	(progn
	  (set 'project (ada-gnat-parse-gpr project gpr_file))
	  ;; append Ada source and object directories to others from Emacs project file
	  (setq src_dir (append (plist-get project 'src_dir) src_dir))
	  (setq obj_dir (append (plist-get project 'obj_dir) obj_dir))
	  (setq ada-xref-runtime-library-specs-path '()
		ada-xref-runtime-library-ali-path   '()))
      )

    ;; FIXME: gnatpath.exe doesn't output the runtime libraries, so always call ada-initialize-runtime-library
    ;; if using a gpr_file, the runtime library directories are
    ;; included in src_dir and obj_dir; otherwise they are in the
    ;; 'runtime-library' variables.
    ;; FIXME: always append to src_dir, obj_dir
    (ada-initialize-runtime-library (or (ada-xref-get-project-field 'cross_prefix) ""))
    ;;)

    (if obj_dir (set 'project (plist-put project 'obj_dir (reverse obj_dir))))
    (if src_dir (set 'project (plist-put project 'src_dir (reverse src_dir))))

    project
    ))

(defun ada-select-prj-file (file)
  "Select FILE as the current project file."
  (interactive)
  (setq ada-prj-default-project-file (expand-file-name file))

  (let ((casing (ada-xref-get-project-field 'casing)))
    (if casing
	(progn
	  ;; FIXME: use ada-get-absolute-dir here
	  (setq ada-case-exception-file casing)
	  (ada-case-read-exceptions))))

  (let ((ada_project_path (ada-xref-get-project-field 'ada_project_path)))
    (if ada_project_path
	;; FIXME: use ada-get-absolute-dir, mapconcat here
	(setenv "ADA_PROJECT_PATH" ada_project_path)))

  (setq compilation-search-path (ada-xref-get-src-dir-field))

  (setq ada-search-directories-internal
	;; FIXME: why do we need directory-file-name here?
	(append (mapcar 'directory-file-name compilation-search-path)
		ada-search-directories))

  ;; return 't', for decent display in message buffer when called interactively
  t)

(defun ada-find-references (&optional pos arg local-only)
  "Find all references to the entity under POS.
Calls gnatfind to find the references.
If ARG is non-nil, the contents of the old *gnatfind* buffer is preserved.
If LOCAL-ONLY is non-nil, only declarations in the current file are returned."
  (interactive "d\nP")
  (ada-require-project-file)

  (let* ((identlist (ada-read-identifier pos))
	 (alifile (ada-get-ali-file-name (ada-file-of identlist)))
	 (process-environment (ada-set-environment)))

    (set-buffer (get-file-buffer (ada-file-of identlist)))

    ;;  if the file is more recent than the executable
    (if (or (buffer-modified-p (current-buffer))
	    (file-newer-than-file-p (ada-file-of identlist) alifile))
	(ada-find-any-references (ada-name-of identlist)
				 (ada-file-of identlist)
				 nil nil local-only arg)
      (ada-find-any-references (ada-name-of identlist)
			       (ada-file-of identlist)
			       (ada-line-of identlist)
			       (ada-column-of identlist) local-only arg)))
  )

(defun ada-find-local-references (&optional pos arg)
  "Find all references to the entity under POS.
Calls `gnatfind' to find the references.
If ARG is non-nil, the contents of the old *gnatfind* buffer is preserved."
  (interactive "d\nP")
  (ada-find-references pos arg t))

(defconst ada-gnatfind-buffer-name "*gnatfind*")

(defun ada-find-any-references
  (entity &optional file line column local-only append)
  "Search for references to any entity whose name is ENTITY.
ENTITY was first found the location given by FILE, LINE and COLUMN.
If LOCAL-ONLY is non-nil, then list only the references in FILE,
which is much faster.
If APPEND is non-nil, then append the output of the command to the
existing buffer `*gnatfind*', if there is one."
  (interactive "sEntity name: ")
  (ada-require-project-file)

  ;;  Prepare the gnatfind command.  Note that we must protect the quotes
  ;;  around operators, so that they are correctly handled and can be
  ;;  processed (gnatfind \"+\":...).
  (let* ((quote-entity
	  (if (= (aref entity 0) ?\")
	      (if ada-on-ms-windows
		  (concat "\\\"" (substring entity 1 -1) "\\\"")
		(concat "'\"" (substring entity 1 -1) "\"'"))
	    entity))
	 (switches (ada-xref-get-project-field 'gnatfind_opt))
	 ;; FIXME: use gpr_file
	 (cross-prefix (ada-xref-get-project-field 'cross_prefix))
	 (command (concat cross-prefix "gnat find " switches " "
			  quote-entity
			  (if file (concat ":" (file-name-nondirectory file)))
			  (if line (concat ":" line))
			  (if column (concat ":" column))
			  (if local-only (concat " " (file-name-nondirectory file)))
			  ))
	 old-contents)

    ;;  If a project file is defined, use it
    (if (and ada-prj-default-project-file
	     (not (string= ada-prj-default-project-file "")))
	(if (string-equal (file-name-extension ada-prj-default-project-file)
			  "gpr")
	    (setq command (concat command " -P\"" ada-prj-default-project-file "\""))
	  (setq command (concat command " -p\"" ada-prj-default-project-file "\""))))

    (if (and append (get-buffer ada-gnatfind-buffer-name))
	(with-current-buffer "*gnatfind*"
	  (setq old-contents (buffer-string))))

    (let ((compilation-error "reference"))
      (compilation-start command 'compilation-mode (lambda (_mode) ada-gnatfind-buffer-name)))

    ;;  Hide the "Compilation" menu
    (with-current-buffer ada-gnatfind-buffer-name
      (local-unset-key [menu-bar compilation-menu])

      (if old-contents
	  (progn
	    (goto-char 1)
	    (set 'buffer-read-only nil)
	    (insert old-contents)
	    (set 'buffer-read-only t)
	    (goto-char (point-max)))))
    )
  )

(defalias 'ada-change-prj (symbol-function 'ada-set-default-project-file))

;; ----- Identifier Completion --------------------------------------------
(defun ada-complete-identifier (pos)
  "Try to complete the identifier around POS, using compiler cross-reference information."
  (interactive "d")
  (ada-require-project-file)

  ;; Initialize function-local variables and jump to the .ali buffer
  ;; Note that for regexp search is case insensitive too
  (let* ((curbuf (current-buffer))
	 (identlist (ada-read-identifier pos))
	 (sofar (concat "^[0-9]+[a-zA-Z][0-9]+[ *]\\("
			(regexp-quote (ada-name-of identlist))
			"[a-zA-Z0-9_]*\\)"))
	 (completed nil)
	 (symalist nil))

    ;; Open the .ali file
    (set-buffer (ada-get-ali-buffer (buffer-file-name)))
    (goto-char (point-max))

    ;; build an alist of possible completions
    (while (re-search-backward sofar nil t)
      (setq symalist (cons (cons (match-string 1) nil) symalist)))

    (setq completed  (try-completion "" symalist))

    ;; kills .ali buffer
    (kill-buffer nil)

    ;; deletes the incomplete identifier in the buffer
    (set-buffer curbuf)
    (looking-at "[a-zA-Z0-9_]+")
    (replace-match "")
    ;; inserts the completed symbol
    (insert completed)
    ))

;; ----- Cross-referencing ----------------------------------------

(defun ada-point-and-xref ()
  "Jump to the declaration of the entity below the cursor."
  (interactive)
  (mouse-set-point last-input-event)
  (ada-goto-declaration (point)))

(defun ada-point-and-xref-body ()
  "Jump to the body of the entity under the cursor."
  (interactive)
  (mouse-set-point last-input-event)
  (ada-goto-body (point)))

(defun ada-goto-body (pos &optional other-frame)
  "Display the body of the entity around POS.
OTHER-FRAME non-nil means display in another frame.
If the entity doesn't have a body, display its declaration.
As a side effect, the buffer for the declaration is also open."
  (interactive "d")
  (ada-goto-declaration pos other-frame)

  ;;  Temporarily force the display in the same buffer, since we
  ;;  already changed previously
  (let ((ada-xref-other-buffer nil))
    (ada-goto-declaration (point) nil)))

(defun ada-goto-declaration (pos &optional other-frame)
  "Display the declaration of the identifier around POS.
The declaration is shown in another buffer if `ada-xref-other-buffer' is
non-nil.
If OTHER-FRAME is non-nil, display the cross-reference in another frame."
  (interactive "d")
  (ada-require-project-file)
  (push-mark pos)
  (ada-xref-push-pos (buffer-file-name) pos)

  ;;  First try the standard algorithm by looking into the .ali file, but if
  ;;  that file was too old or even did not exist, try to look in the whole
  ;;  object path for a possible location.
  (let ((identlist (ada-read-identifier pos)))
    (condition-case err
	(ada-find-in-ali identlist other-frame)
      ;; File not found: print explicit error message
      (error-file-not-found
       (message (concat (error-message-string err)
			(nthcdr 1 err))))

      (error
       (let ((ali-file (ada-get-ali-file-name (ada-file-of identlist))))

	 ;; If the ALI file was up-to-date, then we probably have a predefined
	 ;; entity, whose references are not given by GNAT
	 (if (and (file-exists-p ali-file)
		  (file-newer-than-file-p ali-file (ada-file-of identlist)))
	     (message "No cross-reference found -- may be a predefined entity.")

	   ;; Else, look in every ALI file, except if the user doesn't want that
	   (if ada-xref-search-with-egrep
	       (ada-find-in-src-path identlist other-frame)
	     (message "Cross-referencing information is not up-to-date; please recompile.")
	     )))))))

(defun ada-goto-declaration-other-frame (pos)
  "Display the declaration of the identifier around POS.
The declaration is shown in another frame if `ada-xref-other-buffer' is
non-nil."
  (interactive "d")
  (ada-goto-declaration pos t))

(defun ada-remote (command)
  "Return the remote version of COMMAND, or COMMAND if remote_machine is nil."
  (let ((machine (ada-xref-get-project-field 'remote_machine)))
    (if (or (not machine) (string= machine ""))
	command
      (format "%s %s '(%s)'"
	      remote-shell-program
	      machine
	      command))))

(defun ada-get-absolute-dir-list (dir-list root-dir)
  "Return the list of absolute directories found in DIR-LIST.
If a directory is a relative directory, ROOT-DIR is prepended.
Project and environment variables are substituted."
  (mapcar (lambda (x) (expand-file-name x (ada-treat-cmd-string root-dir))) dir-list))

(defun ada-set-environment ()
  "Prepare an environment for Ada compilation.
This returns a new value to use for `process-environment',
but does not actually put it into use.
It modifies the source path and object path with the values found in the
project file."
  (let ((include   (getenv "ADA_INCLUDE_PATH"))
	(objects   (getenv "ADA_OBJECTS_PATH"))
	(build-dir (ada-xref-get-project-field 'build_dir)))
    (if include
	(set 'include (concat path-separator include)))
    (if objects
	(set 'objects (concat path-separator objects)))
    (cons
     (concat "ADA_INCLUDE_PATH="
	     (mapconcat (lambda(x) (expand-file-name x build-dir))
			(ada-xref-get-project-field 'src_dir)
			path-separator)
	     include)
     (cons
      (concat "ADA_OBJECTS_PATH="
	      (mapconcat (lambda(x) (expand-file-name x build-dir))
			 (ada-xref-get-project-field 'obj_dir)
			 path-separator)
	      objects)
      process-environment))))

(defun ada-compile-application (&optional arg)
  "Compile the application, using the command found in the project file.
If ARG is not nil, ask for user confirmation."
  (interactive "P")
  (ada-require-project-file)
  (let ((cmd (ada-xref-get-project-field 'make_cmd))
	(process-environment (ada-set-environment))
	(compilation-scroll-output t))

    (setq compilation-search-path (ada-xref-get-src-dir-field))

    ;;  If no project file was found, ask the user
    (unless cmd
      (setq cmd '("") arg t))

    ;;  Make a single command from the list of commands, including the
    ;;  commands to run it on a remote machine.
    (setq cmd (ada-remote (mapconcat 'identity cmd ada-command-separator)))

    (if (or ada-xref-confirm-compile arg)
	(setq cmd (read-from-minibuffer "enter command to compile: " cmd)))

    ;;  Insert newlines so as to separate the name of the commands to run
    ;;  and the output of the commands.  This doesn't work with cmdproxy.exe,
    ;;  which gets confused by newline characters.
    (if (not (string-match ".exe" shell-file-name))
	(setq cmd (concat cmd "\n\n")))

    (compile (ada-quote-cmd cmd))))

(defun ada-set-main-compile-application ()
  "Set main project variable to current buffer, build main."
  (interactive)
  (ada-require-project-file)
  (let* ((file (buffer-file-name (current-buffer)))
	 main)
    (if (not file)
	(error "No file for current buffer")

      (setq main
	    (if file
		(file-name-nondirectory
		 (file-name-sans-extension file))
	      ""))
      (ada-xref-set-project-field 'main main)
      (ada-compile-application))))

(defun ada-compile-current (&optional arg prj-field)
  "Recompile the current file.
If ARG is non-nil, ask for user confirmation of the command.
PRJ-FIELD is the name of the field to use in the project file to get the
command, and should be either `comp_cmd' (default) or `check_cmd'."
  (interactive "P")
  (ada-require-project-file)
  (let* ((field (if prj-field prj-field 'comp_cmd))
	 (cmd (ada-xref-get-project-field field))
	 (process-environment (ada-set-environment))
	 (compilation-scroll-output t))

    (unless cmd
      (setq cmd '("") arg t))

    ;;  Make a single command from the list of commands, including the
    ;;  commands to run it on a remote machine.
    (setq cmd (ada-remote (mapconcat 'identity cmd ada-command-separator)))

    ;;  If no project file was found, ask the user
    (if (or ada-xref-confirm-compile arg)
	(setq cmd (read-from-minibuffer "enter command to compile: " cmd)))

    (compile (ada-quote-cmd cmd))))

(defun ada-check-current (&optional arg)
  "Check the current file for syntax errors.
If ARG is non-nil, ask for user confirmation of the command."
  (interactive "P")
  (ada-compile-current arg 'check_cmd))

(defun ada-run-application (&optional arg)
  "Run the application.
If ARG is non-nil, ask for user confirmation."
  (interactive)
  (ada-require-project-file)

  (let ((machine (ada-xref-get-project-field 'cross_prefix)))
    (if (and machine (not (string= machine "")))
      (error "This feature is not supported yet for cross environments")))

  (let ((command (ada-xref-get-project-field 'run_cmd)))

    ;;  Guess the command if it wasn't specified
    (if (not command)
	(set 'command (list (file-name-sans-extension (buffer-name)))))

    ;; Modify the command to run remotely
    (setq command (ada-remote (mapconcat 'identity command
					 ada-command-separator)))

    ;; Ask for the arguments to the command if required
    (if (or ada-xref-confirm-compile arg)
	(setq command (read-from-minibuffer "Enter command to execute: "
					    command)))

    ;; Run the command
    (with-current-buffer (get-buffer-create "*run*")
      (set 'buffer-read-only nil)

      (erase-buffer)
      (start-process "run" (current-buffer) shell-file-name
		     "-c" command)
      (comint-mode)
      ;;  Set these two variables to their default values, since otherwise
      ;;  the output buffer is scrolled so that only the last output line
      ;;  is visible at the top of the buffer.
      (set (make-local-variable 'scroll-step) 0)
      (set (make-local-variable 'scroll-conservatively) 0)
      )
    (display-buffer "*run*")

    ;;  change to buffer *run* for interactive programs
    (other-window 1)
    (switch-to-buffer "*run*")
    ))

(defun ada-gdb-application (&optional arg executable-name)
  "Start the debugger on the application.
If ARG is non-nil, ask the user to confirm the command.
EXECUTABLE-NAME, if non-nil, is debugged instead of the file specified in the
project file."
  (interactive "P")
  (ada-require-project-file)
  (let ((buffer (current-buffer))
	cmd pre-cmd post-cmd)
    (setq cmd   (if executable-name
		    (concat ada-prj-default-debugger " " executable-name)
		  (ada-xref-get-project-field 'debug_cmd))
	  pre-cmd  (ada-xref-get-project-field 'debug_pre_cmd)
	  post-cmd (ada-xref-get-project-field 'debug_post_cmd))

    ;;  If the command was not given in the project file, start a bare gdb
    (if (not cmd)
	(set 'cmd (concat ada-prj-default-debugger
			  " "
			  (or executable-name
			      (file-name-sans-extension (buffer-file-name))))))

    ;;  For gvd, add an extra switch so that the Emacs window is completely
    ;;  swallowed inside the Gvd one
    (if (and ada-tight-gvd-integration
	     (string-match "^[^ \t]*gvd" cmd))
	;;  Start a new frame, so that when gvd exists we do not kill Emacs
	;;  We make sure that gvd swallows the new frame, not the one the
	;;  user has been using until now
	;;  The frame is made invisible initially, so that GtkPlug gets a
	;;  chance to fully manage it.  Then it works fine with Enlightenment
	;;  as well
	(let ((frame (make-frame '((visibility . nil)))))
	  (set 'cmd (concat
		     cmd " --editor-window="
		     (cdr (assoc 'outer-window-id (frame-parameters frame)))))
	  (select-frame frame)))

    ;;  Add a -fullname switch
    ;;  Use the remote machine
    (set 'cmd (ada-remote (concat cmd " -fullname ")))

    ;;  Ask for confirmation if required
    (if (or arg ada-xref-confirm-compile)
	(set 'cmd (read-from-minibuffer "enter command to debug: " cmd)))

    (let ((old-comint-exec (symbol-function 'comint-exec)))

      ;;  Do not add -fullname, since we can have a 'rsh' command in front.
      ;;  FIXME: This is evil but luckily a nop under Emacs-21.3.50 !  -stef
      (fset 'gud-gdb-massage-args (lambda (_file args) args))

      (set 'pre-cmd  (mapconcat 'identity pre-cmd  ada-command-separator))
      (if (not (equal pre-cmd ""))
	  (setq pre-cmd (concat pre-cmd ada-command-separator)))

      (set 'post-cmd (mapconcat 'identity post-cmd "\n"))
      (if post-cmd
	  (set 'post-cmd (concat post-cmd "\n")))


      ;;  Temporarily replaces the definition of `comint-exec' so that we
      ;;  can execute commands before running gdb.
      ;;  FIXME: This is evil and not temporary !!!  -stef
      (fset 'comint-exec
	    `(lambda (buffer name command startfile switches)
	       (let (compilation-buffer-name-function)
		 (save-excursion
		   (set 'compilation-buffer-name-function
			(lambda(x) (buffer-name buffer)))
		   (compile (ada-quote-cmd
			     (concat ,pre-cmd
				     command " "
				     (mapconcat 'identity switches " "))))))
	       ))

      ;;  Tight integration should force the tty mode
      (if (and (string-match "gvd" (comint-arguments cmd 0 0))
	       ada-tight-gvd-integration
	       (not (string-match "--tty" cmd)))
	  (setq cmd (concat cmd "--tty")))

      (if (and (string-match "jdb" (comint-arguments cmd 0 0))
	       (boundp 'jdb))
	  (funcall (symbol-function 'jdb) cmd)
	(gdb cmd))

      ;;  Restore the standard fset command (or for instance C-U M-x shell
      ;;  wouldn't work anymore

      (fset 'comint-exec old-comint-exec)

      ;;  Send post-commands to the debugger
      (process-send-string (get-buffer-process (current-buffer)) post-cmd)

      ;;  Move to the end of the debugger buffer, so that it is automatically
      ;;  scrolled from then on.
      (goto-char (point-max))

      ;;  Display both the source window and the debugger window (the former
      ;;  above the latter).  No need to show the debugger window unless it
      ;;  is going to have some relevant information.
      (if (or (not (string-match "gvd" (comint-arguments cmd 0 0)))
	      (string-match "--tty" cmd))
	  (split-window-below))
      (switch-to-buffer buffer)
      )))

(defun ada-reread-prj-file (&optional filename)
  "Reread either the current project, or FILENAME if non-nil.
If FILENAME is non-nil, set it as current project."
  (interactive "P")
  (if (not filename)
    (setq filename ada-prj-default-project-file))
  (ada-parse-prj-file filename)
  (ada-select-prj-file filename))

;; ------ Private routines

(defun ada-xref-current (file &optional ali-file-name)
  "Update the cross-references for FILE.
This in fact recompiles FILE to create ALI-FILE-NAME.
This function returns the name of the file that was recompiled to generate
the cross-reference information.  Note that the ali file can then be deduced
by replacing the file extension with `.ali'."
  ;; kill old buffer
  (if (and ali-file-name
	   (get-file-buffer ali-file-name))
      (kill-buffer (get-file-buffer ali-file-name)))

  (let* ((name      (ada-convert-file-name file))
	 (body-name (or (ada-get-body-name name) name)))

    ;; Always recompile the body when we can.  We thus temporarily switch to a
    ;; buffer than contains the body of the unit
    (save-excursion
      (let ((body-visible (find-buffer-visiting body-name))
	    process)
	(if body-visible
	    (set-buffer body-visible)
	  (find-file body-name))

	;; Execute the compilation.  Note that we must wait for the end of the
	;; process, or the ALI file would still not be available.
	;; Unfortunately, the underlying `compile' command that we use is
	;; asynchronous.
	(ada-compile-current)
	(setq process (get-buffer-process "*compilation*"))

	(while (and process
		    (not (equal (process-status process) 'exit)))
	  (sit-for 1))

	;; remove the buffer for the body if it wasn't there before
	(unless body-visible
	  (kill-buffer (find-buffer-visiting body-name)))
	))
    body-name))

(defun ada-find-file-in-dir (file dir-list)
  "Search for FILE in DIR-LIST."
  (let (found)
    (while (and (not found) dir-list)
      (set 'found (concat (file-name-as-directory (car dir-list))
			  (file-name-nondirectory file)))

      (unless (file-exists-p found)
	  (set 'found nil))
      (set 'dir-list (cdr dir-list)))
    found))

(defun ada-find-ali-file-in-dir (file)
  "Find the ali file FILE, searching obj_dir for the current project.
Adds build_dir in front of the search path to conform to gnatmake's behavior,
and the standard runtime location at the end."
  (ada-find-file-in-dir file (ada-xref-get-obj-dir-field)))

(defun ada-find-src-file-in-dir (file)
  "Find the source file FILE, searching src_dir for the current project.
Adds the standard runtime location at the end of the search path to conform
to gnatmake's behavior."
  (ada-find-file-in-dir file (ada-xref-get-src-dir-field)))

(defun ada-get-ali-file-name (file)
  "Create the ali file name for the Ada file FILE.
The file is searched for in every directory shown in the obj_dir lines of
the project file."

  ;; This function has to handle the special case of non-standard
  ;; file names (i.e. not .adb or .ads)
  ;; The trick is the following:
  ;;   1- replace the extension of the current file with .ali,
  ;;      and look for this file
  ;;   2- If this file is found:
  ;;      grep the "^U" lines, and make sure we are not reading the
  ;;      .ali file for a spec file.  If we are, go to step 3.
  ;;   3- If the file is not found or step 2 failed:
  ;;      find the name of the "other file", ie the body, and look
  ;;      for its associated .ali file by substituting the extension
  ;;
  ;; We must also handle the case of separate packages and subprograms:
  ;;   4- If no ali file was found, we try to modify the file name by removing
  ;;      everything after the last '-' or '.' character, so as to get the
  ;;      ali file for the parent unit.  If we found an ali file, we check that
  ;;      it indeed contains the definition for the separate entity by checking
  ;;      the 'D' lines.  This is done repeatedly, in case the direct parent is
  ;;      also a separate.

  (with-current-buffer (get-file-buffer file)
    (let ((short-ali-file-name
	   (concat (file-name-sans-extension (file-name-nondirectory file))
		   ".ali"))
	  ali-file-name
	  is-spec)

      ;; If we have a non-standard file name, and this is a spec, we first
      ;; look for the .ali file of the body, since this is the one that
      ;; contains the most complete information.  If not found, we will do what
      ;; we can with the .ali file for the spec...

      (if (not (string= (file-name-extension file) "ads"))
	  (let ((specs ada-spec-suffixes))
	    (while specs
	      (if (string-match (concat (regexp-quote (car specs)) "$")
				file)
		  (set 'is-spec t))
	      (set 'specs (cdr specs)))))

      (if is-spec
	  (set 'ali-file-name
	       (ada-find-ali-file-in-dir
		(concat (file-name-sans-extension
			 (file-name-nondirectory
			  (ada-other-file-name)))
			".ali"))))


      (setq ali-file-name
	    (or ali-file-name

		;;  Else we take the .ali file associated with the unit
		(ada-find-ali-file-in-dir short-ali-file-name)


		;;  else we did not find the .ali file Second chance: in case
		;;  the files do not have standard names (such as for instance
		;;  file_s.ada and file_b.ada), try to go to the other file
		;;  and look for its ali file
		(ada-find-ali-file-in-dir
		 (concat (file-name-sans-extension
			  (file-name-nondirectory (ada-other-file-name)))
			 ".ali"))


		;;  If we still don't have an ali file, try to get the one
		;;  from the parent unit, in case we have a separate entity.
		(let ((parent-name (file-name-sans-extension
				    (file-name-nondirectory file))))

		  (while (and (not ali-file-name)
			      (string-match "^\\(.*\\)[.-][^.-]*" parent-name))

		    (set 'parent-name (match-string 1 parent-name))
		    (set 'ali-file-name (ada-find-ali-file-in-dir
					 (concat parent-name ".ali")))
		    )
		  ali-file-name)))

      ;; If still not found, try to recompile the file
      (if (not ali-file-name)
	  ;; Recompile only if the user asked for this, and search the ali
	  ;; filename again.  We avoid a possible infinite recursion by
	  ;; temporarily disabling the automatic compilation.

	  (if ada-xref-create-ali
	      (setq ali-file-name
		    (concat (file-name-sans-extension (ada-xref-current file))
			    ".ali"))

	    (error "`.ali' file not found; recompile your source file"))


	;; same if the .ali file is too old and we must recompile it
	(if (and (file-newer-than-file-p file ali-file-name)
		 ada-xref-create-ali)
	    (ada-xref-current file ali-file-name)))

      ;;  Always return the correct absolute file name
      (expand-file-name ali-file-name))
      ))

(defun ada-get-ada-file-name (file original-file)
  "Create the complete file name (+directory) for FILE.
The original file (where the user was) is ORIGINAL-FILE.
Search in project file for possible paths."

  (save-excursion

    ;; If the buffer for original-file, use it to get the values from the
    ;; project file, otherwise load the file and its project file
    (let ((buffer (get-file-buffer original-file)))
      (if buffer
	  (set-buffer buffer)
	(find-file original-file)))

    ;; we choose the first possible completion and we
    ;; return the absolute file name
    (let ((filename (ada-find-src-file-in-dir file)))
      (if filename
	  (expand-file-name filename)
	(signal 'error-file-not-found (file-name-nondirectory file)))
      )))

(defun ada-find-file-number-in-ali (file)
  "Return the file number for FILE in the associated ali file."
  (set-buffer (ada-get-ali-buffer file))
  (goto-char (point-min))

  (let ((begin (re-search-forward "^D")))
    (beginning-of-line)
    (re-search-forward (concat "^D " (file-name-nondirectory file)))
    (count-lines begin (point))))

(defun ada-read-identifier (pos)
  "Return the identlist around POS and switch to the .ali buffer.
The returned list represents the entity, and can be manipulated through the
macros `ada-name-of', `ada-line-of', `ada-column-of', `ada-file-of',..."

  ;; If at end of buffer (e.g the buffer is empty), error
  (if (>= (point) (point-max))
      (error "No identifier on point"))

  ;; goto first character of the identifier/operator (skip backward < and >
  ;; since they are part of multiple character operators
  (goto-char pos)
  (skip-chars-backward "a-zA-Z0-9_<>")

  ;; check if it really is an identifier
  (if (ada-in-comment-p)
      (error "Inside comment"))

  (let (identifier identlist)
    ;; Just in front of a string => we could have an operator declaration,
    ;; as in "+", "-", ..
    (if (= (char-after) ?\")
	(forward-char 1))

    ;; if looking at an operator
    ;; This is only true if:
    ;;   - the symbol is +, -, ...
    ;;   - the symbol is made of letters, and not followed by _ or a letter
    (if (and (looking-at ada-operator-re)
	     (or (not (= (char-syntax (char-after)) ?w))
		 (not (or (= (char-syntax (char-after (match-end 0))) ?w)
			  (= (char-after (match-end 0)) ?_)))))
	(progn
	  (if (and (= (char-before) ?\")
		   (= (char-after (+ (length (match-string 0)) (point))) ?\"))
	      (forward-char -1))
	  (set 'identifier (regexp-quote (concat "\"" (match-string 0) "\""))))

      (if (ada-in-string-p)
	  (error "Inside string or character constant"))
      (if (looking-at (concat ada-keywords "[^a-zA-Z_]"))
	  (error "No cross-reference available for reserved keyword"))
      (if (looking-at "[a-zA-Z0-9_]+")
	  (set 'identifier (match-string 0))
	(error "No identifier around")))

    ;; Build the identlist
    (set 'identlist    (ada-make-identlist))
    (ada-set-name      identlist (downcase identifier))
    (ada-set-line      identlist
		       (number-to-string (count-lines 1 (point))))
    (ada-set-column    identlist
		       (number-to-string (1+ (current-column))))
    (ada-set-file      identlist (buffer-file-name))
    identlist
    ))

(defun ada-get-all-references (identlist)
  "Complete IDENTLIST with definition file and places where it is referenced.
Information is extracted from the ali file."

  (let ((ali-buffer (ada-get-ali-buffer (ada-file-of identlist)))
	declaration-found)
    (set-buffer ali-buffer)
    (goto-char (point-min))
    (ada-set-on-declaration identlist nil)

    ;; First attempt: we might already be on the declaration of the identifier
    ;; We want to look for the declaration only in a definite interval (after
    ;; the "^X ..." line for the current file, and before the next "^X" line

    (if (re-search-forward
	 (concat "^X [0-9]+ " (file-name-nondirectory (ada-file-of identlist)))
	 nil t)
	(let ((bound (save-excursion (re-search-forward "^X " nil t))))
	  (set 'declaration-found
	       (re-search-forward
		(concat "^"    (ada-line-of identlist)
			"."    (ada-column-of identlist)
			"[ *]" (ada-name-of identlist)
			"[{\[\(<= ]?\\(.*\\)$") bound t))
	  (if declaration-found
	      (ada-set-on-declaration identlist t))
	  ))

    ;; If declaration is still nil, then we were not on a declaration, and
    ;; have to fall back on other algorithms

    (unless declaration-found

      ;; Since we already know the number of the file, search for a direct
      ;; reference to it
      (goto-char (point-min))
      (set 'declaration-found t)
      (ada-set-ali-index
       identlist
       (number-to-string (ada-find-file-number-in-ali
			  (ada-file-of identlist))))
      (unless (re-search-forward (concat (ada-ali-index-of identlist)
					 "|\\([0-9]+[^0-9][0-9]+\\(\n\\.\\)? \\)*"
					 (ada-line-of identlist)
					 "[^etpzkd<>=^]"
					 (ada-column-of identlist) "\\>")
				 nil t)

	  ;; if we did not find it, it may be because the first reference
	  ;; is not required to have a 'unit_number|' item included.
	  ;; Or maybe we are already on the declaration...
	  (unless (re-search-forward
		   (concat
		    "^[0-9]+.[0-9]+[ *]"
		    (ada-name-of identlist)
		    "[ <{=\(\[]\\(.\\|\n\\.\\)*\\<"
		    (ada-line-of identlist)
		    "[^0-9]"
		    (ada-column-of identlist) "\\>")
		   nil t)

	    ;; If still not found, then either the declaration is unknown
	    ;; or the source file has been modified since the ali file was
	    ;; created
	    (set 'declaration-found nil)
	    )
	  )

      ;; Last check to be completely sure we have found the correct line (the
      ;; ali might not be up to date for instance)
      (if declaration-found
	  (progn
	    (beginning-of-line)
	    ;; while we have a continuation line, go up one line
	    (while (looking-at "^\\.")
	      (forward-line -1)
	      (beginning-of-line))
	    (unless (looking-at (concat "[0-9]+.[0-9]+[ *]"
					(ada-name-of identlist) "[ <{=\(\[]"))
	      (set 'declaration-found nil))))

      ;; Still no success ! The ali file must be too old, and we need to
      ;; use a basic algorithm based on guesses.  Note that this only happens
      ;; if the user does not want us to automatically recompile files
      ;; automatically
      (unless declaration-found
	(if (ada-xref-find-in-modified-ali identlist)
	    (set 'declaration-found t)
	  ;; No more idea to find the declaration.  Give up
	  (progn
	    (kill-buffer ali-buffer)

	    (error "No declaration of %s found" (ada-name-of identlist))
	    )))
      )


    ;; Now that we have found a suitable line in the .ali file, get the
    ;; information available
    (beginning-of-line)
    (if declaration-found
	(let ((current-line (buffer-substring
			     (point) (point-at-eol))))
	  (save-excursion
	    (forward-line 1)
	    (beginning-of-line)
	    (while (looking-at "^\\.\\(.*\\)")
	      (set 'current-line (concat current-line (match-string 1)))
	      (forward-line 1))
	    )

	  (if (re-search-backward "^X [0-9]+ \\([a-zA-Z0-9_.-]+\\)" nil t)

	      ;;  If we can find the file
	      (condition-case err
		  (ada-set-declare-file
		   identlist
		   (ada-get-ada-file-name (match-string 1)
					  (ada-file-of identlist)))

		;;  Else clean up the ali file
		(error-file-not-found
		 (signal (car err) (cdr err)))
		(error
		 (kill-buffer ali-buffer)
		 (error (error-message-string err)))
		))

	  (ada-set-references   identlist current-line)
	  ))
  ))

(defun ada-xref-find-in-modified-ali (identlist)
  "Find the matching position for IDENTLIST in the current ali buffer.
This function is only called when the file was not up-to-date, so we need
to make some guesses.
This function is disabled for operators, and only works for identifiers."

  (unless (= (string-to-char (ada-name-of identlist)) ?\")
      (progn
	(let ((declist '()) ;;; ( (line_in_ali_file line_in_ada) ( ... ))
	      (my-regexp  (concat "[ *]"
				  (regexp-quote (ada-name-of identlist)) " "))
	      (line-ada "--")
	      (col-ada  "--")
	      (line-ali 0)
	      (len 0)
	      (choice 0)
	      (ali-buffer (current-buffer)))

	  (goto-char (point-max))
	  (while (re-search-backward my-regexp nil t)
	    (save-excursion
	      (set 'line-ali (count-lines 1 (point)))
	      (beginning-of-line)
	      ;; have a look at the line and column numbers
	      (if (looking-at "^\\([0-9]+\\).\\([0-9]+\\)[ *]")
		  (progn
		    (setq line-ada (match-string 1))
		    (setq col-ada  (match-string 2)))
		(setq line-ada "--")
		(setq col-ada  "--")
		)
	      ;; construct a list with the file names and the positions within
	      (if (re-search-backward "^X [0-9]+ \\([a-zA-Z0-9._-]+\\)" nil t)
		  (add-to-list
		   'declist (list line-ali (match-string 1) line-ada col-ada))
		)
	      )
	    )

	  ;; how many possible declarations have we found ?
	  (setq len (length declist))
	  (cond
	   ;; none => error
	   ((= len 0)
	    (kill-buffer (current-buffer))
	    (error "No declaration of %s recorded in .ali file"
		   (ada-name-of identlist)))
	   ;; one => should be the right one
	   ((= len 1)
	    (goto-char (point-min))
	    (forward-line (1- (caar declist))))

	   ;; more than one => display choice list
	   (t
	    (save-window-excursion
	      (with-output-to-temp-buffer "*choice list*"

		(princ "Identifier is overloaded and Xref information is not up to date.\n")
		(princ "Possible declarations are:\n\n")
		(princ "  no.   in file                at line  col\n")
		(princ "  ---   ---------------------     ----  ----\n")
		(let ((counter 0))
		  (while (< counter len)
		    (princ (format "  %2d)    %-21s   %4s  %4s\n"
				 (1+ counter)
				 (ada-get-ada-file-name
				  (nth 1 (nth counter declist))
				  (ada-file-of identlist))
				 (nth 2 (nth counter declist))
				 (nth 3 (nth counter declist))
				 ))
		    (setq counter (1+ counter))
		    ) ; end of while
		  ) ; end of let
		) ; end of with-output-to ...
	      (setq choice nil)
	      (while (or
		      (not choice)
		      (not (integerp choice))
		      (< choice 1)
		      (> choice len))
		(setq choice
		      (string-to-number
		       (read-from-minibuffer "Enter No. of your choice: "))))
	      )
	    (set-buffer ali-buffer)
	    (goto-char (point-min))
	    (forward-line (1- (car (nth (1- choice) declist))))
	    ))))))


(defun ada-find-in-ali (identlist &optional other-frame)
  "Look in the .ali file for the definition of the identifier in IDENTLIST.
If OTHER-FRAME is non-nil, and `ada-xref-other-buffer' is non-nil,
opens a new window to show the declaration."

  (ada-get-all-references identlist)
  (let ((ali-line (ada-references-of identlist))
	(locations nil)
	(start 0)
	file  line  col)

    ;; Note: in some cases, an entity can have multiple references to the
    ;; bodies (this is for instance the case for a separate subprogram, that
    ;; has a reference both to the stub and to the real body).
    ;; In that case, we simply go to each one in turn.

    ;; Get all the possible locations
    (string-match "^\\([0-9]+\\)[a-zA-Z+*]\\([0-9]+\\)[ *]" ali-line)
    (set 'locations (list (list (match-string 1 ali-line) ;; line
				(match-string 2 ali-line) ;; column
				(ada-declare-file-of identlist))))
    (while (string-match "\\([0-9]+\\)[bc]\\(<[^>]+>\\)?\\([0-9]+\\)"
			 ali-line start)
      (setq line  (match-string 1 ali-line)
	    col   (match-string 3 ali-line)
	    start (match-end 3))

      ;;  it there was a file number in the same line
      ;;  Make sure we correctly handle the case where the first file reference
      ;;  on the line is the type reference.
      ;;    1U2 T(2|2r3) 34r23
      (if (string-match (concat "[^{(<0-9]\\([0-9]+\\)|\\([^|bc]+\\)?"
				(match-string 0 ali-line))
			ali-line)
	  (let ((file-number (match-string 1 ali-line)))
	    (goto-char (point-min))
	    (re-search-forward "^D \\([a-zA-Z0-9_.-]+\\)" nil t
			       (string-to-number file-number))
	    (set 'file (match-string 1))
	    )
	;; Else get the nearest file
	(set 'file (ada-declare-file-of identlist)))

      (set 'locations (append locations (list (list line col file)))))

    ;; Add the specs at the end again, so that from the last body we go to
    ;; the specs
    (set 'locations (append locations (list (car locations))))

    ;; Find the new location we want to go to.
    ;; If we are on none of the locations listed, we simply go to the specs.

    (setq line (caar locations)
	  col  (nth 1 (car locations))
	  file (nth 2 (car locations)))

    (while locations
      (if (and (string= (caar locations) (ada-line-of identlist))
	       (string= (nth 1 (car locations)) (ada-column-of identlist))
	       (string= (file-name-nondirectory (nth 2 (car locations)))
			(file-name-nondirectory (ada-file-of identlist))))
	  (setq locations (cadr locations)
		line      (car locations)
		col       (nth 1 locations)
		file      (nth 2 locations)
		locations nil)
	(set 'locations (cdr locations))))

    ;;  Find the file in the source path
    (set 'file (ada-get-ada-file-name file (ada-file-of identlist)))

    ;; Kill the .ali buffer
    (kill-buffer (current-buffer))

    ;; Now go to the buffer
    (ada-xref-change-buffer file
			    (string-to-number line)
			    (1- (string-to-number col))
			    identlist
			    other-frame)
    ))

(defun ada-find-in-src-path (identlist &optional other-frame)
  "More general function for cross-references.
This function should be used when the standard algorithm that parses the
.ali file has failed, either because that file was too old or even did not
exist.
This function attempts to find the possible declarations for the identifier
anywhere in the object path.
This command requires the external `egrep' program to be available.

This works well when one is using an external library and wants to find
the declaration and documentation of the subprograms one is using."
;; FIXME: what does this function do?
  (let (list
	(dirs (ada-xref-get-obj-dir-field))
	(regexp (concat "[ *]" (ada-name-of identlist)))
	line column
	choice
	file)

    ;;  Do the grep in all the directories.  We do multiple shell
    ;;  commands instead of one in case there is no .ali file in one
    ;;  of the directory and the shell stops because of that.

    (with-current-buffer (get-buffer-create "*grep*")
      (while dirs
	(insert (shell-command-to-string
		 (concat
		  "grep -E -i -h "
		  (shell-quote-argument (concat "^X|" regexp "( |$)"))
		  " "
		  (shell-quote-argument (file-name-as-directory (car dirs)))
		  "*.ali")))
	(set 'dirs (cdr dirs)))

      ;;  Now parse the output
      (set 'case-fold-search t)
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
	(save-excursion
	  (beginning-of-line)
	  (if (not (= (char-after) ?X))
	      (progn
		(looking-at "\\([0-9]+\\).\\([0-9]+\\)")
		(setq line   (match-string 1)
		      column (match-string 2))
		(re-search-backward "^X [0-9]+ \\(.*\\)$")
		(set 'file (list (match-string 1) line column))

		;;  There could be duplicate choices, because of the structure
		;;  of the .ali files
		(unless (member file list)
		  (set 'list (append list (list file))))))))

      ;;  Current buffer is still "*grep*"
      (kill-buffer "*grep*")
      )

    ;;  Now display the list of possible matches
    (cond

     ;;  No choice found => Error
     ((null list)
      (error "No cross-reference found, please recompile your file"))

     ;;  Only one choice => Do the cross-reference
     ((= (length list) 1)
      (set 'file (ada-find-src-file-in-dir (caar list)))
      (if file
	  (ada-xref-change-buffer file
				  (string-to-number (nth 1 (car list)))
				  (string-to-number (nth 2 (car list)))
				  identlist
				  other-frame)
	(error "%s not found in src_dir"  (caar list)))
      (message "This is only a (good) guess at the cross-reference.")
      )

     ;;  Else, ask the user
     (t
      (save-window-excursion
	(with-output-to-temp-buffer "*choice list*"

	  (princ "Identifier is overloaded and Xref information is not up to date.\n")
	  (princ "Possible declarations are:\n\n")
	  (princ "  no.   in file                at line  col\n")
	  (princ "  ---   ---------------------     ----  ----\n")
	  (let ((counter 0))
	    (while (< counter (length list))
	      (princ (format "  %2d)    %-21s   %4s  %4s\n"
			     (1+ counter)
			     (nth 0 (nth counter list))
			     (nth 1 (nth counter list))
			     (nth 2 (nth counter list))
			     ))
	      (setq counter (1+ counter))
	      )))
	(setq choice nil)
	(while (or (not choice)
		   (not (integerp choice))
		   (< choice 1)
		   (> choice (length list)))
	  (setq choice
		(string-to-number
		 (read-from-minibuffer "Enter No. of your choice: "))))
	)
      (set 'choice (1- choice))
      (kill-buffer "*choice list*")

      (set 'file (ada-find-src-file-in-dir (car (nth choice list))))
      (if file
	  (ada-xref-change-buffer file
				  (string-to-number (nth 1 (nth choice list)))
				  (string-to-number (nth 2 (nth choice list)))
				  identlist
				  other-frame)
	(signal 'error-file-not-found (car (nth choice list))))
      (message "This is only a (good) guess at the cross-reference.")
      ))))

(defun ada-xref-change-buffer
  (file line column identlist &optional other-frame)
  "Select and display FILE, at LINE and COLUMN.
If we do not end on the same identifier as IDENTLIST, find the
closest match.  Kills the .ali buffer at the end.
If OTHER-FRAME is non-nil, creates a new frame to show the file."

  (let (declaration-buffer)

    ;; Select and display the destination buffer
    (if ada-xref-other-buffer
	(if other-frame
	    (find-file-other-frame file)
	  (set 'declaration-buffer (find-file-noselect file))
	  (set-buffer declaration-buffer)
	  (switch-to-buffer-other-window declaration-buffer)
	  )
      (find-file file)
      )

    ;; move the cursor to the correct position
    (push-mark)
    (goto-char (point-min))
    (forward-line (1- line))
    (move-to-column column)

    ;; If we are not on the identifier, the ali file was not up-to-date.
    ;; Try to find the nearest position where the identifier is found,
    ;; this is probably the right one.
    (unless (looking-at (ada-name-of identlist))
      (ada-xref-search-nearest (ada-name-of identlist)))
    ))


(defun ada-xref-search-nearest (name)
  "Search for NAME nearest to the position recorded in the Xref file.
Return the position of the declaration in the buffer, or nil if not found."
  (let ((orgpos (point))
	(newpos nil)
	(diff nil))

    (goto-char (point-max))

    ;; loop - look for all declarations of name in this file
    (while (search-backward name nil t)

      ;; check if it really is a complete Ada identifier
      (if (and
	   (not (save-excursion
		  (goto-char (match-end 0))
		  (looking-at "_")))
	   (not (ada-in-string-or-comment-p))
	   (or
	    ;; variable declaration ?
	    (save-excursion
	      (skip-chars-forward "a-zA-Z_0-9" )
	      (ada-goto-next-non-ws)
	      (looking-at ":[^=]"))
	    ;; procedure, function, task or package declaration ?
	    (save-excursion
	      (ada-goto-previous-word)
	      (looking-at "\\<[pP][rR][oO][cC][eE][dD][uU][rR][eE]\\>\\|\\<[fF][uU][nN][cC][tT][iI][oO][nN]\\>\\|\\<[tT][yY][pP][eE]\\>\\|\\<[tT][aA][sS][kK]\\>\\|\\<[pP][aA][cC][kK][aA][gG][eE]\\>\\|\\<[bB][oO][dD][yY]\\>"))))

	  ;; check if it is nearer than the ones before if any
	  (if (or (not diff)
		  (< (abs (- (point) orgpos)) diff))
	      (progn
		(setq newpos (point)
		      diff (abs (- newpos orgpos))))))
      )

    (if newpos
	(progn
	  (message "ATTENTION: this declaration is only a (good) guess ...")
	  (goto-char newpos))
      nil)))


;; Find the parent library file of the current file
(defun ada-goto-parent ()
  "Go to the parent library file."
  (interactive)
  (ada-require-project-file)

  (let ((buffer (ada-get-ali-buffer (buffer-file-name)))
	(unit-name nil)
	(body-name nil)
	(ali-name nil))
    (with-current-buffer buffer
      (goto-char (point-min))
      (re-search-forward "^U \\([^ \t%]+\\)%[bs][ \t]+\\([^ \t]+\\)")
      (setq unit-name (match-string 1))
      (if (not (string-match "\\(.*\\)\\.[^.]+" unit-name))
	  (progn
	    (kill-buffer buffer)
	    (error "No parent unit !"))
	(setq unit-name (match-string 1 unit-name))
	)

      ;; look for the file name for the parent unit specification
      (goto-char (point-min))
      (re-search-forward (concat "^W " unit-name
				 "%s[ \t]+\\([^ \t]+\\)[ \t]+"
				 "\\([^ \t\n]+\\)"))
      (setq body-name (match-string 1))
      (setq ali-name (match-string 2))
      (kill-buffer buffer)
      )

    (setq ali-name (ada-find-ali-file-in-dir ali-name))

    (save-excursion
      ;; Tries to open the new ali file to find the spec file
      (if ali-name
	  (progn
	    (find-file ali-name)
	    (goto-char (point-min))
	    (re-search-forward (concat "^U " unit-name "%s[ \t]+"
				       "\\([^ \t]+\\)"))
	    (setq body-name (match-string 1))
	    (kill-buffer (current-buffer))
	    )
	)
      )

    (find-file body-name)
    ))

(defun ada-make-filename-from-adaname (adaname)
  "Determine the filename in which ADANAME is found.
This is a GNAT specific function that uses gnatkrunch."
  (let ((krunch-buf (generate-new-buffer "*gkrunch*"))
        (cross-prefix (plist-get (cdr (ada-xref-current-project)) 'cross_prefix)))
    (with-current-buffer krunch-buf
      ;; send adaname to external process `gnatkr'.
      ;; Add a dummy extension, since gnatkr versions have two different
      ;; behaviors depending on the version:
      ;;   Up to 3.15:   "AA.BB.CC"  =>  aa-bb-cc
      ;;   After:        "AA.BB.CC"  =>  aa-bb.cc
      (call-process (concat cross-prefix "gnatkr") nil krunch-buf nil
		    (concat adaname ".adb") ada-krunch-args)
      ;; fetch output of that process
      (setq adaname (buffer-substring
		     (point-min)
		     (progn
		       (goto-char (point-min))
		       (end-of-line)
		       (point))))
      ;;  Remove the extra extension we added above
      (setq adaname (substring adaname 0 -4))

      (kill-buffer krunch-buf)))
  adaname
  )

(defun ada-make-body-gnatstub (&optional interactive)
  "Create an Ada package body in the current buffer.
This function uses the `gnat stub' program to create the body.
This function typically is to be hooked into `ff-file-created-hook'.
If INTERACTIVE is nil, assume this is called from `ff-file-created-hook'."
  (interactive "p")
  (ada-require-project-file)

  ;; If not interactive, assume we are being called from
  ;; ff-file-created-hook. Then the current buffer is for the body
  ;; file, but we will create a new one after gnat stub runs
  (unless interactive
    (set-buffer-modified-p nil)
    (kill-buffer (current-buffer)))

  (save-some-buffers nil nil)

  ;; Make sure the current buffer is the spec, so gnat stub gets the
  ;; right package parameter (this might not be the case if for
  ;; instance the user was asked for a project file)

  (unless (buffer-file-name (car (buffer-list)))
    (set-buffer (cadr (buffer-list))))

  ;; Call the external process
  (let* ((project-plist (cdr (ada-xref-current-project)))
	 (gnatstub-opts (ada-treat-cmd-string ada-gnatstub-opts))
	 (gpr-file (plist-get project-plist 'gpr_file))
	 (filename      (buffer-file-name (car (buffer-list))))
	 (output        (concat (file-name-sans-extension filename) ".adb"))
	 (cross-prefix (plist-get project-plist 'cross_prefix))
	 (gnatstub-cmd  (concat cross-prefix "gnat stub"
				(if (not (string= gpr-file ""))
				    (concat " -P\"" gpr-file "\""))
				" " gnatstub-opts " " filename))
	 (buffer        (get-buffer-create "*gnat stub*")))

    (with-current-buffer buffer
      (compilation-minor-mode 1)
      (erase-buffer)
      (insert gnatstub-cmd)
      (newline)
      )

    (call-process shell-file-name nil buffer nil "-c" gnatstub-cmd)

    ;; clean up the output

    (if (file-exists-p output)
	(progn
	  (find-file output)
	  (kill-buffer buffer))

      ;; file not created; display the error message
      (display-buffer buffer))))

(defun ada-xref-initialize ()
  "Function called by `ada-mode-hook' to initialize the ada-xref.el package.
For instance, it creates the gnat-specific menus, sets some hooks for
`find-file'."
  (remove-hook 'ff-file-created-hook 'ada-make-body) ; from global hook
  (remove-hook 'ff-file-created-hook 'ada-make-body t) ; from local hook
  (add-hook 'ff-file-created-hook 'ada-make-body-gnatstub nil t)

  ;; Completion for file names in the mini buffer should ignore .ali files
  (add-to-list 'completion-ignored-extensions ".ali")

  (ada-xref-update-project-menu)
  )

;; ----- Add to ada-mode-hook ---------------------------------------------

;;  This must be done before initializing the Ada menu.
(add-hook 'ada-mode-hook 'ada-xref-initialize)

;;  Define a new error type
(put 'error-file-not-found
     'error-conditions
     '(error ada-mode-errors error-file-not-found))
(put 'error-file-not-found
     'error-message
     "File not found in src-dir (check project file): ")

(provide 'ada-xref)

;;; ada-xref.el ends here
