;;; ada-prj.el --- GUI editing of project files for the ada-mode

;; Copyright (C) 1998-2012 Free Software Foundation, Inc.

;; Author: Emmanuel Briot <briot@gnat.com>
;; Maintainer: Stephen Leake <stephen_leake@stephe-leake.org>
;; Keywords: languages, ada, project file
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

;;; This package provides a set of functions to easily edit the project
;;; files used by the ada-mode.
;;; The only function publicly available here is `ada-customize'.
;;; See the documentation of the Ada mode for more information on the project
;;; files.
;;; Internally, a project file is represented as a property list, with each
;;; field of the project file matching one property of the list.


;;; History:
;;

;;; Code:


;; ----- Requirements -----------------------------------------------------

(require 'cus-edit)
(require 'ada-xref)

(eval-when-compile
   (require 'ada-mode))

;; ----- Buffer local variables -------------------------------------------

(defvar ada-prj-current-values nil
  "Hold the current value of the fields, This is a property list.")
(make-variable-buffer-local 'ada-prj-current-values)

(defvar ada-prj-default-values nil
  "Hold the default value for the fields, This is a property list.")
(make-variable-buffer-local 'ada-prj-default-values)

(defvar ada-prj-ada-buffer nil
  "Indicates what Ada source file was being edited.")

(defvar ada-old-cross-prefix nil
  "The cross-prefix associated with the currently loaded runtime library.")


;; ----- Functions --------------------------------------------------------

(defun ada-prj-new ()
  "Open a new project file."
  (interactive)
  (let* ((prj
	  (if (and ada-prj-default-project-file
		   (not (string= ada-prj-default-project-file "")))
	      ada-prj-default-project-file
	    "default.adp"))
	 (filename (read-file-name "Project file: "
				   (if prj "" nil)
				   nil
				   nil
				   prj)))
    (if (not (string= (file-name-extension filename t) ".adp"))
	(error "File name extension for project files must be .adp"))

    (ada-customize nil filename)))

(defun ada-prj-edit ()
  "Editing the project file associated with the current Ada buffer.
If there is none, opens a new project file."
  (interactive)
  (if ada-prj-default-project-file
      (ada-customize)
    (ada-prj-new)))

(defun ada-prj-initialize-values (symbol _ada-buffer filename)
  "Set SYMBOL to the property list of the project file FILENAME.
If FILENAME is null, read the file associated with ADA-BUFFER.
If no project file is found, return the default values."
;; FIXME: rationalize arguments; make ada-buffer optional?
  (if (and filename
	   (not (string= filename ""))
	   (assoc filename ada-xref-project-files))
      (set symbol (copy-sequence (cdr (assoc filename ada-xref-project-files))))

    ;;  Set default values (except for the file name if this was given
    ;;  in the buffer
    (set symbol (ada-default-prj-properties))
    (if (and filename (not (string= filename "")))
	(set symbol (plist-put (eval symbol) 'filename filename)))
    ))


(defun ada-prj-save-specific-option (field)
  "Return the string to print in the project file to save FIELD.
If the current value of FIELD is the default value, return an empty string."
  (if (string= (plist-get ada-prj-current-values field)
	       (plist-get ada-prj-default-values field))
      ""
    (concat (symbol-name field)
	    "=" (plist-get ada-prj-current-values field) "\n")))

(defun ada-prj-save ()
  "Save the edited project file."
  (interactive)
  (let ((file-name (or (plist-get ada-prj-current-values 'filename)
		       (read-file-name "Save project as: ")))
	output)
    (set 'output
	 (concat

	  ;;  Save the fields that do not depend on the current buffer
	  ;;  only if they are different from the default value

	  (ada-prj-save-specific-option 'comp_opt)
	  (ada-prj-save-specific-option 'bind_opt)
	  (ada-prj-save-specific-option 'link_opt)
	  (ada-prj-save-specific-option 'gnatmake_opt)
	  (ada-prj-save-specific-option 'gnatfind_opt)
	  (ada-prj-save-specific-option 'cross_prefix)
	  (ada-prj-save-specific-option 'remote_machine)
	  (ada-prj-save-specific-option 'debug_cmd)

	  ;;  Always save the fields that depend on the current buffer
	  "main="      (plist-get ada-prj-current-values 'main) "\n"
	  "build_dir=" (plist-get ada-prj-current-values 'build_dir) "\n"
	  (ada-prj-set-list "check_cmd"
			    (plist-get ada-prj-current-values 'check_cmd)) "\n"
	  (ada-prj-set-list "make_cmd"
			    (plist-get ada-prj-current-values 'make_cmd)) "\n"
	  (ada-prj-set-list "comp_cmd"
			    (plist-get ada-prj-current-values 'comp_cmd)) "\n"
	  (ada-prj-set-list "run_cmd"
			    (plist-get ada-prj-current-values 'run_cmd)) "\n"
	  (ada-prj-set-list "src_dir"
			    (plist-get ada-prj-current-values 'src_dir)
			    t) "\n"
	  (ada-prj-set-list "obj_dir"
			    (plist-get ada-prj-current-values 'obj_dir)
			    t) "\n"
	  (ada-prj-set-list "debug_pre_cmd"
			    (plist-get ada-prj-current-values 'debug_pre_cmd))
	  "\n"
	  (ada-prj-set-list "debug_post_cmd"
			    (plist-get ada-prj-current-values 'debug_post_cmd))
	  "\n"
	  ))

    (find-file file-name)
    (erase-buffer)
    (insert output)
    (save-buffer)
    ;; kill the project buffer
    (kill-buffer nil)

    ;; kill the editor buffer
    (kill-buffer "*Edit Ada Mode Project*")

    ;; automatically set the new project file as the active one
    (set 'ada-prj-default-project-file file-name)

    ;; force Emacs to reread the project files
    (ada-reread-prj-file file-name)
    )
  )

(defun ada-prj-load-from-file (symbol)
  "Load SYMBOL value from file.
One item per line should be found in the file."
  (save-excursion
    (let ((file (read-file-name "File name: " nil nil t))
	  (buffer (current-buffer))
	  line
	  list)
      (find-file file)
      (widen)
      (goto-char (point-min))
      (while (not (eobp))
	(set 'line (buffer-substring-no-properties (point) (point-at-eol)))
	(add-to-list 'list line)
	(forward-line 1))
      (kill-buffer nil)
      (set-buffer buffer)
      (set 'ada-prj-current-values
	   (plist-put ada-prj-current-values
		      symbol
		      (append (plist-get ada-prj-current-values symbol)
			      (reverse list)))))
    (ada-prj-display-page 2)))

(defun ada-prj-subdirs-of (dir)
  "Return a list of all the subdirectories of DIR, recursively."
  (let ((subdirs (directory-files dir t "^[^.].*"))
	(dirlist (list dir)))
    (while subdirs
      (if (file-directory-p (car subdirs))
	  (let ((sub (ada-prj-subdirs-of (car subdirs))))
	    (if sub
		(set 'dirlist (append sub dirlist)))))
      (set 'subdirs (cdr subdirs)))
    dirlist))

(defun ada-prj-load-directory (field &optional file-name)
  "Append to FIELD in the current project the subdirectories of FILE-NAME.
If FILE-NAME is nil, ask the user for the name."

  ;;  Do not use an external dialog for this, since it wouldn't allow
  ;;  the user to select a directory
  (let ((use-dialog-box nil))
    (unless file-name
      (set 'file-name (read-directory-name "Root directory: " nil nil t))))

  (set 'ada-prj-current-values
       (plist-put ada-prj-current-values
		  field
		  (append (plist-get ada-prj-current-values field)
			  (reverse (ada-prj-subdirs-of
				    (expand-file-name file-name))))))
  (ada-prj-display-page 2))

(defun ada-prj-display-page (tab-num)
  "Display page TAB-NUM in the notebook.
The current buffer must be the project editing buffer."

  (let ((inhibit-read-only t))
    (erase-buffer))

  ;;  Widget support in Emacs 21 requires that we clear the buffer first
  (if (and (not (featurep 'xemacs)) (>= emacs-major-version 21))
      (progn
	(setq widget-field-new  nil
	      widget-field-list nil)
	(mapc (lambda (x) (delete-overlay x)) (car (overlay-lists)))
	(mapc (lambda (x) (delete-overlay x)) (cdr (overlay-lists)))))

  ;;  Display the tabs

  (widget-insert "\n               Project configuration.\n
  ___________    ____________    ____________    ____________    ____________\n / ")
  (widget-create 'push-button :notify
		 (lambda (&rest _dummy) (ada-prj-display-page 1)) "General")
  (widget-insert " \\  /   ")
  (widget-create 'push-button :notify
		 (lambda (&rest _dummy) (ada-prj-display-page 2)) "Paths")
  (widget-insert "  \\  / ")
  (widget-create 'push-button :notify
		 (lambda (&rest _dummy) (ada-prj-display-page 3)) "Switches")
  (widget-insert " \\  / ")
  (widget-create 'push-button :notify
		 (lambda (&rest _dummy) (ada-prj-display-page 4)) "Ada Menu")
  (widget-insert " \\  / ")
  (widget-create 'push-button :notify
		 (lambda (&rest _dummy) (ada-prj-display-page 5)) "Debugger")
  (widget-insert " \\\n")

  ;;  Display the currently selected page

  (cond

   ;;
   ;;  First page (General)
   ;;
   ((= tab-num 1)
    (widget-insert "/             \\/______________\\/______________\\/______________\\/______________\\\n")

    (widget-insert "Project file name:\n")
    (widget-insert (plist-get ada-prj-current-values 'filename))
    (widget-insert "\n\n")
    (ada-prj-field 'casing "Casing Exceptions"
"List of files that contain casing exception
dictionaries. All these files contain one
identifier per line, with a special casing.
The first file has the highest priority."
      t nil
      (mapconcat (lambda(x)
		   (concat "           " x))
		 (ada-xref-get-project-field 'casing)
		 "\n")
      )
    (ada-prj-field 'main "Executable file name"
"Name of the executable generated when you
compile your application. This should include
the full directory name, using ${build_dir} if
you wish.")
    (ada-prj-field 'build_dir  "Build directory"
		   "Reference directory for relative paths in
src_dir and obj_dir below. This is also the directory
where the compilation is done.")
    (ada-prj-field 'remote_machine "Name of the remote machine (if any)"
"If you want to remotely compile, debug and
run your application, specify the name of a
remote machine here. This capability requires
the 'rsh' protocol on the remote machine.")
    (ada-prj-field 'cross_prefix "Prefix used in for the cross tool chain"
"When working on multiple cross targets, it is
most convenient to specify the prefix of the
tool chain here. For instance, on PowerPc
vxworks, you would enter 'powerpc-wrs-vxworks-'.
To use JGNAT, enter 'j'.")
    )


   ;;
   ;;  Second page (Paths)
   ;;
   ((= tab-num 2)
    (if (not (equal (plist-get ada-prj-current-values 'cross_prefix)
		    ada-old-cross-prefix))
	(progn
	  (setq ada-old-cross-prefix
		(plist-get ada-prj-current-values 'cross_prefix))
	  (ada-initialize-runtime-library ada-old-cross-prefix)))


    (widget-insert "/_____________\\/              \\/______________\\/______________\\/______________\\\n")
    (ada-prj-field 'src_dir  "Source directories"
"Enter the list of directories where your Ada
sources can be found. These directories will be
used for the cross-references and for the default
compilation commands.
Note that src_dir includes both the build directory
and the standard runtime."
      t t
      (mapconcat (lambda(x)
		   (concat "           " x))
		 ada-xref-runtime-library-specs-path
		 "\n")
      )
    (widget-insert "\n\n")

    (ada-prj-field 'obj_dir  "Object directories"
"Enter the list of directories where the GNAT
library files (ALI files) can be found. These
files are used for cross-references and by the
gnatmake command.
Note that obj_dir includes both the build directory
and the standard runtime."
      t t
      (mapconcat (lambda(x)
		   (concat "           " x))
		 ada-xref-runtime-library-ali-path
		 "\n")
      )
    (widget-insert "\n\n")
    )

   ;;
   ;;  Third page (Switches)
   ;;
   ((= tab-num 3)
    (widget-insert "/_____________\\/______________\\/              \\/______________\\/______________\\\n")
    (ada-prj-field 'comp_opt "Switches for the compiler"
"These switches are used in the default
compilation commands, both for compiling a
single file and rebuilding the whole project")
    (ada-prj-field 'bind_opt "Switches for the binder"
"These switches are used in the default build
command and are passed to the binder")
    (ada-prj-field 'link_opt "Switches for the linker"
"These switches are used in the default build
command and are passed to the linker")
    (ada-prj-field 'gnatmake_opt "Switches for gnatmake"
"These switches are used in the default gnatmake
command.")
    (ada-prj-field 'gnatfind_opt "Switches for gnatfind"
"The command gnatfind is run every time the Ada/Goto/List_References menu.
You should for instance add -a if you are working in an environment
where most ALI files are write-protected, since otherwise they get
ignored by gnatfind and you don't see the references within.")
    )

   ;;
   ;;  Fourth page
   ;;
   ((= tab-num 4)
    (widget-insert "/_____________\\/______________\\/______________\\/              \\/______________\\\n")
    (widget-insert
"All the fields below can use variable substitution. The syntax is ${name},
where name is the name that appears after the Help buttons in this buffer. As
a special case, ${current} is replaced with the name of the file currently
edited, with directory name but no extension, whereas ${full_current} is
replaced with the name of the current file with directory name and
extension.\n")
    (widget-insert
"The environment variables ADA_INCLUDE_PATH and ADA_OBJECTS_PATH are set to
${src_dir} and ${obj_dir} before running the compilation commands, so that you
don't need to specify the -aI and -aO switches on the command line\n")
    (widget-insert
"You can reference any environment variable using the same ${...} syntax as
above, and put the name of the variable between the quotes.\n\n")
    (ada-prj-field 'check_cmd
      "Check syntax of a single file (menu Ada->Check File)"
"This command is run to check the syntax and semantics of a file.
The file name is added at the end of this command." t)
    (ada-prj-field 'comp_cmd
      "Compiling a single file (menu Ada->Compile File)"
"This command is run when the recompilation
of a single file is needed. The file name is
added at the end of this command." t)
    (ada-prj-field 'make_cmd "Rebuilding the whole project (menu Ada->Build)"
"This command is run when you want to rebuild
your whole application. It is never issues
automatically and you will need to ask for it.
If remote_machine has been set, this command
will be executed on the remote machine." t)
    (ada-prj-field 'run_cmd "Running the application (menu Ada->Run)"
"This command specifies how to run the
application, including any switch you need to
specify. If remote_machine has been set, this
command will be executed on the remote host." t)
    )

   ;;
   ;;  Fifth page
   ;;
   ((= tab-num 5)
    (widget-insert "/_____________\\/______________\\/______________\\/______________\\/              \\\n")
    (ada-prj-field 'debug_pre_cmd "Commands to execute before launching the
debugger"
"The following commands are executed one after the other before starting
the debugger. These can be used to set up your environment." t)

    (ada-prj-field 'debug_cmd "Debugging the application"
"Specifies how to debug the application, possibly
remotely if remote_machine has been set. We
recommend the following debuggers:
  > gdb
  > gvd --tty
  > ddd --tty -fullname -toolbar")

    (ada-prj-field 'debug_post_cmd "Commands to execute in the debugger"
"The following commands are executed one in the debugger once it has been
started. These can be used to initialize the debugger, for instance to
connect to the target when working with cross-environments" t)
    )

   )


  (widget-insert "______________________________________________________________________\n\n       ")
  (widget-create 'push-button
		 :notify (lambda (&rest _ignore)
			   (setq ada-prj-current-values (ada-default-prj-properties))
			   (ada-prj-display-page 1))
		 "Reset to Default Values")
  (widget-insert "         ")
  (widget-create 'push-button :notify (lambda (&rest _ignore) (kill-buffer nil))
		 "Cancel")
  (widget-insert "         ")
  (widget-create 'push-button :notify (lambda (&rest _ignore) (ada-prj-save))
		 "Save")
  (widget-insert "\n\n")

  (widget-setup)
  (with-no-warnings
    (beginning-of-buffer))
  )


(defun ada-customize (&optional new-file filename)
  "Edit the project file associated with the current buffer.
If there is none or NEW-FILE is non-nil, make a new one.
If FILENAME is given, edit that file."
  (interactive)

  (let ((ada-buffer (current-buffer))
	(inhibit-read-only t))

    ;;  We can only edit interactively the standard ada-mode project files. If
    ;;  the user is using other formats for the project file (through hooks in
    ;;  `ada-load-project-hook', we simply edit the file

    (if (and (not new-file)
	     (or ada-prj-default-project-file filename)
	     (string= (file-name-extension
		       (or filename ada-prj-default-project-file))
		      "gpr"))
	(progn
	  (find-file ada-prj-default-project-file)
	  (add-hook 'after-save-hook 'ada-reread-prj-file t t)
	  )

      (if filename
	  (ada-reread-prj-file filename)
	(if (not (string= ada-prj-default-project-file ""))
	    (ada-reread-prj-file ada-prj-default-project-file)
	  (ada-reread-prj-file)))

      (switch-to-buffer "*Edit Ada Mode Project*")

      (ada-prj-initialize-values 'ada-prj-current-values
				 ada-buffer
				 ada-prj-default-project-file)

      (set (make-local-variable 'ada-prj-ada-buffer) ada-buffer)

      (use-local-map
       (let ((map (make-sparse-keymap)))
         (set-keymap-parent map custom-mode-map)
         (define-key map "\C-x\C-s" 'ada-prj-save)
         map))

      ;; FIXME: Not sure if this works!!
      (set (make-local-variable 'widget-keymap)
           (let ((map (make-sparse-keymap)))
             (set-keymap-parent map widget-keymap)
             (define-key map "\C-x\C-s" 'ada-prj-save)
             map))

      (set (make-local-variable 'ada-old-cross-prefix)
	   (ada-xref-get-project-field 'cross-prefix))

      (ada-prj-display-page 1)
      )))

;; ---------------- Utilities --------------------------------

(defun ada-prj-set-list (string ada-list &optional is-directory)
  "Prepend STRING to strings in ADA-LIST, return new-line separated string.
If IS-DIRECTORY is non-nil, each element of ADA-LIST is explicitly
converted to a directory name."

  (mapconcat (lambda (x) (concat string "="
				 (if is-directory
				     (file-name-as-directory x)
				   x)))
	     ada-list "\n"))


(defun ada-prj-field-modified (widget &rest _dummy)
  "Callback for modification of WIDGET.
Remaining args DUMMY are ignored.
Save the change in `ada-prj-current-values' so that selecting
another page and coming back keeps the new value."
  (set 'ada-prj-current-values
       (plist-put ada-prj-current-values
		  (widget-get widget ':prj-field)
		  (widget-value widget))))

(defun ada-prj-display-help (widget _widget-modified event)
  "Callback for help button in WIDGET.
Parameters WIDGET-MODIFIED, EVENT match :notify for the widget."
  (let ((text (widget-get widget 'prj-help)))
    (if event
	;;  If we have a mouse-event, popup a menu
	(widget-choose "Help"
		       (mapcar (lambda (a) (cons a t))
			       (split-string text "\n"))
		       event)
      ;;  Else display the help string just before the next group of
      ;;  variables
      (momentary-string-display
       (concat "*****Help*****\n" text "\n**************\n")
       (point-at-bol 2)))))

(defun ada-prj-show-value (widget _widget-modified event)
  "Show the current field value in WIDGET.
Parameters WIDGET-MODIFIED, EVENT match :notify for the widget."
  (let* ((field (widget-get widget ':prj-field))
	 (value (plist-get ada-prj-current-values field))
	 (inhibit-read-only t)
	 w)

    ;;  If the other widget is already visible, delete it
    (if (widget-get widget 'prj-other-widget)
	(progn
	  (widget-delete (widget-get widget 'prj-other-widget))
	  (widget-put widget 'prj-other-widget nil)
	  (widget-put widget ':prj-field field)
	  (widget-default-value-set widget "Show Value")
	  )

      ;;  Else create it
      (save-excursion
	(mouse-set-point event)
	(forward-line 1)
	(beginning-of-line)
	(setq w (widget-create 'editable-list
			       :entry-format "%i%d %v"
			       :notify 'ada-prj-field-modified
			       :help-echo (widget-get widget 'prj-help)
			       :value value
			       (list 'editable-field :keymap widget-keymap)))
	(widget-put widget 'prj-other-widget w)
	(widget-put w ':prj-field field)
	(widget-put widget ':prj-field field)
	(widget-default-value-set widget "Hide Value")
	)
      )
    (widget-setup)
    ))

(defun ada-prj-field (field text help-text &optional is-list is-paths after-text)
  "Create a widget to edit FIELD in the current buffer.
TEXT is a short explanation of what the field means, whereas HELP-TEXT
is the text displayed when the user pressed the help button.
If IS-LIST is non-nil, the field contains a list.  Otherwise, it contains
a single string.
If IS-PATHS is true, some special buttons are added to load paths,...
AFTER-TEXT is inserted just after the widget."
  (let ((value (plist-get ada-prj-current-values field))
	(inhibit-read-only t)
	widget)
    (unless value
      (set 'value
	   (if is-list  '() "")))
    (widget-insert text)
    (widget-insert ":")
    (move-to-column 54 t)
    (widget-put (widget-create 'push-button
			       :notify 'ada-prj-display-help
			       "Help")
		'prj-help
		help-text)
    (widget-insert (concat "  (" (symbol-name field) ")\n"))
    (if is-paths
	(progn
	  (widget-create 'push-button
			 :notify
			 (list 'lambda '(&rest dummy) '(interactive)
			       (list 'ada-prj-load-from-file
				     (list 'quote field)))
			 "Load From File")
	  (widget-insert "      ")
	  (widget-create 'push-button
			 :notify
			 (list 'lambda '(&rest dummy) '(interactive)
			       (list 'ada-prj-load-directory
				     (list 'quote field)))
			 "Load Recursive Directory")
	  (widget-insert "\n           ${build_dir}\n")))

    (set 'widget
	 (if is-list
	     (if (< (length value) 15)
		 (widget-create 'editable-list
				:entry-format "%i%d %v"
				:notify 'ada-prj-field-modified
				:help-echo help-text
				:value value
				(list 'editable-field :keymap widget-keymap))

	       (let ((w (widget-create 'push-button
				       :notify 'ada-prj-show-value
				       "Show value")))
		 (widget-insert "\n")
		 (widget-put w 'prj-help  help-text)
		 (widget-put w 'prj-other-widget nil)
		 w)
	       )
	   (widget-create 'editable-field
			  :format "%v"
			  :notify 'ada-prj-field-modified
			  :help-echo help-text
			  :keymap widget-keymap
			  value)))
    (widget-put widget ':prj-field field)
    (if after-text
	(widget-insert after-text))
    (widget-insert "\n")
    ))


(provide 'ada-prj)

;;; ada-prj.el ends here
