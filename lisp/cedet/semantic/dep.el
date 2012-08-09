;;; semantic/dep.el --- Methods for tracking dependencies (include files)

;; Copyright (C) 2006-2012  Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax

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
;;
;; Include tags (dependencies for a given source file) usually have
;; some short name.  The target file that it is dependent on is
;; generally found on some sort of path controlled by the compiler or
;; project.
;;
;; EDE or even ECB can control our project dependencies, and help us
;; find file within the setting of a given project.  For system
;; dependencies, we need to depend on user supplied lists, which can
;; manifest themselves in the form of system databases (from
;; semanticdb.)
;;
;; Provide ways to track these different files here.

(require 'semantic/tag)

;;; Code:

(defvar semantic-dependency-include-path nil
  "Defines the include path used when searching for files.
This should be a list of directories to search which is specific
to the file being included.

If `semantic-dependency-tag-file' is overridden for a given
language, this path is most likely ignored.

The above function, regardless of being overridden, caches the
located dependency file location in the tag property
`dependency-file'.  If you override this function, you do not
need to implement your own cache.  Each time the buffer is fully
reparsed, the cache will be reset.

TODO: use ffap.el to locate such items?

NOTE: Obsolete this, or use as special user")
(make-variable-buffer-local `semantic-dependency-include-path)

(defvar semantic-dependency-system-include-path nil
  "Defines the system include path.
This should be set with either `defvar-mode-local', or with
`semantic-add-system-include'.

For mode authors, use
`defcustom-mode-local-semantic-dependency-system-include-path'
to create a mode-specific variable to control this.

When searching for a file associated with a name found in an tag of
class include, this path will be inspected for includes of type
`system'.  Some include tags are agnostic to this setting and will
check both the project and system directories.")
(make-variable-buffer-local `semantic-dependency-system-include-path)

(defmacro defcustom-mode-local-semantic-dependency-system-include-path
  (mode name value &optional docstring)
  "Create a mode-local value of the system-dependency include path.
MODE is the `major-mode' this name/value pairs is for.
NAME is the name of the customizable value users will use.
VALUE is the path (a list of strings) to add.
DOCSTRING is a documentation string applied to the variable NAME
users will customize.

Creates a customizable variable users can customize that will
keep semantic data structures up to date."
  `(progn
     ;; Create a variable users can customize.
     (defcustom ,name ,value
       ,docstring
       :group (quote ,(intern (car (split-string (symbol-name mode) "-"))))
       :group 'semantic
       :type '(repeat (directory :tag "Directory"))
       :set (lambda (sym val)
	      (set-default sym val)
	      (setq-mode-local ,mode
			       semantic-dependency-system-include-path
			       val)
	      (when (fboundp
		     'semantic-decoration-unparsed-include-do-reset)
		(mode-local-map-mode-buffers
		 'semantic-decoration-unparsed-include-do-reset
		 (quote ,mode))))
       )
     ;; Set the variable to the default value.
     (defvar-mode-local ,mode semantic-dependency-system-include-path
       ,name
       "System path to search for include files.")
     ;; Bind NAME onto our variable so tools can customize it
     ;; without knowing about it.
     (put 'semantic-dependency-system-include-path
	  (quote ,mode) (quote ,name))
     ))

;;; PATH MANAGEMENT
;;
;; Some fcns to manage paths for a give mode.
;;;###autoload
(defun semantic-add-system-include (dir &optional mode)
  "Add a system include DIR to path for MODE.
Modifies a mode-local version of `semantic-dependency-system-include-path'.

Changes made by this function are not persistent."
  (interactive "DNew Include Directory: ")
  (if (not mode) (setq mode major-mode))
  (let ((dirtmp (file-name-as-directory dir))
	(value
	 (mode-local-value mode 'semantic-dependency-system-include-path))
	)
    (add-to-list 'value dirtmp t)
    (eval `(setq-mode-local ,mode
			    semantic-dependency-system-include-path value))
    ))

;;;###autoload
(defun semantic-remove-system-include (dir &optional mode)
  "Add a system include DIR to path for MODE.
Modifies a mode-local version of`semantic-dependency-system-include-path'.

Changes made by this function are not persistent."
  (interactive (list
		 (completing-read
		  "Include Directory to Remove: "
		  semantic-dependency-system-include-path))
	       )
  (if (not mode) (setq mode major-mode))
  (let ((dirtmp (file-name-as-directory dir))
	(value
	 (mode-local-value mode 'semantic-dependency-system-include-path))
	)
    (setq value (delete dirtmp value))
    (eval `(setq-mode-local ,mode semantic-dependency-system-include-path
			    value))
    ))

;;;###autoload
(defun semantic-reset-system-include (&optional mode)
  "Reset the system include list to empty for MODE.
Modifies a mode-local version of
`semantic-dependency-system-include-path'."
  (interactive)
  (if (not mode) (setq mode major-mode))
  (eval `(setq-mode-local ,mode semantic-dependency-system-include-path
			  nil))
  )

;;;###autoload
(defun semantic-customize-system-include-path (&optional mode)
  "Customize the include path for this `major-mode'.
To create a customizable include path for a major MODE, use the
macro `defcustom-mode-local-semantic-dependency-system-include-path'."
  (interactive)
  (let ((ips (get 'semantic-dependency-system-include-path
		  (or mode major-mode))))
    ;; Do we have one?
    (when (not ips)
      (error "There is no customizable includepath variable for %s"
	     (or mode major-mode)))
    ;; Customize it.
    (customize-variable ips)))

;;; PATH SEARCH
;;
;; methods for finding files on a provided path.
(defmacro semantic--dependency-find-file-on-path (file path)
  (if (fboundp 'locate-file)
      `(locate-file ,file ,path)
    `(let ((p ,path)
	   (found nil))
       (while (and p (not found))
	 (let ((f (expand-file-name ,file (car p))))
	   (if (file-exists-p f)
	       (setq found f)))
	 (setq p (cdr p)))
       found)))

(defvar ede-minor-mode)
(defvar ede-object)
(declare-function ede-system-include-path "ede")

(defun semantic-dependency-find-file-on-path (file systemp &optional mode)
  "Return an expanded file name for FILE on available paths.
If SYSTEMP is true, then only search system paths.
If optional argument MODE is non-nil, then derive paths from the
provided mode, not from the current major mode."
  (if (not mode) (setq mode major-mode))
  (let ((sysp (mode-local-value
	       mode 'semantic-dependency-system-include-path))
	(edesys (when (and (featurep 'ede) ede-minor-mode
			   ede-object)
		  (ede-system-include-path
		   (if (listp ede-object) (car ede-object) ede-object))))
	(locp (mode-local-value
	       mode 'semantic-dependency-include-path))
	(found nil))
    (when (file-exists-p file)
      (setq found file))
    (when (and (not found) (not systemp))
      (setq found (semantic--dependency-find-file-on-path file locp)))
    (when (and (not found) edesys)
      (setq found (semantic--dependency-find-file-on-path file edesys)))
    (when (not found)
      (setq found (semantic--dependency-find-file-on-path file sysp)))
    (if found (expand-file-name found))))


(provide 'semantic/dep)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "semantic/dep"
;; End:

;;; semantic/dep.el ends here
