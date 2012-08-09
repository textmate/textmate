;; ede/source.el --- EDE source code object

;; Copyright (C) 2000, 2008-2012  Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: project, make

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

;; Manage different types of source code.  A master list of source code types
;; will be maintained, and used to track target objects, what they accept,
;; and what compilers can be used.

(require 'eieio-base)

;;; Code:
(defclass ede-sourcecode (eieio-instance-inheritor)
  ((name :initarg :name
	 :type string
	 :documentation
	 "The name of this type of source code.
Such as \"C\" or \"Emacs Lisp\"")
   (sourcepattern :initarg :sourcepattern
		  :initform ".*"
		  :type string
		  :documentation
		  "Emacs regexp matching sourcecode this target accepts.")
   (auxsourcepattern :initarg :auxsourcepattern
		     :initform nil
		     :type (or null string)
		     :documentation
		     "Emacs regexp matching auxiliary source code this target accepts.
Aux source are source code files needed for compilation, which are not compiled
themselves.")
   (enable-subdirectories :initarg :enable-subdirectories
			  :initform nil
			  :type boolean
			  :documentation
			  "Non nil if this sourcecode type uses subdirectories.
If sourcecode always lives near the target creating it, this should be nil.
If sourcecode can, or typically lives in a subdirectory of the owning
target, set this to t.")
   (garbagepattern :initarg :garbagepattern
		   :initform nil
		   :type list
		   :documentation
		   "Shell file regexp matching files considered as garbage.
This is a list of items added to an `rm' command when executing a `clean'
type directive.")
   )
  "Description of some type of source code.
Objects will use sourcecode objects to define the types of source
that they are willing to use.")

(defvar ede-sourcecode-list nil
  "The master list of all EDE compilers.")

;;; Methods
;;
(defmethod initialize-instance :AFTER ((this ede-sourcecode) &rest fields)
  "Make sure that all ede compiler objects are cached in
`ede-compiler-list'."
  (let ((lst ede-sourcecode-list))
    ;; Find an object of the same name.
    (while (and lst (not (string= (oref this name) (oref (car lst) name))))
      (setq lst (cdr lst)))
    (if lst
	;; Replace old definition
	(setcar lst this)
      ;; Add to the beginning of the list.
      (setq ede-sourcecode-list (cons this ede-sourcecode-list)))))

(defmethod ede-want-file-p ((this ede-sourcecode) filename)
  "Return non-nil if sourcecode definition THIS will take FILENAME."
  (or (ede-want-file-source-p this filename)
      (ede-want-file-auxiliary-p this filename)))

(defmethod ede-want-file-source-p ((this ede-sourcecode) filename)
  "Return non-nil if THIS will take FILENAME as an auxiliary ."
  (let ((case-fold-search nil))
    (string-match (oref this sourcepattern) filename)))

(defmethod ede-want-file-auxiliary-p ((this ede-sourcecode) filename)
  "Return non-nil if THIS will take FILENAME as an auxiliary ."
  (let ((case-fold-search nil))
    (and (slot-boundp this 'auxsourcepattern)
	 (oref this auxsourcepattern)
	 (string-match (oref this auxsourcepattern) filename))))

(defmethod ede-want-any-source-files-p ((this ede-sourcecode) filenames)
  "Return non-nil if THIS will accept any source files in FILENAMES."
  (let (found)
    (while (and (not found) filenames)
      (setq found (ede-want-file-source-p this (pop filenames))))
    found))

(defmethod ede-want-any-auxiliary-files-p ((this ede-sourcecode) filenames)
  "Return non-nil if THIS will accept any aux files in FILENAMES."
  (let (found)
    (while (and (not found) filenames)
      (setq found (ede-want-file-auxiliary-p this (pop filenames))))
    found))

(defmethod ede-want-any-files-p ((this ede-sourcecode) filenames)
  "Return non-nil if THIS will accept any files in FILENAMES."
  (let (found)
    (while (and (not found) filenames)
      (setq found (ede-want-file-p this (pop filenames))))
    found))

(defmethod ede-buffer-header-file ((this ede-sourcecode) filename)
  "Return a list of file names of header files for THIS with FILENAME.
Used to guess header files, but uses the auxsource regular expression."
  (let ((dn (file-name-directory filename))
	(ts (file-name-sans-extension (file-name-nondirectory filename)))
	(ae (oref this auxsourcepattern)))
    (if (not ae)
	nil
      (directory-files dn t (concat (regexp-quote ts) ae)))))

;;; Utility functions
;;
(when nil
  ;; not used at the moment.
(defun ede-source-find (name)
  "Find the sourcecode object based on NAME."
  (object-assoc name :name ede-sourcecode-list))

(defun ede-source-match (file)
  "Find the list of sourcecode objects which matches FILE."
  (let ((lst ede-sourcecode-list)
	(match nil))
    (while lst
      ;; ede-file-mine doesn't exist yet
      (if (ede-file-mine (car lst) file)
	  (setq match (cons (car lst) match)))
      (setq lst (cdr lst)))
    match))
)
;;; Master list of source code types
;;
;; This must appear at the end so that the init method will work.
(defvar ede-source-scheme
  (ede-sourcecode "ede-source-scheme"
		  :name "Scheme"
		  :sourcepattern "\\.scm$")
  "Scheme source code definition.")

;;(defvar ede-source-
;;  (ede-sourcecode "ede-source-"
;;		    :name ""
;;		    :sourcepattern "\\.$"
;;		    :garbagepattern '("*."))
;;  " source code definition.")

(provide 'ede/source)

;;; ede/source.el ends here
