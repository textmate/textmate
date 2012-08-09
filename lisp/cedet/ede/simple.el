;;; ede/simple.el --- Overlay an EDE structure on an existing project

;; Copyright (C) 2007-2012 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <eric@siege-engine.com>

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
;; NOTE: EDE Simple Projects are considered obsolete.  Use generic
;; projects instead.  They have much better automatic support and
;; simpler configuration.
;;
;; A vast majority of projects use non-EDE project techniques, such
;; as hand written Makefiles, or other IDE's.
;;
;; The EDE-SIMPLE project type allows EDE to wrap an existing mechanism
;; with minimal configuration, and then provides project-root
;; information to Semantic or other tools, and also provides structure
;; information for in-project include header discovery, or speedbar
;; support.
;;
;; It will also support a the minimal EDE UI for compilation and
;; configuration.

;; @todo - Add support for cpp-root as an ede-simple project.
;; @todo - Allow ede-simple to store locally.

(require 'ede)
(require 'cedet-files)

;;; Code:

(add-to-list 'ede-project-class-files
	     (ede-project-autoload "simple-overlay"
	      :name "Simple" :file 'ede/simple
	      :proj-file 'ede-simple-projectfile-for-dir
	      :load-type 'ede-simple-load
	      :class-sym 'ede-simple-project
	      :safe-p nil)
	     t)

(defcustom ede-simple-save-directory "~/.ede"
  "*Directory where simple EDE project overlays are saved."
 :group 'ede
 :type 'directory)

(defcustom ede-simple-save-file-name "ProjSimple.ede"
  "*File name used for simple project wrappers."
  :group 'ede
  :type 'string)

(defun ede-simple-projectfile-for-dir (&optional dir)
  "Return a full file name to the project file stored in the current directory.
The directory has three parts:
  <STORAGE ROOT>/<PROJ DIR AS FILE>/ProjSimple.ede"
  (let ((d (or dir default-directory)))
    (concat
     ;; Storage root
     (file-name-as-directory (expand-file-name ede-simple-save-directory))
     ;; Convert directory to filename
     (cedet-directory-name-to-file-name d)
     ;; Filename
     ede-simple-save-file-name)
    ))

(defun ede-simple-load (dir &optional rootproj)
  "Load a project of type `Simple' for the directory DIR.
Return nil if there isn't one.
ROOTPROJ is nil, since we will only create a single EDE project here."
  (let ((pf (ede-simple-projectfile-for-dir dir))
	(obj nil))
    (when pf
      (setq obj (eieio-persistent-read pf))
      (oset obj :directory dir)
      )
    obj))

(defclass ede-simple-target (ede-target)
  ()
  "EDE Simple project target.
All directories need at least one target.")

(defclass ede-simple-project (ede-project eieio-persistent)
  ((extension :initform ".ede")
   (file-header-line :initform ";; EDE Simple Project")
   )
  "EDE Simple project class.
Each directory needs a project file to control it.")

(defmethod ede-commit-project ((proj ede-simple-project))
  "Commit any change to PROJ to its file."
  (when (not (file-exists-p ede-simple-save-directory))
    (if (y-or-n-p (concat ede-simple-save-directory
			  " doesn't exist.  Create? "))
	(make-directory ede-simple-save-directory)
      (error "No save directory for new project")))
  (eieio-persistent-save proj))

(defmethod ede-find-subproject-for-directory ((proj ede-simple-project)
					      dir)
  "Return PROJ, for handling all subdirs below DIR."
  proj)

(provide 'ede/simple)

;;; ede/simple.el ends here
