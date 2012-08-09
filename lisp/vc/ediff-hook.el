;;; ediff-hook.el --- setup for Ediff's menus and autoloads

;; Copyright (C) 1995-2012 Free Software Foundation, Inc.

;; Author: Michael Kifer <kifer@cs.stonybrook.edu>
;; Package: ediff

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

;;; Code:

;;;   These must be placed in menu-bar.el in Emacs
;;
;;      (define-key menu-bar-tools-menu [ediff-misc]
;;	'("Ediff Miscellanea" . menu-bar-ediff-misc-menu))
;;      (define-key menu-bar-tools-menu [epatch]
;;	'("Apply Patch" . menu-bar-epatch-menu))
;;      (define-key menu-bar-tools-menu [ediff-merge]
;;	'("Merge" . menu-bar-ediff-merge-menu))
;;      (define-key menu-bar-tools-menu [ediff]
;;	'("Compare" . menu-bar-ediff-menu))

;; Compiler pacifier
(defvar ediff-menu)
(defvar ediff-merge-menu)
(defvar epatch-menu)
(defvar ediff-misc-menu)
;; end pacifier

;; allow menus to be set up without ediff-wind.el being loaded
(defvar ediff-window-setup-function)

;; This autoload is useless in Emacs because ediff-hook.el is dumped with
;; emacs, but it is needed in XEmacs
;;;###autoload
(if (featurep 'xemacs)
    (progn
      (defun ediff-xemacs-init-menus ()
	(when (featurep 'menubar)
	  (add-submenu
	   '("Tools") ediff-menu "OO-Browser...")
	  (add-submenu
	   '("Tools") ediff-merge-menu "OO-Browser...")
	  (add-submenu
	   '("Tools") epatch-menu "OO-Browser...")
	  (add-submenu
	   '("Tools") ediff-misc-menu "OO-Browser...")
	  (add-menu-button
	   '("Tools") "-------" "OO-Browser...")
	  ))
      (defvar ediff-menu
	'("Compare"
	  ["Two Files..."  ediff-files t]
	  ["Two Buffers..." ediff-buffers t]
	  ["Three Files..."  ediff-files3 t]
	  ["Three Buffers..." ediff-buffers3 t]
	  "---"
	  ["Two Directories..." ediff-directories t]
	  ["Three Directories..." ediff-directories3 t]
	  "---"
	  ["File with Revision..."  ediff-revision t]
	  ["Directory Revisions..."  ediff-directory-revisions t]
	  "---"
	  ["Windows Word-by-word..." ediff-windows-wordwise t]
	  ["Windows Line-by-line..." ediff-windows-linewise t]
	  "---"
	  ["Regions Word-by-word..." ediff-regions-wordwise t]
	  ["Regions Line-by-line..." ediff-regions-linewise t]
	  ))
      (defvar ediff-merge-menu
	'("Merge"
	  ["Files..."  ediff-merge-files t]
	  ["Files with Ancestor..." ediff-merge-files-with-ancestor t]
	  ["Buffers..."  ediff-merge-buffers t]
	  ["Buffers with Ancestor..."
	   ediff-merge-buffers-with-ancestor t]
	  "---"
	  ["Directories..."  ediff-merge-directories t]
	  ["Directories with Ancestor..."
	   ediff-merge-directories-with-ancestor t]
	  "---"
	  ["Revisions..."  ediff-merge-revisions t]
	  ["Revisions with Ancestor..."
	   ediff-merge-revisions-with-ancestor t]
	  ["Directory Revisions..." ediff-merge-directory-revisions t]
	  ["Directory Revisions with Ancestor..."
	   ediff-merge-directory-revisions-with-ancestor t]
	  ))
      (defvar epatch-menu
	'("Apply Patch"
	  ["To a file..."  ediff-patch-file t]
	  ["To a buffer..." ediff-patch-buffer t]
	  ))
      (defvar ediff-misc-menu
	'("Ediff Miscellanea"
	  ["Ediff Manual" ediff-documentation t]
	  ["Customize Ediff" ediff-customize t]
	  ["List Ediff Sessions" ediff-show-registry t]
	  ["Use separate frame for Ediff control buffer"
	   ediff-toggle-multiframe
	   :style toggle
	   :selected (if (and (featurep 'ediff-util)
			      (boundp 'ediff-window-setup-function))
			 (eq ediff-window-setup-function
			     'ediff-setup-windows-multiframe))]
	  ["Use a toolbar with Ediff control buffer"
	   ediff-toggle-use-toolbar
	   :style toggle
	   :selected (if (featurep 'ediff-tbar)
			 (ediff-use-toolbar-p))]))

      ;; put these menus before Object-Oriented-Browser in Tools menu
      (if (and (featurep 'menubar) (not (featurep 'infodock))
	       (not (featurep 'ediff-hook)))
	  (ediff-xemacs-init-menus)))
  ;; Emacs
  ;; initialize menu bar keymaps
  (defvar menu-bar-ediff-misc-menu
    (make-sparse-keymap "Ediff Miscellanea"))
  (fset 'menu-bar-ediff-misc-menu
	(symbol-value 'menu-bar-ediff-misc-menu))
  (defvar menu-bar-epatch-menu (make-sparse-keymap "Apply Patch"))
  (fset 'menu-bar-epatch-menu (symbol-value 'menu-bar-epatch-menu))
  (defvar menu-bar-ediff-merge-menu (make-sparse-keymap "Merge"))
  (fset 'menu-bar-ediff-merge-menu
	(symbol-value 'menu-bar-ediff-merge-menu))
  (defvar menu-bar-ediff-menu (make-sparse-keymap "Compare"))
  (fset 'menu-bar-ediff-menu (symbol-value 'menu-bar-ediff-menu))

  ;; define ediff compare menu
  (define-key menu-bar-ediff-menu [ediff-misc]
    `(menu-item ,(purecopy "Ediff Miscellanea") menu-bar-ediff-misc-menu))
  (define-key menu-bar-ediff-menu [separator-ediff-misc] menu-bar-separator)
  (define-key menu-bar-ediff-menu [window]
    `(menu-item ,(purecopy "This Window and Next Window") compare-windows
		:help ,(purecopy "Compare the current window and the next window")))
  (define-key menu-bar-ediff-menu [ediff-windows-linewise]
    `(menu-item ,(purecopy "Windows Line-by-line...") ediff-windows-linewise
		:help ,(purecopy "Compare windows line-wise")))
  (define-key menu-bar-ediff-menu [ediff-windows-wordwise]
    `(menu-item ,(purecopy "Windows Word-by-word...") ediff-windows-wordwise
		:help ,(purecopy "Compare windows word-wise")))
  (define-key menu-bar-ediff-menu [separator-ediff-windows] menu-bar-separator)
  (define-key menu-bar-ediff-menu [ediff-regions-linewise]
    `(menu-item ,(purecopy "Regions Line-by-line...") ediff-regions-linewise
		:help ,(purecopy "Compare regions line-wise")))
  (define-key menu-bar-ediff-menu [ediff-regions-wordwise]
    `(menu-item ,(purecopy "Regions Word-by-word...") ediff-regions-wordwise
		:help ,(purecopy "Compare regions word-wise")))
  (define-key menu-bar-ediff-menu [separator-ediff-regions] menu-bar-separator)
  (define-key menu-bar-ediff-menu [ediff-dir-revision]
    `(menu-item ,(purecopy "Directory Revisions...") ediff-directory-revisions
		:help ,(purecopy "Compare directory files with their older versions")))
  (define-key menu-bar-ediff-menu [ediff-revision]
    `(menu-item ,(purecopy "File with Revision...") ediff-revision
		:help ,(purecopy "Compare file with its older versions")))
  (define-key menu-bar-ediff-menu [separator-ediff-directories] menu-bar-separator)
  (define-key menu-bar-ediff-menu [ediff-directories3]
    `(menu-item ,(purecopy "Three Directories...") ediff-directories3
		:help ,(purecopy "Compare files common to three directories simultaneously")))
  (define-key menu-bar-ediff-menu [ediff-directories]
    `(menu-item ,(purecopy "Two Directories...") ediff-directories
		:help ,(purecopy "Compare files common to two directories simultaneously")))
  (define-key menu-bar-ediff-menu [separator-ediff-files] menu-bar-separator)
  (define-key menu-bar-ediff-menu [ediff-buffers3]
    `(menu-item ,(purecopy "Three Buffers...") ediff-buffers3
		:help ,(purecopy "Compare three buffers simultaneously")))
  (define-key menu-bar-ediff-menu [ediff-files3]
    `(menu-item ,(purecopy "Three Files...") ediff-files3
		:help ,(purecopy "Compare three files simultaneously")))
  (define-key menu-bar-ediff-menu [ediff-buffers]
    `(menu-item ,(purecopy "Two Buffers...") ediff-buffers
		:help ,(purecopy "Compare two buffers simultaneously")))
  (define-key menu-bar-ediff-menu [ediff-files]
    `(menu-item ,(purecopy "Two Files...") ediff-files
		:help ,(purecopy "Compare two files simultaneously")))

  ;; define ediff merge menu
  (define-key
    menu-bar-ediff-merge-menu [ediff-merge-dir-revisions-with-ancestor]
    `(menu-item ,(purecopy "Directory Revisions with Ancestor...")
      ediff-merge-directory-revisions-with-ancestor
      :help ,(purecopy "Merge versions of the files in the same directory by comparing the files with common ancestors")))
  (define-key
    menu-bar-ediff-merge-menu [ediff-merge-dir-revisions]
    `(menu-item ,(purecopy "Directory Revisions...") ediff-merge-directory-revisions
      :help ,(purecopy "Merge versions of the files in the same directory (without using ancestor information)")))
  (define-key
    menu-bar-ediff-merge-menu [ediff-merge-revisions-with-ancestor]
    `(menu-item ,(purecopy "Revisions with Ancestor...")
      ediff-merge-revisions-with-ancestor
      :help ,(purecopy "Merge versions of the same file by comparing them with a common ancestor")))
  (define-key menu-bar-ediff-merge-menu [ediff-merge-revisions]
    `(menu-item ,(purecopy "Revisions...") ediff-merge-revisions
      :help ,(purecopy "Merge versions of the same file (without using ancestor information)")))
  (define-key menu-bar-ediff-merge-menu [separator-ediff-merge] menu-bar-separator)
  (define-key
    menu-bar-ediff-merge-menu [ediff-merge-directories-with-ancestor]
    `(menu-item ,(purecopy "Directories with Ancestor...")
      ediff-merge-directories-with-ancestor
      :help ,(purecopy "Merge files common to a pair of directories by comparing the files with common ancestors")))
  (define-key menu-bar-ediff-merge-menu [ediff-merge-directories]
    `(menu-item ,(purecopy "Directories...") ediff-merge-directories
		:help ,(purecopy "Merge files common to a pair of directories")))
  (define-key
    menu-bar-ediff-merge-menu [separator-ediff-merge-dirs] menu-bar-separator)
  (define-key
    menu-bar-ediff-merge-menu [ediff-merge-buffers-with-ancestor]
    `(menu-item ,(purecopy "Buffers with Ancestor...") ediff-merge-buffers-with-ancestor
      :help ,(purecopy "Merge buffers by comparing their contents with a common ancestor")))
  (define-key menu-bar-ediff-merge-menu [ediff-merge-buffers]
    `(menu-item ,(purecopy "Buffers...") ediff-merge-buffers
      :help ,(purecopy "Merge buffers (without using ancestor information)")))
  (define-key menu-bar-ediff-merge-menu [ediff-merge-files-with-ancestor]
    `(menu-item ,(purecopy "Files with Ancestor...") ediff-merge-files-with-ancestor
      :help ,(purecopy "Merge files by comparing them with a common ancestor")))
  (define-key menu-bar-ediff-merge-menu [ediff-merge-files]
    `(menu-item ,(purecopy "Files...") ediff-merge-files
      :help ,(purecopy "Merge files (without using ancestor information)")))

  ;; define epatch menu
  (define-key menu-bar-epatch-menu [ediff-patch-buffer]
    `(menu-item ,(purecopy "To a Buffer...") ediff-patch-buffer
      :help ,(purecopy "Apply a patch to the contents of a buffer")))
  (define-key menu-bar-epatch-menu [ediff-patch-file]
    `(menu-item ,(purecopy "To a File...") ediff-patch-file
      :help ,(purecopy "Apply a patch to a file")))

  ;; define ediff miscellanea
  (define-key menu-bar-ediff-misc-menu [emultiframe]
    `(menu-item ,(purecopy "Use separate control buffer frame")
      ediff-toggle-multiframe
      :help ,(purecopy "Switch between the single-frame presentation mode and the multi-frame mode")))
  (define-key menu-bar-ediff-misc-menu [eregistry]
    `(menu-item ,(purecopy "List Ediff Sessions") ediff-show-registry
		:help ,(purecopy "List all active Ediff sessions; it is a convenient way to find and resume such a session")))
  (define-key menu-bar-ediff-misc-menu [ediff-cust]
    `(menu-item ,(purecopy "Customize Ediff") ediff-customize
		:help ,(purecopy "Change some of the parameters that govern the behavior of Ediff")))
  (define-key menu-bar-ediff-misc-menu [ediff-doc]
    `(menu-item ,(purecopy "Ediff Manual") ediff-documentation
		:help ,(purecopy "Bring up the Ediff manual"))))

(provide 'ediff-hook)


;;; ediff-hook.el ends here
