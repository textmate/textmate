;;; find-file.el --- find a file corresponding to this one given a pattern

;; Author: Henry Guillaume <henri@tibco.com, henry@c032.aone.net.au>
;; Maintainer: FSF
;; Keywords: c, matching, tools

;; Copyright (C) 1994-1995, 2001-2012 Free Software Foundation, Inc.

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

;; PURPOSE:
;; This package features a function called ff-find-other-file, which performs
;; the following function:
;;
;;     When in a .c file, find the first corresponding .h file in a set
;;     of directories and display it, and vice-versa from the .h file.
;;
;; Many people maintain their include file in a directory separate to their
;; src directory, and very often you may be editing a file and have a need to
;; visit the "other file". This package searches through a set of directories
;; to find that file.
;;
;; THE "OTHER FILE", or "corresponding file", generally has the same basename,
;; and just has a different extension as described by the ff-other-file-alist
;; variable:
;;
;;   '(("\\.cc$"  (".hh" ".h"))
;;     ("\\.hh$"  (".cc" ".C" ".CC" ".cxx" ".cpp")))
;;
;; If the current file has a .cc extension, ff-find-other-file will attempt
;; to look for a .hh file, and then a .h file in some directory as described
;; below. The mechanism here is to replace the matched part of the original
;; filename with each of the corresponding extensions in turn.
;;
;; Alternatively, there are situations where the filename of the other file
;; cannot be determined easily with regexps. For example, a .c file may
;; have two corresponding .h files, for its public and private parts, or
;; the filename for the .c file contains part of the pathname of the .h
;; file, as between src/fooZap.cc and include/FOO/zap.hh. In that case, the
;; format above can be changed to include a function to be called when the
;; current file matches the regexp:
;;
;;   '(("\\.cc$"  cc--function)
;;     ("\\.hh$"  hh-function))
;;
;; These functions must return a list consisting of the possible names of the
;; corresponding file, with or without path. There is no real need for more
;; than one function, and one could imagine the following value for cc-other-
;; file-alist:
;;
;;    (setq cc-other-file-alist
;;        '(("\\.cc$"  ff-cc-hh-converter)
;;          ("\\.hh$"  ff-cc-hh-converter)
;;          ("\\.c$"   (".h"))
;;          ("\\.h$"   (".c" ".cc" ".C" ".CC" ".cxx" ".cpp"))))
;;
;; ff-cc-hh-converter is included at the end of this file as a reference.
;;
;; SEARCHING is carried out in a set of directories specified by the
;; ff-search-directories variable:
;;
;;     ("." "../../src" "../include/*" "/usr/local/*/src/*" "$PROJECT/src")
;;
;; This means that the corresponding file will be searched for first in
;; the current directory, then in ../../src, then in one of the directories
;; under ../include, and so on. The star is _not_ a general wildcard
;; character: it just indicates that the subdirectories of this directory
;; must each be searched in turn. Environment variables will be expanded in
;; the ff-search-directories variable.
;;
;; If the point is on a #include line, the file to be #included is searched
;; for in the same manner. This can be disabled with the ff-ignore-include
;; variable, or by calling ff-get-other-file instead of ff-find-other-file.
;;
;; If the file was not found, ff-find-other-file will prompt you for where
;; to create the new "corresponding file" (defaults to the current directory),
;; unless the variable ff-always-try-to-create is set to nil.
;;
;; GIVEN AN ARGUMENT (with the ^U prefix), ff-find-other-file will get the
;; other file in another (the other?) window (see find-file-other-window and
;; switch-to-buffer-other-window). This can be set on a more permanent basis
;; by setting ff-always-in-other-window to t in which case the ^U prefix will
;; do the opposite of what was described above.
;;
;; THERE ARE FIVE AVAILABLE HOOKS, called in this order if non-nil:
;;
;; - ff-pre-find-hook     - called before the search for the other file starts
;; - ff-not-found-hook    - called when the other file could not be found
;; - ff-pre-load-hook     - called just before the other file is 'loaded'
;; - ff-file-created-hook - called when the other file is created
;; - ff-post-load-hook    - called just after the other file is 'loaded'
;;
;; The *load-hook allow you to place point where you want it in the other
;; file.

;; CREDITS:
;; Many thanks go to TUSC Computer Systems Pty Ltd for providing an environ-
;; ment that made the development of this package possible.
;;
;; Many thanks also go to all those who provided valuable feedback throughout
;; the development of this package:
;;     Rolf Ebert in particular, Fritz Knabe, Heddy Boubaker, Sebastian Kremer,
;;     Vasco Lopes Paulo, Mark A. Plaksin, Robert Lang, Trevor West, Kevin
;;     Pereira, Benedict Lofstedt & Justin Vallon.

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User definable variables:

(defgroup ff nil
  "Find a file corresponding to this one given a pattern."
  :prefix "ff-"
  :link '(emacs-commentary-link "find-file")
  :group 'find-file)

(defcustom ff-pre-find-hook nil
  "List of functions to be called before the search for the file starts."
  :type 'hook
  :group 'ff)

(defcustom ff-pre-load-hook nil
  "List of functions to be called before the other file is loaded."
  :type 'hook
  :group 'ff)

(defcustom ff-post-load-hook nil
  "List of functions to be called after the other file is loaded."
  :type 'hook
  :group 'ff)

(defcustom ff-not-found-hook nil
  "List of functions to be called if the other file could not be found."
  :type 'hook
  :group 'ff)

(defcustom ff-file-created-hook nil
  "List of functions to be called if the other file needs to be created."
  :type 'hook
  :group 'ff)

(defcustom ff-case-fold-search nil
  "Non-nil means ignore cases in matches (see `case-fold-search').
If you have extensions in different cases, you will want this to be nil."
  :type 'boolean
  :group 'ff)

(defcustom ff-always-in-other-window nil
  "If non-nil, find the corresponding file in another window by default.
To override this, give an argument to `ff-find-other-file'."
  :type 'boolean
  :group 'ff)

(defcustom ff-ignore-include nil
  "If non-nil, ignore `#include' lines."
  :type 'boolean
  :group 'ff)

(defcustom ff-always-try-to-create t
  "If non-nil, always attempt to create the other file if it was not found."
  :type 'boolean
  :group 'ff)

(defcustom ff-quiet-mode nil
  "If non-nil, trace which directories are being searched."
  :type 'boolean
  :group 'ff)

;;;###autoload
(defvar ff-special-constructs
  `(
    ;; C/C++ include, for NeXTstep too
    (,(purecopy "^\#\\s *\\(include\\|import\\)\\s +[<\"]\\(.*\\)[>\"]") .
     (lambda ()
       (buffer-substring (match-beginning 2) (match-end 2))))
    )
  ;; We include `ff-treat-as-special' documentation here so that autoload
  ;; can make it available to be read prior to loading this file.
  "*List of special constructs for `ff-treat-as-special' to recognize.
Each element, tried in order, has the form (REGEXP . EXTRACT).
If REGEXP matches the current line (from the beginning of the line),
`ff-treat-as-special' calls function EXTRACT with no args.
If EXTRACT returns nil, keep trying.  Otherwise, return the
filename that EXTRACT returned.")

(defvaralias 'ff-related-file-alist 'ff-other-file-alist)
(defcustom ff-other-file-alist 'cc-other-file-alist
  "Alist of extensions to find given the current file's extension.

This list should contain the most used extensions before the others,
since the search algorithm searches sequentially through each
directory specified in `ff-search-directories'.  If a file is not found,
a new one is created with the first matching extension (`.cc' yields `.hh').
This alist should be set by the major mode."
  :type '(choice (repeat (list regexp (choice (repeat string) function)))
		 symbol)
  :group 'ff)

(defcustom ff-search-directories 'cc-search-directories
  "List of directories to search for a specific file.

Set by default to `cc-search-directories', expanded at run-time.

This list is searched through with each extension specified in
`ff-other-file-alist' that matches this file's extension.  So the
longer the list, the longer it'll take to realize that a file
may not exist.

A typical format is

    '(\".\" \"/usr/include\" \"$PROJECT/*/include\")

Environment variables can be inserted between slashes (`/').
They will be replaced by their definition.  If a variable does
not exist, it is replaced (silently) with an empty string.

The stars are *not* wildcards: they are searched for together with
the preceding slash.  The star represents all the subdirectories except
`..', and each of these subdirectories will be searched in turn."
  :type '(choice (repeat directory) symbol)
  :group 'ff)

(defcustom cc-search-directories
  '("." "/usr/include" "/usr/local/include/*")
  "See the description of the `ff-search-directories' variable."
  :type '(repeat directory)
  :group 'ff)

(defcustom cc-other-file-alist
  '(("\\.cc\\'"  (".hh" ".h"))
    ("\\.hh\\'"  (".cc" ".C"))

    ("\\.c\\'"   (".h"))
    ("\\.h\\'"   (".c" ".cc" ".C" ".CC" ".cxx" ".cpp"))

    ("\\.C\\'"   (".H"  ".hh" ".h"))
    ("\\.H\\'"   (".C"  ".CC"))

    ("\\.CC\\'"  (".HH" ".H"  ".hh" ".h"))
    ("\\.HH\\'"  (".CC"))

    ("\\.c\\+\\+\\'" (".h++" ".hh" ".h"))
    ("\\.h\\+\\+\\'" (".c++"))

    ("\\.cpp\\'" (".hpp" ".hh" ".h"))
    ("\\.hpp\\'" (".cpp"))

    ("\\.cxx\\'" (".hxx" ".hh" ".h"))
    ("\\.hxx\\'" (".cxx")))
  "Alist of extensions to find given the current file's extension.

This list should contain the most used extensions before the others,
since the search algorithm searches sequentially through each directory
specified in `ff-search-directories'.  If a file is not found, a new one
is created with the first matching extension (`.cc' yields `.hh')."
  :type '(repeat (list regexp (choice (repeat string) function)))
  :group 'ff)

(defcustom modula2-other-file-alist
  '(
    ("\\.mi$" (".md")) ;; Modula-2 module definition
    ("\\.md$" (".mi")) ;; and implementation.
    )
  "See the description for the `ff-search-directories' variable."
  :type '(repeat (list regexp (choice (repeat string) function)))
  :group 'ff)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; No user definable variables beyond this point!
;; ==============================================

(make-variable-buffer-local 'ff-pre-find-hook)
(make-variable-buffer-local 'ff-pre-load-hook)
(make-variable-buffer-local 'ff-post-load-hook)
(make-variable-buffer-local 'ff-not-found-hook)
(make-variable-buffer-local 'ff-file-created-hook)
(make-variable-buffer-local 'ff-case-fold-search)
(make-variable-buffer-local 'ff-always-in-other-window)
(make-variable-buffer-local 'ff-ignore-include)
(make-variable-buffer-local 'ff-quiet-mode)
(make-variable-buffer-local 'ff-other-file-alist)
(make-variable-buffer-local 'ff-search-directories)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User entry points

;;;###autoload
(defun ff-get-other-file (&optional in-other-window)
  "Find the header or source file corresponding to this file.
See also the documentation for `ff-find-other-file'.

If optional IN-OTHER-WINDOW is non-nil, find the file in another window."
  (interactive "P")
  (let ((ignore ff-ignore-include))
    (setq ff-ignore-include t)
    (ff-find-the-other-file in-other-window)
    (setq ff-ignore-include ignore)))

;;;###autoload
(defalias 'ff-find-related-file 'ff-find-other-file)

;;;###autoload
(defun ff-find-other-file (&optional in-other-window ignore-include)
  "Find the header or source file corresponding to this file.
Being on a `#include' line pulls in that file.

If optional IN-OTHER-WINDOW is non-nil, find the file in the other window.
If optional IGNORE-INCLUDE is non-nil, ignore being on `#include' lines.

Variables of interest include:

 - `ff-case-fold-search'
   Non-nil means ignore cases in matches (see `case-fold-search').
   If you have extensions in different cases, you will want this to be nil.

 - `ff-always-in-other-window'
   If non-nil, always open the other file in another window, unless an
   argument is given to `ff-find-other-file'.

 - `ff-ignore-include'
   If non-nil, ignores #include lines.

 - `ff-always-try-to-create'
   If non-nil, always attempt to create the other file if it was not found.

 - `ff-quiet-mode'
   If non-nil, traces which directories are being searched.

 - `ff-special-constructs'
   A list of regular expressions specifying how to recognize special
   constructs such as include files etc, and an associated method for
   extracting the filename from that construct.

 - `ff-other-file-alist'
   Alist of extensions to find given the current file's extension.

 - `ff-search-directories'
   List of directories searched through with each extension specified in
   `ff-other-file-alist' that matches this file's extension.

 - `ff-pre-find-hook'
   List of functions to be called before the search for the file starts.

 - `ff-pre-load-hook'
   List of functions to be called before the other file is loaded.

 - `ff-post-load-hook'
   List of functions to be called after the other file is loaded.

 - `ff-not-found-hook'
   List of functions to be called if the other file could not be found.

 - `ff-file-created-hook'
   List of functions to be called if the other file has been created."
  (interactive "P")
  (let ((ignore ff-ignore-include))
    (setq ff-ignore-include ignore-include)
    (ff-find-the-other-file in-other-window)
    (setq ff-ignore-include ignore)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Support functions

(defun ff-find-the-other-file (&optional in-other-window)
  "Find the header or source file corresponding to the current file.
Being on a `#include' line pulls in that file, but see the help on
the `ff-ignore-include' variable.

If optional IN-OTHER-WINDOW is non-nil, find the file in another window."

  (let (match           ;; matching regexp for this file
        suffixes        ;; set of replacing regexps for the matching regexp
        action          ;; function to generate the names of the other files
        fname           ;; basename of this file
        pos             ;; where we start matching filenames
        stub            ;; name of the file without extension
        alist           ;; working copy of the list of file extensions
        pathname        ;; the pathname of the file or the #include line
        default-name    ;; file we should create if none found
        format          ;; what we have to match
        found           ;; name of the file or buffer found - nil if none
        dirs            ;; local value of ff-search-directories
        no-match)       ;; whether we know about this kind of file

    (run-hooks 'ff-pre-find-hook 'ff-pre-find-hooks)

    (message "Working...")

    (setq dirs
          (if (symbolp ff-search-directories)
              (ff-list-replace-env-vars (symbol-value ff-search-directories))
            (ff-list-replace-env-vars ff-search-directories)))

    (setq fname (ff-treat-as-special))

    (cond
     ((and (not ff-ignore-include) fname)
      (setq default-name fname)
      (setq found (ff-get-file dirs fname nil in-other-window)))

     ;; let's just get the corresponding file
     (t
      (setq alist (if (symbolp ff-other-file-alist)
                      (symbol-value ff-other-file-alist)
                    ff-other-file-alist)
            pathname (if (buffer-file-name)
                         (buffer-file-name)
                       "/none.none"))

      (setq fname (file-name-nondirectory pathname)
            no-match nil
            match (car alist))

      ;; find the table entry corresponding to this file
      (setq pos (ff-string-match (car match) fname))
      (while (and match (if (and pos (>= pos 0)) nil (not pos)))
        (setq alist (cdr alist))
        (setq match (car alist))
        (setq pos (ff-string-match (car match) fname)))

      ;; no point going on if we haven't found anything
      (if (not match)
          (setq no-match t)

        ;; otherwise, suffixes contains what we need
        (setq suffixes (car (cdr match))
              action (car (cdr match))
              found nil)

        ;; if we have a function to generate new names,
        ;; invoke it with the name of the current file
        (if (and (atom action) (fboundp action))
            (progn
              (setq suffixes (funcall action (buffer-file-name))
                    match (cons (car match) (list suffixes))
                    stub nil
                    default-name (car suffixes)))

          ;; otherwise build our filename stub
          (cond

           ;; get around the problem that 0 and nil both mean false!
           ((= pos 0)
            (setq format "")
            (setq stub "")
            )

           (t
            (setq format (concat "\\(.+\\)" (car match)))
            (string-match format fname)
            (setq stub (substring fname (match-beginning 1) (match-end 1)))
            ))

          ;; if we find nothing, we should try to get a file like this one
          (setq default-name
                (concat stub (car (car (cdr match))))))

        ;; do the real work - find the file
        (setq found
              (ff-get-file dirs
                           stub
                           suffixes
                           in-other-window)))))

    (cond
     (no-match                     ;; could not even determine the other file
      (message ""))

     (t
      (cond

       ((not found)                ;; could not find the other file

	(run-hooks 'ff-not-found-hook 'ff-not-found-hooks)

        (cond
         (ff-always-try-to-create  ;; try to create the file
          (let (name pathname)

            (setq name
                  (expand-file-name
                   (read-directory-name
                    (format "Find or create %s in: " default-name)
                    default-directory default-name nil)))

            (setq pathname
                  (if (file-directory-p name)
                      (concat (file-name-as-directory name) default-name)
                    (setq found name)))

            (ff-find-file pathname in-other-window t)))

         (t                        ;; don't create the file, just whinge
          (message "No file found for %s" fname))))

       (t                          ;; matching file found
        nil))))

    found))                        ;; return buffer-name or filename

(defun ff-other-file-name ()
  "Return name of the header or source file corresponding to the current file.
Being on a `#include' line pulls in that file, but see the help on
the `ff-ignore-include' variable."

  (let (match           ;; matching regexp for this file
        suffixes        ;; set of replacing regexps for the matching regexp
        action          ;; function to generate the names of the other files
        fname           ;; basename of this file
        pos             ;; where we start matching filenames
        stub            ;; name of the file without extension
        alist           ;; working copy of the list of file extensions
        pathname        ;; the pathname of the file or the #include line
        default-name    ;; file we should create if none found
        format          ;; what we have to match
        found           ;; name of the file or buffer found - nil if none
        dirs            ;; local value of ff-search-directories
        no-match)       ;; whether we know about this kind of file

    (message "Working...")

    (setq dirs
          (if (symbolp ff-search-directories)
              (ff-list-replace-env-vars (symbol-value ff-search-directories))
            (ff-list-replace-env-vars ff-search-directories)))

    (setq fname (ff-treat-as-special))

    (cond
     ((and (not ff-ignore-include) fname)
      (setq default-name fname)
      (setq found (ff-get-file-name dirs fname nil)))

     ;; let's just get the corresponding file
     (t
      (setq alist (if (symbolp ff-other-file-alist)
                      (symbol-value ff-other-file-alist)
                    ff-other-file-alist)
            pathname (if (buffer-file-name)
                         (buffer-file-name)
                       "/none.none"))

      (setq fname (file-name-nondirectory pathname)
            no-match nil
            match (car alist))

      ;; find the table entry corresponding to this file
      (setq pos (ff-string-match (car match) fname))
      (while (and match (if (and pos (>= pos 0)) nil (not pos)))
        (setq alist (cdr alist))
        (setq match (car alist))
        (setq pos (ff-string-match (car match) fname)))

      ;; no point going on if we haven't found anything
      (if (not match)
          (setq no-match t)

        ;; otherwise, suffixes contains what we need
        (setq suffixes (car (cdr match))
              action (car (cdr match))
              found nil)

        ;; if we have a function to generate new names,
        ;; invoke it with the name of the current file
        (if (and (atom action) (fboundp action))
            (progn
              (setq suffixes (funcall action (buffer-file-name))
                    match (cons (car match) (list suffixes))
                    stub nil
                    default-name (car suffixes)))

          ;; otherwise build our filename stub
          (cond

           ;; get around the problem that 0 and nil both mean false!
           ((= pos 0)
            (setq format "")
            (setq stub "")
            )

           (t
            (setq format (concat "\\(.+\\)" (car match)))
            (string-match format fname)
            (setq stub (substring fname (match-beginning 1) (match-end 1)))
            ))

          ;; if we find nothing, we should try to get a file like this one
          (setq default-name
                (concat stub (car (car (cdr match))))))

        ;; do the real work - find the file
        (setq found
              (ff-get-file-name dirs stub suffixes)))))
    found))                        ;; return buffer-name or filename

(defun ff-get-file (search-dirs filename &optional suffix-list other-window)
  "Find a file in the SEARCH-DIRS with the given FILENAME (or filename stub).
If (optional) SUFFIX-LIST is nil, search for FILENAME, otherwise search
for FILENAME with each of the given suffixes.  Get the file or the buffer
corresponding to the name of the first file found, or nil."
  (let ((filename (ff-get-file-name search-dirs filename suffix-list)))

    (cond
     ((not filename)
      nil)

     ((bufferp (get-file-buffer filename))
      (ff-switch-to-buffer (get-file-buffer filename) other-window)
      filename)

     ((file-exists-p filename)
      (ff-find-file filename other-window nil)
      filename)

     (t
      nil))))

(defun ff-get-file-name (search-dirs fname-stub &optional suffix-list)
  "Find a file in SEARCH-DIRS with the given name (or stub) FNAME-STUB.
If (optional) SUFFIX-LIST is nil, search for FNAME-STUB, otherwise
search for FNAME-STUB with each of the given suffixes.  Return the
name of the first file found."
  (let (dirs         ;; working copy of dirs to search
	dir          ;; the current dir considered
	file         ;; filename being looked for
	rest         ;; pathname after first /*
	this-suffix  ;; the suffix we are currently considering
	suffixes     ;; working copy of suffix-list
	filename     ;; built filename
	blist        ;; list of live buffers
	buf          ;; current buffer in blist
	found)       ;; whether we have found anything

    (setq suffixes suffix-list)

    ;; suffixes is nil => fname-stub is the file we are looking for
    ;; otherwise fname-stub is a stub, and we append a suffix
    (if suffixes
        (setq this-suffix (car suffixes))
      (setq this-suffix "")
      (setq suffixes (list "")))

    ;; find whether the file is in a buffer first
    (while (and suffixes (not found))
      (setq filename (concat fname-stub this-suffix))

      (if (not ff-quiet-mode)
          (message "Finding buffer %s..." filename))

      (if (bufferp (get-file-buffer filename))
          (setq found (buffer-file-name (get-file-buffer filename))))

      (setq blist (buffer-list))
      (setq buf (buffer-name (car blist)))
      (while (and blist (not found))

        (if (string-match (concat filename "<[0-9]+>") buf)
            (setq found (buffer-file-name (car blist))))

        (setq blist (cdr blist))
        (setq buf (buffer-name (car blist))))

      (setq suffixes (cdr suffixes))
      (setq this-suffix (car suffixes)))

    ;; now look for the real file
    (setq dirs search-dirs)
    (setq dir  (car dirs))
    (while (and (not found) dirs)

      (setq suffixes suffix-list)

      ;; if dir does not contain '/*', look for the file
      (if (and dir (not (string-match "\\([^*]*\\)/\\\*\\(/.*\\)*" dir)))
          (progn

            ;; suffixes is nil => fname-stub is the file we are looking for
            ;; otherwise fname-stub is a stub, and we append a suffix
            (if suffixes
                (setq this-suffix (car suffixes))
              (setq this-suffix "")
              (setq suffixes (list "")))

            (while (and suffixes (not found))

              (setq filename (concat fname-stub this-suffix))
              (setq file (concat dir "/" filename))

              (if (not ff-quiet-mode)
                  (message "Finding %s..." file))

              (if (file-exists-p file)
                  (setq found file))

              (setq suffixes (cdr suffixes))
              (setq this-suffix (car suffixes))))

        ;; otherwise dir matches the '/*', so search each dir separately
        (progn
          (if (match-beginning 2)
              (setq rest (substring dir (match-beginning 2) (match-end 2)))
            (setq rest "")
            )
          (setq dir  (substring dir (match-beginning 1) (match-end 1)))

          (let ((dirlist (ff-all-dirs-under dir '("..")))
                this-dir compl-dirs)

            (setq this-dir (car dirlist))
            (while dirlist
              (setq compl-dirs
                    (append
                     compl-dirs
                     (list (concat this-dir rest))
                     ))
              (setq dirlist  (cdr dirlist))
              (setq this-dir (car dirlist)))

            (if compl-dirs
                (setq found (ff-get-file-name compl-dirs
                                              fname-stub
                                              suffix-list))))))
      (setq dirs (cdr dirs))
      (setq dir (car dirs)))

    (if found
        (message "%s found" found))

    found))

(defun ff-string-match (regexp string &optional start)
  "Like `string-match', but set `case-fold-search' temporarily.
The value used comes from `ff-case-fold-search'."
  (let ((case-fold-search ff-case-fold-search))
    (if regexp
	(string-match regexp string start))))

(defun ff-list-replace-env-vars (search-list)
  "Replace environment variables (of the form $VARIABLE) in SEARCH-LIST."
  (let (list
        (var (car search-list)))
    (while search-list
      (if (string-match "\\(.*\\)\\$[({]*\\([a-zA-Z0-9_]+\\)[)}]*\\(.*\\)" var)
          (setq var
                (concat
                 (substring var (match-beginning 1) (match-end 1))
                 (getenv (substring var (match-beginning 2) (match-end 2)))
                 (substring var (match-beginning 3) (match-end 3)))))
      (setq search-list (cdr search-list))
      (setq list (cons var list))
      (setq var (car search-list)))
    (setq search-list (reverse list))))

(defun ff-treat-as-special ()
  "Return the file to look for if the construct was special, else nil.
See variable `ff-special-constructs'."
  (save-excursion
    (beginning-of-line 1)
    (let* (fname
           (list ff-special-constructs)
           (elem (car list))
           (regexp (car elem))
           (match (cdr elem)))
      (while (and list (not fname))
        (if (and (looking-at regexp) match)
            (setq fname (funcall match)))
        (setq list (cdr list))
        (setq elem (car list))
        (setq regexp (car elem))
        (setq match (cdr elem)))
      fname)))

(defun ff-basename (string)
  "Return the basename of pathname STRING."
  (setq string (concat "/" string))
  (string-match ".*/\\([^/]+\\)$" string)
  (setq string (substring string (match-beginning 1) (match-end 1))))

(defun ff-all-dirs-under (here &optional exclude)
  "Get all the directory files under directory HERE.
Exclude all files in the optional EXCLUDE list."
  (if (file-directory-p here)
      (condition-case nil
          (progn
            (let ((files (directory-files here t))
                  (dirlist (list))
                  file)
              (while files
                (setq file (car files))
                (if (and
                     (file-directory-p file)
                     (not (member (ff-basename file) exclude)))
                    (setq dirlist (cons file dirlist)))
                (setq files (cdr files)))
              (setq dirlist (reverse dirlist))))
        (error nil))
    nil))

(defun ff-switch-file (f1 f2 file &optional in-other-window new-file)
  "Call F1 or F2 on FILE, according to IN-OTHER-WINDOW.
In addition, this runs various hooks.

Either F1 or F2 receives FILE as the sole argument.
The decision of which one to call is based on IN-OTHER-WINDOW
and on the global variable `ff-always-in-other-window'.

F1 and F2 are typically `find-file' / `find-file-other-window'
or `switch-to-buffer' / `switch-to-buffer-other-window' function pairs.

If optional NEW-FILE is t, then a special hook (`ff-file-created-hook') is
called before `ff-post-load-hook'."
  (run-hooks 'ff-pre-load-hook 'ff-pre-load-hooks)
  (if (or
       (and in-other-window (not ff-always-in-other-window))
       (and (not in-other-window) ff-always-in-other-window))
      (funcall f2 file)
    (funcall f1 file))
  (if new-file
      (run-hooks 'ff-file-created-hook 'ff-file-created-hooks))
  (run-hooks 'ff-post-load-hook 'ff-post-load-hooks))

(defun ff-find-file (file &optional in-other-window new-file)
  "Like `find-file', but may show the file in another window."
  (ff-switch-file 'find-file
                  'find-file-other-window
                  file in-other-window new-file))

(defun ff-switch-to-buffer (buffer-or-name &optional in-other-window)
  "Like `switch-to-buffer', but may show the buffer in another window."

  (ff-switch-file 'switch-to-buffer
                  'switch-to-buffer-other-window
                  buffer-or-name in-other-window nil))

;;;###autoload
(defun ff-mouse-find-other-file (event)
  "Visit the file you click on."
  (interactive "e")
  (save-excursion
    (mouse-set-point event)
    (ff-find-other-file nil)))

;;;###autoload
(defun ff-mouse-find-other-file-other-window (event)
  "Visit the file you click on in another window."
  (interactive "e")
  (save-excursion
    (mouse-set-point event)
    (ff-find-other-file t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section offers an example of user defined function to select files

(defun ff-upcase-p (string &optional start end)
  "Return t if STRING is all uppercase.
Given START and/or END, checks between these characters."
  (let (match str)
    (if (not start)
        (setq start 0))
    (if (not end)
        (setq end (length string)))
    (if (= start end)
        (setq end (1+ end)))
    (setq str (substring string start end))
    (if (and
         (ff-string-match "[A-Z]+" str)
         (setq match (match-data))
         (= (car match) 0)
         (= (car (cdr match)) (length str)))
        t
      nil)))

(defun ff-cc-hh-converter (arg)
  "Discriminate file extensions.
Build up a new file list based possibly on part of the directory name
and the name of the file passed in."
  (ff-string-match "\\(.*\\)/\\([^/]+\\)/\\([^.]+\\).\\([^/]+\\)$" arg)
  (let ((path (if (match-beginning 1)
                  (substring arg (match-beginning 1) (match-end 1)) nil))
        (dire (if (match-beginning 2)
                  (substring arg (match-beginning 2) (match-end 2)) nil))
        (file (if (match-beginning 3)
                  (substring arg (match-beginning 3) (match-end 3)) nil))
        (extn (if (match-beginning 4)
                  (substring arg (match-beginning 4) (match-end 4)) nil))
        return-list)
    (cond
     ;; fooZapJunk.cc => ZapJunk.{hh,h} or fooZapJunk.{hh,h}
     ((and (string= extn "cc")
           (ff-string-match "^\\([a-z]+\\)\\([A-Z].+\\)$" file))
      (let ((stub  (substring file (match-beginning 2) (match-end 2))))
        (setq dire (upcase (substring file (match-beginning 1) (match-end 1))))
        (setq return-list (list (concat stub ".hh")
                                (concat stub ".h")
                                (concat file ".hh")
                                (concat file ".h")))
        ))
     ;; FOO/ZapJunk.hh => fooZapJunk.{cc,C} or ZapJunk.{cc,C}
     ((and (string= extn "hh") (ff-upcase-p dire) file)
      (let ((stub (concat (downcase dire) file)))
        (setq return-list (list (concat stub ".cc")
                                (concat stub ".C")
                                (concat file ".cc")
                                (concat file ".C")))
        ))
     ;; zap.cc => zap.hh or zap.h
     ((string= extn "cc")
      (let ((stub file))
        (setq return-list (list (concat stub ".hh")
                                (concat stub ".h")))
        ))
     ;; zap.hh => zap.cc or zap.C
     ((string= extn "hh")
      (let ((stub file))
        (setq return-list (list (concat stub ".cc")
                                (concat stub ".C")))
        ))
     (t
      nil))

    return-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section offers an example of user defined function to place point.
;; The regexps are Ada specific.

(defvar ff-function-name nil "Name of the function we are in.")

;; bind with (setq ff-pre-load-hook 'ff-which-function-are-we-in)
;;
(defvar ada-procedure-start-regexp)
(defvar ada-package-start-regexp)

(defun ff-which-function-are-we-in ()
  "Return the name of the function whose definition/declaration point is in.
Also remember that name in `ff-function-name'."
  (setq ff-function-name
        (save-excursion
          (if (or (re-search-backward ada-procedure-start-regexp nil t)
                  (re-search-backward ada-package-start-regexp nil t))
              (match-string 0)))))

;; bind with (setq ff-post-load-hook 'ff-set-point-accordingly)
;;
(defun ff-set-point-accordingly ()
  "Find the function specified in `ff-function-name'.
That name was previously determined by `ff-which-function-are-we-in'."
  (if ff-function-name
      (progn
        (goto-char (point-min))
        (search-forward ff-function-name nil t))))

(provide 'find-file)

;;; find-file.el ends here
