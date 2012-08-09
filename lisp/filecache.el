;;; filecache.el --- find files using a pre-loaded cache

;; Copyright (C) 1996, 2000-2012  Free Software Foundation, Inc.

;; Author:  Peter Breton <pbreton@cs.umb.edu>
;; Created: Sun Nov 10 1996
;; Keywords: convenience

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
;; The file-cache package is an attempt to make it easy to locate files
;; by name, without having to remember exactly where they are located.
;; This is very handy when working with source trees. You can also add
;; frequently used files to the cache to create a hotlist effect.
;; The cache can be used with any interactive command which takes a
;; filename as an argument.
;;
;; It is worth noting that this package works best when most of the files
;; in the cache have unique names, or (if they have the same name) exist in
;; only a few directories. The worst case is many files all with
;; the same name and in different directories, for example a big source tree
;; with a Makefile in each directory. In such a case, you should probably
;; use an alternate strategy to find the files.
;;
;; ADDING FILES TO THE CACHE:
;;
;; Use the following functions to add items to the file cache:
;;
;;   * `file-cache-add-file': Adds a single file to the cache
;;
;;   * `file-cache-add-file-list': Adds a list of files to the cache
;;
;; The following functions use the regular expressions in
;; `file-cache-delete-regexps' to eliminate unwanted files:
;;
;;   * `file-cache-add-directory': Adds the files in a directory to the
;;     cache. You can also specify a regular expression to match the files
;;     which should be added.
;;
;;   * `file-cache-add-directory-list': Same as above, but acts on a list
;;     of directories. You can use `load-path', `exec-path' and the like.
;;
;;   * `file-cache-add-directory-using-find': Uses the `find' command to
;;     add a directory tree to the cache.
;;
;;   * `file-cache-add-directory-using-locate': Uses the `locate' command to
;;     add files matching a pattern to the cache.
;;
;;   * `file-cache-add-directory-recursively': Uses the find-lisp package to
;;     add all files matching a pattern to the cache.
;;
;; Use the function `file-cache-clear-cache' to remove all items from the
;; cache. There are a number of `file-cache-delete' functions provided
;; as well, but in general it is probably better to not worry too much
;; about extra files in the cache.
;;
;; The most convenient way to initialize the cache is with an
;; `eval-after-load' function, as noted in the ADDING FILES
;; AUTOMATICALLY section.
;;
;; FINDING FILES USING THE CACHE:
;;
;; You can use the file-cache with any function that expects a filename as
;; an argument. For example:
;;
;; 1) Invoke a function which expects a filename as an argument:
;;    M-x find-file
;;
;; 2) Begin typing a file name.
;;
;; 3) Invoke `file-cache-minibuffer-complete' (bound by default to
;; C-TAB) to complete on the filename using the cache.
;;
;; 4) When you have found a unique completion, the minibuffer contents
;; will change to the full name of that file.
;;
;; If there are a number of directories which contain the completion,
;; invoking `file-cache-minibuffer-complete' repeatedly will cycle through
;; them.
;;
;; 5) You can then edit the minibuffer contents, or press RETURN.
;;
;; It is much easier to simply try it than trying to explain it :)
;;
;;; ADDING FILES AUTOMATICALLY
;;
;; For maximum utility, you should probably define an `eval-after-load'
;; form which loads your favorite files:
;;
;;      (eval-after-load
;;       "filecache"
;;       '(progn
;; 	    (message "Loading file cache...")
;; 	    (file-cache-add-directory-using-find "~/projects")
;;	    (file-cache-add-directory-list load-path)
;;	    (file-cache-add-directory "~/")
;;	    (file-cache-add-file-list (list "~/foo/bar" "~/baz/bar"))
;; 	   ))
;;
;; If you clear and reload the cache frequently, it is probably easiest
;; to put your initializations in a function:
;;
;;   (eval-after-load
;;     "filecache"
;;      '(my-file-cache-initialize))
;;
;;   (defun my-file-cache-initialize ()
;;      (interactive)
;; 	(message "Loading file cache...")
;; 	(file-cache-add-directory-using-find "~/projects")
;;	(file-cache-add-directory-list load-path)
;;	(file-cache-add-directory "~/")
;;	(file-cache-add-file-list (list "~/foo/bar" "~/baz/bar"))
;;   ))
;;
;; Of course, you can still add files to the cache afterwards, via
;; Lisp functions.
;;
;; RELATED WORK:
;;
;; This package is a distant relative of Noah Friedman's fff utilities.
;; Our goal is pretty similar, but the implementation strategies are
;; different.

;;; Code:

(eval-when-compile
  (require 'find-lisp))

(defgroup file-cache nil
  "Find files using a pre-loaded cache."
  :group 'files
  :group 'convenience
  :prefix "file-cache-")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; User-modifiable variables
(defcustom file-cache-filter-regexps
  ;; These are also used in buffers containing lines of file names,
  ;; so the end-of-name is matched with $ rather than \\'.
  (list "~$" "\\.o$" "\\.exe$" "\\.a$" "\\.elc$" ",v$" "\\.output$"
	"\\.$" "#$" "\\.class$")
  "List of regular expressions used as filters by the file cache.
File names which match these expressions will not be added to the cache.
Note that the functions `file-cache-add-file' and `file-cache-add-file-list'
do not use this variable."
  :type '(repeat regexp)
  :group 'file-cache)

(defcustom file-cache-find-command "find"
  "External program used by `file-cache-add-directory-using-find'."
  :type 'string
  :group 'file-cache)

(defcustom file-cache-find-command-posix-flag 'not-defined
  "Set to t, if `file-cache-find-command' handles wildcards POSIX style.
This variable is automatically set to nil or non-nil
if it has the initial value `not-defined' whenever you first
call the `file-cache-add-directory-using-find'.

Under Windows operating system where Cygwin is available, this value
should be t."
  :type  '(choice (const :tag "Yes" t)
		  (const :tag "No" nil)
		  (const :tag "Unknown" not-defined))
  :group 'file-cache)

(defcustom file-cache-locate-command "locate"
  "External program used by `file-cache-add-directory-using-locate'."
  :type 'string
  :group 'file-cache)

;; Minibuffer messages
(defcustom file-cache-no-match-message " [File Cache: No match]"
  "Message to display when there is no completion."
  :type 'string
  :group 'file-cache)

(defcustom file-cache-sole-match-message " [File Cache: sole completion]"
  "Message to display when there is only one completion."
  :type 'string
  :group 'file-cache)

(defcustom file-cache-non-unique-message
  " [File Cache: complete but not unique]"
  "Message to display when there is a non-unique completion."
  :type 'string
  :group 'file-cache)

(defcustom file-cache-completion-ignore-case
  (if (memq system-type '(ms-dos windows-nt cygwin))
      t
     completion-ignore-case)
  "If non-nil, file-cache completion should ignore case.
Defaults to the value of `completion-ignore-case'."
  :type 'boolean
  :group 'file-cache)

(defcustom file-cache-case-fold-search
  (if (memq system-type '(ms-dos windows-nt cygwin))
      t
    case-fold-search)
  "If non-nil, file-cache completion should ignore case.
Defaults to the value of `case-fold-search'."
  :type 'boolean
  :group 'file-cache)

(defcustom file-cache-ignore-case
  (memq system-type '(ms-dos windows-nt cygwin))
  "Non-nil means ignore case when checking completions in the file cache.
Defaults to nil on DOS and Windows, and t on other systems."
  :type 'boolean
  :group 'file-cache)

(defvar file-cache-multiple-directory-message nil)

;; Internal variables
;; This should be named *Completions* because that's what the function
;; switch-to-completions in simple.el expects
(defcustom file-cache-completions-buffer "*Completions*"
  "Buffer to display completions when using the file cache."
  :type 'string
  :group 'file-cache)

(defcustom file-cache-buffer "*File Cache*"
  "Buffer to hold the cache of file names."
  :type 'string
  :group 'file-cache)

(defcustom file-cache-buffer-default-regexp "^.+$"
  "Regexp to match files in `file-cache-buffer'."
  :type 'regexp
  :group 'file-cache)

(defvar file-cache-last-completion nil)

(defvar file-cache-alist nil
  "Internal data structure to hold cache of file names.
It is a list of entries of the form (FILENAME DIRNAME1 DIRNAME2 ...)
where FILENAME is a file name component and the entry represents N
files of names DIRNAME1/FILENAME, DIRNAME2/FILENAME, ...")

(defvar file-cache-completions-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map completion-list-mode-map)
    (define-key map [mouse-2] 'file-cache-choose-completion)
    (define-key map "\C-m" 'file-cache-choose-completion)
    map)
  "Keymap for file cache completions buffer.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions to add files to the cache
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun file-cache-add-directory (directory &optional regexp)
  "Add DIRECTORY to the file cache.
If the optional REGEXP argument is non-nil, only files which match it will
be added to the cache."
  (interactive "DAdd files from directory: ")
  ;; Not an error, because otherwise we can't use load-paths that
  ;; contain non-existent directories.
  (if (not (file-accessible-directory-p directory))
      (message "Directory %s does not exist" directory)
    (let* ((dir       (expand-file-name directory))
	   (dir-files (directory-files dir t regexp)))
      ;; Filter out files we don't want to see
      (dolist (file dir-files)
        (if (file-directory-p file)
            (setq dir-files (delq file dir-files))
          (dolist (regexp file-cache-filter-regexps)
            (if (string-match regexp file)
                (setq dir-files (delq file dir-files))))))
      (file-cache-add-file-list dir-files))))

;;;###autoload
(defun file-cache-add-directory-list (directory-list &optional regexp)
  "Add DIRECTORY-LIST (a list of directory names) to the file cache.
If the optional REGEXP argument is non-nil, only files which match it
will be added to the cache.  Note that the REGEXP is applied to the
files in each directory, not to the directory list itself."
  (interactive "XAdd files from directory list: ")
  (mapcar
   (lambda (dir) (file-cache-add-directory dir regexp))
   directory-list))

(defun file-cache-add-file-list  (file-list)
  "Add FILE-LIST (a list of files names) to the file cache."
  (interactive "XFile List: ")
  (mapcar 'file-cache-add-file file-list))

;; Workhorse function

;;;###autoload
(defun file-cache-add-file (file)
  "Add FILE to the file cache."
  (interactive "fAdd File: ")
  (if (not (file-exists-p file))
      (message "Filecache: file %s does not exist" file)
    (let* ((file-name (file-name-nondirectory file))
	   (dir-name  (file-name-directory    file))
	   (the-entry (assoc-string
		       file-name file-cache-alist
		       file-cache-ignore-case)))
      ;; Does the entry exist already?
      (if the-entry
	  (if (or (and (stringp (cdr the-entry))
		       (string= dir-name (cdr the-entry)))
		  (and (listp (cdr the-entry))
		       (member dir-name (cdr the-entry))))
	      nil
	    (setcdr the-entry (cons dir-name (cdr the-entry))))
	;; If not, add it to the cache
	(push (list file-name dir-name) file-cache-alist)))))

;;;###autoload
(defun file-cache-add-directory-using-find (directory)
  "Use the `find' command to add files to the file cache.
Find is run in DIRECTORY."
  (interactive "DAdd files under directory: ")
  (let ((dir (expand-file-name directory)))
    (when (memq system-type '(windows-nt cygwin))
      (if (eq file-cache-find-command-posix-flag 'not-defined)
	  (setq file-cache-find-command-posix-flag
		(executable-command-find-posix-p file-cache-find-command))))
    (set-buffer (get-buffer-create file-cache-buffer))
    (erase-buffer)
    (call-process file-cache-find-command nil
		  (get-buffer file-cache-buffer) nil
		  dir "-name"
		  (if (memq system-type '(windows-nt cygwin))
		      (if file-cache-find-command-posix-flag
			  "\\*"
			"'*'")
		    "*")
		  "-print")
    (file-cache-add-from-file-cache-buffer)))

;;;###autoload
(defun file-cache-add-directory-using-locate (string)
  "Use the `locate' command to add files to the file cache.
STRING is passed as an argument to the locate command."
  (interactive "sAdd files using locate string: ")
  (set-buffer (get-buffer-create file-cache-buffer))
  (erase-buffer)
  (call-process file-cache-locate-command nil
		(get-buffer file-cache-buffer) nil
		string)
  (file-cache-add-from-file-cache-buffer))

;;;###autoload
(defun file-cache-add-directory-recursively  (dir &optional regexp)
  "Adds DIR and any subdirectories to the file-cache.
This function does not use any external programs.
If the optional REGEXP argument is non-nil, only files which match it
will be added to the cache.  Note that the REGEXP is applied to the
files in each directory, not to the directory list itself."
  (interactive "DAdd directory: ")
  (require 'find-lisp)
  (mapcar
   (function
    (lambda (file)
      (or (file-directory-p file)
	  (let (filtered)
	    (dolist (regexp file-cache-filter-regexps)
              (and (string-match regexp file)
                   (setq filtered t)))
            filtered)
	  (file-cache-add-file file))))
   (find-lisp-find-files dir (if regexp regexp "^"))))

(defun file-cache-add-from-file-cache-buffer (&optional regexp)
  "Add any entries found in the file cache buffer.
Each entry matches the regular expression `file-cache-buffer-default-regexp'
or the optional REGEXP argument."
  (set-buffer file-cache-buffer)
  (dolist (elt file-cache-filter-regexps)
    (goto-char (point-min))
    (delete-matching-lines elt))
  (goto-char (point-min))
  (let ((full-filename))
    (while (re-search-forward
	    (or regexp file-cache-buffer-default-regexp)
	    (point-max) t)
      (setq full-filename (buffer-substring-no-properties
		      (match-beginning 0) (match-end 0)))
      (file-cache-add-file full-filename))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions to delete from the cache
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun file-cache-clear-cache ()
  "Clear the file cache."
  (interactive)
  (setq file-cache-alist nil))

;; This clears *all* files with the given name
(defun file-cache-delete-file (file)
  "Delete FILE from the file cache."
  (interactive
   (list (completing-read "Delete file from cache: " file-cache-alist)))
  (setq file-cache-alist
	(delq (assoc-string file file-cache-alist file-cache-ignore-case)
	      file-cache-alist)))

(defun file-cache-delete-file-list (file-list)
  "Delete FILE-LIST (a list of files) from the file cache."
  (interactive "XFile List: ")
  (mapcar 'file-cache-delete-file file-list))

(defun file-cache-delete-file-regexp (regexp)
  "Delete files matching REGEXP from the file cache."
  (interactive "sRegexp: ")
  (let ((delete-list))
    (dolist (elt file-cache-alist)
      (and (string-match regexp (car elt))
           (push (car elt) delete-list)))
    (file-cache-delete-file-list delete-list)
    (message "Filecache: deleted %d files from file cache"
             (length delete-list))))

(defun file-cache-delete-directory (directory)
  "Delete DIRECTORY from the file cache."
  (interactive "DDelete directory from file cache: ")
  (let ((dir (expand-file-name directory))
	(result 0))
    (dolist (entry file-cache-alist)
      (if (file-cache-do-delete-directory dir entry)
          (setq result (1+ result))))
    (if (zerop result)
	(error "Filecache: no entries containing %s found in cache" directory)
      (message "Filecache: deleted %d entries" result))))

(defun file-cache-do-delete-directory (dir entry)
  (let ((directory-list (cdr entry))
	(directory (file-cache-canonical-directory dir)))
    (and (member directory directory-list)
	 (if (equal 1 (length directory-list))
	     (setq file-cache-alist
		   (delq entry file-cache-alist))
	   (setcdr entry (delete directory directory-list))))))

(defun file-cache-delete-directory-list (directory-list)
  "Delete DIRECTORY-LIST (a list of directories) from the file cache."
  (interactive "XDirectory List: ")
  (mapcar 'file-cache-delete-directory directory-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Returns the name of a directory for a file in the cache
(defun file-cache-directory-name  (file)
  (let* ((directory-list (cdr (assoc-string
			       file file-cache-alist
			       file-cache-ignore-case)))
	 (len            (length directory-list))
	 (directory)
	 (num))
    (if (not (listp directory-list))
	(error "Filecache: unknown type in file-cache-alist for key %s" file))
    (cond
     ;; Single element
     ((eq 1 len)
      (setq directory (elt directory-list 0)))
     ;; No elements
     ((eq 0 len)
      (error "Filecache: no directory found for key %s" file))
     ;; Multiple elements
     (t
      (let* ((minibuffer-dir (file-name-directory (minibuffer-contents)))
	     (dir-list       (member minibuffer-dir directory-list)))
	(setq directory
	      ;; If the directory is in the list, return the next element
	      ;; Otherwise, return the first element
	      (if dir-list
		  (or (elt directory-list
			   (setq num (1+ (- len (length dir-list)))))
		      (elt directory-list (setq num 0)))
		(elt directory-list (setq num 0)))))))
    ;; If there were multiple directories, set up a minibuffer message
    (setq file-cache-multiple-directory-message
	  (and num (format " [%d of %d]" (1+ num) len)))
    directory))

;; Returns the name of a file in the cache
(defun file-cache-file-name  (file)
  (let ((directory (file-cache-directory-name file)))
    (concat directory file)))

;; Return a canonical directory for comparison purposes.
;; Such a directory ends with a forward slash.
(defun file-cache-canonical-directory (dir)
  (let ((directory dir))
    (if (not (char-equal ?/ (string-to-char (substring directory -1))))
	(concat directory "/")
      directory)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minibuffer functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The prefix argument works around a bug in the minibuffer completion.
;; The completion function doesn't distinguish between the states:
;;
;;   "Multiple completions of name" (eg, Makefile, Makefile.in)
;;   "Name available in multiple directories" (/tmp/Makefile, ~me/Makefile)
;;
;; The default is to do the former; a prefix arg forces the latter.

;;;###autoload
(defun file-cache-minibuffer-complete (arg)
  "Complete a filename in the minibuffer using a preloaded cache.
Filecache does two kinds of substitution: it completes on names in
the cache, and, once it has found a unique name, it cycles through
the directories that the name is available in.  With a prefix argument,
the name is considered already unique; only the second substitution
\(directories) is done."
  (interactive "P")
  (let*
      (
       (completion-ignore-case file-cache-completion-ignore-case)
       (case-fold-search       file-cache-case-fold-search)
       (string                 (file-name-nondirectory (minibuffer-contents)))
       (completion-string      (try-completion string file-cache-alist))
       (completion-list)
       (len)
       (file-cache-string))
    (cond
     ;; If it's the only match, replace the original contents
     ((or arg (eq completion-string t))
      (setq file-cache-string (file-cache-file-name string))
      (if (string= file-cache-string (minibuffer-contents))
	  (minibuffer-message file-cache-sole-match-message)
	(delete-minibuffer-contents)
	(insert file-cache-string)
	(if file-cache-multiple-directory-message
	    (minibuffer-message file-cache-multiple-directory-message))))

     ;; If it's the longest match, insert it
     ((stringp completion-string)
      ;; If we've already inserted a unique string, see if the user
      ;; wants to use that one
      (if (and (string= string completion-string)
	       (assoc-string string file-cache-alist
			     file-cache-ignore-case))
	  (if (and (eq last-command this-command)
		   (string= file-cache-last-completion completion-string))
	      (progn
		(delete-minibuffer-contents)
		(insert (file-cache-file-name completion-string))
		(setq file-cache-last-completion nil))
	    (minibuffer-message file-cache-non-unique-message)
	    (setq file-cache-last-completion string))
	(setq file-cache-last-completion string)
	(setq completion-list (all-completions string file-cache-alist)
	      len             (length completion-list))
	(if (> len 1)
	    (progn
	      (goto-char (point-max))
	      (insert
	       (substring completion-string (length string)))
	      ;; Add our own setup function to the Completions Buffer
	      (let ((completion-setup-hook
                     (append completion-setup-hook
                             (list 'file-cache-completion-setup-function))))
		(with-output-to-temp-buffer file-cache-completions-buffer
		  (display-completion-list completion-list string))))
	  (setq file-cache-string (file-cache-file-name completion-string))
	  (if (string= file-cache-string (minibuffer-contents))
	      (minibuffer-message file-cache-sole-match-message)
	    (delete-minibuffer-contents)
	    (insert file-cache-string)
	    (if file-cache-multiple-directory-message
		(minibuffer-message file-cache-multiple-directory-message)))
	  )))

     ;; No match
     ((eq completion-string nil)
      (minibuffer-message file-cache-no-match-message)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun file-cache-completion-setup-function  ()
  (with-current-buffer standard-output ;; i.e. file-cache-completions-buffer
    (use-local-map file-cache-completions-keymap)))

(defun file-cache-choose-completion (&optional event)
  "Choose a completion in the `*Completions*' buffer."
  (interactive (list last-nonmenu-event))
  (let ((completion-no-auto-exit t))
    (choose-completion event)
    (select-window (active-minibuffer-window))
    (file-cache-minibuffer-complete nil)))

(define-obsolete-function-alias 'file-cache-mouse-choose-completion
  'file-cache-choose-completion "23.2")

(defun file-cache-complete  ()
  "Complete the word at point, using the filecache."
  (interactive)
  (let ((start
    (save-excursion
      (skip-syntax-backward "^\"")
           (point))))
    (completion-in-region start (point) file-cache-alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show parts of the cache
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun file-cache-files-matching-internal (regexp)
  "Output a list of files whose names (not including directories)
match REGEXP."
  (let ((results))
    (dolist (cache-element file-cache-alist)
      (and (string-match regexp (elt cache-element 0))
           (push (elt cache-element 0) results)))
    (nreverse results)))

(defun file-cache-files-matching (regexp)
  "Output a list of files whose names (not including directories)
match REGEXP."
  (interactive "sFind files matching regexp: ")
  (let ((results
	 (file-cache-files-matching-internal regexp))
	buf)
    (set-buffer
     (setq buf (get-buffer-create
		"*File Cache Files Matching*")))
    (erase-buffer)
    (insert
     (mapconcat
      'identity
      results
      "\n"))
    (goto-char (point-min))
    (display-buffer buf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debugging functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun file-cache-debug-read-from-minibuffer (file)
  "Debugging function."
  (interactive
   (list (completing-read "File Cache: " file-cache-alist)))
  (message "%s" (assoc-string file file-cache-alist
			      file-cache-ignore-case)))

(defun file-cache-display  ()
  "Display the file cache."
  (interactive)
  (let ((buf "*File Cache Contents*"))
    (with-current-buffer
	(get-buffer-create buf)
      (erase-buffer)
      (dolist (item file-cache-alist)
        (insert (nth 1 item) (nth 0 item) "\n"))
      (pop-to-buffer buf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'filecache)

;;; filecache.el ends here
