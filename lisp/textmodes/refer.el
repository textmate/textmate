;;; refer.el --- look up references in bibliography files

;; Copyright (C) 1992, 1996, 2001-2012  Free Software Foundation, Inc.

;; Author: Ashwin Ram <ashwin@cc.gatech.edu>
;; Maintainer: Gernot Heiser <gernot@acm.org>
;; Adapted-By: ESR
;; Keywords: bib

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

;; Functions to look up references in bibliography files given lists of
;; keywords, similar to refer(1).  I don't use tags since tags on .bib files
;; only picks up the cite key, where as refer-find-entry looks for occurrences
;; of keywords anywhere in the bibliography entry.
;;
;; To use:
;;      (autoload 'refer-find-entry "refer" nil t)
;; or   (require 'refer)
;;
;;      To look for an article by Knuth about semaphores:
;;          Invoke refer-find-entry, then in response to the Keywords: prompt,
;;          say: Knuth semaphores (a blank-separated list of keywords to be used
;;          as search strings).
;;
;;      To continue the previous search, i.e., to search for the next occurrence
;;      of the keywords, use refer-find-next-entry, or invoke refer-find-entry
;;      with a prefix argument.
;;
;;	Once you've found the entry you want to reference, invoke
;;	refer-yank-key to insert it at point in the current buffer
;;	(typically as the argument of a \cite{} command).
;;
;;	I use (define-key tex-mode-map    "\C-c\C-y"  'refer-yank-key)
;;	to bind this often-used function to a key in (la)tex-mode.
;;
;;      If the list of bibliography files changes, reinitialize the variable
;;      refer-bib-files.
;;
;; To customize:
;;      See variables refer-bib-files, refer-cache-bib-files and
;;      refer-bib-files-regexp.  By default, these are set up so that refer
;;      looks for the keywords you specify in all the .bib files in the current
;;      directory.
;;
;;      The only assumption I make about bib files is that they contain a bunch
;;      of entries, one to a paragraph.  refer-find-entry searches paragraph by
;;      paragraph, looking for a paragraph containing all the keywords
;;      specified.  So you should be able to use pretty much any bib file with
;;      this code.  If your bib file does not use paragraphs to separate
;;      entries, try setting the paragraph-start/separate variables, or changing
;;      the (forward-paragraph 1) call in refer-find-entry-in-file.

;;; Code:

(provide 'refer)

(defgroup refer nil
  "Look up references in bibliography files."
  :prefix "refer-"
  :group 'wp)

(defcustom refer-bib-directory nil
  "Directory, or list of directories, to search for \\.bib files.
Can be set to 'bibinputs or 'texinputs, in which case the environment
variable BIBINPUTS or TEXINPUTS, respectively, is used to obtain a
list of directories.  Useful only if `refer-bib-files' is set to 'dir or
a list of file names (without directory).  A value of nil indicates the
current working directory.

If `refer-bib-directory' is 'bibinputs or 'texinputs, it is setq'd to
the appropriate list of directories when it is first used.

Note that an empty directory is interpreted by BibTeX as indicating
the default search path.  Since Refer does not know that default path,
it cannot search it.  Include that path explicitly in your BIBINPUTS
environment if you really want it searched (which is not likely to
happen anyway)."
  :type '(choice (repeat directory) (const bibinputs) (const texinputs))
  :group 'refer)

(defcustom refer-bib-files 'dir
  "List of \\.bib files to search for references,
or one of the following special values:
nil  = prompt for \\.bib file (if visiting a \\.bib file, use it as default)
auto = read \\.bib file names from appropriate command in buffer (see
       `refer-bib-files-regexp') unless the buffer's mode is `bibtex-mode',
       in which case only the buffer is searched
dir  = use all \\.bib files in directories referenced by `refer-bib-directory'.

If a specified file doesn't exist and has no extension, a \\.bib extension
is automatically tried.

If `refer-bib-files' is nil, auto or dir, it is setq'd to the appropriate
list of files when it is first used if `refer-cache-bib-files' is t.  If
`refer-cache-bib-files' is nil, the list of \\.bib files to use is re-read
each time it is needed."
  :type '(choice (repeat file) (const nil) (const auto) (const dir))
  :group 'refer)

(defcustom refer-cache-bib-files t
  "Variable determining whether the value of `refer-bib-files' should be cached.
If t, initialize the value of refer-bib-files the first time it is used.  If
nil, re-read the list of \\.bib files depending on the value of `refer-bib-files'
each time it is needed."
  :type 'boolean
  :group 'refer)

(defcustom refer-bib-files-regexp "\\\\bibliography"
  "Regexp matching a bibliography file declaration.
The current buffer is expected to contain a line such as
\\bibliography{file1,file2,file3}
which is read to set up `refer-bib-files'.  The regexp must specify the command
\(such as \\bibliography) that is used to specify the list of bib files.  The
command is expected to specify a file name, or a list of comma-separated file
names, within curly braces.
If a specified file doesn't exist and has no extension, a \\.bib extension
is automatically tried."
  :type 'regexp
  :group 'refer)

(make-variable-buffer-local 'refer-bib-files)
(make-variable-buffer-local 'refer-cache-bib-files)
(make-variable-buffer-local 'refer-bib-directory)

;;; Internal variables
(defvar refer-saved-state nil)
(defvar refer-previous-keywords nil)
(defvar refer-saved-pos nil)
(defvar refer-same-file nil)

(defun refer-find-entry (keywords &optional continue)
   "Find entry in refer-bib-files containing KEYWORDS.
If KEYWORDS is nil, prompt user for blank-separated list of keywords.
If CONTINUE is non-nil, or if called interactively with a prefix arg,
look for next entry by continuing search from previous point."
   (interactive (list nil current-prefix-arg))
   (or keywords (setq keywords (if continue
                                   refer-previous-keywords
                                 (read-string "Keywords: "))))
   (setq refer-previous-keywords keywords)
   (refer-find-entry-internal keywords continue))

(defun refer-find-next-entry ()
   "Find next occurrence of entry in `refer-bib-files'.  See `refer-find-entry'."
   (interactive)
   (refer-find-entry-internal refer-previous-keywords t))

(defun refer-yank-key ()
  "Inserts at point in current buffer the \"key\" field of the entry
found on the last `refer-find-entry' or `refer-find-next-entry'."
  (interactive)
  (let ((old-point (point)))
    (insert
     (save-window-excursion
       (save-excursion
         (find-file (car refer-saved-state))
         (if (looking-at
              "[ \t\n]*@\\s-*[a-zA-Z][a-zA-Z0-9]*\\s-*{\\s-*\\([^ \t\n,]+\\)\\s-*,")
             (buffer-substring (match-beginning 1) (match-end 1))
           (error "Cannot find key for entry in file %s"
                  (car refer-saved-state))))))
    (if (not (= (point) old-point))
      (set-mark old-point))))

(defun refer-find-entry-internal (keywords continue)
   (let ((keywords-list (refer-convert-string-to-list-of-strings keywords))
         (old-buffer (current-buffer))
         (old-window (selected-window))
         (new-window (selected-window))
         (files (if continue
                    refer-saved-state
                  (setq refer-saved-pos nil)
                  (refer-get-bib-files)))
         (n 0)
         (found nil)
         (file nil))
     ;; find window in which to display bibliography file.
     ;; if a bibliography file is already displayed in a window, use
     ;; that one, otherwise use any window other than the current one
     (setq new-window
	   (get-window-with-predicate
	    (lambda (w)
	      (while (and (not (null (setq file (nth n files))))
			  (setq n (1+ n))
			  (not (string-equal file
					     (buffer-file-name
					      (window-buffer w))))))
	      file)))
     (unless new-window
       ;; didn't find bib file in any window:
       (when (one-window-p 'nomini)
	 (setq old-window (split-window)))
       (setq new-window (next-window old-window 'nomini)))
     (select-window (if refer-same-file
                        old-window
                      new-window))  ; the window in which to show the bib file
     (catch 'found
       (while files
         (let ((file (cond ((file-exists-p (car files)) (car files))
                           ((file-exists-p (concat (car files) ".bib"))
                            (concat (car files) ".bib")))))
           (setq refer-saved-state files)
           (if file
               (if (refer-find-entry-in-file keywords-list file refer-saved-pos)
                   (progn
                     (setq refer-saved-pos (point))
                     (recenter 0)
                     (throw 'found (find-file file)))
                 (setq refer-saved-pos nil
                       files (cdr files)))
             (progn (ding)
		    (message "Scanning %s... No such file" (car files))
                    (sit-for 1)
                    (setq files (cdr files))))))
       (ding)
       (message "Keywords \"%s\" not found in any \.bib file" keywords))
     (select-window old-window)))

(defun refer-find-entry-in-file (keywords-list file &optional old-pos)
   (message "Scanning %s..." file)
   (expand-file-name file)
   (set-buffer (find-file-noselect file))
   (find-file file)
   (if (not old-pos)
       (goto-char (point-min))
     (goto-char old-pos)
     (forward-paragraph 1))
   (let ((begin (point))
         (end 0)
         (found nil))
     (while (and (not found)
                 (not (eobp)))
       (forward-paragraph 1)
       (setq end (point))
       (setq found
             (refer-every (function (lambda (keyword)
                                (goto-char begin)
                                (re-search-forward keyword end t)))
                    keywords-list))
       (if (not found)
           (progn
             (setq begin end)
             (goto-char begin))))
     (if found
         (progn (goto-char begin)
                (re-search-forward "\\W" nil t)
                (message "Scanning %s... found" file))
       (progn (message "Scanning %s... not found" file)
              nil))))

(defun refer-every (pred l)
  (cond ((null l) nil)
	((funcall pred (car l))
	 (or (null (cdr l))
	     (refer-every pred (cdr l))))))

(defun refer-convert-string-to-list-of-strings (s)
   (let ((current (current-buffer))
         (temp-buffer (get-buffer-create "*refer-temp*")))
      (set-buffer temp-buffer)
      (erase-buffer)
      (insert (regexp-quote s))
      (goto-char (point-min))
      (insert "(\"")
      (while (re-search-forward "[ \t]+" nil t)
         (replace-match "\" \"" t t))
      (goto-char (point-max))
      (insert "\")")
      (goto-char (point-min))
      (prog1 (read temp-buffer)
         (set-buffer current))))

(defun refer-expand-files (file-list dir-list)
  (let (file files dir dirs)
    (while (setq file (car file-list))
      (setq dirs (copy-alist dir-list))
      (while (setq dir (car dirs))
        (if (file-exists-p (expand-file-name file dir))
            (setq files (append files (list (expand-file-name file dir)))
                  dirs  nil)
          (if (file-exists-p (expand-file-name (concat file ".bib") dir))
              (setq files (append files (list (expand-file-name (concat file ".bib")
                                                                dir)))
                    dirs  nil)
            (setq dirs (cdr dirs)))))
      (setq file-list (cdr file-list)))
    files))

(defun refer-get-bib-files ()
  (let* ((dir-list
          (cond
           ((null refer-bib-directory)
            '("."))
           ((or (eq refer-bib-directory 'texinputs)
                (eq refer-bib-directory 'bibinputs))
            (let ((envvar (getenv (if (eq refer-bib-directory 'texinputs)
                                      "TEXINPUTS"
                                    "BIBINPUTS")))
                  (dirs nil))
              (if (null envvar)
                  (setq envvar "."))
              (while (string-match ":" envvar)
		(let ((dir (substring envvar 0 (match-beginning 0))))
		  (if (and (not (string-equal "" dir))
                           (file-directory-p dir))
		      (setq dirs (append (list (expand-file-name dir nil))
                                         dirs))))
                (setq envvar (substring envvar (match-end 0))))
	      (if (and (not (string-equal "" envvar))
                       (file-directory-p envvar))
		  (setq dirs (append (list envvar) dirs)))
              (setq dirs (nreverse dirs))))
	   ((listp refer-bib-directory)
	    refer-bib-directory)
           (t
            (list refer-bib-directory))))
         (files
           (cond
            ((null refer-bib-files)
             (list (expand-file-name
                    (if (eq major-mode 'bibtex-mode)
                        (read-file-name
                         (format ".bib file (default %s): "
                                 (file-name-nondirectory
                                  (buffer-file-name)))
                         (file-name-directory (buffer-file-name))
                         (file-name-nondirectory (buffer-file-name))
                         t)
                      (read-file-name ".bib file: " nil nil t)))))
            ((eq refer-bib-files 'auto)
             (let ((files
                    (save-excursion
                      (if (setq refer-same-file (eq major-mode 'bibtex-mode))
                          (list buffer-file-name)
                        (if (progn
                              (goto-char (point-min))
                              (re-search-forward (concat refer-bib-files-regexp
                                                         "\\s-*\{") nil t))
                            (let ((files (list (buffer-substring
                                                (point)
                                                (progn
                                                  (re-search-forward "[,\}]"
                                                                     nil t)
                                                  (backward-char 1)
                                                  (point))))))
                              (while (not (looking-at "\}"))
                                (setq files (append files
                                                    (list (buffer-substring
                                                           (progn (forward-char 1)
                                                                  (point))
                                                           (progn (re-search-forward
                                                                   "[,\}]" nil t)
                                                                  (backward-char 1)
                                                                  (point)))))))
                              files)
                          (error (concat "No \\\\bibliography command in this "
                                         "buffer, can't read refer-bib-files")))))))
               (refer-expand-files files dir-list)))
            ((eq refer-bib-files 'dir)
             (let ((dirs (nreverse dir-list))
                   dir files)
               (while (setq dir (car dirs))
                 (setq files
                       (append (directory-files dir t "\\.bib$")
                               files))
                 (setq dirs (cdr dirs)))
               files))
            ((and (listp refer-bib-files)
                  (or (eq refer-bib-directory 'texinputs)
                      (eq refer-bib-directory 'bibinputs)))
             (refer-expand-files refer-bib-files dir-list))
            ((listp refer-bib-files) refer-bib-files)
            (t (error "Invalid value for refer-bib-files: %s"
                      refer-bib-files)))))
    (if (or (eq refer-bib-directory 'texinputs)
            (eq refer-bib-directory 'bibinputs))
        (setq refer-bib-directory dir-list))
    (if refer-cache-bib-files
        (setq refer-bib-files files))
    files))

;;; refer.el ends here
