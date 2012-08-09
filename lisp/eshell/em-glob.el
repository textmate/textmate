;;; em-glob.el --- extended file name globbing

;; Copyright (C) 1999-2012  Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>

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

;; The globbing code used by Eshell closely follows the syntax used by
;; zsh.  Basically, here is a summary of examples:
;;
;;   echo a*       ; anything starting with 'a'
;;   echo a#b      ; zero or more 'a's, then 'b'
;;   echo a##b     ; one or more 'a's, then 'b'
;;   echo a?       ; a followed by any character
;;   echo a*~ab    ; 'a', then anything, but not 'ab'
;;   echo c*~*~    ; all files beginning with 'c', except backups (*~)
;;
;; Recursive globbing is also supported:
;;
;;   echo **/*.c   ; all '.c' files at or under current directory
;;   echo ***/*.c  ; same as above, but traverse symbolic links
;;
;; Using argument predication, the recursive globbing syntax is
;; sufficient to replace the use of 'find <expr> | xargs <cmd>' in
;; most cases.  For example, to change the readership of all files
;; belonging to 'johnw' in the '/tmp' directory or lower, use:
;;
;;   chmod go-r /tmp/**/*(u'johnw')
;;
;; The glob above matches all of the files beneath '/tmp' that are
;; owned by the user 'johnw'.  See [Value modifiers and predicates],
;; for more information about argument predication.

;;; Code:

(eval-when-compile (require 'eshell))
(require 'esh-util)

;;;###autoload
(eshell-defgroup eshell-glob nil
  "This module provides extended globbing syntax, similar what is used
by zsh for filename generation."
  :tag "Extended filename globbing"
  :group 'eshell-module)

;;; User Variables:

(defcustom eshell-glob-load-hook nil
  "A list of functions to run when `eshell-glob' is loaded."
  :version "24.1"			; removed eshell-glob-initialize
  :type 'hook
  :group 'eshell-glob)

(defcustom eshell-glob-include-dot-files nil
  "If non-nil, glob patterns will match files beginning with a dot."
  :type 'boolean
  :group 'eshell-glob)

(defcustom eshell-glob-include-dot-dot t
  "If non-nil, glob patterns that match dots will match . and .."
  :type 'boolean
  :group 'eshell-glob)

(defcustom eshell-glob-case-insensitive (eshell-under-windows-p)
  "If non-nil, glob pattern matching will ignore case."
  :type 'boolean
  :group 'eshell-glob)

(defcustom eshell-glob-show-progress nil
  "If non-nil, display progress messages during a recursive glob.
This option slows down recursive glob processing by quite a bit."
  :type 'boolean
  :group 'eshell-glob)

(defcustom eshell-error-if-no-glob nil
  "If non-nil, it is an error for a glob pattern not to match.
 This mimics the behavior of zsh if non-nil, but bash if nil."
  :type 'boolean
  :group 'eshell-glob)

(defcustom eshell-glob-chars-list '(?\] ?\[ ?* ?? ?~ ?\( ?\) ?| ?# ?^)
  "List of additional characters used in extended globbing."
  :type '(repeat character)
  :group 'eshell-glob)

(defcustom eshell-glob-translate-alist
  '((?\] . "]")
    (?\[ . "[")
    (?^  . "^")
    (??  . ".")
    (?*  . ".*")
    (?~  . "~")
    (?\( . "\\(")
    (?\) . "\\)")
    (?\| . "\\|")
    (?#  . (lambda (str pos)
	     (if (and (< (1+ pos) (length str))
		      (memq (aref str (1+ pos)) '(?* ?# ?+ ??)))
		 (cons (if (eq (aref str (1+ pos)) ??)
			   "?"
			 (if (eq (aref str (1+ pos)) ?*)
			     "*" "+")) (+ pos 2))
	       (cons "*" (1+ pos))))))
  "An alist for translation of extended globbing characters."
  :type '(repeat (cons character (choice regexp function)))
  :group 'eshell-glob)

;;; Functions:

(defun eshell-glob-initialize ()
  "Initialize the extended globbing code."
  ;; it's important that `eshell-glob-chars-list' come first
  (when (boundp 'eshell-special-chars-outside-quoting)
    (set (make-local-variable 'eshell-special-chars-outside-quoting)
	 (append eshell-glob-chars-list eshell-special-chars-outside-quoting)))
  (add-hook 'eshell-parse-argument-hook 'eshell-parse-glob-chars t t)
  (add-hook 'eshell-pre-rewrite-command-hook
	    'eshell-no-command-globbing nil t))

(defun eshell-no-command-globbing (terms)
  "Don't glob the command argument.  Reflect this by modifying TERMS."
  (ignore
   (when (and (listp (car terms))
	      (eq (caar terms) 'eshell-extended-glob))
     (setcar terms (cadr (car terms))))))

(defun eshell-add-glob-modifier ()
  "Add `eshell-extended-glob' to the argument modifier list."
  (when (memq 'expand-file-name eshell-current-modifiers)
    (setq eshell-current-modifiers
	  (delq 'expand-file-name eshell-current-modifiers))
    ;; if this is a glob pattern than needs to be expanded, then it
    ;; will need to expand each member of the resulting glob list
    (add-to-list 'eshell-current-modifiers
		 (lambda (list)
                   (if (listp list)
                       (mapcar 'expand-file-name list)
                     (expand-file-name list)))))
  (add-to-list 'eshell-current-modifiers 'eshell-extended-glob))

(defun eshell-parse-glob-chars ()
  "Parse a globbing delimiter.
The character is not advanced for ordinary globbing characters, so
that other function may have a chance to override the globbing
interpretation."
  (when (memq (char-after) eshell-glob-chars-list)
    (if (not (memq (char-after) '(?\( ?\[)))
	(ignore (eshell-add-glob-modifier))
      (let ((here (point)))
	(forward-char)
	(let* ((delim (char-before))
	       (end (eshell-find-delimiter
		     delim (if (eq delim ?\[) ?\] ?\)))))
	  (if (not end)
	      (throw 'eshell-incomplete delim)
	    (if (and (eshell-using-module 'eshell-pred)
		     (eshell-arg-delimiter (1+ end)))
		(ignore (goto-char here))
	      (eshell-add-glob-modifier)
	      (prog1
		  (buffer-substring-no-properties (1- (point)) (1+ end))
		(goto-char (1+ end))))))))))

(defvar eshell-glob-chars-regexp nil)

(defun eshell-glob-regexp (pattern)
  "Convert glob-pattern PATTERN to a regular expression.
The basic syntax is:

  glob  regexp   meaning
  ----  ------   -------
  ?      .       matches any single character
  *      .*      matches any group of characters (or none)
  #      *       matches zero or more occurrences of preceding
  ##     +       matches one or more occurrences of preceding
  (x)    \(x\)   makes 'x' a regular expression group
  |      \|      boolean OR within an expression group
  [a-b]  [a-b]   matches a character or range
  [^a]   [^a]    excludes a character or range

If any characters in PATTERN have the text property `eshell-escaped'
set to true, then these characters will match themselves in the
resulting regular expression."
  (let ((matched-in-pattern 0)          ; How much of PATTERN handled
	regexp)
    (while (string-match
	    (or eshell-glob-chars-regexp
		(set (make-local-variable 'eshell-glob-chars-regexp)
		     (format "[%s]+" (apply 'string eshell-glob-chars-list))))
	    pattern matched-in-pattern)
      (let* ((op-begin (match-beginning 0))
	     (op-char (aref pattern op-begin)))
	(setq regexp
	      (concat regexp
		      (regexp-quote
		       (substring pattern matched-in-pattern op-begin))))
	(if (get-text-property op-begin 'escaped pattern)
	    (setq regexp (concat regexp
				 (regexp-quote (char-to-string op-char)))
		  matched-in-pattern (1+ op-begin))
	  (let ((xlat (assq op-char eshell-glob-translate-alist)))
	    (if (not xlat)
		(error "Unrecognized globbing character '%c'" op-char)
	      (if (stringp (cdr xlat))
		  (setq regexp (concat regexp (cdr xlat))
			matched-in-pattern (1+ op-begin))
		(let ((result (funcall (cdr xlat) pattern op-begin)))
		  (setq regexp (concat regexp (car result))
			matched-in-pattern (cdr result)))))))))
    (concat "\\`"
	    regexp
	    (regexp-quote (substring pattern matched-in-pattern))
	    "\\'")))

(defun eshell-extended-glob (glob)
  "Return a list of files generated from GLOB, perhaps looking for DIRS-ONLY.
This function almost fully supports zsh style filename generation
syntax.  Things that are not supported are:

   ^foo        for matching everything but foo
   (foo~bar)   tilde within a parenthesis group
   foo<1-10>   numeric ranges
   foo~x(a|b)  (a|b) will be interpreted as a predicate/modifier list

Mainly they are not supported because file matching is done with Emacs
regular expressions, and these cannot support the above constructs.

If this routine fails, it returns nil.  Otherwise, it returns a list
the form:

   (INCLUDE-REGEXP EXCLUDE-REGEXP (PRED-FUNC-LIST) (MOD-FUNC-LIST))"
  (let ((paths (eshell-split-path glob))
	eshell-glob-matches message-shown ange-cache)
    (unwind-protect
	(if (and (cdr paths)
		 (file-name-absolute-p (car paths)))
	    (eshell-glob-entries (file-name-as-directory (car paths))
				 (cdr paths))
	  (eshell-glob-entries (file-name-as-directory ".") paths))
      (if message-shown
	  (message nil)))
    (or (and eshell-glob-matches (sort eshell-glob-matches #'string<))
	(if eshell-error-if-no-glob
	    (error "No matches found: %s" glob)
	  glob))))

(defvar eshell-glob-matches)
(defvar message-shown)

;; FIXME does this really need to abuse eshell-glob-matches, message-shown?
(defun eshell-glob-entries (path globs &optional recurse-p)
  "Glob the entries in PATHS, possibly recursing if RECURSE-P is non-nil."
  (let* ((entries (ignore-errors
		    (file-name-all-completions "" path)))
	 (case-fold-search eshell-glob-case-insensitive)
	 (glob (car globs))
	 (len (length glob))
	 dirs rdirs
	 incl excl
	 name isdir pathname)
    (while (cond
	    ((and (= len 3) (equal glob "**/"))
	     (setq recurse-p 2
		   globs (cdr globs)
		   glob (car globs)
		   len (length glob)))
	    ((and (= len 4) (equal glob "***/"))
	     (setq recurse-p 3
		   globs (cdr globs)
		   glob (car globs)
		   len (length glob)))))
    (if (and recurse-p (not glob))
	(error "'**' cannot end a globbing pattern"))
    (let ((index 1))
      (setq incl glob)
      (while (and (eq incl glob)
		  (setq index (string-match "~" glob index)))
	(if (or (get-text-property index 'escaped glob)
		(or (= (1+ index) len)))
	    (setq index (1+ index))
	  (setq incl (substring glob 0 index)
		excl (substring glob (1+ index))))))
    ;; can't use `directory-file-name' because it strips away text
    ;; properties in the string
    (let ((len (1- (length incl))))
      (if (eq (aref incl len) ?/)
	  (setq incl (substring incl 0 len)))
      (when excl
	(setq len (1- (length excl)))
	(if (eq (aref excl len) ?/)
	    (setq excl (substring excl 0 len)))))
    (setq incl (eshell-glob-regexp incl)
	  excl (and excl (eshell-glob-regexp excl)))
    (if (or eshell-glob-include-dot-files
	    (eq (aref glob 0) ?.))
	(unless (or eshell-glob-include-dot-dot
		    (cdr globs))
	  (setq excl (if excl
			 (concat "\\(\\`\\.\\.?\\'\\|" excl "\\)")
		       "\\`\\.\\.?\\'")))
      (setq excl (if excl
		     (concat "\\(\\`\\.\\|" excl "\\)")
		   "\\`\\.")))
    (when (and recurse-p eshell-glob-show-progress)
      (message "Building file list...%d so far: %s"
	       (length eshell-glob-matches) path)
      (setq message-shown t))
    (if (equal path "./") (setq path ""))
    (while entries
      (setq name (car entries)
	    len (length name)
	    isdir (eq (aref name (1- len)) ?/))
      (if (let ((fname (directory-file-name name)))
	    (and (not (and excl (string-match excl fname)))
		 (string-match incl fname)))
	  (if (cdr globs)
	      (if isdir
		  (setq dirs (cons (concat path name) dirs)))
	    (setq eshell-glob-matches
		  (cons (concat path name) eshell-glob-matches))))
      (if (and recurse-p isdir
	       (or (> len 3)
		   (not (or (and (= len 2) (equal name "./"))
			    (and (= len 3) (equal name "../")))))
	       (setq pathname (concat path name))
	       (not (and (= recurse-p 2)
			 (file-symlink-p
			  (directory-file-name pathname)))))
	  (setq rdirs (cons pathname rdirs)))
      (setq entries (cdr entries)))
    (setq dirs (nreverse dirs)
	  rdirs (nreverse rdirs))
    (while dirs
      (eshell-glob-entries (car dirs) (cdr globs))
      (setq dirs (cdr dirs)))
    (while rdirs
      (eshell-glob-entries (car rdirs) globs recurse-p)
      (setq rdirs (cdr rdirs)))))

(provide 'em-glob)

;; Local Variables:
;; generated-autoload-file: "esh-groups.el"
;; End:

;;; em-glob.el ends here
