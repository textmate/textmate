;;; files-x.el --- extended file handling commands

;; Copyright (C) 2009-2012 Free Software Foundation, Inc.

;; Author: Juri Linkov <juri@jurta.org>
;; Maintainer: FSF
;; Keywords: files
;; Package: emacs

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

;; This file defines additional infrequently used file- and
;; directory-handling commands that should not be in files.el
;; to not make the dumped image bigger.

;;; Code:


;;; Commands to add/delete file-local/directory-local variables.

(defun read-file-local-variable (prompt)
  "Read file-local variable using PROMPT and completion.
Intended to be used in the `interactive' spec of
`add-file-local-variable', `delete-file-local-variable',
`add-dir-local-variable', `delete-dir-local-variable'."
  (let (default variable)
    (setq default (variable-at-point))
    (setq default (and (symbolp default) (boundp default)
		       (symbol-name default)))
    (setq variable
	  (completing-read
	   (if default
	       (format "%s (default %s): " prompt default)
	     (format "%s: " prompt))
	   obarray
	   (lambda (sym)
	     (or (user-variable-p sym)
                 (get sym 'safe-local-variable)
		 (memq sym '(mode eval coding unibyte))))
	   nil nil nil default nil))
    (and (stringp variable) (intern variable))))

(defun read-file-local-variable-value (variable)
  "Read value of file-local VARIABLE using completion.
Intended to be used in the `interactive' spec of
`add-file-local-variable' and `add-dir-local-variable'."
  (let (default value)
    (cond
     ((eq variable 'mode)
      (setq default (and (symbolp major-mode) (symbol-name major-mode)))
      (setq value
	    (completing-read
	     (if default
		 (format "Add %s with value (default %s): " variable default)
	       (format "Add %s with value: " variable))
	     obarray
	     (lambda (sym)
	       (string-match-p "-mode\\'" (symbol-name sym)))
	     nil nil nil default nil))
      (and (stringp value)
	   (intern (replace-regexp-in-string "-mode\\'" "" value))))
     ((eq variable 'eval)
      (let ((minibuffer-completing-symbol t))
	(read-from-minibuffer (format "Add %s with expression: " variable)
			      nil read-expression-map t
			      'read-expression-history)))
     ((eq variable 'coding)
      (setq default (and (symbolp buffer-file-coding-system)
			 (symbol-name buffer-file-coding-system)))
      (read-coding-system
       (if default
	   (format "Add %s with value (default %s): " variable default)
	 (format "Add %s with value: " variable))
       default))
     (t
      (read (read-string (format "Add %s with value: " variable)
			 nil 'set-variable-value-history
			 (format "%S"
				 (cond ((eq variable 'unibyte) t)
				       ((boundp variable)
					(symbol-value variable))))))))))

(defun read-file-local-variable-mode ()
  "Read per-directory file-local variable's mode using completion.
Intended to be used in the `interactive' spec of
`add-dir-local-variable', `delete-dir-local-variable'."
  (let* ((default (and (symbolp major-mode) (symbol-name major-mode)))
	 (mode
	  (completing-read
	   (if default
	       (format "Mode or subdirectory (default %s): " default)
	     (format "Mode or subdirectory: "))
	   obarray
	   (lambda (sym)
	     (and (string-match-p "-mode\\'" (symbol-name sym))
		  (not (string-match-p "-minor-mode\\'" (symbol-name sym)))))
	   nil nil nil default nil)))
    (cond
     ((equal mode "nil") nil)
     ((and (stringp mode) (fboundp (intern mode))) (intern mode))
     (t mode))))

(defun modify-file-local-variable (variable value op)
  "Modify file-local VARIABLE in Local Variables depending on operation OP.

If OP is `add-or-replace' then delete all existing settings of
VARIABLE (except `mode' and `eval') and add a new file-local VARIABLE
with VALUE to the Local Variables list.

If there is no Local Variables list in the current file buffer and OP
is not `delete' then this function adds the first line containing the
string `Local Variables:' and the last line containing the string `End:'.

If OP is `delete' then delete all existing settings of VARIABLE
from the Local Variables list ignoring the input argument VALUE."
  (catch 'exit
    (let ((beg (point)) end replaced-pos)
      (unless enable-local-variables
	(throw 'exit (message "File-local variables are disabled")))

      ;; Look for "Local variables:" line in last page.
      (widen)
      (goto-char (point-max))
      (search-backward "\n\^L" (max (- (point-max) 3000) (point-min)) 'move)

      ;; Add "Local variables:" list if not found.
      (unless (let ((case-fold-search t))
		(search-forward "Local Variables:" nil t))

	;; Don't add "Local variables:" list for the deletion operation.
	(when (eq op 'delete)
	  (throw 'exit (progn (goto-char beg)
			      (message "Local Variables not found"))))

	(goto-char (point-max))
	(let ((comment-style 'plain)
	      (comment-start (or comment-start ";; ")))
	  (comment-region
	   (prog1 (setq beg (point))
	     (insert "\nLocal Variables:\nEnd:\n"))
	   (point)))

	(unless (let ((case-fold-search t))
		  (goto-char beg)
		  (search-forward "Local Variables:" nil t))
	  (throw 'exit (message "Can't add file-local variables"))))

      ;; prefix is what comes before "local variables:" in its line.
      ;; suffix is what comes after "local variables:" in its line.
      (let* ((prefix (buffer-substring (line-beginning-position)
				       (match-beginning 0)))
	     (suffix (buffer-substring (point) (line-end-position)))
	     (prefix-re (concat "^" (regexp-quote prefix)))
	     (suffix-re (concat (regexp-quote suffix) "$")))

	;; Find or add missing "End:".
	(forward-line 1)
	(setq beg (point))
	(save-excursion
	  (unless (let ((case-fold-search t))
		    (re-search-forward
		     (concat prefix-re "[ \t]*End:[ \t]*" suffix-re)
		     nil t))
	    (save-excursion
	      (insert (format "%sEnd:%s\n" prefix suffix))))
	  (beginning-of-line)
	  (setq end (point-marker)))

	;; Find and delete all existing variable/value pairs.
	(when (member op '(add-or-replace delete))
	  (if (and (eq op 'add-or-replace) (memq variable '(mode eval)))
	      (goto-char end)
	    (goto-char beg)
	    (while (re-search-forward
		    (format "%s%S:.*%s" prefix-re variable suffix-re) end t)
	      (delete-region (match-beginning 0) (1+ (match-end 0)))
	      (setq replaced-pos (point)))))

	;; Add a new variable/value pair.  Add `mode' to the start, add new
	;; variable to the end, and add a replaced variable to its last location.
	(when (eq op 'add-or-replace)
	  (cond
	   ((eq variable 'mode) (goto-char beg))
	   ((null replaced-pos) (goto-char end))
	   (replaced-pos (goto-char replaced-pos)))
	  (insert (format "%s%S: %S%s\n" prefix variable value suffix)))))))

;;;###autoload
(defun add-file-local-variable (variable value)
  "Add file-local VARIABLE with its VALUE to the Local Variables list.

This command deletes all existing settings of VARIABLE (except `mode'
and `eval') and adds a new file-local VARIABLE with VALUE to the
Local Variables list.

If there is no Local Variables list in the current file buffer
then this function adds the first line containing the string
`Local Variables:' and the last line containing the string `End:'."
  (interactive
   (let ((variable (read-file-local-variable "Add file-local variable")))
     (list variable (read-file-local-variable-value variable))))
  (modify-file-local-variable variable value 'add-or-replace))

;;;###autoload
(defun delete-file-local-variable (variable)
  "Delete all settings of file-local VARIABLE from the Local Variables list."
  (interactive
   (list (read-file-local-variable "Delete file-local variable")))
  (modify-file-local-variable variable nil 'delete))

(defun modify-file-local-variable-prop-line (variable value op)
  "Modify file-local VARIABLE in the -*- line depending on operation OP.

If OP is `add-or-replace' then delete all existing settings of
VARIABLE (except `mode' and `eval') and add a new file-local VARIABLE
with VALUE to the -*- line.

If there is no -*- line at the beginning of the current file buffer
and OP is not `delete' then this function adds the -*- line.

If OP is `delete' then delete all existing settings of VARIABLE
from the -*- line ignoring the input argument VALUE."
  (catch 'exit
    (let ((beg (point)) end replaced-pos)
      (unless enable-local-variables
	(throw 'exit (message "File-local variables are disabled")))

      ;; Find the -*- line at the beginning of the current buffer.
      (widen)
      (goto-char (point-min))
      (setq end (set-auto-mode-1))

      (if end
	  (setq beg (point-marker) end (copy-marker end))

	;; Add the -*- line if not found.
	;; Don't add the -*- line for the deletion operation.
	(when (eq op 'delete)
	  (throw 'exit (progn (goto-char beg)
			      (message "The -*- line not found"))))

	(goto-char (point-min))

	;; Skip interpreter magic line "#!"
	(when (looking-at "^\\(#!\\|'\\\\\"\\)")
	  (forward-line 1))

	(let ((comment-style 'plain)
	      (comment-start (or comment-start ";;; ")))
	  (comment-region
	   (prog1 (point)
	     (insert "-*-")
	     (setq beg (point-marker))
	     (setq end (point-marker))
	     (insert "-*-\n"))
	   (point))))

      (cond
       ((looking-at "[ \t]*\\([^ \t\n\r:;]+\\)\\([ \t]*-\\*-\\)")
	;; Simple form: "-*- MODENAME -*-".
	(if (eq variable 'mode)
	    ;; Replace or delete MODENAME
	    (progn
	      (when (member op '(add-or-replace delete))
		(delete-region (match-beginning 1) (match-end 1)))
	      (when (eq op 'add-or-replace)
		(goto-char (match-beginning 1))
		(insert (format "%S" value))))
	  ;; Else, turn `MODENAME' into `mode:MODENAME'
	  ;; and add `VARIABLE: VALUE;'
	  (goto-char (match-beginning 2))
	  (insert (format "; %S: %S; " variable value))
	  (goto-char (match-beginning 1))
	  (insert " mode: ")))

       (t
	;; Hairy form: '-*-' [ <variable> ':' <value> ';' ]* '-*-'
	;; Find and delete all existing variable/value pairs.
	(when (member op '(add-or-replace delete))
	  (if (and (eq op 'add-or-replace) (memq variable '(mode eval)))
	      (goto-char end)
	    (goto-char beg)
	    (while (< (point) end)
	      (or (looking-at "[ \t]*\\([^ \t\n:]+\\)[ \t]*:[ \t]*")
		  (throw 'exit (message "Malformed -*- line")))
	      (goto-char (match-end 0))
	      (let ((key (intern (match-string 1))))
                (save-restriction
                  (narrow-to-region (point) end)
                  (let ((read-circle nil))
                    (read (current-buffer))))
		(skip-chars-forward " \t;")
		(when (eq key variable)
		  (delete-region (match-beginning 0) (point))
		  (setq replaced-pos (point)))))))
	;; Add a new variable/value pair.  Add `mode' to the start, add new
	;; variable to the end, and add a replaced variable to its last location.
	(when (eq op 'add-or-replace)
	  (cond
	   ((eq variable 'mode) (goto-char beg))
	   ((null replaced-pos) (goto-char end))
	   (replaced-pos (goto-char replaced-pos)))
	  (if (and (not (eq (char-before) ?\;))
		   (not (equal (point) (marker-position beg))))
	      (insert ";"))
	  (unless (eq (char-before) ?\s) (insert " "))
	  (insert (format "%S: %S;" variable value))
	  (unless (eq (char-after) ?\s) (insert " "))))))))

;;;###autoload
(defun add-file-local-variable-prop-line (variable value)
  "Add file-local VARIABLE with its VALUE to the -*- line.

This command deletes all existing settings of VARIABLE (except `mode'
and `eval') and adds a new file-local VARIABLE with VALUE to
the -*- line.

If there is no -*- line at the beginning of the current file buffer
then this function adds it."
  (interactive
   (let ((variable (read-file-local-variable "Add -*- file-local variable")))
     (list variable (read-file-local-variable-value variable))))
  (modify-file-local-variable-prop-line variable value 'add-or-replace))

;;;###autoload
(defun delete-file-local-variable-prop-line (variable)
  "Delete all settings of file-local VARIABLE from the -*- line."
  (interactive
   (list (read-file-local-variable "Delete -*- file-local variable")))
  (modify-file-local-variable-prop-line variable nil 'delete))

(defvar auto-insert) ; from autoinsert.el

(defun modify-dir-local-variable (mode variable value op)
  "Modify directory-local VARIABLE in .dir-locals.el depending on operation OP.

If OP is `add-or-replace' then delete all existing settings of
VARIABLE (except `mode' and `eval') and add a new directory-local VARIABLE
with VALUE to the MODE alist where MODE can be a mode name symbol or
a subdirectory name.

If .dir-locals.el was not found and OP is not `delete' then create
this file in the current directory.

If OP is `delete' then delete all existing settings of VARIABLE
from the MODE alist ignoring the input argument VALUE."
  (catch 'exit
    (unless enable-local-variables
      (throw 'exit (message "Directory-local variables are disabled")))
    (let ((variables-file (or (and (buffer-file-name)
				   (not (file-remote-p (buffer-file-name)))
				   (dir-locals-find-file (buffer-file-name)))
			      dir-locals-file))
	  variables)
      (if (consp variables-file)	; result from cache
	  ;; If cache element has an mtime, assume it came from a file.
	  ;; Otherwise, assume it was set directly.
	  (setq variables-file (if (nth 2 variables-file)
				   (expand-file-name dir-locals-file
						     (car variables-file))
				 (cadr variables-file))))
      ;; I can't be bothered to handle this case right now.
      ;; Dir locals were set directly from a class.  You need to
      ;; directly modify the class in dir-locals-class-alist.
      (and variables-file (not (stringp variables-file))
	   (throw 'exit (message "Directory locals were not set from a file")))
      ;; Don't create ".dir-locals.el" for the deletion operation.
      (and (eq op 'delete)
	   (or (not variables-file)
	       (not (file-exists-p variables-file)))
	   (throw 'exit (message "No .dir-locals.el file was found")))
      (let ((auto-insert nil))
	(find-file variables-file))
      (widen)
      (goto-char (point-min))

      ;; Read alist of directory-local variables.
      (ignore-errors
	(delete-region
	 (prog1 (point)
	   (setq variables (let ((read-circle nil))
			     (read (current-buffer)))))
	 (point)))

      ;; Add or replace variable in alist of directory-local variables.
      (let ((mode-assoc (assoc mode variables)))
	(if mode-assoc
	    (setq variables
		  (cons (cons mode
			      (if (eq op 'delete)
				  (assq-delete-all variable (cdr mode-assoc))
				(cons
				 (cons variable value)
				 (if (memq variable '(mode eval))
				     (cdr mode-assoc)
				   (assq-delete-all variable (cdr mode-assoc))))))
			(assq-delete-all mode variables)))
	  (setq variables
		(cons `(,mode . ((,variable . ,value)))
		      variables))))

      ;; Insert modified alist of directory-local variables.
      (insert ";;; Directory Local Variables\n")
      (insert ";;; See Info node `(emacs) Directory Variables' for more information.\n\n")
      (pp (sort variables
		(lambda (a b)
		  (cond
		   ((null (car a)) t)
		   ((null (car b)) nil)
		   ((and (symbolp (car a)) (stringp (car b))) t)
		   ((and (symbolp (car b)) (stringp (car a))) nil)
		   (t (string< (car a) (car b))))))
	  (current-buffer)))))

;;;###autoload
(defun add-dir-local-variable (mode variable value)
  "Add directory-local VARIABLE with its VALUE and MODE to .dir-locals.el."
  (interactive
   (let (variable)
     (list
      (read-file-local-variable-mode)
      (setq variable (read-file-local-variable "Add directory-local variable"))
      (read-file-local-variable-value variable))))
  (modify-dir-local-variable mode variable value 'add-or-replace))

;;;###autoload
(defun delete-dir-local-variable (mode variable)
  "Delete all MODE settings of file-local VARIABLE from .dir-locals.el."
  (interactive
   (list
    (read-file-local-variable-mode)
    (read-file-local-variable "Delete directory-local variable")))
  (modify-dir-local-variable mode variable nil 'delete))

;;;###autoload
(defun copy-file-locals-to-dir-locals ()
  "Copy file-local variables to .dir-locals.el."
  (interactive)
  (dolist (elt file-local-variables-alist)
    (unless (assq (car elt) dir-local-variables-alist)
      (add-dir-local-variable major-mode (car elt) (cdr elt)))))

;;;###autoload
(defun copy-dir-locals-to-file-locals ()
  "Copy directory-local variables to the Local Variables list."
  (interactive)
  (dolist (elt dir-local-variables-alist)
    (add-file-local-variable (car elt) (cdr elt))))

;;;###autoload
(defun copy-dir-locals-to-file-locals-prop-line ()
  "Copy directory-local variables to the -*- line."
  (interactive)
  (dolist (elt dir-local-variables-alist)
    (add-file-local-variable-prop-line (car elt) (cdr elt))))



(provide 'files-x)

;;; files-x.el ends here
