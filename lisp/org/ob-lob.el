;;; ob-lob.el --- functions supporting the Library of Babel

;; Copyright (C) 2009-2012  Free Software Foundation, Inc.

;; Authors: Eric Schulte
;;	 Dan Davison
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org

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

;;; Code:
(eval-when-compile
  (require 'cl))
(require 'ob)
(require 'ob-table)

(declare-function org-babel-in-example-or-verbatim "ob-exp" nil)

(defvar org-babel-library-of-babel nil
  "Library of source-code blocks.
This is an association list.  Populate the library by adding
files to `org-babel-lob-files'.")

(defcustom org-babel-lob-files '()
  "Files used to populate the `org-babel-library-of-babel'.
To add files to this list use the `org-babel-lob-ingest' command."
  :group 'org-babel
  :version "24.1"
  :type 'list)

(defvar org-babel-default-lob-header-args '((:exports . "results"))
  "Default header arguments to use when exporting #+lob/call lines.")

;;;###autoload
(defun org-babel-lob-ingest (&optional file)
  "Add all named source-blocks defined in FILE to
`org-babel-library-of-babel'."
  (interactive "fFile: ")
  (let ((lob-ingest-count 0))
    (org-babel-map-src-blocks file
      (let* ((info (org-babel-get-src-block-info 'light))
	     (source-name (nth 4 info)))
	(when source-name
	  (setq source-name (intern source-name)
		org-babel-library-of-babel
		(cons (cons source-name info)
		      (assq-delete-all source-name org-babel-library-of-babel))
		lob-ingest-count (1+ lob-ingest-count)))))
    (message "%d src block%s added to Library of Babel"
	     lob-ingest-count (if (> lob-ingest-count 1) "s" ""))
    lob-ingest-count))

(defconst org-babel-block-lob-one-liner-regexp
  (concat
   "^\\([ \t]*?\\)#\\+call:[ \t]+\\([^\(\)\n]+?\\)\\(\\[\\(.*\\)\\]\\|\\(\\)\\)"
   "\(\\([^\n]*?\\)\)\\(\\[.+\\]\\|\\)[ \t]*\\(\\([^\n]*\\)\\)?")
  "Regexp to match non-inline calls to predefined source block functions.")

(defconst org-babel-inline-lob-one-liner-regexp
  (concat
   "\\([^\n]*?\\)call_\\([^\(\)\n]+?\\)\\(\\[\\(.*?\\)\\]\\|\\(\\)\\)"
   "\(\\([^\n]*?\\)\)\\(\\[\\(.*?\\)\\]\\)?")
  "Regexp to match inline calls to predefined source block functions.")

(defconst org-babel-lob-one-liner-regexp
  (concat "\\(" org-babel-block-lob-one-liner-regexp
	  "\\|" org-babel-inline-lob-one-liner-regexp "\\)")
  "Regexp to match calls to predefined source block functions.")

;; functions for executing lob one-liners

;;;###autoload
(defun org-babel-lob-execute-maybe ()
  "Execute a Library of Babel source block, if appropriate.
Detect if this is context for a Library Of Babel source block and
if so then run the appropriate source block from the Library."
  (interactive)
  (let ((info (org-babel-lob-get-info)))
    (if (and (nth 0 info) (not (org-babel-in-example-or-verbatim)))
	(progn (org-babel-lob-execute info) t)
      nil)))

;;;###autoload
(defun org-babel-lob-get-info ()
  "Return a Library of Babel function call as a string."
  (flet ((nonempty (a b)
		   (let ((it (match-string a)))
		     (if (= (length it) 0) (match-string b) it))))
    (let ((case-fold-search t))
      (save-excursion
	(beginning-of-line 1)
	(when (looking-at org-babel-lob-one-liner-regexp)
	  (append
	   (mapcar #'org-babel-clean-text-properties
		   (list
		    (format "%s%s(%s)%s"
			    (nonempty 3 12)
			    (if (not (= 0 (length (nonempty 5 14))))
				(concat "[" (nonempty 5 14) "]") "")
			    (or (nonempty 7 16) "")
			    (or (nonempty 8 19) ""))
		    (nonempty 9 18)))
	   (list (length (if (= (length (match-string 12)) 0)
			     (match-string 2) (match-string 11))))))))))

(defun org-babel-lob-execute (info)
  "Execute the lob call specified by INFO."
  (let ((params (org-babel-process-params
		 (org-babel-merge-params
		  org-babel-default-header-args
		  (org-babel-params-from-properties)
		  (org-babel-parse-header-arguments
		   (org-babel-clean-text-properties
		    (concat ":var results="
			    (mapconcat #'identity (butlast info) " "))))))))
    (org-babel-execute-src-block
     nil (list "emacs-lisp" "results" params nil nil (nth 2 info)))))

(provide 'ob-lob)



;;; ob-lob.el ends here
