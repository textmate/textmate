;;; semantic/symref/cscope.el --- Semantic-symref support via cscope.

;;; Copyright (C) 2009-2012 Free Software Foundation, Inc.

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
;; Semantic symref support via cscope.

(require 'cedet-cscope)
(require 'semantic/symref)

(defvar ede-minor-mode)
(declare-function ede-toplevel "ede/base")
(declare-function ede-project-root-directory "ede/files")

;;; Code:
;;;###autoload
(defclass semantic-symref-tool-cscope (semantic-symref-tool-baseclass)
  (
   )
  "A symref tool implementation using CScope.
The CScope command can be used to generate lists of tags in a way
similar to that of `grep'.  This tool will parse the output to generate
the hit list.

See the function `cedet-cscope-search' for more details.")

(defmethod semantic-symref-perform-search ((tool semantic-symref-tool-cscope))
  "Perform a search with GNU Global."
  (let* ((rootproj (when (and (featurep 'ede) ede-minor-mode)
		     (ede-toplevel)))
	 (default-directory (if rootproj
				(ede-project-root-directory rootproj)
			      default-directory))
	 ;; CScope has to be run from the project root where
	 ;; cscope.out is.
	 (b (cedet-cscope-search (oref tool :searchfor)
				 (oref tool :searchtype)
				 (oref tool :resulttype)
				 (oref tool :searchscope)
				 ))
	)
    (semantic-symref-parse-tool-output tool b)
    ))

(defmethod semantic-symref-parse-tool-output-one-line ((tool semantic-symref-tool-cscope))
  "Parse one line of grep output, and return it as a match list.
Moves cursor to end of the match."
  (cond ((eq (oref tool :resulttype) 'file)
	 ;; Search for files
	 (when (re-search-forward "^\\([^\n]+\\)$" nil t)
	   (match-string 1)))
	((eq (oref tool :searchtype) 'tagcompletions)
	 ;; Search for files
	 (when (re-search-forward "^[^ ]+ [^ ]+ [^ ]+ \\(.*\\)$" nil t)
	   (let ((subtxt (match-string 1))
		 (searchtxt (oref tool :searchfor)))
	     (if (string-match (concat "\\<" searchtxt "\\(\\w\\|\\s_\\)*\\>")
			       subtxt)
		 (match-string 0 subtxt)
	       ;; We have to return something at this point.
	       subtxt)))
	 )
	(t
	 (when (re-search-forward "^\\([^ ]+\\) [^ ]+ \\([0-9]+\\) " nil t)
	   (cons (string-to-number (match-string 2))
		 (expand-file-name (match-string 1)))
	   ))))

(provide 'semantic/symref/cscope)

;; Local variables:
;; generated-autoload-file: "../loaddefs.el"
;; generated-autoload-load-name: "semantic/symref/cscope"
;; End:

;;; semantic/symref/cscope.el ends here
