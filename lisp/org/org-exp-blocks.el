;;; org-exp-blocks.el --- pre-process blocks when exporting org files

;; Copyright (C) 2009-2012 Free Software Foundation, Inc.

;; Author: Eric Schulte

;; This file is part of GNU Emacs.
;;
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
;; This is a utility for pre-processing blocks in org files before
;; export using the `org-export-preprocess-hook'.  It can be used for
;; exporting new types of blocks from org-mode files and also for
;; changing the default export behavior of existing org-mode blocks.
;; The `org-export-blocks' and `org-export-interblocks' variables can
;; be used to control how blocks and the spaces between blocks
;; respectively are processed upon export.
;;
;; The type of a block is defined as the string following =#+begin_=,
;; so for example the following block would be of type ditaa.  Note
;; that both upper or lower case are allowed in =#+BEGIN_= and
;; =#+END_=.
;;
;; #+begin_ditaa blue.png -r -S
;; +---------+
;; | cBLU    |
;; |         |
;; |    +----+
;; |    |cPNK|
;; |    |    |
;; +----+----+
;; #+end_ditaa
;;
;;; Currently Implemented Block Types
;;
;; ditaa :: (DEPRECATED--use "#+begin_src ditaa" code blocks) Convert
;;          ascii pictures to actual images using ditaa
;;          http://ditaa.sourceforge.net/.  To use this set
;;          `org-ditaa-jar-path' to the path to ditaa.jar on your
;;          system (should be set automatically in most cases) .
;;
;; dot :: (DEPRECATED--use "#+begin_src dot" code blocks) Convert
;;        graphs defined using the dot graphing language to images
;;        using the dot utility.  For information on dot see
;;        http://www.graphviz.org/
;;
;; export-comment :: Wrap comments with titles and author information,
;;            in their own divs with author-specific ids allowing for
;;            css coloring of comments based on the author.
;;
;;; Adding new blocks
;;
;; When adding a new block type first define a formatting function
;; along the same lines as `org-export-blocks-format-dot' and then use
;; `org-export-blocks-add-block' to add your block type to
;; `org-export-blocks'.

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'org)
(require 'find-func)

(defun org-export-blocks-set (var value)
  "Set the value of `org-export-blocks' and install fontification."
  (set var value)
  (mapc (lambda (spec)
	  (if (nth 2 spec)
	      (setq org-protecting-blocks
		    (delete (symbol-name (car spec))
			    org-protecting-blocks))
	    (add-to-list 'org-protecting-blocks
			 (symbol-name (car spec)))))
	value))

(defcustom org-export-blocks
  '((export-comment org-export-blocks-format-comment t)
    (ditaa org-export-blocks-format-ditaa nil)
    (dot org-export-blocks-format-dot nil))
  "Use this alist to associate block types with block exporting functions.
The type of a block is determined by the text immediately
following the '#+BEGIN_' portion of the block header.  Each block
export function should accept three arguments."
  :group 'org-export-general
  :type '(repeat
	  (list
	   (symbol :tag "Block name")
	   (function :tag "Block formatter")
	   (boolean :tag "Fontify content as Org syntax")))
  :set 'org-export-blocks-set)

(defun org-export-blocks-add-block (block-spec)
  "Add a new block type to `org-export-blocks'.
BLOCK-SPEC should be a three element list the first element of
which should indicate the name of the block, the second element
should be the formatting function called by
`org-export-blocks-preprocess' and the third element a flag
indicating whether these types of blocks should be fontified in
org-mode buffers (see `org-protecting-blocks').  For example the
BLOCK-SPEC for ditaa blocks is as follows.

  (ditaa org-export-blocks-format-ditaa nil)"
  (unless (member block-spec org-export-blocks)
    (setq org-export-blocks (cons block-spec org-export-blocks))
    (org-export-blocks-set 'org-export-blocks org-export-blocks)))

(defcustom org-export-interblocks
  '()
  "Use this a-list to associate block types with block exporting functions.
The type of a block is determined by the text immediately
following the '#+BEGIN_' portion of the block header.  Each block
export function should accept three arguments."
  :group 'org-export-general
  :type 'alist)

(defcustom org-export-blocks-witheld
  '(hidden)
  "List of block types (see `org-export-blocks') which should not be exported."
  :group 'org-export-general
  :type 'list)

(defcustom org-export-blocks-postblock-hook nil
  "Run after blocks have been processed with `org-export-blocks-preprocess'."
  :group 'org-export-general
  :version "24.1"
  :type 'hook)

(defun org-export-blocks-html-quote (body &optional open close)
  "Protect BODY from org html export.
The optional OPEN and CLOSE tags will be inserted around BODY."

  (concat
   "\n#+BEGIN_HTML\n"
   (or open "")
   body (if (string-match "\n$" body) "" "\n")
   (or close "")
   "#+END_HTML\n"))

(defun org-export-blocks-latex-quote (body &optional open close)
  "Protect BODY from org latex export.
The optional OPEN and CLOSE tags will be inserted around BODY."
  (concat
   "\n#+BEGIN_LaTeX\n"
   (or open "")
   body (if (string-match "\n$" body) "" "\n")
   (or close "")
   "#+END_LaTeX\n"))

(defun org-export-blocks-preprocess ()
  "Export all blocks according to the `org-export-blocks' block export alist.
Does not export block types specified in specified in BLOCKS
which defaults to the value of `org-export-blocks-witheld'."
  (interactive)
  (save-window-excursion
    (let ((case-fold-search t)
	  (types '())
	  matched indentation type func
	  start end body headers preserve-indent progress-marker)
      (flet ((interblock (start end)
			 (mapcar (lambda (pair) (funcall (second pair) start end))
				 org-export-interblocks)))
	(goto-char (point-min))
	(setq start (point))
	(let ((beg-re "^\\([ \t]*\\)#\\+begin_\\(\\S-+\\)[ \t]*\\(.*\\)?[\r\n]"))
	  (while (re-search-forward beg-re nil t)
	    (let* ((match-start (copy-marker (match-beginning 0)))
		   (body-start (copy-marker (match-end 0)))
		   (indentation (length (match-string 1)))
		   (inner-re (format "^[ \t]*#\\+\\(begin\\|end\\)_%s"
				     (regexp-quote (downcase (match-string 2)))))
		   (type (intern (downcase (match-string 2))))
		   (headers (save-match-data
			      (org-split-string (match-string 3) "[ \t]+")))
		   (balanced 1)
		   (preserve-indent (or org-src-preserve-indentation
					(member "-i" headers)))
		   match-end)
	      (while (and (not (zerop balanced))
			  (re-search-forward inner-re nil t))
		(if (string= (downcase (match-string 1)) "end")
		    (decf balanced)
		  (incf balanced)))
	      (when (not (zerop balanced))
		(error "unbalanced begin/end_%s blocks with %S"
		       type (buffer-substring match-start (point))))
	      (setq match-end (copy-marker (match-end 0)))
	      (unless preserve-indent
		(setq body (save-match-data (org-remove-indentation
					     (buffer-substring
					      body-start (match-beginning 0))))))
	      (unless (memq type types) (setq types (cons type types)))
	      (save-match-data (interblock start match-start))
	      (when (setq func (cadr (assoc type org-export-blocks)))
		(let ((replacement (save-match-data
				     (if (memq type org-export-blocks-witheld) ""
				       (apply func body headers)))))
		  (when replacement
		    (delete-region match-start match-end)
		    (goto-char match-start) (insert replacement)
		    (if preserve-indent
			;; indent only the code block markers
			(save-excursion
			  (indent-line-to indentation) ; indent end_block
			  (goto-char match-start)
			  (indent-line-to indentation))	; indent begin_block
		      ;; indent everything
		      (indent-code-rigidly match-start (point) indentation)))))
	      ;; cleanup markers
	      (set-marker match-start nil)
	      (set-marker body-start nil)
	      (set-marker match-end nil))
	    (setq start (point))))
	(interblock start (point-max))
	(run-hooks 'org-export-blocks-postblock-hook)))))

;;================================================================================
;; type specific functions

;;--------------------------------------------------------------------------------
;; ditaa: create images from ASCII art using the ditaa utility
(defcustom org-ditaa-jar-path (expand-file-name
			    "ditaa.jar"
			    (file-name-as-directory
			     (expand-file-name
			      "scripts"
			      (file-name-as-directory
			       (expand-file-name
				"../contrib"
				(file-name-directory (find-library-name "org")))))))
  "Path to the ditaa jar executable."
  :group 'org-babel
  :type 'string)

(defvar org-export-current-backend) ; dynamically bound in org-exp.el
(defun org-export-blocks-format-ditaa (body &rest headers)
  "DEPRECATED: use begin_src ditaa code blocks

Pass block BODY to the ditaa utility creating an image.
Specify the path at which the image should be saved as the first
element of headers, any additional elements of headers will be
passed to the ditaa utility as command line arguments."
  (message "begin_ditaa blocks are DEPRECATED, use begin_src blocks")
  (let* ((args (if (cdr headers) (mapconcat 'identity (cdr headers) " ")))
         (data-file (make-temp-file "org-ditaa"))
	 (hash (progn
		 (set-text-properties 0 (length body) nil body)
		 (sha1 (prin1-to-string (list body args)))))
	 (raw-out-file (if headers (car headers)))
	 (out-file-parts (if (string-match "\\(.+\\)\\.\\([^\\.]+\\)$" raw-out-file)
			     (cons (match-string 1 raw-out-file)
				   (match-string 2 raw-out-file))
			   (cons raw-out-file "png")))
	 (out-file (concat (car out-file-parts) "_" hash "." (cdr out-file-parts))))
    (unless (file-exists-p org-ditaa-jar-path)
      (error (format "Could not find ditaa.jar at %s" org-ditaa-jar-path)))
    (setq body (if (string-match "^\\([^:\\|:[^ ]\\)" body)
		   body
		 (mapconcat (lambda (x) (substring x (if (> (length x) 1) 2 1)))
			    (org-split-string body "\n")
			    "\n")))
    (prog1
    (cond
     ((member org-export-current-backend '(html latex docbook))
      (unless (file-exists-p out-file)
        (mapc ;; remove old hashed versions of this file
         (lambda (file)
           (when (and (string-match (concat (regexp-quote (car out-file-parts))
                                            "_\\([[:alnum:]]+\\)\\."
                                            (regexp-quote (cdr out-file-parts)))
                                    file)
                      (= (length (match-string 1 out-file)) 40))
             (delete-file (expand-file-name file
                                            (file-name-directory out-file)))))
         (directory-files (or (file-name-directory out-file)
                              default-directory)))
        (with-temp-file data-file (insert body))
        (message (concat "java -jar " org-ditaa-jar-path " " args " " data-file " " out-file))
        (shell-command (concat "java -jar " org-ditaa-jar-path " " args " " data-file " " out-file)))
      (format "\n[[file:%s]]\n" out-file))
     (t (concat
	 "\n#+BEGIN_EXAMPLE\n"
	 body (if (string-match "\n$" body) "" "\n")
	 "#+END_EXAMPLE\n")))
    (message "begin_ditaa blocks are DEPRECATED, use begin_src blocks"))))

;;--------------------------------------------------------------------------------
;; dot: create graphs using the dot graphing language
;;      (require the dot executable to be in your path)
(defun org-export-blocks-format-dot (body &rest headers)
  "DEPRECATED: use \"#+begin_src dot\" code blocks

Pass block BODY to the dot graphing utility creating an image.
Specify the path at which the image should be saved as the first
element of headers, any additional elements of headers will be
passed to the dot utility as command line arguments.  Don't
forget to specify the output type for the dot command, so if you
are exporting to a file with a name like 'image.png' you should
include a '-Tpng' argument, and your block should look like the
following.

#+begin_dot models.png -Tpng
digraph data_relationships {
  \"data_requirement\" [shape=Mrecord, label=\"{DataRequirement|description\lformat\l}\"]
  \"data_product\" [shape=Mrecord, label=\"{DataProduct|name\lversion\lpoc\lformat\l}\"]
  \"data_requirement\" -> \"data_product\"
}
#+end_dot"
  (message "begin_dot blocks are DEPRECATED, use begin_src blocks")
  (let* ((args (if (cdr headers) (mapconcat 'identity (cdr headers) " ")))
         (data-file (make-temp-file "org-ditaa"))
	 (hash (progn
		 (set-text-properties 0 (length body) nil body)
		 (sha1 (prin1-to-string (list body args)))))
	 (raw-out-file (if headers (car headers)))
	 (out-file-parts (if (string-match "\\(.+\\)\\.\\([^\\.]+\\)$" raw-out-file)
			     (cons (match-string 1 raw-out-file)
				   (match-string 2 raw-out-file))
			   (cons raw-out-file "png")))
	 (out-file (concat (car out-file-parts) "_" hash "." (cdr out-file-parts))))
    (prog1
    (cond
     ((member org-export-current-backend '(html latex docbook))
      (unless (file-exists-p out-file)
	(mapc ;; remove old hashed versions of this file
	 (lambda (file)
	   (when (and (string-match (concat (regexp-quote (car out-file-parts))
					    "_\\([[:alnum:]]+\\)\\."
					    (regexp-quote (cdr out-file-parts)))
				    file)
		      (= (length (match-string 1 out-file)) 40))
	     (delete-file (expand-file-name file
					    (file-name-directory out-file)))))
	 (directory-files (or (file-name-directory out-file)
			      default-directory)))
	(with-temp-file data-file (insert body))
	(message (concat "dot " data-file " " args " -o " out-file))
	(shell-command (concat "dot " data-file " " args " -o " out-file)))
      (format "\n[[file:%s]]\n" out-file))
     (t (concat
	 "\n#+BEGIN_EXAMPLE\n"
	 body (if (string-match "\n$" body) "" "\n")
	 "#+END_EXAMPLE\n")))
    (message "begin_dot blocks are DEPRECATED, use begin_src blocks"))))

;;--------------------------------------------------------------------------------
;; comment: export comments in author-specific css-stylable divs
(defun org-export-blocks-format-comment (body &rest headers)
  "Format comment BODY by OWNER and return it formatted for export.
Currently, this only does something for HTML export, for all
other backends, it converts the comment into an EXAMPLE segment."
  (let ((owner (if headers (car headers)))
	(title (if (cdr headers) (mapconcat 'identity (cdr headers) " "))))
    (cond
     ((eq org-export-current-backend 'html) ;; We are exporting to HTML
      (concat "#+BEGIN_HTML\n"
	      "<div class=\"org-comment\""
	      (if owner (format " id=\"org-comment-%s\" " owner))
	      ">\n"
	      (if owner (concat "<b>" owner "</b> ") "")
	      (if (and title (> (length title) 0)) (concat " -- " title "<br/>\n") "<br/>\n")
	      "<p>\n"
	      "#+END_HTML\n"
	      body
	      "\n#+BEGIN_HTML\n"
	      "</p>\n"
	      "</div>\n"
	      "#+END_HTML\n"))
     (t ;; This is not HTML, so just make it an example.
      (concat "#+BEGIN_EXAMPLE\n"
	      (if title (concat "Title:" title "\n") "")
	      (if owner (concat "By:" owner "\n") "")
	      body
	      (if (string-match "\n\\'" body) "" "\n")
	      "#+END_EXAMPLE\n")))))

(provide 'org-exp-blocks)

;;; org-exp-blocks.el ends here
