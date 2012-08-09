;;; cvs-status.el --- major mode for browsing `cvs status' output -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 1999-2012 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: pcl-cvs cvs status tree vc tools

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

;; Todo:

;; - Somehow allow cvs-status-tree to work on-the-fly

;;; Code:

(eval-when-compile (require 'cl))
(require 'pcvs-util)

;;;

(defgroup cvs-status nil
  "Major mode for browsing `cvs status' output."
  :group 'pcl-cvs
  :prefix "cvs-status-")

(easy-mmode-defmap cvs-status-mode-map
  '(("n"	. next-line)
    ("p"	. previous-line)
    ("N"	. cvs-status-next)
    ("P"	. cvs-status-prev)
    ("\M-n"	. cvs-status-next)
    ("\M-p"	. cvs-status-prev)
    ("t"	. cvs-status-cvstrees)
    ("T"	. cvs-status-trees)
    (">"        . cvs-mode-checkout))
  "CVS-Status' keymap."
  :group 'cvs-status
  :inherit 'cvs-mode-map)

;;(easy-menu-define cvs-status-menu cvs-status-mode-map
;;  "Menu for `cvs-status-mode'."
;;  '("CVS-Status"
;;    ["Show Tag Trees"	cvs-status-tree	t]
;;    ))

(defvar cvs-status-mode-hook nil
  "Hook run at the end of `cvs-status-mode'.")

(defconst cvs-status-tags-leader-re "^   Existing Tags:$")
(defconst cvs-status-entry-leader-re
  "^File:\\s-+\\(?:no file \\)?\\(.*\\S-\\)\\s-+Status: \\(.+\\)$")
(defconst cvs-status-dir-re "^cvs[.ex]* [a-z]+: Examining \\(.+\\)$")
(defconst cvs-status-rev-re "[0-9][.0-9]*\\.[.0-9]*[0-9]")
(defconst cvs-status-tag-re "[ \t]\\([a-zA-Z][^ \t\n.]*\\)")

(defconst cvs-status-font-lock-keywords
  `((,cvs-status-entry-leader-re
     (1 'cvs-filename)
     (2 'cvs-need-action))
    (,cvs-status-tags-leader-re
     (,cvs-status-rev-re
      (save-excursion (re-search-forward "^\n" nil 'move) (point))
      (progn (re-search-backward cvs-status-tags-leader-re nil t)
	     (forward-line 1))
      (0 font-lock-comment-face))
     (,cvs-status-tag-re
      (save-excursion (re-search-forward "^\n" nil 'move) (point))
      (progn (re-search-backward cvs-status-tags-leader-re nil t)
	     (forward-line 1))
      (1 font-lock-function-name-face)))))
(defconst cvs-status-font-lock-defaults
  '(cvs-status-font-lock-keywords t nil nil nil (font-lock-multiline . t)))

(defvar cvs-minor-wrap-function)
(defvar cvs-force-command)
(defvar cvs-minor-current-files)
(defvar cvs-secondary-branch-prefix)
(defvar cvs-branch-prefix)
(defvar cvs-tag-print-rev)

(put 'cvs-status-mode 'mode-class 'special)
;;;###autoload
(define-derived-mode cvs-status-mode fundamental-mode "CVS-Status"
  "Mode used for cvs status output."
  (set (make-local-variable 'font-lock-defaults) cvs-status-font-lock-defaults)
  (set (make-local-variable 'cvs-minor-wrap-function) 'cvs-status-minor-wrap))

;; Define cvs-status-next and cvs-status-prev
(easy-mmode-define-navigation cvs-status cvs-status-entry-leader-re "entry")

(defun cvs-status-current-file ()
  (save-excursion
    (forward-line 1)
    (or (re-search-backward cvs-status-entry-leader-re nil t)
	(re-search-forward cvs-status-entry-leader-re))
    (let* ((file (match-string 1))
	   (cvsdir (and (re-search-backward cvs-status-dir-re nil t)
			(match-string 1)))
	   (pcldir (and (if (boundp 'cvs-pcl-cvs-dirchange-re)
			    (re-search-backward cvs-pcl-cvs-dirchange-re nil t))
			(match-string 1)))
	   (dir ""))
      (let ((default-directory ""))
	(when pcldir (setq dir (expand-file-name pcldir dir)))
	(when cvsdir (setq dir (expand-file-name cvsdir dir)))
	(expand-file-name file dir)))))

(defun cvs-status-current-tag ()
  (save-excursion
    (let ((pt (point))
	  (col (current-column))
	  (start (progn (re-search-backward cvs-status-tags-leader-re nil t) (point)))
	  (end (progn (re-search-forward "^$" nil t) (point))))
      (when (and (< start pt) (> end pt))
	(goto-char pt)
	(end-of-line)
	(let ((tag nil) (dist pt) (end (point)))
	  (beginning-of-line)
	  (while (re-search-forward cvs-status-tag-re end t)
	    (let* ((cole (current-column))
		   (colb (save-excursion
			   (goto-char (match-beginning 1)) (current-column)))
		   (ndist (min (abs (- cole col)) (abs (- colb col)))))
	      (when (< ndist dist)
		(setq dist ndist)
		(setq tag (match-string 1)))))
	  tag)))))

(defun cvs-status-minor-wrap (buf f)
  (let ((data (with-current-buffer buf
		(cons
		 (cons (cvs-status-current-file)
		       (cvs-status-current-tag))
		 (when mark-active
		   (save-excursion
		     (goto-char (mark))
		     (cons (cvs-status-current-file)
			   (cvs-status-current-tag))))))))
    (let ((cvs-branch-prefix (cdar data))
	  (cvs-secondary-branch-prefix (and (cdar data) (cddr data)))
	  (cvs-minor-current-files
	   (cons (caar data)
		 (when (and (cadr data) (not (equal (caar data) (cadr data))))
		   (list (cadr data)))))
	  ;; FIXME:  I need to force because the fileinfos are UNKNOWN
	  (cvs-force-command "/F"))
      (funcall f))))

;;
;; Tagelt, tag element
;;

(defstruct (cvs-tag
	    (:constructor nil)
	    (:constructor cvs-tag-make
			  (vlist &optional name type))
	    (:conc-name cvs-tag->))
  vlist
  name
  type)

(defsubst cvs-status-vl-to-str (vl) (mapconcat 'number-to-string vl "."))

(defun cvs-tag->string (tag)
  (if (stringp tag) tag
    (let ((name (cvs-tag->name tag))
	   (vl (cvs-tag->vlist tag)))
      (if (null name) (cvs-status-vl-to-str vl)
	(let ((rev (if vl (concat " (" (cvs-status-vl-to-str vl) ")") "")))
	  (if (consp name) (mapcar (lambda (name) (concat name rev)) name)
	    (concat name rev)))))))

(defun cvs-tag-compare-1 (vl1 vl2)
  (cond
   ((and (null vl1) (null vl2)) 'equal)
   ((null vl1) 'more2)
   ((null vl2) 'more1)
   (t (let ((v1 (car vl1))
	    (v2 (car vl2)))
	(cond
	 ((> v1 v2) 'more1)
	 ((< v1 v2) 'more2)
	 (t (cvs-tag-compare-1 (cdr vl1) (cdr vl2))))))))

(defsubst cvs-tag-compare (tag1 tag2)
  (cvs-tag-compare-1 (cvs-tag->vlist tag1) (cvs-tag->vlist tag2)))

(defun cvs-tag-merge (tag1 tag2)
  "Merge TAG1 and TAG2 into one."
  (let ((type1 (cvs-tag->type tag1))
	(type2 (cvs-tag->type tag2))
	(name1 (cvs-tag->name tag1))
	(name2 (cvs-tag->name tag2)))
    (unless (equal (cvs-tag->vlist tag1) (cvs-tag->vlist tag2))
      (setf (cvs-tag->vlist tag1) nil))
    (if type1
	(unless (or (not type2) (equal type1 type2))
	  (setf (cvs-tag->type tag1) nil))
      (setf (cvs-tag->type tag1) type2))
    (if name1
	(setf (cvs-tag->name tag1) (cvs-append name1 name2))
      (setf (cvs-tag->name tag1) name2))
    tag1))

(defun cvs-tree-print (tags printer column)
  "Print the tree of TAGS where each tag's string is given by PRINTER.
PRINTER should accept both a tag (in which case it should return a string)
or a string (in which case it should simply return its argument).
A tag cannot be a CONS.  The return value can also be a list of strings,
if several nodes where merged into one.
The tree will be printed no closer than column COLUMN."

  (let* ((eol (save-excursion (end-of-line) (current-column)))
	 (column (max (+ eol 2) column)))
    (if (null tags) column
      (let* ((rev (cvs-car tags))
	     (name (funcall printer (cvs-car rev)))
	     (rest (append (cvs-cdr name) (cvs-cdr tags)))
	     (prefix
	      (save-excursion
		(or (= (forward-line 1) 0) (insert "\n"))
		(cvs-tree-print rest printer column))))
	(assert (>= prefix column))
	(move-to-column prefix t)
	(assert (eolp))
	(insert (cvs-car name))
	(dolist (br (cvs-cdr rev))
	  (let* ((column (current-column))
		 (brrev (funcall printer (cvs-car br)))
		 (brlength (length (cvs-car brrev)))
		 (brfill (concat (make-string (/ brlength 2) ? ) "|"))
		 (prefix
		  (save-excursion
		    (insert " -- ")
		    (cvs-tree-print (cvs-append brrev brfill (cvs-cdr br))
				    printer (current-column)))))
	    (delete-region (save-excursion (move-to-column prefix) (point))
			   (point))
	    (insert " " (make-string (- prefix column 2) ?-) " ")
	    (end-of-line)))
	prefix))))

(defun cvs-tree-merge (tree1 tree2)
  "Merge tags trees TREE1 and TREE2 into one.
BEWARE:  because of stability issues, this is not a symmetric operation."
  (assert (and (listp tree1) (listp tree2)))
  (cond
   ((null tree1) tree2)
   ((null tree2) tree1)
   (t
    (let* ((rev1 (car tree1))
	   (tag1 (cvs-car rev1))
	   (vl1 (cvs-tag->vlist tag1))
	   (l1 (length vl1))
	   (rev2 (car tree2))
	   (tag2 (cvs-car rev2))
	   (vl2 (cvs-tag->vlist tag2))
	   (l2 (length vl2)))
    (cond
     ((= l1 l2)
      (case (cvs-tag-compare tag1 tag2)
	(more1 (list* rev2 (cvs-tree-merge tree1 (cdr tree2))))
	(more2 (list* rev1 (cvs-tree-merge (cdr tree1) tree2)))
	(equal
	 (cons (cons (cvs-tag-merge tag1 tag2)
		     (cvs-tree-merge (cvs-cdr rev1) (cvs-cdr rev2)))
	       (cvs-tree-merge (cdr tree1) (cdr tree2))))))
     ((> l1 l2)
      (cvs-tree-merge
       (list (cons (cvs-tag-make (butlast vl1)) tree1)) tree2))
     ((< l1 l2)
      (cvs-tree-merge
       tree1 (list (cons (cvs-tag-make (butlast vl2)) tree2)))))))))

(defun cvs-tag-make-tag (tag)
  (let ((vl (mapcar 'string-to-number (split-string (nth 2 tag) "\\."))))
    (cvs-tag-make vl (nth 0 tag) (intern (nth 1 tag)))))

(defun cvs-tags->tree (tags)
  "Make a tree out of a list of TAGS."
  (let ((tags
	 (mapcar
	  (lambda (tag)
	    (let ((tag (cvs-tag-make-tag tag)))
	      (list (if (not (eq (cvs-tag->type tag) 'branch)) tag
		      (list (cvs-tag-make (butlast (cvs-tag->vlist tag)))
			    tag)))))
	  tags)))
    (while (cdr tags)
      (let (tl)
	(while tags
	  (push (cvs-tree-merge (pop tags) (pop tags)) tl))
	(setq tags (nreverse tl))))
    (car tags)))

(defun cvs-status-get-tags ()
  "Look for a list of tags, read them in and delete them.
Return nil if there was an empty list of tags and t if there wasn't
even a list.  Else, return the list of tags where each element of
the list is a three-string list TAG, KIND, REV."
  (let ((tags nil))
    (if (not (re-search-forward cvs-status-tags-leader-re nil t)) t
      (forward-char 1)
      (let ((pt (point))
	    (lastrev nil)
	    (case-fold-search t))
	(or
	 (looking-at "\\s-+no\\s-+tags")

	 (progn				; normal listing
	   (while (looking-at "^[ \t]+\\([^ \t\n]+\\)[ \t]+(\\([a-z]+\\): \\(.+\\))$")
	     (push (list (match-string 1) (match-string 2) (match-string 3)) tags)
	     (forward-line 1))
	   (unless (looking-at "^$") (setq tags nil) (goto-char pt))
	   tags)

	 (progn				; cvstree-style listing
	   (while (or (looking-at "^   .+\\(.\\)  \\([0-9.]+\\): \\([^\n\t .0-9][^\n\t ]*\\)?$")
		      (and lastrev
			   (looking-at "^   .+\\(\\)  \\(8\\)?  \\([^\n\t .0-9][^\n\t ]*\\)$")))
	     (setq lastrev (or (match-string 2) lastrev))
	     (push (list (match-string 3)
			 (if (equal (match-string 1) " ") "branch" "revision")
			 lastrev) tags)
	     (forward-line 1))
	   (unless (looking-at "^$") (setq tags nil) (goto-char pt))
	   (setq tags (nreverse tags)))

	 (progn				; new tree style listing
	   (let* ((re-lead "[ \t]*\\(-+\\)?\\(|\n?[ \t]+\\)*")
		  (re3 (concat re-lead "\\(\\.\\)?\\(" cvs-status-rev-re "\\)"))
		  (re2 (concat re-lead cvs-status-tag-re "\\(\\)"))
		  (re1 (concat re-lead cvs-status-tag-re
			       " (\\(" cvs-status-rev-re "\\))")))
	     (while (or (looking-at re1) (looking-at re2) (looking-at re3))
	       (push (list (match-string 3)
			   (if (match-string 1) "branch" "revision")
			   (match-string 4)) tags)
	       (goto-char (match-end 0))
	       (when (eolp) (forward-char 1))))
	   (unless (looking-at "^$") (setq tags nil) (goto-char pt))
	   (setq tags (nreverse tags))))

	(delete-region pt (point)))
      tags)))

(defvar font-lock-mode)
;; (defun cvs-refontify (beg end)
;;   (when (and (boundp 'font-lock-mode)
;; 	     font-lock-mode
;; 	     (fboundp 'font-lock-fontify-region))
;;     (font-lock-fontify-region (1- beg) (1+ end))))

(defun cvs-status-trees ()
  "Look for a lists of tags, and replace them with trees."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t)
	  (tags nil))
      (while (listp (setq tags (cvs-status-get-tags)))
	;;(let ((pt (save-excursion (forward-line -1) (point))))
	  (save-restriction
	    (narrow-to-region (point) (point))
	    ;;(newline)
	    (combine-after-change-calls
	      (cvs-tree-print (cvs-tags->tree tags) 'cvs-tag->string 3)))
	  ;;(cvs-refontify pt (point))
	  ;;(sit-for 0)
	  ;;)
	  ))))

;;;;
;;;; CVSTree-style trees
;;;;

(defvar cvs-tree-use-jisx0208 nil)	;Old compat var.
(defvar cvs-tree-use-charset
  (cond
   (cvs-tree-use-jisx0208 'jisx0208)
   ((char-displayable-p ?━) 'unicode)
   ((char-displayable-p (make-char 'japanese-jisx0208 40 44)) 'jisx0208))
  "*Non-nil if we should use the graphical glyphs from `japanese-jisx0208'.
Otherwise, default to ASCII chars like +, - and |.")

(defconst cvs-tree-char-space
  (case cvs-tree-use-charset
    (jisx0208 (make-char 'japanese-jisx0208 33 33))
    (unicode " ")
    (t "  ")))
(defconst cvs-tree-char-hbar
  (case cvs-tree-use-charset
    (jisx0208 (make-char 'japanese-jisx0208 40 44))
    (unicode "━")
    (t "--")))
(defconst cvs-tree-char-vbar
  (case cvs-tree-use-charset
    (jisx0208 (make-char 'japanese-jisx0208 40 45))
    (unicode "┃")
    (t "| ")))
(defconst cvs-tree-char-branch
  (case cvs-tree-use-charset
    (jisx0208 (make-char 'japanese-jisx0208 40 50))
    (unicode "┣")
    (t "+-")))
(defconst cvs-tree-char-eob		;end of branch
  (case cvs-tree-use-charset
    (jisx0208 (make-char 'japanese-jisx0208 40 49))
    (unicode "┗")
    (t "`-")))
(defconst cvs-tree-char-bob		;beginning of branch
  (case cvs-tree-use-charset
    (jisx0208 (make-char 'japanese-jisx0208 40 51))
    (unicode "┳")
    (t "+-")))

(defun cvs-tag-lessp (tag1 tag2)
  (eq (cvs-tag-compare tag1 tag2) 'more2))

(defvar cvs-tree-nomerge nil)

(defun cvs-status-cvstrees (&optional arg)
  "Look for a list of tags, and replace it with a tree.
Optional prefix ARG chooses between two representations."
  (interactive "P")
  (when (and cvs-tree-use-charset
	     (not enable-multibyte-characters))
    ;; We need to convert the buffer from unibyte to multibyte
    ;; since we'll use multibyte chars for the tree.
    (let ((modified (buffer-modified-p))
	  (inhibit-read-only t)
	  (inhibit-modification-hooks t))
      (unwind-protect
	  (progn
	    (decode-coding-region (point-min) (point-max) 'undecided)
	    (set-buffer-multibyte t))
	(restore-buffer-modified-p modified))))
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t)
	  (tags nil)
	  (cvs-tree-nomerge (if arg (not cvs-tree-nomerge) cvs-tree-nomerge)))
      (while (listp (setq tags (cvs-status-get-tags)))
	(let ((tags (mapcar 'cvs-tag-make-tag tags))
	      ;;(pt (save-excursion (forward-line -1) (point)))
	      )
	  (setq tags (sort tags 'cvs-tag-lessp))
	  (let* ((first (car tags))
		 (prev (if (cvs-tag-p first)
			   (list (car (cvs-tag->vlist first))) nil)))
	    (combine-after-change-calls
	      (cvs-tree-tags-insert tags prev))
	    ;;(cvs-refontify pt (point))
	    ;;(sit-for 0)
	    ))))))

(defun cvs-tree-tags-insert (tags prev)
  (when tags
    (let* ((tag (car tags))
	   (vlist (cvs-tag->vlist tag))
	   (nprev ;"next prev"
	    (let* ((next (cvs-car (cadr tags)))
		   (nprev (if (and cvs-tree-nomerge next
				   (equal vlist (cvs-tag->vlist next)))
			      prev vlist)))
	      (cvs-map (lambda (v _p) v) nprev prev)))
	   (after (save-excursion
		   (newline)
		   (cvs-tree-tags-insert (cdr tags) nprev)))
	   (pe t)			;"prev equal"
	   (nas nil))			;"next afters" to be returned
      (insert "   ")
      (do* ((vs vlist (cdr vs))
	    (ps prev (cdr ps))
	    (as after (cdr as)))
	  ((and (null as) (null vs) (null ps))
	   (let ((revname (cvs-status-vl-to-str vlist)))
	     (if (cvs-every 'identity (cvs-map 'equal prev vlist))
		 (insert (make-string (+ 4 (length revname)) ? )
			 (or (cvs-tag->name tag) ""))
	       (insert "  " revname ": " (or (cvs-tag->name tag) "")))))
	(let* ((eq (and pe (equal (car ps) (car vs))))
	       (next-eq (equal (cadr ps) (cadr vs))))
	  (let* ((na+char
		  (if (car as)
		      (if eq
			  (if next-eq (cons t cvs-tree-char-vbar)
			    (cons t cvs-tree-char-branch))
			(cons nil cvs-tree-char-bob))
		    (if eq
			(if next-eq (cons nil cvs-tree-char-space)
			  (cons t cvs-tree-char-eob))
		      (cons nil (if (and (eq (cvs-tag->type tag) 'branch)
					 (cvs-every 'null as))
				    cvs-tree-char-space
				  cvs-tree-char-hbar))))))
	    (insert (cdr na+char))
	    (push (car na+char) nas))
	  (setq pe eq)))
      (nreverse nas))))

;;;;
;;;; Merged trees from different files
;;;;

;; (defun cvs-tree-fuzzy-merge-1 (trees tree prev)
;;   )

;; (defun cvs-tree-fuzzy-merge (trees tree)
;;   "Do the impossible:  merge TREE into TREES."
;;   ())

;; (defun cvs-tree ()
;;   "Get tags from the status output and merge them all into a big tree."
;;   (save-excursion
;;     (goto-char (point-min))
;;     (let ((inhibit-read-only t)
;; 	  (trees (make-vector 31 0)) tree)
;;       (while (listp (setq tree (cvs-tags->tree (cvs-status-get-tags))))
;; 	(cvs-tree-fuzzy-merge trees tree))
;;       (erase-buffer)
;;       (let ((cvs-tag-print-rev nil))
;; 	(cvs-tree-print tree 'cvs-tag->string 3)))))


(provide 'cvs-status)

;;; cvs-status.el ends here
