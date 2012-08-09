;;; org-list.el --- Plain lists for Org-mode
;;
;; Copyright (C) 2004-2012 Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten at orgmode dot org>
;;	   Bastien Guerry <bzg AT gnu DOT org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains the code dealing with plain lists in Org-mode.

;; The core concept behind lists is their structure.  A structure is
;; a snapshot of the list, in the shape of a data tree (see
;; `org-list-struct').

;; Once the list structure is stored, it is possible to make changes
;; on it that will be mirrored to the real list or to get information
;; about the list, using accessors and methods provided in the
;; library.  Most of them require the use of one or two helper
;; functions, namely `org-list-parents-alist' and
;; `org-list-prevs-alist'.

;; Structure is eventually applied to the buffer with
;; `org-list-write-struct'.  This function repairs (bullets,
;; indentation, checkboxes) the list in the process.  It should be
;; called near the end of any function working on structures.

;; Thus, a function applying to lists should usually follow this
;; template:

;; 1. Verify point is in a list and grab item beginning (with the same
;;    function `org-in-item-p').  If the function requires the cursor
;;    to be at item's bullet, `org-at-item-p' is more selective.  It
;;    is also possible to move point to the closest item with
;;    `org-list-search-backward', or `org-list-search-forward',
;;    applied to the function `org-item-beginning-re'.

;; 2. Get list structure with `org-list-struct'.

;; 3. Compute one, or both, helper functions,
;;    (`org-list-parents-alist', `org-list-prevs-alist') depending on
;;    needed accessors.

;; 4. Proceed with the modifications, using methods and accessors.

;; 5. Verify and apply structure to buffer, using
;;    `org-list-write-struct'.

;; 6. If changes made to the list might have modified check-boxes,
;;    call `org-update-checkbox-count-maybe'.

;; Computing a structure can be a costly operation on huge lists (a
;; few thousand lines long).  Thus, code should follow the rule:
;; "collect once, use many".  As a corollary, it is usually a bad idea
;; to use directly an interactive function inside the code, as those,
;; being independent entities, read the whole list structure another
;; time.

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'org-macs)
(require 'org-compat)

(defvar org-M-RET-may-split-line)
(defvar org-auto-align-tags)
(defvar org-blank-before-new-entry)
(defvar org-clock-string)
(defvar org-closed-string)
(defvar org-deadline-string)
(defvar org-description-max-indent)
(defvar org-drawers)
(defvar org-odd-levels-only)
(defvar org-scheduled-string)
(defvar org-ts-regexp)
(defvar org-ts-regexp-both)

(declare-function org-at-heading-p "org" (&optional ignored))
(declare-function org-before-first-heading-p "org" ())
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-combine-plists "org" (&rest plists))
(declare-function org-count "org" (cl-item cl-seq))
(declare-function org-current-level "org" ())
(declare-function org-entry-get "org"
		  (pom property &optional inherit literal-nil))
(declare-function org-fix-tags-on-the-fly "org" ())
(declare-function org-get-indentation "org" (&optional line))
(declare-function org-icompleting-read "org" (&rest args))
(declare-function org-in-block-p "org" (names))
(declare-function org-in-regexp "org" (re &optional nlines visually))
(declare-function org-inlinetask-goto-beginning "org-inlinetask" ())
(declare-function org-inlinetask-goto-end "org-inlinetask" ())
(declare-function org-inlinetask-in-task-p "org-inlinetask" ())
(declare-function org-inlinetask-outline-regexp "org-inlinetask" ())
(declare-function org-level-increment "org" ())
(declare-function org-narrow-to-subtree "org" ())
(declare-function org-at-heading-p "org" (&optional invisible-ok))
(declare-function org-previous-line-empty-p "org" ())
(declare-function org-remove-if "org" (predicate seq))
(declare-function org-reduced-level "org" (L))
(declare-function org-show-subtree "org" ())
(declare-function org-time-string-to-seconds "org" (s))
(declare-function org-timer-hms-to-secs "org-timer" (hms))
(declare-function org-timer-item "org-timer" (&optional arg))
(declare-function org-trim "org" (s))
(declare-function org-uniquify "org" (list))
(declare-function outline-invisible-p "outline" (&optional pos))
(declare-function outline-flag-region "outline" (from to flag))
(declare-function outline-next-heading "outline" ())
(declare-function outline-previous-heading "outline" ())



;;; Configuration variables

(defgroup org-plain-lists nil
  "Options concerning plain lists in Org-mode."
  :tag "Org Plain lists"
  :group 'org-structure)

(defcustom org-cycle-include-plain-lists t
  "When t, make TAB cycle visibility on plain list items.
Cycling plain lists works only when the cursor is on a plain list
item.  When the cursor is on an outline heading, plain lists are
treated as text.  This is the most stable way of handling this,
which is why it is the default.

When this is the symbol `integrate', then during cycling, plain
list items will *temporarily* be interpreted as outline headlines
with a level given by 1000+i where i is the indentation of the
bullet.  This setting can lead to strange effects when switching
visibility to `children', because the first \"child\" in a
subtree decides what children should be listed.  If that first
\"child\" is a plain list item with an implied large level
number, all true children and grand children of the outline
heading will be exposed in a children' view."
  :group 'org-plain-lists
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "With cursor in plain list (recommended)" t)
	  (const :tag "As children of outline headings" integrate)))

(defcustom org-list-demote-modify-bullet nil
  "Default bullet type installed when demoting an item.
This is an association list, for each bullet type, this alist will point
to the bullet that should be used when this item is demoted.
For example,

 (setq org-list-demote-modify-bullet
       '((\"+\" . \"-\") (\"-\" . \"+\") (\"*\" . \"+\")))

will make

  + Movies
    + Silence of the Lambs
    + My Cousin Vinny
  + Books
    + The Hunt for Red October
    + The Road to Omaha

into

  + Movies
    - Silence of the Lambs
    - My Cousin Vinny
  + Books
    - The Hunt for Red October
    - The Road to Omaha"
  :group 'org-plain-lists
  :type '(repeat
	  (cons
	   (choice :tag "If the current bullet is  "
		   (const "-")
		   (const "+")
		   (const "*")
		   (const "1.")
		   (const "1)"))
	   (choice :tag "demotion will change it to"
		   (const "-")
		   (const "+")
		   (const "*")
		   (const "1.")
		   (const "1)")))))

(defcustom org-plain-list-ordered-item-terminator t
  "The character that makes a line with leading number an ordered list item.
Valid values are ?. and ?\).  To get both terminators, use t."
  :group 'org-plain-lists
  :type '(choice (const :tag "dot like in \"2.\"" ?.)
		 (const :tag "paren like in \"2)\"" ?\))
		 (const :tag "both" t)))

(defcustom org-alphabetical-lists nil
  "Non-nil means single character alphabetical bullets are allowed.
Both uppercase and lowercase are handled.  Lists with more than
26 items will fallback to standard numbering.  Alphabetical
counters like \"[@c]\" will be recognized."
  :group 'org-plain-lists
  :version "24.1"
  :type 'boolean)

(defcustom org-list-two-spaces-after-bullet-regexp nil
  "A regular expression matching bullets that should have 2 spaces after them.
When nil, no bullet will have two spaces after them.  When
a string, it will be used as a regular expression.  When the
bullet type of a list is changed, the new bullet type will be
matched against this regexp.  If it matches, there will be two
spaces instead of one after the bullet in each item of the list."
  :group 'org-plain-lists
  :type '(choice
	  (const :tag "never" nil)
	  (regexp)))

(defcustom org-empty-line-terminates-plain-lists nil
  "Non-nil means an empty line ends all plain list levels.
Otherwise, two of them will be necessary."
  :group 'org-plain-lists
  :type 'boolean)

(defcustom org-list-automatic-rules '((bullet . t)
				      (checkbox . t)
				      (indent . t))
  "Non-nil means apply set of rules when acting on lists.
By default, automatic actions are taken when using
 \\[org-meta-return], \\[org-metaright], \\[org-metaleft],
 \\[org-shiftmetaright], \\[org-shiftmetaleft],
 \\[org-ctrl-c-minus], \\[org-toggle-checkbox] or
 \\[org-insert-todo-heading].  You can disable individually these
 rules by setting them to nil.  Valid rules are:

bullet    when non-nil, cycling bullet do not allow lists at
          column 0 to have * as a bullet and descriptions lists
          to be numbered.
checkbox  when non-nil, checkbox statistics is updated each time
          you either insert a new checkbox or toggle a checkbox.
          It also prevents from inserting a checkbox in a
          description item.
indent    when non-nil, indenting or outdenting list top-item
          with its subtree will move the whole list and
          outdenting a list whose bullet is * to column 0 will
          change that bullet to \"-\"."
   :group 'org-plain-lists
   :version "24.1"
   :type '(alist :tag "Sets of rules"
		 :key-type
		 (choice
		  (const :tag "Bullet" bullet)
		  (const :tag "Checkbox" checkbox)
		  (const :tag "Indent" indent))
		 :value-type
		 (boolean :tag "Activate" :value t)))

(defcustom org-list-use-circular-motion nil
  "Non-nil means commands implying motion in lists should be cyclic.

In that case, the item following the last item is the first one,
and the item preceding the first item is the last one.

This affects the behavior of \\[org-move-item-up],
 \\[org-move-item-down], \\[org-next-item] and
 \\[org-previous-item]."
  :group 'org-plain-lists
  :version "24.1"
  :type 'boolean)

(defvar org-checkbox-statistics-hook nil
  "Hook that is run whenever Org thinks checkbox statistics should be updated.
This hook runs even if checkbox rule in
`org-list-automatic-rules' does not apply, so it can be used to
implement alternative ways of collecting statistics
information.")

(defcustom org-hierarchical-checkbox-statistics t
  "Non-nil means checkbox statistics counts only the state of direct children.
When nil, all boxes below the cookie are counted.
This can be set to nil on a per-node basis using a COOKIE_DATA property
with the word \"recursive\" in the value."
  :group 'org-plain-lists
  :type 'boolean)

(defcustom org-description-max-indent 20
  "Maximum indentation for the second line of a description list.
When the indentation would be larger than this, it will become
5 characters instead."
  :group 'org-plain-lists
  :type 'integer)

(defcustom org-list-indent-offset 0
  "Additional indentation for sub-items in a list.
By setting this to a small number, usually 1 or 2, one can more
clearly distinguish sub-items in a list."
  :group 'org-plain-lists
  :version "24.1"
  :type 'integer)

(defcustom org-list-radio-list-templates
  '((latex-mode "% BEGIN RECEIVE ORGLST %n
% END RECEIVE ORGLST %n
\\begin{comment}
#+ORGLST: SEND %n org-list-to-latex
-
\\end{comment}\n")
    (texinfo-mode "@c BEGIN RECEIVE ORGLST %n
@c END RECEIVE ORGLST %n
@ignore
#+ORGLST: SEND %n org-list-to-texinfo
-
@end ignore\n")
    (html-mode "<!-- BEGIN RECEIVE ORGLST %n -->
<!-- END RECEIVE ORGLST %n -->
<!--
#+ORGLST: SEND %n org-list-to-html
-
-->\n"))
  "Templates for radio lists in different major modes.
All occurrences of %n in a template will be replaced with the name of the
list, obtained by prompting the user."
  :group 'org-plain-lists
  :type '(repeat
	  (list (symbol :tag "Major mode")
		(string :tag "Format"))))

(defvar org-list-forbidden-blocks '("example" "verse" "src" "ascii" "beamer"
				    "docbook" "html" "latex" "odt")
  "Names of blocks where lists are not allowed.
Names must be in lower case.")

(defvar org-list-export-context '(block inlinetask)
  "Context types where lists will be interpreted during export.

Valid types are `drawer', `inlinetask' and `block'.  More
specifically, type `block' is determined by the variable
`org-list-forbidden-blocks'.")



;;; Predicates and regexps

(defconst org-list-end-re (if org-empty-line-terminates-plain-lists "^[ \t]*\n"
			    "^[ \t]*\n[ \t]*\n")
  "Regex corresponding to the end of a list.
It depends on `org-empty-line-terminates-plain-lists'.")

(defconst org-list-full-item-re
  (concat "^[ \t]*\\(\\(?:[-+*]\\|\\(?:[0-9]+\\|[A-Za-z]\\)[.)]\\)\\(?:[ \t]+\\|$\\)\\)"
	  "\\(?:\\[@\\(?:start:\\)?\\([0-9]+\\|[A-Za-z]\\)\\][ \t]*\\)?"
	  "\\(?:\\(\\[[ X-]\\]\\)\\(?:[ \t]+\\|$\\)\\)?"
	  "\\(?:\\(.*\\)[ \t]+::\\(?:[ \t]+\\|$\\)\\)?")
  "Matches a list item and puts everything into groups:
group 1: bullet
group 2: counter
group 3: checkbox
group 4: description tag")

(defun org-item-re ()
  "Return the correct regular expression for plain lists."
  (let ((term (cond
	       ((eq org-plain-list-ordered-item-terminator t) "[.)]")
	       ((= org-plain-list-ordered-item-terminator ?\)) ")")
	       ((= org-plain-list-ordered-item-terminator ?.) "\\.")
	       (t "[.)]")))
	(alpha (if org-alphabetical-lists "\\|[A-Za-z]" "")))
    (concat "\\([ \t]*\\([-+]\\|\\(\\([0-9]+" alpha "\\)" term
	    "\\)\\)\\|[ \t]+\\*\\)\\([ \t]+\\|$\\)")))

(defsubst org-item-beginning-re ()
  "Regexp matching the beginning of a plain list item."
  (concat "^" (org-item-re)))

(defun org-list-at-regexp-after-bullet-p (regexp)
  "Is point at a list item with REGEXP after bullet?"
  (and (org-at-item-p)
       (save-excursion
	 (goto-char (match-end 0))
	 (let ((counter-re (concat "\\(?:\\[@\\(?:start:\\)?"
				   (if org-alphabetical-lists
				       "\\([0-9]+\\|[A-Za-z]\\)"
				     "[0-9]+")
				   "\\][ \t]*\\)")))
	   ;; Ignore counter if any
	   (when (looking-at counter-re) (goto-char (match-end 0))))
	 (looking-at regexp))))

(defun org-list-in-valid-context-p ()
  "Is point in a context where lists are allowed?"
  (not (org-in-block-p org-list-forbidden-blocks)))

(defun org-in-item-p ()
  "Return item beginning position when in a plain list, nil otherwise."
  (save-excursion
    (beginning-of-line)
    (let* ((case-fold-search t)
	   (context (org-list-context))
	   (lim-up (car context))
	   (drawers-re (concat "^[ \t]*:\\("
			       (mapconcat 'regexp-quote org-drawers "\\|")
			       "\\):[ \t]*$"))
	   (inlinetask-re (and (featurep 'org-inlinetask)
			       (org-inlinetask-outline-regexp)))
	   (item-re (org-item-re))
	   ;; Indentation isn't meaningful when point starts at an empty
	   ;; line or an inline task.
	   (ind-ref (if (or (looking-at "^[ \t]*$")
			    (and inlinetask-re (looking-at inlinetask-re)))
			10000
		      (org-get-indentation))))
      (cond
       ((eq (nth 2 context) 'invalid) nil)
       ((looking-at item-re) (point))
       (t
	;; Detect if cursor in amidst `org-list-end-re'.  First, count
	;; number HL of hard lines it takes, then call `org-in-regexp'
	;; to compute its boundaries END-BOUNDS.  When point is
	;; in-between, move cursor before regexp beginning.
	(let ((hl 0) (i -1) end-bounds)
	  (when (and (progn
		       (while (setq i (string-match
				       "[\r\n]" org-list-end-re (1+ i)))
			 (setq hl (1+ hl)))
		       (setq end-bounds (org-in-regexp org-list-end-re hl)))
		     (>= (point) (car end-bounds))
		     (< (point) (cdr end-bounds)))
	    (goto-char (car end-bounds))
	    (forward-line -1)))
	;; Look for an item, less indented that reference line.
	(catch 'exit
	  (while t
	    (let ((ind (org-get-indentation)))
	      (cond
	       ;; This is exactly what we want.
	       ((and (looking-at item-re) (< ind ind-ref))
		(throw 'exit (point)))
	       ;; At upper bound of search or looking at the end of a
	       ;; previous list: search is over.
	       ((<= (point) lim-up) (throw 'exit nil))
	       ((looking-at org-list-end-re) (throw 'exit nil))
	       ;; Skip blocks, drawers, inline-tasks, blank lines
	       ((and (looking-at "^[ \t]*#\\+end_")
		     (re-search-backward "^[ \t]*#\\+begin_" lim-up t)))
	       ((and (looking-at "^[ \t]*:END:")
		     (re-search-backward drawers-re lim-up t))
		(beginning-of-line))
	       ((and inlinetask-re (looking-at inlinetask-re))
		(org-inlinetask-goto-beginning)
		(forward-line -1))
	       ((looking-at "^[ \t]*$") (forward-line -1))
	       ;; Text at column 0 cannot belong to a list: stop.
	       ((zerop ind) (throw 'exit nil))
	       ;; Normal text less indented than reference line, take
	       ;; it as new reference.
	       ((< ind ind-ref)
		(setq ind-ref ind)
		(forward-line -1))
	       (t (forward-line -1)))))))))))

(defun org-at-item-p ()
  "Is point in a line starting a hand-formatted item?"
  (save-excursion
    (beginning-of-line)
    (and (looking-at (org-item-re)) (org-list-in-valid-context-p))))

(defun org-at-item-bullet-p ()
  "Is point at the bullet of a plain list item?"
  (and (org-at-item-p)
       (not (member (char-after) '(?\  ?\t)))
       (< (point) (match-end 0))))

(defun org-at-item-timer-p ()
  "Is point at a line starting a plain list item with a timer?"
  (org-list-at-regexp-after-bullet-p
   "\\([0-9]+:[0-9]+:[0-9]+\\)[ \t]+::[ \t]+"))

(defun org-at-item-description-p ()
  "Is point at a description list item?"
  (org-list-at-regexp-after-bullet-p "\\(\\S-.+\\)[ \t]+::[ \t]+"))

(defun org-at-item-checkbox-p ()
  "Is point at a line starting a plain-list item with a checklet?"
  (org-list-at-regexp-after-bullet-p "\\(\\[[- X]\\]\\)[ \t]+"))

(defun org-at-item-counter-p ()
  "Is point at a line starting a plain-list item with a counter?"
  (and (org-at-item-p)
       (looking-at org-list-full-item-re)
       (match-string 2)))



;;; Structures and helper functions

(defun org-list-context ()
  "Determine context, and its boundaries, around point.

Context will be a cell like (MIN MAX CONTEXT) where MIN and MAX
are boundaries and CONTEXT is a symbol among `drawer', `block',
`invalid', `inlinetask' and nil.

Contexts `block' and `invalid' refer to `org-list-forbidden-blocks'."
  (save-match-data
    (save-excursion
      (org-with-limited-levels
       (beginning-of-line)
       (let ((case-fold-search t) (pos (point)) beg end context-type
	     ;; Get positions of surrounding headings.  This is the
	     ;; default context.
	     (lim-up (or (save-excursion (and (ignore-errors (org-back-to-heading t))
					      (point)))
			 (point-min)))
	     (lim-down (or (save-excursion (outline-next-heading)) (point-max))))
	 ;; Is point inside a drawer?
	 (let ((end-re "^[ \t]*:END:")
	       ;; Can't use org-drawers-regexp as this function might
	       ;; be called in buffers not in Org mode.
	       (beg-re (concat "^[ \t]*:\\("
			       (mapconcat 'regexp-quote org-drawers "\\|")
			       "\\):[ \t]*$")))
	   (when (save-excursion
		   (and (not (looking-at beg-re))
			(not (looking-at end-re))
			(setq beg (and (re-search-backward beg-re lim-up t)
				       (1+ (point-at-eol))))
			(setq end (or (and (re-search-forward end-re lim-down t)
					   (1- (match-beginning 0)))
				      lim-down))
			(>= end pos)))
	     (setq lim-up beg lim-down end context-type 'drawer)))
	 ;; Is point strictly in a block, and of which type?
	 (let ((block-re "^[ \t]*#\\+\\(begin\\|end\\)_") type)
	   (when (save-excursion
		   (and (not (looking-at block-re))
			(setq beg (and (re-search-backward block-re lim-up t)
				       (1+ (point-at-eol))))
			(looking-at "^[ \t]*#\\+begin_\\(\\S-+\\)")
			(setq type (downcase (match-string 1)))
			(goto-char beg)
			(setq end (or (and (re-search-forward block-re lim-down t)
					   (1- (point-at-bol)))
				      lim-down))
			(>= end pos)
			(equal (downcase (match-string 1)) "end")))
	     (setq lim-up beg lim-down end
		   context-type (if (member type org-list-forbidden-blocks)
				    'invalid 'block))))
	 ;; Is point in an inlinetask?
	 (when (and (featurep 'org-inlinetask)
		    (save-excursion
		      (let* ((beg-re (org-inlinetask-outline-regexp))
			     (end-re (concat beg-re "END[ \t]*$")))
			(and (not (looking-at "^\\*+"))
			     (setq beg (and (re-search-backward beg-re lim-up t)
					    (1+ (point-at-eol))))
			     (not (looking-at end-re))
			     (setq end (and (re-search-forward end-re lim-down t)
					    (1- (match-beginning 0))))
			     (> (point) pos)))))
	   (setq lim-up beg lim-down end context-type 'inlinetask))
	 ;; Return context boundaries and type.
	 (list lim-up lim-down context-type))))))

(defun org-list-struct ()
  "Return structure of list at point.

A list structure is an alist where key is point at item, and
values are:
1. indentation,
2. bullet with trailing whitespace,
3. bullet counter, if any,
4. checkbox, if any,
5. description tag, if any,
6. position at item end.

Thus the following list, where numbers in parens are
point-at-bol:

- [X] first item                             (1)
  1. sub-item 1                              (18)
  5. [@5] sub-item 2                         (34)
  some other text belonging to first item    (55)
- last item                                  (97)
  + tag :: description                       (109)
                                             (131)

will get the following structure:

\(\(1 0 \"- \"  nil \"[X]\" nil 97\)
 \(18 2 \"1. \"  nil nil nil 34\)
 \(34 2 \"5. \" \"5\" nil nil 55\)
 \(97 0 \"- \"  nil nil nil 131\)
 \(109 2 \"+ \" nil nil \"tag\" 131\)

Assume point is at an item."
  (save-excursion
    (beginning-of-line)
    (let* ((case-fold-search t)
	   (context (org-list-context))
	   (lim-up (car context))
	   (lim-down (nth 1 context))
	   (text-min-ind 10000)
	   (item-re (org-item-re))
	   (drawers-re (concat "^[ \t]*:\\("
			       (mapconcat 'regexp-quote org-drawers "\\|")
			       "\\):[ \t]*$"))
	   (inlinetask-re (and (featurep 'org-inlinetask)
			       (org-inlinetask-outline-regexp)))
	   (beg-cell (cons (point) (org-get-indentation)))
	   ind itm-lst itm-lst-2 end-lst end-lst-2 struct
	   (assoc-at-point
	    (function
	     ;; Return association at point.
	     (lambda (ind)
	       (looking-at org-list-full-item-re)
	       (list (point)
		     ind
		     (match-string-no-properties 1)	; bullet
		     (match-string-no-properties 2)	; counter
		     (match-string-no-properties 3)	; checkbox
		     (match-string-no-properties 4)))))	; description tag
	   (end-before-blank
	    (function
	     ;; Ensure list ends at the first blank line.
	     (lambda ()
	       (skip-chars-backward " \r\t\n")
	       (min (1+ (point-at-eol)) lim-down)))))
      ;; 1. Read list from starting item to its beginning, and save
      ;;    top item position and indentation in BEG-CELL.  Also store
      ;;    ending position of items in END-LST.
      (save-excursion
	(catch 'exit
	  (while t
	    (let ((ind (+ (or (get-text-property (point) 'original-indentation) 0)
			  (org-get-indentation))))
	      (cond
	       ((<= (point) lim-up)
		;; At upward limit: if we ended at an item, store it,
		;; else dismiss useless data recorded above BEG-CELL.
		;; Jump to part 2.
		(throw 'exit
		       (setq itm-lst
			     (if (or (not (looking-at item-re))
				     (get-text-property (point) 'org-example))
				 (memq (assq (car beg-cell) itm-lst) itm-lst)
			       (setq beg-cell (cons (point) ind))
			       (cons (funcall assoc-at-point ind) itm-lst)))))
	       ;; At a verbatim block, go before its beginning.  Move
	       ;; from eol to ensure `previous-single-property-change'
	       ;; will return a value.
	       ((get-text-property (point) 'org-example)
		(goto-char (previous-single-property-change
			    (point-at-eol) 'org-example nil lim-up))
		(forward-line -1))
	       ;; Looking at a list ending regexp.  Dismiss useless
	       ;; data recorded above BEG-CELL.  Jump to part 2.
	       ((looking-at org-list-end-re)
		(throw 'exit
		       (setq itm-lst
			     (memq (assq (car beg-cell) itm-lst) itm-lst))))
	       ;; Point is at an item.  Add data to ITM-LST. It may
	       ;; also end a previous item: save it in END-LST.  If
	       ;; ind is less or equal than BEG-CELL and there is no
	       ;; end at this ind or lesser, this item becomes the new
	       ;; BEG-CELL.
	       ((looking-at item-re)
		(push (funcall assoc-at-point ind) itm-lst)
		(push (cons ind (point)) end-lst)
		(when (< ind text-min-ind) (setq beg-cell (cons (point) ind)))
		(forward-line -1))
	       ;; Skip blocks, drawers, inline tasks, blank lines.
	       ((and (looking-at "^[ \t]*#\\+end_")
		     (re-search-backward "^[ \t]*#\\+begin_" lim-up t)))
	       ((and (looking-at "^[ \t]*:END:")
		     (re-search-backward drawers-re lim-up t))
		(beginning-of-line))
	       ((and inlinetask-re (looking-at inlinetask-re))
		(org-inlinetask-goto-beginning)
		(forward-line -1))
	       ((looking-at "^[ \t]*$")
		(forward-line -1))
	       ;; From there, point is not at an item. Interpret
	       ;; line's indentation:
	       ;; - text at column 0 is necessarily out of any list.
	       ;;   Dismiss data recorded above BEG-CELL.  Jump to
	       ;;   part 2.
	       ;; - any other case may be an ending position for an
	       ;;   hypothetical item above.  Store it and proceed.
	       ((zerop ind)
		(throw 'exit
		       (setq itm-lst
			     (memq (assq (car beg-cell) itm-lst) itm-lst))))
	       (t
		(when (< ind text-min-ind) (setq text-min-ind ind))
		(push (cons ind (point)) end-lst)
		(forward-line -1)))))))
      ;; 2. Read list from starting point to its end, that is until we
      ;;    get out of context, or that a non-item line is less or
      ;;    equally indented than BEG-CELL's cdr.  Also, store ending
      ;;    position of items in END-LST-2.
      (catch 'exit
	(while t
	  (let ((ind (+ (or (get-text-property (point) 'original-indentation) 0)
			(org-get-indentation))))
	    (cond
	     ((>= (point) lim-down)
	      ;; At downward limit: this is de facto the end of the
	      ;; list.  Save point as an ending position, and jump to
	      ;; part 3.
	      (throw 'exit
		     (push (cons 0 (funcall end-before-blank)) end-lst-2)))
	     ;; At a verbatim block, move to its end.  Point is at bol
	     ;; and 'org-example property is set by whole lines:
	     ;; `next-single-property-change' always return a value.
	     ((get-text-property (point) 'org-example)
	      (goto-char
	       (next-single-property-change (point) 'org-example nil lim-down)))
	     ;; Looking at a list ending regexp.  Save point as an
	     ;; ending position and jump to part 3.
	     ((looking-at org-list-end-re)
	      (throw 'exit (push (cons 0 (point)) end-lst-2)))
	     ((looking-at item-re)
	      ;; Point is at an item.  Add data to ITM-LST-2. It may
	      ;; also end a previous item, so save it in END-LST-2.
	      (push (funcall assoc-at-point ind) itm-lst-2)
	      (push (cons ind (point)) end-lst-2)
	      (forward-line 1))
	     ;; Skip inline tasks and blank lines along the way
	     ((and inlinetask-re (looking-at inlinetask-re))
	      (org-inlinetask-goto-end))
	     ((looking-at "^[ \t]*$")
	      (forward-line 1))
	     ;; Ind is lesser or equal than BEG-CELL's.  The list is
	     ;; over: store point as an ending position and jump to
	     ;; part 3.
	     ((<= ind (cdr beg-cell))
	      (throw 'exit
		     (push (cons 0 (funcall end-before-blank)) end-lst-2)))
	     ;; Else, if ind is lesser or equal than previous item's,
	     ;; this is an ending position: store it.  In any case,
	     ;; skip block or drawer at point, and move to next line.
	     (t
	      (when (<= ind (nth 1 (car itm-lst-2)))
		(push (cons ind (point)) end-lst-2))
	      (cond
	       ((and (looking-at "^[ \t]*#\\+begin_")
		     (re-search-forward "^[ \t]*#\\+end_" lim-down t)))
	       ((and (looking-at drawers-re)
		     (re-search-forward "^[ \t]*:END:" lim-down t))))
	      (forward-line 1))))))
      (setq struct (append itm-lst (cdr (nreverse itm-lst-2)))
	    end-lst (append end-lst (cdr (nreverse end-lst-2))))
      ;; 3. Associate each item to its end position.
      (org-list-struct-assoc-end struct end-lst)
      ;; 4. Return STRUCT
      struct)))

(defun org-list-struct-assoc-end (struct end-list)
  "Associate proper ending point to items in STRUCT.

END-LIST is a pseudo-alist where car is indentation and cdr is
ending position.

This function modifies STRUCT."
  (let ((endings end-list))
    (mapc
     (lambda (elt)
       (let ((pos (car elt))
	     (ind (nth 1 elt)))
	 ;; Remove end candidates behind current item.
	 (while (or (<= (cdar endings) pos))
	   (pop endings))
	 ;; Add end position to item assoc.
	 (let ((old-end (nthcdr 6 elt))
	       (new-end (assoc-default ind endings '<=)))
	   (if old-end
	       (setcar old-end new-end)
	     (setcdr elt (append (cdr elt) (list new-end)))))))
     struct)))

(defun org-list-prevs-alist (struct)
  "Return alist between item and previous item in STRUCT."
  (let ((item-end-alist (mapcar (lambda (e) (cons (car e) (nth 6 e)))
				struct)))
    (mapcar (lambda (e)
	      (let ((prev (car (rassq (car e) item-end-alist))))
		(cons (car e) prev)))
	    struct)))

(defun org-list-parents-alist (struct)
  "Return alist between item and parent in STRUCT."
  (let* ((ind-to-ori (list (list (nth 1 (car struct)))))
	 (top-item (org-list-get-top-point struct))
	 (prev-pos (list top-item)))
    (cons prev-pos
	  (mapcar (lambda (item)
		    (let ((pos (car item))
			  (ind (nth 1 item))
			  (prev-ind (caar ind-to-ori)))
		      (push pos prev-pos)
		      (cond
		       ((> prev-ind ind)
			;; A sub-list is over.  Find the associated
			;; origin in IND-TO-ORI.  If it cannot be
			;; found (ill-formed list), set its parent as
			;; the first item less indented.  If there is
			;; none, make it a top-level item.
			(setq ind-to-ori
			      (or (member (assq ind ind-to-ori) ind-to-ori)
                                  (catch 'exit
                                    (mapc
                                     (lambda (e)
                                       (when (< (car e) ind)
                                         (throw 'exit (member e ind-to-ori))))
                                     ind-to-ori)
                                    (list (list ind)))))
			(cons pos (cdar ind-to-ori)))
                       ;; A sub-list starts.  Every item at IND will
                       ;; have previous item as its parent.
		       ((< prev-ind ind)
			(let ((origin (nth 1 prev-pos)))
			  (push (cons ind origin) ind-to-ori)
			  (cons pos origin)))
                       ;; Another item in the same sub-list: it shares
                       ;; the same parent as the previous item.
		       (t (cons pos (cdar ind-to-ori))))))
		  (cdr struct)))))



;;; Accessors

(defsubst org-list-get-nth (n key struct)
  "Return the Nth value of KEY in STRUCT."
  (nth n (assq key struct)))

(defun org-list-set-nth (n key struct new)
  "Set the Nth value of KEY in STRUCT to NEW.
\nThis function modifies STRUCT."
  (setcar (nthcdr n (assq key struct)) new))

(defsubst org-list-get-ind (item struct)
  "Return indentation of ITEM in STRUCT."
  (org-list-get-nth 1 item struct))

(defun org-list-set-ind (item struct ind)
  "Set indentation of ITEM in STRUCT to IND.
\nThis function modifies STRUCT."
  (org-list-set-nth 1 item struct ind))

(defsubst org-list-get-bullet (item struct)
  "Return bullet of ITEM in STRUCT."
  (org-list-get-nth 2 item struct))

(defun org-list-set-bullet (item struct bullet)
  "Set bullet of ITEM in STRUCT to BULLET.
\nThis function modifies STRUCT."
  (org-list-set-nth 2 item struct bullet))

(defsubst org-list-get-counter (item struct)
  "Return counter of ITEM in STRUCT."
  (org-list-get-nth 3 item struct))

(defsubst org-list-get-checkbox (item struct)
  "Return checkbox of ITEM in STRUCT or nil."
  (org-list-get-nth 4 item struct))

(defun org-list-set-checkbox (item struct checkbox)
  "Set checkbox of ITEM in STRUCT to CHECKBOX.
\nThis function modifies STRUCT."
  (org-list-set-nth 4 item struct checkbox))

(defsubst org-list-get-tag (item struct)
  "Return end position of ITEM in STRUCT."
  (org-list-get-nth 5 item struct))

(defun org-list-get-item-end (item struct)
  "Return end position of ITEM in STRUCT."
  (org-list-get-nth 6 item struct))

(defun org-list-get-item-end-before-blank (item struct)
  "Return point at end of ITEM in STRUCT, before any blank line.
Point returned is at end of line."
  (save-excursion
    (goto-char (org-list-get-item-end item struct))
    (skip-chars-backward " \r\t\n")
    (point-at-eol)))

(defun org-list-get-parent (item struct parents)
  "Return parent of ITEM or nil.
STRUCT is the list structure.  PARENTS is the alist of parents,
as returned by `org-list-parents-alist'."
  (let ((parents (or parents (org-list-parents-alist struct))))
    (cdr (assq item parents))))

(defun org-list-has-child-p (item struct)
  "Non-nil if ITEM has a child.

STRUCT is the list structure.

Value returned is the position of the first child of ITEM."
  (let ((ind (org-list-get-ind item struct))
	(child-maybe (car (nth 1 (member (assq item struct) struct)))))
    (when (and child-maybe
	       (< ind (org-list-get-ind child-maybe struct)))
      child-maybe)))

(defun org-list-get-next-item (item struct prevs)
  "Return next item in same sub-list as ITEM, or nil.
STRUCT is the list structure.  PREVS is the alist of previous
items, as returned by `org-list-prevs-alist'."
  (car (rassq item prevs)))

(defun org-list-get-prev-item (item struct prevs)
  "Return previous item in same sub-list as ITEM, or nil.
STRUCT is the list structure.  PREVS is the alist of previous
items, as returned by `org-list-prevs-alist'."
  (cdr (assq item prevs)))

(defun org-list-get-subtree (item struct)
  "List all items having ITEM as a common ancestor, or nil.
STRUCT is the list structure."
  (let* ((item-end (org-list-get-item-end item struct))
	 (sub-struct (cdr (member (assq item struct) struct)))
	 subtree)
    (catch 'exit
      (mapc (lambda (e)
	      (let ((pos (car e)))
		(if (< pos item-end) (push pos subtree) (throw 'exit nil))))
	    sub-struct))
    (nreverse subtree)))

(defun org-list-get-all-items (item struct prevs)
  "List all items in the same sub-list as ITEM.
STRUCT is the list structure.  PREVS is the alist of previous
items, as returned by `org-list-prevs-alist'."
  (let ((prev-item item)
	(next-item item)
	before-item after-item)
    (while (setq prev-item (org-list-get-prev-item prev-item struct prevs))
      (push prev-item before-item))
    (while (setq next-item (org-list-get-next-item next-item struct prevs))
      (push next-item after-item))
    (append before-item (list item) (nreverse after-item))))

(defun org-list-get-children (item struct parents)
  "List all children of ITEM, or nil.
STRUCT is the list structure.  PARENTS is the alist of parents,
as returned by `org-list-parents-alist'."
  (let (all child)
    (while (setq child (car (rassq item parents)))
      (setq parents (cdr (member (assq child parents) parents)))
      (push child all))
    (nreverse all)))

(defun org-list-get-top-point (struct)
  "Return point at beginning of list.
STRUCT is the list structure."
  (caar struct))

(defun org-list-get-bottom-point (struct)
  "Return point at bottom of list.
STRUCT is the list structure."
  (apply 'max
	 (mapcar (lambda (e) (org-list-get-item-end (car e) struct)) struct)))

(defun org-list-get-list-begin (item struct prevs)
  "Return point at beginning of sub-list ITEM belongs.
STRUCT is the list structure.  PREVS is the alist of previous
items, as returned by `org-list-prevs-alist'."
  (let ((first-item item) prev-item)
    (while (setq prev-item (org-list-get-prev-item first-item struct prevs))
      (setq first-item prev-item))
    first-item))

(defalias 'org-list-get-first-item 'org-list-get-list-begin)

(defun org-list-get-last-item (item struct prevs)
  "Return point at last item of sub-list ITEM belongs.
STRUCT is the list structure.  PREVS is the alist of previous
items, as returned by `org-list-prevs-alist'."
  (let ((last-item item) next-item)
    (while (setq next-item (org-list-get-next-item last-item struct prevs))
      (setq last-item next-item))
    last-item))

(defun org-list-get-list-end (item struct prevs)
  "Return point at end of sub-list ITEM belongs.
STRUCT is the list structure.  PREVS is the alist of previous
items, as returned by `org-list-prevs-alist'."
  (org-list-get-item-end (org-list-get-last-item item struct prevs) struct))

(defun org-list-get-list-type (item struct prevs)
  "Return the type of the list containing ITEM, as a symbol.

STRUCT is the list structure.  PREVS is the alist of previous
items, as returned by `org-list-prevs-alist'.

Possible types are `descriptive', `ordered' and `unordered'.  The
type is determined by the first item of the list."
  (let ((first (org-list-get-list-begin item struct prevs)))
    (cond
     ((org-list-get-tag first struct) 'descriptive)
     ((string-match "[[:alnum:]]" (org-list-get-bullet first struct)) 'ordered)
     (t 'unordered))))



;;; Searching

(defun org-list-search-generic (search re bound noerr)
  "Search a string in valid contexts for lists.
Arguments SEARCH, RE, BOUND and NOERR are similar to those used
in `re-search-forward'."
  (catch 'exit
    (let ((origin (point)))
      (while t
	;; 1. No match: return to origin or bound, depending on NOERR.
	(unless (funcall search re bound noerr)
	  (throw 'exit (and (goto-char (if (memq noerr '(t nil)) origin bound))
			    nil)))
	;; 2. Match in valid context: return point.  Else, continue
	;;    searching.
	(when (org-list-in-valid-context-p) (throw 'exit (point)))))))

(defun org-list-search-backward (regexp &optional bound noerror)
  "Like `re-search-backward' but stop only where lists are recognized.
Arguments REGEXP, BOUND and NOERROR are similar to those used in
`re-search-backward'."
  (org-list-search-generic #'re-search-backward
			   regexp (or bound (point-min)) noerror))

(defun org-list-search-forward (regexp &optional bound noerror)
  "Like `re-search-forward' but stop only where lists are recognized.
Arguments REGEXP, BOUND and NOERROR are similar to those used in
`re-search-forward'."
  (org-list-search-generic #'re-search-forward
			   regexp (or bound (point-max)) noerror))



;;; Methods on structures

(defsubst org-list-bullet-string (bullet)
  "Return BULLET with the correct number of whitespaces.
It determines the number of whitespaces to append by looking at
`org-list-two-spaces-after-bullet-regexp'."
  (save-match-data
    (let ((spaces (if (and org-list-two-spaces-after-bullet-regexp
			   (string-match
			    org-list-two-spaces-after-bullet-regexp bullet))
		      "  "
		    " ")))
      (string-match "\\S-+\\([ \t]*\\)" bullet)
      (replace-match spaces nil nil bullet 1))))

(defun org-list-swap-items (beg-A beg-B struct)
  "Swap item starting at BEG-A with item starting at BEG-B in STRUCT.

Blank lines at the end of items are left in place.  Item
visibility is preserved.  Return the new structure after the
changes.

Assume BEG-A is lesser than BEG-B and that BEG-A and BEG-B belong
to the same sub-list.

This function modifies STRUCT."
  (save-excursion
    (let* ((end-A-no-blank (org-list-get-item-end-before-blank beg-A struct))
	   (end-B-no-blank (org-list-get-item-end-before-blank beg-B struct))
	   (end-A (org-list-get-item-end beg-A struct))
	   (end-B (org-list-get-item-end beg-B struct))
	   (size-A (- end-A-no-blank beg-A))
	   (size-B (- end-B-no-blank beg-B))
	   (body-A (buffer-substring beg-A end-A-no-blank))
	   (body-B (buffer-substring beg-B end-B-no-blank))
	   (between-A-no-blank-and-B (buffer-substring end-A-no-blank beg-B))
	   (sub-A (cons beg-A (org-list-get-subtree beg-A struct)))
	   (sub-B (cons beg-B (org-list-get-subtree beg-B struct)))
	   ;; Store overlays responsible for visibility status.  We
	   ;; also need to store their boundaries as they will be
	   ;; removed from buffer.
	   (overlays (cons
		      (mapcar (lambda (ov)
				(list ov (overlay-start ov) (overlay-end ov)))
			      (overlays-in beg-A end-A))
		      (mapcar (lambda (ov)
				(list ov (overlay-start ov) (overlay-end ov)))
			      (overlays-in beg-B end-B)))))
      ;; 1. Move effectively items in buffer.
      (goto-char beg-A)
      (delete-region beg-A end-B-no-blank)
      (insert (concat body-B between-A-no-blank-and-B body-A))
      ;; 2. Now modify struct.  No need to re-read the list, the
      ;;    transformation is just a shift of positions.  Some special
      ;;    attention is required for items ending at END-A and END-B
      ;;    as empty spaces are not moved there.  In others words,
      ;;    item BEG-A will end with whitespaces that were at the end
      ;;    of BEG-B and the same applies to BEG-B.
      (mapc (lambda (e)
	      (let ((pos (car e)))
		(cond
		 ((< pos beg-A))
		 ((memq pos sub-A)
		  (let ((end-e (nth 6 e)))
		    (setcar e (+ pos (- end-B-no-blank end-A-no-blank)))
		    (setcar (nthcdr 6 e)
			    (+ end-e (- end-B-no-blank end-A-no-blank)))
		    (when (= end-e end-A) (setcar (nthcdr 6 e) end-B))))
		 ((memq pos sub-B)
		  (let ((end-e (nth 6 e)))
		    (setcar e (- (+ pos beg-A) beg-B))
		    (setcar (nthcdr 6 e) (+ end-e (- beg-A beg-B)))
		    (when (= end-e end-B)
		      (setcar (nthcdr 6 e)
			      (+ beg-A size-B (- end-A end-A-no-blank))))))
		 ((< pos beg-B)
		  (let ((end-e (nth 6 e)))
		    (setcar e (+ pos (- size-B size-A)))
		    (setcar (nthcdr 6 e) (+ end-e (- size-B size-A))))))))
	    struct)
      (setq struct (sort struct (lambda (e1 e2) (< (car e1) (car e2)))))
      ;; Restore visibility status, by moving overlays to their new
      ;; position.
      (mapc (lambda (ov)
	      (move-overlay
	       (car ov)
	       (+ (nth 1 ov) (- (+ beg-B (- size-B size-A)) beg-A))
	       (+ (nth 2 ov) (- (+ beg-B (- size-B size-A)) beg-A))))
	    (car overlays))
      (mapc (lambda (ov)
	      (move-overlay (car ov)
			    (+ (nth 1 ov) (- beg-A beg-B))
			    (+ (nth 2 ov) (- beg-A beg-B))))
	    (cdr overlays))
      ;; Return structure.
      struct)))

(defun org-list-separating-blank-lines-number (pos struct prevs)
  "Return number of blank lines that should separate items in list.

POS is the position of point where `org-list-insert-item' was called.

STRUCT is the list structure.  PREVS is the alist of previous
items, as returned by `org-list-prevs-alist'.

Assume point is at item's beginning.  If the item is alone, apply
some heuristics to guess the result."
  (save-excursion
    (let ((item (point))
	  (insert-blank-p
	   (cdr (assq 'plain-list-item org-blank-before-new-entry)))
	  usr-blank
	  (count-blanks
	   (function
	    (lambda ()
	      ;; Count blank lines above beginning of line.
	      (save-excursion
		(count-lines (goto-char (point-at-bol))
			     (progn (skip-chars-backward " \r\t\n")
				    (forward-line)
				    (point))))))))
      (cond
       ;; Trivial cases where there should be none.
       ((or org-empty-line-terminates-plain-lists (not insert-blank-p)) 0)
       ;; When `org-blank-before-new-entry' says so, it is 1.
       ((eq insert-blank-p t) 1)
       ;; `plain-list-item' is 'auto.  Count blank lines separating
       ;; neighbours items in list.
       (t (let ((next-p (org-list-get-next-item item struct prevs)))
	    (cond
	     ;; Is there a next item?
	     (next-p (goto-char next-p)
		     (funcall count-blanks))
	     ;; Is there a previous item?
	     ((org-list-get-prev-item item struct prevs)
	      (funcall count-blanks))
	     ;; User inserted blank lines, trust him.
	     ((and (> pos (org-list-get-item-end-before-blank item struct))
		   (> (save-excursion (goto-char pos)
				      (setq usr-blank (funcall count-blanks)))
		      0))
	      usr-blank)
	     ;; Are there blank lines inside the list so far?
	     ((save-excursion
		(goto-char (org-list-get-top-point struct))
		(org-list-search-forward
		 "^[ \t]*$" (org-list-get-item-end-before-blank item struct) t))
	      1)
	     ;; Default choice: no blank line.
	     (t 0))))))))

(defun org-list-insert-item (pos struct prevs &optional checkbox after-bullet)
  "Insert a new list item at POS and return the new structure.
If POS is before first character after bullet of the item, the
new item will be created before the current one.

STRUCT is the list structure.  PREVS is the the alist of previous
items, as returned by `org-list-prevs-alist'.

Insert a checkbox if CHECKBOX is non-nil, and string AFTER-BULLET
after the bullet.  Cursor will be after this text once the
function ends.

This function modifies STRUCT."
  (let ((case-fold-search t))
    ;; 1. Get information about list: position of point with regards
    ;;    to item start (BEFOREP), blank lines number separating items
    ;;    (BLANK-NB), if we're allowed to (SPLIT-LINE-P).
    (let* ((item (progn (goto-char pos) (goto-char (org-list-get-item-begin))))
	   (item-end (org-list-get-item-end item struct))
	   (item-end-no-blank (org-list-get-item-end-before-blank item struct))
	   (beforep (and (looking-at org-list-full-item-re)
			 (<= pos (match-end 0))))
	   (split-line-p (org-get-alist-option org-M-RET-may-split-line 'item))
	   (blank-nb (org-list-separating-blank-lines-number
		      pos struct prevs))
	   ;; 2. Build the new item to be created.  Concatenate same
	   ;;    bullet as item, checkbox, text AFTER-BULLET if
	   ;;    provided, and text cut from point to end of item
	   ;;    (TEXT-CUT) to form item's BODY.  TEXT-CUT depends on
	   ;;    BEFOREP and SPLIT-LINE-P.  The difference of size
	   ;;    between what was cut and what was inserted in buffer
	   ;;    is stored in SIZE-OFFSET.
	   (ind (org-list-get-ind item struct))
	   (ind-size (if indent-tabs-mode
			 (+ (/ ind tab-width) (mod ind tab-width))
		       ind))
	   (bullet (org-list-bullet-string (org-list-get-bullet item struct)))
	   (box (when checkbox "[ ]"))
	   (text-cut
	    (and (not beforep) split-line-p
		 (progn
		   (goto-char pos)
		   ;; If POS is greater than ITEM-END, then point is
		   ;; in some white lines after the end of the list.
		   ;; Those must be removed, or they will be left,
		   ;; stacking up after the list.
		   (when (< item-end pos)
		     (delete-region (1- item-end) (point-at-eol)))
		   (skip-chars-backward " \r\t\n")
		   (setq pos (point))
		   (delete-and-extract-region pos item-end-no-blank))))
	   (body (concat bullet (when box (concat box " ")) after-bullet
			 (and text-cut
			      (if (string-match "\\`[ \t]+" text-cut)
				  (replace-match "" t t text-cut)
				text-cut))))
	   (item-sep (make-string  (1+ blank-nb) ?\n))
	   (item-size (+ ind-size (length body) (length item-sep)))
	   (size-offset (- item-size (length text-cut))))
      ;; 4. Insert effectively item into buffer.
      (goto-char item)
      (org-indent-to-column ind)
      (insert body item-sep)
      ;; 5. Add new item to STRUCT.
      (mapc (lambda (e)
      	      (let ((p (car e))
      		    (end (nth 6 e)))
      		(cond
		 ;; Before inserted item, positions don't change but
		 ;; an item ending after insertion has its end shifted
		 ;; by SIZE-OFFSET.
		 ((< p item)
		  (when (> end item) (setcar (nthcdr 6 e) (+ end size-offset))))
		 ;; Trivial cases where current item isn't split in
		 ;; two.  Just shift every item after new one by
		 ;; ITEM-SIZE.
		 ((or beforep (not split-line-p))
		  (setcar e (+ p item-size))
		  (setcar (nthcdr 6 e) (+ end item-size)))
		 ;; Item is split in two: elements before POS are just
		 ;; shifted by ITEM-SIZE.  In the case item would end
		 ;; after split POS, ending is only shifted by
		 ;; SIZE-OFFSET.
		 ((< p pos)
		  (setcar e (+ p item-size))
		  (if (< end pos)
		      (setcar (nthcdr 6 e) (+ end item-size))
		    (setcar (nthcdr 6 e) (+ end size-offset))))
		 ;; Elements after POS are moved into new item.
		 ;; Length of ITEM-SEP has to be removed as ITEM-SEP
		 ;; doesn't appear in buffer yet.
		 ((< p item-end)
		  (setcar e (+ p size-offset (- item pos (length item-sep))))
		  (if (= end item-end)
		      (setcar (nthcdr 6 e) (+ item item-size))
		    (setcar (nthcdr 6 e)
			    (+ end size-offset
			       (- item pos (length item-sep))))))
		 ;; Elements at ITEM-END or after are only shifted by
		 ;; SIZE-OFFSET.
		 (t (setcar e (+ p size-offset))
		    (setcar (nthcdr 6 e) (+ end size-offset))))))
	    struct)
      (push (list item ind bullet nil box nil (+ item item-size)) struct)
      (setq struct (sort struct (lambda (e1 e2) (< (car e1) (car e2)))))
      ;; 6. If not BEFOREP, new item must appear after ITEM, so
      ;; exchange ITEM with the next item in list.  Position cursor
      ;; after bullet, counter, checkbox, and label.
      (if beforep
	  (goto-char item)
	(setq struct (org-list-swap-items item (+ item item-size) struct))
	(goto-char (org-list-get-next-item
		    item struct (org-list-prevs-alist struct))))
      struct)))

(defun org-list-delete-item (item struct)
  "Remove ITEM from the list and return the new structure.

STRUCT is the list structure."
  (let* ((end (org-list-get-item-end item struct))
	 (beg (if (= (org-list-get-bottom-point struct) end)
		  ;; If ITEM ends with the list, delete blank lines
		  ;; before it.
		  (save-excursion
		    (goto-char item)
		    (skip-chars-backward " \r\t\n")
		    (min (1+ (point-at-eol)) (point-max)))
		item)))
    ;; Remove item from buffer.
    (delete-region beg end)
    ;; Remove item from structure and shift others items accordingly.
    ;; Don't forget to shift also ending position when appropriate.
    (let ((size (- end beg)))
      (delq nil (mapcar (lambda (e)
			  (let ((pos (car e)))
			    (cond
			     ((< pos item)
			      (let ((end-e (nth 6 e)))
				(cond
				 ((< end-e item) e)
				 ((= end-e item)
				  (append (butlast e) (list beg)))
				 (t
				  (append (butlast e) (list (- end-e size)))))))
			     ((< pos end) nil)
			     (t
			      (cons (- pos size)
				    (append (butlast (cdr e))
					    (list (- (nth 6 e) size))))))))
			struct)))))

(defun org-list-send-item (item dest struct)
  "Send ITEM to destination DEST.

STRUCT is the list structure.

DEST can have various values.

If DEST is a buffer position, the function will assume it points
to another item in the same list as ITEM, and will move the
latter just before the former.

If DEST is `begin' (respectively `end'), ITEM will be moved at
the beginning (respectively end) of the list it belongs to.

If DEST is a string like \"N\", where N is an integer, ITEM will
be moved at the Nth position in the list.

If DEST is `kill', ITEM will be deleted and its body will be
added to the kill-ring.

If DEST is `delete', ITEM will be deleted.

Visibility of item is preserved.

This function returns, destructively, the new list structure."
  (let* ((prevs (org-list-prevs-alist struct))
	 (item-end (org-list-get-item-end item struct))
	 ;; Grab full item body minus its bullet.
	 (body (org-trim
		(buffer-substring
		 (save-excursion
		   (goto-char item)
		   (looking-at
		    (concat "[ \t]*"
			    (regexp-quote (org-list-get-bullet item struct))))
		   (match-end 0))
		 item-end)))
	 ;; Change DEST into a buffer position.  A trick is needed
	 ;; when ITEM is meant to be sent at the end of the list.
	 ;; Indeed, by setting locally `org-M-RET-may-split-line' to
	 ;; nil and insertion point (INS-POINT) to the first line's
	 ;; end of the last item, we ensure the new item will be
	 ;; inserted after the last item, and not after any of its
	 ;; hypothetical sub-items.
	 (ins-point (cond
		     ((or (eq dest 'kill) (eq dest 'delete)))
		     ((eq dest 'begin)
		      (setq dest (org-list-get-list-begin item struct prevs)))
		     ((eq dest 'end)
		      (setq dest (org-list-get-list-end item struct prevs))
		      (save-excursion
			(goto-char (org-list-get-last-item item struct prevs))
			(point-at-eol)))
		     ((string-match "\\`[0-9]+\\'" dest)
		      (let* ((all (org-list-get-all-items item struct prevs))
			     (len (length all))
			     (index (mod (string-to-number dest) len)))
			(if (not (zerop index))
			    (setq dest (nth (1- index) all))
			  ;; Send ITEM at the end of the list.
			  (setq dest (org-list-get-list-end item struct prevs))
			  (save-excursion
			    (goto-char
			     (org-list-get-last-item item struct prevs))
			    (point-at-eol)))))
		     (t dest)))
	 (org-M-RET-may-split-line nil)
	 ;; Store visibility.
	 (visibility (overlays-in item item-end)))
    (cond
     ((eq dest 'delete) (org-list-delete-item item struct))
     ((eq dest 'kill)
      (kill-new body)
      (org-list-delete-item item struct))
     ((and (integerp dest) (/= item ins-point))
      (setq item (copy-marker item))
      (setq struct (org-list-insert-item ins-point struct prevs nil body))
      ;; 1. Structure returned by `org-list-insert-item' may not be
      ;;    accurate, as it cannot see sub-items included in BODY.
      ;;    Thus, first compute the real structure so far.
      (let ((moved-items
	     (cons (marker-position item)
		   (org-list-get-subtree (marker-position item) struct)))
	    (new-end (org-list-get-item-end (point) struct))
	    (old-end (org-list-get-item-end (marker-position item) struct))
	    (new-item (point))
	    (shift (- (point) item)))
	;; 1.1. Remove the item just created in structure.
	(setq struct (delete (assq new-item struct) struct))
	;; 1.2. Copy ITEM and any of its sub-items at NEW-ITEM.
	(setq struct (sort
		      (append
		       struct
		       (mapcar (lambda (e)
				 (let* ((cell (assq e struct))
					(pos (car cell))
					(end (nth 6 cell)))
				   (cons (+ pos shift)
					 (append (butlast (cdr cell))
						 (list (if (= end old-end)
							   new-end
							 (+ end shift)))))))
			       moved-items))
		      (lambda (e1 e2) (< (car e1) (car e2))))))
      ;; 2. Restore visibility.
      (mapc (lambda (ov)
	      (move-overlay ov
			    (+ (overlay-start ov) (- (point) item))
			    (+ (overlay-end ov) (- (point) item))))
	    visibility)
      ;; 3. Eventually delete extra copy of the item and clean marker.
      (prog1 (org-list-delete-item (marker-position item) struct)
	(move-marker item nil)))
     (t struct))))

(defun org-list-struct-outdent (start end struct parents)
  "Outdent items between positions START and END.

STRUCT is the list structure.  PARENTS is the alist of items'
parents, as returned by `org-list-parents-alist'.

START is included, END excluded."
  (let* (acc
	 (out (lambda (cell)
		(let* ((item (car cell))
		       (parent (cdr cell)))
		  (cond
		   ;; Item not yet in zone: keep association.
		   ((< item start) cell)
		   ;; Item out of zone: follow associations in ACC.
		   ((>= item end)
		    (let ((convert (and parent (assq parent acc))))
		      (if convert (cons item (cdr convert)) cell)))
		   ;; Item has no parent: error
		   ((not parent)
		    (error "Cannot outdent top-level items"))
		   ;; Parent is outdented: keep association.
		   ((>= parent start)
		    (push (cons parent item) acc) cell)
		   (t
		    ;; Parent isn't outdented: reparent to grand-parent.
		    (let ((grand-parent (org-list-get-parent
					 parent struct parents)))
		      (push (cons parent item) acc)
		      (cons item grand-parent))))))))
    (mapcar out parents)))

(defun org-list-struct-indent (start end struct parents prevs)
  "Indent items between positions START and END.

STRUCT is the list structure.  PARENTS is the alist of parents
and PREVS is the alist of previous items, returned by,
respectively, `org-list-parents-alist' and
`org-list-prevs-alist'.

START is included and END excluded.

STRUCT may be modified if `org-list-demote-modify-bullet' matches
bullets between START and END."
  (let* (acc
	 (set-assoc (lambda (cell) (push cell acc) cell))
	 (change-bullet-maybe
	  (function
	   (lambda (item)
	     (let ((new-bul-p
		    (cdr (assoc
			  ;; Normalize ordered bullets.
			  (let ((bul (org-trim
				      (org-list-get-bullet item struct))))
			    (cond ((string-match "[A-Z]\\." bul) "A.")
				  ((string-match "[A-Z])" bul) "A)")
				  ((string-match "[a-z]\\." bul) "a.")
				  ((string-match "[a-z])" bul) "a)")
				  ((string-match "[0-9]\\." bul) "1.")
				  ((string-match "[0-9])" bul) "1)")
				  (t bul)))
			  org-list-demote-modify-bullet))))
	       (when new-bul-p (org-list-set-bullet item struct new-bul-p))))))
	 (ind
	  (lambda (cell)
	    (let* ((item (car cell))
		   (parent (cdr cell)))
	      (cond
	       ;; Item not yet in zone: keep association.
	       ((< item start) cell)
	       ((>= item end)
		;; Item out of zone: follow associations in ACC.
		(let ((convert (assq parent acc)))
		  (if convert (cons item (cdr convert)) cell)))
	       (t
		;; Item is in zone...
		(let ((prev (org-list-get-prev-item item struct prevs)))
		  ;; Check if bullet needs to be changed.
		  (funcall change-bullet-maybe item)
		  (cond
		   ;; First item indented but not parent: error
		   ((and (not prev) (< parent start))
		    (error "Cannot indent the first item of a list"))
		   ;; First item and parent indented: keep same
		   ;; parent.
		   ((not prev) (funcall set-assoc cell))
		   ;; Previous item not indented: reparent to it.
		   ((< prev start) (funcall set-assoc (cons item prev)))
		   ;; Previous item indented: reparent like it.
		   (t
		    (funcall set-assoc
			     (cons item (cdr (assq prev acc)))))))))))))
    (mapcar ind parents)))



;;; Repairing structures

(defun org-list-use-alpha-bul-p (first struct prevs)
  "Non-nil if list starting at FIRST can have alphabetical bullets.

STRUCT is list structure.  PREVS is the alist of previous items,
as returned by `org-list-prevs-alist'."
  (and org-alphabetical-lists
       (catch 'exit
	 (let ((item first) (ascii 64) (case-fold-search nil))
	   ;; Pretend that bullets are uppercase and check if alphabet
	   ;; is sufficient, taking counters into account.
	   (while item
	     (let ((bul (org-list-get-bullet item struct))
		   (count (org-list-get-counter item struct)))
	       ;; Virtually determine current bullet
	       (if (and count (string-match "[a-zA-Z]" count))
		   ;; Counters are not case-sensitive.
		   (setq ascii (string-to-char (upcase count)))
		 (setq ascii (1+ ascii)))
	       ;; Test if bullet would be over z or Z.
	       (if (> ascii 90)
		   (throw 'exit nil)
		 (setq item (org-list-get-next-item item struct prevs)))))
	   ;; All items checked. All good.
	   t))))

(defun org-list-inc-bullet-maybe (bullet)
  "Increment BULLET if applicable."
  (let ((case-fold-search nil))
    (cond
     ;; Num bullet: increment it.
     ((string-match "[0-9]+" bullet)
      (replace-match
       (number-to-string (1+ (string-to-number (match-string 0 bullet))))
       nil nil bullet))
     ;; Alpha bullet: increment it.
     ((string-match "[A-Za-z]" bullet)
      (replace-match
       (char-to-string (1+ (string-to-char (match-string 0 bullet))))
       nil nil bullet))
     ;; Unordered bullet: leave it.
     (t bullet))))

(defun org-list-struct-fix-bul (struct prevs)
  "Verify and correct bullets in STRUCT.
PREVS is the alist of previous items, as returned by
`org-list-prevs-alist'.

This function modifies STRUCT."
  (let ((case-fold-search nil)
	(fix-bul
	 (function
	  ;; Set bullet of ITEM in STRUCT, depending on the type of
	  ;; first item of the list, the previous bullet and counter
	  ;; if any.
	  (lambda (item)
	    (let* ((prev (org-list-get-prev-item item struct prevs))
		   (prev-bul (and prev (org-list-get-bullet prev struct)))
		   (counter (org-list-get-counter item struct))
		   (bullet (org-list-get-bullet item struct))
		   (alphap (and (not prev)
				(org-list-use-alpha-bul-p item struct prevs))))
	      (org-list-set-bullet
	       item struct
	       (org-list-bullet-string
		(cond
		 ;; Alpha counter in alpha list: use counter.
		 ((and prev counter
		       (string-match "[a-zA-Z]" counter)
		       (string-match "[a-zA-Z]" prev-bul))
		  ;; Use cond to be sure `string-match' is used in
		  ;; both cases.
		  (let ((real-count
			 (cond
			  ((string-match "[a-z]" prev-bul) (downcase counter))
			  ((string-match "[A-Z]" prev-bul) (upcase counter)))))
		    (replace-match real-count nil nil prev-bul)))
		 ;; Num counter in a num list: use counter.
		 ((and prev counter
		       (string-match "[0-9]+" counter)
		       (string-match "[0-9]+" prev-bul))
		  (replace-match counter nil nil prev-bul))
		 ;; No counter: increase, if needed, previous bullet.
		 (prev
		  (org-list-inc-bullet-maybe (org-list-get-bullet prev struct)))
		 ;; Alpha counter at first item: use counter.
		 ((and counter (org-list-use-alpha-bul-p item struct prevs)
		       (string-match "[A-Za-z]" counter)
		       (string-match "[A-Za-z]" bullet))
		  (let ((real-count
			 (cond
			  ((string-match "[a-z]" bullet) (downcase counter))
			  ((string-match "[A-Z]" bullet) (upcase counter)))))
		    (replace-match real-count nil nil bullet)))
		 ;; Num counter at first item: use counter.
		 ((and counter
		       (string-match "[0-9]+" counter)
		       (string-match "[0-9]+" bullet))
		  (replace-match counter nil nil bullet))
		 ;; First bullet is alpha uppercase: use "A".
		 ((and alphap (string-match "[A-Z]" bullet))
		  (replace-match "A" nil nil bullet))
		 ;; First bullet is alpha lowercase: use "a".
		 ((and alphap (string-match "[a-z]" bullet))
		  (replace-match "a" nil nil bullet))
		 ;; First bullet is num: use "1".
		 ((string-match "\\([0-9]+\\|[A-Za-z]\\)" bullet)
		  (replace-match "1" nil nil bullet))
		 ;; Not an ordered list: keep bullet.
		 (t bullet)))))))))
    (mapc fix-bul (mapcar 'car struct))))

(defun org-list-struct-fix-ind (struct parents &optional bullet-size)
  "Verify and correct indentation in STRUCT.

PARENTS is the alist of parents, as returned by
`org-list-parents-alist'.

If numeric optional argument BULLET-SIZE is set, assume all
bullets in list have this length to determine new indentation.

This function modifies STRUCT."
  (let* ((ancestor (org-list-get-top-point struct))
         (top-ind (org-list-get-ind ancestor struct))
         (new-ind
          (lambda (item)
            (let ((parent (org-list-get-parent item struct parents)))
              (if parent
                  ;; Indent like parent + length of parent's bullet +
		  ;; sub-list offset.
                  (org-list-set-ind
		   item struct (+ (or bullet-size
				      (length
				       (org-list-get-bullet parent struct)))
				  (org-list-get-ind parent struct)
				  org-list-indent-offset))
                ;; If no parent, indent like top-point.
		(org-list-set-ind item struct top-ind))))))
    (mapc new-ind (mapcar 'car (cdr struct)))))

(defun org-list-struct-fix-box (struct parents prevs &optional ordered)
  "Verify and correct checkboxes in STRUCT.

PARENTS is the alist of parents and PREVS is the alist of
previous items, as returned by, respectively,
`org-list-parents-alist' and `org-list-prevs-alist'.

If ORDERED is non-nil, a checkbox can only be checked when every
checkbox before it is checked too.  If there was an attempt to
break this rule, the function will return the blocking item.  In
all others cases, the return value will be nil.

This function modifies STRUCT."
  (let ((all-items (mapcar 'car struct))
	(set-parent-box
	 (function
	  (lambda (item)
	    (let* ((box-list
		    (mapcar (lambda (child)
			      (org-list-get-checkbox child struct))
			    (org-list-get-children item struct parents))))
	      (org-list-set-checkbox
	       item struct
	       (cond
		((and (member "[ ]" box-list) (member "[X]" box-list)) "[-]")
		((member "[-]" box-list) "[-]")
		((member "[X]" box-list) "[X]")
		((member "[ ]" box-list) "[ ]")
		;; Parent has no boxed child: leave box as-is.
		(t (org-list-get-checkbox item struct))))))))
	parent-list)
    ;; 1. List all parents with a checkbox.
    (mapc
     (lambda (e)
       (let* ((parent (org-list-get-parent e struct parents))
	      (parent-box-p (org-list-get-checkbox parent struct)))
	 (when (and parent-box-p (not (memq parent parent-list)))
	   (push parent parent-list))))
     all-items)
    ;; 2. Sort those parents by decreasing indentation.
    (setq parent-list (sort parent-list
			    (lambda (e1 e2)
			      (> (org-list-get-ind e1 struct)
				 (org-list-get-ind e2 struct)))))
    ;; 3. For each parent, get all children's checkboxes to determine
    ;;    and set its checkbox accordingly.
    (mapc set-parent-box parent-list)
    ;; 4. If ORDERED is set, see if we need to uncheck some boxes.
    (when ordered
      (let* ((box-list
	      (mapcar (lambda (e) (org-list-get-checkbox e struct)) all-items))
	     (after-unchecked (member "[ ]" box-list)))
	;; There are boxes checked after an unchecked one: fix that.
	(when (member "[X]" after-unchecked)
	  (let ((index (- (length struct) (length after-unchecked))))
	    (mapc (lambda (e) (org-list-set-checkbox e struct "[ ]"))
		  (nthcdr index all-items))
	    ;; Verify once again the structure, without ORDERED.
	    (org-list-struct-fix-box struct parents prevs nil)
	    ;; Return blocking item.
	    (nth index all-items)))))))

(defun org-list-struct-fix-item-end (struct)
  "Verify and correct each item end position in STRUCT.

This function modifies STRUCT."
  (let (end-list acc-end)
    (mapc (lambda (e)
	    (let* ((pos (car e))
		   (ind-pos (org-list-get-ind pos struct))
		   (end-pos (org-list-get-item-end pos struct)))
	      (unless (assq end-pos struct)
		;; To determine real ind of an ending position that is
		;; not at an item, we have to find the item it belongs
		;; to: it is the last item (ITEM-UP), whose ending is
		;; further than the position we're interested in.
		(let ((item-up (assoc-default end-pos acc-end '>)))
		  (push (cons
			 ;; Else part is for the bottom point.
			 (if item-up (+ (org-list-get-ind item-up struct) 2) 0)
			 end-pos)
			end-list)))
	      (push (cons ind-pos pos) end-list)
	      (push (cons end-pos pos) acc-end)))
	  struct)
    (setq end-list (sort end-list (lambda (e1 e2) (< (cdr e1) (cdr e2)))))
    (org-list-struct-assoc-end struct end-list)))

(defun org-list-struct-apply-struct (struct old-struct)
  "Apply set difference between STRUCT and OLD-STRUCT to the buffer.

OLD-STRUCT is the structure before any modifications, and STRUCT
the structure to be applied.  The function will only modify parts
of the list which have changed.

Initial position of cursor is restored after the changes."
  (let* ((origin (point-marker))
	 (inlinetask-re (and (featurep 'org-inlinetask)
			     (org-inlinetask-outline-regexp)))
	 (item-re (org-item-re))
	 (box-rule-p (cdr (assq 'checkbox org-list-automatic-rules)))
	 (shift-body-ind
	  (function
	   ;; Shift the indentation between END and BEG by DELTA.
	   ;; Start from the line before END.
	   (lambda (end beg delta)
	     (goto-char end)
	     (skip-chars-backward " \r\t\n")
	     (beginning-of-line)
	     (while (or (> (point) beg)
			(and (= (point) beg)
			     (not (looking-at item-re))))
	       (cond
		;; Skip inline tasks.
		((and inlinetask-re (looking-at inlinetask-re))
		 (org-inlinetask-goto-beginning))
		;; Shift only non-empty lines.
		((org-looking-at-p "^[ \t]*\\S-")
		 (let ((i (org-get-indentation)))
		   (org-indent-line-to (+ i delta)))))
	       (forward-line -1)))))
         (modify-item
          (function
	   ;; Replace ITEM first line elements with new elements from
	   ;; STRUCT, if appropriate.
	   (lambda (item)
	     (goto-char item)
	     (let* ((new-ind (org-list-get-ind item struct))
		    (old-ind (org-get-indentation))
		    (new-bul (org-list-bullet-string
			      (org-list-get-bullet item struct)))
		    (old-bul (org-list-get-bullet item old-struct))
		    (new-box (org-list-get-checkbox item struct)))
	       (looking-at org-list-full-item-re)
	       ;; a. Replace bullet
	       (unless (equal old-bul new-bul)
		 (replace-match new-bul nil nil nil 1))
	       ;; b. Replace checkbox.
	       (cond
		((and new-box box-rule-p
		      (save-match-data (org-at-item-description-p)))
		 (message "Cannot add a checkbox to a description list item"))
		((equal (match-string 3) new-box))
		((and (match-string 3) new-box)
		 (replace-match new-box nil nil nil 3))
		((match-string 3)
		 (looking-at ".*?\\([ \t]*\\[[ X-]\\]\\)")
		 (replace-match "" nil nil nil 1))
		(t (let ((counterp (match-end 2)))
		     (goto-char (if counterp (1+ counterp) (match-end 1)))
		     (insert (concat new-box (unless counterp " "))))))
	       ;; c. Indent item to appropriate column.
	       (unless (= new-ind old-ind)
		 (delete-region (goto-char (point-at-bol))
				(progn (skip-chars-forward " \t") (point)))
		 (indent-to new-ind)))))))
    ;; 1. First get list of items and position endings.  We maintain
    ;;    two alists: ITM-SHIFT, determining indentation shift needed
    ;;    at item, and END-POS, a pseudo-alist where key is ending
    ;;    position and value point.
    (let (end-list acc-end itm-shift all-ends sliced-struct)
      (mapc (lambda (e)
	      (let* ((pos (car e))
		     (ind-pos (org-list-get-ind pos struct))
		     (ind-old (org-list-get-ind pos old-struct))
		     (bul-pos (org-list-get-bullet pos struct))
		     (bul-old (org-list-get-bullet pos old-struct))
		     (ind-shift (- (+ ind-pos (length bul-pos))
				   (+ ind-old (length bul-old))))
		     (end-pos (org-list-get-item-end pos old-struct)))
		(push (cons pos ind-shift) itm-shift)
		(unless (assq end-pos old-struct)
		  ;; To determine real ind of an ending position that
		  ;; is not at an item, we have to find the item it
		  ;; belongs to: it is the last item (ITEM-UP), whose
		  ;; ending is further than the position we're
		  ;; interested in.
		  (let ((item-up (assoc-default end-pos acc-end '>)))
		    (push (cons end-pos item-up) end-list)))
		(push (cons end-pos pos) acc-end)))
	    old-struct)
      ;; 2. Slice the items into parts that should be shifted by the
      ;;    same amount of indentation.  The slices are returned in
      ;;    reverse order so changes modifying buffer do not change
      ;;    positions they refer to.
      (setq all-ends (sort (append (mapcar 'car itm-shift)
				   (org-uniquify (mapcar 'car end-list)))
			   '<))
      (while (cdr all-ends)
	(let* ((up (pop all-ends))
	       (down (car all-ends))
	       (ind (if (assq up struct)
			(cdr (assq up itm-shift))
		      (cdr (assq (cdr (assq up end-list)) itm-shift)))))
	  (push (list down up ind) sliced-struct)))
      ;; 3. Shift each slice in buffer, provided delta isn't 0, from
      ;;    end to beginning.  Take a special action when beginning is
      ;;    at item bullet.
      (mapc (lambda (e)
	      (unless (zerop (nth 2 e)) (apply shift-body-ind e))
	      (let* ((beg (nth 1 e))
		     (cell (assq beg struct)))
		(unless (or (not cell) (equal cell (assq beg old-struct)))
		  (funcall modify-item beg))))
	    sliced-struct))
    ;; 4. Go back to initial position and clean marker.
    (goto-char origin)
    (move-marker origin nil)))

(defun org-list-write-struct (struct parents &optional old-struct)
  "Correct bullets, checkboxes and indentation in list at point.

STRUCT is the list structure.  PARENTS is the alist of parents,
as returned by `org-list-parents-alist'.

When non-nil, optional argument OLD-STRUCT is the reference
structure of the list.  It should be provided whenever STRUCT
doesn't correspond anymore to the real list in buffer."
  ;; Order of functions matters here: checkboxes and endings need
  ;; correct indentation to be set, and indentation needs correct
  ;; bullets.
  ;;
  ;; 0. Save a copy of structure before modifications
  (let ((old-struct (or old-struct (copy-tree struct))))
    ;; 1. Set a temporary, but coherent with PARENTS, indentation in
    ;;    order to get items endings and bullets properly
    (org-list-struct-fix-ind struct parents 2)
    ;; 2. Fix each item end to get correct prevs alist.
    (org-list-struct-fix-item-end struct)
    ;; 3. Get bullets right.
    (let ((prevs (org-list-prevs-alist struct)))
      (org-list-struct-fix-bul struct prevs)
      ;; 4. Now get real indentation.
      (org-list-struct-fix-ind struct parents)
      ;; 5. Eventually fix checkboxes.
      (org-list-struct-fix-box struct parents prevs))
    ;; 6. Apply structure modifications to buffer.
    (org-list-struct-apply-struct struct old-struct)))



;;; Misc Tools

(defun org-apply-on-list (function init-value &rest args)
  "Call FUNCTION on each item of the list at point.
FUNCTION must be called with at least one argument: INIT-VALUE,
that will contain the value returned by the function at the
previous item, plus ARGS extra arguments.

FUNCTION is applied on items in reverse order.

As an example, \(org-apply-on-list \(lambda \(result\) \(1+ result\)\) 0\)
will return the number of items in the current list.

Sublists of the list are skipped.  Cursor is always at the
beginning of the item."
  (let* ((struct (org-list-struct))
	 (prevs (org-list-prevs-alist struct))
	 (item (copy-marker (point-at-bol)))
	 (all (org-list-get-all-items (marker-position item) struct prevs))
	 (value init-value))
    (mapc (lambda (e)
	    (goto-char e)
	    (setq value (apply function value args)))
	  (nreverse all))
    (goto-char item)
    (move-marker item nil)
    value))

(defun org-list-set-item-visibility (item struct view)
  "Set visibility of ITEM in STRUCT to VIEW.

Possible values are: `folded', `children' or `subtree'.  See
`org-cycle' for more information."
  (cond
   ((eq view 'folded)
    (let ((item-end (org-list-get-item-end-before-blank item struct)))
      ;; Hide from eol
      (outline-flag-region (save-excursion (goto-char item) (point-at-eol))
			   item-end t)))
   ((eq view 'children)
    ;; First show everything.
    (org-list-set-item-visibility item struct 'subtree)
    ;; Then fold every child.
    (let* ((parents (org-list-parents-alist struct))
	   (children (org-list-get-children item struct parents)))
      (mapc (lambda (e)
	      (org-list-set-item-visibility e struct 'folded))
	    children)))
   ((eq view 'subtree)
    ;; Show everything
    (let ((item-end (org-list-get-item-end item struct)))
      (outline-flag-region item item-end nil)))))

(defun org-list-item-body-column (item)
  "Return column at which body of ITEM should start."
  (let (bpos bcol tpos tcol)
    (save-excursion
      (goto-char item)
      (looking-at "[ \t]*\\(\\S-+\\)\\(.*[ \t]+::\\)?[ \t]+")
      (setq bpos (match-beginning 1) tpos (match-end 0)
	    bcol (progn (goto-char bpos) (current-column))
	    tcol (progn (goto-char tpos) (current-column)))
      (when (> tcol (+ bcol org-description-max-indent))
	(setq tcol (+ bcol 5))))
    tcol))



;;; Interactive functions

(defalias 'org-list-get-item-begin 'org-in-item-p)

(defun org-beginning-of-item ()
  "Go to the beginning of the current item.
Throw an error when not in a list."
  (interactive)
  (let ((begin (org-in-item-p)))
    (if begin (goto-char begin) (error "Not in an item"))))

(defun org-beginning-of-item-list ()
  "Go to the beginning item of the current list or sublist.
Throw an error when not in a list."
  (interactive)
  (let ((begin (org-in-item-p)))
    (if (not begin)
	(error "Not in an item")
      (goto-char begin)
      (let* ((struct (org-list-struct))
	     (prevs (org-list-prevs-alist struct)))
	(goto-char (org-list-get-list-begin begin struct prevs))))))

(defun org-end-of-item-list ()
  "Go to the end of the current list or sublist.
Throw an error when not in a list."
  (interactive)
  (let ((begin (org-in-item-p)))
    (if (not begin)
	(error "Not in an item")
      (goto-char begin)
      (let* ((struct (org-list-struct))
	     (prevs (org-list-prevs-alist struct)))
	(goto-char (org-list-get-list-end begin struct prevs))))))

(defun org-end-of-item ()
  "Go to the end of the current item.
Throw an error when not in a list."
  (interactive)
  (let ((begin (org-in-item-p)))
    (if (not begin)
	(error "Not in an item")
      (goto-char begin)
      (let ((struct (org-list-struct)))
	(goto-char (org-list-get-item-end begin struct))))))

(defun org-previous-item ()
  "Move to the beginning of the previous item.
Throw an error when not in a list.  Also throw an error when at
first item, unless `org-list-use-circular-motion' is non-nil."
  (interactive)
  (let ((item (org-in-item-p)))
    (if (not item)
	(error "Not in an item")
      (goto-char item)
      (let* ((struct (org-list-struct))
	     (prevs (org-list-prevs-alist struct))
	     (prevp (org-list-get-prev-item item struct prevs)))
	(cond
	 (prevp (goto-char prevp))
	 (org-list-use-circular-motion
	  (goto-char (org-list-get-last-item item struct prevs)))
	 (t (error "On first item")))))))

(defun org-next-item ()
  "Move to the beginning of the next item.
Throw an error when not in a list.  Also throw an error when at
last item, unless `org-list-use-circular-motion' is non-nil."
  (interactive)
  (let ((item (org-in-item-p)))
    (if (not item)
	(error "Not in an item")
      (goto-char item)
      (let* ((struct (org-list-struct))
	     (prevs (org-list-prevs-alist struct))
	     (prevp (org-list-get-next-item item struct prevs)))
	(cond
	 (prevp (goto-char prevp))
	 (org-list-use-circular-motion
	  (goto-char (org-list-get-first-item item struct prevs)))
	 (t (error "On last item")))))))

(defun org-move-item-down ()
  "Move the item at point down, i.e. swap with following item.
Sub-items (items with larger indentation) are considered part of
the item, so this really moves item trees."
  (interactive)
  (unless (org-at-item-p) (error "Not at an item"))
  (let* ((col (current-column))
	 (item (point-at-bol))
	 (struct (org-list-struct))
	 (prevs (org-list-prevs-alist struct))
	 (next-item (org-list-get-next-item (point-at-bol) struct prevs)))
    (unless (or next-item org-list-use-circular-motion)
      (error "Cannot move this item further down"))
    (if (not next-item)
	(setq struct (org-list-send-item item 'begin struct))
      (setq struct (org-list-swap-items item next-item struct))
      (goto-char
       (org-list-get-next-item item struct (org-list-prevs-alist struct))))
    (org-list-write-struct struct (org-list-parents-alist struct))
    (org-move-to-column col)))

(defun org-move-item-up ()
  "Move the item at point up, i.e. swap with previous item.
Sub-items (items with larger indentation) are considered part of
the item, so this really moves item trees."
  (interactive)
  (unless (org-at-item-p) (error "Not at an item"))
  (let* ((col (current-column))
	 (item (point-at-bol))
	 (struct (org-list-struct))
	 (prevs (org-list-prevs-alist struct))
	 (prev-item (org-list-get-prev-item (point-at-bol) struct prevs)))
    (unless (or prev-item org-list-use-circular-motion)
      (error "Cannot move this item further up"))
    (if (not prev-item)
	(setq struct (org-list-send-item item 'end struct))
      (setq struct (org-list-swap-items prev-item item struct)))
    (org-list-write-struct struct (org-list-parents-alist struct))
    (org-move-to-column col)))

(defun org-insert-item (&optional checkbox)
  "Insert a new item at the current level.
If cursor is before first character after bullet of the item, the
new item will be created before the current one.

If CHECKBOX is non-nil, add a checkbox next to the bullet.

Return t when things worked, nil when we are not in an item, or
item is invisible."
  (let ((itemp (org-in-item-p))
	(pos (point)))
    ;; If cursor isn't is a list or if list is invisible, return nil.
    (unless (or (not itemp)
		(save-excursion
		  (goto-char itemp)
		  (outline-invisible-p)))
      (if (save-excursion
	    (goto-char itemp)
	    (org-at-item-timer-p))
	  ;; Timer list: delegate to `org-timer-item'.
	  (progn (org-timer-item) t)
	(let* ((struct (save-excursion (goto-char itemp)
				       (org-list-struct)))
	       (prevs (org-list-prevs-alist struct))
	       ;; If we're in a description list, ask for the new term.
	       (desc (when (org-list-get-tag itemp struct)
		       (concat (read-string "Term: ") " :: ")))
	       ;; Don't insert a checkbox if checkbox rule is applied
	       ;; and it is a description item.
	       (checkp (and checkbox
			    (or (not desc)
				(not (cdr (assq 'checkbox
						org-list-automatic-rules)))))))
	  (setq struct
		(org-list-insert-item pos struct prevs checkp desc))
	  (org-list-write-struct struct (org-list-parents-alist struct))
	  (when checkp (org-update-checkbox-count-maybe))
	  (looking-at org-list-full-item-re)
	  (goto-char (match-end 0))
	  t)))))

(defun org-list-repair ()
  "Fix indentation, bullets and checkboxes is the list at point."
  (interactive)
  (unless (org-at-item-p) (error "This is not a list"))
  (let* ((struct (org-list-struct))
	 (parents (org-list-parents-alist struct)))
    (org-list-write-struct struct parents)))

(defun org-cycle-list-bullet (&optional which)
  "Cycle through the different itemize/enumerate bullets.
This cycle the entire list level through the sequence:

   `-'  ->  `+'  ->  `*'  ->  `1.'  ->  `1)'

If WHICH is a valid string, use that as the new bullet.  If WHICH
is an integer, 0 means `-', 1 means `+' etc.  If WHICH is
`previous', cycle backwards."
  (interactive "P")
  (unless (org-at-item-p) (error "Not at an item"))
  (save-excursion
    (beginning-of-line)
    (let* ((struct (org-list-struct))
           (parents (org-list-parents-alist struct))
           (prevs (org-list-prevs-alist struct))
           (list-beg (org-list-get-first-item (point) struct prevs))
           (bullet (org-list-get-bullet list-beg struct))
	   (bullet-rule-p (cdr (assq 'bullet org-list-automatic-rules)))
	   (alpha-p (org-list-use-alpha-bul-p list-beg struct prevs))
	   (case-fold-search nil)
	   (current (cond
		     ((string-match "[a-z]\\." bullet) "a.")
		     ((string-match "[a-z])" bullet) "a)")
		     ((string-match "[A-Z]\\." bullet) "A.")
		     ((string-match "[A-Z])" bullet) "A)")
		     ((string-match "\\." bullet) "1.")
		     ((string-match ")" bullet) "1)")
		     (t (org-trim bullet))))
           ;; Compute list of possible bullets, depending on context.
	   (bullet-list
	    (append '("-" "+" )
		    ;; *-bullets are not allowed at column 0.
		    (unless (and bullet-rule-p
				 (looking-at "\\S-")) '("*"))
		    ;; Description items cannot be numbered.
		    (unless (or (eq org-plain-list-ordered-item-terminator ?\))
				(and bullet-rule-p (org-at-item-description-p)))
		      '("1."))
		    (unless (or (eq org-plain-list-ordered-item-terminator ?.)
				(and bullet-rule-p (org-at-item-description-p)))
		      '("1)"))
		    (unless (or (not alpha-p)
				(eq org-plain-list-ordered-item-terminator ?\))
				(and bullet-rule-p (org-at-item-description-p)))
		      '("a." "A."))
		    (unless (or (not alpha-p)
				(eq org-plain-list-ordered-item-terminator ?.)
				(and bullet-rule-p (org-at-item-description-p)))
		      '("a)" "A)"))))
	   (len (length bullet-list))
	   (item-index (- len (length (member current bullet-list))))
	   (get-value (lambda (index) (nth (mod index len) bullet-list)))
	   (new (cond
		 ((member which bullet-list) which)
		 ((numberp which) (funcall get-value which))
		 ((eq 'previous which) (funcall get-value (1- item-index)))
		 (t (funcall get-value (1+ item-index))))))
      ;; Use a short variation of `org-list-write-struct' as there's
      ;; no need to go through all the steps.
      (let ((old-struct (copy-tree struct)))
        (org-list-set-bullet list-beg struct (org-list-bullet-string new))
        (org-list-struct-fix-bul struct prevs)
        (org-list-struct-fix-ind struct parents)
        (org-list-struct-apply-struct struct old-struct)))))

(defun org-toggle-checkbox (&optional toggle-presence)
  "Toggle the checkbox in the current line.
With prefix arg TOGGLE-PRESENCE, add or remove checkboxes.  With
double prefix, set checkbox to [-].

When there is an active region, toggle status or presence of the
first checkbox there, and make every item inside have the same
status or presence, respectively.

If the cursor is in a headline, apply this to all checkbox items
in the text below the heading, taking as reference the first item
in subtree, ignoring drawers."
  (interactive "P")
  (save-excursion
    (let* (singlep
	   block-item
	   lim-up
	   lim-down
	   (drawer-re (concat "^[ \t]*:\\("
			      (mapconcat 'regexp-quote org-drawers "\\|")
			      "\\):[ \t]*$"))
	   (keyword-re (concat "^[ \t]*\\<\\(" org-scheduled-string
			       "\\|" org-deadline-string
			       "\\|" org-closed-string
			       "\\|" org-clock-string "\\)"
			       " *[[<]\\([^]>]+\\)[]>]"))
	   (orderedp (org-entry-get nil "ORDERED"))
	   (bounds
	    ;; In a region, start at first item in region.
	    (cond
	     ((org-region-active-p)
	      (let ((limit (region-end)))
		(goto-char (region-beginning))
		(if (org-list-search-forward (org-item-beginning-re) limit t)
		    (setq lim-up (point-at-bol))
		  (error "No item in region"))
		(setq lim-down (copy-marker limit))))
	     ((org-at-heading-p)
	      ;; On an heading, start at first item after drawers and
	      ;; time-stamps (scheduled, etc.).
	      (let ((limit (save-excursion (outline-next-heading) (point))))
		(forward-line 1)
		(while (or (looking-at drawer-re) (looking-at keyword-re))
		  (if (looking-at keyword-re)
		      (forward-line 1)
		    (re-search-forward "^[ \t]*:END:" limit nil)))
		(if (org-list-search-forward (org-item-beginning-re) limit t)
		    (setq lim-up (point-at-bol))
		  (error "No item in subtree"))
		(setq lim-down (copy-marker limit))))
	     ;; Just one item: set SINGLEP flag.
	     ((org-at-item-p)
	      (setq singlep t)
	      (setq lim-up (point-at-bol)
		    lim-down (copy-marker (point-at-eol))))
	     (t (error "Not at an item or heading, and no active region"))))
	   ;; Determine the checkbox going to be applied to all items
	   ;; within bounds.
	   (ref-checkbox
	    (progn
	      (goto-char lim-up)
	      (let ((cbox (and (org-at-item-checkbox-p) (match-string 1))))
		(cond
		 ((equal toggle-presence '(16)) "[-]")
		 ((equal toggle-presence '(4))
		  (unless cbox "[ ]"))
		 ((equal "[X]" cbox) "[ ]")
		 (t "[X]"))))))
      ;; When an item is found within bounds, grab the full list at
      ;; point structure, then: (1) set check-box of all its items
      ;; within bounds to REF-CHECKBOX, (2) fix check-boxes of the
      ;; whole list, (3) move point after the list.
      (goto-char lim-up)
      (while (and (< (point) lim-down)
		  (org-list-search-forward (org-item-beginning-re)
					   lim-down 'move))
	(let* ((struct (org-list-struct))
	       (struct-copy (copy-tree struct))
	       (parents (org-list-parents-alist struct))
	       (prevs (org-list-prevs-alist struct))
	       (bottom (copy-marker (org-list-get-bottom-point struct)))
	       (items-to-toggle (org-remove-if
				 (lambda (e) (or (< e lim-up) (> e lim-down)))
				 (mapcar 'car struct))))
	  (mapc (lambda (e) (org-list-set-checkbox
			e struct
			;; If there is no box at item, leave as-is
			;; unless function was called with C-u prefix.
			(let ((cur-box (org-list-get-checkbox e struct)))
			  (if (or cur-box (equal toggle-presence '(4)))
			      ref-checkbox
			    cur-box))))
		items-to-toggle)
	  (setq block-item (org-list-struct-fix-box
			    struct parents prevs orderedp))
	  ;; Report some problems due to ORDERED status of subtree.
	  ;; If only one box was being checked, throw an error, else,
	  ;; only signal problems.
	  (cond
	   ((and singlep block-item (> lim-up block-item))
	    (error
	     "Checkbox blocked because of unchecked box at line %d"
	     (org-current-line block-item)))
	   (block-item
	    (message
	     "Checkboxes were removed due to unchecked box at line %d"
	     (org-current-line block-item))))
	  (goto-char bottom)
	  (move-marker bottom nil)
	  (org-list-struct-apply-struct struct struct-copy)))
      (move-marker lim-down nil)))
  (org-update-checkbox-count-maybe))

(defun org-reset-checkbox-state-subtree ()
  "Reset all checkboxes in an entry subtree."
  (interactive "*")
  (if (org-before-first-heading-p)
      (error "Not inside a tree")
    (save-restriction
      (save-excursion
	(org-narrow-to-subtree)
	(org-show-subtree)
	(goto-char (point-min))
	(let ((end (point-max)))
	  (while (< (point) end)
	    (when (org-at-item-checkbox-p)
	      (replace-match "[ ]" t t nil 1))
	    (beginning-of-line 2)))
	(org-update-checkbox-count-maybe 'all)))))

(defun org-update-checkbox-count (&optional all)
  "Update the checkbox statistics in the current section.
This will find all statistic cookies like [57%] and [6/12] and
update them with the current numbers.

With optional prefix argument ALL, do this for the whole buffer."
  (interactive "P")
  (save-excursion
    (let ((cookie-re "\\(\\(\\[[0-9]*%\\]\\)\\|\\(\\[[0-9]*/[0-9]*\\]\\)\\)")
	  (box-re "^[ \t]*\\([-+*]\\|\\([0-9]+\\|[A-Za-z]\\)[.)]\\)[ \t]+\\(?:\\[@\\(?:start:\\)?\\([0-9]+\\|[A-Za-z]\\)\\][ \t]*\\)?\\(\\[[- X]\\]\\)")
	  (recursivep
	   (or (not org-hierarchical-checkbox-statistics)
	       (string-match "\\<recursive\\>"
			     (or (org-entry-get nil "COOKIE_DATA") ""))))
	  (bounds (if all
		      (cons (point-min) (point-max))
		    (cons (or (ignore-errors (org-back-to-heading t) (point))
			      (point-min))
			  (save-excursion (outline-next-heading) (point)))))
	  (count-boxes
	   (function
	    ;; Return number of checked boxes and boxes of all types
	    ;; in all structures in STRUCTS.  If RECURSIVEP is
	    ;; non-nil, also count boxes in sub-lists.  If ITEM is
	    ;; nil, count across the whole structure, else count only
	    ;; across subtree whose ancestor is ITEM.
	    (lambda (item structs recursivep)
	      (let ((c-on 0) (c-all 0))
		(mapc
		 (lambda (s)
		   (let* ((pre (org-list-prevs-alist s))
			  (par (org-list-parents-alist s))
			  (items
			   (cond
			    ((and recursivep item) (org-list-get-subtree item s))
			    (recursivep (mapcar 'car s))
			    (item (org-list-get-children item s par))
			    (t (org-list-get-all-items
				(org-list-get-top-point s) s pre))))
			  (cookies (delq nil (mapcar
					      (lambda (e)
						(org-list-get-checkbox e s))
					      items))))
		     (setq c-all (+ (length cookies) c-all)
			   c-on (+ (org-count "[X]" cookies) c-on))))
		 structs)
		(cons c-on c-all)))))
	  (backup-end 1)
	  cookies-list structs-bak box-num)
      (goto-char (car bounds))
      ;; 1. Build an alist for each cookie found within BOUNDS.  The
      ;;    key will be position at beginning of cookie and values
      ;;    ending position, format of cookie, and a cell whose car is
      ;;    number of checked boxes to report, and cdr total number of
      ;;    boxes.
      (while (re-search-forward cookie-re (cdr bounds) t)
	(catch 'skip
	  (save-excursion
	    (push
	     (list
	      (match-beginning 1)	; cookie start
	      (match-end 1)		; cookie end
	      (match-string 2)		; percent?
	      (cond			; boxes count
	       ;; Cookie is at an heading, but specifically for todo,
	       ;; not for checkboxes: skip it.
	       ((and (org-at-heading-p)
		     (string-match "\\<todo\\>"
				   (downcase
				    (or (org-entry-get nil "COOKIE_DATA") ""))))
		(throw 'skip nil))
	       ;; Cookie is at an heading, but all lists before next
	       ;; heading already have been read.  Use data collected
	       ;; in STRUCTS-BAK.  This should only happen when
	       ;; heading has more than one cookie on it.
	       ((and (org-at-heading-p)
		     (<= (save-excursion (outline-next-heading) (point))
			 backup-end))
		(funcall count-boxes nil structs-bak recursivep))
	       ;; Cookie is at a fresh heading.  Grab structure of
	       ;; every list containing a checkbox between point and
	       ;; next headline, and save them in STRUCTS-BAK.
	       ((org-at-heading-p)
		(setq backup-end (save-excursion
				   (outline-next-heading) (point))
		      structs-bak nil)
		(while (org-list-search-forward box-re backup-end 'move)
		  (let* ((struct (org-list-struct))
			 (bottom (org-list-get-bottom-point struct)))
		    (push struct structs-bak)
		    (goto-char bottom)))
		(funcall count-boxes nil structs-bak recursivep))
	       ;; Cookie is at an item, and we already have list
	       ;; structure stored in STRUCTS-BAK.
	       ((and (org-at-item-p)
		     (< (point-at-bol) backup-end)
		     ;; Only lists in no special context are stored.
		     (not (nth 2 (org-list-context))))
		(funcall count-boxes (point-at-bol) structs-bak recursivep))
	       ;; Cookie is at an item, but we need to compute list
	       ;; structure.
	       ((org-at-item-p)
		(let ((struct (org-list-struct)))
		  (setq backup-end (org-list-get-bottom-point struct)
			structs-bak (list struct)))
		(funcall count-boxes (point-at-bol) structs-bak recursivep))
	       ;; Else, cookie found is at a wrong place.  Skip it.
	       (t (throw 'skip nil))))
	     cookies-list))))
      ;; 2. Apply alist to buffer, in reverse order so positions stay
      ;;    unchanged after cookie modifications.
      (mapc (lambda (cookie)
	      (let* ((beg (car cookie))
		     (end (nth 1 cookie))
		     (percentp (nth 2 cookie))
		     (checked (car (nth 3 cookie)))
		     (total (cdr (nth 3 cookie)))
		     (new (if percentp
			      (format "[%d%%]" (/ (* 100 checked)
						  (max 1 total)))
			    (format "[%d/%d]" checked total))))
		(goto-char beg)
		(insert new)
		(delete-region (point) (+ (point) (- end beg)))
		(when org-auto-align-tags (org-fix-tags-on-the-fly))))
	    cookies-list))))

(defun org-get-checkbox-statistics-face ()
  "Select the face for checkbox statistics.
The face will be `org-done' when all relevant boxes are checked.
Otherwise it will be `org-todo'."
  (if (match-end 1)
      (if (equal (match-string 1) "100%")
	  'org-checkbox-statistics-done
	'org-checkbox-statistics-todo)
    (if (and (> (match-end 2) (match-beginning 2))
	     (equal (match-string 2) (match-string 3)))
	'org-checkbox-statistics-done
      'org-checkbox-statistics-todo)))

(defun org-update-checkbox-count-maybe (&optional all)
  "Update checkbox statistics unless turned off by user.
With an optional argument ALL, update them in the whole buffer."
  (when (cdr (assq 'checkbox org-list-automatic-rules))
    (org-update-checkbox-count all))
  (run-hooks 'org-checkbox-statistics-hook))

(defvar org-last-indent-begin-marker (make-marker))
(defvar org-last-indent-end-marker (make-marker))
(defun org-list-indent-item-generic (arg no-subtree struct)
  "Indent a local list item including its children.
When number ARG is a negative, item will be outdented, otherwise
it will be indented.

If a region is active, all items inside will be moved.

If NO-SUBTREE is non-nil, only indent the item itself, not its
children.

STRUCT is the list structure.

Return t if successful."
  (save-excursion
    (let* ((regionp (org-region-active-p))
	   (rbeg (and regionp (region-beginning)))
	   (rend (and regionp (region-end)))
	   (top (org-list-get-top-point struct))
	   (parents (org-list-parents-alist struct))
	   (prevs (org-list-prevs-alist struct))
	   ;; Are we going to move the whole list?
	   (specialp
	    (and (not regionp)
		 (= top (point-at-bol))
		 (cdr (assq 'indent org-list-automatic-rules))
		 (if no-subtree
		     (error
		      "First item of list cannot move without its subtree")
		   t))))
      ;; Determine begin and end points of zone to indent.  If moving
      ;; more than one item, save them for subsequent moves.
      (unless (and (memq last-command '(org-shiftmetaright org-shiftmetaleft))
		   (memq this-command '(org-shiftmetaright org-shiftmetaleft)))
	(if regionp
	    (progn
	      (set-marker org-last-indent-begin-marker rbeg)
	      (set-marker org-last-indent-end-marker rend))
	  (set-marker org-last-indent-begin-marker (point-at-bol))
	  (set-marker org-last-indent-end-marker
		      (cond
		       (specialp (org-list-get-bottom-point struct))
		       (no-subtree (1+ (point-at-bol)))
		       (t (org-list-get-item-end (point-at-bol) struct))))))
      (let* ((beg (marker-position org-last-indent-begin-marker))
	     (end (marker-position org-last-indent-end-marker)))
	(cond
	 ;; Special case: moving top-item with indent rule.
	 (specialp
	  (let* ((level-skip (org-level-increment))
		 (offset (if (< arg 0) (- level-skip) level-skip))
		 (top-ind (org-list-get-ind beg struct))
		 (old-struct (copy-tree struct)))
	    (if (< (+ top-ind offset) 0)
		(error "Cannot outdent beyond margin")
	      ;; Change bullet if necessary.
	      (when (and (= (+ top-ind offset) 0)
			 (string-match "*"
				       (org-list-get-bullet beg struct)))
		(org-list-set-bullet beg struct
				     (org-list-bullet-string "-")))
	      ;; Shift every item by OFFSET and fix bullets.  Then
	      ;; apply changes to buffer.
	      (mapc (lambda (e)
		      (let ((ind (org-list-get-ind (car e) struct)))
			(org-list-set-ind (car e) struct (+ ind offset))))
		    struct)
	      (org-list-struct-fix-bul struct prevs)
	      (org-list-struct-apply-struct struct old-struct))))
	 ;; Forbidden move:
	 ((and (< arg 0)
	       ;; If only one item is moved, it mustn't have a child.
	       (or (and no-subtree
			(not regionp)
			(org-list-has-child-p beg struct))
		   ;; If a subtree or region is moved, the last item
		   ;; of the subtree mustn't have a child.
		   (let ((last-item (caar
				     (reverse
				      (org-remove-if
				       (lambda (e) (>= (car e) end))
				       struct)))))
		     (org-list-has-child-p last-item struct))))
	  (error "Cannot outdent an item without its children"))
	 ;; Normal shifting
	 (t
	  (let* ((new-parents
		  (if (< arg 0)
		      (org-list-struct-outdent beg end struct parents)
		    (org-list-struct-indent beg end struct parents prevs))))
	    (org-list-write-struct struct new-parents))
	  (org-update-checkbox-count-maybe))))))
  t)

(defun org-outdent-item ()
  "Outdent a local list item, but not its children.
If a region is active, all items inside will be moved."
  (interactive)
  (let ((regionp (org-region-active-p)))
    (cond
     ((or (org-at-item-p)
	  (and regionp
	       (save-excursion (goto-char (region-beginning))
			       (org-at-item-p))))
      (let ((struct (if (not regionp) (org-list-struct)
		      (save-excursion (goto-char (region-beginning))
				      (org-list-struct)))))
	(org-list-indent-item-generic -1 t struct)))
     (regionp (error "Region not starting at an item"))
     (t (error "Not at an item")))))

(defun org-indent-item ()
  "Indent a local list item, but not its children.
If a region is active, all items inside will be moved."
  (interactive)
  (let ((regionp (org-region-active-p)))
    (cond
     ((or (org-at-item-p)
	  (and regionp
	       (save-excursion (goto-char (region-beginning))
			       (org-at-item-p))))
      (let ((struct (if (not regionp) (org-list-struct)
		      (save-excursion (goto-char (region-beginning))
				      (org-list-struct)))))
	(org-list-indent-item-generic 1 t struct)))
     (regionp (error "Region not starting at an item"))
     (t (error "Not at an item")))))

(defun org-outdent-item-tree ()
  "Outdent a local list item including its children.
If a region is active, all items inside will be moved."
  (interactive)
  (let ((regionp (org-region-active-p)))
    (cond
     ((or (org-at-item-p)
	  (and regionp
	       (save-excursion (goto-char (region-beginning))
			       (org-at-item-p))))
      (let ((struct (if (not regionp) (org-list-struct)
		      (save-excursion (goto-char (region-beginning))
				      (org-list-struct)))))
	(org-list-indent-item-generic -1 nil struct)))
     (regionp (error "Region not starting at an item"))
     (t (error "Not at an item")))))

(defun org-indent-item-tree ()
  "Indent a local list item including its children.
If a region is active, all items inside will be moved."
  (interactive)
  (let ((regionp (org-region-active-p)))
    (cond
     ((or (org-at-item-p)
	  (and regionp
	       (save-excursion (goto-char (region-beginning))
			       (org-at-item-p))))
      (let ((struct (if (not regionp) (org-list-struct)
		      (save-excursion (goto-char (region-beginning))
				      (org-list-struct)))))
	(org-list-indent-item-generic 1 nil struct)))
     (regionp (error "Region not starting at an item"))
     (t (error "Not at an item")))))

(defvar org-tab-ind-state)
(defun org-cycle-item-indentation ()
  "Cycle levels of indentation of an empty item.
The first run indents the item, if applicable.  Subsequent runs
outdent it at meaningful levels in the list.  When done, item is
put back at its original position with its original bullet.

Return t at each successful move."
  (when (org-at-item-p)
    (let* ((org-adapt-indentation nil)
	   (struct (org-list-struct))
	   (ind (org-list-get-ind (point-at-bol) struct))
	   (bullet (org-trim (buffer-substring (point-at-bol) (point-at-eol)))))
      ;; Accept empty items or if cycle has already started.
      (when (or (eq last-command 'org-cycle-item-indentation)
		(and (save-excursion
		       (beginning-of-line)
		       (looking-at org-list-full-item-re))
		     (>= (match-end 0) (save-excursion
					 (goto-char (org-list-get-item-end
						     (point-at-bol) struct))
					 (skip-chars-backward " \r\t\n")
					 (point)))))
	(setq this-command 'org-cycle-item-indentation)
	;; When in the middle of the cycle, try to outdent first.  If
	;; it fails, and point is still at initial position, indent.
	;; Else, re-create it at its original position.
	(if (eq last-command 'org-cycle-item-indentation)
	    (cond
	     ((ignore-errors (org-list-indent-item-generic -1 t struct)))
	     ((and (= ind (car org-tab-ind-state))
		   (ignore-errors (org-list-indent-item-generic 1 t struct))))
	     (t (delete-region (point-at-bol) (point-at-eol))
		(org-indent-to-column (car org-tab-ind-state))
		(insert (cdr org-tab-ind-state) " ")
		;; Break cycle
		(setq this-command 'identity)))
	  ;; If a cycle is starting, remember indentation and bullet,
	  ;; then try to indent.  If it fails, try to outdent.
	  (setq org-tab-ind-state (cons ind bullet))
	  (cond
	   ((ignore-errors (org-list-indent-item-generic 1 t struct)))
	   ((ignore-errors (org-list-indent-item-generic -1 t struct)))
	   (t (error "Cannot move item"))))
	t))))

(defun org-sort-list (&optional with-case sorting-type getkey-func compare-func)
  "Sort list items.
The cursor may be at any item of the list that should be sorted.
Sublists are not sorted.  Checkboxes, if any, are ignored.

Sorting can be alphabetically, numerically, by date/time as given
by a time stamp, by a property or by priority.

Comparing entries ignores case by default.  However, with an
optional argument WITH-CASE, the sorting considers case as well.

The command prompts for the sorting type unless it has been given
to the function through the SORTING-TYPE argument, which needs to
be a character, \(?n ?N ?a ?A ?t ?T ?f ?F).  Here is the precise
meaning of each character:

n   Numerically, by converting the beginning of the item to a number.
a   Alphabetically.  Only the first line of item is checked.
t   By date/time, either the first active time stamp in the entry, if
    any, or by the first inactive one.  In a timer list, sort the timers.

Capital letters will reverse the sort order.

If the SORTING-TYPE is ?f or ?F, then GETKEY-FUNC specifies
a function to be called with point at the beginning of the
record.  It must return either a string or a number that should
serve as the sorting key for that record.  It will then use
COMPARE-FUNC to compare entries."
  (interactive "P")
  (let* ((case-func (if with-case 'identity 'downcase))
         (struct (org-list-struct))
         (prevs (org-list-prevs-alist struct))
	 (start (org-list-get-list-begin (point-at-bol) struct prevs))
	 (end (org-list-get-list-end (point-at-bol) struct prevs))
	 (sorting-type
	  (progn
	    (message
	     "Sort plain list: [a]lpha  [n]umeric  [t]ime  [f]unc   A/N/T/F means reversed:")
	    (read-char-exclusive)))
	 (getkey-func (and (= (downcase sorting-type) ?f)
			   (intern (org-icompleting-read "Sort using function: "
							 obarray 'fboundp t nil nil)))))
    (message "Sorting items...")
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (let* ((dcst (downcase sorting-type))
	     (case-fold-search nil)
	     (now (current-time))
	     (sort-func (cond
			 ((= dcst ?a) 'string<)
			 ((= dcst ?f) compare-func)
			 ((= dcst ?t) '<)
			 (t nil)))
	     (next-record (lambda ()
			     (skip-chars-forward " \r\t\n")
			     (beginning-of-line)))
	     (end-record (lambda ()
			   (goto-char (org-list-get-item-end-before-blank
				       (point) struct))))
	     (value-to-sort
	      (lambda ()
		(when (looking-at "[ \t]*[-+*0-9.)]+\\([ \t]+\\[[- X]\\]\\)?[ \t]+")
		  (cond
		   ((= dcst ?n)
		    (string-to-number (buffer-substring (match-end 0)
							(point-at-eol))))
		   ((= dcst ?a)
		    (funcall case-func
			     (buffer-substring (match-end 0) (point-at-eol))))
		   ((= dcst ?t)
		    (cond
		     ;; If it is a timer list, convert timer to seconds
		     ((org-at-item-timer-p)
		      (org-timer-hms-to-secs (match-string 1)))
		     ((or (re-search-forward org-ts-regexp (point-at-eol) t)
			  (re-search-forward org-ts-regexp-both
					     (point-at-eol) t))
		      (org-time-string-to-seconds (match-string 0)))
		     (t (org-float-time now))))
		   ((= dcst ?f)
		    (if getkey-func
			(let ((value (funcall getkey-func)))
			  (if (stringp value)
			      (funcall case-func value)
			    value))
		      (error "Invalid key function `%s'" getkey-func)))
		   (t (error "Invalid sorting type `%c'" sorting-type)))))))
	(sort-subr (/= dcst sorting-type)
		   next-record
		   end-record
		   value-to-sort
		   nil
		   sort-func)
	;; Read and fix list again, as `sort-subr' probably destroyed
	;; its structure.
	(org-list-repair)
	(run-hooks 'org-after-sorting-entries-or-items-hook)
	(message "Sorting items...done")))))



;;; Send and receive lists

(defun org-list-parse-list (&optional delete)
  "Parse the list at point and maybe DELETE it.

Return a list whose car is a symbol of list type, among
`ordered', `unordered' and `descriptive'.  Then, each item is
a list whose car is counter, and cdr are strings and other
sub-lists.  Inside strings, check-boxes are replaced by
\"[CBON]\", \"[CBOFF]\" and \"[CBTRANS]\".

For example, the following list:

1. first item
   + sub-item one
   + [X] sub-item two
   more text in first item
2. [@3] last item

will be parsed as:

\(ordered
  \(nil \"first item\"
  \(unordered
    \(nil \"sub-item one\"\)
    \(nil \"[CBON] sub-item two\"\)\)
  \"more text in first item\"\)
  \(3 \"last item\"\)\)

Point is left at list end."
  (let* ((struct (org-list-struct))
	 (prevs (org-list-prevs-alist struct))
	 (parents (org-list-parents-alist struct))
	 (top (org-list-get-top-point struct))
	 (bottom (org-list-get-bottom-point struct))
	 out
	 parse-item			; for byte-compiler
	 (get-text
	  (function
	   ;; Return text between BEG and END, trimmed, with
	   ;; checkboxes replaced.
	   (lambda (beg end)
	     (let ((text (org-trim (buffer-substring beg end))))
	       (if (string-match "\\`\\[\\([-X ]\\)\\]" text)
		   (replace-match
		    (let ((box (match-string 1 text)))
		      (cond
		       ((equal box " ") "CBOFF")
		       ((equal box "-") "CBTRANS")
		       (t "CBON")))
		    t nil text 1)
		 text)))))
	 (parse-sublist
	  (function
	   ;; Return a list whose car is list type and cdr a list of
	   ;; items' body.
	   (lambda (e)
	     (cons (org-list-get-list-type (car e) struct prevs)
		   (mapcar parse-item e)))))
	 (parse-item
	  (function
	   ;; Return a list containing counter of item, if any, text
	   ;; and any sublist inside it.
	   (lambda (e)
	     (let ((start (save-excursion
			    (goto-char e)
			    (looking-at "[ \t]*\\S-+\\([ \t]+\\[@\\(start:\\)?\\([0-9]+\\|[a-zA-Z]\\)\\]\\)?[ \t]*")
			    (match-end 0)))
		   ;; Get counter number. For alphabetic counter, get
		   ;; its position in the alphabet.
		   (counter (let ((c (org-list-get-counter e struct)))
			      (cond
			       ((not c) nil)
			       ((string-match "[A-Za-z]" c)
				(- (string-to-char (upcase (match-string 0 c)))
				   64))
			       ((string-match "[0-9]+" c)
				(string-to-number (match-string 0 c))))))
		   (childp (org-list-has-child-p e struct))
		   (end (org-list-get-item-end e struct)))
	       ;; If item has a child, store text between bullet and
	       ;; next child, then recursively parse all sublists.  At
	       ;; the end of each sublist, check for the presence of
	       ;; text belonging to the original item.
	       (if childp
		   (let* ((children (org-list-get-children e struct parents))
			  (body (list (funcall get-text start childp))))
		     (while children
		       (let* ((first (car children))
			      (sub (org-list-get-all-items first struct prevs))
			      (last-c (car (last sub)))
			      (last-end (org-list-get-item-end last-c struct)))
			 (push (funcall parse-sublist sub) body)
			 ;; Remove children from the list just parsed.
			 (setq children (cdr (member last-c children)))
			 ;; There is a chunk of text belonging to the
			 ;; item if last child doesn't end where next
			 ;; child starts or where item ends.
			 (unless (= (or (car children) end) last-end)
			   (push (funcall get-text
					  last-end (or (car children) end))
				 body))))
		     (cons counter (nreverse body)))
		 (list counter (funcall get-text start end))))))))
    ;; Store output, take care of cursor position and deletion of
    ;; list, then return output.
    (setq out (funcall parse-sublist (org-list-get-all-items top struct prevs)))
    (goto-char top)
    (when delete
      (delete-region top bottom)
      (when (and (not (looking-at "[ \t]*$")) (looking-at org-list-end-re))
	(replace-match "")))
    out))

(defun org-list-make-subtree ()
  "Convert the plain list at point into a subtree."
  (interactive)
  (if (not (ignore-errors (goto-char (org-in-item-p))))
      (error "Not in a list")
    (let ((list (save-excursion (org-list-parse-list t))))
      (insert (org-list-to-subtree list)))))

(defun org-list-insert-radio-list ()
  "Insert a radio list template appropriate for this major mode."
  (interactive)
  (let* ((e (assq major-mode org-list-radio-list-templates))
	 (txt (nth 1 e))
	 name pos)
    (unless e (error "No radio list setup defined for %s" major-mode))
    (setq name (read-string "List name: "))
    (while (string-match "%n" txt)
      (setq txt (replace-match name t t txt)))
    (or (bolp) (insert "\n"))
    (setq pos (point))
    (insert txt)
    (goto-char pos)))

(defun org-list-send-list (&optional maybe)
  "Send a transformed version of this list to the receiver position.
With argument MAYBE, fail quietly if no transformation is defined
for this list."
  (interactive)
  (catch 'exit
    (unless (org-at-item-p) (error "Not at a list item"))
    (save-excursion
      (re-search-backward "#\\+ORGLST" nil t)
      (unless (looking-at "[ \t]*#\\+ORGLST[: \t][ \t]*SEND[ \t]+\\([^ \t\r\n]+\\)[ \t]+\\([^ \t\r\n]+\\)\\([ \t]+.*\\)?")
	(if maybe
	    (throw 'exit nil)
	  (error "Don't know how to transform this list"))))
    (let* ((name (match-string 1))
	   (transform (intern (match-string 2)))
	   (bottom-point
	    (save-excursion
	      (re-search-forward
	       "\\(\\\\end{comment}\\|@end ignore\\|-->\\)" nil t)
	      (match-beginning 0)))
	   (top-point
	    (progn
	      (re-search-backward "#\\+ORGLST" nil t)
	      (re-search-forward (org-item-beginning-re) bottom-point t)
	      (match-beginning 0)))
	   (list (save-restriction
		   (narrow-to-region top-point bottom-point)
		   (org-list-parse-list)))
	   beg txt)
      (unless (fboundp transform)
	(error "No such transformation function %s" transform))
      (let ((txt (funcall transform list)))
	;; Find the insertion place
	(save-excursion
	  (goto-char (point-min))
	  (unless (re-search-forward
		   (concat "BEGIN RECEIVE ORGLST +"
			   name
			   "\\([ \t]\\|$\\)") nil t)
	    (error "Don't know where to insert translated list"))
	  (goto-char (match-beginning 0))
	  (beginning-of-line 2)
	  (setq beg (point))
	  (unless (re-search-forward (concat "END RECEIVE ORGLST +" name) nil t)
	    (error "Cannot find end of insertion region"))
	  (delete-region beg (point-at-bol))
	  (goto-char beg)
	  (insert txt "\n")))
      (message "List converted and installed at receiver location"))))

(defsubst org-list-item-trim-br (item)
  "Trim line breaks in a list ITEM."
  (setq item (replace-regexp-in-string "\n +" " " item)))

(defun org-list-to-generic (list params)
  "Convert a LIST parsed through `org-list-parse-list' to other formats.
Valid parameters PARAMS are:

:ustart	    String to start an unordered list
:uend	    String to end an unordered list

:ostart	    String to start an ordered list
:oend	    String to end an ordered list

:dstart	    String to start a descriptive list
:dend	    String to end a descriptive list
:dtstart    String to start a descriptive term
:dtend	    String to end a descriptive term
:ddstart    String to start a description
:ddend	    String to end a description

:splice	    When set to t, return only list body lines, don't wrap
	    them into :[u/o]start and :[u/o]end.  Default is nil.

:istart	    String to start a list item.
:icount     String to start an item with a counter.
:iend	    String to end a list item
:isep	    String to separate items
:lsep	    String to separate sublists
:csep	    String to separate text from a sub-list

:cboff      String to insert for an unchecked check-box
:cbon       String to insert for a checked check-box
:cbtrans    String to insert for a check-box in transitional state

:nobr       Non-nil means remove line breaks in lists items.

Alternatively, each parameter can also be a form returning
a string.  These sexp can use keywords `counter' and `depth',
representing respectively counter associated to the current
item, and depth of the current sub-list, starting at 0.
Obviously, `counter' is only available for parameters applying to
items."
  (interactive)
  (let* ((p params)
	 (splicep (plist-get p :splice))
	 (ostart (plist-get p :ostart))
	 (oend (plist-get p :oend))
	 (ustart (plist-get p :ustart))
	 (uend (plist-get p :uend))
	 (dstart (plist-get p :dstart))
	 (dend (plist-get p :dend))
	 (dtstart (plist-get p :dtstart))
	 (dtend (plist-get p :dtend))
	 (ddstart (plist-get p :ddstart))
	 (ddend (plist-get p :ddend))
	 (istart (plist-get p :istart))
	 (icount (plist-get p :icount))
	 (iend (plist-get p :iend))
	 (isep (plist-get p :isep))
	 (lsep (plist-get p :lsep))
	 (csep (plist-get p :csep))
	 (cbon (plist-get p :cbon))
	 (cboff (plist-get p :cboff))
	 (cbtrans (plist-get p :cbtrans))
	 (nobr (plist-get p :nobr))
	 export-sublist			; for byte-compiler
	 (export-item
	  (function
	   ;; Export an item ITEM of type TYPE, at DEPTH.  First
	   ;; string in item is treated in a special way as it can
	   ;; bring extra information that needs to be processed.
	   (lambda (item type depth)
	     (let* ((counter (pop item))
		    (fmt (concat
			  (cond
			   ((eq type 'descriptive)
			    ;; Stick DTSTART to ISTART by
			    ;; left-trimming the latter.
			    (concat (let ((s (eval istart)))
				      (or (and (string-match "[ \t\n\r]+\\'" s)
					       (replace-match "" t t s))
					  istart))
				    "%s" (eval ddend)))
			   ((and counter (eq type 'ordered))
			    (concat (eval icount) "%s"))
			   (t (concat (eval istart) "%s")))
				 (eval iend)))
		    (first (car item)))
	       ;; Replace checkbox if any is found.
	       (cond
		((string-match "\\[CBON\\]" first)
		 (setq first (replace-match cbon t t first)))
		((string-match "\\[CBOFF\\]" first)
		 (setq first (replace-match cboff t t first)))
		((string-match "\\[CBTRANS\\]" first)
		 (setq first (replace-match cbtrans t t first))))
	       ;; Replace line breaks if required
	       (when nobr (setq first (org-list-item-trim-br first)))
	       ;; Insert descriptive term if TYPE is `descriptive'.
	       (when (eq type 'descriptive)
		 (let* ((complete (string-match "^\\(.*\\)[ \t]+::" first))
			(term (if complete
				  (save-match-data
				    (org-trim (match-string 1 first)))
				"???"))
			(desc (if complete
				  (org-trim (substring first (match-end 0)))
				first)))
		   (setq first (concat (eval dtstart) term (eval dtend)
				       (eval ddstart) desc))))
	       (setcar item first)
	       (format fmt
		       (mapconcat (lambda (e)
				    (if (stringp e) e
				      (funcall export-sublist e (1+ depth))))
				  item (or (eval csep) "")))))))
	 (export-sublist
	  (function
	   ;; Export sublist SUB at DEPTH.
	   (lambda (sub depth)
	     (let* ((type (car sub))
		    (items (cdr sub))
		    (fmt (concat (cond
				  (splicep "%s")
				  ((eq type 'ordered)
				   (concat (eval ostart) "%s" (eval oend)))
				  ((eq type 'descriptive)
				   (concat (eval dstart) "%s" (eval dend)))
				  (t (concat (eval ustart) "%s" (eval uend))))
				 (eval lsep))))
	       (format fmt (mapconcat (lambda (e)
					(funcall export-item e type depth))
				      items (or (eval isep) ""))))))))
    (concat (funcall export-sublist list 0) "\n")))

(defun org-list-to-latex (list &optional params)
  "Convert LIST into a LaTeX list.
LIST is as returned by `org-list-parse-list'.  PARAMS is a property list
with overruling parameters for `org-list-to-generic'."
  (org-list-to-generic
   list
   (org-combine-plists
    '(:splice nil :ostart "\\begin{enumerate}\n" :oend "\\end{enumerate}"
	       :ustart "\\begin{itemize}\n" :uend "\\end{itemize}"
	       :dstart "\\begin{description}\n" :dend "\\end{description}"
	       :dtstart "[" :dtend "] "
	       :istart "\\item " :iend "\n"
	       :icount (let ((enum (nth depth '("i" "ii" "iii" "iv"))))
			 (if enum
			     ;; LaTeX increments counter just before
			     ;; using it, so set it to the desired
			     ;; value, minus one.
			     (format "\\setcounter{enum%s}{%s}\n\\item "
				     enum (1- counter))
			   "\\item "))
	       :csep "\n"
	       :cbon "\\texttt{[X]}" :cboff "\\texttt{[ ]}"
	       :cbtrans "\\texttt{[-]}")
    params)))

(defun org-list-to-html (list &optional params)
  "Convert LIST into a HTML list.
LIST is as returned by `org-list-parse-list'.  PARAMS is a property list
with overruling parameters for `org-list-to-generic'."
  (org-list-to-generic
   list
   (org-combine-plists
    '(:splice nil :ostart "<ol>\n" :oend "\n</ol>"
	       :ustart "<ul>\n" :uend "\n</ul>"
	       :dstart "<dl>\n" :dend "\n</dl>"
	       :dtstart "<dt>" :dtend "</dt>\n"
	       :ddstart "<dd>" :ddend "</dd>"
	       :istart "<li>" :iend "</li>"
	       :icount (format "<li value=\"%s\">" counter)
	       :isep "\n" :lsep "\n" :csep "\n"
	       :cbon "<code>[X]</code>" :cboff "<code>[ ]</code>"
	       :cbtrans "<code>[-]</code>")
    params)))

(defun org-list-to-texinfo (list &optional params)
  "Convert LIST into a Texinfo list.
LIST is as returned by `org-list-parse-list'.  PARAMS is a property list
with overruling parameters for `org-list-to-generic'."
  (org-list-to-generic
   list
   (org-combine-plists
    '(:splice nil :ostart "@itemize @minus\n" :oend "@end itemize"
	       :ustart "@enumerate\n" :uend "@end enumerate"
	       :dstart "@table @asis\n" :dend "@end table"
	       :dtstart " " :dtend "\n"
	       :istart "@item\n" :iend "\n"
	       :icount "@item\n"
	       :csep "\n"
	       :cbon "@code{[X]}" :cboff "@code{[ ]}"
	       :cbtrans "@code{[-]}")
    params)))

(defun org-list-to-subtree (list &optional params)
  "Convert LIST into an Org subtree.
LIST is as returned by `org-list-parse-list'.  PARAMS is a property list
with overruling parameters for `org-list-to-generic'."
  (let* ((rule (cdr (assq 'heading org-blank-before-new-entry)))
	 (level (org-reduced-level (or (org-current-level) 0)))
	 (blankp (or (eq rule t)
		     (and (eq rule 'auto)
			  (save-excursion
			    (outline-previous-heading)
			    (org-previous-line-empty-p)))))
	 (get-stars
	  (function
	   ;; Return the string for the heading, depending on depth D
	   ;; of current sub-list.
	   (lambda (d)
	     (let ((oddeven-level (+ level d 1)))
	       (concat (make-string (if org-odd-levels-only
					(1- (* 2 oddeven-level))
				      oddeven-level)
				    ?*)
		       " "))))))
    (org-list-to-generic
     list
     (org-combine-plists
      '(:splice t
		:dtstart " " :dtend " "
		:istart (funcall get-stars depth)
		:icount (funcall get-stars depth)
		:isep (if blankp "\n\n" "\n")
		:csep (if blankp "\n\n" "\n")
		:cbon "DONE" :cboff "TODO" :cbtrans "TODO")
      params))))

(provide 'org-list)

;;; org-list.el ends here
