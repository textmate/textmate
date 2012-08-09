;;; fill.el --- fill commands for Emacs		-*- coding: utf-8 -*-

;; Copyright (C) 1985-1986, 1992, 1994-1997, 1999, 2001-2012
;;   Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: wp
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

;; All the commands for filling text.  These are documented in the Emacs
;; manual.

;;; Code:

(defgroup fill nil
  "Indenting and filling text."
  :link '(custom-manual "(emacs)Filling")
  :group 'editing)

(defcustom fill-individual-varying-indent nil
  "Controls criterion for a new paragraph in `fill-individual-paragraphs'.
Non-nil means changing indent doesn't end a paragraph.
That mode can handle paragraphs with extra indentation on the first line,
but it requires separator lines between paragraphs.
A value of nil means that any change in indentation starts a new paragraph."
  :type 'boolean
  :group 'fill)

(defcustom colon-double-space nil
  "Non-nil means put two spaces after a colon when filling."
  :type 'boolean
  :group 'fill)
(put 'colon-double-space 'safe-local-variable 'booleanp)

(defvar fill-paragraph-function nil
  "Mode-specific function to fill a paragraph, or nil if there is none.
If the function returns nil, then `fill-paragraph' does its normal work.
A value of t means explicitly \"do nothing special\".
Note: This only affects `fill-paragraph' and not `fill-region'
nor `auto-fill-mode', so it is often better to use some other hook,
such as `fill-forward-paragraph-function'.")

(defvar fill-paragraph-handle-comment t
  "Non-nil means paragraph filling will try to pay attention to comments.")

(defcustom enable-kinsoku t
  "Non-nil means enable \"kinsoku\" processing on filling paragraphs.
Kinsoku processing is designed to prevent certain characters from being
placed at the beginning or end of a line by filling.
See the documentation of `kinsoku' for more information."
  :type 'boolean
  :group 'fill)

(defun set-fill-prefix ()
  "Set the fill prefix to the current line up to point.
Filling expects lines to start with the fill prefix and
reinserts the fill prefix in each resulting line."
  (interactive)
  (let ((left-margin-pos (save-excursion (move-to-left-margin) (point))))
    (if (> (point) left-margin-pos)
	(progn
	  (setq fill-prefix (buffer-substring left-margin-pos (point)))
	  (if (equal fill-prefix "")
	      (setq fill-prefix nil)))
      (setq fill-prefix nil)))
  (if fill-prefix
      (message "fill-prefix: \"%s\"" fill-prefix)
    (message "fill-prefix canceled")))

(defcustom adaptive-fill-mode t
  "Non-nil means determine a paragraph's fill prefix from its text."
  :type 'boolean
  :group 'fill)

(defcustom adaptive-fill-regexp
  ;; Added `!' for doxygen comments starting with `//!' or `/*!'.
  ;; Added `%' for TeX comments.
  ;; RMS: deleted the code to match `1.' and `(1)'.
  ;; Update mail-mode's paragraph-separate if you change this.
  (purecopy "[ \t]*\\([-–!|#%;>*·•‣⁃◦]+[ \t]*\\)*")
  "Regexp to match text at start of line that constitutes indentation.
If Adaptive Fill mode is enabled, a prefix matching this pattern
on the first and second lines of a paragraph is used as the
standard indentation for the whole paragraph.

If the paragraph has just one line, the indentation is taken from that
line, but in that case `adaptive-fill-first-line-regexp' also plays
a role."
  :type 'regexp
  :group 'fill)

(defcustom adaptive-fill-first-line-regexp (purecopy "\\`[ \t]*\\'")
  "Regexp specifying whether to set fill prefix from a one-line paragraph.
When a paragraph has just one line, then after `adaptive-fill-regexp'
finds the prefix at the beginning of the line, if it doesn't
match this regexp, it is replaced with whitespace.

By default, this regexp matches sequences of just spaces and tabs.

However, we never use a prefix from a one-line paragraph
if it would act as a paragraph-starter on the second line."
  :type 'regexp
  :group 'fill)

(defcustom adaptive-fill-function nil
  "Function to call to choose a fill prefix for a paragraph, or nil.
A nil value means the function has not determined the fill prefix."
  :type '(choice (const nil) function)
  :group 'fill)

(defvar fill-indent-according-to-mode nil ;Screws up CC-mode's filling tricks.
  "Whether or not filling should try to use the major mode's indentation.")

(defun current-fill-column ()
  "Return the fill-column to use for this line.
The fill-column to use for a buffer is stored in the variable `fill-column',
but can be locally modified by the `right-margin' text property, which is
subtracted from `fill-column'.

The fill column to use for a line is the first column at which the column
number equals or exceeds the local fill-column - right-margin difference."
  (save-excursion
    (if fill-column
	(let* ((here (line-beginning-position))
	       (here-col 0)
	       (eol (progn (end-of-line) (point)))
	       margin fill-col change col)
	  ;; Look separately at each region of line with a different
	  ;; right-margin.
	  (while (and (setq margin (get-text-property here 'right-margin)
			    fill-col (- fill-column (or margin 0))
			    change (text-property-not-all
				    here eol 'right-margin margin))
		      (progn (goto-char (1- change))
			     (setq col (current-column))
			     (< col fill-col)))
	    (setq here change
		  here-col col))
	  (max here-col fill-col)))))

(defun canonically-space-region (beg end)
  "Remove extra spaces between words in region.
Leave one space between words, two at end of sentences or after colons
\(depending on values of `sentence-end-double-space', `colon-double-space',
and `sentence-end-without-period').
Remove indentation from each line."
  (interactive "*r")
  ;; Ideally, we'd want to scan the text from the end, so that changes to
  ;; text don't affect the boundary, but the regexp we match against does
  ;; not match as eagerly when matching backward, so we instead use
  ;; a marker.
  (unless (markerp end) (setq end (copy-marker end t)))
  (let ((end-spc-re (concat "\\(" (sentence-end) "\\) *\\|  +")))
    (save-excursion
      (goto-char beg)
      ;; Nuke tabs; they get screwed up in a fill.
      ;; This is quick, but loses when a tab follows the end of a sentence.
      ;; Actually, it is difficult to tell that from "Mr.\tSmith".
      ;; Blame the typist.
      (subst-char-in-region beg end ?\t ?\s)
      (while (and (< (point) end)
		  (re-search-forward end-spc-re end t))
	(delete-region
	 (cond
	  ;; `sentence-end' matched and did not match all spaces.
	  ;; I.e. it only matched the number of spaces it needs: drop the rest.
	  ((and (match-end 1) (> (match-end 0) (match-end 1)))  (match-end 1))
	  ;; `sentence-end' matched but with nothing left.  Either that means
	  ;; nothing should be removed, or it means it's the "old-style"
	  ;; sentence-end which matches all it can.  Keep only 2 spaces.
	  ;; We probably don't even need to check `sentence-end-double-space'.
	  ((match-end 1)
	   (min (match-end 0)
		(+ (if sentence-end-double-space 2 1)
		   (save-excursion (goto-char (match-end 0))
				   (skip-chars-backward " ")
				   (point)))))
	  (t ;; It's not an end of sentence.
	   (+ (match-beginning 0)
	      ;; Determine number of spaces to leave:
	      (save-excursion
		(skip-chars-backward " ]})\"'")
		(cond ((and sentence-end-double-space
			    (or (memq (preceding-char) '(?. ?? ?!))
				(and sentence-end-without-period
				     (= (char-syntax (preceding-char)) ?w)))) 2)
		      ((and colon-double-space
			    (= (preceding-char) ?:))  2)
		      ((char-equal (preceding-char) ?\n)  0)
		      (t 1))))))
	 (match-end 0))))))

(defun fill-common-string-prefix (s1 s2)
  "Return the longest common prefix of strings S1 and S2, or nil if none."
  (let ((cmp (compare-strings s1 nil nil s2 nil nil)))
    (if (eq cmp t)
	s1
      (setq cmp (1- (abs cmp)))
      (unless (zerop cmp)
	(substring s1 0 cmp)))))

(defun fill-match-adaptive-prefix ()
  (let ((str (or
              (and adaptive-fill-function (funcall adaptive-fill-function))
              (and adaptive-fill-regexp (looking-at adaptive-fill-regexp)
                   (match-string-no-properties 0)))))
    (if (>= (+ (current-left-margin) (length str)) (current-fill-column))
        ;; Death to insanely long prefixes.
        nil
      str)))

(defun fill-context-prefix (from to &optional first-line-regexp)
  "Compute a fill prefix from the text between FROM and TO.
This uses the variables `adaptive-fill-regexp' and `adaptive-fill-function'
and `adaptive-fill-first-line-regexp'.  `paragraph-start' also plays a role;
we reject a prefix based on a one-line paragraph if that prefix would
act as a paragraph-separator."
  (or first-line-regexp
      (setq first-line-regexp adaptive-fill-first-line-regexp))
  (save-excursion
    (goto-char from)
    (if (eolp) (forward-line 1))
    ;; Move to the second line unless there is just one.
    (move-to-left-margin)
    (let (first-line-prefix
	  ;; Non-nil if we are on the second line.
	  second-line-prefix)
      (setq first-line-prefix
	    ;; We don't need to consider `paragraph-start' here since it
	    ;; will be explicitly checked later on.
	    ;; Also setting first-line-prefix to nil prevents
	    ;; second-line-prefix from being used.
	    ;; ((looking-at paragraph-start) nil)
	    (fill-match-adaptive-prefix))
      (forward-line 1)
      (if (< (point) to)
          (progn
            (move-to-left-margin)
            (setq second-line-prefix
                  (cond ((looking-at paragraph-start) nil) ;Can it happen? -Stef
                        (t (fill-match-adaptive-prefix))))
            ;; If we get a fill prefix from the second line,
            ;; make sure it or something compatible is on the first line too.
            (when second-line-prefix
              (unless first-line-prefix (setq first-line-prefix ""))
              ;; If the non-whitespace chars match the first line,
              ;; just use it (this subsumes the 2 checks used previously).
              ;; Used when first line is `/* ...' and second-line is
              ;; ` * ...'.
              (let ((tmp second-line-prefix)
                    (re "\\`"))
                (while (string-match "\\`[ \t]*\\([^ \t]+\\)" tmp)
                  (setq re (concat re ".*" (regexp-quote (match-string 1 tmp))))
                  (setq tmp (substring tmp (match-end 0))))
                ;; (assert (string-match "\\`[ \t]*\\'" tmp))

                (if (string-match re first-line-prefix)
                    second-line-prefix

                  ;; Use the longest common substring of both prefixes,
                  ;; if there is one.
                  (fill-common-string-prefix first-line-prefix
                                             second-line-prefix)))))
	;; If we get a fill prefix from a one-line paragraph,
	;; maybe change it to whitespace,
	;; and check that it isn't a paragraph starter.
	(if first-line-prefix
	    (let ((result
		   ;; If first-line-prefix comes from the first line,
		   ;; see if it seems reasonable to use for all lines.
		   ;; If not, replace it with whitespace.
		   (if (or (and first-line-regexp
				(string-match first-line-regexp
					      first-line-prefix))
			   (and comment-start-skip
				(string-match comment-start-skip
					      first-line-prefix)))
		       first-line-prefix
		     (make-string (string-width first-line-prefix) ?\s))))
	      ;; But either way, reject it if it indicates the start
	      ;; of a paragraph when text follows it.
	      (if (not (eq 0 (string-match paragraph-start
					   (concat result "a"))))
		  result)))))))

(defun fill-single-word-nobreak-p ()
  "Don't break a line after the first or before the last word of a sentence."
  ;; Actually, allow breaking before the last word of a sentence, so long as
  ;; it's not the last word of the paragraph.
  (or (looking-at (concat "[ \t]*\\sw+" "\\(?:" (sentence-end) "\\)[ \t]*$"))
      (save-excursion
	(skip-chars-backward " \t")
	(and (/= (skip-syntax-backward "w") 0)
	     (/= (skip-chars-backward " \t") 0)
	     (/= (skip-chars-backward ".?!:") 0)
	     (looking-at (sentence-end))))))

(defun fill-french-nobreak-p ()
  "Return nil if French style allows breaking the line at point.
This is used in `fill-nobreak-predicate' to prevent breaking lines just
after an opening paren or just before a closing paren or a punctuation
mark such as `?' or `:'.  It is common in French writing to put a space
at such places, which would normally allow breaking the line at those
places."
  (or (looking-at "[ \t]*[])}»?!;:-]")
      (save-excursion
	(skip-chars-backward " \t")
	(unless (bolp)
	  (backward-char 1)
	  (or (looking-at "[([{«]")
	      ;; Don't cut right after a single-letter word.
	      (and (memq (preceding-char) '(?\t ?\s))
		   (eq (char-syntax (following-char)) ?w)))))))

(defcustom fill-nobreak-predicate nil
  "List of predicates for recognizing places not to break a line.
The predicates are called with no arguments, with point at the place to
be tested.  If it returns t, fill commands do not break the line there."
  :group 'fill
  :type 'hook
  :options '(fill-french-nobreak-p fill-single-word-nobreak-p))

(defcustom fill-nobreak-invisible nil
  "Non-nil means that fill commands do not break lines in invisible text."
  :type 'boolean
  :group 'fill)

(defun fill-nobreak-p ()
  "Return nil if breaking the line at point is allowed.
Can be customized with the variables `fill-nobreak-predicate'
and `fill-nobreak-invisible'."
  (or
   (and fill-nobreak-invisible (invisible-p (point)))
   (unless (bolp)
    (or
     ;; Don't break after a period followed by just one space.
     ;; Move back to the previous place to break.
     ;; The reason is that if a period ends up at the end of a
     ;; line, further fills will assume it ends a sentence.
     ;; If we now know it does not end a sentence, avoid putting
     ;; it at the end of the line.
     (and sentence-end-double-space
	  (save-excursion
	    (skip-chars-backward " ")
	    (and (eq (preceding-char) ?.)
		 (looking-at " \\([^ ]\\|$\\)"))))
     ;; Another approach to the same problem.
     (save-excursion
       (skip-chars-backward " ")
       (and (eq (preceding-char) ?.)
	    (not (progn (forward-char -1) (looking-at (sentence-end))))))
     ;; Don't split a line if the rest would look like a new paragraph.
     (unless use-hard-newlines
       (save-excursion
	 (skip-chars-forward " \t")
	 ;; If this break point is at the end of the line,
	 ;; which can occur for auto-fill, don't consider the newline
	 ;; which follows as a reason to return t.
	 (and (not (eolp))
	      (looking-at paragraph-start))))
     (run-hook-with-args-until-success 'fill-nobreak-predicate)))))

(defvar fill-find-break-point-function-table (make-char-table nil)
  "Char-table of special functions to find line breaking point.")

(defvar fill-nospace-between-words-table (make-char-table nil)
  "Char-table of characters that don't use space between words.")

(progn
  ;; Register `kinsoku' for scripts HAN, KANA, BOPOMOFO, and CJK-MISC.
  ;; Also tell that they don't use space between words.
  (map-char-table
   #'(lambda (key val)
       (when (memq val '(han kana bopomofo cjk-misc))
	 (set-char-table-range fill-find-break-point-function-table
			       key 'kinsoku)
	 (set-char-table-range fill-nospace-between-words-table
			       key t)))
   char-script-table)
  ;; Do the same thing also for full width characters and half
  ;; width kana variants.
  (set-char-table-range fill-find-break-point-function-table
			'(#xFF01 . #xFFE6) 'kinsoku)
  (set-char-table-range fill-nospace-between-words-table
			'(#xFF01 . #xFFE6) 'kinsoku))

(defun fill-find-break-point (limit)
  "Move point to a proper line breaking position of the current line.
Don't move back past the buffer position LIMIT.

This function is called when we are going to break the current line
after or before a non-ASCII character.  If the charset of the
character has the property `fill-find-break-point-function', this
function calls the property value as a function with one arg LIMIT.
If the charset has no such property, do nothing."
  (let ((func (or
	       (aref fill-find-break-point-function-table (following-char))
	       (aref fill-find-break-point-function-table (preceding-char)))))
    (if (and func (fboundp func))
	(funcall func limit))))

(defun fill-delete-prefix (from to prefix)
  "Delete the fill prefix from every line except the first.
The first line may not even have a fill prefix.
Point is moved to just past the fill prefix on the first line."
  (let ((fpre (if (and prefix (not (string-match "\\`[ \t]*\\'" prefix)))
		  (concat "[ \t]*\\("
			  (replace-regexp-in-string
			   "[ \t]+" "[ \t]*"
			   (regexp-quote prefix))
			  "\\)?[ \t]*")
		"[ \t]*")))
    (goto-char from)
    ;; Why signal an error here?  The problem needs to be caught elsewhere.
    ;; (if (>= (+ (current-left-margin) (length prefix))
    ;;         (current-fill-column))
    ;;     (error "fill-prefix too long for specified width"))
    (forward-line 1)
    (while (< (point) to)
      (if (looking-at fpre)
          (delete-region (point) (match-end 0)))
      (forward-line 1))
    (goto-char from)
    (if (looking-at fpre)
	(goto-char (match-end 0)))
    (point)))

;; The `fill-space' property carries the string with which a newline
;; should be replaced when unbreaking a line (in fill-delete-newlines).
;; It is added to newline characters by fill-newline when the default
;; behavior of fill-delete-newlines is not what we want.
(add-to-list 'text-property-default-nonsticky '(fill-space . t))

(defun fill-delete-newlines (from to justify nosqueeze squeeze-after)
  (goto-char from)
  ;; Make sure sentences ending at end of line get an extra space.
  ;; loses on split abbrevs ("Mr.\nSmith")
  (let ((eol-double-space-re
	 (cond
	  ((not colon-double-space) (concat (sentence-end) "$"))
	  ;; Try to add the : inside the `sentence-end' regexp.
	  ((string-match "\\[[^][]*\\(\\.\\)[^][]*\\]" (sentence-end))
	   (concat (replace-match ".:" nil nil (sentence-end) 1) "$"))
	  ;; Can't find the right spot to insert the colon.
	  (t "[.?!:][])}\"']*$")))
	(sentence-end-without-space-list
	 (string-to-list sentence-end-without-space)))
    (while (re-search-forward eol-double-space-re to t)
      (or (>= (point) to) (memq (char-before) '(?\t ?\s))
	  (memq (char-after (match-beginning 0))
		sentence-end-without-space-list)
	  (insert-and-inherit ?\s))))

  (goto-char from)
  (if enable-multibyte-characters
      ;; Delete unnecessary newlines surrounded by words.  The
      ;; character category `|' means that we can break a line at the
      ;; character.  And, char-table
      ;; `fill-nospace-between-words-table' tells how to concatenate
      ;; words.  If a character has non-nil value in the table, never
      ;; put spaces between words, thus delete a newline between them.
      ;; Otherwise, delete a newline only when a character preceding a
      ;; newline has non-nil value in that table.
      (while (search-forward "\n" to t)
	(if (get-text-property (match-beginning 0) 'fill-space)
	    (replace-match (get-text-property (match-beginning 0) 'fill-space))
	  (let ((prev (char-before (match-beginning 0)))
		(next (following-char)))
	    (if (and (or (aref (char-category-set next) ?|)
			 (aref (char-category-set prev) ?|))
		     (or (aref fill-nospace-between-words-table next)
			 (aref fill-nospace-between-words-table prev)))
		(delete-char -1))))))

  (goto-char from)
  (skip-chars-forward " \t")
  ;; Then change all newlines to spaces.
  (subst-char-in-region from to ?\n ?\s)
  (if (and nosqueeze (not (eq justify 'full)))
      nil
    (canonically-space-region (or squeeze-after (point)) to)
    ;; Remove trailing whitespace.
    ;; Maybe canonically-space-region should do that.
    (goto-char to) (delete-char (- (skip-chars-backward " \t"))))
  (goto-char from))

(defun fill-move-to-break-point (linebeg)
  "Move to the position where the line should be broken.
The break position will be always after LINEBEG and generally before point."
  ;; If the fill column is before linebeg, move to linebeg.
  (if (> linebeg (point)) (goto-char linebeg))
  ;; Move back to the point where we can break the line
  ;; at.  We break the line between word or after/before
  ;; the character which has character category `|'.  We
  ;; search space, \c| followed by a character, or \c|
  ;; following a character.  If not found, place
  ;; the point at linebeg.
  (while
      (when (re-search-backward "[ \t]\\|\\c|.\\|.\\c|" linebeg 0)
	;; In case of space, we place the point at next to
	;; the point where the break occurs actually,
	;; because we don't want to change the following
	;; logic of original Emacs.  In case of \c|, the
	;; point is at the place where the break occurs.
	(forward-char 1)
	(when (fill-nobreak-p) (skip-chars-backward " \t" linebeg))))

  ;; Move back over the single space between the words.
  (skip-chars-backward " \t")

  ;; If the left margin and fill prefix by themselves
  ;; pass the fill-column. or if they are zero
  ;; but we have no room for even one word,
  ;; keep at least one word or a character which has
  ;; category `|' anyway.
  (if (>= linebeg (point))
      ;; Ok, skip at least one word or one \c| character.
      ;; Meanwhile, don't stop at a period followed by one space.
      (let ((to (line-end-position))
	    (first t))
	(goto-char linebeg)
	(while (and (< (point) to) (or first (fill-nobreak-p)))
	  ;; Find a breakable point while ignoring the
	  ;; following spaces.
	  (skip-chars-forward " \t")
	  (if (looking-at "\\c|")
	      (forward-char 1)
	    (let ((pos (save-excursion
			 (skip-chars-forward "^ \n\t")
			 (point))))
	      (if (re-search-forward "\\c|" pos t)
		  (forward-char -1)
		(goto-char pos))))
	  (setq first nil)))

    (if enable-multibyte-characters
	;; If we are going to break the line after or
	;; before a non-ascii character, we may have to
	;; run a special function for the charset of the
	;; character to find the correct break point.
	(if (not (and (eq (charset-after (1- (point))) 'ascii)
		      (eq (charset-after (point)) 'ascii)))
	    ;; Make sure we take SOMETHING after the fill prefix if any.
	    (fill-find-break-point linebeg)))))

;; Like text-properties-at but don't include `composition' property.
(defun fill-text-properties-at (pos)
  (let ((l (text-properties-at pos))
	prop-list)
    (while l
      (unless (eq (car l) 'composition)
	(setq prop-list
	      (cons (car l) (cons (cadr l) prop-list))))
      (setq l (cddr l)))
    prop-list))

(defun fill-newline ()
  ;; Replace whitespace here with one newline, then
  ;; indent to left margin.
  (skip-chars-backward " \t")
  (insert ?\n)
  ;; Give newline the properties of the space(s) it replaces
  (set-text-properties (1- (point)) (point)
		       (fill-text-properties-at (point)))
  (and (looking-at "\\( [ \t]*\\)\\(\\c|\\)?")
       (or (aref (char-category-set (or (char-before (1- (point))) ?\000)) ?|)
	   (match-end 2))
       ;; When refilling later on, this newline would normally not be replaced
       ;; by a space, so we need to mark it specially to re-install the space
       ;; when we unfill.
       (put-text-property (1- (point)) (point) 'fill-space (match-string 1)))
  ;; If we don't want breaks in invisible text, don't insert
  ;; an invisible newline.
  (if fill-nobreak-invisible
      (remove-text-properties (1- (point)) (point)
			      '(invisible t)))
  (if (or fill-prefix
	  (not fill-indent-according-to-mode))
      (fill-indent-to-left-margin)
    (indent-according-to-mode))
  ;; Insert the fill prefix after indentation.
  (and fill-prefix (not (equal fill-prefix ""))
       ;; Markers that were after the whitespace are now at point: insert
       ;; before them so they don't get stuck before the prefix.
       (insert-before-markers-and-inherit fill-prefix)))

(defun fill-indent-to-left-margin ()
  "Indent current line to the column given by `current-left-margin'."
  (let ((beg (point)))
    (indent-line-to (current-left-margin))
    (put-text-property beg (point) 'face 'default)))

(defun fill-region-as-paragraph (from to &optional justify
				      nosqueeze squeeze-after)
  "Fill the region as one paragraph.
It removes any paragraph breaks in the region and extra newlines at the end,
indents and fills lines between the margins given by the
`current-left-margin' and `current-fill-column' functions.
\(In most cases, the variable `fill-column' controls the width.)
It leaves point at the beginning of the line following the paragraph.

Normally performs justification according to the `current-justification'
function, but with a prefix arg, does full justification instead.

From a program, optional third arg JUSTIFY can specify any type of
justification.  Fourth arg NOSQUEEZE non-nil means not to make spaces
between words canonical before filling.  Fifth arg SQUEEZE-AFTER, if non-nil,
means don't canonicalize spaces before that position.

Return the `fill-prefix' used for filling.

If `sentence-end-double-space' is non-nil, then period followed by one
space does not end a sentence, so don't break a line there."
  (interactive (progn
		 (barf-if-buffer-read-only)
		 (list (region-beginning) (region-end)
		       (if current-prefix-arg 'full))))
  (unless (memq justify '(t nil none full center left right))
    (setq justify 'full))

  ;; Make sure "to" is the endpoint.
  (goto-char (min from to))
  (setq to   (max from to))
  ;; Ignore blank lines at beginning of region.
  (skip-chars-forward " \t\n")

  (let ((from-plus-indent (point))
	(oneleft nil))

    (beginning-of-line)
    ;; We used to round up to whole line, but that prevents us from
    ;; correctly handling filling of mixed code-and-comment where we do want
    ;; to fill the comment but not the code.  So only use (point) if it's
    ;; further than `from', which means that `from' is followed by some
    ;; number of empty lines.
    (setq from (max (point) from))

    ;; Delete all but one soft newline at end of region.
    ;; And leave TO before that one.
    (goto-char to)
    (while (and (> (point) from) (eq ?\n (char-after (1- (point)))))
      (if (and oneleft
	       (not (and use-hard-newlines
			 (get-text-property (1- (point)) 'hard))))
	  (delete-char -1)
	(backward-char 1)
	(setq oneleft t)))
    (setq to (copy-marker (point) t))
    ;; ;; If there was no newline, and there is text in the paragraph, then
    ;; ;; create a newline.
    ;; (if (and (not oneleft) (> to from-plus-indent))
    ;; 	(newline))
    (goto-char from-plus-indent))

  (if (not (> to (point)))
      nil ;; There is no paragraph, only whitespace: exit now.

    (or justify (setq justify (current-justification)))

    ;; Don't let Adaptive Fill mode alter the fill prefix permanently.
    (let ((fill-prefix fill-prefix))
      ;; Figure out how this paragraph is indented, if desired.
      (when (and adaptive-fill-mode
		 (or (null fill-prefix) (string= fill-prefix "")))
	(setq fill-prefix (fill-context-prefix from to))
	;; Ignore a white-space only fill-prefix
	;; if we indent-according-to-mode.
	(when (and fill-prefix fill-indent-according-to-mode
		   (string-match "\\`[ \t]*\\'" fill-prefix))
	  (setq fill-prefix nil)))

      (goto-char from)
      (beginning-of-line)

      (if (not justify)	  ; filling disabled: just check indentation
	  (progn
	    (goto-char from)
	    (while (< (point) to)
	      (if (and (not (eolp))
		       (< (current-indentation) (current-left-margin)))
		  (fill-indent-to-left-margin))
	      (forward-line 1)))

	(if use-hard-newlines
	    (remove-list-of-text-properties from to '(hard)))
	;; Make sure first line is indented (at least) to left margin...
	(if (or (memq justify '(right center))
		(< (current-indentation) (current-left-margin)))
	    (fill-indent-to-left-margin))
	;; Delete the fill-prefix from every line.
	(fill-delete-prefix from to fill-prefix)
	(setq from (point))

	;; FROM, and point, are now before the text to fill,
	;; but after any fill prefix on the first line.

	(fill-delete-newlines from to justify nosqueeze squeeze-after)

	;; This is the actual filling loop.
	(goto-char from)
	(let (linebeg)
	  (while (< (point) to)
	    (setq linebeg (point))
	    (move-to-column (current-fill-column))
	    (if (when (< (point) to)
		  ;; Find the position where we'll break the line.
		  (forward-char 1) ;Use an immediately following space, if any.
		  (fill-move-to-break-point linebeg)
		  ;; Check again to see if we got to the end of
		  ;; the paragraph.
		  (skip-chars-forward " \t")
		  (< (point) to))
		;; Found a place to cut.
		(progn
		  (fill-newline)
		  (when justify
		    ;; Justify the line just ended, if desired.
		    (save-excursion
		      (forward-line -1)
		      (justify-current-line justify nil t))))

	      (goto-char to)
	      ;; Justify this last line, if desired.
	      (if justify (justify-current-line justify t t))))))
      ;; Leave point after final newline.
      (goto-char to)
      (unless (eobp) (forward-char 1))
      ;; Return the fill-prefix we used
      fill-prefix)))

(defsubst skip-line-prefix (prefix)
  "If point is inside the string PREFIX at the beginning of line, move past it."
  (when (and prefix
	     (< (- (point) (line-beginning-position)) (length prefix))
	     (save-excursion
	       (beginning-of-line)
	       (looking-at (regexp-quote prefix))))
    (goto-char (match-end 0))))

(defun fill-minibuffer-function (arg)
  "Fill a paragraph in the minibuffer, ignoring the prompt."
  (save-restriction
    (narrow-to-region (minibuffer-prompt-end) (point-max))
    (fill-paragraph arg)))

(defvar fill-forward-paragraph-function 'forward-paragraph
  "Function to move over paragraphs used by the filling code.
It is called with a single argument specifying the number of paragraphs to move.
Just like `forward-paragraph', it should return the number of paragraphs
left to move.")

(defun fill-forward-paragraph (arg)
  (funcall fill-forward-paragraph-function arg))

(defun fill-paragraph (&optional justify region)
  "Fill paragraph at or after point.

If JUSTIFY is non-nil (interactively, with prefix argument), justify as well.
If `sentence-end-double-space' is non-nil, then period followed by one
space does not end a sentence, so don't break a line there.
The variable `fill-column' controls the width for filling.

If `fill-paragraph-function' is non-nil, we call it (passing our
argument to it), and if it returns non-nil, we simply return its value.

If `fill-paragraph-function' is nil, return the `fill-prefix' used for filling.

The REGION argument is non-nil if called interactively; in that
case, if Transient Mark mode is enabled and the mark is active,
call `fill-region' to fill each of the paragraphs in the active
region, instead of just filling the current paragraph."
  (interactive (progn
		 (barf-if-buffer-read-only)
		 (list (if current-prefix-arg 'full) t)))
  (or
   ;; 1. Fill the region if it is active when called interactively.
   (and region transient-mark-mode mark-active
	(not (eq (region-beginning) (region-end)))
	(or (fill-region (region-beginning) (region-end) justify) t))
   ;; 2. Try fill-paragraph-function.
   (and (not (eq fill-paragraph-function t))
        (or fill-paragraph-function
            (and (minibufferp (current-buffer))
                 (= 1 (point-min))))
        (let ((function (or fill-paragraph-function
                            ;; In the minibuffer, don't count the width
                            ;; of the prompt.
                            'fill-minibuffer-function))
              ;; If fill-paragraph-function is set, it probably takes care
              ;; of comments and stuff.  If not, it will have to set
              ;; fill-paragraph-handle-comment back to t explicitly or
              ;; return nil.
              (fill-paragraph-handle-comment nil)
              (fill-paragraph-function t))
          (funcall function justify)))
   ;; 3. Try our syntax-aware filling code.
   (and fill-paragraph-handle-comment
        ;; Our code only handles \n-terminated comments right now.
        comment-start (equal comment-end "")
        (let ((fill-paragraph-handle-comment nil))
          (fill-comment-paragraph justify)))
   ;; 4. If it all fails, default to the good ol' text paragraph filling.
   (let ((before (point))
         (paragraph-start paragraph-start)
         ;; Fill prefix used for filling the paragraph.
         fill-pfx)
     ;; Try to prevent code sections and comment sections from being
     ;; filled together.
     (when (and fill-paragraph-handle-comment comment-start-skip)
       (setq paragraph-start
             (concat paragraph-start "\\|[ \t]*\\(?:"
                     comment-start-skip "\\)")))
     (save-excursion
       ;; To make sure the return value of forward-paragraph is meaningful,
       ;; we have to start from the beginning of line, otherwise skipping
       ;; past the last few chars of a paragraph-separator would count as
       ;; a paragraph (and not skipping any chars at EOB would not count
       ;; as a paragraph even if it is).
       (move-to-left-margin)
       (if (not (zerop (fill-forward-paragraph 1)))
           ;; There's no paragraph at or after point: give up.
           (setq fill-pfx "")
         (let ((end (point))
               (beg (progn (fill-forward-paragraph -1) (point))))
           (goto-char before)
           (setq fill-pfx
                 (if use-hard-newlines
                     ;; Can't use fill-region-as-paragraph, since this
                     ;; paragraph may still contain hard newlines.  See
                     ;; fill-region.
                     (fill-region beg end justify)
                   (fill-region-as-paragraph beg end justify))))))
     fill-pfx)))

(declare-function comment-search-forward "newcomment" (limit &optional noerror))
(declare-function comment-string-strip "newcomment" (str beforep afterp))


(defun fill-comment-paragraph (&optional justify)
  "Fill current comment.
If we're not in a comment, just return nil so that the caller
can take care of filling.  JUSTIFY is used as in `fill-paragraph'."
  (comment-normalize-vars)
  (let (has-code-and-comment ; Non-nil if it contains code and a comment.
	comin comstart)
    ;; Figure out what kind of comment we are looking at.
    (save-excursion
      (beginning-of-line)
      (when (setq comstart (comment-search-forward (line-end-position) t))
	(setq comin (point))
	(goto-char comstart) (skip-chars-backward " \t")
	(setq has-code-and-comment (not (bolp)))))

    (if (not (and comstart
                  ;; Make sure the comment-start mark we found is accepted by
                  ;; comment-start-skip.  If not, all bets are off, and
                  ;; we'd better not mess with it.
                  (string-match comment-start-skip
                                (buffer-substring comstart comin))))

	;; Return nil, so the normal filling will take place.
	nil

      ;; Narrow to include only the comment, and then fill the region.
      (let* ((fill-prefix fill-prefix)
	     (commark
	      (comment-string-strip (buffer-substring comstart comin) nil t))
	     (comment-re
              ;; A regexp more specialized than comment-start-skip, that only
              ;; matches the current commark rather than any valid commark.
              ;;
              ;; The specialized regexp only works for "normal" comment
              ;; syntax, not for Texinfo's "@c" (which can't be immediately
              ;; followed by word-chars) or Fortran's "C" (which needs to be
              ;; at bol), so check that comment-start-skip indeed allows the
              ;; commark to appear in the middle of the line and followed by
              ;; word chars.  The choice of "\0" and "a" is mostly arbitrary.
              (if (string-match comment-start-skip (concat "\0" commark "a"))
                  (concat "[ \t]*" (regexp-quote commark)
                          ;; Make sure we only match comments that
                          ;; use the exact same comment marker.
                          "[^" (substring commark -1) "]")
                (concat "[ \t]*\\(?:" comment-start-skip "\\)")))
             (comment-fill-prefix	; Compute a fill prefix.
	      (save-excursion
		(goto-char comstart)
		(if has-code-and-comment
		    (concat
		     (if (not indent-tabs-mode)
			 (make-string (current-column) ?\s)
		       (concat
			(make-string (/ (current-column) tab-width) ?\t)
			(make-string (% (current-column) tab-width) ?\s)))
		     (buffer-substring (point) comin))
		  (buffer-substring (line-beginning-position) comin))))
	     beg end)
	(save-excursion
	  (save-restriction
	    (beginning-of-line)
	    (narrow-to-region
	     ;; Find the first line we should include in the region to fill.
	     (if has-code-and-comment
		 (line-beginning-position)
	       (save-excursion
		 (while (and (zerop (forward-line -1))
			     (looking-at comment-re)))
		 ;; We may have gone too far.  Go forward again.
		 (line-beginning-position
		  (if (progn
			(goto-char
			 (or (comment-search-forward (line-end-position) t)
			     (point)))
			(looking-at comment-re))
		      (progn (setq comstart (point)) 1)
		    (progn (setq comstart (point)) 2)))))
	     ;; Find the beginning of the first line past the region to fill.
	     (save-excursion
	       (while (progn (forward-line 1)
			     (looking-at comment-re)))
	       (point)))
	    ;; Obey paragraph starters and boundaries within comments.
	    (let* ((paragraph-separate
		    ;; Use the default values since they correspond to
		    ;; the values to use for plain text.
		    (concat paragraph-separate "\\|[ \t]*\\(?:"
			    comment-start-skip "\\)\\(?:"
			    (default-value 'paragraph-separate) "\\)"))
		   (paragraph-start
		    (concat paragraph-start "\\|[ \t]*\\(?:"
			    comment-start-skip "\\)\\(?:"
			    (default-value 'paragraph-start) "\\)"))
		   ;; We used to rely on fill-prefix to break paragraph at
		   ;; comment-starter changes, but it did not work for the
		   ;; first line (mixed comment&code).
		   ;; We now use comment-re instead to "manually" make sure
		   ;; we treat comment-marker changes as paragraph boundaries.
		   ;; (paragraph-ignore-fill-prefix nil)
		   ;; (fill-prefix comment-fill-prefix)
		   (after-line (if has-code-and-comment
				   (line-beginning-position 2))))
	      (setq end (progn (forward-paragraph) (point)))
	      ;; If this comment starts on a line with code,
	      ;; include that line in the filling.
	      (setq beg (progn (backward-paragraph)
			       (if (eq (point) after-line)
				   (forward-line -1))
			       (point)))))

	  ;; Find the fill-prefix to use.
	  (cond
	   (fill-prefix)	  ; Use the user-provided fill prefix.
	   ((and adaptive-fill-mode	; Try adaptive fill mode.
		 (setq fill-prefix (fill-context-prefix beg end))
		 (string-match comment-start-skip fill-prefix)))
	   (t
	    (setq fill-prefix comment-fill-prefix)))

	  ;; Don't fill with narrowing.
	  (or
	   (fill-region-as-paragraph
	    (max comstart beg) end justify nil
	    ;; Don't canonicalize spaces within the code just before
	    ;; the comment.
	    (save-excursion
	      (goto-char beg)
	      (if (looking-at fill-prefix)
		  nil
		(re-search-forward comment-start-skip))))
	   ;; Make sure we don't return nil.
	   t))))))

(defun fill-region (from to &optional justify nosqueeze to-eop)
  "Fill each of the paragraphs in the region.
A prefix arg means justify as well.
The `fill-column' variable controls the width.

Noninteractively, the third argument JUSTIFY specifies which
kind of justification to do: `full', `left', `right', `center',
or `none' (equivalent to nil).  A value of t means handle each
paragraph as specified by its text properties.

The fourth arg NOSQUEEZE non-nil means to leave whitespace other
than line breaks untouched, and fifth arg TO-EOP non-nil means
to keep filling to the end of the paragraph (or next hard newline,
if variable `use-hard-newlines' is on).

Return the fill-prefix used for filling the last paragraph.

If `sentence-end-double-space' is non-nil, then period followed by one
space does not end a sentence, so don't break a line there."
  (interactive (progn
		 (barf-if-buffer-read-only)
		 (list (region-beginning) (region-end)
		       (if current-prefix-arg 'full))))
  (unless (memq justify '(t nil none full center left right))
    (setq justify 'full))
  (let (max beg fill-pfx)
    (goto-char (max from to))
    (when to-eop
      (skip-chars-backward "\n")
      (fill-forward-paragraph 1))
    (setq max (copy-marker (point) t))
    (goto-char (setq beg (min from to)))
    (beginning-of-line)
    (while (< (point) max)
      (let ((initial (point))
	    end)
	;; If using hard newlines, break at every one for filling
	;; purposes rather than using paragraph breaks.
	(if use-hard-newlines
	    (progn
	      (while (and (setq end (text-property-any (point) max
						       'hard t))
			  (not (= ?\n (char-after end)))
			  (not (>= end max)))
		(goto-char (1+ end)))
	      (setq end (if end (min max (1+ end)) max))
	      (goto-char initial))
	  (fill-forward-paragraph 1)
	  (setq end (min max (point)))
	  (fill-forward-paragraph -1))
	(if (< (point) beg)
	    (goto-char beg))
	(if (and (>= (point) initial) (< (point) end))
	    (setq fill-pfx
		  (fill-region-as-paragraph (point) end justify nosqueeze))
	  (goto-char end))))
    fill-pfx))


(defcustom default-justification 'left
  "Method of justifying text not otherwise specified.
Possible values are `left', `right', `full', `center', or `none'.
The requested kind of justification is done whenever lines are filled.
The `justification' text-property can locally override this variable."
  :type '(choice (const left)
		 (const right)
		 (const full)
		 (const center)
		 (const none))
  :safe 'symbolp
  :group 'fill)
(make-variable-buffer-local 'default-justification)

(defun current-justification ()
  "How should we justify this line?
This returns the value of the text-property `justification',
or the variable `default-justification' if there is no text-property.
However, it returns nil rather than `none' to mean \"don't justify\"."
  (let ((j (or (get-text-property
		;; Make sure we're looking at paragraph body.
		(save-excursion (skip-chars-forward " \t")
				(if (and (eobp) (not (bobp)))
				    (1- (point)) (point)))
		'justification)
	       default-justification)))
    (if (eq 'none j)
	nil
      j)))

(defun set-justification (begin end style &optional whole-par)
  "Set the region's justification style to STYLE.
This commands prompts for the kind of justification to use.
If the mark is not active, this command operates on the current paragraph.
If the mark is active, it operates on the region.  However, if the
beginning and end of the region are not at paragraph breaks, they are
moved to the beginning and end \(respectively) of the paragraphs they
are in.

If variable `use-hard-newlines' is true, all hard newlines are
taken to be paragraph breaks.

When calling from a program, operates just on region between BEGIN and END,
unless optional fourth arg WHOLE-PAR is non-nil.  In that case bounds are
extended to include entire paragraphs as in the interactive command."
  (interactive (list (if mark-active (region-beginning) (point))
		     (if mark-active (region-end) (point))
		     (let ((s (completing-read
			       "Set justification to: "
			       '(("left") ("right") ("full")
				 ("center") ("none"))
			       nil t)))
		       (if (equal s "") (error ""))
		       (intern s))
		     t))
  (save-excursion
    (save-restriction
      (if whole-par
	  (let ((paragraph-start (if use-hard-newlines "." paragraph-start))
		(paragraph-ignore-fill-prefix (if use-hard-newlines t
						paragraph-ignore-fill-prefix)))
	    (goto-char begin)
	    (while (and (bolp) (not (eobp))) (forward-char 1))
	    (backward-paragraph)
	    (setq begin (point))
	    (goto-char end)
	    (skip-chars-backward " \t\n" begin)
	    (forward-paragraph)
	    (setq end (point))))

      (narrow-to-region (point-min) end)
      (unjustify-region begin (point-max))
      (put-text-property begin (point-max) 'justification style)
      (fill-region begin (point-max) nil t))))

(defun set-justification-none (b e)
  "Disable automatic filling for paragraphs in the region.
If the mark is not active, this applies to the current paragraph."
  (interactive (list (if mark-active (region-beginning) (point))
		     (if mark-active (region-end) (point))))
  (set-justification b e 'none t))

(defun set-justification-left (b e)
  "Make paragraphs in the region left-justified.
This means they are flush at the left margin and ragged on the right.
This is usually the default, but see the variable `default-justification'.
If the mark is not active, this applies to the current paragraph."
  (interactive (list (if mark-active (region-beginning) (point))
		     (if mark-active (region-end) (point))))
  (set-justification b e 'left t))

(defun set-justification-right (b e)
  "Make paragraphs in the region right-justified.
This means they are flush at the right margin and ragged on the left.
If the mark is not active, this applies to the current paragraph."
  (interactive (list (if mark-active (region-beginning) (point))
		     (if mark-active (region-end) (point))))
  (set-justification b e 'right t))

(defun set-justification-full (b e)
  "Make paragraphs in the region fully justified.
This makes lines flush on both margins by inserting spaces between words.
If the mark is not active, this applies to the current paragraph."
  (interactive (list (if mark-active (region-beginning) (point))
		     (if mark-active (region-end) (point))))
  (set-justification b e 'full t))

(defun set-justification-center (b e)
  "Make paragraphs in the region centered.
If the mark is not active, this applies to the current paragraph."
  (interactive (list (if mark-active (region-beginning) (point))
		     (if mark-active (region-end) (point))))
  (set-justification b e 'center t))

;; A line has up to six parts:
;;
;;           >>>                    hello.
;; [Indent-1][FP][    Indent-2     ][text][trailing whitespace][newline]
;;
;; "Indent-1" is the left-margin indentation; normally it ends at column
;;     given by the `current-left-margin' function.
;; "FP" is the fill-prefix.  It can be any string, including whitespace.
;; "Indent-2" is added to justify a line if the `current-justification' is
;;     `center' or `right'.  In `left' and `full' justification regions, any
;;     whitespace there is part of the line's text, and should not be changed.
;; Trailing whitespace is not counted as part of the line length when
;; center- or right-justifying.
;;
;; All parts of the line are optional, although the final newline can
;;     only be missing on the last line of the buffer.

(defun justify-current-line (&optional how eop nosqueeze)
  "Do some kind of justification on this line.
Normally does full justification: adds spaces to the line to make it end at
the column given by `current-fill-column'.
Optional first argument HOW specifies alternate type of justification:
it can be `left', `right', `full', `center', or `none'.
If HOW is t, will justify however the `current-justification' function says to.
If HOW is nil or missing, full justification is done by default.
Second arg EOP non-nil means that this is the last line of the paragraph, so
it will not be stretched by full justification.
Third arg NOSQUEEZE non-nil means to leave interior whitespace unchanged,
otherwise it is made canonical."
  (interactive "*")
  (if (eq t how) (setq how (or (current-justification) 'none))
    (if (null how) (setq how 'full)
      (or (memq how '(none left right center))
	  (setq how 'full))))
  (or (memq how '(none left))  ; No action required for these.
      (let ((fc (current-fill-column))
	    (pos (point-marker))
	    fp-end			; point at end of fill prefix
	    beg				; point at beginning of line's text
	    end				; point at end of line's text
	    indent			; column of `beg'
	    endcol			; column of `end'
	    ncols			; new indent point or offset
	    (nspaces 0)			; number of spaces between words
					; in line (not space characters)
	    (curr-fracspace 0)		; current fractional space amount
	    count)
	(end-of-line)
	;; Check if this is the last line of the paragraph.
	(if (and use-hard-newlines (null eop)
		 (get-text-property (point) 'hard))
	    (setq eop t))
	(skip-chars-backward " \t")
	;; Quick exit if it appears to be properly justified already
	;; or there is no text.
	(if (or (bolp)
		(and (memq how '(full right))
		     (= (current-column) fc)))
	    nil
	  (setq end (point))
	  (beginning-of-line)
	  (skip-chars-forward " \t")
	  ;; Skip over fill-prefix.
	  (if (and fill-prefix
		   (not (string-equal fill-prefix ""))
		   (equal fill-prefix
			  (buffer-substring
			   (point) (min (point-max) (+ (length fill-prefix)
						       (point))))))
	      (forward-char (length fill-prefix))
	    (if (and adaptive-fill-mode
		     (looking-at adaptive-fill-regexp))
		(goto-char (match-end 0))))
	  (setq fp-end (point))
	  (skip-chars-forward " \t")
	  ;; This is beginning of the line's text.
	  (setq indent (current-column))
	  (setq beg (point))
	  (goto-char end)
	  (setq endcol (current-column))

	  ;; HOW can't be null or left--we would have exited already
	  (cond ((eq 'right how)
		 (setq ncols (- fc endcol))
		 (if (< ncols 0)
		     ;; Need to remove some indentation
		     (delete-region
		      (progn (goto-char fp-end)
			     (if (< (current-column) (+ indent ncols))
				 (move-to-column (+ indent ncols) t))
			     (point))
		      (progn (move-to-column indent) (point)))
		   ;; Need to add some
		   (goto-char beg)
		   (indent-to (+ indent ncols))
		   ;; If point was at beginning of text, keep it there.
		   (if (= beg pos)
		       (move-marker pos (point)))))

		((eq 'center how)
		 ;; Figure out how much indentation is needed
		 (setq ncols (+ (current-left-margin)
				(/ (- fc (current-left-margin) ;avail. space
				      (- endcol indent)) ;text width
				   2)))
		 (if (< ncols indent)
		     ;; Have too much indentation - remove some
		     (delete-region
		      (progn (goto-char fp-end)
			     (if (< (current-column) ncols)
				 (move-to-column ncols t))
			     (point))
		      (progn (move-to-column indent) (point)))
		   ;; Have too little - add some
		   (goto-char beg)
		   (indent-to ncols)
		   ;; If point was at beginning of text, keep it there.
		   (if (= beg pos)
		       (move-marker pos (point)))))

		((eq 'full how)
		 ;; Insert extra spaces between words to justify line
		 (save-restriction
		   (narrow-to-region beg end)
		   (or nosqueeze
		       (canonically-space-region beg end))
		   (goto-char (point-max))
		   ;; count word spaces in line
		   (while (search-backward " " nil t)
		     (setq nspaces (1+ nspaces))
		     (skip-chars-backward " "))
		   (setq ncols (- fc endcol))
		   ;; Ncols is number of additional space chars needed
		   (when (and (> ncols 0) (> nspaces 0) (not eop))
                     (setq curr-fracspace (+ ncols (/ nspaces 2))
                           count nspaces)
                     (while (> count 0)
                       (skip-chars-forward " ")
                       (insert-char ?\s (/ curr-fracspace nspaces) t)
                       (search-forward " " nil t)
                       (setq count (1- count)
                             curr-fracspace
                             (+ (% curr-fracspace nspaces) ncols))))))
		(t (error "Unknown justification value"))))
	(goto-char pos)
	(move-marker pos nil)))
  nil)

(defun unjustify-current-line ()
  "Remove justification whitespace from current line.
If the line is centered or right-justified, this function removes any
indentation past the left margin.  If the line is full-justified, it removes
extra spaces between words.  It does nothing in other justification modes."
  (let ((justify (current-justification)))
    (cond ((eq 'left justify) nil)
	  ((eq  nil  justify) nil)
	  ((eq 'full justify)		; full justify: remove extra spaces
	   (beginning-of-line-text)
	   (canonically-space-region (point) (line-end-position)))
	  ((memq justify '(center right))
	   (save-excursion
	     (move-to-left-margin nil t)
	     ;; Position ourselves after any fill-prefix.
	     (if (and fill-prefix
		      (not (string-equal fill-prefix ""))
		      (equal fill-prefix
			     (buffer-substring
			      (point) (min (point-max) (+ (length fill-prefix)
							  (point))))))
		 (forward-char (length fill-prefix)))
	     (delete-region (point) (progn (skip-chars-forward " \t")
					   (point))))))))

(defun unjustify-region (&optional begin end)
  "Remove justification whitespace from region.
For centered or right-justified regions, this function removes any indentation
past the left margin from each line.  For full-justified lines, it removes
extra spaces between words.  It does nothing in other justification modes.
Arguments BEGIN and END are optional; default is the whole buffer."
  (save-excursion
    (save-restriction
      (if end (narrow-to-region (point-min) end))
      (goto-char (or begin (point-min)))
      (while (not (eobp))
	(unjustify-current-line)
	(forward-line 1)))))


(defun fill-nonuniform-paragraphs (min max &optional justifyp citation-regexp)
  "Fill paragraphs within the region, allowing varying indentation within each.
This command divides the region into \"paragraphs\",
only at paragraph-separator lines, then fills each paragraph
using as the fill prefix the smallest indentation of any line
in the paragraph.

When calling from a program, pass range to fill as first two arguments.

Optional third and fourth arguments JUSTIFYP and CITATION-REGEXP:
JUSTIFYP to justify paragraphs (prefix arg).
When filling a mail message, pass a regexp for CITATION-REGEXP
which will match the prefix of a line which is a citation marker
plus whitespace, but no other kind of prefix.
Also, if CITATION-REGEXP is non-nil, don't fill header lines."
  (interactive (progn
		 (barf-if-buffer-read-only)
		 (list (region-beginning) (region-end)
		       (if current-prefix-arg 'full))))
  (let ((fill-individual-varying-indent t))
    (fill-individual-paragraphs min max justifyp citation-regexp)))

(defun fill-individual-paragraphs (min max &optional justify citation-regexp)
  "Fill paragraphs of uniform indentation within the region.
This command divides the region into \"paragraphs\",
treating every change in indentation level or prefix as a paragraph boundary,
then fills each paragraph using its indentation level as the fill prefix.

There is one special case where a change in indentation does not start
a new paragraph.  This is for text of this form:

   foo>    This line with extra indentation starts
   foo> a paragraph that continues on more lines.

These lines are filled together.

When calling from a program, pass the range to fill
as the first two arguments.

Optional third and fourth arguments JUSTIFY and CITATION-REGEXP:
JUSTIFY to justify paragraphs (prefix arg).
When filling a mail message, pass a regexp for CITATION-REGEXP
which will match the prefix of a line which is a citation marker
plus whitespace, but no other kind of prefix.
Also, if CITATION-REGEXP is non-nil, don't fill header lines."
  (interactive (progn
		 (barf-if-buffer-read-only)
		 (list (region-beginning) (region-end)
		       (if current-prefix-arg 'full))))
  (save-restriction
    (save-excursion
      (goto-char min)
      (beginning-of-line)
      (narrow-to-region (point) max)
      (if citation-regexp
	  (while (and (not (eobp))
		      (or (looking-at "[ \t]*[^ \t\n]+:")
			  (looking-at "[ \t]*$")))
	    (if (looking-at "[ \t]*[^ \t\n]+:")
		(search-forward "\n\n" nil 'move)
	      (forward-line 1))))
      (narrow-to-region (point) max)
      ;; Loop over paragraphs.
      (while (progn
	       ;; Skip over all paragraph-separating lines
	       ;; so as to not include them in any paragraph.
               (while (and (not (eobp))
			   (progn (move-to-left-margin)
				  (and (not (eobp))
				       (looking-at paragraph-separate))))
                 (forward-line 1))
               (skip-chars-forward " \t\n") (not (eobp)))
	(move-to-left-margin)
	(let ((start (point))
	      fill-prefix fill-prefix-regexp)
	  ;; Find end of paragraph, and compute the smallest fill-prefix
	  ;; that fits all the lines in this paragraph.
	  (while (progn
		   ;; Update the fill-prefix on the first line
		   ;; and whenever the prefix good so far is too long.
		   (if (not (and fill-prefix
				 (looking-at fill-prefix-regexp)))
		       (setq fill-prefix
			     (fill-individual-paragraphs-prefix
			      citation-regexp)
			     fill-prefix-regexp (regexp-quote fill-prefix)))
		   (forward-line 1)
		   (if (bolp)
		       ;; If forward-line went past a newline,
		       ;; move further to the left margin.
		       (move-to-left-margin))
		   ;; Now stop the loop if end of paragraph.
		   (and (not (eobp))
			(if fill-individual-varying-indent
			    ;; If this line is a separator line, with or
			    ;; without prefix, end the paragraph.
			    (and
			     (not (looking-at paragraph-separate))
			     (save-excursion
			       (not (and (looking-at fill-prefix-regexp)
					 (progn (forward-char
						 (length fill-prefix))
						(looking-at
						 paragraph-separate))))))
			  ;; If this line has more or less indent
			  ;; than the fill prefix wants, end the paragraph.
			  (and (looking-at fill-prefix-regexp)
			       ;; If fill prefix is shorter than a new
			       ;; fill prefix computed here, end paragraph.
 			       (let ((this-line-fill-prefix
				      (fill-individual-paragraphs-prefix
				       citation-regexp)))
 				 (>= (length fill-prefix)
 				     (length this-line-fill-prefix)))
			       (save-excursion
				 (not (progn (forward-char
					      (length fill-prefix))
					     (or (looking-at "[ \t]")
						 (looking-at paragraph-separate)
						 (looking-at paragraph-start)))))
			       (not (and (equal fill-prefix "")
					 citation-regexp
					 (looking-at citation-regexp))))))))
	  ;; Fill this paragraph, but don't add a newline at the end.
	  (let ((had-newline (bolp)))
	    (fill-region-as-paragraph start (point) justify)
	    (if (and (bolp) (not had-newline))
		(delete-char -1))))))))

(defun fill-individual-paragraphs-prefix (citation-regexp)
  (let* ((adaptive-fill-first-line-regexp ".*")
	 (just-one-line-prefix
	  ;; Accept any prefix rather than just the ones matched by
	  ;; adaptive-fill-first-line-regexp.
	  (fill-context-prefix (point) (line-beginning-position 2)))
	 (two-lines-prefix
	  (fill-context-prefix (point) (line-beginning-position 3))))
    (if (not just-one-line-prefix)
	(buffer-substring
	 (point) (save-excursion (skip-chars-forward " \t") (point)))
	;; See if the citation part of JUST-ONE-LINE-PREFIX
	;; is the same as that of TWO-LINES-PREFIX,
	;; except perhaps with longer whitespace.
      (if (and just-one-line-prefix two-lines-prefix
	       (let* ((one-line-citation-part
		       (fill-individual-paragraphs-citation
			just-one-line-prefix citation-regexp))
		      (two-lines-citation-part
		       (fill-individual-paragraphs-citation
			two-lines-prefix citation-regexp))
		      (adjusted-two-lines-citation-part
		       (substring two-lines-citation-part 0
				  (string-match "[ \t]*\\'"
						two-lines-citation-part))))
		 (and
		 (string-match (concat "\\`"
				       (regexp-quote
					adjusted-two-lines-citation-part)
				       "[ \t]*\\'")
			       one-line-citation-part)
		 (>= (string-width one-line-citation-part)
		      (string-width two-lines-citation-part)))))
	    two-lines-prefix
	just-one-line-prefix))))

(defun fill-individual-paragraphs-citation (string citation-regexp)
  (if citation-regexp
      (if (string-match citation-regexp string)
	  (match-string 0 string)
	"")
    string))

;;; fill.el ends here
