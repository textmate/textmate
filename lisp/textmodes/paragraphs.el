;;; paragraphs.el --- paragraph and sentence parsing

;; Copyright (C) 1985-1987, 1991, 1994-1997, 1999-2012
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

;; This package provides the paragraph-oriented commands documented in the
;; Emacs manual.

;;; Code:

(defgroup paragraphs nil
  "Paragraph and sentence parsing."
  :group 'editing)

(put 'use-hard-newlines 'permanent-local t)
(define-minor-mode use-hard-newlines
  "Toggle distinguishing between hard and soft newlines.
With a prefix argument ARG, enable the feature if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
it if ARG is omitted or nil.

When enabled, the functions `newline' and `open-line' add the
text-property `hard' to newlines that they insert, and a line is
only considered as a candidate to match `paragraph-start' or
`paragraph-separate' if it follows a hard newline.

When enabling, if there are newlines in the buffer but no hard
newlines, ask the user whether to mark as hard any newlines
preceding a `paragraph-start' line.  From a program, second arg
INSERT specifies whether to do this; it can be `never' to change
nothing, t or `always' to force marking, `guess' to try to do the
right thing with no questions, nil or anything else to ask the
user.

Newlines not marked hard are called \"soft\", and are always internal
to paragraphs.  The fill functions insert and delete only soft newlines."
  :group 'paragraphs
  :extra-args (insert)
  (when use-hard-newlines
    ;; Turn mode on
    ;; Intuit hard newlines --
    ;;   mark as hard any newlines preceding a paragraph-start line.
    (if (or (eq insert t) (eq insert 'always)
	    (and (not (eq 'never insert))
		 (not (text-property-any (point-min) (point-max) 'hard t))
		 (save-excursion
		   (goto-char (point-min))
		   (search-forward "\n" nil t))
		 (or (eq insert 'guess)
		     (y-or-n-p "Make newlines between paragraphs hard? "))))
	(save-excursion
	  (goto-char (point-min))
	  (while (search-forward "\n" nil t)
	    (let ((pos (point)))
	      (move-to-left-margin)
	      (when (looking-at paragraph-start)
		(set-hard-newline-properties (1- pos) pos))
	      ;; If paragraph-separate, newline after it is hard too.
	      (when (looking-at paragraph-separate)
		(set-hard-newline-properties (1- pos) pos)
		(end-of-line)
		(unless (eobp)
		  (set-hard-newline-properties (point) (1+ (point)))))))))))

(defcustom paragraph-start "\f\\|[ \t]*$" "\
Regexp for beginning of a line that starts OR separates paragraphs.
This regexp should match lines that separate paragraphs
and should also match lines that start a paragraph
\(and are part of that paragraph).

This is matched against the text at the left margin, which is not necessarily
the beginning of the line, so it should never use \"^\" as an anchor.  This
ensures that the paragraph functions will work equally well within a region
of text indented by a margin setting.

The variable `paragraph-separate' specifies how to distinguish
lines that start paragraphs from lines that separate them.

If the variable `use-hard-newlines' is non-nil, then only lines following a
hard newline are considered to match."
  :group 'paragraphs
  :type 'regexp)
(put 'paragraph-start 'safe-local-variable 'stringp)

;; paragraph-start requires a hard newline, but paragraph-separate does not:
;; It is assumed that paragraph-separate is distinctive enough to be believed
;; whenever it occurs, while it is reasonable to set paragraph-start to
;; something very minimal, even including "." (which makes every hard newline
;; start a new paragraph).

(defcustom paragraph-separate "[ \t\f]*$"
  "Regexp for beginning of a line that separates paragraphs.
If you change this, you may have to change `paragraph-start' also.

This is matched against the text at the left margin, which is not necessarily
the beginning of the line, so it should not use \"^\" as an anchor.  This
ensures that the paragraph functions will work equally within a region of
text indented by a margin setting."
  :group 'paragraphs
  :type 'regexp)
(put 'paragraph-separate 'safe-local-variable 'stringp)

(defcustom sentence-end-double-space t
  "Non-nil means a single space does not end a sentence.
This is relevant for filling.  See also `sentence-end-without-period'
and `colon-double-space'.

This value is used by the function `sentence-end' to construct the
regexp describing the end of a sentence, when the value of the variable
`sentence-end' is nil.  See Info node `(elisp)Standard Regexps'."
  :type 'boolean
  :group 'fill)
(put 'sentence-end-double-space 'safe-local-variable 'booleanp)

(defcustom sentence-end-without-period nil
  "Non-nil means a sentence will end without a period.
For example, a sentence in Thai text ends with double space but
without a period.

This value is used by the function `sentence-end' to construct the
regexp describing the end of a sentence, when the value of the variable
`sentence-end' is nil.  See Info node `(elisp)Standard Regexps'."
  :type 'boolean
  :group 'fill)
(put 'sentence-end-without-period 'safe-local-variable 'booleanp)

(defcustom sentence-end-without-space
  "。．？！"
  "String of characters that end sentence without following spaces.

This value is used by the function `sentence-end' to construct the
regexp describing the end of a sentence, when the value of the variable
`sentence-end' is nil.  See Info node `(elisp)Standard Regexps'."
  :group 'paragraphs
  :type 'string)
(put 'sentence-end-without-space 'safe-local-variable 'stringp)

(defcustom sentence-end nil
  "Regexp describing the end of a sentence.
The value includes the whitespace following the sentence.
All paragraph boundaries also end sentences, regardless.

The value nil means to use the default value defined by the
function `sentence-end'.  You should always use this function
to obtain the value of this variable."
  :group 'paragraphs
  :type '(choice regexp (const :tag "Use default value" nil)))
(put 'sentence-end 'safe-local-variable 'string-or-null-p)

(defcustom sentence-end-base "[.?!][]\"'”)}]*"
  "Regexp matching the basic end of a sentence, not including following space."
  :group 'paragraphs
  :type 'string
  :version "22.1")
(put 'sentence-end-base 'safe-local-variable 'stringp)

(defun sentence-end ()
  "Return the regexp describing the end of a sentence.

This function returns either the value of the variable `sentence-end'
if it is non-nil, or the default value constructed from the
variables `sentence-end-base', `sentence-end-double-space',
`sentence-end-without-period' and `sentence-end-without-space'.

The default value specifies that in order to be recognized as the
end of a sentence, the ending period, question mark, or exclamation point
must be followed by two spaces, with perhaps some closing delimiters
in between.  See Info node `(elisp)Standard Regexps'."
  (or sentence-end
      ;; We accept non-break space along with space.
      (concat (if sentence-end-without-period "\\w[ \u00a0][ \u00a0]\\|")
	      "\\("
	      sentence-end-base
              (if sentence-end-double-space
                  "\\($\\|[ \u00a0]$\\|\t\\|[ \u00a0][ \u00a0]\\)" "\\($\\|[\t \u00a0]\\)")
              "\\|[" sentence-end-without-space "]+"
	      "\\)"
              "[ \u00a0\t\n]*")))

(defcustom page-delimiter "^\014"
  "Regexp describing line-beginnings that separate pages."
  :group 'paragraphs
  :type 'regexp)
(put 'page-delimiter 'safe-local-variable 'stringp)

(defcustom paragraph-ignore-fill-prefix nil
  "Non-nil means the paragraph commands are not affected by `fill-prefix'.
This is desirable in modes where blank lines are the paragraph delimiters."
  :group 'paragraphs
  :type 'boolean)
(put 'paragraph-ignore-fill-prefix 'safe-local-variable 'booleanp)

(defun forward-paragraph (&optional arg)
  "Move forward to end of paragraph.
With argument ARG, do it ARG times;
a negative argument ARG = -N means move backward N paragraphs.

A line which `paragraph-start' matches either separates paragraphs
\(if `paragraph-separate' matches it also) or is the first line of a paragraph.
A paragraph end is the beginning of a line which is not part of the paragraph
to which the end of the previous line belongs, or the end of the buffer.
Returns the count of paragraphs left to move."
  (interactive "^p")
  (or arg (setq arg 1))
  (let* ((opoint (point))
	 (fill-prefix-regexp
	  (and fill-prefix (not (equal fill-prefix ""))
	       (not paragraph-ignore-fill-prefix)
	       (regexp-quote fill-prefix)))
	 ;; Remove ^ from paragraph-start and paragraph-sep if they are there.
	 ;; These regexps shouldn't be anchored, because we look for them
	 ;; starting at the left-margin.  This allows paragraph commands to
	 ;; work normally with indented text.
	 ;; This hack will not find problem cases like "whatever\\|^something".
	 (parstart (if (and (not (equal "" paragraph-start))
			    (equal ?^ (aref paragraph-start 0)))
		       (substring paragraph-start 1)
		     paragraph-start))
	 (parsep (if (and (not (equal "" paragraph-separate))
			  (equal ?^ (aref paragraph-separate 0)))
		     (substring paragraph-separate 1)
		   paragraph-separate))
	 (parsep
	  (if fill-prefix-regexp
	      (concat parsep "\\|"
		      fill-prefix-regexp "[ \t]*$")
	    parsep))
	 ;; This is used for searching.
	 (sp-parstart (concat "^[ \t]*\\(?:" parstart "\\|" parsep "\\)"))
	 start found-start)
    (while (and (< arg 0) (not (bobp)))
      (if (and (not (looking-at parsep))
	       (re-search-backward "^\n" (max (1- (point)) (point-min)) t)
	       (looking-at parsep))
	  (setq arg (1+ arg))
	(setq start (point))
	;; Move back over paragraph-separating lines.
	(forward-char -1) (beginning-of-line)
	(while (and (not (bobp))
		    (progn (move-to-left-margin)
			   (looking-at parsep)))
	  (forward-line -1))
	(if (bobp)
	    nil
	  (setq arg (1+ arg))
	  ;; Go to end of the previous (non-separating) line.
	  (end-of-line)
	  ;; Search back for line that starts or separates paragraphs.
	  (if (if fill-prefix-regexp
		  ;; There is a fill prefix; it overrides parstart.
		  (let (multiple-lines)
		    (while (and (progn (beginning-of-line) (not (bobp)))
				(progn (move-to-left-margin)
				       (not (looking-at parsep)))
				(looking-at fill-prefix-regexp))
		      (unless (= (point) start)
			(setq multiple-lines t))
		      (forward-line -1))
		    (move-to-left-margin)
		    ;; This deleted code caused a long hanging-indent line
		    ;; not to be filled together with the following lines.
		    ;; ;; Don't move back over a line before the paragraph
		    ;; ;; which doesn't start with fill-prefix
		    ;; ;; unless that is the only line we've moved over.
		    ;; (and (not (looking-at fill-prefix-regexp))
		    ;;      multiple-lines
		    ;;      (forward-line 1))
		    (not (bobp)))
		(while (and (re-search-backward sp-parstart nil 1)
			    (setq found-start t)
			    ;; Found a candidate, but need to check if it is a
			    ;; REAL parstart.
			    (progn (setq start (point))
				   (move-to-left-margin)
				   (not (looking-at parsep)))
			    (not (and (looking-at parstart)
				      (or (not use-hard-newlines)
					  (bobp)
					  (get-text-property
					   (1- start) 'hard)))))
		  (setq found-start nil)
		  (goto-char start))
		found-start)
	      ;; Found one.
	      (progn
		;; Move forward over paragraph separators.
		;; We know this cannot reach the place we started
		;; because we know we moved back over a non-separator.
		(while (and (not (eobp))
			    (progn (move-to-left-margin)
				   (looking-at parsep)))
		  (forward-line 1))
		;; If line before paragraph is just margin, back up to there.
		(end-of-line 0)
		(if (> (current-column) (current-left-margin))
		    (forward-char 1)
		  (skip-chars-backward " \t")
		  (if (not (bolp))
		      (forward-line 1))))
	    ;; No starter or separator line => use buffer beg.
	    (goto-char (point-min))))))

    (while (and (> arg 0) (not (eobp)))
      ;; Move forward over separator lines...
      (while (and (not (eobp))
		  (progn (move-to-left-margin) (not (eobp)))
		  (looking-at parsep))
	(forward-line 1))
      (unless (eobp) (setq arg (1- arg)))
      ;; ... and one more line.
      (forward-line 1)
      (if fill-prefix-regexp
	  ;; There is a fill prefix; it overrides parstart.
	  (while (and (not (eobp))
		      (progn (move-to-left-margin) (not (eobp)))
		      (not (looking-at parsep))
		      (looking-at fill-prefix-regexp))
	    (forward-line 1))
	(while (and (re-search-forward sp-parstart nil 1)
		    (progn (setq start (match-beginning 0))
			   (goto-char start)
			   (not (eobp)))
		    (progn (move-to-left-margin)
			   (not (looking-at parsep)))
		    (or (not (looking-at parstart))
			(and use-hard-newlines
			     (not (get-text-property (1- start) 'hard)))))
	  (forward-char 1))
	(if (< (point) (point-max))
	    (goto-char start))))
    (constrain-to-field nil opoint t)
    ;; Return the number of steps that could not be done.
    arg))

(defun backward-paragraph (&optional arg)
  "Move backward to start of paragraph.
With argument ARG, do it ARG times;
a negative argument ARG = -N means move forward N paragraphs.

A paragraph start is the beginning of a line which is a
`paragraph-start' or which is ordinary text and follows a
`paragraph-separate'ing line; except: if the first real line of a
paragraph is preceded by a blank line, the paragraph starts at that
blank line.

See `forward-paragraph' for more information."
  (interactive "^p")
  (or arg (setq arg 1))
  (forward-paragraph (- arg)))

(defun mark-paragraph (&optional arg allow-extend)
  "Put point at beginning of this paragraph, mark at end.
The paragraph marked is the one that contains point or follows point.

With argument ARG, puts mark at end of a following paragraph, so that
the number of paragraphs marked equals ARG.

If ARG is negative, point is put at end of this paragraph, mark is put
at beginning of this or a previous paragraph.

Interactively, if this command is repeated
or (in Transient Mark mode) if the mark is active,
it marks the next ARG paragraphs after the ones already marked."
  (interactive "p\np")
  (unless arg (setq arg 1))
  (when (zerop arg)
    (error "Cannot mark zero paragraphs"))
  (cond ((and allow-extend
	      (or (and (eq last-command this-command) (mark t))
		  (and transient-mark-mode mark-active)))
	 (set-mark
	  (save-excursion
	    (goto-char (mark))
	    (forward-paragraph arg)
	    (point))))
	(t
	 (forward-paragraph arg)
	 (push-mark nil t t)
	 (backward-paragraph arg))))

(defun kill-paragraph (arg)
  "Kill forward to end of paragraph.
With arg N, kill forward to Nth end of paragraph;
negative arg -N means kill backward to Nth start of paragraph."
  (interactive "p")
  (kill-region (point) (progn (forward-paragraph arg) (point))))

(defun backward-kill-paragraph (arg)
  "Kill back to start of paragraph.
With arg N, kill back to Nth start of paragraph;
negative arg -N means kill forward to Nth end of paragraph."
  (interactive "p")
  (kill-region (point) (progn (backward-paragraph arg) (point))))

(defun transpose-paragraphs (arg)
  "Interchange the current paragraph with the next one.
With prefix argument ARG a non-zero integer, moves the current
paragraph past ARG paragraphs, leaving point after the current paragraph.
If ARG is positive, moves the current paragraph forwards, if
ARG is negative moves it backwards.  If ARG is zero, exchanges
the current paragraph with the one containing the mark."
  (interactive "*p")
  (transpose-subr 'forward-paragraph arg))

(defun start-of-paragraph-text ()
  (let ((opoint (point)) npoint)
    (forward-paragraph -1)
    (setq npoint (point))
    (skip-chars-forward " \t\n")
    ;; If the range of blank lines found spans the original start point,
    ;; try again from the beginning of it.
    ;; Must be careful to avoid infinite loop
    ;; when following a single return at start of buffer.
    (if (and (>= (point) opoint) (< npoint opoint))
	(progn
	  (goto-char npoint)
	  (if (> npoint (point-min))
	      (start-of-paragraph-text))))))

(defun end-of-paragraph-text ()
  (let ((opoint (point)))
    (forward-paragraph 1)
    (if (eq (preceding-char) ?\n) (forward-char -1))
    (if (<= (point) opoint)
	(progn
	  (forward-char 1)
	  (if (< (point) (point-max))
	      (end-of-paragraph-text))))))

(defun forward-sentence (&optional arg)
  "Move forward to next end of sentence.  With argument, repeat.
With negative argument, move backward repeatedly to start of sentence.

The variable `sentence-end' is a regular expression that matches ends of
sentences.  Also, every paragraph boundary terminates sentences as well."
  (interactive "^p")
  (or arg (setq arg 1))
  (let ((opoint (point))
        (sentence-end (sentence-end)))
    (while (< arg 0)
      (let ((pos (point))
	    par-beg par-text-beg)
	(save-excursion
	  (start-of-paragraph-text)
	  ;; Start of real text in the paragraph.
	  ;; We move back to here if we don't see a sentence-end.
	  (setq par-text-beg (point))
	  ;; Start of the first line of the paragraph.
	  ;; We use this as the search limit
	  ;; to allow s1entence-end to match if it is anchored at
	  ;; BOL and the paragraph starts indented.
	  (beginning-of-line)
	  (setq par-beg (point)))
	(if (and (re-search-backward sentence-end par-beg t)
		 (or (< (match-end 0) pos)
		     (re-search-backward sentence-end par-beg t)))
	    (goto-char (match-end 0))
	  (goto-char par-text-beg)))
      (setq arg (1+ arg)))
    (while (> arg 0)
      (let ((par-end (save-excursion (end-of-paragraph-text) (point))))
	(if (re-search-forward sentence-end par-end t)
	    (skip-chars-backward " \t\n")
	  (goto-char par-end)))
      (setq arg (1- arg)))
    (constrain-to-field nil opoint t)))

(defun repunctuate-sentences ()
  "Put two spaces at the end of sentences from point to the end of buffer.
It works using `query-replace-regexp'."
  (interactive)
  (query-replace-regexp "\\([]\"')]?\\)\\([.?!]\\)\\([]\"')]?\\) +"
			"\\1\\2\\3  "))


(defun backward-sentence (&optional arg)
  "Move backward to start of sentence.  With arg, do it arg times.
See `forward-sentence' for more information."
  (interactive "^p")
  (or arg (setq arg 1))
  (forward-sentence (- arg)))

(defun kill-sentence (&optional arg)
  "Kill from point to end of sentence.
With arg, repeat; negative arg -N means kill back to Nth start of sentence."
  (interactive "p")
  (kill-region (point) (progn (forward-sentence arg) (point))))

(defun backward-kill-sentence (&optional arg)
  "Kill back from point to start of sentence.
With arg, repeat, or kill forward to Nth end of sentence if negative arg -N."
  (interactive "p")
  (kill-region (point) (progn (backward-sentence arg) (point))))

(defun mark-end-of-sentence (arg)
  "Put mark at end of sentence.  Arg works as in `forward-sentence'.
If this command is repeated, it marks the next ARG sentences after the
ones already marked."
  (interactive "p")
  (push-mark
   (save-excursion
     (if (and (eq last-command this-command) (mark t))
	 (goto-char (mark)))
     (forward-sentence arg)
     (point))
   nil t))

(defun transpose-sentences (arg)
  "Interchange the current sentence with the next one.
With prefix argument ARG a non-zero integer, moves the current
sentence past ARG sentences, leaving point after the current sentence.
If ARG is positive, moves the current sentence forwards, if
ARG is negative moves it backwards.  If ARG is zero, exchanges
the current sentence with the one containing the mark."
  (interactive "*p")
  (transpose-subr 'forward-sentence arg))

;; Local Variables:
;; coding: utf-8
;; End:

;;; paragraphs.el ends here
