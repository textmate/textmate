;;; newcomment.el --- (un)comment regions of buffers -*- lexical-binding: t -*-

;; Copyright (C) 1999-2012 Free Software Foundation, Inc.

;; Author: code extracted from Emacs-20's simple.el
;; Maintainer: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: comment uncomment
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

;; A replacement for simple.el's comment-related functions.

;;; Bugs:

;; - boxed comments in Perl are not properly uncommented because they are
;;   uncommented one-line at a time.
;; - nested comments in sgml-mode are not properly quoted.
;; - single-char nestable comment-start can only do the "\\s<+" stuff
;;   if the corresponding closing marker happens to be right.
;; - uncomment-region with a numeric argument can render multichar
;;   comment markers invalid.
;; - comment-indent or comment-region when called inside a comment
;;   will happily break the surrounding comment.
;; - comment-quote-nested will not (un)quote properly all nested comment
;;   markers if there are more than just comment-start and comment-end.
;;   For example, in Pascal where {...*) and (*...} are possible.

;;; Todo:

;; - rebox.el-style refill.
;; - quantized steps in comment-alignment.
;; - try to align tail comments.
;; - check what c-comment-line-break-function has to say.
;; - spill auto-fill of comments onto the end of the next line.
;; - uncomment-region with a consp (for blocks) or somehow make the
;;   deletion of continuation markers less dangerous.
;; - drop block-comment-<foo> unless it's really used.
;; - uncomment-region on a subpart of a comment.
;; - support gnu-style "multi-line with space in continue".
;; - somehow allow comment-dwim to use the region even if transient-mark-mode
;;   is not turned on.

;; - when auto-filling a comment, try to move the comment to the left
;;   rather than break it (if possible).
;; - sometimes default the comment-column to the same
;;   one used on the preceding line(s).

;;; Code:

;;;###autoload
(defalias 'indent-for-comment 'comment-indent)
;;;###autoload
(defalias 'set-comment-column 'comment-set-column)
;;;###autoload
(defalias 'kill-comment 'comment-kill)
;;;###autoload
(defalias 'indent-new-comment-line 'comment-indent-new-line)

(defgroup comment nil
  "Indenting and filling of comments."
  :prefix "comment-"
  :version "21.1"
  :group 'fill)

;; Autoload this to avoid warnings, since some major modes define it.
;;;###autoload
(defvar comment-use-syntax 'undecided
  "Non-nil if syntax-tables can be used instead of regexps.
Can also be `undecided' which means that a somewhat expensive test will
be used to try to determine whether syntax-tables should be trusted
to understand comments or not in the given buffer.
Major modes should set this variable.")

(defcustom comment-fill-column nil
  "Column to use for `comment-indent'.  If nil, use `fill-column' instead."
  :type '(choice (const nil) integer)
  :group 'comment)

;;;###autoload
(defcustom comment-column 32
  "Column to indent right-margin comments to.
Each mode may establish a different default value for this variable; you
can set the value for a particular mode using that mode's hook.
Comments might be indented to a different value in order not to go beyond
`comment-fill-column' or in order to align them with surrounding comments."
  :type 'integer
  :group 'comment)
(make-variable-buffer-local 'comment-column)
;;;###autoload(put 'comment-column 'safe-local-variable 'integerp)

;;;###autoload
(defvar comment-start nil
  "*String to insert to start a new comment, or nil if no comment syntax.")
;;;###autoload(put 'comment-start 'safe-local-variable 'string-or-null-p)

;;;###autoload
(defvar comment-start-skip nil
  "*Regexp to match the start of a comment plus everything up to its body.
If there are any \\(...\\) pairs, the comment delimiter text is held to begin
at the place matched by the close of the first pair.")
;;;###autoload(put 'comment-start-skip 'safe-local-variable 'string-or-null-p)

;;;###autoload
(defvar comment-end-skip nil
  "Regexp to match the end of a comment plus everything back to its body.")
;;;###autoload(put 'comment-end-skip 'safe-local-variable 'string-or-null-p)

;;;###autoload
(defvar comment-end (purecopy "")
  "*String to insert to end a new comment.
Should be an empty string if comments are terminated by end-of-line.")
;;;###autoload(put 'comment-end 'safe-local-variable 'string-or-null-p)

;;;###autoload
(defvar comment-indent-function 'comment-indent-default
  "Function to compute desired indentation for a comment.
This function is called with no args with point at the beginning of
the comment's starting delimiter and should return either the desired
column indentation or nil.
If nil is returned, indentation is delegated to `indent-according-to-mode'.")

;;;###autoload
(defvar comment-insert-comment-function nil
  "Function to insert a comment when a line doesn't contain one.
The function has no args.

Applicable at least in modes for languages like fixed-format Fortran where
comments always start in column zero.")

(defvar comment-region-function 'comment-region-default
  "Function to comment a region.
Its args are the same as those of `comment-region', but BEG and END are
guaranteed to be correctly ordered.  It is called within `save-excursion'.

Applicable at least in modes for languages like fixed-format Fortran where
comments always start in column zero.")

(defvar uncomment-region-function 'uncomment-region-default
  "Function to uncomment a region.
Its args are the same as those of `uncomment-region', but BEG and END are
guaranteed to be correctly ordered.  It is called within `save-excursion'.

Applicable at least in modes for languages like fixed-format Fortran where
comments always start in column zero.")

;; ?? never set
(defvar block-comment-start nil)
(defvar block-comment-end nil)

(defvar comment-quote-nested t
  "Non-nil if nested comments should be quoted.
This should be locally set by each major mode if needed.")

(defvar comment-continue nil
  "Continuation string to insert for multiline comments.
This string will be added at the beginning of each line except the very
first one when commenting a region with a commenting style that allows
comments to span several lines.
It should generally have the same length as `comment-start' in order to
preserve indentation.
If it is nil a value will be automatically derived from `comment-start'
by replacing its first character with a space.")

(defvar comment-add 0
  "How many more comment chars should be inserted by `comment-region'.
This determines the default value of the numeric argument of `comment-region'.
The `plain' comment style doubles this value.

This should generally stay 0, except for a few modes like Lisp where
it is 1 so that regions are commented with two or three semi-colons.")

;;;###autoload
(defconst comment-styles
  '((plain      nil nil nil nil
                "Start in column 0 (do not indent), as in Emacs-20")
    (indent-or-triple nil nil nil multi-char
              "Start in column 0, but only for single-char starters")
    (indent     nil nil nil t
                "Full comment per line, ends not aligned")
    (aligned	nil t   nil t
                "Full comment per line, ends aligned")
    (box	nil t   t   t
                "Full comment per line, ends aligned, + top and bottom")
    (extra-line	t   nil t   t
                "One comment for all lines, end on a line by itself")
    (multi-line	t   nil nil t
                "One comment for all lines, end on last commented line")
    (box-multi	t   t   t   t
                "One comment for all lines, + top and bottom"))
  "Comment region style definitions.
Each style is defined with a form (STYLE . (MULTI ALIGN EXTRA INDENT DOC)).
DOC should succinctly describe the style.
STYLE should be a mnemonic symbol.
MULTI specifies that comments are allowed to span multiple lines.
  e.g. in C it comments regions as
     /* blabla
      * bli */
  rather than
     /* blabla */
     /* bli */
  if `comment-end' is empty, this has no effect.

ALIGN specifies that the `comment-end' markers should be aligned.
  e.g. in C it comments regions as
     /* blabla */
     /* bli    */
  rather than
     /* blabla */
     /* bli */
  if `comment-end' is empty, this has no effect, unless EXTRA is also set,
  in which case the comment gets wrapped in a box.

EXTRA specifies that an extra line should be used before and after the
  region to comment (to put the `comment-end' and `comment-start').
  e.g. in C it comments regions as
     /*
      * blabla
      * bli
      */
  rather than
     /* blabla
      * bli */
  if the comment style is not multi line, this has no effect, unless ALIGN
  is also set, in which case the comment gets wrapped in a box.

INDENT specifies that the `comment-start' markers should not be put at the
  left margin but at the current indentation of the region to comment.
If INDENT is `multi-char', that means indent multi-character
  comment starters, but not one-character comment starters.")

;;;###autoload
(defcustom comment-style 'indent
  "Style to be used for `comment-region'.
See `comment-styles' for a list of available styles."
  :type (if (boundp 'comment-styles)
	    `(choice
              ,@(mapcar (lambda (s)
                          `(const :tag ,(format "%s: %s" (car s) (nth 5 s))
                                  ,(car s)))
                        comment-styles))
	  'symbol)
  :version "23.1"
  :group 'comment)

;;;###autoload
(defcustom comment-padding (purecopy " ")
  "Padding string that `comment-region' puts between comment chars and text.
Can also be an integer which will be automatically turned into a string
of the corresponding number of spaces.

Extra spacing between the comment characters and the comment text
makes the comment easier to read.  Default is 1.  nil means 0."
  :type '(choice string integer (const nil))
  :group 'comment)

;;;###autoload
(defcustom comment-multi-line nil
  "Non-nil means `comment-indent-new-line' continues comments.
That is, it inserts no new terminator or starter.
This affects `auto-fill-mode', which is the main reason to
customize this variable.

It also affects \\[indent-new-comment-line].  However, if you want this
behavior for explicit filling, you might as well use \\[newline-and-indent]."
  :type 'boolean
  :group 'comment)

(defcustom comment-empty-lines nil
  "If nil, `comment-region' does not comment out empty lines.
If t, it always comments out empty lines.
If `eol' it only comments out empty lines if comments are
terminated by the end of line (i.e. `comment-end' is empty)."
  :type '(choice (const :tag "Never" nil)
	  (const :tag "Always" t)
	  (const :tag "EOl-terminated" 'eol))
  :group 'comment)

;;;;
;;;; Helpers
;;;;

(defun comment-string-strip (str beforep afterp)
  "Strip STR of any leading (if BEFOREP) and/or trailing (if AFTERP) space."
  (string-match (concat "\\`" (if beforep "\\s-*")
			"\\(.*?\\)" (if afterp "\\s-*\n?")
			"\\'") str)
  (match-string 1 str))

(defun comment-string-reverse (s)
  "Return the mirror image of string S, without any trailing space."
  (comment-string-strip (concat (nreverse (string-to-list s))) nil t))

;;;###autoload
(defun comment-normalize-vars (&optional noerror)
  "Check and setup the variables needed by other commenting functions.
Functions autoloaded from newcomment.el, being entry points, should call
this function before any other, so the rest of the code can assume that
the variables are properly set."
  (unless (and (not comment-start) noerror)
    (unless comment-start
      (let ((cs (read-string "No comment syntax is defined.  Use: ")))
	(if (zerop (length cs))
	    (error "No comment syntax defined")
	  (set (make-local-variable 'comment-start) cs)
	  (set (make-local-variable 'comment-start-skip) cs))))
    ;; comment-use-syntax
    (when (eq comment-use-syntax 'undecided)
      (set (make-local-variable 'comment-use-syntax)
	   (let ((st (syntax-table))
		 (cs comment-start)
		 (ce (if (string= "" comment-end) "\n" comment-end)))
	     ;; Try to skip over a comment using forward-comment
	     ;; to see if the syntax tables properly recognize it.
	     (with-temp-buffer
	       (set-syntax-table st)
	       (insert cs " hello " ce)
	       (goto-char (point-min))
	       (and (forward-comment 1) (eobp))))))
    ;; comment-padding
    (unless comment-padding (setq comment-padding 0))
    (when (integerp comment-padding)
      (setq comment-padding (make-string comment-padding ? )))
    ;; comment markers
    ;;(setq comment-start (comment-string-strip comment-start t nil))
    ;;(setq comment-end (comment-string-strip comment-end nil t))
    ;; comment-continue
    (unless (or comment-continue (string= comment-end ""))
      (set (make-local-variable 'comment-continue)
	   (concat (if (string-match "\\S-\\S-" comment-start) " " "|")
		   (substring comment-start 1)))
      ;; Hasn't been necessary yet.
      ;; (unless (string-match comment-start-skip comment-continue)
      ;;	(kill-local-variable 'comment-continue))
      )
    ;; comment-skip regexps
    (unless (and comment-start-skip
		 ;; In case comment-start has changed since last time.
		 (string-match comment-start-skip comment-start))
      (set (make-local-variable 'comment-start-skip)
	   (concat "\\(\\(^\\|[^\\\n]\\)\\(\\\\\\\\\\)*\\)\\(\\s<+\\|"
		   (regexp-quote (comment-string-strip comment-start t t))
		   ;; Let's not allow any \s- but only [ \t] since \n
		   ;; might be both a comment-end marker and \s-.
		   "+\\)[ \t]*")))
    (unless (and comment-end-skip
		 ;; In case comment-end has changed since last time.
		 (string-match comment-end-skip
                               (if (string= "" comment-end) "\n" comment-end)))
      (let ((ce (if (string= "" comment-end) "\n"
		  (comment-string-strip comment-end t t))))
	(set (make-local-variable 'comment-end-skip)
	     ;; We use [ \t] rather than \s- because we don't want to
	     ;; remove ^L in C mode when uncommenting.
	     (concat "[ \t]*\\(\\s>" (if comment-quote-nested "" "+")
		     "\\|" (regexp-quote (substring ce 0 1))
		     (if (and comment-quote-nested (<= (length ce) 1)) "" "+")
		     (regexp-quote (substring ce 1))
		     "\\)"))))))

(defun comment-quote-re (str unp)
  (concat (regexp-quote (substring str 0 1))
	  "\\\\" (if unp "+" "*")
	  (regexp-quote (substring str 1))))

(defun comment-quote-nested (cs ce unp)
  "Quote or unquote nested comments.
If UNP is non-nil, unquote nested comment markers."
  (setq cs (comment-string-strip cs t t))
  (setq ce (comment-string-strip ce t t))
  (when (and comment-quote-nested (> (length ce) 0))
    (let ((re (concat (comment-quote-re ce unp)
		      "\\|" (comment-quote-re cs unp))))
      (goto-char (point-min))
      (while (re-search-forward re nil t)
	(goto-char (match-beginning 0))
	(forward-char 1)
	(if unp (delete-char 1) (insert "\\"))
	(when (= (length ce) 1)
	  ;; If the comment-end is a single char, adding a \ after that
	  ;; "first" char won't deactivate it, so we turn such a CE
	  ;; into !CS.  I.e. for pascal, we turn } into !{
	  (if (not unp)
	      (when (string= (match-string 0) ce)
		(replace-match (concat "!" cs) t t))
	    (when (and (< (point-min) (match-beginning 0))
		       (string= (buffer-substring (1- (match-beginning 0))
						  (1- (match-end 0)))
				(concat "!" cs)))
	      (backward-char 2)
	      (delete-char (- (match-end 0) (match-beginning 0)))
	      (insert ce))))))))

;;;;
;;;; Navigation
;;;;

(defvar comment-use-global-state nil
  "Non-nil means that the global syntactic context is used.
More specifically, it means that `syntax-ppss' is used to find out whether
point is within a string or not.  Major modes whose syntax is faithfully
described by the syntax-tables can set this to non-nil so comment markers
in strings will not confuse Emacs.")

(defun comment-search-forward (limit &optional noerror)
  "Find a comment start between point and LIMIT.
Moves point to inside the comment and returns the position of the
comment-starter.  If no comment is found, moves point to LIMIT
and raises an error or returns nil if NOERROR is non-nil."
  (if (not comment-use-syntax)
      (if (re-search-forward comment-start-skip limit noerror)
	  (or (match-end 1) (match-beginning 0))
	(goto-char limit)
	(unless noerror (error "No comment")))
    (let* ((pt (point))
	   ;; Assume (at first) that pt is outside of any string.
	   (s (parse-partial-sexp pt (or limit (point-max)) nil nil
				  (if comment-use-global-state (syntax-ppss pt))
				  t)))
      (when (and (nth 8 s) (nth 3 s) (not comment-use-global-state))
	;; The search ended at eol inside a string.  Try to see if it
	;; works better when we assume that pt is inside a string.
	(setq s (parse-partial-sexp
		 pt (or limit (point-max)) nil nil
		 (list nil nil nil (nth 3 s) nil nil nil nil)
		 t)))
      (if (or (not (and (nth 8 s) (not (nth 3 s))))
	      ;; Make sure the comment starts after PT.
	      (< (nth 8 s) pt))
	  (unless noerror (error "No comment"))
	;; We found the comment.
	(let ((pos (point))
	      (start (nth 8 s))
	      (bol (line-beginning-position))
	      (end nil))
	  (while (and (null end) (>= (point) bol))
	    (if (looking-at comment-start-skip)
		(setq end (min (or limit (point-max)) (match-end 0)))
	      (backward-char)))
	  (goto-char (or end pos))
	  start)))))

(defun comment-search-backward (&optional limit noerror)
  "Find a comment start between LIMIT and point.
Moves point to inside the comment and returns the position of the
comment-starter.  If no comment is found, moves point to LIMIT
and raises an error or returns nil if NOERROR is non-nil."
  ;; FIXME: If a comment-start appears inside a comment, we may erroneously
  ;; stop there.  This can be rather bad in general, but since
  ;; comment-search-backward is only used to find the comment-column (in
  ;; comment-set-column) and to find the comment-start string (via
  ;; comment-beginning) in indent-new-comment-line, it should be harmless.
  (if (not (re-search-backward comment-start-skip limit t))
      (unless noerror (error "No comment"))
    (beginning-of-line)
    (let* ((end (match-end 0))
	   (cs (comment-search-forward end t))
	   (pt (point)))
      (if (not cs)
	  (progn (beginning-of-line)
		 (comment-search-backward limit noerror))
	(while (progn (goto-char cs)
		      (comment-forward)
		      (and (< (point) end)
			   (setq cs (comment-search-forward end t))))
	  (setq pt (point)))
	(goto-char pt)
	cs))))

(defun comment-beginning ()
  "Find the beginning of the enclosing comment.
Returns nil if not inside a comment, else moves point and returns
the same as `comment-search-backward'."
  ;; HACK ATTACK!
  ;; We should really test `in-string-p' but that can be expensive.
  (unless (eq (get-text-property (point) 'face) 'font-lock-string-face)
    (let ((pt (point))
	  (cs (comment-search-backward nil t)))
      (when cs
	(if (save-excursion
	      (goto-char cs)
	      (and
	       ;; For modes where comment-start and comment-end are the same,
	       ;; the search above may have found a `ce' rather than a `cs'.
	       (or (if comment-end-skip (not (looking-at comment-end-skip)))
		   ;; Maybe font-lock knows that it's a `cs'?
		   (eq (get-text-property (match-end 0) 'face)
		       'font-lock-comment-face)
		   (unless (eq (get-text-property (point) 'face)
			       'font-lock-comment-face)
		     ;; Let's assume it's a `cs' if we're on the same line.
		     (>= (line-end-position) pt)))
	       ;; Make sure that PT is not past the end of the comment.
	       (if (comment-forward 1) (> (point) pt) (eobp))))
	    cs
	  (goto-char pt)
	  nil)))))

(defun comment-forward (&optional n)
  "Skip forward over N comments.
Just like `forward-comment' but only for positive N
and can use regexps instead of syntax."
  (setq n (or n 1))
  (if (< n 0) (error "No comment-backward")
    (if comment-use-syntax (forward-comment n)
      (while (> n 0)
	(setq n
	      (if (or (forward-comment 1)
		      (and (looking-at comment-start-skip)
			   (goto-char (match-end 0))
			   (re-search-forward comment-end-skip nil 'move)))
		  (1- n) -1)))
      (= n 0))))

(defun comment-enter-backward ()
  "Move from the end of a comment to the end of its content.
Point is assumed to be just at the end of a comment."
  (if (bolp)
      ;; comment-end = ""
      (progn (backward-char) (skip-syntax-backward " "))
    (cond
     ((save-excursion
        (save-restriction
          (narrow-to-region (line-beginning-position) (point))
          (goto-char (point-min))
          (re-search-forward (concat comment-end-skip "\\'") nil t)))
      (goto-char (match-beginning 0)))
     ;; comment-end-skip not found probably because it was not set
     ;; right.  Since \\s> should catch the single-char case, let's
     ;; check that we're looking at a two-char comment ender.
     ((not (or (<= (- (point-max) (line-beginning-position)) 1)
               (zerop (logand (car (syntax-after (- (point) 1)))
                              ;; Here we take advantage of the fact that
                              ;; the syntax class " " is encoded to 0,
                              ;; so "  4" gives us just the 4 bit.
                              (car (string-to-syntax "  4"))))
               (zerop (logand (car (syntax-after (- (point) 2)))
                              (car (string-to-syntax "  3"))))))
      (backward-char 2)
      (skip-chars-backward (string (char-after)))
      (skip-syntax-backward " "))
     ;; No clue what's going on: maybe we're really not right after the
     ;; end of a comment.  Maybe we're at the "end" because of EOB rather
     ;; than because of a marker.
     (t (skip-syntax-backward " ")))))

;;;;
;;;; Commands
;;;;

;;;###autoload
(defun comment-indent-default ()
  "Default for `comment-indent-function'."
  (if (and (looking-at "\\s<\\s<\\(\\s<\\)?")
	   (or (match-end 1) (/= (current-column) (current-indentation))))
      0
    (when (or (/= (current-column) (current-indentation))
	      (and (> comment-add 0) (looking-at "\\s<\\(\\S<\\|\\'\\)")))
      comment-column)))

(defun comment-choose-indent (&optional indent)
  "Choose the indentation to use for a right-hand-side comment.
The criteria are (in this order):
- try to keep the comment's text within `comment-fill-column'.
- try to align with surrounding comments.
- prefer INDENT (or `comment-column' if nil).
Point is expected to be at the start of the comment."
  (unless indent (setq indent comment-column))
  ;; Avoid moving comments past the fill-column.
  (let ((max (+ (current-column)
                (- (or comment-fill-column fill-column)
                   (save-excursion (end-of-line) (current-column)))))
        (other nil)
        (min (save-excursion (skip-chars-backward " \t")
                             (if (bolp) 0 (1+ (current-column))))))
    ;; Fix up the range.
    (if (< max min) (setq max min))
    ;; Don't move past the fill column.
    (if (<= max indent) (setq indent max))
    ;; We can choose anywhere between min..max.
    ;; Let's try to align to a comment on the previous line.
    (save-excursion
      (when (and (zerop (forward-line -1))
                 (setq other (comment-search-forward
                              (line-end-position) t)))
        (goto-char other) (setq other (current-column))))
    (if (and other (<= other max) (>= other min))
        ;; There is a comment and it's in the range: bingo!
        other
      ;; Can't align to a previous comment: let's try to align to comments
      ;; on the following lines, then.  These have not been re-indented yet,
      ;; so we can't directly align ourselves with them.  All we do is to try
      ;; and choose an indentation point with which they will be able to
      ;; align themselves.
      (save-excursion
        (while (and (zerop (forward-line 1))
                    (setq other (comment-search-forward
                                 (line-end-position) t)))
          (goto-char other)
          (let ((omax (+ (current-column)
                         (- (or comment-fill-column fill-column)
                            (save-excursion (end-of-line) (current-column)))))
                (omin (save-excursion (skip-chars-backward " \t")
                                      (1+ (current-column)))))
            (if (and (>= omax min) (<= omin max))
                (progn (setq min (max omin min))
                       (setq max (min omax max)))
              ;; Can't align with this anyway, so exit the loop.
              (goto-char (point-max))))))
      ;; Return the closest point to indent within min..max.
      (max min (min max indent)))))

;;;###autoload
(defun comment-indent (&optional continue)
  "Indent this line's comment to `comment-column', or insert an empty comment.
If CONTINUE is non-nil, use the `comment-continue' markers if any."
  (interactive "*")
  (comment-normalize-vars)
  (let* ((empty (save-excursion (beginning-of-line)
				(looking-at "[ \t]*$")))
	 (starter (or (and continue comment-continue)
		      (and empty block-comment-start) comment-start))
	 (ender (or (and continue comment-continue "")
		    (and empty block-comment-end) comment-end)))
    (unless starter (error "No comment syntax defined"))
    (beginning-of-line)
    (let* ((eolpos (line-end-position))
	   (begpos (comment-search-forward eolpos t))
	   cpos indent)
      (if (and comment-insert-comment-function (not begpos))
	  ;; If no comment and c-i-c-f is set, let it do everything.
	  (funcall comment-insert-comment-function)
	;; An existing comment?
	(if begpos
	    (progn
	      (if (and (not (looking-at "[\t\n ]"))
		       (looking-at comment-end-skip))
		  ;; The comment is empty and we have skipped all its space
		  ;; and landed right before the comment-ender:
		  ;; Go back to the middle of the space.
		  (forward-char (/ (skip-chars-backward " \t") -2)))
	      (setq cpos (point-marker)))
	  ;; If none, insert one.
	  (save-excursion
	    ;; Some `comment-indent-function's insist on not moving
	    ;; comments that are in column 0, so we first go to the
	    ;; likely target column.
	    (indent-to comment-column)
	    ;; Ensure there's a space before the comment for things
	    ;; like sh where it matters (as well as being neater).
	    (unless (memq (char-before) '(nil ?\n ?\t ?\s))
	      (insert ?\s))
	    (setq begpos (point))
	    (insert starter)
	    (setq cpos (point-marker))
	    (insert ender)))
	(goto-char begpos)
	;; Compute desired indent.
	(setq indent (save-excursion (funcall comment-indent-function)))
	;; If `indent' is nil and there's code before the comment, we can't
	;; use `indent-according-to-mode', so we default to comment-column.
	(unless (or indent (save-excursion (skip-chars-backward " \t") (bolp)))
	  (setq indent comment-column))
	(if (not indent)
	    ;; comment-indent-function refuses: delegate to line-indent.
	    (indent-according-to-mode)
	  ;; If the comment is at the right of code, adjust the indentation.
	  (unless (save-excursion (skip-chars-backward " \t") (bolp))
	    (setq indent (comment-choose-indent indent)))
	  ;; Update INDENT to leave at least one space
	  ;; after other nonwhite text on the line.
	  (save-excursion
	    (skip-chars-backward " \t")
	    (unless (bolp)
	      (setq indent (max indent (1+ (current-column))))))
	  ;; If that's different from comment's current position, change it.
	  (unless (= (current-column) indent)
	    (delete-region (point) (progn (skip-chars-backward " \t") (point)))
	    (indent-to indent)))
	(goto-char cpos)
	(set-marker cpos nil)))))

;;;###autoload
(defun comment-set-column (arg)
  "Set the comment column based on point.
With no ARG, set the comment column to the current column.
With just minus as arg, kill any comment on this line.
With any other arg, set comment column to indentation of the previous comment
 and then align or create a comment on this line at that column."
  (interactive "P")
  (cond
   ((eq arg '-) (comment-kill nil))
   (arg
    (comment-normalize-vars)
    (save-excursion
      (beginning-of-line)
      (comment-search-backward)
      (beginning-of-line)
      (goto-char (comment-search-forward (line-end-position)))
      (setq comment-column (current-column))
      (message "Comment column set to %d" comment-column))
    (comment-indent))
   (t (setq comment-column (current-column))
      (message "Comment column set to %d" comment-column))))

;;;###autoload
(defun comment-kill (arg)
  "Kill the first comment on this line, if any.
With prefix ARG, kill comments on that many lines starting with this one."
  (interactive "P")
  (comment-normalize-vars)
  (dotimes (_i (prefix-numeric-value arg))
    (save-excursion
      (beginning-of-line)
      (let ((cs (comment-search-forward (line-end-position) t)))
	(when cs
	  (goto-char cs)
	  (skip-syntax-backward " ")
	  (setq cs (point))
	  (comment-forward)
	  (kill-region cs (if (bolp) (1- (point)) (point)))
	  (indent-according-to-mode))))
    (if arg (forward-line 1))))

(defun comment-padright (str &optional n)
  "Construct a string composed of STR plus `comment-padding'.
It also adds N copies of the last non-whitespace chars of STR.
If STR already contains padding, the corresponding amount is
ignored from `comment-padding'.
N defaults to 0.
If N is `re', a regexp is returned instead, that would match
the string for any N."
  (setq n (or n 0))
  (when (and (stringp str) (not (string= "" str)))
    ;; Separate the actual string from any leading/trailing padding
    (string-match "\\`\\s-*\\(.*?\\)\\s-*\\'" str)
    (let ((s (match-string 1 str))	;actual string
	  (lpad (substring str 0 (match-beginning 1))) ;left padding
	  (rpad (concat (substring str (match-end 1)) ;original right padding
			(substring comment-padding ;additional right padding
				   (min (- (match-end 0) (match-end 1))
					(length comment-padding)))))
	  ;; We can only duplicate C if the comment-end has multiple chars
	  ;; or if comments can be nested, else the comment-end `}' would
	  ;; be turned into `}}}' where only the first ends the comment
	  ;; and the rest becomes bogus junk.
	  (multi (not (and comment-quote-nested
			   ;; comment-end is a single char
			   (string-match "\\`\\s-*\\S-\\s-*\\'" comment-end)))))
      (if (not (symbolp n))
	  (concat lpad s (when multi (make-string n (aref str (1- (match-end 1))))) rpad)
	;; construct a regexp that would match anything from just S
	;; to any possible output of this function for any N.
	(concat (mapconcat (lambda (c) (concat (regexp-quote (string c)) "?"))
			   lpad "")	;padding is not required
		(regexp-quote s)
		(when multi "+")	;the last char of S might be repeated
		(mapconcat (lambda (c) (concat (regexp-quote (string c)) "?"))
			   rpad "")))))) ;padding is not required

(defun comment-padleft (str &optional n)
  "Construct a string composed of `comment-padding' plus STR.
It also adds N copies of the first non-whitespace chars of STR.
If STR already contains padding, the corresponding amount is
ignored from `comment-padding'.
N defaults to 0.
If N is `re', a regexp is returned instead, that would match
  the string for any N."
  (setq n (or n 0))
  (when (and (stringp str) (not (string= "" str)))
    ;; Only separate the left pad because we assume there is no right pad.
    (string-match "\\`\\s-*" str)
    (let ((s (substring str (match-end 0)))
	  (pad (concat (substring comment-padding
				  (min (- (match-end 0) (match-beginning 0))
				       (length comment-padding)))
		       (match-string 0 str)))
	  (c (aref str (match-end 0)))	;the first non-space char of STR
	  ;; We can only duplicate C if the comment-end has multiple chars
	  ;; or if comments can be nested, else the comment-end `}' would
	  ;; be turned into `}}}' where only the first ends the comment
	  ;; and the rest becomes bogus junk.
	  (multi (not (and comment-quote-nested
			   ;; comment-end is a single char
			   (string-match "\\`\\s-*\\S-\\s-*\\'" comment-end)))))
      (if (not (symbolp n))
	  (concat pad (when multi (make-string n c)) s)
	;; Construct a regexp that would match anything from just S
	;; to any possible output of this function for any N.
	;; We match any number of leading spaces because this regexp will
	;; be used for uncommenting where we might want to remove
	;; uncomment markers with arbitrary leading space (because
	;; they were aligned).
	(concat "\\s-*"
		(if multi (concat (regexp-quote (string c)) "*"))
		(regexp-quote s))))))

;;;###autoload
(defun uncomment-region (beg end &optional arg)
  "Uncomment each line in the BEG .. END region.
The numeric prefix ARG can specify a number of chars to remove from the
comment markers."
  (interactive "*r\nP")
  (comment-normalize-vars)
  (when (> beg end) (setq beg (prog1 end (setq end beg))))
  ;; Bind `comment-use-global-state' to nil.  While uncommenting a region
  ;; (which works a line at a time), a comment can appear to be
  ;; included in a mult-line string, but it is actually not.
  (let ((comment-use-global-state nil))
    (save-excursion
      (funcall uncomment-region-function beg end arg))))

(defun uncomment-region-default (beg end &optional arg)
  "Uncomment each line in the BEG .. END region.
The numeric prefix ARG can specify a number of chars to remove from the
comment markers."
  (goto-char beg)
  (setq end (copy-marker end))
  (let* ((numarg (prefix-numeric-value arg))
	 (ccs comment-continue)
	 (srei (comment-padright ccs 're))
	 (csre (comment-padright comment-start 're))
	 (sre (and srei (concat "^\\s-*?\\(" srei "\\)")))
	 spt)
    (while (and (< (point) end)
		(setq spt (comment-search-forward end t)))
      (let ((ipt (point))
	    ;; Find the end of the comment.
	    (ept (progn
		   (goto-char spt)
		   (unless (or (comment-forward)
			       ;; Allow non-terminated comments.
			       (eobp))
		     (error "Can't find the comment end"))
		   (point)))
	    (box nil)
	    (box-equal nil))	   ;Whether we might be using `=' for boxes.
	(save-restriction
	  (narrow-to-region spt ept)

	  ;; Remove the comment-start.
	  (goto-char ipt)
	  (skip-syntax-backward " ")
	  ;; A box-comment starts with a looong comment-start marker.
	  (when (and (or (and (= (- (point) (point-min)) 1)
			      (setq box-equal t)
			      (looking-at "=\\{7\\}")
			      (not (eq (char-before (point-max)) ?\n))
			      (skip-chars-forward "="))
			 (> (- (point) (point-min) (length comment-start)) 7))
		     (> (count-lines (point-min) (point-max)) 2))
	    (setq box t))
	  ;; Skip the padding.  Padding can come from comment-padding and/or
	  ;; from comment-start, so we first check comment-start.
	  (if (or (save-excursion (goto-char (point-min)) (looking-at csre))
		  (looking-at (regexp-quote comment-padding)))
	      (goto-char (match-end 0)))
	  (when (and sre (looking-at (concat "\\s-*\n\\s-*" srei)))
	    (goto-char (match-end 0)))
	  (if (null arg) (delete-region (point-min) (point))
            (let ((opoint (point-marker)))
              (skip-syntax-backward " ")
              (delete-char (- numarg))
              (unless (and (not (bobp))
                           (save-excursion (goto-char (point-min))
                                           (looking-at comment-start-skip)))
                ;; If there's something left but it doesn't look like
                ;; a comment-start any more, just remove it.
                (delete-region (point-min) opoint))))

	  ;; Remove the end-comment (and leading padding and such).
	  (goto-char (point-max)) (comment-enter-backward)
	  ;; Check for special `=' used sometimes in comment-box.
	  (when (and box-equal (not (eq (char-before (point-max)) ?\n)))
	    (let ((pos (point)))
	      ;; skip `=' but only if there are at least 7.
	      (when (> (skip-chars-backward "=") -7) (goto-char pos))))
	  (unless (looking-at "\\(\n\\|\\s-\\)*\\'")
	    (when (and (bolp) (not (bobp))) (backward-char))
	    (if (null arg) (delete-region (point) (point-max))
	      (skip-syntax-forward " ")
	      (delete-char numarg)
	      (unless (or (eobp) (looking-at comment-end-skip))
		;; If there's something left but it doesn't look like
		;; a comment-end any more, just remove it.
		(delete-region (point) (point-max)))))

	  ;; Unquote any nested end-comment.
	  (comment-quote-nested comment-start comment-end t)

	  ;; Eliminate continuation markers as well.
	  (when sre
	    (let* ((cce (comment-string-reverse (or comment-continue
						    comment-start)))
		   (erei (and box (comment-padleft cce 're)))
		   (ere (and erei (concat "\\(" erei "\\)\\s-*$"))))
	      (goto-char (point-min))
	      (while (progn
		       (if (and ere (re-search-forward
				     ere (line-end-position) t))
			   (replace-match "" t t nil (if (match-end 2) 2 1))
			 (setq ere nil))
		       (forward-line 1)
		       (re-search-forward sre (line-end-position) t))
		(replace-match "" t t nil (if (match-end 2) 2 1)))))
	  ;; Go to the end for the next comment.
	  (goto-char (point-max))))))
  (set-marker end nil))

(defun comment-make-extra-lines (cs ce ccs cce min-indent max-indent &optional block)
  "Make the leading and trailing extra lines.
This is used for `extra-line' style (or `box' style if BLOCK is specified)."
  (let ((eindent 0))
    (if (not block)
	;; Try to match CS and CE's content so they align aesthetically.
	(progn
	  (setq ce (comment-string-strip ce t t))
	  (when (string-match "\\(.+\\).*\n\\(.*?\\)\\1" (concat ce "\n" cs))
	    (setq eindent
		  (max (- (match-end 2) (match-beginning 2) (match-beginning 0))
		       0))))
      ;; box comment
      (let* ((width (- max-indent min-indent))
	     (s (concat cs "a=m" cce))
	     (e (concat ccs "a=m" ce))
	     (c (if (string-match ".*\\S-\\S-" cs)
		    (aref cs (1- (match-end 0)))
		  (if (and (equal comment-end "") (string-match ".*\\S-" cs))
		      (aref cs (1- (match-end 0))) ?=)))
	     (re "\\s-*a=m\\s-*")
	     (_ (string-match re s))
	     (lcs (length cs))
	     (fill
	      (make-string (+ width (- (match-end 0)
				       (match-beginning 0) lcs 3)) c)))
	(setq cs (replace-match fill t t s))
	(when (and (not (string-match comment-start-skip cs))
		   (string-match "a=m" s))
	  ;; The whitespace around CS cannot be ignored: put it back.
	  (setq re "a=m")
	  (setq fill (make-string (- width lcs) c))
	  (setq cs (replace-match fill t t s)))
	(string-match re e)
	(setq ce (replace-match fill t t e))))
    (cons (concat cs "\n" (make-string min-indent ? ) ccs)
	  (concat cce "\n" (make-string (+ min-indent eindent) ? ) ce))))

(defmacro comment-with-narrowing (beg end &rest body)
  "Execute BODY with BEG..END narrowing.
Space is added (and then removed) at the beginning for the text's
indentation to be kept as it was before narrowing."
  (declare (debug t) (indent 2))
  (let ((bindent (make-symbol "bindent")))
    `(let ((,bindent (save-excursion (goto-char ,beg) (current-column))))
       (save-restriction
	 (narrow-to-region ,beg ,end)
	 (goto-char (point-min))
	 (insert (make-string ,bindent ? ))
	 (prog1
	     (progn ,@body)
	   ;; remove the bindent
	   (save-excursion
	     (goto-char (point-min))
	     (when (looking-at " *")
	       (let ((n (min (- (match-end 0) (match-beginning 0)) ,bindent)))
		 (delete-char n)
		 (setq ,bindent (- ,bindent n))))
	     (end-of-line)
	     (let ((e (point)))
	       (beginning-of-line)
	       (while (and (> ,bindent 0) (re-search-forward "   *" e t))
		 (let ((n (min ,bindent (- (match-end 0) (match-beginning 0) 1))))
		   (goto-char (match-beginning 0))
		   (delete-char n)
		   (setq ,bindent (- ,bindent n)))))))))))

(defun comment-add (arg)
  "Compute the number of extra comment starter characters.
\(Extra semicolons in Lisp mode, extra stars in C mode, etc.)
If ARG is non-nil, just follow ARG.
If the comment starter is multi-char, just follow ARG.
Otherwise obey `comment-add'."
  (if (and (null arg) (= (string-match "[ \t]*\\'" comment-start) 1))
      (* comment-add 1)
    (1- (prefix-numeric-value arg))))

(defun comment-region-internal (beg end cs ce
                                &optional ccs cce block lines indent)
  "Comment region BEG .. END.
CS and CE are the comment start string and comment end string,
respectively.  CCS and CCE are the comment continuation strings
for the start and end of lines, respectively (default to CS and CE).
BLOCK indicates that end of lines should be marked with either CCE,
CE or CS \(if CE is empty) and that those markers should be aligned.
LINES indicates that an extra lines will be used at the beginning
and end of the region for CE and CS.
INDENT indicates to put CS and CCS at the current indentation of
the region rather than at left margin."
  ;;(assert (< beg end))
  (let ((no-empty (not (or (eq comment-empty-lines t)
			   (and comment-empty-lines (zerop (length ce))))))
	ce-sanitized)
    ;; Sanitize CE and CCE.
    (if (and (stringp ce) (string= "" ce)) (setq ce nil))
    (setq ce-sanitized ce)
    (if (and (stringp cce) (string= "" cce)) (setq cce nil))
    ;; If CE is empty, multiline cannot be used.
    (unless ce (setq ccs nil cce nil))
    ;; Should we mark empty lines as well ?
    (if (or ccs block lines) (setq no-empty nil))
    ;; Make sure we have end-markers for BLOCK mode.
    (when block (unless ce (setq ce (comment-string-reverse cs))))
    ;; If BLOCK is not requested, we don't need CCE.
    (unless block (setq cce nil))
    ;; Continuation defaults to the same as CS and CE.
    (unless ccs (setq ccs cs cce ce))

    (save-excursion
      (goto-char end)
      ;; If the end is not at the end of a line and the comment-end
      ;; is implicit (i.e. a newline), explicitly insert a newline.
      (unless (or ce-sanitized (eolp)) (insert "\n") (indent-according-to-mode))
      (comment-with-narrowing beg end
	(let ((min-indent (point-max))
	      (max-indent 0))
	  (goto-char (point-min))
	  ;; Quote any nested comment marker
	  (comment-quote-nested comment-start comment-end nil)

	  ;; Loop over all lines to find the needed indentations.
	  (goto-char (point-min))
	  (while
	      (progn
		(unless (looking-at "[ \t]*$")
		  (setq min-indent (min min-indent (current-indentation))))
		(end-of-line)
		(setq max-indent (max max-indent (current-column)))
		(not (or (eobp) (progn (forward-line) nil)))))

	  (setq max-indent
		(+ max-indent (max (length cs) (length ccs))
                   ;; Inserting ccs can change max-indent by (1- tab-width)
                   ;; but only if there are TABs in the boxed text, of course.
                   (if (save-excursion (goto-char beg)
                                       (search-forward "\t" end t))
                       (1- tab-width) 0)))
	  (unless indent (setq min-indent 0))

	  ;; make the leading and trailing lines if requested
	  (when lines
	    (let ((csce
		   (comment-make-extra-lines
		    cs ce ccs cce min-indent max-indent block)))
	      (setq cs (car csce))
	      (setq ce (cdr csce))))

	  (goto-char (point-min))
	  ;; Loop over all lines from BEG to END.
	  (while
	      (progn
		(unless (and no-empty (looking-at "[ \t]*$"))
		  (move-to-column min-indent t)
		  (insert cs) (setq cs ccs) ;switch to CCS after the first line
		  (end-of-line)
		  (if (eobp) (setq cce ce))
		  (when cce
		    (when block (move-to-column max-indent t))
		    (insert cce)))
		(end-of-line)
		(not (or (eobp) (progn (forward-line) nil))))))))))

;;;###autoload
(defun comment-region (beg end &optional arg)
  "Comment or uncomment each line in the region.
With just \\[universal-argument] prefix arg, uncomment each line in region BEG .. END.
Numeric prefix ARG means use ARG comment characters.
If ARG is negative, delete that many comment characters instead.

The strings used as comment starts are built from `comment-start'
and `comment-padding'; the strings used as comment ends are built
from `comment-end' and `comment-padding'.

By default, the `comment-start' markers are inserted at the
current indentation of the region, and comments are terminated on
each line (even for syntaxes in which newline does not end the
comment and blank lines do not get comments).  This can be
changed with `comment-style'."
  (interactive "*r\nP")
  (comment-normalize-vars)
  (if (> beg end) (let (mid) (setq mid beg beg end end mid)))
  (save-excursion
    ;; FIXME: maybe we should call uncomment depending on ARG.
    (funcall comment-region-function beg end arg)))

(defun comment-region-default (beg end &optional arg)
  (let* ((numarg (prefix-numeric-value arg))
	 (style (cdr (assoc comment-style comment-styles)))
	 (lines (nth 2 style))
	 (block (nth 1 style))
	 (multi (nth 0 style)))

    ;; We use `chars' instead of `syntax' because `\n' might be
    ;; of end-comment syntax rather than of whitespace syntax.
    ;; sanitize BEG and END
    (goto-char beg) (skip-chars-forward " \t\n\r") (beginning-of-line)
    (setq beg (max beg (point)))
    (goto-char end) (skip-chars-backward " \t\n\r") (end-of-line)
    (setq end (min end (point)))
    (if (>= beg end) (error "Nothing to comment"))

    ;; sanitize LINES
    (setq lines
	  (and
	   lines ;; multi
	   (progn (goto-char beg) (beginning-of-line)
		  (skip-syntax-forward " ")
		  (>= (point) beg))
	   (progn (goto-char end) (end-of-line) (skip-syntax-backward " ")
		  (<= (point) end))
	   (or block (not (string= "" comment-end)))
	   (or block (progn (goto-char beg) (search-forward "\n" end t)))))

    ;; don't add end-markers just because the user asked for `block'
    (unless (or lines (string= "" comment-end)) (setq block nil))

    (cond
     ((consp arg) (uncomment-region beg end))
     ((< numarg 0) (uncomment-region beg end (- numarg)))
     (t
      (let ((multi-char (/= (string-match "[ \t]*\\'" comment-start) 1))
	    indent triple)
	(if (eq (nth 3 style) 'multi-char)
	    (save-excursion
	      (goto-char beg)
	      (setq indent multi-char
		    ;; Triple if we will put the comment starter at the margin
		    ;; and the first line of the region isn't indented
		    ;; at least two spaces.
		    triple (and (not multi-char) (looking-at "\t\\|  "))))
	  (setq indent (nth 3 style)))

	;; In Lisp and similar modes with one-character comment starters,
	;; double it by default if `comment-add' says so.
	;; If it isn't indented, triple it.
	(if (and (null arg) (not multi-char))
	    (setq numarg (* comment-add (if triple 2 1)))
	  (setq numarg (1- (prefix-numeric-value arg))))

	(comment-region-internal
	 beg end
	 (let ((s (comment-padright comment-start numarg)))
	   (if (string-match comment-start-skip s) s
	     (comment-padright comment-start)))
	 (let ((s (comment-padleft comment-end numarg)))
	   (and s (if (string-match comment-end-skip s) s
		    (comment-padright comment-end))))
	 (if multi (comment-padright comment-continue numarg))
	 (if multi
	     (comment-padleft (comment-string-reverse comment-continue) numarg))
	 block
	 lines
	 indent))))))

;;;###autoload
(defun comment-box (beg end &optional arg)
  "Comment out the BEG .. END region, putting it inside a box.
The numeric prefix ARG specifies how many characters to add to begin- and
end- comment markers additionally to what `comment-add' already specifies."
  (interactive "*r\np")
  (comment-normalize-vars)
  (let ((comment-style (if (cadr (assoc comment-style comment-styles))
			   'box-multi 'box)))
    (comment-region beg end (+ comment-add arg))))

(defun comment-only-p (beg end)
  "Return non-nil if the text between BEG and END is all comments."
  (save-excursion
    (goto-char beg)
    (comment-forward (point-max))
    (<= end (point))))

;;;###autoload
(defun comment-or-uncomment-region (beg end &optional arg)
  "Call `comment-region', unless the region only consists of comments,
in which case call `uncomment-region'.  If a prefix arg is given, it
is passed on to the respective function."
  (interactive "*r\nP")
  (comment-normalize-vars)
  (funcall (if (comment-only-p beg end)
	       'uncomment-region 'comment-region)
	   beg end arg))

;;;###autoload
(defun comment-dwim (arg)
  "Call the comment command you want (Do What I Mean).
If the region is active and `transient-mark-mode' is on, call
`comment-region' (unless it only consists of comments, in which
case it calls `uncomment-region').
Else, if the current line is empty, call `comment-insert-comment-function'
if it is defined, otherwise insert a comment and indent it.
Else if a prefix ARG is specified, call `comment-kill'.
Else, call `comment-indent'.
You can configure `comment-style' to change the way regions are commented."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and mark-active transient-mark-mode)
      (comment-or-uncomment-region (region-beginning) (region-end) arg)
    (if (save-excursion (beginning-of-line) (not (looking-at "\\s-*$")))
	;; FIXME: If there's no comment to kill on this line and ARG is
	;; specified, calling comment-kill is not very clever.
	(if arg (comment-kill (and (integerp arg) arg)) (comment-indent))
      ;; Inserting a comment on a blank line. comment-indent calls
      ;; c-i-c-f if needed in the non-blank case.
      (if comment-insert-comment-function
          (funcall comment-insert-comment-function)
        (let ((add (comment-add arg)))
          ;; Some modes insist on keeping column 0 comment in column 0
          ;; so we need to move away from it before inserting the comment.
          (indent-according-to-mode)
          (insert (comment-padright comment-start add))
          (save-excursion
            (unless (string= "" comment-end)
              (insert (comment-padleft comment-end add)))
            (indent-according-to-mode)))))))

;;;###autoload
(defcustom comment-auto-fill-only-comments nil
  "Non-nil means to only auto-fill inside comments.
This has no effect in modes that do not define a comment syntax."
  :type 'boolean
  :group 'comment)

(defun comment-valid-prefix-p (prefix compos)
    "Check that the adaptive fill prefix is consistent with the context.
PREFIX is the prefix (presumably guessed by `adaptive-fill-mode').
COMPOS is the position of the beginning of the comment we're in, or nil
if we're not inside a comment."
  ;; This consistency checking is mostly needed to workaround the limitation
  ;; of auto-fill-mode whose paragraph-determination doesn't pay attention
  ;; to comment boundaries.
  (if (null compos)
      ;; We're not inside a comment: the prefix shouldn't match
      ;; a comment-starter.
      (not (and comment-start comment-start-skip
                (string-match comment-start-skip prefix)))
    (or
     ;; Accept any prefix if the current comment is not EOL-terminated.
     (save-excursion (goto-char compos) (comment-forward) (not (bolp)))
     ;; Accept any prefix that starts with the same comment-start marker
     ;; as the current one.
     (when (string-match (concat "\\`[ \t]*\\(?:" comment-start-skip "\\)")
                         prefix)
       (let ((prefix-com (comment-string-strip (match-string 0 prefix) nil t)))
         (string-match "\\`[ \t]*" prefix-com)
         (let* ((prefix-space (match-string 0 prefix-com))
                (prefix-indent (string-width prefix-space))
                (prefix-comstart (substring prefix-com (match-end 0))))
           (save-excursion
             (goto-char compos)
             ;; The comstart marker is the same.
             (and (looking-at (regexp-quote prefix-comstart))
                  ;; The indentation as well.
                  (or (= prefix-indent
                         (- (current-column) (current-left-margin)))
                      ;; Check the indentation in two different ways, just
                      ;; to try and avoid most of the potential funny cases.
                      (equal prefix-space
                             (buffer-substring (point)
                                               (progn (move-to-left-margin)
                                                      (point)))))))))))))


;;;###autoload
(defun comment-indent-new-line (&optional soft)
  "Break line at point and indent, continuing comment if within one.
This indents the body of the continued comment
under the previous comment line.

This command is intended for styles where you write a comment per line,
starting a new comment (and terminating it if necessary) on each line.
If you want to continue one comment across several lines, use \\[newline-and-indent].

If a fill column is specified, it overrides the use of the comment column
or comment indentation.

The inserted newline is marked hard if variable `use-hard-newlines' is true,
unless optional argument SOFT is non-nil."
  (interactive)
  (comment-normalize-vars t)
  (let (compos comin)
    ;; If we are not inside a comment and we only auto-fill comments,
    ;; don't do anything (unless no comment syntax is defined).
    (unless (and comment-start
		 comment-auto-fill-only-comments
		 (not (called-interactively-p 'interactive))
		 (not (save-excursion
			(prog1 (setq compos (comment-beginning))
			  (setq comin (point))))))

      ;; Now we know we should auto-fill.
      ;; Insert the newline before removing empty space so that markers
      ;; get preserved better.
      (if soft (insert-and-inherit ?\n) (newline 1))
      (save-excursion (forward-char -1) (delete-horizontal-space))
      (delete-horizontal-space)

      (if (and fill-prefix (not adaptive-fill-mode))
	  ;; Blindly trust a non-adaptive fill-prefix.
	  (progn
	    (indent-to-left-margin)
	    (insert-before-markers-and-inherit fill-prefix))

	;; If necessary check whether we're inside a comment.
	(unless (or compos (null comment-start))
	  (save-excursion
	    (backward-char)
	    (setq compos (comment-beginning))
	    (setq comin (point))))

	(cond
	 ;; If there's an adaptive prefix, use it unless we're inside
	 ;; a comment and the prefix is not a comment starter.
	 ((and fill-prefix
               (comment-valid-prefix-p fill-prefix compos))
	  (indent-to-left-margin)
	  (insert-and-inherit fill-prefix))
	 ;; If we're not inside a comment, just try to indent.
	 ((not compos) (indent-according-to-mode))
	 (t
	  (let* ((comment-column
		  ;; The continuation indentation should be somewhere between
		  ;; the current line's indentation (plus 2 for good measure)
		  ;; and the current comment's indentation, with a preference
		  ;; for comment-column.
		  (save-excursion
		    ;; FIXME: use prev line's info rather than first line's.
		    (goto-char compos)
		    (min (current-column) (max comment-column
					       (+ 2 (current-indentation))))))
		 (comstart (buffer-substring compos comin))
		 (normalp
		  (string-match (regexp-quote (comment-string-strip
					       comment-start t t))
				comstart))
		 (comment-end
		  (if normalp comment-end
		    ;; The comment starter is not the normal comment-start
		    ;; so we can't just use comment-end.
		    (save-excursion
		      (goto-char compos)
		      (if (not (comment-forward)) comment-end
			(comment-string-strip
			 (buffer-substring
			  (save-excursion (comment-enter-backward) (point))
			  (point))
			 nil t)))))
		 (comment-start comstart)
		 (continuep (or comment-multi-line
				(cadr (assoc comment-style comment-styles))))
		 ;; Force comment-continue to be recreated from comment-start.
		 ;; FIXME: wrong if comment-continue was set explicitly!
		 ;; FIXME: use prev line's continuation if available.
		 (comment-continue nil))
	    (if (and comment-multi-line (> (length comment-end) 0))
		(indent-according-to-mode)
	      (insert-and-inherit ?\n)
	      (forward-char -1)
	      (comment-indent continuep)
	      (save-excursion
		(let ((pt (point)))
		  (end-of-line)
		  (let ((comend (buffer-substring pt (point))))
		    ;; The 1+ is to make sure we delete the \n inserted above.
		    (delete-region pt (1+ (point)))
		    (end-of-line 0)
		    (insert comend))))))))))))

(provide 'newcomment)

;;; newcomment.el ends here
