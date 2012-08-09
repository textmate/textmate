;;; cc-align.el --- custom indentation functions for CC Mode

;; Copyright (C) 1985, 1987, 1992-2012  Free Software Foundation, Inc.

;; Authors:    2004- Alan Mackenzie
;;             1998- Martin Stjernholm
;;             1992-1999 Barry A. Warsaw
;;             1987 Dave Detlefs
;;             1987 Stewart Clamen
;;             1985 Richard M. Stallman
;; Maintainer: bug-cc-mode@gnu.org
;; Created:    22-Apr-1997 (split from cc-mode.el)
;; Keywords:   c languages
;; Package:    cc-mode

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

;;; Code:

(eval-when-compile
  (let ((load-path
	 (if (and (boundp 'byte-compile-dest-file)
		  (stringp byte-compile-dest-file))
	     (cons (file-name-directory byte-compile-dest-file) load-path)
	   load-path)))
    (load "cc-bytecomp" nil t)))

(cc-require 'cc-defs)
(cc-require 'cc-vars)
(cc-require 'cc-engine)


;; Standard line-up functions
;;
;; See the section "Custom Indentation Functions" in the manual for
;; details on the calling convention.

(defun c-lineup-topmost-intro-cont (langelem)
  "Line up declaration continuation lines zero or one indentation step.
For lines in the \"header\" of a definition, zero is used.  For other
lines, `c-basic-offset' is added to the indentation.  E.g:

int
neg (int i)           <- c-lineup-topmost-intro-cont
{
    return -i;
}

struct
larch                 <- c-lineup-topmost-intro-cont
{
    double height;
}
    the_larch,        <- c-lineup-topmost-intro-cont
    another_larch;    <- c-lineup-topmost-intro-cont
<--> c-basic-offset

struct larch
the_larch,            <- c-lineup-topmost-intro-cont
    another_larch;    <- c-lineup-topmost-intro-cont

\(This function is mainly provided to mimic the behavior of CC Mode
5.28 and earlier where this case wasn't handled consistently so that
these lines could be analyzed as either topmost-intro-cont or
statement-cont.)

Works with: topmost-intro-cont."
  (save-excursion
    (beginning-of-line)
    (c-backward-syntactic-ws (c-langelem-pos langelem))
    (if (and (memq (char-before) '(?} ?,))
	     (not (and c-overloadable-operators-regexp
		       (c-after-special-operator-id))))
	c-basic-offset)))

(defun c-lineup-gnu-DEFUN-intro-cont (langelem)
  "Line up the continuation lines of a DEFUN macro in the Emacs C source.
These lines are indented as though they were `knr-argdecl-intro' lines.
Return nil when we're not in such a construct.

This function is for historical compatibility with how previous CC Modes (5.28
and earlier) indented such lines.

Here is an example:

DEFUN (\"forward-char\", Fforward_char, Sforward_char, 0, 1, \"p\",
       doc: /* Move point right N characters (left if N is negative).
On reaching end of buffer, stop and signal error.  */)
     (n)                      <- c-lineup-gnu-DEFUN-into-cont
     Lisp_Object n;           <- c-lineup-gnu-DEFUN-into-cont

Works with: topmost-intro-cont."
  (save-excursion
    (let (case-fold-search)
      (goto-char (c-langelem-pos langelem))
      (if (looking-at "\\<DEFUN\\>")
	  (c-calc-offset '(knr-argdecl-intro))))))

(defun c-block-in-arglist-dwim (arglist-start)
  ;; This function implements the DWIM to avoid far indentation of
  ;; brace block constructs in arguments in `c-lineup-arglist' etc.
  ;; Return non-nil if a brace block construct is detected within the
  ;; arglist starting at ARGLIST-START.

  (or
   ;; Check if the syntactic context contains any of the symbols for
   ;; in-expression constructs.  This can both save the work that we
   ;; have to do below, and it also detect the brace list constructs
   ;; that `c-looking-at-inexpr-block' currently misses (they are
   ;; recognized by `c-inside-bracelist-p' instead).
   (assq 'inexpr-class c-syntactic-context)
   (assq 'inexpr-statement c-syntactic-context)
   (assq 'inlambda c-syntactic-context)

   (save-restriction
     ;; Search for open braces from the arglist start to the end of the
     ;; line.
     (narrow-to-region arglist-start (c-point 'eol arglist-start))

     (goto-char arglist-start)
     (while (and (c-syntactic-re-search-forward "{" nil t)
		 (progn
		   (backward-char)
		   (or
		    ;; Ignore starts of special brace lists.
		    (and c-special-brace-lists
			 (save-restriction
			   (widen)
			   (c-looking-at-special-brace-list)))
		    ;; Ignore complete blocks.
		    (c-safe (c-forward-sexp) t))))
       (forward-char))

     (looking-at "{"))

   (let (containing-sexp)
     (goto-char arglist-start)
     ;; `c-syntactic-eol' always matches somewhere on the line.
     (re-search-forward c-syntactic-eol)
     (goto-char (match-beginning 0))
     (c-forward-syntactic-ws)
     (setq containing-sexp (c-most-enclosing-brace (c-parse-state)))
     (c-looking-at-inexpr-block
      (c-safe-position (or containing-sexp (point)) c-state-cache)
      containing-sexp))))

(defun c-lineup-arglist (langelem)
  "Line up the current argument line under the first argument.

As a special case, if the indented line is inside a brace block
construct, the indentation is `c-basic-offset' only.  This is intended
as a \"DWIM\" measure in cases like macros that contains statement
blocks, e.g:

A_VERY_LONG_MACRO_NAME ({
        some (code, with + long, lines * in[it]);
    });
<--> c-basic-offset

This is motivated partly because it's more in line with how code
blocks are handled, and partly since it approximates the behavior of
earlier CC Mode versions, which due to inaccurate analysis tended to
indent such cases this way.

Works with: arglist-cont-nonempty, arglist-close."
  (save-excursion
    (let ((indent-pos (point)))

      (if (c-block-in-arglist-dwim (c-langelem-2nd-pos c-syntactic-element))
	  c-basic-offset		; DWIM case.

	;; Normal case.  Indent to the token after the arglist open paren.
	(goto-char (c-langelem-2nd-pos c-syntactic-element))
	(if (and c-special-brace-lists
		 (c-looking-at-special-brace-list))
	    ;; Skip a special brace list opener like "({".
	    (progn (c-forward-token-2)
		   (forward-char))
	  (forward-char))
	(let ((arglist-content-start (point)))
	  (c-forward-syntactic-ws)
	  (when (< (point) indent-pos)
	    (goto-char arglist-content-start)
	    (skip-chars-forward " \t"))
	  (vector (current-column)))))))

;; Contributed by Kevin Ryde <user42@zip.com.au>.
(defun c-lineup-argcont (elem)
  "Line up a continued argument.

foo (xyz, aaa + bbb + ccc
          + ddd + eee + fff);    <- c-lineup-argcont

Only continuation lines like this are touched, nil is returned on lines
which are the start of an argument.

Within a gcc asm block, \":\" is recognized as an argument separator,
but of course only between operand specifications, not in the expressions
for the operands.

Works with: arglist-cont, arglist-cont-nonempty."

  (save-excursion
    (beginning-of-line)

    (when (eq (car elem) 'arglist-cont-nonempty)
      ;; Our argument list might not be the innermost one.  If it
      ;; isn't, go back to the last position in it.  We do this by
      ;; stepping back over open parens until we get to the open paren
      ;; of our argument list.
      (let ((open-paren (c-langelem-2nd-pos c-syntactic-element))
	    (paren-state (c-parse-state)))
	(while (not (eq (car paren-state) open-paren))
	  (unless (consp (car paren-state)) ;; ignore matched braces
	    (goto-char (car paren-state)))
	  (setq paren-state (cdr paren-state)))))

    (let ((start (point)) c)

      (when (bolp)
	;; Previous line ending in a comma means we're the start of an
	;; argument.  This should quickly catch most cases not for us.
	;; This case is only applicable if we're the innermost arglist.
	(c-backward-syntactic-ws)
	(setq c (char-before)))

      (unless (eq c ?,)
	;; In a gcc asm, ":" on the previous line means the start of an
	;; argument.  And lines starting with ":" are not for us, don't
	;; want them to indent to the preceding operand.
	(let ((gcc-asm (save-excursion
			 (goto-char start)
			 (c-in-gcc-asm-p))))
	  (unless (and gcc-asm
		       (or (eq c ?:)
			   (save-excursion
			     (goto-char start)
			     (looking-at "[ \t]*:"))))

	    (c-lineup-argcont-scan (if gcc-asm ?:))
	    (vector (current-column))))))))

(defun c-lineup-argcont-scan (&optional other-match)
  ;; Find the start of an argument, for `c-lineup-argcont'.
  (when (zerop (c-backward-token-2 1 t))
    (let ((c (char-after)))
      (if (or (eq c ?,) (eq c other-match))
	  (progn
	    (forward-char)
	    (c-forward-syntactic-ws))
	(c-lineup-argcont-scan other-match)))))

(defun c-lineup-arglist-intro-after-paren (langelem)
  "Line up a line to just after the open paren of the surrounding paren
or brace block.

Works with: defun-block-intro, brace-list-intro,
statement-block-intro, statement-case-intro, arglist-intro."
  (save-excursion
    (beginning-of-line)
    (backward-up-list 1)
    (skip-chars-forward " \t" (c-point 'eol))
    (vector (1+ (current-column)))))

(defun c-lineup-arglist-close-under-paren (langelem)
  "Line up a line under the enclosing open paren.
Normally used to line up a closing paren in the same column as its
corresponding open paren, but can also be used with arglist-cont and
arglist-cont-nonempty to line up all lines inside a parenthesis under
the open paren.

As a special case, if a brace block construct starts at the same line
as the open parenthesis of the argument list, the indentation is
`c-basic-offset' only.  See `c-lineup-arglist' for further discussion
of this \"DWIM\" measure.

Works with: Almost all symbols, but are typically most useful on
arglist-close, brace-list-close, arglist-cont and arglist-cont-nonempty."
  (save-excursion
    (if (memq (c-langelem-sym langelem)
	      '(arglist-cont-nonempty arglist-close))
	(goto-char (c-langelem-2nd-pos c-syntactic-element))
      (beginning-of-line)
      (c-go-up-list-backward))

    (if (save-excursion (c-block-in-arglist-dwim (point)))
	c-basic-offset			; DWIM case.

      ;; Normal case.  Indent to the arglist open paren.
      (let (special-list)
	(if (and c-special-brace-lists
		 (setq special-list (c-looking-at-special-brace-list)))
	    ;; Cope if we're in the middle of a special brace list
	    ;; opener like "({".
	    (goto-char (car (car special-list))))
	(vector (current-column))))))

(defun c-lineup-arglist-operators (langelem)
  "Line up lines starting with an infix operator under the open paren.
Return nil on lines that don't start with an operator, to leave those
cases to other line-up functions.  Example:

if (  x < 10
   || at_limit (x,       <- c-lineup-arglist-operators
                list)    <- c-lineup-arglist-operators returns nil
   )

Since this function doesn't do anything for lines without an infix
operator you typically want to use it together with some other line-up
settings, e.g. as follows \(the arglist-close setting is just a
suggestion to get a consistent style):

\(c-set-offset 'arglist-cont '(c-lineup-arglist-operators 0))
\(c-set-offset 'arglist-cont-nonempty '(c-lineup-arglist-operators
                                        c-lineup-arglist))
\(c-set-offset 'arglist-close '(c-lineup-arglist-close-under-paren))

Works with: arglist-cont, arglist-cont-nonempty."
  (save-excursion
    (back-to-indentation)
    (when (looking-at "[-+|&*%<>=]\\|\\(/[^/*]\\)")
      ;; '-' can be both an infix and a prefix operator, but I'm lazy now..
      (c-lineup-arglist-close-under-paren langelem))))

(defun c-lineup-close-paren (langelem)
  "Line up the closing paren under its corresponding open paren if the
open paren is followed by code.  If the open paren ends its line, no
indentation is added.  E.g:

main (int,              main (
      char **               int, char **
     )           <->    )                 <- c-lineup-close-paren

As a special case, if a brace block construct starts at the same line
as the open parenthesis of the argument list, the indentation is
`c-basic-offset' instead of the open paren column.  See
`c-lineup-arglist' for further discussion of this \"DWIM\" measure.

Works with: All *-close symbols."
  (save-excursion
    (if (memq (c-langelem-sym langelem)
	      '(arglist-cont-nonempty arglist-close))
	(goto-char (c-langelem-2nd-pos c-syntactic-element))
      (beginning-of-line)
      (c-go-up-list-backward))

    (let (special-list arglist-start)
      (if (and c-special-brace-lists
	       (setq special-list (c-looking-at-special-brace-list)))
	  ;; Cope if we're in the middle of a special brace list
	  ;; opener like "({".
	  (progn
	    (goto-char (setq arglist-start (car (car special-list))))
	    (c-forward-token-2)
	    (forward-char))
	(setq arglist-start (point))
	(forward-char))

      (cond ((looking-at c-syntactic-eol)
	     0)				; The arglist is "empty".

	    ((c-block-in-arglist-dwim (point))
	     c-basic-offset)		; DWIM case.

	    (t
	     ;; Normal case.  Indent to the arglist open paren.
	     (goto-char arglist-start)
	     (vector (current-column)))))))

(defun c-lineup-streamop (langelem)
  "Line up C++ stream operators under each other.

Works with: stream-op."
  (save-excursion
    (goto-char (c-langelem-pos langelem))
    (re-search-forward "<<\\|>>" (c-point 'eol) 'move)
    (goto-char (match-beginning 0))
    (vector (current-column))))

(defun c-lineup-multi-inher (langelem)
  "Line up the classes in C++ multiple inheritance clauses and member
initializers under each other.  E.g:

class Foo:                Foo::Foo (int a, int b):
    public Cyphr,             Cyphr (a),
    public Bar       <->      Bar (b)               <- c-lineup-multi-inher

class Foo                 Foo::Foo (int a, int b)
    : public Cyphr,           : Cyphr (a),
      public Bar     <->        Bar (b)             <- c-lineup-multi-inher

class Foo                 Foo::Foo (int a, int b)
    : public Cyphr            : Cyphr (a)
    , public Bar     <->      , Bar (b)             <- c-lineup-multi-inher

Works with: inher-cont, member-init-cont."
  (save-excursion
    (back-to-indentation)
    (let* ((eol (c-point 'eol))
	   (here (point))
	   (char-after-ip (char-after)))
      (if (c-langelem-pos langelem)
	  (goto-char (c-langelem-pos langelem)))

      ;; This kludge is necessary to support both inher-cont and
      ;; member-init-cont, since they have different anchor positions.
      (c-backward-syntactic-ws)
      (when (eq (char-before) ?:)
	(backward-char)
	(c-backward-syntactic-ws))

      (c-syntactic-re-search-forward ":" eol 'move)
      (if (looking-at c-syntactic-eol)
	  (c-forward-syntactic-ws here)
	(if (eq char-after-ip ?,)
	    (backward-char)
	  (skip-chars-forward " \t" eol)))
      (if (< (point) here)
	  (vector (current-column)))
      )))

(defun c-lineup-java-inher (langelem)
  "Line up Java implements and extends declarations.
If class names follow on the same line as the implements/extends
keyword, they are lined up under each other.  Otherwise, they are
indented by adding `c-basic-offset' to the column of the keyword.
E.g:

class Foo             class Foo
    extends               extends Cyphr,
        Bar    <->                Bar     <- c-lineup-java-inher
    <--> c-basic-offset

Works with: inher-cont."
  (save-excursion
    (goto-char (c-langelem-pos langelem))
    (forward-word 1)
    (if (looking-at "[ \t]*$")
	c-basic-offset
      (c-forward-syntactic-ws)
      (vector (current-column)))))

(defun c-lineup-java-throws (langelem)
  "Line up Java throws declarations.
If exception names follow on the same line as the throws keyword,
they are lined up under each other.  Otherwise, they are indented by
adding `c-basic-offset' to the column of the throws keyword.  The
throws keyword itself is also indented by `c-basic-offset' from the
function declaration start if it doesn't hang.  E.g:

int foo()           int foo() throws Cyphr,
    throws     <->                   Bar,    <- c-lineup-java-throws
        Bar    <->                   Vlod    <- c-lineup-java-throws
<--><--> c-basic-offset

Works with: func-decl-cont."
  (save-excursion
    (let* ((lim (1- (c-point 'bol)))
	   (throws (catch 'done
		     (goto-char (c-langelem-pos langelem))
		     (while (zerop (c-forward-token-2 1 t lim))
		       (if (looking-at "throws\\>[^_]")
			   (throw 'done t))))))
      (if throws
	  (if (zerop (c-forward-token-2 1 nil (c-point 'eol)))
	      (vector (current-column))
	    (back-to-indentation)
	    (vector (+ (current-column) c-basic-offset)))
	c-basic-offset))))

(defun c-indent-one-line-block (langelem)
  "Indent a one line block `c-basic-offset' extra.
E.g:

if (n > 0)                 if (n > 0)
    {m+=n; n=0;}    <->    {               <- c-indent-one-line-block
<--> c-basic-offset            m+=n; n=0;
                           }

The block may use any kind of parenthesis character.  nil is returned
if the line doesn't start with a one line block, which makes the
function usable in list expressions.

Work with: Almost all syntactic symbols, but most useful on *-open."
  (save-excursion
    (let ((eol (c-point 'eol)))
      (back-to-indentation)
      (if (and (eq (char-syntax (char-after)) ?\()
	       (c-safe (progn (c-forward-sexp) t))
	       (<= (point) eol))
	  c-basic-offset
	nil))))

(defun c-indent-multi-line-block (langelem)
  "Indent a multi line block `c-basic-offset' extra.
E.g:

int *foo[] = {           int *foo[] = {
    NULL,                    NULL,
    {17},         <->            {       <- c-indent-multi-line-block
                                 17
                                 }
                             <--> c-basic-offset

The block may use any kind of parenthesis character.  nil is returned
if the line doesn't start with a multi line block, which makes the
function usable in list expressions.

Work with: Almost all syntactic symbols, but most useful on *-open."
  (save-excursion
    (let ((eol (c-point 'eol)))
      (back-to-indentation)
      (if (and (eq (char-syntax (char-after)) ?\()
	       (or (not (c-safe (progn (c-forward-sexp) t)))
		   (> (point) eol)))
	  c-basic-offset
	nil))))

(defun c-lineup-C-comments (langelem)
  "Line up C block comment continuation lines.
Various heuristics are used to handle many of the common comment
styles.  Some examples:

/*          /**         /*         /* text      /*          /**
 * text      * text       text        text      ** text      ** text
 */          */         */         */           */           */

/*********************************************************************
 * text
 ********************************************************************/

/*********************************************************************
    Free form text comments:
 In comments with a long delimiter line at the start, the indentation
 is kept unchanged for lines that start with an empty comment line
 prefix.  The delimiter line is whatever matches the
 `comment-start-skip' regexp.
*********************************************************************/

The variable `c-comment-prefix-regexp' is used to recognize the
comment line prefix, e.g. the `*' that usually starts every line
inside a comment.

Works with: The `c' syntactic symbol."
  (save-excursion
    (let* ((here (point))
	   (prefixlen (progn (back-to-indentation)
			     (if (looking-at c-current-comment-prefix)
				 (- (match-end 0) (point))
			       0)))
	   (starterlen
	    ;; Get the length of the comment starter, not including
	    ;; the first '/'. We check if the comment prefix matched
	    ;; on the current line matches the starter or if it
	    ;; matches comment-start-skip, and choose whichever is
	    ;; longest.
	    (max (save-excursion
		   (goto-char (1+ (c-langelem-pos langelem)))
		   (if (and (match-string 0)
			    (looking-at (regexp-quote (match-string 0))))
		       (- (match-end 0) (match-beginning 0))
		     0))
		 (save-excursion
		   (goto-char (c-langelem-pos langelem))
		   (looking-at comment-start-skip)
		   (- (or (match-end 1)
			  (save-excursion
			    (goto-char (match-end 0))
			    (skip-chars-backward " \t")
			    (point)))
		      (point)
		      1)))))
      (if (and (> starterlen 10) (zerop prefixlen))
	  ;; The comment has a long starter and the line doesn't have
	  ;; a nonempty comment prefix.  Treat it as free form text
	  ;; and don't change the indentation.
	  (vector (current-column))
	;; Go back to the previous non-blank line, if any.
	(while
	    (progn
	      (forward-line -1)
	      (back-to-indentation)
	      (and (> (point) (c-langelem-pos langelem))
		   (looking-at "[ \t]*$"))))
	;; Is the starting line the first continuation line with content?
	(if (>= (c-langelem-pos langelem) (point))
	    (if (zerop prefixlen)
		;; No nonempty comment prefix. Align after comment
		;; starter.
		(progn
		  (looking-at comment-start-skip)
		  (goto-char (match-end 0))
		  ;; The following should not be necessary, since
		  ;; comment-start-skip should match everything (i.e.
		  ;; typically whitespace) that leads up to the text.
		  ;;(if (looking-at "\\([ \t]+\\).+$")
		  ;;    ;; Align with the text that hangs after the
		  ;;    ;; comment starter.
		  ;;    (goto-char (match-end 1)))
		  (vector (current-column)))
	      ;; How long is the comment starter?  if greater than the
	      ;; length of the comment prefix, align left.  if less
	      ;; than or equal, align right.  this should also pick up
	      ;; Javadoc style comments.
	      (if (> starterlen prefixlen)
		  (progn
		    (goto-char (c-langelem-pos langelem))
		    (vector (1+ (current-column))))
		(goto-char (+ (c-langelem-pos langelem) starterlen 1))
		(vector (- (current-column) prefixlen))))
	  ;; We didn't start on the first non-blank continuation line.  If the
	  ;; previous line has a nonempty comment prefix, align with it.
	  ;; Otherwise, align with the previous nonempty line, but align the
	  ;; comment ender with the starter.
	  (when (or (not (looking-at c-current-comment-prefix))
		    (eq (match-beginning 0) (match-end 0)))
	    (goto-char here)
	    (back-to-indentation)
	    (if (looking-at (concat "\\(" c-current-comment-prefix "\\)\\*/"))
		(goto-char (c-langelem-pos langelem))
	      (while (and (zerop (forward-line -1))
			  (looking-at "^[ \t]*$")))
	      (back-to-indentation)
	      (if (< (point) (c-langelem-pos langelem))
		  ;; Align with the comment starter rather than
		  ;; with the code before it.
		  (goto-char (c-langelem-pos langelem)))))
	  (vector (current-column)))))))

(defun c-lineup-comment (langelem)
  "Line up a comment start according to `c-comment-only-line-offset'.
If the comment is lined up with a comment starter on the previous
line, that alignment is preserved.

Works with: comment-intro."
  (save-excursion
    (back-to-indentation)
    (let ((col (current-column)))
      (cond
       ;; CASE 1: preserve aligned comments
       ((save-excursion
	  (and (c-backward-single-comment)
	       (= col (current-column))))
	(vector col))			; Return an absolute column.
       ;; indent as specified by c-comment-only-line-offset
       ((not (bolp))
	(or (car-safe c-comment-only-line-offset)
	    c-comment-only-line-offset))
       (t
	(or (cdr-safe c-comment-only-line-offset)
	    (car-safe c-comment-only-line-offset)
	    -1000))			;jam it against the left side
       ))))

(defun c-lineup-knr-region-comment (langelem)
  "Line up a comment in the \"K&R region\" with the declaration.
That is the region between the function or class header and the
beginning of the block.  E.g:

int main()
/* This is the main function. */  <- c-lineup-knr-region-comment
{
  return 0;
}

Return nil if called in any other situation, to be useful in list
expressions.

Works with: comment-intro."
  (when (or (assq 'topmost-intro-cont c-syntactic-context)
	    (assq 'func-decl-cont c-syntactic-context)
	    (assq 'knr-argdecl-intro c-syntactic-context)
	    (assq 'lambda-intro-cont c-syntactic-context))
    (save-excursion
      (beginning-of-line)
      (c-beginning-of-statement-1)
      (vector (current-column)))))

(defun c-lineup-runin-statements (langelem)
  "Line up statements when the first statement is on the same line as
the block opening brace.  E.g:

int main()
{ puts (\"Hello world!\");
  return 0;                 <- c-lineup-runin-statements
}

If there is no statement after the opening brace to align with, nil is
returned.  This makes the function usable in list expressions.

Works with: The `statement' syntactic symbol."
  (if (eq (char-after (c-langelem-pos langelem)) ?{)
      (save-excursion
	(if (c-langelem-pos langelem)
	    (goto-char (c-langelem-pos langelem)))
	(forward-char 1)
	(skip-chars-forward " \t")
	(unless (eolp)
	  (vector (current-column))))))

(defun c-lineup-assignments (langelem)
  "Line up the current line after the assignment operator on the first
line in the statement.  If there isn't any, return nil to allow
stacking with other line-up functions.  If the current line contains
an assignment operator too, try to align it with the first one.

Works with: topmost-intro-cont, statement-cont, arglist-cont,
arglist-cont-nonempty."
  (let (startpos endpos equalp)

    (if (eq (c-langelem-sym langelem) 'arglist-cont-nonempty)
	;; If it's an arglist-cont-nonempty then we're only interested
	;; in equal signs outside it.  We don't search for a "=" on
	;; the current line since that'd have a different nesting
	;; compared to the one we should align with.
	(save-excursion
	  (save-restriction
	    (setq endpos (c-langelem-2nd-pos c-syntactic-element))
	    (narrow-to-region (c-langelem-pos langelem) endpos)
	    (if (setq startpos (c-up-list-backward endpos))
		(setq startpos (1+ startpos))
	      (setq startpos (c-langelem-pos langelem)))))

      (setq startpos (c-langelem-pos langelem)
	    endpos (point))

      ;; Find a syntactically relevant and unnested "=" token on the
      ;; current line.  equalp is in that case set to the number of
      ;; columns to left shift the current line to align it with the
      ;; goal column.
      (save-excursion
	(beginning-of-line)
	(when (c-syntactic-re-search-forward
	       c-assignment-op-regexp
	       (c-point 'eol) t t t)
	  (setq equalp (- (or (match-beginning 1)
			      (match-end 0))
			  (c-point 'boi))))))

    (save-excursion
      (goto-char startpos)
      (if (or (if (c-syntactic-re-search-forward
		   c-assignment-op-regexp
		   (min endpos (c-point 'eol)) t t t)
		  (progn
		    (goto-char (or (match-beginning 1)
				   (match-end 0)))
		    nil)
		t)
	      (save-excursion
		(c-forward-syntactic-ws (c-point 'eol))
		(eolp)))
	  ;; There's no equal sign on the line, or there is one but
	  ;; nothing follows it.
	  nil

	;; calculate indentation column after equals and ws, unless
	;; our line contains an equals sign
	(if (not equalp)
	    (progn
	      (skip-chars-forward " \t")
	      (setq equalp 0)))

	(vector (- (current-column) equalp)))
      )))

(defun c-lineup-math (langelem)
  "Like `c-lineup-assignments' but indent with `c-basic-offset' if no
assignment operator was found on the first line.  I.e. this function
is the same as specifying a list (c-lineup-assignments +).  It's
provided for compatibility with old configurations.

Works with: topmost-intro-cont, statement-cont, arglist-cont,
arglist-cont-nonempty."
  (or (c-lineup-assignments langelem)
      c-basic-offset))

(defun c-lineup-cascaded-calls (langelem)
  "Line up \"cascaded calls\" under each other.
If the line begins with \"->\" or \".\" and the preceding line ends
with one or more function calls preceded by the same token, then the
arrow is lined up with the first of those tokens.  E.g:

result = proc->add(17)->add(18)
             ->add(19) +           <- c-lineup-cascaded-calls
  offset;                          <- c-lineup-cascaded-calls (inactive)

In any other situation nil is returned to allow use in list
expressions.

Works with: topmost-intro-cont, statement-cont, arglist-cont,
arglist-cont-nonempty."

  (if (and (eq (c-langelem-sym langelem) 'arglist-cont-nonempty)
	   (not (eq (c-langelem-2nd-pos c-syntactic-element)
		    (c-most-enclosing-brace (c-parse-state)))))
      ;; The innermost open paren is not our one, so don't do
      ;; anything.  This can occur for arglist-cont-nonempty with
      ;; nested arglist starts on the same line.
      nil

    (save-excursion
      (back-to-indentation)
      (let ((operator (and (looking-at "->\\|\\.")
			   (regexp-quote (match-string 0))))
	    (stmt-start (c-langelem-pos langelem)) col)

	(when (and operator
		   (looking-at operator)
		   (zerop (c-backward-token-2 1 t stmt-start))
		   (eq (char-after) ?\()
		   (zerop (c-backward-token-2 2 t stmt-start))
		   (looking-at operator))
	  (setq col (current-column))

	  (while (and (zerop (c-backward-token-2 1 t stmt-start))
		      (eq (char-after) ?\()
		      (zerop (c-backward-token-2 2 t stmt-start))
		      (looking-at operator))
	    (setq col (current-column)))

	  (vector col))))))

(defun c-lineup-string-cont (langelem)
  "Line up a continued string under the one it continues.
A continued string in this sense is where a string literal follows
directly after another one.  E.g:

result = prefix + \"A message \"
                  \"string.\";      <- c-lineup-string-cont

In other situations, returns nil, to allow stacking with other
line-up functions.

Works with: topmost-intro-cont, statement-cont, arglist-cont,
arglist-cont-nonempty."
  (save-excursion
    (back-to-indentation)
    (and (looking-at "\\s\"")
	 (let ((quote (char-after)) pos)
	   (while (and (progn (c-backward-syntactic-ws)
			      (eq (char-before) quote))
		       (c-safe (c-backward-sexp) t)
		       (/= (setq pos (point)) (c-point 'boi))))
	   (when pos
	     (goto-char pos)
	     (vector (current-column)))))))

(defun c-lineup-template-args (langelem)
  "Line up template argument lines under the first argument.
To allow this function to be used in a list expression, nil is
returned if there's no template argument on the first line.

Works with: template-args-cont."
  (save-excursion
    (c-with-syntax-table c++-template-syntax-table
      (beginning-of-line)
      (backward-up-list 1)
      (if (and (eq (char-after) ?<)
	       (zerop (c-forward-token-2 1 nil (c-point 'eol))))
	  (vector (current-column))))))

(defun c-lineup-ObjC-method-call (langelem)
  "Line up selector args as Emacs Lisp mode does with function args:
Go to the position right after the message receiver, and if you are at
the end of the line, indent the current line c-basic-offset columns
from the opening bracket; otherwise you are looking at the first
character of the first method call argument, so line up the current
line with it.

Works with: objc-method-call-cont."
  (save-excursion
    (let* ((extra (save-excursion
		    (back-to-indentation)
		    (c-backward-syntactic-ws (c-langelem-pos langelem))
		    (if (eq (char-before) ?:)
			(- c-basic-offset)
		      0)))
	   (open-bracket-pos (c-langelem-pos langelem))
           (open-bracket-col (progn
			       (goto-char open-bracket-pos)
			       (current-column)))
           (target-col (progn
			 (forward-char)
			 (c-forward-sexp)
			 (skip-chars-forward " \t")
			 (if (eolp)
			     (+ open-bracket-col c-basic-offset)
			   (current-column))))
	   )
      (- target-col open-bracket-col extra))))

(defun c-lineup-ObjC-method-call-colons (langelem)
  "Line up selector args as Project Builder / XCode: colons of first
   selector portions on successive lines are aligned.  If no decision can
   be made return NIL, so that other lineup methods can be tried.  This is
   typically chained with `c-lineup-ObjC-method-call'.

Works with: objc-method-call-cont."
  (save-excursion
    (catch 'no-idea
      (let* ((method-arg-len (progn
			       (back-to-indentation)
			       (if (search-forward ":" (c-point 'eol) 'move)
				   (- (point) (c-point 'boi))
				 ; no complete argument to indent yet
				 (throw 'no-idea nil))))

	     (extra (save-excursion 
                      ; indent parameter to argument if needed
		      (back-to-indentation)
		      (c-backward-syntactic-ws (c-langelem-pos langelem))
		      (if (eq ?: (char-before))
			  c-objc-method-parameter-offset 0)))

	     (open-bracket-col (c-langelem-col langelem))

	     (arg-ralign-colon-ofs (progn
			(forward-char) ; skip over '['
			; skip over object/class name
			; and first argument
			(c-forward-sexp 2)
			(if (search-forward ":" (c-point 'eol) 'move)
			    (- (current-column) open-bracket-col
			       method-arg-len extra)
			  ; previous arg has no param
  			  c-objc-method-arg-unfinished-offset))))

	(if (>= arg-ralign-colon-ofs c-objc-method-arg-min-delta-to-bracket)
	    (+ arg-ralign-colon-ofs extra)
	  (throw 'no-idea nil))))))

(defun c-lineup-ObjC-method-args (langelem)
  "Line up the colons that separate args in a method declaration.
The colon on the current line is aligned with the one on the first
line.

Works with: objc-method-args-cont."
  (save-excursion
    (let* ((here (c-point 'boi))
	   (curcol (progn (goto-char here) (current-column)))
	   (eol (c-point 'eol))
	   (relpos (c-langelem-pos langelem))
	   (first-col-column (progn
			       (goto-char relpos)
			       (skip-chars-forward "^:" eol)
			       (and (eq (char-after) ?:)
				    (current-column)))))
      (if (not first-col-column)
	  c-basic-offset
	(goto-char here)
	(skip-chars-forward "^:" eol)
	(if (eq (char-after) ?:)
	    (+ curcol (- first-col-column (current-column)))
	  c-basic-offset)))))

(defun c-lineup-ObjC-method-args-2 (langelem)
  "Line up the colons that separate args in a method declaration.
The colon on the current line is aligned with the one on the previous
line.

Works with: objc-method-args-cont."
  (save-excursion
    (let* ((here (c-point 'boi))
	   (curcol (progn (goto-char here) (current-column)))
	   (eol (c-point 'eol))
	   (relpos (c-langelem-pos langelem))
	   (prev-col-column (progn
			      (skip-chars-backward "^:" relpos)
			      (and (eq (char-before) ?:)
				   (- (current-column) 1)))))
      (if (not prev-col-column)
	  c-basic-offset
	(goto-char here)
	(skip-chars-forward "^:" eol)
	(if (eq (char-after) ?:)
	    (+ curcol (- prev-col-column (current-column)))
	  c-basic-offset)))))

(defun c-lineup-inexpr-block (langelem)
  "Line up the block for constructs that use a block inside an expression,
e.g. anonymous classes in Java and lambda functions in Pike.  The body
is aligned with the start of the header, e.g. with the \"new\" or
\"lambda\" keyword.  Returns nil if the block isn't part of such a
construct.

Works with: inlambda, inexpr-statement, inexpr-class."
  (save-excursion
    (back-to-indentation)
    (let* ((paren-state (c-parse-state))
	   (containing-sexp (c-most-enclosing-brace paren-state))
	   (res (or (c-looking-at-inexpr-block
		     (c-safe-position containing-sexp paren-state)
		     containing-sexp)
		    (and containing-sexp
			 (progn (goto-char containing-sexp)
				(eq (char-after) ?{))
			 (progn (setq containing-sexp
				      (c-most-enclosing-brace paren-state
							      (point)))
				(c-looking-at-inexpr-block
				 (c-safe-position containing-sexp paren-state)
				 containing-sexp))))))
      (when res
	(goto-char (cdr res))
	(vector (current-column))))))

(defun c-lineup-whitesmith-in-block (langelem)
  "Line up lines inside a block in Whitesmith style.
It's done in a way that works both when the opening brace hangs and
when it doesn't.  E.g:

something
    {                something {
    foo;     <->         foo;     <- c-lineup-whitesmith-in-block
    }                    }
                     <--> c-basic-offset

In the first case the indentation is kept unchanged, in the
second `c-basic-offset' is added.

Works with: defun-close, defun-block-intro, inline-close, block-close,
brace-list-close, brace-list-intro, statement-block-intro,
arglist-intro, arglist-cont-nonempty, arglist-close, and all in*
symbols, e.g. inclass and inextern-lang."
  (save-excursion
    (if (and (c-go-up-list-backward)
	     (= (point) (c-point 'boi)))
	nil
      c-basic-offset)))

(defun c-lineup-after-whitesmith-blocks (langelem)
  "Compensate for Whitesmith style indentation of blocks.
Due to the way CC Mode calculates anchor positions for normal lines
inside blocks, this function is necessary for those lines to get
correct Whitesmith style indentation.  Consider the following
examples:

                    int foo()
                        {
int foo()                   {
    {                       a;
    a;                      }
    x;       <->        x;        <- c-lineup-after-whitesmith-blocks

The fact that the line with \"x\" is preceded by a Whitesmith style
indented block in one case and not the other should not affect its
indentation.  But since CC Mode in cases like this uses the
indentation of the preceding statement as anchor position, the \"x\"
would in the rightmost case be indented too much if the offset for
`statement' was set simply to zero.

This lineup function corrects for this situation by detecting if the
anchor position is at an open paren character.  In that case, it
instead indents relative to the surrounding block just like
`c-lineup-whitesmith-in-block'.

Works with: brace-list-entry, brace-entry-open, statement,
arglist-cont."
  (save-excursion
    (goto-char (c-langelem-pos langelem))
    (when (looking-at "\\s\(")
      (if (c-go-up-list-backward)
	  (let ((pos (point)))
	    (back-to-indentation)
	    (if (= pos (point))
		(vector (current-column))
	      (vector (+ (current-column) c-basic-offset))))
	(vector 0)))))

(defun c-lineup-cpp-define (langelem)
  "Line up macro continuation lines according to the indentation of
the construct preceding the macro.  E.g:

v beg of preceding constr      v beg of preceding constr
                             int dribble() {
const char msg[] =             if (!running)
  \"Some text.\";	         error(\"Not running!\");

#define X(A, B)  \           #define X(A, B)    \
do {             \    <->      do {             \    <- c-lineup-cpp-define
  printf (A, B); \               printf (A, B); \
} while (0)                    } while (0)

If `c-syntactic-indentation-in-macros' is non-nil, the function
returns the relative indentation to the macro start line to allow
accumulation with other offsets.  E.g. in the following cases,
cpp-define-intro is combined with the statement-block-intro that comes
from the \"do {\" that hangs on the \"#define\" line:

                             int dribble() {
const char msg[] =             if (!running)
  \"Some text.\";	         error(\"Not running!\");

#define X(A, B) do { \       #define X(A, B) do { \
  printf (A, B);     \  <->      printf (A, B);   \  <- c-lineup-cpp-define
  this->refs++;      \           this->refs++;    \
} while (0)             <->    } while (0)           <- c-lineup-cpp-define

The relative indentation returned by `c-lineup-cpp-define' is zero and
two, respectively, in these two examples.  They are then added to the
two column indentation that statement-block-intro gives in both cases
here.

If the relative indentation is zero, then nil is returned instead.
That is useful in a list expression to specify the default indentation
on the top level.

If `c-syntactic-indentation-in-macros' is nil then this function keeps
the current indentation, except for empty lines \(ignoring the ending
backslash) where it takes the indentation from the closest preceding
nonempty line in the macro.  If there's no such line in the macro then
the indentation is taken from the construct preceding it, as described
above.

Works with: cpp-define-intro."
  (let (offset)
    (if c-syntactic-indentation-in-macros
	;; Go to the macro start and do a syntactic analysis of it.
	;; Then remove the cpp-macro element it should contain and
	;; calculate the indentation it then would get.
	(save-excursion
	  (c-beginning-of-macro)
	  (setq offset (- (c-get-syntactic-indentation
			   (delete '(cpp-macro) (c-guess-basic-syntax)))
			  (save-excursion
			    (back-to-indentation)
			    (current-column))))
	  (if (zerop offset)
	      nil
	    offset))
      ;; Do not indent syntactically inside the macro.
      (save-excursion
	(let ((macro-start-line (save-excursion
				  (goto-char (c-query-macro-start))
				  (beginning-of-line)
				  (point))))
	  (beginning-of-line)
	  ;; Check every line while inside the macro.
	  (while (and (> (point) macro-start-line)
		      (looking-at "[ \t]*\\\\?$")
		      (= (forward-line -1) 0)))
	  (if (<= (point) macro-start-line)
	      ;; If we've stepped out of the macro we take the
	      ;; syntactic offset.
	      (setq offset (c-get-syntactic-indentation
			    (delete '(cpp-macro) (c-guess-basic-syntax))))
	    (setq offset (current-indentation)))
	  (if (zerop offset)
	      nil
	    (vector offset)))))))

;; Contributed by Kevin Ryde <user42@zip.com.au>.
(defun c-lineup-gcc-asm-reg (elem)
  "Line up a gcc asm register under one on a previous line.

    asm (\"foo %1, %0\\n\"
         \"bar %0, %1\"
         : \"=r\" (w),
           \"=r\" (x)
         :  \"0\" (y),
            \"1\" (z));

The \"x\" line is aligned to the text after the \":\" on the \"w\" line, and
similarly \"z\" under \"y\".

This is done only in an \"asm\" or \"__asm__\" block, and only to
those lines mentioned.  Anywhere else nil is returned.  The usual
arrangement is to have this routine as an extra feature at the start
of arglist line-ups, e.g.

    (c-lineup-gcc-asm-reg c-lineup-arglist)

Works with: arglist-cont, arglist-cont-nonempty."

  (let ((orig-pos (point))
	alignto)
    (save-excursion
      (and
       c-opt-asm-stmt-key

       ;; Don't do anything if the innermost open paren isn't our one.
       ;; This can occur for arglist-cont-nonempty with nested arglist
       ;; starts on the same line.
       (or (not (eq (car elem) 'arglist-cont-nonempty))
	   (eq (c-langelem-2nd-pos c-syntactic-element)
	       (c-most-enclosing-brace (c-parse-state))))

       ;; Find the ":" to align to.  Look for this first so as to quickly
       ;; eliminate pretty much all cases which are not for us.
       (re-search-backward "^[ \t]*:[ \t]*\\(.\\)?" (cdr elem) t)

       ;; Must have something after the ":".
       (setq alignto (match-beginning 1))

       ;; Don't touch ":" lines themselves.
       (progn (goto-char orig-pos)
	      (beginning-of-line)
	      (not (looking-at "^[ \t]*:")))

       ;; Only operate in an asm statement.
       (progn (goto-char orig-pos)
	      (c-in-gcc-asm-p))

       (vector (progn (goto-char alignto) (current-column)))))))

(defun c-lineup-dont-change (langelem)
  "Do not change the indentation of the current line.

Works with: Any syntactic symbol."
  (save-excursion
    (back-to-indentation)
    (vector (current-column))))


(defun c-snug-do-while (syntax pos)
  "Dynamically calculate brace hanginess for do-while statements.
Using this function, `while' clauses that end a `do-while' block will
remain on the same line as the brace that closes that block.

See `c-hanging-braces-alist' for how to utilize this function as an
ACTION associated with `block-close' syntax."
  (save-excursion
    (let (langelem)
      (if (and (eq syntax 'block-close)
	       (setq langelem (assq 'block-close c-syntactic-context))
	       (progn (goto-char (c-langelem-pos langelem))
		      (if (eq (char-after) ?{)
			  (c-safe (c-forward-sexp -1)))
		      (looking-at "\\<do\\>[^_]")))
	  '(before)
	'(before after)))))

(defun c-snug-1line-defun-close (syntax pos)
  "Determine the brace hanginess for an AWK defun-close.
If the action/function being closed is a one-liner, keep it so.  Otherwise put
the closing brace on its own line."
  (save-excursion
    (goto-char pos)
    (if (> (c-point 'bol)
	   (progn (up-list -1) (point)))
	'(before after)
      '(after))))

(defun c-gnu-impose-minimum ()
  "Imposes a minimum indentation for lines inside code blocks.
The variable `c-label-minimum-indentation' specifies the minimum
indentation amount."

  (when (and (not
	      ;; Don't adjust macro or comment-only lines.
	      (or (assq 'cpp-macro c-syntactic-context)
		  (assq 'comment-intro c-syntactic-context)))
	     (c-intersect-lists c-inside-block-syms c-syntactic-context)
	     (save-excursion
	       (back-to-indentation)
	       (< (current-column) c-label-minimum-indentation)))
    (c-shift-line-indentation (- c-label-minimum-indentation
				 (current-indentation)))))


;; Useful for c-hanging-semi&comma-criteria

(defun c-semi&comma-inside-parenlist ()
  "Controls newline insertion after semicolons in parenthesis lists.
If a comma was inserted, no determination is made.  If a semicolon was
inserted inside a parenthesis list, no newline is added otherwise a
newline is added.  In either case, checking is stopped.  This supports
exactly the old newline insertion behavior."
  ;; newline only after semicolon, but only if that semicolon is not
  ;; inside a parenthesis list (e.g. a for loop statement)
  (if (not (eq last-command-event ?\;))
      nil				; continue checking
    (if (condition-case nil
	    (save-excursion
	      (up-list -1)
	      (not (eq (char-after) ?\()))
	  (error t))
	t
      'stop)))

;; Suppresses newlines before non-blank lines
(defun c-semi&comma-no-newlines-before-nonblanks ()
  "Controls newline insertion after semicolons.
If a comma was inserted, no determination is made.  If a semicolon was
inserted, and the following line is not blank, no newline is inserted.
Otherwise, no determination is made."
  (save-excursion
    (if (and (= last-command-event ?\;)
	     ;;(/= (point-max)
	     ;;    (save-excursion (skip-syntax-forward " ") (point))
	     (zerop (forward-line 1))
	     (bolp)			; forward-line has funny behavior at eob.
	     (not (looking-at "^[ \t]*$")))
	'stop
      nil)))

;; Suppresses new lines after semicolons in one-liners methods
(defun c-semi&comma-no-newlines-for-oneline-inliners ()
  "Controls newline insertion after semicolons for some one-line methods.
If a comma was inserted, no determination is made.  Newlines are
suppressed in one-liners, if the line is an in-class inline function.
For other semicolon contexts, no determination is made."
  (let ((syntax (c-guess-basic-syntax))
        (bol (save-excursion
               (if (c-safe (up-list -1) t)
                   (c-point 'bol)
                 -1))))
    (if (and (eq last-command-event ?\;)
             (eq (car (car syntax)) 'inclass)
             (eq (car (car (cdr syntax))) 'topmost-intro)
             (= (c-point 'bol) bol))
        'stop
      nil)))


(cc-provide 'cc-align)

;;; cc-align.el ends here
