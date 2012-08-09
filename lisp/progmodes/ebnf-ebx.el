;;; ebnf-ebx.el --- parser for EBNF used to specify XML (EBNFX)

;; Copyright (C) 2001-2012  Free Software Foundation, Inc.

;; Author: Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Maintainer: Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Keywords: wp, ebnf, PostScript
;; Version: 1.2
;; Package: ebnf2ps

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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; This is part of ebnf2ps package.
;;
;; This package defines a parser for EBNF used to specify XML (EBNFX).
;;
;; See ebnf2ps.el for documentation.
;;
;;
;; EBNFX Syntax
;; ------------
;;
;;	See the URL:
;;	`http://www.w3.org/TR/2004/REC-xml-20040204/#sec-notation'
;;	(Extensible Markup Language (XML) 1.0 (Third Edition))
;;
;;
;; rule ::= symbol '::=' expression
;; /* rules are separated by at least one blank line. */
;;
;; expression ::= concatenation ('|' concatenation)*
;;
;; concatenation ::= exception*
;;
;; exception ::= term ('-' term)?
;;
;; term ::= factor ('*' | '+' | '?')?
;;
;; factor ::= hex-char+
;;          | '[' '^'? ( char ( '-' char )? )+ ']'
;;          | '"' 'string' '"'
;;          | "'" "string" "'"
;;          | '(' expression ')'
;;          | symbol
;;
;; symbol ::= 'upper or lower case letter'
;;            ('upper or lower case letter' | '-' | '_')*
;; /* upper and lower 8-bit accentuated characters are included */
;;
;; hex-char ::= '#x' [0-9A-Fa-f]+
;;
;; char ::= hex-char | 'any character except control characters'
;; /* 8-bit accentuated characters are included */
;;
;; any-char ::= char | 'newline' | 'tab'
;;
;; ignore ::= '[' ('wfc' | 'WFC' | 'vc' | 'VC') ':' ( any-char - ']' )* ']'
;;
;; comment ::= '/*' ( any-char - '*/' ) '*/'
;;
;;
;; Below is the Notation section extracted from the URL cited above.
;;
;; 6 Notation
;;
;; The formal grammar of XML is given in this specification using a simple
;; Extended Backus-Naur Form (EBNF) notation.  Each rule in the grammar defines
;; one symbol, in the form
;;
;; symbol ::= expression
;;
;; Symbols are written with an initial capital letter if they are the start
;; symbol of a regular language, otherwise with an initial lowercase letter.
;; Literal strings are quoted.
;;
;; Within the expression on the right-hand side of a rule, the following
;; expressions are used to match strings of one or more characters:
;;
;; #xN
;;
;;     where N is a hexadecimal integer, the expression matches the character
;;     whose number (code point) in ISO/IEC 10646 is N.  The number of leading
;;     zeros in the #xN form is insignificant.
;;
;; [a-zA-Z], [#xN-#xN]
;;
;;     matches any Char with a value in the range(s) indicated (inclusive).
;;
;; [abc], [#xN#xN#xN]
;;
;;     matches any Char with a value among the characters enumerated.
;;     Enumerations and ranges can be mixed in one set of brackets.
;;
;; [^a-z], [^#xN-#xN]
;;
;;     matches any Char with a value outside the range indicated.
;;
;; [^abc], [^#xN#xN#xN]
;;
;;     matches any Char with a value not among the characters given.
;;     Enumerations and ranges of forbidden values can be mixed in one set of
;;     brackets.
;;
;; "string"
;;
;;     matches a literal string matching that given inside the double quotes.
;;
;; 'string'
;;
;;     matches a literal string matching that given inside the single quotes.
;;
;; These symbols may be combined to match more complex patterns as follows,
;; where A and B represent simple expressions:
;;
;; (expression)
;;
;;     expression is treated as a unit and may be combined as described in this
;;     list.
;;
;; A?
;;
;;     matches A or nothing; optional A.
;;
;; A B
;;
;;     matches A followed by B.  This operator has higher precedence than
;;     alternation; thus A B | C D is identical to (A B) | (C D).
;;
;; A | B
;;
;;     matches A or B.
;;
;; A - B
;;
;;     matches any string that matches A but does not match B.
;;
;; A+
;;
;;     matches one or more occurrences of A.  Concatenation has higher
;;     precedence than alternation; thus A+ | B+ is identical to (A+) | (B+).
;;
;; A*
;;
;;     matches zero or more occurrences of A.  Concatenation has higher
;;     precedence than alternation; thus A* | B* is identical to (A*) | (B*).
;;
;; Other notations used in the productions are:
;;
;; /* ... */
;;
;;     comment.
;;
;; [ wfc: ... ]
;;
;;     well-formedness constraint; this identifies by name a constraint on
;;     well-formed documents associated with a production.
;;
;; [ vc: ... ]
;;
;;     validity constraint; this identifies by name a constraint on valid
;;     documents associated with a production.
;;
;;
;; Differences Between EBNFX And ebnf2ps EBNFX
;; -------------------------------------------
;;
;; Besides the characters that EBNFX accepts, ebnf2ps EBNFX accepts also the
;; underscore (_) and minus (-) for rule name and european 8-bit accentuated
;; characters (from \240 to \377) for rule name, string and comment.  Also
;; rule name can start with upper case letter.
;;
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:


(require 'ebnf-otz)


(defvar ebnf-ebx-lex nil
  "Value returned by `ebnf-ebx-lex' function.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntactic analyzer


;;; rulelist ::=  rule+

(defun ebnf-ebx-parser (start)
  "EBNFX parser."
  (let ((total (+ (- ebnf-limit start) 1))
	(bias (1- start))
	(origin (point))
	rule-list token rule)
    (goto-char start)
    (setq token (ebnf-ebx-lex))
    (and (eq token 'end-of-input)
	 (error "Invalid EBNFX file format"))
    (and (eq token 'end-of-rule)
	 (setq token (ebnf-ebx-lex)))
    (while (not (eq token 'end-of-input))
      (ebnf-message-float
       "Parsing...%s%%"
       (/ (* (- (point) bias) 100.0) total))
      (setq token (ebnf-ebx-rule token)
	    rule  (cdr token)
	    token (car token))
      (or (ebnf-add-empty-rule-list rule)
	  (setq rule-list (cons rule rule-list))))
    (goto-char origin)
    rule-list))


;;; rule ::= symbol '::=' expression


(defun ebnf-ebx-rule (token)
  (let ((name ebnf-ebx-lex)
	(action ebnf-action)
	elements)
    (setq ebnf-action nil)
    (or (eq token 'non-terminal)
	(error "Invalid rule name"))
    (setq token (ebnf-ebx-lex))
    (or (eq token 'production)
	(error "Invalid rule: missing `::='"))
    (setq elements (ebnf-ebx-expression))
    (or (memq (car elements) '(end-of-rule end-of-input))
	(error "Invalid rule: there is no end of rule"))
    (setq elements (cdr elements))
    (ebnf-eps-add-production name)
    (cons (ebnf-ebx-lex)
	  (ebnf-make-production name elements action))))


;; expression ::= concatenation ('|' concatenation)*


(defun ebnf-ebx-expression ()
  (let (body concatenation)
    (while (eq (car (setq concatenation
			  (ebnf-ebx-concatenation (ebnf-ebx-lex))))
	       'alternative)
      (setq body (cons (cdr concatenation) body)))
    (ebnf-token-alternative body concatenation)))


;; concatenation ::= exception*


(defun ebnf-ebx-concatenation (token)
  (let ((term (ebnf-ebx-exception token))
	seq)
    (or (setq token (car term)
	      term  (cdr term))
	(error "Empty element"))
    (setq seq (cons term seq))
    (while (setq term  (ebnf-ebx-exception token)
		 token (car term)
		 term  (cdr term))
      (setq seq (cons term seq)))
    (cons token
	  (ebnf-token-sequence seq))))


;;; exception ::= term ('-' term)?


(defun ebnf-ebx-exception (token)
  (let ((term (ebnf-ebx-term token)))
    (if (eq (car term) 'exception)
	(let ((except (ebnf-ebx-term (ebnf-ebx-lex))))
	  (cons (car except)
		(ebnf-make-except (cdr term) (cdr except))))
      term)))



;;; term ::= factor ('*' | '+' | '?')?


(defun ebnf-ebx-term (token)
  (let ((factor (ebnf-ebx-factor token)))
    (when factor
      (setq token (ebnf-ebx-lex))
      (cond ((eq token 'zero-or-more)
	     (setq factor (ebnf-make-zero-or-more factor)
		   token  (ebnf-ebx-lex)))
	    ((eq token 'one-or-more)
	     (setq factor (ebnf-make-one-or-more factor)
		   token  (ebnf-ebx-lex)))
	    ((eq token 'optional)
	     (setq factor (ebnf-token-optional factor)
		   token  (ebnf-ebx-lex)))))
      (cons token factor)))


;;; factor ::= hex-char+
;;;          | '[' '^'? ( char ( '-' char )? )+ ']'
;;;          | '"' 'string' '"'
;;;          | "'" "string" "'"
;;;          | '(' expression ')'
;;;          | symbol
;;;
;;; symbol ::= 'upper or lower case letter'
;;;            ('upper or lower case letter' | '-' | '_')*
;;; /* upper and lower 8-bit accentuated characters are included */
;;;
;;; hex-char ::= '#x' [0-9A-Fa-f]+
;;;
;;; char ::= hex-char | 'any character except control characters'
;;; /* 8-bit accentuated characters are included */
;;;
;;; any-char ::= char | 'newline' | 'tab'


(defun ebnf-ebx-factor (token)
  (cond
   ;; terminal
   ((eq token 'terminal)
    (ebnf-make-terminal ebnf-ebx-lex))
   ;; non-terminal
   ((eq token 'non-terminal)
    (ebnf-make-non-terminal ebnf-ebx-lex))
   ;; group
   ((eq token 'begin-group)
    (let ((body (ebnf-ebx-expression)))
      (or (eq (car body) 'end-group)
	  (error "Missing `)'"))
      (cdr body)))
   ;; no element
   (t
    nil)
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lexical analyzer


(defconst ebnf-ebx-token-table (make-vector 256 'error)
  "Vector used to map characters to a lexical token.")


(defun ebnf-ebx-initialize ()
  "Initialize EBNFX token table."
  ;; control character & control 8-bit character are set to `error'
  (let ((char ?\101))
    ;; printable character: A-Z
    (while (< char ?\133)
      (aset ebnf-ebx-token-table char 'non-terminal)
      (setq char (1+ char)))
    ;; printable character: a-z
    (setq char ?\141)
    (while (< char ?\173)
      (aset ebnf-ebx-token-table char 'non-terminal)
      (setq char (1+ char)))
    ;; European 8-bit accentuated characters:
    (setq char ?\240)
    (while (< char ?\400)
      (aset ebnf-ebx-token-table char 'non-terminal)
      (setq char (1+ char)))
    ;; Override end of line characters:
    (aset ebnf-ebx-token-table ?\n 'end-of-rule) ; [NL] linefeed
    (aset ebnf-ebx-token-table ?\r 'end-of-rule) ; [CR] carriage return
    ;; Override space characters:
    (aset ebnf-ebx-token-table ?\013 'space) ; [VT] vertical tab
    (aset ebnf-ebx-token-table ?\t   'space) ; [HT] horizontal tab
    (aset ebnf-ebx-token-table ?\    'space) ; [SP] space
    ;; Override form feed character:
    (aset ebnf-ebx-token-table ?\f 'form-feed) ; [FF] form feed
    ;; Override other lexical characters:
    (aset ebnf-ebx-token-table ?#  'hash)
    (aset ebnf-ebx-token-table ?\" 'double-quote)
    (aset ebnf-ebx-token-table ?\' 'single-quote)
    (aset ebnf-ebx-token-table ?\( 'begin-group)
    (aset ebnf-ebx-token-table ?\) 'end-group)
    (aset ebnf-ebx-token-table ?-  'exception)
    (aset ebnf-ebx-token-table ?:  'colon)
    (aset ebnf-ebx-token-table ?\[ 'begin-square)
    (aset ebnf-ebx-token-table ?|  'alternative)
    (aset ebnf-ebx-token-table ?*  'zero-or-more)
    (aset ebnf-ebx-token-table ?+  'one-or-more)
    (aset ebnf-ebx-token-table ?\? 'optional)
    ;; Override comment character:
    (aset ebnf-ebx-token-table ?/  'comment)))


;; replace the range "\240-\377" (see `ebnf-range-regexp').
(defconst ebnf-ebx-non-terminal-chars
  (ebnf-range-regexp "-_A-Za-z" ?\240 ?\377))
(defconst ebnf-ebx-non-terminal-letter-chars
  (ebnf-range-regexp "A-Za-z" ?\240 ?\377))


(defun ebnf-ebx-lex ()
  "Lexical analyzer for EBNFX.

Return a lexical token.

See documentation for variable `ebnf-ebx-lex'."
  (if (>= (point) ebnf-limit)
      'end-of-input
    (let (token)
      ;; skip spaces and comments
      (while (if (> (following-char) 255)
		 (progn
		   (setq token 'error)
		   nil)
	       (setq token (aref ebnf-ebx-token-table (following-char)))
	       (cond
		((eq token 'space)
		 (skip-chars-forward " \013\t" ebnf-limit)
		 (< (point) ebnf-limit))
		((eq token 'comment)
		 (ebnf-ebx-skip-comment))
		((eq token 'form-feed)
		 (forward-char)
		 (setq ebnf-action 'form-feed))
		((eq token 'end-of-rule)
		 (ebnf-ebx-skip-end-of-rule))
		((and (eq token 'begin-square)
		      (let ((case-fold-search  t))
			(looking-at "\\[\\(wfc\\|vc\\):")))
		 (ebnf-ebx-skip-constraint))
		(t nil)
		)))
      (cond
       ;; end of input
       ((>= (point) ebnf-limit)
	'end-of-input)
       ;; error
       ((eq token 'error)
	(error "Invalid character"))
       ;; end of rule
       ((eq token 'end-of-rule)
	'end-of-rule)
       ;; terminal: #x [0-9A-Fa-f]+
       ((eq token 'hash)
	(setq ebnf-ebx-lex (ebnf-ebx-character))
	'terminal)
       ;; terminal: "string"
       ((eq token 'double-quote)
	(setq ebnf-ebx-lex (ebnf-ebx-string ?\"))
	'terminal)
       ;; terminal: 'string'
       ((eq token 'single-quote)
	(setq ebnf-ebx-lex (ebnf-ebx-string ?\'))
	'terminal)
       ;; terminal: [ ^? ( char ( - char )? )+ ]
       ((eq token 'begin-square)
	(setq ebnf-ebx-lex (ebnf-ebx-range))
	'terminal)
       ;; non-terminal: NAME
       ((eq token 'non-terminal)
	(setq ebnf-ebx-lex
	      (ebnf-buffer-substring ebnf-ebx-non-terminal-chars))
	'non-terminal)
       ;; colon: ::=
       ((eq token 'colon)
	(or (looking-at "::=")
	    (error "Missing `::=' token"))
	(forward-char 3)
	'production)
       ;; miscellaneous: (, ), *, +, ?, |, -
       (t
	(forward-char)
	token)
       ))))


;; replace the range "\177-\237" (see `ebnf-range-regexp').
(defconst ebnf-ebx-constraint-chars
  (ebnf-range-regexp "^\000-\010\016-\037]" ?\177 ?\237))


(defun ebnf-ebx-skip-constraint ()
  (or (> (skip-chars-forward ebnf-ebx-constraint-chars ebnf-limit) 0)
      (error "Invalid character"))
  (or (= (following-char) ?\])
      (error "Missing end of constraint `]'"))
  (forward-char)
  t)



(defun ebnf-ebx-skip-end-of-rule ()
  (let (eor-p)
    (while (progn
	     ;; end of rule ==> 2 or more consecutive end of lines
	     (setq eor-p (or (> (skip-chars-forward "\r\n" ebnf-limit) 1)
			     eor-p))
	     ;; skip spaces
	     (skip-chars-forward " \013\t" ebnf-limit)
	     ;; skip comments
	     (and (= (following-char) ?/)
		  (ebnf-ebx-skip-comment))))
    (not eor-p)))


;; replace the range "\177-\237" (see `ebnf-range-regexp').
(defconst ebnf-ebx-comment-chars
  (ebnf-range-regexp "^\000-\010\016-\037\\*" ?\177 ?\237))
(defconst ebnf-ebx-filename-chars
  (ebnf-range-regexp "^\000-\037\\*" ?\177 ?\237))


(defun ebnf-ebx-skip-comment ()
  (forward-char)
  (or (= (following-char) ?*)
      (error "Invalid beginning of comment"))
  (forward-char)
  (cond
   ;; open EPS file
   ((and ebnf-eps-executing (= (following-char) ?\[))
    (ebnf-eps-add-context (ebnf-ebx-eps-filename)))
   ;; close EPS file
   ((and ebnf-eps-executing (= (following-char) ?\]))
    (ebnf-eps-remove-context (ebnf-ebx-eps-filename)))
   ;; EPS header
   ((and ebnf-eps-executing (= (following-char) ?H))
    (ebnf-eps-header-comment (ebnf-ebx-eps-filename)))
   ;; EPS footer
   ((and ebnf-eps-executing (= (following-char) ?F))
    (ebnf-eps-footer-comment (ebnf-ebx-eps-filename)))
   ;; any other action in comment
   (t
    (setq ebnf-action (aref ebnf-comment-table (following-char))))
   )
  (while (progn
	   (skip-chars-forward ebnf-ebx-comment-chars ebnf-limit)
	   (or (= (following-char) ?*)
	       (error "Missing end of comment"))
	   (forward-char)
	   (and (/= (following-char) ?/)
		(< (point) ebnf-limit))))
  ;; check for a valid end of comment
  (and (>= (point) ebnf-limit)
       (error "Missing end of comment"))
  (forward-char)
  t)


(defun ebnf-ebx-eps-filename ()
  (forward-char)
  (let (fname nchar)
    (while (progn
	     (setq fname
		   (concat fname
			   (ebnf-buffer-substring ebnf-ebx-filename-chars)))
	     (and (< (point) ebnf-limit)
		  (> (setq nchar (skip-chars-forward "*" ebnf-limit)) 0)
		  (< (point) ebnf-limit)
		  (/= (following-char) ?/)))
      (setq fname (concat fname (make-string nchar ?*))
	    nchar nil))
    (if (or (not nchar) (= nchar 0))
	fname
      (and (< (point) ebnf-limit)
	   (= (following-char) ?/)
	   (setq nchar (1- nchar)))
      (concat fname (make-string nchar ?*)))))


;; replace the range "\240-\377" (see `ebnf-range-regexp').
(defconst ebnf-ebx-double-string-chars
  (ebnf-range-regexp "\t -!#-~" ?\240 ?\377))
(defconst ebnf-ebx-single-string-chars
  (ebnf-range-regexp "\t -&(-~" ?\240 ?\377))


(defun ebnf-ebx-string (delim)
  (buffer-substring-no-properties
   (progn
     (forward-char)
     (point))
   (progn
     (skip-chars-forward (if (= delim ?\")
			     ebnf-ebx-double-string-chars
			   ebnf-ebx-single-string-chars)
			 ebnf-limit)
     (or (= (following-char) delim)
	 (error "Missing string delimiter `%c'" delim))
     (prog1
	 (point)
       (forward-char)))))


(defun ebnf-ebx-character ()
  ;; #x [0-9A-Fa-f]+
  (buffer-substring-no-properties
   (point)
   (progn
     (ebnf-ebx-hex-character)
     (point))))


(defun ebnf-ebx-range ()
  ;; [ ^? ( char ( - char )? )+ ]
  (buffer-substring-no-properties
   (point)
   (progn
     (forward-char)
     (and (= (following-char) ?^)
	  (forward-char))
     (and (= (following-char) ?-)
	  (forward-char))
     (while (progn
	      (ebnf-ebx-any-character)
	      (when (= (following-char) ?-)
		(forward-char)
		(ebnf-ebx-any-character))
	      (and (/= (following-char) ?\])
		   (< (point) ebnf-limit))))
     (and (>= (point) ebnf-limit)
	  (error "Missing end of character range `]'"))
     (forward-char)
     (point))))


(defun ebnf-ebx-any-character ()
  (let ((char (following-char)))
    (cond ((= char ?#)
	   (ebnf-ebx-hex-character t))
	  ((or (and (<= ?\    char) (<= char ?\")) ; #
	       (and (<= ?$    char) (<= char ?,))  ; -
	       (and (<= ?.    char) (<= char ?\\)) ; ]
	       (and (<= ?^    char) (<= char ?~))
	       (and (<= ?\240 char) (<= char ?\377)))
	   (forward-char))
	  (t
	   (error "Invalid character `%c'" char)))))


(defun ebnf-ebx-hex-character (&optional no-error)
  ;; #x [0-9A-Fa-f]+
  (forward-char)
  (if (/= (following-char) ?x)
      (or no-error
	  (error "Invalid hexadecimal character"))
    (forward-char)
    (or (> (skip-chars-forward "0-9A-Fa-f" ebnf-limit) 0)
	(error "Invalid hexadecimal character"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'ebnf-ebx)

;;; ebnf-ebx.el ends here
