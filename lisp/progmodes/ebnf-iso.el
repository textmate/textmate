;;; ebnf-iso.el --- parser for ISO EBNF

;; Copyright (C) 1999-2012  Free Software Foundation, Inc.

;; Author: Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Maintainer: Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Keywords: wp, ebnf, PostScript
;; Version: 1.9
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
;; This package defines a parser for ISO EBNF.
;;
;; See ebnf2ps.el for documentation.
;;
;;
;; ISO EBNF Syntax
;; ---------------
;;
;;	See the URL:
;;	`http://www.cl.cam.ac.uk/~mgk25/iso-ebnf.html'
;;	("International Standard of the ISO EBNF Notation").
;;
;;
;; ISO EBNF = syntax rule, {syntax rule};
;;
;; syntax rule = meta identifier, '=', definition list, ';';
;;
;; definition list = single definition, {'|', single definition};
;;
;; single definition = term, {',', term};
;;
;; term = factor, ['-', exception];
;;
;; exception = factor (* without <meta identifier> *);
;;
;; factor = [integer, '*'], primary;
;;
;; primary = optional sequence | repeated sequence | special sequence
;;         | grouped sequence | meta identifier | terminal string
;;         | empty;
;;
;; empty = ;
;;
;; optional sequence = '[', definition list, ']';
;;
;; repeated sequence = '{', definition list, '}';
;;
;; grouped sequence = '(', definition list, ')';
;;
;; terminal string = "'", character - "'", {character - "'"}, "'"
;;                 | '"', character - '"', {character - '"'}, '"';
;;
;; special sequence = '?', {character - '?'}, '?';
;;
;; meta identifier = letter, { letter | decimal digit | ' ' };
;;
;; integer = decimal digit, {decimal digit};
;;
;; comment = '(*', {comment symbol}, '*)';
;;
;; comment symbol = comment (* <== NESTED COMMENT *)
;;                | terminal string | special sequence | character;
;;
;; letter = ? A-Z a-z ?;
;;
;; decimal digit = ? 0-9 ?;
;;
;; character = letter | decimal digit
;;           | ',' | '=' | '|' | '/' | '!' | '*' | '(' | ')' | '[' | ']' | '{'
;;           | '}' | "'" | '"' | '?' | '-' | ';' | '.' | ' ' | ':' | '+' | '_'
;;           | '%' | '@' | '&' | '#' | '$' | '<' | '>' | '\' | '^' | '`' | '~';
;;
;;
;; There is also the following alternative representation:
;;
;; STANDARD   ALTERNATIVE
;;    |    ==>   / or !
;;    [    ==>   (/
;;    ]    ==>   /)
;;    {    ==>   (:
;;    }    ==>   :)
;;    ;    ==>   .
;;
;;
;; Differences Between ISO EBNF And ebnf2ps ISO EBNF
;; -------------------------------------------------
;;
;; ISO EBNF accepts the characters given by <character> production above,
;; HORIZONTAL TAB (^I), VERTICAL TAB (^K), NEWLINE (^J or ^M) and FORM FEED
;; (^L), any other characters are invalid.  But ebnf2ps accepts also the
;; european 8-bit accentuated characters (from \240 to \377) and underscore
;; (_).
;;
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:


(require 'ebnf-otz)


(defvar ebnf-iso-lex nil
  "Value returned by `ebnf-iso-lex' function.")


(defvar ebnf-no-meta-identifier nil
  "Used by `ebnf-iso-term' and `ebnf-iso-lex' functions.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntactic analyzer


;;; ISO EBNF = syntax rule, {syntax rule};

(defun ebnf-iso-parser (start)
  "ISO EBNF parser."
  (let ((total (+ (- ebnf-limit start) 1))
	(bias (1- start))
	(origin (point))
	syntax-list token rule)
    (goto-char start)
    (setq token (ebnf-iso-lex))
    (and (eq token 'end-of-input)
	 (error "Invalid ISO EBNF file format"))
    (while (not (eq token 'end-of-input))
      (ebnf-message-float
       "Parsing...%s%%"
       (/ (* (- (point) bias) 100.0) total))
      (setq token (ebnf-iso-syntax-rule token)
	    rule  (cdr token)
	    token (car token))
      (or (ebnf-add-empty-rule-list rule)
	  (setq syntax-list (cons rule syntax-list))))
    (goto-char origin)
    syntax-list))


;;; syntax rule = meta identifier, '=', definition list, ';';

(defun ebnf-iso-syntax-rule (token)
  (let ((header ebnf-iso-lex)
	(action ebnf-action)
	body)
    (setq ebnf-action nil)
    (or (eq token 'non-terminal)
	(error "Invalid meta identifier syntax rule"))
    (or (eq (ebnf-iso-lex) 'equal)
	(error "Invalid syntax rule: missing `='"))
    (setq body (ebnf-iso-definition-list))
    (or (eq (car body) 'period)
	(error "Invalid syntax rule: missing `;' or `.'"))
    (setq body (cdr body))
    (ebnf-eps-add-production header)
    (cons (ebnf-iso-lex)
	  (ebnf-make-production header body action))))


;;; definition list = single definition, {'|', single definition};

(defun ebnf-iso-definition-list ()
  (let (body sequence)
    (while (eq (car (setq sequence (ebnf-iso-single-definition)))
	       'alternative)
      (setq sequence (cdr sequence)
	    body     (cons sequence body)))
    (ebnf-token-alternative body sequence)))


;;; single definition = term, {',', term};

(defun ebnf-iso-single-definition ()
  (let (token seq term)
    (while (and (setq term  (ebnf-iso-term (ebnf-iso-lex))
		      token (car term)
		      term  (cdr term))
		(eq token 'catenate))
      (setq seq (cons term seq)))
    (cons token
	  (ebnf-token-sequence (if term
				   (cons term seq)
				 seq)))))


;;; term = factor, ['-', exception];
;;;
;;; exception = factor (* without <meta identifier> *);

(defun ebnf-iso-term (token)
  (let ((factor (ebnf-iso-factor token)))
    (if (not (eq (car factor) 'except))
	;; factor
	factor
      ;; factor - exception
      (let ((ebnf-no-meta-identifier t))
	(ebnf-token-except (cdr factor) (ebnf-iso-factor (ebnf-iso-lex)))))))


;;; factor = [integer, '*'], primary;

(defun ebnf-iso-factor (token)
  (if (eq token 'integer)
      (let ((times ebnf-iso-lex))
	(or (eq (ebnf-iso-lex) 'repeat)
	    (error "Missing `*'"))
	(ebnf-token-repeat times (ebnf-iso-primary (ebnf-iso-lex))))
    (ebnf-iso-primary token)))


;;; primary = optional sequence | repeated sequence | special sequence
;;;         | grouped sequence | meta identifier | terminal string
;;;         | empty;
;;;
;;; empty = ;
;;;
;;; optional sequence = '[', definition list, ']';
;;;
;;; repeated sequence = '{', definition list, '}';
;;;
;;; grouped sequence = '(', definition list, ')';
;;;
;;; terminal string = "'", character - "'", {character - "'"}, "'"
;;;                 | '"', character - '"', {character - '"'}, '"';
;;;
;;; special sequence = '?', {character - '?'}, '?';
;;;
;;; meta identifier = letter, {letter | decimal digit};

(defun ebnf-iso-primary (token)
  (let ((primary
	 (cond
	  ;; terminal string
	  ((eq token 'terminal)
	   (ebnf-make-terminal ebnf-iso-lex))
	  ;; meta identifier
	  ((eq token 'non-terminal)
	   (ebnf-make-non-terminal ebnf-iso-lex))
	  ;; special sequence
	  ((eq token 'special)
	   (ebnf-make-special ebnf-iso-lex))
	  ;; grouped sequence
	  ((eq token 'begin-group)
	   (let ((body (ebnf-iso-definition-list)))
	     (or (eq (car body) 'end-group)
		 (error "Missing `)'"))
	     (cdr body)))
	  ;; optional sequence
	  ((eq token 'begin-optional)
	   (let ((body (ebnf-iso-definition-list)))
	     (or (eq (car body) 'end-optional)
		 (error "Missing `]' or `/)'"))
	     (ebnf-token-optional (cdr body))))
	  ;; repeated sequence
	  ((eq token 'begin-zero-or-more)
	   (let* ((body   (ebnf-iso-definition-list))
		  (repeat (cdr body)))
	     (or (eq (car body) 'end-zero-or-more)
		 (error "Missing `}' or `:)'"))
	     (ebnf-make-zero-or-more repeat)))
	  ;; empty
	  (t
	   nil)
	  )))
    (cons (if primary
	      (ebnf-iso-lex)
	    token)
	  primary)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lexical analyzer


(defconst ebnf-iso-token-table
  ;; control character & 8-bit character are set to `error'
  (let ((table (make-vector 256 'error))
	(char ?\040))
    ;; printable character
    (while (< char ?\060)
      (aset table char 'character)
      (setq char (1+ char)))
    ;; digits:
    (while (< char ?\072)
      (aset table char 'integer)
      (setq char (1+ char)))
    (while (< char ?\101)
      (aset table char 'character)
      (setq char (1+ char)))
    ;; upper case letters:
    (while (< char ?\133)
      (aset table char 'non-terminal)
      (setq char (1+ char)))
    (while (< char ?\141)
      (aset table char 'character)
      (setq char (1+ char)))
    ;; lower case letters:
    (while (< char ?\173)
      (aset table char 'non-terminal)
      (setq char (1+ char)))
    (while (< char ?\177)
      (aset table char 'character)
      (setq char (1+ char)))
    ;; European 8-bit accentuated characters:
    (setq char ?\240)
    (while (< char ?\400)
      (aset table char 'non-terminal)
      (setq char (1+ char)))
    ;; Override space characters:
    (aset table ?\013 'space)		; [VT] vertical tab
    (aset table ?\n   'space)		; [NL] linefeed
    (aset table ?\r   'space)		; [CR] carriage return
    (aset table ?\t   'space)		; [HT] horizontal tab
    (aset table ?\    'space)		; [SP] space
    ;; Override form feed character:
    (aset table ?\f 'form-feed)		; [FF] form feed
    ;; Override other lexical characters:
    (aset table ?_  'non-terminal)
    (aset table ?\" 'double-terminal)
    (aset table ?\' 'single-terminal)
    (aset table ?\? 'special)
    (aset table ?*  'repeat)
    (aset table ?,  'catenate)
    (aset table ?-  'except)
    (aset table ?=  'equal)
    (aset table ?\) 'end-group)
    table)
  "Vector used to map characters to a lexical token.")


(defun ebnf-iso-initialize ()
  "Initialize ISO EBNF token table."
  (if ebnf-iso-alternative-p
      ;; Override alternative lexical characters:
      (progn
	(aset ebnf-iso-token-table ?\( 'left-parenthesis)
	(aset ebnf-iso-token-table ?\[ 'character)
	(aset ebnf-iso-token-table ?\] 'character)
	(aset ebnf-iso-token-table ?\{ 'character)
	(aset ebnf-iso-token-table ?\} 'character)
	(aset ebnf-iso-token-table ?|  'character)
	(aset ebnf-iso-token-table ?\; 'character)
	(aset ebnf-iso-token-table ?/  'slash)
	(aset ebnf-iso-token-table ?!  'alternative)
	(aset ebnf-iso-token-table ?:  'colon)
	(aset ebnf-iso-token-table ?.  'period))
    ;; Override standard lexical characters:
    (aset ebnf-iso-token-table ?\( 'begin-parenthesis)
    (aset ebnf-iso-token-table ?\[ 'begin-optional)
    (aset ebnf-iso-token-table ?\] 'end-optional)
    (aset ebnf-iso-token-table ?\{ 'begin-zero-or-more)
    (aset ebnf-iso-token-table ?\} 'end-zero-or-more)
    (aset ebnf-iso-token-table ?|  'alternative)
    (aset ebnf-iso-token-table ?\; 'period)
    (aset ebnf-iso-token-table ?/  'character)
    (aset ebnf-iso-token-table ?!  'character)
    (aset ebnf-iso-token-table ?:  'character)
    (aset ebnf-iso-token-table ?.  'character)))


;; replace the range "\240-\377" (see `ebnf-range-regexp').
(defconst ebnf-iso-non-terminal-chars
  (ebnf-range-regexp " 0-9A-Za-z_" ?\240 ?\377))


(defun ebnf-iso-lex ()
  "Lexical analyzer for ISO EBNF.

Return a lexical token.

See documentation for variable `ebnf-iso-lex'."
  (if (>= (point) ebnf-limit)
      'end-of-input
    (let (token)
      ;; skip spaces and comments
      (while (if (> (following-char) 255)
		 (progn
		   (setq token 'error)
		   nil)
	       (setq token (aref ebnf-iso-token-table (following-char)))
	       (cond
		((eq token 'space)
		 (skip-chars-forward " \013\n\r\t" ebnf-limit)
		 (< (point) ebnf-limit))
		((or (eq token 'begin-parenthesis)
		     (eq token 'left-parenthesis))
		 (forward-char)
		 (if (/= (following-char) ?*)
		     ;; no comment
		     nil
		   ;; comment
		   (ebnf-iso-skip-comment)
		   t))
		((eq token 'form-feed)
		 (forward-char)
		 (setq ebnf-action 'form-feed))
		(t nil)
		)))
      (cond
       ;; end of input
       ((>= (point) ebnf-limit)
	'end-of-input)
       ;; error
       ((eq token 'error)
	(error "Invalid character"))
       ;; integer
       ((eq token 'integer)
	(setq ebnf-iso-lex (ebnf-buffer-substring "0-9"))
	'integer)
       ;; special: ?special?
       ((eq token 'special)
	(setq ebnf-iso-lex (concat (and ebnf-special-show-delimiter "?")
				   (ebnf-string " ->@-~" ?\? "special")
				   (and ebnf-special-show-delimiter "?")))
	'special)
       ;; terminal: "string"
       ((eq token 'double-terminal)
	(setq ebnf-iso-lex (ebnf-string " !#-~" ?\" "terminal"))
	'terminal)
       ;; terminal: 'string'
       ((eq token 'single-terminal)
	(setq ebnf-iso-lex (ebnf-string " -&(-~" ?\' "terminal"))
	'terminal)
       ;; non-terminal
       ((eq token 'non-terminal)
	(setq ebnf-iso-lex
	      (ebnf-iso-normalize
	       (ebnf-trim-right
		(ebnf-buffer-substring ebnf-iso-non-terminal-chars))))
	(and ebnf-no-meta-identifier
	     (error "Exception sequence should not contain a meta identifier"))
	'non-terminal)
       ;; begin optional, begin list or begin group
       ((eq token 'left-parenthesis)
	(forward-char)
	(cond ((= (following-char) ?/)
	       (forward-char)
	       'begin-optional)
	      ((= (following-char) ?:)
	       (forward-char)
	       'begin-zero-or-more)
	      (t
	       'begin-group)
	      ))
       ;; end optional or alternative
       ((eq token 'slash)
	(forward-char)
	(if (/= (following-char) ?\))
	    'alternative
	  (forward-char)
	  'end-optional))
       ;; end list
       ((eq token 'colon)
	(forward-char)
	(if (/= (following-char) ?\))
	    'character
	  (forward-char)
	  'end-zero-or-more))
       ;; begin group
       ((eq token 'begin-parenthesis)
	'begin-group)
       ;; miscellaneous
       (t
	(forward-char)
	token)
       ))))


;; replace the range "\177-\237" (see `ebnf-range-regexp').
(defconst ebnf-iso-comment-chars
  (ebnf-range-regexp "^*(\000-\010\016-\037" ?\177 ?\237))


(defun ebnf-iso-skip-comment ()
  (forward-char)
  (cond
   ;; open EPS file
   ((and ebnf-eps-executing (= (following-char) ?\[))
    (ebnf-eps-add-context (ebnf-iso-eps-filename)))
   ;; close EPS file
   ((and ebnf-eps-executing (= (following-char) ?\]))
    (ebnf-eps-remove-context (ebnf-iso-eps-filename)))
   ;; EPS header
   ((and ebnf-eps-executing (= (following-char) ?H))
    (ebnf-eps-header-comment (ebnf-iso-eps-filename)))
   ;; EPS footer
   ((and ebnf-eps-executing (= (following-char) ?F))
    (ebnf-eps-footer-comment (ebnf-iso-eps-filename)))
   ;; any other action in comment
   (t
    (setq ebnf-action (aref ebnf-comment-table (following-char))))
   )
  (let ((pair 1))
    (while (> pair 0)
      (skip-chars-forward ebnf-iso-comment-chars ebnf-limit)
      (cond ((>= (point) ebnf-limit)
	     (error "Missing end of comment: `*)'"))
	    ((= (following-char) ?*)
	     (skip-chars-forward "*" ebnf-limit)
	     (when (= (following-char) ?\))
	       ;; end of comment
	       (forward-char)
	       (setq pair (1- pair))))
	    ((= (following-char) ?\()
	     (skip-chars-forward "(" ebnf-limit)
	     (when (= (following-char) ?*)
	       ;; beginning of comment
	       (forward-char)
	       (setq pair (1+ pair))))
	    (t
	     (error "Invalid character"))
	    ))))


(defun ebnf-iso-eps-filename ()
  (forward-char)
  (buffer-substring-no-properties
   (point)
   (let ((chars (concat ebnf-iso-comment-chars "\n"))
	 found)
     (while (not found)
       (skip-chars-forward chars ebnf-limit)
       (setq found
	     (cond ((>= (point) ebnf-limit)
		    (point))
		   ((= (following-char) ?*)
		    (skip-chars-forward "*" ebnf-limit)
		    (if (/= (following-char) ?\))
			nil
		      (backward-char)
		      (point)))
		   ((= (following-char) ?\()
		    (forward-char)
		    (if (/= (following-char) ?*)
			nil
		      (backward-char)
		      (point)))
		   (t
		    (point))
		   )))
     found)))


(defun ebnf-iso-normalize (str)
  (if (not ebnf-iso-normalize-p)
      str
    (let ((len (length str))
	  (stri 0)
	  (spaces 0))
      ;; count exceeding spaces
      (while (< stri len)
	(if (/= (aref str stri) ?\ )
	    (setq stri (1+ stri))
	  (setq stri (1+ stri))
	  (while (and (< stri len) (= (aref str stri) ?\ ))
	    (setq stri   (1+ stri)
		  spaces (1+ spaces)))))
      (if (zerop spaces)
	  ;; no exceeding space
	  str
	;; at least one exceeding space
	(let ((new (make-string (- len spaces) ?\ ))
	      (newi 0))
	  ;; eliminate exceeding spaces
	  (setq stri 0)
	  (while (> spaces 0)
	    (if (/= (aref str stri) ?\ )
		(progn
		  (aset new newi (aref str stri))
		  (setq stri (1+ stri)
			newi (1+ newi)))
	      (aset new newi (aref str stri))
	      (setq stri (1+ stri)
		    newi (1+ newi))
	      (while (and (> spaces 0) (= (aref str stri) ?\ ))
		(setq stri   (1+ stri)
		      spaces (1- spaces)))))
	  ;; remaining is normalized
	  (while (< stri len)
	    (aset new newi (aref str stri))
	    (setq stri (1+ stri)
		  newi (1+ newi)))
	  new)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'ebnf-iso)


;;; ebnf-iso.el ends here
