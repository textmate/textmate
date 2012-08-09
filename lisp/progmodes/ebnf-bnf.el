;;; ebnf-bnf.el --- parser for EBNF

;; Copyright (C) 1999-2012  Free Software Foundation, Inc.

;; Author: Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Maintainer: Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Keywords: wp, ebnf, PostScript
;; Version: 1.10
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
;; This package defines a parser for EBNF.
;;
;; See ebnf2ps.el for documentation.
;;
;;
;; EBNF Syntax
;; -----------
;;
;; The current EBNF that ebnf2ps accepts has the following constructions:
;;
;;    ;			comment (until end of line)
;;    A			non-terminal
;;    "C"		terminal
;;    ?C?		special
;;    $A		default non-terminal
;;    $"C"		default terminal
;;    $?C?		default special
;;    A = B.		production (A is the header and B the body)
;;    C D		sequence (C occurs before D)
;;    C | D		alternative (C or D occurs)
;;    A - B		exception (A excluding B, B without any non-terminal)
;;    n * A		repetition (A repeats at least n (integer) times)
;;    n * n A		repetition (A repeats exactly n (integer) times)
;;    n * m A		repetition (A repeats at least n (integer) and at most
;;			m (integer) times)
;;    (C)		group (expression C is grouped together)
;;    [C]		optional (C may or not occurs)
;;    C+		one or more occurrences of C
;;    {C}+		one or more occurrences of C
;;    {C}*		zero or more occurrences of C
;;    {C}		zero or more occurrences of C
;;    C / D		equivalent to: C {D C}*
;;    {C || D}+		equivalent to: C {D C}*
;;    {C || D}*		equivalent to: [C {D C}*]
;;    {C || D}		equivalent to: [C {D C}*]
;;
;; The EBNF syntax written using the notation above is:
;;
;;    EBNF = {production}+.
;;
;;    production = non_terminal "=" body ".".   ;; production
;;
;;    body = {sequence || "|"}*.                ;; alternative
;;
;;    sequence = {exception}*.                  ;; sequence
;;
;;    exception = repeat [ "-" repeat].         ;; exception
;;
;;    repeat = [ integer "*" [ integer ]] term. ;; repetition
;;
;;    term = factor
;;         | [factor] "+"                       ;; one-or-more
;;         | [factor] "/" [factor]              ;; one-or-more
;;         .
;;
;;    factor = [ "$" ] "\"" terminal "\""       ;; terminal
;;           | [ "$" ] non_terminal             ;; non-terminal
;;           | [ "$" ] "?" special "?"          ;; special
;;           | "(" body ")"                     ;; group
;;           | "[" body "]"                     ;; zero-or-one
;;           | "{" body [ "||" body ] "}+"      ;; one-or-more
;;           | "{" body [ "||" body ] "}*"      ;; zero-or-more
;;           | "{" body [ "||" body ] "}"       ;; zero-or-more
;;           .
;;
;;    non_terminal = "[!#%&'*-,0-:<>@-Z\\\\^-z~\\240-\\377]+".
;;    ;; that is, a valid non_terminal accepts decimal digits, letters (upper
;;    ;; and lower), 8-bit accentuated characters,
;;    ;; "!", "#", "%", "&", "'", "*", "+", ",", ":",
;;    ;; "<", ">", "@", "\", "^", "_", "`" and "~".
;;
;;    terminal = "\\([^\"\\]\\|\\\\[ -~\\240-\\377]\\)+".
;;    ;; that is, a valid terminal accepts any printable character (including
;;    ;; 8-bit accentuated characters) except `"', as `"' is used to delimit a
;;    ;; terminal.  Also, accepts escaped characters, that is, a character
;;    ;; pair starting with `\' followed by a printable character, for
;;    ;; example: \", \\.
;;
;;    special = "[^?\\000-\\010\\012-\\037\\177-\\237]*".
;;    ;; that is, a valid special accepts any printable character (including
;;    ;; 8-bit accentuated characters) and tabs except `?', as `?' is used to
;;    ;; delimit a special.
;;
;;    integer = "[0-9]+".
;;    ;; that is, an integer is a sequence of one or more decimal digits.
;;
;;    comment = ";" "[^\\n\\000-\\010\\016-\\037\\177-\\237]*" "\\n".
;;    ;; that is, a comment starts with the character `;' and terminates at end
;;    ;; of line.  Also, it only accepts printable characters (including 8-bit
;;    ;; accentuated characters) and tabs.
;;
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:


(require 'ebnf-otz)


(defvar ebnf-bnf-lex nil
  "Value returned by `ebnf-bnf-lex' function.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntactic analyzer


;;; EBNF = {production}+.

(defun ebnf-bnf-parser (start)
  "EBNF parser."
  (let ((total (+ (- ebnf-limit start) 1))
	(bias (1- start))
	(origin (point))
	prod-list token rule)
    (goto-char start)
    (setq token (ebnf-bnf-lex))
    (and (eq token 'end-of-input)
	 (error "Invalid EBNF file format"))
    (while (not (eq token 'end-of-input))
      (ebnf-message-float
       "Parsing...%s%%"
       (/ (* (- (point) bias) 100.0) total))
      (setq token (ebnf-production token)
	    rule  (cdr token)
	    token (car token))
      (or (ebnf-add-empty-rule-list rule)
	  (setq prod-list (cons rule prod-list))))
    (goto-char origin)
    prod-list))


;;; production = non-terminal "=" body ".".

(defun ebnf-production (token)
  (let ((header ebnf-bnf-lex)
	(action ebnf-action)
	body)
    (setq ebnf-action nil)
    (or (eq token 'non-terminal)
	(error "Invalid header production"))
    (or (eq (ebnf-bnf-lex) 'equal)
	(error "Invalid production: missing `='"))
    (setq body (ebnf-body))
    (or (eq (car body) 'period)
	(error "Invalid production: missing `.'"))
    (setq body (cdr body))
    (ebnf-eps-add-production header)
    (cons (ebnf-bnf-lex)
	  (ebnf-make-production header body action))))


;;; body = {sequence || "|"}*.

(defun ebnf-body ()
  (let (body sequence)
    (while (eq (car (setq sequence (ebnf-sequence))) 'alternative)
      (setq sequence (cdr sequence)
	    body     (cons sequence body)))
    (ebnf-token-alternative body sequence)))


;;; sequence = {exception}*.

(defun ebnf-sequence ()
  (let ((token (ebnf-bnf-lex))
	seq term)
    (while (setq term  (ebnf-exception token)
		 token (car term)
		 term  (cdr term))
      (setq seq (cons term seq)))
    (cons token
	  (ebnf-token-sequence seq))))


;;; exception = repeat [ "-" repeat].

(defun ebnf-exception (token)
  (let ((term (ebnf-repeat token)))
    (if (not (eq (car term) 'except))
	;; repeat
	term
      ;; repeat - repeat
      (let ((exception (ebnf-repeat (ebnf-bnf-lex))))
	(ebnf-no-non-terminal (cdr exception))
	(ebnf-token-except (cdr term) exception)))))


(defun ebnf-no-non-terminal (node)
  (and (vectorp node)
       (let ((kind (ebnf-node-kind node)))
	 (cond
	  ((eq kind 'ebnf-generate-non-terminal)
	   (error "Exception sequence should not contain a non-terminal"))
	  ((eq kind 'ebnf-generate-repeat)
	   (ebnf-no-non-terminal (ebnf-node-separator node)))
	  ((memq kind '(ebnf-generate-optional ebnf-generate-except))
	   (ebnf-no-non-terminal (ebnf-node-list node)))
	  ((memq kind '(ebnf-generate-one-or-more ebnf-generate-zero-or-more))
	   (ebnf-no-non-terminal (ebnf-node-list node))
	   (ebnf-no-non-terminal (ebnf-node-separator node)))
	  ((memq kind '(ebnf-generate-alternative ebnf-generate-sequence))
	   (let ((seq (ebnf-node-list node)))
	     (while seq
	       (ebnf-no-non-terminal (car seq))
	       (setq seq (cdr seq)))))
	  ))))


;;; repeat = [ integer "*" [ integer ]] term.

(defun ebnf-repeat (token)
  (if (not (eq token 'integer))
      (ebnf-term token)
    (let ((times ebnf-bnf-lex)
	  upper)
      (or (eq (ebnf-bnf-lex) 'repeat)
	  (error "Missing `*'"))
      (setq token (ebnf-bnf-lex))
      (when (eq token 'integer)
	(setq upper ebnf-bnf-lex
	      token (ebnf-bnf-lex)))
      (ebnf-token-repeat times (ebnf-term token) upper))))


;;; term = factor
;;;      | [factor] "+"                       ;; one-or-more
;;;      | [factor] "/" [factor]              ;; one-or-more
;;;      .

(defun ebnf-term (token)
  (let ((factor (ebnf-factor token)))
    (and factor
	 (setq token (ebnf-bnf-lex)))
    (cond
     ;; [factor] +
     ((eq token 'one-or-more)
      (cons (ebnf-bnf-lex)
	    (and factor
		 (let ((kind (ebnf-node-kind factor)))
		   (cond
		    ;; { A }+ + ==> { A }+
		    ;; { A }* + ==> { A }*
		    ((memq kind '(ebnf-generate-zero-or-more
				  ebnf-generate-one-or-more))
		     factor)
		    ;; [ A ] + ==> { A }*
		    ((eq kind 'ebnf-generate-optional)
		     (ebnf-make-zero-or-more (list factor)))
		    ;; A +
		    (t
		     (ebnf-make-one-or-more (list factor)))
		    )))))
     ;; [factor] / [factor]
     ((eq token 'list)
      (setq token (ebnf-bnf-lex))
      (let ((sep (ebnf-factor token)))
	(and sep
	     (setq factor (or factor (ebnf-make-empty))))
	(cons (if sep
		  (ebnf-bnf-lex)
		token)
	      (and factor
		   (ebnf-make-one-or-more factor sep)))))
     ;; factor
     (t
      (cons token factor))
     )))


;;; factor = [ "$" ] "\"" terminal "\""         ;; terminal
;;;        | [ "$" ] non_terminal               ;; non-terminal
;;;        | [ "$" ] "?" special "?"            ;; special
;;;        | "(" body ")"                       ;; group
;;;        | "[" body "]"                       ;; zero-or-one
;;;        | "{" body [ "||" body ] "}+"        ;; one-or-more
;;;        | "{" body [ "||" body ] "}*"        ;; zero-or-more
;;;        | "{" body [ "||" body ] "}"         ;; zero-or-more
;;;        .

(defun ebnf-factor (token)
  (cond
   ;; terminal
   ((eq token 'terminal)
    (ebnf-make-terminal ebnf-bnf-lex))
   ;; non-terminal
   ((eq token 'non-terminal)
    (ebnf-make-non-terminal ebnf-bnf-lex))
   ;; special
   ((eq token 'special)
    (ebnf-make-special ebnf-bnf-lex))
   ;; group
   ((eq token 'begin-group)
    (let ((body (ebnf-body)))
      (or (eq (car body) 'end-group)
	  (error "Missing `)'"))
      (cdr body)))
   ;; optional
   ((eq token 'begin-optional)
    (let ((body (ebnf-body)))
      (or (eq (car body) 'end-optional)
	  (error "Missing `]'"))
      (ebnf-token-optional (cdr body))))
   ;; list
   ((eq token 'begin-list)
    (let* ((body      (ebnf-body))
	   (token     (car body))
	   (list-part (cdr body))
	   sep-part)
      (and (eq token 'list-separator)
	   ;; { A || B }
	   (setq body     (ebnf-body)	; get separator
		 token    (car body)
		 sep-part (cdr body)))
      (cond
       ;; { A }+
       ((eq token 'end-one-or-more)
	(ebnf-make-one-or-more list-part sep-part))
       ;; { A }*
       ((eq token 'end-zero-or-more)
	(ebnf-make-zero-or-more list-part sep-part))
       (t
	(error "Missing `}+', `}*' or `}'"))
       )))
   ;; no term
   (t
    nil)
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lexical analyzer


(defconst ebnf-bnf-token-table (make-vector 256 'error)
  "Vector used to map characters to a lexical token.")


(defun ebnf-bnf-initialize ()
  "Initialize EBNF token table."
  ;; control character & control 8-bit character are set to `error'
  (let ((char ?\040))
    ;; printable character:
    (while (< char ?\060)
      (aset ebnf-bnf-token-table char 'non-terminal)
      (setq char (1+ char)))
    ;; digits:
    (while (< char ?\072)
      (aset ebnf-bnf-token-table char 'integer)
      (setq char (1+ char)))
    ;; printable character:
    (while (< char ?\177)
      (aset ebnf-bnf-token-table char 'non-terminal)
      (setq char (1+ char)))
    ;; European 8-bit accentuated characters:
    (setq char ?\240)
    (while (< char ?\400)
      (aset ebnf-bnf-token-table char 'non-terminal)
      (setq char (1+ char)))
    ;; Override space characters:
    (aset ebnf-bnf-token-table ?\013 'space) ; [VT] vertical tab
    (aset ebnf-bnf-token-table ?\n   'space) ; [NL] linefeed
    (aset ebnf-bnf-token-table ?\r   'space) ; [CR] carriage return
    (aset ebnf-bnf-token-table ?\t   'space) ; [HT] horizontal tab
    (aset ebnf-bnf-token-table ?\    'space) ; [SP] space
    ;; Override form feed character:
    (aset ebnf-bnf-token-table ?\f 'form-feed) ; [FF] form feed
    ;; Override other lexical characters:
    (aset ebnf-bnf-token-table ?\" 'terminal)
    (aset ebnf-bnf-token-table ?\? 'special)
    (aset ebnf-bnf-token-table ?\( 'begin-group)
    (aset ebnf-bnf-token-table ?\) 'end-group)
    (aset ebnf-bnf-token-table ?*  'repeat)
    (aset ebnf-bnf-token-table ?-  'except)
    (aset ebnf-bnf-token-table ?=  'equal)
    (aset ebnf-bnf-token-table ?\[ 'begin-optional)
    (aset ebnf-bnf-token-table ?\] 'end-optional)
    (aset ebnf-bnf-token-table ?\{ 'begin-list)
    (aset ebnf-bnf-token-table ?|  'alternative)
    (aset ebnf-bnf-token-table ?\} 'end-list)
    (aset ebnf-bnf-token-table ?/  'list)
    (aset ebnf-bnf-token-table ?+  'one-or-more)
    (aset ebnf-bnf-token-table ?$  'default)
    ;; Override comment character:
    (aset ebnf-bnf-token-table ebnf-lex-comment-char 'comment)
    ;; Override end of production character:
    (aset ebnf-bnf-token-table ebnf-lex-eop-char     'period)))


;; replace the range "\240-\377" (see `ebnf-range-regexp').
(defconst ebnf-bnf-non-terminal-chars
  (ebnf-range-regexp "!#%&'*-,0-:<>@-Z\\\\^-z~" ?\240 ?\377))


(defun ebnf-bnf-lex ()
  "Lexical analyzer for EBNF.

Return a lexical token.

See documentation for variable `ebnf-bnf-lex'."
  (if (>= (point) ebnf-limit)
      'end-of-input
    (let (token)
      ;; skip spaces and comments
      (while (if (> (following-char) 255)
		 (progn
		   (setq token 'error)
		   nil)
	       (setq token (aref ebnf-bnf-token-table (following-char)))
	       (cond
		((eq token 'space)
		 (skip-chars-forward " \013\n\r\t" ebnf-limit)
		 (< (point) ebnf-limit))
		((eq token 'comment)
		 (ebnf-bnf-skip-comment))
		((eq token 'form-feed)
		 (forward-char)
		 (setq ebnf-action 'form-feed))
		(t nil)
		)))
      (setq ebnf-default-p nil)
      (cond
       ;; end of input
       ((>= (point) ebnf-limit)
	'end-of-input)
       ;; error
       ((eq token 'error)
	(error "Invalid character"))
       ;; default
       ((eq token 'default)
	(forward-char)
	(if (memq (aref ebnf-bnf-token-table (following-char))
		  '(terminal non-terminal special))
	    (prog1
		(ebnf-bnf-lex)
	      (setq ebnf-default-p t))
	  (error "Invalid `default' element")))
       ;; integer
       ((eq token 'integer)
	(setq ebnf-bnf-lex (ebnf-buffer-substring "0-9"))
	'integer)
       ;; special: ?special?
       ((eq token 'special)
	(setq ebnf-bnf-lex (concat (and ebnf-special-show-delimiter "?")
				   (ebnf-string " ->@-~" ?\? "special")
				   (and ebnf-special-show-delimiter "?")))
	'special)
       ;; terminal: "string"
       ((eq token 'terminal)
	(setq ebnf-bnf-lex (ebnf-unescape-string (ebnf-get-string)))
	'terminal)
       ;; non-terminal or terminal
       ((eq token 'non-terminal)
	(setq ebnf-bnf-lex (ebnf-buffer-substring ebnf-bnf-non-terminal-chars))
	(let ((case-fold-search ebnf-case-fold-search)
	      match)
	  (if (and ebnf-terminal-regexp
		   (setq match (string-match ebnf-terminal-regexp
					     ebnf-bnf-lex))
		   (zerop match)
		   (= (match-end 0) (length ebnf-bnf-lex)))
	      'terminal
	    'non-terminal)))
       ;; end of list: }+, }*, }
       ((eq token 'end-list)
	(forward-char)
	(cond
	 ((= (following-char) ?+)
	  (forward-char)
	  'end-one-or-more)
	 ((= (following-char) ?*)
	  (forward-char)
	  'end-zero-or-more)
	 (t
	  'end-zero-or-more)
	 ))
       ;; alternative: |, ||
       ((eq token 'alternative)
	(forward-char)
	(if (/= (following-char) ?|)
	    'alternative
	  (forward-char)
	  'list-separator))
       ;; miscellaneous: {, (, ), [, ], ., =, /, +, -, *
       (t
	(forward-char)
	token)
       ))))


;; replace the range "\177-\237" (see `ebnf-range-regexp').
(defconst ebnf-bnf-comment-chars
  (ebnf-range-regexp "^\n\000-\010\016-\037" ?\177 ?\237))


(defun ebnf-bnf-skip-comment ()
  (forward-char)
  (cond
   ;; open EPS file
   ((and ebnf-eps-executing (= (following-char) ?\[))
    (ebnf-eps-add-context (ebnf-bnf-eps-filename)))
   ;; close EPS file
   ((and ebnf-eps-executing (= (following-char) ?\]))
    (ebnf-eps-remove-context (ebnf-bnf-eps-filename)))
   ;; EPS header
   ((and ebnf-eps-executing (= (following-char) ?H))
    (ebnf-eps-header-comment (ebnf-bnf-eps-filename)))
   ;; EPS footer
   ((and ebnf-eps-executing (= (following-char) ?F))
    (ebnf-eps-footer-comment (ebnf-bnf-eps-filename)))
   ;; any other action in comment
   (t
    (setq ebnf-action (aref ebnf-comment-table (following-char)))
    (skip-chars-forward ebnf-bnf-comment-chars ebnf-limit))
   )
  ;; check for a valid end of comment
  (cond ((>= (point) ebnf-limit)
	 nil)
	((= (following-char) ?\n)
	 (forward-char)
	 t)
	(t
	 (error "Invalid character"))
	))


(defun ebnf-bnf-eps-filename ()
  (forward-char)
  (ebnf-buffer-substring ebnf-bnf-comment-chars))


(defun ebnf-unescape-string (str)
  (let* ((len (length str))
	 (size (1- len))
	 (istr 0)
	 (n-esc 0))
    ;; count number of escapes
    (while (< istr size)
      (setq istr (+ istr
		    (if (= (aref str istr) ?\\)
			(progn
			  (setq n-esc (1+ n-esc))
			  2)
		      1))))
    (if (zerop n-esc)
	;; no escapes
	str
      ;; at least one escape
      (let ((new (make-string (- len n-esc) ?\ ))
	    (inew 0))
	;; eliminate all escapes
	(setq istr 0)
	(while (> n-esc 0)
	  (and (= (aref str istr) ?\\)
	       (setq istr  (1+ istr)
		     n-esc (1- n-esc)))
	  (aset new inew (aref str istr))
	  (setq inew (1+ inew)
		istr (1+ istr)))
	;; remaining string has no escape
	(while (< istr len)
	  (aset new inew (aref str istr))
	  (setq inew (1+ inew)
		istr (1+ istr)))
	new))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'ebnf-bnf)


;;; ebnf-bnf.el ends here
