;;; ebnf-abn.el --- parser for ABNF (Augmented BNF)

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
;; This package defines a parser for ABNF (Augmented BNF).
;;
;; See ebnf2ps.el for documentation.
;;
;;
;; ABNF Syntax
;; -----------
;;
;;	See the URL:
;;	`http://www.ietf.org/rfc/rfc2234.txt'
;;	or
;;	`http://www.faqs.org/rfcs/rfc2234.html'
;;	or
;;	`http://www.rnp.br/ietf/rfc/rfc2234.txt'
;;	("Augmented BNF for Syntax Specifications: ABNF").
;;
;;
;;    rulelist       =  1*( rule / (*c-wsp c-nl) )
;;
;;    rule           =  rulename defined-as elements c-nl
;;			     ; continues if next line starts with white space
;;
;;    rulename       =  ALPHA *(ALPHA / DIGIT / "-")
;;
;;    defined-as     =  *c-wsp ("=" / "=/") *c-wsp
;;			     ; basic rules definition and incremental
;;			     ; alternatives
;;
;;    elements       =  alternation *c-wsp
;;
;;    c-wsp          =  WSP / (c-nl WSP)
;;
;;    c-nl           =  comment / CRLF
;;			     ; comment or newline
;;
;;    comment        =  ";" *(WSP / VCHAR) CRLF
;;
;;    alternation    =  concatenation
;;			*(*c-wsp "/" *c-wsp concatenation)
;;
;;    concatenation  =  repetition *(1*c-wsp repetition)
;;
;;    repetition     =  [repeat] element
;;
;;    repeat         =  1*DIGIT / (*DIGIT "*" *DIGIT)
;;
;;    element        =  rulename / group / option /
;;			char-val / num-val / prose-val
;;
;;    group          =  "(" *c-wsp alternation *c-wsp ")"
;;
;;    option         =  "[" *c-wsp alternation *c-wsp "]"
;;
;;    char-val       =  DQUOTE *(%x20-21 / %x23-7E) DQUOTE
;;			     ; quoted string of SP and VCHAR without DQUOTE
;;
;;    num-val        =  "%" (bin-val / dec-val / hex-val)
;;
;;    bin-val        =  "b" 1*BIT
;;			[ 1*("." 1*BIT) / ("-" 1*BIT) ]
;;			     ; series of concatenated bit values
;;			     ; or single ONEOF range
;;
;;    dec-val        =  "d" 1*DIGIT
;;			[ 1*("." 1*DIGIT) / ("-" 1*DIGIT) ]
;;
;;    hex-val        =  "x" 1*HEXDIG
;;			[ 1*("." 1*HEXDIG) / ("-" 1*HEXDIG) ]
;;
;;    prose-val      =  "<" *(%x20-3D / %x3F-7E) ">"
;;				; bracketed string of SP and VCHAR without
;;				; angles
;;				; prose description, to be used as last resort
;;
;;    ; Core rules -- the coding depends on the system, here is used 7-bit ASCII
;;
;;    ALPHA          =  %x41-5A / %x61-7A
;;				; A-Z / a-z
;;
;;    BIT            =  "0" / "1"
;;
;;    CHAR           =  %x01-7F
;;				; any 7-bit US-ASCII character, excluding NUL
;;
;;    CR             =  %x0D
;;				; carriage return
;;
;;    CRLF           =  CR LF
;;				; Internet standard newline
;;
;;    CTL            =  %x00-1F / %x7F
;;				; controls
;;
;;    DIGIT          =  %x30-39
;;				; 0-9
;;
;;    DQUOTE         =  %x22
;;				; " (Double Quote)
;;
;;    HEXDIG         =  DIGIT / "A" / "B" / "C" / "D" / "E" / "F"
;;
;;    HTAB           =  %x09
;;				; horizontal tab
;;
;;    LF             =  %x0A
;;				; linefeed
;;
;;    LWSP           =  *(WSP / CRLF WSP)
;;				; linear white space (past newline)
;;
;;    OCTET          =  %x00-FF
;;				; 8 bits of data
;;
;;    SP             =  %x20
;;				; space
;;
;;    VCHAR          =  %x21-7E
;;				; visible (printing) characters
;;
;;    WSP            =  SP / HTAB
;;				; white space
;;
;;
;; NOTES:
;;
;; 1. Rules name and terminal strings are case INSENSITIVE.
;;    So, the following rule names are all equals:
;;	 Rule-name,  rule-Name, rule-name, RULE-NAME
;;    Also, the following strings are equals:
;;	 "abc", "ABC", "aBc", "Abc", "aBC", etc.
;;
;; 2. To have a case SENSITIVE string, use the character notation.
;;    For example, to specify the lowercase string "abc", use:
;;	 %d97.98.99
;;
;; 3. There are no implicit spaces between elements, for example, the
;;    following rules:
;;
;;	 foo = %x61  ; a
;;
;;	 bar = %x62  ; b
;;
;;	 mumble = foo bar foo
;;
;;    Are equivalent to the following rule:
;;
;;	 mumble = %x61.62.61
;;
;;    If spaces are needed, it should be explicit specified, like:
;;
;;	spaces = 1*(%x20 / %x09)  ; one or more spaces or tabs
;;
;;	mumble = foo spaces bar spaces foo
;;
;; 4. Lines starting with space or tab are considered a continuation line.
;;    For example, the rule:
;;
;;	 rule = foo
;;	        bar
;;
;;    Is equivalent to:
;;
;;	 rule = foo bar
;;
;;
;; Differences Between ABNF And ebnf2ps ABNF
;; -----------------------------------------
;;
;; Besides the characters that ABNF accepts, ebnf2ps ABNF accepts also the
;; underscore (_) for rule name and european 8-bit accentuated characters (from
;; \240 to \377) for rule name, string and comment.
;;
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:


(require 'ebnf-otz)


(defvar ebnf-abn-lex nil
  "Value returned by `ebnf-abn-lex' function.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntactic analyzer


;;;    rulelist       =  1*( rule / (*c-wsp c-nl) )

(defun ebnf-abn-parser (start)
  "ABNF parser."
  (let ((total (+ (- ebnf-limit start) 1))
	(bias (1- start))
	(origin (point))
	rule-list token rule)
    (goto-char start)
    (setq token (ebnf-abn-lex))
    (and (eq token 'end-of-input)
	 (error "Invalid ABNF file format"))
    (and (eq token 'end-of-rule)
	 (setq token (ebnf-abn-lex)))
    (while (not (eq token 'end-of-input))
      (ebnf-message-float
       "Parsing...%s%%"
       (/ (* (- (point) bias) 100.0) total))
      (setq token (ebnf-abn-rule token)
	    rule  (cdr token)
	    token (car token))
      (or (ebnf-add-empty-rule-list rule)
	  (setq rule-list (cons rule rule-list))))
    (goto-char origin)
    rule-list))


;;;    rule           =  rulename defined-as elements c-nl
;;;			     ; continues if next line starts with white space
;;;
;;;    rulename       =  ALPHA *(ALPHA / DIGIT / "-")
;;;
;;;    defined-as     =  *c-wsp ("=" / "=/") *c-wsp
;;;			     ; basic rules definition and incremental
;;;			     ; alternatives
;;;
;;;    elements       =  alternation *c-wsp
;;;
;;;    c-wsp          =  WSP / (c-nl WSP)
;;;
;;;    c-nl           =  comment / CRLF
;;;			     ; comment or newline
;;;
;;;    comment        =  ";" *(WSP / VCHAR) CRLF


(defun ebnf-abn-rule (token)
  (let ((name ebnf-abn-lex)
	(action ebnf-action)
	elements)
    (setq ebnf-action nil)
    (or (eq token 'non-terminal)
	(error "Invalid rule name"))
    (setq token (ebnf-abn-lex))
    (or (memq token '(equal incremental-alternative))
	(error "Invalid rule: missing `=' or `=/'"))
    (and (eq token 'incremental-alternative)
	 (setq name (concat name " =/")))
    (setq elements (ebnf-abn-alternation))
    (or (memq (car elements) '(end-of-rule end-of-input))
	(error "Invalid rule: there is no end of rule"))
    (setq elements (cdr elements))
    (ebnf-eps-add-production name)
    (cons (ebnf-abn-lex)
	  (ebnf-make-production name elements action))))


;;;    alternation    =  concatenation
;;;			 *(*c-wsp "/" *c-wsp concatenation)


(defun ebnf-abn-alternation ()
  (let (body concatenation)
    (while (eq (car (setq concatenation
			  (ebnf-abn-concatenation (ebnf-abn-lex))))
	       'alternative)
      (setq body (cons (cdr concatenation) body)))
    (ebnf-token-alternative body concatenation)))


;;;    concatenation  =  repetition *(1*c-wsp repetition)


(defun ebnf-abn-concatenation (token)
  (let ((term (ebnf-abn-repetition token))
	seq)
    (or (setq token (car term)
	      term  (cdr term))
	(error "Empty element"))
    (setq seq (cons term seq))
    (while (setq term  (ebnf-abn-repetition token)
		 token (car term)
		 term  (cdr term))
      (setq seq (cons term seq)))
    (cons token
	  (ebnf-token-sequence seq))))


;;;    repetition     =  [repeat] element
;;;
;;;    repeat         =  1*DIGIT / (*DIGIT "*" *DIGIT)


(defun ebnf-abn-repetition (token)
  (let (lower upper)
    ;; INTEGER [ "*" [ INTEGER ] ]
    (when (eq token 'integer)
      (setq lower ebnf-abn-lex
	    token (ebnf-abn-lex))
      (or (eq token 'repeat)
	  (setq upper lower)))
    ;; "*" [ INTEGER ]
    (when (eq token 'repeat)
      ;; only * ==> lower & upper are empty string
      (or lower
	  (setq lower ""
		upper ""))
      (when (eq (setq token (ebnf-abn-lex)) 'integer)
	(setq upper ebnf-abn-lex
	      token (ebnf-abn-lex))))
    (let ((element (ebnf-abn-element token)))
      (cond
       ;; there is a repetition
       (lower
	(or element
	    (error "Missing element repetition"))
	(setq token (ebnf-abn-lex))
	(cond
	 ;; one or more
	 ((and (string= lower "1") (null upper))
	  (cons token (ebnf-make-one-or-more element)))
	 ;; zero or more
	 ((or (and (string= lower "0") (null upper))
	      (and (string= lower "") (string= upper "")))
	  (cons token (ebnf-make-zero-or-more element)))
	 ;; real repetition
	 (t
	  (ebnf-token-repeat lower (cons token element) upper))))
       ;; there is an element
       (element
	(cons (ebnf-abn-lex) element))
       ;; something that caller has to deal
       (t
	(cons token nil))))))


;;;    element        =  rulename / group / option /
;;;			char-val / num-val / prose-val
;;;
;;;    group          =  "(" *c-wsp alternation *c-wsp ")"
;;;
;;;    option         =  "[" *c-wsp alternation *c-wsp "]"
;;;
;;;    char-val       =  DQUOTE *(%x20-21 / %x23-7E) DQUOTE
;;;			     ; quoted string of SP and VCHAR without DQUOTE
;;;
;;;    num-val        =  "%" (bin-val / dec-val / hex-val)
;;;
;;;    bin-val        =  "b" 1*BIT
;;;			[ 1*("." 1*BIT) / ("-" 1*BIT) ]
;;;			     ; series of concatenated bit values
;;;			     ; or single ONEOF range
;;;
;;;    dec-val        =  "d" 1*DIGIT
;;;			[ 1*("." 1*DIGIT) / ("-" 1*DIGIT) ]
;;;
;;;    hex-val        =  "x" 1*HEXDIG
;;;			[ 1*("." 1*HEXDIG) / ("-" 1*HEXDIG) ]
;;;
;;;    prose-val      =  "<" *(%x20-3D / %x3F-7E) ">"
;;;				; bracketed string of SP and VCHAR without
;;;				; angles
;;;				; prose description, to be used as last resort


(defun ebnf-abn-element (token)
  (cond
   ;; terminal
   ((eq token 'terminal)
    (ebnf-make-terminal ebnf-abn-lex))
   ;; non-terminal
   ((eq token 'non-terminal)
    (ebnf-make-non-terminal ebnf-abn-lex))
   ;; group
   ((eq token 'begin-group)
    (let ((body (ebnf-abn-alternation)))
      (or (eq (car body) 'end-group)
	  (error "Missing `)'"))
      (cdr body)))
   ;; optional
   ((eq token 'begin-optional)
    (let ((body (ebnf-abn-alternation)))
      (or (eq (car body) 'end-optional)
	  (error "Missing `]'"))
      (ebnf-token-optional (cdr body))))
   ;; no element
   (t
    nil)
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lexical analyzer


(defconst ebnf-abn-token-table (make-vector 256 'error)
  "Vector used to map characters to a lexical token.")


(defun ebnf-abn-initialize ()
  "Initialize EBNF token table."
  ;; control character & control 8-bit character are set to `error'
  (let ((char ?\060))
    ;; digits: 0-9
    (while (< char ?\072)
      (aset ebnf-abn-token-table char 'integer)
      (setq char (1+ char)))
    ;; printable character: A-Z
    (setq char ?\101)
    (while (< char ?\133)
      (aset ebnf-abn-token-table char 'non-terminal)
      (setq char (1+ char)))
    ;; printable character: a-z
    (setq char ?\141)
    (while (< char ?\173)
      (aset ebnf-abn-token-table char 'non-terminal)
      (setq char (1+ char)))
    ;; European 8-bit accentuated characters:
    (setq char ?\240)
    (while (< char ?\400)
      (aset ebnf-abn-token-table char 'non-terminal)
      (setq char (1+ char)))
    ;; Override end of line characters:
    (aset ebnf-abn-token-table ?\n 'end-of-rule) ; [NL] linefeed
    (aset ebnf-abn-token-table ?\r 'end-of-rule) ; [CR] carriage return
    ;; Override space characters:
    (aset ebnf-abn-token-table ?\013 'space) ; [VT] vertical tab
    (aset ebnf-abn-token-table ?\t   'space) ; [HT] horizontal tab
    (aset ebnf-abn-token-table ?\    'space) ; [SP] space
    ;; Override form feed character:
    (aset ebnf-abn-token-table ?\f 'form-feed) ; [FF] form feed
    ;; Override other lexical characters:
    (aset ebnf-abn-token-table ?<  'non-terminal)
    (aset ebnf-abn-token-table ?%  'terminal)
    (aset ebnf-abn-token-table ?\" 'terminal)
    (aset ebnf-abn-token-table ?\( 'begin-group)
    (aset ebnf-abn-token-table ?\) 'end-group)
    (aset ebnf-abn-token-table ?*  'repeat)
    (aset ebnf-abn-token-table ?=  'equal)
    (aset ebnf-abn-token-table ?\[ 'begin-optional)
    (aset ebnf-abn-token-table ?\] 'end-optional)
    (aset ebnf-abn-token-table ?/  'alternative)
    ;; Override comment character:
    (aset ebnf-abn-token-table ?\; 'comment)))


;; replace the range "\240-\377" (see `ebnf-range-regexp').
(defconst ebnf-abn-non-terminal-chars
  (ebnf-range-regexp "-_0-9A-Za-z" ?\240 ?\377))
(defconst ebnf-abn-non-terminal-letter-chars
  (ebnf-range-regexp "A-Za-z" ?\240 ?\377))


(defun ebnf-abn-lex ()
  "Lexical analyzer for ABNF.

Return a lexical token.

See documentation for variable `ebnf-abn-lex'."
  (if (>= (point) ebnf-limit)
      'end-of-input
    (let (token)
      ;; skip spaces and comments
      (while (if (> (following-char) 255)
		 (progn
		   (setq token 'error)
		   nil)
	       (setq token (aref ebnf-abn-token-table (following-char)))
	       (cond
		((eq token 'space)
		 (skip-chars-forward " \013\t" ebnf-limit)
		 (< (point) ebnf-limit))
		((eq token 'comment)
		 (ebnf-abn-skip-comment))
		((eq token 'form-feed)
		 (forward-char)
		 (setq ebnf-action 'form-feed))
		((eq token 'end-of-rule)
		 (ebnf-abn-skip-end-of-rule))
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
       ;; integer
       ((eq token 'integer)
	(setq ebnf-abn-lex (ebnf-buffer-substring "0-9"))
	'integer)
       ;; terminal: "string" or %[bdx]NNN((.NNN)+|-NNN)?
       ((eq token 'terminal)
	(setq ebnf-abn-lex
	      (if (= (following-char) ?\")
		  (ebnf-abn-string)
		(ebnf-abn-character)))
	'terminal)
       ;; non-terminal: NAME or <NAME>
       ((eq token 'non-terminal)
	(let ((prose-p (= (following-char) ?<)))
	  (when prose-p
	    (forward-char)
	    (or (looking-at ebnf-abn-non-terminal-letter-chars)
		(error "Invalid prose value")))
	  (setq ebnf-abn-lex
		(ebnf-buffer-substring ebnf-abn-non-terminal-chars))
	  (when prose-p
	    (or (= (following-char) ?>)
		(error "Invalid prose value"))
	    (setq ebnf-abn-lex (concat "<" ebnf-abn-lex ">"))))
	'non-terminal)
       ;; equal: =, =/
       ((eq token 'equal)
	(forward-char)
	(if (/= (following-char) ?/)
	    'equal
	  (forward-char)
	  'incremental-alternative))
       ;; miscellaneous: (, ), [, ], /, *
       (t
	(forward-char)
	token)
       ))))


(defun ebnf-abn-skip-end-of-rule ()
  (let (eor-p)
    (while (progn
	     ;; end of rule ==> 2 or more consecutive end of lines
	     (setq eor-p (or (> (skip-chars-forward "\r\n" ebnf-limit) 1)
			     eor-p))
	     ;; skip spaces
	     (skip-chars-forward " \013\t" ebnf-limit)
	     ;; skip comments
	     (and (= (following-char) ?\;)
		  (ebnf-abn-skip-comment))))
    (not eor-p)))


;; replace the range "\177-\237" (see `ebnf-range-regexp').
(defconst ebnf-abn-comment-chars
  (ebnf-range-regexp "^\n\000-\010\016-\037" ?\177 ?\237))


(defun ebnf-abn-skip-comment ()
  (forward-char)
  (cond
   ;; open EPS file
   ((and ebnf-eps-executing (= (following-char) ?\[))
    (ebnf-eps-add-context (ebnf-abn-eps-filename)))
   ;; close EPS file
   ((and ebnf-eps-executing (= (following-char) ?\]))
    (ebnf-eps-remove-context (ebnf-abn-eps-filename)))
   ;; EPS header
   ((and ebnf-eps-executing (= (following-char) ?H))
    (ebnf-eps-header-comment (ebnf-abn-eps-filename)))
   ;; EPS footer
   ((and ebnf-eps-executing (= (following-char) ?F))
    (ebnf-eps-footer-comment (ebnf-abn-eps-filename)))
   ;; any other action in comment
   (t
    (setq ebnf-action (aref ebnf-comment-table (following-char)))
    (skip-chars-forward ebnf-abn-comment-chars ebnf-limit))
   )
  ;; check for a valid end of comment
  (cond ((>= (point) ebnf-limit)
	 nil)
	((= (following-char) ?\n)
	 t)
	(t
	 (error "Invalid character"))
	))


(defun ebnf-abn-eps-filename ()
  (forward-char)
  (ebnf-buffer-substring ebnf-abn-comment-chars))


;; replace the range "\240-\377" (see `ebnf-range-regexp').
(defconst ebnf-abn-string-chars
  (ebnf-range-regexp " -!#-~" ?\240 ?\377))


(defun ebnf-abn-string ()
  (buffer-substring-no-properties
   (progn
     (forward-char)
     (point))
   (progn
     (skip-chars-forward ebnf-abn-string-chars ebnf-limit)
     (or (= (following-char) ?\")
	 (error "Missing `\"'"))
     (prog1
	 (point)
       (forward-char)))))


(defun ebnf-abn-character ()
  ;; %[bdx]NNN((-NNN)|(.NNN)+)?
  (buffer-substring-no-properties
   (point)
   (progn
     (forward-char)
     (let* ((char  (following-char))
	    (chars (cond ((or (= char ?B) (= char ?b)) "01")
			 ((or (= char ?D) (= char ?d)) "0-9")
			 ((or (= char ?X) (= char ?x)) "0-9A-Fa-f")
			 (t (error "Invalid terminal value")))))
       (forward-char)
       (or (> (skip-chars-forward chars ebnf-limit) 0)
	   (error "Invalid terminal value"))
       (if (= (following-char) ?-)
	   (progn
	     (forward-char)
	     (or (> (skip-chars-forward chars ebnf-limit) 0)
		 (error "Invalid terminal value range")))
	 (while (= (following-char) ?.)
	   (forward-char)
	   (or (> (skip-chars-forward chars ebnf-limit) 0)
	       (error "Invalid terminal value")))))
     (point))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'ebnf-abn)

;;; ebnf-abn.el ends here
