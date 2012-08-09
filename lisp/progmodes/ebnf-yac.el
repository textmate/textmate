;;; ebnf-yac.el --- parser for Yacc/Bison

;; Copyright (C) 1999-2012  Free Software Foundation, Inc.

;; Author: Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Maintainer: Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Keywords: wp, ebnf, PostScript
;; Version: 1.4
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
;; This package defines a parser for Yacc/Bison.
;;
;; See ebnf2ps.el for documentation.
;;
;;
;; Yacc/Bison Syntax
;; -----------------
;;
;; YACC = { YACC-Definitions }* "%%" { YACC-Rule }* [ "%%" [ YACC-Code ] ].
;;
;; YACC-Definitions = ( "%token" | "%left" | "%right" | "%nonassoc" )
;;                    [ "<" Name ">" ] Name-List
;;                  | "%prec" Name
;;                  | "any other Yacc definition"
;;                  .
;;
;; YACC-Code = "any C definition".
;;
;; YACC-Rule = Name ":" Alternative ";".
;;
;; Alternative = { Sequence || "|" }*.
;;
;; Sequence = { Factor }*.
;;
;; Factor = Name
;;        | "'" "character" "'"
;;        | "error"
;;        | "{" "C like commands" "}"
;;        .
;;
;; Name-List = { Name || "," }*.
;;
;; Name = "[A-Za-z][A-Za-z0-9_.]*".
;;
;; Comment = "/*" "any character, but the sequence \"*/\"" "*/"
;;         | "//" "any character, but the newline \"\\n\"" "\\n".
;;
;;
;; In other words, a valid Name begins with a letter (upper or lower case)
;; followed by letters, decimal digits, underscore (_) or point (.).  For
;; example: this_is_a_valid.name, Another_EXAMPLE, mIxEd.CaSe.
;;
;;
;; Acknowledgements
;; ----------------
;;
;; Thanks to Matthew K. Junker <junker@alum.mit.edu> for the suggestion to deal
;; with %right, %left and %prec pragmas.  His suggestion was extended to deal
;; with %nonassoc pragma too.
;;
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:


(require 'ebnf-otz)


(defvar ebnf-yac-lex nil
  "Value returned by `ebnf-yac-lex' function.")


(defvar ebnf-yac-token-list nil
  "List of `%TOKEN' names.")


(defvar ebnf-yac-skip-char nil
  "Non-nil means skip printable characters with no grammatical meaning.")


(defvar ebnf-yac-error nil
  "Non-nil means \"error\" occurred.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntactic analyzer


;;; YACC = { YACC-Definitions }* "%%" { YACC-Rule }* [ "%%" [ YACC-Code ] ].
;;;
;;; YACC-Code = "any C definition".

(defun ebnf-yac-parser (start)
  "yacc/Bison parser."
  (let ((total (+ (- ebnf-limit start) 1))
	(bias (1- start))
	(origin (point))
	syntax-list token rule)
    (goto-char start)
    (setq token (ebnf-yac-lex))
    (and (eq token 'end-of-input)
	 (error "Invalid Yacc/Bison file format"))
    (or (eq (ebnf-yac-definitions token) 'yac-separator)
	(error "Missing `%%%%'"))
    (setq token (ebnf-yac-lex))
    (while (not (memq token '(end-of-input yac-separator)))
      (ebnf-message-float
       "Parsing...%s%%"
       (/ (* (- (point) bias) 100.0) total))
      (setq token (ebnf-yac-rule token)
	    rule  (cdr token)
	    token (car token))
      (or (ebnf-add-empty-rule-list rule)
	  (setq syntax-list (cons rule syntax-list))))
    (goto-char origin)
    syntax-list))


;;; YACC-Definitions = ( "%token" | "%left" | "%right" | "%nonassoc" )
;;;                    [ "<" Name ">" ] Name-List
;;;                  | "%prec" Name
;;;                  | "any other Yacc definition"
;;;                  .

(defun ebnf-yac-definitions (token)
  (let ((ebnf-yac-skip-char t))
    (while (not (memq token '(yac-separator end-of-input)))
      (setq token
	    (cond
	     ;; ( "%token" | "%left" | "%right" | "%nonassoc" )
	     ;; [ "<" Name ">" ] Name-List
	     ((eq token 'yac-token)
	      (setq token (ebnf-yac-lex))
	      (when (eq token 'open-angle)
		(or (eq (ebnf-yac-lex) 'non-terminal)
		    (error "Missing type name"))
		(or (eq (ebnf-yac-lex) 'close-angle)
		    (error "Missing `>'"))
		(setq token (ebnf-yac-lex)))
	      (setq token               (ebnf-yac-name-list token)
		    ebnf-yac-token-list (nconc (cdr token)
					       ebnf-yac-token-list))
	      (car token))
	     ;;  "%prec" Name
	     ((eq token 'yac-prec)
	      (or (eq (ebnf-yac-lex) 'non-terminal)
		  (error "Missing prec name"))
	      (ebnf-yac-lex))
	     ;;  "any other Yacc definition"
	     (t
	      (ebnf-yac-lex))
	     )))
    token))


;;; YACC-Rule = Name ":" Alternative ";".

(defun ebnf-yac-rule (token)
  (let ((header ebnf-yac-lex)
	(action ebnf-action)
	body)
    (setq ebnf-action nil)
    (or (eq token 'non-terminal)
	(error "Invalid rule name"))
    (or (eq (ebnf-yac-lex) 'colon)
	(error "Invalid rule: missing `:'"))
    (setq body (ebnf-yac-alternative))
    (or (eq (car body) 'period)
	(error "Invalid rule: missing `;'"))
    (setq body (cdr body))
    (ebnf-eps-add-production header)
    (cons (ebnf-yac-lex)
	  (ebnf-make-production header body action))))


;;; Alternative = { Sequence || "|" }*.

(defun ebnf-yac-alternative ()
  (let (body sequence)
    (while (eq (car (setq sequence (ebnf-yac-sequence)))
	       'alternative)
      (and (setq sequence (cdr sequence))
	   (setq body     (cons sequence body))))
    (ebnf-token-alternative body sequence)))


;;; Sequence = { Factor }*.

(defun ebnf-yac-sequence ()
  (let (ebnf-yac-error token seq factor)
    (while (setq token  (ebnf-yac-lex)
		 factor (ebnf-yac-factor token))
      (setq seq (cons factor seq)))
    (cons token
	  (if (and ebnf-yac-ignore-error-recovery ebnf-yac-error)
	      ;; ignore error recovery
	      nil
	    (ebnf-token-sequence seq)))))


;;; Factor = Name
;;;        | "'" "character" "'"
;;;        | "error"
;;;        | "{" "C like commands" "}"
;;;        .

(defun ebnf-yac-factor (token)
  (cond
   ;; 'character'
   ((eq token 'terminal)
    (ebnf-make-terminal ebnf-yac-lex))
   ;; Name
   ((eq token 'non-terminal)
    (ebnf-make-non-terminal ebnf-yac-lex))
   ;; "error"
   ((eq token 'yac-error)
    (ebnf-make-special ebnf-yac-lex))
   ;; not a factor
   (t
    nil)
   ))


;;; Name-List = { Name || "," }*.

(defun ebnf-yac-name-list (token)
  (let (names)
    (when (eq token 'non-terminal)
      (while (progn
	       (setq names (cons ebnf-yac-lex names)
		     token (ebnf-yac-lex))
	       (eq token 'comma))
	(or (eq (ebnf-yac-lex) 'non-terminal)
	    (error "Missing token name"))))
    (cons token names)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lexical analyzer


;;; Name = "[A-Za-z][A-Za-z0-9_.]*".
;;;
;;; Comment = "/*" "any character, but the sequence \"*/\"" "*/"
;;;         | "//" "any character" "\\n".

(defconst ebnf-yac-token-table
  ;; control character & 8-bit character are set to `error'
  (let ((table (make-vector 256 'error)))
    ;; upper & lower case letters:
    (mapc
     #'(lambda (char)
	 (aset table char 'non-terminal))
     "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
    ;; printable characters:
    (mapc
     #'(lambda (char)
	 (aset table char 'character))
     "!#$&()*+-.0123456789=?@[\\]^_`~")
    ;; Override space characters:
    (aset table ?\n 'space)		; [NL] linefeed
    (aset table ?\r 'space)		; [CR] carriage return
    (aset table ?\t 'space)		; [HT] horizontal tab
    (aset table ?\  'space)		; [SP] space
    ;; Override form feed character:
    (aset table ?\f 'form-feed)		; [FF] form feed
    ;; Override other lexical characters:
    (aset table ?<  'open-angle)
    (aset table ?>  'close-angle)
    (aset table ?,  'comma)
    (aset table ?%  'yac-pragma)
    (aset table ?/  'slash)
    (aset table ?\{ 'yac-code)
    (aset table ?\" 'string)
    (aset table ?\' 'terminal)
    (aset table ?:  'colon)
    (aset table ?|  'alternative)
    (aset table ?\; 'period)
    table)
  "Vector used to map characters to a lexical token.")


(defun ebnf-yac-initialize ()
  "Initializations for Yacc/Bison parser."
  (setq ebnf-yac-token-list nil))


(defun ebnf-yac-lex ()
  "Lexical analyzer for Yacc/Bison.

Return a lexical token.

See documentation for variable `ebnf-yac-lex'."
  (if (>= (point) ebnf-limit)
      'end-of-input
    (let (token)
      ;; skip spaces, code blocks and comments
      (while (if (> (following-char) 255)
		 (progn
		   (setq token 'error)
		   nil)
	       (setq token (aref ebnf-yac-token-table (following-char)))
	       (cond
		((or (eq token 'space)
		     (and ebnf-yac-skip-char
			  (eq token 'character)))
		 (ebnf-yac-skip-spaces))
		((eq token 'yac-code)
		 (ebnf-yac-skip-code))
		((eq token 'slash)
		 (ebnf-yac-handle-comment))
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
       ;; "string"
       ((eq token 'string)
	(setq ebnf-yac-lex (ebnf-get-string))
	'string)
       ;; terminal: 'char'
       ((eq token 'terminal)
	(setq ebnf-yac-lex (ebnf-string " -&(-~" ?\' "terminal"))
	'terminal)
       ;; non-terminal, terminal or "error"
       ((eq token 'non-terminal)
	(setq ebnf-yac-lex (ebnf-buffer-substring "0-9A-Za-z_."))
	(cond ((member ebnf-yac-lex ebnf-yac-token-list)
	       'terminal)
	      ((string= ebnf-yac-lex "error")
	       (setq ebnf-yac-error t)
	       'yac-error)
	      (t
	       'non-terminal)
	      ))
       ;; %% and Yacc pragmas (%TOKEN, %START, etc).
       ((eq token 'yac-pragma)
	(forward-char)
	(cond
	 ;; Yacc separator
	 ((eq (following-char) ?%)
	  (forward-char)
	  'yac-separator)
	 ;; %TOKEN, %RIGHT, %LEFT,  %PREC, %NONASSOC
	 ((cdr (assoc (upcase (ebnf-buffer-substring "0-9A-Za-z_"))
		      '(("TOKEN"    . yac-token)
			("RIGHT"    . yac-token)
			("LEFT"     . yac-token)
			("NONASSOC" . yac-token)
			("PREC"     . yac-prec)))))
	 ;; other Yacc pragmas
	 (t
	  'yac-pragma)
	 ))
       ;; miscellaneous
       (t
	(forward-char)
	token)
       ))))


(defun ebnf-yac-skip-spaces ()
  (skip-chars-forward
   (if ebnf-yac-skip-char
       "\n\r\t !#$&()*+-.0123456789=?@[\\\\]^_`~"
     "\n\r\t ")
   ebnf-limit)
  (< (point) ebnf-limit))


;; replace the range "\177-\377" (see `ebnf-range-regexp').
(defconst ebnf-yac-skip-chars
  (ebnf-range-regexp "^{}/'\"\000-\010\013\016-\037" ?\177 ?\377))


(defun ebnf-yac-skip-code ()
  (forward-char)
  (let ((pair 1))
    (while (> pair 0)
      (skip-chars-forward ebnf-yac-skip-chars ebnf-limit)
      (cond
       ((= (following-char) ?{)
	(forward-char)
	(setq pair (1+ pair)))
       ((= (following-char) ?})
	(forward-char)
	(setq pair (1- pair)))
       ((= (following-char) ?/)
	(ebnf-yac-handle-comment))
       ((= (following-char) ?\")
	(ebnf-get-string))
       ((= (following-char) ?\')
	(ebnf-string " -&(-~" ?\' "character"))
       (t
	(error "Invalid character"))
       )))
  (ebnf-yac-skip-spaces))


(defun ebnf-yac-handle-comment ()
  (forward-char)
  (cond
   ;; begin comment
   ((= (following-char) ?*)
    (ebnf-yac-skip-comment)
    (ebnf-yac-skip-spaces))
   ;; line comment
   ((= (following-char) ?/)
    (end-of-line)
    (ebnf-yac-skip-spaces))
   ;; no comment
   (t nil)
   ))


;; replace the range "\177-\237" (see `ebnf-range-regexp').
(defconst ebnf-yac-comment-chars
  (ebnf-range-regexp "^*\000-\010\013\016-\037" ?\177 ?\237))


(defun ebnf-yac-skip-comment ()
  (forward-char)
  (cond
   ;; open EPS file
   ((and ebnf-eps-executing (= (following-char) ?\[))
    (ebnf-eps-add-context (ebnf-yac-eps-filename)))
   ;; close EPS file
   ((and ebnf-eps-executing (= (following-char) ?\]))
    (ebnf-eps-remove-context (ebnf-yac-eps-filename)))
   ;; EPS header
   ((and ebnf-eps-executing (= (following-char) ?H))
    (ebnf-eps-header-comment (ebnf-yac-eps-filename)))
   ;; EPS footer
   ((and ebnf-eps-executing (= (following-char) ?F))
    (ebnf-eps-footer-comment (ebnf-yac-eps-filename)))
   ;; any other action in comment
   (t
    (setq ebnf-action (aref ebnf-comment-table (following-char))))
   )
  (let ((not-end t))
    (while not-end
      (skip-chars-forward ebnf-yac-comment-chars ebnf-limit)
      (cond ((>= (point) ebnf-limit)
	     (error "Missing end of comment: `*/'"))
	    ((= (following-char) ?*)
	     (skip-chars-forward "*" ebnf-limit)
	     (when (= (following-char) ?/)
	       ;; end of comment
	       (forward-char)
	       (setq not-end nil)))
	    (t
	     (error "Invalid character"))
	    ))))


(defun ebnf-yac-eps-filename ()
  (forward-char)
  (buffer-substring-no-properties
   (point)
   (let ((chars (concat ebnf-yac-comment-chars "\n"))
	 found)
     (while (not found)
       (skip-chars-forward chars ebnf-limit)
       (setq found
	     (cond ((>= (point) ebnf-limit)
		    (point))
		   ((= (following-char) ?*)
		    (skip-chars-forward "*" ebnf-limit)
		    (if (/= (following-char) ?\/)
			nil
		      (backward-char)
		      (point)))
		   (t
		    (point))
		   )))
     found)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'ebnf-yac)


;;; ebnf-yac.el ends here
