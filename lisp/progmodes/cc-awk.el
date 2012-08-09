;;; cc-awk.el --- AWK specific code within cc-mode.

;; Copyright (C) 1988, 1994, 1996, 2000-2012  Free Software Foundation, Inc.

;; Author: Alan Mackenzie <acm@muc.de> (originally based on awk-mode.el)
;; Maintainer: FSF
;; Keywords: AWK, cc-mode, unix, languages
;; Package: cc-mode

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

;; This file contains (most of) the adaptations to cc-mode required for the
;; integration of AWK Mode.
;; It is organized thusly, the sections being separated by page breaks:
;;   1. The AWK Mode syntax table.
;;   2. Regular expressions for analyzing AWK code.
;;   3. Indentation calculation stuff ("c-awk-NL-prop text-property").
;;   4. Syntax-table property/font-locking stuff, including the
;;      font-lock-keywords setting.
;;   5. The AWK Mode before/after-change-functions.
;;   6. AWK Mode specific versions of commands like beginning-of-defun.
;; The AWK Mode keymap, abbreviation table, and the mode function itself are
;; in cc-mode.el.

;;; Code:

(eval-when-compile
  (let ((load-path
	 (if (and (boundp 'byte-compile-dest-file)
		  (stringp byte-compile-dest-file))
	     (cons (file-name-directory byte-compile-dest-file) load-path)
	   load-path)))
    (load "cc-bytecomp" nil t)))

(cc-require 'cc-defs)

;; Silence the byte compiler.
(cc-bytecomp-defvar font-lock-mode)	; Checked with boundp before use.
(cc-bytecomp-defvar c-new-BEG)
(cc-bytecomp-defvar c-new-END)

;; Some functions in cc-engine that are used below.  There's a cyclic
;; dependency so it can't be required here.  (Perhaps some functions
;; could be moved to cc-engine to avoid it.)
(cc-bytecomp-defun c-backward-token-1)
(cc-bytecomp-defun c-beginning-of-statement-1)
(cc-bytecomp-defun c-backward-sws)

(defvar awk-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\\ "\\" st)
    (modify-syntax-entry ?\n ">   " st)
    (modify-syntax-entry ?\r ">   " st)
    (modify-syntax-entry ?\f ">   " st)
    (modify-syntax-entry ?\# "<   " st)
    ;; / can delimit regexes or be a division operator.  By default we assume
    ;; that it is a division sign, and fix the regexp operator cases with
    ;; `font-lock-syntactic-keywords'.
    (modify-syntax-entry ?/ "." st)     ; ACM 2002/4/27.
    (modify-syntax-entry ?* "." st)
    (modify-syntax-entry ?+ "." st)
    (modify-syntax-entry ?- "." st)
    (modify-syntax-entry ?= "." st)
    (modify-syntax-entry ?% "." st)
    (modify-syntax-entry ?< "." st)
    (modify-syntax-entry ?> "." st)
    (modify-syntax-entry ?& "." st)
    (modify-syntax-entry ?| "." st)
    (modify-syntax-entry ?_ "_" st)
    (modify-syntax-entry ?\' "." st)
    st)
  "Syntax table in use in AWK Mode buffers.")


;; This section defines regular expressions used in the analysis of AWK code.

;; N.B. In the following regexps, an EOL is either \n OR \r.  This is because
;; Emacs has in the past used \r to mark hidden lines in some fashion (and
;; maybe still does).

(defconst c-awk-esc-pair-re "\\\\\\(.\\|\n\\|\r\\|\\'\\)")
;;   Matches any escaped (with \) character-pair, including an escaped newline.
(defconst c-awk-non-eol-esc-pair-re "\\\\\\(.\\|\\'\\)")
;;   Matches any escaped (with \) character-pair, apart from an escaped newline.
(defconst c-awk-comment-without-nl "#.*")
;; Matches an AWK comment, not including the terminating NL (if any).  Note
;; that the "enclosing" (elisp) regexp must ensure the # is real.
(defconst c-awk-nl-or-eob "\\(\n\\|\r\\|\\'\\)")
;; Matches a newline, or the end of buffer.

;; "Space" regular expressions.
(eval-and-compile
  (defconst c-awk-escaped-nl "\\\\[\n\r]"))
;; Matches an escaped newline.
(eval-and-compile
  (defconst c-awk-escaped-nls* (concat "\\(" c-awk-escaped-nl "\\)*")))
;; Matches a possibly empty sequence of escaped newlines.  Used in
;; awk-font-lock-keywords.
;; (defconst c-awk-escaped-nls*-with-space*
;;   (concat "\\(" c-awk-escaped-nls* "\\|" "[ \t]+" "\\)*"))
;; The above RE was very slow.  It's runtime was doubling with each additional
;; space :-(  Reformulate it as below:
(eval-and-compile
  (defconst c-awk-escaped-nls*-with-space*
    (concat "\\(" c-awk-escaped-nl "\\|" "[ \t]" "\\)*")))
;; Matches a possibly empty sequence of escaped newlines with optional
;; interspersed spaces and tabs.  Used in awk-font-lock-keywords.
(defconst c-awk-blank-or-comment-line-re
  (concat "[ \t]*\\(#\\|\\\\?$\\)"))
;; Matche (the tail of) a line containing at most either a comment or an
;; escaped EOL.

;; REGEXPS FOR "HARMLESS" STRINGS/LINES.
(defconst c-awk-harmless-char-re "[^_#/\"\\\\\n\r]")
;;   Matches any character but a _, #, /, ", \, or newline.  N.B. _" starts a
;; localization string in gawk 3.1
(defconst c-awk-harmless-_ "_\\([^\"]\\|\\'\\)")
;;   Matches an underline NOT followed by ".
(defconst c-awk-harmless-string*-re
  (concat "\\(" c-awk-harmless-char-re "\\|" c-awk-esc-pair-re "\\|" c-awk-harmless-_ "\\)*"))
;;   Matches a (possibly empty) sequence of chars without unescaped /, ", \,
;; #, or newlines.
(defconst c-awk-harmless-string*-here-re
  (concat "\\=" c-awk-harmless-string*-re))
;; Matches the (possibly empty) sequence of chars without unescaped /, ", \,
;; at point.
(defconst c-awk-harmless-line-re
  (concat c-awk-harmless-string*-re
          "\\(" c-awk-comment-without-nl "\\)?" c-awk-nl-or-eob))
;;   Matches (the tail of) an AWK \"logical\" line not containing an unescaped
;; " or /.  "logical" means "possibly containing escaped newlines".  A comment
;; is matched as part of the line even if it contains a " or a /.  The End of
;; buffer is also an end of line.
(defconst c-awk-harmless-lines+-here-re
  (concat "\\=\\(" c-awk-harmless-line-re "\\)+"))
;; Matches a sequence of (at least one) \"harmless-line\" at point.


;; REGEXPS FOR AWK STRINGS.
(defconst c-awk-string-ch-re "[^\"\\\n\r]")
;; Matches any character which can appear unescaped in a string.
(defconst c-awk-string-innards-re
  (concat "\\(" c-awk-string-ch-re "\\|" c-awk-esc-pair-re "\\)*"))
;;   Matches the inside of an AWK string (i.e. without the enclosing quotes).
(defconst c-awk-string-without-end-here-re
  (concat "\\=_?\"" c-awk-string-innards-re))
;;   Matches an AWK string at point up to, but not including, any terminator.
;; A gawk 3.1+ string may look like _"localizable string".
(defconst c-awk-one-line-possibly-open-string-re
  (concat "\"\\(" c-awk-string-ch-re "\\|" c-awk-non-eol-esc-pair-re "\\)*"
	  "\\(\"\\|\\\\?$\\|\\'\\)"))

;; REGEXPS FOR AWK REGEXPS.
(defconst c-awk-regexp-normal-re "[^[/\\\n\r]")
;;   Matches any AWK regexp character which doesn't require special analysis.
(defconst c-awk-escaped-newlines*-re "\\(\\\\[\n\r]\\)*")
;;   Matches a (possibly empty) sequence of escaped newlines.

;; NOTE: In what follows, "[asdf]" in a regexp will be called a "character
;; list", and "[:alpha:]" inside a character list will be known as a
;; "character class".  These terms for these things vary between regexp
;; descriptions .
(defconst c-awk-regexp-char-class-re
  "\\[:[a-z]+:\\]")
  ;; Matches a character class spec (e.g. [:alpha:]).
(defconst c-awk-regexp-char-list-re
  (concat "\\[" c-awk-escaped-newlines*-re "^?" c-awk-escaped-newlines*-re "]?"
          "\\(" c-awk-esc-pair-re "\\|" c-awk-regexp-char-class-re
	  "\\|" "[^]\n\r]" "\\)*" "\\(]\\|$\\)"))
;;   Matches a regexp char list, up to (but not including) EOL if the ] is
;;   missing.
(defconst c-awk-regexp-one-line-possibly-open-char-list-re
  (concat "\\[\\]?\\(" c-awk-non-eol-esc-pair-re "\\|" "[^]\n\r]" "\\)*"
	  "\\(]\\|\\\\?$\\|\\'\\)"))
;;   Matches the head (or all) of a regexp char class, up to (but not
;;   including) the first EOL.
(defconst c-awk-regexp-innards-re
  (concat "\\(" c-awk-esc-pair-re "\\|" c-awk-regexp-char-list-re
          "\\|" c-awk-regexp-normal-re "\\)*"))
;;   Matches the inside of an AWK regexp (i.e. without the enclosing /s)
(defconst c-awk-regexp-without-end-re
  (concat "/" c-awk-regexp-innards-re))
;; Matches an AWK regexp up to, but not including, any terminating /.
(defconst c-awk-one-line-possibly-open-regexp-re
  (concat "/\\(" c-awk-non-eol-esc-pair-re
	  "\\|" c-awk-regexp-one-line-possibly-open-char-list-re
	  "\\|" c-awk-regexp-normal-re "\\)*"
	  "\\(/\\|\\\\?$\\|\\'\\)"))
;; Matches as much of the head of an AWK regexp which fits on one line,
;; possibly all of it.

;; REGEXPS used for scanning an AWK buffer in order to decide IF A '/' IS A
;; REGEXP OPENER OR A DIVISION SIGN.  By "state" in the following is meant
;; whether a '/' at the current position would by a regexp opener or a
;; division sign.
(defconst c-awk-neutral-re
;  "\\([{}@` \t]\\|\\+\\+\\|--\\|\\\\.\\)+") ; changed, 2003/6/7
  "\\([{}@` \t]\\|\\+\\+\\|--\\|\\\\.\\)")
;;   A "neutral" char(pair).  Doesn't change the "state" of a subsequent /.
;; This is space/tab, braces, an auto-increment/decrement operator or an
;; escaped character.  Or one of the (invalid) characters @ or `.  But NOT an
;; end of line (even if escaped).
(defconst c-awk-neutrals*-re
  (concat "\\(" c-awk-neutral-re "\\)*"))
;;   A (possibly empty) string of neutral characters (or character pairs).
(defconst c-awk-var-num-ket-re "[]\)0-9a-zA-Z_$.\x80-\xff]+")
;;   Matches a char which is a constituent of a variable or number, or a ket
;; (i.e. closing bracKET), round or square.  Assume that all characters \x80 to
;; \xff are "letters".
(defconst c-awk-div-sign-re
  (concat c-awk-var-num-ket-re c-awk-neutrals*-re "/"))
;;   Will match a piece of AWK buffer ending in / which is a division sign, in
;; a context where an immediate / would be a regexp bracket.  It follows a
;; variable or number (with optional intervening "neutral" characters).  This
;; will only work when there won't be a preceding " or / before the sought /
;; to foul things up.
(defconst c-awk-non-arith-op-bra-re
  "[[\(&=:!><,?;'~|]")
;;   Matches an opening BRAcket, round or square, or any operator character
;; apart from +,-,/,*,%.  For the purpose at hand (detecting a / which is a
;; regexp bracket) these arith ops are unnecessary and a pain, because of "++"
;; and "--".
(defconst c-awk-regexp-sign-re
  (concat c-awk-non-arith-op-bra-re c-awk-neutrals*-re "/"))
;;   Will match a piece of AWK buffer ending in / which is an opening regexp
;; bracket, in a context where an immediate / would be a division sign.  This
;; will only work when there won't be a preceding " or / before the sought /
;; to foul things up.

;; REGEXPS USED FOR FINDING THE POSITION OF A "virtual semicolon"
(defconst c-awk-_-harmless-nonws-char-re "[^#/\"\\\\\n\r \t]")
;; NEW VERSION!  (which will be restricted to the current line)
(defconst c-awk-one-line-non-syn-ws*-re
  (concat "\\([ \t]*"
              "\\(" c-awk-_-harmless-nonws-char-re "\\|"
	            c-awk-non-eol-esc-pair-re "\\|"
		    c-awk-one-line-possibly-open-string-re "\\|"
		    c-awk-one-line-possibly-open-regexp-re
	      "\\)"
          "\\)*"))


;; ACM, 2002/5/29:
;;
;; The next section of code is about determining whether or not an AWK
;; statement is complete or not.  We use this to indent the following line.
;; The determination is pretty straightforward in C, where a statement ends
;; with either a ; or a }.  Only "while" really gives any trouble there, since
;; it might be the end of a do-while.  In AWK, on the other hand, semicolons
;; are rarely used, and EOLs _usually_ act as "virtual semicolons".  In
;; addition, we have the complexity of escaped EOLs.  The core of this
;; analysis is in the middle of the function
;; c-awk-calculate-NL-prop-prev-line, about 130 lines lower down.
;;
;; To avoid continually repeating this expensive analysis, we "cache" its
;; result in a text-property, c-awk-NL-prop, whose value for a line is set on
;; the EOL (if any) which terminates that line.  Should the property be
;; required for the very last line (which has no EOL), it is calculated as
;; required but not cached.  The c-awk-NL-prop property should be thought of
;; as only really valid immediately after a buffer change, not a permanently
;; set property.  (By contrast, the syntax-table text properties (set by an
;; after-change function) must be constantly updated for the mode to work
;; properly).
;;
;; This text property is also used for "syntactic whitespace" movement, this
;; being where the distinction between the values '$' and '}' is significant.
;;
;; The valid values for c-awk-NL-prop are:
;;
;; nil The property is not currently set for this line.
;; '#' There is NO statement on this line (at most a comment), and no open
;;     statement from a previous line which could have been completed on this
;;     line.
;; '{' There is an unfinished statement on this (or a previous) line which
;;     doesn't require \s to continue onto another line, e.g. the line ends
;;     with {, or the && operator, or "if (condition)".  Note that even if the
;;     newline is redundantly escaped, it remains a '{' line.
;; '\' There is an escaped newline at the end of this line and this '\' is
;;     essential to the syntax of the program.  (i.e. if it had been a
;;     frivolous \, it would have been ignored and the line been given one of
;;     the other property values.)
;; '$' A non-empty statement is terminated on the line by an EOL (a "virtual
;;     semicolon").  This might be a content-free line terminating a statement
;;     from the preceding (continued) line (which has property \).
;; '}' A statement, being the last thing (aside from ws/comments) is
;;     explicitly terminated on this line by a closing brace (or sometimes a
;;     semicolon).
;;
;; This set of values has been chosen so that the property's value on a line
;; is completely determined by the contents of the line and the property on
;; the previous line, EXCEPT for where a "while" might be the closing
;; statement of a do-while.

(defun c-awk-after-if-for-while-condition-p (&optional do-lim)
  ;; Are we just after the ) in "if/for/while (<condition>)"?
  ;;
  ;; Note that the end of the ) in a do .... while (<condition>) doesn't
  ;; count, since the purpose of this routine is essentially to decide
  ;; whether to indent the next line.
  ;;
  ;; DO-LIM sets a limit on how far back we search for the "do" of a possible
  ;; do-while.
  ;;
  ;; This function might do hidden buffer changes.
  (and
   (eq (char-before) ?\))
   (save-excursion
     (let ((par-pos (c-safe (scan-lists (point) -1 0))))
       (when par-pos
         (goto-char par-pos)            ; back over "(...)"
         (c-backward-token-1)           ; BOB isn't a problem.
         (or (looking-at "\\(if\\|for\\)\\>\\([^_]\\|$\\)")
             (and (looking-at "while\\>\\([^_]\\|$\\)") ; Ensure this isn't a do-while.
                  (not (eq (c-beginning-of-statement-1 do-lim)
                           'beginning)))))))))

(defun c-awk-after-function-decl-param-list ()
  ;; Are we just after the ) in "function foo (bar)" ?
  ;;
  ;; This function might do hidden buffer changes.
  (and (eq (char-before) ?\))
       (save-excursion
         (let ((par-pos (c-safe (scan-lists (point) -1 0))))
           (when par-pos
             (goto-char par-pos)        ; back over "(...)"
             (c-backward-token-1)       ; BOB isn't a problem
             (and (looking-at "[_a-zA-Z][_a-zA-Z0-9]*\\>")
                  (progn (c-backward-token-1)
                         (looking-at "func\\(tion\\)?\\>"))))))))

;; 2002/11/8:  FIXME!  Check c-backward-token-1/2 for success (0 return code).
(defun c-awk-after-continue-token ()
;; Are we just after a token which can be continued onto the next line without
;; a backslash?
;;
;; This function might do hidden buffer changes.
  (save-excursion
    (c-backward-token-1)              ; FIXME 2002/10/27.  What if this fails?
    (if (and (looking-at "[&|]") (not (bobp)))
        (backward-char)) ; c-backward-token-1 doesn't do this :-(
    (looking-at "[,{?:]\\|&&\\|||\\|do\\>\\|else\\>")))

(defun c-awk-after-rbrace-or-statement-semicolon ()
  ;; Are we just after a } or a ; which closes a statement?
  ;; Be careful about ;s in for loop control bits.  They don't count!
  ;;
  ;; This function might do hidden buffer changes.
  (or (eq (char-before) ?\})
      (and
       (eq (char-before) ?\;)
       (save-excursion
         (let ((par-pos (c-safe (scan-lists (point) -1 1))))
           (when par-pos
             (goto-char par-pos) ; go back to containing (
             (not (and (looking-at "(")
                       (c-backward-token-1) ; BOB isn't a problem
                       (looking-at "for\\>")))))))))

(defun c-awk-back-to-contentful-text-or-NL-prop ()
  ;;  Move back to just after the first found of either (i) an EOL which has
  ;;  the c-awk-NL-prop text-property set; or (ii) non-ws text; or (iii) BOB.
  ;;  We return either the value of c-awk-NL-prop (in case (i)) or nil.
  ;;  Calling functions can best distinguish cases (ii) and (iii) with (bolp).
  ;;
  ;;  Note that an escaped eol counts as whitespace here.
  ;;
  ;;  Kludge: If c-backward-syntactic-ws gets stuck at a BOL, it is likely
  ;;  that the previous line contains an unterminated string (without \).  In
  ;;  this case, assume that the previous line's c-awk-NL-prop is a $.
  ;;
  ;;  POINT MUST BE AT THE START OF A LINE when calling this function.  This
  ;;  is to ensure that the various backward-comment functions will work
  ;;  properly.
  ;;
  ;; This function might do hidden buffer changes.
  (let ((nl-prop nil)
        bol-pos bsws-pos) ; starting pos for a backward-syntactic-ws call.
    (while ;; We are at a BOL here.  Go back one line each iteration.
        (and
         (not (bobp))
         (not (setq nl-prop (c-get-char-property (1- (point)) 'c-awk-NL-prop)))
         (progn (setq bol-pos (c-point 'bopl))
                (setq bsws-pos (point))
                ;; N.B. the following function will not go back past an EOL if
                ;; there is an open string (without \) on the previous line.
                ;; If we find such, set the c-awk-NL-prop on it, too
                ;; (2004/3/29).
                (c-backward-syntactic-ws bol-pos)
                (or (/= (point) bsws-pos)
                    (progn (setq nl-prop ?\$)
			   (c-put-char-property (1- (point)) 'c-awk-NL-prop nl-prop)
                           nil)))
         ;; If we had a backslash at EOL, c-backward-syntactic-ws will
         ;; have gone backwards over it.  Check the backslash was "real".
         (progn
           (if (looking-at "[ \t]*\\\\+$")
               (if (progn
                     (end-of-line)
                     (search-backward-regexp
                      "\\(^\\|[^\\]\\)\\(\\\\\\\\\\)*\\\\$" ; ODD number of \s at EOL  :-)
                      bol-pos t))
                   (progn (end-of-line)   ; escaped EOL.
                          (backward-char)
                          (c-backward-syntactic-ws bol-pos))
                 (end-of-line)))          ; The \ at eol is a fake.
           (bolp))))
    nl-prop))

(defun c-awk-calculate-NL-prop-prev-line (&optional do-lim)
  ;; Calculate and set the value of the c-awk-NL-prop on the immediately
  ;; preceding EOL.  This may also involve doing the same for several
  ;; preceding EOLs.
  ;;
  ;; NOTE that if the property was already set, we return it without
  ;; recalculation.  (This is by accident rather than design.)
  ;;
  ;; Return the property which got set (or was already set) on the previous
  ;; line.  Return nil if we hit BOB.
  ;;
  ;; See c-awk-after-if-for-while-condition-p for a description of DO-LIM.
  ;;
  ;; This function might do hidden buffer changes.
  (save-excursion
    (save-match-data
      (beginning-of-line)
      (let* ((pos (point))
             (nl-prop (c-awk-back-to-contentful-text-or-NL-prop)))
        ;; We are either (1) at a BOL (with nl-prop containing the previous
        ;; line's c-awk-NL-prop) or (2) after contentful text on a line.  At
        ;; the BOB counts as case (1), so we test next for bolp rather than
        ;; non-nil nl-prop.
        (when (not (bolp))
          (setq nl-prop
                (cond
                 ;; Incomplete statement which doesn't require escaped EOL?
                 ((or (c-awk-after-if-for-while-condition-p do-lim)
                      (c-awk-after-function-decl-param-list)
                      (c-awk-after-continue-token))
                  ?\{)
                 ;; Escaped EOL (where there's also something to continue)?
                 ((and (looking-at "[ \t]*\\\\$")
                       (not (c-awk-after-rbrace-or-statement-semicolon)))
                  ?\\)
		 ;; A statement was completed on this line.  How?
		 ((memq (char-before) '(?\; ?\}))  ?\})	; Real ; or }
                 (t ?\$)))            ; A virtual semicolon.
          (end-of-line)
          (c-put-char-property (point) 'c-awk-NL-prop nl-prop)
          (forward-line))

        ;; We are now at a (possibly empty) sequence of content-free lines.
        ;; Set c-awk-NL-prop on each of these lines's EOL.
        (while (< (point) pos)         ; one content-free line each iteration.
          (cond              ; recalculate nl-prop from previous line's value.
           ((memq nl-prop '(?\} ?\$ nil)) (setq nl-prop ?\#))
           ((eq nl-prop ?\\)
            (if (not (looking-at "[ \t]*\\\\$")) (setq nl-prop ?\$)))
           ;; ?\# (empty line) and ?\{ (open stmt) don't change.
           )
          (forward-line)
          (c-put-char-property (1- (point)) 'c-awk-NL-prop nl-prop))
        nl-prop))))

(defun c-awk-get-NL-prop-prev-line (&optional do-lim)
  ;; Get the c-awk-NL-prop text-property from the previous line, calculating
  ;; it if necessary.  Return nil if we're already at BOB.
  ;; See c-awk-after-if-for-while-condition-p for a description of DO-LIM.
  ;;
  ;; This function might do hidden buffer changes.
  (if (bobp)
      nil
    (or (c-get-char-property (c-point 'eopl) 'c-awk-NL-prop)
        (c-awk-calculate-NL-prop-prev-line do-lim))))

(defun c-awk-get-NL-prop-cur-line (&optional do-lim)
  ;; Get the c-awk-NL-prop text-property from the current line, calculating it
  ;; if necessary. (As a special case, the property doesn't get set on an
  ;; empty line at EOB (there's no position to set the property on), but the
  ;; function returns the property value an EOL would have got.)
  ;;
  ;; See c-awk-after-if-for-while-condition-p for a description of DO-LIM.
  ;;
  ;; This function might do hidden buffer changes.
  (save-excursion
    (let ((extra-nl nil))
      (end-of-line)                ; Necessary for the following test to work.
      (when (= (forward-line) 1)        ; if we were on the last line....
        (insert-char ?\n 1) ; ...artificial eol is needed for comment detection.
        (setq extra-nl t))
      (prog1 (c-awk-get-NL-prop-prev-line do-lim)
        (if extra-nl (delete-char -1))))))

(defsubst c-awk-prev-line-incomplete-p (&optional do-lim)
  ;; Is there an incomplete statement at the end of the previous line?
  ;; See c-awk-after-if-for-while-condition-p for a description of DO-LIM.
  ;;
  ;; This function might do hidden buffer changes.
  (memq (c-awk-get-NL-prop-prev-line do-lim) '(?\\ ?\{)))

(defsubst c-awk-cur-line-incomplete-p (&optional do-lim)
  ;; Is there an incomplete statement at the end of the current line?
  ;; See c-awk-after-if-for-while-condition-p for a description of DO-LIM.
  ;;
  ;; This function might do hidden buffer changes.
  (memq (c-awk-get-NL-prop-cur-line do-lim) '(?\\ ?\{)))

;; NOTES ON "VIRTUAL SEMICOLONS"
;;
;; A "virtual semicolon" is what terminates a statement when there is no ;
;; or } to do the job.  Like point, it is considered to lie _between_ two
;; characters.  As from mid-March 2004, it is considered to lie just after
;; the last non-syntactic-whitespace character on the line; (previously, it
;; was considered an attribute of the EOL on the line).  A real semicolon
;; never counts as a virtual one.

(defun c-awk-at-vsemi-p (&optional pos)
  ;; Is there a virtual semicolon at POS (or POINT)?
  (save-excursion
    (let (nl-prop
	  (pos-or-point (progn (if pos (goto-char pos)) (point))))
      (forward-line 0)
      (search-forward-regexp c-awk-one-line-non-syn-ws*-re)
      (and (eq (point) pos-or-point)
	   (progn
	     (while (and (eq (setq nl-prop (c-awk-get-NL-prop-cur-line)) ?\\)
			 (eq (forward-line) 0)
			 (looking-at c-awk-blank-or-comment-line-re)))
	     (eq nl-prop ?\$))))))

(defun c-awk-vsemi-status-unknown-p ()
  ;; Are we unsure whether there is a virtual semicolon on the current line?
  ;; DO NOT under any circumstances attempt to calculate this; that would
  ;; defeat the (admittedly kludgy) purpose of this function, which is to
  ;; prevent an infinite recursion in c-beginning-of-statement-1 when point
  ;; starts at a `while' token.
  (not (c-get-char-property (c-point 'eol) 'c-awk-NL-prop)))

(defun c-awk-clear-NL-props (beg end)
  ;; This function is run from before-change-hooks.  It clears the
  ;; c-awk-NL-prop text property from beg to the end of the buffer (The END
  ;; parameter is ignored).  This ensures that the indentation engine will
  ;; never use stale values for this property.
  ;;
  ;; This function might do hidden buffer changes.
  (save-restriction
    (widen)
    (c-clear-char-properties beg (point-max) 'c-awk-NL-prop)))

(defun c-awk-unstick-NL-prop ()
  ;; Ensure that the text property c-awk-NL-prop is "non-sticky".  Without
  ;; this, a new newline inserted after an old newline (e.g. by C-j) would
  ;; inherit any c-awk-NL-prop from the old newline.  This would be a Bad
  ;; Thing.  This function's action is required by c-put-char-property.
  (if (and (boundp 'text-property-default-nonsticky) ; doesn't exist in XEmacs
           (not (assoc 'c-awk-NL-prop text-property-default-nonsticky)))
      (setq text-property-default-nonsticky
            (cons '(c-awk-NL-prop . t) text-property-default-nonsticky))))

;; The following is purely a diagnostic command, to be commented out of the
;; final release.  ACM, 2002/6/1
;; (defun NL-props ()
;;   (interactive)
;;   (let (pl-prop cl-prop)
;;     (message "Prev-line: %s  Cur-line: %s"
;;              (if (setq pl-prop (c-get-char-property (c-point 'eopl) 'c-awk-NL-prop))
;;                  (char-to-string pl-prop)
;;                "nil")
;;              (if (setq cl-prop (c-get-char-property (c-point 'eol) 'c-awk-NL-prop))
;;                  (char-to-string cl-prop)
;;                "nil"))))
;(define-key awk-mode-map [?\C-c ?\r] 'NL-props) ; commented out, 2002/8/31
;for now.  In the byte compiled version, this causes things to crash because
;awk-mode-map isn't yet defined.  :-(

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following section of the code is to do with font-locking.  The biggest
;; problem for font-locking is deciding whether a / is a regular expression
;; delimiter or a division sign - determining precisely where strings and
;; regular expressions start and stop is also troublesome.  This is the
;; purpose of the function c-awk-set-syntax-table-properties and the myriad
;; elisp regular expressions it uses.
;;
;; Because AWK is a line oriented language, I felt the normal cc-mode strategy
;; for font-locking unterminated strings (i.e. font-locking the buffer up to
;; the next string delimiter as a string) was inappropriate.  Instead,
;; unbalanced string/regexp delimiters are given the warning font, being
;; refonted with the string font as soon as the matching delimiter is entered.
;;
;; This requires the region processed by the current font-lock after-change
;; function to have access to the start of the string/regexp, which may be
;; several lines back.  The elisp "advice" feature is used on these functions
;; to allow this.

(defun c-awk-beginning-of-logical-line (&optional pos)
;; Go back to the start of the (apparent) current line (or the start of the
;; line containing POS), returning the buffer position of that point.  I.e.,
;; go back to the last line which doesn't have an escaped EOL before it.
;;
;; This is guaranteed to be "safe" for syntactic analysis, i.e. outwith any
;; comment, string or regexp.  IT MAY WELL BE that this function should not be
;; executed on a narrowed buffer.
;;
;; This function might do hidden buffer changes.
  (if pos (goto-char pos))
  (forward-line 0)
  (while (and (> (point) (point-min))
              (eq (char-before (1- (point))) ?\\))
    (forward-line -1))
  (point))

(defun c-awk-beyond-logical-line (&optional pos)
;; Return the position just beyond the (apparent) current logical line, or the
;; one containing POS.  This is usually the beginning of the next line which
;; doesn't follow an escaped EOL.  At EOB, this will be EOB.
;;
;; Point is unchanged.
;;
;; This is guaranteed to be "safe" for syntactic analysis, i.e. outwith any
;; comment, string or regexp.  IT MAY WELL BE that this function should not be
;; executed on a narrowed buffer.
  (save-excursion
    (if pos (goto-char pos))
    (end-of-line)
    (while (and (< (point) (point-max))
		(eq (char-before) ?\\))
      (end-of-line 2))
    (if (< (point) (point-max))
	(1+ (point))
      (point))))

;; ACM, 2002/02/15: The idea of the next function is to put the "Error font"
;; on strings/regexps which are missing their closing delimiter.
;; 2002/4/28.  The default syntax for / has been changed from "string" to
;; "punctuation", to reduce hassle when this character appears within a string
;; or comment.

(defun c-awk-set-string-regexp-syntax-table-properties (beg end)
;; BEG and END bracket a (possibly unterminated) string or regexp.  The
;; opening delimiter is after BEG, and the closing delimiter, IF ANY, is AFTER
;; END.  Set the appropriate syntax-table properties on the delimiters and
;; contents of this string/regex.
;;
;; "String" here can also mean a gawk 3.1 "localizable" string which starts
;; with _".  In this case, we step over the _ and ignore it; It will get it's
;; font from an entry in awk-font-lock-keywords.
;;
;; If the closing delimiter is missing (i.e., there is an EOL there) set the
;; STRING-FENCE property on the opening " or / and closing EOL.
;;
;; This function does hidden buffer changes.
  (if (eq (char-after beg) ?_) (setq beg (1+ beg)))

  ;; First put the properties on the delimiters.
  (cond ((eq end (point-max))           ; string/regexp terminated by EOB
         (c-put-char-property beg 'syntax-table '(15))) ; (15) = "string fence"
        ((/= (char-after beg) (char-after end)) ; missing end delimiter
         (c-put-char-property beg 'syntax-table '(15))
         (c-put-char-property end 'syntax-table '(15)))
        ((eq (char-after beg) ?/)       ; Properly bracketed regexp
         (c-put-char-property beg 'syntax-table '(7)) ; (7) = "string"
         (c-put-char-property end 'syntax-table '(7)))
        (t))                       ; Properly bracketed string: Nothing to do.
  ;; Now change the properties of any escaped "s in the string to punctuation.
  (save-excursion
    (goto-char (1+ beg))
    (or (eobp)
        (while (search-forward "\"" end t)
          (c-put-char-property (1- (point)) 'syntax-table '(1))))))

(defun c-awk-syntax-tablify-string ()
  ;; Point is at the opening " or _" of a string.  Set the syntax-table
  ;; properties on this string, leaving point just after the string.
  ;;
  ;; The result is nil if a / immediately after the string would be a regexp
  ;; opener, t if it would be a division sign.
  ;;
  ;; This function does hidden buffer changes.
  (search-forward-regexp c-awk-string-without-end-here-re nil t) ; a (possibly unterminated) string
  (c-awk-set-string-regexp-syntax-table-properties
   (match-beginning 0) (match-end 0))
  (cond ((looking-at "\"")
         (forward-char)
         t)                             ; In AWK, ("15" / 5) gives 3 ;-)
        ((looking-at "[\n\r]")          ; Unterminated string with EOL.
         (forward-char)
         nil)                           ; / on next line would start a regexp
        (t nil)))                       ; Unterminated string at EOB

(defun c-awk-syntax-tablify-/ (anchor anchor-state-/div)
  ;; Point is at a /.  Determine whether this is a division sign or a regexp
  ;; opener, and if the latter, apply syntax-table properties to the entire
  ;; regexp.  Point is left immediately after the division sign or regexp, as
  ;; the case may be.
  ;;
  ;; ANCHOR-STATE-/DIV identifies whether a / at ANCHOR would have been a
  ;; division sign (value t) or a regexp opener (value nil).  The idea is that
  ;; we analyze the line from ANCHOR up till point to determine what the / at
  ;; point is.
  ;;
  ;; The result is what ANCHOR-STATE-/DIV (see above) is where point is left.
  ;;
  ;; This function does hidden buffer changes.
  (let ((/point (point)))
    (goto-char anchor)
    ;; Analyze the line to find out what the / is.
    (if (if anchor-state-/div
            (not (search-forward-regexp c-awk-regexp-sign-re (1+ /point) t))
          (search-forward-regexp c-awk-div-sign-re (1+ /point) t))
        ;; A division sign.
	(progn (goto-char (1+ /point)) nil)
      ;; A regexp opener
      ;; Jump over the regexp innards, setting the match data.
      (goto-char /point)
      (search-forward-regexp c-awk-regexp-without-end-re)
      (c-awk-set-string-regexp-syntax-table-properties
       (match-beginning 0) (match-end 0))
      (cond ((looking-at "/")           ; Terminating /
             (forward-char)
             t)
            ((looking-at "[\n\r]")      ; Incomplete regexp terminated by EOL
             (forward-char)
             nil)                  ; / on next line would start another regexp
            (t nil)))))                 ; Unterminated regexp at EOB

(defun c-awk-set-syntax-table-properties (lim)
;;     Scan the buffer text between point and LIM, setting (and clearing) the
;; syntax-table property where necessary.
;;
;; This function is designed to be called as the FUNCTION in a MATCHER in
;; font-lock-syntactic-keywords, and it always returns NIL (to inhibit
;; repeated calls from font-lock: See elisp info page "Search-based
;; Fontification").  It also gets called, with a bit of glue, from
;; after-change-functions when font-lock isn't active.  Point is left
;; "undefined" after this function exits.  THE BUFFER SHOULD HAVE BEEN
;; WIDENED, AND ANY PRECIOUS MATCH-DATA SAVED BEFORE CALLING THIS ROUTINE.
;;
;; We need to set/clear the syntax-table property on:
;; (i) / - It is set to "string" on a / which is the opening or closing
;;     delimiter of the properly terminated regexp (and left unset on a
;;     division sign).
;; (ii) the opener of an unterminated string/regexp, we set the property
;;    "generic string delimiter" on both the opening " or / and the end of the
;;    line where the closing delimiter is missing.
;; (iii) "s inside strings/regexps (these will all be escaped "s).  They are
;;   given the property "punctuation".  This will later allow other routines
;;   to use the regexp "\\S\"*" to skip over the string innards.
;; (iv) Inside a comment, all syntax-table properties are cleared.
;;
;; This function does hidden buffer changes.
  (let (anchor
	(anchor-state-/div nil)) ; t means a following / would be a div sign.
    (c-awk-beginning-of-logical-line) ; ACM 2002/7/21.  This is probably redundant.
    (c-clear-char-properties (point) lim 'syntax-table)
    ;; Once round the next loop for each string, regexp, or div sign
    (while (progn
             ;; Skip any "harmless" lines before the next tricky one.
             (if (search-forward-regexp c-awk-harmless-lines+-here-re nil t)
                 (setq anchor-state-/div nil))
             (< (point) lim))
      (setq anchor (point))
      (search-forward-regexp c-awk-harmless-string*-here-re nil t)
      ;; We are now looking at either a " or a /.
      ;; Do our thing on the string, regexp or division sign.
      (setq anchor-state-/div
            (if (looking-at "_?\"")
                (c-awk-syntax-tablify-string)
              (c-awk-syntax-tablify-/ anchor anchor-state-/div))))
    nil))

;; ACM, 2002/07/21: Thoughts: We need an AWK Mode after-change function to set
;; the syntax-table properties even when font-lock isn't enabled, for the
;; subsequent use of movement functions, etc.  However, it seems that if font
;; lock _is_ enabled, we can always leave it to do the job.
(defvar c-awk-old-ByLL 0)
(make-variable-buffer-local 'c-awk-old-Byll)
;; Just beyond logical line following the region which is about to be changed.
;; Set in c-awk-record-region-clear-NL and used in c-awk-after-change.

(defun c-awk-record-region-clear-NL (beg end)
;; This function is called exclusively from the before-change-functions hook.
;; It does two things: Finds the end of the (logical) line on which END lies,
;; and clears c-awk-NL-prop text properties from this point onwards.  BEG is
;; ignored.
;;
;; On entry, the buffer will have been widened and match-data will have been
;; saved; point is undefined on both entry and exit; the return value is
;; ignored.
;;
;; This function does hidden buffer changes.
  (c-save-buffer-state ()
    (setq c-awk-old-ByLL (c-awk-beyond-logical-line end))
    (c-save-buffer-state nil
      (c-awk-clear-NL-props end (point-max)))))

(defun c-awk-end-of-change-region (beg end old-len)
  ;; Find the end of the region which needs to be font-locked after a change.
  ;; This is the end of the logical line on which the change happened, either
  ;; as it was before the change, or as it is now, whichever is later.
  ;; N.B. point is left undefined.
  (max (+ (- c-awk-old-ByLL old-len) (- end beg))
       (c-awk-beyond-logical-line end)))

;; ACM 2002/5/25.  When font-locking is invoked by a buffer change, the region
;; specified by the font-lock after-change function must be expanded to
;; include ALL of any string or regexp within the region.  The simplest way to
;; do this in practice is to use the beginning/end-of-logical-line functions.
;; Don't overlook the possibility of the buffer change being the "recapturing"
;; of a previously escaped newline.

;; ACM 2008-02-05:
(defun c-awk-extend-and-syntax-tablify-region (beg end old-len)
  ;; Expand the region (BEG END) as needed to (c-new-BEG c-new-END) then put
  ;; `syntax-table' properties on this region.
  ;;
  ;; This function is called from an after-change function, BEG END and
  ;; OLD-LEN being the standard parameters.
  ;;
  ;; Point is undefined both before and after this function call, the buffer
  ;; has been widened, and match-data saved.  The return value is ignored.
  ;;
  ;; It prepares the buffer for font
  ;; locking, hence must get called before `font-lock-after-change-function'.
  ;;
  ;; This function is the AWK value of `c-before-font-lock-function'.
  ;; It does hidden buffer changes.
  (c-save-buffer-state ()
    (setq c-new-END (c-awk-end-of-change-region beg end old-len))
    (setq c-new-BEG (c-awk-beginning-of-logical-line beg))
    (goto-char c-new-BEG)
    (c-awk-set-syntax-table-properties c-new-END)))

;; Awk regexps written with help from Peter Galbraith
;; <galbraith@mixing.qc.dfo.ca>.
;; Take GNU Emacs's 'words out of the following regexp-opts.  They don't work
;; in XEmacs 21.4.4.  acm 2002/9/19.
(defconst awk-font-lock-keywords
  (eval-when-compile
    (list
     ;; Function names.
     '("^\\s *\\(func\\(tion\\)?\\)\\>\\s *\\(\\sw+\\)?"
       (1 font-lock-keyword-face) (3 font-lock-function-name-face nil t))
     ;;
     ;; Variable names.
     (cons
      (concat "\\<"
	      (regexp-opt
	       '("ARGC" "ARGIND" "ARGV" "BINMODE" "CONVFMT" "ENVIRON"
		 "ERRNO" "FIELDWIDTHS" "FILENAME" "FNR" "FS" "IGNORECASE"
		 "LINT" "NF" "NR" "OFMT" "OFS" "ORS" "PROCINFO" "RLENGTH"
		 "RS" "RSTART" "RT" "SUBSEP" "TEXTDOMAIN") t) "\\>")
      'font-lock-variable-name-face)

     ;; Special file names.  (acm, 2002/7/22)
     ;; The following regexp was created by first evaluating this in GNU Emacs 21.1:
     ;; (regexp-opt '("/dev/stdin" "/dev/stdout" "/dev/stderr" "/dev/fd/n" "/dev/pid"
     ;;                 "/dev/ppid" "/dev/pgrpid" "/dev/user") 'words)
     ;; , removing the "?:" from each "\\(?:" (for backward compatibility with older Emacsen)
     ;; , replacing the "n" in "dev/fd/n" with "[0-9]+"
     ;; , removing the unwanted \\< at the beginning, and finally filling out the
     ;; regexp so that a " must come before, and either a " or heuristic stuff after.
     ;; The surrounding quotes are fontified along with the filename, since, semantically,
     ;; they are an indivisible unit.
     '("\\(\"/dev/\\(fd/[0-9]+\\|p\\(\\(\\(gr\\)?p\\)?id\\)\\|\
std\\(err\\|in\\|out\\)\\|user\\)\\)\\>\
\\(\\(\"\\)\\|\\([^\"/\n\r][^\"\n\r]*\\)?$\\)"
       (1 font-lock-variable-name-face t)
       (8 font-lock-variable-name-face t t))
     ;; Do the same (almost) with
     ;; (regexp-opt '("/inet/tcp/lport/rhost/rport" "/inet/udp/lport/rhost/rport"
     ;;                 "/inet/raw/lport/rhost/rport") 'words)
     ;; This cannot be combined with the above pattern, because the match number
     ;; for the (optional) closing \" would then exceed 9.
     '("\\(\"/inet/\\(\\(raw\\|\\(tc\\|ud\\)p\\)/lport/rhost/rport\\)\\)\\>\
\\(\\(\"\\)\\|\\([^\"/\n\r][^\"\n\r]*\\)?$\\)"
       (1 font-lock-variable-name-face t)
       (6 font-lock-variable-name-face t t))

     ;; Keywords.
     (concat "\\<"
	     (regexp-opt
	      '("BEGIN" "END" "break" "case" "continue" "default" "delete"
		"do" "else" "exit" "for" "getline" "if" "in" "next"
		"nextfile" "return" "switch" "while")
	      t) "\\>")

     ;; Builtins.
     `(eval . (list
	       ,(concat
		 "\\<"
		 (regexp-opt
		  '("adump" "and" "asort" "atan2" "bindtextdomain" "close"
		    "compl" "cos" "dcgettext" "exp" "extension" "fflush"
		    "gensub" "gsub" "index" "int" "length" "log" "lshift"
		    "match" "mktime" "or" "print" "printf" "rand" "rshift"
		    "sin" "split" "sprintf" "sqrt" "srand" "stopme"
		    "strftime" "strtonum" "sub" "substr"  "system"
		    "systime" "tolower" "toupper" "xor") t)
		 "\\>")
	       0 c-preprocessor-face-name))

     ;; gawk debugging keywords.  (acm, 2002/7/21)
     ;; (Removed, 2003/6/6.  These functions are now fontified as built-ins)
     ;;	(list (concat "\\<" (regexp-opt '("adump" "stopme") t) "\\>")
     ;;	   0 'font-lock-warning-face)

     ;; User defined functions with an apparent spurious space before the
     ;; opening parenthesis.  acm, 2002/5/30.
     `(,(concat "\\(\\w\\|_\\)" c-awk-escaped-nls* "\\s "
		c-awk-escaped-nls*-with-space* "(")
       (0 'font-lock-warning-face))

     ;; Space after \ in what looks like an escaped newline.  2002/5/31
     '("\\\\\\s +$" 0 font-lock-warning-face t)

     ;; Unbalanced string (") or regexp (/) delimiters.  2002/02/16.
     '("\\s|" 0 font-lock-warning-face t nil)
     ;; gawk 3.1 localizable strings ( _"translate me!").  2002/5/21
     '("\\(_\\)\\s|" 1 font-lock-warning-face)
     '("\\(_\\)\\s\"" 1 font-lock-string-face) ; FIXME! not for XEmacs. 2002/10/6
     ))
  "Default expressions to highlight in AWK mode.")

;; ACM 2002/9/29.  Movement functions, e.g. for C-M-a and C-M-e

;; The following three regexps differ from those earlier on in cc-awk.el in
;; that they assume the syntax-table properties have been set.  They are thus
;; not useful for code which sets these properties.
(defconst c-awk-terminated-regexp-or-string-here-re "\\=\\s\"\\S\"*\\s\"")
;; Matches a terminated string/regexp.

(defconst c-awk-unterminated-regexp-or-string-here-re "\\=\\s|\\S|*$")
;; Matches an unterminated string/regexp, NOT including the eol at the end.

(defconst c-awk-harmless-pattern-characters*
  (concat "\\([^{;#/\"\\\\\n\r]\\|" c-awk-esc-pair-re "\\)*"))
;; Matches any "harmless" character in a pattern or an escaped character pair.

(defun c-awk-at-statement-end-p ()
  ;; Point is not inside a comment or string.  Is it AT the end of a
  ;; statement?  This means immediately after the last non-ws character of the
  ;; statement.  The caller is responsible for widening the buffer, if
  ;; appropriate.
  (and (not (bobp))
       (save-excursion
	 (backward-char)
	 (or (looking-at "[};]")
	     (and (memq (c-awk-get-NL-prop-cur-line) '(?\$ ?\\))
		  (looking-at
		   (eval-when-compile
		     (concat "[^ \t\n\r\\]" c-awk-escaped-nls*-with-space*
			     "[#\n\r]"))))))))

(defun c-awk-beginning-of-defun (&optional arg)
  "Move backward to the beginning of an AWK \"defun\".  With ARG, do it that
many times.  Negative arg -N means move forward to Nth following beginning of
defun.  Returns t unless search stops due to beginning or end of buffer.

By a \"defun\" is meant either a pattern-action pair or a function.  The start
of a defun is recognized as code starting at column zero which is neither a
closing brace nor a comment nor a continuation of the previous line.  Unlike
in some other modes, having an opening brace at column 0 is neither necessary
nor helpful.

Note that this function might do hidden buffer changes.  See the
comment at the start of cc-engine.el for more info."
  (interactive "p")
  (or arg (setq arg 1))
  (save-match-data
    (c-save-buffer-state                ; ensures the buffer is writable.
     nil
     (let ((found t))     ; Has the most recent regexp search found b-of-defun?
       (if (>= arg 0)
           ;; Go back one defun each time round the following loop. (For +ve arg)
           (while (and found (> arg 0) (not (eq (point) (point-min))))
             ;; Go back one "candidate" each time round the next loop until one
             ;; is genuinely a beginning-of-defun.
             (while (and (setq found (search-backward-regexp
                                      "^[^#} \t\n\r]" (point-min) 'stop-at-limit))
                         (not (memq (c-awk-get-NL-prop-prev-line) '(?\$ ?\} ?\#)))))
             (setq arg (1- arg)))
         ;; The same for a -ve arg.
         (if (not (eq (point) (point-max))) (forward-char 1))
         (while (and found (< arg 0) (not (eq (point) (point-max)))) ; The same for -ve arg.
           (while (and (setq found (search-forward-regexp
                                    "^[^#} \t\n\r]" (point-max) 'stop-at-limit))
                       (not (memq (c-awk-get-NL-prop-prev-line) '(?\$ ?\} ?\#)))))
           (setq arg (1+ arg)))
         (if found (goto-char (match-beginning 0))))
       (eq arg 0)))))

(defun c-awk-forward-awk-pattern ()
  ;; Point is at the start of an AWK pattern (which may be null) or function
  ;; declaration.  Move to the pattern's end, and past any trailing space or
  ;; comment.  Typically, we stop at the { which denotes the corresponding AWK
  ;; action/function body.  Otherwise we stop at the EOL (or ;) marking the
  ;; absence of an explicit action.
  ;;
  ;; This function might do hidden buffer changes.
  (while
      (progn
        (search-forward-regexp c-awk-harmless-pattern-characters*)
        (if (looking-at "#") (end-of-line))
        (cond
         ((eobp) nil)
         ((looking-at "[{;]") nil)  ; We've finished!
         ((eolp)
          (if (c-awk-cur-line-incomplete-p)
              (forward-line)            ; returns non-nil
            nil))
         ((search-forward-regexp c-awk-terminated-regexp-or-string-here-re nil t))
         ((search-forward-regexp c-awk-unterminated-regexp-or-string-here-re nil t))
         ((looking-at "/") (forward-char) t))))) ; division sign.

(defun c-awk-end-of-defun1 ()
  ;; point is at the start of a "defun".  Move to its end.  Return end position.
  ;;
  ;; This function might do hidden buffer changes.
  (c-awk-forward-awk-pattern)
  (cond
   ((looking-at "{") (goto-char (scan-sexps (point) 1)))
   ((looking-at ";") (forward-char))
   ((eolp))
   (t (error "c-awk-end-of-defun1:  Failure of c-awk-forward-awk-pattern")))
  (point))

(defun c-awk-beginning-of-defun-p ()
  ;; Are we already at the beginning of a defun?  (i.e. at code in column 0
  ;; which isn't a }, and isn't a continuation line of any sort.
  ;;
  ;; This function might do hidden buffer changes.
  (and (looking-at "^[^#} \t\n\r]")
       (not (c-awk-prev-line-incomplete-p))))

(defun c-awk-end-of-defun (&optional arg)
  "Move forward to next end of defun.  With argument, do it that many times.
Negative argument -N means move back to Nth preceding end of defun.

An end of a defun occurs right after the closing brace that matches the
opening brace at its start, or immediately after the AWK pattern when there is
no explicit action; see function `c-awk-beginning-of-defun'.

Note that this function might do hidden buffer changes.  See the
comment at the start of cc-engine.el for more info."
  (interactive "p")
  (or arg (setq arg 1))
  (save-match-data
    (c-save-buffer-state
     nil
     (let ((start-point (point)) end-point)
       ;; Strategy: (For +ve ARG): If we're not already at a beginning-of-defun,
       ;; move backwards to one.
       ;; Repeat [(i) move forward to end-of-current-defun (see below);
       ;;         (ii) If this isn't it, move forward to beginning-of-defun].
       ;; We start counting ARG only when step (i) has passed the original point.
       (when (> arg 0)
         ;; Try to move back to a beginning-of-defun, if not already at one.
         (if (not (c-awk-beginning-of-defun-p))
             (when (not (c-awk-beginning-of-defun 1)) ; No bo-defun before point.
               (goto-char start-point)
               (c-awk-beginning-of-defun -1))) ; if this fails, we're at EOB, tough!
         ;; Now count forward, one defun at a time
         (while (and (not (eobp))
                     (c-awk-end-of-defun1)
                     (if (> (point) start-point) (setq arg (1- arg)) t)
                     (> arg 0)
                     (c-awk-beginning-of-defun -1))))

       (when (< arg 0)
         (setq end-point start-point)
         (while (and (not (bobp))
                     (c-awk-beginning-of-defun 1)
                     (if (< (setq end-point (if (bobp) (point)
                                              (save-excursion (c-awk-end-of-defun1))))
                            start-point)
                         (setq arg (1+ arg)) t)
                     (< arg 0)))
         (goto-char (min start-point end-point)))))))


(cc-provide 'cc-awk)			; Changed from 'awk-mode, ACM 2002/5/21

;;; awk-mode.el ends here
