;;; expand.el --- make abbreviations more usable

;; Copyright (C) 1995-1996, 2001-2012 Free Software Foundation, Inc.

;; Author: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Maintainer: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Keywords: abbrev

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
;;
;; This package defines abbrevs which expand into structured constructs
;; for certain languages.  The construct is indented for you,
;; and contains slots for you to fill in other text.

;; These abbrevs expand only at the end of a line and when not in a comment
;; or a string.
;;
;;   Look at the Sample: section for emacs-lisp, perl and c expand lists.
;; For example for c-mode, you could declare your abbrev table with :
;;
;; (defconst c-expand-list
;;   '(("if" "if () {\n \n} else {\n \n}" (5 10 21))
;;     ("ifn" "if () {}" (5 8))
;;     ("uns" "unsigned ")
;;     ("for" "for(; ; ) {\n\n}" (5 7 9 13))
;;     ("switch" "switch () {\n\n}" (9 13))
;;     ("case" "case :\n\nbreak;\n" (6 8 16))
;;     ("do" "do {\n\n} while ();" (6 16))
;;     ("while" "while () {\n\n}" (8 12))
;;     ("default" "default:\n\nbreak;" 10)
;;     ("main" "int\nmain(int argc, char * argv[])\n{\n\n}\n" 37))
;;   "Expansions for C mode")
;;
;;   and enter Abbrev mode with the following hook :
;;
;; (add-hook 'c-mode-hook
;; 	  (lambda ()
;; 	    (expand-add-abbrevs c-mode-abbrev-table c-expand-list)
;; 	    (abbrev-mode 1)))
;;
;;   you can also init some post-process hooks :
;;
;; (add-hook 'expand-load-hook
;; 	  (lambda ()
;; 	    (add-hook 'expand-expand-hook 'indent-according-to-mode)
;; 	    (add-hook 'expand-jump-hook 'indent-according-to-mode)))
;;
;; Remarks:
;;
;;   Many thanks to Heddy Boubaker <boubaker@cenatls.cena.dgac.fr>,
;;                  Jerome Santini <santini@chambord.univ-orleans.fr>,
;;                  Jari Aalto <jaalto@tre.tele.nokia.fi>.
;;
;;   Please send me a word to give me your feeling about this feature or
;; to explain me how you use it (your expansions table for example) using
;; the function expand-submit-report.
;;; Code:

;;; Constants:

(defgroup expand nil
  "Make abbreviations more usable."
  :group 'abbrev)

(defcustom expand-load-hook nil
  "Hooks run when `expand.el' is loaded."
  :type 'hook
  :group 'expand)

(defcustom expand-expand-hook nil
  "Hooks run when an abbrev made by `expand-add-abbrevs' is expanded."
  :type 'hook
  :group 'expand)

(defcustom expand-jump-hook nil
  "Hooks run by `expand-jump-to-previous-slot' and `expand-jump-to-next-slot'."
  :type 'hook
  :group 'expand)

;;; Samples:

(define-skeleton expand-c-for-skeleton "For loop skeleton"
  "Loop var: "
  "for(" str _ @ "=0; " str @ "; " str @ ") {" \n
  @ _ \n
  "}" > \n)

(defconst expand-c-sample-expand-list
  '(("if" "if () {\n \n} else {\n \n}" (5 10 21))
    ("ifn" "if () {}" (5 8))
    ("uns" "unsigned ")
    ("for" expand-c-for-skeleton)
    ("switch" "switch () {\n\n}" (9 13))
    ("case" "case :\n\nbreak;\n" (6 8 16))
    ("do" "do {\n\n} while ();" (6 16))
    ("while" "while () {\n\n}" (8 12))
    ("default" "default:\n\nbreak;" 10)
    ("main" "int\nmain(int argc, char * argv[])\n{\n\n}\n" 37))
  "Expansions for C mode.  See `expand-add-abbrevs'.")

;; lisp example from Jari Aalto <jaalto@tre.tele.nokia.fi>
(defconst expand-sample-lisp-mode-expand-list
  (list
   (list
    "defu"
    (concat
     "(defun   ()\n"
     "  \"\"\n"
     "  (interactive)\n"
     "  (let* (\n"
     "         )\n"
     "    \n"
     "    ))")
    (list 8 11 16 32 43 59))

   (list
    "defs"
    (concat
     "(defsubst   ()\n"
     "  \"\"\n"
     "  (interactive)\n"
     "  )")
    (list 11 14 19 23 39))

   (list
    "defm"
    (concat
     "(defmacro  ()\n"
     "  \"\"\n"
     "  `( \n"
     "    ))")
    (list 11 13 18 25))

   (list
    "defa"
    (concat
     "(defadvice   (around   act)\n"
     "  \"\"\n"
     "  \n"
     "  )")
    (list 12 22 32 36))

    (list
     "defc"
     "(defconst   nil\n  \"\")\n"
     (list 11 13 20))

    (list
     "defv"
     "(defvar   nil\n  \"\")\n"
     (list 9 11 18))

    (list
     "let"
     "(let* (\n)\n    "
     (list 8 13))

     (list
     "sav"
     "(save-excursion\n \n)"
     (list 18))

     (list
     "aut"
     "(autoload ' \"\" t t)\n"
     (list 12 14))

    )
   "Expansions for Lisp mode.  See `expand-add-abbrevs'.")

;; perl example from Jari Aalto <jaalto@tre.tele.nokia.fi>
(defconst expand-sample-perl-mode-expand-list
  (list
   (list
    ;;   This is default perl4 subroutine template
    ;;
    "sub"
    (concat
     "#" (make-string 70 ?-) "\n"
     "sub   {\n"
     "    # DESCRIPTION\n"
     "    #   \n"
     "    #   \n"
     "    # INPUT\n"
     "    #   \n"
     "    #   \n"
     "    # RETURN\n"
     "    #   \n"
     "\n"
     "    local( $f ) = \"$lib.\";\n"   ;; Function name AFTER period
     "    local() = @_;\n"              ;; func arguments here
     "    \n"
     "    \n}\n"
     )
    (list 77 88 120 146 159 176))

   (list
    "for"                               ; foreach
    (concat
     "for (  )\n"
     "{\n\n\}"
     )
    (list 7 12))

   (list
    "whi"                               ; foreach
    (concat
     "while (  )\n"
     "{\n\n\}"
     )
    (list 9 15))


   ;;   The normal "if" can be used like
   ;;   print $F "xxxxxx"  if defined @arr;
   ;;
   (list
    "iff"
    (concat
     "if (  )\n"
     "{\n\n\}"
     )
    (list 6 12))

   (list "loc"  "local( $ );"   (list 9))
   (list "my"   "my( $ );"      (list 6))
   (list "ope"  "open(,\"\")\t|| die \"$f: Can't open [$]\";" (list 6 8 36))
   (list "clo"  "close ;"       7)
   (list "def"  "defined  "     (list 9))
   (list "und"  "undef ;"       (list 7))

   ;;   There is no ending colon, because they can be in statement
   ;;    defined $REXP_NOT_NEW && (print "xxxxx" );
   ;;
   (list "pr"  "print "         7)
   (list "pf"  "printf "        8)


   (list "gre"  "grep( //, );"  (list 8 11))
   (list "pus"  "push( , );"    (list 7 9))
   (list "joi"  "join( '', );"  (list 7 11))
   (list "rtu"  "return ;"      (list 8))

   )
  "Expansions for Perl mode.  See `expand-add-abbrevs'.")

;;; Code:

;;;###autoload
(defun expand-add-abbrevs (table abbrevs)
  "Add a list of abbreviations to abbrev table TABLE.
ABBREVS is a list of abbrev definitions; each abbrev description entry
has the form (ABBREV EXPANSION ARG).

ABBREV is the abbreviation to replace.

EXPANSION is the replacement string or a function which will make the
expansion.  For example, you could use the DMacros or skeleton packages
to generate such functions.

ARG is an optional argument which can be a number or a list of
numbers.  If ARG is a number, point is placed ARG chars from the
beginning of the expanded text.

If ARG is a list of numbers, point is placed according to the first
member of the list, but you can visit the other specified positions
cyclically with the functions `expand-jump-to-previous-slot' and
`expand-jump-to-next-slot'.

If ARG is omitted, point is placed at the end of the expanded text."

  (if (null abbrevs)
      table
    (expand-add-abbrev table (nth 0 (car abbrevs)) (nth 1 (car abbrevs))
		       (nth 2 (car abbrevs)))
    (expand-add-abbrevs table (cdr abbrevs))))

(defvar expand-list nil "Temporary variable used by the Expand package.")

(defvar expand-pos nil
  "If non-nil, stores a vector containing markers to positions defined by the last expansion.")
(make-variable-buffer-local 'expand-pos)

(defvar expand-index 0
  "Index of the last marker used in `expand-pos'.")
(make-variable-buffer-local 'expand-index)

(defvar expand-point nil
  "End of the expanded region.")
(make-variable-buffer-local 'expand-point)

(defun expand-add-abbrev (table abbrev expansion arg)
  "Add one abbreviation and provide the hook to move to the specified positions."
  (let* ((string-exp (if (and (symbolp expansion) (fboundp expansion))
			 nil
		       expansion))
         (position   (if (and arg string-exp)
			 (if (listp arg)
			     (- (length expansion) (1- (car arg)))
			   (- (length expansion) (1- arg)))
		       0)))
    (define-abbrev
      table
      abbrev
      (vector string-exp
	      position
	      (if (and (listp arg)
		       (not (null arg)))
		  (cons (length string-exp) arg)
		nil)
	      (if (and (symbolp expansion) (fboundp expansion))
		  expansion
		nil)
	      )
      'expand-abbrev-hook)))

(put 'expand-abbrev-hook 'no-self-insert t)
;;;###autoload
(defun expand-abbrev-hook ()
  "Abbrev hook used to do the expansion job of expand abbrevs.
See `expand-add-abbrevs'.  Value is non-nil if expansion was done."
  ;; Expand only at the end of a line if we are near a word that has
  ;; an abbrev built from expand-add-abbrev.
  (if (and (eolp)
	   (not (expand-in-literal)))
      (let ((p (point)))
	(setq expand-point nil)
	;; don't expand if the preceding char isn't a word constituent
	(if (and (eq (char-syntax (preceding-char))
		     ?w)
		 (expand-do-expansion))
	    (progn
	      ;; expand-point tells us if we have inserted the text
	      ;; ourself or if it is the hook which has done the job.
	      (if expand-point
		  (progn
		    (if (vectorp expand-list)
			(expand-build-marks expand-point))
		    (indent-region p expand-point nil))
		;; an outside function can set expand-list to a list of
		;; markers in reverse order.
		(if (listp expand-list)
		    (setq expand-index 0
			  expand-pos (expand-list-to-markers expand-list)
			  expand-list nil)))
	      (run-hooks 'expand-expand-hook)
	      t)
	  nil))
    nil))

(defun expand-do-expansion ()
  (delete-char (- (length last-abbrev-text)))
  (let* ((vect (symbol-value last-abbrev))
	 (text (aref vect 0))
	 (position (aref vect 1))
	 (jump-args (aref vect 2))
	 (hook (aref vect 3)))
    (cond (text
	   (insert text)
	   (setq expand-point (point))))
    (if jump-args
	(funcall 'expand-build-list (car jump-args) (cdr jump-args)))
    (if position
	(backward-char position))
    (if hook
	(funcall hook))
    t)
  )

(defun expand-abbrev-from-expand (word)
  "Test if an abbrev has a hook."
  (or
   (and (intern-soft word local-abbrev-table)
	(symbol-function (intern-soft word local-abbrev-table)))
   (and (intern-soft word global-abbrev-table)
	(symbol-function (intern-soft word global-abbrev-table)))))

(defun expand-previous-word ()
  "Return the previous word."
  (save-excursion
    (let ((p (point)))
      (backward-word 1)
      (buffer-substring p (point)))))

;;;###autoload
(defun expand-jump-to-previous-slot ()
  "Move the cursor to the previous slot in the last abbrev expansion.
This is used only in conjunction with `expand-add-abbrevs'."
  (interactive)
  (if expand-pos
      (progn
	(setq expand-index (1- expand-index))
	(if (< expand-index 0)
	    (setq expand-index (1- (length expand-pos))))
	(goto-char (aref expand-pos expand-index))
	(run-hooks 'expand-jump-hook))))

;;;###autoload
(defun expand-jump-to-next-slot ()
  "Move the cursor to the next slot in the last abbrev expansion.
This is used only in conjunction with `expand-add-abbrevs'."
  (interactive)
  (if expand-pos
      (progn
	(setq expand-index (1+ expand-index))
	(if (>= expand-index (length expand-pos))
	    (setq expand-index 0))
	(goto-char (aref expand-pos expand-index))
	(run-hooks 'expand-jump-hook))))

;;;###autoload (define-key abbrev-map "p" 'expand-jump-to-previous-slot)
;;;###autoload (define-key abbrev-map "n" 'expand-jump-to-next-slot)

(defun expand-build-list (len l)
  "Build a vector of offset positions from the list of positions."
  (expand-clear-markers)
  (setq expand-list (vconcat l))
  (let ((i 0)
	(lenlist (length expand-list)))
    (while (< i lenlist)
      (aset expand-list i (- len (1- (aref expand-list i))))
      (setq i (1+ i))))
  )

(defun expand-build-marks (p)
  "Transform the offsets vector into a marker vector."
  (if expand-list
      (progn
	(setq expand-index 0)
	(setq expand-pos (make-vector (length expand-list) nil))
	(let ((i (1- (length expand-list))))
	  (while (>= i 0)
	    (aset expand-pos i (copy-marker (- p (aref expand-list i))))
	    (setq i (1- i))))
	(setq expand-list nil))))

(defun expand-clear-markers ()
  "Make the markers point nowhere."
  (if expand-pos
      (progn
    (let ((i (1- (length expand-pos))))
      (while (>= i 0)
	(set-marker (aref expand-pos i) nil)
	(setq i (1- i))))
    (setq expand-pos nil))))

(defun expand-in-literal ()
  "Test if we are in a comment or in a string."
  (save-excursion
    (let* ((lim (or (save-excursion
		      (beginning-of-defun)
		      (point))
		    (point-min)))
	   (state (parse-partial-sexp lim (point))))
      (cond
       ((nth 3 state) 'string)
       ((nth 4 state) 'comment)
       (t nil)))))

;; support functions to add marks to jump from outside function

(defun expand-list-to-markers (l)
  "Transform a list of markers in reverse order into a vector in the correct order."
  (let* ((len (1- (length l)))
	 (loop len)
	 (v (make-vector (+ len 1) nil)))
    (while (>= loop 0)
      (aset v loop (if (markerp (car l)) (car l) (copy-marker (car l))))
      (setq l (cdr l)
	    loop (1- loop)))
    v))

;; integration with skeleton.el
;; Used in `skeleton-end-hook' to fetch the positions for  @ skeleton tags.
;; See `skeleton-insert'.
(defun expand-skeleton-end-hook ()
  (if skeleton-positions
      (setq expand-list skeleton-positions)))

(add-hook 'skeleton-end-hook (function expand-skeleton-end-hook))

(provide 'expand)

;; run load hooks
(run-hooks 'expand-load-hook)

;;; expand.el ends here
