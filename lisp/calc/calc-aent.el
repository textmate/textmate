;;; calc-aent.el --- algebraic entry functions for Calc

;; Copyright (C) 1990-1993, 2001-2012  Free Software Foundation, Inc.

;; Author: Dave Gillespie <daveg@synaptics.com>
;; Maintainer: Jay Belanger <jay.p.belanger@gmail.com>

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

;; This file is autoloaded from calc.el.

(require 'calc)
(require 'calc-macs)

;; Declare functions which are defined elsewhere.
(declare-function calc-refresh-evaltos "calc-ext" (&optional which-var))
(declare-function calc-execute-kbd-macro "calc-prog" (mac arg &rest prefix))
(declare-function math-is-true "calc-ext" (expr))
(declare-function calc-explain-why "calc-stuff" (why &optional more))
(declare-function calc-alg-edit "calc-yank" (str))
(declare-function math-composite-inequalities "calc-prog" (x op))
(declare-function math-flatten-lands "calc-rewr" (expr))
(declare-function math-multi-subst "calc-map" (expr olds news))
(declare-function calcFunc-vmatches "calc-rewr" (expr pat))
(declare-function math-simplify "calc-alg" (top-expr))
(declare-function math-known-matrixp "calc-arith" (a))
(declare-function math-parse-fortran-subscr "calc-lang" (sym args))
(declare-function math-to-radians-2 "calc-math" (a))
(declare-function math-read-string "calc-ext" ())
(declare-function math-read-brackets "calc-vec" (space-sep math-rb-close))
(declare-function math-read-angle-brackets "calc-forms" ())
(declare-function math-to-percentsigns "calccomp" (x))

(defvar calc-quick-calc-history nil
  "The history list for quick-calc.")

;;;###autoload
(defun calc-do-quick-calc ()
  (require 'calc-ext)
  (calc-check-defines)
  (if (eq major-mode 'calc-mode)
      (calc-algebraic-entry t)
    (let (buf shortbuf)
      (save-excursion
	(calc-create-buffer)
	(let* ((calc-command-flags nil)
	       (calc-dollar-values calc-quick-prev-results)
	       (calc-dollar-used 0)
	       (enable-recursive-minibuffers t)
	       (calc-language (if (memq calc-language '(nil big))
				  'flat calc-language))
	       (entry (calc-do-alg-entry "" "Quick calc: " t 'calc-quick-calc-history))
	       (alg-exp (mapcar 'math-evaluate-expr entry)))
	  (when (and (= (length alg-exp) 1)
		     (eq (car-safe (car alg-exp)) 'calcFunc-assign)
		     (= (length (car alg-exp)) 3)
		     (eq (car-safe (nth 1 (car alg-exp))) 'var))
	    (set (nth 2 (nth 1 (car alg-exp))) (nth 2 (car alg-exp)))
	    (calc-refresh-evaltos (nth 2 (nth 1 (car alg-exp))))
	    (setq alg-exp (list (nth 2 (car alg-exp)))))
	  (setq calc-quick-prev-results alg-exp
		buf (mapconcat (function (lambda (x)
					   (math-format-value x 1000)))
			       alg-exp
			       " ")
		shortbuf buf)
	  (if (and (= (length alg-exp) 1)
		   (memq (car-safe (car alg-exp)) '(nil bigpos bigneg))
		   (< (length buf) 20)
		   (= calc-number-radix 10))
	      (setq buf (concat buf "  ("
				(let ((calc-number-radix 16))
				  (math-format-value (car alg-exp) 1000))
				", "
				(let ((calc-number-radix 8))
				  (math-format-value (car alg-exp) 1000))
                                ", "
                                (let ((calc-number-radix 2))
                                  (math-format-value (car alg-exp) 1000))
				(if (and (integerp (car alg-exp))
					 (> (car alg-exp) 0)
					 (< (car alg-exp) 127))
				    (format ", \"%c\"" (car alg-exp))
				  "")
				")")))
	  (if (and (< (length buf) (frame-width)) (= (length entry) 1)
		   (featurep 'calc-ext))
	      (let ((long (concat (math-format-value (car entry) 1000)
				  " =>  " buf)))
		(if (<= (length long) (- (frame-width) 8))
		    (setq buf long))))
	  (calc-handle-whys)
	  (message "Result: %s" buf)))
      (if (eq last-command-event 10)
	  (insert shortbuf)
        (kill-new shortbuf)))))

;;;###autoload
(defun calc-do-calc-eval (str separator args)
  (calc-check-defines)
  (catch 'calc-error
    (save-excursion
      (calc-create-buffer)
      (cond
       ((and (consp str) (not (symbolp (car str))))
	(let ((calc-language nil)
	      (math-expr-opers (math-standard-ops))
	      (calc-internal-prec 12)
	      (calc-word-size 32)
	      (calc-symbolic-mode nil)
	      (calc-matrix-mode nil)
	      (calc-angle-mode 'deg)
	      (calc-number-radix 10)
              (calc-twos-complement-mode nil)
	      (calc-leading-zeros nil)
	      (calc-group-digits nil)
	      (calc-point-char ".")
	      (calc-frac-format '(":" nil))
	      (calc-prefer-frac nil)
	      (calc-hms-format "%s@ %s' %s\"")
	      (calc-date-format '((H ":" mm C SS pp " ")
				  Www " " Mmm " " D ", " YYYY))
	      (calc-float-format '(float 0))
	      (calc-full-float-format '(float 0))
	      (calc-complex-format nil)
	      (calc-matrix-just nil)
	      (calc-full-vectors t)
	      (calc-break-vectors nil)
	      (calc-vector-commas ",")
	      (calc-vector-brackets "[]")
	      (calc-matrix-brackets '(R O))
	      (calc-complex-mode 'cplx)
	      (calc-infinite-mode nil)
	      (calc-display-strings nil)
	      (calc-simplify-mode nil)
	      (calc-display-working-message 'lots)
	      (strp (cdr str)))
	  (while strp
	    (set (car strp) (nth 1 strp))
	    (setq strp (cdr (cdr strp))))
	  (calc-do-calc-eval (car str) separator args)))
       ((eq separator 'eval)
	(eval str))
       ((eq separator 'macro)
	(require 'calc-ext)
	(let* ((calc-buffer (current-buffer))
	       (calc-window (get-buffer-window calc-buffer))
	       (save-window (selected-window)))
	  (if calc-window
	      (unwind-protect
		  (progn
		    (select-window calc-window)
		    (calc-execute-kbd-macro str nil (car args)))
		(and (window-point save-window)
		     (select-window save-window)))
	    (save-window-excursion
	      (select-window (get-largest-window))
	      (switch-to-buffer calc-buffer)
	      (calc-execute-kbd-macro str nil (car args)))))
	nil)
       ((eq separator 'pop)
	(or (not (integerp str))
	    (= str 0)
	    (calc-pop (min str (calc-stack-size))))
	(calc-stack-size))
       ((eq separator 'top)
	(and (integerp str)
	     (> str 0)
	     (<= str (calc-stack-size))
	     (math-format-value (calc-top-n str (car args)) 1000)))
       ((eq separator 'rawtop)
	(and (integerp str)
	     (> str 0)
	     (<= str (calc-stack-size))
	     (calc-top-n str (car args))))
       (t
	(let* ((calc-command-flags nil)
	       (calc-next-why nil)
	       (calc-language (if (memq calc-language '(nil big))
				  'flat calc-language))
	       (calc-dollar-values (mapcar
				    (function
				     (lambda (x)
				       (if (stringp x)
					   (progn
					     (setq x (math-read-exprs x))
					     (if (eq (car-safe x)
						     'error)
						 (throw 'calc-error
							(calc-eval-error
							 (cdr x)))
					       (car x)))
					 x)))
				    args))
	       (calc-dollar-used 0)
	       (res (if (stringp str)
			(math-read-exprs str)
		      (list str)))
	       buf)
	  (if (eq (car res) 'error)
	      (calc-eval-error (cdr res))
	    (setq res (mapcar 'calc-normalize res))
	    (and (memq 'clear-message calc-command-flags)
		 (message ""))
	    (cond ((eq separator 'pred)
		   (require 'calc-ext)
		   (if (= (length res) 1)
		       (math-is-true (car res))
		     (calc-eval-error '(0 "Single value expected"))))
		  ((eq separator 'raw)
		   (if (= (length res) 1)
		       (car res)
		     (calc-eval-error '(0 "Single value expected"))))
		  ((eq separator 'list)
		   res)
		  ((memq separator '(num rawnum))
		   (if (= (length res) 1)
		       (if (math-constp (car res))
			   (if (eq separator 'num)
			       (math-format-value (car res) 1000)
			     (car res))
			 (calc-eval-error
			  (list 0
				(if calc-next-why
				    (calc-explain-why (car calc-next-why))
				  "Number expected"))))
		     (calc-eval-error '(0 "Single value expected"))))
		  ((eq separator 'push)
		   (calc-push-list res)
		   nil)
		  (t (while res
		       (setq buf (concat buf
					 (and buf (or separator ", "))
					 (math-format-value (car res) 1000))
			     res (cdr res)))
		     buf)))))))))

(defvar calc-eval-error nil
  "Determines how calc handles errors.
If nil, return a list containing the character position of error.
STRING means return error message as string rather than list.
The value t means abort and give an error message.")

(defun calc-eval-error (msg)
  (if calc-eval-error
      (if (eq calc-eval-error 'string)
	  (nth 1 msg)
	(error "%s" (nth 1 msg)))
    msg))


;;;; Reading an expression in algebraic form.

;;;###autoload
(defun calc-auto-algebraic-entry (&optional prefix)
  (interactive "P")
  (calc-algebraic-entry prefix t))

;;;###autoload
(defun calc-algebraic-entry (&optional prefix auto)
  (interactive "P")
  (calc-wrapper
   (let ((calc-language (if prefix nil calc-language))
	 (math-expr-opers (if prefix (math-standard-ops) (math-expr-ops))))
     (calc-alg-entry (and auto (char-to-string last-command-event))))))

(defvar calc-alg-entry-history nil
  "History for algebraic entry.")

;;;###autoload
(defun calc-alg-entry (&optional initial prompt)
  (let* ((sel-mode nil)
	 (calc-dollar-values (mapcar 'calc-get-stack-element
				     (nthcdr calc-stack-top calc-stack)))
	 (calc-dollar-used 0)
	 (calc-plain-entry t)
	 (alg-exp (calc-do-alg-entry initial prompt t 'calc-alg-entry-history)))
    (if (stringp alg-exp)
	(progn
	  (require 'calc-ext)
	  (calc-alg-edit alg-exp))
      (let* ((calc-simplify-mode (if (eq last-command-event ?\C-j)
				     'none
				   calc-simplify-mode))
	     (nvals (mapcar 'calc-normalize alg-exp)))
	(while alg-exp
	  (calc-record (if (featurep 'calc-ext) (car alg-exp) (car nvals))
		       "alg'")
	  (calc-pop-push-record-list calc-dollar-used
				     (and (not (equal (car alg-exp)
						      (car nvals)))
					  (featurep 'calc-ext)
					  "")
				     (list (car nvals)))
	  (setq alg-exp (cdr alg-exp)
		nvals (cdr nvals)
		calc-dollar-used 0)))
      (calc-handle-whys))))

(defvar calc-alg-ent-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "'" 'calcAlg-previous)
    (define-key map "`" 'calcAlg-edit)
    (define-key map "\C-m" 'calcAlg-enter)
    (define-key map "\C-j" 'calcAlg-enter)
    map)
  "The keymap used for algebraic entry.")

(defvar calc-alg-ent-esc-map
  (let ((map (make-keymap))
        (i 33))
    (set-keymap-parent map esc-map)
    (while (< i 127)
      (define-key map (vector i) 'calcAlg-escape)
      (setq i (1+ i)))
    map)
  "The keymap used for escapes in algebraic entry.")

(defvar calc-alg-exp)

;;;###autoload
(defun calc-do-alg-entry (&optional initial prompt no-normalize history)
  (let* ((calc-buffer (current-buffer))
	 (blink-matching-check-function 'calcAlg-blink-matching-check)
	 (calc-alg-exp 'error))
    (define-key calc-alg-ent-map "\e" nil)
    (if (eq calc-algebraic-mode 'total)
	(define-key calc-alg-ent-map "\e" calc-alg-ent-esc-map)
      (define-key calc-alg-ent-map "\e+" 'calcAlg-plus-minus)
      (define-key calc-alg-ent-map "\em" 'calcAlg-mod)
      (define-key calc-alg-ent-map "\e=" 'calcAlg-equals)
      (define-key calc-alg-ent-map "\e\r" 'calcAlg-equals)
      (define-key calc-alg-ent-map "\ep" 'previous-history-element)
      (define-key calc-alg-ent-map "\en" 'next-history-element)
      (define-key calc-alg-ent-map "\e%" 'self-insert-command))
    (setq calc-aborted-prefix nil)
    (let ((buf (read-from-minibuffer (or prompt "Algebraic: ")
				     (or initial "")
				     calc-alg-ent-map nil history)))
      (when (eq calc-alg-exp 'error)
	(when (eq (car-safe (setq calc-alg-exp (math-read-exprs buf))) 'error)
	  (setq calc-alg-exp nil)))
      (setq calc-aborted-prefix "alg'")
      (or no-normalize
	  (and calc-alg-exp (setq calc-alg-exp (mapcar 'calc-normalize calc-alg-exp))))
      calc-alg-exp)))

(defun calcAlg-plus-minus ()
  (interactive)
  (if (calc-minibuffer-contains ".* \\'")
      (insert "+/- ")
    (insert " +/- ")))

(defun calcAlg-mod ()
  (interactive)
  (if (not (calc-minibuffer-contains ".* \\'"))
      (insert " "))
  (if (calc-minibuffer-contains ".* mod +\\'")
      (if calc-previous-modulo
	  (insert (math-format-flat-expr calc-previous-modulo 0))
	(beep))
    (insert "mod ")))

(defun calcAlg-previous ()
  (interactive)
  (if (calc-minibuffer-contains "\\'")
      (previous-history-element 1)
    (insert "'")))

(defun calcAlg-equals ()
  (interactive)
  (unwind-protect
      (calcAlg-enter)
    (if (consp calc-alg-exp)
	(progn (setq prefix-arg (length calc-alg-exp))
	       (calc-unread-command ?=)))))

(defun calcAlg-escape ()
  (interactive)
  (calc-unread-command)
  (save-excursion
    (calc-select-buffer)
    (use-local-map calc-mode-map))
  (calcAlg-enter))

(defvar calc-plain-entry nil)
(defun calcAlg-edit ()
  (interactive)
  (if (or (not calc-plain-entry)
	  (calc-minibuffer-contains
	   "\\`\\([^\"]*\"[^\"]*\"\\)*[^\"]*\"[^\"]*\\'"))
      (insert "`")
    (setq calc-alg-exp (minibuffer-contents))
    (exit-minibuffer)))

(defvar calc-buffer)

(defun calcAlg-enter ()
  (interactive)
  (let* ((str (minibuffer-contents))
	 (exp (and (> (length str) 0)
		   (with-current-buffer calc-buffer
		     (math-read-exprs str)))))
    (if (eq (car-safe exp) 'error)
	(progn
	  (goto-char (minibuffer-prompt-end))
	  (forward-char (nth 1 exp))
	  (beep)
	  (calc-temp-minibuffer-message
	   (concat " [" (or (nth 2 exp) "Error") "]"))
	  (calc-clear-unread-commands))
      (setq calc-alg-exp (if (calc-minibuffer-contains "\\` *\\[ *\\'")
			'((incomplete vec))
		      exp))
      (exit-minibuffer))))

(defun calcAlg-blink-matching-check (leftpt rightpt)
  (let ((rightchar (char-before rightpt))
        (leftchar (if leftpt (char-after leftpt))))
    (if (and leftpt
 	     (or (and (= rightchar ?\))
 		      (= leftchar ?\[))
 		 (and (= rightchar ?\])
 		      (= leftchar ?\()))
 	     (save-excursion
 	       (goto-char leftpt)
 	       (looking-at ".+\\(\\.\\.\\|\\\\dots\\|\\\\ldots\\)")))
        ;; [2..5) perfectly valid!
 	nil
      (blink-matching-check-mismatch leftpt rightpt))))

;;;###autoload
(defun calc-alg-digit-entry ()
  (calc-alg-entry
   (cond ((eq last-command-event ?e)
	  (if (> calc-number-radix 14) (format "%d.^" calc-number-radix) "1e"))
	 ((eq last-command-event ?#) (format "%d#" calc-number-radix))
	 ((eq last-command-event ?_) "-")
	 ((eq last-command-event ?@) "0@ ")
	 (t (char-to-string last-command-event)))))

;; The variable calc-digit-value is initially declared in calc.el,
;; but can be set by calcDigit-algebraic and calcDigit-edit.
(defvar calc-digit-value)

;;;###autoload
(defun calcDigit-algebraic ()
  (interactive)
  (if (calc-minibuffer-contains ".*[@oh] *[^'m ]+[^'m]*\\'")
      (calcDigit-key)
    (setq calc-digit-value (minibuffer-contents))
    (exit-minibuffer)))

;;;###autoload
(defun calcDigit-edit ()
  (interactive)
  (calc-unread-command)
  (setq calc-digit-value (minibuffer-contents))
  (exit-minibuffer))


;;; Algebraic expression parsing.   [Public]

(defvar math-read-replacement-list
  '(;; Misc symbols
    ("±" "+/-")  ; plus or minus
    ("×" "*")    ; multiplication sign
    ("÷" ":")    ; division sign
    ("−" "-")    ; subtraction sign
    ("∕" "/")    ; division sign
    ("∗" "*")    ; asterisk multiplication
    ("∞" "inf")  ; infinity symbol
    ("≤" "<=")
    ("≥" ">=")
    ("≦" "<=")
    ("≧" ">=")
    ("µ" "μ")
    ;; fractions
    ("¼" "(1:4)") ; 1/4
    ("½" "(1:2)") ; 1/2
    ("¾" "(3:4)") ; 3/4
    ("⅓" "(1:3)") ; 1/3
    ("⅔" "(2:3)") ; 2/3
    ("⅕" "(1:5)") ; 1/5
    ("⅖" "(2:5)") ; 2/5
    ("⅗" "(3:5)") ; 3/5
    ("⅘" "(4:5)") ; 4/5
    ("⅙" "(1:6)") ; 1/6
    ("⅚" "(5:6)") ; 5/6
    ("⅛" "(1:8)") ; 1/8
    ("⅜" "(3:8)") ; 3/8
    ("⅝" "(5:8)") ; 5/8
    ("⅞" "(7:8)") ; 7/8
    ("⅟" "1:")    ; 1/...
    ;; superscripts
    ("⁰" "0")  ; 0
    ("¹" "1")  ; 1
    ("²" "2")  ; 2
    ("³" "3")  ; 3
    ("⁴" "4")  ; 4
    ("⁵" "5")  ; 5
    ("⁶" "6")  ; 6
    ("⁷" "7")  ; 7
    ("⁸" "8")  ; 8
    ("⁹" "9")  ; 9
    ("⁺" "+")  ; +
    ("⁻" "-")  ; -
    ("⁽" "(")  ; (
    ("⁾" ")")  ; )
    ("ⁿ" "n")  ; n
    ("ⁱ" "i")  ; i
    ;; subscripts
    ("₀"  "0")  ; 0
    ("₁"  "1")  ; 1
    ("₂"  "2")  ; 2
    ("₃"  "3")  ; 3
    ("₄"  "4")  ; 4
    ("₅"  "5")  ; 5
    ("₆"  "6")  ; 6
    ("₇"  "7")  ; 7
    ("₈"  "8")  ; 8
    ("₉"  "9")  ; 9
    ("₊"  "+")  ; +
    ("₋"  "-")  ; -
    ("₍"  "(")  ; (
    ("₎"  ")"))  ; )
  "A list whose elements (old new) indicate replacements to make
in Calc algebraic input.")

(defvar math-read-superscripts
  "⁰¹²³⁴⁵⁶⁷⁸⁹⁺⁻⁽⁾ⁿⁱ" ; 0123456789+-()ni
  "A string consisting of the superscripts allowed by Calc.")

(defvar math-read-subscripts
  "₀₁₂₃₄₅₆₇₈₉₊₋₍₎" ; 0123456789+-()
  "A string consisting of the subscripts allowed by Calc.")

;;;###autoload
(defun math-read-preprocess-string (str)
  "Replace some substrings of STR by Calc equivalents."
  (setq str
        (replace-regexp-in-string (concat "[" math-read-superscripts "]+")
                                  "^(\\&)" str))
  (setq str
        (replace-regexp-in-string (concat "[" math-read-subscripts "]+")
                                  "_(\\&)" str))
  (let ((rep-list math-read-replacement-list))
    (while rep-list
      (setq str
            (replace-regexp-in-string (nth 0 (car rep-list))
                                      (nth 1 (car rep-list)) str))
      (setq rep-list (cdr rep-list))))
  str)

;; The next few variables are local to math-read-exprs (and math-read-expr
;; in calc-ext.el), but are set in functions they call.

(defvar math-exp-pos)
(defvar math-exp-str)
(defvar math-exp-old-pos)
(defvar math-exp-token)
(defvar math-exp-keep-spaces)
(defvar math-expr-data)

;;;###autoload
(defun math-read-exprs (math-exp-str)
  (let ((math-exp-pos 0)
	(math-exp-old-pos 0)
	(math-exp-keep-spaces nil)
	math-exp-token math-expr-data)
    (setq math-exp-str (math-read-preprocess-string math-exp-str))
    (if (memq calc-language calc-lang-allow-percentsigns)
        (setq math-exp-str (math-remove-percentsigns math-exp-str)))
    (if calc-language-input-filter
	(setq math-exp-str (funcall calc-language-input-filter math-exp-str)))
    (while (setq math-exp-token
                 (string-match "\\.\\.\\([^.]\\|.[^.]\\)" math-exp-str))
      (setq math-exp-str
            (concat (substring math-exp-str 0 math-exp-token) "\\dots"
			    (substring math-exp-str (+ math-exp-token 2)))))
    (math-build-parse-table)
    (math-read-token)
    (let ((val (catch 'syntax (math-read-expr-list))))
      (if (stringp val)
	  (list 'error math-exp-old-pos val)
	(if (equal math-exp-token 'end)
	    val
	  (list 'error math-exp-old-pos "Syntax error"))))))

;;;###autoload
(defun math-read-expr-list ()
  (let* ((math-exp-keep-spaces nil)
	 (val (list (math-read-expr-level 0)))
	 (last val))
    (while (equal math-expr-data ",")
      (math-read-token)
      (let ((rest (list (math-read-expr-level 0))))
	(setcdr last rest)
	(setq last rest)))
    val))

(defvar calc-user-parse-table nil)
(defvar calc-last-main-parse-table nil)
(defvar calc-last-user-lang-parse-table nil)
(defvar calc-last-lang-parse-table nil)
(defvar calc-user-tokens nil)
(defvar calc-user-token-chars nil)

(defvar math-toks nil
  "Tokens to pass between math-build-parse-table and math-find-user-tokens.")

;;;###autoload
(defun math-build-parse-table ()
  (let ((mtab (cdr (assq nil calc-user-parse-tables)))
	(ltab (cdr (assq calc-language calc-user-parse-tables)))
        (lltab (get calc-language 'math-parse-table)))
    (or (and (eq mtab calc-last-main-parse-table)
	     (eq ltab calc-last-user-lang-parse-table)
             (eq lltab calc-last-lang-parse-table))
	(let ((p (append mtab ltab lltab))
	      (math-toks nil))
	  (setq calc-user-parse-table p)
	  (setq calc-user-token-chars nil)
	  (while p
	    (math-find-user-tokens (car (car p)))
	    (setq p (cdr p)))
	  (setq calc-user-tokens (mapconcat 'identity
					    (sort (mapcar 'car math-toks)
						  (function (lambda (x y)
							      (> (length x)
								 (length y)))))
					    "\\|")
		calc-last-main-parse-table mtab
		calc-last-user-lang-parse-table ltab
                calc-last-lang-parse-table lltab)))))

;;;###autoload
(defun math-find-user-tokens (p)
  (while p
    (cond ((and (stringp (car p))
		(or (> (length (car p)) 1) (equal (car p) "$")
		    (equal (car p) "\""))
		(string-match "[^a-zA-Zα-ωΑ-Ω0-9]" (car p)))
	   (let ((s (regexp-quote (car p))))
	     (if (string-match "\\`[a-zA-Zα-ωΑ-Ω0-9]" s)
		 (setq s (concat "\\<" s)))
	     (if (string-match "[a-zA-Zα-ωΑ-Ω0-9]\\'" s)
		 (setq s (concat s "\\>")))
	     (or (assoc s math-toks)
		 (progn
		   (setq math-toks (cons (list s) math-toks))
		   (or (memq (aref (car p) 0) calc-user-token-chars)
		       (setq calc-user-token-chars
			     (cons (aref (car p) 0)
				   calc-user-token-chars)))))))
	  ((consp (car p))
	   (math-find-user-tokens (nth 1 (car p)))
	   (or (eq (car (car p)) '\?)
	       (math-find-user-tokens (nth 2 (car p))))))
    (setq p (cdr p))))

;;;###autoload
(defun math-read-token ()
  (if (>= math-exp-pos (length math-exp-str))
      (setq math-exp-old-pos math-exp-pos
	    math-exp-token 'end
	    math-expr-data "\000")
    (let (adfn
          (ch (aref math-exp-str math-exp-pos)))
      (setq math-exp-old-pos math-exp-pos)
      (cond ((memq ch '(32 10 9))
	     (setq math-exp-pos (1+ math-exp-pos))
	     (if math-exp-keep-spaces
		 (setq math-exp-token 'space
		       math-expr-data " ")
	       (math-read-token)))
	    ((and (memq ch calc-user-token-chars)
		  (let ((case-fold-search nil))
		    (eq (string-match
                         calc-user-tokens math-exp-str math-exp-pos)
			math-exp-pos)))
	     (setq math-exp-token 'punc
		   math-expr-data (math-match-substring math-exp-str 0)
		   math-exp-pos (match-end 0)))
	    ((or (and (>= ch ?a) (<= ch ?z))
		 (and (>= ch ?A) (<= ch ?Z))
		 (and (>= ch ?α) (<= ch ?ω))
		 (and (>= ch ?Α) (<= ch ?Ω)))
	     (string-match
              (cond
               ((and (memq calc-language calc-lang-allow-underscores)
                     (memq calc-language calc-lang-allow-percentsigns))
                "[a-zA-Zα-ωΑ-Ω0-9_'#]*")
               ((memq calc-language calc-lang-allow-underscores)
			       "[a-zA-Zα-ωΑ-Ω0-9_#]*")
               (t "[a-zA-Zα-ωΑ-Ω0-9'#]*"))
              math-exp-str math-exp-pos)
	     (setq math-exp-token 'symbol
		   math-exp-pos (match-end 0)
		   math-expr-data (math-restore-dashes
			     (math-match-substring math-exp-str 0)))
             (if (setq adfn (get calc-language 'math-lang-adjust-words))
                 (funcall adfn)))
	    ((or (and (>= ch ?0) (<= ch ?9))
		 (and (eq ch '?\.)
		      (eq (string-match "\\.[0-9]" math-exp-str math-exp-pos)
                          math-exp-pos))
		 (and (eq ch '?_)
		      (eq (string-match "_\\.?[0-9]" math-exp-str math-exp-pos)
                          math-exp-pos)
		      (or (eq math-exp-pos 0)
			  (and (not (memq calc-language
                                          calc-lang-allow-underscores))
			       (eq (string-match "[^])}\"a-zA-Zα-ωΑ-Ω0-9'$]_"
						 math-exp-str (1- math-exp-pos))
				   (1- math-exp-pos))))))
	     (or (and (memq calc-language calc-lang-c-type-hex)
		      (string-match "0[xX][0-9a-fA-F]+" math-exp-str math-exp-pos))
		 (string-match "_?\\([0-9]+.?0*@ *\\)?\\([0-9]+.?0*' *\\)?\\(0*\\([2-9]\\|1[0-4]\\)\\(#[#]?\\|\\^\\^\\)[0-9a-dA-D.]+[eE][-+_]?[0-9]+\\|0*\\([2-9]\\|[0-2][0-9]\\|3[0-6]\\)\\(#[#]?\\|\\^\\^\\)[0-9a-zA-Zα-ωΑ-Ω:.]+\\|[0-9]+:[0-9:]+\\|[0-9.]+\\([eE][-+_]?[0-9]+\\)?\"?\\)?"
                               math-exp-str math-exp-pos))
	     (setq math-exp-token 'number
		   math-expr-data (math-match-substring math-exp-str 0)
		   math-exp-pos (match-end 0)))
            ((and (setq adfn
                        (assq ch (get calc-language 'math-lang-read-symbol)))
                  (eval (nth 1 adfn)))
             (eval (nth 2 adfn)))
	    ((eq ch ?\$)
             (if (eq (string-match "\\$\\([1-9][0-9]*\\)" math-exp-str math-exp-pos)
                     math-exp-pos)
                 (setq math-expr-data (- (string-to-number (math-match-substring
                                                            math-exp-str 1))))
               (string-match "\\$+" math-exp-str math-exp-pos)
               (setq math-expr-data (- (match-end 0) (match-beginning 0))))
             (setq math-exp-token 'dollar
                   math-exp-pos (match-end 0)))
	    ((eq ch ?\#)
	     (if (eq (string-match "#\\([1-9][0-9]*\\)" math-exp-str math-exp-pos)
		     math-exp-pos)
		 (setq math-expr-data (string-to-number
				 (math-match-substring math-exp-str 1))
		       math-exp-pos (match-end 0))
	       (setq math-expr-data 1
		     math-exp-pos (1+ math-exp-pos)))
	     (setq math-exp-token 'hash))
	    ((eq (string-match "~=\\|<=\\|>=\\|<>\\|/=\\|\\+/-\\|\\\\dots\\|\\\\ldots\\|\\*\\*\\|<<\\|>>\\|==\\|!=\\|&&&\\||||\\|!!!\\|&&\\|||\\|!!\\|:=\\|::\\|=>"
			       math-exp-str math-exp-pos)
		 math-exp-pos)
	     (setq math-exp-token 'punc
		   math-expr-data (math-match-substring math-exp-str 0)
		   math-exp-pos (match-end 0)))
	    ((and (eq ch ?\")
		  (string-match "\\(\"\\([^\"\\]\\|\\\\.\\)*\\)\\(\"\\|\\'\\)"
                                math-exp-str math-exp-pos))
             (setq math-exp-token 'string
                   math-expr-data (math-match-substring math-exp-str 1)
                   math-exp-pos (match-end 0)))
            ((and (setq adfn (get calc-language 'math-lang-read))
                  (eval (nth 0 adfn))
                  (eval (nth 1 adfn))))
	    ((eq (string-match "%%.*$" math-exp-str math-exp-pos) math-exp-pos)
	     (setq math-exp-pos (match-end 0))
	     (math-read-token))
	    (t
             (if (setq adfn (assq ch (get calc-language 'math-punc-table)))
                 (setq ch (cdr adfn)))
	     (setq math-exp-token 'punc
		   math-expr-data (char-to-string ch)
		   math-exp-pos (1+ math-exp-pos)))))))

(defconst math-alg-inequalities
  '(calcFunc-lt calcFunc-gt calcFunc-leq calcFunc-geq
		calcFunc-eq calcFunc-neq))

(defun math-read-expr-level (exp-prec &optional exp-term)
  (let* ((math-expr-opers (math-expr-ops))
         (x (math-read-factor))
         (first t)
         op op2)
    (while (and (or (and calc-user-parse-table
			 (setq op (calc-check-user-syntax x exp-prec))
			 (setq x op
			       op '("2x" ident 999999 -1)))
		    (and (setq op (assoc math-expr-data math-expr-opers))
			 (/= (nth 2 op) -1)
			 (or (and (setq op2 (assoc
					     math-expr-data
					     (cdr (memq op math-expr-opers))))
				  (eq (= (nth 3 op) -1)
				      (/= (nth 3 op2) -1))
				  (eq (= (nth 3 op2) -1)
				      (not (math-factor-after)))
				  (setq op op2))
			     t))
		    (and (or (eq (nth 2 op) -1)
			     (memq math-exp-token '(symbol number dollar hash))
			     (equal math-expr-data "(")
			     (and (equal math-expr-data "[")
				  (not (equal
                                        (get calc-language
                                             'math-function-open) "["))
				  (not (and math-exp-keep-spaces
					    (eq (car-safe x) 'vec)))))
			 (or (not (setq op (assoc math-expr-data math-expr-opers)))
			     (/= (nth 2 op) -1))
			 (or (not calc-user-parse-table)
			     (not (eq math-exp-token 'symbol))
			     (let ((p calc-user-parse-table))
			       (while (and p
					   (or (not (integerp
						     (car (car (car p)))))
					       (not (equal
						     (nth 1 (car (car p)))
						     math-expr-data))))
				 (setq p (cdr p)))
			       (not p)))
			 (setq op (assoc "2x" math-expr-opers))))
		(not (and exp-term (equal math-expr-data exp-term)))
		(>= (nth 2 op) exp-prec))
      (if (not (equal (car op) "2x"))
	  (math-read-token))
      (and (memq (nth 1 op) '(sdev mod))
	   (require 'calc-ext))
      (setq x (cond ((consp (nth 1 op))
		     (funcall (car (nth 1 op)) x op))
		    ((eq (nth 3 op) -1)
		     (if (eq (nth 1 op) 'ident)
			 x
		       (if (eq (nth 1 op) 'closing)
			   (if (eq (nth 2 op) exp-prec)
			       (progn
				 (setq exp-prec 1000)
				 x)
			     (throw 'syntax "Mismatched delimiters"))
			 (list (nth 1 op) x))))
		    ((and (not first)
			  (memq (nth 1 op) math-alg-inequalities)
			  (memq (car-safe x) math-alg-inequalities))
		     (require 'calc-ext)
		     (math-composite-inequalities x op))
		    (t (list (nth 1 op)
			     x
			     (math-read-expr-level (nth 3 op) exp-term))))
	    first nil))
    x))

;; calc-arg-values is defined in calc-ext.el, but is used here.
(defvar calc-arg-values)

;;;###autoload
(defun calc-check-user-syntax (&optional x prec)
  (let ((p calc-user-parse-table)
	(matches nil)
	match rule)
    (while (and p
		(or (not (progn
			   (setq rule (car (car p)))
			   (if x
			       (and (integerp (car rule))
				    (>= (car rule) prec)
				    (equal math-expr-data
					   (car (setq rule (cdr rule)))))
			     (equal math-expr-data (car rule)))))
		    (let ((save-exp-pos math-exp-pos)
			  (save-exp-old-pos math-exp-old-pos)
			  (save-exp-token math-exp-token)
			  (save-exp-data math-expr-data))
		      (or (not (listp
				(setq matches (calc-match-user-syntax rule))))
			  (let ((args (progn
					(require 'calc-ext)
					calc-arg-values))
				(conds nil)
				temp)
			    (if x
				(setq matches (cons x matches)))
			    (setq match (cdr (car p)))
			    (while (and (eq (car-safe match)
					    'calcFunc-condition)
					(= (length match) 3))
			      (setq conds (append (math-flatten-lands
						   (nth 2 match))
						  conds)
				    match (nth 1 match)))
			    (while (and conds match)
			      (require 'calc-ext)
			      (cond ((eq (car-safe (car conds))
					 'calcFunc-let)
				     (setq temp (car conds))
				     (or (= (length temp) 3)
					 (and (= (length temp) 2)
					      (eq (car-safe (nth 1 temp))
						  'calcFunc-assign)
					      (= (length (nth 1 temp)) 3)
					      (setq temp (nth 1 temp)))
					 (setq match nil))
				     (setq matches (cons
						    (math-normalize
						     (math-multi-subst
						      (nth 2 temp)
						      args matches))
						    matches)
					   args (cons (nth 1 temp)
						      args)))
				    ((and (eq (car-safe (car conds))
					      'calcFunc-matches)
					  (= (length (car conds)) 3))
				     (setq temp (calcFunc-vmatches
						 (math-multi-subst
						  (nth 1 (car conds))
						  args matches)
						 (nth 2 (car conds))))
				     (if (eq temp 0)
					 (setq match nil)
				       (while (setq temp (cdr temp))
					 (setq matches (cons (nth 2 (car temp))
							     matches)
					       args (cons (nth 1 (car temp))
							  args)))))
				    (t
				     (or (math-is-true (math-simplify
							(math-multi-subst
							 (car conds)
							 args matches)))
					 (setq match nil))))
			      (setq conds (cdr conds)))
			    (if match
				(not (setq match (math-multi-subst
						  match args matches)))
			      (setq math-exp-old-pos save-exp-old-pos
				    math-exp-token save-exp-token
				    math-expr-data save-exp-data
				    math-exp-pos save-exp-pos)))))))
      (setq p (cdr p)))
    (and p match)))

;;;###autoload
(defun calc-match-user-syntax (p &optional term)
  (let ((matches nil)
	(save-exp-pos math-exp-pos)
	(save-exp-old-pos math-exp-old-pos)
	(save-exp-token math-exp-token)
	(save-exp-data math-expr-data)
        m)
    (while (and p
		(cond ((stringp (car p))
		       (and (equal math-expr-data (car p))
			    (progn
			      (math-read-token)
			      t)))
		      ((integerp (car p))
		       (and (setq m (catch 'syntax
				      (math-read-expr-level
				       (car p)
				       (if (cdr p)
					   (if (consp (nth 1 p))
					       (car (nth 1 (nth 1 p)))
					     (nth 1 p))
					 term))))
			    (not (stringp m))
			    (setq matches (nconc matches (list m)))))
		      ((eq (car (car p)) '\?)
		       (setq m (calc-match-user-syntax (nth 1 (car p))))
		       (or (nth 2 (car p))
			   (setq matches
				 (nconc matches
					(list
					 (cons 'vec (and (listp m) m))))))
		       (or (listp m) (not (nth 2 (car p)))
			   (not (eq (aref (car (nth 2 (car p))) 0) ?\$))
			   (eq math-exp-token 'end)))
		      (t
		       (setq m (calc-match-user-syntax (nth 1 (car p))
						       (car (nth 2 (car p)))))
		       (if (listp m)
			   (let ((vec (cons 'vec m))
				 opos mm)
			     (while (and (listp
					  (setq opos math-exp-pos
						mm (calc-match-user-syntax
						    (or (nth 2 (car p))
							(nth 1 (car p)))
						    (car (nth 2 (car p))))))
					 (> math-exp-pos opos))
			       (setq vec (nconc vec mm)))
			     (setq matches (nconc matches (list vec))))
			 (and (eq (car (car p)) '*)
			      (setq matches (nconc matches (list '(vec)))))))))
      (setq p (cdr p)))
    (if p
	(setq math-exp-pos save-exp-pos
	      math-exp-old-pos save-exp-old-pos
	      math-exp-token save-exp-token
	      math-expr-data save-exp-data
	      matches "Failed"))
    matches))

;;;###autoload
(defun math-remove-dashes (x)
  (if (string-match "\\`\\(.*\\)-\\(.*\\)\\'" x)
      (math-remove-dashes
       (concat (math-match-substring x 1) "#" (math-match-substring x 2)))
    x))

(defun math-remove-percentsigns (x)
  (if (string-match "\\`\\(.*\\)%\\(.*\\)\\'" x)
      (math-remove-percentsigns
       (concat (math-match-substring x 1) "o'o" (math-match-substring x 2)))
    x))

(defun math-restore-dashes (x)
  (if (string-match "\\`\\(.*\\)[#_]\\(.*\\)\\'" x)
      (math-restore-dashes
       (concat (math-match-substring x 1) "-" (math-match-substring x 2)))
    x))

(defun math-restore-placeholders (x)
  "Replace placeholders by the proper characters in the symbol x.
This includes `#' for `_' and `'' for `%'.
If the current Calc language does not use placeholders, return nil."
  (if (or (memq calc-language calc-lang-allow-underscores)
          (memq calc-language calc-lang-allow-percentsigns))
      (let ((sx (symbol-name x)))
        (when (memq calc-language calc-lang-allow-percentsigns)
          (require 'calccomp)
          (setq sx (math-to-percentsigns sx)))
        (if (memq calc-language calc-lang-allow-underscores)
            (setq sx (math-string-restore-underscores sx)))
        (intern-soft sx))))

(defun math-string-restore-underscores (x)
  "Replace pound signs by underscores in the string x."
  (if (string-match "\\`\\(.*\\)#\\(.*\\)\\'" x)
      (math-string-restore-underscores
       (concat (math-match-substring x 1) "_" (math-match-substring x 2)))
    x))

;;;###autoload
(defun math-read-if (cond op)
  (let ((then (math-read-expr-level 0)))
    (or (equal math-expr-data ":")
	(throw 'syntax "Expected ':'"))
    (math-read-token)
    (list 'calcFunc-if cond then (math-read-expr-level (nth 3 op)))))

(defun math-factor-after ()
  (let ((math-exp-pos math-exp-pos)
	math-exp-old-pos math-exp-token math-expr-data)
    (math-read-token)
    (or (memq math-exp-token '(number symbol dollar hash string))
	(and (assoc math-expr-data '(("-") ("+") ("!") ("|") ("/")))
	     (assoc (concat "u" math-expr-data) math-expr-opers))
	(eq (nth 2 (assoc math-expr-data math-expr-opers)) -1)
	(assoc math-expr-data '(("(") ("[") ("{"))))))

(defun math-read-factor ()
  (let ((math-expr-opers (math-expr-ops))
        op)
    (cond ((eq math-exp-token 'number)
	   (let ((num (math-read-number math-expr-data)))
	     (if (not num)
		 (progn
		   (setq math-exp-old-pos math-exp-pos)
		   (throw 'syntax "Bad format")))
	     (math-read-token)
	     (if (and math-read-expr-quotes
		      (consp num))
		 (list 'quote num)
	       num)))
	  ((and calc-user-parse-table
		(setq op (calc-check-user-syntax)))
	   op)
	  ((or (equal math-expr-data "-")
	       (equal math-expr-data "+")
	       (equal math-expr-data "!")
	       (equal math-expr-data "|")
	       (equal math-expr-data "/"))
	   (setq math-expr-data (concat "u" math-expr-data))
	   (math-read-factor))
	  ((and (setq op (assoc math-expr-data math-expr-opers))
		(eq (nth 2 op) -1))
	   (if (consp (nth 1 op))
	       (funcall (car (nth 1 op)) op)
	     (math-read-token)
	     (let ((val (math-read-expr-level (nth 3 op))))
	       (cond ((eq (nth 1 op) 'ident)
		      val)
		     ((and (Math-numberp val)
			   (equal (car op) "u-"))
		      (math-neg val))
		     (t (list (nth 1 op) val))))))
	  ((eq math-exp-token 'symbol)
	   (let ((sym (intern math-expr-data)))
	     (math-read-token)
	     (if (equal math-expr-data calc-function-open)
		 (let ((f (assq sym math-expr-function-mapping)))
		   (math-read-token)
		   (if (consp (cdr f))
		       (funcall (car (cdr f)) f sym)
		     (let ((args (if (or (equal math-expr-data calc-function-close)
					 (eq math-exp-token 'end))
				     nil
				   (math-read-expr-list))))
		       (if (not (or (equal math-expr-data calc-function-close)
				    (eq math-exp-token 'end)))
			   (throw 'syntax "Expected `)'"))
		       (math-read-token)
		       (if (and (memq calc-language
                                      calc-lang-parens-are-subscripts)
                                args
				(require 'calc-ext)
				(let ((calc-matrix-mode 'scalar))
				  (math-known-matrixp
				   (list 'var sym
					 (intern
					  (concat "var-"
						  (symbol-name sym)))))))
			   (math-parse-fortran-subscr sym args)
			 (if f
			     (setq sym (cdr f))
			   (and (= (aref (symbol-name sym) 0) ?\\)
				(< (prefix-numeric-value calc-language-option)
				   0)
				(setq sym (intern (substring (symbol-name sym)
							     1))))
			   (or (string-match "-" (symbol-name sym))
			       (setq sym (intern
					  (concat "calcFunc-"
						  (symbol-name sym))))))
			 (cons sym args)))))
	       (if math-read-expr-quotes
		   sym
		 (let ((val (list 'var
				  (intern (math-remove-dashes
					   (symbol-name sym)))
				  (if (string-match "-" (symbol-name sym))
				      sym
				    (intern (concat "var-"
						    (symbol-name sym)))))))
		   (let ((v (or
                             (assq (nth 1 val) math-expr-variable-mapping)
                             (assq (math-restore-placeholders (nth 1 val))
                                   math-expr-variable-mapping))))
		     (and v (setq val (if (consp (cdr v))
					  (funcall (car (cdr v)) v val)
					(list 'var
					      (intern
					       (substring (symbol-name (cdr v))
							  4))
					      (cdr v))))))
		   (while (and (memq calc-language
                                     calc-lang-brackets-are-subscripts)
			       (equal math-expr-data "["))
		     (math-read-token)
                     (let ((el (math-read-expr-list)))
                       (while el
                         (setq val (append (list 'calcFunc-subscr val)
                                           (list (car el))))
                         (setq el (cdr el))))
		     (if (equal math-expr-data "]")
			 (math-read-token)
		       (throw 'syntax "Expected ']'")))
		   val)))))
	  ((eq math-exp-token 'dollar)
	   (let ((abs (if (> math-expr-data 0) math-expr-data (- math-expr-data))))
	     (if (>= (length calc-dollar-values) abs)
		 (let ((num math-expr-data))
		   (math-read-token)
		   (setq calc-dollar-used (max calc-dollar-used num))
		   (math-check-complete (nth (1- abs) calc-dollar-values)))
	       (throw 'syntax (if calc-dollar-values
				  "Too many $'s"
				"$'s not allowed in this context")))))
	  ((eq math-exp-token 'hash)
	   (or calc-hashes-used
	       (throw 'syntax "#'s not allowed in this context"))
	   (require 'calc-ext)
	   (if (<= math-expr-data (length calc-arg-values))
	       (let ((num math-expr-data))
		 (math-read-token)
		 (setq calc-hashes-used (max calc-hashes-used num))
		 (nth (1- num) calc-arg-values))
	     (throw 'syntax "Too many # arguments")))
	  ((equal math-expr-data "(")
	   (let* ((exp (let ((math-exp-keep-spaces nil))
			 (math-read-token)
			 (if (or (equal math-expr-data "\\dots")
				 (equal math-expr-data "\\ldots"))
			     '(neg (var inf var-inf))
			   (math-read-expr-level 0)))))
	     (let ((math-exp-keep-spaces nil))
	       (cond
		((equal math-expr-data ",")
		 (progn
		   (math-read-token)
		   (let ((exp2 (math-read-expr-level 0)))
		     (setq exp
			   (if (and exp2 (Math-realp exp) (Math-realp exp2))
			       (math-normalize (list 'cplx exp exp2))
			     (list '+ exp (list '* exp2 '(var i var-i))))))))
		((equal math-expr-data ";")
		 (progn
		   (math-read-token)
		   (let ((exp2 (math-read-expr-level 0)))
		     (setq exp (if (and exp2 (Math-realp exp)
					(Math-anglep exp2))
				   (math-normalize (list 'polar exp exp2))
				 (require 'calc-ext)
				 (list '* exp
				       (list 'calcFunc-exp
					     (list '*
						   (math-to-radians-2 exp2)
						   '(var i var-i)))))))))
		((or (equal math-expr-data "\\dots")
		     (equal math-expr-data "\\ldots"))
		 (progn
		   (math-read-token)
		   (let ((exp2 (if (or (equal math-expr-data ")")
				       (equal math-expr-data "]")
				       (eq math-exp-token 'end))
				   '(var inf var-inf)
				 (math-read-expr-level 0))))
		     (setq exp
			   (list 'intv
				 (if (equal math-expr-data ")") 0 1)
				 exp
				 exp2)))))))
	     (if (not (or (equal math-expr-data ")")
			  (and (equal math-expr-data "]") (eq (car-safe exp) 'intv))
			  (eq math-exp-token 'end)))
		 (throw 'syntax "Expected `)'"))
	     (math-read-token)
	     exp))
	  ((eq math-exp-token 'string)
	   (require 'calc-ext)
	   (math-read-string))
	  ((equal math-expr-data "[")
	   (require 'calc-ext)
	   (math-read-brackets t "]"))
	  ((equal math-expr-data "{")
	   (require 'calc-ext)
	   (math-read-brackets nil "}"))
	  ((equal math-expr-data "<")
	   (require 'calc-ext)
	   (math-read-angle-brackets))
	  (t (throw 'syntax "Expected a number")))))

(provide 'calc-aent)

;; Local variables:
;; coding: utf-8
;; generated-autoload-file: "calc-loaddefs.el"
;; End:

;;; calc-aent.el ends here
