;;; calc-help.el --- help display functions for Calc,

;; Copyright (C) 1990-1993, 2001-2012  Free Software Foundation, Inc.

;; Author: David Gillespie <daveg@synaptics.com>
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

;; This file is autoloaded from calc-ext.el.

(require 'calc-ext)
(require 'calc-macs)

;; Declare functions which are defined elsewhere.
(declare-function Info-goto-node "info" (nodename &optional fork))
(declare-function Info-last "info" ())


(defun calc-help-prefix (arg)
  "This key is the prefix for Calc help functions.  See calc-help-for-help."
  (interactive "P")
  (or calc-dispatch-help (sit-for echo-keystrokes))
  (let ((key (calc-read-key-sequence
	      (if calc-dispatch-help
		  "Calc Help options: Help, Info, Tutorial, Summary; Key, Function; ?=more"
		(format "%s  (Type ? for a list of Calc Help options)"
			(key-description (this-command-keys))))
	      calc-help-map)))
    (setq key (lookup-key calc-help-map key))
    (message "")
    (if key
	(call-interactively key)
      (beep))))

(defun calc-help-for-help (arg)
  "You have typed `h', the Calc help character.  Type a Help option:

B  calc-describe-bindings.  Display a table of all key bindings.
H  calc-full-help.  Display all `?' key messages at once.

I  calc-info.  Read the Calc manual using the Info system.
T  calc-tutorial.  Read the Calc tutorial using the Info system.
S  calc-info-summary.  Read the Calc summary using the Info system.

C  calc-describe-key-briefly.  Look up the command name for a given key.
K  calc-describe-key.  Look up a key's documentation in the manual.
F  calc-describe-function.  Look up a function's documentation in the manual.
V  calc-describe-variable.  Look up a variable's documentation in the manual.

N  calc-view-news.  Display Calc history of changes.

C-c  Describe conditions for copying Calc.
C-d  Describe how you can get a new copy of Calc or report a bug.
C-w  Describe how there is no warranty for Calc."
  (interactive "P")
  (if calc-dispatch-help
      (let (key)
	(save-window-excursion
	  (describe-function 'calc-help-for-help)
	  (select-window (get-buffer-window "*Help*"))
	  (while (progn
		   (message "Calc Help options: Help, Info, ...  press SPC, DEL to scroll, C-g to cancel")
		   (memq (car (setq key (calc-read-key t)))
			 '(?  ?\C-h ?\C-? ?\C-v ?\M-v)))
	    (condition-case err
		(if (memq (car key) '(?  ?\C-v))
		    (scroll-up)
		  (scroll-down))
	      (error (beep)))))
	(calc-unread-command (cdr key))
	(calc-help-prefix nil))
    (let ((calc-dispatch-help t))
      (calc-help-prefix arg))))

(defun calc-describe-copying ()
  (interactive)
  (calc-info-goto-node "Copying"))

(defun calc-describe-distribution ()
  (interactive)
  (calc-info-goto-node "Reporting Bugs"))

(defun calc-describe-no-warranty ()
  (interactive)
  (calc-info-goto-node "Copying")
  (let ((case-fold-search nil))
    (search-forward "     NO WARRANTY"))
  (beginning-of-line)
  (recenter 0))

(defun calc-describe-bindings ()
  (interactive)
  (describe-bindings)
  (with-current-buffer "*Help*"
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (when (search-forward "Major Mode Bindings:" nil t)
        (delete-region (point-min) (point))
        (insert "Calc Mode Bindings:"))
      (when (search-forward "Global bindings:" nil t)
        (forward-line -1)
        (delete-region (point) (point-max)))
      (goto-char (point-min))
      (while
          (re-search-forward
           "\n[a-z] [0-9]\\( .*\n\\)\\([a-z] [0-9]\\1\\)*[a-z] \\([0-9]\\)\\1"
           nil t)
        (let ((dig1 (char-after (1- (match-beginning 1))))
              (dig2 (char-after (match-beginning 3))))
          (delete-region (match-end 1) (match-end 0))
          (goto-char (match-beginning 1))
          (delete-char -1)
          (delete-char 5)
          (insert (format "%c .. %c" (min dig1 dig2) (max dig1 dig2)))))
      (goto-char (point-min)))))

(defun calc-describe-key-briefly (key)
  (interactive "kDescribe key briefly: ")
  (calc-describe-key key t))

(defvar Info-history)

(defun calc-describe-key (key &optional briefly)
  (interactive "kDescribe key: ")
  (let ((defn (if (eq (key-binding key) 'calc-dispatch)
		  (let ((key2 (calc-read-key-sequence
			       (format "Describe key briefly: %s-"
				       (key-description key))
			       calc-dispatch-map)))
		    (setq key (concat key key2))
		    (lookup-key calc-dispatch-map key2))
		(if (eq (key-binding key) 'calc-help-prefix)
		    (let ((key2 (calc-read-key-sequence
				 (format "Describe key briefly: %s-"
					 (key-description key))
				 calc-help-map)))
		      (setq key (concat key key2))
		      (lookup-key calc-help-map key2))
		  (key-binding key))))
	(inv nil)
	(hyp nil)
        calc-summary-indentation)
    (while (or (equal key "I") (equal key "H"))
      (if (equal key "I")
	  (setq inv (not inv))
	(setq hyp (not hyp)))
      (setq key (read-key-sequence (format "Describe key%s:%s%s "
					   (if briefly " briefly" "")
					   (if inv " I" "")
					   (if hyp " H" "")))
	    defn (key-binding key)))
    (let ((desc (key-description key))
	  target)
      (if (string-match "^ESC " desc)
	  (setq desc (concat "M-" (substring desc 4))))
      (while (string-match "^M-# \\(ESC \\|C-\\)" desc)
	(setq desc (concat "M-# " (substring desc (match-end 0)))))
      (if (string-match "\\(DEL\\|\\LFD\\|RET\\|SPC\\|TAB\\)" desc)
          (setq desc (replace-match "<\\&>" nil nil desc)))
      (if briefly
	  (let ((msg (with-current-buffer (get-buffer-create "*Calc Summary*")
		       (if (= (buffer-size) 0)
			   (progn
			     (message "Reading Calc summary from manual...")
                             (require 'info nil t)
                             (with-temp-buffer
                               (Info-mode)
                               (Info-goto-node "(Calc)Summary")
                               (goto-char (point-min))
                               (forward-line 1)
                               (copy-to-buffer "*Calc Summary*"
                                               (point) (point-max)))
                             (setq buffer-read-only t)))
                       (goto-char (point-min))
                       (setq case-fold-search nil)
                       (re-search-forward "^\\(.*\\)\\[\\.\\. a b")
                       (setq calc-summary-indentation
                             (- (match-end 1) (match-beginning 1)))
		       (goto-char (point-min))
		       (setq target (if (and (string-match "[0-9]\\'" desc)
					     (not (string-match "[d#]" desc)))
					(concat (substring desc 0 -1) "0-9")
				      desc))
		       (if (re-search-forward
			    (format "\n%s%s%s%s[ a-zA-Z]"
				    (make-string (+ calc-summary-indentation 9)
						 ?\.)
				    (if (string-match "M-#" desc) "   "
				      (if inv
					  (if hyp "I H " "  I ")
					(if hyp "  H " "    ")))
				    (regexp-quote target)
				    (make-string (max (- 6 (length target)) 0)
						 ?\ ))
			    nil t)
			   (let (pt)
			     (beginning-of-line)
			     (forward-char calc-summary-indentation)
			     (setq pt (point))
			     (end-of-line)
			     (buffer-substring pt (point)))))))
	    (if msg
		(let ((args (substring msg 0 9))
		      (keys (substring msg 9 19))
		      (prompts (substring msg 19 38))
		      (notes "")
		      (cmd (substring msg 40))
		      msg)
		  (if (string-match "\\` +" args)
		      (setq args (substring args (match-end 0))))
		  (if (string-match " +\\'" args)
		      (setq args (substring args 0 (match-beginning 0))))
		  (if (string-match "\\` +" keys)
		      (setq keys (substring keys (match-end 0))))
		  (if (string-match " +\\'" keys)
		      (setq keys (substring keys 0 (match-beginning 0))))
		  (if (string-match " [0-9,]+\\'" prompts)
		      (setq notes (substring prompts (1+ (match-beginning 0)))
			    prompts (substring prompts 0 (match-beginning 0))))
		  (if (string-match " +\\'" prompts)
		      (setq prompts (substring prompts 0 (match-beginning 0))))
		  (if (string-match "\\` +" prompts)
		      (setq prompts (substring prompts (match-end 0))))
		  (setq msg (format
			     "%s:  %s%s`%s'%s%s %s%s"
			     (if (string-match
				  "\\`\\(calc-[-a-zA-Z0-9]+\\) *\\(.*\\)\\'"
				  cmd)
				 (prog1 (math-match-substring cmd 1)
				   (setq cmd (math-match-substring cmd 2)))
			       defn)
			     args (if (equal args "") "" " ")
			     keys
			     (if (equal prompts "") "" " ") prompts
			     (if (equal cmd "") "" " => ") cmd))
		  (message "%s%s%s runs %s%s"
			   (if inv "I " "") (if hyp "H " "") desc
			   msg
			   (if (equal notes "") ""
			     (format "  (?=notes %s)" notes)))
		  (let ((key (calc-read-key t)))
		    (if (eq (car key) ??)
			(if (equal notes "")
			    (message "No notes for this command")
			  (while (string-match "," notes)
			    (aset notes (match-beginning 0) ? ))
			  (setq notes (sort (car (read-from-string
						  (format "(%s)" notes)))
					    '<))
			  (with-output-to-temp-buffer "*Help*"
			    (princ (format "%s\n\n" msg))
			    (set-buffer "*Calc Summary*")
			    (re-search-forward "^ *NOTES")
			    (while notes
			      (re-search-forward
			       (format "^ *%d\\. " (car notes)))
			      (beginning-of-line)
			      (let ((pt (point)))
				(forward-line 1)
				(or (re-search-forward "^ ? ?[0-9]+\\. " nil t)
				    (goto-char (point-max)))
				(beginning-of-line)
				(princ (buffer-substring pt (point))))
			      (setq notes (cdr notes)))
			    (help-print-return-message)))
		      (calc-unread-command (cdr key)))))
	      (if (or (null defn) (integerp defn))
		  (message "%s is undefined" desc)
		(message "%s runs the command %s"
			 desc
			 (if (symbolp defn) defn (prin1-to-string defn))))))
	(if inv (setq desc (concat "I " desc)))
	(if hyp (setq desc (concat "H " desc)))
	(calc-describe-thing desc "Key Index" nil
			     (string-match "[A-Z][A-Z][A-Z]" desc))))))

(defvar calc-help-function-list nil
  "List of functions provided by Calc.")

(defvar calc-help-variable-list nil
  "List of variables provided by Calc.")

(defun calc-help-index-entries (&rest indices)
  "Create a list of entries from the INDICES in the Calc info manual."
  (let ((entrylist '())
        entry)
    (require 'info nil t)
    (while indices
      (condition-case nil
          (with-temp-buffer
            (Info-mode)
            (Info-goto-node (concat "(Calc)" (car indices) " Index"))
            (goto-char (point-min))
            (while (re-search-forward "\n\\* \\(.*\\): " nil t)
              (setq entry (match-string 1))
              (if (and (not (string-match "<[1-9]+>" entry))
                       (not (string-match "(.*)" entry))
                       (not (string= entry "Menu")))
                  (unless (assoc entry entrylist)
                    (setq entrylist (cons entry entrylist))))))
        (error nil))
      (setq indices (cdr indices)))
    entrylist))

(defun calc-describe-function (&optional func)
  (interactive)
  (unless calc-help-function-list
    (setq calc-help-function-list
          (calc-help-index-entries "Function" "Command")))
  (or func
      (setq func (completing-read "Describe function: "
                                  calc-help-function-list
                                  nil t)))
  (if (string-match "\\`calc-." func)
      (calc-describe-thing func "Command Index")
    (calc-describe-thing func "Function Index")))

(defun calc-describe-variable (&optional var)
  (interactive)
  (unless calc-help-variable-list
    (setq calc-help-variable-list
          (calc-help-index-entries "Variable")))
  (or var
      (setq var (completing-read "Describe variable: "
                                 calc-help-variable-list
                                 nil t)))
  (calc-describe-thing var "Variable Index"))

(defun calc-describe-thing (thing where &optional target not-quoted)
  (message "Looking for `%s' in %s..." thing where)
  (let ((savewin (current-window-configuration)))
    (calc-info-goto-node where)
    (or (let ((case-fold-search nil))
	  (re-search-forward (format "\n\\* +%s: \\(.*\\)\\."
				     (regexp-quote thing))
			     nil t))
	(and (string-match "\\`\\([a-z ]*\\)[0-9]\\'" thing)
	     (re-search-forward (format "\n\\* +%s[01]-9: \\(.*\\)\\."
					(substring thing 0 -1))
				nil t)
	     (setq thing (format "%s9" (substring thing 0 -1))))
	(progn
          (if Info-history
              (Info-last))
	  (set-window-configuration savewin)
	  (error "Can't find `%s' in %s" thing where)))
    (let (Info-history)
      (Info-goto-node (buffer-substring (match-beginning 1) (match-end 1))))
    (or (let ((case-fold-search nil))
	  (or (re-search-forward (format "\\[`%s'\\]\\|(`%s')\\|\\<The[ \n]`%s'"
                                         (or target thing)
                                         (or target thing)
                                         (or target thing)) nil t)
	      (and not-quoted
		   (let ((case-fold-search t))
		     (search-forward (or target thing) nil t)))
	      (search-forward (format "`%s'" (or target thing)) nil t)
	      (search-forward (or target thing) nil t)))
	(let ((case-fold-search t))
	  (or (re-search-forward (format "\\[`%s'\\]\\|(`%s')\\|\\<The[ \n]`%s'"
                                         (or target thing)
                                         (or target thing)
                                         (or target thing)) nil t)
	      (search-forward (format "`%s'" (or target thing)) nil t)
	      (search-forward (or target thing) nil t))))
    (beginning-of-line)
    (message "Found `%s' in %s" thing where)))

(defun calc-view-news ()
  (interactive)
  (let ((path load-path))
    (while (and path
		(not (file-exists-p (expand-file-name "calc.el" (car path)))))
      (setq path (cdr path)))
    (or (and path
	     (file-exists-p (expand-file-name "README" (car path))))
	(error "Can't locate Calc sources"))
    (calc-quit)
    (switch-to-buffer "*Help*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert-file-contents (expand-file-name "README" (car path)))
      (search-forward "Summary of changes")
      (forward-line -1)
      (delete-region (point-min) (point))
      (goto-char (point-min)))
    (help-mode)))

(defvar calc-help-long-names '((?b . "binary/business")
			       (?g . "graphics")
			       (?j . "selection")
			       (?k . "combinatorics/statistics")
			       (?u . "units/statistics")))

(defun calc-full-help ()
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ "GNU Emacs Calculator.\n")
    (princ "  By Dave Gillespie.\n")
    (princ (format "  %s\n\n" emacs-copyright))
    (princ "Type `h s' for a more detailed summary.\n")
    (princ "Or type `h i' to read the full Calc manual on-line.\n\n")
    (princ "Basic keys:\n")
    (let* ((calc-full-help-flag t))
      (mapc (function (lambda (x) (princ (format "  %s\n" x))))
	    (nreverse (cdr (reverse (cdr (calc-help))))))
      (mapc (function (lambda (prefix)
			(let ((msgs (condition-case err
					(funcall prefix)
				      (error nil))))
			  (if (car msgs)
			      (princ
			       (if (eq (nth 2 msgs) ?v)
				   "\n`v' or `V' prefix (vector/matrix) keys: \n"
				 (if (nth 2 msgs)
				     (format
				      "\n`%c' prefix (%s) keys:\n"
				      (nth 2 msgs)
				      (or (cdr (assq (nth 2 msgs)
						     calc-help-long-names))
					  (nth 1 msgs)))
				   (format "\n%s-modified keys:\n"
					   (capitalize (nth 1 msgs)))))))
			  (mapcar (function (lambda (x)
				    (princ (format "  %s\n" x))))
				  (car msgs)))))
	    '(calc-inverse-prefix-help
	      calc-hyperbolic-prefix-help
	      calc-inv-hyp-prefix-help
              calc-option-prefix-help
	      calc-a-prefix-help
	      calc-b-prefix-help
	      calc-c-prefix-help
	      calc-d-prefix-help
	      calc-f-prefix-help
	      calc-g-prefix-help
	      calc-h-prefix-help
	      calc-j-prefix-help
	      calc-k-prefix-help
	      calc-l-prefix-help
	      calc-m-prefix-help
	      calc-r-prefix-help
	      calc-s-prefix-help
	      calc-t-prefix-help
	      calc-u-prefix-help
	      calc-v-prefix-help
	      calc-shift-Y-prefix-help
	      calc-shift-Z-prefix-help
	      calc-z-prefix-help)))
    (help-print-return-message)))

(defun calc-h-prefix-help ()
  (interactive)
  (calc-do-prefix-help
   '("Help; Bindings; Info, Tutorial, Summary; News"
     "describe: Key, C (briefly), Function, Variable")
   "help" ?h))

(defun calc-inverse-prefix-help ()
  (interactive)
  (calc-do-prefix-help
   '("I + S (arcsin), C (arccos), T (arctan); Q (square)"
     "I + E (ln), L (exp), B (alog: B^X); f E (lnp1), f L (expm1)"
     "I + F (ceiling), R (truncate); a S (invert func)"
     "I + a m (match-not); c h (from-hms); k n (prev prime)"
     "I + f G (gamma-Q); f e (erfc); k B (etc., lower-tail dists)"
     "I + V S (reverse sort); V G (reverse grade)"
     "I + v s (remove subvec); v h (tail)"
     "I + t + (alt sum), t M (mean with error)"
     "I + t S (pop std dev), t C (pop covar)")
   "inverse" nil))

(defun calc-hyperbolic-prefix-help ()
  (interactive)
  (calc-do-prefix-help
   '("H + S (sinh), C (cosh), T (tanh); E (exp10), L (log10)"
     "H + F (float floor), R (float round); P (constant \"e\")"
     "H + a d (total derivative); k c (permutations)"
     "H + k b (bern-poly), k e (euler-poly); k s (stirling-2)"
     "H + f G (gamma-g), f B (beta-B); v h (rhead), v k (rcons)"
     "H + v e (expand w/filler); V H (weighted histogram)"
     "H + a S (general solve eqn), j I (general isolate)"
     "H + a R (widen/root), a N (widen/min), a X (widen/max)"
     "H + t M (median), t S (variance), t C (correlation coef)"
     "H + c f/F/c (pervasive float/frac/clean)")
   "hyperbolic" nil))

(defun calc-inv-hyp-prefix-help ()
  (interactive)
  (calc-do-prefix-help
   '("I H + S (arcsinh), C (arccosh), T (arctanh)"
     "I H + E (log10), L (exp10); f G (gamma-G)"
     "I H + F (float ceiling), R (float truncate)"
     "I H + t S (pop variance)"
     "I H + a S (general invert func); v h (rtail)")
   "inverse-hyperbolic" nil))

(defun calc-option-prefix-help ()
  (interactive)
  (calc-do-prefix-help
   '("")
   "option" nil))

(defun calc-f-prefix-help ()
  (interactive)
  (calc-do-prefix-help
   '("miN, maX; Hypot; Im, Re; Sign; [, ] (incr/decr)"
     "Gamma, Beta, Erf, besselJ, besselY"
     "SHIFT + int-sQrt; Int-log, Exp(x)-1, Ln(x+1); arcTan2"
     "SHIFT + Abssqr; Mantissa, eXponent, Scale"
     "SHIFT + incomplete: Gamma-P, Beta-I")
   "functions" ?f))


(defun calc-s-prefix-help ()
  (interactive)
  (calc-do-prefix-help
   '("Store, inTo, Xchg, Unstore; Recall, 0-9; : (:=); = (=>)"
     "Let; Copy, K=copy constant; Declare; Insert, Perm; Edit"
     "Negate, +, -, *, /, ^, &, |, [, ]; Map"
     "SHIFT + Decls, GenCount, TimeZone, Holidays; IntegLimit"
     "SHIFT + LineStyles, PointStyles, plotRejects; Units"
     "SHIFT + Eval-, AlgSimp-, ExtSimp-, FitRules")
   "store" ?s))

(defun calc-r-prefix-help ()
  (interactive)
  (calc-do-prefix-help
   '("digits 0-9: recall, same as `s r 0-9'"
     "Save to register, Insert from register")
   "recall/register" ?r))


(defun calc-j-prefix-help ()
  (interactive)
  (calc-do-prefix-help
   '("Select, Additional, Once; eVal, Formula; Rewrite"
     "More, Less, 1-9, Next, Previous"
     "Unselect, Clear; Display; Enable; Breakable"
     "' (replace), ` (edit), +, -, *, /, RET (grab), DEL"
     "SHIFT + swap: Left, Right; maybe: Select, Once"
     "SHIFT + Commute, Merge, Distrib, jump-Eqn, Isolate"
     "SHIFT + Negate, & (invert); Unpack")
   "select" ?j))


(defun calc-a-prefix-help ()
  (interactive)
  (calc-do-prefix-help
   '("Simplify, Extended-simplify, eVal; \" (exp-formula)"
     "eXpand, Collect, Factor, Apart, Norm-rat"
     "GCD, /, \\, % (polys); Polint"
     "Derivative, Integral, Taylor; _ (subscr)"
     "suBstitute; Rewrite, Match"
     "SHIFT + Solve; Root, miN, maX; Poly-roots; Fit"
     "SHIFT + Map; Tabulate, + (sum), * (prod); num-Integ"
     "relations: =, # (not =), <, >, [ (< or =), ] (> or =)"
     "logical: & (and), | (or), ! (not); : (if)"
     "misc: { (in-set); . (rmeq)")
   "algebra" ?a))


(defun calc-b-prefix-help ()
  (interactive)
  (calc-do-prefix-help
   '("And, Or, Xor, Diff, Not; Wordsize, Clip"
     "Lshift, Rshift, roTate; SHIFT + signed Lshift, Rshift"
     "SHIFT + business: Pv, Npv, Fv, pMt, #pmts, raTe, Irr"
     "SHIFT + business: Sln, sYd, Ddb; %ch")
   "binary/bus" ?b))


(defun calc-c-prefix-help ()
  (interactive)
  (calc-do-prefix-help
   '("Deg, Rad, HMS; Float; Polar/rect; Clean, 0-9; %"
     "SHIFT + Fraction")
   "convert" ?c))


(defun calc-d-prefix-help ()
  (interactive)
  (calc-do-prefix-help
   '("Group, \",\"; Normal, Fix, Sci, Eng, \".\"; Over"
     "Radix, Zeros, 2, 8, 0, 6; Hms; Date; Complex, I, J"
     "Why; Line-nums, line-Breaks; <, =, > (justify); Plain"
     "\" (strings); Truncate, [, ]; SPC (refresh), RET, @"
     "SHIFT + language: Normal, One-line, Big, Unformatted"
     "SHIFT + language: C, Pascal, Fortran; TeX, LaTeX, Eqn"
     "SHIFT + language: Yacas, X=Maxima, A=Giac"
     "SHIFT + language: Mathematica, W=Maple")
   "display" ?d))


(defun calc-g-prefix-help ()
  (interactive)
  (calc-do-prefix-help
   '("Fast; Add, Delete, Juggle; Plot, Clear; Quit"
     "Header, Name, Grid, Border, Key; View-commands, X-display"
     "x-axis: Range, Title, Log, Zero; lineStyle"
     "SHIFT + y-axis: Range, Title, Log, Zero; pointStyle"
     "SHIFT + Print; Device, Output-file; X-geometry"
     "SHIFT + Num-pts; Command, Kill, View-trail"
     "SHIFT + 3d: Fast, Add; CTRL + z-axis: Range, Title, Log")
   "graph" ?g))


(defun calc-k-prefix-help ()
  (interactive)
  (calc-do-prefix-help
   '("GCD, LCM; Choose (binomial), Double-factorial"
     "Random, random-Again, sHuffle"
     "Factors, Prime-test, Next-prime, Totient, Moebius"
     "Bernoulli, Euler, Stirling"
     "SHIFT + Extended-gcd"
     "SHIFT + dists: Binomial, Chi-square, F, Normal"
     "SHIFT + dists: Poisson, student's-T")
   "combinatorics" ?k))


(defun calc-m-prefix-help ()
  (interactive)
  (calc-do-prefix-help
   '("Deg, Rad, HMS; Frac; Polar; Inf; Alg, Total; Symb; Vec/mat"
     "Working; Xtensions; Mode-save; preserve Embedded modes"
     "SHIFT + Shifted-prefixes, mode-Filename; Record; reCompute"
     "SHIFT + simplify: Off, Num, Default, Bin, Alg, Ext, Units")
   "mode" ?m))


(defun calc-t-prefix-help ()
  (interactive)
  (calc-do-prefix-help
   '("Display; Fwd, Back; Next, Prev, Here, [, ]; Yank"
     "Search, Rev; In, Out; <, >; Kill; Marker; . (abbrev)"
     "SHIFT + time: Now; Part; Date, Julian, Unix, Czone"
     "SHIFT + time: newWeek, newMonth, newYear; Incmonth"
     "SHIFT + time: +, - (business days)"
     "digits 0-9: store-to, same as `s t 0-9'")
   "trail/time" ?t))


(defun calc-u-prefix-help ()
  (interactive)
  (calc-do-prefix-help
   '("Simplify, Convert, Temperature-convert, Base-units"
     "Autorange; Remove, eXtract; Explain; View-table; 0-9"
     "Define, Undefine, Get-defn, Permanent"
     "SHIFT + View-table-other-window"
     "SHIFT + stat: Mean, G-mean, Std-dev, Covar, maX, miN"
     "SHIFT + stat: + (sum), - (asum), * (prod), # (count)")
   "units/stat" ?u))

(defun calc-l-prefix-help ()
  (interactive)
  (calc-do-prefix-help
   '("Quantity, DB level, Np level"
     "+, -, *, /"
     "Scientific pitch notation, Midi number, Frequency"
     )
   "log units" ?l))

(defun calc-v-prefix-help ()
  (interactive)
  (calc-do-prefix-help
   '("Pack, Unpack, Identity, Diagonal, indeX, Build"
     "Row, Column, Subvector; Length; Find; Mask, Expand"
     "Transpose, Arrange, reVerse; Head, Kons; rNorm"
     "SHIFT + Det, & (inverse), LUD, Trace, conJtrn, Cross"
     "SHIFT + Sort, Grade, Histogram; cNorm"
     "SHIFT + Apply, Map, Reduce, accUm, Inner-, Outer-prod"
     "SHIFT + sets: V (union), ^ (intersection), - (diff)"
     "SHIFT + sets: Xor, ~ (complement), Floor, Enum"
     "SHIFT + sets: : (span), # (card), + (rdup)"
     "<, =, > (justification); , (commas); [, {, ( (brackets)"
     "} (matrix brackets); . (abbreviate); / (multi-lines)")
   "vec/mat" ?v))

(provide 'calc-help)

;;; calc-help.el ends here
