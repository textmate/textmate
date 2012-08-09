;;; calc-mode.el --- calculator modes for Calc

;; Copyright (C) 1990-1993, 2001-2012 Free Software Foundation, Inc.

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
(declare-function calc-embedded-save-original-modes "calc-embed" ())


(defun calc-line-numbering (n)
  (interactive "P")
  (calc-wrapper
   (message (if (calc-change-mode 'calc-line-numbering n t t)
		"Displaying stack level numbers"
	      "Hiding stack level numbers"))))

(defun calc-line-breaking (n)
  (interactive "P")
  (calc-wrapper
   (setq n (if n
	       (and (> (setq n (prefix-numeric-value n)) 0)
		    (or (< n 5)
			n))
	     (not calc-line-breaking)))
   (if (calc-change-mode 'calc-line-breaking n t)
       (if (integerp calc-line-breaking)
	   (message "Breaking lines longer than %d characters" n)
	 (message "Breaking long lines in Stack display"))
     (message "Not breaking long lines in Stack display"))))


(defun calc-left-justify (n)
  (interactive "P")
  (calc-wrapper
   (and n (setq n (prefix-numeric-value n)))
   (calc-change-mode '(calc-display-just calc-display-origin)
		     (list nil n) t)
   (if n
       (message "Displaying stack entries indented by %d" n)
     (message "Displaying stack entries left-justified"))))

(defun calc-center-justify (n)
  (interactive "P")
  (calc-wrapper
   (and n (setq n (prefix-numeric-value n)))
   (calc-change-mode '(calc-display-just calc-display-origin)
		     (list 'center n) t)
   (if n
       (message "Displaying stack entries centered on column %d" n)
     (message "Displaying stack entries centered in window"))))

(defun calc-right-justify (n)
  (interactive "P")
  (calc-wrapper
   (and n (setq n (prefix-numeric-value n)))
   (calc-change-mode '(calc-display-just calc-display-origin)
		     (list 'right n) t)
   (if n
       (message "Displaying stack entries right-justified to column %d" n)
     (message "Displaying stack entries right-justified in window"))))

(defun calc-left-label (s)
  (interactive "sLefthand label: ")
  (calc-wrapper
   (or (equal s "")
       (setq s (concat s " ")))
   (calc-change-mode 'calc-left-label s t)))

(defun calc-right-label (s)
  (interactive "sRighthand label: ")
  (calc-wrapper
   (or (equal s "")
       (setq s (concat " " s)))
   (calc-change-mode 'calc-right-label s t)))

(defun calc-auto-why (n)
  (interactive "P")
  (calc-wrapper
   (if n
       (progn
	 (setq n (prefix-numeric-value n))
	 (if (<= n 0) (setq n nil)
	   (if (> n 1) (setq n t))))
     (setq n (and (not (eq calc-auto-why t)) (if calc-auto-why t 1))))
   (calc-change-mode 'calc-auto-why n nil)
   (cond ((null n)
	  (message "User must press `w' to explain unsimplified results"))
	 ((eq n t)
	  (message "Automatically doing `w' to explain unsimplified results"))
	 (t
	  (message "Automatically doing `w' only for unusual messages")))))

(defun calc-group-digits (n)
  (interactive "P")
  (calc-wrapper
   (if n
       (progn
	 (setq n (prefix-numeric-value n))
	 (cond ((or (> n 0) (< n -1)))
	       ((= n -1)
		(setq n nil))
	       (t
		(setq n calc-group-digits))))
     (setq n (not calc-group-digits)))
   (calc-change-mode 'calc-group-digits n t)
   (cond ((null n)
	  (message "Grouping is off"))
	 ((integerp n)
	  (message "Grouping every %d digits" (math-abs n)))
	 (t
	  (message "Grouping is on")))))

(defun calc-group-char (ch)
  (interactive "cGrouping character: ")
  (calc-wrapper
   (or (>= ch 32)
       (error "Control characters not allowed for grouping"))
   (if (= ch ?\\)
       (setq ch "\\,")
     (setq ch (char-to-string ch)))
   (calc-change-mode 'calc-group-char ch calc-group-digits)
   (message "Digit grouping character is \"%s\"" ch)))

(defun calc-point-char (ch)
  (interactive "cCharacter to use as decimal point: ")
  (calc-wrapper
   (or (>= ch 32)
       (error "Control characters not allowed as decimal point"))
   (calc-change-mode 'calc-point-char (char-to-string ch) t)
   (message "Decimal point character is \"%c\"" ch)))

(defun calc-normal-notation (n)
  (interactive "P")
  (calc-wrapper
   (calc-change-mode 'calc-float-format
		     (let* ((val (if n (prefix-numeric-value n) 0))
			    (mode (/ (+ val 5000) 10000)))
		       (if (or (< val -5000) (> mode 3))
			   (error "Prefix out of range"))
		       (setq n (list (aref [float sci eng fix] mode)
				     (- (% (+ val 5000) 10000) 5000))))
		     t)
   (if (eq (nth 1 n) 0)
       (message "Displaying floating-point numbers normally")
     (if (> (nth 1 n) 0)
	 (message
	  "Displaying floating-point numbers with %d significant digits"
	  (nth 1 n))
       (message "Displaying floating-point numbers with (precision%d)"
		(nth 1 n))))))

(defun calc-fix-notation (n)
  (interactive "NDigits after decimal point: ")
  (calc-wrapper
   (calc-change-mode 'calc-float-format
		     (setq n (list 'fix (if n (prefix-numeric-value n) 0)))
		     t)
   (message "Displaying floats with %d digits after decimal"
	    (math-abs (nth 1 n)))))

(defun calc-sci-notation (n)
  (interactive "P")
  (calc-wrapper
   (calc-change-mode 'calc-float-format
		     (setq n (list 'sci (if n (prefix-numeric-value n) 0)))
		     t)
   (if (eq (nth 1 n) 0)
       (message "Displaying floats in scientific notation")
     (if (> (nth 1 n) 0)
	 (message "Displaying scientific notation with %d significant digits"
		  (nth 1 n))
       (message "Displaying scientific notation with (precision%d)"
		(nth 1 n))))))

(defun calc-eng-notation (n)
  (interactive "P")
  (calc-wrapper
   (calc-change-mode 'calc-float-format
		     (setq n (list 'eng (if n (prefix-numeric-value n) 0)))
		     t)
   (if (eq (nth 1 n) 0)
       (message "Displaying floats in engineering notation")
     (if (> (nth 1 n) 0)
	 (message "Displaying engineering notation with %d significant digits"
		  (nth 1 n))
       (message "Displaying engineering notation with (precision%d)"
		(nth 1 n))))))


(defun calc-truncate-stack (n &optional rel)
  (interactive "P")
  (calc-wrapper
   (let ((oldtop calc-stack-top)
	 (newtop calc-stack-top))
     (calc-record-undo (list 'set 'saved-stack-top calc-stack-top))
     (let ((calc-stack-top 0)
	   (nn (prefix-numeric-value n)))
       (setq newtop
	     (if n
		 (progn
		   (if rel
		       (setq nn (+ oldtop nn))
		     (if (< nn 0)
			 (setq nn (+ nn (calc-stack-size)))
		       (setq nn (1+ nn))))
		   (if (< nn 1)
		       1
		     (if (> nn (calc-stack-size))
			 (calc-stack-size)
		       nn)))
	       (max 1 (calc-locate-cursor-element (point)))))
       (if (= newtop oldtop)
	   ()
	 (calc-pop-stack 1 oldtop t)
	 (calc-push-list '(top-of-stack) newtop)
	 (if calc-line-numbering
	     (calc-refresh))))
     (calc-record-undo (list 'set 'saved-stack-top 0))
     (setq calc-stack-top newtop))))

(defun calc-truncate-up (n)
  (interactive "p")
  (calc-truncate-stack n t))

(defun calc-truncate-down (n)
  (interactive "p")
  (calc-truncate-stack (- n) t))

(defun calc-display-raw (arg)
  (interactive "P")
  (calc-wrapper
   (setq calc-display-raw (if calc-display-raw nil (if arg 0 t)))
   (calc-do-refresh)
   (if calc-display-raw
       (message "Press d ' again to cancel \"raw\" display mode"))))




;;; Mode commands.

(defun calc-save-modes ()
  (interactive)
  (calc-wrapper
   (let (pos
	 (vals (mapcar (function (lambda (v) (symbol-value (car v))))
		       calc-mode-var-list)))
     (unless calc-settings-file
       (error "No `calc-settings-file' specified"))
     (set-buffer (find-file-noselect (substitute-in-file-name
				      calc-settings-file)))
     (goto-char (point-min))
     (if (and (search-forward ";;; Mode settings stored by Calc" nil t)
	      (progn
		(beginning-of-line)
		(setq pos (point))
		(search-forward "\n;;; End of mode settings" nil t)))
	 (progn
	   (beginning-of-line)
	   (forward-line 1)
	   (delete-region pos (point)))
       (goto-char (point-max))
       (insert "\n\n")
       (forward-char -1))
     (insert ";;; Mode settings stored by Calc on " (current-time-string) "\n")
     (let ((list calc-mode-var-list))
       (while list
	 (let* ((v (car (car list)))
		(def (nth 1 (car list)))
		(val (car vals)))
	   (or (equal val def)
	       (progn
		 (insert "(setq " (symbol-name v) " ")
		 (if (and (or (listp val)
			      (symbolp val))
			  (not (memq val '(nil t))))
		     (insert "'"))
		 (insert (prin1-to-string val) ")\n"))))
	 (setq list (cdr list)
	       vals (cdr vals))))
     (run-hooks 'calc-mode-save-hook)
     (insert ";;; End of mode settings\n")
     (save-buffer)
     (if calc-embedded-info
         (calc-embedded-save-original-modes)))))

(defun calc-settings-file-name (name &optional arg)
  (interactive
   (list (read-file-name (format "Settings file name (normally %s): "
				 (abbreviate-file-name calc-settings-file)))
	 current-prefix-arg))
  (calc-wrapper
   (setq arg (if arg (prefix-numeric-value arg) 0))
   (if (string-equal (file-name-nondirectory name) "")
       (message "Calc settings file is \"%s\"" calc-settings-file)
     (if (< (math-abs arg) 2)
	 (let ((list calc-mode-var-list))
	   (while list
	     (set (car (car list)) (nth 1 (car list)))
	     (setq list (cdr list)))))
     (setq calc-settings-file name)
     (or (and
	  calc-settings-file
          (equal user-init-file calc-settings-file)
          (> arg 0))
	 (< arg 0)
	 (load name t)
	 (message "New file")))))

(defun math-get-modes-vec ()
  (list 'vec
	calc-internal-prec
	calc-word-size
	(calc-stack-size)
	calc-number-radix
	(+ (if (<= (nth 1 calc-float-format) 0)
	       (+ calc-internal-prec (nth 1 calc-float-format))
	     (nth 1 calc-float-format))
	   (cdr (assq (car calc-float-format)
		      '((float . 0) (sci . 10000)
			(eng . 20000) (fix . 30000)))))
	(cond ((eq calc-angle-mode 'rad) 2)
	      ((eq calc-angle-mode 'hms) 3)
	      (t 1))
	(if calc-symbolic-mode 1 0)
	(if calc-prefer-frac 1 0)
	(if (eq calc-complex-mode 'polar) 1 0)
	(cond ((eq calc-matrix-mode 'scalar) 0)
	      ((eq calc-matrix-mode 'matrix) -2)
	      ((eq calc-matrix-mode 'sqmatrix) -3)
	      (calc-matrix-mode)
	      (t -1))
	(cond ((eq calc-simplify-mode 'none) -1)
	      ((eq calc-simplify-mode 'num) 0)
	      ((eq calc-simplify-mode 'binary) 2)
	      ((eq calc-simplify-mode 'alg) 3)
	      ((eq calc-simplify-mode 'ext) 4)
	      ((eq calc-simplify-mode 'units) 5)
	      (t 1))
	(cond ((eq calc-infinite-mode 1) 0)
	      (calc-infinite-mode 1)
	      (t -1))))

(defun calc-get-modes (n)
  (interactive "P")
  (calc-wrapper
   (let ((modes (math-get-modes-vec)))
     (calc-enter-result 0 "mode"
			(if n
			    (if (and (>= (setq n (prefix-numeric-value n)) 1)
				     (< n (length modes)))
				(nth n modes)
			      (error "Prefix out of range"))
			  modes)))))

(defun calc-shift-prefix (arg)
  (interactive "P")
  (calc-wrapper
   (setq calc-shift-prefix (if arg
			       (> (prefix-numeric-value arg) 0)
			     (not calc-shift-prefix)))
   (calc-init-prefixes)
   (message (if calc-shift-prefix
		"Prefix keys are now case-insensitive"
	      "Prefix keys must be unshifted (except V, Z)"))))

(defun calc-mode-record-mode (n)
  (interactive "P")
  (calc-wrapper
   (calc-change-mode 'calc-mode-save-mode
		     (cond ((null n)
			    (cond ((not calc-embedded-info)
				   (if (eq calc-mode-save-mode 'save)
				       'local 'save))
				  ((eq calc-mode-save-mode 'local)  'edit)
				  ((eq calc-mode-save-mode 'edit)   'perm)
				  ((eq calc-mode-save-mode 'perm)   'global)
				  ((eq calc-mode-save-mode 'global) 'save)
				  ((eq calc-mode-save-mode 'save)   nil)
				  ((eq calc-mode-save-mode nil)     'local)))
			   ((= (setq n (prefix-numeric-value n)) 0) nil)
			   ((= n 2) 'edit)
			   ((= n 3) 'perm)
			   ((= n 4) 'global)
			   ((= n 5) 'save)
			   (t 'local)))
   (message "%s" 
	    (cond ((and (eq calc-mode-save-mode 'local) calc-embedded-info)
		   "Recording mode changes with [calc-mode: ...]")
		  ((eq calc-mode-save-mode 'edit)
		   "Recording mode changes with [calc-edit-mode: ...]")
		  ((eq calc-mode-save-mode 'perm)
		   "Recording mode changes with [calc-perm-mode: ...]")
		  ((eq calc-mode-save-mode 'global)
		   "Recording mode changes with [calc-global-mode: ...]")
		  ((eq calc-mode-save-mode 'save)
		   (format "Recording mode changes in \"%s\""
			   calc-settings-file))
		  (t
		   "Not recording mode changes permanently")))))

(defun calc-total-algebraic-mode (flag)
  (interactive "P")
  (calc-wrapper
   (if (eq calc-algebraic-mode 'total)
       (calc-algebraic-mode nil)
     (calc-change-mode '(calc-algebraic-mode calc-incomplete-algebraic-mode)
		       '(total nil))
     (use-local-map calc-alg-map)
     (message
      "All keys begin algebraic entry; use Meta (ESC) for Calc keys"))))

(defun calc-algebraic-mode (flag)
  (interactive "P")
  (calc-wrapper
   (if flag
       (calc-change-mode '(calc-algebraic-mode
			   calc-incomplete-algebraic-mode)
			 (list nil (not calc-incomplete-algebraic-mode)))
     (calc-change-mode '(calc-algebraic-mode calc-incomplete-algebraic-mode)
		       (list (not calc-algebraic-mode) nil)))
   (use-local-map calc-mode-map)
   (message (if calc-algebraic-mode
		"Numeric keys and ( and [ begin algebraic entry"
	      (if calc-incomplete-algebraic-mode
		  "Only ( and [ begin algebraic entry"
		"No keys except ' and $ begin algebraic entry")))))

(defun calc-symbolic-mode (n)
  (interactive "P")
  (calc-wrapper

   (message (if (calc-change-mode 'calc-symbolic-mode n nil t)
		"Inexact computations like sqrt(2) are deferred"
	      "Numerical computations are always done immediately"))))

(defun calc-infinite-mode (n)
  (interactive "P")
  (calc-wrapper
   (if (eq n 0)
       (progn
	 (calc-change-mode 'calc-infinite-mode 1)
	 (message "Computations like 1 / 0 produce \"inf\""))
     (message (if (calc-change-mode 'calc-infinite-mode n nil t)
		  "Computations like 1 / 0 produce \"uinf\""
		"Computations like 1 / 0 are left unsimplified")))))

(defun calc-matrix-mode (arg)
  (interactive "P")
  (calc-wrapper
   (calc-change-mode 'calc-matrix-mode
		     (cond ((eq arg 0) 'scalar)
			   ((< (prefix-numeric-value arg) 1)
			    (and (< (prefix-numeric-value arg) -1) 'matrix))
			   (arg 
                            (if (consp arg) 'sqmatrix
                              (prefix-numeric-value arg)))
			   ((eq calc-matrix-mode 'matrix) 'scalar)
			   ((eq calc-matrix-mode 'scalar) nil)
			   (t 'matrix)))
   (if (integerp calc-matrix-mode)
       (message "Variables are assumed to be %dx%d matrices"
		calc-matrix-mode calc-matrix-mode)
     (message (if (eq calc-matrix-mode 'matrix)
		  "Variables are assumed to be matrices"
                (if (eq calc-matrix-mode 'sqmatrix)
                    "Variables are assumed to be square matrices"
                  (if calc-matrix-mode
                      "Variables are assumed to be scalars (non-matrices)"
                    "Variables are not assumed to be matrix or scalar")))))))

(defun calc-set-simplify-mode (mode arg msg)
  (calc-change-mode 'calc-simplify-mode
		    (if arg
			(and (> (prefix-numeric-value arg) 0)
			     mode)
		      (and (not (eq calc-simplify-mode mode))
			   mode)))
  (message "%s" (if (eq calc-simplify-mode mode)
	       msg
	     "Default simplifications enabled")))

(defun calc-no-simplify-mode (arg)
  (interactive "P")
  (calc-wrapper
   (calc-set-simplify-mode 'none arg
			   "All default simplifications are disabled")))

(defun calc-num-simplify-mode (arg)
  (interactive "P")
  (calc-wrapper
   (calc-set-simplify-mode 'num arg
			   "Default simplifications apply only if arguments are numeric")))

(defun calc-default-simplify-mode (arg)
  (interactive "p")
  (cond ((= arg 1)
	 (calc-wrapper
	  (calc-set-simplify-mode
	   nil nil "Usual default simplifications are enabled")))
	((= arg 0) (calc-num-simplify-mode 1))
	((< arg 0) (calc-no-simplify-mode 1))
	((= arg 2) (calc-bin-simplify-mode 1))
	((= arg 3) (calc-alg-simplify-mode 1))
	((= arg 4) (calc-ext-simplify-mode 1))
	((= arg 5) (calc-units-simplify-mode 1))
	(t (error "Prefix argument out of range"))))

(defun calc-bin-simplify-mode (arg)
  (interactive "P")
  (calc-wrapper
   (calc-set-simplify-mode 'binary arg
			   (format "Binary simplification occurs by default (word size=%d)"
				   calc-word-size))))

(defun calc-alg-simplify-mode (arg)
  (interactive "P")
  (calc-wrapper
   (calc-set-simplify-mode 'alg arg
			   "Algebraic simplification occurs by default")))

(defun calc-ext-simplify-mode (arg)
  (interactive "P")
  (calc-wrapper
   (calc-set-simplify-mode 'ext arg
			   "Extended algebraic simplification occurs by default")))

(defun calc-units-simplify-mode (arg)
  (interactive "P")
  (calc-wrapper
   (calc-set-simplify-mode 'units arg
			   "Units simplification occurs by default")))

(defun calc-auto-recompute (arg)
  (interactive "P")
  (calc-wrapper
   (calc-change-mode 'calc-auto-recompute arg nil t)
   (calc-refresh-evaltos)
   (message (if calc-auto-recompute
		"Automatically recomputing `=>' forms when necessary"
	      "Not recomputing `=>' forms automatically"))))

(defun calc-working (n)
  (interactive "P")
  (calc-wrapper
   (cond ((consp n)
	  (calc-pop-push-record 0 "work"
				(cond ((eq calc-display-working-message t) 1)
				      (calc-display-working-message 2)
				      (t 0))))
	 ((eq n 2) (calc-change-mode 'calc-display-working-message 'lots))
	 ((eq n 0) (calc-change-mode 'calc-display-working-message nil))
	 ((eq n 1) (calc-change-mode 'calc-display-working-message t)))
   (cond ((eq calc-display-working-message t)
	  (message "\"Working...\" messages enabled"))
	 (calc-display-working-message
	  (message "Detailed \"Working...\" messages enabled"))
	 (t
	  (message "\"Working...\" messages disabled")))))

(defun calc-always-load-extensions ()
  (interactive)
  (calc-wrapper
   (if (setq calc-always-load-extensions (not calc-always-load-extensions))
       (message "Always loading extensions package")
     (message "Loading extensions package on demand only"))))


(defun calc-matrix-left-justify ()
  (interactive)
  (calc-wrapper
   (calc-change-mode 'calc-matrix-just nil t)
   (message "Matrix elements will be left-justified in columns")))

(defun calc-matrix-center-justify ()
  (interactive)
  (calc-wrapper
   (calc-change-mode 'calc-matrix-just 'center t)
   (message "Matrix elements will be centered in columns")))

(defun calc-matrix-right-justify ()
  (interactive)
  (calc-wrapper
   (calc-change-mode 'calc-matrix-just 'right t)
   (message "Matrix elements will be right-justified in columns")))

(defun calc-full-vectors (n)
  (interactive "P")
  (calc-wrapper
   (message (if (calc-change-mode 'calc-full-vectors n t t)
		"Displaying long vectors in full"
	      "Displaying long vectors in [a, b, c, ..., z] notation"))))

(defun calc-full-trail-vectors (n)
  (interactive "P")
  (calc-wrapper
   (message (if (calc-change-mode 'calc-full-trail-vectors n nil t)
		"Recording long vectors in full"
	      "Recording long vectors in [a, b, c, ..., z] notation"))))

(defun calc-break-vectors (n)
  (interactive "P")
  (calc-wrapper
   (message (if (calc-change-mode 'calc-break-vectors n t t)
		"Displaying vector elements one-per-line"
	      "Displaying vector elements all on one line"))))

(defun calc-vector-commas ()
  (interactive)
  (calc-wrapper
   (if (calc-change-mode 'calc-vector-commas (if calc-vector-commas nil ",") t)
       (message "Separating vector elements with \",\"")
     (message "Separating vector elements with spaces"))))

(defun calc-vector-brackets ()
  (interactive)
  (calc-wrapper
   (if (calc-change-mode 'calc-vector-brackets
			 (if (equal calc-vector-brackets "[]") nil "[]") t)
       (message "Surrounding vectors with \"[]\"")
     (message "Not surrounding vectors with brackets"))))

(defun calc-vector-braces ()
  (interactive)
  (calc-wrapper
   (if (calc-change-mode 'calc-vector-brackets
			 (if (equal calc-vector-brackets "{}") nil "{}") t)
       (message "Surrounding vectors with \"{}\"")
     (message "Not surrounding vectors with brackets"))))

(defun calc-vector-parens ()
  (interactive)
  (calc-wrapper
   (if (calc-change-mode 'calc-vector-brackets
			 (if (equal calc-vector-brackets "()") nil "()") t)
       (message "Surrounding vectors with \"()\"")
     (message "Not surrounding vectors with brackets"))))

(defun calc-matrix-brackets (arg)
  (interactive "sCode letters (R, O, C): ")
  (calc-wrapper
   (let ((code (append (and (string-match "[rR]" arg) '(R))
		       (and (string-match "[oO]" arg) '(O))
		       (and (string-match "[cC]" arg) '(C))
		       (and (string-match "[pP]" arg) '(P))))
	 (bad (string-match "[^rRoOcCpP ]" arg)))
     (if bad
	 (error "Unrecognized character: %c" (aref arg bad)))
     (calc-change-mode 'calc-matrix-brackets code t))))

(provide 'calc-mode)

;;; calc-mode.el ends here
