;;; calc-sel.el --- data selection functions for Calc

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

;;; Selection commands.

(defvar calc-keep-selection t)

(defvar calc-selection-cache-entry nil)
(defvar calc-selection-cache-num)
(defvar calc-selection-cache-comp)
(defvar calc-selection-cache-offset)
(defvar calc-selection-true-num)

(defun calc-select-here (num &optional once keep)
  (interactive "P")
  (calc-wrapper
   (calc-prepare-selection)
   (let ((found (calc-find-selected-part))
	 (entry calc-selection-cache-entry))
     (or (and keep (nth 2 entry))
	 (progn
	   (if once (progn
		      (setq calc-keep-selection nil)
		      (message "(Selection will apply to next command only)")))
	   (calc-change-current-selection
	    (if found
		(if (and num (> (setq num (prefix-numeric-value num)) 0))
		    (progn
		      (while (and (>= (setq num (1- num)) 0)
				  (not (eq found (car entry))))
			(setq found (calc-find-assoc-parent-formula
				     (car entry) found)))
		      found)
		  (calc-grow-assoc-formula (car entry) found))
	      (car entry))))))))

(defun calc-select-once (num)
  (interactive "P")
  (calc-select-here num t))

(defun calc-select-here-maybe (num)
  (interactive "P")
  (calc-select-here num nil t))

(defun calc-select-once-maybe (num)
  (interactive "P")
  (calc-select-here num t t))

(defun calc-select-additional ()
  (interactive)
  (calc-wrapper
   (let (calc-keep-selection)
     (calc-prepare-selection))
   (let ((found (calc-find-selected-part))
	 (entry calc-selection-cache-entry))
     (calc-change-current-selection
      (if found
	  (let ((sel (nth 2 entry)))
	    (if sel
		(progn
		  (while (not (or (eq sel (car entry))
				  (calc-find-sub-formula sel found)))
		    (setq sel (calc-find-assoc-parent-formula
			       (car entry) sel)))
		  sel)
	      (calc-grow-assoc-formula (car entry) found)))
	(car entry))))))

(defun calc-select-more (num)
  (interactive "P")
  (calc-wrapper
   (calc-prepare-selection)
   (let ((entry calc-selection-cache-entry))
     (if (nth 2 entry)
	 (let ((sel (nth 2 entry)))
	   (while (and (not (eq sel (car entry)))
		       (>= (setq num (1- (prefix-numeric-value num))) 0))
	     (setq sel (calc-find-assoc-parent-formula (car entry) sel)))
	   (calc-change-current-selection sel))
       (calc-select-here num)))))

(defun calc-select-less (num)
  (interactive "p")
  (calc-wrapper
   (calc-prepare-selection)
   (let ((found (calc-find-selected-part))
	 (entry calc-selection-cache-entry))
     (calc-change-current-selection
      (and found
	   (let ((sel (nth 2 entry))
		 old index op)
	     (while (and sel
			 (not (eq sel found))
			 (>= (setq num (1- num)) 0))
	       (setq old sel
		     index (calc-find-sub-formula sel found))
	       (and (setq sel (and index (nth index old)))
		    calc-assoc-selections
		    (setq op (assq (car-safe sel) calc-assoc-ops))
		    (memq (car old) (nth index op))
		    (setq num (1+ num))))
	     sel))))))

(defun calc-select-part (num)
  (interactive "P")
  (or num (setq num (- last-command-event ?0)))
  (calc-wrapper
   (calc-prepare-selection)
   (let ((sel (calc-find-nth-part (or (nth 2 calc-selection-cache-entry)
				      (car calc-selection-cache-entry))
				  num)))
     (if sel
	 (calc-change-current-selection sel)
       (error "%d is not a valid sub-formula index" num)))))

;; The variables calc-fnp-op and calc-fnp-num are local to 
;; calc-find-nth-part (and calc-select-previous) but used by 
;; calc-find-nth-part-rec, which is called by them.
(defvar calc-fnp-op)
(defvar calc-fnp-num)

(defun calc-find-nth-part (expr calc-fnp-num)
  (if (and calc-assoc-selections
	   (assq (car-safe expr) calc-assoc-ops))
      (let (calc-fnp-op)
	(calc-find-nth-part-rec expr))
    (if (eq (car-safe expr) 'intv)
	(and (>= calc-fnp-num 1) (<= calc-fnp-num 2) (nth (1+ calc-fnp-num) expr))
      (and (not (Math-primp expr)) (>= calc-fnp-num 1) (< calc-fnp-num (length expr))
	   (nth calc-fnp-num expr)))))

(defun calc-find-nth-part-rec (expr)   ; uses num, op
  (or (if (and (setq calc-fnp-op (assq (car-safe (nth 1 expr)) calc-assoc-ops))
	       (memq (car expr) (nth 1 calc-fnp-op)))
	  (calc-find-nth-part-rec (nth 1 expr))
	(and (= (setq calc-fnp-num (1- calc-fnp-num)) 0)
	     (nth 1 expr)))
      (if (and (setq calc-fnp-op (assq (car-safe (nth 2 expr)) calc-assoc-ops))
	       (memq (car expr) (nth 2 calc-fnp-op)))
	  (calc-find-nth-part-rec (nth 2 expr))
	(and (= (setq calc-fnp-num (1- calc-fnp-num)) 0)
	     (nth 2 expr)))))

(defun calc-select-next (num)
  (interactive "p")
  (if (< num 0)
      (calc-select-previous (- num))
    (calc-wrapper
     (calc-prepare-selection)
     (let* ((entry calc-selection-cache-entry)
	    (sel (nth 2 entry)))
       (if sel
	   (progn
	     (while (>= (setq num (1- num)) 0)
	       (let* ((parent (calc-find-parent-formula (car entry) sel))
		     (p parent)
		     op)
		 (and (eq p t) (setq p nil))
		 (while (and (setq p (cdr p))
			     (not (eq (car p) sel))))
		 (if (cdr p)
		     (setq sel (or (and calc-assoc-selections
					(setq op (assq (car-safe (nth 1 p))
						       calc-assoc-ops))
					(memq (car parent) (nth 2 op))
					(nth 1 (nth 1 p)))
				   (nth 1 p)))
		   (if (and calc-assoc-selections
			    (setq op (assq (car-safe parent) calc-assoc-ops))
			    (consp (setq p (calc-find-parent-formula
					    (car entry) parent)))
			    (eq (nth 1 p) parent)
			    (memq (car p) (nth 1 op)))
		       (setq sel (nth 2 p))
		     (error "No \"next\" sub-formula")))))
	     (calc-change-current-selection sel))
	 (if (Math-primp (car entry))
	     (calc-change-current-selection (car entry))
	   (calc-select-part num)))))))

(defun calc-select-previous (num)
  (interactive "p")
  (if (< num 0)
      (calc-select-next (- num))
    (calc-wrapper
     (calc-prepare-selection)
     (let* ((entry calc-selection-cache-entry)
	    (sel (nth 2 entry)))
       (if sel
	   (progn
	     (while (>= (setq num (1- num)) 0)
	       (let* ((parent (calc-find-parent-formula (car entry) sel))
		      (p (cdr-safe parent))
		      (prev nil)
		      op)
		 (if (eq (car-safe parent) 'intv) (setq p (cdr p)))
		 (while (and (not (eq (car p) sel))
			     (setq prev (car p)
				   p (cdr p))))
		 (if prev
		     (setq sel (or (and calc-assoc-selections
					(setq op (assq (car-safe prev)
						       calc-assoc-ops))
					(memq (car parent) (nth 1 op))
					(nth 2 prev))
				   prev))
		   (if (and calc-assoc-selections
			    (setq op (assq (car-safe parent) calc-assoc-ops))
			    (consp (setq p (calc-find-parent-formula
					    (car entry) parent)))
			    (eq (nth 2 p) parent)
			    (memq (car p) (nth 2 op)))
		       (setq sel (nth 1 p))
		     (error "No \"previous\" sub-formula")))))
	     (calc-change-current-selection sel))
	 (if (Math-primp (car entry))
	     (calc-change-current-selection (car entry))
	   (let ((len (if (and calc-assoc-selections
			       (assq (car (car entry)) calc-assoc-ops))
			  (let (calc-fnp-op (calc-fnp-num 0))
			    (calc-find-nth-part-rec (car entry))
			    (- 1 calc-fnp-num))
			(length (car entry)))))
	     (calc-select-part (- len num)))))))))

(defun calc-find-parent-formula (expr part)
  (cond ((eq expr part) t)
	((Math-primp expr) nil)
	(t
	 (let ((p expr) res)
	   (while (and (setq p (cdr p))
		       (not (setq res (calc-find-parent-formula
				       (car p) part)))))
	   (and p
		(if (eq res t) expr res))))))


(defun calc-find-assoc-parent-formula (expr part)
  (calc-grow-assoc-formula expr (calc-find-parent-formula expr part)))

(defun calc-grow-assoc-formula (expr part)
  (if calc-assoc-selections
      (let ((op (assq (car-safe part) calc-assoc-ops)))
	(if op
	    (let (new)
	      (while (and (consp (setq new (calc-find-parent-formula
					    expr part)))
			  (memq (car new)
				(nth (calc-find-sub-formula new part) op)))
		(setq part new))))
	part)
    part))

(defun calc-find-sub-formula (expr part)
  (cond ((eq expr part) t)
	((Math-primp expr) nil)
	(t
	 (let ((num 1))
	   (while (and (setq expr (cdr expr))
		       (not (calc-find-sub-formula (car expr) part)))
	     (setq num (1+ num)))
	   (and expr num)))))

(defun calc-unselect (num)
  (interactive "P")
  (calc-wrapper
   (calc-prepare-selection num)
   (calc-change-current-selection nil)))

(defun calc-clear-selections ()
  (interactive)
  (calc-wrapper
   (let ((limit (calc-stack-size))
	 (n 1))
     (while (<= n limit)
       (if (calc-top n 'sel)
	   (progn
	     (calc-prepare-selection n)
	     (calc-change-current-selection nil)))
       (setq n (1+ n))))
   (calc-clear-command-flag 'position-point)))

(defvar calc-highlight-selections-with-faces)

(defun calc-show-selections (arg)
  (interactive "P")
  (calc-wrapper
   (calc-preserve-point)
   (setq calc-show-selections (if arg
				  (> (prefix-numeric-value arg) 0)
				(not calc-show-selections)))
   (let ((p calc-stack))
     (while (and p
		 (or (null (nth 2 (car p)))
		     (equal (car p) calc-selection-cache-entry)))
       (setq p (cdr p)))
     (or (and p
	      (let ((calc-selection-cache-default-entry
		     calc-selection-cache-entry))
		(calc-do-refresh)))
	 (and calc-selection-cache-entry
	      (let ((sel (nth 2 calc-selection-cache-entry)))
		(setcar (nthcdr 2 calc-selection-cache-entry) nil)
		(calc-change-current-selection sel)))))
   (message (if calc-show-selections
                (if calc-highlight-selections-with-faces
                    "De-emphasizing all but selected part of formulas"
                  "Displaying only selected part of formulas")
              (if calc-highlight-selections-with-faces
                  "Emphasizing selected part of formulas"
                "Displaying all but selected part of formulas")))))

;; The variables calc-final-point-line and calc-final-point-column
;; are declared in calc.el, and are used throughout.
(defvar calc-final-point-line)
(defvar calc-final-point-column)

(defun calc-preserve-point ()
  (or (looking-at "\\.\n+\\'")
      (progn
	(setq calc-final-point-line (+ (count-lines (point-min) (point))
				       (if (bolp) 1 0))
	      calc-final-point-column (current-column))
	(calc-set-command-flag 'position-point))))

(defun calc-enable-selections (arg)
  (interactive "P")
  (calc-wrapper
   (calc-preserve-point)
   (setq calc-use-selections (if arg
				 (> (prefix-numeric-value arg) 0)
			       (not calc-use-selections)))
   (calc-set-command-flag 'renum-stack)
   (message (if calc-use-selections
		"Commands operate only on selected sub-formulas"
	      "Selections of sub-formulas have no effect"))))

(defun calc-break-selections (arg)
  (interactive "P")
  (calc-wrapper
   (calc-preserve-point)
   (setq calc-assoc-selections (if arg
				   (<= (prefix-numeric-value arg) 0)
				 (not calc-assoc-selections)))
   (message (if calc-assoc-selections
		"Selection treats a+b+c as a sum of three terms"
	      "Selection treats a+b+c as (a+b)+c"))))

(defun calc-prepare-selection (&optional num)
  (or num (setq num (calc-locate-cursor-element (point))))
  (setq calc-selection-true-num num
	calc-keep-selection t)
  (or (> num 0) (setq num 1))
  ;; (if (or (< num 1) (> num (calc-stack-size)))
  ;;     (error "Cursor must be positioned on a stack element"))
  (let* ((entry (calc-top num 'entry))
	 ww w)
    (or (equal entry calc-selection-cache-entry)
	(progn
	  (setcar entry (calc-encase-atoms (car entry)))
	  (setq calc-selection-cache-entry entry
		calc-selection-cache-num num
		calc-selection-cache-comp
		(let ((math-comp-tagged t))
		  (math-compose-expr (car entry) 0))
		calc-selection-cache-offset
		(+ (car (math-stack-value-offset calc-selection-cache-comp))
		   (length calc-left-label)
		   (if calc-line-numbering 4 0))))))
  (calc-preserve-point))

;;; The following ensures that no two subformulas will be "eq" to each other!
(defun calc-encase-atoms (x)
  (if (or (not (consp x))
	  (equal x '(float 0 0)))
      (list 'cplx x 0)
    (calc-encase-atoms-rec x)
    x))

(defun calc-encase-atoms-rec (x)
  (or (Math-primp x)
      (progn
	(if (eq (car x) 'intv)
	    (setq x (cdr x)))
	(while (setq x (cdr x))
	  (if (or (not (consp (car x)))
		  (equal (car x) '(float 0 0)))
	      (setcar x (list 'cplx (car x) 0))
	    (calc-encase-atoms-rec (car x)))))))

;; The variable math-comp-sel-tag is local to calc-find-selected-part,
;; but is used by math-comp-sel-flat-term and math-comp-add-string-sel
;; in calccomp.el, which are called (indirectly) by calc-find-selected-part.

(defun calc-find-selected-part ()
  (let* ((math-comp-sel-hpos (- (current-column) calc-selection-cache-offset))
	 toppt
	 (lcount 0)
	 (spaces 0)
	 (math-comp-sel-vpos (save-excursion
			       (beginning-of-line)
			       (let ((line (point)))
				 (calc-cursor-stack-index
				  calc-selection-cache-num)
				 (setq toppt (point))
				 (while (< (point) line)
				   (forward-line 1)
				   (setq spaces (+ spaces
						   (current-indentation))
					 lcount (1+ lcount)))
				 (- lcount (math-comp-ascent
					    calc-selection-cache-comp) -1))))
	 (math-comp-sel-cpos (- (point) toppt calc-selection-cache-offset
				spaces lcount))
	 (math-comp-sel-tag nil))
    (and (>= math-comp-sel-hpos 0)
	 (> calc-selection-true-num 0)
	 (math-composition-to-string calc-selection-cache-comp 1000000))
    (nth 1 math-comp-sel-tag)))

(defun calc-change-current-selection (sub-expr)
  (or (eq sub-expr (nth 2 calc-selection-cache-entry))
      (let ((calc-prepared-composition calc-selection-cache-comp)
	    (buffer-read-only nil)
	    top)
	(calc-set-command-flag 'renum-stack)
	(setcar (nthcdr 2 calc-selection-cache-entry) sub-expr)
	(calc-cursor-stack-index calc-selection-cache-num)
	(setq top (point))
	(calc-cursor-stack-index (1- calc-selection-cache-num))
	(delete-region top (point))
	(let ((calc-selection-cache-default-entry calc-selection-cache-entry))
	  (insert (math-format-stack-value calc-selection-cache-entry)
		  "\n")))))

(defun calc-top-selected (&optional n m)
  (and calc-any-selections
       calc-use-selections
       (progn
	 (or n (setq n 1))
	 (or m (setq m 1))
	 (calc-check-stack (+ n m -1))
	 (let ((top (nthcdr (+ m calc-stack-top -1) calc-stack))
	       (sel nil))
	   (while (>= (setq n (1- n)) 0)
	     (if (nth 2 (car top))
		 (setq sel (if sel t (nth 2 (car top)))))
	     (setq top (cdr top)))
	   sel))))

;; The variables calc-rsf-old and calc-rsf-new are local to
;; calc-replace-sub-formula, but used by calc-replace-sub-formula-rec,
;; which is called by calc-replace-sub-formula.
(defvar calc-rsf-old)
(defvar calc-rsf-new)

(defun calc-replace-sub-formula (expr calc-rsf-old calc-rsf-new)
  (setq calc-rsf-new (calc-encase-atoms calc-rsf-new))
  (calc-replace-sub-formula-rec expr))

(defun calc-replace-sub-formula-rec (expr)
  (cond ((eq expr calc-rsf-old) calc-rsf-new)
	((Math-primp expr) expr)
	(t
	 (cons (car expr)
	       (mapcar 'calc-replace-sub-formula-rec (cdr expr))))))

(defun calc-sel-error ()
  (error "Invalid operation on sub-formulas"))

(defun calc-replace-selections (n vals m)
  (if (calc-top-selected n m)
      (let ((num (length vals)))
	(calc-preserve-point)
	(cond
	 ((= n num)
	  (let* ((old (calc-top-list n m 'entry))
		 (new nil)
		 (sel nil)
		 val)
	    (while old
	      (if (nth 2 (car old))
		  (setq val (calc-encase-atoms (car vals))
			new (cons (calc-replace-sub-formula (car (car old))
							    (nth 2 (car old))
							    val)
				  new)
			sel (cons val sel))
		(setq new (cons (car vals) new)
		      sel (cons nil sel)))
	      (setq vals (cdr vals)
		    old (cdr old)))
	    (calc-pop-stack n m t)
	    (calc-push-list (nreverse new)
			    m (and calc-keep-selection (nreverse sel)))))
	 ((= num 1)
	  (let* ((old (calc-top-list n m 'entry))
		 more)
	    (while (and old (not (nth 2 (car old))))
	      (setq old (cdr old)))
	    (setq more old)
	    (while (and (setq more (cdr more)) (not (nth 2 (car more)))))
	    (and more
		 (calc-sel-error))
	    (calc-pop-stack n m t)
	    (if old
		(let ((val (calc-encase-atoms (car vals))))
		  (calc-push-list (list (calc-replace-sub-formula
					 (car (car old))
					 (nth 2 (car old))
					 val))
				  m (and calc-keep-selection (list val))))
	      (calc-push-list vals))))
	 (t (calc-sel-error))))
    (calc-pop-stack n m t)
    (calc-push-list vals m)))

(defun calc-delete-selection (n)
  (let ((entry (calc-top n 'entry)))
    (if (nth 2 entry)
	(if (eq (nth 2 entry) (car entry))
	    (progn
	      (calc-pop-stack 1 n t)
	      (calc-push-list '(0) n))
	  (let ((parent (calc-find-parent-formula (car entry) (nth 2 entry)))
		(repl nil))
	    (calc-preserve-point)
	    (calc-pop-stack 1 n t)
	    (cond ((or (memq (car parent) '(* / %))
		       (and (eq (car parent) '^)
			    (eq (nth 2 parent) (nth 2 entry))))
		   (setq repl 1))
		  ((memq (car parent) '(vec calcFunc-min calcFunc-max)))
		  ((and (assq (car parent) calc-tweak-eqn-table)
			(= (length parent) 3))
		   (setq repl 'del))
		  (t
		   (setq repl 0)))
	    (cond
	     ((eq repl 'del)
	      (calc-push-list (list
			       (calc-normalize
				(calc-replace-sub-formula
				 (car entry)
				 parent
				 (if (eq (nth 2 entry) (nth 1 parent))
				     (nth 2 parent)
				   (nth 1 parent)))))
			      n))
	     (repl
	      (calc-push-list (list
			       (calc-normalize
				(calc-replace-sub-formula (car entry)
							  (nth 2 entry)
							  repl)))
			      n))
	     (t
	      (calc-push-list (list
			       (calc-normalize
				(calc-replace-sub-formula (car entry)
							  parent
							  (delq (nth 2 entry)
								(copy-sequence
								 parent)))))
			      n)))))
      (calc-pop-stack 1 n t))))

(defun calc-roll-down-with-selections (n m)
  (let ((vals (append (calc-top-list m 1)
		      (calc-top-list (- n m) (1+ m))))
	(sels (append (calc-top-list m 1 'sel)
		      (calc-top-list (- n m) (1+ m) 'sel))))
    (calc-pop-push-list n vals 1 sels)))

(defun calc-roll-up-with-selections (n m)
  (let ((vals (append (calc-top-list (- n m) 1)
		      (calc-top-list m (- n m -1))))
	(sels (append (calc-top-list (- n m) 1 'sel)
		      (calc-top-list m (- n m -1) 'sel))))
    (calc-pop-push-list n vals 1 sels)))

;; The variable calc-sel-reselect is local to several functions
;; which call calc-auto-selection.
(defvar calc-sel-reselect)

(defun calc-auto-selection (entry)
  (or (nth 2 entry)
      (progn
	(setq calc-sel-reselect nil)
	(calc-prepare-selection)
	(calc-grow-assoc-formula (car entry) (calc-find-selected-part)))))

(defun calc-copy-selection ()
  (interactive)
  (calc-wrapper
   (calc-preserve-point)
   (let* ((num (max 1 (calc-locate-cursor-element (point))))
	  (entry (calc-top num 'entry)))
     (calc-push (or (calc-auto-selection entry) (car entry))))))

(defun calc-del-selection ()
  (interactive)
  (calc-wrapper
   (calc-preserve-point)
   (let* ((num (max 1 (calc-locate-cursor-element (point))))
	  (entry (calc-top num 'entry))
	  (sel (calc-auto-selection entry)))
     (setcar (nthcdr 2 entry) (and (not (eq sel (car entry))) sel))
     (calc-delete-selection num))))

(defvar calc-selection-history nil
  "History for calc selections.")

(defun calc-enter-selection ()
  (interactive)
  (calc-wrapper
   (calc-preserve-point)
   (let* ((num (max 1 (calc-locate-cursor-element (point))))
	  (calc-sel-reselect calc-keep-selection)
	  (entry (calc-top num 'entry))
	  (expr (car entry))
	  (sel (or (calc-auto-selection entry) expr))
	  alg)
     (let ((calc-dollar-values (list sel))
	   (calc-dollar-used 0))
       (setq alg (calc-do-alg-entry "" "Replace selection with: " nil 
                                    'calc-selection-history))
       (and alg
	    (progn
	      (setq alg (calc-encase-atoms (car alg)))
	      (calc-pop-push-record-list 1 "repl"
					 (list (calc-replace-sub-formula
						expr sel alg))
					 num
					 (list (and calc-sel-reselect alg))))))
     (calc-handle-whys))))

(defun calc-edit-selection ()
  (interactive)
  (calc-wrapper
   (calc-preserve-point)
   (let* ((num (max 1 (calc-locate-cursor-element (point))))
	  (calc-sel-reselect calc-keep-selection)
	  (entry (calc-top num 'entry))
	  (expr (car entry))
	  (sel (or (calc-auto-selection entry) expr))
	  alg)
     (let ((str (math-showing-full-precision
		 (math-format-nice-expr sel (frame-width)))))
       (calc-edit-mode (list 'calc-finish-selection-edit
			     num (list 'quote sel) calc-sel-reselect))
       (insert str "\n"))))
  (calc-show-edit-buffer))

(defvar calc-original-buffer)

;; The variable calc-edit-disp-trail is local to calc-edit-finish,
;; in calc-yank.el.
(defvar calc-edit-disp-trail)
(defvar calc-edit-top)

(defun calc-finish-selection-edit (num sel reselect)
  (let ((buf (current-buffer))
	(str (buffer-substring calc-edit-top (point-max)))
	(start (point)))
    (switch-to-buffer calc-original-buffer)
    (let ((val (math-read-expr str)))
      (if (eq (car-safe val) 'error)
	  (progn
	    (switch-to-buffer buf)
	    (goto-char (+ start (nth 1 val)))
	    (error (nth 2 val))))
      (calc-wrapper
       (calc-preserve-point)
       (if calc-edit-disp-trail
	   (calc-trail-display 1 t))
       (setq val (calc-encase-atoms (calc-normalize val)))
       (let ((expr (calc-top num 'full)))
	 (if (calc-find-sub-formula expr sel)
	     (calc-pop-push-record-list 1 "edit"
					(list (calc-replace-sub-formula
					       expr sel val))
					num
					(list (and reselect val)))
	   (calc-push val)
	   (error "Original selection has been lost")))))))

(defun calc-sel-evaluate (arg)
  (interactive "p")
  (calc-slow-wrapper
   (calc-preserve-point)
   (let* ((num (max 1 (calc-locate-cursor-element (point))))
	  (calc-sel-reselect calc-keep-selection)
	  (entry (calc-top num 'entry))
	  (sel (or (calc-auto-selection entry) (car entry))))
     (calc-with-default-simplification
      (let ((math-simplify-only nil))
	(calc-modify-simplify-mode arg)
	(let ((val (calc-encase-atoms (calc-normalize sel))))
	  (calc-pop-push-record-list 1 "jsmp"
				     (list (calc-replace-sub-formula
					    (car entry) sel val))
				     num
				     (list (and calc-sel-reselect val))))))
     (calc-handle-whys))))

(defun calc-sel-expand-formula (arg)
  (interactive "p")
  (calc-slow-wrapper
   (calc-preserve-point)
   (let* ((num (max 1 (calc-locate-cursor-element (point))))
	  (calc-sel-reselect calc-keep-selection)
	  (entry (calc-top num 'entry))
	  (sel (or (calc-auto-selection entry) (car entry))))
     (calc-with-default-simplification
      (let ((math-simplify-only nil))
	(calc-modify-simplify-mode arg)
	(let* ((math-expand-formulas (> arg 0))
	       (val (calc-normalize sel))
	       top)
	  (and (<= arg 0)
	       (setq top (math-expand-formula val))
	       (setq val (calc-normalize top)))
	  (setq val (calc-encase-atoms val))
	  (calc-pop-push-record-list 1 "jexf"
				     (list (calc-replace-sub-formula
					    (car entry) sel val))
				     num
				     (list (and calc-sel-reselect val))))))
     (calc-handle-whys))))

(defun calc-sel-mult-both-sides (arg &optional divide)
  (interactive "P")
  (calc-wrapper
   (calc-preserve-point)
   (let* ((no-simp (consp arg))
          (num (max 1 (calc-locate-cursor-element (point))))
	  (calc-sel-reselect calc-keep-selection)
	  (entry (calc-top num 'entry))
	  (expr (car entry))
	  (sel (or (calc-auto-selection entry) expr))
	  (func (car-safe sel))
	  alg lhs rhs)
     (setq alg (calc-with-default-simplification
		(car (calc-do-alg-entry ""
					(if divide
					    "Divide both sides by: "
					  "Multiply both sides by: ")
                                        nil 'calc-selection-history))))
     (and alg
	  (progn
	    (if (and (or (eq func '/)
			 (assq func calc-tweak-eqn-table))
		     (= (length sel) 3))
		(progn
		  (or (memq func '(/ calcFunc-eq calcFunc-neq))
		      (if (math-known-nonposp alg)
			  (progn
			    (setq func (nth 1 (assq func
						    calc-tweak-eqn-table)))
			    (or (math-known-negp alg)
				(message "Assuming this factor is nonzero")))
			(or (math-known-posp alg)
			    (if (math-known-nonnegp alg)
				(message "Assuming this factor is nonzero")
			      (message "Assuming this factor is positive")))))
		  (setq lhs (list (if divide '/ '*) (nth 1 sel) alg)
			rhs (list (if divide '/ '*) (nth 2 sel) alg))
		  (or no-simp
		      (progn
			(setq lhs (math-simplify lhs)
			      rhs (math-simplify rhs))
			(and (eq func '/)
			     (or (Math-equal (nth 1 sel) 1)
				 (Math-equal (nth 1 sel) -1))
;				 (and (memq (car-safe (nth 2 sel)) '(+ -))
;				      (memq (car-safe alg) '(+ -))))
                             (unless arg
                               (setq rhs (math-expand-term rhs))))))
                  (if (and arg (not no-simp))
                      (setq rhs (math-simplify
                                 (calcFunc-expand rhs (unless (= arg 0) arg)))))
		  (setq alg (calc-encase-atoms
			     (calc-normalize (list func lhs rhs)))))
	      (setq rhs (list (if divide '* '/) sel alg))
	      (or no-simp
		  (setq rhs (math-simplify rhs)))
	      (setq alg (calc-encase-atoms
			 (calc-normalize (if divide
					     (list '/ rhs alg)
					   (list '* alg rhs))))))
	    (calc-pop-push-record-list 1 (if divide "div" "mult")
				       (list (calc-replace-sub-formula
					      expr sel alg))
				       num
				       (list (and calc-sel-reselect alg)))))
     (calc-handle-whys))))

(defun calc-sel-div-both-sides (no-simp)
  (interactive "P")
  (calc-sel-mult-both-sides no-simp t))

(defun calc-sel-add-both-sides (no-simp &optional subtract)
  (interactive "P")
  (calc-wrapper
   (calc-preserve-point)
   (let* ((num (max 1 (calc-locate-cursor-element (point))))
	  (calc-sel-reselect calc-keep-selection)
	  (entry (calc-top num 'entry))
	  (expr (car entry))
	  (sel (or (calc-auto-selection entry) expr))
	  (func (car-safe sel))
	  alg lhs rhs)
     (setq alg (calc-with-default-simplification
		(car (calc-do-alg-entry ""
					(if subtract
					    "Subtract from both sides: "
					  "Add to both sides: ")
                                        nil 'calc-selection-history))))
     (and alg
	  (progn
	    (if (and (assq func calc-tweak-eqn-table)
		     (= (length sel) 3))
		(progn
		  (setq lhs (list (if subtract '- '+) (nth 1 sel) alg)
			rhs (list (if subtract '- '+) (nth 2 sel) alg))
		  (or no-simp
		      (setq lhs (math-simplify lhs)
			    rhs (math-simplify rhs)))
		  (setq alg (calc-encase-atoms
			     (calc-normalize (list func lhs rhs)))))
	      (setq rhs (list (if subtract '+ '-) sel alg))
	      (or no-simp
		  (setq rhs (math-simplify rhs)))
	      (setq alg (calc-encase-atoms
			 (calc-normalize (list (if subtract '- '+) alg rhs)))))
	    (calc-pop-push-record-list 1 (if subtract "sub" "add")
				       (list (calc-replace-sub-formula
					      expr sel alg))
				       num
				       (list (and calc-sel-reselect alg)))))
     (calc-handle-whys))))

(defun calc-sel-sub-both-sides (no-simp)
  (interactive "P")
  (calc-sel-add-both-sides no-simp t))

(provide 'calc-sel)

;;; calc-sel.el ends here
