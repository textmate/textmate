;;; calcsel2.el --- selection functions for Calc

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

;; The variable calc-keep-selection is declared and set in calc-sel.el.
(defvar calc-keep-selection)

;; The variable calc-sel-reselect is local to the methods below,
;; but is used by some functions in calc-sel.el which are called
;; by the functions below.

(defun calc-commute-left (arg)
  (interactive "p")
  (if (< arg 0)
      (calc-commute-right (- arg))
    (calc-wrapper
     (calc-preserve-point)
     (let ((num (max 1 (calc-locate-cursor-element (point))))
	   (calc-sel-reselect calc-keep-selection))
       (if (= arg 0) (setq arg nil))
       (while (or (null arg) (>= (setq arg (1- arg)) 0))
	 (let* ((entry (calc-top num 'entry))
		(expr (car entry))
		(sel (calc-auto-selection entry))
		parent new)
	   (or (and sel
		    (consp (setq parent (calc-find-assoc-parent-formula
					 expr sel))))
	       (error "No term is selected"))
	   (if (and calc-assoc-selections
		    (assq (car parent) calc-assoc-ops))
	       (let ((outer (calc-find-parent-formula parent sel)))
		 (if (eq sel (nth 2 outer))
		     (setq new (calc-replace-sub-formula
				parent outer
				(cond
				 ((memq (car outer)
					(nth 1 (assq (car-safe (nth 1 outer))
						     calc-assoc-ops)))
				  (let* ((other (nth 2 (nth 1 outer)))
					 (new (calc-build-assoc-term
					       (car (nth 1 outer))
					       (calc-build-assoc-term
						(car outer)
						(nth 1 (nth 1 outer))
						sel)
					       other)))
				    (setq sel (nth 2 (nth 1 new)))
				    new))
				 ((eq (car outer) '-)
				  (calc-build-assoc-term
				   '+
				   (setq sel (math-neg sel))
				   (nth 1 outer)))
				 ((eq (car outer) '/)
				  (calc-build-assoc-term
				   '*
				   (setq sel (calcFunc-div 1 sel))
				   (nth 1 outer)))
				 (t (calc-build-assoc-term
				     (car outer) sel (nth 1 outer))))))
		   (let ((next (calc-find-parent-formula parent outer)))
		     (if (not (and (consp next)
				   (eq outer (nth 2 next))
				   (eq (car next) (car outer))))
			 (setq new nil)
		       (setq new (calc-build-assoc-term
				  (car next)
				  sel
				  (calc-build-assoc-term
				   (car next) (nth 1 next) (nth 2 outer)))
			     sel (nth 1 new)
			     new (calc-replace-sub-formula
				  parent next new))))))
	     (if (eq (nth 1 parent) sel)
		 (setq new nil)
	       (let ((p (nthcdr (1- (calc-find-sub-formula parent sel))
				(setq new (copy-sequence parent)))))
		 (setcar (cdr p) (car p))
		 (setcar p sel))))
	   (if (null new)
	       (if arg
		   (error "Term is already leftmost")
		 (or calc-sel-reselect
		     (calc-pop-push-list 1 (list expr) num '(nil)))
		 (setq arg 0))
	     (calc-pop-push-record-list
	      1 "left"
	      (list (calc-replace-sub-formula expr parent new))
	      num
	      (list (and (or (not (eq arg 0)) calc-sel-reselect)
			 sel))))))))))

(defun calc-commute-right (arg)
  (interactive "p")
  (if (< arg 0)
      (calc-commute-left (- arg))
    (calc-wrapper
     (calc-preserve-point)
     (let ((num (max 1 (calc-locate-cursor-element (point))))
	   (calc-sel-reselect calc-keep-selection))
       (if (= arg 0) (setq arg nil))
       (while (or (null arg) (>= (setq arg (1- arg)) 0))
	 (let* ((entry (calc-top num 'entry))
		(expr (car entry))
		(sel (calc-auto-selection entry))
		parent new)
	   (or (and sel
		    (consp (setq parent (calc-find-assoc-parent-formula
					 expr sel))))
	       (error "No term is selected"))
	   (if (and calc-assoc-selections
		    (assq (car parent) calc-assoc-ops))
	       (let ((outer (calc-find-parent-formula parent sel)))
		 (if (eq sel (nth 1 outer))
		     (setq new (calc-replace-sub-formula
				parent outer
				(if (memq (car outer)
					  (nth 2 (assq (car-safe (nth 2 outer))
						       calc-assoc-ops)))
				    (let ((other (nth 1 (nth 2 outer))))
				      (calc-build-assoc-term
				       (car outer)
				       other
				       (calc-build-assoc-term
					(car (nth 2 outer))
					sel
					(nth 2 (nth 2 outer)))))
				  (let ((new (cond
					      ((eq (car outer) '-)
					       (calc-build-assoc-term
						'+
						(math-neg (nth 2 outer))
						sel))
					      ((eq (car outer) '/)
					       (calc-build-assoc-term
						'*
						(calcFunc-div 1 (nth 2 outer))
						sel))
					      (t (calc-build-assoc-term
						  (car outer)
						  (nth 2 outer)
						  sel)))))
				    (setq sel (nth 2 new))
				    new))))
		   (let ((next (calc-find-parent-formula parent outer)))
		     (if (not (and (consp next)
				   (eq outer (nth 1 next))))
			 (setq new nil)
		       (setq new (calc-build-assoc-term
				  (car outer)
				  (calc-build-assoc-term
				   (car next) (nth 1 outer) (nth 2 next))
				  sel)
			     sel (nth 2 new)
			     new (calc-replace-sub-formula
				  parent next new))))))
	     (if (eq (nth (1- (length parent)) parent) sel)
		 (setq new nil)
	       (let ((p (nthcdr (calc-find-sub-formula parent sel)
				(setq new (copy-sequence parent)))))
		 (setcar p (nth 1 p))
		 (setcar (cdr p) sel))))
	   (if (null new)
	       (if arg
		   (error "Term is already rightmost")
		 (or calc-sel-reselect
		     (calc-pop-push-list 1 (list expr) num '(nil)))
		 (setq arg 0))
	     (calc-pop-push-record-list
	      1 "rght"
	      (list (calc-replace-sub-formula expr parent new))
	      num
	      (list (and (or (not (eq arg 0)) calc-sel-reselect)
			 sel))))))))))

(defun calc-build-assoc-term (op lhs rhs)
  (cond ((and (eq op '+) (or (math-looks-negp rhs)
			     (and (eq (car-safe rhs) 'cplx)
				  (math-negp (nth 1 rhs))
				  (eq (nth 2 rhs) 0))))
	 (list '- lhs (math-neg rhs)))
	((and (eq op '-) (or (math-looks-negp rhs)
			     (and (eq (car-safe rhs) 'cplx)
				  (math-negp (nth 1 rhs))
				  (eq (nth 2 rhs) 0))))
	 (list '+ lhs (math-neg rhs)))
	((and (eq op '*) (and (eq (car-safe rhs) '/)
			      (or (math-equal-int (nth 1 rhs) 1)
				  (equal (nth 1 rhs) '(cplx 1 0)))))
	 (list '/ lhs (nth 2 rhs)))
	((and (eq op '/) (and (eq (car-safe rhs) '/)
			      (or (math-equal-int (nth 1 rhs) 1)
				  (equal (nth 1 rhs) '(cplx 1 0)))))
	 (list '/ lhs (nth 2 rhs)))
	(t (list op lhs rhs))))

(defun calc-sel-unpack ()
  (interactive)
  (calc-wrapper
   (calc-preserve-point)
   (let* ((num (max 1 (calc-locate-cursor-element (point))))
	  (calc-sel-reselect calc-keep-selection)
	  (entry (calc-top num 'entry))
	  (expr (car entry))
	  (sel (or (calc-auto-selection entry) expr)))
     (or (and (not (math-primp sel))
	      (= (length sel) 2))
	 (error "Selection must be a function of one argument"))
     (calc-pop-push-record-list 1 "unpk"
				(list (calc-replace-sub-formula
				       expr sel (nth 1 sel)))
				num
				(list (and calc-sel-reselect (nth 1 sel)))))))

(defun calc-sel-isolate ()
  (interactive)
  (calc-slow-wrapper
   (calc-preserve-point)
   (let* ((num (max 1 (calc-locate-cursor-element (point))))
	  (calc-sel-reselect calc-keep-selection)
	  (entry (calc-top num 'entry))
	  (expr (car entry))
	  (sel (or (calc-auto-selection entry) (error "No selection")))
	  (eqn sel)
	  soln)
     (while (and (or (consp (setq eqn (calc-find-parent-formula expr eqn)))
		     (error "Selection must be a member of an equation"))
		 (not (assq (car eqn) calc-tweak-eqn-table))))
     (setq soln (math-solve-eqn eqn sel calc-hyperbolic-flag))
     (or soln
	 (error "No solution found"))
     (setq soln (calc-encase-atoms
		 (if (eq (not (calc-find-sub-formula (nth 2 eqn) sel))
			 (eq (nth 1 soln) sel))
		     soln
		   (list (nth 1 (assq (car soln) calc-tweak-eqn-table))
			 (nth 2 soln)
			 (nth 1 soln)))))
     (calc-pop-push-record-list 1 "isol"
				(list (calc-replace-sub-formula
				       expr eqn soln))
				num
				(list (and calc-sel-reselect sel)))
     (calc-handle-whys))))

(defun calc-sel-commute (many)
  (interactive "P")
  (let ((calc-assoc-selections nil))
    (calc-rewrite-selection "CommuteRules" many "cmut"))
  (calc-set-mode-line))

(defun calc-sel-jump-equals (many)
  (interactive "P")
  (calc-rewrite-selection "JumpRules" many "jump"))

(defun calc-sel-distribute (many)
  (interactive "P")
  (calc-rewrite-selection "DistribRules" many "dist"))

(defun calc-sel-merge (many)
  (interactive "P")
  (calc-rewrite-selection "MergeRules" many "merg"))

(defun calc-sel-negate (many)
  (interactive "P")
  (calc-rewrite-selection "NegateRules" many "jneg"))

(defun calc-sel-invert (many)
  (interactive "P")
  (calc-rewrite-selection "InvertRules" many "jinv"))

(provide 'calcsel2)

;;; calcsel2.el ends here
