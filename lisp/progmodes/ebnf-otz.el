;;; ebnf-otz.el --- syntactic chart OpTimiZer

;; Copyright (C) 1999-2012  Free Software Foundation, Inc.

;; Author: Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Maintainer: Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Keywords: wp, ebnf, PostScript
;; Version: 1.0
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
;; This package defines an optimizer for ebnf2ps.
;;
;; See ebnf2ps.el for documentation.
;;
;;
;; Optimizations
;; -------------
;;
;;
;; *To be implemented*:
;;    left recursion:
;;    A = B | A C B | A C D.   ==>   A = B {C (B | D)}*.
;;
;;    right recursion:
;;    A = B | C A.             ==>   A = {C}* B.
;;    A = B | D | C A | E A.   ==>   A = { C | E }* ( B | D ).
;;
;;    optional:
;;    A = B | C B.             ==>   A = [C] B.
;;    A = B | B C.             ==>   A = B [C].
;;    A = D | B D | B C D.     ==>   A = [B [C]] D.
;;
;;
;; *Already implemented*:
;;    left recursion:
;;    A = B | A C.             ==>   A = B {C}*.
;;    A = B | A B.             ==>   A = {B}+.
;;    A =   | A B.             ==>   A = {B}*.
;;    A = B | A C B.           ==>   A = {B || C}+.
;;    A = B | D | A C | A E.   ==>   A = ( B | D ) { C | E }*.
;;
;;    optional:
;;    A = B | .                ==>   A = [B].
;;    A =   | B .              ==>   A = [B].
;;
;;    factorization:
;;    A = B C | B D.           ==>   A = B (C | D).
;;    A = C B | D B.           ==>   A = (C | D) B.
;;    A = B C E | B D E.       ==>   A = B (C | D) E.
;;
;;    none:
;;    A = B | C | .            ==>   A = B | C | .
;;    A = B | C A D.           ==>   A = B | C A D.
;;
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:


(require 'ebnf2ps)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar ebnf-empty-rule-list nil
  "List of empty rule name.")


(defun ebnf-add-empty-rule-list (rule)
  "Add empty RULE in `ebnf-empty-rule-list'."
  (and ebnf-ignore-empty-rule
       (eq (ebnf-node-kind (ebnf-node-production rule))
	   'ebnf-generate-empty)
       (setq ebnf-empty-rule-list (cons (ebnf-node-name rule)
					ebnf-empty-rule-list))))


(defun ebnf-otz-initialize ()
  "Initialize optimizer."
  (setq ebnf-empty-rule-list nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eliminate empty rules


(defun ebnf-eliminate-empty-rules (syntax-list)
  "Eliminate empty rules."
  (while ebnf-empty-rule-list
    (let ((ebnf-total (length syntax-list))
	  (ebnf-nprod 0)
	  (prod-list syntax-list)
	  new-list before)
      (while prod-list
	(ebnf-message-info "Eliminating empty rules")
	(let ((rule (car prod-list)))
	  ;; if any non-terminal pertains to ebnf-empty-rule-list
	  ;; then eliminate non-terminal from rule
	  (if (ebnf-eliminate-empty rule)
	      (setq before prod-list)
	    ;; eliminate empty rule from syntax-list
	    (setq new-list (cons (ebnf-node-name rule) new-list))
	    (if before
		(setcdr before (cdr prod-list))
	      (setq syntax-list (cdr syntax-list)))))
	(setq prod-list (cdr prod-list)))
      (setq ebnf-empty-rule-list new-list)))
  syntax-list)


;; [production   width-func entry height width name production action]
;; [sequence     width-func entry height width list]
;; [alternative  width-func entry height width list]
;; [non-terminal width-func entry height width name default]
;; [empty        width-func entry height width]
;; [terminal     width-func entry height width name default]
;; [special      width-func entry height width name default]

(defun ebnf-eliminate-empty (rule)
  (let ((kind (ebnf-node-kind rule)))
    (cond
     ;; non-terminal
     ((eq kind 'ebnf-generate-non-terminal)
      (if (member (ebnf-node-name rule) ebnf-empty-rule-list)
	  nil
	rule))
     ;; sequence
     ((eq kind 'ebnf-generate-sequence)
      (let ((seq    (ebnf-node-list rule))
	    (header (ebnf-node-list rule))
	    before elt)
	(while seq
	  (setq elt (car seq))
	  (if (ebnf-eliminate-empty elt)
	      (setq before seq)
	    (if before
		(setcdr before (cdr seq))
	      (setq header (cdr header))))
	  (setq seq (cdr seq)))
	(when header
	  (ebnf-node-list rule header)
	  rule)))
     ;; alternative
     ((eq kind 'ebnf-generate-alternative)
      (let ((seq    (ebnf-node-list rule))
	    (header (ebnf-node-list rule))
	    before elt)
	(while seq
	  (setq elt (car seq))
	  (if (ebnf-eliminate-empty elt)
	      (setq before seq)
	    (if before
		(setcdr before (cdr seq))
	      (setq header (cdr header))))
	  (setq seq (cdr seq)))
	(when header
	  (if (= (length header) 1)
	      (car header)
	    (ebnf-node-list rule header)
	    rule))))
     ;; production
     ((eq kind 'ebnf-generate-production)
      (let ((prod (ebnf-eliminate-empty (ebnf-node-production rule))))
	(when prod
	  (ebnf-node-production rule prod)
	  rule)))
     ;; terminal, special and empty
     (t
      rule)
     )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Optimizations


;; *To be implemented*:
;;    left recursion:
;;    A = B | A C B | A C D.   ==>   A = B {C (B | D)}*.

;;    right recursion:
;;    A = B | C A.             ==>   A = {C}* B.
;;    A = B | D | C A | E A.   ==>   A = { C | E }* ( B | D ).

;;    optional:
;;    A = B | C B.             ==>   A = [C] B.
;;    A = B | B C.             ==>   A = B [C].
;;    A = D | B D | B C D.     ==>   A = [B [C]] D.


;; *Already implemented*:
;;    left recursion:
;;    A = B | A C.             ==>   A = B {C}*.
;;    A = B | A B.             ==>   A = {B}+.
;;    A =   | A B.             ==>   A = {B}*.
;;    A = B | A C B.           ==>   A = {B || C}+.
;;    A = B | D | A C | A E.   ==>   A = ( B | D ) { C | E }*.

;;    optional:
;;    A = B | .                ==>   A = [B].
;;    A =   | B .              ==>   A = [B].

;;    factorization:
;;    A = B C | B D.           ==>   A = B (C | D).
;;    A = C B | D B.           ==>   A = (C | D) B.
;;    A = B C E | B D E.       ==>   A = B (C | D) E.

;;    none:
;;    A = B | C | .            ==>   A = B | C | .
;;    A = B | C A D.           ==>   A = B | C A D.

(defun ebnf-optimize (syntax-list)
  "Syntactic chart optimizer."
  (if (not ebnf-optimize)
      syntax-list
    (let ((ebnf-total (length syntax-list))
	  (ebnf-nprod 0)
	  new)
      (while syntax-list
	(setq new (cons (ebnf-optimize1 (car syntax-list)) new)
	      syntax-list (cdr syntax-list)))
      (nreverse new))))


;; left recursion:
;; 1.  A = B | A C.             ==>   A = B {C}*.
;; 2.  A = B | A B.             ==>   A = {B}+.
;; 3.  A =   | A B.             ==>   A = {B}*.
;; 4.  A = B | A C B.           ==>   A = {B || C}+.
;; 5.  A = B | D | A C | A E.   ==>   A = ( B | D ) { C | E }*.

;; optional:
;; 6.  A = B | .                ==>   A = [B].
;; 7.  A =   | B .              ==>   A = [B].

;; factorization:
;; 8.  A = B C | B D.           ==>   A = B (C | D).
;; 9.  A = C B | D B.           ==>   A = (C | D) B.
;; 10. A = B C E | B D E.       ==>   A = B (C | D) E.

(defun ebnf-optimize1 (prod)
  (ebnf-message-info "Optimizing syntactic chart")
  (let ((production (ebnf-node-production prod)))
    (and (eq (ebnf-node-kind production) 'ebnf-generate-alternative)
	 (let* ((hlist (ebnf-split-header-prefix
			(ebnf-node-list production)
			(ebnf-node-name prod)))
		(nlist (car hlist))
		(zlist (cdr hlist))
		(elist (ebnf-split-header-suffix nlist zlist)))
	   (ebnf-node-production
	    prod
	    (cond
	     ;; cases 2., 4.
	     (elist
	      (and (eq elist t)
		   (setq elist nil))
	      (setq elist (or (ebnf-prefix-suffix elist)
			      elist))
	      (let* ((nl (ebnf-extract-empty nlist))
		     (el (or (ebnf-prefix-suffix (cdr nl))
			     (ebnf-create-alternative (cdr nl)))))
		(if (car nl)
		    (ebnf-make-zero-or-more el elist)
		  (ebnf-make-one-or-more el elist))))
	     ;; cases 1., 3., 5.
	     (zlist
	      (let* ((xlist (cdr (ebnf-extract-empty zlist)))
		     (znode (ebnf-make-zero-or-more
			     (or (ebnf-prefix-suffix xlist)
				 (ebnf-create-alternative xlist))))
		     (nnode (ebnf-map-list-to-optional nlist)))
		(and nnode
		     (setq nlist (list nnode)))
		(if (or (null nlist)
			(and (= (length nlist) 1)
			     (eq (ebnf-node-kind (car nlist))
				 'ebnf-generate-empty)))
		    znode
		  (ebnf-make-sequence
		   (list (or (ebnf-prefix-suffix nlist)
			     (ebnf-create-alternative nlist))
			 znode)))))
	     ;; cases 6., 7.
	     ((ebnf-map-node-to-optional production)
	      )
	     ;; cases 8., 9., 10.
	     ((ebnf-prefix-suffix nlist)
	      )
	     ;; none
	     (t
	      production)
	     ))))
    prod))


(defun ebnf-split-header-prefix (node-list header)
  (let* ((hlist (ebnf-split-header-prefix1 node-list header))
	 (nlist (car hlist))
	 zlist empty-p)
    (while (setq hlist (cdr hlist))
      (let ((elt (car hlist)))
	(if (eq (ebnf-node-kind elt) 'ebnf-generate-sequence)
	    (setq zlist (cons
			 (let ((seq (cdr (ebnf-node-list elt))))
			   (if (= (length seq) 1)
			       (car seq)
			     (ebnf-node-list elt seq)
			     elt))
			 zlist))
	  (setq empty-p t))))
    (and empty-p
	 (setq zlist (cons (ebnf-make-empty)
			   zlist)))
    (cons nlist (nreverse zlist))))


(defun ebnf-split-header-prefix1 (node-list header)
  (let (hlist nlist)
    (while node-list
      (if (ebnf-node-equal-header (car node-list) header)
	  (setq hlist (cons (car node-list) hlist))
	(setq nlist (cons (car node-list) nlist)))
      (setq node-list (cdr node-list)))
    (cons (nreverse nlist) (nreverse hlist))))


(defun ebnf-node-equal-header (node header)
  (let ((kind (ebnf-node-kind node)))
    (cond
     ((eq kind 'ebnf-generate-sequence)
      (ebnf-node-equal-header (car (ebnf-node-list node)) header))
     ((eq kind 'ebnf-generate-non-terminal)
      (string= (ebnf-node-name node) header))
     (t
      nil)
     )))


(defun ebnf-map-node-to-optional (node)
  (and (eq (ebnf-node-kind node) 'ebnf-generate-alternative)
       (ebnf-map-list-to-optional (ebnf-node-list node))))


(defun ebnf-map-list-to-optional (nlist)
  (and (= (length nlist) 2)
       (let ((first  (nth 0 nlist))
	     (second (nth 1 nlist)))
	 (cond
	  ;; empty second
	  ((eq (ebnf-node-kind first) 'ebnf-generate-empty)
	   (ebnf-make-optional second))
	  ;; first empty
	  ((eq (ebnf-node-kind second) 'ebnf-generate-empty)
	   (ebnf-make-optional first))
	  ;; first second
	  (t
	   nil)
	  ))))


(defun ebnf-extract-empty (elist)
  (let ((now elist)
	before empty-p)
    (while now
      (if (not (eq (ebnf-node-kind (car now)) 'ebnf-generate-empty))
	  (setq before now)
	(setq empty-p t)
	(if before
	    (setcdr before (cdr now))
	  (setq elist (cdr elist))))
      (setq now (cdr now)))
    (cons empty-p elist)))


(defun ebnf-split-header-suffix (nlist zlist)
  (let (new empty-p)
    (and (cond
	  ((= (length nlist) 1)
	   (let ((ok t)
		 (elt (car nlist)))
	     (while (and ok zlist)
	       (setq ok    (ebnf-split-header-suffix1 elt (car zlist))
		     zlist (cdr zlist))
	       (if (eq ok t)
		   (setq empty-p t)
		 (setq new (cons ok new))))
	     ok))
	  ((= (length nlist) (length zlist))
	   (let ((ok t))
	     (while (and ok zlist)
	       (setq ok    (ebnf-split-header-suffix1 (car nlist) (car zlist))
		     nlist (cdr nlist)
		     zlist (cdr zlist))
	       (if (eq ok t)
		   (setq empty-p t)
		 (setq new (cons ok new))))
	     ok))
	  (t
	   nil)
	  )
	 (let* ((lis (ebnf-unique-list new))
		(len (length lis)))
	   (cond
	    ((zerop len)
	     t)
	    ((= len 1)
	     (setq lis (car lis))
	     (if empty-p
		 (ebnf-make-optional lis)
	       lis))
	    (t
	     (and empty-p
		  (setq lis (cons (ebnf-make-empty) lis)))
	     (ebnf-create-alternative (nreverse lis)))
	    )))))


(defun ebnf-split-header-suffix1 (ne ze)
  (cond
   ((eq (ebnf-node-kind ne) 'ebnf-generate-sequence)
    (and (eq (ebnf-node-kind ze) 'ebnf-generate-sequence)
	 (let ((nl (ebnf-node-list ne))
	       (zl (ebnf-node-list ze))
	       len z)
	   (and (>= (length zl) (length nl))
		(let ((ok t))
		  (setq len (- (length zl) (length nl))
			z   (nthcdr len zl))
		  (while (and ok z)
		    (setq ok (ebnf-node-equal (car z) (car nl))
			  z  (cdr z)
			  nl (cdr nl)))
		  ok)
		(if (zerop len)
		    t
		  (setcdr (nthcdr (1- len) zl) nil)
		  ze)))))
   ((eq (ebnf-node-kind ze) 'ebnf-generate-sequence)
    (let* ((zl  (ebnf-node-list ze))
	   (len (length zl)))
      (and (ebnf-node-equal ne (car (nthcdr (1- len) zl)))
	   (cond
	    ((= len 1)
	     t)
	    ((= len 2)
	     (car zl))
	    (t
	     (setcdr (nthcdr (- len 2) zl) nil)
	     ze)
	    ))))
   (t
    (ebnf-node-equal ne ze))
   ))


(defun ebnf-prefix-suffix (lis)
  (and lis (listp lis)
       (let* ((prefix (ebnf-split-prefix lis))
	      (suffix (ebnf-split-suffix (cdr prefix)))
	      (middle (cdr suffix)))
	 (setq prefix (car prefix)
	       suffix (car suffix))
	 (and (or prefix suffix)
	      (ebnf-make-sequence
	       (nconc prefix
		      (and middle
			   (list (or (ebnf-map-list-to-optional middle)
				     (ebnf-create-alternative middle))))
		      suffix))))))


(defun ebnf-split-prefix (lis)
  (let* ((len  (length lis))
	 (tail lis)
	 (head (if (eq (ebnf-node-kind (car lis)) 'ebnf-generate-sequence)
		   (ebnf-node-list (car lis))
		 (list (car lis))))
	 (ipre (1+ len)))
    ;; determine prefix length
    (while (and (> ipre 0) (setq tail (cdr tail)))
      (let ((cur head)
	    (this (if (eq (ebnf-node-kind (car tail)) 'ebnf-generate-sequence)
		      (ebnf-node-list (car tail))
		    (list (car tail))))
	    (i 0))
	(while (and cur this
		    (ebnf-node-equal (car cur) (car this)))
	  (setq cur  (cdr cur)
		this (cdr this)
		i    (1+ i)))
	(setq ipre (min ipre i))))
    (if (or (zerop ipre) (> ipre len))
	;; no prefix at all
	(cons nil lis)
      (let* ((tail   (nthcdr ipre head))
	     ;; get prefix
	     (prefix (progn
		       (and tail
			    (setcdr (nthcdr (1- ipre) head) nil))
		       head))
	     empty-p before)
	;; adjust first element
	(if (or (not (eq (ebnf-node-kind (car lis)) 'ebnf-generate-sequence))
		(null tail))
	    (setq lis     (cdr lis)
		  tail    lis
		  empty-p t)
	  (if (= (length tail) 1)
	      (setcar lis (car tail))
	    (ebnf-node-list (car lis) tail))
	  (setq tail (cdr lis)))
	;; eliminate prefix from lis based on ipre
	(while tail
	  (let ((elt (car tail))
		rest)
	    (if (and (eq (ebnf-node-kind elt) 'ebnf-generate-sequence)
		     (setq rest (nthcdr ipre (ebnf-node-list elt))))
		(progn
		  (if (= (length rest) 1)
		      (setcar tail (car rest))
		    (ebnf-node-list elt rest))
		  (setq before tail))
	      (setq empty-p t)
	      (if before
		  (setcdr before (cdr tail))
		(setq lis (cdr lis))))
	    (setq tail (cdr tail))))
	(cons prefix (ebnf-unique-list
		      (if empty-p
			  (nconc lis (list (ebnf-make-empty)))
			lis)))))))


(defun ebnf-split-suffix (lis)
  (let* ((len  (length lis))
	 (tail lis)
	 (head (nreverse
		(if (eq (ebnf-node-kind (car lis)) 'ebnf-generate-sequence)
		    (ebnf-node-list (car lis))
		  (list (car lis)))))
	 (isuf (1+ len)))
    ;; determine suffix length
    (while (and (> isuf 0) (setq tail (cdr tail)))
      (let* ((cur head)
	     (tlis (nreverse
		    (if (eq (ebnf-node-kind (car tail)) 'ebnf-generate-sequence)
			(ebnf-node-list (car tail))
		      (list (car tail)))))
	     (this tlis)
	     (i 0))
	(while (and cur this
		    (ebnf-node-equal (car cur) (car this)))
	  (setq cur  (cdr cur)
		this (cdr this)
		i    (1+ i)))
	(nreverse tlis)
	(setq isuf (min isuf i))))
    (setq head (nreverse head))
    (if (or (zerop isuf) (> isuf len))
	;; no suffix at all
	(cons nil lis)
      (let* ((n      (- (length head) isuf))
	     ;; get suffix
	     (suffix (nthcdr n head))
	     (tail   (and (> n 0)
			  (progn
			    (setcdr (nthcdr (1- n) head) nil)
			    head)))
	     before empty-p)
	;; adjust first element
	(if (or (not (eq (ebnf-node-kind (car lis)) 'ebnf-generate-sequence))
		(null tail))
	    (setq lis     (cdr lis)
		  tail    lis
		  empty-p t)
	  (if (= (length tail) 1)
	      (setcar lis (car tail))
	    (ebnf-node-list (car lis) tail))
	  (setq tail (cdr lis)))
	;; eliminate suffix from lis based on isuf
	(while tail
	  (let ((elt (car tail))
		rest)
	    (if (and (eq (ebnf-node-kind elt) 'ebnf-generate-sequence)
		     (setq rest (ebnf-node-list elt)
			   n    (- (length rest) isuf))
		     (> n 0))
		(progn
		  (if (= n 1)
		      (setcar tail (car rest))
		    (setcdr (nthcdr (1- n) rest) nil)
		    (ebnf-node-list elt rest))
		  (setq before tail))
	      (setq empty-p t)
	      (if before
		  (setcdr before (cdr tail))
		(setq lis (cdr lis))))
	    (setq tail (cdr tail))))
	(cons suffix (ebnf-unique-list
		      (if empty-p
			  (nconc lis (list (ebnf-make-empty)))
			lis)))))))


(defun ebnf-unique-list (nlist)
  (let ((current nlist)
	before)
    (while current
      (let ((tail (cdr current))
	    (head (car current))
	    remove-p)
	(while tail
	  (if (not (ebnf-node-equal head (car tail)))
	      (setq tail (cdr tail))
	    (setq remove-p t
		  tail     nil)
	    (if before
		(setcdr before (cdr current))
	      (setq nlist (cdr nlist)))))
	(or remove-p
	    (setq before current))
	(setq current (cdr current))))
    nlist))


(defun ebnf-node-equal (A B)
  (let ((kindA (ebnf-node-kind A))
	(kindB (ebnf-node-kind B)))
    (and (eq kindA kindB)
	 (cond
	  ;; empty
	  ((eq kindA 'ebnf-generate-empty)
	   t)
	  ;; non-terminal, terminal, special
	  ((memq kindA '(ebnf-generate-non-terminal
			 ebnf-generate-terminal
			 ebnf-generate-special))
	   (string= (ebnf-node-name A) (ebnf-node-name B)))
	  ;; alternative, sequence
	  ((memq kindA '(ebnf-generate-alternative ; any order
			 ebnf-generate-sequence)) ; order is important
	   (let ((listA (ebnf-node-list A))
		 (listB (ebnf-node-list B)))
	     (and (= (length listA) (length listB))
		  (let ((ok t))
		    (while (and ok listA)
		      (setq ok    (ebnf-node-equal (car listA) (car listB))
			    listA (cdr listA)
			    listB (cdr listB)))
		    ok))))
	  ;; production
	  ((eq kindA 'ebnf-generate-production)
	   (and (string= (ebnf-node-name A) (ebnf-node-name B))
		(ebnf-node-equal (ebnf-node-production A)
				 (ebnf-node-production B))))
	  ;; otherwise
	  (t
	   nil)
	  ))))


(defun ebnf-create-alternative (alt)
  (if (> (length alt) 1)
      (ebnf-make-alternative alt)
    (car alt)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'ebnf-otz)


;;; ebnf-otz.el ends here
