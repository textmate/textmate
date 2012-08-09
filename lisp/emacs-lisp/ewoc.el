;;; ewoc.el --- utility to maintain a view of a list of objects in a buffer

;; Copyright (C) 1991-2012 Free Software Foundation, Inc.

;; Author: Per Cederqvist <ceder@lysator.liu.se>
;;	Inge Wallin <inge@lysator.liu.se>
;; Maintainer: monnier@gnu.org
;; Created: 3 Aug 1992
;; Keywords: extensions, lisp

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

;; Ewoc Was Once Cookie
;; But now it's Emacs's Widget for Object Collections

;; As the name implies this derives from the `cookie' package (part
;; of Elib).  The changes are pervasive though mostly superficial:

;; - uses CL (and its `defstruct')
;; - separate from Elib.
;; - uses its own version of a doubly-linked list which allows us
;;   to merge the elib-wrapper and the elib-node structures into ewoc-node
;; - dropping functions not used by PCL-CVS (the only client of ewoc at the
;;   time of writing)
;; - removing unused arguments
;; - renaming:
;;   elib-node	==>  ewoc--node
;;   collection ==>  ewoc
;;   tin 	==>  ewoc--node
;;   cookie 	==>  data or element or elem

;;     Introduction
;;     ============
;;
;; Ewoc is a package that implements a connection between an
;; dll (a doubly linked list) and the contents of a buffer.
;; Possible uses are dired (have all files in a list, and show them),
;; buffer-list, kom-prioritize (in the LysKOM elisp client) and
;; others.  pcl-cvs.el and vc.el use ewoc.el.
;;
;; Ewoc can be considered as the `view' part of a model-view-controller.
;;
;; A `element' can be any lisp object.  When you use the ewoc
;; package you specify a pretty-printer, a function that inserts
;; a printable representation of the element in the buffer.  (The
;; pretty-printer should use "insert" and not
;; "insert-before-markers").
;;
;; A `ewoc' consists of a doubly linked list of elements, a
;; header, a footer and a pretty-printer.  It is displayed at a
;; certain point in a certain buffer.  (The buffer and point are
;; fixed when the ewoc is created).  The header and the footer
;; are constant strings.  They appear before and after the elements.
;;
;; Ewoc does not affect the mode of the buffer in any way. It
;; merely makes it easy to connect an underlying data representation
;; to the buffer contents.
;;
;; A `ewoc--node' is an object that contains one element.  There are
;; functions in this package that given an ewoc--node extract the data, or
;; give the next or previous ewoc--node.  (All ewoc--nodes are linked together
;; in a doubly linked list.  The `previous' ewoc--node is the one that appears
;; before the other in the buffer.)  You should not do anything with
;; an ewoc--node except pass it to the functions in this package.
;;
;; An ewoc is a very dynamic thing.  You can easily add or delete elements.
;; You can apply a function to all elements in an ewoc, etc, etc.
;;
;; Remember that an element can be anything.  Your imagination is the
;; limit!  It is even possible to have another ewoc as an
;; element.  In that way some kind of tree hierarchy can be created.
;;
;; The Emacs Lisp Reference Manual documents ewoc.el's "public interface".

;;     Coding conventions
;;     ==================
;;
;; All functions of course start with `ewoc'.  Functions and macros
;; starting with the prefix `ewoc--' are meant for internal use,
;; while those starting with `ewoc-' are exported for public use.

;;; Code:

(eval-when-compile (require 'cl))

;; The doubly linked list is implemented as a circular list with a dummy
;; node first and last. The dummy node is used as "the dll".
(defstruct (ewoc--node
	    (:type vector)		;ewoc--node-nth needs this
            (:constructor nil)
	    (:constructor ewoc--node-create (start-marker data)))
  left right data start-marker)

(defun ewoc--node-next (dll node)
  "Return the node after NODE, or nil if NODE is the last node."
  (let ((R (ewoc--node-right node)))
    (unless (eq dll R) R)))

(defun ewoc--node-prev (dll node)
  "Return the node before NODE, or nil if NODE is the first node."
  (let ((L (ewoc--node-left node)))
    (unless (eq dll L) L)))

(defun ewoc--node-nth (dll n)
  "Return the Nth node from the doubly linked list `dll'.
N counts from zero.  If N is negative, return the -(N+1)th last element.
If N is out of range, return nil.
Thus, (ewoc--node-nth dll 0) returns the first node,
and (ewoc--node-nth dll -1) returns the last node."
  ;; Presuming a node is ":type vector", starting with `left' and `right':
  ;; Branch 0 ("follow left pointer") is used when n is negative.
  ;; Branch 1 ("follow right pointer") is used otherwise.
  (let* ((branch (if (< n 0) 0 1))
	 (node   (aref dll branch)))
    (if (< n 0) (setq n (- -1 n)))
    (while (and (not (eq dll node)) (> n 0))
      (setq node (aref node branch))
      (setq n (1- n)))
    (unless (eq dll node) node)))

(defun ewoc-location (node)
  "Return the start location of NODE."
  (ewoc--node-start-marker node))


;;; The ewoc data type

(defstruct (ewoc
	    (:constructor nil)
	    (:constructor ewoc--create (buffer pretty-printer dll))
	    (:conc-name ewoc--))
  buffer pretty-printer header footer dll last-node hf-pp)

(defmacro ewoc--set-buffer-bind-dll-let* (ewoc varlist &rest forms)
  "Execute FORMS with ewoc--buffer selected as current buffer,
`dll' bound to the dll, and VARLIST bound as in a let*.
`dll' will be bound when VARLIST is initialized, but
the current buffer will *not* have been changed.
Return value of last form in FORMS."
  (let ((hnd (make-symbol "ewoc")))
    `(let* ((,hnd ,ewoc)
            (dll (ewoc--dll ,hnd))
            ,@varlist)
       (with-current-buffer (ewoc--buffer ,hnd)
         ,@forms))))

(defmacro ewoc--set-buffer-bind-dll (ewoc &rest forms)
  `(ewoc--set-buffer-bind-dll-let* ,ewoc nil ,@forms))

(defsubst ewoc--filter-hf-nodes (ewoc node)
  "Evaluate NODE once and return it.
BUT if it is the header or the footer in EWOC return nil instead."
  (unless (or (eq node (ewoc--header ewoc))
	      (eq node (ewoc--footer ewoc)))
    node))

(defun ewoc--adjust (beg end node dll)
  ;; "Manually reseat" markers for NODE and its successors (including footer
  ;; and dll), in the case where they originally shared start position with
  ;; BEG, to END.  BEG and END are buffer positions describing NODE's left
  ;; neighbor.  This operation is functionally equivalent to temporarily
  ;; setting these nodes' markers' insertion type to t around the pretty-print
  ;; call that precedes the call to `ewoc--adjust', and then changing them back
  ;; to nil.
  (when (< beg end)
    (let (m)
      (while (and (= beg (setq m (ewoc--node-start-marker node)))
                  ;; The "dummy" node `dll' actually holds the marker that
                  ;; points to the end of the footer, so we check `dll'
                  ;; *after* reseating the marker.
                  (progn
                    (set-marker m end)
                    (not (eq dll node))))
        (setq node (ewoc--node-right node))))))

(defun ewoc--insert-new-node (node data pretty-printer dll)
  "Insert before NODE a new node for DATA, displayed by PRETTY-PRINTER.
Fourth arg DLL -- from `(ewoc--dll EWOC)' -- is for internal purposes.
Call PRETTY-PRINTER with point at NODE's start, thus pushing back
NODE and leaving the new node's start there.  Return the new node."
  (save-excursion
    (let ((elemnode (ewoc--node-create
                     (copy-marker (ewoc--node-start-marker node)) data)))
      (setf (ewoc--node-left  elemnode) (ewoc--node-left node)
            (ewoc--node-right elemnode)                  node
            (ewoc--node-right (ewoc--node-left node)) elemnode
            (ewoc--node-left                   node)  elemnode)
      (ewoc--refresh-node pretty-printer elemnode dll)
      elemnode)))

(defun ewoc--refresh-node (pp node dll)
  "Redisplay the element represented by NODE using the pretty-printer PP."
  (let ((inhibit-read-only t)
        (m (ewoc--node-start-marker node))
        (R (ewoc--node-right node)))
    ;; First, remove the string from the buffer:
    (delete-region m (ewoc--node-start-marker R))
    ;; Calculate and insert the string.
    (goto-char m)
    (funcall pp (ewoc--node-data node))
    (ewoc--adjust m (point) R dll)))

(defun ewoc--wrap (func)
  (lexical-let ((ewoc--user-pp func))
    (lambda (data)
      (funcall ewoc--user-pp data)
      (insert "\n"))))


;;; ===========================================================================
;;;                  Public members of the Ewoc package

;;;###autoload
(defun ewoc-create (pretty-printer &optional header footer nosep)
  "Create an empty ewoc.

The ewoc will be inserted in the current buffer at the current position.

PRETTY-PRINTER should be a function that takes one argument, an
element, and inserts a string representing it in the buffer (at
point).  The string PRETTY-PRINTER inserts may be empty or span
several lines.  The PRETTY-PRINTER should use `insert', and not
`insert-before-markers'.

Optional second and third arguments HEADER and FOOTER are strings,
possibly empty, that will always be present at the top and bottom,
respectively, of the ewoc.

Normally, a newline is automatically inserted after the header,
the footer and every node's printed representation.  Optional
fourth arg NOSEP non-nil inhibits this."
  (let* ((dummy-node (ewoc--node-create 'DL-LIST 'DL-LIST))
         (dll (progn (setf (ewoc--node-right dummy-node) dummy-node)
                     (setf (ewoc--node-left dummy-node) dummy-node)
                     dummy-node))
         (wrap (if nosep 'identity 'ewoc--wrap))
         (new-ewoc (ewoc--create (current-buffer)
                                 (funcall wrap pretty-printer)
                                 dll))
         (hf-pp (funcall wrap 'insert))
         (pos (point))
         head foot)
    (ewoc--set-buffer-bind-dll new-ewoc
      ;; Set default values
      (unless header (setq header ""))
      (unless footer (setq footer ""))
      (setf (ewoc--node-start-marker dll) (copy-marker pos)
            foot (ewoc--insert-new-node  dll footer hf-pp dll)
            head (ewoc--insert-new-node foot header hf-pp dll)
            (ewoc--hf-pp new-ewoc) hf-pp
            (ewoc--footer new-ewoc) foot
            (ewoc--header new-ewoc) head))
    ;; Return the ewoc
    new-ewoc))

(defalias 'ewoc-data 'ewoc--node-data
  "Extract the data encapsulated by NODE and return it.

\(fn NODE)")

(defun ewoc-set-data (node data)
  "Set NODE to encapsulate DATA."
  (setf (ewoc--node-data node) data))

(defun ewoc-enter-first (ewoc data)
  "Enter DATA first in EWOC.
Return the new node."
  (ewoc--set-buffer-bind-dll ewoc
    (ewoc-enter-after ewoc (ewoc--node-nth dll 0) data)))

(defun ewoc-enter-last (ewoc data)
  "Enter DATA last in EWOC.
Return the new node."
  (ewoc--set-buffer-bind-dll ewoc
    (ewoc-enter-before ewoc (ewoc--node-nth dll -1) data)))

(defun ewoc-enter-after (ewoc node data)
  "Enter a new element DATA after NODE in EWOC.
Return the new node."
  (ewoc--set-buffer-bind-dll ewoc
    (ewoc-enter-before ewoc (ewoc--node-next dll node) data)))

(defun ewoc-enter-before (ewoc node data)
  "Enter a new element DATA before NODE in EWOC.
Return the new node."
  (ewoc--set-buffer-bind-dll ewoc
    (ewoc--insert-new-node node data (ewoc--pretty-printer ewoc) dll)))

(defun ewoc-next (ewoc node)
  "Return the node in EWOC that follows NODE.
Return nil if NODE is nil or the last element."
  (when node
    (ewoc--filter-hf-nodes
     ewoc (ewoc--node-next (ewoc--dll ewoc) node))))

(defun ewoc-prev (ewoc node)
  "Return the node in EWOC that precedes NODE.
Return nil if NODE is nil or the first element."
  (when node
    (ewoc--filter-hf-nodes
     ewoc (ewoc--node-prev (ewoc--dll ewoc) node))))

(defun ewoc-nth (ewoc n)
  "Return the Nth node.
N counts from zero.  Return nil if there is less than N elements.
If N is negative, return the -(N+1)th last element.
Thus, (ewoc-nth ewoc 0) returns the first node,
and (ewoc-nth ewoc -1) returns the last node.
Use `ewoc-data' to extract the data from the node."
  ;; Skip the header (or footer, if n is negative).
  (setq n (if (< n 0) (1- n) (1+ n)))
  (ewoc--filter-hf-nodes ewoc
                         (ewoc--node-nth (ewoc--dll ewoc) n)))

(defun ewoc-map (map-function ewoc &rest args)
  "Apply MAP-FUNCTION to all elements in EWOC.
MAP-FUNCTION is applied to the first element first.
If MAP-FUNCTION returns non-nil the element will be refreshed (its
pretty-printer will be called once again).

Note that the buffer for EWOC will be the current buffer when
MAP-FUNCTION is called.  MAP-FUNCTION must restore the current
buffer before it returns, if it changes it.

If more than two arguments are given, the remaining
arguments will be passed to MAP-FUNCTION."
  (ewoc--set-buffer-bind-dll-let* ewoc
      ((footer (ewoc--footer ewoc))
       (pp (ewoc--pretty-printer ewoc))
       (node (ewoc--node-nth dll 1)))
    (save-excursion
      (while (not (eq node footer))
        (if (apply map-function (ewoc--node-data node) args)
            (ewoc--refresh-node pp node dll))
        (setq node (ewoc--node-next dll node))))))

(defun ewoc-delete (ewoc &rest nodes)
  "Delete NODES from EWOC."
  (ewoc--set-buffer-bind-dll-let* ewoc
      ((L nil) (R nil) (last (ewoc--last-node ewoc)))
    (dolist (node nodes)
      ;; If we are about to delete the node pointed at by last-node,
      ;; set last-node to nil.
      (when (eq last node)
        (setf last nil (ewoc--last-node ewoc) nil))
      (delete-region (ewoc--node-start-marker node)
                     (ewoc--node-start-marker (ewoc--node-next dll node)))
      (set-marker (ewoc--node-start-marker node) nil)
      (setf L (ewoc--node-left  node)
            R (ewoc--node-right node)
            ;; Link neighbors to each other.
            (ewoc--node-right L) R
            (ewoc--node-left  R) L
            ;; Forget neighbors.
            (ewoc--node-left  node) nil
            (ewoc--node-right node) nil))))

(defun ewoc-filter (ewoc predicate &rest args)
  "Remove all elements in EWOC for which PREDICATE returns nil.
Note that the buffer for EWOC will be current-buffer when PREDICATE
is called.  PREDICATE must restore the current buffer before it returns
if it changes it.
The PREDICATE is called with the element as its first argument.  If any
ARGS are given they will be passed to the PREDICATE."
  (ewoc--set-buffer-bind-dll-let* ewoc
      ((node (ewoc--node-nth dll 1))
       (footer (ewoc--footer ewoc))
       (goodbye nil)
       (inhibit-read-only t))
    (while (not (eq node footer))
      (unless (apply predicate (ewoc--node-data node) args)
        (push node goodbye))
      (setq node (ewoc--node-next dll node)))
    (apply 'ewoc-delete ewoc goodbye)))

(defun ewoc-locate (ewoc &optional pos guess)
  "Return the node that POS (a buffer position) is within.
POS may be a marker or an integer.  It defaults to point.
GUESS should be a node that it is likely to be near POS.

If POS points before the first element, the first node is returned.
If POS points after the last element, the last node is returned.
If the EWOC is empty, nil is returned."
  (unless pos (setq pos (point)))
  (ewoc--set-buffer-bind-dll ewoc

    (cond
     ;; Nothing present?
     ((eq (ewoc--node-nth dll 1) (ewoc--node-nth dll -1))
      nil)

     ;; Before second elem?
     ((< pos (ewoc--node-start-marker (ewoc--node-nth dll 2)))
      (ewoc--node-nth dll 1))

     ;; After one-before-last elem?
     ((>= pos (ewoc--node-start-marker (ewoc--node-nth dll -2)))
      (ewoc--node-nth dll -2))

     ;; We now know that pos is within a elem.
     (t
      ;; Make an educated guess about which of the three known
      ;; node'es (the first, the last, or GUESS) is nearest.
      (let* ((best-guess (ewoc--node-nth dll 1))
	     (distance (abs (- pos (ewoc--node-start-marker best-guess)))))
	(when guess
	  (let ((d (abs (- pos (ewoc--node-start-marker guess)))))
	    (when (< d distance)
	      (setq distance d)
	      (setq best-guess guess))))

	(let* ((g (ewoc--node-nth dll -1))	;Check the last elem
	       (d (abs (- pos (ewoc--node-start-marker g)))))
	  (when (< d distance)
	    (setq distance d)
	    (setq best-guess g)))

	(when (ewoc--last-node ewoc)    ;Check "previous".
	  (let* ((g (ewoc--last-node ewoc))
		 (d (abs (- pos (ewoc--node-start-marker g)))))
	    (when (< d distance)
	      (setq distance d)
	      (setq best-guess g))))

	;; best-guess is now a "best guess".
	;; Find the correct node. First determine in which direction
	;; it lies, and then move in that direction until it is found.

	(cond
	 ;; Is pos after the guess?
	 ((>= pos
	      (ewoc--node-start-marker best-guess))
	  ;; Loop until we are exactly one node too far down...
	  (while (>= pos (ewoc--node-start-marker best-guess))
	    (setq best-guess (ewoc--node-next dll best-guess)))
	  ;; ...and return the previous node.
	  (ewoc--node-prev dll best-guess))

	 ;; Pos is before best-guess
	 (t
	  (while (< pos (ewoc--node-start-marker best-guess))
	    (setq best-guess (ewoc--node-prev dll best-guess)))
	  best-guess)))))))

(defun ewoc-invalidate (ewoc &rest nodes)
  "Call EWOC's pretty-printer for each element in NODES.
Delete current text first, thus effecting a \"refresh\"."
  (ewoc--set-buffer-bind-dll-let* ewoc
      ((pp (ewoc--pretty-printer ewoc)))
    (save-excursion
      (dolist (node nodes)
        (ewoc--refresh-node pp node dll)))))

(defun ewoc-goto-prev (ewoc arg)
  "Move point to the ARGth previous element in EWOC.
Don't move if we are at the first element, or if EWOC is empty.
Return the node we moved to."
  (ewoc--set-buffer-bind-dll-let* ewoc
      ((node (ewoc-locate ewoc (point))))
    (when node
      ;; If we were past the last element, first jump to it.
      (when (>= (point) (ewoc--node-start-marker (ewoc--node-right node)))
	(setq arg (1- arg)))
      (while (and node (> arg 0))
	(setq arg (1- arg))
	(setq node (ewoc--node-prev dll node)))
      ;; Never step above the first element.
      (unless (ewoc--filter-hf-nodes ewoc node)
	(setq node (ewoc--node-nth dll 1)))
      (ewoc-goto-node ewoc node))))

(defun ewoc-goto-next (ewoc arg)
  "Move point to the ARGth next element in EWOC.
Return the node (or nil if we just passed the last node)."
  (ewoc--set-buffer-bind-dll-let* ewoc
      ((node (ewoc-locate ewoc (point))))
    (while (and node (> arg 0))
      (setq arg (1- arg))
      (setq node (ewoc--node-next dll node)))
    ;; Never step below the first element.
    ;; (unless (ewoc--filter-hf-nodes ewoc node)
    ;;   (setq node (ewoc--node-nth dll -2)))
    (unless node
      (error "No next"))
    (ewoc-goto-node ewoc node)))

(defun ewoc-goto-node (ewoc node)
  "Move point to NODE in EWOC."
  (ewoc--set-buffer-bind-dll ewoc
    (goto-char (ewoc--node-start-marker node))
    (if goal-column (move-to-column goal-column))
    (setf (ewoc--last-node ewoc) node)))

(defun ewoc-refresh (ewoc)
  "Refresh all data in EWOC.
The pretty-printer that was specified when the EWOC was created
will be called for all elements in EWOC.
Note that `ewoc-invalidate' is more efficient if only a small
number of elements needs to be refreshed."
  (ewoc--set-buffer-bind-dll-let* ewoc
      ((footer (ewoc--footer ewoc)))
    (let ((inhibit-read-only t))
      (delete-region (ewoc--node-start-marker (ewoc--node-nth dll 1))
		     (ewoc--node-start-marker footer))
      (goto-char (ewoc--node-start-marker footer))
      (let ((pp (ewoc--pretty-printer ewoc))
            (node (ewoc--node-nth dll 1)))
	(while (not (eq node footer))
	  (set-marker (ewoc--node-start-marker node) (point))
	  (funcall pp (ewoc--node-data node))
	  (setq node (ewoc--node-next dll node)))))
    (set-marker (ewoc--node-start-marker footer) (point))))

(defun ewoc-collect (ewoc predicate &rest args)
  "Select elements from EWOC using PREDICATE.
Return a list of all selected data elements.
PREDICATE is a function that takes a data element as its first
argument.  The elements on the returned list will appear in the
same order as in the buffer.  You should not rely on the order of
calls to PREDICATE.
Note that the buffer the EWOC is displayed in is the current
buffer when PREDICATE is called.  PREDICATE must restore it if it
changes it.
If more than two arguments are given the
remaining arguments will be passed to PREDICATE."
  (ewoc--set-buffer-bind-dll-let* ewoc
      ((header (ewoc--header ewoc))
       (node (ewoc--node-nth dll -2))
       result)
    (while (not (eq node header))
      (if (apply predicate (ewoc--node-data node) args)
	  (push (ewoc--node-data node) result))
      (setq node (ewoc--node-prev dll node)))
    result))

(defun ewoc-buffer (ewoc)
  "Return the buffer that is associated with EWOC.
Return nil if the buffer has been deleted."
  (let ((buf (ewoc--buffer ewoc)))
    (when (buffer-name buf) buf)))

(defun ewoc-get-hf (ewoc)
  "Return a cons cell containing the (HEADER . FOOTER) of EWOC."
  (cons (ewoc--node-data (ewoc--header ewoc))
	(ewoc--node-data (ewoc--footer ewoc))))

(defun ewoc-set-hf (ewoc header footer)
  "Set the HEADER and FOOTER of EWOC."
  (ewoc--set-buffer-bind-dll-let* ewoc
      ((head (ewoc--header ewoc))
       (foot (ewoc--footer ewoc))
       (hf-pp (ewoc--hf-pp ewoc)))
    (setf (ewoc--node-data head) header
          (ewoc--node-data foot) footer)
    (save-excursion
      (ewoc--refresh-node hf-pp head dll)
      (ewoc--refresh-node hf-pp foot dll))))


(provide 'ewoc)

;; Local Variables:
;; eval: (put 'ewoc--set-buffer-bind-dll 'lisp-indent-hook 1)
;; eval: (put 'ewoc--set-buffer-bind-dll-let* 'lisp-indent-hook 2)
;; End:

;;; ewoc.el ends here
