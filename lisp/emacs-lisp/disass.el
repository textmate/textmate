;;; disass.el --- disassembler for compiled Emacs Lisp code

;; Copyright (C) 1986, 1991, 2002-2012 Free Software Foundation, Inc.

;; Author: Doug Cutting <doug@csli.stanford.edu>
;;	Jamie Zawinski <jwz@lucid.com>
;; Maintainer: FSF
;; Keywords: internal

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

;; The single entry point, `disassemble', disassembles a code object generated
;; by the Emacs Lisp byte-compiler.  This doesn't invert the compilation
;; operation, not by a long shot, but it's useful for debugging.

;;
;; Original version by Doug Cutting (doug@csli.stanford.edu)
;; Substantially modified by Jamie Zawinski <jwz@lucid.com> for
;; the new lapcode-based byte compiler.

;;; Code:

;;; The variable byte-code-vector is defined by the new bytecomp.el.
;;; The function byte-decompile-lapcode is defined in byte-opt.el.
;;; Since we don't use byte-decompile-lapcode, let's try not loading byte-opt.
(require 'byte-compile "bytecomp")

(defvar disassemble-column-1-indent 8 "*")
(defvar disassemble-column-2-indent 10 "*")

(defvar disassemble-recursive-indent 3 "*")

;;;###autoload
(defun disassemble (object &optional buffer indent interactive-p)
  "Print disassembled code for OBJECT in (optional) BUFFER.
OBJECT can be a symbol defined as a function, or a function itself
\(a lambda expression or a compiled-function object).
If OBJECT is not already compiled, we compile it, but do not
redefine OBJECT if it is a symbol."
  (interactive (list (intern (completing-read "Disassemble function: "
					      obarray 'fboundp t))
		     nil 0 t))
  (if (and (consp object) (not (eq (car object) 'lambda)))
      (setq object (list 'lambda () object)))
  (or indent (setq indent 0))		;Default indent to zero
  (save-excursion
    (if (or interactive-p (null buffer))
	(with-output-to-temp-buffer "*Disassemble*"
	  (set-buffer "*Disassemble*")
	  (disassemble-internal object indent (not interactive-p)))
      (set-buffer buffer)
      (disassemble-internal object indent nil)))
  nil)


(defun disassemble-internal (obj indent interactive-p)
  (let ((macro 'nil)
	(name 'nil)
	(doc 'nil)
	args)
    (while (symbolp obj)
      (setq name obj
	    obj (symbol-function obj)))
    (if (subrp obj)
	(error "Can't disassemble #<subr %s>" name))
    (when (and (listp obj) (eq (car obj) 'autoload))
      (load (nth 1 obj))
      (setq obj (symbol-function name)))
    (if (eq (car-safe obj) 'macro)	;handle macros
	(setq macro t
	      obj (cdr obj)))
    (when (and (listp obj) (eq (car obj) 'closure))
      (error "Don't know how to compile an interpreted closure"))
    (if (and (listp obj) (eq (car obj) 'byte-code))
	(setq obj (list 'lambda nil obj)))
    (if (and (listp obj) (not (eq (car obj) 'lambda)))
	(error "not a function"))
    (if (consp obj)
	(if (assq 'byte-code obj)
	    nil
	  (if interactive-p (message (if name
					 "Compiling %s's definition..."
				       "Compiling definition...")
				     name))
	  (setq obj (byte-compile obj))
	  (if interactive-p (message "Done compiling.  Disassembling..."))))
    (cond ((consp obj)
	   (setq obj (cdr obj))		;throw lambda away
	   (setq args (car obj))	;save arg list
	   (setq obj (cdr obj)))
	  ((byte-code-function-p obj)
	   (setq args (aref obj 0)))
          (t (error "Compilation failed")))
    (if (zerop indent) ; not a nested function
	(progn
	  (indent-to indent)
	  (insert (format "byte code%s%s%s:\n"
			  (if (or macro name) " for" "")
			  (if macro " macro" "")
			  (if name (format " %s" name) "")))))
    (let ((doc (if (consp obj)
		   (and (stringp (car obj)) (car obj))
		 ;; Use documentation to get lazy-loaded doc string
		 (documentation obj t))))
      (if (and doc (stringp doc))
	  (progn (and (consp obj) (setq obj (cdr obj)))
		 (indent-to indent)
		 (princ "  doc:  " (current-buffer))
		 (if (string-match "\n" doc)
		     (setq doc (concat (substring doc 0 (match-beginning 0))
				       " ...")))
		 (insert doc "\n"))))
    (indent-to indent)
    (insert "  args: ")
    (prin1 args (current-buffer))
    (insert "\n")
    (let ((interactive (cond ((consp obj)
			      (assq 'interactive obj))
			     ((> (length obj) 5)
			      (list 'interactive (aref obj 5))))))
      (if interactive
	  (progn
	    (setq interactive (nth 1 interactive))
	    (if (eq (car-safe (car-safe obj)) 'interactive)
		(setq obj (cdr obj)))
	    (indent-to indent)
	    (insert " interactive: ")
	    (if (eq (car-safe interactive) 'byte-code)
		(progn
		  (insert "\n")
		  (disassemble-1 interactive
				 (+ indent disassemble-recursive-indent)))
	      (let ((print-escape-newlines t))
		(prin1 interactive (current-buffer))))
	    (insert "\n"))))
    (cond ((and (consp obj) (assq 'byte-code obj))
	   (disassemble-1 (assq 'byte-code obj) indent))
	  ((byte-code-function-p obj)
	   (disassemble-1 obj indent))
	  (t
	   (insert "Uncompiled body:  ")
	   (let ((print-escape-newlines t))
	     (prin1 (if (cdr obj) (cons 'progn obj) (car obj))
		    (current-buffer))))))
  (if interactive-p
      (message "")))


(defun disassemble-1 (obj indent)
  "Prints the byte-code call OBJ in the current buffer.
OBJ should be a call to BYTE-CODE generated by the byte compiler."
  (let (bytes constvec)
    (if (consp obj)
	(setq bytes (car (cdr obj))		;the byte code
	      constvec (car (cdr (cdr obj))))	;constant vector
      ;; If it is lazy-loaded, load it now
      (fetch-bytecode obj)
      (setq bytes (aref obj 1)
	    constvec (aref obj 2)))
    (let ((lap (byte-decompile-bytecode (string-as-unibyte bytes) constvec))
	  op arg opname pc-value)
      (let ((tagno 0)
	    tmp
	    (lap lap))
	(while (setq tmp (assq 'TAG lap))
	  (setcar (cdr tmp) (setq tagno (1+ tagno)))
	  (setq lap (cdr (memq tmp lap)))))
      (while lap
	;; Take off the pc value of the next thing
	;; and put it in pc-value.
	(setq pc-value nil)
	(if (numberp (car lap))
	    (setq pc-value (car lap)
		  lap (cdr lap)))
	;; Fetch the next op and its arg.
	(setq op (car (car lap))
	      arg (cdr (car lap)))
	(setq lap (cdr lap))
	(indent-to indent)
	(if (eq 'TAG op)
	    (progn
	      ;; We have a label.  Display it, but first its pc value.
	      (if pc-value
		  (insert (format "%d:" pc-value)))
	      (insert (int-to-string (car arg))))
	  ;; We have an instruction.  Display its pc value first.
	  (if pc-value
	      (insert (format "%d" pc-value)))
	  (indent-to (+ indent disassemble-column-1-indent))
	  (if (and op
		   (string-match "^byte-" (setq opname (symbol-name op))))
	      (setq opname (substring opname 5))
	    (setq opname "<not-an-opcode>"))
	  (if (eq op 'byte-constant2)
	      (insert " #### shouldn't have seen constant2 here!\n  "))
	  (insert opname)
	  (indent-to (+ indent disassemble-column-1-indent
			disassemble-column-2-indent
			-1))
	  (insert " ")
	  (cond ((memq op byte-goto-ops)
		 (insert (int-to-string (nth 1 arg))))
		((memq op '(byte-call byte-unbind
			    byte-listN byte-concatN byte-insertN
			    byte-stack-ref byte-stack-set byte-stack-set2
			    byte-discardN byte-discardN-preserve-tos))
		 (insert (int-to-string arg)))
		((memq op '(byte-varref byte-varset byte-varbind))
		 (prin1 (car arg) (current-buffer)))
		((memq op '(byte-constant byte-constant2))
		 ;; it's a constant
		 (setq arg (car arg))
		 ;; but if the value of the constant is compiled code, then
		 ;; recursively disassemble it.
		 (cond ((or (byte-code-function-p arg)
			    (and (eq (car-safe arg) 'lambda)
				 (assq 'byte-code arg))
			    (and (eq (car-safe arg) 'macro)
				 (or (byte-code-function-p (cdr arg))
				     (and (eq (car-safe (cdr arg)) 'lambda)
					  (assq 'byte-code (cdr arg))))))
			(cond ((byte-code-function-p arg)
			       (insert "<compiled-function>\n"))
			      ((eq (car-safe arg) 'lambda)
			       (insert "<compiled lambda>"))
			      (t (insert "<compiled macro>\n")))
			(disassemble-internal
			 arg
			 (+ indent disassemble-recursive-indent 1)
			 nil))
		       ((eq (car-safe arg) 'byte-code)
			(insert "<byte code>\n")
			(disassemble-1	;recurse on byte-code object
			 arg
			 (+ indent disassemble-recursive-indent)))
		       ((eq (car-safe (car-safe arg)) 'byte-code)
			(insert "(<byte code>...)\n")
			(mapc ;recurse on list of byte-code objects
			 (lambda (obj)
                           (disassemble-1
                            obj
                            (+ indent disassemble-recursive-indent)))
			 arg))
		       (t
			;; really just a constant
			(let ((print-escape-newlines t))
			  (prin1 arg (current-buffer))))))
		)
	  (insert "\n")))))
  nil)

(provide 'disass)

;;; disass.el ends here
