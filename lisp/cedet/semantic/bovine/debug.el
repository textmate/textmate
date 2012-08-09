;;; semantic/bovine/debug.el --- Debugger support for bovinator

;; Copyright (C) 2003, 2009-2012  Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>

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
;; Implementation of the semantic debug support framework for the
;; bovine parser.
;;

(require 'semantic/debug)
(require 'semantic/find)

;;; Code:

;;; Support a frame for the Bovinator
;;
(defclass semantic-bovine-debug-frame (semantic-debug-frame)
  ((nonterm :initarg :nonterm
	    :type symbol
	    :documentation
	    "The name of the semantic nonterminal for this frame.")
   (rule :initarg :rule
	 :type number
	 :documentation
	 "The index into NONTERM's rule list.  0 based.")
   (match :initarg :match
	  :type number
	  :documentation
	  "The index into NONTERM's RULE's match.  0 based..")
   (collection :initarg :collection
	       :type list
	       :documentation
	       "List of things matched so far.")
   (lextoken :initarg :lextoken
	     :type list
	     :documentation
	     "A Token created by `semantic-lex-token'.
This is the lexical token being matched by the parser.")
   )
  "Debugger frame representation for the bovinator.")

(defun semantic-bovine-debug-create-frame (nonterm rule match collection
						   lextoken)
  "Create one bovine frame.
NONTERM is the name of a rule we are currently parsing.
RULE is the index into the list of rules in NONTERM.
MATCH is the index into the list of matches in RULE.
For example:
  this: that
      | other thing
      | here
      ;
The NONTERM is THIS.
The RULE is for \"thing\" is 1.
The MATCH for \"thing\" is 1.
COLLECTION is a list of `things' that have been matched so far.
LEXTOKEN, is a token returned by the lexer which is being matched."
  (let ((frame (semantic-bovine-debug-frame "frame"
					    :nonterm nonterm
					    :rule rule
					    :match match
					    :collection collection
					    :lextoken lextoken)))
    (semantic-debug-set-frame semantic-debug-current-interface
			      frame)
    frame))

(defmethod semantic-debug-frame-highlight ((frame semantic-debug-frame))
  "Highlight one parser frame."
  (let* ((nonterm (oref frame nonterm))
	 (pb (oref semantic-debug-current-interface parser-buffer))
	 (start (semantic-brute-find-tag-by-class 'start pb))
	)
    ;; Make sure we get a good rule name, and that it is a string
    (if (and (eq nonterm 'bovine-toplevel) start)
	(setq nonterm (semantic-tag-name (car start)))
      (setq nonterm (symbol-name nonterm)))

    (semantic-debug-highlight-rule semantic-debug-current-interface
				   nonterm
				   (oref frame rule)
				   (oref frame match))
    (semantic-debug-highlight-lexical-token semantic-debug-current-interface
					    (oref frame lextoken))
    ))

(defmethod semantic-debug-frame-info ((frame semantic-debug-frame))
  "Display info about this one parser frame."
  (message "%S" (oref frame collection))
  )

;;; Lisp error thrown frame.
;;
(defclass semantic-bovine-debug-error-frame (semantic-debug-frame)
  ((condition :initarg :condition
	      :documentation
	      "An error condition caught in an action.")
   )
  "Debugger frame representation of a lisp error thrown during parsing.")

(defun semantic-create-bovine-debug-error-frame (condition)
  "Create an error frame for bovine debugger.
Argument CONDITION is the thrown error condition."
  (let ((frame (semantic-bovine-debug-error-frame "frame"
						  :condition condition)))
    (semantic-debug-set-frame semantic-debug-current-interface
			      frame)
    frame))

(defmethod semantic-debug-frame-highlight ((frame semantic-bovine-debug-error-frame))
  "Highlight a frame from an action."
  ;; How do I get the location of the action in the source buffer?
  )

(defmethod semantic-debug-frame-info ((frame semantic-bovine-debug-error-frame))
  "Display info about the error thrown."
  (message "Error: %S" (oref frame condition)))

;;; Parser support for the debugger
;;
(defclass semantic-bovine-debug-parser (semantic-debug-parser)
  (
   )
  "Represents a parser and its state.")


(provide 'semantic/bovine/debug)

;;; semantic/bovine/debug.el ends here
