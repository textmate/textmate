;;; semantic/bovine.el --- LL Parser/Analyzer core.

;; Copyright (C) 1999-2004, 2006-2007, 2009-2012
;;   Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <eric@siege-engine.com>

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
;; Semantic 1.x uses an LL parser named the "bovinator".  This parser
;; had several conveniences in it which made for parsing tags out of
;; languages with list characters easy.  This parser lives on as one
;; of many available parsers for semantic the tool.
;;
;; This parser should be used when the language is simple, such as
;; makefiles or other data-declarative languages.

;;; Code:
(require 'semantic)

(declare-function semantic-create-bovine-debug-error-frame
		  "semantic/bovine/debug")
(declare-function semantic-bovine-debug-create-frame
		  "semantic/bovine/debug")
(declare-function semantic-debug-break "semantic/debug")

;;; Variables
;;
(defvar semantic-bovinate-nonterminal-check-obarray nil
  "Obarray of streams already parsed for nonterminal symbols.
Use this to detect infinite recursion during a parse.")
(make-variable-buffer-local 'semantic-bovinate-nonterminal-check-obarray)



;; These are functions that can be called from within a bovine table.
;; Most of these have code auto-generated from other construct in the
;; bovine input grammar.
(defmacro semantic-lambda (&rest return-val)
  "Create a lambda expression to return a list including RETURN-VAL.
The return list is a lambda expression to be used in a bovine table."
  `(lambda (vals start end)
     (append ,@return-val (list start end))))

;;; Semantic Bovination
;;
;; Take a semantic token stream, and convert it using the bovinator.
;; The bovinator takes a state table, and converts the token stream
;; into a new semantic stream defined by the bovination table.
;;
(defsubst semantic-bovinate-symbol-nonterminal-p (sym table)
  "Return non-nil if SYM is in TABLE, indicating it is NONTERMINAL."
  ;; sym is always a sym, so assq should be ok.
  (if (assq sym table) t nil))

(defmacro semantic-bovinate-nonterminal-db-nt ()
  "Return the current nonterminal symbol.
Part of the grammar source debugger.  Depends on the existing
environment of `semantic-bovinate-stream'."
  `(if nt-stack
       (car (aref (car nt-stack) 2))
     nonterminal))

(defun semantic-bovinate-nonterminal-check (stream nonterminal)
  "Check if STREAM not already parsed for NONTERMINAL.
If so abort because an infinite recursive parse is suspected."
  (or (vectorp semantic-bovinate-nonterminal-check-obarray)
      (setq semantic-bovinate-nonterminal-check-obarray
            (make-vector 13 nil)))
  (let* ((nt (symbol-name nonterminal))
         (vs (symbol-value
              (intern-soft
               nt semantic-bovinate-nonterminal-check-obarray))))
    (if (memq stream vs)
        ;; Always enter debugger to see the backtrace
        (let ((debug-on-signal t)
              (debug-on-error  t))
          (setq semantic-bovinate-nonterminal-check-obarray nil)
          (error "Infinite recursive parse suspected on %s" nt))
      (set (intern nt semantic-bovinate-nonterminal-check-obarray)
           (cons stream vs)))))

;;;###autoload
(defun semantic-bovinate-stream (stream &optional nonterminal)
  "Bovinate STREAM, starting at the first NONTERMINAL rule.
Use `bovine-toplevel' if NONTERMINAL is not provided.
This is the core routine for converting a stream into a table.
Return the list (STREAM SEMANTIC-STREAM) where STREAM are those
elements of STREAM that have not been used.  SEMANTIC-STREAM is the
list of semantic tokens found."
  (if (not nonterminal)
      (setq nonterminal 'bovine-toplevel))

  ;; Try to detect infinite recursive parse when doing a full reparse.
  (or semantic--buffer-cache
      (semantic-bovinate-nonterminal-check stream nonterminal))

  (let* ((table semantic--parse-table)
	 (matchlist (cdr (assq nonterminal table)))
	 (starting-stream stream)
	 (nt-loop  t)		  ;non-terminal loop condition
	 nt-popup                 ;non-nil if return from nt recursion
	 nt-stack		  ;non-terminal recursion stack
	 s			  ;Temp Stream Tracker
	 lse			  ;Local Semantic Element
	 lte			  ;Local matchlist element
	 tev			  ;Matchlist entry values from buffer
	 val			  ;Value found in buffer.
	 cvl			  ;collected values list.
	 out			  ;Output
	 end			  ;End of match
	 result
	 )
    (condition-case debug-condition
        (while nt-loop
          (catch 'push-non-terminal
            (setq nt-popup nil
                  end (semantic-lex-token-end (car stream)))
            (while (or nt-loop nt-popup)
              (setq nt-loop nil
                    out     nil)
              (while (or nt-popup matchlist)
                (if nt-popup
                    ;; End of a non-terminal recursion
                    (setq nt-popup nil)
                  ;; New matching process
                  (setq s   stream      ;init s from stream.
                        cvl nil     ;re-init the collected value list.
                        lte (car matchlist) ;Get the local matchlist entry.
                        )
                  (if (or (byte-code-function-p (car lte))
                          (listp (car lte)))
                      ;; In this case, we have an EMPTY match!  Make
                      ;; stuff up.
                      (setq cvl (list nil))))

                (while (and lte
                            (not (byte-code-function-p (car lte)))
                            (not (listp (car lte))))

                  ;; GRAMMAR SOURCE DEBUGGING!
                  (if (and (boundp 'semantic-debug-enabled)
			   semantic-debug-enabled)
                      (let* ((db-nt   (semantic-bovinate-nonterminal-db-nt))
                             (db-ml   (cdr (assq db-nt table)))
                             (db-mlen (length db-ml))
                             (db-midx (- db-mlen (length matchlist)))
                             (db-tlen (length (nth db-midx db-ml)))
                             (db-tidx (- db-tlen (length lte)))
			     (frame (progn
				      (require 'semantic/bovine/debug)
				      (semantic-bovine-debug-create-frame
				       db-nt db-midx db-tidx cvl (car s))))
			     (cmd (semantic-debug-break frame))
			     )
                        (cond ((eq 'fail cmd) (setq lte '(trash 0 . 0)))
			      ((eq 'quit cmd) (signal 'quit "Abort"))
			      ((eq 'abort cmd) (error "Abort"))
			      ;; support more commands here.

			      )))
                  ;; END GRAMMAR SOURCE DEBUGGING!

                  (cond
                   ;; We have a nonterminal symbol.  Recurse inline.
                   ((setq nt-loop (assq (car lte) table))

                    (setq
                     ;; push state into the nt-stack
                     nt-stack (cons (vector matchlist cvl lte stream end
                                            )
                                    nt-stack)
                     ;; new non-terminal matchlist
                     matchlist   (cdr nt-loop)
                     ;; new non-terminal stream
                     stream      s)

                    (throw 'push-non-terminal t)

                    )
                   ;; Default case
                   (t
                    (setq lse (car s)   ;Get the local stream element
                          s   (cdr s))  ;update stream.
                    ;; Do the compare
                    (if (eq (car lte) (semantic-lex-token-class lse)) ;syntactic match
                        (let ((valdot (semantic-lex-token-bounds lse)))
                          (setq val (semantic-lex-token-text lse))
                          (setq lte (cdr lte))
                          (if (stringp (car lte))
                              (progn
                                (setq tev (car lte)
                                      lte (cdr lte))
                                (if (string-match tev val)
                                    (setq cvl (cons
                                               (if (memq (semantic-lex-token-class lse)
                                                         '(comment semantic-list))
                                                   valdot val)
                                               cvl)) ;append this value
                                  (setq lte nil cvl nil))) ;clear the entry (exit)
                            (setq cvl (cons
                                       (if (memq (semantic-lex-token-class lse)
                                                 '(comment semantic-list))
                                           valdot val) cvl))) ;append unchecked value.
                          (setq end (semantic-lex-token-end lse))
                          )
                      (setq lte nil cvl nil)) ;No more matches, exit
                    )))
                (if (not cvl)           ;lte=nil;  there was no match.
                    (setq matchlist (cdr matchlist)) ;Move to next matchlist entry
                  (let ((start (semantic-lex-token-start (car stream))))
                    (setq out (cond
                               ((car lte)
                                (funcall (car lte) ;call matchlist fn on values
                                         (nreverse cvl) start end))
                               ((and (= (length cvl) 1)
                                     (listp (car cvl))
                                     (not (numberp (car (car cvl)))))
                                (append (car cvl) (list start end)))
                               (t
                                ;;(append (nreverse cvl) (list start end))))
                                ;; MAYBE THE FOLLOWING NEEDS LESS CONS
                                ;; CELLS THAN THE ABOVE?
                                (nreverse (cons end (cons start cvl)))))
                          matchlist nil) ;;generate exit condition
                    (if (not end)
                        (setq out nil)))
                  ;; Nothing?
                  ))
              (setq result
                    (if (eq s starting-stream)
                        (list (cdr s) nil)
                      (list s out)))
              (if nt-stack
                  ;; pop previous state from the nt-stack
                  (let ((state (car nt-stack)))

                    (setq nt-popup    t
                          ;; pop actual parser state
                          matchlist   (aref state 0)
                          cvl         (aref state 1)
                          lte         (aref state 2)
                          stream      (aref state 3)
                          end         (aref state 4)
                          ;; update the stack
                          nt-stack    (cdr nt-stack))

                    (if out
                        (let ((len (length out))
                              (strip (nreverse (cdr (cdr (reverse out))))))
                          (setq end (nth (1- len) out) ;reset end to the end of exp
                                cvl (cons strip cvl) ;prepend value of exp
                                lte (cdr lte)) ;update the local table entry
                          )
                      ;; No value means that we need to terminate this
                      ;; match.
                      (setq lte nil cvl nil)) ;No match, exit
                    )))))
      (error
       ;; On error just move forward the stream of lexical tokens
       (setq result (list (cdr starting-stream) nil))
       (when (and (boundp 'semantic-debug-enabled)
		  semantic-debug-enabled)
	 (require 'semantic/bovine/debug)
	 (let ((frame (semantic-create-bovine-debug-error-frame
		       debug-condition)))
	   (semantic-debug-break frame)))))
    result))

;; Make it the default parser
;;;###autoload
(defalias 'semantic-parse-stream-default 'semantic-bovinate-stream)

(provide 'semantic/bovine)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "semantic/bovine"
;; End:

;;; semantic/bovine.el ends here
