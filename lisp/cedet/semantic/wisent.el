;;; semantic/wisent.el --- Wisent - Semantic gateway

;; Copyright (C) 2001-2007, 2009-2012  Free Software Foundation, Inc.

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 30 Aug 2001
;; Keywords: syntax

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
;; Here are functions necessary to use the Wisent LALR parser from
;; Semantic environment.

;;; History:
;;

;;; Code:

(require 'semantic)
(require 'semantic/wisent/wisent)

;;; Lexical analysis
;;
(defvar wisent-lex-istream nil
  "Input stream of `semantic-lex' syntactic tokens.")

(defvar wisent-lex-lookahead nil
  "Extra lookahead token.
When non-nil it is directly returned by `wisent-lex-function'.")

;; Maintain this alias for compatibility until all WY grammars have
;; been translated again to Elisp code.
(semantic-alias-obsolete 'wisent-lex-make-token-table
                         'semantic-lex-make-type-table "23.2")

(defmacro wisent-lex-eoi ()
  "Return an End-Of-Input lexical token.
The EOI token is like this: ($EOI \"\" POINT-MAX . POINT-MAX)."
  `(cons ',wisent-eoi-term
         (cons ""
               (cons (point-max) (point-max)))))

(defmacro define-wisent-lexer (name doc &rest body)
  "Create a new lexical analyzer with NAME.
DOC is a documentation string describing this analyzer.
When a token is available in `wisent-lex-istream', eval BODY forms
sequentially.  BODY must return a lexical token for the LALR parser.

Each token in input was produced by `semantic-lex', it is a list:

  (TOKSYM START . END)

TOKSYM is a terminal symbol used in the grammar.
START and END mark boundary in the current buffer of that token's
value.

Returned tokens must have the form:

  (TOKSYM VALUE START . END)

where VALUE is the buffer substring between START and END positions."
  `(defun
     ,name () ,doc
     (cond
      (wisent-lex-lookahead
       (prog1 wisent-lex-lookahead
         (setq wisent-lex-lookahead nil)))
      (wisent-lex-istream
       ,@body)
      ((wisent-lex-eoi)))))

(define-wisent-lexer wisent-lex
  "Return the next available lexical token in Wisent's form.
The variable `wisent-lex-istream' contains the list of lexical tokens
produced by `semantic-lex'.  Pop the next token available and convert
it to a form suitable for the Wisent's parser."
  (let* ((tk (car wisent-lex-istream)))
    ;; Eat input stream
    (setq wisent-lex-istream (cdr wisent-lex-istream))
    (cons (semantic-lex-token-class tk)
          (cons (semantic-lex-token-text tk)
                (semantic-lex-token-bounds tk)))))

;;; Syntax analysis
;;
(defvar wisent-error-function nil
  "Function used to report parse error.
By default use the function `wisent-message'.")
(make-variable-buffer-local 'wisent-error-function)

(defvar wisent-lexer-function 'wisent-lex
  "Function used to obtain the next lexical token in input.
Should be a lexical analyzer created with `define-wisent-lexer'.")
(make-variable-buffer-local 'wisent-lexer-function)

;; Tag production
;;
(defsubst wisent-raw-tag (semantic-tag)
  "Return raw form of given Semantic tag SEMANTIC-TAG.
Should be used in semantic actions, in grammars, to build a Semantic
parse tree."
  (nconc semantic-tag
         (if (or $region
                 (setq $region (nthcdr 2 wisent-input)))
             (list (car $region) (cdr $region))
           (list (point-max) (point-max)))))

(defsubst wisent-cook-tag (raw-tag)
  "From raw form of Semantic tag RAW-TAG, return a list of cooked tags.
Should be used in semantic actions, in grammars, to build a Semantic
parse tree."
  (let* ((cooked (semantic--tag-expand raw-tag))
         (l cooked))
    (while l
      (semantic--tag-put-property (car l) 'reparse-symbol $nterm)
      (setq l (cdr l)))
    cooked))

;; Unmatched syntax collector
;;
(defun wisent-collect-unmatched-syntax (nomatch)
  "Add lexical token NOMATCH to the cache of unmatched tokens.
See also the variable `semantic-unmatched-syntax-cache'.

NOMATCH is in Wisent's form: (SYMBOL VALUE START . END)
and will be collected in `semantic-lex' form: (SYMBOL START . END)."
  (let ((region (cddr nomatch)))
    (and (number-or-marker-p (car region))
         (number-or-marker-p (cdr region))
         (setq semantic-unmatched-syntax-cache
               (cons (cons (car nomatch) region)
                     semantic-unmatched-syntax-cache)))))

;; Parser plug-ins
;;
;; The following functions permit to plug the Wisent LALR parser in
;; Semantic toolkit.  They use the standard API provided by Semantic
;; to plug parsers in.
;;
;; Two plug-ins are available, BUT ONLY ONE MUST BE USED AT A TIME:
;;
;; - `wisent-parse-stream' designed to override the standard function
;;   `semantic-parse-stream'.
;;
;; - `wisent-parse-region' designed to override the standard function
;;   `semantic-parse-region'.
;;
;; Maybe the latter is faster because it eliminates a lot of function
;; call.
;;
(defun wisent-parse-stream (stream goal)
  "Parse STREAM using the Wisent LALR parser.
GOAL is a nonterminal symbol to start parsing at.
Return the list (STREAM SEMANTIC-STREAM) where STREAM are those
elements of STREAM that have not been used.  SEMANTIC-STREAM is the
list of semantic tags found.
The LALR parser automaton must be available in buffer local variable
`semantic--parse-table'.

Must be installed by `semantic-install-function-overrides' to override
the standard function `semantic-parse-stream'."
  (let (wisent-lex-istream wisent-lex-lookahead la-elt cache)

    ;; IMPLEMENTATION NOTES:
    ;; `wisent-parse' returns a lookahead token when it stopped
    ;; parsing before encountering the end of input.  To re-enter the
    ;; parser it is necessary to push back in the lexical input stream
    ;; the last lookahead token issued.  Because the format of
    ;; lookahead tokens and tokens in STREAM can be different the
    ;; lookahead token is put in the variable `wisent-lex-lookahead'
    ;; before calling `wisent-parse'.  Wisent's lexers always pop the
    ;; next lexical token from that variable when non nil, then from
    ;; the lexical input stream.
    ;;
    ;; The first element of STREAM is used to keep lookahead tokens
    ;; across successive calls to `wisent-parse-stream'.  In fact
    ;; what is kept is a stack of lookaheads encountered so far.  It
    ;; is cleared when `wisent-parse' returns a valid semantic tag,
    ;; or twice the same lookahead token!  The latter indicates that
    ;; there is a syntax error on that token.  If so, tokens currently
    ;; in the lookahead stack have not been used, and are moved into
    ;; `semantic-unmatched-syntax-cache'.  When the parser will be
    ;; re-entered, a new lexical token will be read from STREAM.
    ;;
    ;; The first element of STREAM that contains the lookahead stack
    ;; has this format (compatible with the format of `semantic-lex'
    ;; tokens):
    ;;
    ;; (LOOKAHEAD-STACK START . END)
    ;;
    ;; where LOOKAHEAD-STACK is a list of lookahead tokens.  And
    ;; START/END are the bounds of the lookahead at top of stack.

    ;; Retrieve lookahead token from stack
    (setq la-elt (car stream))
    (if (consp (car la-elt))
        ;; The first elt of STREAM contains a lookahead stack
        (setq wisent-lex-lookahead (caar la-elt)
              stream (cdr stream))
      (setq la-elt nil))
    ;; Parse
    (setq wisent-lex-istream stream
          cache (semantic-safe "wisent-parse-stream: %s"
                  (condition-case error-to-filter
                      (wisent-parse semantic--parse-table
                                    wisent-lexer-function
                                    wisent-error-function
                                    goal)
                    (args-out-of-range
                     (if (and (not debug-on-error)
                              (= wisent-parse-max-stack-size
                                 (nth 2 error-to-filter)))
                         (progn
                           (message "wisent-parse-stream: %s"
                                    (error-message-string error-to-filter))
                           (message "wisent-parse-max-stack-size \
might need to be increased"))
                       (apply 'signal error-to-filter))))))
    ;; Manage returned lookahead token
    (if wisent-lookahead
        (if (eq (caar la-elt) wisent-lookahead)
            ;; It is already at top of lookahead stack
            (progn
              (setq cache nil
                    la-elt (car la-elt))
              (while la-elt
                ;; Collect unmatched tokens from the stack
                (run-hook-with-args
                 'wisent-discarding-token-functions (car la-elt))
                (setq la-elt (cdr la-elt))))
          ;; New lookahead token
          (if (or (consp cache) ;; Clear the stack if parse succeeded
                  (null la-elt))
              (setq la-elt (cons nil nil)))
          ;; Push it into the stack
          (setcar la-elt (cons wisent-lookahead (car la-elt)))
          ;; Update START/END
          (setcdr la-elt (cddr wisent-lookahead))
          ;; Push (LOOKAHEAD-STACK START . END) in STREAM
          (setq wisent-lex-istream (cons la-elt wisent-lex-istream))))
    ;; Return (STREAM SEMANTIC-STREAM)
    (list wisent-lex-istream
          (if (consp cache) cache '(nil))
          )))

(defun wisent-parse-region (start end &optional goal depth returnonerror)
  "Parse the area between START and END using the Wisent LALR parser.
Return the list of semantic tags found.
Optional arguments GOAL is a nonterminal symbol to start parsing at,
DEPTH is the lexical depth to scan, and RETURNONERROR is a flag to
stop parsing on syntax error, when non-nil.
The LALR parser automaton must be available in buffer local variable
`semantic--parse-table'.

Must be installed by `semantic-install-function-overrides' to override
the standard function `semantic-parse-region'."
  (if (or (< start (point-min)) (> end (point-max)) (< end start))
      (error "Invalid bounds [%s %s] passed to `wisent-parse-region'"
             start end))
  (let* ((case-fold-search semantic-case-fold)
         (wisent-lex-istream (semantic-lex start end depth))
         ptree tag cooked lstack wisent-lex-lookahead)
    ;; Loop while there are lexical tokens available
    (while wisent-lex-istream
      ;; Parse
      (setq wisent-lex-lookahead (car lstack)
            tag (semantic-safe "wisent-parse-region: %s"
                    (wisent-parse semantic--parse-table
                                  wisent-lexer-function
                                  wisent-error-function
                                  goal)))
      ;; Manage returned lookahead token
      (if wisent-lookahead
          (if (eq (car lstack) wisent-lookahead)
              ;; It is already at top of lookahead stack
              (progn
                (setq tag nil)
                (while lstack
                  ;; Collect unmatched tokens from lookahead stack
                  (run-hook-with-args
                   'wisent-discarding-token-functions (car lstack))
                  (setq lstack (cdr lstack))))
            ;; Push new lookahead token into the stack
            (setq lstack (cons wisent-lookahead lstack))))
      ;; Manage the parser result
      (cond
       ;; Parse succeeded, cook result
       ((consp tag)
        (setq lstack nil ;; Clear the lookahead stack
              cooked (semantic--tag-expand tag)
              ptree (append cooked ptree))
        (while cooked
          (setq tag    (car cooked)
                cooked (cdr cooked))
          (or (semantic--tag-get-property tag 'reparse-symbol)
              (semantic--tag-put-property tag 'reparse-symbol goal)))
        )
       ;; Return on error if requested
       (returnonerror
        (setq wisent-lex-istream nil)
        ))
      ;; Work in progress...
      (if wisent-lex-istream
	  (and (eq semantic-working-type 'percent)
	       (boundp 'semantic--progress-reporter)
	       semantic--progress-reporter
	       (progress-reporter-update
		semantic--progress-reporter
		(/ (* 100 (semantic-lex-token-start
			   (car wisent-lex-istream)))
		   (point-max))))))
    ;; Return parse tree
    (nreverse ptree)))

;;; Interfacing with edebug
;;
(add-hook
 'edebug-setup-hook
 #'(lambda ()

     (def-edebug-spec define-wisent-lexer
       (&define name stringp def-body)
       )

     ))

(provide 'semantic/wisent)

;;; semantic/wisent.el ends here
