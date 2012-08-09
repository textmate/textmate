;;; semantic/wisent/wisent.el --- GNU Bison for Emacs - Runtime

;;; Copyright (C) 2002-2007, 2009-2012  Free Software Foundation, Inc.

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 30 January 2002
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
;; Parser engine and runtime of Wisent.
;;
;; Wisent (the European Bison ;-) is an Elisp implementation of the
;; GNU Compiler Compiler Bison.  The Elisp code is a port of the C
;; code of GNU Bison 1.28 & 1.31.
;;
;; For more details on the basic concepts for understanding Wisent,
;; read the Bison manual ;)
;;
;; For more details on Wisent itself read the Wisent manual.

;;; History:
;;

;;; Code:

(defgroup wisent nil
  "
           /\\_.-^^^-._/\\     The GNU
           \\_         _/
            (     `o  `      (European ;-) Bison
             \\      ` /
             (   D  ,¨       for Emacs!
              ` ~ ,¨
               `\"\""
  :group 'semantic)


;;;; -------------
;;;; Runtime stuff
;;;; -------------

;;; Compatibility
(eval-and-compile
  (if (fboundp 'char-valid-p)
      (defalias 'wisent-char-p 'char-valid-p)
    (defalias 'wisent-char-p 'char-or-char-int-p)))

;;; Printed representation of terminals and nonterminals
(defconst wisent-escape-sequence-strings
  '(
    (?\a . "'\\a'")                     ; C-g
    (?\b . "'\\b'")                     ; backspace, BS, C-h
    (?\t . "'\\t'")                     ; tab, TAB, C-i
    (?\n  . "'\\n'")                    ; newline, C-j
    (?\v . "'\\v'")                     ; vertical tab, C-k
    (?\f . "'\\f'")                     ; formfeed character, C-l
    (?\r . "'\\r'")                     ; carriage return, RET, C-m
    (?\e . "'\\e'")                     ; escape character, ESC, C-[
    (?\\ . "'\\'")                      ; backslash character, \
    (?\d . "'\\d'")                     ; delete character, DEL
    )
  "Printed representation of usual escape sequences.")

(defsubst wisent-item-to-string (item)
  "Return a printed representation of ITEM.
ITEM can be a nonterminal or terminal symbol, or a character literal."
  (if (wisent-char-p item)
        (or (cdr (assq item wisent-escape-sequence-strings))
            (format "'%c'" item))
    (symbol-name item)))

(defsubst wisent-token-to-string (token)
  "Return a printed representation of lexical token TOKEN."
  (format "%s%s(%S)" (wisent-item-to-string (car token))
          (if (nth 2 token) (format "@%s" (nth 2 token)) "")
          (nth 1 token)))

;;; Special symbols
(defconst wisent-eoi-term '$EOI
  "End Of Input token.")

(defconst wisent-error-term 'error
  "Error recovery token.")

(defconst wisent-accept-tag 'accept
  "Accept result after input successfully parsed.")

(defconst wisent-error-tag 'error
  "Process a syntax error.")

;;; Special functions
(defun wisent-automaton-p (obj)
  "Return non-nil if OBJ is a LALR automaton.
If OBJ is a symbol check its value."
  (and obj (symbolp obj) (boundp obj)
       (setq obj (symbol-value obj)))
  (and (vectorp obj) (= 4 (length obj))
       (vectorp (aref obj 0)) (vectorp (aref obj 1))
       (= (length (aref obj 0)) (length (aref obj 1)))
       (listp (aref obj 2)) (vectorp (aref obj 3))))

(defsubst wisent-region (&rest positions)
  "Return the start/end positions of the region including POSITIONS.
Each element of POSITIONS is a pair (START-POS . END-POS) or nil.  The
returned value is the pair (MIN-START-POS . MAX-END-POS) or nil if no
POSITIONS are available."
  (let ((pl (delq nil positions)))
    (if pl
        (cons (apply #'min (mapcar #'car pl))
              (apply #'max (mapcar #'cdr pl))))))

;;; Reporting
(defvar wisent-parse-verbose-flag nil
  "*Non-nil means to issue more messages while parsing.")

(defun wisent-parse-toggle-verbose-flag ()
  "Toggle whether to issue more messages while parsing."
  (interactive)
  (setq wisent-parse-verbose-flag (not wisent-parse-verbose-flag))
  (when (called-interactively-p 'interactive)
    (message "More messages while parsing %sabled"
             (if wisent-parse-verbose-flag "en" "dis"))))

(defsubst wisent-message (string &rest args)
  "Print a one-line message if `wisent-parse-verbose-flag' is set.
Pass STRING and ARGS arguments to `message'."
  (and wisent-parse-verbose-flag
       (apply 'message string args)))

;;;; --------------------
;;;; The LR parser engine
;;;; --------------------

(defcustom wisent-parse-max-stack-size 500
  "The parser stack size."
  :type 'integer
  :group 'wisent)

(defcustom wisent-parse-max-recover 3
  "Number of tokens to shift before turning off error status."
  :type 'integer
  :group 'wisent)

(defvar wisent-discarding-token-functions nil
  "List of functions to be called when discarding a lexical token.
These functions receive the lexical token discarded.
When the parser encounters unexpected tokens, it can discards them,
based on what directed by error recovery rules.  Either when the
parser reads tokens until one is found that can be shifted, or when an
semantic action calls the function `wisent-skip-token' or
`wisent-skip-block'.
For language specific hooks, make sure you define this as a local
hook.")

(defvar wisent-pre-parse-hook nil
  "Normal hook run just before entering the LR parser engine.")

(defvar wisent-post-parse-hook nil
  "Normal hook run just after the LR parser engine terminated.")

(defvar wisent-loop nil
  "The current parser action.
Stop parsing when set to nil.
This variable only has meaning in the scope of `wisent-parse'.")

(defvar wisent-nerrs nil
  "The number of parse errors encountered so far.")

(defvar wisent-lookahead nil
  "The lookahead lexical token.
This value is non-nil if the parser terminated because of an
unrecoverable error.")

;; Variables and macros that are useful in semantic actions.
(defvar wisent-parse-lexer-function nil
  "The user supplied lexer function.
This function don't have arguments.
This variable only has meaning in the scope of `wisent-parse'.")

(defvar wisent-parse-error-function nil
  "The user supplied error function.
This function must accept one argument, a message string.
This variable only has meaning in the scope of `wisent-parse'.")

(defvar wisent-input nil
  "The last token read.
This variable only has meaning in the scope of `wisent-parse'.")

(defvar wisent-recovering nil
  "Non-nil means that the parser is recovering.
This variable only has meaning in the scope of `wisent-parse'.")

;; Variables that only have meaning in the scope of a semantic action.
;; These global definitions avoid byte-compiler warnings.
(defvar $region nil)
(defvar $nterm  nil)
(defvar $action nil)

(defmacro wisent-lexer ()
  "Obtain the next terminal in input."
  '(funcall wisent-parse-lexer-function))

(defmacro wisent-error (msg)
  "Call the user supplied error reporting function with message MSG."
  `(funcall wisent-parse-error-function ,msg))

(defmacro wisent-errok ()
  "Resume generating error messages immediately for subsequent syntax errors.
This is useful primarily in error recovery semantic actions."
  '(setq wisent-recovering nil))

(defmacro wisent-clearin ()
  "Discard the current lookahead token.
This will cause a new lexical token to be read.
This is useful primarily in error recovery semantic actions."
  '(setq wisent-input nil))

(defmacro wisent-abort ()
  "Abort parsing and save the lookahead token.
This is useful primarily in error recovery semantic actions."
  '(setq wisent-lookahead wisent-input
         wisent-loop nil))

(defmacro wisent-set-region (start end)
  "Change the region of text matched by the current nonterminal.
START and END are respectively the beginning and end positions of the
region.  If START or END values are not a valid positions the region
is set to nil."
  `(setq $region (and (number-or-marker-p ,start)
                      (number-or-marker-p ,end)
                      (cons ,start ,end))))

(defun wisent-skip-token ()
  "Skip the lookahead token in order to resume parsing.
Return nil.
Must be used in error recovery semantic actions."
  (if (eq (car wisent-input) wisent-eoi-term)
      ;; Does nothing at EOI to avoid infinite recovery loop.
      nil
    (wisent-message "%s: skip %s" $action
                    (wisent-token-to-string wisent-input))
    (run-hook-with-args
     'wisent-discarding-token-functions wisent-input)
    (wisent-clearin)
    (wisent-errok)))

(defun wisent-skip-block (&optional bounds)
  "Safely skip a parenthesized block in order to resume parsing.
Return nil.
Must be used in error recovery semantic actions.
Optional argument BOUNDS is a pair (START . END) which indicates where
the parenthesized block starts.  Typically the value of a `$regionN'
variable, where `N' is the Nth element of the current rule components
that match the block beginning.  It defaults to the value of the
`$region' variable."
  (let ((start (car (or bounds $region)))
        end input)
    (if (not (number-or-marker-p start))
        ;; No nonterminal region available, skip the lookahead token.
        (wisent-skip-token)
      ;; Try to skip a block.
      (if (not (setq end (save-excursion
                           (goto-char start)
                           (and (looking-at "\\s(")
                                (condition-case nil
                                    (1- (scan-lists (point) 1 0))
                                  (error nil))))))
          ;; Not actually a block, skip the lookahead token.
          (wisent-skip-token)
        ;; OK to safely skip the block, so read input until a matching
        ;; close paren or EOI is encountered.
        (setq input wisent-input)
        (while (and (not (eq (car input) wisent-eoi-term))
                    (< (nth 2 input) end))
          (run-hook-with-args
           'wisent-discarding-token-functions input)
          (setq input (wisent-lexer)))
        (wisent-message "%s: in enclosing block, skip from %s to %s"
                        $action
                        (wisent-token-to-string wisent-input)
                        (wisent-token-to-string input))
        (if (eq (car wisent-input) wisent-eoi-term)
            ;; Does nothing at EOI to avoid infinite recovery loop.
            nil
          (wisent-clearin)
          (wisent-errok))
        ;; Set end of $region to end of block.
        (wisent-set-region (car $region) (1+ end))
        nil))))

;;; Core parser engine
(defsubst wisent-production-bounds (stack i j)
  "Determine the start and end locations of a production value.
Return a pair (START . END), where START is the first available start
location, and END the last available end location, in components
values of the rule currently reduced.
Return nil when no component location is available.
STACK is the parser stack.
I and J are the indices in STACK of respectively the value of the
first and last components of the current rule.
This function is for internal use by semantic actions' generated
lambda-expression."
  (let ((f (cadr (aref stack i)))
        (l (cddr (aref stack j))))
    (while (/= i j)
      (cond
       ((not f) (setq f (cadr (aref stack (setq i (+ i 2))))))
       ((not l) (setq l (cddr (aref stack (setq j (- j 2))))))
       ((setq i j))))
    (and f l (cons f l))))

(defmacro wisent-parse-action (i al)
  "Return the next parser action.
I is a token item number and AL is the list of (item . action)
available at current state.  The first element of AL contains the
default action for this state."
  `(cdr (or (assq ,i ,al) (car ,al))))

(defsubst wisent-parse-start (start starts)
  "Return the first lexical token to shift for START symbol.
STARTS is the table of allowed start symbols or nil if the LALR
automaton has only one entry point."
  (if (null starts)
      ;; Only one entry point, return the first lexical token
      ;; available in input.
      (wisent-lexer)
    ;; Multiple start symbols defined, return the internal lexical
    ;; token associated to START.  By default START is the first
    ;; nonterminal defined in STARTS.
    (let ((token (cdr (if start (assq start starts) (car starts)))))
      (if token
          (list token (symbol-name token))
        (error "Invalid start symbol %s" start)))))

(defun wisent-parse (automaton lexer &optional error start)
  "Parse input using the automaton specified in AUTOMATON.

- AUTOMATON is an LALR(1) automaton generated by
  `wisent-compile-grammar'.

- LEXER is a function with no argument called by the parser to obtain
  the next terminal (token) in input.

- ERROR is an optional reporting function called when a parse error
  occurs.  It receives a message string to report.  It defaults to the
  function `wisent-message'.

- START specify the start symbol (nonterminal) used by the parser as
  its goal.  It defaults to the start symbol defined in the grammar
  \(see also `wisent-compile-grammar')."
  (run-hooks 'wisent-pre-parse-hook)
  (let* ((actions (aref automaton 0))
         (gotos   (aref automaton 1))
         (starts  (aref automaton 2))
         (stack (make-vector wisent-parse-max-stack-size nil))
         (sp 0)
         (wisent-loop t)
         (wisent-parse-error-function (or error 'wisent-message))
         (wisent-parse-lexer-function lexer)
         (wisent-recovering nil)
         (wisent-input (wisent-parse-start start starts))
         state tokid choices choice)
    (setq wisent-nerrs     0 ;; Reset parse error counter
          wisent-lookahead nil) ;; and lookahead token
    (aset stack 0 0) ;; Initial state
    (while wisent-loop
      (setq state (aref stack sp)
            tokid (car wisent-input)
            wisent-loop (wisent-parse-action tokid (aref actions state)))
      (cond

       ;; Input successfully parsed
       ;; -------------------------
       ((eq wisent-loop wisent-accept-tag)
        (setq wisent-loop nil))

       ;; Syntax error in input
       ;; ---------------------
       ((eq wisent-loop wisent-error-tag)
        ;; Report this error if not already recovering from an error.
        (setq choices (aref actions state))
        (or wisent-recovering
            (wisent-error
             (format "Syntax error, unexpected %s, expecting %s"
                     (wisent-token-to-string wisent-input)
                     (mapconcat 'wisent-item-to-string
                                (delq wisent-error-term
                                      (mapcar 'car (cdr choices)))
                                ", "))))
        ;; Increment the error counter
        (setq wisent-nerrs (1+ wisent-nerrs))
        ;; If just tried and failed to reuse lookahead token after an
        ;; error, discard it.
        (if (eq wisent-recovering wisent-parse-max-recover)
            (if (eq tokid wisent-eoi-term)
                (wisent-abort) ;; Terminate if at end of input.
              (wisent-message "Error recovery: skip %s"
                              (wisent-token-to-string wisent-input))
              (run-hook-with-args
               'wisent-discarding-token-functions wisent-input)
              (setq wisent-input (wisent-lexer)))

          ;; Else will try to reuse lookahead token after shifting the
          ;; error token.

          ;; Each real token shifted decrements this.
          (setq wisent-recovering wisent-parse-max-recover)
          ;; Pop the value/state stack to see if an action associated
          ;; to special terminal symbol 'error exists.
          (while (and (>= sp 0)
                      (not (and (setq state   (aref stack sp)
                                      choices (aref actions state)
                                      choice  (assq wisent-error-term choices))
                                (natnump (cdr choice)))))
            (setq sp (- sp 2)))

          (if (not choice)
              ;; No 'error terminal was found.  Just terminate.
              (wisent-abort)
            ;; Try to recover and continue parsing.
            ;; Shift the error terminal.
            (setq state (cdr choice)    ; new state
                  sp    (+ sp 2))
            (aset stack (1- sp) nil)    ; push value
            (aset stack sp state)       ; push new state
            ;; Adjust input to error recovery state.  Unless 'error
            ;; triggers a reduction, eat the input stream until an
            ;; expected terminal symbol is found, or EOI is reached.
            (if (cdr (setq choices (aref actions state)))
                (while (not (or (eq (car wisent-input) wisent-eoi-term)
                                (assq (car wisent-input) choices)))
                  (wisent-message "Error recovery: skip %s"
                                  (wisent-token-to-string wisent-input))
                  (run-hook-with-args
                   'wisent-discarding-token-functions wisent-input)
                  (setq wisent-input (wisent-lexer)))))))

       ;; Shift current token on top of the stack
       ;; ---------------------------------------
       ((natnump wisent-loop)
        ;; Count tokens shifted since error; after
        ;; `wisent-parse-max-recover', turn off error status.
        (setq wisent-recovering (and (natnump wisent-recovering)
                                     (> wisent-recovering 1)
                                     (1- wisent-recovering)))
        (setq sp (+ sp 2))
        (aset stack (1- sp) (cdr wisent-input))
        (aset stack sp wisent-loop)
        (setq wisent-input (wisent-lexer)))

       ;; Reduce by rule (call semantic action)
       ;; -------------------------------------
       (t
        (setq sp (funcall wisent-loop stack sp gotos))
        (or wisent-input (setq wisent-input (wisent-lexer))))))
    (run-hooks 'wisent-post-parse-hook)
    (car (aref stack 1))))

(provide 'semantic/wisent/wisent)

;;; semantic/wisent/wisent.el ends here
