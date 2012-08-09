;;; semantic/wisent/comp.el --- GNU Bison for Emacs - Grammar compiler

;; Copyright (C) 1984, 1986, 1989, 1992, 1995, 2000-2007, 2009-2012
;;   Free Software Foundation, Inc.

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
;; Grammar compiler that produces Wisent's LALR automatons.
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
(require 'semantic/wisent)

;;;; -------------------
;;;; Misc. useful things
;;;; -------------------

;; As much as possible I would like to keep the name of global
;; variables used in Bison without polluting too much the Elisp global
;; name space.  Elisp dynamic binding allows that ;-)

;; Here are simple macros to easily define and use set of variables
;; bound locally, without all these "reference to free variable"
;; compiler warnings!

(defmacro wisent-context-name (name)
  "Return the context name from NAME."
  `(if (and ,name (symbolp ,name))
       (intern (format "wisent-context-%s" ,name))
     (error "Invalid context name: %S" ,name)))

(defmacro wisent-context-bindings (name)
  "Return the variables in context NAME."
  `(symbol-value (wisent-context-name ,name)))

(defmacro wisent-defcontext (name &rest vars)
  "Define a context NAME that will bind variables VARS."
  (let* ((context (wisent-context-name name))
         (bindings (mapcar #'(lambda (v) (list 'defvar v)) vars)))
    `(eval-when-compile
       ,@bindings
       (defvar ,context ',vars))))
(put 'wisent-defcontext 'lisp-indent-function 1)

(defmacro wisent-with-context (name &rest body)
  "Bind variables in context NAME then eval BODY."
  `(let* ,(wisent-context-bindings name)
     ,@body))
(put 'wisent-with-context 'lisp-indent-function 1)

;; A naive implementation of data structures!  But it suffice here ;-)

(defmacro wisent-struct (name &rest fields)
  "Define a simple data structure called NAME.
Which contains data stored in FIELDS.  FIELDS is a list of symbols
which are field names or pairs (FIELD INITIAL-VALUE) where
INITIAL-VALUE is a constant used as the initial value of FIELD when
the data structure is created.  INITIAL-VALUE defaults to nil.

This defines a `make-NAME' constructor, get-able `NAME-FIELD' and
set-able `set-NAME-FIELD' accessors."
  (let ((size (length fields))
        (i    0)
        accors field sufx fun ivals)
    (while (< i size)
      (setq field  (car fields)
            fields (cdr fields))
      (if (consp field)
          (setq ivals (cons (cadr field) ivals)
                field (car field))
        (setq ivals (cons nil ivals)))
      (setq sufx   (format "%s-%s" name field)
            fun    (intern (format "%s" sufx))
            accors (cons `(defmacro ,fun (s)
                            (list 'aref s ,i))
                         accors)
            fun    (intern (format "set-%s" sufx))
            accors (cons `(defmacro ,fun (s v)
                            (list 'aset s ,i v))
                         accors)
            i      (1+ i)))
    `(progn
      (defmacro ,(intern (format "make-%s" name)) ()
        (cons 'vector ',(nreverse ivals)))
      ,@accors)))
(put 'wisent-struct 'lisp-indent-function 1)

;; Other utilities

(defsubst wisent-pad-string (s n &optional left)
  "Fill string S with spaces.
Return a new string of at least N characters.  Insert spaces on right.
If optional LEFT is non-nil insert spaces on left."
  (let ((i (length s)))
    (if (< i n)
        (if left
            (concat (make-string (- n i) ?\ ) s)
          (concat s (make-string (- n i) ?\ )))
      s)))

;;;; ------------------------
;;;; Environment dependencies
;;;; ------------------------

(defconst wisent-BITS-PER-WORD
  (let ((i 1))
    (while (not (zerop (lsh 1 i)))
      (setq i (1+ i)))
    i))

(defsubst wisent-WORDSIZE (n)
  "(N + BITS-PER-WORD - 1) / BITS-PER-WORD."
  (/ (1- (+ n wisent-BITS-PER-WORD)) wisent-BITS-PER-WORD))

(defsubst wisent-SETBIT (x i)
  "X[I/BITS-PER-WORD] |= 1 << (I % BITS-PER-WORD)."
  (let ((k (/ i wisent-BITS-PER-WORD)))
    (aset x k (logior (aref x k)
                      (lsh 1 (% i wisent-BITS-PER-WORD))))))

(defsubst wisent-RESETBIT (x i)
  "X[I/BITS-PER-WORD] &= ~(1 << (I % BITS-PER-WORD))."
  (let ((k (/ i wisent-BITS-PER-WORD)))
    (aset x k (logand (aref x k)
                      (lognot (lsh 1 (% i wisent-BITS-PER-WORD)))))))

(defsubst wisent-BITISSET (x i)
  "(X[I/BITS-PER-WORD] & (1 << (I % BITS-PER-WORD))) != 0."
  (not (zerop (logand (aref x (/ i wisent-BITS-PER-WORD))
                      (lsh 1 (% i wisent-BITS-PER-WORD))))))

(defsubst wisent-noninteractive ()
  "Return non-nil if running without interactive terminal."
  (if (featurep 'xemacs)
      (noninteractive)
    noninteractive))

(defvar wisent-debug-flag nil
  "Non-nil means enable some debug stuff.")

;;;; --------------
;;;; Logging/Output
;;;; --------------
(defconst wisent-log-buffer-name "*wisent-log*"
  "Name of the log buffer.")

(defvar wisent-new-log-flag nil
  "Non-nil means to start a new report.")

(defvar wisent-verbose-flag nil
  "*Non-nil means to report verbose information on generated parser.")

(defun wisent-toggle-verbose-flag ()
  "Toggle whether to report verbose information on generated parser."
  (interactive)
  (setq wisent-verbose-flag (not wisent-verbose-flag))
  (when (called-interactively-p 'interactive)
    (message "Verbose report %sabled"
             (if wisent-verbose-flag "en" "dis"))))

(defmacro wisent-log-buffer ()
  "Return the log buffer.
Its name is defined in constant `wisent-log-buffer-name'."
  `(get-buffer-create wisent-log-buffer-name))

(defmacro wisent-clear-log ()
  "Delete the entire contents of the log buffer."
  `(with-current-buffer (wisent-log-buffer)
     (erase-buffer)))

(defvar byte-compile-current-file)

(defun wisent-source ()
  "Return the current source file name or nil."
  (let ((source (or (and (boundp 'byte-compile-current-file)
                         byte-compile-current-file)
                    load-file-name (buffer-file-name))))
    (if source
        (file-relative-name source))))

(defun wisent-new-log ()
  "Start a new entry into the log buffer."
  (setq wisent-new-log-flag nil)
  (let ((text (format "\n\n*** Wisent %s - %s\n\n"
                      (or (wisent-source) (buffer-name))
                      (format-time-string "%Y-%m-%d %R"))))
    (with-current-buffer (wisent-log-buffer)
      (goto-char (point-max))
      (insert text))))

(defsubst wisent-log (&rest args)
  "Insert text into the log buffer.
`format' is applied to ARGS and the result string is inserted into the
log buffer returned by the function `wisent-log-buffer'."
  (and wisent-new-log-flag (wisent-new-log))
  (with-current-buffer (wisent-log-buffer)
    (insert (apply 'format args))))

(defconst wisent-log-file "wisent.output"
  "The log file.
Used when running without interactive terminal.")

(defun wisent-append-to-log-file ()
  "Append contents of logging buffer to `wisent-log-file'."
  (if (get-buffer wisent-log-buffer-name)
      (condition-case err
          (with-current-buffer (wisent-log-buffer)
            (widen)
            (if (> (point-max) (point-min))
                (write-region (point-min) (point-max)
                              wisent-log-file t)))
        (error
         (message "*** %s" (error-message-string err))))))

;;;; -----------------------------------
;;;; Representation of the grammar rules
;;;; -----------------------------------

;; ntokens is the number of tokens, and nvars is the number of
;; variables (nonterminals).  nsyms is the total number, ntokens +
;; nvars.

;; Each symbol (either token or variable) receives a symbol number.
;; Numbers 0 to ntokens-1 are for tokens, and ntokens to nsyms-1 are
;; for variables.  Symbol number zero is the end-of-input token.  This
;; token is counted in ntokens.

;; The rules receive rule numbers 1 to nrules in the order they are
;; written.  Actions and guards are accessed via the rule number.

;; The rules themselves are described by three arrays: rrhs, rlhs and
;; ritem.  rlhs[R] is the symbol number of the left hand side of rule
;; R.  The right hand side is stored as symbol numbers in a portion of
;; ritem.  rrhs[R] contains the index in ritem of the beginning of the
;; portion for rule R.

;; The length of the portion is one greater than the number of symbols
;; in the rule's right hand side.  The last element in the portion
;; contains minus R, which identifies it as the end of a portion and
;; says which rule it is for.

;; The portions of ritem come in order of increasing rule number and
;; are followed by an element which is nil to mark the end.  nitems is
;; the total length of ritem, not counting the final nil.  Each
;; element of ritem is called an "item" and its index in ritem is an
;; item number.

;; Item numbers are used in the finite state machine to represent
;; places that parsing can get to.

;; The vector rprec contains for each rule, the item number of the
;; symbol giving its precedence level to this rule.  The precedence
;; level and associativity of each symbol is recorded in respectively
;; the properties 'wisent--prec and 'wisent--assoc.

;; Precedence levels are assigned in increasing order starting with 1
;; so that numerically higher precedence values mean tighter binding
;; as they ought to.  nil as a symbol or rule's precedence means none
;; is assigned.

(defcustom wisent-state-table-size 1009
  "The size of the state table."
  :type 'integer
  :group 'wisent)

;; These variables only exist locally in the function
;; `wisent-compile-grammar' and are shared by all other nested
;; callees.
(wisent-defcontext compile-grammar
  F LA LAruleno accessing-symbol conflicts consistent default-prec
  derives err-table fderives final-state first-reduction first-shift
  first-state firsts from-state goto-map includes itemset nitemset
  kernel-base kernel-end kernel-items last-reduction last-shift
  last-state lookaheads lookaheadset lookback maxrhs ngotos nitems
  nrules nshifts nstates nsyms ntokens nullable nvars rassoc redset
  reduction-table ritem rlhs rprec rrc-count rrc-total rrhs ruseful
  rcode ruleset rulesetsize shift-symbol shift-table shiftset
  src-count src-total start-table state-table tags this-state to-state
  tokensetsize ;; nb of words req. to hold a bit for each rule
  varsetsize ;; nb of words req. to hold a bit for each variable
  error-token-number start-symbol token-list var-list
  N P V V1 nuseless-nonterminals nuseless-productions
  ptable ;; symbols & characters properties
  )

(defmacro wisent-ISTOKEN (s)
  "Return non-nil if item number S defines a token (terminal).
That is if S < `ntokens'."
  `(< ,s ntokens))

(defmacro wisent-ISVAR(s)
  "Return non-nil if item number S defines a nonterminal.
That is if S >= `ntokens'."
  `(>= ,s ntokens))

(defsubst wisent-tag (s)
  "Return printable form of item number S."
  (wisent-item-to-string (aref tags s)))

;; Symbol and character properties

(defsubst wisent-put (object propname value)
  "Store OBJECT's PROPNAME property with value VALUE.
Use `eq' to locate OBJECT."
  (let ((entry (assq object ptable)))
    (or entry (setq entry (list object) ptable (cons entry ptable)))
    (setcdr entry (plist-put (cdr entry) propname value))))

(defsubst wisent-get (object propname)
  "Return the value of OBJECT's PROPNAME property.
Use `eq' to locate OBJECT."
  (plist-get (cdr (assq object ptable)) propname))

(defsubst wisent-item-number (x)
  "Return the item number of symbol X."
  (wisent-get x 'wisent--item-no))

(defsubst wisent-set-item-number (x n)
  "Set the item number of symbol X to N."
  (wisent-put x 'wisent--item-no n))

(defsubst wisent-assoc (x)
  "Return the associativity of symbol X."
  (wisent-get x 'wisent--assoc))

(defsubst wisent-set-assoc (x a)
  "Set the associativity of symbol X to A."
  (wisent-put x 'wisent--assoc a))

(defsubst wisent-prec (x)
  "Return the precedence level of symbol X."
  (wisent-get x 'wisent--prec))

(defsubst wisent-set-prec (x p)
  "Set the precedence level of symbol X to P."
  (wisent-put x 'wisent--prec p))

;;;; ----------------------------------------------------------
;;;; Type definitions for nondeterministic finite state machine
;;;; ----------------------------------------------------------

;; These type definitions are used to represent a nondeterministic
;; finite state machine that parses the specified grammar.  This
;; information is generated by the function `wisent-generate-states'.

;; Each state of the machine is described by a set of items --
;; particular positions in particular rules -- that are the possible
;; places where parsing could continue when the machine is in this
;; state.  These symbols at these items are the allowable inputs that
;; can follow now.

;; A core represents one state.  States are numbered in the number
;; field.  When `wisent-generate-states' is finished, the starting
;; state is state 0 and `nstates' is the number of states.  (A
;; transition to a state whose state number is `nstates' indicates
;; termination.)  All the cores are chained together and `first-state'
;; points to the first one (state 0).

;; For each state there is a particular symbol which must have been
;; the last thing accepted to reach that state.  It is the
;; accessing-symbol of the core.

;; Each core contains a vector of `nitems' items which are the indices
;; in the `ritems' vector of the items that are selected in this
;; state.

;; The link field is used for chaining buckets that hash states by
;; their itemsets.  This is for recognizing equivalent states and
;; combining them when the states are generated.

;; The two types of transitions are shifts (push the lookahead token
;; and read another) and reductions (combine the last n things on the
;; stack via a rule, replace them with the symbol that the rule
;; derives, and leave the lookahead token alone).  When the states are
;; generated, these transitions are represented in two other lists.

;; Each shifts structure describes the possible shift transitions out
;; of one state, the state whose number is in the number field.  The
;; shifts structures are linked through next and first-shift points to
;; them.  Each contains a vector of numbers of the states that shift
;; transitions can go to.  The accessing-symbol fields of those
;; states' cores say what kind of input leads to them.

;; A shift to state zero should be ignored.  Conflict resolution
;; deletes shifts by changing them to zero.

;; Each reductions structure describes the possible reductions at the
;; state whose number is in the number field.  The data is a list of
;; nreds rules, represented by their rule numbers.  `first-reduction'
;; points to the list of these structures.

;; Conflict resolution can decide that certain tokens in certain
;; states should explicitly be errors (for implementing %nonassoc).
;; For each state, the tokens that are errors for this reason are
;; recorded in an errs structure, which has the state number in its
;; number field.  The rest of the errs structure is full of token
;; numbers.

;; There is at least one shift transition present in state zero.  It
;; leads to a next-to-final state whose accessing-symbol is the
;; grammar's start symbol.  The next-to-final state has one shift to
;; the final state, whose accessing-symbol is zero (end of input).
;; The final state has one shift, which goes to the termination state
;; (whose number is `nstates'-1).
;; The reason for the extra state at the end is to placate the
;; parser's strategy of making all decisions one token ahead of its
;; actions.

(wisent-struct core
  next                                  ; -> core
  link                                  ; -> core
  (number 0)
  (accessing-symbol 0)
  (nitems 0)
  (items [0]))

(wisent-struct shifts
  next                                  ; -> shifts
  (number 0)
  (nshifts 0)
  (shifts [0]))

(wisent-struct reductions
  next                                  ; -> reductions
  (number 0)
  (nreds 0)
  (rules [0]))

(wisent-struct errs
  (nerrs 0)
  (errs [0]))

;;;; --------------------------------------------------------
;;;; Find unreachable terminals, nonterminals and productions
;;;; --------------------------------------------------------

(defun wisent-bits-equal (L R n)
  "Visit L and R and return non-nil if their first N elements are `='.
L and R must be vectors of integers."
  (let* ((i    (1- n))
         (iseq t))
    (while (and iseq (natnump i))
      (setq iseq (= (aref L i) (aref R i))
            i (1- i)))
    iseq))

(defun wisent-nbits (i)
  "Return number of bits set in integer I."
  (let ((count 0))
    (while (not (zerop i))
      ;; i ^= (i & ((unsigned) (-(int) i)))
      (setq i (logxor i (logand i (- i)))
            count (1+ count)))
    count))

(defun wisent-bits-size (S n)
  "In vector S count the total of bits set in first N elements.
S must be a vector of integers."
  (let* ((i (1- n))
         (count 0))
    (while (natnump i)
      (setq count (+ count (wisent-nbits (aref S i)))
            i (1- i)))
    count))

(defun wisent-useful-production (i N0)
  "Return non-nil if production I is in useful set N0."
  (let* ((useful t)
         (r (aref rrhs i))
         n)
    (while (and useful (> (setq n (aref ritem r)) 0))
      (if (wisent-ISVAR n)
          (setq useful (wisent-BITISSET N0 (- n ntokens))))
      (setq r (1+ r)))
    useful))

(defun wisent-useless-nonterminals ()
  "Find out which nonterminals are used."
  (let (Np Ns i n break)
    ;; N is set as built.  Np is set being built this iteration. P is
    ;; set of all productions which have a RHS all in N.
    (setq n  (wisent-WORDSIZE nvars)
          Np (make-vector n 0))

    ;; The set being computed is a set of nonterminals which can
    ;; derive the empty string or strings consisting of all
    ;; terminals. At each iteration a nonterminal is added to the set
    ;; if there is a production with that nonterminal as its LHS for
    ;; which all the nonterminals in its RHS are already in the set.
    ;; Iterate until the set being computed remains unchanged.  Any
    ;; nonterminals not in the set at that point are useless in that
    ;; they will never be used in deriving a sentence of the language.

    ;; This iteration doesn't use any special traversal over the
    ;; productions.  A set is kept of all productions for which all
    ;; the nonterminals in the RHS are in useful.  Only productions
    ;; not in this set are scanned on each iteration.  At the end,
    ;; this set is saved to be used when finding useful productions:
    ;; only productions in this set will appear in the final grammar.

    (while (not break)
      (setq i (1- n))
      (while (natnump i)
        ;; Np[i] = N[i]
        (aset Np i (aref N i))
        (setq i (1- i)))

      (setq i 1)
      (while (<= i nrules)
        (if (not (wisent-BITISSET P i))
            (when (wisent-useful-production i N)
              (wisent-SETBIT Np (- (aref rlhs i) ntokens))
              (wisent-SETBIT P i)))
        (setq i (1+ i)))
      (if (wisent-bits-equal N Np n)
          (setq break t)
        (setq Ns Np
              Np N
              N  Ns)))
    (setq N Np)))

(defun wisent-inaccessable-symbols ()
  "Find out which productions are reachable and which symbols are used."
  ;; Starting with an empty set of productions and a set of symbols
  ;; which only has the start symbol in it, iterate over all
  ;; productions until the set of productions remains unchanged for an
  ;; iteration.  For each production which has a LHS in the set of
  ;; reachable symbols, add the production to the set of reachable
  ;; productions, and add all of the nonterminals in the RHS of the
  ;; production to the set of reachable symbols.

  ;; Consider only the (partially) reduced grammar which has only
  ;; nonterminals in N and productions in P.

  ;; The result is the set P of productions in the reduced grammar,
  ;; and the set V of symbols in the reduced grammar.

  ;; Although this algorithm also computes the set of terminals which
  ;; are reachable, no terminal will be deleted from the grammar. Some
  ;; terminals might not be in the grammar but might be generated by
  ;; semantic routines, and so the user might want them available with
  ;; specified numbers.  (Is this true?)  However, the non reachable
  ;; terminals are printed (if running in verbose mode) so that the
  ;; user can know.
  (let (Vp Vs Pp i tt r n m break)
    (setq n  (wisent-WORDSIZE nsyms)
          m  (wisent-WORDSIZE (1+ nrules))
          Vp (make-vector n 0)
          Pp (make-vector m 0))

    ;; If the start symbol isn't useful, then nothing will be useful.
    (when (wisent-BITISSET N (- start-symbol ntokens))
      (wisent-SETBIT V start-symbol)
      (while (not break)
        (setq i (1- n))
        (while (natnump i)
          (aset Vp i (aref V i))
          (setq i (1- i)))
        (setq i 1)
        (while (<= i nrules)
          (when (and (not (wisent-BITISSET Pp i))
                     (wisent-BITISSET P i)
                     (wisent-BITISSET V (aref rlhs i)))
            (setq r (aref rrhs i))
            (while (natnump (setq tt (aref ritem r)))
              (if (or (wisent-ISTOKEN tt)
                      (wisent-BITISSET N (- tt ntokens)))
                  (wisent-SETBIT Vp tt))
              (setq r (1+ r)))
            (wisent-SETBIT Pp i))
          (setq i (1+ i)))
        (if (wisent-bits-equal V Vp n)
            (setq break t)
          (setq Vs Vp
                Vp V
                V  Vs))))
    (setq V Vp)

    ;; Tokens 0, 1 are internal to Wisent.  Consider them useful.
    (wisent-SETBIT V 0) ;; end-of-input token
    (wisent-SETBIT V 1) ;; error token
    (setq P Pp)

    (setq nuseless-productions  (- nrules (wisent-bits-size P m))
          nuseless-nonterminals nvars
          i ntokens)
    (while (< i nsyms)
      (if (wisent-BITISSET V i)
          (setq nuseless-nonterminals (1- nuseless-nonterminals)))
      (setq i (1+ i)))

    ;; A token that was used in %prec should not be warned about.
    (setq i 1)
    (while (<= i nrules)
      (if (aref rprec i)
          (wisent-SETBIT V1 (aref rprec i)))
      (setq i (1+ i)))
    ))

(defun wisent-reduce-grammar-tables ()
  "Disable useless productions."
  (if (> nuseless-productions 0)
      (let ((pn 1))
        (while (<= pn nrules)
          (aset ruseful pn (wisent-BITISSET P pn))
          (setq pn (1+ pn))))))

(defun wisent-nonterminals-reduce ()
  "Remove useless nonterminals."
  (let (i n r item nontermmap tags-sorted)
    ;; Map the nonterminals to their new index: useful first, useless
    ;; afterwards.  Kept for later report.
    (setq nontermmap (make-vector nvars 0)
          n ntokens
          i ntokens)
    (while (< i nsyms)
      (when (wisent-BITISSET V i)
        (aset nontermmap (- i ntokens) n)
        (setq n (1+ n)))
      (setq i (1+ i)))
    (setq i ntokens)
    (while (< i nsyms)
      (unless (wisent-BITISSET V i)
        (aset nontermmap (- i ntokens) n)
        (setq n (1+ n)))
      (setq i (1+ i)))
    ;; Shuffle elements of tables indexed by symbol number
    (setq tags-sorted (make-vector nvars nil)
          i ntokens)
    (while (< i nsyms)
      (setq n (aref nontermmap (- i ntokens)))
      (aset tags-sorted (- n ntokens) (aref tags i))
      (setq i (1+ i)))
    (setq i ntokens)
    (while (< i nsyms)
      (aset tags i (aref tags-sorted (- i ntokens)))
      (setq i (1+ i)))
    ;; Replace all symbol numbers in valid data structures.
    (setq i 1)
    (while (<= i nrules)
      (aset rlhs i (aref nontermmap (- (aref rlhs i) ntokens)))
      (setq i (1+ i)))
    (setq r 0)
    (while (setq item (aref ritem r))
      (if (wisent-ISVAR item)
          (aset ritem r (aref nontermmap (- item ntokens))))
      (setq r (1+ r)))
    (setq start-symbol (aref nontermmap (- start-symbol ntokens))
          nsyms (- nsyms nuseless-nonterminals)
          nvars (- nvars nuseless-nonterminals))
    ))

(defun wisent-total-useless ()
  "Report number of useless nonterminals and productions."
  (let* ((src (wisent-source))
         (src (if src (concat " in " src) ""))
         (msg (format "Grammar%s contains" src)))
    (if (> nuseless-nonterminals 0)
        (setq msg (format "%s %d useless nonterminal%s"
                          msg nuseless-nonterminals
                          (if (> nuseless-nonterminals 0) "s" ""))))
    (if (and (> nuseless-nonterminals 0) (> nuseless-productions 0))
        (setq msg (format "%s and" msg)))
    (if (> nuseless-productions 0)
        (setq msg (format "%s %d useless rule%s"
                          msg nuseless-productions
                          (if (> nuseless-productions 0) "s" ""))))
    (message msg)))

(defun wisent-reduce-grammar ()
  "Find unreachable terminals, nonterminals and productions."
  ;; Allocate the global sets used to compute the reduced grammar
  (setq N  (make-vector (wisent-WORDSIZE nvars) 0)
        P  (make-vector (wisent-WORDSIZE (1+ nrules)) 0)
        V  (make-vector (wisent-WORDSIZE nsyms) 0)
        V1 (make-vector (wisent-WORDSIZE nsyms) 0)
        nuseless-nonterminals 0
        nuseless-productions  0)

  (wisent-useless-nonterminals)
  (wisent-inaccessable-symbols)

  (when (> (+ nuseless-nonterminals nuseless-productions) 0)
    (wisent-total-useless)
    (or (wisent-BITISSET N (- start-symbol ntokens))
        (error "Start symbol `%s' does not derive any sentence"
               (wisent-tag start-symbol)))
    (wisent-reduce-grammar-tables)
    (if (> nuseless-nonterminals 0)
        (wisent-nonterminals-reduce))))

(defun wisent-print-useless ()
  "Output the detailed results of the reductions."
  (let (i b r)
    (when (> nuseless-nonterminals 0)
      ;; Useless nonterminals have been moved after useful ones.
      (wisent-log "\n\nUseless nonterminals:\n\n")
      (setq i 0)
      (while (< i nuseless-nonterminals)
        (wisent-log "   %s\n" (wisent-tag (+ nsyms i)))
        (setq i (1+ i))))
    (setq b nil
          i 0)
    (while (< i ntokens)
      (unless (or (wisent-BITISSET V i) (wisent-BITISSET V1 i))
        (or b
            (wisent-log "\n\nTerminals which are not used:\n\n"))
        (setq b t)
        (wisent-log "   %s\n" (wisent-tag i)))
      (setq i (1+ i)))
    (when (> nuseless-productions 0)
      (wisent-log "\n\nUseless rules:\n\n")
      (setq i 1)
      (while (<= i nrules)
        (unless (aref ruseful i)
          (wisent-log "#%s  " (wisent-pad-string (format "%d" i) 4))
          (wisent-log "%s:" (wisent-tag (aref rlhs i)))
          (setq r (aref rrhs i))
          (while (natnump (aref ritem r))
            (wisent-log " %s" (wisent-tag (aref ritem r)))
            (setq r (1+ r)))
          (wisent-log ";\n"))
        (setq i (1+ i))))
    (if (or b (> nuseless-nonterminals 0) (> nuseless-productions 0))
        (wisent-log "\n\n"))
    ))

;;;; -----------------------------
;;;; Match rules with nonterminals
;;;; -----------------------------

(defun wisent-set-derives ()
  "Find, for each variable (nonterminal), which rules can derive it.
It sets up the value of DERIVES so that DERIVES[i - NTOKENS] points to
a list of rule numbers, terminated with -1."
  (let (i lhs p q dset delts)
    (setq dset (make-vector nvars nil)
          delts (make-vector (1+ nrules) 0))
    (setq p 0 ;; p = delts
          i nrules)
    (while (> i 0)
      (when (aref ruseful i)
        (setq lhs (aref rlhs i))
        ;; p->next = dset[lhs];
        ;; p->value = i;
        (aset delts p (cons i (aref dset (- lhs ntokens)))) ;; (value . next)
        (aset dset (- lhs ntokens) p) ;; dset[lhs] = p
        (setq p (1+ p)) ;; p++
        )
      (setq i (1- i)))

    (setq derives (make-vector nvars nil)
          i       ntokens)

    (while (< i nsyms)
      (setq q nil
            p (aref dset (- i ntokens))) ;; p = dset[i]

      (while p
        (setq p (aref delts p)
              q (cons (car p) q) ;;q++ = p->value
              p (cdr p))) ;; p = p->next
      (setq q (nreverse (cons -1 q))) ;; *q++ = -1
      (aset derives (- i ntokens) q) ;; derives[i] = q
      (setq i (1+ i)))
    ))

;;;; --------------------------------------------------------
;;;; Find which nonterminals can expand into the null string.
;;;; --------------------------------------------------------

(defun wisent-print-nullable ()
  "Print NULLABLE."
  (let (i)
    (wisent-log "NULLABLE\n")
    (setq i ntokens)
    (while (< i nsyms)
      (wisent-log "\t%s: %s\n" (wisent-tag i)
                  (if (aref nullable (- i ntokens))
                      "yes" : "no"))
      (setq i (1+ i)))
    (wisent-log "\n\n")))

(defun wisent-set-nullable ()
  "Set up NULLABLE.
A vector saying which nonterminals can expand into the null string.
NULLABLE[i - NTOKENS] is nil if symbol I can do so."
  (let (ruleno s1 s2 p r squeue rcount rsets relts item any-tokens)
    (setq squeue (make-vector nvars 0)
          rcount (make-vector (1+ nrules) 0)
          rsets  (make-vector nvars nil) ;; - ntokens
          relts  (make-vector (+ nitems nvars 1) nil)
          nullable (make-vector nvars nil)) ;; - ntokens
    (setq s1 0 s2 0 ;; s1 = s2 = squeue
          p 0 ;; p = relts
          ruleno 1)
    (while (<= ruleno nrules)
      (when (aref ruseful ruleno)
        (if (> (aref ritem (aref rrhs ruleno)) 0)
            (progn
              ;; This rule has a non empty RHS.
              (setq any-tokens nil
                    r (aref rrhs ruleno))
              (while (> (aref ritem r) 0)
                (if (wisent-ISTOKEN (aref ritem r))
                    (setq any-tokens t))
                (setq r (1+ r)))

              ;; This rule has only nonterminals: schedule it for the
              ;; second pass.
              (unless any-tokens
                (setq r (aref rrhs ruleno))
                (while (> (setq item (aref ritem r)) 0)
                  (aset rcount ruleno (1+ (aref rcount ruleno)))
                  ;; p->next = rsets[item];
                  ;; p->value = ruleno;
                  (aset relts p (cons ruleno (aref rsets (- item ntokens))))
                  ;; rsets[item] = p;
                  (aset rsets (- item ntokens) p)
                  (setq p (1+ p)
                        r (1+ r)))))
          ;; This rule has an empty RHS.
          ;; assert (ritem[rrhs[ruleno]] == -ruleno)
          (when (and (aref ruseful ruleno)
                     (setq item (aref rlhs ruleno))
                     (not (aref nullable (- item ntokens))))
            (aset nullable (- item ntokens) t)
            (aset squeue s2 item)
            (setq s2 (1+ s2)))
          )
        )
      (setq ruleno (1+ ruleno)))

    (while (< s1 s2)
      ;; p = rsets[*s1++]
      (setq p (aref rsets (- (aref squeue s1) ntokens))
            s1 (1+ s1))
      (while p
        (setq p (aref relts p)
              ruleno (car p)
              p (cdr p)) ;; p = p->next
        ;; if (--rcount[ruleno] == 0)
        (when (zerop (aset rcount ruleno (1- (aref rcount ruleno))))
          (setq item (aref rlhs ruleno))
          (aset nullable (- item ntokens) t)
          (aset squeue s2 item)
          (setq s2 (1+ s2)))))

    (if wisent-debug-flag
        (wisent-print-nullable))
    ))

;;;; -----------
;;;; Subroutines
;;;; -----------

(defun wisent-print-fderives ()
  "Print FDERIVES."
  (let (i j rp)
    (wisent-log "\n\n\nFDERIVES\n")
    (setq i ntokens)
    (while (< i nsyms)
      (wisent-log "\n\n%s derives\n\n" (wisent-tag i))
      (setq rp (aref fderives (- i ntokens))
            j  0)
      (while (<= j nrules)
        (if (wisent-BITISSET rp j)
            (wisent-log "   %d\n" j))
        (setq j (1+ j)))
      (setq i (1+ i)))))

(defun wisent-set-fderives ()
  "Set up FDERIVES.
An NVARS by NRULES matrix of bits indicating which rules can help
derive the beginning of the data for each nonterminal.  For example,
if symbol 5 can be derived as the sequence of symbols 8 3 20, and one
of the rules for deriving symbol 8 is rule 4, then the
\[5 - NTOKENS, 4] bit in FDERIVES is set."
  (let (i j k)
    (setq fderives (make-vector nvars nil))
    (setq i 0)
    (while (< i nvars)
      (aset fderives i (make-vector rulesetsize 0))
      (setq i (1+ i)))

    (wisent-set-firsts)

    (setq i ntokens)
    (while (< i nsyms)
      (setq j ntokens)
      (while (< j nsyms)
        ;; if (BITISSET (FIRSTS (i), j - ntokens))
        (when (wisent-BITISSET (aref firsts (- i ntokens)) (- j ntokens))
          (setq k (aref derives (- j ntokens)))
          (while (> (car k) 0) ;; derives[j][k] > 0
            ;; SETBIT (FDERIVES (i), derives[j][k]);
            (wisent-SETBIT (aref fderives (- i ntokens)) (car k))
            (setq k (cdr k))))
        (setq j (1+ j)))
      (setq i (1+ i)))

    (if wisent-debug-flag
        (wisent-print-fderives))
    ))

(defun wisent-print-firsts ()
  "Print FIRSTS."
  (let (i j v)
    (wisent-log "\n\n\nFIRSTS\n\n")
    (setq i ntokens)
    (while (< i nsyms)
      (wisent-log "\n\n%s firsts\n\n" (wisent-tag i))
      (setq v (aref firsts (- i ntokens))
            j 0)
      (while (< j nvars)
        (if (wisent-BITISSET v j)
            (wisent-log "\t\t%d (%s)\n"
                        (+ j ntokens) (wisent-tag (+ j ntokens))))
        (setq j (1+ j)))
      (setq i (1+ i)))))

(defun wisent-TC (R n)
  "Transitive closure.
Given R an N by N matrix of bits, modify its contents to be the
transitive closure of what was given."
  (let (i j k)
    ;; R (J, I) && R (I, K) => R (J, K).
    ;; I *must* be the outer loop.
    (setq i 0)
    (while (< i n)
      (setq j 0)
      (while (< j n)
        (when (wisent-BITISSET (aref R j) i)
          (setq k 0)
          (while (< k n)
            (if (wisent-BITISSET (aref R i) k)
                (wisent-SETBIT (aref R j) k))
            (setq k (1+ k))))
        (setq j (1+ j)))
      (setq i (1+ i)))))

(defun wisent-RTC (R n)
  "Reflexive Transitive Closure.
Same as `wisent-TC' and then set all the bits on the diagonal of R, an
N by N matrix of bits."
  (let (i)
    (wisent-TC R n)
    (setq i 0)
    (while (< i n)
      (wisent-SETBIT (aref R i) i)
      (setq i (1+ i)))))

(defun wisent-set-firsts ()
  "Set up FIRSTS.
An NVARS by NVARS bit matrix indicating which items can represent the
beginning of the input corresponding to which other items.  For
example, if some rule expands symbol 5 into the sequence of symbols 8
3 20, the symbol 8 can be the beginning of the data for symbol 5, so
the bit [8 - NTOKENS, 5 - NTOKENS] in FIRSTS is set."
  (let (row symbol sp rowsize i)
    (setq rowsize (wisent-WORDSIZE nvars)
          varsetsize rowsize
          firsts (make-vector nvars nil)
          i 0)
    (while (< i nvars)
      (aset firsts i (make-vector rowsize 0))
      (setq i (1+ i)))

    (setq row 0 ;; row = firsts
          i ntokens)
    (while (< i nsyms)
      (setq sp (aref derives (- i ntokens)))
      (while (>= (car sp) 0)
        (setq symbol (aref ritem (aref rrhs (car sp)))
              sp (cdr sp))
        (when (wisent-ISVAR symbol)
          (setq symbol (- symbol ntokens))
          (wisent-SETBIT (aref firsts row) symbol)
          ))
      (setq row (1+ row)
            i   (1+ i)))

    (wisent-RTC firsts nvars)

    (if wisent-debug-flag
        (wisent-print-firsts))
    ))

(defun wisent-initialize-closure (n)
  "Allocate the ITEMSET and RULESET vectors.
And precompute useful data so that `wisent-closure' can be called.
N is the number of elements to allocate for ITEMSET."
  (setq itemset (make-vector n 0)
        rulesetsize (wisent-WORDSIZE (1+ nrules))
        ruleset (make-vector rulesetsize 0))

  (wisent-set-fderives))

(defun wisent-print-closure ()
  "Print ITEMSET."
  (let (i)
    (wisent-log "\n\nclosure n = %d\n\n" nitemset)
    (setq i 0) ;; isp = itemset
    (while (< i nitemset)
      (wisent-log "   %d\n" (aref itemset i))
      (setq i (1+ i)))))

(defun wisent-closure (core n)
  "Set up RULESET and ITEMSET for the transitions out of CORE state.
Given a vector of item numbers items, of length N, set up RULESET and
ITEMSET to indicate what rules could be run and which items could be
accepted when those items are the active ones.

RULESET contains a bit for each rule.  `wisent-closure' sets the bits
for all rules which could potentially describe the next input to be
read.

ITEMSET is a vector of item numbers; NITEMSET is the number of items
in ITEMSET.  `wisent-closure' places there the indices of all items
which represent units of input that could arrive next."
  (let (c r v symbol ruleno itemno)
    (if (zerop n)
        (progn
          (setq r 0
                v (aref fderives (- start-symbol ntokens)))
          (while (< r rulesetsize)
            ;; ruleset[r] = FDERIVES (start-symbol)[r];
            (aset ruleset r (aref v r))
            (setq r (1+ r)))
          )
      (fillarray ruleset 0)
      (setq c 0)
      (while (< c n)
        (setq symbol (aref ritem (aref core c)))
        (when (wisent-ISVAR symbol)
          (setq r 0
                v (aref fderives (- symbol ntokens)))
          (while (< r rulesetsize)
            ;; ruleset[r] |= FDERIVES (ritem[core[c]])[r];
            (aset ruleset r (logior (aref ruleset r) (aref v r)))
            (setq r (1+ r))))
        (setq c (1+ c)))
      )
    (setq nitemset 0
          c 0
          ruleno 0
          r (* rulesetsize wisent-BITS-PER-WORD))
    (while (< ruleno r)
      (when (wisent-BITISSET ruleset ruleno)
        (setq itemno (aref rrhs ruleno))
        (while (and (< c n) (< (aref core c) itemno))
          (aset itemset nitemset (aref core c))
          (setq nitemset (1+ nitemset)
                c (1+ c)))
        (aset itemset nitemset itemno)
        (setq nitemset (1+ nitemset)))
      (setq ruleno (1+ ruleno)))

    (while (< c n)
      (aset itemset nitemset (aref core c))
      (setq nitemset (1+ nitemset)
            c (1+ c)))

    (if wisent-debug-flag
        (wisent-print-closure))
    ))

;;;; --------------------------------------------------
;;;; Generate the nondeterministic finite state machine
;;;; --------------------------------------------------

(defun wisent-allocate-itemsets ()
  "Allocate storage for itemsets."
  (let (symbol i count symbol-count)
    ;; Count the number of occurrences of all the symbols in RITEMS.
    ;; Note that useless productions (hence useless nonterminals) are
    ;; browsed too, hence we need to allocate room for _all_ the
    ;; symbols.
    (setq count 0
          symbol-count (make-vector (+ nsyms nuseless-nonterminals) 0)
          i 0)
    (while (setq symbol (aref ritem i))
      (when (> symbol 0)
        (setq count (1+ count))
        (aset symbol-count symbol (1+ (aref symbol-count symbol))))
      (setq i (1+ i)))
    ;; See comments before `wisent-new-itemsets'.  All the vectors of
    ;; items live inside kernel-items.  The number of active items
    ;; after some symbol cannot be more than the number of times that
    ;; symbol appears as an item, which is symbol-count[symbol].  We
    ;; allocate that much space for each symbol.
    (setq kernel-base (make-vector nsyms nil)
          kernel-items (make-vector count 0)
          count 0
          i 0)
    (while (< i nsyms)
      (aset kernel-base i count)
      (setq count (+ count (aref symbol-count i))
            i (1+ i)))
    (setq shift-symbol symbol-count
          kernel-end (make-vector nsyms nil))
    ))

(defun wisent-allocate-storage ()
  "Allocate storage for the state machine."
  (wisent-allocate-itemsets)
  (setq shiftset (make-vector nsyms 0)
        redset (make-vector (1+ nrules) 0)
        state-table (make-vector wisent-state-table-size nil)))

(defun wisent-new-itemsets ()
  "Find which symbols can be shifted in the current state.
And for each one record which items would be active after that shift.
Uses the contents of ITEMSET.  SHIFT-SYMBOL is set to a vector of the
symbols that can be shifted.  For each symbol in the grammar,
KERNEL-BASE[symbol] points to a vector of item numbers activated if
that symbol is shifted, and KERNEL-END[symbol] points after the end of
that vector."
  (let (i shiftcount isp ksp symbol)
    (fillarray kernel-end nil)
    (setq shiftcount 0
          isp 0)
    (while (< isp nitemset)
      (setq i (aref itemset isp)
            isp (1+ isp)
            symbol (aref ritem i))
      (when (> symbol 0)
        (setq ksp (aref kernel-end symbol))
        (when (not ksp)
          ;; shift-symbol[shiftcount++] = symbol;
          (aset shift-symbol shiftcount symbol)
          (setq shiftcount (1+ shiftcount)
                ksp (aref kernel-base symbol)))
        ;; *ksp++ = i + 1;
        (aset kernel-items ksp (1+ i))
        (setq ksp (1+ ksp))
        (aset kernel-end symbol ksp)))
    (setq nshifts shiftcount)))

(defun wisent-new-state (symbol)
  "Create a new state for those items, if necessary.
SYMBOL is the core accessing-symbol.
Subroutine of `wisent-get-state'."
  (let (n p isp1 isp2 iend items)
    (setq isp1  (aref kernel-base symbol)
          iend  (aref kernel-end symbol)
          n     (- iend isp1)
          p     (make-core)
          items (make-vector n 0))
    (set-core-accessing-symbol p symbol)
    (set-core-number p nstates)
    (set-core-nitems p n)
    (set-core-items  p items)
    (setq isp2 0) ;; isp2 = p->items
    (while (< isp1 iend)
      ;; *isp2++ = *isp1++;
      (aset items isp2 (aref kernel-items isp1))
      (setq isp1 (1+ isp1)
            isp2 (1+ isp2)))
    (set-core-next last-state p)
    (setq last-state p
          nstates (1+ nstates))
    p))

(defun wisent-get-state (symbol)
  "Find the state we would get to by shifting SYMBOL.
Return the state number for the state we would get to (from the
current state) by shifting SYMBOL.  Create a new state if no
equivalent one exists already.  Used by `wisent-append-states'."
  (let (key isp1 isp2 iend sp sp2 found n)
    (setq isp1 (aref kernel-base symbol)
          iend (aref kernel-end symbol)
          n    (- iend isp1)
          key  0)
    ;; Add up the target state's active item numbers to get a hash key
    (while (< isp1 iend)
      (setq key (+ key (aref kernel-items isp1))
            isp1 (1+ isp1)))
    (setq key (% key wisent-state-table-size)
          sp (aref state-table key))
    (if sp
        (progn
          (setq found nil)
          (while (not found)
            (when (= (core-nitems sp) n)
              (setq found t
                    isp1 (aref kernel-base symbol)
                    ;; isp2 = sp->items;
                    sp2  (core-items sp)
                    isp2 0)

              (while (and found (< isp1 iend))
                ;; if (*isp1++ != *isp2++)
                (if (not (= (aref kernel-items isp1)
                            (aref sp2 isp2)))
                    (setq found nil))
                (setq isp1 (1+ isp1)
                      isp2 (1+ isp2))))
            (if (not found)
                (if (core-link sp)
                    (setq sp (core-link sp))
                  ;; sp = sp->link = new-state(symbol)
                  (setq sp (set-core-link sp (wisent-new-state symbol))
                        found t)))))
      ;; bucket is empty
      ;; state-table[key] = sp = new-state(symbol)
      (setq sp (wisent-new-state symbol))
      (aset state-table key sp))
    ;; return (sp->number);
    (core-number sp)))

(defun wisent-append-states ()
  "Find or create the core structures for states.
Use the information computed by `wisent-new-itemsets' to find the
state numbers reached by each shift transition from the current state.
SHIFTSET is set up as a vector of state numbers of those states."
  (let (i j symbol)
    ;; First sort shift-symbol into increasing order
    (setq i 1)
    (while (< i nshifts)
      (setq symbol (aref shift-symbol i)
            j i)
      (while (and (> j 0) (> (aref shift-symbol (1- j)) symbol))
        (aset shift-symbol j (aref shift-symbol (1- j)))
        (setq j (1- j)))
      (aset shift-symbol j symbol)
      (setq i (1+ i)))
    (setq i 0)
    (while (< i nshifts)
      (setq symbol (aref shift-symbol i))
      (aset shiftset i (wisent-get-state symbol))
      (setq i (1+ i)))
    ))

(defun wisent-initialize-states ()
  "Initialize states."
  (let ((p (make-core)))
    (setq first-state p
          last-state  p
          this-state  p
          nstates     1)))

(defun wisent-save-shifts ()
  "Save the NSHIFTS of SHIFTSET into the current linked list."
  (let (p i shifts)
    (setq p      (make-shifts)
          shifts (make-vector nshifts 0)
          i 0)
    (set-shifts-number p (core-number this-state))
    (set-shifts-nshifts p nshifts)
    (set-shifts-shifts  p shifts)
    (while (< i nshifts)
      ;; (p->shifts)[i] = shiftset[i];
      (aset shifts i (aref shiftset i))
      (setq i (1+ i)))

    (if last-shift
        (set-shifts-next last-shift p)
      (setq first-shift p))
    (setq last-shift p)))

(defun wisent-insert-start-shift ()
  "Create the next-to-final state.
That is the state to which a shift has already been made in the
initial state.  Subroutine of `wisent-augment-automaton'."
  (let (statep sp)
    (setq statep (make-core))
    (set-core-number statep nstates)
    (set-core-accessing-symbol statep start-symbol)
    (set-core-next last-state statep)
    (setq last-state statep)
    ;; Make a shift from this state to (what will be) the final state.
    (setq sp (make-shifts))
    (set-shifts-number sp nstates)
    (setq nstates (1+ nstates))
    (set-shifts-nshifts sp 1)
    (set-shifts-shifts sp (vector nstates))
    (set-shifts-next last-shift sp)
    (setq last-shift sp)))

(defun wisent-augment-automaton ()
  "Set up initial and final states as parser wants them.
Make sure that the initial state has a shift that accepts the
grammar's start symbol and goes to the next-to-final state, which has
a shift going to the final state, which has a shift to the termination
state.  Create such states and shifts if they don't happen to exist
already."
  (let (i k statep sp sp2 sp1 shifts)
    (setq sp first-shift)
    (if sp
        (progn
          (if (zerop (shifts-number sp))
              (progn
                (setq k (shifts-nshifts sp)
                      statep (core-next first-state))
                ;; The states reached by shifts from first-state are
                ;; numbered 1...K.  Look for one reached by
                ;; START-SYMBOL.
                (while (and (< (core-accessing-symbol statep) start-symbol)
                            (< (core-number statep) k))
                  (setq statep (core-next statep)))
                (if (= (core-accessing-symbol statep) start-symbol)
                    (progn
                      ;; We already have a next-to-final state.  Make
                      ;; sure it has a shift to what will be the final
                      ;; state.
                      (setq k (core-number statep))
                      (while (and sp (< (shifts-number sp) k))
                        (setq sp1 sp
                              sp (shifts-next sp)))
                      (if (and sp (= (shifts-number sp) k))
                          (progn
                            (setq i (shifts-nshifts sp)
                                  sp2 (make-shifts)
                                  shifts (make-vector (1+ i) 0))
                            (set-shifts-number sp2 k)
                            (set-shifts-nshifts sp2 (1+ i))
                            (set-shifts-shifts sp2 shifts)
                            (aset shifts 0 nstates)
                            (while (> i 0)
                              ;; sp2->shifts[i] = sp->shifts[i - 1];
                              (aset shifts i (aref (shifts-shifts sp) (1- i)))
                              (setq i (1- i)))
                            ;; Patch sp2 into the chain of shifts in
                            ;; place of sp, following sp1.
                            (set-shifts-next sp2 (shifts-next sp))
                            (set-shifts-next sp1 sp2)
                            (if (eq sp last-shift)
                                (setq last-shift sp2))
                            )
                        (setq sp2 (make-shifts))
                        (set-shifts-number sp2 k)
                        (set-shifts-nshifts sp2 1)
                        (set-shifts-shifts sp2 (vector nstates))
                        ;; Patch sp2 into the chain of shifts between
                        ;; sp1 and sp.
                        (set-shifts-next sp2 sp)
                        (set-shifts-next sp1 sp2)
                        (if (not sp)
                            (setq last-shift sp2))
                        )
                      )
                  ;; There is no next-to-final state as yet.
                  ;; Add one more shift in FIRST-SHIFT, going to the
                  ;; next-to-final state (yet to be made).
                  (setq sp first-shift
                        sp2 (make-shifts)
                        i   (shifts-nshifts sp)
                        shifts (make-vector (1+ i) 0))
                  (set-shifts-nshifts sp2 (1+ i))
                  (set-shifts-shifts sp2 shifts)
                  ;; Stick this shift into the vector at the proper place.
                  (setq statep (core-next first-state)
                        k 0
                        i 0)
                  (while (< i (shifts-nshifts sp))
                    (when (and (> (core-accessing-symbol statep) start-symbol)
                               (= i k))
                      (aset shifts k nstates)
                      (setq k (1+ k)))
                    (aset shifts k (aref (shifts-shifts sp) i))
                    (setq statep (core-next statep))
                    (setq i (1+ i)
                          k (1+ k)))
                  (when (= i k)
                    (aset shifts k nstates)
                    (setq k (1+ k)))
                  ;; Patch sp2 into the chain of shifts in place of
                  ;; sp, at the beginning.
                  (set-shifts-next sp2 (shifts-next sp))
                  (setq first-shift sp2)
                  (if (eq last-shift sp)
                      (setq last-shift sp2))
                  ;; Create the next-to-final state, with shift to
                  ;; what will be the final state.
                  (wisent-insert-start-shift)))
            ;; The initial state didn't even have any shifts.  Give it
            ;; one shift, to the next-to-final state.
            (setq sp (make-shifts))
            (set-shifts-nshifts sp 1)
            (set-shifts-shifts sp (vector nstates))
            ;; Patch sp into the chain of shifts at the beginning.
            (set-shifts-next sp first-shift)
            (setq first-shift sp)
            ;; Create the next-to-final state, with shift to what will
            ;; be the final state.
            (wisent-insert-start-shift)))
      ;; There are no shifts for any state.  Make one shift, from the
      ;; initial state to the next-to-final state.
      (setq sp (make-shifts))
      (set-shifts-nshifts sp 1)
      (set-shifts-shifts sp (vector nstates))
      ;; Initialize the chain of shifts with sp.
      (setq first-shift sp
            last-shift sp)
      ;; Create the next-to-final state, with shift to what will be
      ;; the final state.
      (wisent-insert-start-shift))
    ;; Make the final state--the one that follows a shift from the
    ;; next-to-final state.  The symbol for that shift is 0
    ;; (end-of-file).
    (setq statep (make-core))
    (set-core-number statep nstates)
    (set-core-next last-state statep)
    (setq last-state statep)
    ;; Make the shift from the final state to the termination state.
    (setq sp (make-shifts))
    (set-shifts-number sp nstates)
    (setq nstates (1+ nstates))
    (set-shifts-nshifts sp 1)
    (set-shifts-shifts sp (vector nstates))
    (set-shifts-next last-shift sp)
    (setq last-shift sp)
    ;; Note that the variable FINAL-STATE refers to what we sometimes
    ;; call the termination state.
    (setq final-state nstates)
    ;; Make the termination state.
    (setq statep (make-core))
    (set-core-number statep nstates)
    (setq nstates (1+ nstates))
    (set-core-next last-state statep)
    (setq last-state statep)))

(defun wisent-save-reductions ()
  "Make a reductions structure.
Find which rules can be used for reduction transitions from the
current state and make a reductions structure for the state to record
their rule numbers."
  (let (i item count p rules)
    ;; Find and count the active items that represent ends of rules.
    (setq count 0
          i 0)
    (while (< i nitemset)
      (setq item (aref ritem (aref itemset i)))
      (when (< item 0)
        (aset redset count (- item))
        (setq count (1+ count)))
      (setq i (1+ i)))
    ;; Make a reductions structure and copy the data into it.
    (when (> count 0)
      (setq p (make-reductions)
            rules (make-vector count 0))
      (set-reductions-number p (core-number this-state))
      (set-reductions-nreds  p count)
      (set-reductions-rules  p rules)
      (setq i 0)
      (while (< i count)
        ;; (p->rules)[i] = redset[i]
        (aset rules i (aref redset i))
        (setq i (1+ i)))
      (if last-reduction
          (set-reductions-next last-reduction p)
        (setq first-reduction p))
      (setq last-reduction p))))

(defun wisent-generate-states ()
  "Compute the nondeterministic finite state machine from the grammar."
  (wisent-allocate-storage)
  (wisent-initialize-closure nitems)
  (wisent-initialize-states)
  (while this-state
    ;; Set up RULESET and ITEMSET for the transitions out of this
    ;; state.  RULESET gets a 1 bit for each rule that could reduce
    ;; now.  ITEMSET gets a vector of all the items that could be
    ;; accepted next.
    (wisent-closure (core-items this-state) (core-nitems this-state))
    ;; Record the reductions allowed out of this state.
    (wisent-save-reductions)
    ;; Find the itemsets of the states that shifts can reach.
    (wisent-new-itemsets)
    ;; Find or create the core structures for those states.
    (wisent-append-states)
    ;; Create the shifts structures for the shifts to those states,
    ;; now that the state numbers transitioning to are known.
    (if (> nshifts 0)
        (wisent-save-shifts))
    ;; States are queued when they are created; process them all.
    (setq this-state (core-next this-state)))
  ;; Set up initial and final states as parser wants them.
  (wisent-augment-automaton))

;;;; ---------------------------
;;;; Compute look-ahead criteria
;;;; ---------------------------

;; Compute how to make the finite state machine deterministic; find
;; which rules need lookahead in each state, and which lookahead
;; tokens they accept.

;; `wisent-lalr', the entry point, builds these data structures:

;; GOTO-MAP, FROM-STATE and TO-STATE record each shift transition
;; which accepts a variable (a nonterminal).  NGOTOS is the number of
;; such transitions.
;; FROM-STATE[t] is the state number which a transition leads from and
;; TO-STATE[t] is the state number it leads to.
;; All the transitions that accept a particular variable are grouped
;; together and GOTO-MAP[i - NTOKENS] is the index in FROM-STATE and
;; TO-STATE of the first of them.

;; CONSISTENT[s] is non-nil if no lookahead is needed to decide what
;; to do in state s.

;; LARULENO is a vector which records the rules that need lookahead in
;; various states.  The elements of LARULENO that apply to state s are
;; those from LOOKAHEADS[s] through LOOKAHEADS[s+1]-1.  Each element
;; of LARULENO is a rule number.

;; If LR is the length of LARULENO, then a number from 0 to LR-1 can
;; specify both a rule and a state where the rule might be applied.
;; LA is a LR by NTOKENS matrix of bits.
;; LA[l, i] is 1 if the rule LARULENO[l] is applicable in the
;; appropriate state when the next token is symbol i.
;; If LA[l, i] and LA[l, j] are both 1 for i != j, it is a conflict.

(wisent-defcontext digraph
  INDEX R VERTICES
  infinity top)

(defun wisent-traverse (i)
  "Traverse I."
  (let (j k height Ri Fi break)
    (setq top (1+ top)
          height top)
    (aset VERTICES top i) ;; VERTICES[++top] = i
    (aset INDEX i top) ;; INDEX[i] = height = top

    (setq Ri (aref R i))
    (when Ri
      (setq j 0)
      (while (>= (aref Ri j) 0)
        (if (zerop (aref INDEX (aref Ri j)))
            (wisent-traverse (aref Ri j)))
        ;; if (INDEX[i] > INDEX[R[i][j]])
        (if (> (aref INDEX i) (aref INDEX (aref Ri j)))
            ;; INDEX[i] = INDEX[R[i][j]];
            (aset INDEX i (aref INDEX (aref Ri j))))
        (setq Fi (aref F i)
              k 0)
        (while (< k tokensetsize)
          ;; F (i)[k] |= F (R[i][j])[k];
          (aset Fi k (logior (aref Fi k)
                             (aref (aref F (aref Ri j)) k)))
           (setq k (1+ k)))
        (setq j (1+ j))))

    (when (= (aref INDEX i) height)
      (setq break nil)
      (while (not break)
        (setq j (aref VERTICES top) ;; j = VERTICES[top--]
              top (1- top))
        (aset INDEX j infinity)
        (if (= i j)
            (setq break t)
          (setq k 0)
          (while (< k tokensetsize)
            ;; F (j)[k] = F (i)[k];
            (aset (aref F j) k (aref (aref F i) k))
            (setq k (1+ k))))))
    ))

(defun wisent-digraph (relation)
  "Digraph RELATION."
  (wisent-with-context digraph
    (setq infinity (+ ngotos 2)
          INDEX    (make-vector (1+ ngotos) 0)
          VERTICES (make-vector (1+ ngotos) 0)
          top      0
          R        relation)
    (let ((i 0))
      (while (< i ngotos)
        (if (and (= (aref INDEX i) 0) (aref R i))
            (wisent-traverse i))
        (setq i (1+ i))))))

(defun wisent-set-state-table ()
  "Build state table."
  (let (sp)
    (setq state-table (make-vector nstates nil)
          sp first-state)
    (while sp
      (aset state-table (core-number sp) sp)
      (setq sp (core-next sp)))))

(defun wisent-set-accessing-symbol ()
  "Build accessing symbol table."
  (let (sp)
    (setq accessing-symbol (make-vector nstates 0)
          sp first-state)
    (while sp
      (aset accessing-symbol (core-number sp) (core-accessing-symbol sp))
      (setq sp (core-next sp)))))

(defun wisent-set-shift-table ()
  "Build shift table."
  (let (sp)
    (setq shift-table (make-vector nstates nil)
          sp first-shift)
    (while sp
      (aset shift-table (shifts-number sp) sp)
      (setq sp (shifts-next sp)))))

(defun wisent-set-reduction-table ()
  "Build reduction table."
  (let (rp)
    (setq reduction-table (make-vector nstates nil)
          rp first-reduction)
    (while rp
      (aset reduction-table (reductions-number rp) rp)
      (setq rp (reductions-next rp)))))

(defun wisent-set-maxrhs ()
  "Setup MAXRHS length."
  (let (i len max)
    (setq len 0
          max 0
          i   0)
    (while (aref ritem i)
      (if (> (aref ritem i) 0)
          (setq len (1+ len))
        (if (> len max)
            (setq max len))
        (setq len 0))
      (setq i (1+ i)))
    (setq maxrhs max)))

(defun wisent-initialize-LA ()
  "Set up LA."
  (let (i j k count rp sp np v)
    (setq consistent (make-vector nstates nil)
          lookaheads (make-vector (1+ nstates) 0)
          count 0
          i 0)
    (while (< i nstates)
      (aset lookaheads i count)
      (setq rp (aref reduction-table i)
            sp (aref shift-table i))
      ;; if (rp &&
      ;;     (rp->nreds > 1
      ;;      || (sp && ! ISVAR(accessing-symbol[sp->shifts[0]]))))
      (if (and rp
               (or (> (reductions-nreds rp) 1)
                   (and sp
                        (not (wisent-ISVAR
                              (aref accessing-symbol
                                    (aref (shifts-shifts sp) 0)))))))
          (setq count (+ count (reductions-nreds rp)))
        (aset consistent i t))

      (when sp
        (setq k 0
              j (shifts-nshifts sp)
              v (shifts-shifts sp))
        (while (< k j)
          (when (= (aref accessing-symbol (aref v k))
                   error-token-number)
            (aset consistent i nil)
            (setq k j)) ;; break
          (setq k (1+ k))))
      (setq i (1+ i)))

    (aset lookaheads nstates count)

    (if (zerop count)
        (progn
          (setq LA (make-vector 1 nil)
                LAruleno (make-vector 1 0)
                lookback (make-vector 1 nil)))
      (setq LA (make-vector count nil)
            LAruleno (make-vector count 0)
            lookback (make-vector count nil)))
    (setq i 0 j (length LA))
    (while (< i j)
      (aset LA i (make-vector tokensetsize 0))
      (setq i (1+ i)))

    (setq np 0
          i  0)
    (while (< i nstates)
      (when (not (aref consistent i))
        (setq rp (aref reduction-table i))
        (when rp
          (setq j 0
                k (reductions-nreds rp)
                v (reductions-rules rp))
          (while (< j k)
            (aset LAruleno np (aref v j))
            (setq np (1+ np)
                  j  (1+ j)))))
      (setq i (1+ i)))))

(defun wisent-set-goto-map ()
  "Set up GOTO-MAP."
  (let (sp i j symbol k temp-map state1 state2 v)
    (setq goto-map (make-vector (1+ nvars) 0)
          temp-map (make-vector (1+ nvars) 0))

    (setq ngotos 0
          sp first-shift)
    (while sp
      (setq i (1- (shifts-nshifts sp))
            v (shifts-shifts sp))
      (while (>= i 0)
        (setq symbol (aref accessing-symbol (aref v i)))
        (if (wisent-ISTOKEN symbol)
            (setq i 0) ;; break
          (setq ngotos (1+ ngotos))
          ;; goto-map[symbol]++;
          (aset goto-map (- symbol ntokens)
                (1+ (aref goto-map (- symbol ntokens)))))
        (setq i (1- i)))
      (setq sp (shifts-next sp)))

    (setq k 0
          i ntokens
          j 0)
    (while (< i nsyms)
      (aset temp-map j k)
      (setq k (+ k (aref goto-map j))
            i (1+ i)
            j (1+ j)))
    (setq i ntokens
          j 0)
    (while (< i nsyms)
      (aset goto-map j (aref temp-map j))
      (setq i (1+ i)
            j (1+ j)))
    ;; goto-map[nsyms] = ngotos;
    ;; temp-map[nsyms] = ngotos;
    (aset goto-map j ngotos)
    (aset temp-map j ngotos)

    (setq from-state (make-vector ngotos 0)
          to-state   (make-vector ngotos 0)
          sp first-shift)
    (while sp
      (setq state1 (shifts-number sp)
            v      (shifts-shifts sp)
            i      (1- (shifts-nshifts sp)))
      (while (>= i 0)
        (setq state2 (aref v i)
              symbol (aref accessing-symbol state2))
        (if (wisent-ISTOKEN symbol)
            (setq i 0) ;; break
          ;; k = temp-map[symbol]++;
          (setq k (aref temp-map (- symbol ntokens)))
          (aset temp-map (- symbol ntokens) (1+ k))
          (aset from-state k state1)
          (aset to-state k state2))
        (setq i (1- i)))
      (setq sp (shifts-next sp)))
  ))

(defun wisent-map-goto (state symbol)
  "Map a STATE/SYMBOL pair into its numeric representation."
  (let (high low middle s result)
    ;; low = goto-map[symbol];
    ;; high = goto-map[symbol + 1] - 1;
    (setq low (aref goto-map (- symbol ntokens))
          high (1- (aref goto-map (- (1+ symbol) ntokens))))
    (while (and (not result) (<= low high))
      (setq middle (/ (+ low high) 2)
            s (aref from-state middle))
      (cond
       ((= s state)
        (setq result middle))
       ((< s state)
        (setq low (1+ middle)))
       (t
        (setq high (1- middle)))))
    (or result
        (error "Internal error in `wisent-map-goto'"))
    ))

(defun wisent-initialize-F ()
  "Set up F."
  (let (i j k sp edge rowp rp reads nedges stateno symbol v break)
    (setq F (make-vector ngotos nil)
          i 0)
    (while (< i ngotos)
      (aset F i (make-vector tokensetsize 0))
      (setq i (1+ i)))

    (setq reads (make-vector ngotos nil)
          edge  (make-vector (1+ ngotos) 0)
          nedges 0
          rowp 0 ;; rowp = F
          i 0)
    (while (< i ngotos)
      (setq stateno (aref to-state i)
            sp (aref shift-table stateno))
      (when sp
        (setq k (shifts-nshifts sp)
              v (shifts-shifts sp)
              j 0
              break nil)
        (while (and (not break) (< j k))
          ;; symbol = accessing-symbol[sp->shifts[j]];
          (setq symbol (aref accessing-symbol (aref v j)))
          (if (wisent-ISVAR symbol)
              (setq break t) ;; break
            (wisent-SETBIT (aref F rowp) symbol)
            (setq j (1+ j))))

        (while (< j k)
          ;; symbol = accessing-symbol[sp->shifts[j]];
          (setq symbol (aref accessing-symbol (aref v j)))
          (when (aref nullable (- symbol ntokens))
            (aset edge nedges (wisent-map-goto stateno symbol))
            (setq nedges (1+ nedges)))
          (setq j (1+ j)))

        (when (> nedges 0)
          ;; reads[i] = rp = NEW2(nedges + 1, short);
          (setq rp (make-vector (1+ nedges) 0)
                j 0)
          (aset reads i rp)
          (while (< j nedges)
            ;; rp[j] = edge[j];
            (aset rp j (aref edge j))
            (setq j (1+ j)))
          (aset rp nedges -1)
          (setq nedges 0)))
      (setq rowp (1+ rowp))
      (setq i (1+ i)))
    (wisent-digraph reads)
    ))

(defun wisent-add-lookback-edge (stateno ruleno gotono)
  "Add a lookback edge.
STATENO, RULENO, GOTONO are self-explanatory."
  (let (i k found)
    (setq i (aref lookaheads stateno)
          k (aref lookaheads (1+ stateno))
          found nil)
    (while (and (not found) (< i k))
      (if (= (aref LAruleno i) ruleno)
          (setq found t)
        (setq i (1+ i))))

    (or found
        (error "Internal error in `wisent-add-lookback-edge'"))

    ;;                value  . next
    ;; lookback[i] = (gotono . lookback[i])
    (aset lookback i (cons gotono (aref lookback i)))))

(defun wisent-transpose (R-arg n)
  "Return the transpose of R-ARG, of size N.
Destroy R-ARG, as it is replaced with the result.  R-ARG[I] is nil or
a -1 terminated list of numbers.  RESULT[NUM] is nil or the -1
terminated list of the I such as NUM is in R-ARG[I]."
  (let (i j new-R end-R nedges v sp)
    (setq new-R  (make-vector n nil)
          end-R  (make-vector n nil)
          nedges (make-vector n 0))

    ;; Count.
    (setq i 0)
    (while (< i n)
      (setq v (aref R-arg i))
      (when v
        (setq j 0)
        (while (>= (aref v j) 0)
          (aset nedges (aref v j) (1+ (aref nedges (aref v j))))
          (setq j (1+ j))))
      (setq i (1+ i)))

    ;; Allocate.
    (setq i 0)
    (while (< i n)
      (when (> (aref nedges i) 0)
        (setq sp (make-vector (1+ (aref nedges i)) 0))
        (aset sp (aref nedges i) -1)
        (aset new-R i sp)
        (aset end-R i 0))
      (setq i (1+ i)))

    ;; Store.
    (setq i 0)
    (while (< i n)
      (setq v (aref R-arg i))
      (when v
        (setq j 0)
        (while (>= (aref v j) 0)
          (aset (aref new-R (aref v j)) (aref end-R (aref v j)) i)
          (aset end-R (aref v j) (1+ (aref end-R (aref v j))))
          (setq j (1+ j))))
      (setq i (1+ i)))

    new-R))

(defun wisent-build-relations ()
  "Build relations."
  (let (i j k rulep rp sp length nedges done state1 stateno
          symbol1 symbol2 edge states v)
    (setq includes (make-vector ngotos nil)
          edge (make-vector (1+ ngotos) 0)
          states (make-vector (1+ maxrhs) 0)
          i 0)

    (while (< i ngotos)
      (setq nedges 0
            state1 (aref from-state i)
            symbol1 (aref accessing-symbol (aref to-state i))
            rulep (aref derives (- symbol1 ntokens)))

      (while (> (car rulep) 0)
        (aset states 0 state1)
        (setq length 1
              stateno state1
              rp (aref rrhs (car rulep))) ;; rp = ritem + rrhs[*rulep]
        (while (> (aref ritem rp) 0) ;; *rp > 0
          (setq symbol2 (aref ritem rp)
                sp (aref shift-table stateno)
                k  (shifts-nshifts sp)
                v  (shifts-shifts sp)
                j  0)
          (while (< j k)
            (setq stateno (aref v j))
            (if (= (aref accessing-symbol stateno) symbol2)
                (setq j k) ;; break
              (setq j (1+ j))))
          ;; states[length++] = stateno;
          (aset states length stateno)
          (setq length (1+ length))
          (setq rp (1+ rp)))

        (if (not (aref consistent stateno))
            (wisent-add-lookback-edge stateno (car rulep) i))

        (setq length (1- length)
              done nil)
        (while (not done)
          (setq done t
                rp (1- rp))
          (when (and (>= rp 0) (wisent-ISVAR (aref ritem rp)))
            ;; stateno = states[--length];
            (setq length (1- length)
                  stateno (aref states length))
            (aset edge nedges (wisent-map-goto stateno (aref ritem rp)))
            (setq nedges (1+ nedges))
            (if (aref nullable (- (aref ritem rp) ntokens))
                (setq done nil))))
        (setq rulep (cdr rulep)))

      (when (> nedges 0)
        (setq v (make-vector (1+ nedges) 0)
              j 0)
        (aset includes i v)
        (while (< j nedges)
          (aset v j (aref edge j))
          (setq j (1+ j)))
        (aset v nedges -1))
      (setq i (1+ i)))

    (setq includes (wisent-transpose includes ngotos))
    ))

(defun wisent-compute-FOLLOWS ()
  "Compute follows."
  (wisent-digraph includes))

(defun wisent-compute-lookaheads ()
  "Compute lookaheads."
  (let (i j n v1 v2 sp)
    (setq n (aref lookaheads nstates)
          i 0)
    (while (< i n)
      (setq sp (aref lookback i))
      (while sp
        (setq v1 (aref LA i)
              v2 (aref F (car sp))
              j  0)
        (while (< j tokensetsize)
          ;; LA (i)[j] |= F (sp->value)[j]
          (aset v1 j (logior (aref v1 j) (aref v2 j)))
          (setq j (1+ j)))
        (setq sp (cdr sp)))
      (setq i (1+ i)))))

(defun wisent-lalr ()
  "Make the nondeterministic finite state machine deterministic."
  (setq tokensetsize (wisent-WORDSIZE ntokens))
  (wisent-set-state-table)
  (wisent-set-accessing-symbol)
  (wisent-set-shift-table)
  (wisent-set-reduction-table)
  (wisent-set-maxrhs)
  (wisent-initialize-LA)
  (wisent-set-goto-map)
  (wisent-initialize-F)
  (wisent-build-relations)
  (wisent-compute-FOLLOWS)
  (wisent-compute-lookaheads))

;;;; -----------------------------------------------
;;;; Find and resolve or report look-ahead conflicts
;;;; -----------------------------------------------

(defsubst wisent-log-resolution (state LAno token resolution)
  "Log a shift-reduce conflict resolution.
In specified STATE between rule pointed by lookahead number LANO and
TOKEN, resolved as RESOLUTION."
  (if (or wisent-verbose-flag wisent-debug-flag)
      (wisent-log
       "Conflict in state %d between rule %d and token %s resolved as %s.\n"
       state (aref LAruleno LAno) (wisent-tag token) resolution)))

(defun wisent-flush-shift (state token)
  "Turn off the shift recorded in the specified STATE for TOKEN.
Used when we resolve a shift-reduce conflict in favor of the reduction."
  (let (shiftp i k v)
    (when (setq shiftp (aref shift-table state))
      (setq k (shifts-nshifts shiftp)
            v (shifts-shifts shiftp)
            i 0)
      (while (< i k)
        (if (and (not (zerop (aref v i)))
                 (= token (aref accessing-symbol (aref v i))))
            (aset v i 0))
        (setq i (1+ i))))))

(defun wisent-resolve-sr-conflict (state lookaheadnum)
  "Attempt to resolve shift-reduce conflict for one rule.
Resolve by means of precedence declarations.  The conflict occurred in
specified STATE for the rule pointed by the lookahead symbol
LOOKAHEADNUM.  It has already been checked that the rule has a
precedence.  A conflict is resolved by modifying the shift or reduce
tables so that there is no longer a conflict."
  (let (i redprec errp errs nerrs token sprec sassoc)
    ;; Find the rule to reduce by to get precedence of reduction
    (setq token (aref tags (aref rprec (aref LAruleno lookaheadnum)))
          redprec (wisent-prec token)
          errp  (make-errs)
          errs  (make-vector ntokens 0)
          nerrs 0
          i 0)
    (set-errs-errs errp errs)
    (while (< i ntokens)
      (setq token (aref tags i))
      (when (and (wisent-BITISSET (aref LA lookaheadnum) i)
                 (wisent-BITISSET lookaheadset i)
                 (setq sprec (wisent-prec token)))
        ;; Shift-reduce conflict occurs for token number I and it has
        ;; a precedence.  The precedence of shifting is that of token
        ;; I.
        (cond
         ((< sprec redprec)
          (wisent-log-resolution state lookaheadnum i "reduce")
          ;;  Flush the shift for this token
          (wisent-RESETBIT lookaheadset i)
          (wisent-flush-shift state i)
          )
         ((> sprec redprec)
          (wisent-log-resolution state lookaheadnum i "shift")
          ;; Flush the reduce for this token
          (wisent-RESETBIT (aref LA lookaheadnum) i)
          )
         (t
          ;; Matching precedence levels.
          ;; For left association, keep only the reduction.
          ;; For right association, keep only the shift.
          ;; For nonassociation, keep neither.
          (setq sassoc (wisent-assoc token))
          (cond
           ((eq sassoc 'right)
            (wisent-log-resolution state lookaheadnum i "shift"))
           ((eq sassoc 'left)
            (wisent-log-resolution state lookaheadnum i "reduce"))
           ((eq sassoc 'nonassoc)
            (wisent-log-resolution state lookaheadnum i "an error"))
           )
          (when (not (eq sassoc 'right))
            ;; Flush the shift for this token
            (wisent-RESETBIT lookaheadset i)
            (wisent-flush-shift state i))
          (when (not (eq sassoc 'left))
            ;; Flush the reduce for this token
            (wisent-RESETBIT (aref LA lookaheadnum) i))
          (when (eq sassoc 'nonassoc)
            ;; Record an explicit error for this token
            (aset errs nerrs i)
            (setq nerrs (1+ nerrs)))
          )))
      (setq i (1+ i)))
    (when (> nerrs 0)
      (set-errs-nerrs errp nerrs)
      (aset err-table state errp))
    ))

(defun wisent-set-conflicts (state)
  "Find and attempt to resolve conflicts in specified STATE."
  (let (i j k v shiftp symbol)
    (unless (aref consistent state)
      (fillarray lookaheadset 0)

      (when (setq shiftp (aref shift-table state))
        (setq k (shifts-nshifts shiftp)
              v (shifts-shifts shiftp)
              i 0)
        (while (and (< i k)
                    (wisent-ISTOKEN
                     (setq symbol (aref accessing-symbol (aref v i)))))
          (or (zerop (aref v i))
              (wisent-SETBIT lookaheadset symbol))
          (setq i (1+ i))))

      ;; Loop over all rules which require lookahead in this state
      ;; first check for shift-reduce conflict, and try to resolve
      ;; using precedence
      (setq i (aref lookaheads state)
            k (aref lookaheads (1+ state)))
      (while (< i k)
        (when (aref rprec (aref LAruleno i))
          (setq v (aref LA i)
                j 0)
          (while (< j tokensetsize)
            (if (zerop (logand (aref v j) (aref lookaheadset j)))
                (setq j (1+ j))
              ;; if (LA (i)[j] & lookaheadset[j])
              (wisent-resolve-sr-conflict state i)
              (setq j tokensetsize)))) ;; break
        (setq i (1+ i)))

      ;; Loop over all rules which require lookahead in this state
      ;; Check for conflicts not resolved above.
      (setq i (aref lookaheads state))
      (while (< i k)
        (setq v (aref LA i)
              j 0)
        (while (< j tokensetsize)
          ;; if (LA (i)[j] & lookaheadset[j])
          (if (not (zerop (logand (aref v j) (aref lookaheadset j))))
              (aset conflicts state t))
          (setq j (1+ j)))
        (setq j 0)
        (while (< j tokensetsize)
          ;; lookaheadset[j] |= LA (i)[j];
          (aset lookaheadset j (logior (aref lookaheadset j)
                                       (aref v j)))
          (setq j (1+ j)))
        (setq i (1+ i)))
      )))

(defun wisent-resolve-conflicts ()
  "Find and resolve conflicts."
  (let (i)
    (setq conflicts    (make-vector nstates nil)
          shiftset     (make-vector tokensetsize 0)
          lookaheadset (make-vector tokensetsize 0)
          err-table    (make-vector nstates nil)
          i 0)
    (while (< i nstates)
      (wisent-set-conflicts i)
      (setq i (1+ i)))))

(defun wisent-count-sr-conflicts (state)
  "Count the number of shift/reduce conflicts in specified STATE."
  (let (i j k shiftp symbol v)
    (setq src-count 0
          shiftp (aref shift-table state))
    (when shiftp
      (fillarray shiftset 0)
      (fillarray lookaheadset 0)
      (setq k (shifts-nshifts shiftp)
            v (shifts-shifts shiftp)
            i 0)
      (while (< i k)
        (when (not (zerop (aref v i)))
          (setq symbol (aref accessing-symbol (aref v i)))
          (if (wisent-ISVAR symbol)
              (setq i k) ;; break
            (wisent-SETBIT shiftset symbol)))
        (setq i (1+ i)))

      (setq k (aref lookaheads (1+ state))
            i (aref lookaheads state))
      (while (< i k)
        (setq v (aref LA i)
              j 0)
        (while (< j tokensetsize)
          ;; lookaheadset[j] |= LA (i)[j]
          (aset lookaheadset j (logior (aref lookaheadset j)
                                       (aref v j)))
          (setq j (1+ j)))
        (setq i (1+ i)))

      (setq k 0)
      (while (< k tokensetsize)
        ;; lookaheadset[k] &= shiftset[k];
        (aset lookaheadset k (logand (aref lookaheadset k)
                                     (aref shiftset k)))
        (setq k (1+ k)))

      (setq i 0)
      (while (< i ntokens)
        (if (wisent-BITISSET lookaheadset i)
            (setq src-count (1+ src-count)))
        (setq i (1+ i))))
    src-count))

(defun wisent-count-rr-conflicts (state)
  "Count the number of reduce/reduce conflicts in specified STATE."
  (let (i j count n m)
    (setq rrc-count 0
          m (aref lookaheads state)
          n (aref lookaheads (1+ state)))
    (when (>= (- n m) 2)
      (setq i 0)
      (while (< i ntokens)
        (setq count 0
              j m)
        (while (< j n)
          (if (wisent-BITISSET (aref LA j) i)
              (setq count (1+ count)))
          (setq j (1+ j)))

        (if (>= count 2)
            (setq rrc-count (1+ rrc-count)))
        (setq i (1+ i))))
    rrc-count))

(defvar wisent-expected-conflicts nil
  "*If non-nil suppress the warning about shift/reduce conflicts.
It is a decimal integer N that says there should be no warning if
there are N shift/reduce conflicts and no reduce/reduce conflicts.  A
warning is given if there are either more or fewer conflicts, or if
there are any reduce/reduce conflicts.")

(defun wisent-total-conflicts ()
  "Report the total number of conflicts."
  (unless (and (zerop rrc-total)
               (or (zerop src-total)
                   (= src-total (or wisent-expected-conflicts 0))))
    (let* ((src (wisent-source))
           (src (if src (concat " in " src) ""))
           (msg (format "Grammar%s contains" src)))
      (if (> src-total 0)
          (setq msg (format "%s %d shift/reduce conflict%s"
                            msg src-total (if (> src-total 1)
                                              "s" ""))))
      (if (and (> src-total 0) (> rrc-total 0))
          (setq msg (format "%s and" msg)))
      (if (> rrc-total 0)
        (setq msg (format "%s %d reduce/reduce conflict%s"
                          msg rrc-total (if (> rrc-total 1)
                                            "s" ""))))
      (message msg))))

(defun wisent-print-conflicts ()
  "Report conflicts."
  (let (i)
    (setq  src-total 0
           rrc-total 0
           i 0)
    (while (< i nstates)
      (when (aref conflicts i)
        (wisent-count-sr-conflicts i)
        (wisent-count-rr-conflicts i)
        (setq src-total (+ src-total src-count)
              rrc-total (+ rrc-total rrc-count))
        (when (or wisent-verbose-flag wisent-debug-flag)
          (wisent-log "State %d contains" i)
          (if (> src-count 0)
              (wisent-log " %d shift/reduce conflict%s"
                          src-count (if (> src-count 1) "s" "")))

          (if (and (> src-count 0) (> rrc-count 0))
              (wisent-log " and"))

          (if (> rrc-count 0)
              (wisent-log " %d reduce/reduce conflict%s"
                          rrc-count (if (> rrc-count 1) "s" "")))

          (wisent-log ".\n")))
      (setq i (1+ i)))
    (wisent-total-conflicts)))

;;;; --------------------------------------
;;;; Report information on generated parser
;;;; --------------------------------------
(defun wisent-print-grammar ()
  "Print grammar."
  (let (i j r break left-count right-count)

    (wisent-log "\n\nGrammar\n\n  Number, Rule\n")
    (setq i 1)
    (while (<= i nrules)
      ;; Don't print rules disabled in `wisent-reduce-grammar-tables'.
      (when (aref ruseful i)
        (wisent-log "  %s  %s ->"
                    (wisent-pad-string (number-to-string i) 6)
                    (wisent-tag (aref rlhs i)))
        (setq r (aref rrhs i))
        (if (> (aref ritem r) 0)
            (while (> (aref ritem r) 0)
              (wisent-log " %s" (wisent-tag (aref ritem r)))
              (setq r (1+ r)))
          (wisent-log " /* empty */"))
        (wisent-log "\n"))
      (setq i (1+ i)))

    (wisent-log "\n\nTerminals, with rules where they appear\n\n")
    (wisent-log "%s (-1)\n" (wisent-tag 0))
    (setq i 1)
    (while (< i ntokens)
      (wisent-log "%s (%d)" (wisent-tag i) i)
      (setq j 1)
      (while (<= j nrules)
        (setq r (aref rrhs j)
              break nil)
        (while (and (not break) (> (aref ritem r) 0))
          (if (setq break (= (aref ritem r) i))
              (wisent-log " %d" j)
            (setq r (1+ r))))
        (setq j (1+ j)))
      (wisent-log "\n")
      (setq i (1+ i)))

    (wisent-log "\n\nNonterminals, with rules where they appear\n\n")
    (setq i ntokens)
    (while (< i nsyms)
      (setq left-count 0
            right-count 0
            j 1)
      (while (<= j nrules)
        (if (= (aref rlhs j) i)
            (setq left-count (1+ left-count)))
        (setq r (aref rrhs j)
              break nil)
        (while (and (not break) (> (aref ritem r) 0))
          (if (= (aref ritem r) i)
              (setq right-count (1+ right-count)
                    break t)
            (setq r (1+ r))))
        (setq j (1+ j)))
      (wisent-log "%s (%d)\n   " (wisent-tag i) i)
      (when (> left-count 0)
        (wisent-log " on left:")
        (setq j 1)
        (while (<= j nrules)
          (if (= (aref rlhs j) i)
              (wisent-log " %d" j))
          (setq j (1+ j))))
      (when (> right-count 0)
        (if (> left-count 0)
            (wisent-log ","))
        (wisent-log " on right:")
        (setq j 1)
        (while (<= j nrules)
          (setq r (aref rrhs j)
                break nil)
          (while (and (not break) (> (aref ritem r) 0))
            (if (setq break (= (aref ritem r) i))
                (wisent-log " %d" j)
              (setq r (1+ r))))
          (setq j (1+ j))))
      (wisent-log "\n")
      (setq i (1+ i)))
    ))

(defun wisent-print-reductions (state)
  "Print reductions on STATE."
  (let (i j k v symbol m n defaulted
          default-LA default-rule cmax count shiftp errp nodefault)
    (setq nodefault nil
          i 0)
    (fillarray shiftset 0)

    (setq shiftp (aref shift-table state))
    (when shiftp
      (setq k (shifts-nshifts shiftp)
            v (shifts-shifts  shiftp)
            i 0)
      (while (< i k)
        (when (not (zerop (aref v i)))
          (setq symbol (aref accessing-symbol (aref v i)))
          (if (wisent-ISVAR symbol)
              (setq i k) ;; break
            ;; If this state has a shift for the error token, don't
            ;; use a default rule.
            (if (= symbol error-token-number)
                (setq nodefault t))
            (wisent-SETBIT shiftset symbol)))
        (setq i (1+ i))))

    (setq errp (aref err-table state))
    (when errp
      (setq k (errs-nerrs errp)
            v (errs-errs errp)
            i 0)
      (while (< i k)
        (if (not (zerop (setq symbol (aref v i))))
            (wisent-SETBIT shiftset symbol))
        (setq i (1+ i))))

    (setq m (aref lookaheads state)
          n (aref lookaheads (1+ state)))

    (cond
     ((and (= (- n m) 1) (not nodefault))
      (setq default-rule (aref LAruleno m)
            v (aref LA m)
            k 0)
      (while (< k tokensetsize)
        (aset lookaheadset k (logand (aref v k)
                                     (aref shiftset k)))
        (setq k (1+ k)))

      (setq i 0)
      (while (< i ntokens)
        (if (wisent-BITISSET lookaheadset i)
            (wisent-log "    %s\t[reduce using rule %d (%s)]\n"
                        (wisent-tag i) default-rule
                        (wisent-tag (aref rlhs default-rule))))
        (setq i (1+ i)))
      (wisent-log "    $default\treduce using rule %d (%s)\n\n"
                  default-rule
                  (wisent-tag (aref rlhs default-rule)))
      )
     ((>= (- n m) 1)
      (setq cmax 0
            default-LA -1
            default-rule 0)
      (when (not nodefault)
        (setq i m)
        (while (< i n)
          (setq v (aref LA i)
                count 0
                k 0)
          (while (< k tokensetsize)
            ;; lookaheadset[k] = LA (i)[k] & ~shiftset[k]
            (aset lookaheadset k
                  (logand (aref v k)
                          (lognot (aref shiftset k))))
            (setq k (1+ k)))
          (setq j 0)
          (while (< j ntokens)
            (if (wisent-BITISSET lookaheadset j)
                (setq count (1+ count)))
            (setq j (1+ j)))
          (if (> count cmax)
              (setq cmax count
                    default-LA i
                    default-rule (aref LAruleno i)))
          (setq k 0)
          (while (< k tokensetsize)
            (aset shiftset k (logior (aref shiftset k)
                                     (aref lookaheadset k)))
            (setq k (1+ k)))
          (setq i (1+ i))))

      (fillarray shiftset 0)

      (when shiftp
        (setq k (shifts-nshifts shiftp)
              v (shifts-shifts  shiftp)
              i 0)
        (while (< i k)
          (when (not (zerop (aref v i)))
            (setq symbol (aref accessing-symbol (aref v i)))
            (if (wisent-ISVAR symbol)
                (setq i k) ;; break
              (wisent-SETBIT shiftset symbol)))
          (setq i (1+ i))))

      (setq i 0)
      (while (< i ntokens)
        (setq defaulted nil
              count (if (wisent-BITISSET shiftset i) 1 0)
              j m)
        (while (< j n)
          (when (wisent-BITISSET (aref LA j) i)
            (if (zerop count)
                (progn
                  (if (not (= j default-LA))
                      (wisent-log
                       "    %s\treduce using rule %d (%s)\n"
                       (wisent-tag i) (aref LAruleno j)
                       (wisent-tag (aref rlhs (aref LAruleno j))))
                    (setq defaulted t))
                  (setq count (1+ count)))
              (if defaulted
                  (wisent-log
                   "    %s\treduce using rule %d (%s)\n"
                   (wisent-tag i) (aref LAruleno default-LA)
                   (wisent-tag (aref rlhs (aref LAruleno default-LA)))))
              (setq defaulted nil)
              (wisent-log
               "    %s\t[reduce using rule %d (%s)]\n"
               (wisent-tag i) (aref LAruleno j)
               (wisent-tag (aref rlhs (aref LAruleno j))))))
          (setq j (1+ j)))
        (setq i (1+ i)))

      (if (>= default-LA 0)
          (wisent-log
           "    $default\treduce using rule %d (%s)\n"
           default-rule
           (wisent-tag (aref rlhs default-rule))))
      ))))

(defun wisent-print-actions (state)
  "Print actions on STATE."
  (let (i j k v state1 symbol shiftp errp redp rule nerrs break)
    (setq shiftp (aref shift-table state)
          redp   (aref reduction-table state)
          errp   (aref err-table state))
    (if (and (not shiftp) (not redp))
        (if (= final-state state)
            (wisent-log "    $default\taccept\n")
          (wisent-log "    NO ACTIONS\n"))
     (if (not shiftp)
         (setq i 0
               k 0)
      (setq k (shifts-nshifts shiftp)
            v (shifts-shifts shiftp)
            i 0
            break nil)
      (while (and (not break) (< i k))
        (if (zerop (setq state1 (aref v i)))
            (setq i (1+ i))
          (setq symbol (aref accessing-symbol state1))
          ;;  The following line used to be turned off.
          (if (wisent-ISVAR symbol)
              (setq break t) ;; break
            (wisent-log "    %s\tshift, and go to state %d\n"
                        (wisent-tag symbol) state1)
            (setq i (1+ i)))))
      (if (> i 0)
          (wisent-log "\n")))

     (when errp
       (setq nerrs (errs-nerrs errp)
             v (errs-errs errp)
             j 0)
       (while (< j nerrs)
         (if (aref v j)
             (wisent-log "    %s\terror (nonassociative)\n"
                         (wisent-tag (aref v j))))
         (setq j (1+ j)))
       (if (> j 0)
           (wisent-log "\n")))

     (cond
      ((and (aref consistent state) redp)
       (setq rule (aref (reductions-rules redp) 0)
             symbol (aref rlhs rule))
       (wisent-log "    $default\treduce using rule %d (%s)\n\n"
                   rule (wisent-tag symbol))
       )
      (redp
       (wisent-print-reductions state)
       ))

     (when (< i k)
       (setq v (shifts-shifts shiftp))
       (while (< i k)
         (when (setq state1 (aref v i))
           (setq symbol (aref accessing-symbol state1))
           (wisent-log "    %s\tgo to state %d\n"
                       (wisent-tag symbol) state1))
         (setq i (1+ i)))
       (wisent-log "\n"))
     )))

(defun wisent-print-core (state)
  "Print STATE core."
  (let (i k rule statep sp sp1)
    (setq statep (aref state-table state)
          k (core-nitems statep))
    (when (> k 0)
      (setq i 0)
      (while (< i k)
        ;; sp1 = sp = ritem + statep->items[i];
        (setq sp1 (aref (core-items statep) i)
              sp  sp1)
        (while (> (aref ritem sp) 0)
          (setq sp (1+ sp)))

        (setq rule (- (aref ritem sp)))
        (wisent-log "    %s  ->  " (wisent-tag (aref rlhs rule)))

        (setq sp (aref rrhs rule))
        (while (< sp sp1)
          (wisent-log "%s " (wisent-tag (aref ritem sp)))
          (setq sp (1+ sp)))
        (wisent-log ".")
        (while (> (aref ritem sp) 0)
          (wisent-log " %s" (wisent-tag (aref ritem sp)))
          (setq sp (1+ sp)))
        (wisent-log "   (rule %d)\n" rule)
        (setq i (1+ i)))
      (wisent-log "\n"))))

(defun wisent-print-state (state)
  "Print information on STATE."
  (wisent-log "\n\nstate %d\n\n" state)
  (wisent-print-core state)
  (wisent-print-actions state))

(defun wisent-print-states ()
  "Print information on states."
  (let ((i 0))
    (while (< i nstates)
      (wisent-print-state i)
      (setq i (1+ i)))))

(defun wisent-print-results ()
  "Print information on generated parser.
Report detailed information if `wisent-verbose-flag' or
`wisent-debug-flag' are non-nil."
  (when (or wisent-verbose-flag wisent-debug-flag)
    (wisent-print-useless))
  (wisent-print-conflicts)
  (when (or wisent-verbose-flag wisent-debug-flag)
    (wisent-print-grammar)
    (wisent-print-states))
  ;; Append output to log file when running in batch mode
  (when (wisent-noninteractive)
    (wisent-append-to-log-file)
    (wisent-clear-log)))

;;;; ---------------------------------
;;;; Build the generated parser tables
;;;; ---------------------------------

(defun wisent-action-row (state actrow)
  "Figure out the actions for the specified STATE.
Decide what to do for each type of token if seen as the lookahead
token in specified state.  The value returned is used as the default
action for the state.  In addition, ACTROW is filled with what to do
for each kind of token, index by symbol number, with nil meaning do
the default action.  The value 'error, means this situation is an
error.  The parser recognizes this value specially.

This is where conflicts are resolved.  The loop over lookahead rules
considered lower-numbered rules last, and the last rule considered
that likes a token gets to handle it."
  (let (i j k m n v default-rule nreds rule max count
          shift-state symbol redp shiftp errp nodefault)

    (fillarray actrow nil)

    (setq default-rule 0
          nodefault nil ;; nil inhibit having any default reduction
          nreds 0
          m 0
          n 0
          redp (aref reduction-table state))

    (when redp
      (setq nreds (reductions-nreds redp))
      (when (>= nreds 1)
        ;; loop over all the rules available here which require
        ;; lookahead
        (setq m (aref lookaheads state)
              n (aref lookaheads (1+ state))
              i (1- n))
        (while (>= i m)
          ;; and find each token which the rule finds acceptable to
          ;; come next
          (setq j 0)
          (while (< j ntokens)
            ;; and record this rule as the rule to use if that token
            ;; follows.
            (if (wisent-BITISSET (aref LA i) j)
                (aset actrow j (- (aref LAruleno i)))
              )
            (setq j (1+ j)))
          (setq i (1- i)))))

    ;; Now see which tokens are allowed for shifts in this state.  For
    ;; them, record the shift as the thing to do.  So shift is
    ;; preferred to reduce.
    (setq shiftp (aref shift-table state))
    (when shiftp
      (setq k (shifts-nshifts shiftp)
            v (shifts-shifts shiftp)
            i 0)
      (while (< i k)
        (setq shift-state (aref v i))
        (if (zerop shift-state)
            nil ;; continue
          (setq symbol (aref accessing-symbol shift-state))
          (if (wisent-ISVAR symbol)
              (setq i k) ;; break
            (aset actrow symbol shift-state)
            ;; Do not use any default reduction if there is a shift
            ;; for error
            (if (= symbol error-token-number)
                (setq nodefault t))))
        (setq i (1+ i))))

    ;; See which tokens are an explicit error in this state (due to
    ;; %nonassoc).  For them, record error as the action.
    (setq errp (aref err-table state))
    (when errp
      (setq k (errs-nerrs errp)
            v (errs-errs errp)
            i 0)
      (while (< i k)
        (aset actrow (aref v i) wisent-error-tag)
        (setq i (1+ i))))

    ;; Now find the most common reduction and make it the default
    ;; action for this state.
    (when (and (>= nreds 1) (not nodefault))
      (if (aref consistent state)
          (setq default-rule (- (aref (reductions-rules redp) 0)))
        (setq max 0
              i m)
        (while (< i n)
          (setq count 0
                rule (- (aref LAruleno i))
                j 0)
          (while (< j ntokens)
            (if (and (numberp (aref actrow j))
                     (= (aref actrow j) rule))
                (setq count (1+ count)))
            (setq j (1+ j)))
          (if (> count max)
              (setq max count
                    default-rule rule))
          (setq i (1+ i)))
        ;; actions which match the default are replaced with zero,
        ;; which means "use the default"
        (when (> max 0)
          (setq j 0)
          (while (< j ntokens)
            (if (and (numberp (aref actrow j))
                     (= (aref actrow j) default-rule))
                (aset actrow j nil))
            (setq j (1+ j)))
          )))

    ;; If have no default rule, if this is the final state the default
    ;; is accept else it is an error.  So replace any action which
    ;; says "error" with "use default".
    (when (zerop default-rule)
      (if (= final-state state)
          (setq default-rule wisent-accept-tag)
        (setq j 0)
        (while (< j ntokens)
          (if (eq (aref actrow j) wisent-error-tag)
              (aset actrow j nil))
          (setq j (1+ j)))
        (setq default-rule wisent-error-tag)))
    default-rule))

(defconst wisent-default-tag 'default
  "Tag used in an action table to indicate a default action.")

;; These variables only exist locally in the function
;; `wisent-state-actions' and are shared by all other nested callees.
(wisent-defcontext semantic-actions
  ;; Uninterned symbols used in code generation.
  stack sp gotos state
  ;; Name of the current semantic action
  NAME)

(defun wisent-state-actions ()
  "Figure out the actions for every state.
Return the action table."
  ;; Store the semantic action obarray in (unused) RCODE[0].
  (aset rcode 0 (make-vector 13 0))
  (let (i j action-table actrow action)
    (setq action-table (make-vector nstates nil)
          actrow (make-vector ntokens nil)
          i 0)
    (wisent-with-context semantic-actions
      (setq stack (make-symbol "stack")
            sp    (make-symbol "sp")
            gotos (make-symbol "gotos")
            state (make-symbol "state"))
      (while (< i nstates)
        (setq action (wisent-action-row i actrow))
        ;; Translate a reduction into semantic action
        (and (integerp action) (< action 0)
             (setq action (wisent-semantic-action (- action))))
        (aset action-table i (list (cons wisent-default-tag action)))
        (setq j 0)
        (while (< j ntokens)
          (when (setq action (aref actrow j))
            ;; Translate a reduction into semantic action
            (and (integerp action) (< action 0)
                 (setq action (wisent-semantic-action (- action))))
            (aset action-table i (cons (cons (aref tags j) action)
                                       (aref action-table i)))
            )
          (setq j (1+ j)))
        (aset action-table i (nreverse (aref action-table i)))
        (setq i (1+ i)))
      action-table)))

(defun wisent-goto-actions ()
  "Figure out what to do after reducing with each rule.
Depending on the saved state from before the beginning of parsing the
data that matched this rule.  Return the goto table."
  (let (i j m n symbol state goto-table)
    (setq goto-table (make-vector nstates nil)
          i ntokens)
    (while (< i nsyms)
      (setq symbol (- i ntokens)
            m (aref goto-map symbol)
            n (aref goto-map (1+ symbol))
            j m)
      (while (< j n)
        (setq state (aref from-state j))
        (aset goto-table state
              (cons (cons (aref tags i) (aref to-state j))
                    (aref goto-table state)))
        (setq j (1+ j)))
      (setq i (1+ i)))
    goto-table))

(defsubst wisent-quote-p (sym)
  "Return non-nil if SYM is bound to the `quote' function."
  (condition-case nil
      (eq (indirect-function sym)
          (indirect-function 'quote))
    (error nil)))

(defsubst wisent-backquote-p (sym)
  "Return non-nil if SYM is bound to the `backquote' function."
  (condition-case nil
      (eq (indirect-function sym)
          (indirect-function 'backquote))
    (error nil)))

(defun wisent-check-$N (x m)
  "Return non-nil if X is a valid $N or $regionN symbol.
That is if X is a $N or $regionN symbol with N >= 1 and N <= M.
Also warn if X is a $N or $regionN symbol with N < 1 or N > M."
  (when (symbolp x)
    (let* ((n (symbol-name x))
           (i (and (string-match "\\`\\$\\(region\\)?\\([0-9]+\\)\\'" n)
                   (string-to-number (match-string 2 n)))))
      (when i
        (if (and (>= i 1) (<= i m))
            t
          (message
           "*** In %s, %s might be a free variable (rule has %s)"
           NAME x (format (cond ((< m 1) "no component")
                                ((= m 1) "%d component")
                                ("%d components"))
                          m))
          nil)))))

(defun wisent-semantic-action-expand-body (body n &optional found)
  "Parse BODY of semantic action.
N is the maximum number of $N variables that can be referenced in
BODY.  Warn on references out of permitted range.
Optional argument FOUND is the accumulated list of '$N' references
encountered so far.
Return a cons (FOUND . XBODY), where FOUND is the list of $N
references found in BODY, and XBODY is BODY expression with
`backquote' forms expanded."
  (if (not (listp body))
      ;; BODY is an atom, no expansion needed
      (progn
        (if (wisent-check-$N body n)
            ;; Accumulate $i symbol
            (add-to-list 'found body))
        (cons found body))
    ;; BODY is a list, expand inside it
    (let (xbody sexpr)
      ;; If backquote expand it first
      (if (wisent-backquote-p (car body))
          (setq body (macroexpand body)))
      (while body
        (setq sexpr (car body)
              body  (cdr body))
        (cond
         ;; Function call excepted quote expression
         ((and (consp sexpr)
               (not (wisent-quote-p (car sexpr))))
          (setq sexpr (wisent-semantic-action-expand-body sexpr n found)
                found (car sexpr)
                sexpr (cdr sexpr)))
         ;; $i symbol
         ((wisent-check-$N sexpr n)
          ;; Accumulate $i symbol
          (add-to-list 'found sexpr))
         )
        ;; Accumulate expanded forms
        (setq xbody (nconc xbody (list sexpr))))
      (cons found xbody))))

(defun wisent-semantic-action (r)
  "Set up the Elisp function for semantic action at rule R.
On entry RCODE[R] contains a vector [BODY N (NTERM I)] where BODY is the
body of the semantic action, N is the maximum number of values
available in the parser's stack, NTERM is the nonterminal the semantic
action belongs to, and I is the index of the semantic action inside
NTERM definition.  Return the semantic action symbol.
The semantic action function accepts three arguments:

- the state/value stack
- the top-of-stack index
- the goto table

And returns the updated top-of-stack index."
  (if (not (aref ruseful r))
      (aset rcode r nil)
    (let* ((actn (aref rcode r))
           (n    (aref actn 1))         ; nb of val avail. in stack
           (NAME (apply 'format "%s:%d" (aref actn 2)))
           (form (wisent-semantic-action-expand-body (aref actn 0) n))
           ($l   (car form))            ; list of $vars used in body
           (form (cdr form))            ; expanded form of body
           (nt   (aref rlhs r))         ; nonterminal item no.
           (bl   nil)                   ; `let*' binding list
           $v i j)

      ;; Compute $N and $regionN bindings
      (setq i n)
      (while (> i 0)
        (setq j (1+ (* 2 (- n i))))
        ;; Only bind $regionI if used in action
        (setq $v (intern (format "$region%d" i)))
        (if (memq $v $l)
            (setq bl (cons `(,$v (cdr (aref ,stack (- ,sp ,j)))) bl)))
        ;; Only bind $I if used in action
        (setq $v (intern (format "$%d" i)))
        (if (memq $v $l)
            (setq bl (cons `(,$v (car (aref ,stack (- ,sp ,j)))) bl)))
        (setq i (1- i)))

      ;; Compute J, the length of rule's RHS.  It will give the
      ;; current parser state at STACK[SP - 2*J], and where to push
      ;; the new semantic value and the next state, respectively at:
      ;; STACK[SP - 2*J + 1] and STACK[SP - 2*J + 2].  Generally N,
      ;; the maximum number of values available in the stack, is equal
      ;; to J.  But, for mid-rule actions, N is the number of rule
      ;; elements before the action and J is always 0 (empty rule).
      (setq i (aref rrhs r)
            j 0)
      (while (> (aref ritem i) 0)
        (setq j (1+ j)
              i (1+ i)))

      ;; Create the semantic action symbol.
      (setq actn (intern NAME (aref rcode 0)))

      ;; Store source code in function cell of the semantic action
      ;; symbol.  It will be byte-compiled at automaton's compilation
      ;; time.  Using a byte-compiled automaton can significantly
      ;; speed up parsing!
      (fset actn
            `(lambda (,stack ,sp ,gotos)
               (let* (,@bl
                      ($region
                       ,(cond
                         ((= n 1)
                          (if (assq '$region1 bl)
                              '$region1
                            `(cdr (aref ,stack (1- ,sp)))))
                         ((> n 1)
                          `(wisent-production-bounds
                            ,stack (- ,sp ,(1- (* 2 n))) (1- ,sp)))))
                      ($action ,NAME)
                      ($nterm  ',(aref tags nt))
                      ,@(and (> j 0) `((,sp (- ,sp ,(* j 2)))))
                      (,state (cdr (assq $nterm
                                         (aref ,gotos
                                               (aref ,stack ,sp))))))
                 (setq ,sp (+ ,sp 2))
                 ;; push semantic value
                 (aset ,stack (1- ,sp) (cons ,form $region))
                 ;; push next state
                 (aset ,stack ,sp ,state)
                 ;; return new top of stack
                 ,sp)))

      ;; Return the semantic action symbol
      actn)))

;;;; ----------------------------
;;;; Build parser LALR automaton.
;;;; ----------------------------

(defun wisent-parser-automaton ()
  "Compute and return LALR(1) automaton from GRAMMAR.
GRAMMAR is in internal format.  GRAM/ACTS are grammar rules
in internal format.  STARTS defines the start symbols."
  ;; Check for useless stuff
  (wisent-reduce-grammar)

  (wisent-set-derives)
  (wisent-set-nullable)
  ;; convert to nondeterministic finite state machine.
  (wisent-generate-states)
  ;; make it deterministic.
  (wisent-lalr)
  ;; Find and record any conflicts: places where one token of
  ;; lookahead is not enough to disambiguate the parsing.  Also
  ;; resolve s/r conflicts based on precedence declarations.
  (wisent-resolve-conflicts)
  (wisent-print-results)

  (vector (wisent-state-actions)        ; action table
          (wisent-goto-actions)         ; goto table
          start-table                   ; start symbols
          (aref rcode 0)                ; sem. action symbol obarray
          )
  )

;;;; -------------------
;;;; Parse input grammar
;;;; -------------------

(defconst wisent-reserved-symbols (list wisent-error-term)
  "The list of reserved symbols.
Also all symbols starting with a character defined in
`wisent-reserved-capitals' are reserved for internal use.")

(defconst wisent-reserved-capitals '(?\$ ?\@)
  "The list of reserved capital letters.
All symbol starting with one of these letters are reserved for
internal use.")

(defconst wisent-starts-nonterm '$STARTS
  "Main start symbol.
It gives the rules for start symbols.")

(defvar wisent-single-start-flag nil
  "Non-nil means allows only one start symbol like in Bison.
That is don't add extra start rules to the grammar.  This is
useful to compare the Wisent's generated automaton with the Bison's
one.")

(defsubst wisent-ISVALID-VAR (x)
  "Return non-nil if X is a character or an allowed symbol."
  (and x (symbolp x)
       (not (memq (aref (symbol-name x) 0) wisent-reserved-capitals))
       (not (memq x wisent-reserved-symbols))))

(defsubst wisent-ISVALID-TOKEN (x)
  "Return non-nil if X is a character or an allowed symbol."
  (or (wisent-char-p x)
      (wisent-ISVALID-VAR x)))

(defun wisent-push-token (symbol &optional nocheck)
  "Push a new SYMBOL in the list of tokens.
Bypass checking if NOCHECK is non-nil."
  ;; Check
  (or nocheck (wisent-ISVALID-TOKEN symbol)
      (error "Invalid terminal symbol: %S" symbol))
  (if (memq symbol token-list)
      (message "*** duplicate terminal `%s' ignored" symbol)
    ;; Set up properties
    (wisent-set-prec        symbol nil)
    (wisent-set-assoc       symbol nil)
    (wisent-set-item-number symbol ntokens)
    ;; Add
    (setq ntokens (1+ ntokens)
          token-list (cons symbol token-list))))

(defun wisent-push-var (symbol &optional nocheck)
  "Push a new SYMBOL in the list of nonterminals.
Bypass checking if NOCHECK is non-nil."
  ;; Check
  (unless nocheck
    (or (wisent-ISVALID-VAR symbol)
        (error "Invalid nonterminal symbol: %S" symbol))
    (if (memq symbol var-list)
        (error "Nonterminal `%s' already defined" symbol)))
  ;; Set up properties
  (wisent-set-item-number symbol nvars)
  ;; Add
  (setq nvars (1+ nvars)
        var-list (cons symbol var-list)))

(defun wisent-parse-nonterminals (defs)
  "Parse nonterminal definitions in DEFS.
Fill in each element of the global arrays RPREC, RCODE, RUSEFUL with
respectively rule precedence level, semantic action code and
usefulness flag.  Return a list of rules of the form (LHS . RHS) where
LHS and RHS are respectively the Left Hand Side and Right Hand Side of
the rule."
  (setq rprec  nil
        rcode  nil
        nitems 0
        nrules 0)
  (let (def nonterm rlist rule rules rhs rest item items
            rhl plevel semact @n @count iactn)
    (setq @count 0)
    (while defs
      (setq def     (car defs)
            defs    (cdr defs)
            nonterm (car def)
            rlist   (cdr def)
            iactn   0)
      (or (consp rlist)
          (error "Invalid nonterminal definition syntax: %S" def))
      (while rlist
        (setq rule  (car rlist)
              rlist (cdr rlist)
              items (car rule)
              rest  (cdr rule)
              rhl   0
              rhs   nil)

        ;; Check & count items
        (setq nitems (1+ nitems)) ;; LHS item
        (while items
          (setq item (car items)
                items (cdr items)
                nitems (1+ nitems)) ;; RHS items
          (if (listp item)
              ;; Mid-rule action
              (progn
                (setq @count (1+ @count)
                      @n (intern (format "@%d" @count)))
                (wisent-push-var @n t)
                ;; Push a new empty rule with the mid-rule action
                (setq semact (vector item rhl (list nonterm iactn))
                      iactn  (1+ iactn)
                      plevel nil
                      rcode  (cons semact rcode)
                      rprec  (cons plevel rprec)
                      item   @n ;; Replace action by @N nonterminal
                      rules  (cons (list item) rules)
                      nitems (1+ nitems)
                      nrules (1+ nrules)))
            ;; Check terminal or nonterminal symbol
            (cond
             ((or (memq item token-list) (memq item var-list)))
             ;; Create new literal character token
             ((wisent-char-p item) (wisent-push-token item t))
             ((error "Symbol `%s' is used, but is not defined as a token and has no rules"
                     item))))
          (setq rhl (1+ rhl)
                rhs (cons item rhs)))

        ;; Check & collect rule precedence level
        (setq plevel (when (vectorp (car rest))
                       (setq item (car rest)
                             rest (cdr rest))
                       (if (and (= (length item) 1)
                                (memq (aref item 0) token-list)
                                (wisent-prec (aref item 0)))
                           (wisent-item-number (aref item 0))
                         (error "Invalid rule precedence level syntax: %S" item)))
              rprec (cons plevel rprec))

        ;; Check & collect semantic action body
        (setq semact (vector
                      (if rest
                          (if (cdr rest)
                              (error "Invalid semantic action syntax: %S" rest)
                            (car rest))
                        ;; Give a default semantic action body: nil
                        ;; for an empty rule or $1, the value of the
                        ;; first symbol in the rule, otherwise.
                        (if (> rhl 0) '$1 '()))
                      rhl
                      (list nonterm iactn))
              iactn  (1+ iactn)
              rcode  (cons semact rcode))
        (setq rules  (cons (cons nonterm (nreverse rhs)) rules)
              nrules (1+ nrules))))

    (setq ruseful (make-vector (1+ nrules) t)
          rprec   (vconcat (cons nil (nreverse rprec)))
          rcode   (vconcat (cons nil (nreverse rcode))))
    (nreverse rules)
    ))

(defun wisent-parse-grammar (grammar &optional start-list)
  "Parse GRAMMAR and build a suitable internal representation.
Optional argument START-LIST defines the start symbols.
GRAMMAR is a list of form: (TOKENS ASSOCS . NONTERMS)

TOKENS is a list of terminal symbols (tokens).

ASSOCS is nil or an alist of (ASSOC-TYPE . ASSOC-VALUE) elements
describing the associativity of TOKENS.  ASSOC-TYPE must be one of the
`default-prec' `nonassoc', `left' or `right' symbols.  When ASSOC-TYPE
is `default-prec', ASSOC-VALUE must be nil or t (the default).
Otherwise it is a list of tokens which must have been previously
declared in TOKENS.

NONTERMS is the list of non terminal definitions (see function
`wisent-parse-nonterminals')."
  (or (and (consp grammar) (> (length grammar) 2))
      (error "Bad input grammar"))

  (let (i r rhs pre dpre lst start-var assoc rules item
          token var def tokens defs ep-token ep-var ep-def)

    ;; Built-in tokens
    (setq ntokens 0 nvars 0)
    (wisent-push-token wisent-eoi-term t)
    (wisent-push-token wisent-error-term t)

    ;; Check/collect terminals
    (setq lst (car grammar))
    (while lst
      (wisent-push-token (car lst))
      (setq lst (cdr lst)))

    ;; Check/Set up tokens precedence & associativity
    (setq lst  (nth 1 grammar)
          pre  0
          defs nil
          dpre nil
          default-prec t)
    (while lst
      (setq def    (car lst)
            assoc  (car def)
            tokens (cdr def)
            lst    (cdr lst))
      (if (eq assoc 'default-prec)
          (progn
            (or (null (cdr tokens))
                (memq (car tokens) '(t nil))
                (error "Invalid default-prec value: %S" tokens))
            (setq default-prec (car tokens))
            (if dpre
                (message "*** redefining default-prec to %s"
                         default-prec))
            (setq dpre t))
        (or (memq assoc '(left right nonassoc))
            (error "Invalid associativity syntax: %S" assoc))
        (setq pre (1+ pre))
        (while tokens
          (setq token  (car tokens)
                tokens (cdr tokens))
          (if (memq token defs)
              (message "*** redefining precedence of `%s'" token))
          (or (memq token token-list)
              ;; Define token not previously declared.
              (wisent-push-token token))
          (setq defs (cons token defs))
          ;; Record the precedence and associativity of the terminal.
          (wisent-set-prec  token pre)
          (wisent-set-assoc token assoc))))

    ;; Check/Collect nonterminals
    (setq lst  (nthcdr 2 grammar)
          defs nil)
    (while lst
      (setq def (car lst)
            lst (cdr lst))
      (or (consp def)
          (error "Invalid nonterminal definition: %S" def))
      (if (memq (car def) token-list)
          (error "Nonterminal `%s' already defined as token" (car def)))
      (wisent-push-var (car def))
      (setq defs (cons def defs)))
    (or defs
        (error "No input grammar"))
    (setq defs (nreverse defs))

    ;; Set up the start symbol.
    (setq start-table nil)
    (cond

     ;; 1. START-LIST is nil, the start symbol is the first
     ;;    nonterminal defined in the grammar (Bison like).
     ((null start-list)
      (setq start-var (caar defs)))

     ;; 2. START-LIST contains only one element, it is the start
     ;;    symbol (Bison like).
     ((or wisent-single-start-flag (null (cdr start-list)))
      (setq start-var  (car start-list))
      (or (assq start-var defs)
          (error "Start symbol `%s' has no rule" start-var)))

     ;; 3. START-LIST contains more than one element.  All defines
     ;;    potential start symbols.  One of them (the first one by
     ;;    default) will be given at parse time to be the parser goal.
     ;;    If `wisent-single-start-flag' is non-nil that feature is
     ;;    disabled and the first nonterminal in START-LIST defines
     ;;    the start symbol, like in case 2 above.
     ((not wisent-single-start-flag)

      ;; START-LIST is a list of nonterminals '(nt0 ... ntN).
      ;; Build and push ad hoc start rules in the grammar:

      ;; ($STARTS ((nt0) $1) ((nt1) $1) ... ((ntN) $1))
      ;; ($nt1    (($$nt1 nt1) $2))
      ;; ...
      ;; ($ntN    (($$ntN ntN) $2))

      ;; Where internal symbols $ntI and $$ntI are respectively
      ;; nonterminals and terminals.

      ;; The internal start symbol $STARTS is used to build the
      ;; LALR(1) automaton.  The true default start symbol used by the
      ;; parser is the first nonterminal in START-LIST (nt0).
      (setq start-var wisent-starts-nonterm
            lst       (nreverse start-list))
      (while lst
        (setq var (car lst)
              lst (cdr lst))
        (or (memq var var-list)
            (error "Start symbol `%s' has no rule" var))
        (unless (assq var start-table) ;; Ignore duplicates
          ;; For each nt start symbol
          (setq ep-var   (intern (format "$%s"  var))
                ep-token (intern (format "$$%s" var)))
          (wisent-push-token ep-token t)
          (wisent-push-var   ep-var   t)
          (setq
           ;; Add entry (nt . $$nt) to start-table
           start-table (cons (cons var ep-token) start-table)
           ;; Add rule ($nt (($$nt nt) $2))
           defs (cons (list ep-var (list (list ep-token var) '$2)) defs)
           ;; Add start rule (($nt) $1)
           ep-def (cons (list (list ep-var) '$1) ep-def))
          ))
      (wisent-push-var start-var t)
      (setq defs (cons (cons start-var ep-def) defs))))

    ;; Set up rules main data structure & RPREC, RCODE, RUSEFUL
    (setq rules (wisent-parse-nonterminals defs))

    ;; Set up the terminal & nonterminal lists.
    (setq nsyms      (+ ntokens nvars)
          token-list (nreverse token-list)
          lst        var-list
          var-list   nil)
    (while lst
      (setq var (car lst)
            lst (cdr lst)
            var-list (cons var var-list))
      (wisent-set-item-number ;; adjust nonterminal item number to
       var (+ ntokens (wisent-item-number var)))) ;; I += NTOKENS

    ;; Store special item numbers
    (setq error-token-number (wisent-item-number wisent-error-term)
          start-symbol       (wisent-item-number start-var))

    ;; Keep symbols in the TAGS vector so that TAGS[I] is the symbol
    ;; associated to item number I.
    (setq tags (vconcat token-list var-list))
    ;; Set up RLHS RRHS & RITEM data structures from list of rules
    ;; (LHS . RHS) received from `wisent-parse-nonterminals'.
    (setq rlhs    (make-vector (1+ nrules) nil)
          rrhs    (make-vector (1+ nrules) nil)
          ritem   (make-vector (1+ nitems) nil)
          i 0
          r 1)
    (while rules
      (aset rlhs r (wisent-item-number (caar rules)))
      (aset rrhs r i)
      (setq rhs (cdar rules)
            pre nil)
      (while rhs
        (setq item (wisent-item-number (car rhs)))
        ;; Get default precedence level of rule, that is the
        ;; precedence of the last terminal in it.
        (and (wisent-ISTOKEN item)
             default-prec
             (setq pre item))

        (aset ritem i item)
        (setq i (1+ i)
              rhs (cdr rhs)))
      ;; Setup the precedence level of the rule, that is the one
      ;; specified by %prec or the default one.
      (and (not (aref rprec r)) ;; Already set by %prec
           pre
           (wisent-prec (aref tags pre))
           (aset rprec r pre))
      (aset ritem i (- r))
      (setq i (1+ i)
            r (1+ r))
      (setq rules (cdr rules)))
    ))

;;;; ---------------------
;;;; Compile input grammar
;;;; ---------------------

(defun wisent-compile-grammar (grammar &optional start-list)
  "Compile the LALR(1) GRAMMAR.

GRAMMAR is a list (TOKENS ASSOCS . NONTERMS) where:

- TOKENS is a list of terminal symbols (tokens).

- ASSOCS is nil, or an alist of (ASSOC-TYPE . ASSOC-VALUE) elements
  describing the associativity of TOKENS.  ASSOC-TYPE must be one of
  the `default-prec' `nonassoc', `left' or `right' symbols.  When
  ASSOC-TYPE is `default-prec', ASSOC-VALUE must be nil or t (the
  default).  Otherwise it is a list of tokens which must have been
  previously declared in TOKENS.

- NONTERMS is a list of nonterminal definitions.

Optional argument START-LIST specify the possible grammar start
symbols.  This is a list of nonterminals which must have been
previously declared in GRAMMAR's NONTERMS form.  By default, the start
symbol is the first nonterminal defined.  When START-LIST contains
only one element, it is the start symbol.  Otherwise, all elements are
possible start symbols, unless `wisent-single-start-flag' is non-nil.
In that case, the first element is the start symbol, and others are
ignored.

Return an automaton as a vector: [ACTIONS GOTOS STARTS FUNCTIONS]
where:

- ACTIONS is a state/token matrix telling the parser what to do at
  every state based on the current lookahead token.  That is shift,
  reduce, accept or error.

- GOTOS is a state/nonterminal matrix telling the parser the next
  state to go to after reducing with each rule.

- STARTS is an alist which maps the allowed start nonterminal symbols
  to tokens that will be first shifted into the parser stack.

- FUNCTIONS is an obarray of semantic action symbols.  Each symbol's
  function definition is the semantic action lambda expression."
  (if (wisent-automaton-p grammar)
      grammar ;; Grammar already compiled just return it
    (wisent-with-context compile-grammar
      (let* ((gc-cons-threshold 1000000))
        (garbage-collect)
	(setq wisent-new-log-flag t)
	;; Parse input grammar
	(wisent-parse-grammar grammar start-list)
	;; Generate the LALR(1) automaton
	(wisent-parser-automaton)))))

;;;; --------------------------
;;;; Byte compile input grammar
;;;; --------------------------

(require 'bytecomp)

(defun wisent-byte-compile-grammar (form)
  "Byte compile the `wisent-compile-grammar' FORM.
Automatically called by the Emacs Lisp byte compiler as a
`byte-compile' handler."
  ;; Eval the `wisent-compile-grammar' form to obtain an LALR
  ;; automaton internal data structure.  Then, because the internal
  ;; data structure contains an obarray, convert it to a lisp form so
  ;; it can be byte-compiled.
  (byte-compile-form
   ;; FIXME: we macroexpand here since `byte-compile-form' expects
   ;; macroexpanded code, but that's just a workaround: for lexical-binding
   ;; the lisp form should have to pass through closure-conversion and
   ;; `wisent-byte-compile-grammar' is called much too late for that.
   ;; Why isn't this `wisent-automaton-lisp-form' performed at
   ;; macroexpansion time?  --Stef
   (macroexpand-all
    (wisent-automaton-lisp-form (eval form)))))

;; FIXME: We shouldn't use a `byte-compile' handler.  Maybe using a hash-table
;; instead of an obarray would work around the problem that obarrays
;; aren't printable.  Then (put 'wisent-compile-grammar 'side-effect-free t).
(put 'wisent-compile-grammar 'byte-compile 'wisent-byte-compile-grammar)

(defun wisent-automaton-lisp-form (automaton)
  "Return a Lisp form that produces AUTOMATON.
See also `wisent-compile-grammar' for more details on AUTOMATON."
  (or (wisent-automaton-p automaton)
      (signal 'wrong-type-argument
              (list 'wisent-automaton-p automaton)))
  (let ((obn (make-symbol "ob"))        ; Generated obarray name
        (obv (aref automaton 3))        ; Semantic actions obarray
        )
    `(let ((,obn (make-vector 13 0)))
       ;; Generate code to initialize the semantic actions obarray,
       ;; in local variable OBN.
       ,@(let (obcode)
           (mapatoms
            #'(lambda (s)
                (setq obcode
                      (cons `(fset (intern ,(symbol-name s) ,obn)
                                   #',(symbol-function s))
                            obcode)))
            obv)
           obcode)
       ;; Generate code to create the automaton.
       (vector
        ;; In code generated to initialize the action table, take
        ;; care of symbols that are interned in the semantic actions
        ;; obarray.
        (vector
         ,@(mapcar
            #'(lambda (state) ;; for each state
                `(list
                  ,@(mapcar
                     #'(lambda (tr) ;; for each transition
                         (let ((k (car tr))  ; token
                               (a (cdr tr))) ; action
                           (if (and (symbolp a)
                                    (intern-soft (symbol-name a) obv))
                               `(cons ,(if (symbolp k) `(quote ,k) k)
                                      (intern-soft ,(symbol-name a) ,obn))
                             `(quote ,tr))))
                     state)))
            (aref automaton 0)))
        ;; The code of the goto table is unchanged.
        ,(aref automaton 1)
        ;; The code of the alist of start symbols is unchanged.
        ',(aref automaton 2)
        ;; The semantic actions obarray is in the local variable OBN.
        ,obn))))

(provide 'semantic/wisent/comp)

;;; semantic/wisent/comp.el ends here
