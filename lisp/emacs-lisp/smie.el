;;; smie.el --- Simple Minded Indentation Engine -*- lexical-binding: t -*-

;; Copyright (C) 2010-2012  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: languages, lisp, internal, parsing, indentation

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; While working on the SML indentation code, the idea grew that maybe
;; I could write something generic to do the same thing, and at the
;; end of working on the SML code, I had a pretty good idea of what it
;; could look like.  That idea grew stronger after working on
;; LaTeX indentation.
;;
;; So at some point I decided to try it out, by writing a new
;; indentation code for Coq while trying to keep most of the code
;; "table driven", where only the tables are Coq-specific.  The result
;; (which was used for Beluga-mode as well) turned out to be based on
;; something pretty close to an operator precedence parser.

;; So here is another rewrite, this time following the actual principles of
;; operator precedence grammars.  Why OPG?  Even though they're among the
;; weakest kinds of parsers, these parsers have some very desirable properties
;; for Emacs:
;; - most importantly for indentation, they work equally well in either
;;   direction, so you can use them to parse backward from the indentation
;;   point to learn the syntactic context;
;; - they work locally, so there's no need to keep a cache of
;;   the parser's state;
;; - because of that locality, indentation also works just fine when earlier
;;   parts of the buffer are syntactically incorrect since the indentation
;;   looks at "as little as possible" of the buffer to make an indentation
;;   decision.
;; - they typically have no error handling and can't even detect a parsing
;;   error, so we don't have to worry about what to do in case of a syntax
;;   error because the parser just automatically does something.  Better yet,
;;   we can afford to use a sloppy grammar.

;; A good background to understand the development (especially the parts
;; building the 2D precedence tables and then computing the precedence levels
;; from it) can be found in pages 187-194 of "Parsing techniques" by Dick Grune
;; and Ceriel Jacobs (BookBody.pdf available at
;; http://dickgrune.com/Books/PTAPG_1st_Edition/).
;;
;; OTOH we had to kill many chickens, read many coffee grounds, and practice
;; untold numbers of black magic spells, to come up with the indentation code.
;; Since then, some of that code has been beaten into submission, but the
;; smie-indent-keyword is still pretty obscure.

;; Conflict resolution:
;;
;; - One source of conflicts is when you have:
;;     (exp ("IF" exp "ELSE" exp "END") ("CASE" cases "END"))
;;     (cases (cases "ELSE" insts) ...)
;;   The IF-rule implies ELSE=END and the CASE-rule implies ELSE>END.
;;   This can be resolved simply with:
;;     (exp ("IF" expelseexp "END") ("CASE" cases "END"))
;;     (expelseexp (exp) (exp "ELSE" exp))
;;     (cases (cases "ELSE" insts) ...)
;; - Another source of conflict is when a terminator/separator is used to
;;   terminate elements at different levels, as in:
;;     (decls ("VAR" vars) (decls "," decls))
;;     (vars (id) (vars "," vars))
;;   often these can be resolved by making the lexer distinguish the two
;;   kinds of commas, e.g. based on the following token.

;; TODO & BUGS:
;;
;; - We could try to resolve conflicts such as the IFexpELSEexpEND -vs-
;;   CASE(casesELSEexp)END automatically by changing the way BNF rules such as
;;   the IF-rule is handled.  I.e. rather than IF=ELSE and ELSE=END, we could
;;   turn them into IF<ELSE and ELSE>END and IF=END.
;; - Using the structural information SMIE gives us, it should be possible to
;;   implement a `smie-align' command that would automatically figure out what
;;   there is to align and how to do it (something like: align the token of
;;   lowest precedence that appears the same number of times on all lines,
;;   and then do the same on each side of that token).
;; - Maybe accept two juxtaposed non-terminals in the BNF under the condition
;;   that the first always ends with a terminal, or that the second always
;;   starts with a terminal.
;; - Permit EBNF-style notation.
;; - If the grammar has conflicts, the only way is to make the lexer return
;;   different tokens for the different cases.  This extra work performed by
;;   the lexer can be costly and unnecessary: we perform this extra work every
;;   time we find the conflicting token, regardless of whether or not the
;;   difference between the various situations is relevant to the current
;;   situation.  E.g. we may try to determine whether a ";" is a ";-operator"
;;   or a ";-separator" in a case where we're skipping over a "begin..end" pair
;;   where the difference doesn't matter.  For frequently occurring tokens and
;;   rarely occurring conflicts, this can be a significant performance problem.
;;   We could try and let the lexer return a "set of possible tokens
;;   plus a refinement function" and then let parser call the refinement
;;   function if needed.
;; - Make it possible to better specify the behavior in the face of
;;   syntax errors.  IOW provide some control over the choice of precedence
;;   levels within the limits of the constraints.  E.g. make it possible for
;;   the grammar to specify that "begin..end" has lower precedence than
;;   "Module..EndModule", so that if a "begin" is missing, scanning from the
;;   "end" will stop at "Module" rather than going past it (and similarly,
;;   scanning from "Module" should not stop at a spurious "end").

;;; Code:

;; FIXME:
;; - smie-indent-comment doesn't interact well with mis-indented lines (where
;;   the indent rules don't do what the user wants).  Not sure what to do.

(eval-when-compile (require 'cl))

(defgroup smie nil
  "Simple Minded Indentation Engine."
  :group 'languages)

(defvar comment-continue)
(declare-function comment-string-strip "newcomment" (str beforep afterp))

;;; Building precedence level tables from BNF specs.

;; We have 4 different representations of a "grammar":
;; - a BNF table, which is a list of BNF rules of the form
;;   (NONTERM RHS1 ... RHSn) where each RHS is a list of terminals (tokens)
;;   or nonterminals.  Any element in these lists which does not appear as
;;   the `car' of a BNF rule is taken to be a terminal.
;; - A list of precedences (key word "precs"), is a list, sorted
;;   from lowest to highest precedence, of precedence classes that
;;   have the form (ASSOCIATIVITY TERMINAL1 .. TERMINALn), where
;;   ASSOCIATIVITY can be `assoc', `left', `right' or `nonassoc'.
;; - a 2 dimensional precedence table (key word "prec2"), is a 2D
;;   table recording the precedence relation (can be `<', `=', `>', or
;;   nil) between each pair of tokens.
;; - a precedence-level table (key word "grammar"), which is an alist
;;   giving for each token its left and right precedence level (a
;;   number or nil).  This is used in `smie-grammar'.
;; The prec2 tables are only intermediate data structures: the source
;; code normally provides a mix of BNF and precs tables, and then
;; turns them into a levels table, which is what's used by the rest of
;; the SMIE code.

(defvar smie-warning-count 0)

(defun smie-set-prec2tab (table x y val &optional override)
  (assert (and x y))
  (let* ((key (cons x y))
         (old (gethash key table)))
    (if (and old (not (eq old val)))
        (if (and override (gethash key override))
            ;; FIXME: The override is meant to resolve ambiguities,
            ;; but it also hides real conflicts.  It would be great to
            ;; be able to distinguish the two cases so that overrides
            ;; don't hide real conflicts.
            (puthash key (gethash key override) table)
          (display-warning 'smie (format "Conflict: %s %s/%s %s" x old val y))
          (incf smie-warning-count))
      (puthash key val table))))

(put 'smie-precs->prec2 'pure t)
(defun smie-precs->prec2 (precs)
  "Compute a 2D precedence table from a list of precedences.
PRECS should be a list, sorted by precedence (e.g. \"+\" will
come before \"*\"), of elements of the form \(left OP ...)
or (right OP ...) or (nonassoc OP ...) or (assoc OP ...).  All operators in
one of those elements share the same precedence level and associativity."
  (let ((prec2-table (make-hash-table :test 'equal)))
    (dolist (prec precs)
      (dolist (op (cdr prec))
        (let ((selfrule (cdr (assq (car prec)
                                   '((left . >) (right . <) (assoc . =))))))
          (when selfrule
            (dolist (other-op (cdr prec))
              (smie-set-prec2tab prec2-table op other-op selfrule))))
        (let ((op1 '<) (op2 '>))
          (dolist (other-prec precs)
            (if (eq prec other-prec)
                (setq op1 '> op2 '<)
              (dolist (other-op (cdr other-prec))
                (smie-set-prec2tab prec2-table op other-op op2)
                (smie-set-prec2tab prec2-table other-op op op1)))))))
    prec2-table))

(put 'smie-merge-prec2s 'pure t)
(defun smie-merge-prec2s (&rest tables)
  (if (null (cdr tables))
      (car tables)
    (let ((prec2 (make-hash-table :test 'equal)))
      (dolist (table tables)
        (maphash (lambda (k v)
                   (if (consp k)
                       (smie-set-prec2tab prec2 (car k) (cdr k) v)
                     (if (and (gethash k prec2)
                              (not (equal (gethash k prec2) v)))
                         (error "Conflicting values for %s property" k)
                       (puthash k v prec2))))
                 table))
      prec2)))

(put 'smie-bnf->prec2 'pure t)
(defun smie-bnf->prec2 (bnf &rest resolvers)
  "Convert the BNF grammar into a prec2 table.
BNF is a list of nonterminal definitions of the form:
  \(NONTERM RHS1 RHS2 ...)
where each RHS is a (non-empty) list of terminals (aka tokens) or non-terminals.
Not all grammars are accepted:
- an RHS cannot be an empty list (this is not needed, since SMIE allows all
  non-terminals to match the empty string anyway).
- an RHS cannot have 2 consecutive non-terminals: between each non-terminal
  needs to be a terminal (aka token).  This is a fundamental limitation of
  the parsing technology used (operator precedence grammar).
Additionally, conflicts can occur:
- The returned prec2 table holds constraints between pairs of
  token, and for any given pair only one constraint can be
  present, either: T1 < T2, T1 = T2, or T1 > T2.
- A token can either be an `opener' (something similar to an open-paren),
  a `closer' (like a close-paren), or `neither' of the two (e.g. an infix
  operator, or an inner token like \"else\").
Conflicts can be resolved via RESOLVERS, which is a list of elements that can
be either:
- a precs table (see `smie-precs->prec2') to resolve conflicting constraints,
- a constraint (T1 REL T2) where REL is one of = < or >."
  ;; FIXME: Add repetition operator like (repeat <separator> <elems>).
  ;; Maybe also add (or <elem1> <elem2>...) for things like
  ;; (exp (exp (or "+" "*" "=" ..) exp)).
  ;; Basically, make it EBNF (except for the specification of a separator in
  ;; the repetition, maybe).
  (let* ((nts (mapcar 'car bnf))        ;Non-terminals.
         (first-ops-table ())
         (last-ops-table ())
         (first-nts-table ())
         (last-nts-table ())
         (smie-warning-count 0)
         (prec2 (make-hash-table :test 'equal))
         (override
          (let ((precs ())
                (over (make-hash-table :test 'equal)))
            (dolist (resolver resolvers)
              (cond
               ((and (= 3 (length resolver)) (memq (nth 1 resolver) '(= < >)))
                (smie-set-prec2tab
                 over (nth 0 resolver) (nth 2 resolver) (nth 1 resolver)))
               ((memq (caar resolver) '(left right assoc nonassoc))
                (push resolver precs))
               (t (error "Unknown resolver %S" resolver))))
            (apply #'smie-merge-prec2s over
                   (mapcar 'smie-precs->prec2 precs))))
         again)
    (dolist (rules bnf)
      (let ((nt (car rules))
            (last-ops ())
            (first-ops ())
            (last-nts ())
            (first-nts ()))
        (dolist (rhs (cdr rules))
          (unless (consp rhs)
            (signal 'wrong-type-argument `(consp ,rhs)))
          (if (not (member (car rhs) nts))
              (pushnew (car rhs) first-ops)
            (pushnew (car rhs) first-nts)
            (when (consp (cdr rhs))
              ;; If the first is not an OP we add the second (which
              ;; should be an OP if BNF is an "operator grammar").
              ;; Strictly speaking, this should only be done if the
              ;; first is a non-terminal which can expand to a phrase
              ;; without any OP in it, but checking doesn't seem worth
              ;; the trouble, and it lets the writer of the BNF
              ;; be a bit more sloppy by skipping uninteresting base
              ;; cases which are terminals but not OPs.
              (when (member (cadr rhs) nts)
                (error "Adjacent non-terminals: %s %s"
                       (car rhs) (cadr rhs)))
              (pushnew (cadr rhs) first-ops)))
          (let ((shr (reverse rhs)))
            (if (not (member (car shr) nts))
                (pushnew (car shr) last-ops)
              (pushnew (car shr) last-nts)
              (when (consp (cdr shr))
                (when (member (cadr shr) nts)
                  (error "Adjacent non-terminals: %s %s"
                         (cadr shr) (car shr)))
                (pushnew (cadr shr) last-ops)))))
        (push (cons nt first-ops) first-ops-table)
        (push (cons nt last-ops) last-ops-table)
        (push (cons nt first-nts) first-nts-table)
        (push (cons nt last-nts) last-nts-table)))
    ;; Compute all first-ops by propagating the initial ones we have
    ;; now, according to first-nts.
    (setq again t)
    (while (prog1 again (setq again nil))
      (dolist (first-nts first-nts-table)
        (let* ((nt (pop first-nts))
               (first-ops (assoc nt first-ops-table)))
          (dolist (first-nt first-nts)
            (dolist (op (cdr (assoc first-nt first-ops-table)))
              (unless (member op first-ops)
                (setq again t)
                (push op (cdr first-ops))))))))
    ;; Same thing for last-ops.
    (setq again t)
    (while (prog1 again (setq again nil))
      (dolist (last-nts last-nts-table)
        (let* ((nt (pop last-nts))
               (last-ops (assoc nt last-ops-table)))
          (dolist (last-nt last-nts)
            (dolist (op (cdr (assoc last-nt last-ops-table)))
              (unless (member op last-ops)
                (setq again t)
                (push op (cdr last-ops))))))))
    ;; Now generate the 2D precedence table.
    (dolist (rules bnf)
      (dolist (rhs (cdr rules))
        (while (cdr rhs)
          (cond
           ((member (car rhs) nts)
            (dolist (last (cdr (assoc (car rhs) last-ops-table)))
              (smie-set-prec2tab prec2 last (cadr rhs) '> override)))
           ((member (cadr rhs) nts)
            (dolist (first (cdr (assoc (cadr rhs) first-ops-table)))
              (smie-set-prec2tab prec2 (car rhs) first '< override))
            (if (and (cddr rhs) (not (member (car (cddr rhs)) nts)))
                (smie-set-prec2tab prec2 (car rhs) (car (cddr rhs))
                                   '= override)))
           (t (smie-set-prec2tab prec2 (car rhs) (cadr rhs) '= override)))
          (setq rhs (cdr rhs)))))
    ;; Keep track of which tokens are openers/closer, so they can get a nil
    ;; precedence in smie-prec2->grammar.
    (puthash :smie-open/close-alist (smie-bnf--classify bnf) prec2)
    (puthash :smie-closer-alist (smie-bnf--closer-alist bnf) prec2)
    (if (> smie-warning-count 0)
        (display-warning
         'smie (format "Total: %d warnings" smie-warning-count)))
    prec2))

;; (defun smie-prec2-closer-alist (prec2 include-inners)
;;   "Build a closer-alist from a PREC2 table.
;; The return value is in the same form as `smie-closer-alist'.
;; INCLUDE-INNERS if non-nil means that inner keywords will be included
;; in the table, e.g. the table will include things like (\"if\" . \"else\")."
;;   (let* ((non-openers '())
;;          (non-closers '())
;;          ;; For each keyword, this gives the matching openers, if any.
;;          (openers (make-hash-table :test 'equal))
;;          (closers '())
;;          (done nil))
;;     ;; First, find the non-openers and non-closers.
;;     (maphash (lambda (k v)
;;                (unless (or (eq v '<) (member (cdr k) non-openers))
;;                  (push (cdr k) non-openers))
;;                (unless (or (eq v '>) (member (car k) non-closers))
;;                  (push (car k) non-closers)))
;;              prec2)
;;     ;; Then find the openers and closers.
;;     (maphash (lambda (k _)
;;                (unless (member (car k) non-openers)
;;                  (puthash (car k) (list (car k)) openers))
;;                (unless (or (member (cdr k) non-closers)
;;                            (member (cdr k) closers))
;;                  (push (cdr k) closers)))
;;              prec2)
;;     ;; Then collect the matching elements.
;;     (while (not done)
;;       (setq done t)
;;       (maphash (lambda (k v)
;;                  (when (eq v '=)
;;                    (let ((aopeners (gethash (car k) openers))
;;                          (dopeners (gethash (cdr k) openers))
;;                          (new nil))
;;                      (dolist (o aopeners)
;;                        (unless (member o dopeners)
;;                          (setq new t)
;;                          (push o dopeners)))
;;                      (when new
;;                        (setq done nil)
;;                        (puthash (cdr k) dopeners openers)))))
;;                prec2))
;;     ;; Finally, dump the resulting table.
;;     (let ((alist '()))
;;       (maphash (lambda (k v)
;;                  (when (or include-inners (member k closers))
;;                    (dolist (opener v)
;;                      (unless (equal opener k)
;;                        (push (cons opener k) alist)))))
;;                openers)
;;       alist)))

(defun smie-bnf--closer-alist (bnf &optional no-inners)
  ;; We can also build this closer-alist table from a prec2 table,
  ;; but it takes more work, and the order is unpredictable, which
  ;; is a problem for smie-close-block.
  ;; More convenient would be to build it from a levels table since we
  ;; always have this table (contrary to the BNF), but it has all the
  ;; disadvantages of the prec2 case plus the disadvantage that the levels
  ;; table has lost some info which would result in extra invalid pairs.
  "Build a closer-alist from a BNF table.
The return value is in the same form as `smie-closer-alist'.
NO-INNERS if non-nil means that inner keywords will be excluded
from the table, e.g. the table will not include things like (\"if\" . \"else\")."
  (let ((nts (mapcar #'car bnf))        ;non terminals.
        (alist '()))
    (dolist (nt bnf)
      (dolist (rhs (cdr nt))
        (unless (or (< (length rhs) 2) (member (car rhs) nts))
          (if no-inners
              (let ((last (car (last rhs))))
                (unless (member last nts)
                  (pushnew (cons (car rhs) last) alist :test #'equal)))
            ;; Reverse so that the "real" closer gets there first,
            ;; which is important for smie-close-block.
            (dolist (term (reverse (cdr rhs)))
              (unless (member term nts)
                (pushnew (cons (car rhs) term) alist :test #'equal)))))))
    (nreverse alist)))

(defun smie-bnf--set-class (table token class)
  (let ((prev (gethash token table class)))
    (puthash token
             (cond
              ((eq prev class) class)
              ((eq prev t) t) ;Non-terminal.
              (t (display-warning
                  'smie
                  (format "token %s is both %s and %s" token class prev))
                 'neither))
             table)))

(defun smie-bnf--classify (bnf)
  "Return a table classifying terminals.
Each terminal can either be an `opener', a `closer', or `neither'."
  (let ((table (make-hash-table :test #'equal))
        (alist '()))
    (dolist (category bnf)
      (puthash (car category) t table)) ;Mark non-terminals.
    (dolist (category bnf)
      (dolist (rhs (cdr category))
        (if (null (cdr rhs))
            (smie-bnf--set-class table (pop rhs) 'neither)
          (smie-bnf--set-class table (pop rhs) 'opener)
          (while (cdr rhs)              ;Remove internals.
            (smie-bnf--set-class table (pop rhs) 'neither))
          (smie-bnf--set-class table (pop rhs) 'closer))))
    (maphash (lambda (tok v)
               (when (memq v '(closer opener))
                 (push (cons tok v) alist)))
             table)
    alist))

(defun smie-debug--prec2-cycle (csts)
  "Return a cycle in CSTS, assuming there's one.
CSTS is a list of pairs representing arcs in a graph."
  ;; A PATH is of the form (START . REST) where REST is a reverse
  ;; list of nodes through which the path goes.
  (let ((paths (mapcar (lambda (pair) (list (car pair) (cdr pair))) csts))
        (cycle nil))
    (while (null cycle)
      (dolist (path (prog1 paths (setq paths nil)))
        (dolist (cst csts)
          (when (eq (car cst) (nth 1 path))
            (if (eq (cdr cst) (car path))
                (setq cycle path)
              (push (cons (car path) (cons (cdr cst) (cdr path)))
                    paths))))))
    (cons (car cycle) (nreverse (cdr cycle)))))

(defun smie-debug--describe-cycle (table cycle)
  (let ((names
         (mapcar (lambda (val)
                   (let ((res nil))
                     (dolist (elem table)
                       (if (eq (cdr elem) val)
                           (push (concat "." (car elem)) res))
                       (if (eq (cddr elem) val)
                           (push (concat (car elem) ".") res)))
                     (assert res)
                     res))
                 cycle)))
    (mapconcat
     (lambda (elems) (mapconcat 'identity elems "="))
     (append names (list (car names)))
     " < ")))

;; (defun smie-check-grammar (grammar prec2 &optional dummy)
;;   (maphash (lambda (k v)
;;              (when (consp k)
;;                (let ((left (nth 2 (assoc (car k) grammar)))
;;                      (right (nth 1 (assoc (cdr k) grammar))))
;;                  (when (and left right)
;;                    (cond
;;                     ((< left right) (assert (eq v '<)))
;;                     ((> left right) (assert (eq v '>)))
;;                     (t (assert (eq v '=))))))))
;;            prec2))

(put 'smie-prec2->grammar 'pure t)
(defun smie-prec2->grammar (prec2)
  "Take a 2D precedence table and turn it into an alist of precedence levels.
PREC2 is a table as returned by `smie-precs->prec2' or
`smie-bnf->prec2'."
  ;; For each operator, we create two "variables" (corresponding to
  ;; the left and right precedence level), which are represented by
  ;; cons cells.  Those are the very cons cells that appear in the
  ;; final `table'.  The value of each "variable" is kept in the `car'.
  (let ((table ())
        (csts ())
        (eqs ())
        tmp x y)
    ;; From `prec2' we construct a list of constraints between
    ;; variables (aka "precedence levels").  These can be either
    ;; equality constraints (in `eqs') or `<' constraints (in `csts').
    (maphash (lambda (k v)
               (when (consp k)
                 (if (setq tmp (assoc (car k) table))
                     (setq x (cddr tmp))
                   (setq x (cons nil nil))
                   (push (cons (car k) (cons nil x)) table))
                 (if (setq tmp (assoc (cdr k) table))
                     (setq y (cdr tmp))
                   (setq y (cons nil (cons nil nil)))
                   (push (cons (cdr k) y) table))
                 (ecase v
                   (= (push (cons x y) eqs))
                   (< (push (cons x y) csts))
                   (> (push (cons y x) csts)))))
             prec2)
    ;; First process the equality constraints.
    (let ((eqs eqs))
      (while eqs
        (let ((from (caar eqs))
              (to (cdar eqs)))
          (setq eqs (cdr eqs))
          (if (eq to from)
              nil                       ;Nothing to do.
            (dolist (other-eq eqs)
              (if (eq from (cdr other-eq)) (setcdr other-eq to))
              (when (eq from (car other-eq))
                ;; This can happen because of `assoc' settings in precs
                ;; or because of a rhs like ("op" foo "op").
                (setcar other-eq to)))
            (dolist (cst csts)
              (if (eq from (cdr cst)) (setcdr cst to))
              (if (eq from (car cst)) (setcar cst to)))))))
    ;; Then eliminate trivial constraints iteratively.
    (let ((i 0))
      (while csts
        (let ((rhvs (mapcar 'cdr csts))
              (progress nil))
          (dolist (cst csts)
            (unless (memq (car cst) rhvs)
              (setq progress t)
              ;; We could give each var in a given iteration the same value,
              ;; but we can also give them arbitrarily different values.
              ;; Basically, these are vars between which there is no
              ;; constraint (neither equality nor inequality), so
              ;; anything will do.
              ;; We give them arbitrary values, which means that we
              ;; replace the "no constraint" case with either > or <
              ;; but not =.  The reason we do that is so as to try and
              ;; distinguish associative operators (which will have
              ;; left = right).
              (unless (caar cst)
                (setcar (car cst) i)
                ;; (smie-check-grammar table prec2 'step1)
                (incf i))
              (setq csts (delq cst csts))))
          (unless progress
            (error "Can't resolve the precedence cycle: %s"
                   (smie-debug--describe-cycle
                    table (smie-debug--prec2-cycle csts)))))
        (incf i 10))
      ;; Propagate equality constraints back to their sources.
      (dolist (eq (nreverse eqs))
        (when (null (cadr eq))
          ;; There's an equality constraint, but we still haven't given
          ;; it a value: that means it binds tighter than anything else,
          ;; and it can't be an opener/closer (those don't have equality
          ;; constraints).
          ;; So set it here rather than below since doing it below
          ;; makes it more difficult to obey the equality constraints.
          (setcar (cdr eq) i)
          (incf i))
        (assert (or (null (caar eq)) (eq (caar eq) (cadr eq))))
        (setcar (car eq) (cadr eq))
        ;; (smie-check-grammar table prec2 'step2)
        )
      ;; Finally, fill in the remaining vars (which did not appear on the
      ;; left side of any < constraint).
      (dolist (x table)
        (unless (nth 1 x)
          (setf (nth 1 x) i)
          (incf i))                     ;See other (incf i) above.
        (unless (nth 2 x)
          (setf (nth 2 x) i)
          (incf i))))                   ;See other (incf i) above.
    ;; Mark closers and openers.
    (dolist (x (gethash :smie-open/close-alist prec2))
      (let* ((token (car x))
             (cons (case (cdr x)
                     (closer (cddr (assoc token table)))
                     (opener (cdr (assoc token table))))))
        (assert (numberp (car cons)))
        (setf (car cons) (list (car cons)))))
    (let ((ca (gethash :smie-closer-alist prec2)))
      (when ca (push (cons :smie-closer-alist ca) table)))
    ;; (smie-check-grammar table prec2 'step3)
    table))

;;; Parsing using a precedence level table.

(defvar smie-grammar 'unset
  "List of token parsing info.
This list is normally built by `smie-prec2->grammar'.
Each element is of the form (TOKEN LEFT-LEVEL RIGHT-LEVEL).
Parsing is done using an operator precedence parser.
LEFT-LEVEL and RIGHT-LEVEL can be either numbers or a list, where a list
means that this operator does not bind on the corresponding side,
e.g. a LEFT-LEVEL of nil means this is a token that behaves somewhat like
an open-paren, whereas a RIGHT-LEVEL of nil would correspond to something
like a close-paren.")

(defvar smie-forward-token-function 'smie-default-forward-token
  "Function to scan forward for the next token.
Called with no argument should return a token and move to its end.
If no token is found, return nil or the empty string.
It can return nil when bumping into a parenthesis, which lets SMIE
use syntax-tables to handle them in efficient C code.")

(defvar smie-backward-token-function 'smie-default-backward-token
  "Function to scan backward the previous token.
Same calling convention as `smie-forward-token-function' except
it should move backward to the beginning of the previous token.")

(defalias 'smie-op-left 'car)
(defalias 'smie-op-right 'cadr)

(defun smie-default-backward-token ()
  (forward-comment (- (point)))
  (buffer-substring-no-properties
   (point)
   (progn (if (zerop (skip-syntax-backward "."))
              (skip-syntax-backward "w_'"))
          (point))))

(defun smie-default-forward-token ()
  (forward-comment (point-max))
  (buffer-substring-no-properties
   (point)
   (progn (if (zerop (skip-syntax-forward "."))
              (skip-syntax-forward "w_'"))
          (point))))

(defun smie--associative-p (toklevels)
  ;; in "a + b + c" we want to stop at each +, but in
  ;; "if a then b elsif c then d else c" we don't want to stop at each keyword.
  ;; To distinguish the two cases, we made smie-prec2->grammar choose
  ;; different levels for each part of "if a then b else c", so that
  ;; by checking if the left-level is equal to the right level, we can
  ;; figure out that it's an associative operator.
  ;; This is not 100% foolproof, tho, since the "elsif" will have to have
  ;; equal left and right levels (since it's optional), so smie-next-sexp
  ;; has to be careful to distinguish those different cases.
  (eq (smie-op-left toklevels) (smie-op-right toklevels)))

(defun smie-next-sexp (next-token next-sexp op-forw op-back halfsexp)
  "Skip over one sexp.
NEXT-TOKEN is a function of no argument that moves forward by one
token (after skipping comments if needed) and returns it.
NEXT-SEXP is a lower-level function to skip one sexp.
OP-FORW is the accessor to the forward level of the level data.
OP-BACK is the accessor to the backward level of the level data.
HALFSEXP if non-nil, means skip over a partial sexp if needed.  I.e. if the
first token we see is an operator, skip over its left-hand-side argument.
HALFSEXP can also be a token, in which case it means to parse as if
we had just successfully passed this token.
Possible return values:
  (FORW-LEVEL POS TOKEN): we couldn't skip TOKEN because its back-level
    is too high.  FORW-LEVEL is the forw-level of TOKEN,
    POS is its start position in the buffer.
  (t POS TOKEN): same thing when we bump on the wrong side of a paren.
  (nil POS TOKEN): we skipped over a paren-like pair.
  nil: we skipped over an identifier, matched parentheses, ..."
  (catch 'return
    (let ((levels
           (if (stringp halfsexp)
               (prog1 (list (cdr (assoc halfsexp smie-grammar)))
                 (setq halfsexp nil)))))
      (while
          (let* ((pos (point))
                 (token (funcall next-token))
                 (toklevels (cdr (assoc token smie-grammar))))
            (cond
             ((null toklevels)
              (when (zerop (length token))
                (condition-case err
                    (progn (goto-char pos) (funcall next-sexp 1) nil)
                  (scan-error (throw 'return
                                     (list t (caddr err)
                                           (buffer-substring-no-properties
                                            (caddr err)
                                            (+ (caddr err)
                                               (if (< (point) (caddr err))
                                                   -1 1)))))))
                (if (eq pos (point))
                    ;; We did not move, so let's abort the loop.
                    (throw 'return (list t (point))))))
             ((not (numberp (funcall op-back toklevels)))
              ;; A token like a paren-close.
              (assert (numberp     ; Otherwise, why mention it in smie-grammar.
                       (funcall op-forw toklevels)))
              (push toklevels levels))
             (t
              (while (and levels (< (funcall op-back toklevels)
                                    (funcall op-forw (car levels))))
                (setq levels (cdr levels)))
              (cond
               ((null levels)
                (if (and halfsexp (numberp (funcall op-forw toklevels)))
                    (push toklevels levels)
                  (throw 'return
                         (prog1 (list (or (car toklevels) t) (point) token)
                           (goto-char pos)))))
               (t
                (let ((lastlevels levels))
                  (if (and levels (= (funcall op-back toklevels)
                                     (funcall op-forw (car levels))))
                      (setq levels (cdr levels)))
                  ;; We may have found a match for the previously pending
                  ;; operator.  Is this the end?
                  (cond
                   ;; Keep looking as long as we haven't matched the
                   ;; topmost operator.
                   (levels
                    (cond
                     ((numberp (funcall op-forw toklevels))
                      (push toklevels levels))
                     ;; FIXME: For some languages, we can express the grammar
                     ;; OK, but next-sexp doesn't stop where we'd want it to.
                     ;; E.g. in SML, we'd want to stop right in front of
                     ;; "local" if we're scanning (both forward and backward)
                     ;; from a "val/fun/..." at the same level.
                     ;; Same for Pascal/Modula2's "procedure" w.r.t
                     ;; "type/var/const".
                     ;;
                     ;; ((and (functionp (cadr (funcall op-forw toklevels)))
                     ;;       (funcall (cadr (funcall op-forw toklevels))
                     ;;                levels))
                     ;;  (setq levels nil))
                     ))
                   ;; We matched the topmost operator.  If the new operator
                   ;; is the last in the corresponding BNF rule, we're done.
                   ((not (numberp (funcall op-forw toklevels)))
                    ;; It is the last element, let's stop here.
                    (throw 'return (list nil (point) token)))
                   ;; If the new operator is not the last in the BNF rule,
                   ;; and is not associative, it's one of the inner operators
                   ;; (like the "in" in "let .. in .. end"), so keep looking.
                   ((not (smie--associative-p toklevels))
                    (push toklevels levels))
                   ;; The new operator is associative.  Two cases:
                   ;; - it's really just an associative operator (like + or ;)
                   ;;   in which case we should have stopped right before.
                   ((and lastlevels
                         (smie--associative-p (car lastlevels)))
                    (throw 'return
                           (prog1 (list (or (car toklevels) t) (point) token)
                             (goto-char pos))))
                   ;; - it's an associative operator within a larger construct
                   ;;   (e.g. an "elsif"), so we should just ignore it and keep
                   ;;   looking for the closing element.
                   (t (setq levels lastlevels))))))))
            levels)
        (setq halfsexp nil)))))

(defun smie-backward-sexp (&optional halfsexp)
  "Skip over one sexp.
HALFSEXP if non-nil, means skip over a partial sexp if needed.  I.e. if the
first token we see is an operator, skip over its left-hand-side argument.
HALFSEXP can also be a token, in which case we should skip the text
assuming it is the left-hand-side argument of that token.
Possible return values:
  (LEFT-LEVEL POS TOKEN): we couldn't skip TOKEN because its right-level
    is too high.  LEFT-LEVEL is the left-level of TOKEN,
    POS is its start position in the buffer.
  (t POS TOKEN): same thing but for an open-paren or the beginning of buffer.
  (nil POS TOKEN): we skipped over a paren-like pair.
  nil: we skipped over an identifier, matched parentheses, ..."
  (smie-next-sexp
   (indirect-function smie-backward-token-function)
   (indirect-function 'backward-sexp)
   (indirect-function 'smie-op-left)
   (indirect-function 'smie-op-right)
   halfsexp))

(defun smie-forward-sexp (&optional halfsexp)
  "Skip over one sexp.
HALFSEXP if non-nil, means skip over a partial sexp if needed.  I.e. if the
first token we see is an operator, skip over its right-hand-side argument.
HALFSEXP can also be a token, in which case we should skip the text
assuming it is the right-hand-side argument of that token.
Possible return values:
  (RIGHT-LEVEL POS TOKEN): we couldn't skip TOKEN because its left-level
    is too high.  RIGHT-LEVEL is the right-level of TOKEN,
    POS is its end position in the buffer.
  (t POS TOKEN): same thing but for an open-paren or the beginning of buffer.
  (nil POS TOKEN): we skipped over a paren-like pair.
  nil: we skipped over an identifier, matched parentheses, ..."
  (smie-next-sexp
   (indirect-function smie-forward-token-function)
   (indirect-function 'forward-sexp)
   (indirect-function 'smie-op-right)
   (indirect-function 'smie-op-left)
   halfsexp))

;;; Miscellaneous commands using the precedence parser.

(defun smie-backward-sexp-command (&optional n)
  "Move backward through N logical elements."
  (interactive "^p")
  (smie-forward-sexp-command (- n)))

(defun smie-forward-sexp-command (&optional n)
  "Move forward through N logical elements."
  (interactive "^p")
  (let ((forw (> n 0))
        (forward-sexp-function nil))
    (while (/= n 0)
      (setq n (- n (if forw 1 -1)))
      (let ((pos (point))
            (res (if forw
                     (smie-forward-sexp 'halfsexp)
                   (smie-backward-sexp 'halfsexp))))
        (if (and (car res) (= pos (point)) (not (if forw (eobp) (bobp))))
            (signal 'scan-error
                    (list "Containing expression ends prematurely"
                          (cadr res) (cadr res)))
          nil)))))

(defvar smie-closer-alist nil
  "Alist giving the closer corresponding to an opener.")

(defun smie-close-block ()
  "Close the closest surrounding block."
  (interactive)
  (let ((closer
         (save-excursion
           (backward-up-list 1)
           (if (looking-at "\\s(")
               (string (cdr (syntax-after (point))))
             (let* ((open (funcall smie-forward-token-function))
                    (closer (cdr (assoc open smie-closer-alist)))
                    (levels (list (assoc open smie-grammar)))
                    (seen '())
                    (found '()))
               (cond
                ;; Even if we improve the auto-computation of closers,
                ;; there are still cases where we need manual
                ;; intervention, e.g. for Octave's use of `until'
                ;; as a pseudo-closer of `do'.
                (closer)
                ((or (equal levels '(nil)) (numberp (nth 1 (car levels))))
                 (error "Doesn't look like a block"))
                (t
                 ;; Now that smie-setup automatically sets smie-closer-alist
                 ;; from the BNF, this is not really needed any more.
                 (while levels
                   (let ((level (pop levels)))
                     (dolist (other smie-grammar)
                       (when (and (eq (nth 2 level) (nth 1 other))
                                  (not (memq other seen)))
                         (push other seen)
                         (if (numberp (nth 2 other))
                             (push other levels)
                           (push (car other) found))))))
                 (cond
                  ((null found) (error "No known closer for opener %s" open))
                  ;; What should we do if there are various closers?
                  (t (car found))))))))))
    (unless (save-excursion (skip-chars-backward " \t") (bolp))
      (newline))
    (insert closer)
    (if (save-excursion (skip-chars-forward " \t") (eolp))
        (indent-according-to-mode)
      (reindent-then-newline-and-indent))))

(defun smie-down-list (&optional arg)
  "Move forward down one level paren-like blocks.  Like `down-list'.
With argument ARG, do this that many times.
A negative argument means move backward but still go down a level.
This command assumes point is not in a string or comment."
  (interactive "p")
  (let ((start (point))
        (inc (if (< arg 0) -1 1))
        (offset (if (< arg 0) 1 0))
        (next-token (if (< arg 0)
                        smie-backward-token-function
                      smie-forward-token-function)))
    (while (/= arg 0)
      (setq arg (- arg inc))
      (while
          (let* ((pos (point))
                 (token (funcall next-token))
                 (levels (assoc token smie-grammar)))
            (cond
             ((zerop (length token))
              (if (if (< inc 0) (looking-back "\\s(\\|\\s)" (1- (point)))
                    (looking-at "\\s(\\|\\s)"))
                  ;; Go back to `start' in case of an error.  This presumes
                  ;; none of the token we've found until now include a ( or ).
                  (progn (goto-char start) (down-list inc) nil)
                (forward-sexp inc)
                (/= (point) pos)))
             ((and levels (not (numberp (nth (+ 1 offset) levels)))) nil)
             ((and levels (not (numberp (nth (- 2 offset) levels))))
              (let ((end (point)))
                (goto-char start)
                (signal 'scan-error
                        (list "Containing expression ends prematurely"
                              pos end))))
             (t)))))))

(defvar smie-blink-matching-triggers '(?\s ?\n)
  "Chars which might trigger `blink-matching-open'.
These can include the final chars of end-tokens, or chars that are
typically inserted right after an end token.
I.e. a good choice can be:
    (delete-dups
     (mapcar (lambda (kw) (aref (cdr kw) (1- (length (cdr kw)))))
             smie-closer-alist))")

(defcustom smie-blink-matching-inners t
  "Whether SMIE should blink to matching opener for inner keywords.
If non-nil, it will blink not only for \"begin..end\" but also for \"if...else\"."
  :type 'boolean
  :group 'smie)

(defun smie-blink-matching-check (start end)
  (save-excursion
    (goto-char end)
    (let ((ender (funcall smie-backward-token-function)))
      (cond
       ((not (and ender (rassoc ender smie-closer-alist)))
        ;; This not is one of the begin..end we know how to check.
        (blink-matching-check-mismatch start end))
       ((not start) t)
       ((eq t (car (rassoc ender smie-closer-alist))) nil)
       (t
        (goto-char start)
        (let ((starter (funcall smie-forward-token-function)))
          (not (member (cons starter ender) smie-closer-alist))))))))

(defun smie-blink-matching-open ()
  "Blink the matching opener when applicable.
This uses SMIE's tables and is expected to be placed on `post-self-insert-hook'."
  (let ((pos (point))                   ;Position after the close token.
        token)
    (when (and blink-matching-paren
               smie-closer-alist                     ; Optimization.
               (or (eq (char-before) last-command-event) ;; Sanity check.
                   (save-excursion
                     (or (progn (skip-chars-backward " \t")
                                (setq pos (point))
                                (eq (char-before) last-command-event))
                         (progn (skip-chars-backward " \n\t")
                                (setq pos (point))
                                (eq (char-before) last-command-event)))))
               (memq last-command-event smie-blink-matching-triggers)
               (not (nth 8 (syntax-ppss))))
      (save-excursion
        (setq token (funcall smie-backward-token-function))
        (when (and (eq (point) (1- pos))
                   (= 1 (length token))
                   (not (rassoc token smie-closer-alist)))
          ;; The trigger char is itself a token but is not one of the
          ;; closers (e.g. ?\; in Octave mode), so go back to the
          ;; previous token.
          (setq pos (point))
          (setq token (funcall smie-backward-token-function)))
        (when (rassoc token smie-closer-alist)
          ;; We're after a close token.  Let's still make sure we
          ;; didn't skip a comment to find that token.
          (funcall smie-forward-token-function)
          (when (and (save-excursion
                       ;; Skip the trigger char, if applicable.
                       (if (eq (char-after) last-command-event)
                           (forward-char 1))
                       (if (eq ?\n last-command-event)
                           ;; Skip any auto-indentation, if applicable.
                           (skip-chars-forward " \t"))
                       (>= (point) pos))
                     ;; If token ends with a trigger char, don't blink for
                     ;; anything else than this trigger char, lest we'd blink
                     ;; both when inserting the trigger char and when
                     ;; inserting a subsequent trigger char like SPC.
                     (or (eq (char-before) last-command-event)
                         (not (memq (char-before)
                                    smie-blink-matching-triggers)))
                     (or smie-blink-matching-inners
                         (not (numberp (nth 2 (assoc token smie-grammar))))))
            ;; The major mode might set blink-matching-check-function
            ;; buffer-locally so that interactive calls to
            ;; blink-matching-open work right, but let's not presume
            ;; that's the case.
            (let ((blink-matching-check-function #'smie-blink-matching-check))
              (blink-matching-open))))))))

;;; The indentation engine.

(defcustom smie-indent-basic 4
  "Basic amount of indentation."
  :type 'integer
  :group 'smie)

(defvar smie-rules-function 'ignore
  "Function providing the indentation rules.
It takes two arguments METHOD and ARG where the meaning of ARG
and the expected return value depends on METHOD.
METHOD can be:
- :after, in which case ARG is a token and the function should return the
  OFFSET to use for indentation after ARG.
- :before, in which case ARG is a token and the function should return the
  OFFSET to use to indent ARG itself.
- :elem, in which case the function should return either:
  - the offset to use to indent function arguments (ARG = `arg')
  - the basic indentation step (ARG = `basic').
- :list-intro, in which case ARG is a token and the function should return
  non-nil if TOKEN is followed by a list of expressions (not separated by any
  token) rather than an expression.

When ARG is a token, the function is called with point just before that token.
A return value of nil always means to fallback on the default behavior, so the
function should return nil for arguments it does not expect.

OFFSET can be:
nil				use the default indentation rule.
\(column . COLUMN)		indent to column COLUMN.
NUMBER				offset by NUMBER, relative to a base token
				which is the current token for :after and
				its parent for :before.

The functions whose name starts with \"smie-rule-\" are helper functions
designed specifically for use in this function.")

(defalias 'smie-rule-hanging-p 'smie-indent--hanging-p)
(defun smie-indent--hanging-p ()
  "Return non-nil if the current token is \"hanging\".
A hanging keyword is one that's at the end of a line except it's not at
the beginning of a line."
  (and (not (smie-indent--bolp))
       (save-excursion
         (<= (line-end-position)
             (progn
               (when (zerop (length (funcall smie-forward-token-function)))
                 ;; Could be an open-paren.
                 (forward-char 1))
               (skip-chars-forward " \t")
               (or (eolp)
                   (and (looking-at comment-start-skip)
                        (forward-comment (point-max))))
               (point))))))

(defalias 'smie-rule-bolp 'smie-indent--bolp)
(defun smie-indent--bolp ()
  "Return non-nil if the current token is the first on the line."
  (save-excursion (skip-chars-backward " \t") (bolp)))

;; Dynamically scoped.
(defvar smie--parent) (defvar smie--after) (defvar smie--token)

(defun smie-indent--parent ()
  (or smie--parent
      (save-excursion
        (let* ((pos (point))
               (tok (funcall smie-forward-token-function)))
          (unless (numberp (cadr (assoc tok smie-grammar)))
            (goto-char pos))
          (setq smie--parent
                (or (smie-backward-sexp 'halfsexp)
                    (let (res)
                      (while (null (setq res (smie-backward-sexp))))
                      (list nil (point) (nth 2 res)))))))))

(defun smie-rule-parent-p (&rest parents)
  "Return non-nil if the current token's parent is among PARENTS.
Only meaningful when called from within `smie-rules-function'."
  (member (nth 2 (smie-indent--parent)) parents))

(defun smie-rule-next-p (&rest tokens)
  "Return non-nil if the next token is among TOKENS.
Only meaningful when called from within `smie-rules-function'."
  (let ((next
         (save-excursion
           (unless smie--after
             (smie-indent-forward-token) (setq smie--after (point)))
           (goto-char smie--after)
           (smie-indent-forward-token))))
    (member (car next) tokens)))

(defun smie-rule-prev-p (&rest tokens)
  "Return non-nil if the previous token is among TOKENS."
  (let ((prev (save-excursion
                (smie-indent-backward-token))))
    (member (car prev) tokens)))

(defun smie-rule-sibling-p ()
  "Return non-nil if the parent is actually a sibling.
Only meaningful when called from within `smie-rules-function'."
  (eq (car (smie-indent--parent))
      (cadr (assoc smie--token smie-grammar))))

(defun smie-rule-parent (&optional offset)
  "Align with parent.
If non-nil, OFFSET should be an integer giving an additional offset to apply.
Only meaningful when called from within `smie-rules-function'."
  (save-excursion
    (goto-char (cadr (smie-indent--parent)))
    (cons 'column
          (+ (or offset 0)
             ;; Use smie-indent-virtual when indenting relative to an opener:
             ;; this will also by default use current-column unless
             ;; that opener is hanging, but will additionally consult
             ;; rules-function, so it gives it a chance to tweak
             ;; indentation (e.g. by forcing indentation relative to
             ;; its own parent, as in fn a => fn b => fn c =>).
             (if (or (listp (car smie--parent)) (smie-indent--hanging-p))
                 (smie-indent-virtual) (current-column))))))

(defvar smie-rule-separator-outdent 2)

(defun smie-indent--separator-outdent ()
  ;; FIXME: Here we actually have several reasonable behaviors.
  ;; E.g. for a parent token of "FOO" and a separator ";" we may want to:
  ;; 1- left-align ; with FOO.
  ;; 2- right-align ; with FOO.
  ;; 3- align content after ; with content after FOO.
  ;; 4- align content plus add/remove spaces so as to align ; with FOO.
  ;; Currently, we try to align the contents (option 3) which actually behaves
  ;; just like option 2 (if the number of spaces after FOO and ; is equal).
  (let ((afterpos (save-excursion
                    (let ((tok (funcall smie-forward-token-function)))
                      (unless tok
                        (with-demoted-errors
                          (error "smie-rule-separator: can't skip token %s"
                                 smie--token))))
                    (skip-chars-forward " ")
                    (unless (eolp) (point)))))
    (or (and afterpos
             ;; This should always be true, unless
             ;; smie-forward-token-function skipped a \n.
             (< afterpos (line-end-position))
             (- afterpos (point)))
        smie-rule-separator-outdent)))

(defun smie-rule-separator (method)
  "Indent current token as a \"separator\".
By \"separator\", we mean here a token whose sole purpose is to separate
various elements within some enclosing syntactic construct, and which does
not have any semantic significance in itself (i.e. it would typically no exist
as a node in an abstract syntax tree).
Such a token is expected to have an associative syntax and be closely tied
to its syntactic parent.  Typical examples are \",\" in lists of arguments
\(enclosed inside parentheses), or \";\" in sequences of instructions (enclosed
in a {..} or begin..end block).
METHOD should be the method name that was passed to `smie-rules-function'.
Only meaningful when called from within `smie-rules-function'."
  ;; FIXME: The code below works OK for cases where the separators
  ;; are placed consistently always at beginning or always at the end,
  ;; but not if some are at the beginning and others are at the end.
  ;; I.e. it gets confused in cases such as:
  ;;     (  a
  ;;     ,  a,
  ;;        b
  ;;     ,  c,
  ;;        d
  ;;     )
  ;;
  ;; Assuming token is associative, the default rule for associative
  ;; tokens (which assumes an infix operator) works fine for many cases.
  ;; We mostly need to take care of the case where token is at beginning of
  ;; line, in which case we want to align it with its enclosing parent.
  (cond
   ((and (eq method :before) (smie-rule-bolp) (not (smie-rule-sibling-p)))
    (let ((parent-col (cdr (smie-rule-parent)))
          (parent-pos-col     ;FIXME: we knew this when computing smie--parent.
           (save-excursion
             (goto-char (cadr smie--parent))
             (smie-indent-forward-token)
             (forward-comment (point-max))
             (current-column))))
      (cons 'column
            (max parent-col
                 (min parent-pos-col
                      (- parent-pos-col (smie-indent--separator-outdent)))))))
   ((and (eq method :after) (smie-indent--bolp))
    (smie-indent--separator-outdent))))

(defun smie-indent--offset (elem)
  (or (funcall smie-rules-function :elem elem)
      (if (not (eq elem 'basic))
          (funcall smie-rules-function :elem 'basic))
      smie-indent-basic))

(defun smie-indent--rule (method token
                          ;; FIXME: Too many parameters.
                          &optional after parent base-pos)
  "Compute indentation column according to `indent-rule-functions'.
METHOD and TOKEN are passed to `indent-rule-functions'.
AFTER is the position after TOKEN, if known.
PARENT is the parent info returned by `smie-backward-sexp', if known.
BASE-POS is the position relative to which offsets should be applied."
  ;; This is currently called in 3 cases:
  ;; - :before opener, where rest=nil but base-pos could as well be parent.
  ;; - :before other, where
  ;;                  ; after=nil
  ;;                  ; parent is set
  ;;                  ; base-pos=parent
  ;; - :after tok, where
  ;;                  ; after is set; parent=nil; base-pos=point;
  (save-excursion
    (let ((offset
           (let ((smie--parent parent)
                 (smie--token token)
                 (smie--after after))
             (funcall smie-rules-function method token))))
      (cond
       ((not offset) nil)
       ((eq (car-safe offset) 'column) (cdr offset))
       ((integerp offset)
        (+ offset
           (if (null base-pos) 0
             (goto-char base-pos)
             ;; Use smie-indent-virtual when indenting relative to an opener:
             ;; this will also by default use current-column unless
             ;; that opener is hanging, but will additionally consult
             ;; rules-function, so it gives it a chance to tweak indentation
             ;; (e.g. by forcing indentation relative to its own parent, as in
             ;; fn a => fn b => fn c =>).
             ;; When parent==nil it doesn't matter because the only case
             ;; where it's really used is when the base-pos is hanging anyway.
             (if (or (and parent (null (car parent)))
                     (smie-indent--hanging-p))
                 (smie-indent-virtual) (current-column)))))
       (t (error "Unknown indentation offset %s" offset))))))

(defun smie-indent-forward-token ()
  "Skip token forward and return it, along with its levels."
  (let ((tok (funcall smie-forward-token-function)))
    (cond
     ((< 0 (length tok)) (assoc tok smie-grammar))
     ((looking-at "\\s(\\|\\s)\\(\\)")
      (forward-char 1)
      (cons (buffer-substring (1- (point)) (point))
            (if (match-end 1) '(0 nil) '(nil 0)))))))

(defun smie-indent-backward-token ()
  "Skip token backward and return it, along with its levels."
  (let ((tok (funcall smie-backward-token-function))
        class)
    (cond
     ((< 0 (length tok)) (assoc tok smie-grammar))
     ;; 4 == open paren syntax, 5 == close.
     ((memq (setq class (syntax-class (syntax-after (1- (point))))) '(4 5))
      (forward-char -1)
      (cons (buffer-substring (point) (1+ (point)))
            (if (eq class 4) '(nil 0) '(0 nil)))))))

(defun smie-indent-virtual ()
  ;; We used to take an optional arg (with value :not-hanging) to specify that
  ;; we should only use (smie-indent-calculate) if we're looking at a hanging
  ;; keyword.  This was a bad idea, because the virtual indent of a position
  ;; should not depend on the caller, since it leads to situations where two
  ;; dependent indentations get indented differently.
  "Compute the virtual indentation to use for point.
This is used when we're not trying to indent point but just
need to compute the column at which point should be indented
in order to figure out the indentation of some other (further down) point."
  ;; Trust pre-existing indentation on other lines.
  (if (smie-indent--bolp) (current-column) (smie-indent-calculate)))

(defun smie-indent-fixindent ()
  ;; Obey the `fixindent' special comment.
  (and (smie-indent--bolp)
       (save-excursion
         (comment-normalize-vars)
         (re-search-forward (concat comment-start-skip
                                    "fixindent"
                                    comment-end-skip)
                            ;; 1+ to account for the \n comment termination.
                            (1+ (line-end-position)) t))
       (current-column)))

(defun smie-indent-bob ()
  ;; Start the file at column 0.
  (save-excursion
    (forward-comment (- (point)))
    (if (bobp) 0)))

(defun smie-indent-close ()
  ;; Align close paren with opening paren.
  (save-excursion
    ;; (forward-comment (point-max))
    (when (looking-at "\\s)")
      (while (not (zerop (skip-syntax-forward ")")))
        (skip-chars-forward " \t"))
      (condition-case nil
          (progn
            (backward-sexp 1)
            (smie-indent-virtual))      ;:not-hanging
        (scan-error nil)))))

(defun smie-indent-keyword (&optional token)
  "Indent point based on the token that follows it immediately.
If TOKEN is non-nil, assume that that is the token that follows point.
Returns either a column number or nil if it considers that indentation
should not be computed on the basis of the following token."
  (save-excursion
    (let* ((pos (point))
           (toklevels
            (if token
                (assoc token smie-grammar)
              (let* ((res (smie-indent-forward-token)))
                ;; Ignore tokens on subsequent lines.
                (if (and (< pos (line-beginning-position))
                         ;; Make sure `token' also *starts* on another line.
                         (save-excursion
                           (smie-indent-backward-token)
                           (< pos (line-beginning-position))))
                    nil
                  (goto-char pos)
                  res)))))
      (setq token (pop toklevels))
      (cond
       ((null (cdr toklevels)) nil)     ;Not a keyword.
       ((not (numberp (car toklevels)))
        ;; Different cases:
        ;; - smie-indent--bolp: "indent according to others".
        ;; - common hanging: "indent according to others".
        ;; - SML-let hanging: "indent like parent".
        ;; - if-after-else: "indent-like parent".
        ;; - middle-of-line: "trust current position".
        (cond
         ((smie-indent--rule :before token))
         ((smie-indent--bolp)           ;I.e. non-virtual indent.
          ;; For an open-paren-like thingy at BOL, always indent only
          ;; based on other rules (typically smie-indent-after-keyword).
          nil)
         (t
          ;; By default use point unless we're hanging.
          (unless (smie-indent--hanging-p) (current-column)))))
       (t
        ;; FIXME: This still looks too much like black magic!!
        (let* ((parent (smie-backward-sexp token)))
          ;; Different behaviors:
          ;; - align with parent.
          ;; - parent + offset.
          ;; - after parent's column + offset (actually, after or before
          ;;   depending on where backward-sexp stopped).
          ;; ? let it drop to some other indentation function (almost never).
          ;; ? parent + offset + parent's own offset.
          ;; Different cases:
          ;; - bump into a same-level operator.
          ;; - bump into a specific known parent.
          ;; - find a matching open-paren thingy.
          ;; - bump into some random parent.
          ;; ? borderline case (almost never).
          ;; ? bump immediately into a parent.
          (cond
           ((not (or (< (point) pos)
                     (and (cadr parent) (< (cadr parent) pos))))
            ;; If we didn't move at all, that means we didn't really skip
            ;; what we wanted.  Should almost never happen, other than
            ;; maybe when an infix or close-paren is at the beginning
            ;; of a buffer.
            nil)
           ((save-excursion
              (goto-char pos)
              (smie-indent--rule :before token nil parent (cadr parent))))
           ((eq (car parent) (car toklevels))
            ;; We bumped into a same-level operator; align with it.
            (if (and (smie-indent--bolp) (/= (point) pos)
                     (save-excursion
                       (goto-char (goto-char (cadr parent)))
                       (not (smie-indent--bolp))))
                ;; If the parent is at EOL and its children are indented like
                ;; itself, then we can just obey the indentation chosen for the
                ;; child.
                ;; This is important for operators like ";" which
                ;; are usually at EOL (and have an offset of 0): otherwise we'd
                ;; always go back over all the statements, which is
                ;; a performance problem and would also mean that fixindents
                ;; in the middle of such a sequence would be ignored.
                ;;
                ;; This is a delicate point!
                ;; Even if the offset is not 0, we could follow the same logic
                ;; and subtract the offset from the child's indentation.
                ;; But that would more often be a bad idea: OT1H we generally
                ;; want to reuse the closest similar indentation point, so that
                ;; the user's choice (or the fixindents) are obeyed.  But OTOH
                ;; we don't want this to affect "unrelated" parts of the code.
                ;; E.g. a fixindent in the body of a "begin..end" should not
                ;; affect the indentation of the "end".
                (current-column)
              (goto-char (cadr parent))
              ;; Don't use (smie-indent-virtual :not-hanging) here, because we
              ;; want to jump back over a sequence of same-level ops such as
              ;;    a -> b -> c
              ;;    -> d
              ;; So as to align with the earliest appropriate place.
              (smie-indent-virtual)))
           (t
            (if (and (= (point) pos) (smie-indent--bolp))
                ;; Since we started at BOL, we're not computing a virtual
                ;; indentation, and we're still at the starting point, so
                ;; we can't use `current-column' which would cause
                ;; indentation to depend on itself and we can't use
                ;; smie-indent-virtual since that would be an inf-loop.
                nil
              ;; In indent-keyword, if we're indenting `then' wrt `if', we
              ;; want to use indent-virtual rather than use just
              ;; current-column, so that we can apply the (:before . "if")
              ;; rule which does the "else if" dance in SML.  But in other
              ;; cases, we do not want to use indent-virtual (e.g. indentation
              ;; of "*" w.r.t "+", or ";" wrt "(").  We could just always use
              ;; indent-virtual and then have indent-rules say explicitly to
              ;; use `point' after things like "(" or "+" when they're not at
              ;; EOL, but you'd end up with lots of those rules.
              ;; So we use a heuristic here, which is that we only use virtual
              ;; if the parent is tightly linked to the child token (they're
              ;; part of the same BNF rule).
              (if (car parent) (current-column) (smie-indent-virtual)))))))))))

(defun smie-indent-comment ()
  "Compute indentation of a comment."
  ;; Don't do it for virtual indentations.  We should normally never be "in
  ;; front of a comment" when doing virtual-indentation anyway.  And if we are
  ;; (as can happen in octave-mode), moving forward can lead to inf-loops.
  (and (smie-indent--bolp)
       (let ((pos (point)))
         (save-excursion
           (beginning-of-line)
           (and (re-search-forward comment-start-skip (line-end-position) t)
                (eq pos (or (match-end 1) (match-beginning 0))))))
       (save-excursion
         (forward-comment (point-max))
         (skip-chars-forward " \t\r\n")
         (smie-indent-calculate))))

(defun smie-indent-comment-continue ()
  ;; indentation of comment-continue lines.
  (let ((continue (and comment-continue
                       (comment-string-strip comment-continue t t))))
    (and (< 0 (length continue))
         (looking-at (regexp-quote continue)) (nth 4 (syntax-ppss))
         (let ((ppss (syntax-ppss)))
           (save-excursion
             (forward-line -1)
             (if (<= (point) (nth 8 ppss))
                 (progn (goto-char (1+ (nth 8 ppss))) (current-column))
               (skip-chars-forward " \t")
               (if (looking-at (regexp-quote continue))
                   (current-column))))))))

(defun smie-indent-comment-close ()
  (and (boundp 'comment-end-skip)
       comment-end-skip
       (not (looking-at " \t*$"))       ;Not just a \n comment-closer.
       (looking-at comment-end-skip)
       (let ((end (match-string 0)))
         (and (nth 4 (syntax-ppss))
              (save-excursion
                (goto-char (nth 8 (syntax-ppss)))
                (and (looking-at comment-start-skip)
                     (let ((start (match-string 0)))
                       ;; Align the common substring between starter
                       ;; and ender, if possible.
                       (if (string-match "\\(.+\\).*\n\\(.*?\\)\\1"
                                         (concat start "\n" end))
                           (+ (current-column) (match-beginning 0)
                              (- (match-beginning 2) (match-end 2)))
                         (current-column)))))))))

(defun smie-indent-comment-inside ()
  (and (nth 4 (syntax-ppss))
       'noindent))

(defun smie-indent-inside-string ()
  (and (nth 3 (syntax-ppss))
       'noindent))

(defun smie-indent-after-keyword ()
  ;; Indentation right after a special keyword.
  (save-excursion
    (let* ((pos (point))
           (toklevel (smie-indent-backward-token))
           (tok (car toklevel)))
      (cond
       ((null toklevel) nil)
       ((smie-indent--rule :after tok pos nil (point)))
       ;; The default indentation after a keyword/operator is
       ;; 0 for infix, t for prefix, and use another rule
       ;; for postfix.
       ((not (numberp (nth 2 toklevel))) nil)                   ;A closer.
       ((or (not (numberp (nth 1 toklevel)))                    ;An opener.
            (rassoc tok smie-closer-alist))                     ;An inner.
        (+ (smie-indent-virtual) (smie-indent--offset 'basic))) ;
       (t (smie-indent-virtual))))))                            ;An infix.

(defun smie-indent-exps ()
  ;; Indentation of sequences of simple expressions without
  ;; intervening keywords or operators.  E.g. "a b c" or "g (balbla) f".
  ;; Can be a list of expressions or a function call.
  ;; If it's a function call, the first element is special (it's the
  ;; function).  We distinguish function calls from mere lists of
  ;; expressions based on whether the preceding token is listed in
  ;; the `list-intro' entry of smie-indent-rules.
  ;;
  ;; TODO: to indent Lisp code, we should add a way to specify
  ;; particular indentation for particular args depending on the
  ;; function (which would require always skipping back until the
  ;; function).
  ;; TODO: to indent C code, such as "if (...) {...}" we might need
  ;; to add similar indentation hooks for particular positions, but
  ;; based on the preceding token rather than based on the first exp.
  (save-excursion
    (let ((positions nil)
          arg)
      (while (and (null (car (smie-backward-sexp)))
                  (push (point) positions)
                  (not (smie-indent--bolp))))
      (save-excursion
        ;; Figure out if the atom we just skipped is an argument rather
        ;; than a function.
        (setq arg
              (or (null (car (smie-backward-sexp)))
                  (funcall smie-rules-function :list-intro
                           (funcall smie-backward-token-function)))))
      (cond
       ((null positions)
        ;; We're the first expression of the list.  In that case, the
        ;; indentation should be (have been) determined by its context.
        nil)
       (arg
        ;; There's a previous element, and it's not special (it's not
        ;; the function), so let's just align with that one.
        (goto-char (car positions))
        (current-column))
       ((cdr positions)
        ;; We skipped some args plus the function and bumped into something.
        ;; Align with the first arg.
        (goto-char (cadr positions))
        (current-column))
       (positions
        ;; We're the first arg.
        (goto-char (car positions))
        (+ (smie-indent--offset 'args)
           ;; We used to use (smie-indent-virtual), but that
           ;; doesn't seem right since it might then indent args less than
           ;; the function itself.
           (current-column)))))))

(defvar smie-indent-functions
  '(smie-indent-fixindent smie-indent-bob smie-indent-close
    smie-indent-comment smie-indent-comment-continue smie-indent-comment-close
    smie-indent-comment-inside smie-indent-inside-string
    smie-indent-keyword smie-indent-after-keyword
                          smie-indent-exps)
  "Functions to compute the indentation.
Each function is called with no argument, shouldn't move point, and should
return either nil if it has no opinion, or an integer representing the column
to which that point should be aligned, if we were to reindent it.")

(defun smie-indent-calculate ()
  "Compute the indentation to use for point."
  (run-hook-with-args-until-success 'smie-indent-functions))

(defun smie-indent-line ()
  "Indent current line using the SMIE indentation engine."
  (interactive)
  (let* ((savep (point))
	 (indent (or (with-demoted-errors
                       (save-excursion
                         (forward-line 0)
                         (skip-chars-forward " \t")
                         (if (>= (point) savep) (setq savep nil))
                         (or (smie-indent-calculate) 0)))
                     0)))
    (if (not (numberp indent))
        ;; If something funny is used (e.g. `noindent'), return it.
        indent
      (if (< indent 0) (setq indent 0)) ;Just in case.
      (if savep
          (save-excursion (indent-line-to indent))
        (indent-line-to indent)))))

(defun smie-setup (grammar rules-function &rest keywords)
  "Setup SMIE navigation and indentation.
GRAMMAR is a grammar table generated by `smie-prec2->grammar'.
RULES-FUNCTION is a set of indentation rules for use on `smie-rules-function'.
KEYWORDS are additional arguments, which can use the following keywords:
- :forward-token FUN
- :backward-token FUN"
  (set (make-local-variable 'smie-rules-function) rules-function)
  (set (make-local-variable 'smie-grammar) grammar)
  (set (make-local-variable 'indent-line-function) 'smie-indent-line)
  (set (make-local-variable 'forward-sexp-function)
       'smie-forward-sexp-command)
  (while keywords
    (let ((k (pop keywords))
          (v (pop keywords)))
      (case k
        (:forward-token
         (set (make-local-variable 'smie-forward-token-function) v))
        (:backward-token
         (set (make-local-variable 'smie-backward-token-function) v))
        (t (message "smie-setup: ignoring unknown keyword %s" k)))))
  (let ((ca (cdr (assq :smie-closer-alist grammar))))
    (when ca
      (set (make-local-variable 'smie-closer-alist) ca)
      ;; Only needed for interactive calls to blink-matching-open.
      (set (make-local-variable 'blink-matching-check-function)
           #'smie-blink-matching-check)
      (add-hook 'post-self-insert-hook
                #'smie-blink-matching-open 'append 'local)
      (set (make-local-variable 'smie-blink-matching-triggers)
           (append smie-blink-matching-triggers
                   ;; Rather than wait for SPC to blink, try to blink as
                   ;; soon as we type the last char of a block ender.
                   (let ((closers (sort (mapcar #'cdr smie-closer-alist)
                                        #'string-lessp))
                         (triggers ())
                         closer)
                     (while (setq closer (pop closers))
                       (unless (and closers
                                    ;; FIXME: this eliminates prefixes of other
                                    ;; closers, but we should probably
                                    ;; eliminate prefixes of other keywords
                                    ;; as well.
                                    (string-prefix-p closer (car closers)))
                         (push (aref closer (1- (length closer))) triggers)))
                     (delete-dups triggers)))))))


(provide 'smie)
;;; smie.el ends here
