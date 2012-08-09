;;; syntax.el --- helper functions to find syntactic context

;; Copyright (C) 2000-2012 Free Software Foundation, Inc.

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

;; The main exported function is `syntax-ppss'.  You might also need
;; to call `syntax-ppss-flush-cache' or to add it to
;; before-change-functions'(although this is automatically done by
;; syntax-ppss when needed, but that might fail if syntax-ppss is
;; called in a context where before-change-functions is temporarily
;; let-bound to nil).

;;; Todo:

;; - do something about the case where the syntax-table is changed.
;;   This typically happens with tex-mode and its `$' operator.
;; - new functions `syntax-state', ... to replace uses of parse-partial-state
;;   with something higher-level (similar to syntax-ppss-context).
;; - interaction with mmm-mode.

;;; Code:

;; Note: PPSS stands for `parse-partial-sexp state'

(eval-when-compile (require 'cl))

(defvar font-lock-beginning-of-syntax-function)

;;; Applying syntax-table properties where needed.

(defvar syntax-propertize-function nil
  ;; Rather than a -functions hook, this is a -function because it's easier
  ;; to do a single scan than several scans: with multiple scans, one cannot
  ;; assume that the text before point has been propertized, so syntax-ppss
  ;; gives unreliable results (and stores them in its cache to boot, so we'd
  ;; have to flush that cache between each function, and we couldn't use
  ;; syntax-ppss-flush-cache since that would not only flush the cache but also
  ;; reset syntax-propertize--done which should not be done in this case).
  "Mode-specific function to apply the syntax-table properties.
Called with two arguments: START and END.
This function can call `syntax-ppss' on any position before END, but it
should not call `syntax-ppss-flush-cache', which means that it should not
call `syntax-ppss' on some position and later modify the buffer on some
earlier position.")

(defvar syntax-propertize-chunk-size 500)

(defvar syntax-propertize-extend-region-functions
  '(syntax-propertize-wholelines)
  "Special hook run just before proceeding to propertize a region.
This is used to allow major modes to help `syntax-propertize' find safe buffer
positions as beginning and end of the propertized region.  Its most common use
is to solve the problem of /identification/ of multiline elements by providing
a function that tries to find such elements and move the boundaries such that
they do not fall in the middle of one.
Each function is called with two arguments (START and END) and it should return
either a cons (NEW-START . NEW-END) or nil if no adjustment should be made.
These functions are run in turn repeatedly until they all return nil.
Put first the functions more likely to cause a change and cheaper to compute.")
;; Mark it as a special hook which doesn't use any global setting
;; (i.e. doesn't obey the element t in the buffer-local value).
(make-variable-buffer-local 'syntax-propertize-extend-region-functions)

(defun syntax-propertize-wholelines (start end)
  (goto-char start)
  (cons (line-beginning-position)
        (progn (goto-char end)
               (if (bolp) (point) (line-beginning-position 2)))))

(defun syntax-propertize-multiline (beg end)
  "Let `syntax-propertize' pay attention to the syntax-multiline property."
  (when (and (> beg (point-min))
	     (get-text-property (1- beg) 'syntax-multiline))
    (setq beg (or (previous-single-property-change beg 'syntax-multiline)
		  (point-min))))
  ;;
  (when (get-text-property end 'font-lock-multiline)
    (setq end (or (text-property-any end (point-max)
				     'syntax-multiline nil)
		  (point-max))))
  (cons beg end))

(defvar syntax-propertize--done -1
  "Position up to which syntax-table properties have been set.")
(make-variable-buffer-local 'syntax-propertize--done)

(defun syntax-propertize--shift-groups (re n)
  (replace-regexp-in-string
   "\\\\(\\?\\([0-9]+\\):"
   (lambda (s)
     (replace-match
      (number-to-string (+ n (string-to-number (match-string 1 s))))
      t t s 1))
   re t t))

(defmacro syntax-propertize-precompile-rules (&rest rules)
  "Return a precompiled form of RULES to pass to `syntax-propertize-rules'.
The arg RULES can be of the same form as in `syntax-propertize-rules'.
The return value is an object that can be passed as a rule to
`syntax-propertize-rules'.
I.e. this is useful only when you want to share rules among several
syntax-propertize-functions."
  (declare (debug syntax-propertize-rules))
  ;; Precompile?  Yeah, right!
  ;; Seriously, tho, this is a macro for 2 reasons:
  ;; - we could indeed do some pre-compilation at some point in the future,
  ;;   e.g. fi/when we switch to a DFA-based implementation of
  ;;   syntax-propertize-rules.
  ;; - this lets Edebug properly annotate the expressions inside RULES.
  `',rules)

(defmacro syntax-propertize-rules (&rest rules)
  "Make a function that applies RULES for use in `syntax-propertize-function'.
The function will scan the buffer, applying the rules where they match.
The buffer is scanned a single time, like \"lex\" would, rather than once
per rule.

Each RULE can be a symbol, in which case that symbol's value should be,
at macro-expansion time, a precompiled set of rules, as returned
by `syntax-propertize-precompile-rules'.

Otherwise, RULE should have the form (REGEXP HIGHLIGHT1 ... HIGHLIGHTn), where
REGEXP is an expression (evaluated at time of macro-expansion) that returns
a regexp, and where HIGHLIGHTs have the form (NUMBER SYNTAX) which means to
apply the property SYNTAX to the chars matched by the subgroup NUMBER
of the regular expression, if NUMBER did match.
SYNTAX is an expression that returns a value to apply as `syntax-table'
property.  Some expressions are handled specially:
- if SYNTAX is a string, then it is converted with `string-to-syntax';
- if SYNTAX has the form (prog1 EXP . EXPS) then the value returned by EXP
  will be applied to the buffer before running EXPS and if EXP is a string it
  is also converted with `string-to-syntax'.
The SYNTAX expression is responsible to save the `match-data' if needed
for subsequent HIGHLIGHTs.
Also SYNTAX is free to move point, in which case RULES may not be applied to
some parts of the text or may be applied several times to other parts.

Note: back-references in REGEXPs do not work."
  (declare (debug (&rest &or symbolp    ;FIXME: edebug this eval step.
                         (form &rest
                               (numberp
                                [&or stringp ;FIXME: Use &wrap
                                     ("prog1" [&or stringp def-form] def-body)
                                     def-form])))))
  (let ((newrules nil))
    (while rules
      (if (symbolp (car rules))
          (setq rules (append (symbol-value (pop rules)) rules))
        (push (pop rules) newrules)))
    (setq rules (nreverse newrules)))
  (let* ((offset 0)
         (branches '())
         ;; We'd like to use a real DFA-based lexer, usually, but since Emacs
         ;; doesn't have one yet, we fallback on building one large regexp
         ;; and use groups to determine which branch of the regexp matched.
         (re
          (mapconcat
           (lambda (rule)
             (let* ((orig-re (eval (car rule)))
                    (re orig-re))
               (when (and (assq 0 rule) (cdr rules))
                 ;; If there's more than 1 rule, and the rule want to apply
                 ;; highlight to match 0, create an extra group to be able to
                 ;; tell when *this* match 0 has succeeded.
                 (incf offset)
                 (setq re (concat "\\(" re "\\)")))
               (setq re (syntax-propertize--shift-groups re offset))
               (let ((code '())
                     (condition
                      (cond
                       ((assq 0 rule) (if (zerop offset) t
                                        `(match-beginning ,offset)))
                       ((null (cddr rule))
                        `(match-beginning ,(+ offset (car (cadr rule)))))
                       (t
                        `(or ,@(mapcar
                                (lambda (case)
                                  `(match-beginning ,(+ offset (car case))))
                                (cdr rule))))))
                     (nocode t)
                     (offset offset))
                 ;; If some of the subgroup rules include Elisp code, then we
                 ;; need to set the match-data so it's consistent with what the
                 ;; code expects.  If not, then we can simply use shifted
                 ;; offset in our own code.
                 (unless (zerop offset)
                   (dolist (case (cdr rule))
                     (unless (stringp (cadr case))
                       (setq nocode nil)))
                   (unless nocode
                     (push `(let ((md (match-data 'ints)))
                              ;; Keep match 0 as is, but shift everything else.
                              (setcdr (cdr md) (nthcdr ,(* (1+ offset) 2) md))
                              (set-match-data md))
                           code)
                     (setq offset 0)))
                 ;; Now construct the code for each subgroup rules.
                 (dolist (case (cdr rule))
                   (assert (null (cddr case)))
                   (let* ((gn (+ offset (car case)))
                          (action (nth 1 case))
                          (thiscode
                           (cond
                            ((stringp action)
                             `((put-text-property
                                (match-beginning ,gn) (match-end ,gn)
                                'syntax-table
                                ',(string-to-syntax action))))
                            ((eq (car-safe action) 'ignore)
                             (cdr action))
                            ((eq (car-safe action) 'prog1)
                             (if (stringp (nth 1 action))
                                 `((put-text-property
                                    (match-beginning ,gn) (match-end ,gn)
                                    'syntax-table
                                    ',(string-to-syntax (nth 1 action)))
                                   ,@(nthcdr 2 action))
                               `((let ((mb (match-beginning ,gn))
                                       (me (match-end ,gn))
                                       (syntax ,(nth 1 action)))
                                   (if syntax
                                       (put-text-property
                                        mb me 'syntax-table syntax))
                                   ,@(nthcdr 2 action)))))
                            (t
                             `((let ((mb (match-beginning ,gn))
                                     (me (match-end ,gn))
                                     (syntax ,action))
                                 (if syntax
                                     (put-text-property
                                      mb me 'syntax-table syntax))))))))

                     (if (or (not (cddr rule)) (zerop gn))
                         (setq code (nconc (nreverse thiscode) code))
                       (push `(if (match-beginning ,gn)
                                  ;; Try and generate clean code with no
                                  ;; extraneous progn.
                                  ,(if (null (cdr thiscode))
                                       (car thiscode)
                                     `(progn ,@thiscode)))
                             code))))
                 (push (cons condition (nreverse code))
                       branches))
               (incf offset (regexp-opt-depth orig-re))
               re))
           rules
           "\\|")))
    `(lambda (start end)
       (goto-char start)
       (while (and (< (point) end)
                   (re-search-forward ,re end t))
         (cond ,@(nreverse branches))))))

(defun syntax-propertize-via-font-lock (keywords)
  "Propertize for syntax in START..END using font-lock syntax.
KEYWORDS obeys the format used in `font-lock-syntactic-keywords'.
The return value is a function suitable for `syntax-propertize-function'."
  (lexical-let ((keywords keywords))
    (lambda (start end)
      (with-no-warnings
        (let ((font-lock-syntactic-keywords keywords))
          (font-lock-fontify-syntactic-keywords-region start end)
          ;; In case it was eval'd/compiled.
          (setq keywords font-lock-syntactic-keywords))))))

(defun syntax-propertize (pos)
  "Ensure that syntax-table properties are set until POS."
  (when (and syntax-propertize-function
             (< syntax-propertize--done pos))
    ;; (message "Needs to syntax-propertize from %s to %s"
    ;;          syntax-propertize--done pos)
    (set (make-local-variable 'parse-sexp-lookup-properties) t)
    (save-excursion
      (with-silent-modifications
        (let* ((start (max syntax-propertize--done (point-min)))
               (end (max pos
                         (min (point-max)
                              (+ start syntax-propertize-chunk-size))))
               (funs syntax-propertize-extend-region-functions))
          (while funs
            (let ((new (funcall (pop funs) start end)))
              (if (or (null new)
                      (and (>= (car new) start) (<= (cdr new) end)))
                  nil
                (setq start (car new))
                (setq end (cdr new))
                ;; If there's been a change, we should go through the
                ;; list again since this new position may
                ;; warrant a different answer from one of the funs we've
                ;; already seen.
                (unless (eq funs
                            (cdr syntax-propertize-extend-region-functions))
                  (setq funs syntax-propertize-extend-region-functions)))))
          ;; Move the limit before calling the function, so the function
          ;; can use syntax-ppss.
          (setq syntax-propertize--done end)
          ;; (message "syntax-propertizing from %s to %s" start end)
          (remove-text-properties start end
                                  '(syntax-table nil syntax-multiline nil))
          (funcall syntax-propertize-function start end))))))

;;; Incrementally compute and memoize parser state.

(defsubst syntax-ppss-depth (ppss)
  (nth 0 ppss))

(defun syntax-ppss-toplevel-pos (ppss)
  "Get the latest syntactically outermost position found in a syntactic scan.
PPSS is a scan state, as returned by `parse-partial-sexp' or `syntax-ppss'.
An \"outermost position\" means one that it is outside of any syntactic entity:
outside of any parentheses, comments, or strings encountered in the scan.
If no such position is recorded in PPSS (because the end of the scan was
itself at the outermost level), return nil."
  ;; BEWARE! We rely on the undocumented 9th field.  The 9th field currently
  ;; contains the list of positions of the enclosing open-parens.
  ;; I.e. those positions are outside of any string/comment and the first of
  ;; those is outside of any paren (i.e. corresponds to a nil ppss).
  ;; If this list is empty but we are in a string or comment, then the 8th
  ;; field contains a similar "toplevel" position.
  (or (car (nth 9 ppss))
      (nth 8 ppss)))

(defsubst syntax-ppss-context (ppss)
  (cond
   ((nth 3 ppss) 'string)
   ((nth 4 ppss) 'comment)
   (t nil)))

(defvar syntax-ppss-max-span 20000
  "Threshold below which cache info is deemed unnecessary.
We try to make sure that cache entries are at least this far apart
from each other, to avoid keeping too much useless info.")

(defvar syntax-begin-function nil
  "Function to move back outside of any comment/string/paren.
This function should move the cursor back to some syntactically safe
point (where the PPSS is equivalent to nil).")

(defvar syntax-ppss-cache nil
  "List of (POS . PPSS) pairs, in decreasing POS order.")
(make-variable-buffer-local 'syntax-ppss-cache)
(defvar syntax-ppss-last nil
  "Cache of (LAST-POS . LAST-PPSS).")
(make-variable-buffer-local 'syntax-ppss-last)

(defalias 'syntax-ppss-after-change-function 'syntax-ppss-flush-cache)
(defun syntax-ppss-flush-cache (beg &rest ignored)
  "Flush the cache of `syntax-ppss' starting at position BEG."
  ;; Set syntax-propertize to refontify anything past beg.
  (setq syntax-propertize--done (min beg syntax-propertize--done))
  ;; Flush invalid cache entries.
  (while (and syntax-ppss-cache (> (caar syntax-ppss-cache) beg))
    (setq syntax-ppss-cache (cdr syntax-ppss-cache)))
  ;; Throw away `last' value if made invalid.
  (when (< beg (or (car syntax-ppss-last) 0))
    ;; If syntax-begin-function jumped to BEG, then the old state at BEG can
    ;; depend on the text after BEG (which is presumably changed).  So if
    ;; BEG=(car (nth 10 syntax-ppss-last)) don't reuse that data because the
    ;; assumed nil state at BEG may not be valid any more.
    (if (<= beg (or (syntax-ppss-toplevel-pos (cdr syntax-ppss-last))
                    (nth 3 syntax-ppss-last)
                    0))
	(setq syntax-ppss-last nil)
      (setcar syntax-ppss-last nil)))
  ;; Unregister if there's no cache left.  Sadly this doesn't work
  ;; because `before-change-functions' is temporarily bound to nil here.
  ;; (unless syntax-ppss-cache
  ;;   (remove-hook 'before-change-functions 'syntax-ppss-flush-cache t))
  )

(defvar syntax-ppss-stats
  [(0 . 0.0) (0 . 0.0) (0 . 0.0) (0 . 0.0) (0 . 0.0) (1 . 2500.0)])
(defun syntax-ppss-stats ()
  (mapcar (lambda (x)
	    (condition-case nil
		(cons (car x) (truncate (/ (cdr x) (car x))))
	      (error nil)))
	  syntax-ppss-stats))

(defun syntax-ppss (&optional pos)
  "Parse-Partial-Sexp State at POS, defaulting to point.
The returned value is the same as that of `parse-partial-sexp'
run from point-min to POS except that values at positions 2 and 6
in the returned list (counting from 0) cannot be relied upon.
Point is at POS when this function returns."
  ;; Default values.
  (unless pos (setq pos (point)))
  (syntax-propertize pos)
  ;;
  (let ((old-ppss (cdr syntax-ppss-last))
	(old-pos (car syntax-ppss-last))
	(ppss nil)
	(pt-min (point-min)))
    (if (and old-pos (> old-pos pos)) (setq old-pos nil))
    ;; Use the OLD-POS if usable and close.  Don't update the `last' cache.
    (condition-case nil
	(if (and old-pos (< (- pos old-pos)
			    ;; The time to use syntax-begin-function and
			    ;; find PPSS is assumed to be about 2 * distance.
			    (* 2 (/ (cdr (aref syntax-ppss-stats 5))
				    (1+ (car (aref syntax-ppss-stats 5)))))))
	    (progn
	      (incf (car (aref syntax-ppss-stats 0)))
	      (incf (cdr (aref syntax-ppss-stats 0)) (- pos old-pos))
	      (parse-partial-sexp old-pos pos nil nil old-ppss))

	  (cond
	   ;; Use OLD-PPSS if possible and close enough.
	   ((and (not old-pos) old-ppss
                 ;; If `pt-min' is too far from `pos', we could try to use
		 ;; other positions in (nth 9 old-ppss), but that doesn't
		 ;; seem to happen in practice and it would complicate this
		 ;; code (and the before-change-function code even more).
		 ;; But maybe it would be useful in "degenerate" cases such
		 ;; as when the whole file is wrapped in a set
		 ;; of parentheses.
		 (setq pt-min (or (syntax-ppss-toplevel-pos old-ppss)
				  (nth 2 old-ppss)))
		 (<= pt-min pos) (< (- pos pt-min) syntax-ppss-max-span))
	    (incf (car (aref syntax-ppss-stats 1)))
	    (incf (cdr (aref syntax-ppss-stats 1)) (- pos pt-min))
	    (setq ppss (parse-partial-sexp pt-min pos)))
	   ;; The OLD-* data can't be used.  Consult the cache.
	   (t
	    (let ((cache-pred nil)
		  (cache syntax-ppss-cache)
		  (pt-min (point-min))
		  ;; I differentiate between PT-MIN and PT-BEST because
		  ;; I feel like it might be important to ensure that the
		  ;; cache is only filled with 100% sure data (whereas
		  ;; syntax-begin-function might return incorrect data).
		  ;; Maybe that's just stupid.
		  (pt-best (point-min))
		  (ppss-best nil))
	      ;; look for a usable cache entry.
	      (while (and cache (< pos (caar cache)))
		(setq cache-pred cache)
		(setq cache (cdr cache)))
	      (if cache (setq pt-min (caar cache) ppss (cdar cache)))

	      ;; Setup the before-change function if necessary.
	      (unless (or syntax-ppss-cache syntax-ppss-last)
		(add-hook 'before-change-functions
			  'syntax-ppss-flush-cache t t))

	      ;; Use the best of OLD-POS and CACHE.
	      (if (or (not old-pos) (< old-pos pt-min))
		  (setq pt-best pt-min ppss-best ppss)
		(incf (car (aref syntax-ppss-stats 4)))
		(incf (cdr (aref syntax-ppss-stats 4)) (- pos old-pos))
		(setq pt-best old-pos ppss-best old-ppss))

	      ;; Use the `syntax-begin-function' if available.
	      ;; We could try using that function earlier, but:
	      ;; - The result might not be 100% reliable, so it's better to use
	      ;;   the cache if available.
	      ;; - The function might be slow.
	      ;; - If this function almost always finds a safe nearby spot,
	      ;;   the cache won't be populated, so consulting it is cheap.
	      (when (and (not syntax-begin-function)
			 (boundp 'font-lock-beginning-of-syntax-function)
			 font-lock-beginning-of-syntax-function)
		(set (make-local-variable 'syntax-begin-function)
		     font-lock-beginning-of-syntax-function))
	      (when (and syntax-begin-function
			 (progn (goto-char pos)
				(funcall syntax-begin-function)
				;; Make sure it's better.
				(> (point) pt-best))
			 ;; Simple sanity checks.
                         (< (point) pos) ; backward-paragraph can fail here.
			 (not (memq (get-text-property (point) 'face)
				    '(font-lock-string-face font-lock-doc-face
				      font-lock-comment-face))))
		(incf (car (aref syntax-ppss-stats 5)))
		(incf (cdr (aref syntax-ppss-stats 5)) (- pos (point)))
		(setq pt-best (point) ppss-best nil))

	      (cond
	       ;; Quick case when we found a nearby pos.
	       ((< (- pos pt-best) syntax-ppss-max-span)
		(incf (car (aref syntax-ppss-stats 2)))
		(incf (cdr (aref syntax-ppss-stats 2)) (- pos pt-best))
		(setq ppss (parse-partial-sexp pt-best pos nil nil ppss-best)))
	       ;; Slow case: compute the state from some known position and
	       ;; populate the cache so we won't need to do it again soon.
	       (t
		(incf (car (aref syntax-ppss-stats 3)))
		(incf (cdr (aref syntax-ppss-stats 3)) (- pos pt-min))

		;; If `pt-min' is too far, add a few intermediate entries.
		(while (> (- pos pt-min) (* 2 syntax-ppss-max-span))
		  (setq ppss (parse-partial-sexp
			      pt-min (setq pt-min (/ (+ pt-min pos) 2))
			      nil nil ppss))
		  (let ((pair (cons pt-min ppss)))
		    (if cache-pred
			(push pair (cdr cache-pred))
		      (push pair syntax-ppss-cache))))

		;; Compute the actual return value.
		(setq ppss (parse-partial-sexp pt-min pos nil nil ppss))

		;; Debugging check.
		;; (let ((real-ppss (parse-partial-sexp (point-min) pos)))
		;;   (setcar (last ppss 4) 0)
		;;   (setcar (last real-ppss 4) 0)
		;;   (setcar (last ppss 8) nil)
		;;   (setcar (last real-ppss 8) nil)
		;;   (unless (equal ppss real-ppss)
		;;     (message "!!Syntax: %s != %s" ppss real-ppss)
		;;     (setq ppss real-ppss)))

		;; Store it in the cache.
		(let ((pair (cons pos ppss)))
		  (if cache-pred
		      (if (> (- (caar cache-pred) pos) syntax-ppss-max-span)
			  (push pair (cdr cache-pred))
			(setcar cache-pred pair))
		    (if (or (null syntax-ppss-cache)
			    (> (- (caar syntax-ppss-cache) pos)
			       syntax-ppss-max-span))
			(push pair syntax-ppss-cache)
		      (setcar syntax-ppss-cache pair)))))))))

	  (setq syntax-ppss-last (cons pos ppss))
	  ppss)
      (args-out-of-range
       ;; If the buffer is more narrowed than when we built the cache,
       ;; we may end up calling parse-partial-sexp with a position before
       ;; point-min.  In that case, just parse from point-min assuming
       ;; a nil state.
       (parse-partial-sexp (point-min) pos)))))

;; Debugging functions

(defun syntax-ppss-debug ()
  (let ((pt nil)
	(min-diffs nil))
    (dolist (x (append syntax-ppss-cache (list (cons (point-min) nil))))
      (when pt (push (- pt (car x)) min-diffs))
      (setq pt (car x)))
    min-diffs))

;; XEmacs compatibility functions

;; (defun buffer-syntactic-context (&optional buffer)
;;   "Syntactic context at point in BUFFER.
;; Either of `string', `comment' or `nil'.
;; This is an XEmacs compatibility function."
;;   (with-current-buffer (or buffer (current-buffer))
;;     (syntax-ppss-context (syntax-ppss))))

;; (defun buffer-syntactic-context-depth (&optional buffer)
;;   "Syntactic parenthesis depth at point in BUFFER.
;; This is an XEmacs compatibility function."
;;   (with-current-buffer (or buffer (current-buffer))
;;     (syntax-ppss-depth (syntax-ppss))))

(provide 'syntax)

;;; syntax.el ends here
