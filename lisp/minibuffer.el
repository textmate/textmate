;;; minibuffer.el --- Minibuffer completion functions -*- lexical-binding: t -*-

;; Copyright (C) 2008-2012  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Package: emacs

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

;; Names with "--" are for functions and variables that are meant to be for
;; internal use only.

;; Functional completion tables have an extended calling conventions:
;; The `action' can be (additionally to nil, t, and lambda) of the form
;; - (boundaries . SUFFIX) in which case it should return
;;   (boundaries START . END).  See `completion-boundaries'.
;;   Any other return value should be ignored (so we ignore values returned
;;   from completion tables that don't know about this new `action' form).
;; - `metadata' in which case it should return (metadata . ALIST) where
;;   ALIST is the metadata of this table.  See `completion-metadata'.
;;   Any other return value should be ignored (so we ignore values returned
;;   from completion tables that don't know about this new `action' form).

;;; Bugs:

;; - completion-all-sorted-completions list all the completions, whereas
;;   it should only lists the ones that `try-completion' would consider.
;;   E.g.  it should honor completion-ignored-extensions.
;; - choose-completion can't automatically figure out the boundaries
;;   corresponding to the displayed completions because we only
;;   provide the start info but not the end info in
;;   completion-base-position.
;; - quoting is problematic.  E.g. the double-dollar quoting used in
;;   substitute-in-file-name (and hence read-file-name-internal) bumps
;;   into various bugs:
;; - choose-completion doesn't know how to quote the text it inserts.
;;   E.g. it fails to double the dollars in file-name completion, or
;;   to backslash-escape spaces and other chars in comint completion.
;;   - when completing ~/tmp/fo$$o, the highlighting in *Completions*
;;     is off by one position.
;;   - all code like PCM which relies on all-completions to match
;;     its argument gets confused because all-completions returns unquoted
;;     texts (as desired for *Completions* output).
;; - C-x C-f ~/*/sr ? should not list "~/./src".
;; - minibuffer-force-complete completes ~/src/emacs/t<!>/lisp/minibuffer.el
;;   to ~/src/emacs/trunk/ and throws away lisp/minibuffer.el.

;;; Todo:

;; - for M-x, cycle-sort commands that have no key binding first.
;; - Make things like icomplete-mode or lightning-completion work with
;;   completion-in-region-mode.
;; - extend `metadata':
;;   - quoting/unquoting (so we can complete files names with envvars
;;     and backslashes, and all-completion can list names without
;;     quoting backslashes and dollars).
;;   - indicate how to turn all-completion's output into
;;     try-completion's output: e.g. completion-ignored-extensions.
;;     maybe that could be merged with the "quote" operation above.
;;   - indicate that `all-completions' doesn't do prefix-completion
;;     but just returns some list that relates in some other way to
;;     the provided string (as is the case in filecache.el), in which
;;     case partial-completion (for example) doesn't make any sense
;;     and neither does the completions-first-difference highlight.
;;   - indicate how to display the completions in *Completions* (turn
;;     \n into something else, add special boundaries between
;;     completions).  E.g. when completing from the kill-ring.

;; - case-sensitivity currently confuses two issues:
;;   - whether or not a particular completion table should be case-sensitive
;;     (i.e. whether strings that differ only by case are semantically
;;     equivalent)
;;   - whether the user wants completion to pay attention to case.
;;   e.g. we may want to make it possible for the user to say "first try
;;   completion case-sensitively, and if that fails, try to ignore case".

;; - add support for ** to pcm.
;; - Add vc-file-name-completion-table to read-file-name-internal.
;; - A feature like completing-help.el.

;;; Code:

(eval-when-compile (require 'cl))

;;; Completion table manipulation

;; New completion-table operation.
(defun completion-boundaries (string table pred suffix)
  "Return the boundaries of the completions returned by TABLE for STRING.
STRING is the string on which completion will be performed.
SUFFIX is the string after point.
The result is of the form (START . END) where START is the position
in STRING of the beginning of the completion field and END is the position
in SUFFIX of the end of the completion field.
E.g. for simple completion tables, the result is always (0 . (length SUFFIX))
and for file names the result is the positions delimited by
the closest directory separators."
  (let ((boundaries (if (functionp table)
                        (funcall table string pred
                                 (cons 'boundaries suffix)))))
    (if (not (eq (car-safe boundaries) 'boundaries))
        (setq boundaries nil))
    (cons (or (cadr boundaries) 0)
          (or (cddr boundaries) (length suffix)))))

(defun completion-metadata (string table pred)
  "Return the metadata of elements to complete at the end of STRING.
This metadata is an alist.  Currently understood keys are:
- `category': the kind of objects returned by `all-completions'.
   Used by `completion-category-overrides'.
- `annotation-function': function to add annotations in *Completions*.
   Takes one argument (STRING), which is a possible completion and
   returns a string to append to STRING.
- `display-sort-function': function to sort entries in *Completions*.
   Takes one argument (COMPLETIONS) and should return a new list
   of completions.  Can operate destructively.
- `cycle-sort-function': function to sort entries when cycling.
   Works like `display-sort-function'.
The metadata of a completion table should be constant between two boundaries."
  (let ((metadata (if (functionp table)
                      (funcall table string pred 'metadata))))
    (if (eq (car-safe metadata) 'metadata)
        metadata
      '(metadata))))

(defun completion--field-metadata (field-start)
  (completion-metadata (buffer-substring-no-properties field-start (point))
                       minibuffer-completion-table
                       minibuffer-completion-predicate))

(defun completion-metadata-get (metadata prop)
  (cdr (assq prop metadata)))

(defun completion--some (fun xs)
  "Apply FUN to each element of XS in turn.
Return the first non-nil returned value.
Like CL's `some'."
  (let ((firsterror nil)
        res)
    (while (and (not res) xs)
      (condition-case err
          (setq res (funcall fun (pop xs)))
        (error (unless firsterror (setq firsterror err)) nil)))
    (or res
        (if firsterror (signal (car firsterror) (cdr firsterror))))))

(defun complete-with-action (action table string pred)
  "Perform completion ACTION.
STRING is the string to complete.
TABLE is the completion table.
PRED is a completion predicate.
ACTION can be one of nil, t or `lambda'."
  (cond
   ((functionp table) (funcall table string pred action))
   ((eq (car-safe action) 'boundaries) nil)
   ((eq action 'metadata) nil)
   (t
    (funcall
     (cond
      ((null action) 'try-completion)
      ((eq action t) 'all-completions)
      (t 'test-completion))
     string table pred))))

(defun completion-table-dynamic (fun)
  "Use function FUN as a dynamic completion table.
FUN is called with one argument, the string for which completion is required,
and it should return an alist containing all the intended possible completions.
This alist may be a full list of possible completions so that FUN can ignore
the value of its argument.  If completion is performed in the minibuffer,
FUN will be called in the buffer from which the minibuffer was entered.

The result of the `completion-table-dynamic' form is a function
that can be used as the COLLECTION argument to `try-completion' and
`all-completions'.  See Info node `(elisp)Programmed Completion'."
  (lambda (string pred action)
    (if (or (eq (car-safe action) 'boundaries) (eq action 'metadata))
        ;; `fun' is not supposed to return another function but a plain old
        ;; completion table, whose boundaries are always trivial.
        nil
      (with-current-buffer (let ((win (minibuffer-selected-window)))
                             (if (window-live-p win) (window-buffer win)
                               (current-buffer)))
        (complete-with-action action (funcall fun string) string pred)))))

(defmacro lazy-completion-table (var fun)
  "Initialize variable VAR as a lazy completion table.
If the completion table VAR is used for the first time (e.g., by passing VAR
as an argument to `try-completion'), the function FUN is called with no
arguments.  FUN must return the completion table that will be stored in VAR.
If completion is requested in the minibuffer, FUN will be called in the buffer
from which the minibuffer was entered.  The return value of
`lazy-completion-table' must be used to initialize the value of VAR.

You should give VAR a non-nil `risky-local-variable' property."
  (declare (debug (symbolp lambda-expr)))
  (let ((str (make-symbol "string")))
    `(completion-table-dynamic
      (lambda (,str)
        (when (functionp ,var)
          (setq ,var (,fun)))
        ,var))))

(defun completion-table-case-fold (table &optional dont-fold)
  "Return new completion TABLE that is case insensitive.
If DONT-FOLD is non-nil, return a completion table that is
case sensitive instead."
  (lambda (string pred action)
    (let ((completion-ignore-case (not dont-fold)))
      (complete-with-action action table string pred))))

(defun completion-table-with-context (prefix table string pred action)
  ;; TODO: add `suffix' maybe?
  (let ((pred
         (if (not (functionp pred))
             ;; Notice that `pred' may not be a function in some abusive cases.
             pred
           ;; Predicates are called differently depending on the nature of
           ;; the completion table :-(
           (cond
            ((vectorp table)            ;Obarray.
             (lambda (sym) (funcall pred (concat prefix (symbol-name sym)))))
            ((hash-table-p table)
             (lambda (s _v) (funcall pred (concat prefix s))))
            ((functionp table)
             (lambda (s) (funcall pred (concat prefix s))))
            (t                          ;Lists and alists.
             (lambda (s)
               (funcall pred (concat prefix (if (consp s) (car s) s)))))))))
    (if (eq (car-safe action) 'boundaries)
        (let* ((len (length prefix))
               (bound (completion-boundaries string table pred (cdr action))))
          (list* 'boundaries (+ (car bound) len) (cdr bound)))
      (let ((comp (complete-with-action action table string pred)))
        (cond
         ;; In case of try-completion, add the prefix.
         ((stringp comp) (concat prefix comp))
         (t comp))))))

(defun completion-table-with-terminator (terminator table string pred action)
  "Construct a completion table like TABLE but with an extra TERMINATOR.
This is meant to be called in a curried way by first passing TERMINATOR
and TABLE only (via `apply-partially').
TABLE is a completion table, and TERMINATOR is a string appended to TABLE's
completion if it is complete.  TERMINATOR is also used to determine the
completion suffix's boundary.
TERMINATOR can also be a cons cell (TERMINATOR . TERMINATOR-REGEXP)
in which case TERMINATOR-REGEXP is a regular expression whose submatch
number 1 should match TERMINATOR.  This is used when there is a need to
distinguish occurrences of the TERMINATOR strings which are really terminators
from others (e.g. escaped).  In this form, the car of TERMINATOR can also be,
instead of a string, a function that takes the completion and returns the
\"terminated\" string."
  ;; FIXME: This implementation is not right since it only adds the terminator
  ;; in try-completion, so any completion-style that builds the completion via
  ;; all-completions won't get the terminator, and selecting an entry in
  ;; *Completions* won't get the terminator added either.
  (cond
   ((eq (car-safe action) 'boundaries)
    (let* ((suffix (cdr action))
           (bounds (completion-boundaries string table pred suffix))
           (terminator-regexp (if (consp terminator)
                                  (cdr terminator) (regexp-quote terminator)))
           (max (and terminator-regexp
                     (string-match terminator-regexp suffix))))
      (list* 'boundaries (car bounds)
             (min (cdr bounds) (or max (length suffix))))))
   ((eq action nil)
    (let ((comp (try-completion string table pred)))
      (if (consp terminator) (setq terminator (car terminator)))
      (if (eq comp t)
          (if (functionp terminator)
              (funcall terminator string)
            (concat string terminator))
        (if (and (stringp comp) (not (zerop (length comp)))
                 ;; Try to avoid the second call to try-completion, since
                 ;; it may be very inefficient (because `comp' made us
                 ;; jump to a new boundary, so we complete in that
                 ;; boundary with an empty start string).
                 (let ((newbounds (completion-boundaries comp table pred "")))
                   (< (car newbounds) (length comp)))
                 (eq (try-completion comp table pred) t))
            (if (functionp terminator)
                (funcall terminator comp)
              (concat comp terminator))
          comp))))
   ;; completion-table-with-terminator is always used for
   ;; "sub-completions" so it's only called if the terminator is missing,
   ;; in which case `test-completion' should return nil.
   ((eq action 'lambda) nil)
   (t
    ;; FIXME: We generally want the `try' and `all' behaviors to be
    ;; consistent so pcm can merge the `all' output to get the `try' output,
    ;; but that sometimes clashes with the need for `all' output to look
    ;; good in *Completions*.
    ;; (mapcar (lambda (s) (concat s terminator))
    ;;         (all-completions string table pred))))
    (complete-with-action action table string pred))))

(defun completion-table-with-predicate (table pred1 strict string pred2 action)
  "Make a completion table equivalent to TABLE but filtered through PRED1.
PRED1 is a function of one argument which returns non-nil if and only if the
argument is an element of TABLE which should be considered for completion.
STRING, PRED2, and ACTION are the usual arguments to completion tables,
as described in `try-completion', `all-completions', and `test-completion'.
If STRICT is t, the predicate always applies; if nil it only applies if
it does not reduce the set of possible completions to nothing.
Note: TABLE needs to be a proper completion table which obeys predicates."
  (cond
   ((and (not strict) (eq action 'lambda))
    ;; Ignore pred1 since it doesn't really have to apply anyway.
    (test-completion string table pred2))
   (t
    (or (complete-with-action action table string
                              (if (not (and pred1 pred2))
                                  (or pred1 pred2)
                                (lambda (x)
                                  ;; Call `pred1' first, so that `pred2'
                                  ;; really can't tell that `x' is in table.
                                  (and (funcall pred1 x) (funcall pred2 x)))))
        ;; If completion failed and we're not applying pred1 strictly, try
        ;; again without pred1.
        (and (not strict) pred1 pred2
             (complete-with-action action table string pred2))))))

(defun completion-table-in-turn (&rest tables)
  "Create a completion table that tries each table in TABLES in turn."
  ;; FIXME: the boundaries may come from TABLE1 even when the completion list
  ;; is returned by TABLE2 (because TABLE1 returned an empty list).
  (lambda (string pred action)
    (completion--some (lambda (table)
                        (complete-with-action action table string pred))
                      tables)))

;; (defmacro complete-in-turn (a b) `(completion-table-in-turn ,a ,b))
;; (defmacro dynamic-completion-table (fun) `(completion-table-dynamic ,fun))
(define-obsolete-function-alias
  'complete-in-turn 'completion-table-in-turn "23.1")
(define-obsolete-function-alias
  'dynamic-completion-table 'completion-table-dynamic "23.1")

;;; Minibuffer completion

(defgroup minibuffer nil
  "Controlling the behavior of the minibuffer."
  :link '(custom-manual "(emacs)Minibuffer")
  :group 'environment)

(defun minibuffer-message (message &rest args)
  "Temporarily display MESSAGE at the end of the minibuffer.
The text is displayed for `minibuffer-message-timeout' seconds,
or until the next input event arrives, whichever comes first.
Enclose MESSAGE in [...] if this is not yet the case.
If ARGS are provided, then pass MESSAGE through `format'."
  (if (not (minibufferp (current-buffer)))
      (progn
        (if args
            (apply 'message message args)
          (message "%s" message))
        (prog1 (sit-for (or minibuffer-message-timeout 1000000))
          (message nil)))
    ;; Clear out any old echo-area message to make way for our new thing.
    (message nil)
    (setq message (if (and (null args) (string-match-p "\\` *\\[.+\\]\\'" message))
                      ;; Make sure we can put-text-property.
                      (copy-sequence message)
                    (concat " [" message "]")))
    (when args (setq message (apply 'format message args)))
    (let ((ol (make-overlay (point-max) (point-max) nil t t))
          ;; A quit during sit-for normally only interrupts the sit-for,
          ;; but since minibuffer-message is used at the end of a command,
          ;; at a time when the command has virtually finished already, a C-g
          ;; should really cause an abort-recursive-edit instead (i.e. as if
          ;; the C-g had been typed at top-level).  Binding inhibit-quit here
          ;; is an attempt to get that behavior.
          (inhibit-quit t))
      (unwind-protect
          (progn
            (unless (zerop (length message))
              ;; The current C cursor code doesn't know to use the overlay's
              ;; marker's stickiness to figure out whether to place the cursor
              ;; before or after the string, so let's spoon-feed it the pos.
              (put-text-property 0 1 'cursor t message))
            (overlay-put ol 'after-string message)
            (sit-for (or minibuffer-message-timeout 1000000)))
        (delete-overlay ol)))))

(defun minibuffer-completion-contents ()
  "Return the user input in a minibuffer before point as a string.
That is what completion commands operate on."
  (buffer-substring (field-beginning) (point)))

(defun delete-minibuffer-contents ()
  "Delete all user input in a minibuffer.
If the current buffer is not a minibuffer, erase its entire contents."
  ;; We used to do `delete-field' here, but when file name shadowing
  ;; is on, the field doesn't cover the entire minibuffer contents.
  (delete-region (minibuffer-prompt-end) (point-max)))

(defvar completion-show-inline-help t
  "If non-nil, print helpful inline messages during completion.")

(defcustom completion-auto-help t
  "Non-nil means automatically provide help for invalid completion input.
If the value is t the *Completion* buffer is displayed whenever completion
is requested but cannot be done.
If the value is `lazy', the *Completions* buffer is only displayed after
the second failed attempt to complete."
  :type '(choice (const nil) (const t) (const lazy))
  :group 'minibuffer)

(defconst completion-styles-alist
  '((emacs21
     completion-emacs21-try-completion completion-emacs21-all-completions
     "Simple prefix-based completion.
I.e. when completing \"foo_bar\" (where _ is the position of point),
it will consider all completions candidates matching the glob
pattern \"foobar*\".")
    (emacs22
     completion-emacs22-try-completion completion-emacs22-all-completions
     "Prefix completion that only operates on the text before point.
I.e. when completing \"foo_bar\" (where _ is the position of point),
it will consider all completions candidates matching the glob
pattern \"foo*\" and will add back \"bar\" to the end of it.")
    (basic
     completion-basic-try-completion completion-basic-all-completions
     "Completion of the prefix before point and the suffix after point.
I.e. when completing \"foo_bar\" (where _ is the position of point),
it will consider all completions candidates matching the glob
pattern \"foo*bar*\".")
    (partial-completion
     completion-pcm-try-completion completion-pcm-all-completions
     "Completion of multiple words, each one taken as a prefix.
I.e. when completing \"l-co_h\" (where _ is the position of point),
it will consider all completions candidates matching the glob
pattern \"l*-co*h*\".
Furthermore, for completions that are done step by step in subfields,
the method is applied to all the preceding fields that do not yet match.
E.g. C-x C-f /u/mo/s TAB could complete to /usr/monnier/src.
Additionally the user can use the char \"*\" as a glob pattern.")
    (substring
     completion-substring-try-completion completion-substring-all-completions
     "Completion of the string taken as a substring.
I.e. when completing \"foo_bar\" (where _ is the position of point),
it will consider all completions candidates matching the glob
pattern \"*foo*bar*\".")
    (initials
     completion-initials-try-completion completion-initials-all-completions
     "Completion of acronyms and initialisms.
E.g. can complete M-x lch to list-command-history
and C-x C-f ~/sew to ~/src/emacs/work."))
  "List of available completion styles.
Each element has the form (NAME TRY-COMPLETION ALL-COMPLETIONS DOC):
where NAME is the name that should be used in `completion-styles',
TRY-COMPLETION is the function that does the completion (it should
follow the same calling convention as `completion-try-completion'),
ALL-COMPLETIONS is the function that lists the completions (it should
follow the calling convention of `completion-all-completions'),
and DOC describes the way this style of completion works.")

(defconst completion--styles-type
  `(repeat :tag "insert a new menu to add more styles"
           (choice ,@(mapcar (lambda (x) (list 'const (car x)))
                             completion-styles-alist))))
(defconst completion--cycling-threshold-type
  '(choice (const :tag "No cycling" nil)
           (const :tag "Always cycle" t)
           (integer :tag "Threshold")))

(defcustom completion-styles
  ;; First, use `basic' because prefix completion has been the standard
  ;; for "ever" and works well in most cases, so using it first
  ;; ensures that we obey previous behavior in most cases.
  '(basic
    ;; Then use `partial-completion' because it has proven to
    ;; be a very convenient extension.
    partial-completion
    ;; Finally use `emacs22' so as to maintain (in many/most cases)
    ;; the previous behavior that when completing "foobar" with point
    ;; between "foo" and "bar" the completion try to complete "foo"
    ;; and simply add "bar" to the end of the result.
    emacs22)
  "List of completion styles to use.
The available styles are listed in `completion-styles-alist'.

Note that `completion-category-overrides' may override these
styles for specific categories, such as files, buffers, etc."
  :type completion--styles-type
  :group 'minibuffer
  :version "23.1")

(defcustom completion-category-overrides
  '((buffer (styles . (basic substring))))
  "List of `completion-styles' overrides for specific categories.
Each override has the shape (CATEGORY . ALIST) where ALIST is
an association list that can specify properties such as:
- `styles': the list of `completion-styles' to use for that category.
- `cycle': the `completion-cycle-threshold' to use for that category.
Categories are symbols such as `buffer' and `file', used when
completing buffer and file names, respectively."
  :version "24.1"
  :type `(alist :key-type (choice :tag "Category"
				  (const buffer)
                                  (const file)
                                  (const unicode-name)
                                  symbol)
          :value-type
          (set :tag "Properties to override"
	   (cons :tag "Completion Styles"
		 (const :tag "Select a style from the menu;" styles)
		 ,completion--styles-type)
           (cons :tag "Completion Cycling"
		 (const :tag "Select one value from the menu." cycle)
                 ,completion--cycling-threshold-type))))

(defun completion--styles (metadata)
  (let* ((cat (completion-metadata-get metadata 'category))
         (over (assq 'styles (cdr (assq cat completion-category-overrides)))))
    (if over
        (delete-dups (append (cdr over) (copy-sequence completion-styles)))
       completion-styles)))

(defun completion-try-completion (string table pred point &optional metadata)
  "Try to complete STRING using completion table TABLE.
Only the elements of table that satisfy predicate PRED are considered.
POINT is the position of point within STRING.
The return value can be either nil to indicate that there is no completion,
t to indicate that STRING is the only possible completion,
or a pair (STRING . NEWPOINT) of the completed result string together with
a new position for point."
  (completion--some (lambda (style)
                      (funcall (nth 1 (assq style completion-styles-alist))
                               string table pred point))
                    (completion--styles (or metadata
                                            (completion-metadata
                                             (substring string 0 point)
                                             table pred)))))

(defun completion-all-completions (string table pred point &optional metadata)
  "List the possible completions of STRING in completion table TABLE.
Only the elements of table that satisfy predicate PRED are considered.
POINT is the position of point within STRING.
The return value is a list of completions and may contain the base-size
in the last `cdr'."
  ;; FIXME: We need to additionally return the info needed for the
  ;; second part of completion-base-position.
  (completion--some (lambda (style)
                      (funcall (nth 2 (assq style completion-styles-alist))
                               string table pred point))
                    (completion--styles (or metadata
                                            (completion-metadata
                                             (substring string 0 point)
                                             table pred)))))

(defun minibuffer--bitset (modified completions exact)
  (logior (if modified    4 0)
          (if completions 2 0)
          (if exact       1 0)))

(defun completion--replace (beg end newtext)
  "Replace the buffer text between BEG and END with NEWTEXT.
Moves point to the end of the new text."
  ;; The properties on `newtext' include things like
  ;; completions-first-difference, which we don't want to include
  ;; upon insertion.
  (set-text-properties 0 (length newtext) nil newtext)
  ;; Maybe this should be in subr.el.
  ;; You'd think this is trivial to do, but details matter if you want
  ;; to keep markers "at the right place" and be robust in the face of
  ;; after-change-functions that may themselves modify the buffer.
  (let ((prefix-len 0))
    ;; Don't touch markers in the shared prefix (if any).
    (while (and (< prefix-len (length newtext))
                (< (+ beg prefix-len) end)
                (eq (char-after (+ beg prefix-len))
                    (aref newtext prefix-len)))
      (setq prefix-len (1+ prefix-len)))
    (unless (zerop prefix-len)
      (setq beg (+ beg prefix-len))
      (setq newtext (substring newtext prefix-len))))
  (let ((suffix-len 0))
    ;; Don't touch markers in the shared suffix (if any).
    (while (and (< suffix-len (length newtext))
                (< beg (- end suffix-len))
                (eq (char-before (- end suffix-len))
                    (aref newtext (- (length newtext) suffix-len 1))))
      (setq suffix-len (1+ suffix-len)))
    (unless (zerop suffix-len)
      (setq end (- end suffix-len))
      (setq newtext (substring newtext 0 (- suffix-len))))
    (goto-char beg)
    (insert-and-inherit newtext)
    (delete-region (point) (+ (point) (- end beg)))
    (forward-char suffix-len)))

(defcustom completion-cycle-threshold nil
  "Number of completion candidates below which cycling is used.
Depending on this setting `minibuffer-complete' may use cycling,
like `minibuffer-force-complete'.
If nil, cycling is never used.
If t, cycling is always used.
If an integer, cycling is used as soon as there are fewer completion
candidates than this number."
  :version "24.1"
  :type completion--cycling-threshold-type)

(defun completion--cycle-threshold (metadata)
  (let* ((cat (completion-metadata-get metadata 'category))
         (over (assq 'cycle (cdr (assq cat completion-category-overrides)))))
    (if over (cdr over) completion-cycle-threshold)))

(defvar completion-all-sorted-completions nil)
(make-variable-buffer-local 'completion-all-sorted-completions)
(defvar completion-cycling nil)

(defvar completion-fail-discreetly nil
  "If non-nil, stay quiet when there  is no match.")

(defun completion--message (msg)
  (if completion-show-inline-help
      (minibuffer-message msg)))

(defun completion--do-completion (&optional try-completion-function
                                            expect-exact)
  "Do the completion and return a summary of what happened.
M = completion was performed, the text was Modified.
C = there were available Completions.
E = after completion we now have an Exact match.

 MCE
 000  0 no possible completion
 001  1 was already an exact and unique completion
 010  2 no completion happened
 011  3 was already an exact completion
 100  4 ??? impossible
 101  5 ??? impossible
 110  6 some completion happened
 111  7 completed to an exact completion

TRY-COMPLETION-FUNCTION is a function to use in place of `try-completion'.
EXPECT-EXACT, if non-nil, means that there is no need to tell the user
when the buffer's text is already an exact match."
  (let* ((beg (field-beginning))
         (end (field-end))
         (string (buffer-substring beg end))
         (md (completion--field-metadata beg))
         (comp (funcall (or try-completion-function
                            'completion-try-completion)
                        string
                        minibuffer-completion-table
                        minibuffer-completion-predicate
                        (- (point) beg)
                        md)))
    (cond
     ((null comp)
      (minibuffer-hide-completions)
      (unless completion-fail-discreetly
	(ding)
	(completion--message "No match"))
      (minibuffer--bitset nil nil nil))
     ((eq t comp)
      (minibuffer-hide-completions)
      (goto-char end)
      (completion--done string 'finished
                        (unless expect-exact "Sole completion"))
      (minibuffer--bitset nil nil t))   ;Exact and unique match.
     (t
      ;; `completed' should be t if some completion was done, which doesn't
      ;; include simply changing the case of the entered string.  However,
      ;; for appearance, the string is rewritten if the case changes.
      (let* ((comp-pos (cdr comp))
             (completion (car comp))
             (completed (not (eq t (compare-strings completion nil nil
                                                    string nil nil t))))
             (unchanged (eq t (compare-strings completion nil nil
                                               string nil nil nil))))
        (if unchanged
	    (goto-char end)
          ;; Insert in minibuffer the chars we got.
          (completion--replace beg end completion))
	;; Move point to its completion-mandated destination.
	(forward-char (- comp-pos (length completion)))

        (if (not (or unchanged completed))
            ;; The case of the string changed, but that's all.  We're not sure
            ;; whether this is a unique completion or not, so try again using
            ;; the real case (this shouldn't recurse again, because the next
            ;; time try-completion will return either t or the exact string).
            (completion--do-completion try-completion-function expect-exact)

          ;; It did find a match.  Do we match some possibility exactly now?
          (let* ((exact (test-completion completion
                                         minibuffer-completion-table
                                         minibuffer-completion-predicate))
                 (threshold (completion--cycle-threshold md))
                 (comps
                  ;; Check to see if we want to do cycling.  We do it
                  ;; here, after having performed the normal completion,
                  ;; so as to take advantage of the difference between
                  ;; try-completion and all-completions, for things
                  ;; like completion-ignored-extensions.
                  (when (and threshold
                             ;; Check that the completion didn't make
                             ;; us jump to a different boundary.
                             (or (not completed)
                                 (< (car (completion-boundaries
                                          (substring completion 0 comp-pos)
                                          minibuffer-completion-table
                                          minibuffer-completion-predicate
                                         ""))
                                   comp-pos)))
                   (completion-all-sorted-completions))))
            (completion--flush-all-sorted-completions)
            (cond
             ((and (consp (cdr comps)) ;; There's something to cycle.
                   (not (ignore-errors
                          ;; This signal an (intended) error if comps is too
                          ;; short or if completion-cycle-threshold is t.
                          (consp (nthcdr threshold comps)))))
              ;; Fewer than completion-cycle-threshold remaining
              ;; completions: let's cycle.
              (setq completed t exact t)
              (completion--cache-all-sorted-completions comps)
              (minibuffer-force-complete))
             (completed
              ;; We could also decide to refresh the completions,
              ;; if they're displayed (and assuming there are
              ;; completions left).
              (minibuffer-hide-completions)
              (if exact
                  ;; If completion did not put point at end of field,
                  ;; it's a sign that completion is not finished.
                  (completion--done completion
                                    (if (< comp-pos (length completion))
                                        'exact 'unknown))))
             ;; Show the completion table, if requested.
             ((not exact)
	      (if (case completion-auto-help
                    (lazy (eq this-command last-command))
                    (t completion-auto-help))
                  (minibuffer-completion-help)
                (completion--message "Next char not unique")))
             ;; If the last exact completion and this one were the same, it
             ;; means we've already given a "Complete, but not unique" message
             ;; and the user's hit TAB again, so now we give him help.
             (t
              (if (and (eq this-command last-command) completion-auto-help)
                  (minibuffer-completion-help))
              (completion--done completion 'exact
                                (unless expect-exact
                                  "Complete, but not unique"))))

            (minibuffer--bitset completed t exact))))))))

(defun minibuffer-complete ()
  "Complete the minibuffer contents as far as possible.
Return nil if there is no valid completion, else t.
If no characters can be completed, display a list of possible completions.
If you repeat this command after it displayed such a list,
scroll the window of possible completions."
  (interactive)
  ;; If the previous command was not this,
  ;; mark the completion buffer obsolete.
  (setq this-command 'completion-at-point)
  (unless (eq 'completion-at-point last-command)
    (completion--flush-all-sorted-completions)
    (setq minibuffer-scroll-window nil))

  (cond
   ;; If there's a fresh completion window with a live buffer,
   ;; and this command is repeated, scroll that window.
   ((window-live-p minibuffer-scroll-window)
    (let ((window minibuffer-scroll-window))
      (with-current-buffer (window-buffer window)
        (if (pos-visible-in-window-p (point-max) window)
            ;; If end is in view, scroll up to the beginning.
            (set-window-start window (point-min) nil)
          ;; Else scroll down one screen.
          (scroll-other-window))
        nil)))
   ;; If we're cycling, keep on cycling.
   ((and completion-cycling completion-all-sorted-completions)
    (minibuffer-force-complete)
    t)
   (t (case (completion--do-completion)
        (#b000 nil)
        (t     t)))))

(defun completion--cache-all-sorted-completions (comps)
  (add-hook 'after-change-functions
               'completion--flush-all-sorted-completions nil t)
  (setq completion-all-sorted-completions comps))

(defun completion--flush-all-sorted-completions (&rest _ignore)
  (remove-hook 'after-change-functions
               'completion--flush-all-sorted-completions t)
  (setq completion-cycling nil)
  (setq completion-all-sorted-completions nil))

(defun completion--metadata (string base md-at-point table pred)
  ;; Like completion-metadata, but for the specific case of getting the
  ;; metadata at `base', which tends to trigger pathological behavior for old
  ;; completion tables which don't understand `metadata'.
  (let ((bounds (completion-boundaries string table pred "")))
    (if (eq (car bounds) base) md-at-point
      (completion-metadata (substring string 0 base) table pred))))

(defun completion-all-sorted-completions ()
  (or completion-all-sorted-completions
      (let* ((start (field-beginning))
             (end (field-end))
             (string (buffer-substring start end))
             (md (completion--field-metadata start))
             (all (completion-all-completions
                   string
                   minibuffer-completion-table
                   minibuffer-completion-predicate
                   (- (point) start)
                   md))
             (last (last all))
             (base-size (or (cdr last) 0))
             (all-md (completion--metadata (buffer-substring-no-properties
                                            start (point))
                                           base-size md
                                           minibuffer-completion-table
                                           minibuffer-completion-predicate))
             (sort-fun (completion-metadata-get all-md 'cycle-sort-function)))
        (when last
          (setcdr last nil)
          (setq all (if sort-fun (funcall sort-fun all)
                      ;; Prefer shorter completions, by default.
                      (sort all (lambda (c1 c2) (< (length c1) (length c2))))))
          ;; Prefer recently used completions.
          (when (minibufferp)
            (let ((hist (symbol-value minibuffer-history-variable)))
              (setq all (sort all (lambda (c1 c2)
                                    (> (length (member c1 hist))
                                       (length (member c2 hist))))))))
          ;; Cache the result.  This is not just for speed, but also so that
          ;; repeated calls to minibuffer-force-complete can cycle through
          ;; all possibilities.
          (completion--cache-all-sorted-completions (nconc all base-size))))))

(defun minibuffer-force-complete ()
  "Complete the minibuffer to an exact match.
Repeated uses step through the possible completions."
  (interactive)
  ;; FIXME: Need to deal with the extra-size issue here as well.
  ;; FIXME: ~/src/emacs/t<M-TAB>/lisp/minibuffer.el completes to
  ;; ~/src/emacs/trunk/ and throws away lisp/minibuffer.el.
  (let* ((start (field-beginning))
         (end (field-end))
         ;; (md (completion--field-metadata start))
         (all (completion-all-sorted-completions))
         (base (+ start (or (cdr (last all)) 0))))
    (cond
     ((not (consp all))
        (completion--message
       (if all "No more completions" "No completions")))
     ((not (consp (cdr all)))
      (let ((mod (equal (car all) (buffer-substring-no-properties base end))))
        (if mod (completion--replace base end (car all)))
        (completion--done (buffer-substring-no-properties start (point))
                          'finished (unless mod "Sole completion"))))
     (t
      (completion--replace base end (car all))
      (completion--done (buffer-substring-no-properties start (point)) 'sole)
      ;; Set cycling after modifying the buffer since the flush hook resets it.
      (setq completion-cycling t)
      ;; If completing file names, (car all) may be a directory, so we'd now
      ;; have a new set of possible completions and might want to reset
      ;; completion-all-sorted-completions to nil, but we prefer not to,
      ;; so that repeated calls minibuffer-force-complete still cycle
      ;; through the previous possible completions.
      (let ((last (last all)))
        (setcdr last (cons (car all) (cdr last)))
        (completion--cache-all-sorted-completions (cdr all)))))))

(defvar minibuffer-confirm-exit-commands
  '(minibuffer-complete minibuffer-complete-word PC-complete PC-complete-word)
  "A list of commands which cause an immediately following
`minibuffer-complete-and-exit' to ask for extra confirmation.")

(defun minibuffer-complete-and-exit ()
  "Exit if the minibuffer contains a valid completion.
Otherwise, try to complete the minibuffer contents.  If
completion leads to a valid completion, a repetition of this
command will exit.

If `minibuffer-completion-confirm' is `confirm', do not try to
 complete; instead, ask for confirmation and accept any input if
 confirmed.
If `minibuffer-completion-confirm' is `confirm-after-completion',
 do not try to complete; instead, ask for confirmation if the
 preceding minibuffer command was a member of
 `minibuffer-confirm-exit-commands', and accept the input
 otherwise."
  (interactive)
  (let ((beg (field-beginning))
        (end (field-end)))
    (cond
     ;; Allow user to specify null string
     ((= beg end) (exit-minibuffer))
     ((test-completion (buffer-substring beg end)
                       minibuffer-completion-table
                       minibuffer-completion-predicate)
      ;; FIXME: completion-ignore-case has various slightly
      ;; incompatible meanings.  E.g. it can reflect whether the user
      ;; wants completion to pay attention to case, or whether the
      ;; string will be used in a context where case is significant.
      ;; E.g. usually try-completion should obey the first, whereas
      ;; test-completion should obey the second.
      (when completion-ignore-case
        ;; Fixup case of the field, if necessary.
        (let* ((string (buffer-substring beg end))
               (compl (try-completion
                       string
                       minibuffer-completion-table
                       minibuffer-completion-predicate)))
          (when (and (stringp compl) (not (equal string compl))
                     ;; If it weren't for this piece of paranoia, I'd replace
                     ;; the whole thing with a call to do-completion.
                     ;; This is important, e.g. when the current minibuffer's
                     ;; content is a directory which only contains a single
                     ;; file, so `try-completion' actually completes to
                     ;; that file.
                     (= (length string) (length compl)))
            (completion--replace beg end compl))))
      (exit-minibuffer))

     ((memq minibuffer-completion-confirm '(confirm confirm-after-completion))
      ;; The user is permitted to exit with an input that's rejected
      ;; by test-completion, after confirming her choice.
      (if (or (eq last-command this-command)
              ;; For `confirm-after-completion' we only ask for confirmation
              ;; if trying to exit immediately after typing TAB (this
              ;; catches most minibuffer typos).
              (and (eq minibuffer-completion-confirm 'confirm-after-completion)
                   (not (memq last-command minibuffer-confirm-exit-commands))))
          (exit-minibuffer)
        (minibuffer-message "Confirm")
        nil))

     (t
      ;; Call do-completion, but ignore errors.
      (case (condition-case nil
                (completion--do-completion nil 'expect-exact)
              (error 1))
        ((#b001 #b011) (exit-minibuffer))
        (#b111 (if (not minibuffer-completion-confirm)
                   (exit-minibuffer)
                 (minibuffer-message "Confirm")
                 nil))
        (t nil))))))

(defun completion--try-word-completion (string table predicate point md)
  (let ((comp (completion-try-completion string table predicate point md)))
    (if (not (consp comp))
        comp

      ;; If completion finds next char not unique,
      ;; consider adding a space or a hyphen.
      (when (= (length string) (length (car comp)))
        ;; Mark the added char with the `completion-word' property, so it
        ;; can be handled specially by completion styles such as
        ;; partial-completion.
        ;; We used to remove `partial-completion' from completion-styles
        ;; instead, but it was too blunt, leading to situations where SPC
        ;; was the only insertable char at point but minibuffer-complete-word
        ;; refused inserting it.
        (let ((exts (mapcar (lambda (str) (propertize str 'completion-try-word t))
                            '(" " "-")))
              (before (substring string 0 point))
              (after (substring string point))
	      tem)
	  (while (and exts (not (consp tem)))
            (setq tem (completion-try-completion
		       (concat before (pop exts) after)
		       table predicate (1+ point) md)))
	  (if (consp tem) (setq comp tem))))

      ;; Completing a single word is actually more difficult than completing
      ;; as much as possible, because we first have to find the "current
      ;; position" in `completion' in order to find the end of the word
      ;; we're completing.  Normally, `string' is a prefix of `completion',
      ;; which makes it trivial to find the position, but with fancier
      ;; completion (plus env-var expansion, ...) `completion' might not
      ;; look anything like `string' at all.
      (let* ((comppoint (cdr comp))
	     (completion (car comp))
	     (before (substring string 0 point))
	     (combined (concat before "\n" completion)))
        ;; Find in completion the longest text that was right before point.
        (when (string-match "\\(.+\\)\n.*?\\1" combined)
          (let* ((prefix (match-string 1 before))
                 ;; We used non-greedy match to make `rem' as long as possible.
                 (rem (substring combined (match-end 0)))
                 ;; Find in the remainder of completion the longest text
                 ;; that was right after point.
                 (after (substring string point))
                 (suffix (if (string-match "\\`\\(.+\\).*\n.*\\1"
                                           (concat after "\n" rem))
                             (match-string 1 after))))
            ;; The general idea is to try and guess what text was inserted
            ;; at point by the completion.  Problem is: if we guess wrong,
            ;; we may end up treating as "added by completion" text that was
            ;; actually painfully typed by the user.  So if we then cut
            ;; after the first word, we may throw away things the
            ;; user wrote.  So let's try to be as conservative as possible:
            ;; only cut after the first word, if we're reasonably sure that
            ;; our guess is correct.
            ;; Note: a quick survey on emacs-devel seemed to indicate that
            ;; nobody actually cares about the "word-at-a-time" feature of
            ;; minibuffer-complete-word, whose real raison-d'être is that it
            ;; tries to add "-" or " ".  One more reason to only cut after
            ;; the first word, if we're really sure we're right.
            (when (and (or suffix (zerop (length after)))
                       (string-match (concat
                                      ;; Make submatch 1 as small as possible
                                      ;; to reduce the risk of cutting
                                      ;; valuable text.
                                      ".*" (regexp-quote prefix) "\\(.*?\\)"
                                      (if suffix (regexp-quote suffix) "\\'"))
                                     completion)
                       ;; The new point in `completion' should also be just
                       ;; before the suffix, otherwise something more complex
                       ;; is going on, and we're not sure where we are.
                       (eq (match-end 1) comppoint)
                       ;; (match-beginning 1)..comppoint is now the stretch
                       ;; of text in `completion' that was completed at point.
		       (string-match "\\W" completion (match-beginning 1))
		       ;; Is there really something to cut?
		       (> comppoint (match-end 0)))
              ;; Cut after the first word.
              (let ((cutpos (match-end 0)))
                (setq completion (concat (substring completion 0 cutpos)
                                         (substring completion comppoint)))
                (setq comppoint cutpos)))))

	(cons completion comppoint)))))


(defun minibuffer-complete-word ()
  "Complete the minibuffer contents at most a single word.
After one word is completed as much as possible, a space or hyphen
is added, provided that matches some possible completion.
Return nil if there is no valid completion, else t."
  (interactive)
  (case (completion--do-completion 'completion--try-word-completion)
    (#b000 nil)
    (t     t)))

(defface completions-annotations '((t :inherit italic))
  "Face to use for annotations in the *Completions* buffer.")

(defcustom completions-format 'horizontal
  "Define the appearance and sorting of completions.
If the value is `vertical', display completions sorted vertically
in columns in the *Completions* buffer.
If the value is `horizontal', display completions sorted
horizontally in alphabetical order, rather than down the screen."
  :type '(choice (const horizontal) (const vertical))
  :group 'minibuffer
  :version "23.2")

(defun completion--insert-strings (strings)
  "Insert a list of STRINGS into the current buffer.
Uses columns to keep the listing readable but compact.
It also eliminates runs of equal strings."
  (when (consp strings)
    (let* ((length (apply 'max
			  (mapcar (lambda (s)
				    (if (consp s)
					(+ (string-width (car s))
                                           (string-width (cadr s)))
				      (string-width s)))
				  strings)))
	   (window (get-buffer-window (current-buffer) 0))
	   (wwidth (if window (1- (window-width window)) 79))
	   (columns (min
		     ;; At least 2 columns; at least 2 spaces between columns.
		     (max 2 (/ wwidth (+ 2 length)))
		     ;; Don't allocate more columns than we can fill.
		     ;; Windows can't show less than 3 lines anyway.
		     (max 1 (/ (length strings) 2))))
	   (colwidth (/ wwidth columns))
           (column 0)
	   (rows (/ (length strings) columns))
	   (row 0)
           (first t)
	   (laststring nil))
      ;; The insertion should be "sensible" no matter what choices were made
      ;; for the parameters above.
      (dolist (str strings)
	(unless (equal laststring str) ; Remove (consecutive) duplicates.
	  (setq laststring str)
          ;; FIXME: `string-width' doesn't pay attention to
          ;; `display' properties.
          (let ((length (if (consp str)
                            (+ (string-width (car str))
                               (string-width (cadr str)))
                          (string-width str))))
            (cond
	     ((eq completions-format 'vertical)
	      ;; Vertical format
	      (when (> row rows)
		(forward-line (- -1 rows))
		(setq row 0 column (+ column colwidth)))
	      (when (> column 0)
		(end-of-line)
		(while (> (current-column) column)
		  (if (eobp)
		      (insert "\n")
		    (forward-line 1)
		    (end-of-line)))
		(insert " \t")
		(set-text-properties (1- (point)) (point)
				     `(display (space :align-to ,column)))))
	     (t
	      ;; Horizontal format
	      (unless first
		(if (< wwidth (+ (max colwidth length) column))
		    ;; No space for `str' at point, move to next line.
		    (progn (insert "\n") (setq column 0))
		  (insert " \t")
		  ;; Leave the space unpropertized so that in the case we're
		  ;; already past the goal column, there is still
		  ;; a space displayed.
		  (set-text-properties (1- (point)) (point)
				       ;; We can't just set tab-width, because
				       ;; completion-setup-function will kill
				       ;; all local variables :-(
				       `(display (space :align-to ,column)))
		  nil))))
            (setq first nil)
            (if (not (consp str))
                (put-text-property (point) (progn (insert str) (point))
                                   'mouse-face 'highlight)
              (put-text-property (point) (progn (insert (car str)) (point))
                                 'mouse-face 'highlight)
              (add-text-properties (point) (progn (insert (cadr str)) (point))
                                   '(mouse-face nil
                                     face completions-annotations)))
	    (cond
	     ((eq completions-format 'vertical)
	      ;; Vertical format
	      (if (> column 0)
		  (forward-line)
		(insert "\n"))
	      (setq row (1+ row)))
	     (t
	      ;; Horizontal format
	      ;; Next column to align to.
	      (setq column (+ column
			      ;; Round up to a whole number of columns.
			      (* colwidth (ceiling length colwidth))))))))))))

(defvar completion-common-substring nil)
(make-obsolete-variable 'completion-common-substring nil "23.1")

(defvar completion-setup-hook nil
  "Normal hook run at the end of setting up a completion list buffer.
When this hook is run, the current buffer is the one in which the
command to display the completion list buffer was run.
The completion list buffer is available as the value of `standard-output'.
See also `display-completion-list'.")

(defface completions-first-difference
  '((t (:inherit bold)))
  "Face put on the first uncommon character in completions in *Completions* buffer."
  :group 'completion)

(defface completions-common-part
  '((t (:inherit default)))
  "Face put on the common prefix substring in completions in *Completions* buffer.
The idea of `completions-common-part' is that you can use it to
make the common parts less visible than normal, so that the rest
of the differing parts is, by contrast, slightly highlighted."
  :group 'completion)

(defun completion-hilit-commonality (completions prefix-len base-size)
  (when completions
    (let ((com-str-len (- prefix-len (or base-size 0))))
      (nconc
       (mapcar
        (lambda (elem)
          (let ((str
                 ;; Don't modify the string itself, but a copy, since the
                 ;; the string may be read-only or used for other purposes.
                 ;; Furthermore, since `completions' may come from
                 ;; display-completion-list, `elem' may be a list.
                 (if (consp elem)
                     (car (setq elem (cons (copy-sequence (car elem))
                                           (cdr elem))))
                   (setq elem (copy-sequence elem)))))
            (put-text-property 0
			       ;; If completion-boundaries returns incorrect
			       ;; values, all-completions may return strings
			       ;; that don't contain the prefix.
			       (min com-str-len (length str))
                               'font-lock-face 'completions-common-part
                               str)
            (if (> (length str) com-str-len)
                (put-text-property com-str-len (1+ com-str-len)
                                   'font-lock-face 'completions-first-difference
                                   str)))
          elem)
        completions)
       base-size))))

(defun display-completion-list (completions &optional common-substring)
  "Display the list of completions, COMPLETIONS, using `standard-output'.
Each element may be just a symbol or string
or may be a list of two strings to be printed as if concatenated.
If it is a list of two strings, the first is the actual completion
alternative, the second serves as annotation.
`standard-output' must be a buffer.
The actual completion alternatives, as inserted, are given `mouse-face'
properties of `highlight'.
At the end, this runs the normal hook `completion-setup-hook'.
It can find the completion buffer in `standard-output'.

The obsolete optional arg COMMON-SUBSTRING, if non-nil, should be a string
specifying a common substring for adding the faces
`completions-first-difference' and `completions-common-part' to
the completions buffer."
  (if common-substring
      (setq completions (completion-hilit-commonality
                         completions (length common-substring)
                         ;; We don't know the base-size.
                         nil)))
  (if (not (bufferp standard-output))
      ;; This *never* (ever) happens, so there's no point trying to be clever.
      (with-temp-buffer
	(let ((standard-output (current-buffer))
	      (completion-setup-hook nil))
	  (display-completion-list completions common-substring))
	(princ (buffer-string)))

    (with-current-buffer standard-output
      (goto-char (point-max))
      (if (null completions)
          (insert "There are no possible completions of what you have typed.")
        (insert "Possible completions are:\n")
        (completion--insert-strings completions))))

  ;; The hilit used to be applied via completion-setup-hook, so there
  ;; may still be some code that uses completion-common-substring.
  (with-no-warnings
    (let ((completion-common-substring common-substring))
      (run-hooks 'completion-setup-hook)))
  nil)

(defvar completion-extra-properties nil
  "Property list of extra properties of the current completion job.
These include:

`:annotation-function': Function to annotate the completions buffer.
   The function must accept one argument, a completion string,
   and return either nil or a string which is to be displayed
   next to the completion (but which is not part of the
   completion).  The function can access the completion data via
   `minibuffer-completion-table' and related variables.

`:exit-function': Function to run after completion is performed.

   The function must accept two arguments, STRING and STATUS.
   STRING is the text to which the field was completed, and
   STATUS indicates what kind of operation happened:
     `finished' - text is now complete
     `sole'     - text cannot be further completed but
                  completion is not finished
     `exact'    - text is a valid completion but may be further
                  completed.")

(defvar completion-annotate-function
  nil
  ;; Note: there's a lot of scope as for when to add annotations and
  ;; what annotations to add.  E.g. completing-help.el allowed adding
  ;; the first line of docstrings to M-x completion.  But there's
  ;; a tension, since such annotations, while useful at times, can
  ;; actually drown the useful information.
  ;; So completion-annotate-function should be used parsimoniously, or
  ;; else only used upon a user's request (e.g. we could add a command
  ;; to completion-list-mode to add annotations to the current
  ;; completions).
  "Function to add annotations in the *Completions* buffer.
The function takes a completion and should either return nil, or a string that
will be displayed next to the completion.  The function can access the
completion table and predicates via `minibuffer-completion-table' and related
variables.")
(make-obsolete-variable 'completion-annotate-function
                        'completion-extra-properties "24.1")

(defun completion--done (string &optional finished message)
  (let* ((exit-fun (plist-get completion-extra-properties :exit-function))
         (pre-msg (and exit-fun (current-message))))
    (assert (memq finished '(exact sole finished unknown)))
    ;; FIXME: exit-fun should receive `finished' as a parameter.
    (when exit-fun
      (when (eq finished 'unknown)
        (setq finished
              (if (eq (try-completion string
                                      minibuffer-completion-table
                                      minibuffer-completion-predicate)
                      t)
                  'finished 'exact)))
      (funcall exit-fun string finished))
    (when (and message
               ;; Don't output any message if the exit-fun already did so.
               (equal pre-msg (and exit-fun (current-message))))
      (completion--message message))))

(defun minibuffer-completion-help ()
  "Display a list of possible completions of the current minibuffer contents."
  (interactive)
  (message "Making completion list...")
  (let* ((start (field-beginning))
         (end (field-end))
         (string (field-string))
         (md (completion--field-metadata start))
         (completions (completion-all-completions
                       string
                       minibuffer-completion-table
                       minibuffer-completion-predicate
                       (- (point) (field-beginning))
                       md)))
    (message nil)
    (if (or (null completions)
            (and (not (consp (cdr completions)))
                 (equal (car completions) string)))
        (progn
          ;; If there are no completions, or if the current input is already
          ;; the sole completion, then hide (previous&stale) completions.
          (minibuffer-hide-completions)
          (ding)
          (minibuffer-message
           (if completions "Sole completion" "No completions")))

      (let* ((last (last completions))
             (base-size (cdr last))
             (prefix (unless (zerop base-size) (substring string 0 base-size)))
             (all-md (completion--metadata (buffer-substring-no-properties
                                            start (point))
                                           base-size md
                                           minibuffer-completion-table
                                           minibuffer-completion-predicate))
             (afun (or (completion-metadata-get all-md 'annotation-function)
                       (plist-get completion-extra-properties
                                  :annotation-function)
                       completion-annotate-function))
             ;; If the *Completions* buffer is shown in a new
             ;; window, mark it as softly-dedicated, so bury-buffer in
             ;; minibuffer-hide-completions will know whether to
             ;; delete the window or not.
             (display-buffer-mark-dedicated 'soft))
        (with-output-to-temp-buffer "*Completions*"
          ;; Remove the base-size tail because `sort' requires a properly
          ;; nil-terminated list.
          (when last (setcdr last nil))
          (setq completions
                ;; FIXME: This function is for the output of all-completions,
                ;; not completion-all-completions.  Often it's the same, but
                ;; not always.
                (let ((sort-fun (completion-metadata-get
                                 all-md 'display-sort-function)))
                  (if sort-fun
                      (funcall sort-fun completions)
                    (sort completions 'string-lessp))))
          (when afun
            (setq completions
                  (mapcar (lambda (s)
                            (let ((ann (funcall afun s)))
                              (if ann (list s ann) s)))
                          completions)))

          (with-current-buffer standard-output
            (set (make-local-variable 'completion-base-position)
                 (list (+ start base-size)
                       ;; FIXME: We should pay attention to completion
                       ;; boundaries here, but currently
                       ;; completion-all-completions does not give us the
                       ;; necessary information.
                       end))
            (set (make-local-variable 'completion-list-insert-choice-function)
                 (let ((ctable minibuffer-completion-table)
                       (cpred minibuffer-completion-predicate)
                       (cprops completion-extra-properties))
                   (lambda (start end choice)
                     (unless (or (zerop (length prefix))
                                 (equal prefix
                                        (buffer-substring-no-properties
                                         (max (point-min)
                                              (- start (length prefix)))
                                         start)))
                       (message "*Completions* out of date"))
                     ;; FIXME: Use `md' to do quoting&terminator here.
                     (completion--replace start end choice)
                     (let* ((minibuffer-completion-table ctable)
                            (minibuffer-completion-predicate cpred)
                            (completion-extra-properties cprops)
                            (result (concat prefix choice))
                            (bounds (completion-boundaries
                                     result ctable cpred "")))
                       ;; If the completion introduces a new field, then
                       ;; completion is not finished.
                       (completion--done result
                                         (if (eq (car bounds) (length result))
                                             'exact 'finished)))))))

          (display-completion-list completions))))
    nil))

(defun minibuffer-hide-completions ()
  "Get rid of an out-of-date *Completions* buffer."
  ;; FIXME: We could/should use minibuffer-scroll-window here, but it
  ;; can also point to the minibuffer-parent-window, so it's a bit tricky.
  (let ((win (get-buffer-window "*Completions*" 0)))
    (if win (with-selected-window win (bury-buffer)))))

(defun exit-minibuffer ()
  "Terminate this minibuffer argument."
  (interactive)
  ;; If the command that uses this has made modifications in the minibuffer,
  ;; we don't want them to cause deactivation of the mark in the original
  ;; buffer.
  ;; A better solution would be to make deactivate-mark buffer-local
  ;; (or to turn it into a list of buffers, ...), but in the mean time,
  ;; this should do the trick in most cases.
  (setq deactivate-mark nil)
  (throw 'exit nil))

(defun self-insert-and-exit ()
  "Terminate minibuffer input."
  (interactive)
  (if (characterp last-command-event)
      (call-interactively 'self-insert-command)
    (ding))
  (exit-minibuffer))

(defvar completion-in-region-functions nil
  "Wrapper hook around `completion-in-region'.
The functions on this special hook are called with 5 arguments:
  NEXT-FUN START END COLLECTION PREDICATE.
NEXT-FUN is a function of four arguments (START END COLLECTION PREDICATE)
that performs the default operation.  The other four arguments are like
the ones passed to `completion-in-region'.  The functions on this hook
are expected to perform completion on START..END using COLLECTION
and PREDICATE, either by calling NEXT-FUN or by doing it themselves.")

(defvar completion-in-region--data nil)

(defvar completion-in-region-mode-predicate nil
  "Predicate to tell `completion-in-region-mode' when to exit.
It is called with no argument and should return nil when
`completion-in-region-mode' should exit (and hence pop down
the *Completions* buffer).")

(defvar completion-in-region-mode--predicate nil
  "Copy of the value of `completion-in-region-mode-predicate'.
This holds the value `completion-in-region-mode-predicate' had when
we entered `completion-in-region-mode'.")

(defun completion-in-region (start end collection &optional predicate)
  "Complete the text between START and END using COLLECTION.
Return nil if there is no valid completion, else t.
Point needs to be somewhere between START and END.
PREDICATE (a function called with no arguments) says when to
exit."
  (assert (<= start (point)) (<= (point) end))
  (with-wrapper-hook
      ;; FIXME: Maybe we should use this hook to provide a "display
      ;; completions" operation as well.
      completion-in-region-functions (start end collection predicate)
    (let ((minibuffer-completion-table collection)
          (minibuffer-completion-predicate predicate)
          (ol (make-overlay start end nil nil t)))
      (overlay-put ol 'field 'completion)
      ;; HACK: if the text we are completing is already in a field, we
      ;; want the completion field to take priority (e.g. Bug#6830).
      (overlay-put ol 'priority 100)
      (when completion-in-region-mode-predicate
        (completion-in-region-mode 1)
        (setq completion-in-region--data
	      (list (current-buffer) start end collection)))
      (unwind-protect
          (call-interactively 'minibuffer-complete)
        (delete-overlay ol)))))

(defvar completion-in-region-mode-map
  (let ((map (make-sparse-keymap)))
    ;; FIXME: Only works if completion-in-region-mode was activated via
    ;; completion-at-point called directly.
    (define-key map "\M-?" 'completion-help-at-point)
    (define-key map "\t" 'completion-at-point)
    map)
  "Keymap activated during `completion-in-region'.")

;; It is difficult to know when to exit completion-in-region-mode (i.e. hide
;; the *Completions*).
;; - lisp-mode: never.
;; - comint: only do it if you hit SPC at the right time.
;; - pcomplete: pop it down on SPC or after some time-delay.
;; - semantic: use a post-command-hook check similar to this one.
(defun completion-in-region--postch ()
  (or unread-command-events ;Don't pop down the completions in the middle of
                            ;mouse-drag-region/mouse-set-point.
      (and completion-in-region--data
           (and (eq (car completion-in-region--data)
                    (current-buffer))
                (>= (point) (nth 1 completion-in-region--data))
                (<= (point)
                    (save-excursion
                      (goto-char (nth 2 completion-in-region--data))
                      (line-end-position)))
		(funcall completion-in-region-mode--predicate)))
      (completion-in-region-mode -1)))

;; (defalias 'completion-in-region--prech 'completion-in-region--postch)

(define-minor-mode completion-in-region-mode
  "Transient minor mode used during `completion-in-region'.
With a prefix argument ARG, enable the modemode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil."
  :global t
  (setq completion-in-region--data nil)
  ;; (remove-hook 'pre-command-hook #'completion-in-region--prech)
  (remove-hook 'post-command-hook #'completion-in-region--postch)
  (setq minor-mode-overriding-map-alist
        (delq (assq 'completion-in-region-mode minor-mode-overriding-map-alist)
              minor-mode-overriding-map-alist))
  (if (null completion-in-region-mode)
      (unless (equal "*Completions*" (buffer-name (window-buffer)))
	(minibuffer-hide-completions))
    ;; (add-hook 'pre-command-hook #'completion-in-region--prech)
    (assert completion-in-region-mode-predicate)
    (setq completion-in-region-mode--predicate
	  completion-in-region-mode-predicate)
    (add-hook 'post-command-hook #'completion-in-region--postch)
    (push `(completion-in-region-mode . ,completion-in-region-mode-map)
          minor-mode-overriding-map-alist)))

;; Define-minor-mode added our keymap to minor-mode-map-alist, but we want it
;; on minor-mode-overriding-map-alist instead.
(setq minor-mode-map-alist
      (delq (assq 'completion-in-region-mode minor-mode-map-alist)
            minor-mode-map-alist))

(defvar completion-at-point-functions '(tags-completion-at-point-function)
  "Special hook to find the completion table for the thing at point.
Each function on this hook is called in turns without any argument and should
return either nil to mean that it is not applicable at point,
or a function of no argument to perform completion (discouraged),
or a list of the form (START END COLLECTION . PROPS) where
 START and END delimit the entity to complete and should include point,
 COLLECTION is the completion table to use to complete it, and
 PROPS is a property list for additional information.
Currently supported properties are all the properties that can appear in
`completion-extra-properties' plus:
 `:predicate'	a predicate that completion candidates need to satisfy.
 `:exclusive'	If `no', means that if the completion table fails to
   match the text at point, then instead of reporting a completion
   failure, the completion should try the next completion function.")

(defvar completion--capf-misbehave-funs nil
  "List of functions found on `completion-at-point-functions' that misbehave.
These are functions that neither return completion data nor a completion
function but instead perform completion right away.")
(defvar completion--capf-safe-funs nil
  "List of well-behaved functions found on `completion-at-point-functions'.
These are functions which return proper completion data rather than
a completion function or god knows what else.")

(defun completion--capf-wrapper (fun which)
  ;; FIXME: The safe/misbehave handling assumes that a given function will
  ;; always return the same kind of data, but this breaks down with functions
  ;; like comint-completion-at-point or mh-letter-completion-at-point, which
  ;; could be sometimes safe and sometimes misbehaving (and sometimes neither).
  (if (case which
        (all t)
        (safe (member fun completion--capf-safe-funs))
        (optimist (not (member fun completion--capf-misbehave-funs))))
      (let ((res (funcall fun)))
        (cond
         ((and (consp res) (not (functionp res)))
          (unless (member fun completion--capf-safe-funs)
            (push fun completion--capf-safe-funs))
          (and (eq 'no (plist-get (nthcdr 3 res) :exclusive))
               ;; FIXME: Here we'd need to decide whether there are
               ;; valid completions against the current text.  But this depends
               ;; on the actual completion UI (e.g. with the default completion
               ;; it depends on completion-style) ;-(
               ;; We approximate this result by checking whether prefix
               ;; completion might work, which means that non-prefix completion
               ;; will not work (or not right) for completion functions that
               ;; are non-exclusive.
               (null (try-completion (buffer-substring-no-properties
                                      (car res) (point))
                                     (nth 2 res)
                                     (plist-get (nthcdr 3 res) :predicate)))
               (setq res nil)))
         ((not (or (listp res) (functionp res)))
          (unless (member fun completion--capf-misbehave-funs)
            (message
             "Completion function %S uses a deprecated calling convention" fun)
            (push fun completion--capf-misbehave-funs))))
        (if res (cons fun res)))))

(defun completion-at-point ()
  "Perform completion on the text around point.
The completion method is determined by `completion-at-point-functions'."
  (interactive)
  (let ((res (run-hook-wrapped 'completion-at-point-functions
                               #'completion--capf-wrapper 'all)))
    (pcase res
     (`(,_ . ,(and (pred functionp) f)) (funcall f))
     (`(,hookfun . (,start ,end ,collection . ,plist))
      (let* ((completion-extra-properties plist)
             (completion-in-region-mode-predicate
              (lambda ()
                ;; We're still in the same completion field.
                (eq (car-safe (funcall hookfun)) start))))
        (completion-in-region start end collection
                              (plist-get plist :predicate))))
     ;; Maybe completion already happened and the function returned t.
     (_ (cdr res)))))

(defun completion-help-at-point ()
  "Display the completions on the text around point.
The completion method is determined by `completion-at-point-functions'."
  (interactive)
  (let ((res (run-hook-wrapped 'completion-at-point-functions
                               ;; Ignore misbehaving functions.
                               #'completion--capf-wrapper 'optimist)))
    (pcase res
      (`(,_ . ,(and (pred functionp) f))
       (message "Don't know how to show completions for %S" f))
     (`(,hookfun . (,start ,end ,collection . ,plist))
      (let* ((minibuffer-completion-table collection)
             (minibuffer-completion-predicate (plist-get plist :predicate))
             (completion-extra-properties plist)
             (completion-in-region-mode-predicate
              (lambda ()
                ;; We're still in the same completion field.
                (eq (car-safe (funcall hookfun)) start)))
             (ol (make-overlay start end nil nil t)))
        ;; FIXME: We should somehow (ab)use completion-in-region-function or
        ;; introduce a corresponding hook (plus another for word-completion,
        ;; and another for force-completion, maybe?).
        (overlay-put ol 'field 'completion)
	(overlay-put ol 'priority 100)
        (completion-in-region-mode 1)
        (setq completion-in-region--data
	      (list (current-buffer) start end collection))
        (unwind-protect
            (call-interactively 'minibuffer-completion-help)
          (delete-overlay ol))))
     (`(,hookfun . ,_)
      ;; The hook function already performed completion :-(
      ;; Not much we can do at this point.
      (message "%s already performed completion!" hookfun)
      nil)
     (_ (message "Nothing to complete at point")))))

;;; Key bindings.

(let ((map minibuffer-local-map))
  (define-key map "\C-g" 'abort-recursive-edit)
  (define-key map "\r" 'exit-minibuffer)
  (define-key map "\n" 'exit-minibuffer))

(defvar minibuffer-local-completion-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "\t" 'minibuffer-complete)
    ;; M-TAB is already abused for many other purposes, so we should find
    ;; another binding for it.
    ;; (define-key map "\e\t" 'minibuffer-force-complete)
    (define-key map " " 'minibuffer-complete-word)
    (define-key map "?" 'minibuffer-completion-help)
    map)
  "Local keymap for minibuffer input with completion.")

(defvar minibuffer-local-must-match-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-completion-map)
    (define-key map "\r" 'minibuffer-complete-and-exit)
    (define-key map "\n" 'minibuffer-complete-and-exit)
    map)
  "Local keymap for minibuffer input with completion, for exact match.")

(defvar minibuffer-local-filename-completion-map
  (let ((map (make-sparse-keymap)))
    (define-key map " " nil)
    map)
  "Local keymap for minibuffer input with completion for filenames.
Gets combined either with `minibuffer-local-completion-map' or
with `minibuffer-local-must-match-map'.")

(defvar minibuffer-local-filename-must-match-map (make-sparse-keymap))
(make-obsolete-variable 'minibuffer-local-filename-must-match-map nil "24.1")
(define-obsolete-variable-alias 'minibuffer-local-must-match-filename-map
  'minibuffer-local-filename-must-match-map "23.1")

(let ((map minibuffer-local-ns-map))
  (define-key map " " 'exit-minibuffer)
  (define-key map "\t" 'exit-minibuffer)
  (define-key map "?" 'self-insert-and-exit))

(defvar minibuffer-inactive-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map "e" 'find-file-other-frame)
    (define-key map "f" 'find-file-other-frame)
    (define-key map "b" 'switch-to-buffer-other-frame)
    (define-key map "i" 'info)
    (define-key map "m" 'mail)
    (define-key map "n" 'make-frame)
    (define-key map [mouse-1] (lambda () (interactive)
				(with-current-buffer "*Messages*"
				  (goto-char (point-max))
				  (display-buffer (current-buffer)))))
    ;; So the global down-mouse-1 binding doesn't clutter the execution of the
    ;; above mouse-1 binding.
    (define-key map [down-mouse-1] #'ignore)
    map)
  "Keymap for use in the minibuffer when it is not active.
The non-mouse bindings in this keymap can only be used in minibuffer-only
frames, since the minibuffer can normally not be selected when it is
not active.")

(define-derived-mode minibuffer-inactive-mode nil "InactiveMinibuffer"
  :abbrev-table nil          ;abbrev.el is not loaded yet during dump.
  ;; Note: this major mode is called from minibuf.c.
  "Major mode to use in the minibuffer when it is not active.
This is only used when the minibuffer area has no active minibuffer.")

;;; Completion tables.

(defun minibuffer--double-dollars (str)
  (replace-regexp-in-string "\\$" "$$" str))

(defun completion--make-envvar-table ()
  (mapcar (lambda (enventry)
            (substring enventry 0 (string-match-p "=" enventry)))
          process-environment))

(defconst completion--embedded-envvar-re
  (concat "\\(?:^\\|[^$]\\(?:\\$\\$\\)*\\)"
          "$\\([[:alnum:]_]*\\|{\\([^}]*\\)\\)\\'"))

(defun completion--embedded-envvar-table (string _pred action)
  "Completion table for envvars embedded in a string.
The envvar syntax (and escaping) rules followed by this table are the
same as `substitute-in-file-name'."
  ;; We ignore `pred', because the predicates passed to us via
  ;; read-file-name-internal are not 100% correct and fail here:
  ;; e.g. we get predicates like file-directory-p there, whereas the filename
  ;; completed needs to be passed through substitute-in-file-name before it
  ;; can be passed to file-directory-p.
  (when (string-match completion--embedded-envvar-re string)
    (let* ((beg (or (match-beginning 2) (match-beginning 1)))
           (table (completion--make-envvar-table))
           (prefix (substring string 0 beg)))
      (cond
       ((eq action 'lambda)
        ;; This table is expected to be used in conjunction with some
        ;; other table that provides the "main" completion.  Let the
        ;; other table handle the test-completion case.
        nil)
       ((or (eq (car-safe action) 'boundaries) (eq action 'metadata))
        ;; Only return boundaries/metadata if there's something to complete,
        ;; since otherwise when we're used in
        ;; completion-table-in-turn, we could return boundaries and
        ;; let some subsequent table return a list of completions.
        ;; FIXME: Maybe it should rather be fixed in
        ;; completion-table-in-turn instead, but it's difficult to
        ;; do it efficiently there.
        (when (try-completion (substring string beg) table nil)
          ;; Compute the boundaries of the subfield to which this
          ;; completion applies.
          (if (eq action 'metadata)
              '(metadata (category . environment-variable))
            (let ((suffix (cdr action)))
              (list* 'boundaries
                     (or (match-beginning 2) (match-beginning 1))
                     (when (string-match "[^[:alnum:]_]" suffix)
                       (match-beginning 0)))))))
       (t
        (if (eq (aref string (1- beg)) ?{)
            (setq table (apply-partially 'completion-table-with-terminator
                                         "}" table)))
        ;; Even if file-name completion is case-insensitive, we want
        ;; envvar completion to be case-sensitive.
        (let ((completion-ignore-case nil))
          (completion-table-with-context
           prefix table (substring string beg) nil action)))))))

(defun completion-file-name-table (string pred action)
  "Completion table for file names."
  (condition-case nil
      (cond
       ((eq action 'metadata) '(metadata (category . file)))
       ((eq (car-safe action) 'boundaries)
        (let ((start (length (file-name-directory string)))
              (end (string-match-p "/" (cdr action))))
          (list* 'boundaries
                 ;; if `string' is "C:" in w32, (file-name-directory string)
                 ;; returns "C:/", so `start' is 3 rather than 2.
                 ;; Not quite sure what is The Right Fix, but clipping it
                 ;; back to 2 will work for this particular case.  We'll
                 ;; see if we can come up with a better fix when we bump
                 ;; into more such problematic cases.
                 (min start (length string)) end)))

       ((eq action 'lambda)
        (if (zerop (length string))
            nil          ;Not sure why it's here, but it probably doesn't harm.
          (funcall (or pred 'file-exists-p) string)))

       (t
        (let* ((name (file-name-nondirectory string))
               (specdir (file-name-directory string))
               (realdir (or specdir default-directory)))

          (cond
           ((null action)
            (let ((comp (file-name-completion name realdir pred)))
              (if (stringp comp)
                  (concat specdir comp)
                comp)))

           ((eq action t)
            (let ((all (file-name-all-completions name realdir)))

              ;; Check the predicate, if necessary.
              (unless (memq pred '(nil file-exists-p))
                (let ((comp ())
                      (pred
                       (if (eq pred 'file-directory-p)
                           ;; Brute-force speed up for directory checking:
                           ;; Discard strings which don't end in a slash.
                           (lambda (s)
                             (let ((len (length s)))
                               (and (> len 0) (eq (aref s (1- len)) ?/))))
                         ;; Must do it the hard (and slow) way.
                         pred)))
                  (let ((default-directory (expand-file-name realdir)))
                    (dolist (tem all)
                      (if (funcall pred tem) (push tem comp))))
                  (setq all (nreverse comp))))

              all))))))
    (file-error nil)))               ;PCM often calls with invalid directories.

(defvar read-file-name-predicate nil
  "Current predicate used by `read-file-name-internal'.")
(make-obsolete-variable 'read-file-name-predicate
                        "use the regular PRED argument" "23.2")

(defun completion--file-name-table (string pred action)
  "Internal subroutine for `read-file-name'.  Do not call this.
This is a completion table for file names, like `completion-file-name-table'
except that it passes the file name through `substitute-in-file-name'."
  (cond
   ((eq (car-safe action) 'boundaries)
    ;; For the boundaries, we can't really delegate to
    ;; substitute-in-file-name+completion-file-name-table and then fix
    ;; them up (as we do for the other actions), because it would
    ;; require us to track the relationship between `str' and
    ;; `string', which is difficult.  And in any case, if
    ;; substitute-in-file-name turns "fo-$TO-ba" into "fo-o/b-ba",
    ;; there's no way for us to return proper boundaries info, because
    ;; the boundary is not (yet) in `string'.
    ;;
    ;; FIXME: Actually there is a way to return correct boundaries
    ;; info, at the condition of modifying the all-completions
    ;; return accordingly. But for now, let's not bother.
    (completion-file-name-table string pred action))

   (t
    (let* ((default-directory
             (if (stringp pred)
                 ;; It used to be that `pred' was abused to pass `dir'
                 ;; as an argument.
                 (prog1 (file-name-as-directory (expand-file-name pred))
                   (setq pred nil))
               default-directory))
           (str (condition-case nil
                    (substitute-in-file-name string)
                  (error string)))
           (comp (completion-file-name-table
                  str
		  (with-no-warnings (or pred read-file-name-predicate))
		  action)))

      (cond
       ((stringp comp)
        ;; Requote the $s before returning the completion.
        (minibuffer--double-dollars comp))
       ((and (null action) comp
             ;; Requote the $s before checking for changes.
             (setq str (minibuffer--double-dollars str))
             (not (string-equal string str)))
        ;; If there's no real completion, but substitute-in-file-name
        ;; changed the string, then return the new string.
        str)
       (t comp))))))

(defalias 'read-file-name-internal
  (completion-table-in-turn 'completion--embedded-envvar-table
                            'completion--file-name-table)
  "Internal subroutine for `read-file-name'.  Do not call this.")

(defvar read-file-name-function 'read-file-name-default
  "The function called by `read-file-name' to do its work.
It should accept the same arguments as `read-file-name'.")

(defcustom read-file-name-completion-ignore-case
  (if (memq system-type '(ms-dos windows-nt darwin cygwin))
      t nil)
  "Non-nil means when reading a file name completion ignores case."
  :group 'minibuffer
  :type 'boolean
  :version "22.1")

(defcustom insert-default-directory t
  "Non-nil means when reading a filename start with default dir in minibuffer.

When the initial minibuffer contents show a name of a file or a directory,
typing RETURN without editing the initial contents is equivalent to typing
the default file name.

If this variable is non-nil, the minibuffer contents are always
initially non-empty, and typing RETURN without editing will fetch the
default name, if one is provided.  Note however that this default name
is not necessarily the same as initial contents inserted in the minibuffer,
if the initial contents is just the default directory.

If this variable is nil, the minibuffer often starts out empty.  In
that case you may have to explicitly fetch the next history element to
request the default name; typing RETURN without editing will leave
the minibuffer empty.

For some commands, exiting with an empty minibuffer has a special meaning,
such as making the current buffer visit no file in the case of
`set-visited-file-name'."
  :group 'minibuffer
  :type 'boolean)

;; Not always defined, but only called if next-read-file-uses-dialog-p says so.
(declare-function x-file-dialog "xfns.c"
                  (prompt dir &optional default-filename mustmatch only-dir-p))

(defun read-file-name--defaults (&optional dir initial)
  (let ((default
	  (cond
	   ;; With non-nil `initial', use `dir' as the first default.
	   ;; Essentially, this mean reversing the normal order of the
	   ;; current directory name and the current file name, i.e.
	   ;; 1. with normal file reading:
	   ;; 1.1. initial input is the current directory
	   ;; 1.2. the first default is the current file name
	   ;; 2. with non-nil `initial' (e.g. for `find-alternate-file'):
	   ;; 2.2. initial input is the current file name
	   ;; 2.1. the first default is the current directory
	   (initial (abbreviate-file-name dir))
	   ;; In file buffers, try to get the current file name
	   (buffer-file-name
	    (abbreviate-file-name buffer-file-name))))
	(file-name-at-point
	 (run-hook-with-args-until-success 'file-name-at-point-functions)))
    (when file-name-at-point
      (setq default (delete-dups
		     (delete "" (delq nil (list file-name-at-point default))))))
    ;; Append new defaults to the end of existing `minibuffer-default'.
    (append
     (if (listp minibuffer-default) minibuffer-default (list minibuffer-default))
     (if (listp default) default (list default)))))

(defun read-file-name (prompt &optional dir default-filename mustmatch initial predicate)
  "Read file name, prompting with PROMPT and completing in directory DIR.
Value is not expanded---you must call `expand-file-name' yourself.
Default name to DEFAULT-FILENAME if user exits the minibuffer with
the same non-empty string that was inserted by this function.
 (If DEFAULT-FILENAME is omitted, the visited file name is used,
  except that if INITIAL is specified, that combined with DIR is used.
  If DEFAULT-FILENAME is a list of file names, the first file name is used.)
If the user exits with an empty minibuffer, this function returns
an empty string.  (This can only happen if the user erased the
pre-inserted contents or if `insert-default-directory' is nil.)

Fourth arg MUSTMATCH can take the following values:
- nil means that the user can exit with any input.
- t means that the user is not allowed to exit unless
  the input is (or completes to) an existing file.
- `confirm' means that the user can exit with any input, but she needs
  to confirm her choice if the input is not an existing file.
- `confirm-after-completion' means that the user can exit with any
  input, but she needs to confirm her choice if she called
  `minibuffer-complete' right before `minibuffer-complete-and-exit'
  and the input is not an existing file.
- anything else behaves like t except that typing RET does not exit if it
  does non-null completion.

Fifth arg INITIAL specifies text to start with.

If optional sixth arg PREDICATE is non-nil, possible completions and
the resulting file name must satisfy (funcall PREDICATE NAME).
DIR should be an absolute directory name.  It defaults to the value of
`default-directory'.

If this command was invoked with the mouse, use a graphical file
dialog if `use-dialog-box' is non-nil, and the window system or X
toolkit in use provides a file dialog box, and DIR is not a
remote file.  For graphical file dialogs, any of the special values
of MUSTMATCH `confirm' and `confirm-after-completion' are
treated as equivalent to nil.  Some graphical file dialogs respect
a MUSTMATCH value of t, and some do not (or it only has a cosmetic
effect, and does not actually prevent the user from entering a
non-existent file).

See also `read-file-name-completion-ignore-case'
and `read-file-name-function'."
  ;; If x-gtk-use-old-file-dialog = t (xg_get_file_with_selection),
  ;; then MUSTMATCH is enforced.  But with newer Gtk
  ;; (xg_get_file_with_chooser), it only has a cosmetic effect.
  ;; The user can still type a non-existent file name.
  (funcall (or read-file-name-function #'read-file-name-default)
           prompt dir default-filename mustmatch initial predicate))

;; minibuffer-completing-file-name is a variable used internally in minibuf.c
;; to determine whether to use minibuffer-local-filename-completion-map or
;; minibuffer-local-completion-map.  It shouldn't be exported to Elisp.
;; FIXME: Actually, it is also used in rfn-eshadow.el we'd otherwise have to
;; use (eq minibuffer-completion-table #'read-file-name-internal), which is
;; probably even worse.  Maybe We should add some read-file-name-setup-hook
;; instead, but for now, let's keep this non-obsolete.
;;(make-obsolete-variable 'minibuffer-completing-file-name nil "24.1" 'get)

(defun read-file-name-default (prompt &optional dir default-filename mustmatch initial predicate)
  "Default method for reading file names.
See `read-file-name' for the meaning of the arguments."
  (unless dir (setq dir default-directory))
  (unless (file-name-absolute-p dir) (setq dir (expand-file-name dir)))
  (unless default-filename
    (setq default-filename (if initial (expand-file-name initial dir)
                             buffer-file-name)))
  ;; If dir starts with user's homedir, change that to ~.
  (setq dir (abbreviate-file-name dir))
  ;; Likewise for default-filename.
  (if default-filename
      (setq default-filename
	    (if (consp default-filename)
		(mapcar 'abbreviate-file-name default-filename)
	      (abbreviate-file-name default-filename))))
  (let ((insdef (cond
                 ((and insert-default-directory (stringp dir))
                  (if initial
                      (cons (minibuffer--double-dollars (concat dir initial))
                            (length (minibuffer--double-dollars dir)))
                    (minibuffer--double-dollars dir)))
                 (initial (cons (minibuffer--double-dollars initial) 0)))))

    (let ((completion-ignore-case read-file-name-completion-ignore-case)
          (minibuffer-completing-file-name t)
          (pred (or predicate 'file-exists-p))
          (add-to-history nil))

      (let* ((val
              (if (or (not (next-read-file-uses-dialog-p))
                      ;; Graphical file dialogs can't handle remote
                      ;; files (Bug#99).
                      (file-remote-p dir))
                  ;; We used to pass `dir' to `read-file-name-internal' by
                  ;; abusing the `predicate' argument.  It's better to
                  ;; just use `default-directory', but in order to avoid
                  ;; changing `default-directory' in the current buffer,
                  ;; we don't let-bind it.
                  (let ((dir (file-name-as-directory
                              (expand-file-name dir))))
                    (minibuffer-with-setup-hook
                        (lambda ()
                          (setq default-directory dir)
                          ;; When the first default in `minibuffer-default'
                          ;; duplicates initial input `insdef',
                          ;; reset `minibuffer-default' to nil.
                          (when (equal (or (car-safe insdef) insdef)
                                       (or (car-safe minibuffer-default)
                                           minibuffer-default))
                            (setq minibuffer-default
                                  (cdr-safe minibuffer-default)))
                          ;; On the first request on `M-n' fill
                          ;; `minibuffer-default' with a list of defaults
                          ;; relevant for file-name reading.
                          (set (make-local-variable 'minibuffer-default-add-function)
                               (lambda ()
                                 (with-current-buffer
                                     (window-buffer (minibuffer-selected-window))
				   (read-file-name--defaults dir initial)))))
                      (completing-read prompt 'read-file-name-internal
                                       pred mustmatch insdef
                                       'file-name-history default-filename)))
                ;; If DEFAULT-FILENAME not supplied and DIR contains
                ;; a file name, split it.
                (let ((file (file-name-nondirectory dir))
                      ;; When using a dialog, revert to nil and non-nil
                      ;; interpretation of mustmatch. confirm options
                      ;; need to be interpreted as nil, otherwise
                      ;; it is impossible to create new files using
                      ;; dialogs with the default settings.
                      (dialog-mustmatch
                       (not (memq mustmatch
                                  '(nil confirm confirm-after-completion)))))
                  (when (and (not default-filename)
                             (not (zerop (length file))))
                    (setq default-filename file)
                    (setq dir (file-name-directory dir)))
                  (when default-filename
                    (setq default-filename
                          (expand-file-name (if (consp default-filename)
                                                (car default-filename)
                                              default-filename)
                                            dir)))
                  (setq add-to-history t)
                  (x-file-dialog prompt dir default-filename
                                 dialog-mustmatch
                                 (eq predicate 'file-directory-p)))))

             (replace-in-history (eq (car-safe file-name-history) val)))
        ;; If completing-read returned the inserted default string itself
        ;; (rather than a new string with the same contents),
        ;; it has to mean that the user typed RET with the minibuffer empty.
        ;; In that case, we really want to return ""
        ;; so that commands such as set-visited-file-name can distinguish.
        (when (consp default-filename)
          (setq default-filename (car default-filename)))
        (when (eq val default-filename)
          ;; In this case, completing-read has not added an element
          ;; to the history.  Maybe we should.
          (if (not replace-in-history)
              (setq add-to-history t))
          (setq val ""))
        (unless val (error "No file name specified"))

        (if (and default-filename
                 (string-equal val (if (consp insdef) (car insdef) insdef)))
            (setq val default-filename))
        (setq val (substitute-in-file-name val))

        (if replace-in-history
            ;; Replace what Fcompleting_read added to the history
            ;; with what we will actually return.  As an exception,
            ;; if that's the same as the second item in
            ;; file-name-history, it's really a repeat (Bug#4657).
            (let ((val1 (minibuffer--double-dollars val)))
              (if history-delete-duplicates
                  (setcdr file-name-history
                          (delete val1 (cdr file-name-history))))
              (if (string= val1 (cadr file-name-history))
                  (pop file-name-history)
                (setcar file-name-history val1)))
          (if add-to-history
              ;; Add the value to the history--but not if it matches
              ;; the last value already there.
              (let ((val1 (minibuffer--double-dollars val)))
                (unless (and (consp file-name-history)
                             (equal (car file-name-history) val1))
                  (setq file-name-history
                        (cons val1
                              (if history-delete-duplicates
                                  (delete val1 file-name-history)
                                file-name-history)))))))
	val))))

(defun internal-complete-buffer-except (&optional buffer)
  "Perform completion on all buffers excluding BUFFER.
BUFFER nil or omitted means use the current buffer.
Like `internal-complete-buffer', but removes BUFFER from the completion list."
  (let ((except (if (stringp buffer) buffer (buffer-name buffer))))
    (apply-partially 'completion-table-with-predicate
		     'internal-complete-buffer
		     (lambda (name)
		       (not (equal (if (consp name) (car name) name) except)))
		     nil)))

;;; Old-style completion, used in Emacs-21 and Emacs-22.

(defun completion-emacs21-try-completion (string table pred _point)
  (let ((completion (try-completion string table pred)))
    (if (stringp completion)
        (cons completion (length completion))
      completion)))

(defun completion-emacs21-all-completions (string table pred _point)
  (completion-hilit-commonality
   (all-completions string table pred)
   (length string)
   (car (completion-boundaries string table pred ""))))

(defun completion-emacs22-try-completion (string table pred point)
  (let ((suffix (substring string point))
        (completion (try-completion (substring string 0 point) table pred)))
    (if (not (stringp completion))
        completion
      ;; Merge a trailing / in completion with a / after point.
      ;; We used to only do it for word completion, but it seems to make
      ;; sense for all completions.
      ;; Actually, claiming this feature was part of Emacs-22 completion
      ;; is pushing it a bit: it was only done in minibuffer-completion-word,
      ;; which was (by default) not bound during file completion, where such
      ;; slashes are most likely to occur.
      (if (and (not (zerop (length completion)))
               (eq ?/ (aref completion (1- (length completion))))
               (not (zerop (length suffix)))
               (eq ?/ (aref suffix 0)))
          ;; This leaves point after the / .
          (setq suffix (substring suffix 1)))
      (cons (concat completion suffix) (length completion)))))

(defun completion-emacs22-all-completions (string table pred point)
  (let ((beforepoint (substring string 0 point)))
    (completion-hilit-commonality
     (all-completions beforepoint table pred)
     point
     (car (completion-boundaries beforepoint table pred "")))))

;;; Basic completion.

(defun completion--merge-suffix (completion point suffix)
  "Merge end of COMPLETION with beginning of SUFFIX.
Simple generalization of the \"merge trailing /\" done in Emacs-22.
Return the new suffix."
  (if (and (not (zerop (length suffix)))
           (string-match "\\(.+\\)\n\\1" (concat completion "\n" suffix)
                         ;; Make sure we don't compress things to less
                         ;; than we started with.
                         point)
           ;; Just make sure we didn't match some other \n.
           (eq (match-end 1) (length completion)))
      (substring suffix (- (match-end 1) (match-beginning 1)))
    ;; Nothing to merge.
    suffix))

(defun completion-basic--pattern (beforepoint afterpoint bounds)
  (delete
   "" (list (substring beforepoint (car bounds))
            'point
            (substring afterpoint 0 (cdr bounds)))))

(defun completion-basic-try-completion (string table pred point)
  (let* ((beforepoint (substring string 0 point))
         (afterpoint (substring string point))
         (bounds (completion-boundaries beforepoint table pred afterpoint)))
    (if (zerop (cdr bounds))
        ;; `try-completion' may return a subtly different result
        ;; than `all+merge', so try to use it whenever possible.
        (let ((completion (try-completion beforepoint table pred)))
          (if (not (stringp completion))
              completion
            (cons
             (concat completion
                     (completion--merge-suffix completion point afterpoint))
             (length completion))))
      (let* ((suffix (substring afterpoint (cdr bounds)))
             (prefix (substring beforepoint 0 (car bounds)))
             (pattern (delete
                       "" (list (substring beforepoint (car bounds))
                                'point
                                (substring afterpoint 0 (cdr bounds)))))
             (all (completion-pcm--all-completions prefix pattern table pred)))
        (if minibuffer-completing-file-name
            (setq all (completion-pcm--filename-try-filter all)))
        (completion-pcm--merge-try pattern all prefix suffix)))))

(defun completion-basic-all-completions (string table pred point)
  (let* ((beforepoint (substring string 0 point))
         (afterpoint (substring string point))
         (bounds (completion-boundaries beforepoint table pred afterpoint))
         ;; (suffix (substring afterpoint (cdr bounds)))
         (prefix (substring beforepoint 0 (car bounds)))
         (pattern (delete
                   "" (list (substring beforepoint (car bounds))
                            'point
                            (substring afterpoint 0 (cdr bounds)))))
         (all (completion-pcm--all-completions prefix pattern table pred)))
    (completion-hilit-commonality all point (car bounds))))

;;; Partial-completion-mode style completion.

(defvar completion-pcm--delim-wild-regex nil
  "Regular expression matching delimiters controlling the partial-completion.
Typically, this regular expression simply matches a delimiter, meaning
that completion can add something at (match-beginning 0), but if it has
a submatch 1, then completion can add something at (match-end 1).
This is used when the delimiter needs to be of size zero (e.g. the transition
from lowercase to uppercase characters).")

(defun completion-pcm--prepare-delim-re (delims)
  (setq completion-pcm--delim-wild-regex (concat "[" delims "*]")))

(defcustom completion-pcm-word-delimiters "-_./:| "
  "A string of characters treated as word delimiters for completion.
Some arcane rules:
If `]' is in this string, it must come first.
If `^' is in this string, it must not come first.
If `-' is in this string, it must come first or right after `]'.
In other words, if S is this string, then `[S]' must be a valid Emacs regular
expression (not containing character ranges like `a-z')."
  :set (lambda (symbol value)
         (set-default symbol value)
         ;; Refresh other vars.
         (completion-pcm--prepare-delim-re value))
  :initialize 'custom-initialize-reset
  :group 'minibuffer
  :type 'string)

(defcustom completion-pcm-complete-word-inserts-delimiters nil
  "Treat the SPC or - inserted by `minibuffer-complete-word' as delimiters.
Those chars are treated as delimiters iff this variable is non-nil.
I.e. if non-nil, M-x SPC will just insert a \"-\" in the minibuffer, whereas
if nil, it will list all possible commands in *Completions* because none of
the commands start with a \"-\" or a SPC."
  :version "24.1"
  :type 'boolean)

(defun completion-pcm--pattern-trivial-p (pattern)
  (and (stringp (car pattern))
       ;; It can be followed by `point' and "" and still be trivial.
       (let ((trivial t))
	 (dolist (elem (cdr pattern))
	   (unless (member elem '(point ""))
	     (setq trivial nil)))
	 trivial)))

(defun completion-pcm--string->pattern (string &optional point)
  "Split STRING into a pattern.
A pattern is a list where each element is either a string
or a symbol, see `completion-pcm--merge-completions'."
  (if (and point (< point (length string)))
      (let ((prefix (substring string 0 point))
            (suffix (substring string point)))
        (append (completion-pcm--string->pattern prefix)
                '(point)
                (completion-pcm--string->pattern suffix)))
    (let* ((pattern nil)
           (p 0)
           (p0 p))

      (while (and (setq p (string-match completion-pcm--delim-wild-regex
                                        string p))
                  (or completion-pcm-complete-word-inserts-delimiters
                      ;; If the char was added by minibuffer-complete-word,
                      ;; then don't treat it as a delimiter, otherwise
                      ;; "M-x SPC" ends up inserting a "-" rather than listing
                      ;; all completions.
                      (not (get-text-property p 'completion-try-word string))))
        ;; Usually, completion-pcm--delim-wild-regex matches a delimiter,
        ;; meaning that something can be added *before* it, but it can also
        ;; match a prefix and postfix, in which case something can be added
        ;; in-between (e.g. match [[:lower:]][[:upper:]]).
        ;; This is determined by the presence of a submatch-1 which delimits
        ;; the prefix.
        (if (match-end 1) (setq p (match-end 1)))
        (push (substring string p0 p) pattern)
        (if (eq (aref string p) ?*)
            (progn
              (push 'star pattern)
              (setq p0 (1+ p)))
          (push 'any pattern)
          (setq p0 p))
        (incf p))

      ;; An empty string might be erroneously added at the beginning.
      ;; It should be avoided properly, but it's so easy to remove it here.
      (delete "" (nreverse (cons (substring string p0) pattern))))))

(defun completion-pcm--pattern->regex (pattern &optional group)
  (let ((re
         (concat "\\`"
                 (mapconcat
                  (lambda (x)
                    (cond
                     ((stringp x) (regexp-quote x))
                     ((if (consp group) (memq x group) group) "\\(.*?\\)")
		     (t ".*?")))
                  pattern
                  ""))))
    ;; Avoid pathological backtracking.
    (while (string-match "\\.\\*\\?\\(?:\\\\[()]\\)*\\(\\.\\*\\?\\)" re)
      (setq re (replace-match "" t t re 1)))
    re))

(defun completion-pcm--all-completions (prefix pattern table pred)
  "Find all completions for PATTERN in TABLE obeying PRED.
PATTERN is as returned by `completion-pcm--string->pattern'."
  ;; (assert (= (car (completion-boundaries prefix table pred ""))
  ;;            (length prefix)))
  ;; Find an initial list of possible completions.
  (if (completion-pcm--pattern-trivial-p pattern)

      ;; Minibuffer contains no delimiters -- simple case!
      (all-completions (concat prefix (car pattern)) table pred)

    ;; Use all-completions to do an initial cull.  This is a big win,
    ;; since all-completions is written in C!
    (let* (;; Convert search pattern to a standard regular expression.
	   (regex (completion-pcm--pattern->regex pattern))
           (case-fold-search completion-ignore-case)
           (completion-regexp-list (cons regex completion-regexp-list))
	   (compl (all-completions
                   (concat prefix
                           (if (stringp (car pattern)) (car pattern) ""))
		   table pred)))
      (if (not (functionp table))
	  ;; The internal functions already obeyed completion-regexp-list.
	  compl
	(let ((poss ()))
	  (dolist (c compl)
	    (when (string-match-p regex c) (push c poss)))
	  poss)))))

(defun completion-pcm--hilit-commonality (pattern completions)
  (when completions
    (let* ((re (completion-pcm--pattern->regex pattern '(point)))
           (case-fold-search completion-ignore-case))
      (mapcar
       (lambda (str)
	 ;; Don't modify the string itself.
         (setq str (copy-sequence str))
         (unless (string-match re str)
           (error "Internal error: %s does not match %s" re str))
         (let ((pos (or (match-beginning 1) (match-end 0))))
           (put-text-property 0 pos
                              'font-lock-face 'completions-common-part
                              str)
           (if (> (length str) pos)
               (put-text-property pos (1+ pos)
				  'font-lock-face 'completions-first-difference
				  str)))
	 str)
       completions))))

(defun completion-pcm--find-all-completions (string table pred point
                                                    &optional filter)
  "Find all completions for STRING at POINT in TABLE, satisfying PRED.
POINT is a position inside STRING.
FILTER is a function applied to the return value, that can be used, e.g. to
filter out additional entries (because TABLE might not obey PRED)."
  (unless filter (setq filter 'identity))
  (let* ((beforepoint (substring string 0 point))
         (afterpoint (substring string point))
         (bounds (completion-boundaries beforepoint table pred afterpoint))
         (prefix (substring beforepoint 0 (car bounds)))
         (suffix (substring afterpoint (cdr bounds)))
         firsterror)
    (setq string (substring string (car bounds) (+ point (cdr bounds))))
    (let* ((relpoint (- point (car bounds)))
           (pattern (completion-pcm--string->pattern string relpoint))
           (all (condition-case err
                    (funcall filter
                             (completion-pcm--all-completions
                              prefix pattern table pred))
                  (error (unless firsterror (setq firsterror err)) nil))))
      (when (and (null all)
                 (> (car bounds) 0)
                 (null (ignore-errors (try-completion prefix table pred))))
        ;; The prefix has no completions at all, so we should try and fix
        ;; that first.
        (let ((substring (substring prefix 0 -1)))
          (destructuring-bind (subpat suball subprefix _subsuffix)
              (completion-pcm--find-all-completions
               substring table pred (length substring) filter)
            (let ((sep (aref prefix (1- (length prefix))))
                  ;; Text that goes between the new submatches and the
                  ;; completion substring.
                  (between nil))
              ;; Eliminate submatches that don't end with the separator.
              (dolist (submatch (prog1 suball (setq suball ())))
                (when (eq sep (aref submatch (1- (length submatch))))
                  (push submatch suball)))
              (when suball
                ;; Update the boundaries and corresponding pattern.
                ;; We assume that all submatches result in the same boundaries
                ;; since we wouldn't know how to merge them otherwise anyway.
                ;; FIXME: COMPLETE REWRITE!!!
                (let* ((newbeforepoint
                        (concat subprefix (car suball)
                                (substring string 0 relpoint)))
                       (leftbound (+ (length subprefix) (length (car suball))))
                       (newbounds (completion-boundaries
                                   newbeforepoint table pred afterpoint)))
                  (unless (or (and (eq (cdr bounds) (cdr newbounds))
                                   (eq (car newbounds) leftbound))
                              ;; Refuse new boundaries if they step over
                              ;; the submatch.
                              (< (car newbounds) leftbound))
                    ;; The new completed prefix does change the boundaries
                    ;; of the completed substring.
                    (setq suffix (substring afterpoint (cdr newbounds)))
                    (setq string
                          (concat (substring newbeforepoint (car newbounds))
                                  (substring afterpoint 0 (cdr newbounds))))
                    (setq between (substring newbeforepoint leftbound
                                             (car newbounds)))
                    (setq pattern (completion-pcm--string->pattern
                                   string
                                   (- (length newbeforepoint)
                                      (car newbounds)))))
                  (dolist (submatch suball)
                    (setq all (nconc
                               (mapcar
                                (lambda (s) (concat submatch between s))
                                (funcall filter
                                         (completion-pcm--all-completions
                                          (concat subprefix submatch between)
                                          pattern table pred)))
                               all)))
                  ;; FIXME: This can come in handy for try-completion,
                  ;; but isn't right for all-completions, since it lists
                  ;; invalid completions.
                  ;; (unless all
                  ;;   ;; Even though we found expansions in the prefix, none
                  ;;   ;; leads to a valid completion.
                  ;;   ;; Let's keep the expansions, tho.
                  ;;   (dolist (submatch suball)
                  ;;     (push (concat submatch between newsubstring) all)))
                  ))
              (setq pattern (append subpat (list 'any (string sep))
                                    (if between (list between)) pattern))
              (setq prefix subprefix)))))
      (if (and (null all) firsterror)
          (signal (car firsterror) (cdr firsterror))
        (list pattern all prefix suffix)))))

(defun completion-pcm-all-completions (string table pred point)
  (destructuring-bind (pattern all &optional prefix _suffix)
      (completion-pcm--find-all-completions string table pred point)
    (when all
      (nconc (completion-pcm--hilit-commonality pattern all)
             (length prefix)))))

(defun completion--sreverse (str)
  "Like `reverse' but for a string STR rather than a list."
  (apply 'string (nreverse (mapcar 'identity str))))

(defun completion--common-suffix (strs)
  "Return the common suffix of the strings STRS."
  (completion--sreverse
   (try-completion
    ""
    (mapcar 'completion--sreverse strs))))

(defun completion-pcm--merge-completions (strs pattern)
  "Extract the commonality in STRS, with the help of PATTERN.
PATTERN can contain strings and symbols chosen among `star', `any', `point',
and `prefix'.  They all match anything (aka \".*\") but are merged differently:
`any' only grows from the left (when matching \"a1b\" and \"a2b\" it gets
  completed to just \"a\").
`prefix' only grows from the right (when matching \"a1b\" and \"a2b\" it gets
  completed to just \"b\").
`star' grows from both ends and is reified into a \"*\"  (when matching \"a1b\"
  and \"a2b\" it gets completed to \"a*b\").
`point' is like `star' except that it gets reified as the position of point
  instead of being reified as a \"*\" character.
The underlying idea is that we should return a string which still matches
the same set of elements."
  ;; When completing while ignoring case, we want to try and avoid
  ;; completing "fo" to "foO" when completing against "FOO" (bug#4219).
  ;; So we try and make sure that the string we return is all made up
  ;; of text from the completions rather than part from the
  ;; completions and part from the input.
  ;; FIXME: This reduces the problems of inconsistent capitalization
  ;; but it doesn't fully fix it: we may still end up completing
  ;; "fo-ba" to "foo-BAR" or "FOO-bar" when completing against
  ;; '("foo-barr" "FOO-BARD").
  (cond
   ((null (cdr strs)) (list (car strs)))
   (t
    (let ((re (completion-pcm--pattern->regex pattern 'group))
          (ccs ()))                     ;Chopped completions.

      ;; First chop each string into the parts corresponding to each
      ;; non-constant element of `pattern', using regexp-matching.
      (let ((case-fold-search completion-ignore-case))
        (dolist (str strs)
          (unless (string-match re str)
            (error "Internal error: %s doesn't match %s" str re))
          (let ((chopped ())
                (last 0)
                (i 1)
                next)
            (while (setq next (match-end i))
              (push (substring str last next) chopped)
              (setq last next)
              (setq i (1+ i)))
            ;; Add the text corresponding to the implicit trailing `any'.
            (push (substring str last) chopped)
            (push (nreverse chopped) ccs))))

      ;; Then for each of those non-constant elements, extract the
      ;; commonality between them.
      (let ((res ())
            (fixed ""))
        ;; Make the implicit trailing `any' explicit.
        (dolist (elem (append pattern '(any)))
          (if (stringp elem)
              (setq fixed (concat fixed elem))
            (let ((comps ()))
              (dolist (cc (prog1 ccs (setq ccs nil)))
                (push (car cc) comps)
                (push (cdr cc) ccs))
              ;; Might improve the likelihood to avoid choosing
              ;; different capitalizations in different parts.
              ;; In practice, it doesn't seem to make any difference.
              (setq ccs (nreverse ccs))
              (let* ((prefix (try-completion fixed comps))
                     (unique (or (and (eq prefix t) (setq prefix fixed))
                                 (eq t (try-completion prefix comps)))))
                (unless (or (eq elem 'prefix)
                            (equal prefix ""))
                  (push prefix res))
                ;; If there's only one completion, `elem' is not useful
                ;; any more: it can only match the empty string.
                ;; FIXME: in some cases, it may be necessary to turn an
                ;; `any' into a `star' because the surrounding context has
                ;; changed such that string->pattern wouldn't add an `any'
                ;; here any more.
                (unless unique
                  (push elem res)
                  (when (memq elem '(star point prefix))
                    ;; Extract common suffix additionally to common prefix.
                    ;; Only do it for `point', `star', and `prefix' since for
                    ;; `any' it could lead to a merged completion that
                    ;; doesn't itself match the candidates.
                    (let ((suffix (completion--common-suffix comps)))
                      (assert (stringp suffix))
                      (unless (equal suffix "")
                        (push suffix res)))))
                (setq fixed "")))))
        ;; We return it in reverse order.
        res)))))

(defun completion-pcm--pattern->string (pattern)
  (mapconcat (lambda (x) (cond
                          ((stringp x) x)
                          ((eq x 'star) "*")
                          (t "")))           ;any, point, prefix.
             pattern
             ""))

;; We want to provide the functionality of `try', but we use `all'
;; and then merge it.  In most cases, this works perfectly, but
;; if the completion table doesn't consider the same completions in
;; `try' as in `all', then we have a problem.  The most common such
;; case is for filename completion where completion-ignored-extensions
;; is only obeyed by the `try' code.  We paper over the difference
;; here.  Note that it is not quite right either: if the completion
;; table uses completion-table-in-turn, this filtering may take place
;; too late to correctly fallback from the first to the
;; second alternative.
(defun completion-pcm--filename-try-filter (all)
  "Filter to adjust `all' file completion to the behavior of `try'."
  (when all
    (let ((try ())
          (re (concat "\\(?:\\`\\.\\.?/\\|"
                      (regexp-opt completion-ignored-extensions)
                      "\\)\\'")))
      (dolist (f all)
        (unless (string-match-p re f) (push f try)))
      (or try all))))


(defun completion-pcm--merge-try (pattern all prefix suffix)
  (cond
   ((not (consp all)) all)
   ((and (not (consp (cdr all)))        ;Only one completion.
         ;; Ignore completion-ignore-case here.
         (equal (completion-pcm--pattern->string pattern) (car all)))
    t)
   (t
    (let* ((mergedpat (completion-pcm--merge-completions all pattern))
           ;; `mergedpat' is in reverse order.  Place new point (by
           ;; order of preference) either at the old point, or at
           ;; the last place where there's something to choose, or
           ;; at the very end.
           (pointpat (or (memq 'point mergedpat)
                         (memq 'any   mergedpat)
                         (memq 'star  mergedpat)
                         ;; Not `prefix'.
                         mergedpat))
           ;; New pos from the start.
           (newpos (length (completion-pcm--pattern->string pointpat)))
           ;; Do it afterwards because it changes `pointpat' by side effect.
           (merged (completion-pcm--pattern->string (nreverse mergedpat))))

      (setq suffix (completion--merge-suffix merged newpos suffix))
      (cons (concat prefix merged suffix) (+ newpos (length prefix)))))))

(defun completion-pcm-try-completion (string table pred point)
  (destructuring-bind (pattern all prefix suffix)
      (completion-pcm--find-all-completions
       string table pred point
       (if minibuffer-completing-file-name
           'completion-pcm--filename-try-filter))
    (completion-pcm--merge-try pattern all prefix suffix)))

;;; Substring completion
;; Mostly derived from the code of `basic' completion.

(defun completion-substring--all-completions (string table pred point)
  (let* ((beforepoint (substring string 0 point))
         (afterpoint (substring string point))
         (bounds (completion-boundaries beforepoint table pred afterpoint))
         (suffix (substring afterpoint (cdr bounds)))
         (prefix (substring beforepoint 0 (car bounds)))
         (basic-pattern (completion-basic--pattern
                         beforepoint afterpoint bounds))
         (pattern (if (not (stringp (car basic-pattern)))
                      basic-pattern
                    (cons 'prefix basic-pattern)))
         (all (completion-pcm--all-completions prefix pattern table pred)))
    (list all pattern prefix suffix (car bounds))))

(defun completion-substring-try-completion (string table pred point)
  (destructuring-bind (all pattern prefix suffix _carbounds)
      (completion-substring--all-completions string table pred point)
    (if minibuffer-completing-file-name
        (setq all (completion-pcm--filename-try-filter all)))
    (completion-pcm--merge-try pattern all prefix suffix)))

(defun completion-substring-all-completions (string table pred point)
  (destructuring-bind (all pattern prefix _suffix _carbounds)
      (completion-substring--all-completions string table pred point)
    (when all
      (nconc (completion-pcm--hilit-commonality pattern all)
             (length prefix)))))

;; Initials completion
;; Complete /ums to /usr/monnier/src or lch to list-command-history.

(defun completion-initials-expand (str table pred)
  (let ((bounds (completion-boundaries str table pred "")))
    (unless (or (zerop (length str))
                ;; Only check within the boundaries, since the
                ;; boundary char (e.g. /) might be in delim-regexp.
                (string-match completion-pcm--delim-wild-regex str
                              (car bounds)))
      (if (zerop (car bounds))
          (mapconcat 'string str "-")
        ;; If there's a boundary, it's trickier.  The main use-case
        ;; we consider here is file-name completion.  We'd like
        ;; to expand ~/eee to ~/e/e/e and /eee to /e/e/e.
        ;; But at the same time, we don't want /usr/share/ae to expand
        ;; to /usr/share/a/e just because we mistyped "ae" for "ar",
        ;; so we probably don't want initials to touch anything that
        ;; looks like /usr/share/foo.  As a heuristic, we just check that
        ;; the text before the boundary char is at most 1 char.
        ;; This allows both ~/eee and /eee and not much more.
        ;; FIXME: It sadly also disallows the use of ~/eee when that's
        ;; embedded within something else (e.g. "(~/eee" in Info node
        ;; completion or "ancestor:/eee" in bzr-revision completion).
        (when (< (car bounds) 3)
          (let ((sep (substring str (1- (car bounds)) (car bounds))))
            ;; FIXME: the above string-match checks the whole string, whereas
            ;; we end up only caring about the after-boundary part.
            (concat (substring str 0 (car bounds))
                    (mapconcat 'string (substring str (car bounds)) sep))))))))

(defun completion-initials-all-completions (string table pred _point)
  (let ((newstr (completion-initials-expand string table pred)))
    (when newstr
      (completion-pcm-all-completions newstr table pred (length newstr)))))

(defun completion-initials-try-completion (string table pred _point)
  (let ((newstr (completion-initials-expand string table pred)))
    (when newstr
      (completion-pcm-try-completion newstr table pred (length newstr)))))

(defvar completing-read-function 'completing-read-default
  "The function called by `completing-read' to do its work.
It should accept the same arguments as `completing-read'.")

(defun completing-read-default (prompt collection &optional predicate
                                       require-match initial-input
                                       hist def inherit-input-method)
  "Default method for reading from the minibuffer with completion.
See `completing-read' for the meaning of the arguments."

  (when (consp initial-input)
    (setq initial-input
          (cons (car initial-input)
                ;; `completing-read' uses 0-based index while
                ;; `read-from-minibuffer' uses 1-based index.
                (1+ (cdr initial-input)))))

  (let* ((minibuffer-completion-table collection)
         (minibuffer-completion-predicate predicate)
         (minibuffer-completion-confirm (unless (eq require-match t)
                                          require-match))
         (base-keymap (if require-match
                         minibuffer-local-must-match-map
                        minibuffer-local-completion-map))
         (keymap (if (memq minibuffer-completing-file-name '(nil lambda))
                     base-keymap
                   ;; Layer minibuffer-local-filename-completion-map
                   ;; on top of the base map.
                   (make-composed-keymap
                    minibuffer-local-filename-completion-map
                    ;; Set base-keymap as the parent, so that nil bindings
                    ;; in minibuffer-local-filename-completion-map can
                    ;; override bindings in base-keymap.
                    base-keymap)))
         (result (read-from-minibuffer prompt initial-input keymap
                                       nil hist def inherit-input-method)))
    (when (and (equal result "") def)
      (setq result (if (consp def) (car def) def)))
    result))

;; Miscellaneous

(defun minibuffer-insert-file-name-at-point ()
  "Get a file name at point in original buffer and insert it to minibuffer."
  (interactive)
  (let ((file-name-at-point
	 (with-current-buffer (window-buffer (minibuffer-selected-window))
	   (run-hook-with-args-until-success 'file-name-at-point-functions))))
    (when file-name-at-point
      (insert file-name-at-point))))

(provide 'minibuffer)

;;; minibuffer.el ends here
