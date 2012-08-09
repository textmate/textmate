;;; semantic.el --- Semantic buffer evaluator.

;; Copyright (C) 1999-2012 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax tools
;; Version: 2.0

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
;; API for providing the semantic content of a buffer.
;;
;; The Semantic API provides an interface to a series of different parser
;; implementations.  Each parser outputs a parse tree in a similar format
;; designed to handle typical functional and object oriented languages.
;;
;; To enable Semantic, turn on `semantic-mode', a global minor mode
;; (M-x semantic-mode RET, or "Source Code Parsers" from the Tools
;; menu).  To enable it at startup, put (semantic-mode 1) in your init
;; file.

(require 'cedet)
(require 'semantic/tag)
(require 'semantic/lex)

(defvar semantic-version "2.0"
  "Current version of Semantic.")

(declare-function inversion-test "inversion")
(declare-function semanticdb-load-ebrowse-caches "semantic/db-ebrowse")

(defun semantic-require-version (major minor &optional beta)
  "Non-nil if this version of Semantic does not satisfy a specific version.
Arguments can be:

  (MAJOR MINOR &optional BETA)

  Values MAJOR and MINOR must be integers.  BETA can be an integer, or
excluded if a released version is required.

It is assumed that if the current version is newer than that specified,
everything passes.  Exceptions occur when known incompatibilities are
introduced."
  (require 'inversion)
  (inversion-test 'semantic
		  (concat major "." minor
			  (when beta (concat "beta" beta)))))

(defgroup semantic nil
  "Parser Generator and parser framework."
  :group 'tools)

(defgroup semantic-faces nil
  "Faces used for Semantic enabled tools."
  :group 'semantic)

(require 'semantic/fw)

;;; Code:
;;

;;; Variables and Configuration
;;
(defvar semantic--parse-table nil
  "Variable that defines how to parse top level items in a buffer.
This variable is for internal use only, and its content depends on the
external parser used.")
(make-variable-buffer-local 'semantic--parse-table)
(semantic-varalias-obsolete 'semantic-toplevel-bovine-table
			    'semantic--parse-table "23.2")

(defvar semantic-symbol->name-assoc-list
  '((type     . "Types")
    (variable . "Variables")
    (function . "Functions")
    (include  . "Dependencies")
    (package  . "Provides"))
  "Association between symbols returned, and a string.
The string is used to represent a group of objects of the given type.
It is sometimes useful for a language to use a different string
in place of the default, even though that language will still
return a symbol.  For example, Java return's includes, but the
string can be replaced with `Imports'.")
(make-variable-buffer-local 'semantic-symbol->name-assoc-list)

(defvar semantic-symbol->name-assoc-list-for-type-parts nil
  "Like `semantic-symbol->name-assoc-list' for type parts.
Some tags that have children (see `semantic-tag-children-compatibility')
will want to define the names of classes of tags differently than at
the top level.  For example, in C++, a Function may be called a
Method.  In addition, there may be new types of tags that exist only
in classes, such as protection labels.")
(make-variable-buffer-local 'semantic-symbol->name-assoc-list-for-type-parts)

(defvar semantic-case-fold nil
  "Value for `case-fold-search' when parsing.")
(make-variable-buffer-local 'semantic-case-fold)

(defvar semantic-expand-nonterminal nil
  "Function to call for each nonterminal production.
Return a list of non-terminals derived from the first argument, or nil
if it does not need to be expanded.
Languages with compound definitions should use this function to expand
from one compound symbol into several.  For example, in C the definition
  int a, b;
is easily parsed into one tag.  This function should take this
compound tag and turn it into two tags, one for A, and the other for B.")
(make-variable-buffer-local 'semantic-expand-nonterminal)

(defvar semantic--buffer-cache nil
  "A cache of the fully parsed buffer.
If no significant changes have been made (based on the state) then
this is returned instead of re-parsing the buffer.

  DO NOT USE THIS VARIABLE IN PROGRAMS.

If you need a tag list, use `semantic-fetch-tags'.  If you need the
cached values for some reason, chances are you can add a hook to
`semantic-after-toplevel-cache-change-hook'.")
(make-variable-buffer-local 'semantic--buffer-cache)
(semantic-varalias-obsolete 'semantic-toplevel-bovine-cache
			    'semantic--buffer-cache "23.2")

(defvar semantic-unmatched-syntax-cache nil
  "A cached copy of unmatched syntax tokens.")
(make-variable-buffer-local 'semantic-unmatched-syntax-cache)

(defvar semantic-unmatched-syntax-cache-check nil
  "Non-nil if the unmatched syntax cache is out of date.
This is tracked with `semantic-change-function'.")
(make-variable-buffer-local 'semantic-unmatched-syntax-cache-check)

(defvar semantic-edits-are-safe nil
  "When non-nil, modifications do not require a reparse.
This prevents tags from being marked dirty, and it prevents top level
edits from causing a cache check.
Use this when writing programs that could cause a full reparse, but
will not change the tag structure, such as adding or updating
`top-level' comments.")

(defvar semantic-unmatched-syntax-hook nil
  "Hooks run when Semantic detects syntax not matched in a grammar.
Each individual piece of syntax (such as a symbol or punctuation
character) is called with this hook when it doesn't match in the
grammar, and multiple unmatched syntax elements are not grouped
together.  Each hook is called with one argument, which is a list
of syntax tokens created by the semantic lexer.  Use the functions
`semantic-lex-token-start', `semantic-lex-token-end' and
`semantic-lex-token-text' to get information about these tokens.
The current buffer is the buffer these tokens are derived from.")

(defvar semantic--before-fetch-tags-hook nil
  "Hooks run before a buffer is parsed for tags.
It is called before any request for tags is made via the function
`semantic-fetch-tags' by an application.
If any hook returns a nil value, the cached value is returned
immediately, even if it is empty.")
(semantic-varalias-obsolete 'semantic-before-toplevel-bovination-hook
			    'semantic--before-fetch-tags-hook "23.2")

(defvar semantic-after-toplevel-bovinate-hook nil
  "Hooks run after a toplevel parse.
It is not run if the toplevel parse command is called, and buffer does
not need to be fully reparsed.
For language specific hooks, make sure you define this as a local hook.

This hook should not be used any more.
Use `semantic-after-toplevel-cache-change-hook' instead.")
(make-obsolete-variable 'semantic-after-toplevel-bovinate-hook nil "23.2")

(defvar semantic-after-toplevel-cache-change-hook nil
  "Hooks run after the buffer tag list has changed.
This list will change when a buffer is reparsed, or when the tag list
in a buffer is cleared.  It is *NOT* called if the current tag list is
partially reparsed.

Hook functions must take one argument, which is the new list of tags
associated with this buffer.

For language specific hooks, make sure you define this as a local hook.")

(defvar semantic-before-toplevel-cache-flush-hook nil
  "Hooks run before the toplevel tag cache is flushed.
For language specific hooks, make sure you define this as a local
hook.  This hook is called before a corresponding
`semantic-after-toplevel-cache-change-hook' which is also called
during a flush when the cache is given a new value of nil.")

(defcustom semantic-dump-parse nil
  "When non-nil, dump parsing information."
  :group 'semantic
  :type 'boolean)

(defvar semantic-parser-name "LL"
  "Optional name of the parser used to parse input stream.")
(make-variable-buffer-local 'semantic-parser-name)

(defvar semantic--completion-cache nil
  "Internal variable used by `semantic-complete-symbol'.")
(make-variable-buffer-local 'semantic--completion-cache)

;;; Parse tree state management API
;;
(defvar semantic-parse-tree-state 'needs-rebuild
  "State of the current parse tree.")
(make-variable-buffer-local 'semantic-parse-tree-state)

(defmacro semantic-parse-tree-unparseable ()
  "Indicate that the current buffer is unparseable.
It is also true that the parse tree will need either updating or
a rebuild.  This state will be changed when the user edits the buffer."
  `(setq semantic-parse-tree-state 'unparseable))

(defmacro semantic-parse-tree-unparseable-p ()
  "Return non-nil if the current buffer has been marked unparseable."
  `(eq semantic-parse-tree-state 'unparseable))

(defmacro semantic-parse-tree-set-needs-update ()
  "Indicate that the current parse tree needs to be updated.
The parse tree can be updated by `semantic-parse-changes'."
  `(setq semantic-parse-tree-state 'needs-update))

(defmacro semantic-parse-tree-needs-update-p ()
  "Return non-nil if the current parse tree needs to be updated."
  `(eq semantic-parse-tree-state 'needs-update))

(defmacro semantic-parse-tree-set-needs-rebuild ()
  "Indicate that the current parse tree needs to be rebuilt.
The parse tree must be rebuilt by `semantic-parse-region'."
  `(setq semantic-parse-tree-state 'needs-rebuild))

(defmacro semantic-parse-tree-needs-rebuild-p ()
  "Return non-nil if the current parse tree needs to be rebuilt."
  `(eq semantic-parse-tree-state 'needs-rebuild))

(defmacro semantic-parse-tree-set-up-to-date ()
  "Indicate that the current parse tree is up to date."
  `(setq semantic-parse-tree-state nil))

(defmacro semantic-parse-tree-up-to-date-p ()
  "Return non-nil if the current parse tree is up to date."
  `(null semantic-parse-tree-state))

;;; Interfacing with the system
;;
(defcustom semantic-inhibit-functions nil
  "List of functions to call with no arguments before Semantic is setup.
If any of these functions returns non-nil, the current buffer is not
setup to use Semantic."
  :group 'semantic
  :type 'hook)

(defcustom semantic-new-buffer-setup-functions
  '((c-mode . semantic-default-c-setup)
    (c++-mode . semantic-default-c-setup)
    (html-mode . semantic-default-html-setup)
    (java-mode . wisent-java-default-setup)
    (js-mode . wisent-javascript-setup-parser)
    (python-mode . wisent-python-default-setup)
    (scheme-mode . semantic-default-scheme-setup)
    (srecode-template-mode . srecode-template-setup-parser)
    (makefile-automake-mode . semantic-default-make-setup)
    (makefile-gmake-mode . semantic-default-make-setup)
    (makefile-makepp-mode . semantic-default-make-setup)
    (makefile-bsdmake-mode . semantic-default-make-setup)
    (makefile-imake-mode . semantic-default-make-setup)
    (makefile-mode . semantic-default-make-setup))
  "Alist of functions to call to set up Semantic parsing in the buffer.
Each element has the form (MODE . FN), where MODE is a value of
`major-mode' for the buffer and FN is the corresponding function
to call, with no arguments, to set up the parser.

These functions are called by `semantic-new-buffer-fcn', before
`semantic-inhibit-functions'."
  :group 'semantic
  :type '(alist :key-type symbol :value-type function))

(defvar semantic-init-hook nil
  "Hook run when a buffer is initialized with a parsing table.")

(defvar semantic-init-mode-hook nil
  "Hook run when a buffer of a particular mode is initialized.")
(make-variable-buffer-local 'semantic-init-mode-hook)

(defvar semantic-init-db-hook nil
  "Hook run when a buffer is initialized with a parsing table for DBs.
This hook is for database functions which intend to swap in a tag table.
This guarantees that the DB will go before other modes that require
a parse of the buffer.")

(semantic-varalias-obsolete 'semantic-init-hooks
			    'semantic-init-hook "23.2")
(semantic-varalias-obsolete 'semantic-init-mode-hooks
			    'semantic-init-mode-hook "23.2")
(semantic-varalias-obsolete 'semantic-init-db-hooks
			    'semantic-init-db-hook "23.2")

(defvar semantic-new-buffer-fcn-was-run nil
  "Non-nil after `semantic-new-buffer-fcn' has been executed.")
(make-variable-buffer-local 'semantic-new-buffer-fcn-was-run)

(defsubst semantic-active-p ()
  "Return non-nil if the current buffer was set up for parsing."
  semantic-new-buffer-fcn-was-run)

(defsubst semantic--umatched-syntax-needs-refresh-p  ()
  "Return non-nil if the unmatched syntax cache needs a refresh.
That is, if it is dirty or if the current parse tree isn't up to date."
  (or semantic-unmatched-syntax-cache-check
      (not (semantic-parse-tree-up-to-date-p))))

(defun semantic-new-buffer-fcn ()
  "Setup the current buffer to use Semantic.
If the major mode is ready for Semantic, and no
`semantic-inhibit-functions' disabled it, the current buffer is setup
to use Semantic, and `semantic-init-hook' is run."
  ;; In upstream Semantic, the parser setup functions are called from
  ;; mode hooks.  In the version bundled with Emacs, we do it here.
  (let ((entry (assq major-mode semantic-new-buffer-setup-functions)))
    (when entry
      (funcall (cdr entry))))
  ;; Do stuff if semantic was activated by a mode hook in this buffer,
  ;; and not afterwards disabled.
  (when (and semantic--parse-table
             (not (semantic-active-p))
             (not (run-hook-with-args-until-success
                   'semantic-inhibit-functions)))
    ;; Make sure that if this buffer is cloned, our tags and overlays
    ;; don't go along for the ride.
    (add-hook 'clone-indirect-buffer-hook 'semantic-clear-toplevel-cache
	      nil t)
    ;; Specify that this function has done its work.  At this point
    ;; we can consider that semantic is active in this buffer.
    (setq semantic-new-buffer-fcn-was-run t)
    ;; Here are some buffer local variables we can initialize ourselves
    ;; of a mode does not choose to do so.
    (semantic-lex-init)
    ;; Force this buffer to have its cache refreshed.
    (semantic-clear-toplevel-cache)
    ;; Call DB hooks before regular init hooks
    (run-hooks 'semantic-init-db-hook)
    ;; Set up semantic modes
    (run-hooks 'semantic-init-hook)
    ;; Set up major-mode specific semantic modes
    (run-hooks 'semantic-init-mode-hook)))

(defun semantic-fetch-tags-fast ()
  "For use in a hook.  When only a partial reparse is needed, reparse."
  (condition-case nil
      (if (semantic-parse-tree-needs-update-p)
	  (semantic-fetch-tags))
    (error nil))
  semantic--buffer-cache)

;;; Parsing Commands
;;
(eval-when-compile
  (condition-case nil (require 'pp) (error nil)))

(defvar semantic-edebug nil
  "When non-nil, activate the interactive parsing debugger.
Do not set this yourself.  Call `semantic-debug'.")

(defun semantic-elapsed-time (start end)
  "Copied from elp.el.  Was `elp-elapsed-time'.
Arguments START and END bound the time being calculated."
  (float-time (time-subtract end start)))

(defun bovinate (&optional clear)
  "Parse the current buffer.  Show output in a temp buffer.
Optional argument CLEAR will clear the cache before parsing.
If CLEAR is negative, it will do a full reparse, and also not display
the output buffer."
  (interactive "P")
  (if clear (semantic-clear-toplevel-cache))
  (if (eq clear '-) (setq clear -1))
  (let* ((start (current-time))
	 (out (semantic-fetch-tags))
	 (end (current-time)))
    (message "Retrieving tags took %.2f seconds."
	     (semantic-elapsed-time start end))
    (when (or (null clear) (not (listp clear)))
      (pop-to-buffer "*Parser Output*")
      (require 'pp)
      (erase-buffer)
      (insert (pp-to-string out))
      (goto-char (point-min)))))

;;; Functions of the parser plug-in API
;;
;; Overload these functions to create new types of parsers.
;;
(define-overloadable-function semantic-parse-stream (stream nonterminal)
  "Parse STREAM, starting at the first NONTERMINAL rule.
For bovine and wisent based parsers, STREAM is from the output of
`semantic-lex', and NONTERMINAL is a rule in the appropriate language
specific rules file.
The default parser table used for bovine or wisent based parsers is
`semantic--parse-table'.

Must return a list: (STREAM TAGS) where STREAM is the unused elements
from STREAM, and TAGS is the list of semantic tags found; usually only
one tag is returned with the exception of compound statements.")

(define-overloadable-function semantic-parse-changes ()
  "Reparse changes in the current buffer.
The list of changes are tracked as a series of overlays in the buffer.
When overloading this function, use `semantic-changes-in-region' to
analyze.")

(define-overloadable-function semantic-parse-region
  (start end &optional nonterminal depth returnonerror)
  "Parse the area between START and END, and return any tags found.
If END needs to be extended due to a lexical token being too large, it
will be silently ignored.

Optional arguments:
NONTERMINAL is the rule to start parsing at.
DEPTH specifies the lexical depth to descend for parsers that use
lexical analysis as their first step.
RETURNONERROR specifies that parsing should stop on the first
unmatched syntax encountered.  When nil, parsing skips the syntax,
adding it to the unmatched syntax cache.

Must return a list of semantic tags which have been cooked
\(repositioned properly) but which DO NOT HAVE OVERLAYS associated
with them.  When overloading this function, use `semantic--tag-expand'
to cook raw tags.")

(defun semantic-parse-region-default
  (start end &optional nonterminal depth returnonerror)
  "Parse the area between START and END, and return any tags found.
If END needs to be extended due to a lexical token being too large,
it will be silently ignored.
Optional arguments:
NONTERMINAL is the rule to start parsing at if it is known.
DEPTH specifies the lexical depth to scan.
RETURNONERROR specifies that parsing should end when encountering
unterminated syntax."
  (when (or (null semantic--parse-table) (eq semantic--parse-table t))
    ;; If there is no table, or it was set to t, then we are here by
    ;; some other mistake.  Do not throw an error deep in the parser.
    (error "No support found to parse buffer %S" (buffer-name)))
  (save-restriction
    (widen)
    (when (or (< end start) (> end (point-max)))
      (error "Invalid parse region bounds %S, %S" start end))
    (nreverse
     (semantic-repeat-parse-whole-stream
      (or (cdr (assq start semantic-lex-block-streams))
	  (semantic-lex start end depth))
      nonterminal returnonerror))))

;;; Parsing functions
;;
(defun semantic-set-unmatched-syntax-cache (unmatched-syntax)
  "Set the unmatched syntax cache.
Argument UNMATCHED-SYNTAX is the syntax to set into the cache."
  ;; This function is not actually called by the main parse loop.
  ;; This is intended for use by semanticdb.
  (setq semantic-unmatched-syntax-cache unmatched-syntax
	semantic-unmatched-syntax-cache-check nil)
    ;; Refresh the display of unmatched syntax tokens if enabled
  (run-hook-with-args 'semantic-unmatched-syntax-hook
                      semantic-unmatched-syntax-cache))

(defun semantic-clear-unmatched-syntax-cache ()
  "Clear the cache of unmatched syntax tokens."
  (setq semantic-unmatched-syntax-cache nil
        semantic-unmatched-syntax-cache-check t))

(defun semantic-unmatched-syntax-tokens ()
  "Return the list of unmatched syntax tokens."
  ;; If the cache need refresh then do a full re-parse.
  (if (semantic--umatched-syntax-needs-refresh-p)
      ;; To avoid a recursive call, temporarily disable
      ;; `semantic-unmatched-syntax-hook'.
      (let (semantic-unmatched-syntax-hook)
        (condition-case nil
            (progn
              (semantic-clear-toplevel-cache)
              (semantic-fetch-tags))
          (quit
           (message "semantic-unmatched-syntax-tokens:\
 parsing of buffer canceled"))
          )))
    semantic-unmatched-syntax-cache)

(defun semantic-clear-toplevel-cache ()
  "Clear the toplevel tag cache for the current buffer.
Clearing the cache will force a complete reparse next time a tag list
is requested."
  (interactive)
  (run-hooks 'semantic-before-toplevel-cache-flush-hook)
  (setq semantic--buffer-cache nil)
  (semantic-clear-unmatched-syntax-cache)
  (semantic-clear-parser-warnings)
  ;; Nuke all semantic overlays.  This is faster than deleting based
  ;; on our data structure.
  (let ((l (semantic-overlay-lists)))
    (mapc 'semantic-delete-overlay-maybe (car l))
    (mapc 'semantic-delete-overlay-maybe (cdr l))
    )
  (semantic-parse-tree-set-needs-rebuild)
  ;; Remove this hook which tracks if a buffer is up to date or not.
  (remove-hook 'after-change-functions 'semantic-change-function t)
  ;; Old model.  Delete someday.
  ;;(run-hooks 'semantic-after-toplevel-bovinate-hook)

  (run-hook-with-args 'semantic-after-toplevel-cache-change-hook
		      semantic--buffer-cache)

  (setq semantic--completion-cache nil))

(defvar semantic-bovinate-nonterminal-check-obarray)

(defun semantic--set-buffer-cache (tagtable)
  "Set the toplevel tag cache to TAGTABLE."
  (setq semantic--buffer-cache tagtable
        semantic-unmatched-syntax-cache-check nil)
  ;; This is specific to the bovine parser.
  (set (make-local-variable 'semantic-bovinate-nonterminal-check-obarray)
       nil)
  (semantic-parse-tree-set-up-to-date)
  (semantic-make-local-hook 'after-change-functions)
  (add-hook 'after-change-functions 'semantic-change-function nil t)
  (run-hook-with-args 'semantic-after-toplevel-cache-change-hook
		      semantic--buffer-cache)
  (setq semantic--completion-cache nil)
  ;; Refresh the display of unmatched syntax tokens if enabled
  (run-hook-with-args 'semantic-unmatched-syntax-hook
                      semantic-unmatched-syntax-cache)
  ;; Old Semantic 1.3 hook API.  Maybe useful forever?
  (run-hooks 'semantic-after-toplevel-bovinate-hook)
  )

(defvar semantic-working-type 'percent
  "*The type of working message to use when parsing.
'percent means we are doing a linear parse through the buffer.
'dynamic means we are reparsing specific tags.")
(semantic-varalias-obsolete 'semantic-bovination-working-type
			    'semantic-working-type "23.2")

(defvar semantic-minimum-working-buffer-size (* 1024 5)
  "*The minimum size of a buffer before working messages are displayed.
Buffers smaller than this will parse silently.
Buffers larger than this will display the working progress bar.")

(defsubst semantic-parser-working-message (&optional arg)
  "Return the message string displayed while parsing.
If optional argument ARG is non-nil it is appended to the message
string."
  (concat "Parsing"
	  (if arg (format " %s" arg))
	  (if semantic-parser-name (format " (%s)" semantic-parser-name))
	  "..."))

;;; Application Parser Entry Points
;;
;; The best way to call the parser from programs is via
;; `semantic-fetch-tags'.  This, in turn, uses other internal
;; API functions which plug-in parsers can take advantage of.

(defun semantic-fetch-tags ()
  "Fetch semantic tags from the current buffer.
If the buffer cache is up to date, return that.
If the buffer cache is out of date, attempt an incremental reparse.
If the buffer has not been parsed before, or if the incremental reparse
fails, then parse the entire buffer.
If a lexical error had been previously discovered and the buffer
was marked unparseable, then do nothing, and return the cache."
  (and
   ;; Is this a semantic enabled buffer?
   (semantic-active-p)
   ;; Application hooks say the buffer is safe for parsing
   (run-hook-with-args-until-failure
    'semantic-before-toplevel-bovination-hook)
   (run-hook-with-args-until-failure
    'semantic--before-fetch-tags-hook)
   ;; If the buffer was previously marked unparseable,
   ;; then don't waste our time.
   (not (semantic-parse-tree-unparseable-p))
   ;; The parse tree actually needs to be refreshed
   (not (semantic-parse-tree-up-to-date-p))
   ;; So do it!
   (let* ((gc-cons-threshold (max gc-cons-threshold 10000000))
          (semantic-lex-block-streams nil)
          (res nil))
     (garbage-collect)
     (cond

;;;; Try the incremental parser to do a fast update.
     ((semantic-parse-tree-needs-update-p)
      (setq res (semantic-parse-changes))
      (if (semantic-parse-tree-needs-rebuild-p)
          ;; If the partial reparse fails, jump to a full reparse.
          (semantic-fetch-tags)
        ;; Clear the cache of unmatched syntax tokens
        ;;
        ;; NOTE TO SELF:
        ;;
        ;; Move this into the incremental parser.  This is a bug.
        ;;
        (semantic-clear-unmatched-syntax-cache)
        (run-hook-with-args ;; Let hooks know the updated tags
         'semantic-after-partial-cache-change-hook res))
      (setq semantic--completion-cache nil))

;;;; Parse the whole system.
     ((semantic-parse-tree-needs-rebuild-p)
      ;; Use Emacs's built-in progress-reporter
      (let ((semantic--progress-reporter
	     (and (>= (point-max) semantic-minimum-working-buffer-size)
		  (eq semantic-working-type 'percent)
		  (make-progress-reporter
		   (semantic-parser-working-message (buffer-name))
		   0 100))))
	(setq res (semantic-parse-region (point-min) (point-max)))
	(if semantic--progress-reporter
	    (progress-reporter-done semantic--progress-reporter)))

      ;; Clear the caches when we see there were no errors.
      ;; But preserve the unmatched syntax cache and warnings!
      (let (semantic-unmatched-syntax-cache
	    semantic-unmatched-syntax-cache-check
	    semantic-parser-warnings)
	(semantic-clear-toplevel-cache))
      ;; Set up the new overlays
      (semantic--tag-link-list-to-buffer res)
      ;; Set up the cache with the new results
      (semantic--set-buffer-cache res)
      ))))

  ;; Always return the current parse tree.
  semantic--buffer-cache)

(defun semantic-refresh-tags-safe ()
  "Refresh the current buffer's tags safely.

Return non-nil if the refresh was successful.
Return nil if there is some sort of syntax error preventing a reparse.

Does nothing if the current buffer doesn't need reparsing."

  ;; These checks actually occur in `semantic-fetch-tags', but if we
  ;; do them here, then all the bovination hooks are not run, and
  ;; we save lots of time.
  (cond
   ;; If the buffer was previously marked unparseable,
   ;; then don't waste our time.
   ((semantic-parse-tree-unparseable-p)
    nil)
   ;; The parse tree is already ok.
   ((semantic-parse-tree-up-to-date-p)
    t)
   (t
    (let* ((inhibit-quit nil)
	   (lexically-safe t)
	   )

      (unwind-protect
	  ;; Perform the parsing.
	  (progn
	    (when (semantic-lex-catch-errors safe-refresh
		    (save-excursion (semantic-fetch-tags))
		    nil)
	      ;; If we are here, it is because the lexical step failed,
	      ;; probably due to unterminated lists or something like that.

	      ;; We do nothing, and just wait for the next idle timer
	      ;; to go off.  In the meantime, remember this, and make sure
	      ;; no other idle services can get executed.
	      (setq lexically-safe nil))
	    )
	)
      ;; Return if we are lexically safe
      lexically-safe))))

(defun semantic-bovinate-toplevel (&optional ignored)
  "Backward compatibility function."
  (semantic-fetch-tags))
(make-obsolete 'semantic-bovinate-toplevel 'semantic-fetch-tags "23.2")

;; Another approach is to let Emacs call the parser on idle time, when
;; needed, use `semantic-fetch-available-tags' to only retrieve
;; available tags, and setup the `semantic-after-*-hook' hooks to
;; synchronize with new tags when they become available.

(defsubst semantic-fetch-available-tags ()
  "Fetch available semantic tags from the current buffer.
That is, return tags currently in the cache without parsing the
current buffer.
Parse operations happen asynchronously when needed on Emacs idle time.
Use the `semantic-after-toplevel-cache-change-hook' and
`semantic-after-partial-cache-change-hook' hooks to synchronize with
new tags when they become available."
  semantic--buffer-cache)

;;; Iterative parser helper function
;;
;; Iterative parsers are better than rule-based iterative functions
;; in that they can handle obscure errors more cleanly.
;;
;; `semantic-repeat-parse-whole-stream' abstracts this action for
;; other parser centric routines.
;;
(defun semantic-repeat-parse-whole-stream
  (stream nonterm &optional returnonerror)
  "Iteratively parse the entire stream STREAM starting with NONTERM.
Optional argument RETURNONERROR indicates that the parser should exit
with the current results on a parse error.
This function returns semantic tags without overlays."
  (let ((result nil)
        (case-fold-search semantic-case-fold)
        nontermsym tag)
    (while stream
      (setq nontermsym (semantic-parse-stream stream nonterm)
            tag (car (cdr nontermsym)))
      (if (not nontermsym)
          (error "Parse error @ %d" (car (cdr (car stream)))))
      (if (eq (car nontermsym) stream)
	  (error "Parser error: Infinite loop?"))
      (if tag
          (if (car tag)
              (setq tag (mapcar
                         #'(lambda (tag)
                             ;; Set the 'reparse-symbol property to
                             ;; NONTERM unless it was already setup
                             ;; by a tag expander
                             (or (semantic--tag-get-property
                                  tag 'reparse-symbol)
                                 (semantic--tag-put-property
                                  tag 'reparse-symbol nonterm))
                             tag)
                         (semantic--tag-expand tag))
                    result (append tag result))
            ;; No error in this case, a purposeful nil means don't
            ;; store anything.
            )
        (if returnonerror
            (setq stream nil)
          ;; The current item in the stream didn't match, so add it to
          ;; the list of syntax items which didn't match.
          (setq semantic-unmatched-syntax-cache
                (cons (car stream) semantic-unmatched-syntax-cache))
          ))
      ;; Designated to ignore.
      (setq stream (car nontermsym))
      (if stream
	  ;; Use Emacs's built-in progress reporter:
	  (and (boundp 'semantic--progress-reporter)
	       semantic--progress-reporter
	       (eq semantic-working-type 'percent)
	       (progress-reporter-update
		semantic--progress-reporter
		(/ (* 100 (semantic-lex-token-start (car stream)))
		   (point-max))))))
    result))

;;; Parsing Warnings:
;;
;; Parsing a buffer may result in non-critical things that we should
;; alert the user to without interrupting the normal flow.
;;
;; Any parser can use this API to provide a list of warnings during a
;; parse which a user may want to investigate.
(defvar semantic-parser-warnings nil
  "A list of parser warnings since the last full reparse.")
(make-variable-buffer-local 'semantic-parser-warnings)

(defun semantic-clear-parser-warnings ()
  "Clear the current list of parser warnings for this buffer."
  (setq semantic-parser-warnings nil))

(defun semantic-push-parser-warning (warning start end)
  "Add a parser WARNING that covers text from START to END."
  (setq semantic-parser-warnings
	(cons (cons warning (cons start end))
	      semantic-parser-warnings)))

(defun semantic-dump-parser-warnings ()
  "Dump any parser warnings."
  (interactive)
  (if semantic-parser-warnings
      (let ((pw semantic-parser-warnings))
	(pop-to-buffer "*Parser Warnings*")
	(require 'pp)
	(erase-buffer)
	(insert (pp-to-string pw))
	(goto-char (point-min)))
    (message "No parser warnings.")))



;;; Compatibility:
;;
;; Semantic 1.x parser action helper functions, used by some parsers.
;; Please move away from these functions, and try using semantic 2.x
;; interfaces instead.
;;
(defsubst semantic-bovinate-region-until-error
  (start end nonterm &optional depth)
  "NOTE: Use `semantic-parse-region' instead.

Bovinate between START and END starting with NONTERM.
Optional DEPTH specifies how many levels of parenthesis to enter.
This command will parse until an error is encountered, and return
the list of everything found until that moment.
This is meant for finding variable definitions at the beginning of
code blocks in methods.  If `bovine-inner-scope' can also support
commands, use `semantic-bovinate-from-nonterminal-full'."
  (semantic-parse-region start end nonterm depth t))
(make-obsolete 'semantic-bovinate-region-until-error
               'semantic-parse-region "23.2")

(defsubst semantic-bovinate-from-nonterminal
  (start end nonterm &optional depth length)
  "Bovinate from within a nonterminal lambda from START to END.
Argument NONTERM is the nonterminal symbol to start with.
Optional argument DEPTH is the depth of lists to dive into.  When used
in a `lambda' of a MATCH-LIST, there is no need to include a START and
END part.
Optional argument LENGTH specifies we are only interested in LENGTH
tokens."
  (car-safe (cdr (semantic-parse-stream
		  (semantic-lex start end (or depth 1) length)
		  nonterm))))

(defsubst semantic-bovinate-from-nonterminal-full
  (start end nonterm &optional depth)
  "NOTE: Use `semantic-parse-region' instead.

Bovinate from within a nonterminal lambda from START to END.
Iterates until all the space between START and END is exhausted.
Argument NONTERM is the nonterminal symbol to start with.
If NONTERM is nil, use `bovine-block-toplevel'.
Optional argument DEPTH is the depth of lists to dive into.
When used in a `lambda' of a MATCH-LIST, there is no need to include
a START and END part."
  (semantic-parse-region start end nonterm (or depth 1)))
(make-obsolete 'semantic-bovinate-from-nonterminal-full
               'semantic-parse-region "23.2")

;;; User interface

(defun semantic-force-refresh ()
  "Force a full refresh of the current buffer's tags.
Throw away all the old tags, and recreate the tag database."
  (interactive)
  (semantic-clear-toplevel-cache)
  (semantic-fetch-tags)
  (message "Buffer reparsed."))

(defvar semantic-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Key bindings:
    ;; (define-key km "f"    'senator-search-set-tag-class-filter)
    ;; (define-key km "i"    'senator-isearch-toggle-semantic-mode)
    (define-key map "\C-c,j" 'semantic-complete-jump-local)
    (define-key map "\C-c,J" 'semantic-complete-jump)
    (define-key map "\C-c,m" 'semantic-complete-jump-local-members)
    (define-key map "\C-c,g" 'semantic-symref-symbol)
    (define-key map "\C-c,G" 'semantic-symref)
    (define-key map "\C-c,p" 'senator-previous-tag)
    (define-key map "\C-c,n" 'senator-next-tag)
    (define-key map "\C-c,u" 'senator-go-to-up-reference)
    (define-key map "\C-c, " 'semantic-complete-analyze-inline)
    (define-key map "\C-c,\C-w" 'senator-kill-tag)
    (define-key map "\C-c,\M-w" 'senator-copy-tag)
    (define-key map "\C-c,\C-y" 'senator-yank-tag)
    (define-key map "\C-c,r" 'senator-copy-tag-to-register)
    (define-key map "\C-c,," 'semantic-force-refresh)
    (define-key map [?\C-c ?, up] 'senator-transpose-tags-up)
    (define-key map [?\C-c ?, down] 'senator-transpose-tags-down)
    (define-key map "\C-c,l" 'semantic-analyze-possible-completions)
    ;; This hack avoids showing the CEDET menu twice if ede-minor-mode
    ;; and Semantic are both enabled.  Is there a better way?
    (define-key map [menu-bar cedet-menu]
      (list 'menu-item "Development" cedet-menu-map
	    :enable (quote (not (bound-and-true-p global-ede-mode)))))
    ;; (define-key km "-"    'senator-fold-tag)
    ;; (define-key km "+"    'senator-unfold-tag)
    map))

;; Activate the Semantic items in cedet-menu-map
(let ((navigate-menu (make-sparse-keymap "Navigate Tags"))
      (edit-menu (make-sparse-keymap "Edit Tags")))

  ;; Edit Tags submenu:
  (define-key edit-menu [semantic-analyze-possible-completions]
    '(menu-item "List Completions" semantic-analyze-possible-completions
		:help "Display a list of completions for the tag at point"))
  (define-key edit-menu [semantic-complete-analyze-inline]
    '(menu-item "Complete Tag Inline" semantic-complete-analyze-inline
		:help "Display inline completion for the tag at point"))
  (define-key edit-menu [semantic-completion-separator]
    '("--"))
  (define-key edit-menu [senator-transpose-tags-down]
    '(menu-item "Transpose Tags Down" senator-transpose-tags-down
		:active (semantic-current-tag)
		:help "Transpose the current tag and the next tag"))
  (define-key edit-menu [senator-transpose-tags-up]
    '(menu-item "Transpose Tags Up" senator-transpose-tags-up
		:active (semantic-current-tag)
		:help "Transpose the current tag and the previous tag"))
  (define-key edit-menu [semantic-edit-separator]
    '("--"))
  (define-key edit-menu [senator-yank-tag]
    '(menu-item "Yank Tag" senator-yank-tag
		:active (not (ring-empty-p senator-tag-ring))
		:help "Yank the head of the tag ring into the buffer"))
  (define-key edit-menu [senator-copy-tag-to-register]
    '(menu-item "Copy Tag To Register" senator-copy-tag-to-register
		:active (semantic-current-tag)
		:help "Yank the head of the tag ring into the buffer"))
  (define-key edit-menu [senator-copy-tag]
    '(menu-item "Copy Tag" senator-copy-tag
		:active (semantic-current-tag)
		:help "Copy the current tag to the tag ring"))
  (define-key edit-menu [senator-kill-tag]
    '(menu-item "Kill Tag" senator-kill-tag
		:active (semantic-current-tag)
		:help "Kill the current tag, and copy it to the tag ring"))

  ;; Navigate Tags submenu:
  (define-key navigate-menu [senator-narrow-to-defun]
    '(menu-item "Narrow to Tag" senator-narrow-to-defun
		:active (semantic-current-tag)
		:help "Narrow the buffer to the bounds of the current tag"))
  (define-key navigate-menu [semantic-narrow-to-defun-separator]
    '("--"))
  (define-key navigate-menu [semantic-symref-symbol]
    '(menu-item "Find Tag References..." semantic-symref-symbol
		:help "Read a tag and list the references to it"))
  (define-key navigate-menu [semantic-complete-jump]
    '(menu-item "Find Tag Globally..." semantic-complete-jump
		:help "Read a tag name and find it in the current project"))
  (define-key navigate-menu [semantic-complete-jump-local-members]
    '(menu-item "Find Local Members ..." semantic-complete-jump-local-members
		:help "Read a tag name and find a local member with that name"))
  (define-key navigate-menu [semantic-complete-jump-local]
    '(menu-item "Find Tag in This Buffer..." semantic-complete-jump-local
		:help "Read a tag name and find it in this buffer"))
  (define-key navigate-menu [semantic-navigation-separator]
    '("--"))
  (define-key navigate-menu [senator-go-to-up-reference]
    '(menu-item "Parent Tag" senator-go-to-up-reference
		:help "Navigate up one reference by tag"))
  (define-key navigate-menu [senator-next-tag]
    '(menu-item "Next Tag" senator-next-tag
		:help "Go to the next tag"))
  (define-key navigate-menu [senator-previous-tag]
    '(menu-item "Previous Tag" senator-previous-tag
		:help "Go to the previous tag"))

  ;; Top level menu items:
  (define-key cedet-menu-map [semantic-force-refresh]
    '(menu-item "Reparse Buffer" semantic-force-refresh
		:help "Force a full reparse of the current buffer"
		:visible semantic-mode))
  (define-key cedet-menu-map [semantic-edit-menu]
    `(menu-item "Edit Tags" ,edit-menu
		:visible semantic-mode))
  (define-key cedet-menu-map [navigate-menu]
    `(menu-item "Navigate Tags" ,navigate-menu
		:visible semantic-mode))
  (define-key cedet-menu-map [semantic-options-separator]
    '("--"))
  (define-key cedet-menu-map [global-semantic-highlight-func-mode]
    '(menu-item "Highlight Current Function" global-semantic-highlight-func-mode
		:help "Highlight the tag at point"
		:visible semantic-mode
		:button (:toggle . global-semantic-highlight-func-mode)))
  (define-key cedet-menu-map [global-semantic-decoration-mode]
    '(menu-item "Decorate Tags" global-semantic-decoration-mode
		:help "Decorate tags based on tag attributes"
		:visible semantic-mode
		:button (:toggle . (bound-and-true-p
				    global-semantic-decoration-mode))))
  (define-key cedet-menu-map [global-semantic-idle-completions-mode]
    '(menu-item "Show Tag Completions" global-semantic-idle-completions-mode
		:help "Show tag completions when idle"
		:visible semantic-mode
		:enable global-semantic-idle-scheduler-mode
		:button (:toggle . global-semantic-idle-completions-mode)))
  (define-key cedet-menu-map [global-semantic-idle-summary-mode]
    '(menu-item "Show Tag Summaries" global-semantic-idle-summary-mode
		:help "Show tag summaries when idle"
		:visible semantic-mode
		:enable global-semantic-idle-scheduler-mode
		:button (:toggle . global-semantic-idle-summary-mode)))
  (define-key cedet-menu-map [global-semantic-idle-scheduler-mode]
    '(menu-item "Reparse When Idle" global-semantic-idle-scheduler-mode
		:help "Keep a buffer's parse tree up to date when idle"
		:visible semantic-mode
		:button (:toggle . global-semantic-idle-scheduler-mode)))
  (define-key cedet-menu-map [global-semanticdb-minor-mode]
    '(menu-item "Semantic Database" global-semanticdb-minor-mode
		:help "Store tag information in a database"
		:visible semantic-mode
		:button (:toggle . global-semanticdb-minor-mode))))

;; The `semantic-mode' command, in conjunction with the
;; `semantic-default-submodes' variable, toggles Semantic's various
;; auxiliary minor modes.

(defvar semantic-load-system-cache-loaded nil
  "Non-nil when the Semantic system caches have been loaded.
Prevent this load system from loading files in twice.")

(defconst semantic-submode-list
  '(global-semantic-highlight-func-mode
    global-semantic-decoration-mode
    global-semantic-stickyfunc-mode
    global-semantic-idle-completions-mode
    global-semantic-idle-scheduler-mode
    global-semanticdb-minor-mode
    global-semantic-idle-summary-mode
    global-semantic-mru-bookmark-mode)
  "List of auxiliary minor modes in the Semantic package.")

;;;###autoload
(defcustom semantic-default-submodes
  '(global-semantic-idle-scheduler-mode global-semanticdb-minor-mode)
  "List of auxiliary Semantic minor modes enabled by `semantic-mode'.
The possible elements of this list include the following:

 `global-semanticdb-minor-mode'        - Maintain tag database.
 `global-semantic-idle-scheduler-mode' - Reparse buffer when idle.
 `global-semantic-idle-summary-mode'   - Show summary of tag at point.
 `global-semantic-idle-completions-mode' - Show completions when idle.
 `global-semantic-decoration-mode'     - Additional tag decorations.
 `global-semantic-highlight-func-mode' - Highlight the current tag.
 `global-semantic-stickyfunc-mode'     - Show current fun in header line.
 `global-semantic-mru-bookmark-mode'   - Provide `switch-to-buffer'-like
                                         keybinding for tag names."
  :group 'semantic
  :type `(set ,@(mapcar (lambda (c) (list 'const c))
			semantic-submode-list)))

;;;###autoload
(define-minor-mode semantic-mode
  "Toggle parser features (Semantic mode).
With a prefix argument ARG, enable Semantic mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
Semantic mode if ARG is omitted or nil.

In Semantic mode, Emacs parses the buffers you visit for their
semantic content.  This information is used by a variety of
auxiliary minor modes, listed in `semantic-default-submodes';
all the minor modes in this list are also enabled when you enable
Semantic mode.

\\{semantic-mode-map}"
  :global t
  :group 'semantic
  (if semantic-mode
      ;; Turn on Semantic mode
      (progn
	;; Enable all the global auxiliary minor modes in
	;; `semantic-submode-list'.
	(dolist (mode semantic-submode-list)
	  (if (memq mode semantic-default-submodes)
	      (funcall mode 1)))
	(unless semantic-load-system-cache-loaded
	  (setq semantic-load-system-cache-loaded t)
	  (when (and (boundp 'semanticdb-default-system-save-directory)
		     (stringp semanticdb-default-system-save-directory)
		     (file-exists-p semanticdb-default-system-save-directory))
	    (require 'semantic/db-ebrowse)
	    (semanticdb-load-ebrowse-caches)))
	(add-hook 'mode-local-init-hook 'semantic-new-buffer-fcn)
	;; Add semantic-ia-complete-symbol to
	;; completion-at-point-functions, so that it is run from
	;; M-TAB.
	(add-hook 'completion-at-point-functions
		  'semantic-completion-at-point-function)
	(if global-ede-mode
	    (define-key cedet-menu-map [cedet-menu-separator] '("--")))
	(dolist (b (buffer-list))
	  (with-current-buffer b
	    (semantic-new-buffer-fcn))))
    ;; Disable all Semantic features.
    (remove-hook 'mode-local-init-hook 'semantic-new-buffer-fcn)
    (remove-hook 'completion-at-point-functions
		 'semantic-completion-at-point-function)
    (define-key cedet-menu-map [cedet-menu-separator] nil)
    (define-key cedet-menu-map [semantic-options-separator] nil)
    ;; FIXME: handle semanticdb-load-ebrowse-caches
    (dolist (mode semantic-submode-list)
      (if (and (boundp mode) (eval mode))
	  (funcall mode -1)))))

(defun semantic-completion-at-point-function ()
  'semantic-ia-complete-symbol)

;;; Autoload some functions that are not in semantic/loaddefs

(autoload 'global-semantic-idle-completions-mode "semantic/idle"
  "Toggle global use of `semantic-idle-completions-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle." t nil)

(autoload 'semantic-idle-completions-mode "semantic/idle"
  "Display a list of possible completions in a tooltip.

This is a minor mode which performs actions during idle time.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled." t nil)

(autoload 'global-semantic-idle-summary-mode "semantic/idle"
  "Toggle global use of `semantic-idle-summary-mode'.
If ARG is positive, enable, if it is negative, disable.
If ARG is nil, then toggle." t nil)

(autoload 'semantic-idle-summary-mode "semantic/idle"
  "Display a tag summary of the lexical token under the cursor.
Call `semantic-idle-summary-current-symbol-info' for getting the
current tag to display information.

This is a minor mode which performs actions during idle time.
With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled." t nil)

(autoload 'srecode-template-setup-parser "srecode/srecode-template"
  "Set up buffer for parsing SRecode template files." t nil)

(provide 'semantic)

;; Semantic-util is a part of the semantic API.  Include it last
;; because it depends on semantic.
(require 'semantic/util)

;; (require 'semantic/load)

;;; semantic.el ends here
