;;; completion.el --- dynamic word-completion code

;; Copyright (C) 1990, 1993, 1995, 1997, 2001-2012
;;   Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: abbrev convenience
;; Author: Jim Salem <alem@bbnplanet.com> of Thinking Machines Inc.
;;  (ideas suggested by Brewster Kahle)

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

;; What to put in .emacs
;;-----------------------
;; (dynamic-completion-mode)

;;---------------------------------------------------------------------------
;; Documentation [Slightly out of date]
;;---------------------------------------------------------------------------
;;  (also check the documentation string of the functions)
;;
;; Introduction
;;---------------
;;
;;     After you type a few characters, pressing the "complete" key inserts
;; the rest of the word you are likely to type.
;;
;; This watches all the words that you type and remembers them.  When
;; typing a new word, pressing "complete" (meta-return) "completes" the
;; word by inserting the most recently used word that begins with the
;; same characters.  If you press meta-return repeatedly, it cycles
;; through all the words it knows about.
;;
;;  If you like the completion then just continue typing, it is as if you
;; entered the text by hand.  If you want the inserted extra characters
;; to go away, type control-w or delete.  More options are described below.
;;
;;  The guesses are made in the order of the most recently "used".  Typing
;; in a word and then typing a separator character (such as a space) "uses"
;; the word.  So does moving a cursor over the word.  If no words are found,
;; it uses an extended version of the dabbrev style completion.
;;
;;   You automatically save the completions you use to a file between
;; sessions.
;;
;;   Completion enables programmers to enter longer, more descriptive
;; variable names while typing fewer keystrokes than they normally would.
;;
;;
;; Full documentation
;;---------------------
;;
;;   A "word" is any string containing characters with either word or symbol
;; syntax.  [E.G. Any alphanumeric string with hyphens, underscores, etc.]
;; Unless you change the constants, you must type at least three characters
;; for the word to be recognized.  Only words longer than 6 characters are
;; saved.
;;
;;   When you load this file, completion will be on.  I suggest you use the
;; compiled version (because it is noticeably faster).
;;
;;  M-x completion-mode toggles whether or not new words are added to the
;; database by changing the value of enable-completion.
;;
;;  SAVING/LOADING COMPLETIONS
;;   Completions are automatically saved from one session to another
;; (unless save-completions-flag or enable-completion is nil).
;; Activating this minor-mode (calling completion-initialize) loads
;; a completions database for a saved completions file
;; (default: ~/.completions).  When you exit, Emacs saves a copy of the
;; completions that you often use.  When you next start, Emacs loads in
;; the saved completion file.
;;
;;   The number of completions saved depends loosely on
;; *saved-completions-decay-factor*.  Completions that have never been
;; inserted via "complete" are not saved.  You are encouraged to experiment
;; with different functions (see compute-completion-min-num-uses).
;;
;;   Some completions are permanent and are always saved out.  These
;; completions have their num-uses slot set to T.  Use
;; add-permanent-completion to do this
;;
;;   Completions are saved only if enable-completion is T.  The number of old
;; versions kept of the saved completions file is controlled by
;; completions-file-versions-kept.
;;
;; COMPLETE KEY OPTIONS
;;   The complete function takes a numeric arguments.
;;  control-u :: leave the point at the beginning of the completion rather
;;               than the middle.
;;  a number  :: rotate through the possible completions by that amount
;;  `-'       :: same as -1 (insert previous completion)
;;
;; HOW THE DATABASE IS MAINTAINED
;;  <write>
;;
;; UPDATING THE DATABASE MANUALLY
;;   m-x kill-completion
;;     kills the completion at point.
;;   m-x add-completion
;;   m-x add-permanent-completion
;;
;; UPDATING THE DATABASE FROM A SOURCE CODE FILE
;;   m-x add-completions-from-buffer
;;     Parses all the definition names from a C or LISP mode buffer and
;;     adds them to the completion database.
;;
;;   m-x add-completions-from-lisp-file
;;     Parses all the definition names from a C or Lisp mode file and
;;     adds them to the completion database.
;;
;; UPDATING THE DATABASE FROM A TAGS TABLE
;;   m-x add-completions-from-tags-table
;;     Adds completions from the current tags-table-buffer.
;;
;; HOW A COMPLETION IS FOUND
;;  <write>
;;
;; STRING CASING
;;   Completion is string case independent if case-fold-search has its
;;  normal default of T.  Also when the completion is inserted the case of the
;;  entry is coerced appropriately.
;;  [E.G.  APP --> APPROPRIATELY     app --> appropriately
;;         App --> Appropriately]
;;
;; INITIALIZATION
;;  The form `(completion-initialize)' initializes the completion system by
;; trying to load in the user's completions.  After the first call, further
;; calls have no effect so one should be careful not to put the form in a
;; site's standard site-init file.
;;
;;---------------------------------------------------------------------------
;;
;;

;;---------------------------------------------------------------------------
;; Functions you might like to call
;;---------------------------------------------------------------------------
;;
;;  add-completion  string &optional num-uses
;;    Adds a new string to the database
;;
;;  add-permanent-completion  string
;;    Adds a new string to the database with num-uses = T
;;

;;  kill-completion string
;;    Kills the completion from the database.
;;
;;  clear-all-completions
;;    Clears the database
;;
;;  list-all-completions
;;    Returns a list of all completions.
;;
;;
;;  next-completion string &optional index
;;    Returns a completion entry that starts with string.
;;
;;  find-exact-completion string
;;    Returns a completion entry that exactly matches string.
;;
;;  complete
;;    Inserts a completion at point
;;
;;  completion-initialize
;;    Loads the completions file and sets up so that exiting emacs will
;;  save them.
;;
;;  save-completions-to-file &optional filename
;;  load-completions-from-file &optional filename
;;
;;-----------------------------------------------
;; Other functions
;;-----------------------------------------------
;;
;;  get-completion-list string
;;
;; These things are for manipulating the structure
;;  make-completion string num-uses
;;  completion-num-uses completion
;;  completion-string completion
;;  set-completion-num-uses completion num-uses
;;  set-completion-string completion string
;;
;;

;;-----------------------------------------------
;; To Do :: (anybody ?)
;;-----------------------------------------------
;;
;;   Implement Lookup and keyboard interface in C
;;   Add package prefix smarts (for Common Lisp)
;;   Add autoprompting of possible completions after every keystroke (fast
;;      terminals only !)
;;   Add doc. to texinfo
;;
;;
;;-----------------------------------------------
;;; Change Log:
;;-----------------------------------------------
;;    Sometime in '84 Brewster implemented a somewhat buggy version for
;; Symbolics LISPMs.
;;    Jan. '85 Jim became enamored of the idea and implemented a faster,
;; more robust version.
;;    With input from many users at TMC, (rose, craig, and gls come to mind),
;; the current style of interface was developed.
;;    9/87, Jim and Brewster took terminals home.  Yuck.  After
;; complaining for a while Brewster implemented a subset of the current
;; LISPM version for GNU Emacs.
;;    8/88  After complaining for a while (and with sufficient
;; promised rewards), Jim reimplemented a version of GNU completion
;; superior to that of the LISPM version.
;;
;;-----------------------------------------------
;; Acknowledgements
;;-----------------------------------------------
;;  Cliff Lasser (cal@think.com), Kevin Herbert (kph@cisco.com),
;;  eero@media-lab, kgk@cs.brown.edu, jla@ai.mit.edu,
;;
;;-----------------------------------------------
;; Change Log
;;-----------------------------------------------
;; From version 9 to 10
;;  - Allowance for non-integral *completion-version* nos.
;;  - Fix cmpl-apply-as-top-level for keyboard macros
;;  - Fix broken completion merging (in save-completions-to-file)
;;  - More misc. fixes for version 19.0 of emacs
;;
;; From Version 8 to 9
;;  - Ported to version 19.0 of emacs (backcompatible with version 18)
;;  - Added add-completions-from-tags-table (with thanks to eero@media-lab)
;;
;; From Version 7 to 8
;;  - Misc. changes to comments
;;  - new completion key bindings: c-x o, M->, M-<, c-a, c-e
;;  - cdabbrev now checks all the visible window buffers and the "other buffer"
;;  - `%' is now a symbol character rather than a separator (except in C mode)
;;
;; From Version 6 to 7
;;  - Fixed bug with saving out .completion file the first time
;;
;; From Version 5 to 6
;;  - removed statistics recording
;;  - reworked advise to handle autoloads
;;  - Fixed fortran mode support
;;  - Added new cursor motion triggers
;;
;; From Version 4 to 5
;;  - doesn't bother saving if nothing has changed
;;  - auto-save if haven't used for a 1/2 hour
;;  - save period extended to two weeks
;;  - minor fix to capitalization code
;;  - added *completion-auto-save-period* to variables recorded.
;;  - added reenter protection to cmpl-record-statistics-filter
;;  - added backup protection to save-completions-to-file (prevents
;;    problems with disk full errors)

;;; Code:

;;---------------------------------------------------------------------------
;; User changeable parameters
;;---------------------------------------------------------------------------

(defgroup completion nil
  "Dynamic word-completion code."
  :group 'matching
  :group 'convenience)


(defcustom enable-completion t
  "Non-nil means enable recording and saving of completions.
If nil, no new words are added to the database or saved to the init file."
  :type 'boolean
  :group 'completion)

(defcustom save-completions-flag t
  "Non-nil means save most-used completions when exiting Emacs.
See also `save-completions-retention-time'."
  :type 'boolean
  :group 'completion)

(defcustom save-completions-file-name
  (locate-user-emacs-file "completions" ".completions")
  "The filename to save completions to."
  :type 'file
  :group 'completion)

(defcustom save-completions-retention-time 336
  "Discard a completion if unused for this many hours.
\(1 day = 24, 1 week = 168).  If this is 0, non-permanent completions
will not be saved unless these are used.  Default is two weeks."
  :type 'integer
  :group 'completion)

(defcustom completion-on-separator-character nil
  "Non-nil means separator characters mark previous word as used.
This means the word will be saved as a completion."
  :type 'boolean
  :group 'completion)

(defcustom completions-file-versions-kept kept-new-versions
  "Number of versions to keep for the saved completions file."
  :type 'integer
  :group 'completion)

(defcustom completion-prompt-speed-threshold 4800
  "Minimum output speed at which to display next potential completion."
  :type 'integer
  :group 'completion)

(defcustom completion-cdabbrev-prompt-flag nil
  "If non-nil, the next completion prompt does a cdabbrev search.
This can be time consuming."
  :type 'boolean
  :group 'completion)

(defcustom completion-search-distance 15000
  "How far to search in the buffer when looking for completions.
In number of characters.  If nil, search the whole buffer."
  :type 'integer
  :group 'completion)

(defcustom completions-merging-modes '(lisp c)
  "List of modes {`c' or `lisp'} for automatic completions merging.
Definitions from visited files which have these modes
are automatically added to the completion database."
  :type '(set (const lisp) (const c))
  :group 'completion)

;;(defvar *record-cmpl-statistics-p* nil
;;  "*If non-nil, record completion statistics.")

;;(defvar *completion-auto-save-period* 1800
;;  "*The period in seconds to wait for emacs to be idle before autosaving
;;the completions.  Default is a 1/2 hour.")

(defvar completion-min-length 6
  "*The minimum length of a stored completion.
DON'T CHANGE WITHOUT RECOMPILING !  This is used by macros.")

(defvar completion-max-length 200
  "*The maximum length of a stored completion.
DON'T CHANGE WITHOUT RECOMPILING !  This is used by macros.")

(defvar completion-prefix-min-length 3
  "The minimum length of a completion search string.
DON'T CHANGE WITHOUT RECOMPILING !  This is used by macros.")

;;---------------------------------------------------------------------------
;; Internal Variables
;;---------------------------------------------------------------------------

(defvar cmpl-initialized-p nil
  "Set to t when the completion system is initialized.
Indicates that the old completion file has been read in.")

(defvar cmpl-completions-accepted-p nil
  "Set to t as soon as the first completion has been accepted.
Used to decide whether to save completions.")

(defvar cmpl-preceding-syntax)

(defvar completion-string)

;;---------------------------------------------------------------------------
;; Low level tools
;;---------------------------------------------------------------------------

;;-----------------------------------------------
;; String case coercion
;;-----------------------------------------------

(defun cmpl-string-case-type (string)
  "Return :capitalized, :up, :down, :mixed, or :neither for case of STRING."
  (let ((case-fold-search nil))
    (cond ((string-match "[[:lower:]]" string)
	   (cond ((string-match "[[:upper:]]" string)
		  (cond ((and (> (length string) 1)
			      (null (string-match "[[:upper:]]" string 1)))
			 :capitalized)
			(t
			 :mixed)))
		 (t :down)))
	  (t
	   (cond ((string-match "[[:upper:]]" string)
		  :up)
		 (t :neither))))))

;; Tests -
;; (cmpl-string-case-type "123ABCDEF456") --> :up
;; (cmpl-string-case-type "123abcdef456") --> :down
;; (cmpl-string-case-type "123aBcDeF456") --> :mixed
;; (cmpl-string-case-type "123456")       --> :neither
;; (cmpl-string-case-type "Abcde123")     --> :capitalized

(defun cmpl-coerce-string-case (string case-type)
  (cond ((eq case-type :down) (downcase string))
	((eq case-type :up) (upcase string))
	((eq case-type :capitalized)
	 (setq string (downcase string))
	 (aset string 0 (logand ?\337 (aref string 0)))
	 string)
	(t string)))

(defun cmpl-merge-string-cases (string-to-coerce given-string)
  (let ((string-case-type (cmpl-string-case-type string-to-coerce)))
    (cond ((memq string-case-type '(:down :up :capitalized))
	   ;; Found string is in a standard case.  Coerce to a type based on
	   ;; the given string
	   (cmpl-coerce-string-case string-to-coerce
			       (cmpl-string-case-type given-string)))
	  (t
	   ;; If the found string is in some unusual case, just insert it
	   ;; as is
	   string-to-coerce))))

;; Tests -
;; (cmpl-merge-string-cases "AbCdEf456" "abc")     --> AbCdEf456
;; (cmpl-merge-string-cases "abcdef456" "ABC")     --> ABCDEF456
;; (cmpl-merge-string-cases "ABCDEF456" "Abc")     --> Abcdef456
;; (cmpl-merge-string-cases "ABCDEF456" "abc")     --> abcdef456


(defun cmpl-hours-since-origin ()
  (let ((time (current-time)))
    (floor (+ (* 65536.0 (nth 0 time)) (nth 1 time)) 3600)))

;;---------------------------------------------------------------------------
;; "Symbol" parsing functions
;;---------------------------------------------------------------------------
;; The functions symbol-before-point, symbol-under-point, etc. quickly return
;; an appropriate symbol string.  The strategy is to temporarily change
;; the syntax table to enable fast symbol searching.  There are three classes
;; of syntax in these "symbol" syntax tables ::
;;
;; syntax (?_) - "symbol" chars (e.g. alphanumerics)
;; syntax (?w) - symbol chars to ignore at end of words (e.g. period).
;; syntax (? ) - everything else
;;
;; Thus by judicious use of scan-sexps and forward-word, we can get
;; the word we want relatively fast and without consing.
;;
;; Why do we need a separate category for "symbol chars to ignore at ends" ?
;; For example, in LISP we want starting :'s trimmed
;; so keyword argument specifiers also define the keyword completion.  And,
;; for example, in C we want `.' appearing in a structure ref. to
;; be kept intact in order to store the whole structure ref.; however, if
;; it appears at the end of a symbol it should be discarded because it is
;; probably used as a period.

;; Here is the default completion syntax ::
;; Symbol chars :: A-Z a-z 0-9 @ / \ * + ~ $ < > %
;; Symbol chars to ignore at ends :: _ : . -
;; Separator chars. :: <tab> <space> ! ^ & ( ) = ` | { } [ ] ; " ' #
;;                     , ? <Everything else>

;; Mode specific differences and notes ::
;;  LISP diffs ->
;;    Symbol chars :: ! & ? = ^
;;
;; C diffs ->
;;   Separator chars :: + * / : %
;;  A note on the hyphen (`-').  Perhaps the hyphen should also be a separator
;; char., however, we wanted to have completion symbols include pointer
;; references.  For example, "foo->bar" is a symbol as far as completion is
;; concerned.
;;
;; FORTRAN diffs ->
;;   Separator chars :: + - * / :
;;
;; Pathname diffs ->
;;   Symbol chars :: .
;;  Of course there is no pathname "mode" and in fact we have not implemented
;; this table.  However, if there was such a mode, this is what it would look
;; like.

;;-----------------------------------------------
;; Table definitions
;;-----------------------------------------------

(defconst completion-standard-syntax-table
  (let ((table (make-syntax-table))
	i)
    ;; Default syntax is whitespace.
    (setq i 0)
    (while (< i 256)
      (modify-syntax-entry i " " table)
      (setq i (1+ i)))
    ;; alpha chars
    (setq i 0)
    (while (< i 26)
      (modify-syntax-entry (+ ?a i) "_" table)
      (modify-syntax-entry (+ ?A i) "_" table)
      (setq i (1+ i)))
    ;; digit chars.
    (setq i 0)
    (while (< i 10)
      (modify-syntax-entry (+ ?0 i) "_" table)
      (setq i (1+ i)))
    ;; Other ones
    (let ((symbol-chars '(?@ ?/ ?\\ ?* ?+ ?~ ?$ ?< ?> ?%))
	  (symbol-chars-ignore '(?_ ?- ?: ?.)))
      (dolist (char symbol-chars)
	(modify-syntax-entry char "_" table))
      (dolist (char symbol-chars-ignore)
	(modify-syntax-entry char "w" table)))
    table))

(defvar completion-syntax-table completion-standard-syntax-table
  "This variable holds the current completion syntax table.")
(make-variable-buffer-local 'completion-syntax-table)

;;-----------------------------------------------
;; Symbol functions
;;-----------------------------------------------
(defvar cmpl-symbol-start nil
  "Holds first character of symbol, after any completion symbol function.")
(defvar cmpl-symbol-end nil
  "Holds last character of symbol, after any completion symbol function.")

(defun symbol-under-point ()
  "Return the symbol that the point is currently on.
But only if it is longer than `completion-min-length'."
  (with-syntax-table completion-syntax-table
    (when (memq (char-syntax (following-char)) '(?w ?_))
      ;; Cursor is on following-char and after preceding-char
      (let ((saved-point (point)))
        (setq cmpl-symbol-start (scan-sexps (1+ saved-point) -1)
              cmpl-symbol-end (scan-sexps saved-point 1))
        ;; Remove chars to ignore at the start.
        (cond ((= (char-syntax (char-after cmpl-symbol-start)) ?w)
               (goto-char cmpl-symbol-start)
               (forward-word 1)
               (setq cmpl-symbol-start (point))
               (goto-char saved-point)))
        ;; Remove chars to ignore at the end.
        (cond ((= (char-syntax (char-after (1- cmpl-symbol-end))) ?w)
               (goto-char cmpl-symbol-end)
               (forward-word -1)
               (setq cmpl-symbol-end (point))
               (goto-char saved-point)))
        ;; Return completion if the length is reasonable.
        (if (and (<= completion-min-length
                     (- cmpl-symbol-end cmpl-symbol-start))
                 (<= (- cmpl-symbol-end cmpl-symbol-start)
                     completion-max-length))
            (buffer-substring-no-properties
             cmpl-symbol-start cmpl-symbol-end))))))

;; tests for symbol-under-point
;;  `^' indicates cursor pos. where value is returned
;;  simple-word-test
;;  ^^^^^^^^^^^^^^^^  --> simple-word-test
;;  _harder_word_test_
;;  ^^^^^^^^^^^^^^^^^^ --> harder_word_test
;;  .___.______.
;;  --> nil
;;  /foo/bar/quux.hello
;;  ^^^^^^^^^^^^^^^^^^^ --> /foo/bar/quux.hello
;;

(defun symbol-before-point ()
  "Return a string of the symbol immediately before point.
Returns nil if there isn't one longer than `completion-min-length'."
  ;; This is called when a word separator is typed so it must be FAST !
  (with-syntax-table completion-syntax-table
    ;; Cursor is on following-char and after preceding-char
    (cond ((= (setq cmpl-preceding-syntax (char-syntax (preceding-char))) ?_)
           ;; Number of chars to ignore at end.
           (setq cmpl-symbol-end (point)
                 cmpl-symbol-start (scan-sexps cmpl-symbol-end -1))
           ;; Remove chars to ignore at the start.
           (cond ((= (char-syntax (char-after cmpl-symbol-start)) ?w)
                  (goto-char cmpl-symbol-start)
                  (forward-word 1)
                  (setq cmpl-symbol-start (point))
                  (goto-char cmpl-symbol-end)))
           ;; Return value if long enough.
           (if (>= cmpl-symbol-end
                   (+ cmpl-symbol-start completion-min-length))
               (buffer-substring-no-properties
                cmpl-symbol-start cmpl-symbol-end)))
          ((= cmpl-preceding-syntax ?w)
           ;; chars to ignore at end
           (let ((saved-point (point)))
             (setq cmpl-symbol-start (scan-sexps saved-point -1))
             ;; take off chars. from end
             (forward-word -1)
             (setq cmpl-symbol-end (point))
             ;; remove chars to ignore at the start
             (cond ((= (char-syntax (char-after cmpl-symbol-start)) ?w)
                    (goto-char cmpl-symbol-start)
                    (forward-word 1)
                    (setq cmpl-symbol-start (point))))
             ;; Restore state.
             (goto-char saved-point)
             ;; Return completion if the length is reasonable
             (if (and (<= completion-min-length
                          (- cmpl-symbol-end cmpl-symbol-start))
                      (<= (- cmpl-symbol-end cmpl-symbol-start)
                          completion-max-length))
                 (buffer-substring-no-properties
                  cmpl-symbol-start cmpl-symbol-end)))))))

;; tests for symbol-before-point
;;  `^' indicates cursor pos. where value is returned
;;  simple-word-test
;;  ^ --> nil
;;   ^ --> nil
;;          ^  --> simple-w
;;                  ^ --> simple-word-test
;;  _harder_word_test_
;;                   ^  --> harder_word_test
;;                    ^  --> harder_word_test
;;          ^ --> harder
;;  .___....
;;  --> nil

(defun symbol-under-or-before-point ()
  ;; This could be made slightly faster but it is better to avoid
  ;; copying all the code.
  ;; However, it is only used by the completion string prompter.
  ;; If it comes into common use, it could be rewritten.
  (if (memq (with-syntax-table completion-syntax-table
              (char-syntax (following-char)))
            '(?w ?_))
      (symbol-under-point)
    (symbol-before-point)))


(defun symbol-before-point-for-complete ()
  ;; "Returns a string of the symbol immediately before point
  ;; or nil if there isn't one.  Like symbol-before-point but doesn't trim the
  ;; end chars."
  ;; Cursor is on following-char and after preceding-char
  (with-syntax-table completion-syntax-table
    (cond ((memq (setq cmpl-preceding-syntax (char-syntax (preceding-char)))
                 '(?_ ?w))
           (setq cmpl-symbol-end (point)
                 cmpl-symbol-start (scan-sexps cmpl-symbol-end -1))
           ;; Remove chars to ignore at the start.
           (cond ((= (char-syntax (char-after cmpl-symbol-start)) ?w)
                  (goto-char cmpl-symbol-start)
                  (forward-word 1)
                  (setq cmpl-symbol-start (point))
                  (goto-char cmpl-symbol-end)))
           ;; Return completion if the length is reasonable.
           (if (and (<= completion-prefix-min-length
                        (- cmpl-symbol-end cmpl-symbol-start))
                    (<= (- cmpl-symbol-end cmpl-symbol-start)
                        completion-max-length))
               (buffer-substring-no-properties
                cmpl-symbol-start cmpl-symbol-end))))))

;; tests for symbol-before-point-for-complete
;;  `^' indicates cursor pos. where value is returned
;;  simple-word-test
;;  ^ --> nil
;;   ^ --> nil
;;          ^  --> simple-w
;;                  ^ --> simple-word-test
;;  _harder_word_test_
;;                   ^  --> harder_word_test
;;                    ^  --> harder_word_test_
;;          ^ --> harder_
;;  .___....
;;  --> nil



;;---------------------------------------------------------------------------
;; Statistics Recording
;;---------------------------------------------------------------------------

;; Note that the guts of this has been turned off.  The guts
;; are in completion-stats.el.

;;-----------------------------------------------
;; Conditionalizing code on *record-cmpl-statistics-p*
;;-----------------------------------------------
;; All statistics code outside this block should use this
(defmacro cmpl-statistics-block (&rest _body))
;;  "Only executes body if we are recording statistics."
;;  (list 'cond
;;	(list* '*record-cmpl-statistics-p* body)
;;	))

;;-----------------------------------------------
;; Completion Sources
;;-----------------------------------------------

;; ID numbers
(defconst cmpl-source-unknown 0)
(defconst cmpl-source-init-file 1)
(defconst cmpl-source-file-parsing 2)
(defconst cmpl-source-separator 3)
(defconst cmpl-source-cursor-moves 4)
(defconst cmpl-source-interactive 5)
(defconst cmpl-source-cdabbrev 6)
(defconst num-cmpl-sources 7)
(defvar current-completion-source cmpl-source-unknown)



;;---------------------------------------------------------------------------
;; Completion Method #2: dabbrev-expand style
;;---------------------------------------------------------------------------
;;
;;   This method is used if there are no useful stored completions.  It is
;; based on dabbrev-expand with these differences :
;;   1) Faster (we don't use regexps)
;;   2) case coercion handled correctly
;; This is called cdabbrev to differentiate it.
;;   We simply search backwards through the file looking for words which
;; start with the same letters we are trying to complete.
;;

(defvar cdabbrev-completions-tried nil)
;;  "A list of all the cdabbrev completions since the last reset.")

(defvar cdabbrev-current-point 0)
;;  "The current point position the cdabbrev search is at.")

(defvar cdabbrev-current-window nil)
;;  "The current window we are looking for cdabbrevs in.
;; Return t if looking in (other-buffer), nil if no more  cdabbrevs.")

(defvar cdabbrev-wrapped-p nil)
;;  "Return t if the cdabbrev search has wrapped around the file.")

(defvar cdabbrev-abbrev-string "")
(defvar cdabbrev-start-point 0)
(defvar cdabbrev-stop-point)

;; Test strings for cdabbrev
;; cdat-upcase   ;;same namestring
;; CDAT-UPCASE   ;;ok
;; cdat2         ;;too short
;; cdat-1-2-3-4  ;;ok
;; a-cdat-1      ;;doesn't start correctly
;; cdat-simple   ;;ok


(defun reset-cdabbrev (abbrev-string &optional initial-completions-tried)
  "Reset the cdabbrev search to search for ABBREV-STRING.
INITIAL-COMPLETIONS-TRIED is a list of downcased strings to ignore
during the search."
  (setq cdabbrev-abbrev-string abbrev-string
	cdabbrev-completions-tried
	(cons (downcase abbrev-string) initial-completions-tried))
  (reset-cdabbrev-window t))

(defun set-cdabbrev-buffer ()
  ;; cdabbrev-current-window must not be nil
  (set-buffer (if (eq cdabbrev-current-window t)
		  (other-buffer)
		  (window-buffer cdabbrev-current-window))))


(defun reset-cdabbrev-window (&optional initializep)
  "Reset the cdabbrev search to search for abbrev-string."
  ;; Set the window
  (cond (initializep
	 (setq cdabbrev-current-window (selected-window)))
	((eq cdabbrev-current-window t)
	 ;; Everything has failed
	 (setq cdabbrev-current-window nil))
	(cdabbrev-current-window
	 (setq cdabbrev-current-window (next-window cdabbrev-current-window))
	 (if (eq cdabbrev-current-window (selected-window))
	     ;; No more windows, try other buffer.
	     (setq cdabbrev-current-window t))))
  (if cdabbrev-current-window
      (save-excursion
	(set-cdabbrev-buffer)
	(setq cdabbrev-current-point (point)
	      cdabbrev-start-point cdabbrev-current-point
	      cdabbrev-stop-point
	      (if completion-search-distance
		  (max (point-min)
		       (- cdabbrev-start-point completion-search-distance))
		  (point-min))
	      cdabbrev-wrapped-p nil))))

(defun next-cdabbrev ()
  "Return the next possible cdabbrev expansion or nil if there isn't one.
`reset-cdabbrev' must've been called already.
This is sensitive to `case-fold-search'."
  ;; note that case-fold-search affects the behavior of this function
  ;; Bug: won't pick up an expansion that starts at the top of buffer
  (if cdabbrev-current-window
      (let (saved-point
	    saved-syntax
	    (expansion nil)
	    downcase-expansion tried-list syntax saved-point-2)
	(save-excursion
	  (unwind-protect
	      (progn
		;; Switch to current completion buffer
		(set-cdabbrev-buffer)
		;; Save current buffer state
		(setq saved-point  (point)
		      saved-syntax (syntax-table))
		;; Restore completion state
		(set-syntax-table completion-syntax-table)
		(goto-char cdabbrev-current-point)
		;; Loop looking for completions
		(while
		    ;; This code returns t if it should loop again
		    (cond
		      (;; search for the string
		       (search-backward cdabbrev-abbrev-string cdabbrev-stop-point t)
		       ;; return nil if the completion is valid
		       (not
			(and
			 ;; does it start with a separator char ?
			 (or (= (setq syntax (char-syntax (preceding-char))) ? )
			     (and (= syntax ?w)
				  ;; symbol char to ignore at end.  Are we at end ?
				  (progn
				    (setq saved-point-2 (point))
				    (forward-word -1)
				    (prog1
				      (= (char-syntax (preceding-char)) ? )
				      (goto-char saved-point-2)))))
			 ;; is the symbol long enough ?
			 (setq expansion (symbol-under-point))
			 ;; have we not tried this one before
			 (progn
			   ;; See if we've already used it
			   (setq tried-list cdabbrev-completions-tried
				 downcase-expansion (downcase expansion))
			   (while (and tried-list
				       (not (string-equal downcase-expansion
							  (car tried-list))))
			     ;; Already tried, don't choose this one
			     (setq tried-list (cdr tried-list)))
			   ;; at this point tried-list will be nil if this
			   ;; expansion has not yet been tried
			   (if tried-list
			       (setq expansion nil)
			       t)))))
		      ;; search failed
		      (cdabbrev-wrapped-p
		       ;; If already wrapped, then we've failed completely
		       nil)
		      (t
		       ;; need to wrap
		       (goto-char (setq cdabbrev-current-point
					(if completion-search-distance
					    (min (point-max) (+ cdabbrev-start-point completion-search-distance))
					    (point-max))))

		       (setq cdabbrev-wrapped-p t))))
		;; end of while loop
		(cond (expansion
		       ;; successful
		       (setq cdabbrev-completions-tried
			     (cons downcase-expansion cdabbrev-completions-tried)
			     cdabbrev-current-point (point)))))
	    (set-syntax-table saved-syntax)
	    (goto-char saved-point)))
	;; If no expansion, go to next window
	(cond (expansion)
	      (t (reset-cdabbrev-window)
		 (next-cdabbrev))))))

;; The following must be eval'd in the minibuffer ::
;; (reset-cdabbrev "cdat")
;; (next-cdabbrev)  --> "cdat-simple"
;; (next-cdabbrev)  --> "cdat-1-2-3-4"
;; (next-cdabbrev)  --> "CDAT-UPCASE"
;; (next-cdabbrev)  --> "cdat-wrapping"
;; (next-cdabbrev)  --> "cdat_start_sym"
;; (next-cdabbrev)  --> nil
;; (next-cdabbrev)  --> nil
;; (next-cdabbrev)  --> nil

;; _cdat_start_sym
;; cdat-wrapping


;;---------------------------------------------------------------------------
;; Completion Database
;;---------------------------------------------------------------------------

;; We use two storage modes for the two search types ::
;;  1) Prefix {cmpl-prefix-obarray} for looking up possible completions
;;      Used by search-completion-next
;;      the value of the symbol is nil or a cons of head and tail pointers
;;  2) Interning {cmpl-obarray} to see if it's in the database
;;      Used by find-exact-completion, completion-in-database-p
;;      The value of the symbol is the completion entry

;; bad things may happen if this length is changed due to the way
;; GNU implements obarrays
(defconst cmpl-obarray-length 511)

(defvar cmpl-prefix-obarray (make-vector cmpl-obarray-length 0)
  "An obarray used to store the downcased completion prefixes.
Each symbol is bound to a list of completion entries.")

(defvar cmpl-obarray (make-vector cmpl-obarray-length 0)
  "An obarray used to store the downcased completions.
Each symbol is bound to a single completion entry.")

;;-----------------------------------------------
;; Completion Entry Structure Definition
;;-----------------------------------------------

;; A completion entry is a LIST of string, prefix-symbol num-uses, and
;; last-use-time (the time the completion was last used)
;; last-use-time is t if the string should be kept permanently
;; num-uses is incremented every time the completion is used.

;; We chose lists because (car foo) is faster than (aref foo 0) and the
;; creation time is about the same.

;; READER MACROS

(defmacro completion-string (completion-entry)
  (list 'car completion-entry))

(defmacro completion-num-uses (completion-entry)
  ;;  "The number of times it has used.  Used to decide whether to save
  ;; it."
  (list 'car (list 'cdr completion-entry)))

(defmacro completion-last-use-time (completion-entry)
  ;;  "The time it was last used.  In hours since origin.  Used to decide
  ;; whether to save it.  t if one should always save it."
  (list 'nth 2 completion-entry))

(defmacro completion-source (completion-entry)
  (list 'nth 3 completion-entry))

;; WRITER MACROS
(defmacro set-completion-string (completion-entry string)
  (list 'setcar completion-entry string))

(defmacro set-completion-num-uses (completion-entry num-uses)
  (list 'setcar (list 'cdr completion-entry) num-uses))

(defmacro set-completion-last-use-time (completion-entry last-use-time)
  (list 'setcar (list 'cdr (list 'cdr completion-entry)) last-use-time))

;; CONSTRUCTOR
(defun make-completion (string)
  "Return a completion entry."
  (list string 0 nil current-completion-source))

;; Obsolete
;;(defmacro cmpl-prefix-entry-symbol (completion-entry)
;;  (list 'car (list 'cdr completion-entry)))



;;-----------------------------------------------
;; Prefix symbol entry definition
;;-----------------------------------------------
;; A cons of (head . tail)

;; READER Macros

(defalias 'cmpl-prefix-entry-head 'car)

(defalias 'cmpl-prefix-entry-tail 'cdr)

;; WRITER Macros

(defmacro set-cmpl-prefix-entry-head (prefix-entry new-head)
  (list 'setcar prefix-entry new-head))

(defmacro set-cmpl-prefix-entry-tail (prefix-entry new-tail)
  (list 'setcdr prefix-entry new-tail))

;; Constructor

(defun make-cmpl-prefix-entry (completion-entry-list)
  "Make a new prefix entry containing only completion-entry."
  (cons completion-entry-list completion-entry-list))

;;-----------------------------------------------
;; Completion Database - Utilities
;;-----------------------------------------------

(defun clear-all-completions ()
  "Initialize the completion storage.  All existing completions are lost."
  (interactive)
  (setq cmpl-prefix-obarray (make-vector cmpl-obarray-length 0))
  (setq cmpl-obarray (make-vector cmpl-obarray-length 0))
  (cmpl-statistics-block
    (record-clear-all-completions)))

(defvar completions-list-return-value)

(defun list-all-completions ()
  "Return a list of all the known completion entries."
  (let ((completions-list-return-value nil))
    (mapatoms 'list-all-completions-1 cmpl-prefix-obarray)
    completions-list-return-value))

(defun list-all-completions-1 (prefix-symbol)
  (if (boundp prefix-symbol)
      (setq completions-list-return-value
	    (append (cmpl-prefix-entry-head (symbol-value prefix-symbol))
		    completions-list-return-value))))

(defun list-all-completions-by-hash-bucket ()
  "Return list of lists of known completion entries, organized by hash bucket."
  (let ((completions-list-return-value nil))
    (mapatoms 'list-all-completions-by-hash-bucket-1 cmpl-prefix-obarray)
    completions-list-return-value))

(defun list-all-completions-by-hash-bucket-1 (prefix-symbol)
  (if (boundp prefix-symbol)
      (setq completions-list-return-value
	    (cons (cmpl-prefix-entry-head (symbol-value prefix-symbol))
		  completions-list-return-value))))


;;-----------------------------------------------
;; Updating the database
;;-----------------------------------------------
;;
;;   These are the internal functions used to update the datebase
;;
;;
(defvar completion-to-accept nil
  "Set to a string that is pending its acceptance.")
  ;; this checked by the top level reading functions

(defvar cmpl-db-downcase-string nil
  "Setup by `find-exact-completion', etc.  The given string, downcased.")
(defvar cmpl-db-symbol nil
  "The interned symbol corresponding to `cmpl-db-downcase-string'.
Set up by `cmpl-db-symbol'.")
(defvar cmpl-db-prefix-symbol nil
  "The interned prefix symbol corresponding to `cmpl-db-downcase-string'.")
(defvar cmpl-db-entry nil)
(defvar cmpl-db-debug-p nil
  "Set to t if you want to debug the database.")

;; READS
(defun find-exact-completion (string)
  "Return the completion entry for STRING or nil.
Sets up `cmpl-db-downcase-string' and `cmpl-db-symbol'."
  (and (boundp (setq cmpl-db-symbol
		     (intern (setq cmpl-db-downcase-string (downcase string))
			     cmpl-obarray)))
       (symbol-value cmpl-db-symbol)))

(defun find-cmpl-prefix-entry (prefix-string)
  "Return the prefix entry for string.
Sets `cmpl-db-prefix-symbol'.
Prefix-string must be exactly `completion-prefix-min-length' long
and downcased.  Sets up `cmpl-db-prefix-symbol'."
  (and (boundp (setq cmpl-db-prefix-symbol
		     (intern prefix-string cmpl-prefix-obarray)))
       (symbol-value cmpl-db-prefix-symbol)))

(defvar inside-locate-completion-entry nil)
;; used to trap lossage in silent error correction

(defun locate-completion-entry (completion-entry prefix-entry)
  "Locate the completion entry.
Returns a pointer to the element before the completion entry or nil if
the completion entry is at the head.
Must be called after `find-exact-completion'."
  (let ((prefix-list (cmpl-prefix-entry-head prefix-entry))
	 next-prefix-list)
    (cond
      ((not (eq (car prefix-list) completion-entry))
       ;; not already at head
       (while (and prefix-list
		   (not (eq completion-entry
			    (car (setq next-prefix-list (cdr prefix-list))))))
	 (setq prefix-list next-prefix-list))
       (cond (;; found
	      prefix-list)
	     ;; Didn't find it.  Database is messed up.
	     (cmpl-db-debug-p
	      ;; not found, error if debug mode
	      (error "Completion entry exists but not on prefix list - %s"
		     completion-string))
	     (inside-locate-completion-entry
	      ;; recursive error: really scrod
	      (locate-completion-db-error))
	     (t
	       ;; Patch out
	       (set cmpl-db-symbol nil)
	       ;; Retry
	       (locate-completion-entry-retry completion-entry)))))))

(defun locate-completion-entry-retry (old-entry)
  (let ((inside-locate-completion-entry t))
    (add-completion (completion-string old-entry)
		    (completion-num-uses old-entry)
		    (completion-last-use-time old-entry))
    (let* ((cmpl-entry (find-exact-completion (completion-string old-entry)))
	   (pref-entry
	    (if cmpl-entry
		(find-cmpl-prefix-entry
		  (substring cmpl-db-downcase-string
			     0 completion-prefix-min-length)))))
      (if (and cmpl-entry pref-entry)
	  ;; try again
	  (locate-completion-entry cmpl-entry pref-entry)
	  ;; still losing
	  (locate-completion-db-error)))))

(defun locate-completion-db-error ()
  ;; recursive error: really scrod
  (error "Completion database corrupted.  Try M-x clear-all-completions.  Send bug report"))

;; WRITES
(defun add-completion-to-tail-if-new (string)
  "If STRING is not in the database add it to appropriate prefix list.
STRING is added to the end of the appropriate prefix list with
num-uses = 0.  The database is unchanged if it is there.  STRING must be
longer than `completion-prefix-min-length'.
This must be very fast.
Returns the completion entry."
  (or (find-exact-completion string)
      ;; not there
      (let (;; create an entry
	    (entry (list (make-completion string)))
	    ;; setup the prefix
	    (prefix-entry (find-cmpl-prefix-entry
			    (substring cmpl-db-downcase-string 0
				       completion-prefix-min-length))))
	;; The next two forms should happen as a unit (atomically) but
	;; no fatal errors should result if that is not the case.
	(cond (prefix-entry
	       ;; These two should be atomic, but nothing fatal will happen
	       ;; if they're not.
	       (setcdr (cmpl-prefix-entry-tail prefix-entry) entry)
	       (set-cmpl-prefix-entry-tail prefix-entry entry))
	      (t
	       (set cmpl-db-prefix-symbol (make-cmpl-prefix-entry entry))))
	;; statistics
	(cmpl-statistics-block
	  (note-added-completion))
	;; set symbol
	(set cmpl-db-symbol (car entry)))))

(defun add-completion-to-head (completion-string)
  "If COMPLETION-STRING is not in the database, add it to prefix list.
We add COMPLETION-STRING to the head of the appropriate prefix list,
or to the head of the list.
COMPLETION-STRING must be longer than `completion-prefix-min-length'.
Updates the saved string with the supplied string.
This must be very fast.
Returns the completion entry."
  ;; Handle pending acceptance
  (if completion-to-accept (accept-completion))
  ;; test if already in database
  (if (setq cmpl-db-entry (find-exact-completion completion-string))
      ;; found
      (let* ((prefix-entry (find-cmpl-prefix-entry
			     (substring cmpl-db-downcase-string 0
					completion-prefix-min-length)))
	     (splice-ptr (locate-completion-entry cmpl-db-entry prefix-entry))
	     (cmpl-ptr (cdr splice-ptr)))
	;; update entry
	(set-completion-string cmpl-db-entry completion-string)
	;; move to head (if necessary)
	(cond (splice-ptr
	       ;; These should all execute atomically but it is not fatal if
	       ;; they don't.
	       ;; splice it out
	       (or (setcdr splice-ptr (cdr cmpl-ptr))
		   ;; fix up tail if necessary
		   (set-cmpl-prefix-entry-tail prefix-entry splice-ptr))
	       ;; splice in at head
	       (setcdr cmpl-ptr (cmpl-prefix-entry-head prefix-entry))
	       (set-cmpl-prefix-entry-head prefix-entry cmpl-ptr)))
	cmpl-db-entry)
    ;; not there
    (let (;; create an entry
	  (entry (list (make-completion completion-string)))
	  ;; setup the prefix
	  (prefix-entry (find-cmpl-prefix-entry
			  (substring cmpl-db-downcase-string 0
				     completion-prefix-min-length))))
      (cond (prefix-entry
	     ;; Splice in at head
	     (setcdr entry (cmpl-prefix-entry-head prefix-entry))
	     (set-cmpl-prefix-entry-head prefix-entry entry))
	    (t
	     ;; Start new prefix entry
	     (set cmpl-db-prefix-symbol (make-cmpl-prefix-entry entry))))
      ;; statistics
      (cmpl-statistics-block
	(note-added-completion))
      ;; Add it to the symbol
      (set cmpl-db-symbol (car entry)))))

(defun delete-completion (completion-string)
  "Delete the completion from the database.
String must be longer than `completion-prefix-min-length'."
  ;; Handle pending acceptance
  (if completion-to-accept (accept-completion))
  (if (setq cmpl-db-entry (find-exact-completion completion-string))
      ;; found
      (let* ((prefix-entry (find-cmpl-prefix-entry
			     (substring cmpl-db-downcase-string 0
					completion-prefix-min-length)))
	     (splice-ptr (locate-completion-entry cmpl-db-entry prefix-entry)))
	 ;; delete symbol reference
	 (set cmpl-db-symbol nil)
	 ;; remove from prefix list
	 (cond (splice-ptr
		;; not at head
		(or (setcdr splice-ptr (cdr (cdr splice-ptr)))
		    ;; fix up tail if necessary
		    (set-cmpl-prefix-entry-tail prefix-entry splice-ptr)))
	       (t
		;; at head
		(or (set-cmpl-prefix-entry-head
		      prefix-entry (cdr (cmpl-prefix-entry-head prefix-entry)))
		    ;; List is now empty
		    (set cmpl-db-prefix-symbol nil))))
	 (cmpl-statistics-block
	   (note-completion-deleted)))
      (error "Unknown completion `%s'" completion-string)))

;; Tests --
;;  - Add and Find -
;; (add-completion-to-head "banana")     --> ("banana" 0 nil 0)
;; (find-exact-completion "banana")      --> ("banana" 0 nil 0)
;; (find-exact-completion "bana")        --> nil
;; (car (find-cmpl-prefix-entry "ban"))  --> (("banana" ...))
;; (cdr (find-cmpl-prefix-entry "ban"))  --> (("banana" ...))
;; (add-completion-to-head "banish")     --> ("banish" 0 nil 0)
;; (find-exact-completion "banish")      --> ("banish" 0 nil 0)
;; (car (find-cmpl-prefix-entry "ban"))  --> (("banish" ...) ("banana" ...))
;; (cdr (find-cmpl-prefix-entry "ban"))  --> (("banana" ...))
;; (add-completion-to-head "banana")     --> ("banana" 0 nil 0)
;; (car (find-cmpl-prefix-entry "ban"))  --> (("banana" ...) ("banish" ...))
;; (cdr (find-cmpl-prefix-entry "ban"))  --> (("banish" ...))
;;
;;  - Deleting -
;; (add-completion-to-head "banner")     --> ("banner" 0 nil 0)
;; (delete-completion "banner")
;; (find-exact-completion "banner")      --> nil
;; (car (find-cmpl-prefix-entry "ban"))  --> (("banana" ...) ("banish" ...))
;; (cdr (find-cmpl-prefix-entry "ban"))  --> (("banish" ...))
;; (add-completion-to-head "banner")     --> ("banner" 0 nil 0)
;; (delete-completion "banana")
;; (car (find-cmpl-prefix-entry "ban"))  --> (("banner" ...) ("banish" ...))
;; (cdr (find-cmpl-prefix-entry "ban"))  --> (("banish" ...))
;; (delete-completion "banner")
;; (delete-completion "banish")
;; (find-cmpl-prefix-entry "ban")        --> nil
;; (delete-completion "banner")          --> error
;;
;; - Tail -
;; (add-completion-to-tail-if-new "banana") --> ("banana" 0 nil 0)
;; (car (find-cmpl-prefix-entry "ban"))     --> (("banana" ...))
;; (cdr (find-cmpl-prefix-entry "ban"))     --> (("banana" ...))
;; (add-completion-to-tail-if-new "banish") --> ("banish" 0 nil 0)
;; (car (find-cmpl-prefix-entry "ban"))     -->(("banana" ...) ("banish" ...))
;; (cdr (find-cmpl-prefix-entry "ban"))     -->(("banish" ...))
;;


;;---------------------------------------------------------------------------
;; Database Update :: Interface level routines
;;---------------------------------------------------------------------------
;;
;; These lie on top of the database ref. functions but below the standard
;; user interface level


(defun interactive-completion-string-reader (prompt)
  (let* ((default (symbol-under-or-before-point))
	 (new-prompt
	  (if default
	      (format "%s (default %s): " prompt default)
	      (format "%s: " prompt)))
	 (read (completing-read new-prompt cmpl-obarray)))
    (if (zerop (length read)) (setq read (or default "")))
    (list read)))

(defun check-completion-length (string)
  (if (< (length string) completion-min-length)
      (error "The string `%s' is too short to be saved as a completion"
	     string)
      (list string)))

(defun add-completion (string &optional num-uses last-use-time)
  "Add STRING to completion list, or move it to head of list.
The completion is altered appropriately if NUM-USES and/or LAST-USE-TIME
are specified."
  (interactive (interactive-completion-string-reader "Completion to add"))
  (check-completion-length string)
  (let* ((current-completion-source (if (called-interactively-p 'interactive)
					cmpl-source-interactive
					current-completion-source))
	 (entry (add-completion-to-head string)))

    (if num-uses (set-completion-num-uses entry num-uses))
    (if last-use-time
	(set-completion-last-use-time entry last-use-time))))

(defun add-permanent-completion (string)
  "Add STRING if it isn't already listed, and mark it permanent."
  (interactive
    (interactive-completion-string-reader "Completion to add permanently"))
  (let ((current-completion-source (if (called-interactively-p 'interactive)
				       cmpl-source-interactive
				       current-completion-source)))
    (add-completion string nil t)))

(defun kill-completion (string)
  (interactive (interactive-completion-string-reader "Completion to kill"))
  (check-completion-length string)
  (delete-completion string))

(defun accept-completion ()
  "Accepts the pending completion in `completion-to-accept'.
This bumps num-uses.  Called by `add-completion-to-head' and
`completion-search-reset'."
  (let ((string completion-to-accept)
	;; if this is added afresh here, then it must be a cdabbrev
	(current-completion-source cmpl-source-cdabbrev)
	entry)
    (setq completion-to-accept nil)
    (setq entry (add-completion-to-head string))
    (set-completion-num-uses entry (1+ (completion-num-uses entry)))
    (setq cmpl-completions-accepted-p t)))

(defun use-completion-under-point ()
  "Add the completion symbol underneath the point into the completion buffer."
  (let ((string (and enable-completion (symbol-under-point)))
	(current-completion-source cmpl-source-cursor-moves))
    (if string (add-completion-to-head string))))

(defun use-completion-before-point ()
  "Add the completion symbol before point into the completion buffer."
  (let ((string (and enable-completion (symbol-before-point)))
	(current-completion-source cmpl-source-cursor-moves))
    (if string (add-completion-to-head string))))

(defun use-completion-under-or-before-point ()
  "Add the completion symbol before point into the completion buffer."
  (let ((string (and enable-completion (symbol-under-or-before-point)))
	(current-completion-source cmpl-source-cursor-moves))
    (if string (add-completion-to-head string))))

(defun use-completion-before-separator ()
  "Add the completion symbol before point into the completion buffer.
Completions added this way will automatically be saved if
`completion-on-separator-character' is non-nil."
  (let ((string (and enable-completion (symbol-before-point)))
	(current-completion-source cmpl-source-separator)
	entry)
    (cmpl-statistics-block
      (note-separator-character string))
    (cond (string
	   (setq entry (add-completion-to-head string))
	   (if (and completion-on-separator-character
		      (zerop (completion-num-uses entry)))
	       (progn
		 (set-completion-num-uses entry 1)
		 (setq cmpl-completions-accepted-p t)))))))

;; Tests --
;;  - Add and Find -
;; (add-completion "banana" 5 10)
;; (find-exact-completion "banana")  --> ("banana" 5 10 0)
;; (add-completion "banana" 6)
;; (find-exact-completion "banana")  --> ("banana" 6 10 0)
;; (add-completion "banish")
;; (car (find-cmpl-prefix-entry "ban"))  --> (("banish" ...) ("banana" ...))
;;
;;  - Accepting -
;; (setq completion-to-accept "banana")
;; (accept-completion)
;; (find-exact-completion "banana")      --> ("banana" 7 10)
;; (car (find-cmpl-prefix-entry "ban"))  --> (("banana" ...) ("banish" ...))
;; (setq completion-to-accept "banish")
;; (add-completion "banner")
;; (car (find-cmpl-prefix-entry "ban"))
;;        --> (("banner" ...) ("banish" 1 ...) ("banana" 7 ...))
;;
;;  - Deleting -
;; (kill-completion "banish")
;; (car (find-cmpl-prefix-entry "ban"))  --> (("banner" ...) ("banana" ...))


;;---------------------------------------------------------------------------
;; Searching the database
;;---------------------------------------------------------------------------
;; Functions outside this block must call completion-search-reset followed
;; by calls to completion-search-next or completion-search-peek
;;

;; Status variables
;; Commented out to improve loading speed
(defvar cmpl-test-string "")
;;  "The current string used by completion-search-next."
(defvar cmpl-test-regexp "")
;;  "The current regexp used by completion-search-next.
;;   (derived from cmpl-test-string)"
(defvar cmpl-last-index 0)
;;  "The last index that completion-search-next was called with."
(defvar cmpl-cdabbrev-reset-p nil)
;;  "Set to t when cdabbrevs have been reset."
(defvar cmpl-next-possibilities nil)
;;   "A pointer to the element BEFORE the next set of possible completions.
;;  cadr of this is the cmpl-next-possibility"
(defvar cmpl-starting-possibilities nil)
;;  "The initial list of starting possibilities."
(defvar cmpl-next-possibility nil)
;;   "The cached next possibility."
(defvar cmpl-tried-list nil)
;;   "A downcased list of all the completions we have tried."


(defun completion-search-reset (string)
  "Set up the for completion searching for STRING.
STRING must be longer than `completion-prefix-min-length'."
  (if completion-to-accept (accept-completion))
  (setq cmpl-starting-possibilities
	(cmpl-prefix-entry-head
	  (find-cmpl-prefix-entry
	   (downcase (substring string 0 completion-prefix-min-length))))
	cmpl-test-string string
	cmpl-test-regexp (concat (regexp-quote string) "."))
  (completion-search-reset-1))

(defun completion-search-reset-1 ()
  (setq cmpl-next-possibilities cmpl-starting-possibilities
	cmpl-next-possibility   nil
	cmpl-cdabbrev-reset-p nil
	cmpl-last-index -1
	cmpl-tried-list nil))

(defun completion-search-next (index)
  "Return the next completion entry.
If INDEX is out of sequence, reset and start from the top.
If there are no more entries, try cdabbrev and return only a string."
  (cond
    ((= index (setq cmpl-last-index (1+ cmpl-last-index)))
     (completion-search-peek t))
    ((< index 0)
     (completion-search-reset-1)
     (setq cmpl-last-index index)
     ;; reverse the possibilities list
     (setq cmpl-next-possibilities (reverse cmpl-starting-possibilities))
     ;; do a "normal" search
     (while (and (completion-search-peek nil)
		 (< (setq index (1+ index)) 0))
       (setq cmpl-next-possibility nil))
     (cond ((not cmpl-next-possibilities))
	    ;; If no more possibilities, leave it that way
	   ((= -1 cmpl-last-index)
	    ;; next completion is at index 0.  reset next-possibility list
	    ;; to start at beginning
	    (setq cmpl-next-possibilities cmpl-starting-possibilities))
	   (t
	    ;; otherwise point to one before current
	    (setq cmpl-next-possibilities
		  (nthcdr (- (length cmpl-starting-possibilities)
			     (length cmpl-next-possibilities))
			  cmpl-starting-possibilities)))))
    (t
     ;; non-negative index, reset and search
     ;;(prin1 'reset)
     (completion-search-reset-1)
     (setq cmpl-last-index index)
     (while (and (completion-search-peek t)
		 (not (< (setq index (1- index)) 0)))
       (setq cmpl-next-possibility nil))))
  (prog1
      cmpl-next-possibility
    (setq cmpl-next-possibility nil)))


(defun completion-search-peek (use-cdabbrev)
  "Return the next completion entry without actually moving the pointers.
Calling this again or calling `completion-search-next' results in the same
string being returned.  Depends on `case-fold-search'.
If there are no more entries, try cdabbrev and then return only a string."
  (cond
    ;; return the cached value if we have it
    (cmpl-next-possibility)
    ((and cmpl-next-possibilities
	  ;; still a few possibilities left
	  (progn
	    (while
		(and (not (eq 0 (string-match cmpl-test-regexp
					      (completion-string (car cmpl-next-possibilities)))))
		     (setq cmpl-next-possibilities (cdr cmpl-next-possibilities))))
	    cmpl-next-possibilities))
     ;; successful match
     (setq cmpl-next-possibility (car cmpl-next-possibilities)
	   cmpl-tried-list (cons (downcase (completion-string cmpl-next-possibility))
				 cmpl-tried-list)
	   cmpl-next-possibilities (cdr cmpl-next-possibilities))
     cmpl-next-possibility)
    (use-cdabbrev
     ;; unsuccessful, use cdabbrev
     (cond ((not cmpl-cdabbrev-reset-p)
	    (reset-cdabbrev cmpl-test-string cmpl-tried-list)
	    (setq cmpl-cdabbrev-reset-p t)))
     (setq cmpl-next-possibility (next-cdabbrev)))
    ;; Completely unsuccessful, return nil
    ))

;; Tests --
;;  - Add and Find -
;; (add-completion "banana")
;; (completion-search-reset "ban")
;; (completion-search-next 0)        --> "banana"
;;
;;  - Discrimination -
;; (add-completion "cumberland")
;; (add-completion "cumberbund")
;; cumbering
;; (completion-search-reset "cumb")
;; (completion-search-peek t)        --> "cumberbund"
;; (completion-search-next 0)        --> "cumberbund"
;; (completion-search-peek t)        --> "cumberland"
;; (completion-search-next 1)        --> "cumberland"
;; (completion-search-peek nil)      --> nil
;; (completion-search-next 2)        --> "cumbering"  {cdabbrev}
;; (completion-search-next 3)        -->  nil or "cumming"{depends on context}
;; (completion-search-next 1)        --> "cumberland"
;; (completion-search-peek t)        --> "cumbering"  {cdabbrev}
;;
;;  - Accepting -
;; (completion-search-next 1)        --> "cumberland"
;; (setq completion-to-accept "cumberland")
;; (completion-search-reset "foo")
;; (completion-search-reset "cum")
;; (completion-search-next 0)        --> "cumberland"
;;
;;  - Deleting -
;; (kill-completion "cumberland")
;; cummings
;; (completion-search-reset "cum")
;; (completion-search-next 0)        --> "cumberbund"
;; (completion-search-next 1)        --> "cummings"
;;
;;  - Ignoring Capitalization -
;; (completion-search-reset "CuMb")
;; (completion-search-next 0)            --> "cumberbund"



;;-----------------------------------------------
;; COMPLETE
;;-----------------------------------------------

(defun completion-mode ()
  "Toggle whether or not to add new words to the completion database."
  (interactive)
  (setq enable-completion (not enable-completion))
  (message "Completion mode is now %s." (if enable-completion "ON" "OFF")))

(defvar cmpl-current-index 0)
(defvar cmpl-original-string nil)
(defvar cmpl-last-insert-location -1)
(defvar cmpl-leave-point-at-start nil)

(defun complete (&optional arg)
  "Fill out a completion of the word before point.
Point is left at end.  Consecutive calls rotate through all possibilities.
Prefix args ::
  control-u :: leave the point at the beginning of the completion rather
               than at the end.
  a number  :: rotate through the possible completions by that amount
  `-'       :: same as -1 (insert previous completion)
 {See the comments at the top of `completion.el' for more info.}"
  (interactive "*p")
  ;;; Set up variables
  (cond ((eq last-command this-command)
	 ;; Undo last one
	 (delete-region cmpl-last-insert-location (point))
	 ;; get next completion
	 (setq cmpl-current-index (+ cmpl-current-index (or arg 1))))
	(t
	 (if (not cmpl-initialized-p)
	     (completion-initialize)) ;; make sure everything's loaded
	 (cond ((consp current-prefix-arg) ;; control-u
		(setq arg 0)
		(setq cmpl-leave-point-at-start t))
	       (t
		(setq cmpl-leave-point-at-start nil)))
	 ;; get string
	 (setq cmpl-original-string (symbol-before-point-for-complete))
	 (cond ((not cmpl-original-string)
		(setq this-command 'failed-complete)
		(error "To complete, point must be after a symbol at least %d character long"
		       completion-prefix-min-length)))
	 ;; get index
	 (setq cmpl-current-index (if current-prefix-arg arg 0))
	 ;; statistics
	 (cmpl-statistics-block
	   (note-complete-entered-afresh cmpl-original-string))
	 ;; reset database
	 (completion-search-reset cmpl-original-string)
	 ;; erase what we've got
	 (delete-region cmpl-symbol-start cmpl-symbol-end)))

  ;; point is at the point to insert the new symbol
  ;; Get the next completion
  (let* ((print-status-p
	  (and (>= baud-rate completion-prompt-speed-threshold)
	       (not (window-minibuffer-p (selected-window)))))
	 (insert-point (point))
	 (entry (completion-search-next cmpl-current-index))
	 string)
    ;; entry is either a completion entry or a string (if cdabbrev)

    ;; If found, insert
    (cond (entry
	   ;; Setup for proper case
	   (setq string (if (stringp entry)
			    entry (completion-string entry)))
	   (setq string (cmpl-merge-string-cases
			  string cmpl-original-string))
	   ;; insert
	   (insert string)
	   ;; accept it
	   (setq completion-to-accept string)
	   ;; fixup and cache point
	   (cond (cmpl-leave-point-at-start
		  (setq cmpl-last-insert-location (point))
		  (goto-char insert-point))
		 (t;; point at end,
		  (setq cmpl-last-insert-location insert-point)))
	   ;; statistics
	   (cmpl-statistics-block
	     (note-complete-inserted entry cmpl-current-index))
	   ;; Done ! cmpl-stat-complete-successful
	   ;;display the next completion
	   (cond
	     ((and print-status-p
		   ;; This updates the display and only prints if there
		   ;; is no typeahead
		   (sit-for 0)
		   (setq entry
			 (completion-search-peek
			   completion-cdabbrev-prompt-flag)))
	      (setq string (if (stringp entry)
			       entry (completion-string entry)))
	      (setq string (cmpl-merge-string-cases
			     string cmpl-original-string))
	      (message "Next completion: %s" string))))
	  (t;; none found, insert old
	   (insert cmpl-original-string)
	   ;; Don't accept completions
	   (setq completion-to-accept nil)
	   ;; print message
	   ;; This used to call cmpl19-sit-for, an undefined function.
	   ;; I hope that sit-for does the right thing; I don't know -- rms.
	   (if (and print-status-p (sit-for 0))
	       (message "No %scompletions."
			(if (eq this-command last-command) "more " "")))
	   ;; statistics
	   (cmpl-statistics-block
	     (record-complete-failed cmpl-current-index))
	   ;; Pretend that we were never here
	   (setq this-command 'failed-complete)))))

;;---------------------------------------------------------------------------
;; Parsing definitions from files into the database
;;---------------------------------------------------------------------------

;;-----------------------------------------------
;; Top Level functions ::
;;-----------------------------------------------

;; User interface
(defun add-completions-from-file (file)
  "Parse possible completions from a FILE and add them to database."
  (interactive "fFile: ")
  (setq file (expand-file-name file))
  (let* ((buffer (get-file-buffer file))
	 (buffer-already-there-p buffer))
    (if (not buffer-already-there-p)
	(let ((completions-merging-modes nil))
	  (setq buffer (find-file-noselect file))))
    (unwind-protect
        (with-current-buffer buffer
          (add-completions-from-buffer))
      (if (not buffer-already-there-p)
	  (kill-buffer buffer)))))

(defun add-completions-from-buffer ()
  (interactive)
  (let ((current-completion-source cmpl-source-file-parsing)
	(start-num
	 (cmpl-statistics-block
	  (aref completion-add-count-vector cmpl-source-file-parsing)))
	mode)
    (cond ((memq major-mode '(emacs-lisp-mode lisp-mode))
	   (add-completions-from-lisp-buffer)
	   (setq mode 'lisp))
	  ((memq major-mode '(c-mode))
	   (add-completions-from-c-buffer)
	   (setq mode 'c))
	  (t
	   (error "Cannot parse completions in %s buffers"
		  major-mode)))
    (cmpl-statistics-block
      (record-cmpl-parse-file
	mode (point-max)
	(- (aref completion-add-count-vector cmpl-source-file-parsing)
	   start-num)))))

;; Find file hook
(defun completion-find-file-hook ()
  (cond (enable-completion
	 (cond ((and (memq major-mode '(emacs-lisp-mode lisp-mode))
		     (memq 'lisp completions-merging-modes))
		(add-completions-from-buffer))
	       ((and (memq major-mode '(c-mode))
		     (memq 'c completions-merging-modes))
		(add-completions-from-buffer))))))

;;-----------------------------------------------
;; Tags Table Completions
;;-----------------------------------------------

(defun add-completions-from-tags-table ()
  ;; Inspired by eero@media-lab.media.mit.edu
  "Add completions from the current tags table."
  (interactive)
  (visit-tags-table-buffer)		;this will prompt if no tags-table
  (save-excursion
    (goto-char (point-min))
    (let (string)
      (condition-case nil
	   (while t
	     (search-forward "\177")
	     (backward-char 3)
	     (and (setq string (symbol-under-point))
		  (add-completion-to-tail-if-new string))
	     (forward-char 3))
	 (search-failed)))))


;;-----------------------------------------------
;; Lisp File completion parsing
;;-----------------------------------------------
;;   This merely looks for phrases beginning with (def.... or
;; (package:def ... and takes the next word.
;;
;; We tried using forward-lines and explicit searches but the regexp technique
;; was faster.  (About 100K characters per second)
;;
(defconst *lisp-def-regexp*
  "\n(\\(\\w*:\\)?def\\(\\w\\|\\s_\\)*\\s +(*"
  "A regexp that searches for Lisp definition form.")

;; Tests -
;;  (and (string-match *lisp-def-regexp* "\n(defun foo") (match-end 0)) -> 8
;;  (and (string-match *lisp-def-regexp* "\n(si:def foo") (match-end 0)) -> 9
;;  (and (string-match *lisp-def-regexp* "\n(def-bar foo")(match-end 0)) -> 10
;;  (and (string-match *lisp-def-regexp* "\n(defun (foo") (match-end 0)) -> 9

;; Parses all the definition names from a Lisp mode buffer and adds them to
;; the completion database.
(defun add-completions-from-lisp-buffer ()
  ;;; Benchmarks
  ;;;  Sun-3/280 - 1500 to 3000 lines of lisp code per second
  (let (string)
    (save-excursion
      (goto-char (point-min))
      (condition-case nil
	   (while t
	     (re-search-forward *lisp-def-regexp*)
	     (and (setq string (symbol-under-point))
		  (add-completion-to-tail-if-new string)))
	 (search-failed)))))


;;-----------------------------------------------
;; C file completion parsing
;;-----------------------------------------------
;; C :
;;  Looks for #define or [<storage class>] [<type>] <name>{,<name>}
;; or structure, array or pointer defs.
;; It gets most of the definition names.
;;
;; As you might suspect by now, we use some symbol table hackery
;;
;; Symbol separator chars (have whitespace syntax) --> , ; * = (
;; Opening char --> [ {
;; Closing char --> ] }
;; opening and closing must be skipped over
;; Whitespace chars (have symbol syntax)
;; Everything else has word syntax

(defconst completion-c-def-syntax-table
  (let ((table (make-syntax-table))
	(whitespace-chars '(?  ?\n ?\t ?\f  ?\v ?\r))
	;; unfortunately the ?( causes the parens to appear unbalanced
	(separator-chars '(?, ?* ?= ?\( ?\;))
	i)
    ;; default syntax is whitespace
    (setq i 0)
    (while (< i 256)
      (modify-syntax-entry i "w" table)
      (setq i (1+ i)))
    (dolist (char whitespace-chars)
      (modify-syntax-entry char "_" table))
    (dolist (char separator-chars)
      (modify-syntax-entry char " " table))
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\} "){" table)
    table))

;; Regexps
(defconst *c-def-regexp*
    ;; This stops on lines with possible definitions
    "\n[_a-zA-Z#]"
  ;; This stops after the symbol to add.
  ;;"\n\\(#define\\s +.\\|\\(\\(\\w\\|\\s_\\)+\\b\\s *\\)+[(;,[*{=]\\)"
  ;; This stops before the symbol to add.  {Test cases in parens. below}
  ;;"\n\\(\\(\\w\\|\\s_\\)+\\s *(\\|\\(\\(#define\\|auto\\|extern\\|register\\|static\\|int\\|long\\|short\\|unsigned\\|char\\|void\\|float\\|double\\|enum\\|struct\\|union\\|typedef\\)\\s +\\)+\\)"
  ;; this simple version picks up too much extraneous stuff
  ;; "\n\\(\\w\\|\\s_\\|#\\)\\B"
  "A regexp that searches for a definition form.")
;
;(defconst *c-cont-regexp*
;  "\\(\\w\\|\\s_\\)+\\b\\s *\\({\\|\\(\\[[0-9\t ]*\\]\\s *\\)*,\\(*\\|\\s \\)*\\b\\)"
;  "This regexp should be used in a looking-at to parse for lists of variables.")
;
;(defconst *c-struct-regexp*
;  "\\(*\\|\\s \\)*\\b"
;  "This regexp should be used to test whether a symbol follows a structure definition.")

;(defun test-c-def-regexp (regexp string)
;  (and (eq 0 (string-match regexp string)) (match-end 0))
;  )

;; Tests -
;;  (test-c-def-regexp *c-def-regexp* "\n#define foo") -> 10 (9)
;;  (test-c-def-regexp *c-def-regexp* "\nfoo (x, y) {") -> 6 (6)
;;  (test-c-def-regexp *c-def-regexp* "\nint foo (x, y)") -> 10 (5)
;;  (test-c-def-regexp *c-def-regexp* "\n int foo (x, y)") -> nil
;;  (test-c-def-regexp *c-cont-regexp* "oo, bar") -> 4
;;  (test-c-def-regexp *c-cont-regexp* "oo, *bar") -> 5
;;  (test-c-def-regexp *c-cont-regexp* "a [5][6], bar") -> 10
;;  (test-c-def-regexp *c-cont-regexp* "oo(x,y)") -> nil
;;  (test-c-def-regexp *c-cont-regexp* "a [6] ,\t bar") -> 9
;;  (test-c-def-regexp *c-cont-regexp* "oo {trout =1} my_carp;") -> 14
;;  (test-c-def-regexp *c-cont-regexp* "truct_p complex foon") -> nil

;; Parses all the definition names from a C mode buffer and adds them to the
;; completion database.
(defun add-completions-from-c-buffer ()
  ;; Benchmark --
  ;;  Sun 3/280-- 1250 lines/sec.

  (let (string next-point char)
    (save-excursion
      (goto-char (point-min))
      (catch 'finish-add-completions
        (with-syntax-table completion-c-def-syntax-table
          (while t
            ;; we loop here only when scan-sexps fails
            ;; (i.e. unbalance exps.)
            (condition-case e
                (while t
                  (re-search-forward *c-def-regexp*)
                  (cond
                   ((= (preceding-char) ?#)
                    ;; preprocessor macro, see if it's one we handle
                    (cond ((looking-at "\\(define\\|ifdef\\)\\>")
                           ;; skip forward over definition symbol
                           ;; and add it to database
                           (and (forward-word 2)
                                (setq string (symbol-before-point))
                                ;;(push string foo)
                                (add-completion-to-tail-if-new string)))))
                   (t
                    ;; C definition
                    (setq next-point (point))
                    (while (and
                            next-point
                            ;; scan to next separator char.
                            (setq next-point (scan-sexps next-point 1)))
                      ;; position the point on the word we want to add
                      (goto-char next-point)
                      (while (= (setq char (following-char)) ?*)
                        ;; handle pointer ref
                        ;; move to next separator char.
                        (goto-char
                         (setq next-point (scan-sexps (point) 1))))
                      (forward-word -1)
                      ;; add to database
                      (if (setq string (symbol-under-point))
                          ;; (push string foo)
                          (add-completion-to-tail-if-new string)
                        ;; Local TMC hack (useful for parsing paris.h)
                        (if (and (looking-at "_AP") ;; "ansi prototype"
                                 (progn
                                   (forward-word -1)
                                   (setq string
                                         (symbol-under-point))))
                            (add-completion-to-tail-if-new string)))
                      ;; go to next
                      (goto-char next-point)
                      ;; (push (format "%c" (following-char)) foo)
                      (if (= (char-syntax char) ?\()
                          ;; if on an opening delimiter, go to end
                          (while (= (char-syntax char) ?\()
                            (setq next-point (scan-sexps next-point 1)
                                  char (char-after next-point)))
                        (or (= char ?,)
                            ;; Current char is an end char.
                            (setq next-point nil)))))))
              (search-failed ;;done
               (throw 'finish-add-completions t))
              (error
               ;; Check for failure in scan-sexps
               (if (member (nth 1 e)
                           '("Containing expression ends prematurely"
                             "Unbalanced parentheses"))
                   ;; unbalanced paren., keep going
                   ;;(ding)
                   (forward-line 1)
                 (message "Error parsing C buffer for completions--please send bug report")
                 (throw 'finish-add-completions t))))))))))


;;---------------------------------------------------------------------------
;; Init files
;;---------------------------------------------------------------------------

;; The version of save-completions-to-file called at kill-emacs time.
(defun kill-emacs-save-completions ()
  (if (and save-completions-flag enable-completion cmpl-initialized-p)
      (cond
	((not cmpl-completions-accepted-p)
	 (message "Completions database has not changed - not writing."))
	(t
	 (save-completions-to-file))))
  (cmpl-statistics-block (record-cmpl-kill-emacs)))

;; There is no point bothering to change this again
;; unless the package changes so much that it matters
;; for people that have saved completions.
(defconst completion-version "11")

(defconst saved-cmpl-file-header
    ";;; Completion Initialization file.
;; Version = %s
;; Format is (<string> . <last-use-time>)
;;  <string> is the completion
;;  <last-use-time> is the time the completion was last used
;;    If it is t, the completion will never be pruned from the file.
;;    Otherwise it is in hours since origin.
\n")

(defun completion-backup-filename (filename)
  (concat filename ".BAK"))

(defun save-completions-to-file (&optional filename)
  "Save completions in init file FILENAME.
If file name is not specified, use `save-completions-file-name'."
  (interactive)
  (setq filename (expand-file-name (or filename save-completions-file-name)))
  (if (file-writable-p filename)
      (progn
	(if (not cmpl-initialized-p)
	    (completion-initialize)) ;; make sure everything's loaded
	(message "Saving completions to file %s" filename)

	(let* ((delete-old-versions t)
	       (kept-old-versions 0)
	       (kept-new-versions completions-file-versions-kept)
	       last-use-time
	       (current-time (cmpl-hours-since-origin))
	       (total-in-db 0)
	       (total-perm 0)
	       (total-saved 0)
	       (backup-filename (completion-backup-filename filename)))

	  (with-current-buffer (get-buffer-create " *completion-save-buffer*")
	    (setq buffer-file-name filename)

	    (if (not (verify-visited-file-modtime (current-buffer)))
		(progn
		  ;; file has changed on disk.  Bring us up-to-date
		  (message "Completion file has changed.  Merging. . .")
		  (load-completions-from-file filename t)
		  (message "Merging finished.  Saving completions to file %s" filename)))

	    ;; prepare the buffer to be modified
	    (clear-visited-file-modtime)
	    (erase-buffer)
	    ;; (/ 1 0)
	    (insert (format saved-cmpl-file-header completion-version))
	    (dolist (completion (list-all-completions))
	      (setq total-in-db (1+ total-in-db))
	      (setq last-use-time (completion-last-use-time completion))
	      ;; Update num uses and maybe write completion to a file
	      (cond ((or;; Write to file if
		      ;; permanent
		      (and (eq last-use-time t)
			   (setq total-perm (1+ total-perm)))
		      ;; or if
		      (if (> (completion-num-uses completion) 0)
			  ;; it's been used
			  (setq last-use-time current-time)
			;; or it was saved before and
			(and last-use-time
			     ;; save-completions-retention-time is nil
			     (or (not save-completions-retention-time)
				 ;; or time since last use is < ...retention-time*
				 (< (- current-time last-use-time)
				    save-completions-retention-time)))))
		     ;; write to file
		     (setq total-saved (1+ total-saved))
		     (insert (prin1-to-string (cons (completion-string completion)
						    last-use-time)) "\n"))))

	    ;; write the buffer
	    (condition-case nil
		(let ((file-exists-p (file-exists-p filename)))
		  (if file-exists-p
		      (progn
			;; If file exists . . .
			;; Save a backup(so GNU doesn't screw us when we're out of disk)
			;; (GNU leaves a 0 length file if it gets a disk full error!)

			;; If backup doesn't exit, Rename current to backup
			;;  {If backup exists the primary file is probably messed up}
			(or (file-exists-p backup-filename)
			    (rename-file filename backup-filename))
			;; Copy the backup back to the current name
			;; (so versioning works)
			(copy-file backup-filename filename t)))
		  ;; Save it
		  (save-buffer)
		  (if file-exists-p
		      ;; If successful, remove backup
		      (delete-file backup-filename)))
	      (error
	       (set-buffer-modified-p nil)
	       (message "Couldn't save completion file `%s'" filename)))
	    ;; Reset accepted-p flag
	    (setq cmpl-completions-accepted-p nil) )
	  (cmpl-statistics-block
	   (record-save-completions total-in-db total-perm total-saved))))))

;;(defun auto-save-completions ()
;;  (if (and save-completions-flag enable-completion cmpl-initialized-p
;;	      *completion-auto-save-period*
;;	      (> cmpl-emacs-idle-time *completion-auto-save-period*)
;;	      cmpl-completions-accepted-p)
;;    (save-completions-to-file)))

;;(add-hook 'cmpl-emacs-idle-time-hooks 'auto-save-completions)

(defun load-completions-from-file (&optional filename no-message-p)
  "Load a completion init file FILENAME.
If file is not specified, then use `save-completions-file-name'."
  (interactive)
  (setq filename (expand-file-name (or filename save-completions-file-name)))
  (let* ((backup-filename (completion-backup-filename filename))
	 (backup-readable-p (file-readable-p backup-filename)))
    (if backup-readable-p (setq filename backup-filename))
    (if (file-readable-p filename)
	(progn
	  (if (not no-message-p)
	      (message "Loading completions from %sfile %s . . ."
		       (if backup-readable-p "backup " "") filename))
	  (with-current-buffer (get-buffer-create " *completion-save-buffer*")
	    (setq buffer-file-name filename)
	    ;; prepare the buffer to be modified
	    (clear-visited-file-modtime)
	    (erase-buffer)

	    (let ((insert-okay-p nil)
		  (buffer (current-buffer))
		  string entry last-use-time
		  cmpl-entry cmpl-last-use-time
		  (current-completion-source cmpl-source-init-file)
		  (start-num
		   (cmpl-statistics-block
		    (aref completion-add-count-vector cmpl-source-file-parsing)))
		  (total-in-file 0) (total-perm 0))
	      ;; insert the file into a buffer
	      (condition-case nil
		  (progn (insert-file-contents filename t)
			 (setq insert-okay-p t))

		(file-error
		 (message "File error trying to load completion file %s."
			  filename)))
	      ;; parse it
	      (if insert-okay-p
		  (progn
		    (goto-char (point-min))

		    (condition-case nil
			(while t
			  (setq entry (read buffer))
			  (setq total-in-file (1+ total-in-file))
			  (cond
			   ((and (consp entry)
				 (stringp (setq string (car entry)))
				 (cond
				  ((eq (setq last-use-time (cdr entry)) 'T)
				   ;; handle case sensitivity
				   (setq total-perm (1+ total-perm))
				   (setq last-use-time t))
				  ((eq last-use-time t)
				   (setq total-perm (1+ total-perm)))
				  ((integerp last-use-time))))
			    ;; Valid entry
			    ;; add it in
			    (setq cmpl-last-use-time
				  (completion-last-use-time
				   (setq cmpl-entry
					 (add-completion-to-tail-if-new string))))
			    (if (or (eq last-use-time t)
				    (and (> last-use-time 1000);;backcompatibility
					 (not (eq cmpl-last-use-time t))
					 (or (not cmpl-last-use-time)
					     ;; more recent
					     (> last-use-time  cmpl-last-use-time))))
				;; update last-use-time
				(set-completion-last-use-time cmpl-entry last-use-time)))
			   (t
			    ;; Bad format
			    (message "Error: invalid saved completion - %s"
				     (prin1-to-string entry))
			    ;; try to get back in sync
			    (search-forward "\n("))))
		      (search-failed
		       (message "End of file while reading completions."))
		      (end-of-file
		       (if (= (point) (point-max))
			   (if (not no-message-p)
			       (message "Loading completions from file %s . . . Done."
					filename))
			 (message "End of file while reading completions."))))))

	      (cmpl-statistics-block
	       (record-load-completions
		total-in-file total-perm
		(- (aref completion-add-count-vector cmpl-source-init-file)
		   start-num)))
))))))

(defun completion-initialize ()
  "Load the default completions file.
Also sets up so that exiting Emacs will automatically save the file."
  (interactive)
  (unless cmpl-initialized-p
    (load-completions-from-file)
    (setq cmpl-initialized-p t)))

;;-----------------------------------------------
;; Kill region patch
;;-----------------------------------------------

(defun completion-kill-region (&optional beg end)
  "Kill between point and mark.
The text is deleted but saved in the kill ring.
The command \\[yank] can retrieve it from there.
/(If you want to kill and then yank immediately, use \\[copy-region-as-kill].)

This is the primitive for programs to kill text (as opposed to deleting it).
Supply two arguments, character positions indicating the stretch of text
 to be killed.
Any command that calls this function is a \"kill command\".
If the previous command was also a kill command,
the text killed this time appends to the text killed last time
to make one entry in the kill ring.
Patched to remove the most recent completion."
  (interactive "r")
  (cond ((eq last-command 'complete)
	 (delete-region (point) cmpl-last-insert-location)
	 (insert cmpl-original-string)
	 (setq completion-to-accept nil)
	 (cmpl-statistics-block
	   (record-complete-failed)))
	(t
	 (kill-region beg end))))


;;-----------------------------------------------
;; Patches to self-insert-command.
;;-----------------------------------------------

;; Need 2 versions: generic separator chars. and space (to get auto fill
;; to work)

;; All common separators (eg. space "(" ")" """) characters go through a
;; function to add new words to the list of words to complete from:
;;  COMPLETION-SEPARATOR-SELF-INSERT-COMMAND (arg).
;; If the character before this was an alpha-numeric then this adds the
;; symbol before point to the completion list (using ADD-COMPLETION).

(defun completion-separator-self-insert-command (arg)
  (interactive "p")
  (if (command-remapping 'self-insert-command)
      (funcall (command-remapping 'self-insert-command) arg)
    (use-completion-before-separator)
    (self-insert-command arg)))

(defun completion-separator-self-insert-autofilling (arg)
  (interactive "p")
  (if (command-remapping 'self-insert-command)
      (funcall (command-remapping 'self-insert-command) arg)
    (use-completion-before-separator)
    (self-insert-command arg)
    (and auto-fill-function
	 (funcall auto-fill-function))))

;;-----------------------------------------------
;; Wrapping Macro
;;-----------------------------------------------

;; Note that because of the way byte compiling works, none of
;; the functions defined with this macro get byte compiled.

(defun completion-def-wrapper (function-name type)
  "Add a call to update the completion database before function execution.
TYPE is the type of the wrapper to be added.  Can be :before or :under."
  (put function-name 'completion-function
       (cdr (assq type
                  '((:separator . use-completion-before-separator)
                    (:before . use-completion-before-point)
                    (:backward-under . use-completion-backward-under)
                    (:backward . use-completion-backward)
                    (:under . use-completion-under-point)
                    (:under-or-before . use-completion-under-or-before-point)
                    (:minibuffer-separator
		     . use-completion-minibuffer-separator))))))

(defun use-completion-minibuffer-separator ()
  (let ((completion-syntax-table completion-standard-syntax-table))
    (use-completion-before-separator)))

(defun use-completion-backward-under ()
  (use-completion-under-point)
  (if (eq last-command 'complete)
      ;; probably a failed completion if you have to back up
      (cmpl-statistics-block (record-complete-failed))))

(defun use-completion-backward ()
  (if (eq last-command 'complete)
      ;; probably a failed completion if you have to back up
      (cmpl-statistics-block (record-complete-failed))))

(defun completion-before-command ()
  (funcall (or (and (symbolp this-command)
		    (get this-command 'completion-function))
	       'use-completion-under-or-before-point)))

;; Lisp mode diffs.

(defconst completion-lisp-syntax-table
  (let ((table (copy-syntax-table completion-standard-syntax-table))
	(symbol-chars '(?! ?& ?? ?= ?^)))
    (dolist (char symbol-chars)
      (modify-syntax-entry char "_" table))
    table))

(defun completion-lisp-mode-hook ()
  (setq completion-syntax-table completion-lisp-syntax-table)
  ;; Lisp Mode diffs
  (local-set-key "!" 'self-insert-command)
  (local-set-key "&" 'self-insert-command)
  (local-set-key "%" 'self-insert-command)
  (local-set-key "?" 'self-insert-command)
  (local-set-key "=" 'self-insert-command)
  (local-set-key "^" 'self-insert-command))

;; C mode diffs.

(defconst completion-c-syntax-table
  (let ((table (copy-syntax-table completion-standard-syntax-table))
	(separator-chars '(?+ ?* ?/ ?: ?%)))
    (dolist (char separator-chars)
      (modify-syntax-entry char " " table))
    table))

(completion-def-wrapper 'electric-c-semi :separator)
(defun completion-c-mode-hook ()
  (setq completion-syntax-table completion-c-syntax-table)
  (local-set-key "+" 'completion-separator-self-insert-command)
  (local-set-key "*" 'completion-separator-self-insert-command)
  (local-set-key "/" 'completion-separator-self-insert-command))

;; FORTRAN mode diffs. (these are defined when fortran is called)

(defconst completion-fortran-syntax-table
  (let ((table (copy-syntax-table completion-standard-syntax-table))
	(separator-chars '(?+ ?- ?* ?/ ?:)))
    (dolist (char separator-chars)
      (modify-syntax-entry char " " table))
    table))

(defun completion-setup-fortran-mode ()
  (setq completion-syntax-table completion-fortran-syntax-table)
  (local-set-key "+" 'completion-separator-self-insert-command)
  (local-set-key "-" 'completion-separator-self-insert-command)
  (local-set-key "*" 'completion-separator-self-insert-command)
  (local-set-key "/" 'completion-separator-self-insert-command))

;; Enable completion mode.

(defvar fortran-mode-hook)

(defvar completion-saved-bindings nil)

;;;###autoload
(define-minor-mode dynamic-completion-mode
  "Toggle dynamic word-completion on or off.
With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil."
  :global t
  ;; This is always good, not specific to dynamic-completion-mode.
  (define-key function-key-map [C-return] [?\C-\r])

  (dolist (x '((find-file-hook		. completion-find-file-hook)
               (pre-command-hook	. completion-before-command)
               ;; Save completions when killing Emacs.
               (kill-emacs-hook		. kill-emacs-save-completions)

               ;; Install the appropriate mode tables.
               (lisp-mode-hook		. completion-lisp-mode-hook)
               (c-mode-hook		. completion-c-mode-hook)
               (fortran-mode-hook	. completion-setup-fortran-mode)))
    (if dynamic-completion-mode
        (add-hook (car x) (cdr x))
      (remove-hook (car x) (cdr x))))

  ;; "Complete" Key Keybindings.  We don't want to use a minor-mode
  ;; map because these have too high a priority.  We could/should
  ;; probably change the interpretation of minor-mode-map-alist such
  ;; that a map has lower precedence if the symbol is not buffer-local.
  (while completion-saved-bindings
    (let ((binding (pop completion-saved-bindings)))
      (global-set-key (car binding) (cdr binding))))
  (when dynamic-completion-mode
    (dolist (binding
             '(("\M-\r"  . complete)
               ([?\C-\r] . complete)

               ;; Tests -
               ;; (add-completion "cumberland")
               ;; (add-completion "cumberbund")
               ;; cum
               ;; Cumber
               ;; cumbering
               ;; cumb

               ;; Patches to standard keymaps insert completions
               ([remap kill-region] . completion-kill-region)

               ;; Separators
               ;; We've used the completion syntax table given  as a guide.
               ;;
               ;; Global separator chars.
               ;;  We left out <tab> because there are too many special
               ;; cases for it.  Also, in normal coding it's rarely typed
               ;; after a word.
               (" " . completion-separator-self-insert-autofilling)
               ("!" . completion-separator-self-insert-command)
               ("%" . completion-separator-self-insert-command)
               ("^" . completion-separator-self-insert-command)
               ("&" . completion-separator-self-insert-command)
               ("(" . completion-separator-self-insert-command)
               (")" . completion-separator-self-insert-command)
               ("=" . completion-separator-self-insert-command)
               ("`" . completion-separator-self-insert-command)
               ("|" . completion-separator-self-insert-command)
               ("{" . completion-separator-self-insert-command)
               ("}" . completion-separator-self-insert-command)
               ("[" . completion-separator-self-insert-command)
               ("]" . completion-separator-self-insert-command)
               (";" . completion-separator-self-insert-command)
               ("\"".  completion-separator-self-insert-command)
               ("'" . completion-separator-self-insert-command)
               ("#" . completion-separator-self-insert-command)
               ("," . completion-separator-self-insert-command)
               ("?" . completion-separator-self-insert-command)

               ;; We include period and colon even though they are symbol
               ;; chars because :
               ;;  - in text we want to pick up the last word in a sentence.
               ;;  - in C pointer refs. we want to pick up the first symbol
               ;;  - it won't make a difference for lisp mode (package names
               ;;    are short)
               ("." . completion-separator-self-insert-command)
               (":" . completion-separator-self-insert-command)))
      (push (cons (car binding) (lookup-key global-map (car binding)))
            completion-saved-bindings)
      (global-set-key (car binding) (cdr binding)))

    ;; Tests --
    ;; foobarbiz
    ;; foobar
    ;; fooquux
    ;; fooper

    (cmpl-statistics-block
     (record-completion-file-loaded))

    (completion-initialize)))

;;-----------------------------------------------
;; End of line chars.
;;-----------------------------------------------
(completion-def-wrapper 'newline :separator)
(completion-def-wrapper 'newline-and-indent :separator)
(completion-def-wrapper 'comint-send-input :separator)
(completion-def-wrapper 'exit-minibuffer :minibuffer-separator)
(completion-def-wrapper 'eval-print-last-sexp :separator)
(completion-def-wrapper 'eval-last-sexp :separator)
;;(completion-def-wrapper 'minibuffer-complete-and-exit :minibuffer)

;;-----------------------------------------------
;; Cursor movement
;;-----------------------------------------------

(completion-def-wrapper 'next-line :under-or-before)
(completion-def-wrapper 'previous-line :under-or-before)
(completion-def-wrapper 'beginning-of-buffer :under-or-before)
(completion-def-wrapper 'end-of-buffer :under-or-before)
(completion-def-wrapper 'beginning-of-line :under-or-before)
(completion-def-wrapper 'end-of-line :under-or-before)
(completion-def-wrapper 'forward-char :under-or-before)
(completion-def-wrapper 'forward-word :under-or-before)
(completion-def-wrapper 'forward-sexp :under-or-before)
(completion-def-wrapper 'backward-char :backward-under)
(completion-def-wrapper 'backward-word :backward-under)
(completion-def-wrapper 'backward-sexp :backward-under)

(completion-def-wrapper 'delete-backward-char :backward)
(completion-def-wrapper 'delete-backward-char-untabify :backward)

;; Old names, non-namespace-clean.
(defvaralias 'cmpl-syntax-table 'completion-syntax-table)
(defalias 'initialize-completions 'completion-initialize)

(dolist (x '("^To complete, the point must be after a symbol at least [0-9]* character long\\.$"
	"^The string \".*\" is too short to be saved as a completion\\.$"))
  (add-to-list 'debug-ignored-errors x))

(provide 'completion)

;;; completion.el ends here
