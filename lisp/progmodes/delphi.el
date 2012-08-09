;;; delphi.el --- major mode for editing Delphi source (Object Pascal) in Emacs

;; Copyright (C) 1998-1999, 2001-2012  Free Software Foundation, Inc.

;; Authors: Ray Blaak <blaak@infomatch.com>,
;;          Simon South <ssouth@member.fsf.org>
;; Maintainer: Simon South <ssouth@member.fsf.org>
;; Keywords: languages

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

;; To enter Delphi mode when you find a Delphi source file, one must override
;; the auto-mode-alist to associate Delphi with .pas (and .dpr and .dpk)
;; files.  Emacs, by default, will otherwise enter Pascal mode. E.g.
;;
;; (autoload 'delphi-mode "delphi")
;; (setq auto-mode-alist
;;       (cons '("\\.\\(pas\\|dpr\\|dpk\\)$" . delphi-mode) auto-mode-alist))

;; To get keyword, comment, and string literal coloring, be sure that font-lock
;; is running.  One can manually do M-x font-lock-mode in a Delphi buffer, or
;; one can put in .emacs:
;;
;; (add-hook 'delphi-mode-hook 'turn-on-font-lock)

;; If font-lock is not loaded by default, you might have to do:
;;
;; (autoload 'font-lock-mode "font-lock")
;; (autoload 'turn-on-font-lock "font-lock")
;; (setq font-lock-support-mode 'lazy-lock-mode)
;;
;; Lazy lock is very necessary for faster screen updates.

;; For good performance, be sure to byte-compile delphi.el, e.g.
;;
;; M-x byte-compile-file <give the path to delphi.el when prompted>

;; This will generate delphi.elc, which will be loaded instead of delphi.el
;; when delphi-mode is autoloaded.

;; When you have entered Delphi mode, you may get more info by pressing
;; C-h m.

;; This Delphi mode implementation is fairly tolerant of syntax errors, relying
;; as much as possible on the indentation of the previous statement.  This also
;; makes it faster and simpler, since there is less searching for properly
;; constructed beginnings.

;;; Code:

(provide 'delphi)

(defgroup delphi nil
  "Major mode for editing Delphi source in Emacs."
  :version "21.1"
  :group 'languages)

(defconst delphi-debug nil
  "True if in debug mode.")

(defcustom delphi-search-path "."
  "*Directories to search when finding external units.
It is a list of directory strings.  If only a single directory,
it can be a single string instead of a list.  If a directory
ends in \"...\" then that directory is recursively searched."
  :type 'string
  :group 'delphi)

(defcustom delphi-indent-level 3
  "*Indentation of Delphi statements with respect to containing block.
E.g.

begin
   // This is an indent of 3.
end;"
  :type 'integer
  :group 'delphi)

(defcustom delphi-compound-block-indent 0
  "*Extra indentation for blocks in compound statements. E.g.

// block indent = 0     vs      // block indent = 2
if b then                       if b then
begin                             begin
end else begin                    end
end;                            else
                                  begin
                                  end;"
  :type 'integer
  :group 'delphi)

(defcustom delphi-case-label-indent delphi-indent-level
  "*Extra indentation for case statement labels. E.g.

// case indent = 0      vs      // case indent = 3
case value of                   case value of
v1: process_v1;                    v1: process_v1;
v2: process_v2;                    v2: process_v2;
else                            else
   process_else;                   process_else;
end;                            end;"
  :type 'integer
  :group 'delphi)

(defcustom delphi-verbose t ; nil
  "*If true then Delphi token processing progress is reported to the user."
  :type 'boolean
  :group 'delphi)

(defcustom delphi-tab-always-indents t
  "*Non-nil means TAB in Delphi mode should always reindent the current line,
regardless of where in the line point is when the TAB command is used."
  :type 'boolean
  :group 'delphi)

(defcustom delphi-newline-always-indents t
  "*Non-nil means NEWLINE in Delphi mode should always reindent the current
line, insert a blank line and move to the default indent column of the blank
line.  If nil, then no indentation occurs, and NEWLINE does the usual
behavior.  This is useful when one needs to do customized indentation that
differs from the default."
  :type 'boolean
  :group 'delphi)

(defcustom delphi-comment-face 'font-lock-comment-face
  "*Face used to color Delphi comments."
  :type 'face
  :group 'delphi)

(defcustom delphi-string-face 'font-lock-string-face
  "*Face used to color Delphi strings."
  :type 'face
  :group 'delphi)

(defcustom delphi-keyword-face 'font-lock-keyword-face
  "*Face used to color Delphi keywords."
  :type 'face
  :group 'delphi)

(defcustom delphi-other-face nil
  "*Face used to color everything else."
  :type '(choice (const :tag "None" nil) face)
  :group 'delphi)

(defconst delphi-directives
  '(absolute abstract assembler automated cdecl default dispid dynamic
    export external far forward index inline message name near nodefault
    overload override pascal private protected public published read readonly
    register reintroduce resident resourcestring safecall stdcall stored
    virtual write writeonly)
  "Delphi4 directives.")

(defconst delphi-keywords
  (append
   '(;; Keywords.
     and array as asm at begin case class const constructor contains
     destructor dispinterface div do downto else end except exports
     file finalization finally for function goto if implementation implements
     in inherited initialization interface is label library mod nil not
     of object on or out package packed procedure program property
     raise record repeat requires result self set shl shr then threadvar
     to try type unit uses until var while with xor

     ;; These routines should be keywords, if Borland had the balls.
     break exit)

   ;; We want directives to look like keywords.
   delphi-directives)
  "Delphi4 keywords.")

(defconst delphi-previous-terminators `(semicolon comma)
  "Expression/statement terminators that denote a previous expression.")

(defconst delphi-comments
  '(comment-single-line comment-multi-line-1 comment-multi-line-2)
  "Tokens that represent comments.")

(defconst delphi-strings
  '(string double-quoted-string)
  "Tokens that represent string literals.")

(defconst delphi-whitespace `(space newline ,@delphi-comments)
  "Tokens that are considered whitespace.")

(defconst delphi-routine-statements
  '(procedure function constructor destructor property)
  "Marks the start of a routine, or routine-ish looking expression.")

(defconst delphi-body-expr-statements '(if while for on)
  "Statements that have either a single statement or a block as a body and also
are followed by an expression.")

(defconst delphi-expr-statements `(case ,@delphi-body-expr-statements)
  "Expression statements contain expressions after their keyword.")

(defconst delphi-body-statements `(else ,@delphi-body-expr-statements)
  "Statements that have either a single statement or a block as a body.")

(defconst delphi-expr-delimiters '(then do of)
  "Expression delimiter tokens.")

(defconst delphi-binary-ops
  '(plus minus equals not-equals times divides div mod and or xor)
  "Delphi binary operations.")

(defconst delphi-visibilities '(public private protected published automated)
  "Class visibilities.")

(defconst delphi-block-statements
  '(begin try case repeat initialization finalization asm)
  "Statements that contain multiple substatements.")

(defconst delphi-mid-block-statements
  `(except finally ,@delphi-visibilities)
  "Statements that mark mid sections of the enclosing block.")

(defconst delphi-end-block-statements `(end until)
  "Statements that end block sections.")

(defconst delphi-match-block-statements
  `(,@delphi-end-block-statements ,@delphi-mid-block-statements)
  "Statements that match the indentation of the parent block.")

(defconst delphi-decl-sections '(type const var label resourcestring)
  "Denotes the start of a declaration section.")

(defconst delphi-interface-types '(dispinterface interface)
  "Interface types.")

(defconst delphi-class-types '(class object)
  "Class types.")

(defconst delphi-composite-types
  `(,@delphi-class-types ,@delphi-interface-types record)
  "Types that contain declarations within them.")

(defconst delphi-unit-sections
  '(interface implementation program library package)
  "Unit sections within which the indent is 0.")

(defconst delphi-use-clauses `(uses requires exports contains)
  "Statements that refer to foreign symbols.")

(defconst delphi-unit-statements
  `(,@delphi-use-clauses ,@delphi-unit-sections initialization finalization)
  "Statements indented at level 0.")

(defconst delphi-decl-delimiters
  `(,@delphi-decl-sections ,@delphi-unit-statements
    ,@delphi-routine-statements)
  "Statements that a declaration statement should align with.")

(defconst delphi-decl-matchers
  `(begin ,@delphi-decl-sections)
  "Statements that should match to declaration statement indentation.")

(defconst delphi-enclosing-statements
  `(,@delphi-block-statements ,@delphi-mid-block-statements
    ,@delphi-decl-sections ,@delphi-use-clauses ,@delphi-routine-statements)
  "Delimits an enclosing statement.")

(defconst delphi-previous-statements
  `(,@delphi-unit-statements ,@delphi-routine-statements)
  "Delimits a previous statement.")

(defconst delphi-previous-enclosing-statements
  `(,@delphi-block-statements ,@delphi-mid-block-statements
    ,@delphi-decl-sections)
  "Delimits a previous enclosing statement.")

(defconst delphi-begin-enclosing-tokens
  `(,@delphi-block-statements ,@delphi-mid-block-statements)
  "Tokens that a begin token indents from.")

(defconst delphi-begin-previous-tokens
  `(,@delphi-decl-sections ,@delphi-routine-statements)
  "Tokens that a begin token aligns with, but only if not part of a nested
routine.")

(defconst delphi-space-chars "\000-\011\013- ") ; all except \n
(defconst delphi-non-space-chars (concat "^" delphi-space-chars))
(defconst delphi-spaces-re (concat "[" delphi-space-chars "]*"))
(defconst delphi-leading-spaces-re (concat "^" delphi-spaces-re))
(defconst delphi-word-chars "a-zA-Z0-9_")

(defmacro delphi-save-match-data (&rest forms)
  ;; Executes the forms such that the current match data is preserved, so as
  ;; not to disturb any existing search results.
  `(let ((data (match-data)))
     (unwind-protect
         (progn ,@forms)
       (set-match-data data))))

(defmacro delphi-save-excursion (&rest forms)
  ;; Executes the forms such that any movements have no effect, including
  ;; searches.
  `(save-excursion
     (delphi-save-match-data
      (let ((inhibit-point-motion-hooks t)
            (deactivate-mark nil))
        (progn ,@forms)))))

(defmacro delphi-save-state (&rest forms)
  ;; Executes the forms such that any buffer modifications do not have any side
  ;; effects beyond the buffer's actual content changes.
  `(let ((delphi-ignore-changes t)
         (old-supersession-threat
          (symbol-function 'ask-user-about-supersession-threat))
         (buffer-read-only nil)
         (inhibit-read-only t)
         (buffer-undo-list t)
         (before-change-functions nil)
         (after-change-functions nil)
         (modified (buffer-modified-p)))
     ;; Disable any queries about editing obsolete files.
     (fset 'ask-user-about-supersession-threat (lambda (_fn)))
     (unwind-protect
         (progn ,@forms)
       (set-buffer-modified-p modified)
       (fset 'ask-user-about-supersession-threat old-supersession-threat))))

(defsubst delphi-is (element in-set)
  ;; If the element is in the set, the element cdr is returned, otherwise nil.
  (memq element in-set))

(defun delphi-string-of (start end)
  ;; Returns the buffer string from start to end.
  (buffer-substring-no-properties start end))

(defun delphi-looking-at-string (p s)
  ;; True if point p marks the start of string s. s is not a regular
  ;; expression.
  (let ((limit (+ p (length s))))
    (and (<= limit (point-max))
         (string= s (delphi-string-of p limit)))))

(defun delphi-token-of (kind start end)
  ;; Constructs a token from a kind symbol and its start/end points.
  `[,kind ,start ,end])

(defsubst delphi-token-kind (token)
  ;; Returns the kind symbol of the token.
  (if token (aref token 0) nil))

(defun delphi-set-token-kind (token to-kind)
  ;; Sets the kind symbol of the token.
  (if token (aset token 0 to-kind)))

(defsubst delphi-token-start (token)
  ;; Returns the start point of the token.
  (if token (aref token 1) (point-min)))

(defsubst delphi-token-end (token)
  ;; Returns the end point of the token.
  (if token (aref token 2) (point-min)))

(defun delphi-set-token-start (token start)
  ;; Sets the start point of the token.
  (if token (aset token 1 start)))

(defun delphi-set-token-end (token end)
  ;; Sets the end point of the token.
  (if token (aset token 2 end)))

(defun delphi-token-string (token)
  ;; Returns the string image of the token.
  (if token
      (delphi-string-of (delphi-token-start token) (delphi-token-end token))
    ""))

(defun delphi-in-token (p token)
  ;; Returns true if the point p is within the token's start/end points.
  (and (<= (delphi-token-start token) p) (< p (delphi-token-end token))))

(defun delphi-column-of (p)
  ;; Returns the column of the point p.
  (save-excursion (goto-char p) (current-column)))

(defun delphi-face-of (token-kind)
  ;; Returns the face property appropriate for the token kind.
  (cond ((delphi-is token-kind delphi-comments) delphi-comment-face)
        ((delphi-is token-kind delphi-strings) delphi-string-face)
        ((delphi-is token-kind delphi-keywords) delphi-keyword-face)
        (delphi-other-face)))

(defvar delphi-progress-last-reported-point nil
  "The last point at which progress was reported.")

(defconst delphi-parsing-progress-step 16384
  "Number of chars to process before the next parsing progress report.")
(defconst delphi-scanning-progress-step 2048
  "Number of chars to process before the next scanning progress report.")
(defconst delphi-fontifying-progress-step delphi-scanning-progress-step
  "Number of chars to process before the next fontification progress report.")

(defun delphi-progress-start ()
  ;; Initializes progress reporting.
  (setq delphi-progress-last-reported-point nil))

(defun delphi-progress-done (&rest msgs)
  ;; Finalizes progress reporting.
  (setq delphi-progress-last-reported-point nil)
  (when delphi-verbose
     (if (null msgs)
         (message "")
       (apply #'message msgs))))

(defun delphi-step-progress (p desc step-size)
  ;; If enough distance has elapsed since the last reported point, then report
  ;; the current progress to the user.
  (cond ((null delphi-progress-last-reported-point)
         ;; This is the first progress step.
         (setq delphi-progress-last-reported-point p))

        ((and delphi-verbose
              (>= (abs (- p delphi-progress-last-reported-point)) step-size))
         ;; Report the percentage complete.
         (setq delphi-progress-last-reported-point p)
         (message "%s %s ... %d%%"
                  desc (buffer-name) (/ (* 100 p) (point-max))))))

(defun delphi-next-line-start (&optional from-point)
  ;; Returns the first point of the next line.
  (let ((curr-point (point))
        (next nil))
    (if from-point (goto-char from-point))
    (end-of-line)
    (setq next (min (1+ (point)) (point-max)))
    (goto-char curr-point)
    next))

(defvar delphi-ignore-changes t
  "Internal flag to control if the Delphi mode responds to buffer changes.
Defaults to t in case the `delphi-after-change' function is called on a
non-Delphi buffer.  Set to nil in a Delphi buffer.  To override, just do:
 (let ((delphi-ignore-changes t)) ...)")

(defun delphi-set-text-properties (from to properties)
  ;; Like `set-text-properties', except we do not consider this to be a buffer
  ;; modification.
  (delphi-save-state
   (set-text-properties from to properties)))

(defun delphi-literal-kind (p)
  ;; Returns the literal kind the point p is in (or nil if not in a literal).
  (if (and (<= (point-min) p) (<= p (point-max)))
      (get-text-property p 'token)))

(defun delphi-literal-start-pattern (literal-kind)
  ;; Returns the start pattern of the literal kind.
  (cdr (assoc literal-kind
              '((comment-single-line . "//")
                (comment-multi-line-1 . "{")
                (comment-multi-line-2 . "(*")
                (string . "'")
                (double-quoted-string . "\"")))))

(defun delphi-literal-end-pattern (literal-kind)
  ;; Returns the end pattern of the literal kind.
  (cdr (assoc literal-kind
              '((comment-single-line . "\n")
                (comment-multi-line-1 . "}")
                (comment-multi-line-2 . "*)")
                (string . "'")
                (double-quoted-string . "\"")))))

(defun delphi-literal-stop-pattern (literal-kind)
  ;; Returns the pattern that delimits end of the search for the literal kind.
  ;; These are regular expressions.
  (cdr (assoc literal-kind
              '((comment-single-line . "\n")
                (comment-multi-line-1 . "}")
                (comment-multi-line-2 . "\\*)")
                ;; Strings cannot span lines.
                (string . "['\n]")
                (double-quoted-string . "[\"\n]")))))

(defun delphi-is-literal-start (p)
  ;; True if the point p is at the start point of a (completed) literal.
  (let* ((kind (delphi-literal-kind p))
         (pattern (delphi-literal-start-pattern kind)))
    (or (null kind) ; Non-literals are considered as start points.
        (delphi-looking-at-string p pattern))))

(defun delphi-is-literal-end (p)
  ;; True if the point p is at the end point of a (completed) literal.
  (let* ((kind (delphi-literal-kind (1- p)))
         (pattern (delphi-literal-end-pattern kind)))
    (or (null kind) ; Non-literals are considered as end points.

        (and (delphi-looking-at-string (- p (length pattern)) pattern)
             (or (not (delphi-is kind delphi-strings))
                 ;; Special case: string delimiters are start/end ambiguous.
                 ;; We have an end only if there is some string content (at
                 ;; least a starting delimiter).
                 (not (delphi-is-literal-end (1- p)))))

        ;; Special case: strings cannot span lines.
        (and (delphi-is kind delphi-strings) (eq ?\n (char-after (1- p)))))))

(defun delphi-is-stable-literal (p)
  ;; True if the point p marks a stable point. That is, a point outside of a
  ;; literal region, inside of a literal region, or adjacent to completed
  ;; literal regions.
  (let ((at-start (delphi-is-literal-start p))
        (at-end  (delphi-is-literal-end p)))
    (or (>= p (point-max))
        (and at-start at-end)
        (and (not at-start) (not at-end)
             (eq (delphi-literal-kind (1- p)) (delphi-literal-kind p))))))

(defun delphi-complete-literal (literal-kind limit)
  ;; Continues the search for a literal's true end point and returns the
  ;; point past the end pattern (if found) or the limit (if not found).
  (let ((pattern (delphi-literal-stop-pattern literal-kind)))
    (if (not (stringp pattern))
        (error "Invalid literal kind %S" literal-kind)
      ;; Search up to the limit.
      (re-search-forward pattern limit 'goto-limit-on-fail)
      (point))))

(defun delphi-literal-text-properties (kind)
  ;; Creates a list of text properties for the literal kind.
  (if (and (boundp 'font-lock-mode)
           font-lock-mode)
      (list 'token kind 'face (delphi-face-of kind) 'lazy-lock t)
    (list 'token kind)))

(defun delphi-parse-next-literal (limit)
  ;; Searches for the next literal region (i.e. comment or string) and sets the
  ;; the point to its end (or the limit, if not found). The literal region is
  ;; marked as such with a text property, to speed up tokenizing during face
  ;; coloring and indentation scanning.
  (let ((search-start (point)))
    (cond ((not (delphi-is-literal-end search-start))
           ;; We are completing an incomplete literal.
           (let ((kind (delphi-literal-kind (1- search-start))))
             (delphi-complete-literal kind limit)
             (delphi-set-text-properties
              search-start (point) (delphi-literal-text-properties kind))))

          ((re-search-forward
            "\\(//\\)\\|\\({\\)\\|\\((\\*\\)\\|\\('\\)\\|\\(\"\\)"
            limit 'goto-limit-on-fail)
           ;; We found the start of a new literal. Find its end and mark it.
           (let ((kind (cond ((match-beginning 1) 'comment-single-line)
                             ((match-beginning 2) 'comment-multi-line-1)
                             ((match-beginning 3) 'comment-multi-line-2)
                             ((match-beginning 4) 'string)
                             ((match-beginning 5) 'double-quoted-string)))
                 (start (match-beginning 0)))
             (delphi-set-text-properties search-start start nil)
             (delphi-complete-literal kind limit)
             (delphi-set-text-properties
              start (point) (delphi-literal-text-properties kind))))

          ;; Nothing found. Mark it as a non-literal.
          ((delphi-set-text-properties search-start limit nil)))
    (delphi-step-progress (point) "Parsing" delphi-parsing-progress-step)))

(defun delphi-literal-token-at (p)
  ;; Returns the literal token surrounding the point p, or nil if none.
  (let ((kind (delphi-literal-kind p)))
    (when kind
      (let ((start (previous-single-property-change (1+ p) 'token))
            (end (next-single-property-change p 'token)))
        (delphi-token-of kind (or start (point-min)) (or end (point-max)))))))

(defun delphi-point-token-at (p kind)
  ;; Returns the single character token at the point p.
  (delphi-token-of kind p (1+ p)))

(defsubst delphi-char-token-at (p char kind)
  ;; Returns the token at the point p that describes the specified character.
  ;; If not actually over such a character, nil is returned.
  (when (eq char (char-after p))
    (delphi-token-of kind p (1+ p))))

(defun delphi-charset-token-at (p charset kind)
  ;; Returns the token surrounding point p that contains only members of the
  ;; character set.
  (let ((currp (point))
        (end nil)
        (token nil))
    (goto-char p)
    (when (> (skip-chars-forward charset) 0)
       (setq end (point))
       (goto-char (1+ p))
       (skip-chars-backward charset)
       (setq token (delphi-token-of kind (point) end)))
    (goto-char currp)
    token))

(defun delphi-space-token-at (p)
  ;; If point p is surrounded by space characters, then return the token of the
  ;; contiguous spaces.
  (delphi-charset-token-at p delphi-space-chars 'space))

(defun delphi-word-token-at (p)
  ;; If point p is over a word (i.e. identifier characters), then return a word
  ;; token. If the word is actually a keyword, then return the keyword token.
  (let ((word (delphi-charset-token-at p delphi-word-chars 'word)))
    (when word
      (let* ((word-image (downcase (delphi-token-string word)))
             (keyword (intern-soft word-image)))
        (when (and (or keyword (string= "nil" word-image))
                   (delphi-is keyword delphi-keywords))
          (delphi-set-token-kind word keyword))
        word))))

(defun delphi-explicit-token-at (p token-string kind)
  ;; If point p is anywhere in the token string then returns the resulting
  ;; token.
  (let ((token (delphi-charset-token-at p token-string kind)))
    (when (and token (string= token-string (delphi-token-string token)))
       token)))

(defun delphi-token-at (p)
  ;; Returns the token from parsing text at point p.
  (when (and (<= (point-min) p) (<= p (point-max)))
     (cond ((delphi-char-token-at p ?\n 'newline))

           ((delphi-literal-token-at p))

           ((delphi-space-token-at p))

           ((delphi-word-token-at p))

           ((delphi-char-token-at p ?\( 'open-group))
           ((delphi-char-token-at p ?\) 'close-group))
           ((delphi-char-token-at p ?\[ 'open-group))
           ((delphi-char-token-at p ?\] 'close-group))
           ((delphi-char-token-at p ?\; 'semicolon))
           ((delphi-char-token-at p ?. 'dot))
           ((delphi-char-token-at p ?, 'comma))
           ((delphi-char-token-at p ?= 'equals))
           ((delphi-char-token-at p ?+ 'plus))
           ((delphi-char-token-at p ?- 'minus))
           ((delphi-char-token-at p ?* 'times))
           ((delphi-char-token-at p ?/ 'divides))
           ((delphi-char-token-at p ?: 'colon))

           ((delphi-explicit-token-at p "<>" 'not-equals))

           ((delphi-point-token-at p 'punctuation)))))

(defun delphi-current-token ()
  ;; Returns the delphi source token under the current point.
  (delphi-token-at (point)))

(defun delphi-next-token (token)
  ;; Returns the token after the specified token.
  (when token
     (let ((next (delphi-token-at (delphi-token-end token))))
       (if next
           (delphi-step-progress (delphi-token-start next) "Scanning"
                                 delphi-scanning-progress-step))
       next)))

(defun delphi-previous-token (token)
  ;; Returns the token before the specified token.
  (when token
     (let ((previous (delphi-token-at (1- (delphi-token-start token)))))
       (if previous
           (delphi-step-progress (delphi-token-start previous) "Scanning"
                                 delphi-scanning-progress-step))
       previous)))

(defun delphi-next-visible-token (token)
  ;; Returns the first non-space token after the specified token.
  (let (next-token)
    (while (progn
             (setq next-token (delphi-next-token token))
             (delphi-is (delphi-token-kind next-token) '(space newline))))
    next-token))

(defun delphi-parse-region (from to)
  ;; Parses the literal tokens in the region. The point is set to "to".
  (save-restriction
    (widen)
    (goto-char from)
    (while (< (point) to)
      (delphi-parse-next-literal to))))

(defun delphi-parse-region-until-stable (from to)
  ;; Parses at least the literal tokens in the region. After that, parsing
  ;; continues as long as obsolete literal regions are encountered. The point
  ;; is set to the encountered stable point.
  (save-restriction
    (widen)
    (delphi-parse-region from to)
    (while (not (delphi-is-stable-literal (point)))
      (delphi-parse-next-literal (point-max)))))

(defun delphi-fontify-region (from to &optional verbose)
  ;; Colors the text in the region according to Delphi rules.
  (delphi-save-excursion
   (delphi-save-state
    (let ((p from)
          (delphi-verbose verbose)
          (token nil))
      (delphi-progress-start)
      (while (< p to)
        ;; Color the token and move past it.
        (setq token (delphi-token-at p))
        (add-text-properties
         (delphi-token-start token) (delphi-token-end token)
         (list 'face (delphi-face-of (delphi-token-kind token)) 'lazy-lock t))
        (setq p (delphi-token-end token))
        (delphi-step-progress p "Fontifying" delphi-fontifying-progress-step))
      (delphi-progress-done)))))

(defun delphi-after-change (change-start change-end _old-length)
  ;; Called when the buffer has changed. Reparses the changed region.
  (unless delphi-ignore-changes
    (let ((delphi-ignore-changes t)) ; Prevent recursive calls.
      (delphi-save-excursion
       (delphi-progress-start)
       ;; Reparse at least from the token previous to the change to the end of
       ;; line after the change.
       (delphi-parse-region-until-stable
        (delphi-token-start (delphi-token-at (1- change-start)))
        (progn (goto-char change-end) (end-of-line) (point)))
       (delphi-progress-done)))))

(defun delphi-group-start (from-token)
  ;; Returns the token that denotes the start of the ()/[] group.
  (let ((token (delphi-previous-token from-token))
        (token-kind nil))
    (catch 'done
      (while token
        (setq token-kind (delphi-token-kind token))
        (cond
         ;; Skip over nested groups.
         ((eq 'close-group token-kind) (setq token (delphi-group-start token)))
         ((eq 'open-group token-kind) (throw 'done token)))
        (setq token (delphi-previous-token token)))
      ;; Start not found.
      nil)))

(defun delphi-group-end (from-token)
  ;; Returns the token that denotes the end of the ()/[] group.
  (let ((token (delphi-next-token from-token))
        (token-kind nil))
    (catch 'done
      (while token
        (setq token-kind (delphi-token-kind token))
        (cond
         ;; Skip over nested groups.
         ((eq 'open-group token-kind) (setq token (delphi-group-end token)))
         ((eq 'close-group token-kind) (throw 'done token)))
        (setq token (delphi-next-token token)))
      ;; end not found.
      nil)))

(defun delphi-indent-of (token &optional offset)
  ;; Returns the start column of the token, plus any offset.
  (let ((indent (+ (delphi-column-of (delphi-token-start token))
                   (if offset offset 0))))
    (when delphi-debug
      (delphi-debug-log
       (concat "\n Indent of: %S %S"
               "\n column: %d indent: %d offset: %d")
       token (delphi-token-string token)
       (delphi-column-of (delphi-token-start token))
       indent (if offset offset 0)))
    indent))

(defun delphi-line-indent-of (from-token &optional offset &rest terminators)
  ;; Returns the column of first non-space character on the token's line, plus
  ;; any offset. We also stop if one of the terminators or an open ( or [ is
  ;; encountered.
  (let ((token (delphi-previous-token from-token))
        (last-token from-token)
        (kind nil))
    (catch 'done
      (while token
        (setq kind (delphi-token-kind token))
        (cond
         ;; Skip over ()/[] groups.
         ((eq 'close-group kind) (setq token (delphi-group-start token)))

         ;; Stop at the beginning of the line or an open group.
         ((delphi-is kind '(newline open-group)) (throw 'done nil))

         ;; Stop at one of the specified terminators.
         ((delphi-is kind terminators) (throw 'done nil)))
        (unless (delphi-is kind delphi-whitespace) (setq last-token token))
        (setq token (delphi-previous-token token))))
    (delphi-indent-of last-token offset)))

(defun delphi-stmt-line-indent-of (from-token &optional offset)
  ;; Like `delphi-line-indent-of' except is also stops on a use clause, and
  ;; colons that precede statements (i.e. case labels).
  (let ((token (delphi-previous-token from-token))
        (last-token from-token)
        (kind nil))
    (catch 'done
      (while token
        (setq kind (delphi-token-kind token))
        (cond
         ((and (eq 'colon kind)
               (delphi-is (delphi-token-kind last-token)
                          `(,@delphi-block-statements
                            ,@delphi-expr-statements)))
          ;; We hit a label followed by a statement. Indent to the statement.
          (throw 'done nil))

         ;; Skip over ()/[] groups.
         ((eq 'close-group kind) (setq token (delphi-group-start token)))

         ((delphi-is kind `(newline open-group ,@delphi-use-clauses))
          ;; Stop at the beginning of the line, an open group, or a use clause
          (throw 'done nil)))
        (unless (delphi-is kind delphi-whitespace) (setq last-token token))
        (setq token (delphi-previous-token token))))
    (delphi-indent-of last-token offset)))

(defun delphi-open-group-indent (token last-token &optional offset)
  ;; Returns the indent relative to an unmatched ( or [.
  (when (eq 'open-group (delphi-token-kind token))
    (if last-token
        (delphi-indent-of last-token offset)
      ;; There is nothing following the ( or [. Indent from its line.
      (delphi-stmt-line-indent-of token delphi-indent-level))))

(defun delphi-composite-type-start (token last-token)
  ;; Returns true (actually the last-token) if the pair equals (= class), (=
  ;; dispinterface), (= interface), (= object), or (= record), and nil
  ;; otherwise.
  (if (and (eq 'equals (delphi-token-kind token))
           (delphi-is (delphi-token-kind last-token) delphi-composite-types))
      last-token))

(defun delphi-is-simple-class-type (at-token limit-token)
  ;; True if at-token is the start of a simple class type. E.g.
  ;;   class of TClass;
  ;;   class (TBaseClass);
  ;;   class;
  (when (delphi-is (delphi-token-kind at-token) delphi-class-types)
    (catch 'done
      ;; Scan until the semi colon.
      (let ((token (delphi-next-token at-token))
            (token-kind nil)
            (limit (delphi-token-start limit-token)))
        (while (and token (<= (delphi-token-start token) limit))
          (setq token-kind (delphi-token-kind token))
          (cond
           ;; A semicolon delimits the search.
           ((eq 'semicolon token-kind) (throw 'done token))

           ;; Skip over the inheritance list.
           ((eq 'open-group token-kind) (setq token (delphi-group-end token)))

           ;; Only allow "of" and whitespace, and an identifier
           ((delphi-is token-kind `(of word ,@delphi-whitespace)))

           ;; Otherwise we are not in a simple class declaration.
           ((throw 'done nil)))
          (setq token (delphi-next-token token)))))))

(defun delphi-block-start (from-token &optional stop-on-class)
  ;; Returns the token that denotes the start of the block.
  (let ((token (delphi-previous-token from-token))
        (last-token nil)
        (token-kind nil))
    (catch 'done
      (while token
        (setq token-kind (delphi-token-kind token))
        (cond
         ;; Skip over nested blocks.
         ((delphi-is token-kind delphi-end-block-statements)
          (setq token (delphi-block-start token)))

         ;; Regular block start found.
         ((delphi-is token-kind delphi-block-statements)
          (throw 'done
                 ;; As a special case, when a "case" block appears
                 ;; within a record declaration (to denote a variant
                 ;; part), the record declaration should be considered
                 ;; the enclosing block.
                 (if (eq 'case token-kind)
                     (let ((enclosing-token
                            (delphi-block-start token
                                                'stop-on-class)))
                       (if
                           (eq 'record
                               (delphi-token-kind enclosing-token))
                           (if stop-on-class
                               enclosing-token
                             (delphi-previous-token enclosing-token))
                         token))
                   token)))

         ;; A class/record start also begins a block.
         ((delphi-composite-type-start token last-token)
          (throw 'done (if stop-on-class last-token token)))
         )
        (unless (delphi-is token-kind delphi-whitespace)
          (setq last-token token))
        (setq token (delphi-previous-token token)))
      ;; Start not found.
      nil)))

(defun delphi-else-start (from-else)
  ;; Returns the token of the if or case statement.
  (let ((token (delphi-previous-token from-else))
        (token-kind nil)
        (semicolon-count 0))
    (catch 'done
      (while token
        (setq token-kind (delphi-token-kind token))
        (cond
         ;; Skip over nested groups.
         ((eq 'close-group token-kind) (setq token (delphi-group-start token)))

         ;; Skip over any nested blocks.
         ((delphi-is token-kind delphi-end-block-statements)
          (setq token (delphi-block-start token)))

         ((eq 'semicolon token-kind)
          ;; Semicolon means we are looking for an enclosing if, unless we
          ;; are in a case statement. Keep counts of the semicolons and decide
          ;; later.
          (setq semicolon-count (1+ semicolon-count)))

         ((and (eq 'if token-kind) (= semicolon-count 0))
          ;; We only can match an if when there have been no intervening
          ;; semicolons.
          (throw 'done token))

         ((eq 'case token-kind)
          ;; We have hit a case statement start.
          (throw 'done token)))
        (setq token (delphi-previous-token token)))
      ;; No if or case statement found.
      nil)))

(defun delphi-comment-content-start (comment)
  ;; Returns the point of the first non-space character in the comment.
  (let ((kind (delphi-token-kind comment)))
    (when (delphi-is kind delphi-comments)
      (delphi-save-excursion
       (goto-char (+ (delphi-token-start comment)
                     (length (delphi-literal-start-pattern kind))))
       (skip-chars-forward delphi-space-chars)
       (point)))))

(defun delphi-comment-block-start (comment)
  ;; Returns the starting comment token of a contiguous // comment block. If
  ;; the comment is multiline (i.e. {...} or (*...*)), the original comment is
  ;; returned.
  (if (not (eq 'comment-single-line (delphi-token-kind comment)))
      comment
    ;; Scan until we run out of // comments.
    (let ((prev-comment comment)
          (start-comment comment))
      (while (let ((kind (delphi-token-kind prev-comment)))
               (cond ((eq kind 'space))
                     ((eq kind 'comment-single-line)
                      (setq start-comment prev-comment))
                     (t nil)))
        (setq prev-comment (delphi-previous-token prev-comment)))
      start-comment)))

(defun delphi-comment-block-end (comment)
  ;; Returns the end comment token of a contiguous // comment block. If the
  ;; comment is multiline (i.e. {...} or (*...*)), the original comment is
  ;; returned.
  (if (not (eq 'comment-single-line (delphi-token-kind comment)))
      comment
    ;; Scan until we run out of // comments.
    (let ((next-comment comment)
          (end-comment comment))
      (while (let ((kind (delphi-token-kind next-comment)))
               (cond ((eq kind 'space))
                     ((eq kind 'comment-single-line)
                      (setq end-comment next-comment))
                     (t nil)))
        (setq next-comment (delphi-next-token next-comment)))
      end-comment)))

(defun delphi-on-first-comment-line (comment)
  ;; Returns true if the current point is on the first line of the comment.
  (save-excursion
    (let ((comment-start (delphi-token-start comment))
          (current-point (point)))
      (goto-char comment-start)
      (end-of-line)
      (and (<= comment-start current-point) (<= current-point (point))))))

(defun delphi-comment-indent-of (comment)
  ;; Returns the correct indentation for the comment.
  (let ((start-comment (delphi-comment-block-start comment)))
    (if (and (eq start-comment comment)
             (delphi-on-first-comment-line comment))
        ;; Indent as a statement.
        (delphi-enclosing-indent-of comment)
      (save-excursion
        (let ((kind (delphi-token-kind comment)))
          (beginning-of-line)
          (cond ((eq 'comment-single-line kind)
                 ;; Indent to the first comment in the // block.
                 (delphi-indent-of start-comment))

                ((looking-at (concat delphi-leading-spaces-re
                                     (delphi-literal-stop-pattern kind)))
                 ;; Indent multi-line comment terminators to the comment start.
                 (delphi-indent-of comment))

                ;; Indent according to the comment's content start.
                ((delphi-column-of (delphi-comment-content-start comment)))))))
    ))

(defun delphi-is-use-clause-end (at-token last-token last-colon from-kind)
  ;; True if we are after the end of a uses type clause.
  (when (and last-token
             (not last-colon)
             (eq 'comma (delphi-token-kind at-token))
             (eq 'semicolon from-kind))
    ;; Scan for the uses statement, just to be sure.
    (let ((token (delphi-previous-token at-token))
          (token-kind nil))
      (catch 'done
        (while token
          (setq token-kind (delphi-token-kind token))
          (cond ((delphi-is token-kind delphi-use-clauses)
                 (throw 'done t))

                ;; Whitespace, identifiers, strings, "in" keyword, and commas
                ;; are allowed in use clauses.
                ((or (delphi-is token-kind '(word comma in newline))
                     (delphi-is token-kind delphi-whitespace)
                     (delphi-is token-kind delphi-strings)))

                ;; Nothing else is.
                ((throw 'done nil)))
          (setq token (delphi-previous-token token)))
        nil))))

(defun delphi-is-block-after-expr-statement (token)
  ;; Returns true if we have a block token trailing an expression delimiter (of
  ;; presumably an expression statement).
  (when (delphi-is (delphi-token-kind token) delphi-block-statements)
    (let ((previous (delphi-previous-token token))
          (previous-kind nil))
      (while (progn
               (setq previous-kind (delphi-token-kind previous))
               (eq previous-kind 'space))
        (setq previous (delphi-previous-token previous)))
      (or (delphi-is previous-kind delphi-expr-delimiters)
          (eq previous-kind 'else)))))

(defun delphi-previous-indent-of (from-token)
  ;; Returns the indentation of the previous statement of the token.
  (let ((token (delphi-previous-token from-token))
        (token-kind nil)
        (from-kind (delphi-token-kind from-token))
        (last-colon nil)
        (last-of nil)
        (last-token nil))
    (catch 'done
      (while token
        (setq token-kind (delphi-token-kind token))
        (cond
         ;; An open ( or [ always is an indent point.
         ((eq 'open-group token-kind)
          (throw 'done (delphi-open-group-indent token last-token)))

         ;; Skip over any ()/[] groups.
         ((eq 'close-group token-kind) (setq token (delphi-group-start token)))

         ((delphi-is token-kind delphi-end-block-statements)
          (if (eq 'newline (delphi-token-kind (delphi-previous-token token)))
              ;; We can stop at an end token that is right up against the
              ;; margin.
              (throw 'done 0)
            ;; Otherwise, skip over any nested blocks.
            (setq token (delphi-block-start token))))

         ;; Special case: if we encounter a ", word;" then we assume that we
         ;; are in some kind of uses clause, and thus indent to column 0. This
         ;; works because no other constructs are known to have that form.
         ;; This fixes the irritating case of having indents after a uses
         ;; clause look like:
         ;;   uses
         ;;      someUnit,
         ;;      someOtherUnit;
         ;;      // this should be at column 0!
         ((delphi-is-use-clause-end token last-token last-colon from-kind)
          (throw 'done 0))

         ;; A previous terminator means we can stop. If we are on a directive,
         ;; however, then we are not actually encountering a new statement.
         ((and last-token
               (delphi-is token-kind delphi-previous-terminators)
               (not (delphi-is (delphi-token-kind last-token)
                               delphi-directives)))
          (throw 'done (delphi-stmt-line-indent-of last-token 0)))

         ;; Ignore whitespace.
         ((delphi-is token-kind delphi-whitespace))

         ;; Remember any "of" we encounter, since that affects how we
         ;; indent to a case statement within a record declaration
         ;; (i.e. a variant part).
         ((eq 'of token-kind)
          (setq last-of token))

         ;; Remember any ':' we encounter (until we reach an "of"),
         ;; since that affects how we indent to case statements in
         ;; general.
         ((eq 'colon token-kind)
          (unless last-of (setq last-colon token)))

         ;; A case statement delimits a previous statement. We indent labels
         ;; specially.
         ((eq 'case token-kind)
          (throw 'done
                (if last-colon (delphi-line-indent-of last-colon)
                  (delphi-line-indent-of token delphi-case-label-indent))))

         ;; If we are in a use clause then commas mark an enclosing rather than
         ;; a previous statement.
         ((delphi-is token-kind delphi-use-clauses)
          (throw 'done
                 (if (eq 'comma from-kind)
                     (if last-token
                         ;; Indent to first unit in use clause.
                         (delphi-indent-of last-token)
                       ;; Indent from use clause keyword.
                       (delphi-line-indent-of token delphi-indent-level))
                   ;; Indent to use clause keyword.
                   (delphi-line-indent-of token))))

         ;; Assembly sections always indent in from the asm keyword.
         ((eq token-kind 'asm)
          (throw 'done (delphi-stmt-line-indent-of token delphi-indent-level)))

         ;; An enclosing statement delimits a previous statement.
         ;; We try to use the existing indent of the previous statement,
         ;; otherwise we calculate from the enclosing statement.
         ((delphi-is token-kind delphi-previous-enclosing-statements)
          (throw 'done (if last-token
                           ;; Otherwise indent to the last token
                           (delphi-line-indent-of last-token)
                         ;; Just indent from the enclosing keyword
                         (delphi-line-indent-of token delphi-indent-level))))

         ;; A class or record declaration also delimits a previous statement.
         ((delphi-composite-type-start token last-token)
          (throw
           'done
           (if (delphi-is-simple-class-type last-token from-token)
               ;; c = class; or c = class of T; are previous statements.
               (delphi-line-indent-of token)
             ;; Otherwise c = class ... or r = record ... are enclosing
             ;; statements.
             (delphi-line-indent-of last-token delphi-indent-level))))

         ;; We have a definite previous statement delimiter.
         ((delphi-is token-kind delphi-previous-statements)
          (throw 'done (delphi-stmt-line-indent-of token 0)))
         )
        (unless (delphi-is token-kind delphi-whitespace)
           (setq last-token token))
        (setq token (delphi-previous-token token)))
      ;; We ran out of tokens. Indent to column 0.
      0)))

(defun delphi-section-indent-of (section-token)
  ;; Returns the indentation appropriate for begin/var/const/type/label
  ;; tokens.
  (let* ((token (delphi-previous-token section-token))
         (token-kind nil)
         (last-token nil)
         (nested-block-count 0)
         (expr-delimited nil)
         (last-terminator nil))
    (catch 'done
      (while token
        (setq token-kind (delphi-token-kind token))
        (cond
         ;; Always stop at unmatched ( or [.
         ((eq token-kind 'open-group)
          (throw 'done (delphi-open-group-indent token last-token)))

         ;; Skip over any ()/[] groups.
         ((eq 'close-group token-kind) (setq token (delphi-group-start token)))

         ((delphi-is token-kind delphi-end-block-statements)
          (if (eq 'newline (delphi-token-kind (delphi-previous-token token)))
              ;; We can stop at an end token that is right up against the
              ;; margin.
              (throw 'done 0)
            ;; Otherwise, skip over any nested blocks.
            (setq token (delphi-block-start token)
                  nested-block-count (1+ nested-block-count))))

         ;; Remember if we have encountered any forward routine declarations.
         ((eq 'forward token-kind)
          (setq nested-block-count (1+ nested-block-count)))

         ;; Mark the completion of a nested routine traversal.
         ((and (delphi-is token-kind delphi-routine-statements)
               (> nested-block-count 0))
          (setq nested-block-count (1- nested-block-count)))

         ;; Remember if we have encountered any statement terminators.
         ((eq 'semicolon token-kind) (setq last-terminator token))

         ;; Remember if we have encountered any expression delimiters.
         ((delphi-is token-kind delphi-expr-delimiters)
          (setq expr-delimited token))

         ;; Enclosing body statements are delimiting. We indent the compound
         ;; bodies specially.
         ((and (not last-terminator)
               (delphi-is token-kind delphi-body-statements))
          (throw 'done
           (delphi-stmt-line-indent-of token delphi-compound-block-indent)))

         ;; An enclosing ":" means a label.
         ((and (eq 'colon token-kind)
               (delphi-is (delphi-token-kind section-token)
                          delphi-block-statements)
               (not last-terminator)
               (not expr-delimited)
               (not (eq 'equals (delphi-token-kind last-token))))
          (throw 'done
                 (delphi-stmt-line-indent-of token delphi-indent-level)))

         ;; Block and mid block tokens are always enclosing
         ((delphi-is token-kind delphi-begin-enclosing-tokens)
          (throw 'done
                 (delphi-stmt-line-indent-of token delphi-indent-level)))

         ;; Declaration sections and routines are delimiters, unless they
         ;; are part of a nested routine.
         ((and (delphi-is token-kind delphi-decl-delimiters)
               (= 0 nested-block-count))
          (throw 'done (delphi-line-indent-of token 0)))

         ;; Unit statements mean we indent right to the left.
         ((delphi-is token-kind delphi-unit-statements) (throw 'done 0))
         )
        (unless (delphi-is token-kind delphi-whitespace)
           (setq last-token token))
        (setq token (delphi-previous-token token)))
      ;; We ran out of tokens. Indent to column 0.
      0)))

(defun delphi-enclosing-indent-of (from-token)
  ;; Returns the indentation offset from the enclosing statement of the token.
  (let ((token (delphi-previous-token from-token))
        (from-kind (delphi-token-kind from-token))
        (token-kind nil)
        (stmt-start nil)
        (last-token nil)
        (equals-encountered nil)
        (before-equals nil)
        (expr-delimited nil))
    (catch 'done
      (while token
        (setq token-kind (delphi-token-kind token))
        (cond
         ;; An open ( or [ always is an indent point.
         ((eq 'open-group token-kind)
          (throw 'done
                 (delphi-open-group-indent
                  token last-token
                  (if (delphi-is from-kind delphi-binary-ops)
                      ;; Keep binary operations aligned with the open group.
                      0
                    delphi-indent-level))))

         ;; Skip over any ()/[] groups.
         ((eq 'close-group token-kind) (setq token (delphi-group-start token)))

         ;; Skip over any nested blocks.
         ((delphi-is token-kind delphi-end-block-statements)
          (setq token (delphi-block-start token)))

         ;; An expression delimiter affects indentation depending on whether
         ;; the point is before or after it. Remember that we encountered one.
         ;; Also remember the last encountered token, since if it exists it
         ;; should be the actual indent point.
         ((delphi-is token-kind delphi-expr-delimiters)
          (setq expr-delimited token stmt-start last-token))

         ;; With a non-delimited expression statement we indent after the
         ;; statement's keyword, unless we are on the delimiter itself.
         ((and (not expr-delimited)
               (delphi-is token-kind delphi-expr-statements))
          (throw 'done
             (cond ((delphi-is from-kind delphi-expr-delimiters)
                    ;; We are indenting a delimiter. Indent to the statement.
                    (delphi-stmt-line-indent-of token 0))

                   ((and last-token (delphi-is from-kind delphi-binary-ops))
                    ;; Align binary ops with the expression.
                    (delphi-indent-of last-token))

                   (last-token
                    ;; Indent in from the expression.
                    (delphi-indent-of last-token delphi-indent-level))

           ;; Indent in from the statement's keyword.
           ((delphi-indent-of token delphi-indent-level)))))

         ;; A delimited case statement indents the label according to
         ;; a special rule.
         ((eq 'case token-kind)
          (throw 'done
                 (if stmt-start
                     ;; We are not actually indenting to the case statement,
                     ;; but are within a label expression.
                     (delphi-stmt-line-indent-of
                      stmt-start delphi-indent-level)
                   ;; Indent from the case keyword.
                   (delphi-stmt-line-indent-of
                    token delphi-case-label-indent))))

         ;; Body expression statements are enclosing. Indent from the
         ;; statement's keyword, unless we have a non-block statement following
         ;; it.
         ((delphi-is token-kind delphi-body-expr-statements)
          (throw 'done
                 (delphi-stmt-line-indent-of
                  (or stmt-start token) delphi-indent-level)))

         ;; An else statement is enclosing, but it doesn't have an expression.
         ;; Thus we take into account last-token instead of stmt-start.
         ((eq 'else token-kind)
          (throw 'done (delphi-stmt-line-indent-of
                        (or last-token token) delphi-indent-level)))

         ;; We indent relative to an enclosing declaration section.
         ((delphi-is token-kind delphi-decl-sections)
          (throw 'done (delphi-indent-of (if last-token last-token token)
                                         delphi-indent-level)))

         ;; In unit sections we indent right to the left.
         ((delphi-is token-kind delphi-unit-sections)
          (throw 'done
                 ;; Handle specially the case of "interface", which can be used
                 ;; to start either a unit section or an interface definition.
                 (if (delphi-is token-kind delphi-interface-types)
                     (progn
                       ;; Find the previous non-whitespace token.
                       (while (progn
                                (setq last-token token
                                      token (delphi-previous-token token)
                                      token-kind (delphi-token-kind token))
                                (and token
                                     (delphi-is token-kind
                                                delphi-whitespace))))
                       ;; If this token is an equals sign, "interface" is being
                       ;; used to start an interface definition and we should
                       ;; treat it as a composite type; otherwise, we should
                       ;; consider it the start of a unit section.
                       (if (and token (eq token-kind 'equals))
                           (delphi-line-indent-of last-token
                                                  delphi-indent-level)
                         0))
                   0)))

         ;; A previous terminator means we can stop.
         ((delphi-is token-kind delphi-previous-terminators)
          (throw 'done
              (cond ((and last-token
                          (eq 'comma token-kind)
                          (delphi-is from-kind delphi-binary-ops))
                     ;; Align binary ops with the expression.
                     (delphi-indent-of last-token))

                    (last-token
                     ;; Indent in from the expression.
                     (delphi-indent-of last-token delphi-indent-level))

                    ;; No enclosing expression; use the previous statement's
                    ;; indent.
                    ((delphi-previous-indent-of token)))))

         ;; A block statement after an expression delimiter has its start
         ;; column as the expression statement. E.g.
         ;;    if (a = b)
         ;;       and (a != c) then begin
         ;;       //...
         ;;    end;
         ;; Remember it for when we encounter the expression statement start.
         ((delphi-is-block-after-expr-statement token)
          (throw 'done
           (cond (last-token (delphi-indent-of last-token delphi-indent-level))

                 ((+ (delphi-section-indent-of token) delphi-indent-level)))))

         ;; Assembly sections always indent in from the asm keyword.
         ((eq token-kind 'asm)
          (throw 'done (delphi-stmt-line-indent-of token delphi-indent-level)))

         ;; Stop at an enclosing statement and indent from it.
         ((delphi-is token-kind delphi-enclosing-statements)
          (throw 'done (delphi-stmt-line-indent-of
                        (or last-token token) delphi-indent-level)))

         ;; A class/record declaration is also enclosing.
         ((delphi-composite-type-start token last-token)
          (throw 'done
                 (delphi-line-indent-of last-token delphi-indent-level)))

         ;; A ":" we indent relative to its line beginning.  If we are in a
         ;; parameter list, then stop also if we hit a ";".
         ((and (eq token-kind 'colon)
               (not expr-delimited)
               (not (delphi-is from-kind delphi-expr-delimiters))
               (not equals-encountered)
               (not (eq from-kind 'equals)))
          (throw 'done
           (if last-token
               (delphi-indent-of last-token delphi-indent-level)
             (delphi-line-indent-of token delphi-indent-level 'semicolon))))

         ;; If the ":" was not processed above and we have token after the "=",
         ;; then indent from the "=". Ignore :=, however.
         ((and (eq token-kind 'colon) equals-encountered before-equals)
          (cond
           ;; Ignore binary ops for now. It would do, for example:
           ;;   val := 1 + 2
           ;;          + 3;
           ;; which is good, but also
           ;;   val := Foo
           ;;      (foo, args)
           ;;          + 2;
           ;; which doesn't look right.
           ;;;; Align binary ops with the before token.
           ;;((delphi-is from-kind delphi-binary-ops)
           ;;(throw 'done (delphi-indent-of before-equals 0)))

           ;; Assignments (:=) we skip over to get a normal indent.
           ((eq (delphi-token-kind last-token) 'equals))

           ;; Otherwise indent in from the equals.
           ((throw 'done
                   (delphi-indent-of before-equals delphi-indent-level)))))

         ;; Remember any "=" we encounter if it has not already been processed.
         ((eq token-kind 'equals)
          (setq equals-encountered token
                before-equals last-token))
         )
        (unless (delphi-is token-kind delphi-whitespace)
           (setq last-token token))
        (setq token (delphi-previous-token token)))
      ;; We ran out of tokens. Indent to column 0.
      0)))

(defun delphi-corrected-indentation ()
  ;; Returns the corrected indentation for the current line.
  (delphi-save-excursion
    (delphi-progress-start)
    ;; Move to the first token on the line.
    (beginning-of-line)
    (skip-chars-forward delphi-space-chars)
    (let* ((token (delphi-current-token))
           (token-kind (delphi-token-kind token))
           (indent
            (cond ((eq 'close-group token-kind)
                   ;; Indent to the matching start ( or [.
                   (delphi-indent-of (delphi-group-start token)))

                  ((delphi-is token-kind delphi-unit-statements) 0)

                  ((delphi-is token-kind delphi-comments)
                   ;; In a comment.
                   (delphi-comment-indent-of token))

                  ((delphi-is token-kind delphi-decl-matchers)
                   ;; Use a previous section/routine's indent.
                   (delphi-section-indent-of token))

                  ((delphi-is token-kind delphi-match-block-statements)
                   ;; Use the block's indentation.
                   (let ((block-start
                          (delphi-block-start token 'stop-on-class)))
                     (cond
                      ;; When trailing a body statement, indent to
                      ;; the statement's keyword.
                      ((delphi-is-block-after-expr-statement block-start)
                       (delphi-section-indent-of block-start))

                      ;; Otherwise just indent to the block start.
                      ((delphi-stmt-line-indent-of block-start 0)))))

                  ((eq 'else token-kind)
                   ;; Find the start of the if or case statement.
                   (delphi-stmt-line-indent-of (delphi-else-start token) 0))

                  ;; Otherwise indent in from enclosing statement.
                  ((delphi-enclosing-indent-of
                    (if token token (delphi-token-at (1- (point)))))))))
      (delphi-progress-done)
      indent)))

(defun delphi-indent-line ()
  "Indent the current line according to the current language construct.
If before the indent, the point is moved to the indent."
  (interactive)
  (delphi-save-match-data
   (let ((marked-point (point-marker))  ; Maintain our position reliably.
         (line-start nil)
         (old-indent 0)
         (new-indent 0))
     (beginning-of-line)
     (setq line-start (point))
     (skip-chars-forward delphi-space-chars)
     (setq old-indent (current-column))
     (setq new-indent (delphi-corrected-indentation))
     (if (< marked-point (point))
         ;; If before the indent column, then move to it.
         (set-marker marked-point (point)))
     ;; Advance our marked point after inserted spaces.
     (set-marker-insertion-type marked-point t)
     (when (/= old-indent new-indent)
           (delete-region line-start (point))
           (insert (make-string new-indent ?\s)))
     (goto-char marked-point)
     (set-marker marked-point nil))))

(defvar delphi-mode-abbrev-table nil
  "Abbrev table in use in Delphi mode buffers.")
(define-abbrev-table 'delphi-mode-abbrev-table ())

(defmacro delphi-ensure-buffer (buffer-var buffer-name)
  ;; Ensures there exists a buffer of the specified name in the specified
  ;; variable.
  `(when (not (buffer-live-p ,buffer-var))
     (setq ,buffer-var (get-buffer-create ,buffer-name))))

(defun delphi-log-msg (to-buffer the-msg)
  ;; Writes a message to the end of the specified buffer.
  (with-current-buffer to-buffer
    (save-selected-window
      (switch-to-buffer-other-window to-buffer)
      (goto-char (point-max))
      (set-window-point (get-buffer-window to-buffer) (point))
      (insert the-msg))))

;; Debugging helpers:

(defvar delphi-debug-buffer nil
  "Buffer to write Delphi mode debug messages to.  Created on demand.")

(defun delphi-debug-log (format-string &rest args)
  ;; Writes a message to the log buffer.
  (when delphi-debug
    (delphi-ensure-buffer delphi-debug-buffer "*Delphi Debug Log*")
    (delphi-log-msg delphi-debug-buffer
                    (concat (format-time-string "%H:%M:%S " (current-time))
                            (apply #'format (cons format-string args))
                            "\n"))))

(defun delphi-debug-token-string (token)
  (let* ((image (delphi-token-string token))
         (has-newline (string-match "^\\([^\n]*\\)\n\\(.+\\)?$" image)))
    (when has-newline
       (setq image (concat (match-string 1 image)
                           (if (match-beginning 2) "..."))))
    image))

(defun delphi-debug-show-current-token ()
  (interactive)
  (let ((token (delphi-current-token)))
    (delphi-debug-log "Token: %S %S" token (delphi-debug-token-string token))))

(defun delphi-debug-goto-point (p)
  (interactive "NGoto char: ")
  (goto-char p))

(defun delphi-debug-goto-next-token ()
  (interactive)
  (goto-char (delphi-token-start (delphi-next-token (delphi-current-token)))))

(defun delphi-debug-goto-previous-token ()
  (interactive)
  (goto-char
   (delphi-token-start (delphi-previous-token (delphi-current-token)))))

(defun delphi-debug-show-current-string (from to)
  (interactive "r")
  (delphi-debug-log "String: %S" (buffer-substring from to)))

(defun delphi-debug-show-is-stable ()
  (interactive)
  (delphi-debug-log "stable: %S prev: %S next: %S"
                    (delphi-is-stable-literal (point))
                    (delphi-literal-kind (1- (point)))
                    (delphi-literal-kind (point))))

(defun delphi-debug-unparse-buffer ()
  (interactive)
  (delphi-set-text-properties (point-min) (point-max) nil))

(defun delphi-debug-parse-region (from to)
  (interactive "r")
  (let ((delphi-verbose t))
    (delphi-save-excursion
     (delphi-progress-start)
     (delphi-parse-region from to)
     (delphi-progress-done "Parsing done"))))

(defun delphi-debug-parse-window ()
  (interactive)
  (delphi-debug-parse-region (window-start) (window-end)))

(defun delphi-debug-parse-buffer ()
  (interactive)
  (delphi-debug-parse-region (point-min) (point-max)))

(defun delphi-debug-fontify-window ()
  (interactive)
  (delphi-fontify-region (window-start) (window-end) t))

(defun delphi-debug-fontify-buffer ()
  (interactive)
  (delphi-fontify-region (point-min) (point-max) t))

(defun delphi-debug-tokenize-region (from to)
  (interactive)
  (delphi-save-excursion
   (delphi-progress-start)
   (goto-char from)
   (while (< (point) to)
     (goto-char (delphi-token-end (delphi-current-token)))
     (delphi-step-progress (point) "Tokenizing" delphi-scanning-progress-step))
   (delphi-progress-done "Tokenizing done")))

(defun delphi-debug-tokenize-buffer ()
  (interactive)
  (delphi-debug-tokenize-region (point-min) (point-max)))

(defun delphi-debug-tokenize-window ()
  (interactive)
  (delphi-debug-tokenize-region (window-start) (window-end)))

(defun delphi-newline ()
  "Terminate the current line with a newline and indent the next, unless
`delphi-newline-always-indents' is nil, in which case no reindenting occurs."
  (interactive)
  ;; Remove trailing spaces
  (delete-horizontal-space)
  (newline)
  (when delphi-newline-always-indents
    ;; Indent both the (now) previous and current line first.
    (save-excursion
      (forward-line -1)
      (delphi-indent-line))
    (delphi-indent-line)))


(defun delphi-tab ()
  "Indent the region, when Transient Mark mode is enabled and the region is
active.  Otherwise, indent the current line or insert a TAB, depending on the
value of `delphi-tab-always-indents' and the current line position."
  (interactive)
  (cond ((use-region-p)
         ;; If Transient Mark mode is enabled and the region is active, indent
         ;; the entire region.
         (indent-region (region-beginning) (region-end)))
        ((or delphi-tab-always-indents
             (save-excursion (skip-chars-backward delphi-space-chars) (bolp)))
         ;; Otherwise, if we are configured always to indent (regardless of the
         ;; point's position in the line) or we are before the first non-space
         ;; character on the line, indent the line.
         (delphi-indent-line))
        (t
         ;; Otherwise, insert a tab character.
         (insert "\t"))))


(defun delphi-is-directory (path)
  ;; True if the specified path is an existing directory.
  (let ((attributes (file-attributes path)))
    (and attributes (car attributes))))

(defun delphi-is-file (path)
  ;; True if the specified file exists as a file.
  (let ((attributes (file-attributes path)))
    (and attributes (null (car attributes)))))

(defun delphi-search-directory (unit dir &optional recurse)
  ;; Searches for the unit in the specified directory. If recurse is true, then
  ;; the directory is recursively searched. File name comparison is done in a
  ;; case insensitive manner.
  (when (delphi-is-directory dir)
    (let ((files (directory-files dir))
          (unit-file (downcase unit)))
      (catch 'done
        ;; Search for the file.
        (mapc #'(lambda (file)
		  (let ((path (concat dir "/" file)))
		    (if (and (string= unit-file (downcase file))
			     (delphi-is-file path))
			(throw 'done path))))
	      files)

        ;; Not found. Search subdirectories.
        (when recurse
          (mapc #'(lambda (subdir)
		    (unless (member subdir '("." ".."))
		      (let ((path (delphi-search-directory
				   unit (concat dir "/" subdir) recurse)))
			(if path (throw 'done path)))))
		files))

        ;; Not found.
        nil))))


(defun delphi-find-unit-in-directory (unit dir)
  ;; Searches for the unit in the specified directory. If the directory ends
  ;; in \"...\", then it is recursively searched.
  (let ((dir-name dir)
        (recurse nil))
    ;; Check if we need to recursively search the directory.
    (if (string-match "^\\(.+\\)\\.\\.\\.$" dir-name)
        (setq dir-name (match-string 1 dir-name)
              recurse t))
    ;; Ensure the trailing slash is removed.
    (if (string-match "^\\(.+\\)[\\\\/]$" dir-name)
        (setq dir-name (match-string 1 dir-name)))
    (delphi-search-directory unit dir-name recurse)))

(defun delphi-find-unit-file (unit)
  ;; Finds the specified delphi source file according to `delphi-search-path'.
  ;; If found, the full path is returned, otherwise nil is returned.
  (catch 'done
    (cond ((null delphi-search-path)
           (delphi-find-unit-in-directory unit "."))

          ((stringp delphi-search-path)
           (delphi-find-unit-in-directory unit delphi-search-path))

          ((mapc
              #'(lambda (dir)
                  (let ((file (delphi-find-unit-in-directory unit dir)))
                    (if file (throw 'done file))))
              delphi-search-path)))
    nil))

(defun delphi-find-unit (unit)
  "Find the specified Delphi source file according to `delphi-search-path'.
If no extension is specified, .pas is assumed.  Creates a buffer for the unit."
  (interactive "sDelphi unit name: ")
  (let* ((unit-file (if (string-match "^\\(.*\\)\\.[a-z]+$" unit)
                        unit
                      (concat unit ".pas")))
         (file (delphi-find-unit-file unit-file)))
    (if (null file)
        (error "unit not found: %s" unit-file)
      (find-file file)
      (if (not (derived-mode-p 'delphi-mode))
          (delphi-mode)))
    file))

(defun delphi-find-current-def ()
  "Find the definition of the identifier under the current point."
  (interactive)
  (error "delphi-find-current-def: not implemented yet"))

(defun delphi-find-current-xdef ()
  "Find the definition of the identifier under the current point, searching
in external units if necessary (as listed in the current unit's use clause).
The set of directories to search for a unit is specified by the global variable
`delphi-search-path'."
  (interactive)
  (error "delphi-find-current-xdef: not implemented yet"))

(defun delphi-find-current-body ()
  "Find the body of the identifier under the current point, assuming
it is a routine."
  (interactive)
  (error "delphi-find-current-body: not implemented yet"))

(defun delphi-fill-comment ()
  "Fill the text of the current comment, according to `fill-column'.
An error is raised if not in a comment."
  (interactive)
  (save-excursion
    (save-restriction
    (let* ((comment (delphi-current-token))
           (comment-kind (delphi-token-kind comment)))
      (if (not (delphi-is comment-kind delphi-comments))
          (error "Not in a comment")
        (let* ((start-comment (delphi-comment-block-start comment))
               (end-comment (delphi-comment-block-end comment))
               (comment-start (delphi-token-start start-comment))
               (comment-end (delphi-token-end end-comment))
               (content-start (delphi-comment-content-start start-comment))
               (content-indent (delphi-column-of content-start))
               (content-prefix (make-string content-indent ?\s))
               (content-prefix-re delphi-leading-spaces-re)
               (p nil)
               (marked-point (point-marker))) ; Maintain our position reliably.
          (when (eq 'comment-single-line comment-kind)
            ;; // style comments need more work.
            (setq content-prefix
                  (let ((comment-indent (delphi-column-of comment-start)))
                    (concat (make-string comment-indent ?\s) "//"
                            (make-string (- content-indent comment-indent 2)
                                         ?\s)))
                  content-prefix-re (concat delphi-leading-spaces-re
                                            "//"
                                            delphi-spaces-re)
                  comment-end (if (delphi-is-literal-end comment-end)
                                  ;; Don't include the trailing newline.
                                  (1- comment-end)
                                comment-end)))

          ;; Advance our marked point after inserted spaces.
          (set-marker-insertion-type marked-point t)

          ;; Ensure we can modify the buffer
          (goto-char content-start)
          (insert " ")
          (delete-char -1)

          (narrow-to-region content-start comment-end)

          ;; Strip off the comment prefixes
          (setq p (point-min))
          (while (when (< p (point-max))
                   (goto-char p)
                   (re-search-forward content-prefix-re nil t))
            (replace-match "" nil nil)
            (setq p (1+ (point))))

          ;; add an extra line to prevent the fill from doing it for us.
          (goto-char (point-max))
          (insert "\n")

          ;; Fill the comment contents.
          (let ((fill-column (- fill-column content-indent)))
            (fill-region (point-min) (point-max)))

          (goto-char (point-max))
          (delete-char -1)

          ;; Restore comment prefixes.
          (goto-char (point-min))
          (end-of-line)                 ; Don't reset the first line.
          (setq p (point))
          (while (when (< p (point-max))
                   (goto-char p)
                   (re-search-forward "^" nil t))
            (replace-match content-prefix nil nil)
            (setq p (1+ (point))))

          (setq comment-end (point-max))
          (widen)

          ;; Restore our position
          (goto-char marked-point)
          (set-marker marked-point nil)

          ;; React to the entire fill change as a whole.
          (delphi-progress-start)
          (delphi-parse-region comment-start comment-end)
            (delphi-progress-done)))))))

(defun delphi-new-comment-line ()
  "If in a // comment, do a newline, indented such that one is still in the
comment block.  If not in a // comment, just does a normal newline."
  (interactive)
  (let ((comment (delphi-current-token)))
    (if (not (eq 'comment-single-line (delphi-token-kind comment)))
        ;; Not in a // comment. Just do the normal newline.
        (delphi-newline)
      (let* ((start-comment (delphi-comment-block-start comment))
             (comment-start (delphi-token-start start-comment))
             (content-start (delphi-comment-content-start start-comment))
             (prefix
              (concat (make-string (delphi-column-of comment-start) ?\s) "//"
                      (make-string (- content-start comment-start 2) ?\s))))
        (delete-horizontal-space)
        (newline)
        (insert prefix)))))

(defun delphi-match-token (token limit)
  ;; Sets the match region used by (match-string 0) and friends to the token's
  ;; region.  Sets the current point to the end of the token (or limit).
  (set-match-data nil)
  (if token
      (let ((end (min (delphi-token-end token) limit)))
        (set-match-data (list (delphi-token-start token) end))
        (goto-char end)
        token)))

(defconst delphi-font-lock-defaults
  '(nil ; We have our own fontify routine, so keywords don't apply.
    t ; Syntactic fontification doesn't apply.
    nil ; Don't care about case since we don't use regexps to find tokens.
    nil ; Syntax alists don't apply.
    nil ; Syntax begin movement doesn't apply
    (font-lock-fontify-region-function . delphi-fontify-region)
    (font-lock-verbose . delphi-fontifying-progress-step))
  "Delphi mode font-lock defaults.  Syntactic fontification is ignored.")

(defvar delphi-debug-mode-map
  (let ((kmap (make-sparse-keymap)))
    (mapc #'(lambda (binding) (define-key kmap (car binding) (cadr binding)))
	  '(("n" delphi-debug-goto-next-token)
	    ("p" delphi-debug-goto-previous-token)
	    ("t" delphi-debug-show-current-token)
	    ("T" delphi-debug-tokenize-buffer)
	    ("W" delphi-debug-tokenize-window)
	    ("g" delphi-debug-goto-point)
	    ("s" delphi-debug-show-current-string)
	    ("a" delphi-debug-parse-buffer)
	    ("w" delphi-debug-parse-window)
	    ("f" delphi-debug-fontify-window)
	    ("F" delphi-debug-fontify-buffer)
	    ("r" delphi-debug-parse-region)
	    ("c" delphi-debug-unparse-buffer)
	    ("x" delphi-debug-show-is-stable)
	    ))
    kmap)
  "Keystrokes for Delphi mode debug commands.")

(defvar delphi-mode-map
  (let ((kmap (make-sparse-keymap)))
    (mapc #'(lambda (binding) (define-key kmap (car binding) (cadr binding)))
	  (list '("\r" delphi-newline)
		'("\t" delphi-tab)
		'("\177" backward-delete-char-untabify)
;;              '("\C-cd" delphi-find-current-def)
;;              '("\C-cx" delphi-find-current-xdef)
;;              '("\C-cb" delphi-find-current-body)
		'("\C-cu" delphi-find-unit)
		'("\M-q" delphi-fill-comment)
		'("\M-j" delphi-new-comment-line)
		;; Debug bindings:
		(list "\C-c\C-d" delphi-debug-mode-map)))
    kmap)
  "Keymap used in Delphi mode.")

;;;###autoload
(define-derived-mode delphi-mode prog-mode "Delphi"
  "Major mode for editing Delphi code. \\<delphi-mode-map>
\\[delphi-tab]\t- Indents the current line (or region, if Transient Mark mode
\t  is enabled and the region is active) of Delphi code.
\\[delphi-find-unit]\t- Search for a Delphi source file.
\\[delphi-fill-comment]\t- Fill the current comment.
\\[delphi-new-comment-line]\t- If in a // comment, do a new comment line.

\\[indent-region] also works for indenting a whole region.

Customization:

 `delphi-indent-level'                (default 3)
    Indentation of Delphi statements with respect to containing block.
 `delphi-compound-block-indent'       (default 0)
    Extra indentation for blocks in compound statements.
 `delphi-case-label-indent'           (default 0)
    Extra indentation for case statement labels.
 `delphi-tab-always-indents'          (default t)
    Non-nil means TAB in Delphi mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
 `delphi-newline-always-indents'      (default t)
    Non-nil means NEWLINE in Delphi mode should always reindent the current
    line, insert a blank line and move to the default indent column of the
    blank line.
 `delphi-search-path'                 (default .)
    Directories to search when finding external units.
 `delphi-verbose'                     (default nil)
    If true then Delphi token processing progress is reported to the user.

Coloring:

 `delphi-comment-face'                (default font-lock-comment-face)
    Face used to color Delphi comments.
 `delphi-string-face'                 (default font-lock-string-face)
    Face used to color Delphi strings.
 `delphi-keyword-face'                (default font-lock-keyword-face)
    Face used to color Delphi keywords.
 `delphi-other-face'                  (default nil)
    Face used to color everything else.

Turning on Delphi mode calls the value of the variable `delphi-mode-hook'
with no args, if that value is non-nil."

  ;; Buffer locals:
  (mapc #'(lambda (var)
	    (let ((var-symb (car var))
		  (var-val (cadr var)))
              (set (make-local-variable var-symb) var-val)))
	(list '(indent-line-function delphi-indent-line)
	      '(comment-indent-function delphi-indent-line)
	      '(case-fold-search t)
	      '(delphi-progress-last-reported-point nil)
	      '(delphi-ignore-changes nil)
	      (list 'font-lock-defaults delphi-font-lock-defaults)))

  ;; We need to keep track of changes to the buffer to determine if we need
  ;; to retokenize changed text.
  (add-hook 'after-change-functions 'delphi-after-change nil t)

  (widen)

  (delphi-save-excursion
   (let ((delphi-verbose t))
     (delphi-progress-start)
     (delphi-parse-region (point-min) (point-max))
     (delphi-progress-done)))

  (run-mode-hooks 'delphi-mode-hook))

;;; delphi.el ends here
