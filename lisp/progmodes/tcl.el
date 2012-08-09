;;; tcl.el --- Tcl code editing commands for Emacs

;; Copyright (C) 1994, 1998-2012  Free Software Foundation, Inc.

;; Maintainer: FSF
;; Author: Tom Tromey <tromey@redhat.com>
;;    Chris Lindblad <cjl@lcs.mit.edu>
;; Keywords: languages tcl modes

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

;; BEFORE USE:
;;
;; If you plan to use the interface to the TclX help files, you must
;; set the variable tcl-help-directory-list to point to the topmost
;; directories containing the TclX help files.  Eg:
;;
;;   (setq tcl-help-directory-list '("/usr/local/lib/tclx/help"))
;;
;;; Commentary:

;; CUSTOMIZATION NOTES:
;; * tcl-proc-list can be used to customize a list of things that
;; "define" other things.  Eg in my project I put "defvar" in this
;; list.
;; * tcl-typeword-list is similar, but uses font-lock-type-face.
;; * tcl-keyword-list is a list of keywords.  I've generally used this
;; for flow-control words.  Eg I add "unwind_protect" to this list.
;; * tcl-builtin-list lists commands to be given font-lock-builtin-face.
;; * tcl-type-alist can be used to minimally customize indentation
;; according to context.

;; THANKS FOR CRITICISM AND SUGGESTIONS TO:
;; Guido Bosch <Guido.Bosch@loria.fr>
;; pgs1002@esc.cam.ac.uk (Dr P.G. Sjoerdsma)
;; Mike Scheidler <c23mts@kocrsv01.delcoelect.com>
;; Matt Newman <men@charney.colorado.edu>
;; rwhitby@research.canon.oz.au (Rod Whitby)
;; h9118101@hkuxa.hku.hk (Yip Chi Lap [Beta])
;; Pertti Tapio Kasanen <ptk@delta.hut.fi>
;; schmid@fb3-s7.math.TU-Berlin.DE (Gregor Schmid)
;; warsaw@nlm.nih.gov (Barry A. Warsaw)
;; Carl Witty <cwitty@ai.mit.edu>
;; T. V. Raman <raman@crl.dec.com>
;; Jesper Pedersen <blackie@imada.ou.dk>
;; dfarmer@evolving.com (Doug Farmer)
;; "Chris Alfeld" <calfeld@math.utah.edu>
;; Ben Wing <ben@xemacs.org>

;; KNOWN BUGS:
;; * In Tcl "#" is not always a comment character.  This can confuse tcl.el
;;   in certain circumstances.  For now the only workaround is to use
;;   font-lock which will mark the # chars accordingly or enclose offending
;;   hash characters in quotes or precede them with a backslash.  Note that
;;   using braces won't work -- quotes change the syntax class of characters
;;   between them, while braces do not.  If you don't use font-lock, the
;;   electric-# mode helps alleviate this problem somewhat.
;; * indent-tcl-exp is untested.

;; TODO:
;; * make add-log-tcl-defun smarter.  should notice if we are in the
;;   middle of a defun, or between defuns.  should notice if point is
;;   on first line of defun (or maybe even in comments before defun).
;; * Allow continuation lines to be indented under the first argument
;;   of the preceding line, like this:
;;      [list something \
;;            something-else]
;; * There is a request that indentation work like this:
;;        button .fred -label Fred \
;;                     -command {puts fred}
;; * Should have tcl-complete-symbol that queries the inferior process.
;; * Should have describe-symbol that works by sending the magic
;;   command to a tclX process.
;; * Need C-x C-e binding (tcl-eval-last-exp).
;; * Write indent-region function that is faster than indenting each
;;   line individually.
;; * tcl-figure-type should stop at "beginning of line" (only ws
;;   before point, and no "\" on previous line).  (see tcl-real-command-p).
;; * overrides some comint keybindings; fix.
;; * Trailing \ will eat blank lines.  Should deal with this.
;;   (this would help catch some potential bugs).
;; * Inferior should display in half the screen, not the whole screen.
;; * Indentation should deal with "switch".
;; * Consider writing code to find help files automatically (for
;;   common cases).
;; * `#' shouldn't insert `\#' when point is in string.



;;; Code:

(eval-when-compile
  (require 'imenu)
  (require 'outline)
  (require 'dabbrev)
  (require 'add-log))

(require 'comint)

;;
;; User variables.
;;

(defgroup tcl nil
  "Major mode for editing Tcl source in Emacs."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'languages)

(defcustom tcl-indent-level 4
  "*Indentation of Tcl statements with respect to containing block."
  :type 'integer
  :group 'tcl)
(put 'tcl-indent-level 'safe-local-variable 'integerp)

(defcustom tcl-continued-indent-level 4
  "*Indentation of continuation line relative to first line of command."
  :type 'integer
  :group 'tcl)
(put 'tcl-continued-indent-level 'safe-local-variable 'integerp)

(defcustom tcl-auto-newline nil
  "*Non-nil means automatically newline before and after braces you insert."
  :type 'boolean
  :group 'tcl)

(defcustom tcl-tab-always-indent tab-always-indent
  "*Control effect of TAB key.
If t (the default), always indent current line.
If nil and point is not in the indentation area at the beginning of
the line, a TAB is inserted.
Other values cause the first possible action from the following list
to take place:

  1. Move from beginning of line to correct indentation.
  2. Delete an empty comment.
  3. Move forward to start of comment, indenting if necessary.
  4. Move forward to end of line, indenting if necessary.
  5. Create an empty comment.
  6. Move backward to start of comment, indenting if necessary."
  :type '(choice (const :tag "Always" t)
		 (const :tag "Beginning only" nil)
		 (const :tag "Maybe move or make or delete comment" 'tcl))
  :group 'tcl)


(defcustom tcl-electric-hash-style nil ;; 'smart
  "*Style of electric hash insertion to use.
Possible values are `backslash', meaning that `\\' quoting should be
done; `quote', meaning that `\"' quoting should be done; `smart',
meaning that the choice between `backslash' and `quote' should be
made depending on the number of hashes inserted; or nil, meaning that
no quoting should be done.  Any other value for this variable is
taken to mean `smart'.  The default is nil."
  :type '(choice (const backslash) (const quote) (const smart) (const nil))
  :group 'tcl)

(defcustom tcl-help-directory-list nil
  "*List of topmost directories containing TclX help files."
  :type '(repeat directory)
  :group 'tcl)

(defcustom tcl-use-smart-word-finder t
  "*If not nil, use smart way to find current word, for Tcl help feature."
  :type 'boolean
  :group 'tcl)

(defcustom tcl-application "wish"
  "*Name of Tcl program to run in inferior Tcl mode."
  :type 'string
  :group 'tcl)

(defcustom tcl-command-switches nil
  "*List of switches to supply to the `tcl-application' program."
  :type '(repeat string)
  :group 'tcl)

(defcustom tcl-prompt-regexp "^\\(% \\|\\)"
  "*If not nil, a regexp that will match the prompt in the inferior process.
If nil, the prompt is the name of the application with \">\" appended.

The default is \"^\\(% \\|\\)\", which will match the default primary
and secondary prompts for tclsh and wish."
  :type 'regexp
  :group 'tcl)

(defcustom inferior-tcl-source-command "source %s\n"
  "*Format-string for building a Tcl command to load a file.
This format string should use `%s' to substitute a file name
and should result in a Tcl expression that will command the
inferior Tcl to load that file.  The filename will be appropriately
quoted for Tcl."
  :type 'string
  :group 'tcl)

(defface tcl-escaped-newline '((t :inherit font-lock-string-face))
  "Face used for (non-escaped) backslash at end of a line in Tcl mode."
  :group 'tcl
  :version "22.1")

;;
;; Keymaps, abbrevs, syntax tables.
;;

(defvar tcl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "{" 'tcl-electric-char)
    (define-key map "}" 'tcl-electric-brace)
    (define-key map "[" 'tcl-electric-char)
    (define-key map "]" 'tcl-electric-char)
    (define-key map ";" 'tcl-electric-char)
    (define-key map "#" 'tcl-electric-hash) ;Remove?  -stef
    (define-key map "\e\C-q" 'tcl-indent-exp)
    (define-key map "\177" 'backward-delete-char-untabify)
    (define-key map "\t" 'tcl-indent-command)
    (define-key map "\M-\C-x" 'tcl-eval-defun)
    (define-key map "\C-c\C-i" 'tcl-help-on-word)
    (define-key map "\C-c\C-v" 'tcl-eval-defun)
    (define-key map "\C-c\C-f" 'tcl-load-file)
    (define-key map "\C-c\C-t" 'inferior-tcl)
    (define-key map "\C-c\C-x" 'tcl-eval-region)
    (define-key map "\C-c\C-s" 'switch-to-tcl)
    map)
  "Keymap used in `tcl-mode'.")

(defvar tcl-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?%  "_"  st)
    (modify-syntax-entry ?@  "_"  st)
    (modify-syntax-entry ?&  "_"  st)
    (modify-syntax-entry ?*  "_"  st)
    (modify-syntax-entry ?+  "_"  st)
    (modify-syntax-entry ?-  "_"  st)
    (modify-syntax-entry ?.  "_"  st)
    (modify-syntax-entry ?:  "_"  st)
    (modify-syntax-entry ?!  "_"  st)
    (modify-syntax-entry ?$  "_"  st)	; FIXME use "'"?
    (modify-syntax-entry ?/  "_"  st)
    (modify-syntax-entry ?~  "_"  st)
    (modify-syntax-entry ?<  "_"  st)
    (modify-syntax-entry ?=  "_"  st)
    (modify-syntax-entry ?>  "_"  st)
    (modify-syntax-entry ?|  "_"  st)
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?\; "."  st)
    (modify-syntax-entry ?\n ">"  st)
    ;; (modify-syntax-entry ?\f ">"  st)
    (modify-syntax-entry ?#  "<"  st)
    st)
  "Syntax table in use in `tcl-mode' buffers.")

(defvar inferior-tcl-mode-map
  ;; FIXME we override comint keybindings here.
  ;; Maybe someone has a better set?
  (let ((map (make-sparse-keymap)))
    ;; Will inherit from `comint-mode-map' thanks to define-derived-mode.
    (define-key map "\t" 'comint-dynamic-complete)
    (define-key map "\M-?" 'comint-dynamic-list-filename-completions)
    (define-key map "\177" 'backward-delete-char-untabify)
    (define-key map "\M-\C-x" 'tcl-eval-defun)
    (define-key map "\C-c\C-i" 'tcl-help-on-word)
    (define-key map "\C-c\C-v" 'tcl-eval-defun)
    (define-key map "\C-c\C-f" 'tcl-load-file)
    (define-key map "\C-c\C-t" 'inferior-tcl)
    (define-key map "\C-c\C-x" 'tcl-eval-region)
    (define-key map "\C-c\C-s" 'switch-to-tcl)
    map)
  "Keymap used in `inferior-tcl-mode'.")

(easy-menu-define tcl-mode-menu tcl-mode-map "Menu used in `tcl-mode'."
  '("Tcl"
    ["Beginning of function" beginning-of-defun t]
    ["End of function" end-of-defun t]
    ["Mark function" mark-defun t]
    ["Indent region" indent-region (mark t)]
    ["Comment region" comment-region (mark t)]
    ["Uncomment region" uncomment-region (mark t)]
    "----"
    ["Show Tcl process buffer" inferior-tcl t]
    ["Send function to Tcl process" tcl-eval-defun
     (and inferior-tcl-buffer (get-buffer inferior-tcl-buffer))]
    ["Send region to Tcl process" tcl-eval-region
     (and inferior-tcl-buffer (get-buffer inferior-tcl-buffer))]
    ["Send file to Tcl process" tcl-load-file
     (and inferior-tcl-buffer (get-buffer inferior-tcl-buffer))]
    ["Restart Tcl process with file" tcl-restart-with-file t]
    "----"
    ["Tcl help" tcl-help-on-word tcl-help-directory-list]))

(defvar inferior-tcl-buffer nil
  "*The current inferior-tcl process buffer.

MULTIPLE PROCESS SUPPORT
===========================================================================
To run multiple Tcl processes, you start the first up with
\\[inferior-tcl].  It will be in a buffer named `*inferior-tcl*'.
Rename this buffer with \\[rename-buffer].  You may now start up a new
process with another \\[inferior-tcl].  It will be in a new buffer,
named `*inferior-tcl*'.  You can switch between the different process
buffers with \\[switch-to-buffer].

Commands that send text from source buffers to Tcl processes -- like
`tcl-eval-defun' or `tcl-load-file' -- have to choose a process to
send to, when you have more than one Tcl process around.  This is
determined by the global variable `inferior-tcl-buffer'.  Suppose you
have three inferior Lisps running:
    Buffer              Process
    foo                 inferior-tcl
    bar                 inferior-tcl<2>
    *inferior-tcl*      inferior-tcl<3>
If you do a \\[tcl-eval-defun] command on some Lisp source code, what
process do you send it to?

- If you're in a process buffer (foo, bar, or *inferior-tcl*),
  you send it to that process.
- If you're in some other buffer (e.g., a source file), you
  send it to the process attached to buffer `inferior-tcl-buffer'.
This process selection is performed by function `inferior-tcl-proc'.

Whenever \\[inferior-tcl] fires up a new process, it resets
`inferior-tcl-buffer' to be the new process's buffer.  If you only run
one process, this does the right thing.  If you run multiple
processes, you can change `inferior-tcl-buffer' to another process
buffer with \\[set-variable].")

;;
;; Hooks and other customization.
;;

(defvar tcl-mode-hook nil
  "Hook run on entry to Tcl mode.

Several functions exist which are useful to run from your
`tcl-mode-hook' (see each function's documentation for more
information):

  `tcl-guess-application'
    Guesses a default setting for `tcl-application' based on any
    \"#!\" line at the top of the file.
  `tcl-hashify-buffer'
    Quotes all \"#\" characters that don't correspond to actual
    Tcl comments.  (Useful when editing code not originally created
    with this mode).
  `tcl-auto-fill-mode'
    Auto-filling of Tcl comments.

Add functions to the hook with `add-hook':

   (add-hook 'tcl-mode-hook 'tcl-guess-application)")


(defvar tcl-proc-list
  '("proc" "method" "itcl_class" "body" "configbody" "class")
  "List of commands whose first argument defines something.
This exists because some people (eg, me) use `defvar' et al.
Call `tcl-set-proc-regexp' and `tcl-set-font-lock-keywords'
after changing this list.")

(defvar tcl-proc-regexp nil
  "Regexp to use when matching proc headers.")

(defvar tcl-typeword-list
  '("global" "upvar" "inherit" "public" "protected" "private"
    "common" "itk_option" "variable")
  "List of Tcl keywords denoting \"type\".  Used only for highlighting.
Call `tcl-set-font-lock-keywords' after changing this list.")

;; Generally I've picked control operators to be keywords.
(defvar tcl-keyword-list
  '("if" "then" "else" "elseif" "for" "foreach" "break" "continue" "while"
    "eval" "case" "in" "switch" "default" "exit" "error" "proc" "return"
    "uplevel" "constructor" "destructor" "itcl_class" "loop" "for_array_keys"
    "for_recursive_glob" "for_file" "method" "body" "configbody" "class"
    "chain")
  "List of Tcl keywords.  Used only for highlighting.
Default list includes some TclX keywords.
Call `tcl-set-font-lock-keywords' after changing this list.")

(defvar tcl-builtin-list
  '("after" "append" "array" "bgerror" "binary" "catch" "cd" "clock"
    "close" "concat" "console" "dde" "encoding" "eof" "exec" "expr"
    "fblocked" "fconfigure" "fcopy" "file" "fileevent" "flush"
    "format" "gets" "glob" "history" "incr" "info" "interp" "join"
    "lappend" "lindex" "linsert" "list" "llength" "load" "lrange"
    "lreplace" "lsort" "namespace" "open" "package" "pid" "puts" "pwd"
    "read" "regexp" "registry" "regsub" "rename" "scan" "seek" "set"
    "socket" "source" "split" "string" "subst" "tell" "time" "trace"
    "unknown" "unset" "vwait")
  "List of Tcl commands.  Used only for highlighting.
Call `tcl-set-font-lock-keywords' after changing this list.
This list excludes those commands already found in `tcl-proc-list' and
`tcl-keyword-list'.")

(defvar tcl-font-lock-keywords nil
  "Keywords to highlight for Tcl.  See variable `font-lock-keywords'.
This variable is generally set from `tcl-proc-regexp',
`tcl-typeword-list', and `tcl-keyword-list' by the function
`tcl-set-font-lock-keywords'.")

(defconst tcl-syntax-propertize-function
  (syntax-propertize-rules
   ;; Mark the few `#' that are not comment-markers.
   ("[^;[{ \t\n][ \t]*\\(#\\)" (1 ".")))
  "Syntactic keywords for `tcl-mode'.")

;; FIXME need some way to recognize variables because array refs look
;; like 2 sexps.
(defvar tcl-type-alist
  '(("proc" nil tcl-expr tcl-commands)
    ("method" nil tcl-expr tcl-commands)
    ("destructor" tcl-commands)
    ("constructor" tcl-commands)
    ("expr" tcl-expr)
    ("catch" tcl-commands)
    ("if" tcl-expr "then" tcl-commands)
    ("elseif" tcl-expr "then" tcl-commands)
    ("elseif" tcl-expr tcl-commands)
    ("if" tcl-expr tcl-commands)
    ("while" tcl-expr tcl-commands)
    ("for" tcl-commands tcl-expr tcl-commands tcl-commands)
    ("foreach" nil nil tcl-commands)
    ("for_file" nil nil tcl-commands)
    ("for_array_keys" nil nil tcl-commands)
    ("for_recursive_glob" nil nil nil tcl-commands)
    ;; Loop handling is not perfect, because the third argument can be
    ;; either a command or an expr, and there is no real way to look
    ;; forward.
    ("loop" nil tcl-expr tcl-expr tcl-commands)
    ("loop" nil tcl-expr tcl-commands))
  "Alist that controls indentation.
\(Actually, this really only controls what happens on continuation lines).
Each entry looks like `(KEYWORD TYPE ...)'.
Each type entry describes a sexp after the keyword, and can be one of:
* nil, meaning that this sexp has no particular type.
* tcl-expr, meaning that this sexp is an arithmetic expression.
* tcl-commands, meaning that this sexp holds Tcl commands.
* a string, which must exactly match the string at the corresponding
  position for a match to be made.

For example, the entry for the \"loop\" command is:

   (\"loop\" nil tcl-expr tcl-commands)

This means that the \"loop\" command has three arguments.  The first
argument is ignored (for indentation purposes).  The second argument
is a Tcl expression, and the last argument is Tcl commands.")

(defvar tcl-explain-indentation nil
  "If non-nil, debugging message will be printed during indentation.")



;; Here's another stab.  I think this one actually works.
;; We have to be careful that the open-brace following this regexp
;; is indeed the one corresponding to the function's body so
;; that end-of-defun works correctly.  Tricky cases are:
;;    proc foo { {arg1 def} arg2 } {
;; as well as
;;    proc foo { \n {arg1 def} \n arg2 } {
;; The current setting handles the first case properly but not the second.
;; It also fails if `proc' is not in column-0 (e.g. it's in a namespace).
(defconst tcl-omit-ws-regexp "^[^]\" \t\n#}][^\n\"#]+[ \t]+")



;;
;; Some helper functions.
;;

(defun tcl-set-proc-regexp ()
  "Set `tcl-proc-regexp' from variable `tcl-proc-list'."
  (setq tcl-proc-regexp
	(concat "^\\s-*" (regexp-opt tcl-proc-list t) "[ \t]+")))

(defun tcl-set-font-lock-keywords ()
  "Set `tcl-font-lock-keywords'.
Uses variables `tcl-proc-regexp' and `tcl-keyword-list'."
  (setq tcl-font-lock-keywords
	(list
	 ;; Names of functions (and other "defining things").
	 (list (concat tcl-proc-regexp "\\([^ \t\n]+\\)")
	       2 'font-lock-function-name-face)

	 ;; Names of type-defining things.
	 (list (concat "\\(\\s-\\|^\\)"
		       (regexp-opt tcl-typeword-list t)
		       "\\(\\s-\\|$\\)")
	       2 'font-lock-type-face)

         (list (concat "\\_<" (regexp-opt tcl-builtin-list t) "\\_>")
	       1 'font-lock-builtin-face)

         ;; When variable names are enclosed in {} braces, any
         ;; character can be used. Otherwise just letters, digits,
         ;; underscores.  Variable names can be prefixed with any
         ;; number of "namespace::" qualifiers.  A leading "::" refers
         ;; to the global namespace.
         '("\\${\\([^}]+\\)}" 1 font-lock-variable-name-face)
         '("\\$\\(\\(?:::\\)?\\(?:[[:alnum:]_]+::\\)*[[:alnum:]_]+\\)"
           1 font-lock-variable-name-face)
         '("\\(?:\\s-\\|^\\|\\[\\)set\\s-+{\\([^}]+\\)}"
           1 font-lock-variable-name-face keep)
         '("\\(?:\\s-\\|^\\|\\[\\)set\\s-+\\(\\(?:::\\)?\
\\(?:[[:alnum:]_]+::\\)*[[:alnum:]_]+\\)"
           1 font-lock-variable-name-face keep)

         '("\\(^\\|[^\\]\\)\\(\\\\\\\\\\)*\\(\\\\\\)$" 3 'tcl-escaped-newline)

	 ;; Keywords.  Only recognized if surrounded by whitespace.
	 ;; FIXME consider using "not word or symbol", not
	 ;; "whitespace".
	 (cons (concat "\\_<" (regexp-opt tcl-keyword-list t) "\\_>")
	       1))))

(if tcl-proc-regexp
    ()
  (tcl-set-proc-regexp))

(if tcl-font-lock-keywords
    ()
  (tcl-set-font-lock-keywords))


(defvar tcl-imenu-generic-expression
  `((nil ,(concat tcl-proc-regexp "\\([-A-Za-z0-9_:+*]+\\)") 2))
  "Imenu generic expression for `tcl-mode'.  See `imenu-generic-expression'.")



;;
;; The mode itself.
;;

;;;###autoload
(define-derived-mode tcl-mode prog-mode "Tcl"
  "Major mode for editing Tcl code.
Expression and list commands understand all Tcl brackets.
Tab indents for Tcl code.
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.

Variables controlling indentation style:
  `tcl-indent-level'
    Indentation of Tcl statements within surrounding block.
  `tcl-continued-indent-level'
    Indentation of continuation line relative to first line of command.

Variables controlling user interaction with mode (see variable
documentation for details):
  `tcl-tab-always-indent'
    Controls action of TAB key.
  `tcl-auto-newline'
    Non-nil means automatically newline before and after braces, brackets,
    and semicolons inserted in Tcl code.
  `tcl-use-smart-word-finder'
    If not nil, use a smarter, Tcl-specific way to find the current
    word when looking up help on a Tcl command.

Turning on Tcl mode runs `tcl-mode-hook'.  Read the documentation for
`tcl-mode-hook' to see what kinds of interesting hook functions
already exist."
  (unless (and (boundp 'filladapt-mode) filladapt-mode)
    (set (make-local-variable 'paragraph-ignore-fill-prefix) t))

  (set (make-local-variable 'indent-line-function) 'tcl-indent-line)
  (set (make-local-variable 'comment-indent-function) 'tcl-comment-indent)
  ;; Tcl doesn't require a final newline.
  ;; (make-local-variable 'require-final-newline)
  ;; (setq require-final-newline t)

  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip)
       "\\(\\(^\\|[;{[]\\)\\s-*\\)#+ *")
  (set (make-local-variable 'comment-end) "")

  (set (make-local-variable 'outline-regexp) ".")
  (set (make-local-variable 'outline-level) 'tcl-outline-level)

  (set (make-local-variable 'font-lock-defaults)
       '(tcl-font-lock-keywords nil nil nil beginning-of-defun))
  (set (make-local-variable 'syntax-propertize-function)
       tcl-syntax-propertize-function)

  (set (make-local-variable 'imenu-generic-expression)
       tcl-imenu-generic-expression)

  ;; Settings for new dabbrev code.
  (set (make-local-variable 'dabbrev-case-fold-search) nil)
  (set (make-local-variable 'dabbrev-case-replace) nil)
  (set (make-local-variable 'dabbrev-abbrev-skip-leading-regexp) "[$!]")
  (set (make-local-variable 'dabbrev-abbrev-char-regexp) "\\sw\\|\\s_")

  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  ;; XEmacs has defun-prompt-regexp, but I don't believe
  ;; that it works for end-of-defun -- only for
  ;; beginning-of-defun.
  (set (make-local-variable 'defun-prompt-regexp) tcl-omit-ws-regexp)
  (set (make-local-variable 'add-log-current-defun-function)
       'tcl-add-log-defun)

  (easy-menu-add tcl-mode-menu)
  ;; Append Tcl menu to popup menu for XEmacs.
  (if (boundp 'mode-popup-menu)
      (setq mode-popup-menu
	    (cons (concat mode-name " Mode Commands") tcl-mode-menu))))



;; This is used for braces, brackets, and semi (except for closing
;; braces, which are handled specially).
(defun tcl-electric-char (arg)
  "Insert character and correct line's indentation."
  (interactive "p")
  ;; Indent line first; this looks better if parens blink.
  (tcl-indent-line)
  (self-insert-command arg)
  (if (and tcl-auto-newline (= last-command-event ?\;))
      (progn
	(newline)
	(tcl-indent-line))))

;; This is used for closing braces.  If tcl-auto-newline is set, can
;; insert a newline both before and after the brace, depending on
;; context.  FIXME should this be configurable?  Does anyone use this?
(defun tcl-electric-brace (arg)
  "Insert character and correct line's indentation."
  (interactive "p")
  ;; If auto-newlining and there is stuff on the same line, insert a
  ;; newline first.
  (if tcl-auto-newline
      (progn
	(if (save-excursion
	      (skip-chars-backward " \t")
	      (bolp))
	    ()
	  (tcl-indent-line)
	  (newline))
	;; In auto-newline case, must insert a newline after each
	;; brace.  So an explicit loop is needed.
	(while (> arg 0)
	  (insert last-command-event)
	  (tcl-indent-line)
	  (newline)
	  (setq arg (1- arg))))
    (self-insert-command arg))
  (tcl-indent-line))



(defun tcl-indent-command (&optional _arg)
  "Indent current line as Tcl code, or in some cases insert a tab character.
If `tcl-tab-always-indent' is t (the default), always indent current line.
If `tcl-tab-always-indent' is nil and point is not in the indentation
area at the beginning of the line, a TAB is inserted.
Other values of `tcl-tab-always-indent' cause the first possible action
from the following list to take place:

  1. Move from beginning of line to correct indentation.
  2. Delete an empty comment.
  3. Move forward to start of comment, indenting if necessary.
  4. Move forward to end of line, indenting if necessary.
  5. Create an empty comment.
  6. Move backward to start of comment, indenting if necessary."
  (interactive "p")
  (if (memq tcl-tab-always-indent '(nil t))
      (let ((tab-always-indent tcl-tab-always-indent))
        (call-interactively 'indent-for-tab-command))
    ;; "Perl-mode" style TAB command.
    (let* ((ipoint (point))
	   (eolpoint (progn
		       (end-of-line)
		       (point)))
	   (comment-p (tcl-in-comment)))
      (cond
       ((= ipoint (line-beginning-position))
	(beginning-of-line)
	(tcl-indent-line)
	;; If indenting didn't leave us in column 0, go to the
	;; indentation.  Otherwise leave point at end of line.  This
	;; is a hack.
	(if (= (point) (line-beginning-position))
	    (end-of-line)
	  (back-to-indentation)))
       ((and comment-p (looking-at "[ \t]*$"))
	;; Empty comment, so delete it.  We also delete any ";"
	;; characters at the end of the line.  I think this is
	;; friendlier, but I don't know how other people will feel.
	(backward-char)
	(skip-chars-backward " \t;")
	(delete-region (point) eolpoint))
       ((and comment-p (< ipoint (point)))
	;; Before comment, so skip to it.
	(tcl-indent-line)
	(indent-for-comment))
       ((/= ipoint eolpoint)
	;; Go to end of line (since we're not there yet).
	(goto-char eolpoint)
	(tcl-indent-line))
       ((not comment-p)
	(tcl-indent-line)
	(comment-indent))
       (t
	;; Go to start of comment.  We don't leave point where it is
	;; because we want to skip comment-start-skip.
	(tcl-indent-line)
	(indent-for-comment))))))

(defun tcl-indent-line ()
  "Indent current line as Tcl code.
Return the amount the indentation changed by."
  (let ((indent (tcl-calculate-indent nil))
	beg shift-amt
	(case-fold-search nil)
	(pos (- (point-max) (point))))
    (if (null indent)
        'noindent
      (beginning-of-line)
      (setq beg (point))
      (skip-chars-forward " \t")
      (if (listp indent) (setq indent (car indent)))
      (cond ((= (following-char) ?})
             (setq indent (- indent tcl-indent-level)))
            ((= (following-char) ?\])
             (setq indent (- indent 1))))
      (skip-chars-forward " \t")
      (setq shift-amt (- indent (current-column)))
      (if (zerop shift-amt)
          (if (> (- (point-max) pos) (point))
              (goto-char (- (point-max) pos)))
        (delete-region beg (point))
        (indent-to indent)
        ;; If initial point was within line's indentation,
        ;; position after the indentation.  Else stay at same point in text.
        (if (> (- (point-max) pos) (point))
            (goto-char (- (point-max) pos))))
      shift-amt)))

(defun tcl-figure-type ()
  "Determine type of sexp at point.
This is either `tcl-expr', `tcl-commands', or nil.  Puts point at start
of sexp that indicates types.

See documentation for variable `tcl-type-alist' for more information."
  (let ((count 0)
	result
	word-stack)
    (while (and (< count 5)
		(not result))
      (condition-case nil
	  (progn
	    ;; FIXME should use "tcl-backward-sexp", which would skip
	    ;; over entire variables, etc.
	    (backward-sexp)
	    (if (looking-at "[a-zA-Z_]+")
		(let ((list tcl-type-alist)
		      entry)
		  (setq word-stack (cons (tcl-word-no-props) word-stack))
		  (while (and list (not result))
		    (setq entry (car list))
		    (setq list (cdr list))
		    (let ((index 0))
		      (while (and entry (<= index count))
			;; Abort loop if string does not match word on
			;; stack.
			(and (stringp (car entry))
			     (not (string= (car entry)
					   (nth index word-stack)))
			     (setq entry nil))
			(setq entry (cdr entry))
			(setq index (1+ index)))
		      (and (> index count)
			   (not (stringp (car entry)))
			   (setq result (car entry)))
		      )))
	      (setq word-stack (cons nil word-stack))))
	(error nil))
      (setq count (1+ count)))
    (and tcl-explain-indentation
	 (message "Indentation type %s" result))
    result))

(defun tcl-calculate-indent (&optional parse-start)
  "Return appropriate indentation for current line as Tcl code.
In usual case returns an integer: the column to indent to.
Returns nil if line starts inside a string, t if in a comment."
  (save-excursion
    (beginning-of-line)
    (let* ((indent-point (point))
	   (case-fold-search nil)
	   (continued-line
	    (save-excursion
	      (if (bobp)
		  nil
		(backward-char)
		(= ?\\ (preceding-char)))))
	   (continued-indent-value (if continued-line
				       tcl-continued-indent-level
				     0))
	   state
	   containing-sexp
	   found-next-line)
      (if parse-start
	  (goto-char parse-start)
	(beginning-of-defun))
      (while (< (point) indent-point)
	(setq parse-start (point))
	(setq state (parse-partial-sexp (point) indent-point 0))
	(setq containing-sexp (car (cdr state))))
      (cond ((or (nth 3 state) (nth 4 state))
	     ;; Inside comment or string.  Return nil or t if should
	     ;; not change this line
	     (nth 4 state))
	    ((null containing-sexp)
	     ;; Line is at top level.
	     continued-indent-value)
	    (t
	     ;; Set expr-p if we are looking at the expression part of
	     ;; an "if", "expr", etc statement.  Set commands-p if we
	     ;; are looking at the body part of an if, while, etc
	     ;; statement.  FIXME Should check for "for" loops here.
	     (goto-char containing-sexp)
	     (let* ((sexpr-type (tcl-figure-type))
		    (expr-p (eq sexpr-type 'tcl-expr))
		    (commands-p (eq sexpr-type 'tcl-commands))
		    (expr-start (point)))
	       ;; Find the first statement in the block and indent
	       ;; like it.  The first statement in the block might be
	       ;; on the same line, so what we do is skip all
	       ;; "virtually blank" lines, looking for a non-blank
	       ;; one.  A line is virtually blank if it only contains
	       ;; a comment and whitespace.  FIXME continued comments
	       ;; aren't supported.  They are a wart on Tcl anyway.
	       ;; We do it this funky way because we want to know if
	       ;; we've found a statement on some line _after_ the
	       ;; line holding the sexp opener.
	       (goto-char containing-sexp)
	       (forward-char)
	       (if (and (< (point) indent-point)
			(looking-at "[ \t]*\\(#.*\\)?$"))
		   (progn
		     (forward-line)
		     (while (and (< (point) indent-point)
				 (looking-at "[ \t]*\\(#.*\\)?$"))
		       (setq found-next-line t)
		       (forward-line))))
	       (if (or continued-line
		       (/= (char-after containing-sexp) ?{)
		       expr-p)
		   (progn
		     ;; Line is continuation line, or the sexp opener
		     ;; is not a curly brace, or we are looking at
		     ;; an `expr' expression (which must be split
		     ;; specially).  So indentation is column of first
		     ;; good spot after sexp opener (with some added
		     ;; in the continued-line case).  If there is no
		     ;; nonempty line before the indentation point, we
		     ;; use the column of the character after the sexp
		     ;; opener.
		     (if (>= (point) indent-point)
			 (progn
			   (goto-char containing-sexp)
			   (forward-char))
		       (skip-chars-forward " \t"))
		     (+ (current-column) continued-indent-value))
		 ;; After a curly brace, and not a continuation line.
		 ;; So take indentation from first good line after
		 ;; start of block, unless that line is on the same
		 ;; line as the opening brace.  In this case use the
		 ;; indentation of the opening brace's line, plus
		 ;; another indent step.  If we are in the body part
		 ;; of an "if" or "while" then the indentation is
		 ;; taken from the line holding the start of the
		 ;; statement.
		 (if (and (< (point) indent-point)
			  found-next-line)
		     (current-indentation)
		   (if commands-p
		       (goto-char expr-start)
		     (goto-char containing-sexp))
		   (+ (current-indentation) tcl-indent-level)))))))))



(defun tcl-indent-exp ()
  "Indent each line of the Tcl grouping following point."
  (interactive)
  (let ((indent-stack (list nil))
	(contain-stack (list (point)))
	(case-fold-search nil)
	outer-loop-done inner-loop-done state ostate
	this-indent continued-line
	(next-depth 0)
	last-depth)
    (save-excursion
      (forward-sexp 1))
    (save-excursion
      (setq outer-loop-done nil)
      (while (and (not (eobp)) (not outer-loop-done))
	(setq last-depth next-depth)
	;; Compute how depth changes over this line
	;; plus enough other lines to get to one that
	;; does not end inside a comment or string.
	;; Meanwhile, do appropriate indentation on comment lines.
	(setq inner-loop-done nil)
	(while (and (not inner-loop-done)
		    (not (and (eobp) (setq outer-loop-done t))))
	  (setq ostate state)
	  (setq state (parse-partial-sexp (point) (progn (end-of-line) (point))
					  nil nil state))
	  (setq next-depth (car state))
	  (if (or (nth 4 ostate))
	      (tcl-indent-line))
	  (if (or (nth 3 state))
	      (forward-line 1)
	    (setq inner-loop-done t)))
	(if (<= next-depth 0)
	    (setq outer-loop-done t))
	(if outer-loop-done
	    nil
	  ;; If this line had ..))) (((.. in it, pop out of the levels
	  ;; that ended anywhere in this line, even if the final depth
	  ;; doesn't indicate that they ended.
	  (while (> last-depth (nth 6 state))
	    (setq indent-stack (cdr indent-stack)
		  contain-stack (cdr contain-stack)
		  last-depth (1- last-depth)))
	  ;; Add levels for any parens that were started in this line.
	  (while (< last-depth next-depth)
	    (setq indent-stack (cons nil indent-stack)
		  contain-stack (cons nil contain-stack)
		  last-depth (1+ last-depth)))
	  (if (null (car contain-stack))
	      (setcar contain-stack
		      (or (car (cdr state))
			  (save-excursion
			    (forward-sexp -1)
			    (point)))))
	  (forward-line 1)
	  (setq continued-line
		(save-excursion
		  (backward-char)
		  (= (preceding-char) ?\\)))
	  (skip-chars-forward " \t")
	  (if (eolp)
	      nil
	    (if (and (car indent-stack)
		     (>= (car indent-stack) 0))
		;; Line is on an existing nesting level.
		(setq this-indent (car indent-stack))
	      ;; Just started a new nesting level.
	      ;; Compute the standard indent for this level.
	      (let ((val (tcl-calculate-indent
			  (if (car indent-stack)
			      (- (car indent-stack))))))
		(setcar indent-stack
			(setq this-indent val))
		(setq continued-line nil)))
	    (cond ((not (numberp this-indent)))
		  ((= (following-char) ?})
		   (setq this-indent (- this-indent tcl-indent-level)))
		  ((= (following-char) ?\])
		   (setq this-indent (- this-indent 1))))
	    ;; Put chosen indentation into effect.
	    (or (null this-indent)
		(= (current-column)
		   (if continued-line
		       (+ this-indent tcl-indent-level)
		     this-indent))
		(progn
		  (delete-region (point) (progn (beginning-of-line) (point)))
		  (indent-to
		   (if continued-line
		       (+ this-indent tcl-indent-level)
		     this-indent)))))))))
  )



;;
;; Interfaces to other packages.
;;

;; FIXME Definition of function is very ad-hoc.  Should use
;; beginning-of-defun.  Also has incestuous knowledge about the
;; format of tcl-proc-regexp.
(defun tcl-add-log-defun ()
  "Return name of Tcl function point is in, or nil."
  (save-excursion
    (end-of-line)
    (if (re-search-backward (concat tcl-proc-regexp "\\([^ \t\n{]+\\)") nil t)
	(match-string 2))))

(defun tcl-outline-level ()
  (save-excursion
    (skip-chars-forward " \t")
    (current-column)))



;;
;; Helper functions for inferior Tcl mode.
;;

;; This exists to let us delete the prompt when commands are sent
;; directly to the inferior Tcl.  See gud.el for an explanation of how
;; it all works (I took it from there).  This stuff doesn't really
;; work as well as I'd like it to.  But I don't believe there is
;; anything useful that can be done.
(defvar inferior-tcl-delete-prompt-marker nil)

(defun tcl-filter (proc string)
  (let ((inhibit-quit t))               ;FIXME: Isn't that redundant?
    (with-current-buffer (process-buffer proc)
      ;; Delete prompt if requested.
      (when (marker-buffer inferior-tcl-delete-prompt-marker)
        (delete-region (process-mark proc) inferior-tcl-delete-prompt-marker)
        (set-marker inferior-tcl-delete-prompt-marker nil))))
  (comint-output-filter proc string))

(defun tcl-send-string (proc string)
  (with-current-buffer (process-buffer proc)
    (goto-char (process-mark proc))
    (forward-line 0)             ;Not (beginning-of-line) because of fields.
    (if (looking-at comint-prompt-regexp)
	(set-marker inferior-tcl-delete-prompt-marker (point))))
  (comint-send-string proc string))

(defun tcl-send-region (proc start end)
  (with-current-buffer (process-buffer proc)
    (goto-char (process-mark proc))
    (forward-line 0)             ;Not (beginning-of-line) because of fields.
    (if (looking-at comint-prompt-regexp)
	(set-marker inferior-tcl-delete-prompt-marker (point))))
  (comint-send-region proc start end))

(defun switch-to-tcl (eob-p)
  "Switch to inferior Tcl process buffer.
With argument, positions cursor at end of buffer."
  (interactive "P")
  (if (get-buffer inferior-tcl-buffer)
      (pop-to-buffer inferior-tcl-buffer)
    (error "No current inferior Tcl buffer"))
  (cond (eob-p
	 (push-mark)
	 (goto-char (point-max)))))

(defun inferior-tcl-proc ()
  "Return current inferior Tcl process.
See variable `inferior-tcl-buffer'."
  (let ((proc (get-buffer-process (if (derived-mode-p 'inferior-tcl-mode)
				      (current-buffer)
				    inferior-tcl-buffer))))
    (or proc
	(error "No Tcl process; see variable `inferior-tcl-buffer'"))))

(defun tcl-eval-region (start end &optional and-go)
  "Send the current region to the inferior Tcl process.
Prefix argument means switch to the Tcl buffer afterwards."
  (interactive "r\nP")
  (let ((proc (inferior-tcl-proc)))
    (tcl-send-region
     proc
     ;; Strip leading and trailing whitespace.
     (save-excursion (goto-char start) (skip-chars-forward " \t\n") (point))
     (save-excursion (goto-char end) (skip-chars-backward " \t\n") (point)))
    (tcl-send-string proc "\n")
    (if and-go (switch-to-tcl t))))

(defun tcl-eval-defun (&optional and-go)
  "Send the current defun to the inferior Tcl process.
Prefix argument means switch to the Tcl buffer afterwards."
  (interactive "P")
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (tcl-eval-region (point) end)))
  (if and-go (switch-to-tcl t)))



;;
;; Inferior Tcl mode itself.
;;

(define-derived-mode inferior-tcl-mode comint-mode "Inferior Tcl"
  "Major mode for interacting with Tcl interpreter.

You can start a Tcl process with \\[inferior-tcl].

Entry to this mode runs the normal hooks `comint-mode-hook' and
`inferior-tcl-mode-hook', in that order.

You can send text to the inferior Tcl process from other buffers
containing Tcl source.

Variables controlling Inferior Tcl mode:
  `tcl-application'
    Name of program to run.
  `tcl-command-switches'
    Command line arguments to `tcl-application'.
  `tcl-prompt-regexp'
    Matches prompt.
  `inferior-tcl-source-command'
    Command to use to read Tcl file in running application.
  `inferior-tcl-buffer'
    The current inferior Tcl process buffer.  See variable
    documentation for details on multiple-process support.

The following commands are available:
\\{inferior-tcl-mode-map}"
  (set (make-local-variable 'comint-prompt-regexp)
       (or tcl-prompt-regexp
	   (concat "^" (regexp-quote tcl-application) ">")))
  (setq mode-line-process '(": %s"))
  (setq local-abbrev-table tcl-mode-abbrev-table)
  (set-syntax-table tcl-mode-syntax-table)
  (set (make-local-variable 'defun-prompt-regexp) tcl-omit-ws-regexp)
  (set (make-local-variable 'inferior-tcl-delete-prompt-marker) (make-marker))
  (set-process-filter (get-buffer-process (current-buffer)) 'tcl-filter))

;;;###autoload
(defun inferior-tcl (cmd)
  "Run inferior Tcl process.
Prefix arg means enter program name interactively.
See documentation for function `inferior-tcl-mode' for more information."
  (interactive
   (list (if current-prefix-arg
	     (read-string "Run Tcl: " tcl-application)
	   tcl-application)))
  (unless (comint-check-proc "*inferior-tcl*")
    (set-buffer (apply (function make-comint) "inferior-tcl" cmd nil
		       tcl-command-switches))
    (inferior-tcl-mode)
    ;; Make tclsh display a prompt on ms-windows (or under Unix, when a tty
    ;; wasn't used).  Doesn't affect wish, unfortunately.
    (unless (process-tty-name (inferior-tcl-proc))
      (tcl-send-string (inferior-tcl-proc)
                       "set ::tcl_interactive 1; concat\n")))
  (set (make-local-variable 'tcl-application) cmd)
  (setq inferior-tcl-buffer "*inferior-tcl*")
  (pop-to-buffer "*inferior-tcl*"))

(defalias 'run-tcl 'inferior-tcl)



;;
;; Auto-fill support.
;;

(defun tcl-real-command-p ()
  "Return nil if point is not at the beginning of a command.
A command is the first word on an otherwise empty line, or the
first word following a semicolon, opening brace, or opening bracket."
  (save-excursion
    (skip-chars-backward " \t")
    (cond
     ((bobp) t)
     ((bolp)
      (backward-char)
      ;; Note -- continued comments are not supported here.  I
      ;; consider those to be a wart on the language.
      (not (eq ?\\ (preceding-char))))
     (t
      (memq (preceding-char) '(?\; ?{ ?\[))))))

;; FIXME doesn't actually return t.  See last case.
(defun tcl-real-comment-p ()
  "Return t if point is just after the `#' beginning a real comment.
Does not check to see if previous char is actually `#'.
A real comment is either at the beginning of the buffer,
preceded only by whitespace on the line, or has a preceding
semicolon, opening brace, or opening bracket on the same line."
  (save-excursion
    (backward-char)
    (tcl-real-command-p)))

(defun tcl-hairy-scan-for-comment (state end always-stop)
  "Determine if point is in a comment.
Returns a list of the form `(FLAG . STATE)'.  STATE can be used
as input to future invocations.  FLAG is nil if not in comment,
t otherwise.  If in comment, leaves point at beginning of comment."
  (let ((bol (save-excursion
	       (goto-char end)
	       (line-beginning-position)))
	real-comment
	last-cstart)
    (while (and (not last-cstart) (< (point) end))
      (setq real-comment nil)	 ;In case we've looped around and it is set.
      (setq state (parse-partial-sexp (point) end nil nil state t))
      (if (nth 4 state)
	  (progn
	    ;; If ALWAYS-STOP is set, stop even if we don't have a
	    ;; real comment, or if the comment isn't on the same line
	    ;; as the end.
	    (if always-stop (setq last-cstart (point)))
	    ;; If we have a real comment, then set the comment
	    ;; starting point if we are on the same line as the ending
	    ;; location.
	    (setq real-comment (tcl-real-comment-p))
	    (if real-comment
		(progn
		  (and (> (point) bol) (setq last-cstart (point)))
		  ;; NOTE Emacs 19 has a misfeature whereby calling
		  ;; parse-partial-sexp with COMMENTSTOP set and with
		  ;; an initial list that says point is in a comment
		  ;; will cause an immediate return.  So we must skip
		  ;; over the comment ourselves.
		  (beginning-of-line 2)))
	    ;; Frob the state to make it look like we aren't in a
	    ;; comment.
	    (setcar (nthcdr 4 state) nil))))
    (and last-cstart
	 (goto-char last-cstart))
    (cons real-comment state)))

(defun tcl-in-comment ()
  "Return t if point is in a comment, and leave point at beginning of comment."
  (let ((save (point)))
    (beginning-of-defun)
    (car (tcl-hairy-scan-for-comment nil save nil))))



;;
;; Help-related code.
;;

(defvar tcl-help-saved-dirs nil
  "Saved help directories.
If `tcl-help-directory-list' changes, this allows `tcl-help-on-word'
to update the alist.")

(defvar tcl-help-alist nil
  "Alist with command names as keys and filenames as values.")

(defun tcl-files-alist (dir &optional alist)
  "Recursively add all pairs (FILE . PATH) under DIR to ALIST."
  (dolist (file (directory-files dir t) alist)
    (cond
     ((not (file-directory-p file))
      (push (cons (file-name-nondirectory file) file) alist))
     ((member (file-name-nondirectory file) '("." "..")))
     (t (setq alist (tcl-files-alist file alist))))))

(defun tcl-help-snarf-commands (dirlist)
  "Return alist of commands and filenames."
  (let ((alist nil))
    (dolist (dir dirlist alist)
      (when (file-directory-p dir)
	(setq alist (tcl-files-alist dir alist))))))

(defun tcl-reread-help-files ()
  "Set up to re-read files, and then do it."
  (interactive)
  (message "Building Tcl help file index...")
  (setq tcl-help-saved-dirs tcl-help-directory-list)
  (setq tcl-help-alist (tcl-help-snarf-commands tcl-help-directory-list))
  (message "Building Tcl help file index...done"))

(defun tcl-word-no-props ()
  "Like `current-word', but strips properties."
  (let ((word (current-word)))
    (set-text-properties 0 (length word) nil word)
    word))

(defun tcl-current-word (flag)
  "Return current command word, or nil.
If FLAG is nil, just uses `current-word'.
Otherwise scans backward for most likely Tcl command word."
  (if (and flag
	   (derived-mode-p 'tcl-mode 'inferior-tcl-mode))
      (condition-case nil
	  (save-excursion
	    ;; Look backward for first word actually in alist.
	    (if (bobp)
		()
	      (while (and (not (bobp))
			  (not (tcl-real-command-p)))
		(backward-sexp)))
	    (if (assoc (tcl-word-no-props) tcl-help-alist)
		(tcl-word-no-props)))
	(error nil))
    (tcl-word-no-props)))

;;;###autoload
(defun tcl-help-on-word (command &optional arg)
  "Get help on Tcl command.  Default is word at point.
Prefix argument means invert sense of `tcl-use-smart-word-finder'."
  (interactive
   (list
    (progn
      (if (not (equal tcl-help-directory-list tcl-help-saved-dirs))
	  (tcl-reread-help-files))
      (let ((word (tcl-current-word
		   (if current-prefix-arg
		       (not tcl-use-smart-word-finder)
		     tcl-use-smart-word-finder))))
	(completing-read
	 (if (or (null word) (string= word ""))
	     "Help on Tcl command: "
	   (format "Help on Tcl command (default %s): " word))
	 tcl-help-alist nil t nil nil word)))
    current-prefix-arg))
  (if (not (equal tcl-help-directory-list tcl-help-saved-dirs))
      (tcl-reread-help-files))
  (if (string= command "")
      (setq command (tcl-current-word
		     (if arg
			 (not tcl-use-smart-word-finder)
		       tcl-use-smart-word-finder))))
  (let* ((help (get-buffer-create "*Tcl help*"))
	 (cell (assoc command tcl-help-alist))
	 (file (and cell (cdr cell))))
    (set-buffer help)
    (delete-region (point-min) (point-max))
    (if file
	(progn
	  (insert "*** " command "\n\n")
	  (insert-file-contents file))
      (if (string= command "")
	  (insert "Magical Pig!")
	(insert "Tcl command " command " not in help\n")))
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (display-buffer help)))



;;
;; Other interactive stuff.
;;

(defvar tcl-previous-dir/file nil
  "Record last directory and file used in loading.
This holds a cons cell of the form `(DIRECTORY . FILE)'
describing the last `tcl-load-file' command.")

(defun tcl-load-file (file &optional and-go)
  "Load a Tcl file into the inferior Tcl process.
Prefix argument means switch to the Tcl buffer afterwards."
  (interactive
   (list
    ;; car because comint-get-source returns a list holding the
    ;; filename.
    (car (comint-get-source "Load Tcl file: "
			    (or (and
				 (derived-mode-p 'tcl-mode)
				 (buffer-file-name))
				tcl-previous-dir/file)
			    '(tcl-mode) t))
    current-prefix-arg))
  (comint-check-source file)
  (setq tcl-previous-dir/file (cons (file-name-directory file)
				    (file-name-nondirectory file)))
  (tcl-send-string (inferior-tcl-proc)
		   (format inferior-tcl-source-command (tcl-quote file)))
  (if and-go (switch-to-tcl t)))

(defun tcl-restart-with-file (file &optional and-go)
  "Restart inferior Tcl with file.
If an inferior Tcl process exists, it is killed first.
Prefix argument means switch to the Tcl buffer afterwards."
  (interactive
   (list
    (car (comint-get-source "Restart with Tcl file: "
			    (or (and
				 (derived-mode-p 'tcl-mode)
				 (buffer-file-name))
				tcl-previous-dir/file)
			    '(tcl-mode) t))
    current-prefix-arg))
  (let* ((buf (if (derived-mode-p 'inferior-tcl-mode)
		  (current-buffer)
		inferior-tcl-buffer))
	 (proc (and buf (get-process buf))))
    (cond
     ((not (and buf (get-buffer buf)))
      ;; I think this will be ok.
      (inferior-tcl tcl-application)
      (tcl-load-file file and-go))
     ((or
       (not (comint-check-proc buf))
       (yes-or-no-p
	"A Tcl process is running, are you sure you want to reset it? "))
      (save-excursion
	(comint-check-source file)
	(setq tcl-previous-dir/file (cons (file-name-directory file)
					  (file-name-nondirectory file)))
	(comint-exec (get-buffer-create buf)
		     (if proc
			 (process-name proc)
		       "inferior-tcl")
		     tcl-application file tcl-command-switches)
	(if and-go (switch-to-tcl t)))))))

(defun tcl-auto-fill-mode (&optional arg)
  "Like `auto-fill-mode', but sets `comment-auto-fill-only-comments'."
  (interactive "P")
  (auto-fill-mode arg)
  (if auto-fill-function
      (set (make-local-variable 'comment-auto-fill-only-comments) t)
    (kill-local-variable 'comment-auto-fill-only-comments)))

(defun tcl-electric-hash (&optional count)
  "Insert a `#' and quote if it does not start a real comment.
Prefix arg is number of `#'s to insert.
See variable `tcl-electric-hash-style' for description of quoting
styles."
  (interactive "p")
  (or count (setq count 1))
  (if (> count 0)
      (let ((type
	     (if (eq tcl-electric-hash-style 'smart)
		 (if (> count 3)	; FIXME what is "smart"?
		     'quote
		   'backslash)
	       tcl-electric-hash-style))
	    comment)
	(if type
	    (progn
	      (save-excursion
		(insert "#")
		(setq comment (tcl-in-comment)))
	      (delete-char 1)
	      (and tcl-explain-indentation (message "comment: %s" comment))
	      (cond
	       ((eq type 'quote)
		(if (not comment)
		    (insert "\"")))
	       ((eq type 'backslash)
		;; The following will set count to 0, so the
		;; insert-char can still be run.
		(if (not comment)
		    (while (> count 0)
		      (insert "\\#")
		      (setq count (1- count)))))
	       (t nil))))
	(insert-char ?# count))))

(defun tcl-hashify-buffer ()
  "Quote all `#'s in current buffer that aren't Tcl comments."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let (state
	  result)
      (while (< (point) (point-max))
	(setq result (tcl-hairy-scan-for-comment state (point-max) t))
	(if (car result)
	    (beginning-of-line 2)
	  (backward-char)
	  (if (eq ?# (following-char))
	      (insert "\\"))
	  (forward-char))
	(setq state (cdr result))))))

(defun tcl-comment-indent ()
  "Return the desired indentation, but be careful to add a `;' if needed."
  (save-excursion
    ;; If line is not blank, make sure we insert a ";" first.
    (skip-chars-backward " \t")
    (unless (or (bolp) (tcl-real-command-p))
      (insert ";")
      ;; Try and erase a non-significant char to keep charpos identical.
      (if (memq (char-after) '(?\t ?\s)) (delete-char 1))))
  (funcall (default-value 'comment-indent-function)))

;; The following was inspired by the Tcl editing mode written by
;; Gregor Schmid <schmid@fb3-s7.math.TU-Berlin.DE>.  His version also
;; attempts to snarf the command line options from the command line,
;; but I didn't think that would really be that helpful (doesn't seem
;; like it would be right enough.  His version also looks for the
;; "#!/bin/csh ... exec" hack, but that seemed even less useful.
;; FIXME should make sure that the application mentioned actually
;; exists.
(defun tcl-guess-application ()
  "Attempt to guess Tcl application by looking at first line.
The first line is assumed to look like \"#!.../program ...\"."
  (save-excursion
    (goto-char (point-min))
    (if (looking-at "#![^ \t]*/\\([^ \t\n/]+\\)\\([ \t]\\|$\\)")
	(set (make-local-variable 'tcl-application) (match-string 1)))))



;;
;; XEmacs menu support.
;; Taken from schmid@fb3-s7.math.TU-Berlin.DE (Gregor Schmid),
;; who wrote a different Tcl mode.
;; We also have support for menus in Emacs.  We do this by
;; loading the XEmacs menu emulation code.
;;

(defun tcl-popup-menu (_e)
  (interactive "@e")
  (popup-menu tcl-mode-menu))



;;
;; Quoting and unquoting functions.
;;

;; This quoting is sufficient to protect eg a filename from any sort
;; of expansion or splitting.  Tcl quoting sure sucks.
(defun tcl-quote (string)
  "Quote STRING according to Tcl rules."
  (mapconcat (lambda (char)
	       (if (memq char '(?[ ?] ?{ ?} ?\\ ?\" ?$ ?\s ?\;))
		   (concat "\\" (char-to-string char))
		 (char-to-string char)))
	     string ""))

;;
;; Bug reporting.
;;


;; These are relics kept "just in case".
(defalias 'tcl-uncomment-region 'uncomment-region)
(defalias 'tcl-indent-for-comment 'comment-indent)
(defalias 'add-log-tcl-defun 'tcl-add-log-defun)
(defalias 'indent-tcl-exp 'tcl-indent-exp)
(defalias 'calculate-tcl-indent 'tcl-calculate-indent)
(defalias 'tcl-beginning-of-defun 'beginning-of-defun)
(defalias 'tcl-end-of-defun 'end-of-defun)
(defalias 'tcl-mark-defun 'mark-defun)
(defun tcl-mark () (mark t))

(provide 'tcl)

;;; tcl.el ends here
