;;; sh-script.el --- shell-script editing commands for Emacs

;; Copyright (C) 1993-1997, 1999, 2001-2012  Free Software Foundation, Inc.

;; Author: Daniel Pfeiffer <occitan@esperanto.org>
;; Version: 2.0f
;; Maintainer: FSF
;; Keywords: languages, unix

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

;; Major mode for editing shell scripts.  Bourne, C and rc shells as well
;; as various derivatives are supported and easily derived from.  Structured
;; statements can be inserted with one command or abbrev.  Completion is
;; available for filenames, variables known from the script, the shell and
;; the environment as well as commands.

;;; Known Bugs:

;; - In Bourne the keyword `in' is not anchored to case, for, select ...
;; - Variables in `"' strings aren't fontified because there's no way of
;;   syntactically distinguishing those from `'' strings.

;;		Indentation
;;	 	===========
;; Indentation for rc and es modes is very limited, but for Bourne shells
;; and its derivatives it is quite customizable.
;;
;; The following description applies to sh and derived shells (bash,
;; zsh, ...).
;;
;; There are various customization variables which allow tailoring to
;; a wide variety of styles.  Most of these variables are named
;; sh-indent-for-XXX and sh-indent-after-XXX.  For example.
;; sh-indent-after-if controls the indenting of a line following
;; an if statement, and sh-indent-for-fi controls the indentation
;; of the line containing the fi.
;;
;; You can set each to a numeric value, but it is often more convenient
;; to a symbol such as `+' which uses the value of variable `sh-basic-offset'.
;; By changing this one variable you can increase or decrease how much
;; indentation there is.  Valid symbols:
;;
;; 	+   Indent right by sh-basic-offset
;; 	-   Indent left  by sh-basic-offset
;; 	++  Indent right twice sh-basic-offset
;; 	--  Indent left  twice sh-basic-offset
;; 	*   Indent right half sh-basic-offset
;; 	/   Indent left  half sh-basic-offset.
;;
;; There are 4 commands to help set the indentation variables:
;;
;; `sh-show-indent'
;;    This shows what variable controls the indentation of the current
;;    line and its value.
;;
;; `sh-set-indent'
;;    This allows you to set the value of the variable controlling the
;;    current line's indentation.  You can enter a number or one of a
;;    number of special symbols to denote the value of sh-basic-offset,
;;    or its negative, or half it, or twice it, etc.  If you've used
;;    cc-mode this should be familiar.  If you forget which symbols are
;;    valid simply press C-h at the prompt.
;;
;; `sh-learn-line-indent'
;;    Simply make the line look the way you want it, then invoke this
;;    command.  It will set the variable to the value that makes the line
;;    indent like that.  If called with a prefix argument then it will set
;;    the value to one of the symbols if applicable.
;;
;; `sh-learn-buffer-indent'
;;    This is the deluxe function!  It "learns" the whole buffer (use
;;    narrowing if you want it to process only part).  It outputs to a
;;    buffer *indent* any conflicts it finds, and all the variables it has
;;    learned.  This buffer is a sort of Occur mode buffer, allowing you to
;;    easily find where something was set.  It is popped to automatically
;;    if there are any conflicts found or if `sh-popup-occur-buffer' is
;;    non-nil.
;;    `sh-indent-comment' will be set if all comments follow  the same
;;    pattern;  if they don't it will be set to nil.
;;    Whether `sh-basic-offset' is set is determined by variable
;;    `sh-learn-basic-offset'.
;;
;;    Unfortunately, `sh-learn-buffer-indent' can take a long time to run
;;    (e.g. if there are large case statements).  Perhaps it does not make
;;    sense to run it on large buffers: if lots of lines have different
;;    indentation styles it will produce a lot of diagnostics in the
;;    *indent* buffer; if there is a consistent style then running
;;    `sh-learn-buffer-indent' on a small region of the buffer should
;;    suffice.
;;
;; 	Saving indentation values
;; 	-------------------------
;; After you've learned the values in a buffer, how to you remember
;; them?   Originally I had hoped that `sh-learn-buffer-indent'
;; would make this unnecessary;  simply learn the values when you visit
;; the buffer.
;; You can do this automatically like this:
;;   (add-hook 'sh-set-shell-hook 'sh-learn-buffer-indent)
;;
;; However...  `sh-learn-buffer-indent' is extremely slow,
;; especially on large-ish buffer.  Also, if there are conflicts the
;; "last one wins" which may not produce the desired setting.
;;
;; So...There is a minimal way of being able to save indentation values and
;; to reload them in another buffer or at another point in time.
;;
;; Use `sh-name-style' to give a name to the indentation settings of
;; 	the current buffer.
;; Use `sh-load-style' to load indentation settings for the current
;; 	buffer from a specific style.
;; Use `sh-save-styles-to-buffer' to write all the styles to a buffer
;; 	in lisp code.  You can then store it in a file and later use
;; 	`load-file' to load it.
;;
;; 	Indentation variables - buffer local or global?
;; 	----------------------------------------------
;; I think that often having them buffer-local makes sense,
;; especially if one is using `sh-learn-buffer-indent'.  However, if
;; a user sets values using customization, these changes won't appear
;; to work if the variables are already local!
;;
;; To get round this, there is a variable `sh-make-vars-local' and 2
;; functions: `sh-make-vars-local' and `sh-reset-indent-vars-to-global-values'.
;;
;; If `sh-make-vars-local' is non-nil, then these variables become
;; buffer local when the mode is established.
;; If this is nil, then the variables are global.  At any time you
;; can make them local with the command `sh-make-vars-local'.
;; Conversely, to update with the global values you can use the
;; command `sh-reset-indent-vars-to-global-values'.
;;
;; This may be awkward, but the intent is to cover all cases.
;;
;; 	Awkward things, pitfalls
;; 	------------------------
;; Indentation for a sh script is complicated for a number of reasons:
;;
;; 1. You can't format by simply looking at symbols, you need to look
;;    at keywords.  [This is not the case for rc and es shells.]
;; 2. The character ")" is used both as a matched pair "(" ... ")" and
;;    as a stand-alone symbol (in a case alternative).  This makes
;;    things quite tricky!
;; 3. Here-documents in a script should be treated "as is", and when
;;    they terminate we want to revert to the indentation of the line
;;    containing the "<<" symbol.
;; 4. A line may be continued using the "\".
;; 5. The character "#" (outside a string) normally starts a comment,
;;    but it doesn't in the sequence "$#"!
;;
;; To try and address points 2 3 and 5 I used a feature that cperl mode
;; uses, that of a text's syntax property.  This, however, has 2
;; disadvantages:
;; 1. We need to scan the buffer to find which ")" symbols belong to a
;;    case alternative, to find any here documents, and handle "$#".
;;
;; 	Bugs
;; 	----
;; - Indenting many lines is slow.  It currently does each line
;;   independently, rather than saving state information.
;;
;; - `sh-learn-buffer-indent' is extremely slow.
;;
;; - "case $x in y) echo ;; esac)" the last ) is mis-identified as being
;;   part of a case-pattern.  You need to add a semi-colon after "esac" to
;;   coerce sh-script into doing the right thing.
;;
;; - "echo $z in ps | head)" the last ) is mis-identified as being part of
;;   a case-pattern.  You need to put the "in" between quotes to coerce
;;   sh-script into doing the right thing.
;;
;; - A line starting with "}>foo" is not indented like "} >foo".
;;
;; Richard Sharman <rsharman@pobox.com>  June 1999.

;;; Code:

;; page 1:	variables and settings
;; page 2:	indentation stuff
;; page 3:	mode-command and utility functions
;; page 4:	statement syntax-commands for various shells
;; page 5:	various other commands

(eval-when-compile
  (require 'skeleton)
  (require 'cl)
  (require 'comint))
(require 'executable)

(defvar font-lock-comment-face)
(defvar font-lock-set-defaults)
(defvar font-lock-string-face)


(defgroup sh nil
  "Shell programming utilities."
  :group 'languages)

(defgroup sh-script nil
  "Shell script mode."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'sh
  :prefix "sh-")


(defcustom sh-ancestor-alist
  '((ash . sh)
    (bash . jsh)
    (bash2 . jsh)
    (dtksh . ksh)
    (es . rc)
    (itcsh . tcsh)
    (jcsh . csh)
    (jsh . sh)
    (ksh . ksh88)
    (ksh88 . jsh)
    (oash . sh)
    (pdksh . ksh88)
    (posix . sh)
    (tcsh . csh)
    (wksh . ksh88)
    (wsh . sh)
    (zsh . ksh88)
    (rpm . sh))
  "Alist showing the direct ancestor of various shells.
This is the basis for `sh-feature'.  See also `sh-alias-alist'.
By default we have the following three hierarchies:

csh		C Shell
  jcsh		C Shell with Job Control
  tcsh		TENEX C Shell
    itcsh	Ian's TENEX C Shell
rc		Plan 9 Shell
  es		Extensible Shell
sh		Bourne Shell
  ash		Almquist Shell
  jsh		Bourne Shell with Job Control
    bash	GNU Bourne Again Shell
    ksh88	Korn Shell '88
      ksh	Korn Shell '93
	dtksh	CDE Desktop Korn Shell
      pdksh	Public Domain Korn Shell
      wksh	Window Korn Shell
      zsh	Z Shell
  oash		SCO OA (curses) Shell
  posix		IEEE 1003.2 Shell Standard
  wsh		? Shell"
  :type '(repeat (cons symbol symbol))
  :group 'sh-script)


(defcustom sh-alias-alist
  (append (if (eq system-type 'gnu/linux)
	     '((csh . tcsh)
	       (ksh . pdksh)))
	 ;; for the time being
	 '((ksh . ksh88)
           (bash2 . bash)
	   (sh5 . sh)))
  "Alist for transforming shell names to what they really are.
Use this where the name of the executable doesn't correspond to the type of
shell it really is."
  :type '(repeat (cons symbol symbol))
  :group 'sh-script)


(defcustom sh-shell-file
  (or
   ;; On MSDOS and Windows, collapse $SHELL to lower-case and remove
   ;; the executable extension, so comparisons with the list of
   ;; known shells work.
   (and (memq system-type '(ms-dos windows-nt))
	(let* ((shell (getenv "SHELL"))
	       (shell-base
		(and shell (file-name-nondirectory shell))))
	  ;; shell-script mode doesn't support DOS/Windows shells,
	  ;; so use the default instead.
	  (if (or (null shell)
		  (member (downcase shell-base)
			  '("command.com" "cmd.exe" "4dos.com" "ndos.com"
			    "cmdproxy.exe")))
	      "/bin/sh"
	    (file-name-sans-extension (downcase shell)))))
   (getenv "SHELL")
   "/bin/sh")
  "The executable file name for the shell being programmed."
  :type 'string
  :group 'sh-script)


(defcustom sh-shell-arg
  ;; bash does not need any options when run in a shell script,
  '((bash)
    (csh . "-f")
    (pdksh)
    ;; Bill_Mann@praxisint.com says -p with ksh can do harm.
    (ksh88)
    ;; -p means don't initialize functions from the environment.
    (rc . "-p")
    ;; Someone proposed -motif, but we don't want to encourage
    ;; use of a non-free widget set.
    (wksh)
    ;; -f means don't run .zshrc.
    (zsh . "-f"))
  "Single argument string for the magic number.  See `sh-feature'."
  :type '(repeat (cons (symbol :tag "Shell")
		       (choice (const :tag "No Arguments" nil)
			       (string :tag "Arguments")
			       (sexp :format "Evaluate: %v"))))
  :group 'sh-script)

(defcustom sh-imenu-generic-expression
  `((sh
     . ((nil "^\\s-*\\(function\\s-+\\)?\\([[:alpha:]_][[:alnum:]_]+\\)\\s-*()" 2))))
  "Alist of regular expressions for recognizing shell function definitions.
See `sh-feature' and `imenu-generic-expression'."
  :type '(alist :key-type (symbol :tag "Shell")
		:value-type (alist :key-type (choice :tag "Title"
						     string
						     (const :tag "None" nil))
				   :value-type
				   (repeat :tag "Regexp, index..." sexp)))
  :group 'sh-script
  :version "20.4")

(defvar sh-shell-variables nil
  "Alist of shell variable names that should be included in completion.
These are used for completion in addition to all the variables named
in `process-environment'.  Each element looks like (VAR . VAR), where
the car and cdr are the same symbol.")

(defvar sh-shell-variables-initialized nil
  "Non-nil if `sh-shell-variables' is initialized.")

(defun sh-canonicalize-shell (shell)
  "Convert a shell name SHELL to the one we should handle it as."
  (if (string-match "\\.exe\\'" shell)
      (setq shell (substring shell 0 (match-beginning 0))))
  (or (symbolp shell)
      (setq shell (intern shell)))
  (or (cdr (assq shell sh-alias-alist))
      shell))

(defvar sh-shell (sh-canonicalize-shell (file-name-nondirectory sh-shell-file))
  "The shell being programmed.  This is set by \\[sh-set-shell].")
;;;###autoload(put 'sh-shell 'safe-local-variable 'symbolp)

(define-abbrev-table 'sh-mode-abbrev-table ())


;; I turned off this feature because it doesn't permit typing commands
;; in the usual way without help.
;;(defvar sh-abbrevs
;;  '((csh sh-abbrevs shell
;;	 "switch" 'sh-case
;;	 "getopts" 'sh-while-getopts)

;;    (es sh-abbrevs shell
;;	"function" 'sh-function)

;;    (ksh88 sh-abbrevs sh
;;	   "select" 'sh-select)

;;    (rc sh-abbrevs shell
;;	"case" 'sh-case
;;	"function" 'sh-function)

;;    (sh sh-abbrevs shell
;;	"case" 'sh-case
;;	"function" 'sh-function
;;	"until" 'sh-until
;;	"getopts" 'sh-while-getopts)

;;    ;; The next entry is only used for defining the others
;;    (shell "for" sh-for
;;	   "loop" sh-indexed-loop
;;	   "if" sh-if
;;	   "tmpfile" sh-tmp-file
;;	   "while" sh-while)

;;    (zsh sh-abbrevs ksh88
;;	 "repeat" 'sh-repeat))
;;  "Abbrev-table used in Shell-Script mode.  See `sh-feature'.
;;;Due to the internal workings of abbrev tables, the shell name symbol is
;;;actually defined as the table for the like of \\[edit-abbrevs].")



(defun sh-mode-syntax-table (table &rest list)
  "Copy TABLE and set syntax for successive CHARs according to strings S."
  (setq table (copy-syntax-table table))
  (while list
    (modify-syntax-entry (pop list) (pop list) table))
  table)

(defvar sh-mode-syntax-table
  (sh-mode-syntax-table ()
	?\# "<"
	?\n ">#"
	?\" "\"\""
	?\' "\"'"
	?\` "\"`"
	;; ?$ might also have a ". p" syntax. Both "'" and ". p" seem
	;; to work fine. This is needed so that dabbrev-expand
	;; $VARNAME works.
	?$ "'"
	?! "_"
	?% "_"
	?: "_"
	?. "_"
	?^ "_"
	?~ "_"
	?, "_"
	?= "."
	?< "."
	?> ".")
  "The syntax table to use for Shell-Script mode.
This is buffer-local in every such buffer.")

(defvar sh-mode-syntax-table-input
  '((sh . nil))
  "Syntax-table used in Shell-Script mode.  See `sh-feature'.")

(defvar sh-mode-map
  (let ((map (make-sparse-keymap))
	(menu-map (make-sparse-keymap)))
    (define-key map "\C-c(" 'sh-function)
    (define-key map "\C-c\C-w" 'sh-while)
    (define-key map "\C-c\C-u" 'sh-until)
    (define-key map "\C-c\C-t" 'sh-tmp-file)
    (define-key map "\C-c\C-s" 'sh-select)
    (define-key map "\C-c\C-r" 'sh-repeat)
    (define-key map "\C-c\C-o" 'sh-while-getopts)
    (define-key map "\C-c\C-l" 'sh-indexed-loop)
    (define-key map "\C-c\C-i" 'sh-if)
    (define-key map "\C-c\C-f" 'sh-for)
    (define-key map "\C-c\C-c" 'sh-case)
    (define-key map "\C-c?" 'sh-show-indent)
    (define-key map "\C-c=" 'sh-set-indent)
    (define-key map "\C-c<" 'sh-learn-line-indent)
    (define-key map "\C-c>" 'sh-learn-buffer-indent)
    (define-key map "\C-c\C-\\" 'sh-backslash-region)

    (define-key map "=" 'sh-assignment)
    (define-key map "\C-c+" 'sh-add)
    (define-key map "\C-\M-x" 'sh-execute-region)
    (define-key map "\C-c\C-x" 'executable-interpret)
    ;; FIXME: Use post-self-insert-hook.
    (define-key map "<" 'sh-maybe-here-document)
    (define-key map "(" 'skeleton-pair-insert-maybe)
    (define-key map "{" 'skeleton-pair-insert-maybe)
    (define-key map "[" 'skeleton-pair-insert-maybe)
    (define-key map "'" 'skeleton-pair-insert-maybe)
    (define-key map "`" 'skeleton-pair-insert-maybe)
    (define-key map "\"" 'skeleton-pair-insert-maybe)

    (define-key map [remap complete-tag] 'comint-dynamic-complete)
    (define-key map [remap delete-backward-char]
      'backward-delete-char-untabify)
    (define-key map "\C-c:" 'sh-set-shell)
    (define-key map [remap backward-sentence] 'sh-beginning-of-command)
    (define-key map [remap forward-sentence] 'sh-end-of-command)
    (define-key map [menu-bar sh-script] (cons "Sh-Script" menu-map))
    (define-key menu-map [sh-learn-buffer-indent]
      '(menu-item "Learn buffer indentation" sh-learn-buffer-indent
		  :help "Learn how to indent the buffer the way it currently is."))
    (define-key menu-map [sh-learn-line-indent]
      '(menu-item "Learn line indentation" sh-learn-line-indent
		  :help "Learn how to indent a line as it currently is indented"))
    (define-key menu-map [sh-show-indent]
      '(menu-item "Show indentation" sh-show-indent
		  :help "Show the how the current line would be indented"))
    (define-key menu-map [sh-set-indent]
      '(menu-item "Set indentation" sh-set-indent
		  :help "Set the indentation for the current line"))

    (define-key menu-map [sh-pair]
      '(menu-item "Insert braces and quotes in pairs"
		  (lambda ()
		    (interactive)
		    (require 'skeleton)
		    (setq skeleton-pair (not skeleton-pair)))
		  :button (:toggle . (and (boundp 'skeleton-pair)
					  skeleton-pair))
		  :help "Inserting a brace or quote automatically inserts the matching pair"))

    (define-key menu-map [sh-s0] '("--"))
    ;; Insert
    (define-key menu-map [sh-function]
      '(menu-item "Function..." sh-function
		  :help "Insert a function definition"))
    (define-key menu-map [sh-add]
      '(menu-item "Addition..." sh-add
		  :help "Insert an addition of VAR and prefix DELTA for Bourne (type) shell"))
    (define-key menu-map [sh-until]
      '(menu-item "Until Loop" sh-until
		  :help "Insert an until loop"))
    (define-key menu-map [sh-repeat]
      '(menu-item "Repeat Loop" sh-repeat
		  :help "Insert a repeat loop definition"))
    (define-key menu-map [sh-while]
      '(menu-item "While Loop" sh-while
		  :help "Insert a while loop"))
    (define-key menu-map [sh-getopts]
      '(menu-item "Options Loop" sh-while-getopts
		  :help "Insert a while getopts loop."))
    (define-key menu-map [sh-indexed-loop]
      '(menu-item "Indexed Loop" sh-indexed-loop
		  :help "Insert an indexed loop from 1 to n."))
    (define-key menu-map [sh-select]
      '(menu-item "Select Statement" sh-select
		  :help "Insert a select statement "))
    (define-key menu-map [sh-if]
      '(menu-item "If Statement" sh-if
		  :help "Insert an if statement"))
    (define-key menu-map [sh-for]
      '(menu-item "For Loop" sh-for
		  :help "Insert a for loop"))
    (define-key menu-map [sh-case]
      '(menu-item "Case Statement" sh-case
		  :help "Insert a case/switch statement"))
    (define-key menu-map [sh-s1] '("--"))
    (define-key menu-map [sh-exec]
      '(menu-item "Execute region" sh-execute-region
		  :help "Pass optional header and region to a subshell for noninteractive execution"))
    (define-key menu-map [sh-exec-interpret]
      '(menu-item "Execute script..." executable-interpret
		  :help "Run script with user-specified args, and collect output in a buffer"))
    (define-key menu-map [sh-set-shell]
      '(menu-item "Set shell type..." sh-set-shell
		  :help "Set this buffer's shell to SHELL (a string)"))
    (define-key menu-map [sh-backslash-region]
      '(menu-item "Backslash region" sh-backslash-region
		  :help "Insert, align, or delete end-of-line backslashes on the lines in the region."))
    map)
  "Keymap used in Shell-Script mode.")

(defvar sh-skeleton-pair-default-alist '((?( _ ?)) (?\))
				      (?[ ?\s _ ?\s ?]) (?\])
				      (?{ _ ?}) (?\}))
  "Value to use for `skeleton-pair-default-alist' in Shell-Script mode.")

(defcustom sh-dynamic-complete-functions
  '(shell-dynamic-complete-environment-variable
    shell-dynamic-complete-command
    comint-dynamic-complete-filename)
  "Functions for doing TAB dynamic completion."
  :type '(repeat function)
  :group 'sh-script)

(defcustom sh-assignment-regexp
  '((csh . "\\<\\([[:alnum:]_]+\\)\\(\\[.+\\]\\)?[ \t]*[-+*/%^]?=")
    ;; actually spaces are only supported in let/(( ... ))
    (ksh88 . "\\<\\([[:alnum:]_]+\\)\\(\\[.+\\]\\)?[ \t]*\\([-+*/%&|~^]\\|<<\\|>>\\)?=")
    (bash . "\\<\\([[:alnum:]_]+\\)\\(\\[.+\\]\\)?\\+?=")
    (rc . "\\<\\([[:alnum:]_*]+\\)[ \t]*=")
    (sh . "\\<\\([[:alnum:]_]+\\)="))
  "Regexp for the variable name and what may follow in an assignment.
First grouping matches the variable name.  This is upto and including the `='
sign.  See `sh-feature'."
  :type '(repeat (cons (symbol :tag "Shell")
		       (choice regexp
			       (sexp :format "Evaluate: %v"))))
  :group 'sh-script)


(defcustom sh-indentation 4
  "The width for further indentation in Shell-Script mode."
  :type 'integer
  :group 'sh-script)
(put 'sh-indentation 'safe-local-variable 'integerp)

(defcustom sh-remember-variable-min 3
  "Don't remember variables less than this length for completing reads."
  :type 'integer
  :group 'sh-script)


(defvar sh-header-marker nil
  "When non-nil is the end of header for prepending by \\[sh-execute-region].
That command is also used for setting this variable.")
(make-variable-buffer-local 'sh-header-marker)

(defcustom sh-beginning-of-command
  "\\([;({`|&]\\|\\`\\|[^\\]\n\\)[ \t]*\\([/~[:alnum:]:]\\)"
  "Regexp to determine the beginning of a shell command.
The actual command starts at the beginning of the second \\(grouping\\)."
  :type 'regexp
  :group 'sh-script)


(defcustom sh-end-of-command
  "\\([/~[:alnum:]:]\\)[ \t]*\\([;#)}`|&]\\|$\\)"
  "Regexp to determine the end of a shell command.
The actual command ends at the end of the first \\(grouping\\)."
  :type 'regexp
  :group 'sh-script)



(defcustom sh-here-document-word "EOF"
  "Word to delimit here documents.
If the first character of this string is \"-\", this is taken as
part of the redirection operator, rather than part of the
word (that is, \"<<-\" instead of \"<<\").  This is a feature
used by some shells (for example Bash) to indicate that leading
tabs inside the here document should be ignored.  In this case,
Emacs indents the initial body and end of the here document with
tabs, to the same level as the start (note that apart from this
there is no support for indentation of here documents).  This
will only work correctly if `sh-basic-offset' is a multiple of
`tab-width'.

Any quote characters or leading whitespace in the word are
removed when closing the here document."
  :type 'string
  :group 'sh-script)


(defvar sh-test
  '((sh "[  ]" . 3)
    (ksh88 "[[  ]]" . 4))
  "Initial input in Bourne if, while and until skeletons.  See `sh-feature'.")


;; customized this out of sheer bravado.  not for the faint of heart.
;; but it *did* have an asterisk in the docstring!
(defcustom sh-builtins
  '((bash sh-append posix
	  "." "alias" "bg" "bind" "builtin" "caller" "compgen" "complete"
          "declare" "dirs" "disown" "enable" "fc" "fg" "help" "history"
          "jobs" "kill" "let" "local" "popd" "printf" "pushd" "shopt"
          "source" "suspend" "typeset" "unalias")

    ;; The next entry is only used for defining the others
    (bourne sh-append shell
	    "eval" "export" "getopts" "newgrp" "pwd" "read" "readonly"
	    "times" "ulimit")

    (csh sh-append shell
	 "alias" "chdir" "glob" "history" "limit" "nice" "nohup" "rehash"
	 "setenv" "source" "time" "unalias" "unhash")

    (dtksh sh-append wksh)

    (es "access" "apids" "cd" "echo" "eval" "false" "let" "limit" "local"
	"newpgrp" "result" "time" "umask" "var" "vars" "wait" "whatis")

    (jsh sh-append sh
	 "bg" "fg" "jobs" "kill" "stop" "suspend")

    (jcsh sh-append csh
	  "bg" "fg" "jobs" "kill" "notify" "stop" "suspend")

    (ksh88 sh-append bourne
	   "alias" "bg" "false" "fc" "fg" "jobs" "kill" "let" "print" "time"
	   "typeset" "unalias" "whence")

    (oash sh-append sh
	  "checkwin" "dateline" "error" "form" "menu" "newwin" "oadeinit"
	  "oaed" "oahelp" "oainit" "pp" "ppfile" "scan" "scrollok" "wattr"
	  "wclear" "werase" "win" "wmclose" "wmmessage" "wmopen" "wmove"
	  "wmtitle" "wrefresh")

    (pdksh sh-append ksh88
	   "bind")

    (posix sh-append sh
	   "command")

    (rc "builtin" "cd" "echo" "eval" "limit" "newpgrp" "shift" "umask" "wait"
	"whatis")

    (sh sh-append bourne
	"hash" "test" "type")

    ;; The next entry is only used for defining the others
    (shell "cd" "echo" "eval" "set" "shift" "umask" "unset" "wait")

    (wksh sh-append ksh88
          ;; FIXME: This looks too much like a regexp.  --Stef
	  "Xt[A-Z][A-Za-z]*")

    (zsh sh-append ksh88
	 "autoload" "bindkey" "builtin" "chdir" "compctl" "declare" "dirs"
	 "disable" "disown" "echotc" "enable" "functions" "getln" "hash"
	 "history" "integer" "limit" "local" "log" "popd" "pushd" "r"
	 "readonly" "rehash" "sched" "setopt" "source" "suspend" "true"
	 "ttyctl" "type" "unfunction" "unhash" "unlimit" "unsetopt" "vared"
	 "which"))
  "List of all shell builtins for completing read and fontification.
Note that on some systems not all builtins are available or some are
implemented as aliases.  See `sh-feature'."
  :type '(repeat (cons (symbol :tag "Shell")
		       (choice (repeat string)
			       (sexp :format "Evaluate: %v"))))
  :group 'sh-script)



(defcustom sh-leading-keywords
  '((bash sh-append sh
          "time")

    (csh "else")

    (es "true" "unwind-protect" "whatis")

    (rc "else")

    (sh "!" "do" "elif" "else" "if" "then" "trap" "type" "until" "while"))
  "List of keywords that may be immediately followed by a builtin or keyword.
Given some confusion between keywords and builtins depending on shell and
system, the distinction here has been based on whether they influence the
flow of control or syntax.  See `sh-feature'."
  :type '(repeat (cons (symbol :tag "Shell")
		       (choice (repeat string)
			       (sexp :format "Evaluate: %v"))))
  :group 'sh-script)


(defcustom sh-other-keywords
  '((bash sh-append bourne
	  "bye" "logout" "select")

    ;; The next entry is only used for defining the others
    (bourne sh-append sh
	    "function")

    (csh sh-append shell
	 "breaksw" "default" "end" "endif" "endsw" "foreach" "goto"
	 "if" "logout" "onintr" "repeat" "switch" "then" "while")

    (es "break" "catch" "exec" "exit" "fn" "for" "forever" "fork" "if"
	"return" "throw" "while")

    (ksh88 sh-append bourne
	   "select")

    (rc "break" "case" "exec" "exit" "fn" "for" "if" "in" "return" "switch"
	"while")

    (sh sh-append shell
	"done" "esac" "fi" "for" "in" "return")

    ;; The next entry is only used for defining the others
    (shell "break" "case" "continue" "exec" "exit")

    (zsh sh-append bash
	 "select" "foreach"))
  "List of keywords not in `sh-leading-keywords'.
See `sh-feature'."
  :type '(repeat (cons (symbol :tag "Shell")
		       (choice (repeat string)
			       (sexp :format "Evaluate: %v"))))
  :group 'sh-script)



(defvar sh-variables
  '((bash sh-append sh
	  "allow_null_glob_expansion" "auto_resume" "BASH" "BASH_ENV"
	  "BASH_VERSINFO" "BASH_VERSION" "cdable_vars" "COMP_CWORD"
	  "COMP_LINE" "COMP_POINT" "COMP_WORDS" "COMPREPLY" "DIRSTACK"
	  "ENV" "EUID" "FCEDIT" "FIGNORE" "FUNCNAME"
	  "glob_dot_filenames" "GLOBIGNORE" "GROUPS" "histchars"
	  "HISTCMD" "HISTCONTROL" "HISTFILE" "HISTFILESIZE"
	  "HISTIGNORE" "history_control" "HISTSIZE"
	  "hostname_completion_file" "HOSTFILE" "HOSTTYPE" "IGNOREEOF"
	  "ignoreeof" "INPUTRC" "LINENO" "MACHTYPE" "MAIL_WARNING"
	  "noclobber" "nolinks" "notify" "no_exit_on_failed_exec"
	  "NO_PROMPT_VARS" "OLDPWD" "OPTERR" "OSTYPE" "PIPESTATUS"
	  "PPID" "POSIXLY_CORRECT" "PROMPT_COMMAND" "PS3" "PS4"
	  "pushd_silent" "PWD" "RANDOM" "REPLY" "SECONDS" "SHELLOPTS"
	  "SHLVL" "TIMEFORMAT" "TMOUT" "UID")

    (csh sh-append shell
	 "argv" "cdpath" "child" "echo" "histchars" "history" "home"
	 "ignoreeof" "mail" "noclobber" "noglob" "nonomatch" "path" "prompt"
	 "shell" "status" "time" "verbose")

    (es sh-append shell
	"apid" "cdpath" "CDPATH" "history" "home" "ifs" "noexport" "path"
	"pid" "prompt" "signals")

    (jcsh sh-append csh
	  "notify")

    (ksh88 sh-append sh
	   "ENV" "ERRNO" "FCEDIT" "FPATH" "HISTFILE" "HISTSIZE" "LINENO"
	   "OLDPWD" "PPID" "PS3" "PS4" "PWD" "RANDOM" "REPLY" "SECONDS"
	   "TMOUT")

    (oash sh-append sh
	  "FIELD" "FIELD_MAX" "LAST_KEY" "OALIB" "PP_ITEM" "PP_NUM")

    (rc sh-append shell
	"apid" "apids" "cdpath" "CDPATH" "history" "home" "ifs" "path" "pid"
	"prompt" "status")

    (sh sh-append shell
	"CDPATH" "IFS" "OPTARG" "OPTIND" "PS1" "PS2")

    ;; The next entry is only used for defining the others
    (shell "COLUMNS" "EDITOR" "HOME" "HUSHLOGIN" "LANG" "LC_COLLATE"
	   "LC_CTYPE" "LC_MESSAGES" "LC_MONETARY" "LC_NUMERIC" "LC_TIME"
	   "LINES" "LOGNAME" "MAIL" "MAILCHECK" "MAILPATH" "PAGER" "PATH"
	   "SHELL" "TERM" "TERMCAP" "TERMINFO" "VISUAL")

    (tcsh sh-append csh
	  "addsuffix" "ampm" "autocorrect" "autoexpand" "autolist"
	  "autologout" "chase_symlinks" "correct" "dextract" "edit" "el"
	  "fignore" "gid" "histlit" "HOST" "HOSTTYPE" "HPATH"
	  "ignore_symlinks" "listjobs" "listlinks" "listmax" "matchbeep"
	  "nobeep" "NOREBIND" "oid" "printexitvalue" "prompt2" "prompt3"
	  "pushdsilent" "pushdtohome" "recexact" "recognize_only_executables"
	  "rmstar" "savehist" "SHLVL" "showdots" "sl" "SYSTYPE" "tcsh" "term"
	  "tperiod" "tty" "uid" "version" "visiblebell" "watch" "who"
	  "wordchars")

    (zsh sh-append ksh88
	 "BAUD" "bindcmds" "cdpath" "DIRSTACKSIZE" "fignore" "FIGNORE" "fpath"
	 "HISTCHARS" "hostcmds" "hosts" "HOSTS" "LISTMAX" "LITHISTSIZE"
	 "LOGCHECK" "mailpath" "manpath" "NULLCMD" "optcmds" "path" "POSTEDIT"
	 "prompt" "PROMPT" "PROMPT2" "PROMPT3" "PROMPT4" "psvar" "PSVAR"
	 "READNULLCMD" "REPORTTIME" "RPROMPT" "RPS1" "SAVEHIST" "SPROMPT"
	 "STTY" "TIMEFMT" "TMOUT" "TMPPREFIX" "varcmds" "watch" "WATCH"
	 "WATCHFMT" "WORDCHARS" "ZDOTDIR"))
  "List of all shell variables available for completing read.
See `sh-feature'.")


;; Font-Lock support

(defface sh-heredoc
  '((((min-colors 88) (class color)
      (background dark))
     (:foreground "yellow1" :weight bold))
    (((class color)
      (background dark))
     (:foreground "yellow" :weight bold))
    (((class color)
      (background light))
     (:foreground "tan1" ))
    (t
     (:weight bold)))
  "Face to show a here-document"
  :group 'sh-indentation)

;; These colors are probably icky.  It's just a placeholder though.
(defface sh-quoted-exec
  '((((class color) (background dark))
     (:foreground "salmon"))
    (((class color) (background light))
     (:foreground "magenta"))
    (t
     (:weight bold)))
  "Face to show quoted execs like ``"
  :group 'sh-indentation)
(define-obsolete-face-alias 'sh-heredoc-face 'sh-heredoc "22.1")
(defvar sh-heredoc-face 'sh-heredoc)

(defface sh-escaped-newline '((t :inherit font-lock-string-face))
  "Face used for (non-escaped) backslash at end of a line in Shell-script mode."
  :group 'sh-script
  :version "22.1")

(defvar sh-font-lock-keywords-var
  '((csh sh-append shell
	 ("\\${?[#?]?\\([[:alpha:]_][[:alnum:]_]*\\|0\\)" 1
          font-lock-variable-name-face))

    (es sh-append executable-font-lock-keywords
	("\\$#?\\([[:alpha:]_][[:alnum:]_]*\\|[0-9]+\\)" 1
         font-lock-variable-name-face))

    (rc sh-append es)
    (bash sh-append sh ("\\$(\\(\\sw+\\)" (1 'sh-quoted-exec t) ))
    (sh sh-append shell
	;; Variable names.
	("\\$\\({#?\\)?\\([[:alpha:]_][[:alnum:]_]*\\|[-#?@!]\\)" 2
	  font-lock-variable-name-face)
	;; Function names.
	("^\\(\\sw+\\)[ \t]*(" 1 font-lock-function-name-face)
	("\\<\\(function\\)\\>[ \t]*\\(\\sw+\\)?"
	  (1 font-lock-keyword-face) (2 font-lock-function-name-face nil t))
	("\\(?:^\\s *\\|[[();&|]\\s *\\|\\(?:\\s +-[ao]\\|if\\|else\\|then\\|while\\|do\\)\\s +\\)\\(!\\)"
	 1 font-lock-negation-char-face))

    ;; The next entry is only used for defining the others
    (shell
           ;; Using font-lock-string-face here confuses sh-get-indent-info.
           ("\\(^\\|[^\\]\\)\\(\\\\\\\\\\)*\\(\\\\\\)$" 3 'sh-escaped-newline)
	   ("\\\\[^[:alnum:]]" 0 font-lock-string-face)
	   ("\\${?\\([[:alpha:]_][[:alnum:]_]*\\|[0-9]+\\|[$*_]\\)" 1
	     font-lock-variable-name-face))
    (rpm sh-append rpm2
	 ("%{?\\(\\sw+\\)"  1 font-lock-keyword-face))
    (rpm2 sh-append shell
	  ("^\\(\\sw+\\):"  1 font-lock-variable-name-face)))
  "Default expressions to highlight in Shell Script modes.  See `sh-feature'.")

(defvar sh-font-lock-keywords-var-1
  '((sh "[ \t]in\\>"))
  "Subdued level highlighting for Shell Script modes.")

(defvar sh-font-lock-keywords-var-2 ()
  "Gaudy level highlighting for Shell Script modes.")

;; These are used for the syntax table stuff (derived from cperl-mode).
;; Note: parse-sexp-lookup-properties must be set to t for it to work.
(defconst sh-st-punc (string-to-syntax "."))
(defconst sh-here-doc-syntax (string-to-syntax "|")) ;; generic string

(eval-and-compile
  (defconst sh-escaped-line-re
    ;; Should match until the real end-of-continued-line, but if that is not
    ;; possible (because we bump into EOB or the search bound), then we should
    ;; match until the search bound.
    "\\(?:\\(?:.*[^\\\n]\\)?\\(?:\\\\\\\\\\)*\\\\\n\\)*.*")

  (defconst sh-here-doc-open-re
    (concat "<<-?\\s-*\\\\?\\(\\(?:['\"][^'\"]+['\"]\\|\\sw\\|[-/~._]\\)+\\)"
            sh-escaped-line-re "\\(\n\\)")))

(defun sh-font-lock-open-heredoc (start string eol)
  "Determine the syntax of the \\n after a <<EOF.
START is the position of <<.
STRING is the actual word used as delimiter (e.g. \"EOF\").
INDENTED is non-nil if the here document's content (and the EOF mark) can
be indented (i.e. a <<- was used rather than just <<).
Point is at the beginning of the next line."
  (unless (or (memq (char-before start) '(?< ?>))
	      (sh-in-comment-or-string start))
    ;; We're looking at <<STRING, so we add "^STRING$" to the syntactic
    ;; font-lock keywords to detect the end of this here document.
    (let ((str (replace-regexp-in-string "['\"]" "" string))
          (ppss (save-excursion (syntax-ppss eol))))
      (if (nth 4 ppss)
          ;; The \n not only starts the heredoc but also closes a comment.
          ;; Let's close the comment just before the \n.
          (put-text-property (1- eol) eol 'syntax-table '(12))) ;">"
      (if (or (nth 5 ppss) (> (count-lines start eol) 1))
          ;; If the sh-escaped-line-re part of sh-here-doc-open-re has matched
          ;; several lines, make sure we refontify them together.
          ;; Furthermore, if (nth 5 ppss) is non-nil (i.e. the \n is
          ;; escaped), it means the right \n is actually further down.
          ;; Don't bother fixing it now, but place a multiline property so
          ;; that when jit-lock-context-* refontifies the rest of the
          ;; buffer, it also refontifies the current line with it.
          (put-text-property start (1+ eol) 'syntax-multiline t))
      (put-text-property eol (1+ eol) 'sh-here-doc-marker str)
      (prog1 sh-here-doc-syntax
        (goto-char (+ 2 start))))))

(defun sh-syntax-propertize-here-doc (end)
  (let ((ppss (syntax-ppss)))
    (when (eq t (nth 3 ppss))
      (let ((key (get-text-property (nth 8 ppss) 'sh-here-doc-marker))
            (case-fold-search nil))
        (when (re-search-forward
               (concat "^\\([ \t]*\\)" (regexp-quote key) "\\(\n\\)")
               end 'move)
          (let ((eol (match-beginning 2)))
            (put-text-property eol (1+ eol)
                               'syntax-table sh-here-doc-syntax)))))))

(defun sh-font-lock-quoted-subshell (limit)
  "Search for a subshell embedded in a string.
Find all the unescaped \" characters within said subshell, remembering that
subshells can nest."
  ;; FIXME: This can (and often does) match multiple lines, yet it makes no
  ;; effort to handle multiline cases correctly, so it ends up being
  ;; rather flaky.
  (when (eq ?\" (nth 3 (syntax-ppss))) ; Check we matched an opening quote.
    ;; bingo we have a $( or a ` inside a ""
    (let (;; `state' can be: double-quote, backquote, code.
          (state (if (eq (char-before) ?`) 'backquote 'code))
          ;; Stacked states in the context.
          (states '(double-quote)))
      (while (and state (progn (skip-chars-forward "^'\\\\\"`$()" limit)
                               (< (point) limit)))
        ;; unescape " inside a $( ... ) construct.
        (case (char-after)
          (?\' (case state
                 (double-quote nil)
                 (t (forward-char 1) (skip-chars-forward "^'" limit))))
          (?\\ (forward-char 1))
          (?\" (case state
                 (double-quote (setq state (pop states)))
                 (t (push state states) (setq state 'double-quote)))
               (if state (put-text-property (point) (1+ (point))
                                            'syntax-table '(1))))
          (?\` (case state
                 (backquote (setq state (pop states)))
                 (t (push state states) (setq state 'backquote))))
          (?\$ (if (not (eq (char-after (1+ (point))) ?\())
                   nil
                 (forward-char 1)
                 (case state
                   (t (push state states) (setq state 'code)))))
          (?\( (case state
                 (double-quote nil)
                 (t (push state states) (setq state 'code))))
          (?\) (case state
                 (double-quote nil)
                 (t (setq state (pop states)))))
          (t (error "Internal error in sh-font-lock-quoted-subshell")))
        (forward-char 1)))))


(defun sh-is-quoted-p (pos)
  (and (eq (char-before pos) ?\\)
       (not (sh-is-quoted-p (1- pos)))))

(defun sh-font-lock-paren (start)
  (unless (nth 8 (syntax-ppss))
    (save-excursion
      (goto-char start)
      ;; Skip through all patterns
      (while
          (progn
            (while
                (progn
                  (forward-comment (- (point-max)))
                  (when (and (eolp) (sh-is-quoted-p (point)))
                    (forward-char -1)
                    t)))
            ;; Skip through one pattern
            (while
                (or (/= 0 (skip-syntax-backward "w_"))
                    (/= 0 (skip-chars-backward "-$=?[]*@/\\\\"))
                    (and (sh-is-quoted-p (1- (point)))
                         (goto-char (- (point) 2)))
                    (when (memq (char-before) '(?\" ?\' ?\}))
                      (condition-case nil (progn (backward-sexp 1) t)
                        (error nil)))))
            ;; Patterns can be preceded by an open-paren (Bug#1320).
            (if (eq (char-before (point)) ?\()
                (backward-char 1))
            (while (progn
                     (forward-comment (- (point-max)))
                     ;; Maybe we've bumped into an escaped newline.
                     (sh-is-quoted-p (point)))
              (backward-char 1))
            (when (eq (char-before) ?|)
              (backward-char 1) t)))
      (when (progn (backward-char 2)
                   (if (> start (line-end-position))
                       (put-text-property (point) (1+ start)
                                          'syntax-multiline t))
                   ;; FIXME: The `in' may just be a random argument to
                   ;; a normal command rather than the real `in' keyword.
                   ;; I.e. we should look back to try and find the
                   ;; corresponding `case'.
                   (and (looking-at ";[;&]\\|\\_<in")
                        ;; ";; esac )" is a case that looks like a case-pattern
                        ;; but it's really just a close paren after a case
                        ;; statement.  I.e. if we skipped over `esac' just now,
                        ;; we're not looking at a case-pattern.
                        (not (looking-at "..[ \t\n]+esac[^[:word:]_]"))))
        sh-st-punc))))

(defun sh-font-lock-backslash-quote ()
  (if (eq (save-excursion (nth 3 (syntax-ppss (match-beginning 0)))) ?\')
      ;; In a '...' the backslash is not escaping.
      sh-st-punc
    nil))

(defun sh-syntax-propertize-function (start end)
  (goto-char start)
  (sh-syntax-propertize-here-doc end)
  (funcall
   (syntax-propertize-rules
    (sh-here-doc-open-re
     (2 (sh-font-lock-open-heredoc
         (match-beginning 0) (match-string 1) (match-beginning 2))))
    ("\\s|" (0 (prog1 nil (sh-syntax-propertize-here-doc end))))
    ;; A `#' begins a comment when it is unquoted and at the
    ;; beginning of a word.  In the shell, words are separated by
    ;; metacharacters.  The list of special chars is taken from
    ;; the single-unix spec of the shell command language (under
    ;; `quoting') but with `$' removed.
    ("[^|&;<>()`\\\"' \t\n]\\(#+\\)" (1 "_"))
    ;; In a '...' the backslash is not escaping.
    ("\\(\\\\\\)'" (1 (sh-font-lock-backslash-quote)))
    ;; Make sure $@ and $? are correctly recognized as sexps.
    ("\\$\\([?@]\\)" (1 "_"))
    ;; Distinguish the special close-paren in `case'.
    (")" (0 (sh-font-lock-paren (match-beginning 0))))
    ;; Highlight (possibly nested) subshells inside "" quoted
    ;; regions correctly.
    ("\"\\(?:\\(?:[^\\\"]\\|\\)*?[^\\]\\(?:\\\\\\\\\\)*\\)??\\(\\$(\\|`\\)"
     (1 (ignore
         ;; Save excursion because we want to also apply other
         ;; syntax-propertize rules within the affected region.
         (if (nth 8 (syntax-ppss))
             (goto-char (1+ (match-beginning 0)))
           (save-excursion
             (sh-font-lock-quoted-subshell end)))))))
   (point) end))

(defun sh-font-lock-syntactic-face-function (state)
  (let ((q (nth 3 state)))
    (if q
        (if (characterp q)
            (if (eq q ?\`) 'sh-quoted-exec font-lock-string-face)
          sh-heredoc-face)
      font-lock-comment-face)))

(defgroup sh-indentation nil
  "Variables controlling indentation in shell scripts.

Note: customizing these variables will not affect existing buffers if
`sh-make-vars-local' is non-nil.  See the documentation for
variable `sh-make-vars-local', command `sh-make-vars-local'
and command `sh-reset-indent-vars-to-global-values'."
  :group 'sh-script)


(defcustom sh-set-shell-hook nil
  "Hook run by `sh-set-shell'."
  :type 'hook
  :group 'sh-script)

(defcustom sh-mode-hook nil
  "Hook run by `sh-mode'."
  :type 'hook
  :group 'sh-script)

(defcustom sh-learn-basic-offset nil
  "When `sh-guess-basic-offset' should learn `sh-basic-offset'.

nil mean:              never.
t means:               only if there seems to be an obvious value.
Anything else means:   whenever we have a \"good guess\" as to the value."
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "Only if sure"  t)
	  (const :tag "If have a good guess" usually))
  :group 'sh-indentation)

(defcustom sh-popup-occur-buffer nil
  "Controls when  `sh-learn-buffer-indent' pops the `*indent*' buffer.
If t it is always shown.  If nil, it is shown only when there
are conflicts."
  :type '(choice
	  (const :tag "Only when there are conflicts." nil)
	  (const :tag "Always"  t))
  :group 'sh-indentation)

(defcustom sh-blink t
  "If non-nil, `sh-show-indent' shows the line indentation is relative to.
The position on the line is not necessarily meaningful.
In some cases the line will be the matching keyword, but this is not
always the case."
  :type 'boolean
  :group 'sh-indentation)

(defcustom sh-first-lines-indent 0
  "The indentation of the first non-blank non-comment line.
Usually 0 meaning first column.
Can be set to a number, or to nil which means leave it as is."
  :type '(choice
	  (const :tag "Leave as is"	nil)
	  (integer :tag "Column number"
		   :menu-tag "Indent to this col (0 means first col)" ))
  :group 'sh-indentation)


(defcustom sh-basic-offset 4
  "The default indentation increment.
This value is used for the `+' and `-' symbols in an indentation variable."
  :type 'integer
  :group 'sh-indentation)
(put 'sh-basic-offset 'safe-local-variable 'integerp)

(defcustom sh-indent-comment nil
  "How a comment line is to be indented.
nil means leave it as it is;
t  means indent it as a normal line, aligning it to previous non-blank
   non-comment line;
a number means align to that column, e.g. 0 means first column."
  :type '(choice
	  (const :tag "Leave as is." nil)
	  (const :tag "Indent as a normal line."  t)
	  (integer :menu-tag "Indent to this col (0 means first col)."
		   :tag "Indent to column number.") )
  :group 'sh-indentation)


(defvar sh-debug nil
  "Enable lots of debug messages - if function `sh-debug' is enabled.")


;; Uncomment this defun and comment the defmacro for debugging.
;; (defun sh-debug (&rest args)
;;   "For debugging:  display message ARGS if variable SH-DEBUG is non-nil."
;;   (if sh-debug
;;       (apply 'message args)))
(defmacro sh-debug (&rest _args))

(defconst sh-symbol-list
  '((const :tag "+ "  :value +
	   :menu-tag "+   Indent right by sh-basic-offset")
    (const :tag "- "  :value -
	   :menu-tag "-   Indent left  by sh-basic-offset")
    (const :tag "++"  :value  ++
	   :menu-tag "++  Indent right twice sh-basic-offset")
    (const :tag "--"  :value --
	   :menu-tag "--  Indent left  twice sh-basic-offset")
    (const :tag "* " :value *
	   :menu-tag "*   Indent right half sh-basic-offset")
    (const :tag "/ " :value /
	   :menu-tag "/   Indent left  half sh-basic-offset")))

(defcustom sh-indent-for-else 0
  "How much to indent an `else' relative to its `if'.  Usually 0."
  :type `(choice
	  (integer :menu-tag "A number (positive=>indent right)"
		   :tag "A number")
	  (const :tag "--") ;; separator!
	  ,@ sh-symbol-list
	  )
  :group 'sh-indentation)

(defconst sh-number-or-symbol-list
  (append '((integer :menu-tag "A number (positive=>indent right)"
		     :tag "A number")
	    (const :tag "--"))		; separator
	  sh-symbol-list))

(defcustom sh-indent-for-fi 0
  "How much to indent a `fi' relative to its `if'.  Usually 0."
  :type `(choice ,@ sh-number-or-symbol-list )
  :group 'sh-indentation)

(defcustom sh-indent-for-done 0
  "How much to indent a `done' relative to its matching stmt.  Usually 0."
  :type `(choice ,@ sh-number-or-symbol-list )
  :group 'sh-indentation)

(defcustom sh-indent-after-else '+
  "How much to indent a statement after an `else' statement."
  :type `(choice ,@ sh-number-or-symbol-list )
  :group 'sh-indentation)

(defcustom sh-indent-after-if '+
  "How much to indent a statement after an `if' statement.
This includes lines after `else' and `elif' statements, too, but
does not affect the `else', `elif' or `fi' statements themselves."
  :type `(choice ,@ sh-number-or-symbol-list )
  :group 'sh-indentation)

(defcustom sh-indent-for-then 0
  "How much to indent a `then' relative to its `if'."
  :type `(choice ,@ sh-number-or-symbol-list )
  :group 'sh-indentation)

(defcustom sh-indent-for-do 0
  "How much to indent a `do' statement.
This is relative to the statement before the `do', typically a
`while', `until', `for', `repeat' or `select' statement."
  :type `(choice ,@ sh-number-or-symbol-list)
  :group 'sh-indentation)

(defcustom sh-indent-after-do '+
  "How much to indent a line after a `do' statement.
This is used when the `do' is the first word of the line.
This is relative to the statement before the `do', typically a
`while', `until', `for', `repeat' or `select' statement."
  :type `(choice ,@ sh-number-or-symbol-list)
  :group 'sh-indentation)

(defcustom sh-indent-after-loop-construct '+
  "How much to indent a statement after a loop construct.

This variable is used when the keyword `do' is on the same line as the
loop statement (e.g., `until', `while' or `for').
If the `do' is on a line by itself, then `sh-indent-after-do' is used instead."
  :type `(choice ,@ sh-number-or-symbol-list)
  :group 'sh-indentation)


(defcustom sh-indent-after-done 0
  "How much to indent a statement after a `done' keyword.
Normally this is 0, which aligns the `done' to the matching
looping construct line.
Setting it non-zero allows you to have the `do' statement on a line
by itself and align the done under to do."
  :type `(choice ,@ sh-number-or-symbol-list)
  :group 'sh-indentation)

(defcustom sh-indent-for-case-label '+
  "How much to indent a case label statement.
This is relative to the line containing the `case' statement."
  :type `(choice ,@ sh-number-or-symbol-list)
  :group 'sh-indentation)

(defcustom sh-indent-for-case-alt '++
  "How much to indent statements after the case label.
This is relative to the line containing the `case' statement."
  :type `(choice ,@ sh-number-or-symbol-list)
  :group 'sh-indentation)


(defcustom sh-indent-for-continuation '+
  "How much to indent for a continuation statement."
  :type `(choice ,@ sh-number-or-symbol-list)
  :group 'sh-indentation)

(defcustom sh-indent-after-open '+
  "How much to indent after a line with an opening parenthesis or brace.
For an open paren after a function, `sh-indent-after-function' is used."
  :type `(choice ,@ sh-number-or-symbol-list)
  :group 'sh-indentation)

(defcustom sh-indent-after-function '+
  "How much to indent after a function line."
  :type `(choice ,@ sh-number-or-symbol-list)
  :group 'sh-indentation)

;; These 2 are for the rc shell:

(defcustom sh-indent-after-switch '+
  "How much to indent a `case' statement relative to the `switch' statement.
This is for the rc shell."
  :type `(choice ,@ sh-number-or-symbol-list)
  :group 'sh-indentation)

(defcustom sh-indent-after-case '+
  "How much to indent a statement relative to the `case' statement.
This is for the rc shell."
  :type `(choice ,@ sh-number-or-symbol-list)
  :group 'sh-indentation)

(defcustom sh-backslash-column 48
  "Column in which `sh-backslash-region' inserts backslashes."
  :type 'integer
  :group 'sh)

(defcustom sh-backslash-align t
  "If non-nil, `sh-backslash-region' will align backslashes."
  :type 'boolean
  :group 'sh)

;; Internal use - not designed to be changed by the user:

(defun sh-mkword-regexpr (word)
  "Make a regexp which matches WORD as a word.
This specifically excludes an occurrence of WORD followed by
punctuation characters like '-'."
  (concat word "\\([^-[:alnum:]_]\\|$\\)"))

(defconst sh-re-done (sh-mkword-regexpr "done"))


(defconst sh-kws-for-done
  '((sh .  ( "while" "until" "for" ) )
    (bash . ( "while" "until" "for" "select"  ) )
    (ksh88 . ( "while" "until" "for" "select"  ) )
    (zsh .  ( "while" "until" "for" "repeat" "select" ) ) )
  "Which keywords can match the word `done' in this shell.")


(defconst sh-indent-supported
  '((sh . t)
    (csh . nil)
    (rc . t))
  "Shell types that shell indenting can do something with.")

(defvar sh-indent-supported-here nil
  "Non-nil if we support indentation for the current buffer's shell type.")

(defconst sh-var-list
  '(
    sh-basic-offset sh-first-lines-indent sh-indent-after-case
    sh-indent-after-do sh-indent-after-done
    sh-indent-after-else
    sh-indent-after-if
    sh-indent-after-loop-construct
    sh-indent-after-open
    sh-indent-comment
    sh-indent-for-case-alt
    sh-indent-for-case-label
    sh-indent-for-continuation
    sh-indent-for-do
    sh-indent-for-done
    sh-indent-for-else
    sh-indent-for-fi
    sh-indent-for-then
    )
  "A list of variables used by script mode to control indentation.
This list is used when switching between buffer-local and global
values of variables, and for the commands using indentation styles.")

(defvar sh-make-vars-local t
  "*Controls whether indentation variables are local to the buffer.
If non-nil, indentation variables are made local initially.
If nil, you can later make the variables local by invoking
command `sh-make-vars-local'.
The default is t because I assume that in one Emacs session one is
frequently editing existing scripts with different styles.")


;; mode-command and utility functions

;;;###autoload
(define-derived-mode sh-mode prog-mode "Shell-script"
  "Major mode for editing shell scripts.
This mode works for many shells, since they all have roughly the same syntax,
as far as commands, arguments, variables, pipes, comments etc. are concerned.
Unless the file's magic number indicates the shell, your usual shell is
assumed.  Since filenames rarely give a clue, they are not further analyzed.

This mode adapts to the variations between shells (see `sh-set-shell') by
means of an inheritance based feature lookup (see `sh-feature').  This
mechanism applies to all variables (including skeletons) that pertain to
shell-specific features.

The default style of this mode is that of Rosenblatt's Korn shell book.
The syntax of the statements varies with the shell being used.  The
following commands are available, based on the current shell's syntax:
\\<sh-mode-map>
\\[sh-case]	 case statement
\\[sh-for]	 for loop
\\[sh-function]	 function definition
\\[sh-if]	 if statement
\\[sh-indexed-loop]	 indexed loop from 1 to n
\\[sh-while-getopts]	 while getopts loop
\\[sh-repeat]	 repeat loop
\\[sh-select]	 select loop
\\[sh-until]	 until loop
\\[sh-while]	 while loop

For sh and rc shells indentation commands are:
\\[sh-show-indent]	Show the variable controlling this line's indentation.
\\[sh-set-indent]	Set then variable controlling this line's indentation.
\\[sh-learn-line-indent]	Change the indentation variable so this line
would indent to the way it currently is.
\\[sh-learn-buffer-indent]  Set the indentation variables so the
buffer indents as it currently is indented.


\\[backward-delete-char-untabify]	 Delete backward one position, even if it was a tab.
\\[newline-and-indent]	 Delete unquoted space and indent new line same as this one.
\\[sh-end-of-command]	 Go to end of successive commands.
\\[sh-beginning-of-command]	 Go to beginning of successive commands.
\\[sh-set-shell]	 Set this buffer's shell, and maybe its magic number.
\\[sh-execute-region]	 Have optional header and region be executed in a subshell.

\\[sh-maybe-here-document]	 Without prefix, following an unquoted < inserts here document.
\{, (, [, ', \", `
	Unless quoted with \\, insert the pairs {}, (), [], or '', \"\", ``.

If you generally program a shell different from your login shell you can
set `sh-shell-file' accordingly.  If your shell's file name doesn't correctly
indicate what shell it is use `sh-alias-alist' to translate.

If your shell gives error messages with line numbers, you can use \\[executable-interpret]
with your script for an edit-interpret-debug cycle."
  (make-local-variable 'sh-shell-file)
  (make-local-variable 'sh-shell)

  (set (make-local-variable 'skeleton-pair-default-alist)
       sh-skeleton-pair-default-alist)
  (set (make-local-variable 'skeleton-end-hook)
       (lambda () (or (eolp) (newline) (indent-relative))))

  (set (make-local-variable 'paragraph-start) (concat page-delimiter "\\|$"))
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "#+[\t ]*")
  (set (make-local-variable 'local-abbrev-table) sh-mode-abbrev-table)
  (set (make-local-variable 'comint-dynamic-complete-functions)
       sh-dynamic-complete-functions)
  ;; we can't look if previous line ended with `\'
  (set (make-local-variable 'comint-prompt-regexp) "^[ \t]*")
  (set (make-local-variable 'imenu-case-fold-search) nil)
  (set (make-local-variable 'font-lock-defaults)
       `((sh-font-lock-keywords
          sh-font-lock-keywords-1 sh-font-lock-keywords-2)
         nil nil
         ((?/ . "w") (?~ . "w") (?. . "w") (?- . "w") (?_ . "w")) nil
         (font-lock-syntactic-face-function
          . sh-font-lock-syntactic-face-function)))
  (set (make-local-variable 'syntax-propertize-function)
       #'sh-syntax-propertize-function)
  (add-hook 'syntax-propertize-extend-region-functions
            #'syntax-propertize-multiline 'append 'local)
  (set (make-local-variable 'skeleton-pair-alist) '((?` _ ?`)))
  (set (make-local-variable 'skeleton-pair-filter-function) 'sh-quoted-p)
  (set (make-local-variable 'skeleton-further-elements)
       '((< '(- (min sh-indentation (current-column))))))
  (set (make-local-variable 'skeleton-filter-function) 'sh-feature)
  (set (make-local-variable 'skeleton-newline-indent-rigidly) t)
  (set (make-local-variable 'sh-indent-supported-here) nil)
  (set (make-local-variable 'defun-prompt-regexp)
       (concat "^\\(function[ \t]\\|[[:alnum:]]+[ \t]+()[ \t]+\\)"))
  ;; Parse or insert magic number for exec, and set all variables depending
  ;; on the shell thus determined.
  (sh-set-shell
   (cond ((save-excursion
            (goto-char (point-min))
            (looking-at "#![ \t]?\\([^ \t\n]*/bin/env[ \t]\\)?\\([^ \t\n]+\\)"))
          (match-string 2))
         ((not buffer-file-name)
          sh-shell-file)
         ;; Checks that use `buffer-file-name' follow.
         ((string-match "\\.m?spec\\'" buffer-file-name)
          "rpm")
         ((string-match "[.]sh\\>" buffer-file-name)
          "sh")
         ((string-match "[.]bash\\>" buffer-file-name)
          "bash")
         ((string-match "[.]ksh\\>" buffer-file-name)
          "ksh")
         ((string-match "[.]csh\\>" buffer-file-name)
          "csh")
	 ((equal (file-name-nondirectory buffer-file-name) ".profile")
          "sh")
         (t
          sh-shell-file))
   nil nil))

;;;###autoload
(defalias 'shell-script-mode 'sh-mode)


(defun sh-font-lock-keywords (&optional keywords)
  "Function to get simple fontification based on `sh-font-lock-keywords'.
This adds rules for comments and assignments."
  (sh-feature sh-font-lock-keywords-var
	      (when (stringp (sh-feature sh-assignment-regexp))
		(lambda (list)
		  `((,(sh-feature sh-assignment-regexp)
		     1 font-lock-variable-name-face)
		    ,@keywords
		    ,@list
		    ,@executable-font-lock-keywords)))))

(defun sh-font-lock-keywords-1 (&optional builtins)
  "Function to get better fontification including keywords."
  (let ((keywords (concat "\\([;(){}`|&]\\|^\\)[ \t]*\\(\\("
			  (regexp-opt (sh-feature sh-leading-keywords) t)
			  "[ \t]+\\)?"
			  (regexp-opt (append (sh-feature sh-leading-keywords)
					      (sh-feature sh-other-keywords))
				      t))))
    (sh-font-lock-keywords
     `(,@(if builtins
	     `((,(concat keywords "[ \t]+\\)?"
			 (regexp-opt (sh-feature sh-builtins) t)
			 "\\>")
		(2 font-lock-keyword-face nil t)
		(6 font-lock-builtin-face))
	       ,@(sh-feature sh-font-lock-keywords-var-2)))
	 (,(concat keywords "\\)\\>")
	  2 font-lock-keyword-face)
	 ,@(sh-feature sh-font-lock-keywords-var-1)))))

(defun sh-font-lock-keywords-2 ()
  "Function to get better fontification including keywords and builtins."
  (sh-font-lock-keywords-1 t))


(defvar sh-regexp-for-done nil
  "A buffer-local regexp to match opening keyword for done.")

(defvar sh-kw-alist nil
  "A buffer-local, since it is shell-type dependent, list of keywords.")

;; ( key-word  first-on-this  on-prev-line )
;; This is used to set `sh-kw-alist' which is a list of sublists each
;; having 3 elements:
;;   a keyword
;;   a rule to check when the keyword appears on "this" line
;;   a rule to check when the keyword appears on "the previous" line
;; The keyword is usually a string and is the first word on a line.
;; If this keyword appears on the line whose indentation is to be
;; calculated, the rule in element 2 is called.  If this returns
;; non-zero, the resulting point (which may be changed by the rule)
;; is used as the default indentation.
;; If it returned false or the keyword was not found in the table,
;; then the keyword from the previous line is looked up and the rule
;; in element 3 is called.  In this case, however,
;; `sh-get-indent-info' does not stop but may keep going and test
;; other keywords against rules in element 3.  This is because the
;; preceding line could have, for example, an opening "if" and an
;; opening "while" keyword and we need to add the indentation offsets
;; for both.
;;
(defconst sh-kw
  '((sh
     ("if" nil sh-handle-prev-if)
     ("elif" sh-handle-this-else sh-handle-prev-else)
     ("else" sh-handle-this-else sh-handle-prev-else)
     ("fi" sh-handle-this-fi sh-handle-prev-fi)
     ("then" sh-handle-this-then sh-handle-prev-then)
     ("(" nil sh-handle-prev-open)
     ("{" nil sh-handle-prev-open)
     ("[" nil sh-handle-prev-open)
     ("}" sh-handle-this-close nil)
     (")" sh-handle-this-close nil)
     ("]" sh-handle-this-close nil)
     ("case" nil sh-handle-prev-case)
     ("esac" sh-handle-this-esac sh-handle-prev-esac)
     (case-label nil sh-handle-after-case-label) ;; ???
     (";;" nil sh-handle-prev-case-alt-end) ;; ???
     (";;&" nil sh-handle-prev-case-alt-end) ;Like ";;" with diff semantics.
     (";&" nil sh-handle-prev-case-alt-end) ;Like ";;" with diff semantics.
     ("done" sh-handle-this-done sh-handle-prev-done)
     ("do" sh-handle-this-do sh-handle-prev-do))

    ;; Note: we don't need specific stuff for bash and zsh shells;
    ;; the regexp `sh-regexp-for-done' handles the extra keywords
    ;; these shells use.
    (rc
     ("{" nil sh-handle-prev-open)
     ("}" sh-handle-this-close nil)
     ("case" sh-handle-this-rc-case sh-handle-prev-rc-case))))



(defun sh-set-shell (shell &optional no-query-flag insert-flag)
  "Set this buffer's shell to SHELL (a string).
When used interactively, insert the proper starting #!-line,
and make the visited file executable via `executable-set-magic',
perhaps querying depending on the value of `executable-query'.

When this function is called noninteractively, INSERT-FLAG (the third
argument) controls whether to insert a #!-line and think about making
the visited file executable, and NO-QUERY-FLAG (the second argument)
controls whether to query about making the visited file executable.

Calls the value of `sh-set-shell-hook' if set."
  (interactive (list (completing-read (format "Shell \(default %s\): "
 					      sh-shell-file)
  				      interpreter-mode-alist
 				      (lambda (x) (eq (cdr x) 'sh-mode))
 				      nil nil nil sh-shell-file)
		     (eq executable-query 'function)
		     t))
  (if (string-match "\\.exe\\'" shell)
      (setq shell (substring shell 0 (match-beginning 0))))
  (setq sh-shell (intern (file-name-nondirectory shell))
	sh-shell (or (cdr (assq sh-shell sh-alias-alist))
		     sh-shell))
  (if insert-flag
      (setq sh-shell-file
	    (executable-set-magic shell (sh-feature sh-shell-arg)
				  no-query-flag insert-flag)))
  (setq mode-line-process (format "[%s]" sh-shell))
  (set (make-local-variable 'sh-shell-variables) nil)
  (set (make-local-variable 'sh-shell-variables-initialized) nil)
  (set (make-local-variable 'imenu-generic-expression)
       (sh-feature sh-imenu-generic-expression))
  (let ((tem (sh-feature sh-mode-syntax-table-input)))
    (when tem
      (set (make-local-variable 'sh-mode-syntax-table)
           (apply 'sh-mode-syntax-table tem))
      (set-syntax-table sh-mode-syntax-table)))
  (dolist (var (sh-feature sh-variables))
    (sh-remember-variable var))
  (if (setq sh-indent-supported-here (sh-feature sh-indent-supported))
      (progn
	(message "Setting up indent for shell type %s" sh-shell)
	(set (make-local-variable 'parse-sexp-lookup-properties) t)
	(set (make-local-variable 'sh-kw-alist) (sh-feature sh-kw))
	(let ((regexp (sh-feature sh-kws-for-done)))
	  (if regexp
	      (set (make-local-variable 'sh-regexp-for-done)
		   (sh-mkword-regexpr (regexp-opt regexp t)))))
	(message "setting up indent stuff")
	;; sh-mode has already made indent-line-function local
	;; but do it in case this is called before that.
	(set (make-local-variable 'indent-line-function) 'sh-indent-line)
	(if sh-make-vars-local
	    (sh-make-vars-local))
	(message "Indentation setup for shell type %s" sh-shell))
    (message "No indentation for this shell type.")
    (setq indent-line-function 'sh-basic-indent-line))
  (when font-lock-mode
    (setq font-lock-set-defaults nil)
    (font-lock-set-defaults)
    (font-lock-fontify-buffer))
  (run-hooks 'sh-set-shell-hook))


(defun sh-feature (alist &optional function)
  "Index ALIST by the current shell.
If ALIST isn't a list where every element is a cons, it is returned as is.
Else indexing follows an inheritance logic which works in two ways:

  - Fall back on successive ancestors (see `sh-ancestor-alist') as long as
    the alist contains no value for the current shell.
    The ultimate default is always `sh'.

  - If the value thus looked up is a list starting with `sh-append',
    we call the function `sh-append' with the rest of the list as
    arguments, and use the value.  However, the next element of the
    list is not used as-is; instead, we look it up recursively
    in ALIST to allow the function called to define the value for
    one shell to be derived from another shell.
    The value thus determined is physically replaced into the alist.

If FUNCTION is non-nil, it is called with one argument,
the value thus obtained, and the result is used instead."
  (or (if (consp alist)
	  ;; Check for something that isn't a valid alist.
	  (let ((l alist))
	    (while (and l (consp (car l)))
	      (setq l (cdr l)))
	    (if l alist)))

      (let ((orig-sh-shell sh-shell))
	(let ((sh-shell sh-shell)
	      elt val)
	  (while (and sh-shell
		      (not (setq elt (assq sh-shell alist))))
	    (setq sh-shell (cdr (assq sh-shell sh-ancestor-alist))))
	  ;; If the shell is not known, treat it as sh.
	  (unless elt
	    (setq elt (assq 'sh alist)))
	  (setq val (cdr elt))
	  (if (and (consp val)
		   (memq (car val) '(sh-append sh-modify)))
	      (setq val
		    (apply (car val)
			   ;; Refer to the value for a different shell,
			   ;; as a kind of inheritance.
			   (let ((sh-shell (car (cdr val))))
			     (sh-feature alist))
			   (cddr val))))
	  (if function
	      (setq sh-shell orig-sh-shell
		    val (funcall function val)))
	  val))))



;; I commented this out because nobody calls it -- rms.
;;(defun sh-abbrevs (ancestor &rest list)
;;  "Iff it isn't, define the current shell as abbrev table and fill that.
;;Abbrev table will inherit all abbrevs from ANCESTOR, which is either an abbrev
;;table or a list of (NAME1 EXPANSION1 ...).  In addition it will define abbrevs
;;according to the remaining arguments NAMEi EXPANSIONi ...
;;EXPANSION may be either a string or a skeleton command."
;;  (or (if (boundp sh-shell)
;;	  (symbol-value sh-shell))
;;      (progn
;;	(if (listp ancestor)
;;	    (nconc list ancestor))
;;	(define-abbrev-table sh-shell ())
;;	(if (vectorp ancestor)
;;	    (mapatoms (lambda (atom)
;;			(or (eq atom 0)
;;			    (define-abbrev (symbol-value sh-shell)
;;			      (symbol-name atom)
;;			      (symbol-value atom)
;;			      (symbol-function atom))))
;;		      ancestor))
;;	(while list
;;	  (define-abbrev (symbol-value sh-shell)
;;	    (car list)
;;	    (if (stringp (car (cdr list)))
;;		(car (cdr list))
;;	      "")
;;	    (if (symbolp (car (cdr list)))
;;		(car (cdr list))))
;;	  (setq list (cdr (cdr list)))))
;;      (symbol-value sh-shell)))


(defun sh-append (ancestor &rest list)
  "Return list composed of first argument (a list) physically appended to rest."
  (nconc list ancestor))


(defun sh-modify (skeleton &rest list)
  "Modify a copy of SKELETON by replacing I1 with REPL1, I2 with REPL2 ..."
  (setq skeleton (copy-sequence skeleton))
  (while list
    (setcar (or (nthcdr (car list) skeleton)
		(error "Index %d out of bounds" (car list)))
	    (car (cdr list)))
    (setq list (nthcdr 2 list)))
  skeleton)


(defun sh-basic-indent-line ()
  "Indent a line for Sh mode (shell script mode).
Indent as far as preceding non-empty line, then by steps of `sh-indentation'.
Lines containing only comments are considered empty."
  (interactive)
  (let ((previous (save-excursion
		    (while (and (progn (beginning-of-line)
				       (not (bobp)))
				(progn
				  (forward-line -1)
				  (back-to-indentation)
				  (or (eolp)
				      (eq (following-char) ?#)))))
		    (current-column)))
	current)
    (save-excursion
      (indent-to (if (eq this-command 'newline-and-indent)
		     previous
		   (if (< (current-column)
			  (setq current (progn (back-to-indentation)
					       (current-column))))
		       (if (eolp) previous 0)
		     (delete-region (point)
				    (progn (beginning-of-line) (point)))
		     (if (eolp)
			 (max previous (* (1+ (/ current sh-indentation))
					  sh-indentation))
		       (* (1+ (/ current sh-indentation)) sh-indentation))))))
    (if (< (current-column) (current-indentation))
	(skip-chars-forward " \t"))))


(defun sh-execute-region (start end &optional flag)
  "Pass optional header and region to a subshell for noninteractive execution.
The working directory is that of the buffer, and only environment variables
are already set which is why you can mark a header within the script.

With a positive prefix ARG, instead of sending region, define header from
beginning of buffer to point.  With a negative prefix ARG, instead of sending
region, clear header."
  (interactive "r\nP")
  (if flag
      (setq sh-header-marker (if (> (prefix-numeric-value flag) 0)
				 (point-marker)))
    (if sh-header-marker
	(save-excursion
	  (let (buffer-undo-list)
	    (goto-char sh-header-marker)
	    (append-to-buffer (current-buffer) start end)
	    (shell-command-on-region (point-min)
				     (setq end (+ sh-header-marker
						  (- end start)))
				     sh-shell-file)
	    (delete-region sh-header-marker end)))
      (shell-command-on-region start end (concat sh-shell-file " -")))))


(defun sh-remember-variable (var)
  "Make VARIABLE available for future completing reads in this buffer."
  (or (< (length var) sh-remember-variable-min)
      (getenv var)
      (assoc var sh-shell-variables)
      (push (cons var var) sh-shell-variables))
  var)



(defun sh-quoted-p ()
  "Is point preceded by an odd number of backslashes?"
  (eq -1 (% (save-excursion (skip-chars-backward "\\\\")) 2)))

;; Indentation stuff.
(defun sh-must-support-indent ()
  "*Signal an error if the shell type for this buffer is not supported.
Also, the buffer must be in Shell-script mode."
  (unless sh-indent-supported-here
    (error "This buffer's shell does not support indentation through Emacs")))

(defun sh-make-vars-local ()
  "Make the indentation variables local to this buffer.
Normally they already are local.  This command is provided in case
variable `sh-make-vars-local' has been set to nil.

To revert all these variables to the global values, use
command `sh-reset-indent-vars-to-global-values'."
  (interactive)
  (mapc 'make-local-variable sh-var-list)
  (message "Indentation variables are now local."))

(defun sh-reset-indent-vars-to-global-values ()
  "Reset local indentation variables to the global values.
Then, if variable `sh-make-vars-local' is non-nil, make them local."
  (interactive)
  (mapc 'kill-local-variable sh-var-list)
  (if sh-make-vars-local
      (mapcar 'make-local-variable sh-var-list)))


;; Theoretically these are only needed in shell and derived modes.
;; However, the routines which use them are only called in those modes.
(defconst sh-special-keywords "then\\|do")

(defun sh-help-string-for-variable (var)
  "Construct a string for `sh-read-variable' when changing variable VAR ."
  (let ((msg (documentation-property var 'variable-documentation))
	(msg2 ""))
    (unless (memq var '(sh-first-lines-indent sh-indent-comment))
      (setq msg2
	    (format "\n
You can enter a number (positive to increase indentation,
negative to decrease indentation, zero for no change to indentation).

Or, you can enter one of the following symbols which are relative to
the value of variable `sh-basic-offset'
which in this buffer is currently %s.

\t%s."
		    sh-basic-offset
		    (mapconcat (lambda (x)
				 (nth (1- (length x)) x))
			       sh-symbol-list  "\n\t"))))
    (concat
     ;; The following shows the global not the local value!
     ;; (format "Current value of %s is %s\n\n" var (symbol-value var))
     msg msg2)))

(defun sh-read-variable (var)
  "Read a new value for indentation variable VAR."
  (interactive "*variable? ") ;; to test
  (let ((minibuffer-help-form `(sh-help-string-for-variable
				(quote ,var)))
	val)
    (setq val (read-from-minibuffer
	       (format "New value for %s (press %s for help): "
		       var  (single-key-description help-char))
	       (format "%s" (symbol-value var))
	       nil t))
    val))



(defun sh-in-comment-or-string (start)
  "Return non-nil if START is in a comment or string."
  (save-excursion
    (let ((state (syntax-ppss start)))
      (or (nth 3 state) (nth 4 state)))))

(defun sh-goto-matching-if ()
  "Go to the matching if for a fi.
This handles nested if..fi pairs."
  (let ((found (sh-find-prev-matching "\\bif\\b" "\\bfi\\b" 1)))
    (if found
	(goto-char found))))


;; Functions named sh-handle-this-XXX are called when the keyword on the
;; line whose indentation is being handled contain XXX;
;; those named sh-handle-prev-XXX are when XXX appears on the previous line.

(defun sh-handle-prev-if ()
  (list '(+ sh-indent-after-if)))

(defun sh-handle-this-else ()
  (if (sh-goto-matching-if)
      ;; (list "aligned to if")
      (list "aligned to if" '(+ sh-indent-for-else))
    nil
    ))

(defun sh-handle-prev-else ()
  (if (sh-goto-matching-if)
      (list  '(+ sh-indent-after-if))
    ))

(defun sh-handle-this-fi ()
  (if (sh-goto-matching-if)
      (list "aligned to if" '(+ sh-indent-for-fi))
    nil
    ))

(defun sh-handle-prev-fi ()
  ;; Why do we have this rule?  Because we must go back to the if
  ;; to get its indent.  We may continue back from there.
  ;; We return nil because we don't have anything to add to result,
  ;; the side affect of setting align-point is all that matters.
  ;; we could return a comment (a string) but I can't think of a good one...
  (sh-goto-matching-if)
  nil)

(defun sh-handle-this-then ()
  (let ((p (sh-goto-matching-if)))
    (if p
	(list '(+ sh-indent-for-then))
      )))

(defun sh-handle-prev-then ()
  (let ((p (sh-goto-matching-if)))
    (if p
	(list '(+ sh-indent-after-if))
      )))

(defun sh-handle-prev-open ()
  (save-excursion
    (let ((x (sh-prev-stmt)))
      (if (and x
	       (progn
		 (goto-char x)
		 (or
		  (looking-at "function\\b")
		  (looking-at "\\s-*\\S-+\\s-*()")
		  )))
	  (list '(+ sh-indent-after-function))
	(list '(+ sh-indent-after-open)))
      )))

(defun sh-handle-this-close ()
  (forward-char 1) ;; move over ")"
  (if (sh-safe-forward-sexp -1)
      (list "aligned to opening paren")))

(defun sh-goto-matching-case ()
  (let ((found (sh-find-prev-matching "\\bcase\\b" "\\besac\\b" 1)))
    (if found (goto-char found))))

(defun sh-handle-prev-case ()
  ;; This is typically called when point is on same line as a case
  ;; we shouldn't -- and can't find prev-case
  (if (looking-at ".*\\<case\\>")
      (list '(+ sh-indent-for-case-label))
    (error "We don't seem to be on a line with a case"))) ;; debug

(defun sh-handle-this-esac ()
  (if (sh-goto-matching-case)
      (list "aligned to matching case")))

(defun sh-handle-prev-esac ()
  (if (sh-goto-matching-case)
      (list "matching case")))

(defun sh-handle-after-case-label ()
  (if (sh-goto-matching-case)
      (list '(+ sh-indent-for-case-alt))))

(defun sh-handle-prev-case-alt-end ()
  (if (sh-goto-matching-case)
      (list '(+ sh-indent-for-case-label))))

(defun sh-safe-forward-sexp (&optional arg)
  "Try and do a `forward-sexp', but do not error.
Return new point if successful, nil if an error occurred."
  (condition-case nil
      (progn
	(forward-sexp (or arg 1))
	(point))	;; return point if successful
    (error
     (sh-debug "oops!(1) %d" (point))
     nil))) ;; return nil if fail

(defun sh-goto-match-for-done ()
  (let ((found (sh-find-prev-matching sh-regexp-for-done sh-re-done 1)))
    (if found
	(goto-char found))))

(defun sh-handle-this-done ()
  (if (sh-goto-match-for-done)
      (list  "aligned to do stmt"  '(+ sh-indent-for-done))))

(defun sh-handle-prev-done ()
  (if (sh-goto-match-for-done)
      (list "previous done")))

(defun sh-handle-this-do ()
  (if (sh-goto-match-for-done)
      (list '(+ sh-indent-for-do))))

(defun sh-handle-prev-do ()
  (cond
   ((save-restriction
      (narrow-to-region (point) (line-beginning-position))
      (sh-goto-match-for-done))
    (sh-debug "match for done found on THIS line")
    (list '(+ sh-indent-after-loop-construct)))
   ((sh-goto-match-for-done)
    (sh-debug "match for done found on PREV line")
    (list '(+ sh-indent-after-do)))
   (t
    (message "match for done NOT found")
    nil)))

;; for rc:
(defun sh-find-prev-switch ()
  "Find the line for the switch keyword matching this line's case keyword."
  (re-search-backward "\\<switch\\>" nil t))

(defun sh-handle-this-rc-case ()
  (if (sh-find-prev-switch)
      (list  '(+ sh-indent-after-switch))
    ;; (list  '(+ sh-indent-for-case-label))
    nil))

(defun sh-handle-prev-rc-case ()
  (list '(+ sh-indent-after-case)))

(defun sh-check-rule (n thing)
  (let ((rule (nth n (assoc thing sh-kw-alist)))
	(val nil))
    (if rule
	(progn
	  (setq val (funcall rule))
	  (sh-debug "rule (%d) for %s at %d is %s\n-> returned %s"
		    n thing (point) rule val)))
    val))


(defun sh-get-indent-info ()
  "Return indent-info for this line.
This is a list.  nil means the line is to be left as is.
Otherwise it contains one or more of the following sublists:
\(t NUMBER\)   NUMBER is the base location in the buffer that indentation is
	     relative to.  If present, this is always the first of the
	     sublists.  The indentation of the line in question is
	     derived from the indentation of this point, possibly
	     modified by subsequent sublists.
\(+ VAR\)
\(- VAR\)      Get the value of variable VAR and add to or subtract from
	     the indentation calculated so far.
\(= VAR\)	     Get the value of variable VAR and *replace* the
	     indentation with its value.  This only occurs for
	     special variables such as `sh-indent-comment'.
STRING	     This is ignored for the purposes of calculating
	     indentation, it is printed in certain cases to help show
	     what the indentation is based on."
  ;; See comments before `sh-kw'.
  (save-excursion
    (let ((have-result nil)
	  this-kw
	  val
	  (result nil)
	  (align-point nil)
	  prev-line-end x)
      (beginning-of-line)
      ;; Note: setting result to t means we are done and will return nil.
      ;;(This function never returns just t.)
      (cond
       ((or (nth 3 (syntax-ppss (point)))
	    (eq (get-text-property (point) 'face) sh-heredoc-face))
	;; String continuation -- don't indent
	(setq result t)
	(setq have-result t))
       ((looking-at "\\s-*#")		; was (equal this-kw "#")
	(if (bobp)
	    (setq result t) ;; return nil if 1st line!
	  (setq result (list '(= sh-indent-comment)))
	  ;; we still need to get previous line in case
	  ;; sh-indent-comment is t (indent as normal)
	  (setq align-point (sh-prev-line nil))
	  (setq have-result nil)
	  ))
       ) ;; cond

      (unless have-result
	;; Continuation lines are handled specially
	(if (sh-this-is-a-continuation)
	    (progn
              (setq result
                    (if (save-excursion
                          (beginning-of-line)
                          (not (memq (char-before (- (point) 2)) '(?\s ?\t))))
                        ;; By convention, if the continuation \ is not
                        ;; preceded by a SPC or a TAB it means that the line
                        ;; is cut at a place where spaces cannot be freely
                        ;; added/removed.  I.e. do not indent the line.
                        (list '(= nil))
                      ;; We assume the line being continued is already
                      ;; properly indented...
                      ;; (setq prev-line-end (sh-prev-line))
                      (setq align-point (sh-prev-line nil))
                      (list '(+ sh-indent-for-continuation))))
	      (setq have-result t))
	  (beginning-of-line)
	  (skip-chars-forward " \t")
	  (setq this-kw (sh-get-kw)))

        ;; Handle "this" keyword:  first word on the line we're
	;; calculating indentation info for.
	(if this-kw
	    (if (setq val (sh-check-rule 1 this-kw))
		(progn
		  (setq align-point (point))
		  (sh-debug
		   "this - setting align-point to %d" align-point)
		  (setq result (append result val))
		  (setq have-result t)
		  ;; set prev-line to continue processing remainder
		  ;; of this line as a previous line
		  (setq prev-line-end (point))
		  ))))

      (unless have-result
	(setq prev-line-end (sh-prev-line 'end)))

      (if prev-line-end
	  (save-excursion
	    ;; We start off at beginning of this line.
	    ;; Scan previous statements while this is <=
	    ;; start of previous line.
	    (goto-char prev-line-end)
	    (setq x t)
	    (while (and x (setq x  (sh-prev-thing)))
	      (sh-debug "at %d x is: %s  result is: %s" (point) x result)
	      (cond
	       ((and (equal x ")")
		     (equal (get-text-property (1- (point)) 'syntax-table)
			    sh-st-punc))
		(sh-debug "Case label) here")
		(setq x 'case-label)
		(if (setq val (sh-check-rule 2 x))
		    (progn
		      (setq result (append result val))
		      (setq align-point (point))))
		(or (bobp)
		    (forward-char -1))
                ;; FIXME: This charset looks too much like a regexp.  --Stef
		(skip-chars-forward "[a-z0-9]*?")
		)
	       ((string-match "[])}]" x)
		(setq x (sh-safe-forward-sexp -1))
		(if x
		    (progn
		      (setq align-point (point))
		      (setq result (append result
					   (list "aligned to opening paren")))
		      )))
	       ((string-match "[[({]" x)
		(sh-debug "Checking special thing: %s" x)
		(if (setq val (sh-check-rule 2 x))
		    (setq result (append result val)))
		(forward-char -1)
		(setq align-point (point)))
	       ((string-match "[\"'`]" x)
		(sh-debug "Skipping back for %s" x)
		;; this was oops-2
		(setq x (sh-safe-forward-sexp -1)))
	       ((stringp x)
		(sh-debug "Checking string %s at %s" x (point))
		(if (setq val (sh-check-rule 2 x))
		    ;; (or (eq t (car val))
		    ;; (eq t (car (car val))))
		    (setq result (append result val)))
		;; not sure about this test Wed Jan 27 23:48:35 1999
		(setq align-point (point))
		(unless (bolp)
		  (forward-char -1)))
	       (t
		(error "Don't know what to do with %s" x))
	       )
	      )	;; while
	    (sh-debug "result is %s" result)
	    )
	(sh-debug "No prev line!")
	(sh-debug "result: %s  align-point: %s" result align-point)
	)

      (if align-point
	  ;; was: (setq result (append result (list (list t align-point))))
	  (setq result (append  (list (list t align-point)) result))
	)
      (sh-debug "result is now: %s" result)

      (or result
	  (setq result (list (if prev-line-end
                                 (list t prev-line-end)
                               (list '= 'sh-first-lines-indent)))))

      (if (eq result t)
	  (setq result nil))
      (sh-debug  "result is: %s" result)
      result
      )	;; let
    ))


(defun sh-get-indent-var-for-line (&optional info)
  "Return the variable controlling indentation for this line.
If there is not [just] one such variable, return a string
indicating the problem.
If INFO is supplied it is used, else it is calculated."
  (let ((var nil)
	(result nil)
	(reason nil)
	sym elt)
    (or info
	(setq info (sh-get-indent-info)))
    (if (null info)
	(setq result "this line to be left as is")
      (while (and info (null result))
	(setq elt (car info))
	(cond
	 ((stringp elt)
	  (setq reason elt)
	  )
	 ((not (listp elt))
	  (error "sh-get-indent-var-for-line invalid elt: %s" elt))
	 ;; so it is a list
	 ((eq t (car elt))
	  ) ;; nothing
	 ((symbolp  (setq sym (nth 1 elt)))
	  ;; A bit of a kludge - when we see the sh-indent-comment
	  ;; ignore other variables.  Otherwise it is tricky to
	  ;; "learn" the comment indentation.
	  (if (eq var 'sh-indent-comment)
	      (setq result var)
	    (if var
		(setq result
		      "this line is controlled by more than 1 variable.")
	      (setq var sym))))
	 (t
	  (error "sh-get-indent-var-for-line invalid list elt: %s" elt)))
	(setq info (cdr info))
	))
    (or result
	(setq result var))
    (or result
	(setq result reason))
    (if (null result)
	;; e.g. just had (t POS)
	(setq result "line has default indentation"))
    result))



;; Finding the previous line isn't trivial.
;; We must *always* go back one more and see if that is a continuation
;; line -- it is the PREVIOUS line which is continued, not the one
;; we are going to!
;; Also, we want to treat a whole "here document" as one big line,
;; because we may want to a align to the beginning of it.
;;
;; What we do:
;; - go back to previous non-empty line
;; - if this is in a here-document, go to the beginning of it
;; - while previous line is continued, go back one line
(defun sh-prev-line (&optional end)
  "Back to end of previous non-comment non-empty line.
Go to beginning of logical line unless END is non-nil, in which case
we go to the end of the previous line and do not check for continuations."
  (save-excursion
    (beginning-of-line)
    (forward-comment (- (point-max)))
    (unless end (beginning-of-line))
    (when (and (not (bobp))
	       (equal (get-text-property (1- (point)) 'face)
		      sh-heredoc-face))
      (let ((p1 (previous-single-property-change (1- (point)) 'face)))
	(when p1
	  (goto-char p1)
	  (if end
	      (end-of-line)
	    (beginning-of-line)))))
    (unless end
      ;; we must check previous lines to see if they are continuation lines
      ;; if so, we must return position of first of them
      (while (and (sh-this-is-a-continuation)
		  (>= 0 (forward-line -1))))
      (beginning-of-line)
      (skip-chars-forward " \t"))
    (point)))


(defun sh-prev-stmt ()
  "Return the address of the previous stmt or nil."
  ;; This is used when we are trying to find a matching keyword.
  ;; Searching backward for the keyword would certainly be quicker, but
  ;; it is hard to remove "false matches" -- such as if the keyword
  ;; appears in a string or quote.  This way is slower, but (I think) safer.
  (interactive)
  (save-excursion
    (let ((going t)
	  (start (point))
	  (found nil)
	  (prev nil))
      (skip-chars-backward " \t;|&({[")
      (while (and (not found)
		  (not (bobp))
		  going)
	;; Do a backward-sexp if possible, else backup bit by bit...
	(if (sh-safe-forward-sexp -1)
	    (progn
	      (if (looking-at sh-special-keywords)
		  (progn
		    (setq found prev))
		(setq prev (point))
		))
	  ;; backward-sexp failed
	  (if (zerop (skip-chars-backward " \t()[\]{};`'"))
	      (forward-char -1))
	  (if (bolp)
	      (let ((back (sh-prev-line nil)))
		(if back
		    (goto-char back)
		  (setq going nil)))))
	(unless found
	  (skip-chars-backward " \t")
	  (if (or (and (bolp) (not (sh-this-is-a-continuation)))
		  (eq (char-before) ?\;)
		  (looking-at "\\s-*[|&]"))
	      (setq found (point)))))
      (if found
	  (goto-char found))
      (if found
	  (progn
	    (skip-chars-forward " \t|&({[")
	    (setq found (point))))
      (if (>= (point) start)
	  (progn
	    (debug "We didn't move!")
	    (setq found nil))
	(or found
	    (sh-debug "Did not find prev stmt.")))
      found)))


(defun sh-get-word ()
  "Get a shell word skipping whitespace from point."
  (interactive)
  (skip-chars-forward "\t ")
  (let ((start (point)))
    (while
	(if (looking-at "[\"'`]")
	    (sh-safe-forward-sexp)
	  ;; (> (skip-chars-forward "^ \t\n\"'`") 0)
	  (> (skip-chars-forward "-_$[:alnum:]") 0)
	  ))
    (buffer-substring start (point))
    ))

(defun sh-prev-thing ()
  "Return the previous thing this logical line."
  ;; This is called when `sh-get-indent-info' is working backwards on
  ;; the previous line(s) finding what keywords may be relevant for
  ;; indenting.  It moves over sexps if possible, and will stop
  ;; on a ; and at the beginning of a line if it is not a continuation
  ;; line.
  ;;
  ;; Added a kludge for ";;"
  ;; Possible return values:
  ;;  nil  -  nothing
  ;; a string - possibly a keyword
  ;;
  (if (bolp)
      nil
    (let ((start (point))
          (min-point (if (sh-this-is-a-continuation)
                         (sh-prev-line nil)
                       (line-beginning-position))))
      (skip-chars-backward " \t;" min-point)
      (if (looking-at "\\s-*;[;&]")
          ;; (message "Found ;; !")
          ";;"
        (skip-chars-backward "^)}];\"'`({[" min-point)
        (let ((c (if (> (point) min-point) (char-before))))
          (sh-debug "stopping at %d c is %s  start=%d min-point=%d"
                    (point) c start min-point)
          (if (not (memq c '(?\n nil ?\;)))
              ;; c	-- return a string
              (char-to-string c)
            ;; Return the leading keyword of the "command" we supposedly
            ;; skipped over.  Maybe we skipped too far (e.g. past a `do' or
            ;; `then' that precedes the actual command), so check whether
            ;; we're looking at such a keyword and if so, move back forward.
            (let ((boundary (point))
                  kwd next)
              (while
                  (progn
                    ;; Skip forward over white space newline and \ at eol.
                    (skip-chars-forward " \t\n\\\\" start)
                    (if (>= (point) start)
                        (progn
                          (sh-debug "point: %d >= start: %d" (point) start)
                          nil)
                      (if next (setq boundary next))
                      (sh-debug "Now at %d   start=%d" (point) start)
                      (setq kwd (sh-get-word))
                      (if (member kwd (sh-feature sh-leading-keywords))
                          (progn
                            (setq next (point))
                            t)
                        nil))))
              (goto-char boundary)
              kwd)))))))


(defun sh-this-is-a-continuation ()
  "Return non-nil if current line is a continuation of previous line."
  (save-excursion
    (and (zerop (forward-line -1))
	 (looking-at ".*\\\\$")
	 (not (nth 4 (parse-partial-sexp (match-beginning 0) (match-end 0)
					 nil nil nil t))))))

(defun sh-get-kw (&optional where and-move)
  "Return first word of line from WHERE.
If AND-MOVE is non-nil then move to end of word."
  (let ((start (point)))
    (if where
	(goto-char where))
    (prog1
	(buffer-substring (point)
			  (progn (skip-chars-forward "^ \t\n;&|")(point)))
      (unless and-move
	(goto-char start)))))

(defun sh-find-prev-matching (open close &optional depth)
  "Find a matching token for a set of opening and closing keywords.
This takes into account that there may be nested open..close pairings.
OPEN and CLOSE are regexps denoting the tokens to be matched.
Optional parameter DEPTH (usually 1) says how many to look for."
  (let ((parse-sexp-ignore-comments t)
	prev)
    (setq depth (or depth 1))
    (save-excursion
      (condition-case nil
	  (while (and
		  (/= 0  depth)
		  (not (bobp))
		  (setq prev (sh-prev-stmt)))
	    (goto-char prev)
	    (save-excursion
	      (if (looking-at "\\\\\n")
		  (progn
		    (forward-char 2)
		    (skip-chars-forward " \t")))
	      (cond
	       ((looking-at open)
		(setq depth (1- depth))
		(sh-debug "found open at %d - depth = %d" (point) depth))
	       ((looking-at close)
		(setq depth (1+ depth))
		(sh-debug "found close - depth = %d" depth))
	       (t
		))))
	(error nil))
      (if (eq depth 0)
	  prev ;; (point)
	nil)
      )))


(defun sh-var-value (var &optional ignore-error)
  "Return the value of variable VAR, interpreting symbols.
It can also return t or nil.
If an invalid value is found, throw an error unless Optional argument
IGNORE-ERROR is non-nil."
  (let ((val (symbol-value var)))
    (cond
     ((numberp val)
      val)
     ((eq val t)
      val)
     ((null val)
      val)
     ((eq val '+)
      sh-basic-offset)
     ((eq val '-)
      (- sh-basic-offset))
     ((eq val '++)
      (* 2 sh-basic-offset))
     ((eq val '--)
      (* 2 (- sh-basic-offset)))
     ((eq val '*)
      (/ sh-basic-offset 2))
     ((eq val '/)
      (/ (- sh-basic-offset) 2))
     (t
      (if ignore-error
      (progn
	(message "Don't know how to handle %s's value of %s" var val)
	0)
      (error "Don't know how to handle %s's value of %s" var val))
      ))))

(defun sh-set-var-value (var value &optional no-symbol)
  "Set variable VAR to VALUE.
Unless optional argument NO-SYMBOL is non-nil, then if VALUE is
can be represented by a symbol then do so."
  (cond
   (no-symbol
    (set var value))
   ((= value sh-basic-offset)
    (set var '+))
   ((= value (- sh-basic-offset))
    (set var '-))
   ((eq value (* 2 sh-basic-offset))
    (set var  '++))
   ((eq value (* 2 (- sh-basic-offset)))
    (set var  '--))
   ((eq value (/ sh-basic-offset 2))
    (set var  '*))
   ((eq value (/ (- sh-basic-offset) 2))
    (set var  '/))
   (t
    (set var value)))
  )


(defun sh-calculate-indent (&optional info)
  "Return the indentation for the current line.
If INFO is supplied it is used, else it is calculated from current line."
  (let ((ofs 0)
	(base-value 0)
	elt a b val)
    (or info
	(setq info (sh-get-indent-info)))
    (when info
      (while info
	(sh-debug "info: %s  ofs=%s" info ofs)
	(setq elt (car info))
	(cond
	 ((stringp elt)) ;; do nothing?
	 ((listp elt)
	  (setq a (car (car info)))
	  (setq b (nth 1 (car info)))
	  (cond
	   ((eq a t)
	    (save-excursion
	      (goto-char b)
	      (setq val (current-indentation)))
	    (setq base-value val))
	   ((symbolp b)
	    (setq val (sh-var-value b))
	    (cond
	     ((eq a '=)
	      (cond
	       ((null val)
		;; no indentation
		;; set info to nil so  we stop immediately
		(setq base-value nil  ofs nil  info nil))
	       ((eq val t) (setq ofs 0)) ;; indent as normal line
	       (t
		;; The following assume the (t POS) come first!
		(setq ofs val  base-value 0)
		(setq info nil))))	;; ? stop now
	     ((eq a '+) (setq ofs (+ ofs val)))
	     ((eq a '-) (setq ofs (- ofs val)))
	     (t
	      (error "sh-calculate-indent invalid a a=%s b=%s" a b))))
	   (t
	    (error "sh-calculate-indent invalid elt: a=%s b=%s" a b))))
	 (t
	  (error "sh-calculate-indent invalid elt %s" elt)))
	(sh-debug "a=%s b=%s val=%s base-value=%s ofs=%s"
		  a b val base-value ofs)
	(setq info (cdr info)))
      ;; return value:
      (sh-debug "at end:  base-value: %s    ofs: %s" base-value ofs)

      (cond
       ((or (null base-value)(null ofs))
	nil)
       ((and (numberp base-value)(numberp ofs))
	(sh-debug "base (%d) + ofs (%d) = %d"
		  base-value ofs (+ base-value ofs))
	(+ base-value ofs)) ;; return value
       (t
	(error "sh-calculate-indent:  Help.  base-value=%s ofs=%s"
	       base-value ofs)
	nil)))))


(defun sh-indent-line ()
  "Indent the current line."
  (interactive)
  (let ((indent (sh-calculate-indent))
	(pos (- (point-max) (point))))
    (when indent
      (beginning-of-line)
      (skip-chars-forward " \t")
      (indent-line-to indent)
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos))))))


(defun sh-blink (blinkpos &optional msg)
  "Move cursor momentarily to BLINKPOS and display MSG."
  ;; We can get here without it being a number on first line
  (if (numberp blinkpos)
      (save-excursion
	(goto-char blinkpos)
	(if msg (message "%s" msg) (message nil))
	(sit-for blink-matching-delay))
    (if msg (message "%s" msg) (message nil))))

(defun sh-show-indent (arg)
  "Show the how the current line would be indented.
This tells you which variable, if any, controls the indentation of
this line.
If optional arg ARG is non-null (called interactively with a prefix),
a pop up window describes this variable.
If variable `sh-blink' is non-nil then momentarily go to the line
we are indenting relative to, if applicable."
  (interactive "P")
  (sh-must-support-indent)
  (let* ((info (sh-get-indent-info))
	 (var (sh-get-indent-var-for-line info))
	 (curr-indent (current-indentation))
	 val msg)
    (if (stringp var)
	(message "%s" (setq msg var))
      (setq val (sh-calculate-indent info))

      (if (eq curr-indent val)
	  (setq msg (format "%s is %s" var (symbol-value var)))
	(setq msg
	      (if val
		  (format "%s (%s) would change indent from %d to: %d"
			  var (symbol-value var) curr-indent val)
		(format "%s (%s) would leave line as is"
			var (symbol-value var)))
	      ))
      (if (and arg var)
	  (describe-variable var)))
    (if sh-blink
	(let ((info (sh-get-indent-info)))
	  (if (and info (listp (car info))
		   (eq (car (car info)) t))
	      (sh-blink (nth 1 (car info))  msg)
	    (message "%s" msg)))
      (message "%s" msg))
    ))

(defun sh-set-indent ()
  "Set the indentation for the current line.
If the current line is controlled by an indentation variable, prompt
for a new value for it."
  (interactive)
  (sh-must-support-indent)
  (let* ((info (sh-get-indent-info))
	 (var (sh-get-indent-var-for-line info))
	 val old-val indent-val)
    (if (stringp var)
	(message "Cannot set indent - %s" var)
      (setq old-val (symbol-value var))
      (setq val (sh-read-variable var))
      (condition-case nil
	  (progn
	    (set var val)
	    (setq indent-val (sh-calculate-indent info))
	    (if indent-val
		(message "Variable: %s  Value: %s  would indent to: %d"
			 var (symbol-value var) indent-val)
	      (message "Variable: %s  Value: %s  would leave line as is."
		       var (symbol-value var)))
	    ;; I'm not sure about this, indenting it now?
	    ;; No.  Because it would give the impression that an undo would
	    ;; restore thing, but the value has been altered.
	    ;; (sh-indent-line)
	    )
	(error
	 (set var old-val)
	 (message "Bad value for %s, restoring to previous value %s"
		  var old-val)
	 (sit-for 1)
	 nil))
      )))


(defun sh-learn-line-indent (arg)
  "Learn how to indent a line as it currently is indented.

If there is an indentation variable which controls this line's indentation,
then set it to a value which would indent the line the way it
presently is.

If the value can be represented by one of the symbols then do so
unless optional argument ARG (the prefix when interactive) is non-nil."
  (interactive "*P")
  (sh-must-support-indent)
  ;; I'm not sure if we show allow learning on an empty line.
  ;; Though it might occasionally be useful I think it usually
  ;; would just be confusing.
  (if (save-excursion
	(beginning-of-line)
	(looking-at "\\s-*$"))
      (message "sh-learn-line-indent ignores empty lines.")
    (let* ((info (sh-get-indent-info))
	   (var (sh-get-indent-var-for-line info))
	   ival sval diff new-val
	   (no-symbol arg)
	   (curr-indent (current-indentation)))
      (cond
       ((stringp var)
	(message "Cannot learn line - %s" var))
       ((eq var 'sh-indent-comment)
	;; This is arbitrary...
	;; - if curr-indent is 0, set to curr-indent
	;; - else if it has the indentation of a "normal" line,
	;;   then set to t
	;; - else set to curr-indent.
	(setq sh-indent-comment
	      (if (= curr-indent 0)
		  0
		(let* ((sh-indent-comment t)
		       (val2 (sh-calculate-indent info)))
		  (if (= val2 curr-indent)
		      t
		    curr-indent))))
	(message "%s set to %s" var (symbol-value var))
	)
       ((numberp (setq sval (sh-var-value var)))
	(setq ival (sh-calculate-indent info))
	(setq diff (- curr-indent ival))

	(sh-debug "curr-indent: %d   ival: %d  diff: %d  var:%s  sval %s"
		  curr-indent ival diff  var sval)
	(setq new-val (+ sval diff))
;;;	  I commented out this because someone might want to replace
;;;	  a value of `+' with the current value of sh-basic-offset
;;;	  or vice-versa.
;;;	  (if (= 0 diff)
;;;	      (message "No change needed!")
	(sh-set-var-value var new-val no-symbol)
	(message "%s set to %s" var (symbol-value var))
	)
       (t
	(debug)
	(message "Cannot change %s" var))))))



(defun sh-mark-init (buffer)
  "Initialize a BUFFER to be used by `sh-mark-line'."
  (with-current-buffer (get-buffer-create buffer)
    (erase-buffer)
    (occur-mode)))


(defun sh-mark-line (message point buffer &optional add-linenum occur-point)
  "Insert MESSAGE referring to location POINT in current buffer into BUFFER.
Buffer BUFFER is in `occur-mode'.
If ADD-LINENUM is non-nil the message is preceded by the line number.
If OCCUR-POINT is non-nil then the line is marked as a new occurrence
so that `occur-next' and `occur-prev' will work."
  (let ((m1 (make-marker))
	start
	(line ""))
    (when point
      (set-marker m1 point (current-buffer))
      (if add-linenum
	  (setq line (format "%d: " (1+ (count-lines 1 point))))))
    (save-excursion
      (if (get-buffer buffer)
	  (set-buffer (get-buffer buffer))
	(set-buffer (get-buffer-create buffer))
	(occur-mode)
	)
      (goto-char (point-max))
      (setq start (point))
      (insert line)
      (if occur-point
	  (setq occur-point (point)))
      (insert message)
      (if point
	  (add-text-properties
	   start (point)
	   '(mouse-face highlight
	     help-echo "mouse-2: go to the line where I learned this")))
      (insert "\n")
      (if point
	  (progn
	    (put-text-property start (point) 'occur-target m1)
	    (if occur-point
		(put-text-property start occur-point
				   'occur-match t))
	    ))
      )))



;; Is this really worth having?
(defvar sh-learned-buffer-hook nil
  "*An abnormal hook, called with an alist of learned variables.")
;; Example of how to use sh-learned-buffer-hook
;;
;; (defun what-i-learned (list)
;;   (let ((p list))
;;     (with-current-buffer "*scratch*"
;;       (goto-char (point-max))
;;       (insert "(setq\n")
;;       (while p
;; 	(insert (format "  %s %s \n"
;; 			(nth 0 (car p)) (nth 1 (car p))))
;; 	(setq p (cdr p)))
;;       (insert ")\n")
;;       )))
;;
;; (add-hook 'sh-learned-buffer-hook 'what-i-learned)


;; Originally this was sh-learn-region-indent (beg end)
;; However, in practice this was awkward so I changed it to
;; use the whole buffer.  Use narrowing if needbe.
(defun sh-learn-buffer-indent (&optional arg)
  "Learn how to indent the buffer the way it currently is.

Output in buffer \"*indent*\" shows any lines which have conflicting
values of a variable, and the final value of all variables learned.
When called interactively, pop to this buffer automatically if
there are any discrepancies.

If no prefix ARG is given, then variables are set to numbers.
If a prefix arg is given, then variables are set to symbols when
applicable -- e.g. to symbol `+' if the value is that of the
basic indent.
If a positive numerical prefix is given, then  `sh-basic-offset'
is set to the prefix's numerical value.
Otherwise, sh-basic-offset may or may not be changed, according
to the value of variable `sh-learn-basic-offset'.

Abnormal hook `sh-learned-buffer-hook' if non-nil is called when the
function completes.  The function is abnormal because it is called
with an alist of variables learned.  This feature may be changed or
removed in the future.

This command can often take a long time to run."
  (interactive "P")
  (sh-must-support-indent)
  (save-excursion
    (goto-char (point-min))
    (let ((learned-var-list nil)
	  (out-buffer "*indent*")
	  (num-diffs 0)
	  previous-set-info
	  (max 17)
	  vec
	  msg
	  (comment-col nil) ;; number if all same, t if seen diff values
	  (comments-always-default t) ;; nil if we see one not default
	  initial-msg
	  (specified-basic-offset (and arg (numberp arg)
				       (> arg 0)))
	  (linenum 0)
	  suggested)
      (setq vec (make-vector max 0))
      (sh-mark-init out-buffer)

      (if specified-basic-offset
	  (progn
	    (setq sh-basic-offset arg)
	    (setq initial-msg
		  (format "Using specified sh-basic-offset of %d"
			  sh-basic-offset)))
	(setq initial-msg
	      (format "Initial value of sh-basic-offset: %s"
		      sh-basic-offset)))

      (while (< (point) (point-max))
	(setq linenum (1+ linenum))
	;; (if (zerop (% linenum 10))
	(message "line %d" linenum)
	;; )
	(unless (looking-at "\\s-*$") ;; ignore empty lines!
	  (let* ((sh-indent-comment t) ;; info must return default indent
		 (info (sh-get-indent-info))
		 (var (sh-get-indent-var-for-line info))
		 sval ival diff new-val
		 (curr-indent (current-indentation)))
	    (cond
	     ((null var)
	      nil)
	     ((stringp var)
	      nil)
	     ((numberp (setq sval (sh-var-value var 'no-error)))
	      ;; the numberp excludes comments since sval will be t.
	      (setq ival (sh-calculate-indent))
	      (setq diff (- curr-indent ival))
	      (setq new-val (+ sval diff))
	      (sh-set-var-value var new-val 'no-symbol)
	      (unless (looking-at "\\s-*#") ;; don't learn from comments
		(if (setq previous-set-info (assoc var learned-var-list))
		    (progn
		      ;; it was already there, is it same value ?
		      (unless (eq (symbol-value var)
				  (nth 1 previous-set-info))
			(sh-mark-line
			 (format "Variable %s was set to %s"
				 var (symbol-value var))
			 (point) out-buffer t t)
			(sh-mark-line
			 (format "  but was previously set to %s"
				 (nth 1 previous-set-info))
			 (nth 2 previous-set-info) out-buffer t)
			(setq num-diffs (1+ num-diffs))
			;; (delete previous-set-info  learned-var-list)
			(setcdr previous-set-info
				(list (symbol-value var) (point)))
			)
		      )
		  (setq learned-var-list
			(append (list (list var (symbol-value var)
					    (point)))
				learned-var-list)))
		(if (numberp new-val)
		    (progn
		      (sh-debug
		       "This line's indent value: %d"  new-val)
		      (if (< new-val 0)
			  (setq new-val (- new-val)))
		      (if (< new-val max)
			  (aset vec new-val (1+ (aref vec new-val))))))
		))
	     ((eq var 'sh-indent-comment)
	      (unless (= curr-indent (sh-calculate-indent info))
		;; this is not the default indentation
		(setq comments-always-default nil)
		(if comment-col	;; then we have see one before
		    (or (eq comment-col curr-indent)
			(setq comment-col t)) ;; seen a different one
		  (setq comment-col curr-indent))
		))
	     (t
	      (sh-debug "Cannot learn this line!!!")
	      ))
	    (sh-debug
	     "at %s learned-var-list is %s" (point) learned-var-list)
	    ))
	(forward-line 1)
	) ;; while
      (if sh-debug
	  (progn
	    (setq msg (format
		       "comment-col = %s  comments-always-default = %s"
		       comment-col comments-always-default))
	    ;; (message msg)
	    (sh-mark-line  msg nil out-buffer)))
      (cond
       ((eq comment-col 0)
	(setq msg  "\nComments are all in 1st column.\n"))
       (comments-always-default
	(setq msg  "\nComments follow default indentation.\n")
	(setq comment-col t))
       ((numberp comment-col)
	(setq msg  (format "\nComments are in col %d." comment-col)))
       (t
	(setq msg  "\nComments seem to be mixed, leaving them as is.\n")
	(setq comment-col nil)
	))
      (sh-debug msg)
      (sh-mark-line  msg nil out-buffer)

      (sh-mark-line initial-msg nil out-buffer t t)

      (setq suggested (sh-guess-basic-offset vec))

      (if (and suggested (not specified-basic-offset))
	  (let ((new-value
		 (cond
		  ;; t => set it if we have a single value as a number
		  ((and (eq sh-learn-basic-offset t) (numberp suggested))
		   suggested)
		  ;; other non-nil => set it if only one value was found
		  (sh-learn-basic-offset
		   (if (numberp suggested)
		       suggested
		     (if (= (length suggested) 1)
			 (car suggested))))
		  (t
		   nil))))
	    (if new-value
		(progn
		  (setq learned-var-list
			(append (list (list 'sh-basic-offset
					    (setq sh-basic-offset new-value)
					    (point-max)))
				learned-var-list))
		  ;; Not sure if we need to put this line in, since
		  ;; it will appear in the "Learned variable settings".
		  (sh-mark-line
		   (format "Changed sh-basic-offset to: %d" sh-basic-offset)
		   nil out-buffer))
	      (sh-mark-line
	       (if (listp suggested)
		   (format "Possible value(s) for sh-basic-offset:  %s"
			   (mapconcat 'int-to-string suggested " "))
		 (format "Suggested sh-basic-offset:  %d" suggested))
	       nil out-buffer))))


      (setq learned-var-list
	    (append (list (list 'sh-indent-comment comment-col (point-max)))
		    learned-var-list))
      (setq sh-indent-comment comment-col)
      (let ((name (buffer-name)))
	(sh-mark-line  "\nLearned variable settings:" nil out-buffer)
	(if arg
	    ;; Set learned variables to symbolic rather than numeric
	    ;; values where possible.
	    (dolist (learned-var (reverse learned-var-list))
	      (let ((var (car learned-var))
		    (val (nth 1 learned-var)))
		(when (and (not (eq var 'sh-basic-offset))
			   (numberp val))
		  (sh-set-var-value var val)))))
	(dolist (learned-var (reverse learned-var-list))
	  (let ((var (car learned-var)))
	    (sh-mark-line (format "  %s %s" var (symbol-value var))
			  (nth 2 learned-var) out-buffer)))
	(with-current-buffer out-buffer
	  (goto-char (point-min))
	  (insert
	   (format "Indentation values for buffer %s.\n" name)
	   (format "%d indentation variable%s different values%s\n\n"
		   num-diffs
		   (if (= num-diffs 1)
		       " has"   "s have")
		   (if (zerop num-diffs)
		       "." ":"))
	   )))
      ;; Are abnormal hooks considered bad form?
      (run-hook-with-args 'sh-learned-buffer-hook learned-var-list)
      (and (called-interactively-p 'any)
	   (or sh-popup-occur-buffer (> num-diffs 0))
	   (pop-to-buffer out-buffer)))))

(defun sh-guess-basic-offset (vec)
  "See if we can determine a reasonable value for `sh-basic-offset'.
This is experimental, heuristic and arbitrary!
Argument VEC is a vector of information collected by
`sh-learn-buffer-indent'.
Return values:
  number          - there appears to be a good single value
  list of numbers - no obvious one, here is a list of one or more
		    reasonable choices
  nil		  - we couldn't find a reasonable one."
  (let* ((max (1- (length vec)))
	 (i 1)
	 (totals (make-vector max 0)))
    (while (< i max)
      (aset totals i (+ (aref totals i) (* 4 (aref vec i))))
      (if (zerop (% i 2))
	  (aset totals i (+ (aref totals i) (aref vec (/ i 2)))))
      (if (< (* i 2) max)
	  (aset totals i (+ (aref totals i) (aref vec (* i 2)))))
      (setq i (1+ i)))

    (let ((x nil)
	  (result nil)
	  tot sum p)
      (setq i 1)
      (while (< i max)
	(if (/= (aref totals i) 0)
	    (setq x (append x (list (cons i (aref totals i))))))
	(setq i (1+ i)))

      (setq x (sort x (lambda (a b) (> (cdr a) (cdr b)))))
      (setq tot (apply '+ (append totals nil)))
      (sh-debug (format "vec: %s\ntotals: %s\ntot: %d"
			vec totals tot))
      (cond
       ((zerop (length x))
	(message "no values!"))	;; we return nil
       ((= (length x) 1)
	(message "only value is %d" (car (car x)))
	(setq result (car (car x)))) ;; return single value
       ((> (cdr (car x)) (/ tot 2))
	;; 1st is > 50%
	(message "basic-offset is probably %d" (car (car x)))
	(setq result (car (car x)))) ;;   again, return a single value
       ((>=  (cdr (car x)) (* 2 (cdr (car (cdr x)))))
	;; 1st is >= 2 * 2nd
	(message "basic-offset could be %d" (car (car x)))
	(setq result (car (car x))))
       ((>= (+ (cdr (car x))(cdr (car (cdr x)))) (/ tot 2))
	;; 1st & 2nd together >= 50%  - return a list
	(setq p x  sum 0 result nil)
	(while  (and p
		     (<= (setq sum (+ sum (cdr (car p)))) (/ tot 2)))
	  (setq result (append result (list (car (car p)))))
	  (setq p (cdr p)))
	(message "Possible choices for sh-basic-offset: %s"
		 (mapconcat 'int-to-string result " ")))
       (t
	(message "No obvious value for sh-basic-offset.  Perhaps %d"
		 (car (car x)))
	;; result is nil here
	))
      result)))

;; ========================================================================

;; Styles -- a quick and dirty way of saving the indentation settings.

(defvar sh-styles-alist nil
  "A list of all known shell indentation styles.")

(defun sh-name-style (name &optional confirm-overwrite)
  "Name the current indentation settings as a style called NAME.
If this name exists, the command will prompt whether it should be
overwritten if
- - it was called interactively with a prefix argument, or
- - called non-interactively with optional CONFIRM-OVERWRITE non-nil."
  ;; (interactive "sName for this style: ")
  (interactive
   (list
    (read-from-minibuffer "Name for this style? " )
    (not current-prefix-arg)))
  (let ((slist (cons name
		     (mapcar (lambda (var) (cons var (symbol-value var)))
			     sh-var-list)))
	(style (assoc name sh-styles-alist)))
    (if style
	(if (and confirm-overwrite
		 (not (y-or-n-p "This style exists.  Overwrite it? ")))
	    (message "Not changing style %s" name)
	  (message "Updating style %s" name)
	  (setcdr style (cdr slist)))
      (message "Creating new style %s" name)
      (push slist sh-styles-alist))))

(defun sh-load-style (name)
  "Set shell indentation values for this buffer from those in style NAME."
  (interactive (list (completing-read
		      "Which style to use for this buffer? "
		      sh-styles-alist nil t)))
  (let ((sl (assoc name  sh-styles-alist)))
    (if (null sl)
	(error "sh-load-style - style %s not known" name)
      (dolist (var (cdr sl))
	(set (car var) (cdr var))))))

(defun sh-save-styles-to-buffer (buff)
  "Save all current styles in elisp to buffer BUFF.
This is always added to the end of the buffer."
  (interactive (list
		(read-from-minibuffer "Buffer to save styles in? " "*scratch*")))
  (with-current-buffer (get-buffer-create buff)
    (goto-char (point-max))
    (insert "\n")
    (pp `(setq sh-styles-alist ',sh-styles-alist) (current-buffer))))



;; statement syntax-commands for various shells

;; You are welcome to add the syntax or even completely new statements as
;; appropriate for your favorite shell.

(defconst sh-non-closing-paren
  ;; If we leave it rear-sticky, calling `newline' ends up inserting a \n
  ;; that inherits this property, which then confuses the indentation.
  (propertize ")" 'syntax-table sh-st-punc 'rear-nonsticky t))

(define-skeleton sh-case
  "Insert a case/switch statement.  See `sh-feature'."
  (csh "expression: "
       "switch( " str " )" \n
       > "case " (read-string "pattern: ") ?: \n
       > _ \n
       "breaksw" \n
       ( "other pattern, %s: "
	 < "case " str ?: \n
	 > _ \n
	 "breaksw" \n)
       < "default:" \n
       > _ \n
       resume:
       < < "endsw" \n)
  (es)
  (rc "expression: "
      > "switch( " str " ) {" \n
      > "case " (read-string "pattern: ") \n
      > _ \n
      ( "other pattern, %s: "
	"case " str > \n
	> _ \n)
      "case *" > \n
      > _ \n
      resume:
      ?\} > \n)
  (sh "expression: "
      > "case " str " in" \n
      ( "pattern, %s: "
	> str sh-non-closing-paren \n
	> _ \n
	";;" \n)
      > "*" sh-non-closing-paren \n
      > _ \n
      resume:
      "esac" > \n))

(define-skeleton sh-for
  "Insert a for loop.  See `sh-feature'."
  (csh sh-modify sh
       1 ""
       2 "foreach "
       4 " ( "
       6 " )"
       15 '<
       16 "end")
  (es sh-modify rc
      4 " = ")
  (rc sh-modify sh
      2 "for( "
      6 " ) {"
      15 ?\} )
  (sh "Index variable: "
      > "for " str " in " _ "; do" \n
      > _ | ?$ & (sh-remember-variable str) \n
      "done" > \n))



(define-skeleton sh-indexed-loop
  "Insert an indexed loop from 1 to n.  See `sh-feature'."
  (bash sh-modify posix)
  (csh "Index variable: "
       "@ " str " = 1" \n
       "while( $" str " <= " (read-string "upper limit: ") " )" \n
       > _ ?$ str \n
       "@ " str "++" \n
       < "end" \n)
  (es sh-modify rc
      4 " =")
  (ksh88 "Index variable: "
	 > "integer " str "=0" \n
	 > "while (( ( " str " += 1 ) <= "
	 (read-string "upper limit: ")
	 " )); do" \n
	 > _ ?$ (sh-remember-variable str) > \n
	 "done" > \n)
  (posix "Index variable: "
	 > str "=1" \n
	 "while [ $" str " -le "
	 (read-string "upper limit: ")
	 " ]; do" \n
	 > _ ?$ str \n
	 str ?= (sh-add (sh-remember-variable str) 1) \n
	 "done" > \n)
  (rc "Index variable: "
      > "for( " str " in" " `{awk 'BEGIN { for( i=1; i<="
      (read-string "upper limit: ")
      "; i++ ) print i }'`}) {" \n
      > _ ?$ (sh-remember-variable str) \n
      ?\} > \n)
  (sh "Index variable: "
      > "for " str " in `awk 'BEGIN { for( i=1; i<="
      (read-string "upper limit: ")
      "; i++ ) print i }'`; do" \n
      > _ ?$ (sh-remember-variable str) \n
      "done" > \n))


(defun sh-shell-initialize-variables ()
  "Scan the buffer for variable assignments.
Add these variables to `sh-shell-variables'."
  (message "Scanning buffer `%s' for variable assignments..." (buffer-name))
  (save-excursion
    (goto-char (point-min))
    (setq sh-shell-variables-initialized t)
    (while (search-forward "=" nil t)
      (sh-assignment 0)))
  (message "Scanning buffer `%s' for variable assignments...done"
	   (buffer-name)))

(defvar sh-add-buffer)

(defun sh-add-completer (string predicate code)
  "Do completion using `sh-shell-variables', but initialize it first.
This function is designed for use as the \"completion table\",
so it takes three arguments:
  STRING, the current buffer contents;
  PREDICATE, the predicate for filtering possible matches;
  CODE, which says what kind of things to do.
CODE can be nil, t or `lambda'.
nil means to return the best completion of STRING, or nil if there is none.
t means to return a list of all possible completions of STRING.
`lambda' means to return t if STRING is a valid completion as it stands."
  (let ((vars
	 (with-current-buffer sh-add-buffer
	   (or sh-shell-variables-initialized
	       (sh-shell-initialize-variables))
	   (nconc (mapcar (lambda (var)
                            (substring var 0 (string-match "=" var)))
			  process-environment)
		  sh-shell-variables))))
    (complete-with-action code vars string predicate)))

(defun sh-add (var delta)
  "Insert an addition of VAR and prefix DELTA for Bourne (type) shell."
  (interactive
   (let ((sh-add-buffer (current-buffer)))
     (list (completing-read "Variable: " 'sh-add-completer)
	   (prefix-numeric-value current-prefix-arg))))
  (insert (sh-feature '((bash . "$(( ")
			(ksh88 . "$(( ")
			(posix . "$(( ")
			(rc . "`{expr $")
			(sh . "`expr $")
			(zsh . "$[ ")))
	  (sh-remember-variable var)
	  (if (< delta 0) " - " " + ")
	  (number-to-string (abs delta))
	  (sh-feature '((bash . " ))")
			(ksh88 . " ))")
			(posix . " ))")
			(rc . "}")
			(sh . "`")
			(zsh . " ]")))))



(define-skeleton sh-function
  "Insert a function definition.  See `sh-feature'."
  (bash sh-modify ksh88
	3 "() {")
  (ksh88 "name: "
	 "function " str " {" \n
	 > _ \n
	 < "}" \n)
  (rc sh-modify ksh88
      1 "fn ")
  (sh ()
      "() {" \n
      > _ \n
      < "}" \n))



(define-skeleton sh-if
  "Insert an if statement.  See `sh-feature'."
  (csh "condition: "
       "if( " str " ) then" \n
       > _ \n
       ( "other condition, %s: "
	 < "else if( " str " ) then" \n
	 > _ \n)
       < "else" \n
       > _ \n
       resume:
       < "endif" \n)
  (es "condition: "
      > "if { " str " } {" \n
      > _ \n
      ( "other condition, %s: "
	"} { " str " } {" > \n
	> _ \n)
      "} {" > \n
      > _ \n
      resume:
      ?\} > \n)
  (rc "condition: "
      > "if( " str " ) {" \n
      > _ \n
      ( "other condition, %s: "
	"} else if( " str " ) {"  > \n
	> _ \n)
      "} else {" > \n
      > _ \n
      resume:
      ?\} > \n)
  (sh "condition: "
      '(setq input (sh-feature sh-test))
      > "if " str "; then" \n
      > _ \n
      ( "other condition, %s: "
	>  "elif " str "; then" > \n
	> \n)
      "else" > \n
      > \n
      resume:
      "fi" > \n))



(define-skeleton sh-repeat
  "Insert a repeat loop definition.  See `sh-feature'."
  (es nil
      > "forever {" \n
      > _ \n
      ?\} > \n)
  (zsh "factor: "
       > "repeat " str "; do" > \n
       >  \n
       "done" > \n))

;;;(put 'sh-repeat 'menu-enable '(sh-feature sh-repeat))



(define-skeleton sh-select
  "Insert a select statement.  See `sh-feature'."
  (ksh88 "Index variable: "
	 > "select " str " in " _ "; do" \n
	 > ?$ str \n
	 "done" > \n)
  (bash sh-append ksh88))
;;;(put 'sh-select 'menu-enable '(sh-feature sh-select))



(define-skeleton sh-tmp-file
  "Insert code to setup temporary file handling.  See `sh-feature'."
  (bash sh-append ksh88)
  (csh (file-name-nondirectory (buffer-file-name))
       "set tmp = `mktemp -t " str ".XXXXXX`" \n
       "onintr exit" \n _
       (and (goto-char (point-max))
	    (not (bolp))
	    ?\n)
       "exit:\n"
       "rm $tmp* >&/dev/null" > \n)
  (es (file-name-nondirectory (buffer-file-name))
      > "local( signals = $signals sighup sigint;" \n
      > "tmp = `{ mktemp -t " str ".XXXXXX } ) {" \n
      > "catch @ e {" \n
      > "rm $tmp^* >[2]/dev/null" \n
      "throw $e" \n
      "} {" > \n
      _ \n
      ?\} > \n
      ?\} > \n)
  (ksh88 sh-modify sh
	 7 "EXIT")
  (rc (file-name-nondirectory (buffer-file-name))
      > "tmp = `{ mktemp -t " str ".XXXXXX }" \n
      "fn sigexit { rm $tmp^* >[2]/dev/null }" \n)
  (sh (file-name-nondirectory (buffer-file-name))
      > "TMP=`mktemp -t " str ".XXXXXX`" \n
      "trap \"rm $TMP* 2>/dev/null\" " ?0 \n))



(define-skeleton sh-until
  "Insert an until loop.  See `sh-feature'."
  (sh "condition: "
      '(setq input (sh-feature sh-test))
      > "until " str "; do" \n
      > _ \n
      "done" > \n))
;;;(put 'sh-until 'menu-enable '(sh-feature sh-until))



(define-skeleton sh-while
  "Insert a while loop.  See `sh-feature'."
  (csh sh-modify sh
       2 ""
       3 "while( "
       5 " )"
       10 '<
       11 "end")
  (es sh-modify sh
      3 "while { "
      5 " } {"
      10 ?\} )
  (rc sh-modify sh
      3 "while( "
      5 " ) {"
      10 ?\} )
  (sh "condition: "
      '(setq input (sh-feature sh-test))
      > "while " str "; do" \n
      > _ \n
      "done" > \n))



(define-skeleton sh-while-getopts
  "Insert a while getopts loop.  See `sh-feature'.
Prompts for an options string which consists of letters for each recognized
option followed by a colon `:' if the option accepts an argument."
  (bash sh-modify sh
	18 "${0##*/}")
  (csh nil
       "while( 1 )" \n
       > "switch( \"$1\" )" \n
       '(setq input '("- x" . 2))
       > >
       ( "option, %s: "
	 < "case " '(eval str)
	 '(if (string-match " +" str)
	      (setq v1 (substring str (match-end 0))
		    str (substring str 0 (match-beginning 0)))
	    (setq v1 nil))
	 str ?: \n
	 > "set " v1 & " = $2" | -4 & _ \n
	 (if v1 "shift") & \n
	 "breaksw" \n)
       < "case --:" \n
       > "shift" \n
       < "default:" \n
       > "break" \n
       resume:
       < < "endsw" \n
       "shift" \n
       < "end" \n)
  (ksh88 sh-modify sh
	 16 "print"
	 18 "${0##*/}"
	 37 "OPTIND-1")
  (posix sh-modify sh
	 18 "$(basename $0)")
  (sh "optstring: "
      > "while getopts :" str " OPT; do" \n
      > "case $OPT in" \n
      '(setq v1 (append (vconcat str) nil))
      ( (prog1 (if v1 (char-to-string (car v1)))
	  (if (eq (nth 1 v1) ?:)
	      (setq v1 (nthcdr 2 v1)
		    v2 "\"$OPTARG\"")
	    (setq v1 (cdr v1)
		  v2 nil)))
	> str "|+" str sh-non-closing-paren \n
	> _ v2 \n
	> ";;" \n)
      > "*" sh-non-closing-paren \n
      > "echo" " \"usage: " "`basename $0`"
      " [+-" '(setq v1 (point)) str
      '(save-excursion
	 (while (search-backward ":" v1 t)
	   (replace-match " ARG] [+-" t t)))
      (if (eq (preceding-char) ?-) -5)
      (if (and (sequencep v1) (length v1)) "] " "} ")
      "[--] ARGS...\"" \n
      "exit 2"  > \n
      "esac" >
      \n "done"
      > \n
      "shift " (sh-add "OPTIND" -1) \n
      "OPTIND=1" \n))



(defun sh-assignment (arg)
  "Remember preceding identifier for future completion and do self-insert."
  (interactive "p")
  (self-insert-command arg)
  (if (<= arg 1)
      (sh-remember-variable
       (save-excursion
	 (if (re-search-forward (sh-feature sh-assignment-regexp)
				(prog1 (point)
				  (beginning-of-line 1))
				t)
	     (match-string 1))))))


(defun sh-maybe-here-document (arg)
  "Insert self.  Without prefix, following unquoted `<' inserts here document.
The document is bounded by `sh-here-document-word'."
  (interactive "*P")
  (self-insert-command (prefix-numeric-value arg))
  (or arg
      (not (looking-back "[^<]<<"))
      (save-excursion
	(backward-char 2)
	(sh-quoted-p))
      (nth 8 (syntax-ppss))
      (let ((tabs (if (string-match "\\`-" sh-here-document-word)
                      (make-string (/ (current-indentation) tab-width) ?\t)
                    ""))
            (delim (replace-regexp-in-string "['\"]" ""
                                            sh-here-document-word)))
	(insert sh-here-document-word)
	(or (eolp) (looking-at "[ \t]") (insert ?\s))
	(end-of-line 1)
	(while
	    (sh-quoted-p)
	  (end-of-line 2))
	(insert ?\n tabs)
	(save-excursion
          (insert ?\n tabs (replace-regexp-in-string
                            "\\`-?[ \t]*" "" delim))))))


;; various other commands

(autoload 'comint-dynamic-complete "comint"
  "Dynamically perform completion at point." t)

(autoload 'shell-dynamic-complete-command "shell"
  "Dynamically complete the command at point." t)

(autoload 'comint-dynamic-complete-filename "comint"
  "Dynamically complete the filename at point." t)

(autoload 'shell-dynamic-complete-environment-variable "shell"
  "Dynamically complete the environment variable at point." t)



(defun sh-beginning-of-command ()
  "Move point to successive beginnings of commands."
  (interactive)
  (if (re-search-backward sh-beginning-of-command nil t)
      (goto-char (match-beginning 2))))

(defun sh-end-of-command ()
  "Move point to successive ends of commands."
  (interactive)
  (if (re-search-forward sh-end-of-command nil t)
      (goto-char (match-end 1))))

;; Backslashification.  Stolen from make-mode.el.

(defun sh-backslash-region (from to delete-flag)
  "Insert, align, or delete end-of-line backslashes on the lines in the region.
With no argument, inserts backslashes and aligns existing backslashes.
With an argument, deletes the backslashes.

This function does not modify the last line of the region if the region ends
right at the start of the following line; it does not modify blank lines
at the start of the region.  So you can put the region around an entire
shell command and conveniently use this command."
  (interactive "r\nP")
  (save-excursion
    (goto-char from)
    (let ((column sh-backslash-column)
          (endmark (make-marker)))
      (move-marker endmark to)
      ;; Compute the smallest column number past the ends of all the lines.
      (if sh-backslash-align
	  (progn
	    (if (not delete-flag)
		(while (< (point) to)
		  (end-of-line)
		  (if (= (preceding-char) ?\\)
		      (progn (forward-char -1)
			     (skip-chars-backward " \t")))
		  (setq column (max column (1+ (current-column))))
		  (forward-line 1)))
	    ;; Adjust upward to a tab column, if that doesn't push
	    ;; past the margin.
	    (if (> (% column tab-width) 0)
		(let ((adjusted (* (/ (+ column tab-width -1) tab-width)
				   tab-width)))
		  (if (< adjusted (window-width))
		      (setq column adjusted))))))
      ;; Don't modify blank lines at start of region.
      (goto-char from)
      (while (and (< (point) endmark) (eolp))
        (forward-line 1))
      ;; Add or remove backslashes on all the lines.
      (while (and (< (point) endmark)
                  ;; Don't backslashify the last line
                  ;; if the region ends right at the start of the next line.
                  (save-excursion
                    (forward-line 1)
                    (< (point) endmark)))
        (if (not delete-flag)
            (sh-append-backslash column)
          (sh-delete-backslash))
        (forward-line 1))
      (move-marker endmark nil))))

(defun sh-append-backslash (column)
  (end-of-line)
  ;; Note that "\\\\" is needed to get one backslash.
  (if (= (preceding-char) ?\\)
      (progn (forward-char -1)
             (delete-horizontal-space)
             (indent-to column (if sh-backslash-align nil 1)))
    (indent-to column (if sh-backslash-align nil 1))
    (insert "\\")))

(defun sh-delete-backslash ()
  (end-of-line)
  (or (bolp)
      (progn
 	(forward-char -1)
 	(if (looking-at "\\\\")
 	    (delete-region (1+ (point))
 			   (progn (skip-chars-backward " \t") (point)))))))

(provide 'sh-script)

;;; sh-script.el ends here
