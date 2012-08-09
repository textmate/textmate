;;; octave-mod.el --- editing Octave source files under Emacs

;; Copyright (C) 1997, 2001-2012 Free Software Foundation, Inc.

;; Author: Kurt Hornik <Kurt.Hornik@wu-wien.ac.at>
;;	   John Eaton <jwe@octave.org>
;; Maintainer: FSF
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

;; This package provides Emacs support for Octave.
;; It defines Octave mode, a major mode for editing
;; Octave code.

;; The file octave-inf.el contains code for interacting with an inferior
;; Octave process using comint.

;; See the documentation of `octave-mode' and
;; `run-octave' for further information on usage and customization.

;;; Code:
(require 'custom)

(defgroup octave nil
  "Major mode for editing Octave source files."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'languages)

(defvar inferior-octave-output-list nil)
(defvar inferior-octave-output-string nil)
(defvar inferior-octave-receive-in-progress nil)

(declare-function inferior-octave-send-list-and-digest "octave-inf" (list))

(defconst octave-maintainer-address
  "Kurt Hornik <Kurt.Hornik@wu-wien.ac.at>, bug-gnu-emacs@gnu.org"
  "Current maintainer of the Emacs Octave package.")

(define-abbrev-table 'octave-abbrev-table
  (mapcar (lambda (e) (append e '(nil 0 t)))
          '(("`a" "all_va_args")
            ("`b" "break")
            ("`cs" "case")
            ("`ca" "catch")
            ("`c" "continue")
            ("`el" "else")
            ("`eli" "elseif")
            ("`et" "end_try_catch")
            ("`eu" "end_unwind_protect")
            ("`ef" "endfor")
            ("`efu" "endfunction")
            ("`ei" "endif")
            ("`es" "endswitch")
            ("`ew" "endwhile")
            ("`f" "for")
            ("`fu" "function")
            ("`gl" "global")
            ("`gp" "gplot")
            ("`gs" "gsplot")
            ("`if" "if ()")
            ("`o" "otherwise")
            ("`rp" "replot")
            ("`r" "return")
            ("`s" "switch")
            ("`t" "try")
            ("`u" "until ()")
            ("`up" "unwind_protect")
            ("`upc" "unwind_protect_cleanup")
            ("`w" "while ()")))
  "Abbrev table for Octave's reserved words.
Used in `octave-mode' and inferior-octave-mode buffers.
All Octave abbrevs start with a grave accent (`)."
  :regexp "\\(?:[^`]\\|^\\)\\(\\(?:\\<\\|`\\)\\w+\\)\\W*")

(defvar octave-comment-char ?#
  "Character to start an Octave comment.")
(defvar octave-comment-start
  (string octave-comment-char ?\s)
  "String to insert to start a new Octave in-line comment.")
(defvar octave-comment-start-skip "\\s<+\\s-*"
  "Regexp to match the start of an Octave comment up to its body.")

(defvar octave-begin-keywords
  '("do" "for" "function" "if" "switch" "try" "unwind_protect" "while"))
(defvar octave-else-keywords
  '("case" "catch" "else" "elseif" "otherwise" "unwind_protect_cleanup"))
(defvar octave-end-keywords
  '("endfor" "endfunction" "endif" "endswitch" "end_try_catch"
    "end_unwind_protect" "endwhile" "until" "end"))

(defvar octave-reserved-words
  (append octave-begin-keywords
	  octave-else-keywords
	  octave-end-keywords
	  '("break" "continue" "end" "global" "persistent" "return"))
  "Reserved words in Octave.")

(defvar octave-text-functions
  '("casesen" "cd" "chdir" "clear" "diary" "dir" "document" "echo"
    "edit_history" "format" "help" "history" "hold"
    "load" "ls" "more" "run_history" "save" "type"
    "which" "who" "whos")
  "Text functions in Octave.")

(defvar octave-variables
  '("DEFAULT_EXEC_PATH" "DEFAULT_LOADPATH"
    "EDITOR" "EXEC_PATH" "F_DUPFD" "F_GETFD" "F_GETFL" "F_SETFD"
    "F_SETFL" "I" "IMAGE_PATH" "Inf" "J"
    "NaN" "OCTAVE_VERSION" "O_APPEND" "O_CREAT" "O_EXCL"
    "O_NONBLOCK" "O_RDONLY" "O_RDWR" "O_TRUNC" "O_WRONLY" "PAGER" "PS1"
    "PS2" "PS4" "PWD" "SEEK_CUR" "SEEK_END" "SEEK_SET" "__F_DUPFD__"
    "__F_GETFD__" "__F_GETFL__" "__F_SETFD__" "__F_SETFL__" "__I__"
    "__Inf__" "__J__" "__NaN__" "__OCTAVE_VERSION__" "__O_APPEND__"
    "__O_CREAT__" "__O_EXCL__" "__O_NONBLOCK__" "__O_RDONLY__"
    "__O_RDWR__" "__O_TRUNC__" "__O_WRONLY__" "__PWD__" "__SEEK_CUR__"
    "__SEEK_END__" "__SEEK_SET__" "__argv__" "__e__" "__eps__"
    "__i__" "__inf__" "__j__" "__nan__" "__pi__"
    "__program_invocation_name__" "__program_name__" "__realmax__"
    "__realmin__" "__stderr__" "__stdin__" "__stdout__" "ans" "argv"
    "beep_on_error" "completion_append_char"
    "crash_dumps_octave_core" "default_save_format"
    "e" "echo_executing_commands" "eps"
    "error_text" "gnuplot_binary" "history_file"
    "history_size" "ignore_function_time_stamp"
    "inf" "nan" "nargin" "output_max_field_width" "output_precision"
    "page_output_immediately" "page_screen_output" "pi"
    "print_answer_id_name" "print_empty_dimensions"
    "program_invocation_name" "program_name"
    "realmax" "realmin" "return_last_computed_value" "save_precision"
    "saving_history" "sighup_dumps_octave_core" "sigterm_dumps_octave_core"
    "silent_functions" "split_long_rows" "stderr" "stdin" "stdout"
    "string_fill_char" "struct_levels_to_print"
    "suppress_verbose_help_message")
  "Builtin variables in Octave.")

(defvar octave-function-header-regexp
  (concat "^\\s-*\\_<\\(function\\)\\_>"
	  "\\([^=;\n]*=[ \t]*\\|[ \t]*\\)\\(\\(?:\\w\\|\\s_\\)+\\)\\_>")
  "Regexp to match an Octave function header.
The string `function' and its name are given by the first and third
parenthetical grouping.")

(defvar octave-font-lock-keywords
  (list
   ;; Fontify all builtin keywords.
   (cons (concat "\\_<\\("
		 (regexp-opt (append octave-reserved-words
                                     octave-text-functions))
		 "\\)\\_>")
	 'font-lock-keyword-face)
   ;; Fontify all builtin operators.
   (cons "\\(&\\||\\|<=\\|>=\\|==\\|<\\|>\\|!=\\|!\\)"
	 (if (boundp 'font-lock-builtin-face)
	     'font-lock-builtin-face
	   'font-lock-preprocessor-face))
   ;; Fontify all builtin variables.
   (cons (concat "\\_<" (regexp-opt octave-variables) "\\_>")
	 'font-lock-variable-name-face)
   ;; Fontify all function declarations.
   (list octave-function-header-regexp
	 '(1 font-lock-keyword-face)
	 '(3 font-lock-function-name-face nil t)))
  "Additional Octave expressions to highlight.")

(defun octave-syntax-propertize-function (start end)
  (goto-char start)
  (octave-syntax-propertize-sqs end)
  (funcall (syntax-propertize-rules
            ;; Try to distinguish the string-quotes from the transpose-quotes.
            ("[[({,; ]\\('\\)"
             (1 (prog1 "\"'" (octave-syntax-propertize-sqs end)))))
           (point) end))

(defun octave-syntax-propertize-sqs (end)
  "Propertize the content/end of single-quote strings."
  (when (eq (nth 3 (syntax-ppss)) ?\')
    ;; A '..' string.
    (when (re-search-forward
           "\\(?:\\=\\|[^']\\)\\(?:''\\)*\\('\\)\\($\\|[^']\\)" end 'move)
      (goto-char (match-beginning 2))
      (when (eq (char-before (match-beginning 1)) ?\\)
        ;; Backslash cannot escape a single quote.
        (put-text-property (1- (match-beginning 1)) (match-beginning 1)
                           'syntax-table (string-to-syntax ".")))
      (put-text-property (match-beginning 1) (match-end 1)
                         'syntax-table (string-to-syntax "\"'")))))

(defcustom inferior-octave-buffer "*Inferior Octave*"
  "Name of buffer for running an inferior Octave process."
  :type 'string
  :group 'octave-inferior)

(defvar inferior-octave-process nil)

(defvar octave-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "`" 'octave-abbrev-start)
    (define-key map "\e\n" 'octave-indent-new-comment-line)
    (define-key map "\M-\C-q" 'octave-indent-defun)
    (define-key map "\C-c\C-b" 'octave-submit-bug-report)
    (define-key map "\C-c\C-p" 'octave-previous-code-line)
    (define-key map "\C-c\C-n" 'octave-next-code-line)
    (define-key map "\C-c\C-a" 'octave-beginning-of-line)
    (define-key map "\C-c\C-e" 'octave-end-of-line)
    (define-key map [remap down-list] 'smie-down-list)
    (define-key map "\C-c\M-\C-h" 'octave-mark-block)
    (define-key map "\C-c]" 'smie-close-block)
    (define-key map "\C-c/" 'smie-close-block)
    (define-key map "\C-c\C-f" 'octave-insert-defun)
    ;; FIXME: free C-h so it can do the describe-prefix-bindings.
    (define-key map "\C-c\C-h" 'info-lookup-symbol)
    (define-key map "\C-c\C-il" 'octave-send-line)
    (define-key map "\C-c\C-ib" 'octave-send-block)
    (define-key map "\C-c\C-if" 'octave-send-defun)
    (define-key map "\C-c\C-ir" 'octave-send-region)
    (define-key map "\C-c\C-is" 'octave-show-process-buffer)
    (define-key map "\C-c\C-ih" 'octave-hide-process-buffer)
    (define-key map "\C-c\C-ik" 'octave-kill-process)
    (define-key map "\C-c\C-i\C-l" 'octave-send-line)
    (define-key map "\C-c\C-i\C-b" 'octave-send-block)
    (define-key map "\C-c\C-i\C-f" 'octave-send-defun)
    (define-key map "\C-c\C-i\C-r" 'octave-send-region)
    (define-key map "\C-c\C-i\C-s" 'octave-show-process-buffer)
    ;; FIXME: free C-h so it can do the describe-prefix-bindings.
    (define-key map "\C-c\C-i\C-h" 'octave-hide-process-buffer)
    (define-key map "\C-c\C-i\C-k" 'octave-kill-process)
    map)
  "Keymap used in Octave mode.")



(easy-menu-define octave-mode-menu octave-mode-map
  "Menu for Octave mode."
  '("Octave"
    ("Lines"
      ["Previous Code Line"	octave-previous-code-line t]
      ["Next Code Line"		octave-next-code-line t]
      ["Begin of Continuation"	octave-beginning-of-line t]
      ["End of Continuation"	octave-end-of-line t]
      ["Split Line at Point"	octave-indent-new-comment-line t])
    ("Blocks"
      ["Mark Block"		octave-mark-block t]
      ["Close Block"		smie-close-block t])
    ("Functions"
      ["Indent Function"	octave-indent-defun t]
      ["Insert Function"	octave-insert-defun t])
    "-"
    ("Debug"
      ["Send Current Line"	octave-send-line t]
      ["Send Current Block"	octave-send-block t]
      ["Send Current Function"	octave-send-defun t]
      ["Send Region"		octave-send-region t]
      ["Show Process Buffer"	octave-show-process-buffer t]
      ["Hide Process Buffer"	octave-hide-process-buffer t]
      ["Kill Process"		octave-kill-process t])
    "-"
    ["Indent Line"		indent-according-to-mode t]
    ["Complete Symbol"		completion-at-point t]
    "-"
    ["Toggle Abbrev Mode"	abbrev-mode
     :style toggle :selected abbrev-mode]
    ["Toggle Auto-Fill Mode"	auto-fill-mode
     :style toggle :selected auto-fill-function]
    "-"
    ["Submit Bug Report"	octave-submit-bug-report t]
    "-"
    ["Describe Octave Mode"	describe-mode t]
    ["Lookup Octave Index"	info-lookup-symbol t]))

(defvar octave-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\r " "  table)
    (modify-syntax-entry ?+ "."   table)
    (modify-syntax-entry ?- "."   table)
    (modify-syntax-entry ?= "."   table)
    (modify-syntax-entry ?* "."   table)
    (modify-syntax-entry ?/ "."   table)
    (modify-syntax-entry ?> "."   table)
    (modify-syntax-entry ?< "."   table)
    (modify-syntax-entry ?& "."   table)
    (modify-syntax-entry ?| "."   table)
    (modify-syntax-entry ?! "."   table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?\' "."  table)
    ;; Was "w" for abbrevs, but now that it's not necessary any more,
    (modify-syntax-entry ?\` "."  table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?. "_"   table)
    (modify-syntax-entry ?_ "_"   table)
    ;; The "b" flag only applies to the second letter of the comstart
    ;; and the first letter of the comend, i.e. the "4b" below is ineffective.
    ;; If we try to put `b' on the single-line comments, we get a similar
    ;; problem where the % and # chars appear as first chars of the 2-char
    ;; comend, so the multi-line ender is also turned into style-b.
    ;; So we need the new "c" comment style.
    (modify-syntax-entry ?\% "< 13"  table)
    (modify-syntax-entry ?\# "< 13"  table)
    (modify-syntax-entry ?\{ "(} 2c"  table)
    (modify-syntax-entry ?\} "){ 4c"  table)
    (modify-syntax-entry ?\n ">"  table)
    table)
  "Syntax table in use in `octave-mode' buffers.")

(defcustom octave-blink-matching-block t
  "Control the blinking of matching Octave block keywords.
Non-nil means show matching begin of block when inserting a space,
newline or semicolon after an else or end keyword."
  :type 'boolean
  :group 'octave)

(defcustom octave-block-offset 2
  "Extra indentation applied to statements in Octave block structures."
  :type 'integer
  :group 'octave)

(defvar octave-block-comment-start
  (concat (make-string 2 octave-comment-char) " ")
  "String to insert to start a new Octave comment on an empty line.")

(defcustom octave-continuation-offset 4
  "Extra indentation applied to Octave continuation lines."
  :type 'integer
  :group 'octave)
(eval-and-compile
  (defconst octave-continuation-marker-regexp "\\\\\\|\\.\\.\\."))
(defvar octave-continuation-regexp
  (concat "[^#%\n]*\\(" octave-continuation-marker-regexp
          "\\)\\s-*\\(\\s<.*\\)?$"))
(defcustom octave-continuation-string "\\"
  "Character string used for Octave continuation lines.  Normally \\."
  :type 'string
  :group 'octave)

(defvar octave-completion-alist nil
  "Alist of Octave symbols for completion in Octave mode.
Each element looks like (VAR . VAR), where the car and cdr are the same
symbol (an Octave command or variable name).
Currently, only builtin variables can be completed.")

(defvar octave-mode-imenu-generic-expression
  (list
   ;; Functions
   (list nil octave-function-header-regexp 3))
  "Imenu expression for Octave mode.  See `imenu-generic-expression'.")

(defcustom octave-mode-hook nil
  "Hook to be run when Octave mode is started."
  :type 'hook
  :group 'octave)

(defcustom octave-send-show-buffer t
  "Non-nil means display `inferior-octave-buffer' after sending to it."
  :type 'boolean
  :group 'octave)
(defcustom octave-send-line-auto-forward t
  "Control auto-forward after sending to the inferior Octave process.
Non-nil means always go to the next Octave code line after sending."
  :type 'boolean
  :group 'octave)
(defcustom octave-send-echo-input t
  "Non-nil means echo input sent to the inferior Octave process."
  :type 'boolean
  :group 'octave)


;;; SMIE indentation

(require 'smie)

(defconst octave-operator-table
  '((assoc ";" "\n") (assoc ",") ; The doc claims they have equal precedence!?
    (right "=" "+=" "-=" "*=" "/=")
    (assoc "&&") (assoc "||") ; The doc claims they have equal precedence!?
    (assoc "&") (assoc "|")   ; The doc claims they have equal precedence!?
    (nonassoc "<" "<=" "==" ">=" ">" "!=" "~=")
    (nonassoc ":")                      ;No idea what this is.
    (assoc "+" "-")
    (assoc "*" "/" "\\" ".\\" ".*" "./")
    (nonassoc "'" ".'")
    (nonassoc "++" "--" "!" "~")        ;And unary "+" and "-".
    (right "^" "**" ".^" ".**")
    ;; It's not really an operator, but for indentation purposes it
    ;; could be convenient to treat it as one.
    (assoc "...")))

(defconst octave-smie-bnf-table
  '((atom)
    ;; We can't distinguish the first element in a sequence with
    ;; precedence grammars, so we can't distinguish the condition
    ;; if the `if' from the subsequent body, for example.
    ;; This has to be done later in the indentation rules.
    (exp (exp "\n" exp)
         ;; We need to mention at least one of the operators in this part
         ;; of the grammar: if the BNF and the operator table have
         ;; no overlap, SMIE can't know how they relate.
         (exp ";" exp)
         ("try" exp "catch" exp "end_try_catch")
         ("try" exp "catch" exp "end")
         ("unwind_protect" exp
          "unwind_protect_cleanup" exp "end_unwind_protect")
         ("unwind_protect" exp "unwind_protect_cleanup" exp "end")
         ("for" exp "endfor")
         ("for" exp "end")
         ("do" exp "until" atom)
         ("while" exp "endwhile")
         ("while" exp "end")
         ("if" exp "endif")
         ("if" exp "else" exp "endif")
         ("if" exp "elseif" exp "else" exp "endif")
         ("if" exp "elseif" exp "elseif" exp "else" exp "endif")
         ("if" exp "elseif" exp "elseif" exp "else" exp "end")
         ("switch" exp "case" exp "endswitch")
         ("switch" exp "case" exp "otherwise" exp "endswitch")
         ("switch" exp "case" exp "case" exp "otherwise" exp "endswitch")
         ("switch" exp "case" exp "case" exp "otherwise" exp "end")
         ("function" exp "endfunction")
         ("function" exp "end"))
    ;; (fundesc (atom "=" atom))
    ))

(defconst octave-smie-grammar
  (smie-prec2->grammar
   (smie-merge-prec2s
    (smie-bnf->prec2 octave-smie-bnf-table
                     '((assoc "\n" ";")))

    (smie-precs->prec2 octave-operator-table))))

;; Tokenizing needs to be refined so that ";;" is treated as two
;; tokens and also so as to recognize the \n separator (and
;; corresponding continuation lines).

(defconst octave-operator-regexp
  (regexp-opt (apply 'append (mapcar 'cdr octave-operator-table))))

(defun octave-smie-backward-token ()
  (let ((pos (point)))
    (forward-comment (- (point)))
    (cond
     ((and (not (eq (char-before) ?\;)) ;Coalesce ";" and "\n".
           (> pos (line-end-position))
           (if (looking-back octave-continuation-marker-regexp (- (point) 3))
               (progn
                 (goto-char (match-beginning 0))
                 (forward-comment (- (point)))
                 nil)
             t)
           ;; Ignore it if it's within parentheses.
           (let ((ppss (syntax-ppss)))
             (not (and (nth 1 ppss)
                       (eq ?\( (char-after (nth 1 ppss)))))))
      (skip-chars-forward " \t")
      ;; Why bother distinguishing \n and ;?
      ";") ;;"\n"
     ((and (looking-back octave-operator-regexp (- (point) 3) 'greedy)
           ;; Don't mistake a string quote for a transpose.
           (not (looking-back "\\s\"" (1- (point)))))
      (goto-char (match-beginning 0))
      (match-string-no-properties 0))
     (t
      (smie-default-backward-token)))))

(defun octave-smie-forward-token ()
  (skip-chars-forward " \t")
  (when (looking-at (eval-when-compile
                      (concat "\\(" octave-continuation-marker-regexp
                              "\\)[ \t]*\\($\\|[%#]\\)")))
    (goto-char (match-end 1))
    (forward-comment 1))
  (cond
   ((and (looking-at "$\\|[%#]")
         ;; Ignore it if it's within parentheses.
         (prog1 (let ((ppss (syntax-ppss)))
                  (not (and (nth 1 ppss)
                            (eq ?\( (char-after (nth 1 ppss))))))
           (forward-comment (point-max))))
    ;; Why bother distinguishing \n and ;?
    ";") ;;"\n"
   ((looking-at ";[ \t]*\\($\\|[%#]\\)")
    ;; Combine the ; with the subsequent \n.
    (goto-char (match-beginning 1))
    (forward-comment 1)
    ";")
   ((and (looking-at octave-operator-regexp)
         ;; Don't mistake a string quote for a transpose.
         (not (looking-at "\\s\"")))
    (goto-char (match-end 0))
    (match-string-no-properties 0))
   (t
    (smie-default-forward-token))))

(defun octave-smie-rules (kind token)
  (pcase (cons kind token)
    ;; We could set smie-indent-basic instead, but that would have two
    ;; disadvantages:
    ;; - changes to octave-block-offset wouldn't take effect immediately.
    ;; - edebug wouldn't show the use of this variable.
    (`(:elem . basic) octave-block-offset)
    ;; Since "case" is in the same BNF rules as switch..end, SMIE by default
    ;; aligns it with "switch".
    (`(:before . "case") (if (not (smie-rule-sibling-p)) octave-block-offset))
    (`(:after . ";")
     (if (smie-rule-parent-p "function" "if" "while" "else" "elseif" "for"
                             "otherwise" "case" "try" "catch" "unwind_protect"
                             "unwind_protect_cleanup")
         (smie-rule-parent octave-block-offset)
       ;; For (invalid) code between switch and case.
       ;; (if (smie-parent-p "switch") 4)
       0))))

(defvar electric-layout-rules)

;;;###autoload
(define-derived-mode octave-mode prog-mode "Octave"
  "Major mode for editing Octave code.

This mode makes it easier to write Octave code by helping with
indentation, doing some of the typing for you (with Abbrev mode) and by
showing keywords, comments, strings, etc. in different faces (with
Font Lock mode on terminals that support it).

Octave itself is a high-level language, primarily intended for numerical
computations.  It provides a convenient command line interface for
solving linear and nonlinear problems numerically.  Function definitions
can also be stored in files, and it can be used in a batch mode (which
is why you need this mode!).

The latest released version of Octave is always available via anonymous
ftp from ftp.octave.org in the directory `/pub/octave'.  Complete
source and binaries for several popular systems are available.

Type \\[list-abbrevs] to display the built-in abbrevs for Octave keywords.

Keybindings
===========

\\{octave-mode-map}

Variables you can use to customize Octave mode
==============================================

`octave-blink-matching-block'
  Non-nil means show matching begin of block when inserting a space,
  newline or semicolon after an else or end keyword.  Default is t.

`octave-block-offset'
  Extra indentation applied to statements in block structures.
  Default is 2.

`octave-continuation-offset'
  Extra indentation applied to Octave continuation lines.
  Default is 4.

`octave-continuation-string'
  String used for Octave continuation lines.
  Default is a backslash.

`octave-send-echo-input'
  Non-nil means always display `inferior-octave-buffer' after sending a
  command to the inferior Octave process.

`octave-send-line-auto-forward'
  Non-nil means always go to the next unsent line of Octave code after
  sending a line to the inferior Octave process.

`octave-send-echo-input'
  Non-nil means echo input sent to the inferior Octave process.

Turning on Octave mode runs the hook `octave-mode-hook'.

To begin using this mode for all `.m' files that you edit, add the
following lines to your `.emacs' file:

  (add-to-list 'auto-mode-alist '(\"\\\\.m\\\\'\" . octave-mode))

To automatically turn on the abbrev and auto-fill features,
add the following lines to your `.emacs' file as well:

  (add-hook 'octave-mode-hook
	    (lambda ()
	      (abbrev-mode 1)
	      (auto-fill-mode 1)))

To submit a problem report, enter \\[octave-submit-bug-report] from \
an Octave mode buffer.
This automatically sets up a mail buffer with version information
already added.  You just need to add a description of the problem,
including a reproducible test case and send the message."
  (setq local-abbrev-table octave-abbrev-table)

  (smie-setup octave-smie-grammar #'octave-smie-rules
              :forward-token  #'octave-smie-forward-token
              :backward-token #'octave-smie-backward-token)
  (set (make-local-variable 'smie-indent-basic) 'octave-block-offset)

    (set (make-local-variable 'smie-blink-matching-triggers)
       (cons ?\; smie-blink-matching-triggers))
  (unless octave-blink-matching-block
    (remove-hook 'post-self-insert-hook #'smie-blink-matching-open 'local))

  (set (make-local-variable 'electric-indent-chars)
       (cons ?\; electric-indent-chars))
  ;; IIUC matlab-mode takes the opposite approach: it makes RET insert
  ;; a ";" at those places where it's correct (i.e. outside of parens).
  (set (make-local-variable 'electric-layout-rules) '((?\; . after)))

  (set (make-local-variable 'comment-start) octave-comment-start)
  (set (make-local-variable 'comment-end) "")
  ;; Don't set it here: it's not really a property of the language,
  ;; just a personal preference of the author.
  ;; (set (make-local-variable 'comment-column) 32)
  (set (make-local-variable 'comment-start-skip) "\\s<+\\s-*")
  (set (make-local-variable 'comment-add) 1)

  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'paragraph-start)
       (concat "\\s-*$\\|" page-delimiter))
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'paragraph-ignore-fill-prefix) t)
  (set (make-local-variable 'fill-paragraph-function) 'octave-fill-paragraph)
  ;; FIXME: Why disable it?
  ;; (set (make-local-variable 'adaptive-fill-regexp) nil)
  ;; Again, this is not a property of the language, don't set it here.
  ;; (set (make-local-variable 'fill-column) 72)
  (set (make-local-variable 'normal-auto-fill-function) 'octave-auto-fill)

  (set (make-local-variable 'font-lock-defaults)
       '(octave-font-lock-keywords))

  (set (make-local-variable 'syntax-propertize-function)
       #'octave-syntax-propertize-function)

  (set (make-local-variable 'imenu-generic-expression)
       octave-mode-imenu-generic-expression)
  (set (make-local-variable 'imenu-case-fold-search) nil)

  (add-hook 'completion-at-point-functions
            'octave-completion-at-point-function nil t)
  (set (make-local-variable 'beginning-of-defun-function)
       'octave-beginning-of-defun)

  (easy-menu-add octave-mode-menu)
  (octave-initialize-completions))

;;; Miscellaneous useful functions

(defsubst octave-in-comment-p ()
  "Return t if point is inside an Octave comment."
  (nth 4 (syntax-ppss)))

(defsubst octave-in-string-p ()
  "Return t if point is inside an Octave string."
  (nth 3 (syntax-ppss)))

(defsubst octave-not-in-string-or-comment-p ()
  "Return t if point is not inside an Octave string or comment."
  (let ((pps (syntax-ppss)))
    (not (or (nth 3 pps) (nth 4 pps)))))


(defun octave-looking-at-kw (regexp)
  "Like `looking-at', but sets `case-fold-search' nil."
  (let ((case-fold-search nil))
    (looking-at regexp)))

(defun octave-maybe-insert-continuation-string ()
  (if (or (octave-in-comment-p)
	  (save-excursion
	    (beginning-of-line)
	    (looking-at octave-continuation-regexp)))
      nil
    (delete-horizontal-space)
    (insert (concat " " octave-continuation-string))))

;;; Indentation

(defun octave-indent-new-comment-line ()
  "Break Octave line at point, continuing comment if within one.
If within code, insert `octave-continuation-string' before breaking the
line.  If within a string, signal an error.
The new line is properly indented."
  (interactive)
  (delete-horizontal-space)
  (cond
   ((octave-in-comment-p)
    (indent-new-comment-line))
   ((octave-in-string-p)
    (error "Cannot split a code line inside a string"))
   (t
    (insert (concat " " octave-continuation-string))
    (reindent-then-newline-and-indent))))

(defun octave-indent-defun ()
  "Properly indent the Octave function which contains point."
  (interactive)
  (save-excursion
    (mark-defun)
    (message "Indenting function...")
    (indent-region (point) (mark) nil))
  (message "Indenting function...done."))


;;; Motion
(defun octave-next-code-line (&optional arg)
  "Move ARG lines of Octave code forward (backward if ARG is negative).
Skips past all empty and comment lines.  Default for ARG is 1.

On success, return 0.  Otherwise, go as far as possible and return -1."
  (interactive "p")
  (or arg (setq arg 1))
  (beginning-of-line)
  (let ((n 0)
	(inc (if (> arg 0) 1 -1)))
    (while (and (/= arg 0) (= n 0))
      (setq n (forward-line inc))
      (while (and (= n 0)
		  (looking-at "\\s-*\\($\\|\\s<\\)"))
	(setq n (forward-line inc)))
      (setq arg (- arg inc)))
    n))

(defun octave-previous-code-line (&optional arg)
  "Move ARG lines of Octave code backward (forward if ARG is negative).
Skips past all empty and comment lines.  Default for ARG is 1.

On success, return 0.  Otherwise, go as far as possible and return -1."
  (interactive "p")
  (or arg (setq arg 1))
  (octave-next-code-line (- arg)))

(defun octave-beginning-of-line ()
  "Move point to beginning of current Octave line.
If on an empty or comment line, go to the beginning of that line.
Otherwise, move backward to the beginning of the first Octave code line
which is not inside a continuation statement, i.e., which does not
follow a code line ending in `...' or `\\', or is inside an open
parenthesis list."
  (interactive)
  (beginning-of-line)
  (if (not (looking-at "\\s-*\\($\\|\\s<\\)"))
      (while (or (condition-case nil
		     (progn
		       (up-list -1)
		       (beginning-of-line)
		       t)
		   (error nil))
		 (and (or (looking-at "\\s-*\\($\\|\\s<\\)")
			  (save-excursion
			    (if (zerop (octave-previous-code-line))
				(looking-at octave-continuation-regexp))))
		      (zerop (forward-line -1)))))))

(defun octave-end-of-line ()
  "Move point to end of current Octave line.
If on an empty or comment line, go to the end of that line.
Otherwise, move forward to the end of the first Octave code line which
does not end in `...' or `\\' or is inside an open parenthesis list."
  (interactive)
  (end-of-line)
  (if (save-excursion
	(beginning-of-line)
	(looking-at "\\s-*\\($\\|\\s<\\)"))
      ()
    (while (or (condition-case nil
		   (progn
		     (up-list 1)
		     (end-of-line)
		     t)
		 (error nil))
	       (and (save-excursion
		      (beginning-of-line)
		      (or (looking-at "\\s-*\\($\\|\\s<\\)")
			  (looking-at octave-continuation-regexp)))
		    (zerop (forward-line 1)))))
    (end-of-line)))

(defun octave-mark-block ()
  "Put point at the beginning of this Octave block, mark at the end.
The block marked is the one that contains point or follows point."
  (interactive)
  (unless (or (looking-at "\\s(")
              (save-excursion
                (let* ((token (funcall smie-forward-token-function))
                       (level (assoc token smie-grammar)))
                  (and level (null (cadr level))))))
    (backward-up-list 1))
  (mark-sexp))

(defun octave-beginning-of-defun (&optional arg)
  "Move backward to the beginning of an Octave function.
With positive ARG, do it that many times.  Negative argument -N means
move forward to Nth following beginning of a function.
Returns t unless search stops at the beginning or end of the buffer."
  (let* ((arg (or arg 1))
	 (inc (if (> arg 0) 1 -1))
	 (found nil)
         (case-fold-search nil))
    (and (not (eobp))
	 (not (and (> arg 0) (looking-at "\\_<function\\_>")))
	 (skip-syntax-forward "w"))
    (while (and (/= arg 0)
		(setq found
		      (re-search-backward "\\_<function\\_>" inc)))
      (if (octave-not-in-string-or-comment-p)
	  (setq arg (- arg inc))))
    (if found
	(progn
	  (and (< inc 0) (goto-char (match-beginning 0)))
	  t))))


;;; Filling
(defun octave-auto-fill ()
  "Perform auto-fill in Octave mode.
Returns nil if no feasible place to break the line could be found, and t
otherwise."
  (let (fc give-up)
    (if (or (null (setq fc (current-fill-column)))
	    (save-excursion
	      (beginning-of-line)
	      (and auto-fill-inhibit-regexp
		   (octave-looking-at-kw auto-fill-inhibit-regexp))))
	nil				; Can't do anything
      (if (and (not (octave-in-comment-p))
	       (> (current-column) fc))
	  (setq fc (- fc (+ (length octave-continuation-string) 1))))
      (while (and (not give-up) (> (current-column) fc))
	(let* ((opoint (point))
	       (fpoint
		(save-excursion
		  (move-to-column (+ fc 1))
		  (skip-chars-backward "^ \t\n")
		  ;; If we're at the beginning of the line, break after
		  ;; the first word
		  (if (bolp)
		      (re-search-forward "[ \t]" opoint t))
		  ;; If we're in a comment line, don't break after the
		  ;; comment chars
		  (if (save-excursion
			(skip-syntax-backward " <")
			(bolp))
		      (re-search-forward "[ \t]" (line-end-position)
					 'move))
		  ;; If we're not in a comment line and just ahead the
		  ;; continuation string, don't break here.
		  (if (and (not (octave-in-comment-p))
			   (looking-at
			    (concat "\\s-*"
				    (regexp-quote
				     octave-continuation-string)
				    "\\s-*$")))
		      (end-of-line))
		  (skip-chars-backward " \t")
		  (point))))
	  (if (save-excursion
		(goto-char fpoint)
		(not (or (bolp) (eolp))))
	      (let ((prev-column (current-column)))
		(if (save-excursion
		      (skip-chars-backward " \t")
		      (= (point) fpoint))
		    (progn
		      (octave-maybe-insert-continuation-string)
		      (indent-new-comment-line t))
		  (save-excursion
		    (goto-char fpoint)
		    (octave-maybe-insert-continuation-string)
		    (indent-new-comment-line t)))
		(if (>= (current-column) prev-column)
		    (setq give-up t)))
	    (setq give-up t))))
      (not give-up))))

(defun octave-fill-paragraph (&optional _arg)
  "Fill paragraph of Octave code, handling Octave comments."
  ;; FIXME: difference with generic fill-paragraph:
  ;; - code lines are only split, never joined.
  ;; - \n that end comments are never removed.
  ;; - insert continuation marker when splitting code lines.
  (interactive "P")
  (save-excursion
    (let ((end (progn (forward-paragraph) (copy-marker (point) t)))
          (beg (progn
                 (forward-paragraph -1)
                 (skip-chars-forward " \t\n")
                 (beginning-of-line)
                 (point)))
          (cfc (current-fill-column))
          comment-prefix)
      (goto-char beg)
      (while (< (point) end)
        (condition-case nil
            (indent-according-to-mode)
          (error nil))
        (move-to-column cfc)
        ;; First check whether we need to combine non-empty comment lines
        (if (and (< (current-column) cfc)
                 (octave-in-comment-p)
                 (not (save-excursion
                        (beginning-of-line)
                        (looking-at "^\\s-*\\s<+\\s-*$"))))
            ;; This is a nonempty comment line which does not extend
            ;; past the fill column.  If it is followed by a nonempty
            ;; comment line with the same comment prefix, try to
            ;; combine them, and repeat this until either we reach the
            ;; fill-column or there is nothing more to combine.
            (progn
              ;; Get the comment prefix
              (save-excursion
                (beginning-of-line)
                (while (and (re-search-forward "\\s<+")
                            (not (octave-in-comment-p))))
                (setq comment-prefix (match-string 0)))
              ;; And keep combining ...
              (while (and (< (current-column) cfc)
                          (save-excursion
                            (forward-line 1)
                            (and (looking-at
                                  (concat "^\\s-*"
                                          comment-prefix
                                          "\\S<"))
                                 (not (looking-at
                                       (concat "^\\s-*"
                                               comment-prefix
                                               "\\s-*$"))))))
                (delete-char 1)
                (re-search-forward comment-prefix)
                (delete-region (match-beginning 0) (match-end 0))
                (fixup-whitespace)
                (move-to-column cfc))))
        ;; We might also try to combine continued code lines>  Perhaps
        ;; some other time ...
        (skip-chars-forward "^ \t\n")
        (delete-horizontal-space)
        (if (or (< (current-column) cfc)
                (and (= (current-column) cfc) (eolp)))
            (forward-line 1)
          (if (not (eolp)) (insert " "))
          (or (octave-auto-fill)
              (forward-line 1))))
      t)))


;;; Completions
(defun octave-initialize-completions ()
  "Create an alist for Octave completions."
  (if octave-completion-alist
      ()
    (setq octave-completion-alist
          (append octave-reserved-words
                  octave-text-functions
                  octave-variables))))

(defun octave-completion-at-point-function ()
  "Find the text to complete and the corresponding table."
  (let* ((beg (save-excursion (skip-syntax-backward "w_") (point)))
         (end (point)))
    (if (< beg (point))
        ;; Extend region past point, if applicable.
        (save-excursion (skip-syntax-forward "w_")
                        (setq end (point))))
    (list beg end octave-completion-alist)))

(define-obsolete-function-alias 'octave-complete-symbol
  'completion-at-point "24.1")

;;; Electric characters && friends

(defun octave-abbrev-start ()
  "Start entering an Octave abbreviation.
If Abbrev mode is turned on, typing ` (grave accent) followed by ? or
\\[help-command] lists all Octave abbrevs.  Any other key combination is
executed normally.
Note that all Octave mode abbrevs start with a grave accent."
  (interactive)
  (if (not abbrev-mode)
      (self-insert-command 1)
    (let (c)
      (insert last-command-event)
      (if (if (featurep 'xemacs)
	      (or (eq (event-to-character (setq c (next-event))) ??)
		  (eq (event-to-character c) help-char))
	    (or (eq (setq c (read-event)) ??)
		(eq c help-char)))
	  (let ((abbrev-table-name-list '(octave-abbrev-table)))
	    (list-abbrevs))
	(setq unread-command-events (list c))))))

(define-skeleton octave-insert-defun
  "Insert an Octave function skeleton.
Prompt for the function's name, arguments and return values (to be
entered without parens)."
  (let* ((defname (substring (buffer-name) 0 -2))
         (name (read-string (format "Function name (default %s): " defname)
                            nil nil defname))
         (args (read-string "Arguments: "))
         (vals (read-string "Return values: ")))
    (format "%s%s (%s)"
            (cond
             ((string-equal vals "") vals)
             ((string-match "[ ,]" vals) (concat "[" vals "] = "))
             (t (concat vals " = ")))
            name
            args))
  \n "function " > str \n \n
  octave-block-comment-start "usage: " str \n
  octave-block-comment-start \n octave-block-comment-start
  \n _ \n
  "endfunction" > \n)

;;; Communication with the inferior Octave process
(defun octave-kill-process ()
  "Kill inferior Octave process and its buffer."
  (interactive)
  (if inferior-octave-process
      (progn
	(process-send-string inferior-octave-process "quit;\n")
	(accept-process-output inferior-octave-process)))
  (if inferior-octave-buffer
      (kill-buffer inferior-octave-buffer)))

(defun octave-show-process-buffer ()
  "Make sure that `inferior-octave-buffer' is displayed."
  (interactive)
  (if (get-buffer inferior-octave-buffer)
      (display-buffer inferior-octave-buffer)
    (message "No buffer named %s" inferior-octave-buffer)))

(defun octave-hide-process-buffer ()
  "Delete all windows that display `inferior-octave-buffer'."
  (interactive)
  (if (get-buffer inferior-octave-buffer)
      (delete-windows-on inferior-octave-buffer)
    (message "No buffer named %s" inferior-octave-buffer)))

(defun octave-send-region (beg end)
  "Send current region to the inferior Octave process."
  (interactive "r")
  (inferior-octave t)
  (let ((proc inferior-octave-process)
	(string (buffer-substring-no-properties beg end))
	line)
    (with-current-buffer inferior-octave-buffer
      (setq inferior-octave-output-list nil)
      (while (not (string-equal string ""))
	(if (string-match "\n" string)
	    (setq line (substring string 0 (match-beginning 0))
		  string (substring string (match-end 0)))
	  (setq line string string ""))
	(setq inferior-octave-receive-in-progress t)
	(inferior-octave-send-list-and-digest (list (concat line "\n")))
	(while inferior-octave-receive-in-progress
	  (accept-process-output proc))
	(insert-before-markers
	 (mapconcat 'identity
		    (append
		     (if octave-send-echo-input (list line) (list ""))
		     (mapcar 'inferior-octave-strip-ctrl-g
			     inferior-octave-output-list)
		     (list inferior-octave-output-string))
		    "\n")))))
  (if octave-send-show-buffer
      (display-buffer inferior-octave-buffer)))

(defun octave-send-block ()
  "Send current Octave block to the inferior Octave process."
  (interactive)
  (save-excursion
    (octave-mark-block)
    (octave-send-region (point) (mark))))

(defun octave-send-defun ()
  "Send current Octave function to the inferior Octave process."
  (interactive)
  (save-excursion
    (mark-defun)
    (octave-send-region (point) (mark))))

(defun octave-send-line (&optional arg)
  "Send current Octave code line to the inferior Octave process.
With positive prefix ARG, send that many lines.
If `octave-send-line-auto-forward' is non-nil, go to the next unsent
code line."
  (interactive "P")
  (or arg (setq arg 1))
  (if (> arg 0)
      (let (beg end)
	(beginning-of-line)
	(setq beg (point))
	(octave-next-code-line (- arg 1))
	(end-of-line)
	(setq end (point))
	(if octave-send-line-auto-forward
	    (octave-next-code-line 1))
	(octave-send-region beg end))))

(defun octave-eval-print-last-sexp ()
  "Evaluate Octave sexp before point and print value into current buffer."
  (interactive)
  (inferior-octave t)
  (let ((standard-output (current-buffer))
	(print-escape-newlines nil)
	(opoint (point)))
    (terpri)
    (prin1
     (save-excursion
       (forward-sexp -1)
       (inferior-octave-send-list-and-digest
	(list (concat (buffer-substring-no-properties (point) opoint)
		      "\n")))
       (mapconcat 'identity inferior-octave-output-list "\n")))
    (terpri)))

;;; Bug reporting
(defun octave-submit-bug-report ()
  "Submit a bug report on the Emacs Octave package via mail."
  (interactive)
  (require 'reporter)
  (and
   (y-or-n-p "Do you want to submit a bug report? ")
   (reporter-submit-bug-report
    octave-maintainer-address
    (concat "Emacs version " emacs-version)
    (list
     'octave-blink-matching-block
     'octave-block-offset
     'octave-comment-char
     'octave-continuation-offset
     'octave-continuation-string
     'octave-send-echo-input
     'octave-send-line-auto-forward
     'octave-send-show-buffer))))

;; provide ourself

(provide 'octave-mod)

;;; octave-mod.el ends here
