;;; modula2.el --- Modula-2 editing support package

;; Author: Michael Schmidt <michael@pbinfo.UUCP>
;;	Tom Perrine <Perrin@LOGICON.ARPA>
;; Maintainer: FSF
;; Keywords: languages

;; This file is part of GNU Emacs.

;; The authors distributed this without a copyright notice
;; back in 1988, so it is in the public domain.  The original included
;; the following credit:

;; Author Mick Jordan
;; amended Peter Robinson

;;; Commentary:

;; A major mode for editing Modula-2 code.  It provides convenient abbrevs
;; for Modula-2 keywords, knows about the standard layout rules, and supports
;; a native compile command.

;;; Code:

(require 'smie)

(defgroup modula2 nil
  "Major mode for editing Modula-2 code."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :prefix "m2-"
  :group 'languages)

;;; Added by Tom Perrine (TEP)
(defvar m2-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?/ ". 12" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\( "()1" table)
    (modify-syntax-entry ?\) ")(4" table)
    (modify-syntax-entry ?* ". 23nb" table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?\' "\"" table)
    table)
  "Syntax table in use in Modula-2 buffers.")

(defcustom m2-compile-command "m2c"
  "Command to compile Modula-2 programs."
  :type 'string
  :group 'modula2)

(defcustom m2-link-command "m2l"
  "Command to link Modula-2 programs."
  :type 'string
  :group 'modula2)

(defcustom m2-link-name nil
  "Name of the Modula-2 executable."
  :type '(choice (const nil) string)
  :group 'modula2)

(defcustom m2-end-comment-column 75
  "*Column for aligning the end of a comment, in Modula-2."
  :type 'integer
  :group 'modula2)

;;; Added by TEP
(defvar m2-mode-map
  (let ((map (make-sparse-keymap)))
    ;; FIXME: Many of those bindings are contrary to coding conventions.
    (define-key map "\C-cb" 'm2-begin)
    (define-key map "\C-cc" 'm2-case)
    (define-key map "\C-cd" 'm2-definition)
    (define-key map "\C-ce" 'm2-else)
    (define-key map "\C-cf" 'm2-for)
    (define-key map "\C-ch" 'm2-header)
    (define-key map "\C-ci" 'm2-if)
    (define-key map "\C-cm" 'm2-module)
    (define-key map "\C-cl" 'm2-loop)
    (define-key map "\C-co" 'm2-or)
    (define-key map "\C-cp" 'm2-procedure)
    (define-key map "\C-c\C-w" 'm2-with)
    (define-key map "\C-cr" 'm2-record)
    (define-key map "\C-cs" 'm2-stdio)
    (define-key map "\C-ct" 'm2-type)
    (define-key map "\C-cu" 'm2-until)
    (define-key map "\C-cv" 'm2-var)
    (define-key map "\C-cw" 'm2-while)
    (define-key map "\C-cx" 'm2-export)
    (define-key map "\C-cy" 'm2-import)
    (define-key map "\C-c{" 'm2-begin-comment)
    (define-key map "\C-c}" 'm2-end-comment)
    (define-key map "\C-c\C-z" 'suspend-emacs)
    (define-key map "\C-c\C-v" 'm2-visit)
    (define-key map "\C-c\C-t" 'm2-toggle)
    (define-key map "\C-c\C-l" 'm2-link)
    (define-key map "\C-c\C-c" 'm2-compile)
    map)
  "Keymap used in Modula-2 mode.")

(defcustom m2-indent 5
  "*This variable gives the indentation in Modula-2-Mode."
  :type 'integer
  :group 'modula2)
(put 'm2-indent 'safe-local-variable
     (lambda (v) (or (null v) (integerp v))))

(defconst m2-smie-grammar
  ;; An official definition can be found as "M2R10.pdf".  This grammar does
  ;; not really follow it, for lots of technical reasons, but it can still be
  ;; useful to refer to it.
  (smie-prec2->grammar
   (smie-merge-prec2s
    (smie-bnf->prec2
     '((range) (id) (epsilon)
       (fields (fields ";" fields) (ids ":" type))
       (proctype (id ":" type))
       (type ("RECORD" fields "END")
             ("POINTER" "TO" type)
             ;; The PROCEDURE type is indistinguishable from the beginning
             ;; of a PROCEDURE definition, so we need a "PROCEDURE-type" to
             ;; prevent SMIE from trying to find the matching END.
             ("PROCEDURE-type" proctype)
             ;; OF's right hand side should bind tighter than ; for array
             ;; types, but should bind less tight than | which itself binds
             ;; less tight than ;.  So we use two distinct OFs.
             ("SET" "OF-type" id)
             ("ARRAY" range "OF-type" type))
       (args ("(" fargs ")"))
       ;; VAR has lower precedence than ";" in formal args, but not
       ;; in declarations.  So we use "VAR-arg" for the formal arg case.
       (farg (ids ":" type) ("CONST-arg" farg) ("VAR-arg" farg))
       (fargs (fargs ";" fargs) (farg))
       ;; Handling of PROCEDURE in decls is problematic: we'd want
       ;; TYPE/CONST/VAR/PROCEDURE's parent to be any previous
       ;; CONST/TYPE/VAR/PROCEDURE, but we also want PROCEDURE to be an opener
       ;; (so that its END has PROCEDURE as its parent).  So instead, we treat
       ;; the last ";" in those blocks as a separator (we call it ";-block").
       ;; FIXME: This means that "TYPE \n VAR" is not indented properly
       ;; because there's no ";-block" between the two.
       (decls (decls ";-block" decls)
              ("TYPE" typedecls) ("CONST" constdecls) ("VAR" vardecls)
              ;; END is usually a closer, but not quite for PROCEDURE...END.
              ;; We could use "END-proc" for the procedure case, but
              ;; I preferred to just pretend PROCEDURE's END is the closer.
              ("PROCEDURE" decls "BEGIN" insts "END") ;END-proc id
              ("PROCEDURE" decls "BEGIN" insts "FINALLY" insts "END")
              ("PROCEDURE" decls "FORWARD")
              ;; ("IMPLEMENTATION" epsilon "MODULE" decls
              ;;  "BEGIN" insts "FINALLY" insts "END")
              )
       (typedecls (typedecls ";" typedecls) (id "=" type))
       (ids (ids "," ids))
       (vardecls (vardecls ";" vardecls) (ids ":" type))
       (constdecls (constdecls ";" constdecls) (id "=" exp))
       (exp (id "-anchor-" id) ("(" exp ")"))
       (caselabel (caselabel ".." caselabel) (caselabel "," caselabel))
       ;; : for types binds tighter than ;, but the : for case labels binds
       ;; less tight, so have to use two different :.
       (cases (cases "|" cases) (caselabel ":-case" insts))
       (forspec (exp "TO" exp))
       (insts (insts ";" insts)
              (id ":=" exp)
              ("CASE" exp "OF" cases "END")
              ("CASE" exp "OF" cases "ELSE" insts "END")
              ("LOOP" insts "END")
              ("WITH" exp "DO" insts "END")
              ("REPEAT" insts "UNTIL" exp)
              ("WHILE" exp "DO" insts "END")
              ("FOR" forspec "DO" insts "END")
              ("IF" exp "THEN" insts "END")
              ("IF" exp "THEN" insts "ELSE" insts "END")
              ("IF" exp "THEN" insts
               "ELSIF" exp "THEN" insts "ELSE" insts "END")
              ("IF" exp "THEN" insts
               "ELSIF" exp "THEN" insts
               "ELSIF" exp "THEN" insts "ELSE" insts "END"))
       ;; This category is not used anywhere, but it adds some constraints that
       ;; try to reduce the harm when an OF-type is not properly recognized.
       (error-OF ("ARRAY" range "OF" type) ("SET" "OF" id)))
     '((assoc ";")) '((assoc ";-block")) '((assoc "|"))
     ;; For case labels.
     '((assoc ",") (assoc ".."))
     ;; '((assoc "TYPE" "CONST" "VAR" "PROCEDURE"))
     )
    (smie-precs->prec2
     '((nonassoc "-anchor-" "=")
       (nonassoc "<" "<=" ">=" ">" "<>" "#" "IN")
       (assoc "OR" "+" "-")
       (assoc "AND" "MOD" "DIV" "REM" "*" "/" "&")
       (nonassoc "NOT" "~")
       (left "." "^")
       ))
    )))

(defun m2-smie-refine-colon ()
  (let ((res nil))
    (while (not res)
      (let ((tok (smie-default-backward-token)))
        (cond
         ((zerop (length tok))
          (let ((forward-sexp-function nil))
            (condition-case nil
                (forward-sexp -1)
              (scan-error (setq res ":")))))
         ((member tok '("|" "OF" "..")) (setq res ":-case"))
         ((member tok '(":" "END" ";" "BEGIN" "VAR" "RECORD" "PROCEDURE"))
          (setq res ":")))))
    res))

(defun m2-smie-refine-of ()
  (let ((tok (smie-default-backward-token)))
    (when (zerop (length tok))
      (let ((forward-sexp-function nil))
        (condition-case nil
            (backward-sexp 1)
          (scan-error nil))
        (setq tok (smie-default-backward-token))))
    (if (member tok '("ARRAY" "SET"))
        "OF-type" "OF")))

(defun m2-smie-refine-semi ()
  (forward-comment (point-max))
  (if (looking-at (regexp-opt '("PROCEDURE" "TYPE" "VAR" "CONST" "BEGIN")))
      ";-block" ";"))

;; FIXME: "^." are two tokens, not one.
(defun m2-smie-forward-token ()
  (pcase (smie-default-forward-token)
    (`"VAR" (if (zerop (car (syntax-ppss))) "VAR" "VAR-arg"))
    (`"CONST" (if (zerop (car (syntax-ppss))) "CONST" "CONST-arg"))
    (`";" (save-excursion (m2-smie-refine-semi)))
    (`"OF" (save-excursion (forward-char -2) (m2-smie-refine-of)))
    (`":" (save-excursion (forward-char -1) (m2-smie-refine-colon)))
    ;; (`"END" (if (and (looking-at "[ \t\n]*\\(\\(?:\\sw\\|\\s_\\)+\\)")
    ;;                  (not (assoc (match-string 1) m2-smie-grammar)))
    ;;             "END-proc" "END"))
    (token token)))

(defun m2-smie-backward-token ()
  (pcase (smie-default-backward-token)
    (`"VAR" (if (zerop (car (syntax-ppss))) "VAR" "VAR-arg"))
    (`"CONST" (if (zerop (car (syntax-ppss))) "CONST" "CONST-arg"))
    (`";" (save-excursion (forward-char 1) (m2-smie-refine-semi)))
    (`"OF" (save-excursion (m2-smie-refine-of)))
    (`":" (save-excursion (m2-smie-refine-colon)))
    ;; (`"END" (if (and (looking-at "\\sw+[ \t\n]+\\(\\(?:\\sw\\|\\s_\\)+\\)")
    ;;                  (not (assoc (match-string 1) m2-smie-grammar)))
    ;;             "END-proc" "END"))
    (token token)))

(defun m2-smie-rules (kind token)
  ;; FIXME: Apparently, the usual indentation convention is something like:
  ;;
  ;;    TYPE t1 = bar;
  ;;    VAR x : INTEGER;
  ;;    PROCEDURE f ();
  ;;    TYPE t2 = foo;
  ;;      PROCEDURE g ();
  ;;      BEGIN blabla END;
  ;;    VAR y : type;
  ;;    BEGIN blibli END
  ;;
  ;; This is inconsistent with the actual structure of the code in 2 ways:
  ;; - The inner VAR/TYPE are indented just like the outer VAR/TYPE.
  ;; - The inner PROCEDURE is not aligned with its VAR/TYPE siblings.
  (pcase (cons kind token)
    (`(:elem . basic) m2-indent)
    (`(:after . ":=") (or m2-indent smie-indent-basic))
    (`(:after . ,(or `"CONST" `"VAR" `"TYPE"))
     (or m2-indent smie-indent-basic))
    ;; (`(:before . ,(or `"VAR" `"TYPE" `"CONST"))
    ;;  (if (smie-rule-parent-p "PROCEDURE") 0))
    (`(:after . ";-block")
     (if (smie-rule-parent-p "PROCEDURE")
         (smie-rule-parent (or m2-indent smie-indent-basic))))
    (`(:before . "|") (smie-rule-separator kind))
    ))

;;;###autoload
(defalias 'modula-2-mode 'm2-mode)
;;;###autoload
(define-derived-mode m2-mode prog-mode "Modula-2"
  "This is a mode intended to support program development in Modula-2.
All control constructs of Modula-2 can be reached by typing C-c
followed by the first character of the construct.
\\<m2-mode-map>
  \\[m2-begin] begin         \\[m2-case] case
  \\[m2-definition] definition    \\[m2-else] else
  \\[m2-for] for           \\[m2-header] header
  \\[m2-if] if            \\[m2-module] module
  \\[m2-loop] loop          \\[m2-or] or
  \\[m2-procedure] procedure     Control-c Control-w with
  \\[m2-record] record        \\[m2-stdio] stdio
  \\[m2-type] type          \\[m2-until] until
  \\[m2-var] var           \\[m2-while] while
  \\[m2-export] export        \\[m2-import] import
  \\[m2-begin-comment] begin-comment \\[m2-end-comment] end-comment
  \\[suspend-emacs] suspend Emacs     \\[m2-toggle] toggle
  \\[m2-compile] compile           \\[m2-next-error] next-error
  \\[m2-link] link

   `m2-indent' controls the number of spaces for each indentation.
   `m2-compile-command' holds the command to compile a Modula-2 program.
   `m2-link-command' holds the command to link a Modula-2 program."
  (set (make-local-variable 'paragraph-start) (concat "$\\|" page-delimiter))
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'paragraph-ignore-fill-prefix) t)
  (set (make-local-variable 'comment-start) "(* ")
  (set (make-local-variable 'comment-end) " *)")
  (set (make-local-variable 'comment-start-skip) "\\(?:(\\*+\\|//+\\) *")
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'font-lock-defaults)
	'((m3-font-lock-keywords
	   m3-font-lock-keywords-1 m3-font-lock-keywords-2)
	  nil nil ((?_ . "w") (?. . "w") (?< . ". 1") (?> . ". 4")) nil
	  ))
  (smie-setup m2-smie-grammar #'m2-smie-rules
              :forward-token #'m2-smie-forward-token
              :backward-token #'m2-smie-backward-token))

;; Regexps written with help from Ron Forrester <ron@orcad.com>
;; and Spencer Allain <sallain@teknowledge.com>.
(defconst m3-font-lock-keywords-1
  '(
    ;;
    ;; Module definitions.
    ("\\<\\(INTERFACE\\|MODULE\\|PROCEDURE\\)\\>[ \t]*\\(\\sw+\\)?"
     (1 font-lock-keyword-face) (2 font-lock-function-name-face nil t))
    ;;
    ;; Import directives.
    ("\\<\\(EXPORTS\\|FROM\\|IMPORT\\)\\>"
     (1 font-lock-keyword-face)
     (font-lock-match-c-style-declaration-item-and-skip-to-next
      nil (goto-char (match-end 0))
      (1 font-lock-constant-face)))
    ;;
    ;; Pragmas as warnings.
    ;; Spencer Allain <sallain@teknowledge.com> says do them as comments...
    ;; ("<\\*.*\\*>" . font-lock-warning-face)
    ;; ... but instead we fontify the first word.
    ("<\\*[ \t]*\\(\\sw+\\)" 1 font-lock-warning-face prepend)
    )
  "Subdued level highlighting for Modula-3 modes.")

(defconst m3-font-lock-keywords-2
  (append m3-font-lock-keywords-1
   (eval-when-compile
     (let ((m3-types
	    (regexp-opt
	     '("INTEGER" "BITS" "BOOLEAN" "CARDINAL" "CHAR" "FLOAT" "REAL"
	       "LONGREAL" "REFANY" "ADDRESS" "ARRAY" "SET" "TEXT"
	       "MUTEX" "ROOT" "EXTENDED")))
	   (m3-keywords
	    (regexp-opt
	     '("AND" "ANY" "AS" "BEGIN" "BRANDED" "BY" "CASE" "CONST" "DIV"
	       "DO" "ELSE" "ELSIF" "EVAL" "EXCEPT" "EXIT" "FINALLY"
	       "FOR" "GENERIC" "IF" "IN" "LOCK" "LOOP" "METHODS" "MOD" "NOT"
	       "OBJECT" "OF" "OR" "OVERRIDES" "READONLY" "RECORD" "REF"
	       "REPEAT" "RETURN" "REVEAL" "THEN" "TO" "TRY"
	       "TYPE" "TYPECASE" "UNSAFE" "UNTIL" "UNTRACED" "VAR" "VALUE"
	       "WHILE" "WITH")))
	   (m3-builtins
	    (regexp-opt
	     '("ABS" "ADR" "ADRSIZE" "BITSIZE" "BYTESIZE" "CEILING"
	       "DEC" "DISPOSE" "FIRST" "FLOOR" "INC" "ISTYPE" "LAST"
	       "LOOPHOLE" "MAX" "MIN" "NARROW" "NEW" "NUMBER" "ORD"
	       "ROUND" "SUBARRAY" "TRUNC" "TYPECODE" "VAL")))
	   )
       (list
	;;
	;; Keywords except those fontified elsewhere.
	(concat "\\<\\(" m3-keywords "\\)\\>")
	;;
	;; Builtins.
	(cons (concat "\\<\\(" m3-builtins "\\)\\>") 'font-lock-builtin-face)
	;;
	;; Type names.
	(cons (concat "\\<\\(" m3-types "\\)\\>") 'font-lock-type-face)
	;;
	;; Fontify tokens as function names.
	'("\\<\\(END\\|EXCEPTION\\|RAISES?\\)\\>[ \t{]*"
	  (1 font-lock-keyword-face)
	  (font-lock-match-c-style-declaration-item-and-skip-to-next
	   nil (goto-char (match-end 0))
	   (1 font-lock-function-name-face)))
	;;
	;; Fontify constants as references.
	'("\\<\\(FALSE\\|NIL\\|NULL\\|TRUE\\)\\>" . font-lock-constant-face)
	))))
  "Gaudy level highlighting for Modula-3 modes.")

(defvar m3-font-lock-keywords m3-font-lock-keywords-1
  "Default expressions to highlight in Modula-3 modes.")

;; We don't actually have different keywords for Modula-2.  Volunteers?
(defconst m2-font-lock-keywords-1 m3-font-lock-keywords-1
  "Subdued level highlighting for Modula-2 modes.")

(defconst m2-font-lock-keywords-2 m3-font-lock-keywords-2
  "Gaudy level highlighting for Modula-2 modes.")

(defvar m2-font-lock-keywords m2-font-lock-keywords-1
  "Default expressions to highlight in Modula-2 modes.")

(define-skeleton m2-begin
  "Insert a BEGIN keyword and indent for the next line."
  nil
  \n "BEGIN" > \n)

(define-skeleton m2-case
  "Build skeleton CASE statement, prompting for the <expression>."
  "Case-Expression: "
  \n "CASE " str " OF" > \n _ \n "END (* " str " *);" > \n)

(define-skeleton m2-definition
  "Build skeleton DEFINITION MODULE, prompting for the <module name>."
  "Name: "
  \n "DEFINITION MODULE " str ";" > \n \n _ \n \n "END " str "." > \n)

(define-skeleton m2-else
  "Insert ELSE keyword and indent for next line."
  nil
  \n "ELSE" > \n)

(define-skeleton m2-for
  "Build skeleton FOR loop statement, prompting for the loop parameters."
  "Loop Initializer: "
  ;; FIXME: this seems to be lacking a "<var> :=".
  \n "FOR " str " TO "
  (setq v1 (read-string "Limit: "))
  (let ((by (read-string "Step: ")))
    (if (not (string-equal by ""))
        (concat " BY " by)))
  " DO" > \n _ \n "END (* for " str " to " v1 " *);" > \n)

(define-skeleton m2-header
  "Insert a comment block containing the module title, author, etc."
  "Title: "
  "(*\n    Title: \t" str
  "\n    Created: \t" (current-time-string)
  "\n    Author: \t"  (user-full-name) " <" user-mail-address ">\n"
  "*)" > \n)

(define-skeleton m2-if
  "Insert skeleton IF statement, prompting for <boolean-expression>."
  "<boolean-expression>: "
  \n "IF " str " THEN" > \n _ \n "END (* if " str " *);" > \n)

(define-skeleton m2-loop
  "Build skeleton LOOP (with END)."
  nil
  \n "LOOP" > \n _ \n "END (* loop *);" > \n)

(define-skeleton m2-module
  "Build skeleton IMPLEMENTATION MODULE, prompting for <module-name>."
  "Name: "
  \n "IMPLEMENTATION MODULE " str ";" > \n \n
  '(m2-header)
  '(m2-type) \n
  '(m2-var) \n _ \n \n
  '(m2-begin)
  '(m2-begin-comment)
  " Module " str " Initialization Code "
  '(m2-end-comment)
  \n \n "END " str "." > \n)

(define-skeleton m2-or
  "No doc."
  nil
  \n "|" > \n)

(define-skeleton m2-procedure
  "No doc."
  "Name: "
  \n "PROCEDURE " str " (" (read-string "Arguments: ") ")"
  (let ((args (read-string "Result Type: ")))
    (if (not (equal args "")) (concat " : " args)))
  ";" > \n "BEGIN" > \n _ \n "END " str ";" > \n)

(define-skeleton m2-with
  "No doc."
  "Record-Type: "
  \n "WITH " str " DO" > \n _ \n "END (* with " str " *);" > \n)

(define-skeleton m2-record
  "No doc."
  nil
  \n "RECORD" > \n _ \n "END (* record *);" > \n)

(define-skeleton m2-stdio
  "No doc."
  nil
  \n "FROM TextIO IMPORT"
  > \n "WriteCHAR, ReadCHAR, WriteINTEGER, ReadINTEGER,"
  > \n "WriteCARDINAL, ReadCARDINAL, WriteBOOLEAN, ReadBOOLEAN,"
  > \n "WriteREAL, ReadREAL, WriteBITSET, ReadBITSET,"
  > \n "WriteBasedCARDINAL, ReadBasedCARDINAL, WriteChars, ReadChars,"
  > \n "WriteString, ReadString, WhiteSpace, EndOfLine;"
  > \n \n "FROM SysStreams IMPORT sysIn, sysOut, sysErr;" > \n \n)

(define-skeleton m2-type
  "No doc."
  nil
  \n "TYPE" > \n ";" > \n)

(define-skeleton m2-until
  "No doc."
  "<boolean-expression>: "
  \n "REPEAT" > \n _ \n "UNTIL " str ";" > \n)

(define-skeleton m2-var
  "No doc."
  nil
  \n "VAR" > \n ";" > \n)

(define-skeleton m2-while
  "No doc."
  "<boolean-expression>: "
  \n "WHILE " str " DO" > \n _ \n "END (* while " str " *);" > \n)

(define-skeleton m2-export
  "No doc."
  nil
  \n "EXPORT QUALIFIED " > _ \n)

(define-skeleton m2-import
  "No doc."
  "Module: "
  \n "FROM " str " IMPORT " > _ \n)

(defun m2-begin-comment ()
  (interactive)
  (if (not (bolp))
      (indent-to comment-column 0))
  (insert "(*  "))

(defun m2-end-comment ()
  (interactive)
  (if (not (bolp))
      (indent-to m2-end-comment-column))
  (insert "*)"))

(defun m2-compile ()
  (interactive)
  (compile (concat m2-compile-command " " (buffer-name))))

(defun m2-link ()
  (interactive)
  (compile (concat m2-link-command " "
                   (or m2-link-name
                       (setq m2-link-name (read-string "Name of executable: "
                                                       (buffer-name)))))))

(defun m2-execute-monitor-command (command)
  (let* ((shell shell-file-name)
	 ;; (csh (equal (file-name-nondirectory shell) "csh"))
         )
    (call-process shell nil t t "-cf" (concat "exec " command))))

(defun m2-visit ()
  (interactive)
  (let ((deffile nil)
	(modfile nil)
	modulename)
    (save-excursion
      (setq modulename
	    (read-string "Module name: "))
      (switch-to-buffer "*Command Execution*")
      (m2-execute-monitor-command (concat "m2whereis " modulename))
      (goto-char (point-min))
      (condition-case ()
	  (progn (re-search-forward "\\(.*\\.def\\) *$")
		 (setq deffile (buffer-substring (match-beginning 1)
						 (match-end 1))))
	(search-failed ()))
      (condition-case ()
	  (progn (re-search-forward "\\(.*\\.mod\\) *$")
		 (setq modfile (buffer-substring (match-beginning 1)
						 (match-end 1))))
	(search-failed ()))
      (if (not (or deffile modfile))
	  (error "I can find neither definition nor implementation of %s"
		 modulename)))
    (cond (deffile
	    (find-file deffile)
	    (if modfile
		(save-excursion
		  (find-file modfile))))
	  (modfile
	   (find-file modfile)))))

(defun m2-toggle ()
  "Toggle between .mod and .def files for the module."
  (interactive)
  (cond ((string-equal (substring (buffer-name) -4) ".def")
	 (find-file-other-window
	  (concat (substring (buffer-name) 0 -4) ".mod")))
	((string-equal (substring (buffer-name) -4) ".mod")
	 (find-file-other-window
	  (concat (substring (buffer-name) 0 -4)  ".def")))
	((string-equal (substring (buffer-name) -3) ".mi")
	 (find-file-other-window
	  (concat (substring (buffer-name) 0 -3)  ".md")))
	((string-equal (substring (buffer-name) -3) ".md")
	 (find-file-other-window
	  (concat (substring (buffer-name) 0 -3)  ".mi")))))

(provide 'modula2)

;;; modula2.el ends here
