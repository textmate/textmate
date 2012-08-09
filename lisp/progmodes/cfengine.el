;;; cfengine.el --- mode for editing Cfengine files

;; Copyright (C) 2001-2012  Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; Maintainer: Ted Zlatanov <tzz@lifelogs.com>
;; Keywords: languages
;; Version: 1.1

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

;; Provides support for editing GNU Cfengine files, including
;; font-locking, Imenu and indentation, but with no special keybindings.

;; The CFEngine 3.x support doesn't have Imenu support but patches are
;; welcome.

;; You can set it up so either `cfengine2-mode' (2.x and earlier) or
;; `cfengine3-mode' (3.x) will be picked, depending on the buffer
;; contents:

;; (add-to-list 'auto-mode-alist '("\\.cf\\'" . cfengine-mode))

;; OR you can choose to always use a specific version, if you prefer
;; it:

;; (add-to-list 'auto-mode-alist '("\\.cf\\'" . cfengine3-mode))
;; (add-to-list 'auto-mode-alist '("^cf\\." . cfengine2-mode))
;; (add-to-list 'auto-mode-alist '("^cfagent.conf\\'" . cfengine2-mode))

;; This is not the same as the mode written by Rolf Ebert
;; <ebert@waporo.muc.de>, distributed with cfengine-2.0.5.  It does
;; better fontification and indentation, inter alia.

;;; Code:

(defgroup cfengine ()
  "Editing CFEngine files."
  :group 'languages)

(defcustom cfengine-indent 2
  "*Size of a CFEngine indentation step in columns."
  :group 'cfengine
  :type 'integer)

(defvar cfengine-mode-debug nil
  "Whether `cfengine-mode' should print debugging info.")

(defcustom cfengine-mode-abbrevs nil
  "Abbrevs for CFEngine2 mode."
  :group 'cfengine
  :type '(repeat (list (string :tag "Name")
		       (string :tag "Expansion")
		       (choice  :tag "Hook" (const nil) function))))

(make-obsolete-variable 'cfengine-mode-abbrevs 'edit-abbrevs "24.1")

;; Taken from the doc for pre-release 2.1.
(eval-and-compile
  (defconst cfengine2-actions
    '("acl" "alerts" "binservers" "broadcast" "control" "classes" "copy"
      "defaultroute" "disks" "directories" "disable" "editfiles" "files"
      "filters" "groups" "homeservers" "ignore" "import" "interfaces"
      "links" "mailserver" "methods" "miscmounts" "mountables"
      "processes" "packages" "rename" "required" "resolve"
      "shellcommands" "tidy" "unmount"
      ;; Keywords for cfservd.
      "admit" "grant" "deny")
    "List of the action keywords supported by Cfengine.
This includes those for cfservd as well as cfagent.")

  (defconst cfengine3-defuns
    (mapcar
     'symbol-name
     '(bundle body))
    "List of the CFEngine 3.x defun headings.")

  (defconst cfengine3-defuns-regex
    (regexp-opt cfengine3-defuns t)
    "Regex to match the CFEngine 3.x defuns.")

  (defconst cfengine3-class-selector-regex "\\([[:alnum:]_().&|!]+\\)::")

  (defconst cfengine3-category-regex "\\([[:alnum:]_]+\\):")

  (defconst cfengine3-vartypes
    (mapcar
     'symbol-name
     '(string int real slist ilist rlist irange rrange counter))
    "List of the CFEngine 3.x variable types."))

(defvar cfengine2-font-lock-keywords
  `(;; Actions.
    ;; List the allowed actions explicitly, so that errors are more obvious.
    (,(concat "^[ \t]*" (eval-when-compile
			  (regexp-opt cfengine2-actions t))
	      ":")
     1 font-lock-keyword-face)
    ;; Classes.
    ("^[ \t]*\\([[:alnum:]_().|!]+\\)::" 1 font-lock-function-name-face)
    ;; Variables.
    ("$(\\([[:alnum:]_]+\\))" 1 font-lock-variable-name-face)
    ("${\\([[:alnum:]_]+\\)}" 1 font-lock-variable-name-face)
    ;; Variable definitions.
    ("\\<\\([[:alnum:]_]+\\)[ \t]*=[ \t]*(" 1 font-lock-variable-name-face)
    ;; File, acl &c in group:   { token ... }
    ("{[ \t]*\\([^ \t\n]+\\)" 1 font-lock-constant-face)))

(defvar cfengine3-font-lock-keywords
  `(
    ;; Defuns.  This happens early so they don't get caught by looser
    ;; patterns.
    (,(concat "\\<" cfengine3-defuns-regex "\\>"
              "[ \t]+\\<\\([[:alnum:]_]+\\)\\>"
              "[ \t]+\\<\\([[:alnum:]_]+\\)"
              ;; Optional parentheses with variable names inside.
              "\\(?:(\\([^)]*\\))\\)?")
     (1 font-lock-builtin-face)
     (2 font-lock-constant-face)
     (3 font-lock-function-name-face)
     (4 font-lock-variable-name-face nil t))

    ;; Class selectors.
    (,(concat "^[ \t]*" cfengine3-class-selector-regex)
     1 font-lock-keyword-face)

    ;; Categories.
    (,(concat "^[ \t]*" cfengine3-category-regex)
     1 font-lock-builtin-face)

    ;; Variables, including scope, e.g. module.var
    ("[@$](\\([[:alnum:]_.]+\\))" 1 font-lock-variable-name-face)
    ("[@$]{\\([[:alnum:]_.]+\\)}" 1 font-lock-variable-name-face)

    ;; Variable definitions.
    ("\\<\\([[:alnum:]_]+\\)[ \t]*=[ \t]*(" 1 font-lock-variable-name-face)

    ;; Variable types.
    (,(concat "\\<" (eval-when-compile (regexp-opt cfengine3-vartypes t)) "\\>")
     1 font-lock-type-face)))

(defvar cfengine2-imenu-expression
  `((nil ,(concat "^[ \t]*" (eval-when-compile
			      (regexp-opt cfengine2-actions t))
		  ":[^:]")
	 1)
    ("Variables/classes" "\\<\\([[:alnum:]_]+\\)[ \t]*=[ \t]*(" 1)
    ("Variables/classes" "\\<define=\\([[:alnum:]_]+\\)" 1)
    ("Variables/classes" "\\<DefineClass\\>[ \t]+\\([[:alnum:]_]+\\)" 1))
  "`imenu-generic-expression' for CFEngine mode.")

(defun cfengine2-outline-level ()
  "`outline-level' function for CFEngine mode."
  (if (looking-at "[^:]+\\(?:[:]+\\)$")
      (length (match-string 1))))

(defun cfengine2-beginning-of-defun ()
  "`beginning-of-defun' function for CFEngine mode.
Treats actions as defuns."
  (unless (<= (current-column) (current-indentation))
    (end-of-line))
  (if (re-search-backward "^[[:alpha:]]+: *$" nil t)
      (beginning-of-line)
    (goto-char (point-min)))
  t)

(defun cfengine2-end-of-defun ()
  "`end-of-defun' function for CFEngine mode.
Treats actions as defuns."
  (end-of-line)
  (if (re-search-forward "^[[:alpha:]]+: *$" nil t)
      (beginning-of-line)
    (goto-char (point-max)))
  t)

;; Fixme: Should get an extra indent step in editfiles BeginGroup...s.

(defun cfengine2-indent-line ()
  "Indent a line in Cfengine mode.
Intended as the value of `indent-line-function'."
  (let ((pos (- (point-max) (point))))
    (save-restriction
      (narrow-to-defun)
      (back-to-indentation)
      (cond
       ;; Action selectors aren't indented; class selectors are
       ;; indented one step.
       ((looking-at "[[:alnum:]_().|!]+:\\(:\\)?")
	(if (match-string 1)
	    (indent-line-to cfengine-indent)
	  (indent-line-to 0)))
       ;; Outdent leading close brackets one step.
       ((or (eq ?\} (char-after))
	    (eq ?\) (char-after)))
	(condition-case ()
	    (indent-line-to (save-excursion
			      (forward-char)
			      (backward-sexp)
			      (current-column)))
	  (error nil)))
       ;; Inside brackets/parens: indent to start column of non-comment
       ;; token on line following open bracket or by one step from open
       ;; bracket's column.
       ((condition-case ()
	    (progn (indent-line-to (save-excursion
				     (backward-up-list)
				     (forward-char)
				     (skip-chars-forward " \t")
				     (if (looking-at "[^\n#]")
					 (current-column)
				       (skip-chars-backward " \t")
				       (+ (current-column) -1
					  cfengine-indent))))
		   t)
	  (error nil)))
       ;; Indent by  two steps after a class selector.
       ((save-excursion
	  (re-search-backward "^[ \t]*[[:alnum:]_().|!]+::" nil t))
	(indent-line-to (* 2 cfengine-indent)))
       ;; Indent by one step if we're after an action header.
       ((save-excursion
	  (goto-char (point-min))
	  (looking-at "[[:alpha:]]+:[ \t]*$"))
	(indent-line-to cfengine-indent))
       ;; Else don't indent.
       (t
	(indent-line-to 0))))
    ;; If initial point was within line's indentation,
    ;; position after the indentation.  Else stay at same point in text.
    (if (> (- (point-max) pos) (point))
	(goto-char (- (point-max) pos)))))

;; This doesn't work too well in Emacs 21.2.  See 22.1 development
;; code.
(defun cfengine-fill-paragraph (&optional justify)
  "Fill `paragraphs' in Cfengine code."
  (interactive "P")
  (or (if (fboundp 'fill-comment-paragraph)
	  (fill-comment-paragraph justify) ; post Emacs 21.3
	;; else do nothing in a comment
	(nth 4 (parse-partial-sexp (save-excursion
				     (beginning-of-defun)
				     (point))
				   (point))))
      (let ((paragraph-start
	     ;; Include start of parenthesized block.
	     "\f\\|[ \t]*$\\|.*\(")
	    (paragraph-separate
	     ;; Include action and class lines, start and end of
	     ;; bracketed blocks and end of parenthesized blocks to
	     ;; avoid including these in fill.  This isn't ideal.
	     "[ \t\f]*$\\|.*#\\|.*[\){}]\\|\\s-*[[:alpha:]_().|!]+:")
	    fill-paragraph-function)
	(fill-paragraph justify))
      t))

(defun cfengine3-beginning-of-defun ()
  "`beginning-of-defun' function for Cfengine 3 mode.
Treats body/bundle blocks as defuns."
  (unless (<= (current-column) (current-indentation))
    (end-of-line))
  (if (re-search-backward (concat "^[ \t]*" cfengine3-defuns-regex "\\>") nil t)
      (beginning-of-line)
    (goto-char (point-min)))
  t)

(defun cfengine3-end-of-defun ()
  "`end-of-defun' function for Cfengine 3 mode.
Treats body/bundle blocks as defuns."
  (end-of-line)
  (if (re-search-forward (concat "^[ \t]*" cfengine3-defuns-regex "\\>") nil t)
      (beginning-of-line)
    (goto-char (point-max)))
  t)

(defun cfengine3-indent-line ()
  "Indent a line in Cfengine 3 mode.
Intended as the value of `indent-line-function'."
  (let ((pos (- (point-max) (point)))
        parse)
    (save-restriction
      (narrow-to-defun)
      (back-to-indentation)
      (setq parse (parse-partial-sexp (point-min) (point)))
      (when cfengine-mode-debug
        (message "%S" parse))

      (cond
       ;; Body/bundle blocks start at 0.
       ((looking-at (concat cfengine3-defuns-regex "\\>"))
        (indent-line-to 0))
       ;; Categories are indented one step.
       ((looking-at (concat cfengine3-category-regex "[ \t]*$"))
        (indent-line-to cfengine-indent))
       ;; Class selectors are indented two steps.
       ((looking-at (concat cfengine3-class-selector-regex "[ \t]*$"))
        (indent-line-to (* 2 cfengine-indent)))
       ;; Outdent leading close brackets one step.
       ((or (eq ?\} (char-after))
            (eq ?\) (char-after)))
        (condition-case ()
            (indent-line-to (save-excursion
                              (forward-char)
                              (backward-sexp)
                              (current-column)))
          (error nil)))
       ;; Inside a string and it starts before this line.
       ((and (nth 3 parse)
             (< (nth 8 parse) (save-excursion (beginning-of-line) (point))))
        (indent-line-to 0))

       ;; Inside a defun, but not a nested list (depth is 1).  This is
       ;; a promise, usually.

       ;; Indent to cfengine-indent times the nested depth
       ;; plus 2.  That way, promises indent deeper than class
       ;; selectors, which in turn are one deeper than categories.
       ((= 1 (nth 0 parse))
        (indent-line-to (* (+ 2 (nth 0 parse)) cfengine-indent)))
       ;; Inside brackets/parens: indent to start column of non-comment
       ;; token on line following open bracket or by one step from open
       ;; bracket's column.
       ((condition-case ()
            (progn (indent-line-to (save-excursion
                                     (backward-up-list)
                                     (forward-char)
                                     (skip-chars-forward " \t")
                                     (cond
                                      ((looking-at "[^\n#]")
                                       (current-column))
                                      ((looking-at "[^\n#]")
                                       (current-column))
                                      (t
                                       (skip-chars-backward " \t")
                                       (+ (current-column) -1
                                          cfengine-indent)))))
                   t)
          (error nil)))
       ;; Else don't indent.
       (t (indent-line-to 0))))
    ;; If initial point was within line's indentation,
    ;; position after the indentation.  Else stay at same point in text.
    (if (> (- (point-max) pos) (point))
        (goto-char (- (point-max) pos)))))

;; CFEngine 3.x grammar

;; specification: blocks
;; blocks: block | blocks block;
;; block:                 bundle typeid blockid bundlebody
;;                      | bundle typeid blockid usearglist bundlebody
;;                      | body typeid blockid bodybody
;;                      | body typeid blockid usearglist bodybody;

;; typeid: id
;; blockid: id
;; usearglist: '(' aitems ')';
;; aitems: aitem | aitem ',' aitems |;
;; aitem: id

;; bundlebody: '{' statements '}'
;; statements: statement | statements statement;
;; statement: category | classpromises;

;; bodybody: '{' bodyattribs '}'
;; bodyattribs: bodyattrib | bodyattribs bodyattrib;
;; bodyattrib: class | selections;
;; selections: selection | selections selection;
;; selection: id ASSIGN rval ';' ;

;; classpromises: classpromise | classpromises classpromise;
;; classpromise: class | promises;
;; promises: promise | promises promise;
;; category: CATEGORY
;; promise: promiser ARROW rval constraints ';' | promiser constraints ';';
;; constraints: constraint | constraints ',' constraint |;
;; constraint: id ASSIGN rval;
;; class: CLASS
;; id: ID
;; rval: ID | QSTRING | NAKEDVAR | list | usefunction
;; list: '{' litems '}' ;
;; litems: litem | litem ',' litems |;
;; litem: ID | QSTRING | NAKEDVAR | list | usefunction

;; functionid: ID | NAKEDVAR
;; promiser: QSTRING
;; usefunction: functionid givearglist
;; givearglist: '(' gaitems ')'
;; gaitems: gaitem | gaitems ',' gaitem |;
;; gaitem: ID | QSTRING | NAKEDVAR | list | usefunction

;; # from lexer:

;; bundle: "bundle"
;; body: "body"
;; COMMENT    #[^\n]*
;; NAKEDVAR   [$@][(][a-zA-Z0-9_\200-\377.]+[)]|[$@][{][a-zA-Z0-9_\200-\377.]+[}]
;; ID: [a-zA-Z0-9_\200-\377]+
;; ASSIGN: "=>"
;; ARROW: "->"
;; QSTRING: \"((\\\")|[^"])*\"|\'((\\\')|[^'])*\'|`[^`]*`
;; CLASS: [.|&!()a-zA-Z0-9_\200-\377]+::
;; CATEGORY: [a-zA-Z_]+:

(defun cfengine-common-settings ()
  (set (make-local-variable 'syntax-propertize-function)
       ;; In the main syntax-table, \ is marked as a punctuation, because
       ;; of its use in DOS-style directory separators.  Here we try to
       ;; recognize the cases where \ is used as an escape inside strings.
       (syntax-propertize-rules ("\\(\\(?:\\\\\\)+\\)\"" (1 "\\"))))
  (set (make-local-variable 'parens-require-spaces) nil)
  (set (make-local-variable 'comment-start)  "# ")
  (set (make-local-variable 'comment-start-skip)
       "\\(\\(?:^\\|[^\\\\\n]\\)\\(?:\\\\\\\\\\)*\\)#+[ \t]*")
  ;; Like Lisp mode.  Without this, we lose with, say,
  ;; `backward-up-list' when there's an unbalanced quote in a
  ;; preceding comment.
  (set (make-local-variable 'parse-sexp-ignore-comments) t))

(defun cfengine-common-syntax (table)
  ;; The syntax defaults seem OK to give reasonable word movement.
  (modify-syntax-entry ?# "<" table)
  (modify-syntax-entry ?\n ">#" table)
  (modify-syntax-entry ?\" "\"" table)
  ;; Variable substitution.
  (modify-syntax-entry ?$ "." table)
  ;; Doze path separators.
  (modify-syntax-entry ?\\ "." table))

;;;###autoload
(define-derived-mode cfengine3-mode prog-mode "CFE3"
  "Major mode for editing CFEngine3 input.
There are no special keybindings by default.

Action blocks are treated as defuns, i.e. \\[beginning-of-defun] moves
to the action header."
  (cfengine-common-settings)
  (cfengine-common-syntax cfengine3-mode-syntax-table)

  (set (make-local-variable 'indent-line-function) #'cfengine3-indent-line)
  (setq font-lock-defaults
        '(cfengine3-font-lock-keywords nil nil nil beginning-of-defun))

  ;; Use defuns as the essential syntax block.
  (set (make-local-variable 'beginning-of-defun-function)
       #'cfengine3-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function)
       #'cfengine3-end-of-defun))

;;;###autoload
(define-derived-mode cfengine2-mode prog-mode "CFE2"
  "Major mode for editing CFEngine2 input.
There are no special keybindings by default.

Action blocks are treated as defuns, i.e. \\[beginning-of-defun] moves
to the action header."
  (cfengine-common-settings)
  (cfengine-common-syntax cfengine2-mode-syntax-table)

  ;; Shell commands can be quoted by single, double or back quotes.
  ;; It's debatable whether we should define string syntax, but it
  ;; should avoid potential confusion in some cases.
  (modify-syntax-entry ?\' "\"" cfengine2-mode-syntax-table)
  (modify-syntax-entry ?\` "\"" cfengine2-mode-syntax-table)

  (set (make-local-variable 'indent-line-function) #'cfengine2-indent-line)
  (set (make-local-variable 'outline-regexp) "[ \t]*\\(\\sw\\|\\s_\\)+:+")
  (set (make-local-variable 'outline-level) #'cfengine2-outline-level)
  (set (make-local-variable 'fill-paragraph-function)
       #'cfengine-fill-paragraph)
  (define-abbrev-table 'cfengine2-mode-abbrev-table cfengine-mode-abbrevs)
  (setq font-lock-defaults
        '(cfengine2-font-lock-keywords nil nil nil beginning-of-line))
  ;; Fixme: set the args of functions in evaluated classes to string
  ;; syntax, and then obey syntax properties.
  (setq imenu-generic-expression cfengine2-imenu-expression)
  (set (make-local-variable 'beginning-of-defun-function)
       #'cfengine2-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function) #'cfengine2-end-of-defun))

;;;###autoload
(defun cfengine-auto-mode ()
  "Choose between `cfengine2-mode' and `cfengine3-mode' depending
on the buffer contents"
  (let ((v3 nil))
    (save-restriction
      (goto-char (point-min))
      (while (not (or (eobp) v3))
        (setq v3 (looking-at (concat cfengine3-defuns-regex "\\>")))
        (forward-line)))
    (if v3 (cfengine3-mode) (cfengine2-mode))))

(defalias 'cfengine-mode 'cfengine-auto-mode)

(provide 'cfengine3)
(provide 'cfengine)

;;; cfengine.el ends here
