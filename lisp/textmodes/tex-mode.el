;;; tex-mode.el --- TeX, LaTeX, and SliTeX mode commands -*- coding: utf-8 -*-

;; Copyright (C) 1985-1986, 1989, 1992, 1994-1999, 2001-2012
;;   Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: tex

;; Contributions over the years by William F. Schelter, Dick King,
;; Stephen Gildea, Michael Prange, Jacob Gore, and Edward M. Reingold.

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

;;; Code:

;; Pacify the byte-compiler
(eval-when-compile
  (require 'compare-w)
  (require 'cl)
  (require 'skeleton))

(defvar font-lock-comment-face)
(defvar font-lock-doc-face)

(require 'shell)
(require 'compile)

(defgroup tex-file nil
  "TeX files and directories."
  :prefix "tex-"
  :group 'tex)

(defgroup tex-run nil
  "Running external commands from TeX mode."
  :prefix "tex-"
  :group 'tex)

(defgroup tex-view nil
  "Viewing and printing TeX files."
  :prefix "tex-"
  :group 'tex)

;;;###autoload
(defcustom tex-shell-file-name nil
  "If non-nil, the shell file name to run in the subshell used to run TeX."
  :type '(choice (const :tag "None" nil)
		 string)
  :group 'tex-run)

;;;###autoload
(defcustom tex-directory (purecopy ".")
  "Directory in which temporary files are written.
You can make this `/tmp' if your TEXINPUTS has no relative directories in it
and you don't try to apply \\[tex-region] or \\[tex-buffer] when there are
`\\input' commands with relative directories."
  :type 'directory
  :group 'tex-file)

;;;###autoload
(defcustom tex-first-line-header-regexp nil
  "Regexp for matching a first line which `tex-region' should include.
If this is non-nil, it should be a regular expression string;
if it matches the first line of the file,
`tex-region' always includes the first line in the TeX run."
  :type '(choice (const :tag "None" nil)
                 regexp)
  :group 'tex-file)

;;;###autoload
(defcustom tex-main-file nil
  "The main TeX source file which includes this buffer's file.
The command `tex-file' runs TeX on the file specified by `tex-main-file'
if the variable is non-nil."
  :type '(choice (const :tag "None" nil)
                 file)
  :group 'tex-file)

;;;###autoload
(defcustom tex-offer-save t
  "If non-nil, ask about saving modified buffers before \\[tex-file] is run."
  :type 'boolean
  :group 'tex-file)

;;;###autoload
(defcustom tex-run-command (purecopy "tex")
  "Command used to run TeX subjob.
TeX Mode sets `tex-command' to this string.
See the documentation of that variable."
  :type 'string
  :group 'tex-run)

;;;###autoload
(defcustom latex-run-command (purecopy "latex")
  "Command used to run LaTeX subjob.
LaTeX Mode sets `tex-command' to this string.
See the documentation of that variable."
  :type 'string
  :group 'tex-run)

;;;###autoload
(defcustom slitex-run-command (purecopy "slitex")
  "Command used to run SliTeX subjob.
SliTeX Mode sets `tex-command' to this string.
See the documentation of that variable."
  :type 'string
  :group 'tex-run)

;;;###autoload
(defcustom tex-start-options (purecopy "")
  "TeX options to use when starting TeX.
These immediately precede the commands in `tex-start-commands'
and the input file name, with no separating space and are not shell-quoted.
If nil, TeX runs with no options.  See the documentation of `tex-command'."
  :type 'string
  :group 'tex-run
  :version "22.1")

;;;###autoload
(defcustom tex-start-commands (purecopy "\\nonstopmode\\input")
  "TeX commands to use when starting TeX.
They are shell-quoted and precede the input file name, with a separating space.
If nil, no commands are used.  See the documentation of `tex-command'."
  :type '(radio (const :tag "Interactive \(nil\)" nil)
		(const :tag "Nonstop \(\"\\nonstopmode\\input\"\)"
		       "\\nonstopmode\\input")
		(string :tag "String at your choice"))
  :group 'tex-run
  :version "22.1")

(defvar latex-standard-block-names
  '("abstract"		"array"		"center"	"description"
    "displaymath"	"document"	"enumerate"	"eqnarray"
    "eqnarray*"		"equation"	"figure"	"figure*"
    "flushleft"		"flushright"	"itemize"	"letter"
    "list"		"minipage"	"picture"	"quotation"
    "quote"		"slide"		"sloppypar"	"tabbing"
    "table"		"table*"	"tabular"	"tabular*"
    "thebibliography"	"theindex*"	"titlepage"	"trivlist"
    "verbatim"		"verbatim*"	"verse"		"math")
  "Standard LaTeX block names.")

;;;###autoload
(defcustom latex-block-names nil
  "User defined LaTeX block names.
Combined with `latex-standard-block-names' for minibuffer completion."
  :type '(repeat string)
  :group 'tex-run)

;;;###autoload
(defcustom tex-bibtex-command (purecopy "bibtex")
  "Command used by `tex-bibtex-file' to gather bibliographic data.
If this string contains an asterisk (`*'), that is replaced by the file name;
otherwise, the file name, preceded by blank, is added at the end."
  :type 'string
  :group 'tex-run)

;;;###autoload
(defcustom tex-dvi-print-command (purecopy "lpr -d")
  "Command used by \\[tex-print] to print a .dvi file.
If this string contains an asterisk (`*'), that is replaced by the file name;
otherwise, the file name, preceded by blank, is added at the end."
  :type 'string
  :group 'tex-view)

;;;###autoload
(defcustom tex-alt-dvi-print-command (purecopy "lpr -d")
  "Command used by \\[tex-print] with a prefix arg to print a .dvi file.
If this string contains an asterisk (`*'), that is replaced by the file name;
otherwise, the file name, preceded by blank, is added at the end.

If two printers are not enough of a choice, you can set the variable
`tex-alt-dvi-print-command' to an expression that asks what you want;
for example,

    (setq tex-alt-dvi-print-command
         '(format \"lpr -P%s\" (read-string \"Use printer: \")))

would tell \\[tex-print] with a prefix argument to ask you which printer to
use."
  :type '(choice (string :tag "Command")
		 (sexp :tag "Expression"))
  :group 'tex-view)

;;;###autoload
(defcustom tex-dvi-view-command
  `(cond
    ((eq window-system 'x) ,(purecopy "xdvi"))
    ((eq window-system 'w32) ,(purecopy "yap"))
    (t ,(purecopy "dvi2tty * | cat -s")))
  "Command used by \\[tex-view] to display a `.dvi' file.
If it is a string, that specifies the command directly.
If this string contains an asterisk (`*'), that is replaced by the file name;
otherwise, the file name, preceded by a space, is added at the end.

If the value is a form, it is evaluated to get the command to use."
  :type '(choice (const nil) string sexp)
  :group 'tex-view)

;;;###autoload
(defcustom tex-show-queue-command (purecopy "lpq")
  "Command used by \\[tex-show-print-queue] to show the print queue.
Should show the queue(s) that \\[tex-print] puts jobs on."
  :type 'string
  :group 'tex-view)

;;;###autoload
(defcustom tex-default-mode 'latex-mode
  "Mode to enter for a new file that might be either TeX or LaTeX.
This variable is used when it can't be determined whether the file
is plain TeX or LaTeX or what because the file contains no commands.
Normally set to either `plain-tex-mode' or `latex-mode'."
  :type 'function
  :group 'tex)

;;;###autoload
(defcustom tex-open-quote (purecopy "``")
  "String inserted by typing \\[tex-insert-quote] to open a quotation."
  :type 'string
  :options '("``" "\"<" "\"`" "<<" "«")
  :group 'tex)

;;;###autoload
(defcustom tex-close-quote (purecopy "''")
  "String inserted by typing \\[tex-insert-quote] to close a quotation."
  :type 'string
  :options '("''" "\">" "\"'" ">>" "»")
  :group 'tex)

(defcustom tex-fontify-script t
  "If non-nil, fontify subscript and superscript strings."
  :type 'boolean
  :group 'tex
  :version "23.1")
(put 'tex-fontify-script 'safe-local-variable 'booleanp)

(defcustom tex-font-script-display '(-0.2 0.2)
  "How much to lower and raise subscript and superscript content.
This is a list of two floats.  The first is negative and
specifies how much subscript is lowered, the second is positive
and specifies how much superscript is raised.  Heights are
measured relative to that of the normal text."
  :group 'tex
  :type '(list (float :tag "Subscript")
               (float :tag "Superscript"))
  :version "23.1")

(defvar tex-last-temp-file nil
  "Latest temporary file generated by \\[tex-region] and \\[tex-buffer].
Deleted when the \\[tex-region] or \\[tex-buffer] is next run, or when the
tex shell terminates.")

(defvar tex-command "tex"
  "*Command to run TeX.
If this string contains an asterisk \(`*'\), that is replaced by the file name;
otherwise the value of `tex-start-options', the \(shell-quoted\)
value of `tex-start-commands', and the file name are added at the end
with blanks as separators.

In TeX, LaTeX, and SliTeX Mode this variable becomes buffer local.
In these modes, use \\[set-variable] if you want to change it for the
current buffer.")

(defvar tex-trailer nil
  "String appended after the end of a region sent to TeX by \\[tex-region].")

(defvar tex-start-of-header nil
  "Regular expression used by \\[tex-region] to find start of file's header.")

(defvar tex-end-of-header nil
  "Regular expression used by \\[tex-region] to find end of file's header.")

(defvar tex-shell-cd-command "cd"
  "Command to give to shell running TeX to change directory.
The value of `tex-directory' is appended to this, separated by a space.")

(defvar tex-zap-file nil
  "Temporary file name used for text being sent as input to TeX.
Should be a simple file name with no extension or directory specification.")

(defvar tex-last-buffer-texed nil
  "Buffer which was last TeXed.")

(defvar tex-print-file nil
  "File name that \\[tex-print] prints.
Set by \\[tex-region], \\[tex-buffer], and \\[tex-file].")

(defvar tex-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?% "<" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\f ">" st)
    (modify-syntax-entry ?\C-@ "w" st)
    (modify-syntax-entry ?' "w" st)
    (modify-syntax-entry ?@ "_" st)
    (modify-syntax-entry ?* "_" st)
    (modify-syntax-entry ?\t " " st)
    ;; ~ is printed by TeX as a space, but it's semantics in the syntax
    ;; of TeX is not `whitespace' (i.e. it's just like \hspace{foo}).
    (modify-syntax-entry ?~ "." st)
    (modify-syntax-entry ?$ "$$" st)
    (modify-syntax-entry ?\\ "/" st)
    (modify-syntax-entry ?\" "." st)
    (modify-syntax-entry ?& "." st)
    (modify-syntax-entry ?_ "." st)
    (modify-syntax-entry ?^ "." st)
    st)
  "Syntax table used while in TeX mode.")

;;;;
;;;; Imenu support
;;;;

(defcustom latex-imenu-indent-string ". "
  "String to add repeated in front of nested sectional units for Imenu.
An alternative value is \" . \", if you use a font with a narrow period."
  :type 'string
  :group 'tex)

(defvar latex-section-alist
  '(("part" . 0) ("chapter" . 1)
    ("section" . 2) ("subsection" . 3)
    ("subsubsection" . 4)
    ("paragraph" . 5) ("subparagraph" . 6)))

(defvar latex-metasection-list
  '("documentstyle" "documentclass"
    "begin{document}" "end{document}"
    "appendix" "frontmatter" "mainmatter" "backmatter"))

(defun latex-imenu-create-index ()
  "Generate an alist for imenu from a LaTeX buffer."
  (let ((section-regexp
	 (concat "\\\\" (regexp-opt (mapcar 'car latex-section-alist) t)
		 "\\*?[ \t]*{"))
	(metasection-regexp
	 (concat "\\\\" (regexp-opt latex-metasection-list t)))
	i0 menu case-fold-search)
    (save-excursion
      ;; Find the top-most level in this file but don't allow it to be
      ;; any deeper than "section" (which is top-level in an article).
      (goto-char (point-min))
      (if (search-forward-regexp "\\\\part\\*?[ \t]*{" nil t)
	  (setq i0 0)
	(if (search-forward-regexp "\\\\chapter\\*?[ \t]*{" nil t)
	    (setq i0 1)
	  (setq i0 2)))

      ;; Look for chapters and sections.
      (goto-char (point-min))
      (while (search-forward-regexp section-regexp nil t)
	(let ((start (match-beginning 0))
	      (here (point))
	      (i (cdr (assoc (buffer-substring-no-properties
			      (match-beginning 1)
			      (match-end 1))
			     latex-section-alist))))
	  (backward-char 1)
	  (condition-case err
	      (progn
		;; Using sexps allows some use of matching {...} inside
		;; titles.
		(forward-sexp 1)
		(push (cons (concat (apply 'concat
					   (make-list
					    (max 0 (- i i0))
					    latex-imenu-indent-string))
				    (buffer-substring-no-properties
				     here (1- (point))))
			    start)
		      menu))
	    (error nil))))

      ;; Look for included material.
      (goto-char (point-min))
      (while (search-forward-regexp
	      "\\\\\\(include\\|input\\|verbatiminput\\|bibliography\\)\
\[ \t]*{\\([^}\n]+\\)}"
	      nil t)
	(push (cons (concat "<<" (buffer-substring-no-properties
				  (match-beginning 2)
				  (match-end 2))
			    (if (= (char-after (match-beginning 1)) ?b)
				".bbl"
			      ".tex"))
		    (match-beginning 0))
	      menu))

      ;; Look for \frontmatter, \mainmatter, \backmatter, and \appendix.
      (goto-char (point-min))
      (while (search-forward-regexp metasection-regexp nil t)
	(push (cons "--" (match-beginning 0)) menu))

      ;; Sort in increasing buffer position order.
      (sort menu (function (lambda (a b) (< (cdr a) (cdr b))))))))

;;;;
;;;; Outline support
;;;;

(defvar latex-outline-regexp
  (concat "\\\\"
	  (regexp-opt (append latex-metasection-list
			      (mapcar 'car latex-section-alist)) t)))

(defun latex-outline-level ()
  (if (looking-at latex-outline-regexp)
      (1+ (or (cdr (assoc (match-string 1) latex-section-alist)) -1))
    1000))

;;;;
;;;; Font-Lock support
;;;;

;(defvar tex-font-lock-keywords
;  ;; Regexps updated with help from Ulrik Dickow <dickow@nbi.dk>.
;  '(("\\\\\\(begin\\|end\\|newcommand\\){\\([a-zA-Z0-9\\*]+\\)}"
;     2 font-lock-function-name-face)
;    ("\\\\\\(cite\\|label\\|pageref\\|ref\\){\\([^} \t\n]+\\)}"
;     2 font-lock-constant-face)
;    ;; It seems a bit dubious to use `bold' and `italic' faces since we might
;    ;; not be able to display those fonts.
;    ("{\\\\bf\\([^}]+\\)}" 1 'bold keep)
;    ("{\\\\\\(em\\|it\\|sl\\)\\([^}]+\\)}" 2 'italic keep)
;    ("\\\\\\([a-zA-Z@]+\\|.\\)" . font-lock-keyword-face)
;    ("^[ \t\n]*\\\\def[\\\\@]\\(\\w+\\)" 1 font-lock-function-name-face keep))
;  ;; Rewritten and extended for LaTeX2e by Ulrik Dickow <dickow@nbi.dk>.
;  '(("\\\\\\(begin\\|end\\|newcommand\\){\\([a-zA-Z0-9\\*]+\\)}"
;     2 font-lock-function-name-face)
;    ("\\\\\\(cite\\|label\\|pageref\\|ref\\){\\([^} \t\n]+\\)}"
;     2 font-lock-constant-face)
;    ("^[ \t]*\\\\def\\\\\\(\\(\\w\\|@\\)+\\)" 1 font-lock-function-name-face)
;    "\\\\\\([a-zA-Z@]+\\|.\\)"
;    ;; It seems a bit dubious to use `bold' and `italic' faces since we might
;    ;; not be able to display those fonts.
;    ;; LaTeX2e: \emph{This is emphasized}.
;    ("\\\\emph{\\([^}]+\\)}" 1 'italic keep)
;    ;; LaTeX2e: \textbf{This is bold}, \textit{...}, \textsl{...}
;    ("\\\\text\\(\\(bf\\)\\|it\\|sl\\){\\([^}]+\\)}"
;     3 (if (match-beginning 2) 'bold 'italic) keep)
;    ;; Old-style bf/em/it/sl.  Stop at `\\' and un-escaped `&', for tables.
;    ("\\\\\\(\\(bf\\)\\|em\\|it\\|sl\\)\\>\\(\\([^}&\\]\\|\\\\[^\\]\\)+\\)"
;     3 (if (match-beginning 2) 'bold 'italic) keep))

;; Rewritten with the help of Alexandra Bac <abac@welcome.disi.unige.it>.
(defconst tex-font-lock-keywords-1
  (eval-when-compile
    (let* (;; Names of commands whose arg should be fontified as heading, etc.
	   (headings (regexp-opt
		      '("title"  "begin" "end" "chapter" "part"
			"section" "subsection" "subsubsection"
			"paragraph" "subparagraph" "subsubparagraph"
			"newcommand" "renewcommand" "providecommand"
			"newenvironment" "renewenvironment"
			"newtheorem" "renewtheorem")
		      t))
	   (variables (regexp-opt
		       '("newcounter" "newcounter*" "setcounter" "addtocounter"
			 "setlength" "addtolength" "settowidth")
		       t))
	   (includes (regexp-opt
		      '("input" "include" "includeonly" "bibliography"
			"epsfig" "psfig" "epsf" "nofiles" "usepackage"
			"documentstyle" "documentclass" "verbatiminput"
			"includegraphics" "includegraphics*"
			"url" "nolinkurl")
		      t))
	   ;; Miscellany.
	   (slash "\\\\")
	   (opt " *\\(\\[[^]]*\\] *\\)*")
	   ;; This would allow highlighting \newcommand\CMD but requires
	   ;; adapting subgroup numbers below.
	   ;; (arg "\\(?:{\\(\\(?:[^{}\\]+\\|\\\\.\\|{[^}]*}\\)+\\)\\|\\\\[a-z*]+\\)"))
	   (arg "{\\(\\(?:[^{}\\]+\\|\\\\.\\|{[^}]*}\\)+\\)"))
      (list
       ;; display $$ math $$
       ;; We only mark the match between $$ and $$ because the $$ delimiters
       ;; themselves have already been marked (along with $..$) by syntactic
       ;; fontification.  Also this is done at the very beginning so as to
       ;; interact with the other keywords in the same way as $...$ does.
       (list "\\$\\$\\([^$]+\\)\\$\\$" 1 'tex-math-face)
       ;; Heading args.
       (list (concat slash headings "\\*?" opt arg)
	     ;; If ARG ends up matching too much (if the {} don't match, e.g.)
	     ;; jit-lock will do funny things: when updating the buffer
	     ;; the re-highlighting is only done locally so it will just
	     ;; match the local line, but defer-contextually will
	     ;; match more lines at a time, so ARG will end up matching
	     ;; a lot more, which might suddenly include a comment
	     ;; so you get things highlighted bold when you type them
	     ;; but they get turned back to normal a little while later
	     ;; because "there's already a face there".
	     ;; Using `keep' works around this un-intuitive behavior as well
	     ;; as improves the behavior in the very rare case where you do
	     ;; have a comment in ARG.
	     3 'font-lock-function-name-face 'keep)
       (list (concat slash "\\(?:provide\\|\\(?:re\\)?new\\)command\\** *\\(\\\\[A-Za-z@]+\\)")
	     1 'font-lock-function-name-face 'keep)
       ;; Variable args.
       (list (concat slash variables " *" arg) 2 'font-lock-variable-name-face)
       ;; Include args.
       (list (concat slash includes opt arg) 3 'font-lock-builtin-face)
       ;; Definitions.  I think.
       '("^[ \t]*\\\\def *\\\\\\(\\(\\w\\|@\\)+\\)"
	 1 font-lock-function-name-face))))
  "Subdued expressions to highlight in TeX modes.")

(defun tex-font-lock-append-prop (prop)
  (unless (memq (get-text-property (match-end 1) 'face)
		'(font-lock-comment-face tex-verbatim))
    prop))

(defconst tex-font-lock-keywords-2
  (append tex-font-lock-keywords-1
   (eval-when-compile
     (let* (;;
	    ;; Names of commands whose arg should be fontified with fonts.
	    (bold (regexp-opt '("textbf" "textsc" "textup"
				"boldsymbol" "pmb") t))
	    (italic (regexp-opt '("textit" "textsl" "emph") t))
	    ;; FIXME: unimplemented yet.
	    ;; (type (regexp-opt '("texttt" "textmd" "textrm" "textsf") t))
	    ;;
	    ;; Names of commands whose arg should be fontified as a citation.
	    (citations (regexp-opt
			'("label" "ref" "pageref" "vref" "eqref"
			  "cite" "nocite" "index" "glossary" "bibitem"
                          ;; natbib's two variants of \cite:
                          "citep" "citet"
			  ;; These are text, rather than citations.
			  ;; "caption" "footnote" "footnotemark" "footnotetext"
			  )
			t))
	    ;;
	    ;; Names of commands that should be fontified.
	    (specials-1 (regexp-opt '("\\" "\\*") t)) ;; "-"
	    (specials-2 (regexp-opt
			 '("linebreak" "nolinebreak" "pagebreak" "nopagebreak"
			   "newline" "newpage" "clearpage" "cleardoublepage"
			   "displaybreak" "allowdisplaybreaks"
			   "enlargethispage") t))
	    (general "\\([a-zA-Z@]+\\**\\|[^ \t\n]\\)")
	    ;;
	    ;; Miscellany.
	    (slash "\\\\")
	    (opt " *\\(\\[[^]]*\\] *\\)*")
	    (args "\\(\\(?:[^{}&\\]+\\|\\\\.\\|{[^}]*}\\)+\\)")
	    (arg "{\\(\\(?:[^{}\\]+\\|\\\\.\\|{[^}]*}\\)+\\)"))
       (list
	;;
	;; Citation args.
	(list (concat slash citations opt arg) 3 'font-lock-constant-face)
	;;
	;; Text between `` quotes ''.
	(cons (concat (regexp-opt `("``" "\"<" "\"`" "<<" "«") t)
		      "[^'\">{]+"	;a bit pessimistic
		      (regexp-opt `("''" "\">" "\"'" ">>" "»") t))
	      'font-lock-string-face)
	;;
	;; Command names, special and general.
	(cons (concat slash specials-1) 'font-lock-warning-face)
	(list (concat "\\(" slash specials-2 "\\)\\([^a-zA-Z@]\\|\\'\\)")
	      1 'font-lock-warning-face)
	(concat slash general)
	;;
	;; Font environments.  It seems a bit dubious to use `bold' etc. faces
	;; since we might not be able to display those fonts.
	(list (concat slash bold " *" arg) 2
	      '(tex-font-lock-append-prop 'bold) 'append)
	(list (concat slash italic " *" arg) 2
	      '(tex-font-lock-append-prop 'italic) 'append)
	;; (list (concat slash type arg) 2 '(quote bold-italic) 'append)
	;;
	;; Old-style bf/em/it/sl.  Stop at `\\' and un-escaped `&', for tables.
	(list (concat "\\\\\\(em\\|it\\|sl\\)\\>" args)
	      2 '(tex-font-lock-append-prop 'italic) 'append)
	;; This is separate from the previous one because of cases like
	;; {\em foo {\bf bar} bla} where both match.
 	(list (concat "\\\\\\(bf\\(series\\)?\\)\\>" args)
	      3 '(tex-font-lock-append-prop 'bold) 'append)))))
   "Gaudy expressions to highlight in TeX modes.")

(defun tex-font-lock-suscript (pos)
  (unless (or (memq (get-text-property pos 'face)
		    '(font-lock-constant-face font-lock-builtin-face
		      font-lock-comment-face tex-verbatim))
	      ;; Check for backslash quoting
	      (let ((odd nil)
		    (pos pos))
		(while (eq (char-before pos) ?\\)
		  (setq pos (1- pos) odd (not odd)))
		odd))
    (if (eq (char-after pos) ?_)
	`(face subscript display (raise ,(car tex-font-script-display)))
      `(face superscript display (raise ,(cadr tex-font-script-display))))))

(defun tex-font-lock-match-suscript (limit)
  "Match subscript and superscript patterns up to LIMIT."
  (when (and tex-fontify-script
	     (re-search-forward "[_^] *\\([^\n\\{}]\\|\
\\\\\\([a-zA-Z@]+\\|[^ \t\n]\\)\\|\\({\\)\\)" limit t))
    (when (match-end 3)
      (let ((beg (match-beginning 3))
	    (end (save-restriction
		   (narrow-to-region (point-min) limit)
		   (condition-case nil (scan-lists (point) 1 1) (error nil)))))
	(store-match-data (if end
			      (list (match-beginning 0) end beg end)
                            (list beg beg beg beg)))))
    t))

(defconst tex-font-lock-keywords-3
  (append tex-font-lock-keywords-2
	  '((tex-font-lock-match-suscript
	     (1 (tex-font-lock-suscript (match-beginning 0)) append))))
  "Experimental expressions to highlight in TeX modes.")

(defvar tex-font-lock-keywords tex-font-lock-keywords-1
  "Default expressions to highlight in TeX modes.")

(defvar tex-verbatim-environments
  '("verbatim" "verbatim*"))
(put 'tex-verbatim-environments 'safe-local-variable
     (lambda (x) (null (delq t (mapcar 'stringp x)))))

(eval-when-compile
  (defconst tex-syntax-propertize-rules
    (syntax-propertize-precompile-rules
    ("\\\\verb\\**\\([^a-z@*]\\)"
      (1 (prog1 "\""
           (tex-font-lock-verb
            (match-beginning 0) (char-after (match-beginning 1))))))))

  (defconst latex-syntax-propertize-rules
    (syntax-propertize-precompile-rules
     tex-syntax-propertize-rules
     ("\\\\\\(?:end\\|begin\\) *\\({[^\n{}]*}\\)"
      (1 (ignore
          (tex-env-mark (match-beginning 0)
                        (match-beginning 1) (match-end 1))))))))

(defun tex-env-mark (cmd start end)
  (when (= cmd (line-beginning-position))
    (let ((arg (buffer-substring-no-properties (1+ start) (1- end))))
      (when (member arg tex-verbatim-environments)
        (if (eq ?b (char-after (1+ cmd)))
            ;; \begin
            (put-text-property (line-end-position)
                               (line-beginning-position 2)
                               'syntax-table (string-to-syntax "< c"))
          ;; In the case of an empty verbatim env, the \n after the \begin is
          ;; the same as the \n before the \end.  Lucky for us, the "> c"
          ;; property associated to the \end will be placed afterwards, so it
          ;; will override the "< c".
          (put-text-property (1- cmd) cmd
                             'syntax-table (string-to-syntax "> c"))
          ;; The text between \end{verbatim} and \n is ignored, so we'll treat
          ;; it as a comment.
          (put-text-property end (min (1+ end) (line-end-position))
                             'syntax-table (string-to-syntax "<"))))))
  ;; Mark env args for possible electric pairing.
  (unless (get-char-property (1+ start) 'text-clones) ;Already paired-up.
    (put-text-property start end 'latex-env-pair t)))

(define-minor-mode latex-electric-env-pair-mode
  "Toggle Latex Electric Env Pair mode.
With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable it if ARG
is omitted or nil.

Latex Electric Env Pair mode is a buffer-local minor mode for use
with `latex-mode'.  When enabled, typing a \\begin or \\end tag
automatically inserts its partner."
  :lighter "/e"
  (if latex-electric-env-pair-mode
      (add-hook 'before-change-functions
                #'latex-env-before-change nil 'local)
    (remove-hook 'before-change-functions
                 #'latex-env-before-change 'local)))

(defun latex-env-before-change (start end)
  (when (get-text-property start 'latex-env-pair)
    (condition-case err
        (with-silent-modifications
          ;; Remove properties even if don't find a pair.
          (remove-text-properties
           (previous-single-property-change (1+ start) 'latex-env-pair)
           (next-single-property-change start 'latex-env-pair)
           '(latex-env-pair))
          (unless (or (get-char-property start 'text-clones)
                      (get-char-property (1+ start) 'text-clones)
                      (save-excursion
                        (goto-char start)
                        (not (re-search-backward
                              "\\\\\\(?:end\\|begi\\(n\\)\\) *{"
                              (line-beginning-position) t))))
            (let ((cmd-start (match-beginning 0))
                  (type (match-end 1))  ;nil for \end, else \begin.
                  (arg-start (1- (match-end 0))))
              (save-excursion
                (goto-char (match-end 0))
                (when (and (looking-at "[^\n{}]*}")
                           (> (match-end 0) end))
                  (let ((arg-end (match-end 0)))
                    (if (null type)     ;\end
                        (progn (goto-char arg-end)
                               (latex-forward-sexp -1) (forward-word 1))
                      (goto-char cmd-start)
                      (latex-forward-sexp 1)
                      (let (forward-sexp-function) (backward-sexp)))
                    (when (looking-at
                           (regexp-quote (buffer-substring arg-start arg-end)))
                      (text-clone-create arg-start arg-end))))))))
      (scan-error nil)
      (error (message "Error in latex-env-before-change: %s" err)))))

(defun tex-font-lock-unfontify-region (beg end)
  (font-lock-default-unfontify-region beg end)
  (while (< beg end)
    (let ((next (next-single-property-change beg 'display nil end))
	  (prop (get-text-property beg 'display)))
      (if (and (eq (car-safe prop) 'raise)
	       (member (car-safe (cdr prop)) tex-font-script-display)
	       (null (cddr prop)))
	  (put-text-property beg next 'display nil))
      (setq beg next))))

(defcustom tex-suscript-height-ratio 0.8
  "Ratio of subscript/superscript height to that of the preceding text.
In nested subscript/superscript, this factor is applied repeatedly,
subject to the limit set by `tex-suscript-height-minimum'."
  :type 'float
  :group 'tex
  :version "23.1")

(defcustom tex-suscript-height-minimum 0.0
  "Integer or float limiting the minimum size of subscript/superscript text.
An integer is an absolute height in units of 1/10 point, a float
is a height relative to that of the default font.  Zero means no minimum."
  :type '(choice (integer :tag "Integer height in 1/10 point units")
		 (float :tag "Fraction of default font height"))
  :group 'tex
  :version "23.1")

(defun tex-suscript-height (height)
  "Return the integer height of subscript/superscript font in 1/10 points.
Not smaller than the value set by `tex-suscript-height-minimum'."
  (ceiling (max (if (integerp tex-suscript-height-minimum)
		    tex-suscript-height-minimum
		  ;; For bootstrapping.
		  (condition-case nil
		      (* tex-suscript-height-minimum
			 (face-attribute 'default :height))
		    (error 0)))
		;; NB assumes height is integer.
		(* height tex-suscript-height-ratio))))

(defface superscript
  '((t :height tex-suscript-height)) ;; :raise 0.2
  "Face used for superscripts."
  :group 'tex)
(defface subscript
  '((t :height tex-suscript-height)) ;; :raise -0.2
  "Face used for subscripts."
  :group 'tex)

(defface tex-math
  '((t :inherit font-lock-string-face))
  "Face used to highlight TeX math expressions."
  :group 'tex)
(define-obsolete-face-alias 'tex-math-face 'tex-math "22.1")
(defvar tex-math-face 'tex-math)

(defface tex-verbatim
  ;; '((t :inherit font-lock-string-face))
  '((t :family "courier"))
  "Face used to highlight TeX verbatim environments."
  :group 'tex)
(define-obsolete-face-alias 'tex-verbatim-face 'tex-verbatim "22.1")
(defvar tex-verbatim-face 'tex-verbatim)

(defun tex-font-lock-verb (start delim)
  "Place syntax table properties on the \verb construct.
START is the position of the \\ and DELIM is the delimiter char."
  ;; Do nothing if the \verb construct is itself inside a comment or
  ;; verbatim env.
  (unless (nth 8 (save-excursion (syntax-ppss start)))
    ;; Let's find the end and mark it.
    (let ((afterdelim (point)))
      (skip-chars-forward (string ?^ delim) (line-end-position))
      (if (eolp)
          ;; "LaTeX Error: \verb ended by end of line."
          ;; Remove the syntax-table property we've just put on the
          ;; start-delimiter, so it doesn't spill over subsequent lines.
          (put-text-property (1- afterdelim) afterdelim
                             'syntax-table nil)
        (when (eq (char-syntax (preceding-char)) ?/)
          (put-text-property (1- (point)) (point)
                             'syntax-table (string-to-syntax ".")))
        (put-text-property (point) (1+ (point))
                           'syntax-table (string-to-syntax "\""))))))

;; Use string syntax but math face for $...$.
(defun tex-font-lock-syntactic-face-function (state)
  (let ((char (nth 3 state)))
    (cond
     ((not char)
      (if (eq 2 (nth 7 state)) tex-verbatim-face font-lock-comment-face))
     ((eq char ?$) tex-math-face)
     ;; A \verb element.
     (t tex-verbatim-face))))


(defun tex-define-common-keys (keymap)
  "Define the keys that we want defined both in TeX mode and in the TeX shell."
  (define-key keymap "\C-c\C-k" 'tex-kill-job)
  (define-key keymap "\C-c\C-l" 'tex-recenter-output-buffer)
  (define-key keymap "\C-c\C-q" 'tex-show-print-queue)
  (define-key keymap "\C-c\C-p" 'tex-print)
  (define-key keymap "\C-c\C-v" 'tex-view)

  (define-key keymap [menu-bar tex] (cons "TeX" (make-sparse-keymap "TeX")))

  (define-key keymap [menu-bar tex tex-kill-job]
    '(menu-item "Tex Kill" tex-kill-job :enable (tex-shell-running)))
  (define-key keymap [menu-bar tex tex-recenter-output-buffer]
    '(menu-item "Tex Recenter" tex-recenter-output-buffer
                :enable (get-buffer "*tex-shell*")))
  (define-key keymap [menu-bar tex tex-show-print-queue]
    '("Show Print Queue" . tex-show-print-queue))
  (define-key keymap [menu-bar tex tex-alt-print]
    '(menu-item "Tex Print (alt printer)" tex-alt-print
                :enable (stringp tex-print-file)))
  (define-key keymap [menu-bar tex tex-print]
    '(menu-item "Tex Print" tex-print :enable (stringp tex-print-file)))
  (define-key keymap [menu-bar tex tex-view]
    '(menu-item "Tex View" tex-view :enable (stringp tex-print-file))))

(defvar tex-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (tex-define-common-keys map)
    (define-key map "\"" 'tex-insert-quote)
    (define-key map "(" 'skeleton-pair-insert-maybe)
    (define-key map "{" 'skeleton-pair-insert-maybe)
    (define-key map "[" 'skeleton-pair-insert-maybe)
    (define-key map "$" 'skeleton-pair-insert-maybe)
    (define-key map "\n" 'tex-terminate-paragraph)
    (define-key map "\M-\r" 'latex-insert-item)
    (define-key map "\C-c}" 'up-list)
    (define-key map "\C-c{" 'tex-insert-braces)
    (define-key map "\C-c\C-r" 'tex-region)
    (define-key map "\C-c\C-b" 'tex-buffer)
    (define-key map "\C-c\C-f" 'tex-file)
    (define-key map "\C-c\C-c" 'tex-compile)
    (define-key map "\C-c\C-i" 'tex-bibtex-file)
    (define-key map "\C-c\C-o" 'latex-insert-block)

    ;; Redundant keybindings, for consistency with SGML mode.
    (define-key map "\C-c\C-t" 'latex-insert-block)
    (define-key map "\C-c]" 'latex-close-block)
    (define-key map "\C-c/" 'latex-close-block)

    (define-key map "\C-c\C-e" 'latex-close-block)
    (define-key map "\C-c\C-u" 'tex-goto-last-unclosed-latex-block)
    (define-key map "\C-c\C-m" 'tex-feed-input)
    (define-key map [(control return)] 'tex-feed-input)
    (define-key map [menu-bar tex tex-bibtex-file]
      '("BibTeX File" . tex-bibtex-file))
    (define-key map [menu-bar tex tex-validate-region]
      '(menu-item "Validate Region" tex-validate-region :enable mark-active))
    (define-key map [menu-bar tex tex-validate-buffer]
      '("Validate Buffer" . tex-validate-buffer))
    (define-key map [menu-bar tex tex-region]
      '(menu-item "TeX Region" tex-region :enable mark-active))
    (define-key map [menu-bar tex tex-buffer]
      '("TeX Buffer" . tex-buffer))
    (define-key map [menu-bar tex tex-file] '("TeX File" . tex-file))
    map)
 "Keymap shared by TeX modes.")

(defvar latex-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tex-mode-map)
    (define-key map "\C-c\C-s" 'latex-split-block)
    map)
  "Keymap for `latex-mode'.  See also `tex-mode-map'.")

(defvar plain-tex-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tex-mode-map)
    map)
  "Keymap for `plain-tex-mode'.  See also `tex-mode-map'.")

(defvar tex-shell-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m shell-mode-map)
    (tex-define-common-keys m)
    m)
  "Keymap for the TeX shell.
Inherits `shell-mode-map' with a few additions.")

(defvar tex-face-alist
  '((bold . "{\\bf ")
    (italic . "{\\it ")
    (bold-italic . "{\\bi ")		; hypothetical
    (underline . "\\underline{")
    (default . "{\\rm "))
  "Alist of face and TeX font name for facemenu.")

(defvar tex-latex-face-alist
  `((italic . "{\\em ")
    ,@tex-face-alist)
  "Alist of face and LaTeX font name for facemenu.")

(defun tex-facemenu-add-face-function (face end)
  (or (cdr (assq face tex-face-alist))
      (or (and (consp face)
	       (consp (car face))
	       (null  (cdr face))
	       (eq major-mode 'latex-mode)
	       ;; This actually requires the `color' LaTeX package.
	       (cond ((eq (caar face) :foreground)
		      (format "{\\color{%s} " (cadr (car face))))
		     ((eq (caar face) :background)
		      (format "\\colorbox{%s}{" (cadr (car face))))))
	  (error "Face %s not configured for %s mode" face mode-name))))

;; This would be a lot simpler if we just used a regexp search,
;; but then it would be too slow.
(defun tex-guess-mode ()
  (let ((mode tex-default-mode) slash comment)
    (save-excursion
      (goto-char (point-min))
      (while (and (setq slash (search-forward "\\" nil t))
		  (setq comment (let ((search-end (point)))
				  (save-excursion
				    (beginning-of-line)
				    (search-forward "%" search-end t))))))
      (when (and slash (not comment))
	(setq mode
	      (if (looking-at
		   (eval-when-compile
		     (concat
		      (regexp-opt '("documentstyle" "documentclass"
				    "begin" "subsection" "section"
				    "part" "chapter" "newcommand"
				    "renewcommand" "RequirePackage") 'words)
		      "\\|NeedsTeXFormat{LaTeX")))
		  (if (and (looking-at
			    "document\\(style\\|class\\)\\(\\[.*\\]\\)?{slides}")
			   ;; SliTeX is almost never used any more nowadays.
			   (tex-executable-exists-p slitex-run-command))
		      'slitex-mode
		    'latex-mode)
		'plain-tex-mode))))
    (funcall mode)))

;; `tex-mode' plays two roles: it's the parent of several sub-modes
;; but it's also the function that chooses between those submodes.
;; To tell the difference between those two cases where the function
;; might be called, we check `delay-mode-hooks'.
(define-derived-mode tex-mode text-mode "generic-TeX"
  (tex-common-initialization))
;; We now move the function and define it again.  This gives a warning
;; in the byte-compiler :-( but it's difficult to avoid because
;; `define-derived-mode' will necessarily define the function once
;; and we need to define it a second time for `autoload' to get the
;; proper docstring.
(defalias 'tex-mode-internal (symbol-function 'tex-mode))

;; Suppress the byte-compiler warning about multiple definitions.
;; This is a) ugly, and b) cheating, but this was the last
;; remaining warning from byte-compiling all of Emacs...
(eval-when-compile
  (setq byte-compile-function-environment
	(delq (assq 'tex-mode byte-compile-function-environment)
	      byte-compile-function-environment)))

;;;###autoload
(defun tex-mode ()
  "Major mode for editing files of input for TeX, LaTeX, or SliTeX.
Tries to determine (by looking at the beginning of the file) whether
this file is for plain TeX, LaTeX, or SliTeX and calls `plain-tex-mode',
`latex-mode', or `slitex-mode', respectively.  If it cannot be determined,
such as if there are no commands in the file, the value of `tex-default-mode'
says which mode to use."
  (interactive)
  (if delay-mode-hooks
      ;; We're called from one of the children already.
      (tex-mode-internal)
    (tex-guess-mode)))

;; The following three autoloaded aliases appear to conflict with
;; AUCTeX.  However, even though AUCTeX uses the mixed case variants
;; for all mode relevant variables and hooks, the invocation function
;; and setting of `major-mode' themselves need to be lowercase for
;; AUCTeX to provide a fully functional user-level replacement.  So
;; these aliases should remain as they are, in particular since AUCTeX
;; users are likely to use them.

;;;###autoload
(defalias 'TeX-mode 'tex-mode)
;;;###autoload
(defalias 'plain-TeX-mode 'plain-tex-mode)
;;;###autoload
(defalias 'LaTeX-mode 'latex-mode)

;;;###autoload
(define-derived-mode plain-tex-mode tex-mode "TeX"
  "Major mode for editing files of input for plain TeX.
Makes $ and } display the characters they match.
Makes \" insert `` when it seems to be the beginning of a quotation,
and '' when it appears to be the end; it inserts \" only after a \\.

Use \\[tex-region] to run TeX on the current region, plus a \"header\"
copied from the top of the file (containing macro definitions, etc.),
running TeX under a special subshell.  \\[tex-buffer] does the whole buffer.
\\[tex-file] saves the buffer and then processes the file.
\\[tex-print] prints the .dvi file made by any of these.
\\[tex-view] previews the .dvi file made by any of these.
\\[tex-bibtex-file] runs bibtex on the file of the current buffer.

Use \\[tex-validate-buffer] to check buffer for paragraphs containing
mismatched $'s or braces.

Special commands:
\\{plain-tex-mode-map}

Mode variables:
tex-run-command
	Command string used by \\[tex-region] or \\[tex-buffer].
tex-directory
	Directory in which to create temporary files for TeX jobs
	run by \\[tex-region] or \\[tex-buffer].
tex-dvi-print-command
	Command string used by \\[tex-print] to print a .dvi file.
tex-alt-dvi-print-command
	Alternative command string used by \\[tex-print] (when given a prefix
	argument) to print a .dvi file.
tex-dvi-view-command
	Command string used by \\[tex-view] to preview a .dvi file.
tex-show-queue-command
	Command string used by \\[tex-show-print-queue] to show the print
	queue that \\[tex-print] put your job on.

Entering Plain-tex mode runs the hook `text-mode-hook', then the hook
`tex-mode-hook', and finally the hook `plain-tex-mode-hook'.  When the
special subshell is initiated, the hook `tex-shell-hook' is run."
  (set (make-local-variable 'tex-command) tex-run-command)
  (set (make-local-variable 'tex-start-of-header) "%\\*\\*start of header")
  (set (make-local-variable 'tex-end-of-header) "%\\*\\*end of header")
  (set (make-local-variable 'tex-trailer) "\\bye\n"))

;;;###autoload
(define-derived-mode latex-mode tex-mode "LaTeX"
  "Major mode for editing files of input for LaTeX.
Makes $ and } display the characters they match.
Makes \" insert `` when it seems to be the beginning of a quotation,
and '' when it appears to be the end; it inserts \" only after a \\.

Use \\[tex-region] to run LaTeX on the current region, plus the preamble
copied from the top of the file (containing \\documentstyle, etc.),
running LaTeX under a special subshell.  \\[tex-buffer] does the whole buffer.
\\[tex-file] saves the buffer and then processes the file.
\\[tex-print] prints the .dvi file made by any of these.
\\[tex-view] previews the .dvi file made by any of these.
\\[tex-bibtex-file] runs bibtex on the file of the current buffer.

Use \\[tex-validate-buffer] to check buffer for paragraphs containing
mismatched $'s or braces.

Special commands:
\\{latex-mode-map}

Mode variables:
latex-run-command
	Command string used by \\[tex-region] or \\[tex-buffer].
tex-directory
	Directory in which to create temporary files for LaTeX jobs
	run by \\[tex-region] or \\[tex-buffer].
tex-dvi-print-command
	Command string used by \\[tex-print] to print a .dvi file.
tex-alt-dvi-print-command
	Alternative command string used by \\[tex-print] (when given a prefix
	argument) to print a .dvi file.
tex-dvi-view-command
	Command string used by \\[tex-view] to preview a .dvi file.
tex-show-queue-command
	Command string used by \\[tex-show-print-queue] to show the print
	queue that \\[tex-print] put your job on.

Entering Latex mode runs the hook `text-mode-hook', then
`tex-mode-hook', and finally `latex-mode-hook'.  When the special
subshell is initiated, `tex-shell-hook' is run."
  (set (make-local-variable 'tex-command) latex-run-command)
  (set (make-local-variable 'tex-start-of-header)
       "\\\\document\\(style\\|class\\)")
  (set (make-local-variable 'tex-end-of-header) "\\\\begin\\s-*{document}")
  (set (make-local-variable 'tex-trailer) "\\end{document}\n")
  ;; A line containing just $$ is treated as a paragraph separator.
  ;; A line starting with $$ starts a paragraph,
  ;; but does not separate paragraphs if it has more stuff on it.
  (setq paragraph-start
	(concat "[ \t]*\\(\\$\\$\\|"
		"\\\\[][]\\|"
		"\\\\" (regexp-opt (append
				    (mapcar 'car latex-section-alist)
				    '("begin" "label" "end"
				      "item" "bibitem" "newline" "noindent"
				      "newpage" "footnote" "marginpar"
				      "parbox" "caption")) t)
		"\\>\\|\\\\[a-z]*" (regexp-opt '("space" "skip" "page") t)
		"\\>\\)"))
  (setq paragraph-separate
	(concat "[\f%]\\|[ \t]*\\($\\|"
		"\\\\[][]\\|"
		"\\\\" (regexp-opt (append
				    (mapcar 'car latex-section-alist)
				    '("begin" "label" "end" )) t)
		"\\>\\|\\\\\\(" (regexp-opt '("item" "bibitem" "newline"
					      "noindent" "newpage" "footnote"
					      "marginpar" "parbox" "caption"))
		"\\|\\$\\$\\|[a-z]*\\(space\\|skip\\|page[a-z]*\\)"
		"\\>\\)[ \t]*\\($\\|%\\)\\)"))
  (set (make-local-variable 'imenu-create-index-function)
       'latex-imenu-create-index)
  (set (make-local-variable 'tex-face-alist) tex-latex-face-alist)
  (add-hook 'fill-nobreak-predicate 'latex-fill-nobreak-predicate nil t)
  (set (make-local-variable 'indent-line-function) 'latex-indent)
  (set (make-local-variable 'fill-indent-according-to-mode) t)
  (add-hook 'completion-at-point-functions
            'latex-complete-data nil 'local)
  (set (make-local-variable 'outline-regexp) latex-outline-regexp)
  (set (make-local-variable 'outline-level) 'latex-outline-level)
  (set (make-local-variable 'forward-sexp-function) 'latex-forward-sexp)
  (set (make-local-variable 'skeleton-end-hook) nil))

;;;###autoload
(define-derived-mode slitex-mode latex-mode "SliTeX"
  "Major mode for editing files of input for SliTeX.
Makes $ and } display the characters they match.
Makes \" insert `` when it seems to be the beginning of a quotation,
and '' when it appears to be the end; it inserts \" only after a \\.

Use \\[tex-region] to run SliTeX on the current region, plus the preamble
copied from the top of the file (containing \\documentstyle, etc.),
running SliTeX under a special subshell.  \\[tex-buffer] does the whole buffer.
\\[tex-file] saves the buffer and then processes the file.
\\[tex-print] prints the .dvi file made by any of these.
\\[tex-view] previews the .dvi file made by any of these.
\\[tex-bibtex-file] runs bibtex on the file of the current buffer.

Use \\[tex-validate-buffer] to check buffer for paragraphs containing
mismatched $'s or braces.

Special commands:
\\{slitex-mode-map}

Mode variables:
slitex-run-command
	Command string used by \\[tex-region] or \\[tex-buffer].
tex-directory
	Directory in which to create temporary files for SliTeX jobs
	run by \\[tex-region] or \\[tex-buffer].
tex-dvi-print-command
	Command string used by \\[tex-print] to print a .dvi file.
tex-alt-dvi-print-command
	Alternative command string used by \\[tex-print] (when given a prefix
	argument) to print a .dvi file.
tex-dvi-view-command
	Command string used by \\[tex-view] to preview a .dvi file.
tex-show-queue-command
	Command string used by \\[tex-show-print-queue] to show the print
	queue that \\[tex-print] put your job on.

Entering SliTeX mode runs the hook `text-mode-hook', then the hook
`tex-mode-hook', then the hook `latex-mode-hook', and finally the hook
`slitex-mode-hook'.  When the special subshell is initiated, the hook
`tex-shell-hook' is run."
  (setq tex-command slitex-run-command)
  (setq tex-start-of-header "\\\\documentstyle{slides}\\|\\\\documentclass{slides}"))

(defun tex-common-initialization ()
  ;; Regexp isearch should accept newline and formfeed as whitespace.
  (set (make-local-variable 'search-whitespace-regexp) "[ \t\r\n\f]+")
  ;; A line containing just $$ is treated as a paragraph separator.
  (set (make-local-variable 'paragraph-start)
       "[ \t]*$\\|[\f\\\\%]\\|[ \t]*\\$\\$")
  ;; A line starting with $$ starts a paragraph,
  ;; but does not separate paragraphs if it has more stuff on it.
  (set (make-local-variable 'paragraph-separate)
	"[ \t]*$\\|[\f\\\\%]\\|[ \t]*\\$\\$[ \t]*$")
  (set (make-local-variable 'comment-start) "%")
  (set (make-local-variable 'comment-add) 1)
  (set (make-local-variable 'comment-start-skip)
       "\\(\\(^\\|[^\\\n]\\)\\(\\\\\\\\\\)*\\)\\(%+ *\\)")
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'compare-windows-whitespace)
       'tex-categorize-whitespace)
  (set (make-local-variable 'facemenu-add-face-function)
       'tex-facemenu-add-face-function)
  (set (make-local-variable 'facemenu-end-add-face) "}")
  (set (make-local-variable 'facemenu-remove-face-function) t)
  (set (make-local-variable 'font-lock-defaults)
       '((tex-font-lock-keywords tex-font-lock-keywords-1
	  tex-font-lock-keywords-2 tex-font-lock-keywords-3)
	 nil nil ((?$ . "\"")) nil
	 ;; Who ever uses that anyway ???
	 (font-lock-mark-block-function . mark-paragraph)
	 (font-lock-syntactic-face-function
	  . tex-font-lock-syntactic-face-function)
	 (font-lock-unfontify-region-function
	  . tex-font-lock-unfontify-region)))
  (set (make-local-variable 'syntax-propertize-function)
       (syntax-propertize-rules latex-syntax-propertize-rules))
  ;; TABs in verbatim environments don't do what you think.
  (set (make-local-variable 'indent-tabs-mode) nil)
  ;; Other vars that should be buffer-local.
  (make-local-variable 'tex-command)
  (make-local-variable 'tex-start-of-header)
  (make-local-variable 'tex-end-of-header)
  (make-local-variable 'tex-trailer))

(defun tex-categorize-whitespace (backward-limit)
  ;; compare-windows-whitespace is set to this.
  ;; This is basically a finite-state machine.
  ;; Returns a symbol telling how TeX would treat
  ;; the whitespace we are looking at: null, space, or par.
  (let ((category 'null)
	(not-finished t))
    (skip-chars-backward " \t\n\f" backward-limit)
    (while not-finished
      (cond ((looking-at "[ \t]+")
	     (goto-char (match-end 0))
	     (if (eq category 'null)
		 (setq category 'space)))
	    ((looking-at "\n")
	     (cond ((eq category 'newline)
		    (setq category 'par)
		    (setq not-finished nil))
		   (t
		    (setq category 'newline) ;a strictly internal state
		    (goto-char (match-end 0)))))
	    ((looking-at "\f+")
	     (setq category 'par)
	     (setq not-finished nil))
	    (t
	     (setq not-finished nil))))
    (skip-chars-forward " \t\n\f")
    (if (eq category 'newline)
	'space				;TeX doesn't distinguish
      category)))

(defun tex-insert-quote (arg)
  "Insert the appropriate quote marks for TeX.
Inserts the value of `tex-open-quote' (normally ``) or `tex-close-quote'
\(normally '') depending on the context.  With prefix argument, always
inserts \" characters."
  (interactive "*P")
  (if (or arg (memq (char-syntax (preceding-char)) '(?/ ?\\))
	  (eq (get-text-property (point) 'face) 'tex-verbatim)
	  (save-excursion
	    (backward-char (length tex-open-quote))
	    (when (or (looking-at (regexp-quote tex-open-quote))
		      (looking-at (regexp-quote tex-close-quote)))
	      (delete-char (length tex-open-quote))
	      t)))
      (self-insert-command (prefix-numeric-value arg))
    (insert (if (memq (char-syntax (preceding-char)) '(?\( ?> ?\s))
		tex-open-quote tex-close-quote))))

(defun tex-validate-buffer ()
  "Check current buffer for paragraphs containing mismatched braces or $s.
Their positions are recorded in the buffer `*Occur*'.
To find a particular invalidity from `*Occur*', switch to that buffer
and type C-c C-c or click with mouse-2
on the line for the invalidity you want to see."
  (interactive)
  (let ((buffer (current-buffer))
	(prevpos (point-min))
	(linenum nil)
	(num-matches 0))
    (with-output-to-temp-buffer "*Occur*"
      (princ "Mismatches:\n")
      (with-current-buffer standard-output
	(occur-mode)
	;; This won't actually work...Really, this whole thing should
	;; be rewritten instead of being a hack on top of occur.
	(setq occur-revert-arguments (list nil 0 (list buffer))))
      (save-excursion
	(goto-char (point-max))
	;; Do a little shimmy to place point at the end of the last
	;; "real" paragraph. Need to avoid validating across an \end,
	;; because that blows up latex-forward-sexp.
	(backward-paragraph)
	(forward-paragraph)
	(while (not (bobp))
	    ;; Scan the previous paragraph for invalidities.
	  (backward-paragraph)
	  (save-excursion
	    (or (tex-validate-region (point) (save-excursion
					       (forward-paragraph)
					       (point)))
		(let ((end (line-beginning-position 2))
		       start tem)
		  (beginning-of-line)
		  (setq start (point))
		  ;; Keep track of line number as we scan,
		  ;; in a cumulative fashion.
		  (if linenum
		      (setq linenum (- linenum
				       (count-lines prevpos (point))))
		    (setq linenum (1+ (count-lines 1 start))))
		  (setq prevpos (point))
		  ;; Mention this mismatch in *Occur*.
		  ;; Since we scan from end of buffer to beginning,
		  ;; add each mismatch at the beginning of *Occur*.
		  (save-excursion
		    (setq tem (point-marker))
		    (set-buffer standard-output)
		    (goto-char (point-min))
		    ;; Skip "Mismatches:" header line.
		    (forward-line 1)
		    (setq num-matches (1+ num-matches))
		    (insert-buffer-substring buffer start end)
		    (let (text-beg (text-end (point-marker)))
		      (forward-char (- start end))
		      (setq text-beg (point-marker))
		      (insert (format "%3d: " linenum))
		      (add-text-properties
		       text-beg (- text-end 1)
		       '(mouse-face highlight
				    help-echo
				    "mouse-2: go to this invalidity"))
		      (put-text-property text-beg (- text-end 1)
					 'occur-target tem))))))))
      (with-current-buffer standard-output
	(let ((no-matches (zerop num-matches)))
	  (if no-matches
	      (insert "None!\n"))
	  (if (called-interactively-p 'interactive)
	      (message (cond (no-matches "No mismatches found")
			     ((= num-matches 1) "1 mismatch found")
			     (t "%d mismatches found"))
		       num-matches)))))))

(defun tex-validate-region (start end)
  "Check for mismatched braces or $'s in region.
Returns t if no mismatches.  Returns nil and moves point to suspect
area if a mismatch is found."
  (interactive "r")
  (let ((failure-point nil) (max-possible-sexps (- end start)))
    (save-excursion
      (condition-case ()
	  (save-restriction
	    (narrow-to-region start end)
	    ;; First check that the open and close parens balance in numbers.
	    (goto-char start)
	    (while (and (not (eobp))
			(<= 0 (setq max-possible-sexps
				    (1- max-possible-sexps))))
	      (forward-sexp 1))
	    ;; Now check that like matches like.
	    (goto-char start)
	    (while (re-search-forward "\\s(" nil t)
	      (save-excursion
		(let ((pos (match-beginning 0)))
		  (goto-char pos)
		  (skip-chars-backward "\\\\") ; escaped parens
		  (forward-sexp 1)
		  (or (eq (preceding-char) (cdr (syntax-after pos)))
		      (eq (char-after pos) (cdr (syntax-after (1- (point)))))
		      (error "Mismatched parentheses"))))))
	(error
	 (skip-syntax-forward " .>")
	 (setq failure-point (point)))))
    (if failure-point (goto-char failure-point))
    (not failure-point)))

(defun tex-terminate-paragraph (inhibit-validation)
  "Insert two newlines, breaking a paragraph for TeX.
Check for mismatched braces or $s in paragraph being terminated.
A prefix arg inhibits the checking."
  (interactive "*P")
  (or inhibit-validation
      (save-excursion
	;; For the purposes of this, a "paragraph" is a block of text
	;; wherein all the brackets etc are expected to be balanced.  It
	;; may start after a blank line (ie a "proper" paragraph), or
	;; a begin{} or end{} block, etc.
	(tex-validate-region
	 (save-excursion
	   (backward-paragraph)
	   (point))
	 (point)))
      (message "Paragraph being closed appears to contain a mismatch"))
  (insert "\n\n"))

(define-skeleton tex-insert-braces
  "Make a pair of braces and be poised to type inside of them."
  nil
  ?\{ _ ?})

;; This function is used as the value of fill-nobreak-predicate
;; in LaTeX mode.  Its job is to prevent line-breaking inside
;; of a \verb construct.
(defun latex-fill-nobreak-predicate ()
  (save-excursion
    (skip-chars-backward " ")
    ;; Don't break after \ since `\ ' has special meaning.
    (or (and (not (bobp)) (memq (char-syntax (char-before)) '(?\\ ?/)))
	(let ((opoint (point))
	      inside)
	  (beginning-of-line)
	  (while (re-search-forward "\\\\verb\\(.\\)" opoint t)
	    (unless (re-search-forward (regexp-quote (match-string 1)) opoint t)
	      (setq inside t)))
	  inside))))

(defvar latex-block-default "enumerate")

(defvar latex-block-args-alist
  '(("array" nil ?\{ (skeleton-read "Format: ") ?\})
    ("tabular" nil ?\{ (skeleton-read "Format: ") ?\})
    ("minipage" nil ?\{ (skeleton-read "Size: ") ?\})
    ("picture" nil ?\( (skeleton-read "SizeX,SizeY: ") ?\))
    ;; FIXME: This is right for Prosper, but not for seminar.
    ;; ("slide" nil ?\{ (skeleton-read "Title: ") ?\})
    )
  "Skeleton element to use for arguments to particular environments.
Every element of the list has the form (NAME . SKEL-ELEM) where NAME is
the name of the environment and SKEL-ELEM is an element to use in
a skeleton (see `skeleton-insert').")

(defvar latex-block-body-alist
  '(("enumerate" nil '(latex-insert-item) > _)
    ("itemize" nil '(latex-insert-item) > _)
    ("table" nil "\\caption{" > (skeleton-read "Caption: ") "}" > \n
     '(if (and (boundp 'reftex-mode) reftex-mode) (reftex-label "table"))
     \n _)
    ("figure" nil  > _ \n "\\caption{" > (skeleton-read "Caption: ") "}" > \n
     '(if (and (boundp 'reftex-mode) reftex-mode) (reftex-label "table"))))
  "Skeleton element to use for the body of particular environments.
Every element of the list has the form (NAME . SKEL-ELEM) where NAME is
the name of the environment and SKEL-ELEM is an element to use in
a skeleton (see `skeleton-insert').")

;; Like tex-insert-braces, but for LaTeX.
(defalias 'tex-latex-block 'latex-insert-block)
(define-skeleton latex-insert-block
  "Create a matching pair of lines \\begin{NAME} and \\end{NAME} at point.
Puts point on a blank line between them."
  (let ((choice (completing-read (format "LaTeX block name [%s]: "
					 latex-block-default)
				 (append latex-block-names
					 latex-standard-block-names)
				 nil nil nil nil latex-block-default)))
    (setq latex-block-default choice)
    (unless (or (member choice latex-standard-block-names)
		(member choice latex-block-names))
      ;; Remember new block names for later completion.
      (push choice latex-block-names))
    choice)
  \n "\\begin{" str "}"
  (cdr (assoc str latex-block-args-alist))
  > \n (or (cdr (assoc str latex-block-body-alist)) '(nil > _))
  (unless (bolp) '\n)
  "\\end{" str "}" > \n)

(define-skeleton latex-insert-item
  "Insert a \item macro."
  nil
  \n "\\item " >)


;;;; LaTeX completion.

(defvar latex-complete-bibtex-cache nil)

(defun latex-string-prefix-p (str1 str2)
  (eq t (compare-strings str1 nil nil str2 0 (length str1))))

(defvar bibtex-reference-key)
(declare-function reftex-get-bibfile-list "reftex-cite.el" ())

(defun latex-complete-bibtex-keys ()
  (when (bound-and-true-p reftex-mode)
    (lambda (key pred action)
      (let ((re (concat "^[ \t]*@\\([a-zA-Z]+\\)[ \t\n]*\\([{(][ \t\n]*\\)"
                        (regexp-quote key)))
            (files (reftex-get-bibfile-list))
            keys)
        (if (and (eq (car latex-complete-bibtex-cache)
                     (reftex-get-bibfile-list))
                 (latex-string-prefix-p (nth 1 latex-complete-bibtex-cache)
                                        key))
            ;; Use the cache.
            (setq keys (nth 2 latex-complete-bibtex-cache))
          (dolist (file files)
            (with-current-buffer (find-file-noselect file)
              (goto-char (point-min))
              (while (re-search-forward re nil t)
                (goto-char (match-end 2))
                (when (and (not (member-ignore-case (match-string 1)
                                                    '("c" "comment" "string")))
                           (looking-at bibtex-reference-key))
                  (push (match-string-no-properties 0) keys)))))
          ;; Fill the cache.
          (set (make-local-variable 'latex-complete-bibtex-cache)
               (list files key keys)))
        (complete-with-action action keys key pred)))))

(defun latex-complete-envnames ()
  (append latex-block-names latex-standard-block-names))

(defun latex-complete-refkeys ()
  (when (boundp 'reftex-docstruct-symbol)
    (symbol-value reftex-docstruct-symbol)))

(defvar latex-complete-alist
  ;; TODO: Add \begin, \end, \ref, ...
  '(("\\`\\\\\\(short\\)?cite\\'" . latex-complete-bibtex-keys)
    ("\\`\\\\\\(begin\\|end\\)\\'" . latex-complete-envnames)
    ("\\`\\\\[vf]?ref\\'" . latex-complete-refkeys)))

(defun latex-complete-data ()
  "Get completion-data at point."
  (save-excursion
    (let ((pt (point)))
      (skip-chars-backward "^ {}\n\t\\\\")
      (case (char-before)
        ((nil ?\s ?\n ?\t ?\}) nil)
        (?\\
         ;; TODO: Complete commands.
         nil)
        (?\{
         ;; Complete args to commands.
         (let* ((cmd
                 (save-excursion
                   (forward-char -1)
                   (skip-chars-backward " \n")
                   (buffer-substring (point)
                                     (progn
                                       (skip-chars-backward "a-zA-Z@*")
                                       (let ((n (skip-chars-backward "\\\\")))
                                         (forward-char (* 2 (/ n 2))))
                                       (point)))))
                (start (point))
                (_ (progn (goto-char pt) (skip-chars-backward "^," start)))
                (comp-beg (point))
                (_ (progn (goto-char pt) (skip-chars-forward "^, {}\n\t\\\\")))
                (comp-end (point))
                (table
                 (funcall
                  (let ((f (lambda () t)))
                    (dolist (comp latex-complete-alist)
                      (if (string-match (car comp) cmd)
                          (setq f (cdr comp))))
                    f))))
           (if (eq table t)
               ;; Unknown command.
               nil
             (list comp-beg comp-end table))))))))

;;;;
;;;; LaTeX syntax navigation
;;;;

(defmacro tex-search-noncomment (&rest body)
  "Execute BODY as long as it return non-nil and point is in a comment.
Return the value returned by the last execution of BODY."
  (declare (debug t))
  (let ((res-sym (make-symbol "result")))
    `(let (,res-sym)
       (while
	   (and (setq ,res-sym (progn ,@body))
		(save-excursion (skip-chars-backward "^\n%") (not (bolp)))))
       ,res-sym)))

(defun tex-last-unended-begin ()
  "Leave point at the beginning of the last `\\begin{...}' that is unended."
  (condition-case nil
      (while (and (tex-search-noncomment
		   (re-search-backward "\\\\\\(begin\\|end\\)\\s *{"))
		  (looking-at "\\\\end"))
	(tex-last-unended-begin))
    (search-failed (error "Couldn't find unended \\begin"))))

(defun tex-next-unmatched-end ()
  "Leave point at the end of the next `\\end' that is unmatched."
  (while (and (tex-search-noncomment
	       (re-search-forward "\\\\\\(begin\\|end\\)\\s *{[^}]+}"))
	      (save-excursion (goto-char (match-beginning 0))
			      (looking-at "\\\\begin")))
    (tex-next-unmatched-end)))

(defun tex-next-unmatched-eparen (otype)
  "Leave point after the next unmatched escaped closing parenthesis.
The string OTYPE is an opening parenthesis type: `(', `{', or `['."
  (condition-case nil
      (let ((ctype (char-to-string (cdr (aref (syntax-table)
					      (string-to-char otype))))))
	(while (and (tex-search-noncomment
		     (re-search-forward (format "\\\\[%s%s]" ctype otype)))
		    (save-excursion
		      (goto-char (match-beginning 0))
		      (looking-at (format "\\\\%s" (regexp-quote otype)))))
	  (tex-next-unmatched-eparen otype)))
    (wrong-type-argument (error "Unknown opening parenthesis type: %s" otype))
    (search-failed (error "Couldn't find closing escaped paren"))))

(defun tex-last-unended-eparen (ctype)
  "Leave point at the start of the last unended escaped opening parenthesis.
The string CTYPE is a closing parenthesis type:  `)', `}', or `]'."
  (condition-case nil
      (let ((otype (char-to-string (cdr (aref (syntax-table)
					      (string-to-char ctype))))))
	(while (and (tex-search-noncomment
		     (re-search-backward (format "\\\\[%s%s]" ctype otype)))
		    (looking-at (format "\\\\%s" (regexp-quote ctype))))
	  (tex-last-unended-eparen ctype)))
    (wrong-type-argument (error "Unknown opening parenthesis type: %s" ctype))
    (search-failed (error "Couldn't find unended escaped paren"))))

(defun tex-goto-last-unclosed-latex-block ()
  "Move point to the last unclosed \\begin{...}.
Mark is left at original location."
  (interactive)
  (let ((spot))
    (save-excursion
      (tex-last-unended-begin)
      (setq spot (point)))
    (push-mark)
    (goto-char spot)))

(defvar latex-handle-escaped-parens t)

;; Don't think this one actually _needs_ (for the purposes of
;; tex-mode) to handle escaped parens.
;; Does not handle escaped parens when latex-handle-escaped-parens is nil.
(defun latex-backward-sexp-1 ()
  "Like (backward-sexp 1) but aware of multi-char elements and escaped parens."
  (let ((pos (point))
	(forward-sexp-function))
    (backward-sexp 1)
    (cond ((looking-at
	    (if latex-handle-escaped-parens
		"\\\\\\(begin\\>\\|[[({]\\)"
	      "\\\\begin\\>"))
	   (signal 'scan-error
		   (list "Containing expression ends prematurely"
			 (point) (prog1 (point) (goto-char pos)))))
	  ((and latex-handle-escaped-parens
		(looking-at "\\\\\\([])}]\\)"))
	   (tex-last-unended-eparen (match-string 1)))
	  ((eq (char-after) ?{)
	   (let ((newpos (point)))
	     (when (ignore-errors (backward-sexp 1) t)
	       (if (or (looking-at "\\\\end\\>")
		       ;; In case the \\ ends a verbatim section.
		       (and (looking-at "end\\>") (eq (char-before) ?\\)))
		   (tex-last-unended-begin)
		 (goto-char newpos))))))))

;; Note this does not handle things like mismatched brackets inside
;; begin/end blocks.
;; Needs to handle escaped parens for tex-validate-*.
;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2007-09/msg00038.html
;; Does not handle escaped parens when latex-handle-escaped-parens is nil.
(defun latex-forward-sexp-1 ()
  "Like (forward-sexp 1) but aware of multi-char elements and escaped parens."
  (let ((pos (point))
	(forward-sexp-function))
    (forward-sexp 1)
    (let ((newpos (point)))
      (skip-syntax-backward "/w")
      (cond
       ((looking-at "\\\\end\\>")
	(signal 'scan-error
		(list "Containing expression ends prematurely"
		      (point)
		      (prog1
			  (progn (ignore-errors (forward-sexp 2)) (point))
			(goto-char pos)))))
       ((looking-at "\\\\begin\\>")
	(goto-char (match-end 0))
	(tex-next-unmatched-end))
       ;; A better way to handle this, \( .. \) etc, is probably to
       ;; temporarily change the syntax of the \ in \( to punctuation.
       ((and latex-handle-escaped-parens
	     (looking-back "\\\\[])}]"))
	(signal 'scan-error
		(list "Containing expression ends prematurely"
		      (- (point) 2) (prog1 (point)
				      (goto-char pos)))))
       ((and latex-handle-escaped-parens
	     (looking-back "\\\\\\([({[]\\)"))
	(tex-next-unmatched-eparen (match-string 1)))
       (t (goto-char newpos))))))

(defun latex-forward-sexp (&optional arg)
  "Like `forward-sexp' but aware of multi-char elements and escaped parens."
  (interactive "P")
  (unless arg (setq arg 1))
  (let ((pos (point)))
    (condition-case err
	(while (/= arg 0)
	  (setq arg
		(if (> arg 0)
		    (progn (latex-forward-sexp-1) (1- arg))
		  (progn (latex-backward-sexp-1) (1+ arg)))))
      (scan-error
       (goto-char pos)
       (signal (car err) (cdr err))))))

(defun latex-syntax-after ()
  "Like (char-syntax (char-after)) but aware of multi-char elements."
  (if (looking-at "\\\\end\\>") ?\) (char-syntax (following-char))))

(defun latex-skip-close-parens ()
  "Like (skip-syntax-forward \" )\") but aware of multi-char elements."
  (let ((forward-sexp-function nil))
    (while (progn (skip-syntax-forward " )")
		  (looking-at "\\\\end\\>"))
      (forward-sexp 2))))

(defun latex-down-list ()
  "Like (down-list 1) but aware of multi-char elements."
  (forward-comment (point-max))
  (let ((forward-sexp-function nil))
    (if (not (looking-at "\\\\begin\\>"))
	(down-list 1)
      (forward-sexp 1)
      ;; Skip arguments.
      (while (looking-at "[ \t]*[[{(]")
	(with-syntax-table tex-mode-syntax-table
	  (forward-sexp))))))

(defalias 'tex-close-latex-block 'latex-close-block)
(define-skeleton latex-close-block
  "Create an \\end{...} to match the last unclosed \\begin{...}."
  (save-excursion
    (tex-last-unended-begin)
    (if (not (looking-at "\\\\begin\\(\\s *{[^}\n]*}\\)")) '("{" _ "}")
      (match-string 1)))
  \n "\\end" str > \n)

(define-skeleton latex-split-block
  "Split the enclosing environment by inserting \\end{..}\\begin{..} at point."
  (save-excursion
    (tex-last-unended-begin)
    (if (not (looking-at "\\\\begin\\(\\s *{[^}\n]*}\\)")) '("{" _ "}")
      (prog1 (match-string 1)
	(goto-char (match-end 1))
	(setq v1 (buffer-substring (point)
				   (progn
				     (while (looking-at "[ \t]*[[{]")
				       (forward-sexp 1))
				     (point)))))))
  \n "\\end" str > \n _ \n "\\begin" str v1 > \n)

(defconst tex-discount-args-cmds
  '("begin" "end" "input" "special" "cite" "ref" "include" "includeonly"
    "documentclass" "usepackage" "label")
  "TeX commands whose arguments should not be counted as text.")

(defun tex-count-words (begin end)
  "Count the number of words in the buffer."
  (interactive
   (if (and transient-mark-mode mark-active)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  ;; TODO: skip comments and math and maybe some environments.
  (save-excursion
    (goto-char begin)
    (let ((count 0))
      (while (and (< (point) end) (re-search-forward "\\<" end t))
	(if (not (eq (char-syntax (preceding-char)) ?/))
	    (progn
	      ;; Don't count single-char words.
	      (unless (looking-at ".\\>") (incf count))
	      (forward-char 1))
	  (let ((cmd
		 (buffer-substring-no-properties
		  (point) (progn (when (zerop (skip-chars-forward "a-zA-Z@"))
				   (forward-char 1))
				 (point)))))
	    (when (member cmd tex-discount-args-cmds)
	      (skip-chars-forward "*")
	      (forward-comment (point-max))
	      (when (looking-at "\\[")
		(forward-sexp 1)
		(forward-comment (point-max)))
	      (if (not (looking-at "{"))
		  (forward-char 1)
		(forward-sexp 1))))))
      (message "%s words" count))))



;;; Invoking TeX in an inferior shell.

;; Why use a shell instead of running TeX directly?  Because if TeX
;; gets stuck, the user can switch to the shell window and type at it.

(defvar tex-error-parse-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?\\ "\\" st)
    (modify-syntax-entry ?\{ "_" st)
    (modify-syntax-entry ?\} "_" st)
    (modify-syntax-entry ?\[ "_" st)
    (modify-syntax-entry ?\] "_" st)
    ;; Single quotations may appear in errors
    (modify-syntax-entry ?\" "_" st)
    st)
  "Syntax-table used while parsing TeX error messages.")

(defun tex-old-error-file-name ()
  ;; This is unreliable, partly because we don't try very hard, and
  ;; partly because TeX's output format is eminently ambiguous and unfriendly
  ;; to automation.
  (save-excursion
    (save-match-data
      (with-syntax-table tex-error-parse-syntax-table
        (beginning-of-line)
        (backward-up-list 1)
        (skip-syntax-forward "(_")
        (while (not (let ((try-filename (thing-at-point 'filename)))
                      (and try-filename
                           (not (string= "" try-filename))
                           (file-readable-p try-filename))))
          (skip-syntax-backward "(_")
          (backward-up-list 1)
          (skip-syntax-forward "(_"))
        (thing-at-point 'filename)))))

(defconst tex-error-regexp-alist
  ;; First alternative handles the newer --file-line-error style:
  ;; ./test2.tex:14: Too many }'s.
  '(gnu
    ;; Second handles the old-style, which spans two lines but doesn't include
    ;; any file info:
    ;; ! Too many }'s.
    ;; l.396 toto}
    ("^l\\.\\([1-9][0-9]*\\) \\(?:\\.\\.\\.\\)?\\(.*\\)$"
     tex-old-error-file-name 1 nil nil nil
     ;; Since there's no filename to highlight, let's highlight the message.
     (2 compilation-error-face))
    ;; A few common warning messages.
    ("^\\(?:Und\\|Ov\\)erfull \\\\[hv]box .* at lines? \\(\\([1-9][0-9]*\\)\\(?:--\\([1-9][0-9]*\\)\\)?\\)$"
     tex-old-error-file-name (2 . 3) nil 1 nil
     (1 compilation-warning-face))
    ("^(Font) *\\([^ \n].* on input line \\([1-9][0-9]*\\)\\)\\.$"
     tex-old-error-file-name 2 nil 1 1
     (2 compilation-warning-face))
    ;; Included files get output as (<file> ...).
    ;; FIXME: there tend to be a boatload of them at the beginning of the
    ;; output which aren't that interesting.  Maybe we should filter out
    ;; all the file name that start with /usr/share?
    ;; ("(\\.?/\\([^() \n]+\\)" 1 nil nil 0)
    ))

;; The utility functions:

(define-derived-mode tex-shell shell-mode "TeX-Shell"
  (set (make-local-variable 'compilation-error-regexp-alist)
       tex-error-regexp-alist)
  (compilation-shell-minor-mode t))

;;;###autoload
(defun tex-start-shell ()
  (with-current-buffer
      (make-comint
       "tex-shell"
       (or tex-shell-file-name (getenv "ESHELL") shell-file-name)
       nil
       ;; Specify an interactive shell, to make sure it prompts.
       "-i")
    (let ((proc (get-process "tex-shell")))
      (set-process-sentinel proc 'tex-shell-sentinel)
      (set-process-query-on-exit-flag proc nil)
      (tex-shell)
      (while (zerop (buffer-size))
	(sleep-for 1)))))

(defun tex-feed-input ()
  "Send input to the tex shell process.
In the tex buffer this can be used to continue an interactive tex run.
In the tex shell buffer this command behaves like `comint-send-input'."
  (interactive)
  (set-buffer (tex-shell-buf))
  (comint-send-input)
  (tex-recenter-output-buffer nil))

(defun tex-display-shell ()
  "Make the TeX shell buffer visible in a window."
  (display-buffer (tex-shell-buf))
  (tex-recenter-output-buffer nil))

(defun tex-shell-sentinel (proc msg)
  (cond ((null (buffer-name (process-buffer proc)))
	 ;; buffer killed
	 (set-process-buffer proc nil)
         (tex-delete-last-temp-files))
	((memq (process-status proc) '(signal exit))
         (tex-delete-last-temp-files))))

(defun tex-set-buffer-directory (buffer directory)
  "Set BUFFER's default directory to be DIRECTORY."
  (setq directory (file-name-as-directory (expand-file-name directory)))
  (if (not (file-directory-p directory))
      (error "%s is not a directory" directory)
    (with-current-buffer buffer
      (setq default-directory directory))))

(defvar tex-send-command-modified-tick 0)
(make-variable-buffer-local 'tex-send-command-modified-tick)

(defun tex-shell-proc ()
  (or (tex-shell-running) (error "No TeX subprocess")))
(defun tex-shell-buf ()
  (process-buffer (tex-shell-proc)))
(defun tex-shell-buf-no-error ()
  (let ((proc (tex-shell-running)))
    (and proc (process-buffer proc))))

(defun tex-send-command (command &optional file background)
  "Send COMMAND to TeX shell process, substituting optional FILE for *.
Do this in background if optional BACKGROUND is t.  If COMMAND has no *,
FILE will be appended, preceded by a blank, to COMMAND.  If FILE is nil, no
substitution will be made in COMMAND.  COMMAND can be any expression that
evaluates to a command string.

Return the process in which TeX is running."
  (save-excursion
    (let* ((cmd (eval command))
	   (proc (tex-shell-proc))
	   (buf (process-buffer proc))
           (star (string-match "\\*" cmd))
	   (string
	    (concat
	     (if (null file)
		 cmd
               (if (file-name-absolute-p file)
                   (setq file (convert-standard-filename file)))
	       (if star (concat (substring cmd 0 star)
                                (shell-quote-argument file)
                                (substring cmd (1+ star)))
                 (concat cmd " " (shell-quote-argument file))))
	     (if background "&" ""))))
      ;; Switch to buffer before checking for subproc output in it.
      (set-buffer buf)
      ;; If text is unchanged since previous tex-send-command,
      ;; we haven't got any output.  So wait for output now.
      (if (= (buffer-modified-tick buf) tex-send-command-modified-tick)
	  (accept-process-output proc))
      (goto-char (process-mark proc))
      (insert string)
      (comint-send-input)
      (setq tex-send-command-modified-tick (buffer-modified-tick buf))
      proc)))

(defun tex-delete-last-temp-files (&optional not-all)
  "Delete any junk files from last temp file.
If NOT-ALL is non-nil, save the `.dvi' file."
  (if tex-last-temp-file
      (let* ((dir (file-name-directory tex-last-temp-file))
	     (list (and (file-directory-p dir)
			(file-name-all-completions
			 (file-name-sans-extension
			  (file-name-nondirectory tex-last-temp-file))
			 dir))))
	(while list
	  (if not-all
	      (and
	       ;; If arg is non-nil, don't delete the .dvi file.
	       (not (string-match "\\.dvi$" (car list)))
	       (delete-file (concat dir (car list))))
	    (delete-file (concat dir (car list))))
          (setq list (cdr list))))))

(add-hook 'kill-emacs-hook 'tex-delete-last-temp-files)

;;
;; Machinery to guess the command that the user wants to execute.
;;

(defvar tex-compile-history nil)

(defvar tex-input-files-re
  (eval-when-compile
    (concat "\\." (regexp-opt '("tex" "texi" "texinfo"
				"bbl" "ind" "sty" "cls") t)
	    ;; Include files with no dots (for directories).
	    "\\'\\|\\`[^.]+\\'")))

(defcustom tex-use-reftex t
  "If non-nil, use RefTeX's list of files to determine what command to use."
  :type 'boolean
  :group 'tex)

(defvar tex-compile-commands
  '(((concat "pdf" tex-command
	     " " (if (< 0 (length tex-start-commands))
		     (shell-quote-argument tex-start-commands)) " %f")
     t "%r.pdf")
    ((concat tex-command
	     " " (if (< 0 (length tex-start-commands))
		     (shell-quote-argument tex-start-commands)) " %f")
     t "%r.dvi")
    ("xdvi %r &" "%r.dvi")
    ("\\doc-view \"%r.pdf\"" "%r.pdf")
    ("xpdf %r.pdf &" "%r.pdf")
    ("gv %r.ps &" "%r.ps")
    ("yap %r &" "%r.dvi")
    ("advi %r &" "%r.dvi")
    ("gv %r.pdf &" "%r.pdf")
    ("bibtex %r" "%r.aux" "%r.bbl")
    ("makeindex %r" "%r.idx" "%r.ind")
    ("texindex %r.??")
    ("dvipdfm %r" "%r.dvi" "%r.pdf")
    ("dvipdf %r" "%r.dvi" "%r.pdf")
    ("dvips -o %r.ps %r" "%r.dvi" "%r.ps")
    ("ps2pdf %r.ps" "%r.ps" "%r.pdf")
    ("lpr %r.ps" "%r.ps"))
  "List of commands for `tex-compile'.
Each element should be of the form (FORMAT IN OUT) where
FORMAT is an expression that evaluates to a string that can contain
  - `%r' the main file name without extension.
  - `%f' the main file name.
IN can be either a string (with the same % escapes in it) indicating
  the name of the input file, or t to indicate that the input is all
  the TeX files of the document, or nil if we don't know.
OUT describes the output file and is either a %-escaped string
  or nil to indicate that there is no output file.")

;; defsubst* gives better byte-code than defsubst.
(defsubst* tex-string-prefix-p (str1 str2)
  "Return non-nil if STR1 is a prefix of STR2"
  (eq t (compare-strings str2 nil (length str1) str1 nil nil)))

(defun tex-guess-main-file (&optional all)
  "Find a likely `tex-main-file'.
Looks for hints in other buffers in the same directory or in
ALL other buffers.  If ALL is `sub' only look at buffers in parent directories
of the current buffer."
  (let ((dir default-directory)
	(header-re tex-start-of-header))
    (catch 'found
      ;; Look for a buffer with `tex-main-file' set.
      (dolist (buf (if (consp all) all (buffer-list)))
	(with-current-buffer buf
	  (when (and (cond
		      ((null all) (equal dir default-directory))
		      ((eq all 'sub) (tex-string-prefix-p default-directory dir))
		      (t))
		     (stringp tex-main-file))
	    (throw 'found (expand-file-name tex-main-file)))))
      ;; Look for a buffer containing the magic `tex-start-of-header'.
      (dolist (buf (if (consp all) all (buffer-list)))
	(with-current-buffer buf
	  (when (and (cond
		      ((null all) (equal dir default-directory))
		      ((eq all 'sub) (tex-string-prefix-p default-directory dir))
		      (t))
		     buffer-file-name
		     ;; (or (easy-mmode-derived-mode-p 'latex-mode)
		     ;; 	 (easy-mmode-derived-mode-p 'plain-tex-mode))
		     (save-excursion
		       (save-restriction
			 (widen)
			 (goto-char (point-min))
			 (re-search-forward
			  header-re (+ (point) 10000) t))))
	    (throw 'found (expand-file-name buffer-file-name))))))))

(defun tex-main-file ()
  "Return the relative name of the main file."
  (let* ((file (or tex-main-file
		   ;; Compatibility with AUCTeX.
		   (with-no-warnings
		    (when (boundp 'TeX-master)
		      (cond ((stringp TeX-master)
			     (make-local-variable 'tex-main-file)
			     (setq tex-main-file TeX-master))
			    ((and (eq TeX-master t) buffer-file-name)
			     (file-relative-name buffer-file-name)))))
		   ;; Try to guess the main file.
		   (if (not buffer-file-name)
		       (error "Buffer is not associated with any file")
		     (file-relative-name
		      (if (save-excursion
			    (goto-char (point-min))
			    (re-search-forward tex-start-of-header
					       (+ (point) 10000) t))
			  ;; This is the main file.
			  buffer-file-name
			;; This isn't the main file, let's try to find better,
			(or (tex-guess-main-file)
			    (tex-guess-main-file 'sub)
			    ;; (tex-guess-main-file t)
			    buffer-file-name)))))))
    (if (or (file-exists-p file) (string-match "\\.tex\\'" file))
	file (concat file ".tex"))))

(defun tex-summarize-command (cmd)
  (if (not (stringp cmd)) ""
    (mapconcat 'identity
	       (mapcar (lambda (s) (car (split-string s)))
		       (split-string cmd "\\s-*\\(?:;\\|&&\\)\\s-*"))
	       "&")))

(defun tex-uptodate-p (file)
  "Return non-nil if FILE is not uptodate w.r.t the document source files.
FILE is typically the output DVI or PDF file."
  ;; We should check all the files included !!!
  (and
   ;; Clearly, the target must exist.
   (file-exists-p file)
   ;; And the last run must not have asked for a rerun.
   ;; FIXME: this should check that the last run was done on the same file.
   (let ((buf (condition-case nil (tex-shell-buf) (error nil))))
     (when buf
       (with-current-buffer buf
	 (save-excursion
	   (goto-char (point-max))
	   (and (re-search-backward
                 (concat "(see the transcript file for additional information)"
                         "\\|^Output written on .*"
                         (regexp-quote (file-name-nondirectory file))
                         " (.*)\\.")
                 nil t)
		(> (save-excursion
                     ;; Usually page numbers are output as [N], but
                     ;; I've already seen things like
                     ;; [1{/var/lib/texmf/fonts/map/pdftex/updmap/pdftex.map}]
                     (or (re-search-backward "\\[[0-9]+\\({[^}]*}\\)?\\]"
                                             nil t)
			 (point-min)))
		   (save-excursion
		     (or (re-search-backward "Rerun" nil t)
			 (point-min)))))))))
   ;; And the input files must not have been changed in the meantime.
   (let ((files (if (and tex-use-reftex
			 (fboundp 'reftex-scanning-info-available-p)
			 (reftex-scanning-info-available-p))
		    (reftex-all-document-files)
		  (list (file-name-directory (expand-file-name file)))))
	 (ignored-dirs-re
	  (concat
	   (regexp-opt
	    (delq nil (mapcar (lambda (s) (if (eq (aref s (1- (length s))) ?/)
					 (substring s 0 (1- (length s)))))
			      completion-ignored-extensions))
	    t) "\\'"))
	 (uptodate t))
     (while (and files uptodate)
       (let ((f (pop files)))
	 (if (and (file-directory-p f)
		  ;; Avoid infinite loops.
		  (not (file-symlink-p f)))
	     (unless (string-match ignored-dirs-re f)
	       (setq files (nconc
                            (ignore-errors ;Not readable or something.
                              (directory-files f t tex-input-files-re))
			    files)))
	   (when (file-newer-than-file-p f file)
	     (setq uptodate nil)))))
     uptodate)))


(autoload 'format-spec "format-spec")

(defvar tex-executable-cache nil)
(defun tex-executable-exists-p (name)
  "Like `executable-find' but with a cache."
  (let ((f (and (string-match "^\\\\\\([^ \t\n]+\\)" name)
                (intern-soft (concat "tex-cmd-" (match-string 1 name))))))
    (if (fboundp f)
        f
      (let ((cache (assoc name tex-executable-cache)))
        (if cache (cdr cache)
          (let ((executable (executable-find name)))
            (push (cons name executable) tex-executable-cache)
            executable))))))

(defun tex-command-executable (cmd)
  (let ((s (if (stringp cmd) cmd (eval (car cmd)))))
    (substring s 0 (string-match "[ \t]\\|\\'" s))))

(defun tex-command-active-p (cmd fspec)
  "Return non-nil if the CMD spec might need to be run."
  (let ((in (nth 1 cmd))
	(out (nth 2 cmd)))
    (if (stringp in)
	(let ((file (format-spec in fspec)))
	  (when (file-exists-p file)
	    (or (not out)
		(file-newer-than-file-p
		 file (format-spec out fspec)))))
      (when (and (eq in t) (stringp out))
	(not (tex-uptodate-p (format-spec out fspec)))))))

(defcustom tex-cmd-bibtex-args "--min-crossref=100"
  "Extra args to pass to `bibtex' by default."
  :type 'string
  :version "23.1"
  :group 'tex-run)

(defun tex-format-cmd (format fspec)
  "Like `format-spec' but adds user-specified args to the command.
Only applies the FSPEC to the args part of FORMAT."
  (if (not (string-match "\\([^ /\\]+\\) " format))
      (format-spec format fspec)
    (let* ((prefix (substring format 0 (match-beginning 0)))
           (cmd (match-string 1 format))
           (args (substring format (match-end 0)))
           (sym (intern-soft (format "tex-cmd-%s-args" cmd)))
           (extra-args (and sym (symbol-value sym))))
      (concat prefix cmd
              (if extra-args (concat " " extra-args))
              " " (format-spec args fspec)))))

(defun tex-compile-default (fspec)
  "Guess a default command given the `format-spec' FSPEC."
  ;; TODO: Learn to do latex+dvips!
  (let ((cmds nil)
	(unchanged-in nil))
    ;; Only consider active commands.
    (dolist (cmd tex-compile-commands)
      (when (tex-executable-exists-p (tex-command-executable cmd))
	(if (tex-command-active-p cmd fspec)
	    (push cmd cmds)
	  (push (nth 1 cmd) unchanged-in))))
    ;; If no command seems to be applicable, arbitrarily pick the first one.
    (setq cmds (if cmds (nreverse cmds) (list (car tex-compile-commands))))
    ;; Remove those commands whose input was considered stable for
    ;; some other command (typically if (t . "%.pdf") is inactive
    ;; then we're using pdflatex and the fact that the dvi file
    ;; is nonexistent doesn't matter).
    (let ((tmp nil))
      (dolist (cmd cmds)
	(unless (member (nth 1 cmd) unchanged-in)
	  (push cmd tmp)))
      ;; Only remove if there's something left.
      (if tmp (setq cmds (nreverse tmp))))
    ;; Remove commands whose input is not uptodate either.
    (let ((outs (delq nil (mapcar (lambda (x) (nth 2 x)) cmds)))
	  (tmp nil))
      (dolist (cmd cmds)
	(unless (member (nth 1 cmd) outs)
	  (push cmd tmp)))
      ;; Only remove if there's something left.
      (if tmp (setq cmds (nreverse tmp))))
    ;; Select which file we're going to operate on (the latest).
    (let ((latest (nth 1 (car cmds))))
      (dolist (cmd (prog1 (cdr cmds) (setq cmds (list (car cmds)))))
	(if (equal latest (nth 1 cmd))
	    (push cmd cmds)
	  (unless (eq latest t)		;Can't beat that!
	    (if (or (not (stringp latest))
		    (eq (nth 1 cmd) t)
		    (and (stringp (nth 1 cmd))
			 (file-newer-than-file-p
			  (format-spec (nth 1 cmd) fspec)
			  (format-spec latest fspec))))
		(setq latest (nth 1 cmd) cmds (list cmd)))))))
    ;; Expand the command spec into the actual text.
    (dolist (cmd (prog1 cmds (setq cmds nil)))
      (push (cons (eval (car cmd)) (cdr cmd)) cmds))
    ;; Select the favorite command from the history.
    (let ((hist tex-compile-history)
	  re hist-cmd)
      (while hist
	(setq hist-cmd (pop hist))
	(setq re (concat "\\`"
			 (regexp-quote (tex-command-executable hist-cmd))
			 "\\([ \t]\\|\\'\\)"))
	(dolist (cmd cmds)
	  ;; If the hist entry uses the same command and applies to a file
	  ;; of the same type (e.g. `gv %r.pdf' vs `gv %r.ps'), select cmd.
	  (and (string-match re (car cmd))
	       (or (not (string-match "%[fr]\\([-._[:alnum:]]+\\)" (car cmd)))
		   (string-match (regexp-quote (match-string 1 (car cmd)))
				 hist-cmd))
	       (setq hist nil cmds (list cmd)))))
      ;; Substitute and return.
      (if (and hist-cmd
	       (string-match (concat "[' \t\"]" (format-spec "%r" fspec)
				     "\\([;&' \t\"]\\|\\'\\)") hist-cmd))
	  ;; The history command was already applied to the same file,
	  ;; so just reuse it.
	  hist-cmd
	(if cmds (tex-format-cmd (caar cmds) fspec))))))

(defun tex-cmd-doc-view (file)
  (pop-to-buffer (find-file-noselect file)))

(defun tex-compile (dir cmd)
  "Run a command CMD on current TeX buffer's file in DIR."
  ;; FIXME: Use time-stamps on files to decide the next op.
  (interactive
   (let* ((file (tex-main-file))
	  (default-directory
	    (prog1 (file-name-directory (expand-file-name file))
	      (setq file (file-name-nondirectory file))))
	  (root (file-name-sans-extension file))
	  (fspec (list (cons ?r (shell-quote-argument root))
		       (cons ?f (shell-quote-argument file))))
	  (default (tex-compile-default fspec)))
     (list default-directory
	   (completing-read
	    (format "Command [%s]: " (tex-summarize-command default))
	    (mapcar (lambda (x)
		      (list (tex-format-cmd (eval (car x)) fspec)))
		    tex-compile-commands)
	    nil nil nil 'tex-compile-history default))))
  (save-some-buffers (not compilation-ask-about-save) nil)
  (let ((f (and (string-match "^\\\\\\([^ \t\n]+\\)" cmd)
                (intern-soft (concat "tex-cmd-" (match-string 1 cmd))))))
    (if (functionp f)
        (condition-case nil
            (let ((default-directory dir))
              (apply f (split-string-and-unquote
                        (substring cmd (match-end 0)))))
          (wrong-number-of-arguments
           (error "Wrong number of arguments to %s"
                  (substring (symbol-name f) 8))))
      (if (tex-shell-running)
          (tex-kill-job)
        (tex-start-shell))
      (tex-send-tex-command cmd dir))))

(defun tex-start-tex (command file &optional dir)
  "Start a TeX run, using COMMAND on FILE."
  (let* ((star (string-match "\\*" command))
         (compile-command
          (if star
	      (concat (substring command 0 star)
		      (shell-quote-argument file)
		      (substring command (1+ star)))
            (concat command " "
		    tex-start-options
		    (if (< 0 (length tex-start-commands))
			(concat
			 (shell-quote-argument tex-start-commands) " "))
		    (shell-quote-argument file)))))
    (tex-send-tex-command compile-command dir)))

(defun tex-send-tex-command (cmd &optional dir)
  (unless (or (equal dir (let ((buf (tex-shell-buf-no-error)))
                           (and buf (with-current-buffer buf
                                      default-directory))))
	      (not dir))
    (let (shell-dirtrack-verbose)
      (tex-send-command tex-shell-cd-command dir)))
  (with-current-buffer (process-buffer (tex-send-command cmd))
    (setq compilation-last-buffer (current-buffer))
    (compilation-forget-errors)
    ;; Don't parse previous compilations.
    (set-marker compilation-parsing-end (1- (point-max))))
  (tex-display-shell)
  (setq tex-last-buffer-texed (current-buffer)))

;;; The commands:

(defun tex-region (beg end)
  "Run TeX on the current region, via a temporary file.
The file's name comes from the variable `tex-zap-file' and the
variable `tex-directory' says where to put it.

If the buffer has a header, the header is given to TeX before the
region itself.  The buffer's header is all lines between the strings
defined by `tex-start-of-header' and `tex-end-of-header' inclusive.
The header must start in the first 100 lines of the buffer.

The value of `tex-trailer' is given to TeX as input after the region.

The value of `tex-command' specifies the command to use to run TeX."
  (interactive "r")
  (if (tex-shell-running)
      (tex-kill-job)
    (tex-start-shell))
  (or tex-zap-file
      (setq tex-zap-file (tex-generate-zap-file-name)))
  ;; Temp file will be written and TeX will be run in zap-directory.
  ;; If the TEXINPUTS file has relative directories or if the region has
  ;; \input of files, this must be the same directory as the file for
  ;; TeX to access the correct inputs.  That's why it's safest if
  ;; tex-directory is ".".
  (let* ((zap-directory
          (file-name-as-directory (expand-file-name tex-directory)))
         (tex-out-file (expand-file-name (concat tex-zap-file ".tex")
					 zap-directory))
	 (main-file (expand-file-name (tex-main-file)))
	 (ismain (string-equal main-file (buffer-file-name)))
	 already-output)
    ;; Don't delete temp files if we do the same buffer twice in a row.
    (or (eq (current-buffer) tex-last-buffer-texed)
	(tex-delete-last-temp-files t))
    (let ((default-directory zap-directory)) ; why?
      ;; We assume the header is fully contained in tex-main-file.
      ;; We use f-f-ns so we get prompted about any changes on disk.
      (with-current-buffer (find-file-noselect main-file)
	(setq already-output (tex-region-header tex-out-file
						(and ismain beg))))
      ;; Write out the specified region (but don't repeat anything
      ;; already written in the header).
      (write-region (if ismain
			(max beg already-output)
		      beg)
		    end tex-out-file (not (zerop already-output)))
      ;; Write the trailer, if any.
      ;; Precede it with a newline to make sure it
      ;; is not hidden in a comment.
      (if tex-trailer
	  (write-region (concat "\n" tex-trailer) nil
			tex-out-file t)))
    ;; Record the file name to be deleted afterward.
    (setq tex-last-temp-file tex-out-file)
    ;; Use a relative file name here because (1) the proper dir
    ;; is already current, and (2) the abs file name is sometimes
    ;; too long and can make tex crash.
    (tex-start-tex tex-command (concat tex-zap-file ".tex") zap-directory)
    (setq tex-print-file tex-out-file)))

(defun tex-region-header (file &optional beg)
  "If there is a TeX header in the current buffer, write it to FILE.
Return point at the end of the region so written, or zero.  If
the optional buffer position BEG is specified, then the region
written out starts at BEG, if this lies before the start of the header.

If the first line matches `tex-first-line-header-regexp', it is
also written out.  The variables `tex-start-of-header' and
`tex-end-of-header' are used to locate the header.  Note that the
start of the header is required to be within the first 100 lines."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((search-end (save-excursion
			  (forward-line 100)
			  (point)))
	    (already-output 0)
	    hbeg hend)
	;; Maybe copy first line, such as `\input texinfo', to temp file.
	(and tex-first-line-header-regexp
	     (looking-at tex-first-line-header-regexp)
	     (write-region (point)
			   (progn (forward-line 1)
				  (setq already-output (point)))
			   file))
	;; Write out the header, if there is one, and any of the
	;; specified region which extends before it.  But don't repeat
	;; anything already written.
	(and tex-start-of-header
	     (re-search-forward tex-start-of-header search-end t)
	     (progn
	       (beginning-of-line)
	       (setq hbeg (point))	; mark beginning of header
	       (when (re-search-forward tex-end-of-header nil t)
		 (forward-line 1)
		 (setq hend (point))	; mark end of header
		 (write-region
		  (max (if beg
			   (min hbeg beg)
			 hbeg)
		       already-output)
		  hend file (not (zerop already-output)))
		 (setq already-output hend))))
	already-output))))

(defun tex-buffer ()
  "Run TeX on current buffer.  See \\[tex-region] for more information.
Does not save the buffer, so it's useful for trying experimental versions.
See \\[tex-file] for an alternative."
  (interactive)
  (tex-region (point-min) (point-max)))

(defun tex-file ()
  "Prompt to save all buffers and run TeX (or LaTeX) on current buffer's file.
This function is more useful than \\[tex-buffer] when you need the
`.aux' file of LaTeX to have the correct name."
  (interactive)
  (when tex-offer-save
    (save-some-buffers))
  (let* ((source-file (tex-main-file))
	 (file-dir (file-name-directory (expand-file-name source-file))))
    (if (tex-shell-running)
        (tex-kill-job)
      (tex-start-shell))
    (tex-start-tex tex-command source-file file-dir)
    (setq tex-print-file (expand-file-name source-file))))

(defun tex-generate-zap-file-name ()
  "Generate a unique name suitable for use as a file name."
  ;; Include the shell process number and host name
  ;; in case there are multiple shells (for same or different user).
  ;; Dec 1998: There is a report that some versions of xdvi
  ;; don't work with file names that start with #.
  (format "_TZ_%d-%s"
          (process-id (get-buffer-process "*tex-shell*"))
	  (subst-char-in-string ?. ?- (system-name))))

;; This will perhaps be useful for modifying TEXINPUTS.
;; Expand each file name, separated by colons, in the string S.
(defun tex-expand-files (s)
  (let (elts (start 0))
    (while (string-match ":" s start)
      (setq elts (cons (substring s start (match-beginning 0)) elts))
      (setq start (match-end 0)))
    (or (= start 0)
	(setq elts (cons (substring s start) elts)))
    (mapconcat (lambda (elt)
		 (if (= (length elt) 0) elt (expand-file-name elt)))
	       (nreverse elts) ":")))

(defun tex-shell-running ()
  (let ((proc (get-process "tex-shell")))
    (when proc
      (if (and (eq (process-status proc) 'run)
               (buffer-live-p (process-buffer proc)))
          ;; return the TeX process on success
          proc
          ;; get rid of the process permanently
          ;; this should get rid of the annoying w32 problem with
          ;; dead tex-shell buffer and live process
          (delete-process proc)))))

(defun tex-kill-job ()
  "Kill the currently running TeX job."
  (interactive)
  ;; `quit-process' leads to core dumps of the tex process (except if
  ;; coredumpsize has limit 0kb as on many environments).  One would
  ;; like to use (kill-process proc 'lambda), however that construct
  ;; does not work on some systems and kills the shell itself.
  (let ((proc (get-process "tex-shell")))
    (when proc (quit-process proc t))))

(defun tex-recenter-output-buffer (linenum)
  "Redisplay buffer of TeX job output so that most recent output can be seen.
The last line of the buffer is displayed on
line LINE of the window, or centered if LINE is nil."
  (interactive "P")
  (let ((tex-shell (get-buffer "*tex-shell*"))
	(window))
    (if (null tex-shell)
	(message "No TeX output buffer")
      (setq window (display-buffer tex-shell))
      (save-selected-window
	(select-window window)
	(bury-buffer tex-shell)
	(goto-char (point-max))
	(recenter (if linenum
		      (prefix-numeric-value linenum)
		    (/ (window-height) 2)))))))

(defun tex-print (&optional alt)
  "Print the .dvi file made by \\[tex-region], \\[tex-buffer] or \\[tex-file].
Runs the shell command defined by `tex-dvi-print-command'.  If prefix argument
is provided, use the alternative command, `tex-alt-dvi-print-command'."
  (interactive "P")
  (let ((print-file-name-dvi (tex-append tex-print-file ".dvi"))
	test-name)
    (if (and (not (equal (current-buffer) tex-last-buffer-texed))
	     (buffer-file-name)
	     ;; Check that this buffer's printed file is up to date.
	     (file-newer-than-file-p
	      (setq test-name (tex-append (buffer-file-name) ".dvi"))
	      (buffer-file-name)))
	(setq print-file-name-dvi test-name))
    (if (not (file-exists-p print-file-name-dvi))
        (error "No appropriate `.dvi' file could be found")
      (if (tex-shell-running)
          (tex-kill-job)
        (tex-start-shell))
      (tex-send-command
       (if alt tex-alt-dvi-print-command tex-dvi-print-command)
       print-file-name-dvi
       t))))

(defun tex-alt-print ()
  "Print the .dvi file made by \\[tex-region], \\[tex-buffer] or \\[tex-file].
Runs the shell command defined by `tex-alt-dvi-print-command'."
  (interactive)
  (tex-print t))

(defun tex-view ()
  "Preview the last `.dvi' file made by running TeX under Emacs.
This means, made using \\[tex-region], \\[tex-buffer] or \\[tex-file].
The variable `tex-dvi-view-command' specifies the shell command for preview.
You must set that variable yourself before using this command,
because there is no standard value that would generally work."
  (interactive)
  (or tex-dvi-view-command
      (error "You must set `tex-dvi-view-command'"))
  ;; Restart the TeX shell if necessary.
  (or (tex-shell-running)
      (tex-start-shell))
  (let ((tex-dvi-print-command (eval tex-dvi-view-command)))
    (tex-print)))

(defun tex-append (file-name suffix)
  "Append to FILENAME the suffix SUFFIX, using same algorithm TeX uses.
Pascal-based TeX scans for the first period, C TeX uses the last.
No period is retained immediately before SUFFIX,
so normally SUFFIX starts with one."
  (if (stringp file-name)
      (let ((file (file-name-nondirectory file-name))
	    trial-name)
	;; Try splitting on last period.
	;; The first-period split can get fooled when two files
	;; named a.tex and a.b.tex are both tex'd;
	;; the last-period split must be right if it matches at all.
	(setq trial-name
	      (concat (file-name-directory file-name)
		      (substring file 0
				 (string-match "\\.[^.]*$" file))
		      suffix))
	(if (or (file-exists-p trial-name)
		(file-exists-p (concat trial-name ".aux"))) ;for BibTeX files
	    trial-name
	  ;; Not found, so split on first period.
	  (concat (file-name-directory file-name)
		  (substring file 0
			     (string-match "\\." file))
		  suffix)))
    " "))

(defun tex-show-print-queue ()
  "Show the print queue that \\[tex-print] put your job on.
Runs the shell command defined by `tex-show-queue-command'."
  (interactive)
  (if (tex-shell-running)
      (tex-kill-job)
    (tex-start-shell))
  (tex-send-command tex-show-queue-command)
  (tex-display-shell))

(defun tex-bibtex-file ()
  "Run BibTeX on the current buffer's file."
  (interactive)
  (if (tex-shell-running)
      (tex-kill-job)
    (tex-start-shell))
  (let* (shell-dirtrack-verbose
         (source-file (expand-file-name (tex-main-file)))
         (tex-out-file
          (tex-append (file-name-nondirectory source-file) ""))
         (file-dir (file-name-directory source-file)))
    (tex-send-command tex-shell-cd-command file-dir)
    (tex-send-command tex-bibtex-command tex-out-file))
  (tex-display-shell))

;;;;
;;;; LaTeX indentation
;;;;

(defvar tex-indent-allhanging t)
(defvar tex-indent-arg 4)
(defvar tex-indent-basic 2)
(defvar tex-indent-item tex-indent-basic)
(defvar tex-indent-item-re "\\\\\\(bib\\)?item\\>")
(defvar latex-noindent-environments '("document"))

(defvar tex-latex-indent-syntax-table
  (let ((st (make-syntax-table tex-mode-syntax-table)))
    (modify-syntax-entry ?$ "." st)
    (modify-syntax-entry ?\( "." st)
    (modify-syntax-entry ?\) "." st)
    st)
  "Syntax table used while computing indentation.")

(defun latex-indent (&optional arg)
  (if (and (eq (get-text-property (line-beginning-position) 'face)
	       'tex-verbatim))
      'noindent
    (with-syntax-table tex-latex-indent-syntax-table
      ;; TODO: Rather than ignore $, we should try to be more clever about it.
      (let ((indent
	     (save-excursion
	       (beginning-of-line)
	       (latex-find-indent))))
	(if (< indent 0) (setq indent 0))
	(if (<= (current-column) (current-indentation))
	    (indent-line-to indent)
	  (save-excursion (indent-line-to indent)))))))

(defcustom latex-indent-within-escaped-parens nil
  "Non-nil means add extra indent to text within escaped parens.
When this is non-nil, text within matching pairs of escaped
parens is indented at the column following the open paren.  The
default value does not add any extra indent thus providing the
behavior of Emacs 22 and earlier."
  :type 'boolean
  :group 'tex
  :version "23.1")

(defun latex-find-indent (&optional virtual)
  "Find the proper indentation of text after point.
VIRTUAL if non-nil indicates that we're only trying to find the indentation
  in order to determine the indentation of something else.
There might be text before point."
  (let ((latex-handle-escaped-parens latex-indent-within-escaped-parens))
    (save-excursion
      (skip-chars-forward " \t")
      (or
       ;; Stick the first line at column 0.
       (and (= (point-min) (line-beginning-position)) 0)
       ;; Trust the current indentation, if such info is applicable.
       (and virtual (save-excursion (skip-chars-backward " \t&") (bolp))
	    (current-column))
       ;; Stick verbatim environments to the left margin.
       (and (looking-at "\\\\\\(begin\\|end\\) *{\\([^\n}]+\\)")
	    (member (match-string 2) tex-verbatim-environments)
	    0)
       ;; Put leading close-paren where the matching open paren would be.
       (let (escaped)
	 (and (or (eq (latex-syntax-after) ?\))
		  ;; Try to handle escaped close parens but keep
		  ;; original position if it doesn't work out.
		  (and latex-handle-escaped-parens
		       (setq escaped (looking-at "\\\\\\([])}]\\)"))))
	      (ignore-errors
	       (save-excursion
		 (when escaped
		   (goto-char (match-beginning 1)))
		 (latex-skip-close-parens)
		 (latex-backward-sexp-1)
		 (latex-find-indent 'virtual)))))
       ;; Default (maybe an argument)
       (let ((pos (point))
	     ;; Outdent \item if necessary.
	     (indent (if (looking-at tex-indent-item-re) (- tex-indent-item) 0))
	     up-list-pos)
	 ;; Find the previous point which determines our current indentation.
	 (condition-case err
	     (progn
	       (latex-backward-sexp-1)
	       (while (> (current-column) (current-indentation))
		 (latex-backward-sexp-1)))
	   (scan-error
	    (setq up-list-pos (nth 2 err))))
	 (cond
	  ((= (point-min) pos) 0) ; We're really just indenting the first line.
	  ((integerp up-list-pos)
	   ;; Have to indent relative to the open-paren.
	   (goto-char up-list-pos)
	   (if (and (not tex-indent-allhanging)
		    (save-excursion
		      ;; Make sure we're an argument to a macro and
		      ;; that the macro is at the beginning of a line.
		      (condition-case nil
			  (progn
			    (while (eq (char-syntax (char-after)) ?\()
			      (forward-sexp -1))
			    (and (eq (char-syntax (char-after)) ?/)
				 (progn (skip-chars-backward " \t&")
					(bolp))))
			(scan-error nil)))
		    (> pos (progn (latex-down-list)
				  (forward-comment (point-max))
				  (point))))
	       ;; Align with the first element after the open-paren.
	       (current-column)
	     ;; We're the first element after a hanging brace.
	     (goto-char up-list-pos)
	     (+ (if (and (looking-at "\\\\begin *{\\([^\n}]+\\)")
			 (member (match-string 1)
				 latex-noindent-environments))
		    0 tex-indent-basic)
		indent (latex-find-indent 'virtual))))
	  ;; We're now at the "beginning" of a line.
	  ((not (and (not virtual) (eq (char-after) ?\\)))
	   ;; Nothing particular here: just keep the same indentation.
	   (+ indent (current-column)))
	  ;; We're now looking at a macro call.
	  ((looking-at tex-indent-item-re)
	   ;; Indenting relative to an item, have to re-add the outdenting.
	   (+ indent (current-column) tex-indent-item))
	  (t
	   (let ((col (current-column)))
	     (if (or (not (eq (char-syntax (or (char-after pos) ?\s)) ?\())
		     ;; Can't be an arg if there's an empty line inbetween.
		     (save-excursion (re-search-forward "^[ \t]*$" pos t)))
		 ;; If the first char was not an open-paren, there's
		 ;; a risk that this is really not an argument to the
		 ;; macro at all.
		 (+ indent col)
	       (forward-sexp 1)
	       (if (< (line-end-position)
		      (save-excursion (forward-comment (point-max))
				      (point)))
		   ;; we're indenting the first argument.
		   (min (current-column) (+ tex-indent-arg col))
		 (skip-syntax-forward " ")
		 (current-column)))))))))))
;;; DocTeX support

(defun doctex-font-lock-^^A ()
  (if (eq (char-after (line-beginning-position)) ?\%)
      (progn
	(put-text-property
	 (1- (match-beginning 1)) (match-beginning 1)
	 'syntax-table
	 (if (= (1+ (line-beginning-position)) (match-beginning 1))
	     ;; The `%' is a single-char comment, which Emacs
	     ;; syntax-table can't deal with.  We could turn it
	     ;; into a non-comment, or use `\n%' or `%^' as the comment.
	     ;; Instead, we include it in the ^^A comment.
             (string-to-syntax "< b")
           (string-to-syntax ">")))
	(let ((end (line-end-position)))
	  (if (< end (point-max))
	      (put-text-property
	       end (1+ end)
	       'syntax-table
               (string-to-syntax "> b"))))
        (string-to-syntax "< b"))))

(defun doctex-font-lock-syntactic-face-function (state)
  ;; Mark DocTeX documentation, which is parsed as a style A comment
  ;; starting in column 0.
  (if (or (nth 3 state) (nth 7 state)
	  (not (memq (char-before (nth 8 state))
		     '(?\n nil))))
      ;; Anything else is just as for LaTeX.
      (tex-font-lock-syntactic-face-function state)
    font-lock-doc-face))

(eval-when-compile
  (defconst doctex-syntax-propertize-rules
    (syntax-propertize-precompile-rules
     latex-syntax-propertize-rules
     ;; For DocTeX comment-in-doc.
     ("\\(\\^\\)\\^A" (1 (doctex-font-lock-^^A))))))

(defvar doctex-font-lock-keywords
  (append tex-font-lock-keywords
	  '(("^%<[^>]*>" (0 font-lock-preprocessor-face t)))))

;;;###autoload
(define-derived-mode doctex-mode latex-mode "DocTeX"
  "Major mode to edit DocTeX files."
  (setq font-lock-defaults
	(cons (append (car font-lock-defaults) '(doctex-font-lock-keywords))
	      (mapcar
	       (lambda (x)
		 (case (car-safe x)
		   (font-lock-syntactic-face-function
		    (cons (car x) 'doctex-font-lock-syntactic-face-function))
		   (t x)))
	       (cdr font-lock-defaults))))
  (set (make-local-variable 'syntax-propertize-function)
       (syntax-propertize-rules doctex-syntax-propertize-rules)))

(run-hooks 'tex-mode-load-hook)

(provide 'tex-mode)

;;; tex-mode.el ends here
