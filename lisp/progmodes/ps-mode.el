;;; ps-mode.el --- PostScript mode for GNU Emacs

;; Copyright (C) 1999, 2001-2012  Free Software Foundation, Inc.

;; Author:     Peter Kleiweg <p.c.j.kleiweg@rug.nl>
;; Maintainer: Peter Kleiweg <p.c.j.kleiweg@rug.nl>
;; Created:    20 Aug 1997
;; Version:    1.1h
;; Keywords:   PostScript, languages

;; Yoni Rabkin <yoni@rabkins.net> contacted the maintainer of this
;; file on 18/3/2008, and the maintainer agreed that when a bug is
;; filed in the Emacs bug reporting system against this file, a copy
;; of the bug report be sent to the maintainer's email address, but
;; only if: "... those e-mails have a link to the bug report system,
;; where I can cancel these e-mails if I want to.".

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

(defconst ps-mode-version "1.1h, 16 Jun 2005")
(defconst ps-mode-maintainer-address "Peter Kleiweg <p.c.j.kleiweg@rug.nl>")

(require 'comint)
(require 'easymenu)

;; Define core `PostScript' group.
(defgroup PostScript nil
  "PostScript mode for Emacs."
  :group 'languages)

(defgroup PostScript-edit nil
  "PostScript editing."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :prefix "ps-mode-"
  :group 'PostScript)

(defgroup PostScript-interaction nil
  "PostScript interaction."
  :prefix "ps-run-"
  :group 'PostScript)

;; User variables.

(defcustom ps-mode-auto-indent t
  "*Should we use autoindent?"
  :group 'PostScript-edit
  :type 'boolean)

(defcustom ps-mode-tab 4
  "*Number of spaces to use when indenting."
  :group 'PostScript-edit
  :type 'integer)

(defcustom ps-mode-paper-size '(595 842)
  "*Default paper size.

When inserting an EPSF template these values are used
to set the boundingbox to include the whole page.
When the figure is finished these values should be replaced."
  :group 'PostScript-edit
  :type '(choice
	  (const :tag "letter"       (612  792))
	  (const :tag "legal"        (612 1008))
	  (const :tag "a0" 	    (2380 3368))
	  (const :tag "a1" 	    (1684 2380))
	  (const :tag "a2" 	    (1190 1684))
	  (const :tag "a3" 	     (842 1190))
	  (const :tag "a4" 	     (595  842))
	  (const :tag "a5" 	     (421  595))
	  (const :tag "a6" 	     (297  421))
	  (const :tag "a7" 	     (210  297))
	  (const :tag "a8" 	     (148  210))
	  (const :tag "a9" 	     (105  148))
	  (const :tag "a10"           (74  105))
	  (const :tag "b0" 	    (2836 4008))
	  (const :tag "b1" 	    (2004 2836))
	  (const :tag "b2" 	    (1418 2004))
	  (const :tag "b3" 	    (1002 1418))
	  (const :tag "b4" 	     (709 1002))
	  (const :tag "b5" 	     (501  709))
	  (const :tag "archE"       (2592 3456))
	  (const :tag "archD"       (1728 2592))
	  (const :tag "archC"       (1296 1728))
	  (const :tag "archB"        (864 1296))
	  (const :tag "archA"        (648  864))
	  (const :tag "flsa"         (612  936))
	  (const :tag "flse"         (612  936))
	  (const :tag "halfletter"   (396  612))
	  (const :tag "11x17"        (792 1224))
	  (const :tag "tabloid"      (792 1224))
	  (const :tag "ledger"      (1224  792))
	  (const :tag "csheet"      (1224 1584))
	  (const :tag "dsheet"      (1584 2448))
	  (const :tag "esheet"      (2448 3168))))

(defcustom ps-mode-print-function
  (lambda ()
     (let ((lpr-switches nil)
	   (lpr-command (if (memq system-type '(usg-unix-v hpux irix))
			    "lp" "lpr")))
       (lpr-buffer)))
  "*Lisp function to print current buffer as PostScript."
  :group 'PostScript-edit
  :type 'function)

(defcustom ps-run-prompt "\\(GS\\(<[0-9]+\\)?>\\)+"
  "*Regexp to match prompt in interactive PostScript."
  :group 'PostScript-interaction
  :type 'regexp)

(defcustom ps-run-font-lock-keywords-2
  (append (unless (string= ps-run-prompt "")
	    (list (list (if (= ?^ (string-to-char ps-run-prompt))
			    ps-run-prompt
			  (concat "^" ps-run-prompt))
			'(0 font-lock-function-name-face nil nil))))
	  '((">>showpage, press <return> to continue<<"
	     (0 font-lock-keyword-face nil nil))
	    ("^\\(Error\\|Can't\\).*"
	     (0 font-lock-warning-face nil nil))
	    ("^\\(Current file position is\\) \\([0-9]+\\)"
	     (1 font-lock-comment-face nil nil)
	     (2 font-lock-warning-face nil nil))))
  "*Medium level highlighting of messages from the PostScript interpreter.

See documentation on font-lock for details."
  :group 'PostScript-interaction
  :type '(repeat (list :tag "Expression with one or more highlighters"
		       :value ("" (0 default nil t))
		       (regexp :tag "Expression")
		       (repeat :tag "Highlighters"
			       :inline regexp
			       (list :tag "Highlighter"
				     (integer :tag "Subexp")
				     face
				     (boolean :tag "Override")
				     (boolean :tag "Laxmatch" :value t))))))

(defcustom ps-run-x '("gs" "-r72" "-sPAPERSIZE=a4")
  "*Command as list to run PostScript with graphic display."
  :group 'PostScript-interaction
  :type '(repeat string))

(defcustom ps-run-dumb '("gs" "-dNODISPLAY")
  "*Command as list to run PostScript without graphic display."
  :group 'PostScript-interaction
  :type '(repeat string))

(defcustom ps-run-init nil
  "*String of commands to send to PostScript to start interactive.

Example: \"executive\"

You won't need to set this option for Ghostscript."
  :group 'PostScript-interaction
  :type '(choice (const nil) string))

(defcustom ps-run-error-line-numbers nil
  "*What values are used by the PostScript interpreter in error messages?"
  :group 'PostScript-interaction
  :type '(choice (const :tag "line numbers" t)
                 (const :tag "byte counts" nil)))

(defcustom ps-run-tmp-dir nil
  "*Name of directory to place temporary file.
If nil, use `temporary-file-directory'."
  :group 'PostScript-interaction
  :type '(choice (const nil) directory))


;; Constants used for font-lock.

;; Only a small set of the PostScript operators is selected for fontification.
;; Fontification is meant to clarify the document structure and process flow,
;; fontifying all known PostScript operators would hinder that objective.
(defconst ps-mode-operators
  (let ((ops '("clear" "mark" "cleartomark" "counttomark"
	       "forall"
	       "dict" "begin" "end" "def"
	       "true" "false"
	       "exec" "if" "ifelse" "for" "repeat" "loop" "exit"
	       "stop" "stopped" "countexecstack" "execstack"
	       "quit" "start"
	       "save" "restore"
	       "bind" "null"
	       "gsave" "grestore" "grestoreall"
	       "showpage")))
    (concat "\\<" (regexp-opt ops t) "\\>"))
  "Regexp of PostScript operators that will be fontified.")

;; Level 1 font-lock:
;;  - Special comments (reference face)
;;  - Strings and other comments
;;  - Partial strings (warning face)
;;  - 8bit characters (warning face)
;; Multiline strings are not supported. Strings with nested brackets are.
(defconst ps-mode-font-lock-keywords-1
  '(("\\`%!PS.*" . font-lock-reference-face)
    ("^%%BoundingBox:[ \t]+-?[0-9]+[ \t]+-?[0-9]+[ \t]+-?[0-9]+[ \t]+-?[0-9]+[ \t]*$"
     . font-lock-reference-face)
    (ps-mode-match-string-or-comment
     (1 font-lock-comment-face nil t)
     (2 font-lock-string-face nil t))
    ("([^()\n%]*\\|[^()\n]*)" . font-lock-warning-face)
    ("[\200-\377]+" (0 font-lock-warning-face prepend nil)))
  "Subdued level highlighting for PostScript mode.")

;; Level 2 font-lock:
;;  - All from level 1
;;  - PostScript operators (keyword face)
(defconst ps-mode-font-lock-keywords-2
  (append
   ps-mode-font-lock-keywords-1
   (list
    (cons
     ;; exclude names prepended by `/'
     (concat "\\(^\\|[^/\n]\\)" ps-mode-operators)
     '(2 font-lock-keyword-face))))
  "Medium level highlighting for PostScript mode.")

;; Level 3 font-lock:
;;  - All from level 2
;;  - Immediately evaluated names: those starting with `//' (type face)
;;  - Names that look like they are used for the definition of:
;;     * a function
;;     * an array
;;     * a dictionary
;;     * a "global" variable
;;    (function name face)
;;  - Other names (variable name face)
;; The rules used to determine what names fit in the first category are:
;;  - Only names that are at the left margin, and one of these on the same line:
;;     * Nothing after the name except possibly one or more `[' or a comment
;;     * A `{' or `<<' or `[0-9]+ dict' following the name
;;     * A `def' somewhere in the same line
;; Names are fontified before PostScript operators, allowing the use of
;; a more simple (efficient) regexp than the one used in level 2.
(defconst ps-mode-font-lock-keywords-3
  (append
   ps-mode-font-lock-keywords-1
   (list
    '("//\\w+" . font-lock-type-face)
    `(,(concat
	"^\\(/\\w+\\)\\>"
	"\\([[ \t]*\\(%.*\\)?\r?$"	; Nothing but `[' or comment after the name.
	"\\|[ \t]*\\({\\|<<\\)"		; `{' or `<<' following the name.
	"\\|[ \t]+[0-9]+[ \t]+dict\\>"	; `[0-9]+ dict' following the name.
	"\\|.*\\<def\\>\\)")		; `def' somewhere on the same line.
      . (1 font-lock-function-name-face))
    '("/\\w+" . font-lock-variable-name-face)
    (cons ps-mode-operators 'font-lock-keyword-face)))
  "High level highlighting for PostScript mode.")

(defconst ps-mode-font-lock-keywords ps-mode-font-lock-keywords-1
  "Default expressions to highlight in PostScript mode.")

;; Level 1 font-lock for ps-run-mode
;;  - prompt (function name face)
(defconst ps-run-font-lock-keywords-1
  (unless (string= "" ps-run-prompt)
    (list (cons (if (= ?^ (string-to-char ps-run-prompt))
		    ps-run-prompt
		  (concat "^" ps-run-prompt))
		'font-lock-function-name-face)))
  "Subdued level highlighting for PostScript run mode.")

(defconst ps-run-font-lock-keywords ps-run-font-lock-keywords-1
  "Default expressions to highlight in PostScript run mode.")


;; Variables.

(defvar ps-mode-map nil
  "Local keymap to use in PostScript mode.")

(defvar ps-mode-syntax-table nil
  "Syntax table used while in PostScript mode.")

(defvar ps-run-mode-map nil
  "Local keymap to use in PostScript run mode.")

(defvar ps-mode-tmp-file nil
  "Name of temporary file, set by `ps-run'.")

(defvar ps-run-mark nil
  "Mark to start of region that was sent to PostScript interpreter.")

(defvar ps-run-parent nil
  "Parent window of interactive PostScript.")


;; Menu

(defconst ps-mode-menu-main
  '("PostScript"
    ["EPSF Template, Sparse" ps-mode-epsf-sparse t]
    ["EPSF Template, Rich"   ps-mode-epsf-rich t]
    "---"
    ("Cookbook"
     ["RE" ps-mode-RE t]
     ["ISOLatin1Extended" ps-mode-latin-extended t]
     ["center" ps-mode-center t]
     ["right" ps-mode-right t]
     ["Heapsort" ps-mode-heapsort t])
    ("Fonts (1)"
     ["Times-Roman" (insert "/Times-Roman ") t]
     ["Times-Bold" (insert "/Times-Bold ") t]
     ["Times-Italic" (insert "/Times-Italic ") t]
     ["Times-BoldItalic" (insert "/Times-BoldItalic ") t]
     ["Helvetica" (insert "/Helvetica ") t]
     ["Helvetica-Bold" (insert "/Helvetica-Bold ") t]
     ["Helvetica-Oblique" (insert "/Helvetica-Oblique ") t]
     ["Helvetica-BoldOblique" (insert "/Helvetica-BoldOblique ") t]
     ["Courier" (insert "/Courier ") t]
     ["Courier-Bold" (insert "/Courier-Bold ") t]
     ["Courier-Oblique" (insert "/Courier-Oblique ") t]
     ["Courier-BoldOblique" (insert "/Courier-BoldOblique ") t]
     ["Symbol" (insert "/Symbol") t ])
    ("Fonts (2)"
     ["AvantGarde-Book" (insert "/AvantGarde-Book ") t]
     ["AvantGarde-Demi" (insert "/AvantGarde-Demi ") t]
     ["AvantGarde-BookOblique" (insert "/AvantGarde-BookOblique ") t]
     ["AvantGarde-DemiOblique" (insert "/AvantGarde-DemiOblique ") t]
     ["Bookman-Light" (insert "/Bookman-Light ") t]
     ["Bookman-Demi" (insert "/Bookman-Demi ") t]
     ["Bookman-LightItalic" (insert "/Bookman-LightItalic ") t]
     ["Bookman-DemiItalic" (insert "/Bookman-DemiItalic ") t]
     ["Helvetica-Narrow" (insert "/Helvetica-Narrow ") t]
     ["Helvetica-Narrow-Bold" (insert "/Helvetica-Narrow-Bold ") t]
     ["Helvetica-Narrow-Oblique" (insert "/Helvetica-Narrow-Oblique ") t]
     ["Helvetica-Narrow-BoldOblique" (insert "/Helvetica-Narrow-BoldOblique ") t]
     ["NewCenturySchlbk-Roman" (insert "/NewCenturySchlbk-Roman ") t]
     ["NewCenturySchlbk-Bold" (insert "/NewCenturySchlbk-Bold ") t]
     ["NewCenturySchlbk-Italic" (insert "/NewCenturySchlbk-Italic ") t]
     ["NewCenturySchlbk-BoldItalic" (insert "/NewCenturySchlbk-BoldItalic ") t]
     ["Palatino-Roman" (insert "/Palatino-Roman ") t]
     ["Palatino-Bold" (insert "/Palatino-Bold ") t]
     ["Palatino-Italic" (insert "/Palatino-Italic ") t]
     ["Palatino-BoldItalic" (insert "/Palatino-BoldItalic ") t]
     ["ZapfChancery-MediumItalic" (insert "/ZapfChancery-MediumItalic ") t]
     ["ZapfDingbats" (insert "/ZapfDingbats ") t])
    "---"
    ["Comment Out Region" ps-mode-comment-out-region (mark t)]
    ["Uncomment Region" ps-mode-uncomment-region (mark t)]
    "---"
    ["8-bit to Octal Buffer" ps-mode-octal-buffer t]
    ["8-bit to Octal Region" ps-mode-octal-region (mark t)]
    "---"
    ["Auto Indent" (setq ps-mode-auto-indent (not ps-mode-auto-indent))
     :style toggle :selected ps-mode-auto-indent]
    "---"
    ["Start PostScript"
     ps-run-start
     t]
    ["Quit PostScript" ps-run-quit (process-status "ps-run")]
    ["Kill PostScript" ps-run-kill (process-status "ps-run")]
    ["Send Buffer to Interpreter"
     ps-run-buffer
     (process-status "ps-run")]
    ["Send Region to Interpreter"
     ps-run-region
     (and (mark t) (process-status "ps-run"))]
    ["Send Newline to Interpreter"
     ps-mode-other-newline
     (process-status "ps-run")]
    ["View BoundingBox"
     ps-run-boundingbox
     (process-status "ps-run")]
    ["Clear/Reset PostScript Graphics"
     ps-run-clear
     (process-status "ps-run")]
    "---"
    ["Print Buffer as PostScript"
     ps-mode-print-buffer
     t]
    ["Print Region as PostScript"
     ps-mode-print-region
     (mark t)]
    "---"
    ["Customize for PostScript"
     (customize-group "PostScript")
     t]
    "---"
    ["Submit Bug Report"
     ps-mode-submit-bug-report
     t]))


;; Mode maps for PostScript edit mode and PostScript interaction mode.

(unless ps-mode-map
  (setq ps-mode-map (make-sparse-keymap))
  (define-key ps-mode-map "\C-c\C-v" 'ps-run-boundingbox)
  (define-key ps-mode-map "\C-c\C-u" 'ps-mode-uncomment-region)
  (define-key ps-mode-map "\C-c\C-t" 'ps-mode-epsf-rich)
  (define-key ps-mode-map "\C-c\C-s" 'ps-run-start)
  (define-key ps-mode-map "\C-c\C-r" 'ps-run-region)
  (define-key ps-mode-map "\C-c\C-q" 'ps-run-quit)
  (define-key ps-mode-map "\C-c\C-p" 'ps-mode-print-buffer)
  (define-key ps-mode-map "\C-c\C-o" 'ps-mode-comment-out-region)
  (define-key ps-mode-map "\C-c\C-k" 'ps-run-kill)
  (define-key ps-mode-map "\C-c\C-j" 'ps-mode-other-newline)
  (define-key ps-mode-map "\C-c\C-l" 'ps-run-clear)
  (define-key ps-mode-map "\C-c\C-b" 'ps-run-buffer)
  (define-key ps-mode-map ">" 'ps-mode-r-gt)
  (define-key ps-mode-map "]" 'ps-mode-r-angle)
  (define-key ps-mode-map "}" 'ps-mode-r-brace)
  (define-key ps-mode-map "\177" 'ps-mode-backward-delete-char)
  (define-key ps-mode-map "\t" 'ps-mode-tabkey)
  (define-key ps-mode-map "\r" 'ps-mode-newline)
  (define-key ps-mode-map [return] 'ps-mode-newline)
  (easy-menu-define ps-mode-main ps-mode-map "PostScript" ps-mode-menu-main))

(unless ps-run-mode-map
  (setq ps-run-mode-map (make-sparse-keymap))
  (set-keymap-parent ps-run-mode-map comint-mode-map)
  (define-key ps-run-mode-map "\C-c\C-q" 'ps-run-quit)
  (define-key ps-run-mode-map "\C-c\C-k" 'ps-run-kill)
  (define-key ps-run-mode-map "\C-c\C-e" 'ps-run-goto-error)
  (define-key ps-run-mode-map [mouse-2] 'ps-run-mouse-goto-error))


;; Syntax table.

(unless ps-mode-syntax-table
  (setq ps-mode-syntax-table (make-syntax-table))

  (modify-syntax-entry ?\% "< " ps-mode-syntax-table)
  (modify-syntax-entry ?\n "> " ps-mode-syntax-table)
  (modify-syntax-entry ?\r "> " ps-mode-syntax-table)
  (modify-syntax-entry ?\f "> " ps-mode-syntax-table)
  (modify-syntax-entry ?\< "(>" ps-mode-syntax-table)
  (modify-syntax-entry ?\> ")<" ps-mode-syntax-table)

  (modify-syntax-entry ?\! "w " ps-mode-syntax-table)
  (modify-syntax-entry ?\" "w " ps-mode-syntax-table)
  (modify-syntax-entry ?\# "w " ps-mode-syntax-table)
  (modify-syntax-entry ?\$ "w " ps-mode-syntax-table)
  (modify-syntax-entry ?\& "w " ps-mode-syntax-table)
  (modify-syntax-entry ?\' "w " ps-mode-syntax-table)
  (modify-syntax-entry ?\* "w " ps-mode-syntax-table)
  (modify-syntax-entry ?\+ "w " ps-mode-syntax-table)
  (modify-syntax-entry ?\, "w " ps-mode-syntax-table)
  (modify-syntax-entry ?\- "w " ps-mode-syntax-table)
  (modify-syntax-entry ?\. "w " ps-mode-syntax-table)
  (modify-syntax-entry ?\: "w " ps-mode-syntax-table)
  (modify-syntax-entry ?\; "w " ps-mode-syntax-table)
  (modify-syntax-entry ?\= "w " ps-mode-syntax-table)
  (modify-syntax-entry ?\? "w " ps-mode-syntax-table)
  (modify-syntax-entry ?\@ "w " ps-mode-syntax-table)
  (modify-syntax-entry ?\\ "w " ps-mode-syntax-table)
  (modify-syntax-entry ?^  "w " ps-mode-syntax-table) ; NOT: ?\^
  (modify-syntax-entry ?\_ "w " ps-mode-syntax-table)
  (modify-syntax-entry ?\` "w " ps-mode-syntax-table)
  (modify-syntax-entry ?\| "w " ps-mode-syntax-table)
  (modify-syntax-entry ?\~ "w " ps-mode-syntax-table)

  (let ((i 128))
    (while (< i 256)
      (modify-syntax-entry i "w " ps-mode-syntax-table)
      (setq i (1+ i)))))



(declare-function doc-view-minor-mode "doc-view")

;; PostScript mode.

;;;###autoload
(define-derived-mode ps-mode prog-mode "PostScript"
  "Major mode for editing PostScript with GNU Emacs.

Entry to this mode calls `ps-mode-hook'.

The following variables hold user options, and can
be set through the `customize' command:

  `ps-mode-auto-indent'
  `ps-mode-tab'
  `ps-mode-paper-size'
  `ps-mode-print-function'
  `ps-run-prompt'
  `ps-run-font-lock-keywords-2'
  `ps-run-x'
  `ps-run-dumb'
  `ps-run-init'
  `ps-run-error-line-numbers'
  `ps-run-tmp-dir'

Type \\[describe-variable] for documentation on these options.


\\{ps-mode-map}


When starting an interactive PostScript process with \\[ps-run-start],
a second window will be displayed, and `ps-run-mode-hook' will be called.
The keymap for this second window is:

\\{ps-run-mode-map}


When Ghostscript encounters an error it displays an error message
with a file position. Clicking mouse-2 on this number will bring
point to the corresponding spot in the PostScript window, if input
to the interpreter was sent from that window.
Typing \\<ps-run-mode-map>\\[ps-run-goto-error] when the cursor is at the number has the same effect."
  (set (make-local-variable 'font-lock-defaults)
       '((ps-mode-font-lock-keywords
 	  ps-mode-font-lock-keywords-1
 	  ps-mode-font-lock-keywords-2
 	  ps-mode-font-lock-keywords-3)
	 t))
  (set (make-local-variable 'comment-start) "%")
  ;; NOTE: `\' has a special meaning in strings only
  (set (make-local-variable 'comment-start-skip) "%+[ \t]*")
  ;; enable doc-view-minor-mode => C-c C-c starts viewing the current ps file
  ;; with doc-view-mode.
  (doc-view-minor-mode 1))

(defun ps-mode-show-version ()
  "Show current version of PostScript mode."
  (interactive)
  (message " *** PostScript Mode (ps-mode) Version %s *** " ps-mode-version))

;; From reporter.el
(defvar reporter-prompt-for-summary-p)
(defvar reporter-dont-compact-list)

(defun ps-mode-submit-bug-report ()
  "Submit via mail a bug report on PostScript mode."
  (interactive)
  (when (y-or-n-p "Submit bug report on PostScript mode? ")
    (let ((reporter-prompt-for-summary-p nil)
	  (reporter-dont-compact-list '(ps-mode-print-function
					ps-run-font-lock-keywords-2)))
      (reporter-submit-bug-report
       ps-mode-maintainer-address
       (format "ps-mode.el %s [%s]" ps-mode-version system-type)
       '(ps-mode-auto-indent
	 ps-mode-tab
	 ps-mode-paper-size
	 ps-mode-print-function
	 ps-run-prompt
	 ps-run-font-lock-keywords-2
	 ps-run-x
	 ps-run-dumb
	 ps-run-init
	 ps-run-error-line-numbers
	 ps-run-tmp-dir)))))


;; Helper functions for font-lock.

;; When this function is called, point is at an opening bracket.
;; This function should test if point is at the start of a string
;; with nested brackets.
;; If true:  move point to end of string
;;           set string to match data nr 2
;;           return new point
;; If false: return nil
(defun ps-mode-looking-at-nested (limit)
  (let ((first (point))
	(level 1)
	pos)
    ;; Move past opening bracket.
    (forward-char 1)
    (setq pos (point))
    (while (and (> level 0) (< pos limit))
      ;; Search next bracket, stepping over escaped brackets.
      (if (not (looking-at "\\([^()\\\n]\\|\\\\.\\)*\\([()]\\)"))
          (setq level -1)
	(setq level (+ level (if (string= "(" (match-string 2)) 1 -1)))
	(goto-char (setq pos (match-end 0)))))
    (if (not (= level 0))
        nil
      ;; Found string with nested brackets, now set match data nr 2.
      (set-match-data (list first pos nil nil first pos))
      pos)))

;; This function should search for a string or comment
;; If comment, return as match data nr 1
;; If string, return as match data nr 2
(defun ps-mode-match-string-or-comment (limit)
  ;; Find the first potential match.
  (if (not (re-search-forward "[%(]" limit t))
      ;; Nothing found: return failure.
      nil
    (let ((end (match-end 0)))
      (goto-char (match-beginning 0))
      (cond ((looking-at "\\(%.*\\)\\|\\((\\([^()\\\n]\\|\\\\.\\)*)\\)")
	     ;; It's a comment or string without nested, unescaped brackets.
	     (goto-char (match-end 0))
	     (point))
	    ((ps-mode-looking-at-nested limit)
	     ;; It's a string with nested brackets.
	     (point))
	    (t
	     ;; Try next match.
	     (goto-char end)
	     (ps-mode-match-string-or-comment limit))))))


;; Key-handlers.

(defun ps-mode-target-column ()
  "To what column should text on current line be indented?

Indentation is increased if the last token on the current line
defines the beginning of a group. These tokens are:  {  [  <<"
  (save-excursion
    (beginning-of-line)
    (if (looking-at "[ \t]*\\(}\\|\\]\\|>>\\)")
	(condition-case err
	    (progn
	      (goto-char (match-end 0))
	      (backward-sexp 1)
	      (beginning-of-line)
	      (if (looking-at "[ \t]+")
		  (goto-char (match-end 0)))
	      (current-column))
	  (error
	   (ding)
	   (message "%s" (error-message-string err))
	   0))
      (let (target)
	(if (not (re-search-backward "[^ \t\n\r\f][ \t\n\r\f]*\\=" nil t))
	    0
	  (goto-char (match-beginning 0))
	  (beginning-of-line)
	  (if (looking-at "[ \t]+")
	      (goto-char (match-end 0)))
	  (setq target (current-column))
	  (end-of-line)
	  (if (re-search-backward "\\({\\|\\[\\|<<\\)[ \t]*\\(%[^\n]*\\)?\\=" nil t)
	      (setq target (+ target ps-mode-tab)))
	  target)))))

(defun ps-mode-newline ()
  "Insert newline with proper indentation."
  (interactive)
  (delete-horizontal-space)
  (insert "\n")
  (if ps-mode-auto-indent
      (indent-to (ps-mode-target-column))))

(defun ps-mode-tabkey ()
  "Indent/reindent current line, or insert tab."
  (interactive)
  (let ((column (current-column))
	target)
    (if (or (not ps-mode-auto-indent)
	    (< ps-mode-tab 1)
	    (not (re-search-backward "^[ \t]*\\=" nil t)))
	(insert "\t")
      (setq target (ps-mode-target-column))
      (while (<= target column)
	(setq target (+ target ps-mode-tab)))
      (indent-line-to target))))

(defun ps-mode-backward-delete-char ()
  "Delete backward indentation, or delete backward character."
  (interactive)
  (let ((column (current-column))
	target)
    (if (or (not ps-mode-auto-indent)
	    (< ps-mode-tab 1)
	    (not (re-search-backward "^[ \t]+\\=" nil t)))
	(call-interactively 'delete-backward-char)
      (setq target (ps-mode-target-column))
      (while (> column target)
	(setq target (+ target ps-mode-tab)))
      (while (>= target column)
	(setq target (- target ps-mode-tab)))
      (if (< target 0)
	  (setq target 0))
      (indent-line-to target))))

(defun ps-mode-r-brace ()
  "Insert `}' and perform balance."
  (interactive)
  (insert "}")
  (ps-mode-r-balance "}"))

(defun ps-mode-r-angle ()
  "Insert `]' and perform balance."
  (interactive)
  (insert "]")
  (ps-mode-r-balance "]"))

(defun ps-mode-r-gt ()
  "Insert `>' and perform balance."
  (interactive)
  (insert ">")
  (ps-mode-r-balance ">>"))

(defun ps-mode-r-balance (right)
  "Adjust indenting if point after RIGHT."
  (if ps-mode-auto-indent
      (save-excursion
	(when (re-search-backward (concat "^[ \t]*" (regexp-quote right) "\\=") nil t)
	  (indent-line-to (ps-mode-target-column)))))
  (blink-matching-open))

(defun ps-mode-other-newline ()
  "Perform newline in `*ps-run*' buffer."
  (interactive)
  (ps-run-send-string ""))


;; Print PostScript.

(defun ps-mode-print-buffer ()
  "Print buffer as PostScript."
  (interactive)
  (funcall ps-mode-print-function))

(defun ps-mode-print-region (begin end)
  "Print region as PostScript, adding minimal header and footer lines:

%!PS
<region>
showpage"
  (interactive "r")
  (let ((buf (current-buffer)))
    (with-temp-buffer
      (insert "%!PS\n")
      (insert-buffer-substring buf begin end)
      (insert "\nshowpage\n")
      (funcall ps-mode-print-function))))


;; Comment Out / Uncomment.

(defun ps-mode-comment-out-region (begin end)
  "Comment out region."
  (interactive "r")
  (let ((endm (make-marker)))
    (set-marker endm end)
    (save-excursion
      (goto-char begin)
      (if (= (current-column) 0)
	  (insert "%"))
      (while (and (= (forward-line) 0)
		  (< (point) (marker-position endm)))
	(insert "%")))
    (set-marker endm nil)))

(defun ps-mode-uncomment-region (begin end)
  "Uncomment region.

Only one `%' is removed, and it has to be in the first column."
  (interactive "r")
  (let ((endm (make-marker)))
    (set-marker endm end)
    (save-excursion
      (goto-char begin)
      (if (looking-at "^%")
	  (delete-char 1))
      (while (and (= (forward-line) 0)
		  (< (point) (marker-position endm)))
	(if (looking-at "%")
	    (delete-char 1))))
    (set-marker endm nil)))


;; Convert 8-bit to octal codes.

(defun ps-mode-octal-buffer ()
  "Change 8-bit characters to octal codes in buffer."
  (interactive)
  (ps-mode-octal-region (point-min) (point-max)))

(defun ps-mode-octal-region (begin end)
  "Change 8-bit characters to octal codes in region."
  (interactive "r")
  (if buffer-read-only
      (progn
	(ding)
	(message "Buffer is read only"))
    (save-excursion
      (let (endm i)
        (setq endm (make-marker))
        (set-marker endm end)
        (goto-char begin)
        (setq i 0)
        (while (re-search-forward "[\200-\377]" (marker-position endm) t)
          (setq i (1+ i))
          (backward-char)
          (insert (format "\\%03o" (string-to-char (buffer-substring (point) (1+ (point))))))
          (delete-char 1))
        (message "%d change%s made" i (if (= i 1) "" "s"))
        (set-marker endm nil)))))


;; Cookbook.

(defun ps-mode-center ()
  "Insert function /center."
  (interactive)
  (insert "
/center {
    dup stringwidth
    exch 2 div neg
    exch 2 div neg
    rmoveto
} bind def
"))

(defun ps-mode-right ()
  "Insert function /right."
  (interactive)
  (insert "
/right {
    dup stringwidth
    exch neg
    exch neg
    rmoveto
} bind def
"))

(defun ps-mode-RE ()
  "Insert function /RE."
  (interactive)
  (insert "
% `new-font-name' `encoding-vector' `old-font-name' RE -
/RE {
    findfont
    dup maxlength dict begin {
        1 index /FID ne { def } { pop pop } ifelse
    } forall
    /Encoding exch def
    dup /FontName exch def
    currentdict end definefont pop
} bind def
"))

(defun ps-mode-latin-extended ()
  "Insert array /ISOLatin1Extended.

This encoding vector contains all the entries from ISOLatin1Encoding
plus the usually uncoded characters inserted on positions 1 through 28."
  (interactive)
  (insert "
% ISOLatin1Encoding, extended with remaining uncoded glyphs
/ISOLatin1Extended [
    /.notdef /Lslash /lslash /OE /oe /Scaron /scaron /Zcaron /zcaron
    /Ydieresis /trademark /bullet /dagger /daggerdbl /ellipsis /emdash
    /endash /fi /fl /florin /fraction /guilsinglleft /guilsinglright
    /perthousand /quotedblbase /quotedblleft /quotedblright
    /quotesinglbase /quotesingle /.notdef /.notdef /.notdef /space
    /exclam /quotedbl /numbersign /dollar /percent /ampersand
    /quoteright /parenleft /parenright /asterisk /plus /comma /minus
    /period /slash /zero /one /two /three /four /five /six /seven /eight
    /nine /colon /semicolon /less /equal /greater /question /at /A /B /C
    /D /E /F /G /H /I /J /K /L /M /N /O /P /Q /R /S /T /U /V /W /X /Y /Z
    /bracketleft /backslash /bracketright /asciicircum /underscore
    /quoteleft /a /b /c /d /e /f /g /h /i /j /k /l /m /n /o /p /q /r /s
    /t /u /v /w /x /y /z /braceleft /bar /braceright /asciitilde
    /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
    /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef
    /.notdef /.notdef /.notdef /dotlessi /grave /acute /circumflex
    /tilde /macron /breve /dotaccent /dieresis /.notdef /ring /cedilla
    /.notdef /hungarumlaut /ogonek /caron /space /exclamdown /cent
    /sterling /currency /yen /brokenbar /section /dieresis /copyright
    /ordfeminine /guillemotleft /logicalnot /hyphen /registered /macron
    /degree /plusminus /twosuperior /threesuperior /acute /mu /paragraph
    /periodcentered /cedilla /onesuperior /ordmasculine /guillemotright
    /onequarter /onehalf /threequarters /questiondown /Agrave /Aacute
    /Acircumflex /Atilde /Adieresis /Aring /AE /Ccedilla /Egrave /Eacute
    /Ecircumflex /Edieresis /Igrave /Iacute /Icircumflex /Idieresis /Eth
    /Ntilde /Ograve /Oacute /Ocircumflex /Otilde /Odieresis /multiply
    /Oslash /Ugrave /Uacute /Ucircumflex /Udieresis /Yacute /Thorn
    /germandbls /agrave /aacute /acircumflex /atilde /adieresis /aring
    /ae /ccedilla /egrave /eacute /ecircumflex /edieresis /igrave
    /iacute /icircumflex /idieresis /eth /ntilde /ograve /oacute
    /ocircumflex /otilde /odieresis /divide /oslash /ugrave /uacute
    /ucircumflex /udieresis /yacute /thorn /ydieresis
] def
"))

(defun ps-mode-heapsort ()
  "Insert function /Heapsort."
  (interactive)
  (insert "
% `array-element' Heapsort-cvi-or-cvr-or-cvs `number-or-string'
/Heapsort-cvi-or-cvr-or-cvs {
    % 0 get
} bind def
% `array' Heapsort `sorted-array'
/Heapsort {
    dup length /hsR exch def
    /hsL hsR 2 idiv 1 add def
    {
        hsR 2 lt { exit } if
        hsL 1 gt {
            /hsL hsL 1 sub def
        } {
            /hsR hsR 1 sub def
            dup dup dup 0 get exch dup hsR get
            0 exch put
            hsR exch put
        } ifelse
        dup hsL 1 sub get /hsT exch def
        /hsJ hsL def
        {
            /hsS hsJ def
            /hsJ hsJ dup add def
            hsJ hsR gt { exit } if
            hsJ hsR lt {
                dup dup hsJ 1 sub get Heapsort-cvi-or-cvr-or-cvs
                exch hsJ get Heapsort-cvi-or-cvr-or-cvs
                lt { /hsJ hsJ 1 add def } if
            } if
            dup hsJ 1 sub get Heapsort-cvi-or-cvr-or-cvs
            hsT Heapsort-cvi-or-cvr-or-cvs
            le { exit } if
            dup dup hsS 1 sub exch hsJ 1 sub get put
        } loop
        dup hsS 1 sub hsT put
    } loop
} bind def
"))


;; EPSF document lay-out.

(defun ps-mode-epsf-sparse ()
  "Insert sparse EPSF template."
  (interactive)
  (goto-char (point-max))
  (unless (re-search-backward "%%EOF[ \t\n]*\\'" nil t)
    (goto-char (point-max))
    (insert "\n%%EOF\n"))
  (goto-char (point-max))
  (unless (re-search-backward "\\bshowpage[ \t\n]+%%EOF[ \t\n]*\\'" nil t)
    (re-search-backward "%%EOF")
    (insert "showpage\n"))
  (goto-char (point-max))
  (unless (re-search-backward "\\bend[ \t\n]+\\bshowpage[ \t\n]+%%EOF[ \t\n]*\\'" nil t)
    (re-search-backward "showpage")
    (insert "\nend\n"))
  (goto-char (point-min))
  (insert "%!PS-Adobe-3.0 EPSF-3.0\n%%BoundingBox: 0 0 ")
  (insert (format "%d %d\n\n"
		  (car ps-mode-paper-size)
		  (car (cdr ps-mode-paper-size))))
  (insert "64 dict begin\n\n"))

(defun ps-mode-epsf-rich ()
  "Insert rich EPSF template."
  (interactive)
  (ps-mode-epsf-sparse)
  (forward-line -3)
  (when buffer-file-name
    (insert "%%Title: " (file-name-nondirectory buffer-file-name) "\n"))
  (insert "%%Creator: " (user-full-name) "\n")
  (insert "%%CreationDate: " (current-time-string) "\n")
  (insert "%%EndComments\n")
  (forward-line 3))


;; Interactive PostScript interpreter.

(define-derived-mode ps-run-mode comint-mode "Interactive PS"
  "Major mode in interactive PostScript window.
This mode is invoked from `ps-mode' and should not be called directly."
  (set (make-local-variable 'font-lock-defaults)
       '((ps-run-font-lock-keywords
	  ps-run-font-lock-keywords-1
	  ps-run-font-lock-keywords-2)
	 t))
  (setq mode-line-process '(":%s")))

(defun ps-run-running ()
  "Error if not in `ps-mode' or not running PostScript."
  (unless (derived-mode-p 'ps-mode)
    (error "This function can only be called from PostScript mode"))
  (unless (equal (process-status "ps-run") 'run)
    (error "No PostScript process running")))

(defun ps-run-start ()
  "Start interactive PostScript."
  (interactive)
  (let ((command (or (and window-system ps-run-x) ps-run-dumb))
	(init-file nil)
	(process-connection-type nil)
	(oldwin (selected-window)))
    (unless command
      (error "No command specified to run interactive PostScript"))
    (unless (and ps-run-mark (markerp ps-run-mark))
      (setq ps-run-mark (make-marker)))
    (when ps-run-init
      (setq init-file (ps-run-make-tmp-filename))
      (write-region (concat ps-run-init "\n") 0 init-file)
      (setq init-file (list init-file)))
    (pop-to-buffer "*ps-run*")
    (ps-run-mode)
    (when (process-status "ps-run")
      (delete-process "ps-run"))
    (erase-buffer)
    (setq command (append command init-file))
    (insert (mapconcat 'identity command " ") "\n")
    (apply 'make-comint "ps-run" (car command) nil (cdr command))
    (with-current-buffer "*ps-run*"
      (use-local-map ps-run-mode-map)
      (setq comint-prompt-regexp ps-run-prompt))
    (select-window oldwin)))

(defun ps-run-quit ()
  "Quit interactive PostScript."
  (interactive)
  (ps-run-send-string "quit")
  (ps-run-cleanup))

(defun ps-run-kill ()
  "Kill interactive PostScript."
  (interactive)
  (delete-process "ps-run")
  (ps-run-cleanup))

(defun ps-run-clear ()
  "Clear/reset PostScript graphics."
  (interactive)
  (ps-run-send-string "showpage")
  (sit-for 1)
  (ps-run-send-string ""))

(defun ps-run-buffer ()
  "Send buffer to PostScript interpreter."
  (interactive)
  (ps-run-region (point-min) (point-max)))

(defun ps-run-region (begin end)
  "Send region to PostScript interpreter."
  (interactive "r")
  (ps-run-running)
  (setq ps-run-parent (buffer-name))
  (let ((f (ps-run-make-tmp-filename)))
    (set-marker ps-run-mark begin)
    (write-region begin end f)
    (ps-run-send-string (format "(%s) run" f))))

(defun ps-run-boundingbox ()
  "View BoundingBox."
  (interactive)
  (ps-run-running)
  (let (x1 y1 x2 y2 f
	   (buf (current-buffer)))
    (save-excursion
      (goto-char 1)
      (re-search-forward
       "^%%BoundingBox:[ \t]+\\(-?[0-9]+\\)[ \t]+\\(-?[0-9]+\\)[ \t]+\\(-?[0-9]+\\)[ \t]+\\(-?[0-9]+\\)")
      (setq x1 (match-string 1)
            y1 (match-string 2)
            x2 (match-string 3)
            y2 (match-string 4)))
    (unless (< (string-to-number x1) (string-to-number x2))
      (error "x1 (%s) should be less than x2 (%s)" x1 x2))
    (unless (< (string-to-number y1) (string-to-number y2))
      (error "y1 (%s) should be less than y2 (%s)" y1 y2))
    (setq f (ps-run-make-tmp-filename))
    (write-region
     (format
      "gsave
    initgraphics
    2 setlinewidth
    %s %s moveto
    %s %s lineto
    %s %s lineto
    %s %s lineto
    closepath
    gsave
        [ 4 20 ] 0 setdash
        1 0 0 setrgbcolor
        stroke
    grestore
    gsave
        [ 4 20 ] 8 setdash
        0 1 0 setrgbcolor
        stroke
    grestore
    [ 4 20 ] 16 setdash
    0 0 1 setrgbcolor
    stroke
grestore
" x1 y1 x2 y1 x2 y2 x1 y2)
     0
     f)
    (ps-run-send-string (format "(%s) run" f))
    (set-buffer buf)))

(defun ps-run-send-string (string)
  (let ((oldwin (selected-window)))
    (pop-to-buffer "*ps-run*")
    (comint-goto-process-mark)
    (insert string)
    (comint-send-input)
    (select-window oldwin)))

(defun ps-run-make-tmp-filename ()
  (unless ps-mode-tmp-file
    (setq ps-mode-tmp-file
	  (let ((temporary-file-directory (or ps-run-tmp-dir
					      temporary-file-directory)))
	    (make-temp-file "ps-run-"))))
  ps-mode-tmp-file)

;; Remove temporary file
;; This shouldn't fail twice, because it is called at kill-emacs
(defun ps-run-cleanup ()
  (when ps-mode-tmp-file
    (let ((i ps-mode-tmp-file))
      (setq ps-mode-tmp-file nil)
      (when (file-exists-p i)
	(delete-file i)))))

(defun ps-run-mouse-goto-error (event)
  "Set point at mouse click, then call `ps-run-goto-error'."
  (interactive "e")
  (mouse-set-point event)
  (ps-run-goto-error))

(defun ps-run-goto-error ()
  "Jump to buffer position read as integer at point.
Use line numbers if `ps-run-error-line-numbers' is not nil"
  (interactive)
  (let ((p (point)))
    (unless (looking-at "[0-9]")
      (goto-char (max 1 (1- (point)))))
    (when (looking-at "[0-9]")
      (forward-char 1)
      (forward-word -1)
      (when (looking-at "[0-9]+")
	(let (i)
	  (setq
	   i
	   (string-to-number
	    (buffer-substring (match-beginning 0) (match-end 0))))
	  (goto-char p)
	  (pop-to-buffer ps-run-parent)
	  (if ps-run-error-line-numbers
	      (progn
		(goto-char (marker-position ps-run-mark))
		(forward-line (1- i))
		(end-of-line))
	    (goto-char (+ i (marker-position ps-run-mark)))))))))


;;
(add-hook 'kill-emacs-hook 'ps-run-cleanup)

(provide 'ps-mode)

;;; ps-mode.el ends here
