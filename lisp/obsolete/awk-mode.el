;;; awk-mode.el --- AWK code editing commands for Emacs

;; Copyright (C) 1988, 1994, 1996, 2000-2012 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: unix, languages
;; Obsolete-since: 22.1

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

;; Sets up C-mode with support for awk-style #-comments and a lightly
;; hacked syntax table.

;;; Code:

(defvar awk-mode-syntax-table
  (let ((st (make-syntax-table)))
  (modify-syntax-entry ?\\ "\\" st)
  (modify-syntax-entry ?\n ">   " st)
  (modify-syntax-entry ?\f ">   " st)
  (modify-syntax-entry ?\# "<   " st)
  ;; / can delimit regexes or be a division operator.  We assume that it is
  ;; more commonly used for regexes and fix the remaining cases with
  ;; `font-lock-syntactic-keywords'.
  (modify-syntax-entry ?/ "\"" st)
  (modify-syntax-entry ?* "." st)
  (modify-syntax-entry ?+ "." st)
  (modify-syntax-entry ?- "." st)
  (modify-syntax-entry ?= "." st)
  (modify-syntax-entry ?% "." st)
  (modify-syntax-entry ?< "." st)
  (modify-syntax-entry ?> "." st)
  (modify-syntax-entry ?& "." st)
  (modify-syntax-entry ?| "." st)
  (modify-syntax-entry ?_ "_" st)
  (modify-syntax-entry ?\' "\"" st)
  st)
  "Syntax table in use in `awk-mode' buffers.")

;; Regexps written with help from Peter Galbraith <galbraith@mixing.qc.dfo.ca>.
(defconst awk-font-lock-keywords
  (eval-when-compile
    (list
     ;;
     ;; Function names.
     '("^[ \t]*\\(function\\)\\>[ \t]*\\(\\sw+\\)?"
       (1 font-lock-keyword-face) (2 font-lock-function-name-face nil t))
     ;;
     ;; Variable names.
     (cons (regexp-opt
	    '("ARGC" "ARGIND" "ARGV" "CONVFMT" "ENVIRON" "ERRNO"
	      "FIELDWIDTHS" "FILENAME" "FNR" "FS" "IGNORECASE" "NF" "NR"
	      "OFMT" "OFS" "ORS" "RLENGTH" "RS" "RSTART" "SUBSEP") 'words)
	   'font-lock-variable-name-face)
     ;;
     ;; Keywords.
     (regexp-opt
      '("BEGIN" "END" "break" "continue" "delete" "do" "exit" "else" "for"
	"getline" "if" "next" "print" "printf" "return" "while") 'words)
     ;;
     ;; Builtins.
     (list (regexp-opt
	    '("atan2" "close" "cos" "ctime" "exp" "gsub" "index" "int"
	      "length" "log" "match" "rand" "sin" "split" "sprintf"
	      "sqrt" "srand" "sub" "substr" "system" "time"
	      "tolower" "toupper") 'words)
	   1 'font-lock-builtin-face)
     ;;
     ;; Operators.  Is this too much?
     (cons (regexp-opt '("&&" "||" "<=" "<" ">=" ">" "==" "!=" "!~" "~"))
	   'font-lock-constant-face)
     ))
 "Default expressions to highlight in AWK mode.")

(require 'syntax)

(defconst awk-font-lock-syntactic-keywords
  ;; `/' is mostly used for /.../ regular expressions, but is also
  ;; used as a division operator.  Distinguishing between the two is
  ;; a pain in the youknowwhat.
  ;; '(("\\(^\\|[<=>-+*%/!^,~(?:|&]\\)\\s-*\\(/\\)\\([^/\n\\]\\|\\\\.\\)*\\(/\\)"
  ;;    (2 "\"") (4 "\"")))
  '(("[^<=>-+*%/!^,~(?:|& \t\n\f]\\s-*\\(/\\)"
     (1 (unless (nth 3 (syntax-ppss (match-beginning 1))) "."))))
  "Syntactic keywords for `awk-mode'.")

;; No longer autoloaded since it might clobber the autoload directive in CC Mode.
(define-derived-mode awk-mode c-mode "AWK"
  "Major mode for editing AWK code.
This is much like C mode except for the syntax of comments.  Its keymap
inherits from C mode's and it has the same variables for customizing
indentation.  It has its own abbrev table and its own syntax table.

Turning on AWK mode runs `awk-mode-hook'."
  (set (make-local-variable 'paragraph-start) (concat "$\\|" page-delimiter))
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "#+ *")
  (setq font-lock-defaults '(awk-font-lock-keywords
			     nil nil ((?_ . "w")) nil
			     (parse-sexp-lookup-properties . t)
			     (font-lock-syntactic-keywords
			      . awk-font-lock-syntactic-keywords))))

(provide 'awk-mode)

;;; awk-mode.el ends here
