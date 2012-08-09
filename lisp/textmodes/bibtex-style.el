;;; bibtex-style.el --- Major mode for BibTeX Style files -*- lexical-binding: t -*-

;; Copyright (C) 2005, 2007-2012  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: tex

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

;; Done: font-lock, imenu, outline, commenting, indentation.
;; Todo: tab-completion.
;; Bugs:

;;; Code:

(defvar bibtex-style-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?%  "<" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?.  "_" st)
    (modify-syntax-entry ?'  "'" st)
    (modify-syntax-entry ?#  "'" st)
    (modify-syntax-entry ?*  "." st)
    (modify-syntax-entry ?=  "." st)
    (modify-syntax-entry ?$  "_" st)
    st))


(defconst bibtex-style-commands
  '("ENTRY" "EXECUTE" "FUNCTION" "INTEGERS" "ITERATE" "MACRO" "READ"
    "REVERSE" "SORT" "STRINGS"))

(defconst bibtex-style-functions
  ;; From http://www.eeng.dcu.ie/local-docs/btxdocs/btxhak/btxhak/node4.html.
  '("<" ">" "=" "+" "-" "*" ":="
    "add.period$" "call.type$" "change.case$" "chr.to.int$" "cite$"
    "duplicate$" "empty$" "format.name$" "if$" "int.to.chr$" "int.to.str$"
    "missing$" "newline$" "num.names$" "pop$" "preamble$" "purify$" "quote$"
    "skip$" "stack$" "substring$" "swap$" "text.length$" "text.prefix$"
    "top$" "type$" "warning$" "while$" "width$" "write$"))

(defvar bibtex-style-font-lock-keywords
  `((,(regexp-opt bibtex-style-commands 'words) . font-lock-keyword-face)
    ("\\w+\\$" . font-lock-keyword-face)
    ("\\<\\(FUNCTION\\|MACRO\\)\\s-+{\\([^}\n]+\\)}"
     (2 font-lock-function-name-face))))

;;;###autoload
(define-derived-mode bibtex-style-mode nil "BibStyle"
  "Major mode for editing BibTeX style files."
  (set (make-local-variable 'comment-start) "%")
  (set (make-local-variable 'outline-regexp) "^[a-z]")
  (set (make-local-variable 'imenu-generic-expression)
       '((nil "\\<\\(FUNCTION\\|MACRO\\)\\s-+{\\([^}\n]+\\)}" 2)))
  (set (make-local-variable 'indent-line-function) 'bibtex-style-indent-line)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (setq font-lock-defaults
	'(bibtex-style-font-lock-keywords nil t
	  ((?. . "w")))))

(defun bibtex-style-indent-line ()
  "Indent current line of BibTeX Style code."
  (interactive)
  (let* ((savep (point))
	 (indent (condition-case nil
		     (save-excursion
		       (forward-line 0)
		       (skip-chars-forward " \t")
		       (if (>= (point) savep) (setq savep nil))
		       (max (bibtex-style-calculate-indentation) 0))
		   (error 0))))
    (if savep
	(save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defcustom bibtex-style-indent-basic 2
  "Basic amount of indentation to use in BibTeX Style mode."
  :version "22.2"
  :type 'integer
  :group 'bibtex)

(defun bibtex-style-calculate-indentation (&optional virt)
  (or
   ;; Stick the first line at column 0.
   (and (= (point-min) (line-beginning-position)) 0)
   ;; Commands start at column 0.
   (and (looking-at (regexp-opt bibtex-style-commands 'words)) 0)
   ;; Trust the current indentation, if such info is applicable.
   (and virt (save-excursion (skip-chars-backward " \t{") (bolp))
	(current-column))
   ;; Put leading close-paren where the matching open brace would be.
   (and (looking-at "}")
	(condition-case nil
	    (save-excursion
	      (up-list -1)
	      (bibtex-style-calculate-indentation 'virt))
	  (scan-error nil)))
   ;; Align leading "if$" with previous command.
   (and (looking-at "if\\$")
	(condition-case nil
	    (save-excursion
	      (backward-sexp 3)
	      (bibtex-style-calculate-indentation 'virt))
	  (scan-error
	   ;; There is no command before the "if$".
	   (condition-case nil
	       (save-excursion
		 (up-list -1)
		 (+ bibtex-style-indent-basic
		    (bibtex-style-calculate-indentation 'virt)))
	     (scan-error nil)))))
   ;; Right after an opening brace.
   (condition-case err (save-excursion (backward-sexp 1) nil)
     (scan-error (goto-char (nth 2 err))
		 (+ bibtex-style-indent-basic
		    (bibtex-style-calculate-indentation 'virt))))
   ;; Default, align with previous command.
   (let ((fai ;; First arm of an "if$".
	  (condition-case nil
	      (save-excursion
		(forward-sexp 2)
		(forward-comment (point-max))
		(looking-at "if\\$"))
	    (scan-error nil))))
     (save-excursion
       (condition-case nil
	   (while (progn
		    (backward-sexp 1)
		    (save-excursion (skip-chars-backward " \t{") (not (bolp)))))
	 (scan-error nil))
       (+ (current-column)
	  (if (or fai (looking-at "ENTRY")) bibtex-style-indent-basic 0))))))


(provide 'bibtex-style)
;;; bibtex-style.el ends here
