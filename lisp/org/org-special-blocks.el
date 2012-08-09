;;; org-special-blocks.el --- handle Org special blocks
;; Copyright (C) 2009-2012  Free Software Foundation, Inc.

;; Author: Chris Gray <chrismgray@gmail.com>

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
;;

;; This package generalizes the #+begin_foo and #+end_foo tokens.

;; To use, put the following in your init file:
;;
;; (require 'org-special-blocks)

;; The tokens #+begin_center, #+begin_verse, etc. existed previously.
;; This package generalizes them (at least for the LaTeX and html
;; exporters).  When a #+begin_foo token is encountered by the LaTeX
;; exporter, it is expanded into \begin{foo}.  The text inside the
;; environment is not protected, as text inside environments generally
;; is.  When #+begin_foo is encountered by the html exporter, a div
;; with class foo is inserted into the HTML file.  It is up to the
;; user to add this class to his or her stylesheet if this div is to
;; mean anything.

(require 'org-html)
(require 'org-compat)

(declare-function org-open-par "org-html" ())
(declare-function org-close-par-maybe "org-html" ())

(defvar org-special-blocks-ignore-regexp "^\\(LaTeX\\|HTML\\)$"
  "A regexp indicating the names of blocks that should be ignored
by org-special-blocks.  These blocks will presumably be
interpreted by other mechanisms.")

(defvar org-export-current-backend) ; dynamically bound in org-exp.el
(defun org-special-blocks-make-special-cookies ()
  "Adds special cookies when #+begin_foo and #+end_foo tokens are
seen.  This is run after a few special cases are taken care of."
  (when (or (eq org-export-current-backend 'html)
	    (eq org-export-current-backend 'latex))
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*#\\+\\(begin\\|end\\)_\\(.*\\)$" nil t)
      (unless (org-string-match-p org-special-blocks-ignore-regexp (match-string 2))
	(replace-match
	 (if (equal (downcase (match-string 1)) "begin")
	     (concat "ORG-" (match-string 2) "-START")
	   (concat "ORG-" (match-string 2) "-END"))
	 t t)))))

(add-hook 'org-export-preprocess-after-blockquote-hook
	  'org-special-blocks-make-special-cookies)

(defun org-special-blocks-convert-latex-special-cookies ()
  "Converts the special cookies into LaTeX blocks."
  (goto-char (point-min))
  (while (re-search-forward "^ORG-\\([^ \t\n]*\\)[ \t]*\\(.*\\)-\\(START\\|END\\)$" nil t)
    (replace-match
     (if (equal (match-string 3) "START")
	 (concat "\\begin{" (match-string 1) "}" (match-string 2))
       (concat "\\end{" (match-string 1) "}"))
     t t)))


(add-hook 'org-export-latex-after-blockquotes-hook
	  'org-special-blocks-convert-latex-special-cookies)

(defvar line)
(defun org-special-blocks-convert-html-special-cookies ()
  "Converts the special cookies into div blocks."
  ;; Uses the dynamically-bound variable `line'.
  (when (string-match "^ORG-\\(.*\\)-\\(START\\|END\\)$" line)
    (message "%s" (match-string 1))
    (when (equal (match-string 2 line) "START")
      (org-close-par-maybe)
      (insert "\n<div class=\"" (match-string 1 line) "\">")
      (org-open-par))
    (when (equal (match-string 2 line) "END")
      (org-close-par-maybe)
      (insert "\n</div>")
      (org-open-par))
    (throw 'nextline nil)))

(add-hook 'org-export-html-after-blockquotes-hook
	  'org-special-blocks-convert-html-special-cookies)

(provide 'org-special-blocks)

;;; org-special-blocks.el ends here
