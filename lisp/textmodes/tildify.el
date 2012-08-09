;;; tildify.el --- adding hard spaces into texts

;; Copyright (C) 1997-2012 Free Software Foundation, Inc.

;; Author:     Milan Zamazal <pdm@zamazal.org>
;; Version:    4.5
;; Keywords:   text, TeX, SGML, wp

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

;; This package can be typically used for adding forgotten tildes in TeX
;; sources or adding `&nbsp;' sequences in SGML (e.g. HTML) texts.
;;
;; For example, the Czech orthography requires avoiding one letter
;; prepositions at line endings.  So they should be connected with the
;; following words by a tilde.  Some users forget to do this all the
;; time.  The purpose of this program is to check the text and suggest
;; adding of missing tildes on some places.  It works in a similar
;; manner to `query-replace-regexp'.
;;
;; The functionality of this program is actually performing query
;; replace on certain regions, but for historical reasons explained
;; above it is called `tildify'.
;;
;; The default variable settings are suited for Czech, so do not try to
;; understand them if you are not familiar with Czech grammar and spelling.
;;
;; The algorithm was inspired by Petr Ol¹ák's program `vlna'.  Abilities of
;; `tildify.el' are a little limited; if you have improvement suggestions, let
;; me know.

;;; Code:


;;; *** User configuration variables ***


(defgroup tildify nil
  "Adding missing hard spaces or other text fragments into texts."
  :version "21.1"
  :group 'wp)

(defcustom tildify-pattern-alist
  '((t "\\([,:;(][ \t]*[a]\\|\\<[AIKOSUVZikosuvz]\\)\\([ \t]+\\|[ \t]*\n[ \t]*\\)\\(\\w\\|[([{\\]\\|<[a-zA-Z]\\)" 2))
  "Alist specifying where to insert hard spaces.

Each alist item is of the form (MAJOR-MODE REGEXP NUMBER) or
\(MAJOR-MODE . SYMBOL).

MAJOR-MODE defines major mode, for which the item applies.  It can be either:
- a symbol equal to the major mode of the buffer to be fixed
- t for default item, this applies to all major modes not defined in another
  alist item

REGEXP is a regular expression matching the part of a text, where a hard space
is missing.  The regexp is always case sensitive, regardless of the current
`case-fold-search' setting.

NUMBER defines the number of the REGEXP subexpression which should be replaced
by the hard space character.

The form (MAJOR-MODE . SYMBOL) defines alias item for MAJOR-MODE.  For this
mode, the item for the mode SYMBOL is looked up in the alist instead."
  :group 'tildify
  :type '(repeat (choice (list symbol regexp integer) (cons symbol symbol))))

(defcustom tildify-string-alist
  '((latex-mode . "~")
    (tex-mode . latex-mode)
    (plain-tex-mode . latex-mode)
    (sgml-mode . "&nbsp;")
    (xml-mode . sgml-mode)
    (html-mode . sgml-mode)
    (t . " "))
  "Alist specifying what is a hard space in the current major mode.

Each alist item is of the form (MAJOR-MODE . STRING) or
\(MAJOR-MODE . SYMBOL).

MAJOR-MODE defines major mode, for which the item applies.  It can be either:
- a symbol equal to the major mode of the buffer to be fixed
- t for default item, this applies to all major modes not defined in another
  alist item

STRING defines the hard space, which is inserted at places defined by
`tildify-pattern-alist'.  For example it can be \"~\" for TeX or \"&nbsp;\"
for SGML.

The form (MAJOR-MODE . SYMBOL) defines alias item for MAJOR-MODE.  For this
mode, the item for the mode SYMBOL is looked up in the alist instead."
  :group 'tildify
  :type '(repeat (cons symbol (choice string symbol))))

(defcustom tildify-ignored-environments-alist
  '((latex-mode
     ("\\\\\\\\" . "")		; do not remove this
     ("\\\\begin{verbatim}" . "\\\\end{verbatim}")
     ("\\\\verb\\*?\\(.\\)" . (1))
     ("\\$\\$" . "\\$\\$")
     ("\\$" . "\\$")
     ("\\\\(" . "\\\\)")
     ("\\\\[[]" . "\\\\[]]")
     ("\\\\begin{math}" . "\\\\end{math}")
     ("\\\\begin{displaymath}" . "\\\\end{displaymath}")
     ("\\\\begin{equation}" . "\\\\end{equation}")
     ("\\\\begin{eqnarray\\*?}" . "\\\\end{eqnarray\\*?}")
     ("\\\\[a-zA-Z]+\\( +\\|{}\\)[a-zA-Z]*" . "")
     ("%" . "$"))
    (plain-tex-mode . latex-mode)
    (html-mode
     ("<pre[^>]*>" . "</pre>")
     ("<dfn>" . "</dfn>")
     ("<code>" . "</code>")
     ("<samp>" . "</samp>")
     ("<kbd>" . "</kbd>")
     ("<var>" . "</var>")
     ("<PRE[^>]*>" . "</PRE>")
     ("<DFN>" . "</DFN>")
     ("<CODE>" . "</CODE>")
     ("<SAMP>" . "</SAMP>")
     ("<KBD>" . "</KBD>")
     ("<VAR>" . "</VAR>")
     ("<! *--" . "-- *>")
     ("<" . ">"))
    (sgml-mode . html-mode)
    (t nil))
  "Alist specifying ignored structured text environments.
Parts of text defined in this alist are skipped without performing hard space
insertion on them.  These setting allow skipping text parts like verbatim or
math environments in TeX or preformatted text in SGML.

Each list element is of the form
  (MAJOR-MODE (BEG-REGEX . END-REGEX) (BEG-REGEX . END-REGEX) ... )

MAJOR-MODE defines major mode, for which the item applies.  It can be either:
- a symbol equal to the major mode of the buffer to be fixed
- t for default item, this applies to all major modes not defined in another
  alist item

BEG-REGEX is a regexp matching beginning of a text part to be skipped.
END-REGEX defines end of the corresponding text part and can be either:
- a regexp matching the end of the skipped text part
- a list of regexps and numbers, which will compose the ending regexp by
  concatenating themselves, while replacing the numbers with corresponding
  subexpressions of BEG-REGEX (this is used to solve cases like
  \\\\verb<character> in TeX)."
  :group 'tildify
  :type '(repeat (cons symbol (choice symbol (repeat sexp)))))


;;; *** Internal variables ***

(defvar tildify-count nil
  "Counter for replacements.")


;;; *** Interactive functions ***

;;;###autoload
(defun tildify-region (beg end)
  "Add hard spaces in the region between BEG and END.
See variables `tildify-pattern-alist', `tildify-string-alist', and
`tildify-ignored-environments-alist' for information about configuration
parameters.
This function performs no refilling of the changed text."
  (interactive "*r")
  (setq tildify-count 0)
  (let (a
	z
	(marker-end (copy-marker end))
	end-env
	finish
	(ask t)
	(case-fold-search nil)
	(regexp (tildify-build-regexp))	; beginnings of environments
	aux)
    (if regexp
	;; Yes, ignored environments exist for the current major mode,
	;; tildify just texts outside them
	(save-excursion
	  (save-restriction
	    (widen)
	    (goto-char (point-min))
	    (while (not finish)
	      (setq a (point))
	      (setq end-env (tildify-find-env regexp))
	      (setq z (copy-marker (if end-env (1- (point)) (point-max))))
	      (if (>= (marker-position z) beg)
		  (progn
		    (or (>= a beg) (setq a beg))
		    (or (<= (marker-position z) (marker-position marker-end))
			(setq z marker-end))
		    (setq aux (tildify-tildify a (marker-position z) ask))
		    (if (eq aux 'force)
			(setq ask nil)
		      (if (eq aux nil)
			  (setq finish t)))))
	      (if (>= (marker-position z) (marker-position marker-end))
		  (setq finish t))
	      (or (>= (point) (marker-position z))
		  (goto-char (marker-position z)))
	      (if (not finish)
		  (if (re-search-forward end-env nil t)
		      (if (> (point) (marker-position marker-end))
			  (setq finish t))
		    (message
		     "End of environment not found: %s" end-env)
		    (setq finish t))))))
      ;; No ignored environments, tildify directly
      (tildify-tildify beg end ask)))
  (message "%d spaces replaced." tildify-count))

;;;###autoload
(defun tildify-buffer ()
  "Add hard spaces in the current buffer.
See variables `tildify-pattern-alist', `tildify-string-alist', and
`tildify-ignored-environments-alist' for information about configuration
parameters.
This function performs no refilling of the changed text."
  (interactive  "*")
  (tildify-region (point-min) (point-max)))


;;; *** Auxiliary functions ***

(defun tildify-build-regexp ()
  "Build start of environment regexp."
  (let ((alist (tildify-mode-alist tildify-ignored-environments-alist))
	regexp)
    (when alist
      (setq regexp (caar alist))
      (setq alist (cdr alist))
      (while alist
	(setq regexp (concat regexp "\\|" (caar alist)))
	(setq alist (cdr alist)))
      regexp)))

(defun tildify-mode-alist (mode-alist &optional mode)
  "Return alist item for the MODE-ALIST in the current major MODE."
  (if (null mode)
      (setq mode major-mode))
  (let ((alist (cdr (or (assoc mode mode-alist)
			(assoc t mode-alist)))))
    (if (and alist
	     (symbolp alist))
	(tildify-mode-alist mode-alist alist)
      alist)))

(defun tildify-find-env (regexp)
  "Find environment using REGEXP.
Return regexp for the end of the environment or nil if no environment was
found."
  ;; Find environment
  (if (re-search-forward regexp nil t)
      ;; Build end-env regexp
      (let ((match (match-string 0))
	    (alist (tildify-mode-alist tildify-ignored-environments-alist))
	    expression)
	(save-match-data
	  (while (not (eq (string-match (caar alist) match) 0))
	    (setq alist (cdr alist))))
	(if (stringp (setq expression (cdar alist)))
	    expression
	  (let ((result "")
		aux)
	    (while expression
	      (setq result (concat result
				   (if (stringp (setq aux (car expression)))
				       expression
				     (regexp-quote (match-string aux)))))
	      (setq expression (cdr expression)))
	    result)))
    ;; Return nil if not found
    nil))

(defun tildify-tildify (beg end ask)
  "Add tilde characters in the region between BEG and END.
This function does not do any further checking except of for comments and
macros.

If ASK is nil, perform replace without asking user for confirmation.

Returns one of symbols: t (all right), nil (quit), force (replace without
further questions)."
  (save-excursion
    (goto-char beg)
    (let* ((alist (tildify-mode-alist tildify-pattern-alist))
	   (regexp (car alist))
	   (match-number (cadr alist))
	   (tilde (tildify-mode-alist tildify-string-alist))
	   (end-marker (copy-marker end))
	   answer
	   bad-answer
	   replace
	   quit
	   (message-log-max nil))
      (while (and (not quit)
		  (re-search-forward regexp (marker-position end-marker) t))
	(when (or (not ask)
		  (progn
		    (goto-char (match-beginning match-number))
		    (setq bad-answer t)
		    (while bad-answer
		      (setq bad-answer nil)
		      (message "Replace? (yn!q) ")
		      (setq answer (read-event)))
		    (cond
		     ((or (eq answer ?y) (eq answer ? ) (eq answer 'space))
		      (setq replace t))
		     ((eq answer ?n)
		      (setq replace nil))
		     ((eq answer ?!)
		      (setq replace t
			    ask nil))
		     ((eq answer ?q)
		      (setq replace nil
			    quit t))
		     (t
		      (message "Press y, n, !, or q.")
		      (setq bad-answer t)))
		    replace))
	  (replace-match tilde t t nil match-number)
	  (setq tildify-count (1+ tildify-count))))
      ;; Return value
      (cond
       (quit nil)
       ((not ask) 'force)
       (t t)))))


;;; *** Announce ***

(provide 'tildify)


;; Local variables:
;; coding: iso-latin-2
;; End:

;;; tildify.el ends here
