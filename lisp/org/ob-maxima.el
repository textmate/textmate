;;; ob-maxima.el --- org-babel functions for maxima evaluation

;; Copyright (C) 2009-2012  Free Software Foundation, Inc.

;; Author: Eric S Fraga
;;	Eric Schulte
;; Keywords: literate programming, reproducible research, maxima
;; Homepage: http://orgmode.org

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

;; Org-Babel support for evaluating maxima entries.
;;
;; This differs from most standard languages in that
;;
;; 1) there is no such thing as a "session" in maxima
;;
;; 2) we are adding the "cmdline" header argument

;;; Code:
(require 'ob)

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("maxima" . "max"))

(defvar org-babel-default-header-args:maxima '())

(defcustom org-babel-maxima-command
  (if (boundp 'maxima-command) maxima-command "maxima")
  "Command used to call maxima on the shell."
  :group 'org-babel)

(defun org-babel-maxima-expand (body params)
  "Expand a block of Maxima code according to its header arguments."
  (let ((vars (mapcar #'cdr (org-babel-get-header params :var))))
     (mapconcat 'identity
		(list
		 ;; graphic output
		 (let ((graphic-file (org-babel-maxima-graphical-output-file params)))
		   (if graphic-file
		       (format
			"set_plot_option ([gnuplot_term, png]); set_plot_option ([gnuplot_out_file, %S]);"
			graphic-file)
		     ""))
		 ;; variables
		 (mapconcat 'org-babel-maxima-var-to-maxima vars "\n")
		 ;; body
		 body
		 "gnuplot_close ()$")
		"\n")))

(defun org-babel-execute:maxima (body params)
  "Execute a block of Maxima entries with org-babel.  This function is
called by `org-babel-execute-src-block'."
  (message "executing Maxima source code block")
  (let ((result-params (split-string (or (cdr (assoc :results params)) "")))
	(result
	 (let* ((cmdline (cdr (assoc :cmdline params)))
		(in-file (org-babel-temp-file "maxima-" ".max"))
		(cmd (format "%s --very-quiet -r 'batchload(%S)$' %s"
			     org-babel-maxima-command in-file cmdline)))
	   (with-temp-file in-file (insert (org-babel-maxima-expand body params)))
	   (message cmd)
	   ((lambda (raw) ;; " | grep -v batch | grep -v 'replaced' | sed '/^$/d' "
	      (mapconcat
	       #'identity
	       (delq nil
		     (mapcar (lambda (line)
			       (unless (or (string-match "batch" line)
					   (string-match "^rat: replaced .*$" line)
					   (= 0 (length line)))
				 line))
			     (split-string raw "[\r\n]"))) "\n"))
	    (org-babel-eval cmd "")))))
    (if (org-babel-maxima-graphical-output-file params)
	nil
      (if (or (member "scalar" result-params)
	      (member "verbatim" result-params)
	      (member "output" result-params))
	  result
	(let ((tmp-file (org-babel-temp-file "maxima-res-")))
	  (with-temp-file tmp-file (insert result))
	  (org-babel-import-elisp-from-file tmp-file))))))


(defun org-babel-prep-session:maxima (session params)
  (error "Maxima does not support sessions"))

(defun org-babel-maxima-var-to-maxima (pair)
  "Convert an elisp val into a string of maxima code specifying a var
of the same value."
  (let ((var (car pair))
        (val (cdr pair)))
    (when (symbolp val)
      (setq val (symbol-name val))
      (when (= (length val) 1)
        (setq val (string-to-char val))))
      (format "%S: %s$" var
	      (org-babel-maxima-elisp-to-maxima val))))

(defun org-babel-maxima-graphical-output-file (params)
  "Name of file to which maxima should send graphical output."
  (and (member "graphics" (cdr (assq :result-params params)))
       (cdr (assq :file params))))

(defun org-babel-maxima-elisp-to-maxima (val)
  "Return a string of maxima code which evaluates to VAL."
  (if (listp val)
      (concat "[" (mapconcat #'org-babel-maxima-elisp-to-maxima val ", ") "]")
    (format "%s" val)))


(provide 'ob-maxima)



;;; ob-maxima.el ends here
