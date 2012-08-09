;;; ob-gnuplot.el --- org-babel functions for gnuplot evaluation

;; Copyright (C) 2009-2012  Free Software Foundation, Inc.

;; Author: Eric Schulte
;; Keywords: literate programming, reproducible research
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

;; Org-Babel support for evaluating gnuplot source code.
;;
;; This differs from most standard languages in that
;;
;; 1) we are generally only going to return results of type "file"
;;
;; 2) we are adding the "file" and "cmdline" header arguments

;;; Requirements:

;; - gnuplot :: http://www.gnuplot.info/
;;
;; - gnuplot-mode :: http://cars9.uchicago.edu/~ravel/software/gnuplot-mode.html

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(eval-when-compile (require 'cl))

(declare-function org-time-string-to-time "org" (s))
(declare-function org-combine-plists "org" (&rest plists))
(declare-function orgtbl-to-generic "org-table" (table params))
(declare-function gnuplot-mode "ext:gnuplot-mode" ())
(declare-function gnuplot-send-string-to-gnuplot "ext:gnuplot-mode" (str txt))
(declare-function gnuplot-send-buffer-to-gnuplot "ext:gnuplot-mode" ())

(defvar org-babel-default-header-args:gnuplot
  '((:results . "file") (:exports . "results") (:session . nil))
  "Default arguments to use when evaluating a gnuplot source block.")

(defvar org-babel-gnuplot-timestamp-fmt nil)

(defun org-babel-gnuplot-process-vars (params)
  "Extract variables from PARAMS and process the variables.
Dumps all vectors into files and returns an association list
of variable names and the related value to be used in the gnuplot
code."
  (mapcar
   (lambda (pair)
     (cons
      (car pair) ;; variable name
      (if (listp (cdr pair)) ;; variable value
          (org-babel-gnuplot-table-to-data
           (cdr pair) (org-babel-temp-file "gnuplot-") params)
        (cdr pair))))
   (mapcar #'cdr (org-babel-get-header params :var))))

(defun org-babel-expand-body:gnuplot (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (save-window-excursion
    (let* ((vars (org-babel-gnuplot-process-vars params))
           (out-file (cdr (assoc :file params)))
           (term (or (cdr (assoc :term params))
                     (when out-file (file-name-extension out-file))))
           (cmdline (cdr (assoc :cmdline params)))
           (title (plist-get params :title))
           (lines (plist-get params :line))
           (sets (plist-get params :set))
           (x-labels (plist-get params :xlabels))
           (y-labels (plist-get params :ylabels))
           (timefmt (plist-get params :timefmt))
           (time-ind (or (plist-get params :timeind)
                         (when timefmt 1)))
           output)
      (flet ((add-to-body (text)
                          (setq body (concat text "\n" body))))
        ;; append header argument settings to body
        (when title (add-to-body (format "set title '%s'" title))) ;; title
        (when lines (mapc (lambda (el) (add-to-body el)) lines)) ;; line
        (when sets
          (mapc (lambda (el) (add-to-body (format "set %s" el))) sets))
        (when x-labels
          (add-to-body
           (format "set xtics (%s)"
                   (mapconcat (lambda (pair)
                                (format "\"%s\" %d" (cdr pair) (car pair)))
                              x-labels ", "))))
        (when y-labels
          (add-to-body
           (format "set ytics (%s)"
                   (mapconcat (lambda (pair)
                                (format "\"%s\" %d" (cdr pair) (car pair)))
                              y-labels ", "))))
        (when time-ind
          (add-to-body "set xdata time")
          (add-to-body (concat "set timefmt \""
                               (or timefmt
                                   "%Y-%m-%d-%H:%M:%S") "\"")))
        (when out-file (add-to-body (format "set output \"%s\"" out-file)))
        (when term (add-to-body (format "set term %s" term)))
        ;; insert variables into code body: this should happen last
        ;; placing the variables at the *top* of the code in case their
        ;; values are used later
        (add-to-body (mapconcat #'identity
				(org-babel-variable-assignments:gnuplot params)
				"\n"))
        ;; replace any variable names preceded by '$' with the actual
        ;; value of the variable
        (mapc (lambda (pair)
                (setq body (replace-regexp-in-string
                            (format "\\$%s" (car pair)) (cdr pair) body)))
              vars))
      body)))

(defun org-babel-execute:gnuplot (body params)
  "Execute a block of Gnuplot code.
This function is called by `org-babel-execute-src-block'."
  (require 'gnuplot)
  (let ((session (cdr (assoc :session params)))
        (result-type (cdr (assoc :results params)))
        (out-file (cdr (assoc :file params)))
        (body (org-babel-expand-body:gnuplot body params))
	output)
    (save-window-excursion
      ;; evaluate the code body with gnuplot
      (if (string= session "none")
          (let ((script-file (org-babel-temp-file "gnuplot-script-")))
            (with-temp-file script-file
              (insert (concat body "\n")))
            (message "gnuplot \"%s\"" script-file)
            (setq output
                  (shell-command-to-string
		   (format
		    "gnuplot \"%s\""
		    (org-babel-process-file-name
		     script-file
		     (if (member system-type '(cygwin windows-nt ms-dos))
			 t nil)))))
            (message output))
        (with-temp-buffer
          (insert (concat body "\n"))
          (gnuplot-mode)
          (gnuplot-send-buffer-to-gnuplot)))
      (if (member "output" (split-string result-type))
          output
	nil)))) ;; signal that output has already been written to file

(defun org-babel-prep-session:gnuplot (session params)
  "Prepare SESSION according to the header arguments in PARAMS."
  (let* ((session (org-babel-gnuplot-initiate-session session))
         (var-lines (org-babel-variable-assignments:gnuplot params)))
    (message "%S" session)
    (org-babel-comint-in-buffer session
      (mapc (lambda (var-line)
              (insert var-line) (comint-send-input nil t)
              (org-babel-comint-wait-for-output session)
              (sit-for .1) (goto-char (point-max))) var-lines))
    session))

(defun org-babel-load-session:gnuplot (session body params)
  "Load BODY into SESSION."
  (save-window-excursion
    (let ((buffer (org-babel-prep-session:gnuplot session params)))
      (with-current-buffer buffer
        (goto-char (process-mark (get-buffer-process (current-buffer))))
        (insert (org-babel-chomp body)))
      buffer)))

(defun org-babel-variable-assignments:gnuplot (params)
  "Return list of gnuplot statements assigning the block's variables"
  (mapcar
   (lambda (pair) (format "%s = \"%s\"" (car pair) (cdr pair)))
   (org-babel-gnuplot-process-vars params)))

(defvar gnuplot-buffer)
(defun org-babel-gnuplot-initiate-session (&optional session params)
  "Initiate a gnuplot session.
If there is not a current inferior-process-buffer in SESSION
then create one.  Return the initialized session.  The current
`gnuplot-mode' doesn't provide support for multiple sessions."
  (require 'gnuplot)
  (unless (string= session "none")
    (save-window-excursion
      (gnuplot-send-string-to-gnuplot "" "line")
      gnuplot-buffer)))

(defun org-babel-gnuplot-quote-timestamp-field (s)
  "Convert S from timestamp to Unix time and export to gnuplot."
  (format-time-string org-babel-gnuplot-timestamp-fmt (org-time-string-to-time s)))

(defvar org-table-number-regexp)
(defvar org-ts-regexp3)
(defun org-babel-gnuplot-quote-tsv-field (s)
  "Quote S for export to gnuplot."
  (unless (stringp s)
    (setq s (format "%s" s)))
  (if (string-match org-table-number-regexp s) s
    (if (string-match org-ts-regexp3 s)
	(org-babel-gnuplot-quote-timestamp-field s)
      (concat "\"" (mapconcat 'identity (split-string s "\"") "\"\"") "\""))))

(defun org-babel-gnuplot-table-to-data (table data-file params)
  "Export TABLE to DATA-FILE in a format readable by gnuplot.
Pass PARAMS through to `orgtbl-to-generic' when exporting TABLE."
  (with-temp-file data-file
    (make-local-variable 'org-babel-gnuplot-timestamp-fmt)
    (setq org-babel-gnuplot-timestamp-fmt (or
                                           (plist-get params :timefmt)
                                           "%Y-%m-%d-%H:%M:%S"))
    (insert (orgtbl-to-generic
	     table
	     (org-combine-plists
	      '(:sep "\t" :fmt org-babel-gnuplot-quote-tsv-field)
	      params))))
  data-file)

(provide 'ob-gnuplot)



;;; ob-gnuplot.el ends here
