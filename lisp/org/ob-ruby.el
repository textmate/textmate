;;; ob-ruby.el --- org-babel functions for ruby evaluation

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

;; Org-Babel support for evaluating ruby source code.

;;; Requirements:

;; - ruby and irb executables :: http://www.ruby-lang.org/
;;
;; - ruby-mode :: Can be installed through ELPA, or from
;;   http://github.com/eschulte/rinari/raw/master/util/ruby-mode.el
;;
;; - inf-ruby mode :: Can be installed through ELPA, or from
;;   http://github.com/eschulte/rinari/raw/master/util/inf-ruby.el

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
(eval-when-compile (require 'cl))

(declare-function run-ruby "ext:inf-ruby" (&optional command name))
(declare-function xmp "ext:rcodetools" (&optional option))

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("ruby" . "rb"))

(defvar org-babel-default-header-args:ruby '())

(defvar org-babel-ruby-command "ruby"
  "Name of command to use for executing ruby code.")

(defun org-babel-execute:ruby (body params)
  "Execute a block of Ruby code with Babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((session (org-babel-ruby-initiate-session
		   (cdr (assoc :session params))))
         (result-params (cdr (assoc :result-params params)))
         (result-type (cdr (assoc :result-type params)))
         (full-body (org-babel-expand-body:generic
		     body params (org-babel-variable-assignments:ruby params)))
         (result (if (member "xmp" result-params)
		     (with-temp-buffer
		     (require 'rcodetools)
		     (insert full-body)
		     (xmp (cdr (assoc :xmp-option params)))
		     (buffer-string))
		   (org-babel-ruby-evaluate
		      session full-body result-type result-params))))
    (org-babel-reassemble-table
     result
     (org-babel-pick-name (cdr (assoc :colname-names params))
			  (cdr (assoc :colnames params)))
     (org-babel-pick-name (cdr (assoc :rowname-names params))
			  (cdr (assoc :rownames params))))))

(defun org-babel-prep-session:ruby (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  ;; (message "params=%S" params) ;; debugging
  (let* ((session (org-babel-ruby-initiate-session session))
         (var-lines (org-babel-variable-assignments:ruby params)))
    (org-babel-comint-in-buffer session
      (sit-for .5) (goto-char (point-max))
      (mapc (lambda (var)
              (insert var) (comint-send-input nil t)
              (org-babel-comint-wait-for-output session)
              (sit-for .1) (goto-char (point-max))) var-lines))
    session))

(defun org-babel-load-session:ruby (session body params)
  "Load BODY into SESSION."
  (save-window-excursion
    (let ((buffer (org-babel-prep-session:ruby session params)))
      (with-current-buffer buffer
        (goto-char (process-mark (get-buffer-process (current-buffer))))
        (insert (org-babel-chomp body)))
      buffer)))

;; helper functions

(defun org-babel-variable-assignments:ruby (params)
  "Return list of ruby statements assigning the block's variables"
  (mapcar
   (lambda (pair)
     (format "%s=%s"
	     (car pair)
	     (org-babel-ruby-var-to-ruby (cdr pair))))
   (mapcar #'cdr (org-babel-get-header params :var))))

(defun org-babel-ruby-var-to-ruby (var)
  "Convert VAR into a ruby variable.
Convert an elisp value into a string of ruby source code
specifying a variable of the same value."
  (if (listp var)
      (concat "[" (mapconcat #'org-babel-ruby-var-to-ruby var ", ") "]")
    (format "%S" var)))

(defun org-babel-ruby-table-or-string (results)
  "Convert RESULTS into an appropriate elisp value.
If RESULTS look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (org-babel-script-escape results))

(defun org-babel-ruby-initiate-session (&optional session params)
  "Initiate a ruby session.
If there is not a current inferior-process-buffer in SESSION
then create one.  Return the initialized session."
  (unless (string= session "none")
    (require 'inf-ruby)
    (let ((session-buffer (save-window-excursion
			    (run-ruby nil session) (current-buffer))))
      (if (org-babel-comint-buffer-livep session-buffer)
	  (progn (sit-for .25) session-buffer)
        (sit-for .5)
        (org-babel-ruby-initiate-session session)))))

(defvar org-babel-ruby-eoe-indicator ":org_babel_ruby_eoe"
  "String to indicate that evaluation has completed.")
(defvar org-babel-ruby-f-write
  "File.open('%s','w'){|f| f.write((_.class == String) ? _ : _.inspect)}")
(defvar org-babel-ruby-pp-f-write
  "File.open('%s','w'){|f| $stdout = f; pp(results); $stdout = orig_out}")
(defvar org-babel-ruby-wrapper-method
  "
def main()
%s
end
results = main()
File.open('%s', 'w'){ |f| f.write((results.class == String) ? results : results.inspect) }
")
(defvar org-babel-ruby-pp-wrapper-method
  "
require 'pp'
def main()
%s
end
results = main()
File.open('%s', 'w') do |f|
  $stdout = f
  pp results
end
")

(defun org-babel-ruby-evaluate
  (buffer body &optional result-type result-params)
  "Pass BODY to the Ruby process in BUFFER.
If RESULT-TYPE equals 'output then return a list of the outputs
of the statements in BODY, if RESULT-TYPE equals 'value then
return the value of the last statement in BODY, as elisp."
  (if (not buffer)
      ;; external process evaluation
      (case result-type
	(output (org-babel-eval org-babel-ruby-command body))
	(value (let ((tmp-file (org-babel-temp-file "ruby-")))
		 (org-babel-eval
		  org-babel-ruby-command
		  (format (if (member "pp" result-params)
			      org-babel-ruby-pp-wrapper-method
			    org-babel-ruby-wrapper-method)
			  body (org-babel-process-file-name tmp-file 'noquote)))
		 ((lambda (raw)
		    (if (or (member "code" result-params)
			    (member "pp" result-params))
			raw
		      (org-babel-ruby-table-or-string raw)))
		  (org-babel-eval-read-file tmp-file)))))
    ;; comint session evaluation
    (case result-type
      (output
       (mapconcat
	#'identity
	(butlast
	 (split-string
	  (mapconcat
	   #'org-babel-trim
	   (butlast
	    (org-babel-comint-with-output
		(buffer org-babel-ruby-eoe-indicator t body)
	      (mapc
	       (lambda (line)
		 (insert (org-babel-chomp line)) (comint-send-input nil t))
	       (list body org-babel-ruby-eoe-indicator))
	      (comint-send-input nil t)) 2)
	   "\n") "[\r\n]")) "\n"))
      (value
       ((lambda (results)
	  (if (or (member "code" result-params) (member "pp" result-params))
	      results
	    (org-babel-ruby-table-or-string results)))
	(let* ((tmp-file (org-babel-temp-file "ruby-"))
	       (ppp (or (member "code" result-params)
			(member "pp" result-params))))
	  (org-babel-comint-with-output
	      (buffer org-babel-ruby-eoe-indicator t body)
	    (when ppp (insert "require 'pp';") (comint-send-input nil t))
	    (mapc
	     (lambda (line)
	       (insert (org-babel-chomp line)) (comint-send-input nil t))
	     (append
	      (list body)
	      (if (not ppp)
		  (list (format org-babel-ruby-f-write
				(org-babel-process-file-name tmp-file 'noquote)))
		(list
		 "results=_" "require 'pp'" "orig_out = $stdout"
		 (format org-babel-ruby-pp-f-write
			 (org-babel-process-file-name tmp-file 'noquote))))
	      (list org-babel-ruby-eoe-indicator)))
	    (comint-send-input nil t))
	  (org-babel-eval-read-file tmp-file)))))))

(defun org-babel-ruby-read-string (string)
  "Strip \\\"s from around a ruby string."
  (if (string-match "^\"\\([^\000]+\\)\"$" string)
      (match-string 1 string)
    string))

(provide 'ob-ruby)



;;; ob-ruby.el ends here
