;;; ob-octave.el --- org-babel functions for octave and matlab evaluation

;; Copyright (C) 2010-2012  Free Software Foundation, Inc.

;; Author: Dan Davison
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

;;; Requirements:

;; octave
;; octave-mode.el and octave-inf.el come with GNU emacs

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
(eval-when-compile (require 'cl))

(declare-function matlab-shell "ext:matlab-mode")
(declare-function matlab-shell-run-region "ext:matlab-mode")

(defvar org-babel-default-header-args:matlab '())
(defvar org-babel-default-header-args:octave '())

(defvar org-babel-matlab-shell-command "matlab -nosplash"
  "Shell command to run matlab as an external process.")
(defvar org-babel-octave-shell-command "octave -q"
  "Shell command to run octave as an external process.")

(defvar org-babel-matlab-with-emacs-link nil
  "If non-nil use matlab-shell-run-region for session evaluation.
  This will use EmacsLink if (matlab-with-emacs-link) evaluates
  to a non-nil value.")

(defvar org-babel-matlab-emacs-link-wrapper-method
   "%s
if ischar(ans), fid = fopen('%s', 'w'); fprintf(fid, '%%s\\n', ans); fclose(fid);
else, save -ascii %s ans
end
delete('%s')
")
(defvar org-babel-octave-wrapper-method
  "%s
if ischar(ans), fid = fopen('%s', 'w'); fprintf(fid, '%%s\\n', ans); fclose(fid);
else, dlmwrite('%s', ans, '\\t')
end")

(defvar org-babel-octave-eoe-indicator "\'org_babel_eoe\'")

(defvar org-babel-octave-eoe-output "ans = org_babel_eoe")

(defun org-babel-execute:matlab (body params)
  "Execute a block of matlab code with Babel."
  (org-babel-execute:octave body params 'matlab))

(defun org-babel-execute:octave (body params &optional matlabp)
  "Execute a block of octave code with Babel."
  (let* ((session
	  (funcall (intern (format "org-babel-%s-initiate-session"
				   (if matlabp "matlab" "octave")))
		   (cdr (assoc :session params)) params))
         (vars (mapcar #'cdr (org-babel-get-header params :var)))
         (result-params (cdr (assoc :result-params params)))
         (result-type (cdr (assoc :result-type params)))
	 (out-file (cdr (assoc :file params)))
	 (full-body
	  (org-babel-expand-body:generic
	   body params (org-babel-variable-assignments:octave params)))
	 (result (org-babel-octave-evaluate
		  session
		  (if (org-babel-octave-graphical-output-file params)
		      (mapconcat 'identity
				 (list
				  "set (0, \"defaultfigurevisible\", \"off\");"
				  full-body
				  (format "print -dpng %s" (org-babel-octave-graphical-output-file params)))
				 "\n")
		    full-body)
		  result-type matlabp)))
    (if (org-babel-octave-graphical-output-file params)
	nil
      (org-babel-reassemble-table
       result
       (org-babel-pick-name
	(cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
       (org-babel-pick-name
	(cdr (assoc :rowname-names params)) (cdr (assoc :rownames params)))))))

(defun org-babel-prep-session:matlab (session params)
  "Prepare SESSION according to PARAMS."
  (org-babel-prep-session:octave session params 'matlab))

(defun org-babel-variable-assignments:octave (params)
  "Return list of octave statements assigning the block's variables"
  (mapcar
   (lambda (pair)
     (format "%s=%s;"
	     (car pair)
	     (org-babel-octave-var-to-octave (cdr pair))))
   (mapcar #'cdr (org-babel-get-header params :var))))

(defalias 'org-babel-variable-assignments:matlab
  'org-babel-variable-assignments:octave)

(defun org-babel-octave-var-to-octave (var)
  "Convert an emacs-lisp value into an octave variable.
Converts an emacs-lisp variable into a string of octave code
specifying a variable of the same value."
  (if (listp var)
      (concat "[" (mapconcat #'org-babel-octave-var-to-octave var
			     (if (listp (car var)) "; " ",")) "]")
    (cond
     ((stringp var)
      (format "\'%s\'" var))
     (t
      (format "%s" var)))))

(defun org-babel-prep-session:octave (session params &optional matlabp)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (let* ((session (org-babel-octave-initiate-session session params matlabp))
	 (var-lines (org-babel-variable-assignments:octave params)))
    (org-babel-comint-in-buffer session
      (mapc (lambda (var)
              (end-of-line 1) (insert var) (comint-send-input nil t)
              (org-babel-comint-wait-for-output session)) var-lines))
    session))

(defun org-babel-matlab-initiate-session (&optional session params)
  "Create a matlab inferior process buffer.
If there is not a current inferior-process-buffer in SESSION then
create. Return the initialized session."
  (org-babel-octave-initiate-session session params 'matlab))

(defun org-babel-octave-initiate-session (&optional session params matlabp)
  "Create an octave inferior process buffer.
If there is not a current inferior-process-buffer in SESSION then
create. Return the initialized session."
  (if matlabp (require 'matlab) (require 'octave-inf))
  (unless (string= session "none")
    (let ((session (or session
		       (if matlabp "*Inferior Matlab*" "*Inferior Octave*"))))
      (if (org-babel-comint-buffer-livep session) session
	(save-window-excursion
	  (if matlabp (unless org-babel-matlab-with-emacs-link (matlab-shell))
	    (run-octave))
	  (rename-buffer (if (bufferp session) (buffer-name session)
			   (if (stringp session) session (buffer-name))))
	  (current-buffer))))))

(defun org-babel-octave-evaluate
  (session body result-type &optional matlabp)
  "Pass BODY to the octave process in SESSION.
If RESULT-TYPE equals 'output then return the outputs of the
statements in BODY, if RESULT-TYPE equals 'value then return the
value of the last statement in BODY, as elisp."
  (if session
      (org-babel-octave-evaluate-session session body result-type matlabp)
    (org-babel-octave-evaluate-external-process body result-type matlabp)))

(defun org-babel-octave-evaluate-external-process (body result-type matlabp)
  "Evaluate BODY in an external octave process."
  (let ((cmd (if matlabp
		 org-babel-matlab-shell-command
	       org-babel-octave-shell-command)))
    (case result-type
      (output (org-babel-eval cmd body))
      (value (let ((tmp-file (org-babel-temp-file "octave-")))
	       (org-babel-eval
		cmd
		(format org-babel-octave-wrapper-method body
			(org-babel-process-file-name tmp-file 'noquote)
			(org-babel-process-file-name tmp-file 'noquote)))
	       (org-babel-octave-import-elisp-from-file tmp-file))))))

(defun org-babel-octave-evaluate-session
  (session body result-type &optional matlabp)
  "Evaluate BODY in SESSION."
  (let* ((tmp-file (org-babel-temp-file (if matlabp "matlab-" "octave-")))
	 (wait-file (org-babel-temp-file "matlab-emacs-link-wait-signal-"))
	 (full-body
	  (case result-type
	    (output
	     (mapconcat
	      #'org-babel-chomp
	      (list body org-babel-octave-eoe-indicator) "\n"))
	    (value
	     (if (and matlabp org-babel-matlab-with-emacs-link)
		 (concat
		  (format org-babel-matlab-emacs-link-wrapper-method
			  body
			  (org-babel-process-file-name tmp-file 'noquote)
			  (org-babel-process-file-name tmp-file 'noquote) wait-file) "\n")
	       (mapconcat
		#'org-babel-chomp
		(list (format org-babel-octave-wrapper-method
			      body
			      (org-babel-process-file-name tmp-file 'noquote)
			      (org-babel-process-file-name tmp-file 'noquote))
		      org-babel-octave-eoe-indicator) "\n")))))
	 (raw (if (and matlabp org-babel-matlab-with-emacs-link)
		  (save-window-excursion
		    (with-temp-buffer
		      (insert full-body)
		      (write-region "" 'ignored wait-file nil nil nil 'excl)
		      (matlab-shell-run-region (point-min) (point-max))
		      (message "Waiting for Matlab Emacs Link")
		      (while (file-exists-p wait-file) (sit-for 0.01))
		      "")) ;; matlab-shell-run-region doesn't seem to
			   ;; make *matlab* buffer contents easily
			   ;; available, so :results output currently
			   ;; won't work
		(org-babel-comint-with-output
		    (session
		     (if matlabp
			 org-babel-octave-eoe-indicator
		       org-babel-octave-eoe-output)
		     t full-body)
		  (insert full-body) (comint-send-input nil t)))) results)
    (case result-type
      (value
       (org-babel-octave-import-elisp-from-file tmp-file))
      (output
       (progn
	 (setq results
	       (if matlabp
		   (cdr (reverse (delq "" (mapcar
					   #'org-babel-octave-read-string
					   (mapcar #'org-babel-trim raw)))))
		 (cdr (member org-babel-octave-eoe-output
			      (reverse (mapcar
					#'org-babel-octave-read-string
					(mapcar #'org-babel-trim raw)))))))
	 (mapconcat #'identity (reverse results) "\n"))))))

(defun org-babel-octave-import-elisp-from-file (file-name)
  "Import data from FILE-NAME.
This removes initial blank and comment lines and then calls
`org-babel-import-elisp-from-file'."
  (let ((temp-file (org-babel-temp-file "octave-matlab-")) beg end)
    (with-temp-file temp-file
      (insert-file-contents file-name)
      (re-search-forward "^[ \t]*[^# \t]" nil t)
      (if (< (setq beg (point-min))
	     (setq end (point-at-bol)))
	  (delete-region beg end)))
    (org-babel-import-elisp-from-file temp-file '(16))))

(defun org-babel-octave-read-string (string)
  "Strip \\\"s from around octave string"
  (if (string-match "^\"\\([^\000]+\\)\"$" string)
      (match-string 1 string)
    string))

(defun org-babel-octave-graphical-output-file (params)
  "Name of file to which maxima should send graphical output."
  (and (member "graphics" (cdr (assq :result-params params)))
       (cdr (assq :file params))))

(provide 'ob-octave)



;;; ob-octave.el ends here
