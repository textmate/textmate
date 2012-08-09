;;; ob-R.el --- org-babel functions for R code evaluation

;; Copyright (C) 2009-2012  Free Software Foundation, Inc.

;; Author: Eric Schulte
;;	Dan Davison
;; Keywords: literate programming, reproducible research, R, statistics
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

;; Org-Babel support for evaluating R code

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
(eval-when-compile (require 'cl))

(declare-function orgtbl-to-tsv "org-table" (table params))
(declare-function R "ext:essd-r" (&optional start-args))
(declare-function inferior-ess-send-input "ext:ess-inf" ())
(declare-function ess-make-buffer-current "ext:ess-inf" ())
(declare-function ess-eval-buffer "ext:ess-inf" (vis))
(declare-function org-number-sequence "org-compat" (from &optional to inc))

(defconst org-babel-header-arg-names:R
  '(width height bg units pointsize antialias quality compression
	  res type family title fonts version paper encoding
	  pagecentre colormodel useDingbats horizontal)
  "R-specific header arguments.")

(defvar org-babel-default-header-args:R '())

(defvar org-babel-R-command "R --slave --no-save"
  "Name of command to use for executing R code.")

(defvar ess-local-process-name)
(defun org-babel-edit-prep:R (info)
  (let ((session (cdr (assoc :session (nth 2 info)))))
    (when (and session (string-match "^\\*\\(.+?\\)\\*$" session))
      (save-match-data (org-babel-R-initiate-session session nil))
      (setq ess-local-process-name (match-string 1 session)))))

(defun org-babel-expand-body:R (body params &optional graphics-file)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((graphics-file
	 (or graphics-file (org-babel-R-graphical-output-file params))))
    (mapconcat
     #'identity
     ((lambda (inside)
	(if graphics-file
	    (append
	     (list (org-babel-R-construct-graphics-device-call
		    graphics-file params))
	     inside
	     (list "dev.off()"))
	  inside))
      (append (org-babel-variable-assignments:R params)
	      (list body))) "\n")))

(defun org-babel-execute:R (body params)
  "Execute a block of R code.
This function is called by `org-babel-execute-src-block'."
  (save-excursion
    (let* ((result-params (cdr (assoc :result-params params)))
	   (result-type (cdr (assoc :result-type params)))
           (session (org-babel-R-initiate-session
		     (cdr (assoc :session params)) params))
	   (colnames-p (cdr (assoc :colnames params)))
	   (rownames-p (cdr (assoc :rownames params)))
	   (graphics-file (org-babel-R-graphical-output-file params))
	   (full-body (org-babel-expand-body:R body params graphics-file))
	   (result
	    (org-babel-R-evaluate
	     session full-body result-type result-params
	     (or (equal "yes" colnames-p)
		 (org-babel-pick-name
		  (cdr (assoc :colname-names params)) colnames-p))
	     (or (equal "yes" rownames-p)
		 (org-babel-pick-name
		  (cdr (assoc :rowname-names params)) rownames-p)))))
      (if graphics-file nil result))))

(defun org-babel-prep-session:R (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (let* ((session (org-babel-R-initiate-session session params))
	 (var-lines (org-babel-variable-assignments:R params)))
    (org-babel-comint-in-buffer session
      (mapc (lambda (var)
              (end-of-line 1) (insert var) (comint-send-input nil t)
              (org-babel-comint-wait-for-output session)) var-lines))
    session))

(defun org-babel-load-session:R (session body params)
  "Load BODY into SESSION."
  (save-window-excursion
    (let ((buffer (org-babel-prep-session:R session params)))
      (with-current-buffer buffer
        (goto-char (process-mark (get-buffer-process (current-buffer))))
        (insert (org-babel-chomp body)))
      buffer)))

;; helper functions

(defun org-babel-variable-assignments:R (params)
  "Return list of R statements assigning the block's variables"
  (let ((vars (mapcar #'cdr (org-babel-get-header params :var))))
    (mapcar
     (lambda (pair)
       (org-babel-R-assign-elisp
	(car pair) (cdr pair)
	(equal "yes" (cdr (assoc :colnames params)))
	(equal "yes" (cdr (assoc :rownames params)))))
     (mapcar
      (lambda (i)
	(cons (car (nth i vars))
	      (org-babel-reassemble-table
	       (cdr (nth i vars))
	       (cdr (nth i (cdr (assoc :colname-names params))))
	       (cdr (nth i (cdr (assoc :rowname-names params)))))))
      (org-number-sequence 0 (1- (length vars)))))))

(defun org-babel-R-quote-tsv-field (s)
  "Quote field S for export to R."
  (if (stringp s)
      (concat "\"" (mapconcat 'identity (split-string s "\"") "\"\"") "\"")
    (format "%S" s)))

(defun org-babel-R-assign-elisp (name value colnames-p rownames-p)
  "Construct R code assigning the elisp VALUE to a variable named NAME."
  (if (listp value)
      (let ((transition-file (org-babel-temp-file "R-import-")))
        ;; ensure VALUE has an orgtbl structure (depth of at least 2)
        (unless (listp (car value)) (setq value (list value)))
        (with-temp-file transition-file
          (insert (orgtbl-to-tsv value '(:fmt org-babel-R-quote-tsv-field)))
          (insert "\n"))
        (format "%s <- read.table(\"%s\", header=%s, row.names=%s, sep=\"\\t\", as.is=TRUE)"
                name (org-babel-process-file-name transition-file 'noquote)
		(if (or (eq (nth 1 value) 'hline) colnames-p) "TRUE" "FALSE")
		(if rownames-p "1" "NULL")))
    (format "%s <- %s" name (org-babel-R-quote-tsv-field value))))

(defvar ess-ask-for-ess-directory nil)
(defun org-babel-R-initiate-session (session params)
  "If there is not a current R process then create one."
  (unless (string= session "none")
    (let ((session (or session "*R*"))
	  (ess-ask-for-ess-directory
	   (and ess-ask-for-ess-directory (not (cdr (assoc :dir params))))))
      (if (org-babel-comint-buffer-livep session)
	  session
	(save-window-excursion
	  (require 'ess) (R)
	  (rename-buffer
	   (if (bufferp session)
	       (buffer-name session)
	     (if (stringp session)
		 session
	       (buffer-name))))
	  (current-buffer))))))

(defvar ess-local-process-name nil)
(defun org-babel-R-associate-session (session)
  "Associate R code buffer with an R session.
Make SESSION be the inferior ESS process associated with the
current code buffer."
  (setq ess-local-process-name
	(process-name (get-buffer-process session)))
  (ess-make-buffer-current))

(defun org-babel-R-graphical-output-file (params)
  "Name of file to which R should send graphical output."
  (and (member "graphics" (cdr (assq :result-params params)))
       (cdr (assq :file params))))

(defun org-babel-R-construct-graphics-device-call (out-file params)
  "Construct the call to the graphics device."
  (let ((devices
	 '((:bmp . "bmp")
	   (:jpg . "jpeg")
	   (:jpeg . "jpeg")
	   (:tex . "tikz")
	   (:tiff . "tiff")
	   (:png . "png")
	   (:svg . "svg")
	   (:pdf . "pdf")
	   (:ps . "postscript")
	   (:postscript . "postscript")))
	(allowed-args '(:width :height :bg :units :pointsize
			       :antialias :quality :compression :res
			       :type :family :title :fonts :version
			       :paper :encoding :pagecentre :colormodel
			       :useDingbats :horizontal))
	(device (and (string-match ".+\\.\\([^.]+\\)" out-file)
		     (match-string 1 out-file)))
	(extra-args (cdr (assq :R-dev-args params))) filearg args)
    (setq device (or (and device (cdr (assq (intern (concat ":" device))
					    devices))) "png"))
    (setq filearg
	  (if (member device '("pdf" "postscript" "svg" "tikz")) "file" "filename"))
    (setq args (mapconcat
		(lambda (pair)
		  (if (member (car pair) allowed-args)
		      (format ",%s=%s"
			      (substring (symbol-name (car pair)) 1)
			      (cdr pair)) ""))
		params ""))
    (format "%s(%s=\"%s\"%s%s%s)"
	    device filearg out-file args
	    (if extra-args "," "") (or extra-args ""))))

(defvar org-babel-R-eoe-indicator "'org_babel_R_eoe'")
(defvar org-babel-R-eoe-output "[1] \"org_babel_R_eoe\"")

(defvar org-babel-R-write-object-command "{function(object,transfer.file){object;invisible(if(inherits(try({tfile<-tempfile();write.table(object,file=tfile,sep=\"\\t\",na=\"nil\",row.names=%s,col.names=%s,quote=FALSE);file.rename(tfile,transfer.file)},silent=TRUE),\"try-error\")){if(!file.exists(transfer.file))file.create(transfer.file)})}}(object=%s,transfer.file=\"%s\")")

(defun org-babel-R-evaluate
  (session body result-type result-params column-names-p row-names-p)
  "Evaluate R code in BODY."
  (if session
      (org-babel-R-evaluate-session
       session body result-type result-params column-names-p row-names-p)
    (org-babel-R-evaluate-external-process
     body result-type result-params column-names-p row-names-p)))

(defun org-babel-R-evaluate-external-process
  (body result-type result-params column-names-p row-names-p)
  "Evaluate BODY in external R process.
If RESULT-TYPE equals 'output then return standard output as a
string. If RESULT-TYPE equals 'value then return the value of the
last statement in BODY, as elisp."
  (case result-type
    (value
     (let ((tmp-file (org-babel-temp-file "R-")))
       (org-babel-eval org-babel-R-command
		       (format org-babel-R-write-object-command
			       (if row-names-p "TRUE" "FALSE")
			       (if column-names-p
				   (if row-names-p "NA" "TRUE")
				 "FALSE")
			       (format "{function ()\n{\n%s\n}}()" body)
			       (org-babel-process-file-name tmp-file 'noquote)))
       (org-babel-R-process-value-result
	(if (or (member "scalar" result-params)
		(member "verbatim" result-params))
	    (with-temp-buffer
	      (insert-file-contents tmp-file)
	      (buffer-string))
	  (org-babel-import-elisp-from-file tmp-file '(16)))
	column-names-p)))
    (output (org-babel-eval org-babel-R-command body))))

(defun org-babel-R-evaluate-session
  (session body result-type result-params column-names-p row-names-p)
  "Evaluate BODY in SESSION.
If RESULT-TYPE equals 'output then return standard output as a
string. If RESULT-TYPE equals 'value then return the value of the
last statement in BODY, as elisp."
  (case result-type
    (value
     (with-temp-buffer
       (insert (org-babel-chomp body))
       (let ((ess-local-process-name
	      (process-name (get-buffer-process session)))
	     (ess-eval-visibly-p nil))
	 (ess-eval-buffer nil)))
     (let ((tmp-file (org-babel-temp-file "R-")))
       (org-babel-comint-eval-invisibly-and-wait-for-file
	session tmp-file
	(format org-babel-R-write-object-command
		(if row-names-p "TRUE" "FALSE")
		(if column-names-p
		    (if row-names-p "NA" "TRUE")
		  "FALSE")
		".Last.value" (org-babel-process-file-name tmp-file 'noquote)))
       (org-babel-R-process-value-result
	(if (or (member "scalar" result-params)
		(member "verbatim" result-params))
	    (with-temp-buffer
	      (insert-file-contents tmp-file)
	      (buffer-string))
	  (org-babel-import-elisp-from-file tmp-file '(16)))
	column-names-p)))
    (output
     (mapconcat
      #'org-babel-chomp
      (butlast
       (delq nil
	     (mapcar
	      (lambda (line) (when (> (length line) 0) line))
	      (mapcar
	       (lambda (line) ;; cleanup extra prompts left in output
		 (if (string-match
		      "^\\([ ]*[>+\\.][ ]?\\)+\\([[0-9]+\\|[ ]\\)" line)
		     (substring line (match-end 1))
		   line))
	       (org-babel-comint-with-output (session org-babel-R-eoe-output)
		 (insert (mapconcat #'org-babel-chomp
				    (list body org-babel-R-eoe-indicator)
				    "\n"))
		 (inferior-ess-send-input)))))) "\n"))))

(defun org-babel-R-process-value-result (result column-names-p)
  "R-specific processing of return value.
Insert hline if column names in output have been requested."
  (if column-names-p
      (cons (car result) (cons 'hline (cdr result)))
    result))

(provide 'ob-R)



;;; ob-R.el ends here
