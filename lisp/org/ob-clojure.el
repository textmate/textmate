;;; ob-clojure.el --- org-babel functions for clojure evaluation

;; Copyright (C) 2009-2012  Free Software Foundation, Inc.

;; Author: Joel Boehland
;;	Eric Schulte
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

;;; support for evaluating clojure code, relies on slime for all eval

;;; Requirements:

;;; - clojure (at least 1.2.0)
;;; - clojure-mode
;;; - slime

;;; By far, the best way to install these components is by following
;;; the directions as set out by Phil Hagelberg (Technomancy) on the
;;; web page: http://technomancy.us/126

;;; Code:
(require 'ob)

(declare-function slime-eval "ext:slime" (sexp &optional package))

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj"))

(defvar org-babel-default-header-args:clojure '())
(defvar org-babel-header-arg-names:clojure '(package))

(defun org-babel-expand-body:clojure (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let* ((vars (mapcar #'cdr (org-babel-get-header params :var)))
	 (result-params (cdr (assoc :result-params params)))
	 (print-level nil) (print-length nil)
	 (body (org-babel-trim
		(if (> (length vars) 0)
		    (concat "(let ["
			    (mapconcat
			     (lambda (var)
			       (format "%S (quote %S)" (car var) (cdr var)))
			     vars "\n      ")
			    "]\n" body ")")
		  body))))
    (cond ((or (member "code" result-params) (member "pp" result-params))
	   (format (concat "(let [org-mode-print-catcher (java.io.StringWriter.)] "
			   "(clojure.pprint/with-pprint-dispatch clojure.pprint/%s-dispatch "
			   "(clojure.pprint/pprint (do %s) org-mode-print-catcher) "
			   "(str org-mode-print-catcher)))")
		   (if (member "code" result-params) "code" "simple") body))
	  ;; if (:results output), collect printed output
	  ((member "output" result-params)
	   (format "(clojure.core/with-out-str %s)" body))
	  (t body))))

(defun org-babel-execute:clojure (body params)
  "Execute a block of Clojure code with Babel."
  (require 'slime)
  (with-temp-buffer
    (insert (org-babel-expand-body:clojure body params))
    ((lambda (result)
       (let ((result-params (cdr (assoc :result-params params))))
	 (if (or (member "scalar" result-params)
		 (member "verbatim" result-params))
	     result
	   (condition-case nil (org-babel-script-escape result)
	     (error result)))))
     (slime-eval
      `(swank:eval-and-grab-output
     	,(buffer-substring-no-properties (point-min) (point-max)))
      (cdr (assoc :package params))))))

(provide 'ob-clojure)



;;; ob-clojure.el ends here
