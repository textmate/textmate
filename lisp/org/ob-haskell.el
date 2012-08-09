;;; ob-haskell.el --- org-babel functions for haskell evaluation

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

;; Org-Babel support for evaluating haskell source code.  This one will
;; be sort of tricky because haskell programs must be compiled before
;; they can be run, but haskell code can also be run through an
;; interactive interpreter.
;;
;; For now lets only allow evaluation using the haskell interpreter.

;;; Requirements:

;; - haskell-mode :: http://www.iro.umontreal.ca/~monnier/elisp/#haskell-mode
;;
;; - inf-haskell :: http://www.iro.umontreal.ca/~monnier/elisp/#haskell-mode
;;
;; - (optionally) lhs2tex :: http://people.cs.uu.nl/andres/lhs2tex/

;;; Code:
(require 'ob)
(require 'ob-comint)
(require 'comint)
(eval-when-compile (require 'cl))

(declare-function org-remove-indentation "org" (code &optional n))
(declare-function haskell-mode "ext:haskell-mode" ())
(declare-function run-haskell "ext:inf-haskell" (&optional arg))
(declare-function inferior-haskell-load-file
		  "ext:inf-haskell" (&optional reload))

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("haskell" . "hs"))

(defvar org-babel-default-header-args:haskell '())

(defvar org-babel-haskell-lhs2tex-command "lhs2tex")

(defvar org-babel-haskell-eoe "\"org-babel-haskell-eoe\"")

(defun org-babel-execute:haskell (body params)
  "Execute a block of Haskell code."
  (let* ((session (cdr (assoc :session params)))
         (vars (mapcar #'cdr (org-babel-get-header params :var)))
         (result-type (cdr (assoc :result-type params)))
         (full-body (org-babel-expand-body:generic
		     body params
		     (org-babel-variable-assignments:haskell params)))
         (session (org-babel-haskell-initiate-session session params))
         (raw (org-babel-comint-with-output
		  (session org-babel-haskell-eoe t full-body)
                (insert (org-babel-trim full-body))
                (comint-send-input nil t)
                (insert org-babel-haskell-eoe)
                (comint-send-input nil t)))
         (results (mapcar
                   #'org-babel-haskell-read-string
                   (cdr (member org-babel-haskell-eoe
                                (reverse (mapcar #'org-babel-trim raw)))))))
    (org-babel-reassemble-table
     (cond
      ((equal result-type 'output)
       (mapconcat #'identity (reverse (cdr results)) "\n"))
      ((equal result-type 'value)
       (org-babel-haskell-table-or-string (car results))))
     (org-babel-pick-name (cdr (assoc :colname-names params))
			  (cdr (assoc :colname-names params)))
     (org-babel-pick-name (cdr (assoc :rowname-names params))
			  (cdr (assoc :rowname-names params))))))

(defun org-babel-haskell-read-string (string)
  "Strip \\\"s from around a haskell string."
  (if (string-match "^\"\\([^\000]+\\)\"$" string)
      (match-string 1 string)
    string))

(defun org-babel-haskell-initiate-session (&optional session params)
  "Initiate a haskell session.
If there is not a current inferior-process-buffer in SESSION
then create one.  Return the initialized session."
  (require 'inf-haskell)
  (or (get-buffer "*haskell*")
      (save-window-excursion (run-haskell) (sleep-for 0.25) (current-buffer))))

(defun org-babel-load-session:haskell (session body params)
  "Load BODY into SESSION."
  (save-window-excursion
    (let* ((buffer (org-babel-prep-session:haskell session params))
           (load-file (concat (org-babel-temp-file "haskell-load-") ".hs")))
      (with-temp-buffer
        (insert body) (write-file load-file)
        (haskell-mode) (inferior-haskell-load-file))
      buffer)))

(defun org-babel-prep-session:haskell (session params)
  "Prepare SESSION according to the header arguments in PARAMS."
  (save-window-excursion
    (let ((buffer (org-babel-haskell-initiate-session session)))
      (org-babel-comint-in-buffer buffer
      	(mapc (lambda (line)
		(insert line)
		(comint-send-input nil t))
	      (org-babel-variable-assignments:haskell params)))
      (current-buffer))))

(defun org-babel-variable-assignments:haskell (params)
  "Return list of haskell statements assigning the block's variables"
  (mapcar (lambda (pair)
	    (format "let %s = %s"
		    (car pair)
		    (org-babel-haskell-var-to-haskell (cdr pair))))
   (mapcar #'cdr (org-babel-get-header params :var))))

(defun org-babel-haskell-table-or-string (results)
  "Convert RESULTS to an Emacs-lisp table or string.
If RESULTS look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (org-babel-script-escape results))

(defun org-babel-haskell-var-to-haskell (var)
  "Convert an elisp value VAR into a haskell variable.
The elisp VAR is converted to a string of haskell source code
specifying a variable of the same value."
  (if (listp var)
      (concat "[" (mapconcat #'org-babel-haskell-var-to-haskell var ", ") "]")
    (format "%S" var)))

(defvar org-src-preserve-indentation)
(defun org-babel-haskell-export-to-lhs (&optional arg)
  "Export to a .lhs file with all haskell code blocks escaped.
When called with a prefix argument the resulting
.lhs file will be exported to a .tex file.  This function will
create two new files, base-name.lhs and base-name.tex where
base-name is the name of the current org-mode file.

Note that all standard Babel literate programming
constructs (header arguments, no-web syntax etc...) are ignored."
  (interactive "P")
  (let* ((contents (buffer-string))
         (haskell-regexp
          (concat "^\\([ \t]*\\)#\\+begin_src[ \t]haskell*\\(.*\\)?[\r\n]"
                  "\\([^\000]*?\\)[\r\n][ \t]*#\\+end_src.*"))
         (base-name (file-name-sans-extension (buffer-file-name)))
         (tmp-file (org-babel-temp-file "haskell-"))
         (tmp-org-file (concat tmp-file ".org"))
         (tmp-tex-file (concat tmp-file ".tex"))
         (lhs-file (concat base-name ".lhs"))
         (tex-file (concat base-name ".tex"))
         (command (concat org-babel-haskell-lhs2tex-command
			  " " (org-babel-process-file-name lhs-file)
			  " > " (org-babel-process-file-name tex-file)))
         (preserve-indentp org-src-preserve-indentation)
         indentation)
    ;; escape haskell source-code blocks
    (with-temp-file tmp-org-file
      (insert contents)
      (goto-char (point-min))
      (while (re-search-forward haskell-regexp nil t)
        (save-match-data (setq indentation (length (match-string 1))))
        (replace-match (save-match-data
                         (concat
                          "#+begin_latex\n\\begin{code}\n"
                          (if (or preserve-indentp
                                  (string-match "-i" (match-string 2)))
                              (match-string 3)
                            (org-remove-indentation (match-string 3)))
                          "\n\\end{code}\n#+end_latex\n"))
                       t t)
        (indent-code-rigidly (match-beginning 0) (match-end 0) indentation)))
    (save-excursion
      ;; export to latex w/org and save as .lhs
      (find-file tmp-org-file) (funcall 'org-export-as-latex nil)
      (kill-buffer nil)
      (delete-file tmp-org-file)
      (find-file tmp-tex-file)
      (goto-char (point-min)) (forward-line 2)
      (insert "%include polycode.fmt\n")
      ;; ensure all \begin/end{code} statements start at the first column
      (while (re-search-forward "^[ \t]+\\\\begin{code}[^\000]+\\\\end{code}" nil t)
        (replace-match (save-match-data (org-remove-indentation (match-string 0)))
                       t t))
      (setq contents (buffer-string))
      (save-buffer) (kill-buffer nil))
    (delete-file tmp-tex-file)
    ;; save org exported latex to a .lhs file
    (with-temp-file lhs-file (insert contents))
    (if (not arg)
        (find-file lhs-file)
      ;; process .lhs file with lhs2tex
      (message "running %s" command) (shell-command command) (find-file tex-file))))

(provide 'ob-haskell)



;;; ob-haskell.el ends here
