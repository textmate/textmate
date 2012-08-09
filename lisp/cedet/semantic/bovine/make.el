;;; semantic/bovine/make.el --- Makefile parsing rules.

;; Copyright (C) 2000-2004, 2008-2012  Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>

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
;; Use the Semantic Bovinator to parse Makefiles.
;; Concocted as an experiment for nonstandard languages.

(require 'make-mode)

(require 'semantic)
(require 'semantic/bovine/make-by)
(require 'semantic/analyze)
(require 'semantic/dep)

(declare-function semantic-analyze-possible-completions-default
		  "semantic/analyze/complete")

;;; Code:
(define-lex-analyzer semantic-lex-make-backslash-no-newline
  "Detect and create a beginning of line token (BOL)."
  (and (looking-at "\\(\\\\\n\\s-*\\)")
       ;; We have a \ at eol.  Push it as whitespace, but pretend
       ;; it never happened so we can skip the BOL tokenizer.
       (semantic-lex-push-token (semantic-lex-token 'whitespace
						    (match-beginning 1)
						    (match-end 1)))
       (goto-char (match-end 1))
       nil) ;; CONTINUE
   ;; We want to skip BOL, so move to the next condition.
   nil)

(define-lex-regex-analyzer semantic-lex-make-command
  "A command in a Makefile consists of a line starting with TAB, and ending at the newline."
  "^\\(\t\\)"
  (let ((start (match-end 0)))
    (while (progn (end-of-line)
		  (save-excursion (forward-char -1) (looking-at "\\\\")))
      (forward-char 1))
    (semantic-lex-push-token
     (semantic-lex-token 'shell-command start (point)))))

(define-lex-regex-analyzer semantic-lex-make-ignore-automake-conditional
  "An automake conditional seems to really bog down the parser.
Ignore them."
  "^@\\(\\w\\|\\s_\\)+@"
  (setq semantic-lex-end-point (match-end 0)))

(define-lex semantic-make-lexer
  "Lexical analyzer for Makefiles."
  semantic-lex-beginning-of-line
  semantic-lex-make-ignore-automake-conditional
  semantic-lex-make-command
  semantic-lex-make-backslash-no-newline
  semantic-lex-whitespace
  semantic-lex-newline
  semantic-lex-symbol-or-keyword
  semantic-lex-charquote
  semantic-lex-paren-or-list
  semantic-lex-close-paren
  semantic-lex-string
  semantic-lex-ignore-comments
  semantic-lex-punctuation
  semantic-lex-default-action)

(defun semantic-make-expand-tag (tag)
  "Expand TAG into a list of equivalent tags, or nil."
  (let ((name (semantic-tag-name tag))
        xpand)
    ;(message "Expanding %S" name)
    ;(goto-char (semantic-tag-start tag))
    ;(sit-for 0)
    (if (and (consp name)
	     (memq (semantic-tag-class tag) '(function include))
	     (> (length name) 1))
	(while name
	  (setq xpand (cons (semantic-tag-clone tag (car name)) xpand)
		name  (cdr name)))
      ;; Else, only a single name.
      (when (consp name)
	(setcar tag (car name)))
      (setq xpand (list tag)))
    xpand))

(define-mode-local-override semantic-get-local-variables
  makefile-mode (&optional point)
  "Override `semantic-get-local-variables' so it does not throw an error.
We never have local variables in Makefiles."
  nil)

(define-mode-local-override semantic-ctxt-current-class-list
  makefile-mode (&optional point)
  "List of classes that are valid to place at point."
  (let ((tag (semantic-current-tag)))
    (when tag
      (cond ((condition-case nil
		 (save-excursion
		   (condition-case nil (forward-sexp -1)
		     (error nil))
		   (forward-char -2)
		   (looking-at "\\$\\s("))
	       (error nil))
	     ;; We are in a variable reference
	     '(variable))
	    ((semantic-tag-of-class-p tag 'function)
	     ;; Note: variables are handled above.
	     '(function filename))
	    ((semantic-tag-of-class-p tag 'variable)
	     '(function filename))
	    ))))

(define-mode-local-override semantic-format-tag-abbreviate
  makefile-mode (tag &optional parent color)
  "Return an abbreviated string describing tag for Makefiles."
  (let ((class (semantic-tag-class tag))
	(name (semantic-format-tag-name tag parent color))
	)
    (cond ((eq class 'function)
	   (concat name ":"))
	  ((eq class 'filename)
	   (concat "./" name))
	  (t
	   (semantic-format-tag-abbreviate-default tag parent color)))))

(defvar-mode-local makefile-mode semantic-function-argument-separator
  " "
  "Separator used between dependencies to rules.")

(define-mode-local-override semantic-format-tag-prototype
  makefile-mode (tag &optional parent color)
  "Return a prototype string describing tag for Makefiles."
  (let* ((class (semantic-tag-class tag))
	 (name (semantic-format-tag-name tag parent color))
	 )
    (cond ((eq class 'function)
	   (concat name ": "
		   (semantic--format-tag-arguments
		    (semantic-tag-function-arguments tag)
		    #'semantic-format-tag-prototype
		    color)))
	  ((eq class 'filename)
	   (concat "./" name))
	  (t
	   (semantic-format-tag-prototype-default tag parent color)))))

(define-mode-local-override semantic-format-tag-concise-prototype
  makefile-mode (tag &optional parent color)
  "Return a concise prototype string describing tag for Makefiles.
This is the same as a regular prototype."
  (semantic-format-tag-prototype tag parent color))

(define-mode-local-override semantic-format-tag-uml-prototype
  makefile-mode (tag &optional parent color)
  "Return a UML prototype string describing tag for Makefiles.
This is the same as a regular prototype."
  (semantic-format-tag-prototype tag parent color))

(define-mode-local-override semantic-analyze-possible-completions
  makefile-mode (context)
  "Return a list of possible completions in a Makefile.
Uses default implementation, and also gets a list of filenames."
  (save-excursion
    (require 'semantic/analyze/complete)
    (set-buffer (oref context buffer))
    (let* ((normal (semantic-analyze-possible-completions-default context))
	   (classes (oref context :prefixclass))
	   (filetags nil))
      (when (memq 'filename classes)
	(let* ((prefix (car (oref context :prefix)))
	       (completetext (cond ((semantic-tag-p prefix)
				    (semantic-tag-name prefix))
				   ((stringp prefix)
				    prefix)
				   ((stringp (car prefix))
				    (car prefix))))
	       (files (directory-files default-directory nil
				       (concat "^" completetext))))
	  (setq filetags (mapcar (lambda (f) (semantic-tag f 'filename))
				 files))))
      ;; Return the normal completions found, plus any filenames
      ;; that match.
      (append normal filetags)
      )))

(defcustom-mode-local-semantic-dependency-system-include-path
  makefile-mode semantic-makefile-dependency-system-include-path
  nil
  "The system include path used by Makefiles language.")

;;;###autoload
(defun semantic-default-make-setup ()
  "Set up a Makefile buffer for parsing with semantic."
  (semantic-make-by--install-parser)
  (setq semantic-symbol->name-assoc-list '((variable . "Variables")
                                           (function . "Rules")
                                           (include . "Dependencies")
					   ;; File is a meta-type created
					   ;; to represent completions
					   ;; but not actually parsed.
					   (file . "File"))
        semantic-case-fold t
        semantic-tag-expand-function 'semantic-make-expand-tag
        semantic-lex-syntax-modifications '((?. "_")
                                            (?= ".")
                                            (?/ "_")
                                            (?$ ".")
                                            (?+ ".")
                                            (?\\ ".")
                                            )
        imenu-create-index-function 'semantic-create-imenu-index
        )
  (setq semantic-lex-analyzer #'semantic-make-lexer)
  )

(provide 'semantic/bovine/make)

;; Local variables:
;; generated-autoload-file: "../loaddefs.el"
;; generated-autoload-load-name: "semantic/bovine/make"
;; End:

;;; semantic/bovine/make.el ends here
