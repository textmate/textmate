;;; idlw-complete-structtag.el --- Completion of structure tags.

;; Copyright (C) 2001-2012  Free Software Foundation, Inc.

;; Author: Carsten Dominik <dominik@astro.uva.nl>
;; Maintainer: J.D. Smith <jdsmith@as.arizona.edu>
;; Version: 1.2
;; Keywords: languages
;; Package: idlwave

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

;; Completion of structure tags can be done automatically in the
;; shell, since the list of tags can be determined dynamically through
;; interaction with IDL.

;; Completion of structure tags in a source buffer is highly ambiguous
;; since you never know what kind of structure a variable will hold at
;; runtime.  To make this feature useful in source buffers, we need a
;; special assumption/convention.  We will assume that the structure is
;; defined in the same buffer and directly assigned to the correct
;; variable.  This is mainly useful for applications in which there is one
;; main structure which contains a large amount of information (and many
;; tags).  For example, many widget applications define a "state" structure
;; that contains all important data about the application.  The different
;; routines called by the event handler then use this structure.  If you
;; use the same variable name for this structure throughout your
;; application (a good idea for many reasons), IDLWAVE can support
;; completion for its tags.
;;
;; This file is a completion plugin which implements this kind of
;; completion. It is also an example which shows how completion plugins
;; should be programmed.
;;
;; New versions of IDLWAVE, documentation, and more information available
;; from:
;;                 http://idlwave.org
;;
;; INSTALLATION
;; ============
;; Put this file on the emacs load path and load it with the following 
;; line in your .emacs file:
;;
;;   (add-hook 'idlwave-load-hook 
;;             (lambda () (require 'idlw-complete-structtag)))
;;
;; DESCRIPTION
;; ===========
;; Suppose your IDL program contains something like
;;
;;     myvar = state.a*
;;
;; where the star marks the cursor position.  If you now press the
;; completion key M-TAB, IDLWAVE searches the current file for a
;; structure definition
;;
;;   state = {tag1:val1, tag2:val2, ...}
;;
;; and offers the tags for completion.
;;
;; In the idlwave shell, idlwave sends a "print,tag_names()" for the
;; variable to idl and determines the current tag list dynamically.
;;
;; Notes
;; -----
;;  - The structure definition assignment "state = {...}" must use the
;;    same variable name as the completion location "state.*".
;;  - The structure definition must be in the same file.
;;  - The structure definition is searched backwards and then forward
;;    from the current position, until a definition with tags is found.
;;  - The file is parsed again for each new completion variable and location.
;;  - You can force an update of the tag list with the usual command
;;    to update routine info in IDLWAVE: C-c C-i

(require 'idlwave)

(declare-function idlwave-shell-buffer "idlw-shell")

;; Some variables to identify the previously used structure
(defvar idlwave-current-tags-var nil)
(defvar idlwave-current-tags-buffer nil)
(defvar idlwave-current-tags-completion-pos nil)

;; The tag list used for completion will be stored in the following vars
(defvar idlwave-current-struct-tags nil)
(defvar idlwave-sint-structtags nil)

;; Create the sintern type for structure talks
(declare-function idlwave-sintern-structtag "idlw-complete-structtag" t t)
(idlwave-new-sintern-type 'structtag)

;; Hook the plugin into idlwave
(add-to-list 'idlwave-complete-special 'idlwave-complete-structure-tag)
(add-hook 'idlwave-update-rinfo-hook 'idlwave-structtag-reset)

;;; The main code follows below
(defvar idlwave-completion-help-info)
(defun idlwave-complete-structure-tag ()
  "Complete a structure tag.
This works by looking in the current file for a structure assignment to a
variable with the same name and takes the tags from there.  Quite useful
for big structures like the state variables of a widget application.

In the idlwave shell, the current content of the variable is used to get
an up-to-date completion list."
  (interactive)
  (let ((pos (point))
        start
	(case-fold-search t))
    (if (save-excursion
	  ;; Check if the context is right.
          ;; In the shell, this could be extended to expressions like
          ;; x[i+4].name.g*.  But it is complicated because we would have
          ;; to really parse this expression.  For now, we allow only
          ;; substructures, like "aaa.bbb.ccc.ddd"
	  (skip-chars-backward "[a-zA-Z0-9._$]")
          (setq start (point)) ;; remember the start of the completion pos.
	  (and (< (point) pos)
	       (not (equal (char-before) ?!)) ; no sysvars
	       (looking-at "\\([a-zA-Z][.a-zA-Z0-9_]*\\)\\.")
	       (>= pos (match-end 0))
	       (not (string= (downcase (match-string 1)) "self"))))
	(let* ((var (downcase (match-string 1))))
	  ;; Check if we need to update the "current" structure.  Basically we
          ;; do it always, except for subsequent completions at the same
          ;; spot, to save a bit of time.  Implementation:  We require
          ;; an update if
          ;; - the variable is different or
          ;; - the buffer is different or
          ;; - we are completing at a different position
	  (if (or (not (string= var (or idlwave-current-tags-var "@")))
		  (not (eq (current-buffer) idlwave-current-tags-buffer))
                  (not (equal start idlwave-current-tags-completion-pos)))
	      (idlwave-prepare-structure-tag-completion var))
          (setq idlwave-current-tags-completion-pos start)
	  (setq idlwave-completion-help-info 
		(list 'idlwave-complete-structure-tag-help))
	  (idlwave-complete-in-buffer 'structtag 'structtag 
				      idlwave-current-struct-tags nil
				      "Select a structure tag" "structure tag")
	  t) ; we did the completion: return t to skip other completions
      nil))) ; return nil to allow looking for other ways to complete

(defun idlwave-structtag-reset ()
  "Force an update of the current structure tag list upon next use."
  (setq idlwave-current-tags-buffer nil))

(defvar idlwave-structtag-struct-location nil
  "The location of the structure definition, for help display.")

(defun idlwave-prepare-structure-tag-completion (var)
  "Find and parse the tag list for structure tag completion."
  ;; This works differently in source buffers and in the shell
  (if (derived-mode-p 'idlwave-shell-mode)
      ;; OK, we are in the shell, do it dynamically
      (progn
        (message "preparing shell tags") 
        ;; The following call puts the tags into `idlwave-current-struct-tags'
        (idlwave-complete-structure-tag-query-shell var)
        ;; initialize
        (setq idlwave-sint-structtags nil
              idlwave-current-tags-buffer (current-buffer)
              idlwave-current-tags-var var
              idlwave-structtag-struct-location (point)
              idlwave-current-struct-tags
              (mapcar (lambda (x)
                        (list (idlwave-sintern-structtag x 'set)))
                      idlwave-current-struct-tags))
        (if (not idlwave-current-struct-tags)
            (error "Cannot complete structure tags of variable %s" var)))
    ;; Not the shell, so probably a source buffer.
    (unless
        (catch 'exit
          (save-excursion
            (goto-char (point-max))
            ;; Find possible definitions of the structure.
            (while (idlwave-find-structure-definition var nil 'all)
              (let ((tags (idlwave-struct-tags)))
                (when tags 
                  ;; initialize
                  (setq idlwave-sint-structtags nil
                        idlwave-current-tags-buffer (current-buffer)
                        idlwave-current-tags-var var
                        idlwave-structtag-struct-location (point)
                        idlwave-current-struct-tags
                        (mapcar (lambda (x)
                                  (list (idlwave-sintern-structtag x 'set)))
                                tags))
                  (throw 'exit t))))))
      (error "Cannot complete structure tags of variable %s" var))))

(defun idlwave-complete-structure-tag-query-shell (var)
  "Ask the shell for the tags of the structure in variable or expression VAR."
  (idlwave-shell-send-command
   (format "if size(%s,/TYPE) eq 8 then print,tag_names(%s)" var var)
   'idlwave-complete-structure-tag-get-tags-from-help
   'hide 'wait))

(defvar idlwave-shell-prompt-pattern)
(defvar idlwave-shell-command-output)
(defun idlwave-complete-structure-tag-get-tags-from-help ()
  "Filter structure tag name output, result to `idlwave-current-struct-tags'."
    (setq idlwave-current-struct-tags
	  (if (string-match (concat "tag_names(.*) *\n"
				    "\\(\\(.*[\r\n]?\\)*\\)"
				    "\\(" idlwave-shell-prompt-pattern "\\)")
			    idlwave-shell-command-output)
	      (split-string (match-string 1 idlwave-shell-command-output)))))


;; Fake help in the source buffer for structure tags.
;; idlw-help-kwd is a global-variable (from idlwave-do-mouse-completion-help).
(defvar idlw-help-kwd)
(defvar idlwave-help-do-struct-tag)
(defun idlwave-complete-structure-tag-help (mode word)
  (cond
   ((eq mode 'test)
    ;; fontify only in source buffers, not in the shell.
    (not (equal idlwave-current-tags-buffer
                (get-buffer (idlwave-shell-buffer)))))
   ((eq mode 'set)
    (setq idlw-help-kwd word
	  idlwave-help-do-struct-tag idlwave-structtag-struct-location))
   (t (error "This should not happen"))))

(provide 'idlw-complete-structtag)

;;; idlw-complete-structtag.el ends here
