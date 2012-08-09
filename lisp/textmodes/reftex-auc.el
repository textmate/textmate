;;; reftex-auc.el --- RefTeX's interface to AUCTeX

;; Copyright (C) 1997-2012 Free Software Foundation, Inc.

;; Author: Carsten Dominik <dominik@science.uva.nl>
;; Maintainer: auctex-devel@gnu.org
;; Version: 4.31
;; Package: reftex

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

;;; Code:

(eval-when-compile (require 'cl))
(provide 'reftex-auc)
(require 'reftex)
;;;

(declare-function TeX-argument-insert "ext:tex" (name optional &optional prefix))
(declare-function TeX-argument-prompt "ext:tex" (optional prompt default &optional complete))
(declare-function multi-prompt "ext:multi-prompt" 
		  (separator
		   unique prompt table
		   &optional mp-predicate require-match initial history))
(declare-function LaTeX-add-index-entries "ext:tex" (&rest entries) t)
(declare-function LaTeX-add-labels "ext:tex" (&rest entries) t)
(declare-function LaTeX-bibitem-list "ext:tex" () t)
(declare-function LaTeX-index-entry-list "ext:tex" () t)
(declare-function LaTeX-label-list "ext:tex" () t)

(defun reftex-plug-flag (which)
  ;; Tell if a certain flag is set in reftex-plug-into-AUCTeX
  (or (eq t reftex-plug-into-AUCTeX)
      (and (listp reftex-plug-into-AUCTeX)
           (nth which reftex-plug-into-AUCTeX))))

(defun reftex-arg-label (optional &optional prompt definition)
  "Use `reftex-label', `reftex-reference' or AUCTeX's code to insert label arg.
What is being used depends upon `reftex-plug-into-AUCTeX'."
  (let (label)
    (cond
     ((and definition (reftex-plug-flag 1))
      ;; Create a new label, with a temporary brace for `reftex-what-macro'
      (unwind-protect
          (progn (insert "{") (setq label (or (reftex-label nil t) "")))
        (delete-char -1)))
     ((and (not definition) (reftex-plug-flag 2))
      ;; Reference a label with RefTeX
      (setq label (reftex-reference nil t)))
     (t
      ;; AUCTeX's default mechanism
      (setq label (completing-read (TeX-argument-prompt optional prompt "Key")
                                   (LaTeX-label-list)))))
    (if (and definition (not (string-equal "" label)))
        (LaTeX-add-labels label))
    (TeX-argument-insert label optional)))

(defun reftex-arg-cite (optional &optional prompt definition)
  "Use `reftex-citation' or AUCTeX's code to insert a cite-key macro argument.
What is being used depends upon `reftex-plug-into-AUCTeX'."
  (let (items)
    (cond
     ((and (not definition) (reftex-plug-flag 3))
      (setq items (list (or (reftex-citation t) ""))))
     (t
      (setq prompt (concat (if optional "(Optional) " "")
			   (if prompt prompt "Add key")
			   " (default none): "))
      (setq items (multi-prompt "," t prompt (LaTeX-bibitem-list)))))
    (apply 'LaTeX-add-bibitems items)
    (TeX-argument-insert (mapconcat 'identity items ",") optional)))


(defun reftex-arg-index-tag (optional &optional prompt &rest args)
  "Prompt for an index tag with completion.
This is the name of an index, not the entry."
  (let (tag taglist)
    (setq prompt (concat (if optional "(Optional) " "")
			 (if prompt prompt "Index tag")
			 " (default none): "))
    (if (and reftex-support-index (reftex-plug-flag 4))
        ;; Use RefTeX completion
        (progn
          (reftex-access-scan-info nil)
          (setq taglist
                (cdr (assoc 'index-tags
                            (symbol-value reftex-docstruct-symbol)))
                tag (completing-read prompt (mapcar 'list taglist))))
      ;; Just ask like AUCTeX does.
      (setq tag (read-string prompt)))
    (TeX-argument-insert tag optional)))

(defun reftex-arg-index (optional &optional prompt &rest args)
  "Prompt for an index entry completing with known entries.
Completion is specific for just one index, if the macro or a tag
argument identify one of multiple indices."
  (let* (tag key)
    (if (and reftex-support-index (reftex-plug-flag 4))
        (progn
          (reftex-access-scan-info nil)
          (setq tag (reftex-what-index-tag)
                key (reftex-index-complete-key (or tag "idx"))))
      (setq key (completing-read (TeX-argument-prompt optional prompt "Key")
                                 (LaTeX-index-entry-list))))
    (unless (string-equal "" key)
      (LaTeX-add-index-entries key))
    (TeX-argument-insert key optional)))

(defun reftex-what-index-tag ()
  ;; Look backward to find out what index the macro at point belongs to
  (let ((macro (save-excursion
                 (and (re-search-backward "\\\\[a-zA-Z*]+" nil t)
                      (match-string 0))))
        tag entry)
    (when (and macro
               (setq entry (assoc macro reftex-index-macro-alist)))
      (setq tag (nth 1 entry))
      (cond
       ((stringp tag) tag)
       ((integerp tag)
        (save-excursion
          (goto-char (match-end 1))
          (or (reftex-nth-arg tag (nth 6 entry)) "idx")))
       (t "idx")))))

(defvar LaTeX-label-function)
(defun reftex-plug-into-AUCTeX ()
  ;; Replace AUCTeX functions with RefTeX functions.
  ;; Which functions are replaced is controlled by the variable
  ;; `reftex-plug-into-AUCTeX'.

  (if (reftex-plug-flag 0)
      (setq LaTeX-label-function 'reftex-label)
    (setq LaTeX-label-function nil))

  (and (or (reftex-plug-flag 1) (reftex-plug-flag 2))
       (fboundp 'TeX-arg-label)
       (fset 'TeX-arg-label 'reftex-arg-label))

  (and (reftex-plug-flag 3)
       (fboundp 'TeX-arg-cite)
       (fset 'TeX-arg-cite 'reftex-arg-cite))

  (and (reftex-plug-flag 4)
       (fboundp 'TeX-arg-index-tag)
       (fset 'TeX-arg-index-tag 'reftex-arg-index-tag))
  (and (reftex-plug-flag 4)
       (fboundp 'TeX-arg-index)
       (fset 'TeX-arg-index 'reftex-arg-index)))

(defun reftex-toggle-plug-into-AUCTeX ()
  "Toggle Interface between AUCTeX and RefTeX on and off."
  (interactive)
  (unless (and (featurep 'tex-site) (featurep 'latex))
    (error "AUCTeX's LaTeX mode does not seem to be loaded"))
  (setq reftex-plug-into-AUCTeX (not reftex-plug-into-AUCTeX))
  (reftex-plug-into-AUCTeX)
  (if reftex-plug-into-AUCTeX
      (message "RefTeX has been plugged into AUCTeX.")
    (message "RefTeX no longer interacts with AUCTeX.")))

(defun reftex-add-label-environments (entry-list)
  "Add label environment descriptions to `reftex-label-alist-style'.
The format of ENTRY-LIST is exactly like `reftex-label-alist'.  See there
for details.
This function makes it possible to support RefTeX from AUCTeX style files.
The entries in ENTRY-LIST will be processed after the user settings in
`reftex-label-alist', and before the defaults (specified in
`reftex-default-label-alist-entries').  Any changes made to
`reftex-label-alist-style' will raise a flag to the effect that
the label information is recompiled on next use."
  (unless reftex-docstruct-symbol
    (reftex-tie-multifile-symbols))
  (when (and reftex-docstruct-symbol
             (symbolp reftex-docstruct-symbol))
    (let ((list (get reftex-docstruct-symbol 'reftex-label-alist-style))
          entry changed)
      (while entry-list
        (setq entry (pop entry-list))
        (unless (member entry list)
          (setq reftex-tables-dirty t
                changed t)
          (push entry list)))
      (when changed
        (put reftex-docstruct-symbol 'reftex-label-alist-style list)))))
(defalias 'reftex-add-to-label-alist 'reftex-add-label-environments)

(defun reftex-add-section-levels (entry-list)
  "Add entries to the value of `reftex-section-levels'.
The added values are kept local to the current document.  The format
of ENTRY-LIST is a list of cons cells (\"MACRONAME\" . LEVEL).  See
`reftex-section-levels' for an example."
  (unless reftex-docstruct-symbol
    (reftex-tie-multifile-symbols))
  (when (and reftex-docstruct-symbol
             (symbolp reftex-docstruct-symbol))
    (let ((list (get reftex-docstruct-symbol 'reftex-section-levels))
          entry changed)
      (while entry-list
        (setq entry (pop entry-list))
        (unless (member entry list)
          (setq reftex-tables-dirty t
                changed t)
          (push entry list)))
      (when changed
        (put reftex-docstruct-symbol 'reftex-section-levels list)))))

(defun reftex-notice-new-section ()
  (reftex-notice-new 1 'force))

;;; reftex-auc.el ends here
