;;; rng-maint.el --- commands for RELAX NG maintainers

;; Copyright (C) 2003, 2007-2012 Free Software Foundation, Inc.

;; Author: James Clark
;; Keywords: XML, RelaxNG

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

(require 'xmltok)
(require 'nxml-mode)
(require 'texnfo-upd)

(defvar rng-dir (file-name-directory load-file-name))

;;; Conversion from XML to texinfo.
;; This is all a hack and is just enough to make the conversion work.
;; It's not intended for public use.

(defvar rng-manual-base "nxml-mode")
(defvar rng-manual-xml (concat rng-manual-base ".xml"))
(defvar rng-manual-texi (concat rng-manual-base ".texi"))
(defvar rng-manual-info (concat rng-manual-base ".info"))

(defun rng-format-manual ()
  "Create manual.texi from manual.xml."
  (interactive)
  (let ((xml-buf (find-file-noselect (expand-file-name rng-manual-xml
						       rng-dir)))
	(texi-buf (find-file-noselect (expand-file-name rng-manual-texi
							rng-dir))))
    (with-current-buffer texi-buf
      (erase-buffer)
      (let ((standard-output texi-buf))
	(princ (format "\\input texinfo @c -*- texinfo -*-\n\
@c %%**start of header\n\
@setfilename %s\n\
@settitle \n\
@c %%**end of header\n" rng-manual-info))
	(set-buffer xml-buf)
	(goto-char (point-min))
	(xmltok-save
	  (xmltok-forward-prolog)
	  (rng-process-tokens))
	(princ "\n@bye\n"))
      (set-buffer texi-buf)
      (rng-manual-fixup)
      (texinfo-insert-node-lines (point-min) (point-max) t)
      (texinfo-all-menus-update)
      (save-buffer))))

(defun rng-manual-fixup ()
  (goto-char (point-min))
  (search-forward "@top ")
  (let ((pos (point)))
    (search-forward "\n")
    (let ((title (buffer-substring-no-properties pos (1- (point)))))
      (goto-char (point-min))
      (search-forward "@settitle ")
      (insert title)
      (search-forward "@node")
      (goto-char (match-beginning 0))
      (insert "@dircategory Emacs\n"
	      "@direntry\n* "
	      title
	      ": ("
	      rng-manual-info
	      ").\n@end direntry\n\n"))))

(defvar rng-manual-inline-elements '(kbd key samp code var emph uref point))

(defun rng-process-tokens ()
  (let ((section-depth 0)
	;; stack of per-element space treatment
	;; t means keep, nil means discard, fill means no blank lines
	(keep-space-stack (list nil))
	(ignore-following-newline nil)
	(want-blank-line nil)
	name startp endp data keep-space-for-children)
    (while (xmltok-forward)
      (cond ((memq xmltok-type '(start-tag empty-element end-tag))
	     (setq startp (memq xmltok-type '(start-tag empty-element)))
	     (setq endp (memq xmltok-type '(end-tag empty-element)))
	     (setq name (intern (if startp
				    (xmltok-start-tag-qname)
				  (xmltok-end-tag-qname))))
	     (setq keep-space-for-children nil)
	     (setq ignore-following-newline nil)
	     (cond ((memq name rng-manual-inline-elements)
		    (when startp
		      (when want-blank-line
			(rng-manual-output-force-blank-line)
			(when (eq want-blank-line 'noindent)
			  (princ "@noindent\n"))
			(setq want-blank-line nil))
		      (setq keep-space-for-children t)
		      (princ (format "@%s{" name)))
		    (when endp (princ "}")))
		   ((eq name 'ulist)
		    (when startp
		      (rng-manual-output-force-blank-line)
		      (setq want-blank-line nil)
		      (princ "@itemize @bullet\n"))
		    (when endp
		      (rng-manual-output-force-new-line)
		      (setq want-blank-line 'noindent)
		      (princ "@end itemize\n")))
		   ((eq name 'item)
		    (rng-manual-output-force-new-line)
		    (setq want-blank-line endp)
		    (when startp (princ "@item\n")))
		   ((memq name '(example display))
		    (when startp
		      (setq ignore-following-newline t)
		      (rng-manual-output-force-blank-line)
		      (setq want-blank-line nil)
		      (setq keep-space-for-children t)
		      (princ (format "@%s\n" name)))
		    (when endp
		      (rng-manual-output-force-new-line)
		      (setq want-blank-line 'noindent)
		      (princ (format "@end %s\n" name))))
		   ((eq name 'para)
		    (rng-manual-output-force-new-line)
		    (when startp
		      (when want-blank-line
			(setq want-blank-line t))
		      (setq keep-space-for-children 'fill))
		    (when endp (setq want-blank-line t)))
		   ((eq name 'section)
		    (when startp
		      (rng-manual-output-force-blank-line)
		      (when (eq section-depth 0)
			(princ "@node Top\n"))
		      (princ "@")
		      (princ (nth section-depth '(top
						  chapter
						  section
						  subsection
						  subsubsection)))
		      (princ " ")
		      (setq want-blank-line nil)
		      (setq section-depth (1+ section-depth)))
		    (when endp
		      (rng-manual-output-force-new-line)
		      (setq want-blank-line nil)
		      (setq section-depth (1- section-depth))))
		   ((eq name 'title)
		    (when startp
		      (setq keep-space-for-children 'fill))
		    (when endp
		      (setq want-blank-line t)
		      (princ "\n"))))
	     (when startp
	       (setq keep-space-stack (cons keep-space-for-children
					    keep-space-stack)))
	     (when endp
	       (setq keep-space-stack (cdr keep-space-stack))))
	    ((memq xmltok-type '(data
				 space
				 char-ref
				 entity-ref
				 cdata-section))
	     (setq data nil)
	     (cond ((memq xmltok-type '(data space))
		    (setq data (buffer-substring-no-properties xmltok-start
							       (point))))
		   ((and (memq xmltok-type '(char-ref entity-ref))
			 xmltok-replacement)
		    (setq data xmltok-replacement))
		   ((eq xmltok-type 'cdata-section)
		    (setq data
			  (buffer-substring-no-properties (+ xmltok-start 9)
							  (- (point) 3)))))
	     (when (and data (car keep-space-stack))
	       (setq data (replace-regexp-in-string "[@{}]"
						    "@\\&"
						    data
						    t))
	       (when ignore-following-newline
		 (setq data (replace-regexp-in-string "\\`\n" "" data t)))
	       (setq ignore-following-newline nil)
;;	       (when (eq (car keep-space-stack) 'fill)
;;		 (setq data (replace-regexp-in-string "\n" " " data t)))
	       (when (eq want-blank-line 'noindent)
		 (setq data (replace-regexp-in-string "\\`\n*" "" data t)))
	       (when (> (length data) 0)
		 (when want-blank-line
		   (rng-manual-output-force-blank-line)
		   (when (eq want-blank-line 'noindent)
		     (princ "@noindent\n"))
		   (setq want-blank-line nil))
		 (princ data))))
	     ))))

(defun rng-manual-output-force-new-line ()
  (with-current-buffer standard-output
    (unless (eq (char-before) ?\n)
      (insert ?\n))))

(defun rng-manual-output-force-blank-line ()
  (with-current-buffer standard-output
    (if (eq (char-before) ?\n)
	(unless (eq (char-before (1- (point))) ?\n)
	  (insert ?\n))
      (insert "\n\n"))))

;;; Timing

(defun rng-time-function (function &rest args)
  (let* ((start (current-time))
	 (val (apply function args))
	 (end (current-time)))
    (message "%s ran in %g seconds"
	     function
	     (float-time (time-subtract end start)))
    val))

(defun rng-time-tokenize-buffer ()
  (interactive)
  (rng-time-function 'rng-tokenize-buffer))

(defun rng-tokenize-buffer ()
  (save-excursion
    (goto-char (point-min))
    (xmltok-save
      (xmltok-forward-prolog)
      (while (xmltok-forward)))))

(defun rng-time-validate-buffer ()
  (interactive)
  (rng-time-function 'rng-validate-buffer))

(defvar rng-error-count)
(defvar rng-validate-up-to-date-end)
(declare-function rng-clear-cached-state "rng-valid" (start end))
(declare-function rng-clear-overlays "rng-valid" (beg end))
(declare-function rng-clear-conditional-region "rng-valid" ())
(declare-function rng-do-some-validation "rng-valid"
                  (&optional continue-p-function))

(defun rng-validate-buffer ()
  (save-restriction
    (widen)
    (nxml-with-unmodifying-text-property-changes
      (rng-clear-cached-state (point-min) (point-max)))
    ;; 1+ to clear empty overlays at (point-max)
    (rng-clear-overlays (point-min) (1+ (point-max))))
  (setq rng-validate-up-to-date-end 1)
  (rng-clear-conditional-region)
  (setq rng-error-count 0)
  (while (rng-do-some-validation
	  (lambda () t))))

;;; rng-maint.el ends here
