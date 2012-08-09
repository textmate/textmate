;;; ediff-merg.el --- merging utilities

;; Copyright (C) 1994-2012 Free Software Foundation, Inc.

;; Author: Michael Kifer <kifer@cs.stonybrook.edu>
;; Package: ediff

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


;; compiler pacifier
(defvar ediff-window-A)
(defvar ediff-window-B)
(defvar ediff-window-C)
(defvar ediff-merge-window-share)
(defvar ediff-window-config-saved)

(eval-when-compile
  (require 'ediff-util))
;; end pacifier

(require 'ediff-init)

(defcustom ediff-quit-merge-hook 'ediff-maybe-save-and-delete-merge
  "Hooks to run before quitting a merge job.
The most common use is to save and delete the merge buffer."
  :type 'hook
  :group 'ediff-merge)


(defcustom ediff-default-variant 'combined
  "The variant to be used as a default for buffer C in merging.
Valid values are the symbols `default-A', `default-B', and `combined'."
  :type '(radio (const default-A) (const default-B) (const combined))
  :group 'ediff-merge)

(defcustom ediff-combination-pattern
  '("<<<<<<< variant A" A ">>>>>>> variant B" B  "####### Ancestor" Ancestor "======= end")
  "Pattern to be used for combining difference regions in buffers A and B.
The value must be a list of the form
\(STRING1 bufspec1  STRING2 bufspec2 STRING3 bufspec3 STRING4)
where bufspec is the symbol A, B, or Ancestor. For instance, if the value is
'(STRING1 A  STRING2 Ancestor STRING3 B STRING4) then the
combined text will look like this:

STRING1
diff region from variant A
STRING2
diff region from the ancestor
STRING3
diff region from variant B
STRING4
"
  :type '(choice (list string symbol string symbol string)
		 (list string symbol string symbol string symbol string))
  :group 'ediff-merge)

(defcustom ediff-show-clashes-only nil
  "If t, show only those diff regions where both buffers disagree with the ancestor.
This means that regions that have status prefer-A or prefer-B will be
skipped over.  A value of nil means show all regions."
  :type 'boolean
  :group 'ediff-merge
  )
(make-variable-buffer-local 'ediff-show-clashes-only)

(defcustom ediff-skip-merge-regions-that-differ-from-default nil
  "If t, show only the regions that have not been changed by the user.
A region is considered to have been changed if it is different from the current
default (`default-A', `default-B', `combined') and it hasn't been marked as
`prefer-A' or `prefer-B'.
A region is considered to have been changed also when it is marked as
as `prefer-A', but is different from the corresponding difference region in
Buffer A or if it is marked as `prefer-B' and is different from the region in
Buffer B."
  :type 'boolean
  :group 'ediff-merge
  )
(make-variable-buffer-local 'ediff-skip-merge-regions-that-differ-from-default)

;; check if there is no clash between the ancestor and one of the variants.
;; if it is not a merge job then return true
(defun ediff-merge-region-is-non-clash (n)
  (if (ediff-merge-job)
      (string-match "prefer" (or (ediff-get-state-of-merge n) ""))
    t))

;; If ediff-show-clashes-only, check if there is no clash between the ancestor
;; and one of the variants.
(defun ediff-merge-region-is-non-clash-to-skip (n)
  (and (ediff-merge-job)
       ediff-show-clashes-only
       (ediff-merge-region-is-non-clash n)))

;; If ediff-skip-changed-regions, check if the merge region differs from
;; the current default. If a region is different from the default, it means
;; that the user has made determination as to how to merge for this particular
;; region.
(defun ediff-skip-merge-region-if-changed-from-default-p (n)
  (and (ediff-merge-job)
       ediff-skip-merge-regions-that-differ-from-default
       (ediff-merge-changed-from-default-p n 'prefers-too)))


(defun ediff-get-combined-region (n)
  (let ((pattern-list ediff-combination-pattern)
	(combo-region "")
	(err-msg
	 "ediff-combination-pattern: Invalid format. Please consult the documentation")
	region-delim region-spec)

    (if (< (length pattern-list) 5)
	(error err-msg))

    (while (> (length pattern-list) 2)
      (setq region-delim (nth 0 pattern-list)
	    region-spec (nth 1 pattern-list))
      (or (and (stringp region-delim) (memq region-spec '(A B Ancestor)))
	  (error err-msg))

      (condition-case nil
	  (setq combo-region
		(concat combo-region
			region-delim "\n"
			(ediff-get-region-contents
			 n region-spec ediff-control-buffer)))
	(error ""))
      (setq pattern-list (cdr (cdr pattern-list)))
      )

    (setq region-delim (nth 0 pattern-list))
    (or (stringp region-delim)
	(error err-msg))
    (setq combo-region (concat combo-region region-delim "\n"))
  ))

;;(defsubst ediff-make-combined-diff (regA regB)
;;  (concat (nth 0 ediff-combination-pattern) "\n"
;;	  regA
;;	  (nth 1 ediff-combination-pattern) "\n"
;;	  regB
;;	  (nth 2 ediff-combination-pattern) "\n"))

(defsubst ediff-set-state-of-all-diffs-in-all-buffers (ctl-buf)
  (let ((n 0))
    (while (< n ediff-number-of-differences)
      (ediff-set-state-of-diff-in-all-buffers n ctl-buf)
      (setq n (1+ n)))))

(defun ediff-set-state-of-diff-in-all-buffers (n ctl-buf)
  (let ((regA (ediff-get-region-contents n 'A ctl-buf))
	(regB (ediff-get-region-contents n 'B ctl-buf))
	(regC (ediff-get-region-contents n 'C ctl-buf)))
    (cond ((and (string= regA regB) (string= regA  regC))
	   (ediff-set-state-of-diff n 'A "=diff(B)")
	   (ediff-set-state-of-diff n 'B "=diff(C)")
	   (ediff-set-state-of-diff n 'C "=diff(A)"))
	  ((string= regA regB)
	   (ediff-set-state-of-diff n 'A "=diff(B)")
	   (ediff-set-state-of-diff n 'B "=diff(A)")
	   (ediff-set-state-of-diff n 'C nil))
	  ((string= regA regC)
	   (ediff-set-state-of-diff n 'A "=diff(C)")
	   (ediff-set-state-of-diff n 'C "=diff(A)")
	   (ediff-set-state-of-diff n 'B nil))
	  ((string= regB regC)
	   (ediff-set-state-of-diff n 'C "=diff(B)")
	   (ediff-set-state-of-diff n 'B "=diff(C)")
	   (ediff-set-state-of-diff n 'A nil))
	  ((string= regC (ediff-get-combined-region n))
	   (ediff-set-state-of-diff n 'A nil)
	     (ediff-set-state-of-diff n 'B nil)
	     (ediff-set-state-of-diff n 'C "=diff(A+B)"))
	  (t (ediff-set-state-of-diff n 'A nil)
	     (ediff-set-state-of-diff n 'B nil)
	     (ediff-set-state-of-diff n 'C nil)))
    ))

(defun ediff-set-merge-mode ()
  (normal-mode t)
  (remove-hook 'local-write-file-hooks 'ediff-set-merge-mode))


;; Go over all diffs starting with DIFF-NUM and copy regions into buffer C
;; according to the state of the difference.
;; Since ediff-copy-diff refuses to copy identical diff regions, there is
;; no need to optimize ediff-do-merge any further.
;;
;; If re-merging, change state of merge in all diffs starting with
;; DIFF-NUM, except those where the state is prefer-* or where it is
;; `default-*' or `combined' but the buf C region appears to be modified
;; since last set by default.
(defun ediff-do-merge (diff-num &optional remerging)
  (if (< diff-num 0) (setq diff-num 0))
  (let ((n diff-num)
	;;(default-state-of-merge (format "%S" ediff-default-variant))
	do-not-copy state-of-merge)
    (while (< n ediff-number-of-differences)
      (setq do-not-copy nil) ; reset after each cycle
      (if (= (mod n 10) 0)
	  (message "%s buffers A & B into C ... region %d of %d"
		   (if remerging "Re-merging" "Merging")
		   n
		   ediff-number-of-differences))

      (setq state-of-merge (ediff-get-state-of-merge n))

      (if remerging
	  ;;(let ((reg-A (ediff-get-region-contents n 'A ediff-control-buffer))
	  ;;	(reg-B (ediff-get-region-contents n 'B ediff-control-buffer))
	  ;;	(reg-C (ediff-get-region-contents n 'C ediff-control-buffer)))
	  (progn

	    ;; if region was edited since it was first set by default
	    (if (or (ediff-merge-changed-from-default-p n)
		    ;; was preferred
		    (string-match "prefer" state-of-merge))
		;; then ignore
		(setq do-not-copy t))

	    ;; change state of merge for this diff, if necessary
	    (if (and (string-match "\\(default\\|combined\\)" state-of-merge)
		     (not do-not-copy))
		(ediff-set-state-of-merge
		 n (format "%S" ediff-default-variant)))
	    ))

      ;; state-of-merge may have changed via ediff-set-state-of-merge, so
      ;; check it once again
      (setq state-of-merge (ediff-get-state-of-merge n))

      (or do-not-copy
	  (if (string= state-of-merge "combined")
	      ;; use n+1 because ediff-combine-diffs works via user numbering
	      ;; of diffs, which is 1+ to what ediff uses internally
	      (ediff-combine-diffs (1+ n) 'batch)
	    (ediff-copy-diff
	     n (if (string-match "-A" state-of-merge) 'A 'B) 'C 'batch)))
      (setq n (1+ n)))
    (message "Merging buffers A & B into C ... Done")
    ))


(defun ediff-re-merge ()
  "Remerge unmodified diff regions using a new default.  Start with the current region."
  (interactive)
  (let* ((default-variant-alist
	   (list '("default-A") '("default-B") '("combined")))
	 (actual-alist
	  (delete (list (symbol-name ediff-default-variant))
		  default-variant-alist)))
    (setq ediff-default-variant
	  (intern
	   (completing-read
	    (format "Current merge default is `%S'.  New default: "
		    ediff-default-variant)
	    actual-alist nil 'must-match)))
    (ediff-do-merge ediff-current-difference 'remerge)
    (ediff-recenter)
  ))

(defun ediff-shrink-window-C (arg)
  "Shrink window C to just one line.
With a prefix argument, returns window C to its normal size.
Used only for merging jobs."
  (interactive "P")
  (if (not ediff-merge-job)
      (error "ediff-shrink-window-C can be used only for merging jobs"))
  (cond ((eq arg '-) (setq arg -1))
	((not (numberp arg)) (setq arg nil)))
  (cond ((null arg)
	 (let ((ediff-merge-window-share
		(if (< (window-height ediff-window-C) 3)
		    ediff-merge-window-share 0)))
	   (setq ediff-window-config-saved "") ; force redisplay
	   (ediff-recenter 'no-rehighlight)))
	((and (< arg 0) (> (window-height ediff-window-C) 2))
	 (setq ediff-merge-window-share (* ediff-merge-window-share 0.9))
	 (setq ediff-window-config-saved "") ; force redisplay
	 (ediff-recenter 'no-rehighlight))
	((and (> arg 0) (> (window-height ediff-window-A) 2))
	 (setq ediff-merge-window-share (* ediff-merge-window-share 1.1))
	 (setq ediff-window-config-saved "") ; force redisplay
	 (ediff-recenter 'no-rehighlight))))


;; N here is the user's region number.  It is 1+ what Ediff uses internally.
(defun ediff-combine-diffs (n &optional batch-invocation)
  "Combine Nth diff regions of buffers A and B and place the combination in C.
N is a prefix argument.  If nil, combine the current difference regions.
Combining is done according to the specifications in variable
`ediff-combination-pattern'."
  (interactive "P")
  (setq n (if (numberp n) (1- n) ediff-current-difference))

  (let (reg-combined)
    ;;(setq regA (ediff-get-region-contents n 'A ediff-control-buffer)
    ;;	  regB (ediff-get-region-contents n 'B ediff-control-buffer))
    ;;(setq reg-combined (ediff-make-combined-diff regA regB))
    (setq reg-combined (ediff-get-combined-region n))

    (ediff-copy-diff n nil 'C batch-invocation reg-combined))
    (or batch-invocation (ediff-jump-to-difference (1+ n))))


;; Checks if the region in buff C looks like a combination of the regions
;; in buffers A and B.  Return a list (reg-a-beg reg-a-end reg-b-beg reg-b-end)
;; These refer to where the delimiters for region A, B, Ancestor start and end
;; in buffer C
(defun ediff-looks-like-combined-merge (region-num)
  (if ediff-merge-job
      (let ((combined (string-match (regexp-quote "(A+B)")
				    (or (ediff-get-state-of-diff region-num 'C)
					"")))
	    (mrgreg-beg (ediff-get-diff-posn 'C 'beg region-num))
	    (mrgreg-end (ediff-get-diff-posn 'C 'end region-num))
	    (pattern-list ediff-combination-pattern)
	    delim reg-beg reg-end delim-regs-list)

	(if combined
	    (ediff-with-current-buffer ediff-buffer-C
	      (while pattern-list
		(goto-char mrgreg-beg)
		(setq delim (nth 0 pattern-list))
		(search-forward delim mrgreg-end 'noerror)
		(setq reg-beg (match-beginning 0))
		(setq reg-end (match-end 0))
		(if (and reg-beg reg-end)
		    (setq delim-regs-list
			  ;; in reverse
			  (cons reg-end (cons reg-beg delim-regs-list))))
		(if (> (length pattern-list) 1)
		    (setq pattern-list (cdr (cdr pattern-list)))
		  (setq pattern-list nil))
		)))

	(reverse delim-regs-list)
	)))

(defvar state-of-merge)			; dynamic var

;; Check if the non-preferred merge has been modified since originally set.
;; This affects only the regions that are marked as default-A/B or combined.
;; If PREFERS-TOO is non-nil, then look at the regions marked as prefers-A/B as
;; well.
(defun ediff-merge-changed-from-default-p (diff-num &optional prefers-too)
  (let ((reg-A (ediff-get-region-contents diff-num 'A ediff-control-buffer))
	(reg-B (ediff-get-region-contents diff-num 'B ediff-control-buffer))
	(reg-C (ediff-get-region-contents diff-num 'C ediff-control-buffer)))

    (setq state-of-merge (ediff-get-state-of-merge diff-num))

    ;; if region was edited since it was first set by default
    (or (and (string= state-of-merge "default-A")
	     (not (string= reg-A reg-C)))
	(and (string= state-of-merge "default-B")
	     (not (string= reg-B reg-C)))
	(and (string= state-of-merge "combined")
	     ;;(not (string= (ediff-make-combined-diff reg-A reg-B) reg-C)))
	     (not (string= (ediff-get-combined-region diff-num) reg-C)))
	(and prefers-too
	     (string= state-of-merge "prefer-A")
	     (not (string= reg-A reg-C)))
	(and prefers-too
	     (string= state-of-merge "prefer-B")
	     (not (string= reg-B reg-C)))
	)))


(provide 'ediff-merg)


;; Local Variables:
;; eval: (put 'ediff-defvar-local 'lisp-indent-hook 'defun)
;; eval: (put 'ediff-with-current-buffer 'lisp-indent-hook 1)
;; eval: (put 'ediff-with-current-buffer 'edebug-form-spec '(form body))
;; End:

;;; ediff-merg.el ends here
