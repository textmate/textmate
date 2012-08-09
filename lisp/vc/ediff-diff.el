;;; ediff-diff.el --- diff-related utilities

;; Copyright (C) 1994-2012  Free Software Foundation, Inc.

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


(provide 'ediff-diff)

(eval-when-compile
  (require 'ediff-util))

(require 'ediff-init)

(defgroup ediff-diff nil
  "Diff related utilities."
  :prefix "ediff-"
  :group 'ediff)

(defcustom ediff-diff-program "diff"
  "Program to use for generating the differential of the two files."
  :type 'string
  :group 'ediff-diff)
(defcustom ediff-diff3-program "diff3"
  "Program to be used for three-way comparison.
Must produce output compatible with Unix's diff3 program."
  :type 'string
  :group 'ediff-diff)


;; The following functions must precede all defcustom-defined variables.

(fset 'ediff-set-actual-diff-options (lambda () nil))

(defcustom ediff-shell
  (cond ((memq system-type '(ms-dos windows-nt))
	 shell-file-name) ; no standard name on MS-DOS
	(t  "sh")) ; UNIX
  "The shell used to run diff and patch.
If user's .profile or .cshrc files are set up correctly, any shell
will do.  However, some people set $prompt or other things
incorrectly, which leads to undesirable output messages.  These may
cause Ediff to fail.  In such a case, set `ediff-shell' to a shell that
you are not using or, better, fix your shell's startup file."
  :type 'string
  :group 'ediff-diff)

(defcustom ediff-cmp-program "cmp"
  "Utility to use to determine if two files are identical.
It must return code 0, if its arguments are identical files."
  :type 'string
  :group 'ediff-diff)

(defcustom ediff-cmp-options nil
  "Options to pass to `ediff-cmp-program'.
If GNU diff is used as `ediff-cmp-program', then the most useful options
are `-I REGEXP', to ignore changes whose lines match the REGEXP."
  :type '(repeat string)
  :group 'ediff-diff)

(defun ediff-set-diff-options (symbol value)
  (set symbol value)
  (ediff-set-actual-diff-options))

(defcustom ediff-diff-options
  (if (memq system-type '(ms-dos windows-nt)) "--binary" "")
  "Options to pass to `ediff-diff-program'.
If Unix diff is used as `ediff-diff-program',
then a useful option is `-w', to ignore space.
Options `-c', `-u', and `-i' are not allowed. Case sensitivity can be
toggled interactively using \\[ediff-toggle-ignore-case].

Do not remove the default options. If you need to change this variable, add new
options after the default ones.

This variable is not for customizing the look of the differences produced by
the command \\[ediff-show-diff-output]. Use the variable
`ediff-custom-diff-options' for that."
  :set 'ediff-set-diff-options
  :type 'string
  :group 'ediff-diff)

(ediff-defvar-local ediff-ignore-case nil
  "*If t, skip over difference regions that differ only in letter case.
This variable can be set either in .emacs or toggled interactively.
Use `setq-default' if setting it in .emacs")

(defcustom ediff-ignore-case-option "-i"
  "Option that causes the diff program to ignore case of letters."
  :type 'string
  :group 'ediff-diff)

(defcustom ediff-ignore-case-option3 ""
  "Option that causes the diff3 program to ignore case of letters.
GNU diff3 doesn't have such an option."
  :type 'string
  :group 'ediff-diff)

;; the actual options used in comparison
(ediff-defvar-local ediff-actual-diff-options ediff-diff-options "")

(defcustom ediff-custom-diff-program ediff-diff-program
  "Program to use for generating custom diff output for saving it in a file.
This output is not used by Ediff internally."
  :type 'string
  :group 'ediff-diff)
(defcustom ediff-custom-diff-options "-c"
  "Options to pass to `ediff-custom-diff-program'."
  :type 'string
  :group 'ediff-diff)

;;; Support for diff3

(defvar ediff-match-diff3-line "^====\\(.?\\)\C-m?$"
  "Pattern to match lines produced by diff3 that describe differences.")
(defcustom ediff-diff3-options ""
  "Options to pass to `ediff-diff3-program'."
  :set 'ediff-set-diff-options
  :type 'string
  :group 'ediff-diff)

;; the actual options used in comparison
(ediff-defvar-local ediff-actual-diff3-options ediff-diff3-options "")

(defcustom ediff-diff3-ok-lines-regexp
  "^\\([1-3]:\\|====\\|  \\|.*Warning *:\\|.*No newline\\|.*missing newline\\|^\C-m$\\)"
  "Regexp that matches normal output lines from `ediff-diff3-program'.
Lines that do not match are assumed to be error messages."
  :type 'regexp
  :group 'ediff-diff)

;; keeps the status of the current diff in 3-way jobs.
;; the status can be =diff(A), =diff(B), or =diff(A+B)
(ediff-defvar-local ediff-diff-status "" "")


;;; Fine differences

(ediff-defvar-local ediff-auto-refine (if (ediff-has-face-support-p) 'on 'nix)
  "If `on', Ediff auto-highlights fine diffs for the current diff region.
If `off', auto-highlighting is not used. If `nix', no fine diffs are shown
at all, unless the user force-refines the region by hitting `*'.

This variable can be set either in .emacs or toggled interactively.
Use `setq-default' if setting it in .emacs")

(ediff-defvar-local ediff-ignore-similar-regions nil
  "*If t, skip over difference regions that differ only in the white space and line breaks.
This variable can be set either in .emacs or toggled interactively.
Use `setq-default' if setting it in .emacs")

(ediff-defvar-local ediff-auto-refine-limit 14000
  "*Auto-refine only the regions of this size \(in bytes\) or less.")

;;; General

(defvar ediff-diff-ok-lines-regexp
  (concat
   "^\\("
   "[0-9,]+[acd][0-9,]+\C-m?$"
   "\\|[<>] "
   "\\|---"
   "\\|.*Warning *:"
   "\\|.*No +newline"
   "\\|.*missing +newline"
   "\\|^\C-m?$"
   "\\)")
  "Regexp that matches normal output lines from `ediff-diff-program'.
This is mostly lifted from Emerge, except that Ediff also considers
warnings and `Missing newline'-type messages to be normal output.
Lines that do not match are assumed to be error messages.")

(defvar ediff-match-diff-line
  (let ((x "\\([0-9]+\\)\\(\\|,\\([0-9]+\\)\\)"))
    (concat "^" x "\\([acd]\\)" x "\C-m?$"))
  "Pattern to match lines produced by diff that describe differences.")

(ediff-defvar-local ediff-setup-diff-regions-function nil
  "value is a function symbol depending on the kind of job is to be done.
For 2-way jobs and for ediff-merge, it should be `ediff-setup-diff-regions'.
For jobs requiring diff3, it should be `ediff-setup-diff-regions3'.

The function should take three mandatory arguments, file-A, file-B, and
file-C. It may ignore file C for diff2 jobs. It should also take
one optional arguments, diff-number to refine.")


;;; Functions

;; Generate the difference vector and overlays for the two files
;; With optional arg REG-TO-REFINE, refine this region.
;; File-C argument is not used here. It is there just because
;; ediff-setup-diff-regions is called via a funcall to
;; ediff-setup-diff-regions-function, which can also have the value
;; ediff-setup-diff-regions3, which takes 4 arguments.
(defun ediff-setup-diff-regions (file-A file-B file-C)
  ;; looking for '-c', '-i', '-u', or 'c', 'i', 'u' among clustered non-long options
  (if (string-match "^-[ciu]\\| -[ciu]\\|\\(^\\| \\)-[^- ]+[ciu]"
		    ediff-diff-options)
      (error "Options `-c', `-u', and `-i' are not allowed in `ediff-diff-options'"))

  ;; create, if it doesn't exist
  (or (ediff-buffer-live-p ediff-diff-buffer)
      (setq ediff-diff-buffer
	    (get-buffer-create (ediff-unique-buffer-name "*ediff-diff" "*"))))
  (ediff-make-diff2-buffer ediff-diff-buffer file-A file-B)
  (ediff-prepare-error-list ediff-diff-ok-lines-regexp ediff-diff-buffer)
  (ediff-convert-diffs-to-overlays
   (ediff-extract-diffs
    ediff-diff-buffer ediff-word-mode ediff-narrow-bounds)))

;; Run the diff program on FILE1 and FILE2 and put the output in DIFF-BUFFER
;; Return the size of DIFF-BUFFER
;; The return code isn't used in the program at present.
(defun ediff-make-diff2-buffer (diff-buffer file1 file2)
  (let ((file1-size (ediff-file-size file1))
	(file2-size (ediff-file-size file2)))
    (cond ((not (numberp file1-size))
	   (message "Can't find file: %s"
		    (ediff-abbreviate-file-name file1))
	   (sit-for 2)
	   ;; 1 is an error exit code
	   1)
	  ((not (numberp file2-size))
	   (message "Can't find file: %s"
		    (ediff-abbreviate-file-name file2))
	   (sit-for 2)
	   ;; 1 is an error exit code
	   1)
	  (t (message "Computing differences between %s and %s ..."
		      (file-name-nondirectory file1)
		      (file-name-nondirectory file2))
	     ;; this erases the diff buffer automatically
	     (ediff-exec-process ediff-diff-program
				 diff-buffer
				 'synchronize
				 ediff-actual-diff-options file1 file2)
	     (message "")
	     (ediff-with-current-buffer diff-buffer
	       (buffer-size))))))



;; If file-A/B/C is nil, do 2-way comparison with the non-nil buffers
;; This function works for diff3 and diff2 jobs
(defun ediff-setup-fine-diff-regions (file-A file-B file-C reg-num)
  (or (ediff-buffer-live-p ediff-fine-diff-buffer)
      (setq ediff-fine-diff-buffer
	    (get-buffer-create
	     (ediff-unique-buffer-name "*ediff-fine-diff" "*"))))

  (let (diff3-job diff-program diff-options ok-regexp diff-list)
    (setq diff3-job ediff-3way-job
	  diff-program (if diff3-job ediff-diff3-program ediff-diff-program)
	  diff-options (if diff3-job
			   ediff-actual-diff3-options
			 ediff-actual-diff-options)
	  ok-regexp (if diff3-job
			ediff-diff3-ok-lines-regexp
			ediff-diff-ok-lines-regexp))

    (ediff-message-if-verbose "Refining difference region %d ..." (1+ reg-num))
    (ediff-exec-process diff-program ediff-fine-diff-buffer 'synchronize
			diff-options
			;; The shuffle below is because we can compare 3-way
			;; or in several 2-way fashions, like fA fC, fA fB,
			;; or fB fC.
			(if file-A file-A file-B)
			(if file-B file-B file-A)
			(if diff3-job
			    (if file-C file-C file-B))
			) ; exec process

    (ediff-prepare-error-list ok-regexp ediff-fine-diff-buffer)
    (ediff-message-if-verbose
     "")
    ;; "Refining difference region %d ... done" (1+ reg-num))

    (setq diff-list
	  (if diff3-job
	      (ediff-extract-diffs3
	       ediff-fine-diff-buffer '3way-comparison 'word-mode)
	    (ediff-extract-diffs ediff-fine-diff-buffer 'word-mode)))
    ;; fixup diff-list
    (if diff3-job
	(cond ((not file-A)
	       (mapc (lambda (elt)
		       (aset elt 0 nil)
		       (aset elt 1 nil))
		     (cdr diff-list)))
	      ((not file-B)
	       (mapc (lambda (elt)
		       (aset elt 2 nil)
		       (aset elt 3 nil))
		     (cdr diff-list)))
	      ((not file-C)
	       (mapc (lambda (elt)
		       (aset elt 4 nil)
		       (aset elt 5 nil))
		     (cdr diff-list)))
	  ))

    (ediff-convert-fine-diffs-to-overlays diff-list reg-num)
    ))


(defun ediff-prepare-error-list (ok-regexp diff-buff)
  (or (ediff-buffer-live-p ediff-error-buffer)
      (setq ediff-error-buffer
	    (get-buffer-create (ediff-unique-buffer-name
				"*ediff-errors" "*"))))
  (ediff-with-current-buffer ediff-error-buffer
    (setq buffer-undo-list t)
    (erase-buffer)
    (insert (ediff-with-current-buffer diff-buff (buffer-string)))
    (goto-char (point-min))
    (delete-matching-lines ok-regexp))
  ;; If diff reports errors, show them then quit.
  (if (/= 0 (ediff-with-current-buffer ediff-error-buffer (buffer-size)))
      (let ((ctl-buf ediff-control-buffer)
	    (error-buf ediff-error-buffer))
	(ediff-skip-unsuitable-frames)
	(switch-to-buffer error-buf)
	(ediff-kill-buffer-carefully ctl-buf)
	(error "Errors in diff output.  Diff output is in %S" diff-buff))))

;; BOUNDS specifies visibility bounds to use.
;; WORD-MODE tells whether we are in the word-mode or not.
;; If WORD-MODE, also construct vector of diffs using word numbers.
;; Else, use point values.
;; This function handles diff-2 jobs including the case of
;; merging buffers and files without ancestor.
(defun ediff-extract-diffs (diff-buffer word-mode &optional bounds)
  (let ((A-buffer ediff-buffer-A)
	(B-buffer ediff-buffer-B)
	(C-buffer ediff-buffer-C)
	(a-prev 1) ; this is needed to set the first diff line correctly
	(a-prev-pt nil)
	(b-prev 1)
	(b-prev-pt nil)
	(c-prev 1)
	(c-prev-pt nil)
	diff-list shift-A shift-B
	)

    ;; diff list contains word numbers, unless changed later
    (setq diff-list (cons (if word-mode 'words 'points)
			  diff-list))
    ;; we don't use visibility bounds for buffer C when merging
    (if bounds
	(setq shift-A
	      (ediff-overlay-start
	       (ediff-get-value-according-to-buffer-type 'A bounds))
	      shift-B
	      (ediff-overlay-start
	       (ediff-get-value-according-to-buffer-type 'B bounds))))

    ;; reset point in buffers A/B/C
    (ediff-with-current-buffer A-buffer
      (goto-char (if shift-A shift-A (point-min))))
    (ediff-with-current-buffer B-buffer
      (goto-char (if shift-B shift-B (point-min))))
    (if (ediff-buffer-live-p C-buffer)
	(ediff-with-current-buffer C-buffer
	  (goto-char (point-min))))

    (ediff-with-current-buffer diff-buffer
      (goto-char (point-min))
      (while (re-search-forward ediff-match-diff-line nil t)
       (let* ((a-begin (string-to-number (buffer-substring (match-beginning 1)
                                                           (match-end 1))))
	      (a-end  (let ((b (match-beginning 3))
			    (e (match-end 3)))
			(if b
			    (string-to-number (buffer-substring b e))
			  a-begin)))
	      (diff-type (buffer-substring (match-beginning 4) (match-end 4)))
	      (b-begin (string-to-number (buffer-substring (match-beginning 5)
                                                           (match-end 5))))
	      (b-end (let ((b (match-beginning 7))
			   (e (match-end 7)))
		       (if b
			   (string-to-number (buffer-substring b e))
			 b-begin)))
	      a-begin-pt a-end-pt b-begin-pt b-end-pt
	      c-begin c-end c-begin-pt c-end-pt)
	 ;; fix the beginning and end numbers, because diff is somewhat
	 ;; strange about how it numbers lines
	 (if (string-equal diff-type "a")
	     (setq b-end (1+ b-end)
		   a-begin (1+ a-begin)
		   a-end a-begin)
	   (if (string-equal diff-type "d")
	       (setq a-end (1+ a-end)
		     b-begin (1+ b-begin)
		     b-end b-begin)
	     ;; (string-equal diff-type "c")
	     (setq a-end (1+ a-end)
		   b-end (1+ b-end))))

	 (if (eq ediff-default-variant 'default-B)
	     (setq c-begin b-begin
		   c-end b-end)
	   (setq c-begin a-begin
		 c-end a-end))

	 ;; compute main diff vector
	 (if word-mode
	     ;; make diff-list contain word numbers
	     (setq diff-list
		   (nconc diff-list
			  (list
			   (if (ediff-buffer-live-p C-buffer)
			       (vector (- a-begin a-prev) (- a-end a-begin)
				       (- b-begin b-prev) (- b-end b-begin)
				       (- c-begin c-prev) (- c-end c-begin)
				       nil nil ; dummy ancestor
				       nil     ; state of diff
				       nil     ; state of merge
				       nil     ; state of ancestor
				       )
			     (vector (- a-begin a-prev) (- a-end a-begin)
				     (- b-begin b-prev) (- b-end b-begin)
				     nil nil ; dummy buf C
				     nil nil ; dummy ancestor
				     nil     ; state of diff
				     nil     ; state of merge
				     nil     ; state of ancestor
				     ))
			   ))
		   a-prev a-end
		   b-prev b-end
		   c-prev c-end)
	   ;; else convert lines to points
	   (ediff-with-current-buffer A-buffer
	     (let ((longlines-mode-val
		    (if (and (boundp 'longlines-mode) longlines-mode) 1 0)))
	       ;; we must disable and then restore longlines-mode
	       (if (eq longlines-mode-val 1)
		   (longlines-mode 0))
	       (goto-char (or a-prev-pt shift-A (point-min)))
	       (forward-line (- a-begin a-prev))
	       (setq a-begin-pt (point))
	       (forward-line (- a-end a-begin))
	       (setq a-end-pt (point)
		     a-prev a-end
		     a-prev-pt a-end-pt)
	       (if (eq longlines-mode-val 1)
		   (longlines-mode longlines-mode-val))
	       ))
	   (ediff-with-current-buffer B-buffer
	     (let ((longlines-mode-val
		    (if (and (boundp 'longlines-mode) longlines-mode) 1 0)))
	       (if (eq longlines-mode-val 1)
		   (longlines-mode 0))
	       (goto-char (or b-prev-pt shift-B (point-min)))
	       (forward-line (- b-begin b-prev))
	       (setq b-begin-pt (point))
	       (forward-line (- b-end b-begin))
	       (setq b-end-pt (point)
		     b-prev b-end
		     b-prev-pt b-end-pt)
	       (if (eq longlines-mode-val 1)
		   (longlines-mode longlines-mode-val))
	       ))
	   (if (ediff-buffer-live-p C-buffer)
	       (ediff-with-current-buffer C-buffer
		 (let ((longlines-mode-val
			(if (and (boundp 'longlines-mode) longlines-mode) 1 0)))
		   (if (eq longlines-mode-val 1)
		       (longlines-mode 0))
		   (goto-char (or c-prev-pt (point-min)))
		   (forward-line (- c-begin c-prev))
		   (setq c-begin-pt (point))
		   (forward-line (- c-end c-begin))
		   (setq c-end-pt (point)
			 c-prev c-end
			 c-prev-pt c-end-pt)
		   (if (eq longlines-mode-val 1)
		       (longlines-mode longlines-mode-val))
		 )))
	   (setq diff-list
		 (nconc
		  diff-list
		  (list
		   (if (ediff-buffer-live-p C-buffer)
		       (vector
			a-begin-pt a-end-pt b-begin-pt b-end-pt
			c-begin-pt c-end-pt
			nil nil	; dummy ancestor
			;; state of diff
			;; shows which buff is different from the other two
			(if (eq ediff-default-variant 'default-B) 'A 'B)
			ediff-default-variant	; state of merge
			nil			; state of ancestor
			)
		     (vector a-begin-pt a-end-pt
			     b-begin-pt b-end-pt
			     nil nil	; dummy buf C
			     nil nil	; dummy ancestor
			     nil nil	; dummy state of diff & merge
			     nil	; dummy state of ancestor
			     )))
		  )))

	 ))) ; end ediff-with-current-buffer
    diff-list
    ))


(defun ediff-convert-diffs-to-overlays (diff-list)
  (ediff-set-diff-overlays-in-one-buffer 'A diff-list)
  (ediff-set-diff-overlays-in-one-buffer 'B diff-list)
  (if ediff-3way-job
      (ediff-set-diff-overlays-in-one-buffer 'C diff-list))
  (if ediff-merge-with-ancestor-job
      (ediff-set-diff-overlays-in-one-buffer 'Ancestor diff-list))
  ;; set up vector showing the status of merge regions
  (if ediff-merge-job
      (setq ediff-state-of-merge
	    (vconcat
	     (mapcar (lambda (elt)
		       (let ((state-of-merge (aref elt 9))
			     (state-of-ancestor (aref elt 10)))
			 (vector
			  ;; state of merge: prefers/default-A/B or combined
			  (if state-of-merge (format "%S" state-of-merge))
			  ;; whether the ancestor region is empty
			  state-of-ancestor)))
		     ;; the first elt designates type of list
		     (cdr diff-list))
	     )))
  (message "Processing difference regions ... done"))


(defun ediff-set-diff-overlays-in-one-buffer (buf-type diff-list)
  (let* ((current-diff -1)
	 (buff (ediff-get-buffer buf-type))
	 (ctl-buf ediff-control-buffer)
	 ;; ediff-extract-diffs puts the type of diff-list as the first elt
	 ;; of this list. The type is either 'points or 'words
	 (diff-list-type (car diff-list))
	 (shift (ediff-overlay-start
		 (ediff-get-value-according-to-buffer-type
		  buf-type ediff-narrow-bounds)))
	 (limit (ediff-overlay-end
		 (ediff-get-value-according-to-buffer-type
		  buf-type ediff-narrow-bounds)))
	 diff-overlay-list list-element total-diffs
	 begin end pt-saved overlay state-of-diff)

    (setq diff-list (cdr diff-list)) ; discard diff list type
    (setq total-diffs (length diff-list))

    ;; shift, if necessary
    (ediff-with-current-buffer buff (setq pt-saved shift))

    (while diff-list
      (setq current-diff (1+ current-diff)
	    list-element (car diff-list)
	    begin 	 (aref list-element (cond ((eq buf-type 'A) 0)
						  ((eq buf-type 'B) 2)
						  ((eq buf-type 'C) 4)
						  (t 6)))  ; Ancestor
	    end 	 (aref list-element (cond ((eq buf-type 'A) 1)
						  ((eq buf-type 'B) 3)
						  ((eq buf-type 'C) 5)
						  (t 7)))  ; Ancestor
	    state-of-diff (aref list-element 8)
	    )

      (cond ((and (not (eq buf-type state-of-diff))
		  (not (eq buf-type 'Ancestor))
		  (memq state-of-diff '(A B C)))
	     (setq state-of-diff
		   (car (delq buf-type (delq state-of-diff (list 'A 'B 'C)))))
	     (setq state-of-diff (format "=diff(%S)" state-of-diff))
	     )
	    (t (setq state-of-diff nil)))

      ;; Put overlays at appropriate places in buffer
      ;; convert word numbers to points, if necessary
      (if (eq diff-list-type 'words)
	  (progn
	    (ediff-with-current-buffer buff (goto-char pt-saved))
	    (ediff-with-current-buffer ctl-buf
	      (setq begin (ediff-goto-word (1+ begin) buff)
		    end (ediff-goto-word end buff 'end)))
	    (if (> end limit) (setq end limit))
	    (if (> begin end) (setq begin end))
	    (setq pt-saved (ediff-with-current-buffer buff (point)))))
      (setq overlay (ediff-make-bullet-proof-overlay begin end buff))

      (ediff-overlay-put overlay 'priority ediff-shadow-overlay-priority)
      (ediff-overlay-put overlay 'ediff-diff-num current-diff)
      (if (and (ediff-has-face-support-p)
	       ediff-use-faces ediff-highlight-all-diffs)
	  (ediff-set-overlay-face
	   overlay (ediff-background-face buf-type current-diff)))

      (if (= 0 (mod current-diff 10))
	  (message "Buffer %S: Processing difference region %d of %d"
		   buf-type current-diff total-diffs))
      ;; Record all overlays for this difference.
      ;; The 2-d elt, nil, is a place holder for the fine diff vector.
      ;; The 3-d elt, nil, is a place holder for no-fine-diffs flag.
      ;; The 4-th elt says which diff region is different from the other two
      ;; (3-way jobs only).
      (setq diff-overlay-list
	    (nconc
	     diff-overlay-list
	     (list (vector overlay nil nil state-of-diff)))
	    diff-list
	    (cdr diff-list))
      ) ; while

    (set (ediff-get-symbol-from-alist buf-type ediff-difference-vector-alist)
	 (vconcat diff-overlay-list))
    ))

;; `n' is the diff region to work on.  Default is ediff-current-difference.
;; if `flag' is 'noforce then make fine-diffs only if this region's fine
;; diffs have not been computed before.
;; if `flag' is 'skip then don't compute fine diffs for this region.
(defun ediff-make-fine-diffs (&optional n flag)
  (or n  (setq n ediff-current-difference))

  (if (< ediff-number-of-differences 1)
      (error ediff-NO-DIFFERENCES))

  (if ediff-word-mode
      (setq flag 'skip
	    ediff-auto-refine 'nix))

  (or (< n 0)
      (>= n ediff-number-of-differences)
      ;; n is within the range
      (let ((tmp-buffer (get-buffer-create ediff-tmp-buffer))
	    (file-A ediff-temp-file-A)
	    (file-B ediff-temp-file-B)
	    (file-C ediff-temp-file-C)
	    (empty-A (ediff-empty-diff-region-p n 'A))
	    (empty-B (ediff-empty-diff-region-p n 'B))
	    (empty-C (ediff-empty-diff-region-p n 'C))
	    (whitespace-A (ediff-whitespace-diff-region-p n 'A))
	    (whitespace-B (ediff-whitespace-diff-region-p n 'B))
	    (whitespace-C (ediff-whitespace-diff-region-p n 'C))
	    cumulative-fine-diff-length)

	(cond ;; If one of the regions is empty (or 2 in 3way comparison)
	      ;; then don't refine.
	      ;; If the region happens to be entirely whitespace or empty then
	      ;; mark as such.
	      ((> (length (delq nil (list empty-A empty-B empty-C))) 1)
	       (if (and (ediff-looks-like-combined-merge n)
			ediff-merge-job)
		   (ediff-set-fine-overlays-in-one-buffer 'C nil n))
	       (if ediff-3way-comparison-job
		   (ediff-message-if-verbose
		    "Region %d is empty in all buffers but %S"
		    (1+ n)
		    (cond ((not empty-A) 'A)
			  ((not empty-B) 'B)
			  ((not empty-C) 'C)))
		 (ediff-message-if-verbose
		  "Region %d in buffer %S is empty"
		  (1+ n)
		  (cond (empty-A 'A)
			(empty-B 'B)
			(empty-C 'C)))
		 )
	       ;; if all regions happen to be whitespace
	       (if (and whitespace-A whitespace-B whitespace-C)
		   ;; mark as space only
		   (ediff-mark-diff-as-space-only n t)
		 ;; if some regions are white and others don't, then mark as
		 ;; non-white-space-only
		 (ediff-mark-diff-as-space-only n nil)))

	      ;; don't compute fine diffs if diff vector exists
	      ((and (eq flag 'noforce) (ediff-get-fine-diff-vector n 'A))
	       (if (ediff-no-fine-diffs-p n)
		   (message
		    "Only white-space differences in region %d %s"
		    (1+ n)
		    (cond ((eq (ediff-no-fine-diffs-p n) 'A)
			   "in buffers B & C")
			  ((eq (ediff-no-fine-diffs-p n) 'B)
			   "in buffers A & C")
			  ((eq (ediff-no-fine-diffs-p n) 'C)
			   "in buffers A & B")
			  (t "")))))
	      ;; don't compute fine diffs for this region
	      ((eq flag 'skip)
	       (or (ediff-get-fine-diff-vector n 'A)
		   (memq ediff-auto-refine '(off nix))
		   (ediff-message-if-verbose
		    "Region %d exceeds the auto-refinement limit. Type `%s' to refine"
		    (1+ n)
		    (substitute-command-keys
		     "\\[ediff-make-or-kill-fine-diffs]")
		    )))
	      (t
	       ;; recompute fine diffs
	       (ediff-wordify
		(ediff-get-diff-posn 'A 'beg n)
		(ediff-get-diff-posn 'A 'end n)
		ediff-buffer-A
		tmp-buffer
		ediff-control-buffer)
	       (setq file-A
		     (ediff-make-temp-file tmp-buffer "fineDiffA" file-A))

	       (ediff-wordify
		(ediff-get-diff-posn 'B 'beg n)
		(ediff-get-diff-posn 'B 'end n)
		ediff-buffer-B
		tmp-buffer
		ediff-control-buffer)
	       (setq file-B
		     (ediff-make-temp-file tmp-buffer "fineDiffB" file-B))

	       (if ediff-3way-job
		   (progn
		     (ediff-wordify
		      (ediff-get-diff-posn 'C 'beg n)
		      (ediff-get-diff-posn 'C 'end n)
		      ediff-buffer-C
		      tmp-buffer
		      ediff-control-buffer)
		     (setq file-C
			   (ediff-make-temp-file
			    tmp-buffer "fineDiffC" file-C))))

	       ;; save temp file names.
	       (setq ediff-temp-file-A file-A
		     ediff-temp-file-B file-B
		     ediff-temp-file-C file-C)

	       ;; set the new vector of fine diffs, if none exists
	       (cond ((and ediff-3way-job whitespace-A)
		      (ediff-setup-fine-diff-regions nil file-B file-C n))
		     ((and ediff-3way-job whitespace-B)
		      (ediff-setup-fine-diff-regions file-A nil file-C n))
		     ((and ediff-3way-job
			   ;; In merge-jobs, whitespace-C is t, since
			   ;; ediff-empty-diff-region-p returns t in this case
			   whitespace-C)
		      (ediff-setup-fine-diff-regions file-A file-B nil n))
		     (t
		      (ediff-setup-fine-diff-regions file-A file-B file-C n)))

	       (setq cumulative-fine-diff-length
		     (+ (length (ediff-get-fine-diff-vector n 'A))
			(length (ediff-get-fine-diff-vector n 'B))
			;; in merge jobs, the merge buffer is never refined
			(if (and file-C (not ediff-merge-job))
			    (length (ediff-get-fine-diff-vector n 'C))
			  0)))

	       (cond ((or
		       ;; all regions are white space
		       (and whitespace-A whitespace-B whitespace-C)
		       ;; none is white space and no fine diffs detected
		       (and (not whitespace-A)
			    (not whitespace-B)
			    (not (and ediff-3way-job whitespace-C))
			    (eq cumulative-fine-diff-length 0)))
		      (ediff-mark-diff-as-space-only n t)
		      (ediff-message-if-verbose
		       "Only white-space differences in region %d" (1+ n)))
		     ((eq cumulative-fine-diff-length 0)
		      (ediff-message-if-verbose
		       "Only white-space differences in region %d %s"
		       (1+ n)
		       (cond (whitespace-A (ediff-mark-diff-as-space-only n 'A)
					   "in buffers B & C")
			     (whitespace-B (ediff-mark-diff-as-space-only n 'B)
					   "in buffers A & C")
			     (whitespace-C (ediff-mark-diff-as-space-only n 'C)
					   "in buffers A & B"))))
		     (t
		      (ediff-mark-diff-as-space-only n nil)))
	       )
	      ) ; end cond
	(ediff-set-fine-diff-properties n)
	)))

;; Interface to ediff-make-fine-diffs. Checks for auto-refine limit, etc.
(defun ediff-install-fine-diff-if-necessary (n)
  (cond ((and (eq ediff-auto-refine 'on)
	      ediff-use-faces
	      (not (eq ediff-highlighting-style 'off))
	      (not (eq ediff-highlighting-style 'ascii)))
	 (if (and
	      (> ediff-auto-refine-limit
		 (- (ediff-get-diff-posn 'A 'end n)
		    (ediff-get-diff-posn 'A 'beg n)))
	      (> ediff-auto-refine-limit
		 (- (ediff-get-diff-posn 'B 'end n)
		    (ediff-get-diff-posn 'B 'beg n))))
	     (ediff-make-fine-diffs n 'noforce)
	   (ediff-make-fine-diffs n 'skip)))

	;; highlight if fine diffs already exist
	((eq ediff-auto-refine 'off)
	 (ediff-make-fine-diffs n 'skip))))


;; if fine diff vector is not set for diff N, then do nothing
(defun ediff-set-fine-diff-properties (n &optional default)
  (or (not (ediff-has-face-support-p))
      (< n 0)
      (>= n ediff-number-of-differences)
      ;; when faces are supported, set faces and priorities of fine overlays
      (progn
	(ediff-set-fine-diff-properties-in-one-buffer 'A n default)
	(ediff-set-fine-diff-properties-in-one-buffer 'B n default)
	(if ediff-3way-job
	    (ediff-set-fine-diff-properties-in-one-buffer 'C n default)))))

(defun ediff-set-fine-diff-properties-in-one-buffer (buf-type
						     n &optional default)
  (let ((fine-diff-vector  (ediff-get-fine-diff-vector n buf-type))
	(face (if default
		  'default
		(ediff-get-symbol-from-alist
		 buf-type ediff-fine-diff-face-alist)
		))
	(priority (if default
		      0
		    (1+ (or (ediff-overlay-get
			     (symbol-value
			      (ediff-get-symbol-from-alist
			       buf-type
			       ediff-current-diff-overlay-alist))
			     'priority)
			    0)))))
    (mapcar (lambda (overl)
	      (ediff-set-overlay-face overl face)
	      (ediff-overlay-put overl 'priority priority))
	    fine-diff-vector)))

;; Set overlays over the regions that denote delimiters
(defun ediff-set-fine-overlays-for-combined-merge (diff-list reg-num)
  (let (overlay overlay-list)
    (while diff-list
      (condition-case nil
	  (setq overlay
		(ediff-make-bullet-proof-overlay
		 (nth 0 diff-list) (nth 1 diff-list) ediff-buffer-C))
	(error ""))
      (setq overlay-list (cons overlay overlay-list))
      (if (> (length diff-list) 1)
	  (setq diff-list (cdr (cdr diff-list)))
	(error "ediff-set-fine-overlays-for-combined-merge: corrupt list of
delimiter regions"))
      )
    (setq overlay-list (reverse overlay-list))
    (ediff-set-fine-diff-vector
     reg-num 'C (apply 'vector overlay-list))
    ))


;; Convert diff list to overlays for a given DIFF-REGION
;; in buffer of type BUF-TYPE
(defun ediff-set-fine-overlays-in-one-buffer (buf-type diff-list region-num)
  (let* ((current-diff -1)
	 (reg-start (ediff-get-diff-posn buf-type 'beg region-num))
	 (buff (ediff-get-buffer buf-type))
	 (ctl-buf ediff-control-buffer)
	 combined-merge-diff-list
	 diff-overlay-list list-element
	 begin end overlay)

    (ediff-clear-fine-differences-in-one-buffer region-num buf-type)
    (setq diff-list (cdr diff-list)) ; discard list type (words or points)
    (ediff-with-current-buffer buff (goto-char reg-start))

    ;; if it is a combined merge then set overlays in buff C specially
    (if (and ediff-merge-job (eq buf-type 'C)
	     (setq combined-merge-diff-list
		   (ediff-looks-like-combined-merge region-num)))
	(ediff-set-fine-overlays-for-combined-merge
	 combined-merge-diff-list region-num)
      ;; regular fine diff
      (while diff-list
	(setq current-diff (1+ current-diff)
	      list-element (car diff-list)
	      begin 	 (aref list-element (cond ((eq buf-type 'A) 0)
						  ((eq buf-type 'B) 2)
						  (t 4)))  ; buf C
	      end 	 (aref list-element (cond ((eq buf-type 'A) 1)
						  ((eq buf-type 'B) 3)
						  (t 5)))) ; buf C
	(if (not (or begin end))
	    () ; skip this diff
	  ;; Put overlays at appropriate places in buffers
	  ;; convert lines to points, if necessary
	  (ediff-with-current-buffer ctl-buf
	    (setq begin (ediff-goto-word (1+ begin) buff)
		  end (ediff-goto-word end buff 'end)))
	  (setq overlay (ediff-make-bullet-proof-overlay begin end buff))
	  ;; record all overlays for this difference region
	  (setq diff-overlay-list (nconc diff-overlay-list (list overlay))))

	(setq diff-list (cdr diff-list))
	) ; while
      ;; convert the list of difference information into a vector
      ;; for fast access
      (ediff-set-fine-diff-vector
       region-num buf-type (vconcat diff-overlay-list))
      )))


(defun ediff-convert-fine-diffs-to-overlays (diff-list region-num)
  (ediff-set-fine-overlays-in-one-buffer 'A diff-list region-num)
  (ediff-set-fine-overlays-in-one-buffer 'B diff-list region-num)
  (if ediff-3way-job
      (ediff-set-fine-overlays-in-one-buffer 'C diff-list region-num)
    ))


;; Stolen from emerge.el
(defun ediff-get-diff3-group (file)
  ;; This save-excursion allows ediff-get-diff3-group to be called for the
  ;; various groups of lines (1, 2, 3) in any order, and for the lines to
  ;; appear in any order.  The reason this is necessary is that Gnu diff3
  ;; can produce the groups in the order 1, 2, 3 or 1, 3, 2.
  (save-excursion
    (re-search-forward
     (concat "^" file ":\\([0-9]+\\)\\(,\\([0-9]+\\)\\)?\\([ac]\\)\C-m?$"))
    (beginning-of-line 2)
    ;; treatment depends on whether it is an "a" group or a "c" group
    (if (string-equal (buffer-substring (match-beginning 4) (match-end 4)) "c")
	;; it is a "c" group
	(if (match-beginning 2)
	    ;; it has two numbers
	    (list (string-to-number
		   (buffer-substring (match-beginning 1) (match-end 1)))
		  (1+ (string-to-number
		       (buffer-substring (match-beginning 3) (match-end 3)))))
	  ;; it has one number
	  (let ((x (string-to-number
		    (buffer-substring (match-beginning 1) (match-end 1)))))
	    (list x (1+ x))))
      ;; it is an "a" group
      (let ((x (1+ (string-to-number
		    (buffer-substring (match-beginning 1) (match-end 1))))))
	(list x x)))))


;; If WORD-MODE, construct vector of diffs using word numbers.
;; Else, use point values.
;; WORD-MODE also tells if we are in the word-mode or not.
;; If THREE-WAY-COMP, then it is a 3-way comparison. Else, it is merging
;; with ancestor, in which case buffer-C contents is identical to buffer-A/B,
;; contents (unless buffer-A is narrowed) depending on ediff-default-variant's
;; value.
;; BOUNDS specifies visibility bounds to use.
(defun ediff-extract-diffs3 (diff-buffer word-mode three-way-comp
					  &optional bounds)
  (let ((A-buffer ediff-buffer-A)
	(B-buffer ediff-buffer-B)
	(C-buffer ediff-buffer-C)
	(anc-buffer ediff-ancestor-buffer)
	(a-prev 1) ; needed to set the first diff line correctly
	(a-prev-pt nil)
	(b-prev 1)
	(b-prev-pt nil)
	(c-prev 1)
	(c-prev-pt nil)
	(anc-prev 1)
	diff-list shift-A shift-B shift-C
	)

    ;; diff list contains word numbers or points, depending on word-mode
    (setq diff-list (cons (if word-mode 'words 'points)
			  diff-list))
    (if bounds
	(setq shift-A
	      (ediff-overlay-start
	       (ediff-get-value-according-to-buffer-type 'A bounds))
	      shift-B
	      (ediff-overlay-start
	       (ediff-get-value-according-to-buffer-type 'B bounds))
	      shift-C
	      (if three-way-comp
		  (ediff-overlay-start
		   (ediff-get-value-according-to-buffer-type 'C bounds)))))

    ;; reset point in buffers A, B, C
    (ediff-with-current-buffer A-buffer
      (goto-char (if shift-A shift-A (point-min))))
    (ediff-with-current-buffer B-buffer
      (goto-char (if shift-B shift-B (point-min))))
    (if three-way-comp
	(ediff-with-current-buffer C-buffer
	  (goto-char (if shift-C shift-C (point-min)))))
    (if (ediff-buffer-live-p anc-buffer)
	(ediff-with-current-buffer anc-buffer
	  (goto-char (point-min))))

    (ediff-with-current-buffer diff-buffer
      (goto-char (point-min))
      (while (re-search-forward ediff-match-diff3-line nil t)
	;; leave point after matched line
       (beginning-of-line 2)
       (let ((agreement (buffer-substring (match-beginning 1) (match-end 1))))
	 ;; if the files A and B are the same and not 3way-comparison,
	 ;; ignore the difference
	 (if (or three-way-comp (not (string-equal agreement "3")))
	     (let* ((a-begin (car (ediff-get-diff3-group "1")))
		    (a-end  (nth 1 (ediff-get-diff3-group "1")))
		    (b-begin (car (ediff-get-diff3-group "2")))
		    (b-end (nth 1 (ediff-get-diff3-group "2")))
		    (c-or-anc-begin (car (ediff-get-diff3-group "3")))
		    (c-or-anc-end (nth 1 (ediff-get-diff3-group "3")))
		    (state-of-merge
		     (cond ((string-equal agreement "1") 'prefer-A)
			   ((string-equal agreement "2") 'prefer-B)
			   (t ediff-default-variant)))
		    (state-of-diff-merge
		     (if (memq state-of-merge '(default-A prefer-A)) 'B 'A))
		    (state-of-diff-comparison
		     (cond ((string-equal agreement "1") 'A)
			   ((string-equal agreement "2") 'B)
			   ((string-equal agreement "3") 'C)))
		    state-of-ancestor
		    c-begin c-end
		    a-begin-pt a-end-pt
		    b-begin-pt b-end-pt
		    c-begin-pt c-end-pt
		    anc-begin-pt anc-end-pt)

	       (setq state-of-ancestor
		     (= c-or-anc-begin c-or-anc-end))

	       (cond (three-way-comp
		      (setq c-begin c-or-anc-begin
			    c-end c-or-anc-end))
		     ((eq ediff-default-variant 'default-B)
		      (setq c-begin b-begin
			    c-end b-end))
		     (t
		      (setq c-begin a-begin
			    c-end a-end)))

	       ;; compute main diff vector
	       (if word-mode
		   ;; make diff-list contain word numbers
		   (setq diff-list
			 (nconc diff-list
				(list (vector
				       (- a-begin a-prev) (- a-end a-begin)
				       (- b-begin b-prev) (- b-end b-begin)
				       (- c-begin c-prev) (- c-end c-begin)
				       nil nil ; dummy ancestor
				       nil     ; state of diff
				       nil     ; state of merge
				       nil     ; state of ancestor
				       )))
			 a-prev a-end
			 b-prev b-end
			 c-prev c-end)
		 ;; else convert lines to points
		 (ediff-with-current-buffer A-buffer
		   (let ((longlines-mode-val
			  (if (and (boundp 'longlines-mode) longlines-mode) 1 0)))
		     ;; we must disable and then restore longlines-mode
		     (if (eq longlines-mode-val 1)
			 (longlines-mode 0))
		     (goto-char (or a-prev-pt shift-A (point-min)))
		     (forward-line (- a-begin a-prev))
		     (setq a-begin-pt (point))
		     (forward-line (- a-end a-begin))
		     (setq a-end-pt (point)
			   a-prev a-end
			   a-prev-pt a-end-pt)
		     (if (eq longlines-mode-val 1)
			 (longlines-mode longlines-mode-val))
		     ))
		 (ediff-with-current-buffer B-buffer
		   (let ((longlines-mode-val
			  (if (and (boundp 'longlines-mode) longlines-mode) 1 0)))
		     (if (eq longlines-mode-val 1)
			 (longlines-mode 0))
		     (goto-char (or b-prev-pt shift-B (point-min)))
		     (forward-line (- b-begin b-prev))
		     (setq b-begin-pt (point))
		     (forward-line (- b-end b-begin))
		     (setq b-end-pt (point)
			   b-prev b-end
			   b-prev-pt b-end-pt)
		     (if (eq longlines-mode-val 1)
			 (longlines-mode longlines-mode-val))
		     ))
		 (ediff-with-current-buffer C-buffer
		   (let ((longlines-mode-val
			  (if (and (boundp 'longlines-mode) longlines-mode) 1 0)))
		     (if (eq longlines-mode-val 1)
			 (longlines-mode 0))
		     (goto-char (or c-prev-pt shift-C (point-min)))
		     (forward-line (- c-begin c-prev))
		     (setq c-begin-pt (point))
		     (forward-line (- c-end c-begin))
		     (setq c-end-pt (point)
			   c-prev c-end
			   c-prev-pt c-end-pt)
		     (if (eq longlines-mode-val 1)
			 (longlines-mode longlines-mode-val))
		     ))
		 (if (ediff-buffer-live-p anc-buffer)
		     (ediff-with-current-buffer anc-buffer
		       (let ((longlines-mode-val
			      (if (and (boundp 'longlines-mode) longlines-mode) 1 0)))
			 (if (eq longlines-mode-val 1)
			     (longlines-mode 0))
			 (forward-line (- c-or-anc-begin anc-prev))
			 (setq anc-begin-pt (point))
			 (forward-line (- c-or-anc-end c-or-anc-begin))
			 (setq anc-end-pt (point)
			       anc-prev c-or-anc-end)
			 (if (eq longlines-mode-val 1)
			     (longlines-mode longlines-mode-val))
			 )))
		 (setq diff-list
		       (nconc
			diff-list
			;; if comparing with ancestor, then there also is a
			;; state-of-difference marker
			(if three-way-comp
			    (list (vector
				   a-begin-pt a-end-pt
				   b-begin-pt b-end-pt
				   c-begin-pt c-end-pt
				   nil nil ; ancestor begin/end
				   state-of-diff-comparison
				   nil	; state of merge
				   nil  ; state of ancestor
				   ))
			  (list (vector a-begin-pt a-end-pt
					b-begin-pt b-end-pt
					c-begin-pt c-end-pt
					anc-begin-pt anc-end-pt
					state-of-diff-merge
					state-of-merge
					state-of-ancestor
					)))
			)))
	       ))

	 ))) ; end ediff-with-current-buffer
    diff-list
    ))

;; Generate the difference vector and overlays for three files
;; File-C is either the third file to compare (in case of 3-way comparison)
;; or it is the ancestor file.
(defun ediff-setup-diff-regions3 (file-A file-B file-C)
  ;; looking for '-i' or a 'i' among clustered non-long options
  (if (string-match "^-i\\| -i\\|\\(^\\| \\)-[^- ]+i" ediff-diff-options)
      (error "Option `-i' is not allowed in `ediff-diff3-options'"))

  (or (ediff-buffer-live-p ediff-diff-buffer)
      (setq ediff-diff-buffer
	    (get-buffer-create (ediff-unique-buffer-name "*ediff-diff" "*"))))

  (message "Computing differences ...")
  (ediff-exec-process ediff-diff3-program ediff-diff-buffer 'synchronize
		      ediff-actual-diff3-options file-A file-B file-C)

  (ediff-prepare-error-list ediff-diff3-ok-lines-regexp ediff-diff-buffer)
  ;;(message "Computing differences ... done")
  (ediff-convert-diffs-to-overlays
   (ediff-extract-diffs3
    ediff-diff-buffer
    ediff-word-mode ediff-3way-comparison-job ediff-narrow-bounds)
   ))


;; Execute PROGRAM asynchronously, unless OS/2, Windows-*, or DOS, or unless
;; SYNCH is non-nil.  BUFFER must be a buffer object, and must be alive.  The
;; OPTIONS arg is a list of options to pass to PROGRAM. It may be a blank
;; string.  All elements in FILES must be strings.  We also delete nil from
;; args.
(defun ediff-exec-process (program buffer synch options &rest files)
  (let ((data (match-data))
	;; If this is a buffer job, we are diffing temporary files
	;; produced by Emacs with ediff-coding-system-for-write, so
	;; use the same encoding to read the results.
	(coding-system-for-read
	 (if (string-match "buffer" (symbol-name ediff-job-name))
	     ediff-coding-system-for-write
	   ediff-coding-system-for-read))
	args)
    (setq args (append (split-string options) files))
    (setq args (delete "" (delq nil args))) ; delete nil and "" from arguments
    ;; the --binary option, if present, should be used only for buffer jobs
    ;; or for refining the differences
    (or (string-match "buffer" (symbol-name ediff-job-name))
	(eq buffer ediff-fine-diff-buffer)
	(setq args (delete "--binary" args)))
    (unwind-protect
	(let ((directory default-directory)
	      proc)
	  (with-current-buffer buffer
	    (erase-buffer)
	    (setq default-directory directory)
	    (if (or (memq system-type '(ms-dos windows-nt))
		    synch)
		;; In Windows do it synchronously, since Windows doesn't let us
		;; delete files used by other processes. Thus, in ediff-buffers
		;; and similar functions, we can't delete temp files because
		;; they might be used by the asynch process that computes
		;; custom diffs. So, we have to wait till custom diff
		;; subprocess is done.
		;; In DOS, must synchronize because DOS doesn't have
		;; asynchronous processes.
		(apply 'call-process program nil buffer nil args)
	      ;; On other systems, do it asynchronously.
	      (setq proc (get-buffer-process buffer))
	      (if proc (kill-process proc))
	      (setq proc
		    (apply 'start-process "Custom Diff" buffer program args))
	      (setq mode-line-process '(":%s"))
	      (set-process-sentinel proc 'ediff-process-sentinel)
	      (set-process-filter proc 'ediff-process-filter)
	      )))
      (store-match-data data))))

;; This is shell-command-filter from simple.el in Emacs.
;; Copied here because XEmacs doesn't have it.
(defun ediff-process-filter (proc string)
  ;; Do save-excursion by hand so that we can leave point numerically unchanged
  ;; despite an insertion immediately after it.
  (let* ((obuf (current-buffer))
         (buffer (process-buffer proc))
         opoint
         (window (get-buffer-window buffer))
         (pos (window-start window)))
    (unwind-protect
        (progn
          (set-buffer buffer)
          (or (= (point) (point-max))
              (setq opoint (point)))
          (goto-char (point-max))
          (insert-before-markers string))
      ;; insert-before-markers moved this marker: set it back.
      (set-window-start window pos)
      ;; Finish our save-excursion.
      (if opoint
          (goto-char opoint))
      (set-buffer obuf))))

;; like shell-command-sentinel but doesn't print an exit status message
;; we do this because diff always exits with status 1, if diffs are found
;; so shell-command-sentinel displays a confusing message to the user
(defun ediff-process-sentinel (process signal)
  (if (and (memq (process-status process) '(exit signal))
           (buffer-name (process-buffer process)))
      (progn
        (with-current-buffer (process-buffer process)
          (setq mode-line-process nil))
        (delete-process process))))


;;; Word functions used to refine the current diff

(defvar ediff-forward-word-function 'ediff-forward-word
  "*Function to call to move to the next word.
Used for splitting difference regions into individual words.")
(make-variable-buffer-local 'ediff-forward-word-function)

;; \240 is Unicode symbol for nonbreakable whitespace
(defvar ediff-whitespace " \n\t\f\r\240"
  "*Characters constituting white space.
These characters are ignored when differing regions are split into words.")
(make-variable-buffer-local 'ediff-whitespace)

(defvar ediff-word-1
  (if (featurep 'xemacs) "a-zA-Z---_" "-[:word:]_")
  "*Characters that constitute words of type 1.
More precisely, [ediff-word-1] is a regexp that matches type 1 words.
See `ediff-forward-word' for more details.")
(make-variable-buffer-local 'ediff-word-1)

(defvar ediff-word-2 "0-9.,"
  "*Characters that constitute words of type 2.
More precisely, [ediff-word-2] is a regexp that matches type 2 words.
See `ediff-forward-word' for more details.")
(make-variable-buffer-local 'ediff-word-2)

(defvar ediff-word-3 "`'?!:;\"{}[]()"
  "*Characters that constitute words of type 3.
More precisely, [ediff-word-3] is a regexp that matches type 3 words.
See `ediff-forward-word' for more details.")
(make-variable-buffer-local 'ediff-word-3)

(defvar ediff-word-4
  (concat "^" ediff-word-1 ediff-word-2 ediff-word-3 ediff-whitespace)
  "*Characters that constitute words of type 4.
More precisely, [ediff-word-4] is a regexp that matches type 4 words.
See `ediff-forward-word' for more details.")
(make-variable-buffer-local 'ediff-word-4)

;; Split region along word boundaries. Each word will be on its own line.
;; Output to buffer out-buffer.
(defun ediff-forward-word ()
  "Move point one word forward.
There are four types of words, each of which consists entirely of
characters in `ediff-word-1', `ediff-word-2', `ediff-word-3', or
`ediff-word-4'.  Words are recognized by passing these one after another as
arguments to `skip-chars-forward'."
  (or (> (+ (skip-chars-forward ediff-word-1)
	    (skip-syntax-forward "w"))
	 0)
      (> (skip-chars-forward ediff-word-2) 0)
      (> (skip-chars-forward ediff-word-3) 0)
      (> (skip-chars-forward ediff-word-4) 0)
      ))


(defun ediff-wordify (beg end in-buffer out-buffer &optional control-buf)
  (let ((forward-word-function
	 ;; eval in control buf to let user create local versions for
	 ;; different invocations
	 (if control-buf
	     (ediff-with-current-buffer control-buf
	       ediff-forward-word-function)
	   ediff-forward-word-function))
	inbuf-syntax-tbl sv-point diff-string)
    (with-current-buffer in-buffer
     (setq inbuf-syntax-tbl
	   (if control-buf
	       (ediff-with-current-buffer control-buf
		 ediff-syntax-table)
	     (syntax-table)))
     (setq diff-string (buffer-substring-no-properties beg end))

     (set-buffer out-buffer)
     ;; Make sure that temp buff syntax table is the same as the original buf
     ;; syntax tbl, because we use ediff-forward-word in both and
     ;; ediff-forward-word depends on the syntax classes of characters.
     (set-syntax-table inbuf-syntax-tbl)
     (erase-buffer)
     (insert diff-string)
     (goto-char (point-min))
     (skip-chars-forward ediff-whitespace)
     (delete-region (point-min) (point))

     (while (not (eobp))
       (funcall forward-word-function)
       (setq sv-point (point))
       (skip-chars-forward ediff-whitespace)
       (delete-region sv-point (point))
       (insert "\n")))))

;; copy string specified as BEG END from IN-BUF to OUT-BUF
(defun ediff-copy-to-buffer (beg end in-buffer out-buffer)
  (with-current-buffer out-buffer
    (erase-buffer)
    (insert-buffer-substring in-buffer beg end)
    (goto-char (point-min))))


;; goto word #n starting at current position in buffer `buf'
;; For ediff, a word is determined by ediff-forward-word-function
;; If `flag' is non-nil, goto the end of the n-th word.
(defun ediff-goto-word (n buf &optional flag)
  ;; remember val ediff-forward-word-function has in ctl buf
  (let ((fwd-word-fun ediff-forward-word-function)
	(syntax-tbl ediff-syntax-table))
    (ediff-with-current-buffer buf
      (skip-chars-forward ediff-whitespace)
      (ediff-with-syntax-table syntax-tbl
	(while (> n 1)
	  (funcall fwd-word-fun)
	  (skip-chars-forward ediff-whitespace)
	  (setq n (1- n)))
	(if (and flag (> n 0))
	    (funcall fwd-word-fun)))
      (point))))

(defun ediff-same-file-contents (f1 f2)
  "Return t if files F1 and F2 have identical contents."
  (if (and (not (file-directory-p f1))
           (not (file-directory-p f2)))
      (let ((res
	     (apply 'call-process ediff-cmp-program nil nil nil
		    (append ediff-cmp-options (list (expand-file-name f1)
						    (expand-file-name f2))))
	     ))
	(and (numberp res) (eq res 0)))
    ))


(defun ediff-same-contents (d1 d2 &optional filter-re)
  "Return t if D1 and D2 have the same content.
D1 and D2 can either be both directories or both regular files.
Symlinks and the likes are not handled.
If FILTER-RE is non-nil, recursive checking in directories
affects only files whose names match the expression."
  ;; Normalize empty filter RE to nil.
  (unless (> (length filter-re) 0) (setq filter-re nil))
  ;; Indicate progress
  (message "Comparing '%s' and '%s' modulo '%s'" d1 d2 filter-re)
  (cond
   ;; D1 & D2 directories => recurse
   ((and (file-directory-p d1)
         (file-directory-p d2))
    (if (null ediff-recurse-to-subdirectories)
	(if (y-or-n-p "Compare subdirectories recursively? ")
	    (setq ediff-recurse-to-subdirectories 'yes)
	  (setq ediff-recurse-to-subdirectories 'no)))
    (if (eq ediff-recurse-to-subdirectories 'yes)
	(let* ((all-entries-1 (directory-files d1 t filter-re))
	       (all-entries-2 (directory-files d2 t filter-re))
	       (entries-1 (ediff-delete-all-matches "^\\.\\.?$" all-entries-1))
	       (entries-2 (ediff-delete-all-matches "^\\.\\.?$" all-entries-2))
	       )

	  (ediff-same-file-contents-lists entries-1 entries-2 filter-re)
	  ))
    ) ; end of the directories case
   ;; D1 & D2 are both files => compare directly
   ((and (file-regular-p d1)
         (file-regular-p d2))
    (ediff-same-file-contents d1 d2))
   ;; Otherwise => false: unequal contents
   )
  )

;; If lists have the same length and names of files are pairwise equal
;; (removing the directories) then compare contents pairwise.
;; True if all contents are the same; false otherwise
(defun ediff-same-file-contents-lists (entries-1 entries-2 filter-re)
  ;; First, check only the names (works quickly and ensures a
  ;; precondition for subsequent code)
  (if (and (= (length entries-1) (length entries-2))
	   (equal (mapcar 'file-name-nondirectory entries-1)
		  (mapcar 'file-name-nondirectory entries-2)))
      ;; With name equality established, compare the entries
      ;; through recursion.
      (let ((continue t))
	(while (and entries-1 continue)
	  (if (ediff-same-contents
	       (car entries-1) (car entries-2) filter-re)
	      (setq entries-1 (cdr entries-1)
		    entries-2 (cdr entries-2))
	    (setq continue nil))
	  )
	;; if reached the end then lists are equal
	(null entries-1))
    )
  )


;; ARG1 is a regexp, ARG2 is a list of full-filenames
;; Delete all entries that match the regexp
(defun ediff-delete-all-matches (regex file-list-list)
  (let (result elt)
    (while file-list-list
      (setq elt (car file-list-list))
      (or (string-match regex (file-name-nondirectory elt))
	  (setq result (cons elt result)))
      (setq file-list-list (cdr file-list-list)))
    (reverse result)))


(defun ediff-set-actual-diff-options ()
  (if ediff-ignore-case
      (setq ediff-actual-diff-options
	    (concat ediff-diff-options " " ediff-ignore-case-option)
	    ediff-actual-diff3-options
	    (concat ediff-diff3-options " " ediff-ignore-case-option3))
    (setq ediff-actual-diff-options ediff-diff-options
	  ediff-actual-diff3-options ediff-diff3-options)
    )
  (setq-default ediff-actual-diff-options ediff-actual-diff-options
		ediff-actual-diff3-options ediff-actual-diff3-options)
  )


;; Ignore case handling - some ideas from drew.adams@@oracle.com
(defun ediff-toggle-ignore-case ()
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (setq ediff-ignore-case (not ediff-ignore-case))
  (ediff-set-actual-diff-options)
  (if ediff-ignore-case
      (message "Ignoring regions that differ only in case")
    (message "Ignoring case differences turned OFF"))
  (cond (ediff-merge-job
	 (message "Ignoring letter case is too dangerous in merge jobs"))
	((and ediff-diff3-job (string= ediff-ignore-case-option3 ""))
	 (message "Ignoring letter case is not supported by this diff3 program"))
	((and (not ediff-3way-job) (string= ediff-ignore-case-option ""))
	 (message "Ignoring letter case is not supported by this diff program"))
	(t
	 (sit-for 1)
	 (ediff-update-diffs)))
  )



;; Local Variables:
;; eval: (put 'ediff-defvar-local 'lisp-indent-hook 'defun)
;; eval: (put 'ediff-with-current-buffer 'lisp-indent-hook 1)
;; eval: (put 'ediff-with-current-buffer 'edebug-form-spec '(form body))
;; End:

;;; ediff-diff.el ends here
