;;; find-gc.el --- detect functions that call the garbage collector

;; Copyright (C) 1992, 2001-2012 Free Software Foundation, Inc.

;; Maintainer: FSF

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

;; Produce in find-gc-unsafe-list the set of all functions that may invoke GC.
;; This expects the Emacs sources to live in find-gc-source-directory.
;; It creates a temporary working directory /tmp/esrc.

;;; Code:

(defvar find-gc-unsafe-list nil
  "The list of unsafe functions is placed here by `find-gc-unsafe'.")

(defvar find-gc-source-directory)

(defvar find-gc-subrs-callers nil
  "Alist of users of subrs, from GC testing.
Each entry has the form (FUNCTION . FUNCTIONS-THAT-CALL-IT).")

(defvar find-gc-subrs-called nil
  "Alist of subrs called, in GC testing.
Each entry has the form (FUNCTION . FUNCTIONS-IT-CALLS).")


;;; Functions on this list are safe, even if they appear to be able
;;; to call the target.

(defvar find-gc-noreturn-list '(Fsignal Fthrow wrong_type_argument))

;;; This was originally generated directory-files, but there were
;;; too many files there that were not actually compiled.  The
;;; list below was created for a HP-UX 7.0 system.

(defvar find-gc-source-files
  '("dispnew.c" "scroll.c" "xdisp.c" "window.c"
    "term.c" "cm.c" "emacs.c" "keyboard.c" "macros.c"
    "keymap.c" "sysdep.c" "buffer.c" "filelock.c"
    "insdel.c" "marker.c" "minibuf.c" "fileio.c"
    "dired.c" "cmds.c" "casefiddle.c"
    "indent.c" "search.c" "regex.c" "undo.c"
    "alloc.c" "data.c" "doc.c" "editfns.c"
    "callint.c" "eval.c" "fns.c" "print.c" "lread.c"
    "abbrev.c" "syntax.c" "unexcoff.c"
    "bytecode.c" "process.c" "callproc.c" "doprnt.c"
    "x11term.c" "x11fns.c"))


(defun find-gc-unsafe ()
  "Return a list of unsafe functions--that is, which can call GC.
Also store it in `find-gc-unsafe'."
  (trace-call-tree nil)
  (trace-use-tree)
  (find-unsafe-funcs 'Fgarbage_collect)
  (setq find-gc-unsafe-list
	(sort find-gc-unsafe-list
	      (function (lambda (x y)
			  (string-lessp (car x) (car y))))))
)

;;; This does a depth-first search to find all functions that can
;;; ultimately call the function "target".  The result is an a-list
;;; in find-gc-unsafe-list; the cars are the unsafe functions, and the cdrs
;;; are (one of) the unsafe functions that these functions directly
;;; call.

(defun find-unsafe-funcs (target)
  (setq find-gc-unsafe-list (list (list target)))
  (trace-unsafe target)
)

(defun trace-unsafe (func)
  (let ((used (assq func find-gc-subrs-callers)))
    (or used
	(error "No find-gc-subrs-callers for %s" (car find-gc-unsafe-list)))
    (while (setq used (cdr used))
      (or (assq (car used) find-gc-unsafe-list)
	  (memq (car used) find-gc-noreturn-list)
	  (progn
	    (push (cons (car used) func) find-gc-unsafe-list)
	    (trace-unsafe (car used))))))
)




(defun trace-call-tree (&optional already-setup)
  (message "Setting up directories...")
  (or already-setup
      (progn
	;; Gee, wouldn't a built-in "system" function be handy here.
	(call-process "csh" nil nil nil "-c" "rm -rf /tmp/esrc")
	(call-process "csh" nil nil nil "-c" "mkdir /tmp/esrc")
	(call-process "csh" nil nil nil "-c"
		      (format "ln -s %s/*.[ch] /tmp/esrc"
			      find-gc-source-directory))))
  (with-current-buffer (get-buffer-create "*Trace Call Tree*")
    (setq find-gc-subrs-called nil)
    (let ((case-fold-search nil)
	  (files find-gc-source-files)
	  name entry)
      (while files
	(message "Compiling %s..." (car files))
	(call-process "csh" nil nil nil "-c"
		      (format "gcc -dr -c /tmp/esrc/%s -o /dev/null"
			      (car files)))
	(erase-buffer)
	(insert-file-contents (concat "/tmp/esrc/" (car files) ".rtl"))
	(while (re-search-forward ";; Function \\|(call_insn " nil t)
	  (if (= (char-after (- (point) 3)) ?o)
	      (progn
		(looking-at "[a-zA-Z0-9_]+")
		(setq name (intern (buffer-substring (match-beginning 0)
						     (match-end 0))))
		(message "%s : %s" (car files) name)
		(setq entry (list name)
		      find-gc-subrs-called (cons entry find-gc-subrs-called)))
	    (if (looking-at ".*\n?.*\"\\([A-Za-z0-9_]+\\)\"")
		(progn
		  (setq name (intern (buffer-substring (match-beginning 1)
						       (match-end 1))))
		  (or (memq name (cdr entry))
		      (setcdr entry (cons name (cdr entry))))))))
	(delete-file (concat "/tmp/esrc/" (car files) ".rtl"))
	(setq files (cdr files)))))
)


(defun trace-use-tree ()
  (setq find-gc-subrs-callers (mapcar 'list (mapcar 'car find-gc-subrs-called)))
  (let ((ptr find-gc-subrs-called)
	p2 found)
    (while ptr
      (setq p2 (car ptr))
      (while (setq p2 (cdr p2))
	(if (setq found (assq (car p2) find-gc-subrs-callers))
	    (setcdr found (cons (car (car ptr)) (cdr found)))))
      (setq ptr (cdr ptr))))
)

(provide 'find-gc)

;;; find-gc.el ends here
