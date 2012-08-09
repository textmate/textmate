;;; cmacexp.el --- expand C macros in a region

;; Copyright (C) 1992, 1994, 1996, 2000-2012  Free Software Foundation, Inc.

;; Author: Francesco Potorti` <pot@gnu.org>
;; Adapted-By: ESR
;; Keywords: c

;; Yoni Rabkin <yoni@rabkins.net> contacted the maintainer of this
;; file, and the maintainer agreed that when a bug is filed in the
;; Emacs bug reporting system against this file, a copy of the bug
;; report be sent to the maintainer's email address. However, the
;; maintainer prefers not to be the only person maintaining this file
;; in future.

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

;; USAGE =============================================================

;; In C mode C-C C-e is bound to c-macro-expand.  The result of the
;; expansion is put in a separate buffer.  A user option allows the
;; window displaying the buffer to be optimally sized.
;;
;; When called with a C-u prefix, c-macro-expand replaces the selected
;; region with the expansion.  Both the preprocessor name and the
;; initial flag can be set by the user.  If c-macro-prompt-flag is set
;; to a non-nil value the user is offered to change the options to the
;; preprocessor each time c-macro-expand is invoked.  Preprocessor
;; arguments default to the last ones entered.  If c-macro-prompt-flag
;; is nil, one must use M-x set-variable to set a different value for
;; c-macro-cppflags.

;; A c-macro-expansion function is provided for non-interactive use.

;; INSTALLATION ======================================================

;; Put the following in your ~/.emacs file.

;; If you want the *Macroexpansion* window to be not higher than
;; necessary:
;;(setq c-macro-shrink-window-flag t)
;;
;; If you use a preprocessor other than /lib/cpp (be careful to set a
;; -C option or equivalent in order to make the preprocessor not to
;; strip the comments):
;;(setq c-macro-preprocessor "gpp -C")
;;
;; If you often use a particular set of flags:
;;(setq c-macro-cppflags "-I /usr/include/local -DDEBUG"
;;
;; If you want the "Preprocessor arguments: " prompt:
;;(setq c-macro-prompt-flag t)

;; BUG REPORTS =======================================================

;; Please report bugs, suggestions, complaints and so on to
;; pot@gnu.org (Francesco Potorti`).

;; IMPROVEMENTS OVER emacs 18.xx cmacexp.el ==========================

;; - A lot of user and programmer visible changes.  See above.
;; - #line directives are inserted, so __LINE__ and __FILE__ are
;;   correctly expanded.  Works even with START inside a string, a
;;   comment or a region #ifdef'd away by cpp. cpp is invoked with -C,
;;   making comments visible in the expansion.
;; - All work is done in core memory, no need for temporary files.

;; ACKNOWLEDGEMENTS ==================================================

;; A lot of thanks to Don Maszle who did a great work of testing, bug
;; reporting and suggestion of new features.  This work has been
;; partially inspired by Don Maszle and Jonathan Segal's.

;; BUGS ==============================================================

;; If the start point of the region is inside a macro definition the
;; macro expansion is often inaccurate.

;;; Code:

(require 'cc-mode)

(provide 'cmacexp)

(defvar msdos-shells)


(defgroup c-macro nil
  "Expand C macros in a region."
  :group 'c)


(defcustom c-macro-shrink-window-flag nil
  "*Non-nil means shrink the *Macroexpansion* window to fit its contents."
  :type 'boolean
  :group 'c-macro)

(defcustom c-macro-prompt-flag nil
  "*Non-nil makes `c-macro-expand' prompt for preprocessor arguments."
  :type 'boolean
  :group 'c-macro)

(defcustom c-macro-preprocessor
  (cond ;; Solaris has it in an unusual place.
	((and (string-match "^[^-]*-[^-]*-\\(solaris\\|sunos5\\)"
			    system-configuration)
	      (file-exists-p "/opt/SUNWspro/SC3.0.1/bin/acomp"))
	 "/opt/SUNWspro/SC3.0.1/bin/acomp -C -E")
        ((locate-file "/usr/ccs/lib/cpp"
		      '("/") exec-suffixes 'file-executable-p)
	 "/usr/ccs/lib/cpp -C")
	((locate-file "/lib/cpp"
		      '("/") exec-suffixes 'file-executable-p)
	 "/lib/cpp -C")
	;; On some systems, we cannot rely on standard directories to
	;; find CPP.  In fact, we cannot rely on having cpp, either,
	;; in some GCC versions.
	((locate-file "cpp" exec-path exec-suffixes 'file-executable-p)
	 "cpp -C")
	(t "gcc -E -C -o - -"))
  "The preprocessor used by the cmacexp package.

If you change this, be sure to preserve the `-C' (don't strip comments)
option, or to set an equivalent one."
  :type 'string
  :group 'c-macro)

(defcustom c-macro-cppflags ""
  "*Preprocessor flags used by `c-macro-expand'."
  :type 'string
  :group 'c-macro)

(defconst c-macro-buffer-name "*Macroexpansion*")

;;;###autoload
(defun c-macro-expand (start end subst)
  "Expand C macros in the region, using the C preprocessor.
Normally display output in temp buffer, but
prefix arg means replace the region with it.

`c-macro-preprocessor' specifies the preprocessor to use.
Tf the user option `c-macro-prompt-flag' is non-nil
prompt for arguments to the preprocessor \(e.g. `-DDEBUG -I ./include'),
otherwise use `c-macro-cppflags'.

Noninteractive args are START, END, SUBST.
For use inside Lisp programs, see also `c-macro-expansion'."

  (interactive "r\nP")
  (let ((inbuf (current-buffer))
	(displaybuf (if subst
			(get-buffer c-macro-buffer-name)
		      (get-buffer-create c-macro-buffer-name)))
	(expansion ""))
    ;; Build the command string.
    (if c-macro-prompt-flag
	(setq c-macro-cppflags
	      (read-string "Preprocessor arguments: "
			   c-macro-cppflags)))
    ;; Decide where to display output.
    (if (and subst
	     (and buffer-read-only (not inhibit-read-only))
	     (not (eq inbuf displaybuf)))
	(progn
	  (message
	   "Buffer is read only: displaying expansion in alternate window")
	  (sit-for 2)
	  (setq subst nil)
	  (or displaybuf
	      (setq displaybuf (get-buffer-create c-macro-buffer-name)))))
    ;; Expand the macro and output it.
    (setq expansion (c-macro-expansion start end
				       (concat c-macro-preprocessor " "
					       c-macro-cppflags) t))
    (if subst
	(let ((exchange (= (point) start)))
	  (delete-region start end)
	  (insert expansion)
	  (if exchange
	      (exchange-point-and-mark)))
      (set-buffer displaybuf)
      (setq buffer-read-only nil)
      (buffer-disable-undo displaybuf)
      (erase-buffer)
      (insert expansion)
      (set-buffer-modified-p nil)
      (if (string= "" expansion)
	  (message "Null expansion")
	(c-macro-display-buffer))
      (setq buffer-read-only t)
      (setq buffer-auto-save-file-name nil)
      (bury-buffer displaybuf))))


;; Display the current buffer in a window which is either just large
;; enough to contain the entire buffer, or half the size of the
;; screen, whichever is smaller.  Do not select the new
;; window.
;;
;; Several factors influence window resizing so that the window is
;; sized optimally if it is created anew, and so that it is messed
;; with minimally if it has been created by the user.  If the window
;; chosen for display exists already but contains something else, the
;; window is not re-sized.  If the window already contains the current
;; buffer, it is never shrunk, but possibly expanded.  Finally, if the
;; variable c-macro-shrink-window-flag is nil the window size is *never*
;; changed.
(defun c-macro-display-buffer ()
  (goto-char (point-min))
  (c-mode)
  (let ((oldwinheight (window-height))
	(alreadythere			;the window was already there
	 (get-buffer-window (current-buffer)))
	(popped nil))			;the window popped changing the layout
    (or alreadythere
	(progn
	  (display-buffer (current-buffer) t)
	  (setq popped (/= oldwinheight (window-height)))))
    (if (and c-macro-shrink-window-flag	;user wants fancy shrinking :\)
	     (or alreadythere popped))
	;; Enlarge up to half screen, or shrink properly.
	(let ((oldwin (selected-window))
	      (minheight 0)
	      (maxheight 0))
	  (save-excursion
	    (select-window (get-buffer-window (current-buffer)))
	    (setq minheight (if alreadythere
				(window-height)
			      window-min-height))
	    (setq maxheight (/ (frame-height) 2))
	    (enlarge-window (- (min maxheight
				    (max minheight
					 (+ 2 (vertical-motion (point-max)))))
			       (window-height)))
	    (goto-char (point-min))
	    (select-window oldwin))))))


(defun c-macro-expansion (start end cppcommand &optional display)
  "Run a preprocessor on region and return the output as a string.
Expand the region between START and END in the current buffer using
the shell command CPPCOMMAND (e.g. \"/lib/cpp -C -DDEBUG\").
Be sure to use a -C (don't strip comments) or equivalent option.
Optional arg DISPLAY non-nil means show messages in the echo area."

;; Copy the current buffer's contents to a temporary hidden buffer.
;; Delete from END to end of buffer.  Insert a preprocessor #line
;; directive at START and after each #endif following START that are
;; not inside a comment or a string.  Put all the strings thus
;; inserted (without the "line" substring) in a list named linelist.
;; If START is inside a comment, prepend "*/" and append "/*" to the
;; #line directive.  If inside a string, prepend and append "\"".
;; Preprocess the buffer contents, then look for all the lines stored
;; in linelist starting from end of buffer.  The last line so found is
;; where START was, so return the substring from point to end of
;; buffer.
  (let ((inbuf (current-buffer))
	(outbuf (get-buffer-create " *C Macro Expansion*"))
	(filename (if (and buffer-file-name
			   (string-match (regexp-quote default-directory)
					 buffer-file-name))
		      (substring buffer-file-name (match-end 0))
		    (buffer-name)))
	(mymsg (format "Invoking %s%s%s on region..."
		       c-macro-preprocessor
		       (if (string= "" c-macro-cppflags) "" " ")
		       c-macro-cppflags))
	(uniquestring "??? !!! ??? start of c-macro expansion ??? !!! ???")
	(startlinenum 0)
	(linenum 0)
	(startstat ())
	(startmarker "")
	(exit-status 0)
	(tempname (make-temp-file
		   (expand-file-name "cmacexp"
				     (or small-temporary-file-directory
					 temporary-file-directory)))))
    (unwind-protect
	(save-excursion
	  (save-restriction
	    (widen)
            (let ((in-syntax-table (syntax-table)))
              (set-buffer outbuf)
              (setq buffer-read-only nil)
              (erase-buffer)
              (set-syntax-table in-syntax-table))
	    (insert-buffer-substring inbuf 1 end))

	  ;; We have copied inbuf to outbuf.  Point is at end of
	  ;; outbuf.  Inset a newline at the end, so cpp can correctly
	  ;; parse a token ending at END.
          (insert "\n")

	  ;; Save sexp status and line number at START.
	  (setq startstat (parse-partial-sexp 1 start))
	  (setq startlinenum (+ (count-lines 1 (point))
				(if (bolp) 1 0)))

	  ;; Now we insert the #line directives after all #endif or
	  ;; #else following START going backward, so the lines we
	  ;; insert don't change the line numbers.
	  ;(switch-to-buffer outbuf) (debug)	;debugging instructions
	  (goto-char (point-max))
	  (while (re-search-backward "\n#\\(endif\\|else\\)\\>" start 'move)
	    (if (equal (nthcdr 3 (parse-partial-sexp start (point)
						     nil nil startstat))
		       '(nil nil nil 0 nil)) ;neither in string nor in
					     ;comment nor after quote
		(progn
		  (goto-char (match-end 0))
		  (setq linenum (+ startlinenum
				   (count-lines start (point))))
		  (insert (format "\n#line %d \"%s\"\n" linenum filename))
		  (goto-char (match-beginning 0)))))

	  ;; Now we are at START.  Insert the first #line directive.
	  ;; This must work even inside a string or comment, or after a
	  ;; quote.
	  (let* ((startinstring (nth 3 startstat))
		 (startincomment (nth 4 startstat))
		 (startafterquote (nth 5 startstat))
		 (startinbcomment (nth 7 startstat)))
	    (insert (if startafterquote " " "")
		    (cond (startinstring
			   (char-to-string startinstring))
			  (startincomment "*/")
			  (""))
		    (setq startmarker
			  (concat "\n" uniquestring
				  (cond (startinstring
					 (char-to-string startinstring))
					(startincomment "/*")
					(startinbcomment "//"))
				  (if startafterquote "\\")))
		    (format "\n#line %d \"%s\"\n" startlinenum filename)))

	  ;; Call the preprocessor.
	  (if display (message "%s" mymsg))
	  (setq exit-status
		(call-process-region 1 (point-max)
				     shell-file-name
				     t (list t tempname) nil "-c"
				     cppcommand))
	  (if display (message "%s" (concat mymsg "done")))
	  (if (= (buffer-size) 0)
	      ;; Empty output is normal after a fatal error.
	      (insert "\nPreprocessor produced no output\n")
	    ;; Find and delete the mark of the start of the expansion.
	    ;; Look for `# nn "file.c"' lines and delete them.
	    (goto-char (point-min))
	    (search-forward startmarker)
	    (delete-region 1 (point)))
	  (while (re-search-forward (concat "^# [0-9]+ \""
					    (regexp-quote filename)
					    "\"") nil t)
	    (beginning-of-line)
	    (let ((beg (point)))
	      (forward-line 1)
	      (delete-region beg (point))))

	  ;; If CPP got errors, show them at the beginning.
	  ;; MS-DOS shells don't return the exit code of their children.
	  ;; Look at the size of the error message file instead, but
	  ;; don't punish those MS-DOS users who have a shell that does
	  ;; return an error code.
	  (or (and (or (not (boundp 'msdos-shells))
		       (not (member (file-name-nondirectory shell-file-name)
				    msdos-shells)))
		   (eq exit-status 0))
	      (zerop (nth 7 (file-attributes (expand-file-name tempname))))
	      (progn
		(goto-char (point-min))
		;; Put the messages inside a comment, so they won't get in
		;; the way of font-lock, highlighting etc.
		(insert
		 (format "/* Preprocessor terminated with status %s\n\n   Messages from `%s\':\n\n"
			 exit-status cppcommand))
		(goto-char (+ (point)
			      (nth 1 (insert-file-contents tempname))))
		(insert "\n\n*/\n")))
	  (delete-file tempname)

	  ;; Compute the return value, keeping in account the space
	  ;; inserted at the end of the buffer.
	  (buffer-substring 1 (max 1 (- (point-max) 1))))

      ;; Cleanup.
      (kill-buffer outbuf))))

;;; cmacexp.el ends here
