;;; edmacro.el --- keyboard macro editor

;; Copyright (C) 1993-1994, 2001-2012 Free Software Foundation, Inc.

;; Author: Dave Gillespie <daveg@synaptics.com>
;; Maintainer: Dave Gillespie <daveg@synaptics.com>
;; Version: 2.01
;; Keywords: abbrev

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

;;; Usage:
;;
;; The `C-x C-k e' (`edit-kbd-macro') command edits a keyboard macro
;; in a special buffer.  It prompts you to type a key sequence,
;; which should be one of:
;;
;;  * RET or `C-x e' (call-last-kbd-macro), to edit the most
;;    recently defined keyboard macro.
;;
;;  * `M-x' followed by a command name, to edit a named command
;;    whose definition is a keyboard macro.
;;
;;  * `C-h l' (view-lossage), to edit the 300 most recent keystrokes
;;    and install them as the "current" macro.
;;
;;  * any key sequence whose definition is a keyboard macro.
;;
;; This file includes a version of `insert-kbd-macro' that uses the
;; more readable format defined by these routines.
;;
;; Also, the `read-kbd-macro' command parses the region as
;; a keyboard macro, and installs it as the "current" macro.
;; This and `format-kbd-macro' can also be called directly as
;; Lisp functions.

;; Type `C-h m', or see the documentation for `edmacro-mode' below,
;; for information about the format of written keyboard macros.

;; `edit-kbd-macro' formats the macro with one command per line,
;; including the command names as comments on the right.  If the
;; formatter gets confused about which keymap was used for the
;; characters, the command-name comments will be wrong but that
;; won't hurt anything.

;; With a prefix argument, `edit-kbd-macro' will format the
;; macro in a more concise way that omits the comments.

;;; Code:

(eval-when-compile
 (require 'cl))

(require 'kmacro)

;;; The user-level commands for editing macros.

(defcustom edmacro-eight-bits nil
  "Non-nil if `edit-kbd-macro' should leave 8-bit characters intact.
Default nil means to write characters above \\177 in octal notation."
  :type 'boolean
  :group 'kmacro)

(defvar edmacro-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'edmacro-finish-edit)
    (define-key map "\C-c\C-q" 'edmacro-insert-key)
    map))

(defvar edmacro-store-hook)
(defvar edmacro-finish-hook)
(defvar edmacro-original-buffer)

;;;###autoload
(defun edit-kbd-macro (keys &optional prefix finish-hook store-hook)
  "Edit a keyboard macro.
At the prompt, type any key sequence which is bound to a keyboard macro.
Or, type `C-x e' or RET to edit the last keyboard macro, `C-h l' to edit
the last 300 keystrokes as a keyboard macro, or `M-x' to edit a macro by
its command name.
With a prefix argument, format the macro in a more concise way."
  (interactive "kKeyboard macro to edit (C-x e, M-x, C-h l, or keys): \nP")
  (when keys
    (let ((cmd (if (arrayp keys) (key-binding keys) keys))
	  (mac nil) (mac-counter nil) (mac-format nil)
	  kmacro)
      (cond (store-hook
	     (setq mac keys)
	     (setq cmd nil))
	    ((or (memq cmd '(call-last-kbd-macro kmacro-call-macro
			     kmacro-end-or-call-macro kmacro-end-and-call-macro))
		 (member keys '("\r" [return])))
	     (or last-kbd-macro
		 (y-or-n-p "No keyboard macro defined.  Create one? ")
		 (keyboard-quit))
	     (setq mac (or last-kbd-macro ""))
	     (setq keys nil)
	     (setq cmd 'last-kbd-macro))
	    ((eq cmd 'execute-extended-command)
	     (setq cmd (read-command "Name of keyboard macro to edit: "))
	     (if (string-equal cmd "")
		 (error "No command name given"))
	     (setq keys nil)
	     (setq mac (symbol-function cmd)))
	    ((memq cmd '(view-lossage electric-view-lossage))
	     (setq mac (recent-keys))
	     (setq keys nil)
	     (setq cmd 'last-kbd-macro))
	    ((null cmd)
	     (error "Key sequence %s is not defined" (key-description keys)))
	    ((symbolp cmd)
	     (setq mac (symbol-function cmd)))
	    (t
	     (setq mac cmd)
	     (setq cmd nil)))
      (when (setq kmacro (kmacro-extract-lambda mac))
	(setq mac (car kmacro)
	      mac-counter (nth 1 kmacro)
	      mac-format (nth 2 kmacro)))
      (unless (arrayp mac)
	(error "Key sequence %s is not a keyboard macro"
	       (key-description keys)))
      (message "Formatting keyboard macro...")
      (let* ((oldbuf (current-buffer))
	     (mmac (edmacro-fix-menu-commands mac))
	     (fmt (edmacro-format-keys mmac 1))
	     (fmtv (edmacro-format-keys mmac (not prefix)))
	     (buf (get-buffer-create "*Edit Macro*")))
	(message "Formatting keyboard macro...done")
	(switch-to-buffer buf)
	(kill-all-local-variables)
	(use-local-map edmacro-mode-map)
	(setq buffer-read-only nil)
	(setq major-mode 'edmacro-mode)
	(setq mode-name "Edit Macro")
	(set (make-local-variable 'edmacro-original-buffer) oldbuf)
	(set (make-local-variable 'edmacro-finish-hook) finish-hook)
	(set (make-local-variable 'edmacro-store-hook) store-hook)
	(erase-buffer)
	(insert ";; Keyboard Macro Editor.  Press C-c C-c to finish; "
		"press C-x k RET to cancel.\n")
	(insert ";; Original keys: " fmt "\n")
	(unless store-hook
	  (insert "\nCommand: " (if cmd (symbol-name cmd) "none") "\n")
	  (let ((gkeys (where-is-internal (or cmd mac) '(keymap))))
	    (if (and keys (not (member keys gkeys)))
		(setq gkeys (cons keys gkeys)))
	    (if gkeys
		(while gkeys
		  (insert "Key: " (edmacro-format-keys (pop gkeys) 1) "\n"))
	      (insert "Key: none\n")))
	  (when (and mac-counter mac-format)
	    (insert (format "Counter: %d\nFormat: \"%s\"\n" mac-counter mac-format))))
	(insert "\nMacro:\n\n")
	(save-excursion
	  (insert fmtv "\n"))
	(recenter '(4))
	(when (eq mac mmac)
	  (set-buffer-modified-p nil))
	(run-hooks 'edmacro-format-hook)))))

;;; The next two commands are provided for convenience and backward
;;; compatibility.

;;;###autoload
(defun edit-last-kbd-macro (&optional prefix)
  "Edit the most recently defined keyboard macro."
  (interactive "P")
  (edit-kbd-macro 'call-last-kbd-macro prefix))

;;;###autoload
(defun edit-named-kbd-macro (&optional prefix)
  "Edit a keyboard macro which has been given a name by `name-last-kbd-macro'."
  (interactive "P")
  (edit-kbd-macro 'execute-extended-command prefix))

;;;###autoload
(defun read-kbd-macro (start &optional end)
  "Read the region as a keyboard macro definition.
The region is interpreted as spelled-out keystrokes, e.g., \"M-x abc RET\".
See documentation for `edmacro-mode' for details.
Leading/trailing \"C-x (\" and \"C-x )\" in the text are allowed and ignored.
The resulting macro is installed as the \"current\" keyboard macro.

In Lisp, may also be called with a single STRING argument in which case
the result is returned rather than being installed as the current macro.
The result will be a string if possible, otherwise an event vector.
Second argument NEED-VECTOR means to return an event vector always."
  (interactive "r")
  (if (stringp start)
      (edmacro-parse-keys start end)
    (setq last-kbd-macro (edmacro-parse-keys (buffer-substring start end)))))

;;;###autoload
(defun format-kbd-macro (&optional macro verbose)
  "Return the keyboard macro MACRO as a human-readable string.
This string is suitable for passing to `read-kbd-macro'.
Second argument VERBOSE means to put one command per line with comments.
If VERBOSE is `1', put everything on one line.  If VERBOSE is omitted
or nil, use a compact 80-column format."
  (and macro (symbolp macro) (setq macro (symbol-function macro)))
  (edmacro-format-keys (or macro last-kbd-macro) verbose))

;;; Commands for *Edit Macro* buffer.

(defun edmacro-finish-edit ()
  (interactive)
  (unless (eq major-mode 'edmacro-mode)
    (error
     "This command is valid only in buffers created by `edit-kbd-macro'"))
  (run-hooks 'edmacro-finish-hook)
  (let ((cmd nil) (keys nil) (no-keys nil)
	(mac-counter nil) (mac-format nil)
	(top (point-min)))
    (goto-char top)
    (let ((case-fold-search nil))
      (while (cond ((looking-at "[ \t]*\\($\\|;;\\|REM[ \t\n]\\)")
		    t)
		   ((looking-at "Command:[ \t]*\\([^ \t\n]*\\)[ \t]*$")
		    (when edmacro-store-hook
		      (error "\"Command\" line not allowed in this context"))
		    (let ((str (buffer-substring (match-beginning 1)
						 (match-end 1))))
		      (unless (equal str "")
			(setq cmd (and (not (equal str "none"))
				       (intern str)))
			(and (fboundp cmd) (not (arrayp (symbol-function cmd)))
			     (not (get cmd 'kmacro))
			     (not (y-or-n-p
				   (format "Command %s is already defined; %s"
					   cmd "proceed? ")))
			     (keyboard-quit))))
		    t)
		   ((looking-at "Key:\\(.*\\)$")
		    (when edmacro-store-hook
		      (error "\"Key\" line not allowed in this context"))
		    (let ((key (edmacro-parse-keys
				(buffer-substring (match-beginning 1)
						  (match-end 1)))))
		      (unless (equal key "")
			(if (equal key "none")
			    (setq no-keys t)
			  (push key keys)
			  (let ((b (key-binding key)))
			    (and b (commandp b) (not (arrayp b))
				 (not (kmacro-extract-lambda b))
				 (or (not (fboundp b))
				     (not (or (arrayp (symbol-function b))
					      (get b 'kmacro))))
				 (not (y-or-n-p
				       (format "Key %s is already defined; %s"
					       (edmacro-format-keys key 1)
					       "proceed? ")))
				 (keyboard-quit))))))
		    t)
		   ((looking-at "Counter:[ \t]*\\([^ \t\n]*\\)[ \t]*$")
		    (when edmacro-store-hook
		      (error "\"Counter\" line not allowed in this context"))
		    (let ((str (buffer-substring (match-beginning 1)
						 (match-end 1))))
		      (unless (equal str "")
			(setq mac-counter (string-to-number str))))
		    t)
		   ((looking-at "Format:[ \t]*\"\\([^\n]*\\)\"[ \t]*$")
		    (when edmacro-store-hook
		      (error "\"Format\" line not allowed in this context"))
		    (let ((str (buffer-substring (match-beginning 1)
						 (match-end 1))))
		      (unless (equal str "")
			(setq mac-format str)))
		    t)
		   ((looking-at "Macro:[ \t\n]*")
		    (goto-char (match-end 0))
		    nil)
		   ((eobp) nil)
		   (t (error "Expected a `Macro:' line")))
	(forward-line 1))
      (setq top (point)))
    (let* ((buf (current-buffer))
	   (str (buffer-substring top (point-max)))
	   (modp (buffer-modified-p))
	   (obuf edmacro-original-buffer)
	   (store-hook edmacro-store-hook)
	   (finish-hook edmacro-finish-hook))
      (unless (or cmd keys store-hook (equal str ""))
	(error "No command name or keys specified"))
      (when modp
	(when (buffer-name obuf)
	  (set-buffer obuf))
	(message "Compiling keyboard macro...")
	(let ((mac (edmacro-parse-keys str)))
	  (message "Compiling keyboard macro...done")
	  (if store-hook
	      (funcall store-hook mac)
	    (when (eq cmd 'last-kbd-macro)
	      (setq last-kbd-macro (and (> (length mac) 0) mac))
	      (setq cmd nil))
	    (when cmd
	      (if (= (length mac) 0)
		  (fmakunbound cmd)
		(fset cmd
		      (if (and mac-counter mac-format)
			  (kmacro-lambda-form mac mac-counter mac-format)
			mac))))
	    (if no-keys
		(when cmd
		  (loop for key in (where-is-internal cmd '(keymap)) do
			(global-unset-key key)))
	      (when keys
		(if (= (length mac) 0)
		    (loop for key in keys do (global-unset-key key))
		  (loop for key in keys do
			(global-set-key key
					(or cmd
					    (if (and mac-counter mac-format)
						(kmacro-lambda-form mac mac-counter mac-format)
					      mac))))))))))
      (kill-buffer buf)
      (when (buffer-name obuf)
	(switch-to-buffer obuf))
      (when finish-hook
	(funcall finish-hook)))))

(defun edmacro-insert-key (key)
  "Insert the written name of a key in the buffer."
  (interactive "kKey to insert: ")
  (if (bolp)
      (insert (edmacro-format-keys key t) "\n")
    (insert (edmacro-format-keys key) " ")))

(defun edmacro-mode ()
  "\\<edmacro-mode-map>Keyboard Macro Editing mode.  Press \
\\[edmacro-finish-edit] to save and exit.
To abort the edit, just kill this buffer with \\[kill-buffer] RET.

Press \\[edmacro-insert-key] to insert the name of any key by typing the key.

The editing buffer contains a \"Command:\" line and any number of
\"Key:\" lines at the top.  These are followed by a \"Macro:\" line
and the macro itself as spelled-out keystrokes: `C-x C-f foo RET'.

The \"Command:\" line specifies the command name to which the macro
is bound, or \"none\" for no command name.  Write \"last-kbd-macro\"
to refer to the current keyboard macro (as used by \\[call-last-kbd-macro]).

The \"Key:\" lines specify key sequences to which the macro is bound,
or \"none\" for no key bindings.

You can edit these lines to change the places where the new macro
is stored.


Format of keyboard macros during editing:

Text is divided into \"words\" separated by whitespace.  Except for
the words described below, the characters of each word go directly
as characters of the macro.  The whitespace that separates words
is ignored.  Whitespace in the macro must be written explicitly,
as in \"foo SPC bar RET\".

 * The special words RET, SPC, TAB, DEL, LFD, ESC, and NUL represent
   special control characters.  The words must be written in uppercase.

 * A word in angle brackets, e.g., <return>, <down>, or <f1>, represents
   a function key.  (Note that in the standard configuration, the
   function key <return> and the control key RET are synonymous.)
   You can use angle brackets on the words RET, SPC, etc., but they
   are not required there.

 * Keys can be written by their ASCII code, using a backslash followed
   by up to six octal digits.  This is the only way to represent keys
   with codes above \\377.

 * One or more prefixes M- (meta), C- (control), S- (shift), A- (alt),
   H- (hyper), and s- (super) may precede a character or key notation.
   For function keys, the prefixes may go inside or outside of the
   brackets:  C-<down> = <C-down>.  The prefixes may be written in
   any order:  M-C-x = C-M-x.

   Prefixes are not allowed on multi-key words, e.g., C-abc, except
   that the Meta prefix is allowed on a sequence of digits and optional
   minus sign:  M--123 = M-- M-1 M-2 M-3.

 * The `^' notation for control characters also works:  ^M = C-m.

 * Double angle brackets enclose command names:  <<next-line>> is
   shorthand for M-x next-line RET.

 * Finally, REM or ;; causes the rest of the line to be ignored as a
   comment.

Any word may be prefixed by a multiplier in the form of a decimal
number and `*':  3*<right> = <right> <right> <right>, and
10*foo = foofoofoofoofoofoofoofoofoofoo.

Multiple text keys can normally be strung together to form a word,
but you may need to add whitespace if the word would look like one
of the above notations:  `; ; ;' is a keyboard macro with three
semicolons, but `;;;' is a comment.  Likewise, `\\ 1 2 3' is four
keys but `\\123' is a single key written in octal, and `< right >'
is seven keys but `<right>' is a single function key.  When in
doubt, use whitespace."
  (interactive)
  (error "This mode can be enabled only by `edit-kbd-macro'"))
(put 'edmacro-mode 'mode-class 'special)

;;; Formatting a keyboard macro as human-readable text.

(defun edmacro-format-keys (macro &optional verbose)
  (setq macro (edmacro-fix-menu-commands macro))
  (let* ((maps (current-active-maps))
	 (pkeys '(end-macro ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?- ?\C-u
		  ?\M-- ?\M-0 ?\M-1 ?\M-2 ?\M-3 ?\M-4 ?\M-5 ?\M-6
		  ?\M-7 ?\M-8 ?\M-9))
	 (mdigs (nthcdr 13 pkeys))
	 (maxkey (if edmacro-eight-bits 255 127))
	 (case-fold-search nil)
	 (res-words '("NUL" "TAB" "LFD" "RET" "ESC" "SPC" "DEL" "REM"))
	 (rest-mac (vconcat macro [end-macro]))
	 (res "")
	 (len 0)
	 (one-line (eq verbose 1)))
    (if one-line (setq verbose nil))
    (when (stringp macro)
      (loop for i below (length macro) do
	    (when (>= (aref rest-mac i) 128)
	      (incf (aref rest-mac i) (- ?\M-\^@ 128)))))
    (while (not (eq (aref rest-mac 0) 'end-macro))
      (let* ((prefix
	      (or (and (integerp (aref rest-mac 0))
		       (memq (aref rest-mac 0) mdigs)
		       (memq (key-binding (edmacro-subseq rest-mac 0 1))
			     '(digit-argument negative-argument))
		       (let ((i 1))
			 (while (memq (aref rest-mac i) (cdr mdigs))
			   (incf i))
			 (and (not (memq (aref rest-mac i) pkeys))
			      (prog1 (vconcat "M-" (edmacro-subseq rest-mac 0 i) " ")
				(callf edmacro-subseq rest-mac i)))))
		  (and (eq (aref rest-mac 0) ?\C-u)
		       (eq (key-binding [?\C-u]) 'universal-argument)
		       (let ((i 1))
			 (while (eq (aref rest-mac i) ?\C-u)
			   (incf i))
			 (and (not (memq (aref rest-mac i) pkeys))
			      (prog1 (loop repeat i concat "C-u ")
				(callf edmacro-subseq rest-mac i)))))
		  (and (eq (aref rest-mac 0) ?\C-u)
		       (eq (key-binding [?\C-u]) 'universal-argument)
		       (let ((i 1))
			 (when (eq (aref rest-mac i) ?-)
			   (incf i))
			 (while (memq (aref rest-mac i)
				      '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
			   (incf i))
			 (and (not (memq (aref rest-mac i) pkeys))
			      (prog1 (vconcat "C-u " (edmacro-subseq rest-mac 1 i) " ")
				(callf edmacro-subseq rest-mac i)))))))
	     (bind-len (apply 'max 1
			      (loop for map in maps
				    for b = (lookup-key map rest-mac)
				    when b collect b)))
	     (key (edmacro-subseq rest-mac 0 bind-len))
	     (fkey nil) tlen tkey
	     (bind (or (loop for map in maps for b = (lookup-key map key)
			     thereis (and (not (integerp b)) b))
		       (and (setq fkey (lookup-key local-function-key-map rest-mac))
			    (setq tlen fkey tkey (edmacro-subseq rest-mac 0 tlen)
				  fkey (lookup-key local-function-key-map tkey))
			    (loop for map in maps
				  for b = (lookup-key map fkey)
				  when (and (not (integerp b)) b)
				  do (setq bind-len tlen key tkey)
				  and return b
				  finally do (setq fkey nil)))))
	     (first (aref key 0))
	     (text (loop for i from bind-len below (length rest-mac)
			 for ch = (aref rest-mac i)
			 while (and (integerp ch)
				    (> ch 32) (< ch maxkey) (/= ch 92)
				    (eq (key-binding (char-to-string ch))
					'self-insert-command)
				    (or (> i (- (length rest-mac) 2))
					(not (eq ch (aref rest-mac (+ i 1))))
					(not (eq ch (aref rest-mac (+ i 2))))))
			 finally return i))
	     desc)
	(if (stringp bind) (setq bind nil))
	(cond ((and (eq bind 'self-insert-command) (not prefix)
		    (> text 1) (integerp first)
		    (> first 32) (<= first maxkey) (/= first 92)
		    (progn
		      (if (> text 30) (setq text 30))
		      (setq desc (concat (edmacro-subseq rest-mac 0 text)))
		      (when (string-match "^[ACHMsS]-." desc)
			(setq text 2)
			(callf substring desc 0 2))
		      (not (string-match
			    "^;;\\|^<.*>$\\|^\\\\[0-9]+$\\|^[0-9]+\\*."
			    desc))))
	       (when (or (string-match "^\\^.$" desc)
			 (member desc res-words))
		 (setq desc (mapconcat 'char-to-string desc " ")))
	       (when verbose
		 (setq bind (format "%s * %d" bind text)))
	       (setq bind-len text))
	      ((and (eq bind 'execute-extended-command)
		    (> text bind-len)
		    (memq (aref rest-mac text) '(return 13))
		    (progn
		      (setq desc (concat (edmacro-subseq rest-mac bind-len text)))
		      (commandp (intern-soft desc))))
	       (if (commandp (intern-soft desc)) (setq bind desc))
	       (setq desc (format "<<%s>>" desc))
	       (setq bind-len (1+ text)))
	      (t
	       (setq desc (mapconcat
			   (function
			    (lambda (ch)
			      (cond
			       ((integerp ch)
				(concat
				 (loop for pf across "ACHMsS"
				       for bit in '(?\A-\^@ ?\C-\^@ ?\H-\^@
						    ?\M-\^@ ?\s-\^@ ?\S-\^@)
				       when (/= (logand ch bit) 0)
				       concat (format "%c-" pf))
				 (let ((ch2 (logand ch (1- (lsh 1 18)))))
				   (cond ((<= ch2 32)
					  (case ch2
					    (0 "NUL") (9 "TAB") (10 "LFD")
					    (13 "RET") (27 "ESC") (32 "SPC")
					    (t
					     (format "C-%c"
						     (+ (if (<= ch2 26) 96 64)
							ch2)))))
					 ((= ch2 127) "DEL")
					 ((<= ch2 maxkey) (char-to-string ch2))
					 (t (format "\\%o" ch2))))))
			       ((symbolp ch)
				(format "<%s>" ch))
			       (t
				(error "Unrecognized item in macro: %s" ch)))))
			   (or fkey key) " "))))
	(if prefix
	    (setq desc (concat (edmacro-sanitize-for-string prefix) desc)))
	(unless (string-match " " desc)
	  (let ((times 1) (pos bind-len))
	    (while (not (edmacro-mismatch rest-mac rest-mac
					  0 bind-len pos (+ bind-len pos)))
	      (incf times)
	      (incf pos bind-len))
	    (when (> times 1)
	      (setq desc (format "%d*%s" times desc))
	      (setq bind-len (* bind-len times)))))
	(setq rest-mac (edmacro-subseq rest-mac bind-len))
	(if verbose
	    (progn
	      (unless (equal res "") (callf concat res "\n"))
	      (callf concat res desc)
	      (when (and bind (or (stringp bind) (symbolp bind)))
		(callf concat res
		  (make-string (max (- 3 (/ (length desc) 8)) 1) 9)
		  ";; " (if (stringp bind) bind (symbol-name bind))))
	      (setq len 0))
	  (if (and (> (+ len (length desc) 2) 72) (not one-line))
	      (progn
		(callf concat res "\n ")
		(setq len 1))
	    (unless (equal res "")
	      (callf concat res " ")
	      (incf len)))
	  (callf concat res desc)
	  (incf len (length desc)))))
    res))

(defun edmacro-mismatch (cl-seq1 cl-seq2 cl-start1 cl-end1 cl-start2 cl-end2)
  "Compare SEQ1 with SEQ2, return index of first mismatching element.
Return nil if the sequences match.  If one sequence is a prefix of the
other, the return value indicates the end of the shorted sequence.
\n(fn SEQ1 SEQ2 START1 END1 START2 END2)"
  (let (cl-test cl-test-not cl-key cl-from-end)
    (or cl-end1 (setq cl-end1 (length cl-seq1)))
    (or cl-end2 (setq cl-end2 (length cl-seq2)))
    (if cl-from-end
	(progn
	  (while (and (< cl-start1 cl-end1) (< cl-start2 cl-end2)
		      (cl-check-match (elt cl-seq1 (1- cl-end1))
				      (elt cl-seq2 (1- cl-end2))))
	    (setq cl-end1 (1- cl-end1) cl-end2 (1- cl-end2)))
	  (and (or (< cl-start1 cl-end1) (< cl-start2 cl-end2))
	       (1- cl-end1)))
      (let ((cl-p1 (and (listp cl-seq1) (nthcdr cl-start1 cl-seq1)))
	    (cl-p2 (and (listp cl-seq2) (nthcdr cl-start2 cl-seq2))))
	(while (and (< cl-start1 cl-end1) (< cl-start2 cl-end2)
		    (cl-check-match (if cl-p1 (car cl-p1)
				      (aref cl-seq1 cl-start1))
				    (if cl-p2 (car cl-p2)
				      (aref cl-seq2 cl-start2))))
	  (setq cl-p1 (cdr cl-p1) cl-p2 (cdr cl-p2)
		cl-start1 (1+ cl-start1) cl-start2 (1+ cl-start2)))
	(and (or (< cl-start1 cl-end1) (< cl-start2 cl-end2))
	     cl-start1)))))

(defun edmacro-subseq (seq start &optional end)
  "Return the subsequence of SEQ from START to END.
If END is omitted, it defaults to the length of the sequence.
If START or END is negative, it counts from the end."
  (if (stringp seq) (substring seq start end)
    (let (len)
      (and end (< end 0) (setq end (+ end (setq len (length seq)))))
      (if (< start 0) (setq start (+ start (or len (setq len (length seq))))))
      (cond ((listp seq)
	     (if (> start 0) (setq seq (nthcdr start seq)))
	     (if end
		 (let ((res nil))
		   (while (>= (setq end (1- end)) start)
		     (push (pop seq) res))
		   (nreverse res))
	       (copy-sequence seq)))
	    (t
	     (or end (setq end (or len (length seq))))
	     (let ((res (make-vector (max (- end start) 0) nil))
		   (i 0))
	       (while (< start end)
		 (aset res i (aref seq start))
		 (setq i (1+ i) start (1+ start)))
	       res))))))

(defun edmacro-sanitize-for-string (seq)
  "Convert a key sequence vector SEQ into a string.
The string represents the same events; Meta is indicated by bit 7.
This function assumes that the events can be stored in a string."
  (setq seq (copy-sequence seq))
  (loop for i below (length seq) do
        (when (logand (aref seq i) 128)
          (setf (aref seq i) (logand (aref seq i) 127))))
  seq)

(defun edmacro-fix-menu-commands (macro &optional noerror)
  (if (vectorp macro)
      (let (result)
	;; Make a list of the elements.
	(setq macro (append macro nil))
	(dolist (ev macro)
	  (cond ((atom ev)
		 (push ev result))
		((eq (car ev) 'help-echo))
		((eq (car ev) 'switch-frame))
		((equal ev '(menu-bar))
		 (push 'menu-bar result))
		((equal (cadadr ev) '(menu-bar))
		 (push (vector 'menu-bar (car ev)) result))
		;; It would be nice to do pop-up menus, too, but not enough
		;; info is recorded in macros to make this possible.
		(noerror
		 ;; Just ignore mouse events.
		 nil)
		(t
		 (error "Macros with mouse clicks are not %s"
			"supported by this command"))))
	;; Reverse them again and make them back into a vector.
	(vconcat (nreverse result)))
    macro))

;;; Parsing a human-readable keyboard macro.

(defun edmacro-parse-keys (string &optional need-vector)
  (let ((case-fold-search nil)
	(len (length string)) ; We won't alter string in the loop below.
	(pos 0)
	(res []))
    (while (and (< pos len)
		(string-match "[^ \t\n\f]+" string pos))
      (let* ((word-beg (match-beginning 0))
	     (word-end (match-end 0))
	     (word (substring string word-beg len))
	     (times 1)
	     key)
	;; Try to catch events of the form "<as df>".
	(if (string-match "\\`<[^ <>\t\n\f][^>\t\n\f]*>" word)
	    (setq word (match-string 0 word)
		  pos (+ word-beg (match-end 0)))
	  (setq word (substring string word-beg word-end)
		pos word-end))
	(when (string-match "\\([0-9]+\\)\\*." word)
	  (setq times (string-to-number (substring word 0 (match-end 1))))
	  (setq word (substring word (1+ (match-end 1)))))
	(cond ((string-match "^<<.+>>$" word)
	       (setq key (vconcat (if (eq (key-binding [?\M-x])
					  'execute-extended-command)
				      [?\M-x]
				    (or (car (where-is-internal
					      'execute-extended-command))
					[?\M-x]))
				  (substring word 2 -2) "\r")))
	      ((and (string-match "^\\(\\([ACHMsS]-\\)*\\)<\\(.+\\)>$" word)
		    (progn
		      (setq word (concat (substring word (match-beginning 1)
						    (match-end 1))
					 (substring word (match-beginning 3)
						    (match-end 3))))
		      (not (string-match
			    "\\<\\(NUL\\|RET\\|LFD\\|ESC\\|SPC\\|DEL\\)$"
			    word))))
	       (setq key (list (intern word))))
	      ((or (equal word "REM") (string-match "^;;" word))
	       (setq pos (string-match "$" string pos)))
	      (t
	       (let ((orig-word word) (prefix 0) (bits 0))
		 (while (string-match "^[ACHMsS]-." word)
		   (incf bits (cdr (assq (aref word 0)
					 '((?A . ?\A-\^@) (?C . ?\C-\^@)
					   (?H . ?\H-\^@) (?M . ?\M-\^@)
					   (?s . ?\s-\^@) (?S . ?\S-\^@)))))
		   (incf prefix 2)
		   (callf substring word 2))
		 (when (string-match "^\\^.$" word)
		   (incf bits ?\C-\^@)
		   (incf prefix)
		   (callf substring word 1))
		 (let ((found (assoc word '(("NUL" . "\0") ("RET" . "\r")
					    ("LFD" . "\n") ("TAB" . "\t")
					    ("ESC" . "\e") ("SPC" . " ")
					    ("DEL" . "\177")))))
		   (when found (setq word (cdr found))))
		 (when (string-match "^\\\\[0-7]+$" word)
		   (loop for ch across word
			 for n = 0 then (+ (* n 8) ch -48)
			 finally do (setq word (vector n))))
		 (cond ((= bits 0)
			(setq key word))
		       ((and (= bits ?\M-\^@) (stringp word)
			     (string-match "^-?[0-9]+$" word))
			(setq key (loop for x across word collect (+ x bits))))
		       ((/= (length word) 1)
			(error "%s must prefix a single character, not %s"
			       (substring orig-word 0 prefix) word))
		       ((and (/= (logand bits ?\C-\^@) 0) (stringp word)
			     ;; We used to accept . and ? here,
			     ;; but . is simply wrong,
			     ;; and C-? is not used (we use DEL instead).
			     (string-match "[@-_a-z]" word))
			(setq key (list (+ bits (- ?\C-\^@)
					   (logand (aref word 0) 31)))))
		       (t
			(setq key (list (+ bits (aref word 0)))))))))
	(when key
	  (loop repeat times do (callf vconcat res key)))))
    (when (and (>= (length res) 4)
	       (eq (aref res 0) ?\C-x)
	       (eq (aref res 1) ?\()
	       (eq (aref res (- (length res) 2)) ?\C-x)
	       (eq (aref res (- (length res) 1)) ?\)))
      (setq res (edmacro-subseq res 2 -2)))
    (if (and (not need-vector)
	     (loop for ch across res
		   always (and (characterp ch)
			       (let ((ch2 (logand ch (lognot ?\M-\^@))))
				 (and (>= ch2 0) (<= ch2 127))))))
	(concat (loop for ch across res
		      collect (if (= (logand ch ?\M-\^@) 0)
				  ch (+ ch 128))))
      res)))

(provide 'edmacro)

;;; edmacro.el ends here
