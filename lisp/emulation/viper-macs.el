;;; viper-macs.el --- functions implementing keyboard macros for Viper

;; Copyright (C) 1994-1997, 2000-2012 Free Software Foundation, Inc.

;; Author: Michael Kifer <kifer@cs.stonybrook.edu>
;; Package: viper

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

(provide 'viper-macs)

;; compiler pacifier
(defvar viper-ex-work-buf)
(defvar viper-custom-file-name)
(defvar viper-current-state)
(defvar viper-fast-keyseq-timeout)

;; loading happens only in non-interactive compilation
;; in order to spare non-viperized emacs from being viperized
(if noninteractive
    (eval-when-compile
      (require 'viper-cmd)
      ))
;; end pacifier

(require 'viper-util)
(require 'viper-keym)


;;; Variables

;; Register holding last macro.
(defvar viper-last-macro-reg nil)

;; format of the elements of kbd alists:
;; (name ((buf . macr)...(buf . macr)) ((maj-mode . macr)...) (t . macr))
;; kbd macro alist for Vi state
(defvar viper-vi-kbd-macro-alist nil)
;; same for insert/replace state
(defvar viper-insert-kbd-macro-alist nil)
;; same for emacs state
(defvar viper-emacs-kbd-macro-alist nil)

;; Internal var that passes info between start-kbd-macro and end-kbd-macro
;; in :map and :map!
(defvar viper-kbd-macro-parameters nil)

(defvar viper-this-kbd-macro nil
  "Vector of keys representing the name of currently running Viper kbd macro.")
(defvar viper-last-kbd-macro nil
  "Vector of keys representing the name of last Viper keyboard macro.")

(defcustom viper-repeat-from-history-key 'f12
  "Prefix key for accessing previously typed Vi commands.

The previous command is accessible, as usual, via `.'.  The command before this
can be invoked as `<this key> 1', and the command before that, and the command
before that one is accessible as `<this key> 2'.
The notation for these keys is borrowed from XEmacs.  Basically,
a key is a symbol, e.g., `a', `\\1', `f2', etc., or a list, e.g.,
`(meta control f1)'."
  :type 'sexp
  :group 'viper)



;;; Code

;; Ex map command
(defun ex-map ()
  (let ((mod-char "")
	macro-name macro-body map-args ins)
    (save-window-excursion
      (set-buffer viper-ex-work-buf)
      (if (looking-at "!")
	  (progn
	    (setq ins t
		  mod-char "!")
	    (forward-char 1))))
    (setq map-args (ex-map-read-args mod-char)
	  macro-name (car map-args)
	  macro-body (cdr map-args))
    (setq viper-kbd-macro-parameters (list ins mod-char macro-name macro-body))
    (if macro-body
	(viper-end-mapping-kbd-macro 'ignore)
      (ex-fixup-history (format "map%s %S" mod-char
				(viper-display-macro macro-name)))
      ;; if defining macro for insert, switch there for authentic WYSIWYG
      (if ins (viper-change-state-to-insert))
      (start-kbd-macro nil)
      (define-key viper-vi-intercept-map "\C-x)" 'viper-end-mapping-kbd-macro)
      (define-key viper-insert-intercept-map "\C-x)" 'viper-end-mapping-kbd-macro)
      (define-key viper-emacs-intercept-map "\C-x)" 'viper-end-mapping-kbd-macro)
      (message "Mapping %S in %s state.  Type macro definition followed by `C-x )'"
	       (viper-display-macro macro-name)
	       (if ins "Insert" "Vi")))
    ))


;; Ex unmap
(defun ex-unmap ()
  (let ((mod-char "")
	temp macro-name ins)
    (save-window-excursion
      (set-buffer viper-ex-work-buf)
      (if (looking-at "!")
	  (progn
	    (setq ins t
		  mod-char "!")
	    (forward-char 1))))

    (setq macro-name (ex-unmap-read-args mod-char))
    (setq temp (viper-fixup-macro (vconcat macro-name))) ;; copy and fixup
    (ex-fixup-history (format "unmap%s %S" mod-char
			      (viper-display-macro temp)))
    (viper-unrecord-kbd-macro macro-name (if ins 'insert-state 'vi-state))
    ))


;; read arguments for ex-map
(defun ex-map-read-args (variant)
  (let ((cursor-in-echo-area t)
	(key-seq [])
	temp key event message
	macro-name macro-body args)

    (condition-case nil
	(setq args (concat (ex-get-inline-cmd-args ".*map[!]*[ \t]?" "\n\C-m")
			   " nil nil ")
	      temp (read-from-string args)
	      macro-name (car temp)
	      macro-body (car (read-from-string args (cdr temp))))
      (error
       (signal
	'error
	'("map: Macro name and body must be a quoted string or a vector"))))

    ;; We expect macro-name to be a vector, a string, or a quoted string.
    ;; In the second case, it will emerge as a symbol when read from
    ;; the above read-from-string.  So we need to convert it into a string
    (if macro-name
        (cond ((vectorp macro-name) nil)
	      ((stringp macro-name)
	       (setq macro-name (vconcat macro-name)))
	      (t (setq macro-name (vconcat (prin1-to-string macro-name)))))
      (message ":map%s <Macro Name>" variant)(sit-for 2)
      (while
	  (not (member key
		       '(?\C-m ?\n (control m) (control j) return linefeed)))
	(setq key-seq (vconcat key-seq (if key (vector key) [])))
	;; the only keys available for editing are these-- no help while there
	(if (member
	     key
	     '(?\b ?\d '^? '^H (control h) (control \?) backspace delete))
	    (setq key-seq (viper-subseq key-seq 0 (- (length key-seq) 2))))
	(setq message
	      (format
	       ":map%s %s"
	       variant (if (> (length key-seq) 0)
			   (prin1-to-string (viper-display-macro key-seq))
			 "")))
	(message "%s" message)
	(setq event (viper-read-key))
	;;(setq event (viper-read-event))
	(setq key
	      (if (viper-mouse-event-p event)
		  (progn
		    (message "%s (No mouse---only keyboard keys, please)"
			     message)
		    (sit-for 2)
		    nil)
		(viper-event-key event)))
	)
      (setq macro-name key-seq))

    (if (= (length macro-name) 0)
	(error "Can't map an empty macro name"))
    (setq macro-name (viper-fixup-macro macro-name))
    (if (viper-char-array-p macro-name)
	(setq macro-name (viper-char-array-to-macro macro-name)))

    (if macro-body
	(cond ((viper-char-array-p macro-body)
	       (setq macro-body (viper-char-array-to-macro macro-body)))
	      ((vectorp macro-body) nil)
	      (t (error "map: Invalid syntax in macro definition"))))
    (setq cursor-in-echo-area nil)(sit-for 0) ; this overcomes xemacs tty bug
    (cons macro-name macro-body)))



;; read arguments for ex-unmap
(defun ex-unmap-read-args (variant)
  (let ((cursor-in-echo-area t)
	(macro-alist (if (string= variant "!")
			 viper-insert-kbd-macro-alist
		       viper-vi-kbd-macro-alist))
	;; these are disabled just in case, to avoid surprises when doing
	;; completing-read
	viper-vi-kbd-minor-mode viper-insert-kbd-minor-mode
	viper-emacs-kbd-minor-mode
	viper-vi-intercept-minor-mode viper-insert-intercept-minor-mode
	viper-emacs-intercept-minor-mode
	event message
	key key-seq macro-name)
    (setq macro-name (ex-get-inline-cmd-args ".*unma?p?[!]*[ \t]*"))

    (if (> (length macro-name) 0)
	()
      (message ":unmap%s <Name>" variant) (sit-for 2)
      (while
	  (not
	   (member key '(?\C-m ?\n (control m) (control j) return linefeed)))
	(setq key-seq (vconcat key-seq (if key (vector key) [])))
	;; the only keys available for editing are these-- no help while there
	(cond ((member
		key
		'(?\b ?\d '^? '^H (control h) (control \?) backspace delete))
	       (setq key-seq (viper-subseq key-seq 0 (- (length key-seq) 2))))
	      ((member key '(tab (control i) ?\t))
	       (setq key-seq (viper-subseq key-seq 0 (1- (length key-seq))))
	       (setq message
		     (format
		      ":unmap%s %s"
		      variant (if (> (length key-seq) 0)
				  (prin1-to-string
				   (viper-display-macro key-seq))
				"")))
	       (setq key-seq
		     (viper-do-sequence-completion key-seq macro-alist message))
	       ))
	(setq message
	      (format
	       ":unmap%s %s"
	       variant (if (> (length key-seq) 0)
			   (prin1-to-string
			    (viper-display-macro key-seq))
			 "")))
	(message "%s" message)
	(setq event (viper-read-key))
	;;(setq event (viper-read-event))
	(setq key
	      (if (viper-mouse-event-p event)
		  (progn
		    (message "%s (No mouse---only keyboard keys, please)"
			     message)
		    (sit-for 2)
		    nil)
		(viper-event-key event)))
	)
      (setq macro-name key-seq))

    (if (= (length macro-name) 0)
	(error "Can't unmap an empty macro name"))

    ;; convert macro names into vector, if starts with a `['
    (if (memq (elt macro-name 0) '(?\[ ?\"))
	(car (read-from-string macro-name))
      (vconcat macro-name))
    ))


;; Terminate a Vi kbd macro.
;; optional argument IGNORE, if t, indicates that we are dealing with an
;; existing macro that needs to be registered, but there is no need to
;; terminate a kbd macro.
(defun viper-end-mapping-kbd-macro (&optional ignore)
  (interactive)
  (define-key viper-vi-intercept-map "\C-x)" nil)
  (define-key viper-insert-intercept-map "\C-x)" nil)
  (define-key viper-emacs-intercept-map "\C-x)" nil)
  (if (and (not ignore)
	   (or (not viper-kbd-macro-parameters)
	       (not defining-kbd-macro)))
      (error "Not mapping a kbd-macro"))
  (let ((mod-char (nth 1 viper-kbd-macro-parameters))
	(ins (nth 0 viper-kbd-macro-parameters))
	(macro-name (nth 2 viper-kbd-macro-parameters))
	(macro-body (nth 3 viper-kbd-macro-parameters)))
    (setq viper-kbd-macro-parameters nil)
    (or ignore
	(progn
	  (end-kbd-macro nil)
	  (setq macro-body (viper-events-to-macro last-kbd-macro))
	  ;; always go back to Vi, since this is where we started
	  ;; defining macro
	  (viper-change-state-to-vi)))

    (viper-record-kbd-macro macro-name
			  (if ins 'insert-state 'vi-state)
			  (viper-display-macro macro-body))

    (ex-fixup-history (format "map%s %S %S" mod-char
			      (viper-display-macro macro-name)
			      (viper-display-macro macro-body)))
    ))




;;; Recording, unrecording, executing

;; Accepts as macro names: strings and vectors.
;; strings must be strings of characters; vectors must be vectors of keys
;; in canonical form, which is essentially the form used in XEmacs.
;; More general definitions are inherited by more specific scopes:
;; global->major mode->buffer. More specific definitions override more general
(defun viper-record-kbd-macro (macro-name state macro-body &optional scope)
  "Record a Vi macro.  Can be used in `.viper' file to define permanent macros.
MACRO-NAME is a string of characters or a vector of keys.  STATE is
either `vi-state' or `insert-state'.  It specifies the Viper state in which to
define the macro.  MACRO-BODY is a string that represents the keyboard macro.
Optional SCOPE says whether the macro should be global \(t\), mode-specific
\(a major-mode symbol\), or buffer-specific \(buffer name, a string\).
If SCOPE is nil, the user is asked to specify the scope."
  (let* (state-name keymap
	 (macro-alist-var
	  (cond ((eq state 'vi-state)
		 (setq state-name "Vi state"
		       keymap viper-vi-kbd-map)
		 'viper-vi-kbd-macro-alist)
		((memq state '(insert-state replace-state))
		 (setq state-name "Insert state"
		       keymap viper-insert-kbd-map)
		 'viper-insert-kbd-macro-alist)
		(t
		 (setq state-name "Emacs state"
		       keymap viper-emacs-kbd-map)
		 'viper-emacs-kbd-macro-alist)
		 ))
	 new-elt old-elt old-sub-elt msg
	 temp lis lis2)

    (if (= (length macro-name) 0)
	(error "Can't map an empty macro name"))

    ;; Macro-name is usually a vector.  However, command history or macros
    ;; recorded in ~/.viper may be recorded as strings.  So, convert to
    ;; vectors.
    (setq macro-name (viper-fixup-macro macro-name))
    (if (viper-char-array-p macro-name)
	(setq macro-name (viper-char-array-to-macro macro-name)))
    (setq macro-body (viper-fixup-macro macro-body))
    (if (viper-char-array-p macro-body)
	(setq macro-body (viper-char-array-to-macro macro-body)))

    ;; don't ask if scope is given and is of the right type
    (or (eq scope t)
	(stringp scope)
	(and scope (symbolp scope))
	(progn
	  (setq scope
		(cond
		 ((y-or-n-p
		   (format
		    "Map this macro for buffer `%s' only? "
		    (buffer-name)))
		  (setq msg
			(format
			 "%S is mapped to %s for %s in `%s'"
			 (viper-display-macro macro-name)
			 (viper-abbreviate-string
			  (format
			   "%S"
			   (setq temp (viper-display-macro macro-body)))
			  14 "" ""
			  (if (stringp temp) "  ....\"" "  ....]"))
			 state-name (buffer-name)))
		  (buffer-name))
		 ((y-or-n-p
		   (format
		    "Map this macro for the major mode `%S' only? "
		    major-mode))
		  (setq msg
			(format
			 "%S is mapped to %s for %s in `%S'"
			 (viper-display-macro macro-name)
			 (viper-abbreviate-string
			  (format
			   "%S"
			   (setq temp (viper-display-macro macro-body)))
			  14 "" ""
			  (if (stringp macro-body) "  ....\"" "  ....]"))
			 state-name major-mode))
		  major-mode)
		 (t
		  (setq msg
			(format
			 "%S is globally mapped to %s in %s"
			 (viper-display-macro macro-name)
			 (viper-abbreviate-string
			  (format
			   "%S"
			   (setq temp (viper-display-macro macro-body)))
			  14 "" ""
			  (if (stringp macro-body) "  ....\"" "  ....]"))
			 state-name))
		  t)))
	  (if (y-or-n-p
	       (format "Save this macro in %s? "
		       (viper-abbreviate-file-name viper-custom-file-name)))
	      (viper-save-string-in-file
	       (format "\n(viper-record-kbd-macro %S '%S %s '%S)"
		       (viper-display-macro macro-name)
		       state
		       ;; if we don't let vector macro-body through %S,
		       ;; the symbols `\.' `\[' etc will be converted into
		       ;; characters, causing invalid read  error on recorded
		       ;; macros in .viper.
		       ;; I am not sure is macro-body can still be a string at
		       ;; this point, but I am preserving this option anyway.
		       (if (vectorp macro-body)
			   (format "%S" macro-body)
			 macro-body)
		       scope)
	       viper-custom-file-name))

	  (message "%s" msg)
	  ))

    (setq new-elt
	  (cons macro-name
		(cond ((eq scope t) (list nil nil (cons t nil)))
		      ((symbolp scope)
		       (list nil (list (cons scope nil)) (cons t nil)))
		      ((stringp scope)
		       (list (list (cons scope nil)) nil (cons t nil))))))
    (setq old-elt (assoc macro-name (eval macro-alist-var)))

    (if (null old-elt)
	(progn
	  ;; insert new-elt in macro-alist-var and keep the list sorted
	  (define-key
	    keymap
	    (vector (viper-key-to-emacs-key (aref macro-name 0)))
	    'viper-exec-mapped-kbd-macro)
	  (setq lis (eval macro-alist-var))
	  (while (and lis (string< (viper-array-to-string (car (car lis)))
				   (viper-array-to-string macro-name)))
	    (setq lis2 (cons (car lis) lis2))
	    (setq lis (cdr lis)))

	  (setq lis2 (reverse lis2))
	  (set macro-alist-var (append lis2 (cons new-elt lis)))
	  (setq old-elt new-elt)))
    (setq old-sub-elt
	  (cond ((eq scope t) (viper-kbd-global-pair old-elt))
		((symbolp scope) (assoc scope (viper-kbd-mode-alist old-elt)))
		((stringp scope) (assoc scope (viper-kbd-buf-alist old-elt)))))
    (if old-sub-elt
	(setcdr old-sub-elt macro-body)
      (cond ((symbolp scope) (setcar (cdr (cdr old-elt))
				     (cons (cons scope macro-body)
					   (viper-kbd-mode-alist old-elt))))
	    ((stringp scope) (setcar (cdr old-elt)
				     (cons (cons scope macro-body)
					   (viper-kbd-buf-alist old-elt))))))
    ))



;; macro name must be a vector of viper-style keys
;; viper-unrecord-kbd-macro doesn't have scope. Macro definitions are inherited
;; from global -> major mode -> buffer
;; More specific definition overrides more general
;; Can't unrecord definition for more specific, if a more general definition is
;; in effect
(defun viper-unrecord-kbd-macro (macro-name state)
  "Delete macro MACRO-NAME from Viper STATE.
MACRO-NAME must be a vector of viper-style keys.  This command is used by Viper
internally, but the user can also use it in ~/.viper to delete pre-defined
macros supplied with Viper.  The best way to avoid mistakes in macro names to
be passed to this function is to use viper-describe-kbd-macros and copy the
name from there."
  (let* (state-name keymap
	 (macro-alist-var
	  (cond ((eq state 'vi-state)
		 (setq state-name "Vi state"
		       keymap viper-vi-kbd-map)
		 'viper-vi-kbd-macro-alist)
		((memq state '(insert-state replace-state))
		 (setq state-name "Insert state"
		       keymap viper-insert-kbd-map)
		 'viper-insert-kbd-macro-alist)
		(t
		 (setq state-name "Emacs state"
		       keymap viper-emacs-kbd-map)
		 'viper-emacs-kbd-macro-alist)
		))
	 buf-mapping mode-mapping global-mapping
	 macro-pair macro-entry)

    ;; Macro-name is usually a vector.  However, command history or macros
    ;; recorded in ~/.viper may appear as strings.  So, convert to vectors.
    (setq macro-name (viper-fixup-macro macro-name))
    (if (viper-char-array-p macro-name)
	(setq macro-name (viper-char-array-to-macro macro-name)))

    (setq macro-entry (assoc macro-name (eval macro-alist-var)))
    (if (= (length macro-name) 0)
	(error "Can't unmap an empty macro name"))
    (if (null macro-entry)
	(error "%S is not mapped to a macro for %s in `%s'"
	       (viper-display-macro macro-name)
	       state-name (buffer-name)))

    (setq buf-mapping (viper-kbd-buf-pair macro-entry)
	  mode-mapping (viper-kbd-mode-pair macro-entry)
	  global-mapping (viper-kbd-global-pair macro-entry))

    (cond ((and (cdr buf-mapping)
		(or (and (not (cdr mode-mapping)) (not (cdr global-mapping)))
		    (y-or-n-p
		     (format "Unmap %S for `%s' only? "
			     (viper-display-macro macro-name)
			     (buffer-name)))))
	   (setq macro-pair buf-mapping)
	   (message "%S is unmapped for %s in `%s'"
		    (viper-display-macro macro-name)
		    state-name (buffer-name)))
	  ((and (cdr mode-mapping)
		(or (not (cdr global-mapping))
		    (y-or-n-p
		     (format "Unmap %S for the major mode `%S' only? "
			     (viper-display-macro macro-name)
			     major-mode))))
	   (setq macro-pair mode-mapping)
	   (message "%S is unmapped for %s in %S"
		    (viper-display-macro macro-name) state-name major-mode))
	  ((cdr (setq macro-pair global-mapping))
	   (message
	    "Global mapping for %S in %s is removed"
	    (viper-display-macro macro-name) state-name))
	  (t (error "%S is not mapped to a macro for %s in `%s'"
		    (viper-display-macro macro-name)
		    state-name (buffer-name))))
    (setcdr macro-pair nil)
    (or (cdr buf-mapping)
	(cdr mode-mapping)
	(cdr global-mapping)
	(progn
	  (set macro-alist-var (delq macro-entry (eval macro-alist-var)))
	  (if (viper-can-release-key (aref macro-name 0)
				     (eval macro-alist-var))
	      (define-key
		keymap
		(vector (viper-key-to-emacs-key (aref macro-name 0)))
		nil))
	  ))
    ))

;; Check if MACRO-ALIST has an entry for a macro name starting with
;; CHAR.  If not, this indicates that the binding for this char
;; in viper-vi/insert-kbd-map can be released.
(defun viper-can-release-key (char macro-alist)
  (let ((lis macro-alist)
	(can-release t)
	macro-name)

    (while (and lis can-release)
      (setq macro-name (car (car lis)))
      (if (eq char (aref macro-name 0))
	  (setq can-release nil))
      (setq lis (cdr lis)))
    can-release))


(defun viper-exec-mapped-kbd-macro (count)
  "Dispatch kbd macro."
  (interactive "P")
  (let* ((macro-alist (cond ((eq viper-current-state 'vi-state)
			     viper-vi-kbd-macro-alist)
			    ((memq viper-current-state
				   '(insert-state replace-state))
			     viper-insert-kbd-macro-alist)
			    (t
			     viper-emacs-kbd-macro-alist)))
	(unmatched-suffix "")
	;; Macros and keys are executed with other macros turned off
	;; For macros, this is done to avoid macro recursion
	viper-vi-kbd-minor-mode viper-insert-kbd-minor-mode
	viper-emacs-kbd-minor-mode
	next-best-match keyseq event-seq
	macro-first-char macro-alist-elt macro-body
	command)

    (setq macro-first-char last-command-event
	  event-seq (viper-read-fast-keysequence macro-first-char macro-alist)
	  keyseq (viper-events-to-macro event-seq)
	  macro-alist-elt (assoc keyseq macro-alist)
	  next-best-match (viper-find-best-matching-macro macro-alist keyseq))

    (if (null macro-alist-elt)
	(setq macro-alist-elt (car next-best-match)
	      unmatched-suffix (viper-subseq event-seq (cdr next-best-match))))

    (cond ((null macro-alist-elt))
	  ((setq macro-body (viper-kbd-buf-definition macro-alist-elt)))
	  ((setq macro-body (viper-kbd-mode-definition macro-alist-elt)))
	  ((setq macro-body (viper-kbd-global-definition macro-alist-elt))))

    ;; when defining keyboard macro, don't use the macro mappings
    (if (and macro-body (not defining-kbd-macro))
	;; block cmd executed as part of a macro from entering command history
	(let ((command-history command-history))
	  (setq viper-this-kbd-macro (car macro-alist-elt))
	  (execute-kbd-macro (viper-macro-to-events macro-body) count)
	  (setq viper-this-kbd-macro nil
		viper-last-kbd-macro (car macro-alist-elt))
	  (viper-set-unread-command-events unmatched-suffix))
      ;; If not a macro, or the macro is suppressed while defining another
      ;; macro, put keyseq back on the event queue
      (viper-set-unread-command-events event-seq)
      ;; if the user typed arg, then use it if prefix arg is not set by
      ;; some other command (setting prefix arg can happen if we do, say,
      ;; 2dw and there is a macro starting with 2.  Then control will go to
      ;; this routine
      (or prefix-arg (setq  prefix-arg count))
      (setq command (key-binding (read-key-sequence nil)))
      (if (commandp command)
	  (command-execute command)
	(beep 1)))
    ))



;;; Displaying and completing macros

(defun viper-describe-kbd-macros ()
  "Show currently defined keyboard macros."
  (interactive)
  (with-output-to-temp-buffer " *viper-info*"
    (princ "Macros in Vi state:\n===================\n")
    (mapc 'viper-describe-one-macro viper-vi-kbd-macro-alist)
    (princ "\n\nMacros in Insert and Replace states:\n====================================\n")
    (mapc 'viper-describe-one-macro viper-insert-kbd-macro-alist)
    (princ "\n\nMacros in Emacs state:\n======================\n")
    (mapcar 'viper-describe-one-macro viper-emacs-kbd-macro-alist)
    ))

(defun viper-describe-one-macro (macro)
  (princ (format "\n  *** Mappings for %S:\n      ------------\n"
		 (viper-display-macro (car macro))))
  (princ "   ** Buffer-specific:")
  (if (viper-kbd-buf-alist macro)
      (mapc 'viper-describe-one-macro-elt (viper-kbd-buf-alist macro))
    (princ "  none\n"))
  (princ "\n   ** Mode-specific:")
  (if (viper-kbd-mode-alist macro)
      (mapc 'viper-describe-one-macro-elt (viper-kbd-mode-alist macro))
    (princ "  none\n"))
  (princ "\n   ** Global:")
  (if (viper-kbd-global-definition macro)
      (princ (format "\n           %S" (cdr (viper-kbd-global-pair macro))))
    (princ "  none"))
  (princ "\n"))

(defun viper-describe-one-macro-elt (elt)
  (let ((name (car elt))
	(defn (cdr elt)))
    (princ (format "\n       * %S:\n           %S\n" name defn))))



;; check if SEQ is a prefix of some car of an element in ALIST
(defun viper-keyseq-is-a-possible-macro (seq alist)
  (let ((converted-seq (viper-events-to-macro seq)))
    (eval (cons 'or
		(mapcar
		 (lambda (elt) (viper-prefix-subseq-p converted-seq elt))
		 (viper-this-buffer-macros alist))))))

;; whether SEQ1 is a prefix of SEQ2
(defun viper-prefix-subseq-p (seq1 seq2)
  (let ((len1 (length seq1))
	(len2 (length seq2)))
    (if (<= len1 len2)
	(equal seq1 (viper-subseq seq2 0 len1)))))

;; find the longest common prefix
(defun viper-common-seq-prefix (&rest seqs)
  (let* ((first (car seqs))
	 (rest (cdr seqs))
	 (pref [])
	 (idx 0)
	 len)
    (if (= (length seqs) 0)
	(setq len 0)
      (setq len (apply 'min (mapcar 'length seqs))))
    (while (< idx len)
      (if (eval (cons 'and
		      (mapcar (lambda (s) (equal (elt first idx) (elt s idx)))
			      rest)))
	  (setq pref (vconcat pref (vector (elt first idx)))))
      (setq idx (1+ idx)))
    pref))

;; get all sequences that match PREFIX from a given A-LIST
(defun viper-extract-matching-alist-members (pref alist)
  (delq nil (mapcar (lambda (elt) (if (viper-prefix-subseq-p pref elt) elt))
		    (viper-this-buffer-macros alist))))

(defun viper-do-sequence-completion (seq alist compl-message)
  (let* ((matches (viper-extract-matching-alist-members seq alist))
	 (new-seq (apply 'viper-common-seq-prefix matches))
	 )
    (cond ((and (equal seq new-seq) (= (length matches) 1))
	   (message "%s (Sole completion)" compl-message)
	   (sit-for 2))
	  ((null matches)
	   (message "%s (No match)" compl-message)
	   (sit-for 2)
	   (setq new-seq seq))
	  ((member seq matches)
	   (message "%s (Complete, but not unique)" compl-message)
	   (sit-for 2)
	   (viper-display-vector-completions matches))
	  ((equal seq new-seq)
	   (viper-display-vector-completions matches)))
    new-seq))


(defun viper-display-vector-completions (list)
  (with-output-to-temp-buffer "*Completions*"
    (display-completion-list
     (mapcar 'prin1-to-string
	     (mapcar 'viper-display-macro list)))))



;; alist is the alist of macros
;; str is the fast key sequence entered
;; returns: (matching-macro-def . unmatched-suffix-start-index)
(defun viper-find-best-matching-macro (alist str)
  (let ((lis alist)
	(def-len 0)
	(str-len (length str))
	match unmatched-start-idx found macro-def)
    (while (and (not found) lis)
      (setq macro-def (car lis)
	    def-len (length (car macro-def)))
      (if (and (>= str-len def-len)
	       (equal (car macro-def) (viper-subseq str 0 def-len)))
	  (if (or (viper-kbd-buf-definition macro-def)
		  (viper-kbd-mode-definition macro-def)
		  (viper-kbd-global-definition macro-def))
	      (setq found t))
	)
      (setq lis (cdr lis)))

    (if found
	(setq match macro-def
	      unmatched-start-idx def-len)
      (setq match nil
	    unmatched-start-idx 0))

    (cons match unmatched-start-idx)))



;; returns a list of names of macros defined for the current buffer
(defun viper-this-buffer-macros (macro-alist)
  (let (candidates)
    (setq candidates
	  (mapcar (lambda (elt)
		    (if (or (viper-kbd-buf-definition elt)
			    (viper-kbd-mode-definition elt)
			    (viper-kbd-global-definition elt))
			(car elt)))
		  macro-alist))
    (setq candidates (delq nil candidates))))


;; if seq of Viper key symbols (representing a macro) can be converted to a
;; string--do so.  Otherwise, do nothing.
(defun viper-display-macro (macro-name-or-body)
  (cond ((viper-char-symbol-sequence-p macro-name-or-body)
	 (mapconcat 'symbol-name macro-name-or-body ""))
	((viper-char-array-p macro-name-or-body)
	 (mapconcat 'char-to-string macro-name-or-body ""))
	(t macro-name-or-body)))

;; convert sequence of events (that came presumably from emacs kbd macro) into
;; Viper's macro, which is a vector of the form
;; [ desc desc ... ]
;; Each desc is either a symbol of (meta symb), (shift symb), etc.
;; Here we purge events that happen to be lists.  In most cases, these events
;; got into a macro definition unintentionally; say, when the user moves mouse
;; during a macro definition, then something like (switch-frame ...) might get
;; in.  Another reason for purging lists-events is that we can't store them in
;; textual form (say, in .emacs) and then read them back.
(defun viper-events-to-macro (event-seq)
  (vconcat (delq nil (mapcar (lambda (elt) (if (consp elt)
					       nil
					     (viper-event-key elt)))
			     event-seq))))

;; convert strings or arrays of characters to Viper macro form
(defun viper-char-array-to-macro (array)
  (let ((vec (vconcat array))
	macro)
    (if (featurep 'xemacs)
	(setq macro (mapcar 'character-to-event vec))
      (setq macro vec))
    (vconcat (mapcar 'viper-event-key macro))))

;; For macros bodies and names, goes over MACRO and checks if all members are
;; names of keys (actually, it only checks if they are symbols or lists
;; if a digit is found, it is converted into a symbol (e.g., 0 -> \0, etc).
;; If MACRO is not a list or vector -- doesn't change MACRO.
(defun viper-fixup-macro (macro)
  (let ((len (length macro))
	(idx 0)
	elt break)
    (if (or (vectorp macro) (listp macro))
	(while (and (< idx len) (not break))
	  (setq elt (elt macro idx))
	  (cond ((numberp elt)
		 ;; fix number
		 (if (and (<= 0 elt) (<= elt 9))
		     (cond ((arrayp macro)
			    (aset macro
				  idx
				  (intern (char-to-string (+ ?0 elt)))))
			   ((listp macro)
			    (setcar (nthcdr idx macro)
				    (intern (char-to-string (+ ?0 elt)))))
			   )))
		((listp elt)
		 (viper-fixup-macro elt))
		((symbolp elt) nil)
		(t (setq break t)))
	  (setq idx (1+ idx))))

      (if break
	  (error "Wrong type macro component, symbol-or-listp, %S" elt)
	macro)))

(defun viper-macro-to-events (macro-body)
  (vconcat (mapcar 'viper-key-to-emacs-key macro-body)))



;;; Reading fast key sequences

;; Assuming that CHAR was the first character in a fast succession of key
;; strokes, read the rest.  Return the vector of keys that was entered in
;; this fast succession of key strokes.
;; A fast keysequence is one that is terminated by a pause longer than
;; viper-fast-keyseq-timeout.
(defun viper-read-fast-keysequence (event macro-alist)
  (let ((lis (vector event))
	next-event)
    (while (and (viper-fast-keysequence-p)
           (viper-keyseq-is-a-possible-macro lis macro-alist))
      ;; Seems that viper-read-event is more robust here. We need to be able to
      ;; place these events on unread-command-events list. If we use
      ;; viper-read-key then events will be converted to keys, and sometimes
      ;; (e.g., (control \[)) those keys differ from the corresponding events.
      ;; So, do not use (setq next-event (viper-read-key))
      (setq next-event (viper-read-event))
      (or (viper-mouse-event-p next-event)
	  (setq lis (vconcat lis (vector next-event)))))
    lis))


;;; Keyboard macros in registers

;; sets register to last-kbd-macro carefully.
(defun viper-set-register-macro (reg)
  (if (get-register reg)
      (if (y-or-n-p "Register contains data.  Overwrite? ")
	  ()
	(error
	 "Macro not saved in register.  Can still be invoked via `C-x e'")))
  (set-register reg last-kbd-macro))

(defun viper-register-macro (count)
  "Keyboard macros in registers - a modified \@ command."
  (interactive "P")
  (let ((reg (downcase (read-char))))
    (cond ((or (and (<= ?a reg) (<= reg ?z)))
	   (setq viper-last-macro-reg reg)
	   (if defining-kbd-macro
	       (progn
		 (end-kbd-macro)
		 (viper-set-register-macro reg))
	     (execute-kbd-macro (get-register reg) count)))
	  ((or (= ?@ reg) (= ?\^j reg) (= ?\^m reg))
	   (if viper-last-macro-reg
	       nil
	       (error "No previous kbd macro"))
	   (execute-kbd-macro (get-register viper-last-macro-reg) count))
	  ((= ?\# reg)
	   (start-kbd-macro count))
	  ((= ?! reg)
	   (setq reg (downcase (read-char)))
	   (if (or (and (<= ?a reg) (<= reg ?z)))
	       (progn
	       (setq viper-last-macro-reg reg)
	       (viper-set-register-macro reg))))
	  (t
	   (error "`%c': Unknown register" reg)))))


(defun viper-global-execute ()
  "Call last keyboard macro for each line in the region."
  (if (> (point) (mark t)) (exchange-point-and-mark))
  (beginning-of-line)
  (call-last-kbd-macro)
  (while (< (point) (mark t))
    (forward-line 1)
    (beginning-of-line)
    (call-last-kbd-macro)))


;;; viper-macs.el ends here
