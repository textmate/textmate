;;; kmacro.el --- enhanced keyboard macros

;; Copyright (C) 2002-2012  Free Software Foundation, Inc.

;; Author: Kim F. Storm <storm@cua.dk>
;; Keywords: keyboard convenience

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

;; The kmacro package provides the user interface to emacs' basic
;; keyboard macro functionality.  With kmacro, two function keys are
;; dedicated to keyboard macros, by default F3 and F4.

;; Note: The traditional bindings C-x (, C-x ), and C-x e are still
;; supported, but for some users these bindings are too hard to type
;; to be really useful for doing small repeated tasks.

;; To start defining a keyboard macro, use F3.  To end the macro,
;; use F4, and to call the macro also use F4.  This makes it very
;; easy to repeat a macro immediately after defining it.
;;
;; You can call the macro repeatedly by pressing F4 multiple times, or
;; you can give a numeric prefix argument specifying the number of
;; times to repeat the macro.  Macro execution automatically
;; terminates when point reaches the end of the buffer or if an error
;; is signaled by ringing the bell.

;; When you define a macro with F3/F4, it is automatically added to
;; the head of the "keyboard macro ring", and F4 actually executes the
;; first element of the macro ring.
;;
;; Note: an empty macro is never added to the macro ring.
;;
;; You can execute the second element on the macro ring with C-u F4 or
;; C-x C-k C-l, you can use C-x C-k C-p and C-x C-k C-n to cycle
;; through the macro ring, and you can swap the first and second
;; elements with C-x C-k C-t.  To delete the first element in the
;; macro ring, use C-x C-k C-d.
;;
;; You can also use C-x C-k C-s to start a macro, and C-x C-k C-k to
;; end it; then use C-k to execute it immediately, or C-x C-k C-k to
;; execute it later.
;;
;; In general, immediately after using C-x C-k followed by one of C-k,
;; C-l, C-p, or C-n, you can further cycle the macro ring using C-p or
;; C-n, execute the first or second macro using C-k or C-l, delete
;; the head macro with C-d, or edit the current macro with C-e without
;; repeating the C-x C-k prefix.

;; If you enter F3 while defining the macro, the numeric value of
;; `kmacro-counter' is inserted using the `kmacro-counter-format', and
;; `kmacro-counter' is incremented by 1 (or the numeric prefix value
;; of F3).
;;
;; The initial value of `kmacro-counter' is 0, or the numeric prefix
;; value given to F3 when starting the macro.
;;
;; Now, each time you call the macro using F4, the current
;; value of `kmacro-counter' is inserted and incremented, making it
;; easy to insert incremental numbers in the buffer.
;;
;; Example:
;;
;; The following sequence: M-5 F3 x M-2 F3 y F4 F4 F4 F4
;; inserts the following string:  x5yx7yx9yx11y

;; A macro can also be called using a mouse click, default S-mouse-3.
;; This calls the macro at the point where you click the mouse.

;; You can edit the last macro using C-x C-k C-e.

;; You can append to the last macro using C-u F3.

;; You can set the macro counter using C-x C-k C-c, add to it using C-x C-k C-a,
;; and you can set the macro counter format with C-x C-k C-f.

;; The following key bindings are performed:
;;
;;           Normal                         While defining macro
;;           ---------------------------    ------------------------------
;;  f3       Define macro                   Insert current counter value
;;           Prefix arg specifies initial   and increase counter by prefix
;;           counter value (default 0)      (default increment: 1)
;;
;;  C-u f3   APPENDs to last macro
;;
;;  f4       Call last macro                End macro
;;           Prefix arg specifies number
;;           of times to execute macro.
;;
;;  C-u f4   Swap last and head of macro ring.
;;
;;  S-mouse-3  Set point at click and       End macro and execute macro at
;;             execute last macro.          click.

;;; Code:

;; Customization:

(defgroup kmacro nil
  "Simplified keyboard macro user interface."
  :group 'keyboard
  :group 'convenience
  :version "22.1"
  :link '(emacs-commentary-link :tag "Commentary" "kmacro.el")
  :link '(emacs-library-link :tag "Lisp File" "kmacro.el"))

(defcustom kmacro-call-mouse-event 'S-mouse-3
  "The mouse event used by kmacro to call a macro.
Set to nil if no mouse binding is desired."
  :type 'symbol
  :group 'kmacro)

(defcustom kmacro-ring-max 8
  "Maximum number of keyboard macros to save in macro ring."
  :type 'integer
  :group 'kmacro)


(defcustom kmacro-execute-before-append t
  "Controls whether appending to a macro starts by executing the macro.
If non-nil, using a single \\[universal-argument] prefix executes the macro
before appending, while more than one \\[universal-argument] prefix does not
execute the macro.
Otherwise, a single \\[universal-argument] prefix does not execute the
macro, while more than one \\[universal-argument] prefix causes the
macro to be executed before appending to it."
  :type 'boolean
  :group 'kmacro)


(defcustom kmacro-repeat-no-prefix t
  "Allow repeating certain macro commands without entering the C-x C-k prefix."
  :type 'boolean
  :group 'kmacro)

(defcustom kmacro-call-repeat-key t
  "Allow repeating macro call using last key or a specific key."
  :type '(choice (const :tag "Disabled" nil)
		 (const :tag "Last key" t)
		 (character :tag "Character" :value ?e)
		 (symbol :tag "Key symbol" :value RET))
  :group 'kmacro)

(defcustom kmacro-call-repeat-with-arg nil
  "Repeat macro call with original arg when non-nil; repeat once if nil."
  :type 'boolean
  :group 'kmacro)

(defcustom kmacro-step-edit-mini-window-height 0.75
  "Override `max-mini-window-height' when step edit keyboard macro."
  :type 'number
  :group 'kmacro)

;; Keymap

(defvar kmacro-keymap
  (let ((map (make-sparse-keymap)))
    ;; Start, end, execute macros
    (define-key map "s"    'kmacro-start-macro)
    (define-key map "\C-s" 'kmacro-start-macro)
    (define-key map "\C-k" 'kmacro-end-or-call-macro-repeat)
    (define-key map "r"    'apply-macro-to-region-lines)
    (define-key map "q"    'kbd-macro-query)  ;; Like C-x q

    ;; macro ring
    (define-key map "\C-n" 'kmacro-cycle-ring-next)
    (define-key map "\C-p" 'kmacro-cycle-ring-previous)
    (define-key map "\C-v" 'kmacro-view-macro-repeat)
    (define-key map "\C-d" 'kmacro-delete-ring-head)
    (define-key map "\C-t" 'kmacro-swap-ring)
    (define-key map "\C-l" 'kmacro-call-ring-2nd-repeat)

    ;; macro counter
    (define-key map "\C-f" 'kmacro-set-format)
    (define-key map "\C-c" 'kmacro-set-counter)
    (define-key map "\C-i" 'kmacro-insert-counter)
    (define-key map "\C-a" 'kmacro-add-counter)

    ;; macro editing
    (define-key map "\C-e" 'kmacro-edit-macro-repeat)
    (define-key map "\r"   'kmacro-edit-macro)
    (define-key map "e"    'edit-kbd-macro)
    (define-key map "l"    'kmacro-edit-lossage)
    (define-key map " "    'kmacro-step-edit-macro)

    ;; naming and binding
    (define-key map "b"    'kmacro-bind-to-key)
    (define-key map "n"    'kmacro-name-last-macro)
    map)
  "Keymap for keyboard macro commands.")
(defalias 'kmacro-keymap kmacro-keymap)

;;; Provide some binding for startup:
;;;###autoload (global-set-key "\C-x(" 'kmacro-start-macro)
;;;###autoload (global-set-key "\C-x)" 'kmacro-end-macro)
;;;###autoload (global-set-key "\C-xe" 'kmacro-end-and-call-macro)
;;;###autoload (global-set-key [f3] 'kmacro-start-macro-or-insert-counter)
;;;###autoload (global-set-key [f4] 'kmacro-end-or-call-macro)
;;;###autoload (global-set-key "\C-x\C-k" 'kmacro-keymap)
;;;###autoload (autoload 'kmacro-keymap "kmacro" "Keymap for keyboard macro commands." t 'keymap)

(if kmacro-call-mouse-event
  (global-set-key (vector kmacro-call-mouse-event) 'kmacro-end-call-mouse))


;;; Called from keyboard-quit

(defun kmacro-keyboard-quit ()
  (or (not defining-kbd-macro)
      (eq defining-kbd-macro 'append)
      (kmacro-ring-empty-p)
      (kmacro-pop-ring)))


;;; Keyboard macro counter

(defvar kmacro-counter 0
  "*Current keyboard macro counter.")

(defvar kmacro-default-counter-format "%d")

(defvar kmacro-counter-format "%d"
  "*Current keyboard macro counter format.")

(defvar kmacro-counter-format-start kmacro-counter-format
  "Macro format at start of macro execution.")

(defvar kmacro-counter-value-start kmacro-counter
  "Macro counter at start of macro execution.")

(defvar kmacro-last-counter 0
  "Last counter inserted by key macro.")

(defvar kmacro-initial-counter-value nil
  "Initial counter value for the next keyboard macro to be defined.")


(defun kmacro-insert-counter (arg)
  "Insert macro counter, then increment it by ARG.
Interactively, ARG defaults to 1.  With \\[universal-argument], insert
previous `kmacro-counter', and do not modify counter."
  (interactive "P")
  (if kmacro-initial-counter-value
      (setq kmacro-counter kmacro-initial-counter-value
	    kmacro-initial-counter-value nil))
  (if (and arg (listp arg))
      (insert (format kmacro-counter-format kmacro-last-counter))
    (insert (format kmacro-counter-format kmacro-counter))
    (kmacro-add-counter (prefix-numeric-value arg))))


(defun kmacro-set-format (format)
  "Set macro counter FORMAT."
  (interactive "sMacro Counter Format: ")
  (setq kmacro-counter-format
	(if (equal format "") "%d" format))
  ;; redefine initial macro counter if we are not executing a macro.
  (if (not (or defining-kbd-macro executing-kbd-macro))
      (setq kmacro-default-counter-format kmacro-counter-format)))


(defun kmacro-display-counter (&optional value)
  "Display current counter value."
  (unless value (setq value kmacro-counter))
  (message "New macro counter value: %s (%d)" (format kmacro-counter-format value) value))


(defun kmacro-set-counter (arg)
  "Set `kmacro-counter' to ARG or prompt if missing.
With \\[universal-argument] prefix, reset counter to its value prior to this iteration of the macro."
  (interactive "NMacro counter value: ")
  (if (not (or defining-kbd-macro executing-kbd-macro))
      (kmacro-display-counter (setq kmacro-initial-counter-value arg))
    (setq kmacro-last-counter kmacro-counter
	  kmacro-counter (if (and current-prefix-arg (listp current-prefix-arg))
			     kmacro-counter-value-start
			   arg))
    (unless executing-kbd-macro
      (kmacro-display-counter))))


(defun kmacro-add-counter (arg)
  "Add numeric prefix arg (prompt if missing) to macro counter.
With \\[universal-argument], restore previous counter value."
  (interactive "NAdd to macro counter: ")
  (if kmacro-initial-counter-value
      (setq kmacro-counter kmacro-initial-counter-value
	    kmacro-initial-counter-value nil))
  (let ((last kmacro-last-counter))
    (setq kmacro-last-counter kmacro-counter
	  kmacro-counter (if (and current-prefix-arg (listp current-prefix-arg))
			     last
			   kmacro-counter (+ kmacro-counter arg))))
  (unless executing-kbd-macro
    (kmacro-display-counter)))


(defun kmacro-loop-setup-function ()
  "Function called prior to each iteration of macro."
  ;; Restore macro counter format to initial format, so it is ok to change
  ;; counter format in the macro without restoring it.
  (setq kmacro-counter-format kmacro-counter-format-start)
  ;; Save initial counter value so we can restore it with C-u kmacro-set-counter.
  (setq kmacro-counter-value-start kmacro-counter)
  ;; Return non-nil to continue execution.
  t)


;;; Keyboard macro ring

(defvar kmacro-ring nil
  "The keyboard macro ring.
Each element is a list (MACRO COUNTER FORMAT).  Actually, the head of
the macro ring (when defining or executing) is not stored in the ring;
instead it is available in the variables `last-kbd-macro', `kmacro-counter',
and `kmacro-counter-format'.")

;; Remember what we are currently looking at with kmacro-view-macro.

(defvar kmacro-view-last-item nil)
(defvar kmacro-view-item-no 0)


(defun kmacro-ring-head ()
  "Return pseudo head element in macro ring."
  (and last-kbd-macro
       (list last-kbd-macro kmacro-counter kmacro-counter-format-start)))


(defun kmacro-push-ring (&optional elt)
  "Push ELT or current macro onto `kmacro-ring'."
  (when (setq elt (or elt (kmacro-ring-head)))
    (let ((history-delete-duplicates nil))
      (add-to-history 'kmacro-ring elt kmacro-ring-max))))


(defun kmacro-split-ring-element (elt)
  (setq last-kbd-macro (car elt)
	kmacro-counter (nth 1 elt)
	kmacro-counter-format-start (nth 2 elt)))


(defun kmacro-pop-ring1 (&optional raw)
  "Pop head element off macro ring (no check).
Non-nil arg RAW means just return raw first element."
  (prog1 (car kmacro-ring)
    (unless raw
      (kmacro-split-ring-element (car kmacro-ring)))
    (setq kmacro-ring (cdr kmacro-ring))))


(defun kmacro-pop-ring (&optional raw)
  "Pop head element off macro ring.
Non-nil arg RAW means just return raw first element."
  (unless (kmacro-ring-empty-p)
    (kmacro-pop-ring1 raw)))


(defun kmacro-ring-empty-p (&optional none)
  "Tell user and return t if `last-kbd-macro' is nil or `kmacro-ring' is empty.
Check only `last-kbd-macro' if optional arg NONE is non-nil."
  (while (and (null last-kbd-macro) kmacro-ring)
    (kmacro-pop-ring1))
  (cond
   ((null last-kbd-macro)
    (message "No keyboard macro defined.")
    t)
   ((and (null none) (null kmacro-ring))
    (message "Only one keyboard macro defined.")
    t)
   (t nil)))


(defun kmacro-display (macro &optional trunc descr empty)
  "Display a keyboard MACRO.
Optional arg TRUNC non-nil specifies to limit width of macro to 60 chars.
Optional arg DESCR is descriptive text for macro; default is \"Macro:\".
Optional arg EMPTY is message to print if no macros are defined."
  (if macro
      (let* ((x 60)
	     (m (format-kbd-macro macro))
	     (l (length m))
	     (z (and trunc (> l x))))
	(message "%s%s: %s%s" (or descr "Macro")
		 (if (= kmacro-counter 0) ""
		   (format " [%s]"
			   (format kmacro-counter-format-start kmacro-counter)))
		 (if z (substring m 0 (1- x)) m) (if z "..." "")))
    (message "%s" (or empty "No keyboard macros defined"))))


(defun kmacro-repeat-on-last-key (keys)
  "Process kmacro commands keys immediately after cycling the ring."
  (setq keys (vconcat keys))
  (let ((n (1- (length keys)))
	cmd done repeat)
    (while (and last-kbd-macro
		(not done)
		(aset keys n (read-event))
		(setq cmd (key-binding keys t))
		(setq repeat (get cmd 'kmacro-repeat)))
      (clear-this-command-keys t)
      (cond
       ((eq repeat 'ring)
	(if kmacro-ring
	    (let ((kmacro-repeat-no-prefix nil))
	      (funcall cmd nil))
	  (kmacro-display last-kbd-macro t)))
       ((eq repeat 'head)
	(let ((kmacro-repeat-no-prefix nil))
	  (funcall cmd nil)))
       ((eq repeat 'stop)
	(funcall cmd nil)
	(setq done t)))
      (setq last-input-event nil)))
  (when last-input-event
    (clear-this-command-keys t)
    (setq unread-command-events (list last-input-event))))


(defun kmacro-get-repeat-prefix ()
  (let (keys)
    (and kmacro-repeat-no-prefix
	 (setq keys (this-single-command-keys))
	 (> (length keys) 1)
	 keys)))


;;;###autoload
(defun kmacro-exec-ring-item (item arg)
  "Execute item ITEM from the macro ring."
  ;; Use counter and format specific to the macro on the ring!
  (let ((kmacro-counter (nth 1 item))
	(kmacro-counter-format-start (nth 2 item)))
    (execute-kbd-macro (car item) arg #'kmacro-loop-setup-function)
    (setcar (cdr item) kmacro-counter)))


(defun kmacro-call-ring-2nd (arg)
  "Execute second keyboard macro in macro ring."
  (interactive "P")
  (unless (kmacro-ring-empty-p)
    (kmacro-exec-ring-item (car kmacro-ring) arg)))


(defun kmacro-call-ring-2nd-repeat (arg)
  "Execute second keyboard macro in macro ring.
This is like `kmacro-call-ring-2nd', but allows repeating macro commands
without repeating the prefix."
  (interactive "P")
  (let ((keys (kmacro-get-repeat-prefix)))
    (kmacro-call-ring-2nd arg)
    (if (and kmacro-ring keys)
	(kmacro-repeat-on-last-key keys))))

(put 'kmacro-call-ring-2nd-repeat 'kmacro-repeat 'head)


(defun kmacro-view-ring-2nd ()
  "Display the current head of the keyboard macro ring."
  (interactive)
  (unless (kmacro-ring-empty-p)
    (kmacro-display (car (car kmacro-ring)) "2nd macro")))


(defun kmacro-cycle-ring-next (&optional _arg)
  "Move to next keyboard macro in keyboard macro ring.
Displays the selected macro in the echo area."
  (interactive)
  (unless (kmacro-ring-empty-p)
    (kmacro-push-ring)
    (let* ((keys (kmacro-get-repeat-prefix))
	   (len (length kmacro-ring))
	   (tail (nthcdr (- len 2) kmacro-ring))
	   (elt (car (cdr tail))))
      (setcdr tail nil)
      (kmacro-split-ring-element elt)
      (kmacro-display last-kbd-macro t)
      (if keys
	  (kmacro-repeat-on-last-key keys)))))

(put 'kmacro-cycle-ring-next 'kmacro-repeat 'ring)


(defun kmacro-cycle-ring-previous (&optional _arg)
  "Move to previous keyboard macro in keyboard macro ring.
Displays the selected macro in the echo area."
  (interactive)
  (unless (kmacro-ring-empty-p)
    (let ((keys (kmacro-get-repeat-prefix))
	  (cur (kmacro-ring-head)))
      (kmacro-pop-ring1)
      (if kmacro-ring
	  (nconc kmacro-ring (list cur))
	(setq kmacro-ring (list cur)))
      (kmacro-display last-kbd-macro t)
      (if keys
	  (kmacro-repeat-on-last-key keys)))))

(put 'kmacro-cycle-ring-previous 'kmacro-repeat 'ring)


(defun kmacro-swap-ring ()
  "Swap first two elements on keyboard macro ring."
  (interactive)
  (unless (kmacro-ring-empty-p)
    (let ((cur (kmacro-ring-head)))
      (kmacro-pop-ring1)
      (kmacro-push-ring cur))
    (kmacro-display last-kbd-macro t)))


(defun kmacro-delete-ring-head (&optional _arg)
  "Delete current macro from keyboard macro ring."
  (interactive)
  (unless (kmacro-ring-empty-p t)
    (if (null kmacro-ring)
	(setq last-kbd-macro nil)
      (kmacro-pop-ring))
    (kmacro-display last-kbd-macro t nil "Keyboard macro ring is now empty.")))

(put 'kmacro-delete-ring-head 'kmacro-repeat 'head)

;;; Traditional bindings:


;;;###autoload
(defun kmacro-start-macro (arg)
  "Record subsequent keyboard input, defining a keyboard macro.
The commands are recorded even as they are executed.
Use \\[kmacro-end-macro] to finish recording and make the macro available.
Use \\[kmacro-end-and-call-macro] to execute the macro.

Non-nil arg (prefix arg) means append to last macro defined.

With \\[universal-argument] prefix, append to last keyboard macro
defined.  Depending on `kmacro-execute-before-append', this may begin
by re-executing the last macro as if you typed it again.

Otherwise, it sets `kmacro-counter' to ARG or 0 if missing before
defining the macro.

Use \\[kmacro-insert-counter] to insert (and increment) the macro counter.
The counter value can be set or modified via \\[kmacro-set-counter] and \\[kmacro-add-counter].
The format of the counter can be modified via \\[kmacro-set-format].

Use \\[kmacro-name-last-macro] to give it a permanent name.
Use \\[kmacro-bind-to-key] to bind it to a key sequence."
  (interactive "P")
  (if (or defining-kbd-macro executing-kbd-macro)
      (message "Already defining keyboard macro.")
    (let ((append (and arg (listp arg))))
      (unless append
	(if last-kbd-macro
	    (kmacro-push-ring
	     (list last-kbd-macro kmacro-counter kmacro-counter-format-start)))
	(setq kmacro-counter (or (if arg (prefix-numeric-value arg))
				 kmacro-initial-counter-value
				 0)
	      kmacro-initial-counter-value nil
	      kmacro-counter-value-start kmacro-counter
	      kmacro-last-counter kmacro-counter
	      kmacro-counter-format kmacro-default-counter-format
	      kmacro-counter-format-start kmacro-default-counter-format))

      (start-kbd-macro append
		       (and append
			    (if kmacro-execute-before-append
				(> (car arg) 4)
			      (= (car arg) 4))))
      (if (and defining-kbd-macro append)
	  (setq defining-kbd-macro 'append)))))


;;;###autoload
(defun kmacro-end-macro (arg)
  "Finish defining a keyboard macro.
The definition was started by \\[kmacro-start-macro].
The macro is now available for use via \\[kmacro-call-macro],
or it can be given a name with \\[kmacro-name-last-macro] and then invoked
under that name.

With numeric arg, repeat macro now that many times,
counting the definition just completed as the first repetition.
An argument of zero means repeat until error."
  (interactive "P")
   ;; Isearch may push the kmacro-end-macro key sequence onto the macro.
   ;; Just ignore it when executing the macro.
  (unless executing-kbd-macro
    (end-kbd-macro arg #'kmacro-loop-setup-function)
    (when (and last-kbd-macro (= (length last-kbd-macro) 0))
      (setq last-kbd-macro nil)
      (message "Ignore empty macro")
      ;; Don't call `kmacro-ring-empty-p' to avoid its messages.
      (while (and (null last-kbd-macro) kmacro-ring)
	(kmacro-pop-ring1)))))


;;;###autoload
(defun kmacro-call-macro (arg &optional no-repeat end-macro)
  "Call the last keyboard macro that you defined with \\[kmacro-start-macro].
A prefix argument serves as a repeat count.  Zero means repeat until error.

When you call the macro, you can call the macro again by repeating
just the last key in the key sequence that you used to call this
command.  See `kmacro-call-repeat-key' and `kmacro-call-repeat-with-arg'
for details on how to adjust or disable this behavior.

To make a macro permanent so you can call it even after defining
others, use \\[kmacro-name-last-macro]."
  (interactive "p")
  (let ((repeat-key (and (null no-repeat)
			 (> (length (this-single-command-keys)) 1)
			 last-input-event))
	repeat-key-str)
    (if end-macro
	(kmacro-end-macro arg)
      (call-last-kbd-macro arg #'kmacro-loop-setup-function))
    (when (consp arg)
      (setq arg (car arg)))
    (when (and (or (null arg) (> arg 0))
	       (setq repeat-key
		     (if (eq kmacro-call-repeat-key t)
			 repeat-key
		       kmacro-call-repeat-key)))
      (setq repeat-key-str (format-kbd-macro (vector repeat-key) nil))
      (while repeat-key
	;; Issue a hint to the user, if the echo area isn't in use.
	(unless (current-message)
	  (message "(Type %s to repeat macro%s)"
		   repeat-key-str
		   (if (and kmacro-call-repeat-with-arg
			    arg (> arg 1))
		       (format " %d times" arg) "")))
	(if (equal repeat-key (read-event))
	    (progn
	      (clear-this-command-keys t)
	      (call-last-kbd-macro (and kmacro-call-repeat-with-arg arg)
				   #'kmacro-loop-setup-function)
	      (setq last-input-event nil))
	  (setq repeat-key nil)))
      (when last-input-event
	(clear-this-command-keys t)
	(setq unread-command-events (list last-input-event))))))


;;; Combined function key bindings:

;;;###autoload
(defun kmacro-start-macro-or-insert-counter (arg)
  "Record subsequent keyboard input, defining a keyboard macro.
The commands are recorded even as they are executed.

Sets the `kmacro-counter' to ARG (or 0 if no prefix arg) before defining the
macro.

With \\[universal-argument], appends to current keyboard macro (keeping
the current value of `kmacro-counter').

When defining/executing macro, inserts macro counter and increments
the counter with ARG or 1 if missing.  With \\[universal-argument],
inserts previous `kmacro-counter' (but do not modify counter).

The macro counter can be modified via \\[kmacro-set-counter] and \\[kmacro-add-counter].
The format of the counter can be modified via \\[kmacro-set-format]."
  (interactive "P")
  (if (or defining-kbd-macro executing-kbd-macro)
      (kmacro-insert-counter arg)
    (kmacro-start-macro arg)))


;;;###autoload
(defun kmacro-end-or-call-macro (arg &optional no-repeat)
  "End kbd macro if currently being defined; else call last kbd macro.
With numeric prefix ARG, repeat macro that many times.
With \\[universal-argument], call second macro in macro ring."
  (interactive "P")
  (cond
   (defining-kbd-macro
     (if kmacro-call-repeat-key
	 (kmacro-call-macro arg no-repeat t)
       (kmacro-end-macro arg)))
   ((and (eq this-command 'kmacro-view-macro)  ;; We are in repeat mode!
	 kmacro-view-last-item)
    (kmacro-exec-ring-item (car kmacro-view-last-item) arg))
   ((and arg (listp arg))
    (kmacro-call-ring-2nd 1))
   (t
    (kmacro-call-macro arg no-repeat))))


(defun kmacro-end-or-call-macro-repeat (arg)
  "As `kmacro-end-or-call-macro' but allows repeat without repeating prefix."
  (interactive "P")
  (let ((keys (kmacro-get-repeat-prefix)))
    (kmacro-end-or-call-macro arg t)
    (if keys
	(kmacro-repeat-on-last-key keys))))

(put 'kmacro-end-or-call-macro-repeat 'kmacro-repeat 'head)


;;;###autoload
(defun kmacro-end-and-call-macro (arg &optional no-repeat)
  "Call last keyboard macro, ending it first if currently being defined.
With numeric prefix ARG, repeat macro that many times.
Zero argument means repeat until there is an error.

To give a macro a permanent name, so you can call it
even after defining other macros, use \\[kmacro-name-last-macro]."
  (interactive "P")
  (if defining-kbd-macro
      (kmacro-end-macro nil))
  (kmacro-call-macro arg no-repeat))


;;;###autoload
(defun kmacro-end-call-mouse (event)
  "Move point to the position clicked with the mouse and call last kbd macro.
If kbd macro currently being defined end it before activating it."
  (interactive "e")
  (when defining-kbd-macro
    (end-kbd-macro))
  (mouse-set-point event)
  (kmacro-call-macro nil t))


;;; Misc. commands

;; An idea for macro bindings:
;; Create a separate keymap installed as a minor-mode keymap (e.g. in
;; the emulation-mode-map-alists) in which macro bindings are made
;; independent of any other bindings.  When first binding is made,
;; the keymap is created, installed, and enabled.  Key seq. C-x C-k +
;; can then be used to toggle the use of this keymap on and off.
;; This means that it would be safe(r) to bind ordinary keys like
;; letters and digits, provided that we inhibit the keymap while
;; executing the macro later on (but that's controversial...)

(defun kmacro-lambda-form (mac &optional counter format)
  "Create lambda form for macro bound to symbol or key."
  (if counter
      (setq mac (list mac counter format)))
  `(lambda (&optional arg)
     "Keyboard macro."
     (interactive "p")
     (kmacro-exec-ring-item ',mac arg)))

(defun kmacro-extract-lambda (mac)
  "Extract kmacro from a kmacro lambda form."
  (and (consp mac)
       (eq (car mac) 'lambda)
       (setq mac (assoc 'kmacro-exec-ring-item mac))
       (consp (cdr mac))
       (consp (car (cdr mac)))
       (consp (cdr (car (cdr mac))))
       (setq mac (car (cdr (car (cdr mac)))))
       (listp mac)
       (= (length mac) 3)
       (arrayp (car mac))
       mac))


(defun kmacro-bind-to-key (_arg)
  "When not defining or executing a macro, offer to bind last macro to a key.
The key sequences [C-x C-k 0] through [C-x C-k 9] and [C-x C-k A]
through [C-x C-k Z] are reserved for user bindings, and to bind to
one of these sequences, just enter the digit or letter, rather than
the whole sequence.

You can bind to any valid key sequence, but if you try to bind to
a key with an existing command binding, you will be asked for
confirmation whether to replace that binding.  Note that the
binding is made in the `global-map' keymap, so the macro binding
may be shaded by a local key binding."
  (interactive "p")
  (if (or defining-kbd-macro executing-kbd-macro)
      (if defining-kbd-macro
	  (message "Cannot save macro while defining it."))
    (unless last-kbd-macro
      (error "No keyboard macro defined"))
    (let ((key-seq (read-key-sequence "Bind last macro to key: "))
	  ok cmd)
      (when (= (length key-seq) 1)
	(let ((ch (aref key-seq 0)))
	  (if (and (integerp ch)
		   (or (and (>= ch ?0) (<= ch ?9))
		       (and (>= ch ?A) (<= ch ?Z))))
	      (setq key-seq (concat "\C-x\C-k" key-seq)
		    ok t))))
      (when (and (not (equal key-seq ""))
		 (or ok
		     (not (setq cmd (key-binding key-seq)))
		     (stringp cmd)
		     (vectorp cmd)
		     (yes-or-no-p (format "%s runs command %S.  Bind anyway? "
					  (format-kbd-macro key-seq)
					  cmd))))
	(define-key global-map key-seq
	  (kmacro-lambda-form (kmacro-ring-head)))
	(message "Keyboard macro bound to %s" (format-kbd-macro key-seq))))))


(defun kmacro-name-last-macro (symbol)
  "Assign a name to the last keyboard macro defined.
Argument SYMBOL is the name to define.
The symbol's function definition becomes the keyboard macro string.
Such a \"function\" cannot be called from Lisp, but it is a valid editor command."
  (interactive "SName for last kbd macro: ")
  (or last-kbd-macro
      (error "No keyboard macro defined"))
  (and (fboundp symbol)
       (not (get symbol 'kmacro))
       (not (stringp (symbol-function symbol)))
       (not (vectorp (symbol-function symbol)))
       (error "Function %s is already defined and not a keyboard macro"
	      symbol))
  (if (string-equal symbol "")
      (error "No command name given"))
  (fset symbol (kmacro-lambda-form (kmacro-ring-head)))
  (put symbol 'kmacro t))


(defun kmacro-view-macro (&optional _arg)
  "Display the last keyboard macro.
If repeated, it shows previous elements in the macro ring."
  (interactive)
  (cond
   ((or (kmacro-ring-empty-p)
	(not (eq last-command 'kmacro-view-macro)))
    (setq kmacro-view-last-item nil))
   ((null kmacro-view-last-item)
    (setq kmacro-view-last-item kmacro-ring
	  kmacro-view-item-no 2))
   ((consp kmacro-view-last-item)
    (setq kmacro-view-last-item (cdr kmacro-view-last-item)
	  kmacro-view-item-no (1+ kmacro-view-item-no)))
   (t
    (setq kmacro-view-last-item nil)))
  (setq this-command 'kmacro-view-macro
	last-command this-command) ;; in case we repeat
  (kmacro-display (if kmacro-view-last-item
		      (car (car kmacro-view-last-item))
		    last-kbd-macro)
		  nil
		  (if kmacro-view-last-item
		      (concat (cond ((= kmacro-view-item-no 2) "2nd")
				    ((= kmacro-view-item-no 3) "3nd")
				    (t (format "%dth" kmacro-view-item-no)))
			      " previous macro")
		    "Last macro")))

(defun kmacro-view-macro-repeat (&optional arg)
  "Display the last keyboard macro.
If repeated, it shows previous elements in the macro ring.
To execute the displayed macro ring item without changing the macro ring,
just enter C-k.
This is like `kmacro-view-macro', but allows repeating macro commands
without repeating the prefix."
  (interactive)
  (let ((keys (kmacro-get-repeat-prefix)))
    (kmacro-view-macro arg)
    (if (and last-kbd-macro keys)
	(kmacro-repeat-on-last-key keys))))

(put 'kmacro-view-macro-repeat 'kmacro-repeat 'ring)


(defun kmacro-edit-macro-repeat (&optional arg)
  "Edit last keyboard macro."
  (interactive "P")
  (edit-kbd-macro "\r" arg))

(put 'kmacro-edit-macro-repeat 'kmacro-repeat 'stop)


(defun kmacro-edit-macro (&optional arg)
  "As edit last keyboard macro, but without kmacro-repeat property."
  (interactive "P")
  (edit-kbd-macro "\r" arg))


(defun kmacro-edit-lossage ()
  "Edit most recent 300 keystrokes as a keyboard macro."
  (interactive)
  (kmacro-push-ring)
  (edit-kbd-macro "\C-hl"))


;;; Single-step editing of keyboard macros

(defvar kmacro-step-edit-active)  	 ;; step-editing active
(defvar kmacro-step-edit-new-macro)  	 ;; storage for new macro
(defvar kmacro-step-edit-inserting)  	 ;; inserting into macro
(defvar kmacro-step-edit-appending)  	 ;; append to end of macro
(defvar kmacro-step-edit-replace)    	 ;; replace orig macro when done
(defvar kmacro-step-edit-prefix-index)   ;; index of first prefix arg key
(defvar kmacro-step-edit-key-index)      ;; index of current key
(defvar kmacro-step-edit-action)     	 ;; automatic action on next pre-command hook
(defvar kmacro-step-edit-help)     	 ;; kmacro step edit help enabled
(defvar kmacro-step-edit-num-input-keys) ;; to ignore duplicate pre-command hook

(defvar kmacro-step-edit-map
  (let ((map (make-sparse-keymap)))
    ;; query-replace-map answers include: `act', `skip', `act-and-show',
    ;; `exit', `act-and-exit', `edit', `delete-and-edit', `recenter',
    ;; `automatic', `backup', `exit-prefix', and `help'.")
    ;; Also: `quit', `edit-replacement'

    (set-keymap-parent map query-replace-map)

    (define-key map "\t" 'act-repeat)
    (define-key map [tab] 'act-repeat)
    (define-key map "\C-k" 'skip-rest)
    (define-key map "c" 'automatic)
    (define-key map "f" 'skip-keep)
    (define-key map "q" 'quit)
    (define-key map "d" 'skip)
    (define-key map "\C-d" 'skip)
    (define-key map "i" 'insert)
    (define-key map "I" 'insert-1)
    (define-key map "r" 'replace)
    (define-key map "R" 'replace-1)
    (define-key map "a" 'append)
    (define-key map "A" 'append-end)
    map)
  "Keymap that defines the responses to questions in `kmacro-step-edit-macro'.
This keymap is an extension to the `query-replace-map', allowing the
following additional answers: `insert', `insert-1', `replace', `replace-1',
`append', `append-end', `act-repeat', `skip-end', `skip-keep'.")

(defvar kmacro-step-edit-prefix-commands
  '(universal-argument universal-argument-more universal-argument-minus
		       digit-argument negative-argument)
  "Commands which build up a prefix arg for the current command.")

(defun kmacro-step-edit-prompt (macro index)
  ;; Show step-edit prompt
  (let ((keys (and (not kmacro-step-edit-appending)
		   index (substring macro index executing-kbd-macro-index)))
	(future (and (not kmacro-step-edit-appending)
		     (substring macro executing-kbd-macro-index)))
	(message-log-max nil)
	(curmsg (current-message)))

    ;; TODO: Scroll macro if max-mini-window-height is too small.
    (message "%s"
	     (concat
	      (format "Macro: %s%s%s%s%s\n"
		      (format-kbd-macro kmacro-step-edit-new-macro 1)
		      (if (and kmacro-step-edit-new-macro (> (length kmacro-step-edit-new-macro) 0)) " " "")
		      (propertize (if keys (format-kbd-macro keys)
				    (if kmacro-step-edit-appending "<APPEND>" "<INSERT>")) 'face 'region)
		      (if future " " "")
		      (if future (format-kbd-macro future) ""))
	      (cond
	       ((minibufferp)
		(format "%s\n%s\n"
			  (propertize "\
                         minibuffer                             " 'face 'header-line)
			  (buffer-substring (point-min) (point-max))))
	       (curmsg
		(format "%s\n%s\n"
			(propertize "\
                         echo area                              " 'face 'header-line)
			curmsg))
	       (t ""))
	      (if keys
		  (format "%s\n%s%s %S [yn iIaArR C-k kq!] "
			  (propertize "\
--------------Step Edit Keyboard Macro  [?: help]---------------" 'face 'mode-line)
			  (if kmacro-step-edit-help "\
 Step: y/SPC: execute next,  d/n/DEL: skip next,  f: skip but keep
       TAB: execute while same,  ?: toggle help
 Edit: i: insert,  r: replace,  a: append,  A: append at end,
       I/R: insert/replace with one sequence,
 End:  !/c: execute rest,  C-k: skip rest and save,  q/C-g: quit
----------------------------------------------------------------
" "")
			  (propertize "Next command:" 'face 'bold)
			  this-command)
		(propertize
		 (format "Type key sequence%s to insert and execute%s: "
			 (if (numberp kmacro-step-edit-inserting) ""  "s")
			 (if (numberp kmacro-step-edit-inserting) ""  " (end with C-j)"))
		 'face 'bold))))))

(defun kmacro-step-edit-query ()
  ;; Pre-command hook function for step-edit in "command" mode
  (let ((resize-mini-windows t)
	(max-mini-window-height kmacro-step-edit-mini-window-height)
	act restore-index next-index)

    ;; Handle commands which reads additional input using read-char.
    (cond
     ((and (eq this-command 'quoted-insert)
	   (not (eq kmacro-step-edit-action t)))
      ;; Find the actual end of this key sequence.
      ;; Must be able to backtrack in case we actually execute it.
      (setq restore-index executing-kbd-macro-index)
      (let (unread-command-events)
	(quoted-insert 0)
	(when unread-command-events
	  (setq executing-kbd-macro-index (- executing-kbd-macro-index (length unread-command-events))
		next-index executing-kbd-macro-index)))))

    ;; Query the user; stop macro execution temporarily.
    (let ((macro executing-kbd-macro)
	  (executing-kbd-macro nil)
	  (defining-kbd-macro nil))

      ;; Any action requested by previous command
      (cond
       ((eq kmacro-step-edit-action t)  ;; Reentry for actual command @ end of prefix arg.
	(cond
	 ((eq this-command 'quoted-insert)
	  (clear-this-command-keys) ;; recent-keys actually
	  (let (unread-command-events)
	    (quoted-insert (prefix-numeric-value current-prefix-arg))
	    (setq kmacro-step-edit-new-macro
		  (vconcat kmacro-step-edit-new-macro (recent-keys)))
	    (when unread-command-events
	      (setq kmacro-step-edit-new-macro
		    (substring kmacro-step-edit-new-macro 0 (- (length unread-command-events)))
		    executing-kbd-macro-index (- executing-kbd-macro-index (length unread-command-events)))))
	  (setq current-prefix-arg nil
		prefix-arg nil)
	  (setq act 'ignore))
	 (t
	  (setq act 'act)))
	(setq kmacro-step-edit-action nil))
       ((eq this-command kmacro-step-edit-action)  ;; TAB -> activate while same command
	(setq act 'act))
       (t
	(setq kmacro-step-edit-action nil)))

      ;; Handle prefix arg, or query user
      (cond
       (act act) ;; set above
       ((memq this-command kmacro-step-edit-prefix-commands)
	(unless kmacro-step-edit-prefix-index
	  (setq kmacro-step-edit-prefix-index kmacro-step-edit-key-index))
	(setq act 'universal-argument))
       ((eq this-command 'universal-argument-other-key)
	(setq act 'universal-argument))
       (t
	(kmacro-step-edit-prompt macro (or kmacro-step-edit-prefix-index kmacro-step-edit-key-index))
	(setq act (lookup-key kmacro-step-edit-map
			      (vector (with-current-buffer (current-buffer) (read-event))))))))

    ;; Resume macro execution and perform the action
    (cond
     ((eq act 'universal-argument)
      nil)
     ((cond
       ((eq act 'act)
	t)
       ((eq act 'act-repeat)
	(setq kmacro-step-edit-action this-command)
	t)
       ((eq act 'quit)
	(setq kmacro-step-edit-replace nil)
	(setq kmacro-step-edit-active 'ignore)
	nil)
       ((eq act 'skip)
	(setq kmacro-step-edit-prefix-index nil)
	nil)
       ((eq act 'skip-keep)
	(setq this-command 'ignore)
	t)
       ((eq act 'skip-rest)
	(setq kmacro-step-edit-active 'ignore)
	nil)
       ((memq act '(automatic exit))
	(setq kmacro-step-edit-active nil)
	(setq act t)
	t)
       ((member act '(insert-1 insert))
	(setq executing-kbd-macro-index (or kmacro-step-edit-prefix-index kmacro-step-edit-key-index))
	(setq kmacro-step-edit-inserting (if (eq act 'insert-1) 1 t))
	nil)
       ((member act '(replace-1 replace))
	(setq kmacro-step-edit-inserting (if (eq act 'replace-1) 1 t))
	(setq kmacro-step-edit-prefix-index nil)
	(if (= executing-kbd-macro-index (length executing-kbd-macro))
	    (setq executing-kbd-macro (vconcat executing-kbd-macro [nil])
		  kmacro-step-edit-appending t))
	nil)
       ((eq act 'append)
	(setq kmacro-step-edit-inserting t)
	(if (= executing-kbd-macro-index (length executing-kbd-macro))
	    (setq executing-kbd-macro (vconcat executing-kbd-macro [nil])
		  kmacro-step-edit-appending t))
	t)
       ((eq act 'append-end)
	(if (= executing-kbd-macro-index (length executing-kbd-macro))
	    (setq executing-kbd-macro (vconcat executing-kbd-macro [nil])
		  kmacro-step-edit-inserting t
		  kmacro-step-edit-appending t)
	  (setq kmacro-step-edit-active 'append-end))
	(setq act t)
	t)
       ((eq act 'help)
	(setq executing-kbd-macro-index (or kmacro-step-edit-prefix-index kmacro-step-edit-key-index))
	(setq kmacro-step-edit-help (not kmacro-step-edit-help))
	nil)
       (t ;; Ignore unknown responses
	(setq executing-kbd-macro-index (or kmacro-step-edit-prefix-index kmacro-step-edit-key-index))
	nil))
      (if (> executing-kbd-macro-index (or kmacro-step-edit-prefix-index kmacro-step-edit-key-index))
	  (setq kmacro-step-edit-new-macro
		(vconcat kmacro-step-edit-new-macro
			 (substring executing-kbd-macro
				    (or kmacro-step-edit-prefix-index kmacro-step-edit-key-index)
				    (if (eq act t) nil executing-kbd-macro-index)))
		kmacro-step-edit-prefix-index nil))
      (if restore-index
	  (setq executing-kbd-macro-index restore-index)))
     (t
      (setq this-command 'ignore)))
    (setq kmacro-step-edit-key-index next-index)))

(defun kmacro-step-edit-insert ()
  ;; Pre-command hook function for step-edit in "insert" mode
  (let ((resize-mini-windows t)
	(max-mini-window-height kmacro-step-edit-mini-window-height)
	(macro executing-kbd-macro)
	(executing-kbd-macro nil)
	(defining-kbd-macro nil)
	cmd keys next-index)
    (setq executing-kbd-macro-index (or kmacro-step-edit-prefix-index kmacro-step-edit-key-index)
	  kmacro-step-edit-prefix-index nil)
    (kmacro-step-edit-prompt macro nil)
    ;; Now, we have read a key sequence from the macro, but we don't want
    ;; to execute it yet.  So push it back and read another sequence.
    (reset-this-command-lengths)
    (setq keys (read-key-sequence nil nil nil nil t))
    (setq cmd (key-binding keys t nil))
    (if (cond
	 ((null cmd)
	  t)
	 ((eq cmd 'quoted-insert)
	  (clear-this-command-keys) ;; recent-keys actually
	  (quoted-insert (prefix-numeric-value current-prefix-arg))
	  (setq current-prefix-arg nil
		prefix-arg nil)
	  (setq keys (vconcat keys (recent-keys)))
	  (when (numberp kmacro-step-edit-inserting)
	    (setq kmacro-step-edit-inserting nil)
	    (when unread-command-events
	      (setq keys (substring keys 0 (- (length unread-command-events)))
		    executing-kbd-macro-index (- executing-kbd-macro-index (length unread-command-events))
		    next-index executing-kbd-macro-index
		    unread-command-events nil)))
	  (setq cmd 'ignore)
	  nil)
	 ((memq cmd kmacro-step-edit-prefix-commands)
	  (setq universal-argument-num-events 0)
	  (reset-this-command-lengths)
	  nil)
	 ((eq cmd 'universal-argument-other-key)
	  (setq kmacro-step-edit-action t)
	  (setq universal-argument-num-events 0)
	  (reset-this-command-lengths)
	  (if (numberp kmacro-step-edit-inserting)
	      (setq kmacro-step-edit-inserting nil))
	  nil)
	 ((numberp kmacro-step-edit-inserting)
	  (setq kmacro-step-edit-inserting nil)
	  nil)
	 ((equal keys "\C-j")
	  (setq kmacro-step-edit-inserting nil)
	  (setq kmacro-step-edit-action nil)
	  ;; Forget any (partial) prefix arg from next command
	  (setq kmacro-step-edit-prefix-index nil)
	  (reset-this-command-lengths)
	  (setq overriding-terminal-local-map nil)
	  (setq universal-argument-num-events nil)
	  (setq next-index kmacro-step-edit-key-index)
	  t)
	 (t nil))
	(setq this-command 'ignore)
      (setq this-command cmd)
      (if (memq this-command '(self-insert-command digit-argument))
	  (setq last-command-event (aref keys (1- (length keys)))))
      (if keys
	  (setq kmacro-step-edit-new-macro (vconcat kmacro-step-edit-new-macro keys))))
    (setq kmacro-step-edit-key-index next-index)))

(defun kmacro-step-edit-pre-command ()
  (remove-hook 'post-command-hook 'kmacro-step-edit-post-command)
  (when kmacro-step-edit-active
    (cond
     ((eq kmacro-step-edit-active 'ignore)
      (setq this-command 'ignore))
     ((eq kmacro-step-edit-active 'append-end)
      (if (= executing-kbd-macro-index (length executing-kbd-macro))
	  (setq executing-kbd-macro (vconcat executing-kbd-macro [nil])
		kmacro-step-edit-inserting t
		kmacro-step-edit-appending t
		kmacro-step-edit-active t)))
     ((/= kmacro-step-edit-num-input-keys num-input-keys)
      (if kmacro-step-edit-inserting
	  (kmacro-step-edit-insert)
	(kmacro-step-edit-query))
      (setq kmacro-step-edit-num-input-keys num-input-keys)
      (if (and kmacro-step-edit-appending (not kmacro-step-edit-inserting))
	  (setq kmacro-step-edit-appending nil
		kmacro-step-edit-active 'ignore)))))
  (when (eq kmacro-step-edit-active t)
    (add-hook 'post-command-hook 'kmacro-step-edit-post-command t)))

(defun kmacro-step-edit-minibuf-setup ()
  (remove-hook 'pre-command-hook 'kmacro-step-edit-pre-command t)
  (when kmacro-step-edit-active
    (add-hook 'pre-command-hook 'kmacro-step-edit-pre-command nil t)))

(defun kmacro-step-edit-post-command ()
  (remove-hook 'pre-command-hook 'kmacro-step-edit-pre-command)
  (when kmacro-step-edit-active
    (add-hook 'pre-command-hook 'kmacro-step-edit-pre-command nil nil)
    (if kmacro-step-edit-key-index
	(setq executing-kbd-macro-index kmacro-step-edit-key-index)
      (setq kmacro-step-edit-key-index executing-kbd-macro-index))))


(defun kmacro-step-edit-macro ()
  "Step edit and execute last keyboard macro.

To customize possible responses, change the \"bindings\" in `kmacro-step-edit-map'."
  (interactive)
  (let ((kmacro-step-edit-active t)
	(kmacro-step-edit-new-macro "")
	(kmacro-step-edit-inserting nil)
	(kmacro-step-edit-appending nil)
	(kmacro-step-edit-replace t)
	(kmacro-step-edit-prefix-index nil)
	(kmacro-step-edit-key-index 0)
	(kmacro-step-edit-action nil)
	(kmacro-step-edit-help nil)
	(kmacro-step-edit-num-input-keys num-input-keys)
	(pre-command-hook pre-command-hook)
	(post-command-hook post-command-hook)
	(minibuffer-setup-hook minibuffer-setup-hook))
    (add-hook 'pre-command-hook 'kmacro-step-edit-pre-command nil)
    (add-hook 'post-command-hook 'kmacro-step-edit-post-command t)
    (add-hook 'minibuffer-setup-hook 'kmacro-step-edit-minibuf-setup t)
    (call-last-kbd-macro nil nil)
    (when (and kmacro-step-edit-replace
	       kmacro-step-edit-new-macro
	       (not (equal last-kbd-macro kmacro-step-edit-new-macro)))
      (kmacro-push-ring)
      (setq last-kbd-macro kmacro-step-edit-new-macro))))

(provide 'kmacro)

;;; kmacro.el ends here
