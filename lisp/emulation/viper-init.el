;;; viper-init.el --- some common definitions for Viper

;; Copyright (C) 1997-2012  Free Software Foundation, Inc.

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

;; compiler pacifier
(defvar mark-even-if-inactive)
(defvar quail-mode)
(defvar iso-accents-mode)
(defvar viper-current-state)
(defvar viper-version)
(defvar viper-expert-level)
(defvar current-input-method)
(defvar default-input-method)
(defvar describe-current-input-method-function)
(defvar bar-cursor)
(defvar cursor-type)
;; end pacifier


;; Viper version
(defun viper-version ()
  (interactive)
  (message "Viper version is %s" viper-version))

;; Tell whether we are running as a window application or on a TTY

(defsubst viper-device-type ()
  (if (featurep 'xemacs)
      (device-type (selected-device))
    window-system))

(defun viper-color-display-p ()
  (condition-case nil
      (if (featurep 'xemacs)
          (eq (device-class (selected-device)) 'color)
        (display-color-p))
    (error nil)))

;; in XEmacs: device-type is tty on tty and stream in batch.
(defun viper-window-display-p ()
  (and (viper-device-type) (not (memq (viper-device-type) '(tty stream pc)))))

(defcustom viper-ms-style-os-p
  (memq system-type (if (featurep 'emacs) '(ms-dos windows-nt)
		      '(ms-dos windows-nt windows-95)))
  "Non-nil if Emacs is running under an MS-style OS: MS-DOS, or MS-Windows."
  :type 'boolean
  :tag "Is it Microsoft-made OS?"
  :group 'viper-misc)

(defcustom viper-suppress-input-method-change-message nil
  "If t, the message notifying about changes in the input method is not displayed.
Normally, a message is displayed each time on enters the vi, insert or replace
state."
  :type 'boolean
  :group 'viper-misc)

(defcustom viper-force-faces nil
  "If t, Viper will think that it is running on a display that supports faces.
This is provided as a temporary relief for users of graphics-capable terminals
that Viper doesn't know about.
In all likelihood, you don't need to bother with this setting."
  :type 'boolean
  :group 'viper-highlighting)

(defun viper-has-face-support-p ()
  (cond ((viper-window-display-p))
	(viper-force-faces)
	((viper-color-display-p))
	((featurep 'emacs) (memq (viper-device-type) '(pc)))
	((featurep 'xemacs) (memq (viper-device-type) '(tty pc)))))


;;; Macros

(defmacro viper-deflocalvar (var default-value &optional documentation)
  `(progn
    (defvar ,var ,default-value
      ,(format "%s\n\(buffer local\)" documentation))
    (make-variable-buffer-local ',var)))

;; (viper-loop COUNT BODY) Execute BODY COUNT times.
(defmacro viper-loop (count &rest body)
  `(let ((count ,count))
    (while (> count 0)
      ,@body
      (setq count (1- count)))))

(defmacro viper-buffer-live-p (buf)
  `(and ,buf (get-buffer ,buf) (buffer-name (get-buffer ,buf))))

;; return buffer-specific macro definition, given a full macro definition
(defmacro viper-kbd-buf-alist (macro-elt)
  `(nth 1 ,macro-elt))
;; get a pair: (curr-buffer . macro-definition)
(defmacro viper-kbd-buf-pair (macro-elt)
  `(assoc (buffer-name) (viper-kbd-buf-alist ,macro-elt)))
;; get macro definition for current buffer
(defmacro viper-kbd-buf-definition (macro-elt)
  `(cdr (viper-kbd-buf-pair ,macro-elt)))

;; return mode-specific macro definitions, given a full macro definition
(defmacro viper-kbd-mode-alist (macro-elt)
  `(nth 2 ,macro-elt))
;; get a pair: (major-mode . macro-definition)
(defmacro viper-kbd-mode-pair (macro-elt)
  `(assoc major-mode (viper-kbd-mode-alist ,macro-elt)))
;; get macro definition for the current major mode
(defmacro viper-kbd-mode-definition (macro-elt)
  `(cdr (viper-kbd-mode-pair ,macro-elt)))

;; return global macro definition, given a full macro definition
(defmacro viper-kbd-global-pair (macro-elt)
  `(nth 3 ,macro-elt))
;; get global macro definition from an elt of macro-alist
(defmacro viper-kbd-global-definition (macro-elt)
  `(cdr (viper-kbd-global-pair ,macro-elt)))

;; last elt of a sequence
(defsubst viper-seq-last-elt (seq)
  (elt seq (1- (length seq))))

(defsubst viper-string-to-list (string)
  (append (vconcat string) nil))

(defsubst viper-charlist-to-string (list)
  (mapconcat 'char-to-string list ""))

;; like char-after/before, but saves typing
(defun viper-char-at-pos (direction &optional offset)
  (or (integerp offset) (setq offset 0))
  (if (eq direction 'forward)
      (char-after (+ (point) offset))
    (char-before (- (point) offset))))


(defvar viper-minibuffer-overlay-priority 300)
(defvar viper-replace-overlay-priority 400)
(defvar viper-search-overlay-priority 500)


;;; Viper minor modes

;; Mode for vital things like \e, C-z.
(viper-deflocalvar viper-vi-intercept-minor-mode nil)

(viper-deflocalvar viper-vi-basic-minor-mode nil
  "Viper's minor mode for Vi bindings.")

(viper-deflocalvar viper-vi-local-user-minor-mode nil
  "Auxiliary minor mode for user-defined local bindings in Vi state.")

(viper-deflocalvar viper-vi-global-user-minor-mode nil
  "Auxiliary minor mode for user-defined global bindings in Vi state.")

(viper-deflocalvar viper-vi-state-modifier-minor-mode nil
  "Minor mode used to make major-mode-specific modification to Vi state.")

(viper-deflocalvar viper-vi-diehard-minor-mode nil
  "This minor mode is in effect when the user wants Viper to be Vi.")

(viper-deflocalvar viper-vi-kbd-minor-mode nil
  "Minor mode for Ex command macros in Vi state.
The corresponding keymap stores key bindings of Vi macros defined with
the Ex command :map.")

;; Mode for vital things like \e, C-z.
(viper-deflocalvar viper-insert-intercept-minor-mode nil)

(viper-deflocalvar viper-insert-basic-minor-mode nil
  "Viper's minor mode for bindings in Insert mode.")

(viper-deflocalvar viper-insert-local-user-minor-mode nil
  "Auxiliary minor mode for buffer-local user-defined bindings in Insert state.
This is a way to overshadow normal Insert mode bindings locally to certain
designated buffers.")

(viper-deflocalvar viper-insert-global-user-minor-mode nil
  "Auxiliary minor mode for global user-defined bindings in Insert state.")

(viper-deflocalvar viper-insert-state-modifier-minor-mode nil
  "Minor mode used to make major-mode-specific modification to Insert state.")

(viper-deflocalvar viper-insert-diehard-minor-mode nil
  "Minor mode that simulates Vi very closely.
Not recommended, except for the novice user.")

(viper-deflocalvar viper-insert-kbd-minor-mode nil
"Minor mode for Ex command macros Insert state.
The corresponding keymap stores key bindings of Vi macros defined with
the Ex command :map!.")

(viper-deflocalvar viper-replace-minor-mode nil
  "Minor mode in effect in replace state (cw, C, and the like commands).")

;; Mode for vital things like \C-z and \C-x) This is set to t, when viper-mode
;; is invoked.  So, any new buffer will have C-z defined as switch to Vi,
;; unless we switched states in this buffer
(viper-deflocalvar viper-emacs-intercept-minor-mode nil)

(viper-deflocalvar viper-emacs-local-user-minor-mode nil
  "Minor mode for local user bindings effective in Emacs state.
Users can use it to override Emacs bindings when Viper is in its Emacs
state.")

(viper-deflocalvar viper-emacs-global-user-minor-mode nil
  "Minor mode for global user bindings in effect in Emacs state.
Users can use it to override Emacs bindings when Viper is in its Emacs
state.")

(viper-deflocalvar viper-emacs-kbd-minor-mode nil
  "Minor mode for Vi style macros in Emacs state.
The corresponding keymap stores key bindings of Vi macros defined with
`viper-record-kbd-macro' command.  There is no Ex-level command to do this
interactively.")

(viper-deflocalvar viper-emacs-state-modifier-minor-mode nil
  "Minor mode used to make major-mode-specific modification to Emacs state.
For instance, a Vi purist may want to bind `dd' in Dired mode to a function
that deletes a file.")

(viper-deflocalvar viper-vi-minibuffer-minor-mode nil
   "Minor mode that forces Vi-style when the Minibuffer is in Vi state.")

(viper-deflocalvar viper-insert-minibuffer-minor-mode nil
   "Minor mode that forces Vi-style when the Minibuffer is in Insert state.")



;; Some common error messages

(defconst viper-SpuriousText "Spurious text after command"  "")
(defconst viper-BadExCommand "Not an editor command"   "")
(defconst viper-InvalidCommandArgument "Invalid command argument"   "")
(defconst viper-NoPrevSearch "No previous search string"   "")
(defconst viper-EmptyRegister "`%c': Nothing in this register"   "")
(defconst viper-InvalidRegister "`%c': Invalid register"   "")
(defconst viper-EmptyTextmarker "`%c': Text marker doesn't point anywhere"   "")
(defconst viper-InvalidTextmarker "`%c': Invalid text marker"   "")
(defconst viper-InvalidViCommand "Invalid command"   "")
(defconst viper-BadAddress "Ill-formed address"   "")
(defconst viper-FirstAddrExceedsSecond "First address exceeds second"   "")
(defconst viper-NoFileSpecified "No file specified"   "")

;; Is t until viper-mode executes for the very first time.
;; Prevents recursive descend into startup messages.
(defvar viper-first-time t)

(defvar viper-expert-level (if (boundp 'viper-expert-level) viper-expert-level 0)
  "User's expert level.
The minor mode viper-vi-diehard-minor-mode is in effect when
viper-expert-level is 1 or 2 or when viper-want-emacs-keys-in-vi is t.
The minor mode viper-insert-diehard-minor-mode is in effect when
viper-expert-level is 1 or 2 or if viper-want-emacs-keys-in-insert is t.
Use `M-x viper-set-expert-level' to change this.")

;; Max expert level supported by Viper.  This is NOT a user option.
;; It is here to make it hard for the user from resetting it.
(defconst viper-max-expert-level 5)


;;; ISO characters and MULE

;; If non-nil, ISO accents will be turned on in insert/replace emacs states and
;; turned off in vi-state.  For some users, this behavior may be too
;; primitive.  In this case, use insert/emacs/vi state hooks.
(viper-deflocalvar viper-automatic-iso-accents nil "")
;; Set iso-accents-mode to ARG.  Check if it is bound first
(defsubst viper-set-iso-accents-mode (arg)
  (if (boundp 'iso-accents-mode)
      (setq iso-accents-mode arg)))

;; Internal flag used to control when viper mule hooks are run.
;; Don't change this!
(defvar viper-mule-hook-flag t)
;; If non-nil, the default intl.  input method is turned on.
(viper-deflocalvar viper-special-input-method nil "")

;; viper hook to run on input-method activation
(defun viper-activate-input-method-action ()
  (if (null viper-mule-hook-flag)
      ()
    (setq viper-special-input-method t)
    ;; turn off special input methods in vi-state
    (if (eq viper-current-state 'vi-state)
	(viper-set-input-method nil))
    (if (and (memq viper-current-state '(vi-state insert-state replace-state))
	     (not viper-suppress-input-method-change-message))
	(message "Viper special input method%s: on"
		 (if (or current-input-method default-input-method)
		     (format " %S"
			     (or current-input-method default-input-method))
		   "")))
    ))

;; viper hook to run on input-method deactivation
(defun viper-inactivate-input-method-action ()
  (if (null viper-mule-hook-flag)
      ()
    (setq viper-special-input-method nil)
    (if (and (memq viper-current-state '(vi-state insert-state replace-state))
	     (not viper-suppress-input-method-change-message))
	(message "Viper special input method%s: off"
		 (if (or current-input-method default-input-method)
		     (format " %S"
			     (or current-input-method default-input-method))
		   "")))))

(defun viper-inactivate-input-method ()
  (cond ((and (featurep 'emacs) (fboundp 'inactivate-input-method))
	 (inactivate-input-method))
	((and (featurep 'xemacs) (boundp 'current-input-method))
	 ;; XEmacs had broken quail-mode for some time, so we are working around
	 ;; it here
	 (setq quail-mode nil)
	 (if (featurep 'quail)
	     (quail-delete-overlays))
	 (setq describe-current-input-method-function nil)
	 (setq current-input-method nil)
	 (run-hooks 'input-method-inactivate-hook)
	 (force-mode-line-update))
	))
(defun viper-activate-input-method ()
  (cond ((and (featurep 'emacs) (fboundp 'activate-input-method))
	 (activate-input-method default-input-method))
	((featurep 'xemacs)
	 (if (fboundp 'quail-mode) (quail-mode 1)))))

;; Set quail-mode to ARG
(defun viper-set-input-method (arg)
  (setq viper-mule-hook-flag t) ; just a precaution
  (let (viper-mule-hook-flag) ; temporarily deactivate viper mule hooks
    (cond ((and arg (> (prefix-numeric-value arg) 0) default-input-method)
	   ;; activate input method
	   (viper-activate-input-method))
	  (t ; deactivate input method
	   (viper-inactivate-input-method)))
    ))


;; VI-style Undo

;; Used to 'undo' complex commands, such as replace and insert commands.
(viper-deflocalvar viper-undo-needs-adjustment nil)
(put 'viper-undo-needs-adjustment 'permanent-local t)

;; A mark that Viper puts on buffer-undo-list.  Marks the beginning of a
;; complex command that must be undone atomically.  If inserted, it is
;; erased by viper-change-state-to-vi and viper-repeat.
(defconst viper-buffer-undo-list-mark 'viper)

(defcustom viper-keep-point-on-undo nil
  "*Non-nil means not to move point while undoing commands.
This style is different from Emacs and Vi.  Try it to see if
it better fits your working style."
  :type 'boolean
  :tag "Preserve Position of Point After Undo"
  :group 'viper)

;; Replace mode and changing text

;; Hack used to pass global states around for short period of time
(viper-deflocalvar viper-intermediate-command nil "")

;; This is used to pass the right Vi command key sequence to
;; viper-set-destructive-command whenever (this-command-keys) doesn't give the
;; right result.  For instance, in commands like c/bla<RET>,
;; (this-command-keys) will return ^M, which invoked exit-minibuffer, while we
;; need "c/"
(defconst viper-this-command-keys nil)

;; Indicates that the current destructive command has started in replace mode.
(viper-deflocalvar viper-began-as-replace nil "")

(defcustom viper-allow-multiline-replace-regions t
  "If non-nil, Viper will allow multi-line replace regions.
This is an extension to standard Vi.
If nil, commands that attempt to replace text spanning multiple lines first
delete the text being replaced, as in standard Vi."
  :type 'boolean
  :group 'viper)

(defcustom viper-replace-overlay-cursor-color "Red"
  "*Cursor color when Viper is in Replace state."
  :type 'string
  :group 'viper)

(defcustom viper-insert-state-cursor-color "Green"
  "Cursor color when Viper is in insert state."
  :type 'string
  :group 'viper)

;; viper-emacs-state-cursor-color doesn't work well. Causes cursor colors to be
;; confused in some cases. So, this var is nulled for now.
;; (defcustom viper-emacs-state-cursor-color "Magenta"
(defcustom viper-emacs-state-cursor-color nil
  "Cursor color when Viper is in Emacs state."
  :type 'string
  :group 'viper)

;; internal var, used to remember the default cursor color of emacs frames
(defvar viper-vi-state-cursor-color nil)

;; Frame-local variables are obsolete from Emacs 22.2 onwards, so we
;; do it by hand with viper-frame-value (qv).
(when (and (featurep 'xemacs)
           (fboundp 'make-variable-frame-local))
  (make-variable-frame-local 'viper-replace-overlay-cursor-color)
  (make-variable-frame-local 'viper-insert-state-cursor-color)
  (make-variable-frame-local 'viper-emacs-state-cursor-color)
  (make-variable-frame-local 'viper-vi-state-cursor-color))

(viper-deflocalvar viper-replace-overlay nil "")
(put 'viper-replace-overlay 'permanent-local t)

(defcustom viper-replace-region-end-delimiter "$"
  "A string marking the end of replacement regions.
It is used only with TTYs or if `viper-use-replace-region-delimiters'
is non-nil."
  :type 'string
  :group 'viper)
(defcustom viper-replace-region-start-delimiter ""
  "A string marking the beginning of replacement regions.
It is used only with TTYs or if `viper-use-replace-region-delimiters'
is non-nil."
  :type 'string
  :group 'viper)
(defcustom viper-use-replace-region-delimiters
  (or (not (viper-has-face-support-p))
      (and (featurep 'xemacs) (eq (viper-device-type) 'tty)))
  "*If non-nil, Viper will always use `viper-replace-region-end-delimiter' and
`viper-replace-region-start-delimiter' to delimit replacement regions, even on
color displays.  By default, the delimiters are used only on TTYs."
  :type 'boolean
  :group 'viper)

(defcustom viper-read-buffer-function 'read-buffer
  "Function to use for prompting the user for a buffer name."
  :type 'symbol
  :group 'viper)

;; XEmacs requires glyphs
(when (featurep 'xemacs)
  (or (glyphp viper-replace-region-end-delimiter)
      (setq viper-replace-region-end-delimiter
            (make-glyph viper-replace-region-end-delimiter)))
  (or (glyphp viper-replace-region-start-delimiter)
      (setq viper-replace-region-start-delimiter
            (make-glyph viper-replace-region-start-delimiter))))

;; These are local marker that must be initialized to nil and moved with
;; `viper-move-marker-locally'
;;
;; Remember the last position inside the replace region.
(viper-deflocalvar viper-last-posn-in-replace-region nil)
;; Remember the last position while inserting
(viper-deflocalvar viper-last-posn-while-in-insert-state nil)
(put 'viper-last-posn-in-replace-region 'permanent-local t)
(put 'viper-last-posn-while-in-insert-state 'permanent-local t)

(viper-deflocalvar viper-sitting-in-replace nil "")
(put 'viper-sitting-in-replace 'permanent-local t)

;; Remember the number of characters that have to be deleted in replace
;; mode to compensate for the inserted characters.
(viper-deflocalvar viper-replace-chars-to-delete 0 "")
;; This variable is used internally by the before/after changed functions to
;; determine how many chars were deleted by the change.  This can't be
;; determined inside after-change-functions because those get the length of the
;; deleted region, not the number of chars deleted (which are two different
;; things under MULE).
(viper-deflocalvar viper-replace-region-chars-deleted 0 "")

;; Insertion ring and command ring
(defcustom viper-insertion-ring-size 14
  "The size of history of inserted text.
This is a list where Viper keeps the history of previously inserted pieces of
text."
  :type 'integer
  :group 'viper-misc)
;; The insertion ring.
(defvar viper-insertion-ring nil)
;; This is temp insertion ring.  Used to do rotation for display purposes.
;; When rotation just started, it is initialized to viper-insertion-ring.
(defvar viper-temp-insertion-ring nil)
(defvar viper-last-inserted-string-from-insertion-ring "")

(defcustom viper-command-ring-size 14
  "The size of history of Vi commands repeatable with dot."
  :type 'integer
  :group 'viper-misc)
;; The command ring.
(defvar viper-command-ring nil)
;; This is temp command ring.  Used to do rotation for display purposes.
;; When rotation just started, it is initialized to viper-command-ring.
(defvar viper-temp-command-ring nil)

;; Fast keyseq and ESC keyseq timeouts
(defcustom viper-fast-keyseq-timeout 200
  "*Key sequence separated by no more than this many milliseconds is viewed as a Vi-style macro, if such a macro is defined.
Setting this too high may slow down your typing.  Setting this value too low
will make it hard to use Vi-style timeout macros."
  :type 'integer
  :group 'viper-misc)

;; This function determines if ESC key sequences are to be translated into
;; commands.
(defun viper-translate-all-ESC-keysequences ()
  (not (viper-window-display-p)))

;; Modes and related variables

;; Current mode.  One of: `emacs-state', `vi-state', `insert-state'
(viper-deflocalvar viper-current-state 'emacs-state)


;; Autoindent in insert

;; Variable that keeps track of whether C-t has been pressed.
(viper-deflocalvar viper-cted nil "")

;; Preserve the indent value, used by C-d in insert mode.
(viper-deflocalvar viper-current-indent 0)

;; Whether to preserve the indent, used by C-d in insert mode.
(viper-deflocalvar viper-preserve-indent nil)

(viper-deflocalvar viper-auto-indent nil "")
(defcustom viper-auto-indent nil
  "*Enable autoindent, if t.
This is a buffer-local variable."
  :type 'boolean
  :group 'viper)

(viper-deflocalvar viper-electric-mode t "")
(defcustom viper-electric-mode t
  "*If t, electrify Viper.
Currently, this only electrifies auto-indentation, making it appropriate to the
mode of the buffer.
This means that auto-indentation will depart from standard Vi and will indent
appropriate to the mode of the buffer.  This is especially useful for editing
programs and LaTeX documents."
  :type 'boolean
  :group 'viper)

(defcustom viper-shift-width 8
  "*The value of the shiftwidth.
This determines the number of columns by which the Ctl-t moves the cursor in
the Insert state."
  :type 'integer
  :group 'viper)

;; Variables for repeating destructive commands

(defcustom viper-keep-point-on-repeat t
  "*If t, don't move point when repeating previous command.
This is useful for doing repeated changes with the '.' key.
The user can change this to nil, if she likes when the cursor moves
to a new place after repeating previous Vi command."
  :type 'boolean
  :group 'viper)

;; Remember insert point as a marker.  This is a local marker that must be
;; initialized to nil and moved with `viper-move-marker-locally'.
(viper-deflocalvar viper-insert-point nil)
(put 'viper-insert-point 'permanent-local t)

;; This remembers the point before dabbrev-expand was called.
;; If viper-insert-point turns out to be bigger than that, it is reset
;; back to viper-pre-command-point.
;; The reason this is needed is because dabbrev-expand (and possibly
;; others) may jump to before the insertion point, delete something and
;; then reinsert a bigger piece.  For instance:  bla^blo
;; If dabbrev-expand is called after `blo' and ^ indicates viper-insert-point,
;; then point jumps to the beginning of `blo'.  If expansion is found, `blablo'
;; is deleted, and we have |^, where | denotes point.  Next, dabbrev-expand
;; will insert the expansion, and we get: blablo^
;; Whatever we insert next goes before the ^, i.e., before the
;; viper-insert-point marker.  So, Viper will think that nothing was
;; inserted.  Remembering the orig position of the marker circumvents the
;; problem.
;; We don't know of any command, except dabbrev-expand, that has the same
;; problem.  However, the same trick can be used if such a command is
;; discovered later.
;;
(viper-deflocalvar viper-pre-command-point nil)
(put 'viper-pre-command-point 'permanent-local t) ; this is probably an overkill

;; This is used for saving inserted text.
(defvar viper-last-insertion  nil)

;; Remembers the last replaced region.
(defvar viper-last-replace-region "")

;; Remember com point as a marker.
;; This is a local marker.  Should be moved with `viper-move-marker-locally'
(viper-deflocalvar viper-com-point nil)

;; If non-nil, the value is a list (M-COM VAL COM REG inserted-text cmd-keys)
;; It is used to re-execute last destructive command.
;; M-COM is a Lisp symbol representing the function to be executed.
;; VAL is the prefix argument that was used with that command.
;; COM is an internal descriptor, such as ?r, ?c, ?C, which contains
;; additional information on how the function in M-COM is to be handled.
;; REG is the register used by command
;; INSERTED-TEXT is text inserted by that command (in case of o, c, C, i, r
;; commands).
;; COMMAND-KEYS are the keys that were typed to invoke the command.
(defvar viper-d-com nil)

;; The character remembered by the Vi `r' command.
(defvar viper-d-char nil)

;; Name of register to store deleted or yanked strings
(defvar viper-use-register nil)


;;; Variables for Moves and Searches

(defgroup viper-search nil
  "Variables that define the search and query-replace behavior of Viper."
  :prefix "viper-"
  :group 'viper)

;; For use by `;' command.
(defvar viper-f-char nil)

;; For use by `.' command.
(defvar viper-F-char nil)

;; For use by `;' command.
(defvar viper-f-forward nil)

;; For use by `;' command.
(defvar viper-f-offset nil)

;; Last search string
(defvar viper-s-string "")

(defcustom viper-quote-string "> "
  "String inserted at the beginning of quoted region."
  :type 'string
  :group 'viper)

;; If t, search is forward.
(defvar viper-s-forward nil)

(defcustom viper-case-fold-search nil
  "*If not nil, search ignores cases."
  :type 'boolean
  :group 'viper-search)

(defcustom viper-re-search t
  "*If not nil, search is regexp search, otherwise vanilla search."
  :type 'boolean
  :tag "Regexp Search"
  :group 'viper-search)

(defcustom viper-search-scroll-threshold 2
  "*If search lands within this threshold from the window top/bottom,
the window will be scrolled up or down appropriately, to reveal context.
If you want Viper search to behave as usual in Vi, set this variable to a
negative number."
  :type 'boolean
  :group 'viper-search)

(defcustom viper-re-query-replace t
  "*If t then do regexp replace, if nil then do string replace."
  :type 'boolean
  :tag "Regexp Query Replace"
  :group 'viper-search)

(defcustom viper-re-replace t
  "*If t, do regexp replace.  nil means do string replace."
  :type 'boolean
  :tag "Regexp Replace"
  :group 'viper-search)

(defcustom viper-parse-sexp-ignore-comments t
  "*If t, `%' ignores the parentheses that occur inside comments."
  :type 'boolean
  :group 'viper)

(viper-deflocalvar viper-ex-style-motion t "")
(defcustom viper-ex-style-motion t
  "*If t, the commands l,h do not cross lines, etc (Ex-style).
If nil, these commands cross line boundaries."
  :type 'boolean
  :group 'viper)

(viper-deflocalvar viper-ex-style-editing t "")
(defcustom viper-ex-style-editing t
  "*If t, Ex-style behavior while editing in Vi command and insert states.
`Backspace' and `Delete' don't cross line boundaries in insert.
`X' and `x' can't delete characters across line boundary in Vi, etc.
Note: this doesn't preclude `Backspace' and `Delete' from deleting characters
by moving past the insertion point.  This is a feature, not a bug.

If nil, the above commands can work across lines."
  :type 'boolean
  :group 'viper)

(viper-deflocalvar viper-ESC-moves-cursor-back viper-ex-style-editing "")
(defcustom viper-ESC-moves-cursor-back nil
  "*If t, ESC moves cursor back when changing from insert to vi state.
If nil, the cursor stays where it was when ESC was hit."
  :type 'boolean
  :group 'viper)

(viper-deflocalvar viper-delete-backwards-in-replace nil "")
(defcustom viper-delete-backwards-in-replace nil
  "*If t, DEL key will delete characters while moving the cursor backwards.
If nil, the cursor will move backwards without deleting anything."
  :type 'boolean
  :group 'viper)

(defcustom viper-buffer-search-char nil
  "*Key used for buffer-searching.  Must be a character type, e.g., ?g."
  :type '(choice (const nil) character)
  :group 'viper-search)

(defcustom viper-search-wrap-around t
  "*If t, search wraps around."
  :type 'boolean
  :tag "Search Wraps Around"
  :group 'viper-search)

(viper-deflocalvar viper-related-files-and-buffers-ring nil "")
(defcustom viper-related-files-and-buffers-ring nil
  "*List of file and buffer names that are considered to be related to the current buffer.
Related buffers can be cycled through via :R and :P commands."
  :type 'boolean
  :group 'viper-misc)
(put 'viper-related-files-and-buffers-ring 'permanent-local t)

;; Used to find out if we are done with searching the current buffer.
(viper-deflocalvar viper-local-search-start-marker nil)
;; As above, but global
(defvar viper-search-start-marker (make-marker))

;; the search overlay
(viper-deflocalvar viper-search-overlay nil)


(defvar viper-heading-start
  (concat "^\\s-*(\\s-*defun\\s-\\|"			        ; lisp
	  "^{\\s-*$\\|^[_a-zA-Z][^()]*[()].*{\\s-*$\\|"	        ; C/C++
	  "^\\s-*class.*{\\|^\\s-*struct.*{\\|^\\s-*enum.*{\\|"
	  "^\\\\[sb][a-z]*{.*}\\s-*$\\|"	    		; latex
	  "^@node\\|@table\\|^@m?enu\\|^@itemize\\|^@if\\|"	; texinfo
	  "^.+:-")			                        ; prolog
  "*Regexps for Headings.  Used by \[\[ and \]\].")

(defvar viper-heading-end
  (concat "^}\\|"						; C/C++
	  "^\\\\end{\\|"					; latex
	  "^@end \\|"						; texinfo
	  ")\n\n[ \t\n]*\\|"					; lisp
	  "\\.\\s-*$")						; prolog
      "*Regexps to end Headings/Sections.  Used by \[\].")


;; These two vars control the interaction of jumps performed by ' and `.
;; In this new version, '' doesn't erase the marks set by ``, so one can
;; use both kinds of jumps interchangeably and without losing positions
;; inside the lines.

;; Remembers position of the last jump done using ``'.
(viper-deflocalvar viper-last-jump  nil)
;; Remembers position of the last jump done using `''.
(viper-deflocalvar viper-last-jump-ignore 0)

;; History variables

;; History of search strings.
(defvar viper-search-history  (list ""))
;; History of query-replace strings used as a source.
(defvar viper-replace1-history nil)
;; History of query-replace strings used as replacement.
(defvar viper-replace2-history nil)
;; History of region quoting strings.
(defvar viper-quote-region-history (list viper-quote-string))
;; History of Ex-style commands.
(defvar viper-ex-history nil)
;; History of shell commands.
(defvar viper-shell-history nil)


;; Last shell command.  There are two of these, one for Ex (in viper-ex)
;; and one for Vi.

;; Last shell command executed with ! command.
(defvar viper-last-shell-com nil)


;;; Face-saving tricks

(defgroup viper-highlighting nil
  "Highlighting of replace region, search pattern, minibuffer, etc."
  :prefix "viper-"
  :group 'viper)


(defface viper-search
  '((((class color)) (:foreground "Black" :background "khaki"))
    (t (:underline t :stipple "gray3")))
  "*Face used to flash out the search pattern."
  :group 'viper-highlighting)
;; An internal variable.  Viper takes the face from here.
(defvar viper-search-face 'viper-search
  "Face used to flash out the search pattern.
DO NOT CHANGE this variable.  Instead, use the customization widget
to customize the actual face object `viper-search'
this variable represents.")

(defface viper-replace-overlay
  '((((class color)) (:foreground "Black" :background "darkseagreen2"))
    (t (:underline t :stipple "gray3")))
  "*Face for highlighting replace regions on a window display."
  :group 'viper-highlighting)
;; An internal variable.  Viper takes the face from here.
(defvar viper-replace-overlay-face 'viper-replace-overlay
  "Face for highlighting replace regions on a window display.
DO NOT CHANGE this variable.  Instead, use the customization widget
to customize the actual face object `viper-replace-overlay'
this variable represents.")

(defface viper-minibuffer-emacs
  '((((class color)) (:foreground "Black" :background "darkseagreen2"))
    (t (:weight bold)))
  "Face used in the Minibuffer when it is in Emacs state."
  :group 'viper-highlighting)
;; An internal variable.  Viper takes the face from here.
(defvar viper-minibuffer-emacs-face 'viper-minibuffer-emacs
  "Face used in the Minibuffer when it is in Emacs state.
DO NOT CHANGE this variable.  Instead, use the customization widget
to customize the actual face object `viper-minibuffer-emacs'
this variable represents.")

(defface viper-minibuffer-insert
  '((((class color)) (:foreground "Black" :background "pink"))
    (t (:slant italic)))
  "Face used in the Minibuffer when it is in Insert state."
  :group 'viper-highlighting)
;; An internal variable.  Viper takes the face from here.
(defvar viper-minibuffer-insert-face 'viper-minibuffer-insert
  "Face used in the Minibuffer when it is in Insert state.
DO NOT CHANGE this variable.  Instead, use the customization widget
to customize the actual face object `viper-minibuffer-insert'
this variable represents.")

(defface viper-minibuffer-vi
  '((((class color)) (:foreground "DarkGreen" :background "grey"))
    (t (:inverse-video t)))
  "Face used in the Minibuffer when it is in Vi state."
  :group 'viper-highlighting)
;; An internal variable.  Viper takes the face from here.
(defvar viper-minibuffer-vi-face 'viper-minibuffer-vi
  "Face used in the Minibuffer when it is in Vi state.
DO NOT CHANGE this variable.  Instead, use the customization widget
to customize the actual face object `viper-minibuffer-vi'
this variable represents.")

;; the current face to be used in the minibuffer
(viper-deflocalvar
  viper-minibuffer-current-face viper-minibuffer-emacs-face "")


;;; Miscellaneous

(defvar viper-inhibit-startup-message nil
  "Whether Viper startup message should be inhibited.")

(defcustom viper-spell-function 'ispell-region
  "Spell function used by #s<move> command to spell."
  :type 'function
  :group 'viper-misc)

(defcustom viper-tags-file-name "TAGS"
  "The tags file used by Viper."
  :type 'string
  :group 'viper-misc)

(defcustom viper-change-notification-threshold 1
  "Notify the user when this many lines or characters have been deleted/yanked.
For line-deleting/yanking commands (like `dd', `yy'), the value denotes the
number of lines.  For character-based commands (such as `x', `dw', etc.), the
value refers to the number of characters affected."
  :type 'integer
  :group 'viper-misc)

;; Minibuffer

(defcustom viper-vi-style-in-minibuffer t
  "If t, use vi-style editing in minibuffer.
Should be set in `~/.viper' file."
  :type 'boolean
  :group 'viper)

;; overlay used in the minibuffer to indicate which state it is in
(viper-deflocalvar viper-minibuffer-overlay nil)
(put 'viper-minibuffer-overlay 'permanent-local t)

;; Hook, specific to Viper, which is run just *before* exiting the minibuffer.
;; This is needed because beginning with Emacs 19.26, the standard
;; `minibuffer-exit-hook' is run *after* exiting the minibuffer
(defvar viper-minibuffer-exit-hook nil)


;; Mode line
(defconst viper-vi-state-id  	"<V> "
  "Mode line tag identifying the Vi mode of Viper.")
(defconst viper-emacs-state-id	"<E> "
  "Mode line tag identifying the Emacs mode of Viper.")
(defconst viper-insert-state-id	"<I> "
  "Mode line tag identifying the Insert mode of Viper.")
(defconst viper-replace-state-id	"<R> "
  "Mode line tag identifying the Replace mode of Viper.")


(defgroup viper-hooks nil
  "Viper hooks."
  :prefix "viper-"
  :group 'viper)

(defcustom viper-vi-state-hook 'viper-restore-cursor-type
  "*Hooks run just before the switch to Vi mode is completed."
  :type 'hook
  :group 'viper-hooks)
(defcustom viper-insert-state-hook 'viper-set-insert-cursor-type
  "*Hooks run just before the switch to Insert mode is completed."
  :type 'hook
  :group 'viper-hooks)
(defcustom viper-replace-state-hook 'viper-restore-cursor-type
  "*Hooks run just before the switch to Replace mode is completed."
  :type 'hook
  :group 'viper-hooks)
(defcustom viper-emacs-state-hook 'viper-restore-cursor-type
  "*Hooks run just before the switch to Emacs mode is completed."
  :type 'hook
  :group 'viper-hooks)

(defcustom viper-load-hook nil
  "Hooks run just after loading Viper."
  :type 'hook
  :group 'viper-hooks)

(defun viper-restore-cursor-type ()
  (condition-case nil
      (if (featurep 'xemacs)
	  (set (make-local-variable 'bar-cursor) nil)
	(setq cursor-type (default-value 'cursor-type)))
    (error nil)))

(defun viper-set-insert-cursor-type ()
  (if (featurep 'xemacs)
      (set (make-local-variable 'bar-cursor) 2)
    (setq cursor-type '(bar . 2))))

(defun viper-ESC-keyseq-timeout ()
  "*Key sequence beginning with ESC and separated by no more than this many milliseconds is considered to be generated by a keyboard function key.
Setting this too high may slow down switching from insert to vi state.  Setting
this value too low will make it impossible to use function keys in insert mode
on a dumb terminal."
  (if (viper-window-display-p)
      0 viper-fast-keyseq-timeout))



(provide 'viper-init)


;; Local Variables:
;; eval: (put 'viper-deflocalvar 'lisp-indent-hook 'defun)
;; End:

;;; viper-init.el ends here
