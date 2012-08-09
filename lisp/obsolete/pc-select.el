;;; pc-select.el --- emulate mark, cut, copy and paste from Motif
;;;		     (or MAC GUI or MS-windoze (bah)) look-and-feel
;;;		     including key bindings.

;; Copyright (C) 1995-1997, 2000-2012 Free Software Foundation, Inc.

;; Author: Michael Staats <michael@thp.Uni-Duisburg.DE>
;; Keywords: convenience emulations
;; Created: 26 Sep 1995
;; Obsolete-since: 24.1

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

;; This package emulates the mark, copy, cut and paste look-and-feel of motif
;; programs (which is the same as the MAC gui and (sorry for that) MS-Windows).
;; It modifies the keybindings of the cursor keys and the next, prior,
;; home and end keys. They will modify mark-active.
;; You can still get the old behavior of cursor moving with the
;; control sequences C-f, C-b, etc.
;; This package uses transient-mark-mode and
;; delete-selection-mode.
;;
;; In addition to that all key-bindings from the pc-mode are
;; done here too (as suggested by RMS).
;;
;; As I found out after I finished the first version, s-region.el tries
;; to do the same.... But my code is a little more complete and using
;; delete-selection-mode is very important for the look-and-feel.
;; Pete Forman <pete.forman@airgun.wg.waii.com> provided some motif
;; compliant keybindings which I added. I had to modify them a little
;; to add the -mark and -nomark functionality of cursor moving.
;;
;; Credits:
;; Many thanks to all who made comments.
;; Thanks to RMS and Ralf Muschall <prm@rz.uni-jena.de> for criticism.
;; Kevin Cutts <cutts@ukraine.corp.mot.com> added the beginning-of-buffer
;; and end-of-buffer functions which I modified a little.
;; David Biesack <sasdjb@unx.sas.com> suggested some more cleanup.
;; Thanks to Pete Forman <pete.forman@airgun.wg.waii.com>
;; for additional motif keybindings.
;; Thanks to jvromans@squirrel.nl (Johan Vromans) for a bug report
;; concerning setting of this-command.
;; Dan Nicolaescu <done@ece.arizona.ro> suggested suppressing the
;; scroll-up/scroll-down error.
;; Eli Barzilay (eli@cs.bgu.ac.il) suggested the sexps functions and
;; keybindings.
;;
;; Ok, some details about the idea of PC Selection mode:
;;
;;  o The standard keys for moving around (right, left, up, down, home, end,
;;    prior, next, called "move-keys" from now on) will always de-activate
;;    the mark.
;;  o If you press "Shift" together with the "move-keys", the region
;;    you pass along is activated
;;  o You have the copy, cut and paste functions (as in many other programs)
;;    which will operate on the active region
;;    It was not possible to bind them to C-v, C-x and C-c for obvious
;;    emacs reasons.
;;    They will be bound according to the "old" behavior to S-delete (cut),
;;    S-insert (paste) and C-insert (copy). These keys do the same in many
;;    other programs.
;;

;;; Code:

;; Customization:
(defgroup pc-select nil
  "Emulate pc bindings."
  :prefix "pc-select"
  :group 'emulations)

(define-obsolete-variable-alias 'pc-select-override-scroll-error
                                'scroll-error-top-bottom
                                "24.1")
(defcustom pc-select-override-scroll-error t
  "Non-nil means don't generate error on scrolling past edge of buffer.
This variable applies in PC Selection mode only.
The scroll commands normally generate an error if you try to scroll
past the top or bottom of the buffer.  This is annoying when selecting
text with these commands.  If you set this variable to non-nil, these
errors are suppressed."
  :type 'boolean
  :group 'pc-select)

(defcustom pc-select-selection-keys-only nil
  "Non-nil means only bind the basic selection keys when started.
Other keys that emulate pc-behavior will be untouched.
This gives mostly Emacs-like behavior with only the selection keys enabled."
  :type 'boolean
  :group 'pc-select)

(defcustom pc-select-meta-moves-sexps nil
  "Non-nil means move sexp-wise with Meta key, otherwise move word-wise."
  :type 'boolean
  :group 'pc-select)

(defcustom pc-selection-mode-hook nil
  "The hook to run when PC Selection mode is toggled."
  :type 'hook
  :group 'pc-select)

(defvar pc-select-saved-settings-alist nil
  "The values of the variables before PC Selection mode was toggled on.
When PC Selection mode is toggled on, it sets quite a few variables
for its own purposes.  This alist holds the original values of the
variables PC Selection mode had set, so that these variables can be
restored to their original values when PC Selection mode is toggled off.")

(defvar pc-select-map nil
  "The keymap used as the global map when PC Selection mode is on." )

(defvar pc-select-saved-global-map nil
  "The global map that was in effect when PC Selection mode was toggled on.")

(defvar pc-select-key-bindings-alist nil
  "This alist holds all the key bindings PC Selection mode sets.")

(defvar pc-select-default-key-bindings nil
  "These key bindings always get set by PC Selection mode.")

(defvar pc-select-extra-key-bindings
  ;; The following keybindings are for standard ISO keyboards
  ;; as they are used with IBM compatible PCs, IBM RS/6000,
  ;; MACs, many X-Stations and probably more.
  '(;; Commented out since it's been standard at least since Emacs-21.
    ;;([S-insert]  . yank)
    ;;([C-insert]  . copy-region-as-kill)
    ;;([S-delete]  . kill-region)

    ;; The following bindings are useful on Sun Type 3 keyboards
    ;; They implement the Get-Delete-Put (copy-cut-paste)
    ;; functions from sunview on the L6, L8 and L10 keys
    ;; Sam Steingold <sds@gnu.org> says that f16 is copy and f18 is paste.
    ([f16]  . copy-region-as-kill)
    ([f18]  . yank)
    ([f20]  . kill-region)

    ;; The following bindings are from Pete Forman.
    ([f6] . other-window)		; KNextPane     F6
    ([C-delete] . kill-line)		; KEraseEndLine cDel
    ("\M-\d" . undo)			; KUndo         aBS

    ;; The following binding is taken from pc-mode.el
    ;; as suggested by RMS.
    ;; I only used the one that is not covered above.
    ([C-M-delete]  . kill-sexp)
    ;; Next line proposed by Eli Barzilay
    ([C-escape]    . electric-buffer-list))
  "Key bindings to set only if `pc-select-selection-keys-only' is nil.")

(defvar pc-select-meta-moves-sexps-key-bindings
  '((([M-right]   . forward-sexp)
     ([M-left]    . backward-sexp))
    (([M-right]   . forward-word)
     ([M-left]    . backward-word)))
  "The list of key bindings controlled by `pc-select-meta-moves-sexp'.
The bindings in the car of this list get installed if
`pc-select-meta-moves-sexp' is t, the bindings in the cadr of this
list get installed otherwise.")

;; This is for tty.  We don't turn on normal-erase-is-backspace,
;; but bind keys as pc-selection-mode did before
;; normal-erase-is-backspace was invented, to keep us back
;; compatible.
(defvar pc-select-tty-key-bindings
  '(([delete] . delete-char)		; KDelete       Del
   ([C-backspace] . backward-kill-word))
  "The list of key bindings controlled by `pc-select-selection-keys-only'.
These key bindings get installed when running in a tty, but only if
`pc-select-selection-keys-only' is nil.")

(defvar pc-select-old-M-delete-binding nil
  "Holds the old mapping of [M-delete] in the `function-key-map'.
This variable holds the value associated with [M-delete] in the
`function-key-map' before PC Selection mode had changed that
association.")

;;;;
;; misc
;;;;

(provide 'pc-select)

(defun pc-select-define-keys (alist keymap)
  "Make KEYMAP have the key bindings specified in ALIST."
  (let ((lst alist))
    (while lst
      (define-key keymap (caar lst) (cdar lst))
      (setq lst (cdr lst)))))

(defun pc-select-restore-keys (alist keymap saved-map)
  "Use ALIST to restore key bindings from SAVED-MAP into KEYMAP.
Go through all the key bindings in ALIST, and, for each key
binding, if KEYMAP and ALIST still agree on the key binding,
restore the previous value of that key binding from SAVED-MAP."
  (let ((lst alist))
    (while lst
      (when (equal (lookup-key keymap (caar lst)) (cdar lst))
	(define-key keymap (caar lst) (lookup-key saved-map (caar lst))))
      (setq lst (cdr lst)))))

(defmacro pc-select-add-to-alist (alist var val)
  "Ensure that ALIST contains the cons cell (VAR . VAL).
If a cons cell whose car is VAR is already on the ALIST, update the
cdr of that cell with VAL.  Otherwise, make a new cons cell
\(VAR . VAL), and prepend it onto ALIST."
  (let ((elt (make-symbol "elt")))
    `(let ((,elt (assq ',var ,alist)))
       (if ,elt
	   (setcdr ,elt ,val)
	 (setq ,alist (cons (cons ',var ,val) ,alist))))))

(defmacro pc-select-save-and-set-var (var newval)
  "Set VAR to NEWVAL; save the old value.
The old value is saved on the `pc-select-saved-settings-alist'."
  `(when (boundp ',var)
     (pc-select-add-to-alist pc-select-saved-settings-alist ,var ,var)
     (setq ,var ,newval)))

(defmacro pc-select-save-and-set-mode (mode &optional arg mode-var)
  "Call the function MODE; save the old value of the variable MODE.
MODE is presumed to be a function which turns on a minor mode.  First,
save the value of the variable MODE on `pc-select-saved-settings-alist'.
Then, if ARG is specified, call MODE with ARG, otherwise call it with
nil as an argument.  If MODE-VAR is specified, save the value of the
variable MODE-VAR (instead of the value of the variable MODE) on
`pc-select-saved-settings-alist'."
  (unless mode-var (setq mode-var mode))
  `(when (fboundp ',mode)
     (pc-select-add-to-alist pc-select-saved-settings-alist
			     ,mode-var ,mode-var)
     (,mode ,arg)))

(defmacro pc-select-restore-var (var)
  "Restore the previous value of the variable VAR.
Look up VAR's previous value in `pc-select-saved-settings-alist', and,
if the value is found, set VAR to that value."
  (let ((elt (make-symbol "elt")))
    `(let ((,elt (assq ',var pc-select-saved-settings-alist)))
       (unless (null ,elt)
	 (setq ,var (cdr ,elt))))))

(defmacro pc-select-restore-mode (mode)
  "Restore the previous state (either on or off) of the minor mode MODE.
Look up the value of the variable MODE on `pc-select-saved-settings-alist'.
If the value is non-nil, call the function MODE with an argument of
1, otherwise call it with an argument of -1."
  (let ((elt (make-symbol "elt")))
    `(when (fboundp ',mode)
       (let ((,elt (assq ',mode pc-select-saved-settings-alist)))
	 (unless (null ,elt)
	   (,mode (if (cdr ,elt) 1 -1)))))))


;;;###autoload
(define-minor-mode pc-selection-mode
  "Change mark behavior to emulate Motif, Mac or MS-Windows cut and paste style.

This mode enables Delete Selection mode and Transient Mark mode.

The arrow keys (and others) are bound to new functions
which modify the status of the mark.

The ordinary arrow keys disable the mark.
The shift-arrow keys move, leaving the mark behind.

C-LEFT and C-RIGHT move back or forward one word, disabling the mark.
S-C-LEFT and S-C-RIGHT move back or forward one word, leaving the mark behind.

M-LEFT and M-RIGHT move back or forward one word or sexp, disabling the mark.
S-M-LEFT and S-M-RIGHT move back or forward one word or sexp, leaving the mark
behind.  To control whether these keys move word-wise or sexp-wise set the
variable `pc-select-meta-moves-sexps' after loading pc-select.el but before
turning PC Selection mode on.

C-DOWN and C-UP move back or forward a paragraph, disabling the mark.
S-C-DOWN and S-C-UP move back or forward a paragraph, leaving the mark behind.

HOME moves to beginning of line, disabling the mark.
S-HOME moves to beginning of line, leaving the mark behind.
With Ctrl or Meta, these keys move to beginning of buffer instead.

END moves to end of line, disabling the mark.
S-END moves to end of line, leaving the mark behind.
With Ctrl or Meta, these keys move to end of buffer instead.

PRIOR or PAGE-UP scrolls and disables the mark.
S-PRIOR or S-PAGE-UP scrolls and leaves the mark behind.

S-DELETE kills the region (`kill-region').
S-INSERT yanks text from the kill ring (`yank').
C-INSERT copies the region into the kill ring (`copy-region-as-kill').

In addition, certain other PC bindings are imitated (to avoid this, set
the variable `pc-select-selection-keys-only' to t after loading pc-select.el
but before calling PC Selection mode):

  F6           other-window
  DELETE       delete-char
  C-DELETE     kill-line
  M-DELETE     kill-word
  C-M-DELETE   kill-sexp
  C-BACKSPACE  backward-kill-word
  M-BACKSPACE  undo"
  ;; FIXME: bring pc-bindings-mode here ?
  nil nil nil

  :group 'pc-select
  :global t

  (if pc-selection-mode
      (if (null pc-select-key-bindings-alist)
	  (progn
	    (setq pc-select-saved-global-map (copy-keymap (current-global-map)))
	    (setq pc-select-key-bindings-alist
		  (append pc-select-default-key-bindings
			  (if pc-select-selection-keys-only
			      nil
			    pc-select-extra-key-bindings)
			  (if pc-select-meta-moves-sexps
			      (car pc-select-meta-moves-sexps-key-bindings)
			    (cadr pc-select-meta-moves-sexps-key-bindings))
			  (if  (or pc-select-selection-keys-only
				   (eq window-system 'x)
				   (memq system-name '(ms-dos windows-nt)))
			      nil
			    pc-select-tty-key-bindings)))

	    (pc-select-define-keys pc-select-key-bindings-alist
				   (current-global-map))

	    (unless  (or pc-select-selection-keys-only
			 (eq window-system 'x)
			 (memq system-name '(ms-dos windows-nt)))
	      ;; it is not clear that we need the following line
	      ;; I hope it doesn't do too much harm to leave it in, though...
	      (setq pc-select-old-M-delete-binding
		    (lookup-key function-key-map [M-delete]))
	      (define-key function-key-map  [M-delete] [?\M-d]))

	    (when (and (not pc-select-selection-keys-only)
		       (or (eq window-system 'x)
			   (memq system-name '(ms-dos windows-nt)))
		       (fboundp 'normal-erase-is-backspace-mode))
	      (pc-select-save-and-set-mode normal-erase-is-backspace-mode 1
					   normal-erase-is-backspace))
	    ;; the original author also had this above:
	    ;; (setq-default normal-erase-is-backspace t)
	    ;; However, the documentation for the variable says that
	    ;; "setting it with setq has no effect", so I'm removing it.

	    (pc-select-save-and-set-var highlight-nonselected-windows nil)
	    (pc-select-save-and-set-var transient-mark-mode t)
	    (pc-select-save-and-set-var shift-select-mode t)
	    (pc-select-save-and-set-var mark-even-if-inactive t)
	    (pc-select-save-and-set-mode delete-selection-mode 1))
	;;else
	;; If the user turned on pc-selection-mode a second time
	;; do not clobber the values of the variables that were
	;; saved from before pc-selection mode was activated --
	;; just make sure the values are the way we like them.
	(pc-select-define-keys pc-select-key-bindings-alist
			       (current-global-map))
	(unless  (or pc-select-selection-keys-only
		     (eq window-system 'x)
		     (memq system-name '(ms-dos windows-nt)))
	  ;; it is not clear that we need the following line
	  ;; I hope it doesn't do too much harm to leave it in, though...
	  (define-key function-key-map  [M-delete] [?\M-d]))
	(when (and (not pc-select-selection-keys-only)
		   (or (eq window-system 'x)
		       (memq system-name '(ms-dos windows-nt)))
		   (fboundp 'normal-erase-is-backspace-mode))
	  (normal-erase-is-backspace-mode 1))
	(setq highlight-nonselected-windows nil)
	(setq transient-mark-mode t)
	(setq mark-even-if-inactive t)
	(delete-selection-mode 1))
    ;;else
    (when pc-select-key-bindings-alist
      (when (and (not pc-select-selection-keys-only)
		 (or (eq window-system 'x)
		     (memq system-name '(ms-dos windows-nt))))
	(pc-select-restore-mode normal-erase-is-backspace-mode))

      (pc-select-restore-keys
       pc-select-key-bindings-alist (current-global-map)
       pc-select-saved-global-map)

      (pc-select-restore-var highlight-nonselected-windows)
      (pc-select-restore-var transient-mark-mode)
      (pc-select-restore-var shift-select-mode)
      (pc-select-restore-var mark-even-if-inactive)
      (pc-select-restore-mode delete-selection-mode)
      (and pc-select-old-M-delete-binding
	   (define-key function-key-map [M-delete]
	     pc-select-old-M-delete-binding))
      (setq pc-select-key-bindings-alist nil
	    pc-select-saved-settings-alist nil))))
(make-obsolete 'pc-selection-mode 'delete-selection-mode "24.1")

;;; pc-select.el ends here
