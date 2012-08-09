;;; viper-keym.el --- Viper keymaps

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

;; compiler pacifier
(defvar viper-always)
(defvar viper-current-state)
(defvar viper-mode-string)
(defvar viper-expert-level)
(defvar viper-ex-style-editing)
(defvar viper-ex-style-motion)

(eval-and-compile
  (unless (fboundp 'declare-function) (defmacro declare-function (&rest  r))))
;; end pacifier

(require 'viper-util)

(declare-function viper-ex "viper-ex" (arg &optional string))
(declare-function viper-normalize-minor-mode-map-alist "viper-cmd" ())
(declare-function viper-set-mode-vars-for "viper-cmd" (state))

;;; Variables


;;; Emacs keys in other states.

(defcustom viper-want-emacs-keys-in-insert t
  "*Set to nil if you want complete Vi compatibility in insert mode.
Complete compatibility with Vi is not recommended for power use of Viper."
  :type 'boolean
  :group 'viper)

(defcustom viper-want-emacs-keys-in-vi t
  "*Set to nil if you want complete Vi compatibility in Vi mode.
Full Vi compatibility is not recommended for power use of Viper."
  :type 'boolean
  :group 'viper)

(defcustom viper-no-multiple-ESC  t
  "*If true, multiple ESC in Vi mode will cause bell to ring.
This is set to t on a windowing terminal and to 'twice on a dumb
terminal (unless the user level is 1, 2, or 5).  On a dumb terminal, this
enables cursor keys and is generally more convenient, as terminals usually
don't have a convenient Meta key.
Setting viper-no-multiple-ESC to nil will allow as many multiple ESC,
as is allowed by the major mode in effect."
  :type 'boolean
  :group 'viper)

(defcustom viper-want-ctl-h-help nil
  "*If non-nil, C-h gets bound to help-command; otherwise, C-h gets the usual Vi bindings."
  :type 'boolean
  :group 'viper)


;;; Keymaps

;; Keymaps for vital things like \e and C-z.
;; Not for users
(defvar viper-vi-intercept-map (make-sparse-keymap))
(defvar viper-insert-intercept-map (make-sparse-keymap))
(defvar viper-emacs-intercept-map (make-sparse-keymap))

;; keymap used to zap all keymaps other than function-key-map,
;; device-function-key-map, etc.
(defvar viper-overriding-map (make-sparse-keymap))

(viper-deflocalvar viper-vi-local-user-map (make-sparse-keymap)
  "Keymap for user-defined local bindings.
Useful for changing bindings such as ZZ in certain major modes.
For instance, in letter-mode, one may want to bind ZZ to
mh-send-letter.  In a newsreader such as gnus, tin, or rn, ZZ could be bound
to save-buffers-kill-emacs then post article, etc.")
(put 'viper-vi-local-user-map 'permanent-local t)

(defvar viper-vi-global-user-map (make-sparse-keymap)
  "Keymap for user-defined global bindings.
These bindings are seen in all Viper buffers.")

(defvar viper-vi-basic-map (make-keymap)
  "This is the main keymap in effect in Viper's Vi state.
This map is global, shared by all buffers.")

(defvar  viper-vi-kbd-map (make-sparse-keymap)
  "This keymap keeps keyboard macros defined via the :map command.")

(defvar viper-vi-diehard-map (make-sparse-keymap)
  "This keymap is in use when the user asks Viper to simulate Vi very closely.
This happens when viper-expert-level is 1 or 2.  See viper-set-expert-level.")


(viper-deflocalvar viper-insert-local-user-map (make-sparse-keymap)
  "Auxiliary map for per-buffer user-defined keybindings in Insert state.")
(put 'viper-insert-local-user-map 'permanent-local t)

(defvar viper-insert-global-user-map (make-sparse-keymap)
  "Auxiliary map for global user-defined bindings in Insert state.")

(defvar viper-insert-basic-map (make-sparse-keymap)
  "The basic insert-mode keymap.")

(defvar viper-insert-diehard-map (make-keymap)
  "Map used when user wants vi-style keys in insert mode.
Most of the Emacs keys are suppressed.  This map overshadows
viper-insert-basic-map.  Not recommended, except for novice users.")

(defvar  viper-insert-kbd-map  (make-sparse-keymap)
  "This keymap keeps VI-style kbd macros for insert mode.")

(defvar viper-replace-map (make-sparse-keymap)
  "Map used in Viper's replace state.")

(defvar viper-emacs-global-user-map (make-sparse-keymap)
  "Auxiliary map for global user-defined bindings in Emacs state.")

(defvar  viper-emacs-kbd-map  (make-sparse-keymap)
  "This keymap keeps Vi-style kbd macros for Emacs mode.")

(viper-deflocalvar viper-emacs-local-user-map  (make-sparse-keymap)
  "Auxiliary map for local user-defined bindings in Emacs state.")
(put 'viper-emacs-local-user-map 'permanent-local t)

;; This keymap should stay empty
(defvar viper-empty-keymap (make-sparse-keymap))

;; This was the main Vi mode in old versions of VIP which may have been
;; extensively used by VIP users.  We declare it as a global var
;; and, after .viper is loaded, we add this keymap to viper-vi-basic-map.
(defvar viper-mode-map (make-sparse-keymap))

;; Some important keys used in viper
(defcustom viper-toggle-key [(control ?z)]  ; "\C-z"
  "The key used to change states from Emacs to Vi and back.
In insert mode, this key also functions as Meta.

Enter as a sexp.  Examples: \"\\C-z\", [(control ?z)]."
  :type 'sexp
  :group 'viper
  :set (lambda (symbol value)
	 (let ((old-value (if (boundp 'viper-toggle-key)
			      viper-toggle-key
			    [(control ?z)])))
	   (mapc
	    (lambda (buf)
	      (with-current-buffer buf
		(when (and (boundp 'viper-insert-basic-map)
			   (keymapp viper-insert-basic-map))
		  (when old-value
		    (define-key viper-insert-basic-map old-value nil))
		  (define-key viper-insert-basic-map value 'viper-escape-to-vi))
		(when (and (boundp 'viper-vi-intercept-map)
			   (keymapp viper-vi-intercept-map))
		  (when old-value
		    (define-key viper-vi-intercept-map old-value nil))
		  (define-key
		    viper-vi-intercept-map value 'viper-toggle-key-action))
		(when (and (boundp 'viper-emacs-intercept-map)
			   (keymapp viper-emacs-intercept-map))
		  (define-key viper-emacs-intercept-map old-value nil)
		  (define-key
		    viper-emacs-intercept-map value 'viper-change-state-to-vi))
		))
	    (buffer-list))
	   (set-default symbol value)
           )))

(defcustom viper-quoted-insert-key "\C-v"
  "The key used to quote special characters when inserting them in Insert state."
  :type 'string
  :group 'viper)

(defvar viper-ESC-key (kbd "ESC")
  "Key used to ESC.")


;;; Variables used by minor modes

;; Association list of the form
;; ((major-mode . keymap) (major-mode . keymap) ...)
;; Viper uses these keymaps to make user-requested adjustments
;; to its Vi state in various major modes.")
(defvar viper-vi-state-modifier-alist nil)

;; Association list of the form
;; ((major-mode . keymap) (major-mode . keymap) ...)
;; Viper uses these keymaps to make user-requested adjustments
;; to its Insert state in various major modes.")
(defvar viper-insert-state-modifier-alist nil)

;; Association list of the form
;; ((major-mode . keymap) (major-mode . keymap) ...)
;; Viper uses these keymaps to make user-requested adjustments
;; to its Emacs state in various major modes.
(defvar viper-emacs-state-modifier-alist nil)

;; The list of viper keymaps. Set by viper-normalize-minor-mode-map-alist
(viper-deflocalvar viper--key-maps nil)
(viper-deflocalvar viper--intercept-key-maps nil)

;; Tells viper-add-local-keys to create a new viper-vi-local-user-map for new
;; buffers.  Not a user option.
(viper-deflocalvar viper-need-new-vi-local-map t "")
(put 'viper-need-new-vi-local-map  'permanent-local t)

;; Tells viper-add-local-keys to create a new viper-insert-local-user-map for
;; new buffers.  Not a user option.
(viper-deflocalvar viper-need-new-insert-local-map t "")
(put 'viper-need-new-insert-local-map  'permanent-local t)

;; Tells viper-add-local-keys to create a new viper-emacs-local-user-map for
;; new buffers.  Not a user option.
(viper-deflocalvar viper-need-new-emacs-local-map t "")
(put 'viper-need-new-emacs-local-map  'permanent-local t)



;; Insert mode keymap

;; for novice users, pretend you are the real vi.
(define-key viper-insert-diehard-map "\t"   'viper-insert-tab)
(define-key viper-insert-diehard-map "\C-a" 'self-insert-command)
(define-key viper-insert-diehard-map "\C-b" 'self-insert-command)
(define-key viper-insert-diehard-map "\C-c" 'viper-change-state-to-vi)
(define-key viper-insert-diehard-map "\C-e" 'self-insert-command)
(define-key viper-insert-diehard-map "\C-f" 'self-insert-command)
(define-key viper-insert-diehard-map "\C-g" 'self-insert-command)
(define-key viper-insert-diehard-map "\C-i" 'self-insert-command)
(define-key viper-insert-diehard-map "\C-k" 'self-insert-command)
(define-key viper-insert-diehard-map "\C-l" 'self-insert-command)
(define-key viper-insert-diehard-map "\C-n" 'self-insert-command)
(define-key viper-insert-diehard-map "\C-o" 'self-insert-command)
(define-key viper-insert-diehard-map "\C-p" 'self-insert-command)
(define-key viper-insert-diehard-map "\C-q" 'self-insert-command)
(define-key viper-insert-diehard-map "\C-r" 'self-insert-command)
(define-key viper-insert-diehard-map "\C-s" 'self-insert-command)
(define-key viper-insert-diehard-map "\C-u" 'viper-erase-line)
(define-key viper-insert-diehard-map "\C-x" 'self-insert-command)
(define-key viper-insert-diehard-map "\C-y" 'self-insert-command)
(define-key viper-insert-diehard-map "\C-z" 'self-insert-command)
(define-key viper-insert-diehard-map "\C-]" 'self-insert-command)
(define-key viper-insert-diehard-map "\C-_" 'self-insert-command)

(let ((i ?\ ))
  (while (<= i ?~)
    (define-key viper-insert-diehard-map (make-string 1 i) 'self-insert-command)
    (setq i (1+ i))))

;; Insert mode map when user wants emacs style
(define-key viper-insert-basic-map "\C-d" 'viper-backward-indent)
(define-key viper-insert-basic-map "\C-w" 'viper-delete-backward-word)
(define-key viper-insert-basic-map "\C-t" 'viper-forward-indent)
(define-key viper-insert-basic-map viper-quoted-insert-key 'quoted-insert)
(define-key viper-insert-basic-map "\C-?" 'viper-del-backward-char-in-insert)
(define-key viper-insert-basic-map [backspace] 'viper-del-backward-char-in-insert)
(define-key viper-insert-basic-map "\C-\\" 'viper-alternate-Meta-key)
(define-key viper-insert-basic-map viper-toggle-key 'viper-escape-to-vi)
(define-key viper-insert-basic-map "\C-c\M-p"
  'viper-insert-prev-from-insertion-ring)
(define-key viper-insert-basic-map "\C-c\M-n"
  'viper-insert-next-from-insertion-ring)


;; Replace keymap
(define-key viper-replace-map "\C-t" 'viper-forward-indent)
(define-key viper-replace-map "\C-j" 'viper-replace-state-carriage-return)
(define-key viper-replace-map "\C-m" 'viper-replace-state-carriage-return)
(define-key viper-replace-map "\C-?" 'viper-del-backward-char-in-replace)
(define-key viper-replace-map [backspace] 'viper-del-backward-char-in-replace)



;; Vi keymaps

(define-key viper-vi-basic-map "\C-^" (lambda ()
					(interactive) (viper-ex nil "e#")))
(define-key viper-vi-basic-map "\C-b" 'viper-scroll-screen-back)
(define-key viper-vi-basic-map "\C-d" 'viper-scroll-up)
(define-key viper-vi-basic-map "\C-e" 'viper-scroll-up-one)
(define-key viper-vi-basic-map "\C-f" 'viper-scroll-screen)
(define-key viper-vi-basic-map "\C-m" 'viper-next-line-at-bol)
(define-key viper-vi-basic-map "\C-u" 'viper-scroll-down)
(define-key viper-vi-basic-map "\C-y" 'viper-scroll-down-one)
;;(define-key viper-vi-basic-map "\C-s" 'viper-isearch-forward)
;;(define-key viper-vi-basic-map "\C-r" 'viper-isearch-backward)
(define-key viper-vi-basic-map "\C-c/" 'viper-toggle-search-style)
(define-key viper-vi-basic-map "\C-c\C-g" 'viper-info-on-file)

(define-key viper-vi-basic-map "\C-c\M-p" 'viper-prev-destructive-command)
(define-key viper-vi-basic-map "\C-c\M-n" 'viper-next-destructive-command)


(define-key viper-vi-basic-map " " 'viper-forward-char)
(define-key viper-vi-basic-map "!" 'viper-command-argument)
(define-key viper-vi-basic-map "\"" 'viper-command-argument)
(define-key viper-vi-basic-map "#" 'viper-command-argument)
(define-key viper-vi-basic-map "$" 'viper-goto-eol)
(define-key viper-vi-basic-map "%" 'viper-paren-match)
(define-key viper-vi-basic-map "&" (lambda ()
				     (interactive) (viper-ex nil "&")))
(define-key viper-vi-basic-map "'" 'viper-goto-mark-and-skip-white)
(define-key viper-vi-basic-map "(" 'viper-backward-sentence)
(define-key viper-vi-basic-map ")" 'viper-forward-sentence)
(define-key viper-vi-basic-map "*" 'call-last-kbd-macro)
(define-key viper-vi-basic-map "+" 'viper-next-line-at-bol)
(define-key viper-vi-basic-map "," 'viper-repeat-find-opposite)
(define-key viper-vi-basic-map "-" 'viper-previous-line-at-bol)
(define-key viper-vi-basic-map "." 'viper-repeat)
(define-key viper-vi-basic-map "/" 'viper-search-forward)

(define-key viper-vi-basic-map "0" 'viper-beginning-of-line)
(define-key viper-vi-basic-map "1" 'viper-digit-argument)
(define-key viper-vi-basic-map "2" 'viper-digit-argument)
(define-key viper-vi-basic-map "3" 'viper-digit-argument)
(define-key viper-vi-basic-map "4" 'viper-digit-argument)
(define-key viper-vi-basic-map "5" 'viper-digit-argument)
(define-key viper-vi-basic-map "6" 'viper-digit-argument)
(define-key viper-vi-basic-map "7" 'viper-digit-argument)
(define-key viper-vi-basic-map "8" 'viper-digit-argument)
(define-key viper-vi-basic-map "9" 'viper-digit-argument)

(define-key viper-vi-basic-map ":" 'viper-ex)
(define-key viper-vi-basic-map ";" 'viper-repeat-find)
(define-key viper-vi-basic-map "<" 'viper-command-argument)
(define-key viper-vi-basic-map "=" 'viper-command-argument)
(define-key viper-vi-basic-map ">" 'viper-command-argument)
(define-key viper-vi-basic-map "?" 'viper-search-backward)
(define-key viper-vi-basic-map "@" 'viper-register-macro)

(define-key viper-vi-basic-map "A" 'viper-Append)
(define-key viper-vi-basic-map "B" 'viper-backward-Word)
(define-key viper-vi-basic-map "C" 'viper-change-to-eol)
(define-key viper-vi-basic-map "D" 'viper-kill-line)
(define-key viper-vi-basic-map "E" 'viper-end-of-Word)
(define-key viper-vi-basic-map "F" 'viper-find-char-backward)
(define-key viper-vi-basic-map "G" 'viper-goto-line)
(define-key viper-vi-basic-map "H" 'viper-window-top)
(define-key viper-vi-basic-map "I" 'viper-Insert)
(define-key viper-vi-basic-map "J" 'viper-join-lines)
(define-key viper-vi-basic-map "K" 'viper-nil)
(define-key viper-vi-basic-map "L" 'viper-window-bottom)
(define-key viper-vi-basic-map "M" 'viper-window-middle)
(define-key viper-vi-basic-map "N" 'viper-search-Next)
(define-key viper-vi-basic-map "O" 'viper-Open-line)
(define-key viper-vi-basic-map "P" 'viper-Put-back)
(define-key viper-vi-basic-map "Q" 'viper-query-replace)
(define-key viper-vi-basic-map "R" 'viper-overwrite)
(define-key viper-vi-basic-map "S" 'viper-substitute-line)
(define-key viper-vi-basic-map "T" 'viper-goto-char-backward)
(define-key viper-vi-basic-map "U" 'viper-undo)
(define-key viper-vi-basic-map "V" 'find-file-other-window)
(define-key viper-vi-basic-map "W" 'viper-forward-Word)
(define-key viper-vi-basic-map "X" 'viper-delete-backward-char)
(define-key viper-vi-basic-map "Y" 'viper-yank-line)
(define-key viper-vi-basic-map "ZZ" 'viper-save-kill-buffer)

(define-key viper-vi-basic-map "\\" 'viper-escape-to-emacs)
(define-key viper-vi-basic-map "[" 'viper-brac-function)
(define-key viper-vi-basic-map "]" 'viper-ket-function)
(define-key viper-vi-basic-map "\C-\\" 'viper-alternate-Meta-key)
(define-key viper-vi-basic-map "^" 'viper-bol-and-skip-white)
(define-key viper-vi-basic-map "`" 'viper-goto-mark)

(define-key viper-vi-basic-map "a" 'viper-append)
(define-key viper-vi-basic-map "b" 'viper-backward-word)
(define-key viper-vi-basic-map "c" 'viper-command-argument)
(define-key viper-vi-basic-map "d" 'viper-command-argument)
(define-key viper-vi-basic-map "e" 'viper-end-of-word)
(define-key viper-vi-basic-map "f" 'viper-find-char-forward)
(define-key viper-vi-basic-map "g" 'viper-nil)
(define-key viper-vi-basic-map "h" 'viper-backward-char)
(define-key viper-vi-basic-map [backspace] 'viper-backward-char)
(define-key viper-vi-basic-map "i" 'viper-insert)
(define-key viper-vi-basic-map "j" 'viper-next-line)
(define-key viper-vi-basic-map "k" 'viper-previous-line)
(define-key viper-vi-basic-map "l" 'viper-forward-char)
(define-key viper-vi-basic-map "m" 'viper-mark-point)
(define-key viper-vi-basic-map "n" 'viper-search-next)
(define-key viper-vi-basic-map "o" 'viper-open-line)
(define-key viper-vi-basic-map "p" 'viper-put-back)
(define-key viper-vi-basic-map "q" 'viper-nil)
(define-key viper-vi-basic-map "r" 'viper-replace-char)
(define-key viper-vi-basic-map "s" 'viper-substitute)
(define-key viper-vi-basic-map "t" 'viper-goto-char-forward)
(define-key viper-vi-basic-map "u" 'viper-undo)
(define-key viper-vi-basic-map "v" 'find-file)
(define-key viper-vi-basic-map "\C-v" 'find-file-other-frame)
(define-key viper-vi-basic-map "w" 'viper-forward-word)
(define-key viper-vi-basic-map "x" 'viper-delete-char)
(define-key viper-vi-basic-map "y" 'viper-command-argument)
(define-key viper-vi-basic-map "zH" 'viper-line-to-top)
(define-key viper-vi-basic-map "zM" 'viper-line-to-middle)
(define-key viper-vi-basic-map "zL" 'viper-line-to-bottom)
(define-key viper-vi-basic-map "z\C-m" 'viper-line-to-top)
(define-key viper-vi-basic-map "z." 'viper-line-to-middle)
(define-key viper-vi-basic-map "z-" 'viper-line-to-bottom)

(define-key viper-vi-basic-map "{" 'viper-backward-paragraph)
(define-key viper-vi-basic-map "|" 'viper-goto-col)
(define-key viper-vi-basic-map "}" 'viper-forward-paragraph)
(define-key viper-vi-basic-map "~" 'viper-toggle-case)
(define-key viper-vi-basic-map "\C-?" 'viper-backward-char)
(define-key viper-vi-basic-map "_" 'viper-nil)

;;; This is viper-vi-diehard-map.  Used when viper-vi-diehard-minor-mode is on.

(define-key viper-vi-diehard-map "\C-a" 'viper-nil)
(define-key viper-vi-diehard-map "\C-c" 'viper-nil)
(define-key viper-vi-diehard-map "\C-g" 'viper-info-on-file)
(define-key viper-vi-diehard-map "\C-i" 'viper-nil)
(define-key viper-vi-diehard-map "\C-k" 'viper-nil)
(define-key viper-vi-diehard-map "\C-l" 'redraw-display)
(define-key viper-vi-diehard-map "\C-n" 'viper-next-line)
(define-key viper-vi-diehard-map "\C-o" 'viper-nil)
(define-key viper-vi-diehard-map "\C-p" 'viper-previous-line)
(define-key viper-vi-diehard-map "\C-q" 'viper-nil)
(define-key viper-vi-diehard-map "\C-r" 'redraw-display)
(define-key viper-vi-diehard-map "\C-s" 'viper-nil)
(define-key viper-vi-diehard-map "\C-t" 'viper-nil)
(define-key viper-vi-diehard-map "\C-v" 'viper-nil)
(define-key viper-vi-diehard-map "\C-w" 'viper-nil)
(define-key viper-vi-diehard-map "@" 'viper-nil)
(define-key viper-vi-diehard-map "_" 'viper-nil)
(define-key viper-vi-diehard-map "*" 'viper-nil)
(define-key viper-vi-diehard-map "#" 'viper-nil)
(define-key viper-vi-diehard-map "\C-_" 'viper-nil)
(define-key viper-vi-diehard-map "\C-]" 'viper-nil) ; This is actually tags.


;;; Minibuffer keymap


(defvar viper-minibuffer-map (make-sparse-keymap)
  "Keymap used to modify keys when Minibuffer is in Insert state.")

(define-key viper-minibuffer-map "\C-m" 'viper-exit-minibuffer)
(define-key viper-minibuffer-map "\C-j" 'viper-exit-minibuffer)

;; Map used to read Ex-style commands.
(defvar viper-ex-cmd-map (make-sparse-keymap))
(define-key viper-ex-cmd-map " "  'ex-cmd-read-exit)
(define-key viper-ex-cmd-map "\t" 'ex-cmd-complete)

;; Keymap for reading file names in Ex-style commands.
(defvar ex-read-filename-map (make-sparse-keymap))
(define-key ex-read-filename-map " " 'viper-complete-filename-or-exit)
(define-key ex-read-filename-map "!" 'viper-handle-!)

;; Some other maps
(defvar viper-slash-and-colon-map (make-sparse-keymap)
  "This map redefines `/' and `:' to behave as in Vi.
Useful in some modes, such as Gnus, MH, etc.")
(define-key viper-slash-and-colon-map ":" 'viper-ex)
(define-key viper-slash-and-colon-map "/" 'viper-search-forward)

(defvar viper-comint-mode-modifier-map (make-sparse-keymap)
  "This map modifies comint mode.")
(define-key viper-comint-mode-modifier-map "\C-m" 'viper-exec-key-in-emacs)
(define-key viper-comint-mode-modifier-map "\C-d" 'viper-exec-key-in-emacs)

(defvar viper-dired-modifier-map (make-sparse-keymap)
  "This map modifies Dired behavior.")
(define-key viper-dired-modifier-map ":" 'viper-ex)
(define-key viper-dired-modifier-map "/" 'viper-search-forward)

(defvar viper-gnus-modifier-map (make-sparse-keymap)
  "This map modifies Gnus behavior.")
(define-key viper-gnus-modifier-map ":" 'viper-ex)



;;; Code

(defun viper-add-local-keys (state alist)
  "Override some vi-state or insert-state bindings in the current buffer.
The effect is seen in the current buffer only.
Useful for customizing  mailer buffers, gnus, etc.
STATE is 'vi-state, 'insert-state, or 'emacs-state
ALIST is of the form ((key . func) (key . func) ...)
Normally, this would be called from a hook to a major mode or
on a per buffer basis.
Usage:
      (viper-add-local-keys state '((key-str . func) (key-str . func)...))   "

  (let (map)
    (cond ((eq state 'vi-state)
	   (if viper-need-new-vi-local-map
	       (setq viper-vi-local-user-map (make-sparse-keymap)))
	   (setq viper-need-new-vi-local-map nil
		 map viper-vi-local-user-map))
	  ((eq state 'insert-state)
	   (if viper-need-new-insert-local-map
	       (setq viper-insert-local-user-map (make-sparse-keymap)))
	   (setq viper-need-new-insert-local-map nil
		 map viper-insert-local-user-map))
	  ((eq state 'emacs-state)
	   (if viper-need-new-emacs-local-map
	       (setq viper-emacs-local-user-map (make-sparse-keymap)))
	   (setq viper-need-new-emacs-local-map nil
		 map viper-emacs-local-user-map))
	  (t
	   (error
	    "Invalid state in viper-add-local-keys: %S.  Valid states: vi-state, insert-state or emacs-state" state)))

    (viper-modify-keymap map alist)
    (viper-normalize-minor-mode-map-alist)
    (viper-set-mode-vars-for viper-current-state)))

(defun viper-zap-local-keys ()
  "Unconditionally reset Viper viper-*-local-user-map's.
Rarely useful, but if you made a mistake by switching to a mode that adds
undesirable local keys, e.g., comint-mode, then this function can restore
sanity."
  (interactive)
  (setq viper-vi-local-user-map (make-sparse-keymap)
	viper-need-new-vi-local-map nil
	viper-insert-local-user-map (make-sparse-keymap)
	viper-need-new-insert-local-map nil
	viper-emacs-local-user-map (make-sparse-keymap)
	viper-need-new-emacs-local-map nil)
  (viper-normalize-minor-mode-map-alist))


(defun viper-modify-major-mode (mode state keymap)
  "Modify key bindings in a major-mode in a Viper state using a keymap.

If the default for a major mode is emacs-state, then modifications to this
major mode may not take effect until the buffer switches state to Vi,
Insert or Emacs.  If this happens, add viper-change-state-to-emacs to this
major mode's hook.  If no such hook exists, you may have to put an advice on
the function that invokes the major mode.  See viper-set-hooks for hints.

The above needs not to be done for major modes that come up in Vi or Insert
state by default.

Arguments: (major-mode viper-state keymap)"
  (let ((alist
	 (cond ((eq state 'vi-state) 'viper-vi-state-modifier-alist)
	       ((eq state 'insert-state) 'viper-insert-state-modifier-alist)
	       ((eq state 'emacs-state) 'viper-emacs-state-modifier-alist)))
	elt)
    (if (setq elt (assoc mode (eval alist)))
	(set alist (delq elt (eval alist))))
    (set alist (cons (cons mode keymap) (eval alist)))

    ;; Normalization usually doesn't help here, since one needs to
    ;; normalize in the actual buffer where changes to the keymap are
    ;; to take place.  However, it doesn't hurt, and it helps whenever this
    ;; function is actually called from within the affected buffer.
    (viper-normalize-minor-mode-map-alist)

    (viper-set-mode-vars-for viper-current-state)))


;; Displays variables that control Viper's keymaps
(defun viper-debug-keymaps ()
  (interactive)
  (with-output-to-temp-buffer " *viper-debug*"
    (princ (format "Buffer name:  %s\n\n" (buffer-name)))
    (princ "Variables:  \n")
    (princ (format "major-mode:  %S\n" major-mode))
    (princ (format "viper-current-state:  %S\n" viper-current-state))
    (princ (format "viper-mode-string:  %S\n\n" viper-mode-string))
    (princ (format "viper-vi-intercept-minor-mode:  %S\n"
		   viper-vi-intercept-minor-mode))
    (princ (format "viper-insert-intercept-minor-mode:  %S\n"
		   viper-insert-intercept-minor-mode))
    (princ (format "viper-emacs-intercept-minor-mode:  %S\n"
		   viper-emacs-intercept-minor-mode))
    (princ (format "viper-vi-minibuffer-minor-mode:  %S\n"
		   viper-vi-minibuffer-minor-mode))
    (princ (format "viper-insert-minibuffer-minor-mode:  %S\n\n"
		   viper-insert-minibuffer-minor-mode))
    (princ (format "viper-vi-local-user-minor-mode:  %S\n"
		   viper-vi-local-user-minor-mode))
    (princ (format "viper-vi-global-user-minor-mode:  %S\n"
		   viper-vi-global-user-minor-mode))
    (princ (format "viper-vi-kbd-minor-mode:  %S\n" viper-vi-kbd-minor-mode))
    (princ (format "viper-vi-state-modifier-minor-mode:  %S\n"
		   viper-vi-state-modifier-minor-mode))
    (princ (format "viper-vi-diehard-minor-mode:  %S\n"
		   viper-vi-diehard-minor-mode))
    (princ (format "viper-vi-basic-minor-mode:  %S\n" viper-vi-basic-minor-mode))
    (princ (format "viper-replace-minor-mode:  %S\n" viper-replace-minor-mode))
    (princ (format "viper-insert-local-user-minor-mode:  %S\n"
		   viper-insert-local-user-minor-mode))
    (princ (format "viper-insert-global-user-minor-mode:  %S\n"
		   viper-insert-global-user-minor-mode))
    (princ (format "viper-insert-kbd-minor-mode:  %S\n"
		   viper-insert-kbd-minor-mode))
    (princ (format "viper-insert-state-modifier-minor-mode:  %S\n"
		   viper-insert-state-modifier-minor-mode))
    (princ (format "viper-insert-diehard-minor-mode:  %S\n"
		   viper-insert-diehard-minor-mode))
    (princ (format "viper-insert-basic-minor-mode:  %S\n"
		   viper-insert-basic-minor-mode))
    (princ (format "viper-emacs-local-user-minor-mode:  %S\n"
		   viper-emacs-local-user-minor-mode))
    (princ (format "viper-emacs-kbd-minor-mode:  %S\n"
		   viper-emacs-kbd-minor-mode))
    (princ (format "viper-emacs-global-user-minor-mode:  %S\n"
		   viper-emacs-global-user-minor-mode))
    (princ (format "viper-emacs-state-modifier-minor-mode:  %S\n"
		   viper-emacs-state-modifier-minor-mode))

    (princ (format "\nviper-expert-level  %S\n" viper-expert-level))
    (princ (format "viper-no-multiple-ESC  %S\n" viper-no-multiple-ESC))
    (princ (format "viper-always  %S\n" viper-always))
    (princ (format "viper-ex-style-motion  %S\n"
		   viper-ex-style-motion))
    (princ (format "viper-ex-style-editing  %S\n"
		   viper-ex-style-editing))
    (princ (format "viper-want-emacs-keys-in-vi  %S\n"
		   viper-want-emacs-keys-in-vi))
    (princ (format "viper-want-emacs-keys-in-insert  %S\n"
		   viper-want-emacs-keys-in-insert))
    (princ (format "viper-want-ctl-h-help  %S\n" viper-want-ctl-h-help))

    (princ "\n\n\n")
    (princ (format "Default value for minor-mode-map-alist:  \n%S\n\n"
		   (default-value 'minor-mode-map-alist)))
    (princ (format "Actual value for minor-mode-map-alist:  \n%S\n"
		   minor-mode-map-alist))
    ))


;;; Keymap utils

(defun viper-add-keymap (mapsrc mapdst)
  "Add contents of mapsrc to mapdst.  It is assumed that mapsrc is sparse."
  (if (featurep 'xemacs)
      ;; Emacs 22 has map-keymap.
      (map-keymap (lambda (key binding) (define-key mapdst key binding))
		  mapsrc)
    (mapc (lambda (p) (define-key mapdst (vector (car p)) (cdr p)))
	  (cdr mapsrc))))

(defun viper-modify-keymap (map alist)
   "Modifies MAP with bindings specified in the ALIST.  The alist has the
form ((key . function) (key . function) ... )."
   (mapcar (lambda (p) (define-key map (eval (car p)) (cdr p)))
	   alist))


(provide 'viper-keym)


;; Local Variables:
;; eval: (put 'viper-deflocalvar 'lisp-indent-hook 'defun)
;; End:


;;; viper-keym.el ends here
