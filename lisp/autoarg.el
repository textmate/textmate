;;; autoarg.el --- make digit keys supply prefix args

;; Copyright (C) 1998, 2000-2012 Free Software Foundation, Inc.

;; Author:  Dave Love <fx@gnu.org>
;; Created: 1998-09-04
;; Keywords: abbrev, emulations

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

;; This provides `autoarg-mode', a global minor mode meant to emulate
;; a facility reported from Twenex Emacs whereby digit keys supplied
;; prefix args rather than self inserting, with a digit sequence
;; terminated by space acting to insert the digits.

;; The bindings of DIGIT and C-DIGIT are swapped and a command bound
;; to SPC deals with a numeric prefix arg or acts normally without
;; such an arg.  (In the absence of a suitable terminal, you'd
;; probably want to swap DIGIT and M-DIGIT.)  See the mode doc.

;; You probably don't really want to use this.

;; Also provides `autoarg-kp-mode' which is similar, but leaves the
;; digit keys alone and redefines the `keypad' keys, `kp-1' &c as
;; digit arguments.  (Use `NumLock' if necessary to generate kp-N.)
;; You're more likely to want to use this.

;;; Code:

(defvar autoarg-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Loop over digit characters to set up keymap.
    (dotimes (i 10)
      (define-key map `[,(+ ?0 i)] 'digit-argument)
      (define-key map `[(control ,(+ ?0 i))] 'self-insert-command))
    (define-key map " " 'autoarg-terminate)
    map)
  "Keymap for Autoarg mode.")

;; Logical additions:
;; (define-key autoarg-mode-map [?-] 'negative-argument)
;; (define-key autoarg-mode-map [(control ?-)] 'self-insert-command)
;; A sensible/addition?
;; (define-key autoarg-mode-map [?\r] 'autoarg-terminate)

(defvar autoarg-kp-digits
  (let (alist)
    (dotimes (i 10 alist)
      (push (cons (intern (format "kp-%d" i)) i) alist))))

(defun autoarg-kp-digit-argument (arg)
  "Part of the numeric argument for the next command, like `digit-argument'."
  (interactive "P")
  (let ((digit (cdr (assq last-command-event autoarg-kp-digits))))
    (cond ((integerp arg)
	   (setq prefix-arg (+ (* arg 10)
			       (if (< arg 0) (- digit) digit))))
	  ((eq arg '-)
	   ;; Treat -0 as just -, so that -01 will work.
	   (setq prefix-arg (if (zerop digit) '- (- digit))))
	  (t
	   (setq prefix-arg digit))))
  (setq universal-argument-num-events (length (this-command-keys)))
  (setq overriding-terminal-local-map universal-argument-map))

(defvar autoarg-kp-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Loop over digit characters to set up keymap.
    (dotimes (i 10)
      (let ((sym (intern (format "kp-%d" i))))
	(define-key map (vector sym) 'autoarg-kp-digit-argument)))
    (define-key map [kp-subtract] 'negative-argument)
    map)
  "Keymap for Autoarg-KP mode.")

;;;###autoload
(define-minor-mode autoarg-mode
  "Toggle Autoarg mode, a global minor mode.
With a prefix argument ARG, enable Autoarg mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

\\<autoarg-mode-map>
In Autoarg mode, digits are bound to `digit-argument', i.e. they
supply prefix arguments as C-DIGIT and M-DIGIT normally do.
Furthermore, C-DIGIT inserts DIGIT.
\\[autoarg-terminate] terminates the prefix sequence and inserts
the digits of the autoarg sequence into the buffer.
Without a numeric prefix arg, the normal binding of \\[autoarg-terminate]
is invoked, i.e. what it would be with Autoarg mode off.

For example:
`6 9 \\[autoarg-terminate]' inserts `69' into the buffer, as does `C-6 C-9'.
`6 9 a' inserts 69 `a's into the buffer.
`6 9 \\[autoarg-terminate] \\[autoarg-terminate]' inserts `69' into the buffer and
then invokes the normal binding of \\[autoarg-terminate].
`C-u \\[autoarg-terminate]' invokes the normal binding of \\[autoarg-terminate] four times.

\\{autoarg-mode-map}"
  nil " Aarg" autoarg-mode-map :global t :group 'keyboard)

;;;###autoload
(define-minor-mode autoarg-kp-mode
  "Toggle Autoarg-KP mode, a global minor mode.
With a prefix argument ARG, enable Autoarg-KP mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

\\<autoarg-kp-mode-map>
This is similar to `autoarg-mode' but rebinds the keypad keys
`kp-1' etc. to supply digit arguments.

\\{autoarg-kp-mode-map}"
  nil " Aakp" autoarg-kp-mode-map :global t :group 'keyboard
  (if autoarg-kp-mode
      (dotimes (i 10)
	(let ((sym (intern (format "kp-%d" i))))
	  (define-key universal-argument-map (vector sym)
	    'autoarg-kp-digit-argument)))
    (dotimes (i 10)
      (let ((sym (intern (format "kp-%d" i))))
	(define-key universal-argument-map (vector sym) nil)))))

(defun autoarg-terminate (n)
  "Maybe terminate a digit prefix sequence.
With a non-negative numeric prefix arg, insert the digits comprising
the arg into the current buffer.  Otherwise use the binding of the key
which invoked this function, excluding the Autoarg keymap."
  (interactive "P")
  (if (numberp n)
      (insert (number-to-string n))
    (let* ((autoarg-mode nil)		; hide the bindings
	   (binding (key-binding (this-single-command-keys))))
      (if binding (call-interactively binding)))))

(provide 'autoarg)

;;; autoarg.el ends here
