;;; double.el --- support for keyboard remapping with double clicking

;; Copyright (C) 1994, 1997-1998, 2001-2012 Free Software Foundation, Inc.

;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Keywords: i18n

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

;; This mode is intended for use with languages that adds a small
;; number of extra letters not available on the keyboard.
;;
;; Examples includes Scandinavian and German with an US keyboard.
;;
;; The idea is that certain keys are overloaded.  When you press it
;; once it will insert one string, and when you press it twice the
;; string will be replaced by another.  This can be used for mapping
;; keys on a US keyboard to generate characters according to the local
;; keyboard convention when pressed once, and according to US keyboard
;; convention when pressed twice.
;;
;; To use this mode, you must define the variable `double-map' and
;; then enable double mode with `M-x double-mode'.  Read the
;; documentation for both of them.
;;
;; The default mapping is for getting Danish/Norwegian keyboard layout
;; using ISO Latin 1 on a US keyboard.
;;
;; Important node: While I would like to hear comments, bug reports,
;; suggestions, please do @strong{not} expect me to put other mappings
;; than the default into this file.  There are billions and billions
;; of such mappings, and just supporting the most common would
;; increase the size of this nice small file manyfold.

;;; Code:

(defgroup double nil
  "Remap keyboard, but get original by typing the same key twice."
  :group 'i18n)

(defcustom double-map
  '((?\; "\346" ";")
    (?\' "\370" "'")
    (?\[ "\345" "[")
    (?\: "\306" ":")
    (?\" "\330" "\"")
    (?\{ "\305" "{"))
  "Alist of key translations activated by double mode.

Each entry is a list with three elements:
1. The key activating the translation.
2. The string to be inserted when the key is pressed once.
3. The string to be inserted when the key is pressed twice."
  :group 'double
  :type '(repeat (list (character :tag "Key")
		       (string :tag "Once")
		       (string :tag "Twice"))))

(defcustom double-prefix-only t
  "Non-nil means that Double mode mapping only works for prefix keys.
That is, for any key `X' in `double-map', `X' alone will be mapped
but not `C-u X' or `ESC X' since the X is not the prefix key."
  :group 'double
  :type 'boolean)

;;; Read Event

(defvar double-last-event nil)
;; The last key that generated a double key event.

(defun double-read-event (prompt)
  ;; Read an event
  (if isearch-mode (isearch-update))
  (if prompt
      (prog2 (message "%s%c" prompt double-last-event)
	  (read-event)
	(message ""))
    (read-event)))

(global-set-key [ignore] 'ignore)

(or (boundp 'isearch-mode-map)
    (load-library "isearch"))

(define-key isearch-mode-map [ignore]
  (function (lambda () (interactive) (isearch-update))))

(defun double-translate-key (prompt)
  ;; Translate input events using double map.
  (let ((key last-input-event))
    (cond (unread-command-events
	   ;; Artificial event, ignore it.
	   (vector key))
	  ((and double-prefix-only
		(> (length (this-command-keys)) 1))
	   ;; This is not a prefix key, ignore it.
	   (vector key))
	  ((eq key 'magic-start)
	   ;; End of generated event.  See if he will repeat it...
	   (let ((new (double-read-event prompt))
		 (entry (assoc double-last-event double-map)))
	     (force-window-update (selected-window))
	     (if (eq new double-last-event)
		 (progn
		   (setq unread-command-events
			 (append (make-list (1- (length (nth 1 entry)))
					    127)
				 (nth 2 entry)
				 '(magic-end)))
		   (vector 127))
	       (setq unread-command-events (list new))
	       [ignore])))
	  ((eq key 'magic-end)
	   ;; End of double event.  Ignore.
	   [ignore])
	  (t
	   ;; New key.
	   (let ((exp (nth 1 (assoc key double-map))))
	     (setq double-last-event key)
	     (setq unread-command-events
		   (append (substring exp 1) '(magic-start)))
	     (vector (aref exp 0)))))))

;;; Mode

;; This feature seemed useless and it confused describe-mode,
;; so I deleted it.
;; (defvar double-mode-name "Double")
;; ;; Name of current double mode.
;; (make-variable-buffer-local 'double-mode-name)

;;;###autoload
(define-minor-mode double-mode
  "Toggle special insertion on double keypresses (Double mode).
With a prefix argument ARG, enable Double mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

When Double mode is enabled, some keys will insert different
strings when pressed twice.  See `double-map' for details."
  :lighter " Double"
  :link '(emacs-commentary-link "double")
  (kill-local-variable 'key-translation-map)
  (when double-mode
    ;; Set up key-translation-map as indicated by `double-map'.
    ;; XXX I don't think key-translation-map should be made local here. -- Lorentey
    (make-local-variable 'key-translation-map)
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map key-translation-map)
      (setq key-translation-map map)
      (dolist (entry (append double-map '((magic-start) (magic-end))))
        (define-key map
          (vector (nth 0 entry)) 'double-translate-key)))))

(provide 'double)

;;; double.el ends here
