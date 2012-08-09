;;; keypad.el --- simplified keypad bindings

;; Copyright (C) 2002-2012 Free Software Foundation, Inc.

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

;; The keypad package allows easy binding of the keypad keys to
;; various commonly used sets of commands.
;;
;; With the following setup, the keypad can be used for numeric data
;; entry when NumLock is off, and to give numeric prefix arguments to
;; emacs commands, when NumLock is on.
;;
;;  keypad-setup         => Plain Numeric Keypad
;;  keypad-numlock-setup => Prefix numeric args
;;
;;    +--------+--------+--------+
;;    |  M-7   |  M-8   |  M-9   | <- numlock on
;;    |   7    |   8    |   9    | <- numlock off
;;    +--------+--------+--------+
;;    |  M-4   |  M-5   |  M-6   |
;;    |   4    |   5    |   6    |
;;    +--------+--------+--------+
;;    |  M-1   |  M-2   |  M-3   |
;;    |   1    |   2    |   3    |
;;    +--------+--------+--------+
;;    |       M-0       |  M--   |
;;    |        0        |   .    |
;;    +-----------------+--------+

;; The following keypad setup is used for navigation together with
;; modes like cua-mode which uses shifted movement keys to extend the
;; region.
;;
;;  keypad-setup         => Cursor keys
;;  keypad-shifted-setup => Shifted cursor keys
;;
;;    +--------+--------+--------+
;;    | S-home | S-up   | S-PgUp | <- shifted, numlock off
;;    |  Home  |  up    |  PgUp  | <- unshifted, numlock off
;;    +--------+--------+--------+
;;    | S-left |S-space |S-right |
;;    |  left  | space  | right  |
;;    +--------+--------+--------+
;;    | S-end  | S-down | S-PgDn |
;;    |  end   |  down  |  PgDn  |
;;    +--------+--------+--------+
;;    |    S-insert     |S-delete|
;;    |     insert      | delete |
;;    +-----------------+--------+

;; The following setup binds the unshifted keypad keys to plain
;; numeric keys when NumLock is either on or off, but the decimal key
;; produces either a . (NumLock off) or a , (NumLock on).  This is
;; useful for e.g. Danish users where the decimal separator is a
;; comma.
;;
;;  keypad-setup         => Plain Numeric Keypad
;;  keypad-numlock-setup => Numeric Keypad with Decimal key: ,
;;
;;    +--------+--------+--------+
;;    |   7    |   8    |   9    | <- numlock on
;;    |   7    |   8    |   9    | <- numlock off
;;    +--------+--------+--------+
;;    |   4    |   5    |   6    |
;;    |   4    |   5    |   6    |
;;    +--------+--------+--------+
;;    |   1    |   2    |   3    |
;;    |   1    |   2    |   3    |
;;    +--------+--------+--------+
;;    |        0        |   ,    |
;;    |        0        |   .    |
;;    +-----------------+--------+

;;; Code:

(provide 'keypad)

;;; Customization

;;;###autoload
(defcustom keypad-setup nil
  "Specifies the keypad setup for unshifted keypad keys when NumLock is off.
When selecting the plain numeric keypad setup, the character returned by the
decimal key must be specified."
  :set (lambda (symbol value)
	 (if value
	     (keypad-setup value nil nil value)))
  :initialize 'custom-initialize-default
  :link '(emacs-commentary-link "keypad.el")
  :version "22.1"
  :type '(choice (const :tag "Plain numeric keypad" numeric)
		 (character :tag "Numeric Keypad with Decimal Key"
			    :match (lambda (widget value) (integerp value))
			    :value ?.)
		 (const :tag "Numeric prefix arguments" prefix)
		 (const :tag "Cursor keys" cursor)
		 (const :tag "Shifted cursor keys" S-cursor)
		 (const :tag "Unspecified/User-defined" none)
		 (other :tag "Keep existing bindings" nil))
  :require 'keypad
  :group 'keyboard)

;;;###autoload
(defcustom keypad-numlock-setup nil
  "Specifies the keypad setup for unshifted keypad keys when NumLock is on.
When selecting the plain numeric keypad setup, the character returned by the
decimal key must be specified."
  :set (lambda (symbol value)
	 (if value
	     (keypad-setup value t nil value)))
  :initialize 'custom-initialize-default
  :link '(emacs-commentary-link "keypad.el")
  :version "22.1"
  :type '(choice (const :tag "Plain numeric keypad" numeric)
		 (character :tag "Numeric Keypad with Decimal Key"
			    :match (lambda (widget value) (integerp value))
			    :value ?.)
		 (const :tag "Numeric prefix arguments" prefix)
		 (const :tag "Cursor keys" cursor)
		 (const :tag "Shifted cursor keys" S-cursor)
		 (const :tag "Unspecified/User-defined" none)
		 (other :tag "Keep existing bindings" nil))
  :require 'keypad
  :group 'keyboard)

;;;###autoload
(defcustom keypad-shifted-setup nil
  "Specifies the keypad setup for shifted keypad keys when NumLock is off.
When selecting the plain numeric keypad setup, the character returned by the
decimal key must be specified."
  :set (lambda (symbol value)
	 (if value
	     (keypad-setup value nil t value)))
  :initialize 'custom-initialize-default
  :link '(emacs-commentary-link "keypad.el")
  :version "22.1"
  :type '(choice (const :tag "Plain numeric keypad" numeric)
		 (character :tag "Numeric Keypad with Decimal Key"
			    :match (lambda (widget value) (integerp value))
			    :value ?.)
		 (const :tag "Numeric prefix arguments" prefix)
		 (const :tag "Cursor keys" cursor)
		 (const :tag "Shifted cursor keys" S-cursor)
		 (const :tag "Unspecified/User-defined" none)
		 (other :tag "Keep existing bindings" nil))
  :require 'keypad
  :group 'keyboard)

;;;###autoload
(defcustom keypad-numlock-shifted-setup nil
  "Specifies the keypad setup for shifted keypad keys when NumLock is off.
When selecting the plain numeric keypad setup, the character returned by the
decimal key must be specified."
  :set (lambda (symbol value)
	 (if value
	     (keypad-setup value t t value)))
  :initialize 'custom-initialize-default
  :link '(emacs-commentary-link "keypad.el")
  :version "22.1"
  :type '(choice (const :tag "Plain numeric keypad" numeric)
		 (character :tag "Numeric Keypad with Decimal Key"
			    :match (lambda (widget value) (integerp value))
			    :value ?.)
		 (const :tag "Numeric prefix arguments" prefix)
		 (const :tag "Cursor keys" cursor)
		 (const :tag "Shifted cursor keys" S-cursor)
		 (const :tag "Unspecified/User-defined" none)
		 (other :tag "Keep existing bindings" nil))
  :require 'keypad
  :group 'keyboard)


;;;###autoload
(defun keypad-setup (setup &optional numlock shift decimal)
  "Set keypad bindings in `function-key-map' according to SETUP.
If optional second argument NUMLOCK is non-nil, the NumLock On bindings
are changed.  Otherwise, the NumLock Off bindings are changed.
If optional third argument SHIFT is non-nil, the shifted keypad
keys are bound.

 Setup      Binding
 -------------------------------------------------------------
 'prefix   Command prefix argument, i.e.  M-0 .. M-9 and M--
 'S-cursor Bind shifted keypad keys to the shifted cursor movement keys.
 'cursor   Bind keypad keys to the cursor movement keys.
 'numeric  Plain numeric keypad, i.e. 0 .. 9 and .  (or DECIMAL arg)
 'none     Removes all bindings for keypad keys in function-key-map;
           this enables any user-defined bindings for the keypad keys
           in the global and local keymaps.

If SETUP is 'numeric and the optional fourth argument DECIMAL is non-nil,
the decimal key on the keypad is mapped to DECIMAL instead of `.'"
  (let* ((i 0)
	 (var (cond
	       ((and (not numlock) (not shift)) 'keypad-setup)
	       ((and (not numlock) shift) 'keypad-shifted-setup)
	       ((and numlock (not shift)) 'keypad-numlock-setup)
	       ((and numlock shift) 'keypad-numlock-shifted-setup)))
	 (kp (cond
	      ((eq var 'keypad-setup)
	       [kp-delete kp-insert kp-end kp-down kp-next kp-left
			  kp-space kp-right kp-home kp-up kp-prior])
	      ((eq var 'keypad-shifted-setup)
	       [S-kp-decimal S-kp-0 S-kp-1 S-kp-2 S-kp-3 S-kp-4
			     S-kp-5 S-kp-6 S-kp-7 S-kp-8 S-kp-9])
	      ((eq var 'keypad-numlock-setup)
	       [kp-decimal kp-0 kp-1 kp-2 kp-3 kp-4
			   kp-5 kp-6 kp-7 kp-8 kp-9])
	      ((eq var 'keypad-numlock-shifted-setup)
	       [S-kp-delete S-kp-insert S-kp-end S-kp-down S-kp-next S-kp-left
			    S-kp-space S-kp-right S-kp-home S-kp-up S-kp-prior])))
	 (bind
	  (cond
	   ((or (eq setup 'numeric)
		(characterp setup))
	    (if (eq decimal 'numeric)
		(setq decimal nil))
	    (vector (or decimal ?.) ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
	   ((eq setup 'prefix)
	    [?\M-- ?\M-0 ?\M-1 ?\M-2 ?\M-3 ?\M-4
		   ?\M-5 ?\M-6 ?\M-7 ?\M-8 ?\M-9])
	   ((eq setup 'cursor)
	    [delete insert end down next left
		    space right home up prior])
	   ((eq setup 'S-cursor)
	    [S-delete S-insert S-end S-down S-next S-left
		      S-space S-right S-home S-up S-prior])
	   ((eq setup 'none)
	    nil)
	   (t
	    (signal 'error (list "Unknown keypad setup: " setup))))))

    (set var setup)

    ;; Bind the keys in KP list to BIND list in function-key-map.
    ;; If BIND is nil, all bindings for the keys are removed.
    (if (not (boundp 'function-key-map))
	(setq function-key-map (make-sparse-keymap)))

    (while (< i 11)
      (define-key function-key-map (vector (aref kp i))
	(if bind (vector (aref bind i))))
      (if (= i 6)
	  (cond ((eq (aref kp i) 'kp-space)
		 (define-key function-key-map [kp-begin]
		   (if bind (vector (aref bind i)))))
		((eq (aref kp i) 'S-kp-space)
		 (define-key function-key-map [S-kp-begin]
		   (if bind (vector (aref bind i)))))))

      (setq i (1+ i)))))

;;; keypad.el ends here
