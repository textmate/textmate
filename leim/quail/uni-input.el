;;; uni-input.el --- Hex Unicode input method

;; Copyright (C) 2001-2012  Free Software Foundation, Inc.
;; Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Author: Dave Love <fx@gnu.org>
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

;; Provides an input method for entering characters by hex unicode in
;; the form `uxxxx', similarly to the Yudit editor.

;; This is not really a Quail method, but uses some Quail functions.
;; There is probably A Better Way.

;; You can get a similar effect by using C-q with
;; `read-quoted-char-radix' set to 16.

;; Note that this only allows you to enter BMP values unless someone
;; extends it to use variable numbers of digits.

;;; Code:

(require 'quail)

(defun ucs-input-insert-char (char)
  (insert char)
  (move-overlay quail-overlay (overlay-start quail-overlay) (point)))

(defun ucs-input-method (key)
  (if (or buffer-read-only
	  (and (/= key ?U) (/= key ?u)))
      (list key)
    (quail-setup-overlays nil)
    (ucs-input-insert-char key)
    (let ((modified-p (buffer-modified-p))
	  (buffer-undo-list t)
	  (input-method-function nil)
	  (echo-keystrokes 0)
	  (help-char nil)
	  (events (list key))
	  (str "    "))
      (unwind-protect
	  (catch 'non-digit
	    (progn
	      (dotimes (i 4)
		(let ((seq (read-key-sequence nil))
		      key)
		  (if (and (stringp seq)
			   (= 1 (length seq))
			   (setq key (aref seq 0))
			   (memq key '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?a
					  ?b ?c ?d ?e ?f ?A ?B ?C ?D ?E ?F)))
		      (progn
			(push key events)
			(ucs-input-insert-char key))
		    (quail-delete-region)
		    (throw 'non-digit (append (reverse events)
					      (listify-key-sequence seq))))))
	      (quail-delete-region)
	      (let ((n (string-to-number (apply 'string
					    (cdr (nreverse events)))
				     16)))
		(if (characterp n)
		    (list n)))))
	(quail-delete-overlays)
	(set-buffer-modified-p modified-p)
	(run-hooks 'input-method-after-insert-chunk-hook)))))

(defun ucs-input-activate (&optional arg)
  "Activate UCS input method.
With arg, activate UCS input method if and only if arg is positive.

While this input method is active, the variable
`input-method-function' is bound to the function `ucs-input-method'."
  (if (and arg
	  (< (prefix-numeric-value arg) 0))
      (unwind-protect
	  (progn
	    (quail-hide-guidance)
	    (quail-delete-overlays)
	    (setq describe-current-input-method-function nil))
	(kill-local-variable 'input-method-function))
    (setq inactivate-current-input-method-function 'ucs-input-inactivate)
    (setq describe-current-input-method-function 'ucs-input-help)
    (quail-delete-overlays)
    (if (eq (selected-window) (minibuffer-window))
	(add-hook 'minibuffer-exit-hook 'quail-exit-from-minibuffer))
    (set (make-local-variable 'input-method-function)
	 'ucs-input-method)))

(defun ucs-input-inactivate ()
  "Inactivate UCS input method."
  (interactive)
  (ucs-input-activate -1))

(defun ucs-input-help ()
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ "\
Input method: ucs (mode line indicator:U+)

Input as Unicode: U<hex> or u<hex>, where <hex> is a four-digit hex number.")))

;; The file ../leim-ext.el contains the following call.
;; (register-input-method "ucs" "UTF-8" 'ucs-input-activate "U+"
;; 		       "Unicode input as hex in the form Uxxxx.")

(provide 'uni-input)

;;; uni-input.el ends here
