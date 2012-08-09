;;; button.el --- clickable buttons
;;
;; Copyright (C) 2001-2012  Free Software Foundation, Inc.
;;
;; Author: Miles Bader <miles@gnu.org>
;; Keywords: extensions
;; Package: emacs
;;
;; This file is part of GNU Emacs.
;;
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
;;
;; This package defines functions for inserting and manipulating
;; clickable buttons in Emacs buffers, such as might be used for help
;; hyperlinks, etc.
;;
;; In some ways it duplicates functionality also offered by the
;; `widget' package, but the button package has the advantage that it
;; is (1) much faster, (2) much smaller, and (3) much, much, simpler
;; (the code, that is, not the interface).
;;
;; Buttons can either use overlays, in which case the button is
;; represented by the overlay itself, or text-properties, in which case
;; the button is represented by a marker or buffer-position pointing
;; somewhere in the button.  In the latter case, no markers into the
;; buffer are retained, which is important for speed if there are are
;; extremely large numbers of buttons.  Note however that if there is
;; an existing face text-property at the site of the button, the
;; button face may not be visible.  Using overlays avoids this.
;;
;; Using `define-button-type' to define default properties for buttons
;; is not necessary, but it is encouraged, since doing so makes the
;; resulting code clearer and more efficient.
;;

;;; Code:


;; Globals

;; Use color for the MS-DOS port because it doesn't support underline.
;; FIXME if MS-DOS correctly answers the (supports) question, it need
;; no longer be a special case.
(defface button '((t :inherit link))
  "Default face used for buttons."
  :group 'basic-faces)

(defvar button-map
  (let ((map (make-sparse-keymap)))
    ;; The following definition needs to avoid using escape sequences that
    ;; might get converted to ^M when building loaddefs.el
    (define-key map [(control ?m)] 'push-button)
    (define-key map [mouse-2] 'push-button)
    map)
  "Keymap used by buttons.")

(defvar button-buffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\t] 'forward-button)
    (define-key map "\e\t" 'backward-button)
    (define-key map [backtab] 'backward-button)
    map)
  "Keymap useful for buffers containing buttons.
Mode-specific keymaps may want to use this as their parent keymap.")

;; Default properties for buttons
(put 'default-button 'face 'button)
(put 'default-button 'mouse-face 'highlight)
(put 'default-button 'keymap button-map)
(put 'default-button 'type 'button)
;; action may be either a function to call, or a marker to go to
(put 'default-button 'action 'ignore)
(put 'default-button 'help-echo (purecopy "mouse-2, RET: Push this button"))
;; Make overlay buttons go away if their underlying text is deleted.
(put 'default-button 'evaporate t)
;; Prevent insertions adjacent to the text-property buttons from
;; inheriting its properties.
(put 'default-button 'rear-nonsticky t)

;; A `category-symbol' property for the default button type
(put 'button 'button-category-symbol 'default-button)


;; Button types (which can be used to hold default properties for buttons)

;; Because button-type properties are inherited by buttons using the
;; special `category' property (implemented by both overlays and
;; text-properties), we need to store them on a symbol to which the
;; `category' properties can point.  Instead of using the symbol that's
;; the name of each button-type, however, we use a separate symbol (with
;; `-button' appended, and uninterned) to store the properties.  This is
;; to avoid name clashes.

;; [this is an internal function]
(defsubst button-category-symbol (type)
  "Return the symbol used by button-type TYPE to store properties.
Buttons inherit them by setting their `category' property to that symbol."
  (or (get type 'button-category-symbol)
      (error "Unknown button type `%s'" type)))

(defun define-button-type (name &rest properties)
  "Define a `button type' called NAME (a symbol).
The remaining arguments form a sequence of PROPERTY VALUE pairs,
specifying properties to use as defaults for buttons with this type
\(a button's type may be set by giving it a `type' property when
creating the button, using the :type keyword argument).

In addition, the keyword argument :supertype may be used to specify a
button-type from which NAME inherits its default property values
\(however, the inheritance happens only when NAME is defined; subsequent
changes to a supertype are not reflected in its subtypes)."
  (let ((catsym (make-symbol (concat (symbol-name name) "-button")))
	(super-catsym
	 (button-category-symbol
	  (or (plist-get properties 'supertype)
	      (plist-get properties :supertype)
	      'button))))
    ;; Provide a link so that it's easy to find the real symbol.
    (put name 'button-category-symbol catsym)
    ;; Initialize NAME's properties using the global defaults.
    (let ((default-props (symbol-plist super-catsym)))
      (while default-props
	(put catsym (pop default-props) (pop default-props))))
    ;; Add NAME as the `type' property, which will then be returned as
    ;; the type property of individual buttons.
    (put catsym 'type name)
    ;; Add the properties in PROPERTIES to the real symbol.
    (while properties
      (let ((prop (pop properties)))
	(when (eq prop :supertype)
	  (setq prop 'supertype))
	(put catsym prop (pop properties))))
    ;; Make sure there's a `supertype' property
    (unless (get catsym 'supertype)
      (put catsym 'supertype 'button))
    name))

(defun button-type-put (type prop val)
  "Set the button-type TYPE's PROP property to VAL."
  (put (button-category-symbol type) prop val))

(defun button-type-get (type prop)
  "Get the property of button-type TYPE named PROP."
  (get (button-category-symbol type) prop))

(defun button-type-subtype-p (type supertype)
  "Return t if button-type TYPE is a subtype of SUPERTYPE."
  (or (eq type supertype)
      (and type
	   (button-type-subtype-p (button-type-get type 'supertype)
				  supertype))))


;; Button properties and other attributes

(defun button-start (button)
  "Return the position at which BUTTON starts."
  (if (overlayp button)
      (overlay-start button)
    ;; Must be a text-property button.
    (or (previous-single-property-change (1+ button) 'button)
	(point-min))))

(defun button-end (button)
  "Return the position at which BUTTON ends."
  (if (overlayp button)
      (overlay-end button)
    ;; Must be a text-property button.
    (or (next-single-property-change button 'button)
	(point-max))))

(defun button-get (button prop)
  "Get the property of button BUTTON named PROP."
  (if (overlayp button)
      (overlay-get button prop)
    ;; Must be a text-property button.
    (get-text-property button prop)))

(defun button-put (button prop val)
  "Set BUTTON's PROP property to VAL."
  ;; Treat some properties specially.
  (cond ((memq prop '(type :type))
	 ;; We translate a `type' property a `category' property, since
	 ;; that's what's actually used by overlays/text-properties for
	 ;; inheriting properties.
	 (setq prop 'category)
	 (setq val (button-category-symbol val)))
	((eq prop 'category)
	 ;; Disallow updating the `category' property directly.
	 (error "Button `category' property may not be set directly")))
  ;; Add the property.
  (if (overlayp button)
      (overlay-put button prop val)
    ;; Must be a text-property button.
    (put-text-property
     (or (previous-single-property-change (1+ button) 'button)
	 (point-min))
     (or (next-single-property-change button 'button)
	 (point-max))
     prop val)))

(defsubst button-activate (button &optional use-mouse-action)
  "Call BUTTON's action property.
If USE-MOUSE-ACTION is non-nil, invoke the button's mouse-action
instead of its normal action; if the button has no mouse-action,
the normal action is used instead."
  (let ((action (or (and use-mouse-action (button-get button 'mouse-action))
		    (button-get button 'action))))
    (if (markerp action)
	(save-selected-window
	  (select-window (display-buffer (marker-buffer action)))
	  (goto-char action)
	  (recenter 0))
      (funcall action button))))

(defun button-label (button)
  "Return BUTTON's text label."
  (buffer-substring-no-properties (button-start button) (button-end button)))

(defsubst button-type (button)
  "Return BUTTON's button-type."
  (button-get button 'type))

(defun button-has-type-p (button type)
  "Return t if BUTTON has button-type TYPE, or one of TYPE's subtypes."
  (button-type-subtype-p (button-get button 'type) type))


;; Creating overlay buttons

(defun make-button (beg end &rest properties)
  "Make a button from BEG to END in the current buffer.
The remaining arguments form a sequence of PROPERTY VALUE pairs,
specifying properties to add to the button.
In addition, the keyword argument :type may be used to specify a
button-type from which to inherit other properties; see
`define-button-type'.

Also see `make-text-button', `insert-button'."
  (let ((overlay (make-overlay beg end nil t nil)))
    (while properties
      (button-put overlay (pop properties) (pop properties)))
    ;; Put a pointer to the button in the overlay, so it's easy to get
    ;; when we don't actually have a reference to the overlay.
    (overlay-put overlay 'button overlay)
    ;; If the user didn't specify a type, use the default.
    (unless (overlay-get overlay 'category)
      (overlay-put overlay 'category 'default-button))
    ;; OVERLAY is the button, so return it
    overlay))

(defun insert-button (label &rest properties)
  "Insert a button with the label LABEL.
The remaining arguments form a sequence of PROPERTY VALUE pairs,
specifying properties to add to the button.
In addition, the keyword argument :type may be used to specify a
button-type from which to inherit other properties; see
`define-button-type'.

Also see `insert-text-button', `make-button'."
  (apply #'make-button
	 (prog1 (point) (insert label))
	 (point)
	 properties))


;; Creating text-property buttons

(defun make-text-button (beg end &rest properties)
  "Make a button from BEG to END in the current buffer.
The remaining arguments form a sequence of PROPERTY VALUE pairs,
specifying properties to add to the button.
In addition, the keyword argument :type may be used to specify a
button-type from which to inherit other properties; see
`define-button-type'.

This function is like `make-button', except that the button is actually
part of the text instead of being a property of the buffer.  That is,
this function uses text properties, the other uses overlays.
Creating large numbers of buttons can also be somewhat faster
using `make-text-button'.  Note, however, that if there is an existing
face property at the site of the button, the button face may not be visible.
You may want to use `make-button' in that case.

BEG can also be a string, in which case it is made into a button.

Also see `insert-text-button'."
  (let ((object nil)
        (type-entry
	 (or (plist-member properties 'type)
	     (plist-member properties :type))))
    (when (stringp beg)
      (setq object beg beg 0 end (length object)))
    ;; Disallow setting the `category' property directly.
    (when (plist-get properties 'category)
      (error "Button `category' property may not be set directly"))
    (if (null type-entry)
	;; The user didn't specify a `type' property, use the default.
	(setq properties (cons 'category (cons 'default-button properties)))
      ;; The user did specify a `type' property.  Translate it into a
      ;; `category' property, which is what's actually used by
      ;; text-properties for inheritance.
      (setcar type-entry 'category)
      (setcar (cdr type-entry)
	      (button-category-symbol (car (cdr type-entry)))))
    ;; Now add all the text properties at once
    (add-text-properties beg end
                         ;; Each button should have a non-eq `button'
                         ;; property so that next-single-property-change can
                         ;; detect boundaries reliably.
                         (cons 'button (cons (list t) properties))
                         object)
    ;; Return something that can be used to get at the button.
    beg))

(defun insert-text-button (label &rest properties)
  "Insert a button with the label LABEL.
The remaining arguments form a sequence of PROPERTY VALUE pairs,
specifying properties to add to the button.
In addition, the keyword argument :type may be used to specify a
button-type from which to inherit other properties; see
`define-button-type'.

This function is like `insert-button', except that the button is
actually part of the text instead of being a property of the buffer.
Creating large numbers of buttons can also be somewhat faster using
`insert-text-button'.

Also see `make-text-button'."
  (apply #'make-text-button
	 (prog1 (point) (insert label))
	 (point)
	 properties))


;; Finding buttons in a buffer

(defun button-at (pos)
  "Return the button at position POS in the current buffer, or nil.
If the button at POS is a text property button, the return value
is a marker pointing to POS."
  (let ((button (get-char-property pos 'button)))
    (if (or (overlayp button) (null button))
	button
      ;; Must be a text-property button; return a marker pointing to it.
      (copy-marker pos t))))

(defun next-button (pos &optional count-current)
  "Return the next button after position POS in the current buffer.
If COUNT-CURRENT is non-nil, count any button at POS in the search,
instead of starting at the next button."
    (unless count-current
      ;; Search for the next button boundary.
      (setq pos (next-single-char-property-change pos 'button)))
    (and (< pos (point-max))
	 (or (button-at pos)
	     ;; We must have originally been on a button, and are now in
	     ;; the inter-button space.  Recurse to find a button.
	     (next-button pos))))

(defun previous-button (pos &optional count-current)
  "Return the previous button before position POS in the current buffer.
If COUNT-CURRENT is non-nil, count any button at POS in the search,
instead of starting at the next button."
  (let ((button (button-at pos)))
    (if button
	(if count-current
	    button
	  ;; We started out on a button, so move to its start and look
	  ;; for the previous button boundary.
	  (setq pos (previous-single-char-property-change
		     (button-start button) 'button))
	  (let ((new-button (button-at pos)))
	    (if new-button
		;; We are in a button again; this can happen if there
		;; are adjacent buttons (or at bob).
		(unless (= pos (button-start button)) new-button)
	      ;; We are now in the space between buttons.
	      (previous-button pos))))
      ;; We started out in the space between buttons.
      (setq pos (previous-single-char-property-change pos 'button))
      (or (button-at pos)
	  (and (> pos (point-min))
	       (button-at (1- pos)))))))


;; User commands

(defun push-button (&optional pos use-mouse-action)
  "Perform the action specified by a button at location POS.
POS may be either a buffer position or a mouse-event.  If
USE-MOUSE-ACTION is non-nil, invoke the button's mouse-action
instead of its normal action; if the button has no mouse-action,
the normal action is used instead.  The action may be either a
function to call or a marker to display.
POS defaults to point, except when `push-button' is invoked
interactively as the result of a mouse-event, in which case, the
mouse event is used.
If there's no button at POS, do nothing and return nil, otherwise
return t."
  (interactive
   (list (if (integerp last-command-event) (point) last-command-event)))
  (if (and (not (integerp pos)) (eventp pos))
      ;; POS is a mouse event; switch to the proper window/buffer
      (let ((posn (event-start pos)))
	(with-current-buffer (window-buffer (posn-window posn))
	  (push-button (posn-point posn) t)))
    ;; POS is just normal position
    (let ((button (button-at (or pos (point)))))
      (if (not button)
	  nil
	(button-activate button use-mouse-action)
	t))))

(defun forward-button (n &optional wrap display-message)
  "Move to the Nth next button, or Nth previous button if N is negative.
If N is 0, move to the start of any button at point.
If WRAP is non-nil, moving past either end of the buffer continues from the
other end.
If DISPLAY-MESSAGE is non-nil, the button's help-echo string is displayed.
Any button with a non-nil `skip' property is skipped over.
Returns the button found."
  (interactive "p\nd\nd")
  (let (button)
    (if (zerop n)
	;; Move to start of current button
	(if (setq button (button-at (point)))
	    (goto-char (button-start button)))
      ;; Move to Nth next button
      (let ((iterator (if (> n 0) #'next-button #'previous-button))
	    (wrap-start (if (> n 0) (point-min) (point-max)))
	    opoint fail)
	(setq n (abs n))
	(setq button t)			; just to start the loop
	(while (and (null fail) (> n 0) button)
	  (setq button (funcall iterator (point)))
	  (when (and (not button) wrap)
	    (setq button (funcall iterator wrap-start t)))
	  (when button
	    (goto-char (button-start button))
	    ;; Avoid looping forever (e.g., if all the buttons have
	    ;; the `skip' property).
	    (cond ((null opoint)
		   (setq opoint (point)))
		  ((= opoint (point))
		   (setq fail t)))
	    (unless (button-get button 'skip)
	      (setq n (1- n)))))))
    (if (null button)
	(error (if wrap "No buttons!" "No more buttons"))
      (let ((msg (and display-message (button-get button 'help-echo))))
	(when msg
	  (message "%s" msg)))
      button)))

(defun backward-button (n &optional wrap display-message)
  "Move to the Nth previous button, or Nth next button if N is negative.
If N is 0, move to the start of any button at point.
If WRAP is non-nil, moving past either end of the buffer continues from the
other end.
If DISPLAY-MESSAGE is non-nil, the button's help-echo string is displayed.
Any button with a non-nil `skip' property is skipped over.
Returns the button found."
  (interactive "p\nd\nd")
  (forward-button (- n) wrap display-message))


(provide 'button)

;;; button.el ends here
