;;; srecode/fields.el --- Handling type-in fields in a buffer.
;;
;; Copyright (C) 2009-2012 Free Software Foundation, Inc.
;;
;; Author: Eric M. Ludlam <eric@siege-engine.com>

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
;;
;; Idea courtesy of yasnippets.
;;
;; If someone prefers not to type unknown dictionary entries into
;; mini-buffer prompts, it could instead use in-buffer fields.
;;
;; A template-region specifies an area in which the fields exist.  If
;; the cursor exits the region, all fields are cleared.
;;
;; Each field is independent, but some are linked together by name.
;; Typing in one will cause the matching ones to change in step.
;;
;; Each field has 2 overlays.  The second overlay allows control in
;; the character just after the field, but does not highlight it.

;; @TODO - Cancel an old field array if a new one is about to be created!

;; Keep this library independent of SRecode proper.
(require 'eieio)

;;; Code:
(defvar srecode-field-archive nil
  "While inserting a set of fields, collect in this variable.
Once an insertion set is done, these fields will be activated.")


;;; Customization
;;

(defface srecode-field-face
  '((((class color) (background dark))
     (:underline "green"))
    (((class color) (background light))
     (:underline "green4")))
  "*Face used to specify editable fields from a template."
  :group 'semantic-faces)

(defcustom srecode-fields-exit-confirmation nil
  "Ask for confirmation before leaving field editing mode."
  :group 'srecode
  :type  'boolean)

;;; BASECLASS
;;
;; Fields and the template region share some basic overlay features.

(defclass srecode-overlaid ()
  ((overlay :documentation
	    "Overlay representing this field.
The overlay will crossreference this object.")
   )
  "An object that gets automatically bound to an overlay.
Has virtual :start and :end initializers.")

(defmethod initialize-instance ((olaid srecode-overlaid) &optional args)
  "Initialize OLAID, being sure it archived."
  ;; Extract :start and :end from the olaid list.
  (let ((newargs nil)
	(olay nil)
	start end
	)

    (while args
      (cond ((eq (car args) :start)
	     (setq args (cdr args))
	     (setq start (car args))
	     (setq args (cdr args))
	     )
	    ((eq (car args) :end)
	     (setq args (cdr args))
	     (setq end (car args))
	     (setq args (cdr args))
	     )
	    (t
	     (push (car args) newargs)
	     (setq args (cdr args))
	     (push (car args) newargs)
	     (setq args (cdr args)))
	    ))

    ;; Create a temporary overlay now.  We have to use an overlay and
    ;; not a marker because of the in-front insertion rules.  The rules
    ;; are backward from what is wanted while typing.
    (setq olay (make-overlay start end (current-buffer) t nil))
    (overlay-put olay 'srecode-init-only t)

    (oset olaid overlay olay)
    (call-next-method olaid (nreverse newargs))

    ))

(defmethod srecode-overlaid-activate ((olaid srecode-overlaid))
  "Activate the overlaid area."
  (let* ((ola (oref olaid overlay))
	 (start (overlay-start ola))
	 (end (overlay-end ola))
	 ;; Create a new overlay here.
	 (ol (make-overlay start end (current-buffer) nil t)))

    ;; Remove the old one.
    (delete-overlay ola)

    (overlay-put ol 'srecode olaid)

    (oset olaid overlay ol)

    ))

(defmethod srecode-delete ((olaid srecode-overlaid))
  "Delete the overlay from OLAID."
  (delete-overlay (oref olaid overlay))
  (slot-makeunbound olaid 'overlay)
  )

(defmethod srecode-empty-region-p ((olaid srecode-overlaid))
  "Return non-nil if the region covered by OLAID is of length 0."
  (= 0 (srecode-region-size olaid)))

(defmethod srecode-region-size ((olaid srecode-overlaid))
  "Return the length of region covered by OLAID."
  (let ((start (overlay-start (oref olaid overlay)))
	(end (overlay-end (oref olaid overlay))))
    (- end start)))

(defmethod srecode-point-in-region-p ((olaid srecode-overlaid))
  "Return non-nil if point is in the region of OLAID."
  (let ((start (overlay-start (oref olaid overlay)))
	(end (overlay-end (oref olaid overlay))))
    (and (>= (point) start) (<= (point) end))))

(defun srecode-overlaid-at-point (class)
  "Return a list of overlaid fields of type CLASS at point."
  (let ((ol (overlays-at (point)))
	(ret nil))
    (while ol
      (let ((tmp (overlay-get (car ol) 'srecode)))
	(when (and tmp (object-of-class-p tmp class))
	  (setq ret (cons tmp ret))))
      (setq ol (cdr ol)))
    (car (nreverse ret))))

(defmethod srecode-overlaid-text ((olaid srecode-overlaid) &optional set-to)
  "Return the text under OLAID.
If SET-TO is a string, then replace the text of OLAID wit SET-TO."
  (let* ((ol (oref olaid overlay))
	 (start (overlay-start ol)))
    (if (not (stringp set-to))
	;; Just return it.
	(buffer-substring-no-properties start (overlay-end ol))
      ;; Replace it.
      (save-excursion
	(delete-region start (overlay-end ol))
	(goto-char start)
	(insert set-to)
	(move-overlay ol start (+ start (length set-to))))
      nil)))

;;; INSERTED REGION
;;
;; Managing point-exit, and flushing fields.

(defclass srecode-template-inserted-region (srecode-overlaid)
  ((fields :documentation
	   "A list of field overlays in this region.")
   (active-region :allocation :class
		  :initform nil
		  :documentation
		  "The template region currently being handled.")
   )
  "Manage a buffer region in which fields exist.")

(defmethod initialize-instance ((ir srecode-template-inserted-region)
				&rest args)
  "Initialize IR, capturing the active fields, and creating the overlay."
  ;; Fill in the fields
  (oset ir fields srecode-field-archive)
  (setq srecode-field-archive nil)

  ;; Initialize myself first.
  (call-next-method)
  )

(defmethod srecode-overlaid-activate ((ir srecode-template-inserted-region))
  "Activate the template area for IR."
  ;; Activate all our fields

  (dolist (F (oref ir fields))
    (srecode-overlaid-activate F))

  ;; Activate our overlay.
  (call-next-method)

  ;; Position the cursor at the first field
  (let ((first (car (oref ir fields))))
    (goto-char (overlay-start (oref first overlay))))

  ;; Set ourselves up as 'active'
  (oset ir active-region ir)

  ;; Setup the post command hook.
  (add-hook 'post-command-hook 'srecode-field-post-command t t)
  )

(defmethod srecode-delete ((ir srecode-template-inserted-region))
  "Call into our base, but also clear out the fields."
  ;; Clear us out of the baseclass.
  (oset ir active-region nil)
  ;; Clear our fields.
  (mapc 'srecode-delete (oref ir fields))
  ;; Call to our base
  (call-next-method)
  ;; Clear our hook.
  (remove-hook 'post-command-hook 'srecode-field-post-command t)
  )

(defsubst srecode-active-template-region ()
  "Return the active region for template fields."
  (oref srecode-template-inserted-region active-region))

(defun srecode-field-post-command ()
  "Srecode field handler in the post command hook."
  (let ((ar (srecode-active-template-region))
	)
    (if (not ar)
	;; Find a bug and fix it.
	(remove-hook 'post-command-hook 'srecode-field-post-command t)
      (if (srecode-point-in-region-p ar)
	  nil ;; Keep going
	;; We moved out of the template.  Cancel the edits.
	(srecode-delete ar)))
    ))

;;; FIELDS

(defclass srecode-field (srecode-overlaid)
  ((tail :documentation
	 "Overlay used on character just after this field.
Used to provide useful keybindings there.")
   (name :initarg :name
	 :documentation
	 "The name of this field.
Usually initialized from the dictionary entry name that
the users needs to edit.")
   (prompt :initarg :prompt
	   :documentation
	   "A prompt string to use if this were in the minibuffer.
Display when the cursor enters this field.")
   (read-fcn :initarg :read-fcn
	     :documentation
	     "A function that would be used to read a string.
Try to use this to provide useful completion when available.")
   )
  "Representation of one field.")

(defvar srecode-field-keymap
  (let ((km (make-sparse-keymap)))
    (define-key km "\C-i" 'srecode-field-next)
    (define-key km "\M-\C-i" 'srecode-field-prev)
    (define-key km "\C-e" 'srecode-field-end)
    (define-key km "\C-a" 'srecode-field-start)
    (define-key km "\M-m" 'srecode-field-start)
    (define-key km "\C-c\C-c" 'srecode-field-exit-ask)
    km)
  "Keymap applied to field overlays.")

(defmethod initialize-instance ((field srecode-field) &optional args)
  "Initialize FIELD, being sure it archived."
  (add-to-list 'srecode-field-archive field t)
  (call-next-method)
  )

(defmethod srecode-overlaid-activate ((field srecode-field))
  "Activate the FIELD area."
  (call-next-method)

  (let* ((ol (oref field overlay))
	 (end nil)
	 (tail nil))
    (overlay-put ol 'face 'srecode-field-face)
    (overlay-put ol 'keymap srecode-field-keymap)
    (overlay-put ol 'modification-hooks '(srecode-field-mod-hook))
    (overlay-put ol 'insert-behind-hooks '(srecode-field-behind-hook))
    (overlay-put ol 'insert-in-front-hooks '(srecode-field-mod-hook))

    (setq end (overlay-end ol))
    (setq tail (make-overlay end (+ end 1) (current-buffer)))

    (overlay-put tail 'srecode field)
    (overlay-put tail 'keymap srecode-field-keymap)
    (overlay-put tail 'face 'srecode-field-face)
    (oset field tail tail)
    )
  )

(defmethod srecode-delete ((olaid srecode-field))
  "Delete our secondary overlay."
  ;; Remove our spare overlay
  (delete-overlay (oref olaid tail))
  (slot-makeunbound olaid 'tail)
  ;; Do our baseclass work.
  (call-next-method)
  )

(defvar srecode-field-replication-max-size 100
  "Maximum size of a field before canceling replication.")

(defun srecode-field-mod-hook (ol after start end &optional pre-len)
  "Modification hook for the field overlay.
OL is the overlay.
AFTER is non-nil if it is called after the change.
START and END are the bounds of the change.
PRE-LEN is used in the after mode for the length of the changed text."
  (when (and after (not undo-in-progress))
    (let* ((field (overlay-get ol 'srecode))
	   (inhibit-point-motion-hooks t)
	   (inhibit-modification-hooks t)
	   )
      ;; Sometimes a field is deleted, but we might still get a stray
      ;; event.  Let's just ignore those events.
      (when (slot-boundp field 'overlay)
	;; First, fixup the two overlays, in case they got confused.
	(let ((main (oref field overlay))
	      (tail (oref field tail)))
	  (move-overlay main
				(overlay-start main)
				(1- (overlay-end tail)))
	  (move-overlay tail
				(1- (overlay-end tail))
				(overlay-end tail)))
	;; Now capture text from the main overlay, and propagate it.
	(let* ((new-text (srecode-overlaid-text field))
	       (region (srecode-active-template-region))
	       (allfields (when region (oref region fields)))
	       (name (oref field name)))
	  (dolist (F allfields)
	    (when (and (not (eq F field))
		       (string= name (oref F name)))
	      (if (> (length new-text) srecode-field-replication-max-size)
		  (message "Field size too large for replication.")
		;; If we find other fields with the same name, then keep
		;; then all together.  Disable change hooks to make sure
		;; we don't get a recursive edit.
		(srecode-overlaid-text F new-text)
		))))
	))))

(defun srecode-field-behind-hook (ol after start end &optional pre-len)
  "Modification hook for the field overlay.
OL is the overlay.
AFTER is non-nil if it is called after the change.
START and END are the bounds of the change.
PRE-LEN is used in the after mode for the length of the changed text."
  (when after
    (let* ((field (overlay-get ol 'srecode))
	   )
      (move-overlay ol (overlay-start ol) end)
      (srecode-field-mod-hook ol after start end pre-len))
    ))

(defmethod srecode-field-goto ((field srecode-field))
  "Goto the FIELD."
  (goto-char (overlay-start (oref field overlay))))

(defun srecode-field-next ()
  "Move to the next field."
  (interactive)
  (let* ((f (srecode-overlaid-at-point 'srecode-field))
	 (tr (srecode-overlaid-at-point 'srecode-template-inserted-region))
	 )
    (when (not f) (error "Not in a field"))
    (when (not tr) (error "Not in a template region"))

    (let ((fields (oref tr fields)))
      (while fields
	;; Loop over fields till we match.  Then move to the next one.
	(when (eq f (car fields))
	  (if (cdr fields)
	      (srecode-field-goto (car (cdr fields)))
	    (srecode-field-goto (car (oref tr fields))))
	  (setq fields nil)
	  )
	(setq fields (cdr fields))))
    ))

(defun srecode-field-prev ()
  "Move to the prev field."
  (interactive)
  (let* ((f (srecode-overlaid-at-point 'srecode-field))
	 (tr (srecode-overlaid-at-point 'srecode-template-inserted-region))
	 )
    (when (not f) (error "Not in a field"))
    (when (not tr) (error "Not in a template region"))

    (let ((fields (reverse (oref tr fields))))
      (while fields
	;; Loop over fields till we match.  Then move to the next one.
	(when (eq f (car fields))
	  (if (cdr fields)
	      (srecode-field-goto (car (cdr fields)))
	    (srecode-field-goto (car (oref tr fields))))
	  (setq fields nil)
	  )
	(setq fields (cdr fields))))
    ))

(defun srecode-field-end ()
  "Move to the end of this field."
  (interactive)
  (let* ((f (srecode-overlaid-at-point 'srecode-field)))
    (goto-char (overlay-end (oref f overlay)))))

(defun srecode-field-start ()
  "Move to the end of this field."
  (interactive)
  (let* ((f (srecode-overlaid-at-point 'srecode-field)))
    (goto-char (overlay-start (oref f overlay)))))

(defun srecode-field-exit-ask ()
  "Ask if the user wants to exit field-editing mini-mode."
  (interactive)
  (when (or (not srecode-fields-exit-confirmation)
	    (y-or-n-p "Exit field-editing mode? "))
    (srecode-delete (srecode-active-template-region))))


(provide 'srecode/fields)

;;; srecode/fields.el ends here
