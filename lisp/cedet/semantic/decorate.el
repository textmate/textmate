;;; semantic/decorate.el --- Utilities for decorating/highlighting tokens.

;;; Copyright (C) 1999-2003, 2005-2007, 2009-2012
;;; Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax

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
;; Text representing a semantic tag is wrapped in an overlay.
;; This overlay can be used for highlighting, or setting other
;; editing properties on a tag, such as "read only."
;;

(require 'semantic)
(require 'pulse)

;;; Code:

;;; Highlighting Basics
(defun semantic-highlight-tag (tag &optional face)
  "Specify that TAG should be highlighted.
Optional FACE specifies the face to use."
  (let ((o (semantic-tag-overlay tag)))
    (semantic-overlay-put o 'old-face
			  (cons (semantic-overlay-get o 'face)
				(semantic-overlay-get o 'old-face)))
    (semantic-overlay-put o 'face (or face 'semantic-tag-highlight-face))
    ))

(defun semantic-unhighlight-tag (tag)
  "Unhighlight TAG, restoring its previous face."
  (let ((o (semantic-tag-overlay tag)))
    (semantic-overlay-put o 'face (car (semantic-overlay-get o 'old-face)))
    (semantic-overlay-put o 'old-face (cdr (semantic-overlay-get o 'old-face)))
    ))

;;; Momentary Highlighting - One line
(defun semantic-momentary-highlight-one-tag-line (tag &optional face)
  "Highlight the first line of TAG, unhighlighting before next command.
Optional argument FACE specifies the face to do the highlighting."
  (save-excursion
    ;; Go to first line in tag
    (semantic-go-to-tag tag)
    (pulse-momentary-highlight-one-line (point))))

;;; Momentary Highlighting - Whole Tag
(defun semantic-momentary-highlight-tag (tag &optional face)
  "Highlight TAG, removing highlighting when the user hits a key.
Optional argument FACE is the face to use for highlighting.
If FACE is not specified, then `highlight' will be used."
  (when (semantic-tag-with-position-p tag)
    (if (not (semantic-overlay-p (semantic-tag-overlay tag)))
	;; No overlay, but a position.  Highlight the first line only.
	(semantic-momentary-highlight-one-tag-line tag face)
      ;; The tag has an overlay, highlight the whole thing
      (pulse-momentary-highlight-overlay (semantic-tag-overlay tag)
					 face)
      )))

(defun semantic-set-tag-face (tag face)
  "Specify that TAG should use FACE for display."
  (semantic-overlay-put (semantic-tag-overlay tag) 'face face))

(defun semantic-set-tag-invisible (tag &optional visible)
  "Enable the text in TAG to be made invisible.
If VISIBLE is non-nil, make the text visible."
  (semantic-overlay-put (semantic-tag-overlay tag) 'invisible
			(not visible)))

(defun semantic-tag-invisible-p (tag)
  "Return non-nil if TAG is invisible."
  (semantic-overlay-get (semantic-tag-overlay tag) 'invisible))

(defun semantic-set-tag-intangible (tag &optional tangible)
  "Enable the text in TAG to be made intangible.
If TANGIBLE is non-nil, make the text visible.
This function does not have meaning in XEmacs because it seems that
the extent 'intangible' property does not exist."
  (semantic-overlay-put (semantic-tag-overlay tag) 'intangible
			(not tangible)))

(defun semantic-tag-intangible-p (tag)
  "Return non-nil if TAG is intangible.
This function does not have meaning in XEmacs because it seems that
the extent 'intangible' property does not exist."
  (semantic-overlay-get (semantic-tag-overlay tag) 'intangible))

(defun semantic-overlay-signal-read-only
  (overlay after start end &optional len)
  "Hook used in modification hooks to prevent modification.
Allows deletion of the entire text.
Argument OVERLAY, AFTER, START, END, and LEN are passed in by the system."
  ;; Stolen blithely from cpp.el in Emacs 21.1
  (if (and (not after)
	   (or (< (semantic-overlay-start overlay) start)
	       (> (semantic-overlay-end overlay) end)))
      (error "This text is read only")))

(defun semantic-set-tag-read-only (tag &optional writable)
  "Enable the text in TAG to be made read-only.
Optional argument WRITABLE should be non-nil to make the text writable
instead of read-only."
  (let ((o (semantic-tag-overlay tag))
	(hook (if writable nil '(semantic-overlay-signal-read-only))))
    (if (featurep 'xemacs)
        ;; XEmacs extents have a 'read-only' property.
        (semantic-overlay-put o 'read-only (not writable))
      (semantic-overlay-put o 'modification-hooks hook)
      (semantic-overlay-put o 'insert-in-front-hooks hook)
      (semantic-overlay-put o 'insert-behind-hooks hook))))

(defun semantic-tag-read-only-p (tag)
  "Return non-nil if the current TAG is marked read only."
  (let ((o (semantic-tag-overlay tag)))
    (if (featurep 'xemacs)
        ;; XEmacs extents have a 'read-only' property.
        (semantic-overlay-get o 'read-only)
      (member 'semantic-overlay-signal-read-only
              (semantic-overlay-get o 'modification-hooks)))))

;;; Secondary overlays
;;
;; Some types of decoration require a second overlay to be made.
;; It could be for images, arrows, or whatever.
;; We need a way to create such an overlay, and make sure it
;; gets whacked, but doesn't show up in the master list
;; of overlays used for searching.
(defun semantic-tag-secondary-overlays (tag)
  "Return a list of secondary overlays active on TAG."
  (semantic--tag-get-property tag 'secondary-overlays))

(defun semantic-tag-create-secondary-overlay (tag &optional link-hook)
  "Create a secondary overlay for TAG.
Returns an overlay.  The overlay is also saved in TAG.
LINK-HOOK is a function called whenever TAG is to be linked into
a buffer.  It should take TAG and OVERLAY as arguments.
The LINK-HOOK should be used to position and set properties on the
generated secondary overlay."
  (if (not (semantic-tag-overlay tag))
      ;; do nothing if there is no overlay
      nil
    (let* ((os (semantic-tag-start tag))
	   (oe (semantic-tag-end tag))
	   (o (semantic-make-overlay os oe (semantic-tag-buffer tag) t))
	   (attr (semantic-tag-secondary-overlays tag))
	   )
      (semantic--tag-put-property tag 'secondary-overlays (cons o attr))
      (semantic-overlay-put o 'semantic-secondary t)
      (semantic-overlay-put o 'semantic-link-hook link-hook)
      (semantic-tag-add-hook tag 'link-hook 'semantic--tag-link-secondary-overlays)
      (semantic-tag-add-hook tag 'unlink-hook 'semantic--tag-unlink-secondary-overlays)
      (semantic-tag-add-hook tag 'unlink-copy-hook 'semantic--tag-unlink-copy-secondary-overlays)
      (run-hook-with-args link-hook tag o)
      o)))

(defun semantic-tag-get-secondary-overlay (tag property)
  "Return secondary overlays from TAG with PROPERTY.
PROPERTY is a symbol and all overlays with that symbol are returned.."
  (let* ((olsearch (semantic-tag-secondary-overlays tag))
	 (o nil))
    (while olsearch
      (when (semantic-overlay-get (car olsearch) property)
	(setq o (cons (car olsearch) o)))
      (setq olsearch (cdr olsearch)))
    o))

(defun semantic-tag-delete-secondary-overlay (tag overlay-or-property)
  "Delete from TAG the secondary overlay OVERLAY-OR-PROPERTY.
If OVERLAY-OR-PROPERTY is an overlay, delete that overlay.
If OVERLAY-OR-PROPERTY is a symbol, find the overlay with that property."
  (let* ((o overlay-or-property))
    (if (semantic-overlay-p o)
	(setq o (list o))
      (setq o (semantic-tag-get-secondary-overlay tag overlay-or-property)))
    (while (semantic-overlay-p (car o))
      ;; We don't really need to worry about the hooks.
      ;; They will clean themselves up eventually ??
      (semantic--tag-put-property
       tag 'secondary-overlays
       (delete (car o) (semantic-tag-secondary-overlays tag)))
      (semantic-overlay-delete (car o))
      (setq o (cdr o)))))

(defun semantic--tag-unlink-copy-secondary-overlays (tag)
  "Unlink secondary overlays from TAG which is a copy.
This means we don't destroy the overlays, only remove reference
from them in TAG."
  (let ((ol (semantic-tag-secondary-overlays tag)))
    (while ol
      ;; Else, remove all  traces of ourself from the tag
      ;; Note to self: Does this prevent multiple types of secondary
      ;; overlays per tag?
      (semantic-tag-remove-hook tag 'link-hook 'semantic--tag-link-secondary-overlays)
      (semantic-tag-remove-hook tag 'unlink-hook 'semantic--tag-unlink-secondary-overlays)
      (semantic-tag-remove-hook tag 'unlink-copy-hook 'semantic--tag-unlink-copy-secondary-overlays)
      ;; Next!
      (setq ol (cdr ol)))
    (semantic--tag-put-property tag 'secondary-overlays nil)
    ))

(defun semantic--tag-unlink-secondary-overlays (tag)
  "Unlink secondary overlays from TAG."
  (let ((ol (semantic-tag-secondary-overlays tag))
	(nl nil))
    (while ol
      (if (semantic-overlay-get (car ol) 'semantic-link-hook)
	  ;; Only put in a proxy if there is a link-hook.  If there is no link-hook
	  ;; the decorating mode must know when tags are unlinked on its own.
	  (setq nl (cons (semantic-overlay-get (car ol) 'semantic-link-hook)
			 nl))
	;; Else, remove all  traces of ourself from the tag
	;; Note to self: Does this prevent multiple types of secondary
	;; overlays per tag?
	(semantic-tag-remove-hook tag 'link-hook 'semantic--tag-link-secondary-overlays)
	(semantic-tag-remove-hook tag 'unlink-hook 'semantic--tag-unlink-secondary-overlays)
	(semantic-tag-remove-hook tag 'unlink-copy-hook 'semantic--tag-unlink-copy-secondary-overlays)
	)
      (semantic-overlay-delete (car ol))
      (setq ol (cdr ol)))
    (semantic--tag-put-property tag 'secondary-overlays (nreverse nl))
    ))

(defun semantic--tag-link-secondary-overlays (tag)
  "Unlink secondary overlays from TAG."
  (let ((ol (semantic-tag-secondary-overlays tag)))
    ;; Wipe out old values.
    (semantic--tag-put-property tag 'secondary-overlays nil)
    ;; Run all the link hooks.
    (while ol
      (semantic-tag-create-secondary-overlay tag (car ol))
      (setq ol (cdr ol)))
    ))

;;; Secondary Overlay Uses
;;
;; States to put on tags that depend on a secondary overlay.
(defun semantic-set-tag-folded (tag &optional folded)
  "Fold TAG, such that only the first line of text is shown.
Optional argument FOLDED should be non-nil to fold the tag.
nil implies the tag should be fully shown."
    ;; If they are different, do the deed.
    (let ((o (semantic-tag-folded-p tag)))
      (if (not folded)
	  ;; We unfold.
	  (when o
	    (semantic-tag-delete-secondary-overlay tag 'semantic-folded))
	(unless o
	  ;; Add the foldn
	  (setq o (semantic-tag-create-secondary-overlay tag))
	  ;; mark as folded
	  (semantic-overlay-put o 'semantic-folded t)
	  ;; Move to cover end of tag
	  (save-excursion
	    (goto-char (semantic-tag-start tag))
	    (end-of-line)
	    (semantic-overlay-move o (point) (semantic-tag-end tag)))
	  ;; We need to modify the invisibility spec for this to
	  ;; work.
	  (if (or (eq buffer-invisibility-spec t)
		  (not (assoc 'semantic-fold buffer-invisibility-spec)))
	      (add-to-invisibility-spec '(semantic-fold . t)))
	  (semantic-overlay-put o 'invisible 'semantic-fold)
	  (overlay-put o 'isearch-open-invisible
		       'semantic-set-tag-folded-isearch)))
	  ))

(declare-function semantic-current-tag "semantic/find")

(defun semantic-set-tag-folded-isearch (overlay)
  "Called by isearch if it discovers text in the folded region.
OVERLAY is passed in by isearch."
  (semantic-set-tag-folded (semantic-current-tag) nil)
  )

(defun semantic-tag-folded-p (tag)
  "Non-nil if TAG is currently folded."
  (semantic-tag-get-secondary-overlay tag 'semantic-folded)
  )

(provide 'semantic/decorate)

;;; semantic/decorate.el ends here
