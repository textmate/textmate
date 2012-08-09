;;; semantic/mru-bookmark.el --- Automatic bookmark tracking

;; Copyright (C) 2007-2012 Free Software Foundation, Inc.

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
;; Using editing hooks, track the most recently visited or poked tags,
;; and keep a list of them, with the current point in from, and sorted
;; by most recently used.
;;
;; I envision this would be used in place of switch-buffers once
;; someone got the hang of it.
;;
;; I'd also like to see this used to provide some nice defaults for
;; other programs where logical destinations or targets are the tags
;; that have been recently edited.
;;
;; Quick Start:
;;
;; M-x global-semantic-mru-bookmark-mode RET
;;
;; < edit some code >
;;
;; C-x B  <select a tag name> RET
;;
;; In the above, the history is pre-filled with the tags you recently
;; edited in the order you edited them.

;;; Code:

(eval-when-compile (require 'cl))
(require 'semantic)
(require 'eieio-base)
(require 'ring)

(declare-function data-debug-new-buffer "data-debug")
(declare-function data-debug-insert-object-slots "eieio-datadebug")
(declare-function semantic-momentary-highlight-tag "semantic/decorate")

;;; TRACKING CORE
;;
;; Data structure for tracking MRU tag locations

(defclass semantic-bookmark (eieio-named)
  ((tag :initarg :tag
	:type semantic-tag
	:documentation "The TAG this bookmark belongs to.")
   (parent :type (or semantic-tag null)
	   :documentation "The tag that is the parent of :tag.")
   (offset :type number
	 :documentation "The offset from `tag' start that is
somehow interesting.")
   (filename :type string
	     :documentation "String the tag belongs to.
Set this when the tag gets unlinked from the buffer it belongs to.")
   (frequency :type number
	      :initform 0
	      :documentation "Track the frequency this tag is visited.")
   (reason :type symbol
	   :initform t
	   :documentation
	   "The reason this tag is interesting.
Nice values are 'edit, 'read, 'jump, and 'mark.
 edit - created because the tag text was edited.
 read - created because point lingered in tag text.
 jump - jumped to another tag from this tag.
 mark - created a regular mark in this tag.")
   )
  "A single bookmark.")

(defmethod initialize-instance :AFTER ((sbm semantic-bookmark) &rest fields)
  "Initialize the bookmark SBM with details about :tag."
  (condition-case nil
      (save-excursion
	(oset sbm filename (semantic-tag-file-name (oref sbm tag)))
	(semantic-go-to-tag (oref sbm tag))
	(oset sbm parent (semantic-current-tag-parent)))
    (error (message "Error bookmarking tag.")))
  )

(defmethod semantic-mrub-visit ((sbm semantic-bookmark))
  "Visit the semantic tag bookmark SBM.
Uses `semantic-go-to-tag' and highlighting."
  (require 'semantic/decorate)
  (with-slots (tag filename) sbm
    ;; Go to the tag
    (when (not (semantic-tag-in-buffer-p tag))
      (let ((fn (or (semantic-tag-file-name tag)
 		    filename)))
 	(set-buffer (find-file-noselect fn))))
    (semantic-go-to-tag (oref sbm tag) (oref sbm parent))
    ;; Go back to the offset.
    (condition-case nil
	(let ((o (oref sbm offset)))
	  (forward-char o))
      (error nil))
    ;; make it visible
    (switch-to-buffer (current-buffer))
    (semantic-momentary-highlight-tag tag)
    ))

(defmethod semantic-mrub-update ((sbm semantic-bookmark) point reason)
  "Update the existing bookmark SBM.
POINT is some important location.
REASON is a symbol.  See slot `reason' on `semantic-bookmark'."
  (condition-case nil
      (progn
	(with-slots (tag offset frequency) sbm
	  (setq offset (- point (semantic-tag-start tag)))
	  (setq frequency (1+ frequency))
	  )
	(oset sbm reason reason))
    ;; This can fail on XEmacs at miscellaneous times.
    (error nil))
  )

(defmethod semantic-mrub-preflush ((sbm semantic-bookmark))
  "Method called on a tag before the current buffer list of tags is flushed.
If there is a buffer match, unlink the tag."
  (let ((tag (oref sbm tag))
	(parent (when (slot-boundp sbm 'parent)
		  (oref sbm parent))))
    (let ((b (semantic-tag-in-buffer-p tag)))
      (when (and b (eq b (current-buffer)))
	(semantic--tag-unlink-from-buffer tag)))

    (when parent
      (let ((b (semantic-tag-in-buffer-p parent)))
	(when (and b (eq b (current-buffer)))
	  (semantic--tag-unlink-from-buffer parent))))))

(defclass semantic-bookmark-ring ()
  ((ring :initarg :ring
	 :type ring
	 :documentation
	 "List of `semantic-bookmark' objects.
This list is maintained as a list with the first item
being the current location, and the rest being a list of
items that were recently visited.")
   (current-index :initform 0
		  :type number
		  :documentation
		  "The current index into RING for some operation.
User commands use this to move through the ring, or reset.")
   )
  "Track the current MRU stack of bookmarks.
We can't use the built-in ring data structure because we need
to delete some items from the ring when we don't have the data.")

(defvar semantic-mru-bookmark-ring (semantic-bookmark-ring
				    "Ring"
				    :ring (make-ring 20))
  "The MRU bookmark ring.
This ring tracks the most recent active tags of interest.")

(defun semantic-mrub-find-nearby-tag (point)
  "Find a nearby tag to be pushed for this current location.
Argument POINT is where to find the tag near."
  ;; I thought this was a good idea, but it is not!
  ;;(semantic-fetch-tags) ;; Make sure everything is up-to-date.
  (let ((tag (semantic-current-tag)))
    (when (or (not tag) (semantic-tag-of-class-p tag 'type))
      (let ((nearby (or (semantic-find-tag-by-overlay-next point)
			(semantic-find-tag-by-overlay-prev point))))
	(when nearby (setq tag nearby))))
    tag))

(defmethod semantic-mrub-push ((sbr semantic-bookmark-ring) point
			       &optional reason)
  "Add a bookmark to the ring SBR from POINT.
REASON is why it is being pushed.  See doc for `semantic-bookmark'
for possible reasons.
The resulting bookmark is then sorted within the ring."
  (let* ((ring (oref sbr ring))
	 (tag (semantic-mrub-find-nearby-tag (point)))
	 (idx 0))
    (when tag
      (while (and (not (ring-empty-p ring)) (< idx (ring-size ring)))
	(if (semantic-tag-similar-p (oref (ring-ref ring idx) tag)
				    tag)
	    (ring-remove ring idx))
	(setq idx (1+ idx)))
      ;; Create a new mark
      (let ((sbm (semantic-bookmark (semantic-tag-name tag)
				    :tag tag)))
	;; Take the mark, and update it for the current state.
	(ring-insert ring sbm)
	(semantic-mrub-update sbm point reason))
      )))

(defun semantic-mrub-cache-flush-fcn ()
  "Function called in the `semantic-before-toplevel-cache-flush-hook`.
Cause tags in the ring to become unlinked."
  (let* ((ring (oref semantic-mru-bookmark-ring ring))
	 (len (ring-length ring))
	 (idx 0)
	 )
    (while (< idx len)
      (semantic-mrub-preflush (ring-ref ring idx))
      (setq idx (1+ idx)))))

(add-hook 'semantic-before-toplevel-cache-flush-hook
	  'semantic-mrub-cache-flush-fcn)

;;; EDIT tracker
;;
(defvar semantic-mrub-last-overlay nil
  "The last overlay bumped by `semantic-mru-bookmark-change-hook-fcn'.")

(defun semantic-mru-bookmark-change-hook-fcn (overlay)
  "Function set into `semantic-edits-new/move-change-hook's.
Argument OVERLAY is the overlay created to mark the change.
This function pushes tags onto the tag ring."
  ;; Dup?
  (when (not (eq overlay semantic-mrub-last-overlay))
    (setq semantic-mrub-last-overlay overlay)
    (semantic-mrub-push semantic-mru-bookmark-ring
			(point)
			'edit)))

;;; MINOR MODE
;;
;; Tracking minor mode.

(defcustom global-semantic-mru-bookmark-mode nil
  "If non-nil, enable `semantic-mru-bookmark-mode' globally.
When this mode is enabled, Emacs keeps track of which tags have
been edited, and you can re-visit them with \\[semantic-mrub-switch-tags]."
  :group 'semantic
  :group 'semantic-modes
  :type 'boolean
  :require 'semantic/util-modes
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (global-semantic-mru-bookmark-mode (if val 1 -1))))

;;;###autoload
(define-minor-mode global-semantic-mru-bookmark-mode
  "Toggle global use of option `semantic-mru-bookmark-mode'.
If ARG is positive or nil, enable, if it is negative, disable."
  :global t :group 'semantic :group 'semantic-modes
  ;; Not needed because it's autoloaded instead.
  ;; :require 'semantic-util-modes
  (semantic-toggle-minor-mode-globally
   'semantic-mru-bookmark-mode (if global-semantic-mru-bookmark-mode 1 -1)))

(defcustom semantic-mru-bookmark-mode-hook nil
  "*Hook run at the end of function `semantic-mru-bookmark-mode'."
  :group 'semantic
  :type 'hook)

(defvar semantic-mru-bookmark-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km "\C-xB" 'semantic-mrub-switch-tags)
    km)
  "Keymap for mru-bookmark minor mode.")

(define-minor-mode semantic-mru-bookmark-mode
  "Minor mode for tracking tag-based bookmarks automatically.
When this mode is enabled, Emacs keeps track of which tags have
been edited, and you can re-visit them with \\[semantic-mrub-switch-tags].

\\{semantic-mru-bookmark-mode-map}

With prefix argument ARG, turn on if positive, otherwise off.  The
minor mode can be turned on only if semantic feature is available and
the current buffer was set up for parsing.  Return non-nil if the
minor mode is enabled."
  :keymap semantic-mru-bookmark-mode-map
  (if semantic-mru-bookmark-mode
      (if (not (and (featurep 'semantic) (semantic-active-p)))
	  (progn
            ;; Disable minor mode if semantic stuff not available
            (setq semantic-mru-bookmark-mode nil)
            (error "Buffer %s was not set up for parsing"
                   (buffer-name)))
        (semantic-make-local-hook 'semantic-edits-new-change-hooks)
        (add-hook 'semantic-edits-new-change-hooks
                  'semantic-mru-bookmark-change-hook-fcn nil t)
        (add-hook 'semantic-edits-move-change-hooks
                  'semantic-mru-bookmark-change-hook-fcn nil t))
    ;; Remove hooks
    (remove-hook 'semantic-edits-new-change-hooks
		 'semantic-mru-bookmark-change-hook-fcn t)
    (remove-hook 'semantic-edits-move-change-hooks
		 'semantic-mru-bookmark-change-hook-fcn t)))

(semantic-add-minor-mode 'semantic-mru-bookmark-mode
                         "k")

;;; COMPLETING READ
;;
;; Ask the user for a tag in MRU order.
(defun semantic-mrub-read-history nil
  "History of `semantic-mrub-completing-read'.")

(defun semantic-mrub-ring-to-assoc-list (ring)
  "Convert RING into an association list for completion."
  (let ((idx 0)
	(len (ring-length ring))
	(al nil))
    (while (< idx len)
      (let ((r (ring-ref ring idx)))
	(setq al (cons (cons (oref r :object-name) r)
		       al)))
      (setq idx (1+ idx)))
    (nreverse al)))

(defun semantic-mrub-completing-read (prompt)
  "Do a `completing-read' on elements from the mru bookmark ring.
Argument PROMPT is the prompt to use when reading."
  (if (ring-empty-p (oref semantic-mru-bookmark-ring ring))
      (error "Semantic Bookmark ring is currently empty"))
  (let* ((ring (oref semantic-mru-bookmark-ring ring))
	 (ans nil)
	 (alist (semantic-mrub-ring-to-assoc-list ring))
	 (first (cdr (car alist)))
	 (semantic-mrub-read-history nil)
	 )
    ;; Don't include the current tag.. only those that come after.
    (if (semantic-equivalent-tag-p (oref first tag)
				   (semantic-current-tag))
	(setq first (cdr (car (cdr alist)))))
    ;; Create a fake history list so we don't have to bind
    ;; M-p and M-n to our special cause.
    (let ((elts (reverse alist)))
      (while elts
	(setq semantic-mrub-read-history
	      (cons (car (car elts)) semantic-mrub-read-history))
	(setq elts (cdr elts))))
    (setq semantic-mrub-read-history (nreverse semantic-mrub-read-history))

    ;; Do the read/prompt
    (let ((prompt (if first (format "%s (%s): " prompt
				    (semantic-format-tag-name
				     (oref first tag) t)
				    )
		    (concat prompt ": ")))
	  )
      (setq ans
	    (completing-read prompt alist nil nil nil 'semantic-mrub-read-history)))
    ;; Calculate the return tag.
    (if (string= ans "")
	(setq ans first)
      ;; Return the bookmark object.
      (setq ans (assoc ans alist))
      (if ans
	  (cdr ans)
	;; no match.  Custom word.  Look it up somewhere?
	nil)
      )))

(defun semantic-mrub-switch-tags (tagmark)
  "Switch tags to TAGMARK.
Selects a new tag via prompt through the mru tag ring.
Jumps to the tag and highlights it briefly."
  (interactive (list (semantic-mrub-completing-read "Switch to tag")))
  (if (not (semantic-bookmark-p tagmark))
      (signal 'wrong-type-argument tagmark))

  (semantic-mrub-push semantic-mru-bookmark-ring
		      (point)
		      'jump)
  (semantic-mrub-visit tagmark)
  )

;;; Debugging
;;
(defun semantic-adebug-mrub ()
  "Display a list of items in the MRU bookmarks list.
Useful for debugging mrub problems."
  (interactive)
  (require 'eieio-datadebug)
  (let* ((out semantic-mru-bookmark-ring))
    (data-debug-new-buffer "*TAG RING ADEBUG*")
    (data-debug-insert-object-slots out "]")
    ))


(provide 'semantic/mru-bookmark)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "semantic/mru-bookmark"
;; End:

;;; semantic/mru-bookmark.el ends here
