;;; x-dnd.el --- drag and drop support for X  -*- coding: utf-8 -*-

;; Copyright (C) 2004-2012  Free Software Foundation, Inc.

;; Author: Jan Djärv <jan.h.d@swipnet.se>
;; Maintainer: FSF
;; Keywords: window, drag, drop
;; Package: emacs

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

;; This file provides the drop part only.  Currently supported protocols
;; are XDND, Motif and the old KDE 1.x protocol.

;;; Code:

(require 'dnd)

;;; Customizable variables
(defcustom x-dnd-test-function 'x-dnd-default-test-function
  "The function drag and drop uses to determine if to accept or reject a drop.
The function takes three arguments, WINDOW, ACTION and TYPES.
WINDOW is where the mouse is when the function is called.  WINDOW may be a
frame if the mouse isn't over a real window (i.e. menu bar, tool bar or
scroll bar).  ACTION is the suggested action from the drag and drop source,
one of the symbols move, copy, link or ask.  TYPES is a list of available
types for the drop.

The function shall return nil to reject the drop or a cons with two values,
the wanted action as car and the wanted type as cdr.  The wanted action
can be copy, move, link, ask or private.
The default value for this variable is `x-dnd-default-test-function'."
  :version "22.1"
  :type 'symbol
  :group 'x)



(defcustom x-dnd-types-alist
  `(
    (,(purecopy "text/uri-list") . x-dnd-handle-uri-list)
    (,(purecopy "text/x-moz-url") . x-dnd-handle-moz-url)
    (,(purecopy "_NETSCAPE_URL") . x-dnd-handle-uri-list)
    (,(purecopy "FILE_NAME") . x-dnd-handle-file-name)
    (,(purecopy "UTF8_STRING") . x-dnd-insert-utf8-text)
    (,(purecopy "text/plain;charset=UTF-8") . x-dnd-insert-utf8-text)
    (,(purecopy "text/plain;charset=utf-8") . x-dnd-insert-utf8-text)
    (,(purecopy "text/unicode") . x-dnd-insert-utf16-text)
    (,(purecopy "text/plain") . dnd-insert-text)
    (,(purecopy "COMPOUND_TEXT") . x-dnd-insert-ctext)
    (,(purecopy "STRING") . dnd-insert-text)
    (,(purecopy "TEXT")   . dnd-insert-text)
    )
  "Which function to call to handle a drop of that type.
If the type for the drop is not present, or the function is nil,
the drop is rejected.  The function takes three arguments, WINDOW, ACTION
and DATA.  WINDOW is where the drop occurred, ACTION is the action for
this drop (copy, move, link, private or ask) as determined by a previous
call to `x-dnd-test-function'.  DATA is the drop data.
The function shall return the action used (copy, move, link or private)
if drop is successful, nil if not."
  :version "22.1"
  :type 'alist
  :group 'x)

(defcustom x-dnd-known-types
  (mapcar 'purecopy
  '("text/uri-list"
    "text/x-moz-url"
    "_NETSCAPE_URL"
    "FILE_NAME"
    "UTF8_STRING"
    "text/plain;charset=UTF-8"
    "text/plain;charset=utf-8"
    "text/unicode"
    "text/plain"
    "COMPOUND_TEXT"
    "STRING"
    "TEXT"
    ))
  "The types accepted by default for dropped data.
The types are chosen in the order they appear in the list."
  :version "22.1"
  :type '(repeat string)
  :group 'x
)

;; Internal variables

(defvar x-dnd-current-state nil
  "The current state for a drop.
This is an alist with one entry for each display.  The value for each display
is a vector that contains the state for drag and drop for that display.
Elements in the vector are:
Last buffer drag was in,
last window drag was in,
types available for drop,
the action suggested by the source,
the type we want for the drop,
the action we want for the drop,
any protocol specific data.")

(defvar x-dnd-empty-state [nil nil nil nil nil nil nil])

(declare-function x-register-dnd-atom "xselect.c")

(defun x-dnd-init-frame (&optional frame)
  "Setup drag and drop for FRAME (i.e. create appropriate properties)."
  (when (eq 'x (window-system frame))
    (x-register-dnd-atom "DndProtocol" frame)
    (x-register-dnd-atom "_MOTIF_DRAG_AND_DROP_MESSAGE" frame)
    (x-register-dnd-atom "XdndEnter" frame)
    (x-register-dnd-atom "XdndPosition" frame)
    (x-register-dnd-atom "XdndLeave" frame)
    (x-register-dnd-atom "XdndDrop" frame)
    (x-dnd-init-xdnd-for-frame frame)
    (x-dnd-init-motif-for-frame frame)))

(defun x-dnd-get-state-cons-for-frame (frame-or-window)
  "Return the entry in `x-dnd-current-state' for a frame or window."
  (let* ((frame (if (framep frame-or-window) frame-or-window
		  (window-frame frame-or-window)))
	 (display (frame-parameter frame 'display)))
    (if (not (assoc display x-dnd-current-state))
	(push (cons display (copy-sequence x-dnd-empty-state))
	      x-dnd-current-state))
    (assoc display x-dnd-current-state)))

(defun x-dnd-get-state-for-frame (frame-or-window)
  "Return the state in `x-dnd-current-state' for a frame or window."
  (cdr (x-dnd-get-state-cons-for-frame frame-or-window)))

(defun x-dnd-default-test-function (_window _action types)
  "The default test function for drag and drop.
WINDOW is where the mouse is when this function is called.  It may be
a frame if the mouse is over the menu bar, scroll bar or tool bar.
ACTION is the suggested action from the source, and TYPES are the
types the drop data can have.  This function only accepts drops with
types in `x-dnd-known-types'.  It always returns the action private."
  (let ((type (x-dnd-choose-type types)))
    (when type (cons 'private type))))


(defun x-dnd-current-type (frame-or-window)
  "Return the type we want the DND data to be in for the current drop.
FRAME-OR-WINDOW is the frame or window that the mouse is over."
  (aref (x-dnd-get-state-for-frame frame-or-window) 4))

(defun x-dnd-forget-drop (frame-or-window)
  "Remove all state for the last drop.
FRAME-OR-WINDOW is the frame or window that the mouse is over."
  (setcdr (x-dnd-get-state-cons-for-frame frame-or-window)
	  (copy-sequence x-dnd-empty-state)))

(defun x-dnd-maybe-call-test-function (window action)
  "Call `x-dnd-test-function' if something has changed.
WINDOW is the window the mouse is over.  ACTION is the suggested
action from the source.  If nothing has changed, return the last
action and type we got from `x-dnd-test-function'."
  (let ((buffer (when (window-live-p window)
		  (window-buffer window)))
	(current-state (x-dnd-get-state-for-frame window)))
    (unless (and (equal buffer (aref current-state 0))
                 (equal window (aref current-state 1))
                 (equal action (aref current-state 3)))
      (save-current-buffer
	(when buffer (set-buffer buffer))
	(let* ((action-type (funcall x-dnd-test-function
				     window
				     action
				     (aref current-state 2)))
	       (handler (cdr (assoc (cdr action-type) x-dnd-types-alist))))
	  ;; Ignore action-type if we have no handler.
	  (setq current-state
		(x-dnd-save-state window
				  action
				  (when handler action-type)))))))
  (let ((current-state (x-dnd-get-state-for-frame window)))
    (cons (aref current-state 5)
	  (aref current-state 4))))

(defun x-dnd-save-state (window action action-type &optional types extra-data)
  "Save the state of the current drag and drop.
WINDOW is the window the mouse is over.  ACTION is the action suggested
by the source.  ACTION-TYPE is the result of calling `x-dnd-test-function'.
If given, TYPES are the types for the drop data that the source supports.
EXTRA-DATA is data needed for a specific protocol."
  (let ((current-state (x-dnd-get-state-for-frame window)))
    (aset current-state 5 (car action-type))
    (aset current-state 4 (cdr action-type))
    (aset current-state 3 action)
    (when types (aset current-state 2 types))
    (when extra-data (aset current-state 6 extra-data))
    (aset current-state 1 window)
    (aset current-state 0 (and (window-live-p window) (window-buffer window)))
    (setcdr (x-dnd-get-state-cons-for-frame window) current-state)))


(defun x-dnd-handle-moz-url (window action data)
  "Handle one item of type text/x-moz-url.
WINDOW is the window where the drop happened.  ACTION is ignored.
DATA is the moz-url, which is formatted as two strings separated by \\r\\n.
The first string is the URL, the second string is the title of that URL.
DATA is encoded in utf-16.  Decode the URL and call `x-dnd-handle-uri-list'."
  ;; Mozilla and applications based on it (Galeon for example) uses
  ;; text/unicode, but it is impossible to tell if it is le or be.  Use what
  ;; the machine Emacs runs on use.  This loses if dropping between machines
  ;; with different endian, but it is the best we can do.
  (let* ((coding (if (eq (byteorder) ?B) 'utf-16be 'utf-16le))
	 (string (decode-coding-string data coding))
	 (strings (split-string string "[\r\n]" t))
	 ;; Can one drop more than one moz-url ??  Assume not.
	 (url (car strings)))
    (x-dnd-handle-uri-list window action url)))

(defun x-dnd-insert-utf8-text (window action text)
  "Decode the UTF-8 text and insert it at point.
TEXT is the text as a string, WINDOW is the window where the drop happened."
  (dnd-insert-text window action (decode-coding-string text 'utf-8)))

(defun x-dnd-insert-utf16-text (window action text)
  "Decode the UTF-16 text and insert it at point.
TEXT is the text as a string, WINDOW is the window where the drop happened."
  ;; See comment in x-dnd-handle-moz-url about coding.
  (let ((coding (if (eq (byteorder) ?B) 'utf-16be 'utf-16le)))
    (dnd-insert-text window action (decode-coding-string text coding))))

(defun x-dnd-insert-ctext (window action text)
  "Decode the compound text and insert it at point.
TEXT is the text as a string, WINDOW is the window where the drop happened."
  (dnd-insert-text window action
		   (decode-coding-string text
					 'compound-text-with-extensions)))

(defun x-dnd-handle-uri-list (window action string)
  "Split an uri-list into separate URIs and call `dnd-handle-one-url'.
WINDOW is the window where the drop happened.
STRING is the uri-list as a string.  The URIs are separated by \\r\\n."
  (let ((uri-list (split-string string "[\0\r\n]" t))
	retval)
    (dolist (bf uri-list)
      ;; If one URL is handled, treat as if the whole drop succeeded.
      (let ((did-action (dnd-handle-one-url window action bf)))
	(when did-action (setq retval did-action))))
    retval))

(defun x-dnd-handle-file-name (window action string)
  "Convert file names to URLs and call `dnd-handle-one-url'.
WINDOW is the window where the drop happened.
STRING is the file names as a string, separated by nulls."
  (let ((uri-list (split-string string "[\0\r\n]" t))
	(coding (and (default-value 'enable-multibyte-characters)
		     (or file-name-coding-system
			 default-file-name-coding-system)))
	retval)
    (dolist (bf uri-list)
      ;; If one URL is handled, treat as if the whole drop succeeded.
      (if coding (setq bf (encode-coding-string bf coding)))
      (let* ((file-uri (concat "file://"
			       (mapconcat 'url-hexify-string
					  (split-string bf "/") "/")))
	     (did-action (dnd-handle-one-url window action file-uri)))
	(when did-action (setq retval did-action))))
    retval))


(defun x-dnd-choose-type (types &optional known-types)
  "Choose which type we want to receive for the drop.
TYPES are the types the source of the drop offers, a vector of type names
as strings or symbols.  Select among the types in `x-dnd-known-types' or
KNOWN-TYPES if given, and return that type name.
If no suitable type is found, return nil."
  (let* ((known-list (or known-types x-dnd-known-types))
	 (first-known-type (car known-list))
	 (types-array types)
	 (found (when first-known-type
		  (catch 'done
		    (dotimes (i (length types-array))
		      (let* ((type (aref types-array i))
			     (typename (if (symbolp type)
					   (symbol-name type) type)))
			(when (equal first-known-type typename)
			  (throw 'done first-known-type))))
		    nil))))

    (if (and (not found) (cdr known-list))
	(x-dnd-choose-type types (cdr known-list))
      found)))

(defun x-dnd-drop-data (event frame window data type)
  "Drop one data item onto a frame.
EVENT is the client message for the drop, FRAME is the frame the drop
occurred on.  WINDOW is the window of FRAME where the drop happened.
DATA is the data received from the source, and type is the type for DATA,
see `x-dnd-types-alist').

Returns the action used (move, copy, link, private) if drop was successful,
nil if not."
  (let* ((type-info (assoc type x-dnd-types-alist))
	 (handler (cdr type-info))
	 (state (x-dnd-get-state-for-frame frame))
	 (action (aref state 5))
	 (w (posn-window (event-start event))))
    (when handler
      (if (and (window-live-p w)
	       (not (window-minibuffer-p w))
	       (not (window-dedicated-p w)))
	  ;; If dropping in an ordinary window which we could use,
	  ;; let dnd-open-file-other-window specify what to do.
	  (progn
	    (when (not mouse-yank-at-point)
	      (goto-char (posn-point (event-start event))))
	    (funcall handler window action data))
	;; If we can't display the file here,
	;; make a new window for it.
	(let ((dnd-open-file-other-window t))
	  (select-frame frame)
	  (funcall handler window action data))))))

(defun x-dnd-handle-drag-n-drop-event (event)
  "Receive drag and drop events (X client messages).
Currently XDND, Motif and old KDE 1.x protocols are recognized."
  (interactive "e")
  (let* ((client-message (car (cdr (cdr event))))
	 (window (posn-window (event-start event)))
	 (message-atom (aref client-message 0))
	 (frame (aref client-message 1))
	 (format (aref client-message 2))
	 (data (aref client-message 3)))

    (cond ((equal "DndProtocol" message-atom)	; Old KDE 1.x.
	   (x-dnd-handle-old-kde event frame window message-atom format data))

	  ((equal "_MOTIF_DRAG_AND_DROP_MESSAGE" message-atom)	; Motif
	   (x-dnd-handle-motif event frame window message-atom format data))

	  ((and (> (length message-atom) 4)	; XDND protocol.
		(equal "Xdnd" (substring message-atom 0 4)))
	   (x-dnd-handle-xdnd event frame window message-atom format data)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Old KDE protocol.  Only dropping of files.

(declare-function x-window-property "xfns.c"
		  (prop &optional frame type source delete-p vector-ret-p))

(defun x-dnd-handle-old-kde (_event frame window _message _format _data)
  "Open the files in a KDE 1.x drop."
  (let ((values (x-window-property "DndSelection" frame nil 0 t)))
    (x-dnd-handle-uri-list window 'private
			   (replace-regexp-in-string "\0$" "" values))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  XDND protocol.

(defconst x-dnd-xdnd-to-action
  '(("XdndActionPrivate" . private)
    ("XdndActionCopy" . copy)
    ("XdndActionMove" . move)
    ("XdndActionLink" . link)
    ("XdndActionAsk" . ask))
  "Mapping from XDND action types to lisp symbols.")

(declare-function x-change-window-property "xfns.c"
		  (prop value &optional frame type format outer-P))

(defun x-dnd-init-xdnd-for-frame (frame)
  "Set the XdndAware property for FRAME to indicate that we do XDND."
  (x-change-window-property "XdndAware"
			    '(5)	;; The version of XDND we support.
			    frame "ATOM" 32 t))

(defun x-dnd-get-drop-width-height (frame w accept)
  "Return the width/height to be sent in a XDndStatus message.
FRAME is the frame and W is the window where the drop happened.
If ACCEPT is nil return 0 (empty rectangle),
otherwise if W is a window, return its width/height,
otherwise return the frame width/height."
  (if accept
      (if (windowp w)   ;; w is not a window if dropping on the menu bar,
			;; scroll bar or tool bar.
	  (let ((edges (window-inside-pixel-edges w)))
	    (cons
	     (- (nth 2 edges) (nth 0 edges))	;; right - left
	     (- (nth 3 edges) (nth 1 edges))))	;; bottom - top
	(cons (frame-pixel-width frame)
	      (frame-pixel-height frame)))
    0))

(defun x-dnd-get-drop-x-y (frame w)
  "Return the x/y coordinates to be sent in a XDndStatus message.
Coordinates are required to be absolute.
FRAME is the frame and W is the window where the drop happened.
If W is a window, return its absolute coordinates,
otherwise return the frame coordinates."
  (let* ((frame-left (frame-parameter frame 'left))
	 ;; If the frame is outside the display, frame-left looks like
	 ;; '(0 -16).  Extract the -16.
	 (frame-real-left (if (consp frame-left) (car (cdr frame-left))
			    frame-left))
	 (frame-top (frame-parameter frame 'top))
	 (frame-real-top (if (consp frame-top) (car (cdr frame-top))
			   frame-top)))
    (if (windowp w)
	(let ((edges (window-inside-pixel-edges w)))
	  (cons
	   (+ frame-real-left (nth 0 edges))
	   (+ frame-real-top (nth 1 edges))))
      (cons frame-real-left frame-real-top))))

(declare-function x-get-atom-name "xselect.c" (value &optional frame))
(declare-function x-send-client-message "xselect.c"
		  (display dest from message-type format values))
(declare-function x-get-selection-internal "xselect.c"
		  (selection-symbol target-type &optional time-stamp terminal))

(defun x-dnd-version-from-flags (flags)
  "Return the version byte from the 32 bit FLAGS in an XDndEnter message"
  (if (consp flags)   ;; Long as cons
      (ash (car flags) -8)
    (ash flags -24))) ;; Ordinary number

(defun x-dnd-more-than-3-from-flags (flags)
  "Return the nmore-than3 bit from the 32 bit FLAGS in an XDndEnter message"
  (if (consp flags)
      (logand (cdr flags) 1)
    (logand flags 1)))

(defun x-dnd-handle-xdnd (event frame window message _format data)
  "Receive one XDND event (client message) and send the appropriate reply.
EVENT is the client message.  FRAME is where the mouse is now.
WINDOW is the window within FRAME where the mouse is now.
FORMAT is 32 (not used).  MESSAGE is the data part of an XClientMessageEvent."
  (cond ((equal "XdndEnter" message)
	 (let* ((flags (aref data 1))
		(version (x-dnd-version-from-flags flags))
		(more-than-3 (x-dnd-more-than-3-from-flags flags))
		(dnd-source (aref data 0)))
	(message "%s %s" version  more-than-3)
	   (if version  ;; If flags is bad, version will be nil.
	       (x-dnd-save-state
		window nil nil
		(if (> more-than-3 0)
		    (x-window-property "XdndTypeList"
				       frame "AnyPropertyType"
				       dnd-source nil t)
		  (vector (x-get-atom-name (aref data 2))
			  (x-get-atom-name (aref data 3))
			  (x-get-atom-name (aref data 4))))))))

	((equal "XdndPosition" message)
	 (let* ((action (x-get-atom-name (aref data 4)))
		(dnd-source (aref data 0))
		(action-type (x-dnd-maybe-call-test-function
			      window
			      (cdr (assoc action x-dnd-xdnd-to-action))))
		(reply-action (car (rassoc (car action-type)
					   x-dnd-xdnd-to-action)))
		(accept ;; 1 = accept, 0 = reject
		 (if (and reply-action action-type) 1 0))
		(list-to-send
		 (list (string-to-number
			(frame-parameter frame 'outer-window-id))
		       accept ;; 1 = Accept, 0 = reject.
		       (x-dnd-get-drop-x-y frame window)
		       (x-dnd-get-drop-width-height
			frame window (eq accept 1))
		       (or reply-action 0)
		       )))
	   (x-send-client-message
	    frame dnd-source frame "XdndStatus" 32 list-to-send)
	   ))

	((equal "XdndLeave" message)
	 (x-dnd-forget-drop window))

	((equal "XdndDrop" message)
	 (if (windowp window) (select-window window))
	 (let* ((dnd-source (aref data 0))
		(value (and (x-dnd-current-type window)
			    (x-get-selection-internal
			     'XdndSelection
			     (intern (x-dnd-current-type window)))))
		success action)

	   (setq action (if value
			    (condition-case info
				(x-dnd-drop-data event frame window value
						 (x-dnd-current-type window))
			      (error
			       (message "Error: %s" info)
			       nil))))

	   (setq success (if action 1 0))

	   (x-send-client-message
	    frame dnd-source frame "XdndFinished" 32
	    (list (string-to-number (frame-parameter frame 'outer-window-id))
		  success	;; 1 = Success, 0 = Error
		  (if success "XdndActionPrivate" 0)
		  ))
	   (x-dnd-forget-drop window)))

	(t (error "Unknown XDND message %s %s" message data))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Motif protocol.

(defun x-dnd-init-motif-for-frame (frame)
  "Set _MOTIF_DRAG_RECEIVER_INFO for FRAME to indicate that we do Motif DND."
  (x-change-window-property "_MOTIF_DRAG_RECEIVER_INFO"
			    (list
			     (byteorder)
			     0			; The Motif DND version.
			     5			; We want drag dynamic.
			     0 0 0 0 0 0 0
			     0 0 0 0 0 0)	; Property must be 16 bytes.
			    frame "_MOTIF_DRAG_RECEIVER_INFO" 8 t))

(defun x-dnd-get-motif-value (data offset size byteorder)
  (cond ((eq size 2)
	 (if (eq byteorder ?l)
	     (+ (ash (aref data (1+ offset)) 8)
		(aref data offset))
	   (+ (ash (aref data offset) 8)
	      (aref data (1+ offset)))))

	((eq size 4)
	 (if (eq byteorder ?l)
	     (cons (+ (ash (aref data (+ 3 offset)) 8)
		      (aref data (+ 2 offset)))
		   (+ (ash (aref data (1+ offset)) 8)
		      (aref data offset)))
	   (cons (+ (ash (aref data offset) 8)
		    (aref data (1+ offset)))
		 (+ (ash (aref data (+ 2 offset)) 8)
		    (aref data (+ 3 offset))))))))

(defun x-dnd-motif-value-to-list (value size byteorder)
  (let ((bytes (cond ((eq size 2)
		      (list (logand (lsh value -8) ?\xff)
			    (logand value ?\xff)))

		     ((eq size 4)
		      (if (consp value)
			  (list (logand (lsh (car value) -8) ?\xff)
				(logand (car value) ?\xff)
				(logand (lsh (cdr value) -8) ?\xff)
				(logand (cdr value) ?\xff))
			(list (logand (lsh value -24) ?\xff)
			      (logand (lsh value -16) ?\xff)
			      (logand (lsh value -8) ?\xff)
			      (logand value ?\xff)))))))
    (if (eq byteorder ?l)
	(reverse bytes)
      bytes)))


(defvar x-dnd-motif-message-types
  '((0 . XmTOP_LEVEL_ENTER)
    (1 . XmTOP_LEVEL_LEAVE)
    (2 . XmDRAG_MOTION)
    (3 . XmDROP_SITE_ENTER)
    (4 . XmDROP_SITE_LEAVE)
    (5 . XmDROP_START)
    (6 . XmDROP_FINISH)
    (7 . XmDRAG_DROP_FINISH)
    (8 . XmOPERATION_CHANGED))
  "Mapping from numbers to Motif DND message types.")

(defvar x-dnd-motif-to-action
  '((1 . move)
    (2 . copy)
    (3 . link)	; Both 3 and 4 has been seen as link.
    (4 . link)
    (2 . private)) ; Motif does not have private, so use copy for private.
  "Mapping from number to operation for Motif DND.")

(defun x-dnd-handle-motif (event frame window message-atom _format data)
  (let* ((message-type (cdr (assoc (aref data 0) x-dnd-motif-message-types)))
	 (source-byteorder (aref data 1))
	 (my-byteorder (byteorder))
	 (source-flags (x-dnd-get-motif-value data 2 2 source-byteorder))
	 (source-action (cdr (assoc (logand ?\xF source-flags)
				    x-dnd-motif-to-action))))

    (cond ((eq message-type 'XmTOP_LEVEL_ENTER)
	   (let* ((dnd-source (x-dnd-get-motif-value
			       data 8 4 source-byteorder))
		  (selection-atom (x-dnd-get-motif-value
				   data 12 4 source-byteorder))
		  (atom-name (x-get-atom-name selection-atom))
		  (types (when atom-name
			   (x-get-selection-internal (intern atom-name)
						     'TARGETS))))
	     (x-dnd-forget-drop frame)
	     (when types (x-dnd-save-state window nil nil
					   types
					   dnd-source))))

	  ;; Can not forget drop here, LEAVE comes before DROP_START and
	  ;; we need the state in DROP_START.
	  ((eq message-type 'XmTOP_LEVEL_LEAVE)
	   nil)

	  ((eq message-type 'XmDRAG_MOTION)
	   (let* ((state (x-dnd-get-state-for-frame frame))
		  (timestamp (x-dnd-motif-value-to-list
			      (x-dnd-get-motif-value data 4 4
						     source-byteorder)
			      4 my-byteorder))
		  (x (x-dnd-motif-value-to-list
		      (x-dnd-get-motif-value data 8 2 source-byteorder)
		      2 my-byteorder))
		  (y (x-dnd-motif-value-to-list
		      (x-dnd-get-motif-value data 10 2 source-byteorder)
		      2 my-byteorder))
		  (dnd-source (aref state 6))
		  (first-move (not (aref state 3)))
		  (action-type (x-dnd-maybe-call-test-function
				window
				source-action))
		  (reply-action (car (rassoc (car action-type)
					     x-dnd-motif-to-action)))
		  (reply-flags
		   (x-dnd-motif-value-to-list
		    (if reply-action
			(+ reply-action
			   ?\x30	; 30:  valid drop site
			   ?\x700)	; 700: can do copy, move or link
		      ?\x30)		; 30:  drop site, but noop.
		    2 my-byteorder))
		  (reply (append
			  (list
			   (+ ?\x80	; 0x80 indicates a reply.
			      (if first-move
				  3	; First time, reply is SITE_ENTER.
				2))	; Not first time, reply is DRAG_MOTION.
			   my-byteorder)
			  reply-flags
			  timestamp
			  x
			  y)))
	     (x-send-client-message frame
				    dnd-source
				    frame
				    "_MOTIF_DRAG_AND_DROP_MESSAGE"
				    8
				    reply)))

	  ((eq message-type 'XmOPERATION_CHANGED)
	   (let* ((state (x-dnd-get-state-for-frame frame))
		  (timestamp (x-dnd-motif-value-to-list
			      (x-dnd-get-motif-value data 4 4 source-byteorder)
			      4 my-byteorder))
		  (dnd-source (aref state 6))
		  (action-type (x-dnd-maybe-call-test-function
				window
				source-action))
		  (reply-action (car (rassoc (car action-type)
					     x-dnd-motif-to-action)))
		  (reply-flags
		   (x-dnd-motif-value-to-list
		    (if reply-action
			(+ reply-action
			   ?\x30	; 30:  valid drop site
			   ?\x700)	; 700: can do copy, move or link
		      ?\x30)		; 30:  drop site, but noop
		    2 my-byteorder))
		  (reply (append
			  (list
			   (+ ?\x80	; 0x80 indicates a reply.
			      8)	; 8 is OPERATION_CHANGED
			   my-byteorder)
			  reply-flags
			  timestamp)))
	     (x-send-client-message frame
				    dnd-source
				    frame
				    "_MOTIF_DRAG_AND_DROP_MESSAGE"
				    8
				    reply)))

	  ((eq message-type 'XmDROP_START)
	   (let* ((x (x-dnd-motif-value-to-list
		      (x-dnd-get-motif-value data 8 2 source-byteorder)
		      2 my-byteorder))
		  (y (x-dnd-motif-value-to-list
		      (x-dnd-get-motif-value data 10 2 source-byteorder)
		      2 my-byteorder))
		  (selection-atom (x-dnd-get-motif-value
				   data 12 4 source-byteorder))
		  (atom-name (x-get-atom-name selection-atom))
		  (dnd-source (x-dnd-get-motif-value
			       data 16 4 source-byteorder))
		  (action-type (x-dnd-maybe-call-test-function
				window
				source-action))
		  (reply-action (car (rassoc (car action-type)
					     x-dnd-motif-to-action)))
		  (reply-flags
		   (x-dnd-motif-value-to-list
		    (if reply-action
			(+ reply-action
			   ?\x30	; 30:  valid drop site
			   ?\x700)	; 700: can do copy, move or link
		      (+ ?\x30		; 30:  drop site, but noop.
			 ?\x200))	; 200: drop cancel.
		    2 my-byteorder))
		  (reply (append
			  (list
			   (+ ?\x80	; 0x80 indicates a reply.
			      5)	; DROP_START.
			   my-byteorder)
			  reply-flags
			  x
			  y))
		  (timestamp (x-dnd-get-motif-value
			      data 4 4 source-byteorder))
		  action)

	     (x-send-client-message frame
				    dnd-source
				    frame
				    "_MOTIF_DRAG_AND_DROP_MESSAGE"
				    8
				    reply)
	     (setq action
		   (when (and reply-action atom-name)
		     (let* ((value (x-get-selection-internal
				    (intern atom-name)
				    (intern (x-dnd-current-type window)))))
		       (when value
			 (condition-case info
			     (x-dnd-drop-data event frame window value
					      (x-dnd-current-type window))
			   (error
			    (message "Error: %s" info)
			    nil))))))
	     (x-get-selection-internal
	      (intern atom-name)
	      (if action 'XmTRANSFER_SUCCESS 'XmTRANSFER_FAILURE)
	      timestamp)
	     (x-dnd-forget-drop frame)))

	  (t (error "Unknown Motif DND message %s %s" message-atom data)))))


;;;

(provide 'x-dnd)

;;; x-dnd.el ends here
