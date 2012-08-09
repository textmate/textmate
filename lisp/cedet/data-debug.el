;;; data-debug.el --- Datastructure Debugger

;; Copyright (C) 2007-2012  Free Software Foundation, Inc.

;; Author: Eric M. Ludlam  <zappo@gnu.org>
;; Version: 0.2
;; Keywords: OO, lisp
;; Package: cedet

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
;; Provide a simple way to investigate particularly large and complex
;; data structures.
;;
;; The best way to get started is to bind M-: to 'data-debug-eval-expression.
;;
;; (global-set-key "\M-:" 'data-debug-eval-expression)
;;
;; If you write functions with complex output that need debugging, you
;; can make them interactive with data-debug-show-stuff.  For example:
;;
;; (defun my-complex-output-fcn ()
;;   "Calculate something complicated at point, and return it."
;;   (interactive) ;; function not normally interactive
;;   (let ((stuff (do-stuff)))
;;     (when (interactive-p)
;;       (data-debug-show-stuff stuff "myStuff"))
;;     stuff))

(require 'font-lock)
(require 'ring)

;;; Code:

;;; Compatibility
;;
(if (featurep 'xemacs)
    (eval-and-compile
      (defalias 'data-debug-overlay-properties 'extent-properties)
      (defalias 'data-debug-overlay-p 'extentp)
      (if (not (fboundp 'propertize))
	  (defun dd-propertize (string &rest properties)
	    "Mimic 'propertize' in from Emacs 23."
	    (add-text-properties 0 (length string) properties string)
	    string
	    )
	(defalias 'dd-propertize 'propertize))
      )
  ;; Regular Emacs
  (eval-and-compile
    (defalias 'data-debug-overlay-properties 'overlay-properties)
    (defalias 'data-debug-overlay-p 'overlayp)
    (defalias 'dd-propertize 'propertize)
    )
  )

;;; GENERIC STUFF
;;
(defun data-debug-insert-property-list (proplist prefix &optional parent)
  "Insert the property list PROPLIST.
Each line starts with PREFIX.
The attributes belong to the tag PARENT."
  (while proplist
    (let ((pretext (concat (symbol-name (car proplist)) " : ")))
      (data-debug-insert-thing (car (cdr proplist))
			       prefix
			       pretext
			       parent))
    (setq proplist (cdr (cdr proplist)))))

;;; overlays
;;
(defun data-debug-insert-overlay-props (overlay prefix)
  "Insert all the parts of OVERLAY.
PREFIX specifies what to insert at the start of each line."
  (let ((attrprefix (concat (make-string (length prefix) ? ) "# "))
	(proplist (data-debug-overlay-properties overlay)))
    (data-debug-insert-property-list
     proplist attrprefix)
    )
  )

(defun data-debug-insert-overlay-from-point (point)
  "Insert the overlay found at the overlay button at POINT."
  (let ((overlay (get-text-property point 'ddebug))
	(indent (get-text-property point 'ddebug-indent))
	start
	)
    (end-of-line)
    (setq start (point))
    (forward-char 1)
    (data-debug-insert-overlay-props overlay
				     (concat (make-string indent ? )
					     "| "))
    (goto-char start)
    ))

(defun data-debug-insert-overlay-button (overlay prefix prebuttontext)
  "Insert a button representing OVERLAY.
PREFIX is the text that precedes the button.
PREBUTTONTEXT is some text between prefix and the overlay button."
  (let ((start (point))
	(end nil)
	(str (format "%s" overlay))
	(tip nil))
    (insert prefix prebuttontext str)
    (setq end (point))
    (put-text-property (- end (length str)) end 'face 'font-lock-comment-face)
    (put-text-property start end 'ddebug overlay)
    (put-text-property start end 'ddebug-indent(length prefix))
    (put-text-property start end 'ddebug-prefix prefix)
    (put-text-property start end 'help-echo tip)
    (put-text-property start end 'ddebug-function
		       'data-debug-insert-overlay-from-point)
    (insert "\n")
    )
  )

;;; overlay list
;;
(defun data-debug-insert-overlay-list (overlaylist prefix)
  "Insert all the parts of OVERLAYLIST.
PREFIX specifies what to insert at the start of each line."
  (while overlaylist
    (data-debug-insert-overlay-button (car overlaylist)
				      prefix
				      "")
    (setq overlaylist (cdr overlaylist))))

(defun data-debug-insert-overlay-list-from-point (point)
  "Insert the overlay found at the overlay list button at POINT."
  (let ((overlaylist (get-text-property point 'ddebug))
	(indent (get-text-property point 'ddebug-indent))
	start
	)
    (end-of-line)
    (setq start (point))
    (forward-char 1)
    (data-debug-insert-overlay-list overlaylist
				    (concat (make-string indent ? )
					    "* "))
    (goto-char start)
    ))

(defun data-debug-insert-overlay-list-button (overlaylist
					      prefix
					      prebuttontext)
  "Insert a button representing OVERLAYLIST.
PREFIX is the text that precedes the button.
PREBUTTONTEXT is some text between prefix and the overlay list button."
  (let ((start (point))
	(end nil)
	(str (format "#<overlay list: %d entries>" (length overlaylist)))
	(tip nil))
    (insert prefix prebuttontext str)
    (setq end (point))
    (put-text-property (- end (length str)) end 'face 'font-lock-comment-face)
    (put-text-property start end 'ddebug overlaylist)
    (put-text-property start end 'ddebug-indent(length prefix))
    (put-text-property start end 'ddebug-prefix prefix)
    (put-text-property start end 'help-echo tip)
    (put-text-property start end 'ddebug-function
		       'data-debug-insert-overlay-list-from-point)
    (insert "\n")
    )
  )

;;; buffers
;;
(defun data-debug-insert-buffer-props (buffer prefix)
  "Insert all the parts of BUFFER.
PREFIX specifies what to insert at the start of each line."
  (let ((attrprefix (concat (make-string (length prefix) ? ) "# "))
	(proplist
	 (list :filename (buffer-file-name buffer)
	       :live (buffer-live-p buffer)
	       :modified (buffer-modified-p buffer)
	       :size (buffer-size buffer)
	       :process (get-buffer-process buffer)
	       :localvars (buffer-local-variables buffer)
	       )))
    (data-debug-insert-property-list
     proplist attrprefix)
    )
  )

(defun data-debug-insert-buffer-from-point (point)
  "Insert the buffer found at the buffer button at POINT."
  (let ((buffer (get-text-property point 'ddebug))
	(indent (get-text-property point 'ddebug-indent))
	start
	)
    (end-of-line)
    (setq start (point))
    (forward-char 1)
    (data-debug-insert-buffer-props buffer
				     (concat (make-string indent ? )
					     "| "))
    (goto-char start)
    ))

(defun data-debug-insert-buffer-button (buffer prefix prebuttontext)
  "Insert a button representing BUFFER.
PREFIX is the text that precedes the button.
PREBUTTONTEXT is some text between prefix and the buffer button."
  (let ((start (point))
	(end nil)
	(str (format "%S" buffer))
	(tip nil))
    (insert prefix prebuttontext str)
    (setq end (point))
    (put-text-property (- end (length str)) end 'face 'font-lock-comment-face)
    (put-text-property start end 'ddebug buffer)
    (put-text-property start end 'ddebug-indent(length prefix))
    (put-text-property start end 'ddebug-prefix prefix)
    (put-text-property start end 'help-echo tip)
    (put-text-property start end 'ddebug-function
		       'data-debug-insert-buffer-from-point)
    (insert "\n")
    )
  )

;;; buffer list
;;
(defun data-debug-insert-buffer-list (bufferlist prefix)
  "Insert all the parts of BUFFERLIST.
PREFIX specifies what to insert at the start of each line."
  (while bufferlist
    (data-debug-insert-buffer-button (car bufferlist)
				      prefix
				      "")
    (setq bufferlist (cdr bufferlist))))

(defun data-debug-insert-buffer-list-from-point (point)
  "Insert the buffer found at the buffer list button at POINT."
  (let ((bufferlist (get-text-property point 'ddebug))
	(indent (get-text-property point 'ddebug-indent))
	start
	)
    (end-of-line)
    (setq start (point))
    (forward-char 1)
    (data-debug-insert-buffer-list bufferlist
				    (concat (make-string indent ? )
					    "* "))
    (goto-char start)
    ))

(defun data-debug-insert-buffer-list-button (bufferlist
					      prefix
					      prebuttontext)
  "Insert a button representing BUFFERLIST.
PREFIX is the text that precedes the button.
PREBUTTONTEXT is some text between prefix and the buffer list button."
  (let ((start (point))
	(end nil)
	(str (format "#<buffer list: %d entries>" (length bufferlist)))
	(tip nil))
    (insert prefix prebuttontext str)
    (setq end (point))
    (put-text-property (- end (length str)) end 'face 'font-lock-comment-face)
    (put-text-property start end 'ddebug bufferlist)
    (put-text-property start end 'ddebug-indent(length prefix))
    (put-text-property start end 'ddebug-prefix prefix)
    (put-text-property start end 'help-echo tip)
    (put-text-property start end 'ddebug-function
		       'data-debug-insert-buffer-list-from-point)
    (insert "\n")
    )
  )

;;; processes
;;
(defun data-debug-insert-process-props (process prefix)
  "Insert all the parts of PROCESS.
PREFIX specifies what to insert at the start of each line."
  (let ((attrprefix (concat (make-string (length prefix) ? ) "# "))
	(id (process-id process))
	(tty (process-tty-name process))
	(pcontact (process-contact process t))
	(proplist (process-plist process)))
    (data-debug-insert-property-list
     (append
      (if id (list 'id id))
      (if tty (list 'tty tty))
      (if pcontact pcontact)
      proplist)
     attrprefix)
    )
  )

(defun data-debug-insert-process-from-point (point)
  "Insert the process found at the process button at POINT."
  (let ((process (get-text-property point 'ddebug))
	(indent (get-text-property point 'ddebug-indent))
	start
	)
    (end-of-line)
    (setq start (point))
    (forward-char 1)
    (data-debug-insert-process-props process
				     (concat (make-string indent ? )
					     "| "))
    (goto-char start)
    ))

(defun data-debug-insert-process-button (process prefix prebuttontext)
  "Insert a button representing PROCESS.
PREFIX is the text that precedes the button.
PREBUTTONTEXT is some text between prefix and the process button."
  (let ((start (point))
	(end nil)
	(str (format "%S : %s" process (process-status process)))
	(tip nil))
    (insert prefix prebuttontext str)
    (setq end (point))
    (put-text-property (- end (length str)) end 'face 'font-lock-comment-face)
    (put-text-property start end 'ddebug process)
    (put-text-property start end 'ddebug-indent(length prefix))
    (put-text-property start end 'ddebug-prefix prefix)
    (put-text-property start end 'help-echo tip)
    (put-text-property start end 'ddebug-function
		       'data-debug-insert-process-from-point)
    (insert "\n")
    )
  )

;;; Rings
;;
;; A ring (like kill-ring, or whatever.)
(defun data-debug-insert-ring-contents (ring prefix)
  "Insert all the parts of RING.
PREFIX specifies what to insert at the start of each line."
  (let ((len (ring-length ring))
	(idx 0)
	)
    (while (< idx len)
      (data-debug-insert-thing (ring-ref ring idx) prefix "")
      (setq idx (1+ idx))
      )))

(defun data-debug-insert-ring-items-from-point (point)
  "Insert the ring found at the ring button at POINT."
  (let ((ring (get-text-property point 'ddebug))
	(indent (get-text-property point 'ddebug-indent))
	start
	)
    (end-of-line)
    (setq start (point))
    (forward-char 1)
    (data-debug-insert-ring-contents ring
				     (concat (make-string indent ? )
					     "} "))
    (goto-char start)
    ))

(defun data-debug-insert-ring-button (ring
				      prefix
				      prebuttontext)
  "Insert a button representing RING.
PREFIX is the text that precedes the button.
PREBUTTONTEXT is some text between prefix and the stuff list button."
  (let* ((start (point))
	 (end nil)
	 (str (format "#<RING: %d, %d max>"
		      (ring-length ring)
		      (ring-size ring)))
	 (ringthing
	  (if (= (ring-length ring) 0) nil (ring-ref ring 0)))
	 (tip (format "Ring max-size %d, length %d."
		      (ring-size ring)
		      (ring-length ring)))
	 )
    (insert prefix prebuttontext str)
    (setq end (point))
    (put-text-property (- end (length str)) end 'face 'font-lock-type-face)
    (put-text-property start end 'ddebug ring)
    (put-text-property start end 'ddebug-indent(length prefix))
    (put-text-property start end 'ddebug-prefix prefix)
    (put-text-property start end 'help-echo tip)
    (put-text-property start end 'ddebug-function
		       'data-debug-insert-ring-items-from-point)
    (insert "\n")
    )
  )


;;; Hash-table
;;

(defun data-debug-insert-hash-table (hash-table prefix)
  "Insert the contents of HASH-TABLE inserting PREFIX before each element."
  (maphash
   (lambda (key value)
     (data-debug-insert-thing
      key prefix
      (dd-propertize "key " 'face font-lock-comment-face))
     (data-debug-insert-thing
      value prefix
      (dd-propertize "val " 'face font-lock-comment-face)))
   hash-table))

(defun data-debug-insert-hash-table-from-point (point)
  "Insert the contents of the hash-table button at POINT."
  (let ((hash-table (get-text-property point 'ddebug))
	(indent     (get-text-property point 'ddebug-indent))
	start)
    (end-of-line)
    (setq start (point))
    (forward-char 1)
    (data-debug-insert-hash-table
     hash-table
     (concat (make-string indent ? ) "> "))
    (goto-char start))
  )

(defun data-debug-insert-hash-table-button (hash-table prefix prebuttontext)
  "Insert HASH-TABLE as expandable button with recursive prefix PREFIX and PREBUTTONTEXT in front of the button text."
  (let ((string (dd-propertize (format "%s" hash-table)
			    'face 'font-lock-keyword-face)))
    (insert (dd-propertize
	     (concat prefix prebuttontext string)
	     'ddebug        hash-table
	     'ddebug-indent (length prefix)
	     'ddebug-prefix prefix
	     'help-echo
	     (format "Hash-table\nTest: %s\nWeakness: %s\nElements: %d (of %d)"
		     (hash-table-test hash-table)
		     (if (hash-table-weakness hash-table) "yes" "no")
		     (hash-table-count hash-table)
		     (hash-table-size hash-table))
	     'ddebug-function
	     'data-debug-insert-hash-table-from-point)
	    "\n"))
  )

;;; Widget
;;
;; Widgets have a long list of properties
(defun data-debug-insert-widget-properties (widget prefix)
  "Insert the contents of WIDGET inserting PREFIX before each element."
  (let ((type (car widget))
	(rest (cdr widget)))
    (while rest
      (data-debug-insert-thing (car (cdr rest))
			       prefix
			       (concat
				(dd-propertize (format "%s" (car rest))
					       'face font-lock-comment-face)
				" : "))
      (setq rest (cdr (cdr rest))))
    ))

(defun data-debug-insert-widget-from-point (point)
  "Insert the contents of the widget button at POINT."
  (let ((widget (get-text-property point 'ddebug))
	(indent (get-text-property point 'ddebug-indent))
	start)
    (end-of-line)
    (setq start (point))
    (forward-char 1)
    (data-debug-insert-widget-properties
     widget (concat (make-string indent ? ) "# "))
    (goto-char start))
  )

(defun data-debug-insert-widget (widget prefix prebuttontext)
  "Insert one WIDGET.
A Symbol is a simple thing, but this provides some face and prefix rules.
PREFIX is the text that precedes the button.
PREBUTTONTEXT is some text between prefix and the thing."
  (let ((string (dd-propertize (format "#<WIDGET %s>" (car widget))
			       'face 'font-lock-keyword-face)))
    (insert (dd-propertize
	     (concat prefix prebuttontext string)
	     'ddebug        widget
	     'ddebug-indent (length prefix)
	     'ddebug-prefix prefix
	     'help-echo
	     (format "Widget\nType: %s\n# Properties: %d"
		     (car widget)
		     (/ (1- (length widget)) 2))
	     'ddebug-function
	     'data-debug-insert-widget-from-point)
	    "\n")))

;;; list of stuff
;;
;; just a list.  random stuff inside.
(defun data-debug-insert-stuff-list (stufflist prefix)
  "Insert all the parts of STUFFLIST.
PREFIX specifies what to insert at the start of each line."
  (while stufflist
    (data-debug-insert-thing
     ;; Some lists may put a value in the CDR
     (if (listp stufflist) (car stufflist) stufflist)
     prefix
     "")
    (setq stufflist
	  (if (listp stufflist)
	      (cdr-safe stufflist)
	    nil))))

(defun data-debug-insert-stuff-list-from-point (point)
  "Insert the stuff found at the stuff list button at POINT."
  (let ((stufflist (get-text-property point 'ddebug))
	(indent (get-text-property point 'ddebug-indent))
	start
	)
    (end-of-line)
    (setq start (point))
    (forward-char 1)
    (data-debug-insert-stuff-list stufflist
				  (concat (make-string indent ? )
					  "> "))
    (goto-char start)
    ))

(defun data-debug-insert-stuff-list-button (stufflist
					    prefix
					    prebuttontext)
  "Insert a button representing STUFFLIST.
PREFIX is the text that precedes the button.
PREBUTTONTEXT is some text between prefix and the stuff list button."
  (let ((start (point))
	(end nil)
	(str
	 (condition-case nil
	     (format "#<list o' stuff: %d entries>" (safe-length stufflist))
	   (error "#<list o' stuff>")))
	(tip (if (or (listp (car stufflist))
		     (vectorp (car stufflist)))
		 ""
	       (format "%s" stufflist))))
    (insert prefix prebuttontext str)
    (setq end (point))
    (put-text-property (- end (length str)) end 'face 'font-lock-variable-name-face)
    (put-text-property start end 'ddebug stufflist)
    (put-text-property start end 'ddebug-indent (length prefix))
    (put-text-property start end 'ddebug-prefix prefix)
    (put-text-property start end 'help-echo tip)
    (put-text-property start end 'ddebug-function
		       'data-debug-insert-stuff-list-from-point)
    (insert "\n")
    )
  )

;;; vector of stuff
;;
;; just a vector.  random stuff inside.
(defun data-debug-insert-stuff-vector (stuffvector prefix)
  "Insert all the parts of STUFFVECTOR.
PREFIX specifies what to insert at the start of each line."
  (let ((idx 0))
    (while (< idx (length stuffvector))
      (data-debug-insert-thing
       ;; Some vectors may put a value in the CDR
       (aref stuffvector idx)
       prefix
       "")
      (setq idx (1+ idx)))))

(defun data-debug-insert-stuff-vector-from-point (point)
  "Insert the stuff found at the stuff vector button at POINT."
  (let ((stuffvector (get-text-property point 'ddebug))
	(indent (get-text-property point 'ddebug-indent))
	start
	)
    (end-of-line)
    (setq start (point))
    (forward-char 1)
    (data-debug-insert-stuff-vector stuffvector
				  (concat (make-string indent ? )
					  "[ "))
    (goto-char start)
    ))

(defun data-debug-insert-stuff-vector-button (stuffvector
					    prefix
					    prebuttontext)
  "Insert a button representing STUFFVECTOR.
PREFIX is the text that precedes the button.
PREBUTTONTEXT is some text between prefix and the stuff vector button."
  (let* ((start (point))
	 (end nil)
	 (str (format "#<vector o' stuff: %d entries>" (length stuffvector)))
	 (tip str))
    (insert prefix prebuttontext str)
    (setq end (point))
    (put-text-property (- end (length str)) end 'face 'font-lock-variable-name-face)
    (put-text-property start end 'ddebug stuffvector)
    (put-text-property start end 'ddebug-indent (length prefix))
    (put-text-property start end 'ddebug-prefix prefix)
    (put-text-property start end 'help-echo tip)
    (put-text-property start end 'ddebug-function
		       'data-debug-insert-stuff-vector-from-point)
    (insert "\n")
    )
  )

;;; Symbol
;;

(defun data-debug-insert-symbol-from-point (point)
  "Insert attached properties and possibly the value of symbol at POINT."
  (let ((symbol (get-text-property point 'ddebug))
	(indent (get-text-property point 'ddebug-indent))
	start)
    (end-of-line)
    (setq start (point))
    (forward-char 1)
    (when (and (not (fboundp symbol)) (boundp symbol))
      (data-debug-insert-thing
       (symbol-value symbol)
       (concat (make-string indent ? ) "> ")
       (concat
	(dd-propertize "value"
		    'face 'font-lock-comment-face)
	" ")))
    (data-debug-insert-property-list
     (symbol-plist symbol)
     (concat (make-string indent ? ) "> "))
    (goto-char start))
  )

(defun data-debug-insert-symbol-button (symbol prefix prebuttontext)
  "Insert a button representing SYMBOL.
PREFIX is the text that precedes the button.
PREBUTTONTEXT is some text between prefix and the symbol button."
  (let ((string
	 (cond ((fboundp symbol)
		(dd-propertize (concat "#'" (symbol-name symbol))
			    'face 'font-lock-function-name-face))
	       ((boundp symbol)
		(dd-propertize (concat "'" (symbol-name symbol))
			    'face 'font-lock-variable-name-face))
	       (t (format "'%s" symbol)))))
    (insert (dd-propertize
	     (concat prefix prebuttontext string)
	     'ddebug          symbol
	     'ddebug-indent   (length prefix)
	     'ddebug-prefix   prefix
	     'help-echo       ""
	     'ddebug-function
	     'data-debug-insert-symbol-from-point)
	    "\n"))
  )

;;; String
(defun data-debug-insert-string (thing prefix prebuttontext)
  "Insert one symbol THING.
A Symbol is a simple thing, but this provides some face and prefix rules.
PREFIX is the text that precedes the button.
PREBUTTONTEXT is some text between prefix and the thing."
  (let ((newstr thing))
    (while (string-match "\n" newstr)
      (setq newstr (replace-match "\\n" t t newstr)))
    (while (string-match "\t" newstr)
      (setq newstr (replace-match "\\t" t t newstr)))
    (insert prefix prebuttontext
	    (dd-propertize (format "\"%s\"" newstr)
			'face font-lock-string-face)
	    "\n" )))

;;; Number
(defun data-debug-insert-number (thing prefix prebuttontext)
  "Insert one symbol THING.
A Symbol is a simple thing, but this provides some face and prefix rules.
PREFIX is the text that precedes the button.
PREBUTTONTEXT is some text between prefix and the thing."
  (insert prefix prebuttontext
	  (dd-propertize (format "%S" thing)
			 'face font-lock-string-face)
	  "\n"))

;;; Lambda Expression
(defun data-debug-insert-lambda-expression (thing prefix prebuttontext)
  "Insert one lambda expression THING.
A Symbol is a simple thing, but this provides some face and prefix rules.
PREFIX is the text that precedes the button.
PREBUTTONTEXT is some text between prefix and the thing."
  (let ((txt (prin1-to-string thing)))
    (data-debug-insert-simple-thing
     txt prefix prebuttontext 'font-lock-keyword-face))
  )

;;; nil thing
(defun data-debug-insert-nil (thing prefix prebuttontext)
  "Insert one simple THING with a face.
PREFIX is the text that precedes the button.
PREBUTTONTEXT is some text between prefix and the thing.
FACE is the face to use."
  (insert prefix prebuttontext)
  (insert ": ")
  (let ((start (point))
	(end nil))
    (insert "nil")
    (setq end (point))
    (insert "\n" )
    (put-text-property start end 'face 'font-lock-variable-name-face)
    ))

;;; simple thing
(defun data-debug-insert-simple-thing (thing prefix prebuttontext face)
  "Insert one simple THING with a face.
PREFIX is the text that precedes the button.
PREBUTTONTEXT is some text between prefix and the thing.
FACE is the face to use."
  (insert prefix prebuttontext)
  (let ((start (point))
	(end nil))
    (insert (format "%s" thing))
    (setq end (point))
    (insert "\n" )
    (put-text-property start end 'face face)
    ))

;;; custom thing
(defun data-debug-insert-custom (thingstring prefix prebuttontext face)
  "Insert one simple THINGSTRING with a face.
Use for simple items that need a custom insert.
PREFIX is the text that precedes the button.
PREBUTTONTEXT is some text between prefix and the thing.
FACE is the face to use."
  (insert prefix prebuttontext)
  (let ((start (point))
	(end nil))
    (insert thingstring)
    (setq end (point))
    (insert "\n" )
    (put-text-property start end 'face face)
    ))


(defvar data-debug-thing-alist
  '(
    ;; nil
    (null . data-debug-insert-nil)

    ;; Overlay
    (data-debug-overlay-p . data-debug-insert-overlay-button)

    ;; Overlay list
    ((lambda (thing) (and (consp thing) (data-debug-overlay-p (car thing)))) .
     data-debug-insert-overlay-list-button)

    ;; Buffer
    (bufferp . data-debug-insert-buffer-button)

    ;; Buffer list
    ((lambda (thing) (and (consp thing) (bufferp (car thing)))) .
     data-debug-insert-buffer-list-button)

    ;; Process
    (processp . data-debug-insert-process-button)

    ;; String
    (stringp . data-debug-insert-string)

    ;; Number
    (numberp . data-debug-insert-number)

    ;; Symbol
    (symbolp . data-debug-insert-symbol-button)

    ;; Ring
    (ring-p . data-debug-insert-ring-button)

    ;; Lambda Expression
    ((lambda (thing) (and (consp thing) (eq (car thing) 'lambda))) .
     data-debug-insert-lambda-expression)

    ;; Hash-table
    (hash-table-p . data-debug-insert-hash-table-button)

    ;; Widgets
    (widgetp . data-debug-insert-widget)

    ;; List of stuff
    (listp . data-debug-insert-stuff-list-button)

    ;; Vector of stuff
    (vectorp . data-debug-insert-stuff-vector-button)
    )
  "Alist of methods used to insert things into an Ddebug buffer.")

;; An augmentation function for the thing alist.
(defun data-debug-add-specialized-thing (predicate fcn)
  "Add a new specialized thing to display with data-debug.
PREDICATE is a function that returns t if a thing is this new type.
FCN is a function that will display stuff in the data debug buffer."
  (let ((entry (cons predicate fcn))
	;; Specialized entries show up AFTER nil,
	;; but before listp, vectorp, symbolp, and
	;; other general things.  Splice it into
	;; the beginning.
	(first (nthcdr 0 data-debug-thing-alist))
	(second (nthcdr 1 data-debug-thing-alist))
      )
  (when (not (member entry data-debug-thing-alist))
    (setcdr first (cons entry second)))))

;; uber insert method
(defun data-debug-insert-thing (thing prefix prebuttontext &optional parent)
  "Insert THING with PREFIX.
PREBUTTONTEXT is some text to insert between prefix and the thing
that is not included in the indentation calculation of any children.
If PARENT is non-nil, it is somehow related as a parent to thing."
  (when (catch 'done
	  (dolist (test data-debug-thing-alist)
	    (when (funcall (car test) thing)
	      (condition-case nil
		  (funcall (cdr test) thing prefix prebuttontext parent)
		(error
		 (funcall (cdr test) thing prefix prebuttontext)))
	      (throw 'done nil))
	    )
	  nil)
    (data-debug-insert-simple-thing (format "%S" thing)
				    prefix
				    prebuttontext
				    'bold)))

;;; MAJOR MODE
;;
;; The Ddebug major mode provides an interactive space to explore
;; complicated data structures.
;;
(defgroup data-debug nil
  "data-debug group."
  :group 'extensions)

(defvar data-debug-mode-syntax-table
  (let ((table (make-syntax-table (standard-syntax-table))))
    (modify-syntax-entry ?\; ". 12"  table) ;; SEMI, Comment start ;;
    (modify-syntax-entry ?\n ">"     table) ;; Comment end
    (modify-syntax-entry ?\" "\""    table) ;; String
    (modify-syntax-entry ?\- "_"     table) ;; Symbol
    (modify-syntax-entry ?\\ "\\"    table) ;; Quote
    (modify-syntax-entry ?\` "'"     table) ;; Prefix ` (backquote)
    (modify-syntax-entry ?\' "'"     table) ;; Prefix ' (quote)
    (modify-syntax-entry ?\, "'"     table) ;; Prefix , (comma)

    table)
  "Syntax table used in data-debug macro buffers.")

(defvar data-debug-map
  (let ((km (make-sparse-keymap)))
    (define-key km [mouse-2] 'data-debug-expand-or-contract-mouse)
    (define-key km " " 'data-debug-expand-or-contract)
    (define-key km "\C-m" 'data-debug-expand-or-contract)
    (define-key km "n" 'data-debug-next)
    (define-key km "p" 'data-debug-prev)
    (define-key km "N" 'data-debug-next-expando)
    (define-key km "P" 'data-debug-prev-expando)
    km)
  "Keymap used in data-debug.")

(defcustom data-debug-mode-hook nil
  "*Hook run when data-debug starts."
  :group 'data-debug
  :type 'hook)

(defun data-debug-mode ()
  "Major-mode for the Analyzer debugger.

\\{data-debug-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'data-debug-mode
        mode-name "DATA-DEBUG"
	comment-start ";;"
	comment-end "")
  (set (make-local-variable 'comment-start-skip)
       "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+ *")
  (set-syntax-table data-debug-mode-syntax-table)
  (use-local-map data-debug-map)
  (run-hooks 'data-debug-hook)
  (buffer-disable-undo)
  (set (make-local-variable 'font-lock-global-modes) nil)
  (font-lock-mode -1)
  )

;;;###autoload
(defun data-debug-new-buffer (name)
  "Create a new data-debug buffer with NAME."
  (let ((b (get-buffer-create name)))
    (pop-to-buffer b)
    (set-buffer b)
    (erase-buffer)
    (data-debug-mode)
    b))

;;; Ddebug mode commands
;;
(defun data-debug-next ()
  "Go to the next line in the Ddebug buffer."
  (interactive)
  (forward-line 1)
  (beginning-of-line)
  (skip-chars-forward " *-><[]" (point-at-eol)))

(defun data-debug-prev ()
  "Go to the previous line in the Ddebug buffer."
  (interactive)
  (forward-line -1)
  (beginning-of-line)
  (skip-chars-forward " *-><[]" (point-at-eol)))

(defun data-debug-next-expando ()
  "Go to the next line in the Ddebug buffer.
Contract the current line (if open) and expand the line
we move to."
  (interactive)
  (data-debug-contract-current-line)
  (data-debug-next)
  (data-debug-expand-current-line)
  )

(defun data-debug-prev-expando ()
  "Go to the previous line in the Ddebug buffer.
Contract the current line (if open) and expand the line
we move to."
  (interactive)
  (data-debug-contract-current-line)
  (data-debug-prev)
  (data-debug-expand-current-line)
  )

(defun data-debug-current-line-expanded-p ()
  "Return non-nil if the current line is expanded."
  (let ((ti (current-indentation))
	(ni (condition-case nil
		(save-excursion
		  (end-of-line)
		  (forward-char 1)
		  (current-indentation))
	      (error 0))))
    (> ni ti)))

(defun data-debug-line-expandable-p ()
  "Return non-nil if the current line is expandable.
Lines that are not expandable are assumed to not be contractible."
  (not (get-text-property (point) 'ddebug-noexpand)))

(defun data-debug-expand-current-line ()
  "Expand the current line (if possible).
Do nothing if already expanded."
  (when (or (not (data-debug-line-expandable-p))
	    (not (data-debug-current-line-expanded-p)))
    ;; If the next line is the same or less indentation, expand.
    (let ((fcn (get-text-property (point) 'ddebug-function)))
      (when fcn
	(funcall fcn (point))
	(beginning-of-line)
	))))

(defun data-debug-contract-current-line ()
  "Contract the current line (if possible).
Do nothing if already contracted."
  (when (and (data-debug-current-line-expanded-p)
	     ;; Don't contract if the current line is not expandable.
	     (get-text-property (point) 'ddebug-function))
    (let ((ti (current-indentation))
	  )
      ;; If next indentation is larger, collapse.
      (end-of-line)
      (forward-char 1)
      (let ((start (point))
	    (end nil))
	(condition-case nil
	    (progn
	      ;; Keep checking indentation
	      (while (or (> (current-indentation) ti)
			 (looking-at "^\\s-*$"))
		(end-of-line)
		(forward-char 1))
	      (setq end (point))
	      )
	  (error (setq end (point-max))))
	(delete-region start end)
	(forward-char -1)
	(beginning-of-line)))))

(defun data-debug-expand-or-contract ()
  "Expand or contract anything at the current point."
  (interactive)
  (if (and (data-debug-line-expandable-p)
	   (data-debug-current-line-expanded-p))
      (data-debug-contract-current-line)
    (data-debug-expand-current-line))
  (skip-chars-forward " *-><[]" (point-at-eol)))

(defun data-debug-expand-or-contract-mouse (event)
  "Expand or contract anything at event EVENT."
  (interactive "e")
  (let* ((win (car (car (cdr event))))
	 )
    (select-window win t)
    (save-excursion
      ;(goto-char (window-start win))
      (mouse-set-point event)
      (data-debug-expand-or-contract))
    ))

;;; GENERIC STRUCTURE DUMP
;;
(defun data-debug-show-stuff (stuff name)
  "Data debug STUFF in a buffer named *NAME DDebug*."
  (data-debug-new-buffer (concat "*" name " DDebug*"))
  (data-debug-insert-thing stuff "?" "")
  (goto-char (point-min))
  (when (data-debug-line-expandable-p)
    (data-debug-expand-current-line)))

;;; DEBUG COMMANDS
;;
;; Various commands for displaying complex data structures.

(defun data-debug-edebug-expr (expr)
  "Dump out the contents of some expression EXPR in edebug with ddebug."
  (interactive
   (list (let ((minibuffer-completing-symbol t))
	   (read-from-minibuffer "Eval: "
				 nil read-expression-map t
				 'read-expression-history))
	 ))
  (let ((v (eval expr)))
    (if (not v)
	(message "Expression %s is nil." expr)
      (data-debug-show-stuff v "expression"))))

(defun data-debug-eval-expression (expr)
  "Evaluate EXPR and display the value.
If the result is something simple, show it in the echo area.
If the result is a list or vector, then use the data debugger to display it."
  (interactive
   (list (let ((minibuffer-completing-symbol t))
	   (read-from-minibuffer "Eval: "
				 nil read-expression-map t
				 'read-expression-history))
	 ))

  (if (null eval-expression-debug-on-error)
      (setq values (cons (eval expr) values))
    (let ((old-value (make-symbol "t")) new-value)
      ;; Bind debug-on-error to something unique so that we can
      ;; detect when evalled code changes it.
      (let ((debug-on-error old-value))
	(setq values (cons (eval expr) values))
	(setq new-value debug-on-error))
      ;; If evalled code has changed the value of debug-on-error,
      ;; propagate that change to the global binding.
      (unless (eq old-value new-value)
	(setq debug-on-error new-value))))

  (if (or (consp (car values)) (vectorp (car values)))
      (let ((v (car values)))
	(data-debug-show-stuff v "Expression"))
    ;; Old style
    (prog1
	(prin1 (car values) t)
      (let ((str (eval-expression-print-format (car values))))
	(if str (princ str t))))))

(provide 'data-debug)

(if (featurep 'eieio)
    (require 'eieio-datadebug))

;;; data-debug.el ends here
