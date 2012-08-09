;;; newst-reader.el --- Generic RSS reader functions.

;; Copyright (C) 2003-2012  Free Software Foundation, Inc.

;; Author:      Ulf Jasper <ulf.jasper@web.de>
;; Filename:    newst-reader.el
;; URL:         http://www.nongnu.org/newsticker
;; Time-stamp:  "24. September 2011, 15:47:49 (ulf)"
;; Package:     newsticker

;; ======================================================================

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

;; ======================================================================
;;; Commentary:

;; See newsticker.el

;; ======================================================================
;;; Code:

(require 'newst-backend)

;; ======================================================================
;;; Customization
;; ======================================================================
(defun newsticker--set-customvar-formatting (symbol value)
  "Set newsticker-variable SYMBOL value to VALUE.
Calls all actions which are necessary in order to make the new
value effective."
  (if (or (not (boundp symbol))
          (equal (symbol-value symbol) value))
      (set symbol value)
    ;; something must have changed
    (set symbol value)
    (when (fboundp 'newsticker--forget-preformatted)
      (newsticker--forget-preformatted))))

;; ======================================================================
;; reader
(defgroup newsticker-reader nil
  "Settings for the feed reader."
  :group 'newsticker)

(defcustom newsticker-frontend
  'newsticker-treeview
  "Newsticker frontend for reading news.
This must be one of the functions `newsticker-plainview' or
`newsticker-treeview'."
  :type '(choice :tag "Frontend"
                 (const :tag "Single buffer (plainview)" newsticker-plainview)
                 (const :tag "Tree view (treeview)" newsticker-treeview))
  :group 'newsticker-reader)

;; image related things
(defcustom newsticker-enable-logo-manipulations
  t
  "If non-nil newsticker manipulates logo images.
This enables the following image properties: heuristic mask for all
logos, and laplace-conversion for images without new items."
  :type 'boolean
  :group 'newsticker-reader)

(defcustom newsticker-justification
  'left
  "How to fill item descriptions.
If non-nil newsticker calls `fill-region' to wrap long lines in
item descriptions.  However, if an item description contains HTML
text and `newsticker-html-renderer' is non-nil, filling is not
done."
  :type '(choice :tag "Justification"
                 (const :tag "No filling" nil)
                 (const :tag "Left"       left)
                 (const :tag "Right"      right)
                 (const :tag "Center"     center)
                 (const :tag "Full"       full))
  :set 'newsticker--set-customvar-formatting
  :group 'newsticker-reader)

(defcustom newsticker-use-full-width
  t
  "Decides whether to use the full window width when filling.
If non-nil newsticker sets `fill-column' so that the whole
window is used when filling.  See also `newsticker-justification'."
  :type 'boolean
  :set 'newsticker--set-customvar-formatting
  :group 'newsticker-reader)

(defcustom newsticker-html-renderer
  nil
  "Function for rendering HTML contents.
If non-nil, newsticker.el will call this function whenever it
finds HTML-like tags in item descriptions.  Possible functions
are `w3m-region', `w3-region', and `newsticker-htmlr-render'.
Newsticker automatically loads the respective package w3m, w3, or
htmlr if this option is set."
  :type '(choice :tag "Function"
                 (const :tag "None" nil)
                 (const :tag "w3" w3-region)
                 (const :tag "w3m" w3m-region)
                 (const :tag "htmlr" newsticker-htmlr-render))
  :set 'newsticker--set-customvar-formatting
  :group 'newsticker-reader)

(defcustom newsticker-date-format
  "(%A, %H:%M)"
  "Format for the date part in item and feed lines.
See `format-time-string' for a list of valid specifiers."
  :type 'string
  :set 'newsticker--set-customvar-formatting
  :group 'newsticker-reader)

(defgroup newsticker-faces nil
  "Settings for the faces of the feed reader."
  :group 'newsticker-reader)

(defface newsticker-feed-face
  '((((class color) (background dark))
     (:family "sans" :bold t :height 1.2 :foreground "white"))
    (((class color) (background light))
     (:family "sans" :bold t :height 1.2 :foreground "black")))
  "Face for news feeds."
  :group 'newsticker-faces)

(defface newsticker-extra-face
  '((((class color) (background dark))
     (:italic t :foreground "gray50" :height 0.8))
    (((class color) (background light))
     (:italic t :foreground "gray50" :height 0.8)))
  "Face for newsticker dates."
  :group 'newsticker-faces)

(defface newsticker-enclosure-face
  '((((class color) (background dark))
     (:bold t :background "orange"))
    (((class color) (background light))
     (:bold t :background "orange")))
  "Face for enclosed elements."
  :group 'newsticker-faces)

;; ======================================================================
;;; Utility functions
;; ======================================================================
(defun newsticker--insert-enclosure (item keymap)
  "Insert enclosure element of a news ITEM into the current buffer.
KEYMAP will be applied."
  (let ((enclosure (newsticker--enclosure item))
        (beg (point)))
    (when enclosure
      (let ((url (cdr (assoc 'url enclosure)))
            (length (string-to-number (or (cdr (assoc 'length enclosure))
                                          "-1")))
            (type (cdr (assoc 'type enclosure))))
        (cond ((> length 1048576)
               (insert (format "Enclosed file (%s, %1.2f MBytes)" type
                               (/ length 1048576))))
              ((> length 1024)
               (insert (format "Enclosed file (%s, %1.2f KBytes)" type
                               (/ length 1024))))
              ((> length 0)
               (insert (format "Enclosed file (%s, %1.2f Bytes)" type
                               length)))
              (t
               (insert (format "Enclosed file (%s, unknown size)" type))))
        (add-text-properties beg (point)
                             (list 'mouse-face 'highlight
                                   'nt-link url
                                   'help-echo (format
                                               "mouse-2: visit (%s)" url)
                                   'keymap keymap
                                   'nt-face 'enclosure
                                   'nt-type 'desc))
        (insert "\n")))))

(defun newsticker--print-extra-elements (item keymap)
  "Insert extra-elements of ITEM in a pretty form into the current buffer.
KEYMAP is applied."
  (let ((ignored-elements '(items link title description content
                                  content:encoded dc:subject
                                  dc:date entry item guid pubDate
                                  published updated
                                  enclosure))
        (left-column-width 1))
    (mapc (lambda (extra-element)
            (when (listp extra-element) ;; take care of broken xml
                                        ;; data, 2007-05-25
              (unless (memq (car extra-element) ignored-elements)
                (setq left-column-width (max left-column-width
                                             (length (symbol-name
                                                      (car extra-element))))))))
          (newsticker--extra item))
    (mapc (lambda (extra-element)
            (when (listp extra-element) ;; take care of broken xml
                                        ;; data, 2007-05-25
              (unless (memq (car extra-element) ignored-elements)
                (newsticker--do-print-extra-element extra-element
                                                    left-column-width
                                                    keymap))))
          (newsticker--extra item))))

(defun newsticker--do-print-extra-element (extra-element width keymap)
  "Actually print an EXTRA-ELEMENT using the given WIDTH.
KEYMAP is applied."
  (let ((name (symbol-name (car extra-element))))
    (insert (format "%s: " name))
    (insert (make-string (- width (length name)) ? )))
  (let (;;(attributes (cadr extra-element)) ;FIXME!!!!
        (contents (cddr extra-element)))
    (cond ((listp contents)
           (mapc (lambda (i)
                   (if (and (stringp i)
                            (string-match "^http://.*" i))
                       (let ((pos (point)))
                         (insert i " ") ; avoid self-reference from the
                                        ; nt-link thing
                         (add-text-properties
                          pos (point)
                          (list 'mouse-face 'highlight
                                'nt-link i
                                'help-echo
                                (format "mouse-2: visit (%s)" i)
                                'keymap keymap)))
                         (insert (format "%s" i))))
                 contents))
          (t
           (insert (format "%s" contents))))
    (insert "\n")))

(defun newsticker--image-read (feed-name-symbol disabled)
  "Read the cached image for FEED-NAME-SYMBOL from disk.
If DISABLED is non-nil the image will be converted to a disabled look
\(unless `newsticker-enable-logo-manipulations' is not t\).
Return the image."
  (let ((image-name (concat (newsticker--images-dir)
                            (symbol-name feed-name-symbol)))
        (img nil))
    (when (file-exists-p image-name)
      (condition-case error-data
          (setq img (create-image
                     image-name nil nil
                     :conversion (and newsticker-enable-logo-manipulations
                                      disabled
                                      'disabled)
                     :mask (and newsticker-enable-logo-manipulations
                                'heuristic)
                     :ascent 70))
        (error
         (message "Error: cannot create image for %s: %s"
                  feed-name-symbol error-data))))
    img))

;; the functions we need for retrieval and display
;;;###autoload
(defun newsticker-show-news ()
  "Start reading news.  You may want to bind this to a key."
  (interactive)
  (newsticker-start t) ;; will start only if not running
  ;; Load the html rendering packages
  (if newsticker-html-renderer
      (cond ((eq newsticker-html-renderer 'w3m-region)
             (require 'w3m))
            ((eq newsticker-html-renderer 'w3-region)
             (require 'w3-auto))
            ((eq newsticker-html-renderer 'newsticker-htmlr-render)
             (require 'htmlr))))
  (funcall newsticker-frontend))

;; ======================================================================
;;; Toolbar
;; ======================================================================

(defun newsticker-browse-url-item (feed item)
  "Convert FEED ITEM to html and call `browse-url' on result."
  (interactive)
  (let ((t-file (make-temp-file "newsticker")))
    (with-temp-file t-file
      (insert "<?xml version=\"1.0\" encoding=\"utf-8\"?>
               <!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
               \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
               <html xmlns=\"http://www.w3.org/1999/xhtml\">
               <body>")
      (insert "<h1>" feed ": " (newsticker--title item) "</h1>")
      (insert (format-time-string newsticker-date-format
                                  (newsticker--time item)))
      (insert "<br/>")
      (insert (or (newsticker--desc item) "[No Description]"))
      (when (newsticker--enclosure item)
        (insert "<br/><hr/><i>")
        (newsticker--insert-enclosure item nil)
        (insert "</i>"))
      (when (newsticker--extra item)
        (insert "<br/><hr/><tt>")
        (newsticker--print-extra-elements item nil)
        (insert "</tt>"))
      (insert "</body></html>"))
    (browse-url t-file)))

(provide 'newst-reader)

;;; newst-reader.el ends here
