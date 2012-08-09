;;; mh-gnus.el --- make MH-E compatible with various versions of Gnus

;; Copyright (C) 2003-2004, 2006-2012  Free Software Foundation, Inc.

;; Author: Satyaki Das <satyaki@theforce.stanford.edu>
;; Maintainer: Bill Wohler <wohler@newt.com>
;; Keywords: mail
;; See: mh-e.el

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

;;; Change Log:

;;; Code:

(require 'mh-e)

(mh-require 'gnus-util nil t)
(mh-require 'mm-bodies nil t)
(mh-require 'mm-decode nil t)
(mh-require 'mm-view nil t)
(mh-require 'mml nil t)

;; Copy of function from gnus-util.el.
;; TODO This is not in Gnus 5.11.
(defun-mh mh-gnus-local-map-property gnus-local-map-property (map)
  "Return a list suitable for a text property list specifying keymap MAP."
  (cond ((featurep 'xemacs) (list 'keymap map))
        ((>= emacs-major-version 21) (list 'keymap map))
        (t (list 'local-map map))))

;; Copy of function from mm-decode.el.
(defun-mh mh-mm-merge-handles mm-merge-handles (handles1 handles2)
  (append
   (if (listp (car handles1))
       handles1
     (list handles1))
   (if (listp (car handles2))
       handles2
     (list handles2))))

;; Copy of function from mm-decode.el.
(defun-mh mh-mm-set-handle-multipart-parameter
  mm-set-handle-multipart-parameter (handle parameter value)
  ;; HANDLE could be a CTL.
  (when handle
    (put-text-property 0 (length (car handle)) parameter value
		       (car handle))))

;; Copy of function from mm-view.el.
(defun-mh mh-mm-inline-text-vcard mm-inline-text-vcard (handle)
  (let ((inhibit-read-only t))
    (mm-insert-inline
     handle
     (concat "\n-- \n"
	     (ignore-errors
	       (if (fboundp 'vcard-pretty-print)
		   (vcard-pretty-print (mm-get-part handle))
		 (vcard-format-string
		  (vcard-parse-string (mm-get-part handle)
				      'vcard-standard-filter))))))))

;; Function from mm-decode.el used in PGP messages. Just define it with older
;; Gnus to avoid compiler warning.
(defun-mh mh-mm-possibly-verify-or-decrypt
  mm-possibly-verify-or-decrypt (parts ctl)
  nil)

;; Copy of macro in mm-decode.el.
(defmacro-mh mh-mm-handle-multipart-ctl-parameter
  mm-handle-multipart-ctl-parameter (handle parameter)
  `(get-text-property 0 ,parameter (car ,handle)))

;; Copy of function in mm-decode.el.
(defun-mh mh-mm-readable-p mm-readable-p (handle)
  "Say whether the content of HANDLE is readable."
  (and (< (with-current-buffer (mm-handle-buffer handle)
            (buffer-size)) 10000)
       (mm-with-unibyte-buffer
         (mm-insert-part handle)
         (and (eq (mm-body-7-or-8) '7bit)
              (not (mh-mm-long-lines-p 76))))))

;; Copy of function in mm-bodies.el.
(defun-mh mh-mm-long-lines-p mm-long-lines-p (length)
  "Say whether any of the lines in the buffer is longer than LENGTH."
  (save-excursion
    (goto-char (point-min))
    (end-of-line)
    (while (and (not (eobp))
                (not (> (current-column) length)))
      (forward-line 1)
      (end-of-line))
    (and (> (current-column) length)
         (current-column))))

(defun-mh mh-mm-keep-viewer-alive-p mm-keep-viewer-alive-p (handle)
  ;; Released Gnus doesn't keep handles associated with externally displayed
  ;; MIME parts. So this will always return nil.
  nil)

(defun-mh mh-mm-destroy-parts mm-destroy-parts (list)
  "Older versions of Emacs don't have this function."
  nil)

(defun-mh mh-mm-uu-dissect-text-parts mm-uu-dissect-text-parts (handles)
  "Emacs 21 and XEmacs don't have this function."
  nil)

;; Copy of function in mml.el.
(defun-mh mh-mml-minibuffer-read-disposition
  mml-minibuffer-read-disposition (type &optional default filename)
  (unless default
    (setq default (mml-content-disposition type filename)))
  (let ((disposition (completing-read
		      (format "Disposition (default %s): " default)
		      '(("attachment") ("inline") (""))
		      nil t nil nil default)))
    (if (not (equal disposition ""))
	disposition
      default)))

;; This is mm-save-part from Gnus 5.11 since that function in Emacs
;; 21.2 is buggy (the args to read-file-name are incorrect) and the
;; version in Emacs 22 is not consistent with C-x C-w in that you
;; can't just specify a directory and have the right thing happen.
(defun mh-mm-save-part (handle &optional prompt)
  "Write HANDLE to a file.
PROMPT overrides the default one used to ask user for a file name."
  (let ((filename (or (mail-content-type-get
		       (mm-handle-disposition handle) 'filename)
		      (mail-content-type-get
		       (mm-handle-type handle) 'name)))
	file)
    (when filename
      (setq filename (gnus-map-function mm-file-name-rewrite-functions
					(file-name-nondirectory filename))))
    (setq file
          (read-file-name (or prompt "Save MIME part to: ")
                          (or mm-default-directory default-directory)
                          nil nil (or filename "")))
    (setq mm-default-directory (file-name-directory file))
    (and (or (not (file-exists-p file))
	     (yes-or-no-p (format "File %s already exists; overwrite? "
				  file)))
	 (progn
	   (mm-save-part-to-file handle file)
	   file))))

(defun mh-mm-text-html-renderer ()
  "Find the renderer Gnus is using to display text/html MIME parts."
  (or (and (boundp 'mm-inline-text-html-renderer) mm-inline-text-html-renderer)
      (and (boundp 'mm-text-html-renderer) mm-text-html-renderer)))

(provide 'mh-gnus)

;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; indent-tabs-mode: nil
;; sentence-end-double-space: nil
;; End:

;;; mh-gnus.el ends here
