;;; dnd.el --- drag and drop support.  -*- coding: utf-8 -*-

;; Copyright (C) 2005-2012  Free Software Foundation, Inc.

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

;; This file provides the generic handling of the drop part only.
;; Different DND backends (X11, W32, etc.) that handle the platform
;; specific DND parts call the functions here to do final delivery of
;; a drop.

;;; Code:

;;; Customizable variables


;;;###autoload
(defcustom dnd-protocol-alist
  `((,(purecopy "^file:///")  . dnd-open-local-file)	; XDND format.
    (,(purecopy "^file://")   . dnd-open-file)		; URL with host
    (,(purecopy "^file:")     . dnd-open-local-file)	; Old KDE, Motif, Sun
    (,(purecopy "^\\(https?\\|ftp\\|file\\|nfs\\)://") . dnd-open-file)
   )

  "The functions to call for different protocols when a drop is made.
This variable is used by `dnd-handle-one-url' and `dnd-handle-file-name'.
The list contains of (REGEXP . FUNCTION) pairs.
The functions shall take two arguments, URL, which is the URL dropped and
ACTION which is the action to be performed for the drop (move, copy, link,
private or ask).
If no match is found here, and the value of `browse-url-browser-function'
is a pair of (REGEXP . FUNCTION), those regexps are tried for a match.
If no match is found, the URL is inserted as text by calling `dnd-insert-text'.
The function shall return the action done (move, copy, link or private)
if some action was made, or nil if the URL is ignored."
  :version "22.1"
  :type '(repeat (cons (regexp) (function)))
  :group 'dnd)


(defcustom dnd-open-remote-file-function
  (if (eq system-type 'windows-nt)
      'dnd-open-local-file
    'dnd-open-remote-url)
  "The function to call when opening a file on a remote machine.
The function will be called with two arguments; URI and ACTION. See
`dnd-open-file' for details.
If nil, then dragging remote files into Emacs will result in an error.
Predefined functions are `dnd-open-local-file' and `dnd-open-remote-url'.
`dnd-open-local-file' attempts to open a remote file using its UNC name and
is the  default on MS-Windows.  `dnd-open-remote-url' uses `url-handler-mode'
and is the default except for MS-Windows."
  :version "22.1"
  :type 'function
  :group 'dnd)


(defcustom dnd-open-file-other-window nil
  "If non-nil, always use find-file-other-window to open dropped files."
  :version "22.1"
  :type 'boolean
  :group 'dnd)


;; Functions

(defun dnd-handle-one-url (window action url)
  "Handle one dropped url by calling the appropriate handler.
The handler is first located by looking at `dnd-protocol-alist'.
If no match is found here, and the value of `browse-url-browser-function'
is a pair of (REGEXP . FUNCTION), those regexps are tried for a match.
If no match is found, just call `dnd-insert-text'.
WINDOW is where the drop happened, ACTION is the action for the drop,
URL is what has been dropped.
Returns ACTION."
  (require 'browse-url)
  (let (ret)
    (or
     (catch 'done
       (dolist (bf dnd-protocol-alist)
	 (when (string-match (car bf) url)
	   (setq ret (funcall (cdr bf) url action))
	   (throw 'done t)))
       nil)
     (when (not (functionp browse-url-browser-function))
       (catch 'done
	 (dolist (bf browse-url-browser-function)
	   (when (string-match (car bf) url)
	     (setq ret 'private)
	     (funcall (cdr bf) url action)
	     (throw 'done t)))
	 nil))
     (progn
       (dnd-insert-text window action url)
       (setq ret 'private)))
    ret))


(defun dnd-get-local-file-uri (uri)
  "Return an uri converted to file:/// syntax if uri is a local file.
Return nil if URI is not a local file."

  ;; The hostname may be our hostname, in that case, convert to a local
  ;; file.  Otherwise return nil.  TODO:  How about an IP-address as hostname?
  (let ((hostname (when (string-match "^file://\\([^/]*\\)" uri)
		      (downcase (match-string 1 uri))))
	(system-name-no-dot
	 (downcase (if (string-match "^[^\\.]+" system-name)
		       (match-string 0 system-name)
		     system-name))))
    (when (and hostname
	     (or (string-equal "localhost" hostname)
		 (string-equal (downcase system-name) hostname)
		 (string-equal system-name-no-dot hostname)))
	(concat "file://" (substring uri (+ 7 (length hostname)))))))

(defsubst dnd-unescape-uri (uri)
  (replace-regexp-in-string
   "%[A-Fa-f0-9][A-Fa-f0-9]"
   (lambda (arg)
     (let ((str (make-string 1 0)))
       (aset str 0 (string-to-number (substring arg 1) 16))
       str))
   uri t t))

;; http://lists.gnu.org/archive/html/emacs-devel/2006-05/msg01060.html
(defun dnd-get-local-file-name (uri &optional must-exist)
  "Return file name converted from file:/// or file: syntax.
URI is the uri for the file.  If MUST-EXIST is given and non-nil,
only return non-nil if the file exists.
Return nil if URI is not a local file."
  (let ((f (cond ((string-match "^file:///" uri)	; XDND format.
		  (substring uri (1- (match-end 0))))
		 ((string-match "^file:" uri)		; Old KDE, Motif, Sun
		  (substring uri (match-end 0))))))
    (and f (setq f (decode-coding-string (dnd-unescape-uri f)
                                         (or file-name-coding-system
                                             default-file-name-coding-system))))
    (when (and f must-exist (not (file-readable-p f)))
      (setq f nil))
    f))

(defun dnd-open-local-file (uri _action)
  "Open a local file.
The file is opened in the current window, or a new window if
`dnd-open-file-other-window' is set.  URI is the url for the file,
and must have the format file:file-name or file:///file-name.
The last / in file:/// is part of the file name.  If the system
natively supports unc file names, then remote urls of the form
file://server-name/file-name will also be handled by this function.
An alternative for systems that do not support unc file names is
`dnd-open-remote-url'. ACTION is ignored."

  (let* ((f (dnd-get-local-file-name uri t)))
    (if (and f (file-readable-p f))
	(progn
	  (if dnd-open-file-other-window
	      (find-file-other-window f)
	    (find-file f))
	  'private)
      (error "Can not read %s" uri))))

(defun dnd-open-remote-url (uri _action)
  "Open a remote file with `find-file' and `url-handler-mode'.
Turns `url-handler-mode' on if not on before.  The file is opened in the
current window, or a new window if `dnd-open-file-other-window' is set.
URI is the url for the file.  ACTION is ignored."
  (progn
    (require 'url-handlers)
    (or url-handler-mode (url-handler-mode))
    (if dnd-open-file-other-window
	(find-file-other-window uri)
      (find-file uri))
    'private))


(defun dnd-open-file (uri action)
  "Open a local or remote file.
The file is opened in the current window, or a new window if
`dnd-open-file-other-window' is set.  URI is the url for the file,
and must have the format file://hostname/file-name.  ACTION is ignored.
The last / in file://hostname/ is part of the file name."

  ;; The hostname may be our hostname, in that case, convert to a local
  ;; file.  Otherwise return nil.
  (let ((local-file (dnd-get-local-file-uri uri)))
    (if local-file (dnd-open-local-file local-file action)
      (if dnd-open-remote-file-function
	  (funcall dnd-open-remote-file-function uri action)
	(error "Remote files not supported")))))


(defun dnd-insert-text (window action text)
  "Insert text at point or push to the kill ring if buffer is read only.
TEXT is the text as a string, WINDOW is the window where the drop happened."
  (if (or buffer-read-only
	  (not (windowp window)))
      (progn
	(kill-new text)
	(message "%s"
	 (substitute-command-keys
	  "The dropped text can be accessed with \\[yank]")))
    (insert text))
  action)


(provide 'dnd)

;;; dnd.el ends here
