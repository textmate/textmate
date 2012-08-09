;;; goto-addr.el --- click to browse URL or to send to e-mail address

;; Copyright (C) 1995, 2000-2012 Free Software Foundation, Inc.

;; Author: Eric Ding <ericding@alum.mit.edu>
;; Maintainer: FSF
;; Created: 15 Aug 1995
;; Keywords: mh-e, www, mouse, mail

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

;; This package allows you to click or hit a key sequence while on a
;; URL or e-mail address, and either load the URL into a browser of
;; your choice using the browse-url package, or if it's an e-mail
;; address, to send an e-mail to that address.  By default, we bind to
;; the [mouse-2] and the [C-c return] key sequences.

;; INSTALLATION
;;
;; To use goto-address in a particular mode (for example, while
;; reading mail in mh-e), add something like this in your .emacs file:
;;
;; (add-hook 'mh-show-mode-hook 'goto-address)
;;
;; The mouse click method is bound to [mouse-2] on highlighted URLs or
;; e-mail addresses only; it functions normally everywhere else.  To bind
;; another mouse click to the function, add the following to your .emacs
;; (for example):
;;
;; (setq goto-address-highlight-keymap
;;   (let ((m (make-sparse-keymap)))
;;     (define-key m [S-mouse-2] 'goto-address-at-point)
;;     m))
;;

;; Known bugs/features:
;; * goto-address-mail-regexp only catches foo@bar.org style addressing,
;;   not stuff like X.400 addresses, etc.
;; * regexp also catches Message-Id line, since it is in the format of
;;   an Internet e-mail address (like Compuserve addresses)
;; * If the buffer is fontified after goto-address-fontify is run
;;   (say, using font-lock-fontify-buffer), then font-lock faces will
;;   override goto-address faces.

;;; Code:

(require 'thingatpt)
(autoload 'browse-url-url-at-point "browse-url")

;; XEmacs needs the following definitions.
(unless (fboundp 'overlays-in)
  (require 'overlay))
(unless (fboundp 'line-beginning-position)
  (defalias 'line-beginning-position 'point-at-bol))
(unless (fboundp 'line-end-position)
  (defalias 'line-end-position 'point-at-eol))
(unless (fboundp 'match-string-no-properties)
  (defalias 'match-string-no-properties 'match-string))

(defgroup goto-address nil
  "Click to browse URL or to send to e-mail address."
  :group 'mouse
  :group 'comm)


;; I don't expect users to want fontify'ing without highlighting.
(defcustom goto-address-fontify-p t
  "*Non-nil means URLs and e-mail addresses in buffer are fontified.
But only if `goto-address-highlight-p' is also non-nil."
  :type 'boolean
  :group 'goto-address)

(defcustom goto-address-highlight-p t
  "*Non-nil means URLs and e-mail addresses in buffer are highlighted."
  :type 'boolean
  :group 'goto-address)

(defcustom goto-address-fontify-maximum-size 30000
  "*Maximum size of file in which to fontify and/or highlight URLs.
A value of t means there is no limit--fontify regardless of the size."
  :type '(choice (integer :tag "Maximum size") (const :tag "No limit" t))
  :group 'goto-address)

(defvar goto-address-mail-regexp
  ;; Actually pretty much any char could appear in the username part.  -stef
  "[-a-zA-Z0-9=._+]+@\\([-a-zA-z0-9_]+\\.\\)+[a-zA-Z0-9]+"
  "A regular expression probably matching an e-mail address.")

(defvar goto-address-url-regexp
  (concat
   "\\<\\("
   (mapconcat 'identity
              (delete "mailto:"
		      ;; Remove `data:', as it's not terribly useful to follow
		      ;; those.  Leaving them causes `use Data::Dumper;' to be
		      ;; fontified oddly in Perl files.
                      (delete "data:"
                              (copy-sequence thing-at-point-uri-schemes)))
              "\\|")
   "\\)"
   thing-at-point-url-path-regexp)
  ;; (concat "\\b\\(s?https?\\|ftp\\|file\\|gopher\\|news\\|"
  ;; 	  "telnet\\|wais\\):\\(//[-a-zA-Z0-9_.]+:"
  ;; 	  "[0-9]*\\)?[-a-zA-Z0-9_=?#$@~`%&*+|\\/.,]*"
  ;; 	  "[-a-zA-Z0-9_=#$@~`%&*+|\\/]")
  "A regular expression probably matching a URL.")

(defvar goto-address-highlight-keymap
  (let ((m (make-sparse-keymap)))
    (define-key m (if (featurep 'xemacs) (kbd "<button2>") (kbd "<mouse-2>"))
      'goto-address-at-point)
    (define-key m (kbd "C-c RET") 'goto-address-at-point)
    m)
  "Keymap to hold goto-addr's mouse key defs under highlighted URLs.")

(defcustom goto-address-url-face 'link
  "Face to use for URLs."
  :type 'face
  :group 'goto-address)

(defcustom goto-address-url-mouse-face 'highlight
  "Face to use for URLs when the mouse is on them."
  :type 'face
  :group 'goto-address)

(defcustom goto-address-mail-face 'italic
  "Face to use for e-mail addresses."
  :type 'face
  :group 'goto-address)

(defcustom goto-address-mail-mouse-face 'secondary-selection
  "Face to use for e-mail addresses when the mouse is on them."
  :type 'face
  :group 'goto-address)

(defun goto-address-unfontify (start end)
  "Remove `goto-address' fontification from the given region."
  (dolist (overlay (overlays-in start end))
    (if (overlay-get overlay 'goto-address)
	(delete-overlay overlay))))

(defvar goto-address-prog-mode)

(defun goto-address-fontify ()
  "Fontify the URLs and e-mail addresses in the current buffer.
This function implements `goto-address-highlight-p'
and `goto-address-fontify-p'."
  ;; Clean up from any previous go.
  (goto-address-unfontify (point-min) (point-max))
  (save-excursion
    (let ((inhibit-point-motion-hooks t))
      (goto-char (point-min))
      (when (or (eq t goto-address-fontify-maximum-size)
		(< (- (point-max) (point)) goto-address-fontify-maximum-size))
	(while (re-search-forward goto-address-url-regexp nil t)
	  (let* ((s (match-beginning 0))
		 (e (match-end 0))
		 this-overlay)
	    (when (or (not goto-address-prog-mode)
		      ;; This tests for both comment and string
		      ;; syntax.
		      (nth 8 (syntax-ppss)))
	      (setq this-overlay (make-overlay s e))
	      (and goto-address-fontify-p
		   (overlay-put this-overlay 'face goto-address-url-face))
	      (overlay-put this-overlay 'evaporate t)
	      (overlay-put this-overlay
			   'mouse-face goto-address-url-mouse-face)
	      (overlay-put this-overlay 'follow-link t)
	      (overlay-put this-overlay
			   'help-echo "mouse-2, C-c RET: follow URL")
	      (overlay-put this-overlay
			   'keymap goto-address-highlight-keymap)
	      (overlay-put this-overlay 'goto-address t))))
	(goto-char (point-min))
	(while (re-search-forward goto-address-mail-regexp nil t)
	  (let* ((s (match-beginning 0))
		 (e (match-end 0))
		 this-overlay)
	    (when (or (not goto-address-prog-mode)
		      ;; This tests for both comment and string
		      ;; syntax.
		      (nth 8 (syntax-ppss)))
	      (setq this-overlay (make-overlay s e))
	      (and goto-address-fontify-p
		   (overlay-put this-overlay 'face goto-address-mail-face))
	      (overlay-put this-overlay 'evaporate t)
	      (overlay-put this-overlay 'mouse-face
			   goto-address-mail-mouse-face)
	      (overlay-put this-overlay 'follow-link t)
	      (overlay-put this-overlay
			   'help-echo "mouse-2, C-c RET: mail this address")
	      (overlay-put this-overlay
			   'keymap goto-address-highlight-keymap)
	      (overlay-put this-overlay 'goto-address t))))))))

(defun goto-address-fontify-region (start end)
  "Fontify URLs and e-mail addresses in the given region."
  (save-excursion
    (save-restriction
      (let ((beg-line (progn (goto-char start) (line-beginning-position)))
	    (end-line (progn (goto-char end) (line-end-position))))
	(narrow-to-region beg-line end-line)
	(goto-address-fontify)))))

;; code to find and goto addresses; much of this has been blatantly
;; snarfed from browse-url.el

;;;###autoload
(define-obsolete-function-alias
  'goto-address-at-mouse 'goto-address-at-point "22.1")

;;;###autoload
(defun goto-address-at-point (&optional event)
  "Send to the e-mail address or load the URL at point.
Send mail to address at point.  See documentation for
`goto-address-find-address-at-point'.  If no address is found
there, then load the URL at or before point."
  (interactive (list last-input-event))
  (save-excursion
    (if event (posn-set-point (event-end event)))
    (let ((address (save-excursion (goto-address-find-address-at-point))))
      (if (and address
	       (save-excursion
		 (goto-char (previous-single-char-property-change
			     (point) 'goto-address nil
			     (line-beginning-position)))
		 (not (looking-at goto-address-url-regexp))))
	  (compose-mail address)
	(let ((url (browse-url-url-at-point)))
	  (if url
	      (browse-url url)
	    (error "No e-mail address or URL found")))))))

(defun goto-address-find-address-at-point ()
  "Find e-mail address around or before point.
Then search backwards to beginning of line for the start of an e-mail
address.  If no e-mail address found, return nil."
  (re-search-backward "[^-_A-z0-9.@]" (line-beginning-position) 'lim)
  (if (or (looking-at goto-address-mail-regexp)	; already at start
	  (and (re-search-forward goto-address-mail-regexp
				  (line-end-position) 'lim)
	       (goto-char (match-beginning 0))))
      (match-string-no-properties 0)))

;;;###autoload
(defun goto-address ()
  "Sets up goto-address functionality in the current buffer.
Allows user to use mouse/keyboard command to click to go to a URL
or to send e-mail.
By default, goto-address binds `goto-address-at-point' to mouse-2 and C-c RET
only on URLs and e-mail addresses.

Also fontifies the buffer appropriately (see `goto-address-fontify-p' and
`goto-address-highlight-p' for more information)."
  (interactive)
  (if goto-address-highlight-p
      (goto-address-fontify)))
;;;###autoload(put 'goto-address 'safe-local-eval-function t)

;;;###autoload
(define-minor-mode goto-address-mode
  "Minor mode to buttonize URLs and e-mail addresses in the current buffer.
With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil."
  nil
  ""
  nil
  (if goto-address-mode
      (jit-lock-register #'goto-address-fontify-region)
    (jit-lock-unregister #'goto-address-fontify-region)
    (save-restriction
      (widen)
      (goto-address-unfontify (point-min) (point-max)))))

;;;###autoload
(define-minor-mode goto-address-prog-mode
  "Like `goto-address-mode', but only for comments and strings."
  nil
  ""
  nil
  (if goto-address-prog-mode
      (jit-lock-register #'goto-address-fontify-region)
    (jit-lock-unregister #'goto-address-fontify-region)
    (save-restriction
      (widen)
      (goto-address-unfontify (point-min) (point-max)))))

(provide 'goto-addr)

;;; goto-addr.el ends here
