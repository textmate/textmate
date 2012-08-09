;; erc-page.el - CTCP PAGE support for ERC

;; Copyright (C) 2002, 2004, 2006-2012 Free Software Foundation, Inc.

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

;; Requiring this file will make ERC react to CTCP PAGE messages
;; received, and it will provide a new /PAGE command to send such
;; messages yourself.  To enable it, customize the variable
;; `erc-page-mode'.

;;; Code:

(require 'erc)

;;;###autoload (autoload 'erc-page-mode "erc-page")
(define-erc-module page ctcp-page
  "Process CTCP PAGE requests from IRC."
  nil nil)

(erc-define-catalog-entry 'english 'CTCP-PAGE "Page from %n (%u@%h): %m")

(defgroup erc-page nil
  "React to CTCP PAGE messages."
  :group 'erc)

(defcustom erc-page-function nil
  "A function to process a \"page\" request.
If nil, this prints the page message in the minibuffer and calls
`beep'.  If non-nil, it must be a function that takes two arguments:
SENDER and MSG, both strings.

Example for your ~/.emacs file:

\(setq erc-page-function
      (lambda (sender msg)
	(play-sound-file \"/home/alex/elisp/erc/sounds/ni.wav\")
	(message \"IRC Page from %s: %s\" sender msg)))"
  :group 'erc-page
  :type '(choice (const nil)
		 (function)))

(defcustom erc-ctcp-query-PAGE-hook '(erc-ctcp-query-PAGE)
  "List of functions to be called when a CTCP PAGE is received.
This is called from `erc-process-ctcp-query'.  The functions are called
with six arguments: PROC NICK LOGIN HOST TO MSG.  Note that you can
also set `erc-page-function' to a function, which only gets two arguments,
SENDER and MSG, so that might be easier to use."
  :group 'erc-page
  :type '(repeat function))

(defun erc-ctcp-query-PAGE (proc nick login host to msg)
  "Deal with an CTCP PAGE query, if `erc-page-mode' is non-nil.
This will call `erc-page-function', if defined, or it will just print
a message and `beep'.  In addition to that, the page message is also
inserted into the server buffer."
  (when (and erc-page-mode
	     (string-match "PAGE\\(\\s-+.*\\)?$" msg))
    (let* ((m (match-string 1 msg))
	   (page-msg (if m (erc-controls-interpret (substring m 1))
		       "[no message]"))
	   text)
      (if m (setq m (substring m 1)))
      (setq text (erc-format-message 'CTCP-PAGE
				     ?n nick ?u login
				     ?h host ?m page-msg))
      (if erc-page-function
	  (funcall erc-page-function nick page-msg)
	;; if no function is defined
	(message "%s" text)
	(beep))
      ;; insert text into buffer
      (erc-display-message
       nil 'notice nil text)))
  nil)

(defun erc-cmd-PAGE (line &optional force)
  "Send a CTCP page to the user given as the first word in LINE.
The rest of LINE is the message to send.  Note that you will only
receive pages if `erc-page-mode' is on."
  (when (string-match "^\\s-*\\(\\S-+\\) ?\\(.*\\)" line)
    (let ((nick (match-string 1 line))
	  (msg (match-string 2 line)))
      (erc-cmd-CTCP nick "PAGE" msg))))

(put 'erc-cmd-PAGE 'do-not-parse-args t)

(provide 'erc-page)

;;; erc-page.el ends here
;;
;; Local Variables:
;; indent-tabs-mode: t
;; tab-width: 8
;; End:

