;;; erc-hecomplete.el --- Provides Nick name completion for ERC

;; Copyright (C) 2001-2002, 2004, 2006-2012 Free Software Foundation, Inc.

;; Author: Alex Schroeder <alex@gnu.org>
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?ErcCompletion
;; Obsolete-since: 24.1

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

;; This file is considered obsolete.  It is recommended to use
;; completion from erc-pcomplete instead.

;; This file is based on hippie-expand, while the new file is based on
;; pcomplete.

;;; Code:

(require 'erc)
(require 'erc-match); for erc-pals
(require 'hippie-exp); for the hippie expand stuff

;;;###autoload (autoload 'erc-hecomplete-mode "erc-hecomplete" nil t)
(define-erc-module hecomplete nil
  "Complete nick at point."
  ((add-hook 'erc-complete-functions 'erc-hecomplete))
  ((remove-hook 'erc-complete-functions 'erc-hecomplete)))

(defun erc-hecomplete ()
  "Complete nick at point.
See `erc-try-complete-nick' for more technical info.
This function is obsolete, use `erc-pcomplete' instead."
  (interactive)
  (let ((hippie-expand-try-functions-list '(erc-try-complete-nick)))
    (hippie-expand nil)))

(defgroup erc-hecomplete nil
  "Nick completion.  It is recommended to use erc-pcomplete instead."
  :group 'erc)

(defcustom erc-nick-completion 'all
  "Determine how the list of nicks is determined during nick completion.
See `erc-complete-nick' for information on how to activate this.

pals:   Use `erc-pals'.
all:    All channel members.

You may also provide your own function that returns a list of completions.
One example is `erc-nick-completion-exclude-myself',
or you may use an arbitrary lisp expression."
  :type '(choice (const :tag "List of pals" pals)
		 (const :tag "All channel members" all)
		 (const :tag "All channel members except yourself"
			erc-nick-completion-exclude-myself)
		 (repeat :tag "List" (string :tag "Nick"))
		 function
		 sexp)
  :group 'erc-hecomplete)

(defcustom erc-nick-completion-ignore-case t
  "*Non-nil means don't consider case significant in nick completion.
Case will be automatically corrected when non-nil.
For instance if you type \"dely TAB\" the word completes and changes to
\"delYsid\"."
  :group 'erc-hecomplete
  :type 'boolean)

(defun erc-nick-completion-exclude-myself ()
  "Get a list of all the channel members except you.

This function returns a list of all the members in the channel, except
your own nick.  This way if you're named foo and someone is called foobar,
typing \"f o TAB\" will directly give you foobar.  Use this with
`erc-nick-completion'."
  (remove
   (erc-current-nick)
   (erc-get-channel-nickname-list)))

(defcustom erc-nick-completion-postfix ": "
  "*When `erc-complete' is used in the first word after the prompt,
add this string when a unique expansion was found."
  :group 'erc-hecomplete
  :type 'string)

(defun erc-command-list ()
  "Returns a list of strings of the defined user commands."
  (let ((case-fold-search nil))
    (mapcar (lambda (x)
	      (concat "/" (downcase (substring (symbol-name x) 8))))
	    (apropos-internal "erc-cmd-[A-Z]+"))))

(defun erc-try-complete-nick (old)
  "Complete nick at point.
This is a function to put on `hippie-expand-try-functions-list'.
Then use \\[hippie-expand] to expand nicks.
The type of completion depends on `erc-nick-completion'."
  (try-complete-erc-nick old (cond ((eq erc-nick-completion 'pals) erc-pals)
				   ((eq erc-nick-completion 'all)
				    (append
				     (erc-get-channel-nickname-list)
				     (erc-command-list)))
				   ((functionp erc-nick-completion)
				    (funcall erc-nick-completion))
				   (t erc-nick-completion))))

(defvar try-complete-erc-nick-window-configuration nil
  "The window configuration for `try-complete-erc-nick'.
When called the first time, a window config is stored here,
and when completion is done, the window config is restored
from here.  See `try-complete-erc-nick-restore' and
`try-complete-erc-nick'.")

(defun try-complete-erc-nick-restore ()
  "Restore window configuration."
  (if (not try-complete-erc-nick-window-configuration)
      (when (get-buffer "*Completions*")
	(delete-windows-on "*Completions*"))
    (set-window-configuration
     try-complete-erc-nick-window-configuration)
    (setq try-complete-erc-nick-window-configuration nil)))

(defun try-complete-erc-nick (old completions)
  "Try to complete current word depending on `erc-try-complete-nick'.
The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible completions of the same
string).  It returns t if a new completion is found, nil otherwise.  The
second argument COMPLETIONS is a list of completions to use.  Actually,
it is only used when OLD is nil.  It will be copied to `he-expand-list'
on the first call.  After that, it is no longer used.
Window configurations are stored in
`try-complete-erc-nick-window-configuration'."
  (let (expansion
	final
	(alist (if (consp (car completions))
		   completions
		 (mapcar (lambda (s)
			   (if (and (erc-complete-at-prompt)
				    (and (not (= (length s) 0))
					 (not (eq (elt s 0) ?/))))
			       (list (concat s erc-nick-completion-postfix))
			     (list (concat s " "))))
			 completions))) ; make alist if required
	(completion-ignore-case erc-nick-completion-ignore-case))
    (he-init-string (he-dabbrev-beg) (point))
    ;; If there is a string to complete, complete it using alist.
    ;; expansion is the possible expansion, or t.  If expansion is t
    ;; or if expansion is the "real" thing, we are finished (final is
    ;; t).  Take care -- expansion can also be nil!
    (unless (string= he-search-string "")
      (setq expansion (try-completion he-search-string alist)
	    final (or (eq t expansion)
		      (and expansion
			   (eq t (try-completion expansion alist))))))
    (cond ((not expansion)
	   ;; There is no expansion at all.
	   (try-complete-erc-nick-restore)
	   (he-reset-string)
	   nil)
	  ((eq t expansion)
	   ;; The user already has the correct expansion.
	   (try-complete-erc-nick-restore)
	   (he-reset-string)
	   t)
	  ((and old (string= expansion he-search-string))
	   ;; This is the second time around and nothing changed,
	   ;; ie. the user tried to expand something incomplete
	   ;; without making a choice -- hitting TAB twice, for
	   ;; example.
	   (try-complete-erc-nick-restore)
	   (he-reset-string)
	   nil)
	  (final
	   ;; The user has found the correct expansion.
	   (try-complete-erc-nick-restore)
	   (he-substitute-string expansion)
	   t)
	  (t
	   ;; We found something but we are not finished.  Show a
	   ;; completions buffer.  Substitute what we found and return
	   ;; t.
	   (setq try-complete-erc-nick-window-configuration
		 (current-window-configuration))
	   (with-output-to-temp-buffer "*Completions*"
	     (display-completion-list (all-completions he-search-string alist)))
	   (he-substitute-string expansion)
	   t))))

(defun erc-at-beginning-of-line-p (point &optional bol-func)
  (save-excursion
    (funcall (or bol-func
		 'erc-bol))
    (equal point (point))))

(defun erc-complete-at-prompt ()
  "Returns t if point is directly after `erc-prompt' when doing completion."
  (erc-at-beginning-of-line-p (he-dabbrev-beg)))

(provide 'erc-hecomplete)

;;; erc-hecomplete.el ends here
;;
;; Local Variables:
;; indent-tabs-mode: t
;; tab-width: 8
;; End:

