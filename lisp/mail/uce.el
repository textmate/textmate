;;; uce.el --- facilitate reply to unsolicited commercial email

;; Copyright (C) 1996, 1998, 2000-2012  Free Software Foundation, Inc.

;; Author: stanislav shalunov <shalunov@mccme.ru>
;; Created: 10 Dec 1996
;; Keywords: mail, uce, unsolicited commercial email

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

;; The code in this file provides a semi-automatic means of replying
;; to unsolicited commercial email (UCE) you might get.  Currently, it
;; only works with Rmail and Gnus.  If you would like to make it work
;; with other mail readers, see the mail-client dependent section of
;; uce-reply-to-uce.  Please let me know about your changes so I can
;; incorporate them.  I'd appreciate it.

;; The command uce-reply-to-uce, if called when the current message
;; buffer is a UCE, will setup a reply *mail* buffer as follows.  It
;; scans the full headers of the message for: 1) the normal return
;; address of the sender (From, Reply-To lines), and puts these
;; addresses into the To: header, along with abuse@offenders.host; 2)
;; the mailhub that first saw this message, and adds the address of
;; its postmaster into the To: header; and 3), finally, it looks at
;; the Message-Id and adds the postmaster of that host to the list of
;; addresses.

;; Then, we add an "Errors-To: nobody@localhost" header, so that if
;; some of these addresses are not actually correct, we will never see
;; bounced mail.  Also, mail-self-blind and mail-archive-file-name
;; take no effect: the ideology is that we don't want to save junk or
;; replies to junk.

;; Then we insert a template into the buffer (a customizable message
;; that explains what has happened), customizable signature, and the
;; original message with full headers and envelope for postmasters.
;; Then the buffer is left for editing.

;; The reason that the function uce-reply-to-uce is mail-client
;; dependent is that we want the full headers of the original message,
;; nothing stripped.  If we use the normal means of inserting the
;; original message into the *mail* buffer, headers like Received:
;; (not really headers, but envelope lines) will be stripped, while
;; they bear valuable information for us and postmasters.  I do wish
;; that there would be some portable way to write this function, but I
;; am not aware of any.

;; Usage:

;; Place uce.el in your load-path (and optionally byte-compile it).
;; Add the following line to your ~/.emacs:
;; (autoload 'uce-reply-to-uce "uce" "Reply to UCEs" t nil)
;; If you want to use it with Gnus rather than Rmail:
;; (setq uce-mail-reader 'gnus)

;; Options:

;; uce-message-text is a template that will be inserted into buffer.
;; It has a reasonable default.  If you want to write some scarier
;; one, please do so and send it to me.  Please keep it polite.

;; uce-signature behaves just like mail-signature.  If nil, nothing is
;; inserted, if t, file ~/.signature is used, if a string, its
;; contents are inserted into buffer.

;; uce-uce-separator is a line that separates your message from the
;; UCE that you enclose.

;; uce-subject-line will be used as the subject of the outgoing message.


;;; Change Log:

;; Dec 10, 1996 -- posted draft version to gnu.sources.emacs

;; Dec 11, 1996 -- fixed some typos, and Francesco Potorti`
;; <F.Potorti@cnuce.cnr.it> pointed out that my use of defvar was
;; weird, suggested fix, and added let form.

;; Dec 17, 1996 -- made scanning for host names little bit more clever
;; (obviously bogus stuff like localhost is now ignored).

;; Nov 11, 1997 -- incorporated changes from Mikael Djurfeldt
;; <mdj@nada.kth.se> to make uce.el work with Gnus.  Changed the text
;; of message that is sent.

;; Dec 3, 1997 -- changes from Gareth Jones <gdj1@gdjones.demon.co.uk>
;; handling Received headers following some line like `From:'.

;; Aug 16, 2000 -- changes from Detlev Zundel
;; <detlev.zundel@stud.uni-karlsruhe.de> to make uce.el work with the
;; latest Gnus.  Lars told him it should work for all versions of Gnus
;; younger than three years.


;;; Code:

(defvar gnus-original-article-buffer)
(defvar mail-reply-buffer)

(require 'sendmail)
;; Those sections of code which are dependent upon
;; RMAIL are only evaluated if we have received a message with RMAIL...
;;(require 'rmail)

(defgroup uce nil
  "Facilitate reply to unsolicited commercial email."
  :prefix "uce-"
  :group 'mail)

(defcustom uce-mail-reader 'rmail
  "A symbol indicating which mail reader you are using.
Choose from: `gnus', `rmail'."
  :type '(choice (const gnus) (const rmail))
  :version "20.3"
  :group 'uce)

(defcustom uce-setup-hook nil
  "Hook to run after UCE rant message is composed.
This hook is run after `mail-setup-hook', which is run as well."
  :type 'hook
  :group 'uce)

(defcustom uce-message-text
  "Recently, I have received an Unsolicited Commercial E-mail from you.
I do not like UCE's and I would like to inform you that sending
unsolicited messages to someone while he or she may have to pay for
reading your message may be illegal.  Anyway, it is highly annoying
and not welcome by anyone.  It is rude, after all.

If you think that this is a good way to advertise your products or
services you are mistaken.  Spamming will only make people hate you, not
buy from you.

If you have any list of people you send unsolicited commercial emails to,
REMOVE me from such list immediately.  I suggest that you make this list
just empty.

	----------------------------------------------------

If you are not an administrator of any site and still have received
this message then your email address is being abused by some spammer.
They fake your address in From: or Reply-To: header.  In this case,
you might want to show this message to your system administrator, and
ask him/her to investigate this matter.

Note to the postmaster(s): I append the text of UCE in question to
this message; I would like to hear from you about action(s) taken.
This message has been sent to postmasters at the host that is
mentioned as original sender's host (I do realize that it may be
faked, but I think that if your domain name is being abused this way
you might want to learn about it, and take actions) and to the
postmaster whose host was used as mail relay for this message.  If
message was sent not by your user, could you please compare time when
this message was sent (use time in Received: field of the envelope
rather than Date: field) with your sendmail logs and see what host was
using your sendmail at this moment of time.

Thank you."

  "This is the text that `uce-reply-to-uce' command will put in reply buffer.
Some of spamming programs in use will be set up to read all incoming
to spam address email, and will remove people who put the word `remove'
on beginning of some line from the spamming list.  So, when you set it
up, it might be a good idea to actually use this feature.

Value nil means insert no text by default, lets you type it in."
  :type '(choice (const nil) string)
  :group 'uce)

(defcustom uce-uce-separator
  "----- original unsolicited commercial email follows -----"
  "Line that will begin quoting of the UCE.
Value nil means use no separator."
  :type '(choice (const nil) string)
  :group 'uce)

(defcustom uce-signature mail-signature
"Text to put as your signature after the note to UCE sender.
Value nil means none, t means insert `~/.signature' file (if it happens
to exist), if this variable is a string this string will be inserted
as your signature."
  :type '(choice (const nil) (const t) string)
  :group 'uce)

(defcustom uce-default-headers
  "Errors-To: nobody@localhost\nPrecedence: bulk\n"
  "Additional headers to use when responding to a UCE with \\[uce-reply-to-uce].
These are mostly meant for headers that prevent delivery errors reporting."
  :type '(choice (const nil) string)
  :group 'uce)

(defcustom uce-subject-line
  "Spam alert: unsolicited commercial e-mail"
  "Subject of the message that will be sent in response to a UCE."
  :type 'string
  :group 'uce)

;; End of user options.


(defvar rmail-buffer)
(declare-function rmail-msg-is-pruned "rmail" ())
(declare-function mail-strip-quoted-names "mail-utils" (address))
(declare-function rmail-maybe-set-message-counters "rmail" ())
(declare-function rmail-toggle-header "rmail" (&optional arg))

;;;###autoload
(defun uce-reply-to-uce (&optional ignored)
  "Compose a reply to unsolicited commercial email (UCE).
Sets up a reply buffer addressed to: the sender, his postmaster,
his abuse@ address, and the postmaster of the mail relay used.
You might need to set `uce-mail-reader' before using this."
  (interactive)
  ;; Start of mail-client dependent section.
  (let ((message-buffer
	 (cond ((eq uce-mail-reader 'gnus) gnus-original-article-buffer)
	       ((eq uce-mail-reader 'rmail) (bound-and-true-p rmail-buffer))
	       (t (error
		   "Variable uce-mail-reader set to unrecognized value"))))
	pruned)
    (or (and message-buffer (get-buffer message-buffer))
	(error "No mail buffer, cannot find UCE"))
    (switch-to-buffer message-buffer)
    ;; We need the message with headers pruned.
    ;; Why?  All we do is get the from and reply-to headers.  ?
    (and (eq uce-mail-reader 'rmail)
	 (not (setq pruned (rmail-msg-is-pruned)))
	 (rmail-toggle-header 1))
    (let ((to (mail-strip-quoted-names (mail-fetch-field "from" t)))
	  (reply-to (mail-fetch-field "reply-to"))
	  temp)
      ;; Initial setting of the list of recipients of our message; that's
      ;; what they are pretending to be.
      (setq to (if to
		   (format "%s" (mail-strip-quoted-names to))
		 ""))
      (if reply-to
	  (setq to (format "%s, %s" to (mail-strip-quoted-names reply-to))))
      (let (first-at-sign end-of-hostname sender-host)
	(setq first-at-sign (string-match "@" to)
	      end-of-hostname (string-match "[ ,>]" to first-at-sign)
	      sender-host (substring to first-at-sign end-of-hostname))
	(if (string-match "\\." sender-host)
	    (setq to (format "%s, postmaster%s, abuse%s"
			     to sender-host sender-host))))
      (setq mail-send-actions nil)
      (setq mail-reply-buffer nil)
      (when (eq uce-mail-reader 'rmail)
	(rmail-toggle-header 0)
	(rmail-maybe-set-message-counters)) ; why?
      (copy-region-as-kill (point-min) (point-max))
      ;; Restore the initial header state we found.
      (and pruned (rmail-toggle-header 1))
      (switch-to-buffer "*mail*")
      (erase-buffer)
      (yank)
      (goto-char (point-min))
      ;; Delete any internal Rmail headers.
      (when (eq uce-mail-reader 'rmail)
	(search-forward "\n\n")
	(while (re-search-backward "^X-RMAIL" nil t)
	  (delete-region (point) (line-beginning-position 2)))
	(goto-char (point-min)))
      ;; Now find the mail hub that first accepted this message.
      ;; This should try to find the last Received: header.
      ;; Sometimes there may be other headers inbetween Received: headers.
      (cond ((eq uce-mail-reader 'gnus)
	     ;; Does Gnus always have Lines: in the end?
	     (re-search-forward "^Lines:")
	     (beginning-of-line))
	    ((eq uce-mail-reader 'rmail)
	     (search-forward "\n\n")))
      (re-search-backward "^Received:")
      ;; Is this always good?  It's the only thing I saw when I checked
      ;; a few messages.
      ;;(if (not (re-search-forward ": \\(from\\|by\\) " eol t))
      (unless (re-search-forward "\\(from\\|by\\) " (line-end-position) 'move)
	(if (looking-at "[ \t\n]+\\(from\\|by\\) ")
	    (goto-char (match-end 0))
	  (error "Failed to extract hub address")))
      (setq temp (point))
      (search-forward " ")
      (forward-char -1)
      ;; And add its postmaster to the list of addresses.
      (if (string-match "\\." (buffer-substring temp (point)))
	  (setq to (format "%s, postmaster@%s"
			   to (buffer-substring temp (point)))))
      ;; Also look at the message-id, it helps *very* often.
      (and (search-forward "\nMessage-Id: " nil t)
	   ;; Not all Message-Id:'s have an `@' sign.
	   (search-forward "@" (line-end-position) t)
	   (progn
	     (setq temp (point))
	     (search-forward ">")
	     (forward-char -1)
	     (if (string-match "\\." (buffer-substring temp (point)))
		 (setq to (format "%s, postmaster@%s"
				  to (buffer-substring temp (point)))))))
      (when (eq uce-mail-reader 'gnus)
	;; Does Gnus always have Lines: in the end?
	(re-search-forward "^Lines:")
	(beginning-of-line)
	(setq temp (point))
	(search-forward "\n\n" nil t)
	(forward-line -1)
	(delete-region temp (point)))
      ;; End of mail-client dependent section.
      (auto-save-mode auto-save-default)
      (mail-mode)
      (goto-char (point-min))
      (insert "To: ")
      (save-excursion
	(if to
	    (let ((fill-prefix "\t")
		  (address-start (point)))
	      (insert to "\n")
	      (fill-region-as-paragraph address-start (point)))
	  (newline))
	(insert "Subject: " uce-subject-line "\n")
	(if uce-default-headers
	    (insert uce-default-headers))
	(if mail-default-headers
	    (insert mail-default-headers))
	(if mail-default-reply-to
	    (insert "Reply-to: " mail-default-reply-to "\n"))
	(insert mail-header-separator "\n")
	;; Insert all our text.  Then go back to the place where we started.
	(if to (setq to (point)))
	;; Text of ranting.
	(if uce-message-text
	    (insert uce-message-text))
	;; Signature.
	(cond ((eq uce-signature t)
	       (if (file-exists-p "~/.signature")
		   (progn
		     (insert "\n\n-- \n")
		     (forward-char (cadr (insert-file-contents "~/.signature"))))))
	      (uce-signature
	       (insert "\n\n-- \n" uce-signature)))
	;; And text of the original message.
	(if uce-uce-separator
	    (insert "\n\n" uce-uce-separator "\n"))
	;; If message doesn't end with a newline, insert it.
	(goto-char (point-max))
	(or (bolp) (newline)))
      ;; And go back to the beginning of text.
      (if to (goto-char to))
      (or to (set-buffer-modified-p nil))
      ;; Run hooks before we leave buffer for editing.  Reasonable usage
      ;; might be to set up special key bindings, replace standard
      ;; functions in mail-mode, etc.
      (run-hooks 'mail-setup-hook 'uce-setup-hook))))

(defun uce-insert-ranting (&optional ignored)
  "Insert text of the usual reply to UCE into current buffer."
  (interactive "P")
  (insert uce-message-text))

(provide 'uce)

;;; uce.el ends here
