;;; kermit.el --- additions to shell mode for use with kermit

;; Copyright (C) 1988, 2001-2012 Free Software Foundation, Inc.

;; Author: Jeff Norden <jeff@colgate.csnet>
;; Maintainer: FSF
;; Created: 15 Feb 1988
;; Keywords: comm

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

;; I'm not sure, but I think somebody asked about running kermit under shell
;; mode a while ago.  Anyway, here is some code that I find useful.  The result
;; is that I can log onto machines with primitive operating systems (VMS and
;; ATT system V :-), and still have the features of shell-mode available for
;; command history, etc.  It's also handy to be able to run a file transfer in
;; an emacs window.  The transfer is in the "background", but you can also
;; monitor or stop it easily.

;; The ^\ key is bound to a function for sending escape sequences to kermit,
;; and ^C^Q can be used to send any control characters needed thru to the
;; system you connect to.  A more serious problem is that some brain-dead
;; systems will not recognize a ^J as an end-of-line character.  So LFD is
;; bound to a new function which acts just like CR usually does in shell-mode,
;; but a ^M is sent as an end-of-line.  Functions are also provided to swap the
;; bindings of CR and LFD.  I've also included a filter which will clean out
;; any ^M's or ^@'s that get typed at you, but I don't really recommend it.
;; There doesn't seem to be an acceptably fast way to do this via emacs-lisp.
;; Invoking kermit by the command " kermit | tr -d '\015' " seems to work
;; better (on my system anyway).

;; Here's how I've been using this setup.  We have several machines connected
;; thru a fairly stupid terminal switch.  If I want to connect to unix system,
;; then I use the LFD key to talk to the switch, and ignore any ^M's in the
;; buffer, and do a " stty -echo nl " after I log in.  Then the only real
;; difference from being in local shell-mode is that you need to type
;; ^C^Q^C to send an interrupt, and ^C^Q^Z for a stop signal, etc.  (since ^C^C
;; just generates a local stop signal, which kermit ignores).
;; To connect to a VMS system, I use a shell script to invoke kermit thru the
;; tr filter, do "M-X kermit-send-cr", and then tell VMS that I'm on a
;; half-duplex terminal.

;; Some caveats:
;; 1) Kermit under shell mode is a real pain if you don't have pty's.  I
;; recently discovered this on our 3b2/400.  When kermit can't find a tty, it
;; assumes it is supposed to be in remote mode.  So the simple command "kermit"
;; won't work in shell mode on such a system.  You can get around this by using
;; the -c (connect) command line option, which means you also have to specify a
;; line and baud on the command line, as in "kermit -l /dev/tty53 -b 9600 -c".
;; However, this will cause kermit to exit when the connection is closed.  So
;; in order to do a file transfer, you have to think ahead and add -r
;; (receive) to the command line.  This means that you can't use the server
;; feature.  The only fix I can see is to muck around with the source code for
;; kermit, although this probably wouldn't be too hard.  What is needed is an
;; option to force kermit to be local, to use stdin and stdout for interactive
;; speech, and to forget about cbreak mode.

;; Please let me know if any bugs turn up.
;; Feb 1988, Jeff Norden - jeff@colgate.csnet

;;; Code:

(require 'shell)

(defvar kermit-esc-char "\C-\\" "*Kermit's escape char.")

(defun kermit-esc ()
  "For sending escape sequences to a kermit running in shell mode."
  (interactive)
  (process-send-string
   (get-buffer-process (current-buffer))
   (concat kermit-esc-char (char-to-string (read-char)))))

(defun kermit-send-char ()
  "Send an arbitrary character to a program in shell mode."
  (interactive)
  (process-send-string
   (get-buffer-process (current-buffer))
   (char-to-string (read-char))))

(define-key shell-mode-map "\C-\\" 'kermit-esc)
(define-key shell-mode-map "\C-c\C-q" 'kermit-send-char)
;; extra bindings for folks suffering form ^S/^Q braindamage:
(define-key shell-mode-map "\C-c\\" 'kermit-esc)

(defun kermit-send-input-cr ()
  "Like \\[comint-send-input] but end the line with carriage-return."
  (interactive)
  (comint-send-input)
  (comint-send-string (get-buffer-process (current-buffer)) "\r"))

;; This is backwards of what makes sense, but ...
(define-key shell-mode-map "\n" 'kermit-send-input-cr)

(defun kermit-default-cr ()
  "Make RETURN end the line with carriage-return and LFD end it with a newline.
This is useful for talking to other systems on which carriage-return
is the normal way to end a line."
  (interactive)
  (define-key shell-mode-map "\r" 'kermit-send-input-cr)
  (define-key shell-mode-map "\n" 'comint-send-input))

(defun kermit-default-nl ()
  "Make RETURN end the line with a newline char.  This is the default state.
In this state, use LFD to send a line and end it with a carriage-return."
  (interactive)
  (define-key shell-mode-map "\n" 'kermit-send-input-cr)
  (define-key shell-mode-map "\r" 'comint-send-input))

(defun kermit-clean-filter (proc str)
  "Strip ^M and ^@ characters from process output."
  (save-excursion
    (let ((beg (process-mark proc)))
      (set-buffer (process-buffer proc))
      (goto-char beg)
      (insert-before-markers str)
      (while (re-search-backward "[\r\C-a]+" beg t)
	(replace-match "")))))

(defun kermit-clean-on ()
  "Delete all null characters and ^M's from the kermit output.
Note that another (perhaps better) way to do this is to use the
command `kermit | tr -d '\\015''."
  (interactive)
  (set-process-filter (get-buffer-process (current-buffer))
		      'kermit-clean-filter))

(defun kermit-clean-off ()
  "Cancel a previous `kermit-clean-on' command."
  (interactive)
  (set-process-filter (get-buffer-process (current-buffer)) nil))

(provide 'kermit)

;;; kermit.el ends here
