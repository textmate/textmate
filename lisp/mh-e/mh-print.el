;;; mh-print.el --- MH-E printing support

;; Copyright (C) 2003-2012  Free Software Foundation, Inc.

;; Author: Jeffrey C Honig <jch@honig.net>
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
(require 'mh-scan)

(require 'ps-print)

(defvar mh-ps-print-color-option ps-print-color-p
  "Specify how buffer's text color is printed.

Valid values are:

   nil         - Do not print colors.
   t           - Print colors.
   black-white - Print colors on black/white printer.
                 See also `ps-black-white-faces'.

Any other value is treated as t. This variable is initialized
from `ps-print-color-p'.")

(defvar mh-ps-print-func 'ps-spool-buffer-with-faces
  "Function to use to spool a buffer.

Sensible choices are the functions `ps-spool-buffer' and
`ps-spool-buffer-with-faces'.")

;;;###mh-autoload
(defun mh-ps-print-msg (range)
  "Print RANGE\\<mh-folder-mode-map>.

Check the documentation of `mh-interactive-range' to see how RANGE is
read in interactive use.

This command will print inline text attachments but will not decrypt
messages. However, when a message is displayed in an MH-Show buffer,
then that buffer is used verbatim for printing with the caveat that
only text attachments, if opened inline, are printed. Therefore,
encrypted messages can be printed by showing and decrypting them
first.

MH-E uses the \"ps-print\" package to do the printing, so you can
customize the printing further by going to the `ps-print'
customization group. This command does not use the options
`mh-lpr-command-format' or `mh-print-background-flag'. See also the
commands \\[mh-ps-print-toggle-color] and
\\[mh-ps-print-toggle-faces]."
  (interactive (list (mh-interactive-range "Print")))
  (mh-ps-print-range range nil))

(defun mh-ps-print-range (range file)
  "Print RANGE to FILE.

This is the function that actually does the work.
If FILE is nil, then the messages are spooled to the printer."
  (mh-iterate-on-range msg range
    (unwind-protect
        (mh-ps-spool-msg msg))
    (mh-notate msg mh-note-printed mh-cmd-note))
  (ps-despool file))

(defun mh-ps-spool-msg (msg)
  "Spool MSG."
  (let* ((folder mh-current-folder)
         (buffer (mh-in-show-buffer (mh-show-buffer)
                   (if (not (equal (mh-msg-filename msg folder)
                                   buffer-file-name))
                       (get-buffer-create mh-temp-buffer)))))
    (unwind-protect
        (save-excursion
          (if buffer
              (let ((mh-show-buffer buffer))
                (mh-display-msg msg folder)))
          (mh-ps-spool-buffer (if buffer buffer mh-show-buffer)))
      (if buffer
          (kill-buffer buffer)))))

(defun mh-ps-spool-buffer (buffer)
  "Spool BUFFER."
  (with-current-buffer buffer
    (let ((ps-print-color-p mh-ps-print-color-option)
          (ps-left-header
           (list
            (concat "(" (mh-get-header-field "Subject:") ")")
            (concat "(" (mh-get-header-field "From:") ")")))
          (ps-right-header
           (list
            "/pagenumberstring load"
            (concat "(" (mh-get-header-field "Date:") ")"))))
      (funcall mh-ps-print-func))))

;;;###mh-autoload
(defun mh-ps-print-msg-file (range file)
  "Print RANGE to FILE\\<mh-folder-mode-map>.

Check the documentation of `mh-interactive-range' to see how RANGE is
read in interactive use.

This command will print inline text attachments but will not decrypt
messages. However, when a message is displayed in an MH-Show buffer,
then that buffer is used verbatim for printing with the caveat that
only text attachments, if opened inline, are printed. Therefore,
encrypted messages can be printed by showing and decrypting them
first.

MH-E uses the \"ps-print\" package to do the printing, so you can
customize the printing further by going to the `ps-print'
customization group. This command does not use the options
`mh-lpr-command-format' or `mh-print-background-flag'. See also the
commands \\[mh-ps-print-toggle-color] and
\\[mh-ps-print-toggle-faces]."
  (interactive (list (mh-interactive-range "Print") (mh-ps-print-preprint 1)))
  (mh-ps-print-range range file))

(defun mh-ps-print-preprint (prefix-arg)
  "Provide a better default file name for `ps-print-preprint'.
Pass along the PREFIX-ARG to it."
  (let ((buffer-file-name (format "mh-%s" (substring (buffer-name) 1))))
    (ps-print-preprint prefix-arg)))

;;;###mh-autoload
(defun mh-ps-print-toggle-faces ()
 "Toggle whether printing is done with faces or not.

When faces are enabled, the printed message will look very
similar to the message in the MH-Show buffer."
 (interactive)
 (if (eq mh-ps-print-func 'ps-spool-buffer-with-faces)
     (progn
       (setq mh-ps-print-func 'ps-spool-buffer)
       (message "Printing without faces"))
   (setq mh-ps-print-func 'ps-spool-buffer-with-faces)
   (message "Printing with faces")))

;;;###mh-autoload
(defun mh-ps-print-toggle-color ()
  "Toggle whether color is used in printing messages.

Colors are emulated on black-and-white printers with shades of
gray. This might produce illegible output, even if your screen
colors only use shades of gray. If this is the case, try using
this command to toggle between color, no color, and a black and
white representation of the colors and see which works best. You
change this setting permanently by customizing the option
`ps-print-color-p'."
 (interactive)
 (if (eq mh-ps-print-color-option nil)
     (progn
       (setq mh-ps-print-color-option 'black-white)
       (message "Colors will be printed as black & white"))
   (if (eq mh-ps-print-color-option 'black-white)
       (progn
         (setq mh-ps-print-color-option t)
         (message "Colors will be printed"))
     (setq mh-ps-print-color-option nil)
     (message "Colors will not be printed"))))

;; Old non-PS based printing
;;;###mh-autoload
(defun mh-print-msg (range)
  "Print RANGE the old fashioned way\\<mh-folder-mode-map>.

The message is formatted with \"mhl\" (see option
`mh-mhl-format-file') and printed with the \"lpr\" command (see
option `mh-lpr-command-format').

Check the documentation of `mh-interactive-range' to see how
RANGE is read in interactive use.

Consider using \\[mh-ps-print-msg] instead."
  (interactive (list (mh-interactive-range "Print")))
  (message "Printing...")
  (let (msgs)
    ;; Gather message numbers and add them to "printed" sequence.
    (mh-iterate-on-range msg range
      (mh-add-msgs-to-seq msg 'printed t)
      (mh-notate nil mh-note-printed mh-cmd-note)
      (push msg msgs))
    (setq msgs (nreverse msgs))
    ;; Print scan listing if we have more than one message.
    (if (> (length msgs) 1)
        (let* ((msgs-string
                (mapconcat 'identity (mh-list-to-string
                                      (mh-coalesce-msg-list msgs)) " "))
               (lpr-command
                (format mh-lpr-command-format
                        (cond ((listp range)
                               (format "Folder: %s, Messages: %s"
                                       mh-current-folder msgs-string))
                              ((symbolp range)
                               (format "Folder: %s, Sequence: %s"
                                       mh-current-folder range)))))
               (scan-command
                (format "scan %s | %s" msgs-string lpr-command)))
          (if mh-print-background-flag
              (mh-exec-cmd-daemon shell-file-name nil "-c" scan-command)
            (call-process shell-file-name nil nil nil "-c" scan-command))))
    ;; Print the messages
    (dolist (msg msgs)
      (let* ((mhl-command (format "%s %s %s"
                                  (expand-file-name "mhl" mh-lib-progs)
                                  (if mh-mhl-format-file
                                      (format " -form %s" mh-mhl-format-file)
                                    "")
                                  (mh-msg-filename msg)))
             (lpr-command
              (format mh-lpr-command-format
                      (format "%s/%s" mh-current-folder msg)))
             (print-command
              (format "%s | %s" mhl-command lpr-command)))
        (if mh-print-background-flag
            (mh-exec-cmd-daemon shell-file-name nil "-c" print-command)
          (call-process shell-file-name nil nil nil "-c" print-command)))))
  (message "Printing...done"))

(provide 'mh-print)

;; Local Variables:
;; indent-tabs-mode: nil
;; sentence-end-double-space: nil
;; End:

;;; mh-print.el ends here
