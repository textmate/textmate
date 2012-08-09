;;; mh-show.el --- MH-Show mode

;; Copyright (C) 1993, 1995, 1997, 2000-2012  Free Software Foundation, Inc.

;; Author: Bill Wohler <wohler@newt.com>
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

;; Mode for showing messages.

;;; Change Log:

;;; Code:

(require 'mh-e)
(require 'mh-scan)

;; Dynamically-created function not found in mh-loaddefs.el.
(autoload 'mh-tool-bar-init "mh-tool-bar")

(require 'font-lock)
(require 'gnus-cite)
(require 'gnus-util)
(require 'goto-addr)

(autoload 'mh-make-buffer-data "mh-mime") ;can't be automatically generated



;;; MH-Folder Commands

(defvar mh-showing-with-headers nil
  "If non-nil, MH-Show buffer contains message with all header fields.
If nil, MH-Show buffer contains message processed normally.")

;;;###mh-autoload
(defun mh-show (&optional message redisplay-flag)
  "Display message\\<mh-folder-mode-map>.

If the message under the cursor is already displayed, this command
scrolls to the beginning of the message. MH-E normally hides a lot of
the superfluous header fields that mailers add to a message, but if
you wish to see all of them, use the command \\[mh-header-display].

Two hooks can be used to control how messages are displayed. The
first hook, `mh-show-mode-hook', is called early on in the
process of the message display. It is usually used to perform
some action on the message's content. The second hook,
`mh-show-hook', is the last thing called after messages are
displayed. It's used to affect the behavior of MH-E in general or
when `mh-show-mode-hook' is too early.

From a program, optional argument MESSAGE can be used to display an
alternative message. The optional argument REDISPLAY-FLAG forces the
redisplay of the message even if the show buffer was already
displaying the correct message.

See the \"mh-show\" customization group for a litany of options that
control what displayed messages look like."
  (interactive (list nil t))
  (when (or redisplay-flag
            (and mh-showing-with-headers
                 (or mh-mhl-format-file mh-clean-message-header-flag)))
    (mh-invalidate-show-buffer))
  (mh-show-msg message))

;;;###mh-autoload
(defun mh-header-display ()
  "Display message with all header fields\\<mh-folder-mode-map>.

Use the command \\[mh-show] to show the message normally again."
  (interactive)
  (and (not mh-showing-with-headers)
       (or mh-mhl-format-file mh-clean-message-header-flag)
       (mh-invalidate-show-buffer))
  (let ((mh-decode-mime-flag nil)
        (mh-mhl-format-file nil)
        (mh-clean-message-header-flag nil))
    (mh-show-msg nil)
    (mh-in-show-buffer (mh-show-buffer)
      (goto-char (point-min))
      (mh-recenter 0))
    (setq mh-showing-with-headers t)))

;;;###mh-autoload
(defun  mh-show-preferred-alternative ()
  "Display message with the default preferred alternative.
This is as if `mm-discouraged-alternatives' is set to nil.

Use the command \\[mh-show] to show the message normally again."
  (interactive)
  (let
      ((mm-discouraged-alternatives))
    (mh-show nil t)))



;;; Support Routines for MH-Folder Commands

;;;###mh-autoload
(defun mh-maybe-show (&optional msg)
  "Display message at cursor, but only if in show mode.
If optional arg MSG is non-nil, display that message instead."
  (if mh-showing-mode (mh-show msg)))

(defun mh-show-msg (msg)
  "Show MSG.

The hook `mh-show-hook' is called after the message has been
displayed."
  (if (not msg)
      (setq msg (mh-get-msg-num t)))
  (mh-showing-mode t)
  (setq mh-page-to-next-msg-flag nil)
  (let ((folder mh-current-folder)
        (folders (list mh-current-folder))
        (clean-message-header mh-clean-message-header-flag)
        (show-window (get-buffer-window mh-show-buffer))
        (display-mime-buttons-flag mh-display-buttons-for-inline-parts-flag))
    (if (not (eq (next-window (minibuffer-window)) (selected-window)))
        (delete-other-windows))         ; force ourself to the top window
    (mh-in-show-buffer (mh-show-buffer)
      (setq mh-display-buttons-for-inline-parts-flag display-mime-buttons-flag)
      (if (and show-window
               (equal (mh-msg-filename msg folder) buffer-file-name))
          (progn                        ;just back up to start
            (goto-char (point-min))
            (if (not clean-message-header)
                (mh-start-of-uncleaned-message)))
        (mh-display-msg msg folder)))
    (unless (mh-window-full-height-p) ; not vertically split
      (shrink-window (- (window-height) (or mh-summary-height
                                            (mh-summary-height)))))
    (mh-recenter nil)
    ;; The following line is a nop which forces update of the scan line so
    ;; that font-lock will update it (if needed)...
    (mh-notate nil nil mh-cmd-note)
    (if (not (memq msg mh-seen-list))
        (setq mh-seen-list (cons msg mh-seen-list)))
    (when mh-update-sequences-after-mh-show-flag
      (mh-update-sequences)
      (when mh-index-data
        (setq folders
              (append (mh-index-delete-from-sequence mh-unseen-seq (list msg))
                      folders)))
      (when (mh-speed-flists-active-p)
        (apply #'mh-speed-flists t folders)))
    (run-hooks 'mh-show-hook)))

;;;###mh-autoload
(defun mh-start-of-uncleaned-message ()
  "Position uninteresting headers off the top of the window."
  (let ((case-fold-search t))
    (re-search-forward
     "^To:\\|^Cc:\\|^From:\\|^Subject:\\|^Date:" nil t)
    (beginning-of-line)
    (mh-recenter 0)))

(defvar mh-show-buffer-mode-line-buffer-id "    {show-%s} %d"
  "Format string to produce `mode-line-buffer-identification' for show buffers.

First argument is folder name. Second is message number.")

;;;###mh-autoload
(defun mh-display-msg (msg-num folder-name)
  "Display MSG-NUM of FOLDER-NAME.
Sets the current buffer to the show buffer."
  (let ((folder (mh-msg-folder folder-name)))
    (set-buffer folder)
    ;; When Gnus uses external displayers it has to keep handles longer. So
    ;; we will delete these handles when mh-quit is called on the folder. It
    ;; would be nicer if there are weak pointers in emacs lisp, then we could
    ;; get the garbage collector to do this for us.
    (unless (mh-buffer-data)
      (setf (mh-buffer-data) (mh-make-buffer-data)))
    ;; Bind variables in folder buffer in case they are local
    (let ((formfile mh-mhl-format-file)
          (clean-message-header mh-clean-message-header-flag)
          (invisible-headers mh-invisible-header-fields-compiled)
          (visible-headers nil)
          (msg-filename (mh-msg-filename msg-num folder-name))
          (show-buffer mh-show-buffer)
          (mm-inline-media-tests mh-mm-inline-media-tests))
      (if (not (file-exists-p msg-filename))
          (error "Message %d does not exist" msg-num))
      (if (and (> mh-show-maximum-size 0)
               (> (elt (file-attributes msg-filename) 7)
                  mh-show-maximum-size)
               (not (y-or-n-p
                     (format
                      "Message %d (%d bytes) exceeds %d bytes. Display it? "
                      msg-num (elt (file-attributes msg-filename) 7)
                      mh-show-maximum-size))))
          (error "Message %d not displayed" msg-num))
      (set-buffer show-buffer)
      (cond ((not (equal msg-filename buffer-file-name))
             (mh-unvisit-file)
             (setq buffer-read-only nil)
             ;; Cleanup old mime handles
             (mh-mime-cleanup)
             (erase-buffer)
             ;; Changing contents, so this hook needs to be reinitialized.
             ;; pgp.el uses this.
             (if (boundp 'write-contents-hooks) ;Emacs 19
                 (kill-local-variable 'write-contents-hooks))
             (if formfile
                 (mh-exec-lib-cmd-output "mhl" "-nobell" "-noclear"
                                         (if (stringp formfile)
                                             (list "-form" formfile))
                                         msg-filename)
               (insert-file-contents-literally msg-filename))
             ;; Use mm to display buffer
             (when (and mh-decode-mime-flag (not formfile))
               (mh-add-missing-mime-version-header)
               (setf (mh-buffer-data) (mh-make-buffer-data))
               (mh-mime-display))
             (mh-show-mode)
             ;; Header cleanup
             (goto-char (point-min))
             (cond (clean-message-header
                    (mh-clean-msg-header (point-min)
                                         invisible-headers
                                         visible-headers)
                    (goto-char (point-min)))
                   (t
                    (mh-start-of-uncleaned-message)))
             (mh-decode-message-header)
             ;; the parts of visiting we want to do (no locking)
             (or (eq buffer-undo-list t) ;don't save undo info for prev msgs
                 (setq buffer-undo-list nil))
             (set-buffer-auto-saved)
             ;; the parts of set-visited-file-name we want to do (no locking)
             (setq buffer-file-name msg-filename)
             (setq buffer-backed-up nil)
             (auto-save-mode 1)
             (set-mark nil)
             (unwind-protect
                 (when (and mh-decode-mime-flag (not formfile))
                   (setq buffer-read-only nil)
                   (mh-display-smileys)
                   (mh-display-emphasis))
               (setq buffer-read-only t))
             (set-buffer-modified-p nil)
             (setq mh-show-folder-buffer folder)
             (setq mode-line-buffer-identification
                   (list (format mh-show-buffer-mode-line-buffer-id
                                 folder-name msg-num)))
             (mh-logo-display)
             (set-buffer folder)
             (setq mh-showing-with-headers nil))))))

(defun mh-msg-folder (folder-name)
  "Return the name of the buffer for FOLDER-NAME."
  folder-name)

;;;###mh-autoload
(defun mh-clean-msg-header (start invisible-headers visible-headers)
  "Flush extraneous lines in message header.

Header is cleaned from START to the end of the message header.
INVISIBLE-HEADERS contains a regular expression specifying lines
to delete from the header. VISIBLE-HEADERS contains a regular
expression specifying the lines to display. INVISIBLE-HEADERS is
ignored if VISIBLE-HEADERS is non-nil."
  ;; XXX Note that MH-E no longer supports the `mh-visible-headers'
  ;; variable, so this function could be trimmed of this feature too."
  (let ((case-fold-search t)
        (buffer-read-only nil))
    (save-restriction
      (goto-char start)
      (if (search-forward "\n\n" nil 'move)
          (backward-char 1))
      (narrow-to-region start (point))
      (goto-char (point-min))
      (if visible-headers
          (while (< (point) (point-max))
            (cond ((looking-at visible-headers)
                   (forward-line 1)
                   (while (looking-at "[ \t]") (forward-line 1)))
                  (t
                   (mh-delete-line 1)
                   (while (looking-at "[ \t]")
                     (mh-delete-line 1)))))
        (while (re-search-forward invisible-headers nil t)
          (beginning-of-line)
          (mh-delete-line 1)
          (while (looking-at "[ \t]")
            (mh-delete-line 1)))))
    (let ((mh-compose-skipped-header-fields ()))
      (mh-letter-hide-all-skipped-fields))
    (unlock-buffer)))

;;;###mh-autoload
(defun mh-invalidate-show-buffer ()
  "Invalidate the show buffer so we must update it to use it."
  (if (get-buffer mh-show-buffer)
      (with-current-buffer mh-show-buffer
        (mh-unvisit-file))))

(defun mh-unvisit-file ()
  "Separate current buffer from the message file it was visiting."
  (or (not (buffer-modified-p))
      (null buffer-file-name)           ;we've been here before
      (yes-or-no-p (format "Message %s modified; discard changes? "
                           (file-name-nondirectory buffer-file-name)))
      (error "Changes preserved"))
  (clear-visited-file-modtime)
  (unlock-buffer)
  (setq buffer-file-name nil))

(defun mh-summary-height ()
  "Return ideal value for the variable `mh-summary-height'.
The current frame height is taken into consideration."
  (or (and (fboundp 'frame-height)
           (> (frame-height) 24)
           (min 10 (/ (frame-height) 6)))
      4))



;; Infrastructure to generate show-buffer functions from folder functions
;; XEmacs does not have deactivate-mark? What is the equivalent of
;; transient-mark-mode for XEmacs? Should we be restoring the mark in the
;; folder buffer after the operation has been carried out.
(defmacro mh-defun-show-buffer (function original-function
                                         &optional dont-return)
  "Define FUNCTION to run ORIGINAL-FUNCTION in folder buffer.
If the buffer we start in is still visible and DONT-RETURN is nil
then switch to it after that."
  `(defun ,function ()
     ,(format "Calls %s from the message's folder.\n%s\nSee `%s' for more info.\n"
              original-function
              (if dont-return ""
                "When function completes, returns to the show buffer if it is
still visible.\n")
              original-function)
     (interactive)
     (when (buffer-live-p (get-buffer mh-show-folder-buffer))
       (let ((config (current-window-configuration))
             (folder-buffer mh-show-folder-buffer)
             (normal-exit nil)
             ,@(if dont-return () '((cur-buffer-name (buffer-name)))))
         (pop-to-buffer mh-show-folder-buffer nil)
         (unless (equal (buffer-name
                         (window-buffer (frame-first-window (selected-frame))))
                        folder-buffer)
           (delete-other-windows))
         (mh-goto-cur-msg t)
         (mh-funcall-if-exists deactivate-mark)
         (unwind-protect
             (prog1 (call-interactively (function ,original-function))
               (setq normal-exit t))
           (mh-funcall-if-exists deactivate-mark)
           (when (eq major-mode 'mh-folder-mode)
             (mh-funcall-if-exists hl-line-highlight))
           (cond ((not normal-exit)
                  (set-window-configuration config))
                 ,(if dont-return
                      `(t (setq mh-previous-window-config config))
                    `((and (get-buffer cur-buffer-name)
                           (window-live-p (get-buffer-window
                                           (get-buffer cur-buffer-name))))
                      (pop-to-buffer (get-buffer cur-buffer-name) nil)))))))))

;; Generate interactive functions for the show buffer from the corresponding
;; folder functions.
(mh-defun-show-buffer mh-show-previous-undeleted-msg
                      mh-previous-undeleted-msg)
(mh-defun-show-buffer mh-show-next-undeleted-msg
                      mh-next-undeleted-msg)
(mh-defun-show-buffer mh-show-quit mh-quit)
(mh-defun-show-buffer mh-show-delete-msg mh-delete-msg)
(mh-defun-show-buffer mh-show-refile-msg mh-refile-msg)
(mh-defun-show-buffer mh-show-undo mh-undo)
(mh-defun-show-buffer mh-show-execute-commands mh-execute-commands)
(mh-defun-show-buffer mh-show-reply mh-reply t)
(mh-defun-show-buffer mh-show-redistribute mh-redistribute)
(mh-defun-show-buffer mh-show-forward mh-forward t)
(mh-defun-show-buffer mh-show-header-display mh-header-display)
(mh-defun-show-buffer mh-show-refile-or-write-again
                      mh-refile-or-write-again)
(mh-defun-show-buffer mh-show-show mh-show)
(mh-defun-show-buffer mh-show-show-preferred-alternative mh-show-preferred-alternative)
(mh-defun-show-buffer mh-show-write-message-to-file
                      mh-write-msg-to-file)
(mh-defun-show-buffer mh-show-extract-rejected-mail
                      mh-extract-rejected-mail t)
(mh-defun-show-buffer mh-show-delete-msg-no-motion
                      mh-delete-msg-no-motion)
(mh-defun-show-buffer mh-show-first-msg mh-first-msg)
(mh-defun-show-buffer mh-show-last-msg mh-last-msg)
(mh-defun-show-buffer mh-show-copy-msg mh-copy-msg)
(mh-defun-show-buffer mh-show-edit-again mh-edit-again t)
(mh-defun-show-buffer mh-show-goto-msg mh-goto-msg)
(mh-defun-show-buffer mh-show-inc-folder mh-inc-folder)
(mh-defun-show-buffer mh-show-delete-subject-or-thread
                      mh-delete-subject-or-thread)
(mh-defun-show-buffer mh-show-delete-subject mh-delete-subject)
(mh-defun-show-buffer mh-show-print-msg mh-print-msg)
(mh-defun-show-buffer mh-show-send mh-send t)
(mh-defun-show-buffer mh-show-toggle-showing mh-toggle-showing t)
(mh-defun-show-buffer mh-show-pipe-msg mh-pipe-msg t)
(mh-defun-show-buffer mh-show-sort-folder mh-sort-folder)
(mh-defun-show-buffer mh-show-visit-folder mh-visit-folder t)
(mh-defun-show-buffer mh-show-rescan-folder mh-rescan-folder)
(mh-defun-show-buffer mh-show-pack-folder mh-pack-folder)
(mh-defun-show-buffer mh-show-kill-folder mh-kill-folder t)
(mh-defun-show-buffer mh-show-list-folders mh-list-folders t)
(mh-defun-show-buffer mh-show-undo-folder mh-undo-folder)
(mh-defun-show-buffer mh-show-delete-msg-from-seq
                      mh-delete-msg-from-seq)
(mh-defun-show-buffer mh-show-delete-seq mh-delete-seq)
(mh-defun-show-buffer mh-show-list-sequences mh-list-sequences)
(mh-defun-show-buffer mh-show-narrow-to-seq mh-narrow-to-seq)
(mh-defun-show-buffer mh-show-put-msg-in-seq mh-put-msg-in-seq)
(mh-defun-show-buffer mh-show-msg-is-in-seq mh-msg-is-in-seq)
(mh-defun-show-buffer mh-show-widen mh-widen)
(mh-defun-show-buffer mh-show-narrow-to-subject mh-narrow-to-subject)
(mh-defun-show-buffer mh-show-narrow-to-from mh-narrow-to-from)
(mh-defun-show-buffer mh-show-narrow-to-cc mh-narrow-to-cc)
(mh-defun-show-buffer mh-show-narrow-to-range mh-narrow-to-range)
(mh-defun-show-buffer mh-show-narrow-to-to mh-narrow-to-to)
(mh-defun-show-buffer mh-show-store-msg mh-store-msg)
(mh-defun-show-buffer mh-show-page-digest mh-page-digest)
(mh-defun-show-buffer mh-show-page-digest-backwards
                      mh-page-digest-backwards)
(mh-defun-show-buffer mh-show-burst-digest mh-burst-digest)
(mh-defun-show-buffer mh-show-page-msg mh-page-msg)
(mh-defun-show-buffer mh-show-previous-page mh-previous-page)
(mh-defun-show-buffer mh-show-modify mh-modify t)
(mh-defun-show-buffer mh-show-next-button mh-next-button)
(mh-defun-show-buffer mh-show-prev-button mh-prev-button)
(mh-defun-show-buffer mh-show-toggle-mime-part mh-folder-toggle-mime-part)
(mh-defun-show-buffer mh-show-save-mime-part mh-folder-save-mime-part)
(mh-defun-show-buffer mh-show-inline-mime-part mh-folder-inline-mime-part)
(mh-defun-show-buffer mh-show-toggle-threads mh-toggle-threads)
(mh-defun-show-buffer mh-show-thread-delete mh-thread-delete)
(mh-defun-show-buffer mh-show-thread-refile mh-thread-refile)
(mh-defun-show-buffer mh-show-update-sequences mh-update-sequences)
(mh-defun-show-buffer mh-show-next-unread-msg mh-next-unread-msg)
(mh-defun-show-buffer mh-show-previous-unread-msg mh-previous-unread-msg)
(mh-defun-show-buffer mh-show-thread-ancestor mh-thread-ancestor)
(mh-defun-show-buffer mh-show-thread-next-sibling mh-thread-next-sibling)
(mh-defun-show-buffer mh-show-thread-previous-sibling
                      mh-thread-previous-sibling)
(mh-defun-show-buffer mh-show-index-visit-folder mh-index-visit-folder t)
(mh-defun-show-buffer mh-show-toggle-tick mh-toggle-tick)
(mh-defun-show-buffer mh-show-narrow-to-tick mh-narrow-to-tick)
(mh-defun-show-buffer mh-show-junk-blacklist mh-junk-blacklist)
(mh-defun-show-buffer mh-show-junk-whitelist mh-junk-whitelist)
(mh-defun-show-buffer mh-show-index-new-messages mh-index-new-messages)
(mh-defun-show-buffer mh-show-index-ticked-messages mh-index-ticked-messages)
(mh-defun-show-buffer mh-show-index-sequenced-messages
                      mh-index-sequenced-messages)
(mh-defun-show-buffer mh-show-catchup mh-catchup)
(mh-defun-show-buffer mh-show-ps-print-toggle-color mh-ps-print-toggle-color)
(mh-defun-show-buffer mh-show-ps-print-toggle-faces mh-ps-print-toggle-faces)
(mh-defun-show-buffer mh-show-ps-print-msg-file mh-ps-print-msg-file)
(mh-defun-show-buffer mh-show-ps-print-msg mh-ps-print-msg)
(mh-defun-show-buffer mh-show-toggle-mime-buttons mh-toggle-mime-buttons)
(mh-defun-show-buffer mh-show-display-with-external-viewer
                      mh-display-with-external-viewer)



;;; Sequence Menu

(easy-menu-define
  mh-show-sequence-menu mh-show-mode-map "Menu for MH-E folder-sequence."
  '("Sequence"
    ["Add Message to Sequence..."       mh-show-put-msg-in-seq t]
    ["List Sequences for Message"       mh-show-msg-is-in-seq t]
    ["Delete Message from Sequence..."  mh-show-delete-msg-from-seq t]
    ["List Sequences in Folder..."      mh-show-list-sequences t]
    ["Delete Sequence..."               mh-show-delete-seq t]
    ["Narrow to Sequence..."            mh-show-narrow-to-seq t]
    ["Widen from Sequence"              mh-show-widen t]
    "--"
    ["Narrow to Subject Sequence"       mh-show-narrow-to-subject t]
    ["Narrow to Tick Sequence"          mh-show-narrow-to-tick
     (with-current-buffer mh-show-folder-buffer
       (and mh-tick-seq (mh-seq-msgs (mh-find-seq mh-tick-seq))))]
    ["Delete Rest of Same Subject"      mh-show-delete-subject t]
    ["Toggle Tick Mark"                 mh-show-toggle-tick t]
    "--"
    ["Push State Out to MH"             mh-show-update-sequences t]))

;;; Message Menu

(easy-menu-define
  mh-show-message-menu mh-show-mode-map "Menu for MH-E folder-message."
  '("Message"
    ["Show Message"                     mh-show-show t]
    ["Show Message with Header"         mh-show-header-display t]
    ["Show Message with Preferred Alternative"
                                        mh-show-show-preferred-alternative t]
    ["Next Message"                     mh-show-next-undeleted-msg t]
    ["Previous Message"                 mh-show-previous-undeleted-msg t]
    ["Go to First Message"              mh-show-first-msg t]
    ["Go to Last Message"               mh-show-last-msg t]
    ["Go to Message by Number..."       mh-show-goto-msg t]
    ["Modify Message"                   mh-show-modify t]
    ["Delete Message"                   mh-show-delete-msg t]
    ["Refile Message"                   mh-show-refile-msg t]
    ["Undo Delete/Refile"               mh-show-undo t]
    ["Process Delete/Refile"            mh-show-execute-commands t]
    "--"
    ["Compose a New Message"            mh-send t]
    ["Reply to Message..."              mh-show-reply t]
    ["Forward Message..."               mh-show-forward t]
    ["Redistribute Message..."          mh-show-redistribute t]
    ["Edit Message Again"               mh-show-edit-again t]
    ["Re-edit a Bounced Message"        mh-show-extract-rejected-mail t]
    "--"
    ["Copy Message to Folder..."        mh-show-copy-msg t]
    ["Print Message"                    mh-show-print-msg t]
    ["Write Message to File..."         mh-show-write-msg-to-file t]
    ["Pipe Message to Command..."       mh-show-pipe-msg t]
    ["Unpack Uuencoded Message..."      mh-show-store-msg t]
    ["Burst Digest Message"             mh-show-burst-digest t]))

;;; Folder Menu

(easy-menu-define
  mh-show-folder-menu mh-show-mode-map  "Menu for MH-E folder."
  '("Folder"
    ["Incorporate New Mail"             mh-show-inc-folder t]
    ["Toggle Show/Folder"               mh-show-toggle-showing t]
    ["Execute Delete/Refile"            mh-show-execute-commands t]
    ["Rescan Folder"                    mh-show-rescan-folder t]
    ["Thread Folder"                    mh-show-toggle-threads t]
    ["Pack Folder"                      mh-show-pack-folder t]
    ["Sort Folder"                      mh-show-sort-folder t]
    "--"
    ["List Folders"                     mh-show-list-folders t]
    ["Visit a Folder..."                mh-show-visit-folder t]
    ["View New Messages"                mh-show-index-new-messages t]
    ["Search..."                        mh-search t]
    "--"
    ["Quit MH-E"                        mh-quit t]))



;;; MH-Show Keys

(gnus-define-keys mh-show-mode-map
  " "    mh-show-page-msg
  "!"    mh-show-refile-or-write-again
  "'"    mh-show-toggle-tick
  ","    mh-show-header-display
  "."    mh-show-show
  ":"    mh-show-show-preferred-alternative
  ">"    mh-show-write-message-to-file
  "?"    mh-help
  "E"    mh-show-extract-rejected-mail
  "M"    mh-show-modify
  "\177" mh-show-previous-page
  "\C-d" mh-show-delete-msg-no-motion
  "\t"   mh-show-next-button
  [backtab] mh-show-prev-button
  "\M-\t" mh-show-prev-button
  "\ed"  mh-show-redistribute
  "^"    mh-show-refile-msg
  "c"    mh-show-copy-msg
  "d"    mh-show-delete-msg
  "e"    mh-show-edit-again
  "f"    mh-show-forward
  "g"    mh-show-goto-msg
  "i"    mh-show-inc-folder
  "k"    mh-show-delete-subject-or-thread
  "m"    mh-show-send
  "n"    mh-show-next-undeleted-msg
  "\M-n" mh-show-next-unread-msg
  "o"    mh-show-refile-msg
  "p"    mh-show-previous-undeleted-msg
  "\M-p" mh-show-previous-unread-msg
  "q"    mh-show-quit
  "r"    mh-show-reply
  "s"    mh-show-send
  "t"    mh-show-toggle-showing
  "u"    mh-show-undo
  "x"    mh-show-execute-commands
  "v"    mh-show-index-visit-folder
  "|"    mh-show-pipe-msg)

(gnus-define-keys (mh-show-folder-map "F" mh-show-mode-map)
  "?"    mh-prefix-help
  "'"    mh-index-ticked-messages
  "S"    mh-show-sort-folder
  "c"    mh-show-catchup
  "f"    mh-show-visit-folder
  "k"    mh-show-kill-folder
  "l"    mh-show-list-folders
  "n"    mh-index-new-messages
  "o"    mh-show-visit-folder
  "q"    mh-show-index-sequenced-messages
  "r"    mh-show-rescan-folder
  "s"    mh-search
  "t"    mh-show-toggle-threads
  "u"    mh-show-undo-folder
  "v"    mh-show-visit-folder)

(gnus-define-keys (mh-show-sequence-map "S" mh-show-mode-map)
  "'"    mh-show-narrow-to-tick
  "?"    mh-prefix-help
  "d"    mh-show-delete-msg-from-seq
  "k"    mh-show-delete-seq
  "l"    mh-show-list-sequences
  "n"    mh-show-narrow-to-seq
  "p"    mh-show-put-msg-in-seq
  "s"    mh-show-msg-is-in-seq
  "w"    mh-show-widen)

(define-key mh-show-mode-map "I" mh-inc-spool-map)

(gnus-define-keys (mh-show-junk-map "J" mh-show-mode-map)
  "?"    mh-prefix-help
  "b"    mh-show-junk-blacklist
  "w"    mh-show-junk-whitelist)

(gnus-define-keys (mh-show-ps-print-map "P" mh-show-mode-map)
  "?"   mh-prefix-help
  "C"   mh-show-ps-print-toggle-color
  "F"   mh-show-ps-print-toggle-faces
  "f"   mh-show-ps-print-msg-file
  "l"   mh-show-print-msg
  "p"   mh-show-ps-print-msg)

(gnus-define-keys (mh-show-thread-map "T" mh-show-mode-map)
  "?"    mh-prefix-help
  "u"    mh-show-thread-ancestor
  "p"    mh-show-thread-previous-sibling
  "n"    mh-show-thread-next-sibling
  "t"    mh-show-toggle-threads
  "d"    mh-show-thread-delete
  "o"    mh-show-thread-refile)

(gnus-define-keys (mh-show-limit-map "/" mh-show-mode-map)
  "'"    mh-show-narrow-to-tick
  "?"    mh-prefix-help
  "c"    mh-show-narrow-to-cc
  "g"    mh-show-narrow-to-range
  "m"    mh-show-narrow-to-from
  "s"    mh-show-narrow-to-subject
  "t"    mh-show-narrow-to-to
  "w"    mh-show-widen)

(gnus-define-keys (mh-show-extract-map "X" mh-show-mode-map)
  "?"    mh-prefix-help
  "s"    mh-show-store-msg
  "u"    mh-show-store-msg)

(gnus-define-keys (mh-show-digest-map "D" mh-show-mode-map)
  "?"    mh-prefix-help
  " "    mh-show-page-digest
  "\177" mh-show-page-digest-backwards
  "b"    mh-show-burst-digest)

(gnus-define-keys (mh-show-mime-map "K" mh-show-mode-map)
  "?"           mh-prefix-help
  "a"           mh-mime-save-parts
  "e"           mh-show-display-with-external-viewer
  "v"           mh-show-toggle-mime-part
  "o"           mh-show-save-mime-part
  "i"           mh-show-inline-mime-part
  "t"           mh-show-toggle-mime-buttons
  "\t"          mh-show-next-button
  [backtab]     mh-show-prev-button
  "\M-\t"       mh-show-prev-button)



;;; MH-Show Font Lock

(defun mh-header-field-font-lock (field limit)
  "Return the value of a header field FIELD to font-lock.
Argument LIMIT limits search."
  (if (= (point) limit)
      nil
    (let* ((mail-header-end (mh-mail-header-end))
           (lesser-limit (if (< mail-header-end limit) mail-header-end limit))
           (case-fold-search t))
      (when (and (< (point) mail-header-end) ;Only within header
                 (re-search-forward (format "^%s" field) lesser-limit t))
        (let ((match-one-b (match-beginning 0))
              (match-one-e (match-end 0)))
          (mh-header-field-end)
          (if (> (point) limit)         ;Don't search for end beyond limit
              (goto-char limit))
          (set-match-data (list match-one-b match-one-e
                                (1+ match-one-e) (point)))
          t)))))

(defun mh-header-to-font-lock (limit)
  "Return the value of a header field To to font-lock.
Argument LIMIT limits search."
  (mh-header-field-font-lock "To:" limit))

(defun mh-header-cc-font-lock (limit)
  "Return the value of a header field cc to font-lock.
Argument LIMIT limits search."
  (mh-header-field-font-lock "cc:" limit))

(defun mh-header-subject-font-lock (limit)
  "Return the value of a header field Subject to font-lock.
Argument LIMIT limits search."
  (mh-header-field-font-lock "Subject:" limit))

(defun mh-letter-header-font-lock (limit)
  "Return the entire mail header to font-lock.
Argument LIMIT limits search."
  (if (= (point) limit)
      nil
    (let* ((mail-header-end (save-match-data (mh-mail-header-end)))
           (lesser-limit (if (< mail-header-end limit) mail-header-end limit)))
      (when (mh-in-header-p)
        (set-match-data (list 1 lesser-limit))
        (goto-char lesser-limit)
        t))))

(defun mh-show-font-lock-fontify-region (beg end loudly)
  "Limit font-lock in `mh-show-mode' to the header.

Used when the option `mh-highlight-citation-style' is set to
\"Gnus\", leaving the body to be dealt with by Gnus highlighting.
The region between BEG and END is given over to be fontified and
LOUDLY controls if a user sees a message about the fontification
operation."
  (let ((header-end (mh-mail-header-end)))
    (cond
     ((and (< beg header-end)(< end header-end))
      (font-lock-default-fontify-region beg end loudly))
     ((and (< beg header-end)(>= end header-end))
      (font-lock-default-fontify-region beg header-end loudly))
     (t
      nil))))

(defvar mh-show-font-lock-keywords
  '(("^\\(From:\\|Sender:\\)\\(.*\\)"
     (1 'default)
     (2 'mh-show-from))
    (mh-header-to-font-lock
     (0 'default)
     (1 'mh-show-to))
    (mh-header-cc-font-lock
     (0 'default)
     (1 'mh-show-cc))
    ("^\\(Reply-To:\\|Return-Path:\\)\\(.*\\)$"
     (1 'default)
     (2 'mh-show-from))
    (mh-header-subject-font-lock
     (0 'default)
     (1 'mh-show-subject))
    ("^\\(Apparently-To:\\|Newsgroups:\\)\\(.*\\)"
     (1 'default)
     (2 'mh-show-cc))
    ("^\\(In-reply-to\\|Date\\):\\(.*\\)$"
     (1 'default)
     (2 'mh-show-date))
    (mh-letter-header-font-lock
     (0 'mh-show-header append t)))
  "Additional expressions to highlight in MH-Show buffers.")

;;;###mh-autoload
(defun mh-show-font-lock-keywords ()
  "Return variable `mh-show-font-lock-keywords'."
  mh-show-font-lock-keywords)

(defvar mh-show-font-lock-keywords-with-cite
  (let* ((cite-chars "[>|}]")
         (cite-prefix "A-Za-z")
         (cite-suffix (concat cite-prefix "0-9_.@-`'\"")))
    (append
     mh-show-font-lock-keywords
     (list
      ;; Use MATCH-ANCHORED to effectively anchor the regexp left side.
      `(,cite-chars
        (,(concat "\\=[ \t]*"
                  "\\(\\([" cite-prefix "]+[" cite-suffix "]*\\)?"
                  "\\(" cite-chars "[ \t]*\\)\\)+"
                  "\\(.*\\)")
         (beginning-of-line) (end-of-line)
         (2 font-lock-constant-face nil t)
         (4 font-lock-comment-face nil t))))))
  "Additional expressions to highlight in MH-Show buffers.")

;;;###mh-autoload
(defun mh-show-font-lock-keywords-with-cite ()
  "Return variable `mh-show-font-lock-keywords-with-cite'."
  mh-show-font-lock-keywords-with-cite)



;;; MH-Show Mode

;; Ensure new buffers won't get this mode if default major-mode is nil.
(put 'mh-show-mode 'mode-class 'special)

;; Shush compiler.
(defvar font-lock-auto-fontify)

;;;###mh-autoload
(define-derived-mode mh-show-mode text-mode "MH-Show"
  "Major mode for showing messages in MH-E.\\<mh-show-mode-map>

Email addresses and URLs in the message are highlighted if the
option `goto-address-highlight-p' is on, which it is by default.
To view the web page for a highlighted URL or to send a message
using a highlighted email address, use the middle mouse button or
\\[goto-address-at-point]. See Info node `(mh-e)Sending Mail' to
see how to configure Emacs to send the message using MH-E.

The hook `mh-show-mode-hook' is called upon entry to this mode.

See also `mh-folder-mode'.

\\{mh-show-mode-map}"
  (mh-do-in-gnu-emacs
   (if (boundp 'tool-bar-map)
       (set (make-local-variable 'tool-bar-map) mh-show-tool-bar-map)))
  (mh-do-in-xemacs
    (mh-tool-bar-init :show))
  (set (make-local-variable 'mail-header-separator) mh-mail-header-separator)
  (setq paragraph-start (default-value 'paragraph-start))
  (mh-show-unquote-From)
  (mh-show-xface)
  (mh-show-addr)
  (setq buffer-invisibility-spec '((vanish . t) t))
  (set (make-local-variable 'line-move-ignore-invisible) t)
  (make-local-variable 'font-lock-defaults)
  ;;(set (make-local-variable 'font-lock-support-mode) nil)
  (cond
   ((equal mh-highlight-citation-style 'font-lock)
    (setq font-lock-defaults '(mh-show-font-lock-keywords-with-cite t)))
   ((equal mh-highlight-citation-style 'gnus)
    (setq font-lock-defaults '((mh-show-font-lock-keywords)
                               t nil nil nil
                               (font-lock-fontify-region-function
                                . mh-show-font-lock-fontify-region)))
    (mh-gnus-article-highlight-citation))
   (t
    (setq font-lock-defaults '(mh-show-font-lock-keywords t))))
  (if (and (featurep 'xemacs)
           font-lock-auto-fontify)
      (turn-on-font-lock))
  (when mh-decode-mime-flag
    (mh-make-local-hook 'kill-buffer-hook)
    (add-hook 'kill-buffer-hook 'mh-mime-cleanup nil t))
  (easy-menu-add mh-show-sequence-menu)
  (easy-menu-add mh-show-message-menu)
  (easy-menu-add mh-show-folder-menu)
  (make-local-variable 'mh-show-folder-buffer)
  (buffer-disable-undo)
  (setq buffer-read-only t)
  (use-local-map mh-show-mode-map))



;;; Support Routines

(defun mh-show-unquote-From ()
  "Decode >From at beginning of lines for `mh-show-mode'."
  (save-excursion
    (let ((modified (buffer-modified-p))
          (case-fold-search nil)
          (buffer-read-only nil))
      (goto-char (mh-mail-header-end))
      (while (re-search-forward "^>From" nil t)
        (replace-match "From"))
      (set-buffer-modified-p modified))))

;;;###mh-autoload
(defun mh-show-addr ()
  "Use `goto-address'."
  (goto-address))

;;;###mh-autoload
(defun mh-gnus-article-highlight-citation ()
  "Highlight cited text in current buffer using Gnus."
  (interactive)
  ;; Don't allow Gnus to create buttons while highlighting, maybe this is bad
  ;; style?
  (flet ((gnus-article-add-button (&rest args) nil))
    (let* ((modified (buffer-modified-p))
           (gnus-article-buffer (buffer-name))
           (gnus-cite-face-list `(,@(cdr gnus-cite-face-list)
                                    ,(car gnus-cite-face-list))))
      (gnus-article-highlight-citation t)
      (set-buffer-modified-p modified))))

(provide 'mh-show)

;; Local Variables:
;; indent-tabs-mode: nil
;; sentence-end-double-space: nil
;; End:

;;; mh-show.el ends here
