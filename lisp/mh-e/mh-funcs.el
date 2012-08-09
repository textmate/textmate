;;; mh-funcs.el --- MH-E functions not everyone will use right away

;; Copyright (C) 1993, 1995, 2001-2012  Free Software Foundation, Inc.

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

;; Putting these functions in a separate file lets MH-E start up faster,
;; since less Lisp code needs to be loaded all at once.

;; Please add the functions in alphabetical order. If only one or two
;; small support routines are needed, place them with the function;
;; otherwise, create a separate section for them.

;;; Change Log:

;;; Code:

(require 'mh-e)
(require 'mh-scan)

;;;###mh-autoload
(defun mh-burst-digest ()
  "Break up digest into separate messages\\<mh-folder-mode-map>.

This command uses the MH command \"burst\" to break out each
message in the digest into its own message. Using this command,
you can quickly delete unwanted messages, like this: Once the
digest is split up, toggle out of MH-Folder Show mode with
\\[mh-toggle-showing] so that the scan lines fill the screen and
messages aren't displayed. Then use \\[mh-delete-msg] to quickly
delete messages that you don't want to read (based on the
\"Subject:\" header field). You can also burst the digest to
reply directly to the people who posted the messages in the
digest. One problem you may encounter is that the \"From:\"
header fields are preceded with a \">\" so that your reply can't
create the \"To:\" field correctly. In this case, you must
correct the \"To:\" field yourself."
  (interactive)
  (let ((digest (mh-get-msg-num t)))
    (mh-process-or-undo-commands mh-current-folder)
    (mh-set-folder-modified-p t)        ; lock folder while bursting
    (message "Bursting digest...")
    (mh-exec-cmd "burst" mh-current-folder digest "-inplace")
    (with-mh-folder-updating (t)
      (beginning-of-line)
      (delete-region (point) (point-max)))
    (mh-regenerate-headers (format "%d-last" digest) t)
    (mh-goto-cur-msg)
    (message "Bursting digest...done")))

;;;###mh-autoload
(defun mh-copy-msg (range folder)
  "Copy RANGE to FOLDER\\<mh-folder-mode-map>.

If you wish to copy a message to another folder, you can use this
command (see the \"-link\" argument to \"refile\"). Like the
command \\[mh-refile-msg], this command prompts you for the name
of the target folder and you can specify a range. Note that
unlike the command \\[mh-refile-msg], the copy takes place
immediately. The original copy remains in the current folder.

Check the documentation of `mh-interactive-range' to see how
RANGE is read in interactive use."
  (interactive (list (mh-interactive-range "Copy")
                     (mh-prompt-for-folder "Copy to" "" t)))
  (let ((msg-list (let ((result ()))
                    (mh-iterate-on-range msg range
                      (mh-notate nil mh-note-copied mh-cmd-note)
                      (push msg result))
                    result)))
    (mh-exec-cmd "refile" (mh-coalesce-msg-list msg-list)
                 "-link" "-src" mh-current-folder folder)))

;;;###mh-autoload
(defun mh-kill-folder ()
  "Remove folder.

Remove all of the messages (files) within the current folder, and
then remove the folder (directory) itself.

Run the abnormal hook `mh-kill-folder-suppress-prompt-hooks'. The
hook functions are called with no arguments and should return a
non-nil value to suppress the normal prompt when you remove a
folder. This is useful for folders that are easily regenerated."
  (interactive)
  (if (or (run-hook-with-args-until-success
           'mh-kill-folder-suppress-prompt-hooks)
          (yes-or-no-p (format "Remove folder %s (and all included messages)? "
                               mh-current-folder)))
      (let ((folder mh-current-folder)
            (window-config mh-previous-window-config))
        (mh-set-folder-modified-p t)    ; lock folder to kill it
        (mh-exec-cmd-daemon "rmf" 'mh-rmf-daemon folder)
        (when (boundp 'mh-speed-folder-map)
          (mh-speed-invalidate-map folder))
        (mh-remove-from-sub-folders-cache folder)
        (mh-set-folder-modified-p nil)  ; so kill-buffer doesn't complain
        (if (and mh-show-buffer (get-buffer mh-show-buffer))
            (kill-buffer mh-show-buffer))
        (if (get-buffer folder)
            (kill-buffer folder))
        (when window-config
          (set-window-configuration window-config))
        (message "Folder %s removed" folder))
    (message "Folder not removed")))

(defun mh-rmf-daemon (process output)
  "The rmf PROCESS puts OUTPUT in temporary buffer.
Display the results only if something went wrong."
  (set-buffer (get-buffer-create mh-temp-buffer))
  (insert-before-markers output)
  (when (save-excursion
          (goto-char (point-min))
          (re-search-forward "^rmf: " (point-max) t))
    (display-buffer mh-temp-buffer)))

;; Shush compiler.
(defvar view-exit-action)

;;;###mh-autoload
(defun mh-list-folders ()
  "List mail folders."
  (interactive)
  (let ((temp-buffer mh-folders-buffer))
    (with-output-to-temp-buffer temp-buffer
      (with-current-buffer temp-buffer
        (erase-buffer)
        (message "Listing folders...")
        (mh-exec-cmd-output "folders" t (if mh-recursive-folders-flag
                                            "-recurse"
                                          "-norecurse"))
        (goto-char (point-min))
        (mh-view-mode-enter)
        (setq view-exit-action 'kill-buffer)
        (message "Listing folders...done")))))

;;;###mh-autoload
(defun mh-pack-folder (range)
  "Pack folder\\<mh-folder-mode-map>.

This command packs the folder, removing gaps from the numbering
sequence. If you don't want to rescan the entire folder
afterward, this command will accept a RANGE. Check the
documentation of `mh-interactive-range' to see how RANGE is read
in interactive use.

This command will ask if you want to process refiles or deletes
first and then either run \\[mh-execute-commands] for you or undo
the pending refiles and deletes.

The hook `mh-pack-folder-hook' is run after the folder is packed;
see its documentation for variables it can use."
  (interactive (list (if current-prefix-arg
                         (mh-read-range "Scan" mh-current-folder t nil t
                                        mh-interpret-number-as-range-flag)
                       '("all"))))
  (let ((threaded-flag (memq 'unthread mh-view-ops)))
    (mh-pack-folder-1 range)
    (mh-goto-cur-msg)
    (when mh-index-data
      (mh-index-update-maps mh-current-folder))
    (cond (threaded-flag (mh-toggle-threads))
          (mh-index-data (mh-index-insert-folder-headers))))
  (run-hooks 'mh-pack-folder-hook)
  (message "Packing folder...done"))

(defun mh-pack-folder-1 (range)
  "Close and pack the current folder.

Display RANGE after packing, or the entire folder if RANGE is nil."
  (mh-process-or-undo-commands mh-current-folder)
  (message "Packing folder...")
  (mh-set-folder-modified-p t)          ; lock folder while packing
  (save-excursion
    (mh-exec-cmd-quiet t "folder" mh-current-folder "-pack"
                       "-norecurse" "-fast"))
  (mh-reset-threads-and-narrowing)
  (mh-regenerate-headers range))

;;;###mh-autoload
(defun mh-page-digest ()
  "Display next message in digest."
  (interactive)
  (mh-in-show-buffer (mh-show-buffer)
    ;; Go to top of screen (in case user moved point).
    (move-to-window-line 0)
    (let ((case-fold-search nil))
      ;; Search for blank line and then for From:
      (or (and (search-forward "\n\n" nil t)
               (re-search-forward "^From:" nil t))
          (error "No more messages in digest")))
    ;; Go back to previous blank line, then forward to the first non-blank.
    (search-backward "\n\n" nil t)
    (forward-line 2)
    (mh-recenter 0)))

;;;###mh-autoload
(defun mh-page-digest-backwards ()
  "Display previous message in digest."
  (interactive)
  (mh-in-show-buffer (mh-show-buffer)
    ;; Go to top of screen (in case user moved point).
    (move-to-window-line 0)
    (let ((case-fold-search nil))
      (beginning-of-line)
      (or (and (search-backward "\n\n" nil t)
               (re-search-backward "^From:" nil t))
          (error "No previous message in digest")))
    ;; Go back to previous blank line, then forward to the first non-blank.
    (if (search-backward "\n\n" nil t)
        (forward-line 2))
    (mh-recenter 0)))

;;;###mh-autoload
(defun mh-pipe-msg (command include-header)
  "Pipe message through shell command COMMAND.

You are prompted for the Unix command through which you wish to
run your message. If you give a prefix argument INCLUDE-HEADER to
this command, the message header is included in the text passed
to the command."
  (interactive
   (list (read-string "Shell command on message: ") current-prefix-arg))
  (let ((msg-file-to-pipe (mh-msg-filename (mh-get-msg-num t)))
        (message-directory default-directory))
    (with-current-buffer (get-buffer-create mh-temp-buffer)
      (erase-buffer)
      (insert-file-contents msg-file-to-pipe)
      (goto-char (point-min))
      (if (not include-header) (search-forward "\n\n"))
      (let ((default-directory message-directory))
        (shell-command-on-region (point) (point-max) command nil)))))

;;;###mh-autoload
(defun mh-sort-folder (&optional extra-args)
  "Sort folder.

By default, messages are sorted by date. The option
`mh-sortm-args' holds extra arguments to pass on to the command
\"sortm\" when a prefix argument EXTRA-ARGS is used."
  (interactive "P")
  (mh-process-or-undo-commands mh-current-folder)
  (setq mh-next-direction 'forward)
  (mh-set-folder-modified-p t)          ; lock folder while sorting
  (message "Sorting folder...")
  (let ((threaded-flag (memq 'unthread mh-view-ops)))
    (mh-exec-cmd "sortm" mh-current-folder (if extra-args mh-sortm-args))
    (when mh-index-data
      (mh-index-update-maps mh-current-folder))
    (message "Sorting folder...done")
    (mh-scan-folder mh-current-folder "all")
    (cond (threaded-flag (mh-toggle-threads))
          (mh-index-data (mh-index-insert-folder-headers)))))

;;;###mh-autoload
(defun mh-store-msg (directory)
  "Unpack message created with \"uudecode\" or \"shar\".

The default DIRECTORY for extraction is the current directory;
however, you have a chance to specify a different extraction
directory. The next time you use this command, the default
directory is the last directory you used. If you would like to
change the initial default directory, customize the option
`mh-store-default-directory', change the value from \"Current\"
to \"Directory\", and then enter the name of the directory for
storing the content of these messages."
  (interactive (list (let ((udir (or mh-store-default-directory
                                     default-directory)))
                       (read-directory-name "Store message in directory: "
                                       udir udir nil))))
  (let ((msg-file-to-store (mh-msg-filename (mh-get-msg-num t))))
    (with-current-buffer (get-buffer-create mh-temp-buffer)
      (erase-buffer)
      (insert-file-contents msg-file-to-store)
      (mh-store-buffer directory))))

(defun mh-store-buffer (directory)
  "Unpack buffer created with \"uudecode\" or \"shar\".

See `mh-store-msg' for a description of DIRECTORY."
  (interactive (list (let ((udir (or mh-store-default-directory
                                     default-directory)))
                       (read-directory-name "Store buffer in directory: "
                                       udir udir nil))))
  (let ((store-directory (expand-file-name directory))
        (sh-start (save-excursion
                    (goto-char (point-min))
                    (if (re-search-forward
                         "^#![ \t]*/bin/sh\\|^#\\|^: " nil t)
                        (progn
                          ;; The "cut here" pattern was removed from above
                          ;; because it seemed to hurt more than help.
                          ;; But keep this to make it easier to put it back.
                          (if (looking-at "^[^a-z0-9\"]*cut here\\b")
                              (forward-line 1))
                          (beginning-of-line)
                          (if (looking-at "^[#:]....+\n\\( ?\n\\)?end$")
                              nil       ;most likely end of a uuencode
                            (point))))))
        (command "sh")
        (uudecode-filename "(unknown filename)")
        log-begin)
    (if (not sh-start)
        (save-excursion
          (goto-char (point-min))
          (if (re-search-forward "^begin [0-7]+ " nil t)
              (setq uudecode-filename
                    (buffer-substring (point)
                                      (progn (end-of-line) (point)))))))
    (with-current-buffer (get-buffer-create mh-log-buffer)
      (setq log-begin (mh-truncate-log-buffer))
      (if (not (file-directory-p store-directory))
          (progn
            (insert "mkdir " directory "\n")
            (call-process "mkdir" nil mh-log-buffer t store-directory)))
      (insert "cd " directory "\n")
      (setq mh-store-default-directory directory)
      (if (not sh-start)
          (progn
            (setq command "uudecode")
            (insert uudecode-filename " being uudecoded...\n"))))
    (set-window-start (display-buffer mh-log-buffer) log-begin) ;watch progress
    (let ((default-directory (file-name-as-directory store-directory)))
      (if (equal (call-process-region sh-start (point-max) command
                                      nil mh-log-buffer t)
                 0)
          (with-current-buffer mh-log-buffer
            (insert "\n(mh-store finished)\n"))
        (error "Error occurred during execution of %s" command)))))

;;;###mh-autoload
(defun mh-undo-folder (&rest ignored)
  "Undo all refiles and deletes in the current folder.
Arguments are IGNORED (for `revert-buffer')."
  (interactive)
  (cond ((or mh-do-not-confirm-flag
             (yes-or-no-p "Undo all commands in folder? "))
         (setq mh-delete-list nil
               mh-refile-list nil
               mh-seq-list nil
               mh-next-direction 'forward)
         (with-mh-folder-updating (nil)
           (mh-remove-all-notation)))
        (t
         (message "Commands not undone"))))

(provide 'mh-funcs)

;; Local Variables:
;; indent-tabs-mode: nil
;; sentence-end-double-space: nil
;; End:

;;; mh-funcs.el ends here
