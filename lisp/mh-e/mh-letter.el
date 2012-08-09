;;; mh-letter.el --- MH-Letter mode

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

;; Mode for composing and sending a draft message.

;; Functions that would ordinarily be in here that are needed by
;; mh-show.el should be placed in the Message Utilities section in
;; mh-utils.el. That will help prevent the loading of this file until
;; a message is actually composed.

;;; Change Log:

;;; Code:

(require 'mh-e)

(require 'gnus-util)

;; Dynamically-created functions not found in mh-loaddefs.el.
(autoload 'mh-tool-bar-letter-buttons-init "mh-tool-bar")
(autoload 'mh-tool-bar-init "mh-tool-bar")

(autoload 'mml-insert-tag "mml")

;;; Variables

(defvar mh-letter-complete-function-alist
  '((bcc . mh-alias-letter-expand-alias)
    (cc . mh-alias-letter-expand-alias)
    (dcc . mh-alias-letter-expand-alias)
    (fcc . mh-folder-expand-at-point)
    (from . mh-alias-letter-expand-alias)
    (mail-followup-to . mh-alias-letter-expand-alias)
    (mail-reply-to . mh-alias-letter-expand-alias)
    (reply-to . mh-alias-letter-expand-alias)
    (to . mh-alias-letter-expand-alias))
  "Alist of header fields and completion functions to use.")

(defvar mh-yank-hooks nil
  "Obsolete hook for modifying a citation just inserted in the mail buffer.

Each hook function can find the citation between point and mark.
And each hook function should leave point and mark around the
citation text as modified.

This is a normal hook, misnamed for historical reasons. It is
semi-obsolete and is only used if `mail-citation-hook' is nil.")



;;; Letter Menu

(easy-menu-define
  mh-letter-menu mh-letter-mode-map "Menu for MH-E letter mode."
  '("Letter"
    ["Send This Draft"          mh-send-letter t]
    ["Split Current Line"       mh-open-line t]
    ["Check Recipient"          mh-check-whom t]
    ["Yank Current Message"     mh-yank-cur-msg t]
    ["Insert a Message..."      mh-insert-letter t]
    ["Insert Signature"         mh-insert-signature t]
    ("Encrypt/Sign Message"
     ["Sign Message"
      mh-mml-secure-message-sign mh-pgp-support-flag]
     ["Encrypt Message"
      mh-mml-secure-message-encrypt mh-pgp-support-flag]
     ["Sign+Encrypt Message"
      mh-mml-secure-message-signencrypt mh-pgp-support-flag]
     ["Disable Security"
      mh-mml-unsecure-message mh-pgp-support-flag]
     "--"
     "Security Method"
     ["PGP (MIME)" (setq mh-mml-method-default "pgpmime")
      :style radio
      :selected (equal mh-mml-method-default "pgpmime")]
     ["PGP" (setq mh-mml-method-default "pgp")
      :style radio
      :selected (equal mh-mml-method-default "pgp")]
     ["S/MIME" (setq mh-mml-method-default "smime")
      :style radio
      :selected (equal mh-mml-method-default "smime")]
     "--"
     ["Save Method as Default"
      (customize-save-variable 'mh-mml-method-default mh-mml-method-default) t]
     )
    ["Compose Insertion..."      mh-compose-insertion t]
    ["Compose Compressed tar (MH)..."
     mh-mh-compose-external-compressed-tar t]
    ["Compose Get File (MH)..."       mh-mh-compose-anon-ftp t]
    ["Compose Forward..."        mh-compose-forward t]
    ;; The next two will have to be merged. But I also need to make sure the
    ;; user can't mix tags of both types.
    ["Pull in All Compositions (MH)"
     mh-mh-to-mime (mh-mh-directive-present-p)]
    ["Pull in All Compositions (MML)"
     mh-mml-to-mime (mh-mml-tag-present-p)]
    ["Revert to Non-MIME Edit (MH)"
     mh-mh-to-mime-undo (equal mh-compose-insertion 'mh)]
    ["Kill This Draft"          mh-fully-kill-draft t]))



;;; MH-Letter Keys

;; If this changes, modify mh-letter-mode-help-messages accordingly, above.
(gnus-define-keys  mh-letter-mode-map
  " "                   mh-letter-complete-or-space
  ","                   mh-letter-confirm-address
  "\C-c?"               mh-help
  "\C-c\C-\\"           mh-fully-kill-draft ;if no C-q
  "\C-c\C-^"            mh-insert-signature ;if no C-s
  "\C-c\C-c"            mh-send-letter
  "\C-c\C-d"            mh-insert-identity
  "\C-c\C-e"            mh-mh-to-mime
  "\C-c\C-f\C-a"        mh-to-field
  "\C-c\C-f\C-b"        mh-to-field
  "\C-c\C-f\C-c"        mh-to-field
  "\C-c\C-f\C-d"        mh-to-field
  "\C-c\C-f\C-f"        mh-to-fcc
  "\C-c\C-f\C-l"        mh-to-field
  "\C-c\C-f\C-m"        mh-to-field
  "\C-c\C-f\C-r"        mh-to-field
  "\C-c\C-f\C-s"        mh-to-field
  "\C-c\C-f\C-t"        mh-to-field
  "\C-c\C-fa"           mh-to-field
  "\C-c\C-fb"           mh-to-field
  "\C-c\C-fc"           mh-to-field
  "\C-c\C-fd"           mh-to-field
  "\C-c\C-ff"           mh-to-fcc
  "\C-c\C-fl"           mh-to-field
  "\C-c\C-fm"           mh-to-field
  "\C-c\C-fr"           mh-to-field
  "\C-c\C-fs"           mh-to-field
  "\C-c\C-ft"           mh-to-field
  "\C-c\C-i"            mh-insert-letter
  "\C-c\C-m\C-e"        mh-mml-secure-message-encrypt
  "\C-c\C-m\C-f"        mh-compose-forward
  "\C-c\C-m\C-g"        mh-mh-compose-anon-ftp
  "\C-c\C-m\C-i"        mh-compose-insertion
  "\C-c\C-m\C-m"        mh-mml-to-mime
  "\C-c\C-m\C-n"        mh-mml-unsecure-message
  "\C-c\C-m\C-s"        mh-mml-secure-message-sign
  "\C-c\C-m\C-t"        mh-mh-compose-external-compressed-tar
  "\C-c\C-m\C-u"        mh-mh-to-mime-undo
  "\C-c\C-m\C-x"        mh-mh-compose-external-type
  "\C-c\C-mee"          mh-mml-secure-message-encrypt
  "\C-c\C-mes"          mh-mml-secure-message-signencrypt
  "\C-c\C-mf"           mh-compose-forward
  "\C-c\C-mg"           mh-mh-compose-anon-ftp
  "\C-c\C-mi"           mh-compose-insertion
  "\C-c\C-mm"           mh-mml-to-mime
  "\C-c\C-mn"           mh-mml-unsecure-message
  "\C-c\C-mse"          mh-mml-secure-message-signencrypt
  "\C-c\C-mss"          mh-mml-secure-message-sign
  "\C-c\C-mt"           mh-mh-compose-external-compressed-tar
  "\C-c\C-mu"           mh-mh-to-mime-undo
  "\C-c\C-mx"           mh-mh-compose-external-type
  "\C-c\C-o"            mh-open-line
  "\C-c\C-q"            mh-fully-kill-draft
  "\C-c\C-s"            mh-insert-signature
  "\C-c\C-t"            mh-letter-toggle-header-field-display
  "\C-c\C-w"            mh-check-whom
  "\C-c\C-y"            mh-yank-cur-msg
  "\C-c\M-d"            mh-insert-auto-fields
  "\M-\t"               mh-letter-complete
  "\t"                  mh-letter-next-header-field-or-indent
  [backtab]             mh-letter-previous-header-field)

;; "C-c /" prefix is used in mh-letter-mode by pgp.el and mailcrypt.el.



;;; MH-Letter Help Messages

;; Group messages logically, more or less.
(defvar mh-letter-mode-help-messages
  '((nil
     "Send letter: \\[mh-send-letter]    "
     "Open line:        \\[mh-open-line]\n"
     "Kill letter: \\[mh-fully-kill-draft]    "
     "Check recipients: \\[mh-check-whom]\n\n"
     "Insert:\n"
     "  Current message:      \\[mh-yank-cur-msg]\n"
     "  Attachment:           \\[mh-compose-insertion]\n"
     "  Message to forward:   \\[mh-compose-forward]\n"
     "  Signature:            \\[mh-insert-signature]\n\n"
     "Security:\n"
     "  Encrypt message:      \\[mh-mml-secure-message-encrypt]\n"
     "  Sign message:         \\[mh-mml-secure-message-sign]\n"
     "  Sign+Encrypt message: \\[mh-mml-secure-message-signencrypt]"))
  "Key binding cheat sheet.

This is an associative array which is used to show the most
common commands. The key is a prefix char. The value is one or
more strings which are concatenated together and displayed in the
minibuffer if ? is pressed after the prefix character. The
special key nil is used to display the non-prefixed commands.

The substitutions described in `substitute-command-keys' are
performed as well.")



;;; MH-Letter Font Lock

(defvar mh-letter-font-lock-keywords
  `(,@(mh-show-font-lock-keywords-with-cite)
    (mh-font-lock-field-data
     (1 'mh-letter-header-field prepend t)))
  "Additional expressions to highlight in MH-Letter buffers.")

(defun mh-font-lock-field-data (limit)
  "Find header field region between point and LIMIT."
  (and (< (point) (mh-letter-header-end))
       (< (point) limit)
       (let ((end (min limit (mh-letter-header-end)))
             (point (point))
             data-end data-begin field)
         (end-of-line)
         (setq data-end (if (re-search-forward "^[^ \t]" end t)
                            (match-beginning 0)
                          end))
         (goto-char (1- data-end))
         (if (not (re-search-backward "\\(^[^ \t][^:]*\\):[ \t]*" nil t))
             (setq data-begin (point-min))
           (setq data-begin (match-end 0))
           (setq field (match-string 1)))
         (setq data-begin (max point data-begin))
         (goto-char (if (equal point data-end) (1+ data-end) data-end))
         (cond ((and field (mh-letter-skipped-header-field-p field))
                (set-match-data nil)
                nil)
               (t (set-match-data
                   (list data-begin data-end data-begin data-end))
                  t)))))

(defun mh-letter-header-end ()
  "Find the end of the message header.
This function is to be used only for font locking. It works by
searching for `mh-mail-header-separator' in the buffer."
  (save-excursion
    (goto-char (point-min))
    (cond ((equal mh-mail-header-separator "") (point-min))
          ((search-forward (format "\n%s\n" mh-mail-header-separator) nil t)
           (mh-line-beginning-position 0))
          (t (point-min)))))



;;; MH-Letter Mode

;; Shush compiler.
(mh-do-in-xemacs
  (defvar font-lock-defaults))

;; Ensure new buffers won't get this mode if default major-mode is nil.
(put 'mh-letter-mode 'mode-class 'special)

;;;###mh-autoload
(define-derived-mode mh-letter-mode mail-mode "MH-Letter"
  "Mode for composing letters in MH-E\\<mh-letter-mode-map>.

When you have finished composing, type \\[mh-send-letter] to send
the message using the MH mail handling system.

There are two types of tags used by MH-E when composing MIME
messages: MML and MH. The option `mh-compose-insertion' controls
what type of tags are inserted by MH-E commands. These tags can
be converted to MIME body parts by running \\[mh-mh-to-mime] for
MH-style directives or \\[mh-mml-to-mime] for MML tags.

Options that control this mode can be changed with
\\[customize-group]; specify the \"mh-compose\" group.

When a message is composed, the hooks `text-mode-hook',
`mail-mode-hook', and `mh-letter-mode-hook' are run (in that
order).

\\{mh-letter-mode-map}"
  (mh-find-path)
  (make-local-variable 'mh-send-args)
  (make-local-variable 'mh-annotate-char)
  (make-local-variable 'mh-annotate-field)
  (make-local-variable 'mh-previous-window-config)
  (make-local-variable 'mh-sent-from-folder)
  (make-local-variable 'mh-sent-from-msg)
  (mh-do-in-gnu-emacs
    (unless mh-letter-tool-bar-map
      (mh-tool-bar-letter-buttons-init))
    (if (boundp 'tool-bar-map)
        (set (make-local-variable 'tool-bar-map) mh-letter-tool-bar-map)))
  (mh-do-in-xemacs
    (mh-tool-bar-init :letter))
  ;; Set the local value of mh-mail-header-separator according to what is
  ;; present in the buffer...
  (set (make-local-variable 'mh-mail-header-separator)
       (save-excursion
         (goto-char (mh-mail-header-end))
         (buffer-substring-no-properties (point) (mh-line-end-position))))
  (make-local-variable 'mail-header-separator)
  (setq mail-header-separator mh-mail-header-separator) ;override sendmail.el
  (mh-set-help mh-letter-mode-help-messages)
  (setq buffer-invisibility-spec '((vanish . t) t))
  (set (make-local-variable 'line-move-ignore-invisible) t)

  ;; Enable undo since a show-mode buffer might have been reused.
  (buffer-enable-undo)
  (make-local-variable 'font-lock-defaults)
  (cond
   ((or (equal mh-highlight-citation-style 'font-lock)
        (equal mh-highlight-citation-style 'gnus))
    ;; Let's use font-lock even if gnus is used in show-mode.  The reason
    ;; is that gnus uses static text properties which are not appropriate
    ;; for a buffer that will be edited.  So the choice here is either fontify
    ;; the citations and header...
    (setq font-lock-defaults '(mh-letter-font-lock-keywords t)))
   (t
    ;; ...or the header only
    (setq font-lock-defaults '((mh-show-font-lock-keywords) t))))
  (easy-menu-add mh-letter-menu)
  ;; Maybe we want to use the existing Mail menu from mail-mode in
  ;; 9.0; in the mean time, let's remove it since the redundancy will
  ;; only produce confusion.
  (define-key mh-letter-mode-map [menu-bar mail] 'undefined)
  (mh-do-in-xemacs (easy-menu-remove mail-menubar-menu))
  (setq fill-column mh-letter-fill-column)
  (add-hook 'completion-at-point-functions
            'mh-letter-completion-at-point nil 'local)
  ;; If text-mode-hook turned on auto-fill, tune it for messages
  (when auto-fill-function
    (make-local-variable 'auto-fill-function)
    (setq auto-fill-function 'mh-auto-fill-for-letter)))



;;; MH-Letter Commands

;; Alphabetical.
;; See also mh-comp.el and mh-mime.el.

(defun mh-check-whom ()
  "Verify recipients, showing expansion of any aliases.

This command expands aliases so you can check the actual address(es)
in the alias. A new buffer named \"*MH-E Recipients*\" is created with
the output of \"whom\"."
  (interactive)
  (let ((file-name buffer-file-name))
    (save-buffer)
    (message "Checking recipients...")
    (mh-in-show-buffer (mh-recipients-buffer)
      (bury-buffer (current-buffer))
      (erase-buffer)
      (mh-exec-cmd-output "whom" t file-name))
    (message "Checking recipients...done")))

(defun mh-insert-letter (folder message verbatim)
  "Insert a message.

This command prompts you for the FOLDER and MESSAGE number, which
defaults to the current message in that folder. It then inserts
the message, indented by `mh-ins-buf-prefix' (\"> \") unless
`mh-yank-behavior' is set to one of the supercite flavors in
which case supercite is used to format the message. Certain
undesirable header fields (see
`mh-invisible-header-fields-compiled') are removed before
insertion.

If given a prefix argument VERBATIM, the header is left intact, the
message is not indented, and \"> \" is not inserted before each line.
This command leaves the mark before the letter and point after it."
  (interactive
   (let* ((folder
           (mh-prompt-for-folder "Message from" mh-sent-from-folder nil))
          (default
            (if (equal folder mh-sent-from-folder)
                (or mh-sent-from-msg (nth 0 (mh-translate-range folder "cur")))
              (nth 0 (mh-translate-range folder "cur"))))
          (message
           (read-string (concat "Message number"
                                (or (and default
                                         (format " (default %d): " default))
                                    ": "))
                        nil nil
                        (if (numberp default)
                            (int-to-string default)
                          default))))
     (list folder message current-prefix-arg)))
  (if (equal message "")
      (error "No message number given"))
  (save-restriction
    (narrow-to-region (point) (point))
    (let ((start (point-min)))
      (insert-file-contents
       (expand-file-name message (mh-expand-file-name folder)))
      (when (not verbatim)
        (mh-clean-msg-header start mh-invisible-header-fields-compiled nil)
        (goto-char (point-max))         ;Needed for sc-cite-original
        (push-mark)                     ;Needed for sc-cite-original
        (goto-char (point-min))         ;Needed for sc-cite-original
        (mh-insert-prefix-string mh-ins-buf-prefix)))))

;;;###mh-autoload
(defun mh-insert-signature (&optional file)
  "Insert signature in message.

This command inserts your signature at the current cursor location.

By default, the text of your signature is taken from the file
\"~/.signature\". You can read from other sources by changing the
option `mh-signature-file-name'.

A signature separator (\"-- \") will be added if the signature block
does not contain one and `mh-signature-separator-flag' is on.

The hook `mh-insert-signature-hook' is run after the signature is
inserted. Hook functions may access the actual name of the file or the
function used to insert the signature with `mh-signature-file-name'.

The signature can also be inserted using Identities (see
`mh-identity-list').

In a program, you can pass in a signature FILE."
  (interactive)
  (save-excursion
    (insert "\n")
    (let ((mh-signature-file-name (or file mh-signature-file-name))
          (mh-mh-p (mh-mh-directive-present-p))
          (mh-mml-p (mh-mml-tag-present-p)))
      (save-restriction
        (narrow-to-region (point) (point))
        (cond
         ((mh-file-is-vcard-p mh-signature-file-name)
          (if (equal mh-compose-insertion 'mml)
              (insert "<#part type=\"text/x-vcard\" filename=\""
                      mh-signature-file-name
                      "\" disposition=inline description=VCard>\n<#/part>")
            (insert "#text/x-vcard; name=\""
                    (file-name-nondirectory mh-signature-file-name)
                    "\" [VCard] " (expand-file-name mh-signature-file-name))))
         (t
          (cond
           (mh-mh-p
            (insert "#\n" "Content-Description: Signature\n"))
           (mh-mml-p
            (mml-insert-tag 'part 'type "text/plain" 'disposition "inline"
                            'description "Signature")))
          (cond ((null mh-signature-file-name))
                ((and (stringp mh-signature-file-name)
                      (file-readable-p mh-signature-file-name))
                 (insert-file-contents mh-signature-file-name))
                ((functionp mh-signature-file-name)
                 (funcall mh-signature-file-name)))))
        (save-restriction
          (widen)
          (run-hooks 'mh-insert-signature-hook))
        (goto-char (point-min))
        (when (and (not (mh-file-is-vcard-p mh-signature-file-name))
                   mh-signature-separator-flag
                   (> (point-max) (point-min))
                   (not (mh-signature-separator-p)))
          (cond (mh-mh-p
                 (forward-line 2))
                (mh-mml-p
                 (forward-line 1)))
          (insert mh-signature-separator))
        (if (not (> (point-max) (point-min)))
            (message "No signature found")))))
  (force-mode-line-update))

(defun mh-letter-completion-at-point ()
  "Return the completion data at point for MH letters.
This provides alias and folder completion in header fields according to
`mh-letter-complete-function-alist' and falls back on
`mh-letter-complete-function-alist' elsewhere."
  (let ((func (and (mh-in-header-p)
                   (cdr (assoc (mh-letter-header-field-at-point)
                               mh-letter-complete-function-alist)))))
    (if func
        (or (funcall func) #'ignore)
      mh-letter-complete-function)))

;; TODO Now that completion-at-point performs the task of
;; mh-letter-complete, perhaps mh-letter-complete along with
;; mh-complete-word should be rewritten as a more general function for
;; XEmacs, renamed to mh-completion-at-point, and moved to
;; mh-compat.el.
(defun-mh mh-letter-complete completion-at-point ()
  "Perform completion on header field or word preceding point.

If the field contains addresses (for example, \"To:\" or \"Cc:\")
or folders (for example, \"Fcc:\") then this command will provide
alias completion. In the body of the message, this command runs
`mh-letter-complete-function' instead, which is set to
`ispell-complete-word' by default."
      (interactive)
      (let ((data (mh-letter-completion-at-point)))
        (cond
         ((functionp data) (funcall data))
         ((consp data)
          (let ((start (nth 0 data))
                (end (nth 1 data))
                (table (nth 2 data)))
            (mh-complete-word (buffer-substring-no-properties start end)
                              table start end))))))

(defun mh-letter-complete-or-space (arg)
  "Perform completion or insert space.

Turn on the option `mh-compose-space-does-completion-flag' to use
this command to perform completion in the header. Otherwise, a
space is inserted; use a prefix argument ARG to specify more than
one space."
  (interactive "p")
  (let ((end-of-prev (save-excursion
                       (goto-char (mh-beginning-of-word))
                       (mh-beginning-of-word -1))))
    (cond ((not mh-compose-space-does-completion-flag)
           (self-insert-command arg))
          ;; FIXME: This > test is redundant now that all the completion
          ;; functions do it anyway.
          ((> (point) end-of-prev) (self-insert-command arg))
          ((let ((mh-letter-complete-function nil))
             (mh-letter-completion-at-point))
           (mh-letter-complete))
          (t (self-insert-command arg)))))

(defun mh-letter-confirm-address ()
  "Flash alias expansion.

Addresses are separated by a comma\; when you press the comma,
this command flashes the alias expansion in the minibuffer if
`mh-alias-flash-on-comma' is turned on."
  (interactive)
  (cond ((not (mh-in-header-p)) (self-insert-command 1))
        ((eq (cdr (assoc (mh-letter-header-field-at-point)
                         mh-letter-complete-function-alist))
             'mh-alias-letter-expand-alias)
         (mh-alias-reload-maybe)
         (mh-alias-minibuffer-confirm-address))
        (t (self-insert-command 1))))

(defun mh-letter-next-header-field-or-indent (arg)
  "Cycle to next field.

Within the header of the message, this command moves between
fields that are highlighted with the face
`mh-letter-header-field', skipping those fields listed in
`mh-compose-skipped-header-fields'. After the last field, this
command then moves point to the message body before cycling back
to the first field. If point is already past the first line of
the message body, then this command indents by calling
`indent-relative' with the given prefix argument ARG."
  (interactive "P")
  (let ((header-end (save-excursion
                      (goto-char (mh-mail-header-end))
                      (forward-line)
                      (point))))
    (if (> (point) header-end)
        (indent-relative arg)
      (mh-letter-next-header-field))))

(defun mh-letter-previous-header-field ()
  "Cycle to the previous header field.

This command moves backwards between the fields and cycles to the
body of the message after the first field. Unlike the command
\\[mh-letter-next-header-field-or-indent], it will always take
point to the last field from anywhere in the body."
  (interactive)
  (let ((header-end (mh-mail-header-end)))
    (if (>= (point) header-end)
        (goto-char header-end)
      (mh-header-field-beginning))
    (cond ((re-search-backward mh-letter-header-field-regexp nil t)
           (if (mh-letter-skipped-header-field-p (match-string 1))
               (mh-letter-previous-header-field)
           (goto-char (match-end 0))
           (mh-letter-skip-leading-whitespace-in-header-field)))
          (t (goto-char header-end)
             (forward-line)))))

(defun mh-open-line ()
  "Insert a newline and leave point before it.

This command is similar to the command \\[open-line] in that it
inserts a newline after point. It differs in that it also inserts
the right number of quoting characters and spaces so that the
next line begins in the same column as it was. This is useful
when breaking up paragraphs in replies."
  (interactive)
  (let ((column (current-column))
        (prefix (mh-current-fill-prefix)))
    (if (> (length prefix) column)
        (message "Sorry, point seems to be within the line prefix")
      (newline 2)
      (insert prefix)
      (while (> column (current-column))
        (insert " "))
      (forward-line -1))))

(defun mh-to-fcc (&optional folder)
  "Move to \"Fcc:\" header field.

This command will prompt you for the FOLDER name in which to file
a copy of the draft."
  (interactive (list (mh-prompt-for-folder
                      "Fcc"
                      (or (and mh-default-folder-for-message-function
                               (save-excursion
                                 (goto-char (point-min))
                                 (funcall
                                  mh-default-folder-for-message-function)))
                          "")
                      t)))
  (let ((last-input-event ?\C-f))
    (expand-abbrev)
    (save-excursion
      (mh-to-field)
      (insert (if (mh-folder-name-p folder)
                  (substring folder 1)
                folder)))))

(defvar mh-to-field-choices '(("a" . "Mail-Reply-To:")
                              ("b" . "Bcc:")
                              ("c" . "Cc:")
                              ("d" . "Dcc:")
                              ("f" . "Fcc:")
                              ("l" . "Mail-Followup-To:")
                              ("m" . "From:")
                              ("r" . "Reply-To:")
                              ("s" . "Subject:")
                              ("t" . "To:"))
  "Alist of (final-character . field-name) choices for `mh-to-field'.")

(defun mh-to-field ()
  "Move to specified header field.

The field is indicated by the previous keystroke (the last
keystroke of the command) according to the list in the variable
`mh-to-field-choices'.
Create the field if it does not exist.
Set the mark to point before moving."
  (interactive)
  (expand-abbrev)
  (let ((target (cdr (or (assoc (char-to-string (logior last-input-event ?`))
                                mh-to-field-choices)
                         ;; also look for a char for version 4 compat
                         (assoc (logior last-input-event ?`)
                                mh-to-field-choices))))
        (case-fold-search t))
    (push-mark)
    (cond ((mh-position-on-field target)
           (let ((eol (point)))
             (skip-chars-backward " \t")
             (delete-region (point) eol))
           (if (and (not (eq (logior last-input-event ?`) ?s))
                    (save-excursion
                      (backward-char 1)
                      (not (looking-at "[:,]"))))
               (insert ", ")
             (insert " ")))
          (t
           (if (mh-position-on-field "To:")
               (forward-line 1))
           (insert (format "%s \n" target))
           (backward-char 1)))))

;;;###mh-autoload
(defun mh-yank-cur-msg ()
  "Insert the current message into the draft buffer.

It is often useful to insert a snippet of text from a letter that
someone mailed to provide some context for your reply. This
command does this by adding an attribution, yanking a portion of
text from the message to which you're replying, and inserting
`mh-ins-buf-prefix' (`> ') before each line.

The attribution consists of the sender's name and email address
followed by the content of the option
`mh-extract-from-attribution-verb'.

You can also turn on the option
`mh-delete-yanked-msg-window-flag' to delete the window
containing the original message after yanking it to make more
room on your screen for your reply.

You can control how the message to which you are replying is
yanked into your reply using `mh-yank-behavior'.

If this isn't enough, you can gain full control over the
appearance of the included text by setting `mail-citation-hook'
to a function that modifies it. For example, if you set this hook
to `trivial-cite' (which is NOT part of Emacs), set
`mh-yank-behavior' to \"Body and Header\" (see URL
`http://shasta.cs.uiuc.edu/~lrclause/tc.html').

Note that if `mail-citation-hook' is set, `mh-ins-buf-prefix' is
not inserted. If the option `mh-yank-behavior' is set to one of
the supercite flavors, the hook `mail-citation-hook' is ignored
and `mh-ins-buf-prefix' is not inserted."
  (interactive)
  (if (and mh-sent-from-folder
           (with-current-buffer mh-sent-from-folder mh-show-buffer)
           (with-current-buffer mh-sent-from-folder
             (get-buffer mh-show-buffer))
           mh-sent-from-msg)
      (let ((to-point (point))
            (to-buffer (current-buffer)))
        (set-buffer mh-sent-from-folder)
        (if mh-delete-yanked-msg-window-flag
            (delete-windows-on mh-show-buffer))
        (set-buffer mh-show-buffer)     ; Find displayed message
        (let* ((from-attr (mh-extract-from-attribution))
               (yank-region (mh-mark-active-p nil))
               (mh-ins-str
                (cond ((and yank-region
                            (or (eq 'supercite mh-yank-behavior)
                                (eq 'autosupercite mh-yank-behavior)
                                (eq t mh-yank-behavior)))
                       ;; supercite needs the full header
                       (concat
                        (buffer-substring (point-min) (mh-mail-header-end))
                        "\n"
                        (buffer-substring (region-beginning) (region-end))))
                      (yank-region
                       (buffer-substring (region-beginning) (region-end)))
                      ((or (eq 'body mh-yank-behavior)
                           (eq 'attribution mh-yank-behavior)
                           (eq 'autoattrib mh-yank-behavior))
                       (buffer-substring
                        (save-excursion
                          (goto-char (point-min))
                          (mh-goto-header-end 1)
                          (point))
                        (point-max)))
                      ((or (eq 'supercite mh-yank-behavior)
                           (eq 'autosupercite mh-yank-behavior)
                           (eq t mh-yank-behavior))
                       (buffer-substring (point-min) (point-max)))
                      (t
                       (buffer-substring (point) (point-max))))))
          (set-buffer to-buffer)
          (save-restriction
            (narrow-to-region to-point to-point)
            (insert (mh-filter-out-non-text mh-ins-str))
            (goto-char (point-max))     ;Needed for sc-cite-original
            (push-mark)                 ;Needed for sc-cite-original
            (goto-char (point-min))     ;Needed for sc-cite-original
            (mh-insert-prefix-string mh-ins-buf-prefix)
            (when (or (eq 'attribution mh-yank-behavior)
                      (eq 'autoattrib mh-yank-behavior))
              (insert from-attr)
              (mh-identity-insert-attribution-verb nil)
              (insert "\n\n"))
            ;; If the user has selected a region, he has already "edited" the
            ;; text, so leave the cursor at the end of the yanked text. In
            ;; either case, leave a mark at the opposite end of the included
            ;; text to make it easy to jump or delete to the other end of the
            ;; text.
            (push-mark)
            (goto-char (point-max))
            (if (null yank-region)
                (mh-exchange-point-and-mark-preserving-active-mark)))))
    (error "There is no current message")))



;;; Support Routines

(defun mh-auto-fill-for-letter ()
  "Perform auto-fill for message.
Header is treated specially by inserting a tab before continuation
lines."
  (if (mh-in-header-p)
      (let ((fill-prefix "\t"))
        (do-auto-fill))
    (do-auto-fill)))

(defun mh-filter-out-non-text (string)
  "Return STRING but without adornments such as MIME buttons and smileys."
  (with-temp-buffer
    ;; Insert the string to filter
    (insert string)
    (goto-char (point-min))

    ;; Remove the MIME buttons
    (let ((can-move-forward t)
          (in-button nil))
      (while can-move-forward
        (cond ((and (not (get-text-property (point) 'mh-data))
                    in-button)
               (delete-region (1- (point)) (point))
               (setq in-button nil))
              ((get-text-property (point) 'mh-data)
               (delete-region (point)
                              (save-excursion (forward-line) (point)))
               (setq in-button t))
              (t (setq can-move-forward (= (forward-line) 0))))))

    ;; Return the contents without properties... This gets rid of emphasis
    ;; and smileys
    (buffer-substring-no-properties (point-min) (point-max))))

(defun mh-current-fill-prefix ()
  "Return the `fill-prefix' on the current line as a string."
  (save-excursion
    (beginning-of-line)
    ;; This assumes that the major-mode sets up adaptive-fill-regexp
    ;; correctly such as mh-letter-mode or sendmail.el's mail-mode.  But
    ;; perhaps I should use the variable and simply inserts its value here,
    ;; and set it locally in a let scope.  --psg
    (if (re-search-forward adaptive-fill-regexp nil t)
        (match-string 0)
      "")))

;;;###mh-autoload
(defun mh-letter-next-header-field ()
  "Cycle to the next header field.
If we are at the last header field go to the start of the message
body."
  (let ((header-end (mh-mail-header-end)))
    (cond ((>= (point) header-end) (goto-char (point-min)))
          ((< (point) (progn
                        (beginning-of-line)
                        (re-search-forward mh-letter-header-field-regexp
                                           (mh-line-end-position) t)
                        (point)))
           (beginning-of-line))
          (t (end-of-line)))
    (cond ((re-search-forward mh-letter-header-field-regexp header-end t)
           (if (mh-letter-skipped-header-field-p (match-string 1))
               (mh-letter-next-header-field)
             (mh-letter-skip-leading-whitespace-in-header-field)))
          (t (goto-char header-end)
             (forward-line)))))

;;;###mh-autoload
(defun mh-position-on-field (field &optional ignored)
  "Move to the end of the FIELD in the header.
Move to end of entire header if FIELD not found.
Returns non-nil if FIELD was found.
The optional second arg is for pre-version 4 compatibility and is
IGNORED."
  (cond ((mh-goto-header-field field)
         (mh-header-field-end)
         t)
        ((mh-goto-header-end 0)
         nil)))

(defun mh-letter-header-field-at-point ()
  "Return the header field name at point.
A symbol is returned whose name is the string obtained by
downcasing the field name."
  (save-excursion
    (end-of-line)
    (and (re-search-backward mh-letter-header-field-regexp nil t)
         (intern (downcase (match-string 1))))))

(defun mh-folder-expand-at-point ()
  "Do folder name completion in Fcc header field."
  (let* ((beg (mh-beginning-of-word))
         (end (save-excursion
                (goto-char beg)
                (mh-beginning-of-word -1))))
    (when (>= end (point))
      (list beg (if (fboundp 'completion-at-point) end (point))
            #'mh-folder-completion-function))))

;;;###mh-autoload
(defun mh-complete-word (word choices begin end)
  "Complete WORD from CHOICES.
Any match found replaces the text from BEGIN to END."
  (let ((completion (try-completion word choices))
        (completions-buffer "*Completions*"))
    (cond ((eq completion t)
           (ignore-errors
             (kill-buffer completions-buffer))
           (message "Completed: %s" word))
          ((null completion)
           (ignore-errors
             (kill-buffer completions-buffer))
           (message "No completion for %s" word))
          ((stringp completion)
           (if (equal word completion)
               (with-output-to-temp-buffer completions-buffer
                 (mh-display-completion-list
                  (all-completions word choices)
                  ;; The `common-substring' arg only works if it's a prefix.
                  (unless (and (functionp choices)
                               (let ((bounds
                                      (funcall choices
                                               word nil '(boundaries . ""))))
                                 (and (eq 'boundaries (car-safe bounds))
                                      (< 0 (cadr bounds)))))
                    word)))
             (ignore-errors
               (kill-buffer completions-buffer))
             (delete-region begin end)
             (insert completion))))))

(defun mh-file-is-vcard-p (file)
  "Return t if FILE is a .vcf vcard."
  (let ((case-fold-search t))
    (and (stringp file)
         (file-exists-p file)
         (or (and (not (mh-have-file-command))
                  (not (null (string-match "\.vcf$" file))))
             (string-equal "text/x-vcard" (mh-file-mime-type file))))))

;;;###mh-autoload
(defun mh-letter-toggle-header-field-display-button (event)
  "Toggle header field display at location of EVENT.
This function does the same thing as
`mh-letter-toggle-header-field-display' except that it is
callable from a mouse button."
  (interactive "e")
  (mh-do-at-event-location event
    (mh-letter-toggle-header-field-display nil)))

(defun mh-extract-from-attribution ()
  "Extract phrase or comment from From header field."
  (save-excursion
    (if (not (mh-goto-header-field "From: "))
        nil
      (skip-chars-forward " ")
      (cond
       ((looking-at "\"\\([^\"\n]+\\)\" \\(<.+>\\)")
        (format "%s %s " (match-string 1)(match-string 2)))
       ((looking-at "\\([^<\n]+<.+>\\)$")
        (format "%s " (match-string 1)))
       ((looking-at "\\([^ ]+@[^ ]+\\) +(\\(.+\\))$")
        (format "%s <%s> " (match-string 2)(match-string 1)))
       ((looking-at " *\\(.+\\)$")
        (format "%s " (match-string 1)))))))

(defun mh-insert-prefix-string (mh-ins-string)
  "Insert prefix string before each line in buffer.
The inserted letter is cited using `sc-cite-original' if
`mh-yank-behavior' is one of 'supercite or 'autosupercite.
Otherwise, simply insert MH-INS-STRING before each line."
  (goto-char (point-min))
  (cond ((or (eq mh-yank-behavior 'supercite)
             (eq mh-yank-behavior 'autosupercite))
         (sc-cite-original))
        (mail-citation-hook
         (run-hooks 'mail-citation-hook))
        (mh-yank-hooks                  ;old hook name
         (run-hooks 'mh-yank-hooks))
        (t
         (or (bolp) (forward-line 1))
         (while (< (point) (point-max))
           (insert mh-ins-string)
           (forward-line 1))
         (goto-char (point-min)))))     ;leave point like sc-cite-original

(provide 'mh-letter)

;; Local Variables:
;; indent-tabs-mode: nil
;; sentence-end-double-space: nil
;; End:

;;; mh-letter.el ends here
