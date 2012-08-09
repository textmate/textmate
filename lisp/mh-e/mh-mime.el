;;; mh-mime.el --- MH-E MIME support

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

;; Message composition of MIME message is done with either MH-style
;; directives for mhn or mhbuild (MH 6.8 or later) or MML (MIME Meta
;; Language) tags.

;; TODO:
;;   Paragraph code should not fill # lines if MIME enabled.
;;   Implement mh-auto-mh-to-mime (if non-nil, \\[mh-send-letter]
;;      invokes mh-mh-to-mime automatically before sending.)
;;      Actually, instead of mh-auto-mh-to-mime,
;;      should read automhnproc from profile.
;;   MIME option to mh-forward command to move to content-description
;;   insertion point.

;;; Change Log:

;;; Code:

(require 'mh-e)
(require 'mh-gnus)                      ;needed because mh-gnus.el not compiled

(require 'font-lock)
(require 'gnus-util)
(require 'mailcap)
(require 'mm-decode)
(require 'mm-view)
(require 'mml)

(autoload 'article-emphasize "gnus-art")
(autoload 'gnus-eval-format "gnus-spec")
(autoload 'mail-content-type-get "mail-parse")
(autoload 'mail-decode-encoded-word-string "mail-parse")
(autoload 'mail-header-parse-content-type "mail-parse")
(autoload 'mail-header-strip "mail-parse")
(autoload 'mail-strip-quoted-names "mail-utils")
(autoload 'message-options-get "message")
(autoload 'message-options-set "message")
(autoload 'message-options-set-recipient "message")
(autoload 'mm-decode-body "mm-bodies")
(autoload 'mm-uu-dissect "mm-uu")
(autoload 'mml-unsecure-message "mml-sec")
(autoload 'rfc2047-decode-region "rfc2047")
(autoload 'widget-convert-button "wid-edit")



;;; Variables

;; This has to be a macro, since we do: (setf (mh-buffer-data) ...)
;;;###mh-autoload
(defmacro mh-buffer-data ()
  "Convenience macro to get the MIME data structures of the current buffer."
  `(gethash (current-buffer) mh-globals-hash))

;; Structure to keep track of MIME handles on a per buffer basis.
(mh-defstruct (mh-buffer-data (:conc-name mh-mime-)
                              (:constructor mh-make-buffer-data))
  (handles ())                          ; List of MIME handles
  (handles-cache (make-hash-table))     ; Cache to avoid multiple decodes of
                                        ; nested messages
  (parts-count 0)                       ; The button number is generated from
                                        ; this number
  (part-index-hash (make-hash-table)))  ; Avoid incrementing the part number
                                        ; for nested messages

(defvar mh-mm-inline-media-tests
  `(("image/jpeg"
     mm-inline-image
     (lambda (handle)
       (mm-valid-and-fit-image-p 'jpeg handle)))
    ("image/png"
     mm-inline-image
     (lambda (handle)
       (mm-valid-and-fit-image-p 'png handle)))
    ("image/gif"
     mm-inline-image
     (lambda (handle)
       (mm-valid-and-fit-image-p 'gif handle)))
    ("image/tiff"
     mm-inline-image
     (lambda (handle)
       (mm-valid-and-fit-image-p 'tiff handle)) )
    ("image/xbm"
     mm-inline-image
     (lambda (handle)
       (mm-valid-and-fit-image-p 'xbm handle)))
    ("image/x-xbitmap"
     mm-inline-image
     (lambda (handle)
       (mm-valid-and-fit-image-p 'xbm handle)))
    ("image/xpm"
     mm-inline-image
     (lambda (handle)
       (mm-valid-and-fit-image-p 'xpm handle)))
    ("image/x-pixmap"
     mm-inline-image
     (lambda (handle)
       (mm-valid-and-fit-image-p 'xpm handle)))
    ("image/bmp"
     mm-inline-image
     (lambda (handle)
       (mm-valid-and-fit-image-p 'bmp handle)))
    ("image/x-portable-bitmap"
     mm-inline-image
     (lambda (handle)
       (mm-valid-and-fit-image-p 'pbm handle)))
    ("text/plain" mm-inline-text identity)
    ("text/enriched" mm-inline-text identity)
    ("text/richtext" mm-inline-text identity)
    ("text/x-patch" mm-display-patch-inline
     (lambda (handle)
       (locate-library "diff-mode")))
    ("application/emacs-lisp" mm-display-elisp-inline identity)
    ("application/x-emacs-lisp" mm-display-elisp-inline identity)
    ("text/html"
     ,(if (fboundp 'mm-inline-text-html) 'mm-inline-text-html 'mm-inline-text)
     (lambda (handle)
       (or (and (boundp 'mm-inline-text-html-renderer)
                mm-inline-text-html-renderer)
           (and (boundp 'mm-text-html-renderer) mm-text-html-renderer))))
    ("text/x-vcard"
     mh-mm-inline-text-vcard
     (lambda (handle)
       (or (featurep 'vcard)
           (locate-library "vcard"))))
    ("message/delivery-status" mm-inline-text identity)
    ("message/rfc822" mh-mm-inline-message identity)
    ;;("message/partial" mm-inline-partial identity)
    ;;("message/external-body" mm-inline-external-body identity)
    ("text/.*" mm-inline-text identity)
    ("audio/wav" mm-inline-audio
     (lambda (handle)
       (and (or (featurep 'nas-sound) (featurep 'native-sound))
            (device-sound-enabled-p))))
    ("audio/au"
     mm-inline-audio
     (lambda (handle)
       (and (or (featurep 'nas-sound) (featurep 'native-sound))
            (device-sound-enabled-p))))
    ("application/pgp-signature" ignore identity)
    ("application/x-pkcs7-signature" ignore identity)
    ("application/pkcs7-signature" ignore identity)
    ("application/x-pkcs7-mime" ignore identity)
    ("application/pkcs7-mime" ignore identity)
    ("multipart/alternative" ignore identity)
    ("multipart/mixed" ignore identity)
    ("multipart/related" ignore identity)
    ;; Disable audio and image
    ("audio/.*" ignore ignore)
    ("image/.*" ignore ignore)
    ;; Default to displaying as text
    (".*" mm-inline-text mh-mm-readable-p))
  "Alist of media types/tests saying whether types can be displayed inline.")

(defvar mh-mime-save-parts-directory nil
  "Default to use for `mh-mime-save-parts-default-directory'.
Set from last use.")

;; Copied from gnus-art.el (should be checked for other cool things that can
;; be added to the buttons)
(defvar mh-mime-button-commands
  '((mh-press-button "\r" "Toggle Display")))
(defvar mh-mime-button-map
  (let ((map (make-sparse-keymap)))
    (unless (>= (string-to-number emacs-version) 21)
      ;; XEmacs doesn't care.
      (set-keymap-parent map mh-show-mode-map))
    (mh-do-in-gnu-emacs
     (define-key map [mouse-2] 'mh-push-button))
    (mh-do-in-xemacs
     (define-key map '(button2) 'mh-push-button))
    (dolist (c mh-mime-button-commands)
      (define-key map (cadr c) (car c)))
    map))
(defvar mh-mime-button-line-format-alist
  '((?T long-type ?s)
    (?d description ?s)
    (?p index ?s)
    (?e dots ?s)))
(defvar mh-mime-button-line-format "%{%([%p. %d%T]%)%}%e\n")
(defvar mh-mime-security-button-pressed nil)
(defvar mh-mime-security-button-line-format "%{%([[%t:%i]%D]%)%}\n")
(defvar mh-mime-security-button-end-line-format "%{%([[End of %t]%D]%)%}\n")
(defvar mh-mime-security-button-line-format-alist
  '((?t type ?s)
    (?i info ?s)
    (?d details ?s)
    (?D pressed-details ?s)))
(defvar mh-mime-security-button-map
  (let ((map (make-sparse-keymap)))
    (unless (>= (string-to-number emacs-version) 21)
      (set-keymap-parent map mh-show-mode-map))
    (define-key map "\r" 'mh-press-button)
    (mh-do-in-gnu-emacs
     (define-key map [mouse-2] 'mh-push-button))
    (mh-do-in-xemacs
     (define-key map '(button2) 'mh-push-button))
    map))



;;; MH-Folder Commands

;; Alphabetical.

;;;###mh-autoload
(defun mh-display-with-external-viewer (part-index)
  "View attachment externally.

If Emacs does not know how to view an attachment, you could save
it into a file and then run some program to open it. It is
easier, however, to launch the program directly from MH-E with
this command. While you'll most likely use this to view
spreadsheets and documents, it is also useful to use your browser
to view HTML attachments with higher fidelity than what Emacs can
provide.

This command displays the attachment associated with the button
under the cursor. If the cursor is not located over a button,
then the cursor first moves to the next button, wrapping to the
beginning of the message if necessary. You can provide a numeric
prefix argument PART-INDEX to view the attachment labeled with
that number.

This command tries to provide a reasonable default for the viewer
by calling the Emacs function `mailcap-mime-info'. This function
usually reads the file \"/etc/mailcap\"."
  (interactive "P")
  (when (consp part-index) (setq part-index (car part-index)))
  (mh-folder-mime-action
   part-index
   #'(lambda ()
       (let* ((part (get-text-property (point) 'mh-data))
              (type (mm-handle-media-type part))
              (methods (mapcar (lambda (x) (list (cdr (assoc 'viewer x))))
                               (mailcap-mime-info type 'all)))
              (def (caar methods))
              (prompt (format "Viewer%s: " (if def
                                               (format " (default %s)" def)
                                             "")))
              (method (completing-read prompt methods nil nil nil nil def))
              (folder mh-show-folder-buffer)
              (buffer-read-only nil))
         (when (string-match "^[^% \t]+$" method)
           (setq method (concat method " %s")))
         (flet ((mm-handle-set-external-undisplayer (handle function)
                  (mh-handle-set-external-undisplayer folder handle function)))
           (unwind-protect (mm-display-external part method)
             (set-buffer-modified-p nil)))))
   nil))

;;;###mh-autoload
(defun mh-folder-inline-mime-part (part-index)
  "Show attachment verbatim.

You can view the raw contents of an attachment with this command.
This command displays (or hides) the contents of the attachment
associated with the button under the cursor verbatim. If the
cursor is not located over a button, then the cursor first moves
to the next button, wrapping to the beginning of the message if
necessary.

You can also provide a numeric prefix argument PART-INDEX to view
the attachment labeled with that number."
  (interactive "P")
  (when (consp part-index) (setq part-index (car part-index)))
  (mh-folder-mime-action part-index #'mh-mime-inline-part nil))

(defun mh-mime-inline-part ()
  "Toggle display of the raw MIME part."
  (interactive)
  (let* ((buffer-read-only nil)
         (data (get-text-property (point) 'mh-data))
         (inserted-flag (get-text-property (point) 'mh-mime-inserted))
         (displayed-flag (mm-handle-displayed-p data))
         (point (point))
         start end)
    (cond ((and data (not inserted-flag) (not displayed-flag))
           (let ((contents (mm-get-part data)))
             (add-text-properties (mh-line-beginning-position)
                                  (mh-line-end-position) '(mh-mime-inserted t))
             (setq start (point-marker))
             (forward-line 1)
             (mm-insert-inline data contents)
             (setq end (point-marker))
             (add-text-properties
              start (progn (goto-char start) (mh-line-end-position))
              `(mh-region (,start . ,end)))))
          ((and data (or inserted-flag displayed-flag))
           (mh-press-button)
           (message "MIME part already inserted")))
    (goto-char point)
    (set-buffer-modified-p nil)))

;;;###mh-autoload
(defun mh-folder-save-mime-part (part-index)
  "Save (output) attachment.

This command saves the attachment associated with the button under the
cursor. If the cursor is not located over a button, then the cursor
first moves to the next button, wrapping to the beginning of the
message if necessary.

You can also provide a numeric prefix argument PART-INDEX to save the
attachment labeled with that number.

This command prompts you for a filename and suggests a specific name
if it is available."
  (interactive "P")
  (when (consp part-index) (setq part-index (car part-index)))
  (mh-folder-mime-action part-index #'mh-mime-save-part nil))

(defun mh-mime-save-part ()
  "Save MIME part at point."
  (interactive)
  (let ((data (get-text-property (point) 'mh-data)))
    (when data
      (let ((mm-default-directory
             (file-name-as-directory (or mh-mime-save-parts-directory
                                         default-directory))))
        (mh-mm-save-part data)
        (setq mh-mime-save-parts-directory mm-default-directory)))))

;;;###mh-autoload
(defun mh-folder-toggle-mime-part (part-index)
  "View attachment.

This command displays (or hides) the attachment associated with
the button under the cursor. If the cursor is not located over a
button, then the cursor first moves to the next button, wrapping
to the beginning of the message if necessary. This command has
the advantage over related commands of working from the MH-Folder
buffer.

You can also provide a numeric prefix argument PART-INDEX to view
the attachment labeled with that number. If Emacs does not know
how to display the attachment, then Emacs offers to save the
attachment in a file."
  (interactive "P")
  (when (consp part-index) (setq part-index (car part-index)))
  (mh-folder-mime-action part-index #'mh-press-button t))

;;;###mh-autoload
(defun mh-mime-save-parts (prompt)
  "Save attachments.

You can save all of the attachments at once with this command.
The attachments are saved in the directory specified by the
option `mh-mime-save-parts-default-directory' unless you use a
prefix argument PROMPT in which case you are prompted for the
directory. These directories may be superseded by MH profile
components, since this function calls on \"mhstore\" (\"mhn\") to
do the work."
  (interactive "P")
  (let ((msg (if (eq major-mode 'mh-show-mode)
                 (mh-show-buffer-message-number)
               (mh-get-msg-num t)))
        (folder (if (eq major-mode 'mh-show-mode)
                    mh-show-folder-buffer
                  mh-current-folder))
        (command (if (mh-variant-p 'nmh) "mhstore" "mhn"))
        (directory
         (cond
          ((and (or prompt
                    (equal nil mh-mime-save-parts-default-directory)
                    (equal t mh-mime-save-parts-default-directory))
                (not mh-mime-save-parts-directory))
           (read-directory-name "Store in directory: " nil nil t))
          ((and (or prompt
                    (equal t mh-mime-save-parts-default-directory))
                mh-mime-save-parts-directory)
           (read-directory-name (format
                            "Store in directory (default %s): "
                            mh-mime-save-parts-directory)
                           "" mh-mime-save-parts-directory t ""))
          ((stringp mh-mime-save-parts-default-directory)
           mh-mime-save-parts-default-directory)
          (t
           mh-mime-save-parts-directory))))
    (if (and (equal directory "") mh-mime-save-parts-directory)
        (setq directory mh-mime-save-parts-directory))
    (if (not (file-directory-p directory))
        (message "No directory specified")
      (if (equal nil mh-mime-save-parts-default-directory)
          (setq mh-mime-save-parts-directory directory))
      (with-current-buffer (get-buffer-create mh-log-buffer)
        (cd directory)
        (setq mh-mime-save-parts-directory directory)
        (let ((initial-size (mh-truncate-log-buffer)))
          (apply 'call-process
                 (expand-file-name command mh-progs) nil t nil
                 (mh-list-to-string (list folder msg "-auto"
                                          (if (not (mh-variant-p 'nmh))
                                              "-store"))))
          (if (> (buffer-size) initial-size)
              (save-window-excursion
                (switch-to-buffer-other-window mh-log-buffer)
                (sit-for 3))))))))

;;;###mh-autoload
(defun mh-toggle-mh-decode-mime-flag ()
  "Toggle the value of `mh-decode-mime-flag'."
  (interactive)
  (setq mh-decode-mime-flag (not mh-decode-mime-flag))
  (mh-show nil t)
  (message "%s" (if mh-decode-mime-flag
                    "Processing attachments normally"
                  "Displaying raw message")))

;;;###mh-autoload
(defun mh-toggle-mime-buttons ()
  "Toggle option `mh-display-buttons-for-inline-parts-flag'."
  (interactive)
  (setq mh-display-buttons-for-inline-parts-flag
        (not mh-display-buttons-for-inline-parts-flag))
  (mh-show nil t))



;;; MIME Display Routines

(defun mh-mm-inline-message (handle)
  "Display message, HANDLE.
The function decodes the message and displays it. It avoids
decoding the same message multiple times."
  (let ((b (point))
        (clean-message-header mh-clean-message-header-flag)
        (invisible-headers mh-invisible-header-fields-compiled)
        (visible-headers nil))
    (save-excursion
      (save-restriction
        (narrow-to-region b b)
        (mm-insert-part handle)
        (mh-mime-display
         (or (gethash handle (mh-mime-handles-cache (mh-buffer-data)))
             (setf (gethash handle (mh-mime-handles-cache (mh-buffer-data)))
                   (let ((handles (mm-dissect-buffer nil)))
                     (if handles
                         (mh-mm-uu-dissect-text-parts handles)
                       (setq handles (mm-uu-dissect)))
                     (setf (mh-mime-handles (mh-buffer-data))
                           (mh-mm-merge-handles
                            handles (mh-mime-handles (mh-buffer-data))))
                     handles))))

        (goto-char (point-min))
        (mh-show-xface)
        (cond (clean-message-header
               (mh-clean-msg-header (point-min)
                                    invisible-headers
                                    visible-headers)
               (goto-char (point-min)))
              (t
               (mh-start-of-uncleaned-message)))
        (mh-decode-message-header)
        (mh-show-addr)
        ;; The other highlighting types don't need anything special
        (when (eq mh-highlight-citation-style 'gnus)
          (mh-gnus-article-highlight-citation))
        (goto-char (point-min))
        (insert "\n------- Forwarded Message\n\n")
        (mh-display-smileys)
        (mh-display-emphasis)
        (mm-handle-set-undisplayer
         handle
         `(lambda ()
            (let (buffer-read-only)
              (if (fboundp 'remove-specifier)
                  ;; This is only valid on XEmacs.
                  (mapcar (lambda (prop)
                            (remove-specifier
                             (face-property 'default prop) (current-buffer)))
                          '(background background-pixmap foreground)))
              (delete-region ,(point-min-marker) ,(point-max-marker)))))))))

;;;###mh-autoload
(defun mh-decode-message-header ()
  "Decode RFC2047 encoded message header fields."
  (when mh-decode-mime-flag
    (let ((buffer-read-only nil))
      (rfc2047-decode-region (point-min) (mh-mail-header-end)))))

;;;###mh-autoload
(defun mh-decode-message-subject ()
  "Decode RFC2047 encoded message header fields."
  (when mh-decode-mime-flag
    (save-excursion
      (let ((buffer-read-only nil))
        (rfc2047-decode-region (progn (mh-goto-header-field "Subject:") (point))
                               (progn (mh-header-field-end) (point)))))))

;;;###mh-autoload
(defun mh-mime-display (&optional pre-dissected-handles)
  "Display (and possibly decode) MIME handles.
Optional argument, PRE-DISSECTED-HANDLES is a list of MIME
handles. If present they are displayed otherwise the buffer is
parsed and then displayed."
  (let ((handles ())
        (folder mh-show-folder-buffer)
        (raw-message-data (buffer-string)))
    (flet ((mm-handle-set-external-undisplayer
            (handle function)
            (mh-handle-set-external-undisplayer folder handle function)))
      (goto-char (point-min))
      (unless (search-forward "\n\n" nil t)
        (goto-char (point-max))
        (insert "\n\n"))

      (condition-case err
          (progn
            ;; If needed dissect the current buffer
            (if pre-dissected-handles
                (setq handles pre-dissected-handles)
              (if (setq handles (mm-dissect-buffer nil))
                  (mh-mm-uu-dissect-text-parts handles)
                (setq handles (mm-uu-dissect)))
              (setf (mh-mime-handles (mh-buffer-data))
                    (mh-mm-merge-handles handles
                                         (mh-mime-handles (mh-buffer-data))))
              (unless handles
                (mh-decode-message-body)))

            (cond ((and handles
                        (or (not (stringp (car handles)))
                            (cdr handles)))
                   ;; Go to start of message body
                   (goto-char (point-min))
                   (or (search-forward "\n\n" nil t)
                       (goto-char (point-max)))

                   ;; Delete the body
                   (delete-region (point) (point-max))

                   ;; Display the MIME handles
                   (mh-mime-display-part handles))
                  (t
                   (mh-signature-highlight))))
        (error
         (message "Could not display body: %s" (error-message-string err))
         (delete-region (point-min) (point-max))
         (insert raw-message-data))))))

(defun mh-decode-message-body ()
  "Decode message based on charset.
If message has been encoded for transfer take that into account."
  (let (ct charset cte)
    (goto-char (point-min))
    (re-search-forward "\n\n" nil t)
    (save-restriction
      (narrow-to-region (point-min) (point))
      (setq ct (ignore-errors (mail-header-parse-content-type
                               (message-fetch-field "Content-Type" t)))
            charset (mail-content-type-get ct 'charset)
            cte (message-fetch-field "Content-Transfer-Encoding")))
    (when (stringp cte) (setq cte (mail-header-strip cte)))
    (when (or (not ct) (equal (car ct) "text/plain"))
      (save-restriction
        (narrow-to-region (min (1+ (mh-mail-header-end)) (point-max))
                          (point-max))
        (mm-decode-body charset
                        (and cte (intern (downcase
                                          (gnus-strip-whitespace cte))))
                        (car ct))))))

(defun mh-mime-display-part (handle)
  "Decides the viewer to call based on the type of HANDLE."
  (cond ((null handle)
         nil)
        ((not (stringp (car handle)))
         (mh-mime-display-single handle))
        ((equal (car handle) "multipart/alternative")
         (mh-mime-display-alternative (cdr handle)))
        ((and mh-pgp-support-flag
              (or (equal (car handle) "multipart/signed")
                  (equal (car handle) "multipart/encrypted")))
         (mh-mime-display-security handle))
        (t
         (mh-mime-display-mixed (cdr handle)))))

(defun mh-mime-display-mixed (handles)
  "Display the list of MIME parts, HANDLES recursively."
  (mapcar #'mh-mime-display-part handles))

(defun mh-mime-display-alternative (handles)
  "Choose among the alternatives, HANDLES the part that will be displayed.
If no part is preferred then all the parts are displayed."
  (let* ((preferred (mm-preferred-alternative handles))
         (others (loop for x in handles unless (eq x preferred) collect x)))
    (cond ((and preferred
                (stringp (car preferred)))
           (mh-mime-display-part preferred)
           (mh-mime-maybe-display-alternatives others))
          (preferred
           (save-restriction
             (narrow-to-region (point) (if (eobp) (point) (1+ (point))))
             (mh-mime-display-single preferred)
             (mh-mime-maybe-display-alternatives others)
             (goto-char (point-max))))
          (t
           (mh-mime-display-mixed handles)))))

(defun mh-mime-maybe-display-alternatives (alternatives)
  "Show buttons for ALTERNATIVES.
If `mh-mime-display-alternatives-flag' is non-nil then display
buttons for alternative parts that are usually suppressed."
  (when (and mh-display-buttons-for-alternatives-flag alternatives)
    (insert "\n----------------------------------------------------\n")
    (insert "Alternatives:\n")
    (dolist (x alternatives)
      (insert "\n")
      (mh-insert-mime-button x (mh-mime-part-index x) nil))
    (insert "\n----------------------------------------------------\n")))

(defun mh-mime-display-security (handle)
  "Display PGP encrypted/signed message, HANDLE."
  (save-restriction
    (narrow-to-region (point) (point))
    (insert "\n")
    (mh-insert-mime-security-button handle)
    (mh-mime-display-mixed (cdr handle))
    (insert "\n")
    (let ((mh-mime-security-button-line-format
           mh-mime-security-button-end-line-format))
      (mh-insert-mime-security-button handle))
    (mh-mm-set-handle-multipart-parameter
     handle 'mh-region (cons (point-min-marker) (point-max-marker)))))

(defun mh-mime-display-single (handle)
  "Display a leaf node, HANDLE in the MIME tree."
  (let* ((type (mm-handle-media-type handle))
         (small-image-flag (mh-small-image-p handle))
         (attachmentp (equal (car (mm-handle-disposition handle))
                             "attachment"))
         (inlinep (and (equal (car (mm-handle-disposition handle)) "inline")
                       (mm-inlinable-p handle)
                       (mm-inlined-p handle)))
         (displayp (or inlinep                   ; show if inline OR
                       (mh-inline-vcard-p handle);      inline vcard OR
                       (and (not attachmentp)    ;      if not an attachment
                            (or small-image-flag ;        and small image
                                                 ;        and user wants inline
                                (and (not (equal
                                           (mm-handle-media-supertype handle)
                                           "image"))
                                     (mm-inlinable-p handle)
                                     (mm-inlined-p handle)))))))
    (save-restriction
      (narrow-to-region (point) (if (eobp) (point) (1+ (point))))
      (cond ((and mh-pgp-support-flag
                  (equal type "application/pgp-signature"))
             nil)             ; skip signatures as they are already handled...
            ((not displayp)
             (insert "\n")
             (mh-insert-mime-button handle (mh-mime-part-index handle) nil))
            ((and displayp
                  (not mh-display-buttons-for-inline-parts-flag))
             (or (mm-display-part handle)
                 (mm-display-part handle))
             (mh-signature-highlight handle))
            ((and displayp
                  mh-display-buttons-for-inline-parts-flag)
             (insert "\n")
             (mh-insert-mime-button handle (mh-mime-part-index handle) nil)
             (forward-line -1)
             (mh-mm-display-part handle)))
      (goto-char (point-max)))))

;; There is a bug in Gnus inline image display due to which an extra line
;; gets inserted every time it is viewed. To work around that problem we are
;; using an extra property 'mh-region to remember the region that is added
;; when the button is clicked. The region is then deleted to make sure that
;; no extra lines get inserted.
(defun mh-mm-display-part (handle)
  "Toggle display of button for MIME part, HANDLE."
  (beginning-of-line)
  (let ((id (get-text-property (point) 'mh-part))
        (point (point))
        (window (selected-window))
        (mail-parse-charset 'nil)
        (mail-parse-ignored-charsets nil)
        region buffer-read-only)
    (save-excursion
      (unwind-protect
          (let ((win (get-buffer-window (current-buffer) t)))
            (when win
              (select-window win))
            (goto-char point)

            (if (mm-handle-displayed-p handle)
                ;; This will remove the part.
                (progn
                  ;; Delete the button and displayed part (if any)
                  (let ((region (get-text-property point 'mh-region)))
                    (when region
                      (mh-funcall-if-exists
                       remove-images (car region) (cdr region)))
                    (mm-display-part handle)
                    (when region
                      (delete-region (car region) (cdr region))))
                  ;; Delete button (if it still remains). This happens for
                  ;; externally displayed parts where the previous step does
                  ;; nothing.
                  (unless (eolp)
                    (delete-region (point) (progn (forward-line) (point)))))
              (save-restriction
                (delete-region (point) (progn (forward-line 1) (point)))
                (narrow-to-region (point) (point))
                ;; Maybe we need another unwind-protect here.
                (when (equal (mm-handle-media-supertype handle) "image")
                  (insert "\n"))
                (when (and (not (eq (ignore-errors (mm-display-part handle))
                                    'inline))
                           (equal (mm-handle-media-supertype handle)
                                  "image"))
                  (goto-char (point-min))
                  (delete-char 1))
                (when (equal (mm-handle-media-supertype handle) "text")
                  (when (eq mh-highlight-citation-style 'gnus)
                    (mh-gnus-article-highlight-citation))
                  (mh-display-smileys)
                  (mh-display-emphasis)
                  (mh-signature-highlight handle))
                (setq region (cons (progn (goto-char (point-min))
                                          (point-marker))
                                   (progn (goto-char (point-max))
                                          (point-marker)))))))
        (when (window-live-p window)
          (select-window window))
        (goto-char point)
        (beginning-of-line)
        (mh-insert-mime-button handle id (mm-handle-displayed-p handle))
        (goto-char point)
        (when region
          (add-text-properties (mh-line-beginning-position)
                               (mh-line-end-position)
                               `(mh-region ,region)))))))

(defun mh-mime-part-index (handle)
  "Generate the button number for MIME part, HANDLE.
Notice that a hash table is used to display the same number when
buttons need to be displayed multiple times (for instance when
nested messages are opened)."
  (or (gethash handle (mh-mime-part-index-hash (mh-buffer-data)))
      (setf (gethash handle (mh-mime-part-index-hash (mh-buffer-data)))
            (incf (mh-mime-parts-count (mh-buffer-data))))))

(defun mh-small-image-p (handle)
  "Decide whether HANDLE is a \"small\" image that can be displayed inline.
This is only useful if a Content-Disposition header is not present."
  (let ((media-test (caddr (assoc (car (mm-handle-type handle))
                                  mh-mm-inline-media-tests)))
        (mm-inline-large-images t))
    (and media-test
         (equal (mm-handle-media-supertype handle) "image")
         (funcall media-test handle) ; Since mm-inline-large-images is T,
                                        ; this only tells us if the image is
                                        ; something that emacs can display
         (let* ((image (mm-get-image handle)))
           (or (mh-do-in-xemacs
                 (and (mh-funcall-if-exists glyphp image)
                      (< (glyph-width image)
                         (or mh-max-inline-image-width (window-pixel-width)))
                      (< (glyph-height image)
                         (or mh-max-inline-image-height
                             (window-pixel-height)))))
               (mh-do-in-gnu-emacs
                 (let ((size (mh-funcall-if-exists image-size image)))
                   (and size
                        (< (cdr size) (or mh-max-inline-image-height
                                          (1- (window-height))))
                        (< (car size) (or mh-max-inline-image-width
                                          (window-width)))))))))))

(defun mh-inline-vcard-p (handle)
  "Decide if HANDLE is a vcard that must be displayed inline."
  (let ((type (mm-handle-type handle)))
    (and (or (featurep 'vcard) (fboundp 'vcard-pretty-print))
         (consp type)
         (equal (car type) "text/x-vcard")
         (save-excursion
           (save-restriction
             (widen)
             (goto-char (point-min))
             (not (mh-signature-separator-p)))))))

(defun mh-signature-highlight (&optional handle)
  "Highlight message signature in HANDLE.
The optional argument, HANDLE is a MIME handle if the function is
being used to highlight the signature in a MIME part."
  (let ((regexp
         (cond ((not handle) "^-- $")
               ((not (and (equal (mm-handle-media-supertype handle) "text")
                          (equal (mm-handle-media-subtype handle) "html")))
                "^-- $")
               ((eq (mh-mm-text-html-renderer) 'lynx) "^   --$")
               (t "^--$"))))
    (save-excursion
      (goto-char (point-max))
      (when (re-search-backward regexp nil t)
        (mh-do-in-gnu-emacs
          (let ((ov (make-overlay (point) (point-max))))
            (overlay-put ov 'face 'mh-show-signature)
            (overlay-put ov 'evaporate t)))
        (mh-do-in-xemacs
          (set-extent-property (make-extent (point) (point-max))
                               'face 'mh-show-signature))))))



;;; Button Display

;; Shush compiler.
(mh-do-in-xemacs
  (defvar dots)
  (defvar type)
  (defvar ov))

(defun mh-insert-mime-button (handle index displayed)
  "Insert MIME button for HANDLE.
INDEX is the part number that will be DISPLAYED. It is also used
by commands like \"K v\" which operate on individual MIME parts."
  ;; The button could be displayed by a previous decode. In that case
  ;; undisplay it if we need a hidden button.
  (when (and (mm-handle-displayed-p handle) (not displayed))
    (mm-display-part handle))
  (let ((name (or (mail-content-type-get (mm-handle-type handle) 'name)
                  (mail-content-type-get (mm-handle-disposition handle)
                                         'filename)
                  (mail-content-type-get (mm-handle-type handle) 'url)
                  ""))
        (type (mm-handle-media-type handle))
        (description (mail-decode-encoded-word-string
                      (or (mm-handle-description handle) "")))
        (dots (if (or displayed (mm-handle-displayed-p handle)) "   " "..."))
        long-type begin end)
    (if (string-match ".*/" name) (setq name (substring name (match-end 0))))
    (setq long-type (concat type (and (not (equal name ""))
                                      (concat "; " name))))
    (unless (equal description "")
      (setq long-type (concat " --- " long-type)))
    (unless (bolp) (insert "\n"))
    (setq begin (point))
    (gnus-eval-format
     mh-mime-button-line-format mh-mime-button-line-format-alist
     `(,@(mh-gnus-local-map-property mh-mime-button-map)
         mh-callback mh-mm-display-part
         mh-part ,index
         mh-data ,handle))
    (setq end (point))
    (widget-convert-button
     'link begin end
     :mime-handle handle
     :action 'mh-widget-press-button
     :button-keymap mh-mime-button-map
     :help-echo
     "Mouse-2 click or press RET (in show buffer) to toggle display")
    (dolist (ov (mh-funcall-if-exists overlays-in begin end))
      (mh-funcall-if-exists overlay-put ov 'evaporate t))))

;; Shush compiler.
(defvar mm-verify-function-alist)       ; < Emacs 22
(defvar mm-decrypt-function-alist)      ; < Emacs 22
(mh-do-in-xemacs
  (defvar pressed-details))

(defun mh-insert-mime-security-button (handle)
  "Display buttons for PGP message, HANDLE."
  (let* ((protocol (mh-mm-handle-multipart-ctl-parameter handle 'protocol))
         (crypto-type (or (nth 2 (assoc protocol mm-verify-function-alist))
                          (nth 2 (assoc protocol mm-decrypt-function-alist))
                          "Unknown"))
         (type (concat crypto-type
                       (if (equal (car handle) "multipart/signed")
                           " Signed" " Encrypted")
                       " Part"))
         (info (or (mh-mm-handle-multipart-ctl-parameter handle 'gnus-info)
                   "Undecided"))
         (details (mh-mm-handle-multipart-ctl-parameter handle 'gnus-details))
         pressed-details begin end face)
    (setq details (if details (concat "\n" details) ""))
    (setq pressed-details (if mh-mime-security-button-pressed details ""))
    (setq face (mh-mime-security-button-face info))
    (unless (bolp) (insert "\n"))
    (setq begin (point))
    (gnus-eval-format
     mh-mime-security-button-line-format
     mh-mime-security-button-line-format-alist
     `(,@(mh-gnus-local-map-property mh-mime-security-button-map)
         mh-button-pressed ,mh-mime-security-button-pressed
         mh-callback mh-mime-security-press-button
         mh-line-format ,mh-mime-security-button-line-format
         mh-data ,handle))
    (setq end (point))
    (widget-convert-button 'link begin end
                           :mime-handle handle
                           :action 'mh-widget-press-button
                           :button-keymap mh-mime-security-button-map
                           :button-face face
                           :help-echo "Mouse-2 click or press RET (in show buffer) to see security details.")
    (dolist (ov (mh-funcall-if-exists overlays-in begin end))
      (mh-funcall-if-exists overlay-put ov 'evaporate t))
    (when (equal info "Failed")
      (let* ((type (if (equal (car handle) "multipart/signed")
                       "verification" "decryption"))
             (warning (if (equal type "decryption")
                          "(passphrase may be incorrect)" "")))
        (message "%s %s failed %s" crypto-type type warning)))))

(defun mh-mime-security-button-face (info)
  "Return the button face to use for encrypted/signed mail based on INFO."
  (cond ((string-match "OK" info)       ;Decrypted mail
         'mh-show-pgg-good)
        ((string-match "Failed" info)   ;Decryption failed or signature invalid
         'mh-show-pgg-bad)
        ((string-match "Undecided" info);Unprocessed mail
         'mh-show-pgg-unknown)
        ((string-match "Untrusted" info);Key not trusted
         'mh-show-pgg-unknown)
        (t
         'mh-show-pgg-good)))



;;; Button Handlers

(defun mh-folder-mime-action (part-index action include-security-flag)
  "Go to PART-INDEX and carry out ACTION.

If PART-INDEX is nil then go to the next part in the buffer. The
search for the next buffer wraps around if end of buffer is reached.
If argument INCLUDE-SECURITY-FLAG is non-nil then include security
info buttons when searching for a suitable parts."
  (unless mh-showing-mode
    (mh-show))
  (mh-in-show-buffer (mh-show-buffer)
    (let ((criterion
           (cond (part-index
                  (lambda (p)
                    (let ((part (get-text-property p 'mh-part)))
                      (and (integerp part) (= part part-index)))))
                 (t (lambda (p)
                      (if include-security-flag
                          (get-text-property p 'mh-data)
                        (integerp (get-text-property p 'mh-part)))))))
          (point (point)))
      (cond ((and (get-text-property point 'mh-part)
                  (or (null part-index)
                      (= (get-text-property point 'mh-part) part-index)))
             (funcall action))
            ((and (get-text-property point 'mh-data)
                  include-security-flag
                  (null part-index))
             (funcall action))
            (t
             (mh-goto-next-button nil criterion)
             (if (= (point) point)
                 (message "No matching MIME part found")
               (funcall action)))))))

;;;###mh-autoload
(defun mh-goto-next-button (backward-flag &optional criterion)
  "Search for next button satisfying criterion.

If BACKWARD-FLAG is non-nil search backward in the buffer for a mime
button.
If CRITERION is a function or a symbol which has a function binding
then that function must return non-nil at the button we stop."
  (unless (or (and (symbolp criterion) (fboundp criterion))
              (functionp criterion))
    (setq criterion (lambda (x) t)))
  ;; Move to the next button in the buffer satisfying criterion
  (goto-char (or (save-excursion
                   (beginning-of-line)
                   ;; Find point before current button
                   (let ((point-before-current-button
                          (save-excursion
                            (while (get-text-property (point) 'mh-data)
                              (unless (= (forward-line
                                          (if backward-flag 1 -1))
                                         0)
                                (if backward-flag
                                    (goto-char (point-min))
                                  (goto-char (point-max)))))
                            (point))))
                     ;; Skip over current button
                     (while (and (get-text-property (point) 'mh-data)
                                 (not (if backward-flag (bobp) (eobp))))
                       (forward-line (if backward-flag -1 1)))
                     ;; Stop at next MIME button if any exists.
                     (block loop
                       (while (/= (progn
                                    (unless (= (forward-line
                                                (if backward-flag -1 1))
                                               0)
                                      (if backward-flag
                                          (goto-char (point-max))
                                        (goto-char (point-min)))
                                      (beginning-of-line))
                                    (point))
                                  point-before-current-button)
                         (when (and (get-text-property (point) 'mh-data)
                                    (funcall criterion (point)))
                           (return-from loop (point))))
                       nil)))
                 (point))))

(defun mh-widget-press-button (widget el)
  "Callback for widget, WIDGET.
Parameter EL is unused."
  (goto-char (widget-get widget :from))
  (mh-press-button))

(defun mh-press-button ()
  "View contents of button.

This command is a toggle so if you use it again on the same
attachment, the attachment is hidden."
  (interactive)
  (let ((mm-inline-media-tests mh-mm-inline-media-tests)
        (data (get-text-property (point) 'mh-data))
        (function (get-text-property (point) 'mh-callback))
        (buffer-read-only nil)
        (folder mh-show-folder-buffer))
    (flet ((mm-handle-set-external-undisplayer
            (handle function)
            (mh-handle-set-external-undisplayer folder handle function)))
      (when (and function (eolp))
        (backward-char))
      (unwind-protect (and function (funcall function data))
        (set-buffer-modified-p nil)))))

(defun mh-push-button (event)
  "Click MIME button for EVENT.

If the MIME part is visible then it is removed. Otherwise the
part is displayed. This function is called when the mouse is used
to click the MIME button."
  (interactive "e")
  (mh-do-at-event-location event
    (let ((folder mh-show-folder-buffer)
          (mm-inline-media-tests mh-mm-inline-media-tests)
          (data (get-text-property (point) 'mh-data))
          (function (get-text-property (point) 'mh-callback)))
      (flet ((mm-handle-set-external-undisplayer (handle func)
               (mh-handle-set-external-undisplayer folder handle func)))
        (and function (funcall function data))))))

(defun mh-handle-set-external-undisplayer (folder handle function)
  "Replacement for `mm-handle-set-external-undisplayer'.

This is only called in recent versions of Gnus. The MIME handles
are stored in data structures corresponding to MH-E folder buffer
FOLDER instead of in Gnus (as in the original). The MIME part,
HANDLE is associated with the undisplayer FUNCTION."
  (if (mh-mm-keep-viewer-alive-p handle)
      (let ((new-handle (copy-sequence handle)))
        (mm-handle-set-undisplayer new-handle function)
        (mm-handle-set-undisplayer handle nil)
        (with-current-buffer folder
          (push new-handle (mh-mime-handles (mh-buffer-data)))))
    (mm-handle-set-undisplayer handle function)))

(defun mh-mime-security-press-button (handle)
  "Callback from security button for part HANDLE."
  (if (mh-mm-handle-multipart-ctl-parameter handle 'gnus-info)
      (mh-mime-security-show-details handle)
    (let ((region (mh-mm-handle-multipart-ctl-parameter handle 'mh-region))
          point)
      (setq point (point))
      (goto-char (car region))
      (delete-region (car region) (cdr region))
      (with-current-buffer (mh-mm-handle-multipart-ctl-parameter handle 'buffer)
        (let* ((mm-verify-option 'known)
               (mm-decrypt-option 'known)
               (new (mh-mm-possibly-verify-or-decrypt (cdr handle) handle)))
          (unless (eq new (cdr handle))
            (mh-mm-destroy-parts (cdr handle))
            (setcdr handle new))))
      (mh-mime-display-security handle)
      (goto-char point))))

;; I rewrote the security part because Gnus doesn't seem to ever minimize
;; the button. That is once the mime-security button is pressed there seems
;; to be no way of getting rid of the inserted text.
(defun mh-mime-security-show-details (handle)
  "Toggle display of detailed security info for HANDLE."
  (let ((details (mh-mm-handle-multipart-ctl-parameter handle 'gnus-details)))
    (when details
      (let ((mh-mime-security-button-pressed
             (not (get-text-property (point) 'mh-button-pressed)))
            (mh-mime-security-button-line-format
             (get-text-property (point) 'mh-line-format)))
        (forward-char -1)
        (while (eq (get-text-property (point) 'mh-line-format)
                   mh-mime-security-button-line-format)
          (forward-char -1))
        (forward-char)
        (save-restriction
          (narrow-to-region (point) (point))
          (mh-insert-mime-security-button handle))
        (delete-region
         (point)
         (or (text-property-not-all
              (point) (point-max)
              'mh-line-format mh-mime-security-button-line-format)
             (point-max)))
        (forward-line -1)))))



;;; Miscellaneous Article Washing

;;;###mh-autoload
(defun mh-add-missing-mime-version-header ()
  "Some mail programs don't put a MIME-Version header.
I have seen this only in spam, so maybe we shouldn't fix
this ;-)"
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "\n\n" nil t)
    (save-restriction
      (narrow-to-region (point-min) (point))
      (when (and (message-fetch-field "content-type")
                 (not (message-fetch-field "mime-version")))
        (goto-char (point-min))
        (insert "MIME-Version: 1.0\n")))))

;;;###mh-autoload
(defun mh-display-smileys ()
  "Display smileys."
  (when (and mh-graphical-smileys-flag (mh-small-show-buffer-p))
    (mh-funcall-if-exists smiley-region (point-min) (point-max))))

;;;###mh-autoload
(defun mh-display-emphasis ()
  "Display graphical emphasis."
  (when (and mh-graphical-emphasis-flag (mh-small-show-buffer-p))
    (flet ((article-goto-body ()))      ; shadow this function to do nothing
      (save-excursion
        (goto-char (point-min))
        (article-emphasize)))))

(defun mh-small-show-buffer-p ()
  "Check if show buffer is small.
This is used to decide if smileys and graphical emphasis should be
displayed."
  (let ((max nil))
    (when (and (boundp 'font-lock-maximum-size) font-lock-maximum-size)
      (cond ((numberp font-lock-maximum-size)
             (setq max font-lock-maximum-size))
            ((listp font-lock-maximum-size)
             (setq max (cdr (or (assoc 'mh-show-mode font-lock-maximum-size)
                                (assoc t font-lock-maximum-size)))))))
    (or (not (numberp max)) (>= (/ max 8) (buffer-size)))))



;;; MH-Letter Commands

;; MH-E commands are alphabetical; specific support routines follow command.

;;;###mh-autoload
(defun mh-compose-forward (&optional description folder range)
  "Add tag to forward a message.

You are prompted for a content DESCRIPTION, the name of the
FOLDER in which the messages to forward are located, and a RANGE
of messages, which defaults to the current message in that
folder. Check the documentation of `mh-interactive-range' to see
how RANGE is read in interactive use.

The option `mh-compose-insertion' controls what type of tags are inserted."
  (interactive
   (let* ((description
           (mml-minibuffer-read-description))
          (folder
           (mh-prompt-for-folder "Message from"
                                 mh-sent-from-folder nil))
          (default
            (if (and (equal folder mh-sent-from-folder)
                     (numberp mh-sent-from-msg))
                mh-sent-from-msg
              (nth 0 (mh-translate-range folder "cur"))))
          (range
           (mh-read-range "Forward" folder
                          (or (and default
                                   (number-to-string default))
                              t)
                          t t)))
     (list description folder range)))
  (let ((messages (mapconcat 'identity (mh-list-to-string range) " ")))
    (dolist (message (mh-translate-range folder messages))
      (if (equal mh-compose-insertion 'mml)
          (mh-mml-forward-message description folder (format "%s" message))
        (mh-mh-forward-message description folder (format "%s" message))))))

;;;###mh-autoload
(defun mh-mml-forward-message (description folder message)
  "Forward a message as attachment.

The function will prompt the user for a DESCRIPTION, a FOLDER and
MESSAGE number."
  (let ((msg (if (and (equal message "") (numberp mh-sent-from-msg))
                 mh-sent-from-msg
               (string-to-number message))))
    (cond ((integerp msg)
           (mml-attach-file (format "%s%s/%d"
                                    mh-user-path (substring folder 1) msg)
                            "message/rfc822"
                            (if (string= "" description) nil description)
                            "inline"))
          (t (error "The message number, %s, is not a integer" msg)))))

(defun mh-mh-forward-message (&optional description folder messages)
  "Add tag to forward a message.
You are prompted for a content DESCRIPTION, the name of the
FOLDER in which the messages to forward are located, and the
MESSAGES' numbers.

See also \\[mh-mh-to-mime]."
  (interactive (list
                (mml-minibuffer-read-description)
                (mh-prompt-for-folder "Message from" mh-sent-from-folder nil)
                (read-string (concat "Messages"
                                     (if (numberp mh-sent-from-msg)
                                         (format " (default %d): "
                                                 mh-sent-from-msg)
                                       ": ")))))
  (beginning-of-line)
  (insert "#forw [")
  (and description
       (not (string= description ""))
       (insert description))
  (insert "]")
  (and folder
       (not (string= folder ""))
       (insert " " folder))
  (if (and messages
           (not (string= messages "")))
      (let ((start (point)))
        (insert " " messages)
        (subst-char-in-region start (point) ?, ? ))
    (if (numberp mh-sent-from-msg)
        (insert " " (int-to-string mh-sent-from-msg))))
  (insert "\n"))

;;;###mh-autoload
(defun mh-compose-insertion (&optional inline)
  "Add tag to include a file such as an image or sound.

You are prompted for the filename containing the object, the
media type if it cannot be determined automatically, and a
content description. If you're using MH-style directives, you
will also be prompted for additional attributes.

The option `mh-compose-insertion' controls what type of tags are
inserted. Optional argument INLINE means make it an inline
attachment."
  (interactive "P")
  (if (equal mh-compose-insertion 'mml)
      (if inline
          (mh-mml-attach-file "inline")
        (mh-mml-attach-file))
    (call-interactively 'mh-mh-attach-file)))

(defun mh-mml-attach-file (&optional disposition)
  "Add a tag to insert a MIME message part from a file.

You are prompted for the filename containing the object, the
media type if it cannot be determined automatically, a content
description and the DISPOSITION of the attachment.

This is basically `mml-attach-file' from Gnus, modified such that a prefix
argument yields an \"inline\" disposition and Content-Type is determined
automatically."
  (let* ((file (mml-minibuffer-read-file "Attach file: "))
         (type (mh-minibuffer-read-type file))
         (description (mml-minibuffer-read-description))
         (dispos (or disposition
                     (mh-mml-minibuffer-read-disposition type))))
    (mml-insert-empty-tag 'part 'type type 'filename file
                          'disposition dispos 'description description)))

(defun mh-mh-attach-file (filename type description attributes)
  "Add a tag to insert a MIME message part from a file.
You are prompted for the FILENAME containing the object, the
media TYPE if it cannot be determined automatically, and a
content DESCRIPTION. In addition, you are also prompted for
additional ATTRIBUTES.

See also \\[mh-mh-to-mime]."
  (interactive (let ((filename (mml-minibuffer-read-file "Attach file: ")))
                 (list
                  filename
                  (mh-minibuffer-read-type filename)
                  (mml-minibuffer-read-description)
                  (read-string "Attributes: "
                               (concat "name=\""
                                       (file-name-nondirectory filename)
                                       "\"")))))
  (mh-mh-compose-type filename type description attributes))

(defun mh-mh-compose-type (filename type
                                     &optional description attributes comment)
  "Insert an MH-style directive to insert a file.
The file specified by FILENAME is encoded as TYPE. An optional
DESCRIPTION is used as the Content-Description field, optional
set of ATTRIBUTES and an optional COMMENT can also be included."
  (beginning-of-line)
  (insert "#" type)
  (and attributes
       (insert "; " attributes))
  (and comment
       (insert " (" comment ")"))
  (insert " [")
  (and description
       (insert description))
  (insert "] " (expand-file-name filename))
  (insert "\n"))

;;;###mh-autoload
(defun mh-mh-compose-anon-ftp (host filename type description)
  "Add tag to include anonymous ftp reference to a file.

You can have your message initiate an \"ftp\" transfer when the
recipient reads the message. You are prompted for the remote HOST
and FILENAME, the media TYPE, and the content DESCRIPTION.

See also \\[mh-mh-to-mime]."
  (interactive (list
                (read-string "Remote host: ")
                (read-string "Remote filename: ")
                (mh-minibuffer-read-type "DUMMY-FILENAME")
                (mml-minibuffer-read-description)))
  (mh-mh-compose-external-type "anon-ftp" host filename
                               type description))

;;;###mh-autoload
(defun mh-mh-compose-external-compressed-tar (host filename description)
  "Add tag to include anonymous ftp reference to a compressed tar file.

In addition to retrieving the file via anonymous \"ftp\" as per
the command \\[mh-mh-compose-anon-ftp], the file will also be
uncompressed and untarred. You are prompted for the remote HOST
and FILENAME and the content DESCRIPTION.

See also \\[mh-mh-to-mime]."
  (interactive (list
                (read-string "Remote host: ")
                (read-string "Remote filename: ")
                (mml-minibuffer-read-description)))
  (mh-mh-compose-external-type "anon-ftp" host filename
                               "application/octet-stream"
                               description
                               "type=tar; conversions=x-compress"
                               "mode=image"))

;; RFC 2045 - Multipurpose Internet Mail Extensions (MIME) Part One:
;;            Format of Internet Message Bodies.
;; RFC 2046 - Multipurpose Internet Mail Extensions (MIME) Part Two:
;;            Media Types.
;; RFC 2049 - Multipurpose Internet Mail Extensions (MIME) Part Five:
;;            Conformance Criteria and Examples.
;; RFC 2017 - Definition of the URL MIME External-Body Access-Type
;; RFC 1738 - Uniform Resource Locators (URL)
(defvar mh-access-types
  '(("anon-ftp")        ; RFC2046 Anonymous File Transfer Protocol
    ("file")            ; RFC1738 Host-specific file names
    ("ftp")             ; RFC2046 File Transfer Protocol
    ("gopher")          ; RFC1738 The Gopher Protocol
    ("http")            ; RFC1738 Hypertext Transfer Protocol
    ("local-file")      ; RFC2046 Local file access
    ("mail-server")     ; RFC2046 mail-server Electronic mail address
    ("mailto")          ; RFC1738 Electronic mail address
    ("news")            ; RFC1738 Usenet news
    ("nntp")            ; RFC1738 Usenet news using NNTP access
    ("prospero")        ; RFC1738 Prospero Directory Service
    ("telnet")          ; RFC1738 Telnet
    ("tftp")            ; RFC2046 Trivial File Transfer Protocol
    ("url")             ; RFC2017 URL scheme MIME access-type Protocol
    ("wais"))           ; RFC1738 Wide Area Information Servers
  "Valid MIME access-type values.")

;;;###mh-autoload
(defun mh-mh-compose-external-type (access-type host filename type
                                                &optional description
                                                attributes parameters
                                                comment)
  "Add tag to refer to a remote file.

This command is a general utility for referencing external files.
In fact, all of the other commands that insert directives to
access external files call this command. You are prompted for the
ACCESS-TYPE, remote HOST and FILENAME, and content TYPE. If you
provide a prefix argument, you are also prompted for a content
DESCRIPTION, ATTRIBUTES, PARAMETERS, and a COMMENT.

See also \\[mh-mh-to-mime]."
  (interactive (list
                (completing-read "Access type: " mh-access-types)
                (read-string "Remote host: ")
                (read-string "Remote filename: ")
                (mh-minibuffer-read-type "DUMMY-FILENAME")
                (if current-prefix-arg (mml-minibuffer-read-description))
                (if current-prefix-arg (read-string "Attributes: "))
                (if current-prefix-arg (read-string "Parameters: "))
                (if current-prefix-arg (read-string "Comment: "))))
  (beginning-of-line)
  (insert "#@" type)
  (and attributes
       (insert "; " attributes))
  (and comment
       (insert " (" comment ") "))
  (insert " [")
  (and description
       (insert description))
  (insert "] ")
  (insert "access-type=" access-type "; ")
  (insert "site=" host)
  (insert "; name=" (file-name-nondirectory filename))
  (let ((directory (file-name-directory filename)))
    (and directory
         (insert "; directory=\"" directory "\"")))
  (and parameters
       (insert "; " parameters))
  (insert "\n"))

(defvar mh-mh-to-mime-args nil
  "Extra arguments for \\[mh-mh-to-mime] to pass to the \"mhbuild\" command.
The arguments are passed to \"mhbuild\" if \\[mh-mh-to-mime] is
given a prefix argument. Normally default arguments to
\"mhbuild\" are specified in the MH profile.")

;;;###mh-autoload
(defun mh-mh-to-mime (&optional extra-args)
  "Compose MIME message from MH-style directives.

Typically, you send a message with attachments just like any other
message. However, you may take a sneak preview of the MIME encoding if
you wish by running this command.

If you wish to pass additional arguments to \"mhbuild\" (\"mhn\")
to affect how it builds your message, use the option
`mh-mh-to-mime-args'. For example, you can build a consistency
check into the message by setting `mh-mh-to-mime-args' to
\"-check\". The recipient of your message can then run \"mhbuild
-check\" on the message--\"mhbuild\" (\"mhn\") will complain if
the message has been corrupted on the way. This command only
consults this option when given a prefix argument EXTRA-ARGS.

The hook `mh-mh-to-mime-hook' is called after the message has been
formatted.

The effects of this command can be undone by running
\\[mh-mh-to-mime-undo]."
  (interactive "*P")
  (mh-mh-quote-unescaped-sharp)
  (save-buffer)
  (message "Running %s..." (if (mh-variant-p 'nmh) "mhbuild" "mhn"))
  (cond
   ((mh-variant-p 'nmh)
    (mh-exec-cmd-error nil
                       "mhbuild"
                       (if extra-args mh-mh-to-mime-args)
                       buffer-file-name))
   (t
    (mh-exec-cmd-error (format "mhdraft=%s" buffer-file-name)
                       "mhn"
                       (if extra-args mh-mh-to-mime-args)
                       buffer-file-name)))
  (revert-buffer t t t)
  (message "Running %s...done" (if (mh-variant-p 'nmh) "mhbuild" "mhn"))
  (run-hooks 'mh-mh-to-mime-hook))

(defun mh-mh-quote-unescaped-sharp ()
  "Quote \"#\" characters that haven't been quoted for \"mhbuild\".
If the \"#\" character is present in the first column, but it isn't
part of a MH-style directive then \"mhbuild\" gives an error.
This function will quote all such characters."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^#" nil t)
      (beginning-of-line)
      (unless (mh-mh-directive-present-p (point) (mh-line-end-position))
        (insert "#"))
      (goto-char (mh-line-end-position)))))

;;;###mh-autoload
(defun mh-mh-to-mime-undo (noconfirm)
  "Undo effects of \\[mh-mh-to-mime].

It does this by reverting to a backup file. You are prompted to
confirm this action, but you can avoid the confirmation by adding
a prefix argument NOCONFIRM."
  (interactive "*P")
  (if (null buffer-file-name)
      (error "Buffer does not seem to be associated with any file"))
  (let ((backup-strings '("," "#"))
        backup-file)
    (while (and backup-strings
                (not (file-exists-p
                      (setq backup-file
                            (concat (file-name-directory buffer-file-name)
                                    (car backup-strings)
                                    (file-name-nondirectory buffer-file-name)
                                    ".orig")))))
      (setq backup-strings (cdr backup-strings)))
    (or backup-strings
        (error "Backup file for %s no longer exists" buffer-file-name))
    (or noconfirm
        (yes-or-no-p (format "Revert buffer from file %s? "
                             backup-file))
        (error "Revert not confirmed"))
    (let ((buffer-read-only nil))
      (erase-buffer)
      (insert-file-contents backup-file))
    (after-find-file nil nil nil nil t)))

;; Shush compiler.
(defvar mh-identity-pgg-default-user-id)

;;;###mh-autoload
(defun mh-mml-secure-message-encrypt (method)
  "Add tag to encrypt the message.

A proper multipart message is created for you when you send the
message. Use the command \\[mh-mml-unsecure-message] to remove
this tag. Use a prefix argument METHOD to be prompted for one of
the possible security methods (see `mh-mml-method-default')."
  (interactive (list (mh-mml-query-cryptographic-method)))
  (mh-secure-message method "encrypt" mh-identity-pgg-default-user-id))

;;;###mh-autoload
(defun mh-mml-secure-message-sign (method)
  "Add tag to sign the message.

A proper multipart message is created for you when you send the
message. Use the command \\[mh-mml-unsecure-message] to remove
this tag. Use a prefix argument METHOD to be prompted for one of
the possible security methods (see `mh-mml-method-default')."
  (interactive (list (mh-mml-query-cryptographic-method)))
  (mh-secure-message method "sign" mh-identity-pgg-default-user-id))

;;;###mh-autoload
(defun mh-mml-secure-message-signencrypt (method)
  "Add tag to encrypt and sign the message.

A proper multipart message is created for you when you send the
message. Use the command \\[mh-mml-unsecure-message] to remove
this tag. Use a prefix argument METHOD to be prompted for one of
the possible security methods (see `mh-mml-method-default')."
  (interactive (list (mh-mml-query-cryptographic-method)))
  (mh-secure-message method "signencrypt" mh-identity-pgg-default-user-id))

(defvar mh-mml-cryptographic-method-history ())

(defun mh-mml-query-cryptographic-method ()
  "Read the cryptographic method to use."
  (if current-prefix-arg
      (let ((def (or (car mh-mml-cryptographic-method-history)
                     mh-mml-method-default)))
        (completing-read (format "Method (default %s): " def)
                         '(("pgp") ("pgpmime") ("smime"))
                         nil t nil 'mh-mml-cryptographic-method-history def))
    mh-mml-method-default))

(defun mh-secure-message (method mode &optional identity)
  "Add tag to encrypt or sign message.

METHOD should be one of: \"pgpmime\", \"pgp\", \"smime\".
MODE should be one of: \"sign\", \"encrypt\", \"signencrypt\", \"none\".
IDENTITY is optionally the default-user-id to use."
  (if (not mh-pgp-support-flag)
      (error "Your version of Gnus does not support PGP/GPG")
    ;; Check the arguments
    (let ((valid-methods (list "pgpmime" "pgp" "smime"))
          (valid-modes (list "sign" "encrypt" "signencrypt" "none")))
      (if (not (member method valid-methods))
          (error "Method %s is invalid" method))
      (if (not (member mode valid-modes))
          (error "Mode %s is invalid" mode))
      (mml-unsecure-message)
      (if (not (string= mode "none"))
        (save-excursion
          (goto-char (point-min))
          (mh-goto-header-end 1)
          (if mh-identity-pgg-default-user-id
              (mml-insert-tag 'secure 'method method 'mode mode
                              'sender mh-identity-pgg-default-user-id)
            (mml-insert-tag 'secure 'method method 'mode mode)))))))

;;;###mh-autoload
(defun mh-mml-to-mime ()
  "Compose MIME message from MML tags.

Typically, you send a message with attachments just like any
other message. However, you may take a sneak preview of the MIME
encoding if you wish by running this command.

This action can be undone by running \\[undo]."
  (interactive)
  (require 'message)
  (when mh-pgp-support-flag
    ;; PGP requires actual e-mail addresses, not aliases.
    ;; Parse the recipients and sender from the message.
    (message-options-set-recipient)
    ;; Do an alias lookup on sender (if From field is present).
    (when (message-options-get 'message-sender)
      (message-options-set 'message-sender
                           (mail-strip-quoted-names
                            (mh-alias-expand
                             (message-options-get 'message-sender)))))
    ;; Do an alias lookup on recipients
    (message-options-set 'message-recipients
                         (mapconcat
                          (lambda (ali)
                            (mail-strip-quoted-names (mh-alias-expand ali)))
                          (split-string (message-options-get 'message-recipients) "[, ]+")
                          ", ")))
  (let ((saved-text (buffer-string))
        (buffer (current-buffer))
        (modified-flag (buffer-modified-p)))
    (condition-case err (mml-to-mime)
      (error
       (with-current-buffer buffer
         (delete-region (point-min) (point-max))
         (insert saved-text)
         (set-buffer-modified-p modified-flag))
       (error (error-message-string err))))))

;;;###mh-autoload
(defun mh-mml-unsecure-message ()
  "Remove any secure message tags."
  (interactive)
  (if (not mh-pgp-support-flag)
      (error "Your version of Gnus does not support PGP/GPG")
    (mml-unsecure-message)))



;;; Support Routines for MH-Letter Commands

;;;###mh-autoload
(defun mh-mml-tag-present-p ()
  "Check if the current buffer has text which may be a MML tag."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward
     (concat
      "\\(<#\\(mml\\|part\\)\\(.\\|\n\\)*>[ \n\t]*<#/\\(mml\\|part\\)>\\|"
      "^<#secure.+>$\\)")
     nil t)))

(defvar mh-media-type-regexp
  (concat (regexp-opt '("text" "image" "audio" "video" "application"
                        "multipart" "message") t)
          "/[-.+a-zA-Z0-9]+")
  "Regexp matching valid media types used in MIME attachment compositions.")

;;;###mh-autoload
(defun mh-mh-directive-present-p (&optional begin end)
  "Check if the text between BEGIN and END might be a MH-style directive.
The optional argument BEGIN defaults to the beginning of the
buffer, while END defaults to the end of the buffer."
  (unless begin (setq begin (point-min)))
  (unless end (setq end (point-max)))
  (save-excursion
    (block search-for-mh-directive
      (goto-char begin)
      (while (re-search-forward "^#" end t)
        (let ((s (buffer-substring-no-properties
                  (point) (mh-line-end-position))))
          (cond ((equal s ""))
                ((string-match "^forw[ \t\n]+" s)
                 (return-from search-for-mh-directive t))
                (t (let ((first-token (car (split-string s "[ \t;@]"))))
                     (when (and first-token
                                (string-match mh-media-type-regexp
                                              first-token))
                       (return-from search-for-mh-directive t)))))))
      nil)))

(defun mh-minibuffer-read-type (filename &optional default)
  "Return the content type associated with the given FILENAME.
If the \"file\" command exists and recognizes the given file,
then its value is returned\; otherwise, the user is prompted for
a type (see `mailcap-mime-types').
Optional argument DEFAULT is returned if a type isn't entered."
  (mailcap-parse-mimetypes)
  (let* ((default (or default
                      (mm-default-file-encoding filename)
                      "application/octet-stream"))
         (probed-type (mh-file-mime-type filename))
         (type (or (and (not (equal probed-type "application/octet-stream"))
                        probed-type)
                   (completing-read
                    (format "Content type (default %s): " default)
                    (mapcar 'list (mailcap-mime-types))))))
    (if (not (equal type ""))
        type
      default)))

;;;###mh-autoload
(defun mh-file-mime-type (filename)
  "Return MIME type of FILENAME from file command.
Returns nil if file command not on system."
  (cond
   ((not (mh-have-file-command))
    nil)                                ;no file command, exit now
   ((not (and (file-exists-p filename)
              (file-readable-p filename)))
    nil)                               ;no file or not readable, ditto
   (t
    (let ((tmp-buffer (get-buffer-create mh-temp-buffer)))
      (with-current-buffer tmp-buffer
        (unwind-protect
            (progn
              (call-process "file" nil '(t nil) nil "-b" "-i"
                            (expand-file-name filename))
              (goto-char (point-min))
              (if (not (re-search-forward mh-media-type-regexp nil t))
                  nil
                (mh-file-mime-type-substitute (match-string 0) filename)))
          (kill-buffer tmp-buffer)))))))

(defvar mh-file-mime-type-substitutions
  '(("application/msword" "\.xls" "application/ms-excel")
    ("application/msword" "\.ppt" "application/ms-powerpoint")
    ("text/plain" "\.vcf" "text/x-vcard")
    ("text/rtf" "\.rtf" "application/rtf")
    ("application/x-zip" "\.sxc" "application/vnd.sun.xml.calc")
    ("application/x-zip" "\.sxd" "application/vnd.sun.xml.draw")
    ("application/x-zip" "\.sxi" "application/vnd.sun.xml.impress")
    ("application/x-zip" "\.sxw" "application/vnd.sun.xml.writer")
    ("application/x-zip" "\.odg" "application/vnd.oasis.opendocument.graphics")
    ("application/x-zip" "\.odi" "application/vnd.oasis.opendocument.image")
    ("application/x-zip" "\.odp"
     "application/vnd.oasis.opendocument.presentation")
    ("application/x-zip" "\.ods"
     "application/vnd.oasis.opendocument.spreadsheet")
    ("application/x-zip" "\.odt" "application/vnd.oasis.opendocument.text"))
  "Substitutions to make for Content-Type returned from file command.
The first element is the Content-Type returned by the file command.
The second element is a regexp matching the file name, usually the
extension.
The third element is the Content-Type to replace with.")

(defun mh-file-mime-type-substitute (content-type filename)
  "Return possibly changed CONTENT-TYPE on the FILENAME.
Substitutions are made from the `mh-file-mime-type-substitutions'
variable."
  (let ((subst mh-file-mime-type-substitutions)
        (type) (match) (answer content-type)
        (case-fold-search t))
    (while subst
      (setq type (car (car subst))
            match (elt (car subst) 1))
      (if (and (string-equal content-type type)
               (string-match match filename))
          (setq answer (elt (car subst) 2)
                subst nil)
        (setq subst (cdr subst))))
    answer))

(defvar mh-have-file-command 'undefined
  "Cached value of function `mh-have-file-command'.
Do not reference this variable directly as it might not have been
initialized. Always use the command `mh-have-file-command'.")

;;;###mh-autoload
(defun mh-have-file-command ()
  "Return t if 'file' command is on the system.
'file -i' is used to get MIME type of composition insertion."
  (when (eq mh-have-file-command 'undefined)
    (setq mh-have-file-command
          (and (fboundp 'executable-find)
               (executable-find "file") ; file command exists
                                        ;   and accepts -i and -b args.
               (zerop (call-process "file" nil nil nil "-i" "-b"
                                    (expand-file-name "inc" mh-progs))))))
  mh-have-file-command)



;;; MIME Cleanup

;;;###mh-autoload
(defun mh-mime-cleanup ()
  "Free the decoded MIME parts."
  (let ((mime-data (gethash (current-buffer) mh-globals-hash)))
    ;; This is for Emacs, what about XEmacs?
    (mh-funcall-if-exists remove-images (point-min) (point-max))
    (when mime-data
      (mh-mm-destroy-parts (mh-mime-handles mime-data))
      (remhash (current-buffer) mh-globals-hash))))

;;;###mh-autoload
(defun mh-destroy-postponed-handles ()
  "Free MIME data for externally displayed MIME parts."
  (let ((mime-data (mh-buffer-data)))
    (when mime-data
      (mh-mm-destroy-parts (mh-mime-handles mime-data)))
    (remhash (current-buffer) mh-globals-hash)))

(provide 'mh-mime)

;; Local Variables:
;; indent-tabs-mode: nil
;; sentence-end-double-space: nil
;; End:

;;; mh-mime.el ends here
