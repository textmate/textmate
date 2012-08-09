;;; mh-tool-bar.el --- MH-E tool bar support

;; Copyright (C) 2002-2003, 2005-2012  Free Software Foundation, Inc.

;; Author: Satyaki Das <satyaki@theforce.stanford.edu>
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
(mh-do-in-gnu-emacs
  (require 'tool-bar))
(mh-do-in-xemacs
  (require 'toolbar))

;;; Tool Bar Commands

(defun mh-tool-bar-search (&optional arg)
  "Interactively call `mh-tool-bar-search-function'.
Optional argument ARG is not used."
  (interactive "P")
  (call-interactively mh-tool-bar-search-function))

(defun mh-tool-bar-customize ()
  "Call `mh-customize' from the tool bar."
  (interactive)
  (mh-customize t))

(defun mh-tool-bar-folder-help ()
  "Visit \"(mh-e)Top\"."
  (interactive)
  (info "(mh-e)Top")
  (delete-other-windows))

(defun mh-tool-bar-letter-help ()
  "Visit \"(mh-e)Editing Drafts\"."
  (interactive)
  (info "(mh-e)Editing Drafts")
  (delete-other-windows))

(defmacro mh-tool-bar-reply-generator (function recipient folder-buffer-flag)
  "Generate FUNCTION that replies to RECIPIENT.
If FOLDER-BUFFER-FLAG is nil then the function generated...
When INCLUDE-FLAG is non-nil, include message body being replied to."
  `(defun ,function (&optional arg)
     ,(format "Reply to \"%s\".\nWhen ARG is non-nil include message in reply."
              recipient)
     (interactive "P")
     ,(if folder-buffer-flag nil '(set-buffer mh-show-folder-buffer))
     (mh-reply (mh-get-msg-num nil) ,recipient arg)))

(mh-tool-bar-reply-generator mh-tool-bar-reply-from "from" t)
(mh-tool-bar-reply-generator mh-show-tool-bar-reply-from "from" nil)
(mh-tool-bar-reply-generator mh-tool-bar-reply-to "to" t)
(mh-tool-bar-reply-generator mh-show-tool-bar-reply-to "to" nil)
(mh-tool-bar-reply-generator mh-tool-bar-reply-all "all" t)
(mh-tool-bar-reply-generator mh-show-tool-bar-reply-all "all" nil)



;;; Tool Bar Creation

;; Shush compiler.
(defvar image-load-path)

(defmacro mh-tool-bar-define (defaults &rest buttons)
  "Define a tool bar for MH-E.
DEFAULTS is the list of buttons that are present by default. It
is a list of lists where the sublists are of the following form:

  (:KEYWORD FUNC1 FUNC2 FUNC3 ...)

Here :KEYWORD is one of :folder or :letter. If it is :folder then
the default buttons in the folder and show mode buffers are being
specified. If it is :letter then the default buttons in the
letter mode are listed. FUNC1, FUNC2, FUNC3, ... are the names of
the functions that the buttons would execute.

Each element of BUTTONS is a list consisting of four mandatory
items and one optional item as follows:

  (FUNCTION MODES ICON DOC &optional ENABLE-EXPR)

where,

  FUNCTION is the name of the function that will be executed when
  the button is clicked.

  MODES is a list of symbols. List elements must be from \"folder\",
  \"letter\" and \"sequence\". If \"folder\" is present then the button is
  available in the folder and show buffer. If the name of FUNCTION is
  of the form \"mh-foo\", where foo is some arbitrary string, then we
  check if the function `mh-show-foo' exists. If it exists then that
  function is used in the show buffer. Otherwise the original function
  `mh-foo' is used in the show buffer as well. Presence of \"sequence\"
  is handled similar to the above. The only difference is that the
  button is shown only when the folder is narrowed to a sequence. If
  \"letter\" is present in MODES, then the button is available during
  draft editing and runs FUNCTION when clicked.

  ICON is the icon that is drawn in the button.

  DOC is the documentation for the button. It is used in tool-tips and
  in providing other help to the user. GNU Emacs uses only the first
  line of the string. So the DOC should be formatted such that the
  first line is useful and complete without the rest of the string.

  Optional item ENABLE-EXPR is an arbitrary lisp expression. If it
  evaluates to nil, then the button is inactive, otherwise it is
  active. If it isn't present then the button is always active."
  ;; The following variable names have been carefully chosen to make code
  ;; generation easier. Modifying the names should be done carefully.
  (let (folder-buttons folder-docs folder-button-setter sequence-button-setter
                       show-buttons show-button-setter show-seq-button-setter
                       letter-buttons letter-docs letter-button-setter
                       folder-defaults letter-defaults
                       folder-vectors show-vectors letter-vectors)
    (dolist (x defaults)
      (cond ((eq (car x) :folder) (setq folder-defaults (cdr x)))
            ((eq (car x) :letter) (setq letter-defaults (cdr x)))))
    (dolist (button buttons)
      (unless (and (listp button)
                   (or (equal (length button) 4) (equal (length button) 5)))
        (error "Incorrect MH-E tool-bar button specification: %s" button))
      (let* ((name (nth 0 button))
             (name-str (symbol-name name))
             (icon (nth 2 button))
             (xemacs-icon (mh-do-in-xemacs
                            `(cdr (assoc (quote ,(intern icon)) mh-xemacs-icon-map))))
             (full-doc (nth 3 button))
             (doc (if (string-match "\\(.*\\)\n" full-doc)
                      (match-string 1 full-doc)
                    full-doc))
             (enable-expr (if (eql (length button) 4) t (nth 4 button)))
             (modes (nth 1 button))
             functions show-sym)
        (when (memq 'letter modes) (setq functions `(:letter ,name)))
        (when (or (memq 'folder modes) (memq 'sequence modes))
          (setq functions
                (append `(,(if (memq 'folder modes) :folder :sequence) ,name)
                        functions))
          (setq show-sym
                (if (string-match "^mh-\\(.*\\)$" name-str)
                    (intern (concat "mh-show-" (match-string 1 name-str)))
                  name))
          (setq functions
                (append `(,(if (memq 'folder modes) :show :show-seq)
                          ,(if (fboundp show-sym) show-sym name))
                        functions)))
        (do ((functions functions (cddr functions)))
            ((null functions))
          (let* ((type (car functions))
                 (function (cadr functions))
                 (type1 (substring (symbol-name type) 1))
                 (vector-list (cond ((eq type :show) 'show-vectors)
                                    ((eq type :show-seq) 'show-vectors)
                                    ((eq type :letter) 'letter-vectors)
                                    (t 'folder-vectors)))
                 (list (cond ((eq type :letter) 'mh-tool-bar-letter-buttons)
                             (t 'mh-tool-bar-folder-buttons)))
                 (key (intern (concat "mh-" type1 "-tool-bar-" name-str)))
                 (setter (intern (concat type1 "-button-setter")))
                 (mbuttons (cond ((eq type :letter) 'letter-buttons)
                                 ((eq type :show) 'show-buttons)
                                 ((eq type :show-seq) 'show-buttons)
                                 (t 'folder-buttons)))
                 (docs (cond ((eq mbuttons 'letter-buttons) 'letter-docs)
                             ((eq mbuttons 'folder-buttons) 'folder-docs))))
            (add-to-list vector-list `(vector ,xemacs-icon ',function t ,full-doc))
            (add-to-list
             setter `(when (member ',name ,list)
                       (mh-funcall-if-exists
                        tool-bar-add-item ,icon ',function ',key
                        :help ,doc :enable ',enable-expr)))
            (add-to-list mbuttons name)
            (if docs (add-to-list docs doc))))))
    (setq folder-buttons (nreverse folder-buttons)
          letter-buttons (nreverse letter-buttons)
          show-buttons (nreverse show-buttons)
          letter-docs (nreverse letter-docs)
          folder-docs (nreverse folder-docs)
          folder-vectors (nreverse folder-vectors)
          show-vectors (nreverse show-vectors)
          letter-vectors (nreverse letter-vectors))
    (dolist (x folder-defaults)
      (unless (memq x folder-buttons)
        (error "Folder defaults contains unknown button %s" x)))
    (dolist (x letter-defaults)
      (unless (memq x letter-buttons)
        (error "Letter defaults contains unknown button %s" x)))
    `(eval-when (compile load eval)
       ;; GNU Emacs tool bar specific code
       (mh-do-in-gnu-emacs
         (defun mh-buffer-exists-p (mode)
           "Test whether a buffer with major mode MODE is present."
           (loop for buf in (buffer-list)
                 when (with-current-buffer buf
                        (eq major-mode mode))
                 return t))
         ;; Tool bar initialization functions
         (defun mh-tool-bar-folder-buttons-init ()
           (when (mh-buffer-exists-p 'mh-folder-mode)
             (let* ((load-path (mh-image-load-path-for-library "mh-e"
                                                               "mh-logo.xpm"))
                    (image-load-path (cons (car load-path)
                                           (when (boundp 'image-load-path)
                                             image-load-path))))
               (setq mh-folder-tool-bar-map
                     (let ((tool-bar-map (make-sparse-keymap)))
                       ,@(nreverse folder-button-setter)
                       tool-bar-map))
               (setq mh-folder-seq-tool-bar-map
                     (let ((tool-bar-map (copy-keymap mh-folder-tool-bar-map)))
                       ,@(nreverse sequence-button-setter)
                       tool-bar-map))
               (setq mh-show-tool-bar-map
                     (let ((tool-bar-map (make-sparse-keymap)))
                       ,@(nreverse show-button-setter)
                       tool-bar-map))
               (setq mh-show-seq-tool-bar-map
                     (let ((tool-bar-map (copy-keymap mh-show-tool-bar-map)))
                       ,@(nreverse show-seq-button-setter)
                       tool-bar-map)))))
         (defun mh-tool-bar-letter-buttons-init ()
           (when (mh-buffer-exists-p 'mh-letter-mode)
             (let* ((load-path (mh-image-load-path-for-library "mh-e"
                                                               "mh-logo.xpm"))
                    (image-load-path (cons (car load-path)
                                           (when (boundp 'image-load-path)
                                             image-load-path))))
               (setq mh-letter-tool-bar-map
                     (let ((tool-bar-map (make-sparse-keymap)))
                       ,@(nreverse letter-button-setter)
                       tool-bar-map)))))
         ;; Custom setter functions
         (defun mh-tool-bar-update (mode default-map sequence-map)
           "Update `tool-bar-map' in all buffers of MODE.
Use SEQUENCE-MAP if display is limited; DEFAULT-MAP otherwise."
           (loop for buf in (buffer-list)
                 do (with-current-buffer buf
                      (if (eq mode major-mode)
                          (let ((map (if mh-folder-view-stack
                                         sequence-map
                                       default-map)))
                            ;; Yes, make-local-variable is necessary since we
                            ;; get here during initialization when loading
                            ;; mh-e.el, after the +inbox buffer has been
                            ;; created, but before mh-folder-mode has run and
                            ;; created the local map.
                            (set (make-local-variable 'tool-bar-map) map))))))
         (defun mh-tool-bar-folder-buttons-set (symbol value)
           "Construct tool bar for `mh-folder-mode' and `mh-show-mode'."
           (set-default symbol value)
           (mh-tool-bar-folder-buttons-init)
           (mh-tool-bar-update 'mh-folder-mode mh-folder-tool-bar-map
                               mh-folder-seq-tool-bar-map)
           (mh-tool-bar-update 'mh-show-mode mh-show-tool-bar-map
                               mh-show-seq-tool-bar-map))
         (defun mh-tool-bar-letter-buttons-set (symbol value)
           "Construct tool bar for `mh-letter-mode'."
           (set-default symbol value)
           (mh-tool-bar-letter-buttons-init)
           (mh-tool-bar-update 'mh-letter-mode mh-letter-tool-bar-map
                               mh-letter-tool-bar-map)))
       ;; XEmacs specific code
       (mh-do-in-xemacs
         (defvar mh-tool-bar-folder-vector-map
           (list ,@(loop for button in folder-buttons
                     for vector in folder-vectors
                     collect `(cons ',button ,vector))))
         (defvar mh-tool-bar-show-vector-map
           (list ,@(loop for button in show-buttons
                     for vector in show-vectors
                     collect `(cons ',button ,vector))))
         (defvar mh-tool-bar-letter-vector-map
           (list ,@(loop for button in letter-buttons
                     for vector in letter-vectors
                     collect `(cons ',button ,vector))))
         (defvar mh-tool-bar-folder-buttons)
         (defvar mh-tool-bar-show-buttons)
         (defvar mh-tool-bar-letter-buttons)
         ;; Custom setter functions
         (defun mh-tool-bar-letter-buttons-set (symbol value)
           (set-default symbol value)
           (when mh-xemacs-has-tool-bar-flag
             (setq mh-tool-bar-letter-buttons
                   (loop for b in value
                         collect (cdr
                                  (assoc b mh-tool-bar-letter-vector-map))))))
         (defun mh-tool-bar-folder-buttons-set (symbol value)
           (set-default symbol value)
           (when mh-xemacs-has-tool-bar-flag
             (setq mh-tool-bar-folder-buttons
                   (loop for b in value
                         collect (cdr (assoc b mh-tool-bar-folder-vector-map))))
             (setq mh-tool-bar-show-buttons
                   (loop for b in value
                         collect (cdr (assoc b mh-tool-bar-show-vector-map))))))
         (defun mh-tool-bar-init (mode)
           "Install tool bar in MODE."
           (when mh-xemacs-use-tool-bar-flag
             (let ((tool-bar (cond ((eq mode :folder)
                                    mh-tool-bar-folder-buttons)
                                   ((eq mode :letter)
                                    mh-tool-bar-letter-buttons)
                                   ((eq mode :show)
                                    mh-tool-bar-show-buttons)))
                   (height 37)
                   (width 40)
                   (buffer (current-buffer)))
               (cond
                ((eq mh-xemacs-tool-bar-position 'top)
                 (set-specifier top-toolbar tool-bar buffer)
                 (set-specifier top-toolbar-visible-p t)
                 (set-specifier top-toolbar-height height))
                ((eq mh-xemacs-tool-bar-position 'bottom)
                 (set-specifier bottom-toolbar tool-bar buffer)
                 (set-specifier bottom-toolbar-visible-p t)
                 (set-specifier bottom-toolbar-height height))
                ((eq mh-xemacs-tool-bar-position 'left)
                 (set-specifier left-toolbar tool-bar buffer)
                 (set-specifier left-toolbar-visible-p t)
                 (set-specifier left-toolbar-width width))
                ((eq mh-xemacs-tool-bar-position 'right)
                 (set-specifier right-toolbar tool-bar buffer)
                 (set-specifier right-toolbar-visible-p t)
                 (set-specifier right-toolbar-width width))
                (t (set-specifier default-toolbar tool-bar buffer)))))))
       ;; Declare customizable tool bars
       (custom-declare-variable
        'mh-tool-bar-folder-buttons
        '(list ,@(mapcar (lambda (x) `(quote ,x)) folder-defaults))
        "List of buttons to include in MH-Folder tool bar."
        :group 'mh-tool-bar
        :set 'mh-tool-bar-folder-buttons-set
        :type '(set ,@(loop for x in folder-buttons
                            for y in folder-docs
                            collect `(const :tag ,y ,x)))
        ;;:package-version '(MH-E "7.1")
        )
       (custom-declare-variable
        'mh-tool-bar-letter-buttons
        '(list ,@(mapcar (lambda (x) `(quote ,x)) letter-defaults))
        "List of buttons to include in MH-Letter tool bar."
        :group 'mh-tool-bar
        :set 'mh-tool-bar-letter-buttons-set
        :type '(set ,@(loop for x in letter-buttons
                            for y in letter-docs
                            collect `(const :tag ,y ,x)))
        ;;:package-version '(MH-E "7.1")
        ))))

;; The icon names are duplicated in the Makefile and mh-xemacs.el.
(mh-tool-bar-define
 ((:folder mh-inc-folder mh-mime-save-parts
           mh-previous-undeleted-msg mh-page-msg
           mh-next-undeleted-msg mh-delete-msg mh-refile-msg
           mh-undo mh-execute-commands mh-toggle-tick mh-reply
           mh-alias-grab-from-field mh-send mh-rescan-folder
           mh-tool-bar-search mh-visit-folder
           mh-tool-bar-customize mh-tool-bar-folder-help
           mh-widen)
  (:letter mh-send-letter save-buffer mh-fully-kill-draft
           mh-compose-insertion ispell-message undo
           clipboard-kill-region clipboard-kill-ring-save
           clipboard-yank mh-tool-bar-customize
           mh-tool-bar-letter-help))
 ;; Folder/Show buffer buttons
 (mh-inc-folder (folder) "mail/inbox" "Incorporate new mail in Inbox
This button runs `mh-inc-folder' which drags any
new mail into your Inbox folder")
 (mh-mime-save-parts (folder) "attach" "Save MIME parts from this message
This button runs `mh-mime-save-parts' which saves a message's
different parts into separate files")
 (mh-previous-undeleted-msg (folder) "left-arrow"
                            "Go to the previous undeleted message
This button runs `mh-previous-undeleted-msg'")
 (mh-page-msg (folder) "next-page" "Page the current message forwards
This button runs `mh-page-msg'")
 (mh-next-undeleted-msg (folder) "right-arrow" "Go to the next undeleted message
The button runs `mh-next-undeleted-msg'")
 (mh-delete-msg (folder) "delete" "Mark this message for deletion
This button runs `mh-delete-msg'")
 (mh-refile-msg (folder) "mail/move" "Refile this message
This button runs `mh-refile-msg'")
 (mh-undo (folder) "undo" "Undo last operation
This button runs `undo'"
          (mh-outstanding-commands-p))
 (mh-execute-commands (folder) "data-save" "Perform moves and deletes
This button runs `mh-execute-commands'"
                      (mh-outstanding-commands-p))
 (mh-toggle-tick (folder) "mail/flag-for-followup" "Toggle tick mark
This button runs `mh-toggle-tick'")
 (mh-toggle-showing (folder) "show" "Toggle showing message
This button runs `mh-toggle-showing'")
 (mh-reply (folder) "mail/reply" "Reply to this message
This button runs `mh-reply'")
 (mh-tool-bar-reply-from (folder) "mail/reply-from" "Reply to \"from\"")
 (mh-tool-bar-reply-to (folder) "mail/reply-to" "Reply to \"to\"")
 (mh-tool-bar-reply-all (folder) "mail/reply-all" "Reply to \"all\"")
 (mh-alias-grab-from-field (folder) "contact" "Create alias for sender
This button runs `mh-alias-grab-from-field'"
                           (and (mh-extract-from-header-value)
                                (not (mh-alias-for-from-p))))
 (mh-send (folder) "mail/compose" "Compose new message
This button runs `mh-send'")
 (mh-rescan-folder (folder) "refresh" "Rescan this folder
This button runs `mh-rescan-folder'")
 (mh-pack-folder (folder) "mail/repack" "Repack this folder
This button runs `mh-pack-folder'")
 (mh-tool-bar-search (folder) "search" "Search
This button runs `mh-tool-bar-search-function'")
 (mh-visit-folder (folder) "open" "Visit other folder
This button runs `mh-visit-folder'")
 ;; Letter buffer buttons
 (mh-send-letter (letter) "mail/send" "Send this letter")
 (save-buffer (letter) "save" "Save current buffer to its file"
              (buffer-modified-p))
 (mh-fully-kill-draft (letter) "delete" "Kill this draft")
 (mh-compose-insertion (letter) "attach" "Insert attachment")
 (ispell-message (letter) "spell" "Check spelling")
 (undo (letter) "undo" "Undo last operation")
 (clipboard-kill-region (letter) "cut"
  "Cut (kill) text in region")
 (clipboard-kill-ring-save (letter) "copy"
  "Copy text in region")
 (clipboard-yank (letter) "paste"
  "Paste (yank) text cut or copied earlier")
 ;; Common buttons
 (mh-tool-bar-customize (folder letter) "preferences" "MH-E Preferences")
 (mh-tool-bar-folder-help (folder) "help" "Help! (general help)
This button runs `info'")
 (mh-tool-bar-letter-help (letter) "help" "Help! (general help)
This button runs `info'")
 ;; Folder narrowed to sequence buttons
 (mh-widen (sequence) "zoom-out" "Widen from the sequence
This button runs `mh-widen'"))

(provide 'mh-tool-bar)

;; Local Variables:
;; indent-tabs-mode: nil
;; sentence-end-double-space: nil
;; End:

;;; mh-tool-bar.el ends here
