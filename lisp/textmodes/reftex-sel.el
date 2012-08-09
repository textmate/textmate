;;; reftex-sel.el --- the selection modes for RefTeX

;; Copyright (C) 1997-2012 Free Software Foundation, Inc.

;; Author: Carsten Dominik <dominik@science.uva.nl>
;; Maintainer: auctex-devel@gnu.org
;; Version: 4.31
;; Package: reftex

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

;;; Code:

(eval-when-compile (require 'cl))
(provide 'reftex-sel)
(require 'reftex)
;;;

;; Common bindings in reftex-select-label-mode-map
;; and reftex-select-bib-mode-map.
(defvar reftex-select-shared-map
  (let ((map (make-sparse-keymap)))
    (substitute-key-definition
     'next-line 'reftex-select-next                      map global-map)
    (substitute-key-definition
     'previous-line 'reftex-select-previous              map global-map)
    (substitute-key-definition
     'keyboard-quit 'reftex-select-keyboard-quit         map global-map)
    (substitute-key-definition
     'newline 'reftex-select-accept                      map global-map)

    (loop for x in
          '((" "        . reftex-select-callback)
            ("n"        . reftex-select-next)
            ([(down)]   . reftex-select-next)
            ("p"        . reftex-select-previous)
            ([(up)]     . reftex-select-previous)
            ("f"        . reftex-select-toggle-follow)
            ("\C-m"     . reftex-select-accept)
            ([(return)] . reftex-select-accept)
            ("q"        . reftex-select-quit)
            ("."        . reftex-select-show-insertion-point)
            ("?"        . reftex-select-help))
          do (define-key map (car x) (cdr x)))

    ;; The mouse-2 binding
    (if (featurep 'xemacs)
        (define-key map [(button2)] 'reftex-select-mouse-accept)
      (define-key map [(mouse-2)] 'reftex-select-mouse-accept)
      (define-key map [follow-link] 'mouse-face))


    ;; Digit arguments
    (loop for key across "0123456789" do
          (define-key map (vector (list key)) 'digit-argument))
    (define-key map "-" 'negative-argument)
    map))

(defvar reftex-select-label-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map reftex-select-shared-map)

    (loop for key across "aAcgFlrRstx#%" do
          (define-key map (vector (list key))
            (list 'lambda '()
                  "Press `?' during selection to find out about this key."
                  '(interactive) (list 'throw '(quote myexit) key))))

    (loop for x in
          '(("b"        . reftex-select-jump-to-previous)
            ("z"        . reftex-select-jump)
            ("v"        . reftex-select-toggle-varioref)
            ("V"        . reftex-select-toggle-fancyref)
            ("m"        . reftex-select-mark)
            ("u"        . reftex-select-unmark)
            (","        . reftex-select-mark-comma)
            ("-"        . reftex-select-mark-to)
            ("+"        . reftex-select-mark-and)
            ([(tab)]    . reftex-select-read-label)
            ("\C-i"     . reftex-select-read-label)
            ("\C-c\C-n" . reftex-select-next-heading)
            ("\C-c\C-p" . reftex-select-previous-heading))
          do
          (define-key map (car x) (cdr x)))

    map)
  "Keymap used for *RefTeX Select* buffer, when selecting a label.
This keymap can be used to configure the label selection process which is
started with the command \\[reftex-reference].")
(define-obsolete-variable-alias
  'reftex-select-label-map 'reftex-select-label-mode-map "24.1")

(define-derived-mode reftex-select-label-mode fundamental-mode "LSelect"
  "Major mode for selecting a label in a LaTeX document.
This buffer was created with RefTeX.
It only has a meaningful keymap when you are in the middle of a
selection process.
To select a label, move the cursor to it and press RET.
Press `?' for a summary of important key bindings.

During a selection process, these are the local bindings.

\\{reftex-select-label-mode-map}"
  (when (featurep 'xemacs)
    ;; XEmacs needs the call to make-local-hook
    (make-local-hook 'pre-command-hook)
    (make-local-hook 'post-command-hook))
  (set (make-local-variable 'reftex-select-marked) nil)
  (when (syntax-table-p reftex-latex-syntax-table)
    (set-syntax-table reftex-latex-syntax-table))
  ;; We do not set a local map - reftex-select-item does this.
  )

(defvar reftex-select-bib-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map reftex-select-shared-map)

    (loop for key across "grRaAeE" do
          (define-key map (vector (list key))
            (list 'lambda '()
                  "Press `?' during selection to find out about this key."
                  '(interactive) (list 'throw '(quote myexit) key))))

    (loop for x in
          '(("\C-i"  . reftex-select-read-cite)
            ([(tab)] . reftex-select-read-cite)
            ("m"     . reftex-select-mark)
            ("u"     . reftex-select-unmark))
          do (define-key map (car x) (cdr x)))

    map)
  "Keymap used for *RefTeX Select* buffer, when selecting a BibTeX entry.
This keymap can be used to configure the BibTeX selection process which is
started with the command \\[reftex-citation].")
(define-obsolete-variable-alias
  'reftex-select-bib-map 'reftex-select-bib-mode-map "24.1")

(define-derived-mode reftex-select-bib-mode fundamental-mode "BSelect"
  "Major mode for selecting a citation key in a LaTeX document.
This buffer was created with RefTeX.
It only has a meaningful keymap when you are in the middle of a
selection process.
In order to select a citation, move the cursor to it and press RET.
Press `?' for a summary of important key bindings.

During a selection process, these are the local bindings.

\\{reftex-select-label-mode-map}"
  (when (featurep 'xemacs)
    ;; XEmacs needs the call to make-local-hook
    (make-local-hook 'pre-command-hook)
    (make-local-hook 'post-command-hook))
  (set (make-local-variable 'reftex-select-marked) nil)
  ;; We do not set a local map - reftex-select-item does this.
  )

;; (defun reftex-get-offset (buf here-am-I &optional typekey toc index file)
;;   ;; Find the correct offset data, like insert-docstruct would, but faster.
;;   ;; Buffer BUF knows the correct docstruct to use.
;;   ;; Basically this finds the first docstruct entry after HERE-I-AM which
;;   ;; is of allowed type.  The optional arguments specify what is allowed.
;;   (catch 'exit
;;     (with-current-buffer buf
;;       (reftex-access-scan-info)
;;       (let* ((rest (memq here-am-I (symbol-value reftex-docstruct-symbol)))
;;          entry)
;;     (while (setq entry (pop rest))
;;       (if (or (and typekey
;;                    (stringp (car entry))
;;                    (or (equal typekey " ")
;;                        (equal typekey (nth 1 entry))))
;;               (and toc (eq (car entry) 'toc))
;;               (and index (eq (car entry) 'index))
;;               (and file
;;                    (memq (car entry) '(bof eof file-error))))
;;           (throw 'exit entry)))
;;     nil))))

(defun reftex-get-offset (buf here-am-I &optional typekey toc index file)
  ;; Find the correct offset data, like insert-docstruct would, but faster.
  ;; Buffer BUF knows the correct docstruct to use.
  ;; Basically this finds the first docstruct entry before HERE-I-AM which
  ;; is of allowed type.  The optional arguments specify what is allowed.
  (catch 'exit
    (with-current-buffer buf
      (reftex-access-scan-info)
      (let* ((rest (symbol-value reftex-docstruct-symbol))
             lastentry entry)
        (while (setq entry (pop rest))
          (if (or (and typekey
                       (stringp (car entry))
                       (or (equal typekey " ")
                           (equal typekey (nth 1 entry))))
                  (and toc (eq (car entry) 'toc))
                  (and index (eq (car entry) 'index))
                  (and file
                       (memq (car entry) '(bof eof file-error))))
              (setq lastentry entry))
          (if (eq entry here-am-I)
              (throw 'exit (or lastentry entry))))
        nil))))

(defun reftex-insert-docstruct
  (buf toc labels index-entries files context counter show-commented
            here-I-am xr-prefix toc-buffer)
  ;; Insert an excerpt of the docstruct list.
  ;; Return the data property of the entry corresponding to HERE-I-AM.
  ;; BUF is the buffer which has the correct docstruct-symbol.
  ;; LABELS non-nil means to include labels into the list.
  ;;        When a string, indicates the label type to include
  ;; FILES non-nil means to display file boundaries.
  ;; CONTEXT non-nil means to include label context.
  ;; COUNTER means to count the labels.
  ;; SHOW-COMMENTED means to include also labels which are commented out.
  ;; HERE-I-AM is a member of the docstruct list.  The function will return
  ;;           a used member near to this one, as a possible starting point.
  ;; XR-PREFIX is the prefix to put in front of labels.
  ;; TOC-BUFFER means this is to fill the toc buffer.
  (let* ((font (reftex-use-fonts))
         (cnt 0)
         (index -1)
         (toc-indent " ")
         (label-indent
          (concat "> "
                  (if toc (make-string (* 7 reftex-level-indent) ?\ ) "")))
         (context-indent
          (concat ".   "
                  (if toc (make-string (* 7 reftex-level-indent) ?\ ) "")))
         (mouse-face
          (if (memq reftex-highlight-selection '(mouse both))
              reftex-mouse-selected-face
            nil))
         (label-face (reftex-verified-face reftex-label-face
                                           'font-lock-constant-face
                                           'font-lock-reference-face))
         (index-face (reftex-verified-face reftex-index-face
                                           'font-lock-constant-face
                                           'font-lock-reference-face))
         all cell text label typekey note comment master-dir-re
         prev-inserted offset from to index-tag docstruct-symbol)

    ;; Pop to buffer buf to get the correct buffer-local variables
    (with-current-buffer buf

      ;; Ensure access to scanning info
      (reftex-access-scan-info)

      (setq docstruct-symbol reftex-docstruct-symbol
            all (symbol-value reftex-docstruct-symbol)
            reftex-active-toc nil
            master-dir-re
            (concat "\\`" (regexp-quote
                           (file-name-directory (reftex-TeX-master-file))))))

    (set (make-local-variable 'reftex-docstruct-symbol) docstruct-symbol)
    (set (make-local-variable 'reftex-prefix)
         (cdr (assoc labels reftex-typekey-to-prefix-alist)))
    (if (equal reftex-prefix " ") (setq reftex-prefix nil))

    ;; Walk the docstruct and insert the appropriate stuff
    (while (setq cell (pop all))

      (incf index)
      (setq from (point))

      (cond

       ((memq (car cell) '(bib thebib label-numbers appendix
                               master-dir bibview-cache is-multi xr xr-doc)))
       ;; These are currently ignored

       ((memq (car cell) '(bof eof file-error))
        ;; Beginning or end of a file
        (when files
          (setq prev-inserted cell)
;         (if (eq offset 'attention) (setq offset cell))
          (insert
           " File " (if (string-match master-dir-re (nth 1 cell))
                   (substring (nth 1 cell) (match-end 0))
                 (nth 1 cell))
           (cond ((eq (car cell) 'bof) " starts here\n")
                 ((eq (car cell) 'eof) " ends here\n")
                 ((eq (car cell) 'file-error) " was not found\n")))
          (setq to (point))
          (when font
            (put-text-property from to
                               'face reftex-file-boundary-face))
          (when toc-buffer
            (if mouse-face
                (put-text-property from (1- to)
                                   'mouse-face mouse-face))
            (put-text-property from to :data cell))))

       ((eq (car cell) 'toc)
        ;; a table of contents entry
        (when (and toc
                   (<= (nth 5 cell) reftex-toc-max-level))
          (setq prev-inserted cell)
;         (if (eq offset 'attention) (setq offset cell))
          (setq reftex-active-toc cell)
          (insert (concat toc-indent (nth 2 cell) "\n"))
          (setq to (point))
          (when font
            (put-text-property from to
                               'face reftex-section-heading-face))
          (when toc-buffer
            (if mouse-face
                (put-text-property from (1- to)
                                   'mouse-face mouse-face))
            (put-text-property from to :data cell))
          (goto-char to)))

       ((stringp (car cell))
        ;; a label
        (when (null (nth 2 cell))
          ;; No context yet.  Quick update.
          (setcdr cell (cdr (reftex-label-info-update cell)))
          (put docstruct-symbol 'modified t))

        (setq label   (car cell)
              typekey (nth 1 cell)
              text    (nth 2 cell)
              comment (nth 4 cell)
              note    (nth 5 cell))

        (when (and labels
                   (or (eq labels t)
                       (string= typekey labels)
                       (string= labels " "))
                   (or show-commented (null comment)))

          ;; Yes we want this one
          (incf cnt)
          (setq prev-inserted cell)
;         (if (eq offset 'attention) (setq offset cell))

          (setq label (concat xr-prefix label))
          (when comment (setq label (concat "% " label)))
          (insert label-indent label)
          (when font
            (setq to (point))
            (put-text-property
             (- (point) (length label)) to
             'face (if comment
                       'font-lock-comment-face
                     label-face))
            (goto-char to))

          (insert (if counter (format " (%d) " cnt) "")
                  (if comment " LABEL IS COMMENTED OUT " "")
                  (if (stringp note) (concat "  " note) "")
                  "\n")
          (setq to (point))

          (when context
            (insert context-indent text "\n")
            (setq to (point)))
          (put-text-property from to :data cell)
          (when mouse-face
            (put-text-property from (1- to)
                               'mouse-face mouse-face))
          (goto-char to)))

       ((eq (car cell) 'index)
        ;; index entry
        (when (and index-entries
                   (or (eq t index-entries)
                       (string= index-entries (nth 1 cell))))
          (setq prev-inserted cell)
;         (if (eq offset 'attention) (setq offset cell))
          (setq index-tag (format "<%s>" (nth 1 cell)))
          (and font
               (put-text-property 0 (length index-tag)
                                  'face reftex-index-tag-face index-tag))
          (insert label-indent index-tag " " (nth 7 cell))

          (when font
            (setq to (point))
            (put-text-property
             (- (point) (length (nth 7 cell))) to
             'face index-face)
            (goto-char to))
          (insert "\n")
          (setq to (point))

          (when context
            (insert context-indent (nth 2 cell) "\n")
            (setq to (point)))
          (put-text-property from to :data cell)
          (when mouse-face
            (put-text-property from (1- to)
                               'mouse-face mouse-face))
          (goto-char to))))

      (if (eq cell here-I-am)
          (setq offset 'attention))
      (if (and prev-inserted (eq offset 'attention))
          (setq offset prev-inserted))
      )

    (when (reftex-refontify)
      ;; we need to fontify the buffer
      (reftex-fontify-select-label-buffer buf))
    (run-hooks 'reftex-display-copied-context-hook)
    offset))

(defun reftex-find-start-point (fallback &rest locations)
  ;; Set point to the first available LOCATION.  When a LOCATION is a list,
  ;; search for such a :data text property.  When it is an integer,
  ;; use is as line number.  FALLBACK is a buffer position used if everything
  ;; else  fails.
  (catch 'exit
    (goto-char (point-min))
    (let (loc pos)
      (while locations
        (setq loc (pop locations))
        (cond
         ((null loc))
         ((listp loc)
          (setq pos (text-property-any (point-min) (point-max) :data loc))
          (when pos
            (goto-char pos)
            (throw 'exit t)))
         ((integerp loc)
          (when (<= loc (count-lines (point-min) (point-max)))
            (goto-char (point-min))
            (forward-line (1- loc))
            (throw 'exit t)))))
      (goto-char fallback))))

(defvar reftex-last-data nil)
(defvar reftex-last-line nil)
(defvar reftex-select-marked nil)

(defun reftex-select-item (reftex-select-prompt help-string keymap
                                  &optional offset
                                  call-back cb-flag)
  ;; Select an item, using REFTEX-SELECT-PROMPT.
  ;; The function returns a key indicating an exit status, along with a
  ;; data structure indicating which item was selected.
  ;; HELP-STRING contains help.  KEYMAP is a keymap with the available
  ;; selection commands.
  ;; OFFSET can be a label list item which will be selected at start.
  ;; When it is t, point will start out at the beginning of the buffer.
  ;; Any other value will cause restart where last selection left off.
  ;; When CALL-BACK is given, it is a function which is called with the index
  ;; of the element.
  ;; CB-FLAG is the initial value of that flag.
  (let (ev reftex-select-data last-data (selection-buffer (current-buffer)))

    (setq reftex-select-marked nil)

    (setq ev
          (catch 'myexit
            (save-window-excursion
              (setq truncate-lines t)

              ;; Find a good starting point
              (reftex-find-start-point
               (point-min) offset reftex-last-data reftex-last-line)
              (beginning-of-line 1)
              (set (make-local-variable 'reftex-last-follow-point) (point))

      (unwind-protect
          (progn
            (use-local-map keymap)
            (add-hook 'pre-command-hook 'reftex-select-pre-command-hook nil t)
            (add-hook 'post-command-hook 'reftex-select-post-command-hook nil t)
            (princ reftex-select-prompt)
            (set-marker reftex-recursive-edit-marker (point))
            ;; XEmacs does not run post-command-hook here
            (and (featurep 'xemacs) (run-hooks 'post-command-hook))
            (recursive-edit))

        (set-marker reftex-recursive-edit-marker nil)
        (with-current-buffer selection-buffer
          (use-local-map nil)
          (remove-hook 'pre-command-hook 'reftex-select-pre-command-hook t)
          (remove-hook 'post-command-hook
                       'reftex-select-post-command-hook t))
        ;; Kill the mark overlays
        (mapc (lambda (c) (reftex-delete-overlay (nth 1 c)))
              reftex-select-marked)))))

    (set (make-local-variable 'reftex-last-line)
         (+ (count-lines (point-min) (point)) (if (bolp) 1 0)))
    (set (make-local-variable 'reftex-last-data) last-data)
    (reftex-kill-buffer "*RefTeX Help*")
    (setq reftex-callback-fwd (not reftex-callback-fwd)) ;; ;-)))
    (message "")
    (list ev reftex-select-data last-data)))

;; The following variables are all bound dynamically in `reftex-select-item'.
;; The defvars are here only to silence the byte compiler.

(defvar found-list)
(defvar cb-flag)
(defvar reftex-select-data)
(defvar reftex-select-prompt)
(defvar last-data)
(defvar call-back)
(defvar help-string)

;; The selection commands

(defun reftex-select-pre-command-hook ()
  (reftex-unhighlight 1)
  (reftex-unhighlight 0))

(defun reftex-select-post-command-hook ()
  (let (b e)
    (setq reftex-select-data (get-text-property (point) :data))
    (setq last-data (or reftex-select-data last-data))

    (when (and reftex-select-data cb-flag
               (not (equal reftex-last-follow-point (point))))
      (setq reftex-last-follow-point (point))
      (funcall call-back reftex-select-data reftex-callback-fwd
               (not reftex-revisit-to-follow)))
    (if reftex-select-data
        (setq b (or (previous-single-property-change
                     (1+ (point)) :data)
                    (point-min))
              e (or (next-single-property-change
                     (point) :data)
                    (point-max)))
      (setq b (point) e (point)))
    (and (memq reftex-highlight-selection '(cursor both))
         (reftex-highlight 1 b e))
    (if (or (not (pos-visible-in-window-p b))
            (not (pos-visible-in-window-p e)))
        (recenter '(4)))
    (unless (current-message)
      (princ reftex-select-prompt))))

(defun reftex-select-next (&optional arg)
  "Move to next selectable item."
  (interactive "p")
  (setq reftex-callback-fwd t)
  (or (eobp) (forward-char 1))
  (re-search-forward "^[^. \t\n\r]" nil t arg)
  (beginning-of-line 1))
(defun reftex-select-previous (&optional arg)
  "Move to previous selectable item."
  (interactive "p")
  (setq reftex-callback-fwd nil)
  (re-search-backward "^[^. \t\n\r]" nil t arg))
(defun reftex-select-jump (arg)
  "Jump to a specific section.  E.g. '3 z' jumps to section 3.
Useful for large TOC's."
  (interactive "P")
  (goto-char (point-min))
  (re-search-forward
   (concat "^ *" (number-to-string (if (numberp arg) arg 1)) " ")
   nil t)
  (beginning-of-line))
(defun reftex-select-next-heading (&optional arg)
  "Move to next table of contents line."
  (interactive "p")
  (end-of-line)
  (re-search-forward "^ " nil t arg)
  (beginning-of-line))
(defun reftex-select-previous-heading (&optional arg)
  "Move to previous table of contents line."
  (interactive "p")
  (re-search-backward "^ " nil t arg))
(defun reftex-select-quit ()
  "Abort selection process."
  (interactive)
  (throw 'myexit nil))
(defun reftex-select-keyboard-quit ()
  "Abort selection process."
  (interactive)
  (throw 'exit t))
(defun reftex-select-jump-to-previous ()
  "Jump back to where previous selection process left off."
  (interactive)
  (let (pos)
    (cond
     ((and (local-variable-p 'reftex-last-data (current-buffer))
           reftex-last-data
           (setq pos (text-property-any (point-min) (point-max)
                                        :data reftex-last-data)))
      (goto-char pos))
     ((and (local-variable-p 'reftex-last-line (current-buffer))
           (integerp reftex-last-line))
      (goto-char (point-min))
      (forward-line (1- reftex-last-line)))
     (t (ding)))))
(defun reftex-select-toggle-follow ()
  "Toggle follow mode:  Other window follows with full context."
  (interactive)
  (setq reftex-last-follow-point -1)
  (setq cb-flag (not cb-flag)))

(defvar reftex-refstyle)                ; from reftex-reference

(defun reftex-select-toggle-varioref ()
  "Toggle the macro used for referencing the label between \\ref and \\vref."
  (interactive)
  (if (string= reftex-refstyle "\\ref")
      (setq reftex-refstyle "\\vref")
    (setq reftex-refstyle "\\ref"))
  (force-mode-line-update))
(defun reftex-select-toggle-fancyref ()
  "Toggle the macro used for referencing the label between \\ref and \\vref."
  (interactive)
  (setq reftex-refstyle
        (cond ((string= reftex-refstyle "\\ref") "\\fref")
              ((string= reftex-refstyle "\\fref") "\\Fref")
              (t "\\ref")))
  (force-mode-line-update))
(defun reftex-select-show-insertion-point ()
  "Show the point from where selection was started in another window."
  (interactive)
  (let ((this-window (selected-window)))
    (unwind-protect
        (progn
          (switch-to-buffer-other-window
           (marker-buffer reftex-select-return-marker))
          (goto-char (marker-position reftex-select-return-marker))
          (recenter '(4)))
      (select-window this-window))))
(defun reftex-select-callback ()
  "Show full context in another window."
  (interactive)
  (if reftex-select-data (funcall call-back reftex-select-data reftex-callback-fwd nil) (ding)))
(defun reftex-select-accept ()
  "Accept the currently selected item."
  (interactive)
  (throw 'myexit 'return))
(defun reftex-select-mouse-accept (ev)
  "Accept the item at the mouse click."
  (interactive "e")
  (mouse-set-point ev)
  (setq reftex-select-data (get-text-property (point) :data))
  (setq last-data (or reftex-select-data last-data))
  (throw 'myexit 'return))
(defun reftex-select-read-label ()
  "Use minibuffer to read a label to reference, with completion."
  (interactive)
  (let ((label (completing-read
                "Label: " (symbol-value reftex-docstruct-symbol)
                nil nil reftex-prefix)))
    (unless (or (equal label "") (equal label reftex-prefix))
      (throw 'myexit label))))
(defun reftex-select-read-cite ()
  "Use minibuffer to read a citation key with completion."
  (interactive)
  (let* ((key (completing-read "Citation key: " found-list))
         (entry (assoc key found-list)))
    (cond
     ((or (null key) (equal key "")))
     (entry
      (setq reftex-select-data entry)
      (setq last-data reftex-select-data)
      (throw 'myexit 'return))
     (t (throw 'myexit key)))))

(defun reftex-select-mark (&optional separator)
  "Mark the entry."
  (interactive)
  (let* ((data (get-text-property (point) :data))
         boe eoe ovl)
    (or data (error "No entry to mark at point"))
    (if (assq data reftex-select-marked)
        (error "Entry is already marked"))
    (setq boe (or (previous-single-property-change (1+ (point)) :data)
                  (point-min))
          eoe (or (next-single-property-change (point) :data) (point-max)))
    (setq ovl (reftex-make-overlay boe eoe))
    (push (list data ovl separator) reftex-select-marked)
    (reftex-overlay-put ovl 'face reftex-select-mark-face)
    (reftex-overlay-put ovl 'before-string
                        (if separator
                            (format "*%c%d* " separator
                                    (length reftex-select-marked))
                          (format "*%d*  " (length reftex-select-marked))))
    (message "Entry has mark no. %d" (length reftex-select-marked))))

(defun reftex-select-mark-comma ()
  "Mark the entry and store the `comma' separator."
  (interactive)
  (reftex-select-mark ?,))
(defun reftex-select-mark-to ()
  "Mark the entry and store the `to' separator."
  (interactive)
  (reftex-select-mark ?-))
(defun reftex-select-mark-and ()
  "Mark the entry and store `and' to separator."
  (interactive)
  (reftex-select-mark ?+))

(defun reftex-select-unmark ()
  "Unmark the entry."
  (interactive)
  (let* ((data (get-text-property (point) :data))
         (cell (assq data reftex-select-marked))
         (ovl (nth 1 cell))
         (cnt 0)
         sep)
    (unless cell
      (error "No marked entry at point"))
    (and ovl (reftex-delete-overlay ovl))
    (setq reftex-select-marked (delq cell reftex-select-marked))
    (setq cnt (1+ (length reftex-select-marked)))
    (mapc (lambda (c)
            (setq sep (nth 2 c))
            (reftex-overlay-put (nth 1 c) 'before-string
                                (if sep
                                    (format "*%c%d* " sep (decf cnt))
                                  (format "*%d*  " (decf cnt)))))
            reftex-select-marked)
    (message "Entry no longer marked")))

(defun reftex-select-help ()
  "Display a summary of the special key bindings."
  (interactive)
  (with-output-to-temp-buffer "*RefTeX Help*"
    (princ help-string))
  (reftex-enlarge-to-fit "*RefTeX Help*" t))

;;; reftex-sel.el ends here
