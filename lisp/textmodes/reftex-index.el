;;; reftex-index.el --- index support with RefTeX

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
(provide 'reftex-index)
(require 'reftex)
;;;

;; START remove for XEmacs release
(defvar mark-active)
(defvar transient-mark-mode)
(defvar TeX-master)
;; END remove for XEmacs release

(declare-function texmathp "ext:texmathp" ())

(defun reftex-index-selection-or-word (&optional arg phrase)
  "Put selection or the word near point into the default index macro.
This uses the information in `reftex-index-default-macro' to make an index
entry.  The phrase indexed is the current selection or the word near point.
When called with one `C-u' prefix, let the user have a chance to edit the
index entry.  When called with 2 `C-u' as prefix, also ask for the index
macro and other stuff.
When called inside TeX math mode as determined by the `texmathp.el' library
which is part of AUCTeX, the string is first processed with the
`reftex-index-math-format', which see."
  (interactive "P")
  (let* ((use-default (not (equal arg '(16))))  ; check for double prefix
         ;; check if we have an active selection
         (active (if (featurep 'xemacs)
                     (and zmacs-regions (region-exists-p))  ; XEmacs
                   (and transient-mark-mode mark-active)))  ; Emacs
         (beg (if active
                  (region-beginning)
                (save-excursion
                  (skip-syntax-backward "w\\") (point))))
         (end (if active
                  (region-end)
                (save-excursion
                  (skip-syntax-forward "w\\") (point))))
         (sel (buffer-substring beg end))
         (mathp (condition-case nil (texmathp) (error nil)))
         (current-prefix-arg nil) ; we want to call reftex-index without prefix.
         key def-char def-tag full-entry)

    (if phrase
        (progn
          (reftex-index-visit-phrases-buffer)
          (reftex-index-new-phrase sel))

      (if (equal sel "")
          ;; Nothing selected, no word, so use full reftex-index command
          (reftex-index)
        ;; OK, we have something to index here.
        ;; Add the dollars when necessary
        (setq key (if mathp
                      (format reftex-index-math-format sel)
                    sel))
        ;; Get info from `reftex-index-default-macro'
        (setq def-char  (if use-default (car reftex-index-default-macro)))
        (setq def-tag   (if use-default (nth 1 reftex-index-default-macro)))
        ;; Does the user want to edit the entry?
        (setq full-entry (if arg
                             (reftex-index-complete-key
                              def-tag nil (cons key 0))
                           key))
        ;; Delete what is in the buffer and make the index entry
        (delete-region beg end)
        (reftex-index def-char full-entry def-tag sel)))))

(defun reftex-index (&optional char key tag sel no-insert)
  "Query for an index macro and insert it along with its arguments.
The index macros available are those defined in `reftex-index-macro' or
by a call to `reftex-add-index-macros', typically from an AUCTeX style file.
RefteX provides completion for the index tag and the index key, and
will prompt for other arguments."

  (interactive)

  ;; Ensure access to scanning info
  (reftex-ensure-index-support t)
  (reftex-access-scan-info current-prefix-arg)

  ;; Find out which macro we are going to use
  (let* ((char (or char
                   (reftex-select-with-char reftex-query-index-macro-prompt
                                            reftex-query-index-macro-help)))
         (macro (nth 1 (assoc char reftex-key-to-index-macro-alist)))
         (entry (or (assoc macro reftex-index-macro-alist)
                    (error "No index macro associated with %c" char)))
         (ntag (nth 1 entry))
         (tag (or tag (nth 1 entry)))
         (nargs (nth 4 entry))
         (nindex (nth 5 entry))
         (opt-args (nth 6 entry))
         (repeat (nth 7 entry))
         opt tag1 value)

    ;; Get the supported arguments
    (if (stringp tag)
        (setq tag1 tag)
      (setq tag1 (or (reftex-index-complete-tag tag opt-args) "")))
    (setq key (or key
                  (reftex-index-complete-key
                   (if (string= tag1 "") "idx" tag1)
                   (member nindex opt-args))))

    ;; Insert the macro and ask for any additional args
    (insert macro)
    (loop for i from 1 to nargs do
      (setq opt (member i opt-args)
            value (cond ((= nindex i) key)
                        ((equal ntag i) tag1)
                        (t (read-string (concat "Macro arg nr. "
                                                (int-to-string i)
                                                (if opt " (optional)" "")
                                                ": ")))))
      (unless (and opt (string= value ""))
        (insert (if opt "[" "{") value (if opt "]" "}"))))
    (and repeat (stringp sel) (insert sel))
    (and key reftex-plug-into-AUCTeX (fboundp 'LaTeX-add-index-entries)
         (LaTeX-add-index-entries key))
    (reftex-index-update-taglist tag1)
    (reftex-notice-new)))

(defun reftex-default-index ()
  (cond ((null reftex-index-default-tag) nil)
        ((stringp reftex-index-default-tag) reftex-index-default-tag)
        (t (or (get reftex-docstruct-symbol 'default-index-tag)
               "idx"))))

(defun reftex-update-default-index (tag &optional tag-list)
  (if (and (not (equal tag ""))
           (stringp tag)
           (eq reftex-index-default-tag 'last)
           (or (null tag-list)
               (member tag tag-list)))
      (put reftex-docstruct-symbol 'default-index-tag tag)))

(defun reftex-index-complete-tag (&optional itag opt-args)
  ;; Ask the user for a tag, completing on known tags.
  ;; ITAG is the argument number which contains the tag.
  ;; OPT-ARGS is a list of optional argument indices, as given by
  ;; `reftex-parse-args'.
  (let* ((opt (and (integerp itag) (member itag opt-args)))
	 (index-tags (cdr (assq 'index-tags
				(symbol-value reftex-docstruct-symbol))))
	 (default (reftex-default-index))
	 (prompt (concat "Index tag"
			 (if (or opt default)
			     (format " (%s): "
				     (concat
				      (if opt "optional" "")
				      (if default
					  (concat (if opt ", " "")
						  (format "default %s" default))
					"")))
			   ": ")))
	 (tag (completing-read prompt (mapcar 'list index-tags))))
    (if (and default (equal tag "")) (setq tag default))
    (reftex-update-default-index tag)
    tag))

(defun reftex-index-select-tag ()
  ;; Have the user select an index tag.
  ;; FIXME: should we cache tag-alist, prompt and help?
  (let* ((index-tags (cdr (assoc 'index-tags
                                 (symbol-value reftex-docstruct-symbol))))
         (default (reftex-default-index)))
    (cond
     ((null index-tags)
      (error "No index tags available"))

     ((= (length index-tags) 1)
      ;; Just one index, use it
      (car index-tags))

     ((> (length index-tags) 1)
      ;; Several indices, ask.
      (let* ((tags (copy-sequence index-tags))
             (cnt 0)
             tag-alist i val len tag prompt help rpl)
        ;; Move idx and glo up in the list to ensure ?i and ?g shortcuts
        (if (member "glo" tags)
            (setq tags (cons "glo" (delete "glo" tags))))
        (if (member "idx" tags)
            (setq tags (cons "idx" (delete "idx" tags))))
        ;; Find unique shortcuts for each index.
        (while (setq tag (pop tags))
          (setq len (length tag)
                i -1
                val nil)
          (catch 'exit
            (while (and (< (incf i) len) (null val))
              (unless (assq (aref tag i) tag-alist)
                (push (list (aref tag i)
                            tag
                            (concat (substring tag 0 i)
                                    "[" (substring tag i (incf i)) "]"
                                    (substring tag i)))
                      tag-alist)
                (throw 'exit t)))
            (push (list (+ ?0 (incf cnt)) tag
                        (concat "[" (int-to-string cnt) "]:" tag))
                  tag-alist)))
        (setq tag-alist (nreverse tag-alist))
        ;; Compute Prompt and Help strings
        (setq prompt
              (concat
               (format "Select Index%s: "
                       (if default (format " (Default <%s>)" default) ""))
               (mapconcat (lambda(x) (nth 2 x)) tag-alist "  ")))
        (setq help
              (concat "Select an Index\n===============\n"
                      (if default
                          (format "[^M]  %s (the default)\n" default)
                        "")
                      (mapconcat (lambda(x)
                                   (apply 'format "[%c]   %s" x))
                                 tag-alist "\n")))
        ;; Query the user for an index-tag
        (setq rpl (reftex-select-with-char prompt help 3 t))
        (message "")
        (if (and default (equal rpl ?\C-m))
            default
          (if (assq rpl tag-alist)
              (progn
                (reftex-update-default-index (nth 1 (assq rpl tag-alist)))
                (nth 1 (assq rpl tag-alist)))
            (error "No index tag associated with %c" rpl)))))
     (t (error "This should not happen (reftex-index-select-tag)")))))

(defun reftex-index-complete-key (&optional tag optional initial)
  ;; Read an index key, with completion.
  ;; Restrict completion table on index tag TAG.
  ;; OPTIONAL indicates if the arg is optional.
  (let* ((table (reftex-sublist-nth
                 (symbol-value reftex-docstruct-symbol) 6
                 (lambda(x) (and (eq (car x) 'index)
                                 (string= (nth 1 x) (or tag ""))))
                 t))
         (prompt (concat "Index key" (if optional " (optional)" "") ": "))
         (key (completing-read prompt table nil nil initial)))
    key))

(defun reftex-index-update-taglist (newtag)
  ;; add NEWTAG to the list of available index tags.
  (let ((cell (assoc 'index-tags (symbol-value reftex-docstruct-symbol))))
    (and newtag (cdr cell) (not (member newtag (cdr cell)))
         (push newtag (cdr cell)))))

(defvar reftex-index-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Index map
    (define-key map (if (featurep 'xemacs) [(button2)] [(mouse-2)])
      'reftex-index-mouse-goto-line-and-hide)
    (define-key map [follow-link] 'mouse-face)

    (substitute-key-definition
     'next-line 'reftex-index-next map global-map)
    (substitute-key-definition
     'previous-line 'reftex-index-previous map global-map)

    (loop for x in
          '(("n"    . reftex-index-next)
            ("p"    . reftex-index-previous)
            ("?"    . reftex-index-show-help)
            (" "    . reftex-index-view-entry)
            ("\C-m" . reftex-index-goto-entry-and-hide)
            ("\C-i" . reftex-index-goto-entry)
            ("\C-k" . reftex-index-kill)
            ("r"    . reftex-index-rescan)
            ("R"    . reftex-index-Rescan)
            ("g"    . revert-buffer)
            ("q"    . reftex-index-quit)
            ("k"    . reftex-index-quit-and-kill)
            ("f"    . reftex-index-toggle-follow)
            ("s"    . reftex-index-switch-index-tag)
            ("e"    . reftex-index-edit)
            ("^"    . reftex-index-level-up)
            ("_"    . reftex-index-level-down)
            ("}"    . reftex-index-restrict-to-section)
            ("{"    . reftex-index-widen)
            (">"    . reftex-index-restriction-forward)
            ("<"    . reftex-index-restriction-backward)
            ("("    . reftex-index-toggle-range-beginning)
            (")"    . reftex-index-toggle-range-end)
            ("|"    . reftex-index-edit-attribute)
            ("@"    . reftex-index-edit-visual)
            ("*"    . reftex-index-edit-key)
            ("\C-c=". reftex-index-goto-toc)
            ("c"    . reftex-index-toggle-context))
          do (define-key map (car x) (cdr x)))

    (loop for key across "0123456789" do
          (define-key map (vector (list key)) 'digit-argument))
    (define-key map "-" 'negative-argument)

    ;; The capital letters and the exclamation mark
    (loop for key across (concat "!" reftex-index-section-letters) do
          (define-key map (vector (list key))
            (list 'lambda '() '(interactive)
                  (list 'reftex-index-goto-letter key))))

    (easy-menu-define reftex-index-menu map
      "Menu for Index buffer"
      '("Index"
        ["Goto section A-Z"
         (message "To go to a section, just press any of: !%s"
                  reftex-index-section-letters) t]
        ["Show Entry" reftex-index-view-entry t]
        ["Go To Entry" reftex-index-goto-entry t]
        ["Exit & Go To Entry" reftex-index-goto-entry-and-hide t]
        ["Table of Contents" reftex-index-goto-toc t]
        ["Quit" reftex-index-quit t]
        "--"
        ("Update"
         ["Rebuilt *Index* Buffer" revert-buffer t]
         "--"
         ["Rescan One File" reftex-index-rescan reftex-enable-partial-scans]
         ["Rescan Entire Document" reftex-index-Rescan t])
        ("Restrict"
         ["Restrict to section" reftex-index-restrict-to-section t]
         ["Widen" reftex-index-widen reftex-index-restriction-indicator]
         ["Next Section" reftex-index-restriction-forward
          reftex-index-restriction-indicator]
         ["Previous Section" reftex-index-restriction-backward
          reftex-index-restriction-indicator])
        ("Edit"
         ["Edit Entry" reftex-index-edit t]
         ["Edit Key" reftex-index-edit-key t]
         ["Edit Attribute" reftex-index-edit-attribute t]
         ["Edit Visual" reftex-index-edit-visual t]
         "--"
         ["Add Parentkey" reftex-index-level-down t]
         ["Remove Parentkey " reftex-index-level-up t]
         "--"
         ["Make Start-of-Range" reftex-index-toggle-range-beginning t]
         ["Make End-of-Range" reftex-index-toggle-range-end t]
         "--"
         ["Kill Entry" reftex-index-kill nil]
         "--"
         ["Undo" reftex-index-undo nil])
        ("Options"
         ["Context" reftex-index-toggle-context :style toggle
          :selected reftex-index-include-context]
         "--"
         ["Follow Mode" reftex-index-toggle-follow :style toggle
          :selected reftex-index-follow-mode])
        "--"
        ["Help" reftex-index-show-help t]))

    map)
  "Keymap used for *Index* buffers.")
(define-obsolete-variable-alias
  'reftex-index-map 'reftex-index-mode-map "24.1")

(defvar reftex-index-menu)

(defvar reftex-last-index-file nil
  "Stores the file name from which `reftex-display-index' was called.")
(defvar reftex-index-tag nil
  "Stores the tag of the index in an index buffer.")

(defvar reftex-index-return-marker (make-marker)
  "Marker which makes it possible to return from index to old position.")

(defvar reftex-index-restriction-indicator nil)
(defvar reftex-index-restriction-data nil)

(define-derived-mode reftex-index-mode fundamental-mode "RefTeX Index"
  "Major mode for managing Index buffers for LaTeX files.
This buffer was created with RefTeX.
Press `?' for a summary of important key bindings, or check the menu.

Here are all local bindings.

\\{reftex-index-mode-map}"
  (set (make-local-variable 'revert-buffer-function) 'reftex-index-revert)
  (set (make-local-variable 'reftex-index-restriction-data) nil)
  (set (make-local-variable 'reftex-index-restriction-indicator) nil)
  (setq mode-line-format
        (list "----  " 'mode-line-buffer-identification
              "   " 'global-mode-string
              "  R<" 'reftex-index-restriction-indicator ">"
              " -%-"))
  (setq truncate-lines t)
  (when (featurep 'xemacs)
    ;; XEmacs needs the call to make-local-hook
    (make-local-hook 'post-command-hook)
    (make-local-hook 'pre-command-hook))
  (make-local-variable 'reftex-last-follow-point)
  (easy-menu-add reftex-index-menu reftex-index-mode-map)
  (add-hook 'post-command-hook 'reftex-index-post-command-hook nil t)
  (add-hook 'pre-command-hook  'reftex-index-pre-command-hook nil t))

(defconst reftex-index-help
"                      AVAILABLE KEYS IN INDEX BUFFER
                      ==============================
! A..Z     Goto the section of entries starting with this letter.
n / p      next-entry / previous-entry
SPC / TAB  Show/Goto the corresponding entry in the LaTeX document.
RET        Goto the entry and hide the *Index* window (also on mouse-2).
q / k      Hide/Kill *Index* buffer.
C-c =      Switch to the TOC buffer.
f / c      Toggle follow mode             / Toggle display of [c]ontext.
g          Refresh *Index* buffer.
r / C-u r  Reparse the LaTeX document     / Reparse entire LaTeX document.
s          Switch to a different index (for documents with multiple indices).
e / C-k    Edit/Kill the entry.
* | @      Edit specific part of entry: [*]key [|]attribute [@]visual
           With prefix: kill that part.
\( )        Toggle entry's beginning/end of page range property.
_ ^        Add/Remove parent key (to make this item a subitem).
} / {      Restrict Index to a single document section / Widen.
< / >      When restricted, move restriction to previous/next section.")

(defun reftex-index-show-entry (data &optional no-revisit)
  ;; Find an index entry associated with DATA and display it highlighted
  ;; in another window.  NO-REVISIT means we are not allowed to visit
  ;; files for this.
  ;; Note:  This function just looks for the nearest match of the
  ;; context string and may fail if the entry moved and an identical
  ;; entry is close to the old position.  Frequent rescans make this
  ;; safer.
  (let* ((file (nth 3 data))
         (literal (nth 2 data))
         (pos (nth 4 data))
         (re (regexp-quote literal))
         (match
          (cond
           ((or (not no-revisit)
                (reftex-get-buffer-visiting file))
            (switch-to-buffer-other-window
             (reftex-get-file-buffer-force file nil))
            (goto-char (or pos (point-min)))
            (or (looking-at re)
                (reftex-nearest-match re (length literal))))
           (t (message "%s" reftex-no-follow-message) nil))))
    (when match
      (goto-char (match-beginning 0))
      (recenter '(4))
      (reftex-highlight 0 (match-beginning 0) (match-end 0) (current-buffer)))
    match))

(defun reftex-display-index (&optional tag overriding-restriction redo
                                       &rest locations)
  "Display a buffer with an index compiled from the current document.
When the document has multiple indices, first prompts for the correct one.
When index support is turned off, offer to turn it on.
With one or two `C-u' prefixes, rescan document first.
With prefix 2, restrict index to current document section.
With prefix 3, restrict index to region."

  (interactive)

  ;; Ensure access to scanning info and rescan buffer if prefix are is '(4).
  (let ((current-prefix-arg current-prefix-arg))
    (reftex-ensure-index-support t)
    (reftex-access-scan-info current-prefix-arg))

  (set-marker reftex-index-return-marker (point))
  (setq reftex-last-follow-point 1)

  ;; Determine the correct index to process
  (let* ((docstruct (symbol-value reftex-docstruct-symbol))
         (docstruct-symbol reftex-docstruct-symbol)
         (index-tag (or tag (reftex-index-select-tag)))
         (master (reftex-TeX-master-file))
         (calling-file (buffer-file-name))
         (restriction
          (or overriding-restriction
              (and (not redo)
                   (reftex-get-restriction current-prefix-arg docstruct))))
         (locations
          ;; See if we are on an index macro as initial position
          (or locations
              (let* ((what-macro (reftex-what-macro-safe 1))
                     (macro (car what-macro))
                     (here-I-am (when (member macro reftex-macros-with-index)
                                  (save-excursion
                                    (goto-char (+ (cdr what-macro)
                                                  (length macro)))
                                    (reftex-move-over-touching-args)
                                    (reftex-where-am-I)))))
                (if (eq (car (car here-I-am)) 'index)
                    (list (car here-I-am))))))
         buffer-name)

    (setq buffer-name (reftex-make-index-buffer-name index-tag))

    ;; Goto the buffer and put it into the correct mode

    (when (or restriction current-prefix-arg)
         (reftex-kill-buffer buffer-name))

    (if (get-buffer-window buffer-name)
        (select-window (get-buffer-window buffer-name))
      (switch-to-buffer buffer-name))

    (or (eq major-mode 'reftex-index-mode) (reftex-index-mode))

    ;; If the buffer is currently restricted, empty it to force update.
    (when reftex-index-restriction-data
      (reftex-erase-buffer))
    (set (make-local-variable 'reftex-last-index-file) calling-file)
    (set (make-local-variable 'reftex-index-tag) index-tag)
    (set (make-local-variable 'reftex-docstruct-symbol) docstruct-symbol)
    (if restriction
        (setq reftex-index-restriction-indicator (car restriction)
              reftex-index-restriction-data (cdr restriction))
      (if (not redo)
          (setq reftex-index-restriction-indicator nil
                reftex-index-restriction-data nil)))
    (when (= (buffer-size) 0)
      ;; buffer is empty - fill it
      (message "Building %s buffer..." buffer-name)

      (setq buffer-read-only nil)
      (insert (format
"INDEX <%s> on %s
Restriction: <%s>
SPC=view TAB=goto RET=goto+hide [e]dit [q]uit [r]escan [f]ollow [?]Help
------------------------------------------------------------------------------
" index-tag (abbreviate-file-name master)
(if (eq (car (car reftex-index-restriction-data)) 'toc)
    (nth 2 (car reftex-index-restriction-data))
  reftex-index-restriction-indicator)))

      (if (reftex-use-fonts)
          (put-text-property 1 (point) 'face reftex-index-header-face))
      (put-text-property 1 (point) 'intangible t)

      (reftex-insert-index docstruct index-tag)
      (goto-char (point-min))
      (run-hooks 'reftex-display-copied-context-hook)
      (message "Building %s buffer...done." buffer-name)
      (setq buffer-read-only t))
    (and locations (apply 'reftex-find-start-point (point) locations))
    (if reftex-index-restriction-indicator
        (message "Index restricted: <%s>" reftex-index-restriction-indicator))))

(defun reftex-insert-index (docstruct tag &optional update-one remark)
  ;; Insert an index into the current buffer.  Entries are from the
  ;; DOCSTRUCT.
  ;; TAG is the subindex to process.
  ;; UPDATE-ONE: When non-nil, delete the entry at point and replace
  ;; it with whatever the DOCSTRUCT contains.
  ;; REMARK can be a note to add to the entry.
  (let* ((all docstruct)
         (indent "   ")
         (context reftex-index-include-context)
         (context-indent (concat indent "  "))
         (section-chars (mapcar 'identity reftex-index-section-letters))
         (this-section-char 0)
         (font (reftex-use-fonts))
         (bor (car reftex-index-restriction-data))
         (eor (nth 1 reftex-index-restriction-data))
         (mouse-face
          (if (memq reftex-highlight-selection '(mouse both))
              reftex-mouse-selected-face
            nil))
         (index-face (reftex-verified-face reftex-label-face
                                           'font-lock-constant-face
                                           'font-lock-reference-face))
         sublist cell from to first-char)

    ;; Make the sublist and sort it
    (when bor
      (setq all (or (memq bor all) all)))

    (while (setq cell (pop all))
      (if (eq cell eor)
          (setq all nil)
        (and (eq (car cell) 'index)
             (equal (nth 1 cell) tag)
             (push cell sublist))))
    (setq sublist (sort (nreverse sublist)
                        (lambda (a b) (string< (nth 8 a) (nth 8 b)))))

    (when update-one
      ;; Delete the entry at place
      (and (bolp) (forward-char 1))
      (delete-region (previous-single-property-change (1+ (point)) :data)
                     (or (next-single-property-change (point) :data)
                         (point-max))))

    ;; Walk through the list and insert all entries
    (while (setq cell (pop sublist))
      (unless update-one
        (setq first-char (upcase (string-to-char (nth 6 cell))))
        (when (and (not (equal first-char this-section-char))
                   (member first-char section-chars))
          ;; There is a new initial letter, so start a new section
          (reftex-index-insert-new-letter first-char font)
          (setq section-chars (delete first-char section-chars)
                this-section-char first-char))
        (when (= this-section-char 0)
          (setq this-section-char ?!)
          (reftex-index-insert-new-letter this-section-char font)))

      (setq from (point))
      (insert indent (nth 7 cell))
      (when font
        (setq to (point))
        (put-text-property
         (- (point) (length (nth 7 cell))) to
         'face index-face)
        (goto-char to))

      (when (or remark (nth 9 cell))
        (and (< (current-column) 40)
             ;; FIXME: maybe this is too slow?
             (insert (make-string (max (- 40 (current-column)) 0) ?\ )))
        (and (nth 9 cell) (insert "   " (substring (nth 5 cell) (nth 9 cell))))
        (and remark (insert "     " remark)))

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


(defun reftex-index-insert-new-letter (letter &optional font)
  ;; Start a new section in the index
  (let ((from (point)))
    (insert "\n" letter letter letter
            "-----------------------------------------------------------------")
    (when font
      (put-text-property from (point) 'face reftex-index-section-face))
    (insert "\n")))

(defun reftex-get-restriction (arg docstruct)
  ;; Interpret the prefix ARG and derive index restriction specs.
  (let* ((beg (min (point) (or (condition-case nil (mark) (error nil))
                               (point-max))))
         (end (max (point) (or (condition-case nil (mark) (error nil))
                               (point-min))))
         bor eor label here-I-am)
    (cond
     ((eq arg 2)
      (setq here-I-am (car (reftex-where-am-I))
            bor (if (eq (car here-I-am) 'toc)
                    here-I-am
                  (reftex-last-assoc-before-elt
                   'toc here-I-am docstruct))
            eor (car (memq (assq 'toc (cdr (memq bor docstruct))) docstruct))
            label (nth 6 bor)))
     ((eq arg 3)
      (save-excursion
        (setq label "region")
        (goto-char beg)
        (setq bor (car (reftex-where-am-I)))
        (setq bor (nth 1 (memq bor docstruct)))
        (goto-char end)
        (setq eor (nth 1 (memq (car (reftex-where-am-I)) docstruct)))))
     (t nil))
    (if (and label (or bor eor))
        (list label bor eor)
      nil)))

(defun reftex-index-pre-command-hook ()
  ;; Used as pre command hook in *Index* buffer
  (reftex-unhighlight 0)
  (reftex-unhighlight 1))

(defun reftex-index-post-command-hook ()
  ;; Used in the post-command-hook for the *Index* buffer
  (when (get-text-property (point) :data)
    (and (> (point) 1)
         (not (get-text-property (point) 'intangible))
         (memq reftex-highlight-selection '(cursor both))
         (reftex-highlight 1
           (or (previous-single-property-change (1+ (point)) :data)
               (point-min))
           (or (next-single-property-change (point) :data)
               (point-max)))))
  (if (integerp reftex-index-follow-mode)
      ;; Remove delayed action
      (setq reftex-index-follow-mode t)
    (and reftex-index-follow-mode
         (not (equal reftex-last-follow-point (point)))
         ;; Show context in other window
         (setq reftex-last-follow-point (point))
         (condition-case nil
             (reftex-index-visit-location nil (not reftex-revisit-to-follow))
           (error t)))))

(defun reftex-index-show-help ()
  "Show a summary of special key bindings."
  (interactive)
  (with-output-to-temp-buffer "*RefTeX Help*"
    (princ reftex-index-help))
  (reftex-enlarge-to-fit "*RefTeX Help*" t)
  ;; If follow mode is active, arrange to delay it one command
  (if reftex-index-follow-mode
      (setq reftex-index-follow-mode 1)))

(defun reftex-index-next (&optional arg)
  "Move to next selectable item."
  (interactive "p")
  (setq reftex-callback-fwd t)
  (or (eobp) (forward-char 1))
  (goto-char (or (next-single-property-change (point) :data)
                 (point)))
  (unless (get-text-property (point) :data)
    (goto-char (or (next-single-property-change (point) :data)
                   (point)))))
(defun reftex-index-previous (&optional arg)
  "Move to previous selectable item."
  (interactive "p")
  (setq reftex-callback-fwd nil)
  (goto-char (or (previous-single-property-change (point) :data)
                 (point)))
  (unless (get-text-property (point) :data)
    (goto-char (or (previous-single-property-change (point) :data)
                   (point)))))
(defun reftex-index-toggle-follow ()
  "Toggle follow (other window follows with context)."
  (interactive)
  (setq reftex-last-follow-point -1)
  (setq reftex-index-follow-mode (not reftex-index-follow-mode)))
(defun reftex-index-toggle-context ()
  "Toggle inclusion of label context in *Index* buffer.
Label context is only displayed when the labels are there as well."
  (interactive)
  (setq reftex-index-include-context (not reftex-index-include-context))
  (reftex-index-revert))
(defun reftex-index-view-entry ()
  "View document location in other window."
  (interactive)
  (reftex-index-visit-location))
(defun reftex-index-goto-entry-and-hide ()
  "Go to document location in other window.  Hide the *Index* window."
  (interactive)
  (reftex-index-visit-location 'hide))
(defun reftex-index-goto-entry ()
  "Go to document location in other window. *Index* window stays."
  (interactive)
  (reftex-index-visit-location t))
(defun reftex-index-mouse-goto-line-and-hide (ev)
  "Go to document location in other window.  Hide the *Index* window."
  (interactive "e")
  (mouse-set-point ev)
  (reftex-index-visit-location 'hide))
(defun reftex-index-quit ()
  "Hide the *Index* window and do not move point."
  (interactive)
  (or (one-window-p) (delete-window))
  (switch-to-buffer (marker-buffer reftex-index-return-marker))
  (goto-char (or (marker-position reftex-index-return-marker) (point))))
(defun reftex-index-quit-and-kill ()
  "Kill the *Index* buffer."
  (interactive)
  (kill-buffer (current-buffer))
  (or (one-window-p) (delete-window))
  (switch-to-buffer (marker-buffer reftex-index-return-marker))
  (goto-char (or (marker-position reftex-index-return-marker) (point))))
(defun reftex-index-goto-toc (&rest ignore)
  "Switch to the table of contents of the current document.
The function will go to the section where the entry at point was defined."
  (interactive)
  (if (get-text-property (point) :data)
      (reftex-index-goto-entry)
    (switch-to-buffer (marker-buffer reftex-index-return-marker)))
  (delete-other-windows)
  (reftex-toc))
(defun reftex-index-rescan (&rest ignore)
  "Regenerate the *Index* buffer after reparsing file of section at point."
  (interactive)
  (let ((index-tag reftex-index-tag))
    (if (and reftex-enable-partial-scans
             (null current-prefix-arg))
        (let* ((data (get-text-property (point) :data))
               (file (nth 3 data))
               (line (+ (count-lines (point-min) (point)) (if (bolp) 1 0))))
          (if (not file)
              (error "Don't know which file to rescan.  Try `C-u r'")
            (switch-to-buffer (reftex-get-file-buffer-force file))
            (setq current-prefix-arg '(4))
            (reftex-display-index index-tag nil 'redo line)))
      (reftex-index-Rescan))
    (reftex-kill-temporary-buffers)))
(defun reftex-index-Rescan (&rest ignore)
  "Regenerate the *Index* buffer after reparsing the entire document."
  (interactive)
  (let ((index-tag reftex-index-tag)
        (line (+ (count-lines (point-min) (point)) (if (bolp) 1 0))))
    (switch-to-buffer
     (reftex-get-file-buffer-force reftex-last-index-file))
    (setq current-prefix-arg '(16))
    (reftex-display-index index-tag nil 'redo line)))
(defun reftex-index-revert (&rest ignore)
  "Regenerate the *Index* from the internal lists.  No reparsing os done."
  (interactive)
  (let ((buf (current-buffer))
        (index-tag reftex-index-tag)
        (data (get-text-property (point) :data))
        (line (+ (count-lines (point-min) (point)) (if (bolp) 1 0))))
    (switch-to-buffer
     (reftex-get-file-buffer-force reftex-last-index-file))
    (reftex-erase-buffer buf)
    (setq current-prefix-arg nil
          reftex-last-follow-point 1)
    (reftex-display-index index-tag nil 'redo data line)))
(defun reftex-index-switch-index-tag (&rest ignore)
  "Switch to a different index of the same document."
  (interactive)
  (switch-to-buffer
   (reftex-get-file-buffer-force reftex-last-index-file))
  (setq current-prefix-arg nil)
  (reftex-display-index nil nil 'redo))

(defun reftex-index-restrict-to-section (&optional force)
  "Restrict index to entries defined in same document sect. as entry at point."
  ;; Optional FORCE means, even if point is not on an index entry.
  (interactive)
  (let* ((data (get-text-property (point) :data))
         (docstruct (symbol-value reftex-docstruct-symbol))
         bor eor)
    (if (and (not data) force)
        (setq data (assq 'toc docstruct)))
    (when data
      (setq bor (reftex-last-assoc-before-elt 'toc data docstruct)
            eor (car (memq (assq 'toc (cdr (memq bor docstruct)))
                           docstruct))
            reftex-index-restriction-data (list bor eor)
            reftex-index-restriction-indicator (nth 6 bor) )))
  (reftex-index-revert))

(defun reftex-index-widen (&rest ignore)
  "Show the unrestricted index (all entries)."
  (interactive)
  (setq reftex-index-restriction-indicator nil
        reftex-index-restriction-data nil)
  (reftex-index-revert)
  (message "Index widened"))
(defun reftex-index-restriction-forward (&rest ignore)
  "Restrict to previous section.
When index is currently unrestricted, restrict it to a section.
When index is restricted, select the next section as restriction criterion."
  (interactive)
  (let* ((docstruct (symbol-value reftex-docstruct-symbol))
         (bor (nth 1 reftex-index-restriction-data)))
    (if (or (not bor)
            (not (eq (car bor) 'toc)))
        (reftex-index-restrict-to-section t)
      (setq reftex-index-restriction-indicator (nth 6 bor)
            reftex-index-restriction-data
            (list bor
                  (car (memq (assq 'toc (cdr (memq bor docstruct)))
                             docstruct))))
      (reftex-index-revert))))
(defun reftex-index-restriction-backward (&rest ignore)
  "Restrict to next section.
When index is currently unrestricted, restrict it to a section.
When index is restricted, select the previous section as restriction criterion."
  (interactive)
  (let* ((docstruct (symbol-value reftex-docstruct-symbol))
         (eor (car reftex-index-restriction-data))
         (bor (reftex-last-assoc-before-elt 'toc eor docstruct t)))
    (if (or (not bor)
            (not (eq (car bor) 'toc)))
        (reftex-index-restrict-to-section t)
      (setq reftex-index-restriction-indicator (nth 6 bor)
            reftex-index-restriction-data
            (list bor eor))
      (reftex-index-revert))))

(defun reftex-index-visit-location (&optional final no-revisit)
  ;; Visit the tex file corresponding to the index entry on the current line.
  ;; If FINAL is t, stay there
  ;; If FINAL is 'hide, hide the *Index* window.
  ;; Otherwise, move cursor back into *Index* window.
  ;; NO-REVISIT means don't visit files, just use live biffers.

  (let* ((data (get-text-property (point) :data))
         (index-window (selected-window))
         show-window show-buffer match)

    (unless data (error "Don't know which index entry to visit"))

    (if (eq (car data) 'index)
        (setq match (reftex-index-show-entry data no-revisit)))

    (setq show-window (selected-window)
          show-buffer (current-buffer))

    (unless match
      (select-window index-window)
      (error "Cannot find location"))

    (select-window index-window)

    ;; Use the `final' parameter to decide what to do next
    (cond
     ((eq final t)
      (reftex-unhighlight 0)
      (select-window show-window))
     ((eq final 'hide)
      (reftex-unhighlight 0)
      (or (one-window-p) (delete-window))
      (switch-to-buffer show-buffer))
     (t nil))))

(defun reftex-index-analyze-entry (data)
  ;; This splits the index context so that key, attribute and visual
  ;; values are accessible individually.
  (interactive)
  (let* ((arg (nth 5 data))
         (context (nth 2 data))
         (sc reftex-index-special-chars)
         (boa (if (string-match (regexp-quote (concat "{" arg "}")) context)
                  (1+ (match-beginning 0))
                (error "Something is wrong here")))
         (eoa (1- (match-end 0)))
         (boactual (if (string-match (concat "[^" (nth 3 sc) "]" (nth 2 sc))
                                     context boa)
                       (1+ (match-beginning 0))
                     eoa))
         (boattr (if (string-match (concat "[^" (nth 3 sc) "]" (nth 1 sc))
                                   context boa)
                     (1+ (match-beginning 0))
                   boactual))
         (pre (substring context 0 boa))
         (key (substring context boa boattr))
         (attr (substring context boattr boactual))
         (actual (substring context boactual eoa))
         (post (substring context eoa)))
    (list pre key attr actual post)))

(defun reftex-index-edit ()
  "Edit the index entry at point."
  (interactive)
  (let* ((data (get-text-property (point) :data))
         old new)
    (unless data (error "Don't know which index entry to edit"))
    (reftex-index-view-entry)
    (setq old (nth 2 data) new (read-string "Edit: " old))
    (reftex-index-change-entry new)))

(defun reftex-index-toggle-range-beginning ()
  "Toggle the page range start attribute `|('."
  (interactive)
  (let* ((data (get-text-property (point) :data))
         (bor (concat (nth 1 reftex-index-special-chars) "("))
         new analyze attr)
    (unless data (error "Don't know which index entry to edit"))
    (setq analyze (reftex-index-analyze-entry data)
          attr (nth 2 analyze))
    (setf (nth 2 analyze) (if (string= attr bor) "" bor))
    (setq new (apply 'concat analyze))
    (reftex-index-change-entry
     new (if (string= (nth 2 analyze) bor)
             "Entry is now START-OF-PAGE-RANGE"
           "START-OF-PAGE-RANGE canceled"))))

(defun reftex-index-toggle-range-end ()
  "Toggle the page-range-end attribute `|)'."
  (interactive)
  (let* ((data (get-text-property (point) :data))
         (eor (concat (nth 1 reftex-index-special-chars) ")"))
         new analyze attr)
    (unless data (error "Don't know which index entry to edit"))
    (setq analyze (reftex-index-analyze-entry data)
          attr (nth 2 analyze))
    (setf (nth 2 analyze) (if (string= attr eor) "" eor))
    (setq new (apply 'concat analyze))
    (reftex-index-change-entry
     new (if (string= (nth 2 analyze) eor)
             "Entry is now END-OF-PAGE-RANGE"
           "END-OF-PAGE-RANGE canceled"))))

(defun reftex-index-edit-key ()
  "Edit the KEY part of the index entry."
  (interactive)
  (reftex-index-edit-part nil 1 "" "Key: " t))

(defun reftex-index-edit-attribute (&optional arg)
  "EDIT the ATTRIBUTE part of the entry.  With arg: remove entire ATTRIBUTE."
  (interactive "P")
  (reftex-index-edit-part arg 2 (nth 1 reftex-index-special-chars)
                          "Attribute: "))

(defun reftex-index-edit-visual (&optional arg)
  "EDIT the VISUAL part of the entry.  With arg: remove entire VISUAL string."
  (interactive "P")
  (reftex-index-edit-part arg 3 (nth 2 reftex-index-special-chars) "Visual: "))

(defun reftex-index-edit-part (arg n initial prompt &optional dont-allow-empty)
  ;; This function does the work for all partial editing commands
  (let* ((data (get-text-property (point) :data))
         new analyze opart npart)
    (unless data (error "Don't know which index entry to edit"))
    ;; Analyze the whole context string
    (setq analyze (reftex-index-analyze-entry data)
          opart (nth n analyze))
    (and (> (length opart) 0) (setq opart (substring opart 1)))
    ;; Have the user editing the part
    (setq npart (if arg "" (read-string (concat prompt initial) opart)))
    ;; Tests:
    (cond ((string= npart opart)
           (error "Not changed"))
          ((string= npart "")
           (if dont-allow-empty
               (error "Invalid value")
             (setf (nth n analyze) npart)))
          (t (setf (nth n analyze) (concat initial npart))))
    (setq new (apply 'concat analyze))
    ;; Change the entry and insert the changed version into the index.
    (reftex-index-change-entry
     new (if (string= npart "")
             (format "Deleted: %s" opart)
           (format "New value is: %s" npart)))))

(defun reftex-index-level-down ()
  "Make index entry a subitem of another entry."
  (interactive)
  (let* ((data (get-text-property (point) :data))
         (docstruct (symbol-value reftex-docstruct-symbol))
         old new prefix key)
    (unless data (error "Don't know which index entry to change"))
    (setq old (nth 2 data)
          key (nth 6 data)
          prefix (completing-read
                  "Prefix: "
                  (reftex-sublist-nth
                   docstruct 6
                   (lambda (x)
                     (and (eq (car x) 'index)
                          (string= (nth 1 x) reftex-index-tag))) t)))
    (unless (string-match
             (concat (regexp-quote (car reftex-index-special-chars)) "\\'")
             prefix)
      (setq prefix (concat prefix (car reftex-index-special-chars))))
    (if (string-match (regexp-quote key) old)
        (setq new (replace-match (concat prefix key) t t old))
      (error "Cannot construct new index key"))
    (reftex-index-change-entry new (format "Added prefix: %s" prefix))))

(defun reftex-index-level-up ()
  "Remove the highest level of a hierarchical index entry."
  (interactive)
  (let* ((data (get-text-property (point) :data))
         old new prefix)
    (unless data (error "Don't know which entry to change"))
    (setq old (nth 2 data))
    (if (string-match (concat "{\\([^" (nth 0 reftex-index-special-chars) "]*"
                              "[^" (nth 3 reftex-index-special-chars) "]"
                              (regexp-quote (nth 0 reftex-index-special-chars))
                              "\\)")
                      old)
        (setq prefix (substring old (match-beginning 1) (match-end 1))
              new (concat (substring old 0 (match-beginning 1))
                          (substring old (match-end 1))))
      (error "Entry is not a subitem"))
    (reftex-index-change-entry new (format "Removed prefix: %s" prefix))))

(defun reftex-index-kill ()
  "FIXME: Not yet implemented"
  (interactive)
  (error "This function is currently not implemented"))

(defun reftex-index-undo ()
  "FIXME: Not yet implemented"
  (interactive)
  (error "This function is currently not implemented"))

(defun reftex-index-change-entry (new &optional message)
  ;; Change the full context string of the index entry at point to
  ;; NEW.  This actually edits the buffer where the entry is defined.

  (let* ((data (get-text-property (point) :data))
         old beg end info)
    (unless data (error "Cannot change entry"))
    (reftex-index-view-entry)
    (setq beg (match-beginning 0) end (match-end 0))
    (setq old (nth 2 data))
    (and (equal old new) (error "Entry unchanged"))
    (with-current-buffer (get-file-buffer (nth 3 data))
      (goto-char beg)
      (unless (looking-at (regexp-quote old))
        (error "This should not happen (reftex-index-change-entry)"))
      (delete-region beg end)
      (insert new)
      (goto-char (1- beg))
      (when (and (re-search-forward (reftex-everything-regexp) nil t)
                 (match-end 10)
                 (< (abs (- (match-beginning 10) beg)) (length new))
                 (setq info (reftex-index-info-safe buffer-file-name)))
        (setcdr data (cdr info))))
    (let ((buffer-read-only nil))
      (save-excursion
        (reftex-insert-index (list data) reftex-index-tag t
                             "EDITED")))
    (setq reftex-last-follow-point 1)
    (and message (message "%s" message))))

(defun reftex-index-goto-letter (char)
  "Go to the CHAR section in the index."
  (let ((pos (point))
        (case-fold-search nil))
    (goto-char (point-min))
    (forward-line 2)
    (if (re-search-forward (concat "^" (char-to-string char)) nil t)
        (progn
          (beginning-of-line)
          (recenter 0)
          (reftex-index-next))
      (goto-char pos)
      (if (eq char ?!)
          (error "This <%s> index does not contain entries sorted before the letters"
                 reftex-index-tag)
        (error "This <%s> index does not contain entries starting with `%c'"
               reftex-index-tag char)))))


;;----------------------------------------------------------------------
;; The Index Phrases File

;; Some constants and variables
(defconst reftex-index-phrases-comment-regexp "^[ \t]*%.*"
  "Regular expression to match comment lines in phrases buffer")
(defconst reftex-index-phrases-macrodef-regexp
  "^\\(>>>INDEX_MACRO_DEFINITION:\\)[ \t]+\\(\\S-\\)\\( *\t[ \t]*\\)\\([^\t]*[^ \t]\\)\\( *\t[ \t]*\\)\\(\\S-+\\)"
  "Regular expression to match macro definition lines the phrases buffer.")
;(defconst reftex-index-phrases-macrodef-regexp
;  "^\\(>>>INDEX_MACRO_DEFINITION:\\)[ \t]+\\(\\S-\\)\\([ \t]*\\)\\([^\t]*[^ \t]\\)\\([ \t]*\\)\\(nil\\|t\\)[ \t]*$"
;  "Regular expression to match macro definition lines the phrases buffer.
;This version would allow just spaces as separators.")
(defconst reftex-index-phrases-phrase-regexp1
  "^\\(\\S-?\\)\\(\t\\)\\([^\t\n]*\\S-\\)\\([ \t]*\\)$"
  "Regular expression matching phrases which have no separate index key.")
(defconst reftex-index-phrases-phrase-regexp2
  "^\\(\\S-?\\)\\(\t\\)\\([^\t]*\\S-\\)\\(\t[ \t]*\\)\\([^\n\t]*\\S-\\)[ \t]*$"
  "Regular expression matching phrases which have a separate index key.")
(defconst reftex-index-phrases-phrase-regexp12
  "^\\(\\S-?\\)\\(\t\\)\\([^\n\t]*\\S-\\)\\(\\(\t[ \t]*\\)\\([^\n\t]*\\S-\\)\\)?[ \t]*$"
  "Regular expression matching all types of phrase lines.")
(defvar reftex-index-phrases-macro-data nil
  "Alist containing the information taken from the macro definition lines.
This gets refreshed in every phrases command.")
(defvar reftex-index-phrases-files nil
  "List of document files relevant for the phrases file.")

(defvar reftex-index-phrases-font-lock-keywords nil
  "Font lock keywords for reftex-index-phrases-mode.")
(defvar reftex-index-phrases-font-lock-defaults nil
  "Font lock defaults for reftex-index-phrases-mode.")
(defvar reftex-index-phrases-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Keybindings and Menu for phrases buffer
    (loop for x in
          '(("\C-c\C-c" . reftex-index-phrases-save-and-return)
            ("\C-c\C-x" . reftex-index-this-phrase)
            ("\C-c\C-f" . reftex-index-next-phrase)
            ("\C-c\C-r" . reftex-index-region-phrases)
            ("\C-c\C-a" . reftex-index-all-phrases)
            ("\C-c\C-d" . reftex-index-remaining-phrases)
            ("\C-c\C-s" . reftex-index-sort-phrases)
            ("\C-c\C-n" . reftex-index-new-phrase)
            ("\C-c\C-m" . reftex-index-phrases-set-macro-key)
            ("\C-c\C-i" . reftex-index-phrases-info)
            ("\C-c\C-t" . reftex-index-find-next-conflict-phrase)
            ("\C-i"     . self-insert-command))
          do (define-key map (car x) (cdr x)))

    (easy-menu-define reftex-index-phrases-menu map
      "Menu for Phrases buffer"
      '("Phrases"
        ["New Phrase" reftex-index-new-phrase t]
        ["Set Phrase Macro" reftex-index-phrases-set-macro-key t]
        ["Recreate File Header" reftex-index-initialize-phrases-buffer t]
        "--"
        ("Sort Phrases"
         ["Sort" reftex-index-sort-phrases t]
         "--"
         "Sort Options"
         ["by Search Phrase" (setq reftex-index-phrases-sort-prefers-entry nil)
          :style radio :selected (not reftex-index-phrases-sort-prefers-entry)]
         ["by Index Entry" (setq reftex-index-phrases-sort-prefers-entry t)
          :style radio :selected reftex-index-phrases-sort-prefers-entry]
         ["in Blocks" (setq reftex-index-phrases-sort-in-blocks
                            (not reftex-index-phrases-sort-in-blocks))
          :style toggle :selected reftex-index-phrases-sort-in-blocks])
        ["Describe Phrase" reftex-index-phrases-info t]
        ["Next Phrase Conflict" reftex-index-find-next-conflict-phrase t]
        "--"
        ("Find and Index in Document"
         ["Current Phrase" reftex-index-this-phrase t]
         ["Next Phrase" reftex-index-next-phrase t]
         ["Current and Following" reftex-index-remaining-phrases t]
         ["Region Phrases" reftex-index-region-phrases t]
         ["All Phrases" reftex-index-all-phrases t]
         "--"
         "Options"
         ["Match Whole Words" (setq reftex-index-phrases-search-whole-words
                                    (not reftex-index-phrases-search-whole-words))
          :style toggle :selected reftex-index-phrases-search-whole-words]
         ["Case Sensitive Search" (setq reftex-index-phrases-case-fold-search
                                        (not  reftex-index-phrases-case-fold-search))
          :style toggle :selected (not
                                   reftex-index-phrases-case-fold-search)]
         ["Wrap Long Lines" (setq reftex-index-phrases-wrap-long-lines
                                  (not reftex-index-phrases-wrap-long-lines))
          :style toggle :selected reftex-index-phrases-wrap-long-lines]
         ["Skip Indexed Matches" (setq reftex-index-phrases-skip-indexed-matches
                                       (not reftex-index-phrases-skip-indexed-matches))
          :style toggle :selected reftex-index-phrases-skip-indexed-matches])
        "--"
        ["Save and Return" reftex-index-phrases-save-and-return t]))

    map)
  "Keymap used for *toc* buffer.")
(define-obsolete-variable-alias
  'reftex-index-phrases-map 'reftex-index-phrases-mode-map "24.1")


(defun reftex-index-phrase-selection-or-word (arg)
  "Add current selection or word at point to the phrases buffer.
When you are in transient-mark-mode and the region is active, the
selection will be used - otherwise the word at point.
You get a chance to edit the entry in the phrases buffer - finish with
`C-c C-c'."
  (interactive "P")
  (set-marker reftex-index-return-marker (point))
  (reftex-index-selection-or-word arg 'phrase)
  (if (eq major-mode 'reftex-index-phrases-mode)
      (message "%s"
       (substitute-command-keys
        "Return to LaTeX with \\[reftex-index-phrases-save-and-return]"))))

(defun reftex-index-visit-phrases-buffer ()
  "Switch to the phrases buffer, initialize if empty."
  (interactive)
  (reftex-access-scan-info)
  (let* ((master (reftex-TeX-master-file))
         (name (concat (file-name-sans-extension master)
                       reftex-index-phrase-file-extension)))
    (find-file name)
    (unless (eq major-mode 'reftex-index-phrases-mode)
      (reftex-index-phrases-mode))
    (if (= (buffer-size) 0)
        (reftex-index-initialize-phrases-buffer master))))

(defun reftex-index-initialize-phrases-buffer (&optional master)
  "Initialize the phrases buffer by creating the header.
If the buffer is non-empty, delete the old header first."
  (interactive)
  (let* ((case-fold-search t)
         (default-key (car reftex-index-default-macro))
         (default-macro (nth 1 (assoc default-key
                                      reftex-key-to-index-macro-alist)))
         (macro-alist
          (sort (copy-sequence reftex-index-macro-alist)
                (lambda (a b) (equal (car a) default-macro))))
         macro entry key repeat)

    (if master (set (make-local-variable 'TeX-master)
                    (file-name-nondirectory master)))

    (when (> (buffer-size) 0)
      (goto-char 1)
      (set-mark (point))
      (while (re-search-forward reftex-index-phrases-macrodef-regexp nil t)
        (end-of-line))
      (beginning-of-line 2)
      (if (looking-at reftex-index-phrases-comment-regexp)
          (beginning-of-line 2))
      (while (looking-at "^[ \t]*$")
          (beginning-of-line 2))
      (if (featurep 'xemacs)
	  (zmacs-activate-region)
	(setq mark-active t))
      (if (yes-or-no-p "Delete and rebuild header? ")
          (delete-region (point-min) (point))))

    ;; Insert the mode line
    (insert
     (format "%% -*- mode: reftex-index-phrases; TeX-master: \"%s\" -*-\n"
             (file-name-nondirectory (reftex-index-phrase-tex-master))))
    ;; Insert the macro definitions
    (insert "%                              Key      Macro Format            Repeat\n")
    (insert "%---------------------------------------------------------------------\n")
    (while (setq entry (pop macro-alist))
      (setq macro (car entry)
            repeat (nth 7 entry)
            key (car (delq nil (mapcar (lambda (x) (if (equal (nth 1 x) macro)
                                                       (car x)
                                                     nil))
                                       reftex-key-to-index-macro-alist))))
      (insert (format ">>>INDEX_MACRO_DEFINITION:\t%s\t%-20s\t%s\n"
                      (char-to-string key) (concat macro "{%s}")
                      (if repeat "t" "nil"))))
    (insert "%---------------------------------------------------------------------\n\n\n")))

(defvar TeX-master)
(defun reftex-index-phrase-tex-master (&optional dir)
  "Return the name of the master file associated with a phrase buffer."
  (if (and (boundp 'TeX-master)
           (local-variable-p 'TeX-master (current-buffer))
           (stringp TeX-master))
      ;; We have a local variable which tells us which file to use
      (expand-file-name TeX-master dir)
    ;; have to guess
    (concat (file-name-sans-extension (buffer-file-name)) ".tex")))

(defun reftex-index-phrases-save-and-return ()
  "Return to where the `reftex-index-phrase-selection-or-word' was called."
  (interactive)
  (save-buffer)
  (switch-to-buffer (marker-buffer reftex-index-return-marker))
  (goto-char (or (marker-position reftex-index-return-marker) (point))))


(defvar reftex-index-phrases-menu)
(defvar reftex-index-phrases-marker)
(defvar reftex-index-phrases-restrict-file nil)
;;;###autoload
(define-derived-mode reftex-index-phrases-mode fundamental-mode "Phrases"
  "Major mode for managing the Index phrases of a LaTeX document.
This buffer was created with RefTeX.

To insert new phrases, use
 - `C-c \\' in the LaTeX document to copy selection or word
 - `\\[reftex-index-new-phrase]' in the phrases buffer.

To index phrases use one of:

\\[reftex-index-this-phrase]     index current phrase
\\[reftex-index-next-phrase]     index next phrase (or N with prefix arg)
\\[reftex-index-all-phrases]     index all phrases
\\[reftex-index-remaining-phrases]     index current and following phrases
\\[reftex-index-region-phrases]     index the phrases in the region

You can sort the phrases in this buffer with \\[reftex-index-sort-phrases].
To display information about the phrase at point, use \\[reftex-index-phrases-info].

For more information see the RefTeX User Manual.

Here are all local bindings.

\\{reftex-index-phrases-mode-map}"
  (set (make-local-variable 'font-lock-defaults)
       reftex-index-phrases-font-lock-defaults)
  (easy-menu-add reftex-index-phrases-menu reftex-index-phrases-mode-map)
  (set (make-local-variable 'reftex-index-phrases-marker) (make-marker)))
;; (add-hook 'reftex-index-phrases-mode-hook 'turn-on-font-lock)

;; Font Locking stuff
(let ((ss (if (featurep 'xemacs) 'secondary-selection ''secondary-selection)))
  (setq reftex-index-phrases-font-lock-keywords
        (list
         (cons reftex-index-phrases-comment-regexp 'font-lock-comment-face)
         (list reftex-index-phrases-macrodef-regexp
               '(1 font-lock-type-face)
               '(2 font-lock-keyword-face)
               (list 3 ss)
               '(4 font-lock-function-name-face)
               (list 5 ss)
               '(6 font-lock-string-face))
         (list reftex-index-phrases-phrase-regexp1
               '(1 font-lock-keyword-face)
               (list 2 ss)
               '(3 font-lock-string-face)
               (list 4 ss))
         (list reftex-index-phrases-phrase-regexp2
               '(1 font-lock-keyword-face)
               (list 2 ss)
               '(3 font-lock-string-face)
               (list 4 ss)
               '(5 font-lock-function-name-face))
         (cons "^\t$" ss)))
  (setq reftex-index-phrases-font-lock-defaults
        '((reftex-index-phrases-font-lock-keywords)
          nil t nil beginning-of-line))
  (put 'reftex-index-phrases-mode 'font-lock-defaults
       reftex-index-phrases-font-lock-defaults) ; XEmacs
  )

(defun reftex-index-next-phrase (&optional arg)
  "Index the next ARG phrases in the phrases buffer."
  (interactive "p")
  (reftex-index-phrases-parse-header t)
  (while (> arg 0)
    (decf arg)
    (end-of-line)
    (if (re-search-forward reftex-index-phrases-phrase-regexp12 nil t)
        (progn
          (goto-char (match-beginning 0))
          (reftex-index-this-phrase 'slave))
      (error "No more phrase lines after point"))))

(defun reftex-index-this-phrase (&optional slave)
  "Index the phrase in the current line.
Does a global search and replace in the entire document.  At each
match, the user will be asked to confirm the replacement."
  (interactive)
  (if (not slave) (reftex-index-phrases-parse-header t))
  (save-excursion
    (beginning-of-line)
    (cond ((looking-at reftex-index-phrases-comment-regexp)
           (if (not slave) (error "Comment line")))
          ((looking-at "^[ \t]*$")
           (if (not slave) (error "Empty line")))
          ((looking-at reftex-index-phrases-macrodef-regexp)
           (if (not slave) (error "Macro definition line")))
          ((looking-at reftex-index-phrases-phrase-regexp12)
           ;; This is a phrase
           (let* ((char (if (not (equal (match-string 1) ""))
                            (string-to-char (match-string 1))))
                  (phrase (match-string 3))
                  (index-key (match-string 6))
                  (macro-data (cdr (if (null char)
                                       (car reftex-index-phrases-macro-data)
                                     (assoc char reftex-index-phrases-macro-data))))
                  (macro-fmt (car macro-data))
                  (repeat (nth 1 macro-data))
                  (files
                   (cond ((and (stringp reftex-index-phrases-restrict-file)
                               (file-regular-p reftex-index-phrases-restrict-file))
                          (list reftex-index-phrases-restrict-file))
                         ((stringp reftex-index-phrases-restrict-file)
                          (error "Invalid restriction file %s"
                                 reftex-index-phrases-restrict-file))
                         (t reftex-index-phrases-files)))
                  (as-words reftex-index-phrases-search-whole-words))
             (unless macro-data
               (error "No macro associated with key %c" char))
             (unwind-protect
                 (let ((overlay-arrow-string "=>")
                       (overlay-arrow-position
                        reftex-index-phrases-marker)
                       (replace-count 0))
                   ;; Show the overlay arrow
                   (move-marker reftex-index-phrases-marker
                                (match-beginning 0) (current-buffer))
                   ;; Start the query-replace
                   (reftex-query-index-phrase-globally
                    files phrase macro-fmt
                    index-key repeat as-words)
                   (message "%s replaced"
                            (reftex-number replace-count "occurrence"))))))
          (t (error "Cannot parse this line")))))

(defun reftex-index-all-phrases ()
  "Index all phrases in the phrases buffer.
Calls `reftex-index-this-phrase' on each line in the buffer."
  (interactive)
  (reftex-index-region-phrases (point-min) (point-max)))

(defun reftex-index-remaining-phrases ()
  "Index all phrases in the phrases buffer.
Calls `reftex-index-this-phrase' on each line ay and below point in
the buffer."
  (interactive)
  (beginning-of-line)
  (reftex-index-region-phrases (point) (point-max)))

(defun reftex-index-region-phrases (beg end)
  "Index all phrases in the phrases buffer.
Calls `reftex-index-this-phrase' on each line in the region."
  (interactive "r")
  (reftex-index-phrases-parse-header t)
  (goto-char beg)
  (while (not (or (eobp)
                  (>= (point) end)))
    (save-excursion (reftex-index-this-phrase 'slave))
    (beginning-of-line 2)))

(defun reftex-index-phrases-parse-header (&optional get-files)
  "Parse the header of a phrases file to extract the macro definitions.
The definitions get stored in `reftex-index-phrases-macro-data'.
Also switches to the LaTeX document to find out which files belong to
the document and stores the list in `reftex-index-phrases-files'."
  (let* ((master (reftex-index-phrase-tex-master))
         buf)
    (if get-files
        ;; Get the file list
        (save-excursion
          (setq buf (reftex-get-file-buffer-force master))
          (unless buf (error "Master file %s not found" master))
          (set-buffer buf)
          (reftex-access-scan-info)
          (setq reftex-index-phrases-files
                (reftex-all-document-files))))
    ;; Parse the files header for macro definitions
    (setq reftex-index-phrases-macro-data nil)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward reftex-index-phrases-macrodef-regexp nil t)
        (push (list
               (string-to-char (match-string 2))
               (match-string 4)
               (equal (match-string 6) "t"))
              reftex-index-phrases-macro-data))
      ;; Reverse the list, so that the first macro is first
      (if (null reftex-index-phrases-macro-data)
          (error "No valid MACRO DEFINITION line in %s file (make sure to use TAB separators)" reftex-index-phrase-file-extension))
      (setq reftex-index-phrases-macro-data
            (nreverse reftex-index-phrases-macro-data))
      (goto-char (point-min)))))

(defun reftex-index-phrases-apply-to-region (beg end)
  "Index all index phrases in the current region.
This works exactly like global indexing from the index phrases buffer,
but operation is restricted to the current region.  This is useful if
you need to add/change text in an already indexed document and want to
index the new part without having to go over the unchanged parts again."
  (interactive "r")
  (let ((win-conf (current-window-configuration))
        (reftex-index-phrases-restrict-file (buffer-file-name)))
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (unwind-protect
          (progn
            ;; Hide the region highlighting
            (if (featurep 'xemacs)
		(zmacs-deactivate-region)
	      (deactivate-mark))
            (delete-other-windows)
            (reftex-index-visit-phrases-buffer)
            (reftex-index-all-phrases))
        (set-window-configuration win-conf))))))

(defun reftex-index-new-phrase (&optional text)
  "Open a new line in the phrases buffer, insert TEXT."
  (interactive)
  (if (and text (stringp text))
      (progn
        ;; Check if the phrase is already in the buffer
        (setq text (reftex-index-simplify-phrase text))
        (goto-char (point-min))
        (if (re-search-forward
             (concat "^\\(\\S-*\\)\t\\(" (regexp-quote text)
                     "\\) *[\t\n]") nil t)
            (progn
              (goto-char (match-end 2))
              (error "Phrase is already in phrases buffer")))))
  ;; Add the new phrase line after the last in the buffer
  (goto-char (point-max))
  (if (re-search-backward reftex-index-phrases-phrase-regexp12 nil t)
      (end-of-line))
  (if (not (bolp))
      (insert "\n"))
  (insert "\t")
  (if (and text (stringp text))
      (insert text)))

(defun reftex-index-find-next-conflict-phrase (&optional arg)
  "Find the next a phrase which is has conflicts in the phrase buffer.
The command helps to find possible conflicts in the phrase indexing process.
It searches downward from point for a phrase which is repeated elsewhere
in the buffer, or which is a subphrase of another phrase.  If such a
phrase is found, the phrase info is displayed.
To check the whole buffer, start at the beginning and continue by calling
this function repeatedly."
  (interactive "P")
  (if (catch 'exit
        (while (re-search-forward reftex-index-phrases-phrase-regexp12 nil t)
          (goto-char (match-beginning 3))
          (let* ((phrase (match-string 3))
                 (case-fold-search reftex-index-phrases-case-fold-search)
                 (re (reftex-index-phrases-find-dup-re phrase t)))
            (if (save-excursion
                  (goto-char (point-min))
                  (and (re-search-forward re nil t)
                       (re-search-forward re nil t)))
                (throw 'exit t)))))
      (progn
        (reftex-index-phrases-info)
        (message "Phrase with match conflict discovered"))
    (goto-char (point-max))
    (error "No further problematic phrases found")))

(defun reftex-index-phrases-info ()
  "Display information about the phrase at point."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (unless (looking-at reftex-index-phrases-phrase-regexp12)
      (error "Not a phrase line"))
    (save-match-data (reftex-index-phrases-parse-header t))
    (let* ((char (if (not (equal (match-string 1) ""))
                     (string-to-char (match-string 1))))
           (phrase (match-string 3))
           (index-key (match-string 6))
           (index-keys (split-string
                        (or index-key phrase)
                        reftex-index-phrases-logical-or-regexp))
           (macro-data (cdr (if (null char)
                                (car reftex-index-phrases-macro-data)
                              (assoc char reftex-index-phrases-macro-data))))
           (macro-fmt (car macro-data))
           (repeat (nth 1 macro-data))
           (as-words reftex-index-phrases-search-whole-words)
           (example (reftex-index-make-replace-string
                     macro-fmt (downcase phrase) (car index-keys) repeat))
           (re (reftex-index-make-phrase-regexp phrase as-words t))
           (re1 (reftex-index-phrases-find-dup-re phrase))
           (re2 (reftex-index-phrases-find-dup-re phrase 'sub))
           superphrases
           (nmatches 0)
           (ntimes1 0)
           (ntimes2 0)
           (case-fold-search reftex-index-phrases-case-fold-search)
           file files buf)
      (setq files reftex-index-phrases-files)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (while (re-search-forward re1 nil t)
            (incf ntimes1))
          (goto-char (point-min))
          (while (re-search-forward re2 nil t)
            (push (cons (count-lines 1 (point)) (match-string 1)) superphrases)
            (incf ntimes2))))
      (save-current-buffer
        (while (setq file (pop files))
          (setq buf (reftex-get-file-buffer-force file))
          (when buf
            (set-buffer buf)
            (save-excursion
              (save-restriction
                (widen)
                (goto-char (point-min))
                (let ((case-fold-search reftex-index-phrases-case-fold-search))
                  (while (re-search-forward re nil t)
                    (or (reftex-in-comment)
                        (incf nmatches)))))))))
      (with-output-to-temp-buffer "*Help*"
        (princ (format "       Phrase:  %s\n" phrase))
        (princ (format "    Macro key:  %s\n" char))
        (princ (format " Macro format:  %s\n" macro-fmt))
        (princ (format "       Repeat:  %s\n" repeat))
        (cond
         (index-key
          (let ((iks index-keys) (cnt 0) ik)
            (while (setq ik (pop iks))
              (princ (format "Index entry %d:  %s\n" (incf cnt) ik)))))
         (repeat
          (princ (format "  Index entry:  %s\n" phrase)))
         (t
          (princ (format "    Index key:  <<Given by the match>>\n"))))
        (princ (format "      Example:  %s\n" example))
        (terpri)
        (princ (format "Total matches:  %s in %s\n"
                       (reftex-number nmatches "match" "es")
                       (reftex-number (length reftex-index-phrases-files)
                                      "LaTeX file")))
        (princ (format "   Uniqueness:  Phrase occurs %s in phrase buffer\n"
                       (reftex-number ntimes1 "time")))
        (if (> ntimes2 1)
            (progn
              (princ (format " Superphrases:  Phrase matches the following %s in the phrase buffer:\n"
                             (reftex-number ntimes2 "line")))
              (mapcar (lambda(x)
                        (princ (format "                Line %4d:  %s\n" (car x) (cdr x))))
                      (nreverse superphrases))))))))

(defun reftex-index-phrases-set-macro-key ()
  "Change the macro key for the current line.
Prompts for a macro key and insert is at the beginning of the line.
If you reply with SPACE, the macro keyn will be removed, so that the
default macro will be used.  If you reply with `RET', just prints
information about the currently selected macro."
  (interactive)
  (reftex-index-phrases-parse-header)
  (save-excursion
    (beginning-of-line)
    (unless (or (looking-at reftex-index-phrases-phrase-regexp12)
                (looking-at "\t"))
      (error "This is not a phrase line"))
    (let* ((nc (reftex-index-select-phrases-macro 0))
           (macro-data (assoc nc reftex-index-phrases-macro-data))
           macro-fmt repeat)
      (cond (macro-data)
            ((equal nc ?\ )
             (setq nc ""
                   macro-data (car reftex-index-phrases-macro-data)))
            ((equal nc ?\C-m)
             (setq nc (char-after (point)))
             (if (equal nc ?\t)
                 (setq nc ""
                       macro-data (car reftex-index-phrases-macro-data))
               (setq macro-data (assoc nc reftex-index-phrases-macro-data))))
            (t (error "No macro associated with %c" nc)))

      (setq macro-fmt (nth 1 macro-data)
            repeat (nth 2 macro-data))
      (if macro-data
          (progn
            (if (looking-at "[^\t]") (delete-char 1))
            (insert nc)
            (message "Line will use %s %s repeat" macro-fmt
                     (if repeat "with" "without")))
        (error "Abort")))))

(defun reftex-index-sort-phrases (&optional chars-first)
  "Sort the phrases lines in the buffer alphabetically.
Normally, this looks only at the phrases.  With a prefix arg CHARS-FIRST,
it first compares the macro identifying chars and then the phrases."
  (interactive "P")
  ;; Remember the current line, so that we can return
  (let ((line (buffer-substring (progn (beginning-of-line) (point))
                                (progn (end-of-line) (point))))
        beg end)
    (goto-char (point-min))
    ;; Find first and last phrase line in buffer
    (setq beg
          (and (re-search-forward reftex-index-phrases-phrase-regexp12 nil t)
               (match-beginning 0)))
    (goto-char (point-max))
    (setq end (re-search-backward reftex-index-phrases-phrase-regexp12 nil t))
    (if end (setq end (progn (goto-char end) (end-of-line) (point))))
    ;; Take the lines, sort them and re-insert.
    (if (and beg end)
        (progn
          (message "Sorting lines...")
          (let* ((lines (split-string (buffer-substring beg end) "\n"))
                 (lines1 (sort lines 'reftex-compare-phrase-lines)))
            (message "Sorting lines...done")
            (let ((inhibit-quit t))  ;; make sure we do not lose lines
              (delete-region beg end)
              (insert (mapconcat 'identity lines1 "\n"))))
          (goto-char (point-max))
          (re-search-backward (concat "^" (regexp-quote line) "$") nil t))
      (error "Cannot find phrases lines to sort"))))

(defvar chars-first)
(defun reftex-compare-phrase-lines (a b)
  "The comparison function used for sorting."
  (let (ca cb pa pb c-p p-p)
    (if (string-match reftex-index-phrases-phrase-regexp12 a)
        (progn
          ;; Extract macro char and phrase-or-key for a
          (setq ca (match-string 1 a)
                pa (downcase
                    (or (and reftex-index-phrases-sort-prefers-entry
                             (match-string 6 a))
                        (match-string 3 a))))
          (if (string-match reftex-index-phrases-phrase-regexp12 b)
              (progn
                ;; Extract macro char and phrase-or-key for b
                (setq cb (match-string 1 b)
                      pb (downcase
                          (or (and reftex-index-phrases-sort-prefers-entry
                                   (match-string 6 b))
                              (match-string 3 b))))
                (setq c-p (string< ca cb)
                      p-p (string< pa pb))
                ;; Do the right comparison, based on the value of `chars-first'
                ;; `chars-first' is bound locally in the calling function
                (if chars-first
                    (if (string= ca cb) p-p c-p)
                  (if (string= pa pb) c-p p-p)))))
      ;; If line a does not match, the answer we return determines
      ;; if non-matching lines are collected at the beginning.
      ;; When we return t here, non-matching lines form
      ;; block separators for searches.
      (not reftex-index-phrases-sort-in-blocks))))

(defvar reftex-index-phrases-menu)
(defun reftex-index-make-phrase-regexp (phrase &optional
                                               as-words allow-newline)
  "Return a regexp matching PHRASE, even if distributed over lines.
With optional arg AS-WORDS, require word boundary at beginning and end.
With optional arg ALLOW-NEWLINE, allow single newline between words."
  (let* ((words (split-string phrase))
         (space-re (if allow-newline
                       "\\([ \t]*\\(\n[ \t]*\\)?\\|[ \t]\\)"
                     "\\([ \t]+\\)")))
    (concat (if (and as-words (string-match "\\`\\w" (car words)))
                "\\(\\<\\|[`']\\)" "")
            (mapconcat (lambda (w) (regexp-quote
                                    (if reftex-index-phrases-case-fold-search
                                        (downcase w)
                                      w)))
                       words space-re)
            (if (and as-words
                     (string-match "\\w\\'" (nth (1- (length words)) words)))
                "\\(\\>\\|'\\)" ""))))

(defun reftex-index-simplify-phrase (phrase)
  "Make phrase single spaces and single line."
  (mapconcat 'identity (split-string phrase) " "))

(defun reftex-index-phrases-find-dup-re (phrase &optional sub)
  "Return a regexp which matches variations of PHRASE (with additional space).
When SUB ins non-nil, the regexp will also match when PHRASE is a subphrase
of another phrase.  The regexp works lonly in the phrase buffer."
  (concat (if sub "^\\S-?\t\\([^\t\n]*" "^\\S-?\t")
          (mapconcat 'regexp-quote (split-string phrase) " +")
          (if sub "[^\t\n]*\\)\\([\t\n]\\|$\\)" " *\\([\t\n]\\|$\\)")))

(defun reftex-index-make-replace-string (macro-fmt match index-key
                                                   &optional repeat mathp)
  "Return the string which can be used as replacement.
Treats the logical `and' for index phrases."
  (let ((index-keys (split-string (or index-key match)
                                  reftex-index-phrases-logical-and-regexp)))
    (concat
     (mapconcat (lambda (x)
                  (format macro-fmt
                          (format (if mathp reftex-index-math-format "%s") x)))
                index-keys "")
   (if repeat (reftex-index-simplify-phrase match) ""))))

(defun reftex-query-index-phrase-globally (files &rest args)
  "Call `reftex-query-index-phrase' for all files in FILES."
  (let ((win-conf (current-window-configuration))
        (file))
    (unless files (error "No files"))
    (unwind-protect
        (progn
          (switch-to-buffer-other-window (reftex-get-file-buffer-force
                                          (car files)))
          (catch 'no-more-files
            (while (setq file (pop files))
              (switch-to-buffer (reftex-get-file-buffer-force file))
              (save-excursion
                (save-restriction
                  (unless (stringp reftex-index-phrases-restrict-file)
                    (widen))
                  (goto-char (point-min))
                  (apply 'reftex-query-index-phrase args))))))
      (reftex-unhighlight 0)
      (set-window-configuration win-conf))))

(defconst reftex-index-phrases-help
  "     Keys for query-index search
     ===========================
y       Replace this match
n       Skip this match
!       Replace this and all further matches in this file
q / Q   Skip match, start next file / start next phrase
o       Use a different indexing macro for this match
1 - 9   Select one of the multiple phrases
e       Edit the replacement text
C-r     Recursive edit.
s / S   Save this buffer  /  Save all document buffers
C-g     Abort"
  "The help string for indexing phrases.")

(defvar replace-count)
(defun reftex-query-index-phrase (phrase macro-fmt &optional
                                         index-key repeat as-words)
  "Search through buffer for PHRASE, and offer to replace it with an indexed
version.  The index version is derived by applying `format' with MACRO-FMT
to INDEX-KEY or PHRASE.  When REPEAT is non-nil, the PHRASE is inserted
again after the macro.
AS-WORDS means, the search for PHRASE should require word boundaries at
both ends."
  (let* ((re (reftex-index-make-phrase-regexp phrase as-words 'allow-newline))
         (case-fold-search reftex-index-phrases-case-fold-search)
         (index-keys (split-string
                      (or index-key phrase)
                      reftex-index-phrases-logical-or-regexp))
         (nkeys (length index-keys))
         (ckey (nth 0 index-keys))
         (all-yes nil)
         match rpl char (beg (make-marker)) (end (make-marker)) mathp)
    (move-marker beg 1)
    (move-marker end 1)
    (unwind-protect
        (while (re-search-forward re nil t)
          (catch 'next-match
            (if (reftex-in-comment)
                (throw 'next-match nil))
            (if (and (fboundp reftex-index-verify-function)
                     (not (funcall reftex-index-verify-function)))
                (throw 'next-match nil))
            (setq match (match-string 0))
            (setq mathp
                  (save-match-data
                    (condition-case nil (texmathp) (error nil))))
            (setq beg (move-marker beg (match-beginning 0))
                  end (move-marker end (match-end 0)))
            (if (and reftex-index-phrases-skip-indexed-matches
                     (save-match-data
                       (reftex-index-phrase-match-is-indexed beg
                                                             end)))
                (throw 'next-match nil))
            (reftex-highlight 0 (match-beginning 0) (match-end 0))
            (setq rpl
                  (save-match-data
                    (reftex-index-make-replace-string
                     macro-fmt (match-string 0) ckey repeat mathp)))
            (while
                (not
                 (catch 'loop
                   (message "REPLACE: %s?   (yn!qoe%s?)"
                            rpl
                            (if (> nkeys 1)
                                (concat "1-" (int-to-string nkeys))
                              ""))
                   (setq char (if all-yes ?y (read-char-exclusive)))
                   (cond ((member char '(?y ?Y ?\ ))
                          ;; Yes!
                          (replace-match rpl t t)
                          (incf replace-count)
                          ;; See if we should insert newlines to shorten lines
                          (and reftex-index-phrases-wrap-long-lines
                               (reftex-index-phrases-fixup-line beg end))
                          (throw 'loop t))
                         ((member char '(?n ?N ?\C-h ?\C-?));; FIXME: DEL
                          ;; No
                          (throw 'loop t))
                         ((equal char ?!)
                          ;; Yes for all in this buffer
                          (setq all-yes t))
                         ((equal char ?q)
                          ;; Stop this one in this file
                          (goto-char (point-max))
                          (throw 'loop t))
                         ((equal char ?Q)
                          ;; Stop this one
                          (throw 'no-more-files t))
                         ((equal char ?s)
                          (save-buffer))
                         ((equal char ?S)
                          (reftex-save-all-document-buffers))
                         ((equal char ?\C-g)
                          (keyboard-quit))
                         ((member char '(?o ?O))
                          ;; Select a different macro
                          (let* ((nc (reftex-index-select-phrases-macro 2))
                                 (macro-data
                                  (cdr (assoc nc reftex-index-phrases-macro-data)))
                                 (macro-fmt (car macro-data))
                                 (repeat (nth 1 macro-data)))
                            (if macro-data
                                (setq rpl (save-match-data
                                            (reftex-index-make-replace-string
                                             macro-fmt match
                                             ckey repeat mathp)))
                              (ding))))
                         ((equal char ?\?)
                          ;; Help
                          (with-output-to-temp-buffer "*Help*"
                            (princ reftex-index-phrases-help)))
                         ((equal char ?\C-r)
                          ;; Recursive edit
                          (save-match-data
                            (save-excursion
                              (message "%s"
                               (substitute-command-keys
                                "Recursive edit.  Resume with \\[exit-recursive-edit]"))
                              (recursive-edit))))
                         ((equal char ?e)
                          (setq rpl (read-string "Edit: " rpl)))
                         ((equal char ?0)
                          (setq ckey (or index-key phrase)
                                rpl (save-match-data
                                      (reftex-index-make-replace-string
                                       macro-fmt match ckey repeat mathp))))
                         ((and (> char ?0)
                               (<= char (+ ?0 nkeys)))
                          (setq ckey (nth (1- (- char ?0)) index-keys)
                                rpl (save-match-data
                                      (reftex-index-make-replace-string
                                       macro-fmt match ckey repeat mathp))))
                         (t (ding)))
                   nil)))))
      (message "")
      (move-marker beg nil)
      (move-marker end nil)
      (setq all-yes nil)
      (reftex-unhighlight 0))))

(defun reftex-index-phrase-match-is-indexed (beg end)
  ;; Check if match is in an argument of an index macro, or if an
  ;; index macro is directly attached to the match.
  (save-excursion
    (goto-char end)
    (let* ((all-macros (reftex-what-macro t))
;           (this-macro (car (car all-macros)))
           (before-macro
            (and (> beg 2)
                 (goto-char (1- beg))
                 (memq (char-after (point)) '(?\] ?\}))
                 (car (reftex-what-macro 1))))
           (after-macro
            (and (goto-char end)
                 (looking-at "\\(\\\\[a-zA-Z]+\\*?\\)[[{]")
                 (match-string 1)))
           macro)
      (or (catch 'matched
            (while (setq macro (pop all-macros))
              (if (member (car macro) reftex-macros-with-index)
                  (throw 'matched t)))
            nil)
          (and before-macro
               (member before-macro reftex-macros-with-index))
          (and after-macro
               (member after-macro reftex-macros-with-index))))))

(defun reftex-index-phrases-fixup-line (beg end)
  "Insert newlines before BEG and/or after END to shorten line."
  (let (bol eol space1 space2)
    (save-excursion
      ;; Find line boundaries and possible line breaks near BEG and END
      (beginning-of-line)
      (setq bol (point))
      (end-of-line)
      (setq eol (point))
      (goto-char beg)
      (skip-chars-backward "^ \n")
      (if (and (equal (preceding-char) ?\ )
               (string-match "\\S-" (buffer-substring bol (point))))
          (setq space1 (1- (point))))
      (goto-char end)
      (skip-chars-forward "^ \n")
      (if (and (equal (following-char) ?\ )
               (string-match "\\S-" (buffer-substring (point) eol)))
          (setq space2 (point)))
      ;; Now check what we have and insert the newlines
      (if (<= (- eol bol) fill-column)
          ;; Line is already short
          nil
        (cond
         ((and (not space1) (not space2))) ; No spaces available
         ((not space2)                  ; Do space1
          (reftex-index-phrases-replace-space space1))
         ((not space1)                  ; Do space2
          (reftex-index-phrases-replace-space space2))
         (t ; We have both spaces
          (let ((l1 (- space1 bol))
                (l2 (- space2 space1))
                (l3 (- eol space2)))
            (if (> l2 fill-column)
                ;; The central part alone is more than one line
                (progn
                  (reftex-index-phrases-replace-space space1)
                  (reftex-index-phrases-replace-space space2))
              (if (> (+ l1 l2) fill-column)
                  ;; Need to split beginning
                  (reftex-index-phrases-replace-space space1))
              (if (> (+ l2 l3) fill-column)
                  ;; Need to split end
                  (reftex-index-phrases-replace-space space2))))))))))

(defun reftex-index-phrases-replace-space (pos)
  "If there is a space at POS, replace it with a newline char.
Does not do a save-excursion."
  (when (equal (char-after pos) ?\ )
    (goto-char pos)
    (delete-char 1)
    (insert "\n")))

(defun reftex-index-select-phrases-macro (&optional delay)
  "Offer a list of possible index macros and have the user select one."
  (let* ((prompt (concat "Select macro: ["
                         (mapconcat (lambda (x) (char-to-string (car x)))
                                    reftex-index-phrases-macro-data "")
                         "] "))
         (help (concat "Select an indexing macro\n========================\n"
                       (mapconcat (lambda (x)
                                    (format " [%c]     %s"
                                            (car x) (nth 1 x)))
                                  reftex-index-phrases-macro-data "\n"))))
    (reftex-select-with-char prompt help delay)))


;;; reftex-index.el ends here
