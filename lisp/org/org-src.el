;;; org-src.el --- Source code examples in Org
;;
;; Copyright (C) 2004-2012 Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten at orgmode dot org>
;;	   Bastien Guerry <bzg AT gnu DOT org>
;;         Dan Davison <davison at stats dot ox dot ac dot uk>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains the code dealing with source code examples in Org-mode.

;;; Code:

(require 'org-macs)
(require 'org-compat)
(require 'ob-keys)
(require 'ob-comint)
(eval-when-compile
  (require 'cl))

(declare-function org-do-remove-indentation "org" (&optional n))
(declare-function org-at-table.el-p "org" ())
(declare-function org-get-indentation "org" (&optional line))
(declare-function org-switch-to-buffer-other-window "org" (&rest args))
(declare-function org-pop-to-buffer-same-window
		  "org-compat" (&optional buffer-or-name norecord label))
(declare-function org-strip-protective-commas "org" (beg end))
(declare-function org-base-buffer "org" (buffer))

(defcustom org-edit-src-region-extra nil
  "Additional regexps to identify regions for editing with `org-edit-src-code'.
For examples see the function `org-edit-src-find-region-and-lang'.
The regular expression identifying the begin marker should end with a newline,
and the regexp marking the end line should start with a newline, to make sure
there are kept outside the narrowed region."
  :group 'org-edit-structure
  :type '(repeat
	  (list
	   (regexp :tag "begin regexp")
	   (regexp :tag "end regexp")
	   (choice :tag "language"
		   (string :tag "specify")
		   (integer :tag "from match group")
		   (const :tag "from `lang' element")
		   (const :tag "from `style' element")))))

(defcustom org-coderef-label-format "(ref:%s)"
  "The default coderef format.
This format string will be used to search for coderef labels in literal
examples (EXAMPLE and SRC blocks).  The format can be overwritten in
an individual literal example with the -l option, like

#+BEGIN_SRC pascal +n -r -l \"((%s))\"
...
#+END_SRC

If you want to use this for HTML export, make sure that the format does
not introduce special font-locking, and avoid the HTML special
characters `<', `>', and `&'.  The reason for this restriction is that
the labels are searched for only after htmlize has done its job."
  :group 'org-edit-structure ; FIXME this is not in the right group
  :type 'string)

(defcustom org-edit-fixed-width-region-mode 'artist-mode
  "The mode that should be used to edit fixed-width regions.
These are the regions where each line starts with a colon."
  :group 'org-edit-structure
  :type '(choice
	  (const artist-mode)
	  (const picture-mode)
	  (const fundamental-mode)
	  (function :tag "Other (specify)")))

(defcustom org-src-preserve-indentation nil
  "If non-nil preserve leading whitespace characters on export.
If non-nil leading whitespace characters in source code blocks
are preserved on export, and when switching between the org
buffer and the language mode edit buffer.  If this variable is nil
then, after editing with \\[org-edit-src-code], the
minimum (across-lines) number of leading whitespace characters
are removed from all lines, and the code block is uniformly
indented according to the value of `org-edit-src-content-indentation'."
  :group 'org-edit-structure
  :type 'boolean)

(defcustom org-edit-src-content-indentation 2
  "Indentation for the content of a source code block.
This should be the number of spaces added to the indentation of the #+begin
line in order to compute the indentation of the block content after
editing it with \\[org-edit-src-code].  Has no effect if
`org-src-preserve-indentation' is non-nil."
  :group 'org-edit-structure
  :type 'integer)

(defvar org-src-strip-leading-and-trailing-blank-lines nil
  "If non-nil, blank lines are removed when exiting the code edit
buffer.")

(defcustom org-edit-src-persistent-message t
  "Non-nil means show persistent exit help message while editing src examples.
The message is shown in the header-line, which will be created in the
first line of the window showing the editing buffer."
  :group 'org-edit-structure
  :type 'boolean)

(defcustom org-src-window-setup 'reorganize-frame
  "How the source code edit buffer should be displayed.
Possible values for this option are:

current-window    Show edit buffer in the current window, keeping all other
                  windows.
other-window      Use `switch-to-buffer-other-window' to display edit buffer.
reorganize-frame  Show only two windows on the current frame, the current
                  window and the edit buffer. When exiting the edit buffer,
                  return to one window.
other-frame       Use `switch-to-buffer-other-frame' to display edit buffer.
                  Also, when exiting the edit buffer, kill that frame."
  :group 'org-edit-structure
  :type '(choice
	  (const current-window)
	  (const other-frame)
	  (const other-window)
	  (const reorganize-frame)))

(defvar org-src-mode-hook nil
  "Hook  run after Org switched a source code snippet to its Emacs mode.
This hook will run

- when editing a source code snippet with \"C-c '\".
- When formatting a source code snippet for export with htmlize.

You may want to use this hook for example to turn off `outline-minor-mode'
or similar things which you want to have when editing a source code file,
but which mess up the display of a snippet in Org exported files.")

(defcustom org-src-lang-modes
  '(("ocaml" . tuareg) ("elisp" . emacs-lisp) ("ditaa" . artist)
    ("asymptote" . asy) ("dot" . fundamental) ("sqlite" . sql)
    ("calc" . fundamental) ("C" . c) ("cpp" . c++)
    ("screen" . shell-script))
  "Alist mapping languages to their major mode.
The key is the language name, the value is the string that should
be inserted as the name of the major mode.  For many languages this is
simple, but for language where this is not the case, this variable
provides a way to simplify things on the user side.
For example, there is no ocaml-mode in Emacs, but the mode to use is
`tuareg-mode'."
  :group 'org-edit-structure
  :type '(repeat
	  (cons
	   (string "Language name")
	   (symbol "Major mode"))))

;;; Editing source examples

(defvar org-src-mode-map (make-sparse-keymap))
(define-key org-src-mode-map "\C-c'" 'org-edit-src-exit)

(defvar org-edit-src-force-single-line nil)
(defvar org-edit-src-from-org-mode nil)
(defvar org-edit-src-allow-write-back-p t)
(defvar org-edit-src-picture nil)
(defvar org-edit-src-beg-marker nil)
(defvar org-edit-src-end-marker nil)
(defvar org-edit-src-overlay nil)
(defvar org-edit-src-block-indentation nil)
(defvar org-edit-src-saved-temp-window-config nil)

(defvar org-src-ask-before-returning-to-edit-buffer t
  "If nil, when org-edit-src code is used on a block that already
  has an active edit buffer, it will switch to that edit buffer
  immediately; otherwise it will ask whether you want to return
  to the existing edit buffer.")

(defvar org-src-babel-info nil)

(define-minor-mode org-src-mode
  "Minor mode for language major mode buffers generated by org.
This minor mode is turned on in two situations:
- when editing a source code snippet with \"C-c '\".
- When formatting a source code snippet for export with htmlize.
There is a mode hook, and keybindings for `org-edit-src-exit' and
`org-edit-src-save'")

(defun org-edit-src-code (&optional context code edit-buffer-name)
  "Edit the source code example at point.
The example is copied to a separate buffer, and that buffer is
switched to the correct language mode.  When done, exit with
\\[org-edit-src-exit].  This will remove the original code in the
Org buffer, and replace it with the edited version. Optional
argument CONTEXT is used by \\[org-edit-src-save] when calling
this function. See \\[org-src-window-setup] to configure the
display of windows containing the Org buffer and the code
buffer."
  (interactive)
  (unless (eq context 'save)
    (setq org-edit-src-saved-temp-window-config (current-window-configuration)))
  (let* ((mark (and (org-region-active-p) (mark)))
	 (case-fold-search t)
	 (info (org-edit-src-find-region-and-lang))
	 (full-info (org-babel-get-src-block-info 'light))
	 (org-mode-p (derived-mode-p 'org-mode)) ;; derived-mode-p is reflexive
	 (beg (make-marker))
	 (end (make-marker))
	 (allow-write-back-p (null code))
	 block-nindent total-nindent ovl lang lang-f single lfmt buffer msg
	 begline markline markcol line col transmitted-variables)
    (if (not info)
	nil
      (setq beg (move-marker beg (nth 0 info))
	    end (move-marker end (nth 1 info))
	    msg (if allow-write-back-p
		    (substitute-command-keys
		     "Edit, then exit with C-c ' (C-c and single quote)")
		  "Exit with C-c ' (C-c and single quote)")
	    code (or code (buffer-substring-no-properties beg end))
	    lang (or (cdr (assoc (nth 2 info) org-src-lang-modes))
                     (nth 2 info))
	    lang (if (symbolp lang) (symbol-name lang) lang)
	    single (nth 3 info)
	    block-nindent (nth 5 info)
	    lang-f (intern (concat lang "-mode"))
	    begline (save-excursion (goto-char beg) (org-current-line))
	    transmitted-variables
	    `((org-edit-src-content-indentation
	       ,org-edit-src-content-indentation)
	      (org-edit-src-force-single-line ,single)
	      (org-edit-src-from-org-mode ,org-mode-p)
	      (org-edit-src-allow-write-back-p ,allow-write-back-p)
	      (org-src-preserve-indentation ,org-src-preserve-indentation)
	      (org-src-babel-info ,(org-babel-get-src-block-info 'light))
	      (org-coderef-label-format
	       ,(or (nth 4 info) org-coderef-label-format))
	      (org-edit-src-beg-marker ,beg)
	      (org-edit-src-end-marker ,end)
	      (org-edit-src-block-indentation ,block-nindent)))
      (if (and mark (>= mark beg) (<= mark (1+ end)))
	  (save-excursion (goto-char (min mark end))
			  (setq markline (org-current-line)
				markcol (current-column))))
      (if (equal lang-f 'table.el-mode)
	  (setq lang-f (lambda ()
			 (text-mode)
			 (if (org-bound-and-true-p flyspell-mode)
			     (flyspell-mode -1))
			 (table-recognize)
			 (org-set-local 'org-edit-src-content-indentation 0))))
      (unless (functionp lang-f)
	(error "No such language mode: %s" lang-f))
      (save-excursion
	(if (> (point) end) (goto-char end))
	(setq line (org-current-line)
	      col (current-column)))
      (if (and (setq buffer (org-edit-src-find-buffer beg end))
	       (if org-src-ask-before-returning-to-edit-buffer
		   (y-or-n-p "Return to existing edit buffer? [n] will revert changes: ") t))
	  (org-src-switch-to-buffer buffer 'return)
	(when buffer
	  (with-current-buffer buffer
	    (if (boundp 'org-edit-src-overlay)
		(delete-overlay org-edit-src-overlay)))
	  (kill-buffer buffer))
	(setq buffer (generate-new-buffer
		      (or edit-buffer-name
			  (org-src-construct-edit-buffer-name (buffer-name) lang))))
	(setq ovl (make-overlay beg end))
	(overlay-put ovl 'edit-buffer buffer)
	(overlay-put ovl 'help-echo "Click with mouse-1 to switch to buffer editing this segment")
	(overlay-put ovl 'face 'secondary-selection)
	(overlay-put ovl
		     'keymap
		     (let ((map (make-sparse-keymap)))
		       (define-key map [mouse-1] 'org-edit-src-continue)
		       map))
	(overlay-put ovl :read-only "Leave me alone")
	(setq transmitted-variables
	      (append transmitted-variables `((org-edit-src-overlay ,ovl))))
	(org-src-switch-to-buffer buffer 'edit)
	(if (eq single 'macro-definition)
	    (setq code (replace-regexp-in-string "\\\\n" "\n" code t t)))
	(insert code)
	(remove-text-properties (point-min) (point-max)
				'(display nil invisible nil intangible nil))
	(unless (cadr (assq 'org-src-preserve-indentation transmitted-variables))
	  (setq total-nindent (or (org-do-remove-indentation) 0)))
	(let ((org-inhibit-startup t))
	  (condition-case e
	      (funcall lang-f)
	    (error
	     (error "Language mode `%s' fails with: %S" lang-f (nth 1 e)))))
	(dolist (pair transmitted-variables)
	  (org-set-local (car pair) (cadr pair)))
	(if (eq major-mode 'org-mode)
	    (progn
	      (goto-char (point-min))
	      (while (re-search-forward "^," nil t)
		(if (eq (org-current-line) line) (setq total-nindent (1+ total-nindent)))
		(replace-match "")))
	  (org-strip-protective-commas (point-min) (point-max)))
	(when markline
	  (org-goto-line (1+ (- markline begline)))
	  (org-move-to-column
	   (if org-src-preserve-indentation markcol
	     (max 0 (- markcol total-nindent))))
	  (push-mark (point) 'no-message t)
	  (setq deactivate-mark nil))
	(org-goto-line (1+ (- line begline)))
	(org-move-to-column
	 (if org-src-preserve-indentation col (max 0 (- col total-nindent))))
	(org-src-mode)
	(set-buffer-modified-p nil)
	(and org-edit-src-persistent-message
	     (org-set-local 'header-line-format msg))
	(let ((edit-prep-func (intern (concat "org-babel-edit-prep:" lang))))
	  (when (fboundp edit-prep-func)
	    (funcall edit-prep-func full-info))))
      t)))

(defun org-edit-src-continue (e)
  (interactive "e")
  (mouse-set-point e)
  (let ((buf (get-char-property (point) 'edit-buffer)))
    (if buf (org-src-switch-to-buffer buf 'continue)
      (error "Something is wrong here"))))

(defun org-src-switch-to-buffer (buffer context)
  (case org-src-window-setup
    ('current-window
     (org-pop-to-buffer-same-window buffer))
    ('other-window
     (switch-to-buffer-other-window buffer))
    ('other-frame
     (case context
       ('exit
	(let ((frame (selected-frame)))
	  (switch-to-buffer-other-frame buffer)
	  (delete-frame frame)))
       ('save
	(kill-buffer (current-buffer))
	(org-pop-to-buffer-same-window buffer))
       (t
	(switch-to-buffer-other-frame buffer))))
    ('reorganize-frame
     (if (eq context 'edit) (delete-other-windows))
     (org-switch-to-buffer-other-window buffer)
     (if (eq context 'exit) (delete-other-windows)))
    ('switch-invisibly
     (set-buffer buffer))
    (t
     (message "Invalid value %s for org-src-window-setup"
	      (symbol-name org-src-window-setup))
     (org-pop-to-buffer-same-window buffer))))

(defun org-src-construct-edit-buffer-name (org-buffer-name lang)
  "Construct the buffer name for a source editing buffer."
  (concat "*Org Src " org-buffer-name "[ " lang " ]*"))

(defun org-src-edit-buffer-p (&optional buffer)
  "Test whether BUFFER (or the current buffer if BUFFER is nil)
is a source block editing buffer."
  (let ((buffer (org-base-buffer (or buffer (current-buffer)))))
    (and (buffer-name buffer)
	 (string-match "\\`*Org Src " (buffer-name buffer))
	 (local-variable-p 'org-edit-src-beg-marker buffer)
	 (local-variable-p 'org-edit-src-end-marker buffer))))

(defun org-edit-src-find-buffer (beg end)
  "Find a source editing buffer that is already editing the region BEG to END."
  (catch 'exit
    (mapc
     (lambda (b)
       (with-current-buffer b
	 (if (and (string-match "\\`*Org Src " (buffer-name))
		  (local-variable-p 'org-edit-src-beg-marker (current-buffer))
		  (local-variable-p 'org-edit-src-end-marker (current-buffer))
		  (equal beg org-edit-src-beg-marker)
		  (equal end org-edit-src-end-marker))
	     (throw 'exit (current-buffer)))))
     (buffer-list))
    nil))

(defun org-edit-fixed-width-region ()
  "Edit the fixed-width ascii drawing at point.
This must be a region where each line starts with a colon followed by
a space character.
An new buffer is created and the fixed-width region is copied into it,
and the buffer is switched into `artist-mode' for editing.  When done,
exit with \\[org-edit-src-exit].  The edited text will then replace
the fragment in the Org-mode buffer."
  (interactive)
  (let ((line (org-current-line))
	(col (current-column))
	(case-fold-search t)
	(msg (substitute-command-keys
	      "Edit, then exit with C-c ' (C-c and single quote)"))
	(org-mode-p (eq major-mode 'org-mode))
	(beg (make-marker))
	(end (make-marker))
	(preserve-indentation org-src-preserve-indentation)
	block-nindent ovl beg1 end1 code begline buffer)
    (beginning-of-line 1)
    (if (looking-at "[ \t]*[^:\n \t]")
	nil
      (if (looking-at "[ \t]*\\(\n\\|\\'\\)")
	  (setq beg1 (point) end1 beg1)
	(save-excursion
	  (if (re-search-backward "^[ \t]*[^: \t]" nil 'move)
	      (setq beg1 (point-at-bol 2))
	    (setq beg1 (point))))
	(save-excursion
	  (if (re-search-forward "^[ \t]*[^: \t]" nil 'move)
	      (setq end1 (1- (match-beginning 0)))
	    (setq end1 (point))))
	(org-goto-line line))
      (setq beg (move-marker beg beg1)
	    end (move-marker end end1)
	    code (buffer-substring-no-properties beg end)
	    begline (save-excursion (goto-char beg) (org-current-line)))
      (if (and (setq buffer (org-edit-src-find-buffer beg end))
	       (y-or-n-p "Return to existing edit buffer? [n] will revert changes: "))
	  (org-pop-to-buffer-same-window buffer)
	(when buffer
	  (with-current-buffer buffer
	    (if (boundp 'org-edit-src-overlay)
		(delete-overlay org-edit-src-overlay)))
	  (kill-buffer buffer))
	(setq buffer (generate-new-buffer
		      (org-src-construct-edit-buffer-name
		       (buffer-name) "Fixed Width")))
	(setq ovl (make-overlay beg end))
	(overlay-put ovl 'face 'secondary-selection)
	(overlay-put ovl 'edit-buffer buffer)
	(overlay-put ovl 'help-echo "Click with mouse-1 to switch to buffer editing this segment")
	(overlay-put ovl 'face 'secondary-selection)
	(overlay-put ovl
			 'keymap
			 (let ((map (make-sparse-keymap)))
			   (define-key map [mouse-1] 'org-edit-src-continue)
			   map))
	(overlay-put ovl :read-only "Leave me alone")
	(org-pop-to-buffer-same-window buffer)
	(insert code)
	(remove-text-properties (point-min) (point-max)
				'(display nil invisible nil intangible nil))
	(setq block-nindent (or (org-do-remove-indentation) 0))
	(cond
	 ((eq org-edit-fixed-width-region-mode 'artist-mode)
	  (fundamental-mode)
	  (artist-mode 1))
	 (t (funcall org-edit-fixed-width-region-mode)))
	(set (make-local-variable 'org-edit-src-force-single-line) nil)
	(set (make-local-variable 'org-edit-src-from-org-mode) org-mode-p)
	(set (make-local-variable 'org-edit-src-picture) t)
	(goto-char (point-min))
	(while (re-search-forward "^[ \t]*: ?" nil t)
	  (replace-match ""))
	(org-goto-line (1+ (- line begline)))
	(org-move-to-column (max 0 (- col block-nindent 2)))
	(org-set-local 'org-edit-src-beg-marker beg)
	(org-set-local 'org-edit-src-end-marker end)
	(org-set-local 'org-edit-src-overlay ovl)
	(org-set-local 'org-edit-src-block-indentation block-nindent)
	(org-set-local 'org-edit-src-content-indentation 0)
	(org-set-local 'org-src-preserve-indentation nil)
	(org-src-mode)
	(set-buffer-modified-p nil)
	(and org-edit-src-persistent-message
	     (org-set-local 'header-line-format msg)))
      (message "%s" msg)
      t)))

(defun org-edit-src-find-region-and-lang ()
  "Find the region and language for a local edit.
Return a list with beginning and end of the region, a string representing
the language, a switch telling if the content should be in a single line."
  (let ((re-list
	 (append
	  org-edit-src-region-extra
	  '(
	    ("<src\\>[^<]*>[ \t]*\n?" "\n?[ \t]*</src>" lang)
	    ("<literal\\>[^<]*>[ \t]*\n?" "\n?[ \t]*</literal>" style)
	    ("<example>[ \t]*\n?" "\n?[ \t]*</example>" "fundamental")
	    ("<lisp>[ \t]*\n?" "\n?[ \t]*</lisp>" "emacs-lisp")
	    ("<perl>[ \t]*\n?" "\n?[ \t]*</perl>" "perl")
	    ("<python>[ \t]*\n?" "\n?[ \t]*</python>" "python")
	    ("<ruby>[ \t]*\n?" "\n?[ \t]*</ruby>" "ruby")
	    ("^[ \t]*#\\+begin_src\\( \\([^ \t\n]+\\)\\)?.*\n" "\n[ \t]*#\\+end_src" 2)
	    ("^[ \t]*#\\+begin_example.*\n" "\n[ \t]*#\\+end_example" "fundamental")
	    ("^[ \t]*#\\+html:" "\n" "html" single-line)
	    ("^[ \t]*#\\+begin_html.*\n" "\n[ \t]*#\\+end_html" "html")
	    ("^[ \t]*#\\+latex:" "\n" "latex" single-line)
	    ("^[ \t]*#\\+begin_latex.*\n" "\n[ \t]*#\\+end_latex" "latex")
	    ("^[ \t]*#\\+ascii:" "\n" "fundamental" single-line)
	    ("^[ \t]*#\\+begin_ascii.*\n" "\n[ \t]*#\\+end_ascii" "fundamental")
	    ("^[ \t]*#\\+docbook:" "\n" "xml" single-line)
	    ("^[ \t]*#\\+macro:[ \t]+\\S-+\\( \\|$\\)"
	     "\n" "fundamental" macro-definition)
	    ("^[ \t]*#\\+begin_docbook.*\n" "\n[ \t]*#\\+end_docbook" "xml")
	    )))
	(pos (point))
	re1 re2 single beg end lang lfmt match-re1 ind entry)
    (catch 'exit
      (while (setq entry (pop re-list))
	(setq re1 (car entry) re2 (nth 1 entry) lang (nth 2 entry)
	      single (nth 3 entry))
	(save-excursion
	  (if (or (looking-at re1)
		  (re-search-backward re1 nil t))
	      (progn
		(setq match-re1 (match-string 0))
		(setq beg (match-end 0)
		      lang (org-edit-src-get-lang lang)
		      lfmt (org-edit-src-get-label-format match-re1)
		      ind (org-edit-src-get-indentation (match-beginning 0)))
		(if (and (re-search-forward re2 nil t)
			 (>= (match-end 0) pos))
		    (throw 'exit (list beg (match-beginning 0)
				       lang single lfmt ind))))
	    (if (or (looking-at re2)
		    (re-search-forward re2 nil t))
		(progn
		  (setq end (match-beginning 0))
		  (if (and (re-search-backward re1 nil t)
			   (<= (match-beginning 0) pos))
		      (progn
			(setq lfmt (org-edit-src-get-label-format
				    (match-string 0))
			      ind (org-edit-src-get-indentation
				   (match-beginning 0)))
			(throw 'exit
			       (list (match-end 0) end
				     (org-edit-src-get-lang lang)
				     single lfmt ind)))))))))
      (when (org-at-table.el-p)
	(re-search-backward "^[\t]*[^ \t|\\+]" nil t)
	(setq beg (1+ (point-at-eol)))
	(goto-char beg)
	(or (re-search-forward "^[\t]*[^ \t|\\+]" nil t)
	    (progn (goto-char (point-max)) (newline)))
	(setq end (point-at-bol))
	(setq ind (org-edit-src-get-indentation beg))
	(throw 'exit (list beg end 'table.el nil nil ind))))))

(defun org-edit-src-get-lang (lang)
  "Extract the src language."
  (let ((m (match-string 0)))
    (cond
     ((stringp lang) lang)
     ((integerp lang) (match-string lang))
     ((and (eq lang 'lang)
	   (string-match "\\<lang=\"\\([^ \t\n\"]+\\)\"" m))
      (match-string 1 m))
     ((and (eq lang 'style)
	   (string-match "\\<style=\"\\([^ \t\n\"]+\\)\"" m))
      (match-string 1 m))
     (t "fundamental"))))

(defun org-edit-src-get-label-format (s)
  "Extract the label format."
  (save-match-data
    (if (string-match "-l[ \t]+\\\\?\"\\([^\t\r\n\"]+\\)\\\\?\"" s)
	(match-string 1 s))))

(defun org-edit-src-get-indentation (pos)
  "Count leading whitespace characters on line."
  (save-match-data
    (goto-char pos)
    (org-get-indentation)))

(defun org-edit-src-exit (&optional context)
  "Exit special edit and protect problematic lines."
  (interactive)
  (unless (org-bound-and-true-p org-edit-src-from-org-mode)
    (error "This is not a sub-editing buffer, something is wrong"))
  (widen)
  (let* ((beg org-edit-src-beg-marker)
	 (end org-edit-src-end-marker)
	 (ovl org-edit-src-overlay)
	 (buffer (current-buffer))
	 (single (org-bound-and-true-p org-edit-src-force-single-line))
	 (macro (eq single 'macro-definition))
	 (total-nindent (+ (or org-edit-src-block-indentation 0)
			   org-edit-src-content-indentation))
	 (preserve-indentation org-src-preserve-indentation)
	 (allow-write-back-p (org-bound-and-true-p org-edit-src-allow-write-back-p))
	 (delta 0) code line col indent)
    (when allow-write-back-p
      (unless preserve-indentation (untabify (point-min) (point-max)))
      (if org-src-strip-leading-and-trailing-blank-lines
	  (save-excursion
	    (goto-char (point-min))
	    (if (looking-at "[ \t\n]*\n") (replace-match ""))
	    (unless macro
	      (if (re-search-forward "\n[ \t\n]*\\'" nil t) (replace-match ""))))))
    (setq line (if (org-bound-and-true-p org-edit-src-force-single-line)
		   1
		 (org-current-line))
	  col (current-column))
    (when allow-write-back-p
      (when single
	(goto-char (point-min))
	(if (re-search-forward "\\s-+\\'" nil t) (replace-match ""))
	(goto-char (point-min))
	(let ((cnt 0))
	  (while (re-search-forward "\n" nil t)
	    (setq cnt (1+ cnt))
	    (replace-match (if macro "\\n" " ") t t))
	  (when (and macro (> cnt 0))
	    (goto-char (point-max)) (insert "\\n")))
	(goto-char (point-min))
	(if (looking-at "\\s-*") (replace-match " ")))
      (when (org-bound-and-true-p org-edit-src-from-org-mode)
	(goto-char (point-min))
	(while (re-search-forward
		(if (eq major-mode 'org-mode) "^\\(.\\)" "^\\([*]\\|[ \t]*#\\+\\)") nil t)
	  (if (eq (org-current-line) line) (setq delta (1+ delta)))
	  (replace-match ",\\1")))
      (when (org-bound-and-true-p org-edit-src-picture)
	(setq preserve-indentation nil)
	(untabify (point-min) (point-max))
	(goto-char (point-min))
	(while (re-search-forward "^" nil t)
	  (replace-match ": ")))
      (unless (or single preserve-indentation (= total-nindent 0))
	(setq indent (make-string total-nindent ?\ ))
	(goto-char (point-min))
	(while (re-search-forward "^" nil t)
	  (replace-match indent)))
      (if (org-bound-and-true-p org-edit-src-picture)
	  (setq total-nindent (+ total-nindent 2)))
      (setq code (buffer-string))
      (set-buffer-modified-p nil))
    (org-src-switch-to-buffer (marker-buffer beg) (or context 'exit))
    (kill-buffer buffer)
    (goto-char beg)
    (when allow-write-back-p
      (delete-region beg end)
      (insert code)
      (goto-char beg)
      (if single (just-one-space)))
    (if (memq t (mapcar (lambda (overlay)
			  (eq (overlay-get overlay 'invisible)
			      'org-hide-block))
			(overlays-at (point))))
	;; Block is hidden; put point at start of block
	(beginning-of-line 0)
      ;; Block is visible, put point where it was in the code buffer
      (org-goto-line (1- (+ (org-current-line) line)))
      (org-move-to-column (if preserve-indentation col (+ col total-nindent delta))))
    (move-marker beg nil)
    (move-marker end nil))
  (unless (eq context 'save)
    (when org-edit-src-saved-temp-window-config
      (set-window-configuration org-edit-src-saved-temp-window-config)
      (setq org-edit-src-saved-temp-window-config nil))))

(defun org-edit-src-save ()
  "Save parent buffer with current state source-code buffer."
  (interactive)
  (let ((p (point)) (m (mark)) msg)
    (save-window-excursion
      (org-edit-src-exit 'save)
      (save-buffer)
      (setq msg (current-message))
      (if (eq org-src-window-setup 'other-frame)
	  (let ((org-src-window-setup 'current-window))
	    (org-edit-src-code 'save))
	(org-edit-src-code 'save)))
    (push-mark m 'nomessage)
    (goto-char (min p (point-max)))
    (message (or msg ""))))

(defun org-src-mode-configure-edit-buffer ()
  (when (org-bound-and-true-p org-edit-src-from-org-mode)
    (org-add-hook 'kill-buffer-hook
		  #'(lambda () (delete-overlay org-edit-src-overlay)) nil 'local)
    (if (org-bound-and-true-p org-edit-src-allow-write-back-p)
	(progn
	  (setq buffer-offer-save t)
	  (setq buffer-file-name
		(concat (buffer-file-name (marker-buffer org-edit-src-beg-marker))
			"[" (buffer-name) "]"))
	  (if (featurep 'xemacs)
	      (progn
		(make-variable-buffer-local 'write-contents-hooks) ; needed only for 21.4
		(setq write-contents-hooks '(org-edit-src-save)))
	    (setq write-contents-functions '(org-edit-src-save))))
      (setq buffer-read-only t))))

(org-add-hook 'org-src-mode-hook 'org-src-mode-configure-edit-buffer)


(defun org-src-associate-babel-session (info)
  "Associate edit buffer with comint session."
  (interactive)
  (let ((session (cdr (assoc :session (nth 2 info)))))
    (and session (not (string= session "none"))
	 (org-babel-comint-buffer-livep session)
	 ((lambda (f) (and (fboundp f) (funcall f session)))
	  (intern (format "org-babel-%s-associate-session" (nth 0 info)))))))

(defun org-src-babel-configure-edit-buffer ()
  (when org-src-babel-info
    (org-src-associate-babel-session org-src-babel-info)))

(org-add-hook 'org-src-mode-hook 'org-src-babel-configure-edit-buffer)
(defmacro org-src-do-at-code-block (&rest body)
  "Execute a command from an edit buffer in the Org-mode buffer."
  `(let ((beg-marker org-edit-src-beg-marker))
     (if beg-marker
	 (with-current-buffer (marker-buffer beg-marker)
	   (goto-char (marker-position beg-marker))
	   ,@body))))
(def-edebug-spec org-src-do-at-code-block (body))

(defun org-src-do-key-sequence-at-code-block (&optional key)
  "Execute key sequence at code block in the source Org buffer.
The command bound to KEY in the Org-babel key map is executed
remotely with point temporarily at the start of the code block in
the Org buffer.

This command is not bound to a key by default, to avoid conflicts
with language major mode bindings. To bind it to C-c @ in all
language major modes, you could use

  (add-hook 'org-src-mode-hook
            (lambda () (define-key org-src-mode-map \"\\C-c@\"
                    'org-src-do-key-sequence-at-code-block)))

In that case, for example, C-c @ t issued in code edit buffers
would tangle the current Org code block, C-c @ e would execute
the block and C-c @ h would display the other available
Org-babel commands."
  (interactive "kOrg-babel key: ")
  (if (equal key (kbd "C-g")) (keyboard-quit)
    (org-edit-src-save)
    (org-src-do-at-code-block
     (call-interactively
      (lookup-key org-babel-map key)))))

(defcustom org-src-tab-acts-natively nil
  "If non-nil, the effect of TAB in a code block is as if it were
issued in the language major mode buffer."
  :type 'boolean
  :version "24.1"
  :group 'org-babel)

(defun org-src-native-tab-command-maybe ()
  "Perform language-specific TAB action.
Alter code block according to effect of TAB in the language major
mode."
  (and org-src-tab-acts-natively
       (let ((org-src-strip-leading-and-trailing-blank-lines nil))
	 (org-babel-do-key-sequence-in-edit-buffer (kbd "TAB")))))

(add-hook 'org-tab-first-hook 'org-src-native-tab-command-maybe)

(defun org-src-font-lock-fontify-block (lang start end)
  "Fontify code block.
This function is called by emacs automatic fontification, as long
as `org-src-fontify-natively' is non-nil. For manual
fontification of code blocks see `org-src-fontify-block' and
`org-src-fontify-buffer'"
  (let ((lang-mode (org-src-get-lang-mode lang)))
    (if (fboundp lang-mode)
	(let ((string (buffer-substring-no-properties start end))
	      (modified (buffer-modified-p))
	      (org-buffer (current-buffer)) pos next)
	  (remove-text-properties start end '(face nil))
	  (with-current-buffer
	      (get-buffer-create
	       (concat " org-src-fontification:" (symbol-name lang-mode)))
	    (delete-region (point-min) (point-max))
	    (insert (concat string " ")) ;; so there's a final property change
	    (unless (eq major-mode lang-mode) (funcall lang-mode))
	    (font-lock-fontify-buffer)
	    (setq pos (point-min))
	    (while (setq next (next-single-property-change pos 'face))
	      (put-text-property
	       (+ start (1- pos)) (+ start next) 'face
	       (get-text-property pos 'face) org-buffer)
	      (setq pos next)))
	  (add-text-properties
	   start end
	   '(font-lock-fontified t fontified t font-lock-multiline t))
	  (set-buffer-modified-p modified)))))

(defun org-src-fontify-block ()
  "Fontify code block at point."
  (interactive)
  (save-excursion
    (let ((org-src-fontify-natively t)
	  (info (org-edit-src-find-region-and-lang)))
      (font-lock-fontify-region (nth 0 info) (nth 1 info)))))

(defun org-src-fontify-buffer ()
  "Fontify all code blocks in the current buffer"
  (interactive)
  (org-babel-map-src-blocks nil
    (org-src-fontify-block)))

(defun org-src-get-lang-mode (lang)
  "Return major mode that should be used for LANG.
LANG is a string, and the returned major mode is a symbol."
  (intern
   (concat
    ((lambda (l) (if (symbolp l) (symbol-name l) l))
     (or (cdr (assoc lang org-src-lang-modes)) lang)) "-mode")))

(provide 'org-src)

;;; org-src.el ends here
