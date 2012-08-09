;;; semantic/senator.el --- SEmantic NAvigaTOR

;; Copyright (C) 2000-2012  Free Software Foundation, Inc.

;; Author: David Ponce <david@dponce.com>
;; Maintainer: FSF
;; Created: 10 Nov 2000
;; Keywords: syntax

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
;;
;; This file defines some user commands for navigating between
;; Semantic tags.  This is a subset of the version of senator.el in
;; the upstream CEDET package; the rest is incorporated into other
;; parts of Semantic or Emacs.

;;; Code:

(require 'ring)
(require 'semantic)
(require 'semantic/ctxt)
(require 'semantic/decorate)
(require 'semantic/format)

(eval-when-compile (require 'semantic/find))

;; (eval-when-compile (require 'hippie-exp))

(declare-function semantic-analyze-tag-references "semantic/analyze/refs")
(declare-function semantic-analyze-refs-impl "semantic/analyze/refs")
(declare-function semantic-analyze-find-tag "semantic/analyze")
(declare-function semantic-analyze-tag-type "semantic/analyze/fcn")
(declare-function semantic-tag-external-class "semantic/sort")
(declare-function imenu--mouse-menu "imenu")

;;; Customization
(defgroup senator nil
  "Semantic Navigator."
  :group 'semantic)

;;;###autoload
(defcustom senator-step-at-tag-classes nil
  "List of tag classes recognized by Senator's navigation commands.
A tag class is a symbol, such as `variable', `function', or `type'.

As a special exception, if the value is nil, Senator's navigation
commands recognize all tag classes."
  :group 'senator
  :type '(repeat (symbol)))
;;;###autoload
(make-variable-buffer-local 'senator-step-at-tag-classes)

;;;###autoload
(defcustom senator-step-at-start-end-tag-classes nil
  "List of tag classes at which Senator's navigation commands should stop.
A tag class is a symbol, such as `variable', `function', or `type'.
The navigation commands stop at the start and end of each tag
class in this list, provided the tag class is recognized (see
`senator-step-at-tag-classes').

As a special exception, if the value is nil, the navigation
commands stop at the beginning of every tag.

If t, the navigation commands stop at the start and end of any
tag, where possible."
  :group 'senator
  :type '(choice :tag "Identifiers"
                 (repeat :menu-tag "Symbols" (symbol))
                 (const  :tag "All" t)))
;;;###autoload
(make-variable-buffer-local 'senator-step-at-start-end-tag-classes)

(defcustom senator-highlight-found nil
  "If non-nil, Senator commands momentarily highlight found tags."
  :group 'senator
  :type 'boolean)
(make-variable-buffer-local 'senator-highlight-found)

;;; Faces
(defface senator-momentary-highlight-face
  '((((class color) (background dark))
     (:background "gray30"))
    (((class color) (background light))
     (:background "gray70")))
  "Face used to momentarily highlight tags."
  :group 'semantic-faces)

;;; Common functions

(defun senator-momentary-highlight-tag (tag)
  "Momentarily highlight TAG.
Does nothing if `senator-highlight-found' is nil."
  (and senator-highlight-found
       (semantic-momentary-highlight-tag
        tag 'senator-momentary-highlight-face)))

(defun senator-step-at-start-end-p (tag)
  "Return non-nil if must step at start and end of TAG."
  (and tag
       (or (eq senator-step-at-start-end-tag-classes t)
           (memq (semantic-tag-class tag)
                 senator-step-at-start-end-tag-classes))))

(defun senator-skip-p (tag)
  "Return non-nil if must skip TAG."
  (and tag
       senator-step-at-tag-classes
       (not (memq (semantic-tag-class tag)
                  senator-step-at-tag-classes))))

(defun senator-middle-of-tag-p (pos tag)
  "Return non-nil if POS is between start and end of TAG."
  (and (> pos (semantic-tag-start tag))
       (< pos (semantic-tag-end   tag))))

(defun senator-step-at-parent (tag)
  "Return TAG's outermost parent if must step at start/end of it.
Return nil otherwise."
  (if tag
      (let (parent parents)
        (setq parents (semantic-find-tag-by-overlay
                       (semantic-tag-start tag)))
        (while (and parents (not parent))
          (setq parent  (car parents)
                parents (cdr parents))
          (if (or (eq tag parent)
                  (senator-skip-p parent)
                  (not (senator-step-at-start-end-p parent)))
              (setq parent nil)))
        parent)))

(defun senator-previous-tag-or-parent (pos)
  "Return the tag before POS or one of its parent where to step."
  (let (ol tag)
    (while (and pos (> pos (point-min)) (not tag))
      (setq pos (semantic-overlay-previous-change pos))
      (when pos
        ;; Get overlays at position
        (setq ol (semantic-overlays-at pos))
        ;; find the overlay that belongs to semantic
        ;; and STARTS or ENDS at the found position.
        (while (and ol (not tag))
          (setq tag (semantic-overlay-get (car ol) 'semantic))
          (unless (and tag (semantic-tag-p tag)
                       (or (= (semantic-tag-start tag) pos)
                           (= (semantic-tag-end   tag) pos)))
            (setq tag nil
                  ol (cdr ol))))))
    (or (senator-step-at-parent tag) tag)))

;;; Search functions

(defun senator-search-tag-name (tag)
  "Search for TAG name in current buffer.
Limit the search to TAG bounds.
If found, set point to the end of the name, and return point.  The
beginning of the name is at (match-beginning 0).
Return nil if not found, that is if TAG name doesn't come from the
source."
  (let ((name (semantic-tag-name tag)))
    (setq name (if (string-match "\\`\\([^[]+\\)[[]" name)
                   (match-string 1 name)
                 name))
    (goto-char (semantic-tag-start tag))
    (when (re-search-forward (concat
                              ;; The tag name is expected to be
                              ;; between word delimiters, whitespace,
                              ;; or punctuation.
                              "\\(\\<\\|\\s-+\\|\\s.\\)"
                              (regexp-quote name)
                              "\\(\\>\\|\\s-+\\|\\s.\\)")
                             (semantic-tag-end tag)
                             t)
      (goto-char (match-beginning 0))
      (search-forward name))))

(defcustom senator-search-ignore-tag-classes
  '(code block)
  "List of ignored tag classes.
Tags of those classes are excluded from search."
  :group 'senator
  :type '(repeat (symbol :tag "class")))

(defun senator-search-default-tag-filter (tag)
  "Default function that filters searched tags.
Ignore tags of classes in `senator-search-ignore-tag-classes'"
  (not (memq (semantic-tag-class tag)
             senator-search-ignore-tag-classes)))

(defvar senator-search-tag-filter-functions
  '(senator-search-default-tag-filter)
  "List of functions to be called to filter searched tags.
Each function is passed a tag.  If one of them returns nil, the tag is
excluded from the search.")

(defun senator-search (searcher text &optional bound noerror count)
  "Use the SEARCHER function to search from point for TEXT in a tag name.
SEARCHER is typically the function `search-forward', `search-backward',
`word-search-forward', `word-search-backward', `re-search-forward', or
`re-search-backward'.  See one of the above function to see how the
TEXT, BOUND, NOERROR, and COUNT arguments are interpreted."
  (let* ((origin (point))
         (count  (or count 1))
         (step   (cond ((> count 0) 1)
                       ((< count 0) (setq count (- count)) -1)
                       (0)))
         found next sstart send tag tstart tend)
    (or (zerop step)
        (while (and (not found)
                    (setq next (funcall searcher text bound t step)))
          (setq sstart (match-beginning 0)
                send   (match-end 0))
          (if (= sstart send)
              (setq found t)
            (and (setq tag (semantic-current-tag))
                 (run-hook-with-args-until-failure
                  'senator-search-tag-filter-functions tag)
                 (setq tend   (senator-search-tag-name tag))
                 (setq tstart (match-beginning 0)
                       found  (and (>= sstart tstart)
                                   (<= send tend)
                                   (zerop (setq count (1- count))))))
            (goto-char next))))
    (cond ((null found)
           (setq next origin
                 send origin))
          ((= next sstart)
           (setq next send
                 send sstart))
          (t
           (setq next sstart)))
    (goto-char next)
    ;; Setup the returned value and the `match-data' or maybe fail!
    (funcall searcher text send noerror step)))

;;; Navigation commands

;;;###autoload
(defun senator-next-tag ()
  "Navigate to the next Semantic tag.
Return the tag or nil if at end of buffer."
  (interactive)
  (let ((pos (point))
        (tag (semantic-current-tag))
        where)
    (if (and tag
             (not (senator-skip-p tag))
             (senator-step-at-start-end-p tag)
             (or (= pos (semantic-tag-start tag))
                 (senator-middle-of-tag-p pos tag)))
        nil
      (if (setq tag (senator-step-at-parent tag))
          nil
        (setq tag (semantic-find-tag-by-overlay-next pos))
        (while (and tag (senator-skip-p tag))
          (setq tag (semantic-find-tag-by-overlay-next
                       (semantic-tag-start tag))))))
    (if (not tag)
        (progn
          (goto-char (point-max))
          (message "End of buffer"))
      (cond ((and (senator-step-at-start-end-p tag)
                  (or (= pos (semantic-tag-start tag))
                      (senator-middle-of-tag-p pos tag)))
             (setq where "end")
             (goto-char (semantic-tag-end tag)))
            (t
             (setq where "start")
             (goto-char (semantic-tag-start tag))))
      (senator-momentary-highlight-tag tag)
      (message "%S: %s (%s)"
	       (semantic-tag-class tag)
	       (semantic-tag-name  tag)
	       where))
    tag))

;;;###autoload
(defun senator-previous-tag ()
  "Navigate to the previous Semantic tag.
Return the tag or nil if at beginning of buffer."
  (interactive)
  (let ((pos (point))
        (tag (semantic-current-tag))
        where)
    (if (and tag
             (not (senator-skip-p tag))
             (senator-step-at-start-end-p tag)
             (or (= pos (semantic-tag-end tag))
                 (senator-middle-of-tag-p pos tag)))
        nil
      (if (setq tag (senator-step-at-parent tag))
          nil
        (setq tag (senator-previous-tag-or-parent pos))
        (while (and tag (senator-skip-p tag))
          (setq tag (senator-previous-tag-or-parent
                       (semantic-tag-start tag))))))
    (if (not tag)
        (progn
          (goto-char (point-min))
          (message "Beginning of buffer"))
      (cond ((or (not (senator-step-at-start-end-p tag))
                 (= pos (semantic-tag-end tag))
                 (senator-middle-of-tag-p pos tag))
             (setq where "start")
             (goto-char (semantic-tag-start tag)))
            (t
             (setq where "end")
             (goto-char (semantic-tag-end tag))))
      (senator-momentary-highlight-tag tag)
      (message "%S: %s (%s)"
	       (semantic-tag-class tag)
	       (semantic-tag-name  tag)
	       where))
    tag))

;;; Search commands

(defun senator-search-forward (string &optional bound noerror count)
  "Search in tag names forward from point for STRING.
Set point to the end of the occurrence found, and return point.
See also the function `search-forward' for details on the BOUND,
NOERROR and COUNT arguments."
  (interactive "sSemantic search: ")
  (senator-search 'search-forward string bound noerror count))

(defun senator-re-search-forward (regexp &optional bound noerror count)
  "Search in tag names forward from point for regular expression REGEXP.
Set point to the end of the occurrence found, and return point.
See also the function `re-search-forward' for details on the BOUND,
NOERROR and COUNT arguments."
  (interactive "sSemantic regexp search: ")
  (senator-search 're-search-forward regexp bound noerror count))

(defun senator-word-search-forward (word &optional bound noerror count)
  "Search in tag names forward from point for WORD.
Set point to the end of the occurrence found, and return point.
See also the function `word-search-forward' for details on the BOUND,
NOERROR and COUNT arguments."
  (interactive "sSemantic word search: ")
  (senator-search 'word-search-forward word bound noerror count))

(defun senator-search-backward (string &optional bound noerror count)
  "Search in tag names backward from point for STRING.
Set point to the beginning of the occurrence found, and return point.
See also the function `search-backward' for details on the BOUND,
NOERROR and COUNT arguments."
  (interactive "sSemantic backward search: ")
  (senator-search 'search-backward string bound noerror count))

(defun senator-re-search-backward (regexp &optional bound noerror count)
  "Search in tag names backward from point for regular expression REGEXP.
Set point to the beginning of the occurrence found, and return point.
See also the function `re-search-backward' for details on the BOUND,
NOERROR and COUNT arguments."
  (interactive "sSemantic backward regexp search: ")
  (senator-search 're-search-backward regexp bound noerror count))

(defun senator-word-search-backward (word &optional bound noerror count)
  "Search in tag names backward from point for WORD.
Set point to the beginning of the occurrence found, and return point.
See also the function `word-search-backward' for details on the BOUND,
NOERROR and COUNT arguments."
  (interactive "sSemantic backward word search: ")
  (senator-search 'word-search-backward word bound noerror count))

;;; Other useful search commands (minor mode menu)

(defvar senator-last-search-type nil
  "Type of last non-incremental search command called.")

(defun senator-nonincremental-repeat-search-forward ()
  "Search forward for the previous search string or regexp."
  (interactive)
  (cond
   ((and (eq senator-last-search-type 'string)
         search-ring)
    (senator-search-forward (car search-ring)))
   ((and (eq senator-last-search-type 'regexp)
         regexp-search-ring)
    (senator-re-search-forward (car regexp-search-ring)))
   (t
    (error "No previous search"))))

(defun senator-nonincremental-repeat-search-backward ()
  "Search backward for the previous search string or regexp."
  (interactive)
  (cond
   ((and (eq senator-last-search-type 'string)
         search-ring)
    (senator-search-backward (car search-ring)))
   ((and (eq senator-last-search-type 'regexp)
         regexp-search-ring)
    (senator-re-search-backward (car regexp-search-ring)))
   (t
    (error "No previous search"))))

(defun senator-nonincremental-search-forward (string)
  "Search for STRING nonincrementally."
  (interactive "sSemantic search for string: ")
  (setq senator-last-search-type 'string)
  (if (equal string "")
      (senator-search-forward (car search-ring))
    (isearch-update-ring string nil)
    (senator-search-forward string)))

(defun senator-nonincremental-search-backward (string)
  "Search backward for STRING nonincrementally."
  (interactive "sSemantic search for string: ")
  (setq senator-last-search-type 'string)
  (if (equal string "")
      (senator-search-backward (car search-ring))
    (isearch-update-ring string nil)
    (senator-search-backward string)))

(defun senator-nonincremental-re-search-forward (string)
  "Search for the regular expression STRING nonincrementally."
  (interactive "sSemantic search for regexp: ")
  (setq senator-last-search-type 'regexp)
  (if (equal string "")
      (senator-re-search-forward (car regexp-search-ring))
    (isearch-update-ring string t)
    (senator-re-search-forward string)))

(defun senator-nonincremental-re-search-backward (string)
  "Search backward for the regular expression STRING nonincrementally."
  (interactive "sSemantic search for regexp: ")
  (setq senator-last-search-type 'regexp)
  (if (equal string "")
      (senator-re-search-backward (car regexp-search-ring))
    (isearch-update-ring string t)
    (senator-re-search-backward string)))

(defvar senator--search-filter nil)

(defun senator-search-set-tag-class-filter (&optional classes)
  "In current buffer, limit search scope to tag CLASSES.
CLASSES is a list of tag class symbols or nil.  If nil only global
filters in `senator-search-tag-filter-functions' remain active."
  (interactive "sClasses: ")
  (setq classes
        (cond
         ((null classes)
          nil)
         ((symbolp classes)
          (list classes))
         ((stringp classes)
          (mapcar 'read (split-string classes)))
         (t
          (signal 'wrong-type-argument (list classes)))
         ))
  ;; Clear previous filter.
  (remove-hook 'senator-search-tag-filter-functions
               senator--search-filter t)
  (kill-local-variable 'senator--search-filter)
  (if classes
      (let ((tag   (make-symbol "tag"))
            (names (mapconcat 'symbol-name classes "', `")))
        (set (make-local-variable 'senator--search-filter)
             `(lambda (,tag)
                (memq (semantic-tag-class ,tag) ',classes)))
        (add-hook 'senator-search-tag-filter-functions
                  senator--search-filter nil t)
        (message "Limit search to `%s' tags" names))
    (message "Default search filter restored")))

;;; Folding
;;
;; Use new folding state.  It might be wise to extend the idea
;; of folding for hiding all but this, or show all children, etc.

(defun senator-fold-tag (&optional tag)
  "Fold the current TAG."
  (interactive)
  (semantic-set-tag-folded (or tag (semantic-current-tag)) t))

(defun senator-unfold-tag (&optional tag)
  "Fold the current TAG."
  (interactive)
  (semantic-set-tag-folded (or tag (semantic-current-tag)) nil))

(defun senator-fold-tag-toggle (&optional tag)
  "Fold the current TAG."
  (interactive)
  (let ((tag (or tag (semantic-current-tag))))
    (if (semantic-tag-folded-p tag)
        (senator-unfold-tag tag)
      (senator-fold-tag tag))))

;; @TODO - move this to some analyzer / refs tool
(define-overloadable-function semantic-up-reference (tag)
  "Return a tag that is referred to by TAG.
A \"reference\" could be any interesting feature of TAG.
In C++, a function may have a 'parent' which is non-local.
If that parent which is only a reference in the function tag
is found, we can jump to it.
Some tags such as includes have other reference features.")

;;;###autoload
(defun senator-go-to-up-reference (&optional tag)
  "Move up one reference from the current TAG.
A \"reference\" could be any interesting feature of TAG.
In C++, a function may have a 'parent' which is non-local.
If that parent which is only a reference in the function tag
is found, we can jump to it.
Some tags such as includes have other reference features."
  (interactive)
  (let ((result (semantic-up-reference (or tag (semantic-current-tag)))))
    (if (not result)
        (error "No up reference found")
      (push-mark)
      (cond
       ;; A tag
       ((semantic-tag-p result)
	(semantic-go-to-tag result)
	(switch-to-buffer (current-buffer))
	(semantic-momentary-highlight-tag result))
       ;; Buffers
       ((bufferp result)
	(switch-to-buffer result)
	(pulse-momentary-highlight-one-line (point)))
       ;; Files
       ((and (stringp result) (file-exists-p result))
	(find-file result)
	(pulse-momentary-highlight-one-line (point)))
       (t
	(error "Unknown result type from `semantic-up-reference'"))))))

(defun semantic-up-reference-default (tag)
  "Return a tag that is referred to by TAG.
Makes C/C++ language like assumptions."
  (cond ((semantic-tag-faux-p tag)
         ;; Faux tags should have a real tag in some other location.
	 (require 'semantic/sort)
         (let ((options (semantic-tag-external-class tag)))
           ;; I should do something a little better than
           ;; this.  Oy!
           (car options)
           ))

	;; Include always point to another file.
        ((eq (semantic-tag-class tag) 'include)
	 (let ((file (semantic-dependency-tag-file tag)))
	   (cond
	    ((or (not file) (not (file-exists-p file)))
	     (error "Could not location include %s"
		    (semantic-tag-name tag)))
	    ((get-file-buffer file)
	     (get-file-buffer file))
	    ((stringp file)
	     file)
	    )))

	;; Is there a parent of the function to jump to?
        ((and (semantic-tag-of-class-p tag 'function)
              (semantic-tag-function-parent tag))
         (let* ((scope (semantic-calculate-scope (point))))
	   ;; @todo - it would be cool to ask the user which one if
	   ;; more than one.
	   (car (oref scope parents))
	   ))

	;; Is there a non-prototype version of the tag to jump to?
        ((semantic-tag-get-attribute tag :prototype-flag)
	 (require 'semantic/analyze/refs)
	 (let* ((sar (semantic-analyze-tag-references tag)))
	   (car (semantic-analyze-refs-impl sar t)))
	 )

	;; If this is a datatype, and we have superclasses
	((and (semantic-tag-of-class-p tag 'type)
	      (semantic-tag-type-superclasses tag))
	 (require 'semantic/analyze)
	 (let ((scope (semantic-calculate-scope (point)))
	       (parents (semantic-tag-type-superclasses tag)))
	   (semantic-analyze-find-tag (car parents) 'type scope)))

	;; Get the data type, and try to find that.
        ((semantic-tag-type tag)
	 (require 'semantic/analyze)
	 (let ((scope (semantic-calculate-scope (point))))
	   (semantic-analyze-tag-type tag scope))
	 )
        (t nil)))

(defvar senator-isearch-semantic-mode nil
  "Non-nil if isearch does semantic search.
This is a buffer local variable.")
(make-variable-buffer-local 'senator-isearch-semantic-mode)

(defun senator-beginning-of-defun (&optional arg)
  "Move backward to the beginning of a defun.
Use semantic tags to navigate.
ARG is the number of tags to navigate (not yet implemented)."
  (semantic-fetch-tags)
  (let* ((senator-highlight-found nil)
         ;; Step at beginning of next tag with class specified in
         ;; `senator-step-at-tag-classes'.
         (senator-step-at-start-end-tag-classes t)
         (tag (senator-previous-tag)))
    (when tag
      (if (= (point) (semantic-tag-end tag))
          (goto-char (semantic-tag-start tag)))
      (beginning-of-line))))

(defun senator-end-of-defun (&optional arg)
  "Move forward to next end of defun.
Use semantic tags to navigate.
ARG is the number of tags to navigate (not yet implemented)."
  (semantic-fetch-tags)
  (let* ((senator-highlight-found nil)
         ;; Step at end of next tag with class specified in
         ;; `senator-step-at-tag-classes'.
         (senator-step-at-start-end-tag-classes t)
         (tag (senator-next-tag)))
    (when tag
      (if (= (point) (semantic-tag-start tag))
          (goto-char (semantic-tag-end tag)))
      (skip-chars-forward " \t")
      (if (looking-at "\\s<\\|\n")
          (forward-line 1)))))

(defun senator-narrow-to-defun ()
  "Make text outside current defun invisible.
The defun visible is the one that contains point or follows point.
Use semantic tags to navigate."
  (interactive)
  (semantic-fetch-tags)
  (save-excursion
    (widen)
    (senator-end-of-defun)
    (let ((end (point)))
      (senator-beginning-of-defun)
      (narrow-to-region (point) end))))

(defun senator-mark-defun ()
  "Put mark at end of this defun, point at beginning.
The defun marked is the one that contains point or follows point.
Use semantic tags to navigate."
  (interactive)
  (let ((origin (point))
        (end    (progn (senator-end-of-defun) (point)))
        (start  (progn (senator-beginning-of-defun) (point))))
    (goto-char origin)
    (push-mark (point))
    (goto-char end) ;; end-of-defun
    (push-mark (point) nil t)
    (goto-char start) ;; beginning-of-defun
    (re-search-backward "^\n" (- (point) 1) t)))

;;; Tag Cut & Paste

;; To copy a tag, means to put a tag definition into the tag
;; ring.  To kill a tag, put the tag into the tag ring AND put
;; the body of the tag into the kill-ring.
;;
;; To retrieve a killed tag's text, use C-y (yank), but to retrieve
;; the tag as a reference of some sort, use senator-yank-tag.

(defvar senator-tag-ring (make-ring 20)
  "Ring of tags for use with cut and paste.")

;;;###autoload
(defun senator-copy-tag ()
  "Take the current tag, and place it in the tag ring."
  (interactive)
  (semantic-fetch-tags)
  (let ((ft (semantic-obtain-foreign-tag)))
    (when ft
      (ring-insert senator-tag-ring ft)
      (kill-ring-save (semantic-tag-start ft) (semantic-tag-end ft))
      (when (called-interactively-p 'interactive)
        (message "Use C-y to yank text.  \
Use `senator-yank-tag' for prototype insert.")))
    ft))

;;;###autoload
(defun senator-kill-tag ()
  "Take the current tag, place it in the tag ring, and kill it.
Killing the tag removes the text for that tag, and places it into
the kill ring.  Retrieve that text with \\[yank]."
  (interactive)
  (let ((ct (senator-copy-tag))) ;; this handles the reparse for us.
    (kill-region (semantic-tag-start ct)
                 (semantic-tag-end ct))
    (when (called-interactively-p 'interactive)
      (message "Use C-y to yank text.  \
Use `senator-yank-tag' for prototype insert."))))

;;;###autoload
(defun senator-yank-tag ()
  "Yank a tag from the tag ring.
The form the tag takes is different depending on where it is being
yanked to."
  (interactive)
  (or (ring-empty-p senator-tag-ring)
      (let ((ft (ring-ref senator-tag-ring 0)))
          (semantic-foreign-tag-check ft)
          (semantic-insert-foreign-tag ft)
          (when (called-interactively-p 'interactive)
            (message "Use C-y to recover the yank the text of %s."
                     (semantic-tag-name ft))))))

;;;###autoload
(defun senator-copy-tag-to-register (register &optional kill-flag)
  "Copy the current tag into REGISTER.
Optional argument KILL-FLAG will delete the text of the tag to the
kill ring."
  (interactive "cTag to register: \nP")
  (semantic-fetch-tags)
  (let ((ft (semantic-obtain-foreign-tag)))
    (when ft
      (set-register register ft)
      (if kill-flag
          (kill-region (semantic-tag-start ft)
                       (semantic-tag-end ft))))))

;;;###autoload
(defun senator-transpose-tags-up ()
  "Transpose the current tag, and the preceding tag."
  (interactive)
  (semantic-fetch-tags)
  (let* ((current-tag (semantic-current-tag))
         (prev-tag (save-excursion
                     (goto-char (semantic-tag-start current-tag))
                     (semantic-find-tag-by-overlay-prev)))
         (ct-parent (semantic-find-tag-parent-by-overlay current-tag))
         (pt-parent (semantic-find-tag-parent-by-overlay prev-tag)))
    (if (not (eq ct-parent pt-parent))
        (error "Cannot transpose tags"))
    (let ((txt (buffer-substring (semantic-tag-start current-tag)
                                 (semantic-tag-end current-tag)))
          (line (count-lines (semantic-tag-start current-tag)
                             (point)))
          (insert-point nil)
          )
      (delete-region (semantic-tag-start current-tag)
                     (semantic-tag-end current-tag))
      (delete-blank-lines)
      (goto-char (semantic-tag-start prev-tag))
      (setq insert-point (point))
      (insert txt)
      (if (/= (current-column) 0)
          (insert "\n"))
      (insert "\n")
      (goto-char insert-point)
      (forward-line line)
      )))

;;;###autoload
(defun senator-transpose-tags-down ()
  "Transpose the current tag, and the following tag."
  (interactive)
  (semantic-fetch-tags)
  (let* ((current-tag (semantic-current-tag))
         (next-tag (save-excursion
                     (goto-char (semantic-tag-end current-tag))
                     (semantic-find-tag-by-overlay-next)))
         (end-pt (point-marker))
         )
    (goto-char (semantic-tag-start next-tag))
    (forward-char 1)
    (senator-transpose-tags-up)
    ;; I know that the above fcn deletes the next tag, so our pt marker
    ;; will be stable.
    (goto-char end-pt)))

;;; Using semantic search in isearch mode

(defun senator-lazy-highlight-update ()
  "Force lazy highlight update."
  (lazy-highlight-cleanup t)
  (set 'isearch-lazy-highlight-last-string nil)
  (setq isearch-adjusted t)
  (isearch-update))

;; Recent versions of GNU Emacs allow to override the isearch search
;; function for special needs, and avoid to advice the built-in search
;; function :-)
(defun senator-isearch-search-fun ()
  "Return the function to use for the search.
Use a senator search function when semantic isearch mode is enabled."
  (intern
   (concat (if senator-isearch-semantic-mode
               "senator-"
             "")
           (cond (isearch-word "word-")
                 (isearch-regexp "re-")
                 (t ""))
           "search-"
           (if isearch-forward
               "forward"
             "backward"))))

(defun senator-isearch-toggle-semantic-mode ()
  "Toggle semantic searching on or off in isearch mode."
  (interactive)
  (setq senator-isearch-semantic-mode
	(not senator-isearch-semantic-mode))
  (if isearch-mode
      ;; force lazy highlight update
      (senator-lazy-highlight-update)
    (message "Isearch semantic mode %s"
	     (if senator-isearch-semantic-mode
		 "enabled"
	       "disabled"))))

(defvar senator-old-isearch-search-fun nil
  "Hold previous value of `isearch-search-fun-function'.")

(defun senator-isearch-mode-hook ()
  "Isearch mode hook to setup semantic searching."
  (if (and isearch-mode senator-isearch-semantic-mode)
      (progn
	;; When `senator-isearch-semantic-mode' is on save the
	;; previous `isearch-search-fun-function' and install the
	;; senator one.
	(when (and (local-variable-p 'isearch-search-fun-function)
		   (not (local-variable-p 'senator-old-isearch-search-fun)))
	  (set (make-local-variable 'senator-old-isearch-search-fun)
	       isearch-search-fun-function))
	(set (make-local-variable 'isearch-search-fun-function)
	     'senator-isearch-search-fun))
    ;; When `senator-isearch-semantic-mode' is off restore the
    ;; previous `isearch-search-fun-function'.
    (when (eq isearch-search-fun-function 'senator-isearch-search-fun)
      (if (local-variable-p 'senator-old-isearch-search-fun)
	  (progn
	    (set (make-local-variable 'isearch-search-fun-function)
		 senator-old-isearch-search-fun)
	    (kill-local-variable 'senator-old-isearch-search-fun))
	(kill-local-variable 'isearch-search-fun-function)))))

;; (add-hook 'isearch-mode-hook     'senator-isearch-mode-hook)
;; (add-hook 'isearch-mode-end-hook 'senator-isearch-mode-hook)

;; ;; Keyboard shortcut to toggle semantic search in isearch mode.
;; (define-key isearch-mode-map
;;   [(control ?,)]
;;   'senator-isearch-toggle-semantic-mode)

(provide 'semantic/senator)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "semantic/senator"
;; End:

;;; semantic/senator.el ends here
