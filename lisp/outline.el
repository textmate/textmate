;;; outline.el --- outline mode commands for Emacs

;; Copyright (C) 1986, 1993-1995, 1997, 2000-2012
;;   Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: outlines

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

;; This package is a major mode for editing outline-format documents.
;; An outline can be `abstracted' to show headers at any given level,
;; with all stuff below hidden.  See the Emacs manual for details.

;;; Todo:

;; - subtree-terminators
;; - better handle comments before function bodies (i.e. heading)
;; - don't bother hiding whitespace

;;; Code:

(defvar font-lock-warning-face)


(defgroup outlines nil
  "Support for hierarchical outlining."
  :prefix "outline-"
  :group 'wp)

(defcustom outline-regexp "[*\^L]+"
  "Regular expression to match the beginning of a heading.
Any line whose beginning matches this regexp is considered to start a heading.
Note that Outline mode only checks this regexp at the start of a line,
so the regexp need not (and usually does not) start with `^'.
The recommended way to set this is with a Local Variables: list
in the file it applies to.  See also `outline-heading-end-regexp'."
  :type 'regexp
  :group 'outlines)
;;;###autoload(put 'outline-regexp 'safe-local-variable 'stringp)

(defcustom outline-heading-end-regexp "\n"
  "Regular expression to match the end of a heading line.
You can assume that point is at the beginning of a heading when this
regexp is searched for.  The heading ends at the end of the match.
The recommended way to set this is with a `Local Variables:' list
in the file it applies to."
  :type 'regexp
  :group 'outlines)
;;;###autoload(put 'outline-heading-end-regexp 'safe-local-variable 'stringp)

(defvar outline-mode-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "@" 'outline-mark-subtree)
    (define-key map "\C-n" 'outline-next-visible-heading)
    (define-key map "\C-p" 'outline-previous-visible-heading)
    (define-key map "\C-i" 'show-children)
    (define-key map "\C-s" 'show-subtree)
    (define-key map "\C-d" 'hide-subtree)
    (define-key map "\C-u" 'outline-up-heading)
    (define-key map "\C-f" 'outline-forward-same-level)
    (define-key map "\C-b" 'outline-backward-same-level)
    (define-key map "\C-t" 'hide-body)
    (define-key map "\C-a" 'show-all)
    (define-key map "\C-c" 'hide-entry)
    (define-key map "\C-e" 'show-entry)
    (define-key map "\C-l" 'hide-leaves)
    (define-key map "\C-k" 'show-branches)
    (define-key map "\C-q" 'hide-sublevels)
    (define-key map "\C-o" 'hide-other)
    (define-key map "\C-^" 'outline-move-subtree-up)
    (define-key map "\C-v" 'outline-move-subtree-down)
    (define-key map [(control ?<)] 'outline-promote)
    (define-key map [(control ?>)] 'outline-demote)
    (define-key map "\C-m" 'outline-insert-heading)
    ;; Where to bind outline-cycle ?
    map))

(defvar outline-mode-menu-bar-map
  (let ((map (make-sparse-keymap)))

    (define-key map [hide] (cons "Hide" (make-sparse-keymap "Hide")))

    (define-key map [hide hide-other]
      '(menu-item "Hide Other" hide-other
		  :help "Hide everything except current body and parent and top-level headings"))
    (define-key map [hide hide-sublevels]
      '(menu-item "Hide Sublevels" hide-sublevels
		  :help "Hide everything but the top LEVELS levels of headers, in whole buffer"))
    (define-key map [hide hide-subtree]
      '(menu-item "Hide Subtree" hide-subtree
		  :help "Hide everything after this heading at deeper levels"))
    (define-key map [hide hide-entry]
      '(menu-item "Hide Entry" hide-entry
		  :help "Hide the body directly following this heading"))
    (define-key map [hide hide-body]
      '(menu-item "Hide Body" hide-body
		  :help "Hide all body lines in buffer, leaving all headings visible"))
    (define-key map [hide hide-leaves]
      '(menu-item "Hide Leaves" hide-leaves
		  :help "Hide the body after this heading and at deeper levels"))

    (define-key map [show] (cons "Show" (make-sparse-keymap "Show")))

    (define-key map [show show-subtree]
      '(menu-item "Show Subtree" show-subtree
		  :help "Show everything after this heading at deeper levels"))
    (define-key map [show show-children]
      '(menu-item "Show Children" show-children
		  :help "Show all direct subheadings of this heading"))
    (define-key map [show show-branches]
      '(menu-item "Show Branches" show-branches
		  :help "Show all subheadings of this heading, but not their bodies"))
    (define-key map [show show-entry]
      '(menu-item "Show Entry" show-entry
		  :help "Show the body directly following this heading"))
    (define-key map [show show-all]
      '(menu-item "Show All" show-all
		  :help "Show all of the text in the buffer"))

    (define-key map [headings]
      (cons "Headings" (make-sparse-keymap "Headings")))

    (define-key map [headings demote-subtree]
      '(menu-item "Demote Subtree" outline-demote
		  :help "Demote headings lower down the tree"))
    (define-key map [headings promote-subtree]
      '(menu-item "Promote Subtree" outline-promote
		  :help "Promote headings higher up the tree"))
    (define-key map [headings move-subtree-down]
      '(menu-item "Move Subtree Down" outline-move-subtree-down
		  :help "Move the current subtree down past arg headlines of the same level"))
    (define-key map [headings move-subtree-up]
      '(menu-item "Move Subtree Up" outline-move-subtree-up
		  :help "Move the current subtree up past arg headlines of the same level"))
    (define-key map [headings copy]
      '(menu-item "Copy to Kill Ring" outline-headers-as-kill
		  :enable mark-active
		  :help "Save the visible outline headers in region at the start of the kill ring"))
    (define-key map [headings outline-insert-heading]
      '(menu-item "New Heading" outline-insert-heading
		  :help "Insert a new heading at same depth at point"))
    (define-key map [headings outline-backward-same-level]

      '(menu-item "Previous Same Level" outline-backward-same-level
		  :help "Move backward to the arg'th subheading at same level as this one."))
    (define-key map [headings outline-forward-same-level]

      '(menu-item "Next Same Level" outline-forward-same-level
		  :help "Move forward to the arg'th subheading at same level as this one"))
    (define-key map [headings outline-previous-visible-heading]

      '(menu-item "Previous" outline-previous-visible-heading
		  :help "Move to the previous heading line"))
    (define-key map [headings outline-next-visible-heading]

      '(menu-item "Next" outline-next-visible-heading
		  :help "Move to the next visible heading line"))
    (define-key map [headings outline-up-heading]

      '(menu-item "Up" outline-up-heading
		  :help "Move to the visible heading line of which the present line is a subheading"))
    map))

(defvar outline-minor-mode-menu-bar-map
  (let ((map (make-sparse-keymap)))
    (define-key map [outline]
      (cons "Outline"
	    (nconc (make-sparse-keymap "Outline")
		   ;; Remove extra separator
		   (cdr
		    ;; Flatten the major mode's menus into a single menu.
		    (apply 'append
			   (mapcar (lambda (x)
				     (if (consp x)
					 ;; Add a separator between each
					 ;; part of the unified menu.
					 (cons '(--- "---") (cdr x))))
				   outline-mode-menu-bar-map))))))
    map))


(defvar outline-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c" outline-mode-prefix-map)
    (define-key map [menu-bar] outline-mode-menu-bar-map)
    map))

(defvar outline-font-lock-keywords
  '(;;
    ;; Highlight headings according to the level.
    (eval . (list (concat "^\\(?:" outline-regexp "\\).+")
		  0 '(outline-font-lock-face) nil t)))
  "Additional expressions to highlight in Outline mode.")

(defface outline-1
  '((t :inherit font-lock-function-name-face))
  "Level 1."
  :group 'outlines)

(defface outline-2
  '((t :inherit font-lock-variable-name-face))
  "Level 2."
  :group 'outlines)

(defface outline-3
  '((t :inherit font-lock-keyword-face))
  "Level 3."
  :group 'outlines)

(defface outline-4
  '((t :inherit font-lock-comment-face))
  "Level 4."
  :group 'outlines)

(defface outline-5
  '((t :inherit font-lock-type-face))
  "Level 5."
  :group 'outlines)

(defface outline-6
  '((t :inherit font-lock-constant-face))
  "Level 6."
  :group 'outlines)

(defface outline-7
  '((t :inherit font-lock-builtin-face))
  "Level 7."
  :group 'outlines)

(defface outline-8
  '((t :inherit font-lock-string-face))
  "Level 8."
  :group 'outlines)

(defvar outline-font-lock-faces
  [outline-1 outline-2 outline-3 outline-4
   outline-5 outline-6 outline-7 outline-8])

;; (defvar outline-font-lock-levels nil)
;; (make-variable-buffer-local 'outline-font-lock-levels)

(defun outline-font-lock-face ()
  ;; (save-excursion
  ;;   (outline-back-to-heading t)
  ;;   (let* ((count 0)
  ;; 	   (start-level (funcall outline-level))
  ;; 	   (level start-level)
  ;; 	   face-level)
  ;;     (while (not (setq face-level
  ;; 			(if (or (bobp) (eq level 1)) 0
  ;; 			  (cdr (assq level outline-font-lock-levels)))))
  ;; 	(outline-up-heading 1 t)
  ;; 	(setq count (1+ count))
  ;; 	(setq level (funcall outline-level)))
  ;;     ;; Remember for later.
  ;;     (unless (zerop count)
  ;; 	(setq face-level (+ face-level count))
  ;; 	(push (cons start-level face-level) outline-font-lock-levels))
  ;;     (condition-case nil
  ;; 	  (aref outline-font-lock-faces face-level)
  ;; 	(error font-lock-warning-face))))
  (save-excursion
    (goto-char (match-beginning 0))
    (looking-at outline-regexp)
    (aref outline-font-lock-faces (% (1- (funcall outline-level)) (length outline-font-lock-faces)))))

(defvar outline-view-change-hook nil
  "Normal hook to be run after outline visibility changes.")

(defvar outline-mode-hook nil
  "*This hook is run when outline mode starts.")

(defvar outline-blank-line nil
  "*Non-nil means to leave unhidden blank line before heading.")

;;;###autoload
(define-derived-mode outline-mode text-mode "Outline"
  "Set major mode for editing outlines with selective display.
Headings are lines which start with asterisks: one for major headings,
two for subheadings, etc.  Lines not starting with asterisks are body lines.

Body text or subheadings under a heading can be made temporarily
invisible, or visible again.  Invisible lines are attached to the end
of the heading, so they move with it, if the line is killed and yanked
back.  A heading with text hidden under it is marked with an ellipsis (...).

Commands:\\<outline-mode-map>
\\[outline-next-visible-heading]   outline-next-visible-heading      move by visible headings
\\[outline-previous-visible-heading]   outline-previous-visible-heading
\\[outline-forward-same-level]   outline-forward-same-level        similar but skip subheadings
\\[outline-backward-same-level]   outline-backward-same-level
\\[outline-up-heading]   outline-up-heading		    move from subheading to heading

\\[hide-body]	make all text invisible (not headings).
\\[show-all]	make everything in buffer visible.
\\[hide-sublevels]  make only the first N levels of headers visible.

The remaining commands are used when point is on a heading line.
They apply to some of the body or subheadings of that heading.
\\[hide-subtree]   hide-subtree	make body and subheadings invisible.
\\[show-subtree]   show-subtree	make body and subheadings visible.
\\[show-children]   show-children	make direct subheadings visible.
		 No effect on body, or subheadings 2 or more levels down.
		 With arg N, affects subheadings N levels down.
\\[hide-entry]	   make immediately following body invisible.
\\[show-entry]	   make it visible.
\\[hide-leaves]	   make body under heading and under its subheadings invisible.
		     The subheadings remain visible.
\\[show-branches]  make all subheadings at all levels visible.

The variable `outline-regexp' can be changed to control what is a heading.
A line is a heading if `outline-regexp' matches something at the
beginning of the line.  The longer the match, the deeper the level.

Turning on outline mode calls the value of `text-mode-hook' and then of
`outline-mode-hook', if they are non-nil."
  (make-local-variable 'line-move-ignore-invisible)
  (setq line-move-ignore-invisible t)
  ;; Cause use of ellipses for invisible text.
  (add-to-invisibility-spec '(outline . t))
  (set (make-local-variable 'paragraph-start)
       (concat paragraph-start "\\|\\(?:" outline-regexp "\\)"))
  ;; Inhibit auto-filling of header lines.
  (set (make-local-variable 'auto-fill-inhibit-regexp) outline-regexp)
  (set (make-local-variable 'paragraph-separate)
       (concat paragraph-separate "\\|\\(?:" outline-regexp "\\)"))
  (set (make-local-variable 'font-lock-defaults)
       '(outline-font-lock-keywords t nil nil backward-paragraph))
  (setq imenu-generic-expression
	(list (list nil (concat "^\\(?:" outline-regexp "\\).*$") 0)))
  (add-hook 'change-major-mode-hook 'show-all nil t))

(defcustom outline-minor-mode-prefix "\C-c@"
  "Prefix key to use for Outline commands in Outline minor mode.
The value of this variable is checked as part of loading Outline mode.
After that, changing the prefix key requires manipulating keymaps."
  :type 'string
  :group 'outlines)

;;;###autoload
(define-minor-mode outline-minor-mode
  "Toggle Outline minor mode.
With a prefix argument ARG, enable Outline minor mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

See the command `outline-mode' for more information on this mode."
  nil " Outl" (list (cons [menu-bar] outline-minor-mode-menu-bar-map)
		    (cons outline-minor-mode-prefix outline-mode-prefix-map))
  :group 'outlines
  (if outline-minor-mode
      (progn
	;; Turn off this mode if we change major modes.
	(add-hook 'change-major-mode-hook
		  (lambda () (outline-minor-mode -1))
		  nil t)
	(set (make-local-variable 'line-move-ignore-invisible) t)
	;; Cause use of ellipses for invisible text.
	(add-to-invisibility-spec '(outline . t)))
    (setq line-move-ignore-invisible nil)
    ;; Cause use of ellipses for invisible text.
    (remove-from-invisibility-spec '(outline . t))
    ;; When turning off outline mode, get rid of any outline hiding.
    (show-all)))

(defvar outline-level 'outline-level
  "*Function of no args to compute a header's nesting level in an outline.
It can assume point is at the beginning of a header line and that the match
data reflects the `outline-regexp'.")
;;;###autoload(put 'outline-level 'risky-local-variable t)

(defvar outline-heading-alist ()
  "Alist associating a heading for every possible level.
Each entry is of the form (HEADING . LEVEL).
This alist is used two ways: to find the heading corresponding to
a given level and to find the level of a given heading.
If a mode or document needs several sets of outline headings (for example
numbered and unnumbered sections), list them set by set and sorted by level
within each set.  For example in texinfo mode:

     (setq outline-heading-alist
      '((\"@chapter\" . 2) (\"@section\" . 3) (\"@subsection\" . 4)
           (\"@subsubsection\" . 5)
        (\"@unnumbered\" . 2) (\"@unnumberedsec\" . 3)
           (\"@unnumberedsubsec\" . 4)  (\"@unnumberedsubsubsec\" . 5)
        (\"@appendix\" . 2) (\"@appendixsec\" . 3)...
           (\"@appendixsubsec\" . 4) (\"@appendixsubsubsec\" . 5) ..))

Instead of sorting the entries in each set, you can also separate the
sets with nil.")
(make-variable-buffer-local 'outline-heading-alist)

;; This used to count columns rather than characters, but that made ^L
;; appear to be at level 2 instead of 1.  Columns would be better for
;; tab handling, but the default regexp doesn't use tabs, and anyone
;; who changes the regexp can also redefine the outline-level variable
;; as appropriate.
(defun outline-level ()
  "Return the depth to which a statement is nested in the outline.
Point must be at the beginning of a header line.
This is actually either the level specified in `outline-heading-alist'
or else the number of characters matched by `outline-regexp'."
  (or (cdr (assoc (match-string 0) outline-heading-alist))
      (- (match-end 0) (match-beginning 0))))

(defun outline-next-preface ()
  "Skip forward to just before the next heading line.
If there's no following heading line, stop before the newline
at the end of the buffer."
  (if (re-search-forward (concat "\n\\(?:" outline-regexp "\\)")
			 nil 'move)
      (goto-char (match-beginning 0)))
  (if (and (bolp) (or outline-blank-line (eobp)) (not (bobp)))
      (forward-char -1)))

(defun outline-next-heading ()
  "Move to the next (possibly invisible) heading line."
  (interactive)
  ;; Make sure we don't match the heading we're at.
  (if (and (bolp) (not (eobp))) (forward-char 1))
  (if (re-search-forward (concat "^\\(?:" outline-regexp "\\)")
			 nil 'move)
      (goto-char (match-beginning 0))))

(defun outline-previous-heading ()
  "Move to the previous (possibly invisible) heading line."
  (interactive)
  (re-search-backward (concat "^\\(?:" outline-regexp "\\)")
		      nil 'move))

(defsubst outline-invisible-p (&optional pos)
  "Non-nil if the character after point is invisible."
  (get-char-property (or pos (point)) 'invisible))

(defun outline-back-to-heading (&optional invisible-ok)
  "Move to previous heading line, or beg of this line if it's a heading.
Only visible heading lines are considered, unless INVISIBLE-OK is non-nil."
  (beginning-of-line)
  (or (outline-on-heading-p invisible-ok)
      (let (found)
	(save-excursion
	  (while (not found)
	    (or (re-search-backward (concat "^\\(?:" outline-regexp "\\)")
				    nil t)
		(error "before first heading"))
	    (setq found (and (or invisible-ok (not (outline-invisible-p)))
			     (point)))))
	(goto-char found)
	found)))

(defun outline-on-heading-p (&optional invisible-ok)
  "Return t if point is on a (visible) heading line.
If INVISIBLE-OK is non-nil, an invisible heading line is ok too."
  (save-excursion
    (beginning-of-line)
    (and (bolp) (or invisible-ok (not (outline-invisible-p)))
	 (looking-at outline-regexp))))

(defun outline-insert-heading ()
  "Insert a new heading at same depth at point."
  (interactive)
  (let ((head (save-excursion
		(condition-case nil
		    (outline-back-to-heading)
		  (error (outline-next-heading)))
		(if (eobp)
		    (or (caar outline-heading-alist) "")
		  (match-string 0)))))
    (unless (or (string-match "[ \t]\\'" head)
		(not (string-match (concat "\\`\\(?:" outline-regexp "\\)")
				   (concat head " "))))
      (setq head (concat head " ")))
    (unless (bolp) (end-of-line) (newline))
    (insert head)
    (unless (eolp)
      (save-excursion (newline-and-indent)))
    (run-hooks 'outline-insert-heading-hook)))

(defun outline-invent-heading (head up)
  (save-match-data
    ;; Let's try to invent one by repeating or deleting the last char.
    (let ((new-head (if up (substring head 0 -1)
                      (concat head (substring head -1)))))
      (if (string-match (concat "\\`\\(?:" outline-regexp "\\)")
                        new-head)
          ;; Why bother checking that it is indeed higher/lower level ?
          new-head
        ;; Didn't work, so ask what to do.
        (read-string (format "%s heading for `%s': "
                             (if up "Parent" "Demoted") head)
                     head nil nil t)))))

(defun outline-promote (&optional which)
  "Promote headings higher up the tree.
If transient-mark-mode is on, and mark is active, promote headings in
the region (from a Lisp program, pass `region' for WHICH).  Otherwise:
without prefix argument, promote current heading and all headings in the
subtree (from a Lisp program, pass `subtree' for WHICH); with prefix
argument, promote just the current heading (from a Lisp program, pass
nil for WHICH, or do not pass any argument)."
  (interactive
   (list (if (and transient-mark-mode mark-active) 'region
	   (outline-back-to-heading)
	   (if current-prefix-arg nil 'subtree))))
  (cond
   ((eq which 'region)
    (outline-map-region 'outline-promote (region-beginning) (region-end)))
   (which
    (outline-map-region 'outline-promote
			(point)
			(save-excursion (outline-get-next-sibling) (point))))
   (t
    (outline-back-to-heading t)
    (let* ((head (match-string-no-properties 0))
	   (level (save-match-data (funcall outline-level)))
	   (up-head (or (outline-head-from-level (1- level) head)
			;; Use the parent heading, if it is really
			;; one level less.
			(save-excursion
			  (save-match-data
			    (outline-up-heading 1 t)
			    (and (= (1- level) (funcall outline-level))
				 (match-string-no-properties 0))))
                        ;; Bummer!! There is no lower level heading.
                        (outline-invent-heading head 'up))))

      (unless (rassoc level outline-heading-alist)
	(push (cons head level) outline-heading-alist))

      (replace-match up-head nil t)))))

(defun outline-demote (&optional which)
  "Demote headings lower down the tree.
If transient-mark-mode is on, and mark is active, demote headings in
the region (from a Lisp program, pass `region' for WHICH).  Otherwise:
without prefix argument, demote current heading and all headings in the
subtree (from a Lisp program, pass `subtree' for WHICH); with prefix
argument, demote just the current heading (from a Lisp program, pass
nil for WHICH, or do not pass any argument)."
  (interactive
   (list (if (and transient-mark-mode mark-active) 'region
	   (outline-back-to-heading)
	   (if current-prefix-arg nil 'subtree))))
  (cond
   ((eq which 'region)
    (outline-map-region 'outline-demote (region-beginning) (region-end)))
   (which
    (outline-map-region 'outline-demote
			(point)
			(save-excursion (outline-get-next-sibling) (point))))
   (t
    (let* ((head (match-string-no-properties 0))
	   (level (save-match-data (funcall outline-level)))
	   (down-head
	    (or (outline-head-from-level (1+ level) head)
		(save-excursion
		  (save-match-data
		    (while (and (progn (outline-next-heading) (not (eobp)))
				(<= (funcall outline-level) level)))
		    (when (eobp)
		      ;; Try again from the beginning of the buffer.
		      (goto-char (point-min))
		      (while (and (progn (outline-next-heading) (not (eobp)))
				  (<= (funcall outline-level) level))))
		    (unless (eobp)
		      (looking-at outline-regexp)
		      (match-string-no-properties 0))))
                ;; Bummer!! There is no higher-level heading in the buffer.
                (outline-invent-heading head nil))))

      (unless (rassoc level outline-heading-alist)
	(push (cons head level) outline-heading-alist))
      (replace-match down-head nil t)))))

(defun outline-head-from-level (level head &optional alist)
  "Get new heading with level LEVEL from ALIST.
If there are no such entries, return nil.
ALIST defaults to `outline-heading-alist'.
Similar to (car (rassoc LEVEL ALIST)).
If there are several different entries with same new level, choose
the one with the smallest distance to the association of HEAD in the alist.
This makes it possible for promotion to work in modes with several
independent sets of headings (numbered, unnumbered, appendix...)"
  (unless alist (setq alist outline-heading-alist))
  (let ((l (rassoc level alist))
	ll h hl l2 l2l)
    (cond
     ((null l) nil)
     ;; If there's no HEAD after L, any other entry for LEVEL after L
     ;; can't be much better than L.
     ((null (setq h (assoc head (setq ll (memq l alist))))) (car l))
     ;; If there's no other entry for LEVEL, just keep L.
     ((null (setq l2 (rassoc level (cdr ll)))) (car l))
     ;; Now we have L, L2, and H: see if L2 seems better than L.
     ;; If H is after L2, L2 is better.
     ((memq h (setq l2l (memq l2 (cdr ll))))
      (outline-head-from-level level head l2l))
     ;; Now we have H between L and L2.
     ;; If there's a separator between L and H, prefer L2.
     ((memq h (memq nil ll))
      (outline-head-from-level level head l2l))
     ;; If there's a separator between L2 and H, prefer L.
     ((memq l2 (memq nil (setq hl (memq h ll)))) (car l))
     ;; No separator between L and L2, check the distance.
     ((< (* 2 (length hl)) (+ (length ll) (length l2l)))
      (outline-head-from-level level head l2l))
     ;; If all else fails, just keep L.
     (t (car l)))))

(defun outline-map-region (fun beg end)
  "Call FUN for every heading between BEG and END.
When FUN is called, point is at the beginning of the heading and
the match data is set appropriately."
  (save-excursion
    (setq end (copy-marker end))
    (goto-char beg)
    (when (re-search-forward (concat "^\\(?:" outline-regexp "\\)") end t)
      (goto-char (match-beginning 0))
      (funcall fun)
      (while (and (progn
		    (outline-next-heading)
		    (< (point) end))
		  (not (eobp)))
	(funcall fun)))))

;; Vertical tree motion

(defun outline-move-subtree-up (&optional arg)
  "Move the current subtree up past ARG headlines of the same level."
  (interactive "p")
  (outline-move-subtree-down (- arg)))

(defun outline-move-subtree-down (&optional arg)
  "Move the current subtree down past ARG headlines of the same level."
  (interactive "p")
  (let ((movfunc (if (> arg 0) 'outline-get-next-sibling
		   'outline-get-last-sibling))
	(ins-point (make-marker))
	(cnt (abs arg))
	beg end folded)
    ;; Select the tree
    (outline-back-to-heading)
    (setq beg (point))
    (save-match-data
      (save-excursion (outline-end-of-heading)
		      (setq folded (outline-invisible-p)))
      (outline-end-of-subtree))
    (if (= (char-after) ?\n) (forward-char 1))
    (setq end (point))
    ;; Find insertion point, with error handling
    (goto-char beg)
    (while (> cnt 0)
      (or (funcall movfunc)
	  (progn (goto-char beg)
		 (error "Cannot move past superior level")))
      (setq cnt (1- cnt)))
    (if (> arg 0)
	;; Moving forward - still need to move over subtree
	(progn (outline-end-of-subtree)
	       (if (= (char-after) ?\n) (forward-char 1))))
    (move-marker ins-point (point))
    (insert (delete-and-extract-region beg end))
    (goto-char ins-point)
    (if folded (hide-subtree))
    (move-marker ins-point nil)))

(defun outline-end-of-heading ()
  (if (re-search-forward outline-heading-end-regexp nil 'move)
      (forward-char -1)))

(defun outline-next-visible-heading (arg)
  "Move to the next visible heading line.
With argument, repeats or can move backward if negative.
A heading line is one that starts with a `*' (or that
`outline-regexp' matches)."
  (interactive "p")
  (if (< arg 0)
      (beginning-of-line)
    (end-of-line))
  (let (found-heading-p)
    (while (and (not (bobp)) (< arg 0))
      (while (and (not (bobp))
		  (setq found-heading-p
			(re-search-backward
			 (concat "^\\(?:" outline-regexp "\\)")
			 nil 'move))
		  (outline-invisible-p)))
      (setq arg (1+ arg)))
    (while (and (not (eobp)) (> arg 0))
      (while (and (not (eobp))
		  (setq found-heading-p
			(re-search-forward
			 (concat "^\\(?:" outline-regexp "\\)")
			 nil 'move))
		  (outline-invisible-p (match-beginning 0))))
      (setq arg (1- arg)))
    (if found-heading-p (beginning-of-line))))

(defun outline-previous-visible-heading (arg)
  "Move to the previous heading line.
With argument, repeats or can move forward if negative.
A heading line is one that starts with a `*' (or that
`outline-regexp' matches)."
  (interactive "p")
  (outline-next-visible-heading (- arg)))

(defun outline-mark-subtree ()
  "Mark the current subtree in an outlined document.
This puts point at the start of the current subtree, and mark at the end."
  (interactive)
  (let ((beg))
    (if (outline-on-heading-p)
	;; we are already looking at a heading
	(beginning-of-line)
      ;; else go back to previous heading
      (outline-previous-visible-heading 1))
    (setq beg (point))
    (outline-end-of-subtree)
    (push-mark (point) nil t)
    (goto-char beg)))


(defvar outline-isearch-open-invisible-function nil
  "Function called if `isearch' finishes in an invisible overlay.
The function is called with the overlay as its only argument.
If nil, `show-entry' is called to reveal the invisible text.")

(put 'outline 'reveal-toggle-invisible 'outline-reveal-toggle-invisible)
(defun outline-flag-region (from to flag)
  "Hide or show lines from FROM to TO, according to FLAG.
If FLAG is nil then text is shown, while if FLAG is t the text is hidden."
  (remove-overlays from to 'invisible 'outline)
  (when flag
    ;; We use `front-advance' here because the invisible text begins at the
    ;; very end of the heading, before the newline, so text inserted at FROM
    ;; belongs to the heading rather than to the entry.
    (let ((o (make-overlay from to nil 'front-advance)))
      (overlay-put o 'evaporate t)
      (overlay-put o 'invisible 'outline)
      (overlay-put o 'isearch-open-invisible
		   (or outline-isearch-open-invisible-function
		       'outline-isearch-open-invisible))))
  ;; Seems only used by lazy-lock.  I.e. obsolete.
  (run-hooks 'outline-view-change-hook))

(defun outline-reveal-toggle-invisible (o hidep)
  (save-excursion
    (goto-char (overlay-start o))
    (if hidep
	;; When hiding the area again, we could just clean it up and let
	;; reveal do the rest, by simply doing:
	;; (remove-overlays (overlay-start o) (overlay-end o)
	;;                  'invisible 'outline)
	;;
	;; That works fine as long as everything is in sync, but if the
	;; structure of the document is changed while revealing parts of it,
	;; the resulting behavior can be ugly.  I.e. we need to make
	;; sure that we hide exactly a subtree.
	(progn
	  (let ((end (overlay-end o)))
	    (delete-overlay o)
	    (while (progn
		     (hide-subtree)
		     (outline-next-visible-heading 1)
		     (and (not (eobp)) (< (point) end))))))

      ;; When revealing, we just need to reveal sublevels.  If point is
      ;; inside one of the sublevels, reveal will call us again.
      ;; But we need to preserve the original overlay.
      (let ((o1 (copy-overlay o)))
	(overlay-put o 'invisible nil)	;Show (most of) the text.
	(while (progn
		 (show-entry)
		 (show-children)
		 ;; Normally just the above is needed.
		 ;; But in odd cases, the above might fail to show anything.
		 ;; To avoid an infinite loop, we have to make sure that
		 ;; *something* gets shown.
		 (and (equal (overlay-start o) (overlay-start o1))
		      (< (point) (overlay-end o))
		      (= 0 (forward-line 1)))))
	;; If still nothing was shown, just kill the damn thing.
	(when (equal (overlay-start o) (overlay-start o1))
	  ;; I've seen it happen at the end of buffer.
	  (delete-overlay o1))))))

;; Function to be set as an outline-isearch-open-invisible' property
;; to the overlay that makes the outline invisible (see
;; `outline-flag-region').
(defun outline-isearch-open-invisible (_overlay)
  ;; We rely on the fact that isearch places point on the matched text.
  (show-entry))

(defun hide-entry ()
  "Hide the body directly following this heading."
  (interactive)
  (save-excursion
    (outline-back-to-heading)
    (outline-end-of-heading)
    (outline-flag-region (point) (progn (outline-next-preface) (point)) t)))

(defun show-entry ()
  "Show the body directly following this heading.
Show the heading too, if it is currently invisible."
  (interactive)
  (save-excursion
    (outline-back-to-heading t)
    (outline-flag-region (1- (point))
			 (progn (outline-next-preface) (point)) nil)))

(defun hide-body ()
  "Hide all body lines in buffer, leaving all headings visible."
  (interactive)
  (hide-region-body (point-min) (point-max)))

(defun hide-region-body (start end)
  "Hide all body lines in the region, but not headings."
  ;; Nullify the hook to avoid repeated calls to `outline-flag-region'
  ;; wasting lots of time running `lazy-lock-fontify-after-outline'
  ;; and run the hook finally.
  (let (outline-view-change-hook)
    (save-excursion
      (save-restriction
	(narrow-to-region start end)
	(goto-char (point-min))
	(if (outline-on-heading-p)
	    (outline-end-of-heading)
	  (outline-next-preface))
	(while (not (eobp))
	  (outline-flag-region (point)
			       (progn (outline-next-preface) (point)) t)
	  (unless (eobp)
	    (forward-char (if (looking-at "\n\n") 2 1))
	    (outline-end-of-heading))))))
  (run-hooks 'outline-view-change-hook))

(defun show-all ()
  "Show all of the text in the buffer."
  (interactive)
  (outline-flag-region (point-min) (point-max) nil))

(defun hide-subtree ()
  "Hide everything after this heading at deeper levels."
  (interactive)
  (outline-flag-subtree t))

(defun hide-leaves ()
  "Hide the body after this heading and at deeper levels."
  (interactive)
  (save-excursion
    (outline-back-to-heading)
;; Turned off to fix bug reported by Otto Maddox on 22 Nov 2005.
;;    (outline-end-of-heading)
    (hide-region-body (point) (progn (outline-end-of-subtree) (point)))))

(defun show-subtree ()
  "Show everything after this heading at deeper levels."
  (interactive)
  (outline-flag-subtree nil))

(defun outline-show-heading ()
  "Show the current heading and move to its end."
  (outline-flag-region (- (point)
 			  (if (bobp) 0
 			    (if (and outline-blank-line
                                     (eq (char-before (1- (point))) ?\n))
 				2 1)))
		       (progn (outline-end-of-heading) (point))
		       nil))

(defun hide-sublevels (levels)
  "Hide everything but the top LEVELS levels of headers, in whole buffer."
  (interactive (list
		(cond
		 (current-prefix-arg (prefix-numeric-value current-prefix-arg))
		 ((save-excursion (beginning-of-line)
				  (looking-at outline-regexp))
		  (funcall outline-level))
		 (t 1))))
  (if (< levels 1)
      (error "Must keep at least one level of headers"))
  (save-excursion
    (let* (outline-view-change-hook
           (beg (progn
                  (goto-char (point-min))
                  ;; Skip the prelude, if any.
                  (unless (outline-on-heading-p t) (outline-next-heading))
                  (point)))
           (end (progn
                  (goto-char (point-max))
                  ;; Keep empty last line, if available.
                  (if (bolp) (1- (point)) (point)))))
      (if (< end beg)
	  (setq beg (prog1 end (setq end beg))))
      ;; First hide everything.
      (outline-flag-region beg end t)
      ;; Then unhide the top level headers.
      (outline-map-region
       (lambda ()
	 (if (<= (funcall outline-level) levels)
	     (outline-show-heading)))
       beg end)
      ;; Finally unhide any trailing newline.
      (goto-char (point-max))
      (if (and (bolp) (not (bobp)) (outline-invisible-p (1- (point))))
          (outline-flag-region (1- (point)) (point) nil))))
  (run-hooks 'outline-view-change-hook))

(defun hide-other ()
  "Hide everything except current body and parent and top-level headings."
  (interactive)
  (hide-sublevels 1)
  (let (outline-view-change-hook)
    (save-excursion
      (outline-back-to-heading t)
      (show-entry)
      (while (condition-case nil (progn (outline-up-heading 1 t) (not (bobp)))
	       (error nil))
	(outline-flag-region (1- (point))
			     (save-excursion (forward-line 1) (point))
			     nil))))
  (run-hooks 'outline-view-change-hook))

(defun outline-toggle-children ()
  "Show or hide the current subtree depending on its current state."
  (interactive)
  (save-excursion
    (outline-back-to-heading)
    (if (not (outline-invisible-p (line-end-position)))
	(hide-subtree)
      (show-children)
      (show-entry))))

(defun outline-flag-subtree (flag)
  (save-excursion
    (outline-back-to-heading)
    (outline-end-of-heading)
    (outline-flag-region (point)
			  (progn (outline-end-of-subtree) (point))
			  flag)))

(defun outline-end-of-subtree ()
  (outline-back-to-heading)
  (let ((first t)
	(level (funcall outline-level)))
    (while (and (not (eobp))
		(or first (> (funcall outline-level) level)))
      (setq first nil)
      (outline-next-heading))
    (if (and (bolp) (not (eolp)))
	;; We stopped at a nonempty line (the next heading).
	(progn
	  ;; Go to end of line before heading
	  (forward-char -1)
          (if (and outline-blank-line (bolp))
 	      ;; leave blank line before heading
 	      (forward-char -1))))))

(defun show-branches ()
  "Show all subheadings of this heading, but not their bodies."
  (interactive)
  (show-children 1000))

(defun show-children (&optional level)
  "Show all direct subheadings of this heading.
Prefix arg LEVEL is how many levels below the current level should be shown.
Default is enough to cause the following heading to appear."
  (interactive "P")
  (setq level
	(if level (prefix-numeric-value level)
	  (save-excursion
	    (outline-back-to-heading)
	    (let ((start-level (funcall outline-level)))
	      (outline-next-heading)
	      (if (eobp)
		  1
		(max 1 (- (funcall outline-level) start-level)))))))
  (let (outline-view-change-hook)
    (save-excursion
      (outline-back-to-heading)
      (setq level (+ level (funcall outline-level)))
      (outline-map-region
       (lambda ()
	 (if (<= (funcall outline-level) level)
	     (outline-show-heading)))
       (point)
       (progn (outline-end-of-subtree)
	      (if (eobp) (point-max) (1+ (point)))))))
  (run-hooks 'outline-view-change-hook))



(defun outline-up-heading (arg &optional invisible-ok)
  "Move to the visible heading line of which the present line is a subheading.
With argument, move up ARG levels.
If INVISIBLE-OK is non-nil, also consider invisible lines."
  (interactive "p")
  (and (eq this-command 'outline-up-heading)
       (or (eq last-command 'outline-up-heading) (push-mark)))
  (outline-back-to-heading invisible-ok)
  (let ((start-level (funcall outline-level)))
    (when (<= start-level 1)
      (error "Already at top level of the outline"))
    (while (and (> start-level 1) (> arg 0) (not (bobp)))
      (let ((level start-level))
	(while (not (or (< level start-level) (bobp)))
	  (if invisible-ok
	      (outline-previous-heading)
	    (outline-previous-visible-heading 1))
	  (setq level (funcall outline-level)))
	(setq start-level level))
      (setq arg (- arg 1))))
  (looking-at outline-regexp))

(defun outline-forward-same-level (arg)
  "Move forward to the ARG'th subheading at same level as this one.
Stop at the first and last subheadings of a superior heading."
  (interactive "p")
  (outline-back-to-heading)
  (while (> arg 0)
    (let ((point-to-move-to (save-excursion
			      (outline-get-next-sibling))))
      (if point-to-move-to
	  (progn
	    (goto-char point-to-move-to)
	    (setq arg (1- arg)))
	(progn
	  (setq arg 0)
	  (error "No following same-level heading"))))))

(defun outline-get-next-sibling ()
  "Move to next heading of the same level, and return point.
If there is no such heading, return nil."
  (let ((level (funcall outline-level)))
    (outline-next-visible-heading 1)
    (while (and (not (eobp)) (> (funcall outline-level) level))
      (outline-next-visible-heading 1))
    (if (or (eobp) (< (funcall outline-level) level))
	nil
      (point))))

(defun outline-backward-same-level (arg)
  "Move backward to the ARG'th subheading at same level as this one.
Stop at the first and last subheadings of a superior heading."
  (interactive "p")
  (outline-back-to-heading)
  (while (> arg 0)
    (let ((point-to-move-to (save-excursion
			      (outline-get-last-sibling))))
      (if point-to-move-to
	  (progn
	    (goto-char point-to-move-to)
	    (setq arg (1- arg)))
	(progn
	  (setq arg 0)
	  (error "No previous same-level heading"))))))

(defun outline-get-last-sibling ()
  "Move to previous heading of the same level, and return point.
If there is no such heading, return nil."
  (let ((opoint (point))
	(level (funcall outline-level)))
    (outline-previous-visible-heading 1)
    (when (and (/= (point) opoint) (outline-on-heading-p))
      (while (and (> (funcall outline-level) level)
		  (not (bobp)))
	(outline-previous-visible-heading 1))
      (if (< (funcall outline-level) level)
	  nil
        (point)))))

(defun outline-headers-as-kill (beg end)
  "Save the visible outline headers in region at the start of the kill ring.

Text shown between the headers isn't copied.  Two newlines are
inserted between saved headers.  Yanking the result may be a
convenient way to make a table of contents of the buffer."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((buffer (current-buffer))
	    start end)
	(with-temp-buffer
	  (with-current-buffer buffer
	    ;; Boundary condition: starting on heading:
	    (when (outline-on-heading-p)
	      (outline-back-to-heading)
	      (setq start (point)
		    end (progn (outline-end-of-heading)
			       (point)))
	      (insert-buffer-substring buffer start end)
	      (insert "\n\n")))
	  (let ((temp-buffer (current-buffer)))
	    (with-current-buffer buffer
	      (while (outline-next-heading)
		(unless (outline-invisible-p)
		  (setq start (point)
			end (progn (outline-end-of-heading) (point)))
		  (with-current-buffer temp-buffer
		    (insert-buffer-substring buffer start end)
		    (insert "\n\n"))))))
	  (kill-new (buffer-string)))))))

(provide 'outline)
(provide 'noutline)

;;; outline.el ends here
