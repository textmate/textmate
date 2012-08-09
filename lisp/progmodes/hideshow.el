;;; hideshow.el --- minor mode cmds to selectively display code/comment blocks

;; Copyright (C) 1994-2012 Free Software Foundation, Inc.

;; Author: Thien-Thi Nguyen <ttn@gnu.org>
;;      Dan Nicolaescu <dann@ics.uci.edu>
;; Keywords: C C++ java lisp tools editing comments blocks hiding outlines
;; Maintainer-Version: 5.65.2.2
;; Time-of-Day-Author-Most-Likely-to-be-Recalcitrant: early morning

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

;; * Commands provided
;;
;; This file provides Hideshow Minor Mode.  When active, nine commands
;; are available, implementing block hiding and showing.  They (and their
;; keybindings) are:
;;
;;   hs-hide-block                      C-c @ C-h
;;   hs-show-block                      C-c @ C-s
;;   hs-hide-all                        C-c @ C-M-h
;;   hs-show-all                        C-c @ C-M-s
;;   hs-hide-level                      C-c @ C-l
;;   hs-toggle-hiding                   C-c @ C-c
;;   hs-mouse-toggle-hiding             [(shift mouse-2)]
;;   hs-hide-initial-comment-block
;;
;; Blocks are defined per mode.  In c-mode, c++-mode and java-mode, they
;; are simply text between curly braces, while in Lisp-ish modes parens
;; are used.  Multi-line comment blocks can also be hidden.  Read-only
;; buffers are not a problem, since hideshow doesn't modify the text.
;;
;; The command `M-x hs-minor-mode' toggles the minor mode or sets it
;; (similar to other minor modes).

;; * Suggested usage
;;
;; First make sure hideshow.el is in a directory in your `load-path'.
;; You can optionally byte-compile it using `M-x byte-compile-file'.
;; Then, add the following to your ~/.emacs:
;;
;; (load-library "hideshow")
;; (add-hook 'X-mode-hook               ; other modes similarly
;;           (lambda () (hs-minor-mode 1)))
;;
;; where X = {emacs-lisp,c,c++,perl,...}.  You can also manually toggle
;; hideshow minor mode by typing `M-x hs-minor-mode'.  After hideshow is
;; activated or deactivated, `hs-minor-mode-hook' is run w/ `run-hooks'.
;;
;; Additionally, Joseph Eydelnant writes:
;;   I enjoy your package hideshow.el Ver. 5.24 2001/02/13
;;   a lot and I've been looking for the following functionality:
;;   toggle hide/show all with a single key.
;;   Here are a few lines of code that lets me do just that.
;;
;;   (defvar my-hs-hide nil "Current state of hideshow for toggling all.")
;;   ;;;###autoload
;;   (defun my-toggle-hideshow-all () "Toggle hideshow all."
;;     (interactive)
;;     (setq my-hs-hide (not my-hs-hide))
;;     (if my-hs-hide
;;         (hs-hide-all)
;;       (hs-show-all)))
;;
;; [Your hideshow hacks here!]

;; * Customization
;;
;; You can use `M-x customize-variable' on the following variables:
;;
;; - hs-hide-comments-when-hiding-all -- self-explanatory!
;; - hs-hide-all-non-comment-function -- if non-nil, when doing a
;;                                       `hs-hide-all', this function
;;                                       is called w/ no arguments
;; - hs-isearch-open                  -- what kind of hidden blocks to
;;                                       open when doing isearch
;;
;; Some languages (e.g., Java) are deeply nested, so the normal behavior
;; of `hs-hide-all' (hiding all but top-level blocks) results in very
;; little information shown, which is not very useful.  You can use the
;; variable `hs-hide-all-non-comment-function' to implement your idea of
;; what is more useful.  For example, the following code shows the next
;; nested level in addition to the top-level:
;;
;;   (defun ttn-hs-hide-level-1 ()
;;     (hs-hide-level 1)
;;     (forward-sexp 1))
;;   (setq hs-hide-all-non-comment-function 'ttn-hs-hide-level-1)
;;
;; Hideshow works w/ incremental search (isearch) by setting the variable
;; `hs-headline', which is the line of text at the beginning of a hidden
;; block that contains a match for the search.  You can have this show up
;; in the mode line by modifying the variable `mode-line-format'.  For
;; example, the following code prepends this info to the mode line:
;;
;;   (unless (memq 'hs-headline mode-line-format)
;;     (setq mode-line-format
;;           (append '("-" hs-headline) mode-line-format)))
;;
;; See documentation for `mode-line-format' for more info.
;;
;; Hooks are run after some commands:
;;
;;   hs-hide-hook     in      hs-hide-block, hs-hide-all, hs-hide-level
;;   hs-show-hook             hs-show-block, hs-show-all
;;
;; One of `hs-hide-hook' or `hs-show-hook' is run for the toggling
;; commands when the result of the toggle is to hide or show blocks,
;; respectively.  All hooks are run w/ `run-hooks'.  See docs for each
;; variable or hook for more info.
;;
;; Normally, hideshow tries to determine appropriate values for block
;; and comment definitions by examining the buffer's major mode.  If
;; there are problems, hideshow will not activate and in that case you
;; may wish to override hideshow's heuristics by adding an entry to
;; variable `hs-special-modes-alist'.  Packages that use hideshow should
;; do something like:
;;
;;   (add-to-list 'hs-special-modes-alist '(my-mode "{{" "}}" ...))
;;
;; If you have an entry that works particularly well, consider
;; submitting it for inclusion in hideshow.el.  See docstring for
;; `hs-special-modes-alist' for more info on the entry format.
;;
;; See also variable `hs-set-up-overlay' for per-block customization of
;; appearance or other effects associated with overlays.  For example:
;;
;; (setq hs-set-up-overlay
;;       (defun my-display-code-line-counts (ov)
;;         (when (eq 'code (overlay-get ov 'hs))
;;           (overlay-put ov 'display
;;                        (propertize
;;                         (format " ... <%d>"
;;                                 (count-lines (overlay-start ov)
;;                                              (overlay-end ov)))
;;                         'face 'font-lock-type-face)))))

;; * Bugs
;;
;; (1) Hideshow does not work w/ emacs 18 because emacs 18 lacks the
;;     function `forward-comment' (among other things).  If someone
;;     writes this, please send me a copy.
;;
;; (2) Sometimes `hs-headline' can become out of sync.  To reset, type
;;     `M-x hs-minor-mode' twice (that is, deactivate then re-activate
;;     hideshow).
;;
;; (3) Hideshow 5.x is developed and tested on GNU Emacs 20.7.
;;     XEmacs compatibility may have bitrotted since 4.29.
;;
;; (4) Some buffers can't be `byte-compile-file'd properly.  This is because
;;     `byte-compile-file' inserts the file to be compiled in a temporary
;;     buffer and switches `normal-mode' on.  In the case where you have
;;     `hs-hide-initial-comment-block' in `hs-minor-mode-hook', the hiding of
;;     the initial comment sometimes hides parts of the first statement (seems
;;     to be only in `normal-mode'), so there are unbalanced "(" and ")".
;;
;;     The workaround is to clear `hs-minor-mode-hook' when byte-compiling:
;;
;;     (defadvice byte-compile-file (around
;;                                   byte-compile-file-hideshow-off
;;                                   act)
;;       (let ((hs-minor-mode-hook nil))
;;         ad-do-it))
;;
;; (5) Hideshow interacts badly with Ediff and `vc-diff'.  At the moment, the
;;     suggested workaround is to turn off hideshow entirely, for example:
;;
;;     (add-hook 'ediff-prepare-buffer-hook 'turn-off-hideshow)
;;     (add-hook 'vc-before-checkin-hook 'turn-off-hideshow)
;;
;;     In the case of `vc-diff', here is a less invasive workaround:
;;
;;     (add-hook 'vc-before-checkin-hook
;;               (lambda ()
;;                 (goto-char (point-min))
;;                 (hs-show-block)))
;;
;;     Unfortunately, these workarounds do not restore hideshow state.
;;     If someone figures out a better way, please let me know.

;; * Correspondence
;;
;; Correspondence welcome; please indicate version number.  Send bug
;; reports and inquiries to <ttn@gnu.org>.

;; * Thanks
;;
;; Thanks go to the following people for valuable ideas, code and
;; bug reports.
;;
;;  Dean Andrews, Alf-Ivar Holm, Holger Bauer, Christoph Conrad, Dave Love,
;;  Dirk Herrmann, Gael Marziou, Jan Djarv, Guillaume Leray, Moody Ahmad,
;;  Preston F. Crow, Lars Lindberg, Reto Zimmermann, Keith Sheffield,
;;  Chew Meng Kuan, Tony Lam, Pete Ware, François Pinard, Stefan Monnier,
;;  Joseph Eydelnant, Michael Ernst, Peter Heslin
;;
;; Special thanks go to Dan Nicolaescu, who reimplemented hideshow using
;; overlays (rather than selective display), added isearch magic, folded
;; in custom.el compatibility, generalized comment handling, incorporated
;; mouse support, and maintained the code in general.  Version 4.0 is
;; largely due to his efforts.

;; * History
;;
;; Hideshow was inspired when I learned about selective display.  It was
;; reimplemented to use overlays for 4.0 (see above).  WRT older history,
;; entries in the masterfile corresponding to versions 1.x and 2.x have
;; been lost.  XEmacs support is reliable as of 4.29.  State save and
;; restore was added in 3.5 (not widely distributed), and reliable as of
;; 4.30.  Otherwise, the code seems stable.  Passes checkdoc as of 4.32.
;; Version 5.x uses new algorithms for block selection and traversal,
;; unbundles state save and restore, and includes more isearch support.

;;; Code:

;;---------------------------------------------------------------------------
;; user-configurable variables

(defgroup hideshow nil
  "Minor mode for hiding and showing program and comment blocks."
  :prefix "hs-"
  :group 'languages)

(defcustom hs-hide-comments-when-hiding-all t
  "*Hide the comments too when you do an `hs-hide-all'."
  :type 'boolean
  :group 'hideshow)

(defcustom hs-minor-mode-hook nil
  "*Hook called when hideshow minor mode is activated or deactivated."
  :type 'hook
  :group 'hideshow
  :version "21.1")

(defcustom hs-isearch-open 'code
  "*What kind of hidden blocks to open when doing `isearch'.
One of the following symbols:

  code    -- open only code blocks
  comment -- open only comment blocks
  t       -- open both code and comment blocks
  nil     -- open neither code nor comment blocks

This has effect only if `search-invisible' is set to `open'."
  :type '(choice (const :tag "open only code blocks" code)
                 (const :tag "open only comment blocks" comment)
                 (const :tag "open both code and comment blocks" t)
                 (const :tag "don't open any of them" nil))
  :group 'hideshow)

;;;###autoload
(defvar hs-special-modes-alist
  (mapcar 'purecopy
  '((c-mode "{" "}" "/[*/]" nil nil)
    (c++-mode "{" "}" "/[*/]" nil nil)
    (bibtex-mode ("@\\S(*\\(\\s(\\)" 1))
    (java-mode "{" "}" "/[*/]" nil nil)
    (js-mode "{" "}" "/[*/]" nil)))
  "*Alist for initializing the hideshow variables for different modes.
Each element has the form
  (MODE START END COMMENT-START FORWARD-SEXP-FUNC ADJUST-BEG-FUNC).

If non-nil, hideshow will use these values as regexps to define blocks
and comments, respectively for major mode MODE.

START, END and COMMENT-START are regular expressions.  A block is
defined as text surrounded by START and END.

As a special case, START may be a list of the form (COMPLEX-START
MDATA-SELECTOR), where COMPLEX-START is a regexp w/ multiple parts and
MDATA-SELECTOR an integer that specifies which sub-match is the proper
place to adjust point, before calling `hs-forward-sexp-func'.  Point
is adjusted to the beginning of the specified match.  For example,
see the `hs-special-modes-alist' entry for `bibtex-mode'.

For some major modes, `forward-sexp' does not work properly.  In those
cases, FORWARD-SEXP-FUNC specifies another function to use instead.

See the documentation for `hs-adjust-block-beginning' to see what is the
use of ADJUST-BEG-FUNC.

If any of the elements is left nil or omitted, hideshow tries to guess
appropriate values.  The regexps should not contain leading or trailing
whitespace.  Case does not matter.")

(defvar hs-hide-all-non-comment-function nil
  "*Function called if non-nil when doing `hs-hide-all' for non-comments.")

(defvar hs-allow-nesting nil
  "*If non-nil, hiding remembers internal blocks.
This means that when the outer block is shown again,
any previously hidden internal blocks remain hidden.")

(defvar hs-hide-hook nil
  "*Hook called (with `run-hooks') at the end of commands to hide text.
These commands include the toggling commands (when the result is to hide
a block), `hs-hide-all', `hs-hide-block' and `hs-hide-level'.")

(defvar hs-show-hook nil
  "*Hook called (with `run-hooks') at the end of commands to show text.
These commands include the toggling commands (when the result is to show
a block), `hs-show-all' and `hs-show-block'.")

(defvar hs-set-up-overlay nil
  "*Function called with one arg, OV, a newly initialized overlay.
Hideshow puts a unique overlay on each range of text to be hidden
in the buffer.  Here is a simple example of how to use this variable:

  (defun display-code-line-counts (ov)
    (when (eq 'code (overlay-get ov 'hs))
      (overlay-put ov 'display
                   (format \"... / %d\"
                           (count-lines (overlay-start ov)
                                        (overlay-end ov))))))

  (setq hs-set-up-overlay 'display-code-line-counts)

This example shows how to get information from the overlay as well
as how to set its `display' property.  See `hs-make-overlay' and
info node `(elisp)Overlays'.")

;;---------------------------------------------------------------------------
;; internal variables

(defvar hs-minor-mode nil
  "Non-nil if using hideshow mode as a minor mode of some other mode.
Use the command `hs-minor-mode' to toggle or set this variable.")

(defvar hs-minor-mode-map
  (let ((map (make-sparse-keymap)))
    ;; These bindings roughly imitate those used by Outline mode.
    (define-key map "\C-c@\C-h"	      'hs-hide-block)
    (define-key map "\C-c@\C-s"	      'hs-show-block)
    (define-key map "\C-c@\C-\M-h"    'hs-hide-all)
    (define-key map "\C-c@\C-\M-s"    'hs-show-all)
    (define-key map "\C-c@\C-l"	      'hs-hide-level)
    (define-key map "\C-c@\C-c"	      'hs-toggle-hiding)
    (define-key map [(shift mouse-2)] 'hs-mouse-toggle-hiding)
    map)
  "Keymap for hideshow minor mode.")

(easy-menu-define hs-minor-mode-menu hs-minor-mode-map
  "Menu used when hideshow minor mode is active."
  '("Hide/Show"
    ["Hide Block"    hs-hide-block
     :help "Hide the code or comment block at point"]
    ["Show Block"    hs-show-block
     :help "Show the code or comment block at point"]
    ["Hide All"      hs-hide-all
     :help "Hide all the blocks in the buffer"]
    ["Show All"      hs-show-all
     :help "Show all the blocks in the buffer"]
    ["Hide Level"    hs-hide-level
     :help "Hide all block at levels below the current block"]
    ["Toggle Hiding" hs-toggle-hiding
     :help "Toggle the hiding state of the current block"]
    "----"
    ["Hide comments when hiding all"
     (setq hs-hide-comments-when-hiding-all
    	   (not hs-hide-comments-when-hiding-all))
     :help "If t also hide comment blocks when doing `hs-hide-all'"
     :style toggle :selected hs-hide-comments-when-hiding-all]
   ("Reveal on isearch"
     ["Code blocks" (setq hs-isearch-open 'code)
      :help "Show hidden code blocks when isearch matches inside them"
      :active t :style radio   :selected (eq hs-isearch-open 'code)]
     ["Comment blocks" (setq hs-isearch-open 'comment)
      :help "Show hidden comment blocks when isearch matches inside them"
      :active t :style radio :selected (eq hs-isearch-open 'comment)]
     ["Code and Comment blocks" (setq hs-isearch-open t)
      :help "Show both hidden code and comment blocks when isearch matches inside them"
      :active t :style radio :selected (eq hs-isearch-open t)]
     ["None" (setq hs-isearch-open nil)
      :help "Do not hidden code or comment blocks when isearch matches inside them"
      :active t :style radio :selected (eq hs-isearch-open nil)])))

(defvar hs-c-start-regexp nil
  "Regexp for beginning of comments.
Differs from mode-specific comment regexps in that
surrounding whitespace is stripped.")
(make-variable-buffer-local 'hs-c-start-regexp)

(defvar hs-block-start-regexp nil
  "Regexp for beginning of block.")
(make-variable-buffer-local 'hs-block-start-regexp)

(defvar hs-block-start-mdata-select nil
  "Element in `hs-block-start-regexp' match data to consider as block start.
The internal function `hs-forward-sexp' moves point to the beginning of this
element (using `match-beginning') before calling `hs-forward-sexp-func'.")
(make-variable-buffer-local 'hs-block-start-mdata-select)

(defvar hs-block-end-regexp nil
  "Regexp for end of block.")

(defvar hs-forward-sexp-func 'forward-sexp
  "Function used to do a `forward-sexp'.
Should change for Algol-ish modes.  For single-character block
delimiters -- ie, the syntax table regexp for the character is
either `(' or `)' -- `hs-forward-sexp-func' would just be
`forward-sexp'.  For other modes such as simula, a more specialized
function is necessary.")
(make-variable-buffer-local 'hs-forward-sexp-func)

(defvar hs-adjust-block-beginning nil
  "Function used to tweak the block beginning.
The block is hidden from the position returned by this function,
as opposed to hiding it from the position returned when searching
for `hs-block-start-regexp'.

For example, in c-like modes, if we wish to also hide the curly braces
\(if you think they occupy too much space on the screen), this function
should return the starting point (at the end of line) of the hidden
region.

It is called with a single argument ARG which is the position in
buffer after the block beginning.

It should return the position from where we should start hiding.

It should not move the point.

See `hs-c-like-adjust-block-beginning' for an example of using this.")
(make-variable-buffer-local 'hs-adjust-block-beginning)

(defvar hs-headline nil
  "Text of the line where a hidden block begins, set during isearch.
You can display this in the mode line by adding the symbol `hs-headline'
to the variable `mode-line-format'.  For example,

  (unless (memq 'hs-headline mode-line-format)
    (setq mode-line-format
          (append '(\"-\" hs-headline) mode-line-format)))

Note that `mode-line-format' is buffer-local.")

;;---------------------------------------------------------------------------
;; support functions

(defun hs-discard-overlays (from to)
  "Delete hideshow overlays in region defined by FROM and TO.
Skip \"internal\" overlays if `hs-allow-nesting' is non-nil."
  (when (< to from)
    (setq from (prog1 to (setq to from))))
  (if hs-allow-nesting
      (let (ov)
        (while (> to (setq from (next-overlay-change from)))
          (when (setq ov (hs-overlay-at from))
            (setq from (overlay-end ov))
            (delete-overlay ov))))
    (dolist (ov (overlays-in from to))
      (when (overlay-get ov 'hs)
        (delete-overlay ov)))))

(defun hs-make-overlay (b e kind &optional b-offset e-offset)
  "Return a new overlay in region defined by B and E with type KIND.
KIND is either `code' or `comment'.  Optional fourth arg B-OFFSET
when added to B specifies the actual buffer position where the block
begins.  Likewise for optional fifth arg E-OFFSET.  If unspecified
they are taken to be 0 (zero).  The following properties are set
in the overlay: 'invisible 'hs 'hs-b-offset 'hs-e-offset.  Also,
depending on variable `hs-isearch-open', the following properties may
be present: 'isearch-open-invisible 'isearch-open-invisible-temporary.
If variable `hs-set-up-overlay' is non-nil it should specify a function
to call with the newly initialized overlay."
  (unless b-offset (setq b-offset 0))
  (unless e-offset (setq e-offset 0))
  (let ((ov (make-overlay b e))
        (io (if (eq 'block hs-isearch-open)
                ;; backward compatibility -- `block'<=>`code'
                'code
              hs-isearch-open)))
    (overlay-put ov 'invisible 'hs)
    (overlay-put ov 'hs kind)
    (overlay-put ov 'hs-b-offset b-offset)
    (overlay-put ov 'hs-e-offset e-offset)
    (when (or (eq io t) (eq io kind))
      (overlay-put ov 'isearch-open-invisible 'hs-isearch-show)
      (overlay-put ov 'isearch-open-invisible-temporary
                   'hs-isearch-show-temporary))
    (when hs-set-up-overlay (funcall hs-set-up-overlay ov))
    ov))

(defun hs-isearch-show (ov)
  "Delete overlay OV, and set `hs-headline' to nil.

This function is meant to be used as the `isearch-open-invisible'
property of an overlay."
  (setq hs-headline nil)
  (delete-overlay ov))

(defun hs-isearch-show-temporary (ov hide-p)
  "Hide or show overlay OV, and set `hs-headline', all depending on HIDE-P.
If HIDE-P is non-nil, `hs-headline' is set to nil and overlay OV is hidden.
Otherwise, `hs-headline' is set to the line of text at the head of OV, and
OV is shown.

This function is meant to be used as the `isearch-open-invisible-temporary'
property of an overlay."
  (setq hs-headline
        (if hide-p
            nil
          (or hs-headline
              (let ((start (overlay-start ov)))
                (buffer-substring
                 (save-excursion (goto-char start)
                                 (beginning-of-line)
                                 (skip-chars-forward " \t")
                                 (point))
                 start)))))
  (force-mode-line-update)
  ;; handle `display' property specially
  (let (value)
    (if hide-p
        (when (setq value (overlay-get ov 'hs-isearch-display))
          (overlay-put ov 'display value)
          (overlay-put ov 'hs-isearch-display nil))
      (when (setq value (overlay-get ov 'display))
        (overlay-put ov 'hs-isearch-display value)
        (overlay-put ov 'display nil))))
  (overlay-put ov 'invisible (and hide-p 'hs)))

(defun hs-looking-at-block-start-p ()
  "Return non-nil if the point is at the block start."
  (and (looking-at hs-block-start-regexp)
       (save-match-data (not (nth 8 (syntax-ppss))))))

(defun hs-forward-sexp (match-data arg)
  "Adjust point based on MATCH-DATA and call `hs-forward-sexp-func' w/ ARG.
Original match data is restored upon return."
  (save-match-data
    (set-match-data match-data)
    (goto-char (match-beginning hs-block-start-mdata-select))
    (funcall hs-forward-sexp-func arg)))

(defun hs-hide-comment-region (beg end &optional repos-end)
  "Hide a region from BEG to END, marking it as a comment.
Optional arg REPOS-END means reposition at end."
  (let ((beg-eol (progn (goto-char beg) (line-end-position)))
        (end-eol (progn (goto-char end) (line-end-position))))
    (hs-discard-overlays beg-eol end-eol)
    (hs-make-overlay beg-eol end-eol 'comment beg end))
  (goto-char (if repos-end end beg)))

(defun hs-hide-block-at-point (&optional end comment-reg)
  "Hide block if on block beginning.
Optional arg END means reposition at end.
Optional arg COMMENT-REG is a list of the form (BEGIN END) and
specifies the limits of the comment, or nil if the block is not
a comment.

The block beginning is adjusted by `hs-adjust-block-beginning'
and then further adjusted to be at the end of the line."
  (if comment-reg
      (hs-hide-comment-region (car comment-reg) (cadr comment-reg) end)
    (when (hs-looking-at-block-start-p)
      (let ((mdata (match-data t))
            (header-end (match-end 0))
            p q ov)
	;; `p' is the point at the end of the block beginning, which
	;; may need to be adjusted
	(save-excursion
	  (if hs-adjust-block-beginning
	      (goto-char (funcall hs-adjust-block-beginning
				  header-end))
	    (goto-char header-end))
	  (setq p (line-end-position)))
	;; `q' is the point at the end of the block
	(hs-forward-sexp mdata 1)
	(setq q (if (looking-back hs-block-end-regexp)
		    (match-beginning 0)
		  (point)))
        (when (and (< p q) (> (count-lines p q) 1))
          (cond ((and hs-allow-nesting (setq ov (hs-overlay-at p)))
                 (delete-overlay ov))
                ((not hs-allow-nesting)
                 (hs-discard-overlays p q)))
          (hs-make-overlay p q 'code (- header-end p)))
        (goto-char (if end q (min p header-end)))))))

(defun hs-inside-comment-p ()
  "Return non-nil if point is inside a comment, otherwise nil.
Actually, return a list containing the buffer position of the start
and the end of the comment.  A comment block can be hidden only if on
its starting line there is only whitespace preceding the actual comment
beginning.  If we are inside of a comment but this condition is not met,
we return a list having a nil as its car and the end of comment position
as cdr."
  (save-excursion
    ;; the idea is to look backwards for a comment start regexp, do a
    ;; forward comment, and see if we are inside, then extend
    ;; forward and backward as long as we have comments
    (let ((q (point)))
      (skip-chars-forward "[:blank:]")
      (when (or (looking-at hs-c-start-regexp)
                (re-search-backward hs-c-start-regexp (point-min) t))
        ;; first get to the beginning of this comment...
        (while (and (not (bobp))
                    (= (point) (progn (forward-comment -1) (point))))
          (forward-char -1))
        ;; ...then extend backwards
        (forward-comment (- (buffer-size)))
        (skip-chars-forward " \t\n\f")
        (let ((p (point))
              (hidable t))
          (beginning-of-line)
          (unless (looking-at (concat "[ \t]*" hs-c-start-regexp))
            ;; we are in this situation: (example)
            ;; (defun bar ()
            ;;      (foo)
            ;;                ) ; comment
            ;;                 ^
            ;;   the point was here before doing (beginning-of-line)
            ;; here we should advance till the next comment which
            ;; eventually has only white spaces preceding it on the same
            ;; line
            (goto-char p)
            (forward-comment 1)
            (skip-chars-forward " \t\n\f")
            (setq p (point))
            (while (and (< (point) q)
                        (> (point) p)
                        (not (looking-at hs-c-start-regexp)))
              ;; avoid an infinite cycle
              (setq p (point))
              (forward-comment 1)
              (skip-chars-forward " \t\n\f"))
            (when (or (not (looking-at hs-c-start-regexp))
                      (> (point) q))
              ;; we cannot hide this comment block
              (setq hidable nil)))
          ;; goto the end of the comment
          (forward-comment (buffer-size))
          (skip-chars-backward " \t\n\f")
          (end-of-line)
          (when (>= (point) q)
            (list (and hidable p) (point))))))))

(defun hs-grok-mode-type ()
  "Set up hideshow variables for new buffers.
If `hs-special-modes-alist' has information associated with the
current buffer's major mode, use that.
Otherwise, guess start, end and `comment-start' regexps; `forward-sexp'
function; and adjust-block-beginning function."
  (if (and (boundp 'comment-start)
           (boundp 'comment-end)
           comment-start comment-end)
      (let* ((lookup (assoc major-mode hs-special-modes-alist))
             (start-elem (or (nth 1 lookup) "\\s(")))
        (if (listp start-elem)
            ;; handle (START-REGEXP MDATA-SELECT)
            (setq hs-block-start-regexp (car start-elem)
                  hs-block-start-mdata-select (cadr start-elem))
          ;; backwards compatibility: handle simple START-REGEXP
          (setq hs-block-start-regexp start-elem
                hs-block-start-mdata-select 0))
        (setq hs-block-end-regexp (or (nth 2 lookup) "\\s)")
              hs-c-start-regexp (or (nth 3 lookup)
                                    (let ((c-start-regexp
                                           (regexp-quote comment-start)))
                                      (if (string-match " +$" c-start-regexp)
                                          (substring c-start-regexp
                                                     0 (1- (match-end 0)))
                                        c-start-regexp)))
              hs-forward-sexp-func (or (nth 4 lookup) 'forward-sexp)
              hs-adjust-block-beginning (nth 5 lookup)))
    (setq hs-minor-mode nil)
    (error "%s Mode doesn't support Hideshow Minor Mode"
           (format-mode-line mode-name))))

(defun hs-find-block-beginning ()
  "Reposition point at block-start.
Return point, or nil if original point was not in a block."
  (let ((done nil)
        (here (point)))
    ;; look if current line is block start
    (if (hs-looking-at-block-start-p)
        (point)
      ;; look backward for the start of a block that contains the cursor
      (while (and (re-search-backward hs-block-start-regexp nil t)
		  ;; go again if in a comment or a string
		  (or (save-match-data (nth 8 (syntax-ppss)))
		      (not (setq done
				 (< here (save-excursion
					   (hs-forward-sexp (match-data t) 1)
					   (point))))))))
      (if done
          (point)
        (goto-char here)
        nil))))

(defun hs-hide-level-recursive (arg minp maxp)
  "Recursively hide blocks ARG levels below point in region (MINP MAXP)."
  (when (hs-find-block-beginning)
    (setq minp (1+ (point)))
    (funcall hs-forward-sexp-func 1)
    (setq maxp (1- (point))))
  (unless hs-allow-nesting
    (hs-discard-overlays minp maxp))
  (goto-char minp)
  (while (progn
           (forward-comment (buffer-size))
           (and (< (point) maxp)
                (re-search-forward hs-block-start-regexp maxp t)))
    (when (save-match-data
	    (not (nth 8 (syntax-ppss)))) ; not inside comments or strings
      (if (> arg 1)
	  (hs-hide-level-recursive (1- arg) minp maxp)
	(goto-char (match-beginning hs-block-start-mdata-select))
	(hs-hide-block-at-point t))))
  (goto-char maxp))

(defmacro hs-life-goes-on (&rest body)
  "Evaluate BODY forms if variable `hs-minor-mode' is non-nil.
In the dynamic context of this macro, `inhibit-point-motion-hooks'
and `case-fold-search' are both t."
  `(when hs-minor-mode
     (let ((inhibit-point-motion-hooks t)
           (case-fold-search t))
       ,@body)))

(put 'hs-life-goes-on 'edebug-form-spec '(&rest form))

(defun hs-overlay-at (position)
  "Return hideshow overlay at POSITION, or nil if none to be found."
  (let ((overlays (overlays-at position))
        ov found)
    (while (and (not found) (setq ov (car overlays)))
      (setq found (and (overlay-get ov 'hs) ov)
            overlays (cdr overlays)))
    found))

(defun hs-already-hidden-p ()
  "Return non-nil if point is in an already-hidden block, otherwise nil."
  (save-excursion
    (let ((c-reg (hs-inside-comment-p)))
      (if (and c-reg (nth 0 c-reg))
          ;; point is inside a comment, and that comment is hidable
          (goto-char (nth 0 c-reg))
        (end-of-line)
        (when (and (not c-reg)
                   (hs-find-block-beginning)
		   (hs-looking-at-block-start-p))
          ;; point is inside a block
          (goto-char (match-end 0)))))
    (end-of-line)
    (hs-overlay-at (point))))

;; This function is not used anymore (Bug#700).
(defun hs-c-like-adjust-block-beginning (initial)
  "Adjust INITIAL, the buffer position after `hs-block-start-regexp'.
Actually, point is never moved; a new position is returned that is
the end of the C-function header.  This adjustment function is meant
to be assigned to `hs-adjust-block-beginning' for C-like modes."
  (save-excursion
    (goto-char (1- initial))
    (forward-comment (- (buffer-size)))
    (point)))

;;---------------------------------------------------------------------------
;; commands

(defun hs-hide-all ()
  "Hide all top level blocks, displaying only first and last lines.
Move point to the beginning of the line, and run the normal hook
`hs-hide-hook'.  See documentation for `run-hooks'.
If `hs-hide-comments-when-hiding-all' is non-nil, also hide the comments."
  (interactive)
  (hs-life-goes-on
   (save-excursion
     (unless hs-allow-nesting
       (hs-discard-overlays (point-min) (point-max)))
     (goto-char (point-min))
     (let ((spew (make-progress-reporter "Hiding all blocks..."
                                         (point-min) (point-max)))
           (re (concat "\\("
                       hs-block-start-regexp
                       "\\)"
                       (if hs-hide-comments-when-hiding-all
                           (concat "\\|\\("
                                   hs-c-start-regexp
                                   "\\)")
                         ""))))
       (while (progn
                (unless hs-hide-comments-when-hiding-all
                  (forward-comment (point-max)))
                (re-search-forward re (point-max) t))
         (if (match-beginning 1)
             ;; we have found a block beginning
             (progn
               (goto-char (match-beginning 1))
               (if hs-hide-all-non-comment-function
                   (funcall hs-hide-all-non-comment-function)
                 (hs-hide-block-at-point t)))
           ;; found a comment, probably
           (let ((c-reg (hs-inside-comment-p)))
             (when (and c-reg (car c-reg))
               (if (> (count-lines (car c-reg) (nth 1 c-reg)) 1)
                   (hs-hide-block-at-point t c-reg)
                 (goto-char (nth 1 c-reg))))))
         (progress-reporter-update spew (point)))
       (progress-reporter-done spew)))
   (beginning-of-line)
   (run-hooks 'hs-hide-hook)))

(defun hs-show-all ()
  "Show everything then run `hs-show-hook'.  See `run-hooks'."
  (interactive)
  (hs-life-goes-on
   (message "Showing all blocks ...")
   (let ((hs-allow-nesting nil))
     (hs-discard-overlays (point-min) (point-max)))
   (message "Showing all blocks ... done")
   (run-hooks 'hs-show-hook)))

(defun hs-hide-block (&optional end)
  "Select a block and hide it.  With prefix arg, reposition at END.
Upon completion, point is repositioned and the normal hook
`hs-hide-hook' is run.  See documentation for `run-hooks'."
  (interactive "P")
  (hs-life-goes-on
   (let ((c-reg (hs-inside-comment-p)))
     (cond
      ((and c-reg (or (null (nth 0 c-reg))
                      (<= (count-lines (car c-reg) (nth 1 c-reg)) 1)))
       (message "(not enough comment lines to hide)"))
      ((or c-reg
	   (hs-looking-at-block-start-p)
           (hs-find-block-beginning))
       (hs-hide-block-at-point end c-reg)
       (run-hooks 'hs-hide-hook))))))

(defun hs-show-block (&optional end)
  "Select a block and show it.
With prefix arg, reposition at END.  Upon completion, point is
repositioned and the normal hook `hs-show-hook' is run.
See documentation for functions `hs-hide-block' and `run-hooks'."
  (interactive "P")
  (hs-life-goes-on
   (or
    ;; first see if we have something at the end of the line
    (let ((ov (hs-overlay-at (line-end-position)))
          (here (point)))
      (when ov
        (goto-char
         (cond (end (overlay-end ov))
               ((eq 'comment (overlay-get ov 'hs)) here)
               (t (+ (overlay-start ov) (overlay-get ov 'hs-b-offset)))))
        (delete-overlay ov)
        t))
    ;; not immediately obvious, look for a suitable block
    (let ((c-reg (hs-inside-comment-p))
          p q)
      (cond (c-reg
             (when (car c-reg)
               (setq p (car c-reg)
                     q (cadr c-reg))))
            ((and (hs-find-block-beginning)
                  ;; ugh, fresh match-data
                  (hs-looking-at-block-start-p))
             (setq p (point)
                   q (progn (hs-forward-sexp (match-data t) 1) (point)))))
      (when (and p q)
        (hs-discard-overlays p q)
        (goto-char (if end q (1+ p))))))
   (run-hooks 'hs-show-hook)))

(defun hs-hide-level (arg)
  "Hide all blocks ARG levels below this block.
The hook `hs-hide-hook' is run; see `run-hooks'."
  (interactive "p")
  (hs-life-goes-on
   (save-excursion
     (message "Hiding blocks ...")
     (hs-hide-level-recursive arg (point-min) (point-max))
     (message "Hiding blocks ... done"))
   (run-hooks 'hs-hide-hook)))

(defun hs-toggle-hiding ()
  "Toggle hiding/showing of a block.
See `hs-hide-block' and `hs-show-block'."
  (interactive)
  (hs-life-goes-on
   (if (hs-already-hidden-p)
       (hs-show-block)
     (hs-hide-block))))

(defun hs-mouse-toggle-hiding (e)
  "Toggle hiding/showing of a block.
This command should be bound to a mouse key.
Argument E is a mouse event used by `mouse-set-point'.
See `hs-hide-block' and `hs-show-block'."
  (interactive "@e")
  (hs-life-goes-on
   (mouse-set-point e)
   (hs-toggle-hiding)))

(defun hs-hide-initial-comment-block ()
  "Hide the first block of comments in a file.
This can be useful if you have huge RCS logs in those comments."
  (interactive)
  (hs-life-goes-on
   (let ((c-reg (save-excursion
                  (goto-char (point-min))
                  (skip-chars-forward " \t\n\f")
                  (hs-inside-comment-p))))
     (when c-reg
       (let ((beg (car c-reg)) (end (cadr c-reg)))
         ;; see if we have enough comment lines to hide
         (when (> (count-lines beg end) 1)
           (hs-hide-comment-region beg end)))))))

;;;###autoload
(define-minor-mode hs-minor-mode
  "Minor mode to selectively hide/show code and comment blocks.
With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil.

When hideshow minor mode is on, the menu bar is augmented with hideshow
commands and the hideshow commands are enabled.
The value '(hs . t) is added to `buffer-invisibility-spec'.

The main commands are: `hs-hide-all', `hs-show-all', `hs-hide-block',
`hs-show-block', `hs-hide-level' and `hs-toggle-hiding'.  There is also
`hs-hide-initial-comment-block' and `hs-mouse-toggle-hiding'.

Turning hideshow minor mode off reverts the menu bar and the
variables to default values and disables the hideshow commands.

Lastly, the normal hook `hs-minor-mode-hook' is run using `run-hooks'.

Key bindings:
\\{hs-minor-mode-map}"
  :group 'hideshow
  :lighter " hs"
  :keymap hs-minor-mode-map
  (setq hs-headline nil)
  (if hs-minor-mode
      (progn
        (hs-grok-mode-type)
        ;; Turn off this mode if we change major modes.
        (add-hook 'change-major-mode-hook
                  'turn-off-hideshow
                  nil t)
        (easy-menu-add hs-minor-mode-menu)
        (set (make-local-variable 'line-move-ignore-invisible) t)
        (add-to-invisibility-spec '(hs . t)))
    (remove-from-invisibility-spec '(hs . t))
    ;; hs-show-all does nothing unless h-m-m is non-nil.
    (let ((hs-minor-mode t))
      (hs-show-all))))

;;;###autoload
(defun turn-off-hideshow ()
  "Unconditionally turn off `hs-minor-mode'."
  (hs-minor-mode -1))

;;---------------------------------------------------------------------------
;; that's it

(provide 'hideshow)

;;; hideshow.el ends here
