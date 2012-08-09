;;; diff-mode.el --- a mode for viewing/editing context diffs -*- lexical-binding: t -*-

;; Copyright (C) 1998-2012 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: convenience patch diff vc

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

;; Provides support for font-lock, outline, navigation
;; commands, editing and various conversions as well as jumping
;; to the corresponding source file.

;; Inspired by Pavel Machek's patch-mode.el (<pavel@@atrey.karlin.mff.cuni.cz>)
;; Some efforts were spent to have it somewhat compatible with XEmacs's
;; diff-mode as well as with compilation-minor-mode

;; Bugs:

;; - Reverse doesn't work with normal diffs.

;; Todo:

;; - Improve `diff-add-change-log-entries-other-window',
;;   it is very simplistic now.
;;
;; - Add a `delete-after-apply' so C-c C-a automatically deletes hunks.
;;   Also allow C-c C-a to delete already-applied hunks.
;;
;; - Try `diff <file> <hunk>' to try and fuzzily discover the source location
;;   of a hunk.  Show then the changes between <file> and <hunk> and make it
;;   possible to apply them to <file>, <hunk-src>, or <hunk-dst>.
;;   Or maybe just make it into a ".rej to diff3-markers converter".
;;   Maybe just use `wiggle' (by Neil Brown) to do it for us.
;;
;; - in diff-apply-hunk, strip context in replace-match to better
;;   preserve markers and spacing.
;; - Handle `diff -b' output in context->unified.

;;; Code:
(eval-when-compile (require 'cl))

(defvar add-log-buffer-file-name-function)


(defgroup diff-mode ()
  "Major mode for viewing/editing diffs."
  :version "21.1"
  :group 'tools
  :group 'diff)

(defcustom diff-default-read-only nil
  "If non-nil, `diff-mode' buffers default to being read-only."
  :type 'boolean
  :group 'diff-mode)

(defcustom diff-jump-to-old-file nil
  "Non-nil means `diff-goto-source' jumps to the old file.
Else, it jumps to the new file."
  :type 'boolean
  :group 'diff-mode)

(defcustom diff-update-on-the-fly t
  "Non-nil means hunk headers are kept up-to-date on-the-fly.
When editing a diff file, the line numbers in the hunk headers
need to be kept consistent with the actual diff.  This can
either be done on the fly (but this sometimes interacts poorly with the
undo mechanism) or whenever the file is written (can be slow
when editing big diffs)."
  :type 'boolean
  :group 'diff-mode)

(defcustom diff-advance-after-apply-hunk t
  "Non-nil means `diff-apply-hunk' will move to the next hunk after applying."
  :type 'boolean
  :group 'diff-mode)

(defcustom diff-mode-hook nil
  "Run after setting up the `diff-mode' major mode."
  :type 'hook
  :options '(diff-delete-empty-files diff-make-unified)
  :group 'diff-mode)

(defvar diff-vc-backend nil
  "The VC backend that created the current Diff buffer, if any.")

(defvar diff-outline-regexp
  "\\([*+][*+][*+] [^0-9]\\|@@ ...\\|\\*\\*\\* [0-9].\\|--- [0-9]..\\)")

;;;;
;;;; keymap, menu, ...
;;;;

(easy-mmode-defmap diff-mode-shared-map
  '(;; From Pavel Machek's patch-mode.
    ("n" . diff-hunk-next)
    ("N" . diff-file-next)
    ("p" . diff-hunk-prev)
    ("P" . diff-file-prev)
    ("\t" . diff-hunk-next)
    ([backtab] . diff-hunk-prev)
    ("k" . diff-hunk-kill)
    ("K" . diff-file-kill)
    ;; From compilation-minor-mode.
    ("}" . diff-file-next)
    ("{" . diff-file-prev)
    ("\C-m" . diff-goto-source)
    ([mouse-2] . diff-goto-source)
    ;; From XEmacs's diff-mode.
    ("W" . widen)
    ;;("." . diff-goto-source)		;display-buffer
    ;;("f" . diff-goto-source)		;find-file
    ("o" . diff-goto-source)		;other-window
    ;;("w" . diff-goto-source)		;other-frame
    ;;("N" . diff-narrow)
    ;;("h" . diff-show-header)
    ;;("j" . diff-show-difference)	;jump to Nth diff
    ;;("q" . diff-quit)
    ;; Not useful if you have to metafy them.
    ;;(" " . scroll-up)
    ;;("\177" . scroll-down)
    ("A" . diff-ediff-patch)
    ("r" . diff-restrict-view)
    ("R" . diff-reverse-direction))
  "Basic keymap for `diff-mode', bound to various prefix keys."
  :inherit special-mode-map)

(easy-mmode-defmap diff-mode-map
  `(("\e" . ,(let ((map (make-sparse-keymap)))
               ;; We want to inherit most bindings from diff-mode-shared-map,
               ;; but not all since they may hide useful M-<foo> global
               ;; bindings when editing.
               (set-keymap-parent map diff-mode-shared-map)
               (dolist (key '("A" "r" "R" "g" "q" "W" "z"))
                 (define-key map key nil))
               map))
    ;; From compilation-minor-mode.
    ("\C-c\C-c" . diff-goto-source)
    ;; By analogy with the global C-x 4 a binding.
    ("\C-x4A" . diff-add-change-log-entries-other-window)
    ;; Misc operations.
    ("\C-c\C-a" . diff-apply-hunk)
    ("\C-c\C-e" . diff-ediff-patch)
    ("\C-c\C-n" . diff-restrict-view)
    ("\C-c\C-s" . diff-split-hunk)
    ("\C-c\C-t" . diff-test-hunk)
    ("\C-c\C-r" . diff-reverse-direction)
    ("\C-c\C-u" . diff-context->unified)
    ;; `d' because it duplicates the context :-(  --Stef
    ("\C-c\C-d" . diff-unified->context)
    ("\C-c\C-w" . diff-ignore-whitespace-hunk)
    ("\C-c\C-b" . diff-refine-hunk)  ;No reason for `b' :-(
    ("\C-c\C-f" . next-error-follow-minor-mode))
  "Keymap for `diff-mode'.  See also `diff-mode-shared-map'.")

(easy-menu-define diff-mode-menu diff-mode-map
  "Menu for `diff-mode'."
  '("Diff"
    ["Jump to Source"		diff-goto-source
     :help "Jump to the corresponding source line"]
    ["Apply hunk"		diff-apply-hunk
     :help "Apply the current hunk to the source file and go to the next"]
    ["Test applying hunk"	diff-test-hunk
     :help "See whether it's possible to apply the current hunk"]
    ["Apply diff with Ediff"	diff-ediff-patch
     :help "Call `ediff-patch-file' on the current buffer"]
    ["Create Change Log entries" diff-add-change-log-entries-other-window
     :help "Create ChangeLog entries for the changes in the diff buffer"]
    "-----"
    ["Reverse direction"	diff-reverse-direction
     :help "Reverse the direction of the diffs"]
    ["Context -> Unified"	diff-context->unified
     :help "Convert context diffs to unified diffs"]
    ["Unified -> Context"	diff-unified->context
     :help "Convert unified diffs to context diffs"]
    ;;["Fixup Headers"		diff-fixup-modifs	(not buffer-read-only)]
    ["Show trailing whitespace" whitespace-mode
     :style toggle :selected (bound-and-true-p whitespace-mode)
     :help "Show trailing whitespace in modified lines"]
    "-----"
    ["Split hunk"		diff-split-hunk
     :active (diff-splittable-p)
     :help "Split the current (unified diff) hunk at point into two hunks"]
    ["Ignore whitespace changes" diff-ignore-whitespace-hunk
     :help "Re-diff the current hunk, ignoring whitespace differences"]
    ["Highlight fine changes"	diff-refine-hunk
     :help "Highlight changes of hunk at point at a finer granularity"]
    ["Kill current hunk"	diff-hunk-kill
     :help "Kill current hunk"]
    ["Kill current file's hunks" diff-file-kill
     :help "Kill all current file's hunks"]
    "-----"
    ["Previous Hunk"		diff-hunk-prev
     :help "Go to the previous count'th hunk"]
    ["Next Hunk"		diff-hunk-next
     :help "Go to the next count'th hunk"]
    ["Previous File"		diff-file-prev
     :help "Go to the previous count'th file"]
    ["Next File"		diff-file-next
     :help "Go to the next count'th file"]
    ))

(defcustom diff-minor-mode-prefix "\C-c="
  "Prefix key for `diff-minor-mode' commands."
  :type '(choice (string "\e") (string "C-c=") string)
  :group 'diff-mode)

(easy-mmode-defmap diff-minor-mode-map
  `((,diff-minor-mode-prefix . ,diff-mode-shared-map))
  "Keymap for `diff-minor-mode'.  See also `diff-mode-shared-map'.")

(define-minor-mode diff-auto-refine-mode
  "Toggle automatic diff hunk highlighting (Diff Auto Refine mode).
With a prefix argument ARG, enable Diff Auto Refine mode if ARG
is positive, and disable it otherwise.  If called from Lisp,
enable the mode if ARG is omitted or nil.

Diff Auto Refine mode is a buffer-local minor mode used with
`diff-mode'.  When enabled, Emacs automatically highlights
changes in detail as the user visits hunks.  When transitioning
from disabled to enabled, it tries to refine the current hunk, as
well."
  :group 'diff-mode :init-value t :lighter nil ;; " Auto-Refine"
  (when diff-auto-refine-mode
    (condition-case-unless-debug nil (diff-refine-hunk) (error nil))))

;;;;
;;;; font-lock support
;;;;

(defface diff-header
  '((((class color) (min-colors 88) (background light))
     :background "grey80")
    (((class color) (min-colors 88) (background dark))
     :background "grey45")
    (((class color) (background light))
     :foreground "blue1" :weight bold)
    (((class color) (background dark))
     :foreground "green" :weight bold)
    (t :weight bold))
  "`diff-mode' face inherited by hunk and index header faces."
  :group 'diff-mode)
(define-obsolete-face-alias 'diff-header-face 'diff-header "22.1")
(defvar diff-header-face 'diff-header)

(defface diff-file-header
  '((((class color) (min-colors 88) (background light))
     :background "grey70" :weight bold)
    (((class color) (min-colors 88) (background dark))
     :background "grey60" :weight bold)
    (((class color) (background light))
     :foreground "green" :weight bold)
    (((class color) (background dark))
     :foreground "cyan" :weight bold)
    (t :weight bold))			; :height 1.3
  "`diff-mode' face used to highlight file header lines."
  :group 'diff-mode)
(define-obsolete-face-alias 'diff-file-header-face 'diff-file-header "22.1")
(defvar diff-file-header-face 'diff-file-header)

(defface diff-index
  '((t :inherit diff-file-header))
  "`diff-mode' face used to highlight index header lines."
  :group 'diff-mode)
(define-obsolete-face-alias 'diff-index-face 'diff-index "22.1")
(defvar diff-index-face 'diff-index)

(defface diff-hunk-header
  '((t :inherit diff-header))
  "`diff-mode' face used to highlight hunk header lines."
  :group 'diff-mode)
(define-obsolete-face-alias 'diff-hunk-header-face 'diff-hunk-header "22.1")
(defvar diff-hunk-header-face 'diff-hunk-header)

(defface diff-removed
  '((t :inherit diff-changed))
  "`diff-mode' face used to highlight removed lines."
  :group 'diff-mode)
(define-obsolete-face-alias 'diff-removed-face 'diff-removed "22.1")
(defvar diff-removed-face 'diff-removed)

(defface diff-added
  '((t :inherit diff-changed))
  "`diff-mode' face used to highlight added lines."
  :group 'diff-mode)
(define-obsolete-face-alias 'diff-added-face 'diff-added "22.1")
(defvar diff-added-face 'diff-added)

(defface diff-changed
  ;; We normally apply a `shadow'-based face on the `diff-context'
  ;; face, and keep `diff-changed' the default.
  '((((class color grayscale) (min-colors 88)))
    ;; If the terminal lacks sufficient colors for shadowing,
    ;; highlight changed lines explicitly.
    (((class color) (background light))
     :foreground "magenta" :weight bold :slant italic)
    (((class color) (background dark))
     :foreground "yellow" :weight bold :slant italic))
  "`diff-mode' face used to highlight changed lines."
  :group 'diff-mode)
(define-obsolete-face-alias 'diff-changed-face 'diff-changed "22.1")
(defvar diff-changed-face 'diff-changed)

(defface diff-indicator-removed
  '((t :inherit diff-removed))
  "`diff-mode' face used to highlight indicator of removed lines (-, <)."
  :group 'diff-mode
  :version "22.1")
(defvar diff-indicator-removed-face 'diff-indicator-removed)

(defface diff-indicator-added
  '((t :inherit diff-added))
  "`diff-mode' face used to highlight indicator of added lines (+, >)."
  :group 'diff-mode
  :version "22.1")
(defvar diff-indicator-added-face 'diff-indicator-added)

(defface diff-indicator-changed
  '((t :inherit diff-changed))
  "`diff-mode' face used to highlight indicator of changed lines."
  :group 'diff-mode
  :version "22.1")
(defvar diff-indicator-changed-face 'diff-indicator-changed)

(defface diff-function
  '((t :inherit diff-header))
  "`diff-mode' face used to highlight function names produced by \"diff -p\"."
  :group 'diff-mode)
(define-obsolete-face-alias 'diff-function-face 'diff-function "22.1")
(defvar diff-function-face 'diff-function)

(defface diff-context
  '((((class color grayscale) (min-colors 88)) :inherit shadow))
  "`diff-mode' face used to highlight context and other side-information."
  :group 'diff-mode)
(define-obsolete-face-alias 'diff-context-face 'diff-context "22.1")
(defvar diff-context-face 'diff-context)

(defface diff-nonexistent
  '((t :inherit diff-file-header))
  "`diff-mode' face used to highlight nonexistent files in recursive diffs."
  :group 'diff-mode)
(define-obsolete-face-alias 'diff-nonexistent-face 'diff-nonexistent "22.1")
(defvar diff-nonexistent-face 'diff-nonexistent)

(defconst diff-yank-handler '(diff-yank-function))
(defun diff-yank-function (text)
  ;; FIXME: the yank-handler is now called separately on each piece of text
  ;; with a yank-handler property, so the next-single-property-change call
  ;; below will always return nil :-(   --stef
  (let ((mixed (next-single-property-change 0 'yank-handler text))
	(start (point)))
    ;; First insert the text.
    (insert text)
    ;; If the text does not include any diff markers and if we're not
    ;; yanking back into a diff-mode buffer, get rid of the prefixes.
    (unless (or mixed (derived-mode-p 'diff-mode))
      (undo-boundary)		; Just in case the user wanted the prefixes.
      (let ((re (save-excursion
		  (if (re-search-backward "^[><!][ \t]" start t)
		      (if (eq (char-after) ?!)
			  "^[!+- ][ \t]" "^[<>][ \t]")
		    "^[ <>!+-]"))))
	(save-excursion
	  (while (re-search-backward re start t)
	    (replace-match "" t t)))))))

(defconst diff-hunk-header-re-unified
  "^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@")
(defconst diff-context-mid-hunk-header-re
  "--- \\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? ----$")

(defvar diff-font-lock-keywords
  `((,(concat "\\(" diff-hunk-header-re-unified "\\)\\(.*\\)$")
     (1 diff-hunk-header-face) (6 diff-function-face))
    ("^\\(\\*\\{15\\}\\)\\(.*\\)$"                        ;context
     (1 diff-hunk-header-face) (2 diff-function-face))
    ("^\\*\\*\\* .+ \\*\\*\\*\\*". diff-hunk-header-face) ;context
    (,diff-context-mid-hunk-header-re . diff-hunk-header-face) ;context
    ("^[0-9,]+[acd][0-9,]+$"     . diff-hunk-header-face) ;normal
    ("^---$"                     . diff-hunk-header-face) ;normal
    ;; For file headers, accept files with spaces, but be careful to rule
    ;; out false-positives when matching hunk headers.
    ("^\\(---\\|\\+\\+\\+\\|\\*\\*\\*\\) \\([^\t\n]+?\\)\\(?:\t.*\\| \\(\\*\\*\\*\\*\\|----\\)\\)?\n"
     (0 diff-header-face)
     (2 (if (not (match-end 3)) diff-file-header-face) prepend))
    ("^\\([-<]\\)\\(.*\n\\)"
     (1 diff-indicator-removed-face) (2 diff-removed-face))
    ("^\\([+>]\\)\\(.*\n\\)"
     (1 diff-indicator-added-face) (2 diff-added-face))
    ("^\\(!\\)\\(.*\n\\)"
     (1 diff-indicator-changed-face) (2 diff-changed-face))
    ("^Index: \\(.+\\).*\n"
     (0 diff-header-face) (1 diff-index-face prepend))
    ("^Only in .*\n" . diff-nonexistent-face)
    ("^\\(#\\)\\(.*\\)"
     (1 font-lock-comment-delimiter-face)
     (2 font-lock-comment-face))
    ("^[^-=+*!<>#].*\n" (0 diff-context-face))))

(defconst diff-font-lock-defaults
  '(diff-font-lock-keywords t nil nil nil (font-lock-multiline . nil)))

(defvar diff-imenu-generic-expression
  ;; Prefer second name as first is most likely to be a backup or
  ;; version-control name.  The [\t\n] at the end of the unidiff pattern
  ;; catches Debian source diff files (which lack the trailing date).
  '((nil "\\+\\+\\+\\ \\([^\t\n]+\\)[\t\n]" 1) ; unidiffs
    (nil "^--- \\([^\t\n]+\\)\t.*\n\\*" 1))) ; context diffs

;;;;
;;;; Movement
;;;;

(defvar diff-valid-unified-empty-line t
  "If non-nil, empty lines are valid in unified diffs.
Some versions of diff replace all-blank context lines in unified format with
empty lines.  This makes the format less robust, but is tolerated.
See http://lists.gnu.org/archive/html/emacs-devel/2007-11/msg01990.html")

(defconst diff-hunk-header-re
  (concat "^\\(?:" diff-hunk-header-re-unified ".*\\|\\*\\{15\\}.*\n\\*\\*\\* .+ \\*\\*\\*\\*\\|[0-9]+\\(,[0-9]+\\)?[acd][0-9]+\\(,[0-9]+\\)?\\)$"))
(defconst diff-file-header-re (concat "^\\(--- .+\n\\+\\+\\+ \\|\\*\\*\\* .+\n--- \\|[^-+!<>0-9@* \n]\\).+\n" (substring diff-hunk-header-re 1)))
(defvar diff-narrowed-to nil)

(defun diff-hunk-style (&optional style)
  (when (looking-at diff-hunk-header-re)
    (setq style (cdr (assq (char-after) '((?@ . unified) (?* . context)))))
    (goto-char (match-end 0)))
  style)

(defun diff-end-of-hunk (&optional style donttrustheader)
  (let (end)
    (when (looking-at diff-hunk-header-re)
      ;; Especially important for unified (because headers are ambiguous).
      (setq style (diff-hunk-style style))
      (goto-char (match-end 0))
      (when (and (not donttrustheader) (match-end 2))
        (let* ((nold (string-to-number (or (match-string 2) "1")))
               (nnew (string-to-number (or (match-string 4) "1")))
               (endold
        (save-excursion
          (re-search-forward (if diff-valid-unified-empty-line
                                 "^[- \n]" "^[- ]")
                                     nil t nold)
                  (line-beginning-position 2)))
               (endnew
                ;; The hunk may end with a bunch of "+" lines, so the `end' is
                ;; then further than computed above.
                (save-excursion
                  (re-search-forward (if diff-valid-unified-empty-line
                                         "^[+ \n]" "^[+ ]")
                                     nil t nnew)
                  (line-beginning-position 2))))
          (setq end (max endold endnew)))))
    ;; We may have a first evaluation of `end' thanks to the hunk header.
    (unless end
      (setq end (and (re-search-forward
                      (case style
                        (unified (concat (if diff-valid-unified-empty-line
                                             "^[^-+# \\\n]\\|" "^[^-+# \\]\\|")
                                         ;; A `unified' header is ambiguous.
                                         diff-file-header-re))
                        (context "^[^-+#! \\]")
                        (normal "^[^<>#\\]")
                        (t "^[^-+#!<> \\]"))
                      nil t)
                     (match-beginning 0)))
      (when diff-valid-unified-empty-line
        ;; While empty lines may be valid inside hunks, they are also likely
        ;; to be unrelated to the hunk.
        (goto-char (or end (point-max)))
        (while (eq ?\n (char-before (1- (point))))
          (forward-char -1)
          (setq end (point)))))
    ;; The return value is used by easy-mmode-define-navigation.
    (goto-char (or end (point-max)))))

(defun diff-beginning-of-hunk (&optional try-harder)
  "Move back to beginning of hunk.
If TRY-HARDER is non-nil, try to cater to the case where we're not in a hunk
but in the file header instead, in which case move forward to the first hunk."
  (beginning-of-line)
  (unless (looking-at diff-hunk-header-re)
    (forward-line 1)
    (condition-case ()
	(re-search-backward diff-hunk-header-re)
      (error
       (if (not try-harder)
           (error "Can't find the beginning of the hunk")
         (diff-beginning-of-file-and-junk)
         (diff-hunk-next))))))

(defun diff-unified-hunk-p ()
  (save-excursion
    (ignore-errors
      (diff-beginning-of-hunk)
      (looking-at "^@@"))))

(defun diff-beginning-of-file ()
  (beginning-of-line)
  (unless (looking-at diff-file-header-re)
    (let ((start (point))
          res)
      ;; diff-file-header-re may need to match up to 4 lines, so in case
      ;; we're inside the header, we need to move up to 3 lines forward.
      (forward-line 3)
      (if (and (setq res (re-search-backward diff-file-header-re nil t))
               ;; Maybe the 3 lines forward were too much and we matched
               ;; a file header after our starting point :-(
               (or (<= (point) start)
                   (setq res (re-search-backward diff-file-header-re nil t))))
          res
        (goto-char start)
        (error "Can't find the beginning of the file")))))


(defun diff-end-of-file ()
  (re-search-forward "^[-+#!<>0-9@* \\]" nil t)
  (re-search-forward (concat "^[^-+#!<>0-9@* \\]\\|" diff-file-header-re)
		     nil 'move)
  (if (match-beginning 1)
      (goto-char (match-beginning 1))
    (beginning-of-line)))

;; Define diff-{hunk,file}-{prev,next}
(easy-mmode-define-navigation
 diff-hunk diff-hunk-header-re "hunk" diff-end-of-hunk diff-restrict-view
 (if diff-auto-refine-mode
     (condition-case-unless-debug nil (diff-refine-hunk) (error nil))))

(easy-mmode-define-navigation
 diff-file diff-file-header-re "file" diff-end-of-hunk)

(defun diff-restrict-view (&optional arg)
  "Restrict the view to the current hunk.
If the prefix ARG is given, restrict the view to the current file instead."
  (interactive "P")
  (save-excursion
    (if arg (diff-beginning-of-file) (diff-beginning-of-hunk 'try-harder))
    (narrow-to-region (point)
		      (progn (if arg (diff-end-of-file) (diff-end-of-hunk))
			     (point)))
    (set (make-local-variable 'diff-narrowed-to) (if arg 'file 'hunk))))


(defun diff-hunk-kill ()
  "Kill current hunk."
  (interactive)
  (diff-beginning-of-hunk)
  (let* ((start (point))
         ;; Search the second match, since we're looking at the first.
	 (nexthunk (when (re-search-forward diff-hunk-header-re nil t 2)
		     (match-beginning 0)))
	 (firsthunk (ignore-errors
		      (goto-char start)
		      (diff-beginning-of-file) (diff-hunk-next) (point)))
	 (nextfile (ignore-errors (diff-file-next) (point)))
	 (inhibit-read-only t))
    (goto-char start)
    (if (and firsthunk (= firsthunk start)
	     (or (null nexthunk)
		 (and nextfile (> nexthunk nextfile))))
	;; It's the only hunk for this file, so kill the file.
	(diff-file-kill)
      (diff-end-of-hunk)
      (kill-region start (point)))))

;; "index ", "old mode", "new mode", "new file mode" and
;; "deleted file mode" are output by git-diff.
(defconst diff-file-junk-re
  "diff \\|index \\|\\(?:deleted file\\|new\\(?: file\\)?\\|old\\) mode")

(defun diff-beginning-of-file-and-junk ()
  "Go to the beginning of file-related diff-info.
This is like `diff-beginning-of-file' except it tries to skip back over leading
data such as \"Index: ...\" and such."
  (let* ((orig (point))
         ;; Skip forward over what might be "leading junk" so as to get
         ;; closer to the actual diff.
         (_ (progn (beginning-of-line)
                   (while (looking-at diff-file-junk-re)
                     (forward-line 1))))
         (start (point))
         (prevfile (condition-case err
                       (save-excursion (diff-beginning-of-file) (point))
                     (error err)))
         (err (if (consp prevfile) prevfile))
         (nextfile (ignore-errors
                     (save-excursion
                       (goto-char start) (diff-file-next) (point))))
         ;; prevhunk is one of the limits.
         (prevhunk (save-excursion
                     (ignore-errors
                       (if (numberp prevfile) (goto-char prevfile))
                       (diff-hunk-prev) (point))))
         (previndex (save-excursion
                      (forward-line 1)  ;In case we're looking at "Index:".
                      (re-search-backward "^Index: " prevhunk t))))
    ;; If we're in the junk, we should use nextfile instead of prevfile.
    (if (and (numberp nextfile)
             (or (not (numberp prevfile))
                 (and previndex (> previndex prevfile))))
        (setq prevfile nextfile))
    (if (and previndex (numberp prevfile) (< previndex prevfile))
        (setq prevfile previndex))
    (if (and (numberp prevfile) (<= prevfile start))
          (progn
            (goto-char prevfile)
            ;; Now skip backward over the leading junk we may have before the
            ;; diff itself.
            (while (save-excursion
                     (and (zerop (forward-line -1))
                          (looking-at diff-file-junk-re)))
              (forward-line -1)))
      ;; File starts *after* the starting point: we really weren't in
      ;; a file diff but elsewhere.
      (goto-char orig)
      (signal (car err) (cdr err)))))

(defun diff-file-kill ()
  "Kill current file's hunks."
  (interactive)
  (let ((orig (point))
        (start (progn (diff-beginning-of-file-and-junk) (point)))
	 (inhibit-read-only t))
    (diff-end-of-file)
    (if (looking-at "^\n") (forward-char 1)) ;`tla' generates such diffs.
    (if (> orig (point)) (error "Not inside a file diff"))
    (kill-region start (point))))

(defun diff-kill-junk ()
  "Kill spurious empty diffs."
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (while (re-search-forward (concat "^\\(Index: .*\n\\)"
					"\\([^-+!* <>].*\n\\)*?"
					"\\(\\(Index:\\) \\|"
					diff-file-header-re "\\)")
				nil t)
	(delete-region (if (match-end 4) (match-beginning 0) (match-end 1))
		       (match-beginning 3))
	(beginning-of-line)))))

(defun diff-count-matches (re start end)
  (save-excursion
    (let ((n 0))
      (goto-char start)
      (while (re-search-forward re end t) (incf n))
      n)))

(defun diff-splittable-p ()
  (save-excursion
    (beginning-of-line)
    (and (looking-at "^[-+ ]")
         (progn (forward-line -1) (looking-at "^[-+ ]"))
         (diff-unified-hunk-p))))

(defun diff-split-hunk ()
  "Split the current (unified diff) hunk at point into two hunks."
  (interactive)
  (beginning-of-line)
  (let ((pos (point))
	(start (progn (diff-beginning-of-hunk) (point))))
    (unless (looking-at diff-hunk-header-re-unified)
      (error "diff-split-hunk only works on unified context diffs"))
    (forward-line 1)
    (let* ((start1 (string-to-number (match-string 1)))
	   (start2 (string-to-number (match-string 3)))
	   (newstart1 (+ start1 (diff-count-matches "^[- \t]" (point) pos)))
	   (newstart2 (+ start2 (diff-count-matches "^[+ \t]" (point) pos)))
	   (inhibit-read-only t))
      (goto-char pos)
      ;; Hopefully the after-change-function will not screw us over.
      (insert "@@ -" (number-to-string newstart1) ",1 +"
	      (number-to-string newstart2) ",1 @@\n")
      ;; Fix the original hunk-header.
      (diff-fixup-modifs start pos))))


;;;;
;;;; jump to other buffers
;;;;

(defvar diff-remembered-files-alist nil)
(defvar diff-remembered-defdir nil)

(defun diff-filename-drop-dir (file)
  (when (string-match "/" file) (substring file (match-end 0))))

(defun diff-merge-strings (ancestor from to)
  "Merge the diff between ANCESTOR and FROM into TO.
Returns the merged string if successful or nil otherwise.
The strings are assumed not to contain any \"\\n\" (i.e. end of line).
If ANCESTOR = FROM, returns TO.
If ANCESTOR = TO, returns FROM.
The heuristic is simplistic and only really works for cases
like \(diff-merge-strings \"b/foo\" \"b/bar\" \"/a/c/foo\")."
  ;; Ideally, we want:
  ;;   AMB ANB CMD -> CND
  ;; but that's ambiguous if `foo' or `bar' is empty:
  ;; a/foo a/foo1 b/foo.c -> b/foo1.c but not 1b/foo.c or b/foo.c1
  (let ((str (concat ancestor "\n" from "\n" to)))
    (when (and (string-match (concat
			      "\\`\\(.*?\\)\\(.*\\)\\(.*\\)\n"
			      "\\1\\(.*\\)\\3\n"
			      "\\(.*\\(\\2\\).*\\)\\'") str)
	       (equal to (match-string 5 str)))
      (concat (substring str (match-beginning 5) (match-beginning 6))
	      (match-string 4 str)
	      (substring str (match-end 6) (match-end 5))))))

(defun diff-tell-file-name (old name)
  "Tell Emacs where the find the source file of the current hunk.
If the OLD prefix arg is passed, tell the file NAME of the old file."
  (interactive
   (let* ((old current-prefix-arg)
	  (fs (diff-hunk-file-names current-prefix-arg)))
     (unless fs (error "No file name to look for"))
     (list old (read-file-name (format "File for %s: " (car fs))
			       nil (diff-find-file-name old 'noprompt) t))))
  (let ((fs (diff-hunk-file-names old)))
    (unless fs (error "No file name to look for"))
    (push (cons fs name) diff-remembered-files-alist)))

(defun diff-hunk-file-names (&optional old)
  "Give the list of file names textually mentioned for the current hunk."
  (save-excursion
    (unless (looking-at diff-file-header-re)
      (or (ignore-errors (diff-beginning-of-file))
	  (re-search-forward diff-file-header-re nil t)))
    (let ((limit (save-excursion
		   (condition-case ()
		       (progn (diff-hunk-prev) (point))
		     (error (point-min)))))
	  (header-files
	   (if (looking-at "[-*][-*][-*] \\(\\S-+\\)\\(\\s-.*\\)?\n[-+][-+][-+] \\(\\S-+\\)")
	       (list (if old (match-string 1) (match-string 3))
		     (if old (match-string 3) (match-string 1)))
	     (forward-line 1) nil)))
      (delq nil
	    (append
	     (when (and (not old)
			(save-excursion
			  (re-search-backward "^Index: \\(.+\\)" limit t)))
	       (list (match-string 1)))
	     header-files
	     (when (re-search-backward
		    "^diff \\(-\\S-+ +\\)*\\(\\S-+\\)\\( +\\(\\S-+\\)\\)?"
		    nil t)
	       (list (if old (match-string 2) (match-string 4))
		     (if old (match-string 4) (match-string 2)))))))))

(defun diff-find-file-name (&optional old noprompt prefix)
  "Return the file corresponding to the current patch.
Non-nil OLD means that we want the old file.
Non-nil NOPROMPT means to prefer returning nil than to prompt the user.
PREFIX is only used internally: don't use it."
  (unless (equal diff-remembered-defdir default-directory)
    ;; Flush diff-remembered-files-alist if the default-directory is changed.
    (set (make-local-variable 'diff-remembered-defdir) default-directory)
    (set (make-local-variable 'diff-remembered-files-alist) nil))
  (save-excursion
    (unless (looking-at diff-file-header-re)
      (or (ignore-errors (diff-beginning-of-file))
	  (re-search-forward diff-file-header-re nil t)))
    (let ((fs (diff-hunk-file-names old)))
      (if prefix (setq fs (mapcar (lambda (f) (concat prefix f)) fs)))
      (or
       ;; use any previously used preference
       (cdr (assoc fs diff-remembered-files-alist))
       ;; try to be clever and use previous choices as an inspiration
       (dolist (rf diff-remembered-files-alist)
	 (let ((newfile (diff-merge-strings (caar rf) (car fs) (cdr rf))))
	   (if (and newfile (file-exists-p newfile)) (return newfile))))
       ;; look for each file in turn.  If none found, try again but
       ;; ignoring the first level of directory, ...
       (do* ((files fs (delq nil (mapcar 'diff-filename-drop-dir files)))
	     (file nil nil))
	   ((or (null files)
		(setq file (do* ((files files (cdr files))
				 (file (car files) (car files)))
			       ;; Use file-regular-p to avoid
			       ;; /dev/null, directories, etc.
			       ((or (null file) (file-regular-p file))
				file))))
	    file))
       ;; <foo>.rej patches implicitly apply to <foo>
       (and (string-match "\\.rej\\'" (or buffer-file-name ""))
	    (let ((file (substring buffer-file-name 0 (match-beginning 0))))
	      (when (file-exists-p file) file)))
       ;; If we haven't found the file, maybe it's because we haven't paid
       ;; attention to the PCL-CVS hint.
       (and (not prefix)
	    (boundp 'cvs-pcl-cvs-dirchange-re)
	    (save-excursion
	      (re-search-backward cvs-pcl-cvs-dirchange-re nil t))
	    (diff-find-file-name old noprompt (match-string 1)))
       ;; if all else fails, ask the user
       (unless noprompt
         (let ((file (expand-file-name (or (first fs) ""))))
	   (setq file
		 (read-file-name (format "Use file %s: " file)
				 (file-name-directory file) file t
				 (file-name-nondirectory file)))
           (set (make-local-variable 'diff-remembered-files-alist)
                (cons (cons fs file) diff-remembered-files-alist))
           file))))))


(defun diff-ediff-patch ()
  "Call `ediff-patch-file' on the current buffer."
  (interactive)
  (condition-case nil
      (ediff-patch-file nil (current-buffer))
    (wrong-number-of-arguments (ediff-patch-file))))

;;;;
;;;; Conversion functions
;;;;

;;(defvar diff-inhibit-after-change nil
;;  "Non-nil means inhibit `diff-mode's after-change functions.")

(defun diff-unified->context (start end)
  "Convert unified diffs to context diffs.
START and END are either taken from the region (if a prefix arg is given) or
else cover the whole buffer."
  (interactive (if (or current-prefix-arg (and transient-mark-mode mark-active))
		   (list (region-beginning) (region-end))
		 (list (point-min) (point-max))))
  (unless (markerp end) (setq end (copy-marker end t)))
  (let (;;(diff-inhibit-after-change t)
	(inhibit-read-only t))
    (save-excursion
      (goto-char start)
      (while (and (re-search-forward
                   (concat "^\\(\\(---\\) .+\n\\(\\+\\+\\+\\) .+\\|"
                           diff-hunk-header-re-unified ".*\\)$")
                   nil t)
		  (< (point) end))
	(combine-after-change-calls
	  (if (match-beginning 2)
	      ;; we matched a file header
	      (progn
		;; use reverse order to make sure the indices are kept valid
		(replace-match "---" t t nil 3)
		(replace-match "***" t t nil 2))
	    ;; we matched a hunk header
	    (let ((line1 (match-string 4))
		  (lines1 (or (match-string 5) "1"))
		  (line2 (match-string 6))
		  (lines2 (or (match-string 7) "1"))
		  ;; Variables to use the special undo function.
		  (old-undo buffer-undo-list)
		  (old-end (marker-position end))
		  (start (match-beginning 0))
		  (reversible t))
	      (replace-match
	       (concat "***************\n*** " line1 ","
		       (number-to-string (+ (string-to-number line1)
					    (string-to-number lines1)
					    -1))
		       " ****"))
	      (save-restriction
		(narrow-to-region (line-beginning-position 2)
                                  ;; Call diff-end-of-hunk from just before
                                  ;; the hunk header so it can use the hunk
                                  ;; header info.
				  (progn (diff-end-of-hunk 'unified) (point)))
		(let ((hunk (buffer-string)))
		  (goto-char (point-min))
		  (if (not (save-excursion (re-search-forward "^-" nil t)))
		      (delete-region (point) (point-max))
		    (goto-char (point-max))
		    (let ((modif nil) last-pt)
		      (while (progn (setq last-pt (point))
				    (= (forward-line -1) 0))
			(case (char-after)
			  (?\s (insert " ") (setq modif nil) (backward-char 1))
			  (?+ (delete-region (point) last-pt) (setq modif t))
			  (?- (if (not modif)
				  (progn (forward-char 1)
					 (insert " "))
				(delete-char 1)
				(insert "! "))
			      (backward-char 2))
			  (?\\ (when (save-excursion (forward-line -1)
						     (= (char-after) ?+))
				 (delete-region (point) last-pt) (setq modif t)))
                          ;; diff-valid-unified-empty-line.
                          (?\n (insert "  ") (setq modif nil) (backward-char 2))
			  (t (setq modif nil))))))
		  (goto-char (point-max))
		  (save-excursion
		    (insert "--- " line2 ","
			    (number-to-string (+ (string-to-number line2)
						 (string-to-number lines2)
						 -1))
                            " ----\n" hunk))
		  ;;(goto-char (point-min))
		  (forward-line 1)
		  (if (not (save-excursion (re-search-forward "^+" nil t)))
		      (delete-region (point) (point-max))
		    (let ((modif nil) (delete nil))
		      (if (save-excursion (re-search-forward "^\\+.*\n-" nil t))
                          ;; Normally, lines in a substitution come with
                          ;; first the removals and then the additions, and
                          ;; the context->unified function follows this
                          ;; convention, of course.  Yet, other alternatives
                          ;; are valid as well, but they preclude the use of
                          ;; context->unified as an undo command.
			  (setq reversible nil))
		      (while (not (eobp))
			(case (char-after)
			  (?\s (insert " ") (setq modif nil) (backward-char 1))
			  (?- (setq delete t) (setq modif t))
			  (?+ (if (not modif)
				  (progn (forward-char 1)
					 (insert " "))
				(delete-char 1)
				(insert "! "))
			      (backward-char 2))
			  (?\\ (when (save-excursion (forward-line 1)
						     (not (eobp)))
				 (setq delete t) (setq modif t)))
                          ;; diff-valid-unified-empty-line.
                          (?\n (insert "  ") (setq modif nil) (backward-char 2)
                               (setq reversible nil))
			  (t (setq modif nil)))
			(let ((last-pt (point)))
			  (forward-line 1)
			  (when delete
			    (delete-region last-pt (point))
			    (setq delete nil)))))))
		(unless (or (not reversible) (eq buffer-undo-list t))
                  ;; Drop the many undo entries and replace them with
                  ;; a single entry that uses diff-context->unified to do
                  ;; the work.
		  (setq buffer-undo-list
			(cons (list 'apply (- old-end end) start (point-max)
				    'diff-context->unified start (point-max))
			      old-undo)))))))))))

(defun diff-context->unified (start end &optional to-context)
  "Convert context diffs to unified diffs.
START and END are either taken from the region
\(when it is highlighted) or else cover the whole buffer.
With a prefix argument, convert unified format to context format."
  (interactive (if (and transient-mark-mode mark-active)
		   (list (region-beginning) (region-end) current-prefix-arg)
		 (list (point-min) (point-max) current-prefix-arg)))
  (if to-context
      (diff-unified->context start end)
    (unless (markerp end) (setq end (copy-marker end t)))
    (let ( ;;(diff-inhibit-after-change t)
          (inhibit-read-only t))
      (save-excursion
        (goto-char start)
        (while (and (re-search-forward "^\\(\\(\\*\\*\\*\\) .+\n\\(---\\) .+\\|\\*\\{15\\}.*\n\\*\\*\\* \\([0-9]+\\),\\(-?[0-9]+\\) \\*\\*\\*\\*\\)$" nil t)
                    (< (point) end))
          (combine-after-change-calls
            (if (match-beginning 2)
                ;; we matched a file header
                (progn
                  ;; use reverse order to make sure the indices are kept valid
                  (replace-match "+++" t t nil 3)
                  (replace-match "---" t t nil 2))
              ;; we matched a hunk header
              (let ((line1s (match-string 4))
                    (line1e (match-string 5))
                    (pt1 (match-beginning 0))
                    ;; Variables to use the special undo function.
                    (old-undo buffer-undo-list)
                    (old-end (marker-position end))
                    (reversible t))
                (replace-match "")
                (unless (re-search-forward
                         diff-context-mid-hunk-header-re nil t)
                  (error "Can't find matching `--- n1,n2 ----' line"))
                (let ((line2s (match-string 1))
                      (line2e (match-string 2))
                      (pt2 (progn
                             (delete-region (progn (beginning-of-line) (point))
                                            (progn (forward-line 1) (point)))
                             (point-marker))))
                  (goto-char pt1)
                  (forward-line 1)
                  (while (< (point) pt2)
                    (case (char-after)
                      (?! (delete-char 2) (insert "-") (forward-line 1))
                      (?- (forward-char 1) (delete-char 1) (forward-line 1))
                      (?\s           ;merge with the other half of the chunk
                       (let* ((endline2
                               (save-excursion
                                 (goto-char pt2) (forward-line 1) (point))))
                         (case (char-after pt2)
                           ((?! ?+)
                            (insert "+"
                                    (prog1 (buffer-substring (+ pt2 2) endline2)
                                      (delete-region pt2 endline2))))
                           (?\s
                            (unless (= (- endline2 pt2)
                                       (- (line-beginning-position 2) (point)))
                              ;; If the two lines we're merging don't have the
                              ;; same length (can happen with "diff -b"), then
                              ;; diff-unified->context will not properly undo
                              ;; this operation.
                              (setq reversible nil))
                            (delete-region pt2 endline2)
                            (delete-char 1)
                            (forward-line 1))
                           (?\\ (forward-line 1))
                           (t (setq reversible nil)
                              (delete-char 1) (forward-line 1)))))
                      (t (setq reversible nil) (forward-line 1))))
                  (while (looking-at "[+! ] ")
                    (if (/= (char-after) ?!) (forward-char 1)
                      (delete-char 1) (insert "+"))
                    (delete-char 1) (forward-line 1))
                  (save-excursion
                    (goto-char pt1)
                    (insert "@@ -" line1s ","
                            (number-to-string (- (string-to-number line1e)
                                                 (string-to-number line1s)
                                                 -1))
                            " +" line2s ","
                            (number-to-string (- (string-to-number line2e)
                                                 (string-to-number line2s)
                                                 -1)) " @@"))
                  (set-marker pt2 nil)
                  ;; The whole procedure succeeded, let's replace the myriad
                  ;; of undo elements with just a single special one.
                  (unless (or (not reversible) (eq buffer-undo-list t))
                    (setq buffer-undo-list
                          (cons (list 'apply (- old-end end) pt1 (point)
                                      'diff-unified->context pt1 (point))
                                old-undo)))
                  )))))))))

(defun diff-reverse-direction (start end)
  "Reverse the direction of the diffs.
START and END are either taken from the region (if a prefix arg is given) or
else cover the whole buffer."
  (interactive (if (or current-prefix-arg (and transient-mark-mode mark-active))
		   (list (region-beginning) (region-end))
		 (list (point-min) (point-max))))
  (unless (markerp end) (setq end (copy-marker end t)))
  (let (;;(diff-inhibit-after-change t)
	(inhibit-read-only t))
    (save-excursion
      (goto-char start)
      (while (and (re-search-forward "^\\(\\([-*][-*][-*] \\)\\(.+\\)\n\\([-+][-+][-+] \\)\\(.+\\)\\|\\*\\{15\\}.*\n\\*\\*\\* \\(.+\\) \\*\\*\\*\\*\\|@@ -\\([0-9,]+\\) \\+\\([0-9,]+\\) @@.*\\)$" nil t)
		  (< (point) end))
	(combine-after-change-calls
	  (cond
	   ;; a file header
	   ((match-beginning 2) (replace-match "\\2\\5\n\\4\\3" nil))
	   ;; a context-diff hunk header
	   ((match-beginning 6)
	    (let ((pt-lines1 (match-beginning 6))
		  (lines1 (match-string 6)))
	      (replace-match "" nil nil nil 6)
	      (forward-line 1)
	      (let ((half1s (point)))
		(while (looking-at "[-! \\][ \t]\\|#")
		  (when (= (char-after) ?-) (delete-char 1) (insert "+"))
		  (forward-line 1))
		(let ((half1 (delete-and-extract-region half1s (point))))
		  (unless (looking-at diff-context-mid-hunk-header-re)
		    (insert half1)
		    (error "Can't find matching `--- n1,n2 ----' line"))
		  (let* ((str1end (or (match-end 2) (match-end 1)))
                         (str1 (buffer-substring (match-beginning 1) str1end)))
                    (goto-char str1end)
                    (insert lines1)
                    (delete-region (match-beginning 1) str1end)
		    (forward-line 1)
		    (let ((half2s (point)))
		      (while (looking-at "[!+ \\][ \t]\\|#")
			(when (= (char-after) ?+) (delete-char 1) (insert "-"))
			(forward-line 1))
		      (let ((half2 (delete-and-extract-region half2s (point))))
			(insert (or half1 ""))
			(goto-char half1s)
			(insert (or half2 ""))))
		    (goto-char pt-lines1)
		    (insert str1))))))
	   ;; a unified-diff hunk header
	   ((match-beginning 7)
	    (replace-match "@@ -\\8 +\\7 @@" nil)
	    (forward-line 1)
	    (let ((c (char-after)) first last)
	      (while (case (setq c (char-after))
		       (?- (setq first (or first (point)))
			   (delete-char 1) (insert "+") t)
		       (?+ (setq last (or last (point)))
			   (delete-char 1) (insert "-") t)
		       ((?\\ ?#) t)
		       (t (when (and first last (< first last))
			    (insert (delete-and-extract-region first last)))
			  (setq first nil last nil)
			  (memq c (if diff-valid-unified-empty-line
                                      '(?\s ?\n) '(?\s)))))
		(forward-line 1))))))))))

(defun diff-fixup-modifs (start end)
  "Fixup the hunk headers (in case the buffer was modified).
START and END are either taken from the region (if a prefix arg is given) or
else cover the whole buffer."
  (interactive (if (or current-prefix-arg (and transient-mark-mode mark-active))
		   (list (region-beginning) (region-end))
		 (list (point-min) (point-max))))
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char end) (diff-end-of-hunk nil 'donttrustheader)
      (let ((plus 0) (minus 0) (space 0) (bang 0))
	(while (and (= (forward-line -1) 0) (<= start (point)))
	  (if (not (looking-at
		    (concat diff-hunk-header-re-unified
			    "\\|[-*][-*][-*] [0-9,]+ [-*][-*][-*][-*]$"
			    "\\|--- .+\n\\+\\+\\+ ")))
	      (case (char-after)
		(?\s (incf space))
		(?+ (incf plus))
		(?- (incf minus))
		(?! (incf bang))
		((?\\ ?#) nil)
		(t  (setq space 0 plus 0 minus 0 bang 0)))
	    (cond
	     ((looking-at diff-hunk-header-re-unified)
	      (let* ((old1 (match-string 2))
		     (old2 (match-string 4))
		     (new1 (number-to-string (+ space minus)))
		     (new2 (number-to-string (+ space plus))))
		(if old2
		    (unless (string= new2 old2) (replace-match new2 t t nil 4))
		  (goto-char (match-end 3))
		  (insert "," new2))
		(if old1
		    (unless (string= new1 old1) (replace-match new1 t t nil 2))
		  (goto-char (match-end 1))
		  (insert "," new1))))
	     ((looking-at diff-context-mid-hunk-header-re)
	      (when (> (+ space bang plus) 0)
		(let* ((old1 (match-string 1))
		       (old2 (match-string 2))
		       (new (number-to-string
			     (+ space bang plus -1 (string-to-number old1)))))
		  (unless (string= new old2) (replace-match new t t nil 2)))))
	     ((looking-at "\\*\\*\\* \\([0-9]+\\),\\(-?[0-9]*\\) \\*\\*\\*\\*$")
	      (when (> (+ space bang minus) 0)
		(let* ((old (match-string 1))
		       (new (format
			     (concat "%0" (number-to-string (length old)) "d")
			     (+ space bang minus -1 (string-to-number old)))))
		  (unless (string= new old) (replace-match new t t nil 2))))))
	    (setq space 0 plus 0 minus 0 bang 0)))))))

;;;;
;;;; Hooks
;;;;

(defun diff-write-contents-hooks ()
  "Fixup hunk headers if necessary."
  (if (buffer-modified-p) (diff-fixup-modifs (point-min) (point-max)))
  nil)

;; It turns out that making changes in the buffer from within an
;; *-change-function is asking for trouble, whereas making them
;; from a post-command-hook doesn't pose much problems
(defvar diff-unhandled-changes nil)
(defun diff-after-change-function (beg end _len)
  "Remember to fixup the hunk header.
See `after-change-functions' for the meaning of BEG, END and LEN."
  ;; Ignoring changes when inhibit-read-only is set is strictly speaking
  ;; incorrect, but it turns out that inhibit-read-only is normally not set
  ;; inside editing commands, while it tends to be set when the buffer gets
  ;; updated by an async process or by a conversion function, both of which
  ;; would rather not be uselessly slowed down by this hook.
  (when (and (not undo-in-progress) (not inhibit-read-only))
    (if diff-unhandled-changes
	(setq diff-unhandled-changes
	      (cons (min beg (car diff-unhandled-changes))
		    (max end (cdr diff-unhandled-changes))))
      (setq diff-unhandled-changes (cons beg end)))))

(defun diff-post-command-hook ()
  "Fixup hunk headers if necessary."
  (when (consp diff-unhandled-changes)
    (ignore-errors
      (save-excursion
	(goto-char (car diff-unhandled-changes))
	;; Maybe we've cut the end of the hunk before point.
	(if (and (bolp) (not (bobp))) (backward-char 1))
	;; We used to fixup modifs on all the changes, but it turns out that
	;; it's safer not to do it on big changes, e.g. when yanking a big
	;; diff, or when the user edits the header, since we might then
	;; screw up perfectly correct values.  --Stef
	(diff-beginning-of-hunk)
        (let* ((style (if (looking-at "\\*\\*\\*") 'context))
               (start (line-beginning-position (if (eq style 'context) 3 2)))
               (mid (if (eq style 'context)
                        (save-excursion
                          (re-search-forward diff-context-mid-hunk-header-re
                                             nil t)))))
          (when (and ;; Don't try to fixup changes in the hunk header.
                 (> (car diff-unhandled-changes) start)
                 ;; Don't try to fixup changes in the mid-hunk header either.
                 (or (not mid)
                     (< (cdr diff-unhandled-changes) (match-beginning 0))
                     (> (car diff-unhandled-changes) (match-end 0)))
                 (save-excursion
		(diff-end-of-hunk nil 'donttrustheader)
                   ;; Don't try to fixup changes past the end of the hunk.
                   (>= (point) (cdr diff-unhandled-changes))))
	  (diff-fixup-modifs (point) (cdr diff-unhandled-changes)))))
      (setq diff-unhandled-changes nil))))

(defun diff-next-error (arg reset)
  ;; Select a window that displays the current buffer so that point
  ;; movements are reflected in that window.  Otherwise, the user might
  ;; never see the hunk corresponding to the source she's jumping to.
  (pop-to-buffer (current-buffer))
  (if reset (goto-char (point-min)))
  (diff-hunk-next arg)
  (diff-goto-source))

(defvar whitespace-style)
(defvar whitespace-trailing-regexp)

;;;###autoload
(define-derived-mode diff-mode fundamental-mode "Diff"
  "Major mode for viewing/editing context diffs.
Supports unified and context diffs as well as (to a lesser extent)
normal diffs.

When the buffer is read-only, the ESC prefix is not necessary.
If you edit the buffer manually, diff-mode will try to update the hunk
headers for you on-the-fly.

You can also switch between context diff and unified diff with \\[diff-context->unified],
or vice versa with \\[diff-unified->context] and you can also reverse the direction of
a diff with \\[diff-reverse-direction].

   \\{diff-mode-map}"

  (set (make-local-variable 'font-lock-defaults) diff-font-lock-defaults)
  (set (make-local-variable 'outline-regexp) diff-outline-regexp)
  (set (make-local-variable 'imenu-generic-expression)
       diff-imenu-generic-expression)
  ;; These are not perfect.  They would be better done separately for
  ;; context diffs and unidiffs.
  ;; (set (make-local-variable 'paragraph-start)
  ;;        (concat "@@ "			; unidiff hunk
  ;; 	       "\\|\\*\\*\\* "		; context diff hunk or file start
  ;; 	       "\\|--- [^\t]+\t"))	; context or unidiff file
  ;; 					; start (first or second line)
  ;;   (set (make-local-variable 'paragraph-separate) paragraph-start)
  ;;   (set (make-local-variable 'page-delimiter) "--- [^\t]+\t")
  ;; compile support
  (set (make-local-variable 'next-error-function) 'diff-next-error)

  (set (make-local-variable 'beginning-of-defun-function)
       'diff-beginning-of-file-and-junk)
  (set (make-local-variable 'end-of-defun-function)
       'diff-end-of-file)

  ;; Set up `whitespace-mode' so that turning it on will show trailing
  ;; whitespace problems on the modified lines of the diff.
  (set (make-local-variable 'whitespace-style) '(face trailing))
  (set (make-local-variable 'whitespace-trailing-regexp)
       "^[-\+!<>].*?\\([\t ]+\\)$")

  (setq buffer-read-only diff-default-read-only)
  ;; setup change hooks
  (if (not diff-update-on-the-fly)
      (add-hook 'write-contents-functions 'diff-write-contents-hooks nil t)
    (make-local-variable 'diff-unhandled-changes)
    (add-hook 'after-change-functions 'diff-after-change-function nil t)
    (add-hook 'post-command-hook 'diff-post-command-hook nil t))
  ;; Neat trick from Dave Love to add more bindings in read-only mode:
  (let ((ro-bind (cons 'buffer-read-only diff-mode-shared-map)))
    (add-to-list 'minor-mode-overriding-map-alist ro-bind)
    ;; Turn off this little trick in case the buffer is put in view-mode.
    (add-hook 'view-mode-hook
	      (lambda ()
		(setq minor-mode-overriding-map-alist
		      (delq ro-bind minor-mode-overriding-map-alist)))
	      nil t))
  ;; add-log support
  (set (make-local-variable 'add-log-current-defun-function)
       'diff-current-defun)
  (set (make-local-variable 'add-log-buffer-file-name-function)
       (lambda () (diff-find-file-name nil 'noprompt)))
  (unless (buffer-file-name)
    (hack-dir-local-variables-non-file-buffer)))

;;;###autoload
(define-minor-mode diff-minor-mode
  "Toggle Diff minor mode.
With a prefix argument ARG, enable Diff minor mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

\\{diff-minor-mode-map}"
  :group 'diff-mode :lighter " Diff"
  ;; FIXME: setup font-lock
  ;; setup change hooks
  (if (not diff-update-on-the-fly)
      (add-hook 'write-contents-functions 'diff-write-contents-hooks nil t)
    (make-local-variable 'diff-unhandled-changes)
    (add-hook 'after-change-functions 'diff-after-change-function nil t)
    (add-hook 'post-command-hook 'diff-post-command-hook nil t)))

;;; Handy hook functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun diff-delete-if-empty ()
  ;; An empty diff file means there's no more diffs to integrate, so we
  ;; can just remove the file altogether.  Very handy for .rej files if we
  ;; remove hunks as we apply them.
  (when (and buffer-file-name
	     (eq 0 (nth 7 (file-attributes buffer-file-name))))
    (delete-file buffer-file-name)))

(defun diff-delete-empty-files ()
  "Arrange for empty diff files to be removed."
  (add-hook 'after-save-hook 'diff-delete-if-empty nil t))

(defun diff-make-unified ()
  "Turn context diffs into unified diffs if applicable."
  (if (save-excursion
	(goto-char (point-min))
	(and (looking-at diff-hunk-header-re) (eq (char-after) ?*)))
      (let ((mod (buffer-modified-p)))
	(unwind-protect
	    (diff-context->unified (point-min) (point-max))
	  (restore-buffer-modified-p mod)))))

;;;
;;; Misc operations that have proved useful at some point.
;;;

(defun diff-next-complex-hunk ()
  "Jump to the next \"complex\" hunk.
\"Complex\" is approximated by \"the hunk changes the number of lines\".
Only works for unified diffs."
  (interactive)
  (while
      (and (re-search-forward diff-hunk-header-re-unified nil t)
	   (equal (match-string 2) (match-string 4)))))

(defun diff-sanity-check-context-hunk-half (lines)
  (let ((count lines))
    (while
        (cond
         ((and (memq (char-after) '(?\s ?! ?+ ?-))
               (memq (char-after (1+ (point))) '(?\s ?\t)))
          (decf count) t)
         ((or (zerop count) (= count lines)) nil)
         ((memq (char-after) '(?! ?+ ?-))
          (if (not (and (eq (char-after (1+ (point))) ?\n)
                        (y-or-n-p "Try to auto-fix whitespace loss damage? ")))
              (error "End of hunk ambiguously marked")
            (forward-char 1) (insert " ") (forward-line -1) t))
         ((< lines 0)
          (error "End of hunk ambiguously marked"))
         ((not (y-or-n-p "Try to auto-fix whitespace loss and word-wrap damage? "))
          (error "Abort!"))
         ((eolp) (insert "  ") (forward-line -1) t)
         (t (insert " ") (delete-region (- (point) 2) (- (point) 1)) t))
      (forward-line))))

(defun diff-sanity-check-hunk ()
  (let (;; Every modification is protected by a y-or-n-p, so it's probably
        ;; OK to override a read-only setting.
        (inhibit-read-only t))
    (save-excursion
      (cond
       ((not (looking-at diff-hunk-header-re))
        (error "Not recognizable hunk header"))

       ;; A context diff.
       ((eq (char-after) ?*)
        (if (not (looking-at "\\*\\{15\\}\\(?: .*\\)?\n\\*\\*\\* \\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\*\\*\\*\\*"))
            (error "Unrecognized context diff first hunk header format")
          (forward-line 2)
          (diff-sanity-check-context-hunk-half
	   (if (match-end 2)
	       (1+ (- (string-to-number (match-string 2))
		      (string-to-number (match-string 1))))
	     1))
          (if (not (looking-at diff-context-mid-hunk-header-re))
              (error "Unrecognized context diff second hunk header format")
            (forward-line)
            (diff-sanity-check-context-hunk-half
	     (if (match-end 2)
		 (1+ (- (string-to-number (match-string 2))
			(string-to-number (match-string 1))))
	       1)))))

       ;; A unified diff.
       ((eq (char-after) ?@)
        (if (not (looking-at diff-hunk-header-re-unified))
            (error "Unrecognized unified diff hunk header format")
          (let ((before (string-to-number (or (match-string 2) "1")))
                (after (string-to-number (or (match-string 4) "1"))))
            (forward-line)
            (while
                (case (char-after)
                  (?\s (decf before) (decf after) t)
                  (?-
                   (if (and (looking-at diff-file-header-re)
                            (zerop before) (zerop after))
                       ;; No need to query: this is a case where two patches
                       ;; are concatenated and only counting the lines will
                       ;; give the right result.  Let's just add an empty
                       ;; line so that our code which doesn't count lines
                       ;; will not get confused.
                       (progn (save-excursion (insert "\n")) nil)
                     (decf before) t))
                  (?+ (decf after) t)
                  (t
                   (cond
                    ((and diff-valid-unified-empty-line
                          ;; Not just (eolp) so we don't infloop at eob.
                          (eq (char-after) ?\n)
                          (> before 0) (> after 0))
                     (decf before) (decf after) t)
                    ((and (zerop before) (zerop after)) nil)
                    ((or (< before 0) (< after 0))
                     (error (if (or (zerop before) (zerop after))
                                "End of hunk ambiguously marked"
                              "Hunk seriously messed up")))
                    ((not (y-or-n-p (concat "Try to auto-fix " (if (eolp) "whitespace loss" "word-wrap damage") "? ")))
                     (error "Abort!"))
                    ((eolp) (insert " ") (forward-line -1) t)
                    (t (insert " ")
                       (delete-region (- (point) 2) (- (point) 1)) t))))
              (forward-line)))))

       ;; A plain diff.
       (t
        ;; TODO.
        )))))

(defun diff-hunk-text (hunk destp char-offset)
  "Return the literal source text from HUNK as (TEXT . OFFSET).
If DESTP is nil, TEXT is the source, otherwise the destination text.
CHAR-OFFSET is a char-offset in HUNK, and OFFSET is the corresponding
char-offset in TEXT."
  (with-temp-buffer
    (insert hunk)
    (goto-char (point-min))
    (let ((src-pos nil)
	  (dst-pos nil)
	  (divider-pos nil)
	  (num-pfx-chars 2))
      ;; Set the following variables:
      ;;  SRC-POS     buffer pos of the source part of the hunk or nil if none
      ;;  DST-POS     buffer pos of the destination part of the hunk or nil
      ;;  DIVIDER-POS buffer pos of any divider line separating the src & dst
      ;;  NUM-PFX-CHARS  number of line-prefix characters used by this format"
      (cond ((looking-at "^@@")
	     ;; unified diff
	     (setq num-pfx-chars 1)
	     (forward-line 1)
	     (setq src-pos (point) dst-pos (point)))
	    ((looking-at "^\\*\\*")
	     ;; context diff
	     (forward-line 2)
	     (setq src-pos (point))
	     (re-search-forward diff-context-mid-hunk-header-re nil t)
	     (forward-line 0)
	     (setq divider-pos (point))
	     (forward-line 1)
	     (setq dst-pos (point)))
	    ((looking-at "^[0-9]+a[0-9,]+$")
	     ;; normal diff, insert
	     (forward-line 1)
	     (setq dst-pos (point)))
	    ((looking-at "^[0-9,]+d[0-9]+$")
	     ;; normal diff, delete
	     (forward-line 1)
	     (setq src-pos (point)))
	    ((looking-at "^[0-9,]+c[0-9,]+$")
	     ;; normal diff, change
	     (forward-line 1)
	     (setq src-pos (point))
	     (re-search-forward "^---$" nil t)
	     (forward-line 0)
	     (setq divider-pos (point))
	     (forward-line 1)
	     (setq dst-pos (point)))
	    (t
	     (error "Unknown diff hunk type")))

      (if (if destp (null dst-pos) (null src-pos))
	  ;; Implied empty text
	  (if char-offset '("" . 0) "")

	;; For context diffs, either side can be empty, (if there's only
	;; added or only removed text).  We should then use the other side.
	(cond ((equal src-pos divider-pos) (setq src-pos dst-pos))
	      ((equal dst-pos (point-max)) (setq dst-pos src-pos)))

	(when char-offset (goto-char (+ (point-min) char-offset)))

	;; Get rid of anything except the desired text.
	(save-excursion
	  ;; Delete unused text region
	  (let ((keep (if destp dst-pos src-pos)))
	    (when (and divider-pos (> divider-pos keep))
	      (delete-region divider-pos (point-max)))
	    (delete-region (point-min) keep))
	  ;; Remove line-prefix characters, and unneeded lines (unified diffs).
	  (let ((kill-char (if destp ?- ?+)))
	    (goto-char (point-min))
	    (while (not (eobp))
	      (if (eq (char-after) kill-char)
		  (delete-region (point) (progn (forward-line 1) (point)))
		(delete-char num-pfx-chars)
		(forward-line 1)))))

	(let ((text (buffer-substring-no-properties (point-min) (point-max))))
	  (if char-offset (cons text (- (point) (point-min))) text))))))


(defun diff-find-text (text)
  "Return the buffer position (BEG . END) of the nearest occurrence of TEXT.
If TEXT isn't found, nil is returned."
  (let* ((orig (point))
	 (forw (and (search-forward text nil t)
		    (cons (match-beginning 0) (match-end 0))))
	 (back (and (goto-char (+ orig (length text)))
		    (search-backward text nil t)
		    (cons (match-beginning 0) (match-end 0)))))
    ;; Choose the closest match.
    (if (and forw back)
	(if (> (- (car forw) orig) (- orig (car back))) back forw)
      (or back forw))))

(defun diff-find-approx-text (text)
  "Return the buffer position (BEG . END) of the nearest occurrence of TEXT.
Whitespace differences are ignored."
  (let* ((orig (point))
	 (re (concat "^[ \t\n]*"
		     (mapconcat 'regexp-quote (split-string text) "[ \t\n]+")
		     "[ \t\n]*\n"))
	 (forw (and (re-search-forward re nil t)
		    (cons (match-beginning 0) (match-end 0))))
	 (back (and (goto-char (+ orig (length text)))
		    (re-search-backward re nil t)
		    (cons (match-beginning 0) (match-end 0)))))
    ;; Choose the closest match.
    (if (and forw back)
	(if (> (- (car forw) orig) (- orig (car back))) back forw)
      (or back forw))))

(defsubst diff-xor (a b) (if a (if (not b) a) b))

(defun diff-find-source-location (&optional other-file reverse noprompt)
  "Find out (BUF LINE-OFFSET POS SRC DST SWITCHED).
BUF is the buffer corresponding to the source file.
LINE-OFFSET is the offset between the expected and actual positions
  of the text of the hunk or nil if the text was not found.
POS is a pair (BEG . END) indicating the position of the text in the buffer.
SRC and DST are the two variants of text as returned by `diff-hunk-text'.
  SRC is the variant that was found in the buffer.
SWITCHED is non-nil if the patch is already applied.
NOPROMPT, if non-nil, means not to prompt the user."
  (save-excursion
    (let* ((other (diff-xor other-file diff-jump-to-old-file))
	   (char-offset (- (point) (progn (diff-beginning-of-hunk 'try-harder)
                                          (point))))
           ;; Check that the hunk is well-formed.  Otherwise diff-mode and
           ;; the user may disagree on what constitutes the hunk
           ;; (e.g. because an empty line truncates the hunk mid-course),
           ;; leading to potentially nasty surprises for the user.
	   ;;
	   ;; Suppress check when NOPROMPT is non-nil (Bug#3033).
           (_ (unless noprompt (diff-sanity-check-hunk)))
	   (hunk (buffer-substring
                  (point) (save-excursion (diff-end-of-hunk) (point))))
	   (old (diff-hunk-text hunk reverse char-offset))
	   (new (diff-hunk-text hunk (not reverse) char-offset))
	   ;; Find the location specification.
	   (line (if (not (looking-at "\\(?:\\*\\{15\\}.*\n\\)?[-@* ]*\\([0-9,]+\\)\\([ acd+]+\\([0-9,]+\\)\\)?"))
		     (error "Can't find the hunk header")
		   (if other (match-string 1)
		     (if (match-end 3) (match-string 3)
		       (unless (re-search-forward
                                diff-context-mid-hunk-header-re nil t)
			 (error "Can't find the hunk separator"))
		       (match-string 1)))))
	   (file (or (diff-find-file-name other noprompt)
                     (error "Can't find the file")))
	   (buf (find-file-noselect file)))
      ;; Update the user preference if he so wished.
      (when (> (prefix-numeric-value other-file) 8)
	(setq diff-jump-to-old-file other))
      (with-current-buffer buf
        (goto-char (point-min)) (forward-line (1- (string-to-number line)))
	(let* ((orig-pos (point))
	       (switched nil)
	       ;; FIXME: Check for case where both OLD and NEW are found.
	       (pos (or (diff-find-text (car old))
			(progn (setq switched t) (diff-find-text (car new)))
			(progn (setq switched nil)
			       (condition-case nil
				   (diff-find-approx-text (car old))
				 (invalid-regexp nil)))	;Regex too big.
			(progn (setq switched t)
			       (condition-case nil
				   (diff-find-approx-text (car new))
				 (invalid-regexp nil)))	;Regex too big.
			(progn (setq switched nil) nil))))
	  (nconc
	   (list buf)
	   (if pos
	       (list (count-lines orig-pos (car pos)) pos)
	     (list nil (cons orig-pos (+ orig-pos (length (car old))))))
	   (if switched (list new old t) (list old new))))))))


(defun diff-hunk-status-msg (line-offset reversed dry-run)
  (let ((msg (if dry-run
		 (if reversed "already applied" "not yet applied")
	       (if reversed "undone" "applied"))))
    (message (cond ((null line-offset) "Hunk text not found")
		   ((= line-offset 0) "Hunk %s")
		   ((= line-offset 1) "Hunk %s at offset %d line")
		   (t "Hunk %s at offset %d lines"))
	     msg line-offset)))

(defvar diff-apply-hunk-to-backup-file nil)

(defun diff-apply-hunk (&optional reverse)
  "Apply the current hunk to the source file and go to the next.
By default, the new source file is patched, but if the variable
`diff-jump-to-old-file' is non-nil, then the old source file is
patched instead (some commands, such as `diff-goto-source' can change
the value of this variable when given an appropriate prefix argument).

With a prefix argument, REVERSE the hunk."
  (interactive "P")
  (destructuring-bind (buf line-offset pos old new &optional switched)
      ;; Sometimes we'd like to have the following behavior: if REVERSE go
      ;; to the new file, otherwise go to the old.  But that means that by
      ;; default we use the old file, which is the opposite of the default
      ;; for diff-goto-source, and is thus confusing.  Also when you don't
      ;; know about it it's pretty surprising.
      ;; TODO: make it possible to ask explicitly for this behavior.
      ;;
      ;; This is duplicated in diff-test-hunk.
      (diff-find-source-location nil reverse)
    (cond
     ((null line-offset)
      (error "Can't find the text to patch"))
     ((with-current-buffer buf
        (and buffer-file-name
             (backup-file-name-p buffer-file-name)
             (not diff-apply-hunk-to-backup-file)
             (not (set (make-local-variable 'diff-apply-hunk-to-backup-file)
                       (yes-or-no-p (format "Really apply this hunk to %s? "
                                            (file-name-nondirectory
                                             buffer-file-name)))))))
      (error "%s"
	     (substitute-command-keys
              (format "Use %s\\[diff-apply-hunk] to apply it to the other file"
                      (if (not reverse) "\\[universal-argument] ")))))
     ((and switched
	   ;; A reversed patch was detected, perhaps apply it in reverse.
	   (not (save-window-excursion
		  (pop-to-buffer buf)
		  (goto-char (+ (car pos) (cdr old)))
		  (y-or-n-p
		   (if reverse
		       "Hunk hasn't been applied yet; apply it now? "
		     "Hunk has already been applied; undo it? ")))))
      (message "(Nothing done)"))
     (t
      ;; Apply the hunk
      (with-current-buffer buf
	(goto-char (car pos))
	(delete-region (car pos) (cdr pos))
	(insert (car new)))
      ;; Display BUF in a window
      (set-window-point (display-buffer buf) (+ (car pos) (cdr new)))
      (diff-hunk-status-msg line-offset (diff-xor switched reverse) nil)
      (when diff-advance-after-apply-hunk
	(diff-hunk-next))))))


(defun diff-test-hunk (&optional reverse)
  "See whether it's possible to apply the current hunk.
With a prefix argument, try to REVERSE the hunk."
  (interactive "P")
  (destructuring-bind (buf line-offset pos src _dst &optional switched)
      (diff-find-source-location nil reverse)
    (set-window-point (display-buffer buf) (+ (car pos) (cdr src)))
    (diff-hunk-status-msg line-offset (diff-xor reverse switched) t)))


(defalias 'diff-mouse-goto-source 'diff-goto-source)

(defun diff-goto-source (&optional other-file event)
  "Jump to the corresponding source line.
`diff-jump-to-old-file' (or its opposite if the OTHER-FILE prefix arg
is given) determines whether to jump to the old or the new file.
If the prefix arg is bigger than 8 (for example with \\[universal-argument] \\[universal-argument])
then `diff-jump-to-old-file' is also set, for the next invocations."
  (interactive (list current-prefix-arg last-input-event))
  ;; When pointing at a removal line, we probably want to jump to
  ;; the old location, and else to the new (i.e. as if reverting).
  ;; This is a convenient detail when using smerge-diff.
  (if event (posn-set-point (event-end event)))
  (let ((rev (not (save-excursion (beginning-of-line) (looking-at "[-<]")))))
    (destructuring-bind (buf line-offset pos src _dst &optional switched)
	(diff-find-source-location other-file rev)
      (pop-to-buffer buf)
      (goto-char (+ (car pos) (cdr src)))
      (diff-hunk-status-msg line-offset (diff-xor rev switched) t))))


(defun diff-current-defun ()
  "Find the name of function at point.
For use in `add-log-current-defun-function'."
  ;; Kill change-log-default-name so it gets recomputed each time, since
  ;; each hunk may belong to another file which may belong to another
  ;; directory and hence have a different ChangeLog file.
  (kill-local-variable 'change-log-default-name)
  (save-excursion
    (when (looking-at diff-hunk-header-re)
      (forward-line 1)
      (re-search-forward "^[^ ]" nil t))
    (destructuring-bind (&optional buf _line-offset pos src dst switched)
        ;; Use `noprompt' since this is used in which-func-mode and such.
	(ignore-errors                ;Signals errors in place of prompting.
          (diff-find-source-location nil nil 'noprompt))
      (when buf
        (beginning-of-line)
        (or (when (memq (char-after) '(?< ?-))
              ;; Cursor is pointing at removed text.  This could be a removed
              ;; function, in which case, going to the source buffer will
              ;; not help since the function is now removed.  Instead,
              ;; try to figure out the function name just from the
              ;; code-fragment.
              (let ((old (if switched dst src)))
                (with-temp-buffer
                  (insert (car old))
                  (funcall (buffer-local-value 'major-mode buf))
                  (goto-char (+ (point-min) (cdr old)))
                  (add-log-current-defun))))
            (with-current-buffer buf
              (goto-char (+ (car pos) (cdr src)))
              (add-log-current-defun)))))))

(defun diff-ignore-whitespace-hunk ()
  "Re-diff the current hunk, ignoring whitespace differences."
  (interactive)
  (let* ((char-offset (- (point) (progn (diff-beginning-of-hunk 'try-harder)
                                        (point))))
	 (opts (case (char-after) (?@ "-bu") (?* "-bc") (t "-b")))
	 (line-nb (and (or (looking-at "[^0-9]+\\([0-9]+\\)")
			   (error "Can't find line number"))
		       (string-to-number (match-string 1))))
	 (inhibit-read-only t)
	 (hunk (delete-and-extract-region
		(point) (save-excursion (diff-end-of-hunk) (point))))
	 (lead (make-string (1- line-nb) ?\n)) ;Line nums start at 1.
	 (file1 (make-temp-file "diff1"))
	 (file2 (make-temp-file "diff2"))
	 (coding-system-for-read buffer-file-coding-system)
	 old new)
    (unwind-protect
	(save-excursion
	  (setq old (diff-hunk-text hunk nil char-offset))
	  (setq new (diff-hunk-text hunk t char-offset))
	  (write-region (concat lead (car old)) nil file1 nil 'nomessage)
	  (write-region (concat lead (car new)) nil file2 nil 'nomessage)
	  (with-temp-buffer
	    (let ((status
		   (call-process diff-command nil t nil
				 opts file1 file2)))
	      (case status
		(0 nil)			;Nothing to reformat.
		(1 (goto-char (point-min))
		   ;; Remove the file-header.
		   (when (re-search-forward diff-hunk-header-re nil t)
		     (delete-region (point-min) (match-beginning 0))))
		(t (goto-char (point-max))
		   (unless (bolp) (insert "\n"))
		   (insert hunk)))
	      (setq hunk (buffer-string))
	      (unless (memq status '(0 1))
		(error "Diff returned: %s" status)))))
      ;; Whatever happens, put back some equivalent text: either the new
      ;; one or the original one in case some error happened.
      (insert hunk)
      (delete-file file1)
      (delete-file file2))))

;;; Fine change highlighting.

(defface diff-refine-change
  '((((class color) (min-colors 88) (background light))
     :background "grey85")
    (((class color) (min-colors 88) (background dark))
     :background "grey60")
    (((class color) (background light))
     :background "yellow")
    (((class color) (background dark))
     :background "green")
    (t :weight bold))
  "Face used for char-based changes shown by `diff-refine-hunk'."
  :group 'diff-mode)

(defun diff-refine-preproc ()
  (while (re-search-forward "^[+>]" nil t)
    ;; Remove spurious changes due to the fact that one side of the hunk is
    ;; marked with leading + or > and the other with leading - or <.
    ;; We used to replace all the prefix chars with " " but this only worked
    ;; when we did char-based refinement (or when using
    ;; smerge-refine-weight-hack) since otherwise, the `forward' motion done
    ;; in chopup do not necessarily do the same as the ones in highlight
    ;; since the "_" is not treated the same as " ".
    (replace-match (cdr (assq (char-before) '((?+ . "-") (?> . "<"))))))
  )

(declare-function smerge-refine-subst "smerge-mode"
                  (beg1 end1 beg2 end2 props &optional preproc))

(defun diff-refine-hunk ()
  "Highlight changes of hunk at point at a finer granularity."
  (interactive)
  (require 'smerge-mode)
  (save-excursion
    (diff-beginning-of-hunk 'try-harder)
    (let* ((start (point))
           (style (diff-hunk-style))    ;Skips the hunk header as well.
           (beg (point))
           (props '((diff-mode . fine) (face diff-refine-change)))
           ;; Be careful to go back to `start' so diff-end-of-hunk gets
           ;; to read the hunk header's line info.
           (end (progn (goto-char start) (diff-end-of-hunk) (point))))

      (remove-overlays beg end 'diff-mode 'fine)

      (goto-char beg)
      (case style
        (unified
         (while (re-search-forward "^\\(?:-.*\n\\)+\\(\\)\\(?:\\+.*\n\\)+"
                                   end t)
           (smerge-refine-subst (match-beginning 0) (match-end 1)
                                (match-end 1) (match-end 0)
                                props 'diff-refine-preproc)))
        (context
         (let* ((middle (save-excursion (re-search-forward "^---")))
                (other middle))
           (while (re-search-forward "^\\(?:!.*\n\\)+" middle t)
             (smerge-refine-subst (match-beginning 0) (match-end 0)
                                  (save-excursion
                                    (goto-char other)
                                    (re-search-forward "^\\(?:!.*\n\\)+" end)
                                    (setq other (match-end 0))
                                    (match-beginning 0))
                                  other
                                  props 'diff-refine-preproc))))
        (t ;; Normal diffs.
         (let ((beg1 (1+ (point))))
           (when (re-search-forward "^---.*\n" end t)
             ;; It's a combined add&remove, so there's something to do.
             (smerge-refine-subst beg1 (match-beginning 0)
                                  (match-end 0) end
                                  props 'diff-refine-preproc))))))))


(defun diff-add-change-log-entries-other-window ()
  "Iterate through the current diff and create ChangeLog entries.
I.e. like `add-change-log-entry-other-window' but applied to all hunks."
  (interactive)
  ;; XXX: Currently add-change-log-entry-other-window is only called
  ;; once per hunk.  Some hunks have multiple changes, it would be
  ;; good to call it for each change.
  (save-excursion
    (goto-char (point-min))
    (condition-case nil
        ;; Call add-change-log-entry-other-window for each hunk in
        ;; the diff buffer.
        (while (progn
                 (diff-hunk-next)
                 ;; Move to where the changes are,
                 ;; `add-change-log-entry-other-window' works better in
                 ;; that case.
                 (re-search-forward
                  (concat "\n[!+-<>]"
                          ;; If the hunk is a context hunk with an empty first
                          ;; half, recognize the "--- NNN,MMM ----" line
                          "\\(-- [0-9]+\\(,[0-9]+\\)? ----\n"
                          ;; and skip to the next non-context line.
                          "\\( .*\n\\)*[+]\\)?")
                  nil t))
          (save-excursion
            ;; FIXME: this pops up windows of all the buffers.
            (add-change-log-entry nil nil t nil t)))
      ;; When there's no more hunks, diff-hunk-next signals an error.
      (error nil))))

;; provide the package
(provide 'diff-mode)

;;; Old Change Log from when diff-mode wasn't part of Emacs:
;; Revision 1.11  1999/10/09 23:38:29  monnier
;; (diff-mode-load-hook): dropped.
;; (auto-mode-alist): also catch *.diffs.
;; (diff-find-file-name, diff-mode):  add smarts to find the right file
;;     for *.rej files (that lack any file name indication).
;;
;; Revision 1.10  1999/09/30 15:32:11  monnier
;; added support for "\ No newline at end of file".
;;
;; Revision 1.9  1999/09/15 00:01:13  monnier
;; - added basic `compile' support.
;; - have diff-kill-hunk call diff-kill-file if it's the only hunk.
;; - diff-kill-file now tries to kill the leading garbage as well.
;;
;; Revision 1.8  1999/09/13 21:10:09  monnier
;; - don't use CL in the autoloaded code
;; - accept diffs using -T
;;
;; Revision 1.7  1999/09/05 20:53:03  monnier
;; interface to ediff-patch
;;
;; Revision 1.6  1999/09/01 20:55:13  monnier
;; (ediff=patch-file):  add bindings to call ediff-patch.
;; (diff-find-file-name):  taken out of diff-goto-source.
;; (diff-unified->context, diff-context->unified, diff-reverse-direction,
;;  diff-fixup-modifs):  only use the region if a prefix arg is given.
;;
;; Revision 1.5  1999/08/31 19:18:52  monnier
;; (diff-beginning-of-file, diff-prev-file):  fixed wrong parenthesis.
;;
;; Revision 1.4  1999/08/31 13:01:44  monnier
;; use `combine-after-change-calls' to minimize the slowdown of font-lock.
;;

;;; diff-mode.el ends here
