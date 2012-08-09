;;; whitespace.el --- minor mode to visualize TAB, (HARD) SPACE, NEWLINE

;; Copyright (C) 2000-2012  Free Software Foundation, Inc.

;; Author: Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Maintainer: Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Keywords: data, wp
;; Version: 13.2.2
;; X-URL: http://www.emacswiki.org/cgi-bin/wiki/ViniciusJoseLatorre

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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Introduction
;; ------------
;;
;; This package is a minor mode to visualize blanks (TAB, (HARD) SPACE
;; and NEWLINE).
;;
;; whitespace uses two ways to visualize blanks: faces and display
;; table.
;;
;; * Faces are used to highlight the background with a color.
;;   whitespace uses font-lock to highlight blank characters.
;;
;; * Display table changes the way a character is displayed, that is,
;;   it provides a visual mark for characters, for example, at the end
;;   of line (?\xB6), at SPACEs (?\xB7) and at TABs (?\xBB).
;;
;; The `whitespace-style' variable selects which way blanks are
;; visualized.
;;
;; Note that when whitespace is turned on, whitespace saves the
;; font-lock state, that is, if font-lock is on or off.  And
;; whitespace restores the font-lock state when it is turned off.  So,
;; if whitespace is turned on and font-lock is off, whitespace also
;; turns on the font-lock to highlight blanks, but the font-lock will
;; be turned off when whitespace is turned off.  Thus, turn on
;; font-lock before whitespace is on, if you want that font-lock
;; continues on after whitespace is turned off.
;;
;; When whitespace is on, it takes care of highlighting some special
;; characters over the default mechanism of `nobreak-char-display'
;; (which see) and `show-trailing-whitespace' (which see).
;;
;; The trailing spaces are not highlighted while point is at end of line.
;; Also the spaces at beginning of buffer are not highlighted while point is at
;; beginning of buffer; and the spaces at end of buffer are not highlighted
;; while point is at end of buffer.
;;
;; There are two ways of using whitespace: local and global.
;;
;; * Local whitespace affects only the current buffer.
;;
;; * Global whitespace affects all current and future buffers.  That
;;   is, if you turn on global whitespace and then create a new
;;   buffer, the new buffer will also have whitespace on.  The
;;   `whitespace-global-modes' variable controls which major-mode will
;;   be automagically turned on.
;;
;; You can mix the local and global usage without any conflict.  But
;; local whitespace has priority over global whitespace.  Whitespace
;; mode is active in a buffer if you have enabled it in that buffer or
;; if you have enabled it globally.
;;
;; When global and local whitespace are on:
;;
;; * if local whitespace is turned off, whitespace is turned off for
;;   the current buffer only.
;;
;; * if global whitespace is turned off, whitespace continues on only
;;   in the buffers in which local whitespace is on.
;;
;; To use whitespace, insert in your ~/.emacs:
;;
;;    (require 'whitespace)
;;
;; Or autoload at least one of the commands`whitespace-mode',
;; `whitespace-toggle-options', `global-whitespace-mode' or
;; `global-whitespace-toggle-options'.  For example:
;;
;;    (autoload 'whitespace-mode           "whitespace"
;;      "Toggle whitespace visualization."        t)
;;    (autoload 'whitespace-toggle-options "whitespace"
;;      "Toggle local `whitespace-mode' options." t)
;;
;; whitespace was inspired by:
;;
;;    whitespace.el            Rajesh Vaidheeswarran <rv@gnu.org>
;;	Warn about and clean bogus whitespaces in the file
;;	(inspired the idea to warn and clean some blanks)
;;	This was the original `whitespace.el' which was replaced by
;;	`blank-mode.el'.  And later `blank-mode.el' was renamed to
;;	`whitespace.el'.
;;
;;    show-whitespace-mode.el  Aurelien Tisne <aurelien.tisne@free.fr>
;;       Simple mode to highlight whitespaces
;;       (inspired the idea to use font-lock)
;;
;;    whitespace-mode.el       Lawrence Mitchell <wence@gmx.li>
;;       Major mode for editing Whitespace
;;       (inspired the idea to use display table)
;;
;;    visws.el                 Miles Bader <miles@gnu.org>
;;       Make whitespace visible
;;       (handle display table, his code was modified, but the main
;;       idea was kept)
;;
;;
;; Using whitespace
;; ----------------
;;
;; There is no problem if you mix local and global minor mode usage.
;;
;; * LOCAL whitespace:
;;    + To toggle whitespace options locally, type:
;;
;;         M-x whitespace-toggle-options RET
;;
;;    + To activate whitespace locally, type:
;;
;;         C-u 1 M-x whitespace-mode RET
;;
;;    + To deactivate whitespace locally, type:
;;
;;         C-u 0 M-x whitespace-mode RET
;;
;;    + To toggle whitespace locally, type:
;;
;;         M-x whitespace-mode RET
;;
;; * GLOBAL whitespace:
;;    + To toggle whitespace options globally, type:
;;
;;         M-x global-whitespace-toggle-options RET
;;
;;    + To activate whitespace globally, type:
;;
;;         C-u 1 M-x global-whitespace-mode RET
;;
;;    + To deactivate whitespace globally, type:
;;
;;         C-u 0 M-x global-whitespace-mode RET
;;
;;    + To toggle whitespace globally, type:
;;
;;         M-x global-whitespace-mode RET
;;
;; There are also the following useful commands:
;;
;; `whitespace-newline-mode'
;;    Toggle NEWLINE minor mode visualization ("nl" on modeline).
;;
;; `global-whitespace-newline-mode'
;;    Toggle NEWLINE global minor mode visualization ("NL" on modeline).
;;
;; `whitespace-report'
;;    Report some blank problems in buffer.
;;
;; `whitespace-report-region'
;;    Report some blank problems in a region.
;;
;; `whitespace-cleanup'
;;    Cleanup some blank problems in all buffer or at region.
;;
;; `whitespace-cleanup-region'
;;    Cleanup some blank problems at region.
;;
;; The problems, which are cleaned up, are:
;;
;; 1. empty lines at beginning of buffer.
;; 2. empty lines at end of buffer.
;;    If `whitespace-style' includes the value `empty', remove all
;;    empty lines at beginning and/or end of buffer.
;;
;; 3. 8 or more SPACEs at beginning of line.
;;    If `whitespace-style' includes the value `indentation':
;;    replace 8 or more SPACEs at beginning of line by TABs, if
;;    `indent-tabs-mode' is non-nil; otherwise, replace TABs by
;;    SPACEs.
;;    If `whitespace-style' includes the value `indentation::tab',
;;    replace 8 or more SPACEs at beginning of line by TABs.
;;    If `whitespace-style' includes the value `indentation::space',
;;    replace TABs by SPACEs.
;;
;; 4. SPACEs before TAB.
;;    If `whitespace-style' includes the value `space-before-tab':
;;    replace SPACEs by TABs, if `indent-tabs-mode' is non-nil;
;;    otherwise, replace TABs by SPACEs.
;;    If `whitespace-style' includes the value
;;    `space-before-tab::tab', replace SPACEs by TABs.
;;    If `whitespace-style' includes the value
;;    `space-before-tab::space', replace TABs by SPACEs.
;;
;; 5. SPACEs or TABs at end of line.
;;    If `whitespace-style' includes the value `trailing', remove all
;;    SPACEs or TABs at end of line.
;;
;; 6. 8 or more SPACEs after TAB.
;;    If `whitespace-style' includes the value `space-after-tab':
;;    replace SPACEs by TABs, if `indent-tabs-mode' is non-nil;
;;    otherwise, replace TABs by SPACEs.
;;    If `whitespace-style' includes the value `space-after-tab::tab',
;;    replace SPACEs by TABs.
;;    If `whitespace-style' includes the value
;;    `space-after-tab::space', replace TABs by SPACEs.
;;
;;
;; Hooks
;; -----
;;
;; whitespace has the following hook variables:
;;
;; `whitespace-mode-hook'
;;    It is evaluated always when whitespace is turned on locally.
;;
;; `global-whitespace-mode-hook'
;;    It is evaluated always when whitespace is turned on globally.
;;
;; `whitespace-load-hook'
;;    It is evaluated after whitespace package is loaded.
;;
;;
;; Options
;; -------
;;
;; Below it's shown a brief description of whitespace options, please,
;; see the options declaration in the code for a long documentation.
;;
;; `whitespace-style'		Specify which kind of blank is
;;				visualized.
;;
;; `whitespace-space'		Face used to visualize SPACE.
;;
;; `whitespace-hspace'		Face used to visualize HARD SPACE.
;;
;; `whitespace-tab'		Face used to visualize TAB.
;;
;; `whitespace-newline'		Face used to visualize NEWLINE char
;;				mapping.
;;
;; `whitespace-trailing'	Face used to visualize trailing
;;				blanks.
;;
;; `whitespace-line'		Face used to visualize "long" lines.
;;
;; `whitespace-space-before-tab'	Face used to visualize SPACEs
;;					before TAB.
;;
;; `whitespace-indentation'	Face used to visualize 8 or more
;;				SPACEs at beginning of line.
;;
;; `whitespace-empty'		Face used to visualize empty lines at
;;				beginning and/or end of buffer.
;;
;; `whitespace-space-after-tab'	Face used to visualize 8 or more
;;				SPACEs after TAB.
;;
;; `whitespace-space-regexp'	Specify SPACE characters regexp.
;;
;; `whitespace-hspace-regexp'	Specify HARD SPACE characters regexp.
;;
;; `whitespace-tab-regexp'	Specify TAB characters regexp.
;;
;; `whitespace-trailing-regexp'	Specify trailing characters regexp.
;;
;; `whitespace-space-before-tab-regexp'	Specify SPACEs before TAB
;;					regexp.
;;
;; `whitespace-indentation-regexp'	Specify regexp for 8 or more
;;					SPACEs at beginning of line.
;;
;; `whitespace-empty-at-bob-regexp'	Specify regexp for empty lines
;;					at beginning of buffer.
;;
;; `whitespace-empty-at-eob-regexp'	Specify regexp for empty lines
;;					at end of buffer.
;;
;; `whitespace-space-after-tab-regexp'	Specify regexp for 8 or more
;;					SPACEs after TAB.
;;
;; `whitespace-line-column'	Specify column beyond which the line
;;				is highlighted.
;;
;; `whitespace-display-mappings'	Specify an alist of mappings
;;					for displaying characters.
;;
;; `whitespace-global-modes'	Modes for which global
;;				`whitespace-mode' is automagically
;;				turned on.
;;
;; `whitespace-action'		Specify which action is taken when a
;;				buffer is visited or written.
;;
;;
;; Acknowledgements
;; ----------------
;;
;; Thanks to felix (EmacsWiki) for keeping highlight when switching between
;; major modes on a file.
;;
;; Thanks to David Reitter <david.reitter@gmail.com> for suggesting a
;; `whitespace-newline' initialization with low contrast relative to
;; the background color.
;;
;; Thanks to Stephen Deasey <sdeasey@gmail.com> for the
;; `indent-tabs-mode' usage suggestion.
;;
;; Thanks to Eric Cooper <ecc@cmu.edu> for the suggestion to have hook
;; actions when buffer is written as the original whitespace package
;; had.
;;
;; Thanks to nschum (EmacsWiki) for the idea about highlight "long"
;; lines tail.  See EightyColumnRule (EmacsWiki).
;;
;; Thanks to Juri Linkov <juri@jurta.org> for suggesting:
;;    * `define-minor-mode'.
;;    * `global-whitespace-*' name for global commands.
;;
;; Thanks to Robert J. Chassell <bob@gnu.org> for doc fix and testing.
;;
;; Thanks to Drew Adams <drew.adams@oracle.com> for toggle commands
;; suggestion.
;;
;; Thanks to Antti Kaihola <antti.kaihola@linux-aktivaattori.org> for
;; helping to fix `find-file-hooks' reference.
;;
;; Thanks to Andreas Roehler <andreas.roehler@easy-emacs.de> for
;; indicating defface byte-compilation warnings.
;;
;; Thanks to TimOCallaghan (EmacsWiki) for the idea about highlight
;; "long" lines.  See EightyColumnRule (EmacsWiki).
;;
;; Thanks to Yanghui Bian <yanghuibian@gmail.com> for indicating a new
;; NEWLINE character mapping.
;;
;; Thanks to Pete Forman <pete.forman@westgeo.com> for indicating
;; whitespace-mode.el on XEmacs.
;;
;; Thanks to Miles Bader <miles@gnu.org> for handling display table via
;; visws.el (his code was modified, but the main idea was kept).
;;
;; Thanks to:
;;    Rajesh Vaidheeswarran <rv@gnu.org>	(original) whitespace.el
;;    Aurelien Tisne <aurelien.tisne@free.fr>	show-whitespace-mode.el
;;    Lawrence Mitchell <wence@gmx.li>		whitespace-mode.el
;;    Miles Bader <miles@gnu.org>		visws.el
;; And to all people who contributed with them.
;;
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; User Variables:


;;; Interface to the command system


(defgroup whitespace nil
  "Visualize blanks (TAB, (HARD) SPACE and NEWLINE)."
  :link '(emacs-library-link :tag "Source Lisp File" "whitespace.el")
  :version "23.1"
  :group 'convenience)


(defcustom whitespace-style
  '(face
    tabs spaces trailing lines space-before-tab newline
    indentation empty space-after-tab
    space-mark tab-mark newline-mark)
  "Specify which kind of blank is visualized.

It's a list containing some or all of the following values:

   face			enable all visualization via faces (see below).

   trailing		trailing blanks are visualized via faces.
			It has effect only if `face' (see above)
			is present in `whitespace-style'.

   tabs			TABs are visualized via faces.
			It has effect only if `face' (see above)
			is present in `whitespace-style'.

   spaces		SPACEs and HARD SPACEs are visualized via
			faces.
			It has effect only if `face' (see above)
			is present in `whitespace-style'.

   lines		lines which have columns beyond
			`whitespace-line-column' are highlighted via
			faces.
			Whole line is highlighted.
			It has precedence over `lines-tail' (see
			below).
			It has effect only if `face' (see above)
			is present in `whitespace-style'.

   lines-tail		lines which have columns beyond
			`whitespace-line-column' are highlighted via
			faces.
			But only the part of line which goes
			beyond `whitespace-line-column' column.
			It has effect only if `lines' (see above)
			is not present in `whitespace-style'
			and if `face' (see above) is present in
			`whitespace-style'.

   newline		NEWLINEs are visualized via faces.
			It has effect only if `face' (see above)
			is present in `whitespace-style'.

   empty		empty lines at beginning and/or end of buffer
			are visualized via faces.
			It has effect only if `face' (see above)
			is present in `whitespace-style'.

   indentation::tab	8 or more SPACEs at beginning of line are
			visualized via faces.
			It has effect only if `face' (see above)
			is present in `whitespace-style'.

   indentation::space	TABs at beginning of line are visualized via
			faces.
			It has effect only if `face' (see above)
			is present in `whitespace-style'.

   indentation		8 or more SPACEs at beginning of line are
			visualized, if `indent-tabs-mode' (which see)
			is non-nil; otherwise, TABs at beginning of
			line are visualized via faces.
			It has effect only if `face' (see above)
			is present in `whitespace-style'.

   space-after-tab::tab		8 or more SPACEs after a TAB are
				visualized via faces.
				It has effect only if `face' (see above)
				is present in `whitespace-style'.

   space-after-tab::space	TABs are visualized when 8 or more
				SPACEs occur after a TAB, via faces.
				It has effect only if `face' (see above)
				is present in `whitespace-style'.

   space-after-tab		8 or more SPACEs after a TAB are
				visualized, if `indent-tabs-mode'
				(which see) is non-nil; otherwise,
				the TABs are visualized via faces.
				It has effect only if `face' (see above)
				is present in `whitespace-style'.

   space-before-tab::tab	SPACEs before TAB are visualized via
				faces.
				It has effect only if `face' (see above)
				is present in `whitespace-style'.

   space-before-tab::space	TABs are visualized when SPACEs occur
				before TAB, via faces.
				It has effect only if `face' (see above)
				is present in `whitespace-style'.

   space-before-tab		SPACEs before TAB are visualized, if
				`indent-tabs-mode' (which see) is
				non-nil; otherwise, the TABs are
				visualized via faces.
				It has effect only if `face' (see above)
				is present in `whitespace-style'.

   space-mark		SPACEs and HARD SPACEs are visualized via
			display table.

   tab-mark		TABs are visualized via display table.

   newline-mark		NEWLINEs are visualized via display table.

Any other value is ignored.

If nil, don't visualize TABs, (HARD) SPACEs and NEWLINEs via faces and
via display table.

There is an evaluation order for some values, if they are
included in `whitespace-style' list.  For example, if
indentation, indentation::tab and/or indentation::space are
included in `whitespace-style' list.  The evaluation order for
these values is:

 * For indentation:
   1. indentation
   2. indentation::tab
   3. indentation::space

 * For SPACEs after TABs:
   1. space-after-tab
   2. space-after-tab::tab
   3. space-after-tab::space

 * For SPACEs before TABs:
   1. space-before-tab
   2. space-before-tab::tab
   3. space-before-tab::space

So, for example, if indentation and indentation::space are
included in `whitespace-style' list, the indentation value is
evaluated instead of indentation::space value.

One reason for not visualize spaces via faces (if `face' is not
included in `whitespace-style') is to use exclusively for
cleaning up a buffer.  See `whitespace-cleanup' and
`whitespace-cleanup-region' for documentation.

See also `whitespace-display-mappings' for documentation."
  :type '(repeat :tag "Kind of Blank"
		 (choice :tag "Kind of Blank Face"
			 (const :tag "(Face) Face visualization"
				face)
			 (const :tag "(Face) Trailing TABs, SPACEs and HARD SPACEs"
				trailing)
			 (const :tag "(Face) SPACEs and HARD SPACEs"
				spaces)
			 (const :tag "(Face) TABs" tabs)
			 (const :tag "(Face) Lines" lines)
			 (const :tag "(Face) SPACEs before TAB"
				space-before-tab)
			 (const :tag "(Face) NEWLINEs" newline)
			 (const :tag "(Face) Indentation SPACEs"
				indentation)
			 (const :tag "(Face) Empty Lines At BOB And/Or EOB"
				empty)
			 (const :tag "(Face) SPACEs after TAB"
				space-after-tab)
			 (const :tag "(Mark) SPACEs and HARD SPACEs"
				space-mark)
			 (const :tag "(Mark) TABs" tab-mark)
			 (const :tag "(Mark) NEWLINEs" newline-mark)))
  :group 'whitespace)


(defcustom whitespace-space 'whitespace-space
  "Symbol face used to visualize SPACE.

Used when `whitespace-style' includes the value `spaces'."
  :type 'face
  :group 'whitespace)


(defface whitespace-space
  '((((class color) (background dark))
     (:background "grey20"      :foreground "darkgray"))
    (((class color) (background light))
     (:background "LightYellow" :foreground "lightgray"))
    (t (:inverse-video t)))
  "Face used to visualize SPACE."
  :group 'whitespace)


(defcustom whitespace-hspace 'whitespace-hspace
  "Symbol face used to visualize HARD SPACE.

Used when `whitespace-style' includes the value `spaces'."
  :type 'face
  :group 'whitespace)


(defface whitespace-hspace		; 'nobreak-space
  '((((class color) (background dark))
     (:background "grey24"        :foreground "darkgray"))
    (((class color) (background light))
     (:background "LemonChiffon3" :foreground "lightgray"))
    (t (:inverse-video t)))
  "Face used to visualize HARD SPACE."
  :group 'whitespace)


(defcustom whitespace-tab 'whitespace-tab
  "Symbol face used to visualize TAB.

Used when `whitespace-style' includes the value `tabs'."
  :type 'face
  :group 'whitespace)


(defface whitespace-tab
  '((((class color) (background dark))
     (:background "grey22" :foreground "darkgray"))
    (((class color) (background light))
     (:background "beige"  :foreground "lightgray"))
    (t (:inverse-video t)))
  "Face used to visualize TAB."
  :group 'whitespace)


(defcustom whitespace-newline 'whitespace-newline
  "Symbol face used to visualize NEWLINE char mapping.

See `whitespace-display-mappings'.

Used when `whitespace-style' includes the values `newline-mark'
and `newline'."
  :type 'face
  :group 'whitespace)


(defface whitespace-newline
  '((((class color) (background dark))
     (:foreground "darkgray" :bold nil))
    (((class color) (min-colors 88) (background light))
     (:foreground "lightgray" :bold nil))
    ;; Displays with 16 colors use lightgray as background, so using a
    ;; lightgray foreground makes the newline mark invisible.
    (((class color) (background light))
     (:foreground "brown" :bold nil))
    (t (:underline t :bold nil)))
  "Face used to visualize NEWLINE char mapping.

See `whitespace-display-mappings'."
  :group 'whitespace)


(defcustom whitespace-trailing 'whitespace-trailing
  "Symbol face used to visualize trailing blanks.

Used when `whitespace-style' includes the value `trailing'."
  :type 'face
  :group 'whitespace)


(defface whitespace-trailing		; 'trailing-whitespace
  '((((class mono)) (:inverse-video t :bold t :underline t))
    (t (:background "red1" :foreground "yellow" :bold t)))
  "Face used to visualize trailing blanks."
  :group 'whitespace)


(defcustom whitespace-line 'whitespace-line
  "Symbol face used to visualize \"long\" lines.

See `whitespace-line-column'.

Used when `whitespace-style' includes the value `line'."
  :type 'face
  :group 'whitespace)


(defface whitespace-line
  '((((class mono)) (:inverse-video t :bold t :underline t))
    (t (:background "gray20" :foreground "violet")))
  "Face used to visualize \"long\" lines.

See `whitespace-line-column'."
  :group 'whitespace)


(defcustom whitespace-space-before-tab 'whitespace-space-before-tab
  "Symbol face used to visualize SPACEs before TAB.

Used when `whitespace-style' includes the value `space-before-tab'."
  :type 'face
  :group 'whitespace)


(defface whitespace-space-before-tab
  '((((class mono)) (:inverse-video t :bold t :underline t))
    (t (:background "DarkOrange" :foreground "firebrick")))
  "Face used to visualize SPACEs before TAB."
  :group 'whitespace)


(defcustom whitespace-indentation 'whitespace-indentation
  "Symbol face used to visualize 8 or more SPACEs at beginning of line.

Used when `whitespace-style' includes the value `indentation'."
  :type 'face
  :group 'whitespace)


(defface whitespace-indentation
  '((((class mono)) (:inverse-video t :bold t :underline t))
    (t (:background "yellow" :foreground "firebrick")))
  "Face used to visualize 8 or more SPACEs at beginning of line."
  :group 'whitespace)


(defcustom whitespace-empty 'whitespace-empty
  "Symbol face used to visualize empty lines at beginning and/or end of buffer.

Used when `whitespace-style' includes the value `empty'."
  :type 'face
  :group 'whitespace)


(defface whitespace-empty
  '((((class mono)) (:inverse-video t :bold t :underline t))
    (t (:background "yellow" :foreground "firebrick")))
  "Face used to visualize empty lines at beginning and/or end of buffer."
  :group 'whitespace)


(defcustom whitespace-space-after-tab 'whitespace-space-after-tab
  "Symbol face used to visualize 8 or more SPACEs after TAB.

Used when `whitespace-style' includes the value `space-after-tab'."
  :type 'face
  :group 'whitespace)


(defface whitespace-space-after-tab
  '((((class mono)) (:inverse-video t :bold t :underline t))
    (t (:background "yellow" :foreground "firebrick")))
  "Face used to visualize 8 or more SPACEs after TAB."
  :group 'whitespace)


(defcustom whitespace-hspace-regexp
  "\\(\\(\xA0\\|\x8A0\\|\x920\\|\xE20\\|\xF20\\)+\\)"
  "Specify HARD SPACE characters regexp.

If you're using `mule' package, there may be other characters besides:

   \"\\xA0\"   \"\\x8A0\"   \"\\x920\"   \"\\xE20\"   \"\\xF20\"

that should be considered HARD SPACE.

Here are some examples:

   \"\\\\(^\\xA0+\\\\)\"		\
visualize only leading HARD SPACEs.
   \"\\\\(\\xA0+$\\\\)\"		\
visualize only trailing HARD SPACEs.
   \"\\\\(^\\xA0+\\\\|\\xA0+$\\\\)\"	\
visualize leading and/or trailing HARD SPACEs.
   \"\\t\\\\(\\xA0+\\\\)\\t\"		\
visualize only HARD SPACEs between TABs.

NOTE: Enclose always by \\\\( and \\\\) the elements to highlight.
      Use exactly one pair of enclosing \\\\( and \\\\).

Used when `whitespace-style' includes `spaces'."
  :type '(regexp :tag "HARD SPACE Chars")
  :group 'whitespace)


(defcustom whitespace-space-regexp "\\( +\\)"
  "Specify SPACE characters regexp.

If you're using `mule' package, there may be other characters
besides \" \" that should be considered SPACE.

Here are some examples:

   \"\\\\(^ +\\\\)\"		visualize only leading SPACEs.
   \"\\\\( +$\\\\)\"		visualize only trailing SPACEs.
   \"\\\\(^ +\\\\| +$\\\\)\"	\
visualize leading and/or trailing SPACEs.
   \"\\t\\\\( +\\\\)\\t\"	visualize only SPACEs between TABs.

NOTE: Enclose always by \\\\( and \\\\) the elements to highlight.
      Use exactly one pair of enclosing \\\\( and \\\\).

Used when `whitespace-style' includes `spaces'."
  :type '(regexp :tag "SPACE Chars")
  :group 'whitespace)


(defcustom whitespace-tab-regexp "\\(\t+\\)"
  "Specify TAB characters regexp.

If you're using `mule' package, there may be other characters
besides \"\\t\" that should be considered TAB.

Here are some examples:

   \"\\\\(^\\t+\\\\)\"		visualize only leading TABs.
   \"\\\\(\\t+$\\\\)\"		visualize only trailing TABs.
   \"\\\\(^\\t+\\\\|\\t+$\\\\)\"	\
visualize leading and/or trailing TABs.
   \" \\\\(\\t+\\\\) \"	visualize only TABs between SPACEs.

NOTE: Enclose always by \\\\( and \\\\) the elements to highlight.
      Use exactly one pair of enclosing \\\\( and \\\\).

Used when `whitespace-style' includes `tabs'."
  :type '(regexp :tag "TAB Chars")
  :group 'whitespace)


(defcustom whitespace-trailing-regexp
  "\\([\t \u00A0]+\\)$"
  "Specify trailing characters regexp.

If you're using `mule' package, there may be other characters besides:

   \" \"  \"\\t\"  \"\\u00A0\"

that should be considered blank.

NOTE: Enclose always by \"\\\\(\" and \"\\\\)$\" the elements to highlight.
      Use exactly one pair of enclosing elements above.

Used when `whitespace-style' includes `trailing'."
  :type '(regexp :tag "Trailing Chars")
  :group 'whitespace)


(defcustom whitespace-space-before-tab-regexp "\\( +\\)\\(\t+\\)"
  "Specify SPACEs before TAB regexp.

If you're using `mule' package, there may be other characters besides:

   \" \"  \"\\t\"  \"\\xA0\"  \"\\x8A0\"  \"\\x920\"  \"\\xE20\"  \
\"\\xF20\"

that should be considered blank.

Used when `whitespace-style' includes `space-before-tab',
`space-before-tab::tab' or  `space-before-tab::space'."
  :type '(regexp :tag "SPACEs Before TAB")
  :group 'whitespace)


(defcustom whitespace-indentation-regexp
  '("^\t*\\(\\( \\{%d\\}\\)+\\)[^\n\t]"
    . "^ *\\(\t+\\)[^\n]")
  "Specify regexp for 8 or more SPACEs at beginning of line.

It is a cons where the cons car is used for SPACEs visualization
and the cons cdr is used for TABs visualization.

If you're using `mule' package, there may be other characters besides:

   \" \"  \"\\t\"  \"\\xA0\"  \"\\x8A0\"  \"\\x920\"  \"\\xE20\"  \
\"\\xF20\"

that should be considered blank.

Used when `whitespace-style' includes `indentation',
`indentation::tab' or  `indentation::space'."
  :type '(cons (regexp :tag "Indentation SPACEs")
	       (regexp :tag "Indentation TABs"))
  :group 'whitespace)


(defcustom whitespace-empty-at-bob-regexp "^\\(\\([ \t]*\n\\)+\\)"
  "Specify regexp for empty lines at beginning of buffer.

If you're using `mule' package, there may be other characters besides:

   \" \"  \"\\t\"  \"\\xA0\"  \"\\x8A0\"  \"\\x920\"  \"\\xE20\"  \
\"\\xF20\"

that should be considered blank.

Used when `whitespace-style' includes `empty'."
  :type '(regexp :tag "Empty Lines At Beginning Of Buffer")
  :group 'whitespace)


(defcustom whitespace-empty-at-eob-regexp "^\\([ \t\n]+\\)"
  "Specify regexp for empty lines at end of buffer.

If you're using `mule' package, there may be other characters besides:

   \" \"  \"\\t\"  \"\\xA0\"  \"\\x8A0\"  \"\\x920\"  \"\\xE20\"  \
\"\\xF20\"

that should be considered blank.

Used when `whitespace-style' includes `empty'."
  :type '(regexp :tag "Empty Lines At End Of Buffer")
  :group 'whitespace)


(defcustom whitespace-space-after-tab-regexp
  '("\t+\\(\\( \\{%d\\}\\)+\\)"
    . "\\(\t+\\) +")
  "Specify regexp for 8 or more SPACEs after TAB.

It is a cons where the cons car is used for SPACEs visualization
and the cons cdr is used for TABs visualization.

If you're using `mule' package, there may be other characters besides:

   \" \"  \"\\t\"  \"\\xA0\"  \"\\x8A0\"  \"\\x920\"  \"\\xE20\"  \
\"\\xF20\"

that should be considered blank.

Used when `whitespace-style' includes `space-after-tab',
`space-after-tab::tab' or `space-after-tab::space'."
  :type '(regexp :tag "SPACEs After TAB")
  :group 'whitespace)


(defcustom whitespace-line-column 80
  "Specify column beyond which the line is highlighted.

It must be an integer or nil.  If nil, the `fill-column' variable value is
used.

Used when `whitespace-style' includes `lines' or `lines-tail'."
  :type '(choice :tag "Line Length Limit"
		 (integer :tag "Line Length")
		 (const :tag "Use fill-column" nil))
  :group 'whitespace)


;; Hacked from `visible-whitespace-mappings' in visws.el
(defcustom whitespace-display-mappings
  '(
    (space-mark   ?\     [?\u00B7]     [?.])		; space - centered dot
    (space-mark   ?\xA0  [?\u00A4]     [?_])		; hard space - currency
    (space-mark   ?\x8A0 [?\x8A4]      [?_])		; hard space - currency
    (space-mark   ?\x920 [?\x924]      [?_])		; hard space - currency
    (space-mark   ?\xE20 [?\xE24]      [?_])		; hard space - currency
    (space-mark   ?\xF20 [?\xF24]      [?_])		; hard space - currency
    ;; NEWLINE is displayed using the face `whitespace-newline'
    (newline-mark ?\n    [?$ ?\n])			; eol - dollar sign
    ;; (newline-mark ?\n    [?\u21B5 ?\n] [?$ ?\n])	; eol - downwards arrow
    ;; (newline-mark ?\n    [?\u00B6 ?\n] [?$ ?\n])	; eol - pilcrow
    ;; (newline-mark ?\n    [?\x8AF ?\n]  [?$ ?\n])	; eol - overscore
    ;; (newline-mark ?\n    [?\x8AC ?\n]  [?$ ?\n])	; eol - negation
    ;; (newline-mark ?\n    [?\x8B0 ?\n]  [?$ ?\n])	; eol - grade
    ;;
    ;; WARNING: the mapping below has a problem.
    ;; When a TAB occupies exactly one column, it will display the
    ;; character ?\xBB at that column followed by a TAB which goes to
    ;; the next TAB column.
    ;; If this is a problem for you, please, comment the line below.
    (tab-mark     ?\t    [?\u00BB ?\t] [?\\ ?\t])	; tab - left quote mark
    )
  "Specify an alist of mappings for displaying characters.

Each element has the following form:

   (KIND CHAR VECTOR...)

Where:

KIND	is the kind of character.
	It can be one of the following symbols:

	tab-mark	for TAB character

	space-mark	for SPACE or HARD SPACE character

	newline-mark	for NEWLINE character

CHAR	is the character to be mapped.

VECTOR	is a vector of characters to be displayed in place of CHAR.
	The first display vector that can be displayed is used;
	if no display vector for a mapping can be displayed, then
	that character is displayed unmodified.

The NEWLINE character is displayed using the face given by
`whitespace-newline' variable.

Used when `whitespace-style' includes `tab-mark', `space-mark' or
`newline-mark'."
  :type '(repeat
	  (list :tag "Character Mapping"
		(choice :tag "Char Kind"
			(const :tag "Tab" tab-mark)
			(const :tag "Space" space-mark)
			(const :tag "Newline" newline-mark))
		(character :tag "Char")
		(repeat :inline t :tag "Vector List"
			(vector :tag ""
				(repeat :inline t
					:tag "Vector Characters"
					(character :tag "Char"))))))
  :group 'whitespace)


(defcustom whitespace-global-modes t
  "Modes for which global `whitespace-mode' is automagically turned on.

Global `whitespace-mode' is controlled by the command
`global-whitespace-mode'.

If nil, means no modes have `whitespace-mode' automatically
turned on.

If t, all modes that support `whitespace-mode' have it
automatically turned on.

Else it should be a list of `major-mode' symbol names for which
`whitespace-mode' should be automatically turned on.  The sense
of the list is negated if it begins with `not'.  For example:

   (c-mode c++-mode)

means that `whitespace-mode' is turned on for buffers in C and
C++ modes only."
  :type '(choice :tag "Global Modes"
		 (const :tag "None" nil)
		 (const :tag "All" t)
		 (set :menu-tag "Mode Specific" :tag "Modes"
		      :value (not)
		      (const :tag "Except" not)
		      (repeat :inline t
			      (symbol :tag "Mode"))))
  :group 'whitespace)


(defcustom whitespace-action nil
  "Specify which action is taken when a buffer is visited or written.

It's a list containing some or all of the following values:

   nil			no action is taken.

   cleanup		cleanup any bogus whitespace always when local
			whitespace is turned on.
			See `whitespace-cleanup' and
			`whitespace-cleanup-region'.

   report-on-bogus	report if there is any bogus whitespace always
			when local whitespace is turned on.

   auto-cleanup		cleanup any bogus whitespace when buffer is
			written.
			See `whitespace-cleanup' and
			`whitespace-cleanup-region'.

   abort-on-bogus	abort if there is any bogus whitespace and the
			buffer is written.

   warn-if-read-only	give a warning if `cleanup' or `auto-cleanup'
			is included in `whitespace-action' and the
			buffer is read-only.

Any other value is treated as nil."
  :type '(choice :tag "Actions"
		 (const :tag "None" nil)
		 (repeat :tag "Action List"
		  (choice :tag "Action"
			  (const :tag "Cleanup When On" cleanup)
			  (const :tag "Report On Bogus" report-on-bogus)
			  (const :tag "Auto Cleanup" auto-cleanup)
			  (const :tag "Abort On Bogus" abort-on-bogus)
			  (const :tag "Warn If Read-Only" warn-if-read-only))))
  :group 'whitespace)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; User commands - Local mode


;;;###autoload
(define-minor-mode whitespace-mode
  "Toggle whitespace visualization (Whitespace mode).
With a prefix argument ARG, enable Whitespace mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

See also `whitespace-style', `whitespace-newline' and
`whitespace-display-mappings'."
  :lighter    " ws"
  :init-value nil
  :global     nil
  :group      'whitespace
  (cond
   (noninteractive			; running a batch job
    (setq whitespace-mode nil))
   (whitespace-mode			; whitespace-mode on
    (whitespace-turn-on)
    (whitespace-action-when-on))
   (t					; whitespace-mode off
    (whitespace-turn-off))))


;;;###autoload
(define-minor-mode whitespace-newline-mode
  "Toggle newline visualization (Whitespace Newline mode).
With a prefix argument ARG, enable Whitespace Newline mode if ARG
is positive, and disable it otherwise.  If called from Lisp,
enable the mode if ARG is omitted or nil.

Use `whitespace-newline-mode' only for NEWLINE visualization
exclusively.  For other visualizations, including NEWLINE
visualization together with (HARD) SPACEs and/or TABs, please,
use `whitespace-mode'.

See also `whitespace-newline' and `whitespace-display-mappings'."
  :lighter    " nl"
  :init-value nil
  :global     nil
  :group      'whitespace
  (let ((whitespace-style '(face newline-mark newline)))
    (whitespace-mode (if whitespace-newline-mode
			 1 -1)))
  ;; sync states (running a batch job)
  (setq whitespace-newline-mode whitespace-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; User commands - Global mode


;;;###autoload
(define-minor-mode global-whitespace-mode
  "Toggle whitespace visualization globally (Global Whitespace mode).
With a prefix argument ARG, enable Global Whitespace mode if ARG
is positive, and disable it otherwise.  If called from Lisp,
enable it if ARG is omitted or nil.

See also `whitespace-style', `whitespace-newline' and
`whitespace-display-mappings'."
  :lighter    " WS"
  :init-value nil
  :global     t
  :group      'whitespace
  (cond
   (noninteractive			; running a batch job
    (setq global-whitespace-mode nil))
   (global-whitespace-mode		; global-whitespace-mode on
    (save-current-buffer
      (add-hook 'find-file-hook 'whitespace-turn-on-if-enabled)
      (add-hook 'after-change-major-mode-hook 'whitespace-turn-on-if-enabled)
      (dolist (buffer (buffer-list))	; adjust all local mode
	(set-buffer buffer)
	(unless whitespace-mode
	  (whitespace-turn-on-if-enabled)))))
   (t					; global-whitespace-mode off
    (save-current-buffer
      (remove-hook 'find-file-hook 'whitespace-turn-on-if-enabled)
      (remove-hook 'after-change-major-mode-hook 'whitespace-turn-on-if-enabled)
      (dolist (buffer (buffer-list))	; adjust all local mode
	(set-buffer buffer)
	(unless whitespace-mode
	  (whitespace-turn-off)))))))


(defun whitespace-turn-on-if-enabled ()
  (when (cond
	 ((eq whitespace-global-modes t))
	 ((listp whitespace-global-modes)
	  (if (eq (car-safe whitespace-global-modes) 'not)
	      (not (memq major-mode (cdr whitespace-global-modes)))
	    (memq major-mode whitespace-global-modes)))
	 (t nil))
    (let (inhibit-quit)
      ;; Don't turn on whitespace mode if...
      (or
       ;; ...we don't have a display (we're running a batch job)
       noninteractive
       ;; ...or if the buffer is invisible (name starts with a space)
       (eq (aref (buffer-name) 0) ?\ )
       ;; ...or if the buffer is temporary (name starts with *)
       (and (eq (aref (buffer-name) 0) ?*)
	    ;; except the scratch buffer.
	    (not (string= (buffer-name) "*scratch*")))
       ;; Otherwise, turn on whitespace mode.
       (whitespace-turn-on)))))


;;;###autoload
(define-minor-mode global-whitespace-newline-mode
  "Toggle global newline visualization (Global Whitespace Newline mode).
With a prefix argument ARG, enable Global Whitespace Newline mode
if ARG is positive, and disable it otherwise.  If called from
Lisp, enable it if ARG is omitted or nil.

Use `global-whitespace-newline-mode' only for NEWLINE
visualization exclusively.  For other visualizations, including
NEWLINE visualization together with (HARD) SPACEs and/or TABs,
please use `global-whitespace-mode'.

See also `whitespace-newline' and `whitespace-display-mappings'."
  :lighter    " NL"
  :init-value nil
  :global     t
  :group      'whitespace
  (let ((whitespace-style '(newline-mark newline)))
    (global-whitespace-mode (if global-whitespace-newline-mode
                                1 -1))
    ;; sync states (running a batch job)
    (setq global-whitespace-newline-mode global-whitespace-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; User commands - Toggle


(defconst whitespace-style-value-list
  '(face
    tabs
    spaces
    trailing
    lines
    lines-tail
    newline
    empty
    indentation
    indentation::tab
    indentation::space
    space-after-tab
    space-after-tab::tab
    space-after-tab::space
    space-before-tab
    space-before-tab::tab
    space-before-tab::space
    help-newline       ; value used by `whitespace-insert-option-mark'
    tab-mark
    space-mark
    newline-mark
    )
  "List of valid `whitespace-style' values.")


(defconst whitespace-toggle-option-alist
  '((?f    . face)
    (?t    . tabs)
    (?s    . spaces)
    (?r    . trailing)
    (?l    . lines)
    (?L    . lines-tail)
    (?n    . newline)
    (?e    . empty)
    (?\C-i . indentation)
    (?I    . indentation::tab)
    (?i    . indentation::space)
    (?\C-a . space-after-tab)
    (?A    . space-after-tab::tab)
    (?a    . space-after-tab::space)
    (?\C-b . space-before-tab)
    (?B    . space-before-tab::tab)
    (?b    . space-before-tab::space)
    (?T    . tab-mark)
    (?S    . space-mark)
    (?N    . newline-mark)
    (?x    . whitespace-style)
    )
  "Alist of toggle options.

Each element has the form:

   (CHAR . SYMBOL)

Where:

CHAR	is a char which the user will have to type.

SYMBOL	is a valid symbol associated with CHAR.
	See `whitespace-style-value-list'.")


(defvar whitespace-active-style nil
  "Used to save locally `whitespace-style' value.")

(defvar whitespace-indent-tabs-mode indent-tabs-mode
  "Used to save locally `indent-tabs-mode' value.")

(defvar whitespace-tab-width tab-width
  "Used to save locally `tab-width' value.")

(defvar whitespace-point (point)
  "Used to save locally current point value.
Used by `whitespace-trailing-regexp' function (which see).")

(defvar whitespace-font-lock-refontify nil
  "Used to save locally the font-lock refontify state.
Used by `whitespace-post-command-hook' function (which see).")

(defvar whitespace-bob-marker nil
  "Used to save locally the bob marker value.
Used by `whitespace-post-command-hook' function (which see).")

(defvar whitespace-eob-marker nil
  "Used to save locally the eob marker value.
Used by `whitespace-post-command-hook' function (which see).")

(defvar whitespace-buffer-changed nil
  "Used to indicate locally if buffer changed.
Used by `whitespace-post-command-hook' and `whitespace-buffer-changed'
functions (which see).")


;;;###autoload
(defun whitespace-toggle-options (arg)
  "Toggle local `whitespace-mode' options.

If local whitespace-mode is off, toggle the option given by ARG
and turn on local whitespace-mode.

If local whitespace-mode is on, toggle the option given by ARG
and restart local whitespace-mode.

Interactively, it reads one of the following chars:

  CHAR	MEANING
  (VIA FACES)
   f	toggle face visualization
   t	toggle TAB visualization
   s	toggle SPACE and HARD SPACE visualization
   r	toggle trailing blanks visualization
   l	toggle \"long lines\" visualization
   L	toggle \"long lines\" tail visualization
   n	toggle NEWLINE visualization
   e	toggle empty line at bob and/or eob visualization
   C-i	toggle indentation SPACEs visualization (via `indent-tabs-mode')
   I	toggle indentation SPACEs visualization
   i	toggle indentation TABs visualization
   C-a	toggle SPACEs after TAB visualization (via `indent-tabs-mode')
   A	toggle SPACEs after TAB: SPACEs visualization
   a	toggle SPACEs after TAB: TABs visualization
   C-b	toggle SPACEs before TAB visualization (via `indent-tabs-mode')
   B	toggle SPACEs before TAB: SPACEs visualization
   b	toggle SPACEs before TAB: TABs visualization

  (VIA DISPLAY TABLE)
   T	toggle TAB visualization
   S	toggle SPACEs before TAB visualization
   N	toggle NEWLINE visualization

   x	restore `whitespace-style' value
   ?	display brief help

Non-interactively, ARG should be a symbol or a list of symbols.
The valid symbols are:

   face			toggle face visualization
   tabs			toggle TAB visualization
   spaces		toggle SPACE and HARD SPACE visualization
   trailing		toggle trailing blanks visualization
   lines		toggle \"long lines\" visualization
   lines-tail		toggle \"long lines\" tail visualization
   newline		toggle NEWLINE visualization
   empty		toggle empty line at bob and/or eob visualization
   indentation		toggle indentation SPACEs visualization
   indentation::tab	toggle indentation SPACEs visualization
   indentation::space	toggle indentation TABs visualization
   space-after-tab		toggle SPACEs after TAB visualization
   space-after-tab::tab		toggle SPACEs after TAB: SPACEs visualization
   space-after-tab::space	toggle SPACEs after TAB: TABs visualization
   space-before-tab		toggle SPACEs before TAB visualization
   space-before-tab::tab	toggle SPACEs before TAB: SPACEs visualization
   space-before-tab::space	toggle SPACEs before TAB: TABs visualization

   tab-mark		toggle TAB visualization
   space-mark		toggle SPACEs before TAB visualization
   newline-mark		toggle NEWLINE visualization

   whitespace-style	restore `whitespace-style' value

See `whitespace-style' and `indent-tabs-mode' for documentation."
  (interactive (whitespace-interactive-char t))
  (let ((whitespace-style
	 (whitespace-toggle-list t arg whitespace-active-style)))
    (whitespace-mode 0)
    (whitespace-mode 1)))


(defvar whitespace-toggle-style nil
  "Used to toggle the global `whitespace-style' value.")


;;;###autoload
(defun global-whitespace-toggle-options (arg)
  "Toggle global `whitespace-mode' options.

If global whitespace-mode is off, toggle the option given by ARG
and turn on global whitespace-mode.

If global whitespace-mode is on, toggle the option given by ARG
and restart global whitespace-mode.

Interactively, it accepts one of the following chars:

  CHAR	MEANING
  (VIA FACES)
   f	toggle face visualization
   t	toggle TAB visualization
   s	toggle SPACE and HARD SPACE visualization
   r	toggle trailing blanks visualization
   l	toggle \"long lines\" visualization
   L	toggle \"long lines\" tail visualization
   n	toggle NEWLINE visualization
   e	toggle empty line at bob and/or eob visualization
   C-i	toggle indentation SPACEs visualization (via `indent-tabs-mode')
   I	toggle indentation SPACEs visualization
   i	toggle indentation TABs visualization
   C-a	toggle SPACEs after TAB visualization (via `indent-tabs-mode')
   A	toggle SPACEs after TAB: SPACEs visualization
   a	toggle SPACEs after TAB: TABs visualization
   C-b	toggle SPACEs before TAB visualization (via `indent-tabs-mode')
   B	toggle SPACEs before TAB: SPACEs visualization
   b	toggle SPACEs before TAB: TABs visualization

  (VIA DISPLAY TABLE)
   T	toggle TAB visualization
   S	toggle SPACEs before TAB visualization
   N	toggle NEWLINE visualization

   x	restore `whitespace-style' value
   ?	display brief help

Non-interactively, ARG should be a symbol or a list of symbols.
The valid symbols are:

   face			toggle face visualization
   tabs			toggle TAB visualization
   spaces		toggle SPACE and HARD SPACE visualization
   trailing		toggle trailing blanks visualization
   lines		toggle \"long lines\" visualization
   lines-tail		toggle \"long lines\" tail visualization
   newline		toggle NEWLINE visualization
   empty		toggle empty line at bob and/or eob visualization
   indentation		toggle indentation SPACEs visualization
   indentation::tab	toggle indentation SPACEs visualization
   indentation::space	toggle indentation TABs visualization
   space-after-tab		toggle SPACEs after TAB visualization
   space-after-tab::tab		toggle SPACEs after TAB: SPACEs visualization
   space-after-tab::space	toggle SPACEs after TAB: TABs visualization
   space-before-tab		toggle SPACEs before TAB visualization
   space-before-tab::tab	toggle SPACEs before TAB: SPACEs visualization
   space-before-tab::space	toggle SPACEs before TAB: TABs visualization

   tab-mark		toggle TAB visualization
   space-mark		toggle SPACEs before TAB visualization
   newline-mark		toggle NEWLINE visualization

   whitespace-style	restore `whitespace-style' value

See `whitespace-style' and `indent-tabs-mode' for documentation."
  (interactive (whitespace-interactive-char nil))
  (let ((whitespace-style
	 (whitespace-toggle-list nil arg whitespace-toggle-style)))
    (setq whitespace-toggle-style whitespace-style)
    (global-whitespace-mode 0)
    (global-whitespace-mode 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; User commands - Cleanup


;;;###autoload
(defun whitespace-cleanup ()
  "Cleanup some blank problems in all buffer or at region.

It usually applies to the whole buffer, but in transient mark
mode when the mark is active, it applies to the region.  It also
applies to the region when it is not in transient mark mode, the
mark is active and \\[universal-argument] was pressed just before
calling `whitespace-cleanup' interactively.

See also `whitespace-cleanup-region'.

The problems cleaned up are:

1. empty lines at beginning of buffer.
2. empty lines at end of buffer.
   If `whitespace-style' includes the value `empty', remove all
   empty lines at beginning and/or end of buffer.

3. 8 or more SPACEs at beginning of line.
   If `whitespace-style' includes the value `indentation':
   replace 8 or more SPACEs at beginning of line by TABs, if
   `indent-tabs-mode' is non-nil; otherwise, replace TABs by
   SPACEs.
   If `whitespace-style' includes the value `indentation::tab',
   replace 8 or more SPACEs at beginning of line by TABs.
   If `whitespace-style' includes the value `indentation::space',
   replace TABs by SPACEs.

4. SPACEs before TAB.
   If `whitespace-style' includes the value `space-before-tab':
   replace SPACEs by TABs, if `indent-tabs-mode' is non-nil;
   otherwise, replace TABs by SPACEs.
   If `whitespace-style' includes the value
   `space-before-tab::tab', replace SPACEs by TABs.
   If `whitespace-style' includes the value
   `space-before-tab::space', replace TABs by SPACEs.

5. SPACEs or TABs at end of line.
   If `whitespace-style' includes the value `trailing', remove
   all SPACEs or TABs at end of line.

6. 8 or more SPACEs after TAB.
   If `whitespace-style' includes the value `space-after-tab':
   replace SPACEs by TABs, if `indent-tabs-mode' is non-nil;
   otherwise, replace TABs by SPACEs.
   If `whitespace-style' includes the value
   `space-after-tab::tab', replace SPACEs by TABs.
   If `whitespace-style' includes the value
   `space-after-tab::space', replace TABs by SPACEs.

See `whitespace-style', `indent-tabs-mode' and `tab-width' for
documentation."
  (interactive "@")
  (cond
   ;; read-only buffer
   (buffer-read-only
    (whitespace-warn-read-only "cleanup"))
   ;; region active
   ((and (or transient-mark-mode
	     current-prefix-arg)
	 mark-active)
    ;; PROBLEMs 1 and 2 are not handled in region
    ;; PROBLEM 3: 8 or more SPACEs at bol
    ;; PROBLEM 4: SPACEs before TAB
    ;; PROBLEM 5: SPACEs or TABs at eol
    ;; PROBLEM 6: 8 or more SPACEs after TAB
    (whitespace-cleanup-region (region-beginning) (region-end)))
   ;; whole buffer
   (t
    (save-excursion
      (save-match-data                ;FIXME: Why?
	;; PROBLEM 1: empty lines at bob
	;; PROBLEM 2: empty lines at eob
	;; ACTION: remove all empty lines at bob and/or eob
	(when (memq 'empty whitespace-style)
	  (let (overwrite-mode)		; enforce no overwrite
	    (goto-char (point-min))
	    (when (re-search-forward
		   (concat "\\`" whitespace-empty-at-bob-regexp) nil t)
	      (delete-region (match-beginning 1) (match-end 1)))
	    (when (re-search-forward
		   (concat whitespace-empty-at-eob-regexp "\\'") nil t)
	      (delete-region (match-beginning 1) (match-end 1)))))))
    ;; PROBLEM 3: 8 or more SPACEs at bol
    ;; PROBLEM 4: SPACEs before TAB
    ;; PROBLEM 5: SPACEs or TABs at eol
    ;; PROBLEM 6: 8 or more SPACEs after TAB
    (whitespace-cleanup-region (point-min) (point-max)))))


;;;###autoload
(defun whitespace-cleanup-region (start end)
  "Cleanup some blank problems at region.

The problems cleaned up are:

1. 8 or more SPACEs at beginning of line.
   If `whitespace-style' includes the value `indentation':
   replace 8 or more SPACEs at beginning of line by TABs, if
   `indent-tabs-mode' is non-nil; otherwise, replace TABs by
   SPACEs.
   If `whitespace-style' includes the value `indentation::tab',
   replace 8 or more SPACEs at beginning of line by TABs.
   If `whitespace-style' includes the value `indentation::space',
   replace TABs by SPACEs.

2. SPACEs before TAB.
   If `whitespace-style' includes the value `space-before-tab':
   replace SPACEs by TABs, if `indent-tabs-mode' is non-nil;
   otherwise, replace TABs by SPACEs.
   If `whitespace-style' includes the value
   `space-before-tab::tab', replace SPACEs by TABs.
   If `whitespace-style' includes the value
   `space-before-tab::space', replace TABs by SPACEs.

3. SPACEs or TABs at end of line.
   If `whitespace-style' includes the value `trailing', remove
   all SPACEs or TABs at end of line.

4. 8 or more SPACEs after TAB.
   If `whitespace-style' includes the value `space-after-tab':
   replace SPACEs by TABs, if `indent-tabs-mode' is non-nil;
   otherwise, replace TABs by SPACEs.
   If `whitespace-style' includes the value
   `space-after-tab::tab', replace SPACEs by TABs.
   If `whitespace-style' includes the value
   `space-after-tab::space', replace TABs by SPACEs.

See `whitespace-style', `indent-tabs-mode' and `tab-width' for
documentation."
  (interactive "@r")
  (if buffer-read-only
      ;; read-only buffer
      (whitespace-warn-read-only "cleanup region")
    ;; non-read-only buffer
    (let ((rstart           (min start end))
	  (rend             (copy-marker (max start end)))
	  (indent-tabs-mode whitespace-indent-tabs-mode)
	  (tab-width        whitespace-tab-width)
	  overwrite-mode		; enforce no overwrite
	  tmp)
      (save-excursion
	(save-match-data                ;FIXME: Why?
	  ;; PROBLEM 1: 8 or more SPACEs at bol
	  (cond
	   ;; ACTION: replace 8 or more SPACEs at bol by TABs, if
	   ;; `indent-tabs-mode' is non-nil; otherwise, replace TABs
	   ;; by SPACEs.
	   ((memq 'indentation whitespace-style)
	    (let ((regexp (whitespace-indentation-regexp)))
	      (goto-char rstart)
	      (while (re-search-forward regexp rend t)
		(setq tmp (current-indentation))
		(goto-char (match-beginning 0))
		(delete-horizontal-space)
		(unless (eolp)
		  (indent-to tmp)))))
	   ;; ACTION: replace 8 or more SPACEs at bol by TABs.
	   ((memq 'indentation::tab whitespace-style)
	    (whitespace-replace-action
	     'tabify rstart rend
	     (whitespace-indentation-regexp 'tab) 0))
	   ;; ACTION: replace TABs by SPACEs.
	   ((memq 'indentation::space whitespace-style)
	    (whitespace-replace-action
	     'untabify rstart rend
	     (whitespace-indentation-regexp 'space) 0)))
	  ;; PROBLEM 3: SPACEs or TABs at eol
	  ;; ACTION: remove all SPACEs or TABs at eol
	  (when (memq 'trailing whitespace-style)
	    (whitespace-replace-action
	     'delete-region rstart rend
	     whitespace-trailing-regexp 1))
	  ;; PROBLEM 4: 8 or more SPACEs after TAB
	  (cond
	   ;; ACTION: replace 8 or more SPACEs by TABs, if
	   ;; `indent-tabs-mode' is non-nil; otherwise, replace TABs
	   ;; by SPACEs.
	   ((memq 'space-after-tab whitespace-style)
	    (whitespace-replace-action
	     (if whitespace-indent-tabs-mode 'tabify 'untabify)
	     rstart rend (whitespace-space-after-tab-regexp) 1))
	   ;; ACTION: replace 8 or more SPACEs by TABs.
	   ((memq 'space-after-tab::tab whitespace-style)
	    (whitespace-replace-action
	     'tabify rstart rend
	     (whitespace-space-after-tab-regexp 'tab) 1))
	   ;; ACTION: replace TABs by SPACEs.
	   ((memq 'space-after-tab::space whitespace-style)
	    (whitespace-replace-action
	     'untabify rstart rend
	     (whitespace-space-after-tab-regexp 'space) 1)))
	  ;; PROBLEM 2: SPACEs before TAB
	  (cond
	   ;; ACTION: replace SPACEs before TAB by TABs, if
	   ;; `indent-tabs-mode' is non-nil; otherwise, replace TABs
	   ;; by SPACEs.
	   ((memq 'space-before-tab whitespace-style)
	    (whitespace-replace-action
	     (if whitespace-indent-tabs-mode 'tabify 'untabify)
	     rstart rend whitespace-space-before-tab-regexp
	     (if whitespace-indent-tabs-mode 0 2)))
	   ;; ACTION: replace SPACEs before TAB by TABs.
	   ((memq 'space-before-tab::tab whitespace-style)
	    (whitespace-replace-action
	     'tabify rstart rend
	     whitespace-space-before-tab-regexp 0))
	   ;; ACTION: replace TABs by SPACEs.
	   ((memq 'space-before-tab::space whitespace-style)
	    (whitespace-replace-action
	     'untabify rstart rend
	     whitespace-space-before-tab-regexp 2)))))
      (set-marker rend nil))))		; point marker to nowhere


(defun whitespace-replace-action (action rstart rend regexp index)
  "Do ACTION in the string matched by REGEXP between RSTART and REND.

INDEX is the level group matched by REGEXP and used by ACTION.

See also `tab-width'."
  (goto-char rstart)
  (while (re-search-forward regexp rend t)
    (goto-char (match-end index))
    (funcall action (match-beginning index) (match-end index))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; User command - report


(defun whitespace-regexp (regexp &optional kind)
  "Return REGEXP depending on `whitespace-indent-tabs-mode'."
  (cond
   ((or (eq kind 'tab)
	whitespace-indent-tabs-mode)
    (format (car regexp) whitespace-tab-width))
   ((or (eq kind 'space)
	(not whitespace-indent-tabs-mode))
    (cdr regexp))))


(defun whitespace-indentation-regexp (&optional kind)
  "Return the indentation regexp depending on `whitespace-indent-tabs-mode'."
  (whitespace-regexp whitespace-indentation-regexp kind))


(defun whitespace-space-after-tab-regexp (&optional kind)
  "Return the space-after-tab regexp depending on `whitespace-indent-tabs-mode'."
  (whitespace-regexp whitespace-space-after-tab-regexp kind))


(defconst whitespace-report-list
  (list
   (cons 'empty                   whitespace-empty-at-bob-regexp)
   (cons 'empty                   whitespace-empty-at-eob-regexp)
   (cons 'trailing                whitespace-trailing-regexp)
   (cons 'indentation             nil)
   (cons 'indentation::tab        nil)
   (cons 'indentation::space      nil)
   (cons 'space-before-tab        whitespace-space-before-tab-regexp)
   (cons 'space-before-tab::tab   whitespace-space-before-tab-regexp)
   (cons 'space-before-tab::space whitespace-space-before-tab-regexp)
   (cons 'space-after-tab         nil)
   (cons 'space-after-tab::tab    nil)
   (cons 'space-after-tab::space  nil)
   )
   "List of whitespace bogus symbol and corresponding regexp.")


(defconst whitespace-report-text
  '( ;; `indent-tabs-mode' has non-nil value
    "\
 Whitespace Report

 Current Setting                       Whitespace Problem

 empty                    []     []  empty lines at beginning of buffer
 empty                    []     []  empty lines at end of buffer
 trailing                 []     []  SPACEs or TABs at end of line
 indentation              []     []  8 or more SPACEs at beginning of line
 indentation::tab         []     []  8 or more SPACEs at beginning of line
 indentation::space       []     []  TABs at beginning of line
 space-before-tab         []     []  SPACEs before TAB
 space-before-tab::tab    []     []  SPACEs before TAB: SPACEs
 space-before-tab::space  []     []  SPACEs before TAB: TABs
 space-after-tab          []     []  8 or more SPACEs after TAB
 space-after-tab::tab     []     []  8 or more SPACEs after TAB: SPACEs
 space-after-tab::space   []     []  8 or more SPACEs after TAB: TABs

 indent-tabs-mode =
 tab-width        = \n\n"
    . ;; `indent-tabs-mode' has nil value
    "\
 Whitespace Report

 Current Setting                       Whitespace Problem

 empty                    []     []  empty lines at beginning of buffer
 empty                    []     []  empty lines at end of buffer
 trailing                 []     []  SPACEs or TABs at end of line
 indentation              []     []  TABs at beginning of line
 indentation::tab         []     []  8 or more SPACEs at beginning of line
 indentation::space       []     []  TABs at beginning of line
 space-before-tab         []     []  SPACEs before TAB
 space-before-tab::tab    []     []  SPACEs before TAB: SPACEs
 space-before-tab::space  []     []  SPACEs before TAB: TABs
 space-after-tab          []     []  8 or more SPACEs after TAB
 space-after-tab::tab     []     []  8 or more SPACEs after TAB: SPACEs
 space-after-tab::space   []     []  8 or more SPACEs after TAB: TABs

 indent-tabs-mode =
 tab-width        = \n\n")
  "Text for whitespace bogus report.

It is a cons of strings, where the car part is used when
`indent-tabs-mode' is non-nil, and the cdr part is used when
`indent-tabs-mode' is nil.")


(defconst whitespace-report-buffer-name "*Whitespace Report*"
  "The buffer name for whitespace bogus report.")


;;;###autoload
(defun whitespace-report (&optional force report-if-bogus)
  "Report some whitespace problems in buffer.

Return nil if there is no whitespace problem; otherwise, return
non-nil.

If FORCE is non-nil or \\[universal-argument] was pressed just
before calling `whitespace-report' interactively, it forces
`whitespace-style' to have:

   empty
   trailing
   indentation
   space-before-tab
   space-after-tab

If REPORT-IF-BOGUS is non-nil, it reports only when there are any
whitespace problems in buffer.

Report if some of the following whitespace problems exist:

* If `indent-tabs-mode' is non-nil:
   empty		1. empty lines at beginning of buffer.
   empty		2. empty lines at end of buffer.
   trailing		3. SPACEs or TABs at end of line.
   indentation		4. 8 or more SPACEs at beginning of line.
   space-before-tab	5. SPACEs before TAB.
   space-after-tab	6. 8 or more SPACEs after TAB.

* If `indent-tabs-mode' is nil:
   empty		1. empty lines at beginning of buffer.
   empty		2. empty lines at end of buffer.
   trailing		3. SPACEs or TABs at end of line.
   indentation		4. TABS at beginning of line.
   space-before-tab	5. SPACEs before TAB.
   space-after-tab	6. 8 or more SPACEs after TAB.

See `whitespace-style' for documentation.
See also `whitespace-cleanup' and `whitespace-cleanup-region' for
cleaning up these problems."
  (interactive (list current-prefix-arg))
  (whitespace-report-region (point-min) (point-max)
			    force report-if-bogus))


;;;###autoload
(defun whitespace-report-region (start end &optional force report-if-bogus)
  "Report some whitespace problems in a region.

Return nil if there is no whitespace problem; otherwise, return
non-nil.

If FORCE is non-nil or \\[universal-argument] was pressed just
before calling `whitespace-report-region' interactively, it
forces `whitespace-style' to have:

   empty
   indentation
   space-before-tab
   trailing
   space-after-tab

If REPORT-IF-BOGUS is non-nil, it reports only when there are any
whitespace problems in buffer.

Report if some of the following whitespace problems exist:

* If `indent-tabs-mode' is non-nil:
   empty		1. empty lines at beginning of buffer.
   empty		2. empty lines at end of buffer.
   trailing		3. SPACEs or TABs at end of line.
   indentation		4. 8 or more SPACEs at beginning of line.
   space-before-tab	5. SPACEs before TAB.
   space-after-tab	6. 8 or more SPACEs after TAB.

* If `indent-tabs-mode' is nil:
   empty		1. empty lines at beginning of buffer.
   empty		2. empty lines at end of buffer.
   trailing		3. SPACEs or TABs at end of line.
   indentation		4. TABS at beginning of line.
   space-before-tab	5. SPACEs before TAB.
   space-after-tab	6. 8 or more SPACEs after TAB.

See `whitespace-style' for documentation.
See also `whitespace-cleanup' and `whitespace-cleanup-region' for
cleaning up these problems."
  (interactive "r")
  (setq force (or current-prefix-arg force))
  (save-excursion
    (save-match-data                ;FIXME: Why?
      (let* ((has-bogus nil)
	     (rstart    (min start end))
	     (rend      (max start end))
	     (bogus-list
	      (mapcar
	       #'(lambda (option)
		   (when force
		     (add-to-list 'whitespace-style (car option)))
		   (goto-char rstart)
		   (let ((regexp
			  (cond
			   ((eq (car option) 'indentation)
			    (whitespace-indentation-regexp))
			   ((eq (car option) 'indentation::tab)
			    (whitespace-indentation-regexp 'tab))
			   ((eq (car option) 'indentation::space)
			    (whitespace-indentation-regexp 'space))
			   ((eq (car option) 'space-after-tab)
			    (whitespace-space-after-tab-regexp))
			   ((eq (car option) 'space-after-tab::tab)
			    (whitespace-space-after-tab-regexp 'tab))
			   ((eq (car option) 'space-after-tab::space)
			    (whitespace-space-after-tab-regexp 'space))
			   (t
			    (cdr option)))))
		     (and (re-search-forward regexp rend t)
			  (setq has-bogus t))))
	       whitespace-report-list)))
	(when (if report-if-bogus has-bogus t)
	  (whitespace-kill-buffer whitespace-report-buffer-name)
	  ;; `whitespace-indent-tabs-mode' is local to current buffer
	  ;; `whitespace-tab-width' is local to current buffer
	  (let ((ws-indent-tabs-mode whitespace-indent-tabs-mode)
		(ws-tab-width whitespace-tab-width))
	    (with-current-buffer (get-buffer-create
				  whitespace-report-buffer-name)
	      (erase-buffer)
	      (insert (if ws-indent-tabs-mode
			  (car whitespace-report-text)
			(cdr whitespace-report-text)))
	      (goto-char (point-min))
	      (forward-line 3)
	      (dolist (option whitespace-report-list)
		(forward-line 1)
		(whitespace-mark-x
		 27 (memq (car option) whitespace-style))
		(whitespace-mark-x 7 (car bogus-list))
		(setq bogus-list (cdr bogus-list)))
	      (forward-line 1)
	      (whitespace-insert-value ws-indent-tabs-mode)
	      (whitespace-insert-value ws-tab-width)
	      (when has-bogus
		(goto-char (point-max))
		(insert " Type `M-x whitespace-cleanup'"
			" to cleanup the buffer.\n\n"
			" Type `M-x whitespace-cleanup-region'"
			" to cleanup a region.\n\n"))
	      (whitespace-display-window (current-buffer)))))
	has-bogus))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Internal functions


(defvar whitespace-font-lock-mode nil
  "Used to remember whether a buffer had font lock mode on or not.")

(defvar whitespace-font-lock nil
  "Used to remember whether a buffer initially had font lock on or not.")

(defvar whitespace-font-lock-keywords nil
  "Used to save locally `font-lock-keywords' value.")


(defconst whitespace-help-text
  "\
 Whitespace Toggle Options                  | scroll up  :  SPC   or > |
                                            | scroll down:  M-SPC or < |
 FACES                                      \\__________________________/
 []  f   - toggle face visualization
 []  t   - toggle TAB visualization
 []  s   - toggle SPACE and HARD SPACE visualization
 []  r   - toggle trailing blanks visualization
 []  l   - toggle \"long lines\" visualization
 []  L   - toggle \"long lines\" tail visualization
 []  n   - toggle NEWLINE visualization
 []  e   - toggle empty line at bob and/or eob visualization
 []  C-i - toggle indentation SPACEs visualization (via `indent-tabs-mode')
 []  I   - toggle indentation SPACEs visualization
 []  i   - toggle indentation TABs visualization
 []  C-a - toggle SPACEs after TAB visualization (via `indent-tabs-mode')
 []  A   - toggle SPACEs after TAB: SPACEs visualization
 []  a   - toggle SPACEs after TAB: TABs visualization
 []  C-b - toggle SPACEs before TAB visualization (via `indent-tabs-mode')
 []  B   - toggle SPACEs before TAB: SPACEs visualization
 []  b   - toggle SPACEs before TAB: TABs visualization

 DISPLAY TABLE
 []  T - toggle TAB visualization
 []  S - toggle SPACE and HARD SPACE visualization
 []  N - toggle NEWLINE visualization

      x - restore `whitespace-style' value

      ? - display this text\n\n"
  "Text for whitespace toggle options.")


(defconst whitespace-help-buffer-name "*Whitespace Toggle Options*"
  "The buffer name for whitespace toggle options.")


(defun whitespace-insert-value (value)
  "Insert VALUE at column 20 of next line."
  (forward-line 1)
  (move-to-column 20 t)
  (insert (format "%s" value)))


(defun whitespace-mark-x (nchars condition)
  "Insert the mark ('X' or ' ') after NCHARS depending on CONDITION."
  (forward-char nchars)
  (insert (if condition "X" " ")))


(defun whitespace-insert-option-mark (the-list the-value)
  "Insert the option mark ('X' or ' ') in toggle options buffer."
  (goto-char (point-min))
  (forward-line 2)
  (dolist (sym  the-list)
    (if (eq sym 'help-newline)
	(forward-line 2)
      (forward-line 1)
      (whitespace-mark-x 2 (memq sym the-value)))))


(defun whitespace-help-on (style)
  "Display the whitespace toggle options."
  (unless (get-buffer whitespace-help-buffer-name)
    (delete-other-windows)
    (let ((buffer (get-buffer-create whitespace-help-buffer-name)))
      (with-current-buffer buffer
	(erase-buffer)
	(insert whitespace-help-text)
	(whitespace-insert-option-mark
	 whitespace-style-value-list style)
	(whitespace-display-window buffer)))))


(defun whitespace-display-window (buffer)
  "Display BUFFER in a new window."
  (goto-char (point-min))
  (set-buffer-modified-p nil)
  (when (< (window-height) (* 2 window-min-height))
    (kill-buffer buffer)
    (error "Window height is too small; \
can't split window to display whitespace toggle options"))
  (let ((win (split-window)))
    (set-window-buffer win buffer)
    (shrink-window-if-larger-than-buffer win)))


(defun whitespace-kill-buffer (buffer-name)
  "Kill buffer BUFFER-NAME and windows related with it."
  (let ((buffer (get-buffer buffer-name)))
    (when buffer
      (delete-windows-on buffer)
      (kill-buffer buffer))))


(defun whitespace-help-off ()
  "Remove the buffer and window of the whitespace toggle options."
  (whitespace-kill-buffer whitespace-help-buffer-name))


(defun whitespace-help-scroll (&optional up)
  "Scroll help window, if it exists.

If UP is non-nil, scroll up; otherwise, scroll down."
  (condition-case nil
      (let ((buffer (get-buffer whitespace-help-buffer-name)))
	(if buffer
	    (with-selected-window (get-buffer-window buffer)
	      (if up
		  (scroll-up 3)
		(scroll-down 3)))
	  (ding)))
    ;; handler
    ((error)
     ;; just ignore error
     )))


(defun whitespace-interactive-char (local-p)
  "Interactive function to read a char and return a symbol.

If LOCAL-P is non-nil, it uses a local context; otherwise, it
uses a global context.

It accepts one of the following chars:

  CHAR	MEANING
  (VIA FACES)
   f	toggle face visualization
   t	toggle TAB visualization
   s	toggle SPACE and HARD SPACE visualization
   r	toggle trailing blanks visualization
   l	toggle \"long lines\" visualization
   L	toggle \"long lines\" tail visualization
   n	toggle NEWLINE visualization
   e	toggle empty line at bob and/or eob visualization
   C-i	toggle indentation SPACEs visualization (via `indent-tabs-mode')
   I	toggle indentation SPACEs visualization
   i	toggle indentation TABs visualization
   C-a	toggle SPACEs after TAB visualization (via `indent-tabs-mode')
   A	toggle SPACEs after TAB: SPACEs visualization
   a	toggle SPACEs after TAB: TABs visualization
   C-b	toggle SPACEs before TAB visualization (via `indent-tabs-mode')
   B	toggle SPACEs before TAB: SPACEs visualization
   b	toggle SPACEs before TAB: TABs visualization

  (VIA DISPLAY TABLE)
   T	toggle TAB visualization
   S	toggle SPACE and HARD SPACE visualization
   N	toggle NEWLINE visualization

   x	restore `whitespace-style' value
   ?	display brief help

See also `whitespace-toggle-option-alist'."
  (let* ((is-off (not (if local-p
			  whitespace-mode
			global-whitespace-mode)))
	 (style  (cond (is-off  whitespace-style) ; use default value
		       (local-p whitespace-active-style)
		       (t       whitespace-toggle-style)))
	 (prompt
	  (format "Whitespace Toggle %s (type ? for further options)-"
		  (if local-p "Local" "Global")))
	 ch sym)
    ;; read a valid option and get the corresponding symbol
    (save-window-excursion
      (condition-case data
	  (progn
	    (while
		;; while condition
		(progn
		  (setq ch (read-char prompt))
		  (not
		   (setq sym
			 (cdr
			  (assq ch whitespace-toggle-option-alist)))))
	      ;; while body
	      (cond
	       ((eq ch ?\?)   (whitespace-help-on style))
	       ((eq ch ?\ )   (whitespace-help-scroll t))
	       ((eq ch ?\M- ) (whitespace-help-scroll))
	       ((eq ch ?>)    (whitespace-help-scroll t))
	       ((eq ch ?<)    (whitespace-help-scroll))
	       (t             (ding))))
	    (whitespace-help-off)
	    (message " "))		; clean echo area
	;; handler
	((quit error)
	 (whitespace-help-off)
	 (error (error-message-string data)))))
    (list sym)))			; return the appropriate symbol


(defun whitespace-toggle-list (local-p arg the-list)
  "Toggle options in THE-LIST based on list ARG.

If LOCAL-P is non-nil, it uses a local context; otherwise, it
uses a global context.

ARG is a list of options to be toggled.

THE-LIST is a list of options.  This list will be toggled and the
resultant list will be returned."
  (unless (if local-p whitespace-mode global-whitespace-mode)
    (setq the-list whitespace-style))
  (setq the-list (copy-sequence the-list)) ; keep original list
  (dolist (sym (if (listp arg) arg (list arg)))
    (cond
     ;; ignore help value
     ((eq sym 'help-newline))
     ;; restore default values
     ((eq sym 'whitespace-style)
      (setq the-list whitespace-style))
     ;; toggle valid values
     ((memq sym whitespace-style-value-list)
      (setq the-list (if (memq sym the-list)
			 (delq sym the-list)
		       (cons sym the-list))))))
  the-list)


(defvar whitespace-display-table nil
  "Used to save a local display table.")

(defvar whitespace-display-table-was-local nil
  "Used to remember whether a buffer initially had a local display table.")


(defun whitespace-turn-on ()
  "Turn on whitespace visualization."
  ;; prepare local hooks
  (add-hook 'write-file-functions 'whitespace-write-file-hook nil t)
  ;; create whitespace local buffer environment
  (set (make-local-variable 'whitespace-font-lock-mode) nil)
  (set (make-local-variable 'whitespace-font-lock) nil)
  (set (make-local-variable 'whitespace-font-lock-keywords) nil)
  (set (make-local-variable 'whitespace-display-table) nil)
  (set (make-local-variable 'whitespace-display-table-was-local) nil)
  (set (make-local-variable 'whitespace-active-style)
       (if (listp whitespace-style)
	   whitespace-style
	 (list whitespace-style)))
  (set (make-local-variable 'whitespace-indent-tabs-mode)
       indent-tabs-mode)
  (set (make-local-variable 'whitespace-tab-width)
       tab-width)
  ;; turn on whitespace
  (when whitespace-active-style
    (whitespace-color-on)
    (whitespace-display-char-on)))


(defun whitespace-turn-off ()
  "Turn off whitespace visualization."
  (remove-hook 'write-file-functions 'whitespace-write-file-hook t)
  (when whitespace-active-style
    (whitespace-color-off)
    (whitespace-display-char-off)))


(defun whitespace-style-face-p ()
  "Return t if there is some visualization via face."
  (and (memq 'face whitespace-active-style)
       (or (memq 'tabs                    whitespace-active-style)
	   (memq 'spaces                  whitespace-active-style)
	   (memq 'trailing                whitespace-active-style)
	   (memq 'lines                   whitespace-active-style)
	   (memq 'lines-tail              whitespace-active-style)
	   (memq 'newline                 whitespace-active-style)
	   (memq 'empty                   whitespace-active-style)
	   (memq 'indentation             whitespace-active-style)
	   (memq 'indentation::tab        whitespace-active-style)
	   (memq 'indentation::space      whitespace-active-style)
	   (memq 'space-after-tab         whitespace-active-style)
	   (memq 'space-after-tab::tab    whitespace-active-style)
	   (memq 'space-after-tab::space  whitespace-active-style)
	   (memq 'space-before-tab        whitespace-active-style)
	   (memq 'space-before-tab::tab   whitespace-active-style)
	   (memq 'space-before-tab::space whitespace-active-style))))


(defun whitespace-color-on ()
  "Turn on color visualization."
  (when (whitespace-style-face-p)
    (unless whitespace-font-lock
      (setq whitespace-font-lock t
	    whitespace-font-lock-keywords
	    (copy-sequence font-lock-keywords)))
    ;; save current point and refontify when necessary
    (set (make-local-variable 'whitespace-point)
	 (point))
    (set (make-local-variable 'whitespace-font-lock-refontify)
	 0)
    (set (make-local-variable 'whitespace-bob-marker)
	 (point-min-marker))
    (set (make-local-variable 'whitespace-eob-marker)
	 (point-max-marker))
    (set (make-local-variable 'whitespace-buffer-changed)
	 nil)
    (add-hook 'post-command-hook #'whitespace-post-command-hook nil t)
    (add-hook 'before-change-functions #'whitespace-buffer-changed nil t)
    ;; turn off font lock
    (set (make-local-variable 'whitespace-font-lock-mode)
	 font-lock-mode)
    (font-lock-mode 0)
    ;; add whitespace-mode color into font lock
    (when (memq 'spaces whitespace-active-style)
      (font-lock-add-keywords
       nil
       (list
	;; Show SPACEs
	(list whitespace-space-regexp  1 whitespace-space  t)
	;; Show HARD SPACEs
	(list whitespace-hspace-regexp 1 whitespace-hspace t))
       t))
    (when (memq 'tabs whitespace-active-style)
      (font-lock-add-keywords
       nil
       (list
	;; Show TABs
	(list whitespace-tab-regexp 1 whitespace-tab t))
       t))
    (when (memq 'trailing whitespace-active-style)
      (font-lock-add-keywords
       nil
       (list
	;; Show trailing blanks
	(list #'whitespace-trailing-regexp 1 whitespace-trailing t))
       t))
    (when (or (memq 'lines      whitespace-active-style)
	      (memq 'lines-tail whitespace-active-style))
      (font-lock-add-keywords
       nil
       (list
	;; Show "long" lines
	(list
	 (let ((line-column (or whitespace-line-column fill-column)))
	   (format
	    "^\\([^\t\n]\\{%s\\}\\|[^\t\n]\\{0,%s\\}\t\\)\\{%d\\}%s\\(.+\\)$"
	    whitespace-tab-width
	    (1- whitespace-tab-width)
	    (/ line-column whitespace-tab-width)
	    (let ((rem (% line-column whitespace-tab-width)))
	      (if (zerop rem)
		  ""
		(format ".\\{%d\\}" rem)))))
	 (if (memq 'lines whitespace-active-style)
	     0				; whole line
	   2)				; line tail
	 whitespace-line t))
       t))
    (cond
     ((memq 'space-before-tab whitespace-active-style)
      (font-lock-add-keywords
       nil
       (list
	;; Show SPACEs before TAB (indent-tabs-mode)
	(list whitespace-space-before-tab-regexp
	      (if whitespace-indent-tabs-mode 1 2)
	      whitespace-space-before-tab t))
       t))
     ((memq 'space-before-tab::tab whitespace-active-style)
      (font-lock-add-keywords
       nil
       (list
	;; Show SPACEs before TAB (SPACEs)
	(list whitespace-space-before-tab-regexp
	      1 whitespace-space-before-tab t))
       t))
     ((memq 'space-before-tab::space whitespace-active-style)
      (font-lock-add-keywords
       nil
       (list
	;; Show SPACEs before TAB (TABs)
	(list whitespace-space-before-tab-regexp
	      2 whitespace-space-before-tab t))
       t)))
    (cond
     ((memq 'indentation whitespace-active-style)
      (font-lock-add-keywords
       nil
       (list
	;; Show indentation SPACEs (indent-tabs-mode)
	(list (whitespace-indentation-regexp)
	      1 whitespace-indentation t))
       t))
     ((memq 'indentation::tab whitespace-active-style)
      (font-lock-add-keywords
       nil
       (list
	;; Show indentation SPACEs (SPACEs)
	(list (whitespace-indentation-regexp 'tab)
	      1 whitespace-indentation t))
       t))
     ((memq 'indentation::space whitespace-active-style)
      (font-lock-add-keywords
       nil
       (list
	;; Show indentation SPACEs (TABs)
	(list (whitespace-indentation-regexp 'space)
	      1 whitespace-indentation t))
       t)))
    (when (memq 'empty whitespace-active-style)
      (font-lock-add-keywords
       nil
       (list
	;; Show empty lines at beginning of buffer
	(list #'whitespace-empty-at-bob-regexp
	      1 whitespace-empty t))
       t)
      (font-lock-add-keywords
       nil
       (list
	;; Show empty lines at end of buffer
	(list #'whitespace-empty-at-eob-regexp
	      1 whitespace-empty t))
       t))
    (cond
     ((memq 'space-after-tab whitespace-active-style)
      (font-lock-add-keywords
       nil
       (list
	;; Show SPACEs after TAB (indent-tabs-mode)
	(list (whitespace-space-after-tab-regexp)
	      1 whitespace-space-after-tab t))
       t))
     ((memq 'space-after-tab::tab whitespace-active-style)
      (font-lock-add-keywords
       nil
       (list
	;; Show SPACEs after TAB (SPACEs)
	(list (whitespace-space-after-tab-regexp 'tab)
	      1 whitespace-space-after-tab t))
       t))
     ((memq 'space-after-tab::space whitespace-active-style)
      (font-lock-add-keywords
       nil
       (list
	;; Show SPACEs after TAB (TABs)
	(list (whitespace-space-after-tab-regexp 'space)
	      1 whitespace-space-after-tab t))
       t)))
    ;; now turn on font lock and highlight blanks
    (font-lock-mode 1)))


(defun whitespace-color-off ()
  "Turn off color visualization."
  ;; turn off font lock
  (when (whitespace-style-face-p)
    (font-lock-mode 0)
    (remove-hook 'post-command-hook #'whitespace-post-command-hook t)
    (remove-hook 'before-change-functions #'whitespace-buffer-changed t)
    (when whitespace-font-lock
      (setq whitespace-font-lock nil
	    font-lock-keywords   whitespace-font-lock-keywords))
    ;; restore original font lock state
    (font-lock-mode whitespace-font-lock-mode)))


(defun whitespace-trailing-regexp (limit)
  "Match trailing spaces which do not contain the point at end of line."
  (let ((status t))
    (while (if (re-search-forward whitespace-trailing-regexp limit t)
	       (= whitespace-point (match-end 1)) ;; loop if point at eol
	     (setq status nil)))		  ;; end of buffer
    status))


(defun whitespace-empty-at-bob-regexp (limit)
  "Match spaces at beginning of buffer which do not contain the point at \
beginning of buffer."
  (let ((b (point))
	r)
    (cond
     ;; at bob
     ((= b 1)
      (setq r (and (/= whitespace-point 1)
		   (looking-at whitespace-empty-at-bob-regexp)))
      (set-marker whitespace-bob-marker (if r (match-end 1) b)))
     ;; inside bob empty region
     ((<= limit whitespace-bob-marker)
      (setq r (looking-at whitespace-empty-at-bob-regexp))
      (if r
	  (when (< (match-end 1) limit)
	    (set-marker whitespace-bob-marker (match-end 1)))
	(set-marker whitespace-bob-marker b)))
     ;; intersection with end of bob empty region
     ((<= b whitespace-bob-marker)
      (setq r (looking-at whitespace-empty-at-bob-regexp))
      (set-marker whitespace-bob-marker (if r (match-end 1) b)))
     ;; it is not inside bob empty region
     (t
      (setq r nil)))
    ;; move to end of matching
    (and r (goto-char (match-end 1)))
    r))


(defsubst whitespace-looking-back (regexp limit)
  (save-excursion
    (when (/= 0 (skip-chars-backward " \t\n" limit))
      (unless (bolp)
	(forward-line 1))
      (looking-at regexp))))


(defun whitespace-empty-at-eob-regexp (limit)
  "Match spaces at end of buffer which do not contain the point at end of \
buffer."
  (let ((b (point))
	(e (1+ (buffer-size)))
	r)
    (cond
     ;; at eob
     ((= limit e)
      (when (/= whitespace-point e)
	(goto-char limit)
	(setq r (whitespace-looking-back whitespace-empty-at-eob-regexp b)))
      (if r
	  (set-marker whitespace-eob-marker (match-beginning 1))
	(set-marker whitespace-eob-marker limit)
	(goto-char b)))			; return back to initial position
     ;; inside eob empty region
     ((>= b whitespace-eob-marker)
      (goto-char limit)
      (setq r (whitespace-looking-back whitespace-empty-at-eob-regexp b))
      (if r
	  (when (> (match-beginning 1) b)
	    (set-marker whitespace-eob-marker (match-beginning 1)))
	(set-marker whitespace-eob-marker limit)
	(goto-char b)))			; return back to initial position
     ;; intersection with beginning of eob empty region
     ((>= limit whitespace-eob-marker)
      (goto-char limit)
      (setq r (whitespace-looking-back whitespace-empty-at-eob-regexp b))
      (if r
	  (set-marker whitespace-eob-marker (match-beginning 1))
	(set-marker whitespace-eob-marker limit)
	(goto-char b)))			; return back to initial position
     ;; it is not inside eob empty region
     (t
      (setq r nil)))
    r))


(defun whitespace-buffer-changed (_beg _end)
  "Set `whitespace-buffer-changed' variable to t."
  (setq whitespace-buffer-changed t))


(defun whitespace-post-command-hook ()
  "Save current point into `whitespace-point' variable.
Also refontify when necessary."
  (setq whitespace-point (point))	; current point position
  (let ((refontify
	 (or
	  ;; it is at end of line ...
	  (and (eolp)
	       ;; ... with trailing SPACE or TAB
	       (or (= (preceding-char) ?\ )
		   (= (preceding-char) ?\t)))
	  ;; it is at beginning of buffer (bob)
	  (= whitespace-point 1)
	  ;; the buffer was modified and ...
	  (and whitespace-buffer-changed
	       (or
		;; ... or inside bob whitespace region
		(<= whitespace-point whitespace-bob-marker)
		;; ... or at bob whitespace region border
		(and (= whitespace-point (1+ whitespace-bob-marker))
		     (= (preceding-char) ?\n))))
	  ;; it is at end of buffer (eob)
	  (= whitespace-point (1+ (buffer-size)))
	  ;; the buffer was modified and ...
	  (and whitespace-buffer-changed
	       (or
		;; ... or inside eob whitespace region
	        (>= whitespace-point whitespace-eob-marker)
		;; ... or at eob whitespace region border
		(and (= whitespace-point (1- whitespace-eob-marker))
		     (= (following-char) ?\n)))))))
    (when (or refontify (> whitespace-font-lock-refontify 0))
      (setq whitespace-buffer-changed nil)
      ;; adjust refontify counter
      (setq whitespace-font-lock-refontify
	    (if refontify
		1
	      (1- whitespace-font-lock-refontify)))
      ;; refontify
      (jit-lock-refontify))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Hacked from visws.el (Miles Bader <miles@gnu.org>)


(defun whitespace-style-mark-p ()
  "Return t if there is some visualization via display table."
  (or (memq 'tab-mark     whitespace-active-style)
      (memq 'space-mark   whitespace-active-style)
      (memq 'newline-mark whitespace-active-style)))


(defsubst whitespace-char-valid-p (char)
  ;; This check should be improved!!!
  (or (< char 256)
      (characterp char)))


(defun whitespace-display-vector-p (vec)
  "Return true if every character in vector VEC can be displayed."
  (let ((i (length vec)))
    (when (> i 0)
      (while (and (>= (setq i (1- i)) 0)
		  (whitespace-char-valid-p (aref vec i))))
      (< i 0))))


(defun whitespace-display-char-on ()
  "Turn on character display mapping."
  (when (and whitespace-display-mappings
	     (whitespace-style-mark-p))
    (let (vecs vec)
      ;; Remember whether a buffer has a local display table.
      (unless whitespace-display-table-was-local
	(setq whitespace-display-table-was-local t
	      whitespace-display-table
	      (copy-sequence buffer-display-table))
	;; Assure `buffer-display-table' is unique
	;; when two or more windows are visible.
	(setq buffer-display-table
	      (copy-sequence buffer-display-table)))
      (unless buffer-display-table
	(setq buffer-display-table (make-display-table)))
      (dolist (entry whitespace-display-mappings)
	;; check if it is to display this mark
	(when (memq (car entry) whitespace-style)
	  ;; Get a displayable mapping.
	  (setq vecs (cddr entry))
	  (while (and vecs
		      (not (whitespace-display-vector-p (car vecs))))
	    (setq vecs (cdr vecs)))
	  ;; Display a valid mapping.
	  (when vecs
	    (setq vec (copy-sequence (car vecs)))
	    ;; NEWLINE char
	    (when (and (eq (cadr entry) ?\n)
		       (memq 'newline whitespace-active-style))
	      ;; Only insert face bits on NEWLINE char mapping to avoid
	      ;; obstruction of other faces like TABs and (HARD) SPACEs
	      ;; faces, font-lock faces, etc.
	      (dotimes (i (length vec))
		(or (eq (aref vec i) ?\n)
		    (aset vec i
			  (make-glyph-code (aref vec i)
					   whitespace-newline)))))
	    ;; Display mapping
	    (aset buffer-display-table (cadr entry) vec)))))))


(defun whitespace-display-char-off ()
  "Turn off character display mapping."
  (and whitespace-display-mappings
       (whitespace-style-mark-p)
       whitespace-display-table-was-local
       (setq whitespace-display-table-was-local nil
	     buffer-display-table whitespace-display-table)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Hook


(defun whitespace-action-when-on ()
  "Action to be taken always when local whitespace is turned on."
  (cond ((memq 'cleanup whitespace-action)
	 (whitespace-cleanup))
	((memq 'report-on-bogus whitespace-action)
	 (whitespace-report nil t))))


(defun whitespace-write-file-hook ()
  "Action to be taken when buffer is written.
It should be added buffer-locally to `write-file-functions'."
  (cond ((memq 'auto-cleanup whitespace-action)
	 (whitespace-cleanup))
	((memq 'abort-on-bogus whitespace-action)
	 (when (whitespace-report nil t)
	   (error "Abort write due to whitespace problems in %s"
		  (buffer-name)))))
  nil)					; continue hook processing


(defun whitespace-warn-read-only (msg)
  "Warn if buffer is read-only."
  (when (memq 'warn-if-read-only whitespace-action)
    (message "Can't %s: %s is read-only" msg (buffer-name))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun whitespace-unload-function ()
  "Unload the whitespace library."
  (global-whitespace-mode -1)
  ;; be sure all local whitespace mode is turned off
  (save-current-buffer
    (dolist (buf (buffer-list))
      (set-buffer buf)
      (whitespace-mode -1)))
  nil)					; continue standard unloading


(provide 'whitespace)


(run-hooks 'whitespace-load-hook)


;;; whitespace.el ends here
