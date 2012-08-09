;;; meta-mode.el --- major mode for editing Metafont or MetaPost sources -*- lexical-binding:t -*-

;; Copyright (C) 1997, 2001-2012  Free Software Foundation, Inc.

;; Author: Ulrik Vieth <vieth@thphy.uni-duesseldorf.de>
;; Version: 1.0
;; Keywords: Metafont, MetaPost, tex, languages

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

;; Description:
;;
;; This Emacs Lisp package provides a major mode for editing Metafont
;; or MetaPost sources.  It includes all the necessary code to set up
;; a major mode including an appropriate syntax table, keymap, and a
;; mode-specific pull-down menu.  It also provides a sophisticated set
;; of font-lock patterns, a fancy indentation function adapted from
;; AUCTeX's latex.el, and some basic mode-specific editing functions
;; such as functions to move to the beginning or end of the enclosing
;; environment, or to mark, re-indent, or comment-out environments.
;; On the other hand, it doesn't yet provide any functionality for
;; running Metafont or MetaPost in a shell buffer from within Emacs,
;; but such functionality might be added later, either as part of this
;; package or as a separate Emacs Lisp package.

;; Customization:
;;
;; Following the usual Emacs Lisp coding conventions, the major modes
;; defined in this package provide several hook variables to allow for
;; local customization when entering the modes.  In particular, there
;; is a `meta-common-mode-hook' which applies to both modes as well as
;; `metafont-mode-hook' and `metapost-mode-hook' which apply to the
;; individual modes.  In addition, there are several variables and
;; regexps controlling e.g. the behavior of the indentation function,
;; which may be customized via `edit-options'.  Please refer to the
;; docstrings in the code below for details.

;; Availability:
;;
;; As of this version 1.0, this package will be uploaded to CTAN
;; archives, where it shall find a permanent home, presumably in
;; tex-archive/support/emacs-modes.  It will also be submitted for
;; integration into the GNU Emacs distribution at that time.
;;
;; History:
;;
;; v 0.0 -- 1997/02/01  UV  Started writing meta-mode.el.
;; v 0.1 -- 1997/02/02  UV  Added preliminary set of font-lock patterns.
;; v 0.2 -- 1997/02/03  UV  Improved and debugged font-lock patterns.
;;                          Added indent-line-function for TAB.
;; v 0.3 -- 1997/02/17  UV  Improved font-lock patterns and syntax table.
;;                          Improved and debugged indentation function.
;; v 0.4 -- 1997/02/18  UV  Added functions to indent regions for M-C-q,
;;                          also added a preliminary mode-specific menu.
;; v 0.5 -- 1997/02/19  UV  Added functions to skip to next or previous
;;                          defun and to re-indent or comment-out defuns.
;; v 0.6 -- 1997/02/20  UV  More debugging, testing and clean-up.
;; v 0.7 -- 1997/02/22  UV  Use easymenu to define mode-specific menu.
;; v 0.8 -- 1997/02/24  UV  Added completion function for M-TAB.
;; v 0.9 -- 1997/03/08  UV  Added fill-paragraph function for comments.
;;                          Also fixed a few remaining font-lock problems.
;;                          Added meta-mode-load-hook to load meta-buf.el.
;; v 1.0 -- 1997/04/07  UV  Cleanup for official public release.
;;
;; Historical Footnote:
;;
;; This package was begun on February 1, 1997, exactly 20 years after
;; the genesis of TeX took place according to Don Knuth's own account
;; (cf.  ``The Errors of TeX'', reprinted in ``Literate Programming'',
;; Chapter 10, p. 249).  What better date could there be to choose?
;;


;;; Code:

(require 'easymenu)

(defgroup meta-font nil
  "Major mode for editing Metafont or MetaPost sources."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'languages)

;;; Fontification.

(defvar meta-font-lock-keywords
      (let ((input-keywords
             "\\(input\\|generate\\)")
            (begin-keywords
             (concat "\\(begin\\(char\\|fig\\|graph\\|logochar\\)\\|"
                     "\\cmchar\\|dcchar\\|ecchar\\)"))
            (end-keywords
             "\\(end\\(char\\|fig\\|graph\\)\\)")
            (macro-keywords-1
             "\\(def\\|let\\|mode_def\\|vardef\\)")
            (macro-keywords-2
             "\\(primarydef\\|secondarydef\\|tertiarydef\\)")
;(make-regexp
; '("expr" "suffix" "text" "primary" "secondary" "tertiary") t)
            (args-keywords
             (concat "\\(expr\\|primary\\|s\\(econdary\\|uffix\\)\\|"
                     "te\\(rtiary\\|xt\\)\\)"))
;(make-regexp
; '("boolean" "color" "numeric" "pair" "path" "pen" "picture"
;   "string" "transform" "newinternal") t)
            (type-keywords
             (concat "\\(boolean\\|color\\|n\\(ewinternal\\|umeric\\)\\|"
                     "p\\(a\\(ir\\|th\\)\\|en\\|icture\\)\\|string\\|"
                     "transform\\)"))
;(make-regexp
; '("for" "forever" "forsuffixes" "endfor"
;   "step" "until" "upto" "downto" "thru" "within"
;   "iff" "if" "elseif" "else" "fi" "exitif" "exitunless"
;   "let" "def" "vardef" "enddef" "mode_def"
;   "true" "false" "known" "unknown" "and" "or" "not"
;   "save" "interim" "inner" "outer" "relax"
;   "begingroup" "endgroup" "expandafter" "scantokens"
;   "generate" "input" "endinput" "end" "bye"
;   "message" "errmessage" "errhelp" "special" "numspecial"
;   "readstring" "readfrom" "write") t)
            (syntactic-keywords
             (concat "\\(and\\|b\\(egingroup\\|ye\\)\\|"
                     "d\\(ef\\|ownto\\)\\|e\\(lse\\(\\|if\\)"
                     "\\|nd\\(\\|def\\|for\\|group\\|input\\)"
                     "\\|rr\\(help\\|message\\)"
                     "\\|x\\(it\\(if\\|unless\\)\\|pandafter\\)\\)\\|"
                     "f\\(alse\\|i\\|or\\(\\|ever\\|suffixes\\)\\)\\|"
                     "generate\\|i\\(ff?\\|n\\(ner\\|put\\|terim\\)\\)\\|"
                     "known\\|let\\|m\\(essage\\|ode_def\\)\\|"
                     "n\\(ot\\|umspecial\\)\\|o\\(r\\|uter\\)\\|"
                     "re\\(ad\\(from\\|string\\)\\|lax\\)\\|"
                     "s\\(ave\\|cantokens\\|pecial\\|tep\\)\\|"
                     "t\\(hru\\|rue\\)\\|"
                     "u\\(n\\(known\\|til\\)\\|pto\\)\\|"
                     "vardef\\|w\\(ithin\\|rite\\)\\)"))
            )
        (list
         ;; embedded TeX code in btex ... etex
         (cons (concat "\\(btex\\|verbatimtex\\)"
                       "[ \t\f]+\\(.*\\)[ \t\f]+"
                       "\\(etex\\)")
               '((1 font-lock-keyword-face)
                 (2 font-lock-string-face)
                 (3 font-lock-keyword-face)))
         ;; unary macro definitions: def, vardef, let
         (cons (concat "\\<" macro-keywords-1 "\\>"
                       "[ \t\f]+\\(\\sw+\\|\\s_+\\|\\s.+\\)")
               '((1 font-lock-keyword-face)
                 (2 font-lock-function-name-face)))
         ;; binary macro definitions: <leveldef> x operator y
         (cons (concat "\\<" macro-keywords-2 "\\>"
                       "[ \t\f]+\\(\\sw+\\)"
                       "[ \t\f]*\\(\\sw+\\|\\s.+\\)"
                       "[ \t\f]*\\(\\sw+\\)")
               '((1 font-lock-keyword-face)
                 (2 font-lock-variable-name-face nil t)
                 (3 font-lock-function-name-face nil t)
                 (4 font-lock-variable-name-face nil t)))
         ;; variable declarations: numeric, pair, color, ...
         (cons (concat "\\<" type-keywords "\\>"
                       "\\([ \t\f]+\\(\\sw+\\)\\)*")
               '((1 font-lock-type-face)
                 (font-lock-match-meta-declaration-item-and-skip-to-next
                  (goto-char (match-end 1)) nil
                  (1 font-lock-variable-name-face nil t))))
         ;; argument declarations: expr, suffix, text, ...
         (cons (concat "\\<" args-keywords "\\>"
                       "\\([ \t\f]+\\(\\sw+\\|\\s_+\\)\\)*")
               '((1 font-lock-type-face)
                 (font-lock-match-meta-declaration-item-and-skip-to-next
                  (goto-char (match-end 1)) nil
                  (1 font-lock-variable-name-face nil t))))
         ;; special case of arguments: expr x of y
         (cons (concat "\\(expr\\)[ \t\f]+\\(\\sw+\\)"
                       "[ \t\f]+\\(of\\)[ \t\f]+\\(\\sw+\\)")
               '((1 font-lock-type-face)
                 (2 font-lock-variable-name-face)
                 (3 font-lock-keyword-face nil t)
                 (4 font-lock-variable-name-face nil t)))
         ;; syntactic keywords
         (cons (concat "\\<" syntactic-keywords "\\>")
               'font-lock-keyword-face)
         ;; beginchar, beginfig
         (cons (concat "\\<" begin-keywords "\\>")
               'font-lock-keyword-face)
         ;; endchar, endfig
         (cons (concat "\\<" end-keywords "\\>")
               'font-lock-keyword-face)
         ;; input, generate
         (cons (concat "\\<" input-keywords "\\>"
                       "[ \t\f]+\\(\\sw+\\)")
               '((1 font-lock-keyword-face)
                 (2 font-lock-constant-face)))
         ;; embedded Metafont/MetaPost code in comments
         (cons "|\\([^|]+\\)|"
               '(1 font-lock-constant-face t))
     ))
  "Default expressions to highlight in Metafont or MetaPost mode.")


(defun font-lock-match-meta-declaration-item-and-skip-to-next (limit)
  ;; Match and move over Metafont/MetaPost declaration item after point.
  ;;
  ;; The expected syntax of an item is either "word" or "symbol",
  ;; possibly ending with optional whitespace.  Everything following
  ;; the item (but belonging to it) is expected to by skipable by
  ;; `forward-sexp'.  The list of items is expected to be separated
  ;; by commas and terminated by semicolons or equals signs.
  ;;
  (if (looking-at "[ \t\f]*\\(\\sw+\\|\\s_+\\)")
      (save-match-data
        (condition-case nil
            (save-restriction
              ;; Restrict to end of line, currently guaranteed to be LIMIT.
              (narrow-to-region (point-min) limit)
              (goto-char (match-end 1))
              ;; Move over any item value, etc., to the next item.
              (while (not (looking-at "[ \t\f]*\\(\\(,\\)\\|;\\|=\\|$\\)"))
                (goto-char (or (scan-sexps (point) 1) (point-max))))
              (goto-char (match-end 2)))
          (error t)))))



;;; Completion.

;; The data used to prepare the following lists of primitives and
;; standard macros available in Metafont or MetaPost was extracted
;; from the original sources like this:
;;
;;   grep '^primitive' texk-7.0/web2c/{mf,mp}.web |\
;;   sed 's/primitive(\("[a-zA-Z]*"\).*/\1/' > {mf,mp}_prim.list
;;
;;   grep '\(let\|def\|vardef\|primarydef\|secondarydef\|tertiarydef\)'
;;     texmf/meta{font,post}/plain.{mf,mp} > {mf,mp}_plain.list

(defconst meta-common-primitives-list
  '("ASCII" "addto" "also" "and" "angle" "atleast" "batchmode"
    "begingroup" "boolean" "boundarychar" "char" "charcode" "chardp"
    "charexists" "charext" "charht" "charic" "charlist" "charwd"
    "contour" "controls" "cosd" "curl" "cycle" "day" "decimal" "def"
    "delimiters" "designsize" "directiontime" "doublepath" "dump" "else"
    "elseif" "end" "enddef" "endfor" "endgroup" "endinput" "errhelp"
    "errmessage" "errorstopmode" "everyjob" "exitif" "expandafter"
    "expr" "extensible" "false" "fi" "floor" "fontdimen" "fontmaking"
    "for" "forever" "forsuffixes" "headerbyte" "hex" "if" "inner"
    "input" "interim" "intersectiontimes" "jobname" "kern" "known"
    "length" "let" "ligtable" "makepath" "makepen" "message" "mexp"
    "mlog" "month" "newinternal" "nonstopmode" "normaldeviate" "not"
    "nullpen" "nullpicture" "numeric" "oct" "odd" "of" "or" "outer"
    "pair" "path" "pausing" "pen" "pencircle" "penoffset" "picture"
    "point" "postcontrol" "precontrol" "primary" "primarydef" "quote"
    "randomseed" "readstring" "reverse" "rotated" "save" "scaled"
    "scantokens" "scrollmode" "secondary" "secondarydef" "shifted"
    "shipout" "show" "showdependencies" "showstats" "showstopping"
    "showtoken" "showvariable" "sind" "skipto" "slanted" "special"
    "sqrt" "step" "str" "string" "subpath" "substring" "suffix"
    "tension" "tertiary" "tertiarydef" "text" "time" "to"
    "tracingcapsules" "tracingchoices" "tracingcommands"
    "tracingequations" "tracingmacros" "tracingonline" "tracingoutput"
    "tracingrestores" "tracingspecs" "tracingstats" "tracingtitles"
    "transform" "transformed" "true" "turningnumber" "uniformdeviate"
    "unknown" "until" "vardef" "warningcheck" "withpen" "xpart"
    "xscaled" "xxpart" "xypart" "year" "ypart" "yscaled" "yxpart"
    "yypart" "zscaled")
  "List of primitives common to Metafont and MetaPost.")

(defconst metafont-primitives-list
  '("at" "autorounding" "chardx" "chardy" "cull" "display"
    "dropping" "fillin" "from" "granularity" "hppp" "inwindow"
    "keeping" "numspecial" "openwindow" "proofing" "smoothing"
    "totalweight" "tracingedges" "tracingpens" "turningcheck" "vppp"
    "withweight" "xoffset" "yoffset")
  "List of primitives only defined in Metafont.")

(defconst metapost-primitives-list
  '("arclength" "arctime" "bluepart" "bounded" "btex" "clip"
    "clipped" "color" "dashed" "dashpart" "etex" "filled" "fontpart"
    "fontsize" "greenpart" "infont" "linecap" "linejoin" "llcorner"
    "lrcorner" "miterlimit" "mpxbreak" "pathpart" "penpart"
    "prologues" "readfrom" "redpart" "setbounds" "stroked" "textpart"
    "textual" "tracinglostchars" "truecorners" "ulcorner" "urcorner"
    "verbatimtex" "withcolor" "within" "write")
  "List of primitives only defined in MetaPost.")

(defconst meta-common-plain-macros-list
  '( "abs" "bot" "bye" "byte" "ceiling" "clear_pen_memory"
     "clearit" "clearpen" "clearxy" "counterclockwise" "cutdraw" "decr"
     "dir" "direction" "directionpoint" "div" "dotprod" "downto" "draw"
     "drawdot" "erase" "exitunless" "fill" "filldraw" "flex" "gobble"
     "hide" "incr" "interact" "interpath" "intersectionpoint" "inverse"
     "label" "labels" "lft" "loggingall" "magstep" "makelabel" "max"
     "min" "mod" "numtok" "penlabels" "penpos" "penstroke" "pickup"
     "range" "reflectedabout" "relax" "rotatedabout" "rotatedaround"
     "round" "rt" "savepen" "shipit" "softjoin" "solve" "stop"
     "superellipse" "takepower" "tensepath" "thru" "top" "tracingall"
     "tracingnone" "undraw" "undrawdot" "unfill" "unfilldraw"
     "unitvector" "upto" "whatever")
  "List of macros common to plain Metafont and MetaPost.")

(defconst metafont-plain-macros-list
  '("beginchar" "change_width" "culldraw" "cullit" "cutoff"
    "define_blacker_pixels" "define_corrected_pixels"
    "define_good_x_pixels" "define_good_y_pixels"
    "define_horizontal_corrected_pixels" "define_pixels"
    "define_whole_blacker_pixels" "define_whole_pixels"
    "define_whole_vertical_blacker_pixels"
    "define_whole_vertical_pixels" "endchar" "fix_units"
    "font_coding_scheme" "font_extra_space" "font_identifier"
    "font_normal_shrink" "font_normal_space" "font_normal_stretch"
    "font_quad" "font_size" "font_slant" "font_x_height" "gfcorners"
    "good.bot" "good.lft" "good.rt" "good.top" "good.x" "good.y"
    "grayfont" "hround" "imagerules" "italcorr" "labelfont"
    "lowres_fix" "makebox" "makegrid" "maketicks" "mode_lowres"
    "mode_proof" "mode_setup" "mode_smoke" "nodisplays" "notransforms"
    "openit" "penrazor" "pensquare" "proofoffset" "proofrule"
    "proofrulethickness" "screenchars" "screenrule" "screenstrokes"
    "showit" "slantfont" "smode" "titlefont" "vround")
  "List of macros only defined in plain Metafont.")

(defconst metapost-plain-macros-list
  '("arrowhead" "bbox" "beginfig" "buildcycle" "center" "cutafter"
    "cutbefore" "dashpattern" "dotlabel" "dotlabels" "drawarrow"
    "drawdblarrow" "drawoptions" "endfig" "image" "label" "off" "on"
    "thelabel")
  "List of macros only defined in plain MetaPost.")

(defconst metapost-graph-macros-list
  '("augment" "auto.x" "auto.y" "autogrid" "begingraph" "endgraph"
    "format" "frame" "gdata" "gdotlabel" "gdraw" "gdrawarrow"
    "gdrawdblarrow" "gfill" "glabel" "grid" "itick" "otick" "plot"
    "setcoords" "setrange")
  "List of macros only defined in MetaPost \"graph\" package.")

(defconst metapost-boxes-macros-list
  '("boxit" "boxjoin" "bpath" "circleit" "drawboxed" "drawboxes"
    "drawunboxed" "fixpos" "fixsize" "pic" "rboxit")
  "List of macros only defined in MetaPost \"boxes\" package.")


(defvar metafont-symbol-list
  (append meta-common-primitives-list
          metafont-primitives-list
          meta-common-plain-macros-list
          metafont-plain-macros-list)
  "List of known symbols to complete in Metafont mode.")

(defvar metapost-symbol-list
  (append meta-common-primitives-list
          metapost-primitives-list
          meta-common-plain-macros-list
          metapost-plain-macros-list
          metapost-graph-macros-list
          metapost-boxes-macros-list)
  "List of known symbols to complete in MetaPost mode.")


(defvar meta-symbol-list nil
  "List of known symbols to complete in Metafont or MetaPost mode.")

(defvar meta-symbol-changed nil
  "Flag indicating whether `meta-symbol-list' has been initialized.")

(defvar meta-complete-list nil
;  (list (list "\\<\\(\\sw+\\)" 1 'meta-symbol-list)
;        (list "" 'ispell-complete-word))
  "List of ways to perform completion in Metafont or MetaPost mode.

Each entry is a list with the following elements:
1. Regexp matching the preceding text.
2. A number indicating the subgroup in the regexp containing the text.
3. A function returning an alist of possible completions.
4. Text to append after a successful completion (if any).

Or alternatively:
1. Regexp matching the preceding text.
2. Function to do the actual completion.")


(defun meta-add-symbols (&rest entries)
  "Add entries to list of known symbols in Metafont or MetaPost mode."
  (if meta-symbol-changed
      (setq meta-symbol-list (cons entries meta-symbol-list))
    (setq meta-symbol-changed t)
    (setq meta-symbol-list (cons entries meta-symbol-list))))

(defun meta-symbol-list ()
  "Return value of list of known symbols in Metafont or MetaPost mode.
If the list was changed, sort the list and remove duplicates first."
  (if (not meta-symbol-changed)
      ()
    (setq meta-symbol-changed nil)
    (message "Preparing completion list...")
    ;; sort list of symbols
    (setq meta-symbol-list
          (sort (mapcar 'meta-listify (apply 'append meta-symbol-list))
                'meta-car-string-lessp))
    ;; remove duplicates
    (let ((entry meta-symbol-list))
      (while (and entry (cdr entry))
        (let ((this (car entry))
              (next (car (cdr entry))))
          (if (not (string-equal (car this) (car next)))
              (setq entry (cdr entry))
            (if (> (length next) (length this))
                (setcdr this (cdr next)))
            (setcdr entry (cdr (cdr entry)))))))
    (message "Preparing completion list... done"))
  meta-symbol-list)

(defun meta-listify (a)
  ;; utility function used in `meta-add-symbols'
  (if (listp a) a (list a)))

(defun meta-car-string-lessp (a b)
  ;; utility function used in `meta-add-symbols'
  (string-lessp (car a) (car b)))


(defun meta-completions-at-point ()
  (let ((list meta-complete-list)
        entry)
    (while list
      (setq entry (car list)
            list (cdr list))
      (if (looking-back (car entry) (max (point-min) (- (point) 200)))
          (setq list nil)))
    (if (numberp (nth 1 entry))
        (let* ((sub (nth 1 entry))
               (close (nth 3 entry))
               (begin (match-beginning sub))
               (end (match-end sub))
               (list (funcall (nth 2 entry))))
          (list
           begin end list
           :exit-function
           (unless (zerop (length close))
             (lambda (_s finished)
               (when (memq finished '(sole finished))
                 (if (looking-at (regexp-quote close))
                     (goto-char (match-end 0))
                   (insert close)))))))
      (nth 1 entry))))

(define-obsolete-function-alias 'meta-complete-symbol
  'completion-at-point "24.1")

;;; Indentation.

(defcustom meta-indent-level 2
  "Indentation of begin-end blocks in Metafont or MetaPost mode."
  :type 'integer
  :group 'meta-font)


(defcustom meta-left-comment-regexp "%%+"
  "Regexp matching comments that should be placed on the left margin."
  :type 'regexp
  :group 'meta-font)

(defcustom meta-right-comment-regexp nil
  "Regexp matching comments that should be placed on the right margin."
  :type '(choice regexp
		 (const :tag "None" nil))
  :group 'meta-font)

(defcustom meta-ignore-comment-regexp "%[^%]"
  "Regexp matching comments whose indentation should not be touched."
  :type 'regexp
  :group 'meta-font)


(defcustom meta-begin-environment-regexp
  (concat "\\(begin\\(char\\|fig\\|gr\\(aph\\|oup\\)\\|logochar\\)\\|"
          "def\\|for\\(\\|ever\\|suffixes\\)\\|if\\|mode_def\\|"
          "primarydef\\|secondarydef\\|tertiarydef\\|vardef\\)")
  "Regexp matching the beginning of environments to be indented."
  :type 'regexp
  :group 'meta-font)

(defcustom meta-end-environment-regexp
  (concat "\\(end\\(char\\|def\\|f\\(ig\\|or\\)\\|gr\\(aph\\|oup\\)\\)"
          "\\|fi\\)")
  "Regexp matching the end of environments to be indented."
  :type 'regexp
  :group 'meta-font)

(defcustom meta-within-environment-regexp
; (concat "\\(e\\(lse\\(\\|if\\)\\|xit\\(if\\|unless\\)\\)\\)")
  (concat "\\(else\\(\\|if\\)\\)")
  "Regexp matching keywords within environments not to be indented."
  :type 'regexp
  :group 'meta-font)


(defun meta-comment-indent ()
  "Return the indentation for a comment in Metafont or MetaPost mode."
  (if (and meta-left-comment-regexp
           (looking-at meta-left-comment-regexp))
      (current-column)
    (skip-chars-backward "\t\f ")
    (max (if (bolp) 0 (1+ (current-column)))
         comment-column)))

(defun meta-indent-line ()
  "Indent the line containing point as Metafont or MetaPost source."
  (interactive)
  (let ((indent (meta-indent-calculate)))
    (if (/= (current-indentation) indent)
        (save-excursion
          (delete-region (line-beginning-position)
                         (progn (back-to-indentation) (point)))
          (indent-to indent)))
    (if (< (current-column) indent)
        (back-to-indentation))))

(defun meta-indent-calculate ()
  "Return the indentation of current line of Metafont or MetaPost source."
  ;; Indentation within strings is not considered as Meta* don't allow multi
  ;; line strings.
  (save-excursion
    (back-to-indentation)
    (cond
     ;; Comments to the left margin.
     ((and meta-left-comment-regexp
           (looking-at meta-left-comment-regexp))
      0)
     ;; Comments to the right margin.
     ((and meta-right-comment-regexp
           (looking-at meta-right-comment-regexp))
      comment-column)
     ;; Comments best left alone.
     ((and meta-ignore-comment-regexp
           (looking-at meta-ignore-comment-regexp))
      (current-indentation))
     ;; Beginning of buffer.
     ((eq (point-at-bol) (point-min))
      0)
     ;; Backindent at end of environments.
     ((meta-indent-looking-at-code
       (concat "\\<" meta-end-environment-regexp "\\>"))
      (- (meta-indent-current-indentation) meta-indent-level))
     ;; Backindent at keywords within environments.
     ((meta-indent-looking-at-code
       (concat "\\<" meta-within-environment-regexp "\\>"))
      (- (meta-indent-current-indentation) meta-indent-level))
     (t (meta-indent-current-indentation)))))

(defun meta-indent-in-string-p ()
  "Tell if the point is in a string."
  (or (nth 3 (syntax-ppss))
      (eq (get-text-property (point) 'face) font-lock-string-face)))

(defun meta-indent-looking-at-code (regexp)
  "Same as `looking-at' but checks that the point is not in a string."
  (unless (meta-indent-in-string-p)
    (looking-at regexp)))

(defun meta-indent-previous-line ()
  "Go to the previous line of code, skipping comments."
  (skip-chars-backward "\n\t\f ")
  (move-to-column (current-indentation))
  ;; Ignore comments.
  (while (and (looking-at comment-start) (not (bobp)))
    (skip-chars-backward "\n\t\f ")
    (when (not (bobp))
      (move-to-column (current-indentation)))))

(defun meta-indent-unfinished-line ()
  "Tell if the current line of code ends with an unfinished expression."
  (save-excursion
    (end-of-line)
    ;; Skip backward the comments.
    (let ((point-not-in-string (point)))
      (while (search-backward comment-start (point-at-bol) t)
	(unless (meta-indent-in-string-p)
	  (setq point-not-in-string (point))))
      (goto-char point-not-in-string))
    ;; Search for the end of the previous expression.
    (if (search-backward ";" (point-at-bol) t)
	(progn (while (and (meta-indent-in-string-p)
			   (search-backward ";" (point-at-bol) t)))
	       (if (= (char-after) ?\;)
		   (forward-char)
		 (beginning-of-line)))
      (beginning-of-line))
    ;; See if the last statement of the line is environment-related,
    ;; or exists at all.
    (if (meta-indent-looking-at-code
	 (concat "[ \t\f]*\\($\\|" (regexp-quote comment-start)
		 "\\|\\<" meta-end-environment-regexp "\\>"
		 "\\|\\<" meta-begin-environment-regexp "\\>"
		 "\\|\\<" meta-within-environment-regexp "\\>\\)"))
	nil
      t)))

(defun meta-indent-current-indentation ()
  "Return the indentation wanted for the current line of code."
  (+ (meta-indent-current-nesting)
     (if (save-excursion
	   (back-to-indentation)
	   (and (not (looking-at (concat "\\<" meta-end-environment-regexp "\\>"
					 "\\|\\<" meta-within-environment-regexp "\\>")))
		(progn (meta-indent-previous-line)
		       (meta-indent-unfinished-line))))
	 meta-indent-level
       0)))

(defun meta-indent-current-nesting ()
  "Return the indentation according to the nearest environment keyword."
  (save-excursion
    (save-restriction
      (widen)
      (back-to-indentation)
      (let ((to-add 0))
	;; If we found some environment marker backward...
	(if (catch 'found
	      (while (re-search-backward
		      (concat "(\\|)\\|\\<" meta-end-environment-regexp "\\>"
			      "\\|\\<" meta-begin-environment-regexp "\\>"
			      "\\|\\<" meta-within-environment-regexp "\\>")
		      nil t)
		;; If we aren't in a string or in a comment, we've found something.
		(unless (or (meta-indent-in-string-p)
			    (nth 4 (syntax-ppss)))
		  (cond ((= (char-after) ?\()
			 (setq to-add (+ to-add meta-indent-level)))
			((= (char-after) ?\))
			 (setq to-add (- to-add meta-indent-level)))
			(t (throw 'found t))))))
	    (progn
	      ;; ... then use it to compute the current indentation.
	      (back-to-indentation)
	      (+ to-add (current-indentation) (meta-indent-level-count)
		 ;; Compensate for backindent of end and within keywords.
		 (if (meta-indent-looking-at-code
		      (concat "\\<" meta-end-environment-regexp "\\>\\|"
			      "\\<" meta-within-environment-regexp "\\>"))
		     meta-indent-level
		   ;; Compensate for unfinished line.
		   (if (save-excursion
			 (meta-indent-previous-line)
			 (meta-indent-unfinished-line))
		       (- meta-indent-level)
		     0))))
	  0)))))

(defun meta-indent-level-count ()
  "Count indentation change for begin-end commands in the current line."
  (save-excursion
    (save-restriction
      (let ((count 0))
        (narrow-to-region
         (point) (save-excursion
                   (re-search-forward "[^\\\\\"]%\\|\n\\|\\'" nil t)
                   (backward-char) (point)))
        (while (re-search-forward "\\<\\sw+\\>\\|(\\|)" nil t)
          (save-excursion
            (goto-char (match-beginning 0))
            (cond
             ;; Count number of begin-end keywords within line.
             ((meta-indent-looking-at-code
               (concat "\\<" meta-begin-environment-regexp "\\>"))
              (setq count (+ count meta-indent-level)))
             ((meta-indent-looking-at-code
               (concat "\\<" meta-end-environment-regexp "\\>"))
              (setq count (- count meta-indent-level))))))
        count))))



;;; Editing commands.

(defcustom meta-begin-defun-regexp
  (concat "\\(begin\\(char\\|fig\\|logochar\\)\\|def\\|mode_def\\|"
          "primarydef\\|secondarydef\\|tertiarydef\\|vardef\\)")
  "Regexp matching beginning of defuns in Metafont or MetaPost mode."
  :type 'regexp
  :group 'meta-font)

(defcustom meta-end-defun-regexp
  (concat "\\(end\\(char\\|def\\|fig\\)\\)")
  "Regexp matching the end of defuns in Metafont or MetaPost mode."
  :type 'regexp
  :group 'meta-font)


(defun meta-beginning-of-defun (&optional arg)
  "Move backward to beginning of a defun in Metafont or MetaPost code.
With numeric argument, do it that many times.
Negative arg -N means move forward to Nth following beginning of defun.
Returns t unless search stops due to beginning or end of buffer."
  (interactive "p")
  (if (or (null arg) (= 0 arg)) (setq arg 1))
  (and arg (< arg 0) (not (eobp)) (forward-char 1))
  (and (re-search-backward
        (concat "\\<" meta-begin-defun-regexp "\\>") nil t arg)
       (progn (goto-char (match-beginning 0))
              (skip-chars-backward "%")
              (skip-chars-backward " \t\f") t)))

(defun meta-end-of-defun (&optional arg)
  "Move forward to end of a defun in Metafont or MetaPost code.
With numeric argument, do it that many times.
Negative argument -N means move back to Nth preceding end of defun.
Returns t unless search stops due to beginning or end of buffer."
  (interactive "p")
  (if (or (null arg) (= 0 arg)) (setq arg 1))
  (and (< arg 0) (not (bobp)) (forward-line -1))
  (and (re-search-forward
        (concat "\\<" meta-end-defun-regexp "\\>") nil t arg)
       (progn (goto-char (match-end 0))
              (skip-chars-forward ";")
              (skip-chars-forward " \t\f")
              (if (looking-at "\n") (forward-line 1)) t)))


(defun meta-comment-region (beg end &optional arg)
  "Comment out active region as Metafont or MetaPost source."
  (interactive "r")
  (comment-region beg end arg))

(defun meta-uncomment-region (beg end)
  "Uncomment active region as Metafont or MetaPost source."
  (interactive "r")
  (comment-region beg end -1))

(defun meta-comment-defun (&optional arg)
  "Comment out current environment as Metafont or MetaPost source.
With prefix argument, uncomment the environment.
The environment used is the one that contains point or follows point."
  (interactive "P")
  (save-excursion
    (let* ((end (if (meta-end-of-defun) (point) (point-max)))
           (beg (if (meta-beginning-of-defun) (point) (point-min))))
      (comment-region beg end arg))))

(defun meta-uncomment-defun ()
  "Uncomment current environment as Metafont or MetaPost source."
  (interactive)
  (meta-comment-defun -1))


(defun meta-indent-region (beg end)
  "Indent the active region as Metafont or MetaPost source."
  (interactive "r")
  (indent-region beg end nil))

(defun meta-indent-buffer ()
  "Indent the whole buffer contents as Metafont or MetaPost source."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun meta-indent-defun ()
  "Indent the current environment as Metafont or MetaPost source.
The environment indented is the one that contains point or follows point."
  (interactive)
  (save-excursion
    (let* ((end (if (meta-end-of-defun) (point) (point-max)))
           (beg (if (meta-beginning-of-defun) (point) (point-min))))
      (indent-region beg end nil))))


(defun meta-mark-defun ()
  "Put mark at end of the environment, point at the beginning.
The environment marked is the one that contains point or follows point."
  (interactive)
  (push-mark (point))
  (meta-end-of-defun)
  (push-mark (point) nil t)
  (meta-beginning-of-defun))



;;; Syntax table, keymap and menu.

(define-abbrev-table 'meta-mode-abbrev-table ()
  "Abbrev table used in Metafont or MetaPost mode.")

(defvar meta-common-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; underscores are word constituents
    (modify-syntax-entry ?_  "w"  st)
    ;; miscellaneous non-word symbols
    (modify-syntax-entry ?#  "_"  st)
    (modify-syntax-entry ?@  "_"  st)
    (modify-syntax-entry ?$  "_"  st)
    (modify-syntax-entry ??  "_"  st)
    (modify-syntax-entry ?!  "_"  st)
    ;; binary operators
    (modify-syntax-entry ?&  "."  st)
    (modify-syntax-entry ?+  "."  st)
    (modify-syntax-entry ?-  "."  st)
    (modify-syntax-entry ?/  "."  st)
    (modify-syntax-entry ?*  "."  st)
    (modify-syntax-entry ?.  "."  st)
    (modify-syntax-entry ?:  "."  st)
    (modify-syntax-entry ?=  "."  st)
    (modify-syntax-entry ?<  "."  st)
    (modify-syntax-entry ?>  "."  st)
    (modify-syntax-entry ?|  "."  st)
    ;; opening and closing delimiters
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)
    ;; comment character
    (modify-syntax-entry ?%  "<"  st)
    (modify-syntax-entry ?\n ">"  st)
    ;; escape character, needed for embedded TeX code
    (modify-syntax-entry ?\\ "\\" st)
    st)
  "Syntax table used in Metafont or MetaPost mode.")

(defvar meta-common-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Comment Paragraphs:
    ;; (define-key map "\M-a"      'backward-sentence)
    ;; (define-key map "\M-e"      'forward-sentence)
    ;; (define-key map "\M-h"      'mark-paragraph)
    ;; (define-key map "\M-q"      'fill-paragraph)
    ;; Navigation:
    (define-key map "\M-\C-a"   'meta-beginning-of-defun)
    (define-key map "\M-\C-e"   'meta-end-of-defun)
    (define-key map "\M-\C-h"   'meta-mark-defun)
    ;; Indentation:
    (define-key map "\M-\C-q"   'meta-indent-defun)
    (define-key map "\C-c\C-qe" 'meta-indent-defun)
    (define-key map "\C-c\C-qr" 'meta-indent-region)
    (define-key map "\C-c\C-qb" 'meta-indent-buffer)
    ;; Commenting Out:
    (define-key map "\C-c%"     'meta-comment-defun)
    ;; (define-key map "\C-uC-c%"  'meta-uncomment-defun)
    (define-key map "\C-c;"     'meta-comment-region)
    (define-key map "\C-c:"     'meta-uncomment-region)
    ;; Symbol Completion:
    (define-key map "\M-\t"     'completion-at-point)
    ;; Shell Commands:
    ;; (define-key map "\C-c\C-c"  'meta-command-file)
    ;; (define-key map "\C-c\C-k"  'meta-kill-job)
    ;; (define-key map "\C-c\C-l"  'meta-recenter-output)
    map)
  "Keymap used in Metafont or MetaPost mode.")
(define-obsolete-variable-alias 'meta-mode-map 'meta-common-mode-map "24.1")

(easy-menu-define
 meta-mode-menu meta-common-mode-map
 "Menu used in Metafont or MetaPost mode."
 (list "Meta"
       ["Forward Environment"           meta-beginning-of-defun t]
       ["Backward Environment"          meta-end-of-defun t]
       "--"
       ["Indent Line"                   meta-indent-line t]
       ["Indent Environment"            meta-indent-defun t]
       ["Indent Region"                 meta-indent-region
        :active (meta-mark-active)]
       ["Indent Buffer"                 meta-indent-buffer t]
       "--"
       ["Comment Out Environment"       meta-comment-defun t]
       ["Uncomment Environment"         meta-uncomment-defun t]
       ["Comment Out Region"            meta-comment-region
        :active (meta-mark-active)]
       ["Uncomment Region"              meta-uncomment-region
        :active (meta-mark-active)]
       "--"
       ["Complete Symbol"               completion-at-point t]
;      "--"
;      ["Command on Buffer"             meta-command-file t]
;      ["Kill Job"                      meta-kill-job t]
;      ["Recenter Output Buffer"        meta-recenter-output-buffer t]
       ))

;; Compatibility: XEmacs doesn't have the  `mark-active' variable.
(defun meta-mark-active ()
  "Return whether the mark and region are currently active in this buffer."
  (if (boundp 'mark-active) mark-active (mark)))



;;; Hook variables.

(defcustom meta-mode-load-hook nil
  "Hook evaluated when first loading Metafont or MetaPost mode."
  :type 'hook
  :group 'meta-font)

(defcustom meta-common-mode-hook nil
  "Hook evaluated by both `metafont-mode' and `metapost-mode'."
  :type 'hook
  :group 'meta-font)

(defcustom metafont-mode-hook nil
  "Hook evaluated by `metafont-mode' after `meta-common-mode-hook'."
  :type 'hook
  :group 'meta-font)
(defcustom metapost-mode-hook nil
  "Hook evaluated by `metapost-mode' after `meta-common-mode-hook'."
  :type 'hook
  :group 'meta-font)



;;; Initialization.

(define-derived-mode meta-common-mode prog-mode "-Meta-common-"
  "Common initialization for Metafont or MetaPost mode."
  :abbrev-table meta-mode-abbrev-table
  (set (make-local-variable 'paragraph-start)
       (concat page-delimiter "\\|$"))
  (set (make-local-variable 'paragraph-separate)
       (concat page-delimiter "\\|$"))

  (set (make-local-variable 'paragraph-ignore-fill-prefix) t)

  (set (make-local-variable 'comment-start-skip) "%+[ \t\f]*")
  (set (make-local-variable 'comment-start) "%")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-multi-line) nil)

  ;; We use `back-to-indentation' but \f is no indentation sign.
  (modify-syntax-entry ?\f "_   ")

  (set (make-local-variable 'parse-sexp-ignore-comments) t)

  (add-hook 'completion-at-point-functions #'meta-completions-at-point nil t)
  (set (make-local-variable 'comment-indent-function) #'meta-comment-indent)
  (set (make-local-variable 'indent-line-function) #'meta-indent-line)
  ;; No need to define a mode-specific 'indent-region-function.
  ;; Simply use the generic 'indent-region and 'comment-region.

  ;; Set defaults for font-lock mode.
  (set (make-local-variable 'font-lock-defaults)
       '(meta-font-lock-keywords
         nil nil ((?_ . "w")) nil
         (font-lock-comment-start-regexp . "%")))

  ;; Activate syntax table, keymap and menu.
  (easy-menu-add meta-mode-menu))


;;;###autoload
(define-derived-mode metafont-mode meta-common-mode "Metafont"
  "Major mode for editing Metafont sources."
  ;; Set defaults for completion function.
  (set (make-local-variable 'meta-symbol-list) nil)
  (set (make-local-variable 'meta-symbol-changed) nil)
  (apply 'meta-add-symbols metafont-symbol-list)
  (set (make-local-variable 'meta-complete-list)
        (list (list "\\<\\(\\sw+\\)" 1 'meta-symbol-list)
              (list "" 'ispell-complete-word))))

;;;###autoload
(define-derived-mode metapost-mode meta-common-mode "MetaPost"
  "Major mode for editing MetaPost sources."
  ;; Set defaults for completion function.
  (set (make-local-variable 'meta-symbol-list) nil)
  (set (make-local-variable 'meta-symbol-changed) nil)
  (apply 'meta-add-symbols metapost-symbol-list)
  (set (make-local-variable 'meta-complete-list)
        (list (list "\\<\\(\\sw+\\)" 1 'meta-symbol-list)
              (list "" 'ispell-complete-word))))


;;; Just in case ...

(provide 'meta-mode)
(run-hooks 'meta-mode-load-hook)

;;; meta-mode.el ends here
