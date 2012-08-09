;;; htmlfontify.el --- htmlize a buffer/source tree with optional hyperlinks

;; Copyright (C) 2002-2003, 2009-2012 Free Software Foundation, Inc.

;; Emacs Lisp Archive Entry
;; Package: htmlfontify
;; Filename: htmlfontify.el
;; Version: 0.21
;; Keywords: html, hypermedia, markup, etags
;; Author: Vivek Dasmohapatra <vivek@etla.org>
;; Maintainer: Vivek Dasmohapatra <vivek@etla.org>
;; Created: 2002-01-05
;; Description: htmlize a buffer/source tree with optional hyperlinks
;; URL: http://rtfm.etla.org/emacs/htmlfontify/
;; Compatibility: Emacs23, Emacs22
;; Incompatibility: Emacs19, Emacs20, Emacs21
;; Last Updated: Thu 2009-11-19 01:31:21 +0000
;; Version: 0.21

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I have made some changes to make it work for Emacs 22.   A lot of
;; small bug fixes related to the format of text and overlay
;; properties (which might have changed since the beginning of 2003
;; when this file was originally written).
;;
;; The function `hfy-face-at' currently carries much of the burden of
;; my lacking understanding of the formats mentioned above and should
;; need some knowledgeable help.
;;
;; Another thing that maybe could be fixed is that overlay background
;; colors which are now only seen where there is text (in the XHTML
;; output).  A bit of CSS tweaking is necessary there.
;;
;; The face 'default has a value :background "SystemWindow" for the
;; background color.   There is no explicit notion that this should be
;; considered transparent, but I have assumed that it could be handled
;; like if it was here.  (I am unsure that background and foreground
;; priorities are handled ok, but it looks ok in my tests now.)
;;
;; 2007-12-27 Lennart Borgman
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Here's some elisp code to html-pretty-print an Emacs buffer, preserving
;; the Emacs syntax/whatever highlighting.  It also knows how to drive etags
;; (exuberant-ctags or Emacs etags) and hyperlink the code according
;; to its (etags') output.

;; NOTE: Currently the hyperlinking code only knows how to drive GNU find
;; and the exuberant and GNU variants of etags : I do not know of any other
;; etags variants, but mechanisms have been provided to allow htmlfontify
;; to be taught how to drive them.  As long as your version of find has
;; the -path test and is reasonably sane, you should be fine.

;; A sample of the htmlfontified / hyperlinked output of this module can be
;; found at http://rtfm.etla.org/sql/dbishell/src/ - it's not perfect, but
;; it's a hell of a lot faster and more thorough than I could hope to be
;; doing this by hand.

;; some user / horrified onlooker comments:
;; What? No! There's something deeply wrong here...   (R. Shufflebotham)
;; You're a freak.                                    (D. Silverstone)
;; Aren't we giving you enough to do?                 (J. Busuttil)
;; You're almost as messed up as Lexx is!             (N. Graves-Morris)

;;; History:
;; Changes: moved to changelog (CHANGES) file.

;;; Code:
(eval-when-compile (require 'cl))
(require 'faces)
;;  (`facep' `face-attr-construct' `x-color-values' `color-values' `face-name')
(require 'custom)
;;  (`defgroup' `defcustom')
(require 'font-lock)
;;  (`font-lock-fontify-region')
(require 'cus-edit)

(defconst htmlfontify-version 0.21)

(defconst hfy-meta-tags
  (format "<meta name=\"generator\" content=\"emacs %s; htmlfontify %0.2f\" />"
          emacs-version htmlfontify-version)
  "The generator meta tag for this version of htmlfontify.")

(defconst htmlfontify-manual "Htmlfontify Manual"
  "Copy and convert buffers and files to HTML, adding hyperlinks between files
\(driven by etags) if requested.
\nInteractive functions:
  `htmlfontify-buffer'
  `htmlfontify-run-etags'
  `htmlfontify-copy-and-link-dir'
  `htmlfontify-load-rgb-file'
  `htmlfontify-unload-rgb-file'\n
In order to:\n
fontify a file you have open:           \\[htmlfontify-buffer]
prepare the etags map for a directory:  \\[htmlfontify-run-etags]
copy a directory, fontifying as you go: \\[htmlfontify-copy-and-link-dir]\n
The following might be useful when running non-windowed or in batch mode:
\(note that they shouldn't be necessary - we have a built in map)\n
load an X11 style rgb.txt file:         \\[htmlfontify-load-rgb-file]
unload the current rgb.txt file:        \\[htmlfontify-unload-rgb-file]\n
And here's a programmatic example:\n
\(defun rtfm-build-page-header (file style)
  (format \"#define  TEMPLATE red+black.html
#define  DEBUG    1
#include <build/menu-dirlist|>\\n
html-css-url := /css/red+black.css
title        := rtfm.etla.org ( %s / src/%s )
bodytag      :=
head         <=STYLESHEET;\\n
%s
STYLESHEET
main-title   := rtfm / %s / src/%s\\n
main-content <=MAIN_CONTENT;\\n\" rtfm-section file style rtfm-section file))

\(defun rtfm-build-page-footer (file) \"\\nMAIN_CONTENT\\n\")

\(defun rtfm-build-source-docs (section srcdir destdir)
  (interactive
   \"s section[eg- emacs / p4-blame]:\\nD source-dir: \\nD output-dir: \")
  (require 'htmlfontify)
  (hfy-load-tags-cache srcdir)
  (let ((hfy-page-header  'rtfm-build-page-header)
        (hfy-page-footer  'rtfm-build-page-footer)
        (rtfm-section                     section)
        (hfy-index-file                   \"index\"))
    (htmlfontify-run-etags srcdir)
    (htmlfontify-copy-and-link-dir srcdir destdir \".src\" \".html\")))")

(defgroup htmlfontify nil
  "Convert buffers and files to HTML."
  :group  'applications
  :link '(variable-link htmlfontify-manual)
  :prefix "hfy-")

(defcustom hfy-page-header 'hfy-default-header
  "Function called to build the header of the HTML source.
This is called with two arguments (the filename relative to the top
level source directory being etag'd and fontified), and a string containing
the <style>...</style> text to embed in the document.
It should return a string that will be used as the header for the
htmlfontified version of the source file.\n
See also `hfy-page-footer'."
  :group 'htmlfontify
  ;; FIXME: Why place such a :tag everywhere?  Isn't it imposing your
  ;; own Custom preference on your users?  --Stef
  :tag   "page-header"
  :type  '(function))

(defcustom hfy-split-index nil
  "Whether or not to split the index `hfy-index-file' alphabetically.
If non-nil, the index is split on the first letter of each tag.
Useful when the index would otherwise be large and take
a long time to render or be difficult to navigate."
  :group 'htmlfontify
  :tag   "split-index"
  :type  '(boolean))

(defcustom hfy-page-footer 'hfy-default-footer
  "As `hfy-page-header', but generates the output footer.
It takes only one argument, the filename."
  :group 'htmlfontify
  :tag   "page-footer"
  :type  '(function))

(defcustom hfy-extn ".html"
  "File extension used for output files."
  :group 'htmlfontify
  :tag   "extension"
  :type  '(string))

(defcustom hfy-src-doc-link-style "text-decoration: underline;"
  "String to add to the '<style> a' variant of an htmlfontify CSS class."
  :group 'htmlfontify
  :tag   "src-doc-link-style"
  :type  '(string))

(defcustom hfy-src-doc-link-unstyle " text-decoration: none;"
  "Regex to remove from the <style> a variant of an htmlfontify CSS class."
  :group 'htmlfontify
  :tag   "src-doc-link-unstyle"
  :type  '(string))

(defcustom hfy-link-extn nil
  "File extension used for href links.
Useful where the htmlfontify output files are going to be processed
again, with a resulting change in file extension.  If nil, then any
code using this should fall back to `hfy-extn'."
  :group 'htmlfontify
  :tag   "link-extension"
  :type  '(choice string (const nil)))

(defcustom hfy-link-style-fun 'hfy-link-style-string
  "Function to customize the appearance of hyperlinks.
Set this to a function, which will be called with one argument
\(a \"{ foo: bar; ...}\" CSS style-string) - it should return a copy of
its argument, altered so as to make any changes you want made for text which
is a hyperlink, in addition to being in the class to which that style would
normally be applied."
  :group 'htmlfontify
  :tag   "link-style-function"
  :type  '(function))

(defcustom hfy-index-file "hfy-index"
  "Name (sans extension) of the tag definition index file produced during
fontification-and-hyperlinking."
  :group 'htmlfontify
  :tag   "index-file"
  :type  '(string))

(defcustom hfy-instance-file "hfy-instance"
  "Name (sans extension) of the tag usage index file produced during
fontification-and-hyperlinking."
  :group 'htmlfontify
  :tag   "instance-file"
  :type  '(string))

(defcustom hfy-html-quote-regex "\\([<\"&>]\\)"
  "Regex to match (with a single back-reference per match) strings in HTML
which should be quoted with `hfy-html-quote' (and `hfy-html-quote-map')
to make them safe."
  :group 'htmlfontify
  :tag   "html-quote-regex"
  :type  '(regexp))

(define-obsolete-variable-alias 'hfy-init-kludge-hooks 'hfy-init-kludge-hook
  "23.2")
(defcustom hfy-init-kludge-hook '(hfy-kludge-cperl-mode)
  "List of functions to call when starting `htmlfontify-buffer' to do any
kludging necessary to get highlighting modes to behave as you want, even
when not running under a window system."
  :group 'htmlfontify
  :tag   "init-kludge-hooks"
  :type  '(hook))

(defcustom hfy-post-html-hooks nil
  "List of functions to call after creating and filling the HTML buffer.
These functions will be called with the HTML buffer as the current buffer."
  :group   'htmlfontify
  :tag     "post-html-hooks"
  :options '(set-auto-mode)
  :type    '(hook))

(defcustom hfy-default-face-def nil
  "Fallback `defface' specification for the face 'default, used when
`hfy-display-class' has been set (the normal htmlfontify way of extracting
potentially non-current face information doesn't necessarily work for
'default).\n
Example: I customize this to:\n
\((t :background \"black\" :foreground \"white\" :family \"misc-fixed\"))"
  :group   'htmlfontify
  :tag     "default-face-definition"
  :type    '(alist))

(defcustom hfy-etag-regex (concat ".*"
                                  "\x7f" "\\([^\x01\n]+\\)"
                                  "\x01" "\\([0-9]+\\)"
                                  ","    "\\([0-9]+\\)$"
                                  "\\|"  ".*\x7f[0-9]+,[0-9]+$")
  "Regex used to parse an etags entry: must have 3 subexps, corresponding,
in order, to:\n
   1 - The tag
   2 - The line
   3 - The char (point) at which the tag occurs."
  :group 'htmlfontify
  :tag   "etag-regex"
  :type  '(regexp))

(defcustom hfy-html-quote-map '(("\"" "&quot;")
                                ("<"  "&lt;"  )
                                ("&"  "&amp;" )
                                (">"  "&gt;"  ))
  "Alist of char -> entity mappings used to make the text HTML-safe."
  :group 'htmlfontify
  :tag   "html-quote-map"
  :type  '(alist :key-type (string)))
(defconst hfy-e2x-etags-cmd "for src in `find . -type f`;
do
  ETAGS=%s;
  case ${src} in
    *.ad[absm]|*.[CFHMSacfhlmpsty]|*.def|*.in[cs]|*.s[as]|*.src|*.cc|\\
    *.hh|*.[chy]++|*.[ch]pp|*.[chy]xx|*.pdb|*.[ch]s|*.[Cc][Oo][Bb]|\\
    *.[eh]rl|*.f90|*.for|*.java|*.[cem]l|*.clisp|*.lisp|*.[Ll][Ss][Pp]|\\
    [Mm]akefile*|*.pas|*.[Pp][LlMm]|*.psw|*.lm|*.pc|*.prolog|*.oak|\\
    *.p[sy]|*.sch|*.scheme|*.[Ss][Cc][Mm]|*.[Ss][Mm]|*.bib|*.cl[os]|\\
    *.ltx|*.sty|*.TeX|*.tex|*.texi|*.texinfo|*.txi|*.x[bp]m|*.yy|\\
    *.[Ss][Qq][Ll])
          ${ETAGS} -o- ${src};
          ;;
      *)
          FTYPE=`file ${src}`;
          case ${FTYPE} in
              *script*text*)
                  ${ETAGS} -o- ${src};
                  ;;
              *text*)
                  SHEBANG=`head -n1 ${src} | grep '#!' -c`;
                  if [ ${SHEBANG} -eq 1 ];
                  then
                      ${ETAGS} -o- ${src};
                  fi;
                  ;;
          esac;
          ;;
  esac;
done;")

(defconst hfy-etags-cmd-alist-default
  `(("emacs etags"     . ,hfy-e2x-etags-cmd)
    ("exuberant ctags" . "%s -R -f -"   )))

(defcustom hfy-etags-cmd-alist
  hfy-etags-cmd-alist-default
  "Alist of possible shell commands that will generate etags output that
`htmlfontify' can use.  '%s' will be replaced by `hfy-etags-bin'."
  :group 'htmlfontify
  :tag   "etags-cmd-alist"
  :type  '(alist :key-type (string) :value-type (string)))

(defcustom hfy-etags-bin "etags"
  "Location of etags binary (we begin by assuming it's in your path).\n
Note that if etags is not in your path, you will need to alter the shell
commands in `hfy-etags-cmd-alist'."
  :group 'htmlfontify
  :tag   "etags-bin"
  :type  '(file))

(defcustom hfy-shell-file-name "/bin/sh"
  "Shell (Bourne or compatible) to invoke for complex shell operations."
  :group 'htmlfontify
  :tag   "shell-file-name"
  :type  '(file))

(defcustom hfy-ignored-properties '(read-only
                                    intangible
                                    modification-hooks
                                    insert-in-front-hooks
                                    insert-behind-hooks
                                    point-entered
                                    point-left)
  "Properties to omit when copying a fontified buffer for HTML transformation."
  :group 'htmlfontify
  :tag   "ignored-properties"
  :type '(repeat symbol))

(defun hfy-which-etags ()
  "Return a string indicating which flavor of etags we are using."
  (let ((v (shell-command-to-string (concat hfy-etags-bin " --version"))))
    (cond ((string-match "exube" v) "exuberant ctags")
          ((string-match "GNU E" v) "emacs etags"    )) ))

(defcustom hfy-etags-cmd
  ;; We used to wrap this in a `eval-and-compile', but:
  ;; - it had no effect because this expression was not seen by the
  ;;   byte-compiler (defcustom used to quote this argument).
  ;; - it signals an error (`hfy-which-etags' is not defined at compile-time).
  ;; - we want this auto-detection to reflect the system on which Emacs is run
  ;;   rather than the one on which it's compiled.
  (cdr (assoc (hfy-which-etags) hfy-etags-cmd-alist))
  "The etags equivalent command to run in a source directory to generate a tags
file for the whole source tree from there on down.  The command should emit
the etags output on stdout.\n
Two canned commands are provided - they drive Emacs's etags and
exuberant-ctags' etags respectively."
  :group 'htmlfontify
  :tag   "etags-command"
  :type (let ((clist (list '(string))))
          (dolist (C hfy-etags-cmd-alist)
            (push (list 'const :tag (car C) (cdr C)) clist))
          (cons 'choice clist)))

(defcustom hfy-istext-command "file %s | sed -e 's@^[^:]*:[ \t]*@@'"
  "Command to run with the name of a file, to see whether it is a text file
or not.  The command should emit a string containing the word 'text' if
the file is a text file, and a string not containing 'text' otherwise."
  :group 'htmlfontify
  :tag   "istext-command"
  :type  '(string))

(defcustom hfy-find-cmd
  "find . -type f \\! -name \\*~ \\! -name \\*.flc \\! -path \\*/CVS/\\*"
  "Find command used to harvest a list of files to attempt to fontify."
  :group 'htmlfontify
  :tag   "find-command"
  :type  '(string))

(defcustom hfy-display-class nil
  "Display class to use to determine which display class to use when
calculating a face's attributes.  This is useful when, for example, you
are running Emacs on a tty or in batch mode, and want htmlfontify to have
access to the face spec you would use if you were connected to an X display.\n
Some valid class specification elements are:\n
  '(class      color)
  '(class      grayscale)
  '(background dark)
  '(background light)
  '(type       x-toolkit)
  '(type       tty)
  '(type       motif)
  '(type       lucid)
Multiple values for a tag may be combined, to indicate that any one or more
of these values in the specification key constitutes a match, eg:\n
'((class color grayscale) (type tty)) would match any of:\n
  '((class color))
  '((class grayscale))
  '((class color grayscale))
  '((class color foo))
  '((type  tty))
  '((type  tty) (class color))\n
and so on."
  :type    '(alist :key-type (symbol) :value-type (symbol))
  :group   'htmlfontify
  :tag     "display-class"
  :options '((type       (choice (const :tag "X11"           x-toolkit)
                                 (const :tag "Terminal"      tty      )
                                 (const :tag "Lucid Toolkit" lucid    )
                                 (const :tag "Motif Toolkit" motif    )))

             (class      (choice (const :tag "Color"         color    )
                                 (const :tag "Grayscale"     grayscale)))

             (background (choice (const :tag "Dark"          dark     )
                                 (const :tag "Bright"        light    ))) ))

(defcustom hfy-optimisations (list 'keep-overlays)
  "Optimizations to turn on: So far, the following have been implemented:\n
  merge-adjacent-tags: If two (or more) span tags are adjacent, identical and
                       separated by nothing more than whitespace, they will
                       be merged into one span.
  zap-comment-links  : Suppress hyperlinking of tags found in comments.
  zap-string-links   : Suppress hyperlinking of tags found in strings.
  div-wrapper        : Add <div class=\"default\"> </div> tags around the
                       output.
  keep-overlays      : More of a bell (or possibly whistle) than an
                       optimization - If on, preserve overlay highlighting
                       (cf ediff or goo-font-lock) as well as basic faces.\n
  body-text-only     : Emit only body-text. In concrete terms,
                       1. Suppress calls to `hfy-page-header'and
                          `hfy-page-footer'
                       2. Pretend that `div-wrapper' option above is
                          turned off
                       3. Don't enclose output in <pre> </pre> tags
  And the following are planned but not yet available:\n
  kill-context-leak  : Suppress hyperlinking between files highlighted by
                       different modes.\n
Note: like compiler optimizations, these optimize the _output_ of the code,
not the processing of the source itself, and are therefore likely to slow
htmlfontify down, at least a little.  Except for skip-refontification,
which can never slow you down, but may result in incomplete fontification."
  :type  '(set (const :tag "merge-adjacent-tags"  merge-adjacent-tags )
               (const :tag "zap-comment-links"    zap-comment-links   )
               (const :tag "zap-string-links"     zap-string-links    )
               (const :tag "skip-refontification" skip-refontification)
               (const :tag "kill-context-leak"    kill-context-leak   )
               (const :tag "div-wrapper"          div-wrapper         )
               (const :tag "keep-overlays"        keep-overlays       )
               (const :tag "body-text-only"       body-text-only      ))
  :group 'htmlfontify
  :tag   "optimizations")

(defvar hfy-tags-cache nil
  "Alist of the form:\n
\((\"/src/dir/0\" . tag-hash0) (\"/src/dir/1\" tag-hash1) ...)\n
Each tag hash entry then contains entries of the form:\n
\"tag_string\" => ((\"file/name.ext\" line char) ... )\n
ie an alist mapping (relative) file paths to line and character offsets.\n
See also `hfy-load-tags-cache'.")

(defvar hfy-tags-sortl nil
  "Alist of the form ((\"/src/dir\" . (tag0 tag1 tag2)) ... )\n
where the tags are stored in descending order of length.\n
See also `hfy-load-tags-cache'.")

(defvar hfy-tags-rmap nil
  "Alist of the form ((\"/src/dir\" . tag-rmap-hash))\n
where tag-rmap-hash has entries of the form:
\"tag_string\" => ( \"file/name.ext\" line char )
Unlike `hfy-tags-cache' these are the locations of occurrences of
tagged items, not the locations of their definitions.")

(defvar hfy-style-assoc 'please-ignore-this-line
  "An assoc representing/describing an Emacs face.
Properties may be repeated, in which case later properties should be
treated as if they were inherited from a 'parent' font.
\(For some properties, only the first encountered value is of any importance,
for others the values might be cumulative, and for others they might be
cumulative in a complex way.)\n
Some examples:\n
\(hfy-face-to-style 'default) =>
  ((\"background\"      . \"rgb(0, 0, 0)\")
   (\"color\"           . \"rgb(255, 255, 255)\")
   (\"font-style\"      . \"normal\")
   (\"font-weight\"     . \"500\")
   (\"font-stretch\"    . \"normal\")
   (\"font-family\"     . \"misc-fixed\")
   (\"font-size\"       . \"13pt\")
   (\"text-decoration\" . \"none\"))\n
\(hfy-face-to-style 'Info-title-3-face) =>
  ((\"font-weight\"     . \"700\")
   (\"font-family\"     . \"helv\")
   (\"font-size\"       . \"120%\")
   (\"text-decoration\" . \"none\"))\n")

(defvar hfy-sheet-assoc 'please-ignore-this-line
  "An assoc with elements of the form (face-name style-name . style-string):\n
'((default               \"default\" . \"{background: black; color: white}\")
  (font-lock-string-face \"string\"  . \"{color: rgb(64,224,208)}\"))" )

(defvar hfy-facemap-assoc 'please-ignore-this-line
  "An assoc of (point . FACE-SYMBOL) or (point . DEFFACE-LIST)
and (point . 'end) elements, in descending order of point value
\(ie from the file's end to its beginning).\n
The map is in reverse order because inserting a <style> tag (or any other
string) at `point' invalidates the map for all entries with a greater value of
point.  By traversing the map from greatest to least point, we still invalidate
the map as we go, but only those points we have already dealt with (and
therefore no longer care about) will be invalid at any time.\n
'((64820 . end)
  (64744 . font-lock-comment-face)
  (64736 . end)
  (64722 . font-lock-string-face)
  (64630 . end)
  (64623 . font-lock-string-face)
  (64449 . end)
  (64446 . font-lock-keyword-face)
  (64406 . end)
  (64395 . font-lock-constant-face)
  (64393 . end)
  (64386 . font-lock-keyword-face)
  (64379 . end)
  ;; big similar section elided.  You get the idea.
  (4285 . font-lock-constant-face)
  (4285 . end)
  (4221 . font-lock-comment-face)
  (4221 . end)
  (4197 . font-lock-constant-face)
  (4197 . end)
  (1 . font-lock-comment-face))")

(defvar hfy-tmpfont-stack nil
  "An alist of derived fonts resulting from overlays.")

(defconst hfy-hex-regex "[0-9A-Fa-f]")

(defconst hfy-triplet-regex
  (concat
   "\\(" hfy-hex-regex hfy-hex-regex "\\)"
   "\\(" hfy-hex-regex hfy-hex-regex "\\)"
   "\\(" hfy-hex-regex hfy-hex-regex "\\)"))

(defun hfy-interq (set-a set-b)
  "Return the intersection (using `eq') of two lists SET-A and SET-B."
  (let ((sa set-a) (interq nil) (elt nil))
    (while sa
      (setq elt (car sa)
            sa  (cdr sa))
      (if (memq elt set-b) (setq interq (cons elt interq))))
    interq))

(defun hfy-colour-vals (colour)
  "Where COLOUR is a color name or #XXXXXX style triplet, return a
list of three (16 bit) rgb values for said color.\n
If a window system is unavailable, calls `hfy-fallback-colour-values'."
  (if (string-match hfy-triplet-regex colour)
      (mapcar
       (lambda (x) (* (string-to-number (match-string x colour) 16) 257))
       '(1 2 3))
    ;;(message ">> %s" colour)
    (if window-system
        (if (fboundp 'color-values)
            (color-values colour)
          ;;(message "[%S]" window-system)
          (x-color-values colour))
      ;; blarg - tty colors are no good - go fetch some X colors:
      (hfy-fallback-colour-values colour))))

(defvar hfy-cperl-mode-kludged-p nil)

(defun hfy-kludge-cperl-mode ()
  "CPerl mode does its damnedest not to do some of its fontification when not
in a windowing system - try to trick it..."
  (if (not hfy-cperl-mode-kludged-p)
      (progn (if (not window-system)
                 (let ((window-system 'htmlfontify))
                   (eval-and-compile (require 'cperl-mode))
                   (setq cperl-syntaxify-by-font-lock t)))
             (setq hfy-cperl-mode-kludged-p t))) )

(defun hfy-opt (symbol)
  "Is option SYMBOL set."
  (memq symbol hfy-optimisations))

(defun hfy-default-header (file style)
  "Default value for `hfy-page-header'.
FILE is the name of the file.
STYLE is the inline CSS stylesheet (or tag referring to an external sheet)."
;;   (format "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">
;; <html>\n <head>\n  <title>%s</title>\n %s\n </head>\n  <body>\n" file style))
  (format "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
\"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
  <head>
    <title>%s</title>
%s
    <script type=\"text/javascript\"><!--
  // this function is needed to work around
  // a bug in IE related to element attributes
  function hasClass(obj)
  {
      var result = false;
      if (obj.getAttributeNode(\"class\") != null)
      {
          result = obj.getAttributeNode(\"class\").value;
      }
      return result;
  }

  function stripe(id)
  {
      // the flag we'll use to keep track of
      // whether the current row is odd or even
      var even = false;

      // if arguments are provided to specify the colors
      // of the even & odd rows, then use the them;
      // otherwise use the following defaults:
      var evenColor = arguments[1] ? arguments[1] : \"#fff\";
      var oddColor  = arguments[2] ? arguments[2] : \"#ddd\";

      // obtain a reference to the desired table
      // if no such table exists, abort
      var table = document.getElementById(id);
      if (! table) { return; }

      // by definition, tables can have more than one tbody
      // element, so we'll have to get the list of child
      // &lt;tbody&gt;s
      var tbodies = table.getElementsByTagName(\"tbody\");

      // and iterate through them...
      for (var h = 0; h < tbodies.length; h++)
      {
          // find all the &lt;tr&gt; elements...
          var trs = tbodies[h].getElementsByTagName(\"tr\");

          // ... and iterate through them
          for (var i = 0; i < trs.length; i++)
          {
              // avoid rows that have a class attribute
              // or backgroundColor style
              if (! hasClass(trs[i]) &&
                  ! trs[i].style.backgroundColor)
              {
                  // get all the cells in this row...
                  var tds = trs[i].getElementsByTagName(\"td\");

                  // and iterate through them...
                  for (var j = 0; j < tds.length; j++)
                  {
                      var mytd = tds[j];

                      // avoid cells that have a class attribute
                      // or backgroundColor style
                      if (! hasClass(mytd) &&
                          ! mytd.style.backgroundColor)
                      {
                          mytd.style.backgroundColor =
                            even ? evenColor : oddColor;
                      }
                  }
              }
              // flip from odd to even, or vice-versa
              even =  ! even;
          }
      }
  }

  function toggle_invis( name )
  {
      var filter =
        { acceptNode:
          function( node )
          { var classname = node.id;
            if( classname )
            { var classbase = classname.substr( 0, name.length );
              if( classbase == name ) { return NodeFilter.FILTER_ACCEPT; } }
            return NodeFilter.FILTER_SKIP; } };
      var walker = document.createTreeWalker( document.body           ,
                                              NodeFilter.SHOW_ELEMENT ,
                                              filter                  ,
                                              false                   );
      while( walker.nextNode() )
      {
          var e = walker.currentNode;
          if( e.style.display == \"none\" ) { e.style.display = \"inline\"; }
          else                            { e.style.display = \"none\";   }
      }
  }
--> </script>
  </head>
  <body onload=\"stripe('index'); return true;\">\n"
          file style))

(defun hfy-default-footer (_file)
  "Default value for `hfy-page-footer'.
FILE is the name of the file being rendered, in case it is needed."
  "\n </body>\n</html>\n")

(defun hfy-link-style-string (style-string)
  "Replace the end of a CSS style declaration STYLE-STRING with the contents
of the variable `hfy-src-doc-link-style', removing text matching the regex
`hfy-src-doc-link-unstyle' first, if necessary."
  ;;(message "hfy-colour-vals");;DBUG
  (if (string-match hfy-src-doc-link-unstyle style-string)
      (setq style-string (replace-match "" 'fixed-case 'literal style-string)))
  (if (and (not (string-match hfy-src-doc-link-style style-string))
           (string-match "} *$" style-string))
      (concat (replace-match hfy-src-doc-link-style
                             'fixed-case
                             'literal
                             style-string) " }")
    style-string))

;; utility functions - cast emacs style specification values into their
;; css2 equivalents:
(defun hfy-triplet (colour)
  "Takes a COLOUR name (string) and return a CSS rgb(R, G, B) triplet string.
Uses the definition of \"white\" to map the numbers to the 0-255 range, so
if you've redefined white, (esp. if you've redefined it to have a triplet
member lower than that of the color you are processing) strange things
may happen."
  ;;(message "hfy-colour-vals");;DBUG
  (let ((white (mapcar (lambda (I) (float (1+ I))) (hfy-colour-vals "white")))
        (rgb16 (mapcar (lambda (I) (float (1+ I))) (hfy-colour-vals  colour))))
    (if rgb16
        ;;(apply 'format "rgb(%d, %d, %d)"
        ;; Use #rrggbb instead, it is smaller
        (apply 'format "#%02x%02x%02x"
               (mapcar (lambda (X)
                         (* (/ (nth X rgb16)
                               (nth X white)) 255))
                       '(0 1 2))))))

(defun hfy-family (family) (list (cons "font-family"  family)))
(defun hfy-bgcol  (colour) (list (cons "background"   (hfy-triplet colour))))
(defun hfy-colour (colour) (list (cons "color"        (hfy-triplet colour))))
(defun hfy-width  (width)  (list (cons "font-stretch" (symbol-name  width))))

(defcustom hfy-font-zoom 1.05
  "Font scaling from Emacs to HTML."
  :type 'float
  :group 'htmlfontify)

(defun hfy-size (height)
  "Derive a CSS font-size specifier from an Emacs font :height attribute HEIGHT.
Does not cope with the case where height is a function to be applied to
the height of the underlying font."
  (list
   (cond
    ;;(t                 (cons "font-size" ": 1em"))
    ((floatp   height)
     (cons "font-size" (format "%d%%" (* (* hfy-font-zoom height) 100))))
    ((integerp height)
     (cons "font-size" (format "%dpt" (/ (* hfy-font-zoom height) 10 )))) )) )

(defun hfy-slant (slant)
  "Derive a font-style CSS specifier from the Emacs :slant attribute SLANT:
CSS does not define the reverse-* styles, so just maps those to the
regular specifiers."
  (list (cons "font-style"
              (or (cdr (assq slant '((italic          . "italic")
                                     (reverse-italic  . "italic" )
                                     (oblique         . "oblique")
                                     (reverse-oblique . "oblique"))))
                  "normal"))))

(defun hfy-weight (weight)
  "Derive a font-weight CSS specifier from an Emacs weight spec symbol WEIGHT."
  (list (cons "font-weight" (cdr (assq weight '((ultra-bold  . "900")
                                                (extra-bold  . "800")
                                                (bold        . "700")
                                                (semi-bold   . "600")
                                                (normal      . "500")
                                                (semi-light  . "400")
                                                (light       . "300")
                                                (extra-light . "200")
                                                (ultra-light . "100")))))))

(defun hfy-box-to-border-assoc (spec)
  (if spec
      (let ((tag (car  spec))
            (val (cadr spec)))
        (cons (case tag
                (:color (cons "colour" val))
                (:width (cons "width"  val))
                (:style (cons "style"  val)))
              (hfy-box-to-border-assoc (cddr spec))))))

(defun hfy-box-to-style (spec)
  (let* ((css (hfy-box-to-border-assoc  spec))
         (col (cdr      (assoc "colour" css)))
         (s   (cdr      (assoc "style"  css))))
    (list
     (if col (cons "border-color" (cdr (assoc "colour" css))))
     (cons "border-width" (format "%dpx" (or (cdr (assoc "width" css)) 1)))
     (cons "border-style" (case s
                            (released-button "outset")
                            (pressed-button  "inset" )
                            (t               "solid" ))))))

(defun hfy-box (box)
  "Derive CSS border-* attributes from the Emacs :box attribute BOX."
  (if box
      (cond
       ((integerp box) (list (cons "border-width" (format "%dpx"   box))))
       ((stringp  box) (list (cons "border" (format "solid %s 1px" box))))
       ((listp    box) (hfy-box-to-style box)                            ))) )

(defun hfy-decor (tag _val)
  "Derive CSS text-decoration specifiers from various Emacs font attributes.
TAG is an Emacs font attribute key (eg :underline).
VAL is ignored."
  (list
   ;; FIXME: Why not '("text-decoration" . "underline")?  --Stef
   (case tag
     (:underline      (cons "text-decoration" "underline"   ))
     (:overline       (cons "text-decoration" "overline"    ))
     (:strike-through (cons "text-decoration" "line-through")))))

(defun hfy-invisible (&optional _val)
  "This text should be invisible.
Do something in CSS to make that happen.
VAL is ignored here."
  '(("display" . "none")))

(defun hfy-combined-face-spec (face)
  "Return a `defface' style alist of possible specifications for FACE.
Entries resulting from customization (`custom-set-faces') will take
precedence."
  (append
   (if (and hfy-display-class hfy-default-face-def (eq face 'default))
       hfy-default-face-def)
   (get face 'saved-face)
   (get face 'face-defface-spec)))

(defun hfy-face-attr-for-class (face &optional class)
  "Return the face attributes for FACE.
If CLASS is set, it must be a `defface' alist key [see below],
in which case the first face specification returned by `hfy-combined-face-spec'
which *doesn't* clash with CLASS is returned.\n
\(A specification with a class of t is considered to match any class you
specify - this matches Emacs's behavior when deciding on which face attributes
to use, to the best of my understanding).\n
If CLASS is nil, then you just get whatever `face-attr-construct' returns,
ie the current specification in effect for FACE.\n
*NOTE*: This function forces any face that is not 'default and which has
no :inherit property to inherit from 'default (this is because 'default
is magical in that Emacs's fonts behave as if they inherit implicitly from
'default, but no such behavior exists in HTML/CSS).\n
See also `hfy-display-class' for details of valid values for CLASS."
  (let ((face-spec
         (if class
             (let ((face-props (hfy-combined-face-spec face))
                   (face-specn nil)
                   (face-class nil)
                   (face-attrs nil)
                   (face-score  -1)
                   (face-match nil))
               (while face-props
                 (setq face-specn (car face-props)
                       face-class (car face-specn)
                       face-attrs (cdr face-specn)
                       face-props (cdr face-props))
                 ;; if the current element CEL of CLASS is t we match
                 ;; if the current face-class is t, we match
                 ;; if the cdr of CEL has a non-nil
                 ;;   intersection with the cdr of the first member of
                 ;;   the current face-class with the same car as CEL, we match
                 ;; if we actually clash, then we can't match
                 (let ((cbuf class)
                       (cel    nil)
                       (key    nil)
                       (val    nil)
                       (x      nil)
                       (next   nil)
                       (score    0))
                   (while (and cbuf (not next))
                     (setq cel  (car cbuf)
                           cbuf (cdr cbuf)
                           key  (car  cel)
                           val  (cdr  cel)
                           val  (if (listp val) val (list val)))
                     (cond
                      ((or (eq cel t)
                           (memq face-class '(t default))) ;Default match.
                       (setq score 0) (ignore "t match"))
                      ((not (cdr (assq key face-class))) ;Neither good nor bad.
                       nil (ignore "non match, non collision"))
                      ((setq x (hfy-interq val (cdr (assq key face-class))))
                       (setq score (+ score (length x)))
                       (ignore "intersection"))
                      (t ;; nope.
                       (setq next t score -10) (ignore "collision")) ))
                   (if (> score face-score)
                       (progn
                         (setq face-match face-attrs
                               face-score score     )
                         (ignore "%d << %S/%S" score face-class class))
                     (ignore "--- %d ---- (insufficient)" score)) ))
               ;; matched ? last attrs : nil
               (if face-match
                   (if (listp (car face-match)) (car face-match) face-match)
                 nil))
           ;; Unfortunately the default face returns a
           ;; :background. Fortunately we can remove it, but how do we do
           ;; that in a non-system specific way?
           (let ((spec (face-attr-construct face))
                 (new-spec nil))
             (if (not (memq :background spec))
                 spec
               (while spec
                 (let ((a (nth 0 spec))
                       (b (nth 1 spec)))
                   (unless (and (eq a :background)
                                (stringp b)
                                (string= b "SystemWindow"))
                     (setq new-spec (cons a (cons b new-spec)))))
                 (setq spec (cddr spec)))
               new-spec)))))
    (if (or (memq :inherit face-spec) (eq 'default face))
        face-spec
      (append face-spec (list :inherit 'default)))))

;; construct an assoc of (css-tag-name . css-tag-value) pairs
;; from a face or assoc of face attributes:

;; Some tests etc:
;;  (mumamo-message-with-face "testing face" 'highlight)
;;  (mumamo-message-with-face "testing face" '(:foreground "red" :background "yellow"))
;;  (hfy-face-to-style-i '(:inherit default foreground-color "red"))
;;  default face=(:stipple nil :background "SystemWindow" :foreground
;;    "SystemWindowText" :inverse-video nil :box nil :strike-through
;;    nil :overline nil :underline nil :slant normal :weight normal
;;    :height 98 :width normal :family "outline-courier new")
(defun hfy-face-to-style-i (fn)
  "The guts of `hfy-face-to-style': FN should be a `defface' font spec,
as returned by `face-attr-construct' or `hfy-face-attr-for-class'.
Note that this function does not get font-sizes right if they are based
on inherited modifiers (via the :inherit) attribute, and any other
modifiers that are cumulative if they appear multiple times need to be
merged by the user - `hfy-flatten-style' should do this."
  ;;(message "hfy-face-to-style-i");;DBUG

  ;; fn's value could be something like
  ;; (:inherit
  ;;  ((foreground-color . "blue"))
  ;;  (foreground-color . "blue")
  ;;  nil)

  (when fn
    (let ((key  (car  fn))
          (val  (cadr fn))
          (next (cddr fn))
          (that       nil)
          (this       nil)
          (parent     nil))
      (if (eq key :inherit)
        (let ((vs (if (listp val) val (list val))))
          ;; (let ((x '(a b))) (setq x (append '(c d) x)))
          ;; (let ((x '(a b))) (setq x (append '(c d) x)))
          (dolist (v vs)
            (setq parent
                  (append
                   parent
                   (hfy-face-to-style-i
                    (hfy-face-attr-for-class v hfy-display-class)) ))))
        (setq this
              (if val (case key
                       (:family         (hfy-family    val))
                       (:width          (hfy-width     val))
                       (:weight         (hfy-weight    val))
                       (:slant          (hfy-slant     val))
                       (:foreground     (hfy-colour    val))
                       (:background     (hfy-bgcol     val))
                       (:box            (hfy-box       val))
                       (:height         (hfy-size      val))
                       (:underline      (hfy-decor key val))
                       (:overline       (hfy-decor key val))
                       (:strike-through (hfy-decor key val))
                       (:invisible      (hfy-invisible val))
                       (:bold           (hfy-weight  'bold))
                       (:italic         (hfy-slant 'italic))))))
      (setq that (hfy-face-to-style-i next))
      ;;(lwarn t :warning "%S => %S" fn (nconc this that parent))
      (nconc this that parent))) )

(defun hfy-size-to-int (spec)
  "Convert SPEC, a CSS font-size specifier, to an Emacs :height attribute value.
Used while merging multiple font-size attributes."
  ;;(message "hfy-size-to-int");;DBUG
  (list
   (if (string-match "\\([0-9]+\\)\\(%\\|pt\\)" spec)
       (cond ((string= "%"  (match-string 2 spec))
              (/ (string-to-number (match-string 1 spec)) 100.0))
             ((string= "pt" (match-string 2 spec))
              (* (string-to-number (match-string 1 spec))    10)))
     (string-to-number spec))) )

;; size is different, in that in order to get it right at all,
;; we have to trawl the inheritance path, accumulating modifiers,
;; _until_ we get to an absolute (pt) specifier, then combine the lot
(defun hfy-flatten-style (style)
  "Take STYLE (see `hfy-face-to-style-i', `hfy-face-to-style') and merge
any multiple attributes appropriately.  Currently only font-size is merged
down to a single occurrence - others may need special handling, but I
haven't encountered them yet.  Returns a `hfy-style-assoc'."
  ;;(message "(hfy-flatten-style %S)" style) ;;DBUG
  (let ((n        0)
        (m (list 1))
        (x      nil)
        (r      nil))
    (dolist (css style)
      (if (string= (car css) "font-size")
          (progn
            (when (not x) (setq m (nconc m (hfy-size-to-int (cdr css)))))
            (when (string-match "pt" (cdr css)) (setq x t)))
        (setq r (nconc r (list css)))))
    ;;(message "r: %S" r)
    (setq  n (apply '* m))
    (nconc r (hfy-size (if x (round n) (* n 1.0)))) ))

(defun hfy-face-resolve-face (fn)
  (cond
   ((facep fn)
    (hfy-face-attr-for-class fn hfy-display-class))
   ((and (symbolp fn)
         (facep (symbol-value fn)))
    ;; Obsolete faces like `font-lock-reference-face' are defined as
    ;; aliases for another face.
    (hfy-face-attr-for-class (symbol-value fn) hfy-display-class))
   (t nil)))


(defun hfy-face-to-style (fn)
  "Take FN, a font or `defface' style font specification,
\(as returned by `face-attr-construct' or `hfy-face-attr-for-class')
and return a `hfy-style-assoc'.\n
See also `hfy-face-to-style-i', `hfy-flatten-style'."
  ;;(message "hfy-face-to-style");;DBUG
  (let* ((face-def (hfy-face-resolve-face fn))
         (final-style
          (hfy-flatten-style (hfy-face-to-style-i face-def))))
    ;;(message "%S" final-style)
    (if (not (assoc "text-decoration" final-style))
        (progn (setq final-style
                     ;; Fix-me: there is no need for this since
                     ;; text-decoration is not inherited.
                     ;; but it's not wrong and if this ever changes it will
                     ;; be needed, so I think it's better to leave it in? -- v
                     (nconc final-style '(("text-decoration" . "none"))))))
    final-style))

;; strip redundant bits from a name. Technically, this could result in
;; a collision, but it is pretty unlikely - will fix later...
;; also handle ephemeral fonts created by overlays, which don't actually
;; have names:
(defun hfy-face-or-def-to-name (fn)
  "Render a font symbol or `defface' font spec FN into a name (string)."
  ;;(message "generating name for %s" fn)
  (if (not (listp fn))
      (format "%s" fn)
    (let* ((key   (format       "%s"        fn))
           (entry (assoc key hfy-tmpfont-stack))
           (base  (cadr   (memq  :inherit  fn)))
           (tag   (cdr                   entry)))
      ;;(message "checking for key «%s» in font stack [%d]"
      ;;         key (if entry 1 0))
      (if entry nil ;; noop
        (setq tag               (format "%04d" (length hfy-tmpfont-stack))
              entry             (cons key tag)
              hfy-tmpfont-stack (cons entry hfy-tmpfont-stack)))
      ;;(message "  -> name: %s-%s" (or base 'default) tag)
      (format "%s-%s" (or base 'default) tag)) ))

(defun hfy-css-name (fn)
  "Strip the boring bits from a font-name FN and return a CSS style name."
  ;;(message "hfy-css-name");;DBUG
  (let ((face-name (hfy-face-or-def-to-name fn)))
    (if (or (string-match "font-lock-\\(.*\\)" face-name)
            (string-match "cperl-\\(.*\\)"     face-name)
            (string-match "^[Ii]nfo-\\(.*\\)"   face-name))
        (progn
          (setq face-name (match-string 1 face-name))
          (if (string-match "\\(.*\\)-face\\'" face-name)
              (setq face-name (match-string 1 face-name)))
          face-name)
      face-name)) )

;; construct an assoc of (stripped-name . "{ css-stuff-here }") pairs
;; from a face:
(defun hfy-face-to-css-default (fn)
  "Default handler for mapping faces to styles.
See also `hfy-face-to-css'."
  ;;(message "hfy-face-to-css");;DBUG
  (let* ((css-list (hfy-face-to-style fn))
         (seen     nil)
         (css-text
          (mapcar
           (lambda (E)
             (if (car E)
                 (unless (member (car E) seen)
                   (push (car E) seen)
                   (format " %s: %s; " (car E) (cdr E)))))
           css-list)))
    (cons (hfy-css-name fn) (format "{%s}" (apply 'concat css-text)))) )

(defvar hfy-face-to-css 'hfy-face-to-css-default
  "Handler for mapping faces  to styles.
The signature of the handler is of the form \(lambda (FN) ...\).
FN is a font or `defface' specification (cf
`face-attr-construct').  The handler should return a cons cell of
the form (STYLE-NAME . STYLE-SPEC).

The default handler is `hfy-face-to-css-default'.

See also `hfy-face-to-style'.")

(defalias 'hfy-prop-invisible-p
  (if (fboundp 'invisible-p) #'invisible-p
    (lambda (prop)
      "Is text property PROP an active invisibility property?"
      (or (and (eq buffer-invisibility-spec t) prop)
          (or (memq prop buffer-invisibility-spec)
              (assq prop buffer-invisibility-spec))))))

(defun hfy-find-invisible-ranges ()
  "Return a list of (start-point . end-point) cons cells of invisible regions."
  (save-excursion
    (let (invisible p i s) ;; return-value pos invisible end start
      (setq p (goto-char (point-min)))
      (when (invisible-p p) (setq s p i t))
      (while (< p (point-max))
        (if i ;; currently invisible
            (when (not (invisible-p p)) ;; but became visible
              (setq i         nil
                    invisible (cons (cons s p) invisible)))
          ;; currently visible:
          (when (invisible-p p)  ;; but have become invisible
            (setq s p i t)))
        (setq p (next-char-property-change p)))
      ;; still invisible at buffer end?
      (when i
        (setq invisible (cons (cons s (point-max)) invisible)))
      invisible)))

(defun hfy-invisible-name (point map)
  "Generate a CSS style name for an invisible section of the buffer.
POINT is the point inside the invisible region.
MAP is the invisibility map as returned by `hfy-find-invisible-ranges'."
  ;;(message "(hfy-invisible-name %S %S)" point map)
  (let (name)
    (dolist (range map)
      (when (and (>= point (car range))
                 (<  point (cdr range)))
        (setq name (format "invisible-%S-%S" (car range) (cdr range)))))
    name))

;; Fix-me: This function needs some cleanup by someone who understand
;; all the formats that face properties can have.
;;
;; overlay handling should be fine. haven't tested multiple stacked overlapping
;; overlays recently, but the common case of a text property face + an overlay
;; face produces the correct merged css style (or as close to it as css can get)
;; -- v
(defun hfy-face-at (p)
  "Find face in effect at point P.
If overlays are to be considered (see `hfy-optimisations') then this may
return a `defface' style list of face properties instead of a face symbol."
  ;;(message "hfy-face-at");;DBUG
  ;; Fix-me: clean up, remove face-name etc
  ;; not sure why we'd want to remove face-name? -- v
  (let ((overlay-data nil)
        (base-face    nil)
        (face-name   (get-text-property p 'face))
        ;; (face-name    (hfy-get-face-at p))
        (prop-seen    nil)
        (extra-props  nil)
        (text-props   (text-properties-at p)))
    ;;(message "face-name: %S" face-name)
    (when (and face-name (listp face-name) (facep (car face-name)))
      ;;(message "face-name is a list %S" face-name)
      ;;(setq text-props (cons 'face face-name))
      (dolist (f face-name)
        (setq extra-props (if (listp f)
                              ;; for things like (variable-pitch
                              ;; (:foreground "red"))
                              (cons f extra-props)
                            (cons :inherit (cons f extra-props)))))
      (setq base-face (car face-name)
            face-name nil))
    ;; text-properties-at => (face (:foreground "red" ...))
    ;;                 or => (face (compilation-info underline)) list of faces
    ;; overlay-properties
    ;;   format= (evaporate t face ((foreground-color . "red")))

    ;; SO:    if we have turned overlays off,
    ;;     or if there's no overlay data
    ;; just bail out and return whatever face data we've accumulated so far
    (if (or (not (hfy-opt                      'keep-overlays))
            (not (setq overlay-data  (hfy-overlay-props-at p))))
        (progn
          ;;(message "· %d: %s; %S; %s"
          ;;         p face-name extra-props text-props)
          (or face-name base-face)) ;; no overlays or extra properties
      ;; collect any face data and any overlay data for processing:
      (when text-props
        (push text-props overlay-data))
      (setq overlay-data (nreverse overlay-data))
      ;;(message "- %d: %s; %S; %s; %s"
      ;;         p face-name extra-props text-props overlay-data)
      ;; remember the basic face name so we don't keep repeating its specs:
      (when face-name (setq base-face face-name))
      (dolist (P overlay-data)
        (let ((iprops (cadr (memq 'invisible P)))) ;FIXME: plist-get?
          ;;(message "(hfy-prop-invisible-p %S)" iprops)
          (when (and iprops (hfy-prop-invisible-p iprops))
            (setq extra-props
                  (cons :invisible (cons t extra-props))) ))
        (let ((fprops (cadr (or (memq 'face P)
                                (memq 'font-lock-face P)))))
          ;;(message "overlay face: %s" fprops)
          (if (not (listp fprops))
              (let ((this-face (if (stringp fprops) (intern fprops) fprops)))
                (when (not (eq this-face base-face))
                  (setq extra-props
                        (cons :inherit
                              (cons this-face extra-props))) ))
            (while fprops
              (if (facep (car fprops))
                  (let ((face (car fprops)))
                    (when (stringp face) (setq face (intern fprops)))
                    (setq extra-props
                          (cons :inherit
                                (cons face
                                      extra-props)))
                    (setq fprops (cdr fprops)))
                (let (p v)
                  ;; Sigh.
                  (if (listp (car fprops))
                      (if (nlistp (cdr (car fprops)))
                          (progn
                            ;; ((prop . val))
                            (setq p (caar fprops))
                            (setq v (cdar fprops))
                            (setq fprops (cdr fprops)))
                        ;; ((prop val))
                        (setq p (caar fprops))
                        (setq v (cadar fprops))
                        (setq fprops (cdr fprops)))
                    (if (listp (cdr fprops))
                        (progn
                          ;; (:prop val :prop val ...)
                          (setq p (car fprops))
                          (setq v (cadr fprops))
                          (setq fprops (cddr fprops)))
                      (if (and (listp fprops)
                               (not (listp (cdr fprops))))
                          ;;(and (consp x) (cdr (last x)))
                          (progn
                            ;; (prop . val)
                            (setq p (car fprops))
                            (setq v (cdr fprops))
                            (setq fprops nil))
                        (error "Eh... another format! fprops=%s" fprops) )))
                  (setq p (case p
                            ;; These are all the properties handled
                            ;; in `hfy-face-to-style-i'.
                            ;;
                            ;; Are these translations right?
                            ;; yes, they are -- v
                            (family           :family    )
                            (width            :width     )
                            (height           :height    )
                            (weight           :weight    )
                            (slant            :slant     )
                            (underline        :underline )
                            (overline         :overline  )
                            (strike-through   :strike-through)
                            (box              :box       )
                            (foreground-color :foreground)
                            (background-color :background)
                            (bold             :bold      )
                            (italic           :italic    )
                            (t                 p)))
                  (if (memq p prop-seen) nil ;; noop
                    (setq prop-seen   (cons p prop-seen)
                          extra-props (cons p (cons v extra-props))))))))))
      ;;(message "+ %d: %s; %S" p face-name extra-props)
      (if extra-props
          (nconc extra-props (if (listp face-name)
                                 face-name
                               (face-attr-construct face-name)))
        face-name)) ))

(defun hfy-overlay-props-at (p)
  "Grab overlay properties at point P.
The plists are returned in descending priority order."
  (sort (mapcar #'overlay-properties (overlays-at p))
        (lambda (A B) (> (or (cadr (memq 'priority A)) 0) ;FIXME: plist-get?
                    (or (cadr (memq 'priority B)) 0)))))

;; construct an assoc of (face-name . (css-name . "{ css-style }")) elements:
(defun hfy-compile-stylesheet ()
  "Trawl the current buffer, construct and return a `hfy-sheet-assoc'.
If `hfy-user-sheet-assoc' is currently bound then use it to
collect new styles discovered during this run.  Otherwise create
a new assoc."
  ;;(message "hfy-compile-stylesheet");;DBUG
  (let ((pt (point-min))
        ;; Make the font stack stay:
        ;;(hfy-tmpfont-stack nil)
        (fn         nil)
        (style      (and (boundp 'hfy-user-sheet-assoc) hfy-user-sheet-assoc)))
    (save-excursion
      (goto-char pt)
      (while (< pt (point-max))
        (if (and (setq fn (hfy-face-at pt)) (not (assoc fn style)))
            (push (cons fn (funcall hfy-face-to-css fn)) style))
        (setq pt (next-char-property-change pt))))
    (unless (assoc 'default style)
      (push (cons 'default (funcall hfy-face-to-css 'default)) style))
    (when (boundp 'hfy-user-sheet-assoc)
      (setq hfy-user-sheet-assoc style))
    style))

(defun hfy-fontified-p ()
  "`font-lock' doesn't like to say it's been fontified when in batch
mode, but we want to know if we should fontify or raw copy, so in batch
mode we check for non-default face properties.  Otherwise we test
variable `font-lock-mode' and variable `font-lock-fontified' for truth."
  ;;(message "font-lock-fontified: %S" font-lock-fontified)
  ;;(message "noninteractive     : %S" noninteractive)
  ;;(message "font-lock-mode     : %S" font-lock-mode)
  (and font-lock-fontified
       (if noninteractive
           (let ((pt  (point-min))
                 (face-name   nil))
             (save-excursion
               (goto-char pt)
               (while (and (< pt (point-max)) (not face-name))
                 (setq face-name (hfy-face-at pt))
                 (setq pt (next-char-property-change pt))))
             face-name)
         font-lock-mode)))

;; remember, the map is in reverse point order:
;; I wrote this while suffering the effects of a cold, and maybe a
;; mild fever - I think it's correct, but it might be a little warped
;; as my minfd keeps ... where was I? Oh yes, the bunnies...
(defun hfy-merge-adjacent-spans (face-map)
  "Where FACE-MAP is a `hfy-facemap-assoc' for the current buffer,
this function merges adjacent style blocks which are of the same value
and are separated by nothing more interesting than whitespace.\n
  <span class=\"foo\">narf</span> <span class=\"foo\">brain</span>\n
\(as interpreted from FACE-MAP) would become:\n
  <span class=\"foo\">narf brain</span>\n
Returns a modified copy of FACE-MAP."
  (let ((tmp-map face-map)
        (map-buf      nil)
        (first-start  nil)
        (first-stop   nil)
        (last-start   nil)
        (last-stop    nil)
        (span-stop    nil)
        (span-start   nil)
        (reduced-map  nil))
    ;;(push (car  tmp-map) reduced-map)
    ;;(push (cadr tmp-map) reduced-map)
    (while tmp-map
      (setq first-start (cadddr tmp-map)
            first-stop  (caddr  tmp-map)
            last-start  (cadr   tmp-map)
            last-stop   (car    tmp-map)
            map-buf      tmp-map
            span-start   last-start
            span-stop    last-stop      )
      (while (and (equal (cdr first-start)
                         (cdr  last-start))
                  (save-excursion
                    (goto-char (car first-stop))
                    (not (re-search-forward "[^ \t\n\r]" (car last-start) t))))
        (setq map-buf     (cddr map-buf)
              span-start  first-start
              first-start (cadddr map-buf)
              first-stop  (caddr  map-buf)
              last-start  (cadr   map-buf)
              last-stop   (car    map-buf)))
      (push span-stop  reduced-map)
      (push span-start reduced-map)
      (setq tmp-map (memq last-start tmp-map))
      (setq tmp-map (cdr tmp-map)))
    (setq reduced-map (nreverse reduced-map))))

;; remember to generate 'synthetic' </span> entries -
;; emacs copes by just having a stack of styles in effect
;; and only using the top one: html has a more simplistic approach -
;; we have to explicitly end a style, there's no way of temporarily
;; overriding it w. another one... (afaik)
(defun hfy-compile-face-map ()
;; no need for special <a> version.
;; IME hyperlinks don't get underlined, esp when you htmlfontify a whole
;; source tree, so the <a> version is needed -- v
;; Fix-me: save table for multi-buffer
  "Compile and return a `hfy-facemap-assoc' for the current buffer."
  ;;(message "hfy-compile-face-map");;DBUG
  (let* ((pt         (point-min))
         (pt-narrow  (save-restriction (widen) (point-min)))
         (offset     (- pt pt-narrow))
         (fn         nil)
         (map        nil)
         (prev-tag   nil)) ;; t   if the last tag-point was a span-start
                           ;; nil if it was a span-stop
    (save-excursion
      (goto-char pt)
      (while (< pt (point-max))
        (if (setq fn (hfy-face-at pt))
            (progn (if prev-tag (push (cons pt-narrow 'end) map))
                   (push (cons pt-narrow fn) map)
                   (setq prev-tag t))
          (if prev-tag (push (cons pt-narrow 'end) map))
          (setq prev-tag nil))
        (setq pt (next-char-property-change pt))
        (setq pt-narrow (+ offset pt)))
      (if (and map (not (eq 'end (cdar map))))
          (push (cons (1+ (- (point-max) (point-min))) 'end) map)))
    (if (hfy-opt 'merge-adjacent-tags) (hfy-merge-adjacent-spans map) map)))

(defun hfy-buffer ()
  "Generate a buffer to hold the HTML output.
The filename of this buffer is derived from the source (current) buffer's
variable `buffer-file-name', if it is set, plus `hfy-extn'.
Otherwise a plausible filename is constructed from `default-directory',
`buffer-name' and `hfy-extn'."
  (let* ((name (concat (buffer-name) hfy-extn))
         (src               (buffer-file-name))
         (buf  (get-buffer-create        name)))
    (with-current-buffer buf
      (setq buffer-file-name
            (if src (concat src hfy-extn)
              (expand-file-name (if (string-match "^.*/\\([^/]*\\)\\'" name)
                                    (match-string 1 name)
                                  name))))
      buf)))

(defun hfy-lookup (face style)
  "Get a CSS style name for FACE from STYLE."
  (cadr (assoc face style)))

(defun hfy-link-style (style-string)
  "Copy, alter and return a STYLE-STRING to make it suitable for a hyperlink.
Uses `hfy-link-style-fun' to do this."
  (if (functionp hfy-link-style-fun)
      (funcall hfy-link-style-fun style-string)
    style-string))

(defun hfy-sprintf-stylesheet (css file)
  "Return the inline CSS style sheet for FILE as a string."
  (let ((stylesheet
         (concat
          hfy-meta-tags
          "\n<style type=\"text/css\"><!-- \n"
          ;; Fix-me: Add handling of page breaks here + scan for ^L
          ;; where appropriate.
          (format "body %s\n" (cddr (assq 'default css)))
          (apply 'concat
                 (mapcar
                  (lambda (style)
                    (format
                     "span.%s   %s\nspan.%s a %s\n"
                     (cadr style) (cddr style)
                     (cadr style) (hfy-link-style (cddr style))))
                  css))
          " --></style>\n")))
    (funcall hfy-page-header file stylesheet)))

;; tag all the dangerous characters we want to escape
;; (ie any "<> chars we _didn't_ put there explicitly for css markup)
(defun hfy-html-enkludge-buffer ()
  "Mark dangerous [\"<>] characters with the `hfy-quoteme' property.\n
See also `hfy-html-dekludge-buffer'."
  ;;(message "hfy-html-enkludge-buffer");;DBUG
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward hfy-html-quote-regex nil t)
      (put-text-property (match-beginning 0) (point) 'hfy-quoteme t))) )

;; dangerous char -> &entity;
(defun hfy-html-quote (char-string)
  "Map CHAR-STRING to an HTML safe string (entity) if need be."
  ;;(message "hfy-html-quote");;DBUG
  (or (cadr (assoc char-string hfy-html-quote-map)) char-string) )

;; actually entity-ise dangerous chars.
;; note that we can't do this until _after_ we have inserted the css
;; markup, since we use a position-based map to insert this, and if we
;; enter any other text before we do this, we'd have to track another
;; map of offsets, which would be tedious...
(defun hfy-html-dekludge-buffer ()
  "Transform all dangerous characters marked with the `hfy-quoteme' property
using `hfy-html-quote'.\n
See also `hfy-html-enkludge-buffer'."
  ;;(message "hfy-html-dekludge-buffer");;DBUG
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward hfy-html-quote-regex nil t)
      (if (get-text-property (match-beginning 0) 'hfy-quoteme)
          (replace-match (hfy-html-quote (match-string 1))) )) ))

;; Borrowed from font-lock.el
(defmacro hfy-save-buffer-state (varlist &rest body)
  "Bind variables according to VARLIST and eval BODY restoring buffer state.
Do not record undo information during evaluation of BODY."
  (declare (indent 1) (debug let))
  (let ((modified (make-symbol "modified")))
    `(let* ,(append varlist
                    `((,modified (buffer-modified-p))
                      (buffer-undo-list t)
                      (inhibit-read-only t)
                      (inhibit-point-motion-hooks t)
                      (inhibit-modification-hooks t)
                      deactivate-mark
                      buffer-file-name
                      buffer-file-truename))
       (progn
         ,@body)
       (unless ,modified
         (restore-buffer-modified-p nil)))))

(defun hfy-mark-trailing-whitespace ()
  "Tag trailing whitespace with a hfy property if it is currently highlighted."
  (when show-trailing-whitespace
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (hfy-save-buffer-state nil
          (while (re-search-forward "[ \t]+$" nil t)
            (put-text-property (match-beginning 0) (match-end 0)
                                   'hfy-show-trailing-whitespace t)))))))

(defun hfy-unmark-trailing-whitespace ()
  "Undo the effect of `hfy-mark-trailing-whitespace'."
  (when show-trailing-whitespace
    (hfy-save-buffer-state nil
      (remove-text-properties (point-min) (point-max)
                              '(hfy-show-trailing-whitespace)))))

(defun hfy-begin-span (style text-block text-id text-begins-block-p)
  "Default handler to begin a span of text.
Insert \"<span class=\"STYLE\" ...>\". See
`hfy-begin-span-handler' for more information."
  (when text-begins-block-p
    (insert
     (format "<span onclick=\"toggle_invis('%s');\">…</span>" text-block)))

  (insert
   (if text-block
       (format "<span class=\"%s\" id=\"%s-%d\">" style text-block text-id)
     (format "<span class=\"%s\">" style))))

(defun hfy-end-span ()
  "Default handler to end a span of text.
Insert \"</span>\".  See `hfy-end-span-handler' for more
information."
  (insert "</span>"))

(defvar hfy-begin-span-handler 'hfy-begin-span
  "Handler to begin a span of text.
The signature of the handler is \(lambda (STYLE TEXT-BLOCK
TEXT-ID TEXT-BEGINS-BLOCK-P) ...\).  The handler must insert
appropriate tags to begin a span of text.

STYLE is the name of the style that begins at point.  It is
derived from the face attributes as part of `hfy-face-to-css'
callback.  The other arguments TEXT-BLOCK, TEXT-ID,
TEXT-BEGINS-BLOCK-P are non-nil only if the buffer contains
invisible text.

TEXT-BLOCK is a string that identifies a single chunk of visible
or invisible text of which the current position is a part.  For
visible portions, it's value is \"nil\". For invisible portions,
it's value is computed as part of `hfy-invisible-name'.

TEXT-ID marks a unique position within a block.  It is set to
value of `point' at the current buffer position.

TEXT-BEGINS-BLOCK-P is a boolean and is non-nil if the current
span also begins a invisible portion of text.

An implementation can use TEXT-BLOCK, TEXT-ID,
TEXT-BEGINS-BLOCK-P to implement fold/unfold-on-mouse-click like
behaviour.

The default handler is `hfy-begin-span'.")

(defvar hfy-end-span-handler 'hfy-end-span
  "Handler to end a span of text.
The signature of the handler is \(lambda () ...\).  The handler
must insert appropriate tags to end a span of text.

The default handler is `hfy-end-span'.")

(defun hfy-fontify-buffer (&optional srcdir file)
  "Implement the guts of `htmlfontify-buffer'.
SRCDIR, if set, is the directory being htmlfontified.
FILE, if set, is the file name."
  (if srcdir (setq srcdir (directory-file-name srcdir)))
  (let* ( (html-buffer        (hfy-buffer))
          (css-sheet                   nil)
          (css-map                     nil)
          (invis-ranges                nil)
          (rovl                        nil)
          (orig-ovls      (overlays-in (point-min) (point-max)))
          (rmin           (when mark-active (region-beginning)))
          (rmax           (when mark-active (region-end      ))) )
    (when (and mark-active
               transient-mark-mode)
      (unless (and (= rmin (point-min))
                   (= rmax (point-max)))
        (setq rovl (make-overlay rmin rmax))
        (overlay-put rovl 'priority 1000)
        (overlay-put rovl 'face 'region)))
    ;; copy the buffer, including fontification, and switch to it:
    (hfy-mark-trailing-whitespace)
    (setq css-sheet             (hfy-compile-stylesheet   )
          css-map               (hfy-compile-face-map     )
          invis-ranges          (hfy-find-invisible-ranges))
    (hfy-unmark-trailing-whitespace)
    (when rovl
      (delete-overlay rovl))
    (copy-to-buffer html-buffer (point-min) (point-max))
    (set-buffer     html-buffer)
    ;; rip out props that could interfere with our htmlization of the buffer:
    (remove-text-properties (point-min) (point-max) hfy-ignored-properties)
    ;; Apply overlay invisible spec
    (setq orig-ovls
          (sort orig-ovls
                (lambda (A B)
                  (> (or (cadr (memq 'priority (overlay-properties A))) 0)
                     (or (cadr (memq 'priority (overlay-properties B))) 0)))))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; at this point, html-buffer retains the fontification of the parent:
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; we don't really need or want text in the html buffer to be invisible, as
    ;; that can make it look like we've rendered invalid xhtml when all that's
    ;; happened is some tags are in the invisible portions of the buffer:
    (setq buffer-invisibility-spec nil)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; #####################################################################
    ;; if we are in etags mode, add properties to mark the anchors and links
    (if (and srcdir file)
        (progn
          (hfy-mark-tag-names srcdir file) ;; mark anchors
          (hfy-mark-tag-hrefs srcdir file))) ;; mark links
    ;; #####################################################################
    ;; mark the 'dangerous' characters
    ;;(message "marking dangerous characters")
    (hfy-html-enkludge-buffer)
    ;; trawl the position-based face-map, inserting span tags as we go
    ;; note that we cannot change any character positions before this point
    ;; or we will invalidate the map:
    ;; NB: This also means we have to trawl the map in descending file-offset
    ;; order, obviously.
    ;; ---------------------------------------------------------------------
    ;; Remember, inserting pushes properties to the right, which we don't
    ;; actually want to happen for link properties, so we have to flag
    ;; them and move them by hand - if you don't, you end up with
    ;;
    ;; <span class="foo"><a href="bar">texta</span><span class="bletch"></a>...
    ;;
    ;; instead of:
    ;;
    ;; <span class="foo"><a href="bar">texta</a></span><span class="bletch">...
    ;;
    ;; If my analysis of the problem is correct, we can detect link-ness by
    ;; either hfy-linkp or hfy-endl properties at the insertion point, but I
    ;; think we only need to relocate the hfy-endl property, as the hfy-linkp
    ;; property has already served its main purpose by this point.
    ;;(message "mapcar over the CSS-MAP")
    (message "invis-ranges:\n%S" invis-ranges)
    (dolist (point-face css-map)
      (let ((pt (car point-face))
            (fn (cdr point-face))
            (move-link       nil))
        (goto-char pt)
        (setq move-link
              (or (get-text-property pt 'hfy-linkp)
                  (get-text-property pt 'hfy-endl )))
        (if (eq 'end fn)
            (funcall hfy-end-span-handler)
          (if (not (and srcdir file))
              nil
            (when move-link
              (remove-text-properties (point) (1+ (point)) '(hfy-endl nil))
              (put-text-property pt (1+ pt) 'hfy-endl t) ))
          ;; if we have invisible blocks, we need to do some extra magic:
          (funcall hfy-begin-span-handler
                   (hfy-lookup fn css-sheet)
                   (and invis-ranges
                        (format "%s" (hfy-invisible-name pt invis-ranges)))
                   (and invis-ranges pt)
                   (and invis-ranges (assq pt invis-ranges)))
          (if (not move-link) nil
            ;;(message "removing prop2 @ %d" (point))
            (if (remove-text-properties (point) (1+ (point)) '(hfy-endl nil))
                (put-text-property pt (1+ pt) 'hfy-endl t))))))
    ;; #####################################################################
    ;; Invisibility
    ;; Maybe just make the text invisible in XHTML?
    ;; DONE -- big block of obsolete invisibility code elided here -- v
    ;; #####################################################################
    ;; (message "checking to see whether we should link...")
    (if (and srcdir file)
        (let ((lp 'hfy-link)
              (pt  (point-min))
              (pr  nil)
              (rr  nil))
          ;; (message "  yes we should.")
          ;; translate 'hfy-anchor properties to anchors
          (while (setq pt (next-single-property-change pt 'hfy-anchor))
            (if (setq pr (get-text-property pt 'hfy-anchor))
                (progn (goto-char pt)
                       (remove-text-properties pt (1+ pt) '(hfy-anchor nil))
                       (insert (concat "<a name=\"" pr "\"></a>")))))
          ;; translate alternate 'hfy-link and 'hfy-endl props to opening
          ;; and closing links. (this should avoid those spurious closes
          ;; we sometimes get by generating only paired tags)
          (setq pt (point-min))
          (while (setq pt (next-single-property-change pt lp))
            (if (not (setq pr (get-text-property pt lp))) nil
              (goto-char pt)
              (remove-text-properties pt (1+ pt) (list lp nil))
              (case lp
                (hfy-link
                 (if (setq rr (get-text-property pt 'hfy-inst))
                     (insert (format "<a name=\"%s\"></a>" rr)))
                 (insert (format "<a href=\"%s\">" pr))
                 (setq lp 'hfy-endl))
                (hfy-endl
                 (insert "</a>") (setq lp 'hfy-link)) ))) ))

    ;; #####################################################################
    ;; transform the dangerous chars. This changes character positions
    ;; since entities have > char length.
    ;; note that this deletes the dangerous characters, and therefore
    ;; destroys any properties they may contain (such as 'hfy-endl),
    ;; so we have to do this after we use said properties:
    ;; (message "munging dangerous characters")
    (hfy-html-dekludge-buffer)
    (unless (hfy-opt 'body-text-only)
      ;; insert the stylesheet at the top:
      (goto-char (point-min))

      ;;(message "inserting stylesheet")
      (insert (hfy-sprintf-stylesheet css-sheet file))

      (if (hfy-opt 'div-wrapper) (insert "<div class=\"default\">"))
      (insert "\n<pre>")
      (goto-char (point-max))
      (insert "</pre>\n")
      (if (hfy-opt 'div-wrapper) (insert "</div>"))
      ;;(message "inserting footer")
      (insert (funcall hfy-page-footer file)))
    ;; call any post html-generation hooks:
    (run-hooks 'hfy-post-html-hooks)
    ;; return the html buffer
    (set-buffer-modified-p nil)
    html-buffer))

(defun htmlfontify-string (string)
  "Take a STRING and return a fontified version of it.
It is assumed that STRING has text properties that allow it to be
fontified.  This is a simple convenience wrapper around
`htmlfontify-buffer'."
  (let* ((hfy-optimisations-1 (copy-sequence hfy-optimisations))
         (hfy-optimisations (add-to-list 'hfy-optimisations-1
                                         'skip-refontification)))
    (with-temp-buffer
      (insert string)
      (htmlfontify-buffer)
      (buffer-string))))

(defun hfy-force-fontification ()
  "Try to force font-locking even when it is optimized away."
  (run-hooks 'hfy-init-kludge-hook)
  (eval-and-compile (require 'font-lock))
  (if (boundp 'font-lock-cache-position)
      (or font-lock-cache-position
          (set 'font-lock-cache-position (make-marker))))
  (if (not noninteractive)
      (progn
        (message "hfy interactive mode (%S %S)" window-system major-mode)
        (when (and font-lock-defaults
                   font-lock-mode)
          (font-lock-fontify-region (point-min) (point-max) nil)))
    (message "hfy batch mode (%s:%S)"
             (or (buffer-file-name) (buffer-name)) major-mode)
    (when font-lock-defaults
      (font-lock-fontify-buffer)) ))

;;;###autoload
(defun htmlfontify-buffer (&optional srcdir file)
  "Create a new buffer, named for the current buffer + a .html extension,
containing an inline CSS-stylesheet and formatted CSS-markup HTML
that reproduces the look of the current Emacs buffer as closely
as possible.

Dangerous characters in the existing buffer are turned into HTML
entities, so you should even be able to do HTML-within-HTML
fontified display.

You should, however, note that random control or eight-bit
characters such as ^L (\x0c) or ¤ (\xa4) won't get mapped yet.

If the SRCDIR and FILE arguments are set, lookup etags derived
entries in the `hfy-tags-cache' and add HTML anchors and
hyperlinks as appropriate."
  (interactive)
  ;; pick up the file name in case we didn't receive it
  (if (not file)
      (progn (setq file (or (buffer-file-name) (buffer-name)))
             (if (string-match "/\\([^/]*\\)\\'" file)
                 (setq file (match-string 1 file)))) )

  (if (not (hfy-opt 'skip-refontification))
      (save-excursion ;; Keep region
        (hfy-force-fontification)))
  (if (called-interactively-p 'any) ;; display the buffer in interactive mode:
      (switch-to-buffer (hfy-fontify-buffer srcdir file))
    (hfy-fontify-buffer srcdir file)))

;; recursive file listing
(defun hfy-list-files (directory)
  "Return a list of files under DIRECTORY.
Strips any leading \"./\" from each filename."
  ;;(message "hfy-list-files");;DBUG
  ;; FIXME: this changes the dir of the current buffer.  Is that right??
  (cd directory)
  (mapcar (lambda (F) (if (string-match "^./\\(.*\\)" F) (match-string 1 F) F))
          (split-string (shell-command-to-string hfy-find-cmd))) )

;; strip the filename off, return a directory name
;; not a particularly thorough implementation, but it will be
;; fed pretty carefully, so it should be Ok:
(defun hfy-dirname (file)
  "Return everything preceding the last \"/\" from a relative filename FILE,
on the assumption that this will produce a relative directory name.
Hardly bombproof, but good enough in the context in which it is being used."
  ;;(message "hfy-dirname");;DBUG
  (let ((f (directory-file-name file)))
    (and (string-match "^\\(.*\\)/" f) (match-string 1 f))))

;; create a directory, cf mkdir -p
(defun hfy-make-directory (dir)
  "Approx. equivalent of mkdir -p DIR."
  ;;(message "hfy-make-directory");;DBUG
  (if (file-exists-p dir)
      (if (file-directory-p dir) t)
    (make-directory dir t)))

(defun hfy-text-p (srcdir file)
  "Is SRCDIR/FILE text?  Uses `hfy-istext-command' to determine this."
  (let* ((cmd (format hfy-istext-command (expand-file-name file srcdir)))
         (rsp (shell-command-to-string    cmd)))
    (string-match "text" rsp)))

;; open a file, check fontification, if fontified, write a fontified copy
;; to the destination directory, otherwise just copy the file:
(defun hfy-copy-and-fontify-file (srcdir dstdir file)
  "Open FILE in SRCDIR - if fontified, write a fontified copy to DSTDIR
adding an extension of `hfy-extn'.  Fontification is actually done by
`htmlfontify-buffer'.  If the buffer is not fontified, just copy it."
  ;;(message "hfy-copy-and-fontify-file");;DBUG
  (let (;;(fast-lock-minimum-size      hfy-fast-lock-save)
        ;;(font-lock-support-mode         'fast-lock-mode)
        ;;(window-system  (or window-system 'htmlfontify))
        (target nil)
        (source nil)
        (html   nil))
    (cd srcdir)
    (with-current-buffer (setq source (find-file-noselect file))
      ;; FIXME: Shouldn't this use expand-file-name?  --Stef
      (setq target (concat dstdir "/" file))
      (hfy-make-directory (hfy-dirname target))
      (if (not (hfy-opt 'skip-refontification)) (hfy-force-fontification))
      (if (or (hfy-fontified-p) (hfy-text-p srcdir file))
          (progn (setq html  (hfy-fontify-buffer srcdir file))
                 (set-buffer  html)
                 (write-file (concat target hfy-extn))
                 (kill-buffer html))
        ;; #o0200 == 128, but emacs20 doesn't know that
        (if (and (file-exists-p target) (not (file-writable-p target)))
            (set-file-modes target (logior (file-modes target) 128)))
        (copy-file (buffer-file-name source) target 'overwrite))
      (kill-buffer source)) ))

;; list of tags in file in srcdir
(defun hfy-tags-for-file (cache-hash file)
  "List of etags tags that have definitions in this FILE.
CACHE-HASH is the tags cache."
  ;;(message "hfy-tags-for-file");;DBUG
  (let* ((tag-list    nil))
    (if cache-hash
        (maphash
         (lambda (K V)
           (if (assoc file V)
               (setq tag-list (cons K tag-list))))
         cache-hash))
    tag-list))

;; mark the tags native to this file for anchors
(defun hfy-mark-tag-names (srcdir file)
  "Mark tags in FILE (lookup SRCDIR in `hfy-tags-cache') with the `hfy-anchor'
property, with a value of \"tag.line-number\"."
  ;;(message "(hfy-mark-tag-names %s %s)" srcdir file);;DBUG
  (let* ((cache-entry (assoc srcdir hfy-tags-cache))
         (cache-hash  (cadr cache-entry)))
    (if cache-hash
        (mapcar
         (lambda (TAG)
           (mapcar
            (lambda (TLIST)
              (if (string= file (car TLIST))
                  (let* ((line              (cadr TLIST) )
                         (chr              (caddr TLIST) )
                         (link (format "%s.%d" TAG line) ))
                    (put-text-property (+ 1 chr)
                                       (+ 2 chr)
                                       'hfy-anchor link))))
            (gethash TAG cache-hash)))
         (hfy-tags-for-file cache-hash file)))))

(defun hfy-relstub (file &optional start)
  "Return a \"../\" stub of the appropriate length for the current source
tree depth, as determined from FILE (a filename).
START is the offset at which to start looking for the / character in FILE."
  ;;(message "hfy-relstub");;DBUG
  (let ((c ""))
    (while (setq start (string-match "/" file start))
      (setq start (1+ start)) (setq c (concat c "../")))
    c))

(defun hfy-href-stub (this-file def-files tag)
  "Return an href stub for a tag href in THIS-FILE.
If DEF-FILES (list of files containing definitions for the tag in question)
contains only one entry, the href should link straight to that file.
Otherwise, the link should be to the index file.\n
We are not yet concerned with the file extensions/tag line number and so on at
this point.\n
If `hfy-split-index' is set, and the href wil be to an index file rather than
a source file, append a .X to `hfy-index-file', where X is the uppercased
first character of TAG.\n
See also `hfy-relstub', `hfy-index-file'."
  ;;(message "hfy-href-stub");;DBUG
  ;; FIXME: Why not use something like
  ;; (file-relative-name (if ...) (file-name-directory this-file)) ?  --Stef
  (concat
   (hfy-relstub this-file)
   (if (= 1 (length def-files)) (car def-files)
     (if (not hfy-split-index) hfy-index-file
       (concat hfy-index-file "." (upcase (substring tag 0 1)))))) )

(defun hfy-href (this-file def-files tag tag-map)
  "Return a relative href to the tag in question, based on\n
THIS-FILE `hfy-link-extn' `hfy-extn' DEF-FILES TAG and TAG-MAP\n
THIS-FILE is the current source file
DEF-FILES is a list of file containing possible link endpoints for TAG
TAG is the tag in question
TAG-MAP is the entry in `hfy-tags-cache'."
  ;;(message "hfy-href");;DBUG
  (concat
   (hfy-href-stub this-file def-files tag)
   (or hfy-link-extn hfy-extn) "#" tag ;;(.src -> .html)
   (if (= 1 (length def-files))
       (concat "." (format "%d" (cadr (assoc (car def-files) tag-map)))))) )

(defun hfy-word-regex (string)
  "Return a regex that matches STRING as the first `match-string', with non
word characters on either side."
  ;; FIXME: Should this use [^$[:alnum:]_] instead?  --Stef
  (concat "[^$A-Za-z_0-9]\\(" (regexp-quote string) "\\)[^A-Za-z_0-9]"))

;; mark all tags for hyperlinking, except the tags at
;; their own points of definition, iyswim:
(defun hfy-mark-tag-hrefs (srcdir file)
  "Mark href start points with the `hfy-link' prop (value: href string).\n
Mark href end points with the `hfy-endl' prop (value t).\n
Avoid overlapping links, and mark links in descending length of
tag name in order to prevent subtags from usurping supertags,
\(eg \"term\" for \"terminal\").
SRCDIR is the directory being \"published\".
FILE is the specific file we are rendering."
  ;;(message "hfy-mark-tag-hrefs");;DBUG
  (let ((cache-entry (assoc srcdir hfy-tags-cache))
        (list-cache  (assoc srcdir hfy-tags-sortl))
        (rmap-cache  (assoc srcdir hfy-tags-rmap ))
        (no-comment  (hfy-opt  'zap-comment-links))
        (no-strings  (hfy-opt  'zap-string-links ))
        (cache-hash                            nil)
        (tags-list                             nil)
        (tags-rmap                             nil)
        (case-fold-search                      nil))
    ;; extract the tag mapping hashes (fwd and rev) and the tag list:
    (if (and (setq cache-hash (cadr cache-entry))
             (setq tags-rmap  (cadr rmap-cache ))
             (setq tags-list  (cadr list-cache )))
        (mapcar
         (lambda (TAG)
           (let* ((start            nil)
                  (stop             nil)
                  (href             nil)
                  (name             nil)
                  (case-fold-search nil)
                  (tmp-point        nil)
                  (maybe-start      nil)
                  (face-at          nil)
                  (rmap-entry       nil)
                  (rnew-elt         nil)
                  (rmap-line        nil)
                  (tag-regex       (hfy-word-regex TAG))
                  (tag-map         (gethash TAG cache-hash))
                  (tag-files       (mapcar #'car tag-map)))
             ;; find instances of TAG and do what needs to be done:
             (goto-char (point-min))
             (while (search-forward TAG nil 'NOERROR)
               (setq tmp-point   (point)
                     maybe-start (- (match-beginning 0) 1))
               (goto-char maybe-start)
               (if (not (looking-at tag-regex))
                   nil
                 (setq start   (match-beginning 1))
                 (setq stop    (match-end 1))
                 (setq face-at
                       (and (or no-comment no-strings) (hfy-face-at start)))
                 (if (listp face-at)
                     (setq face-at (cadr (memq :inherit face-at))))
                 (if (or (text-property-any   start  (1+ stop)  'hfy-linkp  t)
                         (and no-comment (eq 'font-lock-comment-face face-at))
                         (and no-strings (eq 'font-lock-string-face  face-at)))
                     nil ;; already a link, NOOP

                   ;; set a reverse map entry:
                   (setq rmap-line  (line-number-at-pos)
                         rmap-entry (gethash    TAG     tags-rmap)
                         rnew-elt   (list  file  rmap-line  start)
                         rmap-entry (cons   rnew-elt   rmap-entry)
                         name       (format "%s.%d" TAG rmap-line))
                   (put-text-property start (1+ start) 'hfy-inst name)
                   (puthash TAG rmap-entry tags-rmap)

                   ;; mark the link. link to index if the tag has > 1 def
                   ;; add the line number to the #name if it does not:
                   (setq href (hfy-href file tag-files TAG tag-map))
                   (put-text-property start (1+ start) 'hfy-link  href)
                   (put-text-property stop  (1+ stop ) 'hfy-endl  t   )
                   (put-text-property start (1+ stop ) 'hfy-linkp t   )))
               (goto-char tmp-point)) ))
         tags-list) )))

(defun hfy-shell ()
  "Return `shell-file-name', or \"/bin/sh\" if it is a non-Bourne shell."
  (if (string-match "\\<bash\\>\\|\\<sh\\>\\|\\<dash\\>" shell-file-name)
      shell-file-name
    (or hfy-shell-file-name "/bin/sh")))

;; cache the #(tag => file line point) entries for files under srcdir
;; and cache the descending sorted list of tags in the relevant alist,
;; also keyed by srcdir:
(defun hfy-load-tags-cache (srcdir)
  "Run `hfy-etags-cmd' on SRCDIR, then call `hfy-parse-tags-buffer'."
  ;;(message "hfy-load-tags-cache");;DBUG
  (let ((etags-buffer  (get-buffer-create     "*hfy-tags*"))
        (etags-command (format hfy-etags-cmd hfy-etags-bin))
        (shell-file-name                        (hfy-shell)))
    (cd srcdir)
    (shell-command  etags-command etags-buffer)
    (hfy-parse-tags-buffer srcdir etags-buffer)) )

;; break this out from `hfy-load-tags-cache' to make the tar file
;; functionality easier to implement.
;; ( tar file functionality not merged here because it requires a
;;   hacked copy of etags capable of tagging stdin: if Francesco
;;   Potorti accepts a patch, or otherwise implements stdin tagging,
;;   then I will provide a `htmlfontify-tar-file' defun )
(defun hfy-parse-tags-buffer (srcdir buffer)
  "Parse a BUFFER containing etags formatted output, loading the
`hfy-tags-cache' and `hfy-tags-sortl' entries for SRCDIR."
  (let ((cache-entry     (assoc srcdir    hfy-tags-cache))
        (tlist-cache     (assoc srcdir    hfy-tags-sortl))
        (trmap-cache     (assoc srcdir    hfy-tags-rmap ))
        (cache-hash nil) (trmap-hash nil) (tags-list  nil)
        (hash-entry nil) (tag-string nil) (tag-line   nil)
        (tag-point  nil) (new-entry  nil) (etags-file nil))

    ;; (re)initialize the tag reverse map:
    (if trmap-cache (setq trmap-hash (cadr trmap-cache))
      (setq trmap-hash (make-hash-table :test 'equal))
      (setq hfy-tags-rmap (list (list srcdir trmap-hash) hfy-tags-rmap)))
    (clrhash trmap-hash)

    ;; (re)initialize the tag cache:
    (if cache-entry (setq cache-hash (cadr cache-entry))
      (setq cache-hash (make-hash-table :test 'equal))
      (setq hfy-tags-cache (list (list srcdir cache-hash) hfy-tags-cache)))
    (clrhash cache-hash)

    ;; cache the TAG => ((file line point) (file line point) ... ) entries:
    (with-current-buffer buffer
      (goto-char (point-min))

      (while (and (looking-at "^\x0c") (= 0 (forward-line 1)))
        ;;(message "^L boundary")
        (if (and (looking-at "^\\(.+\\),\\([0-9]+\\)$")
                 (= 0 (forward-line 1)))
            (progn
              (setq etags-file (match-string 1))
              ;;(message "TAGS for file: %s" etags-file)
              (while (and (looking-at hfy-etag-regex) (= 0 (forward-line 1)))
                (setq tag-string (match-string 1))
                (if (= 0 (length tag-string)) nil ;; noop
                  (setq tag-line   (round (string-to-number (match-string 2))))
                  (setq tag-point  (round (string-to-number (match-string 3))))
                  (setq hash-entry (gethash tag-string  cache-hash))
                  (setq new-entry  (list etags-file tag-line tag-point))
                  (push new-entry hash-entry)
                  ;;(message "HASH-ENTRY %s %S" tag-string new-entry)
                  (puthash tag-string hash-entry cache-hash)))) )))

    ;; cache a list of tags in descending length order:
    (maphash (lambda (K _V) (push K tags-list)) cache-hash)
    (setq tags-list (sort tags-list (lambda (A B) (< (length B) (length A)))))

    ;; put the tag list into the cache:
    (if tlist-cache (setcar (cdr tlist-cache) tags-list)
      (push (list srcdir tags-list) hfy-tags-sortl))

    ;; return the number of tags found:
    (length tags-list) ))

(defun hfy-prepare-index-i (srcdir dstdir filename &optional stub map)
  "Prepare a tags index buffer for SRCDIR.
`hfy-tags-cache' must already have an entry for SRCDIR for this to work.
`hfy-page-header', `hfy-page-footer', `hfy-link-extn' and `hfy-extn'
all play a part here.\n
If STUB is set, prepare an (appropriately named) index buffer
specifically for entries beginning with STUB.\n
If MAP is set, use that instead of `hfy-tags-cache'.
FILENAME is the name of the file being indexed.
DSTDIR is the output directory, where files will be written."
  ;;(message "hfy-write-index");;DBUG
  (let ((cache-entry  (assoc srcdir (or map hfy-tags-cache)))
        (cache-hash    nil)
        (tag-list      nil)
        (index-file
         (concat filename (if stub (concat "." stub) "") hfy-extn))
        (index-buf     nil))
    (if (not (and cache-entry
                  (setq cache-hash (cadr cache-entry))
                  (setq index-buf  (get-buffer-create index-file))))
        nil ;; noop
      (maphash (lambda (K _V) (push K tag-list)) cache-hash)
      (setq tag-list (sort tag-list 'string<))
      (set-buffer index-buf)
      (erase-buffer)
      (insert (funcall hfy-page-header filename "<!-- CSS -->"))
      (insert "<table class=\"index\">\n")

      (dolist (TAG tag-list)
        (let ((tag-started nil))
          (dolist (DEF (gethash TAG cache-hash))
            (if (and stub (not (string-match (concat "^" stub) TAG)))
                nil ;; we have a stub and it didn't match: NOOP
              (let ((file (car  DEF))
                    (line (cadr DEF)))
                (insert
                 (format
                  (concat
                   "  <tr>                                   \n"
                   "   <td>%s</td>                           \n"
                   "   <td><a href=\"%s%s\">%s</a></td>      \n"
                   "   <td><a href=\"%s%s#%s.%d\">%d</a></td>\n"
                   "  </tr>                                  \n")
                  (if (string= TAG tag-started) "&nbsp;"
                    (format "<a name=\"%s\">%s</a>" TAG TAG))
                  file (or hfy-link-extn hfy-extn) file
                  file (or hfy-link-extn hfy-extn) TAG line line))
                (setq tag-started TAG))))))
      (insert "</table>\n")
      (insert (funcall hfy-page-footer filename))
      (and dstdir (cd dstdir))
      (set-visited-file-name index-file)
      index-buf) ))

(defun hfy-prepare-index (srcdir dstdir)
  "Return a list of index buffer(s), as determined by `hfy-split-index'.
SRCDIR and DSTDIR are the source and output directories respectively."
  (if (not hfy-split-index)
      (list (hfy-prepare-index-i srcdir dstdir hfy-index-file nil))
    (let ((stub-list     nil)
          (cache-hash    nil)
          (index-list    nil)
          (cache-entry  (assoc srcdir hfy-tags-cache)))
      (if (and cache-entry (setq cache-hash (cadr cache-entry)))
          (maphash
           (lambda (K _V)
             (let ((stub (upcase (substring K 0 1))))
               (if (member stub stub-list)
                   nil ;; seen this already: NOOP
                 (setq
                  stub-list  (cons stub stub-list)
                  index-list (cons (hfy-prepare-index-i srcdir
                                                        dstdir
                                                        hfy-index-file
                                                        stub)
                                   index-list)) )))
           cache-hash) )
      index-list)))

(defun hfy-prepare-tag-map (srcdir dstdir)
  "Prepare the counterpart(s) to the index buffer(s) - a list of buffers
with the same structure, but listing (and linking to) instances of tags
\(as opposed to their definitions).\n
SRCDIR and DSTDIR are the source and output directories respectively.
See also `hfy-prepare-index', `hfy-split-index'."
  (if (not hfy-split-index)
      (list (hfy-prepare-index-i srcdir
                                 dstdir
                                 hfy-instance-file
                                 nil
                                 hfy-tags-rmap))
    (let ((stub-list     nil)
          (cache-hash    nil)
          (index-list    nil)
          (cache-entry  (assoc srcdir hfy-tags-rmap)))

      (if (and cache-entry (setq cache-hash (cadr cache-entry)))
          (maphash
           (lambda (K _V)
             (let ((stub (upcase (substring K 0 1))))
               (if (member stub stub-list)
                   nil ;; seen this already: NOOP
                 (setq
                  stub-list  (cons stub stub-list)
                  index-list (cons (hfy-prepare-index-i srcdir
                                                        dstdir
                                                        hfy-instance-file
                                                        stub
                                                        hfy-tags-rmap)
                                   index-list)) )))
           cache-hash) )
      index-list)))

(defun hfy-subtract-maps (srcdir)
  "Internal function - strips definitions of tags from the instance map.
SRCDIR is the directory being \"published\".
See also `hfy-tags-cache', `hfy-tags-rmap'."
  (let ((new-list nil)
        (old-list nil)
        (def-list nil)
        (exc-list nil)
        (fwd-map (cadr (assoc srcdir hfy-tags-cache)))
        (rev-map (cadr (assoc srcdir hfy-tags-rmap )))
        (taglist (cadr (assoc srcdir hfy-tags-sortl))))
    (dolist (TAG taglist)
      (setq def-list (gethash TAG fwd-map)
            old-list (gethash TAG rev-map)
            exc-list (mapcar (lambda (P) (list (car P) (cadr P))) def-list)
            new-list  nil)
      (dolist (P old-list)
        (or (member (list (car P) (cadr P)) exc-list)
            (push P new-list)))
      (puthash TAG new-list rev-map))))

(defun htmlfontify-run-etags (srcdir)
  "Load the etags cache for SRCDIR.
See also `hfy-load-tags-cache'."
  (interactive "D source directory: ")
  (hfy-load-tags-cache (directory-file-name srcdir)))

;;(defun hfy-test-read-args (foo bar)
;;  (interactive "D source directory: \nD target directory: ")
;;  (message "foo: %S\nbar: %S" foo bar))

(defun hfy-save-kill-buffers (buffer-list &optional dstdir)
  (dolist (B buffer-list)
    (set-buffer B)
    (and dstdir (file-directory-p dstdir) (cd dstdir))
    (save-buffer)
    (kill-buffer B)))

;;;###autoload
(defun htmlfontify-copy-and-link-dir (srcdir dstdir &optional f-ext l-ext)
  "Trawl SRCDIR and write fontified-and-hyperlinked output in DSTDIR.
F-EXT and L-EXT specify values for `hfy-extn' and `hfy-link-extn'.\n
You may also want to set `hfy-page-header' and `hfy-page-footer'."
  (interactive "D source directory: \nD output directory: ")
  ;;(message "htmlfontify-copy-and-link-dir")
  (setq srcdir (directory-file-name srcdir))
  (setq dstdir (directory-file-name dstdir))
  (let ((source-files "SETME: list of source files, relative to srcdir")
        (tr-cache  (assoc srcdir hfy-tags-rmap))
        (hfy-extn            (or f-ext ".html"))
        (hfy-link-extn       (or l-ext ".html")))
    ;; oops, forgot to load etags for srcdir:
    (if tr-cache nil
      (message "autoload of tags cache")
      (hfy-load-tags-cache srcdir)
      (setq tr-cache (assoc srcdir hfy-tags-rmap)))
    ;; clear out the old cache:
    (clrhash   (cadr tr-cache))
    (hfy-make-directory dstdir)
    (setq source-files (hfy-list-files srcdir))
    (dolist (file source-files)
      (hfy-copy-and-fontify-file srcdir dstdir file))
    (hfy-subtract-maps srcdir)
    (hfy-save-kill-buffers (hfy-prepare-index   srcdir dstdir) dstdir)
    (hfy-save-kill-buffers (hfy-prepare-tag-map srcdir dstdir) dstdir) ))

;; name of the init file we want:
(defun hfy-initfile ()
  "Return the expected location of the htmlfontify specific init/custom file."
  (let* ((file (or (getenv "HFY_INITFILE") ".hfy.el")))
    (expand-file-name file "~") ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; incomplete as yet : transfer hook settings to hfy init file:
;; (defalias 'hfy-set-hooks 'custom-set-variables)

;; (defun hfy-pp-hook (H)
;;   (and (string-match "-hook\\'" (symbol-name H))
;;        (boundp H)
;;        (symbol-value H)
;;        (insert (format "\n '(%S %S)" H (symbol-value H)))
;;        )
;;   )

;; (defun hfy-save-hooks ()
;;   (let ((custom-file (hfy-initfile)))
;;     (custom-save-delete 'hfy-set-hooks)
;;     (let ((standard-output (current-buffer)))
;;       (princ "(hfy-set-hooks\n;;auto-generated, only one copy allowed\n")
;;       (mapatoms 'hfy-pp-hook)
;;       (insert "\n)")
;;       )
;;     )
;;   )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'hfy-init-progn 'progn)

(defun hfy-save-initvar (sym)
  (princ (format "(setq %s\n '" sym))
  (pp    (symbol-value sym))
  (princ ")\n"))

(defun htmlfontify-save-initfile ()
  "Save the htmlfontify settings to the htmlfontify init file."
  (interactive)
  (let* ((start-pos       nil)
         (custom-file     (hfy-initfile))
         (standard-output (find-file-noselect custom-file 'nowarn)))
    (save-excursion
      (custom-save-delete 'hfy-init-progn)
      (setq start-pos (point))
      (princ "(hfy-init-progn\n;;auto-generated, only one copy allowed\n")
      ;; FIXME: This saving&restoring of global customization
      ;; variables can interfere with other customization settings for
      ;; those vars (in .emacs or in Customize).
      (mapc 'hfy-save-initvar
            '(auto-mode-alist interpreter-mode-alist))
      (princ ")\n")
      (indent-region start-pos (point) nil))
    (custom-save-all) ))

(defun htmlfontify-load-initfile ()
  "Load the htmlfontify specific init/custom file."
  (interactive)
  (let ((file (hfy-initfile)))
    (load file 'NOERROR nil nil) ))


;;;### (autoloads (hfy-fallback-colour-values htmlfontify-load-rgb-file)
;;;;;;  "hfy-cmap" "hfy-cmap.el" "ef24066922f1e27b7580d572f12fabbe")
;;; Generated autoloads from hfy-cmap.el

(autoload 'htmlfontify-load-rgb-file "hfy-cmap" "\
Load an X11 style rgb.txt FILE.
Search `hfy-rgb-load-path' if FILE is not specified.
Loads the variable `hfy-rgb-txt-colour-map', which is used by
`hfy-fallback-colour-values'.

\(fn &optional FILE)" t nil)

(autoload 'hfy-fallback-colour-values "hfy-cmap" "\
Use a fallback method for obtaining the rgb values for a color.

\(fn COLOUR-STRING)" nil nil)

;;;***


(provide 'htmlfontify)

;; Local Variables:
;; coding: utf-8
;; End:

;;; htmlfontify.el ends here
