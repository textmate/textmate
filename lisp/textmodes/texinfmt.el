;;; texinfmt.el --- format Texinfo files into Info files

;; Copyright (C) 1985-1986, 1988, 1990-1998, 2000-2012
;;   Free Software Foundation, Inc.

;; Maintainer: Robert J. Chassell <bug-texinfo@gnu.org>
;; Keywords: maint, tex, docs

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

;;; Emacs lisp functions to convert Texinfo files to Info files.

(defvar texinfmt-version "2.42 of  7 Jul 2006")

(defun texinfmt-version (&optional here)
  "Show the version of texinfmt.el in the minibuffer.
If optional argument HERE is non-nil, insert info at point."
  (interactive "P")
  (let ((version-string
         (format "Version of \`texinfmt.el\': %s" texinfmt-version)))
    (if here
        (insert version-string)
      (if (called-interactively-p 'interactive)
          (message "%s" version-string)
        version-string))))


;;; Variable definitions

(require 'texinfo)          ; So `texinfo-footnote-style' is defined.
(require 'texnfo-upd)       ; So `texinfo-section-types-regexp' is defined.

(defvar texinfo-vindex)
(defvar texinfo-findex)
(defvar texinfo-cindex)
(defvar texinfo-pindex)
(defvar texinfo-tindex)
(defvar texinfo-kindex)
(defvar texinfo-last-node)
(defvar texinfo-node-names)
(defvar texinfo-enclosure-list)
(defvar texinfo-alias-list)
(defvar texinfo-fold-nodename-case nil)

(defvar texinfo-command-start)
(defvar texinfo-command-end)
(defvar texinfo-command-name)
(defvar texinfo-defun-type)
(defvar texinfo-last-node-pos)
(defvar texinfo-stack)
(defvar texinfo-short-index-cmds-alist)
(defvar texinfo-short-index-format-cmds-alist)
(defvar texinfo-format-filename)
(defvar texinfo-footnote-number)

(defvar texinfo-raisesections-alist
  '((@chapter . @chapter)             ; Cannot go higher
    (@unnumbered . @unnumbered)
    (@centerchap . @unnumbered)

    (@majorheading . @majorheading)
    (@chapheading . @chapheading)
    (@appendix . @appendix)

    (@section . @chapter)
    (@unnumberedsec . @unnumbered)
    (@heading . @chapheading)
    (@appendixsec . @appendix)

    (@subsection . @section)
    (@unnumberedsubsec . @unnumberedsec)
    (@subheading . @heading)
    (@appendixsubsec . @appendixsec)

    (@subsubsection . @subsection)
    (@unnumberedsubsubsec . @unnumberedsubsec)
    (@subsubheading . @subheading)
    (@appendixsubsubsec . @appendixsubsec))
  "*An alist of next higher levels for chapters, sections, etc...
For example, section to chapter, subsection to section.
Used by `texinfo-raise-lower-sections'.
The keys specify types of section; the values correspond to the next
higher types.")

(defvar texinfo-lowersections-alist
  '((@chapter . @section)
    (@unnumbered . @unnumberedsec)
    (@centerchap . @unnumberedsec)
    (@majorheading . @heading)
    (@chapheading . @heading)
    (@appendix . @appendixsec)

    (@section . @subsection)
    (@unnumberedsec . @unnumberedsubsec)
    (@heading . @subheading)
    (@appendixsec . @appendixsubsec)

    (@subsection . @subsubsection)
    (@unnumberedsubsec . @unnumberedsubsubsec)
    (@subheading . @subsubheading)
    (@appendixsubsec . @appendixsubsubsec)

    (@subsubsection . @subsubsection) ; Cannot go lower.
    (@unnumberedsubsubsec . @unnumberedsubsubsec)
    (@subsubheading . @subsubheading)
    (@appendixsubsubsec . @appendixsubsubsec))
  "*An alist of next lower levels for chapters, sections, etc...
For example, chapter to section, section to subsection.
Used by `texinfo-raise-lower-sections'.
The keys specify types of section; the values correspond to the next
lower types.")

;;; Syntax table

(defvar texinfo-format-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\" " " st)
    (modify-syntax-entry ?\\ " " st)
    (modify-syntax-entry ?@ "\\" st)
    (modify-syntax-entry ?\^q "\\" st)
    (modify-syntax-entry ?\[ "." st)
    (modify-syntax-entry ?\] "." st)
    (modify-syntax-entry ?\( "." st)
    (modify-syntax-entry ?\) "." st)
    (modify-syntax-entry ?{ "(}" st)
    (modify-syntax-entry ?} "){" st)
    (modify-syntax-entry ?\' "." st)
    st))


;;; Top level buffer and region formatting functions

;;;###autoload
(defun texinfo-format-buffer (&optional nosplit)
  "Process the current buffer as texinfo code, into an Info file.
The Info file output is generated in a buffer visiting the Info file
name specified in the @setfilename command.

Non-nil argument (prefix, if interactive) means don't make tag table
and don't split the file if large.  You can use `Info-tagify' and
`Info-split' to do these manually."
  (interactive "P")
  (let ((lastmessage "Formatting Info file...")
	(coding-system-for-write buffer-file-coding-system))
    (message lastmessage)
    (widen)
    (texinfo-format-buffer-1)
    (Info-tagify)
    (if nosplit
        nil
      (if (> (buffer-size) (+ 50000 Info-split-threshold))
          (progn
            (message (setq lastmessage "Splitting Info file..."))
            (Info-split))))
    (message (concat lastmessage
                     (if (called-interactively-p 'interactive)
			 "done.  Now save it." "done.")))))

(defvar texinfo-region-buffer-name "*Info Region*"
  "*Name of the temporary buffer used by \\[texinfo-format-region].")

(defvar texinfo-pre-format-hook nil
  "Hook called before the conversion of the Texinfo file to Info format.
The functions on this hook are called with argument BUFFER, the buffer
containing the Texinfo file.")

;; These come from tex-mode.el.
(defvar tex-start-of-header)
(defvar tex-end-of-header)

;;;###autoload
(defun texinfo-format-region (region-beginning region-end)
  "Convert the current region of the Texinfo file to Info format.
This lets you see what that part of the file will look like in Info.
The command is bound to \\[texinfo-format-region].  The text that is
converted to Info is stored in a temporary buffer."
  (interactive "r")
  (message "Converting region to Info format...")
  (let (texinfo-command-start
        texinfo-command-end
        texinfo-command-name
        texinfo-vindex
        texinfo-findex
        texinfo-cindex
        texinfo-pindex
        texinfo-tindex
        texinfo-kindex
        texinfo-stack
        (texinfo-format-filename "")
        texinfo-example-start
        texinfo-last-node-pos
        texinfo-last-node
        texinfo-node-names
        (texinfo-footnote-number 0)
        last-input-buffer
        (fill-column-for-info fill-column)
        (input-buffer (current-buffer))
        (input-directory default-directory)
        (header-text "")
        (header-beginning 1)
        (header-end 1))

;;; Copy lines between beginning and end of header lines,
;;;    if any, or else copy the `@setfilename' line, if any.
    (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (let ((search-end (line-beginning-position 101)))
            (if (or
                 ;; Either copy header text.
                 (and
                  (prog1
                      (search-forward tex-start-of-header search-end t)
                    (forward-line 1)
                    ;; Mark beginning of header.
                    (setq header-beginning (point)))
                  (prog1
                      (search-forward tex-end-of-header nil t)
                    (beginning-of-line)
                    ;; Mark end of header
                    (setq header-end (point))))
                 ;; Or copy @filename line.
                 (prog2
                  (goto-char (point-min))
                  (search-forward "@setfilename" search-end t)
                  (beginning-of-line)
                  (setq header-beginning (point))
                  (forward-line 1)
                  (setq header-end (point))))

                ;; Copy header
                (setq header-text
                      (buffer-substring-no-properties
                       (min header-beginning region-beginning)
                       header-end))))))

;;; Find a buffer to use.
    (switch-to-buffer (get-buffer-create texinfo-region-buffer-name))
    (setq buffer-read-only t)
    (let ((inhibit-read-only t))
    (erase-buffer)
    ;; Insert the header into the buffer.
    (insert header-text)
    ;; Insert the region into the buffer.
    (insert-buffer-substring
     input-buffer
     (max region-beginning header-end)
     region-end)
    (run-hook-with-args 'texinfo-pre-format-hook input-buffer)
    ;; Make sure region ends in a newline.
    (or (= (preceding-char) ?\n)
        (insert "\n"))

    (goto-char (point-min))
    (texinfo-mode)
    (message "Converting region to Info format...")
    (setq fill-column fill-column-for-info)
    ;; Install a syntax table useful for scanning command operands.
    (set-syntax-table texinfo-format-syntax-table)

    ;; Insert @include files so `texinfo-raise-lower-sections' can
    ;; work on them without losing track of multiple
    ;; @raise/@lowersections commands.
    (while (re-search-forward "^@include" nil t)
      (setq texinfo-command-end (point))
      (let ((filename (concat input-directory
                              (texinfo-parse-line-arg))))
        (re-search-backward "^@include")
        (delete-region (point) (line-beginning-position 2))
        (message "Reading included file: %s" filename)
        (save-excursion
          (save-restriction
            (narrow-to-region
             (point)
             (+ (point) (car (cdr (insert-file-contents filename)))))
            (goto-char (point-min))
            ;; Remove `@setfilename' line from included file, if any,
            ;; so @setfilename command not duplicated.
            (if (re-search-forward "^@setfilename" (line-end-position 100) t)
		(delete-region (line-beginning-position 1)
			       (line-beginning-position 2)))))))

    ;; Raise or lower level of each section, if necessary.
    (goto-char (point-min))
    (texinfo-raise-lower-sections)
    ;; Append @refill to appropriate paragraphs for filling.
    (goto-char (point-min))
    (texinfo-append-refill)
    ;; If the region includes the effective end of the data,
    ;; discard everything after that.
    (goto-char (point-max))
    (if (re-search-backward "^@bye" nil t)
        (delete-region (point) (point-max)))
    ;; Make sure buffer ends in a newline.
    (or (= (preceding-char) ?\n)
        (insert "\n"))
    ;; Don't use a previous value of texinfo-enclosure-list.
    (setq texinfo-enclosure-list nil)
    (setq texinfo-alias-list nil)

    (goto-char (point-min))
    (if (looking-at "\\\\input[ \t]+texinfo")
        (delete-region (point) (line-beginning-position 2)))

    ;; Insert Info region title text.
    (goto-char (point-min))
    (if (search-forward "@setfilename" (line-beginning-position 101) t)
        (progn
          (setq texinfo-command-end (point))
          (beginning-of-line)
          (setq texinfo-command-start (point))
          (let ((arg (texinfo-parse-arg-discard)))
            (insert " "
              texinfo-region-buffer-name
              " buffer for:  `")
            (insert (file-name-nondirectory (expand-file-name arg)))
            (insert "',        -*-Text-*-\n")))
      ;; Else no `@setfilename' line
      (insert " "
              texinfo-region-buffer-name
              " buffer                       -*-Text-*-\n"))
    (insert "produced by `texinfo-format-region'\n"
            "from a region in: "
            (if (buffer-file-name input-buffer)
                  (concat "`"
                          (file-name-sans-versions
                           (file-name-nondirectory
                            (buffer-file-name input-buffer)))
                          "'")
                (concat "buffer `" (buffer-name input-buffer) "'"))
              "\nusing `texinfmt.el' version "
              texinfmt-version
              ".\n\n")

    ;; Now convert for real.
    (goto-char (point-min))
    (texinfo-format-scan)
    (goto-char (point-min))
    (Info-tagify input-buffer)
    (goto-char (point-min))
    (message "Done."))))

;;;###autoload
(defun texi2info (&optional nosplit)
  "Convert the current buffer (written in Texinfo code) into an Info file.
The Info file output is generated in a buffer visiting the Info file
names specified in the @setfilename command.

This function automatically updates all node pointers and menus, and
creates a master menu.  This work is done on a temporary buffer that
is automatically removed when the Info file is created.  The original
Texinfo source buffer is not changed.

Non-nil argument (prefix, if interactive) means don't split the file
if large.  You can use `Info-split' to do this manually."
  (interactive "P")
  (let ((temp-buffer (concat  "*--" (buffer-name) "--temporary-buffer*" )))
    (message "First updating nodes and menus, then creating Info file.")
    ;;  (sit-for 2)
    (copy-to-buffer temp-buffer (point-min) (point-max))
    (switch-to-buffer temp-buffer)
    (texinfo-master-menu t)
    (message "Now creating Info file.")
    (sit-for 2)
    (texinfo-format-buffer nosplit)
    (save-buffer)
    (kill-buffer temp-buffer)))


;;; Primary internal formatting function for the whole buffer.

(defun texinfo-format-buffer-1 ()
  (let (texinfo-format-filename
        texinfo-example-start
        texinfo-command-start
        texinfo-command-end
        texinfo-command-name
        texinfo-last-node
        texinfo-last-node-pos
        texinfo-vindex
        texinfo-findex
        texinfo-cindex
        texinfo-pindex
        texinfo-tindex
        texinfo-kindex
        texinfo-stack
        texinfo-node-names
        (texinfo-footnote-number 0)
        last-input-buffer
        outfile
        (fill-column-for-info fill-column)
        (input-buffer (current-buffer))
        (input-directory default-directory))
    (setq texinfo-enclosure-list nil)
    (setq texinfo-alias-list nil)
    (save-excursion
      (goto-char (point-min))
      (or (search-forward "@setfilename" nil t)
          (error "Texinfo file needs an `@setfilename FILENAME' line"))
      (setq texinfo-command-end (point))
      (setq outfile (texinfo-parse-line-arg)))

    (find-file outfile)
    (texinfo-mode)
    (erase-buffer)
    (buffer-disable-undo)

    (message "Formatting Info file: %s" outfile)
    (setq texinfo-format-filename
          (file-name-nondirectory (expand-file-name outfile)))

    (setq fill-column fill-column-for-info)
    (set-syntax-table texinfo-format-syntax-table)

    (insert-buffer-substring input-buffer)
    (run-hook-with-args 'texinfo-pre-format-hook input-buffer)
    (message "Converting %s to Info format..." (buffer-name input-buffer))

    ;; Insert @include files so `texinfo-raise-lower-sections' can
    ;; work on them without losing track of multiple
    ;; @raise/@lowersections commands.
    (goto-char (point-min))
    (while (re-search-forward "^@include" nil t)
      (setq texinfo-command-end (point))
      (let ((filename (concat input-directory
                              (texinfo-parse-line-arg))))
        (re-search-backward "^@include")
        (delete-region (point) (line-beginning-position 2))
        (message "Reading included file: %s" filename)
        (save-excursion
          (save-restriction
            (narrow-to-region
             (point)
             (+ (point) (car (cdr (insert-file-contents filename)))))
            (goto-char (point-min))
            ;; Remove `@setfilename' line from included file, if any,
            ;; so @setfilename command not duplicated.
            (if (re-search-forward "^@setfilename" (line-end-position 100) t)
		(delete-region (line-beginning-position 1)
			       (line-beginning-position 2)))))))
    ;; Raise or lower level of each section, if necessary.
    (goto-char (point-min))
    (texinfo-raise-lower-sections)
    ;; Append @refill to appropriate paragraphs
    (goto-char (point-min))
    (texinfo-append-refill)
    (goto-char (point-min))
    (search-forward "@setfilename")
    (beginning-of-line)
    (delete-region (point-min) (point))
    ;; Remove @bye at end of file, if it is there.
    (goto-char (point-max))
    (if (search-backward "@bye" nil t)
        (delete-region (point) (point-max)))
    ;; Make sure buffer ends in a newline.
    (or (= (preceding-char) ?\n)
        (insert "\n"))
    ;; Scan the whole buffer, converting to Info format.
    (texinfo-format-scan)
    (goto-char (point-min))
    ;; Insert info about how this file was made.
    (insert "Info file: "
            texinfo-format-filename ",    -*-Text-*-\n"
            "produced by `texinfo-format-buffer'\n"
            ;; Date string removed so that regression testing is easier.
            ;; "on "
            ;; (insert (format-time-string "%e %b %Y")) " "
            "from file"
            (if (buffer-file-name input-buffer)
                (concat " `"
                        (file-name-sans-versions
                         (file-name-nondirectory
                          (buffer-file-name input-buffer)))
                        "'")
              (concat "buffer `" (buffer-name input-buffer) "'"))
            "\nusing `texinfmt.el' version "
            texinfmt-version
            ".\n\n")
    ;; Return data for indices.
    (list outfile
          texinfo-vindex texinfo-findex texinfo-cindex
          texinfo-pindex texinfo-tindex texinfo-kindex)))


;;; Perform non-@-command file conversions: quotes and hyphens

(defun texinfo-format-convert (min max)
  ;; Convert left and right quotes to typewriter font quotes.
  (goto-char min)
  (while (search-forward "``" max t)
    (replace-match "\""))
  (goto-char min)
  (while (search-forward "''" max t)
    (replace-match "\""))
  ;; Convert three hyphens in a row to two.
  (goto-char min)
  (while (re-search-forward "\\( \\|\\w\\)\\(---\\)\\( \\|\\w\\)" max t)
    (delete-region (1+ (match-beginning 2)) (+ 2 (match-beginning 2)))))


;;; Handle paragraph filling

;; Keep as concatenated lists for ease of maintenance

(defvar texinfo-no-refill-regexp
  (concat
   "^@"
   "\\("
   ;; add "itemize\\|"   (from experiment of 2001 Nov 28)
   ;;     because of a problem with @end itemize@refill
   ;;     I don't know if this causes other problems.
   ;;     I suspect itemized lists don't get filled properly and a
   ;;     more precise fix is required.  Bob
   ;; commented out on 2005 Feb 28 by Bob
   ;; "itemize\\|"
   "direntry\\|"
   "lisp\\|"
   "smalllisp\\|"
   "example\\|"
   "smallexample\\|"
   "display\\|"
   "smalldisplay\\|"
   "format\\|"
   "smallformat\\|"
   "flushleft\\|"
   "flushright\\|"
   "menu\\|"
   "multitable\\|"
   "titlepage\\|"
   "iftex\\|"
   "ifhtml\\|"
   "tex\\|"
   "html"
   "\\)")
  "Regexp specifying environments in which paragraphs are not filled.")

(defvar texinfo-accent-commands
  (concat
   "@^\\|"
   "@`\\|"
   "@'\\|"
   "@\"\\|"
   "@,\\|"
   "@=\\|"
   "@~\\|"
   "@OE{\\|"
   "@oe{\\|"
   "@AA{\\|"
   "@aa{\\|"
   "@AE{\\|"
   "@ae{\\|"
   "@ss{\\|"
   "@questiondown{\\|"
   "@exclamdown{\\|"
   "@L{\\|"
   "@l{\\|"
   "@O{\\|"
   "@o{\\|"
   "@dotaccent{\\|"
   "@ubaraccent{\\|"
   "@d{\\|"
   "@H{\\|"
   "@ringaccent{\\|"
   "@tieaccent{\\|"
   "@u{\\|"
   "@v{\\|"
   "@dotless{"
   ))

(defvar texinfo-part-of-para-regexp
  (concat
   "^@"
   "\\("
   "b{\\|"
   "bullet{\\|"
   "cite{\\|"
   "code{\\|"
   "email{\\|"
   "emph{\\|"
   "equiv{\\|"
   "error{\\|"
   "expansion{\\|"
   "file{\\|"
   "i{\\|"
   "inforef{\\|"
   "kbd{\\|"
   "key{\\|"
   "lisp{\\|"
   "minus{\\|"
   "point{\\|"
   "print{\\|"
   "pxref{\\|"
   "r{\\|"
   "ref{\\|"
   "result{\\|"
   "samp{\\|"
   "sc{\\|"
   "t{\\|"
   "TeX{\\|"
   "today{\\|"
   "url{\\|"
   "var{\\|"
   "w{\\|"
   "xref{\\|"
   "@-\\|"    ; @- is a discretionary hyphen (not an accent) (a noop).
   texinfo-accent-commands
   "\\)"
   )
  "Regexp specifying @-commands found within paragraphs.")

(defun texinfo-append-refill ()
  "Append @refill at end of each paragraph that should be filled.
Do not append @refill to paragraphs within @example and similar environments.
Do not append @refill to paragraphs containing @w{TEXT} or @*."

  ;; It is necessary to append @refill before other processing because
  ;; the other processing removes information that tells Texinfo
  ;; whether the text should or should not be filled.

  (while (< (point) (point-max))
    (let ((refill-blank-lines "^[ \t\n]*$")
          (case-fold-search nil))       ; Don't confuse @TeX and @tex....
      (beginning-of-line)
      ;; 1. Skip over blank lines;
      ;;    skip over lines beginning with @-commands,
      ;;    but do not skip over lines
      ;;      that are no-refill environments such as @example or
      ;;      that begin with within-paragraph @-commands such as @code.
      (while (and (looking-at (concat "^@\\|^\\\\\\|" refill-blank-lines))
                  (not (looking-at
                        (concat
                         "\\("
                         texinfo-no-refill-regexp
                         "\\|"
                         texinfo-part-of-para-regexp
                         "\\)")))
                  (< (point) (point-max)))
        (forward-line 1))
      ;; 2. Skip over @example and similar no-refill environments.
      (if (looking-at texinfo-no-refill-regexp)
          (let ((environment (match-string-no-properties 1)))
            (progn (re-search-forward (concat "^@end " environment) nil t)
                   (forward-line 1)))
        ;; Else
        ;; 3. Do not refill a paragraph containing @w or @*, or ending
        ;;    with @<newline> followed by a newline.
        (if (or (>= (point) (point-max))
                (re-search-forward
                 "@w{\\|@\\*\\|@\n\n"
                 (save-excursion (forward-paragraph)
                                 (line-beginning-position 2))
                 t))
            ;; Go to end of paragraph and do nothing.
            (forward-paragraph)
          ;; 4. Else go to end of paragraph and insert @refill
          (forward-paragraph)
          (forward-line -1)
	  (let ((line-beg (point)))
	    (end-of-line)
	    (delete-region
	     (point)
	     (save-excursion (skip-chars-backward " \t") (point)))
	    (forward-char 1)
	    (unless (re-search-backward "@c[ \t\n]\\|@comment[ \t\n]" line-beg t)
	      (forward-char -1))
	    (unless (re-search-backward "@refill\\|^[ \t]*@" line-beg t)
	      (insert "@refill")))
          (forward-line 1))))))


;;; Handle `@raisesections' and `@lowersections' commands

;; These commands change the hierarchical level of chapter structuring
;; commands.
;;
;; @raisesections changes @subsection to @section,
;;                        @section    to @chapter,
;;                        etc.
;;
;; @lowersections changes @chapter    to @section
;;                        @subsection to @subsubsection,
;;                        etc.
;;
;; An @raisesections/@lowersections command changes only those
;; structuring commands that follow the @raisesections/@lowersections
;; command.
;;
;; Repeated @raisesections/@lowersections continue to raise or lower
;; the heading level.
;;
;; An @lowersections command cancels an @raisesections command, and
;; vice versa.
;;
;; You cannot raise or lower "beyond" chapters or subsubsections, but
;; trying to do so does not elicit an error---you just get more
;; headings that mean the same thing as you keep raising or lowering
;; (for example, after a single @raisesections, both @chapter and
;; @section produce chapter headings).

(defun texinfo-raise-lower-sections ()
  "Raise or lower the hierarchical level of chapters, sections, etc.

This function acts according to `@raisesections' and `@lowersections'
commands in the Texinfo file.

For example, an `@lowersections' command is useful if you wish to
include what is written as an outer or standalone Texinfo file in
another Texinfo file as an inner, included file.  The `@lowersections'
command changes chapters to sections, sections to subsections and so
on.

@raisesections changes @subsection to @section,
                       @section    to @chapter,
                       @heading    to @chapheading,
                       etc.

@lowersections changes @chapter    to @section,
                       @subsection to @subsubsection,
                       @heading    to @subheading,
                       etc.

An `@raisesections' or `@lowersections' command changes only those
structuring commands that follow the `@raisesections' or
`@lowersections' command.

An `@lowersections' command cancels an `@raisesections' command, and
vice versa.

Repeated use of the commands continue to raise or lower the hierarchical
level a step at a time.

An attempt to raise above `chapters' reproduces chapter commands; an
attempt to lower below subsubsections reproduces subsubsection
commands."

  ;; `texinfo-section-types-regexp' is defined in `texnfo-upd.el';
  ;; it is a regexp matching chapter, section, other headings
  ;; (but not the top node).

  (let (type (level 0))
    (while
        (re-search-forward
         (concat
          "\\(\\(^@\\(raise\\|lower\\)sections\\)\\|\\("
          texinfo-section-types-regexp
          "\\)\\)")
         nil t)
      (beginning-of-line)
      (save-excursion (setq type (read (current-buffer))))
      (cond

       ;; 1. Increment level
       ((eq type '@raisesections)
        (setq level (1+ level))
        (delete-region
         (point) (line-beginning-position 2)))

       ;; 2. Decrement level
       ((eq type '@lowersections)
        (setq level (1- level))
        (delete-region
         (point) (line-beginning-position 2)))

       ;; Now handle structuring commands
       ((cond

         ;; 3. Raise level when positive
         ((> level 0)
          (let ((count level)
                (new-level type))
            (while (> count 0)
              (setq new-level
                    (cdr (assq new-level texinfo-raisesections-alist)))
              (setq count (1- count)))
            (kill-word 1)
            (insert (symbol-name new-level))))

         ;; 4. Do nothing except move point when level is zero
         ((= level 0) (forward-line 1))

         ;; 5. Lower level when positive
         ((< level 0)
          (let ((count level)
                (new-level type))
            (while (< count 0)
              (setq new-level
                    (cdr (assq new-level texinfo-lowersections-alist)))
              (setq count (1+ count)))
            (kill-word 1)
            (insert (symbol-name new-level))))))))))

;;; Perform those texinfo-to-info conversions that apply to the whole input
;;; uniformly.

(defun texinfo-format-scan ()
  (texinfo-format-convert (point-min) (point-max))
  ;; Search for @copying, which has to be first since the
  ;; @insertcopying command then inserts the text elsewhere.
  (goto-char (point-min))
  (when (search-forward "@copying" nil t)
    (texinfo-copying))
  (while (search-forward "@insertcopying" nil t)
    (delete-region (match-beginning 0) (match-end 0))

    (texinfo-insertcopying))
  ;; Scan for other @-commands.
  (goto-char (point-min))
  (while (search-forward "@" nil t)
    ;;
    ;; These are the single-character accent commands: @^ @` @' @" @= @~
    ;; In Info, they are simply quoted and the @ deleted.
    ;; Other single-character commands:
    ;; @* forces a line break,
    ;; @- is a discretionary hyphenation point; does nothing in Info.
    ;; @<space>, @<tab>, @<newline> each produce a single space,
    ;;    unless followed by a newline.
    ;;
    ;; Old version 2.34 expression: (looking-at "[@{}^'` *\"?!]")
    (if (looking-at "[@{}^'`\"=~ \t\n*?!-]")
        ;; @*, causes a line break.
        (cond
         ;; @*, a line break
         ((= (following-char) ?*)
          ;; remove command
          (delete-region (1- (point)) (1+ (point)))
          ;; insert return if not at end of line;
          ;; else line is already broken.
          (if (not (= (following-char) ?\n))
              (insert ?\n)))
         ;; @-, deleted
         ((= (following-char) ?-)
          (delete-region (1- (point)) (1+ (point))))
         ;; @<space>, @<tab>, @<newline>: produce a single space,
         ;;    unless followed by a newline.
         ((= (following-char) ? )
          (delete-region (1- (point)) (1+ (point)))
          ;; insert single space if not at end of line;
          ;; else line is already broken.
          (if (not (= (following-char) ?\n))
              (insert ? )))
         ((= (following-char) ?\t)
          (delete-region (1- (point)) (1+ (point)))
          ;; insert single space if not at end of line;
          ;; else line is already broken.
          (if (not (= (following-char) ?\n))
              (insert ? )))
         ;; following char is a carriage return
         ((= (following-char) ?\n)
          ;; remove command
          (delete-region (1- (point)) (1+ (point)))
          ;; insert single space if not at end of line;
          ;; else line is already broken.
          (if (not (= (following-char) ?\n))
              (insert ? )))
         ;; Otherwise: the other characters are simply quoted.  Delete the @.
         (t
         (delete-char -1)
	 ;; Be compatible with makeinfo: if @' and its ilk are
	 ;; followed by a @ without a brace, barf.
	 (if (looking-at "[\"'^`~=]")
	     (progn
	       (if (= (char-after (1+ (point))) ?@)
		   (error "Use braces to give a command as an argument to @%c"
			  (following-char)))
	       (forward-char 1)
	       ;; @' etc. can optionally accept their argument in
	       ;; braces (makeinfo supports that).
	       (when (looking-at "{")
		 (let ((start (point)))
		   (forward-list 1)
		   (delete-char -1)
		   (goto-char start)
		   (delete-char 1))))
	   (forward-char 1))))
      ;; @ is followed by a command-word; find the end of the word.
      (setq texinfo-command-start (1- (point)))
      (if (= (char-syntax (following-char)) ?w)
          (forward-word 1)
        (forward-char 1))
      (setq texinfo-command-end (point))
      ;; Detect the case of two @-commands in a row;
      ;; process just the first one.
      (goto-char (1+ texinfo-command-start))
      (skip-chars-forward "^@" texinfo-command-end)
      (setq texinfo-command-end (point))
      ;; Handle let aliasing
      (setq texinfo-command-name
            (let (trial
                  (cmdname
                   (buffer-substring-no-properties
                    (1+ texinfo-command-start) texinfo-command-end)))
              (while (setq trial (assoc cmdname texinfo-alias-list))
                (setq cmdname (cdr trial)))
            (intern cmdname)))
      ;; Call the handler for this command.
      (let ((enclosure-type
             (assoc
              (symbol-name texinfo-command-name)
              texinfo-enclosure-list)))
        (if enclosure-type
            (progn
              (insert
               (car (car (cdr enclosure-type)))
               (texinfo-parse-arg-discard)
               (car (cdr (car (cdr enclosure-type)))))
              (goto-char texinfo-command-start))
          (let ((cmd (get texinfo-command-name 'texinfo-format)))
            (if cmd (funcall cmd) (texinfo-unsupported)))))))

  (cond (texinfo-stack
         (goto-char (nth 2 (car texinfo-stack)))
         (error "Unterminated @%s" (car (car texinfo-stack)))))

  ;; Remove excess whitespace
  (let ((whitespace-silent t))
    (whitespace-cleanup)))

(defvar texinfo-copying-text ""
  "Text of the copyright notice and copying permissions.")

(defun texinfo-copying ()
  "Copy the copyright notice and copying permissions from the Texinfo file,
as indicated by the @copying ... @end copying command;
insert the text with the @insertcopying command."
  (let ((beg (progn (beginning-of-line) (point)))
        (end  (progn (re-search-forward "^@end copying[ \t]*\n") (point))))
    (setq texinfo-copying-text
          (buffer-substring-no-properties
           (save-excursion (goto-char beg) (line-beginning-position 2))
           (save-excursion (goto-char end) (line-beginning-position 0))))
    (delete-region beg end)))

(defun texinfo-insertcopying ()
  "Insert the copyright notice and copying permissions from the Texinfo file,
which are indicated by the @copying ... @end copying command."
  (insert (concat "\n" texinfo-copying-text)))

(put 'begin 'texinfo-format 'texinfo-format-begin)
(defun texinfo-format-begin ()
  (texinfo-format-begin-end 'texinfo-format))

(put 'end 'texinfo-format 'texinfo-format-end)
(defun texinfo-format-end ()
  (texinfo-format-begin-end 'texinfo-end))

(defun texinfo-format-begin-end (prop)
  (setq texinfo-command-name (intern (texinfo-parse-line-arg)))
  (let ((cmd (get texinfo-command-name prop)))
    (if cmd (funcall cmd)
      (texinfo-unsupported))))

;;; Parsing functions

(defun texinfo-parse-line-arg ()
  "Return argument of @-command as string.
Argument is separated from command either by a space or by a brace.
If a space, return rest of line, with beginning and ending white
space removed.  If a brace, return string between braces.
Leave point after argument."
  (goto-char texinfo-command-end)
  (let ((start (point)))
    (cond ((looking-at " ")
           (skip-chars-forward " ")
           (setq start (point))
           (end-of-line)
           (skip-chars-backward " ")
           (delete-region (point) (progn (end-of-line) (point)))
           (setq texinfo-command-end (1+ (point))))
          ((looking-at "{")
           (setq start (1+ (point)))
           (forward-list 1)
           (setq texinfo-command-end (point))
           (forward-char -1))
          (t
           (error "Invalid texinfo command arg format")))
    (prog1 (buffer-substring-no-properties start (point))
           (if (eolp) (forward-char 1)))))

(defun texinfo-parse-expanded-arg ()
  (goto-char texinfo-command-end)
  (let ((start (point))
        marker)
    (cond ((looking-at " ")
           (skip-chars-forward " ")
           (setq start (point))
           (end-of-line)
           (setq texinfo-command-end (1+ (point))))
          ((looking-at "{")
           (setq start (1+ (point)))
           (forward-list 1)
           (setq texinfo-command-end (point))
           (forward-char -1))
          (t
           (error "Invalid texinfo command arg format")))
    (setq marker (move-marker (make-marker) texinfo-command-end))
    (texinfo-format-expand-region start (point))
    (setq texinfo-command-end (marker-position marker))
    (move-marker marker nil)
    (prog1 (buffer-substring-no-properties start (point))
           (if (eolp) (forward-char 1)))))

(defun texinfo-format-expand-region (start end)
  (save-restriction
    (narrow-to-region start end)
    (let (texinfo-command-start
          texinfo-command-end
          texinfo-command-name
          texinfo-stack)
      (texinfo-format-scan))
    (goto-char (point-max))))

(defun texinfo-parse-arg-discard ()
  "Delete command and argument; return argument of command."
  (prog1 (texinfo-parse-line-arg)
         (texinfo-discard-command)))

(defun texinfo-discard-command ()
  (delete-region texinfo-command-start texinfo-command-end))

(defun texinfo-optional-braces-discard ()
  "Discard braces following command, if any."
  (goto-char texinfo-command-end)
  (let ((start (point)))
    (cond ((looking-at "[ \t]*\n"))     ; do nothing
          ((looking-at "{")             ; remove braces, if any
           (forward-list 1)
           (setq texinfo-command-end (point)))
          (t
           (error
            "Invalid `texinfo-optional-braces-discard' format \(need braces?\)")))
    (delete-region texinfo-command-start texinfo-command-end)))

(defun texinfo-format-parse-line-args ()
  (let ((start (1- (point)))
        next beg end
        args)
    (skip-chars-forward " ")
    (while (not (eolp))
      (setq beg (point))
      (re-search-forward "[\n,]")
      (setq next (point))
      (if (bolp) (setq next (1- next)))
      (forward-char -1)
      (skip-chars-backward " ")
      (setq end (point))
      (push (if (> end beg) (buffer-substring-no-properties beg end))
            args)
      (goto-char next)
      (skip-chars-forward " "))
    (if (eolp) (forward-char 1))
    (setq texinfo-command-end (point))
    (nreverse args)))

(defun texinfo-format-parse-args ()
  (let ((start (1- (point)))
        next beg end
        args)
    (search-forward "{")
    (save-excursion
      (texinfo-format-expand-region
       (point)
       (save-excursion (up-list 1) (1- (point)))))
    ;; The following does not handle cross references of the form:
    ;; `@xref{bullet, , @code{@@bullet}@{@}}.' because the
    ;; re-search-forward finds the first right brace after the second
    ;; comma.
    (while (/= (preceding-char) ?\})
      (skip-chars-forward " \t\n")
      (setq beg (point))
      (re-search-forward "[},]")
      (setq next (point))
      (forward-char -1)
      (skip-chars-backward " \t\n")
      (setq end (point))
      (cond ((< beg end)
             (goto-char beg)
             (while (search-forward "\n" end t)
               (replace-match " "))))
      (push (if (> end beg) (buffer-substring-no-properties beg end))
            args)
      (goto-char next))
    ;;(if (eolp) (forward-char 1))
    (setq texinfo-command-end (point))
    (nreverse args)))

(defun texinfo-format-parse-defun-args ()
  (goto-char texinfo-command-end)
  (let ((start (point)))
    (end-of-line)
    (setq texinfo-command-end (1+ (point)))
    (let ((marker (move-marker (make-marker) texinfo-command-end)))
      (texinfo-format-expand-region start (point))
      (setq texinfo-command-end (marker-position marker))
      (move-marker marker nil))
    (goto-char start)
    (let ((args '())
          beg end)
      (skip-chars-forward " ")
      (while (not (eolp))
        (cond ((looking-at "{")
               (setq beg (1+ (point)))
               (forward-list 1)
               (setq end (1- (point))))
              (t
               (setq beg (point))
               (re-search-forward "[\n ]")
               (forward-char -1)
               (setq end (point))))
        (push (buffer-substring-no-properties beg end) args)
        (skip-chars-forward " "))
      (forward-char 1)
      (nreverse args))))

(defun texinfo-discard-line ()
  (goto-char texinfo-command-end)
  (skip-chars-forward " \t")
  (or (eolp)
      (error "Extraneous text at end of command line"))
  (goto-char texinfo-command-start)
  (or (bolp)
      (error "Extraneous text at beginning of command line"))
  (delete-region (point) (progn (forward-line 1) (point))))

(defun texinfo-discard-line-with-args ()
  (goto-char texinfo-command-start)
  (delete-region (point) (progn (forward-line 1) (point))))


;;; @setfilename

;; Only `texinfo-format-buffer' handles @setfilename with this
;; definition; `texinfo-format-region' handles @setfilename, if any,
;; specially.
(put 'setfilename 'texinfo-format 'texinfo-format-setfilename)
(defun texinfo-format-setfilename ()
  (texinfo-parse-arg-discard))

;;; @node, @menu, @detailmenu

(put 'node 'texinfo-format 'texinfo-format-node)
(put 'nwnode 'texinfo-format 'texinfo-format-node)
(defun texinfo-format-node ()
  (let* ((args (texinfo-format-parse-line-args))
         (name (nth 0 args))
         (next (nth 1 args))
         (prev (nth 2 args))
         (up (nth 3 args)))
    (texinfo-discard-command)
    (setq texinfo-last-node name)
    (let ((tem (if texinfo-fold-nodename-case (downcase name) name)))
      (if (assoc tem texinfo-node-names)
          (error "Duplicate node name: %s" name)
        (push (list tem) texinfo-node-names)))
    (setq texinfo-footnote-number 0)
    ;; insert "\n\^_" unconditionally since this is what info is looking for
    (insert "\n\^_\nFile: " texinfo-format-filename
            ", Node: " name)
    (if next
        (insert ", Next: " next))
    (if prev
        (insert ", Prev: " prev))
    (if up
        (insert ", Up: " up))
    (insert ?\n)
    (setq texinfo-last-node-pos (point))))

(put 'anchor 'texinfo-format 'texinfo-anchor)
(defun texinfo-anchor ()
  (let (anchor-string
        (here (- (point) 7))  ; save location of beginning of `@anchor'
        (arg (texinfo-parse-arg-discard)))
    (if (looking-at " ")      ; since a space may be left after -discard
      (delete-char 1))
    (forward-paragraph)
    (let ((end (point)))
      (if (save-excursion
            (backward-word 1)
            (search-forward "@refill" end t))
          (setq anchor-string "@anchor-yes-refill")
        (setq anchor-string "@anchor-no-refill")))
      (goto-char here)
      (insert anchor-string "{" arg "}")))

(put 'menu 'texinfo-format 'texinfo-format-menu)
(defun texinfo-format-menu ()
  (texinfo-discard-line)
  (insert "* Menu:\n\n"))

(put 'menu 'texinfo-end 'texinfo-discard-command)

;; The @detailmenu should be removed eventually.

;; According to Karl Berry, 31 August 1996:
;;
;; You don't like, I don't like it.  I agree, it would be better just to
;; fix the bug [in `makeinfo'].  ..  At this point, since inserting those
;; two commands in the Elisp fn is trivial, I don't especially want to
;; expend more effort...
;;
;; I added a couple sentences of documentation to the manual (putting the
;; blame on makeinfo where it belongs :-().

(put 'detailmenu 'texinfo-format 'texinfo-discard-line)
(put 'detailmenu 'texinfo-end 'texinfo-discard-command)

;; (Also see `texnfo-upd.el')


;;; Cross references

;; @xref {NODE, FNAME, NAME, FILE, DOCUMENT}
;; -> *Note FNAME: (FILE)NODE
;;   If FILE is missing,
;;    *Note FNAME: NODE
;;   If FNAME is empty and NAME is present
;;    *Note NAME: Node
;;   If both NAME and FNAME are missing
;;    *Note NODE::
;;   texinfo ignores the DOCUMENT argument.
;; -> See section <xref to NODE> [NAME, else NODE], page <xref to NODE>
;;   If FILE is specified, (FILE)NODE is used for xrefs.
;;   If fifth argument DOCUMENT is specified, produces
;;    See section <xref to NODE> [NAME, else NODE], page <xref to NODE>
;;    of DOCUMENT

;; @ref             a reference that does not put `See' or `see' in
;;                  the hardcopy and is the same as @xref in Info
(put 'ref 'texinfo-format 'texinfo-format-xref)

(put 'xref 'texinfo-format 'texinfo-format-xref)
(defun texinfo-format-xref ()
  (let ((args (texinfo-format-parse-args)))
    (texinfo-discard-command)
    (insert "*Note ")
    (let ((fname (or (nth 1 args) (nth 2 args))))
      (if (null (or fname (nth 3 args)))
          (insert (car args) "::")
        (insert (or fname (car args)) ": ")
        (if (nth 3 args)
            (insert "(" (nth 3 args) ")"))
        (and (car args) (insert (car args)))))))

(put 'pxref 'texinfo-format 'texinfo-format-pxref)
(defun texinfo-format-pxref ()
  (texinfo-format-xref)
  (or (save-excursion
        (forward-char -2)
        (looking-at "::"))
      (insert ".")))

;; @inforef{NODE, FNAME, FILE}
;; Like @xref{NODE, FNAME,,FILE} in texinfo.
;; In Tex, generates "See Info file FILE, node NODE"
(put 'inforef 'texinfo-format 'texinfo-format-inforef)
(defun texinfo-format-inforef ()
  (let ((args (texinfo-format-parse-args)))
    (texinfo-discard-command)
    (if (nth 1 args)
        (insert "*Note " (nth 1 args) ": (" (nth 2 args) ")" (car args))
      (insert "*Note " "(" (nth 2 args) ")" (car args) "::"))))


;;; URL Reference: @uref

;; @uref produces a reference to a uniform resource locator (URL).
;; It takes one mandatory argument, the URL, and one optional argument,
;; the text to display (the default is the URL itself).

(put 'uref 'texinfo-format 'texinfo-format-uref)
(defun texinfo-format-uref ()
  "Format URL and optional URL-TITLE.
Insert ` ... ' around URL if no URL-TITLE argument;
otherwise, insert URL-TITLE followed by URL in parentheses."
  (let ((args (texinfo-format-parse-args)))
    (texinfo-discard-command)
    ;; if url-title
    (if (nth 1 args)
        (insert  (nth 1 args) " (" (nth 0 args) ")")
      (insert "`" (nth 0 args) "'"))
    (goto-char texinfo-command-start)))


;;; Section headings

(put 'majorheading 'texinfo-format 'texinfo-format-chapter)
(put 'chapheading 'texinfo-format 'texinfo-format-chapter)
(put 'ichapter 'texinfo-format 'texinfo-format-chapter)
(put 'chapter 'texinfo-format 'texinfo-format-chapter)
(put 'iappendix 'texinfo-format 'texinfo-format-chapter)
(put 'appendix 'texinfo-format 'texinfo-format-chapter)
(put 'iunnumbered 'texinfo-format 'texinfo-format-chapter)
(put 'top 'texinfo-format 'texinfo-format-chapter)
(put 'unnumbered 'texinfo-format 'texinfo-format-chapter)
(put 'centerchap 'texinfo-format 'texinfo-format-chapter)
(defun texinfo-format-chapter ()
  (texinfo-format-chapter-1 ?*))

(put 'heading 'texinfo-format 'texinfo-format-section)
(put 'isection 'texinfo-format 'texinfo-format-section)
(put 'section 'texinfo-format 'texinfo-format-section)
(put 'iappendixsection 'texinfo-format 'texinfo-format-section)
(put 'appendixsection 'texinfo-format 'texinfo-format-section)
(put 'iappendixsec 'texinfo-format 'texinfo-format-section)
(put 'appendixsec 'texinfo-format 'texinfo-format-section)
(put 'iunnumberedsec 'texinfo-format 'texinfo-format-section)
(put 'unnumberedsec 'texinfo-format 'texinfo-format-section)
(defun texinfo-format-section ()
  (texinfo-format-chapter-1 ?=))

(put 'subheading 'texinfo-format 'texinfo-format-subsection)
(put 'isubsection 'texinfo-format 'texinfo-format-subsection)
(put 'subsection 'texinfo-format 'texinfo-format-subsection)
(put 'iappendixsubsec 'texinfo-format 'texinfo-format-subsection)
(put 'appendixsubsec 'texinfo-format 'texinfo-format-subsection)
(put 'iunnumberedsubsec 'texinfo-format 'texinfo-format-subsection)
(put 'unnumberedsubsec 'texinfo-format 'texinfo-format-subsection)
(defun texinfo-format-subsection ()
  (texinfo-format-chapter-1 ?-))

(put 'subsubheading 'texinfo-format 'texinfo-format-subsubsection)
(put 'isubsubsection 'texinfo-format 'texinfo-format-subsubsection)
(put 'subsubsection 'texinfo-format 'texinfo-format-subsubsection)
(put 'iappendixsubsubsec 'texinfo-format 'texinfo-format-subsubsection)
(put 'appendixsubsubsec 'texinfo-format 'texinfo-format-subsubsection)
(put 'iunnumberedsubsubsec 'texinfo-format 'texinfo-format-subsubsection)
(put 'unnumberedsubsubsec 'texinfo-format 'texinfo-format-subsubsection)
(defun texinfo-format-subsubsection ()
  (texinfo-format-chapter-1 ?.))

(defun texinfo-format-chapter-1 (belowchar)
  (let ((arg (texinfo-parse-arg-discard)))
    (message "Formatting: %s ... " arg)   ; So we can see where we are.
    (insert ?\n arg ?\n "@SectionPAD " belowchar ?\n)
    (forward-line -2)))

(put 'SectionPAD 'texinfo-format 'texinfo-format-sectionpad)
(defun texinfo-format-sectionpad ()
  (let ((str (texinfo-parse-arg-discard)))
    (forward-char -1)
    (let ((column (current-column)))
      (forward-char 1)
      (while (> column 0)
        (insert str)
        (setq column (1- column))))
    (insert ?\n)))


;;; Space controlling commands:  @. and @:, and the soft hyphen.

(put '\. 'texinfo-format 'texinfo-format-\.)
(defun texinfo-format-\. ()
  (texinfo-discard-command)
  (insert "."))

(put '\: 'texinfo-format 'texinfo-format-\:)
(defun texinfo-format-\: ()
  (texinfo-discard-command))

(put '\- 'texinfo-format 'texinfo-format-soft-hyphen)
(defun texinfo-format-soft-hyphen ()
  (texinfo-discard-command))


;;; @kbdinputstyle, @vskip, headings & footings
;;  These commands for not for Info and should never
;;  appear in an Info environment; but if they do,
;;  this causes them to be discarded.

;; @kbdinputstyle
(put 'kbdinputstyle 'texinfo-format 'texinfo-discard-line-with-args)

;; @vskip
(put 'vskip 'texinfo-format 'texinfo-discard-line-with-args)

;; headings & footings
(put 'evenfooting 'texinfo-format 'texinfo-discard-line-with-args)
(put 'evenheading 'texinfo-format 'texinfo-discard-line-with-args)
(put 'oddfooting 'texinfo-format 'texinfo-discard-line-with-args)
(put 'oddheading 'texinfo-format 'texinfo-discard-line-with-args)
(put 'everyfooting 'texinfo-format 'texinfo-discard-line-with-args)
(put 'everyheading 'texinfo-format 'texinfo-discard-line-with-args)


;;; @documentdescription ... @end documentdescription
;;  This command is for HTML output and should  never
;;  appear in an Info environment; but if it does,
;;  this causes it to be discarded.

(put 'documentdescription 'texinfo-format 'texinfo-format-documentdescription)
(defun texinfo-format-documentdescription ()
  (delete-region texinfo-command-start
		 (progn (re-search-forward "^@end documentdescription[ \t]*\n")
			(point))))



;;; @center, @sp, and @br

(put 'center 'texinfo-format 'texinfo-format-center)
(defun texinfo-format-center ()
  (let ((arg (texinfo-parse-expanded-arg)))
    (texinfo-discard-command)
    (insert arg)
    (insert ?\n)
    (save-restriction
      (goto-char (1- (point)))
      (let ((indent-tabs-mode nil))
        (center-line)))))

(put 'sp 'texinfo-format 'texinfo-format-sp)
(defun texinfo-format-sp ()
  (let* ((arg (texinfo-parse-arg-discard))
         (num (read arg)))
    (insert-char ?\n num)))

(put 'br 'texinfo-format 'texinfo-format-paragraph-break)
(defun texinfo-format-paragraph-break ()
  "Force a paragraph break.
If used within a line, follow `@br' with braces."
  (texinfo-optional-braces-discard)
  ;; insert one return if at end of line;
  ;; else insert two returns, to generate a blank line.
  (if (= (following-char) ?\n)
      (insert ?\n)
    (insert-char ?\n 2)))


;;; @footnote  and  @footnotestyle

;; In Texinfo, footnotes are created with the `@footnote' command.
;; This command is followed immediately by a left brace, then by the text of
;; the footnote, and then by a terminating right brace.  The
;; template for a footnote is:
;;
;;      @footnote{TEXT}
;;
;; Info has two footnote styles:
;;
;;    * In the End of node style, all the footnotes for a single node
;;      are placed at the end of that node.  The footnotes are
;;      separated from the rest of the node by a line of dashes with
;;      the word `Footnotes' within it.
;;
;;    * In the Separate node style, all the footnotes for a single node
;;      are placed in an automatically constructed node of their own.

;; Footnote style is specified by the @footnotestyle command, either
;;    @footnotestyle separate
;; or
;;    @footnotestyle end
;;
;; The default is  separate

(defvar texinfo-footnote-style "separate"
  "Footnote style, either separate or end.")

(put 'footnotestyle 'texinfo-format 'texinfo-footnotestyle)
(defun texinfo-footnotestyle ()
  "Specify whether footnotes are at end of node or in separate nodes.
Argument is either end or separate."
  (setq texinfo-footnote-style (texinfo-parse-arg-discard)))

(put 'footnote 'texinfo-format 'texinfo-format-footnote)
(defun texinfo-format-footnote ()
  "Format a footnote in either end of node or separate node style.
The   texinfo-footnote-style  variable controls which style is used."
  (setq texinfo-footnote-number (1+ texinfo-footnote-number))
  (cond ((string= texinfo-footnote-style "end")
         (texinfo-format-end-node))
        ((string= texinfo-footnote-style "separate")
         (texinfo-format-separate-node))))

(defun texinfo-format-separate-node ()
  "Format footnote in Separate node style, with notes in own node.
The node is constructed automatically."
  (let* (start
         (arg (texinfo-parse-line-arg))
         (node-name-beginning
          (save-excursion
            (re-search-backward
             "^File: \\w+\\(\\w\\|\\s_\\|\\.\\|,\\)*[ \t]+Node:")
            (match-end 0)))
         (node-name
          (save-excursion
            (buffer-substring-no-properties
             (progn (goto-char node-name-beginning) ; skip over node command
                    (skip-chars-forward " \t")  ; and over spaces
                    (point))
             (if (search-forward "," (line-end-position) t) ; bound search
                 (1- (point))
               (end-of-line) (point))))))
    (texinfo-discard-command)  ; remove or insert whitespace, as needed
    (delete-region (save-excursion (skip-chars-backward " \t\n") (point))
                   (point))
    (insert (format " (%d) (*Note %s-Footnotes::)"
                    texinfo-footnote-number node-name))
    (fill-paragraph nil)
    (save-excursion
    (if (re-search-forward "^@node" nil 'move)
        (forward-line -1))

    ;; two cases: for the first footnote, we must insert a node header;
    ;; for the second and subsequent footnotes, we need only insert
    ;; the text of the  footnote.

    (if (save-excursion
         (search-backward
          (concat node-name "-Footnotes, Up: ")
          node-name-beginning
          t))
        (progn   ; already at least one footnote
          (setq start (point))
          (insert (format "\n(%d)  %s\n" texinfo-footnote-number arg))
          (fill-region start (point)))
      ;; else not yet a footnote
      (insert "\n\^_\nFile: "  texinfo-format-filename
              "  Node: " node-name "-Footnotes, Up: " node-name "\n")
      (setq start (point))
      (insert (format "\n(%d)  %s\n" texinfo-footnote-number arg))
      (narrow-to-region (save-excursion (goto-char start) (point)) (point))
      (fill-region (point-min) (point-max))
      (widen)))))

(defun texinfo-format-end-node ()
  "Format footnote in the End of node style, with notes at end of node."
  (let (start
        (arg (texinfo-parse-line-arg)))
    (texinfo-discard-command)  ; remove or insert whitespace, as needed
    (delete-region (save-excursion (skip-chars-backward " \t\n") (point))
                   (point))
    (insert (format " (%d) " texinfo-footnote-number))
    (fill-paragraph nil)
    (save-excursion
      (if (search-forward "\n--------- Footnotes ---------\n" nil t)
          (progn ; already have footnote, put new one before end of node
            (if (re-search-forward "^@node" nil 'move)
                (forward-line -1))
            (setq start (point))
            (insert (format "\n(%d)  %s\n" texinfo-footnote-number arg))
            (fill-region start (point)))
        ;; else no prior footnote
        (if (re-search-forward "^@node" nil 'move)
            (forward-line -1))
        (insert "\n--------- Footnotes ---------\n")
        (setq start (point))
        (insert (format "\n(%d)  %s\n" texinfo-footnote-number arg))))))


;;; @itemize, @enumerate, and similar commands

;; @itemize pushes (itemize "COMMANDS" STARTPOS) on texinfo-stack.
;; @enumerate pushes (enumerate 0 STARTPOS).
;; @item dispatches to the texinfo-item prop of the first elt of the list.
;; For itemize, this puts in and rescans the COMMANDS.
;; For enumerate, this increments the number and puts it in.
;; In either case, it puts a Backspace at the front of the line
;; which marks it not to be indented later.
;; All other lines get indented by 5 when the @end is reached.

(defvar texinfo-stack-depth 0
  "Count of number of unpopped texinfo-push-stack calls.
Used by @refill indenting command to avoid indenting within lists, etc.")

(defun texinfo-push-stack (check arg)
  (setq texinfo-stack-depth (1+ texinfo-stack-depth))
  (push (list check arg texinfo-command-start)
        texinfo-stack))

(defun texinfo-pop-stack (check)
  (setq texinfo-stack-depth (1- texinfo-stack-depth))
  (if (null texinfo-stack)
      (error "Unmatched @end %s" check))
  (if (not (eq (car (car texinfo-stack)) check))
      (error "@end %s matches @%s"
             check (car (car texinfo-stack))))
  (prog1 (cdr (car texinfo-stack))
         (setq texinfo-stack (cdr texinfo-stack))))

(put 'itemize 'texinfo-format 'texinfo-itemize)
(defun texinfo-itemize ()
  (texinfo-push-stack
   'itemize
   (progn (skip-chars-forward " \t")
          (if (eolp)
              "@bullet"
            (texinfo-parse-line-arg))))
  (texinfo-discard-line-with-args)
  (setq fill-column (- fill-column 5)))

(put 'itemize 'texinfo-end 'texinfo-end-itemize)
(defun texinfo-end-itemize ()
  (setq fill-column (+ fill-column 5))
  (texinfo-discard-command)
  (let ((stacktop
         (texinfo-pop-stack 'itemize)))
    (texinfo-do-itemize (nth 1 stacktop))))

(put 'enumerate 'texinfo-format 'texinfo-enumerate)
(defun texinfo-enumerate ()
  (texinfo-push-stack
   'enumerate
   (progn (skip-chars-forward " \t")
          (if (eolp)
              1
            (read (current-buffer)))))
  (if (and (symbolp (car (cdr (car texinfo-stack))))
           (> 1 (length (symbol-name (car (cdr (car texinfo-stack)))))))
      (error
       "@enumerate: Use a number or letter, eg: 1, A, a, 3, B, or d." ))
  (texinfo-discard-line-with-args)
  (setq fill-column (- fill-column 5)))

(put 'enumerate 'texinfo-end 'texinfo-end-enumerate)
(defun texinfo-end-enumerate ()
  (setq fill-column (+ fill-column 5))
  (texinfo-discard-command)
  (let ((stacktop
         (texinfo-pop-stack 'enumerate)))
    (texinfo-do-itemize (nth 1 stacktop))))

;; @alphaenumerate never became a standard part of Texinfo
(put 'alphaenumerate 'texinfo-format 'texinfo-alphaenumerate)
(defun texinfo-alphaenumerate ()
  (texinfo-push-stack 'alphaenumerate (1- ?a))
  (setq fill-column (- fill-column 5))
  (texinfo-discard-line))

(put 'alphaenumerate 'texinfo-end 'texinfo-end-alphaenumerate)
(defun texinfo-end-alphaenumerate ()
  (setq fill-column (+ fill-column 5))
  (texinfo-discard-command)
  (let ((stacktop
         (texinfo-pop-stack 'alphaenumerate)))
    (texinfo-do-itemize (nth 1 stacktop))))

;; @capsenumerate never became a standard part of Texinfo
(put 'capsenumerate 'texinfo-format 'texinfo-capsenumerate)
(defun texinfo-capsenumerate ()
  (texinfo-push-stack 'capsenumerate (1- ?A))
  (setq fill-column (- fill-column 5))
  (texinfo-discard-line))

(put 'capsenumerate 'texinfo-end 'texinfo-end-capsenumerate)
(defun texinfo-end-capsenumerate ()
  (setq fill-column (+ fill-column 5))
  (texinfo-discard-command)
  (let ((stacktop
         (texinfo-pop-stack 'capsenumerate)))
    (texinfo-do-itemize (nth 1 stacktop))))

;; At the @end, indent all the lines within the construct
;; except those marked with backspace.  FROM says where
;; construct started.
(defun texinfo-do-itemize (from)
  (save-excursion
   (while (progn (forward-line -1)
                 (>= (point) from))
     (if (= (following-char) ?\b)
         (save-excursion
           (delete-char 1)
           (end-of-line)
           (delete-char 6))
       (if (not (looking-at "[ \t]*$"))
           (save-excursion (insert "     ")))))))

(put 'item 'texinfo-format 'texinfo-item)
(put 'itemx 'texinfo-format 'texinfo-item)
(defun texinfo-item ()
  (funcall (get (car (car texinfo-stack)) 'texinfo-item)))

(put 'itemize 'texinfo-item 'texinfo-itemize-item)
(defun texinfo-itemize-item ()
  ;; (texinfo-discard-line)   ; Did not handle text on same line as @item.
  (delete-region (1+ (point)) (line-beginning-position))
  (if (looking-at "[ \t]*[^ \t\n]+")
      ;; Text on same line as @item command.
      (insert "\b   " (nth 1 (car texinfo-stack)) " \n")
    ;; Else text on next line.
    (insert "\b   " (nth 1 (car texinfo-stack)) " "))
  (forward-line -1))

(put 'enumerate 'texinfo-item 'texinfo-enumerate-item)
(defun texinfo-enumerate-item ()
  (texinfo-discard-line)
  (let (enumerating-symbol)
    (cond ((integerp (car (cdr (car texinfo-stack))))
           (setq enumerating-symbol (car (cdr (car texinfo-stack))))
           (insert ?\b (format "%3d. " enumerating-symbol) ?\n)
           (setcar (cdr (car texinfo-stack)) (1+ enumerating-symbol)))
          ((symbolp (car (cdr (car texinfo-stack))))
           (setq enumerating-symbol
                 (symbol-name (car (cdr (car texinfo-stack)))))
           (if (or (equal ?\[ (string-to-char enumerating-symbol))
                   (equal ?\{ (string-to-char enumerating-symbol)))
               (error
                "Too many items in enumerated list; alphabet ends at Z."))
           (insert ?\b (format "%3s. " enumerating-symbol) ?\n)
           (setcar (cdr (car texinfo-stack))
                   (make-symbol
                    (char-to-string
                     (1+
                      (string-to-char enumerating-symbol))))))
          (t
          (error
           "@enumerate: Use a number or letter, eg: 1, A, a, 3, B or d." )))
    (forward-line -1)))

(put 'alphaenumerate 'texinfo-item 'texinfo-alphaenumerate-item)
(defun texinfo-alphaenumerate-item ()
  (texinfo-discard-line)
  (let ((next (1+ (car (cdr (car texinfo-stack))))))
    (if (> next ?z)
        (error "More than 26 items in @alphaenumerate; get a bigger alphabet"))
    (setcar (cdr (car texinfo-stack)) next)
    (insert "\b  " next ". \n"))
  (forward-line -1))

(put 'capsenumerate 'texinfo-item 'texinfo-capsenumerate-item)
(defun texinfo-capsenumerate-item ()
  (texinfo-discard-line)
  (let ((next (1+ (car (cdr (car texinfo-stack))))))
    (if (> next ?Z)
        (error "More than 26 items in @capsenumerate; get a bigger alphabet"))
    (setcar (cdr (car texinfo-stack)) next)
    (insert "\b  " next ". \n"))
  (forward-line -1))


;;; @table

;; The `@table' command produces two-column tables.

(put 'table 'texinfo-format 'texinfo-table)
(defun texinfo-table ()
  (texinfo-push-stack
   'table
   (progn (skip-chars-forward " \t")
          (if (eolp)
              "@asis"
            (texinfo-parse-line-arg))))
  (texinfo-discard-line-with-args)
  (setq fill-column (- fill-column 5)))

(put 'table 'texinfo-item 'texinfo-table-item)
(defun texinfo-table-item ()
  (let ((arg (texinfo-parse-arg-discard))
        (itemfont (car (cdr (car texinfo-stack)))))
    (insert ?\b itemfont ?\{ arg "}\n     \n"))
  (forward-line -2))

(put 'table 'texinfo-end 'texinfo-end-table)
(defun texinfo-end-table ()
  (setq fill-column (+ fill-column 5))
  (texinfo-discard-command)
  (let ((stacktop
         (texinfo-pop-stack 'table)))
    (texinfo-do-itemize (nth 1 stacktop))))

;; @description appears to be an undocumented variant on @table that
;; does not require an arg.  It fails in texinfo.tex 2.58 and is not
;; part of makeinfo.c   The command appears to be a relic of the past.
(put 'description 'texinfo-end 'texinfo-end-table)
(put 'description 'texinfo-format 'texinfo-description)
(defun texinfo-description ()
  (texinfo-push-stack 'table "@asis")
  (setq fill-column (- fill-column 5))
  (texinfo-discard-line))


;;; @ftable, @vtable

;; The `@ftable' and `@vtable' commands are like the `@table' command
;; but they also insert each entry in the first column of the table
;; into the function or variable index.

;; Handle the @ftable and @vtable commands:

(put 'ftable 'texinfo-format 'texinfo-ftable)
(put 'vtable 'texinfo-format 'texinfo-vtable)

(defun texinfo-ftable () (texinfo-indextable 'ftable))
(defun texinfo-vtable () (texinfo-indextable 'vtable))

(defun texinfo-indextable (table-type)
  (texinfo-push-stack table-type (texinfo-parse-arg-discard))
  (setq fill-column (- fill-column 5)))

;; Handle the @item commands within ftable and vtable:

(put 'ftable 'texinfo-item 'texinfo-ftable-item)
(put 'vtable 'texinfo-item 'texinfo-vtable-item)

(defun texinfo-ftable-item () (texinfo-indextable-item 'texinfo-findex))
(defun texinfo-vtable-item () (texinfo-indextable-item 'texinfo-vindex))

(defun texinfo-indextable-item (index-type)
  (let ((item (texinfo-parse-arg-discard))
        (itemfont (car (cdr (car texinfo-stack))))
        (indexvar index-type))
    (insert ?\b itemfont ?\{ item "}\n     \n")
    (set indexvar
         (cons
          (list item texinfo-last-node)
          (symbol-value indexvar)))
    (forward-line -2)))

;; Handle @end ftable, @end vtable

(put 'ftable 'texinfo-end 'texinfo-end-ftable)
(put 'vtable 'texinfo-end 'texinfo-end-vtable)

(defun texinfo-end-ftable () (texinfo-end-indextable 'ftable))
(defun texinfo-end-vtable () (texinfo-end-indextable 'vtable))

(defun texinfo-end-indextable (table-type)
  (setq fill-column (+ fill-column 5))
  (texinfo-discard-command)
  (let ((stacktop
         (texinfo-pop-stack table-type)))
    (texinfo-do-itemize (nth 1 stacktop))))


;;; @multitable ... @end multitable

;; Produce a multi-column table, with as many columns as desired.
;;
;; A multi-column table has this template:
;;
;;     @multitable {A1} {A2} {A3}
;;     @item  A1  @tab  A2  @tab  A3
;;     @item  B1  @tab  B2  @tab  B3
;;     @item  C1  @tab  C2  @tab  C3
;;     @end multitable
;;
;; where the width of the text in brackets specifies the width of the
;; respective column.
;;
;; Or else:
;;
;;     @multitable @columnfractions .25 .3 .45
;;     @item  A1  @tab  A2  @tab  A3
;;     @item  B1  @tab  B2  @tab  B3
;;     @end multitable
;;
;; where the fractions specify the width of each column as a percent
;; of the current width of the text (i.e., of the fill-column).
;;
;; Long lines of text are filled within columns.
;;
;; Using the Emacs Lisp formatter, texinfmt.el,
;; the whitespace between columns can be increased by setting
;; `texinfo-extra-inter-column-width' to a value greater than 0.  By default,
;; there is at least one blank space between columns.
;;
;; The Emacs Lisp formatter, texinfmt.el, ignores the following four
;; commands that are defined in texinfo.tex for printed output.
;;
;;     @multitableparskip,
;;     @multitableparindent,
;;     @multitablecolmargin,
;;     @multitablelinespace.

;; How @multitable works.
;; =====================
;;
;; `texinfo-multitable' reads the @multitable line and determines from it
;; how wide each column should be.
;;
;; Also, it pushes this information, along with an identifying symbol,
;; onto the `texinfo-stack'.  At the @end multitable command, the stack
;; is checked for its matching @multitable command, and then popped, or
;; else an error is signaled.  Also, this command pushes the location of
;; the start of the table onto the stack.
;;
;; `texinfo-end-multitable' checks the `texinfo-stack' that the @end
;; multitable truly is ending a corresponding beginning, and if it is,
;; pops the stack.
;;
;; `texinfo-multitable-widths' is called by `texinfo-multitable'.
;; The function returns a list of the widths of each column in a
;; multi-column table, based on the information supplied by the arguments
;; to the @multitable command (by arguments, I mean the text on the rest
;; of the @multitable line, not the remainder of the multi-column table
;; environment).
;;
;; `texinfo-multitable-item' formats a row within a multicolumn table.
;; This command is executed when texinfmt sees @item inside @multitable.
;; Cells in row are separated by `@tab's.  Widths of cells are specified
;; by the arguments in the @multitable line.  Cells are filled.  All cells
;; are made to be the same height by padding their bottoms, as needed,
;; with blanks.
;;
;; `texinfo-multitable-extract-row' is called by `texinfo-multitable-item'.
;; This function returns the text in a multitable row, as a string.
;; The start of a row is marked by an @item and the end of row is the
;; beginning of next @item or beginning of the @end multitable line.
;; Cells within a row are separated by @tab.
;;
;; Note that @tab, the cell separators, are not treated as independent
;; Texinfo commands.

(defvar texinfo-extra-inter-column-width 0
  "*Number of extra spaces between entries (columns) in @multitable.")

(defvar texinfo-multitable-buffer-name "*multitable-temporary-buffer*")
(defvar texinfo-multitable-rectangle-name "texinfo-multitable-temp-")

;; These commands are defined in texinfo.tex for printed output.
(put 'multitableparskip 'texinfo-format 'texinfo-discard-line-with-args)
(put 'multitableparindent 'texinfo-format 'texinfo-discard-line-with-args)
(put 'multitablecolmargin 'texinfo-format 'texinfo-discard-line-with-args)
(put 'multitablelinespace 'texinfo-format 'texinfo-discard-line-with-args)

(put 'multitable 'texinfo-format 'texinfo-multitable)

(defun texinfo-multitable ()
  "Produce multi-column tables.

A multi-column table has this template:

    @multitable {A1} {A2} {A3}
    @item  A1  @tab  A2  @tab  A3
    @item  B1  @tab  B2  @tab  B3
    @item  C1  @tab  C2  @tab  C3
    @end multitable

where the width of the text in brackets specifies the width of the
respective column.

Or else:

    @multitable @columnfractions .25 .3 .45
    @item  A1  @tab  A2  @tab  A3
    @item  B1  @tab  B2  @tab  B3
    @end multitable

where the fractions specify the width of each column as a percent
of the current width of the text (i.e., of the `fill-column').

Long lines of text are filled within columns.

Using the Emacs Lisp formatter, texinfmt.el,
the whitespace between columns can be increased by setting
`texinfo-extra-inter-column-width' to a value greater than 0.  By default,
there is at least one blank space between columns.

The Emacs Lisp formatter, texinfmt.el, ignores the following four
commands that are defined in texinfo.tex for printed output.

    @multitableparskip,
    @multitableparindent,
    @multitablecolmargin,
    @multitablelinespace."

;; This function pushes information onto the `texinfo-stack'.
;; A stack element consists of:
;;   - type-of-command, i.e., multitable
;;   - the information about column widths, and
;;   - the position of texinfo-command-start.
;; e.g., ('multitable (1 2 3 4) 123)
;; The command line is then deleted.
  (texinfo-push-stack
   'multitable
   ;; push width information on stack
   (texinfo-multitable-widths))
  (texinfo-discard-line-with-args))

(put 'multitable 'texinfo-end 'texinfo-end-multitable)
(defun texinfo-end-multitable ()
  "Discard the @end multitable line and pop the stack of multitable."
  (texinfo-discard-command)
  (texinfo-pop-stack 'multitable))

(defun texinfo-multitable-widths ()
  "Return list of widths of each column in a multi-column table."
  (let (texinfo-multitable-width-list)
    ;; Fractions format:
    ;;  @multitable @columnfractions .25 .3 .45
    ;;
    ;; Template format:
    ;;  @multitable {Column 1 template} {Column 2} {Column 3 example}
    ;; Place point before first argument
    (skip-chars-forward " \t")
    (cond
     ;; Check for common misspelling
     ((looking-at "@columnfraction ")
      (error "In @multitable, @columnfractions misspelled"))
     ;; Case 1: @columnfractions .25 .3 .45
     ((looking-at "@columnfractions")
      (forward-word 1)
      (while (not (eolp))
        (push (truncate
               (1-
                (* fill-column (read (get-buffer (current-buffer))))))
              texinfo-multitable-width-list)))
     ;;
     ;; Case 2: {Column 1 template} {Column 2} {Column 3 example}
     ((looking-at "{")
      (let ((start-of-templates (point)))
        (while (not (eolp))
          (skip-chars-forward " \t")
          (let* ((start-of-template (1+ (point)))
                 (end-of-template
                 ;; forward-sexp works with braces in Texinfo mode
                  (progn (forward-sexp 1) (1- (point)))))
            (push (- end-of-template start-of-template)
                  texinfo-multitable-width-list)
            ;; Remove carriage return from within a template, if any.
            ;; This helps those those who want to use more than
            ;; one line's worth of words in @multitable line.
            (narrow-to-region start-of-template end-of-template)
            (goto-char (point-min))
            (while (search-forward "
" nil t)
              (delete-char -1))
            (goto-char (point-max))
            (widen)
            (forward-char 1)))))
     ;;
     ;; Case 3: Trouble
     (t
      (error
       "You probably need to specify column widths for @multitable correctly.")))
    ;; Check whether columns fit on page.
    (let ((desired-columns
           (+
            ;; between column spaces
            (length texinfo-multitable-width-list)
            ;; additional between column spaces, if any
            texinfo-extra-inter-column-width
            ;; sum of spaces for each entry
            (apply '+ texinfo-multitable-width-list))))
      (if (> desired-columns fill-column)
          (error
           "Multi-column table width, %d chars, is greater than page width, %d chars."
            desired-columns fill-column)))
    texinfo-multitable-width-list))

;; @item  A1  @tab  A2  @tab  A3
(defun texinfo-multitable-extract-row ()
  "Return multitable row, as a string.
End of row is beginning of next @item or beginning of @end.
Cells within rows are separated by @tab."
  (skip-chars-forward " \t")
  (let* ((start (point))
         (end (progn
                (re-search-forward "@item\\|@end")
                (match-beginning 0)))
         (row (progn (goto-char end)
                     (skip-chars-backward " ")
                     ;; remove whitespace at end of argument
                     (delete-region (point) end)
                     (buffer-substring-no-properties start (point)))))
    (delete-region texinfo-command-start end)
    row))

(put 'multitable 'texinfo-item 'texinfo-multitable-item)
(defun texinfo-multitable-item ()
  "Format a row within a multicolumn table.
Cells in row are separated by @tab.
Widths of cells are specified by the arguments in the @multitable line.
All cells are made to be the same height.
This command is executed when texinfmt sees @item inside @multitable."
  (let ((original-buffer (current-buffer))
        (table-widths (reverse (car (cdr (car texinfo-stack)))))
        (existing-fill-column fill-column)
        start
        end
        (table-column       0)
        (table-entry-height 0)
        ;; unformatted row looks like:  A1  @tab  A2  @tab  A3
        ;; extract-row command deletes the source line in the table.
        (unformatted-row (texinfo-multitable-extract-row)))
    ;; Use a temporary buffer
    (set-buffer (get-buffer-create texinfo-multitable-buffer-name))
    (delete-region (point-min) (point-max))
    (insert unformatted-row)
    (goto-char (point-min))
;; 1. Check for correct number of @tab in line.
    (let ((tab-number 1))               ; one @tab between two columns
      (while (search-forward "@tab" nil t)
        (setq tab-number (1+ tab-number)))
      (let ((needed-tabs (- (length table-widths) tab-number)))
        (when (> needed-tabs 0)
              (goto-char (point-min))
              (end-of-line)
              (while (> needed-tabs 0)
                (insert "@w{ }\n@tab")
                (setq needed-tabs (1- needed-tabs))
                (message
                 "Added @tabs and empty spaces to a @multitable row")))))
    (goto-char (point-min))
;; 2. Format each cell, and copy to a rectangle
    ;; buffer looks like this:    A1  @tab  A2  @tab  A3
    ;; Cell #1: format up to @tab
    ;; Cell #2: format up to @tab
    ;; Cell #3: format up to eob
    (while (not (eobp))
      (setq start (point))
      (setq end (save-excursion
                  (if (search-forward "@tab" nil 'move)
                      ;; Delete the @tab command, including the @-sign
                      (delete-region
                       (point)
                       (progn (forward-word -1) (1- (point)))))
                  (point)))
      ;; Set fill-column *wider* than needed to produce inter-column space
      (setq fill-column (+ 1
                           texinfo-extra-inter-column-width
                           (nth table-column table-widths)))
      (narrow-to-region start end)
      ;; Remove whitespace before and after entry.
      (skip-chars-forward " ")
      (delete-region (point) (line-beginning-position))
      (goto-char (point-max))
      (skip-chars-backward " ")
      (delete-region (point) (line-end-position))
      ;; Temporarily set texinfo-stack to nil so texinfo-format-scan
      ;; does not see an unterminated @multitable.
      (let (texinfo-stack)                      ; nil
        (texinfo-format-scan))
      (let (fill-prefix)                        ; no fill prefix
        (fill-region (point-min) (point-max)))
      (setq table-entry-height
            (max table-entry-height (count-lines (point-min) (point-max))))
;; 3. Move point to end of bottom line, and pad that line to fill column.
      (goto-char (point-min))
      (forward-line (1- table-entry-height))
      (let* ((beg (point))                      ; beginning of line
             ;; add one more space for inter-column spacing
             (needed-whitespace
              (1+
               (- fill-column
                  (-
                   (progn (end-of-line) (point)) ; end of existing line
                   beg)))))
        (insert (make-string
                 (if (> needed-whitespace 0) needed-whitespace 1)
                 ? )))
      ;; now, put formatted cell into a rectangle
      (set (intern (concat texinfo-multitable-rectangle-name
                           (int-to-string table-column)))
           (extract-rectangle (point-min) (point)))
      (delete-region (point-min) (point))
      (goto-char (point-max))
      (setq table-column (1+ table-column))
      (widen))
;; 4. Add extra lines to rectangles so all are of same height
    (let ((total-number-of-columns table-column)
          (column-number 0)
          here)
      (while (> table-column 0)
        (let ((this-rectangle (int-to-string table-column)))
          (while (< (length this-rectangle) table-entry-height)
            (setq this-rectangle (append this-rectangle '("")))))
        (setq table-column (1- table-column)))
;; 5. Insert formatted rectangles in original buffer
      (switch-to-buffer original-buffer)
      (open-line table-entry-height)
      (while (< column-number total-number-of-columns)
        (setq here (point))
        (insert-rectangle
         (eval (intern
                (concat texinfo-multitable-rectangle-name
                        (int-to-string column-number)))))
        (goto-char here)
        (end-of-line)
        (setq column-number (1+ column-number))))
    (kill-buffer texinfo-multitable-buffer-name)
    (setq fill-column existing-fill-column)))


;;; @image
;;  Use only the FILENAME argument to the command.
;;  In Info, ignore the other arguments.

(put 'image 'texinfo-format 'texinfo-format-image)
(defun texinfo-format-image ()
  "Insert an image from a file ending in .txt.
Use only the FILENAME arg; for Info, ignore the other arguments to @image."
  (let ((args (texinfo-format-parse-args))
	filename)
    (when (null (nth 0 args))
      (error "Invalid image command"))
    (texinfo-discard-command)
    ;; makeinfo uses FILENAME.txt
    (setq filename (format "%s.txt" (nth 0 args)))
    (message "Reading included file: %s" filename)
    ;; verbatim for Info output
    (goto-char (+ (point) (cadr (insert-file-contents filename))))
    (message "Reading included file: %s...done" filename)))


;;; @ifinfo,  @iftex, @tex, @ifhtml, @html, @ifplaintext, @ifxml, @xml
;;  @ifnottex, @ifnotinfo, @ifnothtml, @ifnotplaintext, @ifnotxml

(put 'ifinfo 'texinfo-format 'texinfo-discard-line)
(put 'ifinfo 'texinfo-end 'texinfo-discard-command)

(put 'iftex 'texinfo-format 'texinfo-format-iftex)
(defun texinfo-format-iftex ()
  (delete-region texinfo-command-start
                 (re-search-forward "@end iftex[ \t]*\n")))

(put 'ifhtml 'texinfo-format 'texinfo-format-ifhtml)
(defun texinfo-format-ifhtml ()
  (delete-region texinfo-command-start
                 (re-search-forward "@end ifhtml[ \t]*\n")))

(put 'ifplaintext 'texinfo-format 'texinfo-format-ifplaintext)
(defun texinfo-format-ifplaintext ()
  (delete-region texinfo-command-start
                 (re-search-forward "@end ifplaintext[ \t]*\n")))

(put 'ifxml 'texinfo-format 'texinfo-format-ifxml)
(defun texinfo-format-ifxml ()
  (delete-region texinfo-command-start
                 (progn (re-search-forward "^@end ifxml[ \t]*\n")
                        (point))))

(put 'tex 'texinfo-format 'texinfo-format-tex)
(defun texinfo-format-tex ()
  (delete-region texinfo-command-start
                 (re-search-forward "@end tex[ \t]*\n")))

(put 'html 'texinfo-format 'texinfo-format-html)
(defun texinfo-format-html ()
  (delete-region texinfo-command-start
                 (re-search-forward "@end html[ \t]*\n")))

(put 'xml 'texinfo-format 'texinfo-format-xml)
(defun texinfo-format-xml ()
  (delete-region texinfo-command-start
                 (progn (re-search-forward "^@end xml[ \t]*\n")
                        (point))))

(put 'ifnotinfo 'texinfo-format 'texinfo-format-ifnotinfo)
(defun texinfo-format-ifnotinfo ()
  (delete-region texinfo-command-start
                 (re-search-forward "@end ifnotinfo[ \t]*\n")))

(put 'ifnotplaintext 'texinfo-format 'texinfo-discard-line)
(put 'ifnotplaintext 'texinfo-end 'texinfo-discard-command)

(put 'ifnottex 'texinfo-format 'texinfo-discard-line)
(put 'ifnottex 'texinfo-end 'texinfo-discard-command)

(put 'ifnothtml 'texinfo-format 'texinfo-discard-line)
(put 'ifnothtml 'texinfo-end 'texinfo-discard-command)

(put 'ifnotxml 'texinfo-format 'texinfo-discard-line)
(put 'ifnotxml 'texinfo-end 'texinfo-discard-command)


;;; @titlepage

(put 'titlepage 'texinfo-format 'texinfo-format-titlepage)
(defun texinfo-format-titlepage ()
  (delete-region texinfo-command-start
                 (re-search-forward "@end titlepage[ \t]*\n")))

(put 'endtitlepage 'texinfo-format 'texinfo-discard-line)

;; @titlespec         an alternative titling command; ignored by Info

(put 'titlespec 'texinfo-format 'texinfo-format-titlespec)
(defun texinfo-format-titlespec ()
  (delete-region texinfo-command-start
                 (re-search-forward "@end titlespec[ \t]*\n")))

(put 'endtitlespec 'texinfo-format 'texinfo-discard-line)


;;; @today

(put 'today 'texinfo-format 'texinfo-format-today)

;; Produces Day Month Year style of output.  eg `1 Jan 1900'
;; The `@today{}' command requires a pair of braces, like `@dots{}'.
(defun texinfo-format-today ()
  (texinfo-parse-arg-discard)
  (insert (format-time-string "%e %b %Y")))


;;; @timestamp{}
;; Produce `Day Month Year Hour:Min' style of output.
;; eg `1 Jan 1900 13:52'

(put 'timestamp 'texinfo-format 'texinfo-format-timestamp)

;; The `@timestamp{}' command requires a pair of braces, like `@dots{}'.
(defun texinfo-format-timestamp ()
  "Insert the current local time and date."
  (texinfo-parse-arg-discard)
  ;; For seconds and time zone, replace format string with  "%e %b %Y %T %Z"
  (insert (format-time-string "%e %b %Y %R")))


;;; @ignore

(put 'ignore 'texinfo-format 'texinfo-format-ignore)
(defun texinfo-format-ignore ()
  (delete-region texinfo-command-start
                 (re-search-forward "@end ignore[ \t]*\n")))

(put 'endignore 'texinfo-format 'texinfo-discard-line)


;;; Define the Info enclosure command: @definfoenclose

;; A `@definfoenclose' command may be used to define a highlighting
;; command for Info, but not for TeX.  A command defined using
;; `@definfoenclose' marks text by enclosing it in strings that precede
;; and follow the text.
;;
;; Presumably, if you define a command with `@definfoenclose` for Info,
;; you will also define the same command in the TeX definitions file,
;; `texinfo.tex' in a manner appropriate for typesetting.
;;
;; Write a `@definfoenclose' command on a line and follow it with three
;; arguments separated by commas (commas are used as separators in an
;; `@node' line in the same way).  The first argument to
;; `@definfoenclose' is the @-command name \(without the `@'\); the
;; second argument is the Info start delimiter string; and the third
;; argument is the Info end delimiter string.  The latter two arguments
;; enclose the highlighted text in the Info file.  A delimiter string
;; may contain spaces.  Neither the start nor end delimiter is
;; required.  However, if you do not provide a start delimiter, you
;; must follow the command name with two commas in a row; otherwise,
;; the Info formatting commands will misinterpret the end delimiter
;; string as a start delimiter string.
;;
;; If you do a @definfoenclose{} on the name of a pre-defined macro (such
;; as @emph{}, @strong{}, @tt{}, or @i{}) the enclosure definition will
;; override the built-in definition.
;;
;; An enclosure command defined this way takes one argument in braces.
;;
;; For example, you can write:
;;
;;     @ifinfo
;;     @definfoenclose phoo, //, \\
;;     @end ifinfo
;;
;; near the beginning of a Texinfo file at the beginning of the lines
;; to define `@phoo' as an Info formatting command that inserts `//'
;; before and `\\' after the argument to `@phoo'.  You can then write
;; `@phoo{bar}' wherever you want `//bar\\' highlighted in Info.
;;
;; Also, for TeX formatting, you could write
;;
;;     @iftex
;;     @global@let@phoo=@i
;;     @end iftex
;;
;; to define `@phoo' as a command that causes TeX to typeset
;; the argument to `@phoo' in italics.
;;
;; Note that each definition applies to its own formatter: one for TeX,
;; the other for texinfo-format-buffer or texinfo-format-region.
;;
;; Here is another example: write
;;
;;     @definfoenclose headword, , :
;;
;; near the beginning of the file, to define `@headword' as an Info
;; formatting command that inserts nothing before and a colon after the
;; argument to `@headword'.

(put 'definfoenclose 'texinfo-format 'texinfo-define-info-enclosure)
(defun texinfo-define-info-enclosure ()
  (let* ((args (texinfo-format-parse-line-args))
         (command-name (nth 0 args))
         (beginning-delimiter (or (nth 1 args) ""))
         (end-delimiter (or (nth 2 args) "")))
    (texinfo-discard-command)
    (push (list command-name
                (list
                 beginning-delimiter
                 end-delimiter))
          texinfo-enclosure-list)))


;;; @alias

(put 'alias 'texinfo-format 'texinfo-alias)
(defun texinfo-alias ()
  (let ((start (1- (point)))
        args)
    (skip-chars-forward " ")
    (setq texinfo-command-end (line-end-position))
    (if (not (looking-at "\\([^=]+\\)=\\(.*\\)"))
	(error "Invalid alias command")
      (push (cons
             (match-string-no-properties 1)
             (match-string-no-properties 2))
            texinfo-alias-list)
      (texinfo-discard-command))))


;;; @var, @code and the like

(put 'var 'texinfo-format 'texinfo-format-var)
;;  @sc  a small caps font for TeX; formatted as `var' in Info
(put 'sc 'texinfo-format 'texinfo-format-var)
;;  @acronym   for abbreviations in all caps, such as `NASA'.
;;  Convert all letters to uppercase if they are not already.
(put 'acronym 'texinfo-format 'texinfo-format-var)
(defun texinfo-format-var ()
  (let ((arg (texinfo-parse-expanded-arg)))
    (texinfo-discard-command)
    (insert (upcase arg))))

(put 'cite 'texinfo-format 'texinfo-format-code)
(put 'code 'texinfo-format 'texinfo-format-code)
;; @command (for command names)
(put 'command 'texinfo-format 'texinfo-format-code)
;; @env (for environment variables)
(put 'env 'texinfo-format 'texinfo-format-code)
(put 'file 'texinfo-format 'texinfo-format-code)
(put 'samp 'texinfo-format 'texinfo-format-code)
(put 'url 'texinfo-format 'texinfo-format-code)
(defun texinfo-format-code ()
  (insert "`" (texinfo-parse-arg-discard) "'")
  (goto-char texinfo-command-start))

;; @option (for command-line options) must be different from @code
;; because of its special formatting in @table; namely that it does
;; not lead to inserted ` ... ' in a table, but does elsewhere.
(put 'option 'texinfo-format 'texinfo-format-option)
(defun texinfo-format-option ()
  "Insert ` ... ' around arg unless inside a table; in that case, no quotes."
  ;; `looking-at-backward' not available in v. 18.57, 20.2
  (if (not (search-backward ""    ; searched-for character is a control-H
                    (line-beginning-position)
                    t))
      (insert "`" (texinfo-parse-arg-discard) "'")
      (insert  (texinfo-parse-arg-discard)))
  (goto-char texinfo-command-start))

(put 'emph 'texinfo-format 'texinfo-format-emph)
(put 'strong 'texinfo-format 'texinfo-format-emph)
(defun texinfo-format-emph ()
  (insert "*" (texinfo-parse-arg-discard) "*")
  (goto-char texinfo-command-start))

(put 'dfn 'texinfo-format 'texinfo-format-defn)
(put 'defn 'texinfo-format 'texinfo-format-defn)
(defun texinfo-format-defn ()
  (insert "\"" (texinfo-parse-arg-discard) "\"")
  (goto-char texinfo-command-start))

(put 'email 'texinfo-format 'texinfo-format-email)
(defun texinfo-format-email ()
  "Format email address and optional following full name.
Insert full name, if present, followed by email address
surrounded by in angle brackets."
  (let ((args (texinfo-format-parse-args)))
    (texinfo-discard-command)
    ;; if full-name
    (if (nth 1 args)
        (insert  (nth 1 args) " "))
    (insert "<" (nth 0 args) ">")))

(put 'key 'texinfo-format 'texinfo-format-key)
;; I've decided not want to have angle brackets around these -- rms.
(defun texinfo-format-key ()
  (insert (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @verb{<char>TEXT<char>} (in `makeinfo' 4.1 and later)
(put 'verb 'texinfo-format 'texinfo-format-verb)
(defun texinfo-format-verb ()
  "Format text between non-quoted unique delimiter characters verbatim.
Enclose the verbatim text, including the delimiters, in braces.  Print
text exactly as written (but not the delimiters) in a fixed-width.

For example, @verb\{|@|\} results in @ and
@verb\{+@'e?`!`+} results in @'e?`!`."

  (let ((delimiter (buffer-substring-no-properties
		    (1+ texinfo-command-end) (+ 2 texinfo-command-end))))
    (unless (looking-at "{")
      (error "Not found: @verb start brace"))
    (delete-region texinfo-command-start (+ 2 texinfo-command-end))
    (search-forward  delimiter))
  (delete-char -1)
  (unless (looking-at "}")
    (error "Not found: @verb end brace"))
  (delete-char 1))

;; as of 2002 Dec 10
;; see (texinfo)Block Enclosing Commands
;; need: @verbatim

;; as of 2002 Dec 10
;; see (texinfo)verbatiminclude
;; need: @verbatiminclude FILENAME

(put 'bullet 'texinfo-format 'texinfo-format-bullet)
(defun texinfo-format-bullet ()
  "Insert an asterisk.
If used within a line, follow `@bullet' with braces."
  (texinfo-optional-braces-discard)
  (insert "*"))


;;; @kbd

;; Inside of @example ... @end example and similar environments,
;; @kbd does nothing; but outside of such environments, it places
;; single quotation marks around its argument.

(defvar texinfo-format-kbd-regexp
  (concat
   "^@"
   "\\("
   "display\\|"
   "example\\|"
   "smallexample\\|"
   "lisp\\|"
   "smalllisp"
   "\\)")
  "Regexp matching environments in which @kbd does not put `...' around arg.")

(defvar texinfo-format-kbd-end-regexp
  (concat
   "^@end "
   "\\("
   "display\\|"
   "example\\|"
   "smallexample\\|"
   "lisp\\|"
   "smalllisp"
   "\\)")
  "Regexp specifying end of environments in which @kbd does not put `...'
around argument. (See `texinfo-format-kbd-regexp')")

(put 'kbd 'texinfo-format 'texinfo-format-kbd)
(defun texinfo-format-kbd ()
  "Place single quote marks around arg, except in @example and similar."
  ;; Search forward for @end example closer than an @example.
  ;; Can stop search at nearest @node or texinfo-section-types-regexp
  (let* ((stop
          (save-excursion
            (re-search-forward
             (concat "^@node\\|\\(" texinfo-section-types-regexp "\\)")
             nil
             'move-to-end)    ; if necessary, return point at end of buffer
            (point)))
         (example-location
          (save-excursion
            (re-search-forward texinfo-format-kbd-regexp stop 'move-to-end)
            (point)))
         (end-example-location
          (save-excursion
            (re-search-forward texinfo-format-kbd-end-regexp stop 'move-to-end)
            (point))))
    ;; If inside @example, @end example will be closer than @example
    ;; or end of search i.e., end-example-location less than example-location
    (if (>= end-example-location example-location)
        ;; outside an @example or equivalent
        (insert "`" (texinfo-parse-arg-discard) "'")
    ;; else, in @example; do not surround with `...'
      (insert (texinfo-parse-arg-discard)))
    (goto-char texinfo-command-start)))


;;; @example, @lisp, @quotation, @display, @smalllisp, @smallexample,
;;  @smalldisplay

(put 'display 'texinfo-format 'texinfo-format-example)
(put 'smalldisplay 'texinfo-format 'texinfo-format-example)
(put 'example 'texinfo-format 'texinfo-format-example)
(put 'lisp 'texinfo-format 'texinfo-format-example)
(put 'quotation 'texinfo-format 'texinfo-format-example)
(put 'smallexample 'texinfo-format 'texinfo-format-example)
(put 'smalllisp 'texinfo-format 'texinfo-format-example)
(defun texinfo-format-example ()
  (texinfo-push-stack 'example nil)
  (setq fill-column (- fill-column 5))
  (texinfo-discard-line))

(put 'example 'texinfo-end 'texinfo-end-example)
(put 'display 'texinfo-end 'texinfo-end-example)
(put 'smalldisplay 'texinfo-end 'texinfo-end-example)
(put 'lisp 'texinfo-end 'texinfo-end-example)
(put 'quotation 'texinfo-end 'texinfo-end-example)
(put 'smallexample 'texinfo-end 'texinfo-end-example)
(put 'smalllisp 'texinfo-end 'texinfo-end-example)
(defun texinfo-end-example ()
  (setq fill-column (+ fill-column 5))
  (texinfo-discard-command)
  (let ((stacktop
         (texinfo-pop-stack 'example)))
    (texinfo-do-itemize (nth 1 stacktop))))

(put 'exdent 'texinfo-format 'texinfo-format-exdent)
(defun texinfo-format-exdent ()
  (texinfo-discard-command)
  (delete-region (point)
                 (progn
                  (skip-chars-forward " ")
                  (point)))
  (insert ?\b)
  ;; Cancel out the deletion that texinfo-do-itemize
  ;; is going to do at the end of this line.
  (save-excursion
    (end-of-line)
    (insert "\n     ")))


;; @direntry and @dircategory

(put 'direntry 'texinfo-format 'texinfo-format-direntry)
(defun texinfo-format-direntry ()
  (texinfo-push-stack 'direntry nil)
  (texinfo-discard-line)
  (insert "START-INFO-DIR-ENTRY\n"))

(put 'direntry 'texinfo-end 'texinfo-end-direntry)
(defun texinfo-end-direntry ()
  (texinfo-discard-command)
  (insert "END-INFO-DIR-ENTRY\n\n")
  (texinfo-pop-stack 'direntry))

(put 'dircategory 'texinfo-format 'texinfo-format-dircategory)
(defun texinfo-format-dircategory ()
  (let ((str (texinfo-parse-arg-discard)))
    (delete-region (point)
                   (progn
                     (skip-chars-forward " ")
                     (point)))
    (insert "INFO-DIR-SECTION " str "\n")))

;;; @cartouche

;; The @cartouche command is a noop in Info; in a printed manual,
;; it makes a box with rounded corners.

(put 'cartouche 'texinfo-format 'texinfo-discard-line)
(put 'cartouche 'texinfo-end 'texinfo-discard-command)


;;; @flushleft and @format

;; The @flushleft command left justifies every line but leaves the
;; right end ragged.  As far as Info is concerned, @flushleft is a
;; `do-nothing' command

;; The @format command is similar to @example except that it does not
;; indent; this means that in Info, @format is similar to @flushleft.

(put 'format 'texinfo-format 'texinfo-format-flushleft)
(put 'smallformat 'texinfo-format 'texinfo-format-flushleft)
(put 'flushleft 'texinfo-format 'texinfo-format-flushleft)
(defun texinfo-format-flushleft ()
  (texinfo-discard-line))

(put 'format 'texinfo-end 'texinfo-end-flushleft)
(put 'smallformat 'texinfo-end 'texinfo-end-flushleft)
(put 'flushleft 'texinfo-end 'texinfo-end-flushleft)
(defun texinfo-end-flushleft ()
  (texinfo-discard-command))


;;; @flushright

;; The @flushright command right justifies every line but leaves the
;; left end ragged.  Spaces and tabs at the right ends of lines are
;; removed so that visible text lines up on the right side.

(put 'flushright 'texinfo-format 'texinfo-format-flushright)
(defun texinfo-format-flushright ()
  (texinfo-push-stack 'flushright nil)
  (texinfo-discard-line))

(put 'flushright 'texinfo-end 'texinfo-end-flushright)
(defun texinfo-end-flushright ()
  (texinfo-discard-command)

  (let ((stacktop
         (texinfo-pop-stack 'flushright)))

    (texinfo-do-flushright (nth 1 stacktop))))

(defun texinfo-do-flushright (from)
  (save-excursion
   (while (progn (forward-line -1)
                 (>= (point) from))

     (beginning-of-line)
     (insert
      (make-string
       (- fill-column
          (save-excursion
            (end-of-line)
            (skip-chars-backward " \t")
            (delete-region (point) (progn (end-of-line) (point)))
            (current-column)))
       ? )))))


;;; @ctrl, @TeX, @copyright, @minus, @dots, @enddots, @pounds

(put 'ctrl 'texinfo-format 'texinfo-format-ctrl)
(defun texinfo-format-ctrl ()
  (let ((str (texinfo-parse-arg-discard)))
    (insert (logand 31 (aref str 0)))))

(put 'TeX 'texinfo-format 'texinfo-format-TeX)
(defun texinfo-format-TeX ()
  (texinfo-parse-arg-discard)
  (insert "TeX"))

(put 'copyright 'texinfo-format 'texinfo-format-copyright)
(defun texinfo-format-copyright ()
  (texinfo-parse-arg-discard)
  (insert "(C)"))

(put 'minus 'texinfo-format 'texinfo-format-minus)
(defun texinfo-format-minus ()
  "Insert a minus sign.
If used within a line, follow `@minus' with braces."
  (texinfo-optional-braces-discard)
  (insert "-"))

(put 'dots 'texinfo-format 'texinfo-format-dots)
(defun texinfo-format-dots ()
  (texinfo-parse-arg-discard)
  (insert "..."))

(put 'enddots 'texinfo-format 'texinfo-format-enddots)
(defun texinfo-format-enddots ()
  (texinfo-parse-arg-discard)
  (insert "...."))

(put 'pounds 'texinfo-format 'texinfo-format-pounds)
(defun texinfo-format-pounds ()
  (texinfo-parse-arg-discard)
  (insert "#"))


;;; Refilling and indenting:  @refill, @paragraphindent, @noindent

;; Indent only those paragraphs that are refilled as a result of an
;; @refill command.

;;    * If the value is `asis', do not change the existing indentation at
;;      the starts of paragraphs.

;;    * If the value zero, delete any existing indentation.

;;    * If the value is greater than zero, indent each paragraph by that
;;      number of spaces.

;; But do not refill paragraphs with an @refill command that are
;; preceded by @noindent or are part of a table, list, or deffn.

(defvar texinfo-paragraph-indent "asis"
  "Number of spaces for @refill to indent a paragraph; else to leave as is.")

(put 'paragraphindent 'texinfo-format 'texinfo-paragraphindent)

(defun texinfo-paragraphindent ()
  "Specify the number of spaces for @refill to indent a paragraph.
Default is to leave the number of spaces as is."
  (let ((arg  (texinfo-parse-arg-discard)))
    (if (string= "asis" arg)
        (setq texinfo-paragraph-indent "asis")
      (setq texinfo-paragraph-indent (string-to-number arg)))))

(put 'refill 'texinfo-format 'texinfo-format-refill)
(defun texinfo-format-refill ()
  "Refill paragraph.  Also, indent first line as set by @paragraphindent.
Default is to leave paragraph indentation as is."
  (texinfo-discard-command)
  (let ((position (point-marker)))
    (forward-paragraph -1)
    (if (looking-at "[ \t\n]*$") (forward-line 1))
    ;; Do not indent if an entry in a list, table, or deffn,
    ;; or if paragraph is preceded by @noindent.
    ;; Otherwise, indent
    (cond
     ;; delete a @noindent line and do not indent paragraph
     ((save-excursion (forward-line -1)
		      (looking-at "^@noindent"))
      (forward-line -1)
      (delete-region (point) (progn (forward-line 1) (point))))
     ;; do nothing if "asis"
     ((equal texinfo-paragraph-indent "asis"))
     ;; do no indenting in list, etc.
     ((> texinfo-stack-depth 0))
     ;; otherwise delete existing whitespace and indent
     (t
      (delete-region (point) (progn (skip-chars-forward " \t") (point)))
      (insert (make-string texinfo-paragraph-indent ? ))))
    (forward-paragraph 1)
    (forward-line -1)
    (end-of-line)
    ;; Do not fill a section title line with asterisks, hyphens, etc. that
    ;; are used to underline it.  This could occur if the line following
    ;; the underlining is not an index entry and has text within it.
    (let* ((previous-paragraph-separate paragraph-separate)
	   (paragraph-separate
	    (concat paragraph-separate "\\|[-=.]+\\|\\*\\*+"))
	   (previous-paragraph-start paragraph-start)
	   (paragraph-start
	    (concat paragraph-start "\\|[-=.]+\\|\\*\\*+")))
      (unwind-protect
	  (fill-paragraph nil)
	(setq paragraph-separate previous-paragraph-separate)
	(setq paragraph-start previous-paragraph-start)))
    (goto-char position)))

(put 'noindent 'texinfo-format 'texinfo-noindent)
(defun texinfo-noindent ()
  (save-excursion
    (forward-paragraph 1)
    (if (search-backward "@refill" (line-beginning-position 0) t)
        () ; leave @noindent command so @refill command knows not to indent
      ;; else
      (texinfo-discard-line))))


;;; Index generation

(put 'vindex 'texinfo-format 'texinfo-format-vindex)
(defun texinfo-format-vindex ()
  (texinfo-index 'texinfo-vindex))

(put 'cindex 'texinfo-format 'texinfo-format-cindex)
(defun texinfo-format-cindex ()
  (texinfo-index 'texinfo-cindex))

(put 'findex 'texinfo-format 'texinfo-format-findex)
(defun texinfo-format-findex ()
  (texinfo-index 'texinfo-findex))

(put 'pindex 'texinfo-format 'texinfo-format-pindex)
(defun texinfo-format-pindex ()
  (texinfo-index 'texinfo-pindex))

(put 'tindex 'texinfo-format 'texinfo-format-tindex)
(defun texinfo-format-tindex ()
  (texinfo-index 'texinfo-tindex))

(put 'kindex 'texinfo-format 'texinfo-format-kindex)
(defun texinfo-format-kindex ()
  (texinfo-index 'texinfo-kindex))

(defun texinfo-index (indexvar)
  (let ((arg (texinfo-parse-expanded-arg)))
    (texinfo-discard-command)
    (set indexvar
         (cons (list arg
                     texinfo-last-node
                     ;; Region formatting may not provide last node position.
                     (if texinfo-last-node-pos
                         (1+ (count-lines texinfo-last-node-pos (point)))
                       1))
               (symbol-value indexvar)))))

(defvar texinfo-indexvar-alist
  '(("cp" . texinfo-cindex)
    ("fn" . texinfo-findex)
    ("vr" . texinfo-vindex)
    ("tp" . texinfo-tindex)
    ("pg" . texinfo-pindex)
    ("ky" . texinfo-kindex)))


;;; @defindex   @defcodeindex
(put 'defindex 'texinfo-format 'texinfo-format-defindex)
(put 'defcodeindex 'texinfo-format 'texinfo-format-defindex)

(defun texinfo-format-defindex ()
  (let* ((index-name (texinfo-parse-arg-discard)) ; eg: `aa'
         (indexing-command (intern (concat index-name "index")))
         (index-formatting-command      ; eg: `texinfo-format-aaindex'
          (intern (concat "texinfo-format-" index-name "index")))
         (index-alist-name              ; eg: `texinfo-aaindex'
          (intern (concat "texinfo-" index-name "index"))))

    (set index-alist-name nil)

    (put indexing-command               ; eg, aaindex
         'texinfo-format
         index-formatting-command)      ; eg, texinfo-format-aaindex

    ;; eg: "aa" . texinfo-aaindex
    (or (assoc index-name texinfo-indexvar-alist)
        (push (cons index-name
                    index-alist-name)
              texinfo-indexvar-alist))

    (fset index-formatting-command
          (list 'lambda 'nil
                (list 'texinfo-index
                      (list 'quote index-alist-name))))))


;;; @synindex   @syncodeindex

(put 'synindex 'texinfo-format 'texinfo-format-synindex)
(put 'syncodeindex 'texinfo-format 'texinfo-format-synindex)

(defun texinfo-format-synindex ()
  (let* ((args (texinfo-parse-arg-discard))
         (second (cdr (read-from-string args)))
         (joiner (symbol-name (car (read-from-string args))))
         (joined (symbol-name (car (read-from-string args second)))))

    (if (assoc joiner texinfo-short-index-cmds-alist)
        (put
          (cdr (assoc joiner texinfo-short-index-cmds-alist))
         'texinfo-format
         (or (cdr (assoc joined texinfo-short-index-format-cmds-alist))
             (intern (concat "texinfo-format-" joined "index"))))
      (put
       (intern (concat joiner "index"))
       'texinfo-format
       (or (cdr(assoc joined texinfo-short-index-format-cmds-alist))
           (intern (concat "texinfo-format-" joined "index")))))))

(defconst texinfo-short-index-cmds-alist
  '(("cp" . cindex)
    ("fn" . findex)
    ("vr" . vindex)
    ("tp" . tindex)
    ("pg" . pindex)
    ("ky" . kindex)))

(defconst texinfo-short-index-format-cmds-alist
  '(("cp" . texinfo-format-cindex)
    ("fn" . texinfo-format-findex)
    ("vr" . texinfo-format-vindex)
    ("tp" . texinfo-format-tindex)
    ("pg" . texinfo-format-pindex)
    ("ky" . texinfo-format-kindex)))


;;; @printindex

(put 'printindex 'texinfo-format 'texinfo-format-printindex)

(defun texinfo-format-printindex ()
  (let* ((arg (texinfo-parse-arg-discard))
         (fmt (cdr (assoc arg texinfo-short-index-format-cmds-alist)))
         (index-list (delq nil (mapcar (lambda (e)
                                         (and (eq fmt (get (cdr e) 'texinfo-format))
                                              (cdr (assoc (car e) texinfo-indexvar-alist))))
                                       texinfo-short-index-cmds-alist)))
         (indexelts (apply #'append nil (mapcar #'symbol-value index-list)))
         opoint)
    (insert "\n* Menu:\n\n")
    (setq opoint (point))
    (texinfo-print-index nil indexelts)
    (shell-command-on-region opoint (point) "sort -fd" 1)))

(defun texinfo-print-index (file indexelts)
  (while indexelts
    (if (stringp (car (car indexelts)))
        (progn
          (insert "* " (car (car indexelts)) ": " )
          (indent-to 32)
          (insert
           (if file (concat "(" file ")") "")
           (nth 1 (car indexelts)) ".")
          (indent-to 54)
          (insert
           (if (nth 2 (car indexelts))
               (format "  (line %3d)" (1+ (nth 2 (car indexelts))))
             "")
           "\n"))
      ;; index entries from @include'd file
      (texinfo-print-index (nth 1 (car indexelts))
                           (nth 2 (car indexelts))))
    (setq indexelts (cdr indexelts))))


;;; Glyphs: @equiv, @error, etc

;; @equiv           to show that two expressions are equivalent
;; @error           to show an error message
;; @expansion       to show what a macro expands to
;; @point           to show the location of point in an example
;; @print           to show what an evaluated expression prints
;; @result          to indicate the value returned by an expression

(put 'equiv 'texinfo-format 'texinfo-format-equiv)
(defun texinfo-format-equiv ()
  (texinfo-parse-arg-discard)
  (insert "=="))

(put 'error 'texinfo-format 'texinfo-format-error)
(defun texinfo-format-error ()
  (texinfo-parse-arg-discard)
  (insert "error-->"))

(put 'expansion 'texinfo-format 'texinfo-format-expansion)
(defun texinfo-format-expansion ()
  (texinfo-parse-arg-discard)
  (insert "==>"))

(put 'point 'texinfo-format 'texinfo-format-point)
(defun texinfo-format-point ()
  (texinfo-parse-arg-discard)
  (insert "-!-"))

(put 'print 'texinfo-format 'texinfo-format-print)
(defun texinfo-format-print ()
  (texinfo-parse-arg-discard)
  (insert "-|"))

(put 'result 'texinfo-format 'texinfo-format-result)
(defun texinfo-format-result ()
  (texinfo-parse-arg-discard)
  (insert "=>"))


;;; Accent commands

;; Info presumes a plain ASCII output, so the accented characters do
;; not look as they would if typeset, or output with a different
;; character set.

;; See the `texinfo-accent-commands' variable
;; in the section for `texinfo-append-refill'.
;; Also, see the defun for `texinfo-format-scan'
;; for single-character accent commands.

;; Command           Info output         Name

;;   These do not have braces:
;; @^              ==>    ^         circumflex accent
;; @`              ==>    `         grave accent
;; @'              ==>    '         acute accent
;; @"              ==>    "         umlaut accent
;; @=              ==>    =         overbar accent
;; @~              ==>    ~         tilde accent

;;   These have braces, but take no argument:
;; @OE{}           ==>    OE        French-OE-ligature
;; @oe{}           ==>    oe
;; @AA{}           ==>    AA        Scandinavian-A-with-circle
;; @aa{}           ==>    aa
;; @AE{}           ==>    AE        Latin-Scandinavian-AE
;; @ae{}           ==>    ae
;; @ss{}           ==>    ss        German-sharp-S

;; @questiondown{} ==>    ?         upside-down-question-mark
;; @exclamdown{}   ==>    !         upside-down-exclamation-mark
;; @L{}            ==>    L/        Polish suppressed-L (Lslash)
;; @l{}            ==>    l/        Polish suppressed-L (Lslash) (lower case)
;; @O{}            ==>    O/        Scandinavian O-with-slash
;; @o{}            ==>    o/        Scandinavian O-with-slash (lower case)

;;   These have braces, and take an argument:
;; @,{c}           ==>    c,        cedilla accent
;; @dotaccent{o}   ==>    .o        overdot-accent
;; @ubaraccent{o}  ==>    _o        underbar-accent
;; @udotaccent{o}  ==>    o-.       underdot-accent
;; @H{o}           ==>    ""o       long Hungarian umlaut
;; @ringaccent{o}  ==>    *o        ring accent
;; @tieaccent{oo}  ==>    [oo       tie after accent
;; @u{o}           ==>    (o        breve accent
;; @v{o}           ==>    <o        hacek accent
;; @dotless{i}     ==>    i         dotless i and dotless j

;; ==========

;; Note: The  defun texinfo-format-scan
;; looks at "[@{}^'`\",=~ *?!-]"
;; In the case of @*, a line break is inserted;
;; in the other cases, the characters are simply quoted and the @ is deleted.
;; Thus, `texinfo-format-scan' handles the following
;; single-character accent commands: @^ @` @' @" @, @- @= @~

;; @^              ==>    ^         circumflex accent
;; (put '^ 'texinfo-format 'texinfo-format-circumflex-accent)
;; (defun texinfo-format-circumflex-accent ()
;;   (texinfo-discard-command)
;;   (insert "^"))
;;
;; @`              ==>    `         grave accent
;; (put '\` 'texinfo-format 'texinfo-format-grave-accent)
;; (defun texinfo-format-grave-accent ()
;;   (texinfo-discard-command)
;;   (insert "\`"))
;;
;; @'              ==>    '         acute accent
;; (put '\' 'texinfo-format 'texinfo-format-acute-accent)
;; (defun texinfo-format-acute-accent ()
;;   (texinfo-discard-command)
;;   (insert "'"))
;;
;; @"              ==>    "         umlaut accent
;; (put '\" 'texinfo-format 'texinfo-format-umlaut-accent)
;; (defun texinfo-format-umlaut-accent ()
;;   (texinfo-discard-command)
;;   (insert "\""))
;;
;; @=              ==>    =         overbar accent
;; (put '= 'texinfo-format 'texinfo-format-overbar-accent)
;; (defun texinfo-format-overbar-accent ()
;;   (texinfo-discard-command)
;;   (insert "="))
;;
;; @~              ==>    ~         tilde accent
;; (put '~ 'texinfo-format 'texinfo-format-tilde-accent)
;; (defun texinfo-format-tilde-accent ()
;;   (texinfo-discard-command)
;;   (insert "~"))

;; @OE{}           ==>    OE        French-OE-ligature
(put 'OE 'texinfo-format 'texinfo-format-French-OE-ligature)
(defun texinfo-format-French-OE-ligature ()
   (insert "OE" (texinfo-parse-arg-discard))
   (goto-char texinfo-command-start))

;; @oe{}           ==>    oe
(put 'oe 'texinfo-format 'texinfo-format-French-oe-ligature)
(defun texinfo-format-French-oe-ligature ()  ; lower case
   (insert "oe" (texinfo-parse-arg-discard))
   (goto-char texinfo-command-start))

;; @AA{}           ==>    AA        Scandinavian-A-with-circle
(put 'AA 'texinfo-format 'texinfo-format-Scandinavian-A-with-circle)
(defun texinfo-format-Scandinavian-A-with-circle ()
   (insert "AA" (texinfo-parse-arg-discard))
   (goto-char texinfo-command-start))

;; @aa{}           ==>    aa
(put 'aa 'texinfo-format 'texinfo-format-Scandinavian-a-with-circle)
(defun texinfo-format-Scandinavian-a-with-circle ()  ; lower case
   (insert "aa" (texinfo-parse-arg-discard))
   (goto-char texinfo-command-start))

;; @AE{}           ==>    AE        Latin-Scandinavian-AE
(put 'AE 'texinfo-format 'texinfo-format-Latin-Scandinavian-AE)
(defun texinfo-format-Latin-Scandinavian-AE ()
   (insert "AE" (texinfo-parse-arg-discard))
   (goto-char texinfo-command-start))

;; @ae{}           ==>    ae
(put 'ae 'texinfo-format 'texinfo-format-Latin-Scandinavian-ae)
(defun texinfo-format-Latin-Scandinavian-ae ()   ; lower case
   (insert "ae" (texinfo-parse-arg-discard))
   (goto-char texinfo-command-start))

;; @ss{}           ==>    ss        German-sharp-S
(put 'ss 'texinfo-format 'texinfo-format-German-sharp-S)
(defun texinfo-format-German-sharp-S ()
   (insert "ss" (texinfo-parse-arg-discard))
   (goto-char texinfo-command-start))

;; @questiondown{} ==>    ?         upside-down-question-mark
(put 'questiondown 'texinfo-format 'texinfo-format-upside-down-question-mark)
(defun texinfo-format-upside-down-question-mark ()
   (insert "?" (texinfo-parse-arg-discard))
   (goto-char texinfo-command-start))

;; @exclamdown{}   ==>    !         upside-down-exclamation-mark
(put 'exclamdown 'texinfo-format 'texinfo-format-upside-down-exclamation-mark)
(defun texinfo-format-upside-down-exclamation-mark ()
   (insert "!" (texinfo-parse-arg-discard))
   (goto-char texinfo-command-start))

;; @L{}            ==>    L/        Polish suppressed-L (Lslash)
(put 'L 'texinfo-format 'texinfo-format-Polish-suppressed-L)
(defun texinfo-format-Polish-suppressed-L ()
   (insert (texinfo-parse-arg-discard) "/L")
   (goto-char texinfo-command-start))

;; @l{}            ==>    l/        Polish suppressed-L (Lslash) (lower case)
(put 'l 'texinfo-format 'texinfo-format-Polish-suppressed-l-lower-case)
(defun texinfo-format-Polish-suppressed-l-lower-case ()
   (insert (texinfo-parse-arg-discard) "/l")
   (goto-char texinfo-command-start))


;; @O{}            ==>    O/        Scandinavian O-with-slash
(put 'O 'texinfo-format 'texinfo-format-Scandinavian-O-with-slash)
(defun texinfo-format-Scandinavian-O-with-slash ()
   (insert (texinfo-parse-arg-discard) "O/")
   (goto-char texinfo-command-start))

;; @o{}            ==>    o/        Scandinavian O-with-slash (lower case)
(put 'o 'texinfo-format 'texinfo-format-Scandinavian-o-with-slash-lower-case)
(defun texinfo-format-Scandinavian-o-with-slash-lower-case ()
   (insert (texinfo-parse-arg-discard) "o/")
   (goto-char texinfo-command-start))

;; Take arguments

;; @,{c}           ==>    c,        cedilla accent
(put '\, 'texinfo-format 'texinfo-format-cedilla-accent)
(defun texinfo-format-cedilla-accent ()
   (insert (texinfo-parse-arg-discard) ",")
  (goto-char texinfo-command-start))


;; @dotaccent{o}   ==>    .o        overdot-accent
(put 'dotaccent 'texinfo-format 'texinfo-format-overdot-accent)
(defun texinfo-format-overdot-accent ()
   (insert "." (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @ubaraccent{o}  ==>    _o        underbar-accent
(put 'ubaraccent 'texinfo-format 'texinfo-format-underbar-accent)
(defun texinfo-format-underbar-accent ()
   (insert "_" (texinfo-parse-arg-discard))
   (goto-char texinfo-command-start))

;; @udotaccent{o}  ==>    o-.       underdot-accent
(put 'udotaccent 'texinfo-format 'texinfo-format-underdot-accent)
(defun texinfo-format-underdot-accent ()
   (insert (texinfo-parse-arg-discard) "-.")
   (goto-char texinfo-command-start))

;; @H{o}           ==>    ""o       long Hungarian umlaut
(put 'H 'texinfo-format 'texinfo-format-long-Hungarian-umlaut)
(defun texinfo-format-long-Hungarian-umlaut ()
   (insert "\"\"" (texinfo-parse-arg-discard))
   (goto-char texinfo-command-start))

;; @ringaccent{o}  ==>    *o        ring accent
(put 'ringaccent 'texinfo-format 'texinfo-format-ring-accent)
(defun texinfo-format-ring-accent ()
   (insert "*" (texinfo-parse-arg-discard))
   (goto-char texinfo-command-start))

;; @tieaccent{oo}  ==>    [oo       tie after accent
(put 'tieaccent 'texinfo-format 'texinfo-format-tie-after-accent)
(defun texinfo-format-tie-after-accent ()
   (insert "[" (texinfo-parse-arg-discard))
   (goto-char texinfo-command-start))


;; @u{o}           ==>    (o        breve accent
(put 'u 'texinfo-format 'texinfo-format-breve-accent)
(defun texinfo-format-breve-accent ()
   (insert "(" (texinfo-parse-arg-discard))
   (goto-char texinfo-command-start))

;; @v{o}           ==>    <o        hacek accent
(put 'v 'texinfo-format 'texinfo-format-hacek-accent)
(defun texinfo-format-hacek-accent ()
   (insert "<" (texinfo-parse-arg-discard))
   (goto-char texinfo-command-start))


;; @dotless{i}     ==>    i         dotless i and dotless j
(put 'dotless 'texinfo-format 'texinfo-format-dotless)
(defun texinfo-format-dotless ()
   (insert  (texinfo-parse-arg-discard))
   (goto-char texinfo-command-start))


;;; Definition formatting: @deffn, @defun, etc

;; What definition formatting produces:
;;
;; @deffn category name args...
;;     In Info, `Category: name ARGS'
;;     In index: name:  node. line#.
;;
;; @defvr category name
;;     In Info, `Category: name'
;;     In index: name:  node. line#.
;;
;; @deftp category name attributes...
;; `category name attributes...'       Note: @deftp args in lower case.
;;     In index: name:  node. line#.
;;
;; Specialized function-like or variable-like entity:
;;
;; @defun, @defmac, @defspec, @defvar, @defopt
;;
;; @defun name args           In Info, `Function: name ARGS'
;; @defmac name args          In Info, `Macro: name ARGS'
;; @defvar name               In Info, `Variable: name'
;; etc.
;;     In index: name:  node. line#.
;;
;; Generalized typed-function-like or typed-variable-like entity:
;; @deftypefn category data-type name args...
;;     In Info, `Category:  data-type name args...'
;; @deftypevr category data-type name
;;     In Info, `Category:  data-type name'
;;     In index: name:  node. line#.
;;
;; Specialized typed-function-like or typed-variable-like entity:
;; @deftypefun data-type name args...
;;     In Info, `Function:  data-type name ARGS'
;;     In index: name:  node. line#.
;;
;; @deftypevar data-type name
;;     In Info, `Variable:  data-type name'
;;     In index: name:  node. line#.   but include args after name!?
;;
;; Generalized object oriented entity:
;; @defop category class name args...
;;     In Info, `Category on class: name ARG'
;;     In index: name on class: node. line#.
;;
;; @defcv category class name
;;     In Info, `Category of class: name'
;;     In index: name of class: node. line#.
;;
;; Specialized object oriented entity:
;; @defmethod class name args...
;;     In Info, `Method on class: name ARGS'
;;     In index: name on class: node. line#.
;;
;; @defivar class name
;;     In Info, `Instance variable of class: name'
;;     In index: name of class: node. line#.


;;; The definition formatting functions

(defun texinfo-format-defun ()
  (texinfo-push-stack 'defun nil)
  (setq fill-column (- fill-column 5))
  (texinfo-format-defun-1 t))

(defun texinfo-end-defun ()
  (setq fill-column (+ fill-column 5))
  (texinfo-discard-command)
  (let ((start (nth 1 (texinfo-pop-stack 'defun))))
    (texinfo-do-itemize start)
    ;; Delete extra newline inserted after header.
    (save-excursion
      (goto-char start)
      (delete-char -1))))

(defun texinfo-format-defunx ()
  (texinfo-format-defun-1 nil))

(defun texinfo-format-defun-1 (first-p)
  (let ((parse-args (texinfo-format-parse-defun-args))
        (texinfo-defun-type (get texinfo-command-name 'texinfo-defun-type)))
    (texinfo-discard-command)
    ;; Delete extra newline inserted after previous header line.
    (if (not first-p)
        (delete-char -1))
    (funcall
     (get texinfo-command-name 'texinfo-deffn-formatting-property) parse-args)
    ;; Insert extra newline so that paragraph filling does not mess
    ;; with header line.
    (insert "\n\n")
    (rplaca (cdr (cdr (car texinfo-stack))) (point))
    (funcall
     (get texinfo-command-name 'texinfo-defun-indexing-property) parse-args)))

;;; Formatting the first line of a definition

;; @deffn, @defvr, @deftp
(put 'deffn 'texinfo-deffn-formatting-property 'texinfo-format-deffn)
(put 'deffnx 'texinfo-deffn-formatting-property 'texinfo-format-deffn)
(put 'defvr 'texinfo-deffn-formatting-property 'texinfo-format-deffn)
(put 'defvrx 'texinfo-deffn-formatting-property 'texinfo-format-deffn)
(put 'deftp 'texinfo-deffn-formatting-property 'texinfo-format-deffn)
(put 'deftpx 'texinfo-deffn-formatting-property 'texinfo-format-deffn)
(defun texinfo-format-deffn (parsed-args)
  ;; Generalized function-like, variable-like, or generic data-type entity:
  ;; @deffn category name args...
  ;;     In Info, `Category: name ARGS'
  ;; @deftp category name attributes...
  ;; `category name attributes...'       Note: @deftp args in lower case.
  (let ((category (car parsed-args))
        (name (car (cdr parsed-args)))
        (args (cdr (cdr parsed-args))))
    (insert " -- " category ": " name)
    (while args
      (insert " "
              (if (or (= ?& (aref (car args) 0))
                      (eq (eval (car texinfo-defun-type)) 'deftp-type))
                  (car args)
                (upcase (car args))))
      (setq args (cdr args)))))

;; @defun, @defmac, @defspec, @defvar, @defopt: Specialized, simple
(put 'defun 'texinfo-deffn-formatting-property
     'texinfo-format-specialized-defun)
(put 'defunx 'texinfo-deffn-formatting-property
     'texinfo-format-specialized-defun)
(put 'defmac 'texinfo-deffn-formatting-property
     'texinfo-format-specialized-defun)
(put 'defmacx 'texinfo-deffn-formatting-property
     'texinfo-format-specialized-defun)
(put 'defspec 'texinfo-deffn-formatting-property
     'texinfo-format-specialized-defun)
(put 'defspecx 'texinfo-deffn-formatting-property
     'texinfo-format-specialized-defun)
(put 'defvar 'texinfo-deffn-formatting-property
     'texinfo-format-specialized-defun)
(put 'defvarx 'texinfo-deffn-formatting-property
     'texinfo-format-specialized-defun)
(put 'defopt 'texinfo-deffn-formatting-property
     'texinfo-format-specialized-defun)
(put 'defoptx 'texinfo-deffn-formatting-property
     'texinfo-format-specialized-defun)
(defun texinfo-format-specialized-defun (parsed-args)
  ;; Specialized function-like or variable-like entity:
  ;; @defun name args           In Info, `Function: Name ARGS'
  ;; @defmac name args          In Info, `Macro: Name ARGS'
  ;; @defvar name               In Info, `Variable: Name'
  ;; Use cdr of texinfo-defun-type to determine category:
  (let ((category (car (cdr texinfo-defun-type)))
        (name (car parsed-args))
        (args (cdr parsed-args)))
    (insert " -- " category ": " name)
    (while args
      (insert " "
              (if (= ?& (aref (car args) 0))
                  (car args)
                (upcase (car args))))
      (setq args (cdr args)))))

;; @deftypefn, @deftypevr: Generalized typed
(put 'deftypefn 'texinfo-deffn-formatting-property 'texinfo-format-deftypefn)
(put 'deftypefnx 'texinfo-deffn-formatting-property 'texinfo-format-deftypefn)
(put 'deftypevr 'texinfo-deffn-formatting-property 'texinfo-format-deftypefn)
(put 'deftypevrx 'texinfo-deffn-formatting-property 'texinfo-format-deftypefn)
(defun texinfo-format-deftypefn (parsed-args)
  ;; Generalized typed-function-like or typed-variable-like entity:
  ;; @deftypefn category data-type name args...
  ;;     In Info, `Category:  data-type name args...'
  ;; @deftypevr category data-type name
  ;;     In Info, `Category:  data-type name'
  ;; Note: args in lower case, unless modified in command line.
  (let ((category (car parsed-args))
        (data-type (car (cdr parsed-args)))
        (name (car (cdr (cdr parsed-args))))
        (args (cdr (cdr (cdr parsed-args)))))
    (insert " -- " category ": " data-type " " name)
    (while args
      (insert " " (car args))
      (setq args (cdr args)))))

;; @deftypefun, @deftypevar: Specialized typed
(put 'deftypefun 'texinfo-deffn-formatting-property 'texinfo-format-deftypefun)
(put 'deftypefunx 'texinfo-deffn-formatting-property
     'texinfo-format-deftypefun)
(put 'deftypevar 'texinfo-deffn-formatting-property 'texinfo-format-deftypefun)
(put 'deftypevarx 'texinfo-deffn-formatting-property
     'texinfo-format-deftypefun)
(defun texinfo-format-deftypefun (parsed-args)
  ;; Specialized typed-function-like or typed-variable-like entity:
  ;; @deftypefun data-type name args...
  ;;     In Info, `Function:  data-type name ARGS'
  ;; @deftypevar data-type name
  ;;     In Info, `Variable:  data-type name'
  ;; Note: args in lower case, unless modified in command line.
  ;; Use cdr of texinfo-defun-type to determine category:
  (let ((category (car (cdr texinfo-defun-type)))
        (data-type (car parsed-args))
        (name (car (cdr  parsed-args)))
        (args (cdr (cdr parsed-args))))
    (insert " -- " category ": " data-type " " name)
    (while args
      (insert " " (car args))
      (setq args (cdr args)))))

;; @defop: Generalized object-oriented
(put 'defop 'texinfo-deffn-formatting-property 'texinfo-format-defop)
(put 'defopx 'texinfo-deffn-formatting-property 'texinfo-format-defop)
(defun texinfo-format-defop (parsed-args)
  ;; Generalized object oriented entity:
  ;; @defop category class name args...
  ;;     In Info, `Category on class: name ARG'
  ;; Note: args in upper case; use of `on'
  (let ((category (car parsed-args))
        (class (car (cdr parsed-args)))
        (name (car (cdr (cdr parsed-args))))
        (args (cdr (cdr (cdr parsed-args)))))
    (insert " -- " category " on " class ": " name)
    (while args
      (insert " " (upcase (car args)))
      (setq args (cdr args)))))

;; @defcv: Generalized object-oriented
(put 'defcv 'texinfo-deffn-formatting-property 'texinfo-format-defcv)
(put 'defcvx 'texinfo-deffn-formatting-property 'texinfo-format-defcv)
(defun texinfo-format-defcv (parsed-args)
  ;; Generalized object oriented entity:
  ;; @defcv category class name
  ;;     In Info, `Category of class: name'
  ;; Note: args in upper case; use of `of'
  (let ((category (car parsed-args))
        (class (car (cdr parsed-args)))
        (name (car (cdr (cdr parsed-args))))
        (args (cdr (cdr (cdr parsed-args)))))
    (insert " -- " category " of " class ": " name)
    (while args
      (insert " " (upcase (car args)))
      (setq args (cdr args)))))

;; @defmethod: Specialized object-oriented
(put 'defmethod 'texinfo-deffn-formatting-property 'texinfo-format-defmethod)
(put 'defmethodx 'texinfo-deffn-formatting-property 'texinfo-format-defmethod)
(defun texinfo-format-defmethod (parsed-args)
  ;; Specialized object oriented entity:
  ;; @defmethod class name args...
  ;;     In Info, `Method on class: name ARGS'
  ;; Note: args in upper case; use of `on'
  ;; Use cdr of texinfo-defun-type to determine category:
  (let ((category (car (cdr texinfo-defun-type)))
        (class (car parsed-args))
        (name (car (cdr  parsed-args)))
        (args (cdr  (cdr parsed-args))))
    (insert " -- " category " on " class ": " name)
    (while args
      (insert " " (upcase (car args)))
      (setq args (cdr args)))))

;; @defivar: Specialized object-oriented
(put 'defivar 'texinfo-deffn-formatting-property 'texinfo-format-defivar)
(put 'defivarx 'texinfo-deffn-formatting-property 'texinfo-format-defivar)
(defun texinfo-format-defivar (parsed-args)
  ;; Specialized object oriented entity:
  ;; @defivar class name
  ;;     In Info, `Instance variable of class: name'
  ;; Note: args in upper case; use of `of'
  ;; Use cdr of texinfo-defun-type to determine category:
  (let ((category (car (cdr texinfo-defun-type)))
        (class (car parsed-args))
        (name (car (cdr  parsed-args)))
        (args (cdr  (cdr parsed-args))))
    (insert " -- " category " of " class ": " name)
    (while args
      (insert " " (upcase (car args)))
      (setq args (cdr args)))))


;;; Indexing for definitions

;; An index entry has three parts: the `entry proper', the node name, and the
;; line number.  Depending on the which command is used, the entry is
;; formatted differently:
;;
;; @defun,
;; @defmac,
;; @defspec,
;; @defvar,
;; @defopt          all use their 1st argument as the entry-proper
;;
;; @deffn,
;; @defvr,
;; @deftp
;; @deftypefun
;; @deftypevar      all use their 2nd argument as the entry-proper
;;
;; @deftypefn,
;; @deftypevr       both use their 3rd argument as the entry-proper
;;
;; @defmethod       uses its 2nd and 1st arguments as an entry-proper
;;                    formatted: NAME on CLASS

;; @defop           uses its 3rd and 2nd arguments as an entry-proper
;;                    formatted: NAME on CLASS
;;
;; @defivar         uses its 2nd and 1st arguments as an entry-proper
;;                    formatted: NAME of CLASS
;;
;; @defcv           uses its 3rd and 2nd argument as an entry-proper
;;                    formatted: NAME of CLASS

(put 'defun 'texinfo-defun-indexing-property 'texinfo-index-defun)
(put 'defunx 'texinfo-defun-indexing-property 'texinfo-index-defun)
(put 'defmac 'texinfo-defun-indexing-property 'texinfo-index-defun)
(put 'defmacx 'texinfo-defun-indexing-property 'texinfo-index-defun)
(put 'defspec 'texinfo-defun-indexing-property 'texinfo-index-defun)
(put 'defspecx 'texinfo-defun-indexing-property 'texinfo-index-defun)
(put 'defvar 'texinfo-defun-indexing-property 'texinfo-index-defun)
(put 'defvarx 'texinfo-defun-indexing-property 'texinfo-index-defun)
(put 'defopt  'texinfo-defun-indexing-property 'texinfo-index-defun)
(put 'defoptx  'texinfo-defun-indexing-property 'texinfo-index-defun)
(defun texinfo-index-defun (parsed-args)
  ;; use 1st parsed-arg  as entry-proper
  ;; `index-list' will be texinfo-findex or the like
  (let ((index-list (get texinfo-command-name 'texinfo-defun-index)))
    (set index-list
         (cons
          ;; Three elements: entry-proper, node-name, line-number
          (list
           (car parsed-args)
           texinfo-last-node
           ;; Region formatting may not provide last node position.
           (if texinfo-last-node-pos
               (1+ (count-lines texinfo-last-node-pos (point)))
             1))
          (symbol-value index-list)))))

(put 'deffn 'texinfo-defun-indexing-property 'texinfo-index-deffn)
(put 'deffnx 'texinfo-defun-indexing-property 'texinfo-index-deffn)
(put 'defvr 'texinfo-defun-indexing-property 'texinfo-index-deffn)
(put 'defvrx 'texinfo-defun-indexing-property 'texinfo-index-deffn)
(put 'deftp 'texinfo-defun-indexing-property 'texinfo-index-deffn)
(put 'deftpx 'texinfo-defun-indexing-property 'texinfo-index-deffn)
(put 'deftypefun 'texinfo-defun-indexing-property 'texinfo-index-deffn)
(put 'deftypefunx 'texinfo-defun-indexing-property 'texinfo-index-deffn)
(put 'deftypevar 'texinfo-defun-indexing-property 'texinfo-index-deffn)
(put 'deftypevarx 'texinfo-defun-indexing-property 'texinfo-index-deffn)
(defun texinfo-index-deffn (parsed-args)
 ;; use 2nd parsed-arg  as entry-proper
  ;; `index-list' will be texinfo-findex or the like
  (let ((index-list (get texinfo-command-name 'texinfo-defun-index)))
    (set index-list
         (cons
          ;; Three elements: entry-proper, node-name, line-number
          (list
           (car (cdr parsed-args))
           texinfo-last-node
           ;; Region formatting may not provide last node position.
           (if texinfo-last-node-pos
               (1+ (count-lines texinfo-last-node-pos (point)))
             1))
          (symbol-value index-list)))))

(put 'deftypefn 'texinfo-defun-indexing-property 'texinfo-index-deftypefn)
(put 'deftypefnx 'texinfo-defun-indexing-property 'texinfo-index-deftypefn)
(put 'deftypevr 'texinfo-defun-indexing-property 'texinfo-index-deftypefn)
(put 'deftypevrx 'texinfo-defun-indexing-property 'texinfo-index-deftypefn)
(defun texinfo-index-deftypefn (parsed-args)
  ;; use 3rd parsed-arg  as entry-proper
  ;; `index-list' will be texinfo-findex or the like
  (let ((index-list (get texinfo-command-name 'texinfo-defun-index)))
    (set index-list
         (cons
          ;; Three elements: entry-proper, node-name, line-number
          (list
           (car (cdr (cdr parsed-args)))
           texinfo-last-node
           ;; Region formatting may not provide last node position.
           (if texinfo-last-node-pos
               (1+ (count-lines texinfo-last-node-pos (point)))
             1))
          (symbol-value index-list)))))

(put 'defmethod 'texinfo-defun-indexing-property 'texinfo-index-defmethod)
(put 'defmethodx 'texinfo-defun-indexing-property 'texinfo-index-defmethod)
(defun texinfo-index-defmethod (parsed-args)
  ;; use 2nd on 1st parsed-arg  as entry-proper
  ;; `index-list' will be texinfo-findex or the like
  (let ((index-list (get texinfo-command-name 'texinfo-defun-index)))
    (set index-list
         (cons
          ;; Three elements: entry-proper, node-name, line-number
          (list
           (format "%s on %s"
                   (car (cdr parsed-args))
                   (car parsed-args))
           texinfo-last-node
           ;; Region formatting may not provide last node position.
           (if texinfo-last-node-pos
               (1+ (count-lines texinfo-last-node-pos (point)))
             1))
          (symbol-value index-list)))))

(put 'defop 'texinfo-defun-indexing-property 'texinfo-index-defop)
(put 'defopx 'texinfo-defun-indexing-property 'texinfo-index-defop)
(defun texinfo-index-defop (parsed-args)
  ;; use 3rd on 2nd parsed-arg  as entry-proper
  ;; `index-list' will be texinfo-findex or the like
  (let ((index-list (get texinfo-command-name 'texinfo-defun-index)))
    (set index-list
         (cons
          ;; Three elements: entry-proper, node-name, line-number
          (list
           (format "%s on %s"
                   (car (cdr (cdr parsed-args)))
                   (car (cdr parsed-args)))
           texinfo-last-node
           ;; Region formatting may not provide last node position.
           (if texinfo-last-node-pos
               (1+ (count-lines texinfo-last-node-pos (point)))
             1))
          (symbol-value index-list)))))

(put 'defivar 'texinfo-defun-indexing-property 'texinfo-index-defivar)
(put 'defivarx 'texinfo-defun-indexing-property 'texinfo-index-defivar)
(defun texinfo-index-defivar (parsed-args)
  ;; use 2nd of 1st parsed-arg  as entry-proper
  ;; `index-list' will be texinfo-findex or the like
  (let ((index-list (get texinfo-command-name 'texinfo-defun-index)))
    (set index-list
         (cons
          ;; Three elements: entry-proper, node-name, line-number
          (list
           (format "%s of %s"
                   (car (cdr parsed-args))
                   (car parsed-args))
           texinfo-last-node
           ;; Region formatting may not provide last node position.
           (if texinfo-last-node-pos
               (1+ (count-lines texinfo-last-node-pos (point)))
             1))
          (symbol-value index-list)))))

(put 'defcv 'texinfo-defun-indexing-property 'texinfo-index-defcv)
(put 'defcvx 'texinfo-defun-indexing-property 'texinfo-index-defcv)
(defun texinfo-index-defcv (parsed-args)
  ;; use 3rd of 2nd parsed-arg  as entry-proper
  ;; `index-list' will be texinfo-findex or the like
  (let ((index-list (get texinfo-command-name 'texinfo-defun-index)))
    (set index-list
         (cons
          ;; Three elements: entry-proper, node-name, line-number
          (list
           (format "%s of %s"
                   (car (cdr (cdr parsed-args)))
                   (car (cdr parsed-args)))
           texinfo-last-node
           ;; Region formatting may not provide last node position.
           (if texinfo-last-node-pos
               (1+ (count-lines texinfo-last-node-pos (point)))
             1))
          (symbol-value index-list)))))


;;; Properties for definitions

;; Each definition command has six properties:
;;
;; 1. texinfo-deffn-formatting-property      to format definition line
;; 2. texinfo-defun-indexing-property        to create index entry
;; 3. texinfo-format                         formatting command
;; 4. texinfo-end                            end formatting command
;; 5. texinfo-defun-type                     type of deffn to format
;; 6. texinfo-defun-index                    type of index to use
;;
;; The `x' forms of each definition command are used for the second
;; and subsequent header lines.

;; The texinfo-deffn-formatting-property and texinfo-defun-indexing-property
;; are listed just before the appropriate formatting and indexing commands.

(put 'deffn 'texinfo-format 'texinfo-format-defun)
(put 'deffnx 'texinfo-format 'texinfo-format-defunx)
(put 'deffn 'texinfo-end 'texinfo-end-defun)
(put 'deffn 'texinfo-defun-type '('deffn-type nil))
(put 'deffnx 'texinfo-defun-type '('deffn-type nil))
(put 'deffn 'texinfo-defun-index 'texinfo-findex)
(put 'deffnx 'texinfo-defun-index 'texinfo-findex)

(put 'defun 'texinfo-format 'texinfo-format-defun)
(put 'defunx 'texinfo-format 'texinfo-format-defunx)
(put 'defun 'texinfo-end 'texinfo-end-defun)
(put 'defun 'texinfo-defun-type '('defun-type "Function"))
(put 'defunx 'texinfo-defun-type '('defun-type "Function"))
(put 'defun 'texinfo-defun-index 'texinfo-findex)
(put 'defunx 'texinfo-defun-index 'texinfo-findex)

(put 'defmac 'texinfo-format 'texinfo-format-defun)
(put 'defmacx 'texinfo-format 'texinfo-format-defunx)
(put 'defmac 'texinfo-end 'texinfo-end-defun)
(put 'defmac 'texinfo-defun-type '('defun-type "Macro"))
(put 'defmacx 'texinfo-defun-type '('defun-type "Macro"))
(put 'defmac 'texinfo-defun-index 'texinfo-findex)
(put 'defmacx 'texinfo-defun-index 'texinfo-findex)

(put 'defspec 'texinfo-format 'texinfo-format-defun)
(put 'defspecx 'texinfo-format 'texinfo-format-defunx)
(put 'defspec 'texinfo-end 'texinfo-end-defun)
(put 'defspec 'texinfo-defun-type '('defun-type "Special form"))
(put 'defspecx 'texinfo-defun-type '('defun-type "Special form"))
(put 'defspec 'texinfo-defun-index 'texinfo-findex)
(put 'defspecx 'texinfo-defun-index 'texinfo-findex)

(put 'defvr 'texinfo-format 'texinfo-format-defun)
(put 'defvrx 'texinfo-format 'texinfo-format-defunx)
(put 'defvr 'texinfo-end 'texinfo-end-defun)
(put 'defvr 'texinfo-defun-type '('deffn-type nil))
(put 'defvrx 'texinfo-defun-type '('deffn-type nil))
(put 'defvr 'texinfo-defun-index 'texinfo-vindex)
(put 'defvrx 'texinfo-defun-index 'texinfo-vindex)

(put 'defvar 'texinfo-format 'texinfo-format-defun)
(put 'defvarx 'texinfo-format 'texinfo-format-defunx)
(put 'defvar 'texinfo-end 'texinfo-end-defun)
(put 'defvar 'texinfo-defun-type '('defun-type "Variable"))
(put 'defvarx 'texinfo-defun-type '('defun-type "Variable"))
(put 'defvar 'texinfo-defun-index 'texinfo-vindex)
(put 'defvarx 'texinfo-defun-index 'texinfo-vindex)

(put 'defconst 'texinfo-format 'texinfo-format-defun)
(put 'defconstx 'texinfo-format 'texinfo-format-defunx)
(put 'defconst 'texinfo-end 'texinfo-end-defun)
(put 'defconst 'texinfo-defun-type '('defun-type "Constant"))
(put 'defconstx 'texinfo-defun-type '('defun-type "Constant"))
(put 'defconst 'texinfo-defun-index 'texinfo-vindex)
(put 'defconstx 'texinfo-defun-index 'texinfo-vindex)

(put 'defcmd 'texinfo-format 'texinfo-format-defun)
(put 'defcmdx 'texinfo-format 'texinfo-format-defunx)
(put 'defcmd 'texinfo-end 'texinfo-end-defun)
(put 'defcmd 'texinfo-defun-type '('defun-type "Command"))
(put 'defcmdx 'texinfo-defun-type '('defun-type "Command"))
(put 'defcmd 'texinfo-defun-index 'texinfo-findex)
(put 'defcmdx 'texinfo-defun-index 'texinfo-findex)

(put 'defopt 'texinfo-format 'texinfo-format-defun)
(put 'defoptx 'texinfo-format 'texinfo-format-defunx)
(put 'defopt 'texinfo-end 'texinfo-end-defun)
(put 'defopt 'texinfo-defun-type '('defun-type "User Option"))
(put 'defoptx 'texinfo-defun-type '('defun-type "User Option"))
(put 'defopt 'texinfo-defun-index 'texinfo-vindex)
(put 'defoptx 'texinfo-defun-index 'texinfo-vindex)

(put 'deftp 'texinfo-format 'texinfo-format-defun)
(put 'deftpx 'texinfo-format 'texinfo-format-defunx)
(put 'deftp 'texinfo-end 'texinfo-end-defun)
(put 'deftp 'texinfo-defun-type '('deftp-type nil))
(put 'deftpx 'texinfo-defun-type '('deftp-type nil))
(put 'deftp 'texinfo-defun-index 'texinfo-tindex)
(put 'deftpx 'texinfo-defun-index 'texinfo-tindex)

;;; Object-oriented stuff is a little hairier.

(put 'defop 'texinfo-format 'texinfo-format-defun)
(put 'defopx 'texinfo-format 'texinfo-format-defunx)
(put 'defop 'texinfo-end 'texinfo-end-defun)
(put 'defop 'texinfo-defun-type '('defop-type nil))
(put 'defopx 'texinfo-defun-type '('defop-type nil))
(put 'defop 'texinfo-defun-index 'texinfo-findex)
(put 'defopx 'texinfo-defun-index 'texinfo-findex)

(put 'defmethod 'texinfo-format 'texinfo-format-defun)
(put 'defmethodx 'texinfo-format 'texinfo-format-defunx)
(put 'defmethod 'texinfo-end 'texinfo-end-defun)
(put 'defmethod 'texinfo-defun-type '('defmethod-type "Method"))
(put 'defmethodx 'texinfo-defun-type '('defmethod-type "Method"))
(put 'defmethod 'texinfo-defun-index 'texinfo-findex)
(put 'defmethodx 'texinfo-defun-index 'texinfo-findex)

(put 'defcv 'texinfo-format 'texinfo-format-defun)
(put 'defcvx 'texinfo-format 'texinfo-format-defunx)
(put 'defcv 'texinfo-end 'texinfo-end-defun)
(put 'defcv 'texinfo-defun-type '('defop-type nil))
(put 'defcvx 'texinfo-defun-type '('defop-type nil))
(put 'defcv 'texinfo-defun-index 'texinfo-vindex)
(put 'defcvx 'texinfo-defun-index 'texinfo-vindex)

(put 'defivar 'texinfo-format 'texinfo-format-defun)
(put 'defivarx 'texinfo-format 'texinfo-format-defunx)
(put 'defivar 'texinfo-end 'texinfo-end-defun)
(put 'defivar 'texinfo-defun-type '('defmethod-type "Instance variable"))
(put 'defivarx 'texinfo-defun-type '('defmethod-type "Instance variable"))
(put 'defivar 'texinfo-defun-index 'texinfo-vindex)
(put 'defivarx 'texinfo-defun-index 'texinfo-vindex)

;;; Typed functions and variables

(put 'deftypefn 'texinfo-format 'texinfo-format-defun)
(put 'deftypefnx 'texinfo-format 'texinfo-format-defunx)
(put 'deftypefn 'texinfo-end 'texinfo-end-defun)
(put 'deftypefn 'texinfo-defun-type '('deftypefn-type nil))
(put 'deftypefnx 'texinfo-defun-type '('deftypefn-type nil))
(put 'deftypefn 'texinfo-defun-index 'texinfo-findex)
(put 'deftypefnx 'texinfo-defun-index 'texinfo-findex)

(put 'deftypefun 'texinfo-format 'texinfo-format-defun)
(put 'deftypefunx 'texinfo-format 'texinfo-format-defunx)
(put 'deftypefun 'texinfo-end 'texinfo-end-defun)
(put 'deftypefun 'texinfo-defun-type '('deftypefun-type "Function"))
(put 'deftypefunx 'texinfo-defun-type '('deftypefun-type "Function"))
(put 'deftypefun 'texinfo-defun-index 'texinfo-findex)
(put 'deftypefunx 'texinfo-defun-index 'texinfo-findex)

(put 'deftypevr 'texinfo-format 'texinfo-format-defun)
(put 'deftypevrx 'texinfo-format 'texinfo-format-defunx)
(put 'deftypevr 'texinfo-end 'texinfo-end-defun)
(put 'deftypevr 'texinfo-defun-type '('deftypefn-type nil))
(put 'deftypevrx 'texinfo-defun-type '('deftypefn-type nil))
(put 'deftypevr 'texinfo-defun-index 'texinfo-vindex)
(put 'deftypevrx 'texinfo-defun-index 'texinfo-vindex)

(put 'deftypevar 'texinfo-format 'texinfo-format-defun)
(put 'deftypevarx 'texinfo-format 'texinfo-format-defunx)
(put 'deftypevar 'texinfo-end 'texinfo-end-defun)
(put 'deftypevar 'texinfo-defun-type '('deftypevar-type "Variable"))
(put 'deftypevarx 'texinfo-defun-type '('deftypevar-type "Variable"))
(put 'deftypevar 'texinfo-defun-index 'texinfo-vindex)
(put 'deftypevarx 'texinfo-defun-index 'texinfo-vindex)


;;; @set, @clear, @ifset, @ifclear

;; If a flag is set with @set FLAG, then text between @ifset and @end
;; ifset is formatted normally, but if the flag is cleared with
;; @clear FLAG, then the text is not formatted; it is ignored.

;; If a flag is cleared with @clear FLAG, then text between @ifclear
;; and @end ifclear is formatted normally, but if the flag is set with
;; @set FLAG, then the text is not formatted; it is ignored.  @ifclear
;; is the opposite of @ifset.

;; If a flag is set to a string with @set FLAG,
;; replace  @value{FLAG} with the string.
;; If a flag with a value is cleared,
;; @value{FLAG} is invalid,
;; as if there had never been any @set FLAG previously.

(put 'clear 'texinfo-format 'texinfo-clear)
(defun texinfo-clear ()
  "Clear the value of the flag."
  (let* ((arg (texinfo-parse-arg-discard))
         (flag (car (read-from-string arg)))
         (value (substring arg (cdr (read-from-string arg)))))
    (put flag 'texinfo-whether-setp 'flag-cleared)
    (put flag 'texinfo-set-value "")))

(put 'set 'texinfo-format 'texinfo-set)
(defun texinfo-set ()
  "Set the value of the flag, optionally to a string.
The command  `@set foo This is a string.'
sets flag foo to the value: `This is a string.'
The command  `@value{foo}'  expands to the value."
  (let* ((arg (texinfo-parse-arg-discard))
         (flag (car (read-from-string arg)))
         (value (substring arg (cdr (read-from-string arg)))))
    (if (string-match "^[ \t]+" value)
	(setq value (substring value (match-end 0))))
    (put flag 'texinfo-whether-setp 'flag-set)
    (put flag 'texinfo-set-value value)))

(put 'value 'texinfo-format 'texinfo-value)
(defun texinfo-value ()
  "Insert the string to which the flag is set.
The command  `@set foo This is a string.'
sets flag foo to the value: `This is a string.'
The command  `@value{foo}'  expands to the value."
  (let ((arg (texinfo-parse-arg-discard)))
    (cond ((and
            (eq (get (car (read-from-string arg)) 'texinfo-whether-setp)
                'flag-set)
            (get (car (read-from-string arg)) 'texinfo-set-value))
           (insert (get (car (read-from-string arg)) 'texinfo-set-value)))
          ((eq (get (car (read-from-string arg)) 'texinfo-whether-setp)
               'flag-cleared)
           (insert (format "{No value for \"%s\"}"  arg)))
          ((eq (get (car (read-from-string arg)) 'texinfo-whether-setp) nil)
           (insert (format "{No value for \"%s\"}"  arg))))))

(put 'ifset 'texinfo-end 'texinfo-discard-command)
(put 'ifset 'texinfo-format 'texinfo-if-set)
(defun texinfo-if-set ()
  "If set, continue formatting; else do not format region up to @end ifset."
  (let ((arg (texinfo-parse-arg-discard)))
    (cond
     ((eq (get (car (read-from-string arg)) 'texinfo-whether-setp)
          'flag-set)
      ;; Format the text (i.e., do not remove it); do nothing here.
      ())
     ((eq (get (car (read-from-string arg)) 'texinfo-whether-setp)
          'flag-cleared)
      ;; Clear region (i.e., cause the text to be ignored).
      (delete-region texinfo-command-start
		     (re-search-forward "@end ifset[ \t]*\n")))
     ((eq (get (car (read-from-string arg)) 'texinfo-whether-setp)
          nil)
      ;; In this case flag is neither set nor cleared.
      ;; Act as if set, i.e. do nothing.
      ()))))

(put 'ifclear 'texinfo-end 'texinfo-discard-command)
(put 'ifclear 'texinfo-format 'texinfo-if-clear)
(defun texinfo-if-clear ()
  "If clear, continue formatting; if set, do not format up to @end ifset."
  (let ((arg (texinfo-parse-arg-discard)))
    (cond
     ((eq (get (car (read-from-string arg)) 'texinfo-whether-setp)
          'flag-set)
      ;; Clear region (i.e., cause the text to be ignored).
      (delete-region texinfo-command-start
		     (re-search-forward "@end ifclear[ \t]*\n")))
     ((eq (get (car (read-from-string arg)) 'texinfo-whether-setp)
          'flag-cleared)
      ;; Format the text (i.e., do not remove it); do nothing here.
      ())
     ((eq (get (car (read-from-string arg)) 'texinfo-whether-setp)
          nil)
      ;; In this case flag is neither set nor cleared.
      ;; Act as if clear, i.e. do nothing.
      ()))))

;;; @ifeq

(put 'ifeq 'texinfo-format 'texinfo-format-ifeq)
(defun texinfo-format-ifeq ()
  "If ARG1 and ARG2 caselessly string compare to same string, perform COMMAND.
Otherwise produces no output.

Thus:
        @ifeq{ arg1 , arg1 , @code{foo}} bar

        ==> `foo' bar.
but
        @ifeq{ arg1 , arg2 , @code{foo}} bar

        ==> bar

Note that the Texinfo command and its arguments must be arguments to
the @ifeq command."
  ;; compare-buffer-substrings does not exist in version 18; don't use
  (goto-char texinfo-command-end)
  (let* ((case-fold-search t)
         (stop (save-excursion (forward-sexp 1) (point)))
        start end
        ;; @ifeq{arg1, arg2, @command{optional-args}}
        (arg1
         (progn
           (forward-char 1)
           (skip-chars-forward " ")
           (setq start (point))
           (search-forward "," stop t)
           (skip-chars-backward ", ")
           (buffer-substring-no-properties start (point))))
        (arg2
         (progn
           (search-forward "," stop t)
           (skip-chars-forward " ")
           (setq start (point))
           (search-forward "," stop t)
           (skip-chars-backward ", ")
           (buffer-substring-no-properties start (point))))
        (texinfo-command
         (progn
           (search-forward "," stop t)
           (skip-chars-forward " ")
           (setq start (point))
           (goto-char (1- stop))
           (skip-chars-backward " ")
           (buffer-substring-no-properties start (point)))))
    (delete-region texinfo-command-start stop)
    (if (equal arg1 arg2)
        (insert texinfo-command))
    (goto-char texinfo-command-start)))


;;; Process included files:  `@include' command

;; Updated 19 October 1990
;; In the original version, include files were ignored by Info but
;; incorporated in to the printed manual.  To make references to the
;; included file, the Texinfo source file has to refer to the included
;; files using the `(filename)nodename' format for referring to other
;; Info files.  Also, the included files had to be formatted on their
;; own.  It was just like they were another file.

;; Currently, include files are inserted into the buffer that is
;; formatted for Info.  If large, the resulting info file is split and
;; tagified.  For current include files to work, the master menu must
;; refer to all the nodes, and the highest level nodes in the include
;; files must have the correct next, prev, and up pointers.

;; The included file may have an @setfilename and even an @settitle,
;; but not an `\input texinfo' line.

;; Updated 24 March 1993
;; In order for @raisesections and @lowersections to work, included
;; files must be inserted into the buffer holding the outer file
;; before other Info formatting takes place.  So @include is no longer
;; is treated like other @-commands.
(put 'include 'texinfo-format  'texinfo-format-noop)

;; Original definition:
;; (defun texinfo-format-include ()
;;   (let ((filename (texinfo-parse-arg-discard))
;;       (default-directory input-directory)
;;       subindex)
;;     (setq subindex
;;         (save-excursion
;;           (progn (find-file
;;                   (cond ((file-readable-p (concat filename ".texinfo"))
;;                          (concat filename ".texinfo"))
;;                         ((file-readable-p (concat filename ".texi"))
;;                          (concat filename ".texi"))
;;                         ((file-readable-p (concat filename ".tex"))
;;                          (concat filename ".tex"))
;;                         ((file-readable-p filename)
;;                          filename)
;;                         (t (error "@include'd file %s not found"
;;                                   filename))))
;;                  (texinfo-format-buffer-1))))
;;     (texinfo-subindex 'texinfo-vindex (car subindex) (nth 1 subindex))
;;     (texinfo-subindex 'texinfo-findex (car subindex) (nth 2 subindex))
;;     (texinfo-subindex 'texinfo-cindex (car subindex) (nth 3 subindex))
;;     (texinfo-subindex 'texinfo-pindex (car subindex) (nth 4 subindex))
;;     (texinfo-subindex 'texinfo-tindex (car subindex) (nth 5 subindex))
;;     (texinfo-subindex 'texinfo-kindex (car subindex) (nth 6 subindex))))
;;
;;(defun texinfo-subindex (indexvar file content)
;;  (set indexvar (cons (list 'recurse file content)
;;                      (symbol-value indexvar))))

;; Second definition:
;; (put 'include 'texinfo-format 'texinfo-format-include)
;; (defun texinfo-format-include ()
;;   (let ((filename (concat input-directory
;;                           (texinfo-parse-arg-discard)))
;;         (default-directory input-directory))
;;     (message "Reading: %s" filename)
;;     (save-excursion
;;       (save-restriction
;;         (narrow-to-region
;;          (point)
;;          (+ (point) (car (cdr (insert-file-contents filename)))))
;;         (goto-char (point-min))
;;         (texinfo-append-refill)
;;         (texinfo-format-convert (point-min) (point-max))))
;;     (setq last-input-buffer input-buffer)  ; to bypass setfilename
;;     ))


;;; Numerous commands do nothing in Info
;; These commands are defined in texinfo.tex for printed output.


;;; various noops, such as @b{foo}, that take arguments in braces

(put 'b 'texinfo-format 'texinfo-format-noop)
(put 'i 'texinfo-format 'texinfo-format-noop)
(put 'r 'texinfo-format 'texinfo-format-noop)
(put 't 'texinfo-format 'texinfo-format-noop)
(put 'w 'texinfo-format 'texinfo-format-noop)
(put 'asis 'texinfo-format 'texinfo-format-noop)
(put 'dmn 'texinfo-format 'texinfo-format-noop)
(put 'math 'texinfo-format 'texinfo-format-noop)
(put 'titlefont 'texinfo-format 'texinfo-format-noop)
(defun texinfo-format-noop ()
  (insert (texinfo-parse-arg-discard))
  (goto-char texinfo-command-start))

;; @hyphenation command discards an argument within braces
(put 'hyphenation 'texinfo-format 'texinfo-discard-command-and-arg)
(defun texinfo-discard-command-and-arg ()
  "Discard both @-command and its argument in braces."
  (goto-char texinfo-command-end)
  (forward-list 1)
  (setq texinfo-command-end (point))
  (delete-region texinfo-command-start texinfo-command-end))


;;; Do nothing commands, such as @smallbook, that have no args and no braces
;;  These must appear on a line of their own

(put 'bye 'texinfo-format 'texinfo-discard-line)
(put 'smallbook 'texinfo-format 'texinfo-discard-line)
(put 'finalout 'texinfo-format 'texinfo-discard-line)
(put 'overfullrule 'texinfo-format 'texinfo-discard-line)
(put 'smallbreak 'texinfo-format 'texinfo-discard-line)
(put 'medbreak 'texinfo-format 'texinfo-discard-line)
(put 'bigbreak 'texinfo-format 'texinfo-discard-line)
(put 'afourpaper 'texinfo-format 'texinfo-discard-line)
(put 'afivepaper 'texinfo-format 'texinfo-discard-line)
(put 'afourlatex 'texinfo-format 'texinfo-discard-line)
(put 'afourwide  'texinfo-format 'texinfo-discard-line)


;;; These noop commands discard the rest of the line.

(put 'c 'texinfo-format 'texinfo-discard-line-with-args)
(put 'comment 'texinfo-format 'texinfo-discard-line-with-args)
(put 'contents 'texinfo-format 'texinfo-discard-line-with-args)
(put 'group 'texinfo-end 'texinfo-discard-line-with-args)
(put 'group 'texinfo-format 'texinfo-discard-line-with-args)
(put 'headings 'texinfo-format 'texinfo-discard-line-with-args)
(put 'setchapterstyle 'texinfo-format 'texinfo-discard-line-with-args)
(put 'hsize 'texinfo-format 'texinfo-discard-line-with-args)
(put 'itemindent 'texinfo-format 'texinfo-discard-line-with-args)
(put 'lispnarrowing 'texinfo-format 'texinfo-discard-line-with-args)
(put 'need 'texinfo-format 'texinfo-discard-line-with-args)
(put 'nopara 'texinfo-format 'texinfo-discard-line-with-args)

;; @novalidate suppresses cross-reference checking and auxiliary file
;; creation with TeX.  The Info-validate command checks that every
;; node pointer points to an existing node.  Since this Info command
;; is not invoked automatically, the @novalidate command is irrelevant
;; and not supported by texinfmt.el
(put 'novalidate 'texinfo-format 'texinfo-discard-line-with-args)

(put 'page 'texinfo-format 'texinfo-discard-line-with-args)
(put 'pagesizes 'texinfo-format 'texinfo-discard-line-with-args)
(put 'parindent 'texinfo-format 'texinfo-discard-line-with-args)
(put 'setchapternewpage 'texinfo-format 'texinfo-discard-line-with-args)
(put 'setq 'texinfo-format 'texinfo-discard-line-with-args)

(put 'setcontentsaftertitlepage
     'texinfo-format 'texinfo-discard-line-with-args)
(put 'setshortcontentsaftertitlepage
     'texinfo-format 'texinfo-discard-line-with-args)

(put 'settitle 'texinfo-format 'texinfo-discard-line-with-args)
(put 'setx 'texinfo-format 'texinfo-discard-line-with-args)
(put 'shortcontents 'texinfo-format 'texinfo-discard-line-with-args)
(put 'shorttitlepage 'texinfo-format 'texinfo-discard-line-with-args)
(put 'summarycontents 'texinfo-format 'texinfo-discard-line-with-args)
(put 'input 'texinfo-format 'texinfo-discard-line-with-args)

(put 'documentlanguage 'texinfo-format 'texinfo-discard-line-with-args)
(put 'documentencoding 'texinfo-format 'texinfo-discard-line-with-args)



;;; Some commands cannot be handled

(defun texinfo-unsupported ()
  (error "%s is not handled by texinfo"
         (buffer-substring-no-properties texinfo-command-start texinfo-command-end)))

;;; Batch formatting

(defun batch-texinfo-format ()
  "Run `texinfo-format-buffer' on the files remaining on the command line.
Must be used only with -batch, and kills Emacs on completion.
Each file will be processed even if an error occurred previously.
For example, invoke
  \"emacs -batch -l texinfmt -f batch-texinfo-format $docs/ ~/*.texinfo\"."
  (if (not noninteractive)
      (error "batch-texinfo-format may only be used -batch"))
  (let ((version-control t)
        (auto-save-default nil)
        (find-file-run-dired nil)
        (kept-old-versions 259259)
        (kept-new-versions 259259))
    (let ((error 0)
          file
          (files ()))
      (while command-line-args-left
        (setq file (expand-file-name (car command-line-args-left)))
        (cond ((not (file-exists-p file))
               (message ">> %s does not exist!" file)
               (setq error 1
                     command-line-args-left (cdr command-line-args-left)))
              ((file-directory-p file)
               (setq command-line-args-left
                     (nconc (directory-files file)
                            (cdr command-line-args-left))))
              (t
               (push file files)
               (setq command-line-args-left (cdr command-line-args-left)))))
      (while files
        (setq file (car files)
              files (cdr files))
        (condition-case err
            (progn
              (if buffer-file-name (kill-buffer (current-buffer)))
              (find-file file)
              (buffer-disable-undo (current-buffer))
              (set-buffer-modified-p nil)
              (texinfo-mode)
              (message "texinfo formatting %s..." file)
              (texinfo-format-buffer nil)
              (if (buffer-modified-p)
                  (progn (message "Saving modified %s" (buffer-file-name))
                         (save-buffer))))
          (error
           (message ">> Error: %s" (prin1-to-string err))
           (message ">>  point at")
           (let ((s (buffer-substring-no-properties (point)
						    (min (+ (point) 100)
							 (point-max))))
                 (tem 0))
             (while (setq tem (string-match "\n+" s tem))
               (setq s (concat (substring s 0 (match-beginning 0))
                               "\n>>  "
                               (substring s (match-end 0)))
                     tem (1+ tem)))
             (message ">>  %s" s))
           (setq error 1))))
      (kill-emacs error))))


;;; Place `provide' at end of file.
(provide 'texinfmt)

;;; texinfmt.el ends here
