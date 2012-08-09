;;; org-beamer.el --- Beamer-specific LaTeX export for org-mode
;;
;; Copyright (C) 2007-2012  Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten.dominik AT gmail DOT com>
;; Maintainer: Carsten Dominik <carsten.dominik AT gmail DOT com>
;; Keywords: org, wp, tex

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
;; This library implement the special treatment needed by using the
;; beamer class during LaTeX export.

;;; Code:

(require 'org)
(require 'org-exp)

(defvar org-export-latex-header)
(defvar org-export-latex-options-plist)
(defvar org-export-opt-plist)

(defgroup org-beamer nil
  "Options specific for using the beamer class in LaTeX export."
  :tag "Org Beamer"
  :group 'org-export-latex)

(defcustom org-beamer-use-parts nil
  ""
  :group 'org-beamer
  :version "24.1"
  :type 'boolean)

(defcustom org-beamer-frame-level 1
  "The level that should be interpreted as a frame.
The levels above this one will be translated into a sectioning structure.
Setting this to 2 will allow sections, 3 will allow subsections as well.
You can set this to 4 as well, if you at the same time set
`org-beamer-use-parts' to make the top levels `\part'."
  :group 'org-beamer
  :version "24.1"
  :type '(choice
	  (const :tag "Frames need a BEAMER_env property" nil)
	  (integer :tag "Specific level makes a frame")))

(defcustom org-beamer-frame-default-options ""
  "Default options string to use for frames, should contains the [brackets].
And example for this is \"[allowframebreaks]\"."
  :group 'org-beamer
  :version "24.1"
  :type '(string :tag "[options]"))

(defcustom org-beamer-column-view-format
  "%45ITEM %10BEAMER_env(Env) %10BEAMER_envargs(Env Args) %4BEAMER_col(Col) %8BEAMER_extra(Extra)"
  "Default column view format that should be used to fill the template."
  :group 'org-beamer
  :version "24.1"
  :type '(choice
	  (const  :tag "Do not insert Beamer column view format" nil)
	  (string :tag "Beamer column view format")))

(defcustom org-beamer-themes
  "\\usetheme{default}\\usecolortheme{default}"
  "Default string to be used for extra heading stuff in beamer presentations.
When a beamer template is filled, this will be the default for
BEAMER_HEADER_EXTRA, which will be inserted just before \\begin{document}."
  :group 'org-beamer
  :version "24.1"
  :type '(choice
	  (const  :tag "Do not insert Beamer themes" nil)
	  (string :tag "Beamer themes")))

(defconst org-beamer-column-widths
  "0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 0.0 :ETC"
"The column widths that should be installed as allowed property values.")

(defconst org-beamer-transitions
  "\transblindsvertical \transblindshorizontal \transboxin \transboxout \transdissolve \transduration \transglitter \transsplithorizontalin \transsplithorizontalout \transsplitverticalin \transsplitverticalout \transwipe :ETC"
  "Transitions available for beamer.
These are just a completion help.")

(defconst org-beamer-environments-default
  '(("frame"          "f" "dummy- special handling hard coded" "dummy")
    ("columns"        "C" "\\begin{columns}%o %% %h%x"         "\\end{columns}")
    ("column"         "c" "\\begin{column}%o{%h\\textwidth}%x" "\\end{column}")
    ("block"          "b" "\\begin{block}%a{%h}%x"             "\\end{block}")
    ("alertblock"     "a" "\\begin{alertblock}%a{%h}%x"        "\\end{alertblock}")
    ("verse"          "v" "\\begin{verse}%a %% %h%x"           "\\end{verse}")
    ("quotation"      "q" "\\begin{quotation}%a %% %h%x"       "\\end{quotation}")
    ("quote"          "Q" "\\begin{quote}%a %% %h%x"           "\\end{quote}")
    ("structureenv"   "s" "\\begin{structureenv}%a %% %h%x"    "\\end{structureenv}")
    ("theorem"        "t" "\\begin{theorem}%a%U%x"             "\\end{theorem}")
    ("definition"     "d" "\\begin{definition}%a%U%x"          "\\end{definition}")
    ("example"        "e" "\\begin{example}%a%U%x"             "\\end{example}")
    ("proof"          "p" "\\begin{proof}%a%U%x"               "\\end{proof}")
    ("beamercolorbox" "o" "\\begin{beamercolorbox}%o{%h}%x"    "\\end{beamercolorbox}")
    ("normal"         "h" "%h" "") ; Emit the heading as normal text
    ("note"           "n" "\\note%o%a{%h"                      "}")
    ("noteNH"         "N" "\\note%o%a{"                        "}") ; note, ignore heading
    ("ignoreheading"  "i" "%%%% %h" ""))
  "Environments triggered by properties in Beamer export.
These are the defaults - for user definitions, see
`org-beamer-environments-extra'.
\"normal\" is a special fake environment, which emit the heading as
normal text. It is needed when an environment should be surrounded
by normal text.  Since beamer export converts nodes into environments,
you need to have a node to end the environment.
For example

   ** a frame
      some text
   *** Blocktitle :B_block:
       inside the block
   *** After the block :B_normal:
       continuing here
   ** next frame")

(defcustom org-beamer-environments-extra nil
  "Environments triggered by tags in Beamer export.
Each entry has 4 elements:

name    Name of the environment
key     Selection key for `org-beamer-select-environment'
open    The opening template for the environment, with the following escapes
        %a   the action/overlay specification
        %A   the default action/overlay specification
        %o   the options argument of the template
        %h   the headline text
        %H   if there is headline text, that text in {} braces
        %U   if there is headline text, that text in [] brackets
        %x   the content of the BEAMER_extra property
close   The closing string of the environment."

  :group 'org-beamer
  :version "24.1"
  :type '(repeat
	  (list
	   (string :tag "Environment")
	   (string :tag "Selection key")
	   (string :tag "Begin")
	   (string :tag "End"))))

(defvar org-beamer-frame-level-now nil)
(defvar org-beamer-header-extra nil)
(defvar org-beamer-export-is-beamer-p nil)
(defvar org-beamer-inside-frame-at-level nil)
(defvar org-beamer-columns-open nil)
(defvar org-beamer-column-open nil)

(defun org-beamer-cleanup-column-width (width)
  "Make sure the width is not empty, and that it has a unit."
  (setq width (org-trim (or width "")))
  (unless (string-match "\\S-" width) (setq width "0.5"))
  (if (string-match "\\`[.0-9]+\\'" width)
      (setq width (concat width "\\textwidth")))
  width)

(defun org-beamer-open-column (&optional width opt)
  (org-beamer-close-column-maybe)
  (setq org-beamer-column-open t)
  (setq width (org-beamer-cleanup-column-width width))
  (insert (format "\\begin{column}%s{%s}\n" (or opt "") width)))
(defun org-beamer-close-column-maybe ()
  (when org-beamer-column-open
    (setq org-beamer-column-open nil)
    (insert "\\end{column}\n")))
(defun org-beamer-open-columns-maybe (&optional opts)
  (unless org-beamer-columns-open
    (setq org-beamer-columns-open t)
    (insert (format "\\begin{columns}%s\n" (or opts "")))))
(defun org-beamer-close-columns-maybe ()
  (org-beamer-close-column-maybe)
  (when org-beamer-columns-open
    (setq org-beamer-columns-open nil)
    (insert "\\end{columns}\n")))

(defun org-beamer-select-environment ()
  "Select the environment to be used by beamer for this entry.
While this uses (for convenience) a tag selection interface, the result
of this command will be that the BEAMER_env *property* of the entry is set.

In addition to this, the command will also set a tag as a visual aid, but
the tag does not have any semantic meaning."
  (interactive)
  (let* ((envs (append org-beamer-environments-extra
		       org-beamer-environments-default))
	 (org-tag-alist
	  (append '((:startgroup))
		  (mapcar (lambda (e) (cons (concat "B_" (car e))
					    (string-to-char (nth 1 e))))
			  envs)
		  '((:endgroup))
		  '(("BMCOL" . ?|))))
	 (org-fast-tag-selection-single-key t))
    (org-set-tags)
    (let ((tags (or (ignore-errors (org-get-tags-string)) "")))
      (cond
       ((equal org-last-tag-selection-key ?|)
	(if (string-match ":BMCOL:" tags)
	    (org-set-property "BEAMER_col" (read-string "Column width: "))
	  (org-delete-property "BEAMER_col")))
       ((string-match (concat ":B_\\("
			      (mapconcat 'car envs "\\|")
			      "\\):")
		      tags)
	(org-entry-put nil "BEAMER_env" (match-string 1 tags)))
       (t (org-entry-delete nil "BEAMER_env"))))))


(defun org-beamer-sectioning (level text)
  "Return the sectioning entry for the current headline.
LEVEL is the reduced level of the headline.
TEXT is the text of the headline, everything except the leading stars.
The return value is a cons cell.  The car is the headline text, usually
just TEXT, but possibly modified if options have been extracted from the
text.  The cdr is the sectioning entry, similar to what is given
in org-export-latex-classes."
  (let* ((frame-level (or org-beamer-frame-level-now org-beamer-frame-level))
	 (default
	   (if org-beamer-use-parts
	       '((1 . ("\\part{%s}" . "\\part*{%s}"))
		 (2 . ("\\section{%s}" . "\\section*{%s}"))
		 (3 . ("\\subsection{%s}" . "\\subsection*{%s}")))
	     '((1 . ("\\section{%s}" . "\\section*{%s}"))
	       (2 . ("\\subsection{%s}" . "\\subsection*{%s}")))))
	 (envs (append org-beamer-environments-extra
		       org-beamer-environments-default))
	 (props (org-get-text-property-any 0 'org-props text))
	 (in "") (out "") org-beamer-option org-beamer-action org-beamer-defaction org-beamer-environment org-beamer-extra
	 columns-option column-option
	 env have-text ass tmp)
    (if (= frame-level 0) (setq frame-level nil))
    (when (and org-beamer-inside-frame-at-level
	       (<= level org-beamer-inside-frame-at-level))
      (setq org-beamer-inside-frame-at-level nil))
    (when (setq tmp (org-beamer-assoc-not-empty "BEAMER_col" props))
      (if (and (string-match "\\`[0-9.]+\\'" tmp)
	       (or (= (string-to-number tmp) 1.0)
		   (= (string-to-number tmp) 0.0)))
	  ;; column width 1 means close columns, go back to full width
	  (org-beamer-close-columns-maybe)
	(when (setq ass (assoc "BEAMER_envargs" props))
	  (let (case-fold-search)
	    (while (string-match "C\\(\\[[^][]*\\]\\|<[^<>]*>\\)" (cdr ass))
	      (setq columns-option (match-string 1 (cdr ass)))
	      (setcdr ass (replace-match "" t t (cdr ass))))
	    (while (string-match "c\\(\\[[^][]*\\]\\|<[^<>]*>\\)" (cdr ass))
	      (setq column-option (match-string 1 (cdr ass)))
	      (setcdr ass (replace-match "" t t (cdr ass))))))
	(org-beamer-open-columns-maybe columns-option)
	(org-beamer-open-column tmp column-option)))
    (cond
     ((or (equal (cdr (assoc "BEAMER_env" props)) "frame")
	  (and frame-level (= level frame-level)))
      ;; A frame
      (org-beamer-get-special props)

      (setq in (org-fill-template
		"\\begin{frame}%a%A%o%T%S%x"
		(list (cons "a" (or org-beamer-action ""))
		      (cons "A" (or org-beamer-defaction ""))
		      (cons "o" (or org-beamer-option org-beamer-frame-default-options ""))
		      (cons "x" (if org-beamer-extra (concat "\n" org-beamer-extra) ""))
		      (cons "h" "%s")
		      (cons "T" (if (string-match "\\S-" text)
				    "\n\\frametitle{%s}" ""))
		      (cons "S" (if (string-match "\\\\\\\\" text)
				    "\n\\framesubtitle{%s}" ""))))
	    out (copy-sequence "\\end{frame}"))
      (org-add-props out
	  '(org-insert-hook org-beamer-close-columns-maybe))
      (setq org-beamer-inside-frame-at-level level)
      (cons text (list in out in out)))
     ((and (setq env (cdr (assoc "BEAMER_env" props)))
	   (setq ass (assoc env envs)))
      ;; A beamer environment selected by the BEAMER_env property
      (if (string-match "[ \t]+:[ \t]*$" text)
	  (setq text (replace-match "" t t text)))
      (if (member env '("note" "noteNH"))
	  ;; There should be no labels in a note, so we remove the targets
	  ;; FIXME???
	  (remove-text-properties 0 (length text) '(target nil) text))
      (org-beamer-get-special props)
      (setq text (org-trim text))
      (setq have-text (string-match "\\S-" text))
      (setq in (org-fill-template
		(nth 2 ass)
		(list (cons "a" (or org-beamer-action ""))
		      (cons "A" (or org-beamer-defaction ""))
		      (cons "o" (or org-beamer-option ""))
		      (cons "x" (if org-beamer-extra (concat "\n" org-beamer-extra) ""))
		      (cons "h" "%s")
		      (cons "H" (if have-text (concat "{" text "}") ""))
		      (cons "U" (if have-text (concat "[" text "]") ""))))
	    out (nth 3 ass))
      (cond
       ((equal out "\\end{columns}")
	(setq org-beamer-columns-open t)
	(setq out (org-add-props (copy-sequence out)
		      '(org-insert-hook
			(lambda ()
			  (org-beamer-close-column-maybe)
			  (setq org-beamer-columns-open nil))))))
       ((equal out "\\end{column}")
	(org-beamer-open-columns-maybe)))
      (cons text (list in out in out)))
     ((and (not org-beamer-inside-frame-at-level)
	   (or (not frame-level)
	       (< level frame-level))
	   (assoc level default))
      ;; Normal sectioning
      (cons text (cdr (assoc level default))))
     (t nil))))

(defvar org-beamer-extra)
(defvar org-beamer-option)
(defvar org-beamer-action)
(defvar org-beamer-defaction)
(defvar org-beamer-environment)
(defun org-beamer-get-special (props)
  "Extract an option, action, and default action string from text.
The variables org-beamer-option, org-beamer-action, org-beamer-defaction,
org-beamer-extra are all scoped into this function dynamically."
  (let (tmp)
    (setq org-beamer-environment (org-beamer-assoc-not-empty "BEAMER_env" props))
    (setq org-beamer-extra (org-beamer-assoc-not-empty "BEAMER_extra" props))
    (when org-beamer-extra
      (setq org-beamer-extra (replace-regexp-in-string "\\\\n" "\n" org-beamer-extra)))
    (setq tmp (org-beamer-assoc-not-empty "BEAMER_envargs" props))
    (when tmp
      (setq tmp (copy-sequence tmp))
      (if (string-match "\\[<[^][<>]*>\\]" tmp)
	  (setq org-beamer-defaction (match-string 0 tmp)
		tmp (replace-match "" t t tmp)))
      (if (string-match "\\[[^][]*\\]" tmp)
	  (setq org-beamer-option (match-string 0 tmp)
		tmp (replace-match "" t t tmp)))
      (if (string-match "<[^<>]*>" tmp)
	  (setq org-beamer-action (match-string 0 tmp)
		tmp (replace-match "" t t tmp))))))

(defun org-beamer-assoc-not-empty (elt list)
  (let ((tmp (cdr (assoc elt list))))
    (and tmp (string-match "\\S-" tmp) tmp)))


(defvar org-beamer-mode-map (make-sparse-keymap)
  "The keymap for `org-beamer-mode'.")
(define-key org-beamer-mode-map "\C-c\C-b" 'org-beamer-select-environment)

(define-minor-mode org-beamer-mode
  "Special support for editing Org-mode files made to export to beamer."
  nil " Bm" nil)
(when (fboundp 'font-lock-add-keywords)
  (font-lock-add-keywords
   'org-mode
   '((":\\(B_[a-z]+\\|BMCOL\\):" 1 'org-beamer-tag prepend))
   'prepent))

(defun org-beamer-place-default-actions-for-lists ()
  "Find default overlay specifications in items, and move them.
The need to be after the begin statement of the environment."
  (when org-beamer-export-is-beamer-p
    (let (dovl)
      (goto-char (point-min))
      (while (re-search-forward
	      "^[ \t]*\\\\begin{\\(itemize\\|enumerate\\|description\\)}[ \t\n]*\\\\item\\>\\( ?\\(<[^<>\n]*>\\|\\[[^][\n*]\\]\\)\\)?[ \t]*\\S-" nil t)
	(if (setq dovl (cdr (assoc "BEAMER_dovl"
				   (get-text-property (match-end 0)
						      'org-props))))
	    (save-excursion
	      (goto-char (1+ (match-end 1)))
	      (insert dovl)))))))

(defun org-beamer-amend-header ()
  "Add `org-beamer-header-extra' to the LaTeX header.
If the file contains the string BEAMER-HEADER-EXTRA-HERE on a line
by itself, it will be replaced with `org-beamer-header-extra'.  If not,
the value will be inserted right after the documentclass statement."
  (when (and org-beamer-export-is-beamer-p
	     org-beamer-header-extra)
    (goto-char (point-min))
    (cond
     ((re-search-forward
       "^[ \t]*\\[?BEAMER-HEADER-EXTRA\\(-HERE\\)?\\]?[ \t]*$" nil t)
      (replace-match org-beamer-header-extra t t)
      (or (bolp) (insert "\n")))
     ((re-search-forward "^[ \t]*\\\\begin{document}" nil t)
      (beginning-of-line 1)
      (insert org-beamer-header-extra)
      (or (bolp) (insert "\n"))))))

(defcustom org-beamer-fragile-re "\\\\\\(verb\\|lstinline\\)\\|^[ \t]*\\\\begin{\\(verbatim\\|lstlisting\\|minted\\)}"
  "If this regexp matches in a frame, the frame is marked as fragile."
  :group 'org-beamer
  :version "24.1"
  :type 'regexp)

(defface org-beamer-tag '((t (:box (:line-width 1 :color grey40))))
  "The special face for beamer tags."
  :group 'org-beamer)


;; Functions to initialize and post-process
;; These functions will be hooked into various places in the export process

(defun org-beamer-initialize-open-trackers ()
  "Reset variables that track if certain environments are open during export."
  (setq org-beamer-columns-open nil)
  (setq org-beamer-column-open nil)
  (setq org-beamer-inside-frame-at-level nil)
  (setq org-beamer-export-is-beamer-p nil))

(defun org-beamer-after-initial-vars ()
  "Find special settings for beamer and store them.
The effect is that these values will be accessible during export."
  ;; First verify that we are exporting using the beamer class
  (setq org-beamer-export-is-beamer-p
	(string-match "\\\\documentclass\\(\\[[^][]*?\\]\\)?{beamer}"
		      org-export-latex-header))
  (when org-beamer-export-is-beamer-p
    ;; Find the frame level
    (setq org-beamer-frame-level-now
	  (or (and (org-region-active-p)
		   (save-excursion
		     (goto-char (region-beginning))
		     (and (looking-at org-complex-heading-regexp)
			  (org-entry-get nil "BEAMER_FRAME_LEVEL" 'selective))))
	      (save-excursion
		(save-restriction
		  (widen)
		  (goto-char (point-min))
		  (and (re-search-forward
			"^#\\+BEAMER_FRAME_LEVEL:[ \t]*\\(.*?\\)[ \t]*$" nil t)
		       (match-string 1))))
	      (plist-get org-export-latex-options-plist :beamer-frame-level)
	      org-beamer-frame-level))
    ;; Normalize the value so that the functions can trust the value
    (cond
     ((not org-beamer-frame-level-now)
      (setq org-beamer-frame-level-now nil))
     ((stringp org-beamer-frame-level-now)
      (setq org-beamer-frame-level-now
	    (string-to-number org-beamer-frame-level-now))))
    ;; Find the header additions, most likely theme commands
    (setq org-beamer-header-extra
	  (or (and (org-region-active-p)
		   (save-excursion
		     (goto-char (region-beginning))
		     (and (looking-at org-complex-heading-regexp)
			  (org-entry-get nil "BEAMER_HEADER_EXTRA"
					 'selective))))
	      (save-excursion
		(save-restriction
		  (widen)
		  (let ((txt ""))
		    (goto-char (point-min))
		    (while (re-search-forward
			    "^#\\+BEAMER_HEADER_EXTRA:[ \t]*\\(.*?\\)[ \t]*$"
			    nil t)
		      (setq txt (concat txt "\n" (match-string 1))))
		    (if (> (length txt) 0) (substring txt 1)))))
	      (plist-get org-export-latex-options-plist
			 :beamer-header-extra)))
    (let ((inhibit-read-only t)
	  (case-fold-search nil)
	  props)
      (org-unmodified
       (remove-text-properties (point-min) (point-max) '(org-props nil))
       (org-map-entries
	'(progn
	   (setq props (org-entry-properties nil 'standard))
	   (if (and (not (assoc "BEAMER_env" props))
		    (looking-at ".*?:B_\\(note\\(NH\\)?\\):"))
	       (push (cons "BEAMER_env" (match-string 1)) props))
	   (put-text-property (point-at-bol) (point-at-eol) 'org-props props)))
       (setq org-export-latex-options-plist
	     (plist-put org-export-latex-options-plist :tags nil))))))

(defun org-beamer-auto-fragile-frames ()
  "Mark any frames containing verbatim environments as fragile.
This function will run in the final LaTeX document."
  (when org-beamer-export-is-beamer-p
    (let (opts)
      (goto-char (point-min))
      ;; Find something that might be fragile
      (while (re-search-forward org-beamer-fragile-re nil t)
	(save-excursion
	  ;; Are we inside a frame here?
	  (when (and (re-search-backward "^[ \t]*\\\\\\(begin\\|end\\){frame}"
					 nil t)
		     (equal (match-string 1) "begin"))
	    ;; yes, inside a frame, make sure "fragile" is one of the options
	    (goto-char (match-end 0))
	    (if (not (looking-at "\\[.*?\\]"))
		(insert "[fragile]")
	      (setq opts (substring (match-string 0) 1 -1))
	      (delete-region (match-beginning 0) (match-end 0))
	      (setq opts (org-split-string opts ","))
	      (add-to-list 'opts "fragile")
	      (insert "[" (mapconcat 'identity opts ",") "]"))))))))

(defcustom org-beamer-outline-frame-title "Outline"
  "Default title of a frame containing an outline."
  :group 'org-beamer
  :version "24.1"
  :type '(string :tag "Outline frame title")
)

(defcustom org-beamer-outline-frame-options nil
  "Outline frame options appended after \\begin{frame}.
You might want to put e.g. [allowframebreaks=0.9] here.  Remember to
include square brackets."
  :group 'org-beamer
  :version "24.1"
  :type '(string :tag "Outline frame options")
)

(defun org-beamer-fix-toc ()
  "Fix the table of contents by removing the vspace line."
  (when org-beamer-export-is-beamer-p
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "\\(\\\\setcounter{tocdepth.*\n\\\\tableofcontents.*\n\\)\\(\\\\vspace\\*.*\\)"
			       nil t)
	(replace-match
	 (concat "\\\\begin{frame}" org-beamer-outline-frame-options
		 "\n\\\\frametitle{"
		 org-beamer-outline-frame-title
		 "}\n\\1\\\\end{frame}")
	 t nil)))))

(defun org-beamer-property-changed (property value)
  "Track the BEAMER_env property with tags."
  (cond
   ((equal property "BEAMER_env")
    (save-excursion
      (org-back-to-heading t)
      (let ((tags (org-get-tags)))
	(setq tags (delq nil (mapcar (lambda (x)
				       (if (string-match "^B_" x) nil x))
				     tags)))
	(org-set-tags-to tags))
      (when (and value (stringp value) (string-match "\\S-" value))
	(org-toggle-tag (concat "B_" value) 'on))))
   ((equal property "BEAMER_col")
    (org-toggle-tag "BMCOL" (if (and value (string-match "\\S-" value))
				'on 'off)))))

(defun org-beamer-select-beamer-code ()
  "Take code marked for BEAMER and turn it into marked for LaTeX."
  (when org-beamer-export-is-beamer-p
    (goto-char (point-min))
    (while (re-search-forward
	    "^\\([ \]*#\\+\\(begin_\\|end_\\)?\\)\\(beamer\\)\\>" nil t)
      (replace-match "\\1latex"))))

;; OK, hook all these functions into appropriate places
(add-hook 'org-export-first-hook
	  'org-beamer-initialize-open-trackers)
(add-hook 'org-property-changed-functions
	  'org-beamer-property-changed)
(add-hook 'org-export-latex-after-initial-vars-hook
	  'org-beamer-after-initial-vars)
(add-hook 'org-export-latex-final-hook
	  'org-beamer-place-default-actions-for-lists)
(add-hook 'org-export-latex-final-hook
	  'org-beamer-auto-fragile-frames)
(add-hook 'org-export-latex-final-hook
	  'org-beamer-fix-toc)
(add-hook 'org-export-latex-final-hook
	  'org-beamer-amend-header)
(add-hook 'org-export-preprocess-before-selecting-backend-code-hook
	  'org-beamer-select-beamer-code)

(defun org-insert-beamer-options-template (&optional kind)
  "Insert a settings template, to make sure users do this right."
  (interactive (progn
		 (message "Current [s]ubtree or [g]lobal?")
		 (if (equal (read-char-exclusive) ?g)
		     (list 'global)
		   (list 'subtree))))
  (if (eq kind 'subtree)
      (progn
	(org-back-to-heading t)
	(org-reveal)
	(org-entry-put nil "LaTeX_CLASS" "beamer")
	(org-entry-put nil "LaTeX_CLASS_OPTIONS" "[presentation]")
	(org-entry-put nil "EXPORT_FILE_NAME" "presentation.pdf")
	(org-entry-put nil "BEAMER_FRAME_LEVEL" (number-to-string
						 org-beamer-frame-level))
	(when org-beamer-themes
	  (org-entry-put nil "BEAMER_HEADER_EXTRA" org-beamer-themes))
	(when org-beamer-column-view-format
	  (org-entry-put nil "COLUMNS" org-beamer-column-view-format))
	(org-entry-put nil "BEAMER_col_ALL" "0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0 :ETC"))
    (insert "#+LaTeX_CLASS: beamer\n")
    (insert "#+LaTeX_CLASS_OPTIONS: [presentation]\n")
    (insert (format "#+BEAMER_FRAME_LEVEL: %d\n" org-beamer-frame-level) "\n")
    (when org-beamer-themes
      (insert "#+BEAMER_HEADER_EXTRA: " org-beamer-themes "\n"))
    (when org-beamer-column-view-format
      (insert "#+COLUMNS: " org-beamer-column-view-format "\n"))
    (insert "#+PROPERTY: BEAMER_col_ALL 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0 :ETC\n")))


(defun org-beamer-allowed-property-values (property)
  "Supply allowed values for BEAMER properties."
  (cond
   ((and (equal property "BEAMER_env")
	 (not (org-entry-get nil (concat property "_ALL") 'inherit)))
    ;; If no allowed values for BEAMER_env have been defined,
    ;; supply all defined environments
    (mapcar 'car (append org-beamer-environments-extra
			 org-beamer-environments-default)))
   ((and (equal property "BEAMER_col")
	 (not (org-entry-get nil (concat property "_ALL") 'inherit)))
    ;; If no allowed values for BEAMER_col have been defined,
    ;; supply some
    '("0.1" "0.2" "0.3" "0.4" "0.5" "0.6" "0.7" "0.8" "0.9" "" ":ETC"))
   (t nil)))

(add-hook 'org-property-allowed-value-functions
	  'org-beamer-allowed-property-values)

(provide 'org-beamer)

;;; org-beamer.el ends here
