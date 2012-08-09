;;; org-html.el --- HTML export for Org-mode

;; Copyright (C) 2004-2012 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten at orgmode dot org>
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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;;; Code:

(require 'org-exp)
(require 'format-spec)

(eval-when-compile (require 'cl))

(declare-function org-id-find-id-file "org-id" (id))
(declare-function htmlize-region "ext:htmlize" (beg end))
(declare-function org-pop-to-buffer-same-window
		  "org-compat" (&optional buffer-or-name norecord label))

(defgroup org-export-html nil
  "Options specific for HTML export of Org-mode files."
  :tag "Org Export HTML"
  :group 'org-export)

(defcustom org-export-html-footnotes-section "<div id=\"footnotes\">
<h2 class=\"footnotes\">%s: </h2>
<div id=\"text-footnotes\">
%s
</div>
</div>"
  "Format for the footnotes section.
Should contain a two instances of %s.  The first will be replaced with the
language-specific word for \"Footnotes\", the second one will be replaced
by the footnotes themselves."
  :group 'org-export-html
  :type 'string)

(defcustom org-export-html-footnote-format "<sup>%s</sup>"
  "The format for the footnote reference.
%s will be replaced by the footnote reference itself."
  :group 'org-export-html
  :type 'string)


(defcustom org-export-html-footnote-separator "<sup>, </sup>"
  "Text used to separate footnotes."
  :group 'org-export-html
  :version "24.1"
  :type 'string)

(defcustom org-export-html-coding-system nil
  "Coding system for HTML export, defaults to `buffer-file-coding-system'."
  :group 'org-export-html
  :type 'coding-system)

(defcustom org-export-html-extension "html"
  "The extension for exported HTML files."
  :group 'org-export-html
  :type 'string)

(defcustom org-export-html-xml-declaration
  '(("html" . "<?xml version=\"1.0\" encoding=\"%s\"?>")
    ("php" . "<?php echo \"<?xml version=\\\"1.0\\\" encoding=\\\"%s\\\" ?>\"; ?>"))
  "The extension for exported HTML files.
%s will be replaced with the charset of the exported file.
This may be a string, or an alist with export extensions
and corresponding declarations."
  :group 'org-export-html
  :type '(choice
	  (string :tag "Single declaration")
	  (repeat :tag "Dependent on extension"
		  (cons (string :tag "Extension")
			(string :tag "Declaration")))))

(defcustom org-export-html-style-include-scripts t
  "Non-nil means include the JavaScript snippets in exported HTML files.
The actual script is defined in `org-export-html-scripts' and should
not be modified."
  :group 'org-export-html
  :type 'boolean)

(defconst org-export-html-scripts
"<script type=\"text/javascript\">
<!--/*--><![CDATA[/*><!--*/
 function CodeHighlightOn(elem, id)
 {
   var target = document.getElementById(id);
   if(null != target) {
     elem.cacheClassElem = elem.className;
     elem.cacheClassTarget = target.className;
     target.className = \"code-highlighted\";
     elem.className   = \"code-highlighted\";
   }
 }
 function CodeHighlightOff(elem, id)
 {
   var target = document.getElementById(id);
   if(elem.cacheClassElem)
     elem.className = elem.cacheClassElem;
   if(elem.cacheClassTarget)
     target.className = elem.cacheClassTarget;
 }
/*]]>*///-->
</script>"
"Basic JavaScript that is needed by HTML files produced by Org-mode.")

(defconst org-export-html-style-default
"<style type=\"text/css\">
 <!--/*--><![CDATA[/*><!--*/
  html { font-family: Times, serif; font-size: 12pt; }
  .title  { text-align: center; }
  .todo   { color: red; }
  .done   { color: green; }
  .tag    { background-color: #add8e6; font-weight:normal }
  .target { }
  .timestamp { color: #bebebe; }
  .timestamp-kwd { color: #5f9ea0; }
  .right  {margin-left:auto; margin-right:0px;  text-align:right;}
  .left   {margin-left:0px;  margin-right:auto; text-align:left;}
  .center {margin-left:auto; margin-right:auto; text-align:center;}
  p.verse { margin-left: 3% }
  pre {
	border: 1pt solid #AEBDCC;
	background-color: #F3F5F7;
	padding: 5pt;
	font-family: courier, monospace;
        font-size: 90%;
        overflow:auto;
  }
  table { border-collapse: collapse; }
  td, th { vertical-align: top;  }
  th.right  { text-align:center;  }
  th.left   { text-align:center;   }
  th.center { text-align:center; }
  td.right  { text-align:right;  }
  td.left   { text-align:left;   }
  td.center { text-align:center; }
  dt { font-weight: bold; }
  div.figure { padding: 0.5em; }
  div.figure p { text-align: center; }
  div.inlinetask {
    padding:10px;
    border:2px solid gray;
    margin:10px;
    background: #ffffcc;
  }
  textarea { overflow-x: auto; }
  .linenr { font-size:smaller }
  .code-highlighted {background-color:#ffff00;}
  .org-info-js_info-navigation { border-style:none; }
  #org-info-js_console-label { font-size:10px; font-weight:bold;
                               white-space:nowrap; }
  .org-info-js_search-highlight {background-color:#ffff00; color:#000000;
                                 font-weight:bold; }
  /*]]>*/-->
</style>"
  "The default style specification for exported HTML files.
Please use the variables `org-export-html-style' and
`org-export-html-style-extra' to add to this style.  If you wish to not
have the default style included, customize the variable
`org-export-html-style-include-default'.")

(defcustom org-export-html-style-include-default t
  "Non-nil means include the default style in exported HTML files.
The actual style is defined in `org-export-html-style-default' and should
not be modified.  Use the variables `org-export-html-style' to add
your own style information."
  :group 'org-export-html
  :type 'boolean)
;;;###autoload
(put 'org-export-html-style-include-default 'safe-local-variable 'booleanp)

(defcustom org-export-html-style ""
  "Org-wide style definitions for exported HTML files.

This variable needs to contain the full HTML structure to provide a style,
including the surrounding HTML tags.  If you set the value of this variable,
you should consider to include definitions for the following classes:
 title, todo, done, timestamp, timestamp-kwd, tag, target.

For example, a valid value would be:

   <style type=\"text/css\">
    <![CDATA[
       p { font-weight: normal; color: gray; }
       h1 { color: black; }
      .title { text-align: center; }
      .todo, .timestamp-kwd { color: red; }
      .done { color: green; }
    ]]>
   </style>

If you'd like to refer to an external style file, use something like

   <link rel=\"stylesheet\" type=\"text/css\" href=\"mystyles.css\">

As the value of this option simply gets inserted into the HTML <head> header,
you can \"misuse\" it to add arbitrary text to the header.
See also the variable `org-export-html-style-extra'."
  :group 'org-export-html
  :type 'string)
;;;###autoload
(put 'org-export-html-style 'safe-local-variable 'stringp)

(defcustom org-export-html-style-extra ""
  "Additional style information for HTML export.
The value of this variable is inserted into the HTML buffer right after
the value of `org-export-html-style'.  Use this variable for per-file
settings of style information, and do not forget to surround the style
settings with <style>...</style> tags."
  :group 'org-export-html
  :type 'string)
;;;###autoload
(put 'org-export-html-style-extra 'safe-local-variable 'stringp)

(defcustom org-export-html-mathjax-options
  '((path  "http://orgmode.org/mathjax/MathJax.js")
    (scale "100")
    (align "center")
    (indent "2em")
    (mathml nil))
  "Options for MathJax setup.

path        The path where to find MathJax
scale       Scaling for the HTML-CSS backend, usually between 100 and 133
align       How to align display math: left, center, or right
indent      If align is not center, how far from the left/right side?
mathml      Should a MathML player be used if available?
            This is faster and reduces bandwidth use, but currently
            sometimes has lower spacing quality.  Therefore, the default is
            nil.  When browsers get better, this switch can be flipped.

You can also customize this for each buffer, using something like

#+MATHJAX: scale:\"133\" align:\"right\" mathml:t path:\"/MathJax/\""
  :group 'org-export-html
  :version "24.1"
  :type '(list :greedy t
	      (list :tag "path   (the path from where to load MathJax.js)"
		    (const :format "       " path) (string))
	      (list :tag "scale  (scaling for the displayed math)"
		    (const :format "       " scale) (string))
	      (list :tag "align  (alignment of displayed equations)"
		    (const :format "       " align) (string))
	      (list :tag "indent (indentation with left or right alignment)"
		    (const :format "       " indent) (string))
	      (list :tag "mathml (should MathML display be used is possible)"
		    (const :format "       " mathml) (boolean))))

(defun org-export-html-mathjax-config (template options in-buffer)
  "Insert the user setup into the matchjax template."
  (let (name val (yes "   ") (no "// ") x)
    (mapc
     (lambda (e)
       (setq name (car e) val (nth 1 e))
       (if (string-match (concat "\\<" (symbol-name name) ":") in-buffer)
	   (setq val (car (read-from-string
			   (substring in-buffer (match-end 0))))))
       (if (not (stringp val)) (setq val (format "%s" val)))
       (if (string-match (concat "%" (upcase (symbol-name name))) template)
	   (setq template (replace-match val t t template))))
     options)
    (setq val (nth 1 (assq 'mathml options)))
    (if (string-match (concat "\\<mathml:") in-buffer)
	(setq val (car (read-from-string
			(substring in-buffer (match-end 0))))))
    ;; Exchange prefixes depending on mathml setting
    (if (not val) (setq x yes yes no no x))
    ;; Replace cookies to turn on or off the config/jax lines
    (if (string-match ":MMLYES:" template)
	(setq template (replace-match yes t t template)))
    (if (string-match ":MMLNO:" template)
	(setq template (replace-match no t t template)))
    ;; Return the modified template
    template))

(defcustom org-export-html-mathjax-template
  "<script type=\"text/javascript\" src=\"%PATH\">
<!--/*--><![CDATA[/*><!--*/
    MathJax.Hub.Config({
        // Only one of the two following lines, depending on user settings
        // First allows browser-native MathML display, second forces HTML/CSS
        :MMLYES: config: [\"MMLorHTML.js\"], jax: [\"input/TeX\"],
        :MMLNO: jax: [\"input/TeX\", \"output/HTML-CSS\"],
        extensions: [\"tex2jax.js\",\"TeX/AMSmath.js\",\"TeX/AMSsymbols.js\",
                     \"TeX/noUndefined.js\"],
        tex2jax: {
            inlineMath: [ [\"\\\\(\",\"\\\\)\"] ],
            displayMath: [ ['$$','$$'], [\"\\\\[\",\"\\\\]\"], [\"\\\\begin{displaymath}\",\"\\\\end{displaymath}\"] ],
            skipTags: [\"script\",\"noscript\",\"style\",\"textarea\",\"pre\",\"code\"],
            ignoreClass: \"tex2jax_ignore\",
            processEscapes: false,
            processEnvironments: true,
            preview: \"TeX\"
        },
        showProcessingMessages: true,
        displayAlign: \"%ALIGN\",
        displayIndent: \"%INDENT\",

        \"HTML-CSS\": {
             scale: %SCALE,
             availableFonts: [\"STIX\",\"TeX\"],
             preferredFont: \"TeX\",
             webFont: \"TeX\",
             imageFont: \"TeX\",
             showMathMenu: true,
        },
        MMLorHTML: {
             prefer: {
                 MSIE:    \"MML\",
                 Firefox: \"MML\",
                 Opera:   \"HTML\",
                 other:   \"HTML\"
             }
        }
    });
/*]]>*///-->
</script>"
  "The MathJax setup for XHTML files."
  :group 'org-export-html
  :version "24.1"
  :type 'string)

(defcustom org-export-html-tag-class-prefix ""
  "Prefix to class names for TODO keywords.
Each tag gets a class given by the tag itself, with this prefix.
The default prefix is empty because it is nice to just use the keyword
as a class name.  But if you get into conflicts with other, existing
CSS classes, then this prefix can be very useful."
  :group 'org-export-html
  :type 'string)

(defcustom org-export-html-todo-kwd-class-prefix ""
  "Prefix to class names for TODO keywords.
Each TODO keyword gets a class given by the keyword itself, with this prefix.
The default prefix is empty because it is nice to just use the keyword
as a class name.  But if you get into conflicts with other, existing
CSS classes, then this prefix can be very useful."
  :group 'org-export-html
  :type 'string)

(defcustom org-export-html-headline-anchor-format "<a name=\"%s\" id=\"%s\"></a>"
  "Format for anchors in HTML headlines.
It requires to %s: both will be replaced by the anchor referring
to the headline (e.g. \"sec-2\").  When set to `nil', don't insert
HTML anchors in headlines."
  :group 'org-export-html
  :version "24.1"
  :type 'string)

(defcustom org-export-html-preamble t
  "Non-nil means insert a preamble in HTML export.

When `t', insert a string as defined by one of the formatting
strings in `org-export-html-preamble-format'.  When set to a
string, this string overrides `org-export-html-preamble-format'.
When set to a function, apply this function and insert the
returned string.  The function takes no argument, but you can
use `opt-plist' to access the current export options.

Setting :html-preamble in publishing projects will take
precedence over this variable."
  :group 'org-export-html
  :type '(choice (const :tag "No preamble" nil)
		 (const :tag "Default preamble" t)
		 (string :tag "Custom formatting string")
		 (function :tag "Function (must return a string)")))

(defcustom org-export-html-preamble-format '(("en" ""))
  "The format for the HTML preamble.

%t stands for the title.
%a stands for the author's name.
%e stands for the author's email.
%d stands for the date.

If you need to use a \"%\" character, you need to escape it
like that: \"%%\"."
  :group 'org-export-html
  :version "24.1"
  :type 'string)

(defcustom org-export-html-postamble 'auto
  "Non-nil means insert a postamble in HTML export.

When `t', insert a string as defined by the formatting string in
`org-export-html-postamble-format'.  When set to a string, this
string overrides `org-export-html-postamble-format'.  When set to
'auto, discard `org-export-html-postamble-format' and honor
`org-export-author/email/creator-info' variables.  When set to a
function, apply this function and insert the returned string.
The function takes no argument, but you can use `opt-plist' to
access the current export options.

Setting :html-postamble in publishing projects will take
precedence over this variable."
  :group 'org-export-html
  :type '(choice (const :tag "No postamble" nil)
		 (const :tag "Auto preamble" 'auto)
		 (const :tag "Default formatting string" t)
		 (string :tag "Custom formatting string")
		 (function :tag "Function (must return a string)")))

(defcustom org-export-html-postamble-format
  '(("en" "<p class=\"author\">Author: %a (%e)</p>
<p class=\"date\">Date: %d</p>
<p class=\"creator\">Generated by %c</p>
<p class=\"xhtml-validation\">%v</p>
"))
  "The format for the HTML postamble.

%a stands for the author's name.
%e stands for the author's email.
%d stands for the date.
%c will be replaced by information about Org/Emacs versions.
%v will be replaced by `org-export-html-validation-link'.

If you need to use a \"%\" character, you need to escape it
like that: \"%%\"."
  :group 'org-export-html
  :version "24.1"
  :type 'string)

(defcustom org-export-html-home/up-format
  "<div id=\"org-div-home-and-up\" style=\"text-align:right;font-size:70%%;white-space:nowrap;\">
 <a accesskey=\"h\" href=\"%s\"> UP </a>
 |
 <a accesskey=\"H\" href=\"%s\"> HOME </a>
</div>"
  "Snippet used to insert the HOME and UP links.
This is a format string, the first %s will receive the UP link,
the second the HOME link.  If both `org-export-html-link-up' and
`org-export-html-link-home' are empty, the entire snippet will be
ignored."
  :group 'org-export-html
  :type 'string)

(defcustom org-export-html-toplevel-hlevel 2
  "The <H> level for level 1 headings in HTML export.
This is also important for the classes that will be wrapped around headlines
and outline structure.  If this variable is 1, the top-level headlines will
be <h1>, and the corresponding classes will be outline-1, section-number-1,
and outline-text-1.  If this is 2, all of these will get a 2 instead.
The default for this variable is 2, because we use <h1> for formatting the
document title."
  :group 'org-export-html
  :type 'string)

(defcustom org-export-html-link-org-files-as-html t
  "Non-nil means make file links to `file.org' point to `file.html'.
When org-mode is exporting an org-mode file to HTML, links to
non-html files are directly put into a href tag in HTML.
However, links to other Org-mode files (recognized by the
extension `.org.) should become links to the corresponding html
file, assuming that the linked org-mode file will also be
converted to HTML.
When nil, the links still point to the plain `.org' file."
  :group 'org-export-html
  :type 'boolean)

(defcustom org-export-html-inline-images 'maybe
  "Non-nil means inline images into exported HTML pages.
This is done using an <img> tag.  When nil, an anchor with href is used to
link to the image.  If this option is `maybe', then images in links with
an empty description will be inlined, while images with a description will
be linked only."
  :group 'org-export-html
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Always" t)
		 (const :tag "When there is no description" maybe)))

(defcustom org-export-html-inline-image-extensions
  '("png" "jpeg" "jpg" "gif" "svg")
  "Extensions of image files that can be inlined into HTML."
  :group 'org-export-html
  :type '(repeat (string :tag "Extension")))

(defcustom org-export-html-table-tag
  "<table border=\"2\" cellspacing=\"0\" cellpadding=\"6\" rules=\"groups\" frame=\"hsides\">"
  "The HTML tag that is used to start a table.
This must be a <table> tag, but you may change the options like
borders and spacing."
  :group 'org-export-html
  :type 'string)

(defcustom org-export-table-header-tags '("<th scope=\"%s\"%s>" . "</th>")
  "The opening tag for table header fields.
This is customizable so that alignment options can be specified.
The first %s will be filled with the scope of the field, either row or col.
The second %s will be replaced by a style entry to align the field.
See also the variable `org-export-html-table-use-header-tags-for-first-column'.
See also the variable `org-export-html-table-align-individual-fields'."
  :group 'org-export-tables
  :type '(cons (string :tag "Opening tag") (string :tag "Closing tag")))

(defcustom org-export-table-data-tags '("<td%s>" . "</td>")
  "The opening tag for table data fields.
This is customizable so that alignment options can be specified.
The first %s will be filled with the scope of the field, either row or col.
The second %s will be replaced by a style entry to align the field.
See also the variable `org-export-html-table-align-individual-fields'."
  :group 'org-export-tables
  :type '(cons (string :tag "Opening tag") (string :tag "Closing tag")))

(defcustom org-export-table-row-tags '("<tr>" . "</tr>")
  "The opening tag for table data fields.
This is customizable so that alignment options can be specified.
Instead of strings, these can be Lisp forms that will be evaluated
for each row in order to construct the table row tags.  During evaluation,
the variable `head' will be true when this is a header line, nil when this
is a body line.  And the variable `nline' will contain the line number,
starting from 1 in the first header line.  For example

  (setq org-export-table-row-tags
        (cons '(if head
                   \"<tr>\"
                 (if (= (mod nline 2) 1)
                     \"<tr class=\\\"tr-odd\\\">\"
                   \"<tr class=\\\"tr-even\\\">\"))
              \"</tr>\"))

will give even lines the class \"tr-even\" and odd lines the class \"tr-odd\"."
  :group 'org-export-tables
  :type '(cons
	  (choice :tag "Opening tag"
		  (string :tag "Specify")
		  (sexp))
	  (choice :tag "Closing tag"
		  (string :tag "Specify")
		  (sexp))))

(defcustom org-export-html-table-align-individual-fields t
  "Non-nil means attach style attributes for alignment to each table field.
When nil, alignment will only be specified in the column tags, but this
is ignored by some browsers (like Firefox, Safari).  Opera does it right
though."
  :group 'org-export-tables
  :version "24.1"
  :type 'boolean)

(defcustom org-export-html-table-use-header-tags-for-first-column nil
  "Non-nil means format column one in tables with header tags.
When nil, also column one will use data tags."
  :group 'org-export-tables
  :type 'boolean)

(defcustom org-export-html-validation-link
  "<a href=\"http://validator.w3.org/check?uri=referer\">Validate XHTML 1.0</a>"
  "Link to HTML validation service."
  :group 'org-export-html
  :type 'string)

;; FIXME Obsolete since Org 7.7
;; Use the :timestamp option or `org-export-time-stamp-file' instead
(defvar org-export-html-with-timestamp nil
  "If non-nil, write container for HTML-helper-mode timestamp.")

;; FIXME Obsolete since Org 7.7
(defvar org-export-html-html-helper-timestamp
  "\n<p><br/><br/>\n<!-- hhmts start --> <!-- hhmts end --></p>\n"
  "The HTML tag used as timestamp delimiter for HTML-helper-mode.")

(defcustom org-export-html-protect-char-alist
  '(("&" . "&amp;")
    ("<" . "&lt;")
    (">" . "&gt;"))
  "Alist of characters to be converted by `org-html-protect'."
  :group 'org-export-html
  :version "24.1"
  :type '(repeat (cons (string :tag "Character")
		       (string :tag "HTML equivalent"))))

(defgroup org-export-htmlize nil
  "Options for processing examples with htmlize.el."
  :tag "Org Export Htmlize"
  :group 'org-export-html)

(defcustom org-export-htmlize-output-type 'inline-css
  "Output type to be used by htmlize when formatting code snippets.
Choices are `css', to export the CSS selectors only, or `inline-css', to
export the CSS attribute values inline in the HTML.  We use as default
`inline-css', in order to make the resulting HTML self-containing.

However, this will fail when using Emacs in batch mode for export, because
then no rich font definitions are in place.  It will also not be good if
people with different Emacs setup contribute HTML files to a website,
because the fonts will represent the individual setups.  In these cases,
it is much better to let Org/Htmlize assign classes only, and to use
a style file to define the look of these classes.
To get a start for your css file, start Emacs session and make sure that
all the faces you are interested in are defined, for example by loading files
in all modes you want.  Then, use the command
\\[org-export-htmlize-generate-css] to extract class definitions."
  :group 'org-export-htmlize
  :type '(choice (const css) (const inline-css)))

(defcustom org-export-htmlize-css-font-prefix "org-"
  "The prefix for CSS class names for htmlize font specifications."
  :group 'org-export-htmlize
  :type 'string)

(defcustom org-export-htmlized-org-css-url nil
  "URL pointing to a CSS file defining text colors for htmlized Emacs buffers.
Normally when creating an htmlized version of an Org buffer, htmlize will
create CSS to define the font colors.  However, this does not work when
converting in batch mode, and it also can look bad if different people
with different fontification setup work on the same website.
When this variable is non-nil, creating an htmlized version of an Org buffer
using `org-export-as-org' will remove the internal CSS section and replace it
with a link to this URL."
  :group 'org-export-htmlize
  :type '(choice
	  (const :tag "Keep internal css" nil)
	  (string :tag "URL or local href")))

;; FIXME: The following variable is obsolete since Org 7.7 but is
;; still declared and checked within code for compatibility reasons.
;; Use the custom variables `org-export-html-divs' instead.
(defvar org-export-html-content-div "content"
  "The name of the container DIV that holds all the page contents.

This variable is obsolete since Org version 7.7.
Please set `org-export-html-divs' instead.")

(defcustom org-export-html-divs '("preamble" "content" "postamble")
  "The name of the main divs for HTML export.
This is a list of three strings, the first one for the preamble
DIV, the second one for the content DIV and the third one for the
postamble DIV."
  :group 'org-export-html
  :version "24.1"
  :type '(list
	  (string :tag " Div for the preamble:")
	  (string :tag "  Div for the content:")
	  (string :tag "Div for the postamble:")))

;;; Hooks

(defvar org-export-html-after-blockquotes-hook nil
  "Hook run during HTML export, after blockquote, verse, center are done.")

(defvar org-export-html-final-hook nil
  "Hook run at the end of HTML export, in the new buffer.")

;;; HTML export

(defun org-export-html-preprocess (parameters)
  "Convert LaTeX fragments to images."
  (when (and org-current-export-file
	     (plist-get parameters :LaTeX-fragments))
    (org-format-latex
     (concat "ltxpng/" (file-name-sans-extension
			(file-name-nondirectory
			 org-current-export-file)))
     org-current-export-dir nil "Creating LaTeX image %s"
     nil nil
     (cond
      ((eq (plist-get parameters :LaTeX-fragments) 'verbatim) 'verbatim)
      ((eq (plist-get parameters :LaTeX-fragments) 'mathjax ) 'mathjax)
      ((eq (plist-get parameters :LaTeX-fragments) t        ) 'mathjax)
      ((eq (plist-get parameters :LaTeX-fragments) 'dvipng  ) 'dvipng)
      (t nil))))
  (goto-char (point-min))
  (let (label l1)
    (while (re-search-forward "\\\\ref{\\([^{}\n]+\\)}" nil t)
      (org-if-unprotected-at (match-beginning 1)
	(setq label (match-string 1))
	(save-match-data
	  (if (string-match "\\`[a-z]\\{1,10\\}:\\(.+\\)" label)
	      (setq l1 (substring label (match-beginning 1)))
	    (setq l1 label)))
	(replace-match (format "[[#%s][%s]]" label l1) t t)))))

;;;###autoload
(defun org-export-as-html-and-open (arg)
  "Export the outline as HTML and immediately open it with a browser.
If there is an active region, export only the region.
The prefix ARG specifies how many levels of the outline should become
headlines.  The default is 3.  Lower levels will become bulleted lists."
  (interactive "P")
  (org-export-as-html arg 'hidden)
  (org-open-file buffer-file-name)
  (when org-export-kill-product-buffer-when-displayed
    (kill-buffer (current-buffer))))

;;;###autoload
(defun org-export-as-html-batch ()
  "Call the function `org-export-as-html'.
This function can be used in batch processing as:
emacs   --batch
        --load=$HOME/lib/emacs/org.el
        --eval \"(setq org-export-headline-levels 2)\"
        --visit=MyFile --funcall org-export-as-html-batch"
  (org-export-as-html org-export-headline-levels 'hidden))

;;;###autoload
(defun org-export-as-html-to-buffer (arg)
  "Call `org-export-as-html` with output to a temporary buffer.
No file is created.  The prefix ARG is passed through to `org-export-as-html'."
  (interactive "P")
  (org-export-as-html arg nil nil "*Org HTML Export*")
  (when org-export-show-temporary-export-buffer
    (switch-to-buffer-other-window "*Org HTML Export*")))

;;;###autoload
(defun org-replace-region-by-html (beg end)
  "Assume the current region has org-mode syntax, and convert it to HTML.
This can be used in any buffer.  For example, you could write an
itemized list in org-mode syntax in an HTML buffer and then use this
command to convert it."
  (interactive "r")
  (let (reg html buf pop-up-frames)
    (save-window-excursion
      (if (eq major-mode 'org-mode)
	  (setq html (org-export-region-as-html
		      beg end t 'string))
	(setq reg (buffer-substring beg end)
	      buf (get-buffer-create "*Org tmp*"))
	(with-current-buffer buf
	  (erase-buffer)
	  (insert reg)
	  (org-mode)
	  (setq html (org-export-region-as-html
		      (point-min) (point-max) t 'string)))
	(kill-buffer buf)))
    (delete-region beg end)
    (insert html)))

;;;###autoload
(defun org-export-region-as-html (beg end &optional body-only buffer)
  "Convert region from BEG to END in org-mode buffer to HTML.
If prefix arg BODY-ONLY is set, omit file header, footer, and table of
contents, and only produce the region of converted text, useful for
cut-and-paste operations.
If BUFFER is a buffer or a string, use/create that buffer as a target
of the converted HTML.  If BUFFER is the symbol `string', return the
produced HTML as a string and leave not buffer behind.  For example,
a Lisp program could call this function in the following way:

  (setq html (org-export-region-as-html beg end t 'string))

When called interactively, the output buffer is selected, and shown
in a window.  A non-interactive call will only return the buffer."
  (interactive "r\nP")
  (when (org-called-interactively-p 'any)
    (setq buffer "*Org HTML Export*"))
  (let ((transient-mark-mode t) (zmacs-regions t)
	ext-plist rtn)
    (setq ext-plist (plist-put ext-plist :ignore-subtree-p t))
    (goto-char end)
    (set-mark (point)) ;; to activate the region
    (goto-char beg)
    (setq rtn (org-export-as-html
	       nil nil ext-plist
	       buffer body-only))
    (if (fboundp 'deactivate-mark) (deactivate-mark))
    (if (and (org-called-interactively-p 'any) (bufferp rtn))
	(switch-to-buffer-other-window rtn)
      rtn)))

(defvar html-table-tag nil) ; dynamically scoped into this.
(defvar org-par-open nil)

;;; org-html-cvt-link-fn
(defconst org-html-cvt-link-fn
   nil
   "Function to convert link URLs to exportable URLs.
Takes two arguments, TYPE and PATH.
Returns exportable url as (TYPE PATH), or nil to signal that it
didn't handle this case.
Intended to be locally bound around a call to `org-export-as-html'." )

(defun org-html-cvt-org-as-html (opt-plist type path)
   "Convert an org filename to an equivalent html filename.
If TYPE is not file, just return `nil'.
See variable `org-export-html-link-org-files-as-html'"

   (save-match-data
      (and
	 org-export-html-link-org-files-as-html
	 (string= type "file")
	 (string-match "\\.org$" path)
	 (progn
	    (list
	       "file"
	       (concat
		  (substring path 0 (match-beginning 0))
		  "."
		  (plist-get opt-plist :html-extension)))))))


;;; org-html-should-inline-p
(defun org-html-should-inline-p (filename descp)
   "Return non-nil if link FILENAME should be inlined.
The decision to inline the FILENAME link is based on the current
settings.  DESCP is the boolean of whether there was a link
description.  See variables `org-export-html-inline-images' and
`org-export-html-inline-image-extensions'."
   (declare (special
	     org-export-html-inline-images
	     org-export-html-inline-image-extensions))
   (and (or (eq t org-export-html-inline-images)
	    (and org-export-html-inline-images (not descp)))
	(org-file-image-p
	 filename org-export-html-inline-image-extensions)))

;;; org-html-make-link
(defun org-html-make-link (opt-plist type path fragment desc attr
			     may-inline-p)
   "Make an HTML link.
OPT-PLIST is an options list.
TYPE is the device-type of the link (THIS://foo.html).
PATH is the path of the link (http://THIS#location).
FRAGMENT is the fragment part of the link, if any (foo.html#THIS).
DESC is the link description, if any.
ATTR is a string of other attributes of the \"a\" element.
MAY-INLINE-P allows inlining it as an image."

   (declare (special org-par-open))
   (save-match-data
      (let* ((filename path)
	       ;;First pass.  Just sanity stuff.
	       (components-1
		  (cond
		     ((string= type "file")
			(list
			   type
			   ;;Substitute just if original path was absolute.
			   ;;(Otherwise path must remain relative)
			   (if (file-name-absolute-p path)
			      (concat "file://" (expand-file-name path))
			      path)))
		     ((string= type "")
			(list nil path))
		     (t (list type path))))

	       ;;Second pass.  Components converted so they can refer
	       ;;to a remote site.
	       (components-2
		  (or
		     (and org-html-cvt-link-fn
			(apply org-html-cvt-link-fn
			   opt-plist components-1))
		     (apply #'org-html-cvt-org-as-html
			opt-plist components-1)
		     components-1))
	       (type    (first  components-2))
	       (thefile (second components-2)))


	 ;;Third pass.  Build final link except for leading type
	 ;;spec.
	 (cond
	    ((or
		(not type)
		(string= type "http")
		(string= type "https")
		(string= type "file")
		(string= type "coderef"))
	       (if fragment
		  (setq thefile (concat thefile "#" fragment))))

	    (t))

	 ;;Final URL-build, for all types.
	 (setq thefile
	    (let
	       ((str (org-export-html-format-href thefile)))
	      (if (and type (not (or (string= "file" type)
				     (string= "coderef" type))))
		  (concat type ":" str)
		  str)))

	 (if (and
		may-inline-p
		;;Can't inline a URL with a fragment.
		(not fragment))
	    (progn
	       (message "image %s %s" thefile org-par-open)
	       (org-export-html-format-image thefile org-par-open))
	    (concat
	       "<a href=\"" thefile "\"" (if attr (concat " " attr)) ">"
	       (org-export-html-format-desc desc)
	       "</a>")))))

(defun org-html-handle-links (line opt-plist)
  "Return LINE with markup of Org mode links.
OPT-PLIST is the export options list."
  (let ((start 0)
	(current-dir (if buffer-file-name
			  (file-name-directory buffer-file-name)
			default-directory))
	(link-validate (plist-get opt-plist :link-validation-function))
	type id-file fnc
	rpl path attr desc descp desc1 desc2 link)
    (while (string-match org-bracket-link-analytic-regexp++ line start)
      (setq start (match-beginning 0))
      (setq path (save-match-data (org-link-unescape
				   (match-string 3 line))))
      (setq type (cond
		  ((match-end 2) (match-string 2 line))
		  ((save-match-data
		     (or (file-name-absolute-p path)
			 (string-match "^\\.\\.?/" path)))
		   "file")
		  (t "internal")))
      (setq path (org-extract-attributes path))
      (setq attr (get-text-property 0 'org-attributes path))
      (setq desc1 (if (match-end 5) (match-string 5 line))
	    desc2 (if (match-end 2) (concat type ":" path) path)
	    descp (and desc1 (not (equal desc1 desc2)))
	    desc (or desc1 desc2))
      ;; Make an image out of the description if that is so wanted
      (when (and descp (org-file-image-p
			desc org-export-html-inline-image-extensions))
	(save-match-data
	  (if (string-match "^file:" desc)
	      (setq desc (substring desc (match-end 0)))))
	(setq desc (org-add-props
		       (concat "<img src=\"" desc "\" alt=\""
			       (file-name-nondirectory desc) "\"/>")
		       '(org-protected t))))
      (cond
       ((equal type "internal")
	(let
	    ((frag-0
	      (if (= (string-to-char path) ?#)
		  (substring path 1)
		path)))
	  (setq rpl
		(org-html-make-link
		 opt-plist
		 ""
		 ""
		 (org-solidify-link-text
		  (save-match-data (org-link-unescape frag-0))
		  nil)
		 desc attr nil))))
       ((and (equal type "id")
	     (setq id-file (org-id-find-id-file path)))
	;; This is an id: link to another file (if it was the same file,
	;; it would have become an internal link...)
	(save-match-data
	  (setq id-file (file-relative-name
			 id-file
			 (file-name-directory org-current-export-file)))
	  (setq rpl
		(org-html-make-link opt-plist
				    "file" id-file
				    (concat (if (org-uuidgen-p path) "ID-") path)
				    desc
				    attr
				    nil))))
       ((member type '("http" "https"))
	;; standard URL, can inline as image
	(setq rpl
	      (org-html-make-link opt-plist
				  type path nil
				  desc
				  attr
				  (org-html-should-inline-p path descp))))
       ((member type '("ftp" "mailto" "news"))
	;; standard URL, can't inline as image
	(setq rpl
	      (org-html-make-link opt-plist
				  type path nil
				  desc
				  attr
				  nil)))

       ((string= type "coderef")
	(let*
	    ((coderef-str (format "coderef-%s" path))
	     (attr-1
	      (format "class=\"coderef\" onmouseover=\"CodeHighlightOn(this, '%s');\" onmouseout=\"CodeHighlightOff(this, '%s');\""
		      coderef-str coderef-str)))
	  (setq rpl
		(org-html-make-link opt-plist
				    type "" coderef-str
				    (format
				     (org-export-get-coderef-format
				      path
				      (and descp desc))
				     (cdr (assoc path org-export-code-refs)))
				    attr-1
				    nil))))

       ((functionp (setq fnc (nth 2 (assoc type org-link-protocols))))
	;; The link protocol has a function for format the link
	(setq rpl
	      (save-match-data
		(funcall fnc (org-link-unescape path) desc1 'html))))

       ((string= type "file")
	;; FILE link
	(save-match-data
	  (let*
	      ((components
		(if
		    (string-match "::\\(.*\\)" path)
		    (list
		     (replace-match "" t nil path)
		     (match-string 1 path))
		  (list path nil)))

	       ;;The proper path, without a fragment
	       (path-1
		(first components))

	       ;;The raw fragment
	       (fragment-0
		(second components))

	       ;;Check the fragment.  If it can't be used as
	       ;;target fragment we'll pass nil instead.
	       (fragment-1
		(if
		    (and fragment-0
			 (not (string-match "^[0-9]*$" fragment-0))
			 (not (string-match "^\\*" fragment-0))
			 (not (string-match "^/.*/$" fragment-0)))
		    (org-solidify-link-text
		     (org-link-unescape fragment-0))
		  nil))
	       (desc-2
		;;Description minus "file:" and ".org"
		(if (string-match "^file:" desc)
		    (let
			((desc-1 (replace-match "" t t desc)))
		      (if (string-match "\\.org$" desc-1)
			  (replace-match "" t t desc-1)
			desc-1))
		  desc)))

	    (setq rpl
		  (if
		      (and
		       (functionp link-validate)
		       (not (funcall link-validate path-1 current-dir)))
		      desc
		    (org-html-make-link opt-plist
					"file" path-1 fragment-1 desc-2 attr
					(org-html-should-inline-p path-1 descp)))))))

       (t
	;; just publish the path, as default
	(setq rpl (concat "<i>&lt;" type ":"
			  (save-match-data (org-link-unescape path))
			  "&gt;</i>"))))
      (setq line (replace-match rpl t t line)
	    start (+ start (length rpl))))
    line))

;;; org-export-as-html

(defvar org-heading-keyword-regexp-format) ; defined in org.el

;;;###autoload
(defun org-export-as-html (arg &optional hidden ext-plist
			       to-buffer body-only pub-dir)
  "Export the outline as a pretty HTML file.
If there is an active region, export only the region.  The prefix
ARG specifies how many levels of the outline should become
headlines.  The default is 3.  Lower levels will become bulleted
lists.  HIDDEN is obsolete and does nothing.
EXT-PLIST is a property list with external parameters overriding
org-mode's default settings, but still inferior to file-local
settings.  When TO-BUFFER is non-nil, create a buffer with that
name and export to that buffer.  If TO-BUFFER is the symbol
`string', don't leave any buffer behind but just return the
resulting HTML as a string.  When BODY-ONLY is set, don't produce
the file header and footer, simply return the content of
<body>...</body>, without even the body tags themselves.  When
PUB-DIR is set, use this as the publishing directory."
  (interactive "P")
  (run-hooks 'org-export-first-hook)

  ;; Make sure we have a file name when we need it.
  (when (and (not (or to-buffer body-only))
	     (not buffer-file-name))
    (if (buffer-base-buffer)
	(org-set-local 'buffer-file-name
		       (with-current-buffer (buffer-base-buffer)
			 buffer-file-name))
      (error "Need a file name to be able to export")))

  (message "Exporting...")
  (setq-default org-todo-line-regexp org-todo-line-regexp)
  (setq-default org-deadline-line-regexp org-deadline-line-regexp)
  (setq-default org-done-keywords org-done-keywords)
  (setq-default org-maybe-keyword-time-regexp org-maybe-keyword-time-regexp)
  (let* ((opt-plist
	  (org-export-process-option-filters
	   (org-combine-plists (org-default-export-plist)
			       ext-plist
			       (org-infile-export-plist))))
	 (body-only (or body-only (plist-get opt-plist :body-only)))
	 (style (concat (if (plist-get opt-plist :style-include-default)
			    org-export-html-style-default)
			(plist-get opt-plist :style)
			(plist-get opt-plist :style-extra)
			"\n"
			(if (plist-get opt-plist :style-include-scripts)
			    org-export-html-scripts)))
	 (html-extension (plist-get opt-plist :html-extension))
	 valid thetoc have-headings first-heading-pos
	 (odd org-odd-levels-only)
	 (region-p (org-region-active-p))
	 (rbeg (and region-p (region-beginning)))
	 (rend (and region-p (region-end)))
	 (subtree-p
	  (if (plist-get opt-plist :ignore-subtree-p)
	      nil
	    (when region-p
	      (save-excursion
		(goto-char rbeg)
		(and (org-at-heading-p)
		     (>= (org-end-of-subtree t t) rend))))))
	 (level-offset (if subtree-p
			   (save-excursion
			     (goto-char rbeg)
			     (+ (funcall outline-level)
				(if org-odd-levels-only 1 0)))
			 0))
	 (opt-plist (setq org-export-opt-plist
			  (if subtree-p
			      (org-export-add-subtree-options opt-plist rbeg)
			    opt-plist)))
	 ;; The following two are dynamically scoped into other
	 ;; routines below.
	 (org-current-export-dir
	  (or pub-dir (org-export-directory :html opt-plist)))
	 (org-current-export-file buffer-file-name)
	 (level 0) (line "") (origline "") txt todo
	 (umax nil)
	 (umax-toc nil)
	 (filename (if to-buffer nil
		     (expand-file-name
		      (concat
		       (file-name-sans-extension
			(or (and subtree-p
				 (org-entry-get (region-beginning)
						"EXPORT_FILE_NAME" t))
			    (file-name-nondirectory buffer-file-name)))
		       "." html-extension)
		      (file-name-as-directory
		       (or pub-dir (org-export-directory :html opt-plist))))))
	 (current-dir (if buffer-file-name
			  (file-name-directory buffer-file-name)
			default-directory))
	 (auto-insert nil); Avoid any auto-insert stuff for the new file
	 (buffer (if to-buffer
		     (cond
		      ((eq to-buffer 'string) (get-buffer-create "*Org HTML Export*"))
		      (t (get-buffer-create to-buffer)))
		   (find-file-noselect filename)))
	 (org-levels-open (make-vector org-level-max nil))
	 (date        (org-html-expand (plist-get opt-plist :date)))
	 (author      (org-html-expand (plist-get opt-plist :author)))
	 (html-validation-link (or org-export-html-validation-link ""))
	 (title       (org-html-expand
		       (or (and subtree-p (org-export-get-title-from-subtree))
			   (plist-get opt-plist :title)
			   (and (not body-only)
				(not
				 (plist-get opt-plist :skip-before-1st-heading))
				(org-export-grab-title-from-buffer))
			   (and buffer-file-name
				(file-name-sans-extension
				 (file-name-nondirectory buffer-file-name)))
			   "UNTITLED")))
	 (link-up (and (plist-get opt-plist :link-up)
		       (string-match "\\S-" (plist-get opt-plist :link-up))
		       (plist-get opt-plist :link-up)))
	 (link-home (and (plist-get opt-plist :link-home)
			 (string-match "\\S-" (plist-get opt-plist :link-home))
			 (plist-get opt-plist :link-home)))
	 (dummy (setq opt-plist (plist-put opt-plist :title title)))
	 (html-table-tag (plist-get opt-plist :html-table-tag))
	 (quote-re0   (concat "^ *" org-quote-string "\\( +\\|[ \t]*$\\)"))
	 (quote-re    (format org-heading-keyword-regexp-format
			      org-quote-string))
	 (inquote     nil)
	 (infixed     nil)
	 (inverse     nil)
	 (email       (plist-get opt-plist :email))
	 (language    (plist-get opt-plist :language))
	 (keywords    (org-html-expand (plist-get opt-plist :keywords)))
	 (description (org-html-expand (plist-get opt-plist :description)))
	 (num         (plist-get opt-plist :section-numbers))
	 (lang-words  nil)
	 (head-count  0) cnt
	 (start       0)
	 (coding-system (and (boundp 'buffer-file-coding-system)
			     buffer-file-coding-system))
	 (coding-system-for-write (or org-export-html-coding-system
				      coding-system))
	 (save-buffer-coding-system (or org-export-html-coding-system
					coding-system))
	 (charset (and coding-system-for-write
		       (fboundp 'coding-system-get)
		       (coding-system-get coding-system-for-write
					  'mime-charset)))
	 (region
	  (buffer-substring
	   (if region-p (region-beginning) (point-min))
	   (if region-p (region-end) (point-max))))
	 (org-export-have-math nil)
	 (org-export-footnotes-seen nil)
	 (org-export-footnotes-data (org-footnote-all-labels 'with-defs))
	 (lines
	  (org-split-string
	   (org-export-preprocess-string
	    region
	    :emph-multiline t
	    :for-backend 'html
	    :skip-before-1st-heading
	    (plist-get opt-plist :skip-before-1st-heading)
	    :drawers (plist-get opt-plist :drawers)
	    :todo-keywords (plist-get opt-plist :todo-keywords)
	    :tasks (plist-get opt-plist :tasks)
	    :tags (plist-get opt-plist :tags)
	    :priority (plist-get opt-plist :priority)
	    :footnotes (plist-get opt-plist :footnotes)
	    :timestamps (plist-get opt-plist :timestamps)
	    :archived-trees
	    (plist-get opt-plist :archived-trees)
	    :select-tags (plist-get opt-plist :select-tags)
	    :exclude-tags (plist-get opt-plist :exclude-tags)
	    :add-text
	    (plist-get opt-plist :text)
	    :LaTeX-fragments
	    (plist-get opt-plist :LaTeX-fragments))
	   "[\r\n]"))
	 (mathjax
	  (if (or (eq (plist-get opt-plist :LaTeX-fragments) 'mathjax)
		  (and org-export-have-math
		       (eq (plist-get opt-plist :LaTeX-fragments) t)))

	      (org-export-html-mathjax-config
	       org-export-html-mathjax-template
	       org-export-html-mathjax-options
	       (or (plist-get opt-plist :mathjax) ""))
	    ""))
	 table-open
	 table-buffer table-orig-buffer
	 ind
	 rpl path attr desc descp desc1 desc2 link
	 snumber fnc
	 footnotes footref-seen
	 href
	 )

    (let ((inhibit-read-only t))
      (org-unmodified
       (remove-text-properties (point-min) (point-max)
			       '(:org-license-to-kill t))))

    (message "Exporting...")

    (setq org-min-level (org-get-min-level lines level-offset))
    (setq org-last-level org-min-level)
    (org-init-section-numbers)

    (cond
     ((and date (string-match "%" date))
      (setq date (format-time-string date)))
     (date)
     (t (setq date (format-time-string "%Y-%m-%d %T %Z"))))

    ;; Get the language-dependent settings
    (setq lang-words (or (assoc language org-export-language-setup)
			 (assoc "en" org-export-language-setup)))

    ;; Switch to the output buffer
    (set-buffer buffer)
    (let ((inhibit-read-only t)) (erase-buffer))
    (fundamental-mode)
    (org-install-letbind)

    (and (fboundp 'set-buffer-file-coding-system)
	 (set-buffer-file-coding-system coding-system-for-write))

    (let ((case-fold-search nil)
	  (org-odd-levels-only odd))
      ;; create local variables for all options, to make sure all called
      ;; functions get the correct information
      (mapc (lambda (x)
	      (set (make-local-variable (nth 2 x))
		   (plist-get opt-plist (car x))))
	    org-export-plist-vars)
      (setq umax (if arg (prefix-numeric-value arg)
		   org-export-headline-levels))
      (setq umax-toc (if (integerp org-export-with-toc)
			 (min org-export-with-toc umax)
		       umax))
      (unless body-only
	;; File header
	(insert (format
		 "%s
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
               \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"%s\" xml:lang=\"%s\">
<head>
<title>%s</title>
<meta http-equiv=\"Content-Type\" content=\"text/html;charset=%s\"/>
<meta name=\"title\" content=\"%s\"/>
<meta name=\"generator\" content=\"Org-mode\"/>
<meta name=\"generated\" content=\"%s\"/>
<meta name=\"author\" content=\"%s\"/>
<meta name=\"description\" content=\"%s\"/>
<meta name=\"keywords\" content=\"%s\"/>
%s
%s
</head>
<body>
%s
"
		 (format
		  (or (and (stringp org-export-html-xml-declaration)
			   org-export-html-xml-declaration)
		      (cdr (assoc html-extension org-export-html-xml-declaration))
		      (cdr (assoc "html" org-export-html-xml-declaration))

		      "")
		  (or charset "iso-8859-1"))
		 language language
		 title
		 (or charset "iso-8859-1")
		 title date author description keywords
		 style
		 mathjax
		 (if (or link-up link-home)
		     (concat
		      (format org-export-html-home/up-format
			      (or link-up link-home)
			      (or link-home link-up))
		      "\n")
		   "")))

	;; insert html preamble
	(when (plist-get opt-plist :html-preamble)
	  (let ((html-pre (plist-get opt-plist :html-preamble))
		(html-pre-real-contents ""))
	    (cond ((stringp html-pre)
		   (setq html-pre-real-contents
			 (format-spec html-pre `((?t . ,title) (?a . ,author)
						 (?d . ,date) (?e . ,email)))))
		  ((functionp html-pre)
		   (insert "<div id=\"" (nth 0 org-export-html-divs) "\">\n")
		   (if (stringp (funcall html-pre)) (insert (funcall html-pre)))
		   (insert "\n</div>\n"))
		  (t
		   (setq html-pre-real-contents
		    (format-spec
		     (or (cadr (assoc (nth 0 lang-words)
				      org-export-html-preamble-format))
			 (cadr (assoc "en" org-export-html-preamble-format)))
		     `((?t . ,title) (?a . ,author)
		       (?d . ,date) (?e . ,email))))))
	    ;; don't output an empty preamble DIV
	    (unless (and (functionp html-pre)
			 (equal html-pre-real-contents ""))
	      (insert "<div id=\"" (nth 0 org-export-html-divs) "\">\n")
	      (insert html-pre-real-contents)
	      (insert "\n</div>\n"))))

	;; begin wrap around body
	(insert (format "\n<div id=\"%s\">"
			;; FIXME org-export-html-content-div is obsolete since 7.7
			(or org-export-html-content-div
			    (nth 1 org-export-html-divs)))
		;; FIXME this should go in the preamble but is here so
		;; that org-infojs can still find it
		"\n<h1 class=\"title\">" title "</h1>\n"))

      ;; insert body
      (if (and org-export-with-toc (not body-only))
	  (progn
	    (push (format "<h%d>%s</h%d>\n"
			  org-export-html-toplevel-hlevel
			  (nth 3 lang-words)
			  org-export-html-toplevel-hlevel)
		  thetoc)
	    (push "<div id=\"text-table-of-contents\">\n" thetoc)
	    (push "<ul>\n<li>" thetoc)
	    (setq lines
		  (mapcar
		   #'(lambda (line)
		       (if (and (string-match org-todo-line-regexp line)
				(not (get-text-property 0 'org-protected line)))
			   ;; This is a headline
			   (progn
			     (setq have-headings t)
			     (setq level (- (match-end 1) (match-beginning 1)
					    level-offset)
				   level (org-tr-level level)
				   txt (save-match-data
					 (org-html-expand
					  (org-export-cleanup-toc-line
					   (match-string 3 line))))
				   todo
				   (or (and org-export-mark-todo-in-toc
					    (match-beginning 2)
					    (not (member (match-string 2 line)
							 org-done-keywords)))
					; TODO, not DONE
				       (and org-export-mark-todo-in-toc
					    (= level umax-toc)
					    (org-search-todo-below
					     line lines level))))
			     (if (string-match
				  (org-re "[ \t]+:\\([[:alnum:]_@:]+\\):[ \t]*$") txt)
				 (setq txt (replace-match
					    "&nbsp;&nbsp;&nbsp;<span class=\"tag\">\\1</span>" t nil txt)))
			     (if (string-match quote-re0 txt)
				 (setq txt (replace-match "" t t txt)))
			     (setq snumber (org-section-number level))
			     (if (and num (if (integerp num)
					      (>= num level)
					    num))
				 (setq txt (concat snumber " " txt)))
			     (if (<= level (max umax umax-toc))
				 (setq head-count (+ head-count 1)))
			     (if (<= level umax-toc)
				 (progn
				   (if (> level org-last-level)
				       (progn
					 (setq cnt (- level org-last-level))
					 (while (>= (setq cnt (1- cnt)) 0)
					   (push "\n<ul>\n<li>" thetoc))
					 (push "\n" thetoc)))
				   (if (< level org-last-level)
				       (progn
					 (setq cnt (- org-last-level level))
					 (while (>= (setq cnt (1- cnt)) 0)
					   (push "</li>\n</ul>" thetoc))
					 (push "\n" thetoc)))
				   ;; Check for targets
				   (while (string-match org-any-target-regexp line)
				     (setq line (replace-match
						 (concat "@<span class=\"target\">"
							 (match-string 1 line) "@</span> ")
						 t t line)))
				   (while (string-match "&lt;\\(&lt;\\)+\\|&gt;\\(&gt;\\)+" txt)
				     (setq txt (replace-match "" t t txt)))
				   (setq href
					 (replace-regexp-in-string
					  "\\." "-" (format "sec-%s" snumber)))
				   (setq href (org-solidify-link-text
					       (or (cdr (assoc href
							       org-export-preferred-target-alist)) href)))
				   (push
				    (format
				     (if todo
					 "</li>\n<li><a href=\"#%s\"><span class=\"todo\">%s</span></a>"
				       "</li>\n<li><a href=\"#%s\">%s</a>")
				     href txt) thetoc)

				   (setq org-last-level level)))))
		       line)
		   lines))
	    (while (> org-last-level (1- org-min-level))
	      (setq org-last-level (1- org-last-level))
	      (push "</li>\n</ul>\n" thetoc))
	    (push "</div>\n" thetoc)
	    (setq thetoc (if have-headings (nreverse thetoc) nil))))

      (setq head-count 0)
      (org-init-section-numbers)

      (org-open-par)

      (while (setq line (pop lines) origline line)
	(catch 'nextline

	  ;; end of quote section?
	  (when (and inquote (string-match org-outline-regexp-bol line))
	    (insert "</pre>\n")
	    (org-open-par)
	    (setq inquote nil))
	  ;; inside a quote section?
	  (when inquote
	    (insert (org-html-protect line) "\n")
	    (throw 'nextline nil))

	  ;; Fixed-width, verbatim lines (examples)
	  (when (and org-export-with-fixed-width
		     (string-match "^[ \t]*:\\(\\([ \t]\\|$\\)\\(.*\\)\\)" line))
	    (when (not infixed)
	      (setq infixed t)
	      (org-close-par-maybe)

	      (insert "<pre class=\"example\">\n"))
	    (insert (org-html-protect (match-string 3 line)) "\n")
	    (when (or (not lines)
		      (not (string-match "^[ \t]*:\\(\\([ \t]\\|$\\)\\(.*\\)\\)"
					 (car lines))))
	      (setq infixed nil)
	      (insert "</pre>\n")
	      (org-open-par))
	    (throw 'nextline nil))

	  ;; Protected HTML
	  (when (and (get-text-property 0 'org-protected line)
		     ;; Make sure it is the entire line that is protected
		     (not (< (or (next-single-property-change
				  0 'org-protected line) 10000)
			     (length line))))
	    (let (par (ind (get-text-property 0 'original-indentation line)))
	      (when (re-search-backward
		     "\\(<p>\\)\\([ \t\r\n]*\\)\\=" (- (point) 100) t)
		(setq par (match-string 1))
		(replace-match "\\2\n"))
	      (insert line "\n")
	      (while (and lines
			  (or (= (length (car lines)) 0)
			      (not ind)
			      (equal ind (get-text-property 0 'original-indentation (car lines))))
			  (or (= (length (car lines)) 0)
			      (get-text-property 0 'org-protected (car lines))))
		(insert (pop lines) "\n"))
	      (and par (insert "<p>\n")))
	    (throw 'nextline nil))

	  ;; Blockquotes, verse, and center
	  (when (equal "ORG-BLOCKQUOTE-START" line)
	    (org-close-par-maybe)
	    (insert "<blockquote>\n")
	    (org-open-par)
	    (throw 'nextline nil))
	  (when (equal "ORG-BLOCKQUOTE-END" line)
	    (org-close-par-maybe)
	    (insert "\n</blockquote>\n")
	    (org-open-par)
	    (throw 'nextline nil))
	  (when (equal "ORG-VERSE-START" line)
	    (org-close-par-maybe)
	    (insert "\n<p class=\"verse\">\n")
	    (setq org-par-open t)
	    (setq inverse t)
	    (throw 'nextline nil))
	  (when (equal "ORG-VERSE-END" line)
	    (insert "</p>\n")
	    (setq org-par-open nil)
	    (org-open-par)
	    (setq inverse nil)
	    (throw 'nextline nil))
	  (when (equal "ORG-CENTER-START" line)
	    (org-close-par-maybe)
	    (insert "\n<div style=\"text-align: center\">")
	    (org-open-par)
	    (throw 'nextline nil))
	  (when (equal "ORG-CENTER-END" line)
	    (org-close-par-maybe)
	    (insert "\n</div>")
	    (org-open-par)
	    (throw 'nextline nil))
	  (run-hooks 'org-export-html-after-blockquotes-hook)
	  (when inverse
	    (let ((i (org-get-string-indentation line)))
	      (if (> i 0)
		  (setq line (concat (mapconcat 'identity
						(make-list (* 2 i) "\\nbsp") "")
				     " " (org-trim line))))
	      (unless (string-match "\\\\\\\\[ \t]*$" line)
		(setq line (concat line "\\\\")))))

	  ;; make targets to anchors
	  (setq start 0)
	  (while (string-match
		  "<<<?\\([^<>]*\\)>>>?\\((INVISIBLE)\\)?[ \t]*\n?" line start)
	    (cond
	     ((get-text-property (match-beginning 1) 'org-protected line)
	      (setq start (match-end 1)))
	     ((match-end 2)
	      (setq line (replace-match
			  (format
			   "@<a name=\"%s\" id=\"%s\">@</a>"
			   (org-solidify-link-text (match-string 1 line))
			   (org-solidify-link-text (match-string 1 line)))
			  t t line)))
	     ((and org-export-with-toc (equal (string-to-char line) ?*))
	      ;; FIXME: NOT DEPENDENT on TOC?????????????????????
	      (setq line (replace-match
			  (concat "@<span class=\"target\">"
				  (match-string 1 line) "@</span> ")
			  ;; (concat "@<i>" (match-string 1 line) "@</i> ")
			  t t line)))
	     (t
	      (setq line (replace-match
			  (concat "@<a name=\""
				  (org-solidify-link-text (match-string 1 line))
				  "\" class=\"target\">" (match-string 1 line)
				  "@</a> ")
			  t t line)))))

	  (setq line (org-html-handle-time-stamps line))

	  ;; replace "&" by "&amp;", "<" and ">" by "&lt;" and "&gt;"
	  ;; handle @<..> HTML tags (replace "@&gt;..&lt;" by "<..>")
	  ;; Also handle sub_superscripts and checkboxes
	  (or (string-match org-table-hline-regexp line)
	      (string-match "^[ \t]*\\([+]-\\||[ ]\\)[-+ |]*[+|][ \t]*$" line)
	      (setq line (org-html-expand line)))

	  ;; Format the links
	  (setq line (org-html-handle-links line opt-plist))

	  ;; TODO items
	  (if (and org-todo-line-regexp
		   (string-match org-todo-line-regexp line)
		   (match-beginning 2))

	      (setq line
		    (concat (substring line 0 (match-beginning 2))
			    "<span class=\""
			    (if (member (match-string 2 line)
					org-done-keywords)
				"done" "todo")
			    " " (org-export-html-get-todo-kwd-class-name
				 (match-string 2 line))
			    "\">" (match-string 2 line)
			    "</span>" (substring line (match-end 2)))))

	  ;; Does this contain a reference to a footnote?
	  (when org-export-with-footnotes
	    (setq start 0)
	    (while (string-match "\\([^* \t].*?\\)\\[\\([0-9]+\\)\\]" line start)
	      ;; Discard protected matches not clearly identified as
	      ;; footnote markers.
	      (if (or (get-text-property (match-beginning 2) 'org-protected line)
		      (not (get-text-property (match-beginning 2) 'org-footnote line)))
		  (setq start (match-end 2))
		(let ((n (match-string 2 line)) extra a)
		  (if (setq a (assoc n footref-seen))
		      (progn
			(setcdr a (1+ (cdr a)))
			(setq extra (format ".%d" (cdr a))))
		    (setq extra "")
		    (push (cons n 1) footref-seen))
		  (setq line
			(replace-match
			 (concat
			  (format
			   (concat "%s"
				   (format org-export-html-footnote-format
					   (concat "<a class=\"footref\" name=\"fnr.%s%s\" href=\"#fn.%s\">%s</a>")))
			   (or (match-string 1 line) "") n extra n n)
			  ;; If another footnote is following the
			  ;; current one, add a separator.
			  (if (save-match-data
				(string-match "\\`\\[[0-9]+\\]"
					      (substring line (match-end 0))))
			      org-export-html-footnote-separator
			    ""))
			 t t line))))))

	  (cond
	   ((string-match "^\\(\\*+\\)\\(?: +\\(.*?\\)\\)?[ \t]*$" line)
	    ;; This is a headline
	    (setq level (org-tr-level (- (match-end 1) (match-beginning 1)
					 level-offset))
		  txt (match-string 2 line))
	    (if (string-match quote-re0 txt)
		(setq txt (replace-match "" t t txt)))
	    (if (<= level (max umax umax-toc))
		(setq head-count (+ head-count 1)))
	    (setq first-heading-pos (or first-heading-pos (point)))
	    (org-html-level-start level txt umax
				  (and org-export-with-toc (<= level umax))
				  head-count opt-plist)

	    ;; QUOTES
	    (when (string-match quote-re line)
	      (org-close-par-maybe)
	      (insert "<pre>")
	      (setq inquote t)))

	   ((and org-export-with-tables
		 (string-match "^\\([ \t]*\\)\\(|\\|\\+-+\\+\\)" line))
	    (when (not table-open)
	      ;; New table starts
	      (setq table-open t table-buffer nil table-orig-buffer nil))

	    ;; Accumulate lines
	    (setq table-buffer (cons line table-buffer)
		  table-orig-buffer (cons origline table-orig-buffer))
	    (when (or (not lines)
		      (not (string-match "^\\([ \t]*\\)\\(|\\|\\+-+\\+\\)"
					 (car lines))))
	      (setq table-open nil
		    table-buffer (nreverse table-buffer)
		    table-orig-buffer (nreverse table-orig-buffer))
	      (org-close-par-maybe)
	      (insert (org-format-table-html table-buffer table-orig-buffer))))

	   ;; Normal lines

	   (t
	    ;; This line either is list item or end a list.
	    (when (get-text-property 0 'list-item line)
	      (setq line (org-html-export-list-line
			  line
			  (get-text-property 0 'list-item line)
			  (get-text-property 0 'list-struct line)
			  (get-text-property 0 'list-prevs line))))

	    ;; Horizontal line
	    (when (string-match "^[ \t]*-\\{5,\\}[ \t]*$" line)
	      (if org-par-open
		  (insert "\n</p>\n<hr/>\n<p>\n")
		(insert "\n<hr/>\n"))
	      (throw 'nextline nil))

	    ;; Empty lines start a new paragraph.  If hand-formatted lists
	    ;; are not fully interpreted, lines starting with "-", "+", "*"
	    ;; also start a new paragraph.
	    (if (string-match "^ [-+*]-\\|^[ \t]*$" line) (org-open-par))

	    ;; Is this the start of a footnote?
	    (when org-export-with-footnotes
	      (when (and (boundp 'footnote-section-tag-regexp)
			 (string-match (concat "^" footnote-section-tag-regexp)
				       line))
		;; ignore this line
		(throw 'nextline nil))
	      (when (string-match "^[ \t]*\\[\\([0-9]+\\)\\]" line)
		(org-close-par-maybe)
		(let ((n (match-string 1 line)))
		  (setq org-par-open t
			line (replace-match
			      (format
			       (concat "<p class=\"footnote\">"
				       (format org-export-html-footnote-format
					       "<a class=\"footnum\" name=\"fn.%s\" href=\"#fnr.%s\">%s</a>"))
			       n n n) t t line)))))
	    ;; Check if the line break needs to be conserved
	    (cond
	     ((string-match "\\\\\\\\[ \t]*$" line)
	      (setq line (replace-match "<br/>" t t line)))
	     (org-export-preserve-breaks
	      (setq line (concat line "<br/>"))))

	    ;; Check if a paragraph should be started
	    (let ((start 0))
	      (while (and org-par-open
			  (string-match "\\\\par\\>" line start))
		;; Leave a space in the </p> so that the footnote matcher
		;; does not see this.
		(if (not (get-text-property (match-beginning 0)
					    'org-protected line))
		    (setq line (replace-match "</p ><p >" t t line)))
		(setq start (match-end 0))))

	    (insert line "\n")))))

      ;; Properly close all local lists and other lists
      (when inquote
	(insert "</pre>\n")
	(org-open-par))

      (org-html-level-start 1 nil umax
			    (and org-export-with-toc (<= level umax))
			    head-count opt-plist)
      ;; the </div> to close the last text-... div.
      (when (and (> umax 0) first-heading-pos) (insert "</div>\n"))

      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward
		"\\(\\(<p class=\"footnote\">\\)[^\000]*?\\)\\(\\(\\2\\)\\|\\'\\)"
		nil t)
	  (push (match-string 1) footnotes)
	  (replace-match "\\4" t nil)
	  (goto-char (match-beginning 0))))
      (when footnotes
	(insert (format org-export-html-footnotes-section
			(nth 4 lang-words)
			(mapconcat 'identity (nreverse footnotes) "\n"))
		"\n"))
      (let ((bib (org-export-html-get-bibliography)))
	(when bib
	  (insert "\n" bib "\n")))

      (unless body-only
	;; end wrap around body
	(insert "</div>\n")

	;; export html postamble
	(let ((html-post (plist-get opt-plist :html-postamble))
	      (email
	       (mapconcat (lambda(e)
			    (format "<a href=\"mailto:%s\">%s</a>" e e))
			  (split-string email ",+ *")
			  ", "))
	      (creator-info
	       (concat "Org version " org-version " with Emacs version "
		       (number-to-string emacs-major-version))))

	  (when (plist-get opt-plist :html-postamble)
	    (insert "\n<div id=\"" (nth 2 org-export-html-divs) "\">\n")
	    (cond ((stringp html-post)
		   (insert (format-spec html-post
					`((?a . ,author) (?e . ,email)
					  (?d . ,date)   (?c . ,creator-info)
					  (?v . ,html-validation-link)))))
		  ((functionp html-post)
		   (if (stringp (funcall html-post)) (insert (funcall html-post))))
		  ((eq html-post 'auto)
		   ;; fall back on default postamble
		   (when (plist-get opt-plist :time-stamp-file)
		     (insert "<p class=\"date\">" (nth 2 lang-words) ": " date "</p>\n"))
		   (when (and (plist-get opt-plist :author-info) author)
		       (insert "<p class=\"author\">" (nth 1 lang-words) ": " author "</p>\n"))
		   (when (and (plist-get opt-plist :email-info) email)
		     (insert "<p class=\"email\">" email "</p>\n"))
		   (when (plist-get opt-plist :creator-info)
		     (insert "<p class=\"creator\">"
			     (concat "Org version " org-version " with Emacs version "
				     (number-to-string emacs-major-version) "</p>\n")))
		   (insert html-validation-link "\n"))
		  (t
		   (insert (format-spec
			    (or (cadr (assoc (nth 0 lang-words)
					     org-export-html-postamble-format))
				(cadr (assoc "en" org-export-html-postamble-format)))
			    `((?a . ,author) (?e . ,email)
			      (?d . ,date)   (?c . ,creator-info)
			      (?v . ,html-validation-link))))))
	    (insert "\n</div>"))))

      ;; FIXME `org-export-html-with-timestamp' has been declared
      ;; obsolete since Org 7.7 -- don't forget to remove this.
      (if org-export-html-with-timestamp
	  (insert org-export-html-html-helper-timestamp))

      (unless body-only (insert "\n</body>\n</html>\n"))

      (unless (plist-get opt-plist :buffer-will-be-killed)
	(normal-mode)
	(if (eq major-mode (default-value 'major-mode))
	    (html-mode)))

      ;; insert the table of contents
      (goto-char (point-min))
      (when thetoc
	(if (or (re-search-forward
		 "<p>\\s-*\\[TABLE-OF-CONTENTS\\]\\s-*</p>" nil t)
		(re-search-forward
		 "\\[TABLE-OF-CONTENTS\\]" nil t))
	    (progn
	      (goto-char (match-beginning 0))
	      (replace-match ""))
	  (goto-char first-heading-pos)
	  (when (looking-at "\\s-*</p>")
	    (goto-char (match-end 0))
	    (insert "\n")))
	(insert "<div id=\"table-of-contents\">\n")
	(let ((beg (point)))
	  (mapc 'insert thetoc)
	  (insert "</div>\n")
	  (while (re-search-backward "<li>[ \r\n\t]*</li>\n?" beg t)
	    (replace-match ""))))
      ;; remove empty paragraphs
      (goto-char (point-min))
      (while (re-search-forward "<p>[ \r\n\t]*</p>" nil t)
	(replace-match ""))
      (goto-char (point-min))
      ;; Convert whitespace place holders
      (goto-char (point-min))
      (let (beg end n)
	(while (setq beg (next-single-property-change (point) 'org-whitespace))
	  (setq n (get-text-property beg 'org-whitespace)
		end (next-single-property-change beg 'org-whitespace))
	  (goto-char beg)
	  (delete-region beg end)
	  (insert (format "<span style=\"visibility:hidden;\">%s</span>"
			  (make-string n ?x)))))
      ;; Remove empty lines at the beginning of the file.
      (goto-char (point-min))
      (when (looking-at "\\s-+\n") (replace-match ""))
      ;; Remove display properties
      (remove-text-properties (point-min) (point-max) '(display t))
      ;; Run the hook
      (run-hooks 'org-export-html-final-hook)
      (or to-buffer (save-buffer))
      (goto-char (point-min))
      (or (org-export-push-to-kill-ring "HTML")
	  (message "Exporting... done"))
      (if (eq to-buffer 'string)
	  (prog1 (buffer-substring (point-min) (point-max))
	    (kill-buffer (current-buffer)))
	(current-buffer)))))

(defun org-export-html-format-href (s)
  "Make sure the S is valid as a href reference in an XHTML document."
  (save-match-data
    (let ((start 0))
      (while (string-match "&" s start)
	(setq start (+ (match-beginning 0) 3)
	      s (replace-match "&amp;" t t s)))))
  s)

(defun org-export-html-format-desc (s)
  "Make sure the S is valid as a description in a link."
  (if (and s (not (get-text-property 1 'org-protected s)))
      (save-match-data
	(org-html-do-expand s))
    s))

(defun org-export-html-format-image (src par-open)
  "Create image tag with source and attributes."
  (save-match-data
    (if (string-match "^ltxpng/" src)
	(format "<img src=\"%s\" alt=\"%s\"/>"
                src (org-find-text-property-in-string 'org-latex-src src))
      (let* ((caption (org-find-text-property-in-string 'org-caption src))
	     (attr (org-find-text-property-in-string 'org-attributes src))
	     (label (org-find-text-property-in-string 'org-label src)))
	(setq caption (and caption (org-html-do-expand caption)))
	(concat
	(if caption
	    (format "%s<div %sclass=\"figure\">
<p>"
		    (if org-par-open "</p>\n" "")
		    (if label (format "id=\"%s\" " (org-solidify-link-text label)) "")))
	(format "<img src=\"%s\"%s />"
		src
		(if (string-match "\\<alt=" (or attr ""))
		    (concat " " attr )
		  (concat " " attr " alt=\"" src "\"")))
	(if caption
	    (format "</p>%s
</div>%s"
		(concat "\n<p>" caption "</p>")
		(if org-par-open "\n<p>" ""))))))))

(defun org-export-html-get-bibliography ()
  "Find bibliography, cut it out and return it."
  (catch 'exit
    (let (beg end (cnt 1) bib)
      (save-excursion
	(goto-char (point-min))
	(when (re-search-forward "^[ \t]*<div \\(id\\|class\\)=\"bibliography\"" nil t)
	  (setq beg (match-beginning 0))
	  (while (re-search-forward "</?div\\>" nil t)
	    (setq cnt (+ cnt (if (string= (match-string 0) "<div") +1 -1)))
	    (when (= cnt 0)
	      (and (looking-at ">") (forward-char 1))
	      (setq bib (buffer-substring beg (point)))
	      (delete-region beg (point))
	    (throw 'exit bib))))
	nil))))

(defvar org-table-number-regexp) ; defined in org-table.el
(defun org-format-table-html (lines olines &optional no-css)
  "Find out which HTML converter to use and return the HTML code.
NO-CSS is passed to the exporter."
  (if (stringp lines)
      (setq lines (org-split-string lines "\n")))
  (if (string-match "^[ \t]*|" (car lines))
      ;; A normal org table
      (org-format-org-table-html lines nil no-css)
    ;; Table made by table.el
    (or (org-format-table-table-html-using-table-generate-source
	 olines (not org-export-prefer-native-exporter-for-tables))
	;; We are here only when table.el table has NO col or row
	;; spanning and the user prefers using org's own converter for
	;; exporting of such simple table.el tables.
	(org-format-table-table-html lines))))

(defvar org-table-number-fraction) ; defined in org-table.el
(defun org-format-org-table-html (lines &optional splice no-css)
  "Format a table into HTML.
LINES is a list of lines.  Optional argument SPLICE means, do not
insert header and surrounding <table> tags, just format the lines.
Optional argument NO-CSS means use XHTML attributes instead of CSS
for formatting.  This is required for the DocBook exporter."
  (require 'org-table)
  ;; Get rid of hlines at beginning and end
  (if (string-match "^[ \t]*|-" (car lines)) (setq lines (cdr lines)))
  (setq lines (nreverse lines))
  (if (string-match "^[ \t]*|-" (car lines)) (setq lines (cdr lines)))
  (setq lines (nreverse lines))
  (when org-export-table-remove-special-lines
    ;; Check if the table has a marking column.  If yes remove the
    ;; column and the special lines
    (setq lines (org-table-clean-before-export lines)))

  (let* ((caption (org-find-text-property-in-string 'org-caption (car lines)))
	 (label (org-find-text-property-in-string 'org-label (car lines)))
	 (col-cookies (org-find-text-property-in-string 'org-col-cookies
							(car lines)))
	 (attributes (org-find-text-property-in-string 'org-attributes
						       (car lines)))
	 (html-table-tag (org-export-splice-attributes
			  html-table-tag attributes))
	 (head (and org-export-highlight-first-table-line
		    (delq nil (mapcar
			       (lambda (x) (string-match "^[ \t]*|-" x))
			       (cdr lines)))))
	 (nline 0) fnum nfields i (cnt 0)
	 tbopen line fields html gr colgropen rowstart rowend
	 ali align aligns n)
    (setq caption (and caption (org-html-do-expand caption)))
    (when (and col-cookies org-table-clean-did-remove-column)
      (setq col-cookies
	    (mapcar (lambda (x) (cons (1- (car x)) (cdr x))) col-cookies)))
    (if splice (setq head nil))
    (unless splice (push (if head "<thead>" "<tbody>") html))
    (setq tbopen t)
    (while (setq line (pop lines))
      (catch 'next-line
	(if (string-match "^[ \t]*|-" line)
	    (progn
	      (unless splice
		(push (if head "</thead>" "</tbody>") html)
		(if lines (push "<tbody>" html) (setq tbopen nil)))
	      (setq head nil)   ;; head ends here, first time around
	      ;; ignore this line
	      (throw 'next-line t)))
	;; Break the line into fields
	(setq fields (org-split-string line "[ \t]*|[ \t]*"))
	(unless fnum (setq fnum (make-vector (length fields) 0)
			   nfields (length fnum)))
	(setq nline (1+ nline) i -1
	      rowstart (eval (car org-export-table-row-tags))
	      rowend (eval (cdr org-export-table-row-tags)))
	(push (concat rowstart
		      (mapconcat
		       (lambda (x)
			 (setq i (1+ i) ali (format "@@class%03d@@" i))
			 (if (and (< i nfields) ; make sure no rogue line causes an error here
				  (string-match org-table-number-regexp x))
			     (incf (aref fnum i)))
			 (cond
			  (head
			   (concat
			    (format (car org-export-table-header-tags)
				    "col" ali)
			    x
			    (cdr org-export-table-header-tags)))
			  ((and (= i 0) org-export-html-table-use-header-tags-for-first-column)
			   (concat
			    (format (car org-export-table-header-tags)
				    "row" ali)
			    x
			    (cdr org-export-table-header-tags)))
			  (t
			   (concat (format (car org-export-table-data-tags) ali)
				   x
				   (cdr org-export-table-data-tags)))))
		       fields "")
		      rowend)
	      html)))
    (unless splice (if tbopen (push "</tbody>" html)))
    (unless splice (push "</table>\n" html))
    (setq html (nreverse html))
    (unless splice
      ;; Put in col tags with the alignment (unfortunately often ignored...)
      (unless (car org-table-colgroup-info)
	(setq org-table-colgroup-info
	      (cons :start (cdr org-table-colgroup-info))))
      (setq i 0)
      (push (mapconcat
	     (lambda (x)
	       (setq gr (pop org-table-colgroup-info)
		     i (1+ i)
		     align (if (nth 1 (assoc i col-cookies))
			       (cdr (assoc (nth 1 (assoc i col-cookies))
					   '(("l" . "left") ("r" . "right")
					     ("c" . "center"))))
			     (if (> (/ (float x) nline)
				    org-table-number-fraction)
				 "right" "left")))
	       (push align aligns)
	       (format (if no-css
			   "%s<col align=\"%s\" />%s"
			 "%s<col class=\"%s\" />%s")
		       (if (memq gr '(:start :startend))
			   (prog1
			       (if colgropen
				   "</colgroup>\n<colgroup>"
				 "<colgroup>")
			     (setq colgropen t))
			 "")
		       align
		       (if (memq gr '(:end :startend))
			   (progn (setq colgropen nil) "</colgroup>")
			 "")))
	     fnum "")
	    html)
      (setq aligns (nreverse aligns))
      (if colgropen (setq html (cons (car html)
				     (cons "</colgroup>" (cdr html)))))
      ;; Since the output of HTML table formatter can also be used in
      ;; DocBook document, we want to always include the caption to make
      ;; DocBook XML file valid.
      (push (format "<caption>%s</caption>" (or caption "")) html)
      (when label
	      (setq html-table-tag (org-export-splice-attributes html-table-tag (format "id=\"%s\"" (org-solidify-link-text label)))))
      (push html-table-tag html))
    (setq html (mapcar
		(lambda (x)
		  (replace-regexp-in-string
		   "@@class\\([0-9]+\\)@@"
		   (lambda (txt)
		     (if (not org-export-html-table-align-individual-fields)
			 ""
		       (setq n (string-to-number (match-string 1 txt)))
		       (format (if no-css " align=\"%s\"" " class=\"%s\"")
			       (or (nth n aligns) "left"))))
		   x))
		html))
    (concat (mapconcat 'identity html "\n") "\n")))

(defun org-export-splice-attributes (tag attributes)
  "Read attributes in string ATTRIBUTES, add and replace in HTML tag TAG."
  (if (not attributes)
      tag
    (let (oldatt newatt)
      (setq oldatt (org-extract-attributes-from-string tag)
	    tag (pop oldatt)
	    newatt (cdr (org-extract-attributes-from-string attributes)))
      (while newatt
	(setq oldatt (plist-put oldatt (pop newatt) (pop newatt))))
      (if (string-match ">" tag)
	  (setq tag
		(replace-match (concat (org-attributes-to-string oldatt) ">")
			       t t tag)))
      tag)))

(defun org-format-table-table-html (lines)
  "Format a table generated by table.el into HTML.
This conversion does *not* use `table-generate-source' from table.el.
This has the advantage that Org-mode's HTML conversions can be used.
But it has the disadvantage, that no cell- or row-spanning is allowed."
  (let (line field-buffer
	     (head org-export-highlight-first-table-line)
	     fields html empty i)
    (setq html (concat html-table-tag "\n"))
    (while (setq line (pop lines))
      (setq empty "&nbsp;")
      (catch 'next-line
	(if (string-match "^[ \t]*\\+-" line)
	    (progn
	      (if field-buffer
		  (progn
		    (setq
		     html
		     (concat
		      html
		      "<tr>"
		      (mapconcat
		       (lambda (x)
			 (if (equal x "") (setq x empty))
			 (if head
			     (concat
			      (format (car org-export-table-header-tags) "col" "")
			      x
			      (cdr org-export-table-header-tags))
			   (concat (format (car org-export-table-data-tags) "") x
				   (cdr org-export-table-data-tags))))
		       field-buffer "\n")
		      "</tr>\n"))
		    (setq head nil)
		    (setq field-buffer nil)))
	      ;; Ignore this line
	      (throw 'next-line t)))
	;; Break the line into fields and store the fields
	(setq fields (org-split-string line "[ \t]*|[ \t]*"))
	(if field-buffer
	    (setq field-buffer (mapcar
				(lambda (x)
				  (concat x "<br/>" (pop fields)))
				field-buffer))
	  (setq field-buffer fields))))
    (setq html (concat html "</table>\n"))
    html))

(defun org-format-table-table-html-using-table-generate-source (lines
								&optional
								spanned-only)
  "Format a table into html, using `table-generate-source' from table.el.
Use SPANNED-ONLY to suppress exporting of simple table.el tables.

When SPANNED-ONLY is nil, all table.el tables are exported.  When
SPANNED-ONLY is non-nil, only tables with either row or column
spans are exported.

This routine returns the generated source or nil as appropriate.

Refer docstring of `org-export-prefer-native-exporter-for-tables'
for further information."
  (require 'table)
  (with-current-buffer (get-buffer-create " org-tmp1 ")
    (erase-buffer)
    (insert (mapconcat 'identity lines "\n"))
    (goto-char (point-min))
    (if (not (re-search-forward "|[^+]" nil t))
	(error "Error processing table"))
    (table-recognize-table)
    (when (or (not spanned-only)
	      (let* ((dim (table-query-dimension))
		     (c (nth 4 dim)) (r (nth 5 dim)) (cells (nth 6 dim)))
		(not (= (* c r) cells))))
      (with-current-buffer (get-buffer-create " org-tmp2 ") (erase-buffer))
      (table-generate-source 'html " org-tmp2 ")
      (set-buffer " org-tmp2 ")
      (buffer-substring (point-min) (point-max)))))

(defun org-export-splice-style (style extra)
  "Splice EXTRA into STYLE, just before \"</style>\"."
  (if (and (stringp extra)
	   (string-match "\\S-" extra)
	   (string-match "</style>" style))
      (concat (substring style 0 (match-beginning 0))
	      "\n" extra "\n"
	      (substring style (match-beginning 0)))
    style))

(defun org-html-handle-time-stamps (s)
  "Format time stamps in string S, or remove them."
  (catch 'exit
    (let (r b)
      (when org-maybe-keyword-time-regexp
	(while (string-match org-maybe-keyword-time-regexp s)
	  (or b (setq b (substring s 0 (match-beginning 0))))
	  (setq r (concat
		   r (substring s 0 (match-beginning 0))
		   " @<span class=\"timestamp-wrapper\">"
		   (if (match-end 1)
		       (format "@<span class=\"timestamp-kwd\">%s @</span>"
			       (match-string 1 s)))
		   (format " @<span class=\"timestamp\">%s@</span>"
			   (substring
			    (org-translate-time (match-string 3 s)) 1 -1))
		   "@</span>")
		s (substring s (match-end 0)))))
      ;; Line break if line started and ended with time stamp stuff
      (if (not r)
	  s
	(setq r (concat r s))
	(unless (string-match "\\S-" (concat b s))
	  (setq r (concat r "@<br/>")))
	r))))

(defvar htmlize-buffer-places)  ; from htmlize.el
(defun org-export-htmlize-region-for-paste (beg end)
  "Convert the region to HTML, using htmlize.el.
This is much like `htmlize-region-for-paste', only that it uses
the settings define in the org-... variables."
  (let* ((htmlize-output-type org-export-htmlize-output-type)
	 (htmlize-css-name-prefix org-export-htmlize-css-font-prefix)
	 (htmlbuf (htmlize-region beg end)))
    (unwind-protect
	(with-current-buffer htmlbuf
	  (buffer-substring (plist-get htmlize-buffer-places 'content-start)
			    (plist-get htmlize-buffer-places 'content-end)))
      (kill-buffer htmlbuf))))

;;;###autoload
(defun org-export-htmlize-generate-css ()
  "Create the CSS for all font definitions in the current Emacs session.
Use this to create face definitions in your CSS style file that can then
be used by code snippets transformed by htmlize.
This command just produces a buffer that contains class definitions for all
faces used in the current Emacs session.  You can copy and paste the ones you
need into your CSS file.

If you then set `org-export-htmlize-output-type' to `css', calls to
the function `org-export-htmlize-region-for-paste' will produce code
that uses these same face definitions."
  (interactive)
  (require 'htmlize)
  (and (get-buffer "*html*") (kill-buffer "*html*"))
  (with-temp-buffer
    (let ((fl (face-list))
	  (htmlize-css-name-prefix "org-")
	  (htmlize-output-type 'css)
	  f i)
      (while (setq f (pop fl)
		   i (and f (face-attribute f :inherit)))
	(when (and (symbolp f) (or (not i) (not (listp i))))
	  (insert (org-add-props (copy-sequence "1") nil 'face f))))
      (htmlize-region (point-min) (point-max))))
  (org-pop-to-buffer-same-window "*html*")
  (goto-char (point-min))
  (if (re-search-forward "<style" nil t)
      (delete-region (point-min) (match-beginning 0)))
  (if (re-search-forward "</style>" nil t)
      (delete-region (1+ (match-end 0)) (point-max)))
  (beginning-of-line 1)
  (if (looking-at " +") (replace-match ""))
  (goto-char (point-min)))

(defun org-html-protect (s)
  "Convert characters to HTML equivalent.
Possible conversions are set in `org-export-html-protect-char-alist'."
  (let ((cl org-export-html-protect-char-alist) c)
    (while (setq c (pop cl))
      (let ((start 0))
	(while (string-match (car c) s start)
	  (setq s (replace-match (cdr c) t t s)
		start (1+ (match-beginning 0))))))
    s))

(defun org-html-expand (string)
  "Prepare STRING for HTML export.  Apply all active conversions.
If there are links in the string, don't modify these.  If STRING
is nil, return nil."
  (when string
    (let* ((re (concat org-bracket-link-regexp "\\|"
		       (org-re "[ \t]+\\(:[[:alnum:]_@#%:]+:\\)[ \t]*$")))
	   m s l res)
      (while (setq m (string-match re string))
	(setq s (substring string 0 m)
	      l (match-string 0 string)
	      string (substring string (match-end 0)))
	(push (org-html-do-expand s) res)
      (push l res))
      (push (org-html-do-expand string) res)
      (apply 'concat (nreverse res)))))

(defun org-html-do-expand (s)
  "Apply all active conversions to translate special ASCII to HTML."
  (setq s (org-html-protect s))
  (if org-export-html-expand
      (while (string-match "@&lt;\\([^&]*\\)&gt;" s)
	(setq s (replace-match "<\\1>" t nil s))))
  (if org-export-with-emphasize
      (setq s (org-export-html-convert-emphasize s)))
  (if org-export-with-special-strings
      (setq s (org-export-html-convert-special-strings s)))
  (if org-export-with-sub-superscripts
      (setq s (org-export-html-convert-sub-super s)))
  (if org-export-with-TeX-macros
      (let ((start 0) wd rep)
	(while (setq start (string-match "\\\\\\([a-zA-Z]+[0-9]*\\)\\({}\\)?"
					 s start))
	  (if (get-text-property (match-beginning 0) 'org-protected s)
	      (setq start (match-end 0))
	    (setq wd (match-string 1 s))
	    (if (setq rep (org-entity-get-representation wd 'html))
		(setq s (replace-match rep t t s))
	      (setq start (+ start (length wd))))))))
  s)

(defun org-export-html-convert-special-strings (string)
  "Convert special characters in STRING to HTML."
  (let ((all org-export-html-special-string-regexps)
	e a re rpl start)
    (while (setq a (pop all))
      (setq re (car a) rpl (cdr a) start 0)
      (while (string-match re string start)
	(if (get-text-property (match-beginning 0) 'org-protected string)
	    (setq start (match-end 0))
	  (setq string (replace-match rpl t nil string)))))
    string))

(defun org-export-html-convert-sub-super (string)
  "Convert sub- and superscripts in STRING to HTML."
  (let (key c (s 0) (requireb (eq org-export-with-sub-superscripts '{})))
    (while (string-match org-match-substring-regexp string s)
      (cond
       ((and requireb (match-end 8)) (setq s (match-end 2)))
       ((get-text-property  (match-beginning 2) 'org-protected string)
	(setq s (match-end 2)))
       (t
	(setq s (match-end 1)
	      key (if (string= (match-string 2 string) "_") "sub" "sup")
	      c (or (match-string 8 string)
		    (match-string 6 string)
		    (match-string 5 string))
	      string (replace-match
		      (concat (match-string 1 string)
			      "<" key ">" c "</" key ">")
		      t t string)))))
    (while (string-match "\\\\\\([_^]\\)" string)
      (setq string (replace-match (match-string 1 string) t t string)))
    string))

(defun org-export-html-convert-emphasize (string)
  "Apply emphasis."
  (let ((s 0) rpl)
    (while (string-match org-emph-re string s)
      (if (not (equal
		(substring string (match-beginning 3) (1+ (match-beginning 3)))
		(substring string (match-beginning 4) (1+ (match-beginning 4)))))
	  (setq s (match-beginning 0)
		rpl
		(concat
		 (match-string 1 string)
		 (nth 2 (assoc (match-string 3 string) org-emphasis-alist))
		 (match-string 4 string)
		 (nth 3 (assoc (match-string 3 string)
			       org-emphasis-alist))
		 (match-string 5 string))
		string (replace-match rpl t t string)
		s (+ s (- (length rpl) 2)))
	(setq s (1+ s))))
    string))

(defun org-open-par ()
  "Insert <p>, but first close previous paragraph if any."
  (org-close-par-maybe)
  (insert "\n<p>")
  (setq org-par-open t))
(defun org-close-par-maybe ()
  "Close paragraph if there is one open."
  (when org-par-open
    (insert "</p>")
    (setq org-par-open nil)))
(defun org-close-li (&optional type)
  "Close <li> if necessary."
  (org-close-par-maybe)
  (insert (if (equal type "d") "</dd>\n" "</li>\n")))

(defvar body-only) ; dynamically scoped into this.
(defun org-html-level-start (level title umax with-toc head-count &optional opt-plist)
  "Insert a new level in HTML export.
When TITLE is nil, just close all open levels."
  (org-close-par-maybe)
  (let* ((target (and title (org-get-text-property-any 0 'target title)))
	 (extra-targets (and target
			     (assoc target org-export-target-aliases)))
	 (extra-class (and title (org-get-text-property-any 0 'html-container-class title)))
	 (preferred (and target
			 (cdr (assoc target org-export-preferred-target-alist))))
	 (l org-level-max)
	 (num (plist-get opt-plist :section-numbers))
	 snumber snu href suffix)
    (setq extra-targets (remove (or preferred target) extra-targets))
    (setq extra-targets
	  (mapconcat (lambda (x)
		       (setq x (org-solidify-link-text
				(if (org-uuidgen-p x) (concat "ID-" x) x)))
		       (if (stringp org-export-html-headline-anchor-format)
			   (format org-export-html-headline-anchor-format x x)
			 ""))
		     extra-targets
		     ""))
    (while (>= l level)
      (if (aref org-levels-open (1- l))
	  (progn
	    (org-html-level-close l umax)
	    (aset org-levels-open (1- l) nil)))
      (setq l (1- l)))
    (when title
      ;; If title is nil, this means this function is called to close
      ;; all levels, so the rest is done only if title is given
	(when (string-match (org-re "\\(:[[:alnum:]_@#%:]+:\\)[ \t]*$") title)
	  (setq title (replace-match
		       (if org-export-with-tags
			   (save-match-data
			     (concat
			      "&nbsp;&nbsp;&nbsp;<span class=\"tag\">"
			      (mapconcat
			       (lambda (x)
				 (format "<span class=\"%s\">%s</span>"
					 (org-export-html-get-tag-class-name x)
					 x))
			       (org-split-string (match-string 1 title) ":")
			       "&nbsp;")
			      "</span>"))
			 "")
		       t t title)))
      (if (> level umax)
	  (progn
	    (if (aref org-levels-open (1- level))
		(progn
		  (org-close-li)
		  (if target
		      (insert (format "<li id=\"%s\">" (org-solidify-link-text (or preferred target)))
			      extra-targets title "<br/>\n")
		    (insert "<li>" title "<br/>\n")))
	      (aset org-levels-open (1- level) t)
	      (org-close-par-maybe)
	      (if target
		  (insert (format "<ul>\n<li id=\"%s\">" (org-solidify-link-text (or preferred target)))
			  extra-targets title "<br/>\n")
		(insert "<ul>\n<li>" title "<br/>\n"))))
	(aset org-levels-open (1- level) t)
	(setq snumber (org-section-number level)
	      snu (replace-regexp-in-string "\\." "-" snumber))
	(setq level (+ level org-export-html-toplevel-hlevel -1))
	(if (and num (not body-only))
	    (setq title (concat
			 (format "<span class=\"section-number-%d\">%s</span>"
				 level
				 (if (and num
					  (if (integerp num)
					      ;; fix up num to take into
					      ;; account the top-level
					      ;; heading value
					      (>= (+ num org-export-html-toplevel-hlevel -1)
						  level)
					    num))
				     snumber
				   ""))
			 " " title)))
	(unless (= head-count 1) (insert "\n</div>\n"))
	(setq href (cdr (assoc (concat "sec-" snu) org-export-preferred-target-alist)))
	(setq suffix (org-solidify-link-text (or href snu)))
	(setq href (org-solidify-link-text (or href (concat "sec-" snu))))
	(insert (format "\n<div id=\"outline-container-%s\" class=\"outline-%d%s\">\n<h%d id=\"%s\">%s%s</h%d>\n<div class=\"outline-text-%d\" id=\"text-%s\">\n"
			suffix level (if extra-class (concat " " extra-class) "")
			level href
			extra-targets
			title level level suffix))
	(org-open-par)))))

(defun org-export-html-get-tag-class-name (tag)
  "Turn tag into a valid class name.
Replaces invalid characters with \"_\" and then prepends a prefix."
  (save-match-data
    (while (string-match "[^a-zA-Z0-9_]" tag)
      (setq tag (replace-match "_" t t tag))))
  (concat org-export-html-tag-class-prefix tag))

(defun org-export-html-get-todo-kwd-class-name (kwd)
  "Turn todo keyword into a valid class name.
Replaces invalid characters with \"_\" and then prepends a prefix."
  (save-match-data
    (while (string-match "[^a-zA-Z0-9_]" kwd)
      (setq kwd (replace-match "_" t t kwd))))
  (concat org-export-html-todo-kwd-class-prefix kwd))

(defun org-html-level-close (level max-outline-level)
  "Terminate one level in HTML export."
  (if (<= level max-outline-level)
      (insert "</div>\n")
    (org-close-li)
    (insert "</ul>\n")))

(defun org-html-export-list-line (line pos struct prevs)
  "Insert list syntax in export buffer. Return LINE, maybe modified.

POS is the item position or line position the line had before
modifications to buffer. STRUCT is the list structure. PREVS is
the alist of previous items."
  (let* ((get-type
	  (function
	   ;; Translate type of list containing POS to "d", "o" or
	   ;; "u".
	   (lambda (pos struct prevs)
	     (let ((type (org-list-get-list-type pos struct prevs)))
	       (cond
		((eq 'ordered type) "o")
		((eq 'descriptive type) "d")
		(t "u"))))))
	 (get-closings
	  (function
	   ;; Return list of all items and sublists ending at POS, in
	   ;; reverse order.
	   (lambda (pos)
	     (let (out)
	       (catch 'exit
		 (mapc (lambda (e)
			 (let ((end (nth 6 e))
			       (item (car e)))
			   (cond
			    ((= end pos) (push item out))
			    ((>= item pos) (throw 'exit nil)))))
		       struct))
	       out)))))
    ;; First close any previous item, or list, ending at POS.
    (mapc (lambda (e)
	    (let* ((lastp (= (org-list-get-last-item e struct prevs) e))
		   (first-item (org-list-get-list-begin e struct prevs))
		   (type (funcall get-type first-item struct prevs)))
	      (org-close-par-maybe)
	      ;; Ending for every item
	      (org-close-li type)
	      ;; We're ending last item of the list: end list.
	      (when lastp
		(insert (format "</%sl>\n" type))
		(org-open-par))))
	  (funcall get-closings pos))
    (cond
     ;; At an item: insert appropriate tags in export buffer.
     ((assq pos struct)
      (string-match
       (concat "[ \t]*\\(\\S-+[ \t]*\\)"
	       "\\(?:\\[@\\(?:start:\\)?\\([0-9]+\\|[A-Za-z]\\)\\][ \t]*\\)?"
	       "\\(?:\\(\\[[ X-]\\]\\)[ \t]+\\)?"
	       "\\(?:\\(.*\\)[ \t]+::\\(?:[ \t]+\\|$\\)\\)?"
	       "\\(.*\\)") line)
      (let* ((checkbox (match-string 3 line))
	     (desc-tag (or (match-string 4 line) "???"))
	     (body (or (match-string 5 line) ""))
	     (list-beg (org-list-get-list-begin pos struct prevs))
	     (firstp (= list-beg pos))
	     ;; Always refer to first item to determine list type, in
	     ;; case list is ill-formed.
	     (type (funcall get-type list-beg struct prevs))
	     (counter (let ((count-tmp (org-list-get-counter pos struct)))
			(cond
			 ((not count-tmp) nil)
			 ((string-match "[A-Za-z]" count-tmp)
			  (- (string-to-char (upcase count-tmp)) 64))
			 ((string-match "[0-9]+" count-tmp)
			  count-tmp)))))
	(when firstp
	  (org-close-par-maybe)
	  (insert (format "<%sl>\n" type)))
	(insert (cond
		 ((equal type "d")
		  (format "<dt>%s</dt><dd>" desc-tag))
		 ((and (equal type "o") counter)
		  (format "<li value=\"%s\">" counter))
		 (t "<li>")))
	;; If line had a checkbox, some additional modification is required.
	(when checkbox
	  (setq body
		(concat
		 (cond
		  ((string-match "X" checkbox) "<code>[X]</code> ")
		  ((string-match " " checkbox) "<code>[&nbsp;]</code> ")
		  (t "<code>[-]</code> "))
		 body)))
	;; Return modified line
	body))
     ;; At a list ender: go to next line (side-effects only).
     ((equal "ORG-LIST-END-MARKER" line) (throw 'nextline nil))
     ;; Not at an item: return line unchanged (side-effects only).
     (t line))))

(provide 'org-html)

;;; org-html.el ends here
