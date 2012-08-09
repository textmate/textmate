;; idlwave.el --- IDL editing mode for GNU Emacs

;; Copyright (C) 1999-2012  Free Software Foundation, Inc.

;; Authors: J.D. Smith <jdsmith@as.arizona.edu>
;;          Carsten Dominik <dominik@science.uva.nl>
;;          Chris Chase <chase@att.com>
;; Maintainer: J.D. Smith <jdsmith@as.arizona.edu>
;; Version: 6.1.22
;; Keywords: languages

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

;; IDLWAVE enables feature-rich development and interaction with IDL,
;; the Interactive Data Language. It provides a compelling,
;; full-featured alternative to the IDLDE development environment
;; bundled with IDL.

;; In the remotely distant past, based on pascal.el, though bears
;; little resemblance to it now.
;;
;; Incorporates many ideas, such as abbrevs, action routines, and
;; continuation line indenting, from wave.el.
;; wave.el original written by Lubos Pochman, Precision Visuals, Boulder.
;;
;; See the mode description ("C-h m" in idlwave-mode or "C-h f idlwave-mode")
;; for features, key bindings, and info.
;; Also, Info format documentation is available with `M-x idlwave-info'
;;
;; New versions of IDLWAVE, documentation, and more information
;; available from:
;;                 http://idlwave.org
;;
;; INSTALLATION
;; ============
;;
;; Follow the instructions in the INSTALL file of the distribution.
;; In short, put this file on your load path and add the following
;; lines to your .emacs file:
;;
;; (autoload 'idlwave-mode "idlwave" "IDLWAVE Mode" t)
;; (autoload 'idlwave-shell "idlw-shell" "IDLWAVE Shell" t)
;; (setq auto-mode-alist (cons '("\\.pro\\'" . idlwave-mode) auto-mode-alist))
;;
;;
;; SOURCE
;; ======
;;
;; The newest version of this file is available from the maintainer's
;; Webpage:
;;
;;   http://idlwave.org
;;
;; DOCUMENTATION
;; =============
;;
;; IDLWAVE is documented online in info format.  A printable version
;; of the documentation is available from the maintainers webpage (see
;; SOURCE).
;;
;;
;; ACKNOWLEDGMENTS
;; ===============
;;
;;  Thanks to the following people for their contributions and comments:
;;
;;    Ulrik Dickow <dickow_at_nbi.dk>
;;    Eric E. Dors <edors_at_lanl.gov>
;;    Stein Vidar H. Haugan <s.v.h.haugan_at_astro.uio.no>
;;    David Huenemoerder <dph_at_space.mit.edu>
;;    Kevin Ivory <Kevin.Ivory_at_linmpi.mpg.de>
;;    Dick Jackson <dick_at_d-jackson.com>
;;    Xuyong Liu <liu_at_stsci.edu>
;;    Simon Marshall <Simon.Marshall_at_esrin.esa.it>
;;    Laurent Mugnier <mugnier_at_onera.fr>
;;    Lubos Pochman <lubos_at_rsinc.com>
;;    Bob Portmann <portmann_at_al.noaa.gov>
;;    Patrick M. Ryan <pat_at_jaameri.gsfc.nasa.gov>
;;    Marty Ryba <ryba_at_ll.mit.edu>
;;    Paul Sorenson <aardvark62_at_msn.com>
;;    Phil Sterne <sterne_at_dublin.llnl.gov>
;;    Phil Williams <williams_at_irc.chmcc.org>
;;
;; CUSTOMIZATION:
;; =============
;;
;; IDLWAVE has extensive customize support; to learn about the
;; variables which control the mode's behavior, use `M-x
;; idlwave-customize'.
;;
;; You can set your own preferred values with Customize, or with Lisp
;; code in .emacs.  For an example of what to put into .emacs, check
;; the TexInfo documentation or see a complete .emacs available at the
;; website.
;;
;; KNOWN PROBLEMS:
;; ==============
;;
;;   IDLWAVE support for the IDL-derived PV-WAVE CL language of Visual
;;   Numerics, Inc. is growing less and less complete as the two
;;   languages grow increasingly apart.  The mode probably shouldn't
;;   even have "WAVE" in its title, but it's catchy, and was required
;;   to avoid conflict with the CORBA idl.el mode.  Caveat WAVEor.
;;
;;   Moving the point backwards in conjunction with abbrev expansion
;;   does not work as I would like it, but this is a problem with
;;   emacs abbrev expansion done by the self-insert-command.  It ends
;;   up inserting the character that expanded the abbrev after moving
;;   point backward, e.g., "\cl" expanded with a space becomes
;;   "LONG( )" with point before the close paren.  This is solved by
;;   using a temporary function in `post-command-hook' - not pretty,
;;   but it works.
;;
;;   Tabs and spaces are treated equally as whitespace when filling a
;;   comment paragraph.  To accomplish this, tabs are permanently
;;   replaced by spaces in the text surrounding the paragraph, which
;;   may be an undesirable side-effect.  Replacing tabs with spaces is
;;   limited to comments only and occurs only when a comment
;;   paragraph is filled via `idlwave-fill-paragraph'.
;;
;;   Muti-statement lines (using "&") on block begin and end lines can
;;   ruin the formatting.  For example, multiple end statements on a
;;   line: endif & endif.  Using "&" outside of block begin/end lines
;;   should be okay.
;;
;;   Determining the expression at point for printing and other
;;   examination commands is somewhat rough: currently only fairly
;;   simple entities are found.  You can always drag-select or examine
;;   a pre-selected region.
;;
;;   When forcing completion of method keywords, the initial
;;   query for a method has multiple entries for some methods.  Would
;;   be too difficult to fix this hardly used case.
;;

;;; Code:


(eval-when-compile (require 'cl))
(require 'idlw-help)

;; For XEmacs
(unless (fboundp 'line-beginning-position)
  (defalias 'line-beginning-position 'point-at-bol))
(unless (fboundp 'line-end-position)
  (defalias 'line-end-position 'point-at-eol))
(unless (fboundp 'char-valid-p)
  (defalias 'char-valid-p 'characterp))
(unless (fboundp 'match-string-no-properties)
  (defalias 'match-string-no-properties 'match-string))

(if (not (fboundp 'cancel-timer))
    (condition-case nil
	(require 'timer)
      (error nil)))

(declare-function idlwave-shell-get-path-info "idlw-shell")
(declare-function idlwave-shell-temp-file "idlw-shell")
(declare-function idlwave-shell-is-running "idlw-shell")
(declare-function widget-value "wid-edit" (widget))
(declare-function comint-dynamic-complete-filename "comint" ())

(defgroup idlwave nil
  "Major mode for editing IDL .pro files."
  :tag "IDLWAVE"
  :link '(url-link :tag "Home Page"
		   "http://idlwave.org")
  :link '(emacs-commentary-link :tag "Commentary in idlw-shell.el"
				"idlw-shell.el")
  :link '(emacs-commentary-link :tag "Commentary in idlwave.el" "idlwave.el")
  :link '(custom-manual "(idlwave)Top")
  :prefix "idlwave"
  :group 'languages)


;;; Variables for indentation behavior ---------------------------------------

(defgroup idlwave-code-formatting nil
  "Indentation and formatting options for IDLWAVE mode."
  :group 'idlwave)

(defcustom idlwave-main-block-indent 2
  "*Extra indentation for the main block of code.
That is the block between the FUNCTION/PRO statement and the END
statement for that program unit."
  :group 'idlwave-code-formatting
  :type 'integer)

(defcustom idlwave-block-indent 3
  "*Extra indentation applied to block lines.
If you change this, you probably also want to change `idlwave-end-offset'."
  :group 'idlwave-code-formatting
  :type 'integer)

(defcustom idlwave-end-offset -3
  "*Extra indentation applied to block END lines.
A value equal to negative `idlwave-block-indent' will make END lines
line up with the block BEGIN lines."
  :group 'idlwave-code-formatting
  :type 'integer)

(defcustom idlwave-continuation-indent 3
  "*Extra indentation applied to continuation lines.
This extra offset applies to the first of a set of continuation lines.
The following lines receive the same indentation as the first."
  :group 'idlwave-code-formatting
  :type 'integer)

(defcustom idlwave-max-extra-continuation-indent 40
  "*Maximum additional indentation for special continuation indent.
Several special indentations are tried to help line up continuation
lines in routine calls or definitions, other statements with
parentheses, or assignment statements.  This variable specifies a
maximum amount by which this special indentation can exceed the
standard continuation indentation, otherwise defaulting to a fixed
offset.  Set to 0 to effectively disable all special continuation
indentation, or to a large number (like 100) to enable it in all
cases.  See also `idlwave-indent-to-open-paren', which can override
this variable."
  :group 'idlwave-code-formatting
  :type 'integer)

(defcustom idlwave-indent-to-open-paren t
  "*Non-nil means, indent continuation lines to innermost open parenthesis.
This indentation occurs even if otherwise disallowed by
`idlwave-max-extra-continuation-indent'.  Matching parens and the
interleaving args are lined up.  Example:

  x = function_a(function_b(function_c( a, b, [1,2,3, $
                                               4,5,6 $
                                              ], $
                                        c, d $
                                      )))

When this variable is nil, paren alignment may still occur, based on
the value of `idlwave-max-extra-continuation-indent', which, if zero,
would yield:

  x = function_a(function_b(function_c( a, b, [1,2,3, $
     4,5,6 $
     ], $
     c, d $
     )))"
  :group 'idlwave-code-formatting
  :type 'boolean)

(defcustom idlwave-indent-parens-nested nil
  "*Non-nil means, indent continuation lines with parens by nesting
lines at consecutively deeper levels."
 :group 'idlwave-code-formatting
  :type 'boolean)


(defcustom idlwave-hanging-indent t
  "*If set non-nil then comment paragraphs are indented under the
hanging indent given by `idlwave-hang-indent-regexp' match in the first line
of the paragraph."
  :group 'idlwave-code-formatting
  :type 'boolean)

(defcustom idlwave-hang-indent-regexp "- "
  "*Regular expression matching the position of the hanging indent
in the first line of a comment paragraph.  The size of the indent
extends to the end of the match for the regular expression."
  :group 'idlwave-code-formatting
  :type 'regexp)

(defcustom idlwave-use-last-hang-indent nil
  "*If non-nil then use last match on line for `idlwave-indent-regexp'."
  :group 'idlwave-code-formatting
  :type 'boolean)

(defcustom idlwave-fill-comment-line-only t
  "*If non-nil then auto fill will only operate on comment lines."
  :group 'idlwave-code-formatting
  :type 'boolean)

(defcustom idlwave-auto-fill-split-string t
  "*If non-nil then auto fill will split strings with the IDL `+' operator.
When the line end falls within a string, string concatenation with the
'+' operator will be used to distribute a long string over lines.
If nil and a string is split then a terminal beep and warning are issued.

This variable is ignored when `idlwave-fill-comment-line-only' is
non-nil, since in this case code is not auto-filled."
  :group 'idlwave-code-formatting
  :type 'boolean)

(defcustom idlwave-split-line-string t
  "*If non-nil then `idlwave-split-line' will split strings with `+'.
When the splitting point of a line falls inside a string, split the string
using the `+' string concatenation operator.  If nil and a string is
split then a terminal beep and warning are issued."
  :group 'idlwave-code-formatting
  :type 'boolean)

(defcustom idlwave-no-change-comment ";;;"
  "*The indentation of a comment that starts with this regular
expression will not be changed.  Note that the indentation of a comment
at the beginning of a line is never changed."
  :group 'idlwave-code-formatting
  :type 'string)

(defcustom idlwave-begin-line-comment nil
  "*A comment anchored at the beginning of line.
A comment matching this regular expression will not have its
indentation changed.  If nil the default is \"^;\", i.e., any line
beginning with a \";\".  Expressions for comments at the beginning of
the line should begin with \"^\"."
  :group 'idlwave-code-formatting
  :type '(choice (const :tag "Any line beginning with `;'" nil)
		 'regexp))

(defcustom idlwave-code-comment ";;[^;]"
  "*A comment that starts with this regular expression on a line by
itself is indented as if it is a part of IDL code.  As a result if
the comment is not preceded by whitespace it is unchanged."
  :group 'idlwave-code-formatting
  :type 'regexp)

;; Comments not matching any of the above will be indented as a
;; right-margin comment, i.e., to a minimum of `comment-column'.

;;; Routine Info and Completion ---------------------------------------

(defgroup idlwave-routine-info nil
  "Routine Info options for IDLWAVE mode."
  :group 'idlwave)

(defcustom idlwave-use-library-catalogs t
  "*Non-nil means search the IDL path for library catalog files.

These files, named .idlwave_catalog, document routine information for
individual directories and libraries of IDL .pro files.  Many popular
libraries come with catalog files by default, so leaving this on is
usually a good idea."
  :group 'idlwave-routine-info
  :type 'boolean)

(defcustom idlwave-init-rinfo-when-idle-after 10
  "*Seconds of idle time before routine info is automatically initialized.
Initializing the routine info can take a long time, in particular if a
large number of library catalogs are involved.  When Emacs is idle for
more than the number of seconds specified by this variable, it starts
the initialization.  The process is split into five steps, in order to
keep work interruption as short as possible.  If one of the steps
finishes, and no user input has arrived in the mean time, initialization
proceeds immediately to the next step.  A good value for this variable
is about 1/3 of the time initialization take in your setup.  So if you
have a fast machine and no problems with a slow network connection,
don't hesitate to set this to 2 seconds.  A value of 0 means, don't
initialize automatically, but instead wait until routine information is
needed, and initialize then."
  :group 'idlwave-routine-info
  :type 'number)

(defcustom idlwave-scan-all-buffers-for-routine-info t
  "*Non-nil means, scan buffers for IDL programs when updating info.
The scanning is done by the command `idlwave-update-routine-info'.
The following values are allowed:

nil       Don't scan any buffers.
t         Scan all `idlwave-mode' buffers in the current editing session.
current   Scan only the current buffer, but no other buffers."
  :group 'idlwave-routine-info
  :type '(choice
	  (const :tag "No buffer" nil)
	  (const :tag "All buffers" t)
	  (const :tag "Current buffer only" 'current)))

(defcustom idlwave-query-shell-for-routine-info t
  "*Non-nil means query the shell for info about compiled routines.
Querying the shell is useful to get information about compiled modules,
and it is turned on by default.  However, when you have a complete library
scan, this is not necessary."
  :group 'idlwave-routine-info
  :type 'boolean)

(defcustom idlwave-auto-routine-info-updates
  '(find-file save-buffer kill-buffer compile-buffer)
  "*Controls under what circumstances routine info is updated automatically.
Possible values:
nil       Never
t         All available
\(...)     A list of circumstances.  Allowed members are:
           find-file       Add info for new IDLWAVE buffers.
           save-buffer     Update buffer info when buffer is saved
           kill-buffer     Remove buffer info when buffer gets killed
           compile-buffer  Update shell info after `idlwave-shell-save-and...'"
  :group 'idlwave-routine-info
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "As often as possible" t)
	  (set :tag "Checklist" :greedy t
	       (const :tag "When visiting a file" find-file)
	       (const :tag "When saving a buffer" save-buffer)
	       (const :tag "After a buffer was killed" kill-buffer)
	       (const :tag "After a buffer was compiled successfully, update shell info" compile-buffer))))

(defcustom idlwave-rinfo-max-source-lines 5
  "*Maximum number of source files displayed in the Routine Info window.
When an integer, it is the maximum number of source files displayed.
A value of t means to show all source files."
  :group 'idlwave-routine-info
  :type 'integer)

(defcustom idlwave-library-path nil
  "Library path for Windows and MacOS (OS9).  Not needed under UNIX.
When selecting the directories to scan for IDL user catalog routine
info, IDLWAVE can, under UNIX, query the shell for the exact search
path \(the value of !PATH).  However, under Windows and MacOS
\(pre-OSX), the IDLWAVE shell does not work.  In this case, this
variable can be set to specify the paths where IDLWAVE can find PRO
files.  The shell will only be asked for a list of paths when this
variable is nil.  The value is a list of directories.  A directory
preceded by a `+' will be searched recursively.  If you set this
variable on a UNIX system, the shell will not be queried.  See also
`idlwave-system-directory'."
  :group 'idlwave-routine-info
  :type '(repeat (directory)))

(defcustom idlwave-system-directory ""
  "The IDL system directory for Windows and MacOS.  Not needed under
UNIX.  Set this to the value of the `!DIR' system variable in IDL.
IDLWAVE uses this to find out which of the library routines belong to
the official system library.  All files inside the `lib' subdirectory
are considered system library files - so don't install private stuff
in this directory.  On UNIX systems, IDLWAVE queries the shell for the
value of `!DIR'.  See also `idlwave-library-path'."
  :group 'idlwave-routine-info
  :type 'directory)

;; Configuration files
(defcustom idlwave-config-directory
  (convert-standard-filename "~/.idlwave")
  "*Directory for configuration files and user-library catalog."
  :group 'idlwave-routine-info
  :type 'file)

(defvar idlwave-user-catalog-file "idlusercat.el")
(defvar idlwave-xml-system-rinfo-converted-file "idl_xml_rinfo.el")
(defvar idlwave-path-file "idlpath.el")

(defvar idlwave-libinfo-file nil
  "*Obsolete variable, no longer used.")

(defcustom idlwave-special-lib-alist nil
  "Alist of regular expressions matching special library directories.
When listing routine source locations, IDLWAVE gives a short hint where
the file defining the routine is located.  By default it lists `SystemLib'
for routines in the system library `!DIR/lib' and `Library' for anything
else.  This variable can define additional types.  The car of each entry
is a regular expression matching the file name (they normally will match
on the path).  The cdr is the string to be used as identifier.  Max 10
chars are allowed."
  :group 'idlwave-routine-info
  :type '(repeat
	  (cons regexp string)))

(defcustom idlwave-auto-write-paths t
  "Write out path (!PATH) and system directory (!DIR) info automatically.
Path info is needed to locate library catalog files.  If non-nil,
whenever the path-list changes as a result of shell-query, etc., it is
written to file.  Otherwise, the menu option \"Write Paths\" can be
used to force a write."
  :group 'idlwave-routine-info
  :type 'boolean)

(defgroup idlwave-completion nil
  "Completion options for IDLWAVE mode."
  :prefix "idlwave"
  :group 'idlwave)

(eval-and-compile
  (defconst idlwave-tmp
    '(choice :tag "by applying the function"
      (const upcase)
      (const downcase)
      (const capitalize)
      (const preserve)
      (symbol :tag "Other"))))

(defcustom idlwave-completion-case '((routine . upcase)
				     (keyword . upcase)
				     (class   . preserve)
				     (method  . preserve))
  "Association list setting the case of completed words.

This variable determines the case (UPPER/lower/Capitalized...) of
words inserted into the buffer by completion.  The preferred case can
be specified separately for routine names, keywords, classes and
methods.
This alist should therefore have entries for `routine' (normal
functions and procedures, i.e. non-methods), `keyword', `class', and
`method'.  Plausible values are

upcase      upcase whole word, like `BOX_CURSOR'
downcase    downcase whole word, like `read_ppm'
capitalize  capitalize each part, like `Widget_Control'
preserve    preserve case as is, like `IDLgrView'

The value can also be any Emacs Lisp function which transforms the
case of characters in a string.

A value of `preserve' means that the case of the completed word is
identical to the way it was written in the definition statement of the
routine.  This was implemented to allow for mixed-case completion, in
particular of object classes and methods.
If a completable word is defined in multiple locations, the meaning of
`preserve' is not unique since the different definitions might be
cased differently.  Therefore IDLWAVE always takes the case of the
*first* definition it encounters during routine info collection and
uses the case derived from it consistently.

Note that a lowercase-only string in the buffer will always be completed in
lower case (but see the variable `idlwave-completion-force-default-case').

After changing this variable, you need to either restart Emacs or press
`C-u C-c C-i' to update the internal lists."
  :group 'idlwave-completion
  :type `(repeat
	  (cons (symbol :tag "Derive completion case for")
		,idlwave-tmp)))

(defcustom idlwave-completion-force-default-case nil
  "*Non-nil means, completion will always honor `idlwave-completion-case'.
When nil, only the completion of a mixed case or upper case string
will honor the default settings in `idlwave-completion-case', while
the completion of lower case strings will be completed entirely in
lower case."
  :group 'idlwave-completion
  :type 'boolean)

(defcustom idlwave-complete-empty-string-as-lower-case nil
  "*Non-nil means, the empty string is considered downcase for completion.
The case of what is already in the buffer determines the case of completions.
When this variable is non-nil, the empty string is considered to be downcase.
Completing on the empty string then offers downcase versions of the possible
completions."
  :group 'idlwave-completion
  :type 'boolean)

(defvar idlwave-default-completion-case-is-down nil
  "Obsolete variable.  See `idlwave-complete-empty-string-as-lower-case' and
`idlwave-completion-case'.")

(defcustom idlwave-buffer-case-takes-precedence nil
  "*Non-nil means, the case of tokens in buffers dominates over system stuff.
To make this possible, we need to re-case everything each time we update
the routine info from the buffers.  This is slow.
The default is to consider the case given in the system and library files
first which makes updating much faster."
  :group 'idlwave-completion
  :type 'boolean)

(defcustom idlwave-highlight-help-links-in-completion t
  "*Non-nil means, highlight completions for which system help is available.
Help can then be accessed with mouse-3.
This option is only effective when the online help system is installed."
  :group 'idlwave-completion
  :type 'boolean)

(defcustom idlwave-support-inheritance t
  "Non-nil means, treat inheritance with completion, online help etc.
When nil, IDLWAVE only knows about the native methods and tags of a class,
not about inherited ones."
  :group 'idlwave-routine-info
  :type 'boolean)

(defcustom idlwave-keyword-class-inheritance '("^[gs]etproperty$" "^init$")
  "List of regular expressions for class-driven keyword inheritance.
Keyword inheritance is often tied to class inheritance by \"chaining\"
up the class tree.  While it cannot be assumed that the presence of an
_EXTRA or _REF_EXTRA symbol guarantees such chaining will occur, for
certain methods this assumption is almost always true.  The methods
for which to assume this can be set here."
  :group 'idlwave-routine-info
  :type '(repeat (regexp :tag "Match method:")))


(defcustom idlwave-completion-show-classes 1
  "*Number of classes to show when completing object methods and keywords.
When completing methods or keywords for an object with unknown class,
the *Completions* buffer will show the valid classes for each completion
like this:

MyMethod <Class1,Class2,Class3>

The value of this variable may be nil to inhibit display, or an integer to
indicate the maximum number of classes to display.

On XEmacs, a full list of classes will also be placed into a `help-echo'
property on the completion items, so that the list of classes for the current
item is displayed in the echo area.  If the value of this variable is a
negative integer, the `help-echo' property will be suppressed."
  :group 'idlwave-completion
  :type '(choice (const :tag "Don't show" nil)
		 (integer :tag "Number of classes shown" 1)))

(defcustom idlwave-completion-fontify-classes t
  "*Non-nil means, fontify the classes in completions buffer.
This makes it easier to distinguish the completion items from the extra
class info listed.  See `idlwave-completion-show-classes'."
  :group 'idlwave-completion
  :type 'boolean)

(defcustom idlwave-query-class '((method-default . nil)
				 (keyword-default . nil))
  "Association list governing specification of object classes for completion.

When IDLWAVE tries to complete object-oriented methods, it usually
cannot determine the class of a given object from context.  In order
to provide the user with a correct list of methods or keywords, it
needs to determine the appropriate class.  IDLWAVE has two ways of
doing this (well, three ways if you count the shell... see
`idlwave-shell-query-for-class'):

1. Combine the items of all available classes which contain this
   method for the purpose of completion.  So when completing a method,
   all methods of all known classes are available, and when completing
   a keyword, all keywords allowed for this method in any class are
   shown.  This behavior is very much like normal completion and is
   therefore the default.  It works much better than one might think -
   only for the INIT, GETPROPERTY and SETPROPERTY the keyword lists
   become uncomfortably long.  See also
   `idlwave-completion-show-classes'.

2. The second possibility is to ask the user on each occasion.  To
   make this less interruptive, IDLWAVE can store the class as a text
   property on the object operator `->'.  For a given object in the
   source code, class selection will then be needed only once
   - for example to complete the method.  Keywords to the method can
   then be completed directly, because the class is already known.
   You will have to turn on the storage of the selected class
   explicitly with the variable `idlwave-store-inquired-class'.

This variable allows you to configure IDLWAVE's method and
method-keyword completion behavior.  Its value is an alist, which
should contain at least two elements: (method-default . VALUE) and
\(keyword-default . VALUE), where VALUE is either t or nil.  These
specify if the class should be found during method and keyword
completion, respectively.

The alist may have additional entries specifying exceptions from the
keyword completion rule for specific methods, like INIT or
GETPROPERTY.  In order to turn on class specification for the INIT
method, add an entry (\"INIT\" . t).  The method name must be ALL-CAPS."
  :group 'idlwave-completion
  :type '(list
	  (cons (const method-default)
		(boolean :tag "Determine class when completing METHODS    "))
	  (cons (const keyword-default)
		(boolean :tag "Determine class when completing KEYWORDS   "))
	  (repeat
	   :tag "Exceptions to defaults"
	   :inline t
	   (cons (string  :tag "MODULE" :value "")
		 (boolean :tag "Determine class for this method")))))

(defcustom idlwave-store-inquired-class t
  "*Non-nil means, store class of a method call as text property on `->'.
IDLWAVE sometimes has to ask the user for the class associated with a
particular object method call.  This happens during the commands
`idlwave-routine-info' and `idlwave-complete', depending upon the
value of the variable `idlwave-query-class'.

When you specify a class, this information can be stored as a text
property on the `->' arrow in the source code, so that during the same
editing session, IDLWAVE will not have to ask again.  When this
variable is non-nil, IDLWAVE will store and reuse the class information.
The class stored can be checked and removed with `\\[idlwave-routine-info]'
on the arrow.

The default of this variable is nil, since the result of commands then
is more predictable.  However, if you know what you are doing, it can
be nice to turn this on.

An arrow which knows the class will be highlighted with
`idlwave-class-arrow-face'.  The command \\[idlwave-routine-info]
displays (with prefix arg: deletes) the class stored on the arrow
at point."
  :group 'idlwave-completion
  :type 'boolean)

(defcustom idlwave-class-arrow-face 'bold
  "*Face to highlight object operator arrows `->' which carry a class property.
When IDLWAVE stores a class name as text property on an object arrow
\(see variable `idlwave-store-inquired-class', it highlights the arrow
with this font in order to remind the user that this arrow is special."
  :group 'idlwave-completion
  :type 'symbol)

(defcustom idlwave-resize-routine-help-window t
  "*Non-nil means, resize the Routine-info *Help* window to fit the content."
  :group 'idlwave-completion
  :type 'boolean)

(defcustom idlwave-keyword-completion-adds-equal t
  "*Non-nil means, completion automatically adds `=' after completed keywords."
  :group 'idlwave-completion
  :type 'boolean)

(defcustom idlwave-function-completion-adds-paren t
  "*Non-nil means, completion automatically adds `(' after completed function.
nil means, don't add anything.
A value of `2' means, also add the closing parenthesis and position cursor
between the two."
  :group 'idlwave-completion
  :type '(choice (const :tag "Nothing" nil)
		 (const :tag "(" t)
		 (const :tag "()" 2)))

(defcustom idlwave-completion-restore-window-configuration t
  "*Non-nil means, try to restore the window configuration after completion.
When completion is not unique, Emacs displays a list of completions.
This messes up your window configuration.  With this variable set, IDLWAVE
restores the old configuration after successful completion."
  :group 'idlwave-completion
  :type 'boolean)

;;; Variables for abbrev and action behavior -----------------------------

(defgroup idlwave-abbrev-and-indent-action nil
  "IDLWAVE performs actions when expanding abbreviations or indenting lines.
The variables in this group govern this."
  :group 'idlwave)

(defcustom idlwave-do-actions nil
  "*Non-nil means performs actions when indenting.
The actions that can be performed are listed in `idlwave-indent-action-table'."
  :group 'idlwave-abbrev-and-indent-action
  :type 'boolean)

(defcustom idlwave-abbrev-start-char "\\"
  "*A single character string used to start abbreviations in abbrev mode.
Possible characters to chose from: ~`\%
or even '?'.  '.' is not a good choice because it can make structure
field names act like abbrevs in certain circumstances.

Changes to this in `idlwave-mode-hook' will have no effect.  Instead a user
must set it directly using `setq' in the .emacs file before idlwave.el
is loaded."
  :group 'idlwave-abbrev-and-indent-action
  :type 'string)

(defcustom idlwave-surround-by-blank nil
  "*Non-nil means, enable `idlwave-surround'.
If non-nil, `=',`<',`>',`&',`,', `->' are surrounded with spaces by
`idlwave-surround'.
See help for `idlwave-indent-action-table' for symbols using `idlwave-surround'.

Also see the default key bindings for keys using `idlwave-surround'.
Keys are bound and made into actions calling `idlwave-surround' with
`idlwave-action-and-binding'.
See help for `idlwave-action-and-binding' for examples.

Also see help for `idlwave-surround'."
  :group 'idlwave-abbrev-and-indent-action
  :type 'boolean)

(defcustom idlwave-pad-keyword t
  "*Non-nil means pad '=' in keywords (routine calls or defs) like assignment.
Whenever `idlwave-surround' is non-nil then this affects how '=' is
padded for keywords and for variables.  If t, pad the same as for
assignments.  If nil then spaces are removed.  With any other value,
spaces are left unchanged."
  :group 'idlwave-abbrev-and-indent-action
  :type '(choice
	  (const :tag "Pad like assignments" t)
	  (const :tag "Remove space near `='" nil)
	  (const :tag "Keep space near `='" 'keep)))

(defcustom idlwave-show-block t
  "*Non-nil means point blinks to block beginning for `idlwave-show-begin'."
  :group 'idlwave-abbrev-and-indent-action
  :type 'boolean)

(defcustom idlwave-expand-generic-end nil
  "*Non-nil means expand generic END to ENDIF/ENDELSE/ENDWHILE etc."
  :group 'idlwave-abbrev-and-indent-action
  :type 'boolean)

(defcustom idlwave-reindent-end t
  "*Non-nil means re-indent line after END was typed."
  :group 'idlwave-abbrev-and-indent-action
  :type 'boolean)

(defcustom idlwave-abbrev-move t
  "*Non-nil means the abbrev hook can move point.
Set to nil by `idlwave-expand-region-abbrevs'.  To see the abbrev
definitions, use the command `list-abbrevs', for abbrevs that move
point.  Moving point is useful, for example, to place point between
parentheses of expanded functions.

See `idlwave-check-abbrev'."
  :group 'idlwave-abbrev-and-indent-action
  :type 'boolean)

(defcustom idlwave-abbrev-change-case nil
  "*Non-nil means all abbrevs will be forced to either upper or lower case.
If the value t, all expanded abbrevs will be upper case.
If the value is 'down then abbrevs will be forced to lower case.
If nil, the case will not change.
If `idlwave-reserved-word-upcase' is non-nil, reserved words will always be
upper case, regardless of this variable."
  :group 'idlwave-abbrev-and-indent-action
  :type 'boolean)

(defcustom idlwave-reserved-word-upcase nil
  "*Non-nil means, reserved words will be made upper case via abbrev expansion.
If nil case of reserved words is controlled by `idlwave-abbrev-change-case'.
Has effect only if in abbrev-mode."
  :group 'idlwave-abbrev-and-indent-action
  :type 'boolean)

;;; Action/Expand Tables.
;;
;; The average user may have difficulty modifying this directly.  It
;; can be modified/set in idlwave-mode-hook, but it is easier to use
;; idlwave-action-and-binding. See help for idlwave-action-and-binding for
;; examples of how to add an action.
;;
;; The action table is used by `idlwave-indent-line' whereas both the
;; action and expand tables are used by `idlwave-indent-and-action'.  In
;; general, the expand table is only used when a line is explicitly
;; indented.  Whereas, in addition to being used when the expand table
;; is used, the action table is used when a line is indirectly
;; indented via line splitting, auto-filling or a new line creation.
;;
;; Example actions:
;;
;;  Capitalize system vars
;;   (idlwave-action-and-binding idlwave-sysvar '(capitalize-word 1) t)
;;
;;  Capitalize procedure name
;;   (idlwave-action-and-binding "\\<\\(pro\\|function\\)\\>[ \t]*\\<"
;;                           '(capitalize-word 1) t)
;;
;;  Capitalize common block name
;;   (idlwave-action-and-binding "\\<common\\>[ \t]+\\<"
;;                           '(capitalize-word 1) t)
;;  Capitalize label
;;   (idlwave-action-and-binding (concat "^[ \t]*" idlwave-label)
;;                           '(capitalize-word -1) t)

(defvar idlwave-indent-action-table nil
  "*Associated array containing action lists of search string (car),
and function as a cdr.  This table is used by `idlwave-indent-line'.
See documentation for `idlwave-do-action' for a complete description of
the action lists.

Additions to the table are made with `idlwave-action-and-binding' when a
binding is not requested.
See help on `idlwave-action-and-binding' for examples.")

(defvar idlwave-indent-expand-table nil
  "*Associated array containing action lists of search string (car),
and function as a cdr.  The table is used by the
`idlwave-indent-and-action' function.  See documentation for
`idlwave-do-action' for a complete description of the action lists.

Additions to the table are made with `idlwave-action-and-binding' when a
binding is requested.
See help on `idlwave-action-and-binding' for examples.")

;;; Documentation header and history keyword ---------------------------------

(defgroup idlwave-documentation nil
  "Options for documenting IDLWAVE files."
  :group 'idlwave)

;; FIXME: make defcustom?
(defvar idlwave-file-header
  (list nil
        ";+
; NAME:
;
;
;
; PURPOSE:
;
;
;
; CATEGORY:
;
;
;
; CALLING SEQUENCE:
;
;
;
; INPUTS:
;
;
;
; OPTIONAL INPUTS:
;
;
;
; KEYWORD PARAMETERS:
;
;
;
; OUTPUTS:
;
;
;
; OPTIONAL OUTPUTS:
;
;
;
; COMMON BLOCKS:
;
;
;
; SIDE EFFECTS:
;
;
;
; RESTRICTIONS:
;
;
;
; PROCEDURE:
;
;
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;
;-
")
  "*A list (PATHNAME STRING) specifying the doc-header template to use for
summarizing a file.  If PATHNAME is non-nil then this file will be included.
Otherwise STRING is used.  If nil, the file summary will be omitted.
For example you might set PATHNAME to the path for the
lib_template.pro file included in the IDL distribution.")

(defcustom idlwave-header-to-beginning-of-file t
  "*Non-nil means, the documentation header will always be at start of file.
When nil, the header is positioned between the PRO/FUNCTION line of
the current routine and the code, allowing several routine headers in
a file."
  :group 'idlwave-documentation
  :type 'boolean)

(defcustom idlwave-timestamp-hook 'idlwave-default-insert-timestamp
  "*The hook function used to update the timestamp of a function."
  :group 'idlwave-documentation
  :type 'function)

(defcustom idlwave-doc-modifications-keyword "HISTORY"
  "*The modifications keyword to use with the log documentation commands.
A ':' is added to the keyword end.
Inserted by doc-header and used to position logs by doc-modification.
If nil it will not be inserted."
  :group 'idlwave-documentation
  :type 'string)

(defcustom idlwave-doclib-start "^;+\\+"
  "*Regexp matching the start of a document library header."
  :group 'idlwave-documentation
  :type 'regexp)

(defcustom idlwave-doclib-end "^;+-"
  "*Regexp matching the end of a document library header."
  :group 'idlwave-documentation
  :type 'regexp)

;;; External Programs -------------------------------------------------------

(defgroup idlwave-external-programs nil
  "Path locations of external commands used by IDLWAVE."
  :group 'idlwave)

(defcustom idlwave-shell-explicit-file-name "idl"
  "*If non-nil, this is the command to run IDL.
Should be an absolute file path or path relative to the current environment
execution search path.  If you want to specify command line switches
for the IDL program, use `idlwave-shell-command-line-options'.

I know the name of this variable is badly chosen, but I cannot change
it without compromising backwards-compatibility."
  :group 'idlwave-external-programs
  :type 'string)

(defcustom idlwave-shell-command-line-options nil
  "*A list of command line options for calling the IDL program.
Since IDL is executed directly without going through a shell like /bin/sh,
this should be a list of strings like '(\"-rt=file\" \"-nw\") with a separate
string for each argument.  But you may also give a single string which
contains the options whitespace-separated.  Emacs will be kind enough to
split it for you."
  :type '(choice
	  string
	  (repeat (string :value "")))
  :group 'idlwave-external-programs)

(defcustom idlwave-help-application "idlhelp"
  "*The external application providing reference help for programming.
Obsolete, if the IDL Assistant is being used for help."
  :group 'idlwave-external-programs
  :type 'string)

;;; Some Shell variables which must be defined here.-----------------------

(defcustom idlwave-shell-debug-modifiers '()
  "List of modifiers to be used for the debugging commands.
Will be used to bind debugging commands in the shell buffer and in all
source buffers.  These are additional convenience bindings, the debugging
commands are always available with the `C-c C-d' prefix.
If you set this to '(control shift), this means setting a breakpoint will
be on `C-S-b', compiling a source file on `C-S-c' etc.  Possible modifiers
are `control', `meta', `super', `hyper', `alt', and `shift'."
  :group 'idlwave-shell-general-setup
  :type '(set :tag "Specify modifiers"
	       (const control)
	       (const meta)
	       (const super)
	       (const hyper)
	       (const alt)
	       (const shift)))

(defcustom idlwave-shell-automatic-start nil
  "*If non-nil attempt invoke `idlwave-shell' if not already running.
This is checked when an attempt to send a command to an
IDL process is made."
  :group 'idlwave-shell-general-setup
  :type 'boolean)

;;; Miscellaneous variables -------------------------------------------------

(defgroup idlwave-misc nil
  "Miscellaneous options for IDLWAVE mode."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'idlwave)

(defcustom idlwave-startup-message t
  "*Non-nil displays a startup message when `idlwave-mode' is first called."
  :group 'idlwave-misc
  :type 'boolean)

(defcustom idlwave-default-font-lock-items
  '(pros-and-functions batch-files idlwave-idl-keywords label goto
		       common-blocks class-arrows)
  "Items which should be fontified on the default fontification level 2.
IDLWAVE defines 3 levels of fontification.  Level 1 is very little, level 3
is everything and level 2 is specified by this list.
This variable must be set before IDLWAVE gets loaded.
It is a list of symbols; the following symbols are allowed:

pros-and-functions   Procedure and Function definitions
batch-files          Batch Files
idlwave-idl-keywords IDL Keywords
label                Statement Labels
goto                 Goto Statements
common-blocks        Common Blocks
keyword-parameters   Keyword Parameters in routine definitions and calls
system-variables     System Variables
fixme                FIXME: Warning in comments (on XEmacs only v. 21.0 and up)
class-arrows         Object Arrows with class property"
  :group 'idlwave-misc
  :type '(set
	  :inline t :greedy t
	  (const :tag "Procedure and Function definitions" pros-and-functions)
	  (const :tag "Batch Files"                       batch-files)
	  (const :tag "IDL Keywords (reserved words)"     idlwave-idl-keywords)
	  (const :tag "Statement Labels"                  label)
	  (const :tag "Goto Statements"                   goto)
	  (const :tag "Tags in Structure Definition"      structtag)
	  (const :tag "Structure Name"                    structname)
	  (const :tag "Common Blocks"                     common-blocks)
	  (const :tag "Keyword Parameters"                keyword-parameters)
	  (const :tag "System Variables"                  system-variables)
	  (const :tag "FIXME: Warning"                    fixme)
	  (const :tag "Object Arrows with class property " class-arrows)))

(defcustom idlwave-mode-hook nil
  "Normal hook.  Executed when a buffer is put into `idlwave-mode'."
  :group 'idlwave-misc
  :type 'hook)

(defcustom idlwave-load-hook nil
  "Normal hook.  Executed when idlwave.el is loaded."
  :group 'idlwave-misc
  :type 'hook)

(defvar idlwave-experimental nil
  "Non-nil means turn on a few experimental features.
This variable is only for the maintainer, to test difficult stuff,
while still distributing stable releases.
As a user, you should not set this to t.")

;;;
;;; End customization variables section
;;;

;;; Non customization variables

;;; font-lock mode - Additions by Phil Williams, Ulrik Dickow and
;;; Simon Marshall <simon_at_gnu.ai.mit.edu>
;;; and Carsten Dominik...

;; The following are the reserved words in IDL.  Maybe we should
;; highlight some more stuff as well?
;; Procedure declarations.  Fontify keyword plus procedure name.
(defvar idlwave-idl-keywords
  ;; To update this regexp, update the list of keywords and
  ;; evaluate the form.
  ;;	(insert
  ;;	 (prin1-to-string
  ;;	  (concat
  ;;	   "\\<\\("
  ;;	   (regexp-opt
  ;;	    '("||" "&&" "and" "or" "xor" "not"
  ;;	      "eq" "ge" "gt" "le" "lt" "ne"
  ;;	      "for" "do" "endfor"
  ;;	      "if" "then" "endif" "else" "endelse"
  ;;	      "case" "of" "endcase"
  ;;	      "switch" "break" "continue" "endswitch"
  ;;	      "begin" "end"
  ;;	      "repeat" "until" "endrep"
  ;;	      "while" "endwhile"
  ;;	      "goto" "return"
  ;;	      "inherits" "mod"
  ;;	      "compile_opt" "forward_function"
  ;;	      "on_error" "on_ioerror"))  ; on_error is not officially reserved
  ;;	   "\\)\\>")))
  "\\<\\(&&\\|and\\|b\\(egin\\|reak\\)\\|c\\(ase\\|o\\(mpile_opt\\|ntinue\\)\\)\\|do\\|e\\(lse\\|nd\\(case\\|else\\|for\\|if\\|rep\\|switch\\|while\\)?\\|q\\)\\|for\\(ward_function\\)?\\|g\\(oto\\|[et]\\)\\|i\\(f\\|nherits\\)\\|l[et]\\|mod\\|n\\(e\\|ot\\)\\|o\\(n_\\(error\\|ioerror\\)\\|[fr]\\)\\|re\\(peat\\|turn\\)\\|switch\\|then\\|until\\|while\\|xor\\|||\\)\\>")


(let* (;; Procedure declarations.  Fontify keyword plus procedure name.
       ;; Function  declarations.  Fontify keyword plus function  name.
       (pros-and-functions
	'("\\<\\(function\\|pro\\)\\>[ \t]+\\(\\sw+\\(::\\sw+\\)?\\)"
	  (1 font-lock-keyword-face)
	  (2 font-lock-function-name-face nil t)))

       ;; Common blocks
       (common-blocks
	'("\\<\\(common\\)\\>[ \t]*\\(\\sw+\\)?[ \t]*,?"
	  (1 font-lock-keyword-face)	          ; "common"
	  (2 font-lock-reference-face nil t)      ; block name
	  ("[ \t]*\\(\\sw+\\)[ ,]*"
	   ;; Start with point after block name and comma
	   (goto-char (match-end 0))  ; needed for XEmacs, could be nil
	   nil
	   (1 font-lock-variable-name-face)       ; variable names
	   )))

       ;; Batch files
       (batch-files
	'("^[ \t]*\\(@[^ \t\n]+\\)" (1 font-lock-string-face)))

       ;; FIXME warning.
       (fixme
	'("\\<FIXME:" (0 font-lock-warning-face t)))

       ;; Labels
       (label
	'("^[ \t]*\\([a-zA-Z]\\sw*:\\)" (1 font-lock-reference-face)))

       ;; The goto statement and its label
       (goto
	'("\\(goto\\)[ \t]*,[ \t]*\\([a-zA-Z]\\sw*\\)"
	  (1 font-lock-keyword-face)
	  (2 font-lock-reference-face)))

       ;; Tags in structure definitions.  Note that this definition
       ;; actually collides with labels, so we have to use the same
       ;; face.  It also matches named subscript ranges,
       ;; e.g. vec{bottom:top].  No good way around this.
       (structtag
	'("\\<\\([a-zA-Z][a-zA-Z0-9_]*:\\)[^:]" (1 font-lock-reference-face)))

       ;; Structure names
       (structname
	'("\\({\\|\\<inherits\\s-\\)\\s-*\\([a-zA-Z][a-zA-Z0-9_]*\\)[},\t \n]"
	  (2 font-lock-function-name-face)))

       ;; Keyword parameters, like /xlog or ,xrange=[]
       ;; This is anchored to the comma preceding the keyword.
       ;; Treats continuation lines, works only during whole buffer
       ;; fontification.  Slow, use it only in fancy fontification.
       (keyword-parameters
	'("\\(,\\|[a-zA-Z0-9_](\\)[ \t]*\\(\\$[ \t]*\\(;.*\\)?\n\\([ \t]*\\(;.*\\)?\n\\)*[ \t]*\\)?\\(/[a-zA-Z_]\\sw*\\|[a-zA-Z_]\\sw*[ \t]*=\\)"
	  (6 font-lock-reference-face)))

       ;; System variables start with a bang.
       (system-variables
	'("\\(![a-zA-Z_0-9]+\\(\\.\\sw+\\)?\\)"
	  (1 font-lock-variable-name-face)))

       ;; Special and unusual operators (not used because too noisy)
       ;; (special-operators
       ;;  '("[<>#]" (0 font-lock-keyword-face)))

       ;; All operators (not used because too noisy)
       ;; (all-operators
       ;;  '("[-*^#+<>/]" (0 font-lock-keyword-face)))

       ;; Arrows with text property `idlwave-class'
       (class-arrows
	'(idlwave-match-class-arrows (0 idlwave-class-arrow-face))))

  (defconst idlwave-font-lock-keywords-1
    (list pros-and-functions batch-files)
    "Subdued level highlighting for IDLWAVE mode.")

  (defconst idlwave-font-lock-keywords-2
    (mapcar 'symbol-value idlwave-default-font-lock-items)
    "Medium level highlighting for IDLWAVE mode.")

  (defconst idlwave-font-lock-keywords-3
	(list pros-and-functions
	      batch-files
	      idlwave-idl-keywords
	      label goto
	      structtag
	      structname
	      common-blocks
	      keyword-parameters
	      system-variables
	  class-arrows)
    "Gaudy level highlighting for IDLWAVE mode."))

(defun idlwave-match-class-arrows (limit)
  ;; Match an object arrow with class property
  (and idlwave-store-inquired-class
       (re-search-forward "->" limit 'limit)
       (get-text-property (match-beginning 0) 'idlwave-class)))

(defvar idlwave-font-lock-keywords idlwave-font-lock-keywords-2
  "Default expressions to highlight in IDLWAVE mode.")

(defvar idlwave-font-lock-defaults
  '((idlwave-font-lock-keywords
     idlwave-font-lock-keywords-1
     idlwave-font-lock-keywords-2
     idlwave-font-lock-keywords-3)
    nil t
    ((?$ . "w") (?_ . "w") (?. . "w") (?| . "w") (?& . "w"))
    beginning-of-line))

(put 'idlwave-mode 'font-lock-defaults
     idlwave-font-lock-defaults) ; XEmacs

(defconst idlwave-comment-line-start-skip "^[ \t]*;"
  "Regexp to match the start of a full-line comment.
That is the _beginning_ of a line containing a comment delimiter `;' preceded
only by whitespace.")

(defconst idlwave-begin-block-reg
  "\\<\\(pro\\|function\\|begin\\|case\\|switch\\)\\>"
  "Regular expression to find the beginning of a block.
The case does not matter.  The search skips matches in comments.")

(defconst idlwave-begin-unit-reg "^\\s-*\\(pro\\|function\\)\\>\\|\\`"
  "Regular expression to find the beginning of a unit.
The case does not matter.")

(defconst idlwave-end-unit-reg "^\\s-*\\(pro\\|function\\)\\>\\|\\'"
  "Regular expression to find the line that indicates the end of unit.
This line is the end of buffer or the start of another unit.
The case does not matter.  The search skips matches in comments.")

(defconst idlwave-continue-line-reg "\\<\\$"
  "Regular expression to match a continued line.")

(defconst idlwave-end-block-reg
  "\\<end\\(\\|case\\|switch\\|else\\|for\\|if\\|rep\\|while\\)\\>"
  "Regular expression to find the end of a block.
The case does not matter.  The search skips matches in comments.")

(defconst idlwave-block-matches
  '(("pro"      . "end")
    ("function" . "end")
    ("case"     . "endcase")
    ("else"     . "endelse")
    ("for"      . "endfor")
    ("then"     . "endif")
    ("repeat"   . "endrep")
    ("switch"   . "endswitch")
    ("while"    . "endwhile"))
  "Matches between statements and the corresponding END variant.
The cars are the reserved words starting a block.  If the block really
begins with BEGIN, the cars are the reserved words before the begin
which can be used to identify the block type.
This is used to check for the correct END type, to close blocks and
to expand generic end statements to their detailed form.")

(defconst idlwave-block-match-regexp
  "\\<\\(else\\|for\\|then\\|repeat\\|while\\)\\>"
"Regular expression matching reserved words which can stand before
blocks starting with a BEGIN statement.  The matches must have associations
`idlwave-block-matches'.")

(defconst idlwave-identifier "[a-zA-Z_][a-zA-Z0-9$_]*"
  "Regular expression matching an IDL identifier.")

(defconst idlwave-sysvar (concat "!" idlwave-identifier)
  "Regular expression matching IDL system variables.")

(defconst idlwave-variable (concat idlwave-identifier "\\|" idlwave-sysvar)
  "Regular expression matching IDL variable names.")

(defconst idlwave-label (concat idlwave-identifier ":")
  "Regular expression matching IDL labels.")

(defconst idlwave-method-call (concat idlwave-identifier  "\\s *->"
				      "\\(\\s *" idlwave-identifier "::\\)?"
))

(defconst idlwave-statement-match
  (list
   ;; "endif else" is the only possible "end" that can be
   ;; followed by a statement on the same line.
   '(endelse . ("end\\(\\|if\\)\\s +else" "end\\(\\|if\\)\\s +else"))
   ;; all other "end"s can not be followed by a statement.
   (cons 'end (list idlwave-end-block-reg nil))
   '(if . ("if\\>" "then"))
   '(for . ("for\\>" "do"))
   '(begin . ("begin\\>" nil))
   '(pdef . ("pro\\>\\|function\\>" nil))
   '(while . ("while\\>" "do"))
   '(repeat . ("repeat\\>" "repeat"))
   '(goto . ("goto\\>" nil))
   '(case . ("case\\>" nil))
   '(switch . ("switch\\>" nil))
   (cons 'call (list (concat "\\(" idlwave-variable "\\) *= *"
			     "\\(" idlwave-method-call "\\s *\\)?"
			     idlwave-identifier
			     "\\s *(") nil))
   (cons 'call (list (concat
		      "\\(" idlwave-method-call "\\s *\\)?"
		      idlwave-identifier
		      "\\( *\\($\\|\\$\\)\\|\\s *,\\)") nil))
   (cons 'assign (list (concat
			"\\(" idlwave-variable "\\) *=") nil)))

  "Associated list of statement matching regular expressions.
Each regular expression matches the start of an IDL statement.
The first element of each association is a symbol giving the statement
type.  The associated value is a list.  The first element of this list
is a regular expression matching the start of an IDL statement for
identifying the statement type.  The second element of this list is a
regular expression for finding a substatement for the type.  The
substatement starts after the end of the found match modulo
whitespace.  If it is nil then the statement has no substatement.  The
list order matters since matching an assignment statement exactly is
not possible without parsing.  Thus assignment statement become just
the leftover unidentified statements containing an equal sign.")

;; FIXME: This var seems to only ever be set, but never actually used!
(defvar idlwave-fill-function 'auto-fill-function
  "IDL mode auto fill function.")

(defvar idlwave-comment-indent-function 'comment-indent-function
  "IDL mode comment indent function.")

;; Note that this is documented in the v18 manuals as being a string
;; of length one rather than a single character.
;; The code in this file accepts either format for compatibility.
(defvar idlwave-comment-indent-char ?\
  "Character to be inserted for IDL comment indentation.
Normally a space.")

(defconst idlwave-continuation-char ?$
  "Character which is inserted as a last character on previous line by
   \\[idlwave-split-line] to begin a continuation line.  Normally $.")

(defconst idlwave-mode-version "6.1_em22")

(defmacro idlwave-keyword-abbrev (&rest args)
  "Creates a function for abbrev hooks to call `idlwave-check-abbrev' with args."
  `(quote (lambda ()
	    ,(append '(idlwave-check-abbrev) args))))

;; If I take the time I can replace idlwave-keyword-abbrev with
;; idlwave-code-abbrev and remove the quoted abbrev check from
;; idlwave-check-abbrev.  Then, e.g, (idlwave-keyword-abbrev 0 t) becomes
;; (idlwave-code-abbrev idlwave-check-abbrev 0 t).  In fact I should change
;; the name of idlwave-check-abbrev to something like idlwave-modify-abbrev.

(defmacro idlwave-code-abbrev (&rest args)
  "Creates a function for abbrev hooks that ensures abbrevs are not quoted.
Specifically, if the abbrev is in a comment or string it is unexpanded.
Otherwise ARGS forms a list that is evaluated."
  ;; FIXME: it would probably be better to rely on the new :enable-function
  ;; to enforce the "don't expand in comments or strings".
  `(lambda ()
     ,(prin1-to-string args)  ;; Puts the code in the doc string
     (if (idlwave-quoted)
         (progn (unexpand-abbrev) nil)
       ,(append args))))

(autoload 'idlwave-shell "idlw-shell"
  "Run an inferior IDL, with I/O through buffer `(idlwave-shell-buffer)'." t)
(autoload 'idlwave-shell-send-command "idlw-shell")
(autoload 'idlwave-shell-recenter-shell-window "idlw-shell"
  "Run `idlwave-shell' and switch back to current window" t)
(autoload 'idlwave-shell-save-and-run "idlw-shell"
  "Save and run buffer under the shell." t)
(autoload 'idlwave-shell-break-here "idlw-shell"
  "Set breakpoint in current line." t)
(autoload 'idlwave-shell-run-region "idlw-shell"
  "Compile and run the region." t)

(fset 'idlwave-debug-map (make-sparse-keymap))

(defvar idlwave-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c "    'idlwave-hard-tab)
    (define-key map [(control tab)] 'idlwave-hard-tab)
    ;;(define-key map "\C-c\C- " 'idlwave-hard-tab)
    (define-key map "'"        'idlwave-show-matching-quote)
    (define-key map "\""       'idlwave-show-matching-quote)
    (define-key map "\C-g"     'idlwave-keyboard-quit)
    (define-key map "\C-c;"    'idlwave-toggle-comment-region)
    (define-key map "\C-\M-a"  'idlwave-beginning-of-subprogram)
    (define-key map "\C-\M-e"  'idlwave-end-of-subprogram)
    (define-key map "\C-c{"    'idlwave-beginning-of-block)
    (define-key map "\C-c}"    'idlwave-end-of-block)
    (define-key map "\C-c]"    'idlwave-close-block)
    (define-key map [(meta control h)] 'idlwave-mark-subprogram)
    (define-key map "\M-\C-n"  'idlwave-forward-block)
    (define-key map "\M-\C-p"  'idlwave-backward-block)
    (define-key map "\M-\C-d"  'idlwave-down-block)
    (define-key map "\M-\C-u"  'idlwave-backward-up-block)
    (define-key map "\M-\r"    'idlwave-split-line)
    (define-key map "\M-\C-q"  'idlwave-indent-subprogram)
    (define-key map "\C-c\C-p" 'idlwave-previous-statement)
    (define-key map "\C-c\C-n" 'idlwave-next-statement)
    ;; (define-key map "\r"       'idlwave-newline)
    ;; (define-key map "\t"       'idlwave-indent-line)
    (define-key map [(shift iso-lefttab)] 'idlwave-indent-statement)
    (define-key map "\C-c\C-a" 'idlwave-auto-fill-mode)
    (define-key map "\M-q"     'idlwave-fill-paragraph)
    (define-key map "\M-s"     'idlwave-edit-in-idlde)
    (define-key map "\C-c\C-h" 'idlwave-doc-header)
    (define-key map "\C-c\C-m" 'idlwave-doc-modification)
    (define-key map "\C-c\C-c" 'idlwave-case)
    (define-key map "\C-c\C-d" 'idlwave-debug-map)
    (when (and (listp idlwave-shell-debug-modifiers)
               (not (equal idlwave-shell-debug-modifiers '())))
      ;; Bind the debug commands also with the special modifiers.
      (let ((shift (memq 'shift idlwave-shell-debug-modifiers))
            (mods-noshift
             (delq 'shift (copy-sequence idlwave-shell-debug-modifiers))))
        (define-key map
          (vector (append mods-noshift (list (if shift ?C ?c))))
          'idlwave-shell-save-and-run)
        (define-key map
          (vector (append mods-noshift (list (if shift ?B ?b))))
          'idlwave-shell-break-here)
        (define-key map
          (vector (append mods-noshift (list (if shift ?E ?e))))
          'idlwave-shell-run-region)))
    (define-key map "\C-c\C-d\C-c" 'idlwave-shell-save-and-run)
    (define-key map "\C-c\C-d\C-b" 'idlwave-shell-break-here)
    (define-key map "\C-c\C-d\C-e" 'idlwave-shell-run-region)
    (define-key map "\C-c\C-f" 'idlwave-for)
    ;;  (define-key map "\C-c\C-f" 'idlwave-function)
    ;;  (define-key map "\C-c\C-p" 'idlwave-procedure)
    (define-key map "\C-c\C-r" 'idlwave-repeat)
    (define-key map "\C-c\C-w" 'idlwave-while)
    (define-key map "\C-c\C-k" 'idlwave-kill-autoloaded-buffers)
    (define-key map "\C-c\C-s" 'idlwave-shell)
    (define-key map "\C-c\C-l" 'idlwave-shell-recenter-shell-window)
    (define-key map "\C-c\C-b" 'idlwave-list-buffer-load-path-shadows)
    (define-key map "\C-c\C-v"   'idlwave-find-module)
    (define-key map "\C-c\C-t"   'idlwave-find-module-this-file)
    (define-key map "\C-c?"      'idlwave-routine-info)
    (define-key map "\M-?"       'idlwave-context-help)
    (define-key map [(control meta ?\?)]
      'idlwave-help-assistant-help-with-topic)
    ;; Pickup both forms of Esc/Meta binding
    (define-key map [(meta tab)] 'idlwave-complete)
    (define-key map [?\e?\t] 'idlwave-complete)
    (define-key map "\M-\C-i" 'idlwave-complete)
    (define-key map "\C-c\C-i" 'idlwave-update-routine-info)
    (define-key map "\C-c="    'idlwave-resolve)
    (define-key map
      (if (featurep 'xemacs) [(shift button3)] [(shift mouse-3)])
      'idlwave-mouse-context-help)
    map)
  "Keymap used in IDL mode.")

(defvar idlwave-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?+   "."  st)
    (modify-syntax-entry ?-   "."  st)
    (modify-syntax-entry ?*   "."  st)
    (modify-syntax-entry ?/   "."  st)
    (modify-syntax-entry ?^   "."  st)
    (modify-syntax-entry ?#   "."  st)
    (modify-syntax-entry ?=   "."  st)
    (modify-syntax-entry ?%   "."  st)
    (modify-syntax-entry ?<   "."  st)
    (modify-syntax-entry ?>   "."  st)
    (modify-syntax-entry ?\'  "\"" st)
    (modify-syntax-entry ?\"  "\"" st)
    (modify-syntax-entry ?\\  "."  st)
    (modify-syntax-entry ?_   "_"  st)
    (modify-syntax-entry ?{   "(}" st)
    (modify-syntax-entry ?}   "){" st)
    (modify-syntax-entry ?$   "_"  st)
    (modify-syntax-entry ?.   "."  st)
    (modify-syntax-entry ?\;  "<"  st)
    (modify-syntax-entry ?\n  ">"  st)
    (modify-syntax-entry ?\f  ">"  st)
    st)
  "Syntax table in use in `idlwave-mode' buffers.")

(defvar idlwave-find-symbol-syntax-table
  (let ((st (copy-syntax-table idlwave-mode-syntax-table)))
    (modify-syntax-entry ?$   "w"  st)
    (modify-syntax-entry ?_   "w"  st)
    (modify-syntax-entry ?!   "w"  st)
    (modify-syntax-entry ?.   "w"  st)
    st)
  "Syntax table that treats symbol characters as word characters.")

(defmacro idlwave-with-special-syntax (&rest body)
  "Execute BODY with a different syntax table."
  `(let ((saved-syntax (syntax-table)))
     (unwind-protect
	 (progn
	   (set-syntax-table idlwave-find-symbol-syntax-table)
	   ,@body)
       (set-syntax-table saved-syntax))))

;(defmacro idlwave-with-special-syntax1 (&rest body)
;  "Execute BODY with a different syntax table."
;  `(let ((saved-syntax (syntax-table)))
;     (unwind-protect
;	  (progn
;	    (set-syntax-table idlwave-find-symbol-syntax-table)
;	    ,@body)
;	(set-syntax-table saved-syntax))))

(defun idlwave-action-and-binding (key cmd &optional select)
  "KEY and CMD are made into a key binding and an indent action.
KEY is a string - same as for the `define-key' function.  CMD is a
function of no arguments or a list to be evaluated.  CMD is bound to
KEY in `idlwave-mode-map' by defining an anonymous function calling
`self-insert-command' followed by CMD.  If KEY contains more than one
character a binding will only be set if SELECT is 'both.

\(KEY . CMD\) is also placed in the `idlwave-indent-expand-table',
replacing any previous value for KEY.  If a binding is not set then it
will instead be placed in `idlwave-indent-action-table'.

If the optional argument SELECT is nil then an action and binding are
created.  If SELECT is 'noaction, then a binding is always set and no
action is created.  If SELECT is 'both then an action and binding
will both be created even if KEY contains more than one character.
Otherwise, if SELECT is non-nil then only an action is created.

Some examples:
No spaces before and 1 after a comma
   (idlwave-action-and-binding \",\"  '(idlwave-surround 0 1))
A minimum of 1 space before and after `=' (see `idlwave-expand-equal').
   (idlwave-action-and-binding \"=\"  '(idlwave-expand-equal -1 -1))
Capitalize system variables - action only
   (idlwave-action-and-binding idlwave-sysvar '(capitalize-word 1) t)"
  (if (not (equal select 'noaction))
      ;; Add action
      (let* ((table (if select 'idlwave-indent-action-table
                      'idlwave-indent-expand-table))
	     (table-key (regexp-quote key))
             (cell (assoc table-key (eval table))))
        (if cell
            ;; Replace action command
            (setcdr cell cmd)
          ;; New action
          (set table (append (eval table) (list (cons table-key cmd)))))))
  ;; Make key binding for action
  (if (or (and (null select) (= (length key) 1))
          (equal select 'noaction)
          (equal select 'both))
      (define-key idlwave-mode-map key
        `(lambda ()
           (interactive)
           (self-insert-command 1)
           ,(if (listp cmd) cmd (list cmd))))))

;; Set action and key bindings.
;; See description of the function `idlwave-action-and-binding'.
;; Automatically add spaces for the following characters

;; Actions for & are complicated by &&
(idlwave-action-and-binding "&"  'idlwave-custom-ampersand-surround)

;; Automatically add spaces to equal sign if not keyword.  This needs
;; to go ahead of > and <, so >= and <= will be treated correctly
(idlwave-action-and-binding "="  '(idlwave-expand-equal -1 -1))

;; Actions for > and < are complicated by >=, <=, and ->...
(idlwave-action-and-binding "<"  '(idlwave-custom-ltgtr-surround nil))
(idlwave-action-and-binding ">"  '(idlwave-custom-ltgtr-surround 'gtr))

(idlwave-action-and-binding ","  '(idlwave-surround 0 -1 1))


;;;
;;; Abbrev Section
;;;
;;; When expanding abbrevs and the abbrev hook moves backward, an extra
;;; space is inserted (this is the space typed by the user to expanded
;;; the abbrev).
;;;
(defvar idlwave-mode-abbrev-table nil
  "Abbreviation table used for IDLWAVE mode.")
(define-abbrev-table 'idlwave-mode-abbrev-table ())

(defun idlwave-define-abbrev (name expansion hook &optional noprefix table)
  "Define-abbrev with backward compatibility.

If NOPREFIX is non-nil, don't prepend prefix character.  Installs into
`idlwave-mode-abbrev-table' unless TABLE is non-nil."
  (let ((abbrevs-changed nil)  ;; mask the current value to avoid save
	(args (list (or table idlwave-mode-abbrev-table)
		    (if noprefix name (concat idlwave-abbrev-start-char name))
		    expansion
		    hook)))
    (condition-case nil
	(apply 'define-abbrev (append args '(0 t)))
      (error (apply 'define-abbrev args)))))

(condition-case nil
    (modify-syntax-entry (string-to-char idlwave-abbrev-start-char)
			 "w" idlwave-mode-syntax-table)
  (error nil))

;;
;; Templates
;;
(idlwave-define-abbrev "c"   "" (idlwave-code-abbrev idlwave-case))
(idlwave-define-abbrev "sw"  "" (idlwave-code-abbrev idlwave-switch))
(idlwave-define-abbrev "f"   "" (idlwave-code-abbrev idlwave-for))
(idlwave-define-abbrev "fu"  "" (idlwave-code-abbrev idlwave-function))
(idlwave-define-abbrev "pr"  "" (idlwave-code-abbrev idlwave-procedure))
(idlwave-define-abbrev "r"   "" (idlwave-code-abbrev idlwave-repeat))
(idlwave-define-abbrev "w"   "" (idlwave-code-abbrev idlwave-while))
(idlwave-define-abbrev "i"   "" (idlwave-code-abbrev idlwave-if))
(idlwave-define-abbrev "elif" "" (idlwave-code-abbrev idlwave-elif))
;;
;; Keywords, system functions, conversion routines
;;
(idlwave-define-abbrev "ap" "arg_present()" (idlwave-keyword-abbrev 1))
(idlwave-define-abbrev "b"  "begin"        (idlwave-keyword-abbrev 0 t))
(idlwave-define-abbrev "co" "common"       (idlwave-keyword-abbrev 0 t))
(idlwave-define-abbrev "cb" "byte()"       (idlwave-keyword-abbrev 1))
(idlwave-define-abbrev "cx" "fix()"        (idlwave-keyword-abbrev 1))
(idlwave-define-abbrev "cl" "long()"       (idlwave-keyword-abbrev 1))
(idlwave-define-abbrev "cf" "float()"      (idlwave-keyword-abbrev 1))
(idlwave-define-abbrev "cs" "string()"     (idlwave-keyword-abbrev 1))
(idlwave-define-abbrev "cc" "complex()"    (idlwave-keyword-abbrev 1))
(idlwave-define-abbrev "cd" "double()"     (idlwave-keyword-abbrev 1))
(idlwave-define-abbrev "e"  "else"         (idlwave-keyword-abbrev 0 t))
(idlwave-define-abbrev "ec" "endcase"      'idlwave-show-begin)
(idlwave-define-abbrev "es" "endswitch"    'idlwave-show-begin)
(idlwave-define-abbrev "ee" "endelse"      'idlwave-show-begin)
(idlwave-define-abbrev "ef" "endfor"       'idlwave-show-begin)
(idlwave-define-abbrev "ei" "endif else if" 'idlwave-show-begin)
(idlwave-define-abbrev "el" "endif else"   'idlwave-show-begin)
(idlwave-define-abbrev "en" "endif"        'idlwave-show-begin)
(idlwave-define-abbrev "er" "endrep"       'idlwave-show-begin)
(idlwave-define-abbrev "ew" "endwhile"     'idlwave-show-begin)
(idlwave-define-abbrev "g"  "goto,"        (idlwave-keyword-abbrev 0 t))
(idlwave-define-abbrev "h"  "help,"        (idlwave-keyword-abbrev 0))
(idlwave-define-abbrev "k"  "keyword_set()" (idlwave-keyword-abbrev 1))
(idlwave-define-abbrev "n"  "n_elements()" (idlwave-keyword-abbrev 1))
(idlwave-define-abbrev "on" "on_error,"    (idlwave-keyword-abbrev 0))
(idlwave-define-abbrev "oi" "on_ioerror,"  (idlwave-keyword-abbrev 0 1))
(idlwave-define-abbrev "ow" "openw,"       (idlwave-keyword-abbrev 0))
(idlwave-define-abbrev "or" "openr,"       (idlwave-keyword-abbrev 0))
(idlwave-define-abbrev "ou" "openu,"       (idlwave-keyword-abbrev 0))
(idlwave-define-abbrev "p"  "print,"       (idlwave-keyword-abbrev 0))
(idlwave-define-abbrev "pt" "plot,"        (idlwave-keyword-abbrev 0))
(idlwave-define-abbrev "re" "read,"        (idlwave-keyword-abbrev 0))
(idlwave-define-abbrev "rf" "readf,"       (idlwave-keyword-abbrev 0))
(idlwave-define-abbrev "ru" "readu,"       (idlwave-keyword-abbrev 0))
(idlwave-define-abbrev "rt" "return"       (idlwave-keyword-abbrev 0))
(idlwave-define-abbrev "sc" "strcompress()" (idlwave-keyword-abbrev 1))
(idlwave-define-abbrev "sn" "strlen()"     (idlwave-keyword-abbrev 1))
(idlwave-define-abbrev "sl" "strlowcase()" (idlwave-keyword-abbrev 1))
(idlwave-define-abbrev "su" "strupcase()"  (idlwave-keyword-abbrev 1))
(idlwave-define-abbrev "sm" "strmid()"     (idlwave-keyword-abbrev 1))
(idlwave-define-abbrev "sp" "strpos()"     (idlwave-keyword-abbrev 1))
(idlwave-define-abbrev "st" "strput()"     (idlwave-keyword-abbrev 1))
(idlwave-define-abbrev "sr" "strtrim()"    (idlwave-keyword-abbrev 1))
(idlwave-define-abbrev "t"  "then"         (idlwave-keyword-abbrev 0 t))
(idlwave-define-abbrev "u"  "until"        (idlwave-keyword-abbrev 0 t))
(idlwave-define-abbrev "wu" "writeu,"      (idlwave-keyword-abbrev 0))
(idlwave-define-abbrev "iap" "if arg_present() then"     (idlwave-keyword-abbrev 6))
(idlwave-define-abbrev "ik" "if keyword_set() then" (idlwave-keyword-abbrev 6))
(idlwave-define-abbrev "ine" "if n_elements() eq 0 then" (idlwave-keyword-abbrev 11))
(idlwave-define-abbrev "inn" "if n_elements() ne 0 then" (idlwave-keyword-abbrev 11))
(idlwave-define-abbrev "np" "n_params()"   (idlwave-keyword-abbrev 0))
(idlwave-define-abbrev "s"  "size()"       (idlwave-keyword-abbrev 1))
(idlwave-define-abbrev "wi" "widget_info()" (idlwave-keyword-abbrev 1))
(idlwave-define-abbrev "wc" "widget_control," (idlwave-keyword-abbrev 0))
(idlwave-define-abbrev "pv" "ptr_valid()" (idlwave-keyword-abbrev 1))
(idlwave-define-abbrev "ipv" "if ptr_valid() then" (idlwave-keyword-abbrev 6))

;; This section is reserved words only. (From IDL user manual)
;;
(idlwave-define-abbrev "and"        "and"       (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "begin"      "begin"     (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "break"      "break"     (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "case"       "case"      (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "common"     "common"    (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "continue"   "continue"  (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "do"         "do"        (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "else"       "else"      (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "end"        "end"       'idlwave-show-begin-check t)
(idlwave-define-abbrev "endcase"    "endcase"   'idlwave-show-begin-check t)
(idlwave-define-abbrev "endelse"    "endelse"   'idlwave-show-begin-check t)
(idlwave-define-abbrev "endfor"     "endfor"    'idlwave-show-begin-check t)
(idlwave-define-abbrev "endif"      "endif"     'idlwave-show-begin-check t)
(idlwave-define-abbrev "endrep"     "endrep"    'idlwave-show-begin-check t)
(idlwave-define-abbrev "endswitch"  "endswitch" 'idlwave-show-begin-check t)
(idlwave-define-abbrev "endwhi"     "endwhi"    'idlwave-show-begin-check t)
(idlwave-define-abbrev "endwhile"   "endwhile"  'idlwave-show-begin-check t)
(idlwave-define-abbrev "eq"         "eq"        (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "for"        "for"       (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "function"   "function"  (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "ge"         "ge"        (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "goto"       "goto"      (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "gt"         "gt"        (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "if"         "if"        (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "le"         "le"        (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "lt"         "lt"        (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "mod"        "mod"       (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "ne"         "ne"        (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "not"        "not"       (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "of"         "of"        (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "on_ioerror" "on_ioerror" (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "or"         "or"        (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "pro"        "pro"       (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "repeat"     "repeat"    (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "switch"     "switch"    (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "then"       "then"      (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "until"      "until"     (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "while"      "while"     (idlwave-keyword-abbrev 0 t) t)
(idlwave-define-abbrev "xor"        "xor"       (idlwave-keyword-abbrev 0 t) t)

(defvar imenu-create-index-function)
(defvar extract-index-name-function)
(defvar prev-index-position-function)
(defvar imenu-extract-index-name-function)
(defvar imenu-prev-index-position-function)
;; defined later - so just make the compiler hush
(defvar idlwave-mode-menu)
(defvar idlwave-mode-debug-menu)

;;;###autoload
(define-derived-mode idlwave-mode prog-mode "IDLWAVE"
  "Major mode for editing IDL source files (version 6.1_em22).

The main features of this mode are

1. Indentation and Formatting
   --------------------------
   Like other Emacs programming modes, C-j inserts a newline and indents.
   TAB is used for explicit indentation of the current line.

   To start a continuation line, use \\[idlwave-split-line].  This
   function can also be used in the middle of a line to split the line
   at that point.  When used inside a long constant string, the string
   is split at that point with the `+' concatenation operator.

   Comments are indented as follows:

   `;;;' Indentation remains unchanged.
   `;;'  Indent like the surrounding code
   `;'   Indent to a minimum column.

   The indentation of comments starting in column 0 is never changed.

   Use \\[idlwave-fill-paragraph] to refill a paragraph inside a
   comment.  The indentation of the second line of the paragraph
   relative to the first will be retained.  Use
   \\[idlwave-auto-fill-mode] to toggle auto-fill mode for these
   comments.  When the variable `idlwave-fill-comment-line-only' is
   nil, code can also be auto-filled and auto-indented.

   To convert pre-existing IDL code to your formatting style, mark the
   entire buffer with \\[mark-whole-buffer] and execute
   \\[idlwave-expand-region-abbrevs].  Then mark the entire buffer
   again followed by \\[indent-region] (`indent-region').

2. Routine Info
   ------------
   IDLWAVE displays information about the calling sequence and the
   accepted keyword parameters of a procedure or function with
   \\[idlwave-routine-info].  \\[idlwave-find-module] jumps to the
   source file of a module.  These commands know about system
   routines, all routines in idlwave-mode buffers and (when the
   idlwave-shell is active) about all modules currently compiled under
   this shell.  It also makes use of pre-compiled or custom-scanned
   user and library catalogs many popular libraries ship with by
   default.  Use \\[idlwave-update-routine-info] to update this
   information, which is also used for completion (see item 4).

3. Online IDL Help
   ---------------

   \\[idlwave-context-help] displays the IDL documentation relevant
   for the system variable, keyword, or routines at point.  A single
   key stroke gets you directly to the right place in the docs.  See
   the manual to configure where and how the HTML help is displayed.

4. Completion
   ----------
   \\[idlwave-complete] completes the names of procedures, functions
   class names, keyword parameters, system variables and tags, class
   tags, structure tags, filenames and much more.  It is context
   sensitive and figures out what is expected at point.  Lower case
   strings are completed in lower case, other strings in mixed or
   upper case.

5. Code Templates and Abbreviations
   --------------------------------
   Many Abbreviations are predefined to expand to code fragments and templates.
   The abbreviations start generally with a `\\`.  Some examples:

   \\pr        PROCEDURE template
   \\fu        FUNCTION template
   \\c         CASE statement template
   \\sw        SWITCH statement template
   \\f         FOR loop template
   \\r         REPEAT Loop template
   \\w         WHILE loop template
   \\i         IF statement template
   \\elif      IF-ELSE statement template
   \\b         BEGIN

   For a full list, use \\[idlwave-list-abbrevs].  Some templates also
   have direct keybindings - see the list of keybindings below.

   \\[idlwave-doc-header] inserts a documentation header at the
   beginning of the current program unit (pro, function or main).
   Change log entries can be added to the current program unit with
   \\[idlwave-doc-modification].

6. Automatic Case Conversion
   -------------------------
   The case of reserved words and some abbrevs is controlled by
   `idlwave-reserved-word-upcase' and `idlwave-abbrev-change-case'.

7. Automatic END completion
   ------------------------
   If the variable `idlwave-expand-generic-end' is non-nil, each END typed
   will be converted to the specific version, like ENDIF, ENDFOR, etc.

8. Hooks
   -----
   Loading idlwave.el runs `idlwave-load-hook'.
   Turning on `idlwave-mode' runs `idlwave-mode-hook'.

9. Documentation and Customization
   -------------------------------
   Info documentation for this package is available.  Use
   \\[idlwave-info] to display (complain to your sysadmin if that does
   not work).  For Postscript, PDF, and HTML versions of the
   documentation, check IDLWAVE's homepage at URL `http://idlwave.org'.
   IDLWAVE has customize support - see the group `idlwave'.

10.Keybindings
   -----------
   Here is a list of all keybindings of this mode.
   If some of the key bindings below show with ??, use \\[describe-key]
   followed by the key sequence to see what the key sequence does.

\\{idlwave-mode-map}"
  :abbrev-table idlwave-mode-abbrev-table
  (if idlwave-startup-message
      (message "Emacs IDLWAVE mode version %s." idlwave-mode-version))
  (setq idlwave-startup-message nil)

  (set (make-local-variable 'indent-line-function) 'idlwave-indent-and-action)

  (set (make-local-variable idlwave-comment-indent-function)
       #'idlwave-comment-hook)

  (set (make-local-variable 'comment-start-skip) ";+[ \t]*")
  (set (make-local-variable 'comment-start) ";")
  (set (make-local-variable 'comment-add) 1) ; ";;" for new and regions
  (set (make-local-variable 'abbrev-all-caps) t)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'completion-ignore-case) t)

  (when (featurep 'easymenu)
    (easy-menu-add idlwave-mode-menu idlwave-mode-map)
    (easy-menu-add idlwave-mode-debug-menu idlwave-mode-map))

  (setq abbrev-mode t)

  (set (make-local-variable idlwave-fill-function) 'idlwave-auto-fill)
  (setq comment-end "")
  (set (make-local-variable 'comment-multi-line) nil)
  (set (make-local-variable 'paragraph-separate)
       "[ \t\f]*$\\|[ \t]*;+[ \t]*$\\|;+[+=-_*]+$")
  (set (make-local-variable 'paragraph-start) "[ \t\f]\\|[ \t]*;+[ \t]")
  (set (make-local-variable 'paragraph-ignore-fill-prefix) nil)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)

  ;; ChangeLog
  (set (make-local-variable 'add-log-current-defun-function)
       'idlwave-current-routine-fullname)

  ;; Set tag table list to use IDLTAGS as file name.
  (if (boundp 'tag-table-alist)
      (add-to-list 'tag-table-alist '("\\.pro$" . "IDLTAGS")))

  ;; Font-lock additions
  ;; Following line is for Emacs - XEmacs uses the corresponding property
  ;; on the `idlwave-mode' symbol.
  (set (make-local-variable 'font-lock-defaults) idlwave-font-lock-defaults)
  (set (make-local-variable 'font-lock-mark-block-function)
       'idlwave-mark-subprogram)
  (set (make-local-variable 'font-lock-fontify-region-function)
       'idlwave-font-lock-fontify-region)

  ;; Imenu setup
  (set (make-local-variable 'imenu-create-index-function)
       'imenu-default-create-index-function)
  (set (make-local-variable 'imenu-extract-index-name-function)
       'idlwave-unit-name)
  (set (make-local-variable 'imenu-prev-index-position-function)
       'idlwave-prev-index-position)

  ;; HideShow setup
  (add-to-list 'hs-special-modes-alist
	       (list 'idlwave-mode
		     idlwave-begin-block-reg
		     idlwave-end-block-reg
		     ";"
		     'idlwave-forward-block nil))

  ;; Make a local post-command-hook and add our hook to it
  ;; NB: `make-local-hook' needed for older/alternative Emacs compatibility
  ;; (make-local-hook 'post-command-hook)
  (add-hook 'post-command-hook 'idlwave-command-hook nil 'local)

  ;; Make local hooks for buffer updates
  ;; NB: `make-local-hook' needed for older/alternative Emacs compatibility
  ;; (make-local-hook 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook 'idlwave-kill-buffer-update nil 'local)
  ;; (make-local-hook 'after-save-hook)
  (add-hook 'after-save-hook 'idlwave-save-buffer-update nil 'local)
  (add-hook 'after-save-hook 'idlwave-revoke-license-to-kill nil 'local)

  ;; Setup directories and file, if necessary
  (idlwave-setup)

  ;; Update the routine info with info about current buffer?
  (idlwave-new-buffer-update)

  ;; Check help location
  (idlwave-help-check-locations))

(defvar idlwave-setup-done nil)
(defun idlwave-setup ()
  (unless idlwave-setup-done
    (if (not (file-directory-p idlwave-config-directory))
	(make-directory idlwave-config-directory))
    (setq
     idlwave-user-catalog-file (expand-file-name
				idlwave-user-catalog-file
				idlwave-config-directory)
     idlwave-xml-system-rinfo-converted-file
     (expand-file-name
      idlwave-xml-system-rinfo-converted-file
      idlwave-config-directory)
     idlwave-path-file (expand-file-name
			idlwave-path-file
			idlwave-config-directory))
    (idlwave-read-paths)  ; we may need these early
    (setq idlwave-setup-done t)))

(defun idlwave-font-lock-fontify-region (beg end &optional verbose)
  "Fontify continuation lines correctly."
  (let (pos)
    (save-excursion
      (goto-char beg)
      (forward-line -1)
      (when (setq pos (idlwave-is-continuation-line))
	(goto-char pos)
	(idlwave-beginning-of-statement)
	(setq beg (point)))))
  (font-lock-default-fontify-region beg end verbose))

;;
;; Code Formatting ----------------------------------------------------
;;

(defun idlwave-hard-tab ()
  "Insert TAB in buffer in current position."
  (interactive)
  (insert "\t"))

;;; This stuff is experimental

(defvar idlwave-command-hook nil
  "If non-nil, a list that can be evaluated using `eval'.
It is evaluated in the lisp function `idlwave-command-hook' which is
placed in `post-command-hook'.")

(defun idlwave-command-hook ()
  "Command run after every command.
Evaluates a non-nil value of the *variable* `idlwave-command-hook' and
sets the variable to zero afterwards."
  (and idlwave-command-hook
       (listp idlwave-command-hook)
       (condition-case nil
	   (eval idlwave-command-hook)
	 (error nil)))
  (setq idlwave-command-hook nil))

;;; End experiment

;; It would be better to use expand.el for better abbrev handling and
;; versatility.

(defun idlwave-check-abbrev (arg &optional reserved)
  "Reverse abbrev expansion if in comment or string.
Argument ARG is the number of characters to move point
backward if `idlwave-abbrev-move' is non-nil.
If optional argument RESERVED is non-nil then the expansion
consists of reserved words, which will be capitalized if
`idlwave-reserved-word-upcase' is non-nil.
Otherwise, the abbrev will be capitalized if `idlwave-abbrev-change-case'
is non-nil, unless its value is \`down in which case the abbrev will be
made into all lowercase.
Returns non-nil if abbrev is left expanded."
  (if (idlwave-quoted)
      (progn (unexpand-abbrev)
             nil)
    (if (and reserved idlwave-reserved-word-upcase)
        (upcase-region last-abbrev-location (point))
      (cond
       ((equal idlwave-abbrev-change-case 'down)
        (downcase-region last-abbrev-location (point)))
       (idlwave-abbrev-change-case
        (upcase-region last-abbrev-location (point)))))
    (if (and idlwave-abbrev-move (> arg 0))
        (if (boundp 'post-command-hook)
            (setq idlwave-command-hook (list 'backward-char (1+ arg)))
          (backward-char arg)))
    t))

(defun idlwave-in-comment ()
  "Return t if point is inside a comment, nil otherwise."
  (save-excursion
    (let ((here (point)))
      (and (idlwave-goto-comment) (> here (point))))))

(defun idlwave-goto-comment ()
  "Move to start of comment delimiter on current line.
Moves to end of line if there is no comment delimiter.
Ignores comment delimiters in strings.
Returns point if comment found and nil otherwise."
  (let ((eos (point-at-eol))
        (data (match-data))
        found)
    ;; Look for first comment delimiter not in a string
    (beginning-of-line)
    (setq found (search-forward comment-start eos 'lim))
    (while (and found (idlwave-in-quote))
      (setq found (search-forward comment-start eos 'lim)))
    (store-match-data data)
    (and found (not (idlwave-in-quote))
         (progn
           (backward-char 1)
           (point)))))

(defun idlwave-region-active-p ()
  "Should we operate on an active region?"
  (if (fboundp 'use-region-p)
      (use-region-p)
    (region-active-p)))

(defun idlwave-show-matching-quote ()
  "Insert quote and show matching quote if this is end of a string."
  (interactive)
  (let ((bq (idlwave-in-quote))
        (inq last-command-event))
    (if (and bq (not (idlwave-in-comment)))
        (let ((delim (char-after bq)))
          (insert inq)
          (if (eq inq delim)
              (save-excursion
                (goto-char bq)
                (sit-for 1))))
      ;; Not the end of a string
      (insert inq))))

(defun idlwave-show-begin-check ()
  "Ensure that the previous word was a token before `idlwave-show-begin'.
An END token must be preceded by whitespace."
  (if (not (idlwave-quoted))
      (if
	  (save-excursion
	    (backward-word 1)
	    (backward-char 1)
	    (looking-at "[ \t\n\f]"))
	  (idlwave-show-begin))))

(defun idlwave-show-begin ()
  "Find the start of current block and blinks to it for a second.
Also checks if the correct END statement has been used."
  ;; All end statements are reserved words
  ;; Re-indent end line
  ;;(insert-char ?\  1) ;; So indent, etc. work well
  ;;(backward-char 1)
  (let* ((pos (point-marker))
	 (last-abbrev-marker (copy-marker last-abbrev-location))
	 (eol-pos (point-at-eol))
	 begin-pos end-pos end end1 )
    (if idlwave-reindent-end  (idlwave-indent-line))
    (setq last-abbrev-location (marker-position last-abbrev-marker))
    (when (and (idlwave-check-abbrev 0 t)
	       idlwave-show-block)
      (save-excursion
	;; Move inside current block
	(goto-char last-abbrev-marker)
	(idlwave-block-jump-out -1 'nomark)
	(setq begin-pos (point))
	(idlwave-block-jump-out 1 'nomark)
	(setq end-pos (point))
	(if (> end-pos eol-pos)
	    (setq end-pos pos))
	(goto-char end-pos)
	(setq end (buffer-substring
		   (progn
		     (skip-chars-backward "a-zA-Z")
		     (point))
		   end-pos))
	(goto-char begin-pos)
	(when (setq end1 (cdr (idlwave-block-master)))
	  (cond
	   ((null end1)) ; no-operation
	   ((string= (downcase end) (downcase end1))
	    (sit-for 1))
	   ((string= (downcase end) "end")
	    ;; A generic end
	    (if idlwave-expand-generic-end
		(save-excursion
		  (goto-char pos)
		  (backward-char 3)
		  (insert (if (string= end "END") (upcase end1) end1))
		  (delete-char 3)))
	    (sit-for 1))
	   (t
	    (beep)
	    (message "Warning: Shouldn't this be \"%s\" instead of \"%s\"?"
		     end1 end)
	    (sit-for 1))))))))
  ;;(delete-char 1))

(defun idlwave-block-master ()
  (let ((case-fold-search t))
    (save-excursion
      (cond
       ((looking-at "pro\\|case\\|switch\\|function\\>")
	(assoc (downcase (match-string 0)) idlwave-block-matches))
       ((looking-at "begin\\>")
	(let ((limit (save-excursion
		       (idlwave-beginning-of-statement)
		       (point))))
	  (cond
	   ((re-search-backward ":[ \t]*\\=" limit t)
	    ;; seems to be a case thing
	    '("begin" . "end"))
	   ((re-search-backward idlwave-block-match-regexp limit t)
	    (assoc (downcase (match-string 1))
		   idlwave-block-matches))
	   (t
	    ;; Just a normal block
	    '("begin" . "end")))))
       (t nil)))))

(defun idlwave-close-block ()
  "Terminate the current block with the correct END statement."
  (interactive)
  ;; Start new line if we are not in a new line
  (unless (save-excursion
	    (skip-chars-backward " \t")
	    (bolp))
    (let ((idlwave-show-block nil))
      (newline-and-indent)))
  (let ((last-abbrev-location (point)))  ; for upcasing
    (insert "end")
    (idlwave-show-begin)))

(defun idlwave-custom-ampersand-surround (&optional is-action)
  "Surround &, leaving room for && (which surround as well)."
  (let* ((prev-char (char-after (- (point) 2)))
	 (next-char (char-after (point)))
	 (amp-left (eq prev-char ?&))
	 (amp-right (eq next-char ?&))
	 (len (if amp-left 2 1)))
    (unless amp-right ;no need to do it twice, amp-left will catch it.
      (idlwave-surround -1 (if (or is-action amp-left) -1) len))))

(defun idlwave-custom-ltgtr-surround (gtr &optional is-action)
  "Surround > and < by blanks, leaving room for >= and <=, and considering ->."
  (let* ((prev-char (char-after (- (point) 2)))
	(next-char (char-after (point)))
	(method-invoke (and gtr (eq prev-char ?-)))
	(len (if method-invoke 2 1)))
    (unless  (eq next-char ?=)
      ;; Key binding: pad only on left, to save for possible >=/<=
      (idlwave-surround -1 (if (or is-action method-invoke) -1) len))))

(defun idlwave-surround (&optional before after length is-action)
  "Surround the LENGTH characters before point with blanks.
LENGTH defaults to 1.
Optional arguments BEFORE and AFTER affect the behavior before and
after the characters (see also description of `idlwave-make-space'):

nil            do nothing
0              force no spaces
integer > 0    force exactly n spaces
integer < 0    at least |n| spaces

The function does nothing if any of the following conditions is true:
- `idlwave-surround-by-blank' is nil
- the character before point is inside a string or comment"
  (when (and idlwave-surround-by-blank (not (idlwave-quoted)))
    (let ((length (or length 1))) ; establish a default for LENGTH
      (backward-char length)
      (save-restriction
	(let ((here (point)))
	  (skip-chars-backward " \t")
	  (if (bolp)
	      ;; avoid clobbering indent
	      (progn
		(move-to-column (idlwave-calculate-indent))
		(if (<= (point) here)
		    (narrow-to-region (point) here))
		(goto-char here)))
	  (idlwave-make-space before))
	(skip-chars-forward " \t"))
      (forward-char length)
      (idlwave-make-space after)
      ;; Check to see if the line should auto wrap
      (if (and (equal (char-after (1- (point))) ?\ )
	       (> (current-column) fill-column))
	  (funcall auto-fill-function)))))

(defun idlwave-make-space (n)
  "Make space at point.
The space affected is all the spaces and tabs around point.
If n is non-nil then point is left abs(n) spaces from the beginning of
the contiguous space.
The amount of space at point is determined by N.
If the value of N is:
nil   - do nothing.
> 0   - exactly N spaces.
< 0   - a minimum of -N spaces, i.e., do not change if there are
        already -N spaces.
0     - no spaces (i.e. remove any existing space)."
  (if (integerp n)
      (let
          ((start-col (progn (skip-chars-backward " \t") (current-column)))
           (left (point))
           (end-col (progn (skip-chars-forward " \t") (current-column))))
        (delete-horizontal-space)
        (cond
         ((> n 0)
          (idlwave-indent-to (+ start-col n))
          (goto-char (+ left n)))
         ((< n 0)
          (idlwave-indent-to end-col (- n))
          (goto-char (- left n)))
         ;; n = 0, done
         ))))

(defun idlwave-newline ()
  "Insert a newline and indent the current and previous line."
  (interactive)
  ;;
  ;; Handle unterminated single and double quotes
  ;; If not in a comment and in a string then insertion of a newline
  ;; will mean unbalanced quotes.
  ;;
  (if (and (not (idlwave-in-comment)) (idlwave-in-quote))
      (progn (beep)
             (message "Warning: unbalanced quotes?")))
  (newline)
  ;;
  ;; The current line is being split, the cursor should be at the
  ;; beginning of the new line skipping the leading indentation.
  ;;
  ;; The reason we insert the new line before indenting is that the
  ;; indenting could be confused by keywords (e.g. END) on the line
  ;; after the split point.  This prevents us from just using
  ;; `indent-for-tab-command' followed by `newline-and-indent'.
  ;;
  (beginning-of-line 0)
  (idlwave-indent-line)
  (forward-line)
  (idlwave-indent-line))

;;
;;  Use global variable 'comment-column' to set parallel comment
;;
;; Modeled on lisp.el
;; Emacs Lisp and IDL (Wave CL) have identical comment syntax
(defun idlwave-comment-hook ()
  "Compute indent for the beginning of the IDL comment delimiter."
  (if (or (looking-at idlwave-no-change-comment)
          (looking-at (or idlwave-begin-line-comment "^;")))
      (current-column)
    (if (looking-at idlwave-code-comment)
        (if (save-excursion (skip-chars-backward " \t") (bolp))
            ;; On line by itself, indent as code
            (let ((tem (idlwave-calculate-indent)))
              (if (listp tem) (car tem) tem))
          ;; after code - do not change
          (current-column))
      (skip-chars-backward " \t")
      (max (if (bolp) 0 (1+ (current-column)))
           comment-column))))

(defun idlwave-split-line ()
  "Continue line by breaking line at point and indent the lines.
For a code line insert continuation marker.  If the line is a line comment
then the new line will contain a comment with the same indentation.
Splits strings with the IDL operator `+' if `idlwave-split-line-string' is
non-nil."
  (interactive)
  ;; Expand abbreviation, just like normal RET would.
  (and abbrev-mode (expand-abbrev))
  (let (beg)
    (if (not (idlwave-in-comment))
        ;; For code line add continuation.
        ;; Check if splitting a string.
        (progn
          (if (setq beg (idlwave-in-quote))
              (if idlwave-split-line-string
                  ;; Split the string.
                  (progn (insert (setq beg (char-after beg)) " + "
                                 idlwave-continuation-char beg)
                         (backward-char 1)
			 (newline-and-indent)
			 (forward-char 1))
                ;; Do not split the string.
                (beep)
                (message "Warning: continuation inside string!!")
                (insert " " idlwave-continuation-char))
            ;; Not splitting a string.
	    (if (not (member (char-before) '(?\  ?\t)))
		(insert " "))
            (insert idlwave-continuation-char)
	    (newline-and-indent)))
      (indent-new-comment-line))
    ;; Indent previous line
    (setq beg (- (point-max) (point)))
    (forward-line -1)
    (idlwave-indent-line)
    (goto-char (- (point-max) beg))
    ;; Reindent new line
    (idlwave-indent-line)))

(defun idlwave-beginning-of-subprogram (&optional nomark)
  "Move point to the beginning of the current program unit.
If NOMARK is non-nil, do not push mark."
  (interactive)
  (idlwave-find-key idlwave-begin-unit-reg -1 nomark))

(defun idlwave-end-of-subprogram (&optional nomark)
  "Move point to the start of the next program unit.
If NOMARK is non-nil, do not push mark."
  (interactive)
  (idlwave-end-of-statement)
  (idlwave-find-key idlwave-end-unit-reg 1 nomark))

(defun idlwave-mark-statement ()
  "Mark current IDL statement."
  (interactive)
  (idlwave-end-of-statement)
  (let ((end (point)))
    (idlwave-beginning-of-statement)
    (push-mark end nil t)))

(defun idlwave-mark-block ()
  "Mark containing block."
  (interactive)
  (idlwave-end-of-statement)
  (idlwave-backward-up-block -1)
  (idlwave-end-of-statement)
  (let ((end (point)))
    (idlwave-backward-block)
    (idlwave-beginning-of-statement)
    (push-mark end nil t)))


(defun idlwave-mark-subprogram ()
  "Put mark at beginning of program, point at end.
The marks are pushed."
  (interactive)
  (idlwave-end-of-statement)
  (idlwave-beginning-of-subprogram)
  (let ((beg (point)))
    (idlwave-forward-block)
    (push-mark beg nil t))
  (exchange-point-and-mark))

(defun idlwave-backward-up-block (&optional arg)
  "Move to beginning of enclosing block if prefix ARG >= 0.
If prefix ARG < 0 then move forward to enclosing block end."
  (interactive "p")
  (idlwave-block-jump-out (- arg) 'nomark))

(defun idlwave-beginning-of-block ()
  "Go to the beginning of the current block."
  (interactive)
  (idlwave-block-jump-out -1 'nomark)
  (forward-word 1))

(defun idlwave-end-of-block ()
  "Go to the beginning of the current block."
  (interactive)
  (idlwave-block-jump-out 1 'nomark)
  (backward-word 1))

(defun idlwave-forward-block (&optional arg)
  "Move across next nested block."
  (interactive)
  (let ((arg (or arg 1)))
    (if (idlwave-down-block arg)
	(idlwave-block-jump-out arg 'nomark))))

(defun idlwave-backward-block ()
  "Move backward across previous nested block."
  (interactive)
  (if (idlwave-down-block -1)
      (idlwave-block-jump-out -1 'nomark)))

(defun idlwave-down-block (&optional arg)
  "Go down a block.
With ARG: ARG >= 0 go forwards, ARG < 0 go backwards.
Returns non-nil if successful."
  (interactive "p")
  (let (status)
    (if (< arg 0)
        ;; Backward
        (let ((eos (save-excursion
                     (idlwave-block-jump-out -1 'nomark)
                     (point))))
          (if (setq status (idlwave-find-key
			    idlwave-end-block-reg -1 'nomark eos))
              (idlwave-beginning-of-statement)
            (message "No nested block before beginning of containing block.")))
      ;; Forward
      (let ((eos (save-excursion
                   (idlwave-block-jump-out 1 'nomark)
                   (point))))
        (if (setq status (idlwave-find-key
			  idlwave-begin-block-reg 1 'nomark eos))
            (idlwave-end-of-statement)
          (message "No nested block before end of containing block."))))
    status))

(defun idlwave-mark-doclib ()
  "Put point at beginning of doc library header, mark at end.
The marks are pushed."
  (interactive)
  (let (beg
        (here (point)))
    (goto-char (point-max))
    (if (re-search-backward idlwave-doclib-start nil t)
        (progn
	  (setq beg (progn (beginning-of-line) (point)))
	  (if (re-search-forward idlwave-doclib-end nil t)
	      (progn
		(forward-line 1)
		(push-mark beg nil t)
		(message "Could not find end of doc library header.")))
	  (message "Could not find doc library header start.")
	  (goto-char here)))))

(defun idlwave-current-routine-fullname ()
  (let ((name (idlwave-current-routine)))
    (idlwave-make-full-name (nth 2 name) (car name))))

(defun idlwave-current-routine ()
  "Return (NAME TYPE CLASS) of current routine."
  (idlwave-routines)
  (save-excursion
    (idlwave-beginning-of-subprogram 'nomark)
    (if (looking-at "[ \t]*\\<\\(pro\\|function\\)\\>\\s-+\\(\\([a-zA-Z0-9$_]+\\)::\\)?\\([a-zA-Z0-9$_]+\\)")
	(let* ((type (if (string= (downcase (match-string 1)) "pro")
			 'pro 'function))
	       (class (idlwave-sintern-class (match-string 3)))
	       (name (idlwave-sintern-routine-or-method (match-string 4) class)))
	  (list name type class)))))

(defvar idlwave-shell-prompt-pattern)
(defun idlwave-beginning-of-statement ()
  "Move to beginning of the current statement.
Skips back past statement continuations.
Point is placed at the beginning of the line whether or not this is an
actual statement."
  (interactive)
  (cond
   ((derived-mode-p 'idlwave-shell-mode)
    (if (re-search-backward idlwave-shell-prompt-pattern nil t)
	(goto-char (match-end 0))))
   (t
    (if (save-excursion (forward-line -1) (idlwave-is-continuation-line))
	(idlwave-previous-statement)
      (beginning-of-line)))))

(defun idlwave-previous-statement ()
  "Move point to beginning of the previous statement.
Returns t if the current line before moving is the beginning of
the first non-comment statement in the file, and nil otherwise."
  (interactive)
  (let (first-statement)
    (if (not (= (forward-line -1) 0))
        ;; first line in file
        t
      ;; skip blank lines, label lines, include lines and line comments
      (while (and
              ;; The current statement is the first statement until we
              ;; reach another statement.
              (setq first-statement
                    (or
                     (looking-at idlwave-comment-line-start-skip)
                     (looking-at "[ \t]*$")
                     (looking-at (concat "[ \t]*" idlwave-label "[ \t]*$"))
                     (looking-at "^@")))
              (= (forward-line -1) 0)))
      ;; skip continuation lines
      (while (and
              (save-excursion
                (forward-line -1)
                (idlwave-is-continuation-line))
              (= (forward-line -1) 0)))
      first-statement)))

(defun idlwave-end-of-statement ()
  "Move point to the end of the current IDL statement.
If not in a statement just moves to end of line.  Returns position."
  (interactive)
  (while (and (idlwave-is-continuation-line)
              (= (forward-line 1) 0))
    (while (and (idlwave-is-comment-or-empty-line)
		(= (forward-line 1) 0))))
  (end-of-line)
  (point))

(defun idlwave-end-of-statement0 ()
  "Move point to the end of the current IDL statement.
If not in a statement just moves to end of line.  Returns position."
  (interactive)
  (while (and (idlwave-is-continuation-line)
              (= (forward-line 1) 0)))
  (end-of-line)
  (point))

(defun idlwave-next-statement ()
  "Move point to beginning of the next IDL statement.
Returns t if that statement is the last non-comment IDL statement
in the file, and nil otherwise."
  (interactive)
  (let (last-statement)
    (idlwave-end-of-statement)
    ;; skip blank lines, label lines, include lines and line comments
    (while (and (= (forward-line 1) 0)
                ;; The current statement is the last statement until
                ;; we reach a new statement.
                (setq last-statement
                      (or
                       (looking-at idlwave-comment-line-start-skip)
                       (looking-at "[ \t]*$")
                       (looking-at (concat "[ \t]*" idlwave-label "[ \t]*$"))
                       (looking-at "^@")))))
    last-statement))

(defun idlwave-skip-multi-commands (&optional lim)
  "Skip past multiple commands on a line (with `&')."
  (let ((save-point (point)))
    (when (re-search-forward ".*&" lim t)
      (goto-char (match-end 0))
      (if (idlwave-quoted)
	  (goto-char save-point)
	(if (eq (char-after (- (point) 2)) ?&) (goto-char save-point))))
    (point)))

(defun idlwave-skip-label-or-case ()
  "Skip label or case statement element.
Returns position after label.
If there is no label point is not moved and nil is returned."
  ;; Case expressions and labels are terminated by a colon.
  ;; So we find the first colon in the line and make sure
  ;; - no `?' is before it (might be a ? b : c)
  ;; - it is not in a comment
  ;; - not in a string constant
  ;; - not in parenthesis (like a[0:3])
  ;; - not followed by another ":" in explicit class, ala a->b::c
  ;; As many in this mode, this function is heuristic and not an exact
  ;; parser.
  (let* ((start (point))
	 (eos (save-excursion (idlwave-end-of-statement) (point)))
	 (end (idlwave-find-key ":" 1 'nomark eos)))
    (if (and end
             (= (nth 0 (parse-partial-sexp start end)) 0)
	     (not (string-match "\\?" (buffer-substring start end)))
	     (not (string-match "^::" (buffer-substring end eos))))
        (progn
          (forward-char)
          (point))
      (goto-char start)
      nil)))

(defun idlwave-start-of-substatement (&optional pre)
  "Move to start of next IDL substatement after point.
Uses the type of the current IDL statement to determine if the next
statement is on a new line or is a subpart of the current statement.
Returns point at start of substatement modulo whitespace.
If optional argument is non-nil move to beginning of current
substatement."
  (let ((orig (point))
        (eos (idlwave-end-of-statement))
        (ifnest 0)
        st nst last)
    (idlwave-beginning-of-statement)
    (idlwave-skip-label-or-case)
    (if (< (point) orig)
	(idlwave-skip-multi-commands orig))
    (setq last (point))
    ;; Continue looking for substatements until we are past orig
    (while (and (<= (point) orig) (not (eobp)))
      (setq last (point))
      (setq nst (nth 1 (cdr (setq st (car (idlwave-statement-type))))))
      (if (equal (car st) 'if) (setq ifnest (1+ ifnest)))
      (cond ((and nst
                  (idlwave-find-key nst 1 'nomark eos))
             (goto-char (match-end 0)))
            ((and (> ifnest 0) (idlwave-find-key "\\<else\\>" 1 'nomark eos))
             (setq ifnest (1- ifnest))
             (goto-char (match-end 0)))
            (t (setq ifnest 0)
               (idlwave-next-statement))))
    (if pre (goto-char last))
    ;; If a continuation line starts here, move to next line
    (if (looking-at "[ \t]*\\$\\([ \t]*\\(;\\|$\\)\\)")
	(beginning-of-line 2))
    (point)))

(defun idlwave-statement-type ()
  "Return the type of the current IDL statement.
Uses `idlwave-statement-match' to return a cons of (type . point) with
point the ending position where the type was determined.  Type is the
association from `idlwave-statement-match', i.e. the cons cell from the
list not just the type symbol.  Returns nil if not an identifiable
statement."
  (save-excursion
    ;; Skip whitespace within a statement which is spaces, tabs, continuations
    ;; and possibly comments
    (while (looking-at "[ \t]*\\$")
      (forward-line 1))
    (skip-chars-forward " \t")
    (let ((st idlwave-statement-match)
          (case-fold-search t))
      (while (and (not (looking-at (nth 0 (cdr (car st)))))
                  (setq st (cdr st))))
      (if st
          (append st (match-end 0))))))

(defun idlwave-expand-equal (&optional before after is-action)
  "Pad '=' with spaces.
Two cases: Assignment statement, and keyword assignment.
Which case is determined using `idlwave-start-of-substatement' and
`idlwave-statement-type'.  The equal sign will be surrounded by BEFORE
and AFTER blanks.  If `idlwave-pad-keyword' is t then keyword assignment
is treated just like assignment statements.  When nil, spaces are
removed for keyword assignment.  Any other value keeps the current space
around the `='.  Limits in for loops are treated as keyword assignment.

Starting with IDL 6.0, a number of op= assignments are available.
Since ambiguities of the form:

r and= b
rand= b

can occur, alphanumeric operator assignment will never be pre-padded,
only post-padded.  You must use a space before these to disambiguate
\(not just for padding, but for proper parsing by IDL too!).  Other
operators, such as ##=, ^=, etc., will be pre-padded.

IS-ACTION is ignored.

See `idlwave-surround'."
  (if idlwave-surround-by-blank
      (let
	  ((non-an-ops "\\(##\\|\\*\\|\\+\\|-\\|/\\|<\\|>\\|\\^\\)\\=")
	   (an-ops
	    "\\s-\\(AND\\|EQ\\|GE\\|GT\\|LE\\|LT\\|MOD\\|NE\\|OR\\|XOR\\)\\=")
	   (len 1))

	(save-excursion
	  (let ((case-fold-search t))
	    (backward-char)
	    (if (or
		 (re-search-backward non-an-ops nil t)
		 ;; Why doesn't ##? work for both?
		 (re-search-backward "\\(#\\)\\=" nil t))
		(setq len (1+ (length (match-string 1))))
	      (when (re-search-backward an-ops nil t)
		;(setq begin nil) ; won't modify begin
		(setq len (1+ (length (match-string 1))))))))

	(if (eq t idlwave-pad-keyword)
	    ;; Everything gets padded equally
	    (idlwave-surround before after len)
	  ;; Treating keywords/for variables specially...
	  (let ((st (save-excursion   ; To catch "for" variables
		      (idlwave-start-of-substatement t)
		      (idlwave-statement-type)))
		(what (save-excursion ; To catch keywords
			(skip-chars-backward "= \t")
			(nth 2 (idlwave-where)))))
	    (cond ((or (memq what '(function-keyword procedure-keyword))
		       (memq (caar st) '(for pdef)))
		   (cond
		    ((null idlwave-pad-keyword)
		     (idlwave-surround 0 0)
		     ) ; remove space
		    (t))) ; leave any spaces alone
		  (t (idlwave-surround before after len))))))))


(defun idlwave-indent-and-action (&optional arg)
  "Call `idlwave-indent-line' and do expand actions.
With prefix ARG non-nil, indent the entire sub-statement."
  (interactive "p")
  (save-excursion
    (if	(and idlwave-expand-generic-end
	     (re-search-backward "\\<\\(end\\)\\s-*\\="
				 (max 0 (- (point) 10)) t)
	     (looking-at "\\(end\\)\\([ \n\t]\\|\\'\\)"))
	(progn (goto-char (match-end 1))
	       ;;Expand the END abbreviation, just as RET or Space would have.
	       (if abbrev-mode (expand-abbrev)
		 (idlwave-show-begin)))))
  (when (and (not arg) current-prefix-arg)
    (setq arg current-prefix-arg)
    (setq current-prefix-arg nil))
  (if arg
      (idlwave-indent-statement)
    (idlwave-indent-line t)))

(defun idlwave-indent-line (&optional expand)
  "Indent current IDL line as code or as a comment.
The actions in `idlwave-indent-action-table' are performed.
If the optional argument EXPAND is non-nil then the actions in
`idlwave-indent-expand-table' are performed."
  (interactive)
  ;; Move point out of left margin.
  (if (save-excursion
        (skip-chars-backward " \t")
        (bolp))
      (skip-chars-forward " \t"))
  (let ((mloc (point-marker)))
    (save-excursion
      (beginning-of-line)
      (if (looking-at idlwave-comment-line-start-skip)
          ;; Indentation for a line comment
          (progn
            (skip-chars-forward " \t")
            (idlwave-indent-left-margin (idlwave-comment-hook)))
        ;;
        ;; Code Line
        ;;
        ;; Before indenting, run action routines.
        ;;
        (if (and expand idlwave-do-actions)
            (mapc 'idlwave-do-action idlwave-indent-expand-table))
        ;;
        (if idlwave-do-actions
            (mapc 'idlwave-do-action idlwave-indent-action-table))
        ;;
        ;; No longer expand abbrevs on the line.  The user can do this
        ;; manually using expand-region-abbrevs.
        ;;
        ;; Indent for code line
        ;;
        (beginning-of-line)
        (if (or
             ;; a label line
             (looking-at (concat "^" idlwave-label "[ \t]*$"))
             ;; a batch command
             (looking-at "^[ \t]*@"))
            ;; leave flush left
            nil
          ;; indent the line
          (idlwave-indent-left-margin (idlwave-calculate-indent)))
        ;; Adjust parallel comment
	(end-of-line)
	(if (idlwave-in-comment)
	    ;; Emacs 21 is too smart with fill-column on comment indent
	    (let ((fill-column (if (fboundp 'comment-indent-new-line)
				   (1- (frame-width))
				 fill-column)))
	      (indent-for-comment)))))
    (goto-char mloc)
    ;; Get rid of marker
    (set-marker mloc nil)))

(defun idlwave-do-action (action)
  "Perform an action repeatedly on a line.
ACTION is a list (REG . FUNC).  REG is a regular expression.  FUNC is
either a function name to be called with `funcall' or a list to be
evaluated with `eval'.  The action performed by FUNC should leave
point after the match for REG - otherwise an infinite loop may be
entered.  FUNC is always passed a final argument of 'is-action, so it
can discriminate between being run as an action, or a key binding."
  (let ((action-key (car action))
        (action-routine (cdr action)))
    (beginning-of-line)
    (while (idlwave-look-at action-key)
      (if (listp action-routine)
          (eval (append action-routine '('is-action)))
        (funcall action-routine 'is-action)))))

(defun idlwave-indent-to (col &optional min)
  "Indent from point with spaces until column COL.
Inserts space before markers at point."
  (if (not min) (setq min 0))
  (insert-before-markers
   (make-string (max min (- col (current-column))) ?\ )))

(defun idlwave-indent-left-margin (col)
  "Indent the current line to column COL.
Indents such that first non-whitespace character is at column COL
Inserts spaces before markers at point."
  (save-excursion
    (beginning-of-line)
    (delete-horizontal-space)
    (idlwave-indent-to col)))

(defun idlwave-indent-subprogram ()
  "Indent program unit which contains point."
  (interactive)
  (save-excursion
    (idlwave-end-of-statement)
    (idlwave-beginning-of-subprogram)
    (let ((beg (point)))
      (idlwave-forward-block)
      (message "Indenting subprogram...")
      (indent-region beg (point) nil))
    (message "Indenting subprogram...done.")))

(defun idlwave-indent-statement ()
  "Indent current statement, including all continuation lines."
  (interactive)
  (save-excursion
    (idlwave-beginning-of-statement)
    (let ((beg (point)))
      (idlwave-end-of-statement)
      (indent-region beg (point) nil))))

(defun idlwave-calculate-indent ()
  "Return appropriate indentation for current line as IDL code."
  (save-excursion
    (beginning-of-line)
    (cond
     ;; Check for beginning of unit - main (beginning of buffer), pro, or
     ;; function
     ((idlwave-look-at idlwave-begin-unit-reg)
      0)
     ;; Check for continuation line
     ((save-excursion
        (and (= (forward-line -1) 0)
             (idlwave-is-continuation-line)))
      (idlwave-calculate-cont-indent))
     ;; calculate indent based on previous and current statements
     (t (let* (beg-prev-pos
	       (the-indent
		;; calculate indent based on previous statement
		(save-excursion
		  (cond
		   ;; Beginning of file
		   ((prog1
			(idlwave-previous-statement)
		      (setq beg-prev-pos (point)))
		    0)
		   ;; Main block
		   ((idlwave-look-at idlwave-begin-unit-reg t)
		    (+ (idlwave-current-statement-indent)
		       idlwave-main-block-indent))
		   ;; Begin block
		   ((idlwave-look-at idlwave-begin-block-reg t)
		    (+ (idlwave-min-current-statement-indent)
		       idlwave-block-indent))
		   ;; End Block
		   ((idlwave-look-at idlwave-end-block-reg t)
		    (progn
		      ;; Match to the *beginning* of the block opener
		      (goto-char beg-prev-pos)
		      (idlwave-block-jump-out -1 'nomark) ; go to begin block
		      (idlwave-min-current-statement-indent)))
		      ;;		      idlwave-end-offset
		      ;;		      idlwave-block-indent))

		   ;; Default to current indent
		   ((idlwave-current-statement-indent))))))
          ;; adjust the indentation based on the current statement
          (cond
           ;; End block
           ((idlwave-look-at idlwave-end-block-reg)
	    (+ the-indent idlwave-end-offset))
           (the-indent)))))))

;;
;; Parentheses indent
;;

(defun idlwave-calculate-paren-indent (beg-reg end-reg close-exp)
  "Calculate the continuation indent inside a paren group.
Returns a cons-cell with (open . indent), where open is the
location of the open paren."
  (let ((open (nth 1 (parse-partial-sexp beg-reg end-reg))))
    ;; Found an innermost open paren.
    (when open
      (goto-char open)
      ;; Line up with next word unless this is a closing paren.
      (cons open
	    (cond
	     ;; Plain Kernighan-style nested indent
	     (idlwave-indent-parens-nested
	      (+ idlwave-continuation-indent (idlwave-current-indent)))

	     ;; This is a closed paren - line up under open paren.
	     (close-exp
	      (current-column))

	     ;; Empty (or just comment) follows -- revert to basic indent
	     ((progn
		;; Skip paren
		(forward-char 1)
		(looking-at "[ \t$]*\\(;.*\\)?$"))
	      nil)

	     ;; Line up with first word after any blank space
	     ((progn
		(skip-chars-forward " \t")
		(current-column))))))))

(defun idlwave-calculate-cont-indent ()
  "Calculates the IDL continuation indent column from the previous statement.
Note that here previous statement usually means the beginning of the
current statement if this statement is a continuation of the previous
line.  Various special types of continuations, including assignments,
routine definitions, and parenthetical groupings, are treated separately."
  (save-excursion
    (let* ((case-fold-search t)
           (end-reg (progn (beginning-of-line) (point)))
	   (beg-last-statement (save-excursion (idlwave-previous-statement)
					       (point)))
           (beg-reg (progn (idlwave-start-of-substatement 'pre)
			   (if (eq (line-beginning-position) end-reg)
			       (goto-char beg-last-statement)
			     (point))))
	   (basic-indent (+ (idlwave-min-current-statement-indent end-reg)
			    idlwave-continuation-indent))
	   fancy-nonparen-indent fancy-paren-indent)
      (cond
       ;; Align then with its matching if, etc.
       ((let ((matchers '(("\\<if\\>" . "[ \t]*then")
			  ("\\<\\(if\\|end\\(if\\)?\\)\\>" . "[ \t]*else")
			  ("\\<\\(for\\|while\\)\\>" . "[ \t]*do")
			  ("\\<\\(repeat\\|end\\(rep\\)?\\)\\>" .
			   "[ \t]*until")
			  ("\\<case\\>" . "[ \t]*of")))
	      match cont-re)
	  (goto-char end-reg)
	  (and
	   (setq cont-re
		 (catch 'exit
		   (while (setq match (car matchers))
		     (if (looking-at (cdr match))
			 (throw 'exit (car match)))
		     (setq matchers (cdr matchers)))))
	   (idlwave-find-key cont-re -1 'nomark beg-last-statement)))
	(if (looking-at "end") ;; that one's special
	    (- (idlwave-current-indent)
	       (+ idlwave-block-indent idlwave-end-offset))
	  (idlwave-current-indent)))

       ;; Indent in from the previous line for continuing statements
       ((let ((matchers '("\\<then\\>"
			  "\\<do\\>"
			  "\\<repeat\\>"
			  "\\<else\\>"))
	      match)
	  (catch 'exit
	    (goto-char end-reg)
	    (if (/= (forward-line -1) 0)
		(throw 'exit nil))
	    (while (setq match (car matchers))
	      (if (looking-at (concat ".*" match "[ \t]*\\$[ \t]*"
				      "\\(;.*\\)?$"))
		  (throw 'exit t))
	      (setq matchers (cdr matchers)))))
	(+ idlwave-continuation-indent (idlwave-current-indent)))

       ;; Parenthetical indent, either traditional or Kernighan style
       ((setq fancy-paren-indent
	      (let* ((end-reg end-reg)
		    (close-exp (progn
				 (goto-char end-reg)
				 (skip-chars-forward " \t")
				 (looking-at "\\s)")))
		    indent-cons)
		(catch 'loop
		  (while (setq indent-cons (idlwave-calculate-paren-indent
					    beg-reg end-reg close-exp))
		    ;; First permitted containing paren
		    (if (or
			 idlwave-indent-to-open-paren
			 idlwave-indent-parens-nested
                         (null (cdr indent-cons))
			 (< (- (cdr indent-cons) basic-indent)
			    idlwave-max-extra-continuation-indent))
			(throw 'loop (cdr indent-cons)))
		    (setq end-reg (car indent-cons))))))
	fancy-paren-indent)

       ;; A continued assignment, or procedure call/definition
       ((and
	 (> idlwave-max-extra-continuation-indent 0)
	 (setq fancy-nonparen-indent
	       (progn
		 (goto-char beg-reg)
		 (while (idlwave-look-at "&"))  ; skip continued statements
		 (cond
		  ;; A continued Procedure call or definition
		  ((progn
		     (idlwave-look-at "^[ \t]*\\(pro\\|function\\)") ;skip over
		     (looking-at "[ \t]*\\([a-zA-Z0-9.$_]+[ \t]*->[ \t]*\\)?[a-zA-Z][:a-zA-Z0-9$_]*[ \t]*\\(,\\)[ \t]*"))
		   (goto-char (match-end 0))
		   ;; Comment only, or blank line with "$"?  Basic indent.
		   (if (save-match-data (looking-at "[ \t$]*\\(;.*\\)?$"))
		       nil
		     (current-column)))

		  ;; Continued assignment (with =):
		  ((catch 'assign ;
		     (while (looking-at "[^=\n\r]*\\(=\\)[ \t]*")
		       (goto-char (match-end 0))
		       (if (null (idlwave-what-function beg-reg))
			   (throw 'assign t))))
		   (unless (or
			    (idlwave-in-quote)
			    (looking-at "[ \t$]*\\(;.*\\)?$") ; use basic
			    (save-excursion
			      (goto-char beg-last-statement)
			      (eq (caar (idlwave-statement-type)) 'for)))
		     (current-column))))))
	 (< (- fancy-nonparen-indent basic-indent)
	    idlwave-max-extra-continuation-indent))
	(if fancy-paren-indent ;calculated but disallowed paren indent
	    (+ fancy-nonparen-indent idlwave-continuation-indent)
	  fancy-nonparen-indent))

       ;; Basic indent, by default
       (t basic-indent)))))



(defun idlwave-find-key (key-re &optional dir nomark limit)
  "Move to next match of the regular expression KEY-RE.
Matches inside comments or string constants will be ignored.
If DIR is negative, the search will be backwards.
At a successful match, the mark is pushed unless NOMARK is non-nil.
Searches are limited to LIMIT.
Searches are case-insensitive and use a special syntax table which
treats `$' and `_' as word characters.
Return value is the beginning of the match or (in case of failure) nil."
  (setq dir (or dir 0))
  (let ((case-fold-search t)
	(search-func (if (> dir 0) 're-search-forward 're-search-backward))
	found)
    (idlwave-with-special-syntax
     (save-excursion
       (catch 'exit
	 (while (funcall search-func key-re limit t)
	   (if (not (idlwave-quoted))
	       (throw 'exit (setq found (match-beginning 0)))
	     (if (or (and (> dir 0) (eobp))
		     (and (< dir 0) (bobp)))
		 (throw 'exit nil)))))))
    (if found
	(progn
	  (if (not nomark) (push-mark))
	  (goto-char found)
	  found)
      nil)))

(defun idlwave-block-jump-out (&optional dir nomark)
  "When optional argument DIR is non-negative, move forward to end of
current block using the `idlwave-begin-block-reg' and `idlwave-end-block-reg'
regular expressions.  When DIR is negative, move backwards to block beginning.
Recursively calls itself to skip over nested blocks.  DIR defaults to
forward.  Calls `push-mark' unless the optional argument NOMARK is
non-nil.  Movement is limited by the start of program units because of
possibility of unbalanced blocks."
  (interactive "P")
  (or dir (setq dir 0))
  (let* ((here (point))
         (case-fold-search t)
         (limit (if (>= dir 0) (point-max) (point-min)))
         (block-limit (if (>= dir 0)
			  idlwave-begin-block-reg
			idlwave-end-block-reg))
         found
         (block-reg (concat idlwave-begin-block-reg "\\|"
			    idlwave-end-block-reg))
         (unit-limit (or (save-excursion
			   (if (< dir 0)
			       (idlwave-find-key
				idlwave-begin-unit-reg dir t limit)
			     (end-of-line)
			     (idlwave-find-key
			      idlwave-end-unit-reg dir t limit)))
			 limit)))
    (if (>= dir 0) (end-of-line)) ;Make sure we are in current block
    (if (setq found (idlwave-find-key  block-reg dir t unit-limit))
        (while (and found (looking-at block-limit))
          (if (>= dir 0) (forward-word 1))
          (idlwave-block-jump-out dir t)
          (setq found (idlwave-find-key block-reg dir t unit-limit))))
    (if (not nomark) (push-mark here))
    (if (not found) (goto-char unit-limit)
      (if (>= dir 0) (forward-word 1)))))

(defun idlwave-min-current-statement-indent (&optional end-reg)
  "The minimum indent in the current statement."
  (idlwave-beginning-of-statement)
  (if (not (idlwave-is-continuation-line))
      (idlwave-current-indent)
    (let ((min (idlwave-current-indent)) comm-or-empty)
      (while (and (= (forward-line 1) 0)
		  (or (setq comm-or-empty (idlwave-is-comment-or-empty-line))
		      (idlwave-is-continuation-line))
		  (or (null end-reg) (< (point) end-reg)))
	(unless comm-or-empty (setq min (min min (idlwave-current-indent)))))
      (if (or comm-or-empty (and end-reg (>= (point) end-reg)))
	  min
	(min min (idlwave-current-indent))))))

(defun idlwave-current-statement-indent (&optional last-line)
  "Return indentation of the current statement.
If in a statement, moves to beginning of statement before finding indent."
  (if last-line
      (idlwave-end-of-statement)
    (idlwave-beginning-of-statement))
  (idlwave-current-indent))

(defun idlwave-current-indent ()
  "Return the column of the indentation of the current line.
Skips any whitespace.  Returns 0 if the end-of-line follows the whitespace."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    ;; if we are at the end of blank line return 0
    (cond ((eolp) 0)
          ((current-column)))))

(defun idlwave-is-continuation-line ()
  "Test if current line is continuation line.
Blank or comment-only lines following regular continuation lines (with
`$') count as continuations too."
  (let (p)
    (save-excursion
      (or
       (idlwave-look-at "\\<\\$")
       (catch 'loop
	 (while (and (looking-at "^[ \t]*\\(;.*\\)?$")
		     (eq (forward-line -1) 0))
	   (if (setq p (idlwave-look-at "\\<\\$")) (throw 'loop p))))))))

(defun idlwave-is-comment-line ()
  "Test if the current line is a comment line."
  (save-excursion
    (beginning-of-line 1)
    (looking-at "[ \t]*;")))

(defun idlwave-is-comment-or-empty-line ()
  "Test if the current line is a comment line."
  (save-excursion
    (beginning-of-line 1)
    (looking-at "[ \t]*[;\n]")))

(defun idlwave-look-at (regexp &optional cont beg)
  "Search current line from current point for REGEXP.
If optional argument CONT is non-nil, searches to the end of
the current statement.
If optional arg BEG is non-nil, search starts from the beginning of the
current statement.
Ignores matches that end in a comment or inside a string expression.
Returns point if successful, nil otherwise.
This function produces unexpected results if REGEXP contains quotes or
a comment delimiter.  The search is case insensitive.
If successful leaves point after the match, otherwise, does not move point."
  (let ((here (point))
        (case-fold-search t)
        (eos (save-excursion
	       (if cont (idlwave-end-of-statement) (end-of-line))
	       (point)))
        found)
    (idlwave-with-special-syntax
     (if beg (idlwave-beginning-of-statement))
     (while (and (setq found (re-search-forward regexp eos t))
		 (idlwave-quoted))))
    (if (not found) (goto-char here))
    found))

(defun idlwave-fill-paragraph (&optional nohang)
  "Fill paragraphs in comments.
A paragraph is made up of all contiguous lines having the same comment
leader (the leading whitespace before the comment delimiter and the
comment delimiter).  In addition, paragraphs are separated by blank
line comments.  The indentation is given by the hanging indent of the
first line, otherwise by the minimum indentation of the lines after
the first line.  The indentation of the first line does not change.
Does not effect code lines.  Does not fill comments on the same line
with code.  The hanging indent is given by the end of the first match
matching `idlwave-hang-indent-regexp' on the paragraph's first line.
If the optional argument NOHANG is non-nil then the hanging indent is
ignored."
  (interactive "P")
  ;; check if this is a line comment
  (if (save-excursion
        (beginning-of-line)
        (skip-chars-forward " \t")
        (looking-at comment-start))
      (let
          ((indent 999)
           pre here diff fill-prefix-reg bcl first-indent
           hang start end)
        ;; Change tabs to spaces in the surrounding paragraph.
        ;; The surrounding paragraph will be the largest containing block of
        ;; contiguous line comments. Thus, we may be changing tabs in
        ;; a much larger area than is needed, but this is the easiest
        ;; brute force way to do it.
        ;;
        ;; This has the undesirable side effect of replacing the tabs
        ;; permanently without the user's request or knowledge.
        (save-excursion
          (backward-paragraph)
          (setq start (point)))
        (save-excursion
          (forward-paragraph)
          (setq end (point)))
        (untabify start end)
        ;;
        (setq here (point))
        (beginning-of-line)
        (setq bcl (point))
        (re-search-forward (concat "^[ \t]*" comment-start "+")
			   (point-at-eol) t)
        ;; Get the comment leader on the line and its length
        (setq pre (current-column))
        ;; the comment leader is the indentation plus exactly the
        ;; number of consecutive ";".
        (setq fill-prefix-reg
              (concat
               (setq fill-prefix
                     (regexp-quote (buffer-substring (point-at-bol) (point))))
               "[^;]"))

        ;; Mark the beginning and end of the paragraph
        (goto-char bcl)
        (while (and (looking-at fill-prefix-reg)
                    (not (looking-at paragraph-separate))
                    (not (bobp)))
          (forward-line -1))
        ;; Move to first line of paragraph
        (if (/= (point) bcl)
            (forward-line 1))
        (setq start (point))
        (goto-char bcl)
        (while (and (looking-at fill-prefix-reg)
                    (not (looking-at paragraph-separate))
                    (not (eobp)))
          (forward-line 1))
        (beginning-of-line)
        (if (or (not (looking-at fill-prefix-reg))
                (looking-at paragraph-separate))
            (forward-line -1))
        (end-of-line)
        ;; if at end of buffer add a newline (need this because
        ;; fill-region needs END to be at the beginning of line after
        ;; the paragraph or it will add a line).
        (if (eobp)
            (progn (insert ?\n) (backward-char 1)))
        ;; Set END to the beginning of line after the paragraph
        ;; END is calculated as distance from end of buffer
        (setq end (- (point-max) (point) 1))
        ;;
        ;; Calculate the indentation for the paragraph.
        ;;
        ;; In the following while statements, after one iteration
        ;; point will be at the beginning of a line in which case
        ;; the while will not be executed for the
        ;; the first paragraph line and thus will not affect the
        ;; indentation.
        ;;
        ;; First check to see if indentation is based on hanging indent.
        (if (and (not nohang) idlwave-hanging-indent
                 (setq hang
                       (save-excursion
                         (goto-char start)
                         (idlwave-calc-hanging-indent))))
            ;; Adjust lines of paragraph by inserting spaces so that
            ;; each line's indent is at least as great as the hanging
            ;; indent. This is needed for fill-paragraph to work with
            ;; a fill-prefix.
            (progn
              (setq indent hang)
              (beginning-of-line)
              (while (> (point) start)
                (re-search-forward comment-start-skip (point-at-eol) t)
                (if (> (setq diff (- indent (current-column))) 0)
                    (progn
                      (if (>= here (point))
                          ;; adjust the original location for the
                          ;; inserted text.
                          (setq here (+ here diff)))
                      (insert (make-string diff ?\ ))))
                (forward-line -1))
              )

          ;; No hang. Instead find minimum indentation of paragraph
          ;; after first line.
          ;; For the following while statement, since START is at the
          ;; beginning of line and END is at the end of line
          ;; point is greater than START at least once (which would
          ;; be the case for a single line paragraph).
          (while (> (point) start)
            (beginning-of-line)
            (setq indent
                  (min indent
                       (progn
                         (re-search-forward comment-start-skip (point-at-eol) t)
                         (current-column))))
            (forward-line -1)))
        (setq fill-prefix (concat fill-prefix
                                  (make-string (- indent pre)
                                               ?\ )))
        ;; first-line indent
        (setq first-indent
              (max
               (progn
                 (re-search-forward comment-start-skip (point-at-eol) t)
                 (current-column))
               indent))

        ;; try to keep point at its original place
        (goto-char here)

        ;; In place of the more modern fill-region-as-paragraph, a hack
        ;; to keep whitespace untouched on the first line within the
        ;; indent length and to preserve any indent on the first line
        ;; (first indent).
        (save-excursion
          (setq diff
                (buffer-substring start (+ start first-indent -1)))
          (subst-char-in-region start (+ start first-indent -1) ?\  ?~ nil)
          (fill-region-as-paragraph
           start
           (- (point-max) end)
           (current-justification)
           nil)
          (delete-region start (+ start first-indent -1))
          (goto-char start)
          (insert diff))
        ;; When we want the point at the beginning of the comment
        ;; body fill-region will put it at the beginning of the line.
        (if (bolp) (skip-chars-forward (concat " \t" comment-start)))
        (setq fill-prefix nil))))

(defun idlwave-calc-hanging-indent ()
  "Calculate the position of the hanging indent for the comment paragraph.
The hanging indent position is given by the first match with the
`idlwave-hang-indent-regexp'.  If `idlwave-use-last-hang-indent' is
non-nil then use last occurrence matching `idlwave-hang-indent-regexp'
on the line.
If not found returns nil."
  (if idlwave-use-last-hang-indent
      (save-excursion
        (end-of-line)
        (if (re-search-backward idlwave-hang-indent-regexp (point-at-bol) t)
            (+ (current-column) (length idlwave-hang-indent-regexp))))
    (save-excursion
      (beginning-of-line)
      (if (re-search-forward idlwave-hang-indent-regexp (point-at-eol) t)
          (current-column)))))

(defun idlwave-auto-fill ()
  "Called to break lines in auto fill mode.
Only fills non-comment lines if `idlwave-fill-comment-line-only' is
non-nil.  Places a continuation character at the end of the line if
not in a comment.  Splits strings with IDL concatenation operator `+'
if `idlwave-auto-fill-split-string' is non-nil."
  (if (<= (current-column) fill-column)
      nil                             ; do not to fill
    (if (or (not idlwave-fill-comment-line-only)
	    (save-excursion
	      ;; Check for comment line
	      (beginning-of-line)
	      (looking-at idlwave-comment-line-start-skip)))
	(let (beg)
	  (idlwave-indent-line)
	  ;; Prevent actions do-auto-fill which calls indent-line-function.
	  (let (idlwave-do-actions
		(paragraph-separate ".")
		(fill-nobreak-predicate
		 (if (and (idlwave-in-quote)
			  idlwave-auto-fill-split-string)
		     (lambda () ;; We'll need 5 spaces for " ' + $"
		       (<= (- fill-column (current-column)) 5)
		       ))))
	    (do-auto-fill))
	  (save-excursion
	    (end-of-line 0)
	    ;; Indent the split line
	    (idlwave-indent-line))
	  (if (save-excursion
		(beginning-of-line)
		(looking-at idlwave-comment-line-start-skip))
	      ;; A continued line comment
	      ;; We treat continued line comments as part of a comment
	      ;; paragraph. So we check for a hanging indent.
	      (if idlwave-hanging-indent
		  (let ((here (- (point-max) (point)))
			(indent
			 (save-excursion
			   (forward-line -1)
			   (idlwave-calc-hanging-indent))))
		    (when indent
		      ;; Remove whitespace between comment delimiter and
		      ;; text, insert spaces for appropriate indentation.
		      (beginning-of-line)
		      (re-search-forward comment-start-skip (point-at-eol) t)
		      (delete-horizontal-space)
		      (idlwave-indent-to indent)
		      (goto-char (- (point-max) here)))))
	    ;; Split code or comment?
	    (if (save-excursion
		  (end-of-line 0)
		  (idlwave-in-comment))
		;; Splitting a non-full-line comment.
		;; Insert the comment delimiter from split line
		(progn
		  (save-excursion
		    (beginning-of-line)
		    (skip-chars-forward " \t")
		    ;; Insert blank to keep off beginning of line
		    (insert " "
			    (save-excursion
			      (forward-line -1)
			      (buffer-substring (idlwave-goto-comment)
						(progn
						  (skip-chars-forward "; ")
						  (point))))))
		  (idlwave-indent-line))
	      ;; Split code line - add continuation character
	      (save-excursion
		(end-of-line 0)
		;; Check to see if we split a string
		(if (and (setq beg (idlwave-in-quote))
			 idlwave-auto-fill-split-string)
		    ;; Split the string and concatenate.
		    ;; The first extra space is for the space
		    ;; the line was split. That space was removed.
		    (insert " " (char-after beg) " +"))
		(insert " $"))
	      (if beg
		  (if idlwave-auto-fill-split-string
		      ;; Make the second part of continued string
		      (save-excursion
			(beginning-of-line)
			(skip-chars-forward " \t")
			(insert (char-after beg)))
		    ;; Warning
		    (beep)
		    (message "Warning: continuation inside a string.")))
	      ;; Although do-auto-fill (via indent-new-comment-line) calls
	      ;; idlwave-indent-line for the new line, re-indent again
	      ;; because of the addition of the continuation character.
	      (idlwave-indent-line))
	    )))))

(defun idlwave-auto-fill-mode (arg)
  "Toggle auto-fill mode for IDL mode.
With arg, turn auto-fill mode on if arg is positive.
In auto-fill mode, inserting a space at a column beyond `fill-column'
automatically breaks the line at a previous space."
  (interactive "P")
  (prog1 (set idlwave-fill-function
              (if (if (null arg)
                      (not (symbol-value idlwave-fill-function))
                    (> (prefix-numeric-value arg) 0))
                  'idlwave-auto-fill
                nil))
    ;; update mode-line
    (set-buffer-modified-p (buffer-modified-p))))

;(defun idlwave-fill-routine-call ()
;  "Fill a routine definition or statement, indenting appropriately."
;  (let ((where (idlwave-where)))))


(defun idlwave-doc-header (&optional nomark)
  "Insert a documentation header at the beginning of the unit.
Inserts the value of the variable `idlwave-file-header'.  Sets mark
before moving to do insertion unless the optional prefix argument
NOMARK is non-nil."
  (interactive "P")
  (or nomark (push-mark))
  ;; make sure we catch the current line if it begins the unit
  (if idlwave-header-to-beginning-of-file
      (goto-char (point-min))
    (end-of-line)
    (idlwave-beginning-of-subprogram)
    (beginning-of-line)
    ;; skip function or procedure line
    (if (idlwave-look-at "\\<\\(pro\\|function\\)\\>")
	(progn
	  (idlwave-end-of-statement)
	  (if (> (forward-line 1) 0) (insert "\n")))))
  (let ((pos (point)))
    (if idlwave-file-header
	(cond ((car idlwave-file-header)
	       (insert-file-contents (car idlwave-file-header)))
	      ((stringp (car (cdr idlwave-file-header)))
	       (insert (car (cdr idlwave-file-header))))))
    (goto-char pos)))

(defun idlwave-default-insert-timestamp ()
  "Default timestamp insertion function."
  (insert (current-time-string))
  (insert ", " (user-full-name))
  (if (boundp 'user-mail-address)
      (insert " <" user-mail-address ">")
    (insert " <" (user-login-name) "@" (system-name) ">"))
  ;; Remove extra spaces from line
  (idlwave-fill-paragraph)
  ;; Insert a blank line comment to separate from the date entry -
  ;; will keep the entry from flowing onto date line if re-filled.
  (insert "\n;\n;\t\t"))

(defun idlwave-doc-modification ()
  "Insert a brief modification log at the beginning of the current program.
Looks for an occurrence of the value of user variable
`idlwave-doc-modifications-keyword' if non-nil.  Inserts time and user
name and places the point for the user to add a log.  Before moving, saves
location on mark ring so that the user can return to previous point."
  (interactive)
  (push-mark)
  (let* (beg end)
    (if (and (or (re-search-backward idlwave-doclib-start nil t)
		 (progn
		   (goto-char (point-min))
		   (re-search-forward idlwave-doclib-start nil t)))
	     (setq beg (match-beginning 0))
	     (re-search-forward idlwave-doclib-end nil t)
	     (setq end (match-end 0)))
	(progn
	  (goto-char beg)
	  (if (re-search-forward
	       (concat idlwave-doc-modifications-keyword ":")
	       end t)
	      (end-of-line)
	    (goto-char end)
	    (end-of-line -1)
	    (insert "\n" comment-start "\n")
	    (insert comment-start " " idlwave-doc-modifications-keyword ":"))
	  (insert "\n;\n;\t")
	  (run-hooks 'idlwave-timestamp-hook))
      (error "No valid DOCLIB header"))))


;; CJC 3/16/93
;; Interface to expand-region-abbrevs which did not work when the
;; abbrev hook associated with an abbrev moves point backwards
;; after abbrev expansion, e.g., as with the abbrev '.n'.
;; The original would enter an infinite loop in attempting to expand
;; .n (it would continually expand and unexpand the abbrev without expanding
;; because the point would keep going back to the beginning of the
;; abbrev instead of to the end of the abbrev). We now keep the
;; abbrev hook from moving backwards.
;;;
(defun idlwave-expand-region-abbrevs (start end)
  "Expand each abbrev occurrence in the region.
Calling from a program, arguments are START END."
  (interactive "r")
  (save-excursion
    (goto-char (min start end))
    (let ((idlwave-show-block nil)          ;Do not blink
          (idlwave-abbrev-move nil))        ;Do not move
      (expand-region-abbrevs start end 'noquery))))

(defun idlwave-quoted ()
  "Return t if point is in a comment or quoted string.
Returns nil otherwise."
  (or (idlwave-in-comment) (idlwave-in-quote)))

(defun idlwave-in-quote ()
  "Return location of the opening quote
if point is in a IDL string constant, nil otherwise.
Ignores comment delimiters on the current line.
Properly handles nested quotation marks and octal
constants - a double quote followed by an octal digit."
;; Treat an octal inside an apostrophe to be a normal string. Treat a
;; double quote followed by an octal digit to be an octal constant
;; rather than a string. Therefore, there is no terminating double
;; quote.
  (save-excursion
    ;; Because single and double quotes can quote each other we must
    ;; search for the string start from the beginning of line.
    (let* ((start (point))
           (eol (point-at-eol))
           (bq (progn (beginning-of-line) (point)))
           (endq (point))
           (data (match-data))
           delim
           found)
      (while  (< endq start)
	;; Find string start
	;; Don't find an octal constant beginning with a double quote
	(if (re-search-forward "[\"']" eol 'lim)
	    ;; Find the string end.
	    ;; In IDL, two consecutive delimiters after the start of a
	    ;; string act as an
	    ;; escape for the delimiter in the string.
	    ;; Two consecutive delimiters alone (i.e., not after the
	    ;; start of a string) is the null string.
	    (progn
	      ;; Move to position after quote
	      (goto-char (1+ (match-beginning 0)))
	      (setq bq (1- (point)))
	      ;; Get the string delimiter
	      (setq delim (char-to-string (preceding-char)))
	      ;; Check for null string
	      (if (looking-at delim)
		  (progn (setq endq (point)) (forward-char 1))
		;; Look for next unpaired delimiter
		(setq found (search-forward delim eol 'lim))
		(while (looking-at delim)
		  (forward-char 1)
		  (setq found (search-forward delim eol 'lim)))
		(setq endq (if found (1- (point)) (point)))
		))
	  (progn (setq bq (point)) (setq endq (point)))))
      (store-match-data data)
      ;; return string beginning position or nil
      (if (> start bq) bq))))

(defun idlwave-is-pointer-dereference (&optional limit)
  "Determine if the character after point is a pointer dereference *."
  (and
   (eq (char-after) ?\*)
   (not (idlwave-in-quote))
   (save-excursion
     (forward-char)
     (re-search-backward (concat "\\(" idlwave-idl-keywords
                                 "\\|[[(*+-/=,^><]\\)\\s-*\\*") limit t))))


;; Statement templates

;; Replace these with a general template function, something like
;; expand.el (I think there was also something with a name similar to
;; dmacro.el)

(defun idlwave-template (s1 s2 &optional prompt noindent)
  "Build a template with optional prompt expression.

Opens a line if point is not followed by a newline modulo intervening
whitespace.  S1 and S2 are strings.  S1 is inserted at point followed
by S2.  Point is inserted between S1 and S2.  The case of S1 and S2 is
adjusted according to `idlwave-abbrev-change-case'.  If optional
argument PROMPT is a string then it is displayed as a message in the
minibuffer.  The PROMPT serves as a reminder to the user of an
expression to enter.

The lines containing S1 and S2 are reindented using `indent-region'
unless the optional second argument NOINDENT is non-nil."
  (if (derived-mode-p 'idlwave-shell-mode)
      ;; This is a gross hack to avoit template abbrev expansion
      ;; in the shell.  FIXME: This is a dirty hack.
      (if (and (eq this-command 'self-insert-command)
	       (equal last-abbrev-location (point)))
	  (insert last-abbrev-text)
	(error "No templates in idlwave-shell"))
    (cond ((eq idlwave-abbrev-change-case 'down)
	   (setq s1 (downcase s1) s2 (downcase s2)))
	  (idlwave-abbrev-change-case
	   (setq s1 (upcase s1) s2 (upcase s2))))
    (let ((beg (point-at-bol))
	  end)
      (if (not (looking-at "\\s-*\n"))
	  (open-line 1))
      (insert s1)
      (save-excursion
	(insert s2)
	(setq end (point)))
      (if (not noindent)
	  (indent-region beg end nil))
      (if (stringp prompt)
	  (message "%s" prompt)))))

(defun idlwave-rw-case (string)
  "Make STRING have the case required by `idlwave-reserved-word-upcase'."
  (if idlwave-reserved-word-upcase
      (upcase string)
    string))

(defun idlwave-elif ()
  "Build skeleton IDL if-else block."
  (interactive)
  (idlwave-template
   (idlwave-rw-case "if")
   (idlwave-rw-case " then begin\n\nendif else begin\n\nendelse")
   "Condition expression"))

(defun idlwave-case ()
  "Build skeleton IDL case statement."
  (interactive)
  (idlwave-template
   (idlwave-rw-case "case")
   (idlwave-rw-case " of\n\nendcase")
   "Selector expression"))

(defun idlwave-switch ()
  "Build skeleton IDL switch statement."
  (interactive)
  (idlwave-template
   (idlwave-rw-case "switch")
   (idlwave-rw-case " of\n\nendswitch")
   "Selector expression"))

(defun idlwave-for ()
  "Build skeleton IDL loop statement."
  (interactive)
  (idlwave-template
   (idlwave-rw-case "for")
   (idlwave-rw-case " do begin\n\nendfor")
   "Loop expression"))

(defun idlwave-if ()
  "Build skeleton IDL if statement."
  (interactive)
  (idlwave-template
   (idlwave-rw-case "if")
   (idlwave-rw-case " then begin\n\nendif")
   "Scalar logical expression"))

(defun idlwave-procedure ()
  (interactive)
  (idlwave-template
   (idlwave-rw-case "pro")
   (idlwave-rw-case "\n\nreturn\nend")
   "Procedure name"))

(defun idlwave-function ()
  (interactive)
  (idlwave-template
   (idlwave-rw-case "function")
   (idlwave-rw-case "\n\nreturn\nend")
   "Function name"))

(defun idlwave-repeat ()
  (interactive)
  (idlwave-template
   (idlwave-rw-case "repeat begin\n\nendrep until")
   (idlwave-rw-case "")
   "Exit condition"))

(defun idlwave-while ()
  (interactive)
  (idlwave-template
   (idlwave-rw-case "while")
   (idlwave-rw-case " do begin\n\nendwhile")
   "Entry condition"))

(defun idlwave-split-string (string &optional pattern)
  "Return a list of substrings of STRING which are separated by PATTERN.
If PATTERN is omitted, it defaults to \"[ \\f\\t\\n\\r\\v]+\"."
  (or pattern
      (setq pattern "[ \f\t\n\r\v]+"))
  (let (parts (start 0))
    (while (string-match pattern string start)
      (setq parts (cons (substring string start (match-beginning 0)) parts)
	    start (match-end 0)))
    (nreverse (cons (substring string start) parts))))

(defun idlwave-replace-string (string replace_string replace_with)
  (let* ((start 0)
	 (last (length string))
	 (ret_string "")
	 end)
    (while (setq end (string-match replace_string string start))
      (setq ret_string
	    (concat ret_string (substring string start end) replace_with))
      (setq start (match-end 0)))
    (setq ret_string (concat ret_string (substring string start last)))))

(defun idlwave-get-buffer-visiting (file)
  ;; Return the buffer currently visiting FILE
  (cond
   ((boundp 'find-file-compare-truenames) ; XEmacs
    (let ((find-file-compare-truenames t))
      (get-file-buffer file)))
   ((fboundp 'find-buffer-visiting)       ; Emacs
    (find-buffer-visiting file))
   (t (error "This should not happen (idlwave-get-buffer-visiting)"))))

(defvar idlwave-outlawed-buffers nil
  "List of buffers pulled up by IDLWAVE for special reasons.
Buffers in this list may be killed by `idlwave-kill-autoloaded-buffers'.")

(defun idlwave-find-file-noselect (file &optional why)
  ;; Return a buffer visiting file.
  (or (idlwave-get-buffer-visiting file)
      (let ((buf (find-file-noselect file)))
	(if why (add-to-list 'idlwave-outlawed-buffers (cons buf why)))
	buf)))

(defun idlwave-kill-autoloaded-buffers ()
  "Kill buffers created automatically by IDLWAVE.
Function prompts for a letter to identify the buffers to kill.
Possible letters are:

f    Buffers created by the command \\[idlwave-find-module] or mouse
     clicks in the routine info window.
s    Buffers created by the IDLWAVE Shell to display where execution
     stopped or an error was found.
a    Both of the above.

Buffers containing unsaved changes require confirmation before they are killed."
  (interactive)
  (if (null idlwave-outlawed-buffers)
      (error "No IDLWAVE-created buffers available")
    (princ (format "Kill IDLWAVE-created buffers: [f]ind source(%d), [s]hell display(%d), [a]ll ? "
		   (idlwave-count-outlawed-buffers 'find)
		   (idlwave-count-outlawed-buffers 'shell)))
    (let ((c (read-char)))
      (cond
       ((member c '(?f ?\C-f))
	(idlwave-do-kill-autoloaded-buffers 'find))
       ((member c '(?s ?\C-s))
	(idlwave-do-kill-autoloaded-buffers 'shell))
       ((member c '(?a ?\C-a))
	(idlwave-do-kill-autoloaded-buffers t))
       (t (error "Abort"))))))

(defun idlwave-count-outlawed-buffers (tag)
  "How many outlawed buffers have tag TAG?"
  (length (delq nil
		(mapcar
		 (lambda (x) (eq (cdr x) tag))
		 idlwave-outlawed-buffers))))

(defun idlwave-do-kill-autoloaded-buffers (&rest reasons)
  "Kill all buffers pulled up by IDLWAVE matching REASONS."
  (let* ((list (copy-sequence idlwave-outlawed-buffers))
	 (cnt 0)
	 entry)
    (while (setq entry (pop list))
      (if (buffer-live-p (car entry))
	  (and (or (memq t reasons)
		   (memq (cdr entry) reasons))
	       (kill-buffer (car entry))
	       (incf cnt)
	       (setq idlwave-outlawed-buffers
		     (delq entry idlwave-outlawed-buffers)))
	(setq idlwave-outlawed-buffers
	      (delq entry idlwave-outlawed-buffers))))
    (message "%d buffer%s killed" cnt (if (= cnt 1) "" "s"))))

(defun idlwave-revoke-license-to-kill ()
  "Remove BUFFER from the buffers which may be killed.
Killing would be done by `idlwave-do-kill-autoloaded-buffers'.
Intended for `after-save-hook'."
  (let* ((buf (current-buffer))
	 (entry (assq buf idlwave-outlawed-buffers)))
    ;; Revoke license
    (if entry
	(setq idlwave-outlawed-buffers
	      (delq entry idlwave-outlawed-buffers)))
    ;; Remove this function from the hook.
    (remove-hook 'after-save-hook 'idlwave-revoke-license-to-kill 'local)))

(defvar idlwave-path-alist)
(defun idlwave-locate-lib-file (file)
  ;; Find FILE on the scanned lib path and return a buffer visiting it
  (let* ((dirs idlwave-path-alist)
	 dir efile)
    (catch 'exit
      (while (setq dir (car (pop dirs)))
	(if (file-regular-p
	     (setq efile (expand-file-name file dir)))
	    (throw 'exit efile))))))

(defun idlwave-expand-lib-file-name (file)
  ;; Find FILE on the scanned lib path and return a buffer visiting it
  ;; This is for, e.g., finding source with no user catalog
  (cond
   ((null file) nil)
   ((file-name-absolute-p file) file)
   (t (idlwave-locate-lib-file file))))

(defun idlwave-make-tags ()
  "Create the IDL tags file IDLTAGS in the current directory from
the list of directories specified in the minibuffer.  Directories may be
for example: . /usr/local/rsi/idl/lib.  All the subdirectories of the
specified top directories are searched if the directory name is prefixed
by @.  Specify @ directories with care, it may take a long, long time if
you specify /."
  (interactive)
  (let (directory directories cmd append status numdirs dir getsubdirs
		  buffer save_buffer files numfiles item errbuf)

    ;;
    ;; Read list of directories
    (setq directory (read-string "Tag Directories: " "."))
    (setq directories (idlwave-split-string directory "[ \t]+"))
    ;;
    ;; Set etags command, vars
    (setq cmd "etags --output=IDLTAGS --language=none --regex='/[
\\t]*[pP][Rr][Oo][ \\t]+\\([^ \\t,]+\\)/' --regex='/[
\\t]*[Ff][Uu][Nn][Cc][Tt][Ii][Oo][Nn][ \\t]+\\([^ \\t,]+\\)/' ")
    (setq append " ")
    (setq status 0)
    ;;
    ;; For each directory
    (setq numdirs 0)
    (setq dir (nth numdirs directories))
    (while (and dir)
      ;;
      ;; Find the subdirectories
      (if (string-match "^[@]\\(.+\\)$" dir)
	  (setq getsubdirs t) (setq getsubdirs nil))
      (if (and getsubdirs) (setq dir (substring dir 1 (length dir))))
      (setq dir (expand-file-name dir))
      (if (file-directory-p dir)
	  (progn
	    (if (and getsubdirs)
		(progn
		  (setq buffer (get-buffer-create "*idltags*"))
		  (call-process "sh" nil buffer nil "-c"
				(concat "find " dir " -type d -print"))
		  (setq save_buffer (current-buffer))
		  (set-buffer buffer)
		  (setq files (idlwave-split-string
			       (idlwave-replace-string
				(buffer-substring 1 (point-max))
				"\n" "/*.pro ")
			       "[ \t]+"))
		  (set-buffer save_buffer)
		  (kill-buffer buffer))
	      (setq files (list (concat dir "/*.pro"))))
	    ;;
	    ;; For each subdirectory
	    (setq numfiles 0)
	    (setq item (nth numfiles files))
	    (while (and item)
	      ;;
	      ;; Call etags
	      (if (not (string-match "^[ \\t]*$" item))
		  (progn
		    (message "%s" (concat "Tagging " item "..."))
		    (setq errbuf (get-buffer-create "*idltags-error*"))
		    (setq status (+ status
				    (if (eq 0 (call-process
					       "sh" nil errbuf nil "-c"
					       (concat cmd append item)))
					0
  	                                1)))
		    ;;
		    ;; Append additional tags
		    (setq append " --append ")
		    (setq numfiles (1+ numfiles))
		    (setq item (nth numfiles files)))
		(progn
		  (setq numfiles (1+ numfiles))
		  (setq item (nth numfiles files))
		  )))

	    (setq numdirs (1+ numdirs))
	    (setq dir (nth numdirs directories)))
	(progn
	  (setq numdirs (1+ numdirs))
	  (setq dir (nth numdirs directories)))))

    (setq errbuf (get-buffer-create "*idltags-error*"))
    (if (= status 0)
	(kill-buffer errbuf))
    (message "")
    ))

(defun idlwave-toggle-comment-region (beg end &optional n)
  "Comment the lines in the region if the first non-blank line is
commented, and conversely, uncomment region.  If optional prefix arg
N is non-nil, then for N positive, add N comment delimiters or for N
negative, remove N comment delimiters.
Uses `comment-region' which does not place comment delimiters on
blank lines."
  (interactive "r\nP")
  (if n
      (comment-region beg end (prefix-numeric-value n))
    (save-excursion
      (goto-char beg)
      (beginning-of-line)
      ;; skip blank lines
      (skip-chars-forward " \t\n")
      (if (looking-at (concat "[ \t]*\\(" comment-start "+\\)"))
	  (if (fboundp 'uncomment-region)
	      (uncomment-region beg end)
	    (comment-region beg end
			    (- (length (buffer-substring
					(match-beginning 1)
					(match-end 1))))))
	(comment-region beg end)))))


;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------
;;
;; Completion and Routine Info
;;

;; String "intern" functions

;; For the completion and routine info function, we want to normalize
;; the case of procedure names etc.  We do this by "interning" these
;; string is a hand-crafted way.  Hashes are used to map the downcase
;; version of the strings to the cased versions.  Most *-sint-*
;; variables consist of *two* hashes, a buffer+shell, followed by a
;; system hash.  The former is re-scanned, and the latter takes case
;; precedence.
;;
;; Since these cased versions are really lisp objects, we can use `eq'
;; to search, which is a large performance boost.  All new strings
;; need to be "sinterned".  We do this as early as possible after
;; getting these strings from completion or buffer substrings.  So
;; most of the code can simply assume to deal with "sinterned"
;; strings.  The only exception is that the functions which scan whole
;; buffers for routine information do not intern the grabbed strings.
;; This is only done afterwards.  Therefore in these functions it is
;; *not* safe to assume the strings can be compared with `eq' and be
;; fed into the routine assq functions.

;; Here we define the hashing functions.

;; The variables which hold the hashes.
(defvar idlwave-sint-routines '(nil))
(defvar idlwave-sint-keywords '(nil))
(defvar idlwave-sint-methods  '(nil))
(defvar idlwave-sint-classes  '(nil))
(defvar idlwave-sint-dirs    '(nil))
(defvar idlwave-sint-libnames '(nil))

(defun idlwave-reset-sintern (&optional what)
  "Reset all sintern hashes."
  ;; Make sure the hash functions are accessible.
  (unless (and (fboundp 'gethash)
               (fboundp 'puthash))
    (require 'cl)
    (or (fboundp 'puthash)
        (defalias 'puthash 'cl-puthash)))
  (let ((entries '((idlwave-sint-routines 1000 10)
		   (idlwave-sint-keywords 1000 10)
		   (idlwave-sint-methods   100 10)
		   (idlwave-sint-classes    10 10))))

    ;; Make sure these are lists
    (loop for entry in entries
      for var = (car entry)
      do (if (not (consp (symbol-value var))) (set var (list nil))))

    ;; Reset the system & library hash
    (when (or (eq what t) (eq what 'syslib)
	      (null (cdr idlwave-sint-routines)))
      (loop for entry in entries
	for var = (car entry) for size = (nth 1 entry)
	do (setcdr (symbol-value var)
		   (make-hash-table ':size size ':test 'equal)))
      (setq idlwave-sint-dirs nil
	    idlwave-sint-libnames nil))

    ;; Reset the buffer & shell hash
    (when (or (eq what t) (eq what 'bufsh)
	      (null (car idlwave-sint-routines)))
      (loop for entry in entries
	for var = (car entry) for size = (nth 1 entry)
	do (setcar (symbol-value var)
		   (make-hash-table ':size size ':test 'equal))))))

(defun idlwave-sintern-routine-or-method (name &optional class set)
  (if class
      (idlwave-sintern-method name set)
    (idlwave-sintern-routine name set)))

(defun idlwave-sintern (stype &rest args)
  (apply (intern (concat "idlwave-sintern-" (symbol-name stype))) args))

;;(defmacro idlwave-sintern (type var)
;;  `(cond ((not (stringp name)) name)
;;	 ((gethash (downcase name) (cdr ,var)))
;;	 ((gethash (downcase name) (car ,var)))
;;	 (set (idlwave-sintern-set name ,type ,var set))
;;	 (name)))

(defun idlwave-sintern-routine (name &optional set)
  (cond ((not (stringp name)) name)
	((gethash (downcase name) (cdr idlwave-sint-routines)))
	((gethash (downcase name) (car idlwave-sint-routines)))
	(set (idlwave-sintern-set name 'routine idlwave-sint-routines set))
	(name)))
(defun idlwave-sintern-keyword (name &optional set)
  (cond ((not (stringp name)) name)
	((gethash (downcase name) (cdr idlwave-sint-keywords)))
	((gethash (downcase name) (car idlwave-sint-keywords)))
	(set (idlwave-sintern-set name 'keyword idlwave-sint-keywords set))
	(name)))
(defun idlwave-sintern-method (name &optional set)
  (cond ((not (stringp name)) name)
	((gethash (downcase name) (cdr idlwave-sint-methods)))
	((gethash (downcase name) (car idlwave-sint-methods)))
	(set (idlwave-sintern-set name 'method idlwave-sint-methods set))
	(name)))
(defun idlwave-sintern-class (name &optional set)
  (cond ((not (stringp name)) name)
	((gethash (downcase name) (cdr idlwave-sint-classes)))
	((gethash (downcase name) (car idlwave-sint-classes)))
	(set (idlwave-sintern-set name 'class idlwave-sint-classes set))
	(name)))

(defun idlwave-sintern-dir (dir &optional set)
  (car (or (member dir idlwave-sint-dirs)
	   (setq idlwave-sint-dirs (cons dir idlwave-sint-dirs)))))
(defun idlwave-sintern-libname (name &optional set)
  (car (or (member name idlwave-sint-libnames)
	   (setq idlwave-sint-libnames (cons name idlwave-sint-libnames)))))

(defun idlwave-sintern-set (name type tables set)
  (let* ((func (or (cdr (assq type idlwave-completion-case))
		   'identity))
	 (iname (funcall (if (eq func 'preserve) 'identity func) name))
	 (table (if (eq set 'sys) (cdr tables) (car tables))))
    (puthash (downcase name) iname table)
    iname))

(defun idlwave-sintern-keyword-list (kwd-list &optional set)
  "Sintern a set of keywords (file (key . link) (key2 . link2) ...)"
  (mapc (lambda(x)
	  (setcar x (idlwave-sintern-keyword (car x) set)))
	(cdr kwd-list))
  kwd-list)

(defun idlwave-sintern-rinfo-list (list &optional set default-dir)
  "Sintern all strings in the rinfo LIST.
With optional parameter SET: also set new patterns.  Probably this
will always have to be t.  If DEFAULT-DIR is passed, it is used as
the base of the directory."
  (let (entry name type class kwds res source call new)
    (while list
      (setq entry (car list)
	    list (cdr list)
	    name (car entry)
	    type (nth 1 entry)
	    class (nth 2 entry)
	    source (nth 3 entry)
	    call (nth 4 entry)
	    kwds (nthcdr 5 entry))

      ;; The class and name
      (if class
	  (progn
	    (if (symbolp class) (setq class (symbol-name class)))
	    (setq class (idlwave-sintern-class class set))
	    (setq name (idlwave-sintern-method name set)))
	(setq name (idlwave-sintern-routine name set)))

      ;; The source
      (let ((source-type (car source))
	    (source-file  (nth 1 source))
	    (source-dir  (if default-dir
			     (file-name-as-directory default-dir)
			   (nth 2 source)))
	    (source-lib (nth 3 source)))
	(if (stringp source-dir)
	    (setq source-dir (idlwave-sintern-dir source-dir set)))
	(if (stringp source-lib)
	    (setq source-lib (idlwave-sintern-libname source-lib set)))
	(setq source (list source-type source-file source-dir source-lib)))

      ;; The keywords
      (setq kwds (mapcar (lambda (x)
			   (idlwave-sintern-keyword-list x set))
			 kwds))

      ;; Build a canonicalized list
      (setq new (nconc (list name type class source call) kwds)
	    res (cons new res)))
    (nreverse res)))

;; Creating new sintern tables

(defun idlwave-new-sintern-type (tag)
  "Define a variable and a function to sintern the new type TAG.
This defines the function `idlwave-sintern-TAG' and the variable
`idlwave-sint-TAGs'."
  (let* ((name (symbol-name tag))
	 (names (concat name "s"))
	 (var (intern (concat "idlwave-sint-" names)))
	 (func (intern (concat "idlwave-sintern-" name))))
    (set var nil) ; initial value of the association list
    (fset func    ; set the function
	  `(lambda (name &optional set)
	     (cond ((not (stringp name)) name)
		   ((cdr (assoc (downcase name) ,var)))
		   (set
		    (setq ,var (cons (cons (downcase name) name) ,var))
		    name)
		   (name))))))

(defun idlwave-reset-sintern-type (tag)
  "Reset the sintern variable associated with TAG."
  (set (intern (concat "idlwave-sint-" (symbol-name tag) "s")) nil))

;;---------------------------------------------------------------------------


;; The variables which hold the information
(defvar idlwave-system-routines nil
  "Holds the routine-info obtained by scanning buffers.")
(defvar idlwave-buffer-routines nil
  "Holds the routine-info obtained by scanning buffers.")
(defvar idlwave-compiled-routines nil
  "Holds the routine-info obtained by asking the shell.")
(defvar idlwave-unresolved-routines nil
  "Holds the unresolved routine-info obtained by asking the shell.")
(defvar idlwave-user-catalog-routines nil
  "Holds the procedure routine-info from the user scan.")
(defvar idlwave-library-catalog-routines nil
  "Holds the procedure routine-info from the .idlwave_catalog library files.")
(defvar idlwave-library-catalog-libname nil
  "Name of library catalog loaded from .idlwave_catalog files.")
(defvar idlwave-path-alist nil
  "Alist with !PATH directories and zero or more flags if the dir has
been scanned in a user catalog ('user) or discovered in a library
catalog \('lib).")
(defvar idlwave-true-path-alist nil
  "Like `idlwave-path-alist', but with true filenames.")
(defvar idlwave-routines nil
  "Holds the combined procedure/function/method routine-info.")
(defvar idlwave-class-alist nil
  "Holds the class names known to IDLWAVE.")
(defvar idlwave-class-history nil
  "The history of classes selected with the minibuffer.")
(defvar idlwave-force-class-query nil)
(defvar idlwave-before-completion-wconf nil
  "The window configuration just before the completion buffer was displayed.")
(defvar idlwave-last-system-routine-info-cons-cell nil
  "The last cons cell in the system routine info.")

;;
;; The code to get routine info from different sources.

(defvar idlwave-system-routines)
(defvar idlwave-catalog-process nil
  "The background process currently updating the catalog.")

(defun idlwave-routines ()
  "Provide a list of IDL routines.
This routine loads the builtin routines on the first call.
Later it only returns the value of the variable."
  (if (and idlwave-catalog-process
	   (processp idlwave-catalog-process))
      (progn
	(cond
	 ((equal (process-status idlwave-catalog-process) 'exit)
	  (message "updating........")
	  (setq idlwave-catalog-process nil)
	  (idlwave-update-routine-info '(4)))
	 ((equal (process-status idlwave-catalog-process) 'run)
	  ;; Keep it running...
	  )
	 (t
	  ;; Something is wrong, get rid of the process
	  (message "Problem with catalog process") (beep)
	  (condition-case nil
	      (kill-process idlwave-catalog-process)
	    (error nil))
	  (setq idlwave-catalog-process nil)))))
  (or idlwave-routines
      (progn
	(idlwave-update-routine-info)
	;; return the current value
	idlwave-routines)))

(defvar idlwave-update-rinfo-hook nil
  "List of functions which should run after a global rinfo update.
Does not run after automatic updates of buffer or the shell.")

(defun idlwave-rescan-catalog-directories ()
  "Rescan the previously selected directories.  For batch processing."
  (idlwave-update-routine-info '(16)))

(defun idlwave-rescan-asynchronously ()
  "Dispatch another Emacs instance to update the idlwave catalog.
After the process finishes normally, the first access to routine info
will re-read the catalog."
  (interactive)
  (if (processp idlwave-catalog-process)
      (if (eq (process-status idlwave-catalog-process) 'run)
	  (if (yes-or-no-p "A catalog-updating process is running.  Kill it? ")
	      (progn
		(condition-case nil
		    (kill-process idlwave-catalog-process)
		  (error nil))
		(error "Process killed, no new process started"))
	    (error "Quit"))
	(condition-case nil
	    (kill-process idlwave-catalog-process)
	  (error nil))))
  (if (or (not idlwave-user-catalog-file)
	  (not (stringp idlwave-user-catalog-file))
	  (not (file-regular-p idlwave-user-catalog-file)))
      (error "No catalog has been produced yet"))
  (let* ((emacs (concat invocation-directory invocation-name))
	 (args (list "-batch"
		     "-l" (expand-file-name "~/.emacs")
		     "-l" "idlwave"
		     "-f" "idlwave-rescan-catalog-directories"))
	 (process (apply 'start-process "idlcat"
			 nil emacs args)))
    (setq idlwave-catalog-process process)
    (set-process-sentinel
     process
     (lambda (pro why)
       (when (string-match "finished" why)
	 (setq idlwave-routines nil
	       idlwave-system-routines nil
	       idlwave-catalog-process nil)
	 (or (idlwave-start-load-rinfo-timer)
	     (idlwave-update-routine-info '(4))))))
    (message "Background job started to update catalog file")))


;; Format for all routine info user catalog, library catalogs, etc.:
;;
;; ("ROUTINE" type class
;;  (system) | (lib pro_file dir "LIBNAME") | (user pro_file dir "USERLIB") |
;;  (buffer pro_file dir) | (compiled pro_file dir)
;;   "calling_string" ("HELPFILE" (("KWD1" . link1) ...))
;;                    ("HELPFILE2" (("KWD2" . link) ...)) ...)
;;
;; DIR will be supplied dynamically while loading library catalogs,
;; and is sinterned to save space, as is LIBNAME.  PRO_FILE can be a
;; complete filepath, in which case DIR is unnecessary.  HELPFILE can
;; be nil, as can LINK1, etc., if no HTML help is available.


(defvar idlwave-load-rinfo-idle-timer)
(defvar idlwave-shell-path-query)

(defun idlwave-update-routine-info (&optional arg no-concatenate)
  "Update the internal routine-info lists.
These lists are used by `idlwave-routine-info' (\\[idlwave-routine-info])
and by `idlwave-complete' (\\[idlwave-complete]) to provide information
about individual routines.

The information can come from 4 sources:
1. IDL programs in the current editing session
2. Compiled modules in an IDL shell running as Emacs subprocess
3. A list which covers the IDL system routines.
4. A list which covers the prescanned library files.

Scans all IDLWAVE-mode buffers of the current editing session (see
`idlwave-scan-all-buffers-for-routine-info').
When an IDL shell is running, this command also queries the IDL program
for currently compiled routines.

With prefix ARG, also reload the system and library lists.
With two prefix ARG's, also rescans the chosen user catalog tree.
With three prefix args, dispatch asynchronous process to do the update.

If NO-CONCATENATE is non-nil, don't pre-concatenate the routine info
lists, but instead wait for the shell query to complete and
asynchronously finish updating routine info.  This is set
automatically when called interactively.  When you need routine
information updated immediately, leave NO-CONCATENATE nil."
  (interactive "P\np")
  ;; Stop any idle processing
  (if (or (and (fboundp 'itimerp)
	       (itimerp idlwave-load-rinfo-idle-timer))
	  (and (fboundp 'timerp)
	       (timerp idlwave-load-rinfo-idle-timer)))
      (cancel-timer idlwave-load-rinfo-idle-timer))
  (cond
   ((equal arg '(64))
    ;; Start a background process which updates the catalog.
    (idlwave-rescan-asynchronously))
   ((equal arg '(16))
    ;; Update the user catalog now, and wait for them.
    (idlwave-create-user-catalog-file t))
   (t
    (let* ((load (or arg
		     idlwave-buffer-case-takes-precedence
		     (null idlwave-routines)))
	   ;; The override-idle means, even if the idle timer has done some
	   ;; preparing work, load and renormalize everything anyway.
	   (override-idle (or arg idlwave-buffer-case-takes-precedence)))

      (setq idlwave-buffer-routines nil
	    idlwave-compiled-routines nil
	    idlwave-unresolved-routines nil)
      ;; Reset the appropriate hashes
      (if (get 'idlwave-reset-sintern 'done-by-idle)
	  ;; reset was already done in idle time, so skip this step now once
	  (put 'idlwave-reset-sintern 'done-by-idle nil)
	(idlwave-reset-sintern (cond (load t)
				     ((null idlwave-system-routines) t)
				     (t 'bufsh))))

      (if idlwave-buffer-case-takes-precedence
	  ;; We can safely scan the buffer stuff first
	  (progn
	    (idlwave-update-buffer-routine-info)
	    (and load (idlwave-load-all-rinfo override-idle)))
	;; We first do the system info, and then the buffers
	(and load (idlwave-load-all-rinfo override-idle))
	(idlwave-update-buffer-routine-info))

      ;; Let's see if there is a shell
      (let* ((shell-is-running (and (fboundp 'idlwave-shell-is-running)
				    (idlwave-shell-is-running)))
	     (ask-shell (and shell-is-running
			     idlwave-query-shell-for-routine-info)))

	;; Load the library catalogs again, first re-scanning the path
	(when arg
	  (if shell-is-running
	      (idlwave-shell-send-command idlwave-shell-path-query
					  '(progn
					     (idlwave-shell-get-path-info)
					     (idlwave-scan-library-catalogs))
					  'hide)
	    (idlwave-scan-library-catalogs)))

	(if (or (not ask-shell)
		(not no-concatenate))
	    ;; 1. If we are not going to ask the shell, we need to do the
	    ;;    concatenation now.
	    ;; 2. When this function is called non-interactively, it
	    ;;    means that someone needs routine info *now*.  The
	    ;;    shell update causes the concatenation to be
	    ;;    *delayed*, so not in time for the current command.
	    ;;    Therefore, we do a concatenation now, even though
	    ;;    the shell might do it again.
	    (idlwave-concatenate-rinfo-lists nil 'run-hooks))

	(when ask-shell
	  ;; Ask the shell about the routines it knows of.
	  (message "Querying the shell")
	  (idlwave-shell-update-routine-info nil t)))))))


(defvar idlwave-load-rinfo-steps-done (make-vector 6 nil))
(defvar idlwave-load-rinfo-idle-timer nil)
(defun idlwave-start-load-rinfo-timer ()
  (if (or (and (fboundp 'itimerp)
	       (itimerp idlwave-load-rinfo-idle-timer))
	  (and (fboundp 'timerp)
	       (timerp idlwave-load-rinfo-idle-timer)))
      (cancel-timer idlwave-load-rinfo-idle-timer))
  (setq idlwave-load-rinfo-steps-done (make-vector 6 nil))
  (setq idlwave-load-rinfo-idle-timer nil)
  (if (and idlwave-init-rinfo-when-idle-after
	   (numberp idlwave-init-rinfo-when-idle-after)
	   (not (equal 0 idlwave-init-rinfo-when-idle-after))
	   (not idlwave-routines))
      (condition-case nil
	  (progn
	    (setq idlwave-load-rinfo-idle-timer
		  (run-with-idle-timer
		   idlwave-init-rinfo-when-idle-after
		   nil 'idlwave-load-rinfo-next-step)))
	(error nil))))

(defvar idlwave-library-routines nil "Obsolete variable.")

;;------ XML Help routine info system
(defun idlwave-load-system-routine-info ()
  ;; Load the system routine info from the cached routine info file,
  ;; which, if necessary, will be re-created from the XML file on
  ;; disk.  As a last fallback, load the (likely outdated) idlw-rinfo
  ;; file distributed with older IDLWAVE versions (<6.0)
  (unless (and (load idlwave-xml-system-rinfo-converted-file
		     'noerror 'nomessage)
	       (idlwave-xml-system-routine-info-up-to-date))
    ;; See if we can create it from XML source
    (condition-case nil
	(idlwave-convert-xml-system-routine-info)
      (error
       (unless (load idlwave-xml-system-rinfo-converted-file
		     'noerror 'nomessage)
	 (if idlwave-system-routines
	     (message
	      "Failed to load converted routine info, using old conversion.")
	   (message
	    "Failed to convert XML routine info, falling back on idlw-rinfo.")
	   (if (not (load "idlw-rinfo" 'noerror 'nomessage))
	       (message
		"Could not locate any system routine information."))))))))

(defun idlwave-xml-system-routine-info-up-to-date()
  (let* ((dir (file-name-as-directory
	       (expand-file-name "help/online_help" (idlwave-sys-dir))))
	 (catalog-file (expand-file-name "idl_catalog.xml" dir)))
    (file-newer-than-file-p ;converted file is newer than catalog
     idlwave-xml-system-rinfo-converted-file
     catalog-file)))

(defvar idlwave-system-class-info nil) ; Gathered from idlw-rinfo
(defvar idlwave-system-variables-alist nil
  "Alist of system variables and the associated structure tags.
Gets set in cached XML rinfo, or `idlw-rinfo.el'.")
(defvar idlwave-executive-commands-alist nil
  "Alist of system variables and their help files.")
(defvar idlwave-help-special-topic-words nil)


(defun idlwave-shorten-syntax (syntax name &optional class)
  ;; From a list of syntax statements, shorten with %s and group with "or"
  (let ((case-fold-search t))
    (mapconcat
     (lambda (x)
       (while (string-match name x)
	 (setq x (replace-match "%s" t t x)))
       (if class
	   (while (string-match class x)
	     (setq x (replace-match "%s" t t x))))
       x)
     (nreverse syntax)
     " or ")))

(defun idlwave-xml-create-class-method-lists (xml-entry)
  ;; Create a class list entry from the xml parsed list., returning a
  ;; cons of form (class-entry method-entries).
  (let* ((nameblock (nth 1 xml-entry))
	 (class (cdr (assq 'name nameblock)))
	 (link (cdr (assq 'link nameblock)))
	 (params (cddr xml-entry))
	 (case-fold-search t)
	 class-entry
	 method methods-entry extra-kwds
	 props get-props set-props init-props inherits
	 pelem ptype)
    (while params
      (setq pelem (car params))
      (when (listp pelem)
	(setq ptype (car pelem)
	      props (car (cdr pelem)))
	(cond
	 ((eq ptype 'SUPERCLASS)
	  (let ((pname (cdr (assq 'name props)))
		(plink (cdr (assq 'link props))))
	    (unless (and (string= pname "None")
			 (string= plink "None"))
	      (push pname inherits))))

	 ((eq ptype 'PROPERTY)
	  (let ((pname (cdr (assq 'name props)))
		(plink (cdr (assq 'link props)))
		(get (string= (cdr (assq 'get props)) "Yes"))
		(set (string= (cdr (assq 'set props)) "Yes"))
		(init (string= (cdr (assq 'init props)) "Yes")))
	    (if get (push (list pname plink) get-props))
	    (if set (push (list pname plink) set-props))
	    (if init (push (list pname plink) init-props))))

	 ((eq ptype 'METHOD)
	  (setq method (cdr (assq 'name props)))
	  (setq extra-kwds ;;Assume all property keywords are gathered already
		(cond
		 ((string-match (concat class "::Init") method)
		  (put 'init-props 'matched t)
		  init-props)
		 ((string-match (concat class "::GetProperty") method)
		  (put 'get-props 'matched t)
		  get-props)
		 ((string-match (concat class "::SetProperty") method)
		  (put 'set-props 'matched t)
		  set-props)
		 (t nil)))
	  (setq methods-entry
		(nconc (idlwave-xml-create-rinfo-list pelem class extra-kwds)
		       methods-entry)))
	 (t)))
      (setq params (cdr params)))
    ;;(unless (get 'init-props 'matched)
    ;;  (message "Failed to match Init in class %s" class))
    ;;(unless (get 'get-props 'matched)
    ;;  (message "Failed to match GetProperty in class %s" class))
    ;;(unless (get 'set-props 'matched)
    ;;  (message "Failed to match SetProperty in class %s" class))
    (setq class-entry
	  (if inherits
	      (list class (append '(inherits) inherits) (list 'link link))
	    (list class (list 'link link))))
    (cons class-entry methods-entry)))

(defun idlwave-xml-create-rinfo-list (xml-entry &optional class extra-kws)
  ;; Create correctly structured list elements from ROUTINE or METHOD
  ;; XML list structures.  Return a list of list elements, with more
  ;; than one sub-list possible if a routine can serve as both
  ;; procedure and function (e.g. call_method).
  (let* ((nameblock (nth 1 xml-entry))
	 (name (cdr (assq 'name nameblock)))
	 (link (cdr (assq 'link nameblock)))
	 (params (cddr xml-entry))
	 (syntax-vec (make-vector 3 nil)) ; procedure, function, exec command
	 (case-fold-search t)
	 syntax kwd klink pref-list kwds pelem ptype props result type)
    (if class ;; strip out class name from class method name string
	(if (string-match (concat class "::") name)
	    (setq name (substring name (match-end 0)))))
    (while params
      (setq pelem (car params))
      (when (listp pelem)
	(setq ptype (car pelem)
	      props (car (cdr pelem)))
	(cond
	 ((eq ptype 'SYNTAX)
	  (setq syntax (cdr (assq 'name props)))
	  (if (string-match "-&gt;" syntax)
	      (setq syntax (replace-match "->" t nil syntax)))
	  (setq type (cdr (assq 'type props)))
	  (push syntax
		(aref syntax-vec (cond
				  ((string-match "^pro" type) 0)
				  ((string-match "^fun" type) 1)
				  ((string-match "^exec" type) 2)))))
	 ((eq ptype 'KEYWORD)
	  (setq kwd (cdr (assq 'name props))
		klink (cdr (assq 'link props)))
	  (if (string-match "^\\[XY\\(Z?\\)\\]" kwd)
	      (progn
		(setq pref-list
		      (if (match-string 1 kwd) '("X" "Y" "Z") '("X" "Y"))
		      kwd (substring kwd (match-end 0)))
		(loop for x in pref-list do
		      (push (list (concat x kwd) klink) kwds)))
	    (push (list kwd klink) kwds)))

	 (t))); Do nothing for the others
      (setq params (cdr params)))

    ;; Debug
    ;; (if (and (null (aref syntax-vec 0))
    ;;          (null (aref syntax-vec 1))
    ;;          (null (aref syntax-vec 2)))
    ;;   (with-current-buffer (get-buffer-create "IDL_XML_catalog_complaints")
    ;;     (if class
    ;;         (insert (format "Missing SYNTAX entry for %s::%s\n" class name))
    ;;       (insert (message "Missing SYNTAX entry for %s\n" name)))))

    ;; Executive commands are treated specially
    (if (aref syntax-vec 2)
	(cons (substring name 1) link)
      (if extra-kws (setq kwds (nconc kwds extra-kws)))
      (setq kwds (idlwave-rinfo-group-keywords kwds link))
      (loop for idx from 0 to 1 do
	    (if (aref syntax-vec idx)
		(push (append (list name (if (eq idx 0) 'pro 'fun)
				    class '(system)
				    (idlwave-shorten-syntax
				     (aref syntax-vec idx) name class))
			      kwds) result)))
      result)))


(defun idlwave-rinfo-group-keywords (kwds master-link)
  ;; Group keywords by link file, as a list with elements
  ;; (linkfile ( ("KWD1" . link1) ("KWD2" . link2))
  (let (kwd link anchor linkfiles block master-elt)
    (while kwds
      (setq kwd (car kwds)
	    link (idlwave-split-link-target (nth 1 kwd))
	    anchor (cdr link)
	    link (car link)
	    kwd (car kwd))
      (if (setq block (assoc link linkfiles))
	  (push (cons kwd anchor) (cdr block))
	(push (list link (cons kwd anchor)) linkfiles))
      (setq kwds (cdr kwds)))
    ;; Ensure the master link is there
    (if (setq master-elt (assoc master-link linkfiles))
	(if (eq (car linkfiles) master-elt)
	    linkfiles
 	  (cons master-elt (delq master-elt linkfiles)))
      (push (list master-link) linkfiles))))

(defun idlwave-convert-xml-clean-statement-aliases (aliases)
  ;; Clean up the syntax of routines which are actually aliases by
  ;; removing the "OR" from the statements
  (let (syntax entry)
    (loop for x in aliases do
	  (setq entry (assoc x idlwave-system-routines))
	  (when entry
	    (while (string-match " +or +" (setq syntax (nth 4 entry)))
	      (setf (nth 4 entry) (replace-match ", " t t syntax)))))))

(defun idlwave-convert-xml-clean-routine-aliases (aliases)
  ;; Duplicate and trim original routine aliases from rinfo list
  ;; This if for, e.g. OPENR/OPENW/OPENU
  (let (alias remove-list new parts all-parts)
    (loop for x in aliases do
	  (when (setq parts (split-string (cdr x) "/"))
	    (setq new (assoc (cdr x) all-parts))
	    (unless new
	      (setq new (cons (cdr x) parts))
	      (push new all-parts))
	    (setcdr new (delete (car x) (cdr new)))))

    ;; Add any missing aliases (separate by slashes)
    (loop for x in all-parts do
	  (if (cdr x)
	      (push (cons (nth 1 x) (car x)) aliases)))

    (loop for x in aliases do
	  (when (setq alias (assoc (cdr x) idlwave-system-routines))
	    (unless (memq alias remove-list) (push alias remove-list))
	    (setq alias (copy-sequence alias))
	    (setcar alias (car x))
	    (push alias idlwave-system-routines)))
    (loop for x in remove-list do
	  (delq x idlwave-system-routines))))

(defun idlwave-convert-xml-clean-sysvar-aliases (aliases)
  ;; Duplicate and trim original routine aliases from rinfo list
  ;; This if for, e.g. !X, !Y, !Z.
  (let (alias remove-list)
    (loop for x in aliases do
	  (when (setq alias (assoc (cdr x) idlwave-system-variables-alist))
	    (unless (memq alias remove-list) (push alias remove-list))
	    (setq alias (copy-sequence alias))
	    (setcar alias (car x))
	    (push alias idlwave-system-variables-alist)))
    (loop for x in remove-list do
	  (delq x idlwave-system-variables-alist))))


(defun idlwave-xml-create-sysvar-alist (xml-entry)
  ;; Create a sysvar list entry from the xml parsed list.
  (let* ((nameblock (nth 1 xml-entry))
	 (name (cdr (assq 'name nameblock)))
	 (sysvar (substring name (progn (string-match "^ *!" name)
					(match-end 0))))
	 (link (cdr (assq 'link nameblock)))
	 (params (cddr xml-entry))
	 (case-fold-search t)
	 pelem ptype props tags)
    (while params
      (setq pelem (car params))
      (when (listp pelem)
	(setq ptype (car pelem)
	      props (car (cdr pelem)))
	(cond
	 ((eq ptype 'FIELD)
	  (push (cons (cdr (assq 'name props))
		      (cdr
		       (idlwave-split-link-target (cdr (assq 'link props)))))
		tags))))
	(setq params (cdr params)))
    (delq nil
	  (list sysvar (if tags (cons 'tags tags)) (list 'link link)))))


(defvar idlwave-xml-routine-info-file nil)

(defun idlwave-save-routine-info ()
  (if idlwave-xml-routine-info-file
      (with-temp-file idlwave-xml-system-rinfo-converted-file
	(insert
	 (concat ";; *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
;; IDLWAVE Routine Information File (IDLWAVE version " idlwave-mode-version ")
;; Automatically generated from source file:
;;  " idlwave-xml-routine-info-file "
;; on " (current-time-string) "
;; Do not edit."))
	(insert (format "\n(setq idlwave-xml-routine-info-file \n    \"%s\")"
			idlwave-xml-routine-info-file))
	(insert "\n(setq idlwave-system-routines\n    '")
	(prin1 idlwave-system-routines (current-buffer))
	(insert ")")
	(insert "\n(setq idlwave-system-variables-alist\n    '")
	(prin1 idlwave-system-variables-alist (current-buffer))
	(insert ")")
	(insert "\n(setq idlwave-system-class-info\n    '")
	(prin1 idlwave-system-class-info (current-buffer))
	(insert ")")
	(insert "\n(setq idlwave-executive-commands-alist\n    '")
	(prin1 idlwave-executive-commands-alist (current-buffer))
	(insert ")")
	(insert "\n(setq idlwave-help-special-topic-words\n    '")
	(prin1 idlwave-help-special-topic-words (current-buffer))
	(insert ")"))))

(defun idlwave-convert-xml-system-routine-info ()
  "Convert XML supplied IDL routine info into internal form.
Cache to disk for quick recovery."
  (interactive)
  (let* ((dir (file-name-as-directory
	       (expand-file-name "help/online_help" (idlwave-sys-dir))))
	 (catalog-file (expand-file-name "idl_catalog.xml" dir))
	 (elem-cnt 0)
	 props rinfo msg-cnt elem type nelem class-result alias
	 routines routine-aliases statement-aliases sysvar-aliases)
    (if (not (file-exists-p catalog-file))
	(error "No such XML routine info file: %s" catalog-file)
      (if (not (file-readable-p catalog-file))
	  (error "Cannot read XML routine info file: %s" catalog-file)))
    (message "Reading XML routine info...")
    (setq rinfo (xml-parse-file catalog-file))
    (message "Reading XML routine info...done")
    (setq rinfo (assq 'CATALOG rinfo))
    (unless rinfo (error "Failed to parse XML routine info"))
    ;;(setq rinfo (car rinfo)) ; Skip the catalog stuff.

    (setq rinfo (cddr rinfo))

    (setq nelem (length rinfo)
	  msg-cnt (/ nelem 20))

    (setq idlwave-xml-routine-info-file nil)
    (message "Converting XML routine info...")
    (setq idlwave-system-routines nil
	  idlwave-system-variables-alist nil
	  idlwave-system-class-info nil
	  idlwave-executive-commands-alist nil
	  idlwave-help-special-topic-words nil)

    (while rinfo
      (setq elem (car rinfo)
	    rinfo (cdr rinfo))
      (incf elem-cnt)
      (when (listp elem)
	(setq type (car elem)
	      props (car (cdr elem)))
	(if (= (mod elem-cnt msg-cnt) 0)
	    (message "Converting XML routine info...%2d%%"
		     (/ (* elem-cnt 100) nelem)))
	(cond
	 ((eq type 'ROUTINE)
	  (if (setq alias (assq 'alias_to props))
	      (push (cons (cdr (assq 'name props)) (cdr alias))
		    routine-aliases)
	    (setq routines (idlwave-xml-create-rinfo-list elem))
	    (if (listp (cdr routines))
		(setq idlwave-system-routines
		      (nconc idlwave-system-routines routines))
	      ;; a cons cell is an executive commands
	      (push routines idlwave-executive-commands-alist))))

	 ((eq type 'CLASS)
	  (setq class-result (idlwave-xml-create-class-method-lists elem))
	  (push (car class-result) idlwave-system-class-info)
	  (setq idlwave-system-routines
	  (nconc idlwave-system-routines (cdr class-result))))

	 ((eq type 'STATEMENT)
	  (push (cons (cdr (assq 'name props))
		      (cdr (assq 'link props)))
	  idlwave-help-special-topic-words)
	  ;; Save the links to those which are statement aliases (not routines)
	  (if (setq alias (assq 'alias_to props))
	      (unless (member (cdr alias) statement-aliases)
		(push (cdr alias) statement-aliases))))

	 ((eq type 'SYSVAR)
	  (if (setq alias (cdr (assq 'alias_to props)))
	      (push (cons (substring (cdr (assq 'name props)) 1)
			  (substring alias 1))
		    sysvar-aliases)
	    (push (idlwave-xml-create-sysvar-alist elem)
		  idlwave-system-variables-alist)))
	 (t))))
    (idlwave-convert-xml-clean-routine-aliases routine-aliases)
    (idlwave-convert-xml-clean-statement-aliases statement-aliases)
    (idlwave-convert-xml-clean-sysvar-aliases sysvar-aliases)

    (setq idlwave-xml-routine-info-file catalog-file)
    (idlwave-save-routine-info)
    (message "Converting XML routine info...done")))


;; ("ROUTINE" type class
;;  (system) | (lib pro_file dir "LIBNAME") | (user pro_file dir "USERLIB") |
;;  (buffer pro_file dir) | (compiled pro_file dir)
;;   "calling_string" ("HELPFILE" (("KWD1" . link1) ...))
;;                    ("HELPFILE2" (("KWD2" . link) ...)) ...)


(defun idlwave-load-rinfo-next-step ()
  (let ((inhibit-quit t)
	(arr idlwave-load-rinfo-steps-done))
    (if (catch 'exit
	  (when (not (aref arr 0))
	    (message "Loading system routine info in idle time...")
	    (idlwave-load-system-routine-info)
	    ;;(load "idlw-rinfo" 'noerror 'nomessage)
	    (message "Loading system routine info in idle time...done")
	    (aset arr 0 t)
	    (throw 'exit t))

	  (when (not (aref arr 1))
	    (message "Normalizing idlwave-system-routines in idle time...")
	    (idlwave-reset-sintern t)
	    (put 'idlwave-reset-sintern 'done-by-idle t)
	    (setq idlwave-system-routines
		  (idlwave-sintern-rinfo-list idlwave-system-routines 'sys))
	    (message "Normalizing idlwave-system-routines in idle time...done")
	    (aset arr 1 t)
	    (throw 'exit t))

	  (when (not (aref arr 2))
	    (when (and (stringp idlwave-user-catalog-file)
		       (file-regular-p idlwave-user-catalog-file))
	      (message "Loading user catalog in idle time...")
	      (condition-case nil
		  (load-file idlwave-user-catalog-file)
		(error (throw 'exit nil)))
	      ;; Check for the old style catalog and warn
	      (if (and
		   (boundp 'idlwave-library-routines)
		   idlwave-library-routines)
		  (progn
		    (setq idlwave-library-routines nil)
		    (ding)
		    (message "Outdated user catalog: %s... recreate"
			     idlwave-user-catalog-file))
		(message "Loading user catalog in idle time...done")))
	    (aset arr 2 t)
	    (throw 'exit t))

	  (when (not (aref arr 3))
	    (when idlwave-user-catalog-routines
	      (message "Normalizing user catalog routines in idle time...")
	      (setq idlwave-user-catalog-routines
		    (idlwave-sintern-rinfo-list
		     idlwave-user-catalog-routines 'sys))
	      (message
	       "Normalizing user catalog routines in idle time...done"))
	    (aset arr 3 t)
	    (throw 'exit t))

	  (when (not (aref arr 4))
	    (idlwave-scan-library-catalogs
	     "Loading and normalizing library catalogs in idle time...")
	    (aset arr 4 t)
	    (throw 'exit t))
	  (when (not (aref arr 5))
	    (message "Finishing initialization in idle time...")
	    (idlwave-routines)
	    (message "Finishing initialization in idle time...done")
	    (aset arr 5 t)
	    (throw 'exit nil)))
	;; restart the timer
	(if (sit-for 1)
	    (idlwave-load-rinfo-next-step)
	  (setq idlwave-load-rinfo-idle-timer
		(run-with-idle-timer
		 idlwave-init-rinfo-when-idle-after
		 nil 'idlwave-load-rinfo-next-step))))))

(defvar idlwave-after-load-rinfo-hook nil)

(defun idlwave-load-all-rinfo (&optional force)
  ;; Load and case-treat the system, user catalog, and library routine
  ;; info files.

  ;; System
  (when (or force (not (aref idlwave-load-rinfo-steps-done 0)))
    ;;(load "idlw-rinfo" 'noerror 'nomessage))
    (idlwave-load-system-routine-info))
  (when (or force (not (aref idlwave-load-rinfo-steps-done 1)))
    (message "Normalizing idlwave-system-routines...")
    (setq idlwave-system-routines
	  (idlwave-sintern-rinfo-list idlwave-system-routines 'sys))
    (message "Normalizing idlwave-system-routines...done"))
  (when idlwave-system-routines
    (setq idlwave-routines (copy-sequence idlwave-system-routines))
    (setq idlwave-last-system-routine-info-cons-cell
	  (nthcdr (1- (length idlwave-routines)) idlwave-routines)))

  ;; User catalog
  (when (and (stringp idlwave-user-catalog-file)
	     (file-regular-p idlwave-user-catalog-file))
    (condition-case nil
	(when (or force (not (aref idlwave-load-rinfo-steps-done 2)))
	  (load-file idlwave-user-catalog-file))
      (error nil))
    (when (and
	   (boundp 'idlwave-library-routines)
	   idlwave-library-routines)
      (setq idlwave-library-routines nil)
      (error "Outdated user catalog: %s... recreate"
	     idlwave-user-catalog-file))
    (setq idlwave-true-path-alist nil)
    (when (or force (not (aref idlwave-load-rinfo-steps-done 3)))
      (message "Normalizing user catalog routines...")
      (setq idlwave-user-catalog-routines
	    (idlwave-sintern-rinfo-list
	     idlwave-user-catalog-routines 'sys))
      (message "Normalizing user catalog routines...done")))

  ;; Library catalog
  (when (or force (not (aref idlwave-load-rinfo-steps-done 4)))
    (idlwave-scan-library-catalogs
     "Loading and normalizing library catalogs..."))
  (run-hooks 'idlwave-after-load-rinfo-hook))


(defun idlwave-update-buffer-routine-info ()
  (let (res)
    (cond
     ((eq idlwave-scan-all-buffers-for-routine-info t)
      ;; Scan all buffers, current buffer last
      (message "Scanning all buffers...")
      (setq res (idlwave-get-routine-info-from-buffers
		 (reverse (buffer-list)))))
     ((null idlwave-scan-all-buffers-for-routine-info)
      ;; Don't scan any buffers
      (setq res nil))
     (t
      ;; Just scan this buffer
      (if (derived-mode-p 'idlwave-mode)
	  (progn
	    (message "Scanning current buffer...")
	    (setq res (idlwave-get-routine-info-from-buffers
		       (list (current-buffer))))))))
    ;; Put the result into the correct variable
    (setq idlwave-buffer-routines
	  (idlwave-sintern-rinfo-list res 'set))))

(defun idlwave-concatenate-rinfo-lists (&optional quiet run-hook)
  "Put the different sources for routine information together."
  ;; The sequence here is important because earlier definitions shadow
  ;; later ones.  We assume that if things in the buffers are newer
  ;; then in the shell of the system, they are meant to be different.
  (setcdr idlwave-last-system-routine-info-cons-cell
	  (append idlwave-buffer-routines
		  idlwave-compiled-routines
		  idlwave-library-catalog-routines
		  idlwave-user-catalog-routines))
  (setq idlwave-class-alist nil)

  ;; Give a message with information about the number of routines we have.
  (unless quiet
    (message
     "Routines Found: buffer(%d) compiled(%d) library(%d) user(%d) system(%d)"
     (length idlwave-buffer-routines)
     (length idlwave-compiled-routines)
     (length idlwave-library-catalog-routines)
     (length idlwave-user-catalog-routines)
     (length idlwave-system-routines)))
  (if run-hook
      (run-hooks 'idlwave-update-rinfo-hook)))

(defun idlwave-class-alist ()
  "Return the class alist - make it if necessary."
  (or idlwave-class-alist
      (let (class)
	(loop for x in idlwave-routines do
	  (when (and (setq class (nth 2 x))
		     (not (assq class idlwave-class-alist)))
	    (push (list class) idlwave-class-alist)))
	idlwave-class-alist)))

;; Three functions for the hooks
(defun idlwave-save-buffer-update ()
  (idlwave-update-current-buffer-info 'save-buffer))
(defun idlwave-kill-buffer-update ()
  (idlwave-update-current-buffer-info 'kill-buffer))
(defun idlwave-new-buffer-update ()
  (idlwave-update-current-buffer-info 'find-file))

(defun idlwave-update-current-buffer-info (why)
  "Update `idlwave-routines' for current buffer.
Can run from `after-save-hook'."
  (when (and (derived-mode-p 'idlwave-mode)
	     (or (eq t idlwave-auto-routine-info-updates)
		 (memq why idlwave-auto-routine-info-updates))
	     idlwave-scan-all-buffers-for-routine-info
	     idlwave-routines)
    (condition-case nil
	(let (routines)
	  (idlwave-replace-buffer-routine-info
	   (buffer-file-name)
	   (if (eq why 'kill-buffer)
	       nil
	     (setq routines
		   (idlwave-sintern-rinfo-list
		    (idlwave-get-routine-info-from-buffers
		     (list (current-buffer))) 'set))))
	  (idlwave-concatenate-rinfo-lists 'quiet)
	  routines)
      (error nil))))

(defun idlwave-replace-buffer-routine-info (file new)
  "Cut the part from FILE out of `idlwave-buffer-routines' and add NEW."
  (let ((list idlwave-buffer-routines)
	found)
    (while list
      ;; The following test uses eq to make sure it works correctly
      ;; when two buffers visit the same file.  Then the file names
      ;; will be equal, but not eq.
      (if (eq (idlwave-routine-source-file (nth 3 (car list))) file)
	  (progn
	    (setcar list nil)
	    (setq found t))
	(if found
	    ;; End of that section reached. Jump.
	    (setq list nil)))
      (setq list (cdr list)))
    (setq idlwave-buffer-routines
	  (append new (delq nil idlwave-buffer-routines)))))

;;----- Scanning buffers -------------------

(defun idlwave-get-routine-info-from-buffers (buffers)
  "Call `idlwave-get-buffer-routine-info' on idlwave-mode buffers in BUFFERS."
  (let (buf routine-lists res)
    (save-excursion
      (while (setq buf (pop buffers))
	(set-buffer buf)
	(if (and (derived-mode-p 'idlwave-mode)
		 buffer-file-name)
	    ;; yes, this buffer has the right mode.
	    (progn (setq res (condition-case nil
				 (idlwave-get-buffer-routine-info)
			       (error nil)))
		   (push res routine-lists)))))
    ;; Concatenate the individual lists and return the result
    (apply 'nconc routine-lists)))

(defun idlwave-get-buffer-routine-info ()
  "Scan the current buffer for routine info.  Return (PRO-LIST FUNC-LIST)."
  (let* ((case-fold-search t)
	 routine-list string entry)
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(while (re-search-forward
		"^[ \t]*\\(pro\\|function\\)[ \t]" nil t)
	  (setq string (buffer-substring-no-properties
			(match-beginning 0)
			(progn
			  (idlwave-end-of-statement)
			  (point))))
	  (setq entry (idlwave-parse-definition string))
	  (push entry routine-list))))
    routine-list))

(defvar idlwave-scanning-lib-dir)
(defvar idlwave-scanning-lib)
(defun idlwave-parse-definition (string)
  "Parse a module definition."
  (let ((case-fold-search t)
	start name args type keywords class)
    ;; Remove comments
    (while (string-match ";.*" string)
      (setq string (replace-match "" t t string)))
    ;; Remove the continuation line stuff
    (while (string-match "\\([^a-zA-Z0-9$_]\\)\\$[ \t]*\n" string)
      (setq string (replace-match "\\1 " t nil string)))
    (while (string-match "\n" string)
      (setq string (replace-match " " t nil string)))
    ;; Match the name and type.
    (when (string-match
	   "\\<\\(pro\\|function\\)\\>\\s-+\\(\\([a-zA-Z0-9$_]+\\)::\\)?\\([a-zA-Z0-9$_]+\\)" string)
      (setq start (match-end 0))
      (setq type (downcase (match-string 1 string)))
      (if (match-beginning 3)
	  (setq class (match-string 3 string)))
      (setq name (match-string 4 string)))
    ;; Match normal args and keyword args
    (while (string-match
	    ",\\s-*\\([a-zA-Z][a-zA-Z0-9$_]*\\|\\(_ref\\)?_extra\\)\\s-*\\(=\\)?"
	    string start)
      (setq start (match-end 0))
      (if (match-beginning 3)
	  (push (match-string 1 string) keywords)
	(push (match-string 1 string) args)))
    ;; Normalize and sort.
    (setq args (nreverse args))
    (setq keywords (sort keywords (lambda (a b)
				    (string< (downcase a) (downcase b)))))
    ;; Make and return the entry
    ;; We don't know which argument are optional, so this information
    ;; will not be contained in the calling sequence.
    (list name
	  (if (equal type "pro") 'pro 'fun)
	  class
	  (cond ((not (boundp 'idlwave-scanning-lib))
		 (list  'buffer (buffer-file-name)))
;		((string= (downcase
;			   (file-name-sans-extension
;			    (file-name-nondirectory (buffer-file-name))))
;			  (downcase name))
;		 (list 'lib))
;		(t (cons 'lib (file-name-nondirectory (buffer-file-name))))
		(t (list 'user (file-name-nondirectory (buffer-file-name))
			 idlwave-scanning-lib-dir "UserLib")))
	  (concat
	   (if (string= type "function") "Result = " "")
	   (if class "Obj ->[%s::]" "")
	   "%s"
	   (if args
	       (concat
		(if (string= type "function") "(" ", ")
		(mapconcat 'identity args ", ")
		(if (string= type "function") ")" ""))))
	  (if keywords
	      (cons nil (mapcar 'list keywords)) ;No help file
	    nil))))


;;----- Scanning the user catalog -------------------

(defun idlwave-sys-dir ()
  "Return the syslib directory, or a dummy that never matches."
  (cond
   ((and idlwave-system-directory
	 (not (string= idlwave-system-directory "")))
    idlwave-system-directory)
   ((getenv "IDL_DIR"))
   (t "@@@@@@@@")))


(defun idlwave-create-user-catalog-file (&optional arg)
  "Scan all files on selected dirs of IDL search path for routine information.

A widget checklist will allow you to choose the directories.  Write
the result as a file `idlwave-user-catalog-file'.  When this file
exists, it will be automatically loaded to give routine information
about library routines.  With ARG, just rescan the same directories
as last time - so no widget will pop up."
  (interactive "P")
  ;; Make sure the file is loaded if it exists.
  (if (and (stringp idlwave-user-catalog-file)
	   (file-regular-p idlwave-user-catalog-file))
      (condition-case nil
	  (load-file idlwave-user-catalog-file)
	(error nil)))
  ;; Make sure the file name makes sense
  (unless (and (stringp idlwave-user-catalog-file)
	       (> (length idlwave-user-catalog-file) 0)
	       (file-accessible-directory-p
		(file-name-directory idlwave-user-catalog-file))
	       (not (string= "" (file-name-nondirectory
				 idlwave-user-catalog-file))))
    (error "`idlwave-user-catalog-file' does not point to a file in an accessible directory"))

  (cond
    ;; Rescan the known directories
   ((and arg idlwave-path-alist
	 (consp (car idlwave-path-alist)))
    (idlwave-scan-user-lib-files idlwave-path-alist))

   ;; Expand the directories from library-path and run the widget
   (idlwave-library-path
    (idlwave-display-user-catalog-widget
     (if idlwave-true-path-alist
	 ;; Propagate any flags on the existing path-alist
	 (mapcar (lambda (x)
		   (let ((path-entry (assoc (file-truename x)
					    idlwave-true-path-alist)))
		     (if path-entry
			 (cons x (cdr path-entry))
		       (list x))))
		 (idlwave-expand-path idlwave-library-path))
       (mapcar 'list (idlwave-expand-path idlwave-library-path)))))

   ;; Ask the shell for the path and then run the widget
   (t
    (message "Asking the shell for IDL path...")
    (require 'idlw-shell)
    (idlwave-shell-send-command idlwave-shell-path-query
				'(idlwave-user-catalog-command-hook nil)
				'hide))))


;; Parse shell path information and select among it.
(defun idlwave-user-catalog-command-hook (&optional arg)
  ;; Command hook used by `idlwave-create-user-catalog-file'.
  (if arg
      ;; Scan immediately
      (idlwave-scan-user-lib-files idlwave-path-alist)
    ;; Set the path and display the widget
    (idlwave-shell-get-path-info 'no-write) ; set to something path-alist
    (idlwave-scan-library-catalogs "Locating library catalogs..." 'no-load)
    (idlwave-display-user-catalog-widget idlwave-path-alist)))

(defconst idlwave-user-catalog-widget-help-string
  "This is the front-end to the creation of the IDLWAVE user catalog.
Please select the directories on IDL's search path from which you
would like to extract routine information, to be stored in the file:

             %s

If this is not the correct file, first set variable
`idlwave-user-catalog-file', and call this command again.

N.B. Many libraries include pre-scanned catalog files
\(\".idlwave_catalog\").  These are marked with \"[LIB]\", and need
not be scanned.  You can scan your own libraries off-line using the
perl script `idlwave_catalog'.

After selecting the directories, choose [Scan & Save] to scan the library
directories and save the routine info.
\n")

(defvar idlwave-widget)
(defvar widget-keymap)
(defun idlwave-display-user-catalog-widget (dirs-list)
  "Create the widget to select IDL search path directories for scanning."
  (interactive)
  (require 'widget)
  (require 'wid-edit)
  (unless dirs-list
      (error "Don't know IDL's search path"))

  (kill-buffer (get-buffer-create "*IDLWAVE Widget*"))
  (switch-to-buffer (get-buffer-create "*IDLWAVE Widget*"))
  (kill-all-local-variables)
  (make-local-variable 'idlwave-widget)
  (widget-insert (format idlwave-user-catalog-widget-help-string
			 idlwave-user-catalog-file))

  (widget-create 'push-button
		 :notify 'idlwave-widget-scan-user-lib-files
		 "Scan & Save")
  (widget-insert "  ")
  (widget-create 'push-button
		 :notify 'idlwave-delete-user-catalog-file
		 "Delete File")
  (widget-insert "  ")
  (widget-create 'push-button
		 :notify
		 (lambda (&rest ignore)
                   (let ((path-list (widget-get idlwave-widget :path-dirs)))
                     (dolist (x path-list)
                       (unless (memq 'lib (cdr x))
                         (idlwave-path-alist-add-flag x 'user)))
                     (idlwave-display-user-catalog-widget path-list)))
		 "Select All Non-Lib")
  (widget-insert "  ")
  (widget-create 'push-button
		 :notify
		 (lambda (&rest ignore)
                   (let ((path-list (widget-get idlwave-widget :path-dirs)))
                     (dolist (x path-list)
                       (idlwave-path-alist-remove-flag x 'user))
                     (idlwave-display-user-catalog-widget path-list)))
		 "Deselect All")
  (widget-insert "  ")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (kill-buffer (current-buffer)))
		 "Quit")
  (widget-insert "\n\n")

  (widget-insert "Select Directories: \n")

  (setq idlwave-widget
	(apply 'widget-create
	       'checklist
	       :value  (delq nil (mapcar (lambda (x)
					   (if (memq 'user (cdr x))
					       (car x)))
					 dirs-list))
	       :greedy t
	       :tag "List of directories"
	       (mapcar (lambda (x)
			 (list 'item
			       (if (memq 'lib (cdr x))
				   (concat "[LIB] " (car x) )
				 (car x)))) dirs-list)))
  (widget-put idlwave-widget :path-dirs dirs-list)
  (widget-insert "\n")
  (use-local-map widget-keymap)
  (widget-setup)
  (goto-char (point-min))
  (delete-other-windows))

(defun idlwave-delete-user-catalog-file (&rest ignore)
  (if (yes-or-no-p
       (format "Delete file %s " idlwave-user-catalog-file))
      (progn
	(delete-file idlwave-user-catalog-file)
	(message "%s has been deleted" idlwave-user-catalog-file))))

(defun idlwave-widget-scan-user-lib-files (&rest ignore)
  ;; Call `idlwave-scan-user-lib-files' with data taken from the widget.
  (let* ((widget idlwave-widget)
	 (selected-dirs (widget-value widget))
	 (path-alist (widget-get widget :path-dirs))
	 (this-path-alist path-alist)
	 dir-entry)
    (while (setq dir-entry (pop this-path-alist))
      (if (member
	   (if (memq 'lib (cdr dir-entry))
	       (concat "[LIB] " (car dir-entry))
	     (car dir-entry))
	   selected-dirs)
	  (idlwave-path-alist-add-flag dir-entry 'user)
	(idlwave-path-alist-remove-flag dir-entry 'user)))
    (idlwave-scan-user-lib-files path-alist)))

(defvar font-lock-mode)
(defun idlwave-scan-user-lib-files (path-alist)
  ;; Scan the PRO files in PATH-ALIST and store the info in the user catalog
  (let* ((idlwave-scanning-lib t)
	 (idlwave-scanning-lib-dir "")
	 (idlwave-completion-case nil)
	 dirs-alist dir files file)
    (setq idlwave-user-catalog-routines nil
	  idlwave-path-alist path-alist ; for library-path instead
	  idlwave-true-path-alist nil)
    (if idlwave-auto-write-paths (idlwave-write-paths))
    (with-current-buffer (get-buffer-create "*idlwave-scan.pro*")
      (idlwave-mode)
      (setq dirs-alist (reverse path-alist))
      (while (setq dir (pop dirs-alist))
	(when (memq 'user (cdr dir))	; Has it marked for scan?
	  (setq dir (car dir))
	  (setq idlwave-scanning-lib-dir dir)
	  (when (file-directory-p dir)
	    (setq files (directory-files dir 'full "\\.[pP][rR][oO]\\'"))
	    (while (setq file (pop files))
	      (when (file-regular-p file)
		(if (not (file-readable-p file))
		    (message "Skipping %s (no read permission)" file)
		  (message "Scanning %s..." file)
		  (erase-buffer)
		  (insert-file-contents file 'visit)
		  (setq idlwave-user-catalog-routines
			(append (idlwave-get-routine-info-from-buffers
				 (list (current-buffer)))
				idlwave-user-catalog-routines)))))))))
    (message "Creating user catalog file...")
    (kill-buffer "*idlwave-scan.pro*")
    (kill-buffer (get-buffer-create "*IDLWAVE Widget*"))
    (let ((font-lock-maximum-size 0)
	  (auto-mode-alist nil))
      (find-file idlwave-user-catalog-file))
    (if (and (boundp 'font-lock-mode)
	     font-lock-mode)
	(font-lock-mode 0))
    (erase-buffer)
    (insert ";; IDLWAVE user catalog file\n")
    (insert (format ";; Created %s\n\n" (current-time-string)))

    ;; Define the routine info list
    (insert "\n(setq idlwave-user-catalog-routines\n    '(")
    (let ((standard-output (current-buffer)))
      (mapc (lambda (x)
	      (insert "\n    ")
	      (prin1 x)
	      (goto-char (point-max)))
	    idlwave-user-catalog-routines))
    (insert (format "))\n\n;;; %s ends here\n"
		    (file-name-nondirectory idlwave-user-catalog-file)))
    (goto-char (point-min))
    ;; Save the buffer
    (save-buffer 0)
    (kill-buffer (current-buffer)))
  (message "Creating user catalog file...done")
  (message "Info for %d routines saved in %s"
	   (length idlwave-user-catalog-routines)
	   idlwave-user-catalog-file)
  (sit-for 2)
  (idlwave-update-routine-info t))

(defun idlwave-read-paths ()
  (if (and (stringp idlwave-path-file)
	   (file-regular-p idlwave-path-file))
      (condition-case nil
	  (load idlwave-path-file t t t)
	(error nil))))

(defun idlwave-write-paths ()
  (interactive)
  (when (and idlwave-path-alist idlwave-system-directory)
    (let ((font-lock-maximum-size 0)
	  (auto-mode-alist nil))
      (find-file idlwave-path-file))
    (if (and (boundp 'font-lock-mode)
	     font-lock-mode)
	(font-lock-mode 0))
    (erase-buffer)
    (insert ";; IDLWAVE paths\n")
    (insert (format ";; Created %s\n\n" (current-time-string)))
    ;; Define the variable which knows the value of "!DIR"
    (insert (format "\n(setq idlwave-system-directory \"%s\")\n"
		    idlwave-system-directory))

    ;; Define the variable which contains a list of all scanned directories
    (insert "\n(setq idlwave-path-alist\n    '(")
    (let ((standard-output (current-buffer)))
      (mapc (lambda (x)
	      (insert "\n      ")
	      (prin1 x)
	      (goto-char (point-max)))
	    idlwave-path-alist))
    (insert "))\n")
    (save-buffer 0)
    (kill-buffer (current-buffer))))


(defun idlwave-expand-path (path &optional default-dir)
  ;; Expand parts of path starting with '+' recursively into directory list.
  ;; Relative recursive path elements are expanded relative to DEFAULT-DIR.
  (message "Expanding path...")
  (let (path1 dir recursive)
    (while (setq dir (pop path))
      (if (setq recursive (string= (substring dir 0 1) "+"))
	  (setq dir (substring dir 1)))
      (if (and recursive
	       (not (file-name-absolute-p dir)))
	  (setq dir (expand-file-name dir default-dir)))
      (if recursive
	  ;; Expand recursively
	  (setq path1 (append (idlwave-recursive-directory-list dir) path1))
	;; Keep unchanged
	(push dir path1)))
    (message "Expanding path...done")
    (nreverse path1)))

(defun idlwave-recursive-directory-list (dir)
  ;; Return a list of all directories below DIR, including DIR itself
  (let ((path (list dir)) path1 file files)
    (while (setq dir (pop path))
      (when (file-directory-p dir)
	(setq files (nreverse (directory-files dir t "[^.]")))
	(while (setq file (pop files))
	  (if (file-directory-p file)
	      (push (file-name-as-directory file) path)))
	(push dir path1)))
    path1))


;;----- Scanning the library catalogs ------------------




(defun idlwave-scan-library-catalogs (&optional message-base no-load)
  "Scan for library catalog files (.idlwave_catalog) and ingest.

All directories on `idlwave-path-alist' (or `idlwave-library-path'
instead, if present) are searched.  Print MESSAGE-BASE along with the
libraries being loaded, if passed, and skip loading/normalizing if
NO-LOAD is non-nil.  The variable `idlwave-use-library-catalogs' can
be set to nil to disable library catalog scanning."
  (when idlwave-use-library-catalogs
    (let ((dirs
	   (if idlwave-library-path
	       (idlwave-expand-path idlwave-library-path)
	     (mapcar 'car idlwave-path-alist)))
	  (old-libname "")
	  dir-entry dir catalog all-routines)
      (if message-base (message message-base))
      (while (setq dir (pop dirs))
	(catch 'continue
	  (when (file-readable-p
		 (setq catalog (expand-file-name ".idlwave_catalog" dir)))
	    (unless no-load
	      (setq idlwave-library-catalog-routines nil)
	      ;; Load the catalog file
	      (condition-case nil
		  (load catalog t t t)
		(error (throw 'continue t)))
	      (when (and
		     message-base
		     (not (string= idlwave-library-catalog-libname
				   old-libname)))
		(message "%s" (concat message-base
				      idlwave-library-catalog-libname))
		(setq old-libname idlwave-library-catalog-libname))
	      (when idlwave-library-catalog-routines
		(setq all-routines
		      (append
		       (idlwave-sintern-rinfo-list
			idlwave-library-catalog-routines 'sys dir)
		       all-routines))))

	    ;;  Add a 'lib flag if on path-alist
	    (when (and idlwave-path-alist
		       (setq dir-entry (assoc dir idlwave-path-alist)))
	      (idlwave-path-alist-add-flag dir-entry 'lib)))))
      (unless no-load (setq idlwave-library-catalog-routines all-routines))
      (if message-base (message (concat message-base "done"))))))

;;----- Communicating with the Shell -------------------

;; First, here is the idl program which can be used to query IDL for
;; defined routines.
(defconst idlwave-routine-info.pro
  "
;; START OF IDLWAVE SUPPORT ROUTINES
pro idlwave_print_safe,item,limit
  catch,err
  if err ne 0 then begin
     print,'Could not print item.'
     return
  endif
  if n_elements(item) gt limit then $
     print,item[0:limit-1],'<... truncated at ',strtrim(limit,2),' elements>' $
  else print,item
end

pro idlwave_print_info_entry,name,func=func,separator=sep
  ;; See if it's an object method
  if name eq '' then return
  func    = keyword_set(func)
  methsep = strpos(name,'::')
  meth    = methsep ne -1

  ;; Get routine info
  pars   = routine_info(name,/parameters,functions=func)
  source = routine_info(name,/source,functions=func)
  nargs  = pars.num_args
  nkw    = pars.num_kw_args
  if nargs gt 0 then args = pars.args
  if nkw   gt 0 then kwargs = pars.kw_args

  ;; Trim the class, and make the name
  if meth then begin
      class = strmid(name,0,methsep)
      name  = strmid(name,methsep+2,strlen(name)-1)
      if nargs gt 0 then begin
          ;; remove the self argument
          wh = where(args ne 'SELF',nargs)
          if nargs gt 0 then args = args[wh]
      endif
  endif else begin
      ;; No class, just a normal routine.
      class = \"\"
  endelse

  ;; Calling sequence
  cs = \"\"
  if func then cs = 'Result = '
  if meth then cs = cs + 'Obj -> [' + '%s' + '::]'
  cs = cs + '%s'
  if func then cs = cs + '(' else if nargs gt 0 then cs = cs + ', '
  if nargs gt 0 then begin
      for j=0,nargs-1 do begin
          cs = cs + args[j]
          if j lt nargs-1 then cs = cs + ', '
      endfor
  end
  if func then cs = cs + ')'
  ;; Keyword arguments
  kwstring = ''
  if nkw gt 0 then begin
      for j=0,nkw-1 do begin
          kwstring = kwstring + ' ' + kwargs[j]
      endfor
  endif

  ret=(['IDLWAVE-PRO','IDLWAVE-FUN'])[func]

  print,ret + ': ' + name + sep + class + sep + source[0].path  $
    + sep + cs + sep + kwstring
end

pro idlwave_routine_info,file
  on_error,1
  sep = '<@>'
  print,'>>>BEGIN OF IDLWAVE ROUTINE INFO (\"' + sep + '\" IS THE SEPARATOR)'
  all = routine_info()
  fileQ=n_elements(file) ne 0
  if fileQ then file=strtrim(file,2)
  for i=0L,n_elements(all)-1L do begin
     if fileQ then begin
        if (routine_info(all[i],/SOURCE)).path eq file then $
           idlwave_print_info_entry,all[i],separator=sep
     endif else idlwave_print_info_entry,all[i],separator=sep
  endfor
  all = routine_info(/functions)
  for i=0L,n_elements(all)-1L do begin
     if fileQ then begin
        if (routine_info(all[i],/FUNCTIONS,/SOURCE)).path eq file then $
           idlwave_print_info_entry,all[i],separator=sep,/FUNC
     endif else idlwave_print_info_entry,all[i],separator=sep,/FUNC
  endfor
  print,'>>>END OF IDLWAVE ROUTINE INFO'
end

pro idlwave_get_sysvars
  on_error,1
  catch,error_status
  if error_status ne 0 then begin
      print, 'Cannot get info about system variables'
  endif else begin
      help,/brief,output=s,/system_variables  ; ? unsafe use of OUTPUT=
      s = strtrim(strjoin(s,' ',/single),2)   ; make one line
      v = strsplit(s,' +',/regex,/extract)    ; get variables
      for i=0L,n_elements(v)-1 do begin
          t = ['']                            ; get tag list
          a=execute('if n_tags('+v[i]+') gt 0 then t=tag_names('+v[i]+')')
          print, 'IDLWAVE-SYSVAR: '+v[i]+' '+strjoin(t,' ',/single)
      endfor
  endelse
end

pro idlwave_get_class_tags, class
  res = execute('tags=tag_names({'+class+'})')
  if res then print,'IDLWAVE-CLASS-TAGS: '+class+' '+strjoin(tags,' ',/single)
end
;; END OF IDLWAVE SUPPORT ROUTINES
"
  "The IDL programs to get info from the shell.")

(defvar idlwave-idlwave_routine_info-compiled nil
  "Remember if the routine info procedure is already compiled.")

(defvar idlwave-shell-temp-pro-file)
(defvar idlwave-shell-temp-rinfo-save-file)

(defun idlwave-shell-compile-helper-routines (&optional wait)
  (unless (and idlwave-idlwave_routine_info-compiled
	       (file-readable-p (idlwave-shell-temp-file 'rinfo)))
    (with-current-buffer (idlwave-find-file-noselect
                          (idlwave-shell-temp-file 'pro))
      (erase-buffer)
      (insert idlwave-routine-info.pro)
      (save-buffer 0))
    (idlwave-shell-send-command
     (concat ".run \"" idlwave-shell-temp-pro-file "\"")
     nil 'hide wait)
    (idlwave-shell-send-command
     (format "save,'idlwave_print_safe','idlwave_routine_info','idlwave_print_info_entry','idlwave_get_class_tags','idlwave_get_sysvars',FILE='%s',/ROUTINES"
	     (idlwave-shell-temp-file 'rinfo))
     nil 'hide)
    (setq idlwave-idlwave_routine_info-compiled t))

  ;; Restore if necessary.  Must use execute to hide lame routine_info
  ;; errors on undefined routine
  (idlwave-shell-send-command
   (format "if execute(\"_v=routine_info('idlwave_routine_info',/SOURCE)\") eq 0 then restore,'%s' else if _v.path eq '' then restore,'%s'"
	   idlwave-shell-temp-rinfo-save-file
	   idlwave-shell-temp-rinfo-save-file)
   nil 'hide))


(defun idlwave-shell-update-routine-info (&optional quiet run-hooks wait file)
  "Query the shell for routine_info of compiled modules and update the lists."
  ;; Save and compile the procedure.  The compiled procedure is then
  ;; saved into an IDL SAVE file, to allow for fast RESTORE.  We may
  ;; need to test for and possibly RESTORE the procedure each time we
  ;; use it, since the user may have killed or redefined it.  In
  ;; particular, .RESET_SESSION will kill all user procedures.  If
  ;; FILE is set, only update routine info for routines in that file.

  (idlwave-shell-compile-helper-routines wait)
  ; execute the routine_info procedure, and analyze the output
  (idlwave-shell-send-command
   (format "idlwave_routine_info%s" (if file (concat ",'" file "'") ""))
   `(progn
      (idlwave-shell-routine-info-filter)
      (idlwave-concatenate-rinfo-lists ,quiet ,run-hooks))
   'hide wait))

;; ---------------------------------------------------------------------------
;;
;; Completion and displaying routine calling sequences

(defvar idlwave-completion-help-info nil)
(defvar idlwave-completion-help-links nil)
(defvar idlwave-current-obj_new-class nil)
(defvar idlwave-complete-special nil)
(defvar method-selector)
(defvar class-selector)
(defvar type-selector)
(defvar super-classes)

(defun idlwave-complete (&optional arg module class)
  "Complete a function, procedure or keyword name at point.
This function is smart and figures out what can be completed
at this point.
- At the beginning of a statement it completes procedure names.
- In the middle of a statement it completes function names.
- After a `(' or `,' in the argument list of a function or procedure,
  it completes a keyword of the relevant function or procedure.
- In the first arg of `OBJ_NEW', it completes a class name.

When several completions are possible, a list will be displayed in
the *Completions* buffer.  If this list is too long to fit into the
window, scrolling can be achieved by repeatedly pressing
\\[idlwave-complete].

The function also knows about object methods.  When it needs a class
name, the action depends upon `idlwave-query-class', which see.  You
can force IDLWAVE to ask you for a class name with a
\\[universal-argument] prefix argument to this command.

See also the variables `idlwave-keyword-completion-adds-equal' and
`idlwave-function-completion-adds-paren'.

The optional ARG can be used to specify the completion type in order
to override IDLWAVE's idea of what should be completed at point.
Possible values are:

0  <=>  query for the completion type
1  <=>  'procedure
2  <=>  'procedure-keyword
3  <=>  'function
4  <=>  'function-keyword
5  <=>  'procedure-method
6  <=>  'procedure-method-keyword
7  <=>  'function-method
8  <=>  'function-method-keyword
9  <=>  'class

As a special case, the universal argument C-u forces completion of
function names in places where the default would be a keyword.

Two prefix argument, C-u C-u, prompts for a regexp by which to limit
completion.

For Lisp programmers only:
When we force a keyword, optional argument MODULE can contain the module name.
When we force a method or a method keyword, CLASS can specify the class."
  (interactive "P")
  (idlwave-routines)
  (let* ((where-list
	  (if (and arg
		   (or (and (integerp arg) (not (equal arg '(16))))
		       (symbolp arg)))
	      (idlwave-make-force-complete-where-list arg module class)
	    (idlwave-where)))
	 (what (nth 2 where-list))
	 (idlwave-force-class-query (equal arg '(4)))
	 (completion-regexp-list
	  (if (equal arg '(16))
	      (list (read-string (concat "Completion Regexp: "))))))

    (if (and module (string-match "::" module))
	(setq class (substring module 0 (match-beginning 0))
	      module (substring module (match-end 0))))

    (cond

     ((and (null arg)
	   (eq (car-safe last-command) 'idlwave-display-completion-list)
	   (get-buffer-window "*Completions*"))
      (setq this-command last-command)
      (idlwave-scroll-completions))

     ;; Complete a filename in quotes
     ((and (idlwave-in-quote)
	   (not (eq what 'class)))
      (idlwave-complete-filename))

     ;; Check for any special completion functions
     ((and idlwave-complete-special
	   (idlwave-call-special idlwave-complete-special)))

     ((null what)
      (error "Nothing to complete here"))

     ;; Complete a class
     ((eq what 'class)
      (setq idlwave-completion-help-info '(class))
      (idlwave-complete-class))

     ((eq what 'procedure)
      ;; Complete a procedure name
      (let* ((cw-list (nth 3 where-list))
	     (class-selector (idlwave-determine-class cw-list 'pro))
	     (super-classes (unless (idlwave-explicit-class-listed cw-list)
			      (idlwave-all-class-inherits class-selector)))
	     (isa (concat "procedure" (if class-selector "-method" "")))
	     (type-selector 'pro))
	(setq idlwave-completion-help-info
	      (list 'routine nil type-selector class-selector nil super-classes))
	(idlwave-complete-in-buffer
	 'procedure (if class-selector 'method 'routine)
	 (idlwave-routines) 'idlwave-selector
	 (format "Select a %s name%s"
		 isa
		 (if class-selector
		     (format " (class is %s)"
			     (if (eq class-selector t)
				 "unknown" class-selector))
		   ""))
	 isa
	 'idlwave-attach-method-classes 'idlwave-add-file-link-selector)))

     ((eq what 'function)
      ;; Complete a function name
      (let* ((cw-list (nth 3 where-list))
	     (class-selector (idlwave-determine-class cw-list 'fun))
	     (super-classes (unless (idlwave-explicit-class-listed cw-list)
			      (idlwave-all-class-inherits class-selector)))
	     (isa (concat "function" (if class-selector "-method" "")))
	     (type-selector 'fun))
	(setq idlwave-completion-help-info
	      (list 'routine nil type-selector class-selector nil super-classes))
	(idlwave-complete-in-buffer
	 'function (if class-selector 'method 'routine)
	 (idlwave-routines) 'idlwave-selector
	 (format "Select a %s name%s"
		 isa
		 (if class-selector
		     (format " (class is %s)"
			     (if (eq class-selector t)
				 "unknown" class-selector))
		   ""))
	 isa
	 'idlwave-attach-method-classes 'idlwave-add-file-link-selector)))

     ((and (memq what '(procedure-keyword function-keyword)) ; Special Case
	   (equal arg '(4)))
      (idlwave-complete 3))

     ((eq what 'procedure-keyword)
      ;; Complete a procedure keyword
      (let* ((where (nth 3 where-list))
	     (name  (car where))
	     (method-selector name)
	     (type-selector 'pro)
	     (class (idlwave-determine-class where 'pro))
	     (class-selector class)
	     (super-classes (idlwave-all-class-inherits class-selector))
	     (isa (format "procedure%s-keyword" (if class "-method" "")))
	     (entry (idlwave-best-rinfo-assq
		     name 'pro class (idlwave-routines)))
	     (system (if entry (eq (car (nth 3 entry)) 'system)))
	     (list (idlwave-entry-keywords entry 'do-link)))
	(unless (or entry (eq class t))
	  (error "Nothing known about procedure %s"
		 (idlwave-make-full-name class name)))
	(setq list (idlwave-fix-keywords name 'pro class list
					 super-classes system))
	(unless list (error "No keywords available for procedure %s"
			    (idlwave-make-full-name class name)))
	(setq idlwave-completion-help-info
	      (list 'keyword name type-selector class-selector entry super-classes))
	(idlwave-complete-in-buffer
	 'keyword 'keyword list nil
	 (format "Select keyword for procedure %s%s"
		 (idlwave-make-full-name class name)
		 (if (or (member '("_EXTRA") list)
			 (member '("_REF_EXTRA") list))
		     " (note _EXTRA)" ""))
	 isa
	 'idlwave-attach-keyword-classes)))

     ((eq what 'function-keyword)
      ;; Complete a function keyword
      (let* ((where (nth 3 where-list))
	     (name  (car where))
	     (method-selector name)
	     (type-selector 'fun)
	     (class (idlwave-determine-class where 'fun))
	     (class-selector class)
	     (super-classes (idlwave-all-class-inherits class-selector))
	     (isa (format "function%s-keyword" (if class "-method" "")))
	     (entry (idlwave-best-rinfo-assq
		     name 'fun class (idlwave-routines)))
	     (system (if entry (eq (car (nth 3 entry)) 'system)))
	     (list (idlwave-entry-keywords entry 'do-link))
	     msg-name)
	(unless (or entry (eq class t))
	  (error "Nothing known about function %s"
		 (idlwave-make-full-name class name)))
	(setq list (idlwave-fix-keywords name 'fun class list
					 super-classes system))
	;; OBJ_NEW: Messages mention the proper Init method
	(setq msg-name (if (and (null class)
				(string= (upcase name) "OBJ_NEW"))
			   (concat idlwave-current-obj_new-class
				   "::Init (via OBJ_NEW)")
			 (idlwave-make-full-name class name)))
	(unless list (error "No keywords available for function %s"
			    msg-name))
	(setq idlwave-completion-help-info
	      (list 'keyword name type-selector class-selector nil super-classes))
	(idlwave-complete-in-buffer
	 'keyword 'keyword list nil
	 (format "Select keyword for function %s%s" msg-name
		 (if (or (member '("_EXTRA") list)
			 (member '("_REF_EXTRA") list))
		     " (note _EXTRA)" ""))
	 isa
	 'idlwave-attach-keyword-classes)))

     (t (error "This should not happen (idlwave-complete)")))))

(defvar idlwave-complete-special nil
  "List of special completion functions.
These functions are called for each completion.  Each function must
check if its own special completion context is present.  If yes, it
should use `idlwave-complete-in-buffer' to do some completion and
return t.  If such a function returns t, *no further* attempts to
complete other contexts will be done.  If the function returns nil,
other completions will be tried.")

(defun idlwave-call-special (functions &rest args)
  (let ((funcs functions)
	fun ret)
    (catch 'exit
      (while (setq fun (pop funcs))
	(if (setq ret (apply fun args))
	    (throw 'exit ret)))
      nil)))

(defun idlwave-make-force-complete-where-list (what &optional module class)
  ;; Return an artificial WHERE specification to force the completion
  ;; routine to complete a specific item independent of context.
  ;; WHAT is the prefix arg of `idlwave-complete', see there for details.
  ;; MODULE and CLASS can be used to specify the routine name and class.
  ;; The class name will also be found in MODULE if that is like "class::mod".
  (let* ((what-list '(("procedure") ("procedure-keyword")
		      ("function") ("function-keyword")
		      ("procedure-method") ("procedure-method-keyword")
		      ("function-method") ("function-method-keyword")
		      ("class")))
	 (module (idlwave-sintern-routine-or-method module class))
	 (class (idlwave-sintern-class class))
	 (what (cond
		((equal what 0)
		 (setq what
		       (intern (completing-read
				"Complete what? " what-list nil t))))
		((integerp what)
		 (setq what (intern (car (nth (1- what) what-list)))))
		((and what
		      (symbolp what)
		      (assoc (symbol-name what) what-list))
		 what)
		(t (error "Invalid WHAT"))))
	 (nil-list '(nil nil nil nil))
	 (class-list (list nil nil (or class t) nil)))

    (cond

     ((eq what 'procedure)
      (list nil-list nil-list 'procedure nil-list nil))

     ((eq what 'procedure-keyword)
      (let* ((class-selector nil)
	     (super-classes nil)
	     (type-selector 'pro)
	     (pro (or module
		      (idlwave-completing-read
		       "Procedure: " (idlwave-routines) 'idlwave-selector))))
	(setq pro (idlwave-sintern-routine pro))
	(list nil-list nil-list 'procedure-keyword
	      (list pro nil nil nil) nil)))

     ((eq what 'function)
      (list nil-list nil-list 'function nil-list nil))

     ((eq what 'function-keyword)
      (let* ((class-selector nil)
	     (super-classes nil)
	     (type-selector 'fun)
	     (func (or module
		       (idlwave-completing-read
			"Function: " (idlwave-routines) 'idlwave-selector))))
	(setq func (idlwave-sintern-routine func))
	(list nil-list nil-list 'function-keyword
	      (list func nil nil nil) nil)))

     ((eq what 'procedure-method)
      (list nil-list nil-list 'procedure class-list nil))

     ((eq what 'procedure-method-keyword)
      (let* ((class (idlwave-determine-class class-list 'pro))
	     (class-selector class)
	     (super-classes (idlwave-all-class-inherits class-selector))
	     (type-selector 'pro)
	     (pro (or module
		      (idlwave-completing-read
		       (format "Procedure in %s class: " class-selector)
		       (idlwave-routines) 'idlwave-selector))))
	(setq pro (idlwave-sintern-method pro))
	(list nil-list nil-list 'procedure-keyword
	      (list pro nil class nil) nil)))

     ((eq what 'function-method)
      (list nil-list nil-list 'function class-list nil))

     ((eq what 'function-method-keyword)
      (let* ((class (idlwave-determine-class class-list 'fun))
	     (class-selector class)
	     (super-classes (idlwave-all-class-inherits class-selector))
	     (type-selector 'fun)
	     (func (or module
		       (idlwave-completing-read
			(format "Function in %s class: " class-selector)
			(idlwave-routines) 'idlwave-selector))))
	(setq func (idlwave-sintern-method func))
	(list nil-list nil-list 'function-keyword
	      (list func nil class nil) nil)))

     ((eq what 'class)
      (list nil-list nil-list 'class nil-list nil))

     (t (error "Invalid value for WHAT")))))

(defun idlwave-completing-read (&rest args)
  ;; Completing read, case insensitive
  (let ((old-value (default-value 'completion-ignore-case)))
    (unwind-protect
	(progn
	  (setq-default completion-ignore-case t)
	  (apply 'completing-read args))
      (setq-default completion-ignore-case old-value))))

(defvar idlwave-shell-default-directory)
(defun idlwave-complete-filename ()
  "Use the comint stuff to complete a file name."
  (require 'comint)
  (let* ((comint-file-name-chars "~/A-Za-z0-9+@:_.$#%={}\\-")
	 (comint-completion-addsuffix nil)
	 (default-directory
	   (if (and (boundp 'idlwave-shell-default-directory)
		    (stringp idlwave-shell-default-directory)
		    (file-directory-p idlwave-shell-default-directory))
	       idlwave-shell-default-directory
	     default-directory)))
    (comint-dynamic-complete-filename)))

(defun idlwave-make-full-name (class name)
  ;; Make a fully qualified module name including the class name
  (concat (if class (format "%s::" class) "") name))

(defun idlwave-rinfo-assoc (name type class list)
  "Like `idlwave-rinfo-assq', but sintern strings first."
  (idlwave-rinfo-assq
   (idlwave-sintern-routine-or-method name class)
   type (idlwave-sintern-class class) list))

(defun idlwave-rinfo-assq (name type class list)
  ;; Works like assq, but also checks type and class
  (catch 'exit
    (let (match)
      (while (setq match (assq name list))
	(and (or (eq type t)
		 (eq (nth 1 match) type))
	     (eq (nth 2 match) class)
	     (throw 'exit match))
	(setq list (cdr (memq match list)))))))

(defun idlwave-rinfo-assq-any-class (name type class list)
  ;; Return the first matching method on the inheritance list
  (let* ((classes (cons class (idlwave-all-class-inherits class)))
	 class rtn)
    (while classes
      (if (setq rtn (idlwave-rinfo-assq name type (pop classes) list))
	  (setq classes nil)))
    rtn))

(defun idlwave-best-rinfo-assq (name type class list &optional with-file
				     keep-system)
  "Like `idlwave-rinfo-assq', but get all twins and sort, then return first.
If WITH-FILE is passed, find the best rinfo entry with a file
included.  If KEEP-SYSTEM is set, don't prune system for compiled
syslib files."
  (let ((twins (idlwave-routine-twins
		(idlwave-rinfo-assq-any-class name type class list)
		list))
	syslibp)
    (when (> (length twins) 1)
      (setq twins (sort twins 'idlwave-routine-entry-compare-twins))
      (if (and (null keep-system)
	       (eq 'system (car (nth 3 (car twins))))
	       (setq syslibp (idlwave-any-syslib (cdr twins)))
	       (not (equal 1 syslibp)))
	  ;; Its a compiled syslib, so we need to remove the system entry
	  (setq twins (cdr twins)))
      (if with-file
	  (setq twins (delq nil
			    (mapcar (lambda (x)
				      (if (nth 1 (nth 3 x)) x))
				    twins)))))
    (car twins)))

(defun idlwave-best-rinfo-assoc (name type class list &optional with-file
				     keep-system)
  "Like `idlwave-best-rinfo-assq', but sintern strings first."
  (idlwave-best-rinfo-assq
   (idlwave-sintern-routine-or-method name class)
   type (idlwave-sintern-class class) list with-file keep-system))

(defun idlwave-any-syslib (entries)
  "Does the entry list ENTRIES contain a syslib entry?
If yes, return the index (>=1)."
  (let (file (cnt 0))
    (catch 'exit
      (while entries
	(incf cnt)
	(setq file (idlwave-routine-source-file (nth 3 (car entries))))
	(if (and file (idlwave-syslib-p file))
	    (throw 'exit cnt)
	  (setq entries (cdr entries))))
      nil)))

(defun idlwave-all-assq (key list)
  "Return a list of all associations of Key in LIST."
  (let (rtn elt)
    (while (setq elt (assq key list))
      (push elt rtn)
      (setq list (cdr (memq elt list))))
    (nreverse rtn)))

(defun idlwave-all-method-classes (method &optional type)
  "Return all classes which have a method METHOD.
TYPE is 'fun or 'pro.
When TYPE is not specified, both procedures and functions will be considered."
  (if (null method)
      (mapcar 'car (idlwave-class-alist))
    (let (rtn)
      (mapc (lambda (x)
	      (and (nth 2 x)
		   (or (not type)
		       (eq type (nth 1 x)))
		   (push (nth 2 x) rtn)))
	    (idlwave-all-assq method (idlwave-routines)))
      (idlwave-uniquify rtn))))

(defun idlwave-all-method-keyword-classes (method keyword &optional type)
  "Return all classes which have a method METHOD with keyword KEYWORD.
TYPE is 'fun or 'pro.
When TYPE is not specified, both procedures and functions will be considered."
  (if (or (null method)
	  (null keyword))
      nil
    (let (rtn)
      (mapc (lambda (x)
	      (and (nth 2 x)		; non-nil class
		   (or (not type)	; correct or unspecified type
		       (eq type (nth 1 x)))
		   (assoc keyword (idlwave-entry-keywords x))
		   (push (nth 2 x) rtn)))
	    (idlwave-all-assq method (idlwave-routines)))
      (idlwave-uniquify rtn))))

(defun idlwave-members-only (list club)
  "Return list of all elements in LIST which are also in CLUB."
  (let (rtn)
    (while list
      (if (member (car list) club)
	  (setq rtn (cons (car list) rtn)))
      (setq list (cdr list)))
    (nreverse rtn)))

(defun idlwave-nonmembers-only (list club)
  "Return list of all elements in LIST which are not in CLUB."
  (let (rtn)
    (while list
      (if (member (car list) club)
	  nil
	(setq rtn (cons (car list) rtn)))
      (setq list (cdr list)))
    (nreverse rtn)))

(defun idlwave-explicit-class-listed (info)
  "Return whether or not the class is listed explicitly, ala a->b::c.
INFO is as returned by `idlwave-what-function' or `-procedure'."
  (let ((apos (nth 3 info)))
    (if apos
	(save-excursion (goto-char apos)
			(looking-at "->[a-zA-Z][a-zA-Z0-9$_]*::")))))

(defvar idlwave-determine-class-special nil
  "List of special functions for determining class.
Must accept two arguments: `apos' and `info'.")

(defun idlwave-determine-class (info type)
  ;; Determine the class of a routine call.
  ;; INFO is the `cw-list' structure as returned by idlwave-where.
  ;; The second element in this structure is the class.  When nil, we
  ;; return nil.  When t, try to get the class from text properties at
  ;; the arrow.  When the object is "self", we use the class of the
  ;; current routine.  otherwise prompt the user for a class name.
  ;; Also stores the selected class as a text property at the arrow.
  ;; TYPE is 'fun or 'pro.
  (let* ((class (nth 2 info))
	 (apos (nth 3 info))
	 (nassoc (assoc (if (stringp (car info))
			    (upcase (car info))
			  (car info))
			idlwave-query-class))
	 (dassoc (assq (if (car info) 'keyword-default 'method-default)
		       idlwave-query-class))
	 (query (cond (nassoc (cdr nassoc))
		      (dassoc (cdr dassoc))
		      (t t)))
	 (arrow (and apos (string= (buffer-substring apos (+ 2 apos)) "->")))
	 (is-self
	  (and arrow
	       (save-excursion (goto-char apos)
			       (forward-word -1)
			       (let ((case-fold-search t))
				 (looking-at "self\\>")))))
	 (force-query idlwave-force-class-query)
	 store special-class class-alist)
    (cond
     ((null class) nil)
     ((eq t class)
      ;; There is an object which would like to know its class
      (if (and arrow (get-text-property apos 'idlwave-class)
	       idlwave-store-inquired-class
	       (not force-query))
	  (setq class (get-text-property apos 'idlwave-class)
		class (idlwave-sintern-class class)))
      (if (and (eq t class) is-self)
	  (setq class (or (nth 2 (idlwave-current-routine)) class)))

      ;; Before prompting, try any special class determination routines
      (when (and (eq t class)
		 idlwave-determine-class-special
		 (not force-query))
	(setq special-class
	      (idlwave-call-special idlwave-determine-class-special apos))
	(if special-class
	    (setq class (idlwave-sintern-class special-class)
		  store idlwave-store-inquired-class)))

      ;; Prompt for a class, if we need to
      (when (and (eq class t)
		 (or force-query query))
	(setq class-alist
	      (mapcar 'list (idlwave-all-method-classes (car info) type)))
	(setq class
	      (idlwave-sintern-class
	       (cond
		((and (= (length class-alist) 0) (not force-query))
		 (error "No classes available with method %s" (car info)))
		((and (= (length class-alist) 1) (not force-query))
		 (car (car class-alist)))
		(t
		 (setq store idlwave-store-inquired-class)
		 (idlwave-completing-read
		  (format "Class%s: " (if (stringp (car info))
					  (format " for %s method %s"
						  type (car info))
					""))
		  class-alist nil nil nil 'idlwave-class-history))))))

      ;; Store it, if requested
      (when (and class (not (eq t class)))
	;; We have a real class here
	(when (and store arrow)
	  (condition-case ()
	      (add-text-properties
	       apos (+ apos 2)
	       `(idlwave-class ,class face ,idlwave-class-arrow-face
			       rear-nonsticky t))
	    (error nil)))
	(setf (nth 2 info) class))
      ;; Return the class
      class)
     ;; Default as fallback
     (t class))))

(defun idlwave-selector (a)
  (and (eq (nth 1 a) type-selector)
       (or (and (nth 2 a) (eq class-selector t))
	   (eq (nth 2 a) class-selector)
	   (memq (nth 2 a) super-classes))))

(defun idlwave-add-file-link-selector (a)
  ;; Record a file link, if any, for the tested names during selection.
  (let ((sel (idlwave-selector a)) file)
    (if (and sel (setq file (idlwave-entry-has-help a)))
	(push (cons (car a) file) idlwave-completion-help-links))
    sel))


(defun idlwave-where ()
  "Find out where we are.
The return value is a list with the following stuff:
\(PRO-LIST FUNC-LIST COMPLETE-WHAT CW-LIST LAST-CHAR)

PRO-LIST       (PRO POINT CLASS ARROW)
FUNC-LIST      (FUNC POINT CLASS ARROW)
COMPLETE-WHAT  a symbol indicating what kind of completion makes sense here
CW-LIST        (PRO-OR-FUNC POINT CLASS ARROW)  Like PRO-LIST, for what can
               be completed here.
LAST-CHAR      last relevant character before point (non-white non-comment,
               not part of current identifier or leading slash).

In the lists, we have these meanings:
PRO:    Procedure name
FUNC:   Function name
POINT:  Where is this
CLASS:  What class has the routine (nil=no, t=is method, but class unknown)
ARROW:  Location of the arrow"
  (idlwave-routines)
  (let* (;(bos (save-excursion (idlwave-beginning-of-statement) (point)))
         (bos (save-excursion (idlwave-start-of-substatement 'pre) (point)))
 	 (func-entry (idlwave-what-function bos))
         (func (car func-entry))
         (func-class (nth 1 func-entry))
         (func-arrow (nth 2 func-entry))
	 (func-point (or (nth 3 func-entry) 0))
	 (func-level (or (nth 4 func-entry) 0))
	 (pro-entry (idlwave-what-procedure bos))
	 (pro (car pro-entry))
         (pro-class (nth 1 pro-entry))
         (pro-arrow (nth 2 pro-entry))
	 (pro-point (or (nth 3 pro-entry) 0))
	 (last-char (idlwave-last-valid-char))
         (case-fold-search t)
	 (match-string (buffer-substring bos (point)))
	 cw cw-mod cw-arrow cw-class cw-point)
    (if (< func-point pro-point) (setq func nil))
    (cond
     ((string-match "\\`[ \t]*\\(pro\\|function\\)[ \t]+[a-zA-Z0-9_]*\\'"
                    match-string)
      (setq cw 'class))
     ((string-match
       "\\`[ \t]*\\([a-zA-Z][a-zA-Z0-9$_]*\\)?\\'"
       (if (> pro-point 0)
	   (buffer-substring pro-point (point))
	 match-string))
      (setq cw 'procedure cw-class pro-class cw-point pro-point
	    cw-arrow pro-arrow))
     ((string-match "\\`[ \t]*\\(pro\\|function\\)\\>"
		    match-string)
      nil)
     ((string-match "OBJ_NEW([ \t]*['\"]\\([a-zA-Z0-9$_]*\\)?\\'"
		    match-string)
      (setq cw 'class))
     ((string-match "\\<inherits\\s-+\\([a-zA-Z0-9$_]*\\)?\\'"
		    match-string)
      (setq cw 'class))
     ((and func
	   (> func-point pro-point)
	   (= func-level 1)
	   (memq last-char '(?\( ?,)))
      (setq cw 'function-keyword cw-mod func cw-point func-point
	    cw-class func-class cw-arrow func-arrow))
     ((and pro (eq last-char ?,))
      (setq cw 'procedure-keyword cw-mod pro cw-point pro-point
	    cw-class pro-class cw-arrow pro-arrow))
;     ((member last-char '(?\' ?\) ?\] ?!))
;      ;; after these chars, a function makes no sense
;      ;; FIXME: I am sure there can be more in this list
;      ;; FIXME: Do we want to do this at all?
;      nil)
     ;; Everywhere else we try a function.
     (t
      (setq cw 'function)
      (save-excursion
	(if (re-search-backward "->[ \t]*\\(\\$[ \t]*\\(;.*\\)?\n\\s-*\\)?\\(\\([$a-zA-Z0-9_]+\\)::\\)?[$a-zA-Z0-9_]*\\=" bos t)
	    (setq cw-arrow (copy-marker (match-beginning 0))
		  cw-class (if (match-end 4)
			       (idlwave-sintern-class (match-string 4))
			     t))))))
    (list (list pro pro-point pro-class pro-arrow)
          (list func func-point func-class func-arrow)
          cw
	  (list cw-mod cw-point cw-class cw-arrow)
	  last-char)))

(defun idlwave-this-word (&optional class)
  ;; Grab the word around point.  CLASS is for the `skip-chars=...' functions
  (setq class (or class "a-zA-Z0-9$_."))
  (save-excursion
    (buffer-substring
     (progn (skip-chars-backward class) (point))
     (progn (skip-chars-forward  class) (point)))))

(defun idlwave-what-function (&optional bound)
  ;; Find out if point is within the argument list of a function.
  ;; The return value is ("function-name" class arrow-start (point) level).
  ;; Level is 1 on the top level parentheses, higher further down.

  ;; If the optional BOUND is an integer, bound backwards directed
  ;;    searches to this point.

  (catch 'exit
    (let (pos
	  func-point
	  (cnt 0)
	  func arrow-start class)
      (idlwave-with-special-syntax
       (save-restriction
	 (save-excursion
	   (narrow-to-region (max 1 (or bound 0)) (point-max))
	   ;; move back out of the current parenthesis
	   (while (condition-case nil
		      (progn (up-list -1) t)
		    (error nil))
	     (setq pos (point))
	     (incf cnt)
	     (when (and (= (following-char) ?\()
			(re-search-backward
			 "\\(::\\|\\<\\)\\([a-zA-Z][a-zA-Z0-9$_]*\\)[ \t]*\\="
			 bound t))
	       (setq func (match-string 2)
		     func-point (goto-char (match-beginning 2))
		     pos func-point)
	       (if (re-search-backward
		    "->[ \t]*\\(\\([a-zA-Z][a-zA-Z0-9$_]*\\)::\\)?\\=" bound t)
		   (setq arrow-start (copy-marker (match-beginning 0))
			 class (or (match-string 2) t)))
	       (throw
		'exit
		(list
		 (idlwave-sintern-routine-or-method func class)
		 (idlwave-sintern-class class)
		 arrow-start func-point cnt)))
	     (goto-char pos))
	   (throw 'exit nil)))))))

(defun idlwave-what-procedure (&optional bound)
  ;; Find out if point is within the argument list of a procedure.
  ;; The return value is ("procedure-name" class arrow-pos (point)).

  ;; If the optional BOUND is an integer, bound backwards directed
  ;;    searches to this point.
  (let ((pos (point)) pro-point
	pro class arrow-start string)
    (save-excursion
      ;;(idlwave-beginning-of-statement)
      (idlwave-start-of-substatement 'pre)
      (setq string (buffer-substring (point) pos))
      (if (string-match
	   "\\`[ \t]*\\([a-zA-Z][a-zA-Z0-9$_]*\\)[ \t]*\\(,\\|\\'\\)" string)
	  (setq pro (match-string 1 string)
		pro-point (+ (point) (match-beginning 1)))
	(if (and (idlwave-skip-object)
		 (setq string (buffer-substring (point) pos))
		 (string-match
		  "\\`[ \t]*\\(->\\)[ \t]*\\(\\([a-zA-Z][a-zA-Z0-9$_]*\\)::\\)?\\([a-zA-Z][a-zA-Z0-9$_]*\\)?[ \t]*\\(,\\|\\(\\$\\s *\\(;.*\\)?\\)?$\\)"
		  string))
	    (setq pro (if (match-beginning 4)
			  (match-string 4 string))
		  pro-point (if (match-beginning 4)
			        (+ (point) (match-beginning 4))
			pos)
		  arrow-start (copy-marker (+ (point) (match-beginning 1)))
		  class (or (match-string 3 string) t)))))
    (list (idlwave-sintern-routine-or-method pro class)
	  (idlwave-sintern-class class)
	  arrow-start
	  pro-point)))

(defun idlwave-skip-object ()
  ;; If there is an object at point, move over it and return t.
  (let ((pos (point)))
    (if (catch 'exit
	  (save-excursion
	    (skip-chars-forward " 	")  ; white space
	    (skip-chars-forward "*")        ; de-reference
	    (cond
	     ((looking-at idlwave-identifier)
	      (goto-char (match-end 0)))
	     ((eq (following-char) ?\()
	      nil)
	     (t (throw 'exit nil)))
	    (catch 'endwhile
	      (while t
		(cond ((eq (following-char) ?.)
		       (forward-char 1)
		       (if (not (looking-at idlwave-identifier))
			   (throw 'exit nil))
		       (goto-char (match-end 0)))
		      ((memq (following-char) '(?\( ?\[))
		       (condition-case nil
			   (forward-list 1)
			 (error (throw 'exit nil))))
		      (t (throw 'endwhile t)))))
	    (if (looking-at "[ \t]*->")
		(throw 'exit (setq pos (match-beginning 0)))
	      (throw 'exit nil))))
	(goto-char pos)
      nil)))

(defun idlwave-last-valid-char ()
  "Return the last character before point which is not white or a comment
and also not part of the current identifier.  Since we do this in
order to identify places where keywords are, we consider the initial
`/' of a keyword as part of the identifier.
This function is not general, can only be used for completion stuff."
  (catch 'exit
    (save-excursion
      ;; skip the current identifier
      (skip-chars-backward "a-zA-Z0-9_$")
      ;; also skip a leading slash which might be belong to the keyword
      (if (eq (preceding-char) ?/)
	  (backward-char 1))
      ;; FIXME: does not check if this is a valid identifier
      (while t
	(skip-chars-backward " \t")
	(cond
	 ((memq (preceding-char) '(?\; ?\$)) (throw 'exit nil))
	 ((eq (preceding-char) ?\n)
	  (beginning-of-line 0)
	  (if (looking-at "\\([^\n]*\\)\\$[ \t]*\\(;[^\n]*\\)?\n")
	      ;; continuation line
	      (goto-char (match-end 1))
	    (throw 'exit nil)))
	 (t (throw 'exit (preceding-char))))))))

(defvar idlwave-complete-after-success-form nil
  "A form to evaluate after successful completion.")
(defvar idlwave-complete-after-success-form-force nil
  "A form to evaluate after completion selection in *Completions* buffer.")
(defconst idlwave-completion-mark (make-marker)
  "A mark pointing to the beginning of the completion string.")
(defvar completion-highlight-first-word-only) ;XEmacs.

(defun idlwave-complete-in-buffer (type stype list selector prompt isa
					&optional prepare-display-function
					special-selector)
  "Perform TYPE completion of word before point against LIST.
SELECTOR is the PREDICATE argument for the completion function.  Show
PROMPT in echo area.  TYPE is one of the intern types, e.g. 'function,
'procedure, 'class-tag, 'keyword, 'sysvar, etc.  SPECIAL-SELECTOR is
used only once, for `all-completions', and can be used to, e.g.,
accumulate information on matching completions."
  (let* ((completion-ignore-case t)
	 beg (end (point)) slash part spart completion all-completions
	 dpart dcompletion)

    (unless list
      (error (concat prompt ": No completions available")))

    ;; What is already in the buffer?
    (save-excursion
      (skip-chars-backward "a-zA-Z0-9_$")
      (setq slash (eq (preceding-char) ?/)
	    beg (point)
	    idlwave-complete-after-success-form
	    (list 'idlwave-after-successful-completion
		  (list 'quote type) slash beg)
	    idlwave-complete-after-success-form-force
	    (list 'idlwave-after-successful-completion
		  (list 'quote type) slash (list 'quote 'force))))

    ;; Try a completion
    (setq part (buffer-substring beg end)
	  dpart (downcase part)
	  spart (idlwave-sintern stype part)
	  completion (try-completion part list selector)
	  dcompletion (if (stringp completion) (downcase completion))
	  idlwave-completion-help-links nil)
    (cond
     ((null completion)
      ;; nothing available.
      (error (concat prompt ": no completion for \"%s\"") part))
     ((and (not (equal dpart dcompletion))
	   (not (eq t completion)))
      ;; We can add something
      (delete-region beg end)
      (insert (if (and (string= part dpart)
                       (or (not (string= part ""))
                           idlwave-complete-empty-string-as-lower-case)
                       (not idlwave-completion-force-default-case))
                  dcompletion
                completion))
      (if (eq t (try-completion completion list selector))
	  ;; Now this is a unique match
	  (idlwave-after-successful-completion type slash beg))
      t)
     ((or (eq completion t)
	  (and (= 1 (length (setq all-completions
				  (idlwave-uniquify
				   (all-completions part list
						    (or special-selector
							selector))))))
	       (equal dpart dcompletion)))
      ;; This is already complete
      (idlwave-after-successful-completion type slash beg)
      (message "%s is already the complete %s" part isa)
      nil)
     (t
      ;; We cannot add something - offer a list.
      (message "Making completion list...")

      (unless idlwave-completion-help-links ; already set somewhere?
	(mapc (lambda (x)  ; Pass link prop through to highlight-linked
		(let ((link (get-text-property 0 'link (car x))))
		  (if link
		      (push (cons (car x) link)
			    idlwave-completion-help-links))))
	      list))
      (let* ((list all-completions)
	     ;; "complete" means, this is already a valid completion
	     (complete (memq spart all-completions))
	     (completion-highlight-first-word-only t)) ; XEmacs
	     ;; (completion-fixup-function             ; Emacs
	     ;;  (lambda () (and (eq (preceding-char) ?>)
	     ;;    	      (re-search-backward " <" beg t)))))

	(setq list (sort list (lambda (a b)
				(string< (downcase a) (downcase b)))))
	(if prepare-display-function
	    (setq list (funcall prepare-display-function list)))
	(if (and (string= part dpart)
		 (or (not (string= part ""))
		     idlwave-complete-empty-string-as-lower-case)
		 (not idlwave-completion-force-default-case))
	    (setq list (mapcar (lambda (x)
				 (if (listp x)
				     (setcar x (downcase (car x)))
				   (setq x (downcase x)))
				 x)
			       list)))
	(idlwave-display-completion-list list prompt beg complete))
      t))))

(defun idlwave-complete-class ()
  "Complete a class at point."
  (interactive)
  ;; Call `idlwave-routines' to make sure the class list will be available
  (idlwave-routines)
  ;; Check for the special case of completing empty string after pro/function
  (if (let ((case-fold-search t))
	(save-excursion
	  (and
	   (re-search-backward "\\<\\(pro\\|function\\)[ \t]+\\="
			       (- (point) 15) t)
	   (goto-char (point-min))
	   (re-search-forward
	    "^[ \t]*\\(pro\\|function\\)[ \t]+\\([a-zA-Z0-9_]+::\\)" nil t))))
      ;; Yank the full class specification
      (insert (match-string 2))
    ;; Do the completion, using list gathered from `idlwave-routines'
    (idlwave-complete-in-buffer
     'class 'class (idlwave-class-alist) nil
     "Select a class" "class"
     (lambda (list) ;; Push it to help-links if system help available
       (mapcar (lambda (x)
                 (let* ((entry (idlwave-class-info x))
                        (link (nth 1 (assq 'link entry))))
                   (if link (push (cons x link)
                                  idlwave-completion-help-links))
                   x))
               list)))))

(defun idlwave-attach-classes (list type show-classes)
  ;; Attach the proper class list to a LIST of completion items.
  ;; TYPE, when 'kwd, shows classes for method keywords, when
  ;; 'class-tag, for class tags, and otherwise for methods.
  ;; SHOW-CLASSES is the value of `idlwave-completion-show-classes'.
  (if (or (null show-classes)           ; don't want to see classes
	  (null class-selector)         ; not a method call
	  (and
	   (stringp class-selector) ; the class is already known
	   (not super-classes)))    ; no possibilities for inheritance
      ;; In these cases, we do not have to do anything
      list
    (let* ((do-prop (and (>= show-classes 0)
			 (>= emacs-major-version 21)))
	   (do-buf (not (= show-classes 0)))
	   ;; (do-dots (featurep 'xemacs))
	   (do-dots t)
	   (inherit (if (and (not (eq type 'class-tag)) super-classes)
			(cons class-selector super-classes)))
	   (max (abs show-classes))
	   (lmax (if do-dots (apply 'max (mapcar 'length list))))
	  classes nclasses class-info space)
      (mapcar
       (lambda (x)
	 ;; get the classes
	 (if (eq type 'class-tag)
	     ;; Just one class for tags
	     (setq classes
		   (list
		    (idlwave-class-or-superclass-with-tag class-selector x)))
	   ;; Multiple classes for method or method-keyword
	   (setq classes
		 (if (eq type 'kwd)
		     (idlwave-all-method-keyword-classes
		      method-selector x type-selector)
		   (idlwave-all-method-classes x type-selector)))
	   (if inherit
	       (setq classes
		     (delq nil
			   (mapcar (lambda (x) (if (memq x inherit) x nil))
				   classes)))))
	 (setq nclasses (length classes))
	 ;; Make the separator between item and class-info
	 (if do-dots
	     (setq space (concat " " (make-string (- lmax (length x)) ?.)))
	   (setq space " "))
	 (if  do-buf
	     ;; We do want info in the buffer
	     (if (<= nclasses max)
		 (setq class-info (concat
				   space
				   "<" (mapconcat 'identity classes ",") ">"))
	       (setq class-info (format "%s<%d classes>" space nclasses)))
	   (setq class-info nil))
	 (when do-prop
	   ;; We do want properties
	   (setq x (copy-sequence x))
	   (put-text-property 0 (length x)
                              'help-echo (mapconcat 'identity classes " ")
                              x))
	 (if class-info
	     (list x class-info)
	   x))
       list))))

(defun idlwave-attach-method-classes (list)
  ;; Call idlwave-attach-classes with method parameters
  (idlwave-attach-classes list 'method idlwave-completion-show-classes))
(defun idlwave-attach-keyword-classes (list)
  ;; Call idlwave-attach-classes with keyword parameters
  (idlwave-attach-classes list 'kwd idlwave-completion-show-classes))
(defun idlwave-attach-class-tag-classes (list)
  ;; Call idlwave-attach-classes with class structure tags
  (idlwave-attach-classes list 'class-tag idlwave-completion-show-classes))


;;----------------------------------------------------------------------
;;----------------------------------------------------------------------
;;----------------------------------------------------------------------
;;----------------------------------------------------------------------
;;----------------------------------------------------------------------
(when (featurep 'xemacs)
  (defvar rtn)
  (defun idlwave-pset (item)
    (set 'rtn item)))

(defun idlwave-popup-select (ev list title &optional sort)
  "Select an item in LIST with a popup menu.
TITLE is the title to put atop the popup.  If SORT is non-nil,
sort the list before displaying."
  (let ((maxpopup idlwave-max-popup-menu-items)
	rtn menu)
    (cond ((null list))
	  ((= 1 (length list))
	   (setq rtn (car list)))
	  ((featurep 'xemacs)
	   (if sort (setq list (sort list (lambda (a b)
					    (string< (upcase a) (upcase b))))))
	   (setq menu
		 (append (list title)
			 (mapcar (lambda (x) (vector x (list 'idlwave-pset
							     x)))
				 list)))
	   (setq menu (idlwave-split-menu-xemacs menu maxpopup))
	   (let ((resp (get-popup-menu-response menu)))
             (funcall (event-function resp) (event-object resp))))
	  (t
	   (if sort (setq list (sort list (lambda (a b)
					    (string< (upcase a) (upcase b))))))
	   (setq menu (cons title
			    (list
			     (append (list "")
				     (mapcar (lambda(x) (cons x x)) list)))))
	   (setq menu (idlwave-split-menu-emacs menu maxpopup))
	   (setq rtn (x-popup-menu ev menu))))
    rtn))

(defun idlwave-split-menu-xemacs (menu N)
  "Split the MENU into submenus of maximum length N."
  (if (<= (length menu) (1+ N))
      ;; No splitting needed
      menu
    (let* ((title (car menu))
	   (entries (cdr menu))
	   (menu (list title))
	   (cnt 0)
	   (nextmenu nil))
      (while entries
	(while (and entries (< cnt N))
	  (setq cnt (1+ cnt)
		nextmenu (cons (car entries) nextmenu)
		entries (cdr entries)))
	(setq nextmenu (nreverse nextmenu))
	(setq nextmenu (cons (format "%s...%s"
				     (aref (car nextmenu) 0)
				     (aref (nth (1- cnt) nextmenu) 0))
			     nextmenu))
	(setq menu (cons nextmenu menu)
	      nextmenu nil
	      cnt 0))
      (nreverse menu))))

(defun idlwave-split-menu-emacs (menu N)
  "Split the MENU into submenus of maximum length N."
  (if (<= (length (nth 1 menu)) (1+ N))
      ;; No splitting needed
      menu
    (let* ((title (car menu))
	   (entries (cdr (nth 1 menu)))
	   (menu nil)
	   (cnt 0)
	   (nextmenu nil))
      (while entries
	(while (and entries (< cnt N))
	  (setq cnt (1+ cnt)
		nextmenu (cons (car entries) nextmenu)
		entries (cdr entries)))
	(setq nextmenu (nreverse nextmenu))
	(prin1 nextmenu)
	(setq nextmenu (cons (format "%s...%s"
				     (car (car nextmenu))
				     (car (nth (1- cnt) nextmenu)))
			     nextmenu))
	(setq menu (cons nextmenu menu)
	      nextmenu nil
	      cnt 0))
      (setq menu (nreverse menu))
      (setq menu (cons title menu))
      menu)))

(defvar idlwave-completion-setup-hook nil)

(defun idlwave-scroll-completions (&optional message)
  "Scroll the completion window on this frame."
  (let ((cwin (get-buffer-window "*Completions*" 'visible))
	(win (selected-window)))
    (unwind-protect
	(progn
	  (select-window cwin)
	  (condition-case nil
	      (scroll-up)
	    (error (if (and (listp last-command)
			    (nth 2 last-command))
		       (progn
			 (select-window win)
			 (eval idlwave-complete-after-success-form))
		     (set-window-start cwin (point-min)))))
	  (and message (message "%s" message)))
      (select-window win))))

(defun idlwave-display-completion-list (list &optional message beg complete)
  "Display the completions in LIST in the completions buffer and echo MESSAGE."
  (unless (and (get-buffer-window "*Completions*")
	       (idlwave-local-value 'idlwave-completion-p "*Completions*"))
    (move-marker idlwave-completion-mark beg)
    (setq idlwave-before-completion-wconf (current-window-configuration)))

  (if (featurep 'xemacs)
      (idlwave-display-completion-list-xemacs
       list)
    (idlwave-display-completion-list-emacs list))

  ;; Store a special value in `this-command'.  When `idlwave-complete'
  ;; finds this in `last-command', it will scroll the *Completions* buffer.
  (setq this-command (list 'idlwave-display-completion-list message complete))

  ;; Mark the completions buffer as created by cib
  (idlwave-set-local 'idlwave-completion-p t "*Completions*")

  ;; Fontify the classes
  (if (and idlwave-completion-fontify-classes
           (consp (car list)))
      (idlwave-completion-fontify-classes))

  ;; Run the hook
  (run-hooks 'idlwave-completion-setup-hook)

  ;; Display the message
  (message "%s" (or message "Making completion list...done")))

(defun idlwave-choose (function &rest args)
  "Call FUNCTION as a completion chooser and pass ARGS to it."
  (let ((completion-ignore-case t))	    ; install correct value
    (apply function args))
  (if (and (derived-mode-p 'idlwave-shell-mode)
	   (boundp 'font-lock-mode)
	   (not font-lock-mode))
      ;; For the shell, remove the fontification of the word before point
      (let ((beg (save-excursion
		   (skip-chars-backward "a-zA-Z0-9_")
		   (point))))
	(remove-text-properties beg (point) '(face nil))))
  (eval idlwave-complete-after-success-form-force))

(defun idlwave-keyboard-quit ()
  (interactive)
  (unwind-protect
      (if (eq (car-safe last-command) 'idlwave-display-completion-list)
	  (idlwave-restore-wconf-after-completion))
    (keyboard-quit)))

(defun idlwave-restore-wconf-after-completion ()
  "Restore the old (before completion) window configuration."
  (and idlwave-completion-restore-window-configuration
       idlwave-before-completion-wconf
       (set-window-configuration idlwave-before-completion-wconf)))

(defun idlwave-one-key-select (sym prompt delay)
  "Make the user select an element from the alist in the variable SYM.
The keys of the alist are expected to be strings.  The function returns the
car of the selected association.
To do this, PROMPT is displayed and the user must hit a letter key to
select an entry.  If the user does not reply within DELAY seconds, a help
window with the options is displayed automatically.
The key which is associated with each option is generated automatically.
First, the strings are checked for preselected keys, like in \"[P]rint\".
If these don't exist, a letter in the string is automatically selected."
  (let* ((alist (symbol-value sym))
         (temp-buffer-show-hook (if (fboundp 'fit-window-to-buffer)
				    '(fit-window-to-buffer)))
         keys-alist char)
    ;; First check the cache
    (if (and (eq (symbol-value sym) (get sym :one-key-alist-last)))
        (setq keys-alist (get sym :one-key-alist-cache))
      ;; Need to make new list
      (setq keys-alist (idlwave-make-one-key-alist alist))
      (put sym :one-key-alist-cache keys-alist)
      (put sym :one-key-alist-last alist))
    ;; Display prompt and wait for quick reply
    (message "%s[%s]" prompt
             (mapconcat (lambda(x) (char-to-string (car x)))
                        keys-alist ""))
    (if (sit-for delay)
        ;; No quick reply: Show help
        (save-window-excursion
          (with-output-to-temp-buffer "*Completions*"
	    (dolist (x keys-alist)
	      (princ (nth 1 x))
	      (princ "\n")))
          (setq char (read-char)))
      (setq char (read-char)))
    (message nil)
    ;; Return the selected result
    (nth 2 (assoc char keys-alist))))

;; Used for, e.g., electric debug super-examine.
(defun idlwave-make-one-key-alist (alist)
  "Make an alist for single key selection."
  (let ((l alist) keys-alist name start char help
        (cnt 0)
        (case-fold-search nil))
    (while l
      (setq name (car (car l))
            l (cdr l))
      (catch 'exit
        ;; First check if the configuration predetermined a key
        (if (string-match "\\[\\(.\\)\\]" name)
            (progn
              (setq char (string-to-char (downcase (match-string 1 name)))
                    help (format "%c:  %s" char name)
                    keys-alist (cons (list char help name) keys-alist))
              (throw 'exit t)))
        ;; Then check for capital letters
        (setq start 0)
        (while (string-match "[A-Z]" name start)
          (setq start (match-end 0)
                char (string-to-char (downcase (match-string 0 name))))
          (if (not (assoc char keys-alist))
              (progn
                (setq help (format "%c:  %s" char
                                   (replace-match
                                    (concat "[" (match-string 0 name) "]")
                                          t t name))
                      keys-alist (cons (list char help name) keys-alist))
                (throw 'exit t))))
        ;; Now check for lowercase letters
        (setq start 0)
        (while (string-match "[a-z]" name start)
          (setq start (match-end 0)
                char (string-to-char (match-string 0 name)))
          (if (not (assoc char keys-alist))
              (progn
                (setq help (format "%c:  %s" char
                                   (replace-match
                                    (concat "[" (match-string 0 name) "]")
                                    t t name))
                      keys-alist (cons (list char help name) keys-alist))
                (throw 'exit t))))
        ;; Bummer, nothing found!  Use a stupid number
        (setq char (string-to-char (int-to-string (setq cnt (1+ cnt))))
              help (format "%c:  %s" char name)
              keys-alist (cons (list char help name) keys-alist))))
    (nreverse keys-alist)))

(defun idlwave-set-local (var value &optional buffer)
  "Set the buffer-local value of VAR in BUFFER to VALUE."
  (with-current-buffer (or buffer (current-buffer))
    (set (make-local-variable var) value)))

(defun idlwave-local-value (var &optional buffer)
  "Return the value of VAR in BUFFER, but only if VAR is local to BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (and (local-variable-p var (current-buffer))
	 (symbol-value var))))

;; In XEmacs, we can use :activate-callback directly to advice the
;; choose functions.  We use the private keymap only for the online
;; help feature.

(defvar idlwave-completion-map nil
  "Keymap for `completion-list-mode' with `idlwave-complete'.")

(defun idlwave-display-completion-list-xemacs (list &rest cl-args)
  (with-output-to-temp-buffer "*Completions*"
    (apply 'display-completion-list list
	   ':activate-callback 'idlwave-default-choose-completion
	   cl-args))
  (with-current-buffer "*Completions*"
    (use-local-map
     (or idlwave-completion-map
	 (setq idlwave-completion-map
	       (idlwave-make-modified-completion-map-xemacs
		(current-local-map)))))))

(defun idlwave-default-choose-completion (&rest args)
  "Execute `default-choose-completion' and then restore the win-conf."
  (apply 'idlwave-choose 'default-choose-completion args))

(defun idlwave-make-modified-completion-map-xemacs (old-map)
  "Replace `choose-completion' and `mouse-choose-completion' in OLD-MAP."
  (let ((new-map (copy-keymap old-map)))
    (define-key new-map [button3up] 'idlwave-mouse-completion-help)
    (define-key new-map [button3] (lambda ()
				    (interactive)
				    (setq this-command last-command)))
    new-map))

;; In Emacs we also replace keybindings in the completion
;; map in order to install our wrappers.

(defun idlwave-display-completion-list-emacs (list)
  "Display completion list and install the choose wrappers."
  (with-output-to-temp-buffer "*Completions*"
    (display-completion-list list))
  (with-current-buffer "*Completions*"
    (use-local-map
     (or idlwave-completion-map
	 (setq idlwave-completion-map
	       (idlwave-make-modified-completion-map-emacs
		(current-local-map)))))))

(defun idlwave-make-modified-completion-map-emacs (old-map)
  "Replace `choose-completion' and `mouse-choose-completion' in OLD-MAP."
  (let ((new-map (copy-keymap old-map)))
    (substitute-key-definition
     'choose-completion 'idlwave-choose-completion new-map)
    (substitute-key-definition
     'mouse-choose-completion 'idlwave-mouse-choose-completion new-map)
    (define-key new-map [mouse-3] 'idlwave-mouse-completion-help)
    new-map))

(defun idlwave-choose-completion (&rest args)
  "Choose the completion that point is in or next to."
  (interactive)
  (apply 'idlwave-choose 'choose-completion args))

(defun idlwave-mouse-choose-completion (&rest args)
  "Click on an alternative in the `*Completions*' buffer to choose it."
  (interactive "e")
  (apply 'idlwave-choose 'mouse-choose-completion args))

;;----------------------------------------------------------------------
;;----------------------------------------------------------------------

;;; ------------------------------------------------------------------------
;;; Structure parsing code, and code to manage class info

;;
;; - Go again over the documentation how to write a completion
;;   plugin.  It is in self.el, but currently still very bad.
;;   This could be in a separate file in the distribution, or
;;   in an appendix for the manual.

(defvar idlwave-struct-skip
  "[ \t]*\\(\\$.*\n\\(^[ \t]*\\(\\$[ \t]*\\)?\\(;.*\\)?\n\\)*\\)?[ \t]*"
  "Regexp for skipping continued blank or comment-only lines in structures.")

(defvar idlwave-struct-tag-regexp
  (concat "[{,]" ;leading comma/brace
	  idlwave-struct-skip ; 4 groups
	  "\\([a-zA-Z][a-zA-Z0-9_]*\\)" ;the tag itself, group 5
	  "[ \t]*:") ; the final colon
  "Regexp for structure tags.")

(defun idlwave-struct-tags ()
  "Return a list of all tags in the structure defined at point.
Point is expected just before the opening `{' of the struct definition."
  (save-excursion
    (let* ((borders (idlwave-struct-borders))
	   (beg (car borders))
	   (end (cdr borders))
	   tags)
      (goto-char beg)
      (save-restriction
	(narrow-to-region beg end)
	(while (re-search-forward idlwave-struct-tag-regexp end t)
	  ;; Check if we are still on the top level of the structure.
	  (if (and (condition-case nil (progn (up-list -1) t) (error nil))
		   (= (point) beg))
	      (push (match-string-no-properties 5) tags))
	  (goto-char (match-end 0))))
	(nreverse tags))))

(defun idlwave-find-struct-tag (tag)
  "Find a given TAG in the structure defined at point."
  (let* ((borders (idlwave-struct-borders))
	 (end (cdr borders))
	 (case-fold-search t))
    (re-search-forward (concat "\\(^[ \t]*\\|[,{][ \t]*\\)" tag "[ \t]*:")
		       end t)))

(defun idlwave-struct-inherits ()
  "Return a list of all `inherits' names in the struct at point.
Point is expected just before the opening `{' of the struct definition."
  (save-excursion
    (let* ((borders (idlwave-struct-borders))
	   (beg (car borders))
	   (end (cdr borders))
	   (case-fold-search t)
	   names)
      (goto-char beg)
      (save-restriction
	(narrow-to-region beg end)
	(while (re-search-forward
		(concat "[{,]"  ;leading comma/brace
			idlwave-struct-skip ; 4 groups
			"inherits"    ; The INHERITS tag
			idlwave-struct-skip ; 4 more
			"\\([a-zA-Z][a-zA-Z0-9_]*\\)") ; The super-group, #9
		end t)
	  ;; Check if we are still on the top level of the structure.
	  (if (and (condition-case nil (progn (up-list -1) t) (error nil))
		   (= (point) beg))
	      (push (match-string-no-properties 9) names))
	  (goto-char (match-end 0))))
      (nreverse names))))

(defun idlwave-in-structure ()
  "Return t if point is inside an IDL structure definition."
  (let ((beg (point)))
    (save-excursion
      (if (not (or (idlwave-in-comment) (idlwave-in-quote)))
	  (if (idlwave-find-structure-definition nil nil 'back)
	      (let ((borders (idlwave-struct-borders)))
		(or (= (car borders) (cdr borders)) ;; struct not yet closed...
		    (and (> beg (car borders)) (< beg (cdr borders))))))))))

(defun idlwave-struct-borders ()
  "Return the borders of the {...} after point as a cons cell."
  (let (beg)
    (save-excursion
      (skip-chars-forward "^{")
      (setq beg (point))
      (condition-case nil (forward-list 1)
	(error (goto-char beg)))
      (cons beg (point)))))

(defun idlwave-find-structure-definition (&optional var name bound)
  "Search forward for a structure definition.
If VAR is non-nil, search for a structure assigned to variable VAR.
If NAME is non-nil, search for a named structure NAME, if a string,
or a generic named structure otherwise.  If BOUND is an integer, limit
the search.  If BOUND is the symbol `all', we search first back and
then forward through the entire file.  If BOUND is the symbol `back'
we search only backward."
  (let* ((ws "[ \t]*\\(\\$.*\n[ \t]*\\)*")
	 (case-fold-search t)
	 (lim (if (integerp bound) bound nil))
	 (re (concat
	      (if var
		  (concat "\\<" (regexp-quote (downcase var)) "\\>" ws)
		"\\(\\)")
	      "=" ws "\\({\\)"
	      (if name
		  (if (stringp name)
		      (concat ws "\\(\\<" (downcase name) "\\)[^a-zA-Z0-9_$]")
		    ;; Just a generic name
		    (concat ws "\\<\\([a-zA-Z_0-9$]+\\)" ws ","))
		""))))
    (if (or (and (or (eq bound 'all) (eq bound 'back))
		 (re-search-backward re nil t))
	    (and (not (eq bound 'back)) (re-search-forward re lim t)))
	(progn
	  (goto-char (match-beginning 3))
	  (match-string-no-properties 5)))))

(defvar idlwave-class-info nil)
(defvar idlwave-class-reset nil) ; to reset buffer-local classes

(add-hook 'idlwave-update-rinfo-hook
	  (lambda () (setq idlwave-class-reset t)))
(add-hook 'idlwave-after-load-rinfo-hook
	  (lambda () (setq idlwave-class-info nil)))

(defun idlwave-class-info (class)
  (let (list entry)
    (if idlwave-class-info
	(if idlwave-class-reset
	    (setq
	     idlwave-class-reset nil
	     idlwave-class-info ; Remove any visited in a buffer
	     (delq nil (mapcar
			(lambda (x)
			  (let ((filebuf
				 (idlwave-class-file-or-buffer
				  (or (cdr (assq 'found-in x)) (car x)))))
			    (if (cdr filebuf)
				nil
			      x)))
			idlwave-class-info))))
      ;; Info is nil, put in the system stuff to start.
      (setq idlwave-class-info idlwave-system-class-info)
      (setq list idlwave-class-info)
      (while (setq entry (pop list))
	(idlwave-sintern-class-info entry)))
    (setq class (idlwave-sintern-class class))
    (or (assq class idlwave-class-info)
	(progn (idlwave-scan-class-info class)
	       (assq class idlwave-class-info)))))

(defun idlwave-sintern-class-info (entry)
  "Sintern the class names in a class-info entry."
  (let ((inherits (assq 'inherits entry)))
    (setcar entry (idlwave-sintern-class (car entry) 'set))
    (if inherits
	(setcdr inherits (mapcar (lambda (x) (idlwave-sintern-class x 'set))
				 (cdr inherits))))))

(defun idlwave-find-class-definition (class &optional all-hook alt-class)
  "Find class structure definition(s).
If ALL-HOOK is set, find all named structure definitions in a given
class__define routine, on which ALL-HOOK will be run.  If ALT-CLASS is
set, look for the name__define pro, and inside of it, for the ALT-CLASS
class/struct definition."
  (let ((case-fold-search t) end-lim name)
    (when (re-search-forward
	   (concat "^[ \t]*pro[ \t]+" (downcase class) "__define" "\\>") nil t)
      (if all-hook
	  (progn
	    ;; For everything there
	    (setq end-lim (save-excursion (idlwave-end-of-subprogram) (point)))
	    (while (setq name
			 (idlwave-find-structure-definition nil t end-lim))
	      (funcall all-hook name)))
	(idlwave-find-structure-definition nil (or alt-class class))))))


(defun idlwave-class-file-or-buffer (class)
  "Find buffer visiting CLASS definition."
  (let* ((pro (concat (downcase class) "__define"))
	 (file (idlwave-routine-source-file
		(nth 3 (idlwave-rinfo-assoc pro 'pro nil
					    (idlwave-routines))))))
    (cons file (if file (idlwave-get-buffer-visiting file)))))


(defun idlwave-scan-class-info (class)
  "Scan all class and named structure info in the class__define pro."
  (let* ((idlwave-auto-routine-info-updates nil)
	 (filebuf (idlwave-class-file-or-buffer class))
	 (file (car filebuf))
	 (buf (cdr filebuf))
	 (class (idlwave-sintern-class class)))
    (if (or
	 (not file)
	 (and ;; neither a regular file nor a visited buffer
	  (not buf)
	  (not (file-regular-p file))))
	nil ; Cannot find the file/buffer to get any info
      (save-excursion
	(if buf (set-buffer buf)
	  ;; Read the file in temporarily
	  (set-buffer (get-buffer-create " *IDLWAVE-tmp*"))
	  (erase-buffer)
	  (unless (derived-mode-p 'idlwave-mode)
	    (idlwave-mode))
	  (insert-file-contents file))
	(save-excursion
	  (goto-char 1)
	  (idlwave-find-class-definition class
	   ;; Scan all of the structures found there
	   (lambda (name)
	     (let* ((this-class (idlwave-sintern-class name))
		    (entry
		     (list this-class
			   (cons 'tags (idlwave-struct-tags))
			   (cons 'inherits (idlwave-struct-inherits)))))
	       (if (not (eq this-class class))
		   (setq entry (nconc entry (list (cons 'found-in class)))))
	       (idlwave-sintern-class-info entry)
	       (push entry idlwave-class-info)))))))))

(defun idlwave-class-found-in (class)
  "Return the FOUND-IN property of the CLASS."
  (cdr (assq 'found-in (idlwave-class-info class))))
(defun idlwave-class-tags (class)
  "Return the native tags in CLASS."
  (cdr (assq 'tags (idlwave-class-info class))))
(defun idlwave-class-inherits (class)
  "Return the direct superclasses of CLASS."
  (cdr (assq 'inherits (idlwave-class-info class))))


(defun idlwave-all-class-tags (class)
  "Return a list of native and inherited tags in CLASS."
  (condition-case err
      (apply 'append (mapcar 'idlwave-class-tags
			     (cons class (idlwave-all-class-inherits class))))
    (error
     (idlwave-class-tag-reset)
     (error "%s" (error-message-string err)))))


(defun idlwave-all-class-inherits (class)
  "Return a list of all superclasses of CLASS (recursively expanded).
The list is cached in `idlwave-class-info' for faster access."
  (cond
   ((not idlwave-support-inheritance) nil)
   ((eq class nil) nil)
   ((eq class t) nil)
   (t
    (let ((info (idlwave-class-info class))
	  entry)
      (if (setq entry (assq 'all-inherits info))
	  (cdr entry)
	;; Save the depth of inheritance scan to check for circular references
	(let ((inherits (mapcar (lambda (x) (cons x 0))
				(idlwave-class-inherits class)))
	      rtn all-inherits cl)
	  (while inherits
	    (setq cl (pop inherits)
		  rtn (cons (car cl) rtn)
		  inherits (append (mapcar (lambda (x)
					     (cons x (1+ (cdr cl))))
					   (idlwave-class-inherits (car cl)))
				   inherits))
	    (if (> (cdr cl) 999)
	      (error
	       "Class scan: inheritance depth exceeded. Circular inheritance?")
	      ))
	  (setq all-inherits (nreverse rtn))
	  (nconc info (list (cons 'all-inherits all-inherits)))
	  all-inherits))))))

(defun idlwave-entry-keywords (entry &optional record-link)
  "Return the flat entry keywords alist from routine-info entry.
If RECORD-LINK is non-nil, the keyword text is copied and a text
property indicating the link is added."
  (let (kwds)
    (mapc
     (lambda (key-list)
       (let ((file (car key-list)))
	 (mapcar (lambda (key-cons)
		   (let ((key (car key-cons))
			 (link (cdr key-cons)))
		     (when (and record-link file)
			 (setq key (copy-sequence key))
			 (put-text-property
			  0 (length key)
			  'link
			  (concat
			   file
			   (if link
			       (concat idlwave-html-link-sep
				       (number-to-string link))))
			  key))
		     (push (list key) kwds)))
		 (cdr key-list))))
     (nthcdr 5 entry))
    (nreverse kwds)))

(defun idlwave-entry-find-keyword (entry keyword)
  "Find keyword KEYWORD in entry ENTRY, and return (with link) if set."
  (catch 'exit
    (mapc
     (lambda (key-list)
       (let ((file (car key-list))
	     (kwd (assoc keyword (cdr key-list))))
	 (when kwd
	   (setq kwd (cons (car kwd)
			   (if (and file (cdr kwd))
			       (concat file
				       idlwave-html-link-sep
				       (number-to-string (cdr kwd)))
			     (cdr kwd))))
	   (throw 'exit kwd))))
     (nthcdr 5 entry))))

;;==========================================================================
;;
;; Completing class structure tags.  This is a completion plugin.
;; The necessary taglist is constructed dynamically

(defvar idlwave-current-tags-class nil)
(defvar idlwave-current-class-tags nil)
(defvar idlwave-current-native-class-tags nil)
(defvar idlwave-sint-class-tags nil)
(declare-function idlwave-sintern-class-tag "idlwave" t t)
(idlwave-new-sintern-type 'class-tag)
(add-to-list 'idlwave-complete-special 'idlwave-complete-class-structure-tag)
(add-hook 'idlwave-update-rinfo-hook 'idlwave-class-tag-reset)

(defun idlwave-complete-class-structure-tag ()
  "Complete a structure tag on a `self' argument in an object method."
  (interactive)
  (let ((pos (point))
	(case-fold-search t))
    (if (save-excursion
	  ;; Check if the context is right
	  (skip-chars-backward "a-zA-Z0-9._$")
	  (and (< (point) (- pos 4))
	       (looking-at "self\\.")))
	(let* ((class-selector (nth 2 (idlwave-current-routine)))
	       (super-classes (idlwave-all-class-inherits class-selector)))
	  ;; Check if we are in a class routine
	  (unless class-selector
	    (error "Not in a method procedure or function"))
	  ;; Check if we need to update the "current" class
	  (if (not (equal class-selector idlwave-current-tags-class))
	      (idlwave-prepare-class-tag-completion class-selector))
	  (setq idlwave-completion-help-info
		(list 'idlwave-complete-class-structure-tag-help
		      (idlwave-sintern-routine
		       (concat class-selector "__define"))
		      nil))
          ;; FIXME: idlwave-cpl-bold doesn't seem used anywhere.
	  (let  ((idlwave-cpl-bold idlwave-current-native-class-tags))
	    (idlwave-complete-in-buffer
	     'class-tag 'class-tag
	     idlwave-current-class-tags nil
	     (format "Select a tag of class %s" class-selector)
	     "class tag"
	     'idlwave-attach-class-tag-classes))
	  t) ; return t to skip other completions
      nil)))

(defun idlwave-class-tag-reset ()
  (setq idlwave-current-tags-class nil))

(defun idlwave-prepare-class-tag-completion (class)
  "Find and parse the necessary class definitions for class structure tags."
  (setq idlwave-sint-class-tags nil)
  (setq idlwave-current-tags-class class)
  (setq idlwave-current-class-tags
	(mapcar (lambda (x)
		  (list (idlwave-sintern-class-tag x 'set)))
		(idlwave-all-class-tags class)))
  (setq idlwave-current-native-class-tags
	(mapcar 'downcase (idlwave-class-tags class))))

;===========================================================================
;;
;; Completing system variables and their structure fields
;; This is also a plugin.

(defvar idlwave-sint-sysvars nil)
(defvar idlwave-sint-sysvartags nil)
(declare-function idlwave-sintern-sysvar    "idlwave" t t)
(declare-function idlwave-sintern-sysvartag "idlwave" t t)
(idlwave-new-sintern-type 'sysvar)
(idlwave-new-sintern-type 'sysvartag)
(add-to-list 'idlwave-complete-special 'idlwave-complete-sysvar-or-tag)
(add-hook 'idlwave-update-rinfo-hook 'idlwave-sysvars-reset)
(add-hook 'idlwave-after-load-rinfo-hook 'idlwave-sintern-sysvar-alist)


(defun idlwave-complete-sysvar-or-tag ()
  "Complete a system variable."
  (interactive)
  (let ((pos (point))
	(case-fold-search t))
    (cond ((save-excursion
	     ;; Check if the context is right for system variable
	     (skip-chars-backward "[a-zA-Z0-9_$]")
	     (equal (char-before) ?!))
	   (setq idlwave-completion-help-info '(idlwave-complete-sysvar-help))
	   (idlwave-complete-in-buffer 'sysvar 'sysvar
				       idlwave-system-variables-alist nil
				       "Select a system variable"
				       "system variable")
	   t)  ; return t to skip other completions
	  ((save-excursion
	     ;; Check if the context is right for sysvar tag
	     (skip-chars-backward "a-zA-Z0-9_$.")
	     (and (equal (char-before) ?!)
		  (looking-at "\\([a-zA-Z][a-zA-Z0-9_$]*\\)\\.")
		  (<= (match-end 0) pos)))
	   ;; Complete a system variable tag
	   (let* ((var (idlwave-sintern-sysvar (match-string 1)))
		  (entry (assq var idlwave-system-variables-alist))
		  (tags (cdr (assq 'tags entry))))
	     (or entry (error "!%s is not a known system variable" var))
	     (or tags (error "System variable !%s is not a structure" var))
	     (setq idlwave-completion-help-info
		   (list 'idlwave-complete-sysvar-tag-help var))
	     (idlwave-complete-in-buffer 'sysvartag 'sysvartag
					 tags nil
					 "Select a system variable tag"
					 "system variable tag")
	     t)) ; return t to skip other completions
	  (t nil))))

(defvar idlw-help-link) ;dynamic variables set by help callback
(defun idlwave-complete-sysvar-help (mode word)
  (let ((word (or (nth 1 idlwave-completion-help-info) word))
	(entry (assoc word idlwave-system-variables-alist)))
    (cond
     ((eq mode 'test)
      (and (stringp word) entry (nth 1 (assq 'link entry))))
     ((eq mode 'set)
      ;; Setting dynamic!!!
      (if entry (setq idlw-help-link (nth 1 (assq 'link entry)))))
     (t (error "This should not happen")))))

(defun idlwave-complete-sysvar-tag-help (mode word)
  (let* ((var (nth 1 idlwave-completion-help-info))
	(entry (assoc var idlwave-system-variables-alist))
	(tags (cdr (assq 'tags entry)))
	(main (nth 1 (assq 'link entry)))
	target)
    (cond
     ((eq mode 'test) ; we can at least link the main
      (and (stringp word) entry main))
     ((eq mode 'set)
      (if entry
	  (setq idlw-help-link
		(if (setq target (cdr (assoc-string word tags t)))
		    (idlwave-substitute-link-target main target)
		  main)))) ;; setting dynamic!!!
     (t (error "This should not happen")))))

(defun idlwave-split-link-target (link)
  "Split a given LINK into link file and anchor."
  (if (string-match idlwave-html-link-sep link)
      (cons (substring link 0 (match-beginning 0))
	    (string-to-number (substring link (match-end 0))))))

(defun idlwave-substitute-link-target (link target)
  "Substitute the TARGET anchor for the given LINK."
  (let (main-base)
    (setq main-base (if (string-match "#" link)
			(substring link 0 (match-beginning 0))
		      link))
    (if target
	(concat main-base idlwave-html-link-sep (number-to-string target))
      link)))

;; Fake help in the source buffer for class structure tags.
;; IDLW-HELP-LINK AND IDLW-HELP-NAME ARE GLOBAL-VARIABLES HERE.
;; (from idlwave-do-mouse-completion-help)
(defvar idlw-help-name)
(defvar idlw-help-link)
(defvar idlwave-help-do-class-struct-tag nil)
(defun idlwave-complete-class-structure-tag-help (mode word)
  (cond
   ((eq mode 'test) ; nothing gets fontified for class tags
    nil)
   ((eq mode 'set)
    (let (class-with found-in)
      (when (setq class-with
		(idlwave-class-or-superclass-with-tag
		 idlwave-current-tags-class
		 word))
	(if (assq (idlwave-sintern-class class-with)
		  idlwave-system-class-info)
	    (error "No help available for system class tags"))
	(if (setq found-in (idlwave-class-found-in class-with))
	    (setq idlw-help-name (cons (concat found-in "__define") class-with))
	  (setq idlw-help-name (concat class-with "__define")))))
    (setq idlw-help-link word
	  idlwave-help-do-class-struct-tag t))
   (t (error "This should not happen"))))

(defun idlwave-class-or-superclass-with-tag (class tag)
  "Find and return the CLASS or one of its superclass with the
associated TAG, if any."
  (let ((sclasses (cons class (idlwave-all-class-inherits class)))
	cl)
   (catch 'exit
     (while sclasses
       (setq cl (pop sclasses))
       (let ((tags (idlwave-class-tags cl)))
	 (while tags
	   (if (eq t (compare-strings tag 0 nil (car tags) 0 nil t))
	     (throw 'exit cl))
	   (setq tags (cdr tags))))))))


(defun idlwave-sysvars-reset ()
  (if (and (fboundp 'idlwave-shell-is-running)
	   (idlwave-shell-is-running)
	   idlwave-idlwave_routine_info-compiled)
      (idlwave-shell-send-command "idlwave_get_sysvars"
				  'idlwave-process-sysvars 'hide)))

(defun idlwave-process-sysvars ()
  (idlwave-shell-filter-sysvars)
  (setq idlwave-sint-sysvars nil
	idlwave-sint-sysvartags nil)
  (idlwave-sintern-sysvar-alist))

(defun idlwave-sintern-sysvar-alist ()
  (let ((list idlwave-system-variables-alist) entry tags)
    (while (setq entry (pop list))
      (setcar entry (idlwave-sintern-sysvar (car entry) 'set))
      (setq tags (assq 'tags entry))
      (if tags
	  (setcdr tags
		  (mapcar (lambda (x)
			    (cons (idlwave-sintern-sysvartag (car x) 'set)
				  (cdr x)))
			  (cdr tags)))))))

(defvar idlwave-shell-command-output)
(defun idlwave-shell-filter-sysvars ()
  "Get any new system variables and tags."
  (let ((text idlwave-shell-command-output)
	(start 0)
	(old idlwave-system-variables-alist)
	var tags type name class link old-entry)
    (setq idlwave-system-variables-alist nil)
    (while (string-match "^IDLWAVE-SYSVAR: !\\([a-zA-Z0-9_$]+\\)\\( \\(.*\\)\\)?"
			 text start)
      (setq start (match-end 0)
	    var (match-string 1 text)
	    tags (if (match-end 3)
		     (idlwave-split-string (match-string 3 text))))
      ;; Maintain old links, if present
      (setq old-entry (assq (idlwave-sintern-sysvar var) old))
      (setq link (assq 'link old-entry))
      (setq idlwave-system-variables-alist
	    (cons (list var
			(cons
			 'tags
			 (mapcar (lambda (x)
				   (cons x
					 (cdr (assq
					       (idlwave-sintern-sysvartag x)
					       (cdr (assq 'tags old-entry))))))
				 tags)) link)
		  idlwave-system-variables-alist)))
    ;; Keep the old value if query was not successful
    (setq idlwave-system-variables-alist
	  (or idlwave-system-variables-alist old))))

(defun idlwave-completion-fontify-classes ()
  "Goto the *Completions* buffer and fontify the class info."
  (when (featurep 'font-lock)
    (with-current-buffer "*Completions*"
      (save-excursion
	(goto-char (point-min))
	(let ((buffer-read-only nil))
	  (while (re-search-forward "\\.*<[^>]+>" nil t)
	    (put-text-property (match-beginning 0) (match-end 0)
			       'face 'font-lock-string-face)))))))

(defun idlwave-uniquify (list)
  (let ((ht (make-hash-table :size (length list) :test 'equal)))
    (delq nil
	  (mapcar (lambda (x)
		    (unless (gethash x ht)
		      (puthash x t ht)
		      x))
		  list))))

(defun idlwave-after-successful-completion (type slash &optional verify)
  "Add `=' or `(' after successful completion of keyword and function.
Restore the pre-completion window configuration if possible."
  (cond
   ((eq type 'procedure)
    nil)
   ((eq type 'function)
    (cond
     ((equal idlwave-function-completion-adds-paren nil) nil)
     ((or (equal idlwave-function-completion-adds-paren t)
	  (equal idlwave-function-completion-adds-paren 1))
      (insert "("))
     ((equal idlwave-function-completion-adds-paren 2)
      (insert "()")
      (backward-char 1))
     (t nil)))
   ((eq type 'keyword)
    (if (and idlwave-keyword-completion-adds-equal
	     (not slash))
	(progn (insert "=") t)
      nil)))

  ;; Restore the pre-completion window configuration if this is safe.

  (if (or (eq verify 'force)                                    ; force
	  (and
	   (get-buffer-window "*Completions*")                  ; visible
	   (idlwave-local-value 'idlwave-completion-p
				"*Completions*")                ; cib-buffer
	   (eq (marker-buffer idlwave-completion-mark)
	       (current-buffer))                                ; buffer OK
	   (equal (marker-position idlwave-completion-mark)
		  verify)))                                     ; pos OK
      (idlwave-restore-wconf-after-completion))
  (move-marker idlwave-completion-mark nil)
  (setq idlwave-before-completion-wconf nil))

(defun idlwave-mouse-context-help (ev &optional arg)
  "Call `idlwave-context-help' on the clicked location."
  (interactive "eP")
  (mouse-set-point ev)
  (idlwave-context-help arg))

(defvar idlwave-last-context-help-pos nil)
(defun idlwave-context-help (&optional arg)
  "Display IDL Online Help on context.
If point is on a keyword, help for that keyword will be shown.  If
point is on a routine name or in the argument list of a routine, help
for that routine will be displayed.  Works for system routines and
keywords, it pulls up text help.  For other routines and keywords,
visits the source file, finding help in the header (if
`idlwave-help-source-try-header' is non-nil) or the routine definition
itself."
  (interactive "P")
  (idlwave-do-context-help arg))

(defun idlwave-mouse-completion-help (ev)
  "Display online help about the completion at point."
  (interactive "eP")
  ;; Restore last-command for next command, to make
  ;; scrolling/canceling of completions work.
  (setq this-command last-command)
  (idlwave-do-mouse-completion-help ev))

(defun idlwave-routine-info (&optional arg external)
  "Display a routines calling sequence and list of keywords.
When point is on the name a function or procedure, or in the argument
list of a function or procedure, this command displays a help buffer with
the information.  When called with prefix arg, enforce class query.

When point is on an object operator `->', display the class stored in
this arrow, if any (see `idlwave-store-inquired-class').  With a prefix
arg, the class property is cleared out."

  (interactive "P")
  (idlwave-routines)
  (if (string-match "->" (buffer-substring
			  (max (point-min) (1- (point)))
			  (min (+ 2 (point)) (point-max))))
      ;; Cursor is on an arrow
      (if (get-text-property (point) 'idlwave-class)
	  ;; arrow has class property
	  (if arg
	      ;; Remove property
	      (save-excursion
		(backward-char 1)
		(when (looking-at ".?\\(->\\)")
		  (remove-text-properties (match-beginning 1) (match-end 1)
					  '(idlwave-class nil face nil))
		  (message "Class property removed from arrow")))
	    ;; Echo class property
	    (message "Arrow has text property identifying object to be class %s"
		     (get-text-property (point) 'idlwave-class)))
	;; No property found
	(message "Arrow has no class text property"))

    ;; Not on an arrow...
    (let* ((idlwave-query-class nil)
	   (idlwave-force-class-query (equal arg '(4)))
	   (module (idlwave-what-module)))
      (if (car module)
	  (apply 'idlwave-display-calling-sequence
		 (idlwave-fix-module-if-obj_new module))
	(error "Don't know which calling sequence to show")))))

(defun idlwave-resolve (&optional arg)
  "Call RESOLVE_ROUTINE on the module name at point.
Like `idlwave-routine-info', this looks for a routine call at point.
After confirmation in the minibuffer, it will use the shell to issue
a RESOLVE call for this routine, to attempt to make it defined and its
routine info available for IDLWAVE.  If the routine is a method call,
both `class__method' and `class__define' will be tried.
With ARG, enforce query for the class of object methods."
  (interactive "P")
  (let* ((idlwave-query-class nil)
	 (idlwave-force-class-query (equal arg '(4)))
	 (module (idlwave-what-module))
	 (name (idlwave-make-full-name (nth 2 module) (car module)))
	 (type (if (eq (nth 1 module) 'pro) "pro" "function"))
	 (resolve (read-string "Resolve: " (format "%s %s" type name)))
	 (kwd "")
	 class)
    (if (string-match "\\(pro\\|function\\)[ \t]+\\(\\(.*\\)::\\)?\\(.*\\)"
		      resolve)
	(setq type (match-string 1 resolve)
	      class (if (match-beginning 2)
			(match-string 3 resolve)
		      nil)
	      name (match-string 4 resolve)))
    (if (string= (downcase type) "function")
	(setq kwd ",/is_function"))

    (cond
     ((null class)
      (idlwave-shell-send-command
       (format "resolve_routine,'%s'%s" (downcase name) kwd)
       'idlwave-update-routine-info
       nil t))
     (t
      (idlwave-shell-send-command
       (format "resolve_routine,'%s__define'%s" (downcase class) kwd)
       (list 'idlwave-shell-send-command
	     (format "resolve_routine,'%s__%s'%s"
		     (downcase class) (downcase name) kwd)
	     '(idlwave-update-routine-info)
	     nil t))))))

(defun idlwave-find-module-this-file ()
  (interactive)
  (idlwave-find-module '(4)))

(defun idlwave-find-module (&optional arg)
  "Find the source code of an IDL module.
Works for modules for which IDLWAVE has routine info available.
The function offers as default the module name `idlwave-routine-info'
would use.  With ARG limit to this buffer.  With two prefix ARG's
force class query for object methods."
  (interactive "P")
  (let* ((idlwave-query-class nil)
	 (idlwave-force-class-query (equal arg '(16)))
	 (this-buffer (equal arg '(4)))
	 (module (idlwave-fix-module-if-obj_new (idlwave-what-module)))
	 (default (if module
		      (concat (idlwave-make-full-name
			       (nth 2 module) (car module))
			      (if (eq (nth 1 module) 'pro) "<p>" "<f>"))
		    "none"))
	 (list
	  (idlwave-uniquify
	   (delq nil
		 (mapcar (lambda (x)
			   (if (eq 'system (car-safe (nth 3 x)))
			       ;; Take out system routines with no source.
			       nil
			     (list
			      (concat (idlwave-make-full-name
				       (nth 2 x) (car x))
				      (if (eq (nth 1 x) 'pro) "<p>" "<f>")))))
			 (if this-buffer
			     (idlwave-save-buffer-update)
			   (idlwave-routines))))))
	 (name (idlwave-completing-read
		(if (or (not this-buffer)
			(assoc default list))
		    (format "Module (Default %s): " default)
		  (format "Module in this file: "))
		list))
	 type class)
    (if (string-match "\\`\\s-*\\'" name)
	;; Nothing, use the default.
	(setq name default))
    (if (string-match "<[fp]>" name)
	(setq type (substring name -2 -1)
	      name (substring name 0 -3)))
    (if (string-match "\\(.*\\)::\\(.*\\)" name)
	(setq class (match-string 1 name)
	      name (match-string 2 name)))
    (setq name (idlwave-sintern-routine-or-method name class)
	  class (idlwave-sintern-class class)
	  type (cond ((equal type "f") 'fun)
		     ((equal type "p") 'pro)
		     (t t)))
    (idlwave-do-find-module name type class nil this-buffer)))

(defun idlwave-do-find-module (name type class
				    &optional force-source this-buffer)
  (let ((name1 (idlwave-make-full-name class name))
	source buf1 entry
	(buf (current-buffer))
	(pos (point))
	file name2)
    (setq entry (idlwave-best-rinfo-assq name type class (idlwave-routines)
					 'WITH-FILE)
	  source (or force-source (nth 3 entry))
	  name2 (if (nth 2 entry)
		    (idlwave-make-full-name (nth 2 entry) name)
		  name1))
    (if source
	(setq file (idlwave-routine-source-file source)))
    (unless file  ; Try to find it on the path.
      (setq file
	    (idlwave-expand-lib-file-name
	     (if class
		 (format "%s__define.pro" (downcase class))
	       (format "%s.pro" (downcase name))))))
    (cond
     ((or (null name) (equal name ""))
      (error "Abort"))
     ((eq (car source) 'system)
      (error "Source code for system routine %s is not available"
	     name2))
     ((or (not file) (not (file-regular-p file)))
      (error "Source code for routine %s is not available"
	     name2))
     (t
      (when (not this-buffer)
	(setq buf1
	      (idlwave-find-file-noselect file 'find))
	(pop-to-buffer buf1 t))
      (goto-char (point-max))
      (let ((case-fold-search t))
	(if (re-search-backward
	     (concat "^[ \t]*\\<"
		     (cond ((eq type 'fun) "function")
			   ((eq type 'pro) "pro")
			   (t "\\(pro\\|function\\)"))
		     "\\>[ \t]+"
		     (regexp-quote (downcase name2))
		     "[^a-zA-Z0-9_$]")
	     nil t)
	    (goto-char (match-beginning 0))
	  (pop-to-buffer buf)
	  (goto-char pos)
	  (error "Could not find routine %s" name2)))))))

(defun idlwave-what-module ()
  "Return a default module for stuff near point.
Used by `idlwave-routine-info' and `idlwave-find-module'."
  (idlwave-routines)
  (if (let ((case-fold-search t))
	(save-excursion
	  (idlwave-beginning-of-statement)
	  (looking-at "[ \t]*\\(pro\\|function\\)[ \t]+\\(\\([a-zA-Z0-9_$]+\\)::\\)?\\([a-zA-Z0-9$_]+\\)\\([, \t\n]\\|$\\)")))
      ;; This is a function or procedure definition statement
      ;; We return the defined routine as module.
      (list
       (idlwave-sintern-routine-or-method (match-string-no-properties 4)
					  (match-string-no-properties 2))
       (if (equal (downcase (match-string 1)) "pro") 'pro 'fun)
       (idlwave-sintern-class (match-string 3)))

    ;; Not a definition statement - analyze precise position.
    (let* ((where (idlwave-where))
	   (cw (nth 2 where))
	   (pro (car (nth 0 where)))
	   (func (car (nth 1 where)))
	   (this-word (idlwave-this-word "a-zA-Z0-9$_"))
	   (next-char (save-excursion (skip-chars-forward "a-zA-Z0-9$_")
				      (following-char)))
	   )
      (cond
       ((and (eq cw 'procedure)
	     (not (equal this-word "")))
	(setq this-word (idlwave-sintern-routine-or-method
			 this-word (nth 2 (nth 3 where))))
	(list this-word 'pro
	      (idlwave-determine-class
	       (cons this-word (cdr (nth 3 where)))
	       'pro)))
       ((and (eq cw 'function)
	     (not (equal this-word ""))
	     (or (eq next-char ?\()	; exclude arrays, vars.
		 (looking-at "[a-zA-Z0-9_]*[ \t]*(")))
	(setq this-word (idlwave-sintern-routine-or-method
			 this-word (nth 2 (nth 3 where))))
	(list this-word 'fun
	      (idlwave-determine-class
	       (cons this-word (cdr (nth 3 where)))
	       'fun)))
       ((and (memq cw '(function-keyword procedure-keyword))
	     (not (equal this-word ""))
	     (eq next-char ?\())	; A function!
	(setq this-word (idlwave-sintern-routine this-word))
	(list this-word 'fun nil))
       (func
	(list func 'fun (idlwave-determine-class (nth 1 where) 'fun)))
       (pro
	(list pro 'pro (idlwave-determine-class (nth 0 where) 'pro)))
       (t nil)))))

(defun idlwave-what-module-find-class ()
  "Call `idlwave-what-module' and find the inherited class if necessary."
  (let* ((module (idlwave-what-module))
	 (class (nth 2 module)))
    (if (and (= (length module) 3)
	     (stringp class))
	(list (car module)
	      (nth 1 module)
	      (apply 'idlwave-find-inherited-class module))
      module)))

(defun idlwave-find-inherited-class (name type class)
  "Find the class which defines TYPE NAME and is CLASS or inherited by CLASS."
  (let ((entry (idlwave-best-rinfo-assoc name type class (idlwave-routines))))
    (if entry
	(nth 2 entry)
      class)))

(defun idlwave-fix-module-if-obj_new (module)
  "Check if MODULE points to obj_new.
If yes, and if the cursor is in the keyword region, change to the
appropriate Init method."
  (let* ((name (car module))
	 (pos (point))
	 (case-fold-search t)
	 string)
    (if (and (stringp name)
	     (equal (downcase name) "obj_new")
	     (save-excursion
	       (idlwave-beginning-of-statement)
	       (setq string (buffer-substring (point) pos))
	       (string-match "obj_new([^'\"]*['\"]\\([a-zA-Z0-9_]+\\)"
			     string)))
	(let ((name "Init")
	      (class (match-string 1 string)))
	  (setq module (list (idlwave-sintern-method "Init")
			     'fun
			     (idlwave-sintern-class class)))))
    module))

(defun idlwave-fix-keywords (name type class keywords
				  &optional super-classes system)
  "Update a list of keywords.
Translate OBJ_NEW, adding all super-class keywords, or all keywords
from all classes if CLASS equals t.  If SYSTEM is non-nil, don't
demand _EXTRA in the keyword list."
  (let ((case-fold-search t))

    ;; If this is the OBJ_NEW function, try to figure out the class and use
    ;; the keywords from the corresponding INIT method.
    (if (and (equal (upcase name) "OBJ_NEW")
	     (derived-mode-p 'idlwave-mode 'idlwave-shell-mode))
	(let* ((bos (save-excursion (idlwave-beginning-of-statement) (point)))
	       (string (buffer-substring bos (point)))
	       (case-fold-search t)
	       class)
	  (and (string-match "obj_new([^'\"]*['\"]\\([a-zA-Z0-9_]+\\)"
			     string)
	       (setq class (idlwave-sintern-class (match-string 1 string)))
	       (setq idlwave-current-obj_new-class class)
	       (setq keywords
		     (append keywords
			     (idlwave-entry-keywords
			      (idlwave-rinfo-assq
			       (idlwave-sintern-method "INIT")
			       'fun
			       class
			       (idlwave-routines)) 'do-link))))))

    ;; If the class is `t', combine all keywords of all methods NAME
    (when (eq class t)
      (mapc (lambda (entry)
	      (and
	       (nth 2 entry)             ; non-nil class
	       (eq (nth 1 entry) type)   ; correct type
	       (setq keywords
		     (append keywords
			     (idlwave-entry-keywords entry 'do-link)))))
	    (idlwave-all-assq name (idlwave-routines)))
      (setq keywords (idlwave-uniquify keywords)))

    ;; If we have inheritance, add all keywords from superclasses, if
    ;; the user indicated that method in `idlwave-keyword-class-inheritance'
    (when (and
	   super-classes
	   idlwave-keyword-class-inheritance
	   (stringp class)
	   (or
	    system
	    (assq (idlwave-sintern-keyword "_extra") keywords)
	    (assq (idlwave-sintern-keyword "_ref_extra") keywords))
	   ;; Check if one of the keyword-class regexps matches the name
	   (let ((regexps idlwave-keyword-class-inheritance) re)
	     (catch 'exit
	       (while (setq re (pop regexps))
		 (if (string-match re name) (throw 'exit t))))))

      (loop for entry in (idlwave-routines) do
	    (and (nth 2 entry)                           ; non-nil class
		 (memq (nth 2 entry) super-classes)      ; an inherited class
		 (eq (nth 1 entry) type)                 ; correct type
		 (eq (car entry) name)                   ; correct name
		 (mapc (lambda (k) (add-to-list 'keywords k))
		       (idlwave-entry-keywords entry 'do-link))))
      (setq keywords (idlwave-uniquify keywords)))

    ;; Return the final list
    keywords))

(defun idlwave-expand-keyword (keyword module)
  "Expand KEYWORD to one of the valid keyword parameters of MODULE.
KEYWORD may be an exact match or an abbreviation of a keyword.
If the match is exact, KEYWORD itself is returned, even if there may be other
keywords of which KEYWORD is an abbreviation.  This is necessary because some
system routines have keywords which are prefixes of other keywords.
If KEYWORD is an abbreviation of several keywords, a list of all possible
completions is returned.
If the abbreviation was unique, the correct keyword is returned.
If it cannot be a keyword, the function return nil.
If we do not know about MODULE, just return KEYWORD literally."
  (let* ((name (car module))
	 (type (nth 1 module))
	 (class (nth 2 module))
	 (kwd (idlwave-sintern-keyword keyword))
	 (entry (idlwave-best-rinfo-assoc name type class (idlwave-routines)))
	 (kwd-alist (idlwave-entry-keywords entry))
	 (extra (or (assq (idlwave-sintern-keyword "_EXTRA") kwd-alist)
		    (assq (idlwave-sintern-keyword "_REF_EXTRA") kwd-alist)))
	 (completion-ignore-case t)
	 candidates)
    (cond ((assq kwd kwd-alist)
	   kwd)
	  ((setq candidates (all-completions kwd kwd-alist))
	   (if (= (length candidates) 1)
	       (car candidates)
	     candidates))
	  ((and entry extra)
	   ;; Inheritance may cause this keyword to be correct
	   keyword)
	  (entry
	   ;; We do know the function, which does not have the keyword.
	   nil)
	  (t
	   ;; We do not know the function, so this just might be a correct
	   ;; keyword - return it as it is.
	   keyword))))

(defvar idlwave-rinfo-mouse-map
  (let ((map (make-sparse-keymap)))
    (define-key map
      (if (featurep 'xemacs) [button2] [mouse-2])
      'idlwave-mouse-active-rinfo)
    (define-key map
      (if (featurep 'xemacs) [(shift button2)] [(shift mouse-2)])
      'idlwave-mouse-active-rinfo-shift)
    (define-key map
      (if (featurep 'xemacs) [button3] [mouse-3])
      'idlwave-mouse-active-rinfo-right)
    (define-key map " " 'idlwave-active-rinfo-space)
    (define-key map "q" 'idlwave-quit-help)
    map))

(defvar idlwave-rinfo-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'idlwave-quit-help)
    map))

(defvar idlwave-popup-source nil)
(defvar idlwave-rinfo-marker (make-marker))

(defun idlwave-quit-help ()
  (interactive)
  (let ((ri-window (get-buffer-window "*Help*"))
	(olh-window (get-buffer-window "*IDLWAVE Help*")))
    (when (and olh-window
	       (fboundp 'idlwave-help-quit))
      (select-window olh-window)
      (idlwave-help-quit))
    (when (window-live-p ri-window)
      (delete-window ri-window))))

(defun idlwave-display-calling-sequence (name type class
					      &optional initial-class)
  ;; Display the calling sequence of module NAME, type TYPE in class CLASS.
  (let* ((initial-class (or initial-class class))
	 (entry (or (idlwave-best-rinfo-assq name type class
					     (idlwave-routines))
		    (idlwave-rinfo-assq name type class
					idlwave-unresolved-routines)))
	 (name (or (car entry) name))
	 (class (or (nth 2 entry) class))
	 (superclasses (idlwave-all-class-inherits initial-class))
	 (twins (idlwave-routine-twins entry))
	 (dtwins (idlwave-study-twins twins))
	 (all dtwins)
	 (system (eq (car (nth 3 entry)) 'system))
	 (calling-seq (nth 4 entry))
	 (keywords (idlwave-entry-keywords entry 'do-link))
	 (html-file (car (nth 5 entry)))
	 (help-echo-kwd
	  "Button2: Insert KEYWORD (SHIFT=`/KEYWORD') | Button3: Online Help ")
	 (help-echo-use
	  "Button2/3: Online Help")
	 (help-echo-src
	  "Button2: Jump to source and back | Button3: Source in Help window.")
	 (help-echo-class
	  "Button2: Display info about same method in superclass")
	 (col 0)
	 (data (list name type class (current-buffer) nil initial-class))
	 (km-prop (if (featurep 'xemacs) 'keymap 'local-map))
	 (face 'idlwave-help-link)
	 beg props win cnt total)
    ;; Fix keywords, but don't add chained super-classes, since these
    ;; are shown separately for that super-class
    (setq keywords (idlwave-fix-keywords name type class keywords))
    (cond
     ((null entry)
      (error "No %s %s known %s" type name
	     (if initial-class (concat "in class " initial-class) "")))
     ((or (null name) (equal name ""))
      (error "No function or procedure call at point"))
     ((null calling-seq)
      (error "Calling sequence of %s %s not available" type name))
     (t
      (move-marker idlwave-rinfo-marker (point))
      (with-current-buffer (get-buffer-create "*Help*")
	(use-local-map idlwave-rinfo-map)
	(setq buffer-read-only nil)
	(erase-buffer)
	(set (make-local-variable 'idlwave-popup-source) nil)
	(set (make-local-variable 'idlwave-current-obj_new-class)
				  idlwave-current-obj_new-class)
	(when superclasses
	  (setq props (list 'mouse-face 'highlight
			    km-prop idlwave-rinfo-mouse-map
			    'help-echo help-echo-class
			    'data (cons 'class data)))
	  (let ((classes (cons initial-class superclasses)) c)
	    (insert "Classes: ")
	    (while (setq c (pop classes))
	      (insert " ")
	      (setq beg (point))
	      (insert c)
	      (if (equal (downcase c) (downcase class))
		  (add-text-properties beg (point) (list 'face 'bold))
		;; If Method exists in a different class link it
		(if (idlwave-rinfo-assq name type c (idlwave-routines))
		    (add-text-properties beg (point) props))))
	    (insert "\n")))
	(setq props (list 'mouse-face 'highlight
			  km-prop idlwave-rinfo-mouse-map
			  'help-echo help-echo-use
			  'data (cons 'usage data)))
	(if html-file (setq props (append (list 'face face 'link html-file)
					  props)))
	(insert "Usage:    ")
	(setq beg (point))
	(insert (if class
		    (format calling-seq class name class name class name)
		  (format calling-seq name name name name))
		"\n")
	(add-text-properties beg (point) props)

	(insert "Keywords:")
	(if (null keywords)
	    (insert " No keywords accepted.")
	  (setq col 9)
	  (mapc
	   (lambda (x)
	     (if (>= (+ col 1 (length (car x)))
		     (window-width))
		 (progn
		   (insert "\n         ")
		   (setq col 9)))
	     (insert " ")
	     (setq beg (point)
		   ;; Relevant keywords already have link property attached
		   props (list 'mouse-face 'highlight
			       km-prop idlwave-rinfo-mouse-map
			       'data (cons 'keyword data)
			       'help-echo help-echo-kwd
			       'keyword (car x)))
	     (if system (setq props (append (list 'face face) props)))
	     (insert (car x))
	     (add-text-properties beg (point) props)
	     (setq col (+ col 1 (length (car x)))))
	   keywords))

	(setq cnt 1 total (length all))
	;; Here entry is (key file (list of type-conses))
	(while (setq entry (pop all))
	  (setq props (list 'mouse-face 'highlight
			    km-prop idlwave-rinfo-mouse-map
			    'help-echo help-echo-src
			    'source (list (car (car (nth 2 entry))) ;type
					  (nth 1 entry)
					  nil
					  (cdr (car (nth 2 entry))))
			    'data (cons 'source data)))
	  (idlwave-insert-source-location
	   (format "\n%-8s  %s"
		   (if (equal cnt 1)
		       (if (> total 1) "Sources:" "Source:")
		     "")
		   (if (> total 1) "- " ""))
	   entry props)
	  (incf cnt)
	  (when (and all (> cnt idlwave-rinfo-max-source-lines))
	    ;; No more source lines, please
	    (insert (format
		     "\n          Source information truncated to %d entries."
		     idlwave-rinfo-max-source-lines))
	    (setq all nil)))
	(goto-char (point-min))
	(setq buffer-read-only t))
      (display-buffer "*Help*")
      (if (and (setq win (get-buffer-window "*Help*"))
	       idlwave-resize-routine-help-window)
	  (progn
	    (let ((ww (selected-window)))
	      (unwind-protect
		  (progn
		    (select-window win)
		    (enlarge-window (- (/ (frame-height) 2)
				       (window-height)))
		    (shrink-window-if-larger-than-buffer))
		(select-window ww)))))))))

(defun idlwave-insert-source-location (prefix entry &optional file-props)
  "Insert a source location into the routine info buffer.
Start line with PREFIX.  If a file name is inserted, add FILE-PROPS
to it."
  (let* ((key (car entry))
	 (file (nth 1 entry))
	 (types (nth 2 entry))
	 (shell-flag (assq 'compiled types))
	 (buffer-flag (assq 'buffer types))
	 (user-flag (assq 'user types))
	 (lib-flag (assq 'lib types))
	 (ndupl (or (and buffer-flag (idlwave-count-memq 'buffer types))
		    (and user-flag (idlwave-count-memq 'user types))
		    (and lib-flag (idlwave-count-memq 'lib types))
		    1))
	 (doflags t)
	 beg special)

    (insert prefix)

    (cond
     ((eq key 'system)
      (setq doflags nil)
      (insert "System    "))

     ((eq key 'builtin)
      (setq doflags nil)
      (insert "Builtin   "))

     ((and (not file) shell-flag)
      (insert "Unresolved"))

     ((null file)
      (insert "ERROR"))

     ((idlwave-syslib-p file)
      (if (string-match "obsolete" (file-name-directory file))
	  (insert "Obsolete  ")
	(insert "SystemLib ")))

     ;; New special syntax: taken directly from routine-info for
     ;; library catalog routines
     ((setq special (or (cdr lib-flag) (cdr user-flag)))
      (insert (format "%-10s" special)))

     ;; Old special syntax: a matching regexp
     ((setq special (idlwave-special-lib-test file))
      (insert (format "%-10s" special)))

     ;; Catch-all with file
     ((idlwave-lib-p file)      (insert "Library   "))

     ;; Sanity catch all
     (t                         (insert "Other     ")))

    (when doflags
      (insert (concat
	       "  ["
	       (if lib-flag "L" "-")
	       (if user-flag "C" "-")
	       (if shell-flag "S" "-")
	       (if buffer-flag "B" "-")
	       "] ")))
    (when (> ndupl 1)
      (setq beg (point))
      (insert (format "(%dx) " ndupl))
      (add-text-properties beg (point) (list 'face 'bold)))
    (when (and file (not (equal file "")))
      (setq beg (point))
      (insert (apply 'abbreviate-file-name
		     (if (featurep 'xemacs) (list file t) (list file))))
      (if file-props
	  (add-text-properties beg (point) file-props)))))

(defun idlwave-special-lib-test (file)
  "Check the path of FILE against the regexps which define special libs.
Return the name of the special lib if there is a match."
  (let ((alist idlwave-special-lib-alist)
	entry rtn)
    (cond
     ((stringp file)
      (while (setq entry (pop alist))
	(if (string-match (car entry) file)
	    (setq rtn (cdr entry)
		  alist nil)))
      rtn)
     (t nil))))

(defun idlwave-mouse-active-rinfo-right (ev)
  (interactive "e")
  (idlwave-mouse-active-rinfo ev 'right))

(defun idlwave-mouse-active-rinfo-shift (ev)
  (interactive "e")
  (idlwave-mouse-active-rinfo ev nil 'shift))

(defun idlwave-active-rinfo-space ()
  (interactive)
  (idlwave-mouse-active-rinfo nil 'right))

(defun idlwave-mouse-active-rinfo (ev &optional right shift)
  "Do the mouse actions in the routine info buffer.
Optional args RIGHT and SHIFT indicate, if mouse-3 was used, and if SHIFT
was pressed."
  (interactive "e")
  (if ev (mouse-set-point ev))
  (let (data id name type class buf bufwin source link keyword
	     word initial-class)
    (setq data (get-text-property (point) 'data)
	  source (get-text-property (point) 'source)
	  keyword (get-text-property (point) 'keyword)
	  link (get-text-property (point) 'link)
	  id (car data)
	  name (nth 1 data) type (nth 2 data) class (nth 3 data)
	  buf (nth 4 data)
	  initial-class (nth 6 data)
	  word (idlwave-this-word)
	  bufwin (get-buffer-window buf t))

    (cond ((eq id 'class) ; Switch class being displayed
	   (if (window-live-p bufwin) (select-window bufwin))
	   (idlwave-display-calling-sequence
	    (idlwave-sintern-method name)
	    type (idlwave-sintern-class word)
	    initial-class))
	  ((eq id 'usage) ; Online help on this routine
	   (idlwave-online-help link name type class))
	  ((eq id 'source) ; Source in help or buffer
	   (if right ; In help
	       (let ((idlwave-extra-help-function 'idlwave-help-with-source)
		     (idlwave-help-source-try-header nil)
		     ;; Fake idlwave-routines so help will find the right entry
		     (idlwave-routines
		      (list (list name type class source ""))))
		 (idlwave-help-get-special-help name type class nil))
	     ;; Otherwise just pop to the source
	     (setq idlwave-popup-source (not idlwave-popup-source))
	     (if idlwave-popup-source
		 (condition-case err
		     (idlwave-do-find-module name type class source)
		   (error
		    (setq idlwave-popup-source nil)
		    (if (window-live-p bufwin) (select-window bufwin))
		    (error (nth 1 err))))
	       (if bufwin
		   (select-window bufwin)
		 (pop-to-buffer buf))
	       (goto-char (marker-position idlwave-rinfo-marker)))))
	  ((eq id 'keyword)
	   (if right
	       (idlwave-online-help link name type class keyword)
	     (idlwave-rinfo-insert-keyword keyword buf shift))))))

(defun idlwave-rinfo-insert-keyword (keyword buffer &optional shift)
  "Insert KEYWORD in BUFFER.  Make sure buffer is displayed in a window."
  (let ((bwin (get-buffer-window buffer)))
    (if idlwave-complete-empty-string-as-lower-case
	(setq keyword (downcase keyword)))
    (if bwin
	(select-window bwin)
      (pop-to-buffer buffer)
      (setq bwin (get-buffer-window buffer)))
    (if (eq (preceding-char) ?/)
	(insert keyword)
      (unless (save-excursion
		(re-search-backward
		 "[(,][ \t]*\\(\\$[ \t]*\\(;.*\\)?\n\\)?[ \t]*\\="
		 (min (- (point) 100) (point-min)) t))
	(insert ", "))
      (if shift (insert "/"))
      (insert keyword)
      (if (and (not shift)
	       idlwave-keyword-completion-adds-equal)
	  (insert "=")))))

(defun idlwave-list-buffer-load-path-shadows (&optional arg)
  "List the load path shadows of all routines defined in current buffer."
  (interactive "P")
  (idlwave-routines)
  (if (derived-mode-p 'idlwave-mode)
      (idlwave-list-load-path-shadows
       nil (idlwave-update-current-buffer-info 'save-buffer)
       "in current buffer")
    (error "Current buffer is not in idlwave-mode")))

(defun idlwave-list-shell-load-path-shadows (&optional arg)
  "List the load path shadows of all routines compiled under the shell.
This is very useful for checking an IDL application.  Just compile the
application, do RESOLVE_ALL, and `C-c C-i' to compile all referenced
routines and update IDLWAVE internal info.  Then check for shadowing
with this command."
  (interactive "P")
  (cond
   ((or (not (fboundp 'idlwave-shell-is-running))
	(not (idlwave-shell-is-running)))
    (error "Shell is not running"))
   ((null idlwave-compiled-routines)
    (error "No compiled routines.  Maybe you need to update with `C-c C-i'"))
   (t
    (idlwave-list-load-path-shadows nil idlwave-compiled-routines
				    "in the shell"))))

(defun idlwave-list-all-load-path-shadows (&optional arg)
  "List the load path shadows of all routines known to IDLWAVE."
  (interactive "P")
  (idlwave-list-load-path-shadows nil nil "globally"))

(defvar idlwave-sort-prefer-buffer-info t
  "Internal variable used to influence `idlwave-routine-twin-compare'.")

(defun idlwave-list-load-path-shadows (arg &optional special-routines loc)
  "List the routines which are defined multiple times.
Search the information IDLWAVE has about IDL routines for multiple
definitions.
When SPECIAL-ROUTINES in non-nil, only look for shadows of these routines.

When IDL hits a routine call which is not defined, it will search on
the load path in order to find a definition.  The output of this command
can be used to detect possible name clashes during this process."
  (idlwave-routines)  ; Make sure everything is loaded.
  (unless (or idlwave-user-catalog-routines idlwave-library-catalog-routines)
    (or (y-or-n-p
	 "You don't have any user or library catalogs.  Continue anyway? ")
	(error "Abort")))
  (let* ((routines (append idlwave-system-routines
			   idlwave-compiled-routines
			   idlwave-library-catalog-routines
			   idlwave-user-catalog-routines
			   idlwave-buffer-routines
			   nil))
	 (km-prop (if (featurep 'xemacs) 'keymap 'local-map))
	 (keymap (make-sparse-keymap))
	 (props (list 'mouse-face 'highlight
		      km-prop keymap
		      'help-echo "Mouse2: Find source"))
	 (nroutines (length (or special-routines routines)))
	 (step (/ nroutines 100))
	 (n 0)
	 (cnt 0)
	 (idlwave-sort-prefer-buffer-info nil)
	 routine twins dtwins twin done props1 lroutines)

    (if special-routines
	;; Just looking for shadows of a few special routines
	(setq lroutines routines
	      routines special-routines))

    (message "Sorting routines...")
    (setq routines (sort routines
			 (lambda (a b)
			   (string< (downcase (idlwave-make-full-name
					       (nth 2 a) (car a)))
				    (downcase (idlwave-make-full-name
					       (nth 2 b) (car b)))))))
    (message "Sorting routines...done")

    (define-key keymap (if (featurep 'xemacs) [(button2)] [(mouse-2)])
      (lambda (ev)
	(interactive "e")
	(mouse-set-point ev)
	(apply 'idlwave-do-find-module
	       (get-text-property (point) 'find-args))))
    (define-key keymap [(return)]
      (lambda ()
	(interactive)
	(apply 'idlwave-do-find-module
	       (get-text-property (point) 'find-args))))
    (message "Compiling list...( 0%%)")
    (with-current-buffer (get-buffer-create "*Shadows*")
      (setq buffer-read-only nil)
      (erase-buffer)
      (while (setq routine (pop routines))
	(if (= (mod (setq n (1+ n)) step) 0)
	    (message "Compiling list...(%2d%%)" (/ (* n 100) nroutines)))

	;; Get a list of all twins
	(setq twins (idlwave-routine-twins routine (or lroutines routines)))
	(if (memq routine done)
	    (setq dtwins nil)
	  (setq dtwins (idlwave-study-twins twins)))
	;; Mark all twins as dealt with
	(setq done (append twins done))
	(when (or (> (length dtwins) 1)
		  (> (idlwave-count-memq 'lib (nth 2 (car dtwins))) 1)
		  (> (idlwave-count-memq 'user (nth 2 (car dtwins))) 1)
		  (> (idlwave-count-memq 'buffer (nth 2 (car dtwins))) 1))
	  (incf cnt)
	  (insert (format "\n%s%s"
			  (idlwave-make-full-name (nth 2 routine)
						  (car routine))
			  (if (eq (nth 1 routine) 'fun) "()" "")))
	  (while (setq twin (pop dtwins))
	    (setq props1 (append (list 'find-args
				       (list (nth 0 routine)
					     (nth 1 routine)
					     (nth 2 routine)))
				 props))
	    (idlwave-insert-source-location "\n   - " twin props1))))
      (goto-char (point-min))
      (setq buffer-read-only t))
    (setq loc (or loc ""))
    (if (> cnt 0)
	(progn
	  (display-buffer (get-buffer "*Shadows*"))
	  (message "%d case%s of shadowing found %s"
		   cnt (if (= cnt 1) "" "s") loc))
      (message "No shadowing conflicts found %s" loc))))

(defun idlwave-print-source (routine)
  (let* ((source (nth 3 routine))
	 (stype (car source))
	 (sfile (idlwave-routine-source-file source)))
    (if (idlwave-syslib-p sfile) (setq stype 'syslib))
    (if (and (eq stype 'compiled)
	     (or (not (stringp sfile))
		 (not (string-match "\\S-" sfile))))
	(setq stype 'unresolved))
    (princ (format "      %-10s %s\n"
		   stype
		   (if sfile sfile "No source code available")))))

(defun idlwave-routine-twins (entry &optional list)
  "Return all twin entries of ENTRY in LIST.
LIST defaults to `idlwave-routines'.
Twin entries are those which have the same name, type, and class.
ENTRY will also be returned, as the first item of this list."
  (let* ((name (car entry))
	 (type (nth 1 entry))
	 (class (nth 2 entry))
	 (candidates (idlwave-all-assq name (or list (idlwave-routines))))
	 twins candidate)
    (while (setq candidate (pop candidates))
      (if (and (not (eq candidate entry))
	       (eq type (nth 1 candidate))
	       (eq class (nth 2 candidate)))
	  (push candidate twins)))
    (if (setq candidate (idlwave-rinfo-assq name type class
					    idlwave-unresolved-routines))
	(push candidate twins))
    (cons entry (nreverse twins))))

(defun idlwave-study-twins (entries)
  "Return dangerous twins of first entry in ENTRIES.
Dangerous twins are routines with same name, but in different files on
the load path.  If a file is in the system library and has an entry in
the `idlwave-system-routines' list, we omit the latter as
non-dangerous because many IDL routines are implemented as library
routines, and may have been scanned."
  (let* ((entry (car entries))
	 (idlwave-twin-name (car entry))      ;
	 (type (nth 1 entry))    ; Must be bound for
	 (idlwave-twin-class (nth 2 entry)) ;  idlwave-routine-twin-compare
	 (cnt 0)
	 source type type-cons file alist syslibp key)
    (while (setq entry (pop entries))
      (incf cnt)
      (setq source (nth 3 entry)
	    type (car source)
	    type-cons (cons type (nth 3 source))
	    file (idlwave-routine-source-file source))

      ;; Make KEY to index entry properly
      (setq key (cond ((eq type 'system) type)
		      (file (file-truename file))
		      (t 'unresolved)))

      ;; Check for an entry in the system library
      (if (and file
	       (not syslibp)
	       (idlwave-syslib-p file))
	  (setq syslibp t))

      ;; If there's more than one matching entry for the same file, just
      ;; append the type-cons to the type list.
      (if (setq entry (assoc key alist))
	  (push type-cons (nth 2 entry))
	(push (list key file (list type-cons)) alist)))

    (setq alist (nreverse alist))

    (when syslibp
      ;; File is in system *library* - remove any 'system entry
      (setq alist (delq (assq 'system alist) alist)))

    ;; If 'system remains and we've scanned the syslib, it's a builtin
    ;; (rather than a !DIR/lib/.pro file bundled as source).
    (when (and (idlwave-syslib-scanned-p)
	       (setq entry (assoc 'system alist)))
      (setcar entry 'builtin))
    (sort alist 'idlwave-routine-twin-compare)))

;; FIXME: Dynamically scoped vars need to use the `idlwave-' prefix.
;; (defvar type)
(defmacro idlwave-xor (a b)
  `(and (or ,a ,b)
	(not (and ,a ,b))))

(defun idlwave-routine-entry-compare (a b)
  "Compare two routine info entries for sorting.
This is the general case.  It first compares class, names, and type.
If it turns out that A and B are twins (same name, class, and type),
calls another routine which compares twins on the basis of their file
names and path locations."
  (let ((name (car a)) (type (nth 1 a)) (class (nth 2 a)))
    (cond
     ((not (equal (idlwave-downcase-safe class)
		  (idlwave-downcase-safe (nth 2 b))))
      ;; Class decides
      (cond ((null (nth 2 b)) nil)
	    ((null class) t)
	    (t (string< (downcase class) (downcase (nth 2 b))))))
     ((not (equal (downcase name) (downcase (car b))))
      ;; Name decides
      (string< (downcase name) (downcase (car b))))
     ((not (eq type (nth 1 b)))
      ;; Type decides
      (< (if (eq type 'fun) 1 0) (if (eq (nth 1 b) 'fun) 1 0)))
     (t
      ;; A and B are twins - so the decision is more complicated.
      ;; Call twin-compare with the proper arguments.
      (idlwave-routine-entry-compare-twins a b)))))

(defun idlwave-routine-entry-compare-twins (a b)
  "Compare two routine entries, under the assumption that they are twins.
This basically calls `idlwave-routine-twin-compare' with the correct args."
  (let* ((idlwave-twin-name (car a))
	 (type (nth 1 a))
	 (idlwave-twin-class (nth 2 a)) ; used in idlwave-routine-twin-compare
	 (asrc (nth 3 a))
	 (atype (car asrc))
	 (bsrc (nth 3 b))
	 (btype (car bsrc))
	 (afile (idlwave-routine-source-file asrc))
	 (bfile (idlwave-routine-source-file bsrc)))
    (idlwave-routine-twin-compare
     (if (stringp afile)
	 (list (file-truename afile) afile (list atype))
       (list atype afile (list atype)))
     (if (stringp bfile)
	 (list (file-truename bfile) bfile (list btype))
       (list btype bfile (list btype))))))

;; Bound in idlwave-study-twins,idlwave-routine-entry-compare-twins.
(defvar idlwave-twin-class)
(defvar idlwave-twin-name)

(defun idlwave-routine-twin-compare (a b)
  "Compare two routine twin entries for sorting.
In here, A and B are not normal routine info entries, but special
lists (KEY FILENAME (TYPES...)).
This expects NAME TYPE IDLWAVE-TWIN-CLASS to be bound to the right values."
  (let* (;; Dis-assemble entries
	 (akey (car a))	     (bkey (car b))
	 (afile (nth 1 a))   (bfile (nth 1 b))
	 (atypes (nth 2 a))  (btypes (nth 2 b))
	 ;; System routines?
	 (asysp (memq akey '(builtin system)))
	 (bsysp (memq bkey '(builtin system)))
	 ;; Compiled routines?
	 (acompp (memq 'compiled atypes))
	 (bcompp (memq 'compiled btypes))
	 ;; Unresolved?
	 (aunresp (or (eq akey 'unresolved)
		      (and acompp (not afile))))
	 (bunresp (or (eq bkey 'unresolved)
		      (and bcompp (not bfile))))
	 ;; Buffer info available?
	 (abufp (memq 'buffer atypes))
	 (bbufp (memq 'buffer btypes))
	 ;; On search path?
	 (tpath-alist (idlwave-true-path-alist))
	 (apathp (and (stringp akey)
		      (assoc (file-name-directory akey) tpath-alist)))
	 (bpathp (and (stringp bkey)
		      (assoc (file-name-directory bkey) tpath-alist)))
	 ;; How early on search path?  High number means early since we
	 ;; measure the tail of the path list
	 (anpath (length (memq apathp tpath-alist)))
	 (bnpath (length (memq bpathp tpath-alist)))
	 ;; Look at file names
	 (aname (if (stringp afile) (downcase (file-name-nondirectory afile)) ""))
	 (bname (if (stringp bfile) (downcase (file-name-nondirectory bfile)) ""))
	 (fname-re (if idlwave-twin-class
		       (format "\\`%s__\\(%s\\|define\\)\\.pro\\'"
			       (regexp-quote (downcase idlwave-twin-class))
			       (regexp-quote (downcase idlwave-twin-name)))
		     (format "\\`%s\\.pro" (regexp-quote (downcase idlwave-twin-name)))))
	 ;; Is file name derived from the routine name?
	 ;; Method file or class definition file?
	 (anamep (string-match fname-re aname))
	 (adefp (and idlwave-twin-class anamep
		     (string= "define" (match-string 1 aname))))
	 (bnamep (string-match fname-re bname))
	 (bdefp (and idlwave-twin-class bnamep
		     (string= "define" (match-string 1 bname)))))

    ;; Now: follow JD's ideas about sorting.  Looks really simple now,
    ;; doesn't it?  The difficult stuff is hidden above...
    (cond
     ((idlwave-xor asysp  bsysp)       asysp)	; System entries first
     ((idlwave-xor aunresp bunresp)    bunresp) ; Unresolved last
     ((and idlwave-sort-prefer-buffer-info
	   (idlwave-xor abufp bbufp))  abufp)	; Buffers before non-buffers
     ((idlwave-xor acompp bcompp)      acompp)	; Compiled entries
     ((idlwave-xor apathp bpathp)      apathp)	; Library before non-library
     ((idlwave-xor anamep bnamep)      anamep)	; Correct file names first
     ((and idlwave-twin-class anamep bnamep     ; both file names match ->
	   (idlwave-xor adefp bdefp))  bdefp)	; __define after __method
     ((> anpath bnpath)                t)	; Who is first on path?
     (t                                nil))))	; Default

(defun idlwave-routine-source-file (source)
  (if (nth 2 source)
      (expand-file-name (nth 1 source) (nth 2 source))
    (nth 1 source)))

(defun idlwave-downcase-safe (string)
  "Downcase if string, else return unchanged."
  (if (stringp string)
      (downcase string)
    string))

(defun idlwave-count-eq (elt list)
  "How often is ELT in LIST?"
  (length (delq nil (mapcar (lambda (x) (eq x elt)) list))))

(defun idlwave-count-memq (elt alist)
  "How often is ELT a key in ALIST?"
  (length (delq nil (mapcar (lambda (x) (eq (car x) elt)) alist))))

(defun idlwave-syslib-p (file)
  "Non-nil if FILE is in the system library."
  (let* ((true-syslib (file-name-as-directory
		       (file-truename
			(expand-file-name "lib" (idlwave-sys-dir)))))
	 (true-file (file-truename file)))
    (string-match (concat "^" (regexp-quote true-syslib)) true-file)))

(defun idlwave-lib-p (file)
  "Non-nil if FILE is in the library."
  (let ((true-dir (file-name-directory (file-truename file))))
    (assoc true-dir (idlwave-true-path-alist))))

(defun idlwave-path-alist-add-flag (list-entry flag)
  "Add a flag to the path list entry, if not set."
  (let ((flags (cdr list-entry)))
    (add-to-list 'flags flag)
    (setcdr list-entry flags)))

(defun idlwave-path-alist-remove-flag (list-entry flag)
  "Remove a flag to the path list entry, if set."
  (let ((flags (delq flag (cdr list-entry))))
    (setcdr list-entry flags)))

(defun idlwave-true-path-alist ()
  "Return `idlwave-path-alist' alist with true-names.
Info is cached, but relies on the functions setting `idlwave-path-alist'
to reset the variable `idlwave-true-path-alist' to nil."
  (or idlwave-true-path-alist
      (setq idlwave-true-path-alist
	    (mapcar (lambda(x) (cons
				(file-name-as-directory
				 (file-truename
				  (directory-file-name
				   (car x))))
				(cdr x)))
		    idlwave-path-alist))))

(defun idlwave-syslib-scanned-p ()
  "Non-nil if the system lib file !DIR/lib has been scanned."
  (let* ((true-syslib (file-name-as-directory
		       (file-truename
			(expand-file-name "lib" (idlwave-sys-dir))))))
    (cdr (assoc true-syslib (idlwave-true-path-alist)))))

;; ----------------------------------------------------------------------------
;;
;; Online Help display


;; ----------------------------------------------------------------------------
;;
;; Additions for use with imenu.el and func-menu.el
;; (pop-up a list of IDL units in the current file).
;;

(defun idlwave-prev-index-position ()
  "Search for the previous procedure or function.
Return nil if not found.  For use with imenu.el."
  (save-match-data
    (cond
     ((idlwave-find-key "\\<\\(pro\\|function\\)\\>" -1 'nomark))
     ;;   ((idlwave-find-key idlwave-begin-unit-reg 1 'nomark)
     (t nil))))

(defun idlwave-unit-name ()
  "Return the unit name.
Assumes that point is at the beginning of the unit as found by
`idlwave-prev-index-position'."
  (forward-sexp 2)
  (forward-sexp -1)
  (let ((begin (point)))
    (re-search-forward
     "[a-zA-Z_][a-zA-Z0-9$_]+\\(::[a-zA-Z_][a-zA-Z0-9$_]+\\)?")
    (if (fboundp 'buffer-substring-no-properties)
        (buffer-substring-no-properties begin (point))
      (buffer-substring begin (point)))))

(defalias 'idlwave-function-menu
  (condition-case nil
      (progn
	(require 'func-menu)
	'function-menu)
    (error (condition-case nil
	       (progn
		 (require 'imenu)
		 'imenu)
	     (error nil)))))

;; Here we hack func-menu.el in order to support this new mode.
;; The latest versions of func-menu.el already have this stuff in, so
;; we hack only if it is not already there.
(when (fboundp 'eval-after-load)
  (eval-after-load "func-menu"
    '(progn
       (or (assq 'idlwave-mode fume-function-name-regexp-alist)
	   (not (boundp 'fume-function-name-regexp-idl))      ; avoid problems
	   (setq fume-function-name-regexp-alist
		 (cons '(idlwave-mode . fume-function-name-regexp-idl)
		       fume-function-name-regexp-alist)))
       (or (assq 'idlwave-mode fume-find-function-name-method-alist)
	   (not (fboundp 'fume-find-next-idl-function-name))  ; avoid problems
	   (setq fume-find-function-name-method-alist
		 (cons '(idlwave-mode . fume-find-next-idl-function-name)
		       fume-find-function-name-method-alist))))))

(defun idlwave-edit-in-idlde ()
  "Edit the current file in IDL Development environment."
  (interactive)
  (start-process "idldeclient" nil
		 idlwave-shell-explicit-file-name "-c" "-e"
                 (buffer-file-name)))

(defvar idlwave-help-use-assistant)
(defun idlwave-launch-idlhelp ()
  "Start the IDLhelp application."
  (interactive)
  (if idlwave-help-use-assistant
      (idlwave-help-assistant-raise)
    (start-process "idlhelp" nil idlwave-help-application)))

;; Menus - using easymenu.el
(defvar idlwave-mode-menu-def
  `("IDLWAVE"
    ["PRO/FUNC menu" idlwave-function-menu t]
    ("Motion"
     ["Subprogram Start" idlwave-beginning-of-subprogram t]
     ["Subprogram End" idlwave-end-of-subprogram t]
     ["Block Start" idlwave-beginning-of-block t]
     ["Block End" idlwave-end-of-block t]
     ["Up Block" idlwave-backward-up-block t]
     ["Down Block" idlwave-down-block t]
     ["Skip Block Backward" idlwave-backward-block t]
     ["Skip Block Forward" idlwave-forward-block t])
    ("Mark"
     ["Subprogram" idlwave-mark-subprogram t]
     ["Block" idlwave-mark-block t]
     ["Header" idlwave-mark-doclib t])
    ("Format"
     ["Indent Entire Statement" idlwave-indent-statement
      :active t :keys "C-u \\[indent-for-tab-command]" ]
     ["Indent Subprogram" idlwave-indent-subprogram t]
     ["(Un)Comment Region" idlwave-toggle-comment-region t]
     ["Continue/Split line" idlwave-split-line t]
     "--"
     ["Toggle Auto Fill" idlwave-auto-fill-mode :style toggle
      :selected (symbol-value idlwave-fill-function)])
    ("Templates"
     ["Procedure" idlwave-procedure t]
     ["Function" idlwave-function t]
     ["Doc Header" idlwave-doc-header t]
     ["Log" idlwave-doc-modification t]
     "--"
     ["Case" idlwave-case t]
     ["For" idlwave-for t]
     ["Repeat" idlwave-repeat t]
     ["While" idlwave-while t]
     "--"
     ["Close Block" idlwave-close-block t])
    ("Completion"
     ["Complete" idlwave-complete t]
     ("Complete Specific"
      ["1 Procedure Name" (idlwave-complete 'procedure) t]
      ["2 Procedure Keyword" (idlwave-complete 'procedure-keyword) t]
      "--"
      ["3 Function Name" (idlwave-complete 'function) t]
      ["4 Function Keyword" (idlwave-complete 'function-keyword) t]
      "--"
      ["5 Procedure Method Name" (idlwave-complete 'procedure-method) t]
      ["6 Procedure Method Keyword" (idlwave-complete 'procedure-method-keyword) t]
      "--"
      ["7 Function Method Name" (idlwave-complete 'function-method) t]
      ["8 Function Method Keyword" (idlwave-complete 'function-method-keyword) t]
      "--"
      ["9 Class Name"  idlwave-complete-class t]))
    ("Routine Info"
     ["Show Routine Info" idlwave-routine-info t]
     ["Online Context Help" idlwave-context-help t]
     "--"
     ["Find Routine Source" idlwave-find-module t]
     ["Resolve Routine" idlwave-resolve (featurep 'idlw-shell)]
     "--"
     ["Update Routine Info" idlwave-update-routine-info t]
     ["Rescan XML Help Catalog" idlwave-convert-xml-system-routine-info t]
     "--"
     "IDL User Catalog"
     ["Select Catalog Directories" (idlwave-create-user-catalog-file nil) t]
     ["Scan Directories" (idlwave-update-routine-info '(16))
      (and idlwave-path-alist (not idlwave-catalog-process))]
     ["Scan Directories &" (idlwave-update-routine-info '(64))
      (and idlwave-path-alist (not idlwave-catalog-process))]
     "--"
     "Routine Shadows"
     ["Check Current Buffer" idlwave-list-buffer-load-path-shadows t]
     ["Check Compiled Routines" idlwave-list-shell-load-path-shadows t]
     ["Check Everything" idlwave-list-all-load-path-shadows t])
    ("Misc"
     ["Kill auto-created buffers" idlwave-kill-autoloaded-buffers t]
     "--"
     ["Insert TAB character" idlwave-hard-tab t])
     "--"
    ("External"
     ["Start IDL shell" idlwave-shell t]
     ["Edit file in IDLDE" idlwave-edit-in-idlde t]
     ["Launch IDL Help" idlwave-launch-idlhelp t])
    "--"
    ("Customize"
     ["Browse IDLWAVE Group" idlwave-customize t]
     "--"
     ["Build Full Customize Menu" idlwave-create-customize-menu
      (fboundp 'customize-menu-create)])
    ("Documentation"
     ["Describe Mode" describe-mode t]
     ["Abbreviation List" idlwave-list-abbrevs t]
     "--"
     ["Commentary in idlwave.el" idlwave-show-commentary t]
     ["Commentary in idlw-shell.el" idlwave-shell-show-commentary t]
     "--"
     ["Info" idlwave-info t]
     "--"
     ["Help with Topic" idlwave-help-assistant-help-with-topic
      idlwave-help-use-assistant]
     ["Launch IDL Help" idlwave-launch-idlhelp t])))

(defvar idlwave-mode-debug-menu-def
  '("Debug"
    ["Start IDL shell" idlwave-shell t]
    ["Save and .RUN buffer" idlwave-shell-save-and-run
     (and (boundp 'idlwave-shell-automatic-start)
	  idlwave-shell-automatic-start)]))

(if (or (featurep 'easymenu) (load "easymenu" t))
    (progn
      (easy-menu-define idlwave-mode-menu idlwave-mode-map
			"IDL and WAVE CL editing menu"
			idlwave-mode-menu-def)
      (easy-menu-define idlwave-mode-debug-menu idlwave-mode-map
			"IDL and WAVE CL editing menu"
			idlwave-mode-debug-menu-def)))

(defun idlwave-customize ()
  "Call the customize function with `idlwave' as argument."
  (interactive)
  ;; Try to load the code for the shell, so that we can customize it
  ;; as well.
  (or (featurep 'idlw-shell)
      (load "idlw-shell" t))
  (customize-browse 'idlwave))

(defun idlwave-create-customize-menu ()
  "Create a full customization menu for IDLWAVE, insert it into the menu."
  (interactive)
  (if (fboundp 'customize-menu-create)
      (progn
	;; Try to load the code for the shell, so that we can customize it
	;; as well.
	(or (featurep 'idlw-shell)
	    (load "idlw-shell" t))
	(easy-menu-change
	 '("IDLWAVE") "Customize"
	 `(["Browse IDLWAVE group" idlwave-customize t]
	   "--"
	   ,(customize-menu-create 'idlwave)
	   ["Set" Custom-set t]
	   ["Save" Custom-save t]
	   ["Reset to Current" Custom-reset-current t]
	   ["Reset to Saved" Custom-reset-saved t]
	   ["Reset to Standard Settings" Custom-reset-standard t]))
	(message "\"IDLWAVE\"-menu now contains full customization menu"))
    (error "Cannot expand menu (outdated version of cus-edit.el)")))

(defun idlwave-show-commentary ()
  "Use the finder to view the file documentation from `idlwave.el'."
  (interactive)
  (finder-commentary "idlwave.el"))

(defun idlwave-shell-show-commentary ()
  "Use the finder to view the file documentation from `idlw-shell.el'."
  (interactive)
  (finder-commentary "idlw-shell.el"))

(defun idlwave-info ()
  "Read documentation for IDLWAVE in the info system."
  (interactive)
  (info "idlwave"))

(defun idlwave-list-abbrevs (arg)
  "Show the code abbreviations define in IDLWAVE mode.
This lists all abbrevs where the replacement text differs from the input text.
These are the ones the users want to learn to speed up their writing.

The function does *not* list abbrevs which replace a word with itself
to call a hook.  These hooks are used to change the case of words or
to blink the matching `begin', and the user does not need to know them.

With arg, list all abbrevs with the corresponding hook.

This function was written since `list-abbrevs' looks terrible for IDLWAVE mode."

  (interactive "P")
  (let ((table (symbol-value 'idlwave-mode-abbrev-table))
	abbrevs
	str rpl func fmt (len-str 0) (len-rpl 0))
    (mapatoms
     (lambda (sym)
       (if (symbol-value sym)
	   (progn
	     (setq str (symbol-name sym)
		   rpl (symbol-value sym)
		   func (symbol-function sym))
	     (if arg
		 (setq func (prin1-to-string func))
	       (if (and (listp func) (stringp (nth 2 func)))
		   (setq rpl (concat "EVAL: " (nth 2 func))
			 func "")
		 (setq func "")))
	     (if (or arg (not (string= rpl str)))
		 (progn
		   (setq len-str (max len-str (length str)))
		   (setq len-rpl (max len-rpl (length rpl)))
		   (setq abbrevs (cons (list str rpl func) abbrevs)))))))
     table)
    ;; sort the list
    (setq abbrevs (sort abbrevs (lambda (a b) (string< (car a) (car b)))))
    ;; Make the format
    (setq fmt (format "%%-%ds   %%-%ds   %%s\n" len-str len-rpl))
    (with-output-to-temp-buffer "*Help*"
      (if arg
	  (progn
	    (princ "Abbreviations and Actions in IDLWAVE-Mode\n")
	    (princ "=========================================\n\n")
	    (princ (format fmt "KEY" "REPLACE" "HOOK"))
	    (princ (format fmt "---" "-------" "----")))
	(princ "Code Abbreviations and Templates in IDLWAVE-Mode\n")
	(princ "================================================\n\n")
	(princ (format fmt "KEY" "ACTION" ""))
	(princ (format fmt "---" "------" "")))
      (dolist (list abbrevs)
	(setq str (car list)
	      rpl (nth 1 list)
	      func (nth 2 list))
	(princ (format fmt str rpl func)))))
  ;; Make sure each abbreviation uses only one display line
  (with-current-buffer "*Help*"
    (setq truncate-lines t)))

;; Add .pro files to speedbar for support, if it's loaded
(eval-after-load "speedbar" '(speedbar-add-supported-extension ".pro"))

;; Set an idle timer to load the routine info.
;; Will only work on systems which support this.
(or idlwave-routines (idlwave-start-load-rinfo-timer))

;; Run the hook
(run-hooks 'idlwave-load-hook)

(provide 'idlwave)

;;; idlwave.el ends here
