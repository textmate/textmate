;;; org.el --- Outline-based notes management and organizer
;; Carstens outline-mode for keeping track of everything.
;; Copyright (C) 2004-2012  Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Maintainer: Bastien Guerry <bzg at gnu dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 7.8.11
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
;;
;; Org-mode is a mode for keeping notes, maintaining ToDo lists, and doing
;; project planning with a fast and effective plain-text system.
;;
;; Org-mode develops organizational tasks around NOTES files that contain
;; information about projects as plain text.  Org-mode is implemented on
;; top of outline-mode, which makes it possible to keep the content of
;; large files well structured.  Visibility cycling and structure editing
;; help to work with the tree.  Tables are easily created with a built-in
;; table editor.  Org-mode supports ToDo items, deadlines, time stamps,
;; and scheduling.  It dynamically compiles entries into an agenda that
;; utilizes and smoothly integrates much of the Emacs calendar and diary.
;; Plain text URL-like links connect to websites, emails, Usenet
;; messages, BBDB entries, and any files related to the projects.  For
;; printing and sharing of notes, an Org-mode file can be exported as a
;; structured ASCII file, as HTML, or (todo and agenda items only) as an
;; iCalendar file.  It can also serve as a publishing tool for a set of
;; linked webpages.
;;
;; Installation and Activation
;; ---------------------------
;; See the corresponding sections in the manual at
;;
;;   http://orgmode.org/org.html#Installation
;;
;; Documentation
;; -------------
;; The documentation of Org-mode can be found in the TeXInfo file.  The
;; distribution also contains a PDF version of it.  At the homepage of
;; Org-mode, you can read the same text online as HTML.  There is also an
;; excellent reference card made by Philip Rooke.  This card can be found
;; in the etc/ directory of Emacs 22.
;;
;; A list of recent changes can be found at
;; http://orgmode.org/Changes.html
;;
;;; Code:

(defvar org-inhibit-highlight-removal nil) ; dynamically scoped param
(defvar org-table-formula-constants-local nil
  "Local version of `org-table-formula-constants'.")
(make-variable-buffer-local 'org-table-formula-constants-local)

;;;; Require other packages

(eval-when-compile
  (require 'cl)
  (require 'gnus-sum))

(require 'calendar)
(require 'format-spec)

;; Emacs 22 calendar compatibility:  Make sure the new variables are available
(when (fboundp 'defvaralias)
  (unless (boundp 'calendar-view-holidays-initially-flag)
    (defvaralias 'calendar-view-holidays-initially-flag
      'view-calendar-holidays-initially))
  (unless (boundp 'calendar-view-diary-initially-flag)
    (defvaralias 'calendar-view-diary-initially-flag
      'view-diary-entries-initially))
  (unless (boundp 'diary-fancy-buffer)
    (defvaralias 'diary-fancy-buffer 'fancy-diary-buffer)))

(require 'outline) (require 'noutline)
;; Other stuff we need.
(require 'time-date)
(unless (fboundp 'time-subtract) (defalias 'time-subtract 'subtract-time))
(require 'easymenu)
(require 'overlay)

(require 'org-macs)
(require 'org-entities)
(require 'org-compat)
(require 'org-faces)
(require 'org-list)
(require 'org-pcomplete)
(require 'org-src)
(require 'org-footnote)

(declare-function org-inlinetask-at-task-p "org-inlinetask" ())
(declare-function org-inlinetask-outline-regexp "org-inlinetask" ())
(declare-function org-inlinetask-toggle-visibility "org-inlinetask" ())
(declare-function org-pop-to-buffer-same-window "org-compat" (&optional buffer-or-name norecord label))
(declare-function org-at-clock-log-p "org-clock" ())
(declare-function org-clock-timestamps-up "org-clock" ())
(declare-function org-clock-timestamps-down "org-clock" ())

;; babel
(require 'ob)
(require 'ob-table)
(require 'ob-lob)
(require 'ob-ref)
(require 'ob-tangle)
(require 'ob-comint)
(require 'ob-keys)

;; load languages based on value of `org-babel-load-languages'
(defvar org-babel-load-languages)
;;;###autoload
(defun org-babel-do-load-languages (sym value)
  "Load the languages defined in `org-babel-load-languages'."
  (set-default sym value)
  (mapc (lambda (pair)
	  (let ((active (cdr pair)) (lang (symbol-name (car pair))))
	    (if active
		(progn
		  (require (intern (concat "ob-" lang))))
	      (progn
		(funcall 'fmakunbound
			 (intern (concat "org-babel-execute:" lang)))
		(funcall 'fmakunbound
			 (intern (concat "org-babel-expand-body:" lang)))))))
	org-babel-load-languages))

(defcustom org-babel-load-languages '((emacs-lisp . t))
  "Languages which can be evaluated in Org-mode buffers.
This list can be used to load support for any of the languages
below, note that each language will depend on a different set of
system executables and/or Emacs modes.  When a language is
\"loaded\", then code blocks in that language can be evaluated
with `org-babel-execute-src-block' bound by default to C-c
C-c (note the `org-babel-no-eval-on-ctrl-c-ctrl-c' variable can
be set to remove code block evaluation from the C-c C-c
keybinding.  By default only Emacs Lisp (which has no
requirements) is loaded."
  :group 'org-babel
  :set 'org-babel-do-load-languages
  :version "24.1"
  :type '(alist :tag "Babel Languages"
		:key-type
		(choice
		 (const :tag "Awk" awk)
		 (const :tag "C" C)
		 (const :tag "R" R)
		 (const :tag "Asymptote" asymptote)
		 (const :tag "Calc" calc)
		 (const :tag "Clojure" clojure)
		 (const :tag "CSS" css)
		 (const :tag "Ditaa" ditaa)
		 (const :tag "Dot" dot)
		 (const :tag "Emacs Lisp" emacs-lisp)
		 (const :tag "Fortran" fortran)
		 (const :tag "Gnuplot" gnuplot)
		 (const :tag "Haskell" haskell)
		 (const :tag "Java" java)
		 (const :tag "Javascript" js)
		 (const :tag "Latex" latex)
		 (const :tag "Ledger" ledger)
		 (const :tag "Lilypond" lilypond)
		 (const :tag "Maxima" maxima)
		 (const :tag "Matlab" matlab)
		 (const :tag "Mscgen" mscgen)
		 (const :tag "Ocaml" ocaml)
		 (const :tag "Octave" octave)
		 (const :tag "Org" org)
		 (const :tag "Perl" perl)
		 (const :tag "Pico Lisp" picolisp)
		 (const :tag "PlantUML" plantuml)
		 (const :tag "Python" python)
		 (const :tag "Ruby" ruby)
		 (const :tag "Sass" sass)
		 (const :tag "Scheme" scheme)
		 (const :tag "Screen" screen)
		 (const :tag "Shell Script" sh)
		 (const :tag "Shen" shen)
		 (const :tag "Sql" sql)
		 (const :tag "Sqlite" sqlite))
		:value-type (boolean :tag "Activate" :value t)))

;;;; Customization variables
(defcustom org-clone-delete-id nil
  "Remove ID property of clones of a subtree.
When non-nil, clones of a subtree don't inherit the ID property.
Otherwise they inherit the ID property with a new unique
identifier."
  :type 'boolean
  :version "24.1"
  :group 'org-id)

;;; Version

(defconst org-version "7.8.11"
  "The version number of the file org.el.")

;;;###autoload
(defun org-version (&optional here)
  "Show the org-mode version in the echo area.
With prefix arg HERE, insert it at point."
  (interactive "P")
  (let* ((origin default-directory)
	 (version org-version)
	 (git-version)
	 (dir (concat (file-name-directory (locate-library "org")) "../" )))
    (when (and (file-exists-p (expand-file-name ".git" dir))
	       (executable-find "git"))
      (unwind-protect
	  (progn
	    (cd dir)
	    (when (eql 0 (shell-command "git describe --abbrev=4 HEAD"))
	      (with-current-buffer "*Shell Command Output*"
		(goto-char (point-min))
		(setq git-version (buffer-substring (point) (point-at-eol))))
	      (subst-char-in-string ?- ?. git-version t)
	      (when (string-match "\\S-"
				  (shell-command-to-string
				   "git diff-index --name-only HEAD --"))
		(setq git-version (concat git-version ".dirty")))
	      (setq version (concat version " (" git-version ")"))))
	(cd origin)))
    (setq version (format "Org-mode version %s" version))
    (if here (insert version))
    (message version)))

;;; Compatibility constants

;;; The custom variables

(defgroup org nil
  "Outline-based notes management and organizer."
  :tag "Org"
  :group 'outlines
  :group 'calendar)

(defcustom org-mode-hook nil
  "Mode hook for Org-mode, run after the mode was turned on."
  :group 'org
  :type 'hook)

(defcustom org-load-hook nil
  "Hook that is run after org.el has been loaded."
  :group 'org
  :type 'hook)

(defcustom org-log-buffer-setup-hook nil
  "Hook that is run after an Org log buffer is created."
  :group 'org
  :version "24.1"
  :type 'hook)

(defvar org-modules)  ; defined below
(defvar org-modules-loaded nil
  "Have the modules been loaded already?")

(defun org-load-modules-maybe (&optional force)
  "Load all extensions listed in `org-modules'."
  (when (or force (not org-modules-loaded))
    (mapc (lambda (ext)
	    (condition-case nil (require ext)
	      (error (message "Problems while trying to load feature `%s'" ext))))
	  org-modules)
    (setq org-modules-loaded t)))

(defun org-set-modules (var value)
  "Set VAR to VALUE and call `org-load-modules-maybe' with the force flag."
  (set var value)
  (when (featurep 'org)
    (org-load-modules-maybe 'force)))

(when (org-bound-and-true-p org-modules)
  (let ((a (member 'org-infojs org-modules)))
    (and a (setcar a 'org-jsinfo))))

(defcustom org-modules '(org-bbdb org-bibtex org-docview org-gnus org-info org-jsinfo org-irc org-mew org-mhe org-rmail org-vm org-w3m org-wl)
  "Modules that should always be loaded together with org.el.
If a description starts with <C>, the file is not part of Emacs
and loading it will require that you have downloaded and properly installed
the org-mode distribution.

You can also use this system to load external packages (i.e. neither Org
core modules, nor modules from the CONTRIB directory).  Just add symbols
to the end of the list.  If the package is called org-xyz.el, then you need
to add the symbol `xyz', and the package must have a call to

   (provide 'org-xyz)"
  :group 'org
  :set 'org-set-modules
  :type
  '(set :greedy t
	(const :tag "   bbdb:              Links to BBDB entries" org-bbdb)
	(const :tag "   bibtex:            Links to BibTeX entries" org-bibtex)
	(const :tag "   crypt:             Encryption of subtrees" org-crypt)
	(const :tag "   ctags:             Access to Emacs tags with links" org-ctags)
	(const :tag "   docview:           Links to doc-view buffers" org-docview)
	(const :tag "   gnus:              Links to GNUS folders/messages" org-gnus)
	(const :tag "   id:                Global IDs for identifying entries" org-id)
	(const :tag "   info:              Links to Info nodes" org-info)
	(const :tag "   jsinfo:            Set up Sebastian Rose's JavaScript org-info.js" org-jsinfo)
	(const :tag "   habit:             Track your consistency with habits" org-habit)
	(const :tag "   inlinetask:        Tasks independent of outline hierarchy" org-inlinetask)
	(const :tag "   irc:               Links to IRC/ERC chat sessions" org-irc)
	(const :tag "   mac-message:       Links to messages in Apple Mail" org-mac-message)
	(const :tag "   mew                Links to Mew folders/messages" org-mew)
	(const :tag "   mhe:               Links to MHE folders/messages" org-mhe)
	(const :tag "   protocol:          Intercept calls from emacsclient" org-protocol)
	(const :tag "   rmail:             Links to RMAIL folders/messages" org-rmail)
	(const :tag "   special-blocks:    Turn blocks into LaTeX envs and HTML divs" org-special-blocks)
	(const :tag "   vm:                Links to VM folders/messages" org-vm)
	(const :tag "   wl:                Links to Wanderlust folders/messages" org-wl)
	(const :tag "   w3m:               Special cut/paste from w3m to Org-mode." org-w3m)
	(const :tag "   mouse:             Additional mouse support" org-mouse)
	(const :tag "   TaskJuggler:       Export tasks to a TaskJuggler project" org-taskjuggler)

	(const :tag "C  annotate-file:     Annotate a file with org syntax" org-annotate-file)
	(const :tag "C  bookmark:          Org-mode links to bookmarks" org-bookmark)
	(const :tag "C  checklist:         Extra functions for checklists in repeated tasks" org-checklist)
	(const :tag "C  choose:            Use TODO keywords to mark decisions states" org-choose)
	(const :tag "C  collector:         Collect properties into tables" org-collector)
	(const :tag "C  depend:            TODO dependencies for Org-mode\n\t\t\t(PARTIALLY OBSOLETE, see built-in dependency support))" org-depend)
	(const :tag "C  drill:             Flashcards and spaced repetition for Org-mode" org-drill)
	(const :tag "C  elisp-symbol:      Org-mode links to emacs-lisp symbols" org-elisp-symbol)
	(const :tag "C  eshell             Support for links to working directories in eshell" org-eshell)
	(const :tag "C  eval:              Include command output as text" org-eval)
	(const :tag "C  eval-light:        Evaluate inbuffer-code on demand" org-eval-light)
	(const :tag "C  expiry:            Expiry mechanism for Org-mode entries" org-expiry)
	(const :tag "C  exp-bibtex:        Export citations using BibTeX" org-exp-bibtex)
	(const :tag "C  git-link:          Provide org links to specific file version" org-git-link)
	(const :tag "C  interactive-query: Interactive modification of tags query\n\t\t\t(PARTIALLY OBSOLETE, see secondary filtering)" org-interactive-query)

        (const :tag "C  invoice:           Help manage client invoices in Org-mode" org-invoice)

	(const :tag "C  jira:              Add a jira:ticket protocol to Org-mode" org-jira)
	(const :tag "C  learn:             SuperMemo's incremental learning algorithm" org-learn)
	(const :tag "C  mairix:            Hook mairix search into Org-mode for different MUAs" org-mairix)
	(const :tag "C  notmuch:           Provide org links to notmuch searches or messages" org-notmuch)
	(const :tag "C  mac-iCal           Imports events from iCal.app to the Emacs diary" org-mac-iCal)
	(const :tag "C  mac-link-grabber   Grab links and URLs from various Mac applications" org-mac-link-grabber)
	(const :tag "C  man:               Support for links to manpages in Org-mode" org-man)
	(const :tag "C  mtags:             Support for muse-like tags" org-mtags)
	(const :tag "C  panel:             Simple routines for us with bad memory" org-panel)
	(const :tag "C  registry:          A registry for Org-mode links" org-registry)
	(const :tag "C  org2rem:           Convert org appointments into reminders" org2rem)
	(const :tag "C  screen:            Visit screen sessions through Org-mode links" org-screen)
	(const :tag "C  secretary:         Team management with org-mode" org-secretary)
	(const :tag "C  sqlinsert:         Convert Org-mode tables to SQL insertions" orgtbl-sqlinsert)
	(const :tag "C  toc:               Table of contents for Org-mode buffer" org-toc)
	(const :tag "C  track:             Keep up with Org-mode development" org-track)
	(const :tag "C  velocity           Something like Notational Velocity for Org" org-velocity)
	(const :tag "C  wikinodes:         CamelCase wiki-like links" org-wikinodes)
	(repeat :tag "External packages" :inline t (symbol :tag "Package"))))

(defcustom org-support-shift-select nil
  "Non-nil means make shift-cursor commands select text when possible.

In Emacs 23, when `shift-select-mode' is on, shifted cursor keys
start selecting a region, or enlarge regions started in this way.
In Org-mode, in special contexts, these same keys are used for
other purposes, important enough to compete with shift selection.
Org tries to balance these needs by supporting `shift-select-mode'
outside these special contexts, under control of this variable.

The default of this variable is nil, to avoid confusing behavior.  Shifted
cursor keys will then execute Org commands in the following contexts:
- on a headline, changing TODO state (left/right) and priority (up/down)
- on a time stamp, changing the time
- in a plain list item, changing the bullet type
- in a property definition line, switching between allowed values
- in the BEGIN line of a clock table (changing the time block).
Outside these contexts, the commands will throw an error.

When this variable is t and the cursor is not in a special
context, Org-mode will support shift-selection for making and
enlarging regions.  To make this more effective, the bullet
cycling will no longer happen anywhere in an item line, but only
if the cursor is exactly on the bullet.

If you set this variable to the symbol `always', then the keys
will not be special in headlines, property lines, and item lines,
to make shift selection work there as well.  If this is what you
want, you can use the following alternative commands: `C-c C-t'
and `C-c ,' to change TODO state and priority, `C-u C-u C-c C-t'
can be used to switch TODO sets, `C-c -' to cycle item bullet
types, and properties can be edited by hand or in column view.

However, when the cursor is on a timestamp, shift-cursor commands
will still edit the time stamp - this is just too good to give up.

XEmacs user should have this variable set to nil, because
`shift-select-mode' is in Emacs 23 or later only."
  :group 'org
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "When outside special context" t)
	  (const :tag "Everywhere except timestamps" always)))

(defcustom org-loop-over-headlines-in-active-region nil
  "Shall some commands act upon headlines in the active region?

When set to `t', some commands will be performed in all headlines
within the active region.

When set to `start-level', some commands will be performed in all
headlines within the active region, provided that these headlines
are of the same level than the first one.

When set to a string, those commands will be performed on the
matching headlines within the active region.  Such string must be
a tags/property/todo match as it is used in the agenda tags view.

The list of commands is: `org-schedule', `org-deadline',
`org-todo', `org-archive-subtree', `org-archive-set-tag' and
`org-archive-to-archive-sibling'.  The archiving commands skip
already archived entries."
  :type '(choice (const :tag "Don't loop" nil)
		 (const :tag "All headlines in active region" t)
		 (const :tag "In active region, headlines at the same level than the first one" 'start-level)
		 (string :tag "Tags/Property/Todo matcher"))
  :version "24.1"
  :group 'org-todo
  :group 'org-archive)

(defgroup org-startup nil
  "Options concerning startup of Org-mode."
  :tag "Org Startup"
  :group 'org)

(defcustom org-startup-folded t
  "Non-nil means entering Org-mode will switch to OVERVIEW.
This can also be configured on a per-file basis by adding one of
the following lines anywhere in the buffer:

   #+STARTUP: fold              (or `overview', this is equivalent)
   #+STARTUP: nofold            (or `showall', this is equivalent)
   #+STARTUP: content
   #+STARTUP: showeverything"
  :group 'org-startup
  :type '(choice
	  (const :tag "nofold: show all" nil)
	  (const :tag "fold: overview" t)
	  (const :tag "content: all headlines" content)
	  (const :tag "show everything, even drawers" showeverything)))

(defcustom org-startup-truncated t
  "Non-nil means entering Org-mode will set `truncate-lines'.
This is useful since some lines containing links can be very long and
uninteresting.  Also tables look terrible when wrapped."
  :group 'org-startup
  :type 'boolean)

(defcustom org-startup-indented nil
  "Non-nil means turn on `org-indent-mode' on startup.
This can also be configured on a per-file basis by adding one of
the following lines anywhere in the buffer:

   #+STARTUP: indent
   #+STARTUP: noindent"
  :group 'org-structure
  :type '(choice
	  (const :tag "Not" nil)
	  (const :tag "Globally (slow on startup in large files)" t)))

(defcustom org-use-sub-superscripts t
  "Non-nil means interpret \"_\" and \"^\" for export.
When this option is turned on, you can use TeX-like syntax for sub- and
superscripts.  Several characters after \"_\" or \"^\" will be
considered as a single item - so grouping with {} is normally not
needed.  For example, the following things will be parsed as single
sub- or superscripts.

 10^24   or   10^tau     several digits will be considered 1 item.
 10^-12  or   10^-tau    a leading sign with digits or a word
 x^2-y^3                 will be read as x^2 - y^3, because items are
			 terminated by almost any nonword/nondigit char.
 x_{i^2} or   x^(2-i)    braces or parenthesis do grouping.

Still, ambiguity is possible - so when in doubt use {} to enclose the
sub/superscript.  If you set this variable to the symbol `{}',
the braces are *required* in order to trigger interpretations as
sub/superscript.  This can be helpful in documents that need \"_\"
frequently in plain text.

Not all export backends support this, but HTML does.

This option can also be set with the +OPTIONS line, e.g. \"^:nil\"."
  :group 'org-startup
  :group 'org-export-translation
  :version "24.1"
  :type '(choice
	  (const :tag "Always interpret" t)
	  (const :tag "Only with braces" {})
	  (const :tag "Never interpret" nil)))

(if (fboundp 'defvaralias)
    (defvaralias 'org-export-with-sub-superscripts 'org-use-sub-superscripts))


(defcustom org-startup-with-beamer-mode nil
  "Non-nil means turn on `org-beamer-mode' on startup.
This can also be configured on a per-file basis by adding one of
the following lines anywhere in the buffer:

   #+STARTUP: beamer"
  :group 'org-startup
  :version "24.1"
  :type 'boolean)

(defcustom org-startup-align-all-tables nil
  "Non-nil means align all tables when visiting a file.
This is useful when the column width in tables is forced with <N> cookies
in table fields.  Such tables will look correct only after the first re-align.
This can also be configured on a per-file basis by adding one of
the following lines anywhere in the buffer:
   #+STARTUP: align
   #+STARTUP: noalign"
  :group 'org-startup
  :type 'boolean)

(defcustom org-startup-with-inline-images nil
  "Non-nil means show inline images when loading a new Org file.
This can also be configured on a per-file basis by adding one of
the following lines anywhere in the buffer:
   #+STARTUP: inlineimages
   #+STARTUP: noinlineimages"
  :group 'org-startup
  :version "24.1"
  :type 'boolean)

(defcustom org-insert-mode-line-in-empty-file nil
  "Non-nil means insert the first line setting Org-mode in empty files.
When the function `org-mode' is called interactively in an empty file, this
normally means that the file name does not automatically trigger Org-mode.
To ensure that the file will always be in Org-mode in the future, a
line enforcing Org-mode will be inserted into the buffer, if this option
has been set."
  :group 'org-startup
  :type 'boolean)

(defcustom org-replace-disputed-keys nil
  "Non-nil means use alternative key bindings for some keys.
Org-mode uses S-<cursor> keys for changing timestamps and priorities.
These keys are also used by other packages like shift-selection-mode'
\(built into Emacs 23), `CUA-mode' or `windmove.el'.
If you want to use Org-mode together with one of these other modes,
or more generally if you would like to move some Org-mode commands to
other keys, set this variable and configure the keys with the variable
`org-disputed-keys'.

This option is only relevant at load-time of Org-mode, and must be set
*before* org.el is loaded.  Changing it requires a restart of Emacs to
become effective."
  :group 'org-startup
  :type 'boolean)

(defcustom org-use-extra-keys nil
  "Non-nil means use extra key sequence definitions for certain commands.
This happens automatically if you run XEmacs or if `window-system'
is nil.  This variable lets you do the same manually.  You must
set it before loading org.

Example: on Carbon Emacs 22 running graphically, with an external
keyboard on a Powerbook, the default way of setting M-left might
not work for either Alt or ESC.  Setting this variable will make
it work for ESC."
  :group 'org-startup
  :type 'boolean)

(if (fboundp 'defvaralias)
    (defvaralias 'org-CUA-compatible 'org-replace-disputed-keys))

(defcustom org-disputed-keys
  '(([(shift up)]		. [(meta p)])
    ([(shift down)]		. [(meta n)])
    ([(shift left)]		. [(meta -)])
    ([(shift right)]		. [(meta +)])
    ([(control shift right)] 	. [(meta shift +)])
    ([(control shift left)]	. [(meta shift -)]))
  "Keys for which Org-mode and other modes compete.
This is an alist, cars are the default keys, second element specifies
the alternative to use when `org-replace-disputed-keys' is t.

Keys can be specified in any syntax supported by `define-key'.
The value of this option takes effect only at Org-mode's startup,
therefore you'll have to restart Emacs to apply it after changing."
  :group 'org-startup
  :type 'alist)

(defun org-key (key)
  "Select key according to `org-replace-disputed-keys' and `org-disputed-keys'.
Or return the original if not disputed.
Also apply the translations defined in `org-xemacs-key-equivalents'."
  (when org-replace-disputed-keys
    (let* ((nkey (key-description key))
	   (x (org-find-if (lambda (x)
			     (equal (key-description (car x)) nkey))
			   org-disputed-keys)))
      (setq key (if x (cdr x) key))))
  (when (featurep 'xemacs)
    (setq key (or (cdr (assoc key org-xemacs-key-equivalents)) key)))
  key)

(defun org-find-if (predicate seq)
  (catch 'exit
    (while seq
      (if (funcall predicate (car seq))
	  (throw 'exit (car seq))
	(pop seq)))))

(defun org-defkey (keymap key def)
  "Define a key, possibly translated, as returned by `org-key'."
  (define-key keymap (org-key key) def))

(defcustom org-ellipsis nil
  "The ellipsis to use in the Org-mode outline.
When nil, just use the standard three dots.  When a string, use that instead,
When a face, use the standard 3 dots, but with the specified face.
The change affects only Org-mode (which will then use its own display table).
Changing this requires executing `M-x org-mode' in a buffer to become
effective."
  :group 'org-startup
  :type '(choice (const :tag "Default" nil)
		 (face :tag "Face" :value org-warning)
		 (string :tag "String" :value "...#")))

(defvar org-display-table nil
  "The display table for org-mode, in case `org-ellipsis' is non-nil.")

(defgroup org-keywords nil
  "Keywords in Org-mode."
  :tag "Org Keywords"
  :group 'org)

(defcustom org-deadline-string "DEADLINE:"
  "String to mark deadline entries.
A deadline is this string, followed by a time stamp.  Should be a word,
terminated by a colon.  You can insert a schedule keyword and
a timestamp with \\[org-deadline].
Changes become only effective after restarting Emacs."
  :group 'org-keywords
  :type 'string)

(defcustom org-scheduled-string "SCHEDULED:"
  "String to mark scheduled TODO entries.
A schedule is this string, followed by a time stamp.  Should be a word,
terminated by a colon.  You can insert a schedule keyword and
a timestamp with \\[org-schedule].
Changes become only effective after restarting Emacs."
  :group 'org-keywords
  :type 'string)

(defcustom org-closed-string "CLOSED:"
  "String used as the prefix for timestamps logging closing a TODO entry."
  :group 'org-keywords
  :type 'string)

(defcustom org-clock-string "CLOCK:"
  "String used as prefix for timestamps clocking work hours on an item."
  :group 'org-keywords
  :type 'string)

(defcustom org-comment-string "COMMENT"
  "Entries starting with this keyword will never be exported.
An entry can be toggled between COMMENT and normal with
\\[org-toggle-comment].
Changes become only effective after restarting Emacs."
  :group 'org-keywords
  :type 'string)

(defcustom org-quote-string "QUOTE"
  "Entries starting with this keyword will be exported in fixed-width font.
Quoting applies only to the text in the entry following the headline, and does
not extend beyond the next headline, even if that is lower level.
An entry can be toggled between QUOTE and normal with
\\[org-toggle-fixed-width-section]."
  :group 'org-keywords
  :type 'string)

(defconst org-repeat-re
  "<[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9] [^>\n]*?\\([.+]?\\+[0-9]+[dwmy]\\(/[0-9]+[dwmy]\\)?\\)"
  "Regular expression for specifying repeated events.
After a match, group 1 contains the repeat expression.")

(defgroup org-structure nil
  "Options concerning the general structure of Org-mode files."
  :tag "Org Structure"
  :group 'org)

(defgroup org-reveal-location nil
  "Options about how to make context of a location visible."
  :tag "Org Reveal Location"
  :group 'org-structure)

(defconst org-context-choice
  '(choice
    (const :tag "Always" t)
    (const :tag "Never" nil)
    (repeat :greedy t :tag "Individual contexts"
	    (cons
	     (choice :tag "Context"
		     (const agenda)
		     (const org-goto)
		     (const occur-tree)
		     (const tags-tree)
		     (const link-search)
		     (const mark-goto)
		     (const bookmark-jump)
		     (const isearch)
		     (const default))
	     (boolean))))
  "Contexts for the reveal options.")

(defcustom org-show-hierarchy-above '((default . t))
  "Non-nil means show full hierarchy when revealing a location.
Org-mode often shows locations in an org-mode file which might have
been invisible before.  When this is set, the hierarchy of headings
above the exposed location is shown.
Turning this off for example for sparse trees makes them very compact.
Instead of t, this can also be an alist specifying this option for different
contexts.  Valid contexts are
  agenda         when exposing an entry from the agenda
  org-goto       when using the command `org-goto' on key C-c C-j
  occur-tree     when using the command `org-occur' on key C-c /
  tags-tree      when constructing a sparse tree based on tags matches
  link-search    when exposing search matches associated with a link
  mark-goto      when exposing the jump goal of a mark
  bookmark-jump  when exposing a bookmark location
  isearch        when exiting from an incremental search
  default        default for all contexts not set explicitly"
  :group 'org-reveal-location
  :type org-context-choice)

(defcustom org-show-following-heading '((default . nil))
  "Non-nil means show following heading when revealing a location.
Org-mode often shows locations in an org-mode file which might have
been invisible before.  When this is set, the heading following the
match is shown.
Turning this off for example for sparse trees makes them very compact,
but makes it harder to edit the location of the match.  In such a case,
use the command \\[org-reveal] to show more context.
Instead of t, this can also be an alist specifying this option for different
contexts.  See `org-show-hierarchy-above' for valid contexts."
  :group 'org-reveal-location
  :type org-context-choice)

(defcustom org-show-siblings '((default . nil) (isearch t))
  "Non-nil means show all sibling heading when revealing a location.
Org-mode often shows locations in an org-mode file which might have
been invisible before.  When this is set, the sibling of the current entry
heading are all made visible.  If `org-show-hierarchy-above' is t,
the same happens on each level of the hierarchy above the current entry.

By default this is on for the isearch context, off for all other contexts.
Turning this off for example for sparse trees makes them very compact,
but makes it harder to edit the location of the match.  In such a case,
use the command \\[org-reveal] to show more context.
Instead of t, this can also be an alist specifying this option for different
contexts.  See `org-show-hierarchy-above' for valid contexts."
  :group 'org-reveal-location
  :type org-context-choice)

(defcustom org-show-entry-below '((default . nil))
  "Non-nil means show the entry below a headline when revealing a location.
Org-mode often shows locations in an org-mode file which might have
been invisible before.  When this is set, the text below the headline that is
exposed is also shown.

By default this is off for all contexts.
Instead of t, this can also be an alist specifying this option for different
contexts.  See `org-show-hierarchy-above' for valid contexts."
  :group 'org-reveal-location
  :type org-context-choice)

(defcustom org-indirect-buffer-display 'other-window
  "How should indirect tree buffers be displayed?
This applies to indirect buffers created with the commands
\\[org-tree-to-indirect-buffer] and \\[org-agenda-tree-to-indirect-buffer].
Valid values are:
current-window   Display in the current window
other-window     Just display in another window.
dedicated-frame  Create one new frame, and re-use it each time.
new-frame        Make a new frame each time.  Note that in this case
                 previously-made indirect buffers are kept, and you need to
                 kill these buffers yourself."
  :group 'org-structure
  :group 'org-agenda-windows
  :type '(choice
	  (const :tag "In current window" current-window)
	  (const :tag "In current frame, other window" other-window)
	  (const :tag "Each time a new frame" new-frame)
	  (const :tag "One dedicated frame" dedicated-frame)))

(defcustom org-use-speed-commands nil
  "Non-nil means activate single letter commands at beginning of a headline.
This may also be a function to test for appropriate locations where speed
commands should be active."
  :group 'org-structure
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "At beginning of headline stars" t)
	  (function)))

(defcustom org-speed-commands-user nil
    "Alist of additional speed commands.
This list will be checked before `org-speed-commands-default'
when the variable `org-use-speed-commands' is non-nil
and when the cursor is at the beginning of a headline.
The car if each entry is a string with a single letter, which must
be assigned to `self-insert-command' in the global map.
The cdr is either a command to be called interactively, a function
to be called, or a form to be evaluated.
An entry that is just a list with a single string will be interpreted
as a descriptive headline that will be added when listing the speed
commands in the Help buffer using the `?' speed command."
    :group 'org-structure
    :type '(repeat :value ("k" . ignore)
	    (choice :value ("k" . ignore)
	     (list :tag "Descriptive Headline" (string :tag "Headline"))
	     (cons :tag "Letter and Command"
	      (string :tag "Command letter")
	      (choice
	       (function)
	       (sexp))))))

(defgroup org-cycle nil
  "Options concerning visibility cycling in Org-mode."
  :tag "Org Cycle"
  :group 'org-structure)

(defcustom org-cycle-skip-children-state-if-no-children t
  "Non-nil means skip CHILDREN state in entries that don't have any."
  :group 'org-cycle
  :type 'boolean)

(defcustom org-cycle-max-level nil
  "Maximum level which should still be subject to visibility cycling.
Levels higher than this will, for cycling, be treated as text, not a headline.
When `org-odd-levels-only' is set, a value of N in this variable actually
means 2N-1 stars as the limiting headline.
When nil, cycle all levels.
Note that the limiting level of cycling is also influenced by
`org-inlinetask-min-level'.  When `org-cycle-max-level' is not set but
`org-inlinetask-min-level' is, cycling will be limited to levels one less
than its value."
  :group 'org-cycle
  :type '(choice
	  (const :tag "No limit" nil)
	  (integer :tag "Maximum level")))

(defcustom org-drawers '("PROPERTIES" "CLOCK" "LOGBOOK" "RESULTS")
  "Names of drawers.  Drawers are not opened by cycling on the headline above.
Drawers only open with a TAB on the drawer line itself.  A drawer looks like
this:
   :DRAWERNAME:
   .....
   :END:
The drawer \"PROPERTIES\" is special for capturing properties through
the property API.

Drawers can be defined on the per-file basis with a line like:

#+DRAWERS: HIDDEN STATE PROPERTIES"
  :group 'org-structure
  :group 'org-cycle
  :type '(repeat (string :tag "Drawer Name")))

(defcustom org-hide-block-startup nil
  "Non-nil means entering Org-mode will fold all blocks.
This can also be set in on a per-file basis with

#+STARTUP: hideblocks
#+STARTUP: showblocks"
  :group 'org-startup
  :group 'org-cycle
  :type 'boolean)

(defcustom org-cycle-global-at-bob nil
  "Cycle globally if cursor is at beginning of buffer and not at a headline.
This makes it possible to do global cycling without having to use S-TAB or
\\[universal-argument] TAB.  For this special case to work, the first line \
of the buffer
must not be a headline - it may be empty or some other text.  When used in
this way, `org-cycle-hook' is disables temporarily, to make sure the
cursor stays at the beginning of the buffer.
When this option is nil, don't do anything special at the beginning
of the buffer."
  :group 'org-cycle
  :type 'boolean)

(defcustom org-cycle-level-after-item/entry-creation t
  "Non-nil means cycle entry level or item indentation in new empty entries.

When the cursor is at the end of an empty headline, i.e with only stars
and maybe a TODO keyword, TAB will then switch the entry to become a child,
and then all possible ancestor states, before returning to the original state.
This makes data entry extremely fast:  M-RET to create a new headline,
on TAB to make it a child, two or more tabs to make it a (grand-)uncle.

When the cursor is at the end of an empty plain list item, one TAB will
make it a subitem, two or more tabs will back up to make this an item
higher up in the item hierarchy."
  :group 'org-cycle
  :type 'boolean)

(defcustom org-cycle-emulate-tab t
  "Where should `org-cycle' emulate TAB.
nil         Never
white       Only in completely white lines
whitestart  Only at the beginning of lines, before the first non-white char
t           Everywhere except in headlines
exc-hl-bol  Everywhere except at the start of a headline
If TAB is used in a place where it does not emulate TAB, the current subtree
visibility is cycled."
  :group 'org-cycle
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Only in completely white lines" white)
		 (const :tag "Before first char in a line" whitestart)
		 (const :tag "Everywhere except in headlines" t)
		 (const :tag "Everywhere except at bol in headlines" exc-hl-bol)
		 ))

(defcustom org-cycle-separator-lines 2
  "Number of empty lines needed to keep an empty line between collapsed trees.
If you leave an empty line between the end of a subtree and the following
headline, this empty line is hidden when the subtree is folded.
Org-mode will leave (exactly) one empty line visible if the number of
empty lines is equal or larger to the number given in this variable.
So the default 2 means at least 2 empty lines after the end of a subtree
are needed to produce free space between a collapsed subtree and the
following headline.

If the number is negative, and the number of empty lines is at least -N,
all empty lines are shown.

Special case: when 0, never leave empty lines in collapsed view."
  :group 'org-cycle
  :type 'integer)
(put 'org-cycle-separator-lines 'safe-local-variable 'integerp)

(defcustom org-pre-cycle-hook nil
  "Hook that is run before visibility cycling is happening.
The function(s) in this hook must accept a single argument which indicates
the new state that will be set right after running this hook.  The
argument is a symbol.  Before a global state change, it can have the values
`overview', `content', or `all'.  Before a local state change, it can have
the values `folded', `children', or `subtree'."
  :group 'org-cycle
  :type 'hook)

(defcustom org-cycle-hook '(org-cycle-hide-archived-subtrees
			    org-cycle-hide-drawers
			    org-cycle-show-empty-lines
			    org-optimize-window-after-visibility-change)
  "Hook that is run after `org-cycle' has changed the buffer visibility.
The function(s) in this hook must accept a single argument which indicates
the new state that was set by the most recent `org-cycle' command.  The
argument is a symbol.  After a global state change, it can have the values
`overview', `content', or `all'.  After a local state change, it can have
the values `folded', `children', or `subtree'."
  :group 'org-cycle
  :type 'hook)

(defgroup org-edit-structure nil
  "Options concerning structure editing in Org-mode."
  :tag "Org Edit Structure"
  :group 'org-structure)

(defcustom org-odd-levels-only nil
  "Non-nil means skip even levels and only use odd levels for the outline.
This has the effect that two stars are being added/taken away in
promotion/demotion commands.  It also influences how levels are
handled by the exporters.
Changing it requires restart of `font-lock-mode' to become effective
for fontification also in regions already fontified.
You may also set this on a per-file basis by adding one of the following
lines to the buffer:

   #+STARTUP: odd
   #+STARTUP: oddeven"
  :group 'org-edit-structure
  :group 'org-appearance
  :type 'boolean)

(defcustom org-adapt-indentation t
  "Non-nil means adapt indentation to outline node level.

When this variable is set, Org assumes that you write outlines by
indenting text in each node to align with the headline (after the stars).
The following issues are influenced by this variable:

- When this is set and the *entire* text in an entry is indented, the
  indentation is increased by one space in a demotion command, and
  decreased by one in a promotion command.  If any line in the entry
  body starts with text at column 0, indentation is not changed at all.

- Property drawers and planning information is inserted indented when
  this variable s set.  When nil, they will not be indented.

- TAB indents a line relative to context.  The lines below a headline
  will be indented when this variable is set.

Note that this is all about true indentation, by adding and removing
space characters.  See also `org-indent.el' which does level-dependent
indentation in a virtual way, i.e. at display time in Emacs."
  :group 'org-edit-structure
  :type 'boolean)

(defcustom org-special-ctrl-a/e nil
  "Non-nil means `C-a' and `C-e' behave specially in headlines and items.

When t, `C-a' will bring back the cursor to the beginning of the
headline text, i.e. after the stars and after a possible TODO keyword.
In an item, this will be the position after the bullet.
When the cursor is already at that position, another `C-a' will bring
it to the beginning of the line.

`C-e' will jump to the end of the headline, ignoring the presence of tags
in the headline.  A second `C-e' will then jump to the true end of the
line, after any tags.  This also means that, when this variable is
non-nil, `C-e' also will never jump beyond the end of the heading of a
folded section, i.e. not after the ellipses.

When set to the symbol `reversed', the first `C-a' or `C-e' works normally,
going to the true line boundary first.  Only a directly following, identical
keypress will bring the cursor to the special positions.

This may also be a cons cell where the behavior for `C-a' and `C-e' is
set separately."
  :group 'org-edit-structure
  :type '(choice
	  (const :tag "off" nil)
	  (const :tag "on: after stars/bullet and before tags first" t)
	  (const :tag "reversed: true line boundary first" reversed)
	  (cons :tag "Set C-a and C-e separately"
		(choice :tag "Special C-a"
			(const :tag "off" nil)
			(const :tag "on: after  stars/bullet first" t)
			(const :tag "reversed: before stars/bullet first" reversed))
		(choice :tag "Special C-e"
			(const :tag "off" nil)
			(const :tag "on: before tags first" t)
			(const :tag "reversed: after tags first" reversed)))))
(if (fboundp 'defvaralias)
    (defvaralias 'org-special-ctrl-a 'org-special-ctrl-a/e))

(defcustom org-special-ctrl-k nil
  "Non-nil means `C-k' will behave specially in headlines.
When nil, `C-k' will call the default `kill-line' command.
When t, the following will happen while the cursor is in the headline:

- When the cursor is at the beginning of a headline, kill the entire
  line and possible the folded subtree below the line.
- When in the middle of the headline text, kill the headline up to the tags.
- When after the headline text, kill the tags."
  :group 'org-edit-structure
  :type 'boolean)

(defcustom org-ctrl-k-protect-subtree nil
  "Non-nil means, do not delete a hidden subtree with C-k.
When set to the symbol `error', simply throw an error when C-k is
used to kill (part-of) a headline that has hidden text behind it.
Any other non-nil value will result in a query to the user, if it is
OK to kill that hidden subtree.  When nil, kill without remorse."
  :group 'org-edit-structure
  :version "24.1"
  :type '(choice
	  (const :tag "Do not protect hidden subtrees" nil)
	  (const :tag "Protect hidden subtrees with a security query" t)
	  (const :tag "Never kill a hidden subtree with C-k" error)))

(defcustom org-catch-invisible-edits nil
  "Check if in invisible region before inserting or deleting a character.
Valid values are:

nil              Do not check, so just do invisible edits.
error            Throw an error and do nothing.
show             Make point visible, and do the requested edit.
show-and-error   Make point visible, then throw an error and abort the edit.
smart            Make point visible, and do insertion/deletion if it is
                 adjacent to visible text and the change feels predictable.
                 Never delete a previously invisible character or add in the
                 middle or right after an invisible region.  Basically, this
                 allows insertion and backward-delete right before ellipses.
                 FIXME: maybe in this case we should not even show?"
  :group 'org-edit-structure
  :version "24.1"
  :type '(choice
	  (const :tag "Do not check" nil)
	  (const :tag "Throw error when trying to edit" error)
	  (const :tag "Unhide, but do not do the edit" show-and-error)
	  (const :tag "Show invisible part and do the edit" show)
	  (const :tag "Be smart and do the right thing" smart)))

(defcustom org-yank-folded-subtrees t
  "Non-nil means when yanking subtrees, fold them.
If the kill is a single subtree, or a sequence of subtrees, i.e. if
it starts with a heading and all other headings in it are either children
or siblings, then fold all the subtrees.  However, do this only if no
text after the yank would be swallowed into a folded tree by this action."
  :group 'org-edit-structure
  :type 'boolean)

(defcustom org-yank-adjusted-subtrees nil
  "Non-nil means when yanking subtrees, adjust the level.
With this setting, `org-paste-subtree' is used to insert the subtree, see
this function for details."
  :group 'org-edit-structure
  :type 'boolean)

(defcustom org-M-RET-may-split-line '((default . t))
  "Non-nil means M-RET will split the line at the cursor position.
When nil, it will go to the end of the line before making a
new line.
You may also set this option in a different way for different
contexts.  Valid contexts are:

headline  when creating a new headline
item      when creating a new item
table     in a table field
default   the value to be used for all contexts not explicitly
          customized"
  :group 'org-structure
  :group 'org-table
  :type '(choice
	  (const :tag "Always" t)
	  (const :tag "Never" nil)
	  (repeat :greedy t :tag "Individual contexts"
		  (cons
		   (choice :tag "Context"
			   (const headline)
			   (const item)
			   (const table)
			   (const default))
		   (boolean)))))


(defcustom org-insert-heading-respect-content nil
  "Non-nil means insert new headings after the current subtree.
When nil, the new heading is created directly after the current line.
The commands \\[org-insert-heading-respect-content] and
\\[org-insert-todo-heading-respect-content] turn this variable on
for the duration of the command."
  :group 'org-structure
  :type 'boolean)

(defcustom org-blank-before-new-entry '((heading . auto)
					(plain-list-item . auto))
  "Should `org-insert-heading' leave a blank line before new heading/item?
The value is an alist, with `heading' and `plain-list-item' as CAR,
and a boolean flag as CDR.  The cdr may also be the symbol `auto', in
which case Org will look at the surrounding headings/items and try to
make an intelligent decision whether to insert a blank line or not.

For plain lists, if the variable `org-empty-line-terminates-plain-lists' is
set, the setting here is ignored and no empty line is inserted, to avoid
breaking the list structure."
  :group 'org-edit-structure
  :type '(list
	  (cons (const heading)
		(choice (const :tag "Never" nil)
			(const :tag "Always" t)
			(const :tag "Auto" auto)))
	  (cons (const plain-list-item)
		(choice (const :tag "Never" nil)
			(const :tag "Always" t)
			(const :tag "Auto" auto)))))

(defcustom org-insert-heading-hook nil
  "Hook being run after inserting a new heading."
  :group 'org-edit-structure
  :type 'hook)

(defcustom org-enable-fixed-width-editor t
  "Non-nil means lines starting with \":\" are treated as fixed-width.
This currently only means they are never auto-wrapped.
When nil, such lines will be treated like ordinary lines.
See also the QUOTE keyword."
  :group 'org-edit-structure
  :type 'boolean)

(defcustom org-goto-auto-isearch t
  "Non-nil means typing characters in `org-goto' starts incremental search."
  :group 'org-edit-structure
  :type 'boolean)

(defgroup org-sparse-trees nil
  "Options concerning sparse trees in Org-mode."
  :tag "Org Sparse Trees"
  :group 'org-structure)

(defcustom org-highlight-sparse-tree-matches t
  "Non-nil means highlight all matches that define a sparse tree.
The highlights will automatically disappear the next time the buffer is
changed by an edit command."
  :group 'org-sparse-trees
  :type 'boolean)

(defcustom org-remove-highlights-with-change t
  "Non-nil means any change to the buffer will remove temporary highlights.
Such highlights are created by `org-occur' and `org-clock-display'.
When nil, `C-c C-c needs to be used to get rid of the highlights.
The highlights created by `org-preview-latex-fragment' always need
`C-c C-c' to be removed."
  :group 'org-sparse-trees
  :group 'org-time
  :type 'boolean)


(defcustom org-occur-hook '(org-first-headline-recenter)
  "Hook that is run after `org-occur' has constructed a sparse tree.
This can be used to recenter the window to show as much of the structure
as possible."
  :group 'org-sparse-trees
  :type 'hook)

(defgroup org-imenu-and-speedbar nil
  "Options concerning imenu and speedbar in Org-mode."
  :tag "Org Imenu and Speedbar"
  :group 'org-structure)

(defcustom org-imenu-depth 2
  "The maximum level for Imenu access to Org-mode headlines.
This also applied for speedbar access."
  :group 'org-imenu-and-speedbar
  :type 'integer)

(defgroup org-table nil
  "Options concerning tables in Org-mode."
  :tag "Org Table"
  :group 'org)

(defcustom org-enable-table-editor 'optimized
  "Non-nil means lines starting with \"|\" are handled by the table editor.
When nil, such lines will be treated like ordinary lines.

When equal to the symbol `optimized', the table editor will be optimized to
do the following:
- Automatic overwrite mode in front of whitespace in table fields.
  This makes the structure of the table stay in tact as long as the edited
  field does not exceed the column width.
- Minimize the number of realigns.  Normally, the table is aligned each time
  TAB or RET are pressed to move to another field.  With optimization this
  happens only if changes to a field might have changed the column width.
Optimization requires replacing the functions `self-insert-command',
`delete-char', and `backward-delete-char' in Org-mode buffers, with a
slight (in fact: unnoticeable) speed impact for normal typing.  Org-mode is
very good at guessing when a re-align will be necessary, but you can always
force one with \\[org-ctrl-c-ctrl-c].

If you would like to use the optimized version in Org-mode, but the
un-optimized version in OrgTbl-mode, see the variable `orgtbl-optimized'.

This variable can be used to turn on and off the table editor during a session,
but in order to toggle optimization, a restart is required.

See also the variable `org-table-auto-blank-field'."
  :group 'org-table
  :type '(choice
	  (const :tag "off" nil)
	  (const :tag "on" t)
	  (const :tag "on, optimized" optimized)))

(defcustom org-self-insert-cluster-for-undo t
  "Non-nil means cluster self-insert commands for undo when possible.
If this is set, then, like in the Emacs command loop, 20 consecutive
characters will be undone together.
This is configurable, because there is some impact on typing performance."
  :group 'org-table
  :type 'boolean)

(defcustom org-table-tab-recognizes-table.el t
  "Non-nil means TAB will automatically notice a table.el table.
When it sees such a table, it moves point into it and - if necessary -
calls `table-recognize-table'."
  :group 'org-table-editing
  :type 'boolean)

(defgroup org-link nil
  "Options concerning links in Org-mode."
  :tag "Org Link"
  :group 'org)

(defvar org-link-abbrev-alist-local nil
  "Buffer-local version of `org-link-abbrev-alist', which see.
The value of this is taken from the #+LINK lines.")
(make-variable-buffer-local 'org-link-abbrev-alist-local)

(defcustom org-link-abbrev-alist nil
  "Alist of link abbreviations.
The car of each element is a string, to be replaced at the start of a link.
The cdrs are replacement values, like (\"linkkey\" . REPLACE).  Abbreviated
links in Org-mode buffers can have an optional tag after a double colon, e.g.

     [[linkkey:tag][description]]

The 'linkkey' must be a word word, starting with a letter, followed
by letters, numbers, '-' or '_'.

If REPLACE is a string, the tag will simply be appended to create the link.
If the string contains \"%s\", the tag will be inserted there.  Alternatively,
the placeholder \"%h\" will cause a url-encoded version of the tag to
be inserted at that point (see the function `url-hexify-string').

REPLACE may also be a function that will be called with the tag as the
only argument to create the link, which should be returned as a string.

See the manual for examples."
  :group 'org-link
  :type '(repeat
	  (cons
	   (string :tag "Protocol")
	   (choice
	    (string :tag "Format")
	    (function)))))

(defcustom org-descriptive-links t
  "Non-nil means Org will display descriptive links.
E.g. [[http://orgmode.org][Org website]] will be displayed as
\"Org Website\", hiding the link itself and just displaying its
description.  When set to `nil', Org will display the full links
literally.

You can interactively set the value of this variable by calling
`org-toggle-link-display' or from the menu Org>Hyperlinks menu."
  :group 'org-link
  :type 'boolean)

(defcustom org-link-file-path-type 'adaptive
  "How the path name in file links should be stored.
Valid values are:

relative  Relative to the current directory, i.e. the directory of the file
          into which the link is being inserted.
absolute  Absolute path, if possible with ~ for home directory.
noabbrev  Absolute path, no abbreviation of home directory.
adaptive  Use relative path for files in the current directory and sub-
          directories of it.  For other files, use an absolute path."
  :group 'org-link
  :type '(choice
	  (const relative)
	  (const absolute)
	  (const noabbrev)
	  (const adaptive)))

(defcustom org-activate-links '(bracket angle plain radio tag date footnote)
  "Types of links that should be activated in Org-mode files.
This is a list of symbols, each leading to the activation of a certain link
type.  In principle, it does not hurt to turn on most link types - there may
be a small gain when turning off unused link types.  The types are:

bracket   The recommended [[link][description]] or [[link]] links with hiding.
angle     Links in angular brackets that may contain whitespace like
          <bbdb:Carsten Dominik>.
plain     Plain links in normal text, no whitespace, like http://google.com.
radio     Text that is matched by a radio target, see manual for details.
tag       Tag settings in a headline (link to tag search).
date      Time stamps (link to calendar).
footnote  Footnote labels.

Changing this variable requires a restart of Emacs to become effective."
  :group 'org-link
  :type '(set :greedy t
	      (const :tag "Double bracket links" bracket)
	      (const :tag "Angular bracket links" angle)
	      (const :tag "Plain text links" plain)
	      (const :tag "Radio target matches" radio)
	      (const :tag "Tags" tag)
	      (const :tag "Timestamps" date)
	      (const :tag "Footnotes" footnote)))

(defcustom org-make-link-description-function nil
  "Function to use to generate link descriptions from links.
If nil the link location will be used.  This function must take
two parameters; the first is the link and the second the
description `org-insert-link' has generated, and should return the
description to use."
  :group 'org-link
  :type 'function)

(defgroup org-link-store nil
  "Options concerning storing links in Org-mode."
  :tag "Org Store Link"
  :group 'org-link)

(defcustom org-email-link-description-format "Email %c: %.30s"
  "Format of the description part of a link to an email or usenet message.
The following %-escapes will be replaced by corresponding information:

%F   full \"From\" field
%f   name, taken from \"From\" field, address if no name
%T   full \"To\" field
%t   first name in \"To\" field, address if no name
%c   correspondent.  Usually \"from NAME\", but if you sent it yourself, it
     will be \"to NAME\".  See also the variable `org-from-is-user-regexp'.
%s   subject
%d   date
%m   message-id.

You may use normal field width specification between the % and the letter.
This is for example useful to limit the length of the subject.

Examples: \"%f on: %.30s\", \"Email from %f\", \"Email %c\""
  :group 'org-link-store
  :type 'string)

(defcustom org-from-is-user-regexp
  (let (r1 r2)
    (when (and user-mail-address (not (string= user-mail-address "")))
      (setq r1 (concat "\\<" (regexp-quote user-mail-address) "\\>")))
    (when (and user-full-name (not (string= user-full-name "")))
      (setq r2 (concat "\\<" (regexp-quote user-full-name) "\\>")))
    (if (and r1 r2) (concat r1 "\\|" r2) (or r1 r2)))
  "Regexp matched against the \"From:\" header of an email or usenet message.
It should match if the message is from the user him/herself."
  :group 'org-link-store
  :type 'regexp)

(defcustom org-link-to-org-use-id 'create-if-interactive-and-no-custom-id
  "Non-nil means storing a link to an Org file will use entry IDs.

Note that before this variable is even considered, org-id must be loaded,
so please customize `org-modules' and turn it on.

The variable can have the following values:

t     Create an ID if needed to make a link to the current entry.

create-if-interactive
      If `org-store-link' is called directly (interactively, as a user
      command), do create an ID to support the link.  But when doing the
      job for remember, only use the ID if it already exists.  The
      purpose of this setting is to avoid proliferation of unwanted
      IDs, just because you happen to be in an Org file when you
      call `org-remember' that automatically and preemptively
      creates a link.  If you do want to get an ID link in a remember
      template to an entry not having an ID, create it first by
      explicitly creating a link to it, using `C-c C-l' first.

create-if-interactive-and-no-custom-id
      Like create-if-interactive, but do not create an ID if there is
      a CUSTOM_ID property defined in the entry.  This is the default.

use-existing
      Use existing ID, do not create one.

nil   Never use an ID to make a link, instead link using a text search for
      the headline text."
  :group 'org-link-store
  :type '(choice
	  (const :tag "Create ID to make link" t)
	  (const :tag "Create if storing link interactively"
		 create-if-interactive)
	  (const :tag "Create if storing link interactively and no CUSTOM_ID is present"
		 create-if-interactive-and-no-custom-id)
	  (const :tag "Only use existing" use-existing)
	  (const :tag "Do not use ID to create link" nil)))

(defcustom org-context-in-file-links t
  "Non-nil means file links from `org-store-link' contain context.
A search string will be added to the file name with :: as separator and
used to find the context when the link is activated by the command
`org-open-at-point'.  When this option is t, the entire active region
will be placed in the search string of the file link.  If set to a
positive integer, only the first n lines of context will be stored.

Using a prefix arg to the command \\[org-store-link] (`org-store-link')
negates this setting for the duration of the command."
  :group 'org-link-store
  :type '(choice boolean integer))

(defcustom org-keep-stored-link-after-insertion nil
  "Non-nil means keep link in list for entire session.

The command `org-store-link' adds a link pointing to the current
location to an internal list.  These links accumulate during a session.
The command `org-insert-link' can be used to insert links into any
Org-mode file (offering completion for all stored links).  When this
option is nil, every link which has been inserted once using \\[org-insert-link]
will be removed from the list, to make completing the unused links
more efficient."
  :group 'org-link-store
  :type 'boolean)

(defgroup org-link-follow nil
  "Options concerning following links in Org-mode."
  :tag "Org Follow Link"
  :group 'org-link)

(defcustom org-link-translation-function nil
  "Function to translate links with different syntax to Org syntax.
This can be used to translate links created for example by the Planner
or emacs-wiki packages to Org syntax.
The function must accept two parameters, a TYPE containing the link
protocol name like \"rmail\" or \"gnus\" as a string, and the linked path,
which is everything after the link protocol.  It should return a cons
with possibly modified values of type and path.
Org contains a function for this, so if you set this variable to
`org-translate-link-from-planner', you should be able follow many
links created by planner."
  :group 'org-link-follow
  :type 'function)

(defcustom org-follow-link-hook nil
  "Hook that is run after a link has been followed."
  :group 'org-link-follow
  :type 'hook)

(defcustom org-tab-follows-link nil
  "Non-nil means on links TAB will follow the link.
Needs to be set before org.el is loaded.
This really should not be used, it does not make sense, and the
implementation is bad."
  :group 'org-link-follow
  :type 'boolean)

(defcustom org-return-follows-link nil
  "Non-nil means on links RET will follow the link."
  :group 'org-link-follow
  :type 'boolean)

(defcustom org-mouse-1-follows-link
  (if (boundp 'mouse-1-click-follows-link) mouse-1-click-follows-link t)
  "Non-nil means mouse-1 on a link will follow the link.
A longer mouse click will still set point.  Does not work on XEmacs.
Needs to be set before org.el is loaded."
  :group 'org-link-follow
  :type 'boolean)

(defcustom org-mark-ring-length 4
  "Number of different positions to be recorded in the ring.
Changing this requires a restart of Emacs to work correctly."
  :group 'org-link-follow
  :type 'integer)

(defcustom org-link-search-must-match-exact-headline 'query-to-create
  "Non-nil means internal links in Org files must exactly match a headline.
When nil, the link search tries to match a phrase with all words
in the search text."
  :group 'org-link-follow
  :version "24.1"
  :type '(choice
	  (const :tag "Use fuzzy text search" nil)
	  (const :tag "Match only exact headline" t)
	  (const :tag "Match exact headline or query to create it"
		 query-to-create)))

(defcustom org-link-frame-setup
  '((vm . vm-visit-folder-other-frame)
    (gnus . org-gnus-no-new-news)
    (file . find-file-other-window)
    (wl . wl-other-frame))
  "Setup the frame configuration for following links.
When following a link with Emacs, it may often be useful to display
this link in another window or frame.  This variable can be used to
set this up for the different types of links.
For VM, use any of
    `vm-visit-folder'
    `vm-visit-folder-other-window'
    `vm-visit-folder-other-frame'
For Gnus, use any of
    `gnus'
    `gnus-other-frame'
    `org-gnus-no-new-news'
For FILE, use any of
    `find-file'
    `find-file-other-window'
    `find-file-other-frame'
For Wanderlust use any of
    `wl'
    `wl-other-frame'
For the calendar, use the variable `calendar-setup'.
For BBDB, it is currently only possible to display the matches in
another window."
  :group 'org-link-follow
  :type '(list
	  (cons (const vm)
		(choice
		 (const vm-visit-folder)
		 (const vm-visit-folder-other-window)
		 (const vm-visit-folder-other-frame)))
	  (cons (const gnus)
		(choice
		 (const gnus)
		 (const gnus-other-frame)
		 (const org-gnus-no-new-news)))
	  (cons (const file)
		(choice
		 (const find-file)
		 (const find-file-other-window)
		 (const find-file-other-frame)))
	  (cons (const wl)
		(choice
		 (const wl)
		 (const wl-other-frame)))))

(defcustom org-display-internal-link-with-indirect-buffer nil
  "Non-nil means use indirect buffer to display infile links.
Activating internal links (from one location in a file to another location
in the same file) normally just jumps to the location.  When the link is
activated with a \\[universal-argument] prefix (or with mouse-3), the link \
is displayed in
another window.  When this option is set, the other window actually displays
an indirect buffer clone of the current buffer, to avoid any visibility
changes to the current buffer."
  :group 'org-link-follow
  :type 'boolean)

(defcustom org-open-non-existing-files nil
  "Non-nil means `org-open-file' will open non-existing files.
When nil, an error will be generated.
This variable applies only to external applications because they
might choke on non-existing files.  If the link is to a file that
will be opened in Emacs, the variable is ignored."
  :group 'org-link-follow
  :type 'boolean)

(defcustom org-open-directory-means-index-dot-org nil
  "Non-nil means a link to a directory really means to index.org.
When nil, following a directory link will run dired or open a finder/explorer
window on that directory."
  :group 'org-link-follow
  :type 'boolean)

(defcustom org-link-mailto-program '(browse-url "mailto:%a?subject=%s")
  "Function and arguments to call for following mailto links.
This is a list with the first element being a Lisp function, and the
remaining elements being arguments to the function.  In string arguments,
%a will be replaced by the address, and %s will be replaced by the subject
if one was given like in <mailto:arthur@galaxy.org::this subject>."
  :group 'org-link-follow
  :type '(choice
	  (const :tag "browse-url" (browse-url-mail "mailto:%a?subject=%s"))
	  (const :tag "compose-mail" (compose-mail "%a" "%s"))
	  (const :tag "message-mail" (message-mail "%a" "%s"))
	  (cons :tag "other" (function) (repeat :tag "argument" sexp))))

(defcustom org-confirm-shell-link-function 'yes-or-no-p
  "Non-nil means ask for confirmation before executing shell links.
Shell links can be dangerous: just think about a link

     [[shell:rm -rf ~/*][Google Search]]

This link would show up in your Org-mode document as \"Google Search\",
but really it would remove your entire home directory.
Therefore we advise against setting this variable to nil.
Just change it to `y-or-n-p' if you want to confirm with a
single keystroke rather than having to type \"yes\"."
  :group 'org-link-follow
  :type '(choice
	  (const :tag "with yes-or-no (safer)" yes-or-no-p)
	  (const :tag "with y-or-n (faster)" y-or-n-p)
	  (const :tag "no confirmation (dangerous)" nil)))
(put 'org-confirm-shell-link-function
     'safe-local-variable
     #'(lambda (x) (member x '(yes-or-no-p y-or-n-p))))

(defcustom org-confirm-shell-link-not-regexp ""
  "A regexp to skip confirmation for shell links."
  :group 'org-link-follow
  :version "24.1"
  :type 'regexp)

(defcustom org-confirm-elisp-link-function 'yes-or-no-p
  "Non-nil means ask for confirmation before executing Emacs Lisp links.
Elisp links can be dangerous: just think about a link

     [[elisp:(shell-command \"rm -rf ~/*\")][Google Search]]

This link would show up in your Org-mode document as \"Google Search\",
but really it would remove your entire home directory.
Therefore we advise against setting this variable to nil.
Just change it to `y-or-n-p' if you want to confirm with a
single keystroke rather than having to type \"yes\"."
  :group 'org-link-follow
  :type '(choice
	  (const :tag "with yes-or-no (safer)" yes-or-no-p)
	  (const :tag "with y-or-n (faster)" y-or-n-p)
	  (const :tag "no confirmation (dangerous)" nil)))
(put 'org-confirm-shell-link-function
     'safe-local-variable
     #'(lambda (x) (member x '(yes-or-no-p y-or-n-p))))

(defcustom org-confirm-elisp-link-not-regexp ""
  "A regexp to skip confirmation for Elisp links."
  :group 'org-link-follow
  :version "24.1"
  :type 'regexp)

(defconst org-file-apps-defaults-gnu
  '((remote . emacs)
    (system . mailcap)
    (t . mailcap))
  "Default file applications on a UNIX or GNU/Linux system.
See `org-file-apps'.")

(defconst org-file-apps-defaults-macosx
  '((remote . emacs)
    (t . "open %s")
    (system . "open %s")
    ("ps.gz"  . "gv %s")
    ("eps.gz" . "gv %s")
    ("dvi"    . "xdvi %s")
    ("fig"    . "xfig %s"))
  "Default file applications on a MacOS X system.
The system \"open\" is known as a default, but we use X11 applications
for some files for which the OS does not have a good default.
See `org-file-apps'.")

(defconst org-file-apps-defaults-windowsnt
  (list
   '(remote . emacs)
   (cons t
	 (list (if (featurep 'xemacs)
		   'mswindows-shell-execute
		 'w32-shell-execute)
	       "open" 'file))
   (cons 'system
	 (list (if (featurep 'xemacs)
		   'mswindows-shell-execute
		 'w32-shell-execute)
	       "open" 'file)))
  "Default file applications on a Windows NT system.
The system \"open\" is used for most files.
See `org-file-apps'.")

(defcustom org-file-apps
  '(
    (auto-mode . emacs)
    ("\\.mm\\'" . default)
    ("\\.x?html?\\'" . default)
    ("\\.pdf\\'" . default)
    )
  "External applications for opening `file:path' items in a document.
Org-mode uses system defaults for different file types, but
you can use this variable to set the application for a given file
extension.  The entries in this list are cons cells where the car identifies
files and the cdr the corresponding command.  Possible values for the
file identifier are
 \"string\"    A string as a file identifier can be interpreted in different
               ways, depending on its contents:

               - Alphanumeric characters only:
                 Match links with this file extension.
                 Example: (\"pdf\" . \"evince %s\")
                          to open PDFs with evince.

               - Regular expression: Match links where the
                 filename matches the regexp.  If you want to
                 use groups here, use shy groups.

                 Example: (\"\\.x?html\\'\" . \"firefox %s\")
                          (\"\\(?:xhtml\\|html\\)\" . \"firefox %s\")
                          to open *.html and *.xhtml with firefox.

               - Regular expression which contains (non-shy) groups:
                 Match links where the whole link, including \"::\", and
                 anything after that, matches the regexp.
                 In a custom command string, %1, %2, etc. are replaced with
                 the parts of the link that were matched by the groups.
                 For backwards compatibility, if a command string is given
                 that does not use any of the group matches, this case is
                 handled identically to the second one (i.e. match against
                 file name only).
                 In a custom lisp form, you can access the group matches with
                 (match-string n link).

                 Example: (\"\\.pdf::\\(\\d+\\)\\'\" . \"evince -p %1 %s\")
                     to open [[file:document.pdf::5]] with evince at page 5.

 `directory'   Matches a directory
 `remote'      Matches a remote file, accessible through tramp or efs.
               Remote files most likely should be visited through Emacs
               because external applications cannot handle such paths.
`auto-mode'    Matches files that are matched by any entry in `auto-mode-alist',
               so all files Emacs knows how to handle.  Using this with
               command `emacs' will open most files in Emacs.  Beware that this
               will also open html files inside Emacs, unless you add
               (\"html\" . default) to the list as well.
 t             Default for files not matched by any of the other options.
 `system'      The system command to open files, like `open' on Windows
               and Mac OS X, and mailcap under GNU/Linux.  This is the command
               that will be selected if you call `C-c C-o' with a double
               \\[universal-argument] \\[universal-argument] prefix.

Possible values for the command are:
 `emacs'       The file will be visited by the current Emacs process.
 `default'     Use the default application for this file type, which is the
               association for t in the list, most likely in the system-specific
               part.
               This can be used to overrule an unwanted setting in the
               system-specific variable.
 `system'      Use the system command for opening files, like \"open\".
               This command is specified by the entry whose car is `system'.
               Most likely, the system-specific version of this variable
               does define this command, but you can overrule/replace it
               here.
 string        A command to be executed by a shell; %s will be replaced
               by the path to the file.
 sexp          A Lisp form which will be evaluated.  The file path will
               be available in the Lisp variable `file'.
For more examples, see the system specific constants
`org-file-apps-defaults-macosx'
`org-file-apps-defaults-windowsnt'
`org-file-apps-defaults-gnu'."
  :group 'org-link-follow
  :type '(repeat
	  (cons (choice :value ""
			(string :tag "Extension")
			(const :tag "System command to open files" system)
			(const :tag "Default for unrecognized files" t)
			(const :tag "Remote file" remote)
			(const :tag "Links to a directory" directory)
			(const :tag "Any files that have Emacs modes"
			       auto-mode))
		(choice :value ""
			(const :tag "Visit with Emacs" emacs)
			(const :tag "Use default" default)
			(const :tag "Use the system command" system)
			(string :tag "Command")
			(sexp :tag "Lisp form")))))



(defgroup org-refile nil
  "Options concerning refiling entries in Org-mode."
  :tag "Org Refile"
  :group 'org)

(defcustom org-directory "~/org"
  "Directory with org files.
This is just a default location to look for Org files.  There is no need
at all to put your files into this directory.  It is only used in the
following situations:

1. When a remember template specifies a target file that is not an
   absolute path.  The path will then be interpreted relative to
   `org-directory'
2. When a remember note is filed away in an interactive way (when exiting the
   note buffer with `C-1 C-c C-c'.  The user is prompted for an org file,
   with `org-directory' as the default path."
  :group 'org-refile
  :group 'org-remember
  :type 'directory)

(defcustom org-default-notes-file (convert-standard-filename "~/.notes")
  "Default target for storing notes.
Used as a fall back file for org-remember.el and org-capture.el, for
templates that do not specify a target file."
  :group 'org-refile
  :group 'org-remember
  :type '(choice
	  (const :tag "Default from remember-data-file" nil)
	  file))

(defcustom org-goto-interface 'outline
  "The default interface to be used for `org-goto'.
Allowed values are:
outline                  The interface shows an outline of the relevant file
                         and the correct heading is found by moving through
                         the outline or by searching with incremental search.
outline-path-completion  Headlines in the current buffer are offered via
                         completion.  This is the interface also used by
                         the refile command."
  :group 'org-refile
  :type '(choice
	  (const :tag "Outline" outline)
	  (const :tag "Outline-path-completion" outline-path-completion)))

(defcustom org-goto-max-level 5
  "Maximum target level when running `org-goto' with refile interface."
  :group 'org-refile
  :type 'integer)

(defcustom org-reverse-note-order nil
  "Non-nil means store new notes at the beginning of a file or entry.
When nil, new notes will be filed to the end of a file or entry.
This can also be a list with cons cells of regular expressions that
are matched against file names, and values."
  :group 'org-remember
  :group 'org-refile
  :type '(choice
	  (const :tag "Reverse always" t)
	  (const :tag "Reverse never" nil)
	  (repeat :tag "By file name regexp"
		  (cons regexp boolean))))

(defcustom org-log-refile nil
  "Information to record when a task is refiled.

Possible values are:

nil     Don't add anything
time    Add a time stamp to the task
note    Prompt for a note and add it with template `org-log-note-headings'

This option can also be set with on a per-file-basis with

   #+STARTUP: nologrefile
   #+STARTUP: logrefile
   #+STARTUP: lognoterefile

You can have local logging settings for a subtree by setting the LOGGING
property to one or more of these keywords.

When bulk-refiling from the agenda, the value `note' is forbidden and
will temporarily be changed to `time'."
  :group 'org-refile
  :group 'org-progress
  :version "24.1"
  :type '(choice
	  (const :tag "No logging" nil)
	  (const :tag "Record timestamp" time)
	  (const :tag "Record timestamp with note." note)))

(defcustom org-refile-targets nil
  "Targets for refiling entries with \\[org-refile].
This is a list of cons cells.  Each cell contains:
- a specification of the files to be considered, either a list of files,
  or a symbol whose function or variable value will be used to retrieve
  a file name or a list of file names.  If you use `org-agenda-files' for
  that, all agenda files will be scanned for targets.  Nil means consider
  headings in the current buffer.
- A specification of how to find candidate refile targets.  This may be
  any of:
  - a cons cell (:tag . \"TAG\") to identify refile targets by a tag.
    This tag has to be present in all target headlines, inheritance will
    not be considered.
  - a cons cell (:todo . \"KEYWORD\") to identify refile targets by
    todo keyword.
  - a cons cell (:regexp . \"REGEXP\") with a regular expression matching
    headlines that are refiling targets.
  - a cons cell (:level . N).  Any headline of level N is considered a target.
    Note that, when `org-odd-levels-only' is set, level corresponds to
    order in hierarchy, not to the number of stars.
  - a cons cell (:maxlevel . N).  Any headline with level <= N is a target.
    Note that, when `org-odd-levels-only' is set, level corresponds to
    order in hierarchy, not to the number of stars.

Each element of this list generates a set of possible targets.
The union of these sets is presented (with completion) to
the user by `org-refile'.

You can set the variable `org-refile-target-verify-function' to a function
to verify each headline found by the simple criteria above.

When this variable is nil, all top-level headlines in the current buffer
are used, equivalent to the value `((nil . (:level . 1))'."
  :group 'org-refile
  :type '(repeat
	  (cons
	   (choice :value org-agenda-files
		   (const :tag "All agenda files" org-agenda-files)
		   (const :tag "Current buffer" nil)
		   (function) (variable) (file))
	   (choice :tag "Identify target headline by"
	    (cons :tag "Specific tag" (const :value :tag) (string))
	    (cons :tag "TODO keyword" (const :value :todo) (string))
	    (cons :tag "Regular expression" (const :value :regexp) (regexp))
	    (cons :tag "Level number" (const :value :level) (integer))
	    (cons :tag "Max Level number" (const :value :maxlevel) (integer))))))

(defcustom org-refile-target-verify-function nil
  "Function to verify if the headline at point should be a refile target.
The function will be called without arguments, with point at the
beginning of the headline.  It should return t and leave point
where it is if the headline is a valid target for refiling.

If the target should not be selected, the function must return nil.
In addition to this, it may move point to a place from where the search
should be continued.  For example, the function may decide that the entire
subtree of the current entry should be excluded and move point to the end
of the subtree."
  :group 'org-refile
  :type 'function)

(defcustom org-refile-use-cache nil
  "Non-nil means cache refile targets to speed up the process.
The cache for a particular file will be updated automatically when
the buffer has been killed, or when any of the marker used for flagging
refile targets no longer points at a live buffer.
If you have added new entries to a buffer that might themselves be targets,
you need to clear the cache manually by pressing `C-0 C-c C-w' or, if you
find that easier, `C-u C-u C-u C-c C-w'."
  :group 'org-refile
  :version "24.1"
  :type 'boolean)

(defcustom org-refile-use-outline-path nil
  "Non-nil means provide refile targets as paths.
So a level 3 headline will be available as level1/level2/level3.

When the value is `file', also include the file name (without directory)
into the path.  In this case, you can also stop the completion after
the file name, to get entries inserted as top level in the file.

When `full-file-path', include the full file path."
  :group 'org-refile
  :type '(choice
	  (const :tag "Not" nil)
	  (const :tag "Yes" t)
	  (const :tag "Start with file name" file)
	  (const :tag "Start with full file path" full-file-path)))

(defcustom org-outline-path-complete-in-steps t
  "Non-nil means complete the outline path in hierarchical steps.
When Org-mode uses the refile interface to select an outline path
\(see variable `org-refile-use-outline-path'), the completion of
the path can be done is a single go, or if can be done in steps down
the headline hierarchy.  Going in steps is probably the best if you
do not use a special completion package like `ido' or `icicles'.
However, when using these packages, going in one step can be very
fast, while still showing the whole path to the entry."
  :group 'org-refile
  :type 'boolean)

(defcustom org-refile-allow-creating-parent-nodes nil
  "Non-nil means allow to create new nodes as refile targets.
New nodes are then created by adding \"/new node name\" to the completion
of an existing node.  When the value of this variable is `confirm',
new node creation must be confirmed by the user (recommended)
When nil, the completion must match an existing entry.

Note that, if the new heading is not seen by the criteria
listed in `org-refile-targets', multiple instances of the same
heading would be created by trying again to file under the new
heading."
  :group 'org-refile
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "Always" t)
	  (const :tag "Prompt for confirmation" confirm)))

(defcustom org-refile-active-region-within-subtree nil
  "Non-nil means also refile active region within a subtree.

By default `org-refile' doesn't allow refiling regions if they
don't contain a set of subtrees, but it might be convenient to
do so sometimes: in that case, the first line of the region is
converted to a headline before refiling."
  :group 'org-refile
  :version "24.1"
  :type 'boolean)

(defgroup org-todo nil
  "Options concerning TODO items in Org-mode."
  :tag "Org TODO"
  :group 'org)

(defgroup org-progress nil
  "Options concerning Progress logging in Org-mode."
  :tag "Org Progress"
  :group 'org-time)

(defvar org-todo-interpretation-widgets
  '((:tag "Sequence (cycling hits every state)" sequence)
    (:tag "Type     (cycling directly to DONE)" type))
  "The available interpretation symbols for customizing `org-todo-keywords'.
Interested libraries should add to this list.")

(defcustom org-todo-keywords '((sequence "TODO" "DONE"))
  "List of TODO entry keyword sequences and their interpretation.
\\<org-mode-map>This is a list of sequences.

Each sequence starts with a symbol, either `sequence' or `type',
indicating if the keywords should be interpreted as a sequence of
action steps, or as different types of TODO items.  The first
keywords are states requiring action - these states will select a headline
for inclusion into the global TODO list Org-mode produces.  If one of
the \"keywords\" is the vertical bar, \"|\", the remaining keywords
signify that no further action is necessary.  If \"|\" is not found,
the last keyword is treated as the only DONE state of the sequence.

The command \\[org-todo] cycles an entry through these states, and one
additional state where no keyword is present.  For details about this
cycling, see the manual.

TODO keywords and interpretation can also be set on a per-file basis with
the special #+SEQ_TODO and #+TYP_TODO lines.

Each keyword can optionally specify a character for fast state selection
\(in combination with the variable `org-use-fast-todo-selection')
and specifiers for state change logging, using the same syntax
that is used in the \"#+TODO:\" lines.  For example, \"WAIT(w)\" says
that the WAIT state can be selected with the \"w\" key.  \"WAIT(w!)\"
indicates to record a time stamp each time this state is selected.

Each keyword may also specify if a timestamp or a note should be
recorded when entering or leaving the state, by adding additional
characters in the parenthesis after the keyword.  This looks like this:
\"WAIT(w@/!)\".  \"@\" means to add a note (with time), \"!\" means to
record only the time of the state change.  With X and Y being either
\"@\" or \"!\", \"X/Y\" means use X when entering the state, and use
Y when leaving the state if and only if the *target* state does not
define X.  You may omit any of the fast-selection key or X or /Y,
so WAIT(w@), WAIT(w/@) and WAIT(@/@) are all valid.

For backward compatibility, this variable may also be just a list
of keywords - in this case the interpretation (sequence or type) will be
taken from the (otherwise obsolete) variable `org-todo-interpretation'."
  :group 'org-todo
  :group 'org-keywords
  :type '(choice
	  (repeat :tag "Old syntax, just keywords"
		  (string :tag "Keyword"))
	  (repeat :tag "New syntax"
		  (cons
		   (choice
		    :tag "Interpretation"
		    ;;Quick and dirty way to see
		    ;;`org-todo-interpretations'.  This takes the
		    ;;place of item arguments
		    :convert-widget
		    (lambda (widget)
		      (widget-put widget
				  :args (mapcar
					 #'(lambda (x)
					     (widget-convert
					      (cons 'const x)))
					 org-todo-interpretation-widgets))
		      widget))
		   (repeat
		    (string :tag "Keyword"))))))

(defvar org-todo-keywords-1 nil
  "All TODO and DONE keywords active in a buffer.")
(make-variable-buffer-local 'org-todo-keywords-1)
(defvar org-todo-keywords-for-agenda nil)
(defvar org-done-keywords-for-agenda nil)
(defvar org-drawers-for-agenda nil)
(defvar org-todo-keyword-alist-for-agenda nil)
(defvar org-tag-alist-for-agenda nil)
(defvar org-agenda-contributing-files nil)
(defvar org-not-done-keywords nil)
(make-variable-buffer-local 'org-not-done-keywords)
(defvar org-done-keywords nil)
(make-variable-buffer-local 'org-done-keywords)
(defvar org-todo-heads nil)
(make-variable-buffer-local 'org-todo-heads)
(defvar org-todo-sets nil)
(make-variable-buffer-local 'org-todo-sets)
(defvar org-todo-log-states nil)
(make-variable-buffer-local 'org-todo-log-states)
(defvar org-todo-kwd-alist nil)
(make-variable-buffer-local 'org-todo-kwd-alist)
(defvar org-todo-key-alist nil)
(make-variable-buffer-local 'org-todo-key-alist)
(defvar org-todo-key-trigger nil)
(make-variable-buffer-local 'org-todo-key-trigger)

(defcustom org-todo-interpretation 'sequence
  "Controls how TODO keywords are interpreted.
This variable is in principle obsolete and is only used for
backward compatibility, if the interpretation of todo keywords is
not given already in `org-todo-keywords'.  See that variable for
more information."
  :group 'org-todo
  :group 'org-keywords
  :type '(choice (const sequence)
		 (const type)))

(defcustom org-use-fast-todo-selection t
  "Non-nil means use the fast todo selection scheme with C-c C-t.
This variable describes if and under what circumstances the cycling
mechanism for TODO keywords will be replaced by a single-key, direct
selection scheme.

When nil, fast selection is never used.

When the symbol `prefix', it will be used when `org-todo' is called with
a prefix argument,  i.e. `C-u C-c C-t' in an Org-mode buffer, and `C-u t'
in an agenda buffer.

When t, fast selection is used by default.  In this case, the prefix
argument forces cycling instead.

In all cases, the special interface is only used if access keys have actually
been assigned by the user, i.e. if keywords in the configuration are followed
by a letter in parenthesis, like TODO(t)."
  :group 'org-todo
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "By default" t)
	  (const :tag "Only with C-u C-c C-t" prefix)))

(defcustom org-provide-todo-statistics t
  "Non-nil means update todo statistics after insert and toggle.
ALL-HEADLINES means update todo statistics by including headlines
with no TODO keyword as well, counting them as not done.
A list of TODO keywords means the same, but skip keywords that are
not in this list.

When this is set, todo statistics is updated in the parent of the
current entry each time a todo state is changed."
  :group 'org-todo
  :type '(choice
	  (const :tag "Yes, only for TODO entries" t)
	  (const :tag "Yes, including all entries" 'all-headlines)
	  (repeat :tag "Yes, for TODOs in this list"
		  (string :tag "TODO keyword"))
	  (other :tag "No TODO statistics" nil)))

(defcustom org-hierarchical-todo-statistics t
  "Non-nil means TODO statistics covers just direct children.
When nil, all entries in the subtree are considered.
This has only an effect if `org-provide-todo-statistics' is set.
To set this to nil for only a single subtree, use a COOKIE_DATA
property and include the word \"recursive\" into the value."
  :group 'org-todo
  :type 'boolean)

(defcustom org-after-todo-state-change-hook nil
  "Hook which is run after the state of a TODO item was changed.
The new state (a string with a TODO keyword, or nil) is available in the
Lisp variable `org-state'."
  :group 'org-todo
  :type 'hook)

(defvar org-blocker-hook nil
  "Hook for functions that are allowed to block a state change.

Each function gets as its single argument a property list, see
`org-trigger-hook' for more information about this list.

If any of the functions in this hook returns nil, the state change
is blocked.")

(defvar org-trigger-hook nil
  "Hook for functions that are triggered by a state change.

Each function gets as its single argument a property list with at least
the following elements:

 (:type type-of-change :position pos-at-entry-start
  :from old-state :to new-state)

Depending on the type, more properties may be present.

This mechanism is currently implemented for:

TODO state changes
------------------
:type  todo-state-change
:from  previous state (keyword as a string), or nil, or a symbol
       'todo' or 'done', to indicate the general type of state.
:to    new state, like in :from")

(defcustom org-enforce-todo-dependencies nil
  "Non-nil means undone TODO entries will block switching the parent to DONE.
Also, if a parent has an :ORDERED: property, switching an entry to DONE will
be blocked if any prior sibling is not yet done.
Finally, if the parent is blocked because of ordered siblings of its own,
the child will also be blocked."
  :set (lambda (var val)
	 (set var val)
	 (if val
	     (add-hook 'org-blocker-hook
		       'org-block-todo-from-children-or-siblings-or-parent)
	   (remove-hook 'org-blocker-hook
			'org-block-todo-from-children-or-siblings-or-parent)))
  :group 'org-todo
  :type 'boolean)

(defcustom org-enforce-todo-checkbox-dependencies nil
  "Non-nil means unchecked boxes will block switching the parent to DONE.
When this is nil, checkboxes have no influence on switching TODO states.
When non-nil, you first need to check off all check boxes before the TODO
entry can be switched to DONE.
This variable needs to be set before org.el is loaded, and you need to
restart Emacs after a change to make the change effective.  The only way
to change is while Emacs is running is through the customize interface."
  :set (lambda (var val)
	 (set var val)
	 (if val
	     (add-hook 'org-blocker-hook
		       'org-block-todo-from-checkboxes)
	   (remove-hook 'org-blocker-hook
			'org-block-todo-from-checkboxes)))
  :group 'org-todo
  :type 'boolean)

(defcustom org-treat-insert-todo-heading-as-state-change nil
  "Non-nil means inserting a TODO heading is treated as state change.
So when the command \\[org-insert-todo-heading] is used, state change
logging will apply if appropriate.  When nil, the new TODO item will
be inserted directly, and no logging will take place."
  :group 'org-todo
  :type 'boolean)

(defcustom org-treat-S-cursor-todo-selection-as-state-change t
  "Non-nil means switching TODO states with S-cursor counts as state change.
This is the default behavior.  However, setting this to nil allows a
convenient way to select a TODO state and bypass any logging associated
with that."
  :group 'org-todo
  :type 'boolean)

(defcustom org-todo-state-tags-triggers nil
  "Tag changes that should be triggered by TODO state changes.
This is a list.  Each entry is

  (state-change (tag . flag) .......)

State-change can be a string with a state, and empty string to indicate the
state that has no TODO keyword, or it can be one of the symbols `todo'
or `done', meaning any not-done or done state, respectively."
  :group 'org-todo
  :group 'org-tags
  :type '(repeat
	  (cons (choice :tag "When changing to"
		 (const :tag "Not-done state" todo)
		 (const :tag "Done state" done)
		 (string :tag "State"))
		(repeat
		 (cons :tag "Tag action"
		       (string :tag "Tag")
		       (choice (const :tag "Add" t) (const :tag "Remove" nil)))))))

(defcustom org-log-done nil
  "Information to record when a task moves to the DONE state.

Possible values are:

nil     Don't add anything, just change the keyword
time    Add a time stamp to the task
note    Prompt for a note and add it with template `org-log-note-headings'

This option can also be set with on a per-file-basis with

   #+STARTUP: nologdone
   #+STARTUP: logdone
   #+STARTUP: lognotedone

You can have local logging settings for a subtree by setting the LOGGING
property to one or more of these keywords."
  :group 'org-todo
  :group 'org-progress
  :type '(choice
	  (const :tag "No logging" nil)
	  (const :tag "Record CLOSED timestamp" time)
	  (const :tag "Record CLOSED timestamp with note." note)))

;; Normalize old uses of org-log-done.
(cond
 ((eq org-log-done t) (setq org-log-done 'time))
 ((and (listp org-log-done) (memq 'done org-log-done))
  (setq org-log-done 'note)))

(defcustom org-log-reschedule nil
  "Information to record when the scheduling date of a tasks is modified.

Possible values are:

nil     Don't add anything, just change the date
time    Add a time stamp to the task
note    Prompt for a note and add it with template `org-log-note-headings'

This option can also be set with on a per-file-basis with

   #+STARTUP: nologreschedule
   #+STARTUP: logreschedule
   #+STARTUP: lognotereschedule"
  :group 'org-todo
  :group 'org-progress
  :type '(choice
	  (const :tag "No logging" nil)
	  (const :tag "Record timestamp" time)
	  (const :tag "Record timestamp with note." note)))

(defcustom org-log-redeadline nil
  "Information to record when the deadline date of a tasks is modified.

Possible values are:

nil     Don't add anything, just change the date
time    Add a time stamp to the task
note    Prompt for a note and add it with template `org-log-note-headings'

This option can also be set with on a per-file-basis with

   #+STARTUP: nologredeadline
   #+STARTUP: logredeadline
   #+STARTUP: lognoteredeadline

You can have local logging settings for a subtree by setting the LOGGING
property to one or more of these keywords."
  :group 'org-todo
  :group 'org-progress
  :type '(choice
	  (const :tag "No logging" nil)
	  (const :tag "Record timestamp" time)
	  (const :tag "Record timestamp with note." note)))

(defcustom org-log-note-clock-out nil
  "Non-nil means record a note when clocking out of an item.
This can also be configured on a per-file basis by adding one of
the following lines anywhere in the buffer:

   #+STARTUP: lognoteclock-out
   #+STARTUP: nolognoteclock-out"
  :group 'org-todo
  :group 'org-progress
  :type 'boolean)

(defcustom org-log-done-with-time t
  "Non-nil means the CLOSED time stamp will contain date and time.
When nil, only the date will be recorded."
  :group 'org-progress
  :type 'boolean)

(defcustom org-log-note-headings
  '((done .  "CLOSING NOTE %t")
    (state . "State %-12s from %-12S %t")
    (note .  "Note taken on %t")
    (reschedule .  "Rescheduled from %S on %t")
    (delschedule .  "Not scheduled, was %S on %t")
    (redeadline .  "New deadline from %S on %t")
    (deldeadline .  "Removed deadline, was %S on %t")
    (refile . "Refiled on %t")
    (clock-out . ""))
  "Headings for notes added to entries.
The value is an alist, with the car being a symbol indicating the note
context, and the cdr is the heading to be used.  The heading may also be the
empty string.
%t in the heading will be replaced by a time stamp.
%T will be an active time stamp instead the default inactive one
%d will be replaced by a short-format time stamp.
%D will be replaced by an active short-format time stamp.
%s will be replaced by the new TODO state, in double quotes.
%S will be replaced by the old TODO state, in double quotes.
%u will be replaced by the user name.
%U will be replaced by the full user name.

In fact, it is not a good idea to change the `state' entry, because
agenda log mode depends on the format of these entries."
  :group  'org-todo
  :group  'org-progress
  :type '(list :greedy t
	  (cons (const :tag "Heading when closing an item" done) string)
	  (cons (const :tag
		       "Heading when changing todo state (todo sequence only)"
		       state) string)
	  (cons (const :tag "Heading when just taking a note" note) string)
	  (cons (const :tag "Heading when clocking out" clock-out) string)
	  (cons (const :tag "Heading when an item is no longer scheduled" delschedule) string)
	  (cons (const :tag "Heading when rescheduling" reschedule) string)
	  (cons (const :tag "Heading when changing deadline"  redeadline) string)
	  (cons (const :tag "Heading when deleting a deadline" deldeadline) string)
	  (cons (const :tag "Heading when refiling" refile) string)))

(unless (assq 'note org-log-note-headings)
  (push '(note . "%t") org-log-note-headings))

(defcustom org-log-into-drawer nil
  "Non-nil means insert state change notes and time stamps into a drawer.
When nil, state changes notes will be inserted after the headline and
any scheduling and clock lines, but not inside a drawer.

The value of this variable should be the name of the drawer to use.
LOGBOOK is proposed as the default drawer for this purpose, you can
also set this to a string to define the drawer of your choice.

A value of t is also allowed, representing \"LOGBOOK\".

If this variable is set, `org-log-state-notes-insert-after-drawers'
will be ignored.

You can set the property LOG_INTO_DRAWER to overrule this setting for
a subtree."
  :group 'org-todo
  :group 'org-progress
  :type '(choice
	  (const :tag "Not into a drawer" nil)
	  (const :tag "LOGBOOK" t)
	  (string :tag "Other")))

(if (fboundp 'defvaralias)
    (defvaralias 'org-log-state-notes-into-drawer 'org-log-into-drawer))

(defun org-log-into-drawer ()
  "Return the value of `org-log-into-drawer', but let properties overrule.
If the current entry has or inherits a LOG_INTO_DRAWER property, it will be
used instead of the default value."
  (let ((p (org-entry-get nil "LOG_INTO_DRAWER" 'inherit)))
    (cond
     ((or (not p) (equal p "nil")) org-log-into-drawer)
     ((equal p "t") "LOGBOOK")
     (t p))))

(defcustom org-log-state-notes-insert-after-drawers nil
  "Non-nil means insert state change notes after any drawers in entry.
Only the drawers that *immediately* follow the headline and the
deadline/scheduled line are skipped.
When nil, insert notes right after the heading and perhaps the line
with deadline/scheduling if present.

This variable will have no effect if `org-log-into-drawer' is
set."
  :group 'org-todo
  :group 'org-progress
  :type 'boolean)

(defcustom org-log-states-order-reversed t
  "Non-nil means the latest state note will be directly after heading.
When nil, the state change notes will be ordered according to time."
  :group 'org-todo
  :group 'org-progress
  :type 'boolean)

(defcustom org-todo-repeat-to-state nil
  "The TODO state to which a repeater should return the repeating task.
By default this is the first task in a TODO sequence, or the previous state
in a TODO_TYP set.  But you can specify another task here.
alternatively, set the :REPEAT_TO_STATE: property of the entry."
  :group 'org-todo
  :version "24.1"
  :type '(choice (const :tag "Head of sequence" nil)
		 (string :tag "Specific state")))

(defcustom org-log-repeat 'time
  "Non-nil means record moving through the DONE state when triggering repeat.
An auto-repeating task is immediately switched back to TODO when
marked DONE.  If you are not logging state changes (by adding \"@\"
or \"!\" to the TODO keyword definition), or set `org-log-done' to
record a closing note, there will be no record of the task moving
through DONE.  This variable forces taking a note anyway.

nil     Don't force a record
time    Record a time stamp
note    Record a note

This option can also be set with on a per-file-basis with

   #+STARTUP: logrepeat
   #+STARTUP: lognoterepeat
   #+STARTUP: nologrepeat

You can have local logging settings for a subtree by setting the LOGGING
property to one or more of these keywords."
  :group 'org-todo
  :group 'org-progress
  :type '(choice
	  (const :tag "Don't force a record" nil)
	  (const :tag "Force recording the DONE state" time)
	  (const :tag "Force recording a note with the DONE state" note)))


(defgroup org-priorities nil
  "Priorities in Org-mode."
  :tag "Org Priorities"
  :group 'org-todo)

(defcustom org-enable-priority-commands t
  "Non-nil means priority commands are active.
When nil, these commands will be disabled, so that you never accidentally
set a priority."
  :group 'org-priorities
  :type 'boolean)

(defcustom org-highest-priority ?A
  "The highest priority of TODO items.  A character like ?A, ?B etc.
Must have a smaller ASCII number than `org-lowest-priority'."
  :group 'org-priorities
  :type 'character)

(defcustom org-lowest-priority ?C
  "The lowest priority of TODO items.  A character like ?A, ?B etc.
Must have a larger ASCII number than `org-highest-priority'."
  :group 'org-priorities
  :type 'character)

(defcustom org-default-priority ?B
  "The default priority of TODO items.
This is the priority an item gets if no explicit priority is given.
When starting to cycle on an empty priority the first step in the cycle
depends on `org-priority-start-cycle-with-default'.  The resulting first
step priority must not exceed the range from `org-highest-priority' to
`org-lowest-priority' which means that `org-default-priority' has to be
in this range exclusive or inclusive the range boundaries.  Else the
first step refuses to set the default and the second will fall back
to (depending on the command used) the highest or lowest priority."
  :group 'org-priorities
  :type 'character)

(defcustom org-priority-start-cycle-with-default t
  "Non-nil means start with default priority when starting to cycle.
When this is nil, the first step in the cycle will be (depending on the
command used) one higher or lower than the default priority.
See also `org-default-priority'."
  :group 'org-priorities
  :type 'boolean)

(defcustom org-get-priority-function nil
  "Function to extract the priority from a string.
The string is normally the headline.  If this is nil Org computes the
priority from the priority cookie like [#A] in the headline.  It returns
an integer, increasing by 1000 for each priority level.
The user can set a different function here, which should take a string
as an argument and return the numeric priority."
  :group 'org-priorities
  :version "24.1"
  :type 'function)

(defgroup org-time nil
  "Options concerning time stamps and deadlines in Org-mode."
  :tag "Org Time"
  :group 'org)

(defcustom org-insert-labeled-timestamps-at-point nil
  "Non-nil means SCHEDULED and DEADLINE timestamps are inserted at point.
When nil, these labeled time stamps are forces into the second line of an
entry, just after the headline.  When scheduling from the global TODO list,
the time stamp will always be forced into the second line."
  :group 'org-time
  :type 'boolean)

(defconst org-time-stamp-formats '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M>")
  "Formats for `format-time-string' which are used for time stamps.
It is not recommended to change this constant.")

(defcustom org-time-stamp-rounding-minutes '(0 5)
  "Number of minutes to round time stamps to.
These are two values, the first applies when first creating a time stamp.
The second applies when changing it with the commands `S-up' and `S-down'.
When changing the time stamp, this means that it will change in steps
of N minutes, as given by the second value.

When a setting is 0 or 1, insert the time unmodified.  Useful rounding
numbers should be factors of 60, so for example 5, 10, 15.

When this is larger than 1, you can still force an exact time stamp by using
a double prefix argument to a time stamp command like `C-c .' or `C-c !',
and by using a prefix arg to `S-up/down' to specify the exact number
of minutes to shift."
  :group 'org-time
  :get #'(lambda (var) ; Make sure both elements are there
	  (if (integerp (default-value var))
	      (list (default-value var) 5)
	    (default-value var)))
  :type '(list
	  (integer :tag "when inserting times")
	  (integer :tag "when modifying times")))

;; Normalize old customizations of this variable.
(when (integerp org-time-stamp-rounding-minutes)
  (setq org-time-stamp-rounding-minutes
	(list org-time-stamp-rounding-minutes
	      org-time-stamp-rounding-minutes)))

(defcustom org-display-custom-times nil
  "Non-nil means overlay custom formats over all time stamps.
The formats are defined through the variable `org-time-stamp-custom-formats'.
To turn this on on a per-file basis, insert anywhere in the file:
   #+STARTUP: customtime"
  :group 'org-time
  :set 'set-default
  :type 'sexp)
(make-variable-buffer-local 'org-display-custom-times)

(defcustom org-time-stamp-custom-formats
  '("<%m/%d/%y %a>" . "<%m/%d/%y %a %H:%M>") ; american
  "Custom formats for time stamps.  See `format-time-string' for the syntax.
These are overlaid over the default ISO format if the variable
`org-display-custom-times' is set.  Time like %H:%M should be at the
end of the second format.  The custom formats are also honored by export
commands, if custom time display is turned on at the time of export."
  :group 'org-time
  :type 'sexp)

(defun org-time-stamp-format (&optional long inactive)
  "Get the right format for a time string."
  (let ((f (if long (cdr org-time-stamp-formats)
	     (car org-time-stamp-formats))))
    (if inactive
	(concat "[" (substring f 1 -1) "]")
      f)))

(defcustom org-time-clocksum-format "%d:%02d"
  "The format string used when creating CLOCKSUM lines.
This is also used when org-mode generates a time duration."
  :group 'org-time
  :type 'string)

(defcustom org-time-clocksum-use-fractional nil
  "If non-nil, \\[org-clock-display] uses fractional times.
org-mode generates a time duration."
  :group 'org-time
  :type 'boolean)

(defcustom org-time-clocksum-fractional-format "%.2f"
  "The format string used when creating CLOCKSUM lines, or when
org-mode generates a time duration."
  :group 'org-time
  :type 'string)

(defcustom org-deadline-warning-days 14
  "No. of days before expiration during which a deadline becomes active.
This variable governs the display in sparse trees and in the agenda.
When 0 or negative, it means use this number (the absolute value of it)
even if a deadline has a different individual lead time specified.

Custom commands can set this variable in the options section."
  :group 'org-time
  :group 'org-agenda-daily/weekly
  :type 'integer)

(defcustom org-read-date-prefer-future t
  "Non-nil means assume future for incomplete date input from user.
This affects the following situations:
1. The user gives a month but not a year.
   For example, if it is April and you enter \"feb 2\", this will be read
   as Feb 2, *next* year.  \"May 5\", however, will be this year.
2. The user gives a day, but no month.
   For example, if today is the 15th, and you enter \"3\", Org-mode will
   read this as the third of *next* month.  However, if you enter \"17\",
   it will be considered as *this* month.

If you set this variable to the symbol `time', then also the following
will work:

3. If the user gives a time, but no day.  If the time is before now,
   to will be interpreted as tomorrow.

Currently none of this works for ISO week specifications.

When this option is nil, the current day, month and year will always be
used as defaults.

See also `org-agenda-jump-prefer-future'."
  :group 'org-time
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "Check month and day" t)
	  (const :tag "Check month, day, and time" time)))

(defcustom org-agenda-jump-prefer-future 'org-read-date-prefer-future
  "Should the agenda jump command prefer the future for incomplete dates?
The default is to do the same as configured in `org-read-date-prefer-future'.
But you can also set a deviating value here.
This may t or nil, or the symbol `org-read-date-prefer-future'."
  :group 'org-agenda
  :group 'org-time
  :version "24.1"
  :type '(choice
	  (const :tag "Use org-read-date-prefer-future"
		 org-read-date-prefer-future)
	  (const :tag "Never" nil)
	  (const :tag "Always" t)))

(defcustom org-read-date-force-compatible-dates t
  "Should date/time prompt force dates that are guaranteed to work in Emacs?

Depending on the system Emacs is running on, certain dates cannot
be represented with the type used internally to represent time.
Dates between 1970-1-1 and 2038-1-1 can always be represented
correctly.  Some systems allow for earlier dates, some for later,
some for both.  One way to find out it to insert any date into an
Org buffer, putting the cursor on the year and hitting S-up and
S-down to test the range.

When this variable is set to t, the date/time prompt will not let
you specify dates outside the 1970-2037 range, so it is certain that
these dates will work in whatever version of Emacs you are
running, and also that you can move a file from one Emacs implementation
to another.  WHenever Org is forcing the year for you, it will display
a message and beep.

When this variable is nil, Org will check if the date is
representable in the specific Emacs implementation you are using.
If not, it will force a year, usually the current year, and beep
to remind you.  Currently this setting is not recommended because
the likelihood that you will open your Org files in an Emacs that
has limited date range is not negligible.

A workaround for this problem is to use diary sexp dates for time
stamps outside of this range."
  :group 'org-time
  :version "24.1"
  :type 'boolean)

(defcustom org-read-date-display-live t
  "Non-nil means display current interpretation of date prompt live.
This display will be in an overlay, in the minibuffer."
  :group 'org-time
  :type 'boolean)

(defcustom org-read-date-popup-calendar t
  "Non-nil means pop up a calendar when prompting for a date.
In the calendar, the date can be selected with mouse-1.  However, the
minibuffer will also be active, and you can simply enter the date as well.
When nil, only the minibuffer will be available."
  :group 'org-time
  :type 'boolean)
(if (fboundp 'defvaralias)
    (defvaralias 'org-popup-calendar-for-date-prompt
      'org-read-date-popup-calendar))

(defcustom org-read-date-minibuffer-setup-hook nil
  "Hook to be used to set up keys for the date/time interface.
Add key definitions to `minibuffer-local-map', which will be a temporary
copy."
  :group 'org-time
  :type 'hook)

(defcustom org-extend-today-until 0
  "The hour when your day really ends.  Must be an integer.
This has influence for the following applications:
- When switching the agenda to \"today\".  It it is still earlier than
  the time given here, the day recognized as TODAY is actually yesterday.
- When a date is read from the user and it is still before the time given
  here, the current date and time will be assumed to be yesterday, 23:59.
  Also, timestamps inserted in remember templates follow this rule.

IMPORTANT:  This is a feature whose implementation is and likely will
remain incomplete.  Really, it is only here because past midnight seems to
be the favorite working time of John Wiegley :-)"
  :group 'org-time
  :type 'integer)

(defcustom org-use-effective-time nil
  "If non-nil, consider `org-extend-today-until' when creating timestamps.
For example, if `org-extend-today-until' is 8, and it's 4am, then the
\"effective time\" of any timestamps between midnight and 8am will be
23:59 of the previous day."
  :group 'org-time
  :version "24.1"
  :type 'boolean)

(defcustom org-edit-timestamp-down-means-later nil
  "Non-nil means S-down will increase the time in a time stamp.
When nil, S-up will increase."
  :group 'org-time
  :type 'boolean)

(defcustom org-calendar-follow-timestamp-change t
  "Non-nil means make the calendar window follow timestamp changes.
When a timestamp is modified and the calendar window is visible, it will be
moved to the new date."
  :group 'org-time
  :type 'boolean)

(defgroup org-tags nil
  "Options concerning tags in Org-mode."
  :tag "Org Tags"
  :group 'org)

(defcustom org-tag-alist nil
  "List of tags allowed in Org-mode files.
When this list is nil, Org-mode will base TAG input on what is already in the
buffer.
The value of this variable is an alist, the car of each entry must be a
keyword as a string, the cdr may be a character that is used to select
that tag through the fast-tag-selection interface.
See the manual for details."
  :group 'org-tags
  :type '(repeat
	  (choice
	   (cons   (string    :tag "Tag name")
		   (character :tag "Access char"))
	   (list :tag "Start radio group"
		 (const :startgroup)
		 (option (string :tag "Group description")))
	   (list :tag "End radio group"
		 (const :endgroup)
		 (option (string :tag "Group description")))
	   (const :tag "New line" (:newline)))))

(defcustom org-tag-persistent-alist nil
  "List of tags that will always appear in all Org-mode files.
This is in addition to any in buffer settings or customizations
of `org-tag-alist'.
When this list is nil, Org-mode will base TAG input on `org-tag-alist'.
The value of this variable is an alist, the car of each entry must be a
keyword as a string, the cdr may be a character that is used to select
that tag through the fast-tag-selection interface.
See the manual for details.
To disable these tags on a per-file basis, insert anywhere in the file:
   #+STARTUP: noptag"
  :group 'org-tags
  :type '(repeat
	  (choice
	   (cons   (string    :tag "Tag name")
		   (character :tag "Access char"))
	   (const :tag "Start radio group" (:startgroup))
	   (const :tag "End radio group" (:endgroup))
	   (const :tag "New line" (:newline)))))

(defcustom org-complete-tags-always-offer-all-agenda-tags nil
  "If non-nil, always offer completion for all tags of all agenda files.
Instead of customizing this variable directly, you might want to
set it locally for capture buffers, because there no list of
tags in that file can be created dynamically (there are none).

  (add-hook 'org-capture-mode-hook
            (lambda ()
              (set (make-local-variable
                    'org-complete-tags-always-offer-all-agenda-tags)
                   t)))"
  :group 'org-tags
  :version "24.1"
  :type 'boolean)

(defvar org-file-tags nil
  "List of tags that can be inherited by all entries in the file.
The tags will be inherited if the variable `org-use-tag-inheritance'
says they should be.
This variable is populated from #+FILETAGS lines.")

(defcustom org-use-fast-tag-selection 'auto
  "Non-nil means use fast tag selection scheme.
This is a special interface to select and deselect tags with single keys.
When nil, fast selection is never used.
When the symbol `auto', fast selection is used if and only if selection
characters for tags have been configured, either through the variable
`org-tag-alist' or through a #+TAGS line in the buffer.
When t, fast selection is always used and selection keys are assigned
automatically if necessary."
  :group 'org-tags
  :type '(choice
	  (const :tag "Always" t)
	  (const :tag "Never" nil)
	  (const :tag "When selection characters are configured" 'auto)))

(defcustom org-fast-tag-selection-single-key nil
  "Non-nil means fast tag selection exits after first change.
When nil, you have to press RET to exit it.
During fast tag selection, you can toggle this flag with `C-c'.
This variable can also have the value `expert'.  In this case, the window
displaying the tags menu is not even shown, until you press C-c again."
  :group 'org-tags
  :type '(choice
	  (const :tag "No" nil)
	  (const :tag "Yes" t)
	  (const :tag "Expert" expert)))

(defvar org-fast-tag-selection-include-todo nil
  "Non-nil means fast tags selection interface will also offer TODO states.
This is an undocumented feature, you should not rely on it.")

(defcustom org-tags-column (if (featurep 'xemacs) -76 -77)
  "The column to which tags should be indented in a headline.
If this number is positive, it specifies the column.  If it is negative,
it means that the tags should be flushright to that column.  For example,
-80 works well for a normal 80 character screen.
When 0, place tags directly after headline text, with only one space in
between."
  :group 'org-tags
  :type 'integer)

(defcustom org-auto-align-tags t
  "Non-nil keeps tags aligned when modifying headlines.
Some operations (i.e. demoting) change the length of a headline and
therefore shift the tags around.  With this option turned on, after
each such operation the tags are again aligned to `org-tags-column'."
  :group 'org-tags
  :type 'boolean)

(defcustom org-use-tag-inheritance t
  "Non-nil means tags in levels apply also for sublevels.
When nil, only the tags directly given in a specific line apply there.
This may also be a list of tags that should be inherited, or a regexp that
matches tags that should be inherited.  Additional control is possible
with the variable  `org-tags-exclude-from-inheritance' which gives an
explicit list of tags to be excluded from inheritance., even if the value of
`org-use-tag-inheritance' would select it for inheritance.

If this option is t, a match early-on in a tree can lead to a large
number of matches in the subtree when constructing the agenda or creating
a sparse tree.  If you only want to see the first match in a tree during
a search, check out the variable `org-tags-match-list-sublevels'."
  :group 'org-tags
  :type '(choice
	  (const :tag "Not" nil)
	  (const :tag "Always" t)
	  (repeat :tag "Specific tags" (string :tag "Tag"))
	  (regexp :tag "Tags matched by regexp")))

(defcustom org-tags-exclude-from-inheritance nil
  "List of tags that should never be inherited.
This is a way to exclude a few tags from inheritance.  For way to do
the opposite, to actively allow inheritance for selected tags,
see the variable `org-use-tag-inheritance'."
  :group 'org-tags
  :type '(repeat (string :tag "Tag")))

(defun org-tag-inherit-p (tag)
  "Check if TAG is one that should be inherited."
  (cond
   ((member tag org-tags-exclude-from-inheritance) nil)
   ((eq org-use-tag-inheritance t) t)
   ((not org-use-tag-inheritance) nil)
   ((stringp org-use-tag-inheritance)
    (string-match org-use-tag-inheritance tag))
   ((listp org-use-tag-inheritance)
    (member tag org-use-tag-inheritance))
   (t (error "Invalid setting of `org-use-tag-inheritance'"))))

(defcustom org-tags-match-list-sublevels t
  "Non-nil means list also sublevels of headlines matching a search.
This variable applies to tags/property searches, and also to stuck
projects because this search is based on a tags match as well.

When set to the symbol `indented', sublevels are indented with
leading dots.

Because of tag inheritance (see variable `org-use-tag-inheritance'),
the sublevels of a headline matching a tag search often also match
the same search.  Listing all of them can create very long lists.
Setting this variable to nil causes subtrees of a match to be skipped.

This variable is semi-obsolete and probably should always be true.  It
is better to limit inheritance to certain tags using the variables
`org-use-tag-inheritance' and `org-tags-exclude-from-inheritance'."
  :group 'org-tags
  :type '(choice
	  (const :tag "No, don't list them" nil)
	  (const :tag "Yes, do list them" t)
	  (const :tag "List them, indented with leading dots" indented)))

(defcustom org-tags-sort-function nil
  "When set, tags are sorted using this function as a comparator."
  :group 'org-tags
  :type '(choice
	  (const :tag "No sorting" nil)
	  (const :tag "Alphabetical" string<)
	  (const :tag "Reverse alphabetical" string>)
	  (function :tag "Custom function" nil)))

(defvar org-tags-history nil
  "History of minibuffer reads for tags.")
(defvar org-last-tags-completion-table nil
  "The last used completion table for tags.")
(defvar org-after-tags-change-hook nil
  "Hook that is run after the tags in a line have changed.")

(defgroup org-properties nil
  "Options concerning properties in Org-mode."
  :tag "Org Properties"
  :group 'org)

(defcustom org-property-format "%-10s %s"
  "How property key/value pairs should be formatted by `indent-line'.
When `indent-line' hits a property definition, it will format the line
according to this format, mainly to make sure that the values are
lined-up with respect to each other."
  :group 'org-properties
  :type 'string)

(defcustom org-properties-postprocess-alist nil
  "Alist of properties and functions to adjust inserted values.
Elements of this alist must be of the form

  ([string] [function])

where [string] must be a property name and [function] must be a
lambda expression: this lambda expression must take one argument,
the value to adjust, and return the new value as a string.

For example, this element will allow the property \"Remaining\"
to be updated wrt the relation between the \"Effort\" property
and the clock summary:

 ((\"Remaining\" (lambda(value)
                   (let ((clocksum (org-clock-sum-current-item))
                         (effort (org-duration-string-to-minutes
                                   (org-entry-get (point) \"Effort\"))))
                     (org-minutes-to-hh:mm-string (- effort clocksum))))))"
  :group 'org-properties
  :version "24.1"
  :type 'alist)

(defcustom org-use-property-inheritance nil
  "Non-nil means properties apply also for sublevels.

This setting is chiefly used during property searches.  Turning it on can
cause significant overhead when doing a search, which is why it is not
on by default.

When nil, only the properties directly given in the current entry count.
When t, every property is inherited.  The value may also be a list of
properties that should have inheritance, or a regular expression matching
properties that should be inherited.

However, note that some special properties use inheritance under special
circumstances (not in searches).  Examples are CATEGORY, ARCHIVE, COLUMNS,
and the properties ending in \"_ALL\" when they are used as descriptor
for valid values of a property.

Note for programmers:
When querying an entry with `org-entry-get',  you can control if inheritance
should be used.  By default, `org-entry-get' looks only at the local
properties.  You can request inheritance by setting the inherit argument
to t (to force inheritance) or to `selective' (to respect the setting
in this variable)."
  :group 'org-properties
  :type '(choice
	  (const :tag "Not" nil)
	  (const :tag "Always" t)
	  (repeat :tag "Specific properties" (string :tag "Property"))
	  (regexp :tag "Properties matched by regexp")))

(defun org-property-inherit-p (property)
  "Check if PROPERTY is one that should be inherited."
  (cond
   ((eq org-use-property-inheritance t) t)
   ((not org-use-property-inheritance) nil)
   ((stringp org-use-property-inheritance)
    (string-match org-use-property-inheritance property))
   ((listp org-use-property-inheritance)
    (member property org-use-property-inheritance))
   (t (error "Invalid setting of `org-use-property-inheritance'"))))

(defcustom org-columns-default-format "%25ITEM %TODO %3PRIORITY %TAGS"
  "The default column format, if no other format has been defined.
This variable can be set on the per-file basis by inserting a line

#+COLUMNS: %25ITEM ....."
  :group 'org-properties
  :type 'string)

(defcustom org-columns-ellipses ".."
  "The ellipses to be used when a field in column view is truncated.
When this is the empty string, as many characters as possible are shown,
but then there will be no visual indication that the field has been truncated.
When this is a string of length N, the last N characters of a truncated
field are replaced by this string.  If the column is narrower than the
ellipses string, only part of the ellipses string will be shown."
  :group 'org-properties
  :type 'string)

(defcustom org-columns-modify-value-for-display-function nil
  "Function that modifies values for display in column view.
For example, it can be used to cut out a certain part from a time stamp.
The function must take 2 arguments:

column-title    The title of the column (*not* the property name)
value           The value that should be modified.

The function should return the value that should be displayed,
or nil if the normal value should be used."
  :group 'org-properties
  :type 'function)

(defcustom org-effort-property "Effort"
  "The property that is being used to keep track of effort estimates.
Effort estimates given in this property need to have the format H:MM."
  :group 'org-properties
  :group 'org-progress
  :type '(string :tag "Property"))

(defconst org-global-properties-fixed
  '(("VISIBILITY_ALL" . "folded children content all")
    ("CLOCK_MODELINE_TOTAL_ALL" . "current today repeat all auto"))
  "List of property/value pairs that can be inherited by any entry.

These are fixed values, for the preset properties.  The user variable
that can be used to add to this list is `org-global-properties'.

The entries in this list are cons cells where the car is a property
name and cdr is a string with the value.  If the value represents
multiple items like an \"_ALL\" property, separate the items by
spaces.")

(defcustom org-global-properties nil
  "List of property/value pairs that can be inherited by any entry.

This list will be combined with the constant `org-global-properties-fixed'.

The entries in this list are cons cells where the car is a property
name and cdr is a string with the value.

You can set buffer-local values for the same purpose in the variable
`org-file-properties' this by adding lines like

#+PROPERTY: NAME VALUE"
  :group 'org-properties
  :type '(repeat
	  (cons (string :tag "Property")
		(string :tag "Value"))))

(defvar org-file-properties nil
  "List of property/value pairs that can be inherited by any entry.
Valid for the current buffer.
This variable is populated from #+PROPERTY lines.")
(make-variable-buffer-local 'org-file-properties)

(defgroup org-agenda nil
  "Options concerning agenda views in Org-mode."
  :tag "Org Agenda"
  :group 'org)

(defvar org-category nil
  "Variable used by org files to set a category for agenda display.
Such files should use a file variable to set it, for example

#   -*- mode: org; org-category: \"ELisp\"

or contain a special line

#+CATEGORY: ELisp

If the file does not specify a category, then file's base name
is used instead.")
(make-variable-buffer-local 'org-category)
(put 'org-category 'safe-local-variable #'(lambda (x) (or (symbolp x) (stringp x))))

(defcustom org-agenda-files nil
  "The files to be used for agenda display.
Entries may be added to this list with \\[org-agenda-file-to-front] and removed with
\\[org-remove-file].  You can also use customize to edit the list.

If an entry is a directory, all files in that directory that are matched by
`org-agenda-file-regexp' will be part of the file list.

If the value of the variable is not a list but a single file name, then
the list of agenda files is actually stored and maintained in that file, one
agenda file per line.  In this file paths can be given relative to
`org-directory'.  Tilde expansion and environment variable substitution
are also made."
  :group 'org-agenda
  :type '(choice
	  (repeat :tag "List of files and directories" file)
	  (file :tag "Store list in a file\n" :value "~/.agenda_files")))

(defcustom org-agenda-file-regexp "\\`[^.].*\\.org\\'"
  "Regular expression to match files for `org-agenda-files'.
If any element in the list in that variable contains a directory instead
of a normal file, all files in that directory that are matched by this
regular expression will be included."
  :group 'org-agenda
  :type 'regexp)

(defcustom org-agenda-text-search-extra-files nil
  "List of extra files to be searched by text search commands.
These files will be search in addition to the agenda files by the
commands `org-search-view' (`C-c a s') and `org-occur-in-agenda-files'.
Note that these files will only be searched for text search commands,
not for the other agenda views like todo lists, tag searches or the weekly
agenda.  This variable is intended to list notes and possibly archive files
that should also be searched by these two commands.
In fact, if the first element in the list is the symbol `agenda-archives',
than all archive files of all agenda files will be added to the search
scope."
  :group 'org-agenda
  :type '(set :greedy t
	   (const :tag "Agenda Archives" agenda-archives)
	   (repeat :inline t (file))))

(if (fboundp 'defvaralias)
    (defvaralias 'org-agenda-multi-occur-extra-files
      'org-agenda-text-search-extra-files))

(defcustom org-agenda-skip-unavailable-files nil
  "Non-nil means to just skip non-reachable files in `org-agenda-files'.
A nil value means to remove them, after a query, from the list."
  :group 'org-agenda
  :type 'boolean)

(defcustom org-calendar-to-agenda-key [?c]
  "The key to be installed in `calendar-mode-map' for switching to the agenda.
The command `org-calendar-goto-agenda' will be bound to this key.  The
default is the character `c' because then `c' can be used to switch back and
forth between agenda and calendar."
  :group 'org-agenda
  :type 'sexp)

(defcustom org-calendar-agenda-action-key [?k]
  "The key to be installed in `calendar-mode-map' for agenda-action.
The command `org-agenda-action' will be bound to this key.  The
default is the character `k' because we use the same key in the agenda."
  :group 'org-agenda
  :type 'sexp)

(defcustom org-calendar-insert-diary-entry-key [?i]
  "The key to be installed in `calendar-mode-map' for adding diary entries.
This option is irrelevant until `org-agenda-diary-file' has been configured
to point to an Org-mode file.  When that is the case, the command
`org-agenda-diary-entry' will be bound to the key given here, by default
`i'.  In the calendar, `i' normally adds entries to `diary-file'.  So
if you want to continue doing this, you need to change this to a different
key."
  :group 'org-agenda
  :type 'sexp)

(defcustom org-agenda-diary-file 'diary-file
  "File to which to add new entries with the `i' key in agenda and calendar.
When this is the symbol `diary-file', the functionality in the Emacs
calendar will be used to add entries to the `diary-file'.  But when this
points to a file, `org-agenda-diary-entry' will be used instead."
  :group 'org-agenda
  :type '(choice
	  (const :tag "The standard Emacs diary file" diary-file)
	  (file :tag "Special Org file diary entries")))

(eval-after-load "calendar"
  '(progn
     (org-defkey calendar-mode-map org-calendar-to-agenda-key
		 'org-calendar-goto-agenda)
     (org-defkey calendar-mode-map org-calendar-agenda-action-key
		 'org-agenda-action)
     (add-hook 'calendar-mode-hook
	       (lambda ()
		 (unless (eq org-agenda-diary-file 'diary-file)
		   (define-key calendar-mode-map
		     org-calendar-insert-diary-entry-key
		     'org-agenda-diary-entry))))))

(defgroup org-latex nil
  "Options for embedding LaTeX code into Org-mode."
  :tag "Org LaTeX"
  :group 'org)

(defcustom org-format-latex-options
  '(:foreground default :background default :scale 1.0
    :html-foreground "Black" :html-background "Transparent"
    :html-scale 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))
  "Options for creating images from LaTeX fragments.
This is a property list with the following properties:
:foreground  the foreground color for images embedded in Emacs, e.g. \"Black\".
             `default' means use the foreground of the default face.
:background  the background color, or \"Transparent\".
             `default' means use the background of the default face.
:scale       a scaling factor for the size of the images, to get more pixels
:html-foreground, :html-background, :html-scale
             the same numbers for HTML export.
:matchers    a list indicating which matchers should be used to
             find LaTeX fragments.  Valid members of this list are:
             \"begin\"  find environments
             \"$1\"     find single characters surrounded by $.$
             \"$\"      find math expressions surrounded by $...$
             \"$$\"     find math expressions surrounded by $$....$$
             \"\\(\"     find math expressions surrounded by \\(...\\)
             \"\\ [\"    find math expressions surrounded by \\ [...\\]"
  :group 'org-latex
  :type 'plist)

(defcustom org-format-latex-signal-error t
  "Non-nil means signal an error when image creation of LaTeX snippets fails.
When nil, just push out a message."
  :group 'org-latex
  :version "24.1"
  :type 'boolean)
(defcustom org-latex-to-mathml-jar-file nil
  "Value of\"%j\" in `org-latex-to-mathml-convert-command'.
Use this to specify additional executable file say a jar file.

When using MathToWeb as the converter, specify the full-path to
your mathtoweb.jar file."
  :group 'org-latex
  :version "24.1"
  :type '(choice
	  (const :tag "None" nil)
	  (file :tag "JAR file" :must-match t)))

(defcustom org-latex-to-mathml-convert-command nil
  "Command to convert LaTeX fragments to MathML.
Replace format-specifiers in the command as noted below and use
`shell-command' to convert LaTeX to MathML.
%j:     Executable file in fully expanded form as specified by
        `org-latex-to-mathml-jar-file'.
%I:     Input LaTeX file in fully expanded form
%o:     Output MathML file
This command is used by `org-create-math-formula'.

When using MathToWeb as the converter, set this to
\"java -jar %j -unicode -force -df %o %I\"."
  :group 'org-latex
  :version "24.1"
  :type '(choice
	  (const :tag "None" nil)
	  (string :tag "\nShell command")))

(defun org-format-latex-mathml-available-p ()
  "Return t if `org-latex-to-mathml-convert-command' is usable."
  (save-match-data
    (when (and (boundp 'org-latex-to-mathml-convert-command)
	       org-latex-to-mathml-convert-command)
      (let ((executable (car (split-string
			      org-latex-to-mathml-convert-command))))
	(when (executable-find executable)
	  (if (string-match
	       "%j" org-latex-to-mathml-convert-command)
	      (file-readable-p org-latex-to-mathml-jar-file)
	    t))))))

(defcustom org-format-latex-header "\\documentclass{article}
\\usepackage[usenames]{color}
\\usepackage{amsmath}
\\usepackage[mathscr]{eucal}
\\pagestyle{empty}             % do not remove
\[PACKAGES]
\[DEFAULT-PACKAGES]
% The settings below are copied from fullpage.sty
\\setlength{\\textwidth}{\\paperwidth}
\\addtolength{\\textwidth}{-3cm}
\\setlength{\\oddsidemargin}{1.5cm}
\\addtolength{\\oddsidemargin}{-2.54cm}
\\setlength{\\evensidemargin}{\\oddsidemargin}
\\setlength{\\textheight}{\\paperheight}
\\addtolength{\\textheight}{-\\headheight}
\\addtolength{\\textheight}{-\\headsep}
\\addtolength{\\textheight}{-\\footskip}
\\addtolength{\\textheight}{-3cm}
\\setlength{\\topmargin}{1.5cm}
\\addtolength{\\topmargin}{-2.54cm}"
  "The document header used for processing LaTeX fragments.
It is imperative that this header make sure that no page number
appears on the page.  The package defined in the variables
`org-export-latex-default-packages-alist' and `org-export-latex-packages-alist'
will either replace the placeholder \"[PACKAGES]\" in this header, or they
will be appended."
  :group 'org-latex
  :type 'string)

(defvar org-format-latex-header-extra nil)

(defun org-set-packages-alist (var val)
  "Set the packages alist and make sure it has 3 elements per entry."
  (set var (mapcar (lambda (x)
		     (if (and (consp x) (= (length x) 2))
			 (list (car x) (nth 1 x) t)
		       x))
		   val)))

(defun org-get-packages-alist (var)

  "Get the packages alist and make sure it has 3 elements per entry."
  (mapcar (lambda (x)
	    (if (and (consp x) (= (length x) 2))
		(list (car x) (nth 1 x) t)
	      x))
	  (default-value var)))

;; The following variables are defined here because is it also used
;; when formatting latex fragments.  Originally it was part of the
;; LaTeX exporter, which is why the name includes "export".
(defcustom org-export-latex-default-packages-alist
  '(("AUTO" "inputenc"  t)
    ("T1"   "fontenc"   t)
    (""     "fixltx2e"  nil)
    (""     "graphicx"  t)
    (""     "longtable" nil)
    (""     "float"     nil)
    (""     "wrapfig"   nil)
    (""     "soul"      t)
    (""     "textcomp"  t)
    (""     "marvosym"  t)
    (""     "wasysym"   t)
    (""     "latexsym"  t)
    (""     "amssymb"   t)
    (""     "hyperref"  nil)
    "\\tolerance=1000"
    )
  "Alist of default packages to be inserted in the header.
Change this only if one of the packages here causes an incompatibility
with another package you are using.
The packages in this list are needed by one part or another of Org-mode
to function properly.

- inputenc, fontenc:  for basic font and character selection
- textcomp, marvosymb, wasysym, latexsym, amssym: for various symbols used
  for interpreting the entities in `org-entities'.  You can skip some of these
  packages if you don't use any of the symbols in it.
- graphicx: for including images
- float, wrapfig: for figure placement
- longtable: for long tables
- hyperref: for cross references

Therefore you should not modify this variable unless you know what you
are doing.  The one reason to change it anyway is that you might be loading
some other package that conflicts with one of the default packages.
Each cell is of the format \( \"options\" \"package\" snippet-flag\).
If SNIPPET-FLAG is t, the package also needs to be included when
compiling LaTeX snippets into images for inclusion into HTML."
  :group 'org-export-latex
  :set 'org-set-packages-alist
  :get 'org-get-packages-alist
  :version "24.1"
  :type '(repeat
	  (choice
	   (list :tag "options/package pair"
		 (string :tag "options")
		 (string :tag "package")
		 (boolean :tag "Snippet"))
	   (string :tag "A line of LaTeX"))))

(defcustom org-export-latex-packages-alist nil
  "Alist of packages to be inserted in every LaTeX header.
These will be inserted after `org-export-latex-default-packages-alist'.
Each cell is of the format \( \"options\" \"package\" snippet-flag \).
SNIPPET-FLAG, when t, indicates that this package is also needed when
turning LaTeX snippets into images for inclusion into HTML.
Make sure that you only list packages here which:
- you want in every file
- do not conflict with the default packages in
  `org-export-latex-default-packages-alist'
- do not conflict with the setup in `org-format-latex-header'."
  :group 'org-export-latex
  :set 'org-set-packages-alist
  :get 'org-get-packages-alist
  :type '(repeat
	  (choice
	   (list :tag "options/package pair"
		 (string :tag "options")
		 (string :tag "package")
		 (boolean :tag "Snippet"))
	   (string :tag "A line of LaTeX"))))


(defgroup org-appearance nil
  "Settings for Org-mode appearance."
  :tag "Org Appearance"
  :group 'org)

(defcustom org-level-color-stars-only nil
  "Non-nil means fontify only the stars in each headline.
When nil, the entire headline is fontified.
Changing it requires restart of `font-lock-mode' to become effective
also in regions already fontified."
  :group 'org-appearance
  :type 'boolean)

(defcustom org-hide-leading-stars nil
  "Non-nil means hide the first N-1 stars in a headline.
This works by using the face `org-hide' for these stars.  This
face is white for a light background, and black for a dark
background.  You may have to customize the face `org-hide' to
make this work.
Changing it requires restart of `font-lock-mode' to become effective
also in regions already fontified.
You may also set this on a per-file basis by adding one of the following
lines to the buffer:

   #+STARTUP: hidestars
   #+STARTUP: showstars"
  :group 'org-appearance
  :type 'boolean)

(defcustom org-hidden-keywords nil
  "List of symbols corresponding to keywords to be hidden the org buffer.
For example, a value '(title) for this list will make the document's title
appear in the buffer without the initial #+TITLE: keyword."
  :group 'org-appearance
  :version "24.1"
  :type '(set (const :tag "#+AUTHOR" author)
	      (const :tag "#+DATE" date)
	      (const :tag "#+EMAIL" email)
	      (const :tag "#+TITLE" title)))

(defcustom org-fontify-done-headline nil
  "Non-nil means change the face of a headline if it is marked DONE.
Normally, only the TODO/DONE keyword indicates the state of a headline.
When this is non-nil, the headline after the keyword is set to the
`org-headline-done' as an additional indication."
  :group 'org-appearance
  :type 'boolean)

(defcustom org-fontify-emphasized-text t
  "Non-nil means fontify *bold*, /italic/ and _underlined_ text.
Changing this variable requires a restart of Emacs to take effect."
  :group 'org-appearance
  :type 'boolean)

(defcustom org-fontify-whole-heading-line nil
  "Non-nil means fontify the whole line for headings.
This is useful when setting a background color for the
org-level-* faces."
  :group 'org-appearance
  :type 'boolean)

(defcustom org-highlight-latex-fragments-and-specials nil
  "Non-nil means fontify what is treated specially by the exporters."
  :group 'org-appearance
  :type 'boolean)

(defcustom org-hide-emphasis-markers nil
  "Non-nil mean font-lock should hide the emphasis marker characters."
  :group 'org-appearance
  :type 'boolean)

(defcustom org-pretty-entities nil
  "Non-nil means show entities as UTF8 characters.
When nil, the \\name form remains in the buffer."
  :group 'org-appearance
  :version "24.1"
  :type 'boolean)

(defcustom org-pretty-entities-include-sub-superscripts t
  "Non-nil means, pretty entity display includes formatting sub/superscripts."
  :group 'org-appearance
  :version "24.1"
  :type 'boolean)

(defvar org-emph-re nil
  "Regular expression for matching emphasis.
After a match, the match groups contain these elements:
0  The match of the full regular expression, including the characters
     before and after the proper match
1  The character before the proper match, or empty at beginning of line
2  The proper match, including the leading and trailing markers
3  The leading marker like * or /, indicating the type of highlighting
4  The text between the emphasis markers, not including the markers
5  The character after the match, empty at the end of a line")
(defvar org-verbatim-re nil
  "Regular expression for matching verbatim text.")
(defvar org-emphasis-regexp-components) ; defined just below
(defvar org-emphasis-alist) ; defined just below
(defun org-set-emph-re (var val)
  "Set variable and compute the emphasis regular expression."
  (set var val)
  (when (and (boundp 'org-emphasis-alist)
	     (boundp 'org-emphasis-regexp-components)
	     org-emphasis-alist org-emphasis-regexp-components)
    (let* ((e org-emphasis-regexp-components)
	   (pre (car e))
	   (post (nth 1 e))
	   (border (nth 2 e))
	   (body (nth 3 e))
	   (nl (nth 4 e))
	   (body1 (concat body "*?"))
	   (markers (mapconcat 'car org-emphasis-alist ""))
	   (vmarkers (mapconcat
		      (lambda (x) (if (eq (nth 4 x) 'verbatim) (car x) ""))
		      org-emphasis-alist "")))
      ;; make sure special characters appear at the right position in the class
      (if (string-match "\\^" markers)
	  (setq markers (concat (replace-match "" t t markers) "^")))
      (if (string-match "-" markers)
	  (setq markers (concat (replace-match "" t t markers) "-")))
      (if (string-match "\\^" vmarkers)
	  (setq vmarkers (concat (replace-match "" t t vmarkers) "^")))
      (if (string-match "-" vmarkers)
	  (setq vmarkers (concat (replace-match "" t t vmarkers) "-")))
      (if (> nl 0)
          (setq body1 (concat body1 "\\(?:\n" body "*?\\)\\{0,"
                              (int-to-string nl) "\\}")))
      ;; Make the regexp
      (setq org-emph-re
	    (concat "\\([" pre "]\\|^\\)"
		    "\\("
		    "\\([" markers "]\\)"
		    "\\("
		    "[^" border "]\\|"
		    "[^" border "]"
		    body1
		    "[^" border "]"
		    "\\)"
		    "\\3\\)"
		    "\\([" post "]\\|$\\)"))
      (setq org-verbatim-re
	    (concat "\\([" pre "]\\|^\\)"
		    "\\("
		    "\\([" vmarkers "]\\)"
		    "\\("
		    "[^" border "]\\|"
		    "[^" border "]"
		    body1
		    "[^" border "]"
		    "\\)"
		    "\\3\\)"
		    "\\([" post  "]\\|$\\)")))))

(defcustom org-emphasis-regexp-components
  '(" \t('\"{" "- \t.,:!?;'\")}\\" " \t\r\n,\"'" "." 1)
  "Components used to build the regular expression for emphasis.
This is a list with five entries.  Terminology:  In an emphasis string
like \" *strong word* \", we call the initial space PREMATCH, the final
space POSTMATCH, the stars MARKERS, \"s\" and \"d\" are BORDER characters
and \"trong wor\" is the body.  The different components in this variable
specify what is allowed/forbidden in each part:

pre          Chars allowed as prematch.  Beginning of line will be allowed too.
post         Chars allowed as postmatch.  End of line will be allowed too.
border       The chars *forbidden* as border characters.
body-regexp  A regexp like \".\" to match a body character.  Don't use
             non-shy groups here, and don't allow newline here.
newline      The maximum number of newlines allowed in an emphasis exp.

Use customize to modify this, or restart Emacs after changing it."
  :group 'org-appearance
  :set 'org-set-emph-re
  :type '(list
	  (sexp    :tag "Allowed chars in pre      ")
	  (sexp    :tag "Allowed chars in post     ")
	  (sexp    :tag "Forbidden chars in border ")
	  (sexp    :tag "Regexp for body           ")
	  (integer :tag "number of newlines allowed")
	  (option (boolean :tag "Please ignore this button"))))

(defcustom org-emphasis-alist
  `(("*" bold "<b>" "</b>")
    ("/" italic "<i>" "</i>")
    ("_" underline "<span style=\"text-decoration:underline;\">" "</span>")
    ("=" org-code "<code>" "</code>" verbatim)
    ("~" org-verbatim "<code>" "</code>" verbatim)
    ("+" ,(if (featurep 'xemacs) 'org-table '(:strike-through t))
     "<del>" "</del>")
    )
  "Special syntax for emphasized text.
Text starting and ending with a special character will be emphasized, for
example *bold*, _underlined_ and /italic/.  This variable sets the marker
characters, the face to be used by font-lock for highlighting in Org-mode
Emacs buffers, and the HTML tags to be used for this.
For LaTeX export, see the variable `org-export-latex-emphasis-alist'.
For DocBook export, see the variable `org-export-docbook-emphasis-alist'.
Use customize to modify this, or restart Emacs after changing it."
  :group 'org-appearance
  :set 'org-set-emph-re
  :type '(repeat
	  (list
	   (string :tag "Marker character")
	   (choice
	    (face :tag "Font-lock-face")
	    (plist :tag "Face property list"))
	   (string :tag "HTML start tag")
	   (string :tag "HTML end tag")
	   (option (const verbatim)))))

(defvar org-protecting-blocks
  '("src" "example" "latex" "ascii" "html" "docbook" "ditaa" "dot" "r" "R")
  "Blocks that contain text that is quoted, i.e. not processed as Org syntax.
This is needed for font-lock setup.")

;;; Miscellaneous options

(defgroup org-completion nil
  "Completion in Org-mode."
  :tag "Org Completion"
  :group 'org)

(defcustom org-completion-use-ido nil
  "Non-nil means use ido completion wherever possible.
Note that `ido-mode' must be active for this variable to be relevant.
If you decide to turn this variable on, you might well want to turn off
`org-outline-path-complete-in-steps'.
See also `org-completion-use-iswitchb'."
  :group 'org-completion
  :type 'boolean)

(defcustom org-completion-use-iswitchb nil
  "Non-nil means use iswitchb completion wherever possible.
Note that `iswitchb-mode' must be active for this variable to be relevant.
If you decide to turn this variable on, you might well want to turn off
`org-outline-path-complete-in-steps'.
Note that this variable has only an effect if `org-completion-use-ido' is nil."
  :group 'org-completion
  :type 'boolean)

(defcustom org-completion-fallback-command 'hippie-expand
  "The expansion command called by \\[pcomplete] in normal context.
Normal means, no org-mode-specific context."
  :group 'org-completion
  :type 'function)

;;; Functions and variables from their packages
;;  Declared here to avoid compiler warnings

;; XEmacs only
(defvar outline-mode-menu-heading)
(defvar outline-mode-menu-show)
(defvar outline-mode-menu-hide)
(defvar zmacs-regions) ; XEmacs regions

;; Emacs only
(defvar mark-active)

;; Various packages
(declare-function calendar-absolute-from-iso    "cal-iso"    (date))
(declare-function calendar-forward-day          "cal-move"   (arg))
(declare-function calendar-goto-date            "cal-move"   (date))
(declare-function calendar-goto-today           "cal-move"   ())
(declare-function calendar-iso-from-absolute    "cal-iso"    (date))
(defvar calc-embedded-close-formula)
(defvar calc-embedded-open-formula)
(declare-function cdlatex-tab "ext:cdlatex" ())
(declare-function cdlatex-compute-tables "ext:cdlatex" ())
(declare-function dired-get-filename "dired" (&optional localp no-error-if-not-filep))
(defvar font-lock-unfontify-region-function)
(declare-function iswitchb-read-buffer "iswitchb"
                  (prompt &optional default require-match start matches-set))
(defvar iswitchb-temp-buflist)
(declare-function org-gnus-follow-link "org-gnus" (&optional group article))
(defvar org-agenda-tags-todo-honor-ignore-options)
(declare-function org-agenda-skip "org-agenda" ())
(declare-function
 org-agenda-format-item "org-agenda"
 (extra txt &optional category tags dotime noprefix remove-re habitp))
(declare-function org-agenda-new-marker "org-agenda" (&optional pos))
(declare-function org-agenda-change-all-lines "org-agenda"
		  (newhead hdmarker &optional fixface just-this))
(declare-function org-agenda-set-restriction-lock "org-agenda" (&optional type))
(declare-function org-agenda-maybe-redo "org-agenda" ())
(declare-function org-agenda-save-markers-for-cut-and-paste "org-agenda"
		  (beg end))
(declare-function org-agenda-copy-local-variable "org-agenda" (var))
(declare-function org-agenda-check-for-timestamp-as-reason-to-ignore-todo-item
		  "org-agenda" (&optional end))
(declare-function org-inlinetask-remove-END-maybe "org-inlinetask" ())
(declare-function org-inlinetask-in-task-p "org-inlinetask" ())
(declare-function org-inlinetask-goto-beginning "org-inlinetask" ())
(declare-function org-inlinetask-goto-end "org-inlinetask" ())
(declare-function org-indent-mode "org-indent" (&optional arg))
(declare-function parse-time-string "parse-time" (string))
(declare-function org-attach-reveal "org-attach" (&optional if-exists))
(declare-function org-export-latex-fix-inputenc "org-latex" ())
(declare-function orgtbl-send-table "org-table" (&optional maybe))
(defvar remember-data-file)
(defvar texmathp-why)
(declare-function speedbar-line-directory "speedbar" (&optional depth))
(declare-function table--at-cell-p "table" (position &optional object at-column))

(defvar w3m-current-url)
(defvar w3m-current-title)

(defvar org-latex-regexps)

;;; Autoload and prepare some org modules

;; Some table stuff that needs to be defined here, because it is used
;; by the functions setting up org-mode or checking for table context.

(defconst org-table-any-line-regexp "^[ \t]*\\(|\\|\\+-[-+]\\)"
  "Detect an org-type or table-type table.")
(defconst org-table-line-regexp "^[ \t]*|"
  "Detect an org-type table line.")
(defconst org-table-dataline-regexp "^[ \t]*|[^-]"
  "Detect an org-type table line.")
(defconst org-table-hline-regexp "^[ \t]*|-"
  "Detect an org-type table hline.")
(defconst org-table1-hline-regexp "^[ \t]*\\+-[-+]"
  "Detect a table-type table hline.")
(defconst org-table-any-border-regexp "^[ \t]*[^|+ \t]"
  "Detect the first line outside a table when searching from within it.
This works for both table types.")

;; Autoload the functions in org-table.el that are needed by functions here.

(eval-and-compile
  (org-autoload "org-table"
		'(org-table-align org-table-begin org-table-blank-field
   org-table-convert org-table-convert-region org-table-copy-down
   org-table-copy-region org-table-create
   org-table-create-or-convert-from-region
   org-table-create-with-table.el org-table-current-dline
   org-table-cut-region org-table-delete-column org-table-edit-field
   org-table-edit-formulas org-table-end org-table-eval-formula
   org-table-export org-table-field-info
   org-table-get-stored-formulas org-table-goto-column
   org-table-hline-and-move org-table-import org-table-insert-column
   org-table-insert-hline org-table-insert-row org-table-iterate
   org-table-justify-field-maybe org-table-kill-row
   org-table-maybe-eval-formula org-table-maybe-recalculate-line
   org-table-move-column org-table-move-column-left
   org-table-move-column-right org-table-move-row
   org-table-move-row-down org-table-move-row-up
   org-table-next-field org-table-next-row org-table-paste-rectangle
   org-table-previous-field org-table-recalculate
   org-table-rotate-recalc-marks org-table-sort-lines org-table-sum
   org-table-toggle-coordinate-overlays
   org-table-toggle-formula-debugger org-table-wrap-region
   orgtbl-mode turn-on-orgtbl org-table-to-lisp
   orgtbl-to-generic orgtbl-to-tsv orgtbl-to-csv orgtbl-to-latex
   orgtbl-to-orgtbl orgtbl-to-html orgtbl-to-texinfo)))

(defun org-at-table-p (&optional table-type)
  "Return t if the cursor is inside an org-type table.
If TABLE-TYPE is non-nil, also check for table.el-type tables."
  (if org-enable-table-editor
      (save-excursion
	(beginning-of-line 1)
	(looking-at (if table-type org-table-any-line-regexp
		      org-table-line-regexp)))
    nil))
(defsubst org-table-p () (org-at-table-p))

(defun org-at-table.el-p ()
  "Return t if and only if we are at a table.el table."
  (and (org-at-table-p 'any)
       (save-excursion
	 (goto-char (org-table-begin 'any))
	 (looking-at org-table1-hline-regexp))))
(defun org-table-recognize-table.el ()
  "If there is a table.el table nearby, recognize it and move into it."
  (if org-table-tab-recognizes-table.el
      (if (org-at-table.el-p)
	  (progn
	    (beginning-of-line 1)
	    (if (looking-at org-table-dataline-regexp)
		nil
	      (if (looking-at org-table1-hline-regexp)
		  (progn
		    (beginning-of-line 2)
		    (if (looking-at org-table-any-border-regexp)
			(beginning-of-line -1)))))
	    (if (re-search-forward "|" (org-table-end t) t)
		(progn
		  (require 'table)
		  (if (table--at-cell-p (point))
		      t
		    (message "recognizing table.el table...")
		    (table-recognize-table)
		    (message "recognizing table.el table...done")))
	      (error "This should not happen"))
	    t)
	nil)
    nil))

(defun org-at-table-hline-p ()
  "Return t if the cursor is inside a hline in a table."
  (if org-enable-table-editor
      (save-excursion
	(beginning-of-line 1)
	(looking-at org-table-hline-regexp))
    nil))

(defvar org-table-clean-did-remove-column nil)

(defun org-table-map-tables (function &optional quietly)
  "Apply FUNCTION to the start of all tables in the buffer."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (while (re-search-forward org-table-any-line-regexp nil t)
	(unless quietly
	  (message "Mapping tables: %d%%" (/ (* 100.0 (point)) (buffer-size))))
	(beginning-of-line 1)
	(when (looking-at org-table-line-regexp)
	  (save-excursion (funcall function))
	  (or (looking-at org-table-line-regexp)
	      (forward-char 1)))
	(re-search-forward org-table-any-border-regexp nil 1))))
  (unless quietly (message "Mapping tables: done")))

;; Declare and autoload functions from org-exp.el  & Co

(declare-function org-default-export-plist "org-exp")
(declare-function org-infile-export-plist "org-exp")
(declare-function org-get-current-options "org-exp")
(eval-and-compile
  (org-autoload "org-exp"
		'(org-export org-export-visible
			     org-insert-export-options-template
			     org-table-clean-before-export))
  (org-autoload "org-ascii"
		'(org-export-as-ascii org-export-ascii-preprocess
		  org-export-as-ascii-to-buffer org-replace-region-by-ascii
		  org-export-region-as-ascii))
  (org-autoload "org-latex"
		'(org-export-as-latex-batch org-export-as-latex-to-buffer
                  org-replace-region-by-latex org-export-region-as-latex
		  org-export-as-latex org-export-as-pdf
		  org-export-as-pdf-and-open))
  (org-autoload "org-html"
		'(org-export-as-html-and-open
		  org-export-as-html-batch org-export-as-html-to-buffer
		  org-replace-region-by-html org-export-region-as-html
		  org-export-as-html))
  (org-autoload "org-docbook"
		'(org-export-as-docbook-batch org-export-as-docbook-to-buffer
		  org-replace-region-by-docbook org-export-region-as-docbook
		  org-export-as-docbook-pdf org-export-as-docbook-pdf-and-open
		  org-export-as-docbook))
  (org-autoload "org-icalendar"
		'(org-export-icalendar-this-file
		  org-export-icalendar-all-agenda-files
		  org-export-icalendar-combine-agenda-files))
  (org-autoload "org-xoxo" '(org-export-as-xoxo))
  (org-autoload "org-beamer" '(org-beamer-mode org-beamer-sectioning)))

;; Declare and autoload functions from org-agenda.el

(eval-and-compile
  (org-autoload "org-agenda"
		'(org-agenda org-agenda-list org-search-view
   org-todo-list org-tags-view org-agenda-list-stuck-projects
   org-diary org-agenda-to-appt
   org-agenda-check-for-timestamp-as-reason-to-ignore-todo-item)))

;; Autoload org-remember

(eval-and-compile
  (org-autoload "org-remember"
		'(org-remember-insinuate org-remember-annotation
   org-remember-apply-template org-remember org-remember-handler)))

(eval-and-compile
  (org-autoload "org-capture"
		'(org-capture org-capture-insert-template-here
                  org-capture-import-remember-templates)))

;; Autoload org-clock.el

(declare-function org-clock-save-markers-for-cut-and-paste "org-clock"
		  (beg end))
(declare-function org-clock-update-mode-line "org-clock" ())
(declare-function org-resolve-clocks "org-clock"
		  (&optional also-non-dangling-p prompt last-valid))
(defvar org-clock-start-time)
(defvar org-clock-marker (make-marker)
  "Marker recording the last clock-in.")
(defvar org-clock-hd-marker (make-marker)
  "Marker recording the last clock-in, but the headline position.")
(defvar org-clock-heading ""
  "The heading of the current clock entry.")
(defun org-clock-is-active ()
 "Return non-nil if clock is currently running.
The return value is actually the clock marker."
 (marker-buffer org-clock-marker))

(eval-and-compile
  (org-autoload
   "org-clock"
   '(org-clock-in org-clock-out org-clock-cancel
		  org-clock-goto org-clock-sum org-clock-display
		  org-clock-remove-overlays org-clock-report
		  org-clocktable-shift org-dblock-write:clocktable
		  org-get-clocktable org-resolve-clocks)))

(defun org-clock-update-time-maybe ()
  "If this is a CLOCK line, update it and return t.
Otherwise, return nil."
  (interactive)
  (save-excursion
    (beginning-of-line 1)
    (skip-chars-forward " \t")
    (when (looking-at org-clock-string)
      (let ((re (concat "[ \t]*" org-clock-string
			" *[[<]\\([^]>]+\\)[]>]\\(-+[[<]\\([^]>]+\\)[]>]"
			"\\([ \t]*=>.*\\)?\\)?"))
	    ts te h m s neg)
	(cond
	 ((not (looking-at re))
	  nil)
	 ((not (match-end 2))
	  (when (and (equal (marker-buffer org-clock-marker) (current-buffer))
		     (> org-clock-marker (point))
		     (<= org-clock-marker (point-at-eol)))
	    ;; The clock is running here
	    (setq org-clock-start-time
		  (apply 'encode-time
			 (org-parse-time-string (match-string 1))))
	    (org-clock-update-mode-line)))
	 (t
	  (and (match-end 4) (delete-region (match-beginning 4) (match-end 4)))
	  (end-of-line 1)
	  (setq ts (match-string 1)
		te (match-string 3))
	  (setq s (- (org-float-time
		      (apply 'encode-time (org-parse-time-string te)))
		     (org-float-time
		      (apply 'encode-time (org-parse-time-string ts))))
		neg (< s 0)
		s (abs s)
		h (floor (/ s 3600))
		s (- s (* 3600 h))
		m (floor (/ s 60))
		s (- s (* 60 s)))
	  (insert " => " (format (if neg "-%d:%02d" "%2d:%02d") h m))
	  t))))))

(defun org-check-running-clock ()
  "Check if the current buffer contains the running clock.
If yes, offer to stop it and to save the buffer with the changes."
  (when (and (equal (marker-buffer org-clock-marker) (current-buffer))
	     (y-or-n-p (format "Clock-out in buffer %s before killing it? "
			       (buffer-name))))
    (org-clock-out)
    (when (y-or-n-p "Save changed buffer?")
      (save-buffer))))

(defun org-clocktable-try-shift (dir n)
  "Check if this line starts a clock table, if yes, shift the time block."
  (when (org-match-line "^[ \t]*#\\+BEGIN:[ \t]+clocktable\\>")
    (org-clocktable-shift dir n)))

;; Autoload org-timer.el

(eval-and-compile
  (org-autoload
   "org-timer"
   '(org-timer-start org-timer org-timer-item
		     org-timer-change-times-in-region
		     org-timer-set-timer
		     org-timer-reset-timers
		     org-timer-show-remaining-time)))

;; Autoload org-feed.el

(eval-and-compile
  (org-autoload
   "org-feed"
   '(org-feed-update org-feed-update-all org-feed-goto-inbox)))


;; Autoload org-indent.el

;; Define the variable already here, to make sure we have it.
(defvar org-indent-mode nil
  "Non-nil if Org-Indent mode is enabled.
Use the command `org-indent-mode' to change this variable.")

(eval-and-compile
  (org-autoload
   "org-indent"
   '(org-indent-mode)))

;; Autoload org-mobile.el

(eval-and-compile
  (org-autoload
   "org-mobile"
   '(org-mobile-push org-mobile-pull org-mobile-create-sumo-agenda)))

;; Autoload archiving code
;; The stuff that is needed for cycling and tags has to be defined here.

(defgroup org-archive nil
  "Options concerning archiving in Org-mode."
  :tag "Org Archive"
  :group 'org-structure)

(defcustom org-archive-location "%s_archive::"
  "The location where subtrees should be archived.

The value of this variable is a string, consisting of two parts,
separated by a double-colon.  The first part is a filename and
the second part is a headline.

When the filename is omitted, archiving happens in the same file.
%s in the filename will be replaced by the current file
name (without the directory part).  Archiving to a different file
is useful to keep archived entries from contributing to the
Org-mode Agenda.

The archived entries will be filed as subtrees of the specified
headline.  When the headline is omitted, the subtrees are simply
filed away at the end of the file, as top-level entries.  Also in
the heading you can use %s to represent the file name, this can be
useful when using the same archive for a number of different files.

Here are a few examples:
\"%s_archive::\"
	If the current file is Projects.org, archive in file
	Projects.org_archive, as top-level trees.  This is the default.

\"::* Archived Tasks\"
	Archive in the current file, under the top-level headline
	\"* Archived Tasks\".

\"~/org/archive.org::\"
	Archive in file ~/org/archive.org (absolute path), as top-level trees.

\"~/org/archive.org::* From %s\"
	Archive in file ~/org/archive.org (absolute path), under headlines
        \"From FILENAME\" where file name is the current file name.

\"basement::** Finished Tasks\"
	Archive in file ./basement (relative path), as level 3 trees
	below the level 2 heading \"** Finished Tasks\".

You may set this option on a per-file basis by adding to the buffer a
line like

#+ARCHIVE: basement::** Finished Tasks

You may also define it locally for a subtree by setting an ARCHIVE property
in the entry.  If such a property is found in an entry, or anywhere up
the hierarchy, it will be used."
  :group 'org-archive
  :type 'string)

(defcustom org-archive-tag "ARCHIVE"
  "The tag that marks a subtree as archived.
An archived subtree does not open during visibility cycling, and does
not contribute to the agenda listings.
After changing this, font-lock must be restarted in the relevant buffers to
get the proper fontification."
  :group 'org-archive
  :group 'org-keywords
  :type 'string)

(defcustom org-agenda-skip-archived-trees t
  "Non-nil means the agenda will skip any items located in archived trees.
An archived tree is a tree marked with the tag ARCHIVE.  The use of this
variable is no longer recommended, you should leave it at the value t.
Instead, use the key `v' to cycle the archives-mode in the agenda."
  :group 'org-archive
  :group 'org-agenda-skip
  :type 'boolean)

(defcustom org-columns-skip-archived-trees t
  "Non-nil means ignore archived trees when creating column view."
  :group 'org-archive
  :group 'org-properties
  :type 'boolean)

(defcustom org-cycle-open-archived-trees nil
  "Non-nil means `org-cycle' will open archived trees.
An archived tree is a tree marked with the tag ARCHIVE.
When nil, archived trees will stay folded.  You can still open them with
normal outline commands like `show-all', but not with the cycling commands."
  :group 'org-archive
  :group 'org-cycle
  :type 'boolean)

(defcustom org-sparse-tree-open-archived-trees nil
  "Non-nil means sparse tree construction shows matches in archived trees.
When nil, matches in these trees are highlighted, but the trees are kept in
collapsed state."
  :group 'org-archive
  :group 'org-sparse-trees
  :type 'boolean)

(defun org-cycle-hide-archived-subtrees (state)
  "Re-hide all archived subtrees after a visibility state change."
  (when (and (not org-cycle-open-archived-trees)
             (not (memq state '(overview folded))))
    (save-excursion
      (let* ((globalp (memq state '(contents all)))
             (beg (if globalp (point-min) (point)))
             (end (if globalp (point-max) (org-end-of-subtree t))))
	(org-hide-archived-subtrees beg end)
	(goto-char beg)
	(if (looking-at (concat ".*:" org-archive-tag ":"))
	    (message "%s" (substitute-command-keys
			   "Subtree is archived and stays closed.  Use \\[org-force-cycle-archived] to cycle it anyway.")))))))

(defun org-force-cycle-archived ()
  "Cycle subtree even if it is archived."
  (interactive)
  (setq this-command 'org-cycle)
  (let ((org-cycle-open-archived-trees t))
    (call-interactively 'org-cycle)))

(defun org-hide-archived-subtrees (beg end)
  "Re-hide all archived subtrees after a visibility state change."
  (save-excursion
    (let* ((re (concat ":" org-archive-tag ":")))
      (goto-char beg)
      (while (re-search-forward re end t)
	(when (org-at-heading-p)
	  (org-flag-subtree t)
	  (org-end-of-subtree t))))))

(defun org-flag-subtree (flag)
  (save-excursion
    (org-back-to-heading t)
    (outline-end-of-heading)
    (outline-flag-region (point)
			 (progn (org-end-of-subtree t) (point))
			 flag)))

(defalias 'org-advertized-archive-subtree 'org-archive-subtree)

(eval-and-compile
  (org-autoload "org-archive"
   '(org-add-archive-files org-archive-subtree
     org-archive-to-archive-sibling org-toggle-archive-tag
     org-archive-subtree-default
     org-archive-subtree-default-with-confirmation)))

;; Autoload Column View Code

(declare-function org-columns-number-to-string "org-colview")
(declare-function org-columns-get-format-and-top-level "org-colview")
(declare-function org-columns-compute "org-colview")

(org-autoload (if (featurep 'xemacs) "org-colview-xemacs" "org-colview")
 '(org-columns-number-to-string org-columns-get-format-and-top-level
   org-columns-compute org-agenda-columns org-columns-remove-overlays
   org-columns org-insert-columns-dblock org-dblock-write:columnview))

;; Autoload ID code

(declare-function org-id-store-link "org-id")
(declare-function org-id-locations-load "org-id")
(declare-function org-id-locations-save "org-id")
(defvar org-id-track-globally)
(org-autoload "org-id"
 '(org-id-get-create org-id-new org-id-copy org-id-get
   org-id-get-with-outline-path-completion
   org-id-get-with-outline-drilling org-id-store-link
   org-id-goto org-id-find org-id-store-link))

;; Autoload Plotting Code

(org-autoload "org-plot"
 '(org-plot/gnuplot))

;;; Variables for pre-computed regular expressions, all buffer local

(defvar org-drawer-regexp nil
  "Matches first line of a hidden block.")
(make-variable-buffer-local 'org-drawer-regexp)
(defvar org-todo-regexp nil
  "Matches any of the TODO state keywords.")
(make-variable-buffer-local 'org-todo-regexp)
(defvar org-not-done-regexp nil
  "Matches any of the TODO state keywords except the last one.")
(make-variable-buffer-local 'org-not-done-regexp)
(defvar org-not-done-heading-regexp nil
  "Matches a TODO headline that is not done.")
(make-variable-buffer-local 'org-not-done-regexp)
(defvar org-todo-line-regexp nil
  "Matches a headline and puts TODO state into group 2 if present.")
(make-variable-buffer-local 'org-todo-line-regexp)
(defvar org-complex-heading-regexp nil
  "Matches a headline and puts everything into groups:
group 1: the stars
group 2: The todo keyword, maybe
group 3: Priority cookie
group 4: True headline
group 5: Tags")
(make-variable-buffer-local 'org-complex-heading-regexp)
(defvar org-complex-heading-regexp-format nil
  "Printf format to make regexp to match an exact headline.
This regexp will match the headline of any node which has the
exact headline text that is put into the format, but may have any
TODO state, priority and tags.")
(make-variable-buffer-local 'org-complex-heading-regexp-format)
(defvar org-todo-line-tags-regexp nil
  "Matches a headline and puts TODO state into group 2 if present.
Also put tags into group 4 if tags are present.")
(make-variable-buffer-local 'org-todo-line-tags-regexp)
(defvar org-ds-keyword-length 12
  "Maximum length of the Deadline and SCHEDULED keywords.")
(make-variable-buffer-local 'org-ds-keyword-length)
(defvar org-deadline-regexp nil
  "Matches the DEADLINE keyword.")
(make-variable-buffer-local 'org-deadline-regexp)
(defvar org-deadline-time-regexp nil
  "Matches the DEADLINE keyword together with a time stamp.")
(make-variable-buffer-local 'org-deadline-time-regexp)
(defvar org-deadline-line-regexp nil
  "Matches the DEADLINE keyword and the rest of the line.")
(make-variable-buffer-local 'org-deadline-line-regexp)
(defvar org-scheduled-regexp nil
  "Matches the SCHEDULED keyword.")
(make-variable-buffer-local 'org-scheduled-regexp)
(defvar org-scheduled-time-regexp nil
  "Matches the SCHEDULED keyword together with a time stamp.")
(make-variable-buffer-local 'org-scheduled-time-regexp)
(defvar org-closed-time-regexp nil
  "Matches the CLOSED keyword together with a time stamp.")
(make-variable-buffer-local 'org-closed-time-regexp)

(defvar org-keyword-time-regexp nil
  "Matches any of the 4 keywords, together with the time stamp.")
(make-variable-buffer-local 'org-keyword-time-regexp)
(defvar org-keyword-time-not-clock-regexp nil
  "Matches any of the 3 keywords, together with the time stamp.")
(make-variable-buffer-local 'org-keyword-time-not-clock-regexp)
(defvar org-maybe-keyword-time-regexp nil
  "Matches a timestamp, possibly preceded by a keyword.")
(make-variable-buffer-local 'org-maybe-keyword-time-regexp)
(defvar org-planning-or-clock-line-re nil
  "Matches a line with planning or clock info.")
(make-variable-buffer-local 'org-planning-or-clock-line-re)
(defvar org-all-time-keywords nil
  "List of time keywords.")
(make-variable-buffer-local 'org-all-time-keywords)

(defconst org-plain-time-of-day-regexp
  (concat
   "\\(\\<[012]?[0-9]"
   "\\(\\(:\\([0-5][0-9]\\([AaPp][Mm]\\)?\\)\\)\\|\\([AaPp][Mm]\\)\\)\\>\\)"
   "\\(--?"
   "\\(\\<[012]?[0-9]"
   "\\(\\(:\\([0-5][0-9]\\([AaPp][Mm]\\)?\\)\\)\\|\\([AaPp][Mm]\\)\\)\\>\\)"
   "\\)?")
  "Regular expression to match a plain time or time range.
Examples:  11:45 or 8am-13:15 or 2:45-2:45pm.  After a match, the following
groups carry important information:
0  the full match
1  the first time, range or not
8  the second time, if it is a range.")

(defconst org-plain-time-extension-regexp
  (concat
   "\\(\\<[012]?[0-9]"
   "\\(\\(:\\([0-5][0-9]\\([AaPp][Mm]\\)?\\)\\)\\|\\([AaPp][Mm]\\)\\)\\>\\)"
   "\\+\\([0-9]+\\)\\(:\\([0-5][0-9]\\)\\)?")
  "Regular expression to match a time range like 13:30+2:10 = 13:30-15:40.
Examples:  11:45 or 8am-13:15 or 2:45-2:45pm.  After a match, the following
groups carry important information:
0  the full match
7  hours of duration
9  minutes of duration")

(defconst org-stamp-time-of-day-regexp
  (concat
   "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} +\\sw+ +\\)"
   "\\([012][0-9]:[0-5][0-9]\\(-\\([012][0-9]:[0-5][0-9]\\)\\)?[^\n\r>]*?\\)>"
   "\\(--?"
   "<\\1\\([012][0-9]:[0-5][0-9]\\)>\\)?")
  "Regular expression to match a timestamp time or time range.
After a match, the following groups carry important information:
0  the full match
1  date plus weekday, for back referencing to make sure both times are on the same day
2  the first time, range or not
4  the second time, if it is a range.")

(defconst org-startup-options
  '(("fold" org-startup-folded t)
    ("overview" org-startup-folded t)
    ("nofold" org-startup-folded nil)
    ("showall" org-startup-folded nil)
    ("showeverything" org-startup-folded showeverything)
    ("content" org-startup-folded content)
    ("indent" org-startup-indented t)
    ("noindent" org-startup-indented nil)
    ("hidestars" org-hide-leading-stars t)
    ("showstars" org-hide-leading-stars nil)
    ("odd" org-odd-levels-only t)
    ("oddeven" org-odd-levels-only nil)
    ("align" org-startup-align-all-tables t)
    ("noalign" org-startup-align-all-tables nil)
    ("inlineimages" org-startup-with-inline-images t)
    ("noinlineimages" org-startup-with-inline-images nil)
    ("customtime" org-display-custom-times t)
    ("logdone" org-log-done time)
    ("lognotedone" org-log-done note)
    ("nologdone" org-log-done nil)
    ("lognoteclock-out" org-log-note-clock-out t)
    ("nolognoteclock-out" org-log-note-clock-out nil)
    ("logrepeat" org-log-repeat state)
    ("lognoterepeat" org-log-repeat note)
    ("nologrepeat" org-log-repeat nil)
    ("logreschedule" org-log-reschedule time)
    ("lognotereschedule" org-log-reschedule note)
    ("nologreschedule" org-log-reschedule nil)
    ("logredeadline" org-log-redeadline time)
    ("lognoteredeadline" org-log-redeadline note)
    ("nologredeadline" org-log-redeadline nil)
    ("logrefile" org-log-refile time)
    ("lognoterefile" org-log-refile note)
    ("nologrefile" org-log-refile nil)
    ("fninline" org-footnote-define-inline t)
    ("nofninline" org-footnote-define-inline nil)
    ("fnlocal" org-footnote-section nil)
    ("fnauto" org-footnote-auto-label t)
    ("fnprompt" org-footnote-auto-label nil)
    ("fnconfirm" org-footnote-auto-label confirm)
    ("fnplain" org-footnote-auto-label plain)
    ("fnadjust" org-footnote-auto-adjust t)
    ("nofnadjust" org-footnote-auto-adjust nil)
    ("constcgs" constants-unit-system cgs)
    ("constSI" constants-unit-system SI)
    ("noptag" org-tag-persistent-alist nil)
    ("hideblocks" org-hide-block-startup t)
    ("nohideblocks" org-hide-block-startup nil)
    ("beamer" org-startup-with-beamer-mode t)
    ("entitiespretty" org-pretty-entities t)
    ("entitiesplain" org-pretty-entities nil))
  "Variable associated with STARTUP options for org-mode.
Each element is a list of three items: The startup options as written
in the #+STARTUP line, the corresponding variable, and the value to
set this variable to if the option is found.  An optional forth element PUSH
means to push this value onto the list in the variable.")

(defun org-update-property-plist (key val props)
  "Update PROPS with KEY and VAL."
  (let* ((appending (string= "+" (substring key (- (length key) 1))))
	 (key (if appending (substring key 0 (- (length key) 1)) key))
	 (remainder (org-remove-if (lambda (p) (string= (car p) key)) props))
	 (previous (cdr (assoc key props))))
    (if appending
	(cons (cons key (if previous (concat previous " " val) val)) remainder)
      (cons (cons key val) remainder))))

(defconst org-block-regexp
  "^[ \t]*#\\+begin_?\\([^ \n]+\\)\\(\\([^\n]+\\)\\)?\n\\([^\000]+?\\)#\\+end_?\\1[ \t]*$"
  "Regular expression for hiding blocks.")
(defconst org-heading-keyword-regexp-format
  "^\\(\\*+\\)\\(?: +%s\\)\\(?: +\\(.*?\\)\\)?[ \t]*$"
  "Printf format for a regexp matching an headline with some keyword.
This regexp will match the headline of any node which has the
exact keyword that is put into the format.  The keyword isn't in
any group by default, but the stars and the body are.")
(defconst org-heading-keyword-maybe-regexp-format
  "^\\(\\*+\\)\\(?: +%s\\)?\\(?: +\\(.*?\\)\\)?[ \t]*$"
  "Printf format for a regexp matching an headline, possibly with some keyword.
This regexp can match any headline with the specified keyword, or
without a keyword.  The keyword isn't in any group by default,
but the stars and the body are.")

(defun org-set-regexps-and-options ()
  "Precompute regular expressions for current buffer."
  (when (eq major-mode 'org-mode)
    (org-set-local 'org-todo-kwd-alist nil)
    (org-set-local 'org-todo-key-alist nil)
    (org-set-local 'org-todo-key-trigger nil)
    (org-set-local 'org-todo-keywords-1 nil)
    (org-set-local 'org-done-keywords nil)
    (org-set-local 'org-todo-heads nil)
    (org-set-local 'org-todo-sets nil)
    (org-set-local 'org-todo-log-states nil)
    (org-set-local 'org-file-properties nil)
    (org-set-local 'org-file-tags nil)
    (let ((re (org-make-options-regexp
	       '("CATEGORY" "TODO" "COLUMNS"
		 "STARTUP" "ARCHIVE" "FILETAGS" "TAGS" "LINK" "PRIORITIES"
		 "CONSTANTS" "PROPERTY" "DRAWERS" "SETUPFILE" "LATEX_CLASS"
		 "OPTIONS")
	       "\\(?:[a-zA-Z][0-9a-zA-Z_]*_TODO\\)"))
	  (splitre "[ \t]+")
	  (scripts org-use-sub-superscripts)
	  kwds kws0 kwsa key log value cat arch tags const links hw dws
	  tail sep kws1 prio props ftags drawers beamer-p
	  ext-setup-or-nil setup-contents (start 0))
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char (point-min))
	  (while (or (and ext-setup-or-nil
			  (string-match re ext-setup-or-nil start)
			  (setq start (match-end 0)))
		     (and (setq ext-setup-or-nil nil start 0)
			  (re-search-forward re nil t)))
	    (setq key (upcase (match-string 1 ext-setup-or-nil))
		  value (org-match-string-no-properties 2 ext-setup-or-nil))
	    (if (stringp value) (setq value (org-trim value)))
	    (cond
	     ((equal key "CATEGORY")
	      (setq cat value))
	     ((member key '("SEQ_TODO" "TODO"))
	      (push (cons 'sequence (org-split-string value splitre)) kwds))
	     ((equal key "TYP_TODO")
	      (push (cons 'type (org-split-string value splitre)) kwds))
 	     ((string-match "\\`\\([a-zA-Z][0-9a-zA-Z_]*\\)_TODO\\'" key)
	      ;; general TODO-like setup
 	      (push (cons (intern (downcase (match-string 1 key)))
			  (org-split-string value splitre)) kwds))
	     ((equal key "TAGS")
	      (setq tags (append tags (if tags '("\\n") nil)
				 (org-split-string value splitre))))
	     ((equal key "COLUMNS")
	      (org-set-local 'org-columns-default-format value))
	     ((equal key "LINK")
	      (when (string-match "^\\(\\S-+\\)[ \t]+\\(.+\\)" value)
		(push (cons (match-string 1 value)
			    (org-trim (match-string 2 value)))
		      links)))
	     ((equal key "PRIORITIES")
	      (setq prio (org-split-string value " +")))
	     ((equal key "PROPERTY")
	      (when (string-match "\\(\\S-+\\)\\s-+\\(.*\\)" value)
		(setq props (org-update-property-plist (match-string 1 value)
						       (match-string 2 value)
						       props))))
	     ((equal key "FILETAGS")
	      (when (string-match "\\S-" value)
		(setq ftags
		      (append
		       ftags
		       (apply 'append
			      (mapcar (lambda (x) (org-split-string x ":"))
				      (org-split-string value)))))))
	     ((equal key "DRAWERS")
	      (setq drawers (org-split-string value splitre)))
	     ((equal key "CONSTANTS")
	      (setq const (append const (org-split-string value splitre))))
	     ((equal key "STARTUP")
	      (let ((opts (org-split-string value splitre))
		    l var val)
		(while (setq l (pop opts))
		  (when (setq l (assoc l org-startup-options))
		    (setq var (nth 1 l) val (nth 2 l))
		    (if (not (nth 3 l))
			(set (make-local-variable var) val)
		      (if (not (listp (symbol-value var)))
			  (set (make-local-variable var) nil))
		      (set (make-local-variable var) (symbol-value var))
		      (add-to-list var val))))))
	     ((equal key "ARCHIVE")
	      (setq arch value)
	      (remove-text-properties 0 (length arch)
				      '(face t fontified t) arch))
	     ((equal key "LATEX_CLASS")
	      (setq beamer-p (equal value "beamer")))
	     ((equal key "OPTIONS")
	      (if (string-match "\\([ \t]\\|\\`\\)\\^:\\(t\\|nil\\|{}\\)" value)
		  (setq scripts (read (match-string 2 value)))))
	     ((equal key "SETUPFILE")
	      (setq setup-contents (org-file-contents
				    (expand-file-name
				     (org-remove-double-quotes value))
				    'noerror))
	      (if (not ext-setup-or-nil)
		  (setq ext-setup-or-nil setup-contents start 0)
		(setq ext-setup-or-nil
		      (concat (substring ext-setup-or-nil 0 start)
			      "\n" setup-contents "\n"
			      (substring ext-setup-or-nil start)))))))
	  ;; search for property blocks
	  (goto-char (point-min))
	  (while (re-search-forward org-block-regexp nil t)
	    (when (equal "PROPERTY" (upcase (match-string 1)))
	      (setq value (replace-regexp-in-string
			   "[\n\r]" " " (match-string 4)))
	      (when (string-match "\\(\\S-+\\)\\s-+\\(.*\\)" value)
		(setq props (org-update-property-plist (match-string 1 value)
						       (match-string 2 value)
						       props)))))))
      (org-set-local 'org-use-sub-superscripts scripts)
      (when cat
	(org-set-local 'org-category (intern cat))
	(push (cons "CATEGORY" cat) props))
      (when prio
	(if (< (length prio) 3) (setq prio '("A" "C" "B")))
	(setq prio (mapcar 'string-to-char prio))
	(org-set-local 'org-highest-priority (nth 0 prio))
	(org-set-local 'org-lowest-priority  (nth 1 prio))
	(org-set-local 'org-default-priority (nth 2 prio)))
      (and props (org-set-local 'org-file-properties (nreverse props)))
      (and ftags (org-set-local 'org-file-tags
				(mapcar 'org-add-prop-inherited ftags)))
      (and drawers (org-set-local 'org-drawers drawers))
      (and arch (org-set-local 'org-archive-location arch))
      (and links (setq org-link-abbrev-alist-local (nreverse links)))
      ;; Process the TODO keywords
      (unless kwds
	;; Use the global values as if they had been given locally.
	(setq kwds (default-value 'org-todo-keywords))
	(if (stringp (car kwds))
	    (setq kwds (list (cons org-todo-interpretation
				   (default-value 'org-todo-keywords)))))
	(setq kwds (reverse kwds)))
      (setq kwds (nreverse kwds))
      (let (inter kws kw)
	(while (setq kws (pop kwds))
 	  (let ((kws (or
		      (run-hook-with-args-until-success
 		       'org-todo-setup-filter-hook kws)
		      kws)))
	    (setq inter (pop kws) sep (member "|" kws)
		  kws0 (delete "|" (copy-sequence kws))
		  kwsa nil
		  kws1 (mapcar
			(lambda (x)
			  ;;                     1              2
			  (if (string-match "^\\(.*?\\)\\(?:(\\([^!@/]\\)?.*?)\\)?$" x)
			      (progn
				(setq kw (match-string 1 x)
				      key (and (match-end 2) (match-string 2 x))
				      log (org-extract-log-state-settings x))
				(push (cons kw (and key (string-to-char key))) kwsa)
				(and log (push log org-todo-log-states))
				kw)
			    (error "Invalid TODO keyword %s" x)))
			kws0)
		  kwsa (if kwsa (append '((:startgroup))
					(nreverse kwsa)
					'((:endgroup))))
		  hw (car kws1)
		  dws (if sep (org-remove-keyword-keys (cdr sep)) (last kws1))
		  tail (list inter hw (car dws) (org-last dws))))
	  (add-to-list 'org-todo-heads hw 'append)
	  (push kws1 org-todo-sets)
	  (setq org-done-keywords (append org-done-keywords dws nil))
	  (setq org-todo-key-alist (append org-todo-key-alist kwsa))
	  (mapc (lambda (x) (push (cons x tail) org-todo-kwd-alist)) kws1)
	  (setq org-todo-keywords-1 (append org-todo-keywords-1 kws1 nil)))
	(setq org-todo-sets (nreverse org-todo-sets)
	      org-todo-kwd-alist (nreverse org-todo-kwd-alist)
	      org-todo-key-trigger (delq nil (mapcar 'cdr org-todo-key-alist))
	      org-todo-key-alist (org-assign-fast-keys org-todo-key-alist)))
      ;; Process the constants
      (when const
	(let (e cst)
	  (while (setq e (pop const))
	    (if (string-match "^\\([a-zA-Z0][_a-zA-Z0-9]*\\)=\\(.*\\)" e)
		(push (cons (match-string 1 e) (match-string 2 e)) cst)))
	  (setq org-table-formula-constants-local cst)))

      ;; Process the tags.
      (when tags
	(let (e tgs)
	  (while (setq e (pop tags))
	    (cond
	     ((equal e "{") (push '(:startgroup) tgs))
	     ((equal e "}") (push '(:endgroup) tgs))
	     ((equal e "\\n") (push '(:newline) tgs))
	     ((string-match (org-re "^\\([[:alnum:]_@#%]+\\)(\\(.\\))$") e)
	      (push (cons (match-string 1 e)
			  (string-to-char (match-string 2 e)))
		    tgs))
	     (t (push (list e) tgs))))
	  (org-set-local 'org-tag-alist nil)
	  (while (setq e (pop tgs))
	    (or (and (stringp (car e))
		     (assoc (car e) org-tag-alist))
		(push e org-tag-alist)))))

      ;; Compute the regular expressions and other local variables.
      ;; Using `org-outline-regexp-bol' would complicate them much,
      ;; because of the fixed white space at the end of that string.
      (if (not org-done-keywords)
	  (setq org-done-keywords (and org-todo-keywords-1
				       (list (org-last org-todo-keywords-1)))))
      (setq org-ds-keyword-length (+ 2 (max (length org-deadline-string)
					    (length org-scheduled-string)
					    (length org-clock-string)
					    (length org-closed-string)))
	    org-drawer-regexp
	    (concat "^[ \t]*:\\("
		    (mapconcat 'regexp-quote org-drawers "\\|")
		    "\\):[ \t]*$")
	    org-not-done-keywords
	    (org-delete-all org-done-keywords (copy-sequence org-todo-keywords-1))
	    org-todo-regexp
	    (concat "\\("
		    (mapconcat 'regexp-quote org-todo-keywords-1 "\\|")
		    "\\)")
	    org-not-done-regexp
	    (concat "\\("
		    (mapconcat 'regexp-quote org-not-done-keywords "\\|")
		    "\\)")
	    org-not-done-heading-regexp
	    (format org-heading-keyword-regexp-format org-not-done-regexp)
	    org-todo-line-regexp
	    (format org-heading-keyword-maybe-regexp-format org-todo-regexp)
	    org-complex-heading-regexp
	    (concat "^\\(\\*+\\)"
		    "\\(?: +" org-todo-regexp "\\)?"
		    "\\(?: +\\(\\[#.\\]\\)\\)?"
		    "\\(?: +\\(.*?\\)\\)?"
		    (org-re "\\(?:[ \t]+\\(:[[:alnum:]_@#%:]+:\\)\\)?")
		    "[ \t]*$")
	    org-complex-heading-regexp-format
	    (concat "^\\(\\*+\\)"
		    "\\(?: +" org-todo-regexp "\\)?"
		    "\\(?: +\\(\\[#.\\]\\)\\)?"
		    "\\(?: +"
		    ;; Stats cookies can be stuck to body.
		    "\\(?:\\[[0-9%%/]+\\] *\\)?"
		    "\\(%s\\)"
		    "\\(?: *\\[[0-9%%/]+\\]\\)?"
		    "\\)"
		    (org-re "\\(?:[ \t]+\\(:[[:alnum:]_@#%%:]+:\\)\\)?")
		    "[ \t]*$")
	    org-todo-line-tags-regexp
	    (concat "^\\(\\*+\\)"
		    "\\(?: +" org-todo-regexp "\\)?"
		    "\\(?: +\\(.*?\\)\\)?"
		    (org-re "\\(?:[ \t]+\\(:[[:alnum:]:_@#%]+:\\)\\)?")
		    "[ \t]*$")
	    org-deadline-regexp (concat "\\<" org-deadline-string)
	    org-deadline-time-regexp
	    (concat "\\<" org-deadline-string " *<\\([^>]+\\)>")
	    org-deadline-line-regexp
	    (concat "\\<\\(" org-deadline-string "\\).*")
	    org-scheduled-regexp
	    (concat "\\<" org-scheduled-string)
	    org-scheduled-time-regexp
	    (concat "\\<" org-scheduled-string " *<\\([^>]+\\)>")
	    org-closed-time-regexp
	    (concat "\\<" org-closed-string " *\\[\\([^]]+\\)\\]")
	    org-keyword-time-regexp
	    (concat "\\<\\(" org-scheduled-string
		    "\\|" org-deadline-string
		    "\\|" org-closed-string
		    "\\|" org-clock-string "\\)"
		    " *[[<]\\([^]>]+\\)[]>]")
	    org-keyword-time-not-clock-regexp
	    (concat "\\<\\(" org-scheduled-string
		    "\\|" org-deadline-string
		    "\\|" org-closed-string
		    "\\)"
		    " *[[<]\\([^]>]+\\)[]>]")
	    org-maybe-keyword-time-regexp
	    (concat "\\(\\<\\(" org-scheduled-string
		    "\\|" org-deadline-string
		    "\\|" org-closed-string
		    "\\|" org-clock-string "\\)\\)?"
		    " *\\([[<][0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} ?[^]\r\n>]*?[]>]\\|<%%([^\r\n>]*>\\)")
	    org-planning-or-clock-line-re
	    (concat "^[ \t]*\\("
		    org-scheduled-string "\\|"
		    org-deadline-string "\\|"
		    org-closed-string "\\|"
		    org-clock-string "\\)")
	    org-all-time-keywords
	    (mapcar (lambda (w) (substring w 0 -1))
		    (list org-scheduled-string org-deadline-string
			  org-clock-string org-closed-string))
	    )
      (org-compute-latex-and-specials-regexp)
      (org-set-font-lock-defaults))))

(defun org-file-contents (file &optional noerror)
  "Return the contents of FILE, as a string."
  (if (or (not file)
	  (not (file-readable-p file)))
      (if noerror
	  (progn
	    (message "Cannot read file \"%s\"" file)
	    (ding) (sit-for 2)
	    "")
	(error "Cannot read file \"%s\"" file))
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))

(defun org-extract-log-state-settings (x)
  "Extract the log state setting from a TODO keyword string.
This will extract info from a string like \"WAIT(w@/!)\"."
  (let (kw key log1 log2)
    (when (string-match "^\\(.*?\\)\\(?:(\\([^!@/]\\)?\\([!@]\\)?\\(?:/\\([!@]\\)\\)?)\\)?$" x)
      (setq kw (match-string 1 x)
	    key (and (match-end 2) (match-string 2 x))
	    log1 (and (match-end 3) (match-string 3 x))
	    log2 (and (match-end 4) (match-string 4 x)))
      (and (or log1 log2)
	   (list kw
		 (and log1 (if (equal log1 "!") 'time 'note))
		 (and log2 (if (equal log2 "!") 'time 'note)))))))

(defun org-remove-keyword-keys (list)
  "Remove a pair of parenthesis at the end of each string in LIST."
  (mapcar (lambda (x)
	    (if (string-match "(.*)$" x)
		(substring x 0 (match-beginning 0))
	      x))
	  list))

(defun org-assign-fast-keys (alist)
  "Assign fast keys to a keyword-key alist.
Respect keys that are already there."
  (let (new e (alt ?0))
    (while (setq e (pop alist))
      (if (or (memq (car e) '(:newline :endgroup :startgroup))
	      (cdr e)) ;; Key already assigned.
	  (push e new)
	(let ((clist (string-to-list (downcase (car e))))
	      (used (append new alist)))
	  (when (= (car clist) ?@)
	    (pop clist))
	  (while (and clist (rassoc (car clist) used))
	    (pop clist))
	  (unless clist
	    (while (rassoc alt used)
	      (incf alt)))
	  (push (cons (car e) (or (car clist) alt)) new))))
    (nreverse new)))

;;; Some variables used in various places

(defvar org-window-configuration nil
  "Used in various places to store a window configuration.")
(defvar org-selected-window nil
  "Used in various places to store a window configuration.")
(defvar org-finish-function nil
  "Function to be called when `C-c C-c' is used.
This is for getting out of special buffers like remember.")


;; FIXME: Occasionally check by commenting these, to make sure
;;        no other functions uses these, forgetting to let-bind them.
(defvar entry)
(defvar org-last-state)
(defvar date)

;; Defined somewhere in this file, but used before definition.
(defvar org-entities)     ;; defined in org-entities.el
(defvar org-struct-menu)
(defvar org-org-menu)
(defvar org-tbl-menu)

;;;; Define the Org-mode

(if (and (not (keymapp outline-mode-map)) (featurep 'allout))
    (error "Conflict with outdated version of allout.el.  Load org.el before allout.el, or upgrade to newer allout, for example by switching to Emacs 22"))


;; We use a before-change function to check if a table might need
;; an update.
(defvar org-table-may-need-update t
  "Indicates that a table might need an update.
This variable is set by `org-before-change-function'.
`org-table-align' sets it back to nil.")
(defun org-before-change-function (beg end)
  "Every change indicates that a table might need an update."
  (setq org-table-may-need-update t))
(defvar org-mode-map)
(defvar org-inhibit-startup nil)        ; Dynamically-scoped param.
(defvar org-inhibit-startup-visibility-stuff nil) ; Dynamically-scoped param.
(defvar org-agenda-keep-modes nil)      ; Dynamically-scoped param.
(defvar org-inhibit-logging nil)        ; Dynamically-scoped param.
(defvar org-inhibit-blocking nil)       ; Dynamically-scoped param.
(defvar org-table-buffer-is-an nil)

;; `org-outline-regexp' ought to be a defconst but is let-binding in
;; some places -- e.g. see the macro org-with-limited-levels.
;;
;; In Org buffers, the value of `outline-regexp' is that of
;; `org-outline-regexp'.  The only function still directly relying on
;; `outline-regexp' is `org-overview' so that `org-cycle' can do its
;; job when `orgstruct-mode' is active.
(defvar org-outline-regexp "\\*+ "
  "Regexp to match Org headlines.")
(defconst org-outline-regexp-bol "^\\*+ "
  "Regexp to match Org headlines.
This is similar to `org-outline-regexp' but additionally makes
sure that we are at the beginning of the line.")

(defconst org-heading-regexp "^\\(\\*+\\)\\(?: +\\(.*?\\)\\)?[ \t]*$"
  "Matches an headline, putting stars and text into groups.
Stars are put in group 1 and the trimmed body in group 2.")

(defvar buffer-face-mode-face)

;;;###autoload
(define-derived-mode org-mode outline-mode "Org"
  "Outline-based notes management and organizer, alias
\"Carsten's outline-mode for keeping track of everything.\"

Org-mode develops organizational tasks around a NOTES file which
contains information about projects as plain text.  Org-mode is
implemented on top of outline-mode, which is ideal to keep the content
of large files well structured.  It supports ToDo items, deadlines and
time stamps, which magically appear in the diary listing of the Emacs
calendar.  Tables are easily created with a built-in table editor.
Plain text URL-like links connect to websites, emails (VM), Usenet
messages (Gnus), BBDB entries, and any files related to the project.
For printing and sharing of notes, an Org-mode file (or a part of it)
can be exported as a structured ASCII or HTML file.

The following commands are available:

\\{org-mode-map}"

  ;; Get rid of Outline menus, they are not needed
  ;; Need to do this here because define-derived-mode sets up
  ;; the keymap so late.  Still, it is a waste to call this each time
  ;; we switch another buffer into org-mode.
  (if (featurep 'xemacs)
      (when (boundp 'outline-mode-menu-heading)
	;; Assume this is Greg's port, it uses easymenu
	(easy-menu-remove outline-mode-menu-heading)
	(easy-menu-remove outline-mode-menu-show)
	(easy-menu-remove outline-mode-menu-hide))
    (define-key org-mode-map [menu-bar headings] 'undefined)
    (define-key org-mode-map [menu-bar hide] 'undefined)
    (define-key org-mode-map [menu-bar show] 'undefined))

  (org-load-modules-maybe)
  (easy-menu-add org-org-menu)
  (easy-menu-add org-tbl-menu)
  (org-install-agenda-files-menu)
  (if org-descriptive-links (add-to-invisibility-spec '(org-link)))
  (add-to-invisibility-spec '(org-cwidth))
  (add-to-invisibility-spec '(org-hide-block . t))
  (when (featurep 'xemacs)
    (org-set-local 'line-move-ignore-invisible t))
  (org-set-local 'outline-regexp org-outline-regexp)
  (org-set-local 'outline-level 'org-outline-level)
  (setq bidi-paragraph-direction 'left-to-right)
  (when (and org-ellipsis
             (fboundp 'set-display-table-slot) (boundp 'buffer-display-table)
	     (fboundp 'make-glyph-code))
    (unless org-display-table
      (setq org-display-table (make-display-table)))
    (set-display-table-slot
     org-display-table 4
     (vconcat (mapcar
	       (lambda (c) (make-glyph-code c (and (not (stringp org-ellipsis))
						   org-ellipsis)))
	       (if (stringp org-ellipsis) org-ellipsis "..."))))
    (setq buffer-display-table org-display-table))
  (org-set-regexps-and-options)
  (when (and org-tag-faces (not org-tags-special-faces-re))
    ;; tag faces set outside customize.... force initialization.
    (org-set-tag-faces 'org-tag-faces org-tag-faces))
  ;; Calc embedded
  (org-set-local 'calc-embedded-open-mode "# ")
  (modify-syntax-entry ?@ "w")
  (if org-startup-truncated (setq truncate-lines t))
  (org-set-local 'font-lock-unfontify-region-function
		 'org-unfontify-region)
  ;; Activate before-change-function
  (org-set-local 'org-table-may-need-update t)
  (org-add-hook 'before-change-functions 'org-before-change-function nil
		'local)
  ;; Check for running clock before killing a buffer
  (org-add-hook 'kill-buffer-hook 'org-check-running-clock nil 'local)
  ;; Paragraphs and auto-filling
  (org-set-autofill-regexps)
  (setq indent-line-function 'org-indent-line-function)
  (org-update-radio-target-regexp)
  ;; Beginning/end of defun
  (org-set-local 'beginning-of-defun-function 'org-beginning-of-defun)
  (org-set-local 'end-of-defun-function 'org-end-of-defun)
  ;; Next error for sparse trees
  (org-set-local 'next-error-function 'org-occur-next-match)
  ;; Make sure dependence stuff works reliably, even for users who set it
  ;; too late :-(
  (if org-enforce-todo-dependencies
      (add-hook 'org-blocker-hook
		'org-block-todo-from-children-or-siblings-or-parent)
    (remove-hook 'org-blocker-hook
		 'org-block-todo-from-children-or-siblings-or-parent))
  (if org-enforce-todo-checkbox-dependencies
      (add-hook 'org-blocker-hook
		'org-block-todo-from-checkboxes)
    (remove-hook 'org-blocker-hook
		 'org-block-todo-from-checkboxes))

  ;; Comment characters
  (org-set-local 'comment-start "#")
  (org-set-local 'comment-padding " ")

  ;; Align options lines
  (org-set-local
   'align-mode-rules-list
   '((org-in-buffer-settings
      (regexp . "^#\\+[A-Z_]+:\\(\\s-*\\)\\S-+")
      (modes . '(org-mode)))))

  ;; Imenu
  (org-set-local 'imenu-create-index-function
		 'org-imenu-get-tree)

  ;; Make isearch reveal context
  (if (or (featurep 'xemacs)
	  (not (boundp 'outline-isearch-open-invisible-function)))
      ;; Emacs 21 and XEmacs make use of the hook
      (org-add-hook 'isearch-mode-end-hook 'org-isearch-end 'append 'local)
    ;; Emacs 22 deals with this through a special variable
    (org-set-local 'outline-isearch-open-invisible-function
		   (lambda (&rest ignore) (org-show-context 'isearch))))

  ;; Turn on org-beamer-mode?
  (and org-startup-with-beamer-mode (org-beamer-mode 1))

  ;; Setup the pcomplete hooks
  (set (make-local-variable 'pcomplete-command-completion-function)
       'org-pcomplete-initial)
  (set (make-local-variable 'pcomplete-command-name-function)
       'org-command-at-point)
  (set (make-local-variable 'pcomplete-default-completion-function)
       'ignore)
  (set (make-local-variable 'pcomplete-parse-arguments-function)
       'org-parse-arguments)
  (set (make-local-variable 'pcomplete-termination-string) "")
  (when (>= emacs-major-version 23)
    (set (make-local-variable 'buffer-face-mode-face) 'org-default))

  ;; If empty file that did not turn on org-mode automatically, make it to.
  (if (and org-insert-mode-line-in-empty-file
	   (org-called-interactively-p 'any)
	   (= (point-min) (point-max)))
      (insert "#    -*- mode: org -*-\n\n"))
  (unless org-inhibit-startup
    (when org-startup-align-all-tables
      (let ((bmp (buffer-modified-p)))
	(org-table-map-tables 'org-table-align 'quietly)
	(set-buffer-modified-p bmp)))
    (when org-startup-with-inline-images
      (org-display-inline-images))
    (when org-startup-indented
      (require 'org-indent)
      (org-indent-mode 1))
    (unless org-inhibit-startup-visibility-stuff
      (org-set-startup-visibility))))

(when (fboundp 'abbrev-table-put)
  (abbrev-table-put org-mode-abbrev-table
		    :parents (list text-mode-abbrev-table)))

(put 'org-mode 'flyspell-mode-predicate 'org-mode-flyspell-verify)

(defun org-current-time ()
  "Current time, possibly rounded to `org-time-stamp-rounding-minutes'."
  (if (> (car org-time-stamp-rounding-minutes) 1)
      (let ((r (car org-time-stamp-rounding-minutes))
	    (time (decode-time)))
	(apply 'encode-time
	       (append (list 0 (* r (floor (+ .5 (/ (float (nth 1 time)) r)))))
		       (nthcdr 2 time))))
    (current-time)))

(defun org-today ()
  "Return today date, considering `org-extend-today-until'."
  (time-to-days
   (time-subtract (current-time)
		  (list 0 (* 3600 org-extend-today-until) 0))))

;;;; Font-Lock stuff, including the activators

(defvar org-mouse-map (make-sparse-keymap))
(org-defkey org-mouse-map [mouse-2] 'org-open-at-mouse)
(org-defkey org-mouse-map [mouse-3] 'org-find-file-at-mouse)
(when org-mouse-1-follows-link
  (org-defkey org-mouse-map [follow-link] 'mouse-face))
(when org-tab-follows-link
  (org-defkey org-mouse-map [(tab)] 'org-open-at-point)
  (org-defkey org-mouse-map "\C-i" 'org-open-at-point))

(require 'font-lock)

(defconst org-non-link-chars "]\t\n\r<>")
(defvar org-link-types '("http" "https" "ftp" "mailto" "file" "news"
			   "shell" "elisp" "doi" "message"))
(defvar org-link-types-re nil
   "Matches a link that has a url-like prefix like \"http:\"")
(defvar org-link-re-with-space nil
   "Matches a link with spaces, optional angular brackets around it.")
(defvar org-link-re-with-space2 nil
   "Matches a link with spaces, optional angular brackets around it.")
(defvar org-link-re-with-space3 nil
   "Matches a link with spaces, only for internal part in bracket links.")
(defvar org-angle-link-re nil
   "Matches link with angular brackets, spaces are allowed.")
(defvar org-plain-link-re nil
   "Matches plain link, without spaces.")
(defvar org-bracket-link-regexp nil
  "Matches a link in double brackets.")
(defvar org-bracket-link-analytic-regexp nil
  "Regular expression used to analyze links.
Here is what the match groups contain after a match:
1: http:
2: http
3: path
4: [desc]
5: desc")
(defvar org-bracket-link-analytic-regexp++ nil
  "Like `org-bracket-link-analytic-regexp', but include coderef internal type.")
(defvar org-any-link-re nil
  "Regular expression matching any link.")

(defcustom org-match-sexp-depth 3
  "Number of stacked braces for sub/superscript matching.
This has to be set before loading org.el to be effective."
  :group 'org-export-translation ; ??????????????????????????/
  :type 'integer)

(defun org-create-multibrace-regexp (left right n)
  "Create a regular expression which will match a balanced sexp.
Opening delimiter is LEFT, and closing delimiter is RIGHT, both given
as single character strings.
The regexp returned will match the entire expression including the
delimiters.  It will also define a single group which contains the
match except for the outermost delimiters.  The maximum depth of
stacked delimiters is N.  Escaping delimiters is not possible."
  (let* ((nothing (concat "[^" left right "]*?"))
	 (or "\\|")
	 (re nothing)
	 (next (concat "\\(?:" nothing left nothing right "\\)+" nothing)))
    (while (> n 1)
      (setq n (1- n)
	    re (concat re or next)
	    next (concat "\\(?:" nothing left next right "\\)+" nothing)))
    (concat left "\\(" re "\\)" right)))

(defvar org-match-substring-regexp
  (concat
   "\\([^\\]\\|^\\)\\([_^]\\)\\("
   "\\(" (org-create-multibrace-regexp "{" "}" org-match-sexp-depth) "\\)"
   "\\|"
   "\\(" (org-create-multibrace-regexp "(" ")" org-match-sexp-depth) "\\)"
   "\\|"
   "\\(\\(?:\\*\\|[-+]?[^-+*!@#$%^_ \t\r\n,:\"?<>~;./{}=()]+\\)\\)\\)")
  "The regular expression matching a sub- or superscript.")

(defvar org-match-substring-with-braces-regexp
  (concat
   "\\([^\\]\\|^\\)\\([_^]\\)\\("
   "\\(" (org-create-multibrace-regexp "{" "}" org-match-sexp-depth) "\\)"
   "\\)")
  "The regular expression matching a sub- or superscript, forcing braces.")

(defun org-make-link-regexps ()
  "Update the link regular expressions.
This should be called after the variable `org-link-types' has changed."
  (setq org-link-types-re
	(concat
	 "\\`\\(" (mapconcat 'regexp-quote org-link-types "\\|") "\\):")
	org-link-re-with-space
	(concat
	 "<?\\(" (mapconcat 'regexp-quote org-link-types "\\|") "\\):"
	 "\\([^" org-non-link-chars " ]"
	 "[^" org-non-link-chars "]*"
	 "[^" org-non-link-chars " ]\\)>?")
	org-link-re-with-space2
	(concat
	 "<?\\(" (mapconcat 'regexp-quote org-link-types "\\|") "\\):"
	 "\\([^" org-non-link-chars " ]"
	 "[^\t\n\r]*"
	 "[^" org-non-link-chars " ]\\)>?")
	org-link-re-with-space3
	(concat
	 "<?\\(" (mapconcat 'regexp-quote org-link-types "\\|") "\\):"
	 "\\([^" org-non-link-chars " ]"
	 "[^\t\n\r]*\\)")
	org-angle-link-re
	(concat
	 "<\\(" (mapconcat 'regexp-quote org-link-types "\\|") "\\):"
	 "\\([^" org-non-link-chars " ]"
	 "[^" org-non-link-chars "]*"
	 "\\)>")
	org-plain-link-re
	(concat
	 "\\<\\(" (mapconcat 'regexp-quote org-link-types "\\|") "\\):"
	 (org-re "\\([^ \t\n()<>]+\\(?:([[:word:]0-9_]+)\\|\\([^[:punct:] \t\n]\\|/\\)\\)\\)"))
	;;	 "\\([^]\t\n\r<>() ]+[^]\t\n\r<>,.;() ]\\)")
	org-bracket-link-regexp
	"\\[\\[\\([^][]+\\)\\]\\(\\[\\([^][]+\\)\\]\\)?\\]"
	org-bracket-link-analytic-regexp
	(concat
	 "\\[\\["
	 "\\(\\(" (mapconcat 'regexp-quote org-link-types "\\|") "\\):\\)?"
	 "\\([^]]+\\)"
	 "\\]"
	 "\\(\\[" "\\([^]]+\\)" "\\]\\)?"
	 "\\]")
	org-bracket-link-analytic-regexp++
	(concat
	 "\\[\\["
	 "\\(\\(" (mapconcat 'regexp-quote (cons "coderef" org-link-types) "\\|") "\\):\\)?"
	 "\\([^]]+\\)"
	 "\\]"
	 "\\(\\[" "\\([^]]+\\)" "\\]\\)?"
	 "\\]")
	org-any-link-re
	(concat "\\(" org-bracket-link-regexp "\\)\\|\\("
		org-angle-link-re "\\)\\|\\("
		org-plain-link-re "\\)")))

(org-make-link-regexps)

(defconst org-ts-regexp "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} ?[^\r\n>]*?\\)>"
  "Regular expression for fast time stamp matching.")
(defconst org-ts-regexp-both "[[<]\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} ?[^]\r\n>]*?\\)[]>]"
  "Regular expression for fast time stamp matching.")
(defconst org-ts-regexp0 "\\(\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\) *\\([^]+0-9>\r\n -]*\\)\\( \\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\)?\\)"
  "Regular expression matching time strings for analysis.
This one does not require the space after the date, so it can be used
on a string that terminates immediately after the date.")
(defconst org-ts-regexp1 "\\(\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\) *\\([^]+0-9>\r\n -]*\\)\\( \\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\)?\\)"
  "Regular expression matching time strings for analysis.")
(defconst org-ts-regexp2 (concat "<" org-ts-regexp1 "[^>\n]\\{0,16\\}>")
  "Regular expression matching time stamps, with groups.")
(defconst org-ts-regexp3 (concat "[[<]" org-ts-regexp1 "[^]>\n]\\{0,16\\}[]>]")
  "Regular expression matching time stamps (also [..]), with groups.")
(defconst org-tr-regexp (concat org-ts-regexp "--?-?" org-ts-regexp)
  "Regular expression matching a time stamp range.")
(defconst org-tr-regexp-both
  (concat org-ts-regexp-both "--?-?" org-ts-regexp-both)
  "Regular expression matching a time stamp range.")
(defconst org-tsr-regexp (concat org-ts-regexp "\\(--?-?"
				 org-ts-regexp "\\)?")
  "Regular expression matching a time stamp or time stamp range.")
(defconst org-tsr-regexp-both (concat org-ts-regexp-both "\\(--?-?"
				      org-ts-regexp-both "\\)?")
  "Regular expression matching a time stamp or time stamp range.
The time stamps may be either active or inactive.")

(defvar org-emph-face nil)

(defun org-do-emphasis-faces (limit)
  "Run through the buffer and add overlays to emphasized strings."
  (let (rtn a)
    (while (and (not rtn) (re-search-forward org-emph-re limit t))
      (if (not (= (char-after (match-beginning 3))
		  (char-after (match-beginning 4))))
	  (progn
	    (setq rtn t)
	    (setq a (assoc (match-string 3) org-emphasis-alist))
	    (font-lock-prepend-text-property (match-beginning 2) (match-end 2)
					     'face
					     (nth 1 a))
	    (and (nth 4 a)
		 (org-remove-flyspell-overlays-in
		  (match-beginning 0) (match-end 0)))
	    (add-text-properties (match-beginning 2) (match-end 2)
				 '(font-lock-multiline t org-emphasis t))
	    (when org-hide-emphasis-markers
	      (add-text-properties (match-end 4) (match-beginning 5)
				   '(invisible org-link))
	      (add-text-properties (match-beginning 3) (match-end 3)
				   '(invisible org-link)))))
      (backward-char 1))
    rtn))

(defun org-emphasize (&optional char)
  "Insert or change an emphasis, i.e. a font like bold or italic.
If there is an active region, change that region to a new emphasis.
If there is no region, just insert the marker characters and position
the cursor between them.
CHAR should be either the marker character, or the first character of the
HTML tag associated with that emphasis.  If CHAR is a space, the means
to remove the emphasis of the selected region.
If char is not given (for example in an interactive call) it
will be prompted for."
  (interactive)
  (let ((eal org-emphasis-alist) e det
	(erc org-emphasis-regexp-components)
	(prompt "")
	(string "") beg end move tag c s)
    (if (org-region-active-p)
	(setq beg (region-beginning) end (region-end)
	      string (buffer-substring beg end))
      (setq move t))

    (while (setq e (pop eal))
      (setq tag (car (org-split-string (nth 2 e) "[ <>/]+"))
	    c (aref tag 0))
      (push (cons c (string-to-char (car e))) det)
      (setq prompt (concat prompt (format " [%s%c]%s" (car e) c
					  (substring tag 1)))))
    (setq det (nreverse det))
    (unless char
      (message "%s" (concat "Emphasis marker or tag:" prompt))
      (setq char (read-char-exclusive)))
    (setq char (or (cdr (assoc char det)) char))
    (if (equal char ?\ )
	(setq s "" move nil)
      (unless (assoc (char-to-string char) org-emphasis-alist)
	(error "No such emphasis marker: \"%c\"" char))
      (setq s (char-to-string char)))
    (while (and (> (length string) 1)
		(equal (substring string 0 1) (substring string -1))
		(assoc (substring string 0 1) org-emphasis-alist))
      (setq string (substring string 1 -1)))
    (setq string (concat s string s))
    (if beg (delete-region beg end))
    (unless (or (bolp)
		(string-match (concat "[" (nth 0 erc) "\n]")
			      (char-to-string (char-before (point)))))
      (insert " "))
    (unless (or (eobp)
		(string-match (concat "[" (nth 1 erc) "\n]")
			      (char-to-string (char-after (point)))))
      (insert " ") (backward-char 1))
    (insert string)
    (and move (backward-char 1))))

(defconst org-nonsticky-props
  '(mouse-face highlight keymap invisible intangible help-echo org-linked-text))

(defsubst org-rear-nonsticky-at (pos)
  (add-text-properties (1- pos) pos (list 'rear-nonsticky org-nonsticky-props)))

(defun org-activate-plain-links (limit)
  "Run through the buffer and add overlays to links."
  (catch 'exit
    (let (f)
      (if (re-search-forward org-plain-link-re limit t)
	  (progn
	    (org-remove-flyspell-overlays-in (match-beginning 0) (match-end 0))
	    (setq f (get-text-property (match-beginning 0) 'face))
	    (if (or (eq f 'org-tag)
		    (and (listp f) (memq 'org-tag f)))
		nil
	      (add-text-properties (match-beginning 0) (match-end 0)
				   (list 'mouse-face 'highlight
					 'face 'org-link
					 'keymap org-mouse-map))
	      (org-rear-nonsticky-at (match-end 0)))
	    t)))))

(defun org-activate-code (limit)
  (if (re-search-forward "^[ \t]*\\(:\\(?: .*\\|$\\)\n?\\)" limit t)
      (progn
	(org-remove-flyspell-overlays-in (match-beginning 0) (match-end 0))
	(remove-text-properties (match-beginning 0) (match-end 0)
				'(display t invisible t intangible t))
	t)))

(defcustom org-src-fontify-natively nil
  "When non-nil, fontify code in code blocks."
  :type 'boolean
  :version "24.1"
  :group 'org-appearance
  :group 'org-babel)

(defun org-fontify-meta-lines-and-blocks (limit)
  (condition-case nil
      (org-fontify-meta-lines-and-blocks-1 limit)
    (error (message "org-mode fontification error"))))

(defun org-fontify-meta-lines-and-blocks-1 (limit)
  "Fontify #+ lines and blocks, in the correct ways."
  (let ((case-fold-search t))
    (if (re-search-forward
	 "^\\([ \t]*#\\+\\(\\([a-zA-Z]+:?\\| \\|$\\)\\(_\\([a-zA-Z]+\\)\\)?\\)[ \t]*\\(\\([^ \t\n]*\\)[ \t]*\\(.*\\)\\)\\)"
	 limit t)
	(let ((beg (match-beginning 0))
	      (block-start (match-end 0))
	      (block-end nil)
	      (lang (match-string 7))
	      (beg1 (line-beginning-position 2))
	      (dc1 (downcase (match-string 2)))
	      (dc3 (downcase (match-string 3)))
	      end end1 quoting block-type ovl)
	  (cond
	   ((member dc1 '("html:" "ascii:" "latex:" "docbook:"))
	    ;; a single line of backend-specific content
	    (org-remove-flyspell-overlays-in (match-beginning 0) (match-end 0))
	    (remove-text-properties (match-beginning 0) (match-end 0)
				    '(display t invisible t intangible t))
	    (add-text-properties (match-beginning 1) (match-end 3)
				 '(font-lock-fontified t face org-meta-line))
	    (add-text-properties (match-beginning 6) (+ (match-end 6) 1)
				 '(font-lock-fontified t face org-block))
					; for backend-specific code
	    t)
	   ((and (match-end 4) (equal dc3 "begin"))
	    ;; Truly a block
	    (setq block-type (downcase (match-string 5))
		  quoting (member block-type org-protecting-blocks))
	    (when (re-search-forward
		   (concat "^[ \t]*#\\+end" (match-string 4) "\\>.*")
		   nil t)  ;; on purpose, we look further than LIMIT
	      (setq end (min (point-max) (match-end 0))
		    end1 (min (point-max) (1- (match-beginning 0))))
	      (setq block-end (match-beginning 0))
	      (when quoting
		(remove-text-properties beg end
					'(display t invisible t intangible t)))
	      (add-text-properties
	       beg end
	       '(font-lock-fontified t font-lock-multiline t))
	      (add-text-properties beg beg1 '(face org-meta-line))
	      (add-text-properties end1 (min (point-max) (1+ end))
				   '(face org-meta-line)) ; for end_src
	      (cond
	       ((and lang (not (string= lang "")) org-src-fontify-natively)
		(org-src-font-lock-fontify-block lang block-start block-end)
		;; remove old background overlays
		(mapc (lambda (ov)
			(if (eq (overlay-get ov 'face) 'org-block-background)
			    (delete-overlay ov)))
		      (overlays-at (/ (+ beg1 block-end) 2)))
		;; add a background overlay
		(setq ovl (make-overlay beg1 block-end))
                (overlay-put ovl 'face 'org-block-background)
                (overlay-put ovl 'evaporate t))  ;; make it go away when empty
	       (quoting
		(add-text-properties beg1 (min (point-max) (1+ end1))
				     '(face org-block))) ; end of source block
	       ((not org-fontify-quote-and-verse-blocks))
	       ((string= block-type "quote")
		(add-text-properties beg1 (min (point-max) (1+ end1)) '(face org-quote)))
	       ((string= block-type "verse")
		(add-text-properties beg1 (min (point-max) (1+ end1)) '(face org-verse))))
      	      (add-text-properties beg beg1 '(face org-block-begin-line))
      	      (add-text-properties (min (point-max) (1+ end)) (min (point-max) (1+ end1))
				   '(face org-block-end-line))
	      t))
	   ((member dc1 '("title:" "author:" "email:" "date:"))
	    (add-text-properties
	     beg (match-end 3)
	     (if (member (intern (substring dc1 0 -1)) org-hidden-keywords)
		 '(font-lock-fontified t invisible t)
	       '(font-lock-fontified t face org-document-info-keyword)))
	    (add-text-properties
	     (match-beginning 6) (match-end 6)
	     (if (string-equal dc1 "title:")
		 '(font-lock-fontified t face org-document-title)
	       '(font-lock-fontified t face org-document-info))))
	   ((not (member (char-after beg) '(?\  ?\t)))
	    ;; just any other in-buffer setting, but not indented
	    (add-text-properties
	     beg (match-end 0)
	     '(font-lock-fontified t face org-meta-line))
	    t)
	   ((or (member dc1 '("begin:" "end:" "caption:" "label:"
			      "orgtbl:" "tblfm:" "tblname:" "results:"
			      "call:" "header:" "headers:" "name:"))
		(and (match-end 4) (equal dc3 "attr")))
	    (add-text-properties
	     beg (match-end 0)
	     '(font-lock-fontified t face org-meta-line))
	    t)
	   ((member dc3 '(" " ""))
	    (add-text-properties
	     beg (match-end 0)
	     '(font-lock-fontified t face font-lock-comment-face)))
	   (t nil))))))

(defun org-strip-protective-commas (beg end)
  "Strip protective commas between BEG and END in the current buffer."
  (interactive "r")
  (save-excursion
    (save-match-data
      (goto-char beg)
      (let ((front-line (save-excursion
			  (re-search-forward
			   "[^[:space:]]" end t)
			  (goto-char (match-beginning 0))
			  (current-column))))
	(while (re-search-forward "^[ \t]*\\(,\\)\\([*]\\|#\\+\\)" end t)
	  (goto-char (match-beginning 1))
	  (when (= (current-column) front-line)
	    (replace-match "" nil nil nil 1)))))))

(defun org-activate-angle-links (limit)
  "Run through the buffer and add overlays to links."
  (if (re-search-forward org-angle-link-re limit t)
      (progn
	(org-remove-flyspell-overlays-in (match-beginning 0) (match-end 0))
	(add-text-properties (match-beginning 0) (match-end 0)
			     (list 'mouse-face 'highlight
				   'keymap org-mouse-map))
	(org-rear-nonsticky-at (match-end 0))
	t)))

(defun org-activate-footnote-links (limit)
  "Run through the buffer and add overlays to footnotes."
  (let ((fn (org-footnote-next-reference-or-definition limit)))
    (when fn
      (let ((beg (nth 1 fn)) (end (nth 2 fn)))
	(org-remove-flyspell-overlays-in beg end)
	(add-text-properties beg end
			     (list 'mouse-face 'highlight
				   'keymap org-mouse-map
				   'help-echo
				   (if (= (point-at-bol) beg)
				       "Footnote definition"
				     "Footnote reference")
				   'font-lock-fontified t
				   'font-lock-multiline t
				   'face 'org-footnote))))))

(defun org-activate-bracket-links (limit)
  "Run through the buffer and add overlays to bracketed links."
  (if (re-search-forward org-bracket-link-regexp limit t)
      (let* ((help (concat "LINK: "
			   (org-match-string-no-properties 1)))
	     ;; FIXME: above we should remove the escapes.
	     ;; but that requires another match, protecting match data,
	     ;; a lot of overhead for font-lock.
	     (ip (org-maybe-intangible
		  (list 'invisible 'org-link
			'keymap org-mouse-map 'mouse-face 'highlight
			'font-lock-multiline t 'help-echo help)))
	     (vp (list 'keymap org-mouse-map 'mouse-face 'highlight
		       'font-lock-multiline t 'help-echo help)))
	;; We need to remove the invisible property here.  Table narrowing
	;; may have made some of this invisible.
	(org-remove-flyspell-overlays-in (match-beginning 0) (match-end 0))
	(remove-text-properties (match-beginning 0) (match-end 0)
				'(invisible nil))
	(if (match-end 3)
	    (progn
	      (add-text-properties (match-beginning 0) (match-beginning 3) ip)
	      (org-rear-nonsticky-at (match-beginning 3))
	      (add-text-properties (match-beginning 3) (match-end 3) vp)
	      (org-rear-nonsticky-at (match-end 3))
	      (add-text-properties (match-end 3) (match-end 0) ip)
	      (org-rear-nonsticky-at (match-end 0)))
	  (add-text-properties (match-beginning 0) (match-beginning 1) ip)
	  (org-rear-nonsticky-at (match-beginning 1))
	  (add-text-properties (match-beginning 1) (match-end 1) vp)
	  (org-rear-nonsticky-at (match-end 1))
	  (add-text-properties (match-end 1) (match-end 0) ip)
	  (org-rear-nonsticky-at (match-end 0)))
	t)))

(defun org-activate-dates (limit)
  "Run through the buffer and add overlays to dates."
  (if (re-search-forward org-tsr-regexp-both limit t)
      (progn
	(org-remove-flyspell-overlays-in (match-beginning 0) (match-end 0))
	(add-text-properties (match-beginning 0) (match-end 0)
			     (list 'mouse-face 'highlight
				   'keymap org-mouse-map))
	(org-rear-nonsticky-at (match-end 0))
	(when org-display-custom-times
	  (if (match-end 3)
	      (org-display-custom-time (match-beginning 3) (match-end 3)))
	  (org-display-custom-time (match-beginning 1) (match-end 1)))
	t)))

(defvar org-target-link-regexp nil
  "Regular expression matching radio targets in plain text.")
(make-variable-buffer-local 'org-target-link-regexp)
(defvar org-target-regexp "<<\\([^<>\n\r]+\\)>>"
  "Regular expression matching a link target.")
(defvar org-radio-target-regexp "<<<\\([^<>\n\r]+\\)>>>"
  "Regular expression matching a radio target.")
(defvar org-any-target-regexp "<<<?\\([^<>\n\r]+\\)>>>?" ; FIXME, not exact, would match <<<aaa>>  as a radio target.
  "Regular expression matching any target.")

(defun org-activate-target-links (limit)
  "Run through the buffer and add overlays to target matches."
  (when org-target-link-regexp
    (let ((case-fold-search t))
      (if (re-search-forward org-target-link-regexp limit t)
	  (progn
	    (org-remove-flyspell-overlays-in (match-beginning 0) (match-end 0))
	    (add-text-properties (match-beginning 0) (match-end 0)
				 (list 'mouse-face 'highlight
				       'keymap org-mouse-map
				       'help-echo "Radio target link"
				       'org-linked-text t))
	    (org-rear-nonsticky-at (match-end 0))
	    t)))))

(defun org-update-radio-target-regexp ()
  "Find all radio targets in this file and update the regular expression."
  (interactive)
  (when (memq 'radio org-activate-links)
    (setq org-target-link-regexp
	  (org-make-target-link-regexp (org-all-targets 'radio)))
    (org-restart-font-lock)))

(defun org-hide-wide-columns (limit)
  (let (s e)
    (setq s (text-property-any (point) (or limit (point-max))
			       'org-cwidth t))
    (when s
      (setq e (next-single-property-change s 'org-cwidth))
      (add-text-properties s e (org-maybe-intangible '(invisible org-cwidth)))
      (goto-char e)
      t)))

(defvar org-latex-and-specials-regexp nil
  "Regular expression for highlighting export special stuff.")
(defvar org-match-substring-regexp)
(defvar org-match-substring-with-braces-regexp)

;; This should be with the exporter code, but we also use if for font-locking
(defconst org-export-html-special-string-regexps
  '(("\\\\-" . "&shy;")
    ("---\\([^-]\\)" . "&mdash;\\1")
    ("--\\([^-]\\)" . "&ndash;\\1")
    ("\\.\\.\\." . "&hellip;"))
  "Regular expressions for special string conversion.")


(defun org-compute-latex-and-specials-regexp ()
  "Compute regular expression for stuff treated specially by exporters."
  (if (not org-highlight-latex-fragments-and-specials)
      (org-set-local 'org-latex-and-specials-regexp nil)
    (require 'org-exp)
    (let*
	((matchers (plist-get org-format-latex-options :matchers))
	 (latexs (delq nil (mapcar (lambda (x) (if (member (car x) matchers) x))
				   org-latex-regexps)))
	 (org-export-allow-BIND nil)
	 (options (org-combine-plists (org-default-export-plist)
				      (org-infile-export-plist)))
	 (org-export-with-sub-superscripts (plist-get options :sub-superscript))
	 (org-export-with-LaTeX-fragments (plist-get options :LaTeX-fragments))
	 (org-export-with-TeX-macros (plist-get options :TeX-macros))
	 (org-export-html-expand (plist-get options :expand-quoted-html))
	 (org-export-with-special-strings (plist-get options :special-strings))
	 (re-sub
	  (cond
	   ((equal org-export-with-sub-superscripts '{})
	    (list org-match-substring-with-braces-regexp))
	   (org-export-with-sub-superscripts
	    (list org-match-substring-regexp))
	   (t nil)))
	 (re-latex
	  (if org-export-with-LaTeX-fragments
	      (mapcar (lambda (x) (nth 1 x)) latexs)))
	 (re-macros
	  (if org-export-with-TeX-macros
	      (list (concat "\\\\"
			    (regexp-opt
			     (append

			      (delq nil
				    (mapcar 'car-safe
					    (append org-entities-user
						    org-entities)))
			      (if (boundp 'org-latex-entities)
				  (mapcar (lambda (x)
					    (or (car-safe x) x))
					  org-latex-entities)
				nil))
			     'words))) ; FIXME
	    ))
    ;;			(list "\\\\\\(?:[a-zA-Z]+\\)")))
	 (re-special (if org-export-with-special-strings
			 (mapcar (lambda (x) (car x))
				 org-export-html-special-string-regexps)))
	 (re-rest
	  (delq nil
		(list
		 (if org-export-html-expand "@<[^>\n]+>")
		 ))))
      (org-set-local
       'org-latex-and-specials-regexp
       (mapconcat 'identity (append re-latex re-sub re-macros re-special
				    re-rest) "\\|")))))

(defun org-do-latex-and-special-faces (limit)
  "Run through the buffer and add overlays to links."
  (when org-latex-and-specials-regexp
    (let (rtn d)
      (while (and (not rtn) (re-search-forward org-latex-and-specials-regexp
					       limit t))
	(if (not (memq (car-safe (get-text-property (1+ (match-beginning 0))
						    'face))
		       '(org-code org-verbatim underline)))
	    (progn
	      (setq rtn t
		    d (cond ((member (char-after (1+ (match-beginning 0)))
				     '(?_ ?^)) 1)
			    (t 0)))
	      (font-lock-prepend-text-property
	       (+ d (match-beginning 0)) (match-end 0)
	       'face 'org-latex-and-export-specials)
	      (add-text-properties (+ d (match-beginning 0)) (match-end 0)
				   '(font-lock-multiline t)))))
      rtn)))

(defun org-restart-font-lock ()
  "Restart `font-lock-mode', to force refontification."
  (when (and (boundp 'font-lock-mode) font-lock-mode)
    (font-lock-mode -1)
    (font-lock-mode 1)))

(defun org-all-targets (&optional radio)
  "Return a list of all targets in this file.
With optional argument RADIO, only find radio targets."
  (let ((re (if radio org-radio-target-regexp org-target-regexp))
	rtn)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward re nil t)
	(add-to-list 'rtn (downcase (org-match-string-no-properties 1))))
      rtn)))

(defun org-make-target-link-regexp (targets)
  "Make regular expression matching all strings in TARGETS.
The regular expression finds the targets also if there is a line break
between words."
  (and targets
       (concat
	"\\<\\("
	(mapconcat
	 (lambda (x)
	   (setq x (regexp-quote x))
	   (while (string-match " +" x)
	     (setq x (replace-match "\\s-+" t t x)))
	   x)
	 targets
	 "\\|")
	"\\)\\>")))

(defun org-activate-tags (limit)
  (if (re-search-forward (org-re "^\\*+.*[ \t]\\(:[[:alnum:]_@#%:]+:\\)[ \r\n]") limit t)
      (progn
	(org-remove-flyspell-overlays-in (match-beginning 1) (match-end 1))
	(add-text-properties (match-beginning 1) (match-end 1)
			     (list 'mouse-face 'highlight
				   'keymap org-mouse-map))
	(org-rear-nonsticky-at (match-end 1))
	t)))

(defun org-outline-level ()
  "Compute the outline level of the heading at point.
This function assumes that the cursor is at the beginning of a line matched
by `outline-regexp'.  Otherwise it returns garbage.
If this is called at a normal headline, the level is the number of stars.
Use `org-reduced-level' to remove the effect of `org-odd-levels'."
  (save-excursion
    (looking-at org-outline-regexp)
    (1- (- (match-end 0) (match-beginning 0)))))

(defvar org-font-lock-keywords nil)

(defconst org-property-re (org-re "^[ \t]*\\(:\\([-[:alnum:]_]+\\+?\\):\\)[ \t]*\\([^ \t\r\n].*\\)")
  "Regular expression matching a property line.")

(defvar org-font-lock-hook nil
  "Functions to be called for special font lock stuff.")

(defvar org-font-lock-set-keywords-hook nil
  "Functions that can manipulate `org-font-lock-extra-keywords'.
This is called after `org-font-lock-extra-keywords' is defined, but before
it is installed to be used by font lock.  This can be useful if something
needs to be inserted at a specific position in the font-lock sequence.")

(defun org-font-lock-hook (limit)
  (run-hook-with-args 'org-font-lock-hook limit))

(defun org-set-font-lock-defaults ()
  (let* ((em org-fontify-emphasized-text)
	 (lk org-activate-links)
	 (org-font-lock-extra-keywords
	  (list
	   ;; Call the hook
	   '(org-font-lock-hook)
	   ;; Headlines
	   `(,(if org-fontify-whole-heading-line
		  "^\\(\\**\\)\\(\\* \\)\\(.*\n?\\)"
		"^\\(\\**\\)\\(\\* \\)\\(.*\\)")
	     (1 (org-get-level-face 1))
	     (2 (org-get-level-face 2))
	     (3 (org-get-level-face 3)))
	   ;; Table lines
	   '("^[ \t]*\\(\\(|\\|\\+-[-+]\\).*\\S-\\)"
	     (1 'org-table t))
	   ;; Table internals
	   '("^[ \t]*|\\(?:.*?|\\)? *\\(:?=[^|\n]*\\)" (1 'org-formula t))
	   '("^[ \t]*| *\\([#*]\\) *|" (1 'org-formula t))
	   '("^[ \t]*|\\( *\\([$!_^/]\\) *|.*\\)|" (1 'org-formula t))
	   '("| *\\(<[lrc]?[0-9]*>\\)" (1 'org-formula t))
	   ;; Drawers
	   (list org-drawer-regexp '(0 'org-special-keyword t))
	   (list "^[ \t]*:END:" '(0 'org-special-keyword t))
	   ;; Properties
	   (list org-property-re
		 '(1 'org-special-keyword t)
		 '(3 'org-property-value t))
	   ;; Links
	   (if (memq 'tag lk) '(org-activate-tags (1 'org-tag prepend)))
	   (if (memq 'angle lk) '(org-activate-angle-links (0 'org-link t)))
	   (if (memq 'plain lk) '(org-activate-plain-links))
	   (if (memq 'bracket lk) '(org-activate-bracket-links (0 'org-link t)))
	   (if (memq 'radio lk) '(org-activate-target-links (0 'org-link t)))
	   (if (memq 'date lk) '(org-activate-dates (0 'org-date t)))
	   (if (memq 'footnote lk) '(org-activate-footnote-links))
	   '("^&?%%(.*\\|<%%([^>\n]*?>" (0 'org-sexp-date t))
	   '(org-hide-wide-columns (0 nil append))
	   ;; TODO keyword
	   (list (format org-heading-keyword-regexp-format
			 org-todo-regexp)
		 '(2 (org-get-todo-face 2) t))
	   ;; DONE
	   (if org-fontify-done-headline
	       (list (format org-heading-keyword-regexp-format
			     (concat
			      "\\(?:"
			      (mapconcat 'regexp-quote org-done-keywords "\\|")
			      "\\)"))
		     '(2 'org-headline-done t))
	     nil)
	   ;; Priorities
	   '(org-font-lock-add-priority-faces)
	   ;; Tags
	   '(org-font-lock-add-tag-faces)
	   ;; Special keywords
	   (list (concat "\\<" org-deadline-string) '(0 'org-special-keyword t))
	   (list (concat "\\<" org-scheduled-string) '(0 'org-special-keyword t))
	   (list (concat "\\<" org-closed-string) '(0 'org-special-keyword t))
	   (list (concat "\\<" org-clock-string) '(0 'org-special-keyword t))
	   ;; Emphasis
	   (if em
               (if (featurep 'xemacs)
                   '(org-do-emphasis-faces (0 nil append))
                 '(org-do-emphasis-faces)))
	   ;; Checkboxes
	   '("^[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\)[ \t]+\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\(\\[[- X]\\]\\)"
	     1 'org-checkbox prepend)
	   (if (cdr (assq 'checkbox org-list-automatic-rules))
	       '("\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
		 (0 (org-get-checkbox-statistics-face) t)))
	   ;; Description list items
	   '("^[ \t]*[-+*][ \t]+\\(.*?[ \t]+::\\)\\([ \t]+\\|$\\)"
	     1 'bold prepend)
	   ;; ARCHIVEd headings
	   (list (concat
		  org-outline-regexp-bol
		  "\\(.*:" org-archive-tag ":.*\\)")
		 '(1 'org-archived prepend))
	   ;; Specials
	   '(org-do-latex-and-special-faces)
	   '(org-fontify-entities)
	   '(org-raise-scripts)
	   ;; Code
	   '(org-activate-code (1 'org-code t))
	   ;; COMMENT
	   (list (format org-heading-keyword-regexp-format
			 (concat "\\("
				 org-comment-string "\\|" org-quote-string
				 "\\)"))
		 '(2 'org-special-keyword t))
	   '("^#.*" (0 'font-lock-comment-face t))
	   ;; Blocks and meta lines
	   '(org-fontify-meta-lines-and-blocks)
	   )))
    (setq org-font-lock-extra-keywords (delq nil org-font-lock-extra-keywords))
    (run-hooks 'org-font-lock-set-keywords-hook)
    ;; Now set the full font-lock-keywords
    (org-set-local 'org-font-lock-keywords org-font-lock-extra-keywords)
    (org-set-local 'font-lock-defaults
		   '(org-font-lock-keywords t nil nil backward-paragraph))
    (kill-local-variable 'font-lock-keywords) nil))

(defun org-toggle-pretty-entities ()
  "Toggle the composition display of entities as UTF8 characters."
  (interactive)
  (org-set-local 'org-pretty-entities (not org-pretty-entities))
  (org-restart-font-lock)
  (if org-pretty-entities
      (message "Entities are displayed as UTF8 characters")
    (save-restriction
      (widen)
      (org-decompose-region (point-min) (point-max))
      (message "Entities are displayed plain"))))

(defun org-fontify-entities (limit)
  "Find an entity to fontify."
  (let (ee)
    (when org-pretty-entities
      (catch 'match
	(while (re-search-forward
		"\\\\\\(there4\\|sup[123]\\|frac[13][24]\\|[a-zA-Z]+\\)\\($\\|{}\\|[^[:alpha:]\n]\\)"
		limit t)
	  (if (and (not (org-in-indented-comment-line))
		   (setq ee (org-entity-get (match-string 1)))
		   (= (length (nth 6 ee)) 1))
	      (let*
		  ((end (if (equal (match-string 2) "{}")
			    (match-end 2)
			  (match-end 1))))
		(add-text-properties
		 (match-beginning 0) end
		 (list 'font-lock-fontified t))
		(compose-region (match-beginning 0) end
				(nth 6 ee) nil)
		(backward-char 1)
		(throw 'match t))))
	nil))))

(defun org-fontify-like-in-org-mode (s &optional odd-levels)
  "Fontify string S like in Org-mode."
  (with-temp-buffer
    (insert s)
    (let ((org-odd-levels-only odd-levels))
      (org-mode)
      (font-lock-fontify-buffer)
      (buffer-string))))

(defvar org-m nil)
(defvar org-l nil)
(defvar org-f nil)
(defun org-get-level-face (n)
 "Get the right face for match N in font-lock matching of headlines."
 (setq org-l (- (match-end 2) (match-beginning 1) 1))
 (if org-odd-levels-only (setq org-l (1+ (/ org-l 2))))
 (if org-cycle-level-faces
     (setq org-f (nth (% (1- org-l) org-n-level-faces) org-level-faces))
   (setq org-f (nth (1- (min org-l org-n-level-faces)) org-level-faces)))
 (cond
  ((eq n 1) (if org-hide-leading-stars 'org-hide org-f))
  ((eq n 2) org-f)
  (t (if org-level-color-stars-only nil org-f))))


(defun org-get-todo-face (kwd)
  "Get the right face for a TODO keyword KWD.
If KWD is a number, get the corresponding match group."
  (if (numberp kwd) (setq kwd (match-string kwd)))
  (or (org-face-from-face-or-color
       'todo 'org-todo (cdr (assoc kwd org-todo-keyword-faces)))
      (and (member kwd org-done-keywords) 'org-done)
      'org-todo))

(defun org-face-from-face-or-color (context inherit face-or-color)
  "Create a face list that inherits INHERIT, but sets the foreground color.
When FACE-OR-COLOR is not a string, just return it."
  (if (stringp face-or-color)
      (list :inherit inherit
	    (cdr (assoc context org-faces-easy-properties))
	    face-or-color)
    face-or-color))

(defun org-font-lock-add-tag-faces (limit)
  "Add the special tag faces."
  (when (and org-tag-faces org-tags-special-faces-re)
    (while (re-search-forward org-tags-special-faces-re limit t)
      (add-text-properties (match-beginning 1) (match-end 1)
			   (list 'face (org-get-tag-face 1)
				 'font-lock-fontified t))
      (backward-char 1))))

(defun org-font-lock-add-priority-faces (limit)
  "Add the special priority faces."
  (while (re-search-forward "\\[#\\([A-Z0-9]\\)\\]" limit t)
    (when (save-match-data (org-at-heading-p))
      (add-text-properties
       (match-beginning 0) (match-end 0)
       (list 'face (or (org-face-from-face-or-color
			'priority 'org-special-keyword
			(cdr (assoc (char-after (match-beginning 1))
				    org-priority-faces)))
		       'org-special-keyword)
	     'font-lock-fontified t)))))

(defun org-get-tag-face (kwd)
  "Get the right face for a TODO keyword KWD.
If KWD is a number, get the corresponding match group."
  (if (numberp kwd) (setq kwd (match-string kwd)))
  (or (org-face-from-face-or-color
       'tag 'org-tag (cdr (assoc kwd org-tag-faces)))
      'org-tag))

(defun org-unfontify-region (beg end &optional maybe_loudly)
  "Remove fontification and activation overlays from links."
  (font-lock-default-unfontify-region beg end)
  (let* ((buffer-undo-list t)
	 (inhibit-read-only t) (inhibit-point-motion-hooks t)
	 (inhibit-modification-hooks t)
	 deactivate-mark buffer-file-name buffer-file-truename)
    (org-decompose-region beg end)
    (remove-text-properties beg end
     '(mouse-face t keymap t org-linked-text t
		  invisible t intangible t
		  org-no-flyspell t org-emphasis t))
    (org-remove-font-lock-display-properties beg end)))

(defconst org-script-display  '(((raise -0.3) (height 0.7))
				((raise 0.3)  (height 0.7))
				((raise -0.5))
				((raise 0.5)))
  "Display properties for showing superscripts and subscripts.")

(defun org-remove-font-lock-display-properties (beg end)
  "Remove specific display properties that have been added by font lock.
The will remove the raise properties that are used to show superscripts
and subscripts."
  (let (next prop)
    (while (< beg end)
      (setq next (next-single-property-change beg 'display nil end)
	    prop (get-text-property beg 'display))
      (if (member prop org-script-display)
	  (put-text-property beg next 'display nil))
      (setq beg next))))

(defun org-raise-scripts (limit)
  "Add raise properties to sub/superscripts."
  (when (and org-pretty-entities org-pretty-entities-include-sub-superscripts)
    (if (re-search-forward
	 (if (eq org-use-sub-superscripts t)
	     org-match-substring-regexp
	   org-match-substring-with-braces-regexp)
	 limit t)
	(let* ((pos (point)) table-p comment-p
	       (mpos (match-beginning 3))
	       (emph-p (get-text-property mpos 'org-emphasis))
	       (link-p (get-text-property mpos 'mouse-face))
	       (keyw-p (eq 'org-special-keyword (get-text-property mpos 'face))))
	  (goto-char (point-at-bol))
	  (setq table-p (org-looking-at-p org-table-dataline-regexp)
		comment-p (org-looking-at-p "[ \t]*#"))
	  (goto-char pos)
	  ;; FIXME: Should we go back one character here, for a_b^c
	  ;; (goto-char (1- pos)) ;????????????????????
	  (if (or comment-p emph-p link-p keyw-p)
	      t
	    (put-text-property (match-beginning 3) (match-end 0)
			       'display
			       (if (equal (char-after (match-beginning 2)) ?^)
				   (nth (if table-p 3 1) org-script-display)
				 (nth (if table-p 2 0) org-script-display)))
	    (add-text-properties (match-beginning 2) (match-end 2)
				 (list 'invisible t
				       'org-dwidth t 'org-dwidth-n 1))
	    (if (and (eq (char-after (match-beginning 3)) ?{)
		     (eq (char-before (match-end 3)) ?}))
		(progn
		  (add-text-properties
		   (match-beginning 3) (1+ (match-beginning 3))
		   (list 'invisible t 'org-dwidth t 'org-dwidth-n 1))
		  (add-text-properties
		   (1- (match-end 3)) (match-end 3)
		   (list 'invisible t 'org-dwidth t 'org-dwidth-n 1))))
	    t)))))

;;;; Visibility cycling, including org-goto and indirect buffer

;;; Cycling

(defvar org-cycle-global-status nil)
(make-variable-buffer-local 'org-cycle-global-status)
(defvar org-cycle-subtree-status nil)
(make-variable-buffer-local 'org-cycle-subtree-status)

;;;###autoload

(defvar org-inlinetask-min-level)

(defun org-cycle (&optional arg)
  "TAB-action and visibility cycling for Org-mode.

This is the command invoked in Org-mode by the TAB key.  Its main purpose
is outline visibility cycling, but it also invokes other actions
in special contexts.

- When this function is called with a prefix argument, rotate the entire
  buffer through 3 states (global cycling)
  1. OVERVIEW: Show only top-level headlines.
  2. CONTENTS: Show all headlines of all levels, but no body text.
  3. SHOW ALL: Show everything.
  When called with two `C-u C-u' prefixes, switch to the startup visibility,
  determined by the variable `org-startup-folded', and by any VISIBILITY
  properties in the buffer.
  When called with three `C-u C-u C-u' prefixed, show the entire buffer,
  including any drawers.

- When inside a table, re-align the table and move to the next field.

- When point is at the beginning of a headline, rotate the subtree started
  by this line through 3 different states (local cycling)
  1. FOLDED:   Only the main headline is shown.
  2. CHILDREN: The main headline and the direct children are shown.
               From this state, you can move to one of the children
               and zoom in further.
  3. SUBTREE:  Show the entire subtree, including body text.
  If there is no subtree, switch directly from CHILDREN to FOLDED.

- When point is at the beginning of an empty headline and the variable
  `org-cycle-level-after-item/entry-creation' is set, cycle the level
  of the headline by demoting and promoting it to likely levels.  This
  speeds up creation document structure by pressing TAB once or several
  times right after creating a new headline.

- When there is a numeric prefix, go up to a heading with level ARG, do
  a `show-subtree' and return to the previous cursor position.  If ARG
  is negative, go up that many levels.

- When point is not at the beginning of a headline, execute the global
  binding for TAB, which is re-indenting the line.  See the option
  `org-cycle-emulate-tab' for details.

- Special case: if point is at the beginning of the buffer and there is
  no headline in line 1, this function will act as if called with prefix arg
  (C-u TAB, same as S-TAB) also when called without prefix arg.
  But only if also the variable `org-cycle-global-at-bob' is t."
  (interactive "P")
  (org-load-modules-maybe)
  (unless (or (run-hook-with-args-until-success 'org-tab-first-hook)
	      (and org-cycle-level-after-item/entry-creation
		   (or (org-cycle-level)
		       (org-cycle-item-indentation))))
    (let* ((limit-level
	    (or org-cycle-max-level
		(and (boundp 'org-inlinetask-min-level)
		     org-inlinetask-min-level
		     (1- org-inlinetask-min-level))))
	   (nstars (and limit-level
		      (if org-odd-levels-only
			  (and limit-level (1- (* limit-level 2)))
			limit-level)))
	   (org-outline-regexp
	    (if (not (eq major-mode 'org-mode))
		outline-regexp
	      (concat "\\*" (if nstars (format "\\{1,%d\\} " nstars) "+ "))))
	   (bob-special (and org-cycle-global-at-bob (not arg) (bobp)
			     (not (looking-at org-outline-regexp))))
	   (org-cycle-hook
	    (if bob-special
		(delq 'org-optimize-window-after-visibility-change
		      (copy-sequence org-cycle-hook))
	      org-cycle-hook))
	   (pos (point)))

      (if (or bob-special (equal arg '(4)))
	  ;; special case:  use global cycling
	  (setq arg t))

      (cond

       ((equal arg '(16))
	(setq last-command 'dummy)
	(org-set-startup-visibility)
	(message "Startup visibility, plus VISIBILITY properties"))

       ((equal arg '(64))
	(show-all)
	(message "Entire buffer visible, including drawers"))

       ;; Table: enter it or move to the next field.
       ((org-at-table-p 'any)
	(if (org-at-table.el-p)
	    (message "Use C-c ' to edit table.el tables")
	  (if arg (org-table-edit-field t)
	    (org-table-justify-field-maybe)
	    (call-interactively 'org-table-next-field))))

       ((run-hook-with-args-until-success
	 'org-tab-after-check-for-table-hook))

       ;; Global cycling: delegate to `org-cycle-internal-global'.
       ((eq arg t) (org-cycle-internal-global))

       ;; Drawers: delegate to `org-flag-drawer'.
       ((and org-drawers org-drawer-regexp
	     (save-excursion
	       (beginning-of-line 1)
	       (looking-at org-drawer-regexp)))
	(org-flag-drawer ; toggle block visibility
	 (not (get-char-property (match-end 0) 'invisible))))

       ;; Show-subtree, ARG levels up from here.
       ((integerp arg)
	(save-excursion
	  (org-back-to-heading)
	  (outline-up-heading (if (< arg 0) (- arg)
				(- (funcall outline-level) arg)))
	  (org-show-subtree)))

       ;; Inline task: delegate to `org-inlinetask-toggle-visibility'.
       ((and (featurep 'org-inlinetask)
	     (org-inlinetask-at-task-p)
	     (or (bolp) (not (eq org-cycle-emulate-tab 'exc-hl-bol))))
	(org-inlinetask-toggle-visibility))

       ((org-try-cdlatex-tab))

       ;; At an item/headline: delegate to `org-cycle-internal-local'.
       ((and (or (and org-cycle-include-plain-lists (org-at-item-p))
		 (save-excursion (beginning-of-line 1)
				 (looking-at org-outline-regexp)))
	     (or (bolp) (not (eq org-cycle-emulate-tab 'exc-hl-bol))))
	(org-cycle-internal-local))

       ;; From there: TAB emulation and template completion.
       (buffer-read-only (org-back-to-heading))

       ((run-hook-with-args-until-success
	 'org-tab-after-check-for-cycling-hook))

       ((org-try-structure-completion))

       ((run-hook-with-args-until-success
	 'org-tab-before-tab-emulation-hook))

       ((and (eq org-cycle-emulate-tab 'exc-hl-bol)
	     (or (not (bolp))
		 (not (looking-at org-outline-regexp))))
	(call-interactively (global-key-binding "\t")))

       ((if (and (memq org-cycle-emulate-tab '(white whitestart))
		 (save-excursion (beginning-of-line 1) (looking-at "[ \t]*"))
		 (or (and (eq org-cycle-emulate-tab 'white)
			  (= (match-end 0) (point-at-eol)))
		     (and (eq org-cycle-emulate-tab 'whitestart)
			  (>= (match-end 0) pos))))
	    t
	  (eq org-cycle-emulate-tab t))
	(call-interactively (global-key-binding "\t")))

       (t (save-excursion
	    (org-back-to-heading)
	    (org-cycle)))))))

(defun org-cycle-internal-global ()
  "Do the global cycling action."
  ;; Hack to avoid display of messages for .org  attachments in Gnus
  (let ((ga (string-match "\\*fontification" (buffer-name))))
    (cond
     ((and (eq last-command this-command)
	   (eq org-cycle-global-status 'overview))
      ;; We just created the overview - now do table of contents
      ;; This can be slow in very large buffers, so indicate action
      (run-hook-with-args 'org-pre-cycle-hook 'contents)
      (unless ga (message "CONTENTS..."))
      (org-content)
      (unless ga (message "CONTENTS...done"))
      (setq org-cycle-global-status 'contents)
      (run-hook-with-args 'org-cycle-hook 'contents))

     ((and (eq last-command this-command)
	   (eq org-cycle-global-status 'contents))
      ;; We just showed the table of contents - now show everything
      (run-hook-with-args 'org-pre-cycle-hook 'all)
      (show-all)
      (unless ga (message "SHOW ALL"))
      (setq org-cycle-global-status 'all)
      (run-hook-with-args 'org-cycle-hook 'all))

     (t
      ;; Default action: go to overview
      (run-hook-with-args 'org-pre-cycle-hook 'overview)
      (org-overview)
      (unless ga (message "OVERVIEW"))
      (setq org-cycle-global-status 'overview)
      (run-hook-with-args 'org-cycle-hook 'overview)))))

(defun org-cycle-internal-local ()
  "Do the local cycling action."
  (let ((goal-column 0) eoh eol eos has-children children-skipped struct)
    ;; First, determine end of headline (EOH), end of subtree or item
    ;; (EOS), and if item or heading has children (HAS-CHILDREN).
    (save-excursion
      (if (org-at-item-p)
	  (progn
	    (beginning-of-line)
	    (setq struct (org-list-struct))
	    (setq eoh (point-at-eol))
	    (setq eos (org-list-get-item-end-before-blank (point) struct))
	    (setq has-children (org-list-has-child-p (point) struct)))
	(org-back-to-heading)
	(setq eoh (save-excursion (outline-end-of-heading) (point)))
	(setq eos (save-excursion
		    (org-end-of-subtree t)
		    (unless (eobp)
		      (skip-chars-forward " \t\n"))
		    (if (eobp) (point) (1- (point)))))
	(setq has-children
	      (or (save-excursion
		    (let ((level (funcall outline-level)))
		      (outline-next-heading)
		      (and (org-at-heading-p t)
			   (> (funcall outline-level) level))))
		  (save-excursion
		    (org-list-search-forward (org-item-beginning-re) eos t)))))
      ;; Determine end invisible part of buffer (EOL)
      (beginning-of-line 2)
      ;; XEmacs doesn't have `next-single-char-property-change'
      (if (featurep 'xemacs)
	  (while (and (not (eobp)) ;; this is like `next-line'
		      (get-char-property (1- (point)) 'invisible))
	    (beginning-of-line 2))
	(while (and (not (eobp)) ;; this is like `next-line'
		    (get-char-property (1- (point)) 'invisible))
	  (goto-char (next-single-char-property-change (point) 'invisible))
	  (and (eolp) (beginning-of-line 2))))
      (setq eol (point)))
    ;; Find out what to do next and set `this-command'
    (cond
     ((= eos eoh)
      ;; Nothing is hidden behind this heading
      (run-hook-with-args 'org-pre-cycle-hook 'empty)
      (message "EMPTY ENTRY")
      (setq org-cycle-subtree-status nil)
      (save-excursion
	(goto-char eos)
	(outline-next-heading)
	(if (outline-invisible-p) (org-flag-heading nil))))
     ((and (or (>= eol eos)
	       (not (string-match "\\S-" (buffer-substring eol eos))))
	   (or has-children
	       (not (setq children-skipped
			  org-cycle-skip-children-state-if-no-children))))
      ;; Entire subtree is hidden in one line: children view
      (run-hook-with-args 'org-pre-cycle-hook 'children)
      (if (org-at-item-p)
	  (org-list-set-item-visibility (point-at-bol) struct 'children)
	(org-show-entry)
	(org-with-limited-levels (show-children))
	;; FIXME: This slows down the func way too much.
	;; How keep drawers hidden in subtree anyway?
	;; (when (memq 'org-cycle-hide-drawers org-cycle-hook)
	;;   (org-cycle-hide-drawers 'subtree))

	;; Fold every list in subtree to top-level items.
	(when (eq org-cycle-include-plain-lists 'integrate)
	  (save-excursion
	    (org-back-to-heading)
	    (while (org-list-search-forward (org-item-beginning-re) eos t)
	      (beginning-of-line 1)
	      (let* ((struct (org-list-struct))
		     (prevs (org-list-prevs-alist struct))
		     (end (org-list-get-bottom-point struct)))
		(mapc (lambda (e) (org-list-set-item-visibility e struct 'folded))
		      (org-list-get-all-items (point) struct prevs))
		(goto-char end))))))
      (message "CHILDREN")
      (save-excursion
	(goto-char eos)
	(outline-next-heading)
	(if (outline-invisible-p) (org-flag-heading nil)))
      (setq org-cycle-subtree-status 'children)
      (run-hook-with-args 'org-cycle-hook 'children))
     ((or children-skipped
	  (and (eq last-command this-command)
	       (eq org-cycle-subtree-status 'children)))
      ;; We just showed the children, or no children are there,
      ;; now show everything.
      (run-hook-with-args 'org-pre-cycle-hook 'subtree)
      (outline-flag-region eoh eos nil)
      (message (if children-skipped "SUBTREE (NO CHILDREN)" "SUBTREE"))
      (setq org-cycle-subtree-status 'subtree)
      (run-hook-with-args 'org-cycle-hook 'subtree))
     (t
      ;; Default action: hide the subtree.
      (run-hook-with-args 'org-pre-cycle-hook 'folded)
      (outline-flag-region eoh eos t)
      (message "FOLDED")
      (setq org-cycle-subtree-status 'folded)
      (run-hook-with-args 'org-cycle-hook 'folded)))))

;;;###autoload
(defun org-global-cycle (&optional arg)
  "Cycle the global visibility.  For details see `org-cycle'.
With \\[universal-argument] prefix arg, switch to startup visibility.
With a numeric prefix, show all headlines up to that level."
  (interactive "P")
  (let ((org-cycle-include-plain-lists
	 (if (eq major-mode 'org-mode) org-cycle-include-plain-lists nil)))
    (cond
     ((integerp arg)
      (show-all)
      (hide-sublevels arg)
      (setq org-cycle-global-status 'contents))
     ((equal arg '(4))
      (org-set-startup-visibility)
      (message "Startup visibility, plus VISIBILITY properties."))
     (t
      (org-cycle '(4))))))

(defun org-set-startup-visibility ()
  "Set the visibility required by startup options and properties."
  (cond
   ((eq org-startup-folded t)
    (org-cycle '(4)))
   ((eq org-startup-folded 'content)
    (let ((this-command 'org-cycle) (last-command 'org-cycle))
      (org-cycle '(4)) (org-cycle '(4)))))
  (unless (eq org-startup-folded 'showeverything)
    (if org-hide-block-startup (org-hide-block-all))
    (org-set-visibility-according-to-property 'no-cleanup)
    (org-cycle-hide-archived-subtrees 'all)
    (org-cycle-hide-drawers 'all)
    (org-cycle-show-empty-lines t)))

(defun org-set-visibility-according-to-property (&optional no-cleanup)
  "Switch subtree visibilities according to :VISIBILITY: property."
  (interactive)
  (let (org-show-entry-below state)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
	      "^[ \t]*:VISIBILITY:[ \t]+\\([a-z]+\\)"
	      nil t)
	(setq state (match-string 1))
	(save-excursion
	  (org-back-to-heading t)
	  (hide-subtree)
	  (org-reveal)
	  (cond
	   ((equal state '("fold" "folded"))
	    (hide-subtree))
	   ((equal state "children")
	    (org-show-hidden-entry)
	    (show-children))
	   ((equal state "content")
	    (save-excursion
	      (save-restriction
		(org-narrow-to-subtree)
		(org-content))))
	   ((member state '("all" "showall"))
	    (show-subtree)))))
      (unless no-cleanup
	(org-cycle-hide-archived-subtrees 'all)
	(org-cycle-hide-drawers 'all)
	(org-cycle-show-empty-lines 'all)))))

;; This function uses outline-regexp instead of the more fundamental
;; org-outline-regexp so that org-cycle-global works outside of Org
;; buffers, where outline-regexp is needed.
(defun org-overview ()
  "Switch to overview mode, showing only top-level headlines.
Really, this shows all headlines with level equal or greater than the level
of the first headline in the buffer.  This is important, because if the
first headline is not level one, then (hide-sublevels 1) gives confusing
results."
  (interactive)
  (let ((level (save-excursion
		 (goto-char (point-min))
		 (if (re-search-forward (concat "^" outline-regexp) nil t)
		     (progn
		       (goto-char (match-beginning 0))
		       (funcall outline-level))))))
    (and level (hide-sublevels level))))

(defun org-content (&optional arg)
  "Show all headlines in the buffer, like a table of contents.
With numerical argument N, show content up to level N."
  (interactive "P")
  (save-excursion
    ;; Visit all headings and show their offspring
    (and (integerp arg) (org-overview))
    (goto-char (point-max))
    (catch 'exit
      (while (and (progn (condition-case nil
			     (outline-previous-visible-heading 1)
			   (error (goto-char (point-min))))
			 t)
		  (looking-at org-outline-regexp))
	(if (integerp arg)
	    (show-children (1- arg))
	  (show-branches))
	(if (bobp) (throw 'exit nil))))))


(defun org-optimize-window-after-visibility-change (state)
  "Adjust the window after a change in outline visibility.
This function is the default value of the hook `org-cycle-hook'."
  (when (get-buffer-window (current-buffer))
    (cond
     ((eq state 'content)  nil)
     ((eq state 'all)      nil)
     ((eq state 'folded)   nil)
     ((eq state 'children) (or (org-subtree-end-visible-p) (recenter 1)))
     ((eq state 'subtree)  (or (org-subtree-end-visible-p) (recenter 1))))))

(defun org-remove-empty-overlays-at (pos)
  "Remove outline overlays that do not contain non-white stuff."
  (mapc
   (lambda (o)
     (and (eq 'outline (overlay-get o 'invisible))
	  (not (string-match "\\S-" (buffer-substring (overlay-start o)
						      (overlay-end o))))
	  (delete-overlay o)))
   (overlays-at pos)))

(defun org-clean-visibility-after-subtree-move ()
  "Fix visibility issues after moving a subtree."
  ;; First, find a reasonable region to look at:
  ;; Start two siblings above, end three below
  (let* ((beg (save-excursion
		(and (org-get-last-sibling)
		     (org-get-last-sibling))
		(point)))
	 (end (save-excursion
		(and (org-get-next-sibling)
		     (org-get-next-sibling)
		     (org-get-next-sibling))
		(if (org-at-heading-p)
		    (point-at-eol)
		  (point))))
	 (level (looking-at "\\*+"))
	 (re (if level (concat "^" (regexp-quote (match-string 0)) " "))))
    (save-excursion
      (save-restriction
	(narrow-to-region beg end)
	(when re
	  ;; Properly fold already folded siblings
	  (goto-char (point-min))
	  (while (re-search-forward re nil t)
	    (if (and (not (outline-invisible-p))
		     (save-excursion
		       (goto-char (point-at-eol)) (outline-invisible-p)))
		(hide-entry))))
	(org-cycle-show-empty-lines 'overview)
	(org-cycle-hide-drawers 'overview)))))

(defun org-cycle-show-empty-lines (state)
  "Show empty lines above all visible headlines.
The region to be covered depends on STATE when called through
`org-cycle-hook'.  Lisp program can use t for STATE to get the
entire buffer covered.  Note that an empty line is only shown if there
are at least `org-cycle-separator-lines' empty lines before the headline."
  (when (not (= org-cycle-separator-lines 0))
    (save-excursion
      (let* ((n (abs org-cycle-separator-lines))
	     (re (cond
		  ((= n 1) "\\(\n[ \t]*\n\\*+\\) ")
		  ((= n 2) "^[ \t]*\\(\n[ \t]*\n\\*+\\) ")
		  (t (let ((ns (number-to-string (- n 2))))
		       (concat "^\\(?:[ \t]*\n\\)\\{" ns "," ns "\\}"
			       "[ \t]*\\(\n[ \t]*\n\\*+\\) ")))))
	     beg end b e)
	(cond
	 ((memq state '(overview contents t))
	  (setq beg (point-min) end (point-max)))
	 ((memq state '(children folded))
	  (setq beg (point) end (progn (org-end-of-subtree t t)
				       (beginning-of-line 2)
				       (point)))))
	(when beg
	  (goto-char beg)
	  (while (re-search-forward re end t)
	    (unless (get-char-property (match-end 1) 'invisible)
	      (setq e (match-end 1))
	      (if (< org-cycle-separator-lines 0)
		  (setq b (save-excursion
			    (goto-char (match-beginning 0))
			    (org-back-over-empty-lines)
			    (if (save-excursion
				  (goto-char (max (point-min) (1- (point))))
				  (org-at-heading-p))
				(1- (point))
			      (point))))
		(setq b (match-beginning 1)))
	      (outline-flag-region b e nil)))))))
  ;; Never hide empty lines at the end of the file.
  (save-excursion
    (goto-char (point-max))
    (outline-previous-heading)
    (outline-end-of-heading)
    (if (and (looking-at "[ \t\n]+")
	     (= (match-end 0) (point-max)))
	(outline-flag-region (point) (match-end 0) nil))))

(defun org-show-empty-lines-in-parent ()
  "Move to the parent and re-show empty lines before visible headlines."
  (save-excursion
    (let ((context (if (org-up-heading-safe) 'children 'overview)))
      (org-cycle-show-empty-lines context))))

(defun org-files-list ()
  "Return `org-agenda-files' list, plus all open org-mode files.
This is useful for operations that need to scan all of a user's
open and agenda-wise Org files."
  (let ((files (mapcar 'expand-file-name (org-agenda-files))))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
	(if (and (eq major-mode 'org-mode) (buffer-file-name))
	    (let ((file (expand-file-name (buffer-file-name))))
	      (unless (member file files)
		(push file files))))))
    files))

(defsubst org-entry-beginning-position ()
  "Return the beginning position of the current entry."
  (save-excursion (outline-back-to-heading t) (point)))

(defsubst org-entry-end-position ()
  "Return the end position of the current entry."
  (save-excursion (outline-next-heading) (point)))

(defun org-cycle-hide-drawers (state)
  "Re-hide all drawers after a visibility state change."
  (when (and (eq major-mode 'org-mode)
	     (not (memq state '(overview folded contents))))
    (save-excursion
      (let* ((globalp (memq state '(contents all)))
             (beg (if globalp (point-min) (point)))
             (end (if globalp (point-max)
		    (if (eq state 'children)
			(save-excursion (outline-next-heading) (point))
		      (org-end-of-subtree t)))))
	(goto-char beg)
	(while (re-search-forward org-drawer-regexp end t)
	  (org-flag-drawer t))))))

(defun org-flag-drawer (flag)
  (save-excursion
    (beginning-of-line 1)
    (when (looking-at "^[ \t]*:[a-zA-Z][a-zA-Z0-9]*:")
      (let ((b (match-end 0)))
	(if (re-search-forward
	     "^[ \t]*:END:"
	     (save-excursion (outline-next-heading) (point)) t)
	    (outline-flag-region b (point-at-eol) flag)
	  (error ":END: line missing at position %s" b))))))

(defun org-subtree-end-visible-p ()
  "Is the end of the current subtree visible?"
  (pos-visible-in-window-p
   (save-excursion (org-end-of-subtree t) (point))))

(defun org-first-headline-recenter (&optional N)
  "Move cursor to the first headline and recenter the headline.
Optional argument N means put the headline into the Nth line of the window."
  (goto-char (point-min))
  (when (re-search-forward (concat "^\\(" org-outline-regexp "\\)") nil t)
    (beginning-of-line)
    (recenter (prefix-numeric-value N))))

;;; Saving and restoring visibility

(defun org-outline-overlay-data (&optional use-markers)
  "Return a list of the locations of all outline overlays.
These are overlays with the `invisible' property value `outline'.
The return value is a list of cons cells, with start and stop
positions for each overlay.
If USE-MARKERS is set, return the positions as markers."
  (let (beg end)
    (save-excursion
      (save-restriction
	(widen)
	(delq nil
	      (mapcar (lambda (o)
			(when (eq (overlay-get o 'invisible) 'outline)
			  (setq beg (overlay-start o)
				end (overlay-end o))
			  (and beg end (> end beg)
			       (if use-markers
				   (cons (move-marker (make-marker) beg)
					 (move-marker (make-marker) end))
				 (cons beg end)))))
		      (overlays-in (point-min) (point-max))))))))

(defun org-set-outline-overlay-data (data)
  "Create visibility overlays for all positions in DATA.
DATA should have been made by `org-outline-overlay-data'."
  (let (o)
    (save-excursion
      (save-restriction
	(widen)
	(show-all)
	(mapc (lambda (c)
		(outline-flag-region (car c) (cdr c) t))
	      data)))))

;;; Folding of blocks

(defvar org-hide-block-overlays nil
  "Overlays hiding blocks.")
(make-variable-buffer-local 'org-hide-block-overlays)

(defun org-block-map (function &optional start end)
  "Call FUNCTION at the head of all source blocks in the current buffer.
Optional arguments START and END can be used to limit the range."
  (let ((start (or start (point-min)))
        (end (or end (point-max))))
    (save-excursion
      (goto-char start)
      (while (and (< (point) end) (re-search-forward org-block-regexp end t))
	(save-excursion
	  (save-match-data
            (goto-char (match-beginning 0))
            (funcall function)))))))

(defun org-hide-block-toggle-all ()
  "Toggle the visibility of all blocks in the current buffer."
  (org-block-map #'org-hide-block-toggle))

(defun org-hide-block-all ()
  "Fold all blocks in the current buffer."
  (interactive)
  (org-show-block-all)
  (org-block-map #'org-hide-block-toggle-maybe))

(defun org-show-block-all ()
  "Unfold all blocks in the current buffer."
  (interactive)
  (mapc 'delete-overlay org-hide-block-overlays)
  (setq org-hide-block-overlays nil))

(defun org-hide-block-toggle-maybe ()
  "Toggle visibility of block at point."
  (interactive)
  (let ((case-fold-search t))
    (if (save-excursion
          (beginning-of-line 1)
          (looking-at org-block-regexp))
        (progn (org-hide-block-toggle)
               t) ;; to signal that we took action
      nil))) ;; to signal that we did not

(defun org-hide-block-toggle (&optional force)
  "Toggle the visibility of the current block."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward org-block-regexp nil t)
        (let ((start (- (match-beginning 4) 1)) ;; beginning of body
              (end (match-end 0)) ;; end of entire body
              ov)
          (if (memq t (mapcar (lambda (overlay)
                                (eq (overlay-get overlay 'invisible)
				    'org-hide-block))
                              (overlays-at start)))
              (if (or (not force) (eq force 'off))
                  (mapc (lambda (ov)
                          (when (member ov org-hide-block-overlays)
                            (setq org-hide-block-overlays
                                  (delq ov org-hide-block-overlays)))
                          (when (eq (overlay-get ov 'invisible)
                                    'org-hide-block)
                            (delete-overlay ov)))
                        (overlays-at start)))
            (setq ov (make-overlay start end))
            (overlay-put ov 'invisible 'org-hide-block)
            ;; make the block accessible to isearch
            (overlay-put
             ov 'isearch-open-invisible
             (lambda (ov)
               (when (member ov org-hide-block-overlays)
                 (setq org-hide-block-overlays
                       (delq ov org-hide-block-overlays)))
               (when (eq (overlay-get ov 'invisible)
                         'org-hide-block)
                 (delete-overlay ov))))
            (push ov org-hide-block-overlays)))
      (error "Not looking at a source block"))))

;; org-tab-after-check-for-cycling-hook
(add-hook 'org-tab-first-hook 'org-hide-block-toggle-maybe)
;; Remove overlays when changing major mode
(add-hook 'org-mode-hook
	  (lambda () (org-add-hook 'change-major-mode-hook
				   'org-show-block-all 'append 'local)))

;;; Org-goto

(defvar org-goto-window-configuration nil)
(defvar org-goto-marker nil)
(defvar org-goto-map
  (let ((map (make-sparse-keymap)))
    (let ((cmds '(isearch-forward isearch-backward kill-ring-save set-mark-command mouse-drag-region universal-argument org-occur)) cmd)
      (while (setq cmd (pop cmds))
	(substitute-key-definition cmd cmd map global-map)))
    (suppress-keymap map)
    (org-defkey map "\C-m"     'org-goto-ret)
    (org-defkey map [(return)] 'org-goto-ret)
    (org-defkey map [(left)]   'org-goto-left)
    (org-defkey map [(right)]  'org-goto-right)
    (org-defkey map [(control ?g)] 'org-goto-quit)
    (org-defkey map "\C-i" 'org-cycle)
    (org-defkey map [(tab)] 'org-cycle)
    (org-defkey map [(down)] 'outline-next-visible-heading)
    (org-defkey map [(up)] 'outline-previous-visible-heading)
    (if org-goto-auto-isearch
	(if (fboundp 'define-key-after)
	    (define-key-after map [t] 'org-goto-local-auto-isearch)
	  nil)
      (org-defkey map "q" 'org-goto-quit)
      (org-defkey map "n" 'outline-next-visible-heading)
      (org-defkey map "p" 'outline-previous-visible-heading)
      (org-defkey map "f" 'outline-forward-same-level)
      (org-defkey map "b" 'outline-backward-same-level)
      (org-defkey map "u" 'outline-up-heading))
    (org-defkey map "/" 'org-occur)
    (org-defkey map "\C-c\C-n" 'outline-next-visible-heading)
    (org-defkey map "\C-c\C-p" 'outline-previous-visible-heading)
    (org-defkey map "\C-c\C-f" 'outline-forward-same-level)
    (org-defkey map "\C-c\C-b" 'outline-backward-same-level)
    (org-defkey map "\C-c\C-u" 'outline-up-heading)
    map))

(defconst org-goto-help
"Browse buffer copy, to find location or copy text.  Just type for auto-isearch.
RET=jump to location             [Q]uit and return to previous location
\[Up]/[Down]=next/prev headline   TAB=cycle visibility   [/] org-occur")

(defvar org-goto-start-pos) ; dynamically scoped parameter

;; FIXME: Docstring does not mention both interfaces
(defun org-goto (&optional alternative-interface)
  "Look up a different location in the current file, keeping current visibility.

When you want look-up or go to a different location in a document, the
fastest way is often to fold the entire buffer and then dive into the tree.
This method has the disadvantage, that the previous location will be folded,
which may not be what you want.

This command works around this by showing a copy of the current buffer
in an indirect buffer, in overview mode.  You can dive into the tree in
that copy, use org-occur and incremental search to find a location.
When pressing RET or `Q', the command returns to the original buffer in
which the visibility is still unchanged.  After RET it will also jump to
the location selected in the indirect buffer and expose the headline
hierarchy above."
  (interactive "P")
  (let* ((org-refile-targets `((nil . (:maxlevel . ,org-goto-max-level))))
	 (org-refile-use-outline-path t)
	 (org-refile-target-verify-function nil)
	 (interface
	  (if (not alternative-interface)
	      org-goto-interface
	    (if (eq org-goto-interface 'outline)
		'outline-path-completion
	      'outline)))
	 (org-goto-start-pos (point))
	 (selected-point
	  (if (eq interface 'outline)
	      (car (org-get-location (current-buffer) org-goto-help))
	    (let ((pa (org-refile-get-location "Goto" nil nil t)))
	      (org-refile-check-position pa)
	      (nth 3 pa)))))
    (if selected-point
	(progn
	  (org-mark-ring-push org-goto-start-pos)
	  (goto-char selected-point)
	  (if (or (outline-invisible-p) (org-invisible-p2))
	      (org-show-context 'org-goto)))
      (message "Quit"))))

(defvar org-goto-selected-point nil) ; dynamically scoped parameter
(defvar org-goto-exit-command nil) ; dynamically scoped parameter
(defvar org-goto-local-auto-isearch-map) ; defined below

(defun org-get-location (buf help)
  "Let the user select a location in the Org-mode buffer BUF.
This function uses a recursive edit.  It returns the selected position
or nil."
  (let ((isearch-mode-map org-goto-local-auto-isearch-map)
	(isearch-hide-immediately nil)
	(isearch-search-fun-function
	 (lambda () 'org-goto-local-search-headings))
	(org-goto-selected-point org-goto-exit-command)
	(pop-up-frames nil)
	(special-display-buffer-names nil)
	(special-display-regexps nil)
	(special-display-function nil))
    (save-excursion
      (save-window-excursion
	(delete-other-windows)
	(and (get-buffer "*org-goto*") (kill-buffer "*org-goto*"))
	(org-pop-to-buffer-same-window
	 (condition-case nil
	     (make-indirect-buffer (current-buffer) "*org-goto*")
	   (error (make-indirect-buffer (current-buffer) "*org-goto*"))))
	(with-output-to-temp-buffer "*Help*"
	  (princ help))
	(org-fit-window-to-buffer (get-buffer-window "*Help*"))
	(setq buffer-read-only nil)
	(let ((org-startup-truncated t)
	      (org-startup-folded nil)
	      (org-startup-align-all-tables nil))
	  (org-mode)
	  (org-overview))
	(setq buffer-read-only t)
	(if (and (boundp 'org-goto-start-pos)
		 (integer-or-marker-p org-goto-start-pos))
	    (let ((org-show-hierarchy-above t)
		  (org-show-siblings t)
		  (org-show-following-heading t))
	      (goto-char org-goto-start-pos)
	      (and (outline-invisible-p) (org-show-context)))
	  (goto-char (point-min)))
	(let (org-special-ctrl-a/e) (org-beginning-of-line))
	(message "Select location and press RET")
	(use-local-map org-goto-map)
	(recursive-edit)
	))
    (kill-buffer "*org-goto*")
    (cons org-goto-selected-point org-goto-exit-command)))

(defvar org-goto-local-auto-isearch-map (make-sparse-keymap))
(set-keymap-parent org-goto-local-auto-isearch-map isearch-mode-map)
(define-key org-goto-local-auto-isearch-map "\C-i" 'isearch-other-control-char)
(define-key org-goto-local-auto-isearch-map "\C-m" 'isearch-other-control-char)

(defun org-goto-local-search-headings (string bound noerror)
  "Search and make sure that any matches are in headlines."
  (catch 'return
    (while (if isearch-forward
               (search-forward string bound noerror)
             (search-backward string bound noerror))
      (when (let ((context (mapcar 'car (save-match-data (org-context)))))
	      (and (member :headline context)
		   (not (member :tags context))))
	(throw 'return (point))))))

(defun org-goto-local-auto-isearch ()
  "Start isearch."
 (interactive)
 (goto-char (point-min))
 (let ((keys (this-command-keys)))
   (when (eq (lookup-key isearch-mode-map keys) 'isearch-printing-char)
     (isearch-mode t)
     (isearch-process-search-char (string-to-char keys)))))

(defun org-goto-ret (&optional arg)
  "Finish `org-goto' by going to the new location."
  (interactive "P")
  (setq org-goto-selected-point (point)
	org-goto-exit-command 'return)
  (throw 'exit nil))

(defun org-goto-left ()
  "Finish `org-goto' by going to the new location."
  (interactive)
  (if (org-at-heading-p)
      (progn
	(beginning-of-line 1)
	(setq org-goto-selected-point (point)
	      org-goto-exit-command 'left)
	(throw 'exit nil))
    (error "Not on a heading")))

(defun org-goto-right ()
  "Finish `org-goto' by going to the new location."
  (interactive)
  (if (org-at-heading-p)
      (progn
	(setq org-goto-selected-point (point)
	      org-goto-exit-command 'right)
	(throw 'exit nil))
    (error "Not on a heading")))

(defun org-goto-quit ()
  "Finish `org-goto' without cursor motion."
  (interactive)
  (setq org-goto-selected-point nil)
  (setq org-goto-exit-command 'quit)
  (throw 'exit nil))

;;; Indirect buffer display of subtrees

(defvar org-indirect-dedicated-frame nil
  "This is the frame being used for indirect tree display.")
(defvar org-last-indirect-buffer nil)

(defun org-tree-to-indirect-buffer (&optional arg)
  "Create indirect buffer and narrow it to current subtree.
With numerical prefix ARG, go up to this level and then take that tree.
If ARG is negative, go up that many levels.
If `org-indirect-buffer-display' is not `new-frame', the command removes the
indirect buffer previously made with this command, to avoid proliferation of
indirect buffers.  However, when you call the command with a \
\\[universal-argument] prefix, or
when `org-indirect-buffer-display' is `new-frame', the last buffer
is kept so that you can work with several indirect buffers at the same time.
If `org-indirect-buffer-display' is `dedicated-frame', the \
\\[universal-argument] prefix also
requests that a new frame be made for the new buffer, so that the dedicated
frame is not changed."
  (interactive "P")
  (let ((cbuf (current-buffer))
	(cwin (selected-window))
	(pos (point))
	beg end level heading ibuf)
    (save-excursion
      (org-back-to-heading t)
      (when (numberp arg)
	(setq level (org-outline-level))
	(if (< arg 0) (setq arg (+ level arg)))
	(while (> (setq level (org-outline-level)) arg)
	  (outline-up-heading 1 t)))
      (setq beg (point)
	    heading (org-get-heading))
      (org-end-of-subtree t t)
      (if (org-at-heading-p) (backward-char 1))
      (setq end (point)))
    (if (and (buffer-live-p org-last-indirect-buffer)
	     (not (eq org-indirect-buffer-display 'new-frame))
	     (not arg))
	(kill-buffer org-last-indirect-buffer))
    (setq ibuf (org-get-indirect-buffer cbuf)
	  org-last-indirect-buffer ibuf)
    (cond
     ((or (eq org-indirect-buffer-display 'new-frame)
	  (and arg (eq org-indirect-buffer-display 'dedicated-frame)))
      (select-frame (make-frame))
      (delete-other-windows)
      (org-pop-to-buffer-same-window ibuf)
      (org-set-frame-title heading))
     ((eq org-indirect-buffer-display 'dedicated-frame)
      (raise-frame
       (select-frame (or (and org-indirect-dedicated-frame
			      (frame-live-p org-indirect-dedicated-frame)
			      org-indirect-dedicated-frame)
			 (setq org-indirect-dedicated-frame (make-frame)))))
      (delete-other-windows)
      (org-pop-to-buffer-same-window ibuf)
      (org-set-frame-title (concat "Indirect: " heading)))
     ((eq org-indirect-buffer-display 'current-window)
      (org-pop-to-buffer-same-window ibuf))
     ((eq org-indirect-buffer-display 'other-window)
      (pop-to-buffer ibuf))
     (t (error "Invalid value")))
    (if (featurep 'xemacs)
        (save-excursion (org-mode) (turn-on-font-lock)))
    (narrow-to-region beg end)
    (show-all)
    (goto-char pos)
    (run-hook-with-args 'org-cycle-hook 'all)
    (and (window-live-p cwin) (select-window cwin))))

(defun org-get-indirect-buffer (&optional buffer)
  (setq buffer (or buffer (current-buffer)))
  (let ((n 1) (base (buffer-name buffer)) bname)
    (while (buffer-live-p
	    (get-buffer (setq bname (concat base "-" (number-to-string n)))))
      (setq n (1+ n)))
    (condition-case nil
        (make-indirect-buffer buffer bname 'clone)
      (error (make-indirect-buffer buffer bname)))))

(defun org-set-frame-title (title)
  "Set the title of the current frame to the string TITLE."
  ;; FIXME: how to name a single frame in XEmacs???
  (unless (featurep 'xemacs)
    (modify-frame-parameters (selected-frame) (list (cons 'name title)))))

;;;; Structure editing

;;; Inserting headlines

(defun org-previous-line-empty-p ()
  (save-excursion
    (and (not (bobp))
	 (or (beginning-of-line 0) t)
	 (save-match-data
	   (looking-at "[ \t]*$")))))

(defun org-insert-heading (&optional force-heading invisible-ok)
  "Insert a new heading or item with same depth at point.
If point is in a plain list and FORCE-HEADING is nil, create a new list item.
If point is at the beginning of a headline, insert a sibling before the
current headline.  If point is not at the beginning, split the line,
create the new headline with the text in the current line after point
\(but see also the variable `org-M-RET-may-split-line').

When INVISIBLE-OK is set, stop at invisible headlines when going back.
This is important for non-interactive uses of the command."
  (interactive "P")
  (if (or (= (buffer-size) 0)
	  (and (not (save-excursion
		      (and (ignore-errors (org-back-to-heading invisible-ok))
			   (org-at-heading-p))))
	       (or force-heading (not (org-in-item-p)))))
      (progn
	(insert "\n* ")
	(run-hooks 'org-insert-heading-hook))
    (when (or force-heading (not (org-insert-item)))
      (let* ((empty-line-p nil)
	     (level nil)
	     (on-heading (org-at-heading-p))
	     (head (save-excursion
		     (condition-case nil
			 (progn
			   (org-back-to-heading invisible-ok)
			   (when (and (not on-heading)
				      (featurep 'org-inlinetask)
				      (integerp org-inlinetask-min-level)
				      (>= (length (match-string 0))
					  org-inlinetask-min-level))
			     ;; Find a heading level before the inline task
			     (while (and (setq level (org-up-heading-safe))
					 (>= level org-inlinetask-min-level)))
			     (if (org-at-heading-p)
				 (org-back-to-heading invisible-ok)
			       (error "This should not happen")))
			   (setq empty-line-p (org-previous-line-empty-p))
			   (match-string 0))
		       (error "*"))))
	     (blank-a (cdr (assq 'heading org-blank-before-new-entry)))
	     (blank (if (eq blank-a 'auto) empty-line-p blank-a))
	     pos hide-previous previous-pos)
	(cond
	 ((and (org-at-heading-p) (bolp)
	       (or (bobp)
		   (save-excursion (backward-char 1) (not (outline-invisible-p)))))
	  ;; insert before the current line
	  (open-line (if blank 2 1)))
	 ((and (bolp)
	       (not org-insert-heading-respect-content)
	       (or (bobp)
		   (save-excursion
		     (backward-char 1) (not (outline-invisible-p)))))
	  ;; insert right here
	  nil)
	 (t
	  ;; somewhere in the line
          (save-excursion
	    (setq previous-pos (point-at-bol))
            (end-of-line)
            (setq hide-previous (outline-invisible-p)))
	  (and org-insert-heading-respect-content (org-show-subtree))
	  (let ((split
		 (and (org-get-alist-option org-M-RET-may-split-line 'headline)
		      (save-excursion
			(let ((p (point)))
			  (goto-char (point-at-bol))
			  (and (looking-at org-complex-heading-regexp)
			       (match-beginning 4)
			       (> p (match-beginning 4)))))))
		tags pos)
	    (cond
	     (org-insert-heading-respect-content
	      (org-end-of-subtree nil t)
	      (when (featurep 'org-inlinetask)
		(while (and (not (eobp))
			    (looking-at "\\(\\*+\\)[ \t]+")
			    (>= (length (match-string 1))
				org-inlinetask-min-level))
		  (org-end-of-subtree nil t)))
	      (or (bolp) (newline))
	      (or (org-previous-line-empty-p)
		  (and blank (newline)))
	      (open-line 1))
	     ((org-at-heading-p)
	      (when hide-previous
		(show-children)
		(org-show-entry))
	      (looking-at ".*?\\([ \t]+\\(:[[:alnum:]_@#%:]+:\\)\\)?[ \t]*$")
	      (setq tags (and (match-end 2) (match-string 2)))
	      (and (match-end 1)
		   (delete-region (match-beginning 1) (match-end 1)))
	      (setq pos (point-at-bol))
	      (or split (end-of-line 1))
	      (delete-horizontal-space)
	      (if (string-match "\\`\\*+\\'"
				(buffer-substring (point-at-bol) (point)))
		  (insert " "))
	      (newline (if blank 2 1))
	      (when tags
		(save-excursion
		  (goto-char pos)
		  (end-of-line 1)
		  (insert " " tags)
		  (org-set-tags nil 'align))))
	     (t
	      (or split (end-of-line 1))
	      (newline (if blank 2 1)))))))
	(insert head) (just-one-space)
	(setq pos (point))
	(end-of-line 1)
	(unless (= (point) pos) (just-one-space) (backward-delete-char 1))
        (when (and org-insert-heading-respect-content hide-previous)
	  (save-excursion
	    (goto-char previous-pos)
	    (hide-subtree)))
	(run-hooks 'org-insert-heading-hook)))))

(defun org-get-heading (&optional no-tags no-todo)
  "Return the heading of the current entry, without the stars.
When NO-TAGS is non-nil, don't include tags.
When NO-TODO is non-nil, don't include TODO keywords."
  (save-excursion
    (org-back-to-heading t)
    (cond
     ((and no-tags no-todo)
      (looking-at org-complex-heading-regexp)
      (match-string 4))
     (no-tags
      (looking-at (concat org-outline-regexp
			  "\\(.*?\\)"
			  "\\(?:[ \t]+:[[:alnum:]:_@#%]+:\\)?[ \t]*$"))
      (match-string 1))
     (no-todo
      (looking-at org-todo-line-regexp)
      (match-string 3))
     (t (looking-at org-heading-regexp)
	(match-string 2)))))

(defun org-heading-components ()
  "Return the components of the current heading.
This is a list with the following elements:
- the level as an integer
- the reduced level, different if `org-odd-levels-only' is set.
- the TODO keyword, or nil
- the priority character, like ?A, or nil if no priority is given
- the headline text itself, or the tags string if no headline text
- the tags string, or nil."
  (save-excursion
    (org-back-to-heading t)
    (if (let (case-fold-search) (looking-at org-complex-heading-regexp))
	(list (length (match-string 1))
	      (org-reduced-level (length (match-string 1)))
	      (org-match-string-no-properties 2)
	      (and (match-end 3) (aref (match-string 3) 2))
	      (org-match-string-no-properties 4)
	      (org-match-string-no-properties 5)))))

(defun org-get-entry ()
  "Get the entry text, after heading, entire subtree."
  (save-excursion
    (org-back-to-heading t)
    (buffer-substring (point-at-bol 2) (org-end-of-subtree t))))

(defun org-insert-heading-after-current ()
  "Insert a new heading with same level as current, after current subtree."
  (interactive)
  (org-back-to-heading)
  (org-insert-heading)
  (org-move-subtree-down)
  (end-of-line 1))

(defun org-insert-heading-respect-content ()
  (interactive)
  (let ((org-insert-heading-respect-content t))
    (org-insert-heading t)))

(defun org-insert-todo-heading-respect-content (&optional force-state)
  (interactive "P")
  (let ((org-insert-heading-respect-content t))
    (org-insert-todo-heading force-state t)))

(defun org-insert-todo-heading (arg &optional force-heading)
  "Insert a new heading with the same level and TODO state as current heading.
If the heading has no TODO state, or if the state is DONE, use the first
state (TODO by default).  Also with prefix arg, force first state."
  (interactive "P")
  (when (or force-heading (not (org-insert-item 'checkbox)))
    (org-insert-heading force-heading)
    (save-excursion
      (org-back-to-heading)
      (outline-previous-heading)
      (looking-at org-todo-line-regexp))
    (let*
        ((new-mark-x
	  (if (or arg
		  (not (match-beginning 2))
		  (member (match-string 2) org-done-keywords))
 	      (car org-todo-keywords-1)
	    (match-string 2)))
	 (new-mark
	  (or
	   (run-hook-with-args-until-success
	    'org-todo-get-default-hook new-mark-x nil)
	   new-mark-x)))
      (beginning-of-line 1)
      (and (looking-at org-outline-regexp) (goto-char (match-end 0))
	   (if org-treat-insert-todo-heading-as-state-change
	       (org-todo new-mark)
	     (insert new-mark " "))))
    (when org-provide-todo-statistics
      (org-update-parent-todo-statistics))))

(defun org-insert-subheading (arg)
  "Insert a new subheading and demote it.
Works for outline headings and for plain lists alike."
  (interactive "P")
  (org-insert-heading arg)
  (cond
   ((org-at-heading-p) (org-do-demote))
   ((org-at-item-p) (org-indent-item))))

(defun org-insert-todo-subheading (arg)
  "Insert a new subheading with TODO keyword or checkbox and demote it.
Works for outline headings and for plain lists alike."
  (interactive "P")
  (org-insert-todo-heading arg)
  (cond
   ((org-at-heading-p) (org-do-demote))
   ((org-at-item-p) (org-indent-item))))

;;; Promotion and Demotion

(defvar org-after-demote-entry-hook nil
  "Hook run after an entry has been demoted.
The cursor will be at the beginning of the entry.
When a subtree is being demoted, the hook will be called for each node.")

(defvar org-after-promote-entry-hook nil
  "Hook run after an entry has been promoted.
The cursor will be at the beginning of the entry.
When a subtree is being promoted, the hook will be called for each node.")

(defun org-promote-subtree ()
  "Promote the entire subtree.
See also `org-promote'."
  (interactive)
  (save-excursion
    (org-with-limited-levels (org-map-tree 'org-promote)))
  (org-fix-position-after-promote))

(defun org-demote-subtree ()
  "Demote the entire subtree.  See `org-demote'.
See also `org-promote'."
  (interactive)
  (save-excursion
    (org-with-limited-levels (org-map-tree 'org-demote)))
  (org-fix-position-after-promote))


(defun org-do-promote ()
  "Promote the current heading higher up the tree.
If the region is active in `transient-mark-mode', promote all headings
in the region."
  (interactive)
  (save-excursion
    (if (org-region-active-p)
	(org-map-region 'org-promote (region-beginning) (region-end))
      (org-promote)))
  (org-fix-position-after-promote))

(defun org-do-demote ()
  "Demote the current heading lower down the tree.
If the region is active in `transient-mark-mode', demote all headings
in the region."
  (interactive)
  (save-excursion
    (if (org-region-active-p)
	(org-map-region 'org-demote (region-beginning) (region-end))
      (org-demote)))
  (org-fix-position-after-promote))

(defun org-fix-position-after-promote ()
  "Make sure that after pro/demotion cursor position is right."
  (let ((pos (point)))
    (when (save-excursion
	    (beginning-of-line 1)
	    (looking-at org-todo-line-regexp)
	    (or (equal pos (match-end 1)) (equal pos (match-end 2))))
      (cond ((eobp) (insert " "))
	    ((eolp) (insert " "))
	    ((equal (char-after) ?\ ) (forward-char 1))))))

(defun org-current-level ()
  "Return the level of the current entry, or nil if before the first headline.
The level is the number of stars at the beginning of the headline."
  (save-excursion
    (org-with-limited-levels
     (if (ignore-errors (org-back-to-heading t))
	 (funcall outline-level)))))

(defun org-get-previous-line-level ()
  "Return the outline depth of the last headline before the current line.
Returns 0 for the first headline in the buffer, and nil if before the
first headline."
  (let ((current-level (org-current-level))
	(prev-level (when (> (line-number-at-pos) 1)
		      (save-excursion
			(beginning-of-line 0)
			(org-current-level)))))
    (cond ((null current-level) nil) ; Before first headline
	  ((null prev-level) 0)      ; At first headline
	  (prev-level))))

(defun org-reduced-level (l)
  "Compute the effective level of a heading.
This takes into account the setting of `org-odd-levels-only'."
  (cond
   ((zerop l) 0)
   (org-odd-levels-only (1+ (floor (/ l 2))))
   (t l)))

(defun org-level-increment ()
  "Return the number of stars that will be added or removed at a
time to headlines when structure editing, based on the value of
`org-odd-levels-only'."
  (if org-odd-levels-only 2 1))

(defun org-get-valid-level (level &optional change)
  "Rectify a level change under the influence of `org-odd-levels-only'
LEVEL is a current level, CHANGE is by how much the level should be
modified.  Even if CHANGE is nil, LEVEL may be returned modified because
even level numbers will become the next higher odd number."
  (if org-odd-levels-only
      (cond ((or (not change) (= 0 change)) (1+ (* 2 (/ level 2))))
	    ((> change 0) (1+ (* 2 (/ (+ level (* 2 change)) 2))))
	    ((< change 0) (max 1 (1+ (* 2 (/ (+ level (* 2 change)) 2))))))
    (max 1 (+ level (or change 0)))))

(if (boundp 'define-obsolete-function-alias)
    (if (or (featurep 'xemacs) (< emacs-major-version 23))
	(define-obsolete-function-alias 'org-get-legal-level
	  'org-get-valid-level)
      (define-obsolete-function-alias 'org-get-legal-level
	'org-get-valid-level "23.1")))

(defun org-promote ()
  "Promote the current heading higher up the tree.
If the region is active in `transient-mark-mode', promote all headings
in the region."
  (org-back-to-heading t)
  (let* ((level (save-match-data (funcall outline-level)))
	 (after-change-functions (remove 'flyspell-after-change-function
					  after-change-functions))
	 (up-head (concat (make-string (org-get-valid-level level -1) ?*) " "))
	 (diff (abs (- level (length up-head) -1))))
    (if (= level 1) (error "Cannot promote to level 0.  UNDO to recover if necessary"))
    (replace-match up-head nil t)
    ;; Fixup tag positioning
    (and org-auto-align-tags (org-set-tags nil t))
    (if org-adapt-indentation (org-fixup-indentation (- diff)))
    (run-hooks 'org-after-promote-entry-hook)))

(defun org-demote ()
  "Demote the current heading lower down the tree.
If the region is active in `transient-mark-mode', demote all headings
in the region."
  (org-back-to-heading t)
  (let* ((level (save-match-data (funcall outline-level)))
	 (after-change-functions (remove 'flyspell-after-change-function
					  after-change-functions))
	 (down-head (concat (make-string (org-get-valid-level level 1) ?*) " "))
	 (diff (abs (- level (length down-head) -1))))
    (replace-match down-head nil t)
    ;; Fixup tag positioning
    (and org-auto-align-tags (org-set-tags nil t))
    (if org-adapt-indentation (org-fixup-indentation diff))
    (run-hooks 'org-after-demote-entry-hook)))

(defun org-cycle-level ()
  "Cycle the level of an empty headline through possible states.
This goes first to child, then to parent, level, then up the hierarchy.
After top level, it switches back to sibling level."
  (interactive)
  (let ((org-adapt-indentation nil))
    (when (org-point-at-end-of-empty-headline)
      (setq this-command 'org-cycle-level) ; Only needed for caching
      (let ((cur-level (org-current-level))
            (prev-level (org-get-previous-line-level)))
        (cond
         ;; If first headline in file, promote to top-level.
         ((= prev-level 0)
          (loop repeat (/ (- cur-level 1) (org-level-increment))
                do (org-do-promote)))
         ;; If same level as prev, demote one.
         ((= prev-level cur-level)
          (org-do-demote))
         ;; If parent is top-level, promote to top level if not already.
         ((= prev-level 1)
          (loop repeat (/ (- cur-level 1) (org-level-increment))
                do (org-do-promote)))
         ;; If top-level, return to prev-level.
         ((= cur-level 1)
          (loop repeat (/ (- prev-level 1) (org-level-increment))
                do (org-do-demote)))
         ;; If less than prev-level, promote one.
         ((< cur-level prev-level)
          (org-do-promote))
         ;; If deeper than prev-level, promote until higher than
         ;; prev-level.
         ((> cur-level prev-level)
          (loop repeat (+ 1 (/ (- cur-level prev-level) (org-level-increment)))
                do (org-do-promote))))
        t))))

(defun org-map-tree (fun)
  "Call FUN for every heading underneath the current one."
  (org-back-to-heading)
  (let ((level (funcall outline-level)))
    (save-excursion
      (funcall fun)
      (while (and (progn
		    (outline-next-heading)
		    (> (funcall outline-level) level))
		  (not (eobp)))
	(funcall fun)))))

(defun org-map-region (fun beg end)
  "Call FUN for every heading between BEG and END."
  (let ((org-ignore-region t))
    (save-excursion
      (setq end (copy-marker end))
      (goto-char beg)
      (if (and (re-search-forward org-outline-regexp-bol nil t)
	       (< (point) end))
	  (funcall fun))
      (while (and (progn
		    (outline-next-heading)
		    (< (point) end))
		  (not (eobp)))
	(funcall fun)))))

(defvar org-property-end-re) ; silence byte-compiler
(defun org-fixup-indentation (diff)
  "Change the indentation in the current entry by DIFF.
However, if any line in the current entry has no indentation, or if it
would end up with no indentation after the change, nothing at all is done."
  (save-excursion
    (let ((end (save-excursion (outline-next-heading)
			       (point-marker)))
	  (prohibit (if (> diff 0)
			"^\\S-"
		      (concat "^ \\{0," (int-to-string (- diff)) "\\}\\S-")))
	  col)
      (unless (save-excursion (end-of-line 1)
			      (re-search-forward prohibit end t))
	(while (and (< (point) end)
		    (re-search-forward "^[ \t]+" end t))
	  (goto-char (match-end 0))
	  (setq col (current-column))
	  (if (< diff 0) (replace-match ""))
	  (org-indent-to-column (+ diff col))))
      (move-marker end nil))))

(defun org-convert-to-odd-levels ()
  "Convert an org-mode file with all levels allowed to one with odd levels.
This will leave level 1 alone, convert level 2 to level 3, level 3 to
level 5 etc."
  (interactive)
  (when (yes-or-no-p "Are you sure you want to globally change levels to odd? ")
    (let ((outline-level 'org-outline-level)
	  (org-odd-levels-only nil) n)
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "^\\*\\*+ " nil t)
	  (setq n (- (length (match-string 0)) 2))
	  (while (>= (setq n (1- n)) 0)
	    (org-demote))
	  (end-of-line 1))))))

(defun org-convert-to-oddeven-levels ()
  "Convert an org-mode file with only odd levels to one with odd/even levels.
This promotes level 3 to level 2, level 5 to level 3 etc.  If the
file contains a section with an even level, conversion would
destroy the structure of the file.  An error is signaled in this
case."
  (interactive)
  (goto-char (point-min))
  ;; First check if there are no even levels
  (when (re-search-forward "^\\(\\*\\*\\)+ " nil t)
    (org-show-context t)
    (error "Not all levels are odd in this file.  Conversion not possible"))
  (when (yes-or-no-p "Are you sure you want to globally change levels to odd-even? ")
    (let ((outline-regexp org-outline-regexp)
	  (outline-level 'org-outline-level)
	  (org-odd-levels-only nil) n)
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "^\\*\\*+ " nil t)
	  (setq n (/ (1- (length (match-string 0))) 2))
	  (while (>= (setq n (1- n)) 0)
	    (org-promote))
	  (end-of-line 1))))))

(defun org-tr-level (n)
  "Make N odd if required."
  (if org-odd-levels-only (1+ (/ n 2)) n))

;;; Vertical tree motion, cutting and pasting of subtrees

(defun org-move-subtree-up (&optional arg)
  "Move the current subtree up past ARG headlines of the same level."
  (interactive "p")
  (org-move-subtree-down (- (prefix-numeric-value arg))))

(defun org-move-subtree-down (&optional arg)
  "Move the current subtree down past ARG headlines of the same level."
  (interactive "p")
  (setq arg (prefix-numeric-value arg))
  (let ((movfunc (if (> arg 0) 'org-get-next-sibling
		   'org-get-last-sibling))
	(ins-point (make-marker))
	(cnt (abs arg))
	(col (current-column))
	beg beg0 end txt folded ne-beg ne-end ne-ins ins-end)
    ;; Select the tree
    (org-back-to-heading)
    (setq beg0 (point))
    (save-excursion
      (setq ne-beg (org-back-over-empty-lines))
      (setq beg (point)))
    (save-match-data
      (save-excursion (outline-end-of-heading)
		      (setq folded (outline-invisible-p)))
      (outline-end-of-subtree))
    (outline-next-heading)
    (setq ne-end (org-back-over-empty-lines))
    (setq end (point))
    (goto-char beg0)
    (when (and (> arg 0) (org-first-sibling-p) (< ne-end ne-beg))
      ;; include less whitespace
      (save-excursion
	(goto-char beg)
	(forward-line (- ne-beg ne-end))
	(setq beg (point))))
    ;; Find insertion point, with error handling
    (while (> cnt 0)
      (or (and (funcall movfunc) (looking-at org-outline-regexp))
	  (progn (goto-char beg0)
		 (error "Cannot move past superior level or buffer limit")))
      (setq cnt (1- cnt)))
    (if (> arg 0)
	;; Moving forward - still need to move over subtree
	(progn (org-end-of-subtree t t)
	       (save-excursion
		 (org-back-over-empty-lines)
		 (or (bolp) (newline)))))
    (setq ne-ins (org-back-over-empty-lines))
    (move-marker ins-point (point))
    (setq txt (buffer-substring beg end))
    (org-save-markers-in-region beg end)
    (delete-region beg end)
    (org-remove-empty-overlays-at beg)
    (or (= beg (point-min)) (outline-flag-region (1- beg) beg nil))
    (or (bobp) (outline-flag-region (1- (point)) (point) nil))
    (and (not (bolp)) (looking-at "\n") (forward-char 1))
    (let ((bbb (point)))
      (insert-before-markers txt)
      (org-reinstall-markers-in-region bbb)
      (move-marker ins-point bbb))
    (or (bolp) (insert "\n"))
    (setq ins-end (point))
    (goto-char ins-point)
    (org-skip-whitespace)
    (when (and (< arg 0)
	       (org-first-sibling-p)
	       (> ne-ins ne-beg))
      ;; Move whitespace back to beginning
      (save-excursion
	(goto-char ins-end)
	(let ((kill-whole-line t))
	  (kill-line (- ne-ins ne-beg)) (point)))
      (insert (make-string (- ne-ins ne-beg) ?\n)))
    (move-marker ins-point nil)
    (if folded
	(hide-subtree)
      (org-show-entry)
      (show-children)
      (org-cycle-hide-drawers 'children))
    (org-clean-visibility-after-subtree-move)
    ;; move back to the initial column we were at
    (move-to-column col)))

(defvar org-subtree-clip ""
  "Clipboard for cut and paste of subtrees.
This is actually only a copy of the kill, because we use the normal kill
ring.  We need it to check if the kill was created by `org-copy-subtree'.")

(defvar org-subtree-clip-folded nil
  "Was the last copied subtree folded?
This is used to fold the tree back after pasting.")

(defun org-cut-subtree (&optional n)
  "Cut the current subtree into the clipboard.
With prefix arg N, cut this many sequential subtrees.
This is a short-hand for marking the subtree and then cutting it."
  (interactive "p")
  (org-copy-subtree n 'cut))

(defun org-copy-subtree (&optional n cut force-store-markers)
  "Cut the current subtree into the clipboard.
With prefix arg N, cut this many sequential subtrees.
This is a short-hand for marking the subtree and then copying it.
If CUT is non-nil, actually cut the subtree.
If FORCE-STORE-MARKERS is non-nil, store the relative locations
of some markers in the region, even if CUT is non-nil.  This is
useful if the caller implements cut-and-paste as copy-then-paste-then-cut."
  (interactive "p")
  (let (beg end folded (beg0 (point)))
    (if (org-called-interactively-p 'any)
	(org-back-to-heading nil) ; take what looks like a subtree
      (org-back-to-heading t)) ; take what is really there
    (org-back-over-empty-lines)
    (setq beg (point))
    (skip-chars-forward " \t\r\n")
    (save-match-data
      (save-excursion (outline-end-of-heading)
		      (setq folded (outline-invisible-p)))
      (condition-case nil
	  (org-forward-same-level (1- n) t)
	(error nil))
      (org-end-of-subtree t t))
    (org-back-over-empty-lines)
    (setq end (point))
    (goto-char beg0)
    (when (> end beg)
      (setq org-subtree-clip-folded folded)
      (when (or cut force-store-markers)
	(org-save-markers-in-region beg end))
      (if cut (kill-region beg end) (copy-region-as-kill beg end))
      (setq org-subtree-clip (current-kill 0))
      (message "%s: Subtree(s) with %d characters"
	       (if cut "Cut" "Copied")
	       (length org-subtree-clip)))))

(defun org-paste-subtree (&optional level tree for-yank)
  "Paste the clipboard as a subtree, with modification of headline level.
The entire subtree is promoted or demoted in order to match a new headline
level.

If the cursor is at the beginning of a headline, the same level as
that headline is used to paste the tree

If not, the new level is derived from the *visible* headings
before and after the insertion point, and taken to be the inferior headline
level of the two.  So if the previous visible heading is level 3 and the
next is level 4 (or vice versa), level 4 will be used for insertion.
This makes sure that the subtree remains an independent subtree and does
not swallow low level entries.

You can also force a different level, either by using a numeric prefix
argument, or by inserting the heading marker by hand.  For example, if the
cursor is after \"*****\", then the tree will be shifted to level 5.

If optional TREE is given, use this text instead of the kill ring.

When FOR-YANK is set, this is called by `org-yank'.  In this case, do not
move back over whitespace before inserting, and move point to the end of
the inserted text when done."
  (interactive "P")
  (setq tree (or tree (and kill-ring (current-kill 0))))
  (unless (org-kill-is-subtree-p tree)
    (error "%s"
     (substitute-command-keys
      "The kill is not a (set of) tree(s) - please use \\[yank] to yank anyway")))
  (org-with-limited-levels
   (let* ((visp (not (outline-invisible-p)))
	  (txt tree)
	  (^re_ "\\(\\*+\\)[  \t]*")
	  (old-level (if (string-match org-outline-regexp-bol txt)
			 (- (match-end 0) (match-beginning 0) 1)
		       -1))
	  (force-level (cond (level (prefix-numeric-value level))
			     ((and (looking-at "[ \t]*$")
				   (string-match
				    "^\\*+$" (buffer-substring
					      (point-at-bol) (point))))
			      (- (match-end 1) (match-beginning 1)))
			     ((and (bolp)
				   (looking-at org-outline-regexp))
			      (- (match-end 0) (point) 1))
			     (t nil)))
	  (previous-level (save-excursion
			    (condition-case nil
				(progn
				  (outline-previous-visible-heading 1)
				  (if (looking-at ^re_)
				      (- (match-end 0) (match-beginning 0) 1)
				    1))
			      (error 1))))
	  (next-level (save-excursion
			(condition-case nil
			    (progn
			      (or (looking-at org-outline-regexp)
				  (outline-next-visible-heading 1))
			      (if (looking-at ^re_)
				  (- (match-end 0) (match-beginning 0) 1)
				1))
			  (error 1))))
	  (new-level (or force-level (max previous-level next-level)))
	  (shift (if (or (= old-level -1)
			 (= new-level -1)
			 (= old-level new-level))
		     0
		   (- new-level old-level)))
	  (delta (if (> shift 0) -1 1))
	  (func (if (> shift 0) 'org-demote 'org-promote))
	  (org-odd-levels-only nil)
	  beg end newend)
     ;; Remove the forced level indicator
     (if force-level
	 (delete-region (point-at-bol) (point)))
     ;; Paste
     (beginning-of-line (if (bolp) 1 2))
     (unless for-yank (org-back-over-empty-lines))
     (setq beg (point))
     (and (fboundp 'org-id-paste-tracker) (org-id-paste-tracker txt))
     (insert-before-markers txt)
     (unless (string-match "\n\\'" txt) (insert "\n"))
     (setq newend (point))
     (org-reinstall-markers-in-region beg)
     (setq end (point))
     (goto-char beg)
     (skip-chars-forward " \t\n\r")
     (setq beg (point))
     (if (and (outline-invisible-p) visp)
	 (save-excursion (outline-show-heading)))
     ;; Shift if necessary
     (unless (= shift 0)
       (save-restriction
	 (narrow-to-region beg end)
	 (while (not (= shift 0))
	   (org-map-region func (point-min) (point-max))
	   (setq shift (+ delta shift)))
	 (goto-char (point-min))
	 (setq newend (point-max))))
     (when (or (org-called-interactively-p 'interactive) for-yank)
       (message "Clipboard pasted as level %d subtree" new-level))
     (if (and (not for-yank) ; in this case, org-yank will decide about folding
	      kill-ring
	      (eq org-subtree-clip (current-kill 0))
	      org-subtree-clip-folded)
	 ;; The tree was folded before it was killed/copied
	 (hide-subtree))
     (and for-yank (goto-char newend)))))

(defun org-kill-is-subtree-p (&optional txt)
  "Check if the current kill is an outline subtree, or a set of trees.
Returns nil if kill does not start with a headline, or if the first
headline level is not the largest headline level in the tree.
So this will actually accept several entries of equal levels as well,
which is OK for `org-paste-subtree'.
If optional TXT is given, check this string instead of the current kill."
  (let* ((kill (or txt (and kill-ring (current-kill 0)) ""))
	 (re (org-get-limited-outline-regexp))
	 (^re (concat "^" re))
	 (start-level (and kill
			   (string-match
			    (concat "\\`\\([ \t\n\r]*?\n\\)?\\(" re "\\)")
			    kill)
			   (- (match-end 2) (match-beginning 2) 1)))
	 (start (1+ (or (match-beginning 2) -1))))
    (if (not start-level)
	(progn
	  nil)  ;; does not even start with a heading
      (catch 'exit
	(while (setq start (string-match ^re kill (1+ start)))
	  (when (< (- (match-end 0) (match-beginning 0) 1) start-level)
	    (throw 'exit nil)))
	t))))

(defvar org-markers-to-move nil
  "Markers that should be moved with a cut-and-paste operation.
Those markers are stored together with their positions relative to
the start of the region.")

(defun org-save-markers-in-region (beg end)
  "Check markers in region.
If these markers are between BEG and END, record their position relative
to BEG, so that after moving the block of text, we can put the markers back
into place.
This function gets called just before an entry or tree gets cut from the
buffer.  After re-insertion, `org-reinstall-markers-in-region' must be
called immediately, to move the markers with the entries."
  (setq org-markers-to-move nil)
  (when (featurep 'org-clock)
    (org-clock-save-markers-for-cut-and-paste beg end))
  (when (featurep 'org-agenda)
    (org-agenda-save-markers-for-cut-and-paste beg end)))

(defun org-check-and-save-marker (marker beg end)
  "Check if MARKER is between BEG and END.
If yes, remember the marker and the distance to BEG."
  (when (and (marker-buffer marker)
	     (equal (marker-buffer marker) (current-buffer)))
    (if (and (>= marker beg) (< marker end))
	(push (cons marker (- marker beg)) org-markers-to-move))))

(defun org-reinstall-markers-in-region (beg)
  "Move all remembered markers to their position relative to BEG."
  (mapc (lambda (x)
	  (move-marker (car x) (+ beg (cdr x))))
	org-markers-to-move)
  (setq org-markers-to-move nil))

(defun org-narrow-to-subtree ()
  "Narrow buffer to the current subtree."
  (interactive)
  (save-excursion
    (save-match-data
      (org-with-limited-levels
       (narrow-to-region
	(progn (org-back-to-heading t) (point))
	(progn (org-end-of-subtree t t)
	       (if (and (org-at-heading-p) (not (eobp))) (backward-char 1))
	       (point)))))))

(defun org-narrow-to-block ()
  "Narrow buffer to the current block."
  (interactive)
  (let* ((case-fold-search t)
	 (blockp (org-between-regexps-p "^[ \t]*#\\+begin_.*"
					 "^[ \t]*#\\+end_.*")))
    (if blockp
	(narrow-to-region (car blockp) (cdr blockp))
      (error "Not in a block"))))

(eval-when-compile
  (defvar org-property-drawer-re))

(defvar org-property-start-re)  ;; defined below
(defun org-clone-subtree-with-time-shift (n &optional shift)
  "Clone the task (subtree) at point N times.
The clones will be inserted as siblings.

In interactive use, the user will be prompted for the number of
clones to be produced, and for a time SHIFT, which may be a
repeater as used in time stamps, for example `+3d'.

When a valid repeater is given and the entry contains any time
stamps, the clones will become a sequence in time, with time
stamps in the subtree shifted for each clone produced.  If SHIFT
is nil or the empty string, time stamps will be left alone.  The
ID property of the original subtree is removed.

If the original subtree did contain time stamps with a repeater,
the following will happen:
- the repeater will be removed in each clone
- an additional clone will be produced, with the current, unshifted
  date(s) in the entry.
- the original entry will be placed *after* all the clones, with
  repeater intact.
- the start days in the repeater in the original entry will be shifted
  to past the last clone.
In this way you can spell out a number of instances of a repeating task,
and still retain the repeater to cover future instances of the task."
  (interactive "nNumber of clones to produce: \nsDate shift per clone (e.g. +1w, empty to copy unchanged): ")
  (let (beg end template task idprop
	    shift-n shift-what doshift nmin nmax (n-no-remove -1)
	    (drawer-re org-drawer-regexp))
    (if (not (and (integerp n) (> n 0)))
	(error "Invalid number of replications %s" n))
    (if (and (setq doshift (and (stringp shift) (string-match "\\S-" shift)))
	     (not (string-match "\\`[ \t]*\\+?\\([0-9]+\\)\\([dwmy]\\)[ \t]*\\'"
				shift)))
	(error "Invalid shift specification %s" shift))
    (when doshift
      (setq shift-n (string-to-number (match-string 1 shift))
	    shift-what (cdr (assoc (match-string 2 shift)
				   '(("d" . day) ("w" . week)
				     ("m" . month) ("y" . year))))))
    (if (eq shift-what 'week) (setq shift-n (* 7 shift-n) shift-what 'day))
    (setq nmin 1 nmax n)
    (org-back-to-heading t)
    (setq beg (point))
    (setq idprop (org-entry-get nil "ID"))
    (org-end-of-subtree t t)
    (or (bolp) (insert "\n"))
    (setq end (point))
    (setq template (buffer-substring beg end))
    (when (and doshift
	       (string-match "<[^<>\n]+ [.+]?\\+[0-9]+[dwmy][^<>\n]*>" template))
      (delete-region beg end)
      (setq end beg)
      (setq nmin 0 nmax (1+ nmax) n-no-remove nmax))
    (goto-char end)
    (loop for n from nmin to nmax do
	  ;; prepare clone
	  (with-temp-buffer
	    (insert template)
	    (org-mode)
	    (goto-char (point-min))
	    (org-show-subtree)
	    (and idprop (if org-clone-delete-id
			    (org-entry-delete nil "ID")
			  (org-id-get-create t)))
	    (unless (= n 0)
	      (while (re-search-forward "^[ \t]*CLOCK:.*$" nil t)
		(kill-whole-line))
	      (goto-char (point-min))
	      (while (re-search-forward drawer-re nil t)
		(mapc (lambda (d)
			(org-remove-empty-drawer-at d (point))) org-drawers)))
	    (goto-char (point-min))
	    (when doshift
	      (while (re-search-forward org-ts-regexp-both nil t)
		(org-timestamp-change (* n shift-n) shift-what))
	      (unless (= n n-no-remove)
		(goto-char (point-min))
		(while (re-search-forward org-ts-regexp nil t)
		  (save-excursion
		    (goto-char (match-beginning 0))
		    (if (looking-at "<[^<>\n]+\\( +[.+]?\\+[0-9]+[dwmy]\\)")
			(delete-region (match-beginning 1) (match-end 1)))))))
	    (setq task (buffer-string)))
	  (insert task))
    (goto-char beg)))

;;; Outline Sorting

(defun org-sort (with-case)
  "Call `org-sort-entries', `org-table-sort-lines' or `org-sort-list'.
Optional argument WITH-CASE means sort case-sensitively."
  (interactive "P")
  (cond
   ((org-at-table-p) (org-call-with-arg 'org-table-sort-lines with-case))
   ((org-at-item-p) (org-call-with-arg 'org-sort-list with-case))
   (t
    (org-call-with-arg 'org-sort-entries with-case))))

(defun org-sort-remove-invisible (s)
  (remove-text-properties 0 (length s) org-rm-props s)
  (while (string-match org-bracket-link-regexp s)
    (setq s (replace-match (if (match-end 2)
			       (match-string 3 s)
			     (match-string 1 s)) t t s)))
  s)

(defvar org-priority-regexp) ; defined later in the file

(defvar org-after-sorting-entries-or-items-hook nil
  "Hook that is run after a bunch of entries or items have been sorted.
When children are sorted, the cursor is in the parent line when this
hook gets called.  When a region or a plain list is sorted, the cursor
will be in the first entry of the sorted region/list.")

(defun org-sort-entries
  (&optional with-case sorting-type getkey-func compare-func property)
  "Sort entries on a certain level of an outline tree.
If there is an active region, the entries in the region are sorted.
Else, if the cursor is before the first entry, sort the top-level items.
Else, the children of the entry at point are sorted.

Sorting can be alphabetically, numerically, by date/time as given by
a time stamp, by a property or by priority.

The command prompts for the sorting type unless it has been given to the
function through the SORTING-TYPE argument, which needs to be a character,
\(?n ?N ?a ?A ?t ?T ?s ?S ?d ?D ?p ?P ?r ?R ?f ?F).  Here is the
precise meaning of each character:

n   Numerically, by converting the beginning of the entry/item to a number.
a   Alphabetically, ignoring the TODO keyword and the priority, if any.
t   By date/time, either the first active time stamp in the entry, or, if
    none exist, by the first inactive one.
s   By the scheduled date/time.
d   By deadline date/time.
c   By creation time, which is assumed to be the first inactive time stamp
    at the beginning of a line.
p   By priority according to the cookie.
r   By the value of a property.

Capital letters will reverse the sort order.

If the SORTING-TYPE is ?f or ?F, then GETKEY-FUNC specifies a function to be
called with point at the beginning of the record.  It must return either
a string or a number that should serve as the sorting key for that record.

Comparing entries ignores case by default.  However, with an optional argument
WITH-CASE, the sorting considers case as well."
  (interactive "P")
  (let ((case-func (if with-case 'identity 'downcase))
        start beg end stars re re2
        txt what tmp)
    ;; Find beginning and end of region to sort
    (cond
     ((org-region-active-p)
      ;; we will sort the region
      (setq end (region-end)
            what "region")
      (goto-char (region-beginning))
      (if (not (org-at-heading-p)) (outline-next-heading))
      (setq start (point)))
     ((or (org-at-heading-p)
          (condition-case nil (progn (org-back-to-heading) t) (error nil)))
      ;; we will sort the children of the current headline
      (org-back-to-heading)
      (setq start (point)
	    end (progn (org-end-of-subtree t t)
		       (or (bolp) (insert "\n"))
		       (org-back-over-empty-lines)
		       (point))
	    what "children")
      (goto-char start)
      (show-subtree)
      (outline-next-heading))
     (t
      ;; we will sort the top-level entries in this file
      (goto-char (point-min))
      (or (org-at-heading-p) (outline-next-heading))
      (setq start (point))
      (goto-char (point-max))
      (beginning-of-line 1)
      (when (looking-at ".*?\\S-")
	;; File ends in a non-white line
	(end-of-line 1)
	(insert "\n"))
      (setq end (point-max))
      (setq what "top-level")
      (goto-char start)
      (show-all)))

    (setq beg (point))
    (if (>= beg end) (error "Nothing to sort"))

    (looking-at "\\(\\*+\\)")
    (setq stars (match-string 1)
	  re (concat "^" (regexp-quote stars) " +")
	  re2 (concat "^" (regexp-quote (substring stars 0 -1)) "[ \t\n]")
	  txt (buffer-substring beg end))
    (if (not (equal (substring txt -1) "\n")) (setq txt (concat txt "\n")))
    (if (and (not (equal stars "*")) (string-match re2 txt))
	(error "Region to sort contains a level above the first entry"))

    (unless sorting-type
      (message
       "Sort %s: [a]lpha  [n]umeric  [p]riority  p[r]operty  todo[o]rder  [f]unc
               [t]ime [s]cheduled  [d]eadline  [c]reated
               A/N/T/S/D/C/P/O/F means reversed:"
       what)
      (setq sorting-type (read-char-exclusive))

      (and (= (downcase sorting-type) ?f)
           (setq getkey-func
                 (org-icompleting-read "Sort using function: "
				       obarray 'fboundp t nil nil))
           (setq getkey-func (intern getkey-func)))

      (and (= (downcase sorting-type) ?r)
           (setq property
                 (org-icompleting-read "Property: "
				       (mapcar 'list (org-buffer-property-keys t))
				       nil t))))

    (message "Sorting entries...")

    (save-restriction
      (narrow-to-region start end)
      (let ((dcst (downcase sorting-type))
	    (case-fold-search nil)
            (now (current-time)))
        (sort-subr
         (/= dcst sorting-type)
         ;; This function moves to the beginning character of the "record" to
         ;; be sorted.
	 (lambda nil
	   (if (re-search-forward re nil t)
	       (goto-char (match-beginning 0))
	     (goto-char (point-max))))
         ;; This function moves to the last character of the "record" being
         ;; sorted.
	 (lambda nil
	   (save-match-data
	     (condition-case nil
		 (outline-forward-same-level 1)
	       (error
		(goto-char (point-max))))))
         ;; This function returns the value that gets sorted against.
	 (lambda nil
	   (cond
	    ((= dcst ?n)
	     (if (looking-at org-complex-heading-regexp)
		 (string-to-number (match-string 4))
	       nil))
	    ((= dcst ?a)
	     (if (looking-at org-complex-heading-regexp)
		 (funcall case-func (match-string 4))
	       nil))
	    ((= dcst ?t)
	     (let ((end (save-excursion (outline-next-heading) (point))))
	       (if (or (re-search-forward org-ts-regexp end t)
		       (re-search-forward org-ts-regexp-both end t))
		   (org-time-string-to-seconds (match-string 0))
		 (org-float-time now))))
	    ((= dcst ?c)
	     (let ((end (save-excursion (outline-next-heading) (point))))
	       (if (re-search-forward
		    (concat "^[ \t]*\\[" org-ts-regexp1 "\\]")
		    end t)
		   (org-time-string-to-seconds (match-string 0))
		 (org-float-time now))))
	    ((= dcst ?s)
	     (let ((end (save-excursion (outline-next-heading) (point))))
	       (if (re-search-forward org-scheduled-time-regexp end t)
		   (org-time-string-to-seconds (match-string 1))
		 (org-float-time now))))
	    ((= dcst ?d)
	     (let ((end (save-excursion (outline-next-heading) (point))))
	       (if (re-search-forward org-deadline-time-regexp end t)
		   (org-time-string-to-seconds (match-string 1))
		 (org-float-time now))))
	    ((= dcst ?p)
	     (if (re-search-forward org-priority-regexp (point-at-eol) t)
		 (string-to-char (match-string 2))
	       org-default-priority))
	    ((= dcst ?r)
	     (or (org-entry-get nil property) ""))
	    ((= dcst ?o)
	     (if (looking-at org-complex-heading-regexp)
		 (- 9999 (length (member (match-string 2)
					 org-todo-keywords-1)))))
	    ((= dcst ?f)
	     (if getkey-func
		 (progn
		   (setq tmp (funcall getkey-func))
		   (if (stringp tmp) (setq tmp (funcall case-func tmp)))
		   tmp)
	       (error "Invalid key function `%s'" getkey-func)))
	    (t (error "Invalid sorting type `%c'" sorting-type))))
         nil
         (cond
          ((= dcst ?a) 'string<)
          ((= dcst ?f) compare-func)
          ((member dcst '(?p ?t ?s ?d ?c)) '<)
          (t nil)))))
    (run-hooks 'org-after-sorting-entries-or-items-hook)
    (message "Sorting entries...done")))

(defun org-do-sort (table what &optional with-case sorting-type)
  "Sort TABLE of WHAT according to SORTING-TYPE.
The user will be prompted for the SORTING-TYPE if the call to this
function does not specify it.  WHAT is only for the prompt, to indicate
what is being sorted.  The sorting key will be extracted from
the car of the elements of the table.
If WITH-CASE is non-nil, the sorting will be case-sensitive."
  (unless sorting-type
    (message
     "Sort %s: [a]lphabetic. [n]umeric. [t]ime.  A/N/T means reversed:"
     what)
    (setq sorting-type (read-char-exclusive)))
  (let ((dcst (downcase sorting-type))
	extractfun comparefun)
    ;; Define the appropriate functions
    (cond
     ((= dcst ?n)
      (setq extractfun 'string-to-number
	    comparefun (if (= dcst sorting-type) '< '>)))
     ((= dcst ?a)
      (setq extractfun (if with-case (lambda(x) (org-sort-remove-invisible x))
			 (lambda(x) (downcase (org-sort-remove-invisible x))))
	    comparefun (if (= dcst sorting-type)
			   'string<
			 (lambda (a b) (and (not (string< a b))
					    (not (string= a b)))))))
     ((= dcst ?t)
      (setq extractfun
	    (lambda (x)
	      (if (or (string-match org-ts-regexp x)
		      (string-match org-ts-regexp-both x))
		  (org-float-time
		   (org-time-string-to-time (match-string 0 x)))
		0))
	    comparefun (if (= dcst sorting-type) '< '>)))
     (t (error "Invalid sorting type `%c'" sorting-type)))

    (sort (mapcar (lambda (x) (cons (funcall extractfun (car x)) (cdr x)))
		  table)
	  (lambda (a b) (funcall comparefun (car a) (car b))))))


;;; The orgstruct minor mode

;; Define a minor mode which can be used in other modes in order to
;; integrate the org-mode structure editing commands.

;; This is really a hack, because the org-mode structure commands use
;; keys which normally belong to the major mode.  Here is how it
;; works: The minor mode defines all the keys necessary to operate the
;; structure commands, but wraps the commands into a function which
;; tests if the cursor is currently at a headline or a plain list
;; item.  If that is the case, the structure command is used,
;; temporarily setting many Org-mode variables like regular
;; expressions for filling etc.  However, when any of those keys is
;; used at a different location, function uses `key-binding' to look
;; up if the key has an associated command in another currently active
;; keymap (minor modes, major mode, global), and executes that
;; command.  There might be problems if any of the keys is otherwise
;; used as a prefix key.

;; Another challenge is that the key binding for TAB can be tab or \C-i,
;; likewise the binding for RET can be return or \C-m.  Orgtbl-mode
;; addresses this by checking explicitly for both bindings.

(defvar orgstruct-mode-map (make-sparse-keymap)
  "Keymap for the minor `orgstruct-mode'.")

(defvar org-local-vars nil
  "List of local variables, for use by `orgstruct-mode'.")

;;;###autoload
(define-minor-mode orgstruct-mode
  "Toggle the minor mode `orgstruct-mode'.
This mode is for using Org-mode structure commands in other
modes.  The following keys behave as if Org-mode were active, if
the cursor is on a headline, or on a plain list item (both as
defined by Org-mode).

M-up        Move entry/item up
M-down	    Move entry/item down
M-left	    Promote
M-right	    Demote
M-S-up	    Move entry/item up
M-S-down    Move entry/item down
M-S-left    Promote subtree
M-S-right   Demote subtree
M-q	    Fill paragraph and items like in Org-mode
C-c ^	    Sort entries
C-c -	    Cycle list bullet
TAB         Cycle item visibility
M-RET       Insert new heading/item
S-M-RET     Insert new TODO heading / Checkbox item
C-c C-c     Set tags / toggle checkbox"
  nil " OrgStruct" nil
  (org-load-modules-maybe)
  (and (orgstruct-setup) (defun orgstruct-setup () nil)))

;;;###autoload
(defun turn-on-orgstruct ()
  "Unconditionally turn on `orgstruct-mode'."
  (orgstruct-mode 1))

(defun orgstruct++-mode (&optional arg)
  "Toggle `orgstruct-mode', the enhanced version of it.
In addition to setting orgstruct-mode, this also exports all indentation
and autofilling variables from org-mode into the buffer.  It will also
recognize item context in multiline items.
Note that turning off orgstruct-mode will *not* remove the
indentation/paragraph settings.  This can only be done by refreshing the
major mode, for example with \\[normal-mode]."
  (interactive "P")
  (setq arg (prefix-numeric-value (or arg (if orgstruct-mode -1 1))))
  (if (< arg 1)
      (orgstruct-mode -1)
    (orgstruct-mode 1)
    (let (var val)
      (mapc
       (lambda (x)
	 (when (string-match
		"^\\(paragraph-\\|auto-fill\\|fill-paragraph\\|adaptive-fill\\|indent-\\)"
		(symbol-name (car x)))
	   (setq var (car x) val (nth 1 x))
	   (org-set-local var (if (eq (car-safe val) 'quote) (nth 1 val) val))))
       org-local-vars)
      (org-set-local 'orgstruct-is-++ t))))

(defvar orgstruct-is-++ nil
  "Is `orgstruct-mode' in ++ version in the current-buffer?")
(make-variable-buffer-local 'orgstruct-is-++)

;;;###autoload
(defun turn-on-orgstruct++ ()
  "Unconditionally turn on `orgstruct++-mode'."
  (orgstruct++-mode 1))

(defun orgstruct-error ()
  "Error when there is no default binding for a structure key."
  (interactive)
  (error "This key has no function outside structure elements"))

(defun orgstruct-setup ()
  "Setup orgstruct keymaps."
  (let ((nfunc 0)
	(bindings
	 (list
	  '([(meta up)]           org-metaup)
	  '([(meta down)]         org-metadown)
	  '([(meta left)]         org-metaleft)
	  '([(meta right)]        org-metaright)
	  '([(meta shift up)]     org-shiftmetaup)
	  '([(meta shift down)]   org-shiftmetadown)
	  '([(meta shift left)]   org-shiftmetaleft)
	  '([(meta shift right)]  org-shiftmetaright)
	  '([?\e (up)]            org-metaup)
	  '([?\e (down)]          org-metadown)
	  '([?\e (left)]          org-metaleft)
	  '([?\e (right)]         org-metaright)
	  '([?\e (shift up)]      org-shiftmetaup)
	  '([?\e (shift down)]    org-shiftmetadown)
	  '([?\e (shift left)]    org-shiftmetaleft)
	  '([?\e (shift right)]   org-shiftmetaright)
	  '([(shift up)]          org-shiftup)
	  '([(shift down)]        org-shiftdown)
	  '([(shift left)]        org-shiftleft)
	  '([(shift right)]       org-shiftright)
	  '("\C-c\C-c"            org-ctrl-c-ctrl-c)
	  '("\M-q"                fill-paragraph)
	  '("\C-c^"               org-sort)
	  '("\C-c-"               org-cycle-list-bullet)))
	elt key fun cmd)
    (while (setq elt (pop bindings))
      (setq nfunc (1+ nfunc))
      (setq key (org-key (car elt))
	    fun (nth 1 elt)
	    cmd (orgstruct-make-binding fun nfunc key))
      (org-defkey orgstruct-mode-map key cmd))

    ;; Special treatment needed for TAB and RET
    (org-defkey orgstruct-mode-map [(tab)]
		(orgstruct-make-binding 'org-cycle 102 [(tab)] "\C-i"))
    (org-defkey orgstruct-mode-map "\C-i"
		(orgstruct-make-binding 'org-cycle 103 "\C-i" [(tab)]))

    (org-defkey orgstruct-mode-map "\M-\C-m"
		(orgstruct-make-binding 'org-insert-heading 105
				     "\M-\C-m" [(meta return)]))
    (org-defkey orgstruct-mode-map [(meta return)]
		(orgstruct-make-binding 'org-insert-heading 106
				     [(meta return)] "\M-\C-m"))

    (org-defkey orgstruct-mode-map [(shift meta return)]
		(orgstruct-make-binding 'org-insert-todo-heading 107
				     [(meta return)] "\M-\C-m"))

    (org-defkey orgstruct-mode-map "\e\C-m"
		(orgstruct-make-binding 'org-insert-heading 108
				     "\e\C-m" [?\e (return)]))
    (org-defkey orgstruct-mode-map [?\e (return)]
		(orgstruct-make-binding 'org-insert-heading 109
				     [?\e (return)] "\e\C-m"))
    (org-defkey orgstruct-mode-map [?\e (shift return)]
		(orgstruct-make-binding 'org-insert-todo-heading 110
					[?\e (return)] "\e\C-m"))

    (unless org-local-vars
      (setq org-local-vars (org-get-local-variables)))

    t))

(defun orgstruct-make-binding (fun n &rest keys)
  "Create a function for binding in the structure minor mode.
FUN is the command to call inside a table.  N is used to create a unique
command name.  KEYS are keys that should be checked in for a command
to execute outside of tables."
  (eval
   (list 'defun
	 (intern (concat "orgstruct-hijacker-command-" (int-to-string n)))
	 '(arg)
	 (concat "In Structure, run `" (symbol-name fun) "'.\n"
		 "Outside of structure, run the binding of `"
		 (mapconcat (lambda (x) (format "%s" x)) keys "' or `")
		 "'.")
	 '(interactive "p")
	 (list 'if
	       `(org-context-p 'headline 'item
			       (and orgstruct-is-++
				    ,(and (memq fun '(org-insert-heading org-insert-todo-heading)) t)
				    'item-body))
	       (list 'org-run-like-in-org-mode (list 'quote fun))
	       (list 'let '(orgstruct-mode)
		     (list 'call-interactively
			   (append '(or)
				   (mapcar (lambda (k)
					     (list 'key-binding k))
					   keys)
				   '('orgstruct-error))))))))

(defun org-context-p (&rest contexts)
  "Check if local context is any of CONTEXTS.
Possible values in the list of contexts are `table', `headline', and `item'."
  (let ((pos (point)))
    (goto-char (point-at-bol))
    (prog1 (or (and (memq 'table contexts)
		    (looking-at "[ \t]*|"))
	       (and (memq 'headline contexts)
		    (looking-at org-outline-regexp))
	       (and (memq 'item contexts)
		    (looking-at "[ \t]*\\([-+*] \\|[0-9]+[.)] \\)"))
	       (and (memq 'item-body contexts)
		    (org-in-item-p)))
      (goto-char pos))))

(defun org-get-local-variables ()
  "Return a list of all local variables in an org-mode buffer."
  (let (varlist)
    (with-current-buffer (get-buffer-create "*Org tmp*")
      (erase-buffer)
      (org-mode)
      (setq varlist (buffer-local-variables)))
    (kill-buffer "*Org tmp*")
    (delq nil
	  (mapcar
	   (lambda (x)
	     (setq x
		   (if (symbolp x)
		       (list x)
		     (list (car x) (list 'quote (cdr x)))))
	     (if (string-match
		  "^\\(org-\\|orgtbl-\\|outline-\\|comment-\\|paragraph-\\|auto-fill\\|fill-paragraph\\|adaptive-fill\\|indent-\\)"
		  (symbol-name (car x)))
		 x nil))
	   varlist))))

(defun org-clone-local-variables (from-buffer &optional regexp)
  "Clone local variables from FROM-BUFFER.
Optional argument REGEXP selects variables to clone."
  (mapc
   (lambda (pair)
     (and (symbolp (car pair))
	  (or (null regexp)
	      (string-match regexp (symbol-name (car pair))))
	  (set (make-local-variable (car pair))
	       (cdr pair))))
   (buffer-local-variables from-buffer)))

;;;###autoload
(defun org-run-like-in-org-mode (cmd)
  "Run a command, pretending that the current buffer is in Org-mode.
This will temporarily bind local variables that are typically bound in
Org-mode to the values they have in Org-mode, and then interactively
call CMD."
  (org-load-modules-maybe)
  (unless org-local-vars
    (setq org-local-vars (org-get-local-variables)))
  (eval (list 'let org-local-vars
	      (list 'call-interactively (list 'quote cmd)))))

;;;; Archiving

(defun org-get-category (&optional pos force-refresh)
  "Get the category applying to position POS."
  (save-match-data
    (if force-refresh (org-refresh-category-properties))
    (let ((pos (or pos (point))))
      (or (get-text-property pos 'org-category)
	  (progn (org-refresh-category-properties)
		 (get-text-property pos 'org-category))))))

(defun org-refresh-category-properties ()
  "Refresh category text properties in the buffer."
  (let ((def-cat (cond
		  ((null org-category)
		   (if buffer-file-name
		       (file-name-sans-extension
			(file-name-nondirectory buffer-file-name))
		     "???"))
		  ((symbolp org-category) (symbol-name org-category))
		  (t org-category)))
	beg end cat pos optionp)
    (org-unmodified
     (save-excursion
       (save-restriction
	 (widen)
	 (goto-char (point-min))
	 (put-text-property (point) (point-max) 'org-category def-cat)
	 (while (re-search-forward
		 "^\\(#\\+CATEGORY:\\|[ \t]*:CATEGORY:\\)\\(.*\\)" nil t)
	   (setq pos (match-end 0)
		 optionp (equal (char-after (match-beginning 0)) ?#)
		 cat (org-trim (match-string 2)))
	   (if optionp
	       (setq beg (point-at-bol) end (point-max))
	     (org-back-to-heading t)
	     (setq beg (point) end (org-end-of-subtree t t)))
	   (put-text-property beg end 'org-category cat)
	   (put-text-property beg end 'org-category-position beg)
	   (goto-char pos)))))))


;;;; Link Stuff

;;; Link abbreviations

(defun org-link-expand-abbrev (link)
  "Apply replacements as defined in `org-link-abbrev-alist'."
  (if (string-match "^\\([^:]*\\)\\(::?\\(.*\\)\\)?$" link)
      (let* ((key (match-string 1 link))
	     (as (or (assoc key org-link-abbrev-alist-local)
		     (assoc key org-link-abbrev-alist)))
	     (tag (and (match-end 2) (match-string 3 link)))
	     rpl)
	(if (not as)
	    link
	  (setq rpl (cdr as))
	  (cond
	   ((symbolp rpl) (funcall rpl tag))
	   ((string-match "%s" rpl) (replace-match (or tag "") t t rpl))
	   ((string-match "%h" rpl)
	    (replace-match (url-hexify-string (or tag "")) t t rpl))
	   (t (concat rpl tag)))))
    link))

;;; Storing and inserting links

(defvar org-insert-link-history nil
  "Minibuffer history for links inserted with `org-insert-link'.")

(defvar org-stored-links nil
  "Contains the links stored with `org-store-link'.")

(defvar org-store-link-plist nil
  "Plist with info about the most recently link created with `org-store-link'.")

(defvar org-link-protocols nil
  "Link protocols added to Org-mode using `org-add-link-type'.")

(defvar org-store-link-functions nil
  "List of functions that are called to create and store a link.
Each function will be called in turn until one returns a non-nil
value.  Each function should check if it is responsible for creating
this link (for example by looking at the major mode).
If not, it must exit and return nil.
If yes, it should return a non-nil value after a calling
`org-store-link-props' with a list of properties and values.
Special properties are:

:type         The link prefix, like \"http\".  This must be given.
:link         The link, like \"http://www.astro.uva.nl/~dominik\".
              This is obligatory as well.
:description  Optional default description for the second pair
              of brackets in an Org-mode link.  The user can still change
              this when inserting this link into an Org-mode buffer.

In addition to these, any additional properties can be specified
and then used in remember templates.")

(defun org-add-link-type (type &optional follow export)
  "Add TYPE to the list of `org-link-types'.
Re-compute all regular expressions depending on `org-link-types'

FOLLOW and EXPORT are two functions.

FOLLOW should take the link path as the single argument and do whatever
is necessary to follow the link, for example find a file or display
a mail message.

EXPORT should format the link path for export to one of the export formats.
It should be a function accepting three arguments:

  path    the path of the link, the text after the prefix (like \"http:\")
  desc    the description of the link, if any, or a description added by
          org-export-normalize-links if there is none
  format  the export format, a symbol like `html' or `latex' or `ascii'..

The function may use the FORMAT information to return different values
depending on the format.  The return value will be put literally into
the exported file.  If the return value is nil, this means Org should
do what it normally does with links which do not have EXPORT defined.

Org-mode has a built-in default for exporting links.  If you are happy with
this default, there is no need to define an export function for the link
type.  For a simple example of an export function, see `org-bbdb.el'."
  (add-to-list 'org-link-types type t)
  (org-make-link-regexps)
  (if (assoc type org-link-protocols)
      (setcdr (assoc type org-link-protocols) (list follow export))
    (push (list type follow export) org-link-protocols)))

(defvar org-agenda-buffer-name)

;;;###autoload
(defun org-store-link (arg)
  "\\<org-mode-map>Store an org-link to the current location.
This link is added to `org-stored-links' and can later be inserted
into an org-buffer with \\[org-insert-link].

For some link types, a prefix arg is interpreted:
For links to usenet articles, arg negates `org-gnus-prefer-web-links'.
For file links, arg negates `org-context-in-file-links'."
  (interactive "P")
  (org-load-modules-maybe)
  (setq org-store-link-plist nil)  ; reset
  (org-with-limited-levels
   (let (link cpltxt desc description search txt custom-id agenda-link)
     (cond

      ((run-hook-with-args-until-success 'org-store-link-functions)
       (setq link (plist-get org-store-link-plist :link)
	     desc (or (plist-get org-store-link-plist :description) link)))

      ((org-src-edit-buffer-p)
       (let (label gc)
	 (while (or (not label)
		    (save-excursion
		      (save-restriction
			(widen)
			(goto-char (point-min))
			(re-search-forward
			 (regexp-quote (format org-coderef-label-format label))
			 nil t))))
	   (when label (message "Label exists already") (sit-for 2))
	   (setq label (read-string "Code line label: " label)))
	 (end-of-line 1)
	 (setq link (format org-coderef-label-format label))
	 (setq gc (- 79 (length link)))
	 (if (< (current-column) gc) (org-move-to-column gc t) (insert " "))
	 (insert link)
	 (setq link (concat "(" label ")") desc nil)))

      ((equal (org-bound-and-true-p org-agenda-buffer-name) (buffer-name))
       ;; We are in the agenda, link to referenced location
       (let ((m (or (get-text-property (point) 'org-hd-marker)
		    (get-text-property (point) 'org-marker))))
	 (when m
	   (org-with-point-at m
	     (setq agenda-link
		   (if (org-called-interactively-p 'any)
		       (call-interactively 'org-store-link)
		     (org-store-link nil)))))))

      ((eq major-mode 'calendar-mode)
       (let ((cd (calendar-cursor-to-date)))
	 (setq link
	       (format-time-string
		(car org-time-stamp-formats)
		(apply 'encode-time
		       (list 0 0 0 (nth 1 cd) (nth 0 cd) (nth 2 cd)
			     nil nil nil))))
	 (org-store-link-props :type "calendar" :date cd)))

      ((eq major-mode 'w3-mode)
       (setq cpltxt (if (and (buffer-name)
			     (not (string-match "Untitled" (buffer-name))))
			(buffer-name)
		      (url-view-url t))
	     link (org-make-link (url-view-url t)))
       (org-store-link-props :type "w3" :url (url-view-url t)))

      ((eq major-mode 'w3m-mode)
       (setq cpltxt (or w3m-current-title w3m-current-url)
	     link (org-make-link w3m-current-url))
       (org-store-link-props :type "w3m" :url (url-view-url t)))

      ((setq search (run-hook-with-args-until-success
		     'org-create-file-search-functions))
       (setq link (concat "file:" (abbreviate-file-name buffer-file-name)
			  "::" search))
       (setq cpltxt (or description link)))

      ((eq major-mode 'image-mode)
       (setq cpltxt (concat "file:"
			    (abbreviate-file-name buffer-file-name))
	     link (org-make-link cpltxt))
       (org-store-link-props :type "image" :file buffer-file-name))

      ((eq major-mode 'dired-mode)
       ;; link to the file in the current line
       (let ((file (dired-get-filename nil t)))
	 (setq file (if file
			(abbreviate-file-name
			 (expand-file-name (dired-get-filename nil t)))
		      ;; otherwise, no file so use current directory.
		      default-directory))
	 (setq cpltxt (concat "file:" file)
	       link (org-make-link cpltxt))))

      ((and (buffer-file-name (buffer-base-buffer)) (eq major-mode 'org-mode))
       (setq custom-id (org-entry-get nil "CUSTOM_ID"))
       (cond
	((org-in-regexp "<<\\(.*?\\)>>")
	 (setq cpltxt
	       (concat "file:"
		       (abbreviate-file-name
			(buffer-file-name (buffer-base-buffer)))
		       "::" (match-string 1))
	       link (org-make-link cpltxt)))
	((and (featurep 'org-id)
	      (or (eq org-link-to-org-use-id t)
		  (and (eq org-link-to-org-use-id 'create-if-interactive)
		       (org-called-interactively-p 'any))
		  (and (eq org-link-to-org-use-id
			   'create-if-interactive-and-no-custom-id)
		       (org-called-interactively-p 'any)
		       (not custom-id))
		  (and org-link-to-org-use-id
			   (org-entry-get nil "ID"))))
	 ;; We can make a link using the ID.
	 (setq link (condition-case nil
			(prog1 (org-id-store-link)
			  (setq desc (plist-get org-store-link-plist
						:description)))
		      (error
		       ;; probably before first headline, link to file only
		       (concat "file:"
			       (abbreviate-file-name
				(buffer-file-name (buffer-base-buffer))))))))
	(t
	 ;; Just link to current headline
	 (setq cpltxt (concat "file:"
			      (abbreviate-file-name
			       (buffer-file-name (buffer-base-buffer)))))
	 ;; Add a context search string
	 (when (org-xor org-context-in-file-links arg)
	   (setq txt (cond
		      ((org-at-heading-p) nil)
		      ((org-region-active-p)
		       (buffer-substring (region-beginning) (region-end)))
		      (t nil)))
	   (when (or (null txt) (string-match "\\S-" txt))
	     (setq cpltxt
		   (concat cpltxt "::"
			   (condition-case nil
			       (org-make-org-heading-search-string txt)
			     (error "")))
		   desc (or (nth 4 (ignore-errors
				     (org-heading-components))) "NONE"))))
	 (if (string-match "::\\'" cpltxt)
	     (setq cpltxt (substring cpltxt 0 -2)))
	 (setq link (org-make-link cpltxt)))))

      ((buffer-file-name (buffer-base-buffer))
       ;; Just link to this file here.
       (setq cpltxt (concat "file:"
			    (abbreviate-file-name
			     (buffer-file-name (buffer-base-buffer)))))
       ;; Add a context string
       (when (org-xor org-context-in-file-links arg)
	 (setq txt (if (org-region-active-p)
		       (buffer-substring (region-beginning) (region-end))
		     (buffer-substring (point-at-bol) (point-at-eol))))
	 ;; Only use search option if there is some text.
	 (when (string-match "\\S-" txt)
	   (setq cpltxt
		 (concat cpltxt "::" (org-make-org-heading-search-string txt))
		 desc "NONE")))
       (setq link (org-make-link cpltxt)))

      ((org-called-interactively-p 'interactive)
       (error "Cannot link to a buffer which is not visiting a file"))

      (t (setq link nil)))

     (if (consp link) (setq cpltxt (car link) link (cdr link)))
     (setq link (or link cpltxt)
	   desc (or desc cpltxt))
     (if (equal desc "NONE") (setq desc nil))

     (if (and (or (org-called-interactively-p 'any) executing-kbd-macro) link)
	 (progn
	   (setq org-stored-links
		 (cons (list link desc) org-stored-links))
	   (message "Stored: %s" (or desc link))
	   (when custom-id
	     (setq link (concat "file:" (abbreviate-file-name (buffer-file-name))
				"::#" custom-id))
	     (setq org-stored-links
		   (cons (list link desc) org-stored-links))))
       (or agenda-link (and link (org-make-link-string link desc)))))))

(defun org-store-link-props (&rest plist)
  "Store link properties, extract names and addresses."
  (let (x adr)
    (when (setq x (plist-get plist :from))
      (setq adr (mail-extract-address-components x))
      (setq plist (plist-put plist :fromname (car adr)))
      (setq plist (plist-put plist :fromaddress (nth 1 adr))))
    (when (setq x (plist-get plist :to))
      (setq adr (mail-extract-address-components x))
      (setq plist (plist-put plist :toname (car adr)))
      (setq plist (plist-put plist :toaddress (nth 1 adr)))))
  (let ((from (plist-get plist :from))
	(to (plist-get plist :to)))
    (when (and from to org-from-is-user-regexp)
      (setq plist
	    (plist-put plist :fromto
		       (if (string-match org-from-is-user-regexp from)
			   (concat "to %t")
			 (concat "from %f"))))))
  (setq org-store-link-plist plist))

(defun org-add-link-props (&rest plist)
  "Add these properties to the link property list."
  (let (key value)
    (while plist
      (setq key (pop plist) value (pop plist))
      (setq org-store-link-plist
	    (plist-put org-store-link-plist key value)))))

(defun org-email-link-description (&optional fmt)
  "Return the description part of an email link.
This takes information from `org-store-link-plist' and formats it
according to FMT (default from `org-email-link-description-format')."
  (setq fmt (or fmt org-email-link-description-format))
  (let* ((p org-store-link-plist)
	 (to (plist-get p :toaddress))
	 (from (plist-get p :fromaddress))
	 (table
	  (list
	   (cons "%c" (plist-get p :fromto))
	   (cons "%F" (plist-get p :from))
	   (cons "%f" (or (plist-get p :fromname) (plist-get p :fromaddress) "?"))
	   (cons "%T" (plist-get p :to))
	   (cons "%t" (or (plist-get p :toname) (plist-get p :toaddress) "?"))
	   (cons "%s" (plist-get p :subject))
	   (cons "%d" (plist-get p :date))
	   (cons "%m" (plist-get p :message-id)))))
    (when (string-match "%c" fmt)
      ;; Check if the user wrote this message
      (if (and org-from-is-user-regexp from to
	       (save-match-data (string-match org-from-is-user-regexp from)))
	  (setq fmt (replace-match "to %t" t t fmt))
	(setq fmt (replace-match "from %f" t t fmt))))
    (org-replace-escapes fmt table)))

(defun org-make-org-heading-search-string (&optional string heading)
  "Make search string for STRING or current headline."
  (interactive)
  (let ((s (or string (org-get-heading)))
	(lines org-context-in-file-links))
    (unless (and string (not heading))
      ;; We are using a headline, clean up garbage in there.
      (if (string-match org-todo-regexp s)
	  (setq s (replace-match "" t t s)))
      (if (string-match (org-re ":[[:alnum:]_@#%:]+:[ \t]*$") s)
	  (setq s (replace-match "" t t s)))
      (setq s (org-trim s))
      (if (string-match (concat "^\\(" org-quote-string "\\|"
				org-comment-string "\\)") s)
	  (setq s (replace-match "" t t s)))
      (while (string-match org-ts-regexp s)
	(setq s (replace-match "" t t s))))
    (or string (setq s (concat "*" s)))  ; Add * for headlines
    (when (and string (integerp lines) (> lines 0))
      (let ((slines (org-split-string s "\n")))
	(when (< lines (length slines))
	  (setq s (mapconcat
		   'identity
		   (reverse (nthcdr (- (length slines) lines)
				    (reverse slines))) "\n")))))
    (mapconcat 'identity (org-split-string s "[ \t]+") " ")))

(defun org-make-link (&rest strings)
  "Concatenate STRINGS."
  (apply 'concat strings))

(defun org-make-link-string (link &optional description)
  "Make a link with brackets, consisting of LINK and DESCRIPTION."
  (unless (string-match "\\S-" link)
    (error "Empty link"))
  (when (and description
	     (stringp description)
	     (not (string-match "\\S-" description)))
    (setq description nil))
  (when (stringp description)
    ;; Remove brackets from the description, they are fatal.
    (while (string-match "\\[" description)
      (setq description (replace-match "{" t t description)))
    (while (string-match "\\]" description)
      (setq description (replace-match "}" t t description))))
  (when (equal link description)
    ;; No description needed, it is identical
    (setq description nil))
  (when (and (not description)
	     (not (string-match (org-image-file-name-regexp) link))
	     (not (equal link (org-link-escape link))))
    (setq description (org-extract-attributes link)))
  (setq link
	(cond ((string-match (org-image-file-name-regexp) link) link)
	      ((string-match org-link-types-re link)
	       (concat (match-string 1 link)
		       (org-link-escape (substring link (match-end 1)))))
	      (t (org-link-escape link))))
  (concat "[[" link "]"
	  (if description (concat "[" description "]") "")
	  "]"))

(defconst org-link-escape-chars
  '(?\ ?\[ ?\] ?\; ?\= ?\+)
  "List of characters that should be escaped in link.
This is the list that is used for internal purposes.")

(defvar org-url-encoding-use-url-hexify nil)

(defconst org-link-escape-chars-browser
  '(?\ )
  "List of escapes for characters that are problematic in links.
This is the list that is used before handing over to the browser.")

(defun org-link-escape (text &optional table merge)
  "Return percent escaped representation of TEXT.
TEXT is a string with the text to escape.
Optional argument TABLE is a list with characters that should be
escaped.  When nil, `org-link-escape-chars' is used.
If optional argument MERGE is set, merge TABLE into
`org-link-escape-chars'."
  (if (and org-url-encoding-use-url-hexify (not table))
      (url-hexify-string text)
    (cond
     ((and table merge)
      (mapc (lambda (defchr)
	      (unless (member defchr table)
		(setq table (cons defchr table)))) org-link-escape-chars))
     ((null table)
      (setq table org-link-escape-chars)))
    (mapconcat
     (lambda (char)
       (if (or (member char table)
	       (< char 32) (= char 37) (> char 126))
	   (mapconcat (lambda (sequence-element)
			(format "%%%.2X" sequence-element))
		      (or (encode-coding-char char 'utf-8)
			  (error "Unable to percent escape character: %s"
				 (char-to-string char))) "")
	 (char-to-string char))) text "")))

(defun org-link-unescape (str)
  "Unhex hexified Unicode strings as returned from the JavaScript function
encodeURIComponent.  E.g. `%C3%B6' is the german Umlaut `ö'."
  (unless (and (null str) (string= "" str))
    (let ((pos 0) (case-fold-search t) unhexed)
      (while (setq pos (string-match "\\(%[0-9a-f][0-9a-f]\\)+" str pos))
	(setq unhexed (org-link-unescape-compound (match-string 0 str)))
	(setq str (replace-match unhexed t t str))
	(setq pos (+ pos (length unhexed))))))
  str)

(defun org-link-unescape-compound (hex)
  "Unhexify Unicode hex-chars.  E.g. `%C3%B6' is the German Umlaut `ö'.
Note: this function also decodes single byte encodings like
`%E1' (\"á\") if not followed by another `%[A-F0-9]{2}' group."
  (save-match-data
    (let* ((bytes (cdr (split-string hex "%")))
	   (ret "")
	   (eat 0)
	   (sum 0))
      (while bytes
	(let* ((val (string-to-number (pop bytes) 16))
	       (shift-xor
		(if (= 0 eat)
		    (cond
		     ((>= val 252) (cons 6 252))
		     ((>= val 248) (cons 5 248))
		     ((>= val 240) (cons 4 240))
		     ((>= val 224) (cons 3 224))
		     ((>= val 192) (cons 2 192))
		     (t (cons 0 0)))
		  (cons 6 128))))
	  (if (>= val 192) (setq eat (car shift-xor)))
	  (setq val (logxor val (cdr shift-xor)))
	  (setq sum (+ (lsh sum (car shift-xor)) val))
	  (if (> eat 0) (setq eat (- eat 1)))
	  (cond
	   ((= 0 eat)			;multi byte
	    (setq ret (concat ret (org-char-to-string sum)))
	    (setq sum 0))
	   ((not bytes)			; single byte(s)
	    (setq ret (org-link-unescape-single-byte-sequence hex))))
	  )) ;; end (while bytes
      ret )))

(defun org-link-unescape-single-byte-sequence (hex)
  "Unhexify hex-encoded single byte character sequences."
  (mapconcat (lambda (byte)
	       (char-to-string (string-to-number byte 16)))
	     (cdr (split-string hex "%")) ""))

(defun org-xor (a b)
  "Exclusive or."
  (if a (not b) b))

(defun org-fixup-message-id-for-http (s)
  "Replace special characters in a message id, so it can be used in an http query."
  (when (string-match "%" s)
    (setq s (mapconcat (lambda (c)
			 (if (eq c ?%)
			     "%25"
			   (char-to-string c)))
		       s "")))
  (while (string-match "<" s)
    (setq s (replace-match "%3C" t t s)))
  (while (string-match ">" s)
    (setq s (replace-match "%3E" t t s)))
  (while (string-match "@" s)
    (setq s (replace-match "%40" t t s)))
  s)

;;;###autoload
(defun org-insert-link-global ()
  "Insert a link like Org-mode does.
This command can be called in any mode to insert a link in Org-mode syntax."
  (interactive)
  (org-load-modules-maybe)
  (org-run-like-in-org-mode 'org-insert-link))

(defun org-insert-link (&optional complete-file link-location default-description)
  "Insert a link.  At the prompt, enter the link.

Completion can be used to insert any of the link protocol prefixes like
http or ftp in use.

The history can be used to select a link previously stored with
`org-store-link'.  When the empty string is entered (i.e. if you just
press RET at the prompt), the link defaults to the most recently
stored link.  As SPC triggers completion in the minibuffer, you need to
use M-SPC or C-q SPC to force the insertion of a space character.

You will also be prompted for a description, and if one is given, it will
be displayed in the buffer instead of the link.

If there is already a link at point, this command will allow you to edit link
and description parts.

With a \\[universal-argument] prefix, prompts for a file to link to.  The file name can
be selected using completion.  The path to the file will be relative to the
current directory if the file is in the current directory or a subdirectory.
Otherwise, the link will be the absolute path as completed in the minibuffer
\(i.e. normally ~/path/to/file).  You can configure this behavior using the
option `org-link-file-path-type'.

With two \\[universal-argument] prefixes, enforce an absolute path even if the file is in
the current directory or below.

With three \\[universal-argument] prefixes, negate the meaning of
`org-keep-stored-link-after-insertion'.

If `org-make-link-description-function' is non-nil, this function will be
called with the link target, and the result will be the default
link description.

If the LINK-LOCATION parameter is non-nil, this value will be
used as the link location instead of reading one interactively.

If the DEFAULT-DESCRIPTION parameter is non-nil, this value will
be used as the default description."
  (interactive "P")
  (let* ((wcf (current-window-configuration))
	 (region (if (org-region-active-p)
		     (buffer-substring (region-beginning) (region-end))))
	 (remove (and region (list (region-beginning) (region-end))))
	 (desc region)
	 tmphist ; byte-compile incorrectly complains about this
	 (link link-location)
	 entry file all-prefixes)
    (cond
     (link-location) ; specified by arg, just use it.
     ((org-in-regexp org-bracket-link-regexp 1)
      ;; We do have a link at point, and we are going to edit it.
      (setq remove (list (match-beginning 0) (match-end 0)))
      (setq desc (if (match-end 3) (org-match-string-no-properties 3)))
      (setq link (read-string "Link: "
			      (org-link-unescape
			       (org-match-string-no-properties 1)))))
     ((or (org-in-regexp org-angle-link-re)
	  (org-in-regexp org-plain-link-re))
      ;; Convert to bracket link
      (setq remove (list (match-beginning 0) (match-end 0))
	    link (read-string "Link: "
			      (org-remove-angle-brackets (match-string 0)))))
     ((member complete-file '((4) (16)))
      ;; Completing read for file names.
      (setq link (org-file-complete-link complete-file)))
     (t
      ;; Read link, with completion for stored links.
      (with-output-to-temp-buffer "*Org Links*"
	(princ "Insert a link.
Use TAB to complete link prefixes, then RET for type-specific completion support\n")
	(when org-stored-links
	  (princ "\nStored links are available with <up>/<down> or M-p/n (most recent with RET):\n\n")
	  (princ (mapconcat
		  (lambda (x)
		    (if (nth 1 x) (concat (car x) " (" (nth 1 x) ")") (car x)))
		  (reverse org-stored-links) "\n"))))
      (let ((cw (selected-window)))
	(select-window (get-buffer-window "*Org Links*" 'visible))
	(with-current-buffer "*Org Links*" (setq truncate-lines t))
	(unless (pos-visible-in-window-p (point-max))
	  (org-fit-window-to-buffer))
	(and (window-live-p cw) (select-window cw)))
      ;; Fake a link history, containing the stored links.
      (setq tmphist (append (mapcar 'car org-stored-links)
			    org-insert-link-history))
      (setq all-prefixes (append (mapcar 'car org-link-abbrev-alist-local)
				 (mapcar 'car org-link-abbrev-alist)
				 org-link-types))
      (unwind-protect
	  (progn
	    (setq link
		  (let ((org-completion-use-ido nil)
			(org-completion-use-iswitchb nil))
		    (org-completing-read
		     "Link: "
		     (append
		      (mapcar (lambda (x) (list (concat x ":")))
			      all-prefixes)
		      (mapcar 'car org-stored-links))
		     nil nil nil
		     'tmphist
		     (car (car org-stored-links)))))
	    (if (not (string-match "\\S-" link))
		(error "No link selected"))
	    (if (or (member link all-prefixes)
		    (and (equal ":" (substring link -1))
			 (member (substring link 0 -1) all-prefixes)
			 (setq link (substring link 0 -1))))
		(setq link (org-link-try-special-completion link))))
	(set-window-configuration wcf)
	(kill-buffer "*Org Links*"))
      (setq entry (assoc link org-stored-links))
      (or entry (push link org-insert-link-history))
      (if (funcall (if (equal complete-file '(64)) 'not 'identity)
		   (not org-keep-stored-link-after-insertion))
	  (setq org-stored-links (delq (assoc link org-stored-links)
				       org-stored-links)))
      (setq desc (or desc (nth 1 entry)))))

    (if (string-match org-plain-link-re link)
	;; URL-like link, normalize the use of angular brackets.
	(setq link (org-make-link (org-remove-angle-brackets link))))

    ;; Check if we are linking to the current file with a search option
    ;; If yes, simplify the link by using only the search option.
    (when (and buffer-file-name
	       (string-match "^file:\\(.+?\\)::\\([^>]+\\)" link))
      (let* ((path (match-string 1 link))
	     (case-fold-search nil)
	     (search (match-string 2 link)))
	(save-match-data
	  (if (equal (file-truename buffer-file-name) (file-truename path))
	      ;; We are linking to this same file, with a search option
	      (setq link search)))))

    ;; Check if we can/should use a relative path.  If yes, simplify the link
    (when (string-match "^\\(file:\\|docview:\\)\\(.*\\)" link)
      (let* ((type (match-string 1 link))
	     (path (match-string 2 link))
	     (origpath path)
	     (case-fold-search nil))
	(cond
	 ((or (eq org-link-file-path-type 'absolute)
	      (equal complete-file '(16)))
	  (setq path (abbreviate-file-name (expand-file-name path))))
	 ((eq org-link-file-path-type 'noabbrev)
	  (setq path (expand-file-name path)))
	 ((eq org-link-file-path-type 'relative)
	  (setq path (file-relative-name path)))
	 (t
	  (save-match-data
	    (if (string-match (concat "^" (regexp-quote
					   (expand-file-name
					    (file-name-as-directory
					     default-directory))))
			      (expand-file-name path))
		;; We are linking a file with relative path name.
		(setq path (substring (expand-file-name path)
				      (match-end 0)))
	      (setq path (abbreviate-file-name (expand-file-name path)))))))
	(setq link (concat type path))
	(if (equal desc origpath)
	    (setq desc path))))

    (if org-make-link-description-function
	(setq desc (funcall org-make-link-description-function link desc))
      (if default-description (setq desc default-description)))

    (setq desc (read-string "Description: " desc))
    (unless (string-match "\\S-" desc) (setq desc nil))
    (if remove (apply 'delete-region remove))
    (insert (org-make-link-string link desc))))

(defun org-link-try-special-completion (type)
  "If there is completion support for link type TYPE, offer it."
  (let ((fun (intern (concat "org-" type "-complete-link"))))
    (if (functionp fun)
	(funcall fun)
      (read-string "Link (no completion support): " (concat type ":")))))

(defun org-file-complete-link (&optional arg)
  "Create a file link using completion."
  (let (file link)
    (setq file (read-file-name "File: "))
    (let ((pwd (file-name-as-directory (expand-file-name ".")))
	  (pwd1 (file-name-as-directory (abbreviate-file-name
					 (expand-file-name ".")))))
      (cond
       ((equal arg '(16))
	(setq link (org-make-link
		    "file:"
		    (abbreviate-file-name (expand-file-name file)))))
       ((string-match (concat "^" (regexp-quote pwd1) "\\(.+\\)") file)
	(setq link  (org-make-link "file:" (match-string 1 file))))
       ((string-match (concat "^" (regexp-quote pwd) "\\(.+\\)")
		      (expand-file-name file))
	(setq link  (org-make-link
		     "file:" (match-string 1 (expand-file-name file)))))
       (t (setq link (org-make-link "file:" file)))))
    link))

(defun org-completing-read (&rest args)
  "Completing-read with SPACE being a normal character."
  (let ((enable-recursive-minibuffers t)
	(minibuffer-local-completion-map
	 (copy-keymap minibuffer-local-completion-map)))
    (org-defkey minibuffer-local-completion-map " " 'self-insert-command)
    (org-defkey minibuffer-local-completion-map "?" 'self-insert-command)
    (org-defkey minibuffer-local-completion-map (kbd "C-c !") 'org-time-stamp-inactive)
    (apply 'org-icompleting-read args)))

(defun org-completing-read-no-i (&rest args)
  (let (org-completion-use-ido org-completion-use-iswitchb)
    (apply 'org-completing-read args)))

(defun org-iswitchb-completing-read (prompt choices &rest args)
  "Use iswitch as a completing-read replacement to choose from choices.
PROMPT is a string to prompt with.  CHOICES is a list of strings to choose
from."
  (let* ((iswitchb-use-virtual-buffers nil)
	 (iswitchb-make-buflist-hook
	  (lambda ()
	    (setq iswitchb-temp-buflist choices))))
    (iswitchb-read-buffer prompt)))

(defun org-icompleting-read (&rest args)
  "Completing-read using `ido-mode' or `iswitchb' speedups if available."
  (org-without-partial-completion
   (if (and org-completion-use-ido
	    (fboundp 'ido-completing-read)
	    (boundp 'ido-mode) ido-mode
	    (listp (second args)))
       (let ((ido-enter-matching-directory nil))
	 (apply 'ido-completing-read (concat (car args))
		(if (consp (car (nth 1 args)))
		    (mapcar 'car (nth 1 args))
		  (nth 1 args))
		(cddr args)))
     (if (and org-completion-use-iswitchb
	      (boundp 'iswitchb-mode) iswitchb-mode
	      (listp (second args)))
	 (apply 'org-iswitchb-completing-read (concat (car args))
		(if (consp (car (nth 1 args)))
		    (mapcar 'car (nth 1 args))
		  (nth 1 args))
		(cddr args))
       (apply 'completing-read args)))))

(defun org-extract-attributes (s)
  "Extract the attributes cookie from a string and set as text property."
  (let (a attr (start 0) key value)
    (save-match-data
      (when (string-match "{{\\([^}]+\\)}}$" s)
	(setq a (match-string 1 s) s (substring s 0 (match-beginning 0)))
	(while (string-match "\\([a-zA-Z]+\\)=\"\\([^\"]*\\)\"" a start)
	  (setq key (match-string 1 a) value (match-string 2 a)
		start (match-end 0)
		attr (plist-put attr (intern key) value))))
      (org-add-props s nil 'org-attr attr))
    s))

(defun org-extract-attributes-from-string (tag)
  (let (key value attr)
    (while (string-match "\\([a-zA-Z]+\\)=\"\\([^\"]*\\)\"\\s-?" tag)
      (setq key (match-string 1 tag) value (match-string 2 tag)
	    tag (replace-match "" t t tag)
	    attr (plist-put attr (intern key) value)))
    (cons tag attr)))

(defun org-attributes-to-string (plist)
  "Format a property list into an HTML attribute list."
  (let ((s "") key value)
    (while plist
      (setq key (pop plist) value (pop plist))
      (and value
	   (setq s (concat s " " (symbol-name key) "=\"" value "\""))))
    s))

;;; Opening/following a link

(defvar org-link-search-failed nil)

(defvar org-open-link-functions nil
  "Hook for functions finding a plain text link.
These functions must take a single argument, the link content.
They will be called for links that look like [[link text][description]]
when LINK TEXT does not have a protocol like \"http:\" and does not look
like a filename (e.g. \"./blue.png\").

These functions will be called *before* Org attempts to resolve the
link by doing text searches in the current buffer - so if you want a
link \"[[target]]\" to still find \"<<target>>\", your function should
handle this as a special case.

When the function does handle the link, it must return a non-nil value.
If it decides that it is not responsible for this link, it must return
nil to indicate that that Org-mode can continue with other options
like exact and fuzzy text search.")

(defun org-next-link ()
  "Move forward to the next link.
If the link is in hidden text, expose it."
  (interactive)
  (when (and org-link-search-failed (eq this-command last-command))
    (goto-char (point-min))
    (message "Link search wrapped back to beginning of buffer"))
  (setq org-link-search-failed nil)
  (let* ((pos (point))
	 (ct (org-context))
	 (a (assoc :link ct)))
    (if a (goto-char (nth 2 a)))
    (if (re-search-forward org-any-link-re nil t)
	(progn
	  (goto-char (match-beginning 0))
	  (if (outline-invisible-p) (org-show-context)))
      (goto-char pos)
      (setq org-link-search-failed t)
      (error "No further link found"))))

(defun org-previous-link ()
  "Move backward to the previous link.
If the link is in hidden text, expose it."
  (interactive)
  (when (and org-link-search-failed (eq this-command last-command))
    (goto-char (point-max))
    (message "Link search wrapped back to end of buffer"))
  (setq org-link-search-failed nil)
  (let* ((pos (point))
	 (ct (org-context))
	 (a (assoc :link ct)))
    (if a (goto-char (nth 1 a)))
    (if (re-search-backward org-any-link-re nil t)
	(progn
	  (goto-char (match-beginning 0))
	  (if (outline-invisible-p) (org-show-context)))
      (goto-char pos)
      (setq org-link-search-failed t)
      (error "No further link found"))))

(defun org-translate-link (s)
  "Translate a link string if a translation function has been defined."
  (if (and org-link-translation-function
	   (fboundp org-link-translation-function)
	   (string-match "\\([a-zA-Z0-9]+\\):\\(.*\\)" s))
      (progn
	(setq s (funcall org-link-translation-function
			 (match-string 1 s) (match-string 2 s)))
	(concat (car s) ":" (cdr s)))
    s))

(defun org-translate-link-from-planner (type path)
  "Translate a link from Emacs Planner syntax so that Org can follow it.
This is still an experimental function, your mileage may vary."
 (cond
  ((member type '("http" "https" "news" "ftp"))
   ;; standard Internet links are the same.
   nil)
  ((and (equal type "irc") (string-match "^//" path))
   ;; Planner has two / at the beginning of an irc link, we have 1.
   ;; We should have zero, actually....
   (setq path (substring path 1)))
  ((and (equal type "lisp") (string-match "^/" path))
   ;; Planner has a slash, we do not.
   (setq type "elisp" path (substring path 1)))
  ((string-match "^//\\(.?*\\)/\\(<.*>\\)$" path)
   ;; A typical message link.  Planner has the id after the final slash,
   ;; we separate it with a hash mark
   (setq path (concat (match-string 1 path) "#"
		      (org-remove-angle-brackets (match-string 2 path)))))
  )
 (cons type path))

(defun org-find-file-at-mouse (ev)
  "Open file link or URL at mouse."
  (interactive "e")
  (mouse-set-point ev)
  (org-open-at-point 'in-emacs))

(defun org-open-at-mouse (ev)
  "Open file link or URL at mouse.
See the docstring of `org-open-file' for details."
  (interactive "e")
  (mouse-set-point ev)
  (if (eq major-mode 'org-agenda-mode)
      (org-agenda-copy-local-variable 'org-link-abbrev-alist-local))
  (org-open-at-point))

(defvar org-window-config-before-follow-link nil
  "The window configuration before following a link.
This is saved in case the need arises to restore it.")

(defvar org-open-link-marker (make-marker)
  "Marker pointing to the location where `org-open-at-point; was called.")

;;;###autoload
(defun org-open-at-point-global ()
  "Follow a link like Org-mode does.
This command can be called in any mode to follow a link that has
Org-mode syntax."
  (interactive)
  (org-run-like-in-org-mode 'org-open-at-point))

;;;###autoload
(defun org-open-link-from-string (s &optional arg reference-buffer)
  "Open a link in the string S, as if it was in Org-mode."
  (interactive "sLink: \nP")
  (let ((reference-buffer (or reference-buffer (current-buffer))))
    (with-temp-buffer
      (let ((org-inhibit-startup (not reference-buffer)))
	(org-mode)
	(insert s)
	(goto-char (point-min))
	(when reference-buffer
	  (setq org-link-abbrev-alist-local
		(with-current-buffer reference-buffer
		  org-link-abbrev-alist-local)))
	(org-open-at-point arg reference-buffer)))))

(defvar org-open-at-point-functions nil
  "Hook that is run when following a link at point.

Functions in this hook must return t if they identify and follow
a link at point.  If they don't find anything interesting at point,
they must return nil.")

(defun org-open-at-point (&optional arg reference-buffer)
  "Open link at or after point.
If there is no link at point, this function will search forward up to
the end of the current line.
Normally, files will be opened by an appropriate application.  If the
optional prefix argument ARG is non-nil, Emacs will visit the file.
With a double prefix argument, try to open outside of Emacs, in the
application the system uses for this file type."
  (interactive "P")
  ;; if in a code block, then open the block's results
  (unless (call-interactively #'org-babel-open-src-block-result)
  (org-load-modules-maybe)
  (move-marker org-open-link-marker (point))
  (setq org-window-config-before-follow-link (current-window-configuration))
  (org-remove-occur-highlights nil nil t)
  (cond
   ((and (org-at-heading-p)
	 (not (org-in-regexp
	       (concat org-plain-link-re "\\|"
		       org-bracket-link-regexp "\\|"
		       org-angle-link-re "\\|"
		       "[ \t]:[^ \t\n]+:[ \t]*$")))
	 (not (get-text-property (point) 'org-linked-text)))
    (or (org-offer-links-in-entry arg)
	(progn (require 'org-attach) (org-attach-reveal 'if-exists))))
   ((run-hook-with-args-until-success 'org-open-at-point-functions))
   ((org-at-timestamp-p t) (org-follow-timestamp-link))
   ((and (or (org-footnote-at-reference-p) (org-footnote-at-definition-p))
	 (not (org-in-regexp org-bracket-link-regexp)))
    (org-footnote-action))
   (t
    (let (type path link line search (pos (point)))
      (catch 'match
	(save-excursion
	  (skip-chars-forward "^]\n\r")
	  (when (org-in-regexp org-bracket-link-regexp 1)
	    (setq link (org-extract-attributes
			(org-link-unescape (org-match-string-no-properties 1))))
	    (while (string-match " *\n *" link)
	      (setq link (replace-match " " t t link)))
	    (setq link (org-link-expand-abbrev link))
	    (cond
	     ((or (file-name-absolute-p link)
		  (string-match "^\\.\\.?/" link))
	      (setq type "file" path link))
	     ((string-match org-link-re-with-space3 link)
	      (setq type (match-string 1 link) path (match-string 2 link)))
	     (t (setq type "thisfile" path link)))
	    (throw 'match t)))

	(when (get-text-property (point) 'org-linked-text)
	  (setq type "thisfile"
		pos (if (get-text-property (1+ (point)) 'org-linked-text)
			(1+ (point)) (point))
		path (buffer-substring
		      (or (previous-single-property-change pos 'org-linked-text)
			  (point-min))
		      (or (next-single-property-change pos 'org-linked-text)
			  (point-max))))
	  (throw 'match t))

	(save-excursion
	  (when (or (org-in-regexp org-angle-link-re)
		    (org-in-regexp org-plain-link-re))
	    (setq type (match-string 1)
		  path (org-link-unescape (match-string 2)))
	    (throw 'match t)))
	(save-excursion
	  (when (org-in-regexp (org-re "\\(:[[:alnum:]_@#%:]+\\):[ \t]*$"))
	    (setq type "tags"
		  path (match-string 1))
	    (while (string-match ":" path)
	      (setq path (replace-match "+" t t path)))
	    (throw 'match t)))
	(when (org-in-regexp "<\\([^><\n]+\\)>")
	  (setq type "tree-match"
		path (match-string 1))
	  (throw 'match t)))
      (unless path
	(error "No link found"))

      ;; switch back to reference buffer
      ;; needed when if called in a temporary buffer through
      ;; org-open-link-from-string
      (with-current-buffer (or reference-buffer (current-buffer))

	;; Remove any trailing spaces in path
	(if (string-match " +\\'" path)
	    (setq path (replace-match "" t t path)))
	(if (and org-link-translation-function
		 (fboundp org-link-translation-function))
	    ;; Check if we need to translate the link
	    (let ((tmp (funcall org-link-translation-function type path)))
	      (setq type (car tmp) path (cdr tmp))))

	(cond

	 ((assoc type org-link-protocols)
	  (funcall (nth 1 (assoc type org-link-protocols)) path))

	 ((equal type "mailto")
	  (let ((cmd (car org-link-mailto-program))
		(args (cdr org-link-mailto-program)) args1
		(address path) (subject "") a)
	    (if (string-match "\\(.*\\)::\\(.*\\)" path)
		(setq address (match-string 1 path)
		      subject (org-link-escape (match-string 2 path))))
	    (while args
	      (cond
	       ((not (stringp (car args))) (push (pop args) args1))
	       (t (setq a (pop args))
		  (if (string-match "%a" a)
		      (setq a (replace-match address t t a)))
		  (if (string-match "%s" a)
		      (setq a (replace-match subject t t a)))
		  (push a args1))))
	    (apply cmd (nreverse args1))))

	 ((member type '("http" "https" "ftp" "news"))
	  (browse-url (concat type ":" (if (org-string-match-p "[[:nonascii:] ]" path)
					   (org-link-escape
					    path org-link-escape-chars-browser)
					 path))))

	 ((string= type "doi")
	  (browse-url (concat "http://dx.doi.org/" (if (org-string-match-p "[[:nonascii:] ]" path)
						       (org-link-escape
							path org-link-escape-chars-browser)
						     path))))

	 ((member type '("message"))
	  (browse-url (concat type ":" path)))

	 ((string= type "tags")
	  (org-tags-view arg path))

	 ((string= type "tree-match")
	  (org-occur (concat "\\[" (regexp-quote path) "\\]")))

	 ((string= type "file")
	  (if (string-match "::\\([0-9]+\\)\\'" path)
	      (setq line (string-to-number (match-string 1 path))
		    path (substring path 0 (match-beginning 0)))
	    (if (string-match "::\\(.+\\)\\'" path)
		(setq search (match-string 1 path)
		      path (substring path 0 (match-beginning 0)))))
	  (if (string-match "[*?{]" (file-name-nondirectory path))
	      (dired path)
	    (org-open-file path arg line search)))

	 ((string= type "shell")
	  (let ((cmd path))
	    (if (or (and (not (string= org-confirm-shell-link-not-regexp ""))
			 (string-match org-confirm-shell-link-not-regexp cmd))
		    (not org-confirm-shell-link-function)
		    (funcall org-confirm-shell-link-function
			     (format "Execute \"%s\" in shell? "
				     (org-add-props cmd nil
				       'face 'org-warning))))
		(progn
		  (message "Executing %s" cmd)
		  (shell-command cmd))
	      (error "Abort"))))

	 ((string= type "elisp")
	  (let ((cmd path))
	    (if (or (and (not (string= org-confirm-elisp-link-not-regexp ""))
			 (string-match org-confirm-elisp-link-not-regexp cmd))
		    (not org-confirm-elisp-link-function)
		    (funcall org-confirm-elisp-link-function
			     (format "Execute \"%s\" as elisp? "
				     (org-add-props cmd nil
				       'face 'org-warning))))
		(message "%s => %s" cmd
			 (if (equal (string-to-char cmd) ?\()
			     (eval (read cmd))
			   (call-interactively (read cmd))))
	      (error "Abort"))))

	 ((and (string= type "thisfile")
	       (run-hook-with-args-until-success
		'org-open-link-functions path)))

	 ((string= type "thisfile")
	  (if arg
	      (switch-to-buffer-other-window
	       (org-get-buffer-for-internal-link (current-buffer)))
	    (org-mark-ring-push))
	  (let ((cmd `(org-link-search
		       ,path
		       ,(cond ((equal arg '(4)) ''occur)
			      ((equal arg '(16)) ''org-occur)
			      (t nil))
		       ,pos)))
	    (condition-case nil (let ((org-link-search-inhibit-query t))
				  (eval cmd))
	      (error (progn (widen) (eval cmd))))))

	 (t
	  (browse-url-at-point)))))))
  (move-marker org-open-link-marker nil)
  (run-hook-with-args 'org-follow-link-hook)))

(defun org-offer-links-in-entry (&optional nth zero)
  "Offer links in the current entry and follow the selected link.
If there is only one link, follow it immediately as well.
If NTH is an integer, immediately pick the NTH link found.
If ZERO is a string, check also this string for a link, and if
there is one, offer it as link number zero."
  (let ((re (concat "\\(" org-bracket-link-regexp "\\)\\|"
		    "\\(" org-angle-link-re "\\)\\|"
		    "\\(" org-plain-link-re "\\)"))
	(cnt ?0)
	(in-emacs (if (integerp nth) nil nth))
	have-zero end links link c)
    (when (and (stringp zero) (string-match org-bracket-link-regexp zero))
      (push (match-string 0 zero) links)
      (setq cnt (1- cnt) have-zero t))
    (save-excursion
      (org-back-to-heading t)
      (setq end (save-excursion (outline-next-heading) (point)))
      (while (re-search-forward re end t)
	(push (match-string 0) links))
      (setq links (org-uniquify (reverse links))))

    (cond
     ((null links)
      (message "No links"))
     ((equal (length links) 1)
      (setq link (list (car links))))
     ((and (integerp nth) (>= (length links) (if have-zero (1+ nth) nth)))
      (setq link (list (nth (if have-zero nth (1- nth)) links))))
     (t ; we have to select a link
      (save-excursion
	(save-window-excursion
	  (delete-other-windows)
	  (with-output-to-temp-buffer "*Select Link*"
	    (mapc (lambda (l)
		    (if (not (string-match org-bracket-link-regexp l))
			(princ (format "[%c]  %s\n" (incf cnt)
				       (org-remove-angle-brackets l)))
		      (if (match-end 3)
			  (princ (format "[%c]  %s (%s)\n" (incf cnt)
					 (match-string 3 l) (match-string 1 l)))
			(princ (format "[%c]  %s\n" (incf cnt)
				       (match-string 1 l))))))
		  links))
	  (org-fit-window-to-buffer (get-buffer-window "*Select Link*"))
	  (message "Select link to open, RET to open all:")
	  (setq c (read-char-exclusive))
	  (and (get-buffer "*Select Link*") (kill-buffer "*Select Link*"))))
      (when (equal c ?q) (error "Abort"))
      (if (equal c ?\C-m)
	  (setq link links)
	(setq nth (- c ?0))
	(if have-zero (setq nth (1+ nth)))
	(unless (and (integerp nth) (>= (length links) nth))
	  (error "Invalid link selection"))
	(setq link (list (nth (1- nth) links))))))
    (if link
	(let ((buf (current-buffer)))
	  (dolist (l link)
	    (org-open-link-from-string l in-emacs buf))
	  t)
      nil)))

;; Add special file links that specify the way of opening

(org-add-link-type "file+sys" 'org-open-file-with-system)
(org-add-link-type "file+emacs" 'org-open-file-with-emacs)
(defun org-open-file-with-system (path)
  "Open file at PATH using the system way of opening it."
  (org-open-file path 'system))
(defun org-open-file-with-emacs (path)
  "Open file at PATH in Emacs."
  (org-open-file path 'emacs))
(defun org-remove-file-link-modifiers ()
  "Remove the file link modifiers in `file+sys:' and `file+emacs:' links."
  (goto-char (point-min))
  (while (re-search-forward "\\<file\\+\\(sys\\|emacs\\):" nil t)
    (org-if-unprotected
     (replace-match "file:" t t))))
(eval-after-load "org-exp"
  '(add-hook 'org-export-preprocess-before-normalizing-links-hook
	     'org-remove-file-link-modifiers))

;;;; Time estimates

(defun org-get-effort (&optional pom)
  "Get the effort estimate for the current entry."
  (org-entry-get pom org-effort-property))

;;; File search

(defvar org-create-file-search-functions nil
  "List of functions to construct the right search string for a file link.
These functions are called in turn with point at the location to
which the link should point.

A function in the hook should first test if it would like to
handle this file type, for example by checking the `major-mode'
or the file extension.  If it decides not to handle this file, it
should just return nil to give other functions a chance.  If it
does handle the file, it must return the search string to be used
when following the link.  The search string will be part of the
file link, given after a double colon, and `org-open-at-point'
will automatically search for it.  If special measures must be
taken to make the search successful, another function should be
added to the companion hook `org-execute-file-search-functions',
which see.

A function in this hook may also use `setq' to set the variable
`description' to provide a suggestion for the descriptive text to
be used for this link when it gets inserted into an Org-mode
buffer with \\[org-insert-link].")

(defvar org-execute-file-search-functions nil
  "List of functions to execute a file search triggered by a link.

Functions added to this hook must accept a single argument, the
search string that was part of the file link, the part after the
double colon.  The function must first check if it would like to
handle this search, for example by checking the `major-mode' or
the file extension.  If it decides not to handle this search, it
should just return nil to give other functions a chance.  If it
does handle the search, it must return a non-nil value to keep
other functions from trying.

Each function can access the current prefix argument through the
variable `current-prefix-argument'.  Note that a single prefix is
used to force opening a link in Emacs, so it may be good to only
use a numeric or double prefix to guide the search function.

In case this is needed, a function in this hook can also restore
the window configuration before `org-open-at-point' was called using:

    (set-window-configuration org-window-config-before-follow-link)")

(defvar org-link-search-inhibit-query nil) ;; dynamically scoped
(defun org-link-search (s &optional type avoid-pos stealth)
  "Search for a link search option.
If S is surrounded by forward slashes, it is interpreted as a
regular expression.  In org-mode files, this will create an `org-occur'
sparse tree.  In ordinary files, `occur' will be used to list matches.
If the current buffer is in `dired-mode', grep will be used to search
in all files.  If AVOID-POS is given, ignore matches near that position.

When optional argument STEALTH is non-nil, do not modify
visibility around point, thus ignoring
`org-show-hierarchy-above', `org-show-following-heading' and
`org-show-siblings' variables."
  (let ((case-fold-search t)
	(s0 (mapconcat 'identity (org-split-string s "[ \t\r\n]+") " "))
	(markers (concat "\\(?:" (mapconcat (lambda (x) (regexp-quote (car x)))
					    (append '(("") (" ") ("\t") ("\n"))
						    org-emphasis-alist)
					    "\\|") "\\)"))
	(pos (point))
	(pre nil) (post nil)
	words re0 re1 re2 re3 re4_ re4 re5 re2a re2a_ reall)
    (cond
     ;; First check if there are any special search functions
     ((run-hook-with-args-until-success 'org-execute-file-search-functions s))
     ;; Now try the builtin stuff
     ((and (equal (string-to-char s0) ?#)
	   (> (length s0) 1)
	   (save-excursion
	     (goto-char (point-min))
	     (and
	      (re-search-forward
	       (concat "^[ \t]*:CUSTOM_ID:[ \t]+" (regexp-quote (substring s0 1)) "[ \t]*$") nil t)
	      (setq type 'dedicated
		    pos (match-beginning 0))))
	   ;; There is an exact target for this
	   (goto-char pos)
	   (org-back-to-heading t)))
     ((save-excursion
	(goto-char (point-min))
	(and
	 (re-search-forward
	  (concat "<<" (regexp-quote s0) ">>") nil t)
	 (setq type 'dedicated
	       pos (match-beginning 0))))
      ;; There is an exact target for this
      (goto-char pos))
     ((and (string-match "^(\\(.*\\))$" s0)
	   (save-excursion
	     (goto-char (point-min))
	     (and
	      (re-search-forward
	       (concat "[^[]" (regexp-quote
			       (format org-coderef-label-format
				       (match-string 1 s0))))
	       nil t)
	      (setq type 'dedicated
		    pos (1+ (match-beginning 0))))))
      ;; There is a coderef target for this
      (goto-char pos))
     ((string-match "^/\\(.*\\)/$" s)
      ;; A regular expression
      (cond
       ((eq major-mode 'org-mode)
	(org-occur (match-string 1 s)))
       ;;((eq major-mode 'dired-mode)
       ;; (grep (concat "grep -n -e '" (match-string 1 s) "' *")))
       (t (org-do-occur (match-string 1 s)))))
     ((and (eq major-mode 'org-mode) org-link-search-must-match-exact-headline)
      (and (equal (string-to-char s) ?*) (setq s (substring s 1)))
      (goto-char (point-min))
      (cond
       ((let (case-fold-search)
	  (re-search-forward (format org-complex-heading-regexp-format
				     (regexp-quote s))
			     nil t))
	;; OK, found a match
	(setq type 'dedicated)
	(goto-char (match-beginning 0)))
       ((and (not org-link-search-inhibit-query)
	     (eq org-link-search-must-match-exact-headline 'query-to-create)
	     (y-or-n-p "No match - create this as a new heading? "))
	(goto-char (point-max))
	(or (bolp) (newline))
	(insert "* " s "\n")
	(beginning-of-line 0))
       (t
	(goto-char pos)
	(error "No match"))))
     (t
      ;; A normal search string
      (when (equal (string-to-char s) ?*)
	;; Anchor on headlines, post may include tags.
	(setq pre "^\\*+[ \t]+\\(?:\\sw+\\)?[ \t]*"
	      post (org-re "[ \t]*\\(?:[ \t]+:[[:alnum:]_@#%:+]:[ \t]*\\)?$")
	      s (substring s 1)))
      (remove-text-properties
       0 (length s)
       '(face nil mouse-face nil keymap nil fontified nil) s)
      ;; Make a series of regular expressions to find a match
      (setq words (org-split-string s "[ \n\r\t]+")

	    re0 (concat "\\(<<" (regexp-quote s0) ">>\\)")
	    re2 (concat markers "\\(" (mapconcat 'downcase words "[ \t]+")
			"\\)" markers)
	    re2a_ (concat "\\(" (mapconcat 'downcase words "[ \t\r\n]+") "\\)[ \t\r\n]")
	    re2a (concat "[ \t\r\n]" re2a_)
	    re4_ (concat "\\(" (mapconcat 'downcase words "[^a-zA-Z_\r\n]+") "\\)[^a-zA-Z_]")
	    re4 (concat "[^a-zA-Z_]" re4_)

	    re1 (concat pre re2 post)
	    re3 (concat pre (if pre re4_ re4) post)
	    re5 (concat pre ".*" re4)
	    re2 (concat pre re2)
	    re2a (concat pre (if pre re2a_ re2a))
	    re4 (concat pre (if pre re4_ re4))
	    reall (concat "\\(" re0 "\\)\\|\\(" re1 "\\)\\|\\(" re2
			  "\\)\\|\\(" re3 "\\)\\|\\(" re4 "\\)\\|\\("
			  re5 "\\)"
			  ))
      (cond
       ((eq type 'org-occur) (org-occur reall))
       ((eq type 'occur) (org-do-occur (downcase reall) 'cleanup))
       (t (goto-char (point-min))
	  (setq type 'fuzzy)
	  (if (or (and (org-search-not-self 1 re0 nil t) (setq type 'dedicated))
		  (org-search-not-self 1 re1 nil t)
		  (org-search-not-self 1 re2 nil t)
		  (org-search-not-self 1 re2a nil t)
		  (org-search-not-self 1 re3 nil t)
		  (org-search-not-self 1 re4 nil t)
		  (org-search-not-self 1 re5 nil t)
		  )
	      (goto-char (match-beginning 1))
	    (goto-char pos)
	    (error "No match"))))))
    (and (eq major-mode 'org-mode)
	 (not stealth)
	 (org-show-context 'link-search))
    type))

(defun org-search-not-self (group &rest args)
  "Execute `re-search-forward', but only accept matches that do not
enclose the position of `org-open-link-marker'."
  (let ((m org-open-link-marker))
    (catch 'exit
      (while (apply 're-search-forward args)
	(unless (get-text-property (match-end group) 'intangible) ; Emacs 21
	  (goto-char (match-end group))
	  (if (and (or (not (eq (marker-buffer m) (current-buffer)))
		       (> (match-beginning 0) (marker-position m))
		       (< (match-end 0) (marker-position m)))
		   (save-match-data
		     (or (not (org-in-regexp
			       org-bracket-link-analytic-regexp 1))
			 (not (match-end 4))  ; no description
			 (and (<= (match-beginning 4) (point))
			      (>= (match-end 4) (point))))))
	      (throw 'exit (point))))))))

(defun org-get-buffer-for-internal-link (buffer)
  "Return a buffer to be used for displaying the link target of internal links."
  (cond
   ((not org-display-internal-link-with-indirect-buffer)
    buffer)
   ((string-match "(Clone)$" (buffer-name buffer))
    (message "Buffer is already a clone, not making another one")
    ;; we also do not modify visibility in this case
    buffer)
   (t ; make a new indirect buffer for displaying the link
    (let* ((bn (buffer-name buffer))
	   (ibn (concat bn "(Clone)"))
	   (ib (or (get-buffer ibn) (make-indirect-buffer buffer ibn 'clone))))
      (with-current-buffer ib (org-overview))
      ib))))

(defun org-do-occur (regexp &optional cleanup)
  "Call the Emacs command `occur'.
If CLEANUP is non-nil, remove the printout of the regular expression
in the *Occur* buffer.  This is useful if the regex is long and not useful
to read."
  (occur regexp)
  (when cleanup
    (let ((cwin (selected-window)) win beg end)
      (when (setq win (get-buffer-window "*Occur*"))
	(select-window win))
      (goto-char (point-min))
      (when (re-search-forward "match[a-z]+" nil t)
	(setq beg (match-end 0))
	(if (re-search-forward "^[ \t]*[0-9]+" nil t)
	    (setq end (1- (match-beginning 0)))))
      (and beg end (let ((inhibit-read-only t)) (delete-region beg end)))
      (goto-char (point-min))
      (select-window cwin))))

;;; The mark ring for links jumps

(defvar org-mark-ring nil
  "Mark ring for positions before jumps in Org-mode.")
(defvar org-mark-ring-last-goto nil
  "Last position in the mark ring used to go back.")
;; Fill and close the ring
(setq org-mark-ring nil org-mark-ring-last-goto nil) ;; in case file is reloaded
(loop for i from 1 to org-mark-ring-length do
      (push (make-marker) org-mark-ring))
(setcdr (nthcdr (1- org-mark-ring-length) org-mark-ring)
	org-mark-ring)

(defun org-mark-ring-push (&optional pos buffer)
  "Put the current position or POS into the mark ring and rotate it."
  (interactive)
  (setq pos (or pos (point)))
  (setq org-mark-ring (nthcdr (1- org-mark-ring-length) org-mark-ring))
  (move-marker (car org-mark-ring)
	       (or pos (point))
	       (or buffer (current-buffer)))
  (message "%s"
   (substitute-command-keys
    "Position saved to mark ring, go back with \\[org-mark-ring-goto].")))

(defun org-mark-ring-goto (&optional n)
  "Jump to the previous position in the mark ring.
With prefix arg N, jump back that many stored positions.  When
called several times in succession, walk through the entire ring.
Org-mode commands jumping to a different position in the current file,
or to another Org-mode file, automatically push the old position
onto the ring."
  (interactive "p")
  (let (p m)
    (if (eq last-command this-command)
	(setq p (nthcdr n (or org-mark-ring-last-goto org-mark-ring)))
      (setq p org-mark-ring))
    (setq org-mark-ring-last-goto p)
    (setq m (car p))
    (org-pop-to-buffer-same-window (marker-buffer m))
    (goto-char m)
    (if (or (outline-invisible-p) (org-invisible-p2)) (org-show-context 'mark-goto))))

(defun org-remove-angle-brackets (s)
  (if (equal (substring s 0 1) "<") (setq s (substring s 1)))
  (if (equal (substring s -1) ">") (setq s (substring s 0 -1)))
  s)
(defun org-add-angle-brackets (s)
  (if (equal (substring s 0 1) "<") nil (setq s (concat "<" s)))
  (if (equal (substring s -1) ">") nil (setq s (concat s ">")))
  s)
(defun org-remove-double-quotes (s)
  (if (equal (substring s 0 1) "\"") (setq s (substring s 1)))
  (if (equal (substring s -1) "\"") (setq s (substring s 0 -1)))
  s)

;;; Following specific links

(defun org-follow-timestamp-link ()
  (cond
   ((org-at-date-range-p t)
    (let ((org-agenda-start-on-weekday)
	  (t1 (match-string 1))
	  (t2 (match-string 2)))
      (setq t1 (time-to-days (org-time-string-to-time t1))
	    t2 (time-to-days (org-time-string-to-time t2)))
      (org-agenda-list nil t1 (1+ (- t2 t1)))))
   ((org-at-timestamp-p t)
    (org-agenda-list nil (time-to-days (org-time-string-to-time
					(substring (match-string 1) 0 10)))
		     1))
   (t (error "This should not happen"))))


;;; Following file links
(declare-function mailcap-parse-mailcaps "mailcap" (&optional path force))
(declare-function mailcap-extension-to-mime "mailcap" (extn))
(declare-function mailcap-mime-info
		  "mailcap" (string &optional request no-decode))
(defvar org-wait nil)
(defun org-open-file (path &optional in-emacs line search)
  "Open the file at PATH.
First, this expands any special file name abbreviations.  Then the
configuration variable `org-file-apps' is checked if it contains an
entry for this file type, and if yes, the corresponding command is launched.

If no application is found, Emacs simply visits the file.

With optional prefix argument IN-EMACS, Emacs will visit the file.
With a double \\[universal-argument] \\[universal-argument] \
prefix arg, Org tries to avoid opening in Emacs
and to use an external application to visit the file.

Optional LINE specifies a line to go to, optional SEARCH a string
to search for.  If LINE or SEARCH is given, the file will be
opened in Emacs, unless an entry from org-file-apps that makes
use of groups in a regexp matches.

If you want to change the way frames are used when following a
link, please customize `org-link-frame-setup'.

If the file does not exist, an error is thrown."
  (let* ((file (if (equal path "")
		   buffer-file-name
		 (substitute-in-file-name (expand-file-name path))))
	 (file-apps (append org-file-apps (org-default-apps)))
	 (apps (org-remove-if
		'org-file-apps-entry-match-against-dlink-p file-apps))
	 (apps-dlink (org-remove-if-not
		      'org-file-apps-entry-match-against-dlink-p file-apps))
	 (remp (and (assq 'remote apps) (org-file-remote-p file)))
	 (dirp (if remp nil (file-directory-p file)))
	 (file (if (and dirp org-open-directory-means-index-dot-org)
		   (concat (file-name-as-directory file) "index.org")
		 file))
	 (a-m-a-p (assq 'auto-mode apps))
	 (dfile (downcase file))
	 ;; reconstruct the original file: link from the PATH, LINE and SEARCH args
	 (link (cond ((and (eq line nil)
			    (eq search nil))
		       file)
		      (line
		      (concat file "::" (number-to-string line)))
		     (search
		      (concat file "::" search))))
	 (dlink (downcase link))
	 (old-buffer (current-buffer))
	 (old-pos (point))
	 (old-mode major-mode)
	 ext cmd link-match-data)
    (if (string-match "^.*\\.\\([a-zA-Z0-9]+\\.gz\\)$" dfile)
	(setq ext (match-string 1 dfile))
      (if (string-match "^.*\\.\\([a-zA-Z0-9]+\\)$" dfile)
	  (setq ext (match-string 1 dfile))))
    (cond
     ((member in-emacs '((16) system))
      (setq cmd (cdr (assoc 'system apps))))
     (in-emacs (setq cmd 'emacs))
     (t
      (setq cmd (or (and remp (cdr (assoc 'remote apps)))
		    (and dirp (cdr (assoc 'directory apps)))
		    ; first, try matching against apps-dlink
		    ; if we get a match here, store the match data for later
		    (let ((match (assoc-default dlink apps-dlink
						'string-match)))
		      (if match
			  (progn (setq link-match-data (match-data))
				 match)
			(progn (setq in-emacs (or in-emacs line search))
			       nil))) ; if we have no match in apps-dlink,
				      ; always open the file in emacs if line or search
				      ; is given (for backwards compatibility)
		    (assoc-default dfile (org-apps-regexp-alist apps a-m-a-p)
				   'string-match)
		    (cdr (assoc ext apps))
		    (cdr (assoc t apps))))))
    (when (eq cmd 'system)
      (setq cmd (cdr (assoc 'system apps))))
    (when (eq cmd 'default)
      (setq cmd (cdr (assoc t apps))))
    (when (eq cmd 'mailcap)
      (require 'mailcap)
      (mailcap-parse-mailcaps)
      (let* ((mime-type (mailcap-extension-to-mime (or ext "")))
	     (command (mailcap-mime-info mime-type)))
	(if (stringp command)
	    (setq cmd command)
	  (setq cmd 'emacs))))
    (if (and (not (eq cmd 'emacs)) ; Emacs has no problems with non-ex files
	     (not (file-exists-p file))
	     (not org-open-non-existing-files))
	(error "No such file: %s" file))
    (cond
     ((and (stringp cmd) (not (string-match "^\\s-*$" cmd)))
      ;; Remove quotes around the file name - we'll use shell-quote-argument.
      (while (string-match "['\"]%s['\"]" cmd)
	(setq cmd (replace-match "%s" t t cmd)))
      (while (string-match "%s" cmd)
	(setq cmd (replace-match
		   (save-match-data
		     (shell-quote-argument
		      (convert-standard-filename file)))
		   t t cmd)))

      ;; Replace "%1", "%2" etc. in command with group matches from regex
      (save-match-data
	(let ((match-index 1)
	      (number-of-groups (- (/ (length link-match-data) 2) 1)))
	  (set-match-data link-match-data)
	  (while (<= match-index number-of-groups)
	    (let ((regex (concat "%" (number-to-string match-index)))
		  (replace-with (match-string match-index dlink)))
	      (while (string-match regex cmd)
		(setq cmd (replace-match replace-with t t cmd))))
	    (setq match-index (+ match-index 1)))))

      (save-window-excursion
	(start-process-shell-command cmd nil cmd)
	(and (boundp 'org-wait) (numberp org-wait) (sit-for org-wait))
	))
     ((or (stringp cmd)
	  (eq cmd 'emacs))
      (funcall (cdr (assq 'file org-link-frame-setup)) file)
      (widen)
      (if line (org-goto-line line)
	(if search (org-link-search search))))
     ((consp cmd)
      (let ((file (convert-standard-filename file)))
	(save-match-data
	  (set-match-data link-match-data)
	  (eval cmd))))
     (t (funcall (cdr (assq 'file org-link-frame-setup)) file)))
    (and (eq major-mode 'org-mode) (eq old-mode 'org-mode)
	 (or (not (equal old-buffer (current-buffer)))
	     (not (equal old-pos (point))))
	 (org-mark-ring-push old-pos old-buffer))))

(defun org-file-apps-entry-match-against-dlink-p (entry)
  "This function returns non-nil if `entry' uses a regular
expression which should be matched against the whole link by
org-open-file.

It assumes that is the case when the entry uses a regular
expression which has at least one grouping construct and the
action is either a lisp form or a command string containing
'%1', i.e. using at least one subexpression match as a
parameter."
  (let ((selector (car entry))
	(action (cdr entry)))
    (if (stringp selector)
	(and (> (regexp-opt-depth selector) 0)
	     (or (and (stringp action)
		      (string-match "%[0-9]" action))
		 (consp action)))
      nil)))

(defun org-default-apps ()
  "Return the default applications for this operating system."
  (cond
   ((eq system-type 'darwin)
    org-file-apps-defaults-macosx)
   ((eq system-type 'windows-nt)
    org-file-apps-defaults-windowsnt)
   (t org-file-apps-defaults-gnu)))

(defun org-apps-regexp-alist (list &optional add-auto-mode)
  "Convert extensions to regular expressions in the cars of LIST.
Also, weed out any non-string entries, because the return value is used
only for regexp matching.
When ADD-AUTO-MODE is set, make all matches in `auto-mode-alist'
point to the symbol `emacs', indicating that the file should
be opened in Emacs."
  (append
   (delq nil
	 (mapcar (lambda (x)
		   (if (not (stringp (car x)))
		       nil
		     (if (string-match "\\W" (car x))
			 x
		       (cons (concat "\\." (car x) "\\'") (cdr x)))))
		 list))
   (if add-auto-mode
       (mapcar (lambda (x) (cons (car x) 'emacs)) auto-mode-alist))))

(defvar ange-ftp-name-format) ; to silence the XEmacs compiler.
(defun org-file-remote-p (file)
  "Test whether FILE specifies a location on a remote system.
Return non-nil if the location is indeed remote.

For example, the filename \"/user@host:/foo\" specifies a location
on the system \"/user@host:\"."
  (cond ((fboundp 'file-remote-p)
         (file-remote-p file))
        ((fboundp 'tramp-handle-file-remote-p)
         (tramp-handle-file-remote-p file))
        ((and (boundp 'ange-ftp-name-format)
              (string-match (car ange-ftp-name-format) file))
         t)
        (t nil)))


;;;; Refiling

(defun org-get-org-file ()
  "Read a filename, with default directory `org-directory'."
  (let ((default (or org-default-notes-file remember-data-file)))
    (read-file-name (format "File name [%s]: " default)
		    (file-name-as-directory org-directory)
		    default)))

(defun org-notes-order-reversed-p ()
  "Check if the current file should receive notes in reversed order."
  (cond
   ((not org-reverse-note-order) nil)
   ((eq t org-reverse-note-order) t)
   ((not (listp org-reverse-note-order)) nil)
   (t (catch 'exit
	(let  ((all org-reverse-note-order)
	       entry)
	  (while (setq entry (pop all))
	    (if (string-match (car entry) buffer-file-name)
		(throw 'exit (cdr entry))))
	  nil)))))

(defvar org-refile-target-table nil
  "The list of refile targets, created by `org-refile'.")

(defvar org-agenda-new-buffers nil
  "Buffers created to visit agenda files.")

(defvar org-refile-cache nil
  "Cache for refile targets.")

(defvar org-refile-markers nil
  "All the markers used for caching refile locations.")

(defun org-refile-marker (pos)
  "Get a new refile marker, but only if caching is in use."
  (if (not org-refile-use-cache)
      pos
    (let ((m (make-marker)))
      (move-marker m pos)
      (push m org-refile-markers)
      m)))

(defun org-refile-cache-clear ()
  "Clear the refile cache and disable all the markers."
  (mapc (lambda (m) (move-marker m nil)) org-refile-markers)
  (setq org-refile-markers nil)
  (setq org-refile-cache nil)
  (message "Refile cache has been cleared"))

(defun org-refile-cache-check-set (set)
  "Check if all the markers in the cache still have live buffers."
  (let (marker)
    (catch 'exit
      (while (and set (setq marker (nth 3 (pop set))))
	;; if org-refile-use-outline-path is 'file, marker may be nil
	(when (and marker (null (marker-buffer marker)))
	  (message "not found") (sit-for 3)
	  (throw 'exit nil)))
      t)))

(defun org-refile-cache-put (set &rest identifiers)
  "Push the refile targets SET into the cache, under IDENTIFIERS."
  (let* ((key (sha1 (prin1-to-string identifiers)))
	 (entry (assoc key org-refile-cache)))
    (if entry
	(setcdr entry set)
      (push (cons key set) org-refile-cache))))

(defun org-refile-cache-get (&rest identifiers)
  "Retrieve the cached value for refile targets given by IDENTIFIERS."
  (cond
   ((not org-refile-cache) nil)
   ((not org-refile-use-cache) (org-refile-cache-clear) nil)
   (t
    (let ((set (cdr (assoc (sha1 (prin1-to-string identifiers))
			   org-refile-cache))))
      (and set (org-refile-cache-check-set set) set)))))

(defun org-refile-get-targets (&optional default-buffer excluded-entries)
  "Produce a table with refile targets."
  (let ((case-fold-search nil)
	;; otherwise org confuses "TODO" as a kw and "Todo" as a word
	(entries (or org-refile-targets '((nil . (:level . 1)))))
	targets tgs txt re files f desc descre fast-path-p level pos0)
    (message "Getting targets...")
    (with-current-buffer (or default-buffer (current-buffer))
      (while (setq entry (pop entries))
	(setq files (car entry) desc (cdr entry))
	(setq fast-path-p nil)
	(cond
	 ((null files) (setq files (list (current-buffer))))
	 ((eq files 'org-agenda-files)
	  (setq files (org-agenda-files 'unrestricted)))
	 ((and (symbolp files) (fboundp files))
	  (setq files (funcall files)))
	 ((and (symbolp files) (boundp files))
	  (setq files (symbol-value files))))
	(if (stringp files) (setq files (list files)))
	(cond
	 ((eq (car desc) :tag)
	  (setq descre (concat "^\\*+[ \t]+.*?:" (regexp-quote (cdr desc)) ":")))
	 ((eq (car desc) :todo)
	  (setq descre (concat "^\\*+[ \t]+" (regexp-quote (cdr desc)) "[ \t]")))
	 ((eq (car desc) :regexp)
	  (setq descre (cdr desc)))
	 ((eq (car desc) :level)
	  (setq descre (concat "^\\*\\{" (number-to-string
					  (if org-odd-levels-only
					      (1- (* 2 (cdr desc)))
					    (cdr desc)))
			       "\\}[ \t]")))
	 ((eq (car desc) :maxlevel)
	  (setq fast-path-p t)
	  (setq descre (concat "^\\*\\{1," (number-to-string
					    (if org-odd-levels-only
						(1- (* 2 (cdr desc)))
					      (cdr desc)))
			       "\\}[ \t]")))
	 (t (error "Bad refiling target description %s" desc)))
	(while (setq f (pop files))
	  (with-current-buffer
	      (if (bufferp f) f (org-get-agenda-file-buffer f))
	    (or
	     (setq tgs (org-refile-cache-get (buffer-file-name) descre))
	     (progn
	       (if (bufferp f) (setq f (buffer-file-name
					(buffer-base-buffer f))))
	       (setq f (and f (expand-file-name f)))
	       (if (eq org-refile-use-outline-path 'file)
		   (push (list (file-name-nondirectory f) f nil nil) tgs))
	       (save-excursion
		 (save-restriction
		   (widen)
		   (goto-char (point-min))
		   (while (re-search-forward descre nil t)
		     (goto-char (setq pos0 (point-at-bol)))
		     (catch 'next
		       (when org-refile-target-verify-function
			 (save-match-data
			   (or (funcall org-refile-target-verify-function)
			       (throw 'next t))))
		       (when (and (looking-at org-complex-heading-regexp)
				  (not (member (match-string 4) excluded-entries))
				  (match-string 4))
			 (setq level (org-reduced-level
				      (- (match-end 1) (match-beginning 1)))
			       txt (org-link-display-format (match-string 4))
			       txt (replace-regexp-in-string "\\( *\[[0-9]+/?[0-9]*%?\]\\)+$" "" txt)
			       re (format org-complex-heading-regexp-format
					  (regexp-quote (match-string 4))))
			 (when org-refile-use-outline-path
			   (setq txt (mapconcat
				      'org-protect-slash
				      (append
				       (if (eq org-refile-use-outline-path
					       'file)
					   (list (file-name-nondirectory
						  (buffer-file-name
						   (buffer-base-buffer))))
					 (if (eq org-refile-use-outline-path
						 'full-file-path)
					     (list (buffer-file-name
						    (buffer-base-buffer)))))
				       (org-get-outline-path fast-path-p
							     level txt)
				       (list txt))
				      "/")))
			 (push (list txt f re (org-refile-marker (point)))
			       tgs)))
		     (when (= (point) pos0)
		       ;; verification function has not moved point
		       (goto-char (point-at-eol))))))))
	    (when org-refile-use-cache
	      (org-refile-cache-put tgs (buffer-file-name) descre))
	    (setq targets (append tgs targets))
	    ))))
    (message "Getting targets...done")
    (nreverse targets)))

(defun org-protect-slash (s)
  (while (string-match "/" s)
    (setq s (replace-match "\\" t t s)))
  s)

(defvar org-olpa (make-vector 20 nil))

(defun org-get-outline-path (&optional fastp level heading)
  "Return the outline path to the current entry, as a list.

The parameters FASTP, LEVEL, and HEADING are for use by a scanner
routine which makes outline path derivations for an entire file,
avoiding backtracing.  Refile target collection makes use of that."
  (if fastp
      (progn
	(if (> level 19)
	    (error "Outline path failure, more than 19 levels"))
	(loop for i from level upto 19 do
	      (aset org-olpa i nil))
	(prog1
	    (delq nil (append org-olpa nil))
	  (aset org-olpa level heading)))
    (let (rtn case-fold-search)
      (save-excursion
	(save-restriction
	  (widen)
	  (while (org-up-heading-safe)
	    (when (looking-at org-complex-heading-regexp)
	      (push (org-match-string-no-properties 4) rtn)))
	  rtn)))))

(defun org-format-outline-path (path &optional width prefix)
  "Format the outline path PATH for display.
Width is the maximum number of characters that is available.
Prefix is a prefix to be included in the returned string,
such as the file name."
  (setq width (or width 79))
  (if prefix (setq width (- width (length prefix))))
  (if (not path)
      (or prefix "")
    (let* ((nsteps (length path))
	   (total-width (+ nsteps (apply '+ (mapcar 'length path))))
	   (maxwidth (if (<= total-width width)
			 10000  ;; everything fits
		       ;; we need to shorten the level headings
		       (/ (- width nsteps) nsteps)))
	   (org-odd-levels-only nil)
	   (n 0)
	   (total (1+ (length prefix))))
      (setq maxwidth (max maxwidth 10))
      (concat prefix
	      (mapconcat
	       (lambda (h)
		 (setq n (1+ n))
		 (if (and (= n nsteps) (< maxwidth 10000))
		     (setq maxwidth (- total-width total)))
		 (if (< (length h) maxwidth)
		     (progn (setq total (+ total (length h) 1)) h)
		   (setq h (substring h 0 (- maxwidth 2))
			 total (+ total maxwidth 1))
		   (if (string-match "[ \t]+\\'" h)
		       (setq h (substring h 0 (match-beginning 0))))
		   (setq h (concat  h "..")))
		 (org-add-props h nil 'face
				(nth (% (1- n) org-n-level-faces)
				     org-level-faces))
		 h)
	       path "/")))))

(defun org-display-outline-path (&optional file current)
  "Display the current outline path in the echo area."
  (interactive "P")
  (let* ((bfn (buffer-file-name (buffer-base-buffer)))
	 (case-fold-search nil)
	 (path (and (eq major-mode 'org-mode) (org-get-outline-path))))
    (if current (setq path (append path
				   (save-excursion
				     (org-back-to-heading t)
				     (if (looking-at org-complex-heading-regexp)
					 (list (match-string 4)))))))
    (message "%s"
	     (org-format-outline-path
	      path
	      (1- (frame-width))
	      (and file bfn (concat (file-name-nondirectory bfn) "/"))))))

(defvar org-refile-history nil
  "History for refiling operations.")

(defvar org-after-refile-insert-hook nil
  "Hook run after `org-refile' has inserted its stuff at the new location.
Note that this is still *before* the stuff will be removed from
the *old* location.")

(defvar org-capture-last-stored-marker)
(defun org-refile (&optional goto default-buffer rfloc)
  "Move the entry or entries at point to another heading.
The list of target headings is compiled using the information in
`org-refile-targets', which see.

At the target location, the entry is filed as a subitem of the target
heading.  Depending on `org-reverse-note-order', the new subitem will
either be the first or the last subitem.

If there is an active region, all entries in that region will be moved.
However, the region must fulfill the requirement that the first heading
is the first one sets the top-level of the moved text - at most siblings
below it are allowed.

With prefix arg GOTO, the command will only visit the target location
and not actually move anything.

With a double prefix arg \\[universal-argument] \\[universal-argument], \
go to the location where the last refiling operation has put the subtree.
With a prefix argument of `2', refile to the running clock.

RFLOC can be a refile location obtained in a different way.

See also `org-refile-use-outline-path' and `org-completion-use-ido'.

If you are using target caching (see `org-refile-use-cache'),
You have to clear the target cache in order to find new targets.
This can be done with a 0 prefix (`C-0 C-c C-w') or a triple
prefix argument (`C-u C-u C-u C-c C-w')."

  (interactive "P")
  (if (member goto '(0 (64)))
      (org-refile-cache-clear)
    (let* ((cbuf (current-buffer))
	   (regionp (org-region-active-p))
	   (region-start (and regionp (region-beginning)))
	   (region-end (and regionp (region-end)))
	   (region-length (and regionp (- region-end region-start)))
	   (filename (buffer-file-name (buffer-base-buffer cbuf)))
	   pos it nbuf file re level reversed)
      (setq last-command nil)
      (when regionp
	(goto-char region-start)
	(or (bolp) (goto-char (point-at-bol)))
	(setq region-start (point))
	(unless (or (org-kill-is-subtree-p
		     (buffer-substring region-start region-end))
		    (prog1 org-refile-active-region-within-subtree
		      (org-toggle-heading)))
	  (error "The region is not a (sequence of) subtree(s)")))
      (if (equal goto '(16))
	  (org-refile-goto-last-stored)
	(when (or
	       (and (equal goto 2)
		    org-clock-hd-marker (marker-buffer org-clock-hd-marker)
		    (prog1
			(setq it (list (or org-clock-heading "running clock")
				       (buffer-file-name
					(marker-buffer org-clock-hd-marker))
				       ""
				       (marker-position org-clock-hd-marker)))
		      (setq goto nil)))
	       (setq it (or rfloc
			    (let (heading-text)
			      (save-excursion
				(unless goto
				  (org-back-to-heading t)
				  (setq heading-text
					(nth 4 (org-heading-components))))
				(org-refile-get-location
				 (cond (goto "Goto")
				       (regionp "Refile region to")
				       (t (concat "Refile subtree \""
						  heading-text "\" to")))
				 default-buffer
				 (and (not (equal '(4) goto))
				      org-refile-allow-creating-parent-nodes)
				 goto))))))
	  (setq file (nth 1 it)
		re (nth 2 it)
		pos (nth 3 it))
	  (if (and (not goto)
		   pos
		   (equal (buffer-file-name) file)
		   (if regionp
		       (and (>= pos region-start)
			    (<= pos region-end))
		     (and (>= pos (point))
			  (< pos (save-excursion
				   (org-end-of-subtree t t))))))
	      (error "Cannot refile to position inside the tree or region"))

	  (setq nbuf (or (find-buffer-visiting file)
			 (find-file-noselect file)))
	  (if goto
	      (progn
		(org-pop-to-buffer-same-window nbuf)
		(goto-char pos)
		(org-show-context 'org-goto))
	    (if regionp
		(progn
		  (org-kill-new (buffer-substring region-start region-end))
		  (org-save-markers-in-region region-start region-end))
	      (org-copy-subtree 1 nil t))
	    (with-current-buffer (setq nbuf (or (find-buffer-visiting file)
						(find-file-noselect file)))
	      (setq reversed (org-notes-order-reversed-p))
	      (save-excursion
		(save-restriction
		  (widen)
		  (if pos
		      (progn
			(goto-char pos)
			(looking-at org-outline-regexp)
			(setq level (org-get-valid-level (funcall outline-level) 1))
			(goto-char
			 (if reversed
			     (or (outline-next-heading) (point-max))
			   (or (save-excursion (org-get-next-sibling))
			       (org-end-of-subtree t t)
			       (point-max)))))
		    (setq level 1)
		    (if (not reversed)
			(goto-char (point-max))
		      (goto-char (point-min))
		      (or (outline-next-heading) (goto-char (point-max)))))
		  (if (not (bolp)) (newline))
		  (org-paste-subtree level)
		  (when org-log-refile
		    (org-add-log-setup 'refile nil nil 'findpos
				       org-log-refile)
		    (unless (eq org-log-refile 'note)
		      (save-excursion (org-add-log-note))))
		  (and org-auto-align-tags (org-set-tags nil t))
		  (bookmark-set "org-refile-last-stored")
		  ;; If we are refiling for capture, make sure that the
		  ;; last-capture pointers point here
		  (when (org-bound-and-true-p org-refile-for-capture)
		    (bookmark-set "org-capture-last-stored-marker")
		    (move-marker org-capture-last-stored-marker (point)))
		  (if (fboundp 'deactivate-mark) (deactivate-mark))
		  (run-hooks 'org-after-refile-insert-hook))))
	    (if regionp
		(delete-region (point) (+ (point) region-length))
	      (org-cut-subtree))
	    (when (featurep 'org-inlinetask)
	      (org-inlinetask-remove-END-maybe))
	    (setq org-markers-to-move nil)
	    (message "Refiled to \"%s\" in file %s" (car it) file)))))))

(defun org-refile-goto-last-stored ()
  "Go to the location where the last refile was stored."
  (interactive)
  (bookmark-jump "org-refile-last-stored")
  (message "This is the location of the last refile"))

(defun org-refile-get-location (&optional prompt default-buffer new-nodes
					  no-exclude)
  "Prompt the user for a refile location, using PROMPT.
PROMPT should not be suffixed with a colon and a space, because
this function appends the default value from
`org-refile-history' automatically, if that is not empty.
When NO-EXCLUDE is set, do not exclude headlines in the current subtree,
this is used for the GOTO interface."
  (let ((org-refile-targets org-refile-targets)
	(org-refile-use-outline-path org-refile-use-outline-path)
	excluded-entries)
    (when (and (eq major-mode 'org-mode)
	       (not org-refile-use-cache)
	       (not no-exclude))
      (org-map-tree
       (lambda()
	 (setq excluded-entries
	       (append excluded-entries (list (org-get-heading t t)))))))
    (setq org-refile-target-table
	  (org-refile-get-targets default-buffer excluded-entries)))
  (unless org-refile-target-table
    (error "No refile targets"))
  (let* ((prompt (concat prompt
			 (and (car org-refile-history)
			      (concat " (default " (car org-refile-history) ")"))
			 ": "))
	 (cbuf (current-buffer))
	 (partial-completion-mode nil)
	 (cfn (buffer-file-name (buffer-base-buffer cbuf)))
	 (cfunc (if (and org-refile-use-outline-path
			 org-outline-path-complete-in-steps)
		    'org-olpath-completing-read
		  'org-icompleting-read))
	 (extra (if org-refile-use-outline-path "/" ""))
	 (filename (and cfn (expand-file-name cfn)))
	 (tbl (mapcar
	       (lambda (x)
		 (if (and (not (member org-refile-use-outline-path
				       '(file full-file-path)))
			  (not (equal filename (nth 1 x))))
		     (cons (concat (car x) extra " ("
				   (file-name-nondirectory (nth 1 x)) ")")
			   (cdr x))
		   (cons (concat (car x) extra) (cdr x))))
	       org-refile-target-table))
	 (completion-ignore-case t)
	 pa answ parent-target child parent old-hist)
    (setq old-hist org-refile-history)
    (setq answ (funcall cfunc prompt tbl nil (not new-nodes)
			nil 'org-refile-history (car org-refile-history)))
    (setq pa (or (assoc answ tbl) (assoc (concat answ "/") tbl)))
    (org-refile-check-position pa)
    (if pa
	(progn
	  (when (or (not org-refile-history)
		    (not (eq old-hist org-refile-history))
		    (not (equal (car pa) (car org-refile-history))))
	    (setq org-refile-history
		  (cons (car pa) (if (assoc (car org-refile-history) tbl)
				     org-refile-history
				   (cdr org-refile-history))))
	    (if (equal (car org-refile-history) (nth 1 org-refile-history))
		(pop org-refile-history)))
	  pa)
      (if (string-match "\\`\\(.*\\)/\\([^/]+\\)\\'" answ)
	  (progn
	    (setq parent (match-string 1 answ)
		  child (match-string 2 answ))
	    (setq parent-target (or (assoc parent tbl)
				    (assoc (concat parent "/") tbl)))
	    (when (and parent-target
		       (or (eq new-nodes t)
			   (and (eq new-nodes 'confirm)
				(y-or-n-p (format "Create new node \"%s\"? "
						  child)))))
	      (org-refile-new-child parent-target child)))
	(error "Invalid target location")))))

(defun org-refile-check-position (refile-pointer)
  "Check if the refile pointer matches the readline to which it points."
  (let* ((file (nth 1 refile-pointer))
	 (re (nth 2 refile-pointer))
	 (pos (nth 3 refile-pointer))
	 buffer)
    (when (org-string-nw-p re)
      (setq buffer (if (markerp pos)
		       (marker-buffer pos)
		     (or (find-buffer-visiting file)
			 (find-file-noselect file))))
      (with-current-buffer buffer
	(save-excursion
	  (save-restriction
	    (widen)
	    (goto-char pos)
	    (beginning-of-line 1)
	    (unless (org-looking-at-p re)
	      (error "Invalid refile position, please clear the cache with `C-0 C-c C-w' before refiling"))))))))

(defun org-refile-new-child (parent-target child)
  "Use refile target PARENT-TARGET to add new CHILD below it."
  (unless parent-target
    (error "Cannot find parent for new node"))
  (let ((file (nth 1 parent-target))
	(pos (nth 3 parent-target))
	level)
    (with-current-buffer (or (find-buffer-visiting file)
			     (find-file-noselect file))
      (save-excursion
	(save-restriction
	  (widen)
	  (if pos
	      (goto-char pos)
	    (goto-char (point-max))
	    (if (not (bolp)) (newline)))
	  (when (looking-at org-outline-regexp)
	    (setq level (funcall outline-level))
	    (org-end-of-subtree t t))
	  (org-back-over-empty-lines)
	  (insert "\n" (make-string
			(if pos (org-get-valid-level level 1) 1) ?*)
		  " " child "\n")
	  (beginning-of-line 0)
	  (list (concat (car parent-target) "/" child) file "" (point)))))))

(defun org-olpath-completing-read (prompt collection &rest args)
  "Read an outline path like a file name."
  (let ((thetable collection)
	(org-completion-use-ido nil)	   ; does not work with ido.
	(org-completion-use-iswitchb nil)) ; or iswitchb
    (apply
     'org-icompleting-read prompt
     (lambda (string predicate &optional flag)
       (let (rtn r f (l (length string)))
	 (cond
	  ((eq flag nil)
	   ;; try completion
	   (try-completion string thetable))
	  ((eq flag t)
	   ;; all-completions
	   (setq rtn (all-completions string thetable predicate))
	   (mapcar
	    (lambda (x)
	      (setq r (substring x l))
	      (if (string-match " ([^)]*)$" x)
		  (setq f (match-string 0 x))
		(setq f ""))
	      (if (string-match "/" r)
		  (concat string (substring r 0 (match-end 0)) f)
		x))
	    rtn))
	  ((eq flag 'lambda)
	   ;; exact match?
	   (assoc string thetable)))
	 ))
     args)))

;;;; Dynamic blocks

(defun org-find-dblock (name)
  "Find the first dynamic block with name NAME in the buffer.
If not found, stay at current position and return nil."
  (let (pos)
    (save-excursion
      (goto-char (point-min))
      (setq pos (and (re-search-forward (concat "^[ \t]*#\\+BEGIN:[ \t]+" name "\\>")
					nil t)
		     (match-beginning 0))))
    (if pos (goto-char pos))
    pos))

(defconst org-dblock-start-re
  "^[ \t]*#\\+BEGIN:[ \t]+\\(\\S-+\\)\\([ \t]+\\(.*\\)\\)?"
  "Matches the start line of a dynamic block, with parameters.")

(defconst org-dblock-end-re "^[ \t]*#\\+END\\([: \t\r\n]\\|$\\)"
  "Matches the end of a dynamic block.")

(defun org-create-dblock (plist)
  "Create a dynamic block section, with parameters taken from PLIST.
PLIST must contain a :name entry which is used as name of the block."
  (when (string-match "\\S-" (buffer-substring (point-at-bol) (point-at-eol)))
    (end-of-line 1)
    (newline))
  (let ((col (current-column))
	(name (plist-get plist :name)))
    (insert "#+BEGIN: " name)
    (while plist
      (if (eq (car plist) :name)
	  (setq plist (cddr plist))
	(insert " " (prin1-to-string (pop plist)))))
    (insert "\n\n" (make-string col ?\ ) "#+END:\n")
    (beginning-of-line -2)))

(defun org-prepare-dblock ()
  "Prepare dynamic block for refresh.
This empties the block, puts the cursor at the insert position and returns
the property list including an extra property :name with the block name."
  (unless (looking-at org-dblock-start-re)
    (error "Not at a dynamic block"))
  (let* ((begdel (1+ (match-end 0)))
	 (name (org-no-properties (match-string 1)))
	 (params (append (list :name name)
			 (read (concat "(" (match-string 3) ")")))))
    (save-excursion
      (beginning-of-line 1)
      (skip-chars-forward " \t")
      (setq params (plist-put params :indentation-column (current-column))))
    (unless (re-search-forward org-dblock-end-re nil t)
      (error "Dynamic block not terminated"))
    (setq params
	  (append params
		  (list :content (buffer-substring
				  begdel (match-beginning 0)))))
    (delete-region begdel (match-beginning 0))
    (goto-char begdel)
    (open-line 1)
    params))

(defun org-map-dblocks (&optional command)
  "Apply COMMAND to all dynamic blocks in the current buffer.
If COMMAND is not given, use `org-update-dblock'."
  (let ((cmd (or command 'org-update-dblock)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-dblock-start-re nil t)
	(goto-char (match-beginning 0))
        (save-excursion
          (condition-case nil
              (funcall cmd)
            (error (message "Error during update of dynamic block"))))
	(unless (re-search-forward org-dblock-end-re nil t)
	  (error "Dynamic block not terminated"))))))

(defun org-dblock-update (&optional arg)
  "User command for updating dynamic blocks.
Update the dynamic block at point.  With prefix ARG, update all dynamic
blocks in the buffer."
  (interactive "P")
  (if arg
      (org-update-all-dblocks)
    (or (looking-at org-dblock-start-re)
	(org-beginning-of-dblock))
    (org-update-dblock)))

(defun org-update-dblock ()
  "Update the dynamic block at point.
This means to empty the block, parse for parameters and then call
the correct writing function."
  (interactive)
  (save-window-excursion
    (let* ((pos (point))
	   (line (org-current-line))
	   (params (org-prepare-dblock))
	   (name (plist-get params :name))
	   (indent (plist-get params :indentation-column))
	   (cmd (intern (concat "org-dblock-write:" name))))
      (message "Updating dynamic block `%s' at line %d..." name line)
      (funcall cmd params)
      (message "Updating dynamic block `%s' at line %d...done" name line)
      (goto-char pos)
      (when (and indent (> indent 0))
	(setq indent (make-string indent ?\ ))
	(save-excursion
	(org-beginning-of-dblock)
	(forward-line 1)
	(while (not (looking-at org-dblock-end-re))
	  (insert indent)
	  (beginning-of-line 2))
	(when (looking-at org-dblock-end-re)
	  (and (looking-at "[ \t]+")
	       (replace-match ""))
	  (insert indent)))))))

(defun org-beginning-of-dblock ()
  "Find the beginning of the dynamic block at point.
Error if there is no such block at point."
  (let ((pos (point))
	beg)
    (end-of-line 1)
    (if (and (re-search-backward org-dblock-start-re nil t)
	     (setq beg (match-beginning 0))
	     (re-search-forward org-dblock-end-re nil t)
	     (> (match-end 0) pos))
	(goto-char beg)
      (goto-char pos)
      (error "Not in a dynamic block"))))

(defun org-update-all-dblocks ()
  "Update all dynamic blocks in the buffer.
This function can be used in a hook."
  (interactive)
  (when (eq major-mode 'org-mode)
    (org-map-dblocks 'org-update-dblock)))


;;;; Completion

(defconst org-additional-option-like-keywords
  '("BEGIN_HTML"  "END_HTML"  "HTML:" "ATTR_HTML:"
    "BEGIN_DocBook"  "END_DocBook"  "DocBook:" "ATTR_DocBook:"
    "BEGIN_LaTeX" "END_LaTeX" "LaTeX:" "LATEX_HEADER:"
    "LATEX_CLASS:" "LATEX_CLASS_OPTIONS:" "ATTR_LaTeX:"
    "BEGIN:" "END:"
    "ORGTBL" "TBLFM:" "TBLNAME:"
    "BEGIN_EXAMPLE" "END_EXAMPLE"
    "BEGIN_QUOTE" "END_QUOTE"
    "BEGIN_VERSE" "END_VERSE"
    "BEGIN_CENTER" "END_CENTER"
    "BEGIN_SRC" "END_SRC"
    "BEGIN_RESULT" "END_RESULT"
    "NAME:" "RESULTS:"
    "HEADER:" "HEADERS:"
    "CATEGORY:" "COLUMNS:" "PROPERTY:"
    "CAPTION:" "LABEL:"
    "SETUPFILE:"
    "INCLUDE:"
    "BIND:"
    "MACRO:"))

(defcustom org-structure-template-alist
  '(
    ("s" "#+begin_src ?\n\n#+end_src"
         "<src lang=\"?\">\n\n</src>")
    ("e" "#+begin_example\n?\n#+end_example"
         "<example>\n?\n</example>")
    ("q" "#+begin_quote\n?\n#+end_quote"
         "<quote>\n?\n</quote>")
    ("v" "#+BEGIN_VERSE\n?\n#+END_VERSE"
         "<verse>\n?\n</verse>")
    ("c" "#+BEGIN_CENTER\n?\n#+END_CENTER"
         "<center>\n?\n</center>")
    ("l" "#+BEGIN_LaTeX\n?\n#+END_LaTeX"
         "<literal style=\"latex\">\n?\n</literal>")
    ("L" "#+latex: "
         "<literal style=\"latex\">?</literal>")
    ("h" "#+begin_html\n?\n#+end_html"
         "<literal style=\"html\">\n?\n</literal>")
    ("H" "#+html: "
         "<literal style=\"html\">?</literal>")
    ("a" "#+begin_ascii\n?\n#+end_ascii")
    ("A" "#+ascii: ")
    ("i" "#+index: ?"
     "#+index: ?")
    ("I" "#+include %file ?"
         "<include file=%file markup=\"?\">")
    )
  "Structure completion elements.
This is a list of abbreviation keys and values.  The value gets inserted
if you type `<' followed by the key and then press the completion key,
usually `M-TAB'.  %file will be replaced by a file name after prompting
for the file using completion.  The cursor will be placed at the position
of the `?` in the template.
There are two templates for each key, the first uses the original Org syntax,
the second uses Emacs Muse-like syntax tags.  These Muse-like tags become
the default when the /org-mtags.el/ module has been loaded.  See also the
variable `org-mtags-prefer-muse-templates'.
This is an experimental feature, it is undecided if it is going to stay in."
  :group 'org-completion
  :type '(repeat
	  (string :tag "Key")
	  (string :tag "Template")
	  (string :tag "Muse Template")))

(defun org-try-structure-completion ()
  "Try to complete a structure template before point.
This looks for strings like \"<e\" on an otherwise empty line and
expands them."
  (let ((l (buffer-substring (point-at-bol) (point)))
	a)
    (when (and (looking-at "[ \t]*$")
	       (string-match "^[ \t]*<\\([a-zA-Z]+\\)$" l)
	       (setq a (assoc (match-string 1 l) org-structure-template-alist)))
      (org-complete-expand-structure-template (+ -1 (point-at-bol)
						 (match-beginning 1)) a)
      t)))

(defun org-complete-expand-structure-template (start cell)
  "Expand a structure template."
  (let* ((musep (org-bound-and-true-p org-mtags-prefer-muse-templates))
	 (rpl (nth (if musep 2 1) cell))
	 (ind ""))
    (delete-region start (point))
    (when (string-match "\\`#\\+" rpl)
      (cond
       ((bolp))
       ((not (string-match "\\S-" (buffer-substring (point-at-bol) (point))))
	(setq ind (buffer-substring (point-at-bol) (point))))
       (t (newline))))
    (setq start (point))
    (if (string-match "%file" rpl)
	(setq rpl (replace-match
		   (concat
		    "\""
		    (save-match-data
		      (abbreviate-file-name (read-file-name "Include file: ")))
		    "\"")
		   t t rpl)))
    (setq rpl (mapconcat 'identity (split-string rpl "\n")
			 (concat "\n" ind)))
    (insert rpl)
    (if (re-search-backward "\\?" start t) (delete-char 1))))

;;;; TODO, DEADLINE, Comments

(defun org-toggle-comment ()
  "Change the COMMENT state of an entry."
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (let (case-fold-search)
      (cond
       ((looking-at (format org-heading-keyword-regexp-format
			    org-comment-string))
	(goto-char (match-end 1))
	(looking-at (concat " +" org-comment-string))
	(replace-match "" t t)
	(when (eolp) (insert " ")))
       ((looking-at org-outline-regexp)
	(goto-char (match-end 0))
	(insert org-comment-string " "))))))

(defvar org-last-todo-state-is-todo nil
  "This is non-nil when the last TODO state change led to a TODO state.
If the last change removed the TODO tag or switched to DONE, then
this is nil.")

(defvar org-setting-tags nil) ; dynamically skipped

(defvar org-todo-setup-filter-hook nil
  "Hook for functions that pre-filter todo specs.
Each function takes a todo spec and returns either nil or the spec
transformed into canonical form." )

(defvar org-todo-get-default-hook nil
  "Hook for functions that get a default item for todo.
Each function takes arguments (NEW-MARK OLD-MARK) and returns either
nil or a string to be used for the todo mark." )

(defvar org-agenda-headline-snapshot-before-repeat)

(defun org-current-effective-time ()
  "Return current time adjusted for `org-extend-today-until' variable"
  (let* ((ct (org-current-time))
	  (dct (decode-time ct))
	  (ct1
	   (if (and org-use-effective-time
		    (< (nth 2 dct) org-extend-today-until))
	       (encode-time 0 59 23 (1- (nth 3 dct)) (nth 4 dct) (nth 5 dct))
	     ct)))
    ct1))

(defun org-todo-yesterday (&optional arg)
  "Like `org-todo' but the time of change will be 23:59 of yesterday."
  (interactive "P")
  (if (eq major-mode 'org-agenda-mode)
      (apply 'org-agenda-todo-yesterday arg)
    (let* ((hour (third (decode-time
			 (org-current-time))))
	   (org-extend-today-until (1+ hour)))
      (org-todo arg))))

(defun org-todo (&optional arg)
  "Change the TODO state of an item.
The state of an item is given by a keyword at the start of the heading,
like
     *** TODO Write paper
     *** DONE Call mom

The different keywords are specified in the variable `org-todo-keywords'.
By default the available states are \"TODO\" and \"DONE\".
So for this example: when the item starts with TODO, it is changed to DONE.
When it starts with DONE, the DONE is removed.  And when neither TODO nor
DONE are present, add TODO at the beginning of the heading.

With \\[universal-argument] prefix arg, use completion to determine the new \
state.
With numeric prefix arg, switch to that state.
With a double \\[universal-argument] prefix, switch to the next set of TODO \
keywords (nextset).
With a triple \\[universal-argument] prefix, circumvent any state blocking.
With a numeric prefix arg of 0, inhibit note taking for the change.

For calling through lisp, arg is also interpreted in the following way:
'none             -> empty state
\"\"(empty string)  -> switch to empty state
'done             -> switch to DONE
'nextset          -> switch to the next set of keywords
'previousset      -> switch to the previous set of keywords
\"WAITING\"         -> switch to the specified keyword, but only if it
                     really is a member of `org-todo-keywords'."
  (interactive "P")
  (if (and (org-region-active-p) org-loop-over-headlines-in-active-region)
      (let ((cl (if (eq org-loop-over-headlines-in-active-region 'start-level)
		    'region-start-level 'region))
	    org-loop-over-headlines-in-active-region)
	(org-map-entries
	 `(org-todo ,arg)
	 org-loop-over-headlines-in-active-region
	 cl (if (outline-invisible-p) (org-end-of-subtree nil t))))
    (if (equal arg '(16)) (setq arg 'nextset))
    (let ((org-blocker-hook org-blocker-hook)
	  (case-fold-search nil))
      (when (equal arg '(64))
	(setq arg nil org-blocker-hook nil))
      (when (and org-blocker-hook
		 (or org-inhibit-blocking
		     (org-entry-get nil "NOBLOCKING")))
	(setq org-blocker-hook nil))
      (save-excursion
	(catch 'exit
	  (org-back-to-heading t)
	  (if (looking-at org-outline-regexp) (goto-char (1- (match-end 0))))
	  (or (looking-at (concat " +" org-todo-regexp "\\( +\\|[ \t]*$\\)"))
	      (looking-at "\\(?: *\\|[ \t]*$\\)"))
	  (let* ((match-data (match-data))
		 (startpos (point-at-bol))
		 (logging (save-match-data (org-entry-get nil "LOGGING" t t)))
		 (org-log-done org-log-done)
		 (org-log-repeat org-log-repeat)
		 (org-todo-log-states org-todo-log-states)
		 (org-inhibit-logging
		  (if (equal arg 0)
		      (progn (setq arg nil) 'note) org-inhibit-logging))
		 (this (match-string 1))
		 (hl-pos (match-beginning 0))
		 (head (org-get-todo-sequence-head this))
		 (ass (assoc head org-todo-kwd-alist))
		 (interpret (nth 1 ass))
		 (done-word (nth 3 ass))
		 (final-done-word (nth 4 ass))
		 (org-last-state (or this ""))
		 (completion-ignore-case t)
		 (member (member this org-todo-keywords-1))
		 (tail (cdr member))
		 (org-state (cond
			     ((and org-todo-key-trigger
				   (or (and (equal arg '(4))
					    (eq org-use-fast-todo-selection 'prefix))
				       (and (not arg) org-use-fast-todo-selection
					    (not (eq org-use-fast-todo-selection
						     'prefix)))))
			      ;; Use fast selection
			      (org-fast-todo-selection))
			     ((and (equal arg '(4))
				   (or (not org-use-fast-todo-selection)
				       (not org-todo-key-trigger)))
			  ;; Read a state with completion
			  (org-icompleting-read
			   "State: " (mapcar (lambda(x) (list x))
					     org-todo-keywords-1)
			   nil t))
			 ((eq arg 'right)
			  (if this
			      (if tail (car tail) nil)
			    (car org-todo-keywords-1)))
			 ((eq arg 'left)
			  (if (equal member org-todo-keywords-1)
			      nil
			    (if this
				(nth (- (length org-todo-keywords-1)
					(length tail) 2)
				     org-todo-keywords-1)
			      (org-last org-todo-keywords-1))))
			 ((and (eq org-use-fast-todo-selection t) (equal arg '(4))
			       (setq arg nil))) ; hack to fall back to cycling
			 (arg
			  ;; user or caller requests a specific state
			  (cond
			   ((equal arg "") nil)
			   ((eq arg 'none) nil)
			   ((eq arg 'done) (or done-word (car org-done-keywords)))
			   ((eq arg 'nextset)
			    (or (car (cdr (member head org-todo-heads)))
				(car org-todo-heads)))
			   ((eq arg 'previousset)
			    (let ((org-todo-heads (reverse org-todo-heads)))
			      (or (car (cdr (member head org-todo-heads)))
				  (car org-todo-heads))))
			   ((car (member arg org-todo-keywords-1)))
			   ((stringp arg)
			    (error "State `%s' not valid in this file" arg))
			   ((nth (1- (prefix-numeric-value arg))
				 org-todo-keywords-1))))
			 ((null member) (or head (car org-todo-keywords-1)))
			 ((equal this final-done-word) nil) ;; -> make empty
			 ((null tail) nil) ;; -> first entry
			 ((memq interpret '(type priority))
			  (if (eq this-command last-command)
			      (car tail)
			    (if (> (length tail) 0)
				(or done-word (car org-done-keywords))
			      nil)))
			 (t
			  (car tail))))
		 (org-state (or
			     (run-hook-with-args-until-success
			      'org-todo-get-default-hook org-state org-last-state)
			     org-state))
		 (next (if org-state (concat " " org-state " ") " "))
		 (change-plist (list :type 'todo-state-change :from this :to org-state
				     :position startpos))
		 dolog now-done-p)
	    (when org-blocker-hook
	      (setq org-last-todo-state-is-todo
		    (not (member this org-done-keywords)))
	      (unless (save-excursion
			(save-match-data
			  (org-with-wide-buffer
			   (run-hook-with-args-until-failure
			    'org-blocker-hook change-plist))))
		(if (org-called-interactively-p 'interactive)
		    (error "TODO state change from %s to %s blocked" this org-state)
		  ;; fail silently
		  (message "TODO state change from %s to %s blocked" this org-state)
		  (throw 'exit nil))))
	    (store-match-data match-data)
	    (replace-match next t t)
	    (unless (pos-visible-in-window-p hl-pos)
	      (message "TODO state changed to %s" (org-trim next)))
	    (unless head
	      (setq head (org-get-todo-sequence-head org-state)
		    ass (assoc head org-todo-kwd-alist)
		    interpret (nth 1 ass)
		    done-word (nth 3 ass)
		    final-done-word (nth 4 ass)))
	    (when (memq arg '(nextset previousset))
	      (message "Keyword-Set %d/%d: %s"
		       (- (length org-todo-sets) -1
			  (length (memq (assoc org-state org-todo-sets) org-todo-sets)))
		       (length org-todo-sets)
		       (mapconcat 'identity (assoc org-state org-todo-sets) " ")))
	    (setq org-last-todo-state-is-todo
		  (not (member org-state org-done-keywords)))
	    (setq now-done-p (and (member org-state org-done-keywords)
				  (not (member this org-done-keywords))))
	    (and logging (org-local-logging logging))
	    (when (and (or org-todo-log-states org-log-done)
		       (not (eq org-inhibit-logging t))
		       (not (memq arg '(nextset previousset))))
	      ;; we need to look at recording a time and note
	      (setq dolog (or (nth 1 (assoc org-state org-todo-log-states))
			      (nth 2 (assoc this org-todo-log-states))))
	      (if (and (eq dolog 'note) (eq org-inhibit-logging 'note))
		  (setq dolog 'time))
	      (when (and org-state
			 (member org-state org-not-done-keywords)
			 (not (member this org-not-done-keywords)))
		;; This is now a todo state and was not one before
		;; If there was a CLOSED time stamp, get rid of it.
		(org-add-planning-info nil nil 'closed))
	      (when (and now-done-p org-log-done)
		;; It is now done, and it was not done before
		(org-add-planning-info 'closed (org-current-effective-time))
		(if (and (not dolog) (eq 'note org-log-done))
		    (org-add-log-setup 'done org-state this 'findpos 'note)))
	      (when (and org-state dolog)
		;; This is a non-nil state, and we need to log it
		(org-add-log-setup 'state org-state this 'findpos dolog)))
	    ;; Fixup tag positioning
	    (org-todo-trigger-tag-changes org-state)
	    (and org-auto-align-tags (not org-setting-tags) (org-set-tags nil t))
	    (when org-provide-todo-statistics
	      (org-update-parent-todo-statistics))
	    (run-hooks 'org-after-todo-state-change-hook)
	    (if (and arg (not (member org-state org-done-keywords)))
		(setq head (org-get-todo-sequence-head org-state)))
	    (put-text-property (point-at-bol) (point-at-eol) 'org-todo-head head)
	    ;; Do we need to trigger a repeat?
	    (when now-done-p
	      (when (boundp 'org-agenda-headline-snapshot-before-repeat)
		;; This is for the agenda, take a snapshot of the headline.
		(save-match-data
		  (setq org-agenda-headline-snapshot-before-repeat
			(org-get-heading))))
	      (org-auto-repeat-maybe org-state))
	    ;; Fixup cursor location if close to the keyword
	    (if (and (outline-on-heading-p)
		     (not (bolp))
		     (save-excursion (beginning-of-line 1)
				     (looking-at org-todo-line-regexp))
		     (< (point) (+ 2 (or (match-end 2) (match-end 1)))))
		(progn
		  (goto-char (or (match-end 2) (match-end 1)))
		  (and (looking-at " ") (just-one-space))))
	    (when org-trigger-hook
	      (save-excursion
		(run-hook-with-args 'org-trigger-hook change-plist)))))))))

(defun org-block-todo-from-children-or-siblings-or-parent (change-plist)
  "Block turning an entry into a TODO, using the hierarchy.
This checks whether the current task should be blocked from state
changes.  Such blocking occurs when:

  1. The task has children which are not all in a completed state.

  2. A task has a parent with the property :ORDERED:, and there
     are siblings prior to the current task with incomplete
     status.

  3. The parent of the task is blocked because it has siblings that should
     be done first, or is child of a block grandparent TODO entry."

  (if (not org-enforce-todo-dependencies)
      t ; if locally turned off don't block
    (catch 'dont-block
      ;; If this is not a todo state change, or if this entry is already DONE,
      ;; do not block
      (when (or (not (eq (plist-get change-plist :type) 'todo-state-change))
		(member (plist-get change-plist :from)
			(cons 'done org-done-keywords))
		(member (plist-get change-plist :to)
			(cons 'todo org-not-done-keywords))
		(not (plist-get change-plist :to)))
	(throw 'dont-block t))
      ;; If this task has children, and any are undone, it's blocked
      (save-excursion
	(org-back-to-heading t)
	(let ((this-level (funcall outline-level)))
	  (outline-next-heading)
	  (let ((child-level (funcall outline-level)))
	    (while (and (not (eobp))
			(> child-level this-level))
	      ;; this todo has children, check whether they are all
	      ;; completed
	      (if (and (not (org-entry-is-done-p))
		       (org-entry-is-todo-p))
		  (throw 'dont-block nil))
	      (outline-next-heading)
	      (setq child-level (funcall outline-level))))))
      ;; Otherwise, if the task's parent has the :ORDERED: property, and
      ;; any previous siblings are undone, it's blocked
      (save-excursion
	(org-back-to-heading t)
	(let* ((pos (point))
	       (parent-pos (and (org-up-heading-safe) (point))))
	  (if (not parent-pos) (throw 'dont-block t)) ; no parent
	  (when (and (org-not-nil (org-entry-get (point) "ORDERED"))
		     (forward-line 1)
		     (re-search-forward org-not-done-heading-regexp pos t))
	    (throw 'dont-block nil))  ; block, there is an older sibling not done.
	  ;; Search further up the hierarchy, to see if an ancestor is blocked
	  (while t
	    (goto-char parent-pos)
	    (if (not (looking-at org-not-done-heading-regexp))
		(throw 'dont-block t))	; do not block, parent is not a TODO
	    (setq pos (point))
	    (setq parent-pos (and (org-up-heading-safe) (point)))
	    (if (not parent-pos) (throw 'dont-block t)) ; no parent
	    (when (and (org-not-nil (org-entry-get (point) "ORDERED"))
		       (forward-line 1)
		       (re-search-forward org-not-done-heading-regexp pos t))
	      (throw 'dont-block nil)))))))) ; block, older sibling not done.

(defcustom org-track-ordered-property-with-tag nil
  "Should the ORDERED property also be shown as a tag?
The ORDERED property decides if an entry should require subtasks to be
completed in sequence.  Since a property is not very visible, setting
this option means that toggling the ORDERED property with the command
`org-toggle-ordered-property' will also toggle a tag ORDERED.  That tag is
not relevant for the behavior, but it makes things more visible.

Note that toggling the tag with tags commands will not change the property
and therefore not influence behavior!

This can be t, meaning the tag ORDERED should be used,  It can also be a
string to select a different tag for this task."
  :group 'org-todo
  :type '(choice
	  (const :tag "No tracking" nil)
	  (const :tag "Track with ORDERED tag" t)
	  (string :tag "Use other tag")))

(defun org-toggle-ordered-property ()
  "Toggle the ORDERED property of the current entry.
For better visibility, you can track the value of this property with a tag.
See variable `org-track-ordered-property-with-tag'."
  (interactive)
  (let* ((t1 org-track-ordered-property-with-tag)
	 (tag (and t1 (if (stringp t1) t1 "ORDERED"))))
    (save-excursion
      (org-back-to-heading)
      (if (org-entry-get nil "ORDERED")
	  (progn
	    (org-delete-property "ORDERED")
	    (and tag (org-toggle-tag tag 'off))
	    (message "Subtasks can be completed in arbitrary order"))
	(org-entry-put nil "ORDERED" "t")
	(and tag (org-toggle-tag tag 'on))
	(message "Subtasks must be completed in sequence")))))

(defvar org-blocked-by-checkboxes) ; dynamically scoped
(defun org-block-todo-from-checkboxes (change-plist)
  "Block turning an entry into a TODO, using checkboxes.
This checks whether the current task should be blocked from state
changes because there are unchecked boxes in this entry."
  (if (not org-enforce-todo-checkbox-dependencies)
      t ; if locally turned off don't block
    (catch 'dont-block
      ;; If this is not a todo state change, or if this entry is already DONE,
      ;; do not block
      (when (or (not (eq (plist-get change-plist :type) 'todo-state-change))
		(member (plist-get change-plist :from)
			(cons 'done org-done-keywords))
		(member (plist-get change-plist :to)
			(cons 'todo org-not-done-keywords))
		(not (plist-get change-plist :to)))
	(throw 'dont-block t))
      ;; If this task has checkboxes that are not checked, it's blocked
      (save-excursion
	(org-back-to-heading t)
	(let ((beg (point)) end)
	  (outline-next-heading)
	  (setq end (point))
	  (goto-char beg)
	  (if (org-list-search-forward
	       (concat (org-item-beginning-re)
		       "\\(?:\\[@\\(?:start:\\)?\\([0-9]+\\|[A-Za-z]\\)\\][ \t]*\\)?"
		       "\\[[- ]\\]")
	       end t)
	      (progn
		(if (boundp 'org-blocked-by-checkboxes)
		    (setq org-blocked-by-checkboxes t))
		(throw 'dont-block nil)))))
      t))) ; do not block

(defun org-entry-blocked-p ()
  "Is the current entry blocked?"
  (if (org-entry-get nil "NOBLOCKING")
      nil ;; Never block this entry
    (not
     (run-hook-with-args-until-failure
      'org-blocker-hook
      (list :type 'todo-state-change
	    :position (point)
	    :from 'todo
	    :to 'done)))))

(defun org-update-statistics-cookies (all)
  "Update the statistics cookie, either from TODO or from checkboxes.
This should be called with the cursor in a line with a statistics cookie."
  (interactive "P")
  (if all
      (progn
	(org-update-checkbox-count 'all)
	(org-map-entries 'org-update-parent-todo-statistics))
    (if (not (org-at-heading-p))
	(org-update-checkbox-count)
      (let ((pos (move-marker (make-marker) (point)))
	    end l1 l2)
	(ignore-errors (org-back-to-heading t))
	(if (not (org-at-heading-p))
	    (org-update-checkbox-count)
	  (setq l1 (org-outline-level))
	  (setq end (save-excursion
		      (outline-next-heading)
		      (if (org-at-heading-p) (setq l2 (org-outline-level)))
		      (point)))
	  (if (and (save-excursion
		     (re-search-forward
		      "^[ \t]*\\([-+*]\\|[0-9]+[.)]\\) \\[[- X]\\]" end t))
		   (not (save-excursion (re-search-forward
					 ":COOKIE_DATA:.*\\<todo\\>" end t))))
	      (org-update-checkbox-count)
	    (if (and l2 (> l2 l1))
		(progn
		  (goto-char end)
		  (org-update-parent-todo-statistics))
	      (goto-char pos)
	      (beginning-of-line 1)
	      (while (re-search-forward
		      "\\(\\(\\[[0-9]*%\\]\\)\\|\\(\\[[0-9]*/[0-9]*\\]\\)\\)"
		      (point-at-eol) t)
		(replace-match (if (match-end 2) "[100%]" "[0/0]") t t)))))
	(goto-char pos)
	(move-marker pos nil)))))

(defvar org-entry-property-inherited-from) ;; defined below
(defun org-update-parent-todo-statistics ()
  "Update any statistics cookie in the parent of the current headline.
When `org-hierarchical-todo-statistics' is nil, statistics will cover
the entire subtree and this will travel up the hierarchy and update
statistics everywhere."
  (let* ((prop (save-excursion (org-up-heading-safe)
			       (org-entry-get nil "COOKIE_DATA" 'inherit)))
	 (recursive (or (not org-hierarchical-todo-statistics)
			(and prop (string-match "\\<recursive\\>" prop))))
	 (lim (or (and prop (marker-position org-entry-property-inherited-from))
		  0))
	 (first t)
	 (box-re "\\(\\(\\[[0-9]*%\\]\\)\\|\\(\\[[0-9]*/[0-9]*\\]\\)\\)")
	 level ltoggle l1 new ndel
	 (cnt-all 0) (cnt-done 0) is-percent kwd
	 checkbox-beg ov ovs ove cookie-present)
    (catch 'exit
      (save-excursion
	(beginning-of-line 1)
	(setq ltoggle (funcall outline-level))
	;; Three situations are to consider:

	;; 1. if `org-hierarchical-todo-statistics' is nil, repeat up
	;;    to the top-level ancestor on the headline;

	;; 2. If parent has "recursive" property, repeat up to the
	;;    headline setting that property, taking inheritance into
	;;    account;

	;; 3. Else, move up to direct parent and proceed only once.
	(while (and (setq level (org-up-heading-safe))
		    (or recursive first)
		    (>= (point) lim))
	  (setq first nil cookie-present nil)
	  (unless (and level
		       (not (string-match
			     "\\<checkbox\\>"
			     (downcase (or (org-entry-get nil "COOKIE_DATA")
					   "")))))
	    (throw 'exit nil))
	  (while (re-search-forward box-re (point-at-eol) t)
	    (setq cnt-all 0 cnt-done 0 cookie-present t)
	    (setq is-percent (match-end 2) checkbox-beg (match-beginning 0))
	    (save-match-data
	      (unless (outline-next-heading) (throw 'exit nil))
	      (while (and (looking-at org-complex-heading-regexp)
	    		  (> (setq l1 (length (match-string 1))) level))
	    	(setq kwd (and (or recursive (= l1 ltoggle))
	    		       (match-string 2)))
	    	(if (or (eq org-provide-todo-statistics 'all-headlines)
	    		(and (listp org-provide-todo-statistics)
	    		     (or (member kwd org-provide-todo-statistics)
	    			 (member kwd org-done-keywords))))
	    	    (setq cnt-all (1+ cnt-all))
	    	  (if (eq org-provide-todo-statistics t)
	    	      (and kwd (setq cnt-all (1+ cnt-all)))))
	    	(and (member kwd org-done-keywords)
	    	     (setq cnt-done (1+ cnt-done)))
	    	(outline-next-heading)))
	    (setq new
	    	  (if is-percent
	    	      (format "[%d%%]" (/ (* 100 cnt-done) (max 1 cnt-all)))
	    	    (format "[%d/%d]" cnt-done cnt-all))
	    	  ndel (- (match-end 0) checkbox-beg))
	    ;; handle overlays when updating cookie from column view
	    (when (setq ov (car (overlays-at checkbox-beg)))
	      (setq ovs (overlay-start ov) ove (overlay-end ov))
	      (delete-overlay ov))
	    (goto-char checkbox-beg)
	    (insert new)
	    (delete-region (point) (+ (point) ndel))
	    (when org-auto-align-tags (org-fix-tags-on-the-fly))
	    (when ov (move-overlay ov ovs ove)))
	  (when cookie-present
	    (run-hook-with-args 'org-after-todo-statistics-hook
				cnt-done (- cnt-all cnt-done))))))
    (run-hooks 'org-todo-statistics-hook)))

(defvar org-after-todo-statistics-hook nil
  "Hook that is called after a TODO statistics cookie has been updated.
Each function is called with two arguments: the number of not-done entries
and the number of done entries.

For example, the following function, when added to this hook, will switch
an entry to DONE when all children are done, and back to TODO when new
entries are set to a TODO status.  Note that this hook is only called
when there is a statistics cookie in the headline!

 (defun org-summary-todo (n-done n-not-done)
   \"Switch entry to DONE when all subentries are done, to TODO otherwise.\"
   (let (org-log-done org-log-states)   ; turn off logging
     (org-todo (if (= n-not-done 0) \"DONE\" \"TODO\"))))
")

(defvar org-todo-statistics-hook nil
  "Hook that is run whenever Org thinks TODO statistics should be updated.
This hook runs even if there is no statistics cookie present, in which case
`org-after-todo-statistics-hook' would not run.")

(defun org-todo-trigger-tag-changes (state)
  "Apply the changes defined in `org-todo-state-tags-triggers'."
  (let ((l org-todo-state-tags-triggers)
	changes)
    (when (or (not state) (equal state ""))
      (setq changes (append changes (cdr (assoc "" l)))))
    (when (and (stringp state) (> (length state) 0))
      (setq changes (append changes (cdr (assoc state l)))))
    (when (member state org-not-done-keywords)
      (setq changes (append changes (cdr (assoc 'todo l)))))
    (when (member state org-done-keywords)
      (setq changes (append changes (cdr (assoc 'done l)))))
    (dolist (c changes)
      (org-toggle-tag (car c) (if (cdr c) 'on 'off)))))

(defun org-local-logging (value)
  "Get logging settings from a property VALUE."
  (let* (words w a)
    ;; directly set the variables, they are already local.
    (setq org-log-done nil
	  org-log-repeat nil
	  org-todo-log-states nil)
    (setq words (org-split-string value))
    (while (setq w (pop words))
      (cond
       ((setq a (assoc w org-startup-options))
	(and (member (nth 1 a) '(org-log-done org-log-repeat))
	     (set (nth 1 a) (nth 2 a))))
       ((setq a (org-extract-log-state-settings w))
	(and (member (car a) org-todo-keywords-1)
	     (push a org-todo-log-states)))))))

(defun org-get-todo-sequence-head (kwd)
  "Return the head of the TODO sequence to which KWD belongs.
If KWD is not set, check if there is a text property remembering the
right sequence."
  (let (p)
    (cond
     ((not kwd)
      (or (get-text-property (point-at-bol) 'org-todo-head)
	  (progn
	    (setq p (next-single-property-change (point-at-bol) 'org-todo-head
						 nil (point-at-eol)))
	    (get-text-property p 'org-todo-head))))
     ((not (member kwd org-todo-keywords-1))
      (car org-todo-keywords-1))
     (t (nth 2 (assoc kwd org-todo-kwd-alist))))))

(defun org-fast-todo-selection ()
  "Fast TODO keyword selection with single keys.
Returns the new TODO keyword, or nil if no state change should occur."
  (let* ((fulltable org-todo-key-alist)
	 (done-keywords org-done-keywords) ;; needed for the faces.
	 (maxlen (apply 'max (mapcar
			      (lambda (x)
				(if (stringp (car x)) (string-width (car x)) 0))
			      fulltable)))
	 (expert nil)
	 (fwidth (+ maxlen 3 1 3))
	 (ncol (/ (- (window-width) 4) fwidth))
	 tg cnt e c tbl
	 groups ingroup)
    (save-excursion
      (save-window-excursion
	(if expert
	    (set-buffer (get-buffer-create " *Org todo*"))
	  (org-switch-to-buffer-other-window (get-buffer-create " *Org todo*")))
	(erase-buffer)
	(org-set-local 'org-done-keywords done-keywords)
	(setq tbl fulltable cnt 0)
	(while (setq e (pop tbl))
	  (cond
	   ((equal e '(:startgroup))
	    (push '() groups) (setq ingroup t)
	    (when (not (= cnt 0))
	      (setq cnt 0)
	      (insert "\n"))
	    (insert "{ "))
	   ((equal e '(:endgroup))
	    (setq ingroup nil cnt 0)
	    (insert "}\n"))
	   ((equal e '(:newline))
	    (when (not (= cnt 0))
	      (setq cnt 0)
	      (insert "\n")
	      (setq e (car tbl))
	      (while (equal (car tbl) '(:newline))
		(insert "\n")
		(setq tbl (cdr tbl)))))
	   (t
	    (setq tg (car e) c (cdr e))
	    (if ingroup (push tg (car groups)))
	    (setq tg (org-add-props tg nil 'face
				    (org-get-todo-face tg)))
	    (if (and (= cnt 0) (not ingroup)) (insert "  "))
	    (insert "[" c "] " tg (make-string
				   (- fwidth 4 (length tg)) ?\ ))
	    (when (= (setq cnt (1+ cnt)) ncol)
	      (insert "\n")
	      (if ingroup (insert "  "))
	      (setq cnt 0)))))
	(insert "\n")
	(goto-char (point-min))
	(if (not expert) (org-fit-window-to-buffer))
	(message "[a-z..]:Set [SPC]:clear")
	(setq c (let ((inhibit-quit t)) (read-char-exclusive)))
	(cond
	 ((or (= c ?\C-g)
	      (and (= c ?q) (not (rassoc c fulltable))))
	  (setq quit-flag t))
	 ((= c ?\ ) nil)
	 ((setq e (rassoc c fulltable) tg (car e))
	  tg)
	 (t (setq quit-flag t)))))))

(defun org-entry-is-todo-p ()
  (member (org-get-todo-state) org-not-done-keywords))

(defun org-entry-is-done-p ()
  (member (org-get-todo-state) org-done-keywords))

(defun org-get-todo-state ()
  (save-excursion
    (org-back-to-heading t)
    (and (looking-at org-todo-line-regexp)
	 (match-end 2)
	 (match-string 2))))

(defun org-at-date-range-p (&optional inactive-ok)
  "Is the cursor inside a date range?"
  (interactive)
  (save-excursion
    (catch 'exit
      (let ((pos (point)))
	(skip-chars-backward "^[<\r\n")
	(skip-chars-backward "<[")
	(and (looking-at (if inactive-ok org-tr-regexp-both org-tr-regexp))
	     (>= (match-end 0) pos)
	     (throw 'exit t))
	(skip-chars-backward "^<[\r\n")
	(skip-chars-backward "<[")
	(and (looking-at (if inactive-ok org-tr-regexp-both org-tr-regexp))
	     (>= (match-end 0) pos)
	     (throw 'exit t)))
      nil)))

(defun org-get-repeat (&optional tagline)
  "Check if there is a deadline/schedule with repeater in this entry."
  (save-match-data
    (save-excursion
      (org-back-to-heading t)
      (and (re-search-forward (if tagline
				  (concat tagline "\\s-*" org-repeat-re)
				org-repeat-re)
			      (org-entry-end-position) t)
	   (match-string-no-properties 1)))))

(defvar org-last-changed-timestamp)
(defvar org-last-inserted-timestamp)
(defvar org-log-post-message)
(defvar org-log-note-purpose)
(defvar org-log-note-how)
(defvar org-log-note-extra)
(defun org-auto-repeat-maybe (done-word)
  "Check if the current headline contains a repeated deadline/schedule.
If yes, set TODO state back to what it was and change the base date
of repeating deadline/scheduled time stamps to new date.
This function is run automatically after each state change to a DONE state."
  ;; last-state is dynamically scoped into this function
  (let* ((repeat (org-get-repeat))
	 (aa (assoc org-last-state org-todo-kwd-alist))
	 (interpret (nth 1 aa))
	 (head (nth 2 aa))
	 (whata '(("d" . day) ("m" . month) ("y" . year)))
	 (msg "Entry repeats: ")
	 (org-log-done nil)
	 (org-todo-log-states nil)
	 re type n what ts time to-state)
    (when repeat
      (if (eq org-log-repeat t) (setq org-log-repeat 'state))
      (setq to-state (or (org-entry-get nil "REPEAT_TO_STATE")
			 org-todo-repeat-to-state))
      (unless (and to-state (member to-state org-todo-keywords-1))
	(setq to-state (if (eq interpret 'type) org-last-state head)))
      (org-todo to-state)
      (when (or org-log-repeat (org-entry-get nil "CLOCK"))
	(org-entry-put nil "LAST_REPEAT" (format-time-string
					  (org-time-stamp-format t t))))
      (when org-log-repeat
	(if (or (memq 'org-add-log-note (default-value 'post-command-hook))
		(memq 'org-add-log-note post-command-hook))
	    ;; OK, we are already setup for some record
	    (if (eq org-log-repeat 'note)
		;; make sure we take a note, not only a time stamp
		(setq org-log-note-how 'note))
	  ;; Set up for taking a record
	  (org-add-log-setup 'state (or done-word (car org-done-keywords))
			     org-last-state
			     'findpos org-log-repeat)))
      (org-back-to-heading t)
      (org-add-planning-info nil nil 'closed)
      (setq re (concat "\\(" org-scheduled-time-regexp "\\)\\|\\("
		       org-deadline-time-regexp "\\)\\|\\("
		       org-ts-regexp "\\)"))
      (while (re-search-forward
	      re (save-excursion (outline-next-heading) (point)) t)
	(setq type (if (match-end 1) org-scheduled-string
		     (if (match-end 3) org-deadline-string "Plain:"))
	      ts (match-string (if (match-end 2) 2 (if (match-end 4) 4 0))))
	(when (string-match "\\([.+]\\)?\\(\\+[0-9]+\\)\\([dwmy]\\)" ts)
	  (setq	n (string-to-number (match-string 2 ts))
		what (match-string 3 ts))
	  (if (equal what "w") (setq n (* n 7) what "d"))
	  ;; Preparation, see if we need to modify the start date for the change
	  (when (match-end 1)
	    (setq time (save-match-data (org-time-string-to-time ts)))
	    (cond
	     ((equal (match-string 1 ts) ".")
	      ;; Shift starting date to today
	      (org-timestamp-change
	       (- (org-today) (time-to-days time))
	       'day))
	     ((equal (match-string 1 ts) "+")
	      (let ((nshiftmax 10) (nshift 0))
		(while (or (= nshift 0)
			   (<= (time-to-days time)
			       (time-to-days (current-time))))
		  (when (= (incf nshift) nshiftmax)
		    (or (y-or-n-p (message "%d repeater intervals were not enough to shift date past today.  Continue? " nshift))
			(error "Abort")))
		  (org-timestamp-change n (cdr (assoc what whata)))
		  (org-at-timestamp-p t)
		  (setq ts (match-string 1))
		  (setq time (save-match-data (org-time-string-to-time ts)))))
	      (org-timestamp-change (- n) (cdr (assoc what whata)))
	      ;; rematch, so that we have everything in place for the real shift
	      (org-at-timestamp-p t)
	      (setq ts (match-string 1))
	      (string-match "\\([.+]\\)?\\(\\+[0-9]+\\)\\([dwmy]\\)" ts))))
	  (org-timestamp-change n (cdr (assoc what whata)))
	  (setq msg (concat msg type " " org-last-changed-timestamp " "))))
      (setq org-log-post-message msg)
      (message "%s" msg))))

(defun org-show-todo-tree (arg)
  "Make a compact tree which shows all headlines marked with TODO.
The tree will show the lines where the regexp matches, and all higher
headlines above the match.
With a \\[universal-argument] prefix, prompt for a regexp to match.
With a numeric prefix N, construct a sparse tree for the Nth element
of `org-todo-keywords-1'."
  (interactive "P")
  (let ((case-fold-search nil)
	(kwd-re
	 (cond ((null arg) org-not-done-regexp)
	       ((equal arg '(4))
		(let ((kwd (org-icompleting-read "Keyword (or KWD1|KWD2|...): "
					    (mapcar 'list org-todo-keywords-1))))
		  (concat "\\("
			  (mapconcat 'identity (org-split-string kwd "|") "\\|")
			  "\\)\\>")))
	       ((<= (prefix-numeric-value arg) (length org-todo-keywords-1))
		(regexp-quote (nth (1- (prefix-numeric-value arg))
				   org-todo-keywords-1)))
	       (t (error "Invalid prefix argument: %s" arg)))))
    (message "%d TODO entries found"
	     (org-occur (concat "^" org-outline-regexp " *" kwd-re )))))

(defun org-deadline (&optional remove time)
  "Insert the \"DEADLINE:\" string with a timestamp to make a deadline.
With argument REMOVE, remove any deadline from the item.
With argument TIME, set the deadline at the corresponding date.  TIME
can either be an Org date like \"2011-07-24\" or a delta like \"+2d\"."
  (interactive "P")
  (if (and (org-region-active-p) org-loop-over-headlines-in-active-region)
      (let ((cl (if (eq org-loop-over-headlines-in-active-region 'start-level)
		    'region-start-level 'region))
	    org-loop-over-headlines-in-active-region)
	(org-map-entries
	 `(org-deadline ',remove ,time)
	 org-loop-over-headlines-in-active-region
	 cl (if (outline-invisible-p) (org-end-of-subtree nil t))))
    (let* ((old-date (org-entry-get nil "DEADLINE"))
	   (repeater (and old-date
			  (string-match
			   "\\([.+-]+[0-9]+[dwmy]\\(?:[/ ][-+]?[0-9]+[dwmy]\\)?\\) ?"
			   old-date)
			  (match-string 1 old-date))))
      (if remove
	  (progn
	    (when (and old-date org-log-redeadline)
	      (org-add-log-setup 'deldeadline nil old-date 'findpos
				 org-log-redeadline))
	    (org-remove-timestamp-with-keyword org-deadline-string)
	    (message "Item no longer has a deadline."))
	(org-add-planning-info 'deadline time 'closed)
	(when (and old-date org-log-redeadline
		   (not (equal old-date
			       (substring org-last-inserted-timestamp 1 -1))))
	  (org-add-log-setup 'redeadline nil old-date 'findpos
			     org-log-redeadline))
	(when repeater
	  (save-excursion
	    (org-back-to-heading t)
	    (when (re-search-forward (concat org-deadline-string " "
					     org-last-inserted-timestamp)
				     (save-excursion
				       (outline-next-heading) (point)) t)
	      (goto-char (1- (match-end 0)))
	      (insert " " repeater)
	      (setq org-last-inserted-timestamp
		    (concat (substring org-last-inserted-timestamp 0 -1)
			    " " repeater
			    (substring org-last-inserted-timestamp -1))))))
	(message "Deadline on %s" org-last-inserted-timestamp)))))

(defun org-schedule (&optional remove time)
  "Insert the SCHEDULED: string with a timestamp to schedule a TODO item.
With argument REMOVE, remove any scheduling date from the item.
With argument TIME, scheduled at the corresponding date.  TIME can
either be an Org date like \"2011-07-24\" or a delta like \"+2d\"."
  (interactive "P")
  (if (and (org-region-active-p) org-loop-over-headlines-in-active-region)
      (let ((cl (if (eq org-loop-over-headlines-in-active-region 'start-level)
		    'region-start-level 'region))
	    org-loop-over-headlines-in-active-region)
	(org-map-entries
	 `(org-schedule ',remove ,time)
	 org-loop-over-headlines-in-active-region
	 cl (if (outline-invisible-p) (org-end-of-subtree nil t))))
    (let* ((old-date (org-entry-get nil "SCHEDULED"))
	   (repeater (and old-date
			  (string-match
			   "\\([.+-]+[0-9]+[dwmy]\\(?:[/ ][-+]?[0-9]+[dwmy]\\)?\\) ?"
			   old-date)
			  (match-string 1 old-date))))
      (if remove
	  (progn
	    (when (and old-date org-log-reschedule)
	      (org-add-log-setup 'delschedule nil old-date 'findpos
				 org-log-reschedule))
	    (org-remove-timestamp-with-keyword org-scheduled-string)
	    (message "Item is no longer scheduled."))
	(org-add-planning-info 'scheduled time 'closed)
	(when (and old-date org-log-reschedule
		   (not (equal old-date
			       (substring org-last-inserted-timestamp 1 -1))))
	  (org-add-log-setup 'reschedule nil old-date 'findpos
			     org-log-reschedule))
	(when repeater
	  (save-excursion
	    (org-back-to-heading t)
	    (when (re-search-forward (concat org-scheduled-string " "
					     org-last-inserted-timestamp)
				     (save-excursion
				       (outline-next-heading) (point)) t)
	      (goto-char (1- (match-end 0)))
	      (insert " " repeater)
	      (setq org-last-inserted-timestamp
		    (concat (substring org-last-inserted-timestamp 0 -1)
			    " " repeater
			    (substring org-last-inserted-timestamp -1))))))
	(message "Scheduled to %s" org-last-inserted-timestamp)))))

(defun org-get-scheduled-time (pom &optional inherit)
  "Get the scheduled time as a time tuple, of a format suitable
for calling org-schedule with, or if there is no scheduling,
returns nil."
  (let ((time (org-entry-get pom "SCHEDULED" inherit)))
    (when time
      (apply 'encode-time (org-parse-time-string time)))))

(defun org-get-deadline-time (pom &optional inherit)
  "Get the deadline as a time tuple, of a format suitable for
calling org-deadline with, or if there is no scheduling, returns
nil."
  (let ((time (org-entry-get pom "DEADLINE" inherit)))
    (when time
      (apply 'encode-time (org-parse-time-string time)))))

(defun org-remove-timestamp-with-keyword (keyword)
  "Remove all time stamps with KEYWORD in the current entry."
  (let ((re (concat "\\<" (regexp-quote keyword) " +<[^>\n]+>[ \t]*"))
	beg)
    (save-excursion
      (org-back-to-heading t)
      (setq beg (point))
      (outline-next-heading)
      (while (re-search-backward re beg t)
	(replace-match "")
	(if (and (string-match "\\S-" (buffer-substring (point-at-bol) (point)))
		 (equal (char-before) ?\ ))
	    (backward-delete-char 1)
	  (if (string-match "^[ \t]*$" (buffer-substring
					(point-at-bol) (point-at-eol)))
	      (delete-region (point-at-bol)
			     (min (point-max) (1+ (point-at-eol))))))))))

(defun org-add-planning-info (what &optional time &rest remove)
  "Insert new timestamp with keyword in the line directly after the headline.
WHAT indicates what kind of time stamp to add.  TIME indicates the time to use.
If non is given, the user is prompted for a date.
REMOVE indicates what kind of entries to remove.  An old WHAT entry will also
be removed."
  (interactive)
  (let (org-time-was-given org-end-time-was-given ts
			   end default-time default-input)

    (catch 'exit
      (when (and (memq what '(scheduled deadline))
		 (or (not time)
		     (and (stringp time)
			  (string-match "^[-+]+[0-9]" time))))
	;; Try to get a default date/time from existing timestamp
	(save-excursion
	  (org-back-to-heading t)
	  (setq end (save-excursion (outline-next-heading) (point)))
	  (when (re-search-forward (if (eq what 'scheduled)
				       org-scheduled-time-regexp
				     org-deadline-time-regexp)
				   end t)
	    (setq ts (match-string 1)
		  default-time
		  (apply 'encode-time (org-parse-time-string ts))
		  default-input (and ts (org-get-compact-tod ts))))))
      (when what
	(setq time
	      (if (stringp time)
		  ;; This is a string (relative or absolute), set proper date
		  (apply 'encode-time
			 (org-read-date-analyze
			  time default-time (decode-time default-time)))
		;; If necessary, get the time from the user
		(or time (org-read-date nil 'to-time nil nil
					default-time default-input)))))

      (when (and org-insert-labeled-timestamps-at-point
		 (member what '(scheduled deadline)))
	(insert
	 (if (eq what 'scheduled) org-scheduled-string org-deadline-string) " ")
	(org-insert-time-stamp time org-time-was-given
			       nil nil nil (list org-end-time-was-given))
	(setq what nil))
      (save-excursion
	(save-restriction
	  (let (col list elt ts buffer-invisibility-spec)
	    (org-back-to-heading t)
	    (looking-at (concat org-outline-regexp "\\( *\\)[^\r\n]*"))
	    (goto-char (match-end 1))
	    (setq col (current-column))
	    (goto-char (match-end 0))
	    (if (eobp) (insert "\n") (forward-char 1))
	    (when (and (not what)
		       (not (looking-at
			     (concat "[ \t]*"
				     org-keyword-time-not-clock-regexp))))
	      ;; Nothing to add, nothing to remove...... :-)
	      (throw 'exit nil))
	    (if (and (not (looking-at org-outline-regexp))
		     (looking-at (concat "[^\r\n]*?" org-keyword-time-regexp
					 "[^\r\n]*"))
		     (not (equal (match-string 1) org-clock-string)))
		(narrow-to-region (match-beginning 0) (match-end 0))
	      (insert-before-markers "\n")
	      (backward-char 1)
	      (narrow-to-region (point) (point))
	      (and org-adapt-indentation (org-indent-to-column col)))
	    ;; Check if we have to remove something.
	    (setq list (cons what remove))
	    (while list
	      (setq elt (pop list))
	      (when (or (and (eq elt 'scheduled)
			     (re-search-forward org-scheduled-time-regexp nil t))
			(and (eq elt 'deadline)
			     (re-search-forward org-deadline-time-regexp nil t))
			(and (eq elt 'closed)
			     (re-search-forward org-closed-time-regexp nil t)))
		(replace-match "")
		(if (looking-at "--+<[^>]+>") (replace-match ""))))
	    (and (looking-at "[ \t]+") (replace-match ""))
	    (and org-adapt-indentation (bolp) (org-indent-to-column col))
	    (when what
	      (insert
	       (if (not (or (bolp) (eq (char-before) ?\ ))) " " "")
	       (cond ((eq what 'scheduled) org-scheduled-string)
		     ((eq what 'deadline) org-deadline-string)
		     ((eq what 'closed) org-closed-string))
	       " ")
	      (setq ts (org-insert-time-stamp
			time
			(or org-time-was-given
			    (and (eq what 'closed) org-log-done-with-time))
			(eq what 'closed)
			nil nil (list org-end-time-was-given)))
	      (insert
	       (if (not (or (bolp) (eq (char-before) ?\ )
			    (memq (char-after) '(32 10))
			    (eobp))) " " ""))
	      (end-of-line 1))
	    (goto-char (point-min))
	    (widen)
	    (if (and (looking-at "[ \t]*\n")
		     (equal (char-before) ?\n))
		(delete-region (1- (point)) (point-at-eol)))
	    ts))))))

(defvar org-log-note-marker (make-marker))
(defvar org-log-note-purpose nil)
(defvar org-log-note-state nil)
(defvar org-log-note-previous-state nil)
(defvar org-log-note-how nil)
(defvar org-log-note-extra nil)
(defvar org-log-note-window-configuration nil)
(defvar org-log-note-return-to (make-marker))
(defvar org-log-note-effective-time nil
  "Remembered current time so that dynamically scoped
`org-extend-today-until' affects tha timestamps in state change
log")

(defvar org-log-post-message nil
  "Message to be displayed after a log note has been stored.
The auto-repeater uses this.")

(defun org-add-note ()
  "Add a note to the current entry.
This is done in the same way as adding a state change note."
  (interactive)
  (org-add-log-setup 'note nil nil 'findpos nil))

(defvar org-property-end-re)
(defun org-add-log-setup (&optional purpose state prev-state
				    findpos how extra)
  "Set up the post command hook to take a note.
If this is about to TODO state change, the new state is expected in STATE.
When FINDPOS is non-nil, find the correct position for the note in
the current entry.  If not, assume that it can be inserted at point.
HOW is an indicator what kind of note should be created.
EXTRA is additional text that will be inserted into the notes buffer."
  (let* ((org-log-into-drawer (org-log-into-drawer))
	 (drawer (cond ((stringp org-log-into-drawer)
			org-log-into-drawer)
		       (org-log-into-drawer "LOGBOOK")
		       (t nil))))
    (save-restriction
      (save-excursion
	(when findpos
	  (org-back-to-heading t)
	  (narrow-to-region (point) (save-excursion
				      (outline-next-heading) (point)))
	  (looking-at (concat org-outline-regexp "\\( *\\)[^\r\n]*"
			      "\\(\n[^\r\n]*?" org-keyword-time-not-clock-regexp
			      "[^\r\n]*\\)?"))
	  (goto-char (match-end 0))
	  (cond
	   (drawer
	    (if (re-search-forward (concat "^[ \t]*:" drawer ":[ \t]*$")
				   nil t)
		(progn
		  (goto-char (match-end 0))
		  (or org-log-states-order-reversed
		      (and (re-search-forward org-property-end-re nil t)
			   (goto-char (1- (match-beginning 0))))))
	      (insert "\n:" drawer ":\n:END:")
	      (beginning-of-line 0)
	      (org-indent-line-function)
	      (beginning-of-line 2)
	      (org-indent-line-function)
	      (end-of-line 0)))
	   ((and org-log-state-notes-insert-after-drawers
		 (save-excursion
		   (forward-line) (looking-at org-drawer-regexp)))
	    (forward-line)
	    (while (looking-at org-drawer-regexp)
	      (goto-char (match-end 0))
	      (re-search-forward org-property-end-re (point-max) t)
	      (forward-line))
	    (forward-line -1)))
	  (unless org-log-states-order-reversed
	    (and (= (char-after) ?\n) (forward-char 1))
	    (org-skip-over-state-notes)
	    (skip-chars-backward " \t\n\r")))
	(move-marker org-log-note-marker (point))
	(setq org-log-note-purpose purpose
	      org-log-note-state state
	      org-log-note-previous-state prev-state
	      org-log-note-how how
	      org-log-note-extra extra
	      org-log-note-effective-time (org-current-effective-time))
	(add-hook 'post-command-hook 'org-add-log-note 'append)))))

(defun org-skip-over-state-notes ()
  "Skip past the list of State notes in an entry."
  (if (looking-at "\n[ \t]*- State") (forward-char 1))
  (when (ignore-errors (goto-char (org-in-item-p)))
    (let* ((struct (org-list-struct))
	   (prevs (org-list-prevs-alist struct)))
      (while (looking-at "[ \t]*- State")
	(goto-char (or (org-list-get-next-item (point) struct prevs)
		       (org-list-get-item-end (point) struct)))))))

(defun org-add-log-note (&optional purpose)
  "Pop up a window for taking a note, and add this note later at point."
  (remove-hook 'post-command-hook 'org-add-log-note)
  (setq org-log-note-window-configuration (current-window-configuration))
  (delete-other-windows)
  (move-marker org-log-note-return-to (point))
  (org-pop-to-buffer-same-window (marker-buffer org-log-note-marker))
  (goto-char org-log-note-marker)
  (org-switch-to-buffer-other-window "*Org Note*")
  (erase-buffer)
  (if (memq org-log-note-how '(time state))
      (let (current-prefix-arg)	(org-store-log-note))
    (let ((org-inhibit-startup t)) (org-mode))
    (insert (format "# Insert note for %s.
# Finish with C-c C-c, or cancel with C-c C-k.\n\n"
		    (cond
		     ((eq org-log-note-purpose 'clock-out) "stopped clock")
		     ((eq org-log-note-purpose 'done)  "closed todo item")
		     ((eq org-log-note-purpose 'state)
		      (format "state change from \"%s\" to \"%s\""
			      (or org-log-note-previous-state "")
			      (or org-log-note-state "")))
		     ((eq org-log-note-purpose 'reschedule)
		      "rescheduling")
		     ((eq org-log-note-purpose 'delschedule)
		      "no longer scheduled")
		     ((eq org-log-note-purpose 'redeadline)
		      "changing deadline")
		     ((eq org-log-note-purpose 'deldeadline)
		      "removing deadline")
		     ((eq org-log-note-purpose 'refile)
		      "refiling")
		     ((eq org-log-note-purpose 'note)
		      "this entry")
		     (t (error "This should not happen")))))
    (if org-log-note-extra (insert org-log-note-extra))
    (org-set-local 'org-finish-function 'org-store-log-note)
    (run-hooks 'org-log-buffer-setup-hook)))

(defvar org-note-abort nil) ; dynamically scoped
(defun org-store-log-note ()
  "Finish taking a log note, and insert it to where it belongs."
  (let ((txt (buffer-string))
	(note (cdr (assq org-log-note-purpose org-log-note-headings)))
	lines ind bul)
    (kill-buffer (current-buffer))
    (while (string-match "\\`#.*\n[ \t\n]*" txt)
      (setq txt (replace-match "" t t txt)))
    (if (string-match "\\s-+\\'" txt)
	(setq txt (replace-match "" t t txt)))
    (setq lines (org-split-string txt "\n"))
    (when (and note (string-match "\\S-" note))
      (setq note
	    (org-replace-escapes
	     note
	     (list (cons "%u" (user-login-name))
		   (cons "%U" user-full-name)
		   (cons "%t" (format-time-string
			       (org-time-stamp-format 'long 'inactive)
			       org-log-note-effective-time))
		   (cons "%T" (format-time-string
			       (org-time-stamp-format 'long nil)
			       org-log-note-effective-time))
		   (cons "%d" (format-time-string
			       (org-time-stamp-format nil 'inactive)
			       org-log-note-effective-time))
		   (cons "%D" (format-time-string
			       (org-time-stamp-format nil nil)
			       org-log-note-effective-time))
		   (cons "%s" (if org-log-note-state
				  (concat "\"" org-log-note-state "\"")
				""))
		   (cons "%S" (if org-log-note-previous-state
				  (concat "\"" org-log-note-previous-state "\"")
				"\"\"")))))
      (if lines (setq note (concat note " \\\\")))
      (push note lines))
    (when (or current-prefix-arg org-note-abort)
      (when org-log-into-drawer
	(org-remove-empty-drawer-at
	 (if (stringp org-log-into-drawer) org-log-into-drawer "LOGBOOK")
	 org-log-note-marker))
      (setq lines nil))
    (when lines
      (with-current-buffer (marker-buffer org-log-note-marker)
	(save-excursion
	  (goto-char org-log-note-marker)
	  (move-marker org-log-note-marker nil)
	  (end-of-line 1)
	  (if (not (bolp)) (let ((inhibit-read-only t)) (insert "\n")))
	  (setq ind (save-excursion
		      (if (ignore-errors (goto-char (org-in-item-p)))
			  (let ((struct (org-list-struct)))
			    (org-list-get-ind
			     (org-list-get-top-point struct) struct))
			(skip-chars-backward " \r\t\n")
			(cond
			 ((and (org-at-heading-p)
			       org-adapt-indentation)
			  (1+ (org-current-level)))
			 ((org-at-heading-p) 0)
			 (t (org-get-indentation))))))
	  (setq bul (org-list-bullet-string "-"))
	  (org-indent-line-to ind)
	  (insert bul (pop lines))
	  (let ((ind-body (+ (length bul) ind)))
	    (while lines
	      (insert "\n")
	      (org-indent-line-to ind-body)
	      (insert (pop lines))))
	  (message "Note stored")
	  (org-back-to-heading t)
	  (org-cycle-hide-drawers 'children)))))
  (set-window-configuration org-log-note-window-configuration)
  (with-current-buffer (marker-buffer org-log-note-return-to)
    (goto-char org-log-note-return-to))
  (move-marker org-log-note-return-to nil)
  (and org-log-post-message (message "%s" org-log-post-message)))

(defun org-remove-empty-drawer-at (drawer pos)
  "Remove an empty drawer DRAWER at position POS.
POS may also be a marker."
  (with-current-buffer (if (markerp pos) (marker-buffer pos) (current-buffer))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char pos)
	(if (org-in-regexp
	     (concat "^[ \t]*:" drawer ":[ \t]*\n[ \t]*:END:[ \t]*\n?") 2)
	    (replace-match ""))))))

(defun org-sparse-tree (&optional arg)
  "Create a sparse tree, prompt for the details.
This command can create sparse trees.  You first need to select the type
of match used to create the tree:

t      Show all TODO entries.
T      Show entries with a specific TODO keyword.
m      Show entries selected by a tags/property match.
p      Enter a property name and its value (both with completion on existing
       names/values) and show entries with that property.
r      Show entries matching a regular expression (`/' can be used as well)
d      Show deadlines due within `org-deadline-warning-days'.
b      Show deadlines and scheduled items before a date.
a      Show deadlines and scheduled items after a date."
  (interactive "P")
  (let (ans kwd value)
    (message "Sparse tree: [r]egexp [/]regexp [t]odo [T]odo-kwd [m]atch [p]roperty\n             [d]eadlines [b]efore-date [a]fter-date [D]ates range")
    (setq ans (read-char-exclusive))
    (cond
     ((equal ans ?d)
      (call-interactively 'org-check-deadlines))
     ((equal ans ?b)
      (call-interactively 'org-check-before-date))
     ((equal ans ?a)
      (call-interactively 'org-check-after-date))
     ((equal ans ?D)
      (call-interactively 'org-check-dates-range))
     ((equal ans ?t)
      (org-show-todo-tree nil))
     ((equal ans ?T)
      (org-show-todo-tree '(4)))
     ((member ans '(?T ?m))
      (call-interactively 'org-match-sparse-tree))
     ((member ans '(?p ?P))
      (setq kwd (org-icompleting-read "Property: "
				 (mapcar 'list (org-buffer-property-keys))))
      (setq value (org-icompleting-read "Value: "
				   (mapcar 'list (org-property-values kwd))))
      (unless (string-match "\\`{.*}\\'" value)
	(setq value (concat "\"" value "\"")))
      (org-match-sparse-tree arg (concat kwd "=" value)))
     ((member ans '(?r ?R ?/))
      (call-interactively 'org-occur))
     (t (error "No such sparse tree command \"%c\"" ans)))))

(defvar org-occur-highlights nil
  "List of overlays used for occur matches.")
(make-variable-buffer-local 'org-occur-highlights)
(defvar org-occur-parameters nil
  "Parameters of the active org-occur calls.
This is a list, each call to org-occur pushes as cons cell,
containing the regular expression and the callback, onto the list.
The list can contain several entries if `org-occur' has been called
several time with the KEEP-PREVIOUS argument.  Otherwise, this list
will only contain one set of parameters.  When the highlights are
removed (for example with `C-c C-c', or with the next edit (depending
on `org-remove-highlights-with-change'), this variable is emptied
as well.")
(make-variable-buffer-local 'org-occur-parameters)

(defun org-occur (regexp &optional keep-previous callback)
  "Make a compact tree which shows all matches of REGEXP.
The tree will show the lines where the regexp matches, and all higher
headlines above the match.  It will also show the heading after the match,
to make sure editing the matching entry is easy.
If KEEP-PREVIOUS is non-nil, highlighting and exposing done by a previous
call to `org-occur' will be kept, to allow stacking of calls to this
command.
If CALLBACK is non-nil, it is a function which is called to confirm
that the match should indeed be shown."
  (interactive "sRegexp: \nP")
  (when (equal regexp "")
    (error "Regexp cannot be empty"))
  (unless keep-previous
    (org-remove-occur-highlights nil nil t))
  (push (cons regexp callback) org-occur-parameters)
  (let ((cnt 0))
    (save-excursion
      (goto-char (point-min))
      (if (or (not keep-previous)          ; do not want to keep
	      (not org-occur-highlights))  ; no previous matches
	  ;; hide everything
	  (org-overview))
      (while (re-search-forward regexp nil t)
	(when (or (not callback)
		  (save-match-data (funcall callback)))
	  (setq cnt (1+ cnt))
	  (when org-highlight-sparse-tree-matches
	    (org-highlight-new-match (match-beginning 0) (match-end 0)))
	  (org-show-context 'occur-tree))))
    (when org-remove-highlights-with-change
      (org-add-hook 'before-change-functions 'org-remove-occur-highlights
		    nil 'local))
    (unless org-sparse-tree-open-archived-trees
      (org-hide-archived-subtrees (point-min) (point-max)))
    (run-hooks 'org-occur-hook)
    (if (org-called-interactively-p 'interactive)
	(message "%d match(es) for regexp %s" cnt regexp))
    cnt))

(defun org-occur-next-match (&optional n reset)
  "Function for `next-error-function' to find sparse tree matches.
N is the number of matches to move, when negative move backwards.
RESET is entirely ignored - this function always goes back to the
starting point when no match is found."
  (let* ((limit (if (< n 0) (point-min) (point-max)))
	 (search-func (if (< n 0)
			  'previous-single-char-property-change
			'next-single-char-property-change))
	 (n (abs n))
	 (pos (point))
	 p1)
    (catch 'exit
      (while (setq p1 (funcall search-func (point) 'org-type))
	(when (equal p1 limit)
	  (goto-char pos)
	  (error "No more matches"))
	(when (equal (get-char-property p1 'org-type) 'org-occur)
	  (setq n (1- n))
	  (when (= n 0)
	    (goto-char p1)
	    (throw 'exit (point))))
	(goto-char p1))
      (goto-char p1)
      (error "No more matches"))))

(defun org-show-context (&optional key)
  "Make sure point and context are visible.
How much context is shown depends upon the variables
`org-show-hierarchy-above', `org-show-following-heading',
`org-show-entry-below' and `org-show-siblings'."
  (let ((heading-p   (org-at-heading-p t))
	(hierarchy-p (org-get-alist-option org-show-hierarchy-above key))
	(following-p (org-get-alist-option org-show-following-heading key))
	(entry-p     (org-get-alist-option org-show-entry-below key))
	(siblings-p  (org-get-alist-option org-show-siblings key)))
    (catch 'exit
      ;; Show heading or entry text
      (if (and heading-p (not entry-p))
	  (org-flag-heading nil)    ; only show the heading
	(and (or entry-p (outline-invisible-p) (org-invisible-p2))
	     (org-show-hidden-entry)))    ; show entire entry
      (when following-p
	;; Show next sibling, or heading below text
	(save-excursion
	  (and (if heading-p (org-goto-sibling) (outline-next-heading))
	       (org-flag-heading nil))))
      (when siblings-p (org-show-siblings))
      (when hierarchy-p
	;; show all higher headings, possibly with siblings
	(save-excursion
	  (while (and (condition-case nil
			  (progn (org-up-heading-all 1) t)
			(error nil))
		      (not (bobp)))
	    (org-flag-heading nil)
	    (when siblings-p (org-show-siblings))))))))

(defvar org-reveal-start-hook nil
  "Hook run before revealing a location.")

(defun org-reveal (&optional siblings)
  "Show current entry, hierarchy above it, and the following headline.
This can be used to show a consistent set of context around locations
exposed with `org-show-hierarchy-above' or `org-show-following-heading'
not t for the search context.

With optional argument SIBLINGS, on each level of the hierarchy all
siblings are shown.  This repairs the tree structure to what it would
look like when opened with hierarchical calls to `org-cycle'.
With double optional argument \\[universal-argument] \\[universal-argument], \
go to the parent and show the
entire tree."
  (interactive "P")
  (run-hooks 'org-reveal-start-hook)
  (let ((org-show-hierarchy-above t)
	(org-show-following-heading t)
	(org-show-siblings (if siblings t org-show-siblings)))
    (org-show-context nil))
  (when (equal siblings '(16))
    (save-excursion
      (when (org-up-heading-safe)
	(org-show-subtree)
	(run-hook-with-args 'org-cycle-hook 'subtree)))))

(defun org-highlight-new-match (beg end)
  "Highlight from BEG to END and mark the highlight is an occur headline."
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face 'secondary-selection)
    (overlay-put ov 'org-type 'org-occur)
    (push ov org-occur-highlights)))

(defun org-remove-occur-highlights (&optional beg end noremove)
  "Remove the occur highlights from the buffer.
BEG and END are ignored.  If NOREMOVE is nil, remove this function
from the `before-change-functions' in the current buffer."
  (interactive)
  (unless org-inhibit-highlight-removal
    (mapc 'delete-overlay org-occur-highlights)
    (setq org-occur-highlights nil)
    (setq org-occur-parameters nil)
    (unless noremove
      (remove-hook 'before-change-functions
		   'org-remove-occur-highlights 'local))))

;;;; Priorities

(defvar org-priority-regexp ".*?\\(\\[#\\([A-Z0-9]\\)\\] ?\\)"
  "Regular expression matching the priority indicator.")

(defvar org-remove-priority-next-time nil)

(defun org-priority-up ()
  "Increase the priority of the current item."
  (interactive)
  (org-priority 'up))

(defun org-priority-down ()
  "Decrease the priority of the current item."
  (interactive)
  (org-priority 'down))

(defun org-priority (&optional action)
  "Change the priority of an item by ARG.
ACTION can be `set', `up', `down', or a character."
  (interactive)
  (unless org-enable-priority-commands
    (error "Priority commands are disabled"))
  (setq action (or action 'set))
  (let (current new news have remove)
    (save-excursion
      (org-back-to-heading t)
      (if (looking-at org-priority-regexp)
	  (setq current (string-to-char (match-string 2))
		have t))
      (cond
       ((eq action 'remove)
	(setq remove t new ?\ ))
       ((or (eq action 'set)
	    (if (featurep 'xemacs) (characterp action) (integerp action)))
	(if (not (eq action 'set))
	    (setq new action)
	  (message "Priority %c-%c, SPC to remove: "
		   org-highest-priority org-lowest-priority)
	  (save-match-data
	    (setq new (read-char-exclusive))))
	(if (and (= (upcase org-highest-priority) org-highest-priority)
		 (= (upcase org-lowest-priority) org-lowest-priority))
	    (setq new (upcase new)))
	(cond ((equal new ?\ ) (setq remove t))
	      ((or (< (upcase new) org-highest-priority) (> (upcase new) org-lowest-priority))
	       (error "Priority must be between `%c' and `%c'"
		      org-highest-priority org-lowest-priority))))
       ((eq action 'up)
	(setq new (if have
		      (1- current)  ; normal cycling
		    ;; last priority was empty
		    (if (eq last-command this-command)
			org-lowest-priority  ; wrap around empty to lowest
		      ;; default
		      (if org-priority-start-cycle-with-default
			  org-default-priority
			(1- org-default-priority))))))
       ((eq action 'down)
	(setq new (if have
		      (1+ current)  ; normal cycling
		    ;; last priority was empty
		    (if (eq last-command this-command)
			org-highest-priority  ; wrap around empty to highest
		      ;; default
		      (if org-priority-start-cycle-with-default
			  org-default-priority
			(1+ org-default-priority))))))
       (t (error "Invalid action")))
      (if (or (< (upcase new) org-highest-priority)
	      (> (upcase new) org-lowest-priority))
	  (if (and (memq action '(up down))
		   (not have) (not (eq last-command this-command)))
              ;; `new' is from default priority
	      (error
	       "The default can not be set, see `org-default-priority' why")
            ;; normal cycling: `new' is beyond highest/lowest priority
            ;; and is wrapped around to the empty priority
	    (setq remove t)))
      (setq news (format "%c" new))
      (if have
	  (if remove
	      (replace-match "" t t nil 1)
	    (replace-match news t t nil 2))
	(if remove
	    (error "No priority cookie found in line")
	  (let ((case-fold-search nil))
	    (looking-at org-todo-line-regexp))
	  (if (match-end 2)
	      (progn
		(goto-char (match-end 2))
		(insert " [#" news "]"))
	    (goto-char (match-beginning 3))
	    (insert "[#" news "] "))))
      (org-preserve-lc (org-set-tags nil 'align)))
    (if remove
	(message "Priority removed")
      (message "Priority of current item set to %s" news))))

(defun org-get-priority (s)
  "Find priority cookie and return priority."
  (if (functionp org-get-priority-function)
      (funcall org-get-priority-function)
    (save-match-data
      (if (not (string-match org-priority-regexp s))
	  (* 1000 (- org-lowest-priority org-default-priority))
	(* 1000 (- org-lowest-priority
		   (string-to-char (match-string 2 s))))))))

;;;; Tags

(defvar org-agenda-archives-mode)
(defvar org-map-continue-from nil
  "Position from where mapping should continue.
Can be set by the action argument to `org-scan-tags' and `org-map-entries'.")

(defvar org-scanner-tags nil
  "The current tag list while the tags scanner is running.")
(defvar org-trust-scanner-tags nil
  "Should `org-get-tags-at' use the tags for the scanner.
This is for internal dynamical scoping only.
When this is non-nil, the function `org-get-tags-at' will return the value
of `org-scanner-tags' instead of building the list by itself.  This
can lead to large speed-ups when the tags scanner is used in a file with
many entries, and when the list of tags is retrieved, for example to
obtain a list of properties.  Building the tags list for each entry in such
a file becomes an N^2 operation - but with this variable set, it scales
as N.")

(defun org-scan-tags (action matcher todo-only &optional start-level)
  "Scan headline tags with inheritance and produce output ACTION.

ACTION can be `sparse-tree' to produce a sparse tree in the current buffer,
or `agenda' to produce an entry list for an agenda view.  It can also be
a Lisp form or a function that should be called at each matched headline, in
this case the return value is a list of all return values from these calls.

MATCHER is a Lisp form to be evaluated, testing if a given set of tags
qualifies a headline for inclusion.  When TODO-ONLY is non-nil,
only lines with a not-done TODO keyword are included in the output.
This should be the same variable that was scoped into
and set by `org-make-tags-matcher' when it constructed MATCHER.

START-LEVEL can be a string with asterisks, reducing the scope to
headlines matching this string."
  (require 'org-agenda)
  (let* ((re (concat "^"
		     (if start-level
			 ;; Get the correct level to match
			 (concat "\\*\\{" (number-to-string start-level) "\\} ")
		       org-outline-regexp)
		     " *\\(\\<\\("
		     (mapconcat 'regexp-quote org-todo-keywords-1 "\\|")
		     (org-re "\\)\\>\\)? *\\(.*?\\)\\(:[[:alnum:]_@#%:]+:\\)?[ \t]*$")))
	 (props (list 'face 'default
		      'done-face 'org-agenda-done
		      'undone-face 'default
		      'mouse-face 'highlight
		      'org-not-done-regexp org-not-done-regexp
		      'org-todo-regexp org-todo-regexp
		      'org-complex-heading-regexp org-complex-heading-regexp
		      'help-echo
		      (format "mouse-2 or RET jump to org file %s"
			      (abbreviate-file-name
			       (or (buffer-file-name (buffer-base-buffer))
				   (buffer-name (buffer-base-buffer)))))))
	 (case-fold-search nil)
	 (org-map-continue-from nil)
         lspos tags tags-list
	 (tags-alist (list (cons 0 org-file-tags)))
	 (llast 0) rtn rtn1 level category i txt
	 todo marker entry priority)
    (when (not (or (member action '(agenda sparse-tree)) (functionp action)))
      (setq action (list 'lambda nil action)))
    (save-excursion
      (goto-char (point-min))
      (when (eq action 'sparse-tree)
	(org-overview)
	(org-remove-occur-highlights))
      (while (re-search-forward re nil t)
	(setq org-map-continue-from nil)
	(catch :skip
	  (setq todo (if (match-end 1) (org-match-string-no-properties 2))
		tags (if (match-end 4) (org-match-string-no-properties 4)))
	  (goto-char (setq lspos (match-beginning 0)))
	  (setq level (org-reduced-level (funcall outline-level))
		category (org-get-category))
	  (setq i llast llast level)
	  ;; remove tag lists from same and sublevels
	  (while (>= i level)
	    (when (setq entry (assoc i tags-alist))
	      (setq tags-alist (delete entry tags-alist)))
	    (setq i (1- i)))
	  ;; add the next tags
	  (when tags
	    (setq tags (org-split-string tags ":")
		  tags-alist
		  (cons (cons level tags) tags-alist)))
	  ;; compile tags for current headline
	  (setq tags-list
		(if org-use-tag-inheritance
		    (apply 'append (mapcar 'cdr (reverse tags-alist)))
		  tags)
		org-scanner-tags tags-list)
	  (when org-use-tag-inheritance
	    (setcdr (car tags-alist)
		    (mapcar (lambda (x)
			      (setq x (copy-sequence x))
			      (org-add-prop-inherited x))
			    (cdar tags-alist))))
	  (when (and tags org-use-tag-inheritance
		     (or (not (eq t org-use-tag-inheritance))
			 org-tags-exclude-from-inheritance))
	    ;; selective inheritance, remove uninherited ones
	    (setcdr (car tags-alist)
		    (org-remove-uninherited-tags (cdar tags-alist))))
	  (when (and

		 ;; eval matcher only when the todo condition is OK
		 (and (or (not todo-only) (member todo org-not-done-keywords))
		      (let ((case-fold-search t)) (eval matcher)))

		 ;; Call the skipper, but return t if it does not skip,
		 ;; so that the `and' form continues evaluating
		 (progn
		   (unless (eq action 'sparse-tree) (org-agenda-skip))
		   t)

		 ;; Check if timestamps are deselecting this entry
		 (or (not todo-only)
		     (and (member todo org-not-done-keywords)
			  (or (not org-agenda-tags-todo-honor-ignore-options)
			      (not (org-agenda-check-for-timestamp-as-reason-to-ignore-todo-item)))))

		 ;; Extra check for the archive tag
		 ;; FIXME: Does the skipper already do this????
		 (or
		  (not (member org-archive-tag tags-list))
		  ;; we have an archive tag, should we use this anyway?
		  (or (not org-agenda-skip-archived-trees)
		      (and (eq action 'agenda) org-agenda-archives-mode))))

	    ;; select this headline

	    (cond
	     ((eq action 'sparse-tree)
	      (and org-highlight-sparse-tree-matches
		   (org-get-heading) (match-end 0)
		   (org-highlight-new-match
		    (match-beginning 1) (match-end 1)))
	      (org-show-context 'tags-tree))
	     ((eq action 'agenda)
	      (setq txt (org-agenda-format-item
			 ""
			 (concat
			  (if (eq org-tags-match-list-sublevels 'indented)
			      (make-string (1- level) ?.) "")
			  (org-get-heading))
			 category
			 tags-list
			 )
		    priority (org-get-priority txt))
	      (goto-char lspos)
	      (setq marker (org-agenda-new-marker))
	      (org-add-props txt props
		'org-marker marker 'org-hd-marker marker 'org-category category
		'todo-state todo
		'priority priority 'type "tagsmatch")
	      (push txt rtn))
	     ((functionp action)
	      (setq org-map-continue-from nil)
	      (save-excursion
		(setq rtn1 (funcall action))
		(push rtn1 rtn)))
	     (t (error "Invalid action")))

	    ;; if we are to skip sublevels, jump to end of subtree
	    (unless org-tags-match-list-sublevels
	      (org-end-of-subtree t)
	      (backward-char 1))))
	;; Get the correct position from where to continue
	(if org-map-continue-from
	    (goto-char org-map-continue-from)
	  (and (= (point) lspos) (end-of-line 1)))))
    (when (and (eq action 'sparse-tree)
	       (not org-sparse-tree-open-archived-trees))
      (org-hide-archived-subtrees (point-min) (point-max)))
    (nreverse rtn)))

(defun org-remove-uninherited-tags (tags)
  "Remove all tags that are not inherited from the list TAGS."
  (cond
   ((eq org-use-tag-inheritance t)
    (if org-tags-exclude-from-inheritance
	(org-delete-all org-tags-exclude-from-inheritance tags)
      tags))
   ((not org-use-tag-inheritance) nil)
   ((stringp org-use-tag-inheritance)
    (delq nil (mapcar
	       (lambda (x)
		 (if (and (string-match org-use-tag-inheritance x)
			  (not (member x org-tags-exclude-from-inheritance)))
		     x nil))
	       tags)))
   ((listp org-use-tag-inheritance)
    (delq nil (mapcar
	       (lambda (x)
		 (if (member x org-use-tag-inheritance) x nil))
	       tags)))))

(defun org-match-sparse-tree (&optional todo-only match)
  "Create a sparse tree according to tags string MATCH.
MATCH can contain positive and negative selection of tags, like
\"+WORK+URGENT-WITHBOSS\".
If optional argument TODO-ONLY is non-nil, only select lines that are
also TODO lines."
  (interactive "P")
  (org-prepare-agenda-buffers (list (current-buffer)))
  (org-scan-tags 'sparse-tree (cdr (org-make-tags-matcher match)) todo-only))

(defalias 'org-tags-sparse-tree 'org-match-sparse-tree)

(defvar org-cached-props nil)
(defun org-cached-entry-get (pom property)
  (if (or (eq t org-use-property-inheritance)
	  (and (stringp org-use-property-inheritance)
	       (string-match org-use-property-inheritance property))
	  (and (listp org-use-property-inheritance)
	       (member property org-use-property-inheritance)))
      ;; Caching is not possible, check it directly
      (org-entry-get pom property 'inherit)
    ;; Get all properties, so that we can do complicated checks easily
    (cdr (assoc property (or org-cached-props
			     (setq org-cached-props
				   (org-entry-properties pom)))))))

(defun org-global-tags-completion-table (&optional files)
  "Return the list of all tags in all agenda buffer/files.
Optional FILES argument is a list of files to which can be used
instead of the agenda files."
  (save-excursion
    (org-uniquify
     (delq nil
	   (apply 'append
		  (mapcar
		   (lambda (file)
		     (set-buffer (find-file-noselect file))
		     (append (org-get-buffer-tags)
			     (mapcar (lambda (x) (if (stringp (car-safe x))
						     (list (car-safe x)) nil))
				     org-tag-alist)))
		   (if (and files (car files))
		       files
		     (org-agenda-files))))))))

(defun org-make-tags-matcher (match)
  "Create the TAGS/TODO matcher form for the selection string MATCH.

The variable `todo-only' is scoped dynamically into this function; it will be
set to t if the matcher restricts matching to TODO entries,
otherwise will not be touched.

Returns a cons of the selection string MATCH and the constructed
lisp form implementing the matcher.  The matcher is to be
evaluated at an Org entry, with point on the headline,
and returns t if the entry matches the
selection string MATCH.  The returned lisp form references
two variables with information about the entry, which must be
bound around the form's evaluation: todo, the TODO keyword at the
entry (or nil of none); and tags-list, the list of all tags at the
entry including inherited ones.  Additionally, the category
of the entry (if any) must be specified as the text property
'org-category on the headline.

See also `org-scan-tags'.
"
  (declare (special todo-only))
  (unless (boundp 'todo-only)
    (error "org-make-tags-matcher expects todo-only to be scoped in"))
  (unless match
    ;; Get a new match request, with completion
    (let ((org-last-tags-completion-table
	   (org-global-tags-completion-table)))
      (setq match (org-completing-read-no-i
		   "Match: " 'org-tags-completion-function nil nil nil
		   'org-tags-history))))

  ;; Parse the string and create a lisp form
  (let ((match0 match)
	(re (org-re "^&?\\([-+:]\\)?\\({[^}]+}\\|LEVEL\\([<=>]\\{1,2\\}\\)\\([0-9]+\\)\\|\\(\\(?:[[:alnum:]_]+\\(?:\\\\-\\)*\\)+\\)\\([<>=]\\{1,2\\}\\)\\({[^}]+}\\|\"[^\"]*\"\\|-?[.0-9]+\\(?:[eE][-+]?[0-9]+\\)?\\)\\|[[:alnum:]_@#%]+\\)"))
	minus tag mm
	tagsmatch todomatch tagsmatcher todomatcher kwd matcher
	orterms term orlist re-p str-p level-p level-op time-p
	prop-p pn pv po gv rest)
    (if (string-match "/+" match)
	;; match contains also a todo-matching request
	(progn
	  (setq tagsmatch (substring match 0 (match-beginning 0))
		todomatch (substring match (match-end 0)))
	  (if (string-match "^!" todomatch)
	      (setq todo-only t todomatch (substring todomatch 1)))
	  (if (string-match "^\\s-*$" todomatch)
	      (setq todomatch nil)))
      ;; only matching tags
      (setq tagsmatch match todomatch nil))

    ;; Make the tags matcher
    (if (or (not tagsmatch) (not (string-match "\\S-" tagsmatch)))
	(setq tagsmatcher t)
      (setq orterms (org-split-string tagsmatch "|") orlist nil)
      (while (setq term (pop orterms))
	(while (and (equal (substring term -1) "\\") orterms)
	  (setq term (concat term "|" (pop orterms)))) ; repair bad split
	(while (string-match re term)
	  (setq rest (substring term (match-end 0))
		minus (and (match-end 1)
			   (equal (match-string 1 term) "-"))
		tag (save-match-data (replace-regexp-in-string
				      "\\\\-" "-"
				      (match-string 2 term)))
		re-p (equal (string-to-char tag) ?{)
		level-p (match-end 4)
		prop-p (match-end 5)
		mm (cond
		    (re-p `(org-match-any-p ,(substring tag 1 -1) tags-list))
		    (level-p
		     (setq level-op (org-op-to-function (match-string 3 term)))
		     `(,level-op level ,(string-to-number
					 (match-string 4 term))))
		    (prop-p
		     (setq pn (match-string 5 term)
			   po (match-string 6 term)
			   pv (match-string 7 term)
			   re-p (equal (string-to-char pv) ?{)
			   str-p (equal (string-to-char pv) ?\")
			   time-p (save-match-data
				    (string-match "^\"[[<].*[]>]\"$" pv))
			   pv (if (or re-p str-p) (substring pv 1 -1) pv))
		     (if time-p (setq pv (org-matcher-time pv)))
		     (setq po (org-op-to-function po (if time-p 'time str-p)))
		     (cond
		      ((equal pn "CATEGORY")
		       (setq gv '(get-text-property (point) 'org-category)))
		      ((equal pn "TODO")
		       (setq gv 'todo))
		      (t
		       (setq gv `(org-cached-entry-get nil ,pn))))
		     (if re-p
			 (if (eq po 'org<>)
			     `(not (string-match ,pv (or ,gv "")))
			   `(string-match ,pv (or ,gv "")))
		       (if str-p
			   `(,po (or ,gv "") ,pv)
			 `(,po (string-to-number (or ,gv ""))
			       ,(string-to-number pv) ))))
		    (t `(member ,tag tags-list)))
		mm (if minus (list 'not mm) mm)
		term rest)
	  (push mm tagsmatcher))
	(push (if (> (length tagsmatcher) 1)
		  (cons 'and tagsmatcher)
		(car tagsmatcher))
	      orlist)
	(setq tagsmatcher nil))
      (setq tagsmatcher (if (> (length orlist) 1) (cons 'or orlist) (car orlist)))
      (setq tagsmatcher
	    (list 'progn '(setq org-cached-props nil) tagsmatcher)))
    ;; Make the todo matcher
    (if (or (not todomatch) (not (string-match "\\S-" todomatch)))
	(setq todomatcher t)
      (setq orterms (org-split-string todomatch "|") orlist nil)
      (while (setq term (pop orterms))
	(while (string-match re term)
	  (setq minus (and (match-end 1)
			   (equal (match-string 1 term) "-"))
		kwd (match-string 2 term)
		re-p (equal (string-to-char kwd) ?{)
		term (substring term (match-end 0))
		mm (if re-p
		       `(string-match  ,(substring kwd 1 -1) todo)
		     (list 'equal 'todo kwd))
		mm (if minus (list 'not mm) mm))
	  (push mm todomatcher))
	(push (if (> (length todomatcher) 1)
		  (cons 'and todomatcher)
		(car todomatcher))
	      orlist)
	(setq todomatcher nil))
      (setq todomatcher (if (> (length orlist) 1)
			    (cons 'or orlist) (car orlist))))

    ;; Return the string and lisp forms of the matcher
    (setq matcher (if todomatcher
		      (list 'and tagsmatcher todomatcher)
		    tagsmatcher))
    (when todo-only
      (setq matcher (list 'and '(member todo org-not-done-keywords)
			  matcher)))
    (cons match0 matcher)))

(defun org-op-to-function (op &optional stringp)
  "Turn an operator into the appropriate function."
  (setq op
	(cond
	 ((equal  op   "<"       ) '(<     string<      org-time<))
	 ((equal  op   ">"       ) '(>     org-string>  org-time>))
	 ((member op '("<=" "=<")) '(<=    org-string<= org-time<=))
	 ((member op '(">=" "=>")) '(>=    org-string>= org-time>=))
	 ((member op '("="  "==")) '(=     string=      org-time=))
	 ((member op '("<>" "!=")) '(org<> org-string<> org-time<>))))
  (nth (if (eq stringp 'time) 2 (if stringp 1 0)) op))

(defun org<> (a b) (not (= a b)))
(defun org-string<= (a b) (or (string= a b) (string< a b)))
(defun org-string>= (a b) (not (string< a b)))
(defun org-string>  (a b) (and (not (string= a b)) (not (string< a b))))
(defun org-string<> (a b) (not (string= a b)))
(defun org-time=  (a b) (setq a (org-2ft a) b (org-2ft b)) (and (> a 0) (> b 0) (=     a b)))
(defun org-time<  (a b) (setq a (org-2ft a) b (org-2ft b)) (and (> a 0) (> b 0) (<     a b)))
(defun org-time<= (a b) (setq a (org-2ft a) b (org-2ft b)) (and (> a 0) (> b 0) (<=    a b)))
(defun org-time>  (a b) (setq a (org-2ft a) b (org-2ft b)) (and (> a 0) (> b 0) (>     a b)))
(defun org-time>= (a b) (setq a (org-2ft a) b (org-2ft b)) (and (> a 0) (> b 0) (>=    a b)))
(defun org-time<> (a b) (setq a (org-2ft a) b (org-2ft b)) (and (> a 0) (> b 0) (org<> a b)))
(defun org-2ft (s)
  "Convert S to a floating point time.
If S is already a number, just return it.  If it is a string, parse
it as a time string and apply `float-time' to it.  If S is nil, just return 0."
  (cond
   ((numberp s) s)
   ((stringp s)
    (condition-case nil
	(float-time (apply 'encode-time (org-parse-time-string s)))
      (error 0.)))
   (t 0.)))

(defun org-time-today ()
  "Time in seconds today at 0:00.
Returns the float number of seconds since the beginning of the
epoch to the beginning of today (00:00)."
  (float-time (apply 'encode-time
		     (append '(0 0 0) (nthcdr 3 (decode-time))))))

(defun org-matcher-time (s)
  "Interpret a time comparison value."
  (save-match-data
    (cond
     ((string= s "<now>") (float-time))
     ((string= s "<today>") (org-time-today))
     ((string= s "<tomorrow>")   (+ 86400.0 (org-time-today)))
     ((string= s "<yesterday>")  (- (org-time-today) 86400.0))
     ((string-match "^<\\([-+][0-9]+\\)\\([dwmy]\\)>$" s)
      (+ (org-time-today)
	 (* (string-to-number (match-string 1 s))
	    (cdr (assoc (match-string 2 s)
			'(("d" . 86400.0)   ("w" . 604800.0)
			  ("m" . 2678400.0) ("y" . 31557600.0)))))))
     (t (org-2ft s)))))

(defun org-match-any-p (re list)
  "Does re match any element of list?"
  (setq list (mapcar (lambda (x) (string-match re x)) list))
  (delq nil list))

(defvar org-add-colon-after-tag-completion nil)  ;; dynamically scoped param
(defvar org-tags-overlay (make-overlay 1 1))
(org-detach-overlay org-tags-overlay)

(defun org-get-local-tags-at (&optional pos)
  "Get a list of tags defined in the current headline."
  (org-get-tags-at pos 'local))

(defun org-get-local-tags ()
  "Get a list of tags defined in the current headline."
  (org-get-tags-at nil 'local))

(defun org-get-tags-at (&optional pos local)
  "Get a list of all headline tags applicable at POS.
POS defaults to point.  If tags are inherited, the list contains
the targets in the same sequence as the headlines appear, i.e.
the tags of the current headline come last.
When LOCAL is non-nil, only return tags from the current headline,
ignore inherited ones."
  (interactive)
  (if (and org-trust-scanner-tags
	   (or (not pos) (equal pos (point)))
	   (not local))
      org-scanner-tags
    (let (tags ltags lastpos parent)
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char (or pos (point)))
	  (save-match-data
	    (catch 'done
	      (condition-case nil
		  (progn
		    (org-back-to-heading t)
		    (while (not (equal lastpos (point)))
		      (setq lastpos (point))
		      (when (looking-at
			     (org-re "[^\r\n]+?:\\([[:alnum:]_@#%:]+\\):[ \t]*$"))
			(setq ltags (org-split-string
				     (org-match-string-no-properties 1) ":"))
			(when parent
			  (setq ltags (mapcar 'org-add-prop-inherited ltags)))
			(setq tags (append
				    (if parent
					(org-remove-uninherited-tags ltags)
				      ltags)
				    tags)))
		      (or org-use-tag-inheritance (throw 'done t))
		      (if local (throw 'done t))
		      (or (org-up-heading-safe) (error nil))
		      (setq parent t)))
		(error nil)))))
	(if local
	    tags
	  (append (org-remove-uninherited-tags org-file-tags) tags))))))

(defun org-add-prop-inherited (s)
  (add-text-properties 0 (length s) '(inherited t) s)
  s)

(defun org-toggle-tag (tag &optional onoff)
  "Toggle the tag TAG for the current line.
If ONOFF is `on' or `off', don't toggle but set to this state."
  (let (res current)
    (save-excursion
      (org-back-to-heading t)
      (if (re-search-forward (org-re "[ \t]:\\([[:alnum:]_@#%:]+\\):[ \t]*$")
			     (point-at-eol) t)
	  (progn
	    (setq current (match-string 1))
	    (replace-match ""))
	(setq current ""))
      (setq current (nreverse (org-split-string current ":")))
      (cond
       ((eq onoff 'on)
	(setq res t)
	(or (member tag current) (push tag current)))
       ((eq onoff 'off)
	(or (not (member tag current)) (setq current (delete tag current))))
       (t (if (member tag current)
	      (setq current (delete tag current))
	    (setq res t)
	    (push tag current))))
      (end-of-line 1)
      (if current
	  (progn
	    (insert " :" (mapconcat 'identity (nreverse current) ":") ":")
	    (org-set-tags nil t))
	(delete-horizontal-space))
      (run-hooks 'org-after-tags-change-hook))
    res))

(defun org-align-tags-here (to-col)
  ;; Assumes that this is a headline
  (let ((pos (point)) (col (current-column)) ncol tags-l p)
    (beginning-of-line 1)
    (if	(and (looking-at (org-re ".*?\\([ \t]+\\)\\(:[[:alnum:]_@#%:]+:\\)[ \t]*$"))
	     (< pos (match-beginning 2)))
	(progn
	  (setq tags-l (- (match-end 2) (match-beginning 2)))
	  (goto-char (match-beginning 1))
	  (insert " ")
	  (delete-region (point) (1+ (match-beginning 2)))
	  (setq ncol (max (current-column)
			  (1+ col)
			  (if (> to-col 0)
			      to-col
			    (- (abs to-col) tags-l))))
	  (setq p (point))
	  (insert (make-string (- ncol (current-column)) ?\ ))
	  (setq ncol (current-column))
	  (when indent-tabs-mode (tabify p (point-at-eol)))
	  (org-move-to-column (min ncol col) t))
      (goto-char pos))))

(defun org-set-tags-command (&optional arg just-align)
  "Call the set-tags command for the current entry."
  (interactive "P")
  (if (org-at-heading-p)
      (org-set-tags arg just-align)
    (save-excursion
      (org-back-to-heading t)
      (org-set-tags arg just-align))))

(defun org-set-tags-to (data)
  "Set the tags of the current entry to DATA, replacing the current tags.
DATA may be a tags string like :aa:bb:cc:, or a list of tags.
If DATA is nil or the empty string, any tags will be removed."
  (interactive "sTags: ")
  (setq data
	(cond
	 ((eq data nil) "")
	 ((equal data "") "")
	 ((stringp data)
	  (concat ":" (mapconcat 'identity (org-split-string data ":+") ":")
		  ":"))
	 ((listp data)
	  (concat ":" (mapconcat 'identity data ":") ":"))
	 (t nil)))
  (when data
    (save-excursion
      (org-back-to-heading t)
      (when (looking-at org-complex-heading-regexp)
	(if (match-end 5)
	    (progn
	      (goto-char (match-beginning 5))
	      (insert data)
	      (delete-region (point) (point-at-eol))
	      (org-set-tags nil 'align))
	  (goto-char (point-at-eol))
	  (insert " " data)
	  (org-set-tags nil 'align)))
      (beginning-of-line 1)
      (if (looking-at ".*?\\([ \t]+\\)$")
	  (delete-region (match-beginning 1) (match-end 1))))))

(defun org-align-all-tags ()
  "Align the tags i all headings."
  (interactive)
  (save-excursion
    (or (ignore-errors (org-back-to-heading t))
	(outline-next-heading))
    (if (org-at-heading-p)
	(org-set-tags t)
      (message "No headings"))))

(defvar org-indent-indentation-per-level)
(defun org-set-tags (&optional arg just-align)
  "Set the tags for the current headline.
With prefix ARG, realign all tags in headings in the current buffer."
  (interactive "P")
  (let* ((re org-outline-regexp-bol)
	 (current (org-get-tags-string))
	 (col (current-column))
	 (org-setting-tags t)
	 table current-tags inherited-tags ; computed below when needed
	 tags p0 c0 c1 rpl di tc level)
    (if arg
	(save-excursion
	  (goto-char (point-min))
	  (let ((buffer-invisibility-spec (org-inhibit-invisibility)))
	    (while (re-search-forward re nil t)
	      (org-set-tags nil t)
	      (end-of-line 1)))
	  (message "All tags realigned to column %d" org-tags-column))
      (if just-align
	  (setq tags current)
	;; Get a new set of tags from the user
	(save-excursion
	  (setq table (append org-tag-persistent-alist
			      (or org-tag-alist (org-get-buffer-tags))
			      (and
			       org-complete-tags-always-offer-all-agenda-tags
			       (org-global-tags-completion-table
				(org-agenda-files))))
		org-last-tags-completion-table table
		current-tags (org-split-string current ":")
		inherited-tags (nreverse
				(nthcdr (length current-tags)
					(nreverse (org-get-tags-at))))
		tags
		(if (or (eq t org-use-fast-tag-selection)
			(and org-use-fast-tag-selection
			     (delq nil (mapcar 'cdr table))))
		    (org-fast-tag-selection
		     current-tags inherited-tags table
		     (if org-fast-tag-selection-include-todo
			 org-todo-key-alist))
		  (let ((org-add-colon-after-tag-completion (< 1 (length table))))
		    (org-trim
		     (org-icompleting-read "Tags: "
					   'org-tags-completion-function
					   nil nil current 'org-tags-history))))))
	(while (string-match "[-+&]+" tags)
	  ;; No boolean logic, just a list
	  (setq tags (replace-match ":" t t tags))))

      (setq tags (replace-regexp-in-string "[,]" ":" tags))

      (if org-tags-sort-function
      	  (setq tags (mapconcat 'identity
      				(sort (org-split-string
				       tags (org-re "[^[:alnum:]_@#%]+"))
      				      org-tags-sort-function) ":")))

      (if (string-match "\\`[\t ]*\\'" tags)
	  (setq tags "")
	(unless (string-match ":$" tags) (setq tags (concat tags ":")))
	(unless (string-match "^:" tags) (setq tags (concat ":" tags))))

      ;; Insert new tags at the correct column
      (beginning-of-line 1)
      (setq level (or (and (looking-at org-outline-regexp)
			   (- (match-end 0) (point) 1))
		      1))
      (cond
       ((and (equal current "") (equal tags "")))
       ((re-search-forward
	 (concat "\\([ \t]*" (regexp-quote current) "\\)[ \t]*$")
	 (point-at-eol) t)
	(if (equal tags "")
	    (setq rpl "")
	  (goto-char (match-beginning 0))
	  (setq c0 (current-column)
		;; compute offset for the case of org-indent-mode active
		di (if org-indent-mode
		       (* (1- org-indent-indentation-per-level) (1- level))
		     0)
		p0 (if (equal (char-before) ?*) (1+ (point)) (point))
		tc (+ org-tags-column (if (> org-tags-column 0) (- di) di))
		c1 (max (1+ c0) (if (> tc 0) tc (- (- tc) (length tags))))
		rpl (concat (make-string (max 0 (- c1 c0)) ?\ ) tags)))
	(replace-match rpl t t)
	(and (not (featurep 'xemacs)) c0 indent-tabs-mode (tabify p0 (point)))
	tags)
       (t (error "Tags alignment failed")))
      (org-move-to-column col)
      (unless just-align
	(run-hooks 'org-after-tags-change-hook)))))

(defun org-change-tag-in-region (beg end tag off)
  "Add or remove TAG for each entry in the region.
This works in the agenda, and also in an org-mode buffer."
  (interactive
   (list (region-beginning) (region-end)
	 (let ((org-last-tags-completion-table
		(if (eq major-mode 'org-mode)
		    (org-get-buffer-tags)
		  (org-global-tags-completion-table))))
	   (org-icompleting-read
	    "Tag: " 'org-tags-completion-function nil nil nil
	    'org-tags-history))
	 (progn
	   (message "[s]et or [r]emove? ")
	   (equal (read-char-exclusive) ?r))))
  (if (fboundp 'deactivate-mark) (deactivate-mark))
  (let ((agendap (equal major-mode 'org-agenda-mode))
	l1 l2 m buf pos newhead (cnt 0))
    (goto-char end)
    (setq l2 (1- (org-current-line)))
    (goto-char beg)
    (setq l1 (org-current-line))
    (loop for l from l1 to l2 do
	  (org-goto-line l)
	  (setq m (get-text-property (point) 'org-hd-marker))
	  (when (or (and (eq major-mode 'org-mode) (org-at-heading-p))
		    (and agendap m))
	    (setq buf (if agendap (marker-buffer m) (current-buffer))
		  pos (if agendap m (point)))
	    (with-current-buffer buf
	      (save-excursion
		(save-restriction
		  (goto-char pos)
		  (setq cnt (1+ cnt))
		  (org-toggle-tag tag (if off 'off 'on))
		  (setq newhead (org-get-heading)))))
	    (and agendap (org-agenda-change-all-lines newhead m))))
    (message "Tag :%s: %s in %d headings" tag (if off "removed" "set") cnt)))

(defun org-tags-completion-function (string predicate &optional flag)
  (let (s1 s2 rtn (ctable org-last-tags-completion-table)
	   (confirm (lambda (x) (stringp (car x)))))
    (if (string-match "^\\(.*[-+:&,|]\\)\\([^-+:&,|]*\\)$" string)
        (setq s1 (match-string 1 string)
              s2 (match-string 2 string))
      (setq s1 "" s2 string))
    (cond
     ((eq flag nil)
      ;; try completion
      (setq rtn (try-completion s2 ctable confirm))
      (if (stringp rtn)
	  (setq rtn
		(concat s1 s2 (substring rtn (length s2))
			(if (and org-add-colon-after-tag-completion
				 (assoc rtn ctable))
			    ":" ""))))
      rtn)
     ((eq flag t)
      ;; all-completions
      (all-completions s2 ctable confirm)
      )
     ((eq flag 'lambda)
      ;; exact match?
      (assoc s2 ctable)))
    ))

(defun org-fast-tag-insert (kwd tags face &optional end)
  "Insert KDW, and the TAGS, the latter with face FACE.  Also insert END."
  (insert (format "%-12s" (concat kwd ":"))
	  (org-add-props (mapconcat 'identity tags " ") nil 'face face)
	  (or end "")))

(defun org-fast-tag-show-exit (flag)
  (save-excursion
    (org-goto-line 3)
    (if (re-search-forward "[ \t]+Next change exits" (point-at-eol) t)
	(replace-match ""))
    (when flag
      (end-of-line 1)
      (org-move-to-column (- (window-width) 19) t)
      (insert (org-add-props " Next change exits" nil 'face 'org-warning)))))

(defun org-set-current-tags-overlay (current prefix)
  (let ((s (concat ":" (mapconcat 'identity current ":") ":")))
    (if (featurep 'xemacs)
	(org-overlay-display org-tags-overlay (concat prefix s)
			     'secondary-selection)
      (put-text-property 0 (length s) 'face '(secondary-selection org-tag) s)
      (org-overlay-display org-tags-overlay (concat prefix s)))))

(defvar org-last-tag-selection-key nil)
(defun org-fast-tag-selection (current inherited table &optional todo-table)
  "Fast tag selection with single keys.
CURRENT is the current list of tags in the headline, INHERITED is the
list of inherited tags, and TABLE is an alist of tags and corresponding keys,
possibly with grouping information.  TODO-TABLE is a similar table with
TODO keywords, should these have keys assigned to them.
If the keys are nil, a-z are automatically assigned.
Returns the new tags string, or nil to not change the current settings."
  (let* ((fulltable (append table todo-table))
	 (maxlen (apply 'max (mapcar
			      (lambda (x)
				(if (stringp (car x)) (string-width (car x)) 0))
			      fulltable)))
	 (buf (current-buffer))
	 (expert (eq org-fast-tag-selection-single-key 'expert))
	 (buffer-tags nil)
	 (fwidth (+ maxlen 3 1 3))
	 (ncol (/ (- (window-width) 4) fwidth))
	 (i-face 'org-done)
	 (c-face 'org-todo)
	 tg cnt e c char c1 c2 ntable tbl rtn
	 ov-start ov-end ov-prefix
	 (exit-after-next org-fast-tag-selection-single-key)
	 (done-keywords org-done-keywords)
	 groups ingroup)
    (save-excursion
      (beginning-of-line 1)
      (if (looking-at
	   (org-re ".*[ \t]\\(:[[:alnum:]_@#%:]+:\\)[ \t]*$"))
	  (setq ov-start (match-beginning 1)
		ov-end (match-end 1)
		ov-prefix "")
	(setq ov-start (1- (point-at-eol))
	      ov-end (1+ ov-start))
	(skip-chars-forward "^\n\r")
	(setq ov-prefix
	      (concat
	       (buffer-substring (1- (point)) (point))
	       (if (> (current-column) org-tags-column)
		   " "
		 (make-string (- org-tags-column (current-column)) ?\ ))))))
    (move-overlay org-tags-overlay ov-start ov-end)
    (save-window-excursion
      (if expert
	  (set-buffer (get-buffer-create " *Org tags*"))
	(delete-other-windows)
	(split-window-vertically)
	(org-switch-to-buffer-other-window (get-buffer-create " *Org tags*")))
      (erase-buffer)
      (org-set-local 'org-done-keywords done-keywords)
      (org-fast-tag-insert "Inherited" inherited i-face "\n")
      (org-fast-tag-insert "Current" current c-face "\n\n")
      (org-fast-tag-show-exit exit-after-next)
      (org-set-current-tags-overlay current ov-prefix)
      (setq tbl fulltable char ?a cnt 0)
      (while (setq e (pop tbl))
	(cond
	 ((equal (car e) :startgroup)
	  (push '() groups) (setq ingroup t)
	  (when (not (= cnt 0))
	    (setq cnt 0)
	    (insert "\n"))
	  (insert (if (cdr e) (format "%s: " (cdr e)) "") "{ "))
	 ((equal (car e) :endgroup)
	  (setq ingroup nil cnt 0)
	  (insert "}" (if (cdr e) (format " (%s) " (cdr e)) "") "\n"))
	 ((equal e '(:newline))
	  (when (not (= cnt 0))
	    (setq cnt 0)
	    (insert "\n")
	    (setq e (car tbl))
	    (while (equal (car tbl) '(:newline))
	      (insert "\n")
	      (setq tbl (cdr tbl)))))
	 (t
	  (setq tg (copy-sequence (car e)) c2 nil)
	  (if (cdr e)
	      (setq c (cdr e))
	    ;; automatically assign a character.
	    (setq c1 (string-to-char
		      (downcase (substring
				 tg (if (= (string-to-char tg) ?@) 1 0)))))
	    (if (or (rassoc c1 ntable) (rassoc c1 table))
		(while (or (rassoc char ntable) (rassoc char table))
		  (setq char (1+ char)))
	      (setq c2 c1))
	    (setq c (or c2 char)))
	  (if ingroup (push tg (car groups)))
	  (setq tg (org-add-props tg nil 'face
				  (cond
				   ((not (assoc tg table))
				    (org-get-todo-face tg))
				   ((member tg current) c-face)
				   ((member tg inherited) i-face)
				   (t nil))))
	  (if (and (= cnt 0) (not ingroup)) (insert "  "))
	  (insert "[" c "] " tg (make-string
				 (- fwidth 4 (length tg)) ?\ ))
	  (push (cons tg c) ntable)
	  (when (= (setq cnt (1+ cnt)) ncol)
	    (insert "\n")
	    (if ingroup (insert "  "))
	    (setq cnt 0)))))
      (setq ntable (nreverse ntable))
      (insert "\n")
      (goto-char (point-min))
      (if (not expert) (org-fit-window-to-buffer))
      (setq rtn
	    (catch 'exit
	      (while t
		(message "[a-z..]:Toggle [SPC]:clear [RET]:accept [TAB]:free [!] %sgroups%s"
			 (if (not groups) "no " "")
			 (if expert " [C-c]:window" (if exit-after-next " [C-c]:single" " [C-c]:multi")))
		(setq c (let ((inhibit-quit t)) (read-char-exclusive)))
		(setq org-last-tag-selection-key c)
		(cond
		 ((= c ?\r) (throw 'exit t))
		 ((= c ?!)
		  (setq groups (not groups))
		  (goto-char (point-min))
		  (while (re-search-forward "[{}]" nil t) (replace-match " ")))
		 ((= c ?\C-c)
		  (if (not expert)
		      (org-fast-tag-show-exit
		       (setq exit-after-next (not exit-after-next)))
		    (setq expert nil)
		    (delete-other-windows)
		    (set-window-buffer (split-window-vertically) " *Org tags*")
		    (org-switch-to-buffer-other-window " *Org tags*")
		    (org-fit-window-to-buffer)))
		 ((or (= c ?\C-g)
		      (and (= c ?q) (not (rassoc c ntable))))
		  (org-detach-overlay org-tags-overlay)
		  (setq quit-flag t))
		 ((= c ?\ )
		  (setq current nil)
		  (if exit-after-next (setq exit-after-next 'now)))
		 ((= c ?\t)
		  (condition-case nil
		      (setq tg (org-icompleting-read
				"Tag: "
				(or buffer-tags
				    (with-current-buffer buf
				      (org-get-buffer-tags)))))
		    (quit (setq tg "")))
		  (when (string-match "\\S-" tg)
		    (add-to-list 'buffer-tags (list tg))
		    (if (member tg current)
			(setq current (delete tg current))
		      (push tg current)))
		  (if exit-after-next (setq exit-after-next 'now)))
		 ((setq e (rassoc c todo-table) tg (car e))
		  (with-current-buffer buf
		    (save-excursion (org-todo tg)))
		  (if exit-after-next (setq exit-after-next 'now)))
		 ((setq e (rassoc c ntable) tg (car e))
		  (if (member tg current)
		      (setq current (delete tg current))
		    (loop for g in groups do
			  (if (member tg g)
			      (mapc (lambda (x)
				      (setq current (delete x current)))
				    g)))
		    (push tg current))
		  (if exit-after-next (setq exit-after-next 'now))))

		;; Create a sorted list
		(setq current
		      (sort current
			    (lambda (a b)
			      (assoc b (cdr (memq (assoc a ntable) ntable))))))
		(if (eq exit-after-next 'now) (throw 'exit t))
		(goto-char (point-min))
		(beginning-of-line 2)
		(delete-region (point) (point-at-eol))
		(org-fast-tag-insert "Current" current c-face)
		(org-set-current-tags-overlay current ov-prefix)
		(while (re-search-forward
			(org-re "\\[.\\] \\([[:alnum:]_@#%]+\\)") nil t)
		  (setq tg (match-string 1))
		  (add-text-properties
		   (match-beginning 1) (match-end 1)
		   (list 'face
			 (cond
			  ((member tg current) c-face)
			  ((member tg inherited) i-face)
			  (t (get-text-property (match-beginning 1) 'face))))))
		(goto-char (point-min)))))
      (org-detach-overlay org-tags-overlay)
      (if rtn
	  (mapconcat 'identity current ":")
	nil))))

(defun org-get-tags-string ()
  "Get the TAGS string in the current headline."
  (unless (org-at-heading-p t)
    (error "Not on a heading"))
  (save-excursion
    (beginning-of-line 1)
    (if (looking-at (org-re ".*[ \t]\\(:[[:alnum:]_@#%:]+:\\)[ \t]*$"))
	(org-match-string-no-properties 1)
      "")))

(defun org-get-tags ()
  "Get the list of tags specified in the current headline."
  (org-split-string (org-get-tags-string) ":"))

(defun org-get-buffer-tags ()
  "Get a table of all tags used in the buffer, for completion."
  (let (tags)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
	      (org-re "[ \t]:\\([[:alnum:]_@#%:]+\\):[ \t\r\n]") nil t)
	(when (equal (char-after (point-at-bol 0)) ?*)
	  (mapc (lambda (x) (add-to-list 'tags x))
		(org-split-string (org-match-string-no-properties 1) ":")))))
    (mapc (lambda (s) (add-to-list 'tags s)) org-file-tags)
    (mapcar 'list tags)))

;;;; The mapping API

;;;###autoload
(defun org-map-entries (func &optional match scope &rest skip)
  "Call FUNC at each headline selected by MATCH in SCOPE.

FUNC is a function or a lisp form.  The function will be called without
arguments, with the cursor positioned at the beginning of the headline.
The return values of all calls to the function will be collected and
returned as a list.

The call to FUNC will be wrapped into a save-excursion form, so FUNC
does not need to preserve point.  After evaluation, the cursor will be
moved to the end of the line (presumably of the headline of the
processed entry) and search continues from there.  Under some
circumstances, this may not produce the wanted results.  For example,
if you have removed (e.g. archived) the current (sub)tree it could
mean that the next entry will be skipped entirely.  In such cases, you
can specify the position from where search should continue by making
FUNC set the variable `org-map-continue-from' to the desired buffer
position.

MATCH is a tags/property/todo match as it is used in the agenda tags view.
Only headlines that are matched by this query will be considered during
the iteration.  When MATCH is nil or t, all headlines will be
visited by the iteration.

SCOPE determines the scope of this command.  It can be any of:

nil     The current buffer, respecting the restriction if any
tree    The subtree started with the entry at point
region  The entries within the active region, if any
region-start-level
        The entries within the active region, but only those at
        the same level than the first one.
file    The current buffer, without restriction
file-with-archives
        The current buffer, and any archives associated with it
agenda  All agenda files
agenda-with-archives
        All agenda files with any archive files associated with them
\(file1 file2 ...)
        If this is a list, all files in the list will be scanned

The remaining args are treated as settings for the skipping facilities of
the scanner.  The following items can be given here:

  archive    skip trees with the archive tag.
  comment    skip trees with the COMMENT keyword
  function or Emacs Lisp form:
             will be used as value for `org-agenda-skip-function', so whenever
             the function returns t, FUNC will not be called for that
             entry and search will continue from the point where the
             function leaves it.

If your function needs to retrieve the tags including inherited tags
at the *current* entry, you can use the value of the variable
`org-scanner-tags' which will be much faster than getting the value
with `org-get-tags-at'.  If your function gets properties with
`org-entry-properties' at the *current* entry, bind `org-trust-scanner-tags'
to t around the call to `org-entry-properties' to get the same speedup.
Note that if your function moves around to retrieve tags and properties at
a *different* entry, you cannot use these techniques."
  (unless (and (or (eq scope 'region) (eq scope 'region-start-level))
	       (not (org-region-active-p)))
    (let* ((org-agenda-archives-mode nil) ; just to make sure
	   (org-agenda-skip-archived-trees (memq 'archive skip))
	   (org-agenda-skip-comment-trees (memq 'comment skip))
	   (org-agenda-skip-function
	    (car (org-delete-all '(comment archive) skip)))
	   (org-tags-match-list-sublevels t)
	   (start-level (eq scope 'region-start-level))
	   matcher file res
	   org-todo-keywords-for-agenda
	   org-done-keywords-for-agenda
	   org-todo-keyword-alist-for-agenda
	   org-drawers-for-agenda
	   org-tag-alist-for-agenda
	   todo-only)

      (cond
       ((eq match t)   (setq matcher t))
       ((eq match nil) (setq matcher t))
       (t (setq matcher (if match (cdr (org-make-tags-matcher match)) t))))

      (save-excursion
	(save-restriction
	  (cond ((eq scope 'tree)
		 (org-back-to-heading t)
		 (org-narrow-to-subtree)
		 (setq scope nil))
		((and (or (eq scope 'region) (eq scope 'region-start-level))
		      (org-region-active-p))
		 ;; If needed, set start-level to a string like "2"
		 (when start-level
		   (save-excursion
		     (goto-char (region-beginning))
		     (unless (org-at-heading-p) (outline-next-heading))
		     (setq start-level (org-current-level))))
		 (narrow-to-region (region-beginning)
				   (save-excursion
				     (goto-char (region-end))
				     (unless (and (bolp) (org-at-heading-p))
				       (outline-next-heading))
				     (point)))
		 (setq scope nil)))

	  (if (not scope)
	      (progn
		(org-prepare-agenda-buffers
		 (list (buffer-file-name (current-buffer))))
		(setq res (org-scan-tags func matcher todo-only start-level)))
	    ;; Get the right scope
	    (cond
	     ((and scope (listp scope) (symbolp (car scope)))
	      (setq scope (eval scope)))
	     ((eq scope 'agenda)
	      (setq scope (org-agenda-files t)))
	     ((eq scope 'agenda-with-archives)
	      (setq scope (org-agenda-files t))
	      (setq scope (org-add-archive-files scope)))
	     ((eq scope 'file)
	      (setq scope (list (buffer-file-name))))
	     ((eq scope 'file-with-archives)
	      (setq scope (org-add-archive-files (list (buffer-file-name))))))
	    (org-prepare-agenda-buffers scope)
	    (while (setq file (pop scope))
	      (with-current-buffer (org-find-base-buffer-visiting file)
		(save-excursion
		  (save-restriction
		    (widen)
		    (goto-char (point-min))
		    (setq res (append res (org-scan-tags func matcher todo-only))))))))))
      res)))

;;;; Properties

;;; Setting and retrieving properties

(defconst org-special-properties
  '("TODO" "TAGS" "ALLTAGS" "DEADLINE" "SCHEDULED" "CLOCK" "CLOSED" "PRIORITY"
    "TIMESTAMP" "TIMESTAMP_IA" "BLOCKED" "FILE" "CLOCKSUM")
  "The special properties valid in Org-mode.

These are properties that are not defined in the property drawer,
but in some other way.")

(defconst org-default-properties
  '("ARCHIVE" "CATEGORY" "SUMMARY" "DESCRIPTION" "CUSTOM_ID"
    "LOCATION" "LOGGING" "COLUMNS" "VISIBILITY"
    "TABLE_EXPORT_FORMAT" "TABLE_EXPORT_FILE"
    "EXPORT_OPTIONS" "EXPORT_TEXT" "EXPORT_FILE_NAME"
    "EXPORT_TITLE" "EXPORT_AUTHOR" "EXPORT_DATE"
    "ORDERED" "NOBLOCKING" "COOKIE_DATA" "LOG_INTO_DRAWER" "REPEAT_TO_STATE"
    "CLOCK_MODELINE_TOTAL" "STYLE" "HTML_CONTAINER_CLASS")
  "Some properties that are used by Org-mode for various purposes.
Being in this list makes sure that they are offered for completion.")

(defconst org-property-start-re "^[ \t]*:PROPERTIES:[ \t]*$"
  "Regular expression matching the first line of a property drawer.")

(defconst org-property-end-re "^[ \t]*:END:[ \t]*$"
  "Regular expression matching the last line of a property drawer.")

(defconst org-clock-drawer-start-re "^[ \t]*:CLOCK:[ \t]*$"
  "Regular expression matching the first line of a property drawer.")

(defconst org-clock-drawer-end-re "^[ \t]*:END:[ \t]*$"
  "Regular expression matching the first line of a property drawer.")

(defconst org-property-drawer-re
  (concat "\\(" org-property-start-re "\\)[^\000]*\\("
	  org-property-end-re "\\)\n?")
  "Matches an entire property drawer.")

(defconst org-clock-drawer-re
  (concat "\\(" org-clock-drawer-start-re "\\)[^\000]*\\("
	  org-property-end-re "\\)\n?")
  "Matches an entire clock drawer.")

(defsubst org-re-property (property)
  "Return a regexp matching PROPERTY.
Match group 1 will be set to the value "
  (concat "^[ \t]*:" (regexp-quote property) ":[ \t]*\\(\\S-.*\\)"))

(defun org-property-action ()
  "Do an action on properties."
  (interactive)
  (let (c)
    (org-at-property-p)
    (message "Property Action:  [s]et  [d]elete  [D]elete globally  [c]ompute")
    (setq c (read-char-exclusive))
    (cond
     ((equal c ?s)
      (call-interactively 'org-set-property))
     ((equal c ?d)
      (call-interactively 'org-delete-property))
     ((equal c ?D)
      (call-interactively 'org-delete-property-globally))
     ((equal c ?c)
      (call-interactively 'org-compute-property-at-point))
     (t (error "No such property action %c" c)))))

(defun org-set-effort (&optional value)
  "Set the effort property of the current entry.
With numerical prefix arg, use the nth allowed value, 0 stands for the 10th
allowed value."
  (interactive "P")
  (if (equal value 0) (setq value 10))
  (let* ((completion-ignore-case t)
	 (prop org-effort-property)
	 (cur (org-entry-get nil prop))
	 (allowed (org-property-get-allowed-values nil prop 'table))
	 (existing (mapcar 'list (org-property-values prop)))
	 rpl
	 (val (cond
	       ((stringp value) value)
	       ((and allowed (integerp value))
		(or (car (nth (1- value) allowed))
		    (car (org-last allowed))))
	       (allowed
		(message "Select 1-9,0, [RET%s]: %s"
			 (if cur (concat "=" cur) "")
			 (mapconcat 'car allowed " "))
		(setq rpl (read-char-exclusive))
		(if (equal rpl ?\r)
		    cur
		  (setq rpl (- rpl ?0))
		  (if (equal rpl 0) (setq rpl 10))
		  (if (and (> rpl 0) (<= rpl (length allowed)))
		      (car (nth (1- rpl) allowed))
		    (org-completing-read "Effort: " allowed nil))))
	       (t
		(let (org-completion-use-ido org-completion-use-iswitchb)
		  (org-completing-read
		   (concat "Effort " (if (and cur (string-match "\\S-" cur))
					(concat "[" cur "]") "")
			   ": ")
		   existing nil nil "" nil cur))))))
    (unless (equal (org-entry-get nil prop) val)
      (org-entry-put nil prop val))
    (message "%s is now %s" prop val)))

(defun org-at-property-p ()
  "Is cursor inside a property drawer?"
  (save-excursion
    (beginning-of-line 1)
    (when (looking-at (org-re "^[ \t]*\\(:\\([[:alpha:]][[:alnum:]_-]*\\):\\)[ \t]*\\(.*\\)"))
      (save-match-data ;; Used by calling procedures
	(let ((p (point))
	      (range (unless (org-before-first-heading-p)
		       (org-get-property-block))))
	  (and range (<= (car range) p) (< p (cdr range))))))))

(defun org-get-property-block (&optional beg end force)
  "Return the (beg . end) range of the body of the property drawer.
BEG and END can be beginning and end of subtree, if not given
they will be found.
If the drawer does not exist and FORCE is non-nil, create the drawer."
  (catch 'exit
    (save-excursion
      (let* ((beg (or beg (progn (org-back-to-heading t) (point))))
	     (end (or end (progn (outline-next-heading) (point)))))
	(goto-char beg)
	(if (re-search-forward org-property-start-re end t)
	    (setq beg (1+ (match-end 0)))
	  (if force
	      (save-excursion
		(org-insert-property-drawer)
		(setq end (progn (outline-next-heading) (point))))
	    (throw 'exit nil))
	  (goto-char beg)
	  (if (re-search-forward org-property-start-re end t)
	      (setq beg (1+ (match-end 0)))))
	(if (re-search-forward org-property-end-re end t)
	    (setq end (match-beginning 0))
	  (or force (throw 'exit nil))
	  (goto-char beg)
	  (setq end beg)
	  (org-indent-line-function)
	  (insert ":END:\n"))
	(cons beg end)))))

(defun org-entry-properties (&optional pom which specific)
  "Get all properties of the entry at point-or-marker POM.
This includes the TODO keyword, the tags, time strings for deadline,
scheduled, and clocking, and any additional properties defined in the
entry.  The return value is an alist, keys may occur multiple times
if the property key was used several times.
POM may also be nil, in which case the current entry is used.
If WHICH is nil or `all', get all properties.  If WHICH is
`special' or `standard', only get that subclass.  If WHICH
is a string only get exactly this property.  SPECIFIC can be a string, the
specific property we are interested in.  Specifying it can speed
things up because then unnecessary parsing is avoided."
  (setq which (or which 'all))
  (org-with-point-at pom
    (let ((clockstr (substring org-clock-string 0 -1))
	  (excluded '("TODO" "TAGS" "ALLTAGS" "PRIORITY" "BLOCKED"))
	  (case-fold-search nil)
	  beg end range props sum-props key key1 value string clocksum)
      (save-excursion
	(when (condition-case nil
		  (and (eq major-mode 'org-mode) (org-back-to-heading t))
		(error nil))
	  (setq beg (point))
	  (setq sum-props (get-text-property (point) 'org-summaries))
	  (setq clocksum (get-text-property (point) :org-clock-minutes))
	  (outline-next-heading)
	  (setq end (point))
	  (when (memq which '(all special))
	    ;; Get the special properties, like TODO and tags
	    (goto-char beg)
	    (when (and (or (not specific) (string= specific "TODO"))
		       (looking-at org-todo-line-regexp) (match-end 2))
	      (push (cons "TODO" (org-match-string-no-properties 2)) props))
	    (when (and (or (not specific) (string= specific "PRIORITY"))
		       (looking-at org-priority-regexp))
	      (push (cons "PRIORITY" (org-match-string-no-properties 2)) props))
	    (when (or (not specific) (string= specific "FILE"))
	      (push (cons "FILE" buffer-file-name) props))
	    (when (and (or (not specific) (string= specific "TAGS"))
		       (setq value (org-get-tags-string))
		       (string-match "\\S-" value))
	      (push (cons "TAGS" value) props))
	    (when (and (or (not specific) (string= specific "ALLTAGS"))
		       (setq value (org-get-tags-at)))
	      (push (cons "ALLTAGS" (concat ":" (mapconcat 'identity value ":")
					    ":"))
		    props))
	    (when (or (not specific) (string= specific "BLOCKED"))
	      (push (cons "BLOCKED" (if (org-entry-blocked-p) "t" "")) props))
	    (when (or (not specific)
		      (member specific
			      '("SCHEDULED" "DEADLINE" "CLOCK" "CLOSED"
				"TIMESTAMP" "TIMESTAMP_IA")))
	      (catch 'match
		(while (re-search-forward org-maybe-keyword-time-regexp end t)
		  (setq key (if (match-end 1)
				(substring (org-match-string-no-properties 1)
					   0 -1))
			string (if (equal key clockstr)
				   (org-no-properties
				    (org-trim
				     (buffer-substring
				      (match-beginning 3) (goto-char
							   (point-at-eol)))))
				 (substring (org-match-string-no-properties 3)
					    1 -1)))
		  ;; Get the correct property name from the key.  This is
		  ;; necessary if the user has configured time keywords.
		  (setq key1 (concat key ":"))
		  (cond
		   ((not key)
		    (setq key
			  (if (= (char-after (match-beginning 3)) ?\[)
			      "TIMESTAMP_IA" "TIMESTAMP")))
		   ((equal key1 org-scheduled-string) (setq key "SCHEDULED"))
		   ((equal key1 org-deadline-string)  (setq key "DEADLINE"))
		   ((equal key1 org-closed-string)    (setq key "CLOSED"))
		   ((equal key1 org-clock-string)     (setq key "CLOCK")))
		  (if (and specific (equal key specific) (not (equal key "CLOCK")))
		      (progn
			(push (cons key string) props)
			;; no need to search further if match is found
			(throw 'match t))
		    (when (or (equal key "CLOCK") (not (assoc key props)))
		      (push (cons key string) props))))))
	    )

	  (when (memq which '(all standard))
	    ;; Get the standard properties, like :PROP: ...
	    (setq range (org-get-property-block beg end))
	    (when range
	      (goto-char (car range))
	      (while (re-search-forward
		      (org-re "^[ \t]*:\\([[:alpha:]][[:alnum:]_-]*\\):[ \t]*\\(\\S-.*\\)?")
		      (cdr range) t)
		(setq key (org-match-string-no-properties 1)
		      value (org-trim (or (org-match-string-no-properties 2) "")))
		(unless (member key excluded)
		  (push (cons key (or value "")) props)))))
	  (if clocksum
	      (push (cons "CLOCKSUM"
			  (org-columns-number-to-string (/ (float clocksum) 60.)
						       'add_times))
		    props))
	  (unless (assoc "CATEGORY" props)
	    (push (cons "CATEGORY" (org-get-category)) props))
	  (append sum-props (nreverse props)))))))

(defun org-entry-get (pom property &optional inherit literal-nil)
  "Get value of PROPERTY for entry at point-or-marker POM.
If INHERIT is non-nil and the entry does not have the property,
then also check higher levels of the hierarchy.
If INHERIT is the symbol `selective', use inheritance only if the setting
in `org-use-property-inheritance' selects PROPERTY for inheritance.
If the property is present but empty, the return value is the empty string.
If the property is not present at all, nil is returned.

If LITERAL-NIL is set, return the string value \"nil\" as a string,
do not interpret it as the list atom nil.  This is used for inheritance
when a \"nil\" value can supersede a non-nil value higher up the hierarchy."
  (org-with-point-at pom
    (if (and inherit (if (eq inherit 'selective)
			 (org-property-inherit-p property)
		       t))
	(org-entry-get-with-inheritance property literal-nil)
      (if (member property org-special-properties)
	  ;; We need a special property.  Use `org-entry-properties' to
	  ;; retrieve it, but specify the wanted property
	  (cdr (assoc property (org-entry-properties nil 'special property)))
	(let ((range (unless (org-before-first-heading-p)
		       (org-get-property-block)))
	      (props (list (or (assoc property org-file-properties)
			       (assoc property org-global-properties)
			       (assoc property org-global-properties-fixed))))
	      val)
	  (flet ((ap (key)
		     (when (re-search-forward
			    (org-re-property key) (cdr range) t)
		       (setq props
			     (org-update-property-plist
			      key
			      (if (match-end 1)
				  (org-match-string-no-properties 1) "")
			      props)))))
	    (when (and range (goto-char (car range)))
	      (ap property)
	      (goto-char (car range))
	      (while (ap (concat property "+")))
	      (setq val (cdr (assoc property props)))
	      (when val (if literal-nil val (org-not-nil val))))))))))

(defun org-property-or-variable-value (var &optional inherit)
  "Check if there is a property fixing the value of VAR.
If yes, return this value.  If not, return the current value of the variable."
  (let ((prop (org-entry-get nil (symbol-name var) inherit)))
    (if (and prop (stringp prop) (string-match "\\S-" prop))
	(read prop)
      (symbol-value var))))

(defun org-entry-delete (pom property)
  "Delete the property PROPERTY from entry at point-or-marker POM."
  (org-with-point-at pom
    (if (member property org-special-properties)
	nil ; cannot delete these properties.
      (let ((range (org-get-property-block)))
	(if (and range
		 (goto-char (car range))
		 (re-search-forward
		  (org-re-property property)
		  (cdr range) t))
	    (progn
	      (delete-region (match-beginning 0) (1+ (point-at-eol)))
	      t)
	  nil)))))

;; Multi-values properties are properties that contain multiple values
;; These values are assumed to be single words, separated by whitespace.
(defun org-entry-add-to-multivalued-property (pom property value)
  "Add VALUE to the words in the PROPERTY in entry at point-or-marker POM."
  (let* ((old (org-entry-get pom property))
	 (values (and old (org-split-string old "[ \t]"))))
    (setq value (org-entry-protect-space value))
    (unless (member value values)
      (setq values (cons value values))
      (org-entry-put pom property
		     (mapconcat 'identity values " ")))))

(defun org-entry-remove-from-multivalued-property (pom property value)
  "Remove VALUE from words in the PROPERTY in entry at point-or-marker POM."
  (let* ((old (org-entry-get pom property))
	 (values (and old (org-split-string old "[ \t]"))))
    (setq value (org-entry-protect-space value))
    (when (member value values)
      (setq values (delete value values))
      (org-entry-put pom property
		     (mapconcat 'identity values " ")))))

(defun org-entry-member-in-multivalued-property (pom property value)
  "Is VALUE one of the words in the PROPERTY in entry at point-or-marker POM?"
  (let* ((old (org-entry-get pom property))
	 (values (and old (org-split-string old "[ \t]"))))
    (setq value (org-entry-protect-space value))
    (member value values)))

(defun org-entry-get-multivalued-property (pom property)
  "Return a list of values in a multivalued property."
  (let* ((value (org-entry-get pom property))
	 (values (and value (org-split-string value "[ \t]"))))
    (mapcar 'org-entry-restore-space values)))

(defun org-entry-put-multivalued-property (pom property &rest values)
  "Set multivalued PROPERTY at point-or-marker POM to VALUES.
VALUES should be a list of strings.  Spaces will be protected."
  (org-entry-put pom property
		 (mapconcat 'org-entry-protect-space values " "))
  (let* ((value (org-entry-get pom property))
	 (values (and value (org-split-string value "[ \t]"))))
    (mapcar 'org-entry-restore-space values)))

(defun org-entry-protect-space (s)
  "Protect spaces and newline in string S."
  (while (string-match " " s)
    (setq s (replace-match "%20" t t s)))
  (while (string-match "\n" s)
    (setq s (replace-match "%0A" t t s)))
  s)

(defun org-entry-restore-space (s)
  "Restore spaces and newline in string S."
  (while (string-match "%20" s)
    (setq s (replace-match " " t t s)))
  (while (string-match "%0A" s)
    (setq s (replace-match "\n" t t s)))
  s)

(defvar org-entry-property-inherited-from (make-marker)
  "Marker pointing to the entry from where a property was inherited.
Each call to `org-entry-get-with-inheritance' will set this marker to the
location of the entry where the inheritance search matched.  If there was
no match, the marker will point nowhere.
Note that also `org-entry-get' calls this function, if the INHERIT flag
is set.")

(defun org-entry-get-with-inheritance (property &optional literal-nil)
  "Get entry property, and search higher levels if not present.
The search will stop at the first ancestor which has the property defined.
If the value found is \"nil\", return nil to show that the property
should be considered as undefined (this is the meaning of nil here).
However, if LITERAL-NIL is set, return the string value \"nil\" instead."
  (move-marker org-entry-property-inherited-from nil)
  (let (tmp)
    (unless (org-before-first-heading-p)
      (save-excursion
	(save-restriction
	  (widen)
	  (catch 'ex
	    (while t
	      (when (setq tmp (org-entry-get nil property nil 'literal-nil))
		(org-back-to-heading t)
		(move-marker org-entry-property-inherited-from (point))
		(throw 'ex tmp))
	      (or (org-up-heading-safe) (throw 'ex nil)))))))
    (setq tmp (or tmp
		  (cdr (assoc property org-file-properties))
		  (cdr (assoc property org-global-properties))
		  (cdr (assoc property org-global-properties-fixed))))
    (if literal-nil tmp (org-not-nil tmp))))

(defvar org-property-changed-functions nil
  "Hook called when the value of a property has changed.
Each hook function should accept two arguments, the name of the property
and the new value.")

(defun org-entry-put (pom property value)
  "Set PROPERTY to VALUE for entry at point-or-marker POM."
  (org-with-point-at pom
    (org-back-to-heading t)
    (let ((beg (point)) (end (save-excursion (outline-next-heading) (point)))
	  range)
      (cond
       ((equal property "TODO")
	(when (and (stringp value) (string-match "\\S-" value)
		   (not (member value org-todo-keywords-1)))
	  (error "\"%s\" is not a valid TODO state" value))
	(if (or (not value)
		(not (string-match "\\S-" value)))
	    (setq value 'none))
	(org-todo value)
	(org-set-tags nil 'align))
       ((equal property "PRIORITY")
	(org-priority (if (and value (stringp value) (string-match "\\S-" value))
			       (string-to-char value) ?\ ))
	(org-set-tags nil 'align))
       ((equal property "SCHEDULED")
	(if (re-search-forward org-scheduled-time-regexp end t)
	    (cond
	     ((eq value 'earlier) (org-timestamp-change -1 'day))
	     ((eq value 'later) (org-timestamp-change 1 'day))
	     (t (call-interactively 'org-schedule)))
	  (call-interactively 'org-schedule)))
       ((equal property "DEADLINE")
	(if (re-search-forward org-deadline-time-regexp end t)
	    (cond
	     ((eq value 'earlier) (org-timestamp-change -1 'day))
	     ((eq value 'later) (org-timestamp-change 1 'day))
	     (t (call-interactively 'org-deadline)))
	  (call-interactively 'org-deadline)))
       ((member property org-special-properties)
	(error "The %s property can not yet be set with `org-entry-put'"
	       property))
       (t ; a non-special property
	(let ((buffer-invisibility-spec (org-inhibit-invisibility))) ; Emacs 21
	  (setq range (org-get-property-block beg end 'force))
	  (goto-char (car range))
	  (if (re-search-forward
	       (org-re-property property) (cdr range) t)
	      (progn
		(delete-region (match-beginning 0) (match-end 0))
		(goto-char (match-beginning 0)))
	    (goto-char (cdr range))
	    (insert "\n")
	    (backward-char 1)
	    (org-indent-line-function))
	  (insert ":" property ":")
	  (and value (insert " " value))
	  (org-indent-line-function)))))
    (run-hook-with-args 'org-property-changed-functions property value)))

(defun org-buffer-property-keys (&optional include-specials include-defaults include-columns)
  "Get all property keys in the current buffer.
With INCLUDE-SPECIALS, also list the special properties that reflect things
like tags and TODO state.
With INCLUDE-DEFAULTS, also include properties that has special meaning
internally: ARCHIVE, CATEGORY, SUMMARY, DESCRIPTION, LOCATION, and LOGGING
and others.
With INCLUDE-COLUMNS, also include property names given in COLUMN
formats in the current buffer."
  (let (rtn range cfmt s p)
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(while (re-search-forward org-property-start-re nil t)
	  (setq range (org-get-property-block))
	  (goto-char (car range))
	  (while (re-search-forward
		  (org-re "^[ \t]*:\\([-[:alnum:]_]+\\):")
		  (cdr range) t)
	    (add-to-list 'rtn (org-match-string-no-properties 1)))
	  (outline-next-heading))))

    (when include-specials
      (setq rtn (append org-special-properties rtn)))

    (when include-defaults
      (mapc (lambda (x) (add-to-list 'rtn x)) org-default-properties)
      (add-to-list 'rtn org-effort-property))

    (when include-columns
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char (point-min))
	  (while (re-search-forward
		  "^\\(#\\+COLUMNS:\\|[ \t]*:COLUMNS:\\)[ \t]*\\(.*\\)"
		  nil t)
	    (setq cfmt (match-string 2) s 0)
	    (while (string-match (org-re "%[0-9]*\\([-[:alnum:]_]+\\)")
				 cfmt s)
	      (setq s (match-end 0)
		    p (match-string 1 cfmt))
	      (unless (or (equal p "ITEM")
			  (member p org-special-properties))
		(add-to-list 'rtn (match-string 1 cfmt))))))))

    (sort rtn (lambda (a b) (string< (upcase a) (upcase b))))))

(defun org-property-values (key)
  "Return a list of all values of property KEY in the current buffer."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((re (org-re-property key))
	    values)
	(while (re-search-forward re nil t)
	  (add-to-list 'values (org-trim (match-string 1))))
	(delete "" values)))))

(defun org-insert-property-drawer ()
  "Insert a property drawer into the current entry."
  (interactive)
  (org-back-to-heading t)
  (looking-at org-outline-regexp)
  (let ((indent (if org-adapt-indentation
		    (- (match-end 0)(match-beginning 0))
		  0))
	(beg (point))
	(re (concat "^[ \t]*" org-keyword-time-regexp))
	end hiddenp)
    (outline-next-heading)
    (setq end (point))
    (goto-char beg)
    (while (re-search-forward re end t))
    (setq hiddenp (outline-invisible-p))
    (end-of-line 1)
    (and (equal (char-after) ?\n) (forward-char 1))
    (while (looking-at "^[ \t]*\\(:CLOCK:\\|:LOGBOOK:\\|CLOCK:\\|:END:\\)")
      (if (member (match-string 1) '("CLOCK:" ":END:"))
	  ;; just skip this line
	  (beginning-of-line 2)
	;; Drawer start, find the end
	(re-search-forward "^\\*+ \\|^[ \t]*:END:" nil t)
	(beginning-of-line 1)))
    (org-skip-over-state-notes)
    (skip-chars-backward " \t\n\r")
    (if (eq (char-before) ?*) (forward-char 1))
    (let ((inhibit-read-only t)) (insert "\n:PROPERTIES:\n:END:"))
    (beginning-of-line 0)
    (org-indent-to-column indent)
    (beginning-of-line 2)
    (org-indent-to-column indent)
    (beginning-of-line 0)
    (if hiddenp
	(save-excursion
	  (org-back-to-heading t)
	  (hide-entry))
      (org-flag-drawer t))))

(defvar org-property-set-functions-alist nil
  "Property set function alist.
Each entry should have the following format:

 (PROPERTY . READ-FUNCTION)

The read function will be called with the same argument as
`org-completing-read'.")

(defun org-set-property-function (property)
  "Get the function that should be used to set PROPERTY.
This is computed according to `org-property-set-functions-alist'."
  (or (cdr (assoc property org-property-set-functions-alist))
      'org-completing-read))

(defun org-read-property-value (property)
  "Read PROPERTY value from user."
  (let* ((completion-ignore-case t)
	 (allowed (org-property-get-allowed-values nil property 'table))
	 (cur (org-entry-get nil property))
	 (prompt (concat property " value"
			 (if (and cur (string-match "\\S-" cur))
			     (concat " [" cur "]") "") ": "))
	 (set-function (org-set-property-function property))
	 (val (if allowed
		  (funcall set-function prompt allowed nil
			   (not (get-text-property 0 'org-unrestricted
						   (caar allowed))))
		(let (org-completion-use-ido org-completion-use-iswitchb)
		  (funcall set-function prompt
			   (mapcar 'list (org-property-values property))
			   nil nil "" nil cur)))))
    (if (equal val "")
	cur
      val)))

(defvar org-last-set-property nil)
(defun org-read-property-name ()
  "Read a property name."
  (let* ((completion-ignore-case t)
	 (keys (org-buffer-property-keys nil t t))
	 (default-prop (or (save-excursion
			     (save-match-data
			       (beginning-of-line)
			       (and (looking-at "^\\s-*:\\([^:\n]+\\):")
				    (null (string= (match-string 1) "END"))
				    (match-string 1))))
			   org-last-set-property))
	 (property (org-icompleting-read
		    (concat "Property"
			    (if default-prop (concat " [" default-prop "]") "")
			    ": ")
		    (mapcar 'list keys)
		    nil nil nil nil
		    default-prop
		    )))
    (if (member property keys)
	property
      (or (cdr (assoc (downcase property)
		      (mapcar (lambda (x) (cons (downcase x) x))
			      keys)))
	  property))))

(defun org-set-property (property value)
  "In the current entry, set PROPERTY to VALUE.
When called interactively, this will prompt for a property name, offering
completion on existing and default properties.  And then it will prompt
for a value, offering completion either on allowed values (via an inherited
xxx_ALL property) or on existing values in other instances of this property
in the current file."
  (interactive (list nil nil))
  (let* ((property (or property (org-read-property-name)))
	 (value (or value (org-read-property-value property)))
	 (fn (assoc property org-properties-postprocess-alist)))
    (setq org-last-set-property property)
    ;; Possibly postprocess the inserted value:
    (when fn (setq value (funcall (cadr fn) value)))
    (unless (equal (org-entry-get nil property) value)
      (org-entry-put nil property value))))

(defun org-delete-property (property)
  "In the current entry, delete PROPERTY."
  (interactive
   (let* ((completion-ignore-case t)
	  (prop (org-icompleting-read "Property: "
				      (org-entry-properties nil 'standard))))
     (list prop)))
  (message "Property %s %s" property
	   (if (org-entry-delete nil property)
	       "deleted"
	     "was not present in the entry")))

(defun org-delete-property-globally (property)
  "Remove PROPERTY globally, from all entries."
  (interactive
   (let* ((completion-ignore-case t)
	  (prop (org-icompleting-read
		 "Globally remove property: "
		 (mapcar 'list (org-buffer-property-keys)))))
     (list prop)))
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((cnt 0))
	(while (re-search-forward
		(org-re-property property)
		nil t)
	  (setq cnt (1+ cnt))
	  (delete-region (match-beginning 0) (1+ (point-at-eol))))
	(message "Property \"%s\" removed from %d entries" property cnt)))))

(defvar org-columns-current-fmt-compiled) ; defined in org-colview.el

(defun org-compute-property-at-point ()
  "Compute the property at point.
This looks for an enclosing column format, extracts the operator and
then applies it to the property in the column format's scope."
  (interactive)
  (unless (org-at-property-p)
    (error "Not at a property"))
  (let ((prop (org-match-string-no-properties 2)))
    (org-columns-get-format-and-top-level)
    (unless (nth 3 (assoc prop org-columns-current-fmt-compiled))
      (error "No operator defined for property %s" prop))
    (org-columns-compute prop)))

(defvar org-property-allowed-value-functions nil
  "Hook for functions supplying allowed values for a specific property.
The functions must take a single argument, the name of the property, and
return a flat list of allowed values.  If \":ETC\" is one of
the values, this means that these values are intended as defaults for
completion, but that other values should be allowed too.
The functions must return nil if they are not responsible for this
property.")

(defun org-property-get-allowed-values (pom property &optional table)
  "Get allowed values for the property PROPERTY.
When TABLE is non-nil, return an alist that can directly be used for
completion."
  (let (vals)
    (cond
     ((equal property "TODO")
      (setq vals (org-with-point-at pom
		   (append org-todo-keywords-1 '("")))))
     ((equal property "PRIORITY")
      (let ((n org-lowest-priority))
	(while (>= n org-highest-priority)
	  (push (char-to-string n) vals)
	  (setq n (1- n)))))
     ((member property org-special-properties))
     ((setq vals (run-hook-with-args-until-success
		  'org-property-allowed-value-functions property)))
     (t
      (setq vals (org-entry-get pom (concat property "_ALL") 'inherit))
      (when (and vals (string-match "\\S-" vals))
	(setq vals (car (read-from-string (concat "(" vals ")"))))
	(setq vals (mapcar (lambda (x)
			     (cond ((stringp x) x)
				   ((numberp x) (number-to-string x))
				   ((symbolp x) (symbol-name x))
				   (t "???")))
			   vals)))))
    (when (member ":ETC" vals)
      (setq vals (remove ":ETC" vals))
      (org-add-props (car vals) '(org-unrestricted t)))
    (if table (mapcar 'list vals) vals)))

(defun org-property-previous-allowed-value (&optional previous)
  "Switch to the next allowed value for this property."
  (interactive)
  (org-property-next-allowed-value t))

(defun org-property-next-allowed-value (&optional previous)
  "Switch to the next allowed value for this property."
  (interactive)
  (unless (org-at-property-p)
    (error "Not at a property"))
  (let* ((key (match-string 2))
	 (value (match-string 3))
	 (allowed (or (org-property-get-allowed-values (point) key)
		      (and (member value  '("[ ]" "[-]" "[X]"))
			   '("[ ]" "[X]"))))
	 nval)
    (unless allowed
      (error "Allowed values for this property have not been defined"))
    (if previous (setq allowed (reverse allowed)))
    (if (member value allowed)
	(setq nval (car (cdr (member value allowed)))))
    (setq nval (or nval (car allowed)))
    (if (equal nval value)
	(error "Only one allowed value for this property"))
    (org-at-property-p)
    (replace-match (concat " :" key ": " nval) t t)
    (org-indent-line-function)
    (beginning-of-line 1)
    (skip-chars-forward " \t")
    (run-hook-with-args 'org-property-changed-functions key nval)))

(defun org-find-olp (path &optional this-buffer)
  "Return a marker pointing to the entry at outline path OLP.
If anything goes wrong, throw an error.
You can wrap this call to catch the error like this:

  (condition-case msg
      (org-mobile-locate-entry (match-string 4))
    (error (nth 1 msg)))

The return value will then be either a string with the error message,
or a marker if everything is OK.

If THIS-BUFFER is set, the outline path does not contain a file,
only headings."
  (let* ((file (if this-buffer buffer-file-name (pop path)))
	 (buffer (if this-buffer (current-buffer) (find-file-noselect file)))
	 (level 1)
	 (lmin 1)
	 (lmax 1)
	 limit re end found pos heading cnt flevel)
    (unless buffer (error "File not found :%s" file))
    (with-current-buffer buffer
      (save-excursion
	(save-restriction
	  (widen)
	  (setq limit (point-max))
	  (goto-char (point-min))
	  (while (setq heading (pop path))
	    (setq re (format org-complex-heading-regexp-format
			     (regexp-quote heading)))
	    (setq cnt 0 pos (point))
	    (while (re-search-forward re end t)
	      (setq level (- (match-end 1) (match-beginning 1)))
	      (if (and (>= level lmin) (<= level lmax))
		  (setq found (match-beginning 0) flevel level cnt (1+ cnt))))
	    (when (= cnt 0) (error "Heading not found on level %d: %s"
				   lmax heading))
	    (when (> cnt 1) (error "Heading not unique on level %d: %s"
				   lmax heading))
	    (goto-char found)
	    (setq lmin (1+ flevel) lmax (+ lmin (if org-odd-levels-only 1 0)))
	    (setq end (save-excursion (org-end-of-subtree t t))))
	  (when (org-at-heading-p)
	    (move-marker (make-marker) (point))))))))

(defun org-find-exact-headline-in-buffer (heading &optional buffer pos-only)
  "Find node HEADING in BUFFER.
Return a marker to the heading if it was found, or nil if not.
If POS-ONLY is set, return just the position instead of a marker.

The heading text must match exact, but it may have a TODO keyword,
a priority cookie and tags in the standard locations."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(let (case-fold-search)
	  (if (re-search-forward
	       (format org-complex-heading-regexp-format
		       (regexp-quote heading)) nil t)
	      (if pos-only
		  (match-beginning 0)
		(move-marker (make-marker) (match-beginning 0)))))))))

(defun org-find-exact-heading-in-directory (heading &optional dir)
  "Find Org node headline HEADING in all .org files in directory DIR.
When the target headline is found, return a marker to this location."
  (let ((files (directory-files (or dir default-directory)
				nil "\\`[^.#].*\\.org\\'"))
        file visiting m buffer)
    (catch 'found
      (while (setq file (pop files))
        (message "trying %s" file)
        (setq visiting (org-find-base-buffer-visiting file))
        (setq buffer (or visiting (find-file-noselect file)))
        (setq m (org-find-exact-headline-in-buffer
                 heading buffer))
        (when (and (not m) (not visiting)) (kill-buffer buffer))
        (and m (throw 'found m))))))

(defun org-find-entry-with-id (ident)
  "Locate the entry that contains the ID property with exact value IDENT.
IDENT can be a string, a symbol or a number, this function will search for
the string representation of it.
Return the position where this entry starts, or nil if there is no such entry."
  (interactive "sID: ")
  (let ((id (cond
	     ((stringp ident) ident)
	     ((symbol-name ident) (symbol-name ident))
	     ((numberp ident) (number-to-string ident))
	     (t (error "IDENT %s must be a string, symbol or number" ident))))
	(case-fold-search nil))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(when (re-search-forward
	       (concat "^[ \t]*:ID:[ \t]+" (regexp-quote id) "[ \t]*$")
	       nil t)
	  (org-back-to-heading t)
	  (point))))))

;;;; Timestamps

(defvar org-last-changed-timestamp nil)
(defvar org-last-inserted-timestamp nil
  "The last time stamp inserted with `org-insert-time-stamp'.")
(defvar org-time-was-given) ; dynamically scoped parameter
(defvar org-end-time-was-given) ; dynamically scoped parameter
(defvar org-ts-what) ; dynamically scoped parameter

(defun org-time-stamp (arg &optional inactive)
  "Prompt for a date/time and insert a time stamp.
If the user specifies a time like HH:MM, or if this command is called
with a prefix argument, the time stamp will contain date and time.
Otherwise, only the date will be included.  All parts of a date not
specified by the user will be filled in from the current date/time.
So if you press just return without typing anything, the time stamp
will represent the current date/time.  If there is already a timestamp
at the cursor, it will be modified."
  (interactive "P")
  (let* ((ts nil)
	 (default-time
	   ;; Default time is either today, or, when entering a range,
	   ;; the range start.
	   (if (or (and (org-at-timestamp-p t) (setq ts (match-string 0)))
		   (save-excursion
		     (re-search-backward
		      (concat org-ts-regexp "--?-?\\=") ; 1-3 minuses
		      (- (point) 20) t)))
	       (apply 'encode-time (org-parse-time-string (match-string 1)))
	     (current-time)))
	 (default-input (and ts (org-get-compact-tod ts)))
	 (repeater (save-excursion
	 	     (save-match-data
	 	       (beginning-of-line)
	 	       (when (re-search-forward
			      "\\([.+-]+[0-9]+[dwmy] ?\\)+" ;;\\(?:[/ ][-+]?[0-9]+[dwmy]\\)?\\) ?"
			      (save-excursion (progn (end-of-line) (point))) t)
			 (match-string 0)))))
	 org-time-was-given org-end-time-was-given time)
    (cond
     ((and (org-at-timestamp-p t)
	   (memq last-command '(org-time-stamp org-time-stamp-inactive))
	   (memq this-command '(org-time-stamp org-time-stamp-inactive)))
      (insert "--")
      (setq time (let ((this-command this-command))
		  (org-read-date arg 'totime nil nil
				 default-time default-input)))
      (org-insert-time-stamp time (or org-time-was-given arg) inactive))
     ((org-at-timestamp-p t)
      (setq time (let ((this-command this-command))
		   (org-read-date arg 'totime nil nil default-time default-input)))
      (when (org-at-timestamp-p t) ; just to get the match data
;	(setq inactive (eq (char-after (match-beginning 0)) ?\[))
	(replace-match "")
	(setq org-last-changed-timestamp
	      (org-insert-time-stamp
	       time (or org-time-was-given arg)
	       inactive nil nil (list org-end-time-was-given)))
	(when repeater (goto-char (1- (point))) (insert " " repeater)
	      (setq org-last-changed-timestamp
		    (concat (substring org-last-inserted-timestamp 0 -1)
			    " " repeater ">"))))
      (message "Timestamp updated"))
     (t
      (setq time (let ((this-command this-command))
		   (org-read-date arg 'totime nil nil default-time default-input)))
      (org-insert-time-stamp time (or org-time-was-given arg) inactive
			     nil nil (list org-end-time-was-given))))))

;; FIXME: can we use this for something else, like computing time differences?
(defun org-get-compact-tod (s)
  (when (string-match "\\(\\([012]?[0-9]\\):\\([0-5][0-9]\\)\\)\\(-\\(\\([012]?[0-9]\\):\\([0-5][0-9]\\)\\)\\)?" s)
    (let* ((t1 (match-string 1 s))
	   (h1 (string-to-number (match-string 2 s)))
	   (m1 (string-to-number (match-string 3 s)))
	   (t2 (and (match-end 4) (match-string 5 s)))
	   (h2 (and t2 (string-to-number (match-string 6 s))))
	   (m2 (and t2 (string-to-number (match-string 7 s))))
	   dh dm)
      (if (not t2)
	  t1
	(setq dh (- h2 h1) dm (- m2 m1))
	(if (< dm 0) (setq dm (+ dm 60) dh (1- dh)))
	(concat t1 "+" (number-to-string dh)
		(if (/= 0 dm) (concat ":" (number-to-string dm))))))))

(defun org-time-stamp-inactive (&optional arg)
  "Insert an inactive time stamp.
An inactive time stamp is enclosed in square brackets instead of angle
brackets.  It is inactive in the sense that it does not trigger agenda entries,
does not link to the calendar and cannot be changed with the S-cursor keys.
So these are more for recording a certain time/date."
  (interactive "P")
  (org-time-stamp arg 'inactive))

(defvar org-date-ovl (make-overlay 1 1))
(overlay-put org-date-ovl 'face 'org-date-selected)
(org-detach-overlay org-date-ovl)

(defvar org-ans1) ; dynamically scoped parameter
(defvar org-ans2) ; dynamically scoped parameter

(defvar org-plain-time-of-day-regexp) ; defined below

(defvar org-overriding-default-time nil) ; dynamically scoped
(defvar org-read-date-overlay nil)
(defvar org-dcst nil) ; dynamically scoped
(defvar org-read-date-history nil)
(defvar org-read-date-final-answer nil)
(defvar org-read-date-analyze-futurep nil)
(defvar org-read-date-analyze-forced-year nil)

(defun org-read-date (&optional org-with-time to-time from-string prompt
				default-time default-input)
  "Read a date, possibly a time, and make things smooth for the user.
The prompt will suggest to enter an ISO date, but you can also enter anything
which will at least partially be understood by `parse-time-string'.
Unrecognized parts of the date will default to the current day, month, year,
hour and minute.  If this command is called to replace a timestamp at point,
or to enter the second timestamp of a range, the default time is taken
from the existing stamp.  Furthermore, the command prefers the future,
so if you are giving a date where the year is not given, and the day-month
combination is already past in the current year, it will assume you
mean next year.  For details, see the manual.  A few examples:

  3-2-5         --> 2003-02-05
  feb 15        --> currentyear-02-15
  2/15          --> currentyear-02-15
  sep 12 9      --> 2009-09-12
  12:45         --> today 12:45
  22 sept 0:34  --> currentyear-09-22 0:34
  12            --> currentyear-currentmonth-12
  Fri           --> nearest Friday (today or later)
  etc.

Furthermore you can specify a relative date by giving, as the *first* thing
in the input:  a plus/minus sign, a number and a letter [dwmy] to indicate
change in days weeks, months, years.
With a single plus or minus, the date is relative to today.  With a double
plus or minus, it is relative to the date in DEFAULT-TIME.  E.g.
  +4d           --> four days from today
  +4            --> same as above
  +2w           --> two weeks from today
  ++5           --> five days from default date

The function understands only English month and weekday abbreviations.

While prompting, a calendar is popped up - you can also select the
date with the mouse (button 1).  The calendar shows a period of three
months.  To scroll it to other months, use the keys `>' and `<'.
If you don't like the calendar, turn it off with
       \(setq org-read-date-popup-calendar nil)

With optional argument TO-TIME, the date will immediately be converted
to an internal time.
With an optional argument WITH-TIME, the prompt will suggest to also
insert a time.  Note that when WITH-TIME is not set, you can still
enter a time, and this function will inform the calling routine about
this change.  The calling routine may then choose to change the format
used to insert the time stamp into the buffer to include the time.
With optional argument FROM-STRING, read from this string instead from
the user.  PROMPT can overwrite the default prompt.  DEFAULT-TIME is
the time/date that is used for everything that is not specified by the
user."
  (require 'parse-time)
  (let* ((org-time-stamp-rounding-minutes
	  (if (equal org-with-time '(16)) '(0 0) org-time-stamp-rounding-minutes))
	 (org-dcst org-display-custom-times)
	 (ct (org-current-time))
	 (org-def (or org-overriding-default-time default-time ct))
	 (org-defdecode (decode-time org-def))
	 (dummy (progn
		  (when (< (nth 2 org-defdecode) org-extend-today-until)
		    (setcar (nthcdr 2 org-defdecode) -1)
		    (setcar (nthcdr 1 org-defdecode) 59)
		    (setq org-def (apply 'encode-time org-defdecode)
			  org-defdecode (decode-time org-def)))))
	 (calendar-frame-setup nil)
	 (calendar-setup nil)
	 (calendar-move-hook nil)
	 (calendar-view-diary-initially-flag nil)
	 (calendar-view-holidays-initially-flag nil)
	 (timestr (format-time-string
		   (if org-with-time "%Y-%m-%d %H:%M" "%Y-%m-%d") org-def))
	 (prompt (concat (if prompt (concat prompt " ") "")
			 (format "Date+time [%s]: " timestr)))
	 ans (org-ans0 "") org-ans1 org-ans2 final)

    (cond
     (from-string (setq ans from-string))
     (org-read-date-popup-calendar
      (save-excursion
	(save-window-excursion
	  (calendar)
          (unwind-protect
              (progn
		(calendar-forward-day (- (time-to-days org-def)
					 (calendar-absolute-from-gregorian
					  (calendar-current-date))))
		(org-eval-in-calendar nil t)
		(let* ((old-map (current-local-map))
		       (map (copy-keymap calendar-mode-map))
		       (minibuffer-local-map (copy-keymap minibuffer-local-map)))
		  (org-defkey map (kbd "RET") 'org-calendar-select)
		  (org-defkey map [mouse-1] 'org-calendar-select-mouse)
		  (org-defkey map [mouse-2] 'org-calendar-select-mouse)
		  (org-defkey minibuffer-local-map [(meta shift left)]
			      (lambda () (interactive)
				(org-eval-in-calendar '(calendar-backward-month 1))))
		  (org-defkey minibuffer-local-map [(meta shift right)]
			      (lambda () (interactive)
				(org-eval-in-calendar '(calendar-forward-month 1))))
		  (org-defkey minibuffer-local-map [(meta shift up)]
			      (lambda () (interactive)
				(org-eval-in-calendar '(calendar-backward-year 1))))
		  (org-defkey minibuffer-local-map [(meta shift down)]
			      (lambda () (interactive)
				(org-eval-in-calendar '(calendar-forward-year 1))))
		  (org-defkey minibuffer-local-map [?\e (shift left)]
			      (lambda () (interactive)
				(org-eval-in-calendar '(calendar-backward-month 1))))
		  (org-defkey minibuffer-local-map [?\e (shift right)]
			      (lambda () (interactive)
				(org-eval-in-calendar '(calendar-forward-month 1))))
		  (org-defkey minibuffer-local-map [?\e (shift up)]
			      (lambda () (interactive)
				(org-eval-in-calendar '(calendar-backward-year 1))))
		  (org-defkey minibuffer-local-map [?\e (shift down)]
			      (lambda () (interactive)
				(org-eval-in-calendar '(calendar-forward-year 1))))
		  (org-defkey minibuffer-local-map [(shift up)]
			      (lambda () (interactive)
				(org-eval-in-calendar '(calendar-backward-week 1))))
		  (org-defkey minibuffer-local-map [(shift down)]
			      (lambda () (interactive)
				(org-eval-in-calendar '(calendar-forward-week 1))))
		  (org-defkey minibuffer-local-map [(shift left)]
			      (lambda () (interactive)
				(org-eval-in-calendar '(calendar-backward-day 1))))
		  (org-defkey minibuffer-local-map [(shift right)]
			      (lambda () (interactive)
				(org-eval-in-calendar '(calendar-forward-day 1))))
		  (org-defkey minibuffer-local-map ">"
			      (lambda () (interactive)
				(org-eval-in-calendar '(scroll-calendar-left 1))))
		  (org-defkey minibuffer-local-map "<"
			      (lambda () (interactive)
				(org-eval-in-calendar '(scroll-calendar-right 1))))
		  (org-defkey minibuffer-local-map "\C-v"
			      (lambda () (interactive)
				(org-eval-in-calendar
				 '(calendar-scroll-left-three-months 1))))
		  (org-defkey minibuffer-local-map "\M-v"
			      (lambda () (interactive)
				(org-eval-in-calendar
				 '(calendar-scroll-right-three-months 1))))
		  (run-hooks 'org-read-date-minibuffer-setup-hook)
		  (unwind-protect
		      (progn
			(use-local-map map)
			(add-hook 'post-command-hook 'org-read-date-display)
			(setq org-ans0 (read-string prompt default-input
						    'org-read-date-history nil))
			;; org-ans0: from prompt
			;; org-ans1: from mouse click
			;; org-ans2: from calendar motion
			(setq ans (concat org-ans0 " " (or org-ans1 org-ans2))))
		    (remove-hook 'post-command-hook 'org-read-date-display)
		    (use-local-map old-map)
		    (when org-read-date-overlay
		      (delete-overlay org-read-date-overlay)
                      (setq org-read-date-overlay nil)))))
	    (bury-buffer "*Calendar*")))))

     (t ; Naked prompt only
      (unwind-protect
	  (setq ans (read-string prompt default-input
				 'org-read-date-history timestr))
	(when org-read-date-overlay
	  (delete-overlay org-read-date-overlay)
	  (setq org-read-date-overlay nil)))))

    (setq final (org-read-date-analyze ans org-def org-defdecode))

    (when org-read-date-analyze-forced-year
      (message "Year was forced into %s"
	       (if org-read-date-force-compatible-dates
		   "compatible range (1970-2037)"
		 "range representable on this machine"))
      (ding))

    ;; One round trip to get rid of 34th of August and stuff like that....
    (setq final (decode-time (apply 'encode-time final)))

    (setq org-read-date-final-answer ans)

    (if to-time
	(apply 'encode-time final)
      (if (and (boundp 'org-time-was-given) org-time-was-given)
	  (format "%04d-%02d-%02d %02d:%02d"
		  (nth 5 final) (nth 4 final) (nth 3 final)
		  (nth 2 final) (nth 1 final))
	(format "%04d-%02d-%02d" (nth 5 final) (nth 4 final) (nth 3 final))))))

(defvar org-def)
(defvar org-defdecode)
(defvar org-with-time)
(defun org-read-date-display ()
  "Display the current date prompt interpretation in the minibuffer."
  (when org-read-date-display-live
    (when org-read-date-overlay
      (delete-overlay org-read-date-overlay))
    (when (minibufferp (current-buffer))
      (save-excursion
	(end-of-line 1)
	(while (not (equal (buffer-substring
			    (max (point-min) (- (point) 4)) (point))
			   "    "))
	  (insert " ")))
      (let* ((ans (concat (buffer-substring (point-at-bol) (point-max))
			  " " (or org-ans1 org-ans2)))
	     (org-end-time-was-given nil)
	     (f (org-read-date-analyze ans org-def org-defdecode))
	     (fmts (if org-dcst
		       org-time-stamp-custom-formats
		     org-time-stamp-formats))
	     (fmt (if (or org-with-time
			  (and (boundp 'org-time-was-given) org-time-was-given))
		      (cdr fmts)
		    (car fmts)))
	     (txt (concat "=> " (format-time-string fmt (apply 'encode-time f)))))
	(when (and org-end-time-was-given
		   (string-match org-plain-time-of-day-regexp txt))
	  (setq txt (concat (substring txt 0 (match-end 0)) "-"
			    org-end-time-was-given
			    (substring txt (match-end 0)))))
	(when org-read-date-analyze-futurep
	  (setq txt (concat txt " (=>F)")))
	(setq org-read-date-overlay
	      (make-overlay (1- (point-at-eol)) (point-at-eol)))
	(org-overlay-display org-read-date-overlay txt 'secondary-selection)))))

(defun org-read-date-analyze (ans org-def org-defdecode)
  "Analyze the combined answer of the date prompt."
  ;; FIXME: cleanup and comment
  (let ((nowdecode (decode-time (current-time)))
	delta deltan deltaw deltadef year month day
	hour minute second wday pm h2 m2 tl wday1
	iso-year iso-weekday iso-week iso-year iso-date futurep kill-year)
    (setq org-read-date-analyze-futurep nil
	  org-read-date-analyze-forced-year nil)
    (when (string-match "\\`[ \t]*\\.[ \t]*\\'" ans)
      (setq ans "+0"))

    (when (setq delta (org-read-date-get-relative ans (current-time) org-def))
      (setq ans (replace-match "" t t ans)
	    deltan (car delta)
	    deltaw (nth 1 delta)
            deltadef (nth 2 delta)))

    ;; Check if there is an iso week date in there
    ;; If yes, store the info and postpone interpreting it until the rest
    ;; of the parsing is done
    (when (string-match "\\<\\(?:\\([0-9]+\\)-\\)?[wW]\\([0-9]\\{1,2\\}\\)\\(?:-\\([0-6]\\)\\)?\\([ \t]\\|$\\)" ans)
      (setq iso-year (if (match-end 1)
			 (org-small-year-to-year
			  (string-to-number (match-string 1 ans))))
	    iso-weekday (if (match-end 3)
			    (string-to-number (match-string 3 ans)))
	    iso-week (string-to-number (match-string 2 ans)))
      (setq ans (replace-match "" t t ans)))

    ;; Help matching ISO dates with single digit month or day, like 2006-8-11.
    (when (string-match
	   "^ *\\(\\([0-9]+\\)-\\)?\\([0-1]?[0-9]\\)-\\([0-3]?[0-9]\\)\\([^-0-9]\\|$\\)" ans)
      (setq year (if (match-end 2)
		     (string-to-number (match-string 2 ans))
		   (progn (setq kill-year t)
			  (string-to-number (format-time-string "%Y"))))
	    month (string-to-number (match-string 3 ans))
	    day (string-to-number (match-string 4 ans)))
      (if (< year 100) (setq year (+ 2000 year)))
      (setq ans (replace-match (format "%04d-%02d-%02d\\5" year month day)
			       t nil ans)))

    ;; Help matching dotted european dates
    (when (string-match
	   "^ *\\(3[01]\\|0?[1-9]\\|[12][0-9]\\)\\. ?\\(0?[1-9]\\|1[012]\\)\\. ?\\([1-9][0-9][0-9][0-9]\\)?" ans)
      (setq year (if (match-end 3)
		     (string-to-number (match-string 3 ans))
		   (progn (setq kill-year t)
			  (string-to-number (format-time-string "%Y"))))
	    day (string-to-number (match-string 1 ans))
	    month (string-to-number (match-string 2 ans))
	    ans (replace-match (format "%04d-%02d-%02d\\5" year month day)
				     t nil ans)))

    ;; Help matching american dates, like 5/30 or 5/30/7
    (when (string-match
	   "^ *\\(0?[1-9]\\|1[012]\\)/\\(0?[1-9]\\|[12][0-9]\\|3[01]\\)\\(/\\([0-9]+\\)\\)?\\([^/0-9]\\|$\\)" ans)
      (setq year (if (match-end 4)
		     (string-to-number (match-string 4 ans))
		   (progn (setq kill-year t)
			  (string-to-number (format-time-string "%Y"))))
	    month (string-to-number (match-string 1 ans))
	    day (string-to-number (match-string 2 ans)))
      (if (< year 100) (setq year (+ 2000 year)))
      (setq ans (replace-match (format "%04d-%02d-%02d\\5" year month day)
			       t nil ans)))
    ;; Help matching am/pm times, because `parse-time-string' does not do that.
    ;; If there is a time with am/pm, and *no* time without it, we convert
    ;; so that matching will be successful.
    (loop for i from 1 to 2 do ; twice, for end time as well
	  (when (and (not (string-match "\\(\\`\\|[^+]\\)[012]?[0-9]:[0-9][0-9]\\([ \t\n]\\|$\\)" ans))
		     (string-match "\\([012]?[0-9]\\)\\(:\\([0-5][0-9]\\)\\)?\\(am\\|AM\\|pm\\|PM\\)\\>" ans))
	    (setq hour (string-to-number (match-string 1 ans))
		  minute (if (match-end 3)
			     (string-to-number (match-string 3 ans))
			   0)
		  pm (equal ?p
			    (string-to-char (downcase (match-string 4 ans)))))
	    (if (and (= hour 12) (not pm))
		(setq hour 0)
	      (if (and pm (< hour 12)) (setq hour (+ 12 hour))))
	    (setq ans (replace-match (format "%02d:%02d" hour minute)
				     t t ans))))

    ;; Check if a time range is given as a duration
    (when (string-match "\\([012]?[0-9]\\):\\([0-6][0-9]\\)\\+\\([012]?[0-9]\\)\\(:\\([0-5][0-9]\\)\\)?" ans)
      (setq hour (string-to-number (match-string 1 ans))
	    h2 (+ hour (string-to-number (match-string 3 ans)))
	    minute (string-to-number (match-string 2 ans))
	    m2 (+ minute (if (match-end 5) (string-to-number
					    (match-string 5 ans))0)))
      (if (>= m2 60) (setq h2 (1+ h2) m2 (- m2 60)))
      (setq ans (replace-match (format "%02d:%02d-%02d:%02d" hour minute h2 m2)
			       t t ans)))

    ;; Check if there is a time range
    (when (boundp 'org-end-time-was-given)
      (setq org-time-was-given nil)
      (when (and (string-match org-plain-time-of-day-regexp ans)
		 (match-end 8))
	(setq org-end-time-was-given (match-string 8 ans))
	(setq ans (concat (substring ans 0 (match-beginning 7))
			  (substring ans (match-end 7))))))

    (setq tl (parse-time-string ans)
	  day (or (nth 3 tl) (nth 3 org-defdecode))
	  month (or (nth 4 tl)
		    (if (and org-read-date-prefer-future
			     (nth 3 tl) (< (nth 3 tl) (nth 3 nowdecode)))
			(prog1 (1+ (nth 4 nowdecode)) (setq futurep t))
		      (nth 4 org-defdecode)))
	  year (or (and (not kill-year) (nth 5 tl))
		   (if (and org-read-date-prefer-future
			    (nth 4 tl) (< (nth 4 tl) (nth 4 nowdecode)))
		       (prog1 (1+ (nth 5 nowdecode)) (setq futurep t))
		     (nth 5 org-defdecode)))
	  hour (or (nth 2 tl) (nth 2 org-defdecode))
	  minute (or (nth 1 tl) (nth 1 org-defdecode))
	  second (or (nth 0 tl) 0)
	  wday (nth 6 tl))

    (when (and (eq org-read-date-prefer-future 'time)
	       (not (nth 3 tl)) (not (nth 4 tl)) (not (nth 5 tl))
	       (equal day (nth 3 nowdecode))
	       (equal month (nth 4 nowdecode))
	       (equal year (nth 5 nowdecode))
	       (nth 2 tl)
	       (or (< (nth 2 tl) (nth 2 nowdecode))
		   (and (= (nth 2 tl) (nth 2 nowdecode))
			(nth 1 tl)
			(< (nth 1 tl) (nth 1 nowdecode)))))
      (setq day (1+ day)
	    futurep t))

    ;; Special date definitions below
    (cond
     (iso-week
      ;; There was an iso week
      (require 'cal-iso)
      (setq futurep nil)
      (setq year (or iso-year year)
	    day (or iso-weekday wday 1)
	    wday nil ; to make sure that the trigger below does not match
	    iso-date (calendar-gregorian-from-absolute
		      (calendar-absolute-from-iso
		       (list iso-week day year))))
; FIXME:  Should we also push ISO weeks into the future?
;      (when (and org-read-date-prefer-future
;		 (not iso-year)
;		 (< (calendar-absolute-from-gregorian iso-date)
;		    (time-to-days (current-time))))
;	(setq year (1+ year)
;	      iso-date (calendar-gregorian-from-absolute
;			(calendar-absolute-from-iso
;			 (list iso-week day year)))))
      (setq month (car iso-date)
	    year (nth 2 iso-date)
	    day (nth 1 iso-date)))
     (deltan
      (setq futurep nil)
      (unless deltadef
	(let ((now (decode-time (current-time))))
	  (setq day (nth 3 now) month (nth 4 now) year (nth 5 now))))
      (cond ((member deltaw '("d" "")) (setq day (+ day deltan)))
	    ((equal deltaw "w") (setq day (+ day (* 7 deltan))))
	    ((equal deltaw "m") (setq month (+ month deltan)))
	    ((equal deltaw "y") (setq year (+ year deltan)))))
     ((and wday (not (nth 3 tl)))
      (setq futurep nil)
      ;; Weekday was given, but no day, so pick that day in the week
      ;; on or after the derived date.
      (setq wday1 (nth 6 (decode-time (encode-time 0 0 0 day month year))))
      (unless (equal wday wday1)
	(setq day (+ day (% (- wday wday1 -7) 7))))))
    (if (and (boundp 'org-time-was-given)
	     (nth 2 tl))
	(setq org-time-was-given t))
    (if (< year 100) (setq year (+ 2000 year)))
    ;; Check of the date is representable
    (if org-read-date-force-compatible-dates
	(progn
	  (if (< year 1970)
	      (setq year 1970 org-read-date-analyze-forced-year t))
	  (if (> year 2037)
	      (setq year 2037  org-read-date-analyze-forced-year t)))
      (condition-case nil
	  (ignore (encode-time second minute hour day month year))
	(error
	 (setq year (nth 5 org-defdecode))
	 (setq org-read-date-analyze-forced-year t))))
    (setq org-read-date-analyze-futurep futurep)
    (list second minute hour day month year)))

(defvar parse-time-weekdays)
(defun org-read-date-get-relative (s today default)
  "Check string S for special relative date string.
TODAY and DEFAULT are internal times, for today and for a default.
Return shift list (N what def-flag)
WHAT       is \"d\", \"w\", \"m\", or \"y\" for day, week, month, year.
N          is the number of WHATs to shift.
DEF-FLAG   is t when a double ++ or -- indicates shift relative to
           the DEFAULT date rather than TODAY."
  (require 'parse-time)
  (when (and
	 (string-match
	  (concat
	   "\\`[ \t]*\\([-+]\\{0,2\\}\\)"
	   "\\([0-9]+\\)?"
	   "\\([dwmy]\\|\\(" (mapconcat 'car parse-time-weekdays "\\|") "\\)\\)?"
	   "\\([ \t]\\|$\\)") s)
	 (or (> (match-end 1) (match-beginning 1)) (match-end 4)))
    (let* ((dir (if (> (match-end 1) (match-beginning 1))
		    (string-to-char (substring (match-string 1 s) -1))
		  ?+))
	   (rel (and (match-end 1) (= 2 (- (match-end 1) (match-beginning 1)))))
	   (n (if (match-end 2) (string-to-number (match-string 2 s)) 1))
	   (what (if (match-end 3) (match-string 3 s) "d"))
	   (wday1 (cdr (assoc (downcase what) parse-time-weekdays)))
	   (date (if rel default today))
	   (wday (nth 6 (decode-time date)))
	   delta)
      (if wday1
	  (progn
	    (setq delta (mod (+ 7 (- wday1 wday)) 7))
	    (if (= dir ?-) (setq delta (- delta 7)))
	    (if (> n 1) (setq delta (+ delta (* (1- n) (if (= dir ?-) -7 7)))))
	    (list delta "d" rel))
	(list (* n (if (= dir ?-) -1 1)) what rel)))))

(defun org-order-calendar-date-args (arg1 arg2 arg3)
  "Turn a user-specified date into the internal representation.
The internal representation needed by the calendar is (month day year).
This is a wrapper to handle the brain-dead convention in calendar that
user function argument order change dependent on argument order."
  (if (boundp 'calendar-date-style)
      (cond
       ((eq calendar-date-style 'american)
	(list arg1 arg2 arg3))
       ((eq calendar-date-style 'european)
	(list arg2 arg1 arg3))
       ((eq calendar-date-style 'iso)
	(list arg2 arg3 arg1)))
    (with-no-warnings ;; european-calendar-style is obsolete as of version 23.1
      (if (org-bound-and-true-p european-calendar-style)
	  (list arg2 arg1 arg3)
	(list arg1 arg2 arg3)))))

(defun org-eval-in-calendar (form &optional keepdate)
  "Eval FORM in the calendar window and return to current window.
Also, store the cursor date in variable org-ans2."
  (let ((sf (selected-frame))
	(sw (selected-window)))
    (select-window (get-buffer-window "*Calendar*" t))
    (eval form)
    (when (and (not keepdate) (calendar-cursor-to-date))
      (let* ((date (calendar-cursor-to-date))
	     (time (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date))))
	(setq org-ans2 (format-time-string "%Y-%m-%d" time))))
    (move-overlay org-date-ovl (1- (point)) (1+ (point)) (current-buffer))
    (select-window sw)
    (org-select-frame-set-input-focus sf)))

(defun org-calendar-select ()
  "Return to `org-read-date' with the date currently selected.
This is used by `org-read-date' in a temporary keymap for the calendar buffer."
  (interactive)
  (when (calendar-cursor-to-date)
    (let* ((date (calendar-cursor-to-date))
	   (time (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date))))
      (setq org-ans1 (format-time-string "%Y-%m-%d" time)))
    (if (active-minibuffer-window) (exit-minibuffer))))

(defun org-insert-time-stamp (time &optional with-hm inactive pre post extra)
  "Insert a date stamp for the date given by the internal TIME.
WITH-HM means use the stamp format that includes the time of the day.
INACTIVE means use square brackets instead of angular ones, so that the
stamp will not contribute to the agenda.
PRE and POST are optional strings to be inserted before and after the
stamp.
The command returns the inserted time stamp."
  (let ((fmt (funcall (if with-hm 'cdr 'car) org-time-stamp-formats))
	stamp)
    (if inactive (setq fmt (concat "[" (substring fmt 1 -1) "]")))
    (insert-before-markers (or pre ""))
    (when (listp extra)
      (setq extra (car extra))
      (if (and (stringp extra)
	       (string-match "\\([0-9]+\\):\\([0-9]+\\)" extra))
	  (setq extra (format "-%02d:%02d"
			      (string-to-number (match-string 1 extra))
			      (string-to-number (match-string 2 extra))))
	(setq extra nil)))
    (when extra
      (setq fmt (concat (substring fmt 0 -1) extra (substring fmt -1))))
    (insert-before-markers (setq stamp (format-time-string fmt time)))
    (insert-before-markers (or post ""))
    (setq org-last-inserted-timestamp stamp)))

(defun org-toggle-time-stamp-overlays ()
  "Toggle the use of custom time stamp formats."
  (interactive)
  (setq org-display-custom-times (not org-display-custom-times))
  (unless org-display-custom-times
    (let ((p (point-min)) (bmp (buffer-modified-p)))
      (while (setq p (next-single-property-change p 'display))
	(if (and (get-text-property p 'display)
		 (eq (get-text-property p 'face) 'org-date))
	    (remove-text-properties
	     p (setq p (next-single-property-change p 'display))
	     '(display t))))
      (set-buffer-modified-p bmp)))
  (if (featurep 'xemacs)
      (remove-text-properties (point-min) (point-max) '(end-glyph t)))
  (org-restart-font-lock)
  (setq org-table-may-need-update t)
  (if org-display-custom-times
      (message "Time stamps are overlaid with custom format")
    (message "Time stamp overlays removed")))

(defun org-display-custom-time (beg end)
  "Overlay modified time stamp format over timestamp between BEG and END."
  (let* ((ts (buffer-substring beg end))
	 t1 w1 with-hm tf time str w2 (off 0))
    (save-match-data
      (setq t1 (org-parse-time-string ts t))
      (if (string-match "\\(-[0-9]+:[0-9]+\\)?\\( [.+]?\\+[0-9]+[dwmy]\\(/[0-9]+[dwmy]\\)?\\)?\\'" ts)
	  (setq off (- (match-end 0) (match-beginning 0)))))
    (setq end (- end off))
    (setq w1 (- end beg)
	  with-hm (and (nth 1 t1) (nth 2 t1))
	  tf (funcall (if with-hm 'cdr 'car) org-time-stamp-custom-formats)
	  time (org-fix-decoded-time t1)
	  str (org-add-props
		  (format-time-string
		   (substring tf 1 -1) (apply 'encode-time time))
		  nil 'mouse-face 'highlight)
	  w2 (length str))
    (if (not (= w2 w1))
	(add-text-properties (1+ beg) (+ 2 beg)
			     (list 'org-dwidth t 'org-dwidth-n (- w1 w2))))
    (if (featurep 'xemacs)
	(progn
	  (put-text-property beg end 'invisible t)
	  (put-text-property beg end 'end-glyph (make-glyph str)))
      (put-text-property beg end 'display str))))

(defun org-translate-time (string)
  "Translate all timestamps in STRING to custom format.
But do this only if the variable `org-display-custom-times' is set."
  (when org-display-custom-times
    (save-match-data
      (let* ((start 0)
	     (re org-ts-regexp-both)
	     t1 with-hm inactive tf time str beg end)
	(while (setq start (string-match re string start))
	  (setq beg (match-beginning 0)
		end (match-end 0)
		t1 (save-match-data
		     (org-parse-time-string (substring string beg end) t))
		with-hm (and (nth 1 t1) (nth 2 t1))
		inactive (equal (substring string beg (1+ beg)) "[")
		tf (funcall (if with-hm 'cdr 'car)
			    org-time-stamp-custom-formats)
		time (org-fix-decoded-time t1)
		str (format-time-string
		     (concat
		      (if inactive "[" "<") (substring tf 1 -1)
		      (if inactive "]" ">"))
		     (apply 'encode-time time))
		string (replace-match str t t string)
		start (+ start (length str)))))))
  string)

(defun org-fix-decoded-time (time)
  "Set 0 instead of nil for the first 6 elements of time.
Don't touch the rest."
  (let ((n 0))
    (mapcar (lambda (x) (if (< (setq n (1+ n)) 7) (or x 0) x)) time)))

(defun org-days-to-time (timestamp-string)
  "Difference between TIMESTAMP-STRING and now in days."
  (- (time-to-days (org-time-string-to-time timestamp-string))
     (time-to-days (current-time))))

(defun org-deadline-close (timestamp-string &optional ndays)
  "Is the time in TIMESTAMP-STRING close to the current date?"
  (setq ndays (or ndays (org-get-wdays timestamp-string)))
  (and (< (org-days-to-time timestamp-string) ndays)
       (not (org-entry-is-done-p))))

(defun org-get-wdays (ts)
  "Get the deadline lead time appropriate for timestring TS."
  (cond
   ((<= org-deadline-warning-days 0)
    ;; 0 or negative, enforce this value no matter what
    (- org-deadline-warning-days))
   ((string-match "-\\([0-9]+\\)\\([dwmy]\\)\\(\\'\\|>\\| \\)" ts)
    ;; lead time is specified.
    (floor (* (string-to-number (match-string 1 ts))
	      (cdr (assoc (match-string 2 ts)
			  '(("d" . 1)    ("w" . 7)
			    ("m" . 30.4) ("y" . 365.25)))))))
   ;; go for the default.
   (t org-deadline-warning-days)))

(defun org-calendar-select-mouse (ev)
  "Return to `org-read-date' with the date currently selected.
This is used by `org-read-date' in a temporary keymap for the calendar buffer."
  (interactive "e")
  (mouse-set-point ev)
  (when (calendar-cursor-to-date)
    (let* ((date (calendar-cursor-to-date))
	   (time (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date))))
      (setq org-ans1 (format-time-string "%Y-%m-%d" time)))
    (if (active-minibuffer-window) (exit-minibuffer))))

(defun org-check-deadlines (ndays)
  "Check if there are any deadlines due or past due.
A deadline is considered due if it happens within `org-deadline-warning-days'
days from today's date.  If the deadline appears in an entry marked DONE,
it is not shown.  The prefix arg NDAYS can be used to test that many
days.  If the prefix is a raw \\[universal-argument] prefix, all deadlines are shown."
  (interactive "P")
  (let* ((org-warn-days
	  (cond
	   ((equal ndays '(4)) 100000)
	   (ndays (prefix-numeric-value ndays))
	   (t (abs org-deadline-warning-days))))
	 (case-fold-search nil)
	 (regexp (concat "\\<" org-deadline-string " *<\\([^>]+\\)>"))
	 (callback
	  (lambda () (org-deadline-close (match-string 1) org-warn-days))))

    (message "%d deadlines past-due or due within %d days"
	     (org-occur regexp nil callback)
	     org-warn-days)))

(defun org-check-before-date (date)
  "Check if there are deadlines or scheduled entries before DATE."
  (interactive (list (org-read-date)))
  (let ((case-fold-search nil)
	(regexp (concat "\\<\\(" org-deadline-string
			"\\|" org-scheduled-string
			"\\) *<\\([^>]+\\)>"))
	(callback
	 (lambda () (time-less-p
		     (org-time-string-to-time (match-string 2))
		     (org-time-string-to-time date)))))
    (message "%d entries before %s"
	     (org-occur regexp nil callback) date)))

(defun org-check-after-date (date)
  "Check if there are deadlines or scheduled entries after DATE."
  (interactive (list (org-read-date)))
  (let ((case-fold-search nil)
	(regexp (concat "\\<\\(" org-deadline-string
			"\\|" org-scheduled-string
			"\\) *<\\([^>]+\\)>"))
	(callback
	 (lambda () (not
		     (time-less-p
		      (org-time-string-to-time (match-string 2))
		      (org-time-string-to-time date))))))
    (message "%d entries after %s"
	     (org-occur regexp nil callback) date)))

(defun org-check-dates-range (start-date end-date)
  "Check for deadlines/scheduled entries between START-DATE and END-DATE."
  (interactive (list (org-read-date nil nil nil "Range starts")
		     (org-read-date nil nil nil "Range end")))
  (let ((case-fold-search nil)
	(regexp (concat "\\<\\(" org-deadline-string
			"\\|" org-scheduled-string
			"\\) *<\\([^>]+\\)>"))
	(callback
	 (lambda ()
	   (let ((match (match-string 2)))
	     (and
	      (not (time-less-p
		    (org-time-string-to-time match)
		    (org-time-string-to-time start-date)))
	      (time-less-p
	       (org-time-string-to-time match)
	       (org-time-string-to-time end-date)))))))
    (message "%d entries between %s and %s"
	     (org-occur regexp nil callback) start-date end-date)))

(defun org-evaluate-time-range (&optional to-buffer)
  "Evaluate a time range by computing the difference between start and end.
Normally the result is just printed in the echo area, but with prefix arg
TO-BUFFER, the result is inserted just after the date stamp into the buffer.
If the time range is actually in a table, the result is inserted into the
next column.
For time difference computation, a year is assumed to be exactly 365
days in order to avoid rounding problems."
  (interactive "P")
  (or
   (org-clock-update-time-maybe)
   (save-excursion
     (unless (org-at-date-range-p t)
       (goto-char (point-at-bol))
       (re-search-forward org-tr-regexp-both (point-at-eol) t))
     (if (not (org-at-date-range-p t))
	 (error "Not at a time-stamp range, and none found in current line")))
   (let* ((ts1 (match-string 1))
	  (ts2 (match-string 2))
	  (havetime (or (> (length ts1) 15) (> (length ts2) 15)))
	  (match-end (match-end 0))
	  (time1 (org-time-string-to-time ts1))
	  (time2 (org-time-string-to-time ts2))
	  (t1 (org-float-time time1))
	  (t2 (org-float-time time2))
	  (diff (abs (- t2 t1)))
	  (negative (< (- t2 t1) 0))
	  ;; (ys (floor (* 365 24 60 60)))
	  (ds (* 24 60 60))
	  (hs (* 60 60))
	  (fy "%dy %dd %02d:%02d")
	  (fy1 "%dy %dd")
	  (fd "%dd %02d:%02d")
	  (fd1 "%dd")
	  (fh "%02d:%02d")
	  y d h m align)
     (if havetime
	 (setq ; y (floor (/ diff ys))  diff (mod diff ys)
	  y 0
	  d (floor (/ diff ds))  diff (mod diff ds)
	  h (floor (/ diff hs))  diff (mod diff hs)
	  m (floor (/ diff 60)))
       (setq ; y (floor (/ diff ys))  diff (mod diff ys)
	y 0
	d (floor (+ (/ diff ds) 0.5))
	h 0 m 0))
     (if (not to-buffer)
	 (message "%s" (org-make-tdiff-string y d h m))
       (if (org-at-table-p)
	   (progn
	     (goto-char match-end)
	     (setq align t)
	     (and (looking-at " *|") (goto-char (match-end 0))))
	 (goto-char match-end))
       (if (looking-at
	    "\\( *-? *[0-9]+y\\)?\\( *[0-9]+d\\)? *[0-9][0-9]:[0-9][0-9]")
	   (replace-match ""))
       (if negative (insert " -"))
       (if (> y 0) (insert " " (format (if havetime fy fy1) y d h m))
	 (if (> d 0) (insert " " (format (if havetime fd fd1) d h m))
	   (insert " " (format fh h m))))
       (if align (org-table-align))
       (message "Time difference inserted")))))

(defun org-make-tdiff-string (y d h m)
  (let ((fmt "")
	(l nil))
    (if (> y 0) (setq fmt (concat fmt "%d year" (if (> y 1) "s" "") " ")
		      l (push y l)))
    (if (> d 0) (setq fmt (concat fmt "%d day"  (if (> d 1) "s" "") " ")
		      l (push d l)))
    (if (> h 0) (setq fmt (concat fmt "%d hour" (if (> h 1) "s" "") " ")
		      l (push h l)))
    (if (> m 0) (setq fmt (concat fmt "%d minute" (if (> m 1) "s" "") " ")
		      l (push m l)))
    (apply 'format fmt (nreverse l))))

(defun org-time-string-to-time (s &optional buffer pos)
  (condition-case errdata
      (apply 'encode-time (org-parse-time-string s))
    (error (error "Bad timestamp `%s'%s\nError was: %s"
		  s (if (not (and buffer pos))
			""
		      (format " at %d in buffer `%s'" pos buffer))
		  (cdr errdata)))))

(defun org-time-string-to-seconds (s)
  (org-float-time (org-time-string-to-time s)))

(defun org-time-string-to-absolute (s &optional daynr prefer show-all buffer pos)
  "Convert a time stamp to an absolute day number.
If there is a specifier for a cyclic time stamp, get the closest date to
DAYNR.
PREFER and SHOW-ALL are passed through to `org-closest-date'.
The variable date is bound by the calendar when this is called."
  (cond
   ((and daynr (string-match "\\`%%\\((.*)\\)" s))
    (if (org-diary-sexp-entry (match-string 1 s) "" date)
	daynr
      (+ daynr 1000)))
   ((and daynr (string-match "\\+[0-9]+[dwmy]" s))
    (org-closest-date s (if (and (boundp 'daynr) (integerp daynr)) daynr
			  (time-to-days (current-time))) (match-string 0 s)
			  prefer show-all))
   (t (time-to-days
       (condition-case errdata
	   (apply 'encode-time (org-parse-time-string s))
	 (error (error "Bad timestamp `%s'%s\nError was: %s"
		       s (if (not (and buffer pos))
			     ""
			   (format " at %d in buffer `%s'" pos buffer))
		       (cdr errdata))))))))

(defun org-days-to-iso-week (days)
  "Return the iso week number."
  (require 'cal-iso)
  (car (calendar-iso-from-absolute days)))

(defun org-small-year-to-year (year)
  "Convert 2-digit years into 4-digit years.
38-99 are mapped into 1938-1999.  1-37 are mapped into 2001-2007.
The year 2000 cannot be abbreviated.  Any year larger than 99
is returned unchanged."
  (if (< year 38)
      (setq year (+ 2000 year))
    (if (< year 100)
	(setq year (+ 1900 year))))
  year)

(defun org-time-from-absolute (d)
  "Return the time corresponding to date D.
D may be an absolute day number, or a calendar-type list (month day year)."
  (if (numberp d) (setq d (calendar-gregorian-from-absolute d)))
  (encode-time 0 0 0 (nth 1 d) (car d) (nth 2 d)))

(defun org-calendar-holiday ()
  "List of holidays, for Diary display in Org-mode."
  (require 'holidays)
  (let ((hl (funcall
	     (if (fboundp 'calendar-check-holidays)
		 'calendar-check-holidays 'check-calendar-holidays) date)))
    (if hl (mapconcat 'identity hl "; "))))

(defun org-diary-sexp-entry (sexp entry date)
  "Process a SEXP diary ENTRY for DATE."
  (require 'diary-lib)
  (let ((result (if calendar-debug-sexp
                    (let ((stack-trace-on-error t))
                      (eval (car (read-from-string sexp))))
                  (condition-case nil
                      (eval (car (read-from-string sexp)))
                    (error
                     (beep)
                     (message "Bad sexp at line %d in %s: %s"
			      (org-current-line)
			      (buffer-file-name) sexp)
                     (sleep-for 2))))))
    (cond ((stringp result) (split-string result "; "))
	  ((and (consp result)
		(not (consp (cdr result)))
		(stringp (cdr result))) (cdr result))
	  ((and (consp result)
		(stringp (car result))) result)
	  (result entry)
          (t nil))))

(defun org-diary-to-ical-string (frombuf)
  "Get iCalendar entries from diary entries in buffer FROMBUF.
This uses the icalendar.el library."
  (let* ((tmpdir (if (featurep 'xemacs)
		     (temp-directory)
		   temporary-file-directory))
	 (tmpfile (make-temp-name
		   (expand-file-name "orgics" tmpdir)))
	 buf rtn b e)
    (with-current-buffer frombuf
      (icalendar-export-region (point-min) (point-max) tmpfile)
      (setq buf (find-buffer-visiting tmpfile))
      (set-buffer buf)
      (goto-char (point-min))
      (if (re-search-forward "^BEGIN:VEVENT" nil t)
	  (setq b (match-beginning 0)))
      (goto-char (point-max))
      (if (re-search-backward "^END:VEVENT" nil t)
	  (setq e (match-end 0)))
      (setq rtn (if (and b e) (concat (buffer-substring b e) "\n") "")))
    (kill-buffer buf)
    (delete-file tmpfile)
    rtn))

(defun org-closest-date (start current change prefer show-all)
  "Find the date closest to CURRENT that is consistent with START and CHANGE.
When PREFER is `past', return a date that is either CURRENT or past.
When PREFER is `future', return a date that is either CURRENT or future.
When SHOW-ALL is nil, only return the current occurrence of a time stamp."
  ;; Make the proper lists from the dates
  (catch 'exit
    (let ((a1 '(("d" . day) ("w" . week) ("m" . month) ("y" . year)))
	  dn dw sday cday n1 n2 n0
	  d m y y1 y2 date1 date2 nmonths nm ny m2)

      (setq start (org-date-to-gregorian start)
	    current (org-date-to-gregorian
		     (if show-all
			 current
		       (time-to-days (current-time))))
	    sday (calendar-absolute-from-gregorian start)
	    cday  (calendar-absolute-from-gregorian current))

      (if (<= cday sday) (throw 'exit sday))

      (if (string-match "\\(\\+[0-9]+\\)\\([dwmy]\\)" change)
	  (setq dn (string-to-number (match-string 1 change))
		dw (cdr (assoc (match-string 2 change) a1)))
	(error "Invalid change specifier: %s" change))
      (if (eq dw 'week) (setq dw 'day dn (* 7 dn)))
      (cond
       ((eq dw 'day)
	(setq n1 (+ sday (* dn (floor (/ (- cday sday) dn))))
	      n2 (+ n1 dn)))
       ((eq dw 'year)
	(setq d (nth 1 start) m (car start) y1 (nth 2 start) y2 (nth 2 current))
	(setq y1 (+ (* (floor (/ (- y2 y1) dn)) dn) y1))
	(setq date1 (list m d y1)
	      n1 (calendar-absolute-from-gregorian date1)
	      date2 (list m d (+ y1 (* (if (< n1 cday) 1 -1) dn)))
	      n2 (calendar-absolute-from-gregorian date2)))
       ((eq dw 'month)
	;; approx number of month between the two dates
	(setq nmonths (floor (/ (- cday sday) 30.436875)))
	;; How often does dn fit in there?
	(setq d (nth 1 start) m (car start) y (nth 2 start)
	      nm (* dn (max 0 (1- (floor (/ nmonths dn)))))
	      m (+ m nm)
	      ny (floor (/ m 12))
	      y (+ y ny)
	      m (- m (* ny 12)))
	(while (> m 12) (setq m (- m 12) y (1+ y)))
	(setq n1 (calendar-absolute-from-gregorian (list m d y)))
	(setq m2 (+ m dn) y2 y)
	(if (> m2 12) (setq y2 (1+ y2) m2 (- m2 12)))
	(setq n2 (calendar-absolute-from-gregorian (list m2 d y2)))
	(while (<= n2 cday)
	  (setq n1 n2 m m2 y y2)
	  (setq m2 (+ m dn) y2 y)
	  (if (> m2 12) (setq y2 (1+ y2) m2 (- m2 12)))
	  (setq n2 (calendar-absolute-from-gregorian (list m2 d y2))))))
      ;; Make sure n1 is the earlier date
      (setq n0 n1  n1 (min n1 n2)  n2 (max n0 n2))
      (if show-all
	  (cond
	   ((eq prefer 'past)   (if (= cday n2) n2 n1))
	   ((eq prefer 'future) (if (= cday n1) n1 n2))
	   (t (if (> (abs (- cday n1)) (abs (- cday n2))) n2 n1)))
	(cond
	 ((eq prefer 'past)   (if (= cday n2) n2 n1))
	 ((eq prefer 'future) (if (= cday n1) n1 n2))
	 (t (if (= cday n1) n1 n2)))))))

(defun org-date-to-gregorian (date)
  "Turn any specification of DATE into a Gregorian date for the calendar."
  (cond ((integerp date) (calendar-gregorian-from-absolute date))
	((and (listp date) (= (length date) 3)) date)
	((stringp date)
	 (setq date (org-parse-time-string date))
	 (list (nth 4 date) (nth 3 date) (nth 5 date)))
	((listp date)
	 (list (nth 4 date) (nth 3 date) (nth 5 date)))))

(defun org-parse-time-string (s &optional nodefault)
  "Parse the standard Org-mode time string.
This should be a lot faster than the normal `parse-time-string'.
If time is not given, defaults to 0:00.  However, with optional NODEFAULT,
hour and minute fields will be nil if not given."
  (if (string-match org-ts-regexp0 s)
      (list 0
	    (if (or (match-beginning 8) (not nodefault))
		(string-to-number (or (match-string 8 s) "0")))
	    (if (or (match-beginning 7) (not nodefault))
		(string-to-number (or (match-string 7 s) "0")))
	    (string-to-number (match-string 4 s))
	    (string-to-number (match-string 3 s))
	    (string-to-number (match-string 2 s))
	    nil nil nil)
    (error "Not a standard Org-mode time string: %s" s)))

(defun org-timestamp-up (&optional arg)
  "Increase the date item at the cursor by one.
If the cursor is on the year, change the year.  If it is on the month,
the day or the time, change that.
With prefix ARG, change by that many units."
  (interactive "p")
  (org-timestamp-change (prefix-numeric-value arg) nil 'updown))

(defun org-timestamp-down (&optional arg)
  "Decrease the date item at the cursor by one.
If the cursor is on the year, change the year.  If it is on the month,
the day or the time, change that.
With prefix ARG, change by that many units."
  (interactive "p")
  (org-timestamp-change (- (prefix-numeric-value arg)) nil 'updown))

(defun org-timestamp-up-day (&optional arg)
  "Increase the date in the time stamp by one day.
With prefix ARG, change that many days."
  (interactive "p")
  (if (and (not (org-at-timestamp-p t))
	   (org-at-heading-p))
      (org-todo 'up)
    (org-timestamp-change (prefix-numeric-value arg) 'day 'updown)))

(defun org-timestamp-down-day (&optional arg)
  "Decrease the date in the time stamp by one day.
With prefix ARG, change that many days."
  (interactive "p")
  (if (and (not (org-at-timestamp-p t))
	   (org-at-heading-p))
      (org-todo 'down)
    (org-timestamp-change (- (prefix-numeric-value arg)) 'day) 'updown))

(defun org-at-timestamp-p (&optional inactive-ok)
  "Determine if the cursor is in or at a timestamp."
  (interactive)
  (let* ((tsr (if inactive-ok org-ts-regexp3 org-ts-regexp2))
	 (pos (point))
	 (ans (or (looking-at tsr)
		  (save-excursion
		    (skip-chars-backward "^[<\n\r\t")
		    (if (> (point) (point-min)) (backward-char 1))
		    (and (looking-at tsr)
			 (> (- (match-end 0) pos) -1))))))
    (and ans
	 (boundp 'org-ts-what)
	 (setq org-ts-what
	      (cond
	       ((= pos (match-beginning 0))         'bracket)
	       ;; Point is considered to be "on the bracket" whether
	       ;; it's really on it or right after it.
	       ((or (= pos (1- (match-end 0)))
                    (= pos (match-end 0)))          'bracket)
	       ((org-pos-in-match-range pos 2)      'year)
	       ((org-pos-in-match-range pos 3)      'month)
	       ((org-pos-in-match-range pos 7)      'hour)
	       ((org-pos-in-match-range pos 8)      'minute)
	       ((or (org-pos-in-match-range pos 4)
		    (org-pos-in-match-range pos 5)) 'day)
	       ((and (> pos (or (match-end 8) (match-end 5)))
		     (< pos (match-end 0)))
		(- pos (or (match-end 8) (match-end 5))))
	       (t 'day))))
    ans))

(defun org-toggle-timestamp-type ()
  "Toggle the type (<active> or [inactive]) of a time stamp."
  (interactive)
  (when (org-at-timestamp-p t)
    (let ((beg (match-beginning 0)) (end (match-end 0))
	  (map '((?\[ . "<") (?\] . ">") (?< . "[") (?> . "]"))))
      (save-excursion
	(goto-char beg)
	(while (re-search-forward "[][<>]" end t)
	  (replace-match (cdr (assoc (char-after (match-beginning 0)) map))
			 t t)))
      (message "Timestamp is now %sactive"
	       (if (equal (char-after beg) ?<) "" "in")))))

(defun org-timestamp-change (n &optional what updown)
  "Change the date in the time stamp at point.
The date will be changed by N times WHAT.  WHAT can be `day', `month',
`year', `minute', `second'.  If WHAT is not given, the cursor position
in the timestamp determines what will be changed."
  (let ((origin (point)) origin-cat
	with-hm inactive
	(dm (max (nth 1 org-time-stamp-rounding-minutes) 1))
	org-ts-what
	extra rem
	ts time time0)
    (if (not (org-at-timestamp-p t))
	(error "Not at a timestamp"))
    (if (and (not what) (eq org-ts-what 'bracket))
	(org-toggle-timestamp-type)
      ;; Point isn't on brackets.  Remember the part of the time-stamp
      ;; the point was in.  Indeed, size of time-stamps may change,
      ;; but point must be kept in the same category nonetheless.
      (setq origin-cat org-ts-what)
      (if (and (not what) (not (eq org-ts-what 'day))
	       org-display-custom-times
	       (get-text-property (point) 'display)
	       (not (get-text-property (1- (point)) 'display)))
	  (setq org-ts-what 'day))
      (setq org-ts-what (or what org-ts-what)
	    inactive (= (char-after (match-beginning 0)) ?\[)
	    ts (match-string 0))
      (replace-match "")
      (if (string-match
	   "\\(\\(-[012][0-9]:[0-5][0-9]\\)?\\( +[.+]?[-+][0-9]+[dwmy]\\(/[0-9]+[dwmy]\\)?\\)*\\)[]>]"
	   ts)
	  (setq extra (match-string 1 ts)))
      (if (string-match "^.\\{10\\}.*?[0-9]+:[0-9][0-9]" ts)
	  (setq with-hm t))
      (setq time0 (org-parse-time-string ts))
      (when (and updown
		 (eq org-ts-what 'minute)
		 (not current-prefix-arg))
	;; This looks like s-up and s-down.  Change by one rounding step.
	(setq n (* dm (cond ((> n 0) 1) ((< n 0) -1) (t 0))))
	(when (not (= 0 (setq rem (% (nth 1 time0) dm))))
	  (setcar (cdr time0) (+ (nth 1 time0)
				 (if (> n 0) (- rem) (- dm rem))))))
      (setq time
	    (encode-time (or (car time0) 0)
			 (+ (if (eq org-ts-what 'minute) n 0) (nth 1 time0))
			 (+ (if (eq org-ts-what 'hour) n 0)   (nth 2 time0))
			 (+ (if (eq org-ts-what 'day) n 0)    (nth 3 time0))
			 (+ (if (eq org-ts-what 'month) n 0)  (nth 4 time0))
			 (+ (if (eq org-ts-what 'year) n 0)   (nth 5 time0))
			 (nthcdr 6 time0)))
      (when (and (member org-ts-what '(hour minute))
		 extra
		 (string-match "-\\([012][0-9]\\):\\([0-5][0-9]\\)" extra))
	(setq extra (org-modify-ts-extra
		     extra
		     (if (eq org-ts-what 'hour) 2 5)
		     n dm)))
      (when (integerp org-ts-what)
	(setq extra (org-modify-ts-extra extra org-ts-what n dm)))
      (if (eq what 'calendar)
	  (let ((cal-date (org-get-date-from-calendar)))
	    (setcar (nthcdr 4 time0) (nth 0 cal-date)) ; month
	    (setcar (nthcdr 3 time0) (nth 1 cal-date)) ; day
	    (setcar (nthcdr 5 time0) (nth 2 cal-date)) ; year
	    (setcar time0 (or (car time0) 0))
	    (setcar (nthcdr 1 time0) (or (nth 1 time0) 0))
	    (setcar (nthcdr 2 time0) (or (nth 2 time0) 0))
	    (setq time (apply 'encode-time time0))))
      ;; Insert the new time-stamp, and ensure point stays in the same
      ;; category as before (i.e. not after the last position in that
      ;; category).
      (let ((pos (point)))
	;; Stay before inserted string. `save-excursion' is of no use.
	(setq org-last-changed-timestamp
	      (org-insert-time-stamp time with-hm inactive nil nil extra))
	(goto-char pos))
      (save-match-data
	(looking-at org-ts-regexp3)
	(goto-char (cond
		    ;; `day' category ends before `hour' if any, or at
		    ;; the end of the day name.
		    ((eq origin-cat 'day)
		     (min (or (match-beginning 7) (1- (match-end 5))) origin))
		    ((eq origin-cat 'hour) (min (match-end 7) origin))
		    ((eq origin-cat 'minute) (min (1- (match-end 8)) origin))
		    ((integerp origin-cat) (min (1- (match-end 0)) origin))
		    ;; `year' and `month' have both fixed size: point
		    ;; couldn't have moved into another part.
		    (t origin))))
      ;; Update clock if on a CLOCK line.
      (org-clock-update-time-maybe)
      ;; Try to recenter the calendar window, if any.
      (if (and org-calendar-follow-timestamp-change
	       (get-buffer-window "*Calendar*" t)
	       (memq org-ts-what '(day month year)))
	  (org-recenter-calendar (time-to-days time))))))

(defun org-modify-ts-extra (s pos n dm)
  "Change the different parts of the lead-time and repeat fields in timestamp."
  (let ((idx '(("d" . 0) ("w" . 1) ("m" . 2) ("y" . 3) ("d" . -1) ("y" . 4)))
	ng h m new rem)
    (when (string-match "\\(-\\([012][0-9]\\):\\([0-5][0-9]\\)\\)?\\( +\\+\\([0-9]+\\)\\([dmwy]\\)\\)?\\( +-\\([0-9]+\\)\\([dmwy]\\)\\)?" s)
      (cond
       ((or (org-pos-in-match-range pos 2)
	    (org-pos-in-match-range pos 3))
	(setq m (string-to-number (match-string 3 s))
	      h (string-to-number (match-string 2 s)))
	(if (org-pos-in-match-range pos 2)
	    (setq h (+ h n))
	  (setq n (* dm (org-no-warnings (signum n))))
	  (when (not (= 0 (setq rem (% m dm))))
	    (setq m (+ m (if (> n 0) (- rem) (- dm rem)))))
	  (setq m (+ m n)))
	(if (< m 0) (setq m (+ m 60) h (1- h)))
	(if (> m 59) (setq m (- m 60) h (1+ h)))
	(setq h (min 24 (max 0 h)))
	(setq ng 1 new (format "-%02d:%02d" h m)))
       ((org-pos-in-match-range pos 6)
	(setq ng 6 new (car (rassoc (+ n (cdr (assoc (match-string 6 s) idx))) idx))))
       ((org-pos-in-match-range pos 5)
	(setq ng 5 new (format "%d" (max 1 (+ n (string-to-number (match-string 5 s)))))))

       ((org-pos-in-match-range pos 9)
	(setq ng 9 new (car (rassoc (+ n (cdr (assoc (match-string 9 s) idx))) idx))))
       ((org-pos-in-match-range pos 8)
	(setq ng 8 new (format "%d" (max 0 (+ n (string-to-number (match-string 8 s))))))))

      (when ng
	(setq s (concat
		 (substring s 0 (match-beginning ng))
		 new
		 (substring s (match-end ng))))))
    s))

(defun org-recenter-calendar (date)
  "If the calendar is visible, recenter it to DATE."
  (let ((cwin (get-buffer-window "*Calendar*" t)))
    (when cwin
      (let ((calendar-move-hook nil))
	(with-selected-window cwin
	  (calendar-goto-date (if (listp date) date
				(calendar-gregorian-from-absolute date))))))))

(defun org-goto-calendar (&optional arg)
  "Go to the Emacs calendar at the current date.
If there is a time stamp in the current line, go to that date.
A prefix ARG can be used to force the current date."
  (interactive "P")
  (let ((tsr org-ts-regexp) diff
	(calendar-move-hook nil)
	(calendar-view-holidays-initially-flag nil)
	(calendar-view-diary-initially-flag nil))
    (if (or (org-at-timestamp-p)
	    (save-excursion
	      (beginning-of-line 1)
	      (looking-at (concat ".*" tsr))))
	(let ((d1 (time-to-days (current-time)))
	      (d2 (time-to-days
		   (org-time-string-to-time (match-string 1)))))
	  (setq diff (- d2 d1))))
    (calendar)
    (calendar-goto-today)
    (if (and diff (not arg)) (calendar-forward-day diff))))

(defun org-get-date-from-calendar ()
  "Return a list (month day year) of date at point in calendar."
  (with-current-buffer "*Calendar*"
    (save-match-data
      (calendar-cursor-to-date))))

(defun org-date-from-calendar ()
  "Insert time stamp corresponding to cursor date in *Calendar* buffer.
If there is already a time stamp at the cursor position, update it."
  (interactive)
  (if (org-at-timestamp-p t)
      (org-timestamp-change 0 'calendar)
    (let ((cal-date (org-get-date-from-calendar)))
      (org-insert-time-stamp
       (encode-time 0 0 0 (nth 1 cal-date) (car cal-date) (nth 2 cal-date))))))

(defun org-minutes-to-hh:mm-string (m)
  "Compute H:MM from a number of minutes."
  (let ((h (/ m 60)))
    (setq m (- m (* 60 h)))
    (format org-time-clocksum-format h m)))

(defun org-hh:mm-string-to-minutes (s)
  "Convert a string H:MM to a number of minutes.
If the string is just a number, interpret it as minutes.
In fact, the first hh:mm or number in the string will be taken,
there can be extra stuff in the string.
If no number is found, the return value is 0."
  (cond
   ((integerp s) s)
   ((string-match "\\([0-9]+\\):\\([0-9]+\\)" s)
    (+ (* (string-to-number (match-string 1 s)) 60)
       (string-to-number (match-string 2 s))))
   ((string-match "\\([0-9]+\\)" s)
    (string-to-number (match-string 1 s)))
   (t 0)))

(defcustom org-effort-durations
  `(("h" . 60)
    ("d" . ,(* 60 8))
    ("w" . ,(* 60 8 5))
    ("m" . ,(* 60 8 5 4))
    ("y" . ,(* 60 8 5 40)))
  "Conversion factor to minutes for an effort modifier.

Each entry has the form (MODIFIER . MINUTES).

In an effort string, a number followed by MODIFIER is multiplied
by the specified number of MINUTES to obtain an effort in
minutes.

For example, if the value of this variable is ((\"hours\" . 60)), then an
effort string \"2hours\" is equivalent to 120 minutes."
  :group 'org-agenda
  :version "24.1"
  :type '(alist :key-type (string :tag "Modifier")
		:value-type (number :tag "Minutes")))

(defun org-duration-string-to-minutes (s)
  "Convert a duration string S to minutes.

A bare number is interpreted as minutes, modifiers can be set by
customizing `org-effort-durations' (which see).

Entries containing a colon are interpreted as H:MM by
`org-hh:mm-string-to-minutes'."
  (let ((result 0)
	(re (concat "\\([0-9]+\\) *\\("
		    (regexp-opt (mapcar 'car org-effort-durations))
		    "\\)")))
    (while (string-match re s)
      (incf result (* (cdr (assoc (match-string 2 s) org-effort-durations))
		      (string-to-number (match-string 1 s))))
      (setq s (replace-match "" nil t s)))
    (incf result (org-hh:mm-string-to-minutes s))
    result))

;;;; Files

(defun org-save-all-org-buffers ()
  "Save all Org-mode buffers without user confirmation."
  (interactive)
  (message "Saving all Org-mode buffers...")
  (save-some-buffers t (lambda () (eq major-mode 'org-mode)))
  (when (featurep 'org-id) (org-id-locations-save))
  (message "Saving all Org-mode buffers... done"))

(defun org-revert-all-org-buffers ()
  "Revert all Org-mode buffers.
Prompt for confirmation when there are unsaved changes.
Be sure you know what you are doing before letting this function
overwrite your changes.

This function is useful in a setup where one tracks org files
with a version control system, to revert on one machine after pulling
changes from another.  I believe the procedure must be like this:

1. M-x org-save-all-org-buffers
2. Pull changes from the other machine, resolve conflicts
3. M-x org-revert-all-org-buffers"
  (interactive)
  (unless (yes-or-no-p "Revert all Org buffers from their files? ")
    (error "Abort"))
  (save-excursion
    (save-window-excursion
      (mapc
       (lambda (b)
	 (when (and (with-current-buffer b (eq major-mode 'org-mode))
		    (with-current-buffer b buffer-file-name))
	   (org-pop-to-buffer-same-window b)
	   (revert-buffer t 'no-confirm)))
       (buffer-list))
      (when (and (featurep 'org-id) org-id-track-globally)
	(org-id-locations-load)))))

;;;; Agenda files

;;;###autoload
(defun org-switchb (&optional arg)
  "Switch between Org buffers.
With one prefix argument, restrict available buffers to files.
With two prefix arguments, restrict available buffers to agenda files.

Defaults to `iswitchb' for buffer name completion.
Set `org-completion-use-ido' to make it use ido instead."
  (interactive "P")
  (let ((blist (cond ((equal arg '(4))  (org-buffer-list 'files))
                     ((equal arg '(16)) (org-buffer-list 'agenda))
                     (t                 (org-buffer-list))))
	(org-completion-use-iswitchb org-completion-use-iswitchb)
	(org-completion-use-ido org-completion-use-ido))
    (unless (or org-completion-use-ido org-completion-use-iswitchb)
      (setq org-completion-use-iswitchb t))
    (org-pop-to-buffer-same-window
     (org-icompleting-read "Org buffer: "
			   (mapcar 'list (mapcar 'buffer-name blist))
			   nil t))))

;;; Define some older names previously used for this functionality
;;;###autoload
(defalias 'org-ido-switchb 'org-switchb)
;;;###autoload
(defalias 'org-iswitchb 'org-switchb)

(defun org-buffer-list (&optional predicate exclude-tmp)
  "Return a list of Org buffers.
PREDICATE can be `export', `files' or `agenda'.

export   restrict the list to Export buffers.
files    restrict the list to buffers visiting Org files.
agenda   restrict the list to buffers visiting agenda files.

If EXCLUDE-TMP is non-nil, ignore temporary buffers."
  (let* ((bfn nil)
	 (agenda-files (and (eq predicate 'agenda)
			    (mapcar 'file-truename (org-agenda-files t))))
	 (filter
	  (cond
	   ((eq predicate 'files)
	    (lambda (b) (with-current-buffer b (eq major-mode 'org-mode))))
	   ((eq predicate 'export)
	    (lambda (b) (string-match "\*Org .*Export" (buffer-name b))))
	   ((eq predicate 'agenda)
	    (lambda (b)
	      (with-current-buffer b
		(and (eq major-mode 'org-mode)
		     (setq bfn (buffer-file-name b))
		     (member (file-truename bfn) agenda-files)))))
	   (t (lambda (b) (with-current-buffer b
			    (or (eq major-mode 'org-mode)
				(string-match "\*Org .*Export"
					      (buffer-name b)))))))))
    (delq nil
	  (mapcar
	   (lambda(b)
	     (if (and (funcall filter b)
		      (or (not exclude-tmp)
			  (not (string-match "tmp" (buffer-name b)))))
		 b
	       nil))
	   (buffer-list)))))

(defun org-agenda-files (&optional unrestricted archives)
  "Get the list of agenda files.
Optional UNRESTRICTED means return the full list even if a restriction
is currently in place.
When ARCHIVES is t, include all archive files that are really being
used by the agenda files.  If ARCHIVE is `ifmode', do this only if
`org-agenda-archives-mode' is t."
  (let ((files
	 (cond
	  ((and (not unrestricted) (get 'org-agenda-files 'org-restrict)))
	  ((stringp org-agenda-files) (org-read-agenda-file-list))
	  ((listp org-agenda-files) org-agenda-files)
	  (t (error "Invalid value of `org-agenda-files'")))))
    (setq files (apply 'append
		       (mapcar (lambda (f)
				 (if (file-directory-p f)
				     (directory-files
				      f t org-agenda-file-regexp)
				   (list f)))
			       files)))
    (when org-agenda-skip-unavailable-files
      (setq files (delq nil
			(mapcar (function
				 (lambda (file)
				   (and (file-readable-p file) file)))
				files))))
    (when (or (eq archives t)
	      (and (eq archives 'ifmode) (eq org-agenda-archives-mode t)))
      (setq files (org-add-archive-files files)))
    files))

(defun org-agenda-file-p (&optional file)
  "Return non-nil, if FILE is an agenda file.
If FILE is omitted, use the file associated with the current
buffer."
  (member (or file (buffer-file-name))
          (org-agenda-files t)))

(defun org-edit-agenda-file-list ()
  "Edit the list of agenda files.
Depending on setup, this either uses customize to edit the variable
`org-agenda-files', or it visits the file that is holding the list.  In the
latter case, the buffer is set up in a way that saving it automatically kills
the buffer and restores the previous window configuration."
  (interactive)
  (if (stringp org-agenda-files)
      (let ((cw (current-window-configuration)))
	(find-file org-agenda-files)
	(org-set-local 'org-window-configuration cw)
	(org-add-hook 'after-save-hook
		      (lambda ()
			(set-window-configuration
			 (prog1 org-window-configuration
			   (kill-buffer (current-buffer))))
			(org-install-agenda-files-menu)
			(message "New agenda file list installed"))
		      nil 'local)
	(message "%s" (substitute-command-keys
		       "Edit list and finish with \\[save-buffer]")))
    (customize-variable 'org-agenda-files)))

(defun org-store-new-agenda-file-list (list)
  "Set new value for the agenda file list and save it correctly."
  (if (stringp org-agenda-files)
      (let ((fe (org-read-agenda-file-list t)) b u)
	(while (setq b (find-buffer-visiting org-agenda-files))
	  (kill-buffer b))
	(with-temp-file org-agenda-files
	  (insert
	   (mapconcat
	    (lambda (f) ;; Keep un-expanded entries.
	      (if (setq u (assoc f fe))
		  (cdr u)
		f))
	    list "\n")
	   "\n")))
    (let ((org-mode-hook nil) (org-inhibit-startup t)
	  (org-insert-mode-line-in-empty-file nil))
      (setq org-agenda-files list)
      (customize-save-variable 'org-agenda-files org-agenda-files))))

(defun org-read-agenda-file-list (&optional pair-with-expansion)
  "Read the list of agenda files from a file.
If PAIR-WITH-EXPANSION is t return pairs with un-expanded
filenames, used by `org-store-new-agenda-file-list' to write back
un-expanded file names."
  (when (file-directory-p org-agenda-files)
    (error "`org-agenda-files' cannot be a single directory"))
  (when (stringp org-agenda-files)
    (with-temp-buffer
      (insert-file-contents org-agenda-files)
      (mapcar
       (lambda (f)
	 (let ((e (expand-file-name (substitute-in-file-name f)
				    org-directory)))
	   (if pair-with-expansion
	       (cons e f)
	     e)))
       (org-split-string (buffer-string) "[ \t\r\n]*?[\r\n][ \t\r\n]*")))))

;;;###autoload
(defun org-cycle-agenda-files ()
  "Cycle through the files in `org-agenda-files'.
If the current buffer visits an agenda file, find the next one in the list.
If the current buffer does not, find the first agenda file."
  (interactive)
  (let* ((fs (org-agenda-files t))
	 (files (append fs (list (car fs))))
	 (tcf (if buffer-file-name (file-truename buffer-file-name)))
	 file)
    (unless files (error "No agenda files"))
    (catch 'exit
      (while (setq file (pop files))
	(if (equal (file-truename file) tcf)
	    (when (car files)
	      (find-file (car files))
	      (throw 'exit t))))
      (find-file (car fs)))
    (if (buffer-base-buffer) (org-pop-to-buffer-same-window (buffer-base-buffer)))))

(defun org-agenda-file-to-front (&optional to-end)
  "Move/add the current file to the top of the agenda file list.
If the file is not present in the list, it is added to the front.  If it is
present, it is moved there.  With optional argument TO-END, add/move to the
end of the list."
  (interactive "P")
  (let ((org-agenda-skip-unavailable-files nil)
	(file-alist (mapcar (lambda (x)
			      (cons (file-truename x) x))
			    (org-agenda-files t)))
	(ctf (file-truename buffer-file-name))
	x had)
    (setq x (assoc ctf file-alist) had x)

    (if (not x) (setq x (cons ctf (abbreviate-file-name buffer-file-name))))
    (if to-end
	(setq file-alist (append (delq x file-alist) (list x)))
      (setq file-alist (cons x (delq x file-alist))))
    (org-store-new-agenda-file-list (mapcar 'cdr file-alist))
    (org-install-agenda-files-menu)
    (message "File %s to %s of agenda file list"
	     (if had "moved" "added") (if to-end "end" "front"))))

(defun org-remove-file (&optional file)
  "Remove current file from the list of files in variable `org-agenda-files'.
These are the files which are being checked for agenda entries.
Optional argument FILE means use this file instead of the current."
  (interactive)
  (let* ((org-agenda-skip-unavailable-files nil)
	 (file (or file buffer-file-name))
	 (true-file (file-truename file))
	 (afile (abbreviate-file-name file))
	 (files (delq nil (mapcar
			   (lambda (x)
			     (if (equal true-file
					(file-truename x))
				 nil x))
			   (org-agenda-files t)))))
    (if (not (= (length files) (length (org-agenda-files t))))
	(progn
	  (org-store-new-agenda-file-list files)
	  (org-install-agenda-files-menu)
	  (message "Removed file: %s" afile))
      (message "File was not in list: %s (not removed)" afile))))

(defun org-file-menu-entry (file)
  (vector file (list 'find-file file) t))

(defun org-check-agenda-file (file)
  "Make sure FILE exists.  If not, ask user what to do."
  (when (not (file-exists-p file))
    (message "non-existent agenda file %s. [R]emove from list or [A]bort?"
	     (abbreviate-file-name file))
    (let ((r (downcase (read-char-exclusive))))
      (cond
       ((equal r ?r)
	(org-remove-file file)
	(throw 'nextfile t))
       (t (error "Abort"))))))

(defun org-get-agenda-file-buffer (file)
  "Get a buffer visiting FILE.  If the buffer needs to be created, add
it to the list of buffers which might be released later."
  (let ((buf (org-find-base-buffer-visiting file)))
    (if buf
	buf ; just return it
      ;; Make a new buffer and remember it
      (setq buf (find-file-noselect file))
      (if buf (push buf org-agenda-new-buffers))
      buf)))

(defun org-release-buffers (blist)
  "Release all buffers in list, asking the user for confirmation when needed.
When a buffer is unmodified, it is just killed.  When modified, it is saved
\(if the user agrees) and then killed."
  (let (buf file)
    (while (setq buf (pop blist))
      (setq file (buffer-file-name buf))
      (when (and (buffer-modified-p buf)
		 file
		 (y-or-n-p (format "Save file %s? " file)))
	(with-current-buffer buf (save-buffer)))
      (kill-buffer buf))))

(defun org-prepare-agenda-buffers (files)
  "Create buffers for all agenda files, protect archived trees and comments."
  (interactive)
  (let ((pa '(:org-archived t))
	(pc '(:org-comment t))
	(pall '(:org-archived t :org-comment t))
	(inhibit-read-only t)
	(rea (concat ":" org-archive-tag ":"))
	     bmp file re)
    (save-excursion
      (save-restriction
	(while (setq file (pop files))
	  (catch 'nextfile
	    (if (bufferp file)
		(set-buffer file)
	      (org-check-agenda-file file)
	      (set-buffer (org-get-agenda-file-buffer file)))
	    (widen)
	    (setq bmp (buffer-modified-p))
	    (org-refresh-category-properties)
	    (setq org-todo-keywords-for-agenda
		  (append org-todo-keywords-for-agenda org-todo-keywords-1))
	    (setq org-done-keywords-for-agenda
		  (append org-done-keywords-for-agenda org-done-keywords))
	    (setq org-todo-keyword-alist-for-agenda
		  (append org-todo-keyword-alist-for-agenda org-todo-key-alist))
	    (setq org-drawers-for-agenda
		  (append org-drawers-for-agenda org-drawers))
	    (setq org-tag-alist-for-agenda
		  (append org-tag-alist-for-agenda org-tag-alist))

	    (save-excursion
	      (remove-text-properties (point-min) (point-max) pall)
	      (when org-agenda-skip-archived-trees
		(goto-char (point-min))
		(while (re-search-forward rea nil t)
		  (if (org-at-heading-p t)
		      (add-text-properties (point-at-bol) (org-end-of-subtree t) pa))))
	      (goto-char (point-min))
	      (setq re (format org-heading-keyword-regexp-format
			       org-comment-string))
	      (while (re-search-forward re nil t)
		(add-text-properties
		 (match-beginning 0) (org-end-of-subtree t) pc)))
	    (set-buffer-modified-p bmp)))))
    (setq org-todo-keywords-for-agenda
          (org-uniquify org-todo-keywords-for-agenda))
    (setq org-todo-keyword-alist-for-agenda
	  (org-uniquify org-todo-keyword-alist-for-agenda)
	  org-tag-alist-for-agenda (org-uniquify org-tag-alist-for-agenda))))

;;;; Embedded LaTeX

(defvar org-cdlatex-mode-map (make-sparse-keymap)
  "Keymap for the minor `org-cdlatex-mode'.")

(org-defkey org-cdlatex-mode-map "_" 'org-cdlatex-underscore-caret)
(org-defkey org-cdlatex-mode-map "^" 'org-cdlatex-underscore-caret)
(org-defkey org-cdlatex-mode-map "`" 'cdlatex-math-symbol)
(org-defkey org-cdlatex-mode-map "'" 'org-cdlatex-math-modify)
(org-defkey org-cdlatex-mode-map "\C-c{" 'cdlatex-environment)

(defvar org-cdlatex-texmathp-advice-is-done nil
  "Flag remembering if we have applied the advice to texmathp already.")

(define-minor-mode org-cdlatex-mode
  "Toggle the minor `org-cdlatex-mode'.
This mode supports entering LaTeX environment and math in LaTeX fragments
in Org-mode.
\\{org-cdlatex-mode-map}"
  nil " OCDL" nil
  (when org-cdlatex-mode
    (require 'cdlatex)
    (run-hooks 'cdlatex-mode-hook)
    (cdlatex-compute-tables))
  (unless org-cdlatex-texmathp-advice-is-done
    (setq org-cdlatex-texmathp-advice-is-done t)
    (defadvice texmathp (around org-math-always-on activate)
      "Always return t in org-mode buffers.
This is because we want to insert math symbols without dollars even outside
the LaTeX math segments.  If Orgmode thinks that point is actually inside
an embedded LaTeX fragment, let texmathp do its job.
\\[org-cdlatex-mode-map]"
      (interactive)
      (let (p)
	(cond
	 ((not (eq major-mode 'org-mode)) ad-do-it)
	 ((eq this-command 'cdlatex-math-symbol)
	  (setq ad-return-value t
		texmathp-why '("cdlatex-math-symbol in org-mode" . 0)))
	 (t
	  (let ((p (org-inside-LaTeX-fragment-p)))
	    (if (and p (member (car p) (plist-get org-format-latex-options :matchers)))
		(setq ad-return-value t
		      texmathp-why '("Org-mode embedded math" . 0))
	      (if p ad-do-it)))))))))

(defun turn-on-org-cdlatex ()
  "Unconditionally turn on `org-cdlatex-mode'."
  (org-cdlatex-mode 1))

(defun org-inside-LaTeX-fragment-p ()
  "Test if point is inside a LaTeX fragment.
I.e. after a \\begin, \\(, \\[, $, or $$, without the corresponding closing
sequence appearing also before point.
Even though the matchers for math are configurable, this function assumes
that \\begin, \\(, \\[, and $$ are always used.  Only the single dollar
delimiters are skipped when they have been removed by customization.
The return value is nil, or a cons cell with the delimiter and the
position of this delimiter.

This function does a reasonably good job, but can locally be fooled by
for example currency specifications.  For example it will assume being in
inline math after \"$22.34\".  The LaTeX fragment formatter will only format
fragments that are properly closed, but during editing, we have to live
with the uncertainty caused by missing closing delimiters.  This function
looks only before point, not after."
  (catch 'exit
    (let ((pos (point))
	  (dodollar (member "$" (plist-get org-format-latex-options :matchers)))
	  (lim (progn
		 (re-search-backward (concat "^\\(" paragraph-start "\\)") nil t)
		 (point)))
	  dd-on str (start 0) m re)
      (goto-char pos)
      (when dodollar
	(setq str (concat (buffer-substring lim (point)) "\000 X$.")
	      re (nth 1 (assoc "$" org-latex-regexps)))
	(while (string-match re str start)
	  (cond
	   ((= (match-end 0) (length str))
	    (throw 'exit (cons "$" (+ lim (match-beginning 0) 1))))
	   ((= (match-end 0) (- (length str) 5))
	    (throw 'exit nil))
	   (t (setq start (match-end 0))))))
      (when (setq m (re-search-backward "\\(\\\\begin{[^}]*}\\|\\\\(\\|\\\\\\[\\)\\|\\(\\\\end{[^}]*}\\|\\\\)\\|\\\\\\]\\)\\|\\(\\$\\$\\)" lim t))
	(goto-char pos)
	(and (match-beginning 1) (throw 'exit (cons (match-string 1) m)))
	(and (match-beginning 2) (throw 'exit nil))
	;; count $$
	(while (re-search-backward "\\$\\$" lim t)
	  (setq dd-on (not dd-on)))
	(goto-char pos)
	(if dd-on (cons "$$" m))))))

(defun org-inside-latex-macro-p ()
  "Is point inside a LaTeX macro or its arguments?"
  (save-match-data
    (org-in-regexp
     "\\\\[a-zA-Z]+\\*?\\(\\(\\[[^][\n{}]*\\]\\)\\|\\({[^{}\n]*}\\)\\)*")))

(defun org-try-cdlatex-tab ()
  "Check if it makes sense to execute `cdlatex-tab', and do it if yes.
It makes sense to do so if `org-cdlatex-mode' is active and if the cursor is
  - inside a LaTeX fragment, or
  - after the first word in a line, where an abbreviation expansion could
    insert a LaTeX environment."
  (when org-cdlatex-mode
    (cond
     ;; Before any word on the line: No expansion possible.
     ((save-excursion (skip-chars-backward " \t") (bolp)) nil)
     ;; Just after first word on the line: Expand it.  Make sure it
     ;; cannot happen on headlines, though.
     ((save-excursion
	(skip-chars-backward "a-zA-Z0-9*")
	(skip-chars-backward " \t")
	(and (bolp) (not (org-at-heading-p))))
      (cdlatex-tab) t)
     ((org-inside-LaTeX-fragment-p) (cdlatex-tab) t))))

(defun org-cdlatex-underscore-caret (&optional arg)
  "Execute `cdlatex-sub-superscript' in LaTeX fragments.
Revert to the normal definition outside of these fragments."
  (interactive "P")
  (if (org-inside-LaTeX-fragment-p)
      (call-interactively 'cdlatex-sub-superscript)
    (let (org-cdlatex-mode)
      (call-interactively (key-binding (vector last-input-event))))))

(defun org-cdlatex-math-modify (&optional arg)
  "Execute `cdlatex-math-modify' in LaTeX fragments.
Revert to the normal definition outside of these fragments."
  (interactive "P")
  (if (org-inside-LaTeX-fragment-p)
      (call-interactively 'cdlatex-math-modify)
    (let (org-cdlatex-mode)
      (call-interactively (key-binding (vector last-input-event))))))

(defvar org-latex-fragment-image-overlays nil
  "List of overlays carrying the images of latex fragments.")
(make-variable-buffer-local 'org-latex-fragment-image-overlays)

(defun org-remove-latex-fragment-image-overlays ()
  "Remove all overlays with LaTeX fragment images in current buffer."
  (mapc 'delete-overlay org-latex-fragment-image-overlays)
  (setq org-latex-fragment-image-overlays nil))

(defun org-preview-latex-fragment (&optional subtree)
  "Preview the LaTeX fragment at point, or all locally or globally.
If the cursor is in a LaTeX fragment, create the image and overlay
it over the source code.  If there is no fragment at point, display
all fragments in the current text, from one headline to the next.  With
prefix SUBTREE, display all fragments in the current subtree.  With a
double prefix arg \\[universal-argument] \\[universal-argument], or when \
the cursor is before the first headline,
display all fragments in the buffer.
The images can be removed again with \\[org-ctrl-c-ctrl-c]."
  (interactive "P")
  (unless buffer-file-name
    (error "Can't preview LaTeX fragment in a non-file buffer"))
  (org-remove-latex-fragment-image-overlays)
  (save-excursion
    (save-restriction
      (let (beg end at msg)
	(cond
	 ((or (equal subtree '(16))
	      (not (save-excursion
		     (re-search-backward org-outline-regexp-bol nil t))))
	  (setq beg (point-min) end (point-max)
		msg "Creating images for buffer...%s"))
	 ((equal subtree '(4))
	  (org-back-to-heading)
	  (setq beg (point) end (org-end-of-subtree t)
		msg "Creating images for subtree...%s"))
	 (t
	  (if (setq at (org-inside-LaTeX-fragment-p))
	      (goto-char (max (point-min) (- (cdr at) 2)))
	    (org-back-to-heading))
	  (setq beg (point) end (progn (outline-next-heading) (point))
		msg (if at "Creating image...%s"
		      "Creating images for entry...%s"))))
	(message msg "")
	(narrow-to-region beg end)
	(goto-char beg)
	(org-format-latex
	 (concat "ltxpng/" (file-name-sans-extension
			    (file-name-nondirectory
			     buffer-file-name)))
	 default-directory 'overlays msg at 'forbuffer 'dvipng)
      (message msg "done.  Use `C-c C-c' to remove images.")))))

(defvar org-latex-regexps
  '(("begin" "^[ \t]*\\(\\\\begin{\\([a-zA-Z0-9\\*]+\\)[^\000]+?\\\\end{\\2}\\)" 1 t)
    ;; ("$" "\\([ 	(]\\|^\\)\\(\\(\\([$]\\)\\([^ 	\r\n,.$].*?\\(\n.*?\\)\\{0,5\\}[^ 	\r\n,.$]\\)\\4\\)\\)\\([ 	.,?;:'\")]\\|$\\)" 2 nil)
    ;; \000 in the following regex is needed for org-inside-LaTeX-fragment-p
    ("$1" "\\([^$]\\|^\\)\\(\\$[^ 	\r\n,;.$]\\$\\)\\([- 	.,?;:'\")\000]\\|$\\)" 2 nil)
    ("$" "\\([^$]\\|^\\)\\(\\(\\$\\([^ 	\r\n,;.$][^$\n\r]*?\\(\n[^$\n\r]*?\\)\\{0,2\\}[^ 	\r\n,.$]\\)\\$\\)\\)\\([- 	.,?;:'\")\000]\\|$\\)" 2 nil)
    ("\\(" "\\\\([^\000]*?\\\\)" 0 nil)
    ("\\[" "\\\\\\[[^\000]*?\\\\\\]" 0 nil)
    ("$$" "\\$\\$[^\000]*?\\$\\$" 0 nil))
  "Regular expressions for matching embedded LaTeX.")

(defvar org-export-have-math nil) ;; dynamic scoping
(defun org-format-latex (prefix &optional dir overlays msg at
				forbuffer processing-type)
  "Replace LaTeX fragments with links to an image, and produce images.
Some of the options can be changed using the variable
`org-format-latex-options'."
  (if (and overlays (fboundp 'clear-image-cache)) (clear-image-cache))
  (let* ((prefixnodir (file-name-nondirectory prefix))
	 (absprefix (expand-file-name prefix dir))
	 (todir (file-name-directory absprefix))
	 (opt org-format-latex-options)
	 (matchers (plist-get opt :matchers))
	 (re-list org-latex-regexps)
	 (org-format-latex-header-extra
	  (plist-get (org-infile-export-plist) :latex-header-extra))
	 (cnt 0) txt hash link beg end re e checkdir
	 executables-checked string
	 m n block-type block linkfile movefile ov)
    ;; Check the different regular expressions
    (while (setq e (pop re-list))
      (setq m (car e) re (nth 1 e) n (nth 2 e) block-type (nth 3 e)
	    block (if block-type "\n\n" ""))
      (when (member m matchers)
	(goto-char (point-min))
	(while (re-search-forward re nil t)
	  (when (and (or (not at) (equal (cdr at) (match-beginning n)))
		     (not (get-text-property (match-beginning n)
					     'org-protected))
		     (or (not overlays)
			 (not (eq (get-char-property (match-beginning n)
						     'org-overlay-type)
				  'org-latex-overlay))))
	    (setq org-export-have-math t)
	    (cond
	     ((eq processing-type 'verbatim)
	      ;; Leave the text verbatim, just protect it
	      (add-text-properties (match-beginning n) (match-end n)
				   '(org-protected t)))
	     ((eq processing-type 'mathjax)
	      ;; Prepare for MathJax processing
	      (setq string (match-string n))
	      (if (member m '("$" "$1"))
		  (save-excursion
		    (delete-region (match-beginning n) (match-end n))
		    (goto-char (match-beginning n))
		    (insert (org-add-props (concat "\\(" (substring string 1 -1)
						   "\\)")
				'(org-protected t))))
		(add-text-properties (match-beginning n) (match-end n)
				     '(org-protected t))))
	     ((eq processing-type 'dvipng)
	      ;; Process to an image
	      (setq txt (match-string n)
		    beg (match-beginning n) end (match-end n)
		    cnt (1+ cnt))
	      (let (print-length print-level) ; make sure full list is printed
		(setq hash (sha1 (prin1-to-string
				  (list org-format-latex-header
					org-format-latex-header-extra
					org-export-latex-default-packages-alist
					org-export-latex-packages-alist
					org-format-latex-options
					forbuffer txt)))
		      linkfile (format "%s_%s.png" prefix hash)
		      movefile (format "%s_%s.png" absprefix hash)))
	      (setq link (concat block "[[file:" linkfile "]]" block))
	      (if msg (message msg cnt))
	      (goto-char beg)
	      (unless checkdir ; make sure the directory exists
		(setq checkdir t)
		(or (file-directory-p todir) (make-directory todir t)))

	      (unless executables-checked
		(org-check-external-command
		 "latex" "needed to convert LaTeX fragments to images")
		(org-check-external-command
		 "dvipng" "needed to convert LaTeX fragments to images")
		(setq executables-checked t))

	      (unless (file-exists-p movefile)
		(org-create-formula-image
		 txt movefile opt forbuffer))
	      (if overlays
		  (progn
		    (mapc (lambda (o)
			    (if (eq (overlay-get o 'org-overlay-type)
				    'org-latex-overlay)
				(delete-overlay o)))
			  (overlays-in beg end))
		    (setq ov (make-overlay beg end))
		    (overlay-put ov 'org-overlay-type 'org-latex-overlay)
		    (if (featurep 'xemacs)
			(progn
			  (overlay-put ov 'invisible t)
			  (overlay-put
			   ov 'end-glyph
			   (make-glyph (vector 'png :file movefile))))
		      (overlay-put
		       ov 'display
		       (list 'image :type 'png :file movefile :ascent 'center)))
		    (push ov org-latex-fragment-image-overlays)
		    (goto-char end))
		(delete-region beg end)
		(insert (org-add-props link
			    (list 'org-latex-src
				  (replace-regexp-in-string
				   "\"" "" txt)
				  'org-latex-src-embed-type
				  (if block-type 'paragraph 'character))))))
	     ((eq processing-type 'mathml)
	      ;; Process to MathML
	      (unless executables-checked
		(unless (save-match-data (org-format-latex-mathml-available-p))
		  (error "LaTeX to MathML converter not configured"))
		(setq executables-checked t))
	      (setq txt (match-string n)
		    beg (match-beginning n) end (match-end n)
		    cnt (1+ cnt))
	      (if msg (message msg cnt))
	      (goto-char beg)
	      (delete-region beg end)
	      (insert (org-format-latex-as-mathml
		       txt block-type prefix dir)))
	     (t
	      (error "Unknown conversion type %s for latex fragments"
		     processing-type)))))))))

(defun org-create-math-formula (latex-frag &optional mathml-file)
  "Convert LATEX-FRAG to MathML and store it in MATHML-FILE.
Use `org-latex-to-mathml-convert-command'.  If the conversion is
sucessful, return the portion between \"<math...> </math>\"
elements otherwise return nil.  When MATHML-FILE is specified,
write the results in to that file.  When invoked as an
interactive command, prompt for LATEX-FRAG, with initial value
set to the current active region and echo the results for user
inspection."
  (interactive (list (let ((frag (when (region-active-p)
				   (buffer-substring-no-properties
				    (region-beginning) (region-end)))))
		       (read-string "LaTeX Fragment: " frag nil frag))))
  (unless latex-frag (error "Invalid latex-frag"))
  (let* ((tmp-in-file (file-relative-name
		       (make-temp-name (expand-file-name "ltxmathml-in"))))
	 (ignore (write-region latex-frag nil tmp-in-file))
	 (tmp-out-file (file-relative-name
			(make-temp-name (expand-file-name  "ltxmathml-out"))))
	 (cmd (format-spec
	       org-latex-to-mathml-convert-command
	       `((?j . ,(shell-quote-argument
			 (expand-file-name org-latex-to-mathml-jar-file)))
		 (?I . ,(shell-quote-argument tmp-in-file))
		 (?o . ,(shell-quote-argument tmp-out-file)))))
	 mathml shell-command-output)
    (when (org-called-interactively-p 'any)
      (unless (org-format-latex-mathml-available-p)
	(error "LaTeX to MathML converter not configured")))
    (message "Running %s" cmd)
    (setq shell-command-output (shell-command-to-string cmd))
    (setq mathml
	  (when (file-readable-p tmp-out-file)
	    (with-current-buffer (find-file-noselect tmp-out-file t)
	      (goto-char (point-min))
	      (when (re-search-forward
		     (concat
		      (regexp-quote
		       "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">")
		      "\\(.\\|\n\\)*"
		      (regexp-quote "</math>")) nil t)
		(prog1 (match-string 0) (kill-buffer))))))
    (cond
     (mathml
      (setq mathml
	    (concat "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" mathml))
      (when mathml-file
	(write-region mathml nil mathml-file))
      (when (org-called-interactively-p 'any)
	(message mathml)))
     ((message "LaTeX to MathML conversion failed")
      (message shell-command-output)))
    (delete-file tmp-in-file)
    (when (file-exists-p tmp-out-file)
      (delete-file tmp-out-file))
    mathml))

(defun org-format-latex-as-mathml (latex-frag latex-frag-type
					      prefix &optional dir)
  "Use `org-create-math-formula' but check local cache first."
  (let* ((absprefix (expand-file-name prefix dir))
	 (print-length nil) (print-level nil)
	 (formula-id (concat
		      "formula-"
		      (sha1
		       (prin1-to-string
			(list latex-frag
			      org-latex-to-mathml-convert-command)))))
	 (formula-cache (format "%s-%s.mathml" absprefix formula-id))
	 (formula-cache-dir (file-name-directory formula-cache)))

    (unless (file-directory-p formula-cache-dir)
      (make-directory formula-cache-dir t))

    (unless (file-exists-p formula-cache)
      (org-create-math-formula latex-frag formula-cache))

    (if (file-exists-p formula-cache)
	;; Successful conversion.  Return the link to MathML file.
	(org-add-props
	    (format  "[[file:%s]]" (file-relative-name formula-cache dir))
	    (list 'org-latex-src (replace-regexp-in-string "\"" "" latex-frag)
		  'org-latex-src-embed-type (if latex-frag-type
						'paragraph 'character)))
      ;; Failed conversion.  Return the LaTeX fragment verbatim
      (add-text-properties
       0 (1- (length latex-frag)) '(org-protected t) latex-frag)
      latex-frag)))

;; This function borrows from Ganesh Swami's latex2png.el
(defun org-create-formula-image (string tofile options buffer)
  "This calls dvipng."
  (require 'org-latex)
  (let* ((tmpdir (if (featurep 'xemacs)
		     (temp-directory)
		   temporary-file-directory))
	 (texfilebase (make-temp-name
		       (expand-file-name "orgtex" tmpdir)))
	 (texfile (concat texfilebase ".tex"))
	 (dvifile (concat texfilebase ".dvi"))
	 (pngfile (concat texfilebase ".png"))
	 (fnh (if (featurep 'xemacs)
                  (font-height (face-font 'default))
                (face-attribute 'default :height nil)))
	 (scale (or (plist-get options (if buffer :scale :html-scale)) 1.0))
	 (dpi (number-to-string (* scale (floor (* 0.9 (if buffer fnh 140.))))))
	 (fg (or (plist-get options (if buffer :foreground :html-foreground))
		 "Black"))
	 (bg (or (plist-get options (if buffer :background :html-background))
		 "Transparent")))
    (if (eq fg 'default) (setq fg (org-dvipng-color :foreground)))
    (if (eq bg 'default) (setq bg (org-dvipng-color :background)))
    (with-temp-file texfile
      (insert (org-splice-latex-header
	       org-format-latex-header
	       org-export-latex-default-packages-alist
	       org-export-latex-packages-alist t
	       org-format-latex-header-extra))
      (insert "\n\\begin{document}\n" string "\n\\end{document}\n")
      (require 'org-latex)
      (org-export-latex-fix-inputenc))
    (let ((dir default-directory))
      (condition-case nil
	  (progn
	    (cd tmpdir)
	    (call-process "latex" nil nil nil texfile))
	(error nil))
      (cd dir))
    (if (not (file-exists-p dvifile))
	(progn (message "Failed to create dvi file from %s" texfile) nil)
      (condition-case nil
	  (if (featurep 'xemacs)
	      	  (call-process "dvipng" nil nil nil
			"-fg" fg "-bg" bg
			"-T" "tight"
			"-o" pngfile
			dvifile)
	    (call-process "dvipng" nil nil nil
			  "-fg" fg "-bg" bg
			  "-D" dpi
			  ;;"-x" scale "-y" scale
			  "-T" "tight"
			  "-o" pngfile
			  dvifile))
	(error nil))
      (if (not (file-exists-p pngfile))
	  (if org-format-latex-signal-error
	      (error "Failed to create png file from %s" texfile)
	    (message "Failed to create png file from %s" texfile)
	    nil)
	;; Use the requested file name and clean up
	(copy-file pngfile tofile 'replace)
	(loop for e in '(".dvi" ".tex" ".aux" ".log" ".png") do
	      (delete-file (concat texfilebase e)))
	pngfile))))

(defun org-splice-latex-header (tpl def-pkg pkg snippets-p &optional extra)
  "Fill a LaTeX header template TPL.
In the template, the following place holders will be recognized:

 [DEFAULT-PACKAGES]      \\usepackage statements for DEF-PKG
 [NO-DEFAULT-PACKAGES]   do not include DEF-PKG
 [PACKAGES]              \\usepackage statements for PKG
 [NO-PACKAGES]           do not include PKG
 [EXTRA]                 the string EXTRA
 [NO-EXTRA]              do not include EXTRA

For backward compatibility, if both the positive and the negative place
holder is missing, the positive one (without the \"NO-\") will be
assumed to be present at the end of the template.
DEF-PKG and PKG are assumed to be alists of options/packagename lists.
EXTRA is a string.
SNIPPETS-P indicates if this is run to create snippet images for HTML."
  (let (rpl (end ""))
    (if (string-match "^[ \t]*\\[\\(NO-\\)?DEFAULT-PACKAGES\\][ \t]*\n?" tpl)
	(setq rpl (if (or (match-end 1) (not def-pkg))
		      "" (org-latex-packages-to-string def-pkg snippets-p t))
	      tpl (replace-match rpl t t tpl))
      (if def-pkg (setq end (org-latex-packages-to-string def-pkg snippets-p))))

    (if (string-match "\\[\\(NO-\\)?PACKAGES\\][ \t]*\n?" tpl)
	(setq rpl (if (or (match-end 1) (not pkg))
		      "" (org-latex-packages-to-string pkg snippets-p t))
	      tpl (replace-match rpl t t tpl))
      (if pkg (setq end
		    (concat end "\n"
			    (org-latex-packages-to-string pkg snippets-p)))))

    (if (string-match "\\[\\(NO-\\)?EXTRA\\][ \t]*\n?" tpl)
	(setq rpl (if (or (match-end 1) (not extra))
		      "" (concat extra "\n"))
	      tpl (replace-match rpl t t tpl))
      (if (and extra (string-match "\\S-" extra))
	  (setq end (concat end "\n" extra))))

    (if (string-match "\\S-" end)
	(concat tpl "\n" end)
      tpl)))

(defun org-latex-packages-to-string (pkg &optional snippets-p newline)
  "Turn an alist of packages into a string with the \\usepackage macros."
  (setq pkg (mapconcat (lambda(p)
			 (cond
			  ((stringp p) p)
			  ((and snippets-p (>= (length p) 3) (not (nth 2 p)))
			   (format "%% Package %s omitted" (cadr p)))
			  ((equal "" (car p))
			   (format "\\usepackage{%s}" (cadr p)))
			  (t
			   (format "\\usepackage[%s]{%s}"
				   (car p) (cadr p)))))
		       pkg
		       "\n"))
  (if newline (concat pkg "\n") pkg))

(defun org-dvipng-color (attr)
  "Return an rgb color specification for dvipng."
  (apply 'format "rgb %s %s %s"
	 (mapcar 'org-normalize-color
		 (if (featurep 'xemacs)
		     (color-rgb-components
		      (face-property 'default
				     (cond ((eq attr :foreground) 'foreground)
					   ((eq attr :background) 'background))))
		   (color-values (face-attribute 'default attr nil))))))

(defun org-normalize-color (value)
  "Return string to be used as color value for an RGB component."
  (format "%g" (/ value 65535.0)))

;; Image display


(defvar org-inline-image-overlays nil)
(make-variable-buffer-local 'org-inline-image-overlays)

(defun org-toggle-inline-images (&optional include-linked)
  "Toggle the display of inline images.
INCLUDE-LINKED is passed to `org-display-inline-images'."
  (interactive "P")
  (if org-inline-image-overlays
      (progn
	(org-remove-inline-images)
	(message "Inline image display turned off"))
    (org-display-inline-images include-linked)
    (if org-inline-image-overlays
	(message "%d images displayed inline"
		 (length org-inline-image-overlays))
      (message "No images to display inline"))))

(defun org-display-inline-images (&optional include-linked refresh beg end)
  "Display inline images.
Normally only links without a description part are inlined, because this
is how it will work for export.  When INCLUDE-LINKED is set, also links
with a description part will be inlined.  This can be nice for a quick
look at those images, but it does not reflect what exported files will look
like.
When REFRESH is set, refresh existing images between BEG and END.
This will create new image displays only if necessary.
BEG and END default to the buffer boundaries."
  (interactive "P")
  (unless refresh
    (org-remove-inline-images)
    (if (fboundp 'clear-image-cache) (clear-image-cache)))
  (save-excursion
    (save-restriction
      (widen)
      (setq beg (or beg (point-min)) end (or end (point-max)))
      (goto-char beg)
      (let ((re (concat "\\[\\[\\(\\(file:\\)\\|\\([./~]\\)\\)\\([^]\n]+?"
			(substring (org-image-file-name-regexp) 0 -2)
			"\\)\\]" (if include-linked "" "\\]")))
	    old file ov img)
	(while (re-search-forward re end t)
	  (setq old (get-char-property-and-overlay (match-beginning 1)
						   'org-image-overlay))
	  (setq file (expand-file-name
		      (concat (or (match-string 3) "") (match-string 4))))
	  (when (file-exists-p file)
	    (if (and (car-safe old) refresh)
		(image-refresh (overlay-get (cdr old) 'display))
	      (setq img (save-match-data (create-image file)))
	      (when img
		(setq ov (make-overlay (match-beginning 0) (match-end 0)))
		(overlay-put ov 'display img)
		(overlay-put ov 'face 'default)
		(overlay-put ov 'org-image-overlay t)
		(overlay-put ov 'modification-hooks
			     (list 'org-display-inline-modification-hook))
		(push ov org-inline-image-overlays)))))))))

(defun org-display-inline-modification-hook (ov after beg end &optional len)
  "Remove inline-display overlay if a corresponding region is modified."
  (let ((inhibit-modification-hooks t))
    (when (and ov after)
      (delete ov org-inline-image-overlays)
      (delete-overlay ov))))

(defun org-remove-inline-images ()
  "Remove inline display of images."
  (interactive)
  (mapc 'delete-overlay org-inline-image-overlays)
  (setq org-inline-image-overlays nil))

;;;; Key bindings

;; Outline functions from `outline-mode-prefix-map'
;; that can be remapped in Org:
(define-key org-mode-map [remap outline-mark-subtree] 'org-mark-subtree)
(define-key org-mode-map [remap show-subtree] 'org-show-subtree)
(define-key org-mode-map [remap outline-forward-same-level]
  'org-forward-same-level)
(define-key org-mode-map [remap outline-backward-same-level]
  'org-backward-same-level)
(define-key org-mode-map [remap show-branches]
  'org-kill-note-or-show-branches)
(define-key org-mode-map [remap outline-promote] 'org-promote-subtree)
(define-key org-mode-map [remap outline-demote] 'org-demote-subtree)
(define-key org-mode-map [remap outline-insert-heading] 'org-ctrl-c-ret)

;; Outline functions from `outline-mode-prefix-map'
;; that can not be remapped in Org:
;; - the column "key binding" shows whether the Outline function is still
;;   available in Org mode on the same key that it has been bound to in
;;   Outline mode:
;;   - "overridden": key used for a different functionality in Org mode
;;   - else: key still bound to the same Outline function in Org mode
;; | Outline function                   | key binding | Org replacement       |
;; |------------------------------------+-------------+-----------------------|
;; | `outline-next-visible-heading'     | `C-c C-n'   | still same function   |
;; | `outline-previous-visible-heading' | `C-c C-p'   | still same function   |
;; | `show-children'                    | `C-c C-i'   | visibility cycling    |
;; | `hide-subtree'                     | overridden  | visibility cycling    |
;; | `outline-up-heading'               | `C-c C-u'   | still same function   |
;; | `hide-body'                        | overridden  | no replacement        |
;; | `show-all'                         | overridden  | no replacement        |
;; | `hide-entry'                       | overridden  | visibility cycling    |
;; | `show-entry'                       | overridden  | no replacement        |
;; | `hide-leaves'                      | overridden  | no replacement        |
;; | `hide-sublevels'                   | overridden  | no replacement        |
;; | `hide-other'                       | overridden  | no replacement        |
;; | `outline-move-subtree-up'          | `C-c C-^'   | better: org-shiftup   |
;; | `outline-move-subtree-down'        | overridden  | better: org-shiftdown |

;; Make `C-c C-x' a prefix key
(org-defkey org-mode-map "\C-c\C-x" (make-sparse-keymap))

;; TAB key with modifiers
(org-defkey org-mode-map "\C-i"       'org-cycle)
(org-defkey org-mode-map [(tab)]      'org-cycle)
(org-defkey org-mode-map [(control tab)] 'org-force-cycle-archived)
(org-defkey org-mode-map "\M-\t" 'pcomplete)
;; The following line is necessary under Suse GNU/Linux
(unless (featurep 'xemacs)
  (org-defkey org-mode-map [S-iso-lefttab]  'org-shifttab))
(org-defkey org-mode-map [(shift tab)]    'org-shifttab)
(define-key org-mode-map [backtab] 'org-shifttab)

(org-defkey org-mode-map [(shift return)]   'org-table-copy-down)
(org-defkey org-mode-map [(meta shift return)] 'org-insert-todo-heading)
(org-defkey org-mode-map [(meta return)]       'org-meta-return)

;; Cursor keys with modifiers
(org-defkey org-mode-map [(meta left)]  'org-metaleft)
(org-defkey org-mode-map [(meta right)] 'org-metaright)
(org-defkey org-mode-map [(meta up)]    'org-metaup)
(org-defkey org-mode-map [(meta down)]  'org-metadown)

(org-defkey org-mode-map [(meta shift left)]   'org-shiftmetaleft)
(org-defkey org-mode-map [(meta shift right)]  'org-shiftmetaright)
(org-defkey org-mode-map [(meta shift up)]     'org-shiftmetaup)
(org-defkey org-mode-map [(meta shift down)]   'org-shiftmetadown)

(org-defkey org-mode-map [(shift up)]          'org-shiftup)
(org-defkey org-mode-map [(shift down)]        'org-shiftdown)
(org-defkey org-mode-map [(shift left)]        'org-shiftleft)
(org-defkey org-mode-map [(shift right)]       'org-shiftright)

(org-defkey org-mode-map [(control shift right)] 'org-shiftcontrolright)
(org-defkey org-mode-map [(control shift left)]  'org-shiftcontrolleft)
(org-defkey org-mode-map [(control shift up)] 'org-shiftcontrolup)
(org-defkey org-mode-map [(control shift down)]  'org-shiftcontroldown)

;; Babel keys
(define-key org-mode-map org-babel-key-prefix org-babel-map)
(mapc (lambda (pair)
        (define-key org-babel-map (car pair) (cdr pair)))
      org-babel-key-bindings)

;;; Extra keys for tty access.
;;  We only set them when really needed because otherwise the
;;  menus don't show the simple keys

(when (or org-use-extra-keys
	  (featurep 'xemacs)   ;; because XEmacs supports multi-device stuff
	  (not window-system))
  (org-defkey org-mode-map "\C-c\C-xc"    'org-table-copy-down)
  (org-defkey org-mode-map "\C-c\C-xM"    'org-insert-todo-heading)
  (org-defkey org-mode-map "\C-c\C-xm"    'org-meta-return)
  (org-defkey org-mode-map [?\e (return)] 'org-meta-return)
  (org-defkey org-mode-map [?\e (left)]   'org-metaleft)
  (org-defkey org-mode-map "\C-c\C-xl"    'org-metaleft)
  (org-defkey org-mode-map [?\e (right)]  'org-metaright)
  (org-defkey org-mode-map "\C-c\C-xr"    'org-metaright)
  (org-defkey org-mode-map [?\e (up)]     'org-metaup)
  (org-defkey org-mode-map "\C-c\C-xu"    'org-metaup)
  (org-defkey org-mode-map [?\e (down)]   'org-metadown)
  (org-defkey org-mode-map "\C-c\C-xd"    'org-metadown)
  (org-defkey org-mode-map "\C-c\C-xL"    'org-shiftmetaleft)
  (org-defkey org-mode-map "\C-c\C-xR"    'org-shiftmetaright)
  (org-defkey org-mode-map "\C-c\C-xU"    'org-shiftmetaup)
  (org-defkey org-mode-map "\C-c\C-xD"    'org-shiftmetadown)
  (org-defkey org-mode-map [?\C-c (up)]    'org-shiftup)
  (org-defkey org-mode-map [?\C-c (down)]  'org-shiftdown)
  (org-defkey org-mode-map [?\C-c (left)]  'org-shiftleft)
  (org-defkey org-mode-map [?\C-c (right)] 'org-shiftright)
  (org-defkey org-mode-map [?\C-c ?\C-x (right)] 'org-shiftcontrolright)
  (org-defkey org-mode-map [?\C-c ?\C-x (left)] 'org-shiftcontrolleft)
  (org-defkey org-mode-map [?\e (tab)] 'pcomplete)
  (org-defkey org-mode-map [?\e (shift return)] 'org-insert-todo-heading)
  (org-defkey org-mode-map [?\e (shift left)]   'org-shiftmetaleft)
  (org-defkey org-mode-map [?\e (shift right)]  'org-shiftmetaright)
  (org-defkey org-mode-map [?\e (shift up)]     'org-shiftmetaup)
  (org-defkey org-mode-map [?\e (shift down)]   'org-shiftmetadown))

  ;; All the other keys

(org-defkey org-mode-map "\C-c\C-a" 'show-all)  ; in case allout messed up.
(org-defkey org-mode-map "\C-c\C-r" 'org-reveal)
(if (boundp 'narrow-map)
    (org-defkey narrow-map "s" 'org-narrow-to-subtree)
  (org-defkey org-mode-map "\C-xns" 'org-narrow-to-subtree))
(if (boundp 'narrow-map)
    (org-defkey narrow-map "b" 'org-narrow-to-block)
  (org-defkey org-mode-map "\C-xnb" 'org-narrow-to-block))
(org-defkey org-mode-map "\C-c\C-f"    'org-forward-same-level)
(org-defkey org-mode-map "\C-c\C-b"    'org-backward-same-level)
(org-defkey org-mode-map "\C-c$"    'org-archive-subtree)
(org-defkey org-mode-map "\C-c\C-x\C-s" 'org-advertized-archive-subtree)
(org-defkey org-mode-map "\C-c\C-x\C-a" 'org-archive-subtree-default)
(org-defkey org-mode-map "\C-c\C-xa" 'org-toggle-archive-tag)
(org-defkey org-mode-map "\C-c\C-xA" 'org-archive-to-archive-sibling)
(org-defkey org-mode-map "\C-c\C-xb" 'org-tree-to-indirect-buffer)
(org-defkey org-mode-map "\C-c\C-j" 'org-goto)
(org-defkey org-mode-map "\C-c\C-t" 'org-todo)
(org-defkey org-mode-map "\C-c\C-q" 'org-set-tags-command)
(org-defkey org-mode-map "\C-c\C-s" 'org-schedule)
(org-defkey org-mode-map "\C-c\C-d" 'org-deadline)
(org-defkey org-mode-map "\C-c;"    'org-toggle-comment)
(org-defkey org-mode-map "\C-c\C-w" 'org-refile)
(org-defkey org-mode-map "\C-c/"    'org-sparse-tree)   ; Minor-mode reserved
(org-defkey org-mode-map "\C-c\\"   'org-match-sparse-tree) ; Minor-mode res.
(org-defkey org-mode-map "\C-c\C-m" 'org-ctrl-c-ret)
(org-defkey org-mode-map "\M-\C-m"  'org-insert-heading)
(org-defkey org-mode-map "\C-c\C-xc" 'org-clone-subtree-with-time-shift)
(org-defkey org-mode-map "\C-c\C-xv" 'org-copy-visible)
(org-defkey org-mode-map [(control return)] 'org-insert-heading-respect-content)
(org-defkey org-mode-map [(shift control return)] 'org-insert-todo-heading-respect-content)
(org-defkey org-mode-map "\C-c\C-x\C-n" 'org-next-link)
(org-defkey org-mode-map "\C-c\C-x\C-p" 'org-previous-link)
(org-defkey org-mode-map "\C-c\C-l" 'org-insert-link)
(org-defkey org-mode-map "\C-c\C-o" 'org-open-at-point)
(org-defkey org-mode-map "\C-c%"    'org-mark-ring-push)
(org-defkey org-mode-map "\C-c&"    'org-mark-ring-goto)
(org-defkey org-mode-map "\C-c\C-z" 'org-add-note)  ; Alternative binding
(org-defkey org-mode-map "\C-c."    'org-time-stamp)  ; Minor-mode reserved
(org-defkey org-mode-map "\C-c!"    'org-time-stamp-inactive) ; Minor-mode r.
(org-defkey org-mode-map "\C-c,"    'org-priority)    ; Minor-mode reserved
(org-defkey org-mode-map "\C-c\C-y" 'org-evaluate-time-range)
(org-defkey org-mode-map "\C-c>"    'org-goto-calendar)
(org-defkey org-mode-map "\C-c<"    'org-date-from-calendar)
(org-defkey org-mode-map [(control ?,)]     'org-cycle-agenda-files)
(org-defkey org-mode-map [(control ?\')]     'org-cycle-agenda-files)
(org-defkey org-mode-map "\C-c["    'org-agenda-file-to-front)
(org-defkey org-mode-map "\C-c]"    'org-remove-file)
(org-defkey org-mode-map "\C-c\C-x<" 'org-agenda-set-restriction-lock)
(org-defkey org-mode-map "\C-c\C-x>" 'org-agenda-remove-restriction-lock)
(org-defkey org-mode-map "\C-c-"    'org-ctrl-c-minus)
(org-defkey org-mode-map "\C-c*"    'org-ctrl-c-star)
(org-defkey org-mode-map "\C-c^"    'org-sort)
(org-defkey org-mode-map "\C-c\C-c" 'org-ctrl-c-ctrl-c)
(org-defkey org-mode-map "\C-c\C-k" 'org-kill-note-or-show-branches)
(org-defkey org-mode-map "\C-c#"    'org-update-statistics-cookies)
(org-defkey org-mode-map "\C-m"     'org-return)
(org-defkey org-mode-map "\C-j"     'org-return-indent)
(org-defkey org-mode-map "\C-c?"    'org-table-field-info)
(org-defkey org-mode-map "\C-c "    'org-table-blank-field)
(org-defkey org-mode-map "\C-c+"    'org-table-sum)
(org-defkey org-mode-map "\C-c="    'org-table-eval-formula)
(org-defkey org-mode-map "\C-c'"    'org-edit-special)
(org-defkey org-mode-map "\C-c`"    'org-table-edit-field)
(org-defkey org-mode-map "\C-c|"    'org-table-create-or-convert-from-region)
(org-defkey org-mode-map [(control ?#)] 'org-table-rotate-recalc-marks)
(org-defkey org-mode-map "\C-c~"    'org-table-create-with-table.el)
(org-defkey org-mode-map "\C-c\C-a" 'org-attach)
(org-defkey org-mode-map "\C-c}"    'org-table-toggle-coordinate-overlays)
(org-defkey org-mode-map "\C-c{"    'org-table-toggle-formula-debugger)
(org-defkey org-mode-map "\C-c\C-e" 'org-export)
(org-defkey org-mode-map "\C-c:"    'org-toggle-fixed-width-section)
(org-defkey org-mode-map "\C-c\C-x\C-f" 'org-emphasize)
(org-defkey org-mode-map "\C-c\C-xf"    'org-footnote-action)
(org-defkey org-mode-map "\C-c\C-x\C-mg"    'org-mobile-pull)
(org-defkey org-mode-map "\C-c\C-x\C-mp"    'org-mobile-push)
(org-defkey org-mode-map "\C-c@" 'org-mark-subtree)
(org-defkey org-mode-map [?\C-c (control ?*)] 'org-list-make-subtree)
;;(org-defkey org-mode-map [?\C-c (control ?-)] 'org-list-make-list-from-subtree)

(org-defkey org-mode-map "\C-c\C-x\C-k" 'org-mark-entry-for-agenda-action)
(org-defkey org-mode-map "\C-c\C-x\C-w" 'org-cut-special)
(org-defkey org-mode-map "\C-c\C-x\M-w" 'org-copy-special)
(org-defkey org-mode-map "\C-c\C-x\C-y" 'org-paste-special)

(org-defkey org-mode-map "\C-c\C-x\C-t" 'org-toggle-time-stamp-overlays)
(org-defkey org-mode-map "\C-c\C-x\C-i" 'org-clock-in)
(org-defkey org-mode-map "\C-c\C-x\C-o" 'org-clock-out)
(org-defkey org-mode-map "\C-c\C-x\C-j" 'org-clock-goto)
(org-defkey org-mode-map "\C-c\C-x\C-x" 'org-clock-cancel)
(org-defkey org-mode-map "\C-c\C-x\C-d" 'org-clock-display)
(org-defkey org-mode-map "\C-c\C-x\C-r" 'org-clock-report)
(org-defkey org-mode-map "\C-c\C-x\C-u" 'org-dblock-update)
(org-defkey org-mode-map "\C-c\C-x\C-l" 'org-preview-latex-fragment)
(org-defkey org-mode-map "\C-c\C-x\C-v" 'org-toggle-inline-images)
(org-defkey org-mode-map "\C-c\C-x\\"   'org-toggle-pretty-entities)
(org-defkey org-mode-map "\C-c\C-x\C-b" 'org-toggle-checkbox)
(org-defkey org-mode-map "\C-c\C-xp"    'org-set-property)
(org-defkey org-mode-map "\C-c\C-xe"    'org-set-effort)
(org-defkey org-mode-map "\C-c\C-xo"    'org-toggle-ordered-property)
(org-defkey org-mode-map "\C-c\C-xi"    'org-insert-columns-dblock)
(org-defkey org-mode-map [(control ?c) (control ?x) ?\;] 'org-timer-set-timer)
(org-defkey org-mode-map [(control ?c) (control ?x) ?\:] 'org-timer-cancel-timer)

(org-defkey org-mode-map "\C-c\C-x."    'org-timer)
(org-defkey org-mode-map "\C-c\C-x-"    'org-timer-item)
(org-defkey org-mode-map "\C-c\C-x0"    'org-timer-start)
(org-defkey org-mode-map "\C-c\C-x_"    'org-timer-stop)
(org-defkey org-mode-map "\C-c\C-x,"    'org-timer-pause-or-continue)

(define-key org-mode-map "\C-c\C-x\C-c" 'org-columns)

(define-key org-mode-map "\C-c\C-x!" 'org-reload)

(define-key org-mode-map "\C-c\C-xg" 'org-feed-update-all)
(define-key org-mode-map "\C-c\C-xG" 'org-feed-goto-inbox)

(define-key org-mode-map "\C-c\C-x[" 'org-reftex-citation)


(when (featurep 'xemacs)
  (org-defkey org-mode-map 'button3   'popup-mode-menu))


(defconst org-speed-commands-default
  '(
    ("Outline Navigation")
    ("n" . (org-speed-move-safe 'outline-next-visible-heading))
    ("p" . (org-speed-move-safe 'outline-previous-visible-heading))
    ("f" . (org-speed-move-safe 'org-forward-same-level))
    ("b" . (org-speed-move-safe 'org-backward-same-level))
    ("u" . (org-speed-move-safe 'outline-up-heading))
    ("j" . org-goto)
    ("g" . (org-refile t))
    ("Outline Visibility")
    ("c" . org-cycle)
    ("C" . org-shifttab)
    (" " . org-display-outline-path)
    ("Outline Structure Editing")
    ("U" . org-shiftmetaup)
    ("D" . org-shiftmetadown)
    ("r" . org-metaright)
    ("l" . org-metaleft)
    ("R" . org-shiftmetaright)
    ("L" . org-shiftmetaleft)
    ("i" . (progn (forward-char 1) (call-interactively
				    'org-insert-heading-respect-content)))
    ("^" . org-sort)
    ("w" . org-refile)
    ("a" . org-archive-subtree-default-with-confirmation)
    ("." . org-mark-subtree)
    ("Clock Commands")
    ("I" . org-clock-in)
    ("O" . org-clock-out)
    ("Meta Data Editing")
    ("t" . org-todo)
    ("0" . (org-priority ?\ ))
    ("1" . (org-priority ?A))
    ("2" . (org-priority ?B))
    ("3" . (org-priority ?C))
    (";" . org-set-tags-command)
    ("e" . org-set-effort)
    ("Agenda Views etc")
    ("v" . org-agenda)
    ("/" . org-sparse-tree)
    ("Misc")
    ("o" . org-open-at-point)
    ("?" . org-speed-command-help)
    ("<" . (org-agenda-set-restriction-lock 'subtree))
    (">" . (org-agenda-remove-restriction-lock))
    )
  "The default speed commands.")

(defun org-print-speed-command (e)
  (if (> (length (car e)) 1)
      (progn
	(princ "\n")
	(princ (car e))
	(princ "\n")
	(princ (make-string (length (car e)) ?-))
	(princ "\n"))
    (princ (car e))
    (princ "   ")
    (if (symbolp (cdr e))
	(princ (symbol-name (cdr e)))
      (prin1 (cdr e)))
    (princ "\n")))

(defun org-speed-command-help ()
  "Show the available speed commands."
  (interactive)
  (if (not org-use-speed-commands)
      (error "Speed commands are not activated, customize `org-use-speed-commands'")
    (with-output-to-temp-buffer "*Help*"
      (princ "User-defined Speed commands\n===========================\n")
      (mapc 'org-print-speed-command org-speed-commands-user)
      (princ "\n")
      (princ "Built-in Speed commands\n=======================\n")
      (mapc 'org-print-speed-command org-speed-commands-default))
    (with-current-buffer "*Help*"
      (setq truncate-lines t))))

(defun org-speed-move-safe (cmd)
  "Execute CMD, but make sure that the cursor always ends up in a headline.
If not, return to the original position and throw an error."
  (interactive)
  (let ((pos (point)))
    (call-interactively cmd)
    (unless (and (bolp) (org-at-heading-p))
      (goto-char pos)
      (error "Boundary reached while executing %s" cmd))))

(defvar org-self-insert-command-undo-counter 0)

(defvar org-table-auto-blank-field) ; defined in org-table.el
(defvar org-speed-command nil)

(defun org-speed-command-default-hook (keys)
  "Hook for activating single-letter speed commands.
`org-speed-commands-default' specifies a minimal command set.
Use `org-speed-commands-user' for further customization."
  (when (or (and (bolp) (looking-at org-outline-regexp))
	    (and (functionp org-use-speed-commands)
		 (funcall org-use-speed-commands)))
    (cdr (assoc keys (append org-speed-commands-user
			     org-speed-commands-default)))))

(defun org-babel-speed-command-hook (keys)
  "Hook for activating single-letter code block commands."
  (when (and (bolp) (looking-at org-babel-src-block-regexp))
    (cdr (assoc keys org-babel-key-bindings))))

(defcustom org-speed-command-hook
  '(org-speed-command-default-hook org-babel-speed-command-hook)
  "Hook for activating speed commands at strategic locations.
Hook functions are called in sequence until a valid handler is
found.

Each hook takes a single argument, a user-pressed command key
which is also a `self-insert-command' from the global map.

Within the hook, examine the cursor position and the command key
and return nil or a valid handler as appropriate.  Handler could
be one of an interactive command, a function, or a form.

Set `org-use-speed-commands' to non-nil value to enable this
hook.  The default setting is `org-speed-command-default-hook'."
  :group 'org-structure
  :type 'hook)

(defun org-self-insert-command (N)
  "Like `self-insert-command', use overwrite-mode for whitespace in tables.
If the cursor is in a table looking at whitespace, the whitespace is
overwritten, and the table is not marked as requiring realignment."
  (interactive "p")
  (org-check-before-invisible-edit 'insert)
  (cond
   ((and org-use-speed-commands
	 (setq org-speed-command
	       (run-hook-with-args-until-success
		'org-speed-command-hook (this-command-keys))))
    (cond
     ((commandp org-speed-command)
      (setq this-command org-speed-command)
      (call-interactively org-speed-command))
     ((functionp org-speed-command)
      (funcall org-speed-command))
     ((and org-speed-command (listp org-speed-command))
      (eval org-speed-command))
     (t (let (org-use-speed-commands)
	  (call-interactively 'org-self-insert-command)))))
   ((and
     (org-table-p)
     (progn
       ;; check if we blank the field, and if that triggers align
       (and (featurep 'org-table) org-table-auto-blank-field
	    (member last-command
		    '(org-cycle org-return org-shifttab org-ctrl-c-ctrl-c yas/expand))
	    (if (or (equal (char-after) ?\ ) (looking-at "[^|\n]*  |"))
		;; got extra space, this field does not determine column width
		(let (org-table-may-need-update) (org-table-blank-field))
		;; no extra space, this field may determine column width
	      (org-table-blank-field)))
       t)
     (eq N 1)
     (looking-at "[^|\n]*  |"))
    (let (org-table-may-need-update)
      (goto-char (1- (match-end 0)))
      (backward-delete-char 1)
      (goto-char (match-beginning 0))
      (self-insert-command N)))
   (t
    (setq org-table-may-need-update t)
    (self-insert-command N)
    (org-fix-tags-on-the-fly)
    (if org-self-insert-cluster-for-undo
	(if (not (eq last-command 'org-self-insert-command))
	    (setq org-self-insert-command-undo-counter 1)
	  (if (>= org-self-insert-command-undo-counter 20)
	      (setq org-self-insert-command-undo-counter 1)
	    (and (> org-self-insert-command-undo-counter 0)
		 buffer-undo-list (listp buffer-undo-list)
		 (not (cadr buffer-undo-list)) ; remove nil entry
		 (setcdr buffer-undo-list (cddr buffer-undo-list)))
	    (setq org-self-insert-command-undo-counter
		  (1+ org-self-insert-command-undo-counter))))))))

(defun org-check-before-invisible-edit (kind)
  "Check is editing if kind KIND would be dangerous with invisible text around.
The detailed reaction depends on the user option `org-catch-invisible-edits'."
  ;; First, try to get out of here as quickly as possible, to reduce overhead
  (if (and org-catch-invisible-edits
	   (or (not (boundp 'visible-mode)) (not visible-mode))
	   (or (get-char-property (point) 'invisible)
	       (get-char-property (max (point-min) (1- (point))) 'invisible)))
      ;; OK, we need to take a closer look
      (let* ((invisible-at-point (get-char-property (point) 'invisible))
	     (invisible-before-point (if (bobp) nil  (get-char-property
						      (1- (point)) 'invisible)))
	     (border-and-ok-direction
	      (or
	       ;; Check if we are acting predictably before invisible text
	       (and invisible-at-point (not invisible-before-point)
		    (memq kind '(insert delete-backward)))
	       ;; Check if we are acting predictably after invisible text
	       ;; This works not well, and I have turned it off.  It seems
	       ;; better to always show and stop after invisible text.
	       ;; (and (not invisible-at-point) invisible-before-point
	       ;;  (memq kind '(insert delete)))
	       )))

	(when (or (memq invisible-at-point '(outline org-hide-block))
		  (memq invisible-before-point '(outline org-hide-block)))
	  (if (eq org-catch-invisible-edits 'error)
	      (error "Editing in invisible areas is prohibited - make visible first"))
	  ;; Make the area visible
	  (save-excursion
	    (if invisible-before-point
		(goto-char (previous-single-char-property-change
			    (point) 'invisible)))
	    (org-cycle))
	  (cond
	   ((eq org-catch-invisible-edits 'show)
	    ;; That's it, we do the edit after showing
	    (message
	     "Unfolding invisible region around point before editing")
	    (sit-for 1))
	   ((and (eq org-catch-invisible-edits 'smart)
		 border-and-ok-direction)
	    (message "Unfolding invisible region around point before editing"))
	   (t
	    ;; Don't do the edit, make the user repeat it in full visibility
	    (error "Edit in invisible region aborted, repeat to confirm with text visible")))))))

(defun org-fix-tags-on-the-fly ()
  (when (and (equal (char-after (point-at-bol)) ?*)
	     (org-at-heading-p))
    (org-align-tags-here org-tags-column)))

(defun org-delete-backward-char (N)
  "Like `delete-backward-char', insert whitespace at field end in tables.
When deleting backwards, in tables this function will insert whitespace in
front of the next \"|\" separator, to keep the table aligned.  The table will
still be marked for re-alignment if the field did fill the entire column,
because, in this case the deletion might narrow the column."
  (interactive "p")
  (org-check-before-invisible-edit 'delete-backward)
  (if (and (org-table-p)
	   (eq N 1)
	   (string-match "|" (buffer-substring (point-at-bol) (point)))
	   (looking-at ".*?|"))
      (let ((pos (point))
	    (noalign (looking-at "[^|\n\r]*  |"))
	    (c org-table-may-need-update))
	(backward-delete-char N)
	(if (not overwrite-mode)
	    (progn
	      (skip-chars-forward "^|")
	      (insert " ")
	      (goto-char (1- pos))))
	;; noalign: if there were two spaces at the end, this field
	;; does not determine the width of the column.
	(if noalign (setq org-table-may-need-update c)))
    (backward-delete-char N)
    (org-fix-tags-on-the-fly)))

(defun org-delete-char (N)
  "Like `delete-char', but insert whitespace at field end in tables.
When deleting characters, in tables this function will insert whitespace in
front of the next \"|\" separator, to keep the table aligned.  The table will
still be marked for re-alignment if the field did fill the entire column,
because, in this case the deletion might narrow the column."
  (interactive "p")
  (org-check-before-invisible-edit 'delete)
  (if (and (org-table-p)
	   (not (bolp))
	   (not (= (char-after) ?|))
	   (eq N 1))
      (if (looking-at ".*?|")
	  (let ((pos (point))
		(noalign (looking-at "[^|\n\r]*  |"))
		(c org-table-may-need-update))
	    (replace-match (concat
			    (substring (match-string 0) 1 -1)
			    " |"))
	    (goto-char pos)
	    ;; noalign: if there were two spaces at the end, this field
	    ;; does not determine the width of the column.
	    (if noalign (setq org-table-may-need-update c)))
	(delete-char N))
    (delete-char N)
    (org-fix-tags-on-the-fly)))

;; Make `delete-selection-mode' work with org-mode and orgtbl-mode
(put 'org-self-insert-command 'delete-selection t)
(put 'orgtbl-self-insert-command 'delete-selection t)
(put 'org-delete-char 'delete-selection 'supersede)
(put 'org-delete-backward-char 'delete-selection 'supersede)
(put 'org-yank 'delete-selection 'yank)

;; Make `flyspell-mode' delay after some commands
(put 'org-self-insert-command 'flyspell-delayed t)
(put 'orgtbl-self-insert-command 'flyspell-delayed t)
(put 'org-delete-char 'flyspell-delayed t)
(put 'org-delete-backward-char 'flyspell-delayed t)

;; Make pabbrev-mode expand after org-mode commands
(put 'org-self-insert-command 'pabbrev-expand-after-command t)
(put 'orgtbl-self-insert-command 'pabbrev-expand-after-command t)

;; How to do this: Measure non-white length of current string
;; If equal to column width, we should realign.

(defun org-remap (map &rest commands)
  "In MAP, remap the functions given in COMMANDS.
COMMANDS is a list of alternating OLDDEF NEWDEF command names."
  (let (new old)
    (while commands
      (setq old (pop commands) new (pop commands))
      (if (fboundp 'command-remapping)
	  (org-defkey map (vector 'remap old) new)
	(substitute-key-definition old new map global-map)))))

(when (eq org-enable-table-editor 'optimized)
  ;; If the user wants maximum table support, we need to hijack
  ;; some standard editing functions
  (org-remap org-mode-map
	     'self-insert-command 'org-self-insert-command
	     'delete-char 'org-delete-char
	     'delete-backward-char 'org-delete-backward-char)
  (org-defkey org-mode-map "|" 'org-force-self-insert))

(defvar org-ctrl-c-ctrl-c-hook nil
  "Hook for functions attaching themselves to `C-c C-c'.

This can be used to add additional functionality to the C-c C-c
key which executes context-dependent commands.  This hook is run
before any other test, while `org-ctrl-c-ctrl-c-final-hook' is
run after the last test.

Each function will be called with no arguments.  The function
must check if the context is appropriate for it to act.  If yes,
it should do its thing and then return a non-nil value.  If the
context is wrong, just do nothing and return nil.")

(defvar org-ctrl-c-ctrl-c-final-hook nil
  "Hook for functions attaching themselves to `C-c C-c'.

This can be used to add additional functionality to the C-c C-c
key which executes context-dependent commands.  This hook is run
after any other test, while `org-ctrl-c-ctrl-c-hook' is run
before the first test.

Each function will be called with no arguments.  The function
must check if the context is appropriate for it to act.  If yes,
it should do its thing and then return a non-nil value.  If the
context is wrong, just do nothing and return nil.")

(defvar org-tab-first-hook nil
  "Hook for functions to attach themselves to TAB.
See `org-ctrl-c-ctrl-c-hook' for more information.
This hook runs as the first action when TAB is pressed, even before
`org-cycle' messes around with the `outline-regexp' to cater for
inline tasks and plain list item folding.
If any function in this hook returns t, any other actions that
would have been caused by TAB (such as table field motion or visibility
cycling) will not occur.")

(defvar org-tab-after-check-for-table-hook nil
  "Hook for functions to attach themselves to TAB.
See `org-ctrl-c-ctrl-c-hook' for more information.
This hook runs after it has been established that the cursor is not in a
table, but before checking if the cursor is in a headline or if global cycling
should be done.
If any function in this hook returns t, not other actions like visibility
cycling will be done.")

(defvar org-tab-after-check-for-cycling-hook nil
  "Hook for functions to attach themselves to TAB.
See `org-ctrl-c-ctrl-c-hook' for more information.
This hook runs after it has been established that not table field motion and
not visibility should be done because of current context.  This is probably
the place where a package like yasnippets can hook in.")

(defvar org-tab-before-tab-emulation-hook nil
  "Hook for functions to attach themselves to TAB.
See `org-ctrl-c-ctrl-c-hook' for more information.
This hook runs after every other options for TAB have been exhausted, but
before indentation and \t insertion takes place.")

(defvar org-metaleft-hook nil
  "Hook for functions attaching themselves to `M-left'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-metaright-hook nil
  "Hook for functions attaching themselves to `M-right'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-metaup-hook nil
  "Hook for functions attaching themselves to `M-up'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-metadown-hook nil
  "Hook for functions attaching themselves to `M-down'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftmetaleft-hook nil
  "Hook for functions attaching themselves to `M-S-left'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftmetaright-hook nil
  "Hook for functions attaching themselves to `M-S-right'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftmetaup-hook nil
  "Hook for functions attaching themselves to `M-S-up'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftmetadown-hook nil
  "Hook for functions attaching themselves to `M-S-down'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-metareturn-hook nil
  "Hook for functions attaching themselves to `M-RET'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftup-hook nil
  "Hook for functions attaching themselves to `S-up'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftup-final-hook nil
  "Hook for functions attaching themselves to `S-up'.
This one runs after all other options except shift-select have been excluded.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftdown-hook nil
  "Hook for functions attaching themselves to `S-down'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftdown-final-hook nil
  "Hook for functions attaching themselves to `S-down'.
This one runs after all other options except shift-select have been excluded.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftleft-hook nil
  "Hook for functions attaching themselves to `S-left'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftleft-final-hook nil
  "Hook for functions attaching themselves to `S-left'.
This one runs after all other options except shift-select have been excluded.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftright-hook nil
  "Hook for functions attaching themselves to `S-right'.
See `org-ctrl-c-ctrl-c-hook' for more information.")
(defvar org-shiftright-final-hook nil
  "Hook for functions attaching themselves to `S-right'.
This one runs after all other options except shift-select have been excluded.
See `org-ctrl-c-ctrl-c-hook' for more information.")

(defun org-modifier-cursor-error ()
  "Throw an error, a modified cursor command was applied in wrong context."
  (error "This command is active in special context like tables, headlines or items"))

(defun org-shiftselect-error ()
  "Throw an error because Shift-Cursor command was applied in wrong context."
  (if (and (boundp 'shift-select-mode) shift-select-mode)
      (error "To use shift-selection with Org-mode, customize `org-support-shift-select'")
    (error "This command works only in special context like headlines or timestamps")))

(defun org-call-for-shift-select (cmd)
  (let ((this-command-keys-shift-translated t))
    (call-interactively cmd)))

(defun org-shifttab (&optional arg)
  "Global visibility cycling or move to previous table field.
Calls `org-cycle' with argument t, or `org-table-previous-field', depending
on context.
See the individual commands for more information."
  (interactive "P")
  (cond
   ((org-at-table-p) (call-interactively 'org-table-previous-field))
   ((integerp arg)
    (let ((arg2 (if org-odd-levels-only (1- (* 2 arg)) arg)))
      (message "Content view to level: %d" arg)
      (org-content (prefix-numeric-value arg2))
      (setq org-cycle-global-status 'overview)))
   (t (call-interactively 'org-global-cycle))))

(defun org-shiftmetaleft ()
  "Promote subtree or delete table column.
Calls `org-promote-subtree', `org-outdent-item-tree', or
`org-table-delete-column', depending on context.  See the
individual commands for more information."
  (interactive)
  (cond
   ((run-hook-with-args-until-success 'org-shiftmetaleft-hook))
   ((org-at-table-p) (call-interactively 'org-table-delete-column))
   ((org-at-heading-p) (call-interactively 'org-promote-subtree))
   ((if (not (org-region-active-p)) (org-at-item-p)
      (save-excursion (goto-char (region-beginning))
		      (org-at-item-p)))
    (call-interactively 'org-outdent-item-tree))
   (t (org-modifier-cursor-error))))

(defun org-shiftmetaright ()
  "Demote subtree or insert table column.
Calls `org-demote-subtree', `org-indent-item-tree', or
`org-table-insert-column', depending on context.  See the
individual commands for more information."
  (interactive)
  (cond
   ((run-hook-with-args-until-success 'org-shiftmetaright-hook))
   ((org-at-table-p) (call-interactively 'org-table-insert-column))
   ((org-at-heading-p) (call-interactively 'org-demote-subtree))
   ((if (not (org-region-active-p)) (org-at-item-p)
      (save-excursion (goto-char (region-beginning))
		      (org-at-item-p)))
    (call-interactively 'org-indent-item-tree))
   (t (org-modifier-cursor-error))))

(defun org-shiftmetaup (&optional arg)
  "Move subtree up or kill table row.
Calls `org-move-subtree-up' or `org-table-kill-row' or
`org-move-item-up' depending on context.  See the individual commands
for more information."
  (interactive "P")
  (cond
   ((run-hook-with-args-until-success 'org-shiftmetaup-hook))
   ((org-at-table-p) (call-interactively 'org-table-kill-row))
   ((org-at-heading-p) (call-interactively 'org-move-subtree-up))
   ((org-at-item-p) (call-interactively 'org-move-item-up))
   (t (org-modifier-cursor-error))))

(defun org-shiftmetadown (&optional arg)
  "Move subtree down or insert table row.
Calls `org-move-subtree-down' or `org-table-insert-row' or
`org-move-item-down', depending on context.  See the individual
commands for more information."
  (interactive "P")
  (cond
   ((run-hook-with-args-until-success 'org-shiftmetadown-hook))
   ((org-at-table-p) (call-interactively 'org-table-insert-row))
   ((org-at-heading-p) (call-interactively 'org-move-subtree-down))
   ((org-at-item-p) (call-interactively 'org-move-item-down))
   (t (org-modifier-cursor-error))))

(defsubst org-hidden-tree-error ()
  (error
   "Hidden subtree, open with TAB or use subtree command M-S-<left>/<right>"))

(defun org-metaleft (&optional arg)
  "Promote heading or move table column to left.
Calls `org-do-promote' or `org-table-move-column', depending on context.
With no specific context, calls the Emacs default `backward-word'.
See the individual commands for more information."
  (interactive "P")
  (cond
   ((run-hook-with-args-until-success 'org-metaleft-hook))
   ((org-at-table-p) (org-call-with-arg 'org-table-move-column 'left))
   ((org-with-limited-levels
     (or (org-at-heading-p)
	 (and (org-region-active-p)
	      (save-excursion
		(goto-char (region-beginning))
		(org-at-heading-p)))))
    (when (org-check-for-hidden 'headlines) (org-hidden-tree-error))
    (call-interactively 'org-do-promote))
   ;; At an inline task.
   ((org-at-heading-p)
    (call-interactively 'org-inlinetask-promote))
   ((or (org-at-item-p)
	(and (org-region-active-p)
	     (save-excursion
	       (goto-char (region-beginning))
	       (org-at-item-p))))
    (when (org-check-for-hidden 'items) (org-hidden-tree-error))
    (call-interactively 'org-outdent-item))
   (t (call-interactively 'backward-word))))

(defun org-metaright (&optional arg)
  "Demote subtree or move table column to right.
Calls `org-do-demote' or `org-table-move-column', depending on context.
With no specific context, calls the Emacs default `forward-word'.
See the individual commands for more information."
  (interactive "P")
  (cond
   ((run-hook-with-args-until-success 'org-metaright-hook))
   ((org-at-table-p) (call-interactively 'org-table-move-column))
   ((org-with-limited-levels
     (or (org-at-heading-p)
	 (and (org-region-active-p)
	      (save-excursion
		(goto-char (region-beginning))
		(org-at-heading-p)))))
    (when (org-check-for-hidden 'headlines) (org-hidden-tree-error))
    (call-interactively 'org-do-demote))
   ;; At an inline task.
   ((org-at-heading-p)
    (call-interactively 'org-inlinetask-demote))
   ((or (org-at-item-p)
	(and (org-region-active-p)
	     (save-excursion
	       (goto-char (region-beginning))
	       (org-at-item-p))))
    (when (org-check-for-hidden 'items) (org-hidden-tree-error))
    (call-interactively 'org-indent-item))
   (t (call-interactively 'forward-word))))

(defun org-check-for-hidden (what)
  "Check if there are hidden headlines/items in the current visual line.
WHAT can be either `headlines' or `items'.  If the current line is
an outline or item heading and it has a folded subtree below it,
this function returns t, nil otherwise."
  (let ((re (cond
	     ((eq what 'headlines) org-outline-regexp-bol)
	     ((eq what 'items) (org-item-beginning-re))
	     (t (error "This should not happen"))))
	beg end)
    (save-excursion
      (catch 'exit
	(unless (org-region-active-p)
	  (setq beg (point-at-bol))
	  (beginning-of-line 2)
	  (while (and (not (eobp)) ;; this is like `next-line'
		      (get-char-property (1- (point)) 'invisible))
	    (beginning-of-line 2))
	  (setq end (point))
	  (goto-char beg)
	  (goto-char (point-at-eol))
	  (setq end (max end (point)))
	  (while (re-search-forward re end t)
	    (if (get-char-property (match-beginning 0) 'invisible)
		(throw 'exit t))))
	nil))))

(defun org-metaup (&optional arg)
  "Move subtree up or move table row up.
Calls `org-move-subtree-up' or `org-table-move-row' or
`org-move-item-up', depending on context.  See the individual commands
for more information."
  (interactive "P")
  (cond
   ((run-hook-with-args-until-success 'org-metaup-hook))
   ((org-at-table-p) (org-call-with-arg 'org-table-move-row 'up))
   ((org-at-heading-p) (call-interactively 'org-move-subtree-up))
   ((org-at-item-p) (call-interactively 'org-move-item-up))
   (t (transpose-lines 1) (beginning-of-line -1))))

(defun org-metadown (&optional arg)
  "Move subtree down or move table row down.
Calls `org-move-subtree-down' or `org-table-move-row' or
`org-move-item-down', depending on context.  See the individual
commands for more information."
  (interactive "P")
  (cond
   ((run-hook-with-args-until-success 'org-metadown-hook))
   ((org-at-table-p) (call-interactively 'org-table-move-row))
   ((org-at-heading-p) (call-interactively 'org-move-subtree-down))
   ((org-at-item-p) (call-interactively 'org-move-item-down))
   (t (beginning-of-line 2) (transpose-lines 1) (beginning-of-line 0))))

(defun org-shiftup (&optional arg)
  "Increase item in timestamp or increase priority of current headline.
Calls `org-timestamp-up' or `org-priority-up', or `org-previous-item',
depending on context.  See the individual commands for more information."
  (interactive "P")
  (cond
   ((run-hook-with-args-until-success 'org-shiftup-hook))
   ((and org-support-shift-select (org-region-active-p))
    (org-call-for-shift-select 'previous-line))
   ((org-at-timestamp-p t)
    (call-interactively (if org-edit-timestamp-down-means-later
			    'org-timestamp-down 'org-timestamp-up)))
   ((and (not (eq org-support-shift-select 'always))
	 org-enable-priority-commands
	 (org-at-heading-p))
    (call-interactively 'org-priority-up))
   ((and (not org-support-shift-select) (org-at-item-p))
    (call-interactively 'org-previous-item))
   ((org-clocktable-try-shift 'up arg))
   ((run-hook-with-args-until-success 'org-shiftup-final-hook))
   (org-support-shift-select
    (org-call-for-shift-select 'previous-line))
   (t (org-shiftselect-error))))

(defun org-shiftdown (&optional arg)
  "Decrease item in timestamp or decrease priority of current headline.
Calls `org-timestamp-down' or `org-priority-down', or `org-next-item'
depending on context.  See the individual commands for more information."
  (interactive "P")
  (cond
   ((run-hook-with-args-until-success 'org-shiftdown-hook))
   ((and org-support-shift-select (org-region-active-p))
    (org-call-for-shift-select 'next-line))
   ((org-at-timestamp-p t)
    (call-interactively (if org-edit-timestamp-down-means-later
			    'org-timestamp-up 'org-timestamp-down)))
   ((and (not (eq org-support-shift-select 'always))
	 org-enable-priority-commands
	 (org-at-heading-p))
    (call-interactively 'org-priority-down))
   ((and (not org-support-shift-select) (org-at-item-p))
    (call-interactively 'org-next-item))
   ((org-clocktable-try-shift 'down arg))
   ((run-hook-with-args-until-success 'org-shiftdown-final-hook))
   (org-support-shift-select
    (org-call-for-shift-select 'next-line))
   (t (org-shiftselect-error))))

(defun org-shiftright (&optional arg)
  "Cycle the thing at point or in the current line, depending on context.
Depending on context, this does one of the following:

- switch a timestamp at point one day into the future
- on a headline, switch to the next TODO keyword.
- on an item, switch entire list to the next bullet type
- on a property line, switch to the next allowed value
- on a clocktable definition line, move time block into the future"
  (interactive "P")
  (cond
   ((run-hook-with-args-until-success 'org-shiftright-hook))
   ((and org-support-shift-select (org-region-active-p))
    (org-call-for-shift-select 'forward-char))
   ((org-at-timestamp-p t) (call-interactively 'org-timestamp-up-day))
   ((and (not (eq org-support-shift-select 'always))
	 (org-at-heading-p))
    (let ((org-inhibit-logging
	   (not org-treat-S-cursor-todo-selection-as-state-change))
	  (org-inhibit-blocking
	   (not org-treat-S-cursor-todo-selection-as-state-change)))
      (org-call-with-arg 'org-todo 'right)))
   ((or (and org-support-shift-select
	     (not (eq org-support-shift-select 'always))
	     (org-at-item-bullet-p))
	(and (not org-support-shift-select) (org-at-item-p)))
    (org-call-with-arg 'org-cycle-list-bullet nil))
   ((and (not (eq org-support-shift-select 'always))
	 (org-at-property-p))
    (call-interactively 'org-property-next-allowed-value))
   ((org-clocktable-try-shift 'right arg))
   ((run-hook-with-args-until-success 'org-shiftright-final-hook))
   (org-support-shift-select
    (org-call-for-shift-select 'forward-char))
   (t (org-shiftselect-error))))

(defun org-shiftleft (&optional arg)
  "Cycle the thing at point or in the current line, depending on context.
Depending on context, this does one of the following:

- switch a timestamp at point one day into the past
- on a headline, switch to the previous TODO keyword.
- on an item, switch entire list to the previous bullet type
- on a property line, switch to the previous allowed value
- on a clocktable definition line, move time block into the past"
  (interactive "P")
  (cond
   ((run-hook-with-args-until-success 'org-shiftleft-hook))
   ((and org-support-shift-select (org-region-active-p))
    (org-call-for-shift-select 'backward-char))
   ((org-at-timestamp-p t) (call-interactively 'org-timestamp-down-day))
   ((and (not (eq org-support-shift-select 'always))
	 (org-at-heading-p))
    (let ((org-inhibit-logging
	   (not org-treat-S-cursor-todo-selection-as-state-change))
	  (org-inhibit-blocking
	   (not org-treat-S-cursor-todo-selection-as-state-change)))
      (org-call-with-arg 'org-todo 'left)))
   ((or (and org-support-shift-select
	     (not (eq org-support-shift-select 'always))
	     (org-at-item-bullet-p))
	(and (not org-support-shift-select) (org-at-item-p)))
    (org-call-with-arg 'org-cycle-list-bullet 'previous))
   ((and (not (eq org-support-shift-select 'always))
	 (org-at-property-p))
    (call-interactively 'org-property-previous-allowed-value))
   ((org-clocktable-try-shift 'left arg))
   ((run-hook-with-args-until-success 'org-shiftleft-final-hook))
   (org-support-shift-select
    (org-call-for-shift-select 'backward-char))
   (t (org-shiftselect-error))))

(defun org-shiftcontrolright ()
  "Switch to next TODO set."
  (interactive)
  (cond
   ((and org-support-shift-select (org-region-active-p))
    (org-call-for-shift-select 'forward-word))
   ((and (not (eq org-support-shift-select 'always))
	 (org-at-heading-p))
    (org-call-with-arg 'org-todo 'nextset))
   (org-support-shift-select
    (org-call-for-shift-select 'forward-word))
   (t (org-shiftselect-error))))

(defun org-shiftcontrolleft ()
  "Switch to previous TODO set."
  (interactive)
  (cond
   ((and org-support-shift-select (org-region-active-p))
    (org-call-for-shift-select 'backward-word))
   ((and (not (eq org-support-shift-select 'always))
	 (org-at-heading-p))
    (org-call-with-arg 'org-todo 'previousset))
   (org-support-shift-select
    (org-call-for-shift-select 'backward-word))
   (t (org-shiftselect-error))))

(defun org-shiftcontrolup ()
  "Change timestamps synchronously up in CLOCK log lines."
  (interactive)
  (cond ((and (not org-support-shift-select)
	      (org-at-clock-log-p)
	      (org-at-timestamp-p t))
	 (org-clock-timestamps-up))
	(t (org-shiftselect-error))))

(defun org-shiftcontroldown ()
  "Change timestamps synchronously down in CLOCK log lines."
  (interactive)
  (cond ((and (not org-support-shift-select)
	      (org-at-clock-log-p)
	      (org-at-timestamp-p t))
	 (org-clock-timestamps-down))
	(t (org-shiftselect-error))))

(defun org-ctrl-c-ret ()
  "Call `org-table-hline-and-move' or `org-insert-heading' dep. on context."
  (interactive)
  (cond
   ((org-at-table-p) (call-interactively 'org-table-hline-and-move))
   (t (call-interactively 'org-insert-heading))))

(defun org-find-visible ()
  (let ((s (point)))
    (while (and (not (= (point-max) (setq s (next-overlay-change s))))
		(get-char-property s 'invisible)))
    s))
(defun org-find-invisible ()
  (let ((s (point)))
    (while (and (not (= (point-max) (setq s (next-overlay-change s))))
		(not (get-char-property s 'invisible))))
    s))

(defun org-copy-visible (beg end)
  "Copy the visible parts of the region."
 (interactive "r")
 (let (snippets s)
   (save-excursion
     (save-restriction
	(narrow-to-region beg end)
	(setq s (goto-char (point-min)))
	(while (not (= (point) (point-max)))
	  (goto-char (org-find-invisible))
	  (push (buffer-substring s (point)) snippets)
	  (setq s (goto-char (org-find-visible))))))
   (kill-new (apply 'concat (nreverse snippets)))))

(defun org-copy-special ()
  "Copy region in table or copy current subtree.
Calls `org-table-copy' or `org-copy-subtree', depending on context.
See the individual commands for more information."
  (interactive)
  (call-interactively
   (if (org-at-table-p) 'org-table-copy-region 'org-copy-subtree)))

(defun org-cut-special ()
  "Cut region in table or cut current subtree.
Calls `org-table-copy' or `org-cut-subtree', depending on context.
See the individual commands for more information."
  (interactive)
  (call-interactively
   (if (org-at-table-p) 'org-table-cut-region 'org-cut-subtree)))

(defun org-paste-special (arg)
  "Paste rectangular region into table, or past subtree relative to level.
Calls `org-table-paste-rectangle' or `org-paste-subtree', depending on context.
See the individual commands for more information."
  (interactive "P")
  (if (org-at-table-p)
      (org-table-paste-rectangle)
    (org-paste-subtree arg)))

(defun org-edit-special (&optional arg)
  "Call a special editor for the stuff at point.
When at a table, call the formula editor with `org-table-edit-formulas'.
When at the first line of an src example, call `org-edit-src-code'.
When in an #+include line, visit the include file.  Otherwise call
`ffap' to visit the file at point."
  (interactive)
  ;; possibly prep session before editing source
  (when arg
    (let* ((info (org-babel-get-src-block-info))
           (lang (nth 0 info))
           (params (nth 2 info))
           (session (cdr (assoc :session params))))
      (when (and info session) ;; we are in a source-code block with a session
        (funcall
         (intern (concat "org-babel-prep-session:" lang)) session params))))
  (cond ;; proceed with `org-edit-special'
   ((save-excursion
      (beginning-of-line 1)
      (looking-at "\\(?:#\\+\\(?:setupfile\\|include\\):?[ \t]+\"?\\|[ \t]*<include\\>.*?file=\"\\)\\([^\"\n>]+\\)"))
    (find-file (org-trim (match-string 1))))
   ((org-edit-src-code))
   ((org-edit-fixed-width-region))
   ((org-at-table.el-p)
    (org-edit-src-code))
   ((or (org-at-table-p)
	(save-excursion
	  (beginning-of-line 1)
	  (looking-at "[ \t]*#\\+TBLFM:")))
    (call-interactively 'org-table-edit-formulas))
   (t (call-interactively 'ffap))))

(defvar org-table-coordinate-overlays) ; defined in org-table.el
(defun org-ctrl-c-ctrl-c (&optional arg)
  "Set tags in headline, or update according to changed information at point.

This command does many different things, depending on context:

- If a function in `org-ctrl-c-ctrl-c-hook' recognizes this location,
  this is what we do.

- If the cursor is on a statistics cookie, update it.

- If the cursor is in a headline, prompt for tags and insert them
  into the current line, aligned to `org-tags-column'.  When called
  with prefix arg, realign all tags in the current buffer.

- If the cursor is in one of the special #+KEYWORD lines, this
  triggers scanning the buffer for these lines and updating the
  information.

- If the cursor is inside a table, realign the table.  This command
  works even if the automatic table editor has been turned off.

- If the cursor is on a #+TBLFM line, re-apply the formulas to
  the entire table.

- If the cursor is at a footnote reference or definition, jump to
  the corresponding definition or references, respectively.

- If the cursor is a the beginning of a dynamic block, update it.

- If the current buffer is a capture buffer, close note and file it.

- If the cursor is on a <<<target>>>, update radio targets and
  corresponding links in this buffer.

- If the cursor is on a numbered item in a plain list, renumber the
  ordered list.

- If the cursor is on a checkbox, toggle it.

- If the cursor is on a code block, evaluate it.  The variable
  `org-confirm-babel-evaluate' can be used to control prompting
  before code block evaluation, by default every code block
  evaluation requires confirmation.  Code block evaluation can be
  inhibited by setting `org-babel-no-eval-on-ctrl-c-ctrl-c'."
  (interactive "P")
  (let  ((org-enable-table-editor t))
    (cond
     ((or (and (boundp 'org-clock-overlays) org-clock-overlays)
	  org-occur-highlights
	  org-latex-fragment-image-overlays)
      (and (boundp 'org-clock-overlays) (org-clock-remove-overlays))
      (org-remove-occur-highlights)
      (org-remove-latex-fragment-image-overlays)
      (message "Temporary highlights/overlays removed from current buffer"))
     ((and (local-variable-p 'org-finish-function (current-buffer))
	   (fboundp org-finish-function))
      (funcall org-finish-function))
     ((run-hook-with-args-until-success 'org-ctrl-c-ctrl-c-hook))
     ((org-in-regexp org-ts-regexp-both)
      (org-timestamp-change 0 'day))
     ((or (looking-at org-property-start-re)
	  (org-at-property-p))
      (call-interactively 'org-property-action))
     ((org-at-target-p) (call-interactively 'org-update-radio-target-regexp))
     ((and (org-in-regexp "\\[\\([0-9]*%\\|[0-9]*/[0-9]*\\)\\]")
	   (or (org-at-heading-p) (org-at-item-p)))
      (call-interactively 'org-update-statistics-cookies))
     ((org-at-heading-p) (call-interactively 'org-set-tags))
     ((org-at-table.el-p)
      (message "Use C-c ' to edit table.el tables"))
     ((org-at-table-p)
      (org-table-maybe-eval-formula)
      (if arg
	  (call-interactively 'org-table-recalculate)
	(org-table-maybe-recalculate-line))
      (call-interactively 'org-table-align)
      (orgtbl-send-table 'maybe))
     ((or (org-footnote-at-reference-p)
	  (org-footnote-at-definition-p))
      (call-interactively 'org-footnote-action))
     ((org-at-item-checkbox-p)
      ;; Cursor at a checkbox: repair list and update checkboxes. Send
      ;; list only if at top item.
      (let* ((cbox (match-string 1))
	     (struct (org-list-struct))
	     (old-struct (copy-tree struct))
	     (parents (org-list-parents-alist struct))
	     (orderedp (org-entry-get nil "ORDERED"))
	     (firstp (= (org-list-get-top-point struct) (point-at-bol)))
	     block-item)
	;; Use a light version of `org-toggle-checkbox' to avoid
	;; computing list structure twice.
	(let ((new-box (cond
			((equal arg '(16)) "[-]")
			((equal arg '(4)) nil)
			((equal "[X]" cbox) "[ ]")
			(t "[X]"))))
	  (if (and firstp arg)
	      ;; If at first item of sub-list, remove check-box from
	      ;; every item at the same level.
	      (mapc
	       (lambda (pos) (org-list-set-checkbox pos struct new-box))
	       (org-list-get-all-items
		(point-at-bol) struct (org-list-prevs-alist struct)))
	    (org-list-set-checkbox (point-at-bol) struct new-box)))
	;; Replicate `org-list-write-struct', while grabbing a return
	;; value from `org-list-struct-fix-box'.
	(org-list-struct-fix-ind struct parents 2)
	(org-list-struct-fix-item-end struct)
	(let ((prevs (org-list-prevs-alist struct)))
	  (org-list-struct-fix-bul struct prevs)
	  (org-list-struct-fix-ind struct parents)
	  (setq block-item
		(org-list-struct-fix-box struct parents prevs orderedp)))
	(org-list-struct-apply-struct struct old-struct)
	(org-update-checkbox-count-maybe)
	(when block-item
	  (message
	   "Checkboxes were removed due to unchecked box at line %d"
	   (org-current-line block-item)))
	(when firstp (org-list-send-list 'maybe))))
     ((org-at-item-p)
      ;; Cursor at an item: repair list.  Do checkbox related actions
      ;; only if function was called with an argument.  Send list only
      ;; if at top item.
      (let* ((struct (org-list-struct))
	     (firstp (= (org-list-get-top-point struct) (point-at-bol)))
	     old-struct)
	(when arg
	  (setq old-struct (copy-tree struct))
	  (if firstp
	      ;; If at first item of sub-list, add check-box to every
	      ;; item at the same level.
	      (mapc
	       (lambda (pos)
		 (unless (org-list-get-checkbox pos struct)
		   (org-list-set-checkbox pos struct "[ ]")))
	       (org-list-get-all-items
		(point-at-bol) struct (org-list-prevs-alist struct)))
	    (org-list-set-checkbox (point-at-bol) struct "[ ]")))
	(org-list-write-struct
	 struct (org-list-parents-alist struct) old-struct)
	(when arg (org-update-checkbox-count-maybe))
	(when firstp (org-list-send-list 'maybe))))
     ((save-excursion (beginning-of-line 1) (looking-at org-dblock-start-re))
      ;; Dynamic block
      (beginning-of-line 1)
      (save-excursion (org-update-dblock)))
     ((save-excursion
	(beginning-of-line 1)
	(looking-at "[ \t]*#\\+\\([A-Z]+\\)"))
      (cond
       ((equal (match-string 1) "TBLFM")
	;; Recalculate the table before this line
	(save-excursion
	  (beginning-of-line 1)
	  (skip-chars-backward " \r\n\t")
	  (if (org-at-table-p)
	      (org-call-with-arg 'org-table-recalculate (or arg t)))))
       (t
	(let ((org-inhibit-startup-visibility-stuff t)
	      (org-startup-align-all-tables nil))
	  (when (boundp 'org-table-coordinate-overlays)
	    (mapc 'delete-overlay org-table-coordinate-overlays)
	    (setq org-table-coordinate-overlays nil))
	  (org-save-outline-visibility 'use-markers (org-mode-restart)))
	(message "Local setup has been refreshed"))))
     ((org-clock-update-time-maybe))
     (t
      (or (run-hook-with-args-until-success 'org-ctrl-c-ctrl-c-final-hook)
	  (error "C-c C-c can do nothing useful at this location"))))))

(defun org-mode-restart ()
  "Restart Org-mode, to scan again for special lines.
Also updates the keyword regular expressions."
  (interactive)
  (org-mode)
  (message "Org-mode restarted"))

(defun org-kill-note-or-show-branches ()
  "If this is a Note buffer, abort storing the note.  Else call `show-branches'."
  (interactive)
  (if (not org-finish-function)
      (progn
	(hide-subtree)
	(call-interactively 'show-branches))
    (let ((org-note-abort t))
      (funcall org-finish-function))))

(defun org-return (&optional indent)
  "Goto next table row or insert a newline.
Calls `org-table-next-row' or `newline', depending on context.
See the individual commands for more information."
  (interactive)
  (cond
   ((bobp) (if indent (newline-and-indent) (newline)))
   ((org-at-table-p)
    (org-table-justify-field-maybe)
    (call-interactively 'org-table-next-row))
   ;; when `newline-and-indent' is called within a list, make sure
   ;; text moved stays inside the item.
   ((and (org-in-item-p) indent)
    (if (and (org-at-item-p) (>= (point) (match-end 0)))
	(progn
	  (save-match-data (newline))
	  (org-indent-line-to (length (match-string 0))))
      (let ((ind (org-get-indentation)))
	(newline)
	(if (org-looking-back org-list-end-re)
	    (org-indent-line-function)
	  (org-indent-line-to ind)))))
   ((and org-return-follows-link
         (let ((tprop (get-text-property (point) 'face)))
	   (or (eq tprop 'org-link)
	       (and (listp tprop) (memq 'org-link tprop)))))
    (call-interactively 'org-open-at-point))
   ((and (org-at-heading-p)
	 (looking-at
	  (org-re "\\([ \t]+\\(:[[:alnum:]_@#%:]+:\\)\\)[ \t]*$")))
    (org-show-entry)
    (end-of-line 1)
    (newline))
   (t (if indent (newline-and-indent) (newline)))))

(defun org-return-indent ()
  "Goto next table row or insert a newline and indent.
Calls `org-table-next-row' or `newline-and-indent', depending on
context.  See the individual commands for more information."
  (interactive)
  (org-return t))

(defun org-ctrl-c-star ()
  "Compute table, or change heading status of lines.
Calls `org-table-recalculate' or `org-toggle-heading',
depending on context."
  (interactive)
  (cond
   ((org-at-table-p)
    (call-interactively 'org-table-recalculate))
   (t
    ;; Convert all lines in region to list items
    (call-interactively 'org-toggle-heading))))

(defun org-ctrl-c-minus ()
  "Insert separator line in table or modify bullet status of line.
Also turns a plain line or a region of lines into list items.
Calls `org-table-insert-hline', `org-toggle-item', or
`org-cycle-list-bullet', depending on context."
  (interactive)
  (cond
   ((org-at-table-p)
    (call-interactively 'org-table-insert-hline))
   ((org-region-active-p)
    (call-interactively 'org-toggle-item))
   ((org-in-item-p)
    (call-interactively 'org-cycle-list-bullet))
   (t
    (call-interactively 'org-toggle-item))))

(defun org-toggle-item (arg)
  "Convert headings or normal lines to items, items to normal lines.
If there is no active region, only the current line is considered.

If the first non blank line in the region is an headline, convert
all headlines to items, shifting text accordingly.

If it is an item, convert all items to normal lines.

If it is normal text, change region into an item.  With a prefix
argument ARG, change each line in region into an item."
  (interactive "P")
  (let ((shift-text
	 (function
	  ;; Shift text in current section to IND, from point to END.
	  ;; The function leaves point to END line.
	  (lambda (ind end)
	    (let ((min-i 1000) (end (copy-marker end)))
	      ;; First determine the minimum indentation (MIN-I) of
	      ;; the text.
	      (save-excursion
		(catch 'exit
		  (while (< (point) end)
		    (let ((i (org-get-indentation)))
		      (cond
		       ;; Skip blank lines and inline tasks.
		       ((looking-at "^[ \t]*$"))
		       ((looking-at org-outline-regexp-bol))
		       ;; We can't find less than 0 indentation.
		       ((zerop i) (throw 'exit (setq min-i 0)))
		       ((< i min-i) (setq min-i i))))
		    (forward-line))))
	      ;; Then indent each line so that a line indented to
	      ;; MIN-I becomes indented to IND.  Ignore blank lines
	      ;; and inline tasks in the process.
	      (let ((delta (- ind min-i)))
		(while (< (point) end)
		  (unless (or (looking-at "^[ \t]*$")
			      (looking-at org-outline-regexp-bol))
		    (org-indent-line-to (+ (org-get-indentation) delta)))
		  (forward-line)))))))
	(skip-blanks
	 (function
	  ;; Return beginning of first non-blank line, starting from
	  ;; line at POS.
	  (lambda (pos)
	    (save-excursion
	      (goto-char pos)
	      (skip-chars-forward " \r\t\n")
	      (point-at-bol)))))
	beg end)
    ;; Determine boundaries of changes.
    (if (org-region-active-p)
	(setq beg (funcall skip-blanks (region-beginning))
	      end (copy-marker (region-end)))
      (setq beg (funcall skip-blanks (point-at-bol))
	    end (copy-marker (point-at-eol))))
    ;; Depending on the starting line, choose an action on the text
    ;; between BEG and END.
    (org-with-limited-levels
     (save-excursion
       (goto-char beg)
       (cond
	;; Case 1. Start at an item: de-itemize.  Note that it only
	;;         happens when a region is active: `org-ctrl-c-minus'
	;;         would call `org-cycle-list-bullet' otherwise.
	((org-at-item-p)
	 (while (< (point) end)
	   (when (org-at-item-p)
	     (skip-chars-forward " \t")
	     (delete-region (point) (match-end 0)))
	   (forward-line)))
	;; Case 2. Start at an heading: convert to items.
	((org-at-heading-p)
	 (let* ((bul (org-list-bullet-string "-"))
		(bul-len (length bul))
		;; Indentation of the first heading.  It should be
		;; relative to the indentation of its parent, if any.
		(start-ind (save-excursion
			     (cond
			      ((not org-adapt-indentation) 0)
			      ((not (outline-previous-heading)) 0)
			      (t (length (match-string 0))))))
		;; Level of first heading. Further headings will be
		;; compared to it to determine hierarchy in the list.
		(ref-level (org-reduced-level (org-outline-level))))
	   (while (< (point) end)
	     (let* ((level (org-reduced-level (org-outline-level)))
		    (delta (max 0 (- level ref-level))))
	       ;; If current headline is less indented than the first
	       ;; one, set it as reference, in order to preserve
	       ;; subtrees.
	       (when (< level ref-level) (setq ref-level level))
	       (replace-match bul t t)
	       (org-indent-line-to (+ start-ind (* delta bul-len)))
	       ;; Ensure all text down to END (or SECTION-END) belongs
	       ;; to the newly created item.
	       (let ((section-end (save-excursion
				    (or (outline-next-heading) (point)))))
		 (forward-line)
		 (funcall shift-text
			  (+ start-ind (* (1+ delta) bul-len))
			  (min end section-end)))))))
	;; Case 3. Normal line with ARG: turn each non-item line into
	;;         an item.
	(arg
	 (while (< (point) end)
	   (unless (or (org-at-heading-p) (org-at-item-p))
	     (if (looking-at "\\([ \t]*\\)\\(\\S-\\)")
		 (replace-match
		  (concat "\\1" (org-list-bullet-string "-") "\\2"))))
	   (forward-line)))
	;; Case 4. Normal line without ARG: make the first line of
	;;         region an item, and shift indentation of others
	;;         lines to set them as item's body.
	(t (let* ((bul (org-list-bullet-string "-"))
		  (bul-len (length bul))
		  (ref-ind (org-get-indentation)))
	     (skip-chars-forward " \t")
	     (insert bul)
	     (forward-line)
	     (while (< (point) end)
	       ;; Ensure that lines less indented than first one
	       ;; still get included in item body.
	       (funcall shift-text
			(+ ref-ind bul-len)
			(min end (save-excursion (or (outline-next-heading)
						     (point)))))
	       (forward-line)))))))))

(defun org-toggle-heading (&optional nstars)
  "Convert headings to normal text, or items or text to headings.
If there is no active region, only the current line is considered.

If the first non blank line is an headline, remove the stars from
all headlines in the region.

If it is a plain list item, turn all plain list items into headings.

If it is a normal line, turn each and every normal line (i.e. not
an heading or an item) in the region into a heading.

When converting a line into a heading, the number of stars is chosen
such that the lines become children of the current entry.  However,
when a prefix argument is given, its value determines the number of
stars to add."
  (interactive "P")
  (let ((skip-blanks
	 (function
	  ;; Return beginning of first non-blank line, starting from
	  ;; line at POS.
	  (lambda (pos)
	    (save-excursion
	      (goto-char pos)
	      (skip-chars-forward " \r\t\n")
	      (point-at-bol)))))
	beg end)
    ;; Determine boundaries of changes. If region ends at a bol, do
    ;; not consider the last line to be in the region.
    (if (org-region-active-p)
	(setq beg (funcall skip-blanks (region-beginning))
	      end (copy-marker (save-excursion
				 (goto-char (region-end))
				 (if (bolp) (point) (point-at-eol)))))
      (setq beg (funcall skip-blanks (point-at-bol))
	    end (copy-marker (point-at-eol))))
    ;; Ensure inline tasks don't count as headings.
    (org-with-limited-levels
     (save-excursion
       (goto-char beg)
       (cond
	;; Case 1. Started at an heading: de-star headings.
	((org-at-heading-p)
	 (while (< (point) end)
	   (when (org-at-heading-p t)
	     (looking-at org-outline-regexp) (replace-match ""))
	   (forward-line)))
	;; Case 2. Started at an item: change items into headlines.
	;;         One star will be added by `org-list-to-subtree'.
	((org-at-item-p)
	 (let* ((stars (make-string
			(if nstars
			    ;; subtract the star that will be added again by
			    ;; `org-list-to-subtree'
			    (1- (prefix-numeric-value current-prefix-arg))
			  (or (org-current-level) 0))
			?*))
		(add-stars
		 (cond (nstars "")               ; stars from prefix only
		       ((equal stars "") "")     ; before first heading
		       (org-odd-levels-only "*") ; inside heading, odd
		       (t ""))))                 ; inside heading, oddeven
	   (while (< (point) end)
	     (when (org-at-item-p)
	       ;; Pay attention to cases when region ends before list.
	       (let* ((struct (org-list-struct))
		      (list-end (min (org-list-get-bottom-point struct) (1+ end))))
		 (save-restriction
		   (narrow-to-region (point) list-end)
		   (insert
		    (org-list-to-subtree
		     (org-list-parse-list t)
		     '(:istart (concat stars add-stars (funcall get-stars depth))
			       :icount (concat stars add-stars (funcall get-stars depth))))))))
	     (forward-line))))
	;; Case 3. Started at normal text: make every line an heading,
	;;         skipping headlines and items.
	(t (let* ((stars (make-string
			  (if nstars
			      (prefix-numeric-value current-prefix-arg)
			    (or (org-current-level) 0))
			  ?*))
		  (add-stars
		   (cond (nstars "")                ; stars from prefix only
			 ((equal stars "") "*")     ; before first heading
			 (org-odd-levels-only "**") ; inside heading, odd
			 (t "*")))                  ; inside heading, oddeven
		  (rpl (concat stars add-stars " ")))
	     (while (< (point) end)
	       (when (and (not (org-at-heading-p)) (not (org-at-item-p))
			  (looking-at "\\([ \t]*\\)\\(\\S-\\)"))
		 (replace-match (concat rpl (match-string 2))))
	       (forward-line)))))))))

(defun org-meta-return (&optional arg)
  "Insert a new heading or wrap a region in a table.
Calls `org-insert-heading' or `org-table-wrap-region', depending on context.
See the individual commands for more information."
  (interactive "P")
  (cond
   ((run-hook-with-args-until-success 'org-metareturn-hook))
   ((org-at-table-p)
    (call-interactively 'org-table-wrap-region))
   (t (call-interactively 'org-insert-heading))))

;;; Menu entries

;; Define the Org-mode menus
(easy-menu-define org-tbl-menu org-mode-map "Tbl menu"
  '("Tbl"
    ["Align" org-ctrl-c-ctrl-c :active (org-at-table-p)]
    ["Next Field" org-cycle (org-at-table-p)]
    ["Previous Field" org-shifttab (org-at-table-p)]
    ["Next Row" org-return (org-at-table-p)]
    "--"
    ["Blank Field" org-table-blank-field (org-at-table-p)]
    ["Edit Field" org-table-edit-field (org-at-table-p)]
    ["Copy Field from Above" org-table-copy-down (org-at-table-p)]
    "--"
    ("Column"
     ["Move Column Left" org-metaleft (org-at-table-p)]
     ["Move Column Right" org-metaright (org-at-table-p)]
     ["Delete Column" org-shiftmetaleft (org-at-table-p)]
     ["Insert Column" org-shiftmetaright (org-at-table-p)])
    ("Row"
     ["Move Row Up" org-metaup (org-at-table-p)]
     ["Move Row Down" org-metadown (org-at-table-p)]
     ["Delete Row" org-shiftmetaup (org-at-table-p)]
     ["Insert Row" org-shiftmetadown (org-at-table-p)]
     ["Sort lines in region" org-table-sort-lines (org-at-table-p)]
     "--"
     ["Insert Hline" org-ctrl-c-minus (org-at-table-p)])
    ("Rectangle"
     ["Copy Rectangle" org-copy-special (org-at-table-p)]
     ["Cut Rectangle" org-cut-special (org-at-table-p)]
     ["Paste Rectangle" org-paste-special (org-at-table-p)]
     ["Fill Rectangle" org-table-wrap-region (org-at-table-p)])
    "--"
    ("Calculate"
     ["Set Column Formula" org-table-eval-formula (org-at-table-p)]
     ["Set Field Formula" (org-table-eval-formula '(4)) :active (org-at-table-p) :keys "C-u C-c ="]
     ["Edit Formulas" org-edit-special (org-at-table-p)]
     "--"
     ["Recalculate line" org-table-recalculate (org-at-table-p)]
     ["Recalculate all" (lambda () (interactive) (org-table-recalculate '(4))) :active (org-at-table-p) :keys "C-u C-c *"]
     ["Iterate all" (lambda () (interactive) (org-table-recalculate '(16))) :active (org-at-table-p) :keys "C-u C-u C-c *"]
     "--"
     ["Toggle Recalculate Mark" org-table-rotate-recalc-marks (org-at-table-p)]
     "--"
     ["Sum Column/Rectangle" org-table-sum
      (or (org-at-table-p) (org-region-active-p))]
     ["Which Column?" org-table-current-column (org-at-table-p)])
    ["Debug Formulas"
     org-table-toggle-formula-debugger
     :style toggle :selected (org-bound-and-true-p org-table-formula-debug)]
    ["Show Col/Row Numbers"
     org-table-toggle-coordinate-overlays
     :style toggle
     :selected (org-bound-and-true-p org-table-overlay-coordinates)]
    "--"
    ["Create" org-table-create (and (not (org-at-table-p))
				    org-enable-table-editor)]
    ["Convert Region" org-table-convert-region (not (org-at-table-p 'any))]
    ["Import from File" org-table-import (not (org-at-table-p))]
    ["Export to File" org-table-export (org-at-table-p)]
    "--"
    ["Create/Convert from/to table.el" org-table-create-with-table.el t]))

(easy-menu-define org-org-menu org-mode-map "Org menu"
  '("Org"
    ("Show/Hide"
     ["Cycle Visibility" org-cycle :active (or (bobp) (outline-on-heading-p))]
     ["Cycle Global Visibility" org-shifttab :active (not (org-at-table-p))]
     ["Sparse Tree..." org-sparse-tree t]
     ["Reveal Context" org-reveal t]
     ["Show All" show-all t]
     "--"
     ["Subtree to indirect buffer" org-tree-to-indirect-buffer t])
    "--"
    ["New Heading" org-insert-heading t]
    ("Navigate Headings"
     ["Up" outline-up-heading t]
     ["Next" outline-next-visible-heading t]
     ["Previous" outline-previous-visible-heading t]
     ["Next Same Level" outline-forward-same-level t]
     ["Previous Same Level" outline-backward-same-level t]
     "--"
     ["Jump" org-goto t])
    ("Edit Structure"
     ["Move Subtree Up" org-shiftmetaup (not (org-at-table-p))]
     ["Move Subtree Down" org-shiftmetadown (not (org-at-table-p))]
     "--"
     ["Copy Subtree"  org-copy-special (not (org-at-table-p))]
     ["Cut Subtree"  org-cut-special (not (org-at-table-p))]
     ["Paste Subtree"  org-paste-special (not (org-at-table-p))]
     "--"
     ["Clone subtree, shift time" org-clone-subtree-with-time-shift t]
     "--"
     ["Copy visible text"  org-copy-visible t]
     "--"
     ["Promote Heading" org-metaleft (not (org-at-table-p))]
     ["Promote Subtree" org-shiftmetaleft (not (org-at-table-p))]
     ["Demote Heading"  org-metaright (not (org-at-table-p))]
     ["Demote Subtree"  org-shiftmetaright (not (org-at-table-p))]
     "--"
     ["Sort Region/Children" org-sort  (not (org-at-table-p))]
     "--"
     ["Convert to odd levels" org-convert-to-odd-levels t]
     ["Convert to odd/even levels" org-convert-to-oddeven-levels t])
    ("Editing"
     ["Emphasis..." org-emphasize t]
     ["Edit Source Example" org-edit-special t]
     "--"
     ["Footnote new/jump" org-footnote-action t]
     ["Footnote extra" (org-footnote-action t) :active t :keys "C-u C-c C-x f"])
    ("Archive"
     ["Archive (default method)" org-archive-subtree-default t]
     "--"
     ["Move Subtree to Archive file" org-advertized-archive-subtree t]
     ["Toggle ARCHIVE tag" org-toggle-archive-tag t]
     ["Move subtree to Archive sibling" org-archive-to-archive-sibling t]
     )
    "--"
    ("Hyperlinks"
     ["Store Link (Global)" org-store-link t]
     ["Find existing link to here" org-occur-link-in-agenda-files t]
     ["Insert Link" org-insert-link t]
     ["Follow Link" org-open-at-point t]
     "--"
     ["Next link" org-next-link t]
     ["Previous link" org-previous-link t]
     "--"
     ["Descriptive Links"
      org-toggle-link-display
      :style radio
      :selected org-descriptive-links
      ]
     ["Literal Links"
      org-toggle-link-display
      :style radio
      :selected (not org-descriptive-links)])
    "--"
    ("TODO Lists"
     ["TODO/DONE/-" org-todo t]
     ("Select keyword"
      ["Next keyword" org-shiftright (org-at-heading-p)]
      ["Previous keyword" org-shiftleft (org-at-heading-p)]
      ["Complete Keyword" pcomplete (assq :todo-keyword (org-context))]
      ["Next keyword set" org-shiftcontrolright (and (> (length org-todo-sets) 1) (org-at-heading-p))]
      ["Previous keyword set" org-shiftcontrolright (and (> (length org-todo-sets) 1) (org-at-heading-p))])
     ["Show TODO Tree" org-show-todo-tree :active t :keys "C-c / t"]
     ["Global TODO list" org-todo-list :active t :keys "C-c a t"]
     "--"
     ["Enforce dependencies" (customize-variable 'org-enforce-todo-dependencies)
      :selected org-enforce-todo-dependencies :style toggle :active t]
     "Settings for tree at point"
     ["Do Children sequentially" org-toggle-ordered-property :style radio
      :selected (org-entry-get nil "ORDERED")
      :active org-enforce-todo-dependencies :keys "C-c C-x o"]
     ["Do Children parallel" org-toggle-ordered-property :style radio
      :selected (not (org-entry-get nil "ORDERED"))
      :active org-enforce-todo-dependencies :keys "C-c C-x o"]
     "--"
     ["Set Priority" org-priority t]
     ["Priority Up" org-shiftup t]
     ["Priority Down" org-shiftdown t]
     "--"
     ["Get news from all feeds" org-feed-update-all t]
     ["Go to the inbox of a feed..." org-feed-goto-inbox t]
     ["Customize feeds" (customize-variable 'org-feed-alist) t])
    ("TAGS and Properties"
     ["Set Tags" org-set-tags-command t]
     ["Change tag in region" org-change-tag-in-region (org-region-active-p)]
     "--"
     ["Set property" org-set-property t]
     ["Column view of properties" org-columns t]
     ["Insert Column View DBlock" org-insert-columns-dblock t])
    ("Dates and Scheduling"
     ["Timestamp" org-time-stamp t]
     ["Timestamp (inactive)" org-time-stamp-inactive t]
     ("Change Date"
      ["1 Day Later" org-shiftright t]
      ["1 Day Earlier" org-shiftleft t]
      ["1 ... Later" org-shiftup t]
      ["1 ... Earlier" org-shiftdown t])
     ["Compute Time Range" org-evaluate-time-range t]
     ["Schedule Item" org-schedule t]
     ["Deadline" org-deadline t]
     "--"
     ["Custom time format" org-toggle-time-stamp-overlays
      :style radio :selected org-display-custom-times]
     "--"
     ["Goto Calendar" org-goto-calendar t]
     ["Date from Calendar" org-date-from-calendar t]
     "--"
     ["Start/Restart Timer" org-timer-start t]
     ["Pause/Continue Timer" org-timer-pause-or-continue t]
     ["Stop Timer" org-timer-pause-or-continue :active t :keys "C-u C-c C-x ,"]
     ["Insert Timer String" org-timer t]
     ["Insert Timer Item" org-timer-item t])
    ("Logging work"
     ["Clock in" org-clock-in :active t :keys "C-c C-x C-i"]
     ["Switch task" (lambda () (interactive) (org-clock-in '(4))) :active t :keys "C-u C-c C-x C-i"]
     ["Clock out" org-clock-out t]
     ["Clock cancel" org-clock-cancel t]
     "--"
     ["Mark as default task" org-clock-mark-default-task t]
     ["Clock in, mark as default" (lambda () (interactive) (org-clock-in '(16))) :active t :keys "C-u C-u C-c C-x C-i"]
     ["Goto running clock" org-clock-goto t]
     "--"
     ["Display times" org-clock-display t]
     ["Create clock table" org-clock-report t]
     "--"
     ["Record DONE time"
      (progn (setq org-log-done (not org-log-done))
	     (message "Switching to %s will %s record a timestamp"
		      (car org-done-keywords)
		      (if org-log-done "automatically" "not")))
      :style toggle :selected org-log-done])
    "--"
    ["Agenda Command..." org-agenda t]
    ["Set Restriction Lock" org-agenda-set-restriction-lock t]
    ("File List for Agenda")
    ("Special views current file"
     ["TODO Tree"  org-show-todo-tree t]
     ["Check Deadlines" org-check-deadlines t]
     ["Timeline" org-timeline t]
     ["Tags/Property tree" org-match-sparse-tree t])
    "--"
    ["Export/Publish..." org-export t]
    ("LaTeX"
     ["Org CDLaTeX mode" org-cdlatex-mode :style toggle
      :selected org-cdlatex-mode]
     ["Insert Environment" cdlatex-environment (fboundp 'cdlatex-environment)]
     ["Insert math symbol" cdlatex-math-symbol (fboundp 'cdlatex-math-symbol)]
     ["Modify math symbol" org-cdlatex-math-modify
      (org-inside-LaTeX-fragment-p)]
     ["Insert citation" org-reftex-citation t]
     "--"
     ["Template for BEAMER" (progn (require 'org-beamer)
				   (org-insert-beamer-options-template)) t])
    "--"
    ("MobileOrg"
     ["Push Files and Views" org-mobile-push t]
     ["Get Captured and Flagged" org-mobile-pull t]
     ["Find FLAGGED Tasks" (org-agenda nil "?") :active t :keys "C-c a ?"]
     "--"
     ["Setup" (progn (require 'org-mobile) (customize-group 'org-mobile)) t])
    "--"
    ("Documentation"
     ["Show Version" org-version t]
     ["Info Documentation" org-info t])
    ("Customize"
     ["Browse Org Group" org-customize t]
     "--"
     ["Expand This Menu" org-create-customize-menu
      (fboundp 'customize-menu-create)])
    ["Send bug report" org-submit-bug-report t]
    "--"
    ("Refresh/Reload"
     ["Refresh setup current buffer" org-mode-restart t]
     ["Reload Org (after update)" org-reload t]
     ["Reload Org uncompiled" (org-reload t) :active t :keys "C-u C-c C-x r"])
    ))

(defun org-info (&optional node)
  "Read documentation for Org-mode in the info system.
With optional NODE, go directly to that node."
  (interactive)
  (info (format "(org)%s" (or node ""))))

;;;###autoload
(defun org-submit-bug-report ()
  "Submit a bug report on Org-mode via mail.

Don't hesitate to report any problems or inaccurate documentation.

If you don't have setup sending mail from (X)Emacs, please copy the
output buffer into your mail program, as it gives us important
information about your Org-mode version and configuration."
  (interactive)
  (require 'reporter)
  (org-load-modules-maybe)
  (org-require-autoloaded-modules)
  (let ((reporter-prompt-for-summary-p "Bug report subject: "))
    (reporter-submit-bug-report
     "emacs-orgmode@gnu.org"
     (org-version)
     (let (list)
       (save-window-excursion
	 (org-pop-to-buffer-same-window (get-buffer-create "*Warn about privacy*"))
	 (delete-other-windows)
	 (erase-buffer)
	 (insert "You are about to submit a bug report to the Org-mode mailing list.

We would like to add your full Org-mode and Outline configuration to the
bug report.  This greatly simplifies the work of the maintainer and
other experts on the mailing list.

HOWEVER, some variables you have customized may contain private
information.  The names of customers, colleagues, or friends, might
appear in the form of file names, tags, todo states, or search strings.
If you answer yes to the prompt, you might want to check and remove
such private information before sending the email.")
	 (add-text-properties (point-min) (point-max) '(face org-warning))
	 (when (yes-or-no-p "Include your Org-mode configuration ")
	   (mapatoms
	    (lambda (v)
	      (and (boundp v)
		   (string-match "\\`\\(org-\\|outline-\\)" (symbol-name v))
		   (or (and (symbol-value v)
			    (string-match "\\(-hook\\|-function\\)\\'" (symbol-name v)))
		       (and
			(get v 'custom-type) (get v 'standard-value)
			(not (equal (symbol-value v) (eval (car (get v 'standard-value)))))))
		   (push v list)))))
	 (kill-buffer (get-buffer "*Warn about privacy*"))
	 list))
     nil nil
     "Remember to cover the basics, that is, what you expected to happen and
what in fact did happen.  You don't know how to make a good report?  See

     http://orgmode.org/manual/Feedback.html#Feedback

Your bug report will be posted to the Org-mode mailing list.
------------------------------------------------------------------------")
    (save-excursion
      (if (re-search-backward "^\\(Subject: \\)Org-mode version \\(.*?\\);[ \t]*\\(.*\\)" nil t)
	  (replace-match "\\1Bug: \\3 [\\2]")))))


(defun org-install-agenda-files-menu ()
  (let ((bl (buffer-list)))
    (save-excursion
      (while bl
	(set-buffer (pop bl))
	(if (eq major-mode 'org-mode) (setq bl nil)))
      (when (eq major-mode 'org-mode)
	(easy-menu-change
	 '("Org") "File List for Agenda"
	 (append
	  (list
	   ["Edit File List" (org-edit-agenda-file-list) t]
	   ["Add/Move Current File to Front of List" org-agenda-file-to-front t]
	   ["Remove Current File from List" org-remove-file t]
	   ["Cycle through agenda files" org-cycle-agenda-files t]
	   ["Occur in all agenda files" org-occur-in-agenda-files t]
	   "--")
	  (mapcar 'org-file-menu-entry (org-agenda-files t))))))))

;;;; Documentation

;;;###autoload
(defun org-require-autoloaded-modules ()
  (interactive)
  (mapc 'require
	'(org-agenda org-archive org-ascii org-attach org-clock org-colview
		     org-docbook org-exp org-html org-icalendar
		     org-id org-latex
		     org-publish org-remember org-table
		     org-timer org-xoxo)))

;;;###autoload
(defun org-reload (&optional uncompiled)
  "Reload all org lisp files.
With prefix arg UNCOMPILED, load the uncompiled versions."
  (interactive "P")
  (require 'find-func)
  (let* ((file-re "^\\(org\\|orgtbl\\)\\(\\.el\\|-.*\\.el\\)")
	 (dir-org (file-name-directory (org-find-library-name "org")))
	 (dir-org-contrib (ignore-errors
			   (file-name-directory
			    (org-find-library-name "org-contribdir"))))
	 (babel-files
	  (mapcar (lambda (el) (concat "ob" (when el (format "-%s" el)) ".el"))
		  (append (list nil "comint" "eval" "exp" "keys"
				    "lob" "ref" "table" "tangle")
			  (delq nil
				(mapcar
				 (lambda (lang)
				   (when (cdr lang) (symbol-name (car lang))))
				 org-babel-load-languages)))))
	 (files
	  (append (directory-files dir-org t file-re)
		  babel-files
		  (and dir-org-contrib
		       (directory-files dir-org-contrib t file-re))))
	 (remove-re (concat (if (featurep 'xemacs)
				"org-colview" "org-colview-xemacs")
			    "\\'")))
    (setq files (mapcar 'file-name-sans-extension files))
    (setq files (mapcar
		 (lambda (x) (if (string-match remove-re x) nil x))
		 files))
    (setq files (delq nil files))
    (mapc
     (lambda (f)
       (when (featurep (intern (file-name-nondirectory f)))
	 (if (and (not uncompiled)
		  (file-exists-p (concat f ".elc")))
	     (load (concat f ".elc") nil nil t)
	   (load (concat f ".el") nil nil t))))
     files))
  (org-version))

;;;###autoload
(defun org-customize ()
  "Call the customize function with org as argument."
  (interactive)
  (org-load-modules-maybe)
  (org-require-autoloaded-modules)
  (customize-browse 'org))

(defun org-create-customize-menu ()
  "Create a full customization menu for Org-mode, insert it into the menu."
  (interactive)
  (org-load-modules-maybe)
  (org-require-autoloaded-modules)
  (if (fboundp 'customize-menu-create)
      (progn
	(easy-menu-change
	 '("Org") "Customize"
	 `(["Browse Org group" org-customize t]
	   "--"
	   ,(customize-menu-create 'org)
	   ["Set" Custom-set t]
	   ["Save" Custom-save t]
	   ["Reset to Current" Custom-reset-current t]
	   ["Reset to Saved" Custom-reset-saved t]
	   ["Reset to Standard Settings" Custom-reset-standard t]))
	(message "\"Org\"-menu now contains full customization menu"))
    (error "Cannot expand menu (outdated version of cus-edit.el)")))

;;;; Miscellaneous stuff

;;; Generally useful functions

(defun org-get-at-bol (property)
  "Get text property PROPERTY at beginning of line."
  (get-text-property (point-at-bol) property))

(defun org-find-text-property-in-string (prop s)
  "Return the first non-nil value of property PROP in string S."
  (or (get-text-property 0 prop s)
      (get-text-property (or (next-single-property-change 0 prop s) 0)
			 prop s)))

(defun org-display-warning (message) ;; Copied from Emacs-Muse
  "Display the given MESSAGE as a warning."
  (if (fboundp 'display-warning)
      (display-warning 'org message
                       (if (featurep 'xemacs) 'warning :warning))
    (let ((buf (get-buffer-create "*Org warnings*")))
      (with-current-buffer buf
        (goto-char (point-max))
        (insert "Warning (Org): " message)
        (unless (bolp)
          (newline)))
      (display-buffer buf)
      (sit-for 0))))

(defun org-eval (form)
  "Eval FORM and return result."
  (condition-case error
      (eval form)
    (error (format "%%![Error: %s]" error))))

(defun org-in-clocktable-p ()
  "Check if the cursor is in a clocktable."
  (let ((pos (point)) start)
    (save-excursion
      (end-of-line 1)
      (and (re-search-backward "^[ \t]*#\\+BEGIN:[ \t]+clocktable" nil t)
	   (setq start (match-beginning 0))
	   (re-search-forward "^[ \t]*#\\+END:.*" nil t)
	   (>= (match-end 0) pos)
	   start))))

(defun org-in-commented-line ()
  "Is point in a line starting with `#'?"
  (equal (char-after (point-at-bol)) ?#))

(defun org-in-indented-comment-line ()
  "Is point in a line starting with `#' after some white space?"
  (save-excursion
    (save-match-data
      (goto-char (point-at-bol))
      (looking-at "[ \t]*#"))))

(defun org-in-verbatim-emphasis ()
  (save-match-data
    (and (org-in-regexp org-emph-re 2) (member (match-string 3) '("=" "~")))))

(defun org-goto-marker-or-bmk (marker &optional bookmark)
  "Go to MARKER, widen if necessary.  When marker is not live, try BOOKMARK."
  (if (and marker (marker-buffer marker)
	   (buffer-live-p (marker-buffer marker)))
      (progn
	(org-pop-to-buffer-same-window (marker-buffer marker))
	(if (or (> marker (point-max)) (< marker (point-min)))
	    (widen))
	(goto-char marker)
	(org-show-context 'org-goto))
    (if bookmark
	(bookmark-jump bookmark)
      (error "Cannot find location"))))

(defun org-quote-csv-field (s)
  "Quote field for inclusion in CSV material."
  (if (string-match "[\",]" s)
      (concat "\"" (mapconcat 'identity (split-string s "\"") "\"\"") "\"")
    s))

(defun org-force-self-insert (N)
  "Needed to enforce self-insert under remapping."
  (interactive "p")
  (self-insert-command N))

(defun org-string-width (s)
  "Compute width of string, ignoring invisible characters.
This ignores character with invisibility property `org-link', and also
characters with property `org-cwidth', because these will become invisible
upon the next fontification round."
  (let (b l)
    (when (or (eq t buffer-invisibility-spec)
	      (assq 'org-link buffer-invisibility-spec))
      (while (setq b (text-property-any 0 (length s)
					'invisible 'org-link s))
	(setq s (concat (substring s 0 b)
			(substring s (or (next-single-property-change
					  b 'invisible s) (length s)))))))
    (while (setq b (text-property-any 0 (length s) 'org-cwidth t s))
      (setq s (concat (substring s 0 b)
		      (substring s (or (next-single-property-change
					b 'org-cwidth s) (length s))))))
    (setq l (string-width s) b -1)
    (while (setq b (text-property-any (1+ b) (length s) 'org-dwidth t s))
      (setq l (- l (get-text-property b 'org-dwidth-n s))))
    l))

(defun org-shorten-string (s maxlength)
  "Shorten string S so tht it is no longer than MAXLENGTH characters.
If the string is shorter or has length MAXLENGTH, just return the
original string.  If it is longer, the functions finds a space in the
string, breaks this string off at that locations and adds three dots
as ellipsis.  Including the ellipsis, the string will not be longer
than MAXLENGTH.  If finding a good breaking point in the string does
not work, the string is just chopped off in the middle of a word
if necessary."
  (if (<= (length s) maxlength)
      s
    (let* ((n (max (- maxlength 4) 1))
	   (re (concat "\\`\\(.\\{1," (int-to-string n) "\\}[^ ]\\)\\([ ]\\|\\'\\)")))
      (if (string-match re s)
	  (concat (match-string 1 s) "...")
	(concat (substring s 0 (max (- maxlength 3) 0)) "...")))))

(defun org-get-indentation (&optional line)
  "Get the indentation of the current line, interpreting tabs.
When LINE is given, assume it represents a line and compute its indentation."
  (if line
      (if (string-match "^ *" (org-remove-tabs line))
	  (match-end 0))
    (save-excursion
      (beginning-of-line 1)
      (skip-chars-forward " \t")
      (current-column))))

(defun org-get-string-indentation (s)
  "What indentation has S due to SPACE and TAB at the beginning of the string?"
  (let ((n -1) (i 0) (w tab-width) c)
    (catch 'exit
      (while (< (setq n (1+ n)) (length s))
	(setq c (aref s n))
	(cond ((= c ?\ ) (setq i (1+ i)))
	      ((= c ?\t) (setq i (* (/ (+ w i) w) w)))
	      (t (throw 'exit t)))))
    i))

(defun org-remove-tabs (s &optional width)
  "Replace tabulators in S with spaces.
Assumes that s is a single line, starting in column 0."
  (setq width (or width tab-width))
  (while (string-match "\t" s)
    (setq s (replace-match
	     (make-string
	      (- (* width (/ (+ (match-beginning 0) width) width))
		 (match-beginning 0)) ?\ )
	     t t s)))
  s)

(defun org-fix-indentation (line ind)
  "Fix indentation in LINE.
IND is a cons cell with target and minimum indentation.
If the current indentation in LINE is smaller than the minimum,
leave it alone.  If it is larger than ind, set it to the target."
  (let* ((l (org-remove-tabs line))
	 (i (org-get-indentation l))
	 (i1 (car ind)) (i2 (cdr ind)))
    (if (>= i i2) (setq l (substring line i2)))
    (if (> i1 0)
	(concat (make-string i1 ?\ ) l)
      l)))

(defun org-remove-indentation (code &optional n)
  "Remove the maximum common indentation from the lines in CODE.
N may optionally be the number of spaces to remove."
  (with-temp-buffer
    (insert code)
    (org-do-remove-indentation n)
    (buffer-string)))

(defun org-do-remove-indentation (&optional n)
  "Remove the maximum common indentation from the buffer."
  (untabify (point-min) (point-max))
  (let ((min 10000) re)
    (if n
	(setq min n)
      (goto-char (point-min))
      (while (re-search-forward "^ *[^ \n]" nil t)
	(setq min (min min (1- (- (match-end 0) (match-beginning 0)))))))
    (unless (or (= min 0) (= min 10000))
      (setq re (format "^ \\{%d\\}" min))
      (goto-char (point-min))
      (while (re-search-forward re nil t)
	(replace-match "")
	(end-of-line 1))
      min)))

(defun org-fill-template (template alist)
  "Find each %key of ALIST in TEMPLATE and replace it."
  (let ((case-fold-search nil)
	entry key value)
    (setq alist (sort (copy-sequence alist)
		      (lambda (a b) (< (length (car a)) (length (car b))))))
    (while (setq entry (pop alist))
      (setq template
	    (replace-regexp-in-string
	     (concat "%" (regexp-quote (car entry)))
	     (cdr entry) template t t)))
    template))

(defun org-base-buffer (buffer)
  "Return the base buffer of BUFFER, if it has one.  Else return the buffer."
  (if (not buffer)
      buffer
    (or (buffer-base-buffer buffer)
	buffer)))

(defun org-trim (s)
  "Remove whitespace at beginning and end of string."
  (if (string-match "\\`[ \t\n\r]+" s) (setq s (replace-match "" t t s)))
  (if (string-match "[ \t\n\r]+\\'" s) (setq s (replace-match "" t t s)))
  s)

(defun org-wrap (string &optional width lines)
  "Wrap string to either a number of lines, or a width in characters.
If WIDTH is non-nil, the string is wrapped to that width, however many lines
that costs.  If there is a word longer than WIDTH, the text is actually
wrapped to the length of that word.
IF WIDTH is nil and LINES is non-nil, the string is forced into at most that
many lines, whatever width that takes.
The return value is a list of lines, without newlines at the end."
  (let* ((words (org-split-string string "[ \t\n]+"))
	 (maxword (apply 'max (mapcar 'org-string-width words)))
	 w ll)
    (cond (width
	   (org-do-wrap words (max maxword width)))
	  (lines
	   (setq w maxword)
	   (setq ll (org-do-wrap words maxword))
	   (if (<= (length ll) lines)
	       ll
	     (setq ll words)
	     (while (> (length ll) lines)
	       (setq w (1+ w))
	       (setq ll (org-do-wrap words w)))
	     ll))
	  (t (error "Cannot wrap this")))))

(defun org-do-wrap (words width)
  "Create lines of maximum width WIDTH (in characters) from word list WORDS."
  (let (lines line)
    (while words
      (setq line (pop words))
      (while (and words (< (+ (length line) (length (car words))) width))
	(setq line (concat line " " (pop words))))
      (setq lines (push line lines)))
    (nreverse lines)))

(defun org-split-string (string &optional separators)
  "Splits STRING into substrings at SEPARATORS.
No empty strings are returned if there are matches at the beginning
and end of string."
  (let ((rexp (or separators "[ \f\t\n\r\v]+"))
	(start 0)
	notfirst
	(list nil))
    (while (and (string-match rexp string
			      (if (and notfirst
				       (= start (match-beginning 0))
				       (< start (length string)))
				  (1+ start) start))
		(< (match-beginning 0) (length string)))
      (setq notfirst t)
      (or (eq (match-beginning 0) 0)
	  (and (eq (match-beginning 0) (match-end 0))
	       (eq (match-beginning 0) start))
	  (setq list
		(cons (substring string start (match-beginning 0))
		      list)))
      (setq start (match-end 0)))
    (or (eq start (length string))
	(setq list
	      (cons (substring string start)
		    list)))
    (nreverse list)))

(defun org-quote-vert (s)
  "Replace \"|\" with \"\\vert\"."
  (while (string-match "|" s)
    (setq s (replace-match "\\vert" t t s)))
  s)

(defun org-uuidgen-p (s)
  "Is S an ID created by UUIDGEN?"
  (string-match "\\`[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}\\'" (downcase s)))

(defun org-context ()
  "Return a list of contexts of the current cursor position.
If several contexts apply, all are returned.
Each context entry is a list with a symbol naming the context, and
two positions indicating start and end of the context.  Possible
contexts are:

:headline         anywhere in a headline
:headline-stars   on the leading stars in a headline
:todo-keyword     on a TODO keyword (including DONE) in a headline
:tags             on the TAGS in a headline
:priority         on the priority cookie in a headline
:item             on the first line of a plain list item
:item-bullet      on the bullet/number of a plain list item
:checkbox         on the checkbox in a plain list item
:table            in an org-mode table
:table-special    on a special filed in a table
:table-table      in a table.el table
:link             on a hyperlink
:keyword          on a keyword: SCHEDULED, DEADLINE, CLOSE,COMMENT, QUOTE.
:target           on a <<target>>
:radio-target     on a <<<radio-target>>>
:latex-fragment   on a LaTeX fragment
:latex-preview    on a LaTeX fragment with overlaid preview image

This function expects the position to be visible because it uses font-lock
faces as a help to recognize the following contexts: :table-special, :link,
and :keyword."
  (let* ((f (get-text-property (point) 'face))
	 (faces (if (listp f) f (list f)))
	 (p (point)) clist o)
    ;; First the large context
    (cond
     ((org-at-heading-p t)
      (push (list :headline (point-at-bol) (point-at-eol)) clist)
      (when (progn
	      (beginning-of-line 1)
	      (looking-at org-todo-line-tags-regexp))
	(push (org-point-in-group p 1 :headline-stars) clist)
	(push (org-point-in-group p 2 :todo-keyword) clist)
	(push (org-point-in-group p 4 :tags) clist))
      (goto-char p)
      (skip-chars-backward "^[\n\r \t") (or (bobp) (backward-char 1))
      (if (looking-at "\\[#[A-Z0-9]\\]")
	  (push (org-point-in-group p 0 :priority) clist)))

     ((org-at-item-p)
      (push (org-point-in-group p 2 :item-bullet) clist)
      (push (list :item (point-at-bol)
		  (save-excursion (org-end-of-item) (point)))
	    clist)
      (and (org-at-item-checkbox-p)
	   (push (org-point-in-group p 0 :checkbox) clist)))

     ((org-at-table-p)
      (push (list :table (org-table-begin) (org-table-end)) clist)
      (if (memq 'org-formula faces)
	  (push (list :table-special
		      (previous-single-property-change p 'face)
		      (next-single-property-change p 'face)) clist)))
     ((org-at-table-p 'any)
      (push (list :table-table) clist)))
    (goto-char p)

    ;; Now the small context
    (cond
     ((org-at-timestamp-p)
      (push (org-point-in-group p 0 :timestamp) clist))
     ((memq 'org-link faces)
      (push (list :link
		  (previous-single-property-change p 'face)
		  (next-single-property-change p 'face)) clist))
     ((memq 'org-special-keyword faces)
      (push (list :keyword
		  (previous-single-property-change p 'face)
		  (next-single-property-change p 'face)) clist))
     ((org-at-target-p)
      (push (org-point-in-group p 0 :target) clist)
      (goto-char (1- (match-beginning 0)))
      (if (looking-at org-radio-target-regexp)
	  (push (org-point-in-group p 0 :radio-target) clist))
      (goto-char p))
     ((setq o (car (delq nil
			 (mapcar
			  (lambda (x)
			    (if (memq x org-latex-fragment-image-overlays) x))
			  (overlays-at (point))))))
      (push (list :latex-fragment
		  (overlay-start o) (overlay-end o)) clist)
      (push (list :latex-preview
		  (overlay-start o) (overlay-end o)) clist))
     ((org-inside-LaTeX-fragment-p)
      ;; FIXME: positions wrong.
      (push (list :latex-fragment (point) (point)) clist)))

    (setq clist (nreverse (delq nil clist)))
    clist))

;; FIXME: Compare with at-regexp-p Do we need both?
(defun org-in-regexp (re &optional nlines visually)
  "Check if point is inside a match of regexp.
Normally only the current line is checked, but you can include NLINES extra
lines both before and after point into the search.
If VISUALLY is set, require that the cursor is not after the match but
really on, so that the block visually is on the match."
  (catch 'exit
    (let ((pos (point))
          (eol (point-at-eol (+ 1 (or nlines 0))))
	  (inc (if visually 1 0)))
      (save-excursion
	(beginning-of-line (- 1 (or nlines 0)))
	(while (re-search-forward re eol t)
	  (if (and (<= (match-beginning 0) pos)
		   (>= (+ inc (match-end 0)) pos))
	      (throw 'exit (cons (match-beginning 0) (match-end 0)))))))))

(defun org-at-regexp-p (regexp)
  "Is point inside a match of REGEXP in the current line?"
  (catch 'exit
    (save-excursion
      (let ((pos (point)) (end (point-at-eol)))
	(beginning-of-line 1)
	(while (re-search-forward regexp end t)
	  (if (and (<= (match-beginning 0) pos)
		   (>= (match-end 0) pos))
	      (throw 'exit t)))
	nil))))

(defun org-between-regexps-p (start-re end-re &optional lim-up lim-down)
  "Non-nil when point is between matches of START-RE and END-RE.

Also return a non-nil value when point is on one of the matches.

Optional arguments LIM-UP and LIM-DOWN bound the search; they are
buffer positions.  Default values are the positions of headlines
surrounding the point.

The functions returns a cons cell whose car (resp. cdr) is the
position before START-RE (resp. after END-RE)."
  (save-match-data
    (let ((pos (point))
	  (limit-up (or lim-up (save-excursion (outline-previous-heading))))
	  (limit-down (or lim-down (save-excursion (outline-next-heading))))
	  beg end)
      (save-excursion
	;; Point is on a block when on START-RE or if START-RE can be
	;; found before it...
	(and (or (org-at-regexp-p start-re)
		 (re-search-backward start-re limit-up t))
	     (setq beg (match-beginning 0))
	     ;; ... and END-RE after it...
	     (goto-char (match-end 0))
	     (re-search-forward end-re limit-down t)
	     (> (setq end (match-end 0)) pos)
	     ;; ... without another START-RE in-between.
	     (goto-char (match-beginning 0))
	     (not (re-search-backward start-re (1+ beg) t))
	     ;; Return value.
	     (cons beg end))))))

(defun org-in-block-p (names)
  "Non-nil when point belongs to a block whose name belongs to NAMES.

NAMES is a list of strings containing names of blocks.

Return first block name matched, or nil.  Beware that in case of
nested blocks, the returned name may not belong to the closest
block from point."
  (save-match-data
    (catch 'exit
      (let ((case-fold-search t)
	    (lim-up (save-excursion (outline-previous-heading)))
	    (lim-down (save-excursion (outline-next-heading))))
	(mapc (lambda (name)
		(let ((n (regexp-quote name)))
		  (when (org-between-regexps-p
			 (concat "^[ \t]*#\\+begin_" n)
			 (concat "^[ \t]*#\\+end_" n)
			 lim-up lim-down)
		    (throw 'exit n))))
	      names))
      nil)))

(defun org-occur-in-agenda-files (regexp &optional nlines)
  "Call `multi-occur' with buffers for all agenda files."
  (interactive "sOrg-files matching: \np")
  (let* ((files (org-agenda-files))
	 (tnames (mapcar 'file-truename files))
	 (extra org-agenda-text-search-extra-files)
	 f)
    (when (eq (car extra) 'agenda-archives)
      (setq extra (cdr extra))
      (setq files (org-add-archive-files files)))
    (while (setq f (pop extra))
      (unless (member (file-truename f) tnames)
	(add-to-list 'files f 'append)
	(add-to-list 'tnames (file-truename f) 'append)))
    (multi-occur
     (mapcar (lambda (x)
	       (with-current-buffer
		   (or (get-file-buffer x) (find-file-noselect x))
		 (widen)
		 (current-buffer)))
	     files)
     regexp)))

(if (boundp 'occur-mode-find-occurrence-hook)
    ;; Emacs 23
    (add-hook 'occur-mode-find-occurrence-hook
	      (lambda ()
		(when (eq major-mode 'org-mode)
		  (org-reveal))))
  ;; Emacs 22
  (defadvice occur-mode-goto-occurrence
    (after org-occur-reveal activate)
    (and (eq major-mode 'org-mode) (org-reveal)))
  (defadvice occur-mode-goto-occurrence-other-window
    (after org-occur-reveal activate)
    (and (eq major-mode 'org-mode) (org-reveal)))
  (defadvice occur-mode-display-occurrence
    (after org-occur-reveal activate)
    (when (eq major-mode 'org-mode)
      (let ((pos (occur-mode-find-occurrence)))
	(with-current-buffer (marker-buffer pos)
	  (save-excursion
	    (goto-char pos)
	    (org-reveal)))))))

(defun org-occur-link-in-agenda-files ()
  "Create a link and search for it in the agendas.
The link is not stored in `org-stored-links', it is just created
for the search purpose."
  (interactive)
  (let ((link (condition-case nil
		  (org-store-link nil)
		(error "Unable to create a link to here"))))
    (org-occur-in-agenda-files (regexp-quote link))))

(defun org-uniquify (list)
  "Remove duplicate elements from LIST."
  (let (res)
    (mapc (lambda (x) (add-to-list 'res x 'append)) list)
    res))

(defun org-delete-all (elts list)
  "Remove all elements in ELTS from LIST."
  (while elts
    (setq list (delete (pop elts) list)))
  list)

(defun org-count (cl-item cl-seq)
  "Count the number of occurrences of ITEM in SEQ.
Taken from `count' in cl-seq.el with all keyword arguments removed."
  (let ((cl-end (length cl-seq)) (cl-start 0) (cl-count 0)  cl-x)
    (when (consp cl-seq) (setq cl-seq (nthcdr cl-start cl-seq)))
    (while (< cl-start cl-end)
      (setq cl-x (if (consp cl-seq) (pop cl-seq) (aref cl-seq cl-start)))
      (if (equal cl-item cl-x) (setq cl-count (1+ cl-count)))
      (setq cl-start (1+ cl-start)))
    cl-count))

(defun org-remove-if (predicate seq)
  "Remove everything from SEQ that fulfills PREDICATE."
  (let (res e)
    (while seq
      (setq e (pop seq))
      (if (not (funcall predicate e)) (push e res)))
    (nreverse res)))

(defun org-remove-if-not (predicate seq)
  "Remove everything from SEQ that does not fulfill PREDICATE."
  (let (res e)
    (while seq
      (setq e (pop seq))
      (if (funcall predicate e) (push e res)))
    (nreverse res)))

(defun org-reduce (cl-func cl-seq &rest cl-keys)
  "Reduce two-argument FUNCTION across SEQ.
Taken from `reduce' in cl-seq.el with all keyword arguments but
\":initial-value\" removed."
  (let ((cl-accum (cond ((memq :initial-value cl-keys)
                         (cadr (memq :initial-value cl-keys)))
                        (cl-seq (pop cl-seq))
                        (t (funcall cl-func)))))
    (while cl-seq
      (setq cl-accum (funcall cl-func cl-accum (pop cl-seq))))
    cl-accum))

(defun org-back-over-empty-lines ()
  "Move backwards over whitespace, to the beginning of the first empty line.
Returns the number of empty lines passed."
  (let ((pos (point)))
    (if (cdr (assoc 'heading org-blank-before-new-entry))
       (skip-chars-backward " \t\n\r")
      (unless (eobp)
	(forward-line -1)))
    (beginning-of-line 2)
    (goto-char (min (point) pos))
    (count-lines (point) pos)))

(defun org-skip-whitespace ()
  (skip-chars-forward " \t\n\r"))

(defun org-point-in-group (point group &optional context)
  "Check if POINT is in match-group GROUP.
If CONTEXT is non-nil, return a list with CONTEXT and the boundaries of the
match.  If the match group does not exist or point is not inside it,
return nil."
  (and (match-beginning group)
       (>= point (match-beginning group))
       (<= point (match-end group))
       (if context
	   (list context (match-beginning group) (match-end group))
	 t)))

(defun org-switch-to-buffer-other-window (&rest args)
  "Switch to buffer in a second window on the current frame.
In particular, do not allow pop-up frames.
Returns the newly created buffer."
  (let (pop-up-frames special-display-buffer-names special-display-regexps
		      special-display-function)
    (apply 'switch-to-buffer-other-window args)))

(defun org-combine-plists (&rest plists)
  "Create a single property list from all plists in PLISTS.
The process starts by copying the first list, and then setting properties
from the other lists.  Settings in the last list are the most significant
ones and overrule settings in the other lists."
  (let ((rtn (copy-sequence (pop plists)))
	p v ls)
    (while plists
      (setq ls (pop plists))
      (while ls
	(setq p (pop ls) v (pop ls))
	(setq rtn (plist-put rtn p v))))
    rtn))

(defun org-move-line-down (arg)
  "Move the current line down.  With prefix argument, move it past ARG lines."
  (interactive "p")
  (let ((col (current-column))
	beg end pos)
    (beginning-of-line 1) (setq beg (point))
    (beginning-of-line 2) (setq end (point))
    (beginning-of-line (+ 1 arg))
    (setq pos (move-marker (make-marker) (point)))
    (insert (delete-and-extract-region beg end))
    (goto-char pos)
    (org-move-to-column col)))

(defun org-move-line-up (arg)
  "Move the current line up.  With prefix argument, move it past ARG lines."
  (interactive "p")
  (let ((col (current-column))
	beg end pos)
    (beginning-of-line 1) (setq beg (point))
    (beginning-of-line 2) (setq end (point))
    (beginning-of-line (- arg))
    (setq pos (move-marker (make-marker) (point)))
    (insert (delete-and-extract-region beg end))
    (goto-char pos)
    (org-move-to-column col)))

(defun org-replace-escapes (string table)
  "Replace %-escapes in STRING with values in TABLE.
TABLE is an association list with keys like \"%a\" and string values.
The sequences in STRING may contain normal field width and padding information,
for example \"%-5s\".  Replacements happen in the sequence given by TABLE,
so values can contain further %-escapes if they are define later in TABLE."
  (let ((tbl (copy-alist table))
	(case-fold-search nil)
        (pchg 0)
        e re rpl)
    (while (setq e (pop tbl))
      (setq re (concat "%-?[0-9.]*" (substring (car e) 1)))
      (when (and (cdr e) (string-match re (cdr e)))
        (let ((sref (substring (cdr e) (match-beginning 0) (match-end 0)))
              (safe "SREF"))
          (add-text-properties 0 3 (list 'sref sref) safe)
          (setcdr e (replace-match safe t t (cdr e)))))
      (while (string-match re string)
        (setq rpl (format (concat (substring (match-string 0 string) 0 -1) "s")
                          (cdr e)))
        (setq string (replace-match rpl t t string))))
    (while (setq pchg (next-property-change pchg string))
      (let ((sref (get-text-property pchg 'sref string)))
	(when (and sref (string-match "SREF" string pchg))
	  (setq string (replace-match sref t t string)))))
    string))

(defun org-sublist (list start end)
  "Return a section of LIST, from START to END.
Counting starts at 1."
  (let (rtn (c start))
    (setq list (nthcdr (1- start) list))
    (while (and list (<= c end))
      (push (pop list) rtn)
      (setq c (1+ c)))
    (nreverse rtn)))

(defun org-find-base-buffer-visiting (file)
  "Like `find-buffer-visiting' but always return the base buffer and
not an indirect buffer."
  (let ((buf (or (get-file-buffer file)
		 (find-buffer-visiting file))))
    (if buf
	(or (buffer-base-buffer buf) buf)
      nil)))

(defun org-image-file-name-regexp (&optional extensions)
  "Return regexp matching the file names of images.
If EXTENSIONS is given, only match these."
  (if (and (not extensions) (fboundp 'image-file-name-regexp))
      (image-file-name-regexp)
    (let ((image-file-name-extensions
	   (or extensions
	       '("png" "jpeg" "jpg" "gif" "tiff" "tif"
		 "xbm" "xpm" "pbm" "pgm" "ppm"))))
      (concat "\\."
	      (regexp-opt (nconc (mapcar 'upcase
					 image-file-name-extensions)
				 image-file-name-extensions)
			  t)
	      "\\'"))))

(defun org-file-image-p (file &optional extensions)
  "Return non-nil if FILE is an image."
  (save-match-data
    (string-match (org-image-file-name-regexp extensions) file)))

(defun org-get-cursor-date ()
  "Return the date at cursor in as a time.
This works in the calendar and in the agenda, anywhere else it just
returns the current time."
  (let (date day defd)
    (cond
     ((eq major-mode 'calendar-mode)
      (setq date (calendar-cursor-to-date)
	    defd (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date))))
     ((eq major-mode 'org-agenda-mode)
      (setq day (get-text-property (point) 'day))
      (if day
	  (setq date (calendar-gregorian-from-absolute day)
		defd (encode-time 0 0 0 (nth 1 date) (nth 0 date)
				  (nth 2 date))))))
    (or defd (current-time))))

(defvar org-agenda-action-marker (make-marker)
  "Marker pointing to the entry for the next agenda action.")

(defun org-mark-entry-for-agenda-action ()
  "Mark the current entry as target of an agenda action.
Agenda actions are actions executed from the agenda with the key `k',
which make use of the date at the cursor."
  (interactive)
  (move-marker org-agenda-action-marker
	       (save-excursion (org-back-to-heading t) (point))
	       (current-buffer))
  (message
   "Entry marked for action; press `k' at desired date in agenda or calendar"))

(defun org-mark-subtree ()
  "Mark the current subtree.
This puts point at the start of the current subtree, and mark at the end.

If point is in an inline task, mark that task instead."
  (interactive)
  (let ((inline-task-p
	 (and (featurep 'org-inlinetask)
	      (org-inlinetask-in-task-p)))
	(beg))
    ;; Get beginning of subtree
    (cond
     (inline-task-p (org-inlinetask-goto-beginning))
     ((org-at-heading-p) (beginning-of-line))
     (t (org-with-limited-levels (outline-previous-visible-heading 1))))
    (setq beg (point))
    ;; Get end of it
    (if	inline-task-p
	(org-inlinetask-goto-end)
      (org-end-of-subtree))
    ;; Mark zone
    (push-mark (point) nil t)
    (goto-char beg)))

;;; Paragraph filling stuff.
;; We want this to be just right, so use the full arsenal.

(defun org-indent-line-function ()
  "Indent line depending on context."
  (interactive)
  (let* ((pos (point))
	 (itemp (org-at-item-p))
	 (case-fold-search t)
	 (org-drawer-regexp (or org-drawer-regexp "\000"))
	 (inline-task-p (and (featurep 'org-inlinetask)
			     (org-inlinetask-in-task-p)))
	 (inline-re (and inline-task-p
			 (org-inlinetask-outline-regexp)))
	 column)
    (beginning-of-line 1)
    (cond
     ;; Comments
     ((looking-at "# ") (setq column 0))
     ;; Headings
     ((looking-at org-outline-regexp) (setq column 0))
     ;; Included files
     ((looking-at "#\\+include:") (setq column 0))
     ;; Footnote definition
     ((looking-at org-footnote-definition-re) (setq column 0))
     ;; Literal examples
     ((looking-at "[ \t]*:\\( \\|$\\)")
      (setq column (org-get-indentation))) ; do nothing
     ;; Lists
     ((ignore-errors (goto-char (org-in-item-p)))
      (setq column (if itemp
		       (org-get-indentation)
		     (org-list-item-body-column (point))))
      (goto-char pos))
     ;; Drawers
     ((and (looking-at "[ \t]*:END:")
	   (save-excursion (re-search-backward org-drawer-regexp nil t)))
      (save-excursion
	(goto-char (1- (match-beginning 1)))
	(setq column (current-column))))
     ;; Special blocks
     ((and (looking-at "[ \t]*#\\+end_\\([a-z]+\\)")
	   (save-excursion
	     (re-search-backward
	      (concat "^[ \t]*#\\+begin_" (downcase (match-string 1))) nil t)))
      (setq column (org-get-indentation (match-string 0))))
     ((and (not (looking-at "[ \t]*#\\+begin_"))
	   (org-between-regexps-p "^[ \t]*#\\+begin_" "[ \t]*#\\+end_"))
      (save-excursion
	(re-search-backward "^[ \t]*#\\+begin_\\([a-z]+\\)" nil t))
      (setq column
            (cond ((equal (downcase (match-string 1)) "src")
                   ;; src blocks: let `org-edit-src-exit' handle them
                   (org-get-indentation))
                  ((equal (downcase (match-string 1)) "example")
                   (max (org-get-indentation)
			(org-get-indentation (match-string 0))))
                  (t
                   (org-get-indentation (match-string 0))))))
     ;; This line has nothing special, look at the previous relevant
     ;; line to compute indentation
     (t
      (beginning-of-line 0)
      (while (and (not (bobp))
		  (not (looking-at org-drawer-regexp))
		  ;; When point started in an inline task, do not move
		  ;; above task starting line.
		  (not (and inline-task-p (looking-at inline-re)))
		  ;; Skip drawers, blocks, empty lines, verbatim,
		  ;; comments, tables, footnotes definitions, lists,
		  ;; inline tasks.
		  (or (and (looking-at "[ \t]*:END:")
			   (re-search-backward org-drawer-regexp nil t))
		      (and (looking-at "[ \t]*#\\+end_")
			   (re-search-backward "[ \t]*#\\+begin_"nil t))
		      (looking-at "[ \t]*[\n:#|]")
		      (looking-at org-footnote-definition-re)
		      (and (ignore-errors (goto-char (org-in-item-p)))
			   (goto-char
			    (org-list-get-top-point (org-list-struct))))
		      (and (not inline-task-p)
			   (featurep 'org-inlinetask)
			   (org-inlinetask-in-task-p)
			   (or (org-inlinetask-goto-beginning) t))))
      	(beginning-of-line 0))
      (cond
       ;; There was an heading above.
       ((looking-at "\\*+[ \t]+")
	(if (not org-adapt-indentation)
	    (setq column 0)
	  (goto-char (match-end 0))
	  (setq column (current-column))))
       ;; A drawer had started and is unfinished
       ((looking-at org-drawer-regexp)
	(goto-char (1- (match-beginning 1)))
	(setq column (current-column)))
       ;; Else, nothing noticeable found: get indentation and go on.
       (t (setq column (org-get-indentation))))))
    ;; Now apply indentation and move cursor accordingly
    (goto-char pos)
    (if (<= (current-column) (current-indentation))
	(org-indent-line-to column)
      (save-excursion (org-indent-line-to column)))
    ;; Special polishing for properties, see `org-property-format'
    (setq column (current-column))
    (beginning-of-line 1)
    (if (looking-at
	 "\\([ \t]+\\)\\(:[-_0-9a-zA-Z]+:\\)[ \t]*\\(\\S-.*\\(\\S-\\|$\\)\\)")
	(replace-match (concat (match-string 1)
			       (format org-property-format
				       (match-string 2) (match-string 3)))
		       t t))
    (org-move-to-column column)))

(defvar org-adaptive-fill-regexp-backup adaptive-fill-regexp
  "Variable to store copy of `adaptive-fill-regexp'.
Since `adaptive-fill-regexp' is set to never match, we need to
store a backup of its value before entering `org-mode' so that
the functionality can be provided as a fall-back.")

(defun org-set-autofill-regexps ()
  (interactive)
  ;; In the paragraph separator we include headlines, because filling
  ;; text in a line directly attached to a headline would otherwise
  ;; fill the headline as well.
  (org-set-local 'comment-start-skip "^#+[ \t]*")
  (org-set-local 'paragraph-separate "\f\\|\\*+ \\|[ 	]*$\\|[ \t]*[:|#]")
  ;; The paragraph starter includes hand-formatted lists.
  (org-set-local
   'paragraph-start
   (concat
    "\f" "\\|"
    "[ 	]*$" "\\|"
    org-outline-regexp "\\|"
    "[ \t]*#" "\\|"
    (org-item-re) "\\|"
    "[ \t]*[:|]" "\\|"
    "\\$\\$" "\\|"
    "\\\\\\(begin\\|end\\|[][]\\)"))
  ;; Inhibit auto-fill for headers, tables and fixed-width lines.
  ;; But only if the user has not turned off tables or fixed-width regions
  (org-set-local
   'auto-fill-inhibit-regexp
   (concat org-outline-regexp
	   "\\|#\\+"
	   "\\|[ \t]*" org-keyword-time-regexp
	   (if (or org-enable-table-editor org-enable-fixed-width-editor)
	       (concat
		"\\|[ \t]*["
		(if org-enable-table-editor "|" "")
		(if org-enable-fixed-width-editor ":"  "")
		"]"))))
  ;; We use our own fill-paragraph function, to make sure that tables
  ;; and fixed-width regions are not wrapped.  That function will pass
  ;; through to `fill-paragraph' when appropriate.
  (org-set-local 'fill-paragraph-function 'org-fill-paragraph)
  ;; Prevent auto-fill from inserting unwanted new items.
  (if (boundp 'fill-nobreak-predicate)
      (org-set-local 'fill-nobreak-predicate
		     (if (memq 'org-fill-item-nobreak-p fill-nobreak-predicate)
			 fill-nobreak-predicate
		       (cons 'org-fill-item-nobreak-p fill-nobreak-predicate))))
  ;; Adaptive filling: To get full control, first make sure that
  ;; `adaptive-fill-regexp' never matches.  Then install our own matcher.
  (unless (local-variable-p 'adaptive-fill-regexp (current-buffer))
    (org-set-local 'org-adaptive-fill-regexp-backup
                   adaptive-fill-regexp))
  (org-set-local 'adaptive-fill-regexp "\000")
  (org-set-local 'normal-auto-fill-function 'org-auto-fill-function)
  (org-set-local 'adaptive-fill-function
		 'org-adaptive-fill-function)
  (org-set-local
   'align-mode-rules-list
   '((org-in-buffer-settings
      (regexp . "^#\\+[A-Z_]+:\\(\\s-*\\)\\S-+")
      (modes . '(org-mode))))))

(defun org-fill-item-nobreak-p ()
  "Non-nil when a line break at point would insert a new item."
  (and (looking-at (org-item-re)) (org-list-in-valid-context-p)))

(defun org-fill-paragraph (&optional justify)
  "Re-align a table, pass through to fill-paragraph if no table."
  (let ((table-p (org-at-table-p))
	(table.el-p (org-at-table.el-p))
	(itemp (org-in-item-p)))
    (cond ((and (equal (char-after (point-at-bol)) ?*)
		(save-excursion (goto-char (point-at-bol))
				(looking-at org-outline-regexp)))
	   t)				; skip headlines
	  (table.el-p t)		; skip table.el tables
	  (table-p (org-table-align) t)	; align Org tables
	  (itemp			; align text in items
	   (let* ((struct (save-excursion (goto-char itemp)
					  (org-list-struct)))
		  (parents (org-list-parents-alist struct))
		  (children (org-list-get-children itemp struct parents))
		  beg end prev next prefix)
	     ;; Determine in which part of item point is: before
	     ;; first child, after last child, between two
	     ;; sub-lists, or simply in item if there's no child.
	     (cond
	      ((not children)
	       (setq prefix (make-string (org-list-item-body-column itemp) ?\ )
		     beg itemp
		     end (org-list-get-item-end itemp struct)))
	      ((< (point) (setq next (car children)))
	       (setq prefix (make-string (org-list-item-body-column itemp) ?\ )
		     beg itemp
		     end next))
	      ((> (point) (setq prev (car (last children))))
	       (setq beg (org-list-get-item-end prev struct)
		     end (org-list-get-item-end itemp struct)
		     prefix (save-excursion
			      (goto-char beg)
			      (skip-chars-forward " \t")
			      (make-string (current-column) ?\ ))))
	      (t (catch 'exit
		   (while (setq next (pop children))
		     (if (> (point) next)
			 (setq prev next)
		       (setq beg (org-list-get-item-end prev struct)
			     end next
			     prefix (save-excursion
				      (goto-char beg)
				      (skip-chars-forward " \t")
				      (make-string (current-column) ?\ )))
		       (throw 'exit nil))))))
	     ;; Use `fill-paragraph' with buffer narrowed to item
	     ;; without any child, and with our computed PREFIX.
	     (flet ((fill-context-prefix (from to &optional flr) prefix))
	       (save-restriction
		 (narrow-to-region beg end)
		 (save-excursion (fill-paragraph justify)))) t))
	  ;; Special case where point is not in a list but is on
	  ;; a paragraph adjacent to a list: make sure this paragraph
	  ;; doesn't get merged with the end of the list by narrowing
	  ;; buffer first.
	  ((save-excursion (forward-paragraph -1)
			   (setq itemp (org-in-item-p)))
	   (let ((struct (save-excursion (goto-char itemp)
					 (org-list-struct))))
	     (save-restriction
	       (narrow-to-region (org-list-get-bottom-point struct)
				 (save-excursion (forward-paragraph 1)
						 (point)))
	       (fill-paragraph justify) t)))
	  ;; Else simply call `fill-paragraph'.
	  (t nil))))

;; For reference, this is the default value of adaptive-fill-regexp
;;  "[ \t]*\\([-|#;>*]+[ \t]*\\|(?[0-9]+[.)][ \t]*\\)*"

(defun org-adaptive-fill-function ()
  "Return a fill prefix for org-mode files."
  (let (itemp)
    (save-excursion
      (cond
       ;; Comment line
       ((looking-at "#[ \t]+")
	(match-string-no-properties 0))
       ;; Plain list item
       ((org-at-item-p)
	(make-string (org-list-item-body-column (point-at-bol)) ?\ ))
       ;; Point is in a list after `backward-paragraph': original
       ;; point wasn't in the list, or filling would have been taken
       ;; care of by `org-auto-fill-function', but the list and the
       ;; real paragraph are not separated by a blank line. Thus, move
       ;; point after the list to go back to real paragraph and
       ;; determine fill-prefix.
       ((setq itemp (org-in-item-p))
	(goto-char itemp)
	(let* ((struct (org-list-struct))
	       (bottom (org-list-get-bottom-point struct)))
	  (goto-char bottom)
	  (make-string (org-get-indentation) ?\ )))
       ;; Other text
       ((looking-at org-adaptive-fill-regexp-backup)
	(match-string-no-properties 0))))))

(defun org-auto-fill-function ()
  "Auto-fill function."
  (let (itemp prefix)
    ;; When in a list, compute an appropriate fill-prefix and make
    ;; sure it will be used by `do-auto-fill'.
    (if (setq itemp (org-in-item-p))
	(progn
	  (setq prefix (make-string (org-list-item-body-column itemp) ?\ ))
	  (flet ((fill-context-prefix (from to &optional flr) prefix))
	    (do-auto-fill)))
      ;; Else just use `do-auto-fill'.
      (do-auto-fill))))

;;; Other stuff.

(defun org-toggle-fixed-width-section (arg)
  "Toggle the fixed-width export.
If there is no active region, the QUOTE keyword at the current headline is
inserted or removed.  When present, it causes the text between this headline
and the next to be exported as fixed-width text, and unmodified.
If there is an active region, this command adds or removes a colon as the
first character of this line.  If the first character of a line is a colon,
this line is also exported in fixed-width font."
  (interactive "P")
  (let* ((cc 0)
	 (regionp (org-region-active-p))
	 (beg (if regionp (region-beginning) (point)))
	 (end (if regionp (region-end)))
	 (nlines (or arg (if (and beg end) (count-lines beg end) 1)))
	 (case-fold-search nil)
	 (re "[ \t]*\\(:\\(?: \\|$\\)\\)")
	 off)
    (if regionp
	(save-excursion
	  (goto-char beg)
	  (setq cc (current-column))
	  (beginning-of-line 1)
	  (setq off (looking-at re))
	  (while (> nlines 0)
	    (setq nlines (1- nlines))
	    (beginning-of-line 1)
	    (cond
	     (arg
	      (org-move-to-column cc t)
	      (insert ": \n")
	      (forward-line -1))
	     ((and off (looking-at re))
	      (replace-match "" t t nil 1))
	     ((not off) (org-move-to-column cc t) (insert ": ")))
	    (forward-line 1)))
      (save-excursion
	(org-back-to-heading)
	(cond
	 ((looking-at (format org-heading-keyword-regexp-format
			      org-quote-string))
	  (goto-char (match-end 1))
	  (looking-at (concat " +" org-quote-string))
	  (replace-match "" t t)
	  (when (eolp) (insert " ")))
	 ((looking-at org-outline-regexp)
	  (goto-char (match-end 0))
	  (insert org-quote-string " ")))))))

(defun org-reftex-citation ()
  "Use reftex-citation to insert a citation into the buffer.
This looks for a line like

#+BIBLIOGRAPHY: foo plain option:-d

and derives from it that foo.bib is the bibliography file relevant
for this document.  It then installs the necessary environment for RefTeX
to work in this buffer and calls `reftex-citation'  to insert a citation
into the buffer.

Export of such citations to both LaTeX and HTML is handled by the contributed
package org-exp-bibtex by Taru Karttunen."
  (interactive)
  (let ((reftex-docstruct-symbol 'rds)
	(reftex-cite-format "\\cite{%l}")
	rds bib)
    (save-excursion
      (save-restriction
	(widen)
	(let ((case-fold-search t)
	      (re "^#\\+bibliography:[ \t]+\\([^ \t\n]+\\)"))
	  (if (not (save-excursion
		     (or (re-search-forward re nil t)
			 (re-search-backward re nil t))))
	      (error "No bibliography defined in file")
	    (setq bib (concat (match-string 1) ".bib")
		  rds (list (list 'bib bib)))))))
    (call-interactively 'reftex-citation)))

;;;; Functions extending outline functionality

(defun org-beginning-of-line (&optional arg)
  "Go to the beginning of the current line.  If that is invisible, continue
to a visible line beginning.  This makes the function of C-a more intuitive.
If this is a headline, and `org-special-ctrl-a/e' is set, ignore tags on the
first attempt, and only move to after the tags when the cursor is already
beyond the end of the headline."
  (interactive "P")
  (let ((pos (point))
	(special (if (consp org-special-ctrl-a/e)
		     (car org-special-ctrl-a/e)
		   org-special-ctrl-a/e))
	refpos)
    (if (org-bound-and-true-p line-move-visual)
	(beginning-of-visual-line 1)
      (beginning-of-line 1))
    (if (and arg (fboundp 'move-beginning-of-line))
	(call-interactively 'move-beginning-of-line)
      (if (bobp)
	  nil
	(backward-char 1)
	(if (org-truely-invisible-p)
	    (while (and (not (bobp)) (org-truely-invisible-p))
	      (backward-char 1)
	      (beginning-of-line 1))
	  (forward-char 1))))
    (when special
      (cond
       ((and (looking-at org-complex-heading-regexp)
	     (= (char-after (match-end 1)) ?\ ))
	(setq refpos (min (1+ (or (match-end 3) (match-end 2) (match-end 1)))
			  (point-at-eol)))
	(goto-char
	 (if (eq special t)
	     (cond ((> pos refpos) refpos)
		   ((= pos (point)) refpos)
		   (t (point)))
	   (cond ((> pos (point)) (point))
		 ((not (eq last-command this-command)) (point))
		 (t refpos)))))
       ((org-at-item-p)
	;; Being at an item and not looking at an the item means point
	;; was previously moved to beginning of a visual line, which
	;; doesn't contain the item.  Therefore, do nothing special,
	;; just stay here.
	(when (looking-at org-list-full-item-re)
	  ;; Set special position at first white space character after
	  ;; bullet, and check-box, if any.
	  (let ((after-bullet
		 (let ((box (match-end 3)))
		   (if (not box) (match-end 1)
		     (let ((after (char-after box)))
		       (if (and after (= after ? )) (1+ box) box))))))
	    ;; Special case: Move point to special position when
	    ;; currently after it or at beginning of line.
	    (if (eq special t)
		(when (or (> pos after-bullet) (= (point) pos))
		  (goto-char after-bullet))
	      ;; Reversed case: Move point to special position when
	      ;; point was already at beginning of line and command is
	      ;; repeated.
	      (when (and (= (point) pos) (eq last-command this-command))
		(goto-char after-bullet))))))))
    (org-no-warnings
     (and (featurep 'xemacs) (setq zmacs-region-stays t)))))

(defun org-end-of-line (&optional arg)
  "Go to the end of the line.
If this is a headline, and `org-special-ctrl-a/e' is set, ignore tags on the
first attempt, and only move to after the tags when the cursor is already
beyond the end of the headline."
  (interactive "P")
  (let ((special (if (consp org-special-ctrl-a/e)
		     (cdr org-special-ctrl-a/e)
		   org-special-ctrl-a/e)))
    (cond
     ((or (not special) arg
	  (not (or (org-at-heading-p) (org-at-item-p) (org-at-drawer-p))))
      (call-interactively
       (cond ((org-bound-and-true-p line-move-visual) 'end-of-visual-line)
	     ((fboundp 'move-end-of-line) 'move-end-of-line)
	     (t 'end-of-line))))
     ((org-at-heading-p)
      (let ((pos (point)))
	(beginning-of-line 1)
	(if (looking-at (org-re ".*?\\(?:\\([ \t]*\\)\\(:[[:alnum:]_@#%:]+:\\)?[ \t]*\\)?$"))
	    (if (eq special t)
		(if (or (< pos (match-beginning 1))
			(= pos (match-end 0)))
		    (goto-char (match-beginning 1))
		  (goto-char (match-end 0)))
	      (if (or (< pos (match-end 0)) (not (eq this-command last-command)))
		  (goto-char (match-end 0))
		(goto-char (match-beginning 1))))
	  (call-interactively (if (fboundp 'move-end-of-line)
				  'move-end-of-line
				'end-of-line)))))
     ((org-at-drawer-p)
      (move-end-of-line 1)
      (when (overlays-at (1- (point))) (backward-char 1)))
     ;; At an item: Move before any hidden text.
     (t (call-interactively
	 (cond ((org-bound-and-true-p line-move-visual) 'end-of-visual-line)
	       ((fboundp 'move-end-of-line) 'move-end-of-line)
	       (t 'end-of-line)))))
    (org-no-warnings
     (and (featurep 'xemacs) (setq zmacs-region-stays t)))))

(define-key org-mode-map "\C-a" 'org-beginning-of-line)
(define-key org-mode-map "\C-e" 'org-end-of-line)

(defun org-backward-sentence (&optional arg)
  "Go to beginning of sentence, or beginning of table field.
This will call `backward-sentence' or `org-table-beginning-of-field',
depending on context."
  (interactive "P")
  (cond
   ((org-at-table-p) (call-interactively 'org-table-beginning-of-field))
   (t (call-interactively 'backward-sentence))))

(defun org-forward-sentence (&optional arg)
  "Go to end of sentence, or end of table field.
This will call `forward-sentence' or `org-table-end-of-field',
depending on context."
  (interactive "P")
  (cond
   ((org-at-table-p) (call-interactively 'org-table-end-of-field))
   (t (call-interactively 'forward-sentence))))

(define-key org-mode-map "\M-a" 'org-backward-sentence)
(define-key org-mode-map "\M-e" 'org-forward-sentence)

(defun org-kill-line (&optional arg)
  "Kill line, to tags or end of line."
  (interactive "P")
  (cond
   ((or (not org-special-ctrl-k)
	(bolp)
	(not (org-at-heading-p)))
    (if (and (get-char-property (min (point-max) (point-at-eol)) 'invisible)
	     org-ctrl-k-protect-subtree)
	(if (or (eq org-ctrl-k-protect-subtree 'error)
		(not (y-or-n-p "Kill hidden subtree along with headline? ")))
	    (error "C-k aborted - would kill hidden subtree")))
    (call-interactively 'kill-line))
   ((looking-at (org-re ".*?\\S-\\([ \t]+\\(:[[:alnum:]_@#%:]+:\\)\\)[ \t]*$"))
    (kill-region (point) (match-beginning 1))
    (org-set-tags nil t))
   (t (kill-region (point) (point-at-eol)))))

(define-key org-mode-map "\C-k" 'org-kill-line)

(defun org-yank (&optional arg)
  "Yank.  If the kill is a subtree, treat it specially.
This command will look at the current kill and check if is a single
subtree, or a series of subtrees[1].  If it passes the test, and if the
cursor is at the beginning of a line or after the stars of a currently
empty headline, then the yank is handled specially.  How exactly depends
on the value of the following variables, both set by default.

org-yank-folded-subtrees
    When set, the subtree(s) will be folded after insertion, but only
    if doing so would now swallow text after the yanked text.

org-yank-adjusted-subtrees
    When set, the subtree will be promoted or demoted in order to
    fit into the local outline tree structure, which means that the level
    will be adjusted so that it becomes the smaller one of the two
    *visible* surrounding headings.

Any prefix to this command will cause `yank' to be called directly with
no special treatment.  In particular, a simple \\[universal-argument] prefix \
will just
plainly yank the text as it is.

\[1] The test checks if the first non-white line is a heading
    and if there are no other headings with fewer stars."
  (interactive "P")
  (org-yank-generic 'yank arg))

(defun org-yank-generic (command arg)
  "Perform some yank-like command.

This function implements the behavior described in the `org-yank'
documentation.  However, it has been generalized to work for any
interactive command with similar behavior."

  ;; pretend to be command COMMAND
  (setq this-command command)

  (if arg
      (call-interactively command)

    (let ((subtreep ; is kill a subtree, and the yank position appropriate?
	   (and (org-kill-is-subtree-p)
		(or (bolp)
		    (and (looking-at "[ \t]*$")
			 (string-match
			  "\\`\\*+\\'"
			  (buffer-substring (point-at-bol) (point)))))))
	  swallowp)
      (cond
       ((and subtreep org-yank-folded-subtrees)
	(let ((beg (point))
	      end)
	  (if (and subtreep org-yank-adjusted-subtrees)
	      (org-paste-subtree nil nil 'for-yank)
           (call-interactively command))

	  (setq end (point))
	  (goto-char beg)
	  (when (and (bolp) subtreep
		     (not (setq swallowp
				(org-yank-folding-would-swallow-text beg end))))
	    (org-with-limited-levels
	     (or (looking-at org-outline-regexp)
		 (re-search-forward org-outline-regexp-bol end t))
	     (while (and (< (point) end) (looking-at org-outline-regexp))
	       (hide-subtree)
	       (org-cycle-show-empty-lines 'folded)
	       (condition-case nil
		   (outline-forward-same-level 1)
		 (error (goto-char end))))))
	  (when swallowp
	    (message
	     "Inserted text not folded because that would swallow text"))

	  (goto-char end)
	  (skip-chars-forward " \t\n\r")
	  (beginning-of-line 1)
	  (push-mark beg 'nomsg)))
       ((and subtreep org-yank-adjusted-subtrees)
	(let ((beg (point-at-bol)))
	  (org-paste-subtree nil nil 'for-yank)
	  (push-mark beg 'nomsg)))
       (t
       (call-interactively command))))))

(defun org-yank-folding-would-swallow-text (beg end)
  "Would hide-subtree at BEG swallow any text after END?"
  (let (level)
    (org-with-limited-levels
     (save-excursion
       (goto-char beg)
       (when (or (looking-at org-outline-regexp)
		 (re-search-forward org-outline-regexp-bol end t))
	 (setq level (org-outline-level)))
       (goto-char end)
       (skip-chars-forward " \t\r\n\v\f")
       (if (or (eobp)
	       (and (bolp) (looking-at org-outline-regexp)
		    (<= (org-outline-level) level)))
	   nil				; Nothing would be swallowed
	 t)))))				; something would swallow

(define-key org-mode-map "\C-y" 'org-yank)

(defun org-truely-invisible-p ()
  "Check if point is at a character currently not visible.
This version does not only check the character property, but also
`visible-mode'."
  ;; Early versions of noutline don't have `outline-invisible-p'.
  (if (org-bound-and-true-p visible-mode)
      nil
    (outline-invisible-p)))

(defun org-invisible-p2 ()
  "Check if point is at a character currently not visible."
  (save-excursion
    (if (and (eolp) (not (bobp))) (backward-char 1))
    ;; Early versions of noutline don't have `outline-invisible-p'.
    (outline-invisible-p)))

(defun org-back-to-heading (&optional invisible-ok)
  "Call `outline-back-to-heading', but provide a better error message."
  (condition-case nil
      (outline-back-to-heading invisible-ok)
    (error (error "Before first headline at position %d in buffer %s"
		  (point) (current-buffer)))))

(defun org-beginning-of-defun ()
  "Go to the beginning of the subtree, i.e. back to the heading."
  (org-back-to-heading))
(defun org-end-of-defun ()
  "Go to the end of the subtree."
  (org-end-of-subtree nil t))

(defun org-before-first-heading-p ()
  "Before first heading?"
  (save-excursion
    (end-of-line)
    (null (re-search-backward org-outline-regexp-bol nil t))))

(defun org-at-heading-p (&optional ignored)
  (outline-on-heading-p t))
;; Compatibility alias with Org versions < 7.8.03
(defalias 'org-on-heading-p 'org-at-heading-p)

(defun org-at-drawer-p nil
  "Whether point is at a drawer."
  (save-excursion
    (move-beginning-of-line 1)
    (looking-at org-drawer-regexp)))

(defun org-point-at-end-of-empty-headline ()
  "If point is at the end of an empty headline, return t, else nil.
If the heading only contains a TODO keyword, it is still still considered
empty."
  (and (looking-at "[ \t]*$")
       (when org-todo-line-regexp
	 (save-excursion
	   (beginning-of-line 1)
	   (let ((case-fold-search nil))
	     (looking-at org-todo-line-regexp)
	     (string= (match-string 3) ""))))))

(defun org-at-heading-or-item-p ()
  (or (org-at-heading-p) (org-at-item-p)))

(defun org-at-target-p ()
  (or (org-in-regexp org-radio-target-regexp)
      (org-in-regexp org-target-regexp)))
;; Compatibility alias with Org versions < 7.8.03
(defalias 'org-on-target-p 'org-at-target-p)

(defun org-up-heading-all (arg)
  "Move to the heading line of which the present line is a subheading.
This function considers both visible and invisible heading lines.
With argument, move up ARG levels."
  (if (fboundp 'outline-up-heading-all)
      (outline-up-heading-all arg)   ; emacs 21 version of outline.el
    (outline-up-heading arg t)))     ; emacs 22 version of outline.el

(defun org-up-heading-safe ()
  "Move to the heading line of which the present line is a subheading.
This version will not throw an error.  It will return the level of the
headline found, or nil if no higher level is found.

Also, this function will be a lot faster than `outline-up-heading',
because it relies on stars being the outline starters.  This can really
make a significant difference in outlines with very many siblings."
  (let (start-level re)
    (org-back-to-heading t)
    (setq start-level (funcall outline-level))
    (if (equal start-level 1)
	nil
      (setq re (concat "^\\*\\{1," (number-to-string (1- start-level)) "\\} "))
      (if (re-search-backward re nil t)
	  (funcall outline-level)))))

(defun org-first-sibling-p ()
  "Is this heading the first child of its parents?"
  (interactive)
  (let ((re org-outline-regexp-bol)
	level l)
    (unless (org-at-heading-p t)
      (error "Not at a heading"))
    (setq level (funcall outline-level))
    (save-excursion
      (if (not (re-search-backward re nil t))
	  t
	(setq l (funcall outline-level))
	(< l level)))))

(defun org-goto-sibling (&optional previous)
  "Goto the next sibling, even if it is invisible.
When PREVIOUS is set, go to the previous sibling instead.  Returns t
when a sibling was found.  When none is found, return nil and don't
move point."
  (let ((fun (if previous 're-search-backward 're-search-forward))
	(pos (point))
	(re org-outline-regexp-bol)
	level l)
    (when (condition-case nil (org-back-to-heading t) (error nil))
      (setq level (funcall outline-level))
      (catch 'exit
	(or previous (forward-char 1))
	(while (funcall fun re nil t)
	  (setq l (funcall outline-level))
	  (when (< l level) (goto-char pos) (throw 'exit nil))
	  (when (= l level) (goto-char (match-beginning 0)) (throw 'exit t)))
	(goto-char pos)
	nil))))

(defun org-show-siblings ()
  "Show all siblings of the current headline."
  (save-excursion
    (while (org-goto-sibling) (org-flag-heading nil)))
  (save-excursion
    (while (org-goto-sibling 'previous)
      (org-flag-heading nil))))

(defun org-goto-first-child ()
  "Goto the first child, even if it is invisible.
Return t when a child was found.  Otherwise don't move point and
return nil."
  (let (level (pos (point)) (re org-outline-regexp-bol))
    (when (condition-case nil (org-back-to-heading t) (error nil))
      (setq level (outline-level))
      (forward-char 1)
      (if (and (re-search-forward re nil t) (> (outline-level) level))
	  (progn (goto-char (match-beginning 0)) t)
	(goto-char pos) nil))))

(defun org-show-hidden-entry ()
  "Show an entry where even the heading is hidden."
  (save-excursion
    (org-show-entry)))

(defun org-flag-heading (flag &optional entry)
  "Flag the current heading.  FLAG non-nil means make invisible.
When ENTRY is non-nil, show the entire entry."
  (save-excursion
    (org-back-to-heading t)
    ;; Check if we should show the entire entry
    (if entry
	(progn
	  (org-show-entry)
	  (save-excursion
	    (and (outline-next-heading)
		 (org-flag-heading nil))))
      (outline-flag-region (max (point-min) (1- (point)))
			   (save-excursion (outline-end-of-heading) (point))
			   flag))))

(defun org-get-next-sibling ()
  "Move to next heading of the same level, and return point.
If there is no such heading, return nil.
This is like outline-next-sibling, but invisible headings are ok."
  (let ((level (funcall outline-level)))
    (outline-next-heading)
    (while (and (not (eobp)) (> (funcall outline-level) level))
      (outline-next-heading))
    (if (or (eobp) (< (funcall outline-level) level))
	nil
      (point))))

(defun org-get-last-sibling ()
  "Move to previous heading of the same level, and return point.
If there is no such heading, return nil."
  (let ((opoint (point))
	(level (funcall outline-level)))
    (outline-previous-heading)
    (when (and (/= (point) opoint) (outline-on-heading-p t))
      (while (and (> (funcall outline-level) level)
		  (not (bobp)))
	(outline-previous-heading))
      (if (< (funcall outline-level) level)
	  nil
        (point)))))

(defun org-end-of-subtree (&optional invisible-OK to-heading)
  ;; This contains an exact copy of the original function, but it uses
  ;; `org-back-to-heading', to make it work also in invisible
  ;; trees.  And is uses an invisible-OK argument.
  ;; Under Emacs this is not needed, but the old outline.el needs this fix.
  ;; Furthermore, when used inside Org, finding the end of a large subtree
  ;; with many children and grandchildren etc, this can be much faster
  ;; than the outline version.
  (org-back-to-heading invisible-OK)
  (let ((first t)
	(level (funcall outline-level)))
    (if (and (eq major-mode 'org-mode) (< level 1000))
	;; A true heading (not a plain list item), in Org-mode
	;; This means we can easily find the end by looking
	;; only for the right number of stars.  Using a regexp to do
	;; this is so much faster than using a Lisp loop.
	(let ((re (concat "^\\*\\{1," (int-to-string level) "\\} ")))
	  (forward-char 1)
	  (and (re-search-forward re nil 'move) (beginning-of-line 1)))
      ;; something else, do it the slow way
      (while (and (not (eobp))
		  (or first (> (funcall outline-level) level)))
	(setq first nil)
	(outline-next-heading)))
    (unless to-heading
      (if (memq (preceding-char) '(?\n ?\^M))
    	  (progn
    	    ;; Go to end of line before heading
    	    (forward-char -1)
    	    (if (memq (preceding-char) '(?\n ?\^M))
    		;; leave blank line before heading
    		(forward-char -1))))))
  (point))

(defadvice outline-end-of-subtree (around prefer-org-version activate compile)
  "Use Org version in org-mode, for dramatic speed-up."
  (if (eq major-mode 'org-mode)
      (progn
	(org-end-of-subtree nil t)
	(unless (eobp) (backward-char 1)))
    ad-do-it))

(defun org-end-of-meta-data-and-drawers ()
  "Jump to the first text after meta data and drawers in the current entry.
This will move over empty lines, lines with planning time stamps,
clocking lines, and drawers."
  (org-back-to-heading t)
  (let ((end (save-excursion (outline-next-heading) (point)))
	(re (concat "\\(" org-drawer-regexp "\\)"
		    "\\|" "[ \t]*" org-keyword-time-regexp)))
    (forward-line 1)
    (while (re-search-forward re end t)
      (if (not (match-end 1))
	  ;; empty or planning line
	  (forward-line 1)
	;; a drawer, find the end
	(re-search-forward "^[ \t]*:END:" end 'move)
	(forward-line 1)))
    (and (re-search-forward "[^\n]" nil t) (backward-char 1))
    (point)))

(defun org-forward-same-level (arg &optional invisible-ok)
  "Move forward to the arg'th subheading at same level as this one.
Stop at the first and last subheadings of a superior heading.
Normally this only looks at visible headings, but when INVISIBLE-OK is non-nil
it wil also look at invisible ones."
  (interactive "p")
  (org-back-to-heading invisible-ok)
  (org-at-heading-p)
  (let* ((level (- (match-end 0) (match-beginning 0) 1))
	 (re (format "^\\*\\{1,%d\\} " level))
	 l)
    (forward-char 1)
    (while (> arg 0)
      (while (and (re-search-forward re nil 'move)
		  (setq l (- (match-end 0) (match-beginning 0) 1))
		  (= l level)
		  (not invisible-ok)
		  (progn (backward-char 1) (outline-invisible-p)))
	(if (< l level) (setq arg 1)))
      (setq arg (1- arg)))
    (beginning-of-line 1)))

(defun org-backward-same-level (arg &optional invisible-ok)
  "Move backward to the arg'th subheading at same level as this one.
Stop at the first and last subheadings of a superior heading."
  (interactive "p")
  (org-back-to-heading)
  (org-at-heading-p)
  (let* ((level (- (match-end 0) (match-beginning 0) 1))
	 (re (format "^\\*\\{1,%d\\} " level))
	 l)
    (while (> arg 0)
      (while (and (re-search-backward re nil 'move)
		  (setq l (- (match-end 0) (match-beginning 0) 1))
		  (= l level)
		  (not invisible-ok)
		  (outline-invisible-p))
	(if (< l level) (setq arg 1)))
      (setq arg (1- arg)))))

(defun org-show-subtree ()
  "Show everything after this heading at deeper levels."
  (interactive)
  (outline-flag-region
   (point)
   (save-excursion
     (org-end-of-subtree t t))
   nil))

(defun org-show-entry ()
  "Show the body directly following this heading.
Show the heading too, if it is currently invisible."
  (interactive)
  (save-excursion
    (condition-case nil
	(progn
	  (org-back-to-heading t)
	  (outline-flag-region
	   (max (point-min) (1- (point)))
	   (save-excursion
	     (if (re-search-forward
		  (concat "[\r\n]\\(" org-outline-regexp "\\)") nil t)
		 (match-beginning 1)
	       (point-max)))
	   nil)
	  (org-cycle-hide-drawers 'children))
      (error nil))))

(defun org-make-options-regexp (kwds &optional extra)
  "Make a regular expression for keyword lines."
  (concat
   "^"
   "#?[ \t]*\\+\\("
   (mapconcat 'regexp-quote kwds "\\|")
   (if extra (concat "\\|" extra))
   "\\):[ \t]*"
   "\\(.*\\)"))

;; Make isearch reveal the necessary context
(defun org-isearch-end ()
  "Reveal context after isearch exits."
  (when isearch-success ; only if search was successful
    (if (featurep 'xemacs)
	;; Under XEmacs, the hook is run in the correct place,
	;; we directly show the context.
	(org-show-context 'isearch)
      ;; In Emacs the hook runs *before* restoring the overlays.
      ;; So we have to use a one-time post-command-hook to do this.
      ;; (Emacs 22 has a special variable, see function `org-mode')
      (unless (and (boundp 'isearch-mode-end-hook-quit)
		   isearch-mode-end-hook-quit)
	;; Only when the isearch was not quitted.
	(org-add-hook 'post-command-hook 'org-isearch-post-command
		      'append 'local)))))

(defun org-isearch-post-command ()
  "Remove self from hook, and show context."
  (remove-hook 'post-command-hook 'org-isearch-post-command 'local)
  (org-show-context 'isearch))


;;;; Integration with and fixes for other packages

;;; Imenu support

(defvar org-imenu-markers nil
  "All markers currently used by Imenu.")
(make-variable-buffer-local 'org-imenu-markers)

(defun org-imenu-new-marker (&optional pos)
  "Return a new marker for use by Imenu, and remember the marker."
  (let ((m (make-marker)))
    (move-marker m (or pos (point)))
    (push m org-imenu-markers)
    m))

(defun org-imenu-get-tree ()
  "Produce the index for Imenu."
  (mapc (lambda (x) (move-marker x nil)) org-imenu-markers)
  (setq org-imenu-markers nil)
  (let* ((n org-imenu-depth)
	 (re (concat "^" (org-get-limited-outline-regexp)))
	 (subs (make-vector (1+ n) nil))
	 (last-level 0)
	 m level head)
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-max))
	(while (re-search-backward re nil t)
	  (setq level (org-reduced-level (funcall outline-level)))
	  (when (and (<= level n)
		     (looking-at org-complex-heading-regexp))
	    (setq head (org-link-display-format
			(org-match-string-no-properties 4))
		  m (org-imenu-new-marker))
	    (org-add-props head nil 'org-imenu-marker m 'org-imenu t)
	    (if (>= level last-level)
		(push (cons head m) (aref subs level))
	      (push (cons head (aref subs (1+ level))) (aref subs level))
	      (loop for i from (1+ level) to n do (aset subs i nil)))
	    (setq last-level level)))))
    (aref subs 1)))

(eval-after-load "imenu"
  '(progn
     (add-hook 'imenu-after-jump-hook
	       (lambda ()
		 (if (eq major-mode 'org-mode)
		     (org-show-context 'org-goto))))))

(defun org-link-display-format (link)
  "Replace a link with either the description, or the link target
if no description is present"
  (save-match-data
    (if (string-match org-bracket-link-analytic-regexp link)
	    (replace-match (if (match-end 5)
			       (match-string 5 link)
			     (concat (match-string 1 link)
				     (match-string 3 link)))
			   nil t link)
      link)))

(defun org-toggle-link-display ()
  "Toggle the literal or descriptive display of links."
  (interactive)
  (if org-descriptive-links
      (progn (org-remove-from-invisibility-spec '(org-link))
	     (org-restart-font-lock)
	     (setq org-descriptive-links nil))
    (progn (add-to-invisibility-spec '(org-link))
	   (org-restart-font-lock)
	   (setq org-descriptive-links t))))

;; Speedbar support

(defvar org-speedbar-restriction-lock-overlay (make-overlay 1 1)
  "Overlay marking the agenda restriction line in speedbar.")
(overlay-put org-speedbar-restriction-lock-overlay
		 'face 'org-agenda-restriction-lock)
(overlay-put org-speedbar-restriction-lock-overlay
		 'help-echo "Agendas are currently limited to this item.")
(org-detach-overlay org-speedbar-restriction-lock-overlay)

(defun org-speedbar-set-agenda-restriction ()
  "Restrict future agenda commands to the location at point in speedbar.
To get rid of the restriction, use \\[org-agenda-remove-restriction-lock]."
  (interactive)
  (require 'org-agenda)
  (let (p m tp np dir txt)
    (cond
     ((setq p (text-property-any (point-at-bol) (point-at-eol)
				 'org-imenu t))
      (setq m (get-text-property p 'org-imenu-marker))
      (with-current-buffer (marker-buffer m)
	(goto-char m)
	(org-agenda-set-restriction-lock 'subtree)))
     ((setq p (text-property-any (point-at-bol) (point-at-eol)
				 'speedbar-function 'speedbar-find-file))
      (setq tp (previous-single-property-change
		(1+ p) 'speedbar-function)
	    np (next-single-property-change
		tp 'speedbar-function)
	    dir (speedbar-line-directory)
	    txt (buffer-substring-no-properties (or tp (point-min))
						(or np (point-max))))
      (with-current-buffer (find-file-noselect
			    (let ((default-directory dir))
			      (expand-file-name txt)))
	(unless (eq major-mode 'org-mode)
	  (error "Cannot restrict to non-Org-mode file"))
	(org-agenda-set-restriction-lock 'file)))
     (t (error "Don't know how to restrict Org-mode's agenda")))
    (move-overlay org-speedbar-restriction-lock-overlay
		  (point-at-bol) (point-at-eol))
    (setq current-prefix-arg nil)
    (org-agenda-maybe-redo)))

(eval-after-load "speedbar"
  '(progn
     (speedbar-add-supported-extension ".org")
     (define-key speedbar-file-key-map "<" 'org-speedbar-set-agenda-restriction)
     (define-key speedbar-file-key-map "\C-c\C-x<" 'org-speedbar-set-agenda-restriction)
     (define-key speedbar-file-key-map ">" 'org-agenda-remove-restriction-lock)
     (define-key speedbar-file-key-map "\C-c\C-x>" 'org-agenda-remove-restriction-lock)
     (add-hook 'speedbar-visiting-tag-hook
	       (lambda () (and (eq major-mode 'org-mode) (org-show-context 'org-goto))))))

;;; Fixes and Hacks for problems with other packages

;; Make flyspell not check words in links, to not mess up our keymap
(defun org-mode-flyspell-verify ()
  "Don't let flyspell put overlays at active buttons, or on
   {todo,all-time,additional-option-like}-keywords."
  (let ((pos (max (1- (point)) (point-min)))
	(word (thing-at-point 'word)))
    (and (not (get-text-property pos 'keymap))
	 (not (get-text-property pos 'org-no-flyspell))
	 (not (member word org-todo-keywords-1))
	 (not (member word org-all-time-keywords))
	 (not (member word org-additional-option-like-keywords)))))

(defun org-remove-flyspell-overlays-in (beg end)
  "Remove flyspell overlays in region."
  (and (org-bound-and-true-p flyspell-mode)
       (fboundp 'flyspell-delete-region-overlays)
       (flyspell-delete-region-overlays beg end))
  (add-text-properties beg end '(org-no-flyspell t)))

;; Make `bookmark-jump' shows the jump location if it was hidden.
(eval-after-load "bookmark"
  '(if (boundp 'bookmark-after-jump-hook)
       ;; We can use the hook
       (add-hook 'bookmark-after-jump-hook 'org-bookmark-jump-unhide)
     ;; Hook not available, use advice
     (defadvice bookmark-jump (after org-make-visible activate)
       "Make the position visible."
       (org-bookmark-jump-unhide))))

;; Make sure saveplace shows the location if it was hidden
(eval-after-load "saveplace"
  '(defadvice save-place-find-file-hook (after org-make-visible activate)
     "Make the position visible."
     (org-bookmark-jump-unhide)))

;; Make sure ecb shows the location if it was hidden
(eval-after-load "ecb"
  '(defadvice ecb-method-clicked (after esf/org-show-context activate)
     "Make hierarchy visible when jumping into location from ECB tree buffer."
     (if (eq major-mode 'org-mode)
	 (org-show-context))))

(defun org-bookmark-jump-unhide ()
  "Unhide the current position, to show the bookmark location."
  (and (eq major-mode 'org-mode)
       (or (outline-invisible-p)
	   (save-excursion (goto-char (max (point-min) (1- (point))))
			   (outline-invisible-p)))
       (org-show-context 'bookmark-jump)))

;; Make session.el ignore our circular variable
(eval-after-load "session"
  '(add-to-list 'session-globals-exclude 'org-mark-ring))

;;;; Experimental code

(defun org-closed-in-range ()
  "Sparse tree of items closed in a certain time range.
Still experimental, may disappear in the future."
  (interactive)
  ;; Get the time interval from the user.
  (let* ((time1 (org-float-time
                 (org-read-date nil 'to-time nil "Starting date: ")))
         (time2 (org-float-time
                 (org-read-date nil 'to-time nil "End date:")))
         ;; callback function
         (callback (lambda ()
                     (let ((time
                            (org-float-time
                             (apply 'encode-time
                                    (org-parse-time-string
                                     (match-string 1))))))
                       ;; check if time in interval
                       (and (>= time time1) (<= time time2))))))
    ;; make tree, check each match with the callback
    (org-occur "CLOSED: +\\[\\(.*?\\)\\]" nil callback)))

;;;; Finish up

(provide 'org)

(run-hooks 'org-load-hook)

;;; org.el ends here
