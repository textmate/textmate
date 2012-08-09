;;; gnus-group.el --- group mode commands for Gnus

;; Copyright (C) 1996-2012  Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news

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

;; For Emacs <22.2 and XEmacs.
(eval-and-compile
  (unless (fboundp 'declare-function) (defmacro declare-function (&rest r))))

(eval-when-compile
  (require 'cl))
(defvar tool-bar-mode)

(require 'gnus)
(require 'gnus-start)
(require 'nnmail)
(require 'gnus-spec)
(require 'gnus-int)
(require 'gnus-range)
(require 'gnus-win)
(require 'gnus-undo)
(require 'gmm-utils)
(require 'time-date)
(require 'gnus-ems)

(eval-when-compile
  (require 'mm-url)
  (let ((features (cons 'gnus-group features)))
    (require 'gnus-sum))
  (unless (boundp 'gnus-cache-active-hashtb)
    (defvar gnus-cache-active-hashtb nil)))

(autoload 'gnus-agent-total-fetched-for "gnus-agent")
(autoload 'gnus-cache-total-fetched-for "gnus-cache")

(autoload 'gnus-group-make-nnir-group "nnir")

(defcustom gnus-no-groups-message "No Gnus is good news"
  "*Message displayed by Gnus when no groups are available."
  :group 'gnus-start
  :type 'string)

(defcustom gnus-keep-same-level nil
  "*Non-nil means that the next newsgroup after the current will be on the same level.
When you type, for instance, `n' after reading the last article in the
current newsgroup, you will go to the next newsgroup.  If this variable
is nil, the next newsgroup will be the next from the group
buffer.
If this variable is non-nil, Gnus will either put you in the
next newsgroup with the same level, or, if no such newsgroup is
available, the next newsgroup with the lowest possible level higher
than the current level.
If this variable is `best', Gnus will make the next newsgroup the one
with the best level."
  :group 'gnus-group-levels
  :type '(choice (const nil)
		 (const best)
		 (sexp :tag "other" t)))

(defcustom gnus-group-goto-unread t
  "*If non-nil, movement commands will go to the next unread and subscribed group."
  :link '(custom-manual "(gnus)Group Maneuvering")
  :group 'gnus-group-various
  :type 'boolean)

(defcustom gnus-goto-next-group-when-activating t
  "*If non-nil, the \\<gnus-group-mode-map>\\[gnus-group-get-new-news-this-group] command will advance point to the next group."
  :link '(custom-manual "(gnus)Scanning New Messages")
  :group 'gnus-group-various
  :type 'boolean)

(defcustom gnus-permanently-visible-groups nil
  "*Regexp to match groups that should always be listed in the group buffer.
This means that they will still be listed even when there are no
unread articles in the groups.

If nil, no groups are permanently visible."
  :group 'gnus-group-listing
  :type '(choice regexp (const nil)))

(defcustom gnus-safe-html-newsgroups "\\`nnrss[+:]"
  "Groups in which links in html articles are considered all safe.
The value may be a regexp matching those groups, a list of group names,
or nil.  This overrides `mm-w3m-safe-url-regexp' (which see).  This is
effective only when emacs-w3m renders html articles, i.e., in the case
`mm-text-html-renderer' is set to `w3m'."
  :version "23.2"
  :group 'gnus-group-various
  :type '(choice regexp
		 (repeat :tag "List of group names" (string :tag "Group"))
		 (const nil)))

(defcustom gnus-list-groups-with-ticked-articles t
  "*If non-nil, list groups that have only ticked articles.
If nil, only list groups that have unread articles."
  :group 'gnus-group-listing
  :type 'boolean)

(defcustom gnus-group-default-list-level gnus-level-subscribed
  "Default listing level.
Ignored if `gnus-group-use-permanent-levels' is non-nil."
  :group 'gnus-group-listing
  :type '(choice (integer :tag "Level")
                 (function :tag "Function returning level")))

(defcustom gnus-group-list-inactive-groups t
  "*If non-nil, inactive groups will be listed."
  :group 'gnus-group-listing
  :group 'gnus-group-levels
  :type 'boolean)

(defcustom gnus-group-sort-function 'gnus-group-sort-by-alphabet
  "*Function used for sorting the group buffer.
This function will be called with group info entries as the arguments
for the groups to be sorted.  Pre-made functions include
`gnus-group-sort-by-alphabet', `gnus-group-sort-by-real-name',
`gnus-group-sort-by-unread', `gnus-group-sort-by-level',
`gnus-group-sort-by-score', `gnus-group-sort-by-method',
`gnus-group-sort-by-server', and `gnus-group-sort-by-rank'.

This variable can also be a list of sorting functions.  In that case,
the most significant sort function should be the last function in the
list."
  :group 'gnus-group-listing
  :link '(custom-manual "(gnus)Sorting Groups")
  :type '(repeat :value-to-internal (lambda (widget value)
				      (if (listp value) value (list value)))
		 :match (lambda (widget value)
			  (or (symbolp value)
			      (widget-editable-list-match widget value)))
		 (choice (function-item gnus-group-sort-by-alphabet)
			 (function-item gnus-group-sort-by-real-name)
			 (function-item gnus-group-sort-by-unread)
			 (function-item gnus-group-sort-by-level)
			 (function-item gnus-group-sort-by-score)
			 (function-item gnus-group-sort-by-method)
			 (function-item gnus-group-sort-by-server)
			 (function-item gnus-group-sort-by-rank)
			 (function :tag "other" nil))))

(defcustom gnus-group-line-format "%M\%S\%p\%P\%5y:%B%(%g%)\n"
  "*Format of group lines.
It works along the same lines as a normal formatting string,
with some simple extensions.

%M    Only marked articles (character, \"*\" or \" \")
%S    Whether the group is subscribed (character, \"U\", \"K\", \"Z\" or \" \")
%L    Level of subscribedness (integer)
%N    Number of unread articles (integer)
%I    Number of dormant articles (integer)
%i    Number of ticked and dormant (integer)
%T    Number of ticked articles (integer)
%R    Number of read articles (integer)
%U    Number of unseen articles (integer)
%t    Estimated total number of articles (integer)
%y    Number of unread, unticked articles (integer)
%G    Group name (string)
%g    Qualified group name (string)
%c    Short (collapsed) group name.  See `gnus-group-uncollapsed-levels'.
%C    Group comment (string)
%D    Group description (string)
%s    Select method (string)
%o    Moderated group (char, \"m\")
%p    Process mark (char)
%B    Whether a summary buffer for the group is open (char, \"*\")
%O    Moderated group (string, \"(m)\" or \"\")
%P    Topic indentation (string)
%m    Whether there is new(ish) mail in the group (char, \"%\")
%n    Select from where (string)
%z    A string that look like `<%s:%n>' if a foreign select method is used
%d    The date the group was last entered.
%E    Icon as defined by `gnus-group-icon-list'.
%F    The disk space used by the articles fetched by both the cache and agent.
%u    User defined specifier.  The next character in the format string should
      be a letter.  Gnus will call the function gnus-user-format-function-X,
      where X is the letter following %u.  The function will be passed a
      single dummy parameter as argument.  The function should return a
      string, which will be inserted into the buffer just like information
      from any other group specifier.

Note that this format specification is not always respected.  For
reasons of efficiency, when listing killed groups, this specification
is ignored altogether.  If the spec is changed considerably, your
output may end up looking strange when listing both alive and killed
groups.

If you use %o or %O, reading the active file will be slower and quite
a bit of extra memory will be used.  %D and %F will also worsen
performance.  Also note that if you change the format specification to
include any of these specs, you must probably re-start Gnus to see
them go into effect.

General format specifiers can also be used.
See Info node `(gnus)Formatting Variables'."
  :link '(custom-manual "(gnus)Formatting Variables")
  :group 'gnus-group-visual
  :type 'string)

(defcustom gnus-group-mode-line-format "Gnus: %%b {%M\%:%S}"
  "*The format specification for the group mode line.
It works along the same lines as a normal formatting string,
with some simple extensions:

%S   The native news server.
%M   The native select method.
%:   \":\" if %S isn't \"\"."
  :group 'gnus-group-visual
  :type 'string)

;; Extracted from gnus-xmas-redefine in order to preserve user settings
(when (featurep 'xemacs)
  (add-hook 'gnus-group-mode-hook 'gnus-xmas-group-menu-add)
  (add-hook 'gnus-group-mode-hook 'gnus-xmas-setup-group-toolbar))

(defcustom gnus-group-menu-hook nil
  "Hook run after the creation of the group mode menu."
  :group 'gnus-group-various
  :type 'hook)

(defcustom gnus-group-catchup-group-hook nil
  "Hook run when catching up a group from the group buffer."
  :group 'gnus-group-various
  :link '(custom-manual "(gnus)Group Data")
  :type 'hook)

(defcustom gnus-group-update-group-hook nil
  "Hook called when updating group lines."
  :group 'gnus-group-visual
  :type 'hook)

(defcustom gnus-group-prepare-function 'gnus-group-prepare-flat
  "*A function that is called to generate the group buffer.
The function is called with three arguments: The first is a number;
all group with a level less or equal to that number should be listed,
if the second is non-nil, empty groups should also be displayed.  If
the third is non-nil, it is a number.  No groups with a level lower
than this number should be displayed.

The only current function implemented is `gnus-group-prepare-flat'."
  :group 'gnus-group-listing
  :type 'function)

(defcustom gnus-group-prepare-hook nil
  "Hook called after the group buffer has been generated.
If you want to modify the group buffer, you can use this hook."
  :group 'gnus-group-listing
  :type 'hook)

(defcustom gnus-suspend-gnus-hook nil
  "Hook called when suspending (not exiting) Gnus."
  :group 'gnus-exit
  :type 'hook)

(defcustom gnus-exit-gnus-hook nil
  "Hook called when exiting Gnus."
  :group 'gnus-exit
  :type 'hook)

(defcustom gnus-after-exiting-gnus-hook nil
  "Hook called after exiting Gnus."
  :group 'gnus-exit
  :type 'hook)

(defcustom gnus-group-update-hook nil
  "Hook called when a group line is changed."
  :group 'gnus-group-visual
  :version "24.1"
  :type 'hook)

(defcustom gnus-useful-groups
  '(("(ding) mailing list mirrored at gmane.org"
     "gmane.emacs.gnus.general"
     (nntp "Gmane"
	   (nntp-address "news.gmane.org")))
    ("Gnus bug archive"
     "gnus.gnus-bug"
     (nntp "news.gnus.org"
	   (nntp-address "news.gnus.org")))
    ("Local Gnus help group"
     "gnus-help"
     (nndoc "gnus-help"
	    (nndoc-article-type mbox)
	    (eval `(nndoc-address
		    ,(let ((file (nnheader-find-etc-directory
				  "gnus-tut.txt" t)))
		       (unless file
			 (error "Couldn't find doc group"))
		       file))))))
  "*Alist of useful group-server pairs."
  :group 'gnus-group-listing
  :type '(repeat (list (string :tag "Description")
		       (string :tag "Name")
		       (sexp :tag "Method"))))

(defcustom gnus-group-highlight
  '(;; Mail.
    ((and mailp (= unread 0) (eq level 1)) .
     gnus-group-mail-1-empty)
    ((and mailp (eq level 1)) .
     gnus-group-mail-1)
    ((and mailp (= unread 0) (eq level 2)) .
     gnus-group-mail-2-empty)
    ((and mailp (eq level 2)) .
     gnus-group-mail-2)
    ((and mailp (= unread 0) (eq level 3)) .
     gnus-group-mail-3-empty)
    ((and mailp (eq level 3)) .
     gnus-group-mail-3)
    ((and mailp (= unread 0)) .
     gnus-group-mail-low-empty)
    ((and mailp) .
     gnus-group-mail-low)
    ;; News.
    ((and (= unread 0) (eq level 1)) .
     gnus-group-news-1-empty)
    ((and (eq level 1)) .
     gnus-group-news-1)
    ((and (= unread 0) (eq level 2)) .
     gnus-group-news-2-empty)
    ((and (eq level 2)) .
     gnus-group-news-2)
    ((and (= unread 0) (eq level 3)) .
     gnus-group-news-3-empty)
    ((and (eq level 3)) .
     gnus-group-news-3)
    ((and (= unread 0) (eq level 4)) .
     gnus-group-news-4-empty)
    ((and (eq level 4)) .
     gnus-group-news-4)
    ((and (= unread 0) (eq level 5)) .
     gnus-group-news-5-empty)
    ((and (eq level 5)) .
     gnus-group-news-5)
    ((and (= unread 0) (eq level 6)) .
     gnus-group-news-6-empty)
    ((and (eq level 6)) .
     gnus-group-news-6)
    ((and (= unread 0)) .
     gnus-group-news-low-empty)
    (t .
     gnus-group-news-low))
  "*Controls the highlighting of group buffer lines.

Below is a list of `Form'/`Face' pairs.  When deciding how a
particular group line should be displayed, each form is
evaluated.  The content of the face field after the first true form is
used.  You can change how those group lines are displayed by
editing the face field.

It is also possible to change and add form fields, but currently that
requires an understanding of Lisp expressions.  Hopefully this will
change in a future release.  For now, you can use the following
variables in the Lisp expression:

group: The name of the group.
unread: The number of unread articles in the group.
method: The select method used.
mailp: Whether it's a mail group or not.
level: The level of the group.
score: The score of the group.
ticked: The number of ticked articles."
  :group 'gnus-group-visual
  :type '(repeat (cons (sexp :tag "Form") face)))
(put 'gnus-group-highlight 'risky-local-variable t)

(defcustom gnus-new-mail-mark ?%
  "Mark used for groups with new mail."
  :group 'gnus-group-visual
  :type 'character)

(defgroup gnus-group-icons nil
  "Add Icons to your group buffer."
  :group 'gnus-group-visual)

(defcustom gnus-group-icon-list
  nil
  "*Controls the insertion of icons into group buffer lines.

Below is a list of `Form'/`File' pairs.  When deciding how a
particular group line should be displayed, each form is evaluated.
The icon from the file field after the first true form is used.  You
can change how those group lines are displayed by editing the file
field.  The File will either be found in the
`gnus-group-glyph-directory' or by designating absolute name of the
file.

It is also possible to change and add form fields, but currently that
requires an understanding of Lisp expressions.  Hopefully this will
change in a future release.  For now, you can use the following
variables in the Lisp expression:

group: The name of the group.
unread: The number of unread articles in the group.
method: The select method used.
mailp: Whether it's a mail group or not.
level: The level of the group.
score: The score of the group.
ticked: The number of ticked articles."
  :group 'gnus-group-icons
  :type '(repeat (cons (sexp :tag "Form") file)))
(put 'gnus-group-icon-list 'risky-local-variable t)

(defcustom gnus-group-name-charset-method-alist nil
  "Alist of method and the charset for group names.

For example:
    (((nntp \"news.com.cn\") . cn-gb-2312))"
  :version "21.1"
  :group 'gnus-charset
  :type '(repeat (cons (sexp :tag "Method") (symbol :tag "Charset"))))

(defcustom gnus-group-name-charset-group-alist
  (if (or (and (fboundp 'find-coding-system) (find-coding-system 'utf-8))
	  (mm-coding-system-p 'utf-8))
      '((".*" . utf-8))
    nil)
  "Alist of group regexp and the charset for group names.

For example:
    ((\"\\.com\\.cn:\" . cn-gb-2312))"
  :group 'gnus-charset
  :type '(repeat (cons (regexp :tag "Group") (symbol :tag "Charset"))))

(defcustom gnus-group-jump-to-group-prompt nil
  "Default prompt for `gnus-group-jump-to-group'.

If non-nil, the value should be a string or an alist.  If it is a string,
e.g. \"nnml:\", in which case `gnus-group-jump-to-group' offers \"Group:
nnml:\" in the minibuffer prompt.

If it is an alist, it must consist of \(NUMBER .  PROMPT\) pairs, for example:
\((1 .  \"\") (2 .  \"nnfolder+archive:\")).  The element with number 0 is
used when no prefix argument is given to `gnus-group-jump-to-group'."
  :version "22.1"
  :group 'gnus-group-various
  :type '(choice (string :tag "Prompt string")
		 (const :tag "Empty" nil)
		 (repeat (cons (integer :tag "Argument")
			       (string :tag "Prompt string")))))

(defvar gnus-group-listing-limit 1000
  "*A limit of the number of groups when listing.
If the number of groups is larger than the limit, list them in a
simple manner.")

;;; Internal variables

(defvar gnus-group-is-exiting-p nil)
(defvar gnus-group-is-exiting-without-update-p nil)
(defvar gnus-group-sort-alist-function 'gnus-group-sort-flat
  "Function for sorting the group buffer.")

(defvar gnus-group-sort-selected-function 'gnus-group-sort-selected-flat
  "Function for sorting the selected groups in the group buffer.")

(defvar gnus-group-indentation-function nil)
(defvar gnus-goto-missing-group-function nil)
(defvar gnus-group-update-group-function nil)
(defvar gnus-group-goto-next-group-function nil
  "Function to override finding the next group after listing groups.")

(defvar gnus-group-edit-buffer nil)

(defvar gnus-group-line-format-alist
  `((?M gnus-tmp-marked-mark ?c)
    (?S gnus-tmp-subscribed ?c)
    (?L gnus-tmp-level ?d)
    (?N (cond ((eq number t) "*" )
	      ((numberp number)
	       (int-to-string
		(+ number
		   (gnus-range-length (cdr (assq 'dormant gnus-tmp-marked)))
		   (gnus-range-length (cdr (assq 'tick gnus-tmp-marked))))))
	      (t number)) ?s)
    (?R gnus-tmp-number-of-read ?s)
    (?U (if (gnus-active gnus-tmp-group)
	    (gnus-number-of-unseen-articles-in-group gnus-tmp-group)
	  "*")
	?s)
    (?t gnus-tmp-number-total ?d)
    (?y gnus-tmp-number-of-unread ?s)
    (?I (gnus-range-length (cdr (assq 'dormant gnus-tmp-marked))) ?d)
    (?T (gnus-range-length (cdr (assq 'tick gnus-tmp-marked))) ?d)
    (?i (+ (gnus-range-length (cdr (assq 'dormant gnus-tmp-marked)))
	   (gnus-range-length (cdr (assq 'tick gnus-tmp-marked)))) ?d)
    (?g (if (boundp 'gnus-tmp-decoded-group)
	    gnus-tmp-decoded-group
	  gnus-tmp-group)
	?s)
    (?G gnus-tmp-qualified-group ?s)
    (?c (gnus-short-group-name (if (boundp 'gnus-tmp-decoded-group)
				   gnus-tmp-decoded-group
				 gnus-tmp-group))
	?s)
    (?C gnus-tmp-comment ?s)
    (?D gnus-tmp-newsgroup-description ?s)
    (?o gnus-tmp-moderated ?c)
    (?O gnus-tmp-moderated-string ?s)
    (?p gnus-tmp-process-marked ?c)
    (?s gnus-tmp-news-server ?s)
    (?n ,(if (featurep 'xemacs)
	     '(symbol-name gnus-tmp-news-method)
	   'gnus-tmp-news-method)
	?s)
    (?P gnus-group-indentation ?s)
    (?E gnus-tmp-group-icon ?s)
    (?B gnus-tmp-summary-live ?c)
    (?z gnus-tmp-news-method-string ?s)
    (?m (gnus-group-new-mail gnus-tmp-group) ?c)
    (?d (gnus-group-timestamp-string gnus-tmp-group) ?s)
    (?u gnus-tmp-user-defined ?s)
    (?F (gnus-total-fetched-for gnus-tmp-group) ?s)
    ))

(defvar gnus-group-mode-line-format-alist
  `((?S gnus-tmp-news-server ?s)
    (?M gnus-tmp-news-method ?s)
    (?u gnus-tmp-user-defined ?s)
    (?: gnus-tmp-colon ?s)))

(defvar gnus-topic-topology nil
  "The complete topic hierarchy.")

(defvar gnus-topic-alist nil
  "The complete topic-group alist.")

(defvar gnus-group-marked nil)

(defvar gnus-group-list-mode nil)


(defvar gnus-group-listed-groups nil)
(defvar gnus-group-list-option nil)

;;;
;;; Gnus group mode
;;;

(put 'gnus-group-mode 'mode-class 'special)

(gnus-define-keys gnus-group-mode-map
  " " gnus-group-read-group
  "=" gnus-group-select-group
  "\r" gnus-group-select-group
  "\M-\r" gnus-group-quick-select-group
  "\M- " gnus-group-visible-select-group
  [(meta control return)] gnus-group-select-group-ephemerally
  "j" gnus-group-jump-to-group
  "n" gnus-group-next-unread-group
  "p" gnus-group-prev-unread-group
  "\177" gnus-group-prev-unread-group
  [delete] gnus-group-prev-unread-group
  [backspace] gnus-group-prev-unread-group
  "N" gnus-group-next-group
  "P" gnus-group-prev-group
  "\M-n" gnus-group-next-unread-group-same-level
  "\M-p" gnus-group-prev-unread-group-same-level
  "," gnus-group-best-unread-group
  "." gnus-group-first-unread-group
  "u" gnus-group-unsubscribe-current-group
  "U" gnus-group-unsubscribe-group
  "c" gnus-group-catchup-current
  "C" gnus-group-catchup-current-all
  "\M-c" gnus-group-clear-data
  "l" gnus-group-list-groups
  "L" gnus-group-list-all-groups
  "m" gnus-group-mail
  "i" gnus-group-news
  "g" gnus-group-get-new-news
  "\M-g" gnus-group-get-new-news-this-group
  "R" gnus-group-restart
  "r" gnus-group-read-init-file
  "B" gnus-group-browse-foreign-server
  "b" gnus-group-check-bogus-groups
  "F" gnus-group-find-new-groups
  "\C-c\C-d" gnus-group-describe-group
  "\M-d" gnus-group-describe-all-groups
  "\C-c\C-a" gnus-group-apropos
  "\C-c\M-\C-a" gnus-group-description-apropos
  "a" gnus-group-post-news
  "\ek" gnus-group-edit-local-kill
  "\eK" gnus-group-edit-global-kill
  "\C-k" gnus-group-kill-group
  "\C-y" gnus-group-yank-group
  "\C-w" gnus-group-kill-region
  "\C-x\C-t" gnus-group-transpose-groups
  "\C-c\C-l" gnus-group-list-killed
  "\C-c\C-x" gnus-group-expire-articles
  "\C-c\M-\C-x" gnus-group-expire-all-groups
  "V" gnus-version
  "s" gnus-group-save-newsrc
  "z" gnus-group-suspend
  "q" gnus-group-exit
  "Q" gnus-group-quit
  "?" gnus-group-describe-briefly
  "\C-c\C-i" gnus-info-find-node
  "\M-e" gnus-group-edit-group-method
  "^" gnus-group-enter-server-mode
  gnus-mouse-2 gnus-mouse-pick-group
  [follow-link] mouse-face
  "<" beginning-of-buffer
  ">" end-of-buffer
  "\C-c\C-b" gnus-bug
  "\C-c\C-s" gnus-group-sort-groups
  "t" gnus-topic-mode
  "\C-c\M-g" gnus-activate-all-groups
  "\M-&" gnus-group-universal-argument
  "#" gnus-group-mark-group
  "\M-#" gnus-group-unmark-group)

(gnus-define-keys (gnus-group-mark-map "M" gnus-group-mode-map)
  "m" gnus-group-mark-group
  "u" gnus-group-unmark-group
  "w" gnus-group-mark-region
  "b" gnus-group-mark-buffer
  "r" gnus-group-mark-regexp
  "U" gnus-group-unmark-all-groups)

(gnus-define-keys (gnus-group-sieve-map "D" gnus-group-mode-map)
  "u" gnus-sieve-update
  "g" gnus-sieve-generate)

(gnus-define-keys (gnus-group-group-map "G" gnus-group-mode-map)
  "d" gnus-group-make-directory-group
  "h" gnus-group-make-help-group
  "u" gnus-group-make-useful-group
  "l" gnus-group-nnimap-edit-acl
  "m" gnus-group-make-group
  "E" gnus-group-edit-group
  "e" gnus-group-edit-group-method
  "p" gnus-group-edit-group-parameters
  "v" gnus-group-add-to-virtual
  "V" gnus-group-make-empty-virtual
  "D" gnus-group-enter-directory
  "f" gnus-group-make-doc-group
  "w" gnus-group-make-web-group
  "G" gnus-group-make-nnir-group
  "M" gnus-group-read-ephemeral-group
  "r" gnus-group-rename-group
  "R" gnus-group-make-rss-group
  "c" gnus-group-customize
  "z" gnus-group-compact-group
  "x" gnus-group-expunge-group
  "\177" gnus-group-delete-group
  [delete] gnus-group-delete-group)

(gnus-define-keys (gnus-group-sort-map "S" gnus-group-group-map)
  "s" gnus-group-sort-groups
  "a" gnus-group-sort-groups-by-alphabet
  "u" gnus-group-sort-groups-by-unread
  "l" gnus-group-sort-groups-by-level
  "v" gnus-group-sort-groups-by-score
  "r" gnus-group-sort-groups-by-rank
  "m" gnus-group-sort-groups-by-method
  "n" gnus-group-sort-groups-by-real-name)

(gnus-define-keys (gnus-group-sort-selected-map "P" gnus-group-group-map)
  "s" gnus-group-sort-selected-groups
  "a" gnus-group-sort-selected-groups-by-alphabet
  "u" gnus-group-sort-selected-groups-by-unread
  "l" gnus-group-sort-selected-groups-by-level
  "v" gnus-group-sort-selected-groups-by-score
  "r" gnus-group-sort-selected-groups-by-rank
  "m" gnus-group-sort-selected-groups-by-method
  "n" gnus-group-sort-selected-groups-by-real-name)

(gnus-define-keys (gnus-group-list-map "A" gnus-group-mode-map)
  "k" gnus-group-list-killed
  "z" gnus-group-list-zombies
  "s" gnus-group-list-groups
  "u" gnus-group-list-all-groups
  "A" gnus-group-list-active
  "a" gnus-group-apropos
  "d" gnus-group-description-apropos
  "m" gnus-group-list-matching
  "M" gnus-group-list-all-matching
  "l" gnus-group-list-level
  "c" gnus-group-list-cached
  "?" gnus-group-list-dormant
  "!" gnus-group-list-ticked)

(gnus-define-keys (gnus-group-list-limit-map "/" gnus-group-list-map)
  "k"  gnus-group-list-limit
  "z"  gnus-group-list-limit
  "s"  gnus-group-list-limit
  "u"  gnus-group-list-limit
  "A"  gnus-group-list-limit
  "m"  gnus-group-list-limit
  "M"  gnus-group-list-limit
  "l"  gnus-group-list-limit
  "c"  gnus-group-list-limit
  "?"  gnus-group-list-limit
  "!"  gnus-group-list-limit)

(gnus-define-keys (gnus-group-list-flush-map "f" gnus-group-list-map)
  "k"  gnus-group-list-flush
  "z"  gnus-group-list-flush
  "s"  gnus-group-list-flush
  "u"  gnus-group-list-flush
  "A"  gnus-group-list-flush
  "m"  gnus-group-list-flush
  "M"  gnus-group-list-flush
  "l"  gnus-group-list-flush
  "c"  gnus-group-list-flush
  "?"  gnus-group-list-flush
  "!"  gnus-group-list-flush)

(gnus-define-keys (gnus-group-list-plus-map "p" gnus-group-list-map)
  "k"  gnus-group-list-plus
  "z"  gnus-group-list-plus
  "s"  gnus-group-list-plus
  "u"  gnus-group-list-plus
  "A"  gnus-group-list-plus
  "m"  gnus-group-list-plus
  "M"  gnus-group-list-plus
  "l"  gnus-group-list-plus
  "c"  gnus-group-list-plus
  "?"  gnus-group-list-plus
  "!"  gnus-group-list-plus)

(gnus-define-keys (gnus-group-score-map "W" gnus-group-mode-map)
  "f" gnus-score-flush-cache
  "e" gnus-score-edit-all-score)

(gnus-define-keys (gnus-group-help-map "H" gnus-group-mode-map)
  "d" gnus-group-describe-group
  "v" gnus-version)

(gnus-define-keys (gnus-group-sub-map "S" gnus-group-mode-map)
  "l" gnus-group-set-current-level
  "t" gnus-group-unsubscribe-current-group
  "s" gnus-group-unsubscribe-group
  "k" gnus-group-kill-group
  "y" gnus-group-yank-group
  "w" gnus-group-kill-region
  "\C-k" gnus-group-kill-level
  "z" gnus-group-kill-all-zombies)

(defun gnus-topic-mode-p ()
  "Return non-nil in `gnus-topic-mode'."
  (and (boundp 'gnus-topic-mode)
       (symbol-value 'gnus-topic-mode)))

(defun gnus-group-make-menu-bar ()
  (unless (boundp 'gnus-group-reading-menu)

    (easy-menu-define
     gnus-group-reading-menu gnus-group-mode-map ""
     `("Group"
       ["Read" gnus-group-read-group
	:included (not (gnus-topic-mode-p))
	:active (gnus-group-group-name)]
       ["Read " gnus-topic-read-group
	:included (gnus-topic-mode-p)]
       ["Select" gnus-group-select-group
	:included (not (gnus-topic-mode-p))
	:active (gnus-group-group-name)]
       ["Select " gnus-topic-select-group
	:included (gnus-topic-mode-p)]
       ["See old articles" (gnus-group-select-group 'all)
	:keys "C-u SPC" :active (gnus-group-group-name)]
       ["Catch up" gnus-group-catchup-current
	:included (not (gnus-topic-mode-p))
	:active (gnus-group-group-name)
	,@(if (featurep 'xemacs) nil
	    '(:help "Mark unread articles in the current group as read"))]
       ["Catch up " gnus-topic-catchup-articles
	:included (gnus-topic-mode-p)
	,@(if (featurep 'xemacs) nil
	    '(:help "Mark unread articles in the current group or topic as read"))]
       ["Catch up all articles" gnus-group-catchup-current-all
	(gnus-group-group-name)]
       ["Check for new articles" gnus-group-get-new-news-this-group
	:included (not (gnus-topic-mode-p))
	:active (gnus-group-group-name)
	,@(if (featurep 'xemacs) nil
	    '(:help "Check for new messages in current group"))]
       ["Check for new articles " gnus-topic-get-new-news-this-topic
	:included (gnus-topic-mode-p)
	,@(if (featurep 'xemacs) nil
	    '(:help "Check for new messages in current group or topic"))]
       ["Toggle subscription" gnus-group-unsubscribe-current-group
	(gnus-group-group-name)]
       ["Kill" gnus-group-kill-group :active (gnus-group-group-name)
	,@(if (featurep 'xemacs) nil
	      '(:help "Kill (remove) current group"))]
       ["Yank" gnus-group-yank-group gnus-list-of-killed-groups]
       ["Describe" gnus-group-describe-group :active (gnus-group-group-name)
	,@(if (featurep 'xemacs) nil
	    '(:help "Display description of the current group"))]
       ;; Actually one should check, if any of the marked groups gives t for
       ;; (gnus-check-backend-function 'request-expire-articles ...)
       ["Expire articles" gnus-group-expire-articles
	:included (not (gnus-topic-mode-p))
	:active (or (and (gnus-group-group-name)
			 (gnus-check-backend-function
			  'request-expire-articles
			  (gnus-group-group-name))) gnus-group-marked)]
       ["Expire articles " gnus-topic-expire-articles
	:included (gnus-topic-mode-p)]
       ["Set group level..." gnus-group-set-current-level
	(gnus-group-group-name)]
       ["Select quick" gnus-group-quick-select-group (gnus-group-group-name)]
       ["Customize" gnus-group-customize (gnus-group-group-name)]
       ["Compact" gnus-group-compact-group
	:active (gnus-group-group-name)]
       ("Edit"
	["Parameters" gnus-group-edit-group-parameters
	 :included (not (gnus-topic-mode-p))
	 :active (gnus-group-group-name)]
	["Parameters " gnus-topic-edit-parameters
	 :included (gnus-topic-mode-p)]
	["Select method" gnus-group-edit-group-method
	 (gnus-group-group-name)]
	["Info" gnus-group-edit-group (gnus-group-group-name)]
	["Local kill file" gnus-group-edit-local-kill (gnus-group-group-name)]
	["Global kill file" gnus-group-edit-global-kill t])))

    (easy-menu-define
     gnus-group-group-menu gnus-group-mode-map ""
     '("Groups"
       ("Listing"
	["List unread subscribed groups" gnus-group-list-groups t]
	["List (un)subscribed groups" gnus-group-list-all-groups t]
	["List killed groups" gnus-group-list-killed gnus-killed-list]
	["List zombie groups" gnus-group-list-zombies gnus-zombie-list]
	["List level..." gnus-group-list-level t]
	["Describe all groups" gnus-group-describe-all-groups t]
	["Group apropos..." gnus-group-apropos t]
	["Group and description apropos..." gnus-group-description-apropos t]
	["List groups matching..." gnus-group-list-matching t]
	["List all groups matching..." gnus-group-list-all-matching t]
	["List active file" gnus-group-list-active t]
	["List groups with cached" gnus-group-list-cached t]
	["List groups with dormant" gnus-group-list-dormant t]
	["List groups with ticked" gnus-group-list-ticked t])
       ("Sort"
	["Default sort" gnus-group-sort-groups t]
	["Sort by method" gnus-group-sort-groups-by-method t]
	["Sort by rank" gnus-group-sort-groups-by-rank t]
	["Sort by score" gnus-group-sort-groups-by-score t]
	["Sort by level" gnus-group-sort-groups-by-level t]
	["Sort by unread" gnus-group-sort-groups-by-unread t]
	["Sort by name" gnus-group-sort-groups-by-alphabet t]
	["Sort by real name" gnus-group-sort-groups-by-real-name t])
       ("Sort process/prefixed"
	["Default sort" gnus-group-sort-selected-groups
	 (not (gnus-topic-mode-p))]
	["Sort by method" gnus-group-sort-selected-groups-by-method
	 (not (gnus-topic-mode-p))]
	["Sort by rank" gnus-group-sort-selected-groups-by-rank
	 (not (gnus-topic-mode-p))]
	["Sort by score" gnus-group-sort-selected-groups-by-score
	 (not (gnus-topic-mode-p))]
	["Sort by level" gnus-group-sort-selected-groups-by-level
	 (not (gnus-topic-mode-p))]
	["Sort by unread" gnus-group-sort-selected-groups-by-unread
	 (not (gnus-topic-mode-p))]
	["Sort by name" gnus-group-sort-selected-groups-by-alphabet
	 (not (gnus-topic-mode-p))]
	["Sort by real name" gnus-group-sort-selected-groups-by-real-name
	 (not (gnus-topic-mode-p))])
       ("Mark"
	["Mark group" gnus-group-mark-group
	 (and (gnus-group-group-name)
	      (not (memq (gnus-group-group-name) gnus-group-marked)))]
	["Unmark group" gnus-group-unmark-group
	 (and (gnus-group-group-name)
	      (memq (gnus-group-group-name) gnus-group-marked))]
	["Unmark all" gnus-group-unmark-all-groups gnus-group-marked]
	["Mark regexp..." gnus-group-mark-regexp t]
	["Mark region" gnus-group-mark-region :active (gnus-mark-active-p)]
	["Mark buffer" gnus-group-mark-buffer t]
	["Execute command" gnus-group-universal-argument
	 (or gnus-group-marked (gnus-group-group-name))])
       ("Subscribe"
	["Subscribe to a group..." gnus-group-unsubscribe-group t]
	["Kill all newsgroups in region" gnus-group-kill-region
	 :active (gnus-mark-active-p)]
	["Kill all zombie groups" gnus-group-kill-all-zombies
	 gnus-zombie-list]
	["Kill all groups on level..." gnus-group-kill-level t])
       ("Foreign groups"
	["Make a foreign group..." gnus-group-make-group t]
	["Add a directory group..." gnus-group-make-directory-group t]
	["Add the help group" gnus-group-make-help-group t]
	["Make a doc group..." gnus-group-make-doc-group t]
	["Make a web group..." gnus-group-make-web-group t]
	["Make a search group..." gnus-group-make-nnir-group t]
	["Make a virtual group..." gnus-group-make-empty-virtual t]
	["Add a group to a virtual..." gnus-group-add-to-virtual t]
	["Make an ephemeral group..." gnus-group-read-ephemeral-group t]
	["Make an RSS group..." gnus-group-make-rss-group t]
	["Rename group..." gnus-group-rename-group
	 (gnus-check-backend-function
	  'request-rename-group (gnus-group-group-name))]
	["Delete group" gnus-group-delete-group
	 (gnus-check-backend-function
	  'request-delete-group (gnus-group-group-name))])
       ("Move"
	["Next" gnus-group-next-group t]
	["Previous" gnus-group-prev-group t]
	["Next unread" gnus-group-next-unread-group t]
	["Previous unread" gnus-group-prev-unread-group t]
	["Next unread same level" gnus-group-next-unread-group-same-level t]
	["Previous unread same level"
	 gnus-group-prev-unread-group-same-level t]
	["Jump to group..." gnus-group-jump-to-group t]
	["First unread group" gnus-group-first-unread-group t]
	["Best unread group" gnus-group-best-unread-group t])
       ("Sieve"
	["Generate" gnus-sieve-generate t]
	["Generate and update" gnus-sieve-update t])
       ["Delete bogus groups" gnus-group-check-bogus-groups t]
       ["Find new newsgroups" gnus-group-find-new-groups t]
       ["Transpose" gnus-group-transpose-groups
	(gnus-group-group-name)]
       ["Read a directory as a group..." gnus-group-enter-directory t]))

    (easy-menu-define
     gnus-group-misc-menu gnus-group-mode-map ""
     `("Gnus"
       ["Send a mail" gnus-group-mail t]
       ["Send a message (mail or news)" gnus-group-post-news t]
       ["Create a local message" gnus-group-news t]
       ["Check for new news" gnus-group-get-new-news
	,@(if (featurep 'xemacs) '(t)
	    '(:help "Get newly arrived articles"))
	]
       ["Send queued messages" gnus-delay-send-queue
	,@(if (featurep 'xemacs) '(t)
	    '(:help "Send all messages that are scheduled to be sent now"))
	]
       ["Activate all groups" gnus-activate-all-groups t]
       ["Restart Gnus" gnus-group-restart t]
       ["Read init file" gnus-group-read-init-file t]
       ["Browse foreign server..." gnus-group-browse-foreign-server t]
       ["Enter server buffer" gnus-group-enter-server-mode t]
       ["Expire all expirable articles" gnus-group-expire-all-groups t]
       ["Gnus version" gnus-version t]
       ["Save .newsrc files" gnus-group-save-newsrc t]
       ["Suspend Gnus" gnus-group-suspend t]
       ["Clear dribble buffer" gnus-group-clear-dribble t]
       ["Read manual" gnus-info-find-node t]
       ["Flush score cache" gnus-score-flush-cache t]
       ["Toggle topics" gnus-topic-mode t]
       ["Send a bug report" gnus-bug t]
       ["Exit from Gnus" gnus-group-exit
	,@(if (featurep 'xemacs) '(t)
	    '(:help "Quit reading news"))]
       ["Exit without saving" gnus-group-quit t]))

    (gnus-run-hooks 'gnus-group-menu-hook)))


(defvar gnus-group-tool-bar-map nil)

(defun gnus-group-tool-bar-update (&optional symbol value)
  "Update group buffer toolbar.
Setter function for custom variables."
  (when symbol
    (set-default symbol value))
  ;; (setq-default gnus-group-tool-bar-map nil)
  ;; (use-local-map gnus-group-mode-map)
  (when (gnus-alive-p)
    (with-current-buffer gnus-group-buffer
      (gnus-group-make-tool-bar t))))

(defcustom gnus-group-tool-bar (if (eq gmm-tool-bar-style 'gnome)
				   'gnus-group-tool-bar-gnome
				 'gnus-group-tool-bar-retro)
  "Specifies the Gnus group tool bar.

It can be either a list or a symbol referring to a list.  See
`gmm-tool-bar-from-list' for the format of the list.  The
default key map is `gnus-group-mode-map'.

Pre-defined symbols include `gnus-group-tool-bar-gnome' and
`gnus-group-tool-bar-retro'."
  :type '(choice (const :tag "GNOME style" gnus-group-tool-bar-gnome)
		 (const :tag "Retro look" gnus-group-tool-bar-retro)
		 (repeat :tag "User defined list" gmm-tool-bar-item)
		 (symbol))
  :version "23.1" ;; No Gnus
  :initialize 'custom-initialize-default
  :set 'gnus-group-tool-bar-update
  :group 'gnus-group)

(defcustom gnus-group-tool-bar-gnome
  '((gnus-group-post-news "mail/compose")
    ;; Some useful agent icons?  I don't use the agent so agent users should
    ;; suggest useful commands:
    (gnus-agent-toggle-plugged "unplugged" t
			       :help "Gnus is currently unplugged.  Click to work online."
     			       :visible (and gnus-agent (not gnus-plugged)))
    (gnus-agent-toggle-plugged "plugged" t
			       :help "Gnus is currently plugged.  Click to work offline."
     			       :visible (and gnus-agent gnus-plugged))
    ;; FIXME: gnus-agent-toggle-plugged (in gnus-agent-group-make-menu-bar)
    ;; should have a better help text.
    (gnus-group-send-queue "mail/outbox" t
			   :visible (and gnus-agent gnus-plugged)
			   :help "Send articles from the queue group")
    (gnus-group-get-new-news "mail/inbox" nil
			     :visible (or (not gnus-agent)
					  gnus-plugged))
    ;; FIXME: gnus-*-read-group should have a better help text.
    (gnus-topic-read-group "open" nil
			   :visible (and (boundp 'gnus-topic-mode)
					 gnus-topic-mode))
    (gnus-group-read-group "open" nil
			   :visible (not (and (boundp 'gnus-topic-mode)
					      gnus-topic-mode)))
    ;; (gnus-group-find-new-groups "???" nil)
    (gnus-group-save-newsrc "save")
    (gnus-group-describe-group "describe")
    (gnus-group-unsubscribe-current-group "gnus/toggle-subscription")
    (gnus-group-prev-unread-group "left-arrow")
    (gnus-group-next-unread-group "right-arrow")
    (gnus-group-exit "exit")
    (gmm-customize-mode "preferences" t :help "Edit mode preferences")
    (gnus-info-find-node "help"))
  "List of functions for the group tool bar (GNOME style).

See `gmm-tool-bar-from-list' for the format of the list."
  :type '(repeat gmm-tool-bar-item)
  :version "23.1" ;; No Gnus
  :initialize 'custom-initialize-default
  :set 'gnus-group-tool-bar-update
  :group 'gnus-group)

(defcustom gnus-group-tool-bar-retro
  '((gnus-group-get-new-news "gnus/get-news")
    (gnus-group-get-new-news-this-group "gnus/gnntg")
    (gnus-group-catchup-current "gnus/catchup")
    (gnus-group-describe-group "gnus/describe-group")
    (gnus-group-subscribe "gnus/subscribe" t
			  :help "Subscribe to the current group")
    (gnus-group-unsubscribe "gnus/unsubscribe" t
			    :help "Unsubscribe from the current group")
    (gnus-group-exit "gnus/exit-gnus" gnus-group-mode-map))
  "List of functions for the group tool bar (retro look).

See `gmm-tool-bar-from-list' for the format of the list."
  :type '(repeat gmm-tool-bar-item)
  :version "23.1" ;; No Gnus
  :initialize 'custom-initialize-default
  :set 'gnus-group-tool-bar-update
  :group 'gnus-group)

(defcustom gnus-group-tool-bar-zap-list t
  "List of icon items from the global tool bar.
These items are not displayed in the Gnus group mode tool bar.

See `gmm-tool-bar-from-list' for the format of the list."
  :type 'gmm-tool-bar-zap-list
  :version "23.1" ;; No Gnus
  :initialize 'custom-initialize-default
  :set 'gnus-group-tool-bar-update
  :group 'gnus-group)

(defvar image-load-path)
(defvar tool-bar-map)

(defun gnus-group-make-tool-bar (&optional force)
  "Make a group mode tool bar from `gnus-group-tool-bar'.
When FORCE, rebuild the tool bar."
  (when (and (not (featurep 'xemacs))
	     (boundp 'tool-bar-mode)
	     tool-bar-mode
             (display-graphic-p)
	     (or (not gnus-group-tool-bar-map) force))
    (let* ((load-path
	    (gmm-image-load-path-for-library "gnus"
					     "gnus/toggle-subscription.xpm"
					     nil t))
           (image-load-path (cons (car load-path)
                                  (when (boundp 'image-load-path)
                                    image-load-path)))
	   (map (gmm-tool-bar-from-list gnus-group-tool-bar
					gnus-group-tool-bar-zap-list
					'gnus-group-mode-map)))
      (if map
	  (set (make-local-variable 'tool-bar-map) map))))
  gnus-group-tool-bar-map)

(defun gnus-group-mode ()
  "Major mode for reading news.

All normal editing commands are switched off.
\\<gnus-group-mode-map>
The group buffer lists (some of) the groups available.  For instance,
`\\[gnus-group-list-groups]' will list all subscribed groups with unread articles, while `\\[gnus-group-list-zombies]'
lists all zombie groups.

Groups that are displayed can be entered with `\\[gnus-group-read-group]'.  To subscribe
to a group not displayed, type `\\[gnus-group-unsubscribe-group]'.

For more in-depth information on this mode, read the manual (`\\[gnus-info-find-node]').

The following commands are available:

\\{gnus-group-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (when (gnus-visual-p 'group-menu 'menu)
    (gnus-group-make-menu-bar)
    (gnus-group-make-tool-bar))
  (gnus-simplify-mode-line)
  (setq major-mode 'gnus-group-mode)
  (setq mode-name "Group")
  (gnus-group-set-mode-line)
  (setq mode-line-process nil)
  (use-local-map gnus-group-mode-map)
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only t
	show-trailing-whitespace nil)
  (gnus-set-default-directory)
  (gnus-update-format-specifications nil 'group 'group-mode)
  (gnus-update-group-mark-positions)
  (when gnus-use-undo
    (gnus-undo-mode 1))
  (when gnus-slave
    (gnus-slave-mode))
  (gnus-run-mode-hooks 'gnus-group-mode-hook))

(defun gnus-update-group-mark-positions ()
  (save-excursion
    (let ((gnus-process-mark ?\200)
	  (gnus-group-update-hook nil)
	  (gnus-group-marked '("dummy.group"))
	  (gnus-active-hashtb (make-vector 10 0))
	  (topic ""))
      (gnus-set-active "dummy.group" '(0 . 0))
      (gnus-set-work-buffer)
      (gnus-group-insert-group-line "dummy.group" 0 nil 0 nil)
      (goto-char (point-min))
      (setq gnus-group-mark-positions
	    (list (cons 'process (and (search-forward
				       (mm-string-to-multibyte "\200") nil t)
				      (- (point) (point-min) 1))))))))

(defun gnus-mouse-pick-group (e)
  "Enter the group under the mouse pointer."
  (interactive "e")
  (mouse-set-point e)
  (gnus-group-read-group nil))

(defun gnus-group-default-list-level ()
  "Return the real value for `gnus-group-default-list-level'."
  (if (functionp gnus-group-default-list-level)
      (funcall gnus-group-default-list-level)
    gnus-group-default-list-level))

;; Look at LEVEL and find out what the level is really supposed to be.
;; If LEVEL is non-nil, LEVEL will be returned, if not, what happens
;; will depend on whether `gnus-group-use-permanent-levels' is used.
(defun gnus-group-default-level (&optional level number-or-nil)
  (cond
   (gnus-group-use-permanent-levels
    (or (setq gnus-group-use-permanent-levels
	      (or level (if (numberp gnus-group-use-permanent-levels)
			    gnus-group-use-permanent-levels
			  (or (gnus-group-default-list-level)
			      gnus-level-subscribed))))
	(gnus-group-default-list-level) gnus-level-subscribed))
   (number-or-nil
    level)
   (t
    (or level (gnus-group-default-list-level) gnus-level-subscribed))))

(defun gnus-group-setup-buffer ()
  (set-buffer (gnus-get-buffer-create gnus-group-buffer))
  (unless (eq major-mode 'gnus-group-mode)
    (gnus-group-mode)))

(defun gnus-group-name-charset (method group)
  (unless method
    (setq method (gnus-find-method-for-group group)))
  (when (stringp method)
    (setq method (gnus-server-to-method method)))
  (if (eq (car method) 'nnimap)
      ;; IMAP groups should not be encoded, since they do the encoding
      ;; in utf7 in the protocol.
      'utf-8
    (let ((item (or (assoc method gnus-group-name-charset-method-alist)
		    (and (consp method)
			 (assoc (list (car method) (cadr method))
				gnus-group-name-charset-method-alist))))
	  (alist gnus-group-name-charset-group-alist)
	  result)
      (if item
	  (cdr item)
	(while (setq item (pop alist))
	  (if (string-match (car item) group)
	      (setq alist nil
		    result (cdr item))))
	result))))

(defun gnus-group-name-decode (string charset)
  ;; Fixme: Don't decode in unibyte mode.
  (if (and string charset (featurep 'mule))
      (mm-decode-coding-string string charset)
    string))

(defun gnus-group-decoded-name (string)
  (let ((charset (gnus-group-name-charset nil string)))
    (gnus-group-name-decode string charset)))

(defun gnus-group-list-groups (&optional level unread lowest)
  "List newsgroups with level LEVEL or lower that have unread articles.
Default is all subscribed groups.
If argument UNREAD is non-nil, groups with no unread articles are also
listed.

Also see the `gnus-group-use-permanent-levels' variable."
  (interactive
   (list (if current-prefix-arg
	     (prefix-numeric-value current-prefix-arg)
	   (or
	    (gnus-group-default-level nil t)
	    (gnus-group-default-list-level)
	    gnus-level-subscribed))))
  (unless level
    (setq level (car gnus-group-list-mode)
	  unread (cdr gnus-group-list-mode)))
  (setq level (gnus-group-default-level level))
  (gnus-group-setup-buffer)
  (gnus-update-format-specifications nil 'group 'group-mode)
  (let ((case-fold-search nil)
	(props (text-properties-at (point-at-bol)))
	(empty (= (point-min) (point-max)))
	(group (gnus-group-group-name))
	number)
    (set-buffer gnus-group-buffer)
    (setq number (funcall gnus-group-prepare-function level unread lowest))
    (when (or (and (numberp number)
		   (zerop number))
	      (zerop (buffer-size)))
      ;; No groups in the buffer.
      (gnus-message 5 "%s" gnus-no-groups-message))
    ;; We have some groups displayed.
    (goto-char (point-max))
    (when (or (not gnus-group-goto-next-group-function)
	      (not (funcall gnus-group-goto-next-group-function
			    group props)))
      (cond
       (empty
	(goto-char (point-min)))
       ((not group)
	;; Go to the first group with unread articles.
	(gnus-group-search-forward t))
       (t
	;; Find the right group to put point on.  If the current group
	;; has disappeared in the new listing, try to find the next
	;; one.  If no next one can be found, just leave point at the
	;; first newsgroup in the buffer.
	(when (not (gnus-goto-char
		    (text-property-any
		     (point-min) (point-max)
		     'gnus-group (gnus-intern-safe
				  group gnus-active-hashtb))))
	  (let ((newsrc (cdddr (gnus-group-entry group))))
	    (while (and newsrc
			(not (gnus-goto-char
			      (text-property-any
			       (point-min) (point-max) 'gnus-group
			       (gnus-intern-safe
				(caar newsrc) gnus-active-hashtb)))))
	      (setq newsrc (cdr newsrc)))
	    (unless newsrc
	      (goto-char (point-max))
	      (forward-line -1)))))))
    ;; Adjust cursor point.
    (gnus-group-position-point)))

(defun gnus-group-list-level (level &optional all)
  "List groups on LEVEL.
If ALL (the prefix), also list groups that have no unread articles."
  (interactive "nList groups on level: \nP")
  (gnus-group-list-groups level all level))

(defun gnus-group-prepare-logic (group test)
  (or (and gnus-group-listed-groups
	   (null gnus-group-list-option)
	   (member group gnus-group-listed-groups))
      (cond
       ((null gnus-group-listed-groups) test)
       ((null gnus-group-list-option) test)
       (t (and (member group gnus-group-listed-groups)
	       (if (eq gnus-group-list-option 'flush)
		   (not test)
		 test))))))

(defun gnus-group-prepare-flat (level &optional predicate lowest regexp)
  "List all newsgroups with unread articles of level LEVEL or lower.
If PREDICATE is a function, list groups that the function returns non-nil;
if it is t, list groups that have no unread articles.
If LOWEST is non-nil, list all newsgroups of level LOWEST or higher.
If REGEXP is a function, list dead groups that the function returns non-nil;
if it is a string, only list groups matching REGEXP."
  (set-buffer gnus-group-buffer)
  (let ((buffer-read-only nil)
	(newsrc (cdr gnus-newsrc-alist))
	(lowest (or lowest 1))
	(not-in-list (and gnus-group-listed-groups
			  (copy-sequence gnus-group-listed-groups)))
	info clevel unread group params)
    (erase-buffer)
    (when (or (< lowest gnus-level-zombie)
	      gnus-group-listed-groups)
      ;; List living groups.
      (while newsrc
	(setq info (car newsrc)
	      group (gnus-info-group info)
	      params (gnus-info-params info)
	      newsrc (cdr newsrc)
	      unread (gnus-group-unread group))
	(when not-in-list
	  (setq not-in-list (delete group not-in-list)))
	(when (gnus-group-prepare-logic
	       group
	       (and (or unread		; This group might be unchecked
			predicate)	; Check if this group should be listed
		    (or (not (stringp regexp))
			(string-match regexp group))
		    (<= (setq clevel (gnus-info-level info)) level)
		    (>= clevel lowest)
		    (cond
		     ((functionp predicate)
		      (funcall predicate info))
		     (predicate t)	; We list all groups?
		     (t
		      (or
		       (if (eq unread t) ; Inactive?
			   gnus-group-list-inactive-groups
					; We list inactive
			 (and (numberp unread) (> unread 0)))
					; We list groups with unread articles
		       (and gnus-list-groups-with-ticked-articles
			    (cdr (assq 'tick (gnus-info-marks info))))
					; And groups with ticked articles
		       ;; Check for permanent visibility.
		       (and gnus-permanently-visible-groups
			    (string-match gnus-permanently-visible-groups
					  group))
		       (memq 'visible params)
		       (cdr (assq 'visible params)))))))
	  (gnus-group-insert-group-line
	   group (gnus-info-level info)
	   (gnus-info-marks info) unread (gnus-info-method info)))))

    ;; List dead groups.
    (when (or gnus-group-listed-groups
	      (and (>= level gnus-level-zombie)
		   (<= lowest gnus-level-zombie)))
      (gnus-group-prepare-flat-list-dead
       (setq gnus-zombie-list (sort gnus-zombie-list 'string<))
       gnus-level-zombie ?Z
       regexp))
    (when not-in-list
      (dolist (group gnus-zombie-list)
	(setq not-in-list (delete group not-in-list))))
    (when (or gnus-group-listed-groups
	      (and (>= level gnus-level-killed) (<= lowest gnus-level-killed)))
      (gnus-group-prepare-flat-list-dead
       (gnus-union
	not-in-list
	(setq gnus-killed-list (sort gnus-killed-list 'string<)))
       gnus-level-killed ?K regexp))

    (gnus-group-set-mode-line)
    (setq gnus-group-list-mode (cons level predicate))
    (gnus-run-hooks 'gnus-group-prepare-hook)
    t))

(defun gnus-group-prepare-flat-list-dead (groups level mark regexp)
  ;; List zombies and killed lists somewhat faster, which was
  ;; suggested by Jack Vinson <vinson@unagi.cis.upenn.edu>.  It does
  ;; this by ignoring the group format specification altogether.
  (let (group)
    (if (> (length groups) gnus-group-listing-limit)
	(while groups
	  (setq group (pop groups))
	  (when (gnus-group-prepare-logic
		 group
		 (or (not regexp)
		     (and (stringp regexp) (string-match regexp group))
		     (and (functionp regexp) (funcall regexp group))))
	    (gnus-add-text-properties
	     (point) (prog1 (1+ (point))
		       (insert " " mark "     *: "
			       (gnus-group-decoded-name group)
			       "\n"))
	     (list 'gnus-group (gnus-intern-safe group gnus-active-hashtb)
		   'gnus-unread t
		   'gnus-level level))))
      (while groups
	(setq group (pop groups))
	(when (gnus-group-prepare-logic
	       group
	       (or (not regexp)
		   (and (stringp regexp) (string-match regexp group))
		   (and (functionp regexp) (funcall regexp group))))
	  (gnus-group-insert-group-line
	   group level nil
	   (let ((active (gnus-active group)))
	     (if active
		 (if (zerop (cdr active))
		     0
		   (- (1+ (cdr active)) (car active)))
	       nil))
	   (gnus-method-simplify (gnus-find-method-for-group group))))))))

(defun gnus-group-update-group-line ()
  "Update the current line in the group buffer."
  (let* ((buffer-read-only nil)
	 (group (gnus-group-group-name))
	 (entry (and group (gnus-group-entry group)))
	 gnus-group-indentation)
    (when group
      (and entry
	   (not (gnus-ephemeral-group-p group))
	   (gnus-dribble-enter
	    (concat "(gnus-group-set-info '"
		    (gnus-prin1-to-string (nth 2 entry))
		    ")")
	    (concat "^(gnus-group-set-info '(\"" (regexp-quote group) "\"")))
      (setq gnus-group-indentation (gnus-group-group-indentation))
      (gnus-delete-line)
      (gnus-group-insert-group-line-info group)
      (forward-line -1)
      (gnus-group-position-point))))

(defun gnus-group-insert-group-line-info (group)
  "Insert GROUP on the current line."
  (let ((entry (gnus-group-entry group))
	(gnus-group-indentation (gnus-group-group-indentation))
	active info)
    (if entry
	(progn
	  ;; (Un)subscribed group.
	  (setq info (nth 2 entry))
	  (gnus-group-insert-group-line
	   group (gnus-info-level info) (gnus-info-marks info)
	   (or (car entry) t) (gnus-info-method info)))
      ;; This group is dead.
      (gnus-group-insert-group-line
       group
       (if (member group gnus-zombie-list) gnus-level-zombie gnus-level-killed)
       nil
       (if (setq active (gnus-active group))
	   (if (zerop (cdr active))
	       0
	     (- (1+ (cdr active)) (car active)))
	 nil)
       (gnus-method-simplify (gnus-find-method-for-group group))))))

(defun gnus-number-of-unseen-articles-in-group (group)
  (let* ((info (nth 2 (gnus-group-entry group)))
	 (marked (gnus-info-marks info))
	 (seen (cdr (assq 'seen marked)))
	 (active (gnus-active group)))
    (if (not active)
	0
      (length (gnus-uncompress-range
	       (gnus-range-difference
		(gnus-range-difference (list active) (gnus-info-read info))
		seen))))))

;; Moving through the Group buffer (in topic mode) e.g. with C-n doesn't
;; update the state (enabled/disabled) of the icon `gnus-group-describe-group'
;; automatically.  After `C-l' the state is correct.  See the following report
;; on emacs-devel
;; <http://thread.gmane.org/v9acdmrcse.fsf@marauder.physik.uni-ulm.de>:
;; From: Reiner Steib
;; Subject: tool bar icons not updated according to :active condition
;; Newsgroups: gmane.emacs.devel
;; Date: Mon, 23 Jan 2006 19:59:13 +0100
;; Message-ID: <v9acdmrcse.fsf@marauder.physik.uni-ulm.de>

(defcustom gnus-group-update-tool-bar
  (and (not (featurep 'xemacs))
       (boundp 'tool-bar-mode)
       tool-bar-mode
       ;; Using `redraw-frame' (see `gnus-tool-bar-update') in Emacs might
       ;; be confusing, so maybe we shouldn't call it by default.
       (fboundp 'force-window-update))
  "Force updating the group buffer tool bar."
  :group 'gnus-group
  :version "22.1"
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (when (gnus-alive-p)
	   (with-current-buffer gnus-group-buffer
	     ;; FIXME: Is there a better way to redraw the group buffer?
	     (gnus-group-get-new-news 0))))
  :type 'boolean)

(defun gnus-group-insert-group-line (gnus-tmp-group gnus-tmp-level
						    gnus-tmp-marked number
						    gnus-tmp-method)
  "Insert a group line in the group buffer."
  (let* ((gnus-tmp-method
	  (gnus-server-get-method gnus-tmp-group gnus-tmp-method))
	 (group-name-charset (gnus-group-name-charset gnus-tmp-method
						      gnus-tmp-group))
	 (gnus-tmp-active (gnus-active gnus-tmp-group))
	 (gnus-tmp-number-total
	  (if gnus-tmp-active
	      (1+ (- (cdr gnus-tmp-active) (car gnus-tmp-active)))
	    0))
	 (gnus-tmp-number-of-unread
	  (if (numberp number) (int-to-string (max 0 number))
	    "*"))
	 (gnus-tmp-number-of-read
	  (if (numberp number)
	      (int-to-string (max 0 (- gnus-tmp-number-total number)))
	    "*"))
	 (gnus-tmp-subscribed
	  (cond ((<= gnus-tmp-level gnus-level-subscribed) ? )
		((<= gnus-tmp-level gnus-level-unsubscribed) ?U)
		((= gnus-tmp-level gnus-level-zombie) ?Z)
		(t ?K)))
	 (gnus-tmp-qualified-group
	  (gnus-group-name-decode (gnus-group-real-name gnus-tmp-group)
				  group-name-charset))
	 (gnus-tmp-comment
	  (or (gnus-group-get-parameter gnus-tmp-group 'comment t)
	      gnus-tmp-group))
	 (gnus-tmp-newsgroup-description
	  (if gnus-description-hashtb
	      (or (gnus-group-name-decode
		   (gnus-gethash gnus-tmp-group gnus-description-hashtb)
		   group-name-charset) "")
	    ""))
	 (gnus-tmp-moderated
	  (if (and gnus-moderated-hashtb
		   (gnus-gethash gnus-tmp-group gnus-moderated-hashtb))
	      ?m ? ))
	 (gnus-tmp-moderated-string
	  (if (eq gnus-tmp-moderated ?m) "(m)" ""))
         (gnus-tmp-group-icon (gnus-group-get-icon gnus-tmp-group))
	 (gnus-tmp-news-server (or (cadr gnus-tmp-method) ""))
	 (gnus-tmp-news-method (or (car gnus-tmp-method) ""))
	 (gnus-tmp-news-method-string
	  (if gnus-tmp-method
	      (format "(%s:%s)" (car gnus-tmp-method)
		      (cadr gnus-tmp-method)) ""))
	 (gnus-tmp-marked-mark
	  (if (and (numberp number)
		   (zerop number)
		   (cdr (assq 'tick gnus-tmp-marked)))
	      ?* ? ))
	 (gnus-tmp-summary-live
	  (if (and (not gnus-group-is-exiting-p)
		   (gnus-buffer-live-p (gnus-summary-buffer-name
					gnus-tmp-group)))
	      ?* ? ))
	 (gnus-tmp-process-marked
	  (if (member gnus-tmp-group gnus-group-marked)
	      gnus-process-mark ? ))
	 (buffer-read-only nil)
	 beg end
	 header gnus-tmp-header)	; passed as parameter to user-funcs.
    (beginning-of-line)
    (setq beg (point))
    (gnus-add-text-properties
     (point)
     (prog1 (1+ (point))
       ;; Insert the text.
       (let ((gnus-tmp-decoded-group (gnus-group-name-decode
				      gnus-tmp-group group-name-charset)))
	 (eval gnus-group-line-format-spec)))
     `(gnus-group ,(gnus-intern-safe gnus-tmp-group gnus-active-hashtb)
		  gnus-unread ,(if (numberp number)
				   (string-to-number gnus-tmp-number-of-unread)
				 t)
		  gnus-marked ,gnus-tmp-marked-mark
		  gnus-indentation ,gnus-group-indentation
		  gnus-level ,gnus-tmp-level))
    (setq end (point))
    (when gnus-group-update-tool-bar
      (gnus-put-text-property beg end 'point-entered
			      'gnus-tool-bar-update)
      (gnus-put-text-property beg end 'point-left
			      'gnus-tool-bar-update))
    (forward-line -1)
    (when (inline (gnus-visual-p 'group-highlight 'highlight))
      (gnus-group-highlight-line gnus-tmp-group beg end))
    (gnus-run-hooks 'gnus-group-update-hook)
    (forward-line)))

(defun gnus-group-update-eval-form (group list)
  "Eval `car' of each element of LIST, and return the first that return t.
Some value are bound so the form can use them."
  (when list
    (let* ((entry (gnus-group-entry group))
           (unread (if (numberp (car entry)) (car entry) 0))
           (active (gnus-active group))
           (total (if active (1+ (- (cdr active) (car active))) 0))
           (info (nth 2 entry))
           (method (inline (gnus-server-get-method group (gnus-info-method info))))
           (marked (gnus-info-marks info))
           (mailp (apply 'append
                         (mapcar
                          (lambda (x)
                            (memq x (assoc (symbol-name
                                            (car (or method gnus-select-method)))
                                           gnus-valid-select-methods)))
                          '(mail post-mail))))
           (level (or (gnus-info-level info) gnus-level-killed))
           (score (or (gnus-info-score info) 0))
           (ticked (gnus-range-length (cdr (assq 'tick marked))))
           (group-age (gnus-group-timestamp-delta group)))
      ;; FIXME: http://thread.gmane.org/gmane.emacs.gnus.general/65451/focus=65465
      ;; ======================================================================
      ;; From: Richard Stallman
      ;; Subject: Re: Rewriting gnus-group-highlight-line (was: [...])
      ;; Cc: ding@gnus.org
      ;; Date: Sat, 27 Oct 2007 19:41:20 -0400
      ;; Message-ID: <E1IlvHM-0006TS-7t@fencepost.gnu.org>
      ;;
      ;; [...]
      ;; The kludge is that the alist elements contain expressions that refer
      ;; to local variables with short names.  Perhaps write your own tiny
      ;; evaluator that handles just `and', `or', and numeric comparisons
      ;; and just a few specific variables.
      ;; ======================================================================
      ;;
      ;; Similar for other evaluated variables.  Grep for risky-local-variable
      ;; to find them!  -- rsteib
      ;;
      ;; Eval the cars of the lists until we find a match.
      (while (and list
                  (not (eval (caar list))))
        (setq list (cdr list)))
      list)))

(defun gnus-group-highlight-line (group beg end)
  "Highlight the current line according to `gnus-group-highlight'.
GROUP is current group, and the line to highlight starts at BEG
and ends at END."
  (let ((face (cdar (gnus-group-update-eval-form
                      group
                      gnus-group-highlight))))
    (unless (eq face (get-text-property beg 'face))
      (let ((inhibit-read-only t))
        (gnus-put-text-property-excluding-characters-with-faces
         beg end 'face
         (if (boundp face) (symbol-value face) face)))
      (gnus-extent-start-open beg))))

(defun gnus-group-get-icon (group)
  "Return an icon for GROUP according to `gnus-group-icon-list'."
  (if gnus-group-icon-list
      (let ((image-path
             (cdar (gnus-group-update-eval-form group gnus-group-icon-list))))
        (if image-path
            (propertize " "
                        'display
                        (append
                         (gnus-create-image (expand-file-name image-path))
                         '(:ascent center)))
          " "))
    " "))


(defun gnus-group-refresh-group (group)
  (gnus-activate-group group)
  (gnus-get-unread-articles-in-group (gnus-get-info group)
				     (gnus-active group))
  (gnus-group-update-group group))

(defun gnus-group-update-group (group &optional visible-only
				      info-unchanged)
  "Update all lines where GROUP appear.
If VISIBLE-ONLY is non-nil, the group won't be displayed if it isn't
already.  If INFO-UNCHANGED is non-nil, dribble buffer is not updated."
  (with-current-buffer gnus-group-buffer
    (save-excursion
      ;; The buffer may be narrowed.
      (save-restriction
        (widen)
        (let ((ident (gnus-intern-safe group gnus-active-hashtb))
              (loc (point-min))
              found buffer-read-only)
	  (unless info-unchanged
	    ;; Enter the current status into the dribble buffer.
	    (let ((entry (gnus-group-entry group)))
	      (when (and entry
			 (not (gnus-ephemeral-group-p group)))
		(gnus-dribble-enter
		 (concat "(gnus-group-set-info '"
			 (gnus-prin1-to-string (nth 2 entry))
			 ")")
		 (concat "^(gnus-group-set-info '(\""
			 (regexp-quote group) "\"")))))
          ;; Find all group instances.  If topics are in use, each group
          ;; may be listed in more than once.
          (while (setq loc (text-property-any
                            loc (point-max) 'gnus-group ident))
            (setq found t)
            (goto-char loc)
            (let ((gnus-group-indentation (gnus-group-group-indentation)))
              (gnus-delete-line)
              (gnus-group-insert-group-line-info group)
              (save-excursion
                (forward-line -1)
                (gnus-run-hooks 'gnus-group-update-group-hook)))
            (setq loc (1+ loc)))
          (unless (or found visible-only)
            ;; No such line in the buffer, find out where it's supposed to
            ;; go, and insert it there (or at the end of the buffer).
            (if gnus-goto-missing-group-function
                (funcall gnus-goto-missing-group-function group)
              (let ((entry (cddr (gnus-group-entry group))))
                (while (and entry (car entry)
                            (not
                             (gnus-goto-char
                              (text-property-any
                               (point-min) (point-max)
                               'gnus-group (gnus-intern-safe
                                            (caar entry)
                                            gnus-active-hashtb)))))
                  (setq entry (cdr entry)))
                (or entry (goto-char (point-max)))))
            ;; Finally insert the line.
            (let ((gnus-group-indentation (gnus-group-group-indentation)))
              (gnus-group-insert-group-line-info group)
              (save-excursion
                (forward-line -1)
                (gnus-run-hooks 'gnus-group-update-group-hook))))
          (when gnus-group-update-group-function
            (funcall gnus-group-update-group-function group))
          (gnus-group-set-mode-line))))))

(defun gnus-group-set-mode-line ()
  "Update the mode line in the group buffer."
  (when (memq 'group gnus-updated-mode-lines)
    ;; Yes, we want to keep this mode line updated.
    (with-current-buffer gnus-group-buffer
      (let* ((gformat (or gnus-group-mode-line-format-spec
			  (gnus-set-format 'group-mode)))
	     (gnus-tmp-news-server (cadr gnus-select-method))
	     (gnus-tmp-news-method (car gnus-select-method))
	     (gnus-tmp-colon (if (equal gnus-tmp-news-server "") "" ":"))
	     (max-len 60)
	     gnus-tmp-header		;Dummy binding for user-defined formats
	     ;; Get the resulting string.
	     (modified
	      (and gnus-dribble-buffer
		   (buffer-name gnus-dribble-buffer)
		   (buffer-modified-p gnus-dribble-buffer)
		   (with-current-buffer gnus-dribble-buffer
		     (not (zerop (buffer-size))))))
	     (mode-string (eval gformat)))
	;; Say whether the dribble buffer has been modified.
	(setq mode-line-modified
	      (if modified (car gnus-mode-line-modified)
		(cdr gnus-mode-line-modified)))
	;; If the line is too long, we chop it off.
	(when (> (length mode-string) max-len)
	  (setq mode-string (substring mode-string 0 (- max-len 4))))
	(prog1
	    (setq mode-line-buffer-identification
		  (gnus-mode-line-buffer-identification
		   (list mode-string)))
	  (set-buffer-modified-p modified))))))

(defun gnus-group-group-name ()
  "Get the name of the newsgroup on the current line."
  (let ((group (get-text-property (point-at-bol) 'gnus-group)))
    (when group
      (symbol-name group))))

(defun gnus-group-group-level ()
  "Get the level of the newsgroup on the current line."
  (get-text-property (point-at-bol) 'gnus-level))

(defun gnus-group-group-indentation ()
  "Get the indentation of the newsgroup on the current line."
  (or (get-text-property (point-at-bol) 'gnus-indentation)
      (and gnus-group-indentation-function
	   (funcall gnus-group-indentation-function))
      ""))

(defun gnus-group-group-unread ()
  "Get the number of unread articles of the newsgroup on the current line."
  (get-text-property (point-at-bol) 'gnus-unread))

(defun gnus-group-new-mail (group)
  (if (nnmail-new-mail-p (gnus-group-real-name group))
      gnus-new-mail-mark
    ? ))

(defun gnus-group-level (group)
  "Return the estimated level of GROUP."
  (or (gnus-info-level (gnus-get-info group))
      (and (member group gnus-zombie-list) gnus-level-zombie)
      gnus-level-killed))

(defun gnus-group-search-forward (&optional backward all level first-too)
  "Find the next newsgroup with unread articles.
If BACKWARD is non-nil, find the previous newsgroup instead.
If ALL is non-nil, just find any newsgroup.
If LEVEL is non-nil, find group with level LEVEL, or higher if no such
group exists.
If FIRST-TOO, the current line is also eligible as a target."
  (let ((way (if backward -1 1))
	(low gnus-level-killed)
	(beg (point))
	pos found lev)
    (if (and backward (progn (beginning-of-line)) (bobp))
	nil
      (unless first-too
	(forward-line way))
      (while (and
	      (not (eobp))
	      (not (setq
		    found
		    (and
		     (get-text-property (point) 'gnus-group)
		     (or all
			 (and
			  (let ((unread
				 (get-text-property (point) 'gnus-unread)))
			    (and (numberp unread) (> unread 0)))
			  (setq lev (get-text-property (point)
						       'gnus-level))
			  (<= lev gnus-level-subscribed)))
		     (or (not level)
			 (and (setq lev (get-text-property (point)
							   'gnus-level))
			      (or (= lev level)
				  (and (< lev low)
				       (< level lev)
				       (progn
					 (setq low lev)
					 (setq pos (point))
					 nil))))))))
	      (zerop (forward-line way)))))
    (if found
	(progn (gnus-group-position-point) t)
      (goto-char (or pos beg))
      (and pos t))))

(defun gnus-total-fetched-for (group)
  (let* ((size-in-cache (or (gnus-cache-total-fetched-for group) 0))
	 (size-in-agent (or (gnus-agent-total-fetched-for group) 0))
	 (size (+ size-in-cache size-in-agent))
	 (suffix '("B" "K" "M" "G"))
	 (scale 1024.0)
	 (cutoff scale))
    (while (> size cutoff)
      (setq size (/ size scale)
	    suffix (cdr suffix)))
    (format "%5.1f%s" size (car suffix))))

;;; Gnus group mode commands

;; Group marking.

(defun gnus-group-mark-line-p ()
  (save-excursion
    (beginning-of-line)
    (forward-char (or (cdr (assq 'process gnus-group-mark-positions)) 2))
    (eq (char-after) gnus-process-mark)))

(defun gnus-group-mark-group (n &optional unmark no-advance)
  "Mark the current group."
  (interactive "p")
  (let ((buffer-read-only nil)
	group)
    (while (and (> n 0)
		(not (eobp)))
      (when (setq group (gnus-group-group-name))
	;; Go to the mark position.
	(beginning-of-line)
	(forward-char (or (cdr (assq 'process gnus-group-mark-positions)) 2))
	(delete-char 1)
	(if unmark
	    (progn
	      (setq gnus-group-marked (delete group gnus-group-marked))
	      (insert-char ? 1 t))
	   (setq gnus-group-marked
		 (cons group (delete group gnus-group-marked)))
	   (insert-char gnus-process-mark 1 t)))
      (unless no-advance
	(gnus-group-next-group 1))
      (decf n))
    (gnus-group-position-point)
    n))

(defun gnus-group-unmark-group (n)
  "Remove the mark from the current group."
  (interactive "p")
  (gnus-group-mark-group n 'unmark)
  (gnus-group-position-point))

(defun gnus-group-unmark-all-groups ()
  "Unmark all groups."
  (interactive)
  (save-excursion
    (mapc 'gnus-group-remove-mark gnus-group-marked))
  (gnus-group-position-point))

(defun gnus-group-mark-region (unmark beg end)
  "Mark all groups between point and mark.
If UNMARK, remove the mark instead."
  (interactive "P\nr")
  (let ((num (count-lines beg end)))
    (save-excursion
      (goto-char beg)
      (- num (gnus-group-mark-group num unmark)))))

(defun gnus-group-mark-buffer (&optional unmark)
  "Mark all groups in the buffer.
If UNMARK, remove the mark instead."
  (interactive "P")
  (gnus-group-mark-region unmark (point-min) (point-max)))

(defun gnus-group-mark-regexp (regexp)
  "Mark all groups that match some regexp."
  (interactive "sMark (regexp): ")
  (let ((alist (cdr gnus-newsrc-alist))
	group)
    (save-excursion
      (while alist
	(when (string-match regexp (setq group (gnus-info-group (pop alist))))
	  (gnus-group-jump-to-group group)
	  (gnus-group-set-mark group)))))
  (gnus-group-position-point))

(defun gnus-group-remove-mark (group &optional test-marked)
  "Remove the process mark from GROUP and move point there.
Return nil if the group isn't displayed."
  (if (gnus-group-goto-group group nil test-marked)
      (save-excursion
	(gnus-group-mark-group 1 'unmark t)
	t)
    (setq gnus-group-marked
	  (delete group gnus-group-marked))
    nil))

(defun gnus-group-set-mark (group)
  "Set the process mark on GROUP."
  (if (gnus-group-goto-group group)
      (save-excursion
	(gnus-group-mark-group 1 nil t))
    (setq gnus-group-marked (cons group (delete group gnus-group-marked)))))

(defun gnus-group-universal-argument (arg &optional groups func)
  "Perform any command on all groups according to the process/prefix convention."
  (interactive "P")
  (if (eq (setq func (or func
			 (key-binding
			  (read-key-sequence
			   (substitute-command-keys
			    "\\<gnus-group-mode-map>\\[gnus-group-universal-argument]")))))
	  'undefined)
      (gnus-error 1 "Undefined key")
    (gnus-group-iterate arg
      (lambda (group)
	(command-execute func))))
  (gnus-group-position-point))

(defun gnus-group-process-prefix (n)
  "Return a list of groups to work on.
Take into consideration N (the prefix) and the list of marked groups."
  (cond
   (n
    (setq n (prefix-numeric-value n))
    ;; There is a prefix, so we return a list of the N next
    ;; groups.
    (let ((way (if (< n 0) -1 1))
	  (n (abs n))
	  group groups)
      (save-excursion
	(while (> n 0)
	  (if (setq group (gnus-group-group-name))
	      (push group groups))
	  (setq n (1- n))
	  (gnus-group-next-group way)))
      (nreverse groups)))
   ((and (gnus-region-active-p) (mark))
    ;; Work on the region between point and mark.
    (let ((max (max (point) (mark)))
	  groups)
      (save-excursion
	(goto-char (min (point) (mark)))
	(while
	    (and
	     (push (gnus-group-group-name) groups)
	     (zerop (gnus-group-next-group 1))
	     (< (point) max)))
	(nreverse groups))))
   (gnus-group-marked
    ;; No prefix, but a list of marked articles.
    (reverse gnus-group-marked))
   (t
    ;; Neither marked articles or a prefix, so we return the
    ;; current group.
    (let ((group (gnus-group-group-name)))
      (and group (list group))))))

;;; !!!Surely gnus-group-iterate should be a macro instead?  I can't
;;; imagine why I went through these contortions...
(eval-and-compile
  (let ((function (make-symbol "gnus-group-iterate-function"))
	(window (make-symbol "gnus-group-iterate-window"))
	(groups (make-symbol "gnus-group-iterate-groups"))
	(group (make-symbol "gnus-group-iterate-group")))
    (eval
     `(defun gnus-group-iterate (arg ,function)
	"Iterate FUNCTION over all process/prefixed groups.
FUNCTION will be called with the group name as the parameter
and with point over the group in question."
	(let ((,groups (gnus-group-process-prefix arg))
	      (,window (selected-window))
	      ,group)
	  (while ,groups
	    (setq ,group (car ,groups)
		  ,groups (cdr ,groups))
	    (select-window ,window)
	    (gnus-group-remove-mark ,group)
	    (save-selected-window
	      (save-excursion
		(funcall ,function ,group)))))))))

(put 'gnus-group-iterate 'lisp-indent-function 1)

;; Selecting groups.

(defun gnus-group-read-group (&optional all no-article group select-articles)
  "Read news in this newsgroup.
If the prefix argument ALL is non-nil, already read articles become
readable.

If ALL is a positive number, fetch this number of the latest
articles in the group.  If ALL is a negative number, fetch this
number of the earliest articles in the group.

If the optional argument NO-ARTICLE is non-nil, no article will
be auto-selected upon group entry.  If GROUP is non-nil, fetch
that group."
  (interactive "P")
  (let ((no-display (eq all 0))
	(group (or group (gnus-group-group-name)))
	number active marked entry)
    (when (eq all 0)
      (setq all nil))
    (unless group
      (error "No group on current line"))
    (setq marked (gnus-info-marks
		  (nth 2 (setq entry (gnus-group-entry group)))))
    ;; This group might be a dead group.  In that case we have to get
    ;; the number of unread articles from `gnus-active-hashtb'.
    (setq number
	  (cond ((numberp all) all)
		(entry (car entry))
		((setq active (gnus-active group))
		 (- (1+ (cdr active)) (car active)))))
    (gnus-summary-read-group
     group (or all (and (numberp number)
			(zerop (+ number (gnus-range-length
					  (cdr (assq 'tick marked)))
				  (gnus-range-length
				   (cdr (assq 'dormant marked)))))))
     no-article nil no-display nil select-articles)))

(defun gnus-group-select-group (&optional all)
  "Select this newsgroup.
No article is selected automatically.
If the group is opened, just switch the summary buffer.
If ALL is non-nil, already read articles become readable.
If ALL is a positive number, fetch this number of the latest
articles in the group.
If ALL is a negative number, fetch this number of the earliest
articles in the group."
  (interactive "P")
  (when (and (eobp) (not (gnus-group-group-name)))
    (forward-line -1))
  (gnus-group-read-group all t))

(defun gnus-group-quick-select-group (&optional all group)
  "Select the GROUP \"quickly\".
This means that no highlighting or scoring will be performed.  If
ALL (the prefix argument) is 0, don't even generate the summary
buffer.  If GROUP is nil, use current group.

This might be useful if you want to toggle threading
before entering the group."
  (interactive "P")
  (require 'gnus-score)
  (let (gnus-visual
	gnus-score-find-score-files-function
	gnus-home-score-file
	gnus-apply-kill-hook
	gnus-summary-expunge-below)
    (gnus-group-read-group all t group)))

(defun gnus-group-visible-select-group (&optional all)
  "Select the current group without hiding any articles."
  (interactive "P")
  (let ((gnus-inhibit-limiting t))
    (gnus-group-read-group all t)))

(defun gnus-group-select-group-ephemerally ()
  "Select the current group without doing any processing whatsoever.
You will actually be entered into a group that's a copy of
the current group; no changes you make while in this group will
be permanent."
  (interactive)
  (require 'gnus-score)
  (let* (gnus-visual
	 gnus-score-find-score-files-function gnus-apply-kill-hook
	 gnus-summary-expunge-below gnus-show-threads gnus-suppress-duplicates
	 gnus-summary-mode-hook gnus-select-group-hook
	 (group (gnus-group-group-name))
	 (method (gnus-find-method-for-group group)))
    (gnus-group-read-ephemeral-group
     (gnus-group-prefixed-name group method) method)))

(defun gnus-group-name-at-point ()
  "Return a group name from around point if it exists, or nil."
  (if (eq major-mode 'gnus-group-mode)
      (let ((group (gnus-group-group-name)))
	(when group
	  (gnus-group-decoded-name group)))
    (let ((regexp "[][\C-@-\t\v-*,/:-@\\^`{-\C-?]*\
\\(nn[a-z]+\\(?:\\+[^][\C-@-*,/:-@\\^`{-\C-?]+\\)?:\
\[^][\C-@-*,./:-@\\^`{-\C-?]+\\(?:\\.[^][\C-@-*,./:-@\\^`{-\C-?]+\\)*\
\\|[^][\C-@-*,./:-@\\^`{-\C-?]+\\(?:\\.[^][\C-@-*,./:-@\\^`{-\C-?]+\\)+\\)")
	  (start (point))
	  (case-fold-search nil))
      (prog1
	  (if (or (and (not (or (eobp)
				(looking-at "[][\C-@-*,/;-@\\^`{-\C-?]")))
		       (prog1 t
			 (skip-chars-backward "^][\C-@-\t\v-*,/;-@\\^`{-\C-?"
					      (point-at-bol))))
		  (and (looking-at "[][\C-@-\t\v-*,/;-@\\^`{-\C-?]*$")
		       (prog1 t
			 (skip-chars-backward "][\C-@-\t\v-*,/;-@\\^`{-\C-?")
			 (skip-chars-backward "^][\C-@-\t\v-*,/;-@\\^`{-\C-?"
					      (point-at-bol))))
		  (string-match "\\`[][\C-@-\t\v-*,/;-@\\^`{-\C-?]*\\'"
				(buffer-substring (point-at-bol) (point))))
	      (when (looking-at regexp)
		(match-string 1))
	    (let (group distance)
	      (when (looking-at regexp)
		(setq group (match-string 1)
		      distance (- (match-beginning 1) (match-beginning 0))))
	      (skip-chars-backward "][\C-@-\t\v-*,/;-@\\^`{-\C-?")
	      (skip-chars-backward "^][\C-@-\t\v-*,/;-@\\^`{-\C-?"
				   (point-at-bol))
	      (if (looking-at regexp)
		  (if (and group (<= distance (- start (match-end 0))))
		      group
		    (match-string 1))
		group)))
	(goto-char start)))))

(defun gnus-group-completing-read (&optional prompt collection
					     require-match initial-input hist
					     def)
  "Read a group name with completion.  Non-ASCII group names are allowed.
The arguments are the same as `completing-read' except that COLLECTION
and HIST default to `gnus-active-hashtb' and `gnus-group-history'
respectively if they are omitted.  Regards COLLECTION as a hash table
if it is not a list."
  (or collection (setq collection gnus-active-hashtb))
  (let (choices group)
    (if (listp collection)
	(dolist (symbol collection)
	  (setq group (symbol-name symbol))
	  (push (if (string-match "[^\000-\177]" group)
		    (gnus-group-decoded-name group)
		  group)
		choices))
      (mapatoms (lambda (symbol)
		  (setq group (symbol-name symbol))
		  (push (if (string-match "[^\000-\177]" group)
			    (gnus-group-decoded-name group)
			  group)
			choices))
		collection))
    (setq group (gnus-completing-read (or prompt "Group") (nreverse choices)
				      require-match initial-input
				      (or hist 'gnus-group-history)
				      def))
    (unless (if (listp collection)
		(member group (mapcar 'symbol-name collection))
	      (symbol-value (intern-soft group collection)))
      (setq group
	    (mm-encode-coding-string
	     group (gnus-group-name-charset nil group))))
    (gnus-replace-in-string group "\n" "")))

;;;###autoload
(defun gnus-fetch-group (group &optional articles)
  "Start Gnus if necessary and enter GROUP.
If ARTICLES, display those articles.
Returns whether the fetching was successful or not."
  (interactive (list (gnus-group-completing-read nil
						 nil nil
						 (gnus-group-name-at-point))))
  (unless (gnus-alive-p)
    (gnus-no-server))
  (gnus-group-read-group (if articles nil t) nil group articles))

;;;###autoload
(defun gnus-fetch-group-other-frame (group)
  "Pop up a frame and enter GROUP."
  (interactive "P")
  (let ((window (get-buffer-window gnus-group-buffer)))
    (cond (window
	   (select-frame (window-frame window)))
	  ((= (length (frame-list)) 1)
	   (select-frame (make-frame)))
	  (t
	   (other-frame 1))))
  (gnus-fetch-group group))

(defcustom gnus-large-ephemeral-newsgroup 200
  "The number of articles which indicates a large ephemeral newsgroup.
Same as `gnus-large-newsgroup', but only used for ephemeral newsgroups.

If the number of articles in a newsgroup is greater than this value,
confirmation is required for selecting the newsgroup.  If it is nil, no
confirmation is required."
  :version "22.1"
  :group 'gnus-group-select
  :type '(choice (const :tag "No limit" nil)
		 integer))

(defcustom gnus-fetch-old-ephemeral-headers nil
  "Same as `gnus-fetch-old-headers', but only used for ephemeral newsgroups."
  :version "22.1"
  :group 'gnus-thread
  :type '(choice (const :tag "off" nil)
		 (const some)
		 number
		 (sexp :menu-tag "other" t)))

;; Enter a group that is not in the group buffer.  Non-nil is returned
;; if selection was successful.
(defun gnus-group-read-ephemeral-group (group method &optional activate
					      quit-config request-only
					      select-articles
					      parameters
					      number)
  "Read GROUP from METHOD as an ephemeral group.
If ACTIVATE, request the group first.
If QUIT-CONFIG, use that Gnus window configuration name when
exiting from the ephemeral group.
If REQUEST-ONLY, don't actually read the group; just request it.
If SELECT-ARTICLES, only select those articles.
If PARAMETERS, use those as the group parameters.
If NUMBER, fetch this number of articles.

Return the name of the group if selection was successful."
  (interactive
   (list
    ;; (gnus-read-group "Group name: ")
    (gnus-group-completing-read)
    (gnus-read-method "From method")))
  ;; Transform the select method into a unique server.
  (unless (gnus-alive-p)
    (gnus-no-server))
  (when (stringp method)
    (setq method (gnus-server-to-method method)))
  (let ((address-slot
	 (intern (format "%s-address" (car method)))))
    (setq method
	  (if (assq address-slot (cddr method))
	      `(,(car method) ,(concat (cadr method) "-ephemeral")
		,@(cddr method))
	    `(,(car method) ,(concat (cadr method) "-ephemeral")
	      (,address-slot ,(cadr method))
	      ,@(cddr method)))))
  (let ((group (if (gnus-group-foreign-p group) group
		 (gnus-group-prefixed-name (gnus-group-real-name group)
					   method))))
    (gnus-sethash
     group
     `(-1 nil (,group
	       ,gnus-level-default-subscribed nil nil ,method
	       ,(cons
		 (cond
		  (quit-config
		   (cons 'quit-config quit-config))
		  ((assq gnus-current-window-configuration
			 gnus-buffer-configuration)
		   (cons 'quit-config
			 (cons gnus-summary-buffer
			       gnus-current-window-configuration))))
		 parameters)))
     gnus-newsrc-hashtb)
    (push method gnus-ephemeral-servers)
    (set-buffer gnus-group-buffer)
    (unless (gnus-check-server method)
      (error "Unable to contact server: %s" (gnus-status-message method)))
    (when activate
      (gnus-activate-group group 'scan)
      (unless (gnus-request-group group)
	(error "Couldn't request group: %s"
	       (nnheader-get-report (car method)))))
    (if request-only
	group
      (condition-case ()
	  (when (let ((gnus-large-newsgroup gnus-large-ephemeral-newsgroup)
		      (gnus-fetch-old-headers
		       gnus-fetch-old-ephemeral-headers))
		  (gnus-group-read-group (or number t) t group select-articles))
	    group)
	(quit
	 (if debug-on-quit
	     (debug "Quit")
	   (message "Quit reading the ephemeral group"))
	 nil)))))

(defcustom gnus-gmane-group-download-format
  "http://download.gmane.org/%s/%s/%s"
  "URL for downloading mbox files.
It must contain three \"%s\".  They correspond to the group, the
minimal and maximal article numbers, respectively."
  :group 'gnus-group-foreign
  :version "23.1" ;; No Gnus
  :type 'string)

(autoload 'url-insert-file-contents "url-handlers")
;; FIXME:
;; - Add documentation, menu, key bindings, ...

(defun gnus-read-ephemeral-gmane-group (group start &optional range)
  "Read articles from Gmane group GROUP as an ephemeral group.
START is the first article.  RANGE specifies how many articles
are fetched.  The articles are downloaded via HTTP using the URL
specified by `gnus-gmane-group-download-format'."
  ;; See <http://gmane.org/export.php> for more information.
  (interactive
   (list
    (gnus-group-completing-read "Gmane group")
    (read-number "Start article number: ")
    (read-number "How many articles: ")))
  (unless range (setq range 500))
  (when (< range 1)
    (error "Invalid range: %s" range))
  (let ((tmpfile (mm-make-temp-file
		  (format "%s.start-%s.range-%s." group start range)))
	(gnus-thread-sort-functions '(gnus-thread-sort-by-number)))
    (with-temp-file tmpfile
      (url-insert-file-contents
       (format gnus-gmane-group-download-format
	       group start (+ start range)))
      (write-region (point-min) (point-max) tmpfile)
      (gnus-group-read-ephemeral-group
       (format "%s.start-%s.range-%s" group start range)
       `(nndoc ,tmpfile
	       (nndoc-article-type mbox))))
    (delete-file tmpfile)))

(defun gnus-read-ephemeral-gmane-group-url (url)
  "Create an ephemeral Gmane group from URL.

Valid input formats include:
\"http://thread.gmane.org/gmane.foo.bar/12300/focus=12399\",
\"http://thread.gmane.org/gmane.foo.bar/12345/\",
\"http://article.gmane.org/gmane.foo.bar/12345/\",
\"http://news.gmane.org/group/gmane.foo.bar/thread=12345\""
  ;; - Feel free to add other useful Gmane URLs here!  Maybe the URLs should
  ;;   be customizable?
  ;; - The URLs should be added to `gnus-button-alist'.  Probably we should
  ;;   prompt the user to decide: "View via `browse-url' or in Gnus? "
  ;;   (`gnus-read-ephemeral-gmane-group-url')
  (interactive
   (list (gnus-group-completing-read "Gmane URL")))
  (let (group start range)
    (cond
     ;; URLs providing `group', `start' and `range':
     ((string-match
       ;; http://thread.gmane.org/gmane.emacs.devel/86326/focus=86525
       "^http://thread\.gmane\.org/\\([^/]+\\)/\\([0-9]+\\)/focus=\\([0-9]+\\)$"
       url)
      (setq group (match-string 1 url)
	    start (string-to-number (match-string 2 url))
	    ;; Ensure that `range' is large enough to ensure focus article is
	    ;; included.
	    range (- (string-to-number (match-string 3 url))
		     start -1)))
     ;; URLs providing `group' and `start':
     ((or (string-match
	   ;; http://article.gmane.org/gmane.comp.gnu.make.bugs/3584
	   "^http://\\(?:thread\\|article\\|permalink\\)\.gmane\.org/\\([^/]+\\)/\\([0-9]+\\)"
	   url)
	  (string-match
	   ;; Don't advertise these in the doc string yet:
	   "^\\(?:nntp\\|news\\)://news\.gmane\.org/\\([^/]+\\)/\\([0-9]+\\)"
	   url)
	  (string-match
	   ;; http://news.gmane.org/group/gmane.emacs.gnus.general/thread=65099/force_load=t
	   "^http://news\.gmane\.org/group/\\([^/]+\\)/thread=\\([0-9]+\\)"
	   url))
      (setq group (match-string 1 url)
	    start (string-to-number (match-string 2 url))))
     (t
      (error "Can't parse URL %s" url)))
    (gnus-read-ephemeral-gmane-group group start range)))

(defcustom gnus-bug-group-download-format-alist
  '((emacs . "http://debbugs.gnu.org/%s;mboxmaint=yes;mboxstat=yes")
    (debian
     . "http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=%s&mbox=yes;mboxmaint=yes"))
  "Alist of symbols for bug trackers and the corresponding URL format string.
The URL format string must contain a single \"%s\", specifying
the bug number, and browsing the URL must return mbox output."
  :group 'gnus-group-foreign
  ;; Added mboxmaint=yes.  This gets the version with the messages as
  ;; they went out, not as they came in.
  ;; Eg bug-gnu-emacs is replaced by ###@debbugs.
  :version "24.1"
  :type '(repeat (cons (symbol) (string :tag "URL format string"))))

(defun gnus-read-ephemeral-bug-group (ids mbox-url &optional window-conf)
  "Browse bug NUMBER as ephemeral group."
  (interactive (list (read-string "Enter bug number: "
				  (thing-at-point 'word) nil)
		     ;; FIXME: Add completing-read from
		     ;; `gnus-emacs-bug-group-download-format' ...
		     (cdr (assoc 'emacs gnus-bug-group-download-format-alist))))
  (when (stringp ids)
    (setq ids (string-to-number ids)))
  (unless (listp ids)
    (setq ids (list ids)))
  (let ((tmpfile (mm-make-temp-file "gnus-temp-group-"))
	(coding-system-for-write 'binary)
	(coding-system-for-read 'binary))
    (with-temp-file tmpfile
      (dolist (id ids)
	(url-insert-file-contents (format mbox-url id)))
      (goto-char (point-min))
      ;; Add the debbugs address so that we can respond to reports easily.
      (while (re-search-forward "^To: " nil t)
	(end-of-line)
	(insert (format ", %s@%s" (car ids)
			(gnus-replace-in-string
			 (gnus-replace-in-string mbox-url "^http://" "")
			 "/.*$" ""))))
      (write-region (point-min) (point-max) tmpfile)
      (gnus-group-read-ephemeral-group
       "gnus-read-ephemeral-bug"
       `(nndoc ,tmpfile
	       (nndoc-article-type mbox))
       nil window-conf))
    (delete-file tmpfile)))

(defun gnus-read-ephemeral-debian-bug-group (number)
  "Browse Debian bug NUMBER as ephemeral group."
  (interactive (list (read-string "Enter bug number: "
				  (thing-at-point 'word) nil)))
  (gnus-read-ephemeral-bug-group
   number
   (cdr (assoc 'debian gnus-bug-group-download-format-alist))))

(defvar debbugs-gnu-bug-number)		; debbugs-gnu

(defun gnus-read-ephemeral-emacs-bug-group (ids &optional window-conf)
  "Browse Emacs bugs IDS as an ephemeral group."
  (interactive (list (string-to-number
		      (read-string "Enter bug number: "
				   (thing-at-point 'word) nil))))
  (unless (listp ids)
    (setq ids (list ids)))
  (gnus-read-ephemeral-bug-group
   ids
   (cdr (assoc 'emacs gnus-bug-group-download-format-alist))
   window-conf)
  (when (fboundp 'debbugs-gnu-summary-mode)
    (with-current-buffer (window-buffer (selected-window))
      (debbugs-gnu-summary-mode 1)
      (set (make-local-variable 'debbugs-gnu-bug-number) (car ids)))))

(defun gnus-group-jump-to-group (group &optional prompt)
  "Jump to newsgroup GROUP.

If PROMPT (the prefix) is a number, use the prompt specified in
`gnus-group-jump-to-group-prompt'."
  (interactive
   (list (gnus-group-completing-read
          nil nil nil
          (if current-prefix-arg
              (cdr (assq current-prefix-arg gnus-group-jump-to-group-prompt))
            (or (and (stringp gnus-group-jump-to-group-prompt)
                     gnus-group-jump-to-group-prompt)
                (let ((p (cdr (assq 0 gnus-group-jump-to-group-prompt))))
                  (and (stringp p) p)))))))

  (when (equal group "")
    (error "Empty group name"))

  (unless (gnus-ephemeral-group-p group)
    ;; Either go to the line in the group buffer...
    (unless (gnus-group-goto-group group)
      ;; ... or insert the line.
      (gnus-group-update-group group)
      (gnus-group-goto-group group)))
  ;; Adjust cursor point.
  (gnus-group-position-point))

(defun gnus-group-goto-group (group &optional far test-marked)
  "Goto to newsgroup GROUP.
If FAR, it is likely that the group is not on the current line.
If TEST-MARKED, the line must be marked."
  (when group
    (beginning-of-line)
    (cond
     ;; It's quite likely that we are on the right line, so
     ;; we check the current line first.
     ((and (not far)
	   (eq (get-text-property (point) 'gnus-group)
	       (gnus-intern-safe group gnus-active-hashtb))
	   (or (not test-marked) (gnus-group-mark-line-p)))
      (point))
     ;; Previous and next line are also likely, so we check them as well.
     ((and (not far)
	   (save-excursion
	     (forward-line -1)
	     (and (eq (get-text-property (point) 'gnus-group)
		      (gnus-intern-safe group gnus-active-hashtb))
		  (or (not test-marked) (gnus-group-mark-line-p)))))
      (forward-line -1)
      (point))
     ((and (not far)
	   (save-excursion
	     (forward-line 1)
	     (and (eq (get-text-property (point) 'gnus-group)
		      (gnus-intern-safe group gnus-active-hashtb))
		  (or (not test-marked) (gnus-group-mark-line-p)))))
      (forward-line 1)
      (point))
     (test-marked
      (goto-char (point-min))
      (let (found)
	(while (and (not found)
		    (gnus-goto-char
		     (text-property-any
		      (point) (point-max)
		      'gnus-group
		      (gnus-intern-safe group gnus-active-hashtb))))
	  (if (gnus-group-mark-line-p)
	      (setq found t)
	    (forward-line 1)))
	found))
     (t
      ;; Search through the entire buffer.
      (gnus-goto-char
       (text-property-any
	(point-min) (point-max)
	'gnus-group (gnus-intern-safe group gnus-active-hashtb)))))))

(defun gnus-group-next-group (n &optional silent)
  "Go to next N'th newsgroup.
If N is negative, search backward instead.
Returns the difference between N and the number of skips actually
done."
  (interactive "p")
  (gnus-group-next-unread-group n t nil silent))

(defun gnus-group-next-unread-group (n &optional all level silent)
  "Go to next N'th unread newsgroup.
If N is negative, search backward instead.
If ALL is non-nil, choose any newsgroup, unread or not.
If LEVEL is non-nil, choose the next group with level LEVEL, or, if no
such group can be found, the next group with a level higher than
LEVEL.
Returns the difference between N and the number of skips actually
made."
  (interactive "p")
  (let ((backward (< n 0))
	(n (abs n)))
    (while (and (> n 0)
		(gnus-group-search-forward
		 backward (or (not gnus-group-goto-unread) all) level))
      (setq n (1- n)))
    (when (and (/= 0 n)
	       (not silent))
      (gnus-message 7 "No more%s newsgroups%s" (if all "" " unread")
		    (if level " on this level or higher" "")))
    n))

(defun gnus-group-prev-group (n)
  "Go to previous N'th newsgroup.
Returns the difference between N and the number of skips actually
done."
  (interactive "p")
  (gnus-group-next-unread-group (- n) t))

(defun gnus-group-prev-unread-group (n)
  "Go to previous N'th unread newsgroup.
Returns the difference between N and the number of skips actually
done."
  (interactive "p")
  (gnus-group-next-unread-group (- n)))

(defun gnus-group-next-unread-group-same-level (n)
  "Go to next N'th unread newsgroup on the same level.
If N is negative, search backward instead.
Returns the difference between N and the number of skips actually
done."
  (interactive "p")
  (gnus-group-next-unread-group n t (gnus-group-group-level))
  (gnus-group-position-point))

(defun gnus-group-prev-unread-group-same-level (n)
  "Go to next N'th unread newsgroup on the same level.
Returns the difference between N and the number of skips actually
done."
  (interactive "p")
  (gnus-group-next-unread-group (- n) t (gnus-group-group-level))
  (gnus-group-position-point))

(defun gnus-group-best-unread-group (&optional exclude-group)
  "Go to the group with the highest level.
If EXCLUDE-GROUP, do not go to that group."
  (interactive)
  (goto-char (point-min))
  (let ((best 100000)
	unread best-point)
    (while (not (eobp))
      (setq unread (get-text-property (point) 'gnus-unread))
      (when (and (numberp unread) (> unread 0))
	(when (and (get-text-property (point) 'gnus-level)
		   (< (get-text-property (point) 'gnus-level) best)
		   (or (not exclude-group)
		       (not (equal exclude-group (gnus-group-group-name)))))
	  (setq best (get-text-property (point) 'gnus-level))
	  (setq best-point (point))))
      (forward-line 1))
    (when best-point
      (goto-char best-point))
    (gnus-group-position-point)
    (and best-point (gnus-group-group-name))))

;; Is there something like an after-point-motion-hook?
;; (inhibit-point-motion-hooks?).  Is there a tool-bar-update function?

;; (defun gnus-group-menu-bar-update ()
;;   (let* ((buf (list (with-current-buffer gnus-group-buffer
;; 		      (current-buffer))))
;; 	 (name (buffer-name (car buf))))
;;     (setcdr buf
;; 	    (if (> (length name) 27)
;; 		(concat (substring name 0 12)
;; 			"..."
;; 			(substring name -12))
;; 	      name))
;;     (menu-bar-update-buffers-1 buf)))

;; (defun gnus-group-position-point ()
;;   (gnus-goto-colon)
;;   (gnus-group-menu-bar-update))

(defun gnus-group-first-unread-group ()
  "Go to the first group with unread articles."
  (interactive)
  (prog1
      (let ((opoint (point))
	    unread)
	(goto-char (point-min))
	(if (or (eq (setq unread (gnus-group-group-unread)) t) ; Not active.
		(and (numberp unread)	; Not a topic.
		     (not (zerop unread))) ; Has unread articles.
		(zerop (gnus-group-next-unread-group 1))) ; Next unread group.
	    (point)			; Success.
	  (goto-char opoint)
	  nil))				; Not success.
    (gnus-group-position-point)))

(defun gnus-group-enter-server-mode ()
  "Jump to the server buffer."
  (interactive)
  (gnus-enter-server-buffer))

(defun gnus-group-make-group-simple (&optional group)
  "Add a new newsgroup.
The user will be prompted for GROUP."
  (interactive (list (gnus-group-completing-read)))
  (gnus-group-make-group (gnus-group-real-name group)
			 (gnus-group-server group)
			 nil nil t))

(defun gnus-group-make-group (name &optional method address args encoded)
  "Add a new newsgroup.
The user will be prompted for a NAME, for a select METHOD, and an
ADDRESS.  NAME should be a human-readable string (i.e., not be encoded
even if it contains non-ASCII characters) unless ENCODED is non-nil.

If the backend supports it, the group will also be created on the
server."
  (interactive
   (list
    (gnus-read-group "Group name: ")
    (gnus-read-method "From method")))

  (when (stringp method)
    (setq method (or (gnus-server-to-method method) method)))
  (unless encoded
    (setq name (mm-encode-coding-string
		name
		(gnus-group-name-charset method name))))
  (let* ((meth (gnus-method-simplify
		(when (and method
			   (not (gnus-server-equal method gnus-select-method)))
		  (if address (list (intern method) address)
		    method))))
	 (nname (if method (gnus-group-prefixed-name name meth) name))
	 backend info)
    (when (gnus-group-entry nname)
      (error "Group %s already exists" (gnus-group-decoded-name nname)))
    ;; Subscribe to the new group.
    (gnus-group-change-level
     (setq info (list t nname gnus-level-default-subscribed nil nil meth))
     gnus-level-default-subscribed gnus-level-killed
     (and (gnus-group-group-name)
	  (gnus-group-entry (gnus-group-group-name)))
     t)
    ;; Make it active.
    (gnus-set-active nname (cons 1 0))
    (unless (gnus-ephemeral-group-p name)
      (gnus-dribble-enter
       (concat "(gnus-group-set-info '"
	       (gnus-prin1-to-string (cdr info)) ")")
       (concat "^(gnus-group-set-info '(\"" (regexp-quote name) "\"")))
    ;; Insert the line.
    (gnus-group-insert-group-line-info nname)
    (forward-line -1)
    (gnus-group-position-point)

    ;; Load the back end and try to make the back end create
    ;; the group as well.
    (when (assoc (symbol-name (setq backend (car (gnus-server-get-method
						  nil meth))))
		 gnus-valid-select-methods)
      (require backend))
    (gnus-check-server meth)
    (when (gnus-check-backend-function 'request-create-group nname)
      (unless (gnus-request-create-group nname nil args)
	(error "Could not create group on server: %s"
	       (nnheader-get-report backend))))
    t))

(defun gnus-group-delete-groups (&optional arg)
  "Delete the current group.  Only meaningful with editable groups."
  (interactive "P")
  (let ((n (length (gnus-group-process-prefix arg))))
    (when (gnus-yes-or-no-p
	   (if (= n 1)
	       "Delete this 1 group? "
	     (format "Delete these %d groups? " n)))
      (gnus-group-iterate arg
	(lambda (group)
	  (gnus-group-delete-group group nil t))))))

(defun gnus-group-delete-articles (group)
  "Delete all articles in the current group."
  (interactive (list (gnus-group-group-name)))
  (let ((articles (gnus-uncompress-range (gnus-active group))))
    (when (gnus-yes-or-no-p
	   (format "Do you really want to delete these %d articles forever? "
		   (length articles)))
      (gnus-request-expire-articles articles group 'force))))

(defun gnus-group-delete-group (group &optional force no-prompt)
  "Delete the current group.  Only meaningful with editable groups.
If FORCE (the prefix) is non-nil, all the articles in the group will
be deleted.  This is \"deleted\" as in \"removed forever from the face
of the Earth\".  There is no undo.  The user will be prompted before
doing the deletion.
Note that you also have to specify FORCE if you want the group to
be removed from the server, even when it's empty."
  (interactive
   (list (gnus-group-group-name)
	 current-prefix-arg))
  (unless group
    (error "No group to delete"))
  (unless (gnus-check-backend-function 'request-delete-group group)
    (error "This back end does not support group deletion"))
  (prog1
      (let ((group-decoded (gnus-group-decoded-name group)))
	(if (and (not no-prompt)
		 (not (gnus-yes-or-no-p
		       (format
			"Do you really want to delete %s%s? "
			group-decoded (if force " and all its contents" "")))))
	    ()				; Whew!
	  (gnus-message 6 "Deleting group %s..." group-decoded)
	  (if (not (gnus-request-delete-group group force))
	      (gnus-error 3 "Couldn't delete group %s" group-decoded)
	    (gnus-message 6 "Deleting group %s...done" group-decoded)
	    (gnus-group-goto-group group)
	    (gnus-group-kill-group 1 t)
	    (gnus-set-active group nil)
	    t)))
    (gnus-group-position-point)))

(defun gnus-group-rename-group (group new-name)
  "Rename group from GROUP to NEW-NAME.
When used interactively, GROUP is the group under point
and NEW-NAME will be prompted for."
  (interactive
   (let ((group (gnus-group-group-name))
	 method new-name)
     (unless (gnus-check-backend-function 'request-rename-group group)
       (error "This back end does not support renaming groups"))
     (setq new-name (gnus-read-group
		     "Rename group to: "
		     (gnus-group-real-name (gnus-group-decoded-name group)))
	   method (gnus-info-method (gnus-get-info group)))
     (list group (mm-encode-coding-string
		  new-name
		  (gnus-group-name-charset
		   method
		   (gnus-group-prefixed-name new-name method))))))

  (unless (gnus-check-backend-function 'request-rename-group group)
    (error "This back end does not support renaming groups"))
  (unless group
    (error "No group to rename"))
  (when (equal (gnus-group-real-name group) new-name)
    (error "Can't rename to the same name"))

  ;; We find the proper prefixed name.
  (setq new-name
	(if (gnus-group-native-p group)
	    ;; Native group.
	    new-name
	  ;; Foreign group.
	  (gnus-group-prefixed-name
	   (gnus-group-real-name new-name)
	   (gnus-info-method (gnus-get-info group)))))

  (let ((decoded-group (gnus-group-decoded-name group))
	(decoded-new-name (gnus-group-decoded-name new-name)))
    (when (gnus-active new-name)
      (error "The group %s already exists" decoded-new-name))

    (gnus-message 6 "Renaming group %s to %s..."
		  decoded-group decoded-new-name)
    (prog1
	(if (progn
	      (gnus-group-goto-group group)
	      (not (when (< (gnus-group-group-level) gnus-level-zombie)
		     (gnus-request-rename-group group new-name))))
	    (gnus-error 3 "Couldn't rename group %s to %s"
			decoded-group decoded-new-name)
	  ;; We rename the group internally by killing it...
	  (gnus-group-kill-group)
	  ;; ... changing its name ...
	  (setcar (cdar gnus-list-of-killed-groups) new-name)
	  ;; ... and then yanking it.  Magic!
	  (gnus-group-yank-group)
	  (gnus-set-active new-name (gnus-active group))
	  (gnus-message 6 "Renaming group %s to %s...done"
			decoded-group decoded-new-name)
	  new-name)
      (setq gnus-killed-list (delete group gnus-killed-list))
      (gnus-set-active group nil)
      (gnus-dribble-touch)
      (gnus-group-position-point))))

(defun gnus-group-edit-group (group &optional part)
  "Edit the group on the current line."
  (interactive (list (gnus-group-group-name)))
  (let ((part (or part 'info))
	info)
    (unless group
      (error "No group on current line"))
    (unless (setq info (gnus-get-info group))
      (error "Killed group; can't be edited"))
    (ignore-errors
      (gnus-close-group group))
    (gnus-edit-form
     ;; Find the proper form to edit.
     (cond ((eq part 'method)
	    (or (gnus-info-method info) "native"))
	   ((eq part 'params)
	    (gnus-info-params info))
	   (t info))
     ;; The proper documentation.
     (format
      "Editing the %s for `%s'."
      (cond
       ((eq part 'method) "select method")
       ((eq part 'params) "group parameters")
       (t "group info"))
      (gnus-group-decoded-name group))
     `(lambda (form)
	(gnus-group-edit-group-done ',part ,group form)))
    (local-set-key
     "\C-c\C-i"
     (gnus-create-info-command
      (cond
       ((eq part 'method)
	"(gnus)Select Methods")
       ((eq part 'params)
	"(gnus)Group Parameters")
       (t
	"(gnus)Group Info"))))))

(defun gnus-group-edit-group-method (group)
  "Edit the select method of GROUP."
  (interactive (list (gnus-group-group-name)))
  (gnus-group-edit-group group 'method))

(defun gnus-group-edit-group-parameters (group)
  "Edit the group parameters of GROUP."
  (interactive (list (gnus-group-group-name)))
  (gnus-group-edit-group group 'params))

(defun gnus-group-edit-group-done (part group form)
  "Update variables."
  (let* ((method (cond ((eq part 'info) (nth 4 form))
		       ((eq part 'method) form)
		       (t nil)))
	 (info (cond ((eq part 'info) form)
		     ((eq part 'method) (gnus-get-info group))
		     (t nil)))
	 (new-group (if info
			(if (or (not method)
				(gnus-server-equal
				 gnus-select-method method))
			    (gnus-group-real-name (car info))
			  (gnus-group-prefixed-name
			   (gnus-group-real-name (car info)) method))
		      nil)))
    (when (and new-group
	       (not (equal new-group group)))
      (when (gnus-group-goto-group group)
	(gnus-group-kill-group 1))
      (gnus-activate-group new-group))
    ;; Set the info.
    (if (not (and info new-group))
	(gnus-group-set-info form (or new-group group) part)
      (setq info (gnus-copy-sequence info))
      (setcar info new-group)
      (unless (gnus-server-equal method "native")
	(unless (nthcdr 3 info)
	  (nconc info (list nil nil)))
	(unless (nthcdr 4 info)
	  (nconc info (list nil)))
	(gnus-info-set-method info method))
      (gnus-group-set-info info))
    (gnus-group-update-group (or new-group group))
    (gnus-group-position-point)))

(defun gnus-group-make-useful-group (group method)
  "Create one of the groups described in `gnus-useful-groups'."
  (interactive
   (let ((entry (assoc (gnus-completing-read "Create group"
                                             (mapcar 'car gnus-useful-groups)
                                             t)
		       gnus-useful-groups)))
     (list (cadr entry)
	   ;; Don't use `caddr' here since macros within the `interactive'
	   ;; form won't be expanded.
	   (car (cddr entry)))))
  (setq method (gnus-copy-sequence method))
  (let (entry)
    (while (setq entry (memq (assq 'eval method) method))
      (setcar entry (eval (cadar entry)))))
  (gnus-group-make-group group method))

(defun gnus-group-make-help-group (&optional noerror)
  "Create the Gnus documentation group.
Optional argument NOERROR modifies the behavior of this function when the
group already exists:
- if not given, and error is signaled,
- if t, stay silent,
- if anything else, just print a message."
  (interactive)
  (let ((name (gnus-group-prefixed-name "gnus-help" '(nndoc "gnus-help")))
	(file (nnheader-find-etc-directory "gnus-tut.txt" t)))
    (if (gnus-group-entry name)
	(cond ((eq noerror nil)
	       (error "Documentation group already exists"))
	      ((eq noerror t)
	       ;; stay silent
	       )
	      (t
	       (gnus-message 1 "Documentation group already exists")))
      ;; else:
      (if (not file)
	  (gnus-message 1 "Couldn't find doc group")
	(gnus-group-make-group
	 (gnus-group-real-name name)
	 (list 'nndoc "gnus-help"
	       (list 'nndoc-address file)
	       (list 'nndoc-article-type 'mbox))))
      ))
  (gnus-group-position-point))

(defun gnus-group-make-doc-group (file type)
  "Create a group that uses a single file as the source.

If called with a prefix argument, ask for the file type."
  (interactive
   (list (read-file-name "File name: ")
	 (and current-prefix-arg 'ask)))
  (when (eq type 'ask)
    (let ((err "")
	  char found)
      (while (not found)
	(message
	 "%sFile type (mbox, babyl, digest, forward, mmdf, guess) [m, b, d, f, a, g]: "
	 err)
	(setq found (cond ((= (setq char (read-char)) ?m) 'mbox)
			  ((= char ?b) 'babyl)
			  ((= char ?d) 'digest)
			  ((= char ?f) 'forward)
			  ((= char ?a) 'mmfd)
			  ((= char ?g) 'guess)
			  (t (setq err (format "%c unknown. " char))
			     nil))))
      (setq type found)))
  (setq file (expand-file-name file))
  (let* ((name (gnus-generate-new-group-name
		(gnus-group-prefixed-name
		 (file-name-nondirectory file) '(nndoc ""))))
	 (method (list 'nndoc file
		       (list 'nndoc-address file)
		       (list 'nndoc-article-type (or type 'guess))))
	 (coding (gnus-group-name-charset method name)))
    (setcar (cdr method) (mm-encode-coding-string file coding))
    (gnus-group-make-group
     (mm-encode-coding-string (gnus-group-real-name name) coding)
     method nil nil t)))

(defvar nnweb-type-definition)
(defvar gnus-group-web-type-history nil)
(defvar gnus-group-web-search-history nil)
(defun gnus-group-make-web-group (&optional solid)
  "Create an ephemeral nnweb group.
If SOLID (the prefix), create a solid group."
  (interactive "P")
  (require 'nnweb)
  (let* ((group
	  (if solid (gnus-read-group "Group name: ")
	    (message-unique-id)))
	 (default-type (or (car gnus-group-web-type-history)
			   (symbol-name (caar nnweb-type-definition))))
	 (type
	  (gnus-string-or
	   (gnus-completing-read
	    "Search engine type"
	    (mapcar (lambda (elem) (symbol-name (car elem)))
		    nnweb-type-definition)
	    t nil 'gnus-group-web-type-history)
	   default-type))
	 (search
	  (read-string
	   "Search string: "
	   (cons (or (car gnus-group-web-search-history) "") 0)
	   'gnus-group-web-search-history))
	 (method
	  `(nnweb ,group (nnweb-search ,search)
		  (nnweb-type ,(intern type))
		  (nnweb-ephemeral-p t))))
    (if solid
	(progn
	  (gnus-alist-pull 'nnweb-ephemeral-p method)
	  (gnus-group-make-group group method))
      (gnus-group-read-ephemeral-group
       group method t
       (cons (current-buffer)
	     (if (eq major-mode 'gnus-summary-mode) 'summary 'group))))))

(defvar nnrss-group-alist)
(eval-when-compile
  (defun nnrss-discover-feed (arg))
  (defun nnrss-save-server-data (arg)))
(defun gnus-group-make-rss-group (&optional url)
  "Given a URL, discover if there is an RSS feed.
If there is, use Gnus to create an nnrss group"
  (interactive)
  (require 'nnrss)
  (if (not url)
      (setq url (read-from-minibuffer "URL to Search for RSS: ")))
  (let ((feedinfo (nnrss-discover-feed url)))
    (if feedinfo
	(let* ((title (gnus-newsgroup-savable-name
		       (read-from-minibuffer "Title: "
					     (gnus-newsgroup-savable-name
					      (mapconcat
					       'identity
					       (split-string
						(or (cdr (assoc 'title
								feedinfo))
						    ""))
					       " ")))))
	       (desc  (read-from-minibuffer "Description: "
					    (mapconcat
					     'identity
					     (split-string
					      (or (cdr (assoc 'description
							      feedinfo))
						  ""))
					     " ")))
	       (href (cdr (assoc 'href feedinfo)))
	       (coding (gnus-group-name-charset '(nnrss "") title)))
	  (when coding
	    ;; Unify non-ASCII text.
	    (setq title (mm-decode-coding-string
			 (mm-encode-coding-string title coding)
			 coding)))
	  (gnus-group-make-group title '(nnrss ""))
	  (push (list title href desc) nnrss-group-alist)
	  (nnrss-save-server-data nil))
      (error "No feeds found for %s" url))))

(defun gnus-group-make-directory-group (dir)
  "Create an nndir group.
The user will be prompted for a directory.  The contents of this
directory will be used as a newsgroup.  The directory should contain
mail messages or news articles in files that have numeric names."
  (interactive
   (list (read-directory-name "Create group from directory: ")))
  (unless (file-exists-p dir)
    (error "No such directory"))
  (unless (file-directory-p dir)
    (error "Not a directory"))
  (let ((ext "")
	(i 0)
	group)
    (while (or (not group) (gnus-group-entry group))
      (setq group
	    (gnus-group-prefixed-name
	     (expand-file-name ext dir)
	     '(nndir "")))
      (setq ext (format "<%d>" (setq i (1+ i)))))
    (gnus-group-make-group
     (gnus-group-real-name group)
     (list 'nndir (gnus-group-real-name group) (list 'nndir-directory dir)))))

(defun gnus-group-add-to-virtual (n vgroup)
  "Add the current group to a virtual group."
  (interactive
   (list current-prefix-arg
	 (gnus-group-completing-read "Add to virtual group"
                                     nil t "nnvirtual:")))
  (unless (eq (car (gnus-find-method-for-group vgroup)) 'nnvirtual)
    (error "%s is not an nnvirtual group" vgroup))
  (gnus-close-group vgroup)
  (let* ((groups (gnus-group-process-prefix n))
	 (method (gnus-info-method (gnus-get-info vgroup))))
    (setcar (cdr method)
	    (concat
	     (nth 1 method) "\\|"
	     (mapconcat
	      (lambda (s)
		(gnus-group-remove-mark s)
		(concat "\\(^" (regexp-quote s) "$\\)"))
	      groups "\\|"))))
  (gnus-group-position-point))

(defun gnus-group-make-empty-virtual (group)
  "Create a new, fresh, empty virtual group."
  (interactive "sCreate new, empty virtual group: ")
  (let* ((method (list 'nnvirtual "^$"))
	 (pgroup (gnus-group-prefixed-name group method)))
    ;; Check whether it exists already.
    (when (gnus-group-entry pgroup)
      (error "Group %s already exists" pgroup))
    ;; Subscribe the new group after the group on the current line.
    (gnus-subscribe-group pgroup (gnus-group-group-name) method)
    (gnus-group-update-group pgroup)
    (forward-line -1)
    (gnus-group-position-point)))

(defun gnus-group-enter-directory (dir)
  "Enter an ephemeral nneething group."
  (interactive "DDirectory to read: ")
  (let* ((method (list 'nneething dir '(nneething-read-only t)))
	 (leaf (gnus-group-prefixed-name
		(file-name-nondirectory (directory-file-name dir))
		method))
	 (name (gnus-generate-new-group-name leaf)))
    (unless (gnus-group-read-ephemeral-group
	     name method t
	     (cons (current-buffer)
		   (if (eq major-mode 'gnus-summary-mode)
		       'summary 'group)))
      (error "Couldn't enter %s" dir))))

(defun gnus-group-expunge-group (group)
  "Expunge deleted articles in current nnimap GROUP."
  (interactive (list (gnus-group-group-name)))
  (let ((method (gnus-find-method-for-group group)))
    (if (not (gnus-check-backend-function
	      'request-expunge-group (car method)))
	(error "%s does not support expunging" (car method))
      (gnus-request-expunge-group group method))))

(autoload 'nnimap-acl-get "nnimap")
(autoload 'nnimap-acl-edit "nnimap")

(defun gnus-group-nnimap-edit-acl (group)
  "Edit the Access Control List of current nnimap GROUP."
  (interactive (list (gnus-group-group-name)))
  (let ((mailbox (gnus-group-real-name group)) method acl)
    (unless group
      (error "No group on current line"))
    (unless (gnus-get-info group)
      (error "Killed group; can't be edited"))
    (unless (eq (car (setq method (gnus-find-method-for-group group))) 'nnimap)
      (error "%s is not an nnimap group" group))
    (unless (setq acl (nnimap-acl-get mailbox (cadr method)))
      (error "Server does not support ACL's"))
    (gnus-edit-form acl (format "Editing the access control list for `%s'.

   An access control list is a list of (identifier . rights) elements.

   The identifier string specifies the corresponding user.  The
   identifier \"anyone\" is reserved to refer to the universal identity.

   Rights is a string listing a (possibly empty) set of alphanumeric
   characters, each character listing a set of operations which is being
   controlled.  Letters are reserved for ``standard'' rights, listed
   below.  Digits are reserved for implementation or site defined rights.

   l - lookup (mailbox is visible to LIST/LSUB commands)
   r - read (SELECT the mailbox, perform CHECK, FETCH, PARTIAL,
       SEARCH, COPY from mailbox)
   s - keep seen/unseen information across sessions (STORE \\SEEN flag)
   w - write (STORE flags other than \\SEEN and \\DELETED)
   i - insert (perform APPEND, COPY into mailbox)
   p - post (send mail to submission address for mailbox,
       not enforced by IMAP4 itself)
   c - create and delete mailbox (CREATE new sub-mailboxes in any
       implementation-defined hierarchy, RENAME or DELETE mailbox)
   d - delete messages (STORE \\DELETED flag, perform EXPUNGE)
   a - administer (perform SETACL)" group)
		    `(lambda (form)
		       (nnimap-acl-edit
			,mailbox ',method ',acl form)))))

;; Group sorting commands
;; Suggested by Joe Hildebrand <hildjj@idaho.fuentez.com>.

(defun gnus-group-sort-groups (func &optional reverse)
  "Sort the group buffer according to FUNC.
When used interactively, the sorting function used will be
determined by the `gnus-group-sort-function' variable.
If REVERSE (the prefix), reverse the sorting order."
  (interactive (list gnus-group-sort-function current-prefix-arg))
  (funcall gnus-group-sort-alist-function
	   (gnus-make-sort-function func) reverse)
  (gnus-group-unmark-all-groups)
  (gnus-group-list-groups)
  (gnus-dribble-touch))

(defun gnus-group-sort-flat (func reverse)
  ;; We peel off the dummy group from the alist.
  (when func
    (when (equal (gnus-info-group (car gnus-newsrc-alist)) "dummy.group")
      (pop gnus-newsrc-alist))
    ;; Do the sorting.
    (setq gnus-newsrc-alist
	  (sort gnus-newsrc-alist func))
    (when reverse
      (setq gnus-newsrc-alist (nreverse gnus-newsrc-alist)))
    ;; Regenerate the hash table.
    (gnus-make-hashtable-from-newsrc-alist)))

(defun gnus-group-sort-groups-by-alphabet (&optional reverse)
  "Sort the group buffer alphabetically by group name.
If REVERSE, sort in reverse order."
  (interactive "P")
  (gnus-group-sort-groups 'gnus-group-sort-by-alphabet reverse))

(defun gnus-group-sort-groups-by-real-name (&optional reverse)
  "Sort the group buffer alphabetically by real (unprefixed) group name.
If REVERSE, sort in reverse order."
  (interactive "P")
  (gnus-group-sort-groups 'gnus-group-sort-by-real-name reverse))

(defun gnus-group-sort-groups-by-unread (&optional reverse)
  "Sort the group buffer by number of unread articles.
If REVERSE, sort in reverse order."
  (interactive "P")
  (gnus-group-sort-groups 'gnus-group-sort-by-unread reverse))

(defun gnus-group-sort-groups-by-level (&optional reverse)
  "Sort the group buffer by group level.
If REVERSE, sort in reverse order."
  (interactive "P")
  (gnus-group-sort-groups 'gnus-group-sort-by-level reverse))

(defun gnus-group-sort-groups-by-score (&optional reverse)
  "Sort the group buffer by group score.
If REVERSE, sort in reverse order."
  (interactive "P")
  (gnus-group-sort-groups 'gnus-group-sort-by-score reverse))

(defun gnus-group-sort-groups-by-rank (&optional reverse)
  "Sort the group buffer by group rank.
If REVERSE, sort in reverse order."
  (interactive "P")
  (gnus-group-sort-groups 'gnus-group-sort-by-rank reverse))

(defun gnus-group-sort-groups-by-method (&optional reverse)
  "Sort the group buffer alphabetically by back end name.
If REVERSE, sort in reverse order."
  (interactive "P")
  (gnus-group-sort-groups 'gnus-group-sort-by-method reverse))

(defun gnus-group-sort-groups-by-server (&optional reverse)
  "Sort the group buffer alphabetically by server name.
If REVERSE, sort in reverse order."
  (interactive "P")
  (gnus-group-sort-groups 'gnus-group-sort-by-server reverse))

;;; Selected group sorting.

(defun gnus-group-sort-selected-groups (n func &optional reverse)
  "Sort the process/prefixed groups."
  (interactive (list current-prefix-arg gnus-group-sort-function))
  (let ((groups (gnus-group-process-prefix n)))
    (funcall gnus-group-sort-selected-function
	     groups (gnus-make-sort-function func) reverse)
    (gnus-group-unmark-all-groups)
    (gnus-group-list-groups)
    (gnus-dribble-touch)))

(defun gnus-group-sort-selected-flat (groups func reverse)
  (let (entries infos)
    ;; First find all the group entries for these groups.
    (while groups
      (push (nthcdr 2 (gnus-group-entry (pop groups)))
	    entries))
    ;; Then sort the infos.
    (setq infos
	  (sort
	   (mapcar
	    (lambda (entry) (car entry))
	    (setq entries (nreverse entries)))
	   func))
    (when reverse
      (setq infos (nreverse infos)))
    ;; Go through all the infos and replace the old entries
    ;; with the new infos.
    (while infos
      (setcar (car entries) (pop infos))
      (pop entries))
    ;; Update the hashtable.
    (gnus-make-hashtable-from-newsrc-alist)))

(defun gnus-group-sort-selected-groups-by-alphabet (&optional n reverse)
  "Sort the group buffer alphabetically by group name.
Obeys the process/prefix convention.  If REVERSE (the symbolic prefix),
sort in reverse order."
  (interactive (gnus-interactive "P\ny"))
  (gnus-group-sort-selected-groups n 'gnus-group-sort-by-alphabet reverse))

(defun gnus-group-sort-selected-groups-by-real-name (&optional n reverse)
  "Sort the group buffer alphabetically by real group name.
Obeys the process/prefix convention.  If REVERSE (the symbolic prefix),
sort in reverse order."
  (interactive (gnus-interactive "P\ny"))
  (gnus-group-sort-selected-groups n 'gnus-group-sort-by-real-name reverse))

(defun gnus-group-sort-selected-groups-by-unread (&optional n reverse)
  "Sort the group buffer by number of unread articles.
Obeys the process/prefix convention.  If REVERSE (the symbolic prefix),
sort in reverse order."
  (interactive (gnus-interactive "P\ny"))
  (gnus-group-sort-selected-groups n 'gnus-group-sort-by-unread reverse))

(defun gnus-group-sort-selected-groups-by-level (&optional n reverse)
  "Sort the group buffer by group level.
Obeys the process/prefix convention.  If REVERSE (the symbolic prefix),
sort in reverse order."
  (interactive (gnus-interactive "P\ny"))
  (gnus-group-sort-selected-groups n 'gnus-group-sort-by-level reverse))

(defun gnus-group-sort-selected-groups-by-score (&optional n reverse)
  "Sort the group buffer by group score.
Obeys the process/prefix convention.  If REVERSE (the symbolic prefix),
sort in reverse order."
  (interactive (gnus-interactive "P\ny"))
  (gnus-group-sort-selected-groups n 'gnus-group-sort-by-score reverse))

(defun gnus-group-sort-selected-groups-by-rank (&optional n reverse)
  "Sort the group buffer by group rank.
Obeys the process/prefix convention.  If REVERSE (the symbolic prefix),
sort in reverse order."
  (interactive (gnus-interactive "P\ny"))
  (gnus-group-sort-selected-groups n 'gnus-group-sort-by-rank reverse))

(defun gnus-group-sort-selected-groups-by-method (&optional n reverse)
  "Sort the group buffer alphabetically by back end name.
Obeys the process/prefix convention.  If REVERSE (the symbolic prefix),
sort in reverse order."
  (interactive (gnus-interactive "P\ny"))
  (gnus-group-sort-selected-groups n 'gnus-group-sort-by-method reverse))

;;; Sorting predicates.

(defun gnus-group-sort-by-alphabet (info1 info2)
  "Sort alphabetically."
  (string< (gnus-info-group info1) (gnus-info-group info2)))

(defun gnus-group-sort-by-real-name (info1 info2)
  "Sort alphabetically on real (unprefixed) names."
  (string< (gnus-group-real-name (gnus-info-group info1))
	   (gnus-group-real-name (gnus-info-group info2))))

(defun gnus-group-sort-by-unread (info1 info2)
  "Sort by number of unread articles."
  (let ((n1 (gnus-group-unread (gnus-info-group info1)))
	(n2 (gnus-group-unread (gnus-info-group info2))))
    (< (or (and (numberp n1) n1) 0)
       (or (and (numberp n2) n2) 0))))

(defun gnus-group-sort-by-level (info1 info2)
  "Sort by level."
  (< (gnus-info-level info1) (gnus-info-level info2)))

(defun gnus-group-sort-by-method (info1 info2)
  "Sort alphabetically by back end name."
  (string< (car (gnus-find-method-for-group
		 (gnus-info-group info1) info1))
	   (car (gnus-find-method-for-group
		 (gnus-info-group info2) info2))))

(defun gnus-group-sort-by-server (info1 info2)
  "Sort alphabetically by server name."
  (string< (gnus-method-to-full-server-name
	    (gnus-find-method-for-group
	     (gnus-info-group info1) info1))
	   (gnus-method-to-full-server-name
	    (gnus-find-method-for-group
	     (gnus-info-group info2) info2))))

(defun gnus-group-sort-by-score (info1 info2)
  "Sort by group score."
  (> (gnus-info-score info1) (gnus-info-score info2)))

(defun gnus-group-sort-by-rank (info1 info2)
  "Sort by level and score."
  (let ((level1 (gnus-info-level info1))
	(level2 (gnus-info-level info2)))
    (or (< level1 level2)
	(and (= level1 level2)
	     (> (gnus-info-score info1) (gnus-info-score info2))))))

;;; Clearing data

(defun gnus-group-clear-data (&optional arg)
  "Clear all marks and read ranges from the current group.
Obeys the process/prefix convention."
  (interactive "P")
  (when (gnus-y-or-n-p "Really clear data? ")
    (gnus-group-iterate arg
      (lambda (group)
	(let (info)
	  (gnus-info-clear-data (setq info (gnus-get-info group)))
	  (gnus-get-unread-articles-in-group info (gnus-active group) t)
	  (when (gnus-group-goto-group group)
	    (gnus-group-update-group-line)))))))

(defun gnus-group-clear-data-on-native-groups ()
  "Clear all marks and read ranges from all native groups."
  (interactive)
  (when (gnus-yes-or-no-p "Really clear all data from almost all groups? ")
    (let ((alist (cdr gnus-newsrc-alist))
	  info)
      (while (setq info (pop alist))
	(when (gnus-group-native-p (gnus-info-group info))
	  (gnus-info-clear-data info)))
      (gnus-get-unread-articles)
      (gnus-dribble-touch)
      (when (gnus-y-or-n-p
	     "Move the cache away to avoid problems in the future? ")
	(call-interactively 'gnus-cache-move-cache)))))

(defun gnus-info-clear-data (info)
  "Clear all marks and read ranges from INFO."
  (let ((group (gnus-info-group info))
	action)
    (dolist (el (gnus-info-marks info))
      (push `(,(cdr el) add (,(car el))) action))
    (push `(,(gnus-info-read info) add (read)) action)
    (gnus-undo-register
      `(progn
	 (gnus-request-set-mark ,group ',action)
	 (gnus-info-set-marks ',info ',(gnus-info-marks info) t)
	 (gnus-info-set-read ',info ',(gnus-info-read info))
	 (when (gnus-group-goto-group ,group)
	   (gnus-get-unread-articles-in-group ',info ',(gnus-active group) t)
	   (gnus-group-update-group-line))))
    (setq action (mapcar (lambda (el) (list (nth 0 el) 'del (nth 2 el)))
			 action))
    (gnus-request-set-mark group action)
    (gnus-info-set-read info nil)
    (when (gnus-info-marks info)
      (gnus-info-set-marks info nil))))

;; Group catching up.

(defun gnus-group-catchup-current (&optional n all)
  "Mark all unread articles in the current newsgroup as read.
If prefix argument N is numeric, the next N newsgroups will be
caught up.  If ALL is non-nil, marked articles will also be marked as
read.  Cross references (Xref: header) of articles are ignored.
The number of newsgroups that this function was unable to catch
up is returned."
  (interactive "P")
  (let ((groups (gnus-group-process-prefix n))
	(ret 0)
	group)
    (unless groups (error "No groups selected"))
    (if (not
	 (or (not gnus-interactive-catchup) ;Without confirmation?
	     gnus-expert-user
	     (gnus-y-or-n-p
	      (format
	       (if all
		   "Do you really want to mark all articles in %s as read? "
		 "Mark all unread articles in %s as read? ")
	       (if (= (length groups) 1)
		   (gnus-group-decoded-name (car groups))
		 (format "these %d groups" (length groups)))))))
	n
      (while (setq group (pop groups))
	(gnus-group-remove-mark group)
	;; Virtual groups have to be given special treatment.
	(let ((method (gnus-find-method-for-group group)))
	  (when (eq 'nnvirtual (car method))
	    (nnvirtual-catchup-group
	     (gnus-group-real-name group) (nth 1 method) all)))
	(cond
	 ((>= (gnus-group-level group) gnus-level-zombie)
	  (gnus-message 2 "Dead groups can't be caught up"))
	 ((prog1
	      (gnus-group-goto-group group)
	    (gnus-group-catchup group all))
	  (gnus-group-update-group-line))
	 (t
	  (setq ret (1+ ret)))))
      (gnus-group-next-unread-group 1)
      ret)))

(defun gnus-group-catchup-current-all (&optional n)
  "Mark all articles in current newsgroup as read.
Cross references (Xref: header) of articles are ignored."
  (interactive "P")
  (gnus-group-catchup-current n 'all))

(defun gnus-group-catchup (group &optional all)
  "Mark all articles in GROUP as read.
If ALL is non-nil, all articles are marked as read.
The return value is the number of articles that were marked as read,
or nil if no action could be taken."
  (let* ((entry (gnus-group-entry group))
	 (num (car entry))
	 (marks (gnus-info-marks (nth 2 entry)))
	 (unread (gnus-sequence-of-unread-articles group)))
    ;; Remove entries for this group.
    (nnmail-purge-split-history (gnus-group-real-name group))
    ;; Do the updating only if the newsgroup isn't killed.
    (if (not (numberp (car entry)))
	(gnus-message 1 "Can't catch up %s; non-active group" group)
      (gnus-update-read-articles group nil)
      (when all
	;; Nix out the lists of marks and dormants.
	(gnus-request-set-mark group (list (list (cdr (assq 'tick marks))
						 'del '(tick))
					   (list (cdr (assq 'dormant marks))
						 'del '(dormant))))
	(setq unread (gnus-range-add (gnus-range-add
                                      unread (cdr (assq 'dormant marks)))
                                     (cdr (assq 'tick marks))))
	(gnus-add-marked-articles group 'tick nil nil 'force)
	(gnus-add-marked-articles group 'dormant nil nil 'force))
      ;; Do auto-expirable marks if that's required.
      (when (and (gnus-group-auto-expirable-p group)
		 (not (gnus-group-read-only-p group)))
        (gnus-range-map
	 (lambda (article)
	   (gnus-add-marked-articles group 'expire (list article))
	   (gnus-request-set-mark group (list (list (list article)
						    'add '(expire)))))
	 unread))
      (let ((gnus-newsgroup-name group))
	(gnus-run-hooks 'gnus-group-catchup-group-hook))
      num)))

(defun gnus-group-expire-articles (&optional n)
  "Expire all expirable articles in the current newsgroup.
Uses the process/prefix convention."
  (interactive "P")
  (let ((groups (gnus-group-process-prefix n))
	group)
    (unless groups
      (error "No groups to expire"))
    (while (setq group (pop groups))
      (gnus-group-remove-mark group)
      (gnus-group-expire-articles-1 group)
      (gnus-dribble-touch)
      (gnus-group-position-point))))

(defun gnus-group-expire-articles-1 (group)
  (when (gnus-check-backend-function 'request-expire-articles group)
    (gnus-message 6 "Expiring articles in %s..."
		  (gnus-group-decoded-name group))
    (let* ((info (gnus-get-info group))
	   (expirable (if (gnus-group-total-expirable-p group)
			  (cons nil (gnus-list-of-read-articles group))
			(assq 'expire (gnus-info-marks info))))
	   (expiry-wait (gnus-group-find-parameter group 'expiry-wait))
	   (nnmail-expiry-target
	    (or (gnus-group-find-parameter group 'expiry-target)
		nnmail-expiry-target)))
      (when expirable
	(gnus-check-group group)
	(setcdr
	 expirable
	 (gnus-compress-sequence
	  (if expiry-wait
	      ;; We set the expiry variables to the group
	      ;; parameter.
	      (let ((nnmail-expiry-wait-function nil)
		    (nnmail-expiry-wait expiry-wait))
		(gnus-request-expire-articles
		 (gnus-uncompress-sequence (cdr expirable)) group))
	    ;; Just expire using the normal expiry values.
	    (gnus-request-expire-articles
	     (gnus-uncompress-sequence (cdr expirable)) group))))
	(gnus-close-group group))
      (gnus-message 6 "Expiring articles in %s...done"
		    (gnus-group-decoded-name group))
      ;; Return the list of un-expired articles.
      (cdr expirable))))

(defun gnus-group-expire-all-groups ()
  "Expire all expirable articles in all newsgroups."
  (interactive)
  (save-excursion
    (gnus-message 5 "Expiring...")
    (let ((gnus-group-marked (mapcar (lambda (info) (gnus-info-group info))
				     (cdr gnus-newsrc-alist))))
      (gnus-group-expire-articles nil)))
  (gnus-group-position-point)
  (gnus-message 5 "Expiring...done"))

(defun gnus-group-set-current-level (n level)
  "Set the level of the next N groups to LEVEL."
  (interactive
   (list
    current-prefix-arg
    (progn
      (unless (gnus-group-process-prefix current-prefix-arg)
	(error "No group on the current line"))
      (string-to-number
       (let ((s (read-string
		 (format "Level (default %s): "
			 (or (gnus-group-group-level)
			     gnus-level-default-subscribed)))))
	 (if (string-match "^\\s-*$" s)
	     (int-to-string (or (gnus-group-group-level)
				gnus-level-default-subscribed))
	   s))))))
  (unless (and (>= level 1) (<= level gnus-level-killed))
    (error "Invalid level: %d" level))
  (dolist (group (gnus-group-process-prefix n))
    (gnus-group-remove-mark group)
    (gnus-message 6 "Changed level of %s from %d to %d"
		  (gnus-group-decoded-name group)
		  (or (gnus-group-group-level) gnus-level-killed)
		  level)
    (gnus-group-change-level
     group level (or (gnus-group-group-level) gnus-level-killed))
    (gnus-group-update-group-line))
  (gnus-group-position-point))

(defun gnus-group-unsubscribe (&optional n)
  "Unsubscribe the current group."
  (interactive "P")
  (gnus-group-unsubscribe-current-group n 'unsubscribe))

(defun gnus-group-subscribe (&optional n)
  "Subscribe the current group."
  (interactive "P")
  (gnus-group-unsubscribe-current-group n 'subscribe))

(defun gnus-group-unsubscribe-current-group (&optional n do-sub)
  "Toggle subscription of the current group.
If given numerical prefix, toggle the N next groups."
  (interactive "P")
  (dolist (group (gnus-group-process-prefix n))
    (gnus-group-remove-mark group)
    (gnus-group-unsubscribe-group
     group
     (cond
      ((eq do-sub 'unsubscribe)
       gnus-level-default-unsubscribed)
      ((eq do-sub 'subscribe)
       gnus-level-default-subscribed)
      ((<= (gnus-group-group-level) gnus-level-subscribed)
       gnus-level-default-unsubscribed)
      (t
       gnus-level-default-subscribed))
     t)
    (gnus-group-update-group-line))
  (gnus-group-next-group 1))

(defun gnus-group-unsubscribe-group (group &optional level silent)
  "Toggle subscription to GROUP.
Killed newsgroups are subscribed.  If SILENT, don't try to update the
group line."
  (interactive (list (gnus-group-completing-read
		      nil nil (gnus-read-active-file-p))))
  (let ((newsrc (gnus-group-entry group)))
    (cond
     ((string-match "^[ \t]*$" group)
      (error "Empty group name"))
     (newsrc
      ;; Toggle subscription flag.
      (gnus-group-change-level
       newsrc (if level level (if (<= (gnus-info-level (nth 2 newsrc))
				      gnus-level-subscribed)
				  (1+ gnus-level-subscribed)
				gnus-level-default-subscribed)))
      (unless silent
	(gnus-group-update-group group)))
     ((and (stringp group)
	   (or (not (gnus-read-active-file-p))
	       (gnus-active group)))
      ;; Add new newsgroup.
      (gnus-group-change-level
       group
       (if level level gnus-level-default-subscribed)
       (or (and (member group gnus-zombie-list)
		gnus-level-zombie)
	   gnus-level-killed)
       (when (gnus-group-group-name)
	 (gnus-group-entry (gnus-group-group-name))))
      (unless silent
	(gnus-group-update-group group)))
     (t (error "No such newsgroup: %s" group)))
    (gnus-group-position-point)))

(defun gnus-group-transpose-groups (n)
  "Move the current newsgroup up N places.
If given a negative prefix, move down instead.  The difference between
N and the number of steps taken is returned."
  (interactive "p")
  (unless (gnus-group-group-name)
    (error "No group on current line"))
  (gnus-group-kill-group 1)
  (prog1
      (forward-line (- n))
    (gnus-group-yank-group)
    (gnus-group-position-point)))

(defun gnus-group-kill-all-zombies (&optional dummy)
  "Kill all zombie newsgroups.
The optional DUMMY should always be nil."
  (interactive (list (not (gnus-yes-or-no-p "Really kill all zombies? "))))
  (unless dummy
    (setq gnus-killed-list (nconc gnus-zombie-list gnus-killed-list))
    (setq gnus-zombie-list nil)
    (gnus-dribble-touch)
    (gnus-group-list-groups)))

(defun gnus-group-kill-region (begin end)
  "Kill newsgroups in current region (excluding current point).
The killed newsgroups can be yanked by using \\[gnus-group-yank-group]."
  (interactive "r")
  (let ((lines
	 ;; Count lines.
	 (save-excursion
	   (count-lines
	    (progn
	      (goto-char begin)
	      (point-at-bol))
	    (progn
	      (goto-char end)
	      (point-at-bol))))))
    (goto-char begin)
    (beginning-of-line)			;Important when LINES < 1
    (gnus-group-kill-group lines)))

(defun gnus-group-kill-group (&optional n discard)
  "Kill the next N groups.
The killed newsgroups can be yanked by using \\[gnus-group-yank-group].
However, only groups that were alive can be yanked; already killed
groups or zombie groups can't be yanked.
The return value is the name of the group that was killed, or a list
of groups killed."
  (interactive "P")
  (let ((buffer-read-only nil)
	(groups (gnus-group-process-prefix n))
	group entry level out)
    (if (< (length groups) 10)
	;; This is faster when there are few groups.
	(while groups
	  (push (setq group (pop groups)) out)
	  (gnus-group-remove-mark group)
	  (setq level (gnus-group-group-level))
	  (gnus-delete-line)
	  (when (and (not discard)
		     (setq entry (gnus-group-entry group)))
	    (gnus-undo-register
	      `(progn
		 (gnus-group-goto-group ,(gnus-group-group-name))
		 (gnus-group-yank-group)))
	    (push (cons (car entry) (nth 2 entry))
		  gnus-list-of-killed-groups))
	  (gnus-group-change-level
	   (if entry entry group) gnus-level-killed (if entry nil level))
	  (when (numberp (gnus-group-unread group))
	    (gnus-request-update-group-status group 'unsubscribe))
	  (message "Killed group %s" (gnus-group-decoded-name group)))
      ;; If there are lots and lots of groups to be killed, we use
      ;; this thing instead.
      (dolist (group (nreverse groups))
	(gnus-group-remove-mark group)
	(gnus-delete-line)
	(push group gnus-killed-list)
	(setq gnus-newsrc-alist
	      (delq (assoc group gnus-newsrc-alist)
		    gnus-newsrc-alist))
	(when gnus-group-change-level-function
	  (funcall gnus-group-change-level-function
		   group gnus-level-killed 3))
	(cond
	 ((setq entry (gnus-group-entry group))
	  (push (cons (car entry) (nth 2 entry))
		gnus-list-of-killed-groups)
	  (setcdr (cdr entry) (cdddr entry)))
	 ((member group gnus-zombie-list)
	  (setq gnus-zombie-list (delete group gnus-zombie-list))))
	;; There may be more than one instance displayed.
	(while (gnus-group-goto-group group)
	  (gnus-delete-line))
	(when (numberp (gnus-group-unread group))
	  (gnus-request-update-group-status group 'unsubscribe)))
      (gnus-make-hashtable-from-newsrc-alist))

    (gnus-group-position-point)
    (if (< (length out) 2) (car out) (nreverse out))))

(defun gnus-group-yank-group (&optional arg)
  "Yank the last newsgroups killed with \\[gnus-group-kill-group], inserting it before the current newsgroup.
The numeric ARG specifies how many newsgroups are to be yanked.  The
name of the newsgroup yanked is returned, or (if several groups are
yanked) a list of yanked groups is returned."
  (interactive "p")
  (setq arg (or arg 1))
  (let (info group prev out)
    (while (>= (decf arg) 0)
      (when (not (setq info (pop gnus-list-of-killed-groups)))
	(error "No more newsgroups to yank"))
      (push (setq group (nth 1 info)) out)
      ;; Find which newsgroup to insert this one before - search
      ;; backward until something suitable is found.  If there are no
      ;; other newsgroups in this buffer, just make this newsgroup the
      ;; first newsgroup.
      (setq prev (gnus-group-group-name))
      (gnus-group-change-level
       info (gnus-info-level (cdr info)) gnus-level-killed
       (and prev (gnus-group-entry prev))
       t)
      (gnus-group-insert-group-line-info group)
      (gnus-request-update-group-status group 'subscribe)
      (gnus-undo-register
	`(when (gnus-group-goto-group ,group)
	   (gnus-group-kill-group 1))))
    (forward-line -1)
    (gnus-group-position-point)
    (if (< (length out) 2) (car out) (nreverse out))))

(defun gnus-group-kill-level (level)
  "Kill all groups that is on a certain LEVEL."
  (interactive "nKill all groups on level: ")
  (cond
   ((= level gnus-level-zombie)
    (setq gnus-killed-list
	  (nconc gnus-zombie-list gnus-killed-list))
    (setq gnus-zombie-list nil))
   ((and (< level gnus-level-zombie)
	 (> level 0)
	 (or gnus-expert-user
	     (gnus-yes-or-no-p
	      (format
	       "Do you really want to kill all groups on level %d? "
	       level))))
    (let* ((prev gnus-newsrc-alist)
	   (alist (cdr prev)))
      (while alist
	(if (= (gnus-info-level (car alist)) level)
	    (progn
	      (push (gnus-info-group (car alist)) gnus-killed-list)
	      (setcdr prev (cdr alist)))
	  (setq prev alist))
	(setq alist (cdr alist)))
      (gnus-make-hashtable-from-newsrc-alist)
      (gnus-group-list-groups)))
   (t
    (error "Can't kill; invalid level: %d" level))))

(defun gnus-group-list-all-groups (&optional arg)
  "List all newsgroups with level ARG or lower.
Default is `gnus-level-unsubscribed', which lists all subscribed and most
unsubscribed groups."
  (interactive "P")
  (gnus-group-list-groups (or arg gnus-level-unsubscribed) t))

;; Redefine this to list ALL killed groups if prefix arg used.
;; Rewritten by engstrom@src.honeywell.com (Eric Engstrom).
(defun gnus-group-list-killed (&optional arg)
  "List all killed newsgroups in the group buffer.
If ARG is non-nil, list ALL killed groups known to Gnus.  This may
entail asking the server for the groups."
  (interactive "P")
  ;; Find all possible killed newsgroups if arg.
  (when arg
    (gnus-get-killed-groups))
  (if (not gnus-killed-list)
      (gnus-message 6 "No killed groups")
    (let (gnus-group-list-mode)
      (funcall gnus-group-prepare-function
	       gnus-level-killed t gnus-level-killed))
    (goto-char (point-min)))
  (gnus-group-position-point))

(defun gnus-group-list-zombies ()
  "List all zombie newsgroups in the group buffer."
  (interactive)
  (if (not gnus-zombie-list)
      (gnus-message 6 "No zombie groups")
    (let (gnus-group-list-mode)
      (funcall gnus-group-prepare-function
	       gnus-level-zombie t gnus-level-zombie))
    (goto-char (point-min)))
  (gnus-group-position-point))

(defun gnus-group-list-active ()
  "List all groups that are available from the server(s)."
  (interactive)
  ;; First we make sure that we have really read the active file.
  (unless (gnus-read-active-file-p)
    (let ((gnus-read-active-file t)
	  (gnus-agent gnus-plugged)); If we're actually plugged, store the active file in the agent.
      (gnus-read-active-file)))
  ;; Find all groups and sort them.
  (let ((groups
	 (sort
	  (let (list)
	    (mapatoms
	     (lambda (sym)
	       (and (boundp sym)
		    (symbol-value sym)
		    (push (symbol-name sym) list)))
	     gnus-active-hashtb)
	    list)
	  'string<))
	(buffer-read-only nil)
	group)
    (erase-buffer)
    (while groups
      (setq group (pop groups))
      (gnus-add-text-properties
       (point) (prog1 (1+ (point))
		 (insert "       *: "
			 (gnus-group-decoded-name group)
			 "\n"))
       (list 'gnus-group (gnus-intern-safe group gnus-active-hashtb)
	     'gnus-unread t
	     'gnus-level (inline (gnus-group-level group)))))
    (goto-char (point-min))))

(defun gnus-activate-all-groups (level)
  "Activate absolutely all groups."
  (interactive (list gnus-level-unsubscribed))
  (let ((gnus-activate-level level)
	(gnus-activate-foreign-newsgroups level))
    (gnus-group-get-new-news)))

(defun gnus-group-get-new-news (&optional arg)
  "Get newly arrived articles.
If ARG is a number, it specifies which levels you are interested in
re-scanning.  If ARG is non-nil and not a number, this will force
\"hard\" re-reading of the active files from all servers."
  (interactive "P")
  (require 'nnmail)
  (let ((gnus-inhibit-demon t)
	;; Binding this variable will inhibit multiple fetchings
	;; of the same mail source.
	(nnmail-fetched-sources (list t)))
    (gnus-run-hooks 'gnus-get-top-new-news-hook)
    (gnus-run-hooks 'gnus-get-new-news-hook)

    ;; Read any slave files.
    (unless gnus-slave
      (gnus-master-read-slave-newsrc))

    (gnus-get-unread-articles arg)

    ;; If the user wants it, we scan for new groups.
    (when (eq gnus-check-new-newsgroups 'always)
      (gnus-find-new-newsgroups))

    (gnus-check-reasonable-setup)
    (gnus-run-hooks 'gnus-after-getting-new-news-hook)
    (gnus-group-list-groups (and (numberp arg)
				 (max (car gnus-group-list-mode) arg)))))

(defun gnus-group-get-new-news-this-group (&optional n dont-scan)
  "Check for newly arrived news in the current group (and the N-1 next groups).
The difference between N and the number of newsgroup checked is returned.
If N is negative, this group and the N-1 previous groups will be checked.
If DONT-SCAN is non-nil, scan non-activated groups as well."
  (interactive "P")
  (let* ((groups (gnus-group-process-prefix n))
	 (ret (if (numberp n) (- n (length groups)) 0))
	 (beg (unless n
		(point-marker)))
	 group method
	 (gnus-inhibit-demon t)
	 ;; Binding this variable will inhibit multiple fetchings
	 ;; of the same mail source.
	 (nnmail-fetched-sources (list t)))
    (gnus-run-hooks 'gnus-get-new-news-hook)
    (while (setq group (pop groups))
      (gnus-group-remove-mark group)
      ;; Bypass any previous denials from the server.
      (gnus-remove-denial (setq method (gnus-find-method-for-group group)))
      (if (gnus-activate-group group (if dont-scan nil 'scan) nil method)
	  (let ((info (gnus-get-info group))
		(active (gnus-active group)))
	    (when info
	      (gnus-request-update-info info method))
	    (gnus-get-unread-articles-in-group info active)
	    (unless (gnus-virtual-group-p group)
	      (gnus-close-group group))
	    (when gnus-agent
	      (gnus-agent-save-group-info
	       method (gnus-group-real-name group) active))
	    (gnus-group-update-group group nil t))
	(gnus-error 3 "%s error: %s" group (gnus-status-message group))))
    (when beg
      (goto-char beg))
    (when gnus-goto-next-group-when-activating
      (gnus-group-next-unread-group 1 t))
    (gnus-group-position-point)
    ret))

(defun gnus-group-describe-group (force &optional group)
  "Display a description of the current newsgroup."
  (interactive (list current-prefix-arg (gnus-group-group-name)))
  (let* ((method (gnus-find-method-for-group group))
	 (mname (gnus-group-prefixed-name "" method))
	 desc)
    (when (and force
	       gnus-description-hashtb)
      (gnus-sethash mname nil gnus-description-hashtb))
    (unless group
      (error "No group name given"))
    (when (or (and gnus-description-hashtb
		   ;; We check whether this group's method has been
		   ;; queried for a description file.
		   (gnus-gethash mname gnus-description-hashtb))
	      (setq desc (gnus-group-get-description group))
	      (gnus-read-descriptions-file method))
      (gnus-message 1 "%s"
		    (or desc (gnus-gethash group gnus-description-hashtb)
			"No description available")))))

;; Suggested by Per Abrahamsen <amanda@iesd.auc.dk>.
(defun gnus-group-describe-all-groups (&optional force)
  "Pop up a buffer with descriptions of all newsgroups."
  (interactive "P")
  (when force
    (setq gnus-description-hashtb nil))
  (when (not (or gnus-description-hashtb
		 (gnus-read-all-descriptions-files)))
    (error "Couldn't request descriptions file"))
  (let ((buffer-read-only nil)
	b)
    (erase-buffer)
    (mapatoms
     (lambda (group)
       (setq b (point))
       (let ((charset (gnus-group-name-charset nil (symbol-name group))))
	 (insert (format "      *: %-20s %s\n"
			 (gnus-group-name-decode
			  (symbol-name group) charset)
			 (gnus-group-name-decode
			  (symbol-value group) charset))))
       (gnus-add-text-properties
	b (1+ b) (list 'gnus-group group
		       'gnus-unread t 'gnus-marked nil
		       'gnus-level (1+ gnus-level-subscribed))))
     gnus-description-hashtb)
    (goto-char (point-min))
    (gnus-group-position-point)))

;; Suggested by Daniel Quinlan <quinlan@best.com>.
(defun gnus-group-apropos (regexp &optional search-description)
  "List all newsgroups that have names that match a regexp."
  (interactive "sGnus apropos (regexp): ")
  (let ((prev "")
	(obuf (current-buffer))
	groups des)
    ;; Go through all newsgroups that are known to Gnus.
    (mapatoms
     (lambda (group)
       (and (symbol-name group)
	    (string-match regexp (symbol-name group))
	    (symbol-value group)
	    (push (symbol-name group) groups)))
     gnus-active-hashtb)
    ;; Also go through all descriptions that are known to Gnus.
    (when search-description
      (mapatoms
       (lambda (group)
	 (and (string-match regexp (symbol-value group))
	      (push (symbol-name group) groups)))
       gnus-description-hashtb))
    (if (not groups)
	(gnus-message 3 "No groups matched \"%s\"." regexp)
      ;; Print out all the groups.
      (save-excursion
	(pop-to-buffer "*Gnus Help*")
	(buffer-disable-undo)
	(erase-buffer)
	(setq groups (sort groups 'string<))
	(while groups
	  ;; Groups may be entered twice into the list of groups.
	  (when (not (string= (car groups) prev))
	    (setq prev (car groups))
	    (let ((charset (gnus-group-name-charset nil prev)))
	      (insert (gnus-group-name-decode prev charset) "\n")
	      (when (and gnus-description-hashtb
			 (setq des (gnus-gethash (car groups)
						 gnus-description-hashtb)))
		(insert "  " (gnus-group-name-decode des charset) "\n"))))
	  (setq groups (cdr groups)))
	(goto-char (point-min))))
    (pop-to-buffer obuf)))

(defun gnus-group-description-apropos (regexp)
  "List all newsgroups that have names or descriptions that match REGEXP."
  (interactive "sGnus description apropos (regexp): ")
  (when (not (or gnus-description-hashtb
		 (gnus-read-all-descriptions-files)))
    (error "Couldn't request descriptions file"))
  (gnus-group-apropos regexp t))

;; Suggested by Per Abrahamsen <amanda@iesd.auc.dk>.
(defun gnus-group-list-matching (level regexp &optional all lowest)
  "List all groups with unread articles that match REGEXP.
If the prefix LEVEL is non-nil, it should be a number that says which
level to cut off listing groups.
If ALL, also list groups with no unread articles.
If LOWEST, don't list groups with level lower than LOWEST.

This command may read the active file."
  (interactive "P\nsList newsgroups matching: ")
  ;; First make sure active file has been read.
  (when (and level
	     (> (prefix-numeric-value level) gnus-level-killed))
    (gnus-get-killed-groups))
  (funcall gnus-group-prepare-function
   (or level gnus-level-subscribed) (and all t) (or lowest 1) regexp)
  (goto-char (point-min))
  (gnus-group-position-point))

(defun gnus-group-list-all-matching (level regexp &optional lowest)
  "List all groups that match REGEXP.
If the prefix LEVEL is non-nil, it should be a number that says which
level to cut off listing groups.
If LOWEST, don't list groups with level lower than LOWEST."
  (interactive "P\nsList newsgroups matching: ")
  (when level
    (setq level (prefix-numeric-value level)))
  (gnus-group-list-matching (or level gnus-level-killed) regexp t lowest))

;; Suggested by Jack Vinson <vinson@unagi.cis.upenn.edu>.
(defun gnus-group-save-newsrc (&optional force)
  "Save the Gnus startup files.
If FORCE, force saving whether it is necessary or not."
  (interactive "P")
  (gnus-save-newsrc-file force))

(defun gnus-group-restart (&optional arg)
  "Force Gnus to read the .newsrc file."
  (interactive "P")
  (when (gnus-yes-or-no-p
	 (format "Are you sure you want to restart Gnus? "))
    (gnus-save-newsrc-file)
    (gnus-clear-system)
    (gnus)))

(defun gnus-group-read-init-file ()
  "Read the Gnus elisp init file."
  (interactive)
  (gnus-read-init-file)
  (gnus-message 5 "Read %s" gnus-init-file))

(defun gnus-group-check-bogus-groups (&optional silent)
  "Check bogus newsgroups.
If given a prefix, don't ask for confirmation before removing a bogus
group."
  (interactive "P")
  (gnus-check-bogus-newsgroups (and (not silent) (not gnus-expert-user)))
  (gnus-group-list-groups))

(defun gnus-group-find-new-groups (&optional arg)
  "Search for new groups and add them.
Each new group will be treated with `gnus-subscribe-newsgroup-method'.
With 1 C-u, use the `ask-server' method to query the server for new
groups.
With 2 C-u's, use most complete method possible to query the server
for new groups, and subscribe the new groups as zombies."
  (interactive "p")
  (let ((new-groups (gnus-find-new-newsgroups (or arg 1)))
	current-group)
    (gnus-group-list-groups)
    (setq current-group (gnus-group-group-name))
    (dolist (group new-groups)
      (gnus-group-jump-to-group group))
    (when current-group
      (gnus-group-jump-to-group current-group))))

(defun gnus-group-edit-global-kill (&optional article group)
  "Edit the global kill file.
If GROUP, edit that local kill file instead."
  (interactive "P")
  (setq gnus-current-kill-article article)
  (gnus-kill-file-edit-file group)
  (gnus-message 6 "Editing a %s kill file (Type %s to exit)"
		(if group "local" "global")
		(substitute-command-keys "\\[gnus-kill-file-exit]")))

(defun gnus-group-edit-local-kill (article group)
  "Edit a local kill file."
  (interactive (list nil (gnus-group-group-name)))
  (gnus-group-edit-global-kill article group))

(defun gnus-group-force-update ()
  "Update `.newsrc' file."
  (interactive)
  (gnus-save-newsrc-file))

(defvar gnus-backlog-articles)

(defun gnus-group-suspend ()
  "Suspend the current Gnus session.
In fact, cleanup buffers except for group mode buffer.
The hook `gnus-suspend-gnus-hook' is called before actually suspending."
  (interactive)
  (gnus-run-hooks 'gnus-suspend-gnus-hook)
  (gnus-offer-save-summaries)
  ;; Kill Gnus buffers except for group mode buffer.
  (let ((group-buf (get-buffer gnus-group-buffer)))
    (dolist (buf (gnus-buffers))
      (unless (or (eq buf group-buf)
		  (eq buf gnus-dribble-buffer)
		  (with-current-buffer buf
		    (eq major-mode 'message-mode)))
	(gnus-kill-buffer buf)))
    (setq gnus-backlog-articles nil)
    (gnus-kill-gnus-frames)
    (when group-buf
      (bury-buffer group-buf)
      (delete-windows-on group-buf t))))

(defun gnus-group-clear-dribble ()
  "Clear all information from the dribble buffer."
  (interactive)
  (gnus-dribble-clear)
  (gnus-message 7 "Cleared dribble buffer"))

(defun gnus-group-exit ()
  "Quit reading news after updating .newsrc.eld and .newsrc.
The hook `gnus-exit-gnus-hook' is called before actually exiting."
  (interactive)
  (when
      (or noninteractive		;For gnus-batch-kill
	  (not gnus-interactive-exit)	;Without confirmation
	  gnus-expert-user
	  (gnus-y-or-n-p "Are you sure you want to quit reading news? "))
    (gnus-run-hooks 'gnus-exit-gnus-hook)
    ;; Offer to save data from non-quitted summary buffers.
    (gnus-offer-save-summaries)
    ;; Save the newsrc file(s).
    (gnus-save-newsrc-file)
    ;; Kill-em-all.
    (gnus-close-backends)
    ;; Reset everything.
    (gnus-clear-system)
    ;; Allow the user to do things after cleaning up.
    (gnus-run-hooks 'gnus-after-exiting-gnus-hook)))

(defun gnus-group-quit ()
  "Quit reading news without updating .newsrc.eld or .newsrc.
The hook `gnus-exit-gnus-hook' is called before actually exiting."
  (interactive)
  (when (or noninteractive		;For gnus-batch-kill
	    (zerop (buffer-size))
	    (not (gnus-server-opened gnus-select-method))
	    gnus-expert-user
	    (not gnus-current-startup-file)
	    (gnus-yes-or-no-p
	     (format "Quit reading news without saving %s? "
		     (file-name-nondirectory gnus-current-startup-file))))
    (gnus-run-hooks 'gnus-exit-gnus-hook)
    (gnus-configure-windows 'group t)
    (when (and (gnus-buffer-live-p gnus-dribble-buffer)
	       (not (zerop (with-current-buffer gnus-dribble-buffer
			    (buffer-size)))))
      (gnus-dribble-enter
       ";;; Gnus was exited on purpose without saving the .newsrc files."))
    (gnus-dribble-save)
    (gnus-close-backends)
    (gnus-clear-system)
    (gnus-kill-buffer gnus-group-buffer)
    ;; Allow the user to do things after cleaning up.
    (gnus-run-hooks 'gnus-after-exiting-gnus-hook)))

(defun gnus-group-describe-briefly ()
  "Give a one line description of the group mode commands."
  (interactive)
  (gnus-message 7 "%s" (substitute-command-keys "\\<gnus-group-mode-map>\\[gnus-group-read-group]:Select  \\[gnus-group-next-unread-group]:Forward  \\[gnus-group-prev-unread-group]:Backward  \\[gnus-group-exit]:Exit  \\[gnus-info-find-node]:Run Info  \\[gnus-group-describe-briefly]:This help")))

(defun gnus-group-browse-foreign-server (method)
  "Browse a foreign news server.
If called interactively, this function will ask for a select method
 (nntp, nnspool, etc.) and a server address (eg. nntp.some.where).
If not, METHOD should be a list where the first element is the method
and the second element is the address."
  (interactive
   (list (let ((how (gnus-completing-read
		     "Which back end"
		     (mapcar 'car (append gnus-valid-select-methods
					  gnus-server-alist))
		     t (cons "nntp" 0) 'gnus-method-history)))
	   ;; We either got a back end name or a virtual server name.
	   ;; If the first, we also need an address.
	   (if (assoc how gnus-valid-select-methods)
	       (list (intern how)
		     ;; Suggested by mapjph@bath.ac.uk.
		     (gnus-completing-read
		      "Address"
		      gnus-secondary-servers))
	     ;; We got a server name.
	     how))))
  (gnus-browse-foreign-server method))

(defun gnus-group-set-info (info &optional method-only-group part)
  (when (or info part)
    (let* ((entry (gnus-group-entry
		   (or method-only-group (gnus-info-group info))))
	   (part-info info)
	   (info (if method-only-group (nth 2 entry) info))
	   method)
      (when method-only-group
	(unless entry
	  (error "Trying to change non-existent group %s" method-only-group))
	;; We have received parts of the actual group info - either the
	;; select method or the group parameters.  We first check
	;; whether we have to extend the info, and if so, do that.
	(let ((len (length info))
	      (total (if (eq part 'method) 5 6)))
	  (when (< len total)
	    (setcdr (nthcdr (1- len) info)
		    (make-list (- total len) nil)))
	  ;; Then we enter the new info.
	  (setcar (nthcdr (1- total) info) part-info)))
      (unless entry
	;; This is a new group, so we just create it.
	(with-current-buffer gnus-group-buffer
	  (setq method (gnus-info-method info))
	  (when (gnus-server-equal method "native")
	    (setq method nil))
	  (with-current-buffer gnus-group-buffer
	    (if method
		;; It's a foreign group...
		(gnus-group-make-group
		 (gnus-group-real-name (gnus-info-group info))
		 (if (stringp method) method
		   (prin1-to-string (car method)))
		 (and (consp method)
		      (nth 1 (gnus-info-method info)))
		 nil t)
	      ;; It's a native group.
	      (gnus-group-make-group (gnus-info-group info) nil nil nil t)))
	  (gnus-message 6 "Note: New group created")
	  (setq entry
		(gnus-group-entry (gnus-group-prefixed-name
				   (gnus-group-real-name (gnus-info-group info))
				   (or (gnus-info-method info) gnus-select-method))))))
      ;; Whether it was a new group or not, we now have the entry, so we
      ;; can do the update.
      (if entry
	  (progn
	    (setcar (nthcdr 2 entry) info)
	    (when (and (not (eq (car entry) t))
		       (gnus-active (gnus-info-group info)))
	      (setcar entry (length
			     (gnus-list-of-unread-articles (car info))))))
	(error "No such group: %s" (gnus-info-group info))))))

(defun gnus-group-set-method-info (group select-method)
  (gnus-group-set-info select-method group 'method))

(defun gnus-group-set-params-info (group params)
  (gnus-group-set-info params group 'params))

;; Ad-hoc function for inserting data from a different newsrc.eld
;; file.  Use with caution, if at all.
(defun gnus-import-other-newsrc-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (let (form)
      (while (ignore-errors
	       (setq form (read (current-buffer))))
	(when (and (consp form)
		   (eq (cadr form) 'gnus-newsrc-alist))
	  (let ((infos (cadr (nth 2 form))))
	    (dolist (info infos)
	      (when (gnus-get-info (car info))
		(gnus-set-info (car info) info)))))))))

(defun gnus-add-marked-articles (group type articles &optional info force)
  ;; Add ARTICLES of TYPE to the info of GROUP.
  ;; If INFO is non-nil, use that info.  If FORCE is non-nil, don't
  ;; add, but replace marked articles of TYPE with ARTICLES.
  (let ((info (or info (gnus-get-info group)))
	marked m)
    (or (not info)
	(and (not (setq marked (nthcdr 3 info)))
	     (or (null articles)
		 (setcdr (nthcdr 2 info)
			 (list (list (cons type (gnus-compress-sequence
						 articles t)))))))
	(and (not (setq m (assq type (car marked))))
	     (or (null articles)
		 (setcar marked
			 (cons (cons type (gnus-compress-sequence articles t) )
			       (car marked)))))
	(if force
	    (if (null articles)
		(setcar (nthcdr 3 info)
			(gnus-delete-alist type (car marked)))
	      (setcdr m (gnus-compress-sequence articles t)))
	  (setcdr m (gnus-compress-sequence
		     (sort (nconc (gnus-uncompress-range (cdr m))
				  (copy-sequence articles)) '<) t))))))

(defun gnus-add-mark (group mark article)
  "Mark ARTICLE in GROUP with MARK, whether the group is displayed or not."
  (let ((buffer (gnus-summary-buffer-name group)))
    (if (gnus-buffer-live-p buffer)
	(with-current-buffer (get-buffer buffer)
	  (gnus-summary-add-mark article mark))
      (gnus-add-marked-articles group (cdr (assq mark gnus-article-mark-lists))
				(list article)))))

;;;
;;; Group timestamps
;;;

(defun gnus-group-set-timestamp ()
  "Change the timestamp of the current group to the current time.
This function can be used in hooks like `gnus-select-group-hook'
or `gnus-group-catchup-group-hook'."
  (when gnus-newsgroup-name
    (let ((time (current-time)))
      (setcdr (cdr time) nil)
      (gnus-group-set-parameter gnus-newsgroup-name 'timestamp time))))

(defsubst gnus-group-timestamp (group)
  "Return the timestamp for GROUP."
  (gnus-group-get-parameter group 'timestamp t))

(defun gnus-group-timestamp-delta (group)
  "Return the offset in seconds from the timestamp for GROUP to the current time, as a floating point number."
  (let* ((time (or (gnus-group-timestamp group)
		   (list 0 0)))
	 (delta (subtract-time (current-time) time)))
    (+ (* (nth 0 delta) 65536.0)
       (nth 1 delta))))

(defun gnus-group-timestamp-string (group)
  "Return a string of the timestamp for GROUP."
  (let ((time (gnus-group-timestamp group)))
    (if (not time)
	""
      (gnus-time-iso8601 time))))

(defun gnus-group-list-cached (level &optional lowest)
  "List all groups with cached articles.
If the prefix LEVEL is non-nil, it should be a number that says which
level to cut off listing groups.
If LOWEST, don't list groups with level lower than LOWEST.

This command may read the active file."
  (interactive "P")
  (when level
    (setq level (prefix-numeric-value level)))
  (when (or (not level) (>= level gnus-level-zombie))
    (gnus-cache-open))
  (funcall gnus-group-prepare-function
	   (or level gnus-level-subscribed)
	   #'(lambda (info)
	       (let ((marks (gnus-info-marks info)))
		 (assq 'cache marks)))
	   lowest
	   #'(lambda (group)
	       (or (gnus-gethash group
				 gnus-cache-active-hashtb)
		   ;; Cache active file might use "."
		   ;; instead of ":".
		   (gnus-gethash
		    (mapconcat 'identity
			       (split-string group ":")
			       ".")
		    gnus-cache-active-hashtb))))
  (goto-char (point-min))
  (gnus-group-position-point))

(defun gnus-group-list-dormant (level &optional lowest)
  "List all groups with dormant articles.
If the prefix LEVEL is non-nil, it should be a number that says which
level to cut off listing groups.
If LOWEST, don't list groups with level lower than LOWEST.

This command may read the active file."
  (interactive "P")
  (when level
    (setq level (prefix-numeric-value level)))
  (when (or (not level) (>= level gnus-level-zombie))
    (gnus-cache-open))
  (funcall gnus-group-prepare-function
	   (or level gnus-level-subscribed)
	   #'(lambda (info)
	       (let ((marks (gnus-info-marks info)))
		 (assq 'dormant marks)))
	   lowest
	   'ignore)
  (goto-char (point-min))
  (gnus-group-position-point))

(defun gnus-group-list-ticked (level &optional lowest)
  "List all groups with ticked articles.
If the prefix LEVEL is non-nil, it should be a number that says which
level to cut off listing groups.
If LOWEST, don't list groups with level lower than LOWEST.

This command may read the active file."
  (interactive "P")
  (when level
    (setq level (prefix-numeric-value level)))
  (when (or (not level) (>= level gnus-level-zombie))
    (gnus-cache-open))
  (funcall gnus-group-prepare-function
	   (or level gnus-level-subscribed)
	   #'(lambda (info)
	       (let ((marks (gnus-info-marks info)))
		 (assq 'tick marks)))
	   lowest
	   'ignore)
  (goto-char (point-min))
  (gnus-group-position-point))

(defun gnus-group-listed-groups ()
  "Return a list of listed groups."
  (let (point groups)
    (goto-char (point-min))
    (while (setq point (text-property-not-all (point) (point-max)
					      'gnus-group nil))
      (goto-char point)
      (push (symbol-name (get-text-property point 'gnus-group)) groups)
      (forward-char 1))
    groups))

(defun gnus-group-list-plus (&optional args)
  "List groups plus the current selection."
  (interactive "P")
  (let ((gnus-group-listed-groups (gnus-group-listed-groups))
	(gnus-group-list-mode gnus-group-list-mode) ;; Save it.
	func)
    (push last-command-event unread-command-events)
    (if (featurep 'xemacs)
	(push (make-event 'key-press '(key ?A)) unread-command-events)
      (push ?A unread-command-events))
    (let (gnus-pick-mode keys)
      (setq keys (if (featurep 'xemacs)
		     (events-to-keys (read-key-sequence nil))
		   (read-key-sequence nil)))
      (setq func (lookup-key (current-local-map) keys)))
    (if (or (not func)
	    (numberp func))
	(ding)
      (call-interactively func))))

(defun gnus-group-list-flush (&optional args)
  "Flush groups from the current selection."
  (interactive "P")
  (let ((gnus-group-list-option 'flush))
    (gnus-group-list-plus args)))

(defun gnus-group-list-limit (&optional args)
  "List groups limited within the current selection.
If you've limited the groups, you can further limit the selection
with this command.  If you've first limited to groups with
dormant articles with `A ?', you can then further limit with
`A / c', which will then limit to groups with cached articles, giving
you the groups that have both dormant articles and cached articles."
  (interactive "P")
  (let ((gnus-group-list-option 'limit))
    (gnus-group-list-plus args)))

(defun gnus-group-mark-article-read (group article)
  "Mark ARTICLE read."
  (let ((buffer (gnus-summary-buffer-name group))
	(mark gnus-read-mark)
	active n)
    (if (get-buffer buffer)
	(with-current-buffer buffer
	  (setq active gnus-newsgroup-active)
	  (gnus-activate-group group)
	  (when gnus-newsgroup-prepared
	    (when (and gnus-newsgroup-auto-expire
		       (memq mark gnus-auto-expirable-marks))
	      (setq mark gnus-expirable-mark))
	    (setq mark (gnus-request-update-mark
			group article mark))
	    (gnus-mark-article-as-read article mark)
	    (setq gnus-newsgroup-active (gnus-active group))
	    (when active
	      (setq n (1+ (cdr active)))
	      (while (<= n (cdr gnus-newsgroup-active))
		(unless (eq n article)
		  (push n gnus-newsgroup-unselected))
		(setq n (1+ n)))
	      (setq gnus-newsgroup-unselected
		    (sort gnus-newsgroup-unselected '<)))))
      (gnus-activate-group group)
      (gnus-group-make-articles-read group (list article))
      (when (and (gnus-group-auto-expirable-p group)
		 (not (gnus-group-read-only-p group)))
	(gnus-add-marked-articles
	 group 'expire (list article))))))


;;;
;;; Group compaction. -- dvl
;;;

(defun gnus-group-compact-group (group)
  "Compact the current group.
Compaction means removing gaps between article numbers.  Hence, this
operation is only meaningful for back ends using one file per article
\(e.g. nnml).

Note: currently only implemented in nnml."
  (interactive (list (gnus-group-group-name)))
  (unless group
    (error "No group to compact"))
  (unless (gnus-check-backend-function 'request-compact-group group)
    (error "This back end does not support group compaction"))
  (let ((group-decoded (gnus-group-decoded-name group)))
    (gnus-message 6 "\
Compacting group %s... (this may take a long time)"
		  group-decoded)
    (prog1
	(if (not (gnus-request-compact-group group))
	    (gnus-error 3 "Couldn't compact group %s" group-decoded)
	  (gnus-message 6 "Compacting group %s...done" group-decoded)
	  t)
      ;; Invalidate the "original article" buffer which might be out of date.
      ;; #### NOTE: Yes, this might be a bit rude, but since compaction
      ;; #### will not happen very often, I think this is acceptable.
      (let ((original (get-buffer gnus-original-article-buffer)))
	(and original (gnus-kill-buffer original)))
      ;; Update the group line to reflect new information (art number etc).
      (gnus-group-update-group-line))))

(provide 'gnus-group)

;;; gnus-group.el ends here
