;;; gnus-sum.el --- summary mode commands for Gnus

;; Copyright (C) 1996-2012 Free Software Foundation, Inc.

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
(eval-when-compile
  (when (featurep 'xemacs)
    (require 'easy-mmode))) ; for `define-minor-mode'

(defvar tool-bar-mode)
(defvar gnus-tmp-header)

(require 'gnus)
(require 'gnus-group)
(require 'gnus-spec)
(require 'gnus-range)
(require 'gnus-int)
(require 'gnus-undo)
(require 'gnus-util)
(require 'gmm-utils)
(require 'mm-decode)
(require 'nnoo)

(autoload 'gnus-summary-limit-include-cached "gnus-cache" nil t)
(autoload 'gnus-cache-write-active "gnus-cache")
(autoload 'gnus-mailing-list-insinuate "gnus-ml" nil t)
(autoload 'turn-on-gnus-mailing-list-mode "gnus-ml" nil t)
(autoload 'gnus-pick-line-number "gnus-salt" nil t)
(autoload 'mm-uu-dissect "mm-uu")
(autoload 'gnus-article-outlook-deuglify-article "deuglify"
  "Deuglify broken Outlook (Express) articles and redisplay."
  t)
(autoload 'gnus-article-outlook-unwrap-lines "deuglify" nil t)
(autoload 'gnus-article-outlook-repair-attribution "deuglify" nil t)
(autoload 'gnus-article-outlook-rearrange-citation "deuglify" nil t)
(autoload 'nnir-article-rsv "nnir" nil nil 'macro)
(autoload 'nnir-article-group "nnir" nil nil 'macro)

(defcustom gnus-kill-summary-on-exit t
  "*If non-nil, kill the summary buffer when you exit from it.
If nil, the summary will become a \"*Dead Summary*\" buffer, and
it will be killed sometime later."
  :group 'gnus-summary-exit
  :type 'boolean)

(defcustom gnus-summary-next-group-on-exit t
  "If non-nil, go to the next unread newsgroup on summary exit.
See `gnus-group-goto-unread'."
  :link '(custom-manual "(gnus)Group Maneuvering")
  :group 'gnus-summary-exit
  :version "23.1" ;; No Gnus
  :type 'boolean)

(defcustom gnus-summary-stop-at-end-of-message nil
  "If non-nil, don't select the next message when using `SPC'."
  :link '(custom-manual "(gnus)Group Maneuvering")
  :group 'gnus-summary-maneuvering
  :version "24.1"
  :type 'boolean)

(defcustom gnus-fetch-old-headers nil
  "*Non-nil means that Gnus will try to build threads by grabbing old headers.
If an unread article in the group refers to an older, already
read (or just marked as read) article, the old article will not
normally be displayed in the Summary buffer.  If this variable is
t, Gnus will attempt to grab the headers to the old articles, and
thereby build complete threads.  If it has the value `some', all
old headers will be fetched but only enough headers to connect
otherwise loose threads will be displayed.  This variable can
also be a number.  In that case, no more than that number of old
headers will be fetched.  If it has the value `invisible', all
old headers will be fetched, but none will be displayed.

The server has to support NOV for any of this to work.

This feature can seriously impact performance it ignores all
locally cached header entries.  Setting it to t for groups for a
server that doesn't expire articles (such as news.gmane.org),
leads to very slow summary generation."
  :group 'gnus-thread
  :type '(choice (const :tag "off" nil)
		 (const :tag "on" t)
		 (const some)
		 (const invisible)
		 number
		 (sexp :menu-tag "other" t)))

(defcustom gnus-refer-thread-limit 500
  "*The number of old headers to fetch when doing \\<gnus-summary-mode-map>\\[gnus-summary-refer-thread].
If t, fetch all the available old headers."
  :group 'gnus-thread
  :type '(choice number
		 (sexp :menu-tag "other" t)))

(defcustom gnus-refer-thread-use-nnir nil
  "*Use nnir to search an entire server when referring threads. A
nil value will only search for thread-related articles in the
current group."
  :version "24.1"
  :group 'gnus-thread
  :type 'boolean)

(defcustom gnus-summary-make-false-root 'adopt
  "*nil means that Gnus won't gather loose threads.
If the root of a thread has expired or been read in a previous
session, the information necessary to build a complete thread has been
lost.  Instead of having many small sub-threads from this original thread
scattered all over the summary buffer, Gnus can gather them.

If non-nil, Gnus will try to gather all loose sub-threads from an
original thread into one large thread.

If this variable is non-nil, it should be one of `none', `adopt',
`dummy' or `empty'.

If this variable is `none', Gnus will not make a false root, but just
present the sub-threads after another.
If this variable is `dummy', Gnus will create a dummy root that will
have all the sub-threads as children.
If this variable is `adopt', Gnus will make one of the \"children\"
the parent and mark all the step-children as such.
If this variable is `empty', the \"children\" are printed with empty
subject fields.  (Or rather, they will be printed with a string
given by the `gnus-summary-same-subject' variable.)"
  :group 'gnus-thread
  :type '(choice (const :tag "off" nil)
		 (const none)
		 (const dummy)
		 (const adopt)
		 (const empty)))

(defcustom gnus-summary-make-false-root-always nil
  "Always make a false dummy root."
  :version "22.1"
  :group 'gnus-thread
  :type 'boolean)

(defcustom gnus-summary-gather-exclude-subject "^ *$\\|^(none)$"
  "*A regexp to match subjects to be excluded from loose thread gathering.
As loose thread gathering is done on subjects only, that means that
there can be many false gatherings performed.  By rooting out certain
common subjects, gathering might become saner."
  :group 'gnus-thread
  :type 'regexp)

(defcustom gnus-summary-gather-subject-limit nil
  "*Maximum length of subject comparisons when gathering loose threads.
Use nil to compare full subjects.  Setting this variable to a low
number will help gather threads that have been corrupted by
newsreaders chopping off subject lines, but it might also mean that
unrelated articles that have subject that happen to begin with the
same few characters will be incorrectly gathered.

If this variable is `fuzzy', Gnus will use a fuzzy algorithm when
comparing subjects."
  :group 'gnus-thread
  :type '(choice (const :tag "off" nil)
		 (const fuzzy)
		 (sexp :menu-tag "on" t)))

(defcustom gnus-simplify-subject-functions nil
  "List of functions taking a string argument that simplify subjects.
The functions are applied recursively.

Useful functions to put in this list include:
`gnus-simplify-subject-re', `gnus-simplify-subject-fuzzy',
`gnus-simplify-whitespace', and `gnus-simplify-all-whitespace'."
  :group 'gnus-thread
  :type '(repeat function))

(defcustom gnus-simplify-ignored-prefixes nil
  "*Remove matches for this regexp from subject lines when simplifying fuzzily."
  :group 'gnus-thread
  :type '(choice (const :tag "off" nil)
		 regexp))

(defcustom gnus-build-sparse-threads nil
  "*If non-nil, fill in the gaps in threads.
If `some', only fill in the gaps that are needed to tie loose threads
together.  If `more', fill in all leaf nodes that Gnus can find.  If
non-nil and non-`some', fill in all gaps that Gnus manages to guess."
  :group 'gnus-thread
  :type '(choice (const :tag "off" nil)
		 (const some)
		 (const more)
		 (sexp :menu-tag "all" t)))

(defcustom gnus-summary-thread-gathering-function
  'gnus-gather-threads-by-subject
  "*Function used for gathering loose threads.
There are two pre-defined functions: `gnus-gather-threads-by-subject',
which only takes Subjects into consideration; and
`gnus-gather-threads-by-references', which compared the References
headers of the articles to find matches."
  :group 'gnus-thread
  :type '(radio (function-item gnus-gather-threads-by-subject)
		(function-item gnus-gather-threads-by-references)
		(function :tag "other")))

(defcustom gnus-summary-same-subject ""
  "*String indicating that the current article has the same subject as the previous.
This variable will only be used if the value of
`gnus-summary-make-false-root' is `empty'."
  :group 'gnus-summary-format
  :type 'string)

(defcustom gnus-summary-goto-unread nil
  "*If t, many commands will go to the next unread article.
This applies to marking commands as well as other commands that
\"naturally\" select the next article, like, for instance, `SPC' at
the end of an article.

If nil, the marking commands do NOT go to the next unread article
\(they go to the next article instead).  If `never', commands that
usually go to the next unread article, will go to the next article,
whether it is read or not."
  :version "24.1"
  :group 'gnus-summary-marks
  :link '(custom-manual "(gnus)Setting Marks")
  :type '(choice (const :tag "off" nil)
		 (const never)
		 (sexp :menu-tag "on" t)))

(defcustom gnus-summary-default-score 0
  "*Default article score level.
All scores generated by the score files will be added to this score.
If this variable is nil, scoring will be disabled."
  :group 'gnus-score-default
  :type '(choice (const :tag "disable")
		 integer))

(defcustom gnus-summary-default-high-score 0
  "*Default threshold for a high scored article.
An article will be highlighted as high scored if its score is greater
than this score."
  :version "22.1"
  :group 'gnus-score-default
  :type 'integer)

(defcustom gnus-summary-default-low-score 0
  "*Default threshold for a low scored article.
An article will be highlighted as low scored if its score is smaller
than this score."
  :version "22.1"
  :group 'gnus-score-default
  :type 'integer)

(defcustom gnus-summary-zcore-fuzz 0
  "*Fuzziness factor for the zcore in the summary buffer.
Articles with scores closer than this to `gnus-summary-default-score'
will not be marked."
  :group 'gnus-summary-format
  :type 'integer)

(defcustom gnus-simplify-subject-fuzzy-regexp nil
  "*Strings to be removed when doing fuzzy matches.
This can either be a regular expression or list of regular expressions
that will be removed from subject strings if fuzzy subject
simplification is selected."
  :group 'gnus-thread
  :type '(repeat regexp))

(defcustom gnus-show-threads t
  "*If non-nil, display threads in summary mode."
  :group 'gnus-thread
  :type 'boolean)

(defcustom gnus-thread-hide-subtree nil
  "*If non-nil, hide all threads initially.
This can be a predicate specifier which says which threads to hide.
If threads are hidden, you have to run the command
`gnus-summary-show-thread' by hand or select an article."
  :group 'gnus-thread
  :type '(radio (sexp :format "Non-nil\n"
		      :match (lambda (widget value)
			       (not (or (consp value) (functionp value))))
		      :value t)
		(const nil)
		(sexp :tag "Predicate specifier")))

(defcustom gnus-thread-hide-killed t
  "*If non-nil, hide killed threads automatically."
  :group 'gnus-thread
  :type 'boolean)

(defcustom gnus-thread-ignore-subject t
  "*If non-nil, which is the default, ignore subjects and do all threading based on the Reference header.
If nil, articles that have different subjects from their parents will
start separate threads."
  :group 'gnus-thread
  :type 'boolean)

(defcustom gnus-thread-operation-ignore-subject t
  "*If non-nil, subjects will be ignored when doing thread commands.
This affects commands like `gnus-summary-kill-thread' and
`gnus-summary-lower-thread'.

If this variable is nil, articles in the same thread with different
subjects will not be included in the operation in question.  If this
variable is `fuzzy', only articles that have subjects that are fuzzily
equal will be included."
  :group 'gnus-thread
  :type '(choice (const :tag "off" nil)
		 (const fuzzy)
		 (sexp :tag "on" t)))

(defcustom gnus-thread-indent-level 4
  "*Number that says how much each sub-thread should be indented."
  :group 'gnus-thread
  :type 'integer)

(defcustom gnus-auto-extend-newsgroup t
  "*If non-nil, extend newsgroup forward and backward when requested."
  :group 'gnus-summary-choose
  :type 'boolean)

(defcustom gnus-auto-select-first t
  "If non-nil, select an article on group entry.
An article is selected automatically when entering a group
e.g. with \\<gnus-group-mode-map>\\[gnus-group-read-group], or via `gnus-summary-next-page' or
`gnus-summary-catchup-and-goto-next-group'.

Which article is selected is controlled by the variable
`gnus-auto-select-subject'.

If you want to prevent automatic selection of articles in some
newsgroups, set the variable to nil in `gnus-select-group-hook'."
  ;; Commands include...
  ;; \\<gnus-group-mode-map>\\[gnus-group-read-group]
  ;; \\<gnus-summary-mode-map>\\[gnus-summary-next-page]
  ;; \\<gnus-summary-mode-map>\\[gnus-summary-catchup-and-goto-next-group]
  :group 'gnus-group-select
  :type '(choice (const :tag "none" nil)
		 (sexp :menu-tag "first" t)))

(defcustom gnus-auto-select-subject 'unseen-or-unread
  "*Says what subject to place under point when entering a group.

This variable can either be the symbols `first' (place point on the
first subject), `unread' (place point on the subject line of the first
unread article), `best' (place point on the subject line of the
highest-scored article), `unseen' (place point on the subject line of
the first unseen article), `unseen-or-unread' (place point on the subject
line of the first unseen article or, if all articles have been seen, on the
subject line of the first unread article), or a function to be called to
place point on some subject line."
  :version "24.1"
  :group 'gnus-group-select
  :type '(choice (const best)
		 (const unread)
		 (const first)
		 (const unseen)
	         (const unseen-or-unread)
		 (function :tag "Function to call")))

(defcustom gnus-auto-select-next t
  "*If non-nil, offer to go to the next group from the end of the previous.
If the value is t and the next newsgroup is empty, Gnus will exit
summary mode and go back to group mode.  If the value is neither nil
nor t, Gnus will select the following unread newsgroup.  In
particular, if the value is the symbol `quietly', the next unread
newsgroup will be selected without any confirmation, and if it is
`almost-quietly', the next group will be selected without any
confirmation if you are located on the last article in the group.
Finally, if this variable is `slightly-quietly', the `\\<gnus-summary-mode-map>\\[gnus-summary-catchup-and-goto-next-group]' command
will go to the next group without confirmation."
  :group 'gnus-summary-maneuvering
  :type '(choice (const :tag "off" nil)
		 (const quietly)
		 (const almost-quietly)
		 (const slightly-quietly)
		 (sexp :menu-tag "on" t)))

(defcustom gnus-auto-select-same nil
  "*If non-nil, select the next article with the same subject.
If there are no more articles with the same subject, go to
the first unread article."
  :group 'gnus-summary-maneuvering
  :type 'boolean)

(defcustom gnus-auto-select-on-ephemeral-exit 'next-noselect
  "What article should be selected after exiting an ephemeral group.
Valid values include:

`next'
  Select the next article.
`next-unread'
  Select the next unread article.
`next-noselect'
  Move the cursor to the next article.  This is the default.
`next-unread-noselect'
  Move the cursor to the next unread article.

If it has any other value or there is no next (unread) article, the
article selected before entering to the ephemeral group will appear."
  :version "23.1" ;; No Gnus
  :group 'gnus-summary-maneuvering
  :type '(choice :format "%{%t%}:\n %[Value Menu%] %v"
		 (const next) (const next-unread)
		 (const next-noselect) (const next-unread-noselect)
		 (sexp :tag "other" :value nil)))

(defcustom gnus-auto-goto-ignores 'unfetched
  "*Says how to handle unfetched articles when maneuvering.

This variable can either be the symbols nil (maneuver to any
article), `undownloaded' (maneuvering while unplugged ignores articles
that have not been fetched), `always-undownloaded' (maneuvering always
ignores articles that have not been fetched), `unfetched' (maneuvering
ignores articles whose headers have not been fetched).

NOTE: The list of unfetched articles will always be nil when plugged
and, when unplugged, a subset of the undownloaded article list."
  :version "22.1"
  :group 'gnus-summary-maneuvering
  :type '(choice (const :tag "None" nil)
                 (const :tag "Undownloaded when unplugged" undownloaded)
                 (const :tag "Undownloaded" always-undownloaded)
                 (const :tag "Unfetched" unfetched)))

(defcustom gnus-summary-check-current nil
  "*If non-nil, consider the current article when moving.
The \"unread\" movement commands will stay on the same line if the
current article is unread."
  :group 'gnus-summary-maneuvering
  :type 'boolean)

(defcustom gnus-auto-center-summary 2
  "*If non-nil, always center the current summary buffer.
In particular, if `vertical' do only vertical recentering.  If non-nil
and non-`vertical', do both horizontal and vertical recentering."
  :group 'gnus-summary-maneuvering
  :type '(choice (const :tag "none" nil)
		 (const vertical)
		 (integer :tag "height")
		 (sexp :menu-tag "both" t)))

(defcustom gnus-auto-center-group t
  "If non-nil, always center the group buffer."
  :group 'gnus-summary-maneuvering
  :type 'boolean)

(defcustom gnus-show-all-headers nil
  "*If non-nil, don't hide any headers."
  :group 'gnus-article-hiding
  :group 'gnus-article-headers
  :type 'boolean)

(defcustom gnus-summary-ignore-duplicates nil
  "*If non-nil, ignore articles with identical Message-ID headers."
  :group 'gnus-summary
  :type 'boolean)

(defcustom gnus-single-article-buffer nil
  "*If non-nil, display all articles in the same buffer.
If nil, each group will get its own article buffer."
  :version "24.1"
  :group 'gnus-article-various
  :type 'boolean)

(defcustom gnus-widen-article-window nil
  "If non-nil, selecting the article buffer will display only the article buffer."
  :version "24.1"
  :group 'gnus-article-various
  :type 'boolean)

(defcustom gnus-break-pages t
  "*If non-nil, do page breaking on articles.
The page delimiter is specified by the `gnus-page-delimiter'
variable."
  :group 'gnus-article-various
  :type 'boolean)

(defcustom gnus-move-split-methods nil
  "*Variable used to suggest where articles are to be moved to.
It uses the same syntax as the `gnus-split-methods' variable.
However, whereas `gnus-split-methods' specifies file names as targets,
this variable specifies group names."
  :group 'gnus-summary-mail
  :type '(repeat (choice (list :value (fun) function)
			 (cons :value ("" "") regexp (repeat string))
			 (sexp :value nil))))

(defcustom gnus-move-group-prefix-function 'gnus-group-real-prefix
  "Function used to compute default prefix for article move/copy/etc prompts.
The function should take one argument, a group name, and return a
string with the suggested prefix."
  :group 'gnus-summary-mail
  :type 'function)

;; FIXME: Although the custom type is `character' for the following variables,
;; using multibyte characters (Latin-1, UTF-8) doesn't work.  -- rs

(defcustom gnus-unread-mark ?           ;Whitespace
  "*Mark used for unread articles."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-ticked-mark ?!
  "*Mark used for ticked articles."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-dormant-mark ??
  "*Mark used for dormant articles."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-del-mark ?r
  "*Mark used for del'd articles."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-read-mark ?R
  "*Mark used for read articles."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-expirable-mark ?E
  "*Mark used for expirable articles."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-killed-mark ?K
  "*Mark used for killed articles."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-spam-mark ?$
  "*Mark used for spam articles."
  :version "22.1"
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-kill-file-mark ?X
  "*Mark used for articles killed by kill files."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-low-score-mark ?Y
  "*Mark used for articles with a low score."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-catchup-mark ?C
  "*Mark used for articles that are caught up."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-replied-mark ?A
  "*Mark used for articles that have been replied to."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-forwarded-mark ?F
  "*Mark used for articles that have been forwarded."
  :version "22.1"
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-recent-mark ?N
  "*Mark used for articles that are recent."
  :version "22.1"
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-cached-mark ?*
  "*Mark used for articles that are in the cache."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-saved-mark ?S
  "*Mark used for articles that have been saved."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-unseen-mark ?.
  "*Mark used for articles that haven't been seen."
  :version "22.1"
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-no-mark ?               ;Whitespace
  "*Mark used for articles that have no other secondary mark."
  :version "22.1"
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-ancient-mark ?O
  "*Mark used for ancient articles."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-sparse-mark ?Q
  "*Mark used for sparsely reffed articles."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-canceled-mark ?G
  "*Mark used for canceled articles."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-duplicate-mark ?M
  "*Mark used for duplicate articles."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-undownloaded-mark ?-
  "*Mark used for articles that weren't downloaded."
  :version "22.1"
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-downloaded-mark ?+
  "*Mark used for articles that were downloaded."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-downloadable-mark ?%
  "*Mark used for articles that are to be downloaded."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-unsendable-mark ?=
  "*Mark used for articles that won't be sent."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-score-over-mark ?+
  "*Score mark used for articles with high scores."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-score-below-mark ?-
  "*Score mark used for articles with low scores."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-empty-thread-mark ?     ;Whitespace
  "*There is no thread under the article."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-not-empty-thread-mark ?=
  "*There is a thread under the article."
  :group 'gnus-summary-marks
  :type 'character)

(defcustom gnus-view-pseudo-asynchronously nil
  "*If non-nil, Gnus will view pseudo-articles asynchronously."
  :group 'gnus-extract-view
  :type 'boolean)

(defcustom gnus-auto-expirable-marks
  (list gnus-killed-mark gnus-del-mark gnus-catchup-mark
	gnus-low-score-mark gnus-ancient-mark gnus-read-mark
	gnus-duplicate-mark)
  "*The list of marks converted into expiration if a group is auto-expirable."
  :version "24.1"
  :group 'gnus-summary
  :type '(repeat character))

(defcustom gnus-inhibit-user-auto-expire t
  "*If non-nil, user marking commands will not mark an article as expirable, even if the group has auto-expire turned on."
  :version "21.1"
  :group 'gnus-summary
  :type 'boolean)

(defcustom gnus-mark-copied-or-moved-articles-as-expirable nil
  "If non-nil, mark articles copied or moved to auto-expire group as expirable.
If nil, the expirable marks will be unchanged except that the marks
will be removed when copying or moving articles to a group that has
not turned auto-expire on.  If non-nil, articles that have been read
will be marked as expirable when being copied or moved to a group in
which auto-expire is turned on."
  :version "23.2"
  :type 'boolean
  :group 'gnus-summary-marks)

(defcustom gnus-view-pseudos nil
  "*If `automatic', pseudo-articles will be viewed automatically.
If `not-confirm', pseudos will be viewed automatically, and the user
will not be asked to confirm the command."
  :group 'gnus-extract-view
  :type '(choice (const :tag "off" nil)
		 (const automatic)
		 (const not-confirm)))

(defcustom gnus-view-pseudos-separately t
  "*If non-nil, one pseudo-article will be created for each file to be viewed.
If nil, all files that use the same viewing command will be given as a
list of parameters to that command."
  :group 'gnus-extract-view
  :type 'boolean)

(defcustom gnus-insert-pseudo-articles t
  "*If non-nil, insert pseudo-articles when decoding articles."
  :group 'gnus-extract-view
  :type 'boolean)

(defcustom gnus-summary-dummy-line-format
  "   %(:                             :%) %S\n"
  "*The format specification for the dummy roots in the summary buffer.
It works along the same lines as a normal formatting string,
with some simple extensions.

%S  The subject

General format specifiers can also be used.
See `(gnus)Formatting Variables'."
  :link '(custom-manual "(gnus)Formatting Variables")
  :group 'gnus-threading
  :type 'string)

(defcustom gnus-summary-mode-line-format "Gnus: %g [%A] %Z"
  "*The format specification for the summary mode line.
It works along the same lines as a normal formatting string,
with some simple extensions:

%G  Group name
%p  Unprefixed group name
%A  Current article number
%z  Current article score
%V  Gnus version
%U  Number of unread articles in the group
%e  Number of unselected articles in the group
%Z  A string with unread/unselected article counts
%g  Shortish group name
%S  Subject of the current article
%u  User-defined spec
%s  Current score file name
%d  Number of dormant articles
%r  Number of articles that have been marked as read in this session
%E  Number of articles expunged by the score files"
  :group 'gnus-summary-format
  :type 'string)

(defcustom gnus-list-identifiers nil
  "Regexp that matches list identifiers to be removed from subject.
This can also be a list of regexps."
  :version "21.1"
  :group 'gnus-summary-format
  :group 'gnus-article-hiding
  :type '(choice (const :tag "none" nil)
		 (regexp :value ".*")
		 (repeat :value (".*") regexp)))

(defcustom gnus-summary-mark-below 0
  "*Mark all articles with a score below this variable as read.
This variable is local to each summary buffer and usually set by the
score file."
  :group 'gnus-score-default
  :type 'integer)

(defun gnus-widget-reversible-match (widget value)
  "Ignoring WIDGET, convert VALUE to internal form.
VALUE should have the form `FOO' or `(not FOO)', where FOO is an symbol."
  ;; (debug value)
  (or (symbolp value)
      (and (listp value)
           (eq (length value) 2)
           (eq (nth 0 value) 'not)
           (symbolp (nth 1 value)))))

(defun gnus-widget-reversible-to-internal (widget value)
  "Ignoring WIDGET, convert VALUE to internal form.
VALUE should have the form `FOO' or `(not FOO)', where FOO is an atom.
FOO is converted to (FOO nil) and (not FOO) is converted to (FOO t)."
  ;; (debug value)
  (if (atom value)
      (list value nil)
    (list (nth 1 value) t)))

(defun gnus-widget-reversible-to-external (widget value)
  "Ignoring WIDGET, convert VALUE to external form.
VALUE should have the form `(FOO nil)' or `(FOO t)', where FOO is an atom.
\(FOO  nil) is converted to FOO and (FOO t) is converted to (not FOO)."
  ;; (debug value)
  (if (nth 1 value)
      (list 'not (nth 0 value))
    (nth 0 value)))

(define-widget 'gnus-widget-reversible 'group
  "A `group' that convert values."
  :match 'gnus-widget-reversible-match
  :value-to-internal 'gnus-widget-reversible-to-internal
  :value-to-external 'gnus-widget-reversible-to-external)

(defcustom gnus-article-sort-functions '(gnus-article-sort-by-number)
  "*List of functions used for sorting articles in the summary buffer.

Each function takes two articles and returns non-nil if the first
article should be sorted before the other.  If you use more than one
function, the primary sort function should be the last.  You should
probably always include `gnus-article-sort-by-number' in the list of
sorting functions -- preferably first.  Also note that sorting by date
is often much slower than sorting by number, and the sorting order is
very similar.  (Sorting by date means sorting by the time the message
was sent, sorting by number means sorting by arrival time.)

Each item can also be a list `(not F)' where F is a function;
this reverses the sort order.

Ready-made functions include `gnus-article-sort-by-number',
`gnus-article-sort-by-author', `gnus-article-sort-by-subject',
`gnus-article-sort-by-date', `gnus-article-sort-by-random'
and `gnus-article-sort-by-score'.

When threading is turned on, the variable `gnus-thread-sort-functions'
controls how articles are sorted."
  :group 'gnus-summary-sort
  :type '(repeat (gnus-widget-reversible
                  (choice (function-item gnus-article-sort-by-number)
                          (function-item gnus-article-sort-by-author)
                          (function-item gnus-article-sort-by-subject)
                          (function-item gnus-article-sort-by-date)
                          (function-item gnus-article-sort-by-score)
                          (function-item gnus-article-sort-by-random)
                          (function :tag "other"))
                  (boolean :tag "Reverse order"))))


(defcustom gnus-thread-sort-functions '(gnus-thread-sort-by-number)
  "*List of functions used for sorting threads in the summary buffer.
By default, threads are sorted by article number.

Each function takes two threads and returns non-nil if the first
thread should be sorted before the other.  If you use more than one
function, the primary sort function should be the last.  You should
probably always include `gnus-thread-sort-by-number' in the list of
sorting functions -- preferably first.  Also note that sorting by date
is often much slower than sorting by number, and the sorting order is
very similar.  (Sorting by date means sorting by the time the message
was sent, sorting by number means sorting by arrival time.)

Each list item can also be a list `(not F)' where F is a
function; this specifies reversed sort order.

Ready-made functions include `gnus-thread-sort-by-number',
`gnus-thread-sort-by-author', `gnus-thread-sort-by-recipient'
`gnus-thread-sort-by-subject', `gnus-thread-sort-by-date',
`gnus-thread-sort-by-score', `gnus-thread-sort-by-most-recent-number',
`gnus-thread-sort-by-most-recent-date', `gnus-thread-sort-by-random',
and `gnus-thread-sort-by-total-score' (see
`gnus-thread-score-function').

When threading is turned off, the variable
`gnus-article-sort-functions' controls how articles are sorted."
  :group 'gnus-summary-sort
  :type '(repeat
          (gnus-widget-reversible
           (choice (function-item gnus-thread-sort-by-number)
                   (function-item gnus-thread-sort-by-author)
                   (function-item gnus-thread-sort-by-recipient)
                   (function-item gnus-thread-sort-by-subject)
                   (function-item gnus-thread-sort-by-date)
                   (function-item gnus-thread-sort-by-score)
                   (function-item gnus-thread-sort-by-most-recent-number)
                   (function-item gnus-thread-sort-by-most-recent-date)
                   (function-item gnus-thread-sort-by-random)
                   (function-item gnus-thread-sort-by-total-score)
                   (function :tag "other"))
           (boolean :tag "Reverse order"))))

(defcustom gnus-thread-score-function '+
  "*Function used for calculating the total score of a thread.

The function is called with the scores of the article and each
subthread and should then return the score of the thread.

Some functions you can use are `+', `max', or `min'."
  :group 'gnus-summary-sort
  :type 'function)

(defcustom gnus-summary-expunge-below nil
  "All articles that have a score less than this variable will be expunged.
This variable is local to the summary buffers."
  :group 'gnus-score-default
  :type '(choice (const :tag "off" nil)
		 integer))

(defcustom gnus-thread-expunge-below nil
  "All threads that have a total score less than this variable will be expunged.
See `gnus-thread-score-function' for en explanation of what a
\"thread score\" is.

This variable is local to the summary buffers."
  :group 'gnus-threading
  :group 'gnus-score-default
  :type '(choice (const :tag "off" nil)
		 integer))

(defcustom gnus-summary-mode-hook nil
  "*A hook for Gnus summary mode.
This hook is run before any variables are set in the summary buffer."
  :options '(turn-on-gnus-mailing-list-mode gnus-pick-mode)
  :group 'gnus-summary-various
  :type 'hook)

;; Extracted from gnus-xmas-redefine in order to preserve user settings
(when (featurep 'xemacs)
  (add-hook 'gnus-summary-mode-hook 'gnus-xmas-summary-menu-add)
  (add-hook 'gnus-summary-mode-hook 'gnus-xmas-setup-summary-toolbar)
  (add-hook 'gnus-summary-mode-hook
	    'gnus-xmas-switch-horizontal-scrollbar-off))

(defcustom gnus-summary-menu-hook nil
  "*Hook run after the creation of the summary mode menu."
  :group 'gnus-summary-visual
  :type 'hook)

(defcustom gnus-summary-exit-hook nil
  "*A hook called on exit from the summary buffer.
It will be called with point in the group buffer."
  :group 'gnus-summary-exit
  :type 'hook)

(defcustom gnus-summary-prepare-hook nil
  "*A hook called after the summary buffer has been generated.
If you want to modify the summary buffer, you can use this hook."
  :group 'gnus-summary-various
  :type 'hook)

(defcustom gnus-summary-prepared-hook nil
  "*A hook called as the last thing after the summary buffer has been generated."
  :group 'gnus-summary-various
  :type 'hook)

(defcustom gnus-summary-generate-hook nil
  "*A hook run just before generating the summary buffer.
This hook is commonly used to customize threading variables and the
like."
  :group 'gnus-summary-various
  :type 'hook)

(defcustom gnus-select-group-hook nil
  "*A hook called when a newsgroup is selected.

If you'd like to simplify subjects like the
`gnus-summary-next-same-subject' command does, you can use the
following hook:

 (add-hook gnus-select-group-hook
	   (lambda ()
	     (mapcar (lambda (header)
		       (mail-header-set-subject
			header
			(gnus-simplify-subject
			 (mail-header-subject header) 're-only)))
		     gnus-newsgroup-headers)))"
  :group 'gnus-group-select
  :type 'hook)

(defcustom gnus-select-article-hook nil
  "*A hook called when an article is selected."
  :group 'gnus-summary-choose
  :options '(gnus-agent-fetch-selected-article)
  :type 'hook)

(defcustom gnus-visual-mark-article-hook
  (list 'gnus-highlight-selected-summary)
  "*Hook run after selecting an article in the summary buffer.
It is meant to be used for highlighting the article in some way.  It
is not run if `gnus-visual' is nil."
  :group 'gnus-summary-visual
  :type 'hook)

(defcustom gnus-parse-headers-hook nil
  "*A hook called before parsing the headers."
  :group 'gnus-various
  :type 'hook)

(defcustom gnus-exit-group-hook nil
  "*A hook called when exiting summary mode.
This hook is not called from the non-updating exit commands like `Q'."
  :group 'gnus-various
  :type 'hook)

(defcustom gnus-summary-update-hook nil
  "*A hook called when a summary line is changed.
The hook will not be called if `gnus-visual' is nil.

The default function `gnus-summary-highlight-line' will
highlight the line according to the `gnus-summary-highlight'
variable."
  :group 'gnus-summary-visual
  :type 'hook)

(defcustom gnus-mark-article-hook '(gnus-summary-mark-read-and-unread-as-read)
  "*A hook called when an article is selected for the first time.
The hook is intended to mark an article as read (or unread)
automatically when it is selected."
  :group 'gnus-summary-choose
  :type 'hook)

(defcustom gnus-group-no-more-groups-hook nil
  "*A hook run when returning to group mode having no more (unread) groups."
  :group 'gnus-group-select
  :type 'hook)

(defcustom gnus-ps-print-hook nil
  "*A hook run before ps-printing something from Gnus."
  :group 'gnus-summary
  :type 'hook)

(defcustom gnus-summary-article-move-hook nil
  "*A hook called after an article is moved, copied, respooled, or crossposted."
  :version "22.1"
  :group 'gnus-summary
  :type 'hook)

(defcustom gnus-summary-article-delete-hook nil
  "*A hook called after an article is deleted."
  :version "22.1"
  :group 'gnus-summary
  :type 'hook)

(defcustom gnus-summary-article-expire-hook nil
  "*A hook called after an article is expired."
  :version "22.1"
  :group 'gnus-summary
  :type 'hook)

(defcustom gnus-summary-display-arrow
  (and (fboundp 'display-graphic-p)
       (display-graphic-p))
  "*If non-nil, display an arrow highlighting the current article."
  :version "22.1"
  :group 'gnus-summary
  :type 'boolean)

(defcustom gnus-summary-selected-face 'gnus-summary-selected
  "Face used for highlighting the current article in the summary buffer."
  :group 'gnus-summary-visual
  :type 'face)

(defvar gnus-tmp-downloaded nil)

(defcustom gnus-summary-highlight
  '(((eq mark gnus-canceled-mark)
     . gnus-summary-cancelled)
    ((and uncached (> score default-high))
     . gnus-summary-high-undownloaded)
    ((and uncached (< score default-low))
     . gnus-summary-low-undownloaded)
    (uncached
     . gnus-summary-normal-undownloaded)
    ((and (> score default-high)
	  (or (eq mark gnus-dormant-mark)
	      (eq mark gnus-ticked-mark)))
     . gnus-summary-high-ticked)
    ((and (< score default-low)
	  (or (eq mark gnus-dormant-mark)
	      (eq mark gnus-ticked-mark)))
     . gnus-summary-low-ticked)
    ((or (eq mark gnus-dormant-mark)
	 (eq mark gnus-ticked-mark))
     . gnus-summary-normal-ticked)
    ((and (> score default-high) (eq mark gnus-ancient-mark))
     . gnus-summary-high-ancient)
    ((and (< score default-low) (eq mark gnus-ancient-mark))
     . gnus-summary-low-ancient)
    ((eq mark gnus-ancient-mark)
     . gnus-summary-normal-ancient)
    ((and (> score default-high) (eq mark gnus-unread-mark))
     . gnus-summary-high-unread)
    ((and (< score default-low) (eq mark gnus-unread-mark))
     . gnus-summary-low-unread)
    ((eq mark gnus-unread-mark)
     . gnus-summary-normal-unread)
    ((> score default-high)
     . gnus-summary-high-read)
    ((< score default-low)
     . gnus-summary-low-read)
    (t
     . gnus-summary-normal-read))
  "*Controls the highlighting of summary buffer lines.

A list of (FORM . FACE) pairs.  When deciding how a particular
summary line should be displayed, each form is evaluated.  The content
of the face field after the first true form is used.  You can change
how those summary lines are displayed, by editing the face field.

You can use the following variables in the FORM field.

score:        The article's score.
default:      The default article score.
default-high: The default score for high scored articles.
default-low:  The default score for low scored articles.
below:        The score below which articles are automatically marked as read.
mark:         The article's mark.
uncached:     Non-nil if the article is uncached."
  :group 'gnus-summary-visual
  :type '(repeat (cons (sexp :tag "Form" nil)
		       face)))
(put 'gnus-summary-highlight 'risky-local-variable t)

(defcustom gnus-alter-header-function nil
  "Function called to allow alteration of article header structures.
The function is called with one parameter, the article header vector,
which it may alter in any way."
  :type '(choice (const :tag "None" nil)
		 function)
  :group 'gnus-summary)

(defvar gnus-decode-encoded-word-function 'mail-decode-encoded-word-string
  "Function used to decode a string with encoded words.")

(defvar gnus-decode-encoded-address-function
  'mail-decode-encoded-address-string
  "Function used to decode addresses with encoded words.")

(defcustom gnus-extra-headers '(To Cc Keywords Gcc Newsgroups)
  "*Extra headers to parse."
  :version "24.1"                       ; added Cc Keywords Gcc
  :group 'gnus-summary
  :type '(repeat symbol))

(defcustom gnus-ignored-from-addresses
  (and user-mail-address
       (not (string= user-mail-address ""))
       (regexp-quote user-mail-address))
  "*From headers that may be suppressed in favor of To headers.
This can be a regexp or a list of regexps."
  :version "21.1"
  :group 'gnus-summary
  :type '(choice regexp
		 (repeat :tag "Regexp List" regexp)))

(defsubst gnus-ignored-from-addresses ()
  (gmm-regexp-concat gnus-ignored-from-addresses))

(defcustom gnus-summary-to-prefix "-> "
  "*String prefixed to the To field in the summary line when
using `gnus-ignored-from-addresses'."
  :version "22.1"
  :group 'gnus-summary
  :type 'string)

(defcustom gnus-summary-newsgroup-prefix "=> "
  "*String prefixed to the Newsgroup field in the summary
line when using `gnus-ignored-from-addresses'."
  :version "22.1"
  :group 'gnus-summary
  :type 'string)

(defcustom gnus-newsgroup-ignored-charsets '(unknown-8bit x-unknown)
  "List of charsets that should be ignored.
When these charsets are used in the \"charset\" parameter, the
default charset will be used instead."
  :version "21.1"
  :type '(repeat symbol)
  :group 'gnus-charset)

(defcustom gnus-newsgroup-maximum-articles nil
  "The maximum number of articles a newsgroup.
If this is a number, old articles in a newsgroup exceeding this number
are silently ignored.  If it is nil, no article is ignored.  Note that
setting this variable to a number might prevent you from reading very
old articles."
  :group 'gnus-group-select
  :version "22.2"
  :type '(choice (const :tag "No limit" nil)
		 integer))

(gnus-define-group-parameter
 ignored-charsets
 :type list
 :function-document
 "Return the ignored charsets of GROUP."
 :variable gnus-group-ignored-charsets-alist
 :variable-default
 '(("alt\\.chinese\\.text" iso-8859-1))
 :variable-document
 "Alist of regexps (to match group names) and charsets that should be ignored.
When these charsets are used in the \"charset\" parameter, the
default charset will be used instead."
 :variable-group gnus-charset
 :variable-type '(repeat (cons (regexp :tag "Group")
			       (repeat symbol)))
 :parameter-type '(choice :tag "Ignored charsets"
			  :value nil
			  (repeat (symbol)))
 :parameter-document       "\
List of charsets that should be ignored.

When these charsets are used in the \"charset\" parameter, the
default charset will be used instead.")

(defcustom gnus-group-highlight-words-alist nil
  "Alist of group regexps and highlight regexps.
This variable uses the same syntax as `gnus-emphasis-alist'."
  :version "21.1"
  :type '(repeat (cons (regexp :tag "Group")
		       (repeat (list (regexp :tag "Highlight regexp")
				     (number :tag "Group for entire word" 0)
				     (number :tag "Group for displayed part" 0)
				     (symbol :tag "Face"
					     gnus-emphasis-highlight-words)))))
  :group 'gnus-summary-visual)

(defcustom gnus-summary-show-article-charset-alist
  nil
  "Alist of number and charset.
The article will be shown with the charset corresponding to the
numbered argument.
For example: ((1 . cn-gb-2312) (2 . big5))."
  :version "21.1"
  :type '(repeat (cons (number :tag "Argument" 1)
		       (symbol :tag "Charset")))
  :group 'gnus-charset)

(defcustom gnus-preserve-marks t
  "Whether marks are preserved when moving, copying and respooling messages."
  :version "21.1"
  :type 'boolean
  :group 'gnus-summary-marks)

(defcustom gnus-propagate-marks nil
  "If non-nil, Gnus will store and retrieve marks from the backends.
This means that marks will be stored both in .newsrc.eld and in
the backend, and will slow operation down somewhat."
  :type 'boolean
  :group 'gnus-summary-marks)

(defcustom gnus-alter-articles-to-read-function nil
  "Function to be called to alter the list of articles to be selected."
  :type '(choice (const nil) function)
  :group 'gnus-summary)

(defcustom gnus-orphan-score nil
  "*All orphans get this score added.  Set in the score file."
  :group 'gnus-score-default
  :type '(choice (const nil)
		 integer))

(defcustom gnus-summary-save-parts-default-mime "image/.*"
  "*A regexp to match MIME parts when saving multiple parts of a
message with `gnus-summary-save-parts' (\\<gnus-summary-mode-map>\\[gnus-summary-save-parts]).
This regexp will be used by default when prompting the user for which
type of files to save."
  :group 'gnus-summary
  :type 'regexp)

(defcustom gnus-read-all-available-headers nil
  "Whether Gnus should parse all headers made available to it.
This is mostly relevant for slow back ends where the user may
wish to widen the summary buffer to include all headers
that were fetched."
  :version "22.1"
  :group 'gnus-summary
  :type '(choice boolean regexp))

(defcustom gnus-summary-pipe-output-default-command nil
  "Command (and optional arguments) used to pipe article to subprocess.
This will be used as the default command if it is non-nil.  The value
will be updated if you modify it when executing the command
`gnus-summary-pipe-output' or the function `gnus-summary-save-in-pipe'."
  :version "23.1" ;; No Gnus
  :group 'gnus-summary
  :type '(radio (const :tag "None" nil) (string :tag "Command")))

(defcustom gnus-summary-muttprint-program "muttprint"
  "Command (and optional arguments) used to run Muttprint.
The value will be updated if you modify it when executing the command
`gnus-summary-muttprint'."
  :version "22.1"
  :group 'gnus-summary
  :type 'string)

(defcustom gnus-article-loose-mime t
  "If non-nil, don't require MIME-Version header.
Some brain-damaged MUA/MTA, e.g. Lotus Domino 5.0.6 clients, does not
supply the MIME-Version header or deliberately strip it from the mail.
If non-nil (the default), Gnus will treat some articles as MIME
even if the MIME-Version header is missing."
  :version "22.1"
  :type 'boolean
  :group 'gnus-article-mime)

(defcustom gnus-article-emulate-mime t
  "If non-nil, use MIME emulation for uuencode and the like.
This means that Gnus will search message bodies for text that look
like uuencoded bits, yEncoded bits, and so on, and present that using
the normal Gnus MIME machinery."
  :version "22.1"
  :type 'boolean
  :group 'gnus-article-mime)

;;; Internal variables

(defvar gnus-summary-display-cache nil)
(defvar gnus-article-mime-handles nil)
(defvar gnus-article-decoded-p nil)
(defvar gnus-article-charset nil)
(defvar gnus-article-ignored-charsets nil)
(defvar gnus-scores-exclude-files nil)
(defvar gnus-page-broken nil)

(defvar gnus-original-article nil)
(defvar gnus-article-internal-prepare-hook nil)
(defvar gnus-newsgroup-process-stack nil)

(defvar gnus-thread-indent-array nil)
(defvar gnus-thread-indent-array-level gnus-thread-indent-level)
(defvar gnus-sort-gathered-threads-function 'gnus-thread-sort-by-number
  "Function called to sort the articles within a thread after it has been gathered together.")

(defvar gnus-summary-save-parts-type-history nil)
(defvar gnus-summary-save-parts-last-directory mm-default-directory)

;; Avoid highlighting in kill files.
(defvar gnus-summary-inhibit-highlight nil)
(defvar gnus-newsgroup-selected-overlay nil)
(defvar gnus-inhibit-limiting nil)
(defvar gnus-newsgroup-adaptive-score-file nil)
(defvar gnus-current-score-file nil)
(defvar gnus-current-move-group nil)
(defvar gnus-current-copy-group nil)
(defvar gnus-current-crosspost-group nil)
(defvar gnus-newsgroup-display nil)

(defvar gnus-newsgroup-dependencies nil)
(defvar gnus-newsgroup-adaptive nil)
(defvar gnus-summary-display-article-function nil)
(defvar gnus-summary-highlight-line-function nil
  "Function called after highlighting a summary line.")

(defvar gnus-summary-line-format-alist
  `((?N ,(macroexpand '(mail-header-number gnus-tmp-header)) ?d)
    (?S ,(macroexpand '(mail-header-subject gnus-tmp-header)) ?s)
    (?s gnus-tmp-subject-or-nil ?s)
    (?n gnus-tmp-name ?s)
    (?A (car (cdr (funcall gnus-extract-address-components gnus-tmp-from)))
	?s)
    (?a (or (car (funcall gnus-extract-address-components gnus-tmp-from))
	    gnus-tmp-from) ?s)
    (?F gnus-tmp-from ?s)
    (?x ,(macroexpand '(mail-header-xref gnus-tmp-header)) ?s)
    (?D ,(macroexpand '(mail-header-date gnus-tmp-header)) ?s)
    (?d (gnus-dd-mmm (mail-header-date gnus-tmp-header)) ?s)
    (?o (gnus-date-iso8601 (mail-header-date gnus-tmp-header)) ?s)
    (?M ,(macroexpand '(mail-header-id gnus-tmp-header)) ?s)
    (?r ,(macroexpand '(mail-header-references gnus-tmp-header)) ?s)
    (?c (or (mail-header-chars gnus-tmp-header) 0) ?d)
    (?k (gnus-summary-line-message-size gnus-tmp-header) ?s)
    (?L gnus-tmp-lines ?s)
    (?Z (or (nnir-article-rsv (mail-header-number gnus-tmp-header))
	    0) ?d)
    (?G (or (nnir-article-group (mail-header-number gnus-tmp-header))
	    "") ?s)
    (?g (or (gnus-group-short-name
	     (nnir-article-group (mail-header-number gnus-tmp-header)))
	    "") ?s)
    (?O gnus-tmp-downloaded ?c)
    (?I gnus-tmp-indentation ?s)
    (?T (if (= gnus-tmp-level 0) "" (make-string (frame-width) ? )) ?s)
    (?R gnus-tmp-replied ?c)
    (?\[ gnus-tmp-opening-bracket ?c)
    (?\] gnus-tmp-closing-bracket ?c)
    (?\> (make-string gnus-tmp-level ? ) ?s)
    (?\< (make-string (max 0 (- 20 gnus-tmp-level)) ? ) ?s)
    (?i gnus-tmp-score ?d)
    (?z gnus-tmp-score-char ?c)
    (?V (gnus-thread-total-score (and (boundp 'thread) (car thread))) ?d)
    (?U gnus-tmp-unread ?c)
    (?f (gnus-summary-from-or-to-or-newsgroups gnus-tmp-header gnus-tmp-from)
	?s)
    (?t (gnus-summary-number-of-articles-in-thread
	 (and (boundp 'thread) (car thread)) gnus-tmp-level)
	?d)
    (?e (gnus-summary-number-of-articles-in-thread
	 (and (boundp 'thread) (car thread)) gnus-tmp-level t)
	?c)
    (?u gnus-tmp-user-defined ?s)
    (?P (gnus-pick-line-number) ?d)
    (?B gnus-tmp-thread-tree-header-string ?s)
    (user-date (gnus-user-date
		,(macroexpand '(mail-header-date gnus-tmp-header))) ?s))
  "An alist of format specifications that can appear in summary lines.
These are paired with what variables they correspond with, along with
the type of the variable (string, integer, character, etc).")

(defvar gnus-summary-dummy-line-format-alist
  `((?S gnus-tmp-subject ?s)
    (?N gnus-tmp-number ?d)
    (?u gnus-tmp-user-defined ?s)))

(defvar gnus-summary-mode-line-format-alist
  `((?G gnus-tmp-group-name ?s)
    (?g (gnus-short-group-name gnus-tmp-group-name) ?s)
    (?p (gnus-group-real-name gnus-tmp-group-name) ?s)
    (?A gnus-tmp-article-number ?d)
    (?Z gnus-tmp-unread-and-unselected ?s)
    (?V gnus-version ?s)
    (?U gnus-tmp-unread-and-unticked ?d)
    (?S gnus-tmp-subject ?s)
    (?e gnus-tmp-unselected ?d)
    (?u gnus-tmp-user-defined ?s)
    (?d (length gnus-newsgroup-dormant) ?d)
    (?t (length gnus-newsgroup-marked) ?d)
    (?h (length gnus-newsgroup-spam-marked) ?d)
    (?r (length gnus-newsgroup-reads) ?d)
    (?z (gnus-summary-article-score gnus-tmp-article-number) ?d)
    (?E gnus-newsgroup-expunged-tally ?d)
    (?s (gnus-current-score-file-nondirectory) ?s)))

;; This is here rather than in gnus-art for compilation reasons.
(defvar gnus-article-mode-line-format-alist
  (nconc '((?w (gnus-article-wash-status) ?s)
	   (?m (gnus-article-mime-part-status) ?s))
	 gnus-summary-mode-line-format-alist))

(defvar gnus-last-search-regexp nil
  "Default regexp for article search command.")

(defvar gnus-last-shell-command nil
  "Default shell command on article.")

(defvar gnus-newsgroup-agentized nil
  "Locally bound in each summary buffer to indicate whether the server has been agentized.")
(defvar gnus-newsgroup-begin nil)
(defvar gnus-newsgroup-end nil)
(defvar gnus-newsgroup-last-rmail nil)
(defvar gnus-newsgroup-last-mail nil)
(defvar gnus-newsgroup-last-folder nil)
(defvar gnus-newsgroup-last-file nil)
(defvar gnus-newsgroup-last-directory nil)
(defvar gnus-newsgroup-auto-expire nil)
(defvar gnus-newsgroup-active nil)
(defvar gnus-newsgroup-highest nil)

(defvar gnus-newsgroup-data nil)
(defvar gnus-newsgroup-data-reverse nil)
(defvar gnus-newsgroup-limit nil)
(defvar gnus-newsgroup-limits nil)
(defvar gnus-summary-use-undownloaded-faces nil)

(defvar gnus-newsgroup-unreads nil
  "Sorted list of unread articles in the current newsgroup.")

(defvar gnus-newsgroup-unselected nil
  "Sorted list of unselected unread articles in the current newsgroup.")

(defvar gnus-newsgroup-reads nil
  "Alist of read articles and article marks in the current newsgroup.")

(defvar gnus-newsgroup-expunged-tally nil)

(defvar gnus-newsgroup-marked nil
  "Sorted list of ticked articles in the current newsgroup (a subset of unread art).")

(defvar gnus-newsgroup-spam-marked nil
  "List of ranges of articles that have been marked as spam.")

(defvar gnus-newsgroup-killed nil
  "List of ranges of articles that have been through the scoring process.")

(defvar gnus-newsgroup-cached nil
  "Sorted list of articles that come from the article cache.")

(defvar gnus-newsgroup-saved nil
  "List of articles that have been saved.")

(defvar gnus-newsgroup-kill-headers nil)

(defvar gnus-newsgroup-replied nil
  "List of articles that have been replied to in the current newsgroup.")

(defvar gnus-newsgroup-forwarded nil
  "List of articles that have been forwarded in the current newsgroup.")

(defvar gnus-newsgroup-expirable nil
  "Sorted list of articles in the current newsgroup that can be expired.")

(defvar gnus-newsgroup-processable nil
  "List of articles in the current newsgroup that can be processed.")

(defvar gnus-newsgroup-downloadable nil
  "Sorted list of articles in the current newsgroup that can be processed.")

(defvar gnus-newsgroup-unfetched nil
  "Sorted list of articles in the current newsgroup whose headers have
not been fetched into the agent.

This list will always be a subset of gnus-newsgroup-undownloaded.")

(defvar gnus-newsgroup-undownloaded nil
  "List of articles in the current newsgroup that haven't been downloaded.")

(defvar gnus-newsgroup-unsendable nil
  "List of articles in the current newsgroup that won't be sent.")

(defvar gnus-newsgroup-bookmarks nil
  "List of articles in the current newsgroup that have bookmarks.")

(defvar gnus-newsgroup-dormant nil
  "Sorted list of dormant articles in the current newsgroup.")

(defvar gnus-newsgroup-unseen nil
  "List of unseen articles in the current newsgroup.")

(defvar gnus-newsgroup-seen nil
  "Range of seen articles in the current newsgroup.")

(defvar gnus-newsgroup-articles nil
  "List of articles in the current newsgroup.")

(defvar gnus-newsgroup-scored nil
  "List of scored articles in the current newsgroup.")

(defvar gnus-newsgroup-headers nil
  "List of article headers in the current newsgroup.")

(defvar gnus-newsgroup-threads nil)

(defvar gnus-newsgroup-prepared nil
  "Whether the current group has been prepared properly.")

(defvar gnus-newsgroup-ancient nil
  "List of `gnus-fetch-old-headers' articles in the current newsgroup.")

(defvar gnus-newsgroup-sparse nil)

(defvar gnus-current-article nil)
(defvar gnus-article-current nil)
(defvar gnus-current-headers nil)
(defvar gnus-have-all-headers nil)
(defvar gnus-last-article nil)
(defvar gnus-newsgroup-history nil)
(defvar gnus-newsgroup-charset nil)
(defvar gnus-newsgroup-ephemeral-charset nil)
(defvar gnus-newsgroup-ephemeral-ignored-charsets nil)

(defvar gnus-article-before-search nil)

(defvar gnus-summary-local-variables
  '(gnus-newsgroup-name

    ;; Marks lists
    gnus-newsgroup-unreads
    gnus-newsgroup-unselected
    gnus-newsgroup-marked
    gnus-newsgroup-spam-marked
    gnus-newsgroup-reads
    gnus-newsgroup-saved
    gnus-newsgroup-replied
    gnus-newsgroup-forwarded
    gnus-newsgroup-expirable
    gnus-newsgroup-killed
    gnus-newsgroup-unseen
    gnus-newsgroup-seen
    gnus-newsgroup-cached
    gnus-newsgroup-downloadable
    gnus-newsgroup-undownloaded
    gnus-newsgroup-unsendable

    gnus-newsgroup-begin gnus-newsgroup-end
    gnus-newsgroup-last-rmail gnus-newsgroup-last-mail
    gnus-newsgroup-last-folder gnus-newsgroup-last-file
    gnus-newsgroup-last-directory
    gnus-newsgroup-auto-expire
    gnus-newsgroup-processable
    gnus-newsgroup-unfetched
    gnus-newsgroup-articles
    gnus-newsgroup-bookmarks gnus-newsgroup-dormant
    gnus-newsgroup-headers gnus-newsgroup-threads
    gnus-newsgroup-prepared gnus-summary-highlight-line-function
    gnus-current-article gnus-current-headers gnus-have-all-headers
    gnus-last-article gnus-article-internal-prepare-hook
    (gnus-summary-article-delete-hook . global)
    (gnus-summary-article-move-hook . global)
    gnus-newsgroup-dependencies gnus-newsgroup-selected-overlay
    gnus-newsgroup-scored gnus-newsgroup-kill-headers
    gnus-thread-expunge-below
    gnus-score-alist gnus-current-score-file
    (gnus-summary-expunge-below . global)
    (gnus-summary-mark-below . global)
    (gnus-orphan-score . global)
    gnus-newsgroup-active gnus-scores-exclude-files
    gnus-newsgroup-highest
    gnus-newsgroup-history gnus-newsgroup-ancient
    gnus-newsgroup-sparse gnus-newsgroup-process-stack
    (gnus-newsgroup-adaptive . gnus-use-adaptive-scoring)
    gnus-newsgroup-adaptive-score-file (gnus-reffed-article-number . -1)
    (gnus-newsgroup-expunged-tally . 0)
    gnus-cache-removable-articles
    gnus-newsgroup-data gnus-newsgroup-data-reverse
    gnus-newsgroup-limit gnus-newsgroup-limits
    gnus-newsgroup-charset gnus-newsgroup-display
    gnus-summary-use-undownloaded-faces)
  "Variables that are buffer-local to the summary buffers.")

(defvar gnus-newsgroup-variables nil
  "A list of variables that have separate values in different newsgroups.
A list of newsgroup (summary buffer) local variables, or cons of
variables and their default expressions to be evalled (when the default
values are not nil), that should be made global while the summary buffer
is active.

Note: The default expressions will be evaluated (using function `eval')
before assignment to the local variable rather than just assigned to it.
If the default expression is the symbol `global', that symbol will not
be evaluated but the global value of the local variable will be used
instead.

These variables can be used to set variables in the group parameters
while still allowing them to affect operations done in other buffers.
For example:

\(setq gnus-newsgroup-variables
     '(message-use-followup-to
       (gnus-visible-headers .
	 \"^From:\\\\|^Newsgroups:\\\\|^Subject:\\\\|^Date:\\\\|^To:\")))
")

(eval-when-compile
  ;; Bind features so that require will believe that gnus-sum has
  ;; already been loaded (avoids infinite recursion)
  (let ((features (cons 'gnus-sum features)))
    (require 'gnus-art)))

;; MIME stuff.

(defvar gnus-decode-encoded-word-methods
  '(mail-decode-encoded-word-string)
  "List of methods used to decode encoded words.

This variable is a list of FUNCTION or (REGEXP . FUNCTION).  If item
is FUNCTION, FUNCTION will be apply to all newsgroups.  If item is a
\(REGEXP . FUNCTION), FUNCTION will be applied only to the newsgroups
whose names match REGEXP.

For example:
\((\"chinese\" . gnus-decode-encoded-word-string-by-guess)
 mail-decode-encoded-word-string
 (\"chinese\" . rfc1843-decode-string))")

(defvar gnus-decode-encoded-word-methods-cache nil)

(defun gnus-multi-decode-encoded-word-string (string)
  "Apply the functions from `gnus-encoded-word-methods' that match."
  (unless (and gnus-decode-encoded-word-methods-cache
	       (eq gnus-newsgroup-name
		   (car gnus-decode-encoded-word-methods-cache)))
    (setq gnus-decode-encoded-word-methods-cache (list gnus-newsgroup-name))
    (dolist (method gnus-decode-encoded-word-methods)
      (if (symbolp method)
	  (nconc gnus-decode-encoded-word-methods-cache (list method))
	(if (and gnus-newsgroup-name
		 (string-match (car method) gnus-newsgroup-name))
	    (nconc gnus-decode-encoded-word-methods-cache
		   (list (cdr method)))))))
  (dolist (method (cdr gnus-decode-encoded-word-methods-cache) string)
    (setq string (funcall method string))))

;; Subject simplification.

(defun gnus-simplify-whitespace (str)
  "Remove excessive whitespace from STR."
  ;; Multiple spaces.
  (while (string-match "[ \t][ \t]+" str)
    (setq str (concat (substring str 0 (match-beginning 0))
			" "
			(substring str (match-end 0)))))
  ;; Leading spaces.
  (when (string-match "^[ \t]+" str)
    (setq str (substring str (match-end 0))))
  ;; Trailing spaces.
  (when (string-match "[ \t]+$" str)
    (setq str (substring str 0 (match-beginning 0))))
  str)

(defun gnus-simplify-all-whitespace (str)
  "Remove all whitespace from STR."
  (while (string-match "[ \t\n]+" str)
    (setq str (replace-match "" nil nil str)))
  str)

(defsubst gnus-simplify-subject-re (subject)
  "Remove \"Re:\" from subject lines."
  (if (string-match message-subject-re-regexp subject)
      (substring subject (match-end 0))
    subject))

(defun gnus-simplify-subject (subject &optional re-only)
  "Remove `Re:' and words in parentheses.
If RE-ONLY is non-nil, strip leading `Re:'s only."
  (let ((case-fold-search t))		;Ignore case.
    ;; Remove `Re:', `Re^N:', `Re(n)', and `Re[n]:'.
    (when (string-match "\\`\\(re\\([[(^][0-9]+[])]?\\)?:[ \t]*\\)+" subject)
      (setq subject (substring subject (match-end 0))))
    ;; Remove uninteresting prefixes.
    (when (and (not re-only)
	       gnus-simplify-ignored-prefixes
	       (string-match gnus-simplify-ignored-prefixes subject))
      (setq subject (substring subject (match-end 0))))
    ;; Remove words in parentheses from end.
    (unless re-only
      (while (string-match "[ \t\n]*([^()]*)[ \t\n]*\\'" subject)
	(setq subject (substring subject 0 (match-beginning 0)))))
    ;; Return subject string.
    subject))

;; Remove any leading "re:"s, any trailing paren phrases, and simplify
;; all whitespace.
(defsubst gnus-simplify-buffer-fuzzy-step (regexp &optional newtext)
  (goto-char (point-min))
  (while (re-search-forward regexp nil t)
    (replace-match (or newtext ""))))

(defun gnus-simplify-buffer-fuzzy (regexp)
  "Simplify string in the buffer fuzzily.
The string in the accessible portion of the current buffer is simplified.
It is assumed to be a single-line subject.
Whitespace is generally cleaned up, and miscellaneous leading/trailing
matter is removed.  Additional things can be deleted by setting
`gnus-simplify-subject-fuzzy-regexp'."
  (let ((case-fold-search t)
	(modified-tick))
    (gnus-simplify-buffer-fuzzy-step "\t" " ")

    (while (not (eq modified-tick (buffer-modified-tick)))
      (setq modified-tick (buffer-modified-tick))
      (cond
       ((listp regexp)
	(mapc 'gnus-simplify-buffer-fuzzy-step regexp))
       (regexp
	(gnus-simplify-buffer-fuzzy-step regexp)))
      (gnus-simplify-buffer-fuzzy-step "^ *\\[[-+?*!][-+?*!]\\] *")
      (gnus-simplify-buffer-fuzzy-step
       "^ *\\(re\\|fw\\|fwd\\)[[{(^0-9]*[])}]?[:;] *")
      (gnus-simplify-buffer-fuzzy-step "^[[].*:\\( .*\\)[]]$" "\\1"))

    (gnus-simplify-buffer-fuzzy-step " *[[{(][^()\n]*[]})] *$")
    (gnus-simplify-buffer-fuzzy-step "  +" " ")
    (gnus-simplify-buffer-fuzzy-step " $")
    (gnus-simplify-buffer-fuzzy-step "^ +")))

(defun gnus-simplify-subject-fuzzy (subject)
  "Simplify a subject string fuzzily.
See `gnus-simplify-buffer-fuzzy' for details."
  (save-excursion
    (let ((regexp gnus-simplify-subject-fuzzy-regexp))
      (gnus-set-work-buffer)
      (let ((case-fold-search t))
	;; Remove uninteresting prefixes.
	(when (and gnus-simplify-ignored-prefixes
		   (string-match gnus-simplify-ignored-prefixes subject))
	  (setq subject (substring subject (match-end 0))))
	(insert subject)
	(inline (gnus-simplify-buffer-fuzzy regexp))
	(buffer-string)))))

(defsubst gnus-simplify-subject-fully (subject)
  "Simplify a subject string according to `gnus-summary-gather-subject-limit'."
  (cond
   (gnus-simplify-subject-functions
    (gnus-map-function gnus-simplify-subject-functions subject))
   ((null gnus-summary-gather-subject-limit)
    (gnus-simplify-subject-re subject))
   ((eq gnus-summary-gather-subject-limit 'fuzzy)
    (gnus-simplify-subject-fuzzy subject))
   ((numberp gnus-summary-gather-subject-limit)
    (truncate-string-to-width (gnus-simplify-subject-re subject)
			      gnus-summary-gather-subject-limit))
   (t
    subject)))

(defsubst gnus-subject-equal (s1 s2 &optional simple-first)
  "Check whether two subjects are equal.
If optional argument SIMPLE-FIRST is t, first argument is already
simplified."
  (cond
   ((null simple-first)
    (equal (gnus-simplify-subject-fully s1)
	   (gnus-simplify-subject-fully s2)))
   (t
    (equal s1
	   (gnus-simplify-subject-fully s2)))))

(defun gnus-summary-bubble-group ()
  "Increase the score of the current group.
This is a handy function to add to `gnus-summary-exit-hook' to
increase the score of each group you read."
  (gnus-group-add-score gnus-newsgroup-name))


;;;
;;; Gnus summary mode
;;;

(put 'gnus-summary-mode 'mode-class 'special)

(defvar gnus-article-commands-menu)

;; Non-orthogonal keys

(gnus-define-keys gnus-summary-mode-map
  " " gnus-summary-next-page
  "\177" gnus-summary-prev-page
  [delete] gnus-summary-prev-page
  [backspace] gnus-summary-prev-page
  "\r" gnus-summary-scroll-up
  "\M-\r" gnus-summary-scroll-down
  "n" gnus-summary-next-unread-article
  "p" gnus-summary-prev-unread-article
  "N" gnus-summary-next-article
  "P" gnus-summary-prev-article
  "\M-\C-n" gnus-summary-next-same-subject
  "\M-\C-p" gnus-summary-prev-same-subject
  "\M-n" gnus-summary-next-unread-subject
  "\M-p" gnus-summary-prev-unread-subject
  "." gnus-summary-first-unread-article
  "," gnus-summary-best-unread-article
  "\M-s" gnus-summary-search-article-forward
  "\M-r" gnus-summary-search-article-backward
  "\M-S" gnus-summary-repeat-search-article-forward
  "\M-R" gnus-summary-repeat-search-article-backward
  "<" gnus-summary-beginning-of-article
  ">" gnus-summary-end-of-article
  "j" gnus-summary-goto-article
  "^" gnus-summary-refer-parent-article
  "\M-^" gnus-summary-refer-article
  "u" gnus-summary-tick-article-forward
  "!" gnus-summary-tick-article-forward
  "U" gnus-summary-tick-article-backward
  "d" gnus-summary-mark-as-read-forward
  "D" gnus-summary-mark-as-read-backward
  "E" gnus-summary-mark-as-expirable
  "\M-u" gnus-summary-clear-mark-forward
  "\M-U" gnus-summary-clear-mark-backward
  "k" gnus-summary-kill-same-subject-and-select
  "\C-k" gnus-summary-kill-same-subject
  "\M-\C-k" gnus-summary-kill-thread
  "\M-\C-l" gnus-summary-lower-thread
  "e" gnus-summary-edit-article
  "#" gnus-summary-mark-as-processable
  "\M-#" gnus-summary-unmark-as-processable
  "\M-\C-t" gnus-summary-toggle-threads
  "\M-\C-s" gnus-summary-show-thread
  "\M-\C-h" gnus-summary-hide-thread
  "\M-\C-f" gnus-summary-next-thread
  "\M-\C-b" gnus-summary-prev-thread
  [(meta down)] gnus-summary-next-thread
  [(meta up)] gnus-summary-prev-thread
  "\M-\C-u" gnus-summary-up-thread
  "\M-\C-d" gnus-summary-down-thread
  "&" gnus-summary-execute-command
  "c" gnus-summary-catchup-and-exit
  "\C-w" gnus-summary-mark-region-as-read
  "\C-t" gnus-summary-toggle-truncation
  "?" gnus-summary-mark-as-dormant
  "\C-c\M-\C-s" gnus-summary-limit-include-expunged
  "\C-c\C-s\C-n" gnus-summary-sort-by-number
  "\C-c\C-s\C-m\C-n" gnus-summary-sort-by-most-recent-number
  "\C-c\C-s\C-l" gnus-summary-sort-by-lines
  "\C-c\C-s\C-c" gnus-summary-sort-by-chars
  "\C-c\C-s\C-a" gnus-summary-sort-by-author
  "\C-c\C-s\C-t" gnus-summary-sort-by-recipient
  "\C-c\C-s\C-s" gnus-summary-sort-by-subject
  "\C-c\C-s\C-d" gnus-summary-sort-by-date
  "\C-c\C-s\C-m\C-d" gnus-summary-sort-by-most-recent-date
  "\C-c\C-s\C-i" gnus-summary-sort-by-score
  "\C-c\C-s\C-o" gnus-summary-sort-by-original
  "\C-c\C-s\C-r" gnus-summary-sort-by-random
  "=" gnus-summary-expand-window
  "\C-x\C-s" gnus-summary-reselect-current-group
  "\M-g" gnus-summary-rescan-group
  "\C-c\C-r" gnus-summary-caesar-message
  "f" gnus-summary-followup
  "F" gnus-summary-followup-with-original
  "C" gnus-summary-cancel-article
  "r" gnus-summary-reply
  "R" gnus-summary-reply-with-original
  "\C-c\C-f" gnus-summary-mail-forward
  "o" gnus-summary-save-article
  "\C-o" gnus-summary-save-article-mail
  "|" gnus-summary-pipe-output
  "\M-k" gnus-summary-edit-local-kill
  "\M-K" gnus-summary-edit-global-kill
  ;; "V" gnus-version
  "\C-c\C-d" gnus-summary-describe-group
  "q" gnus-summary-exit
  "Q" gnus-summary-exit-no-update
  "\C-c\C-i" gnus-info-find-node
  gnus-mouse-2 gnus-mouse-pick-article
  [follow-link] mouse-face
  "m" gnus-summary-mail-other-window
  "a" gnus-summary-post-news
  "x" gnus-summary-limit-to-unread
  "s" gnus-summary-isearch-article
  [tab] gnus-summary-widget-forward
  "t" gnus-summary-toggle-header
  "g" gnus-summary-show-article
  "l" gnus-summary-goto-last-article
  "\C-c\C-v\C-v" gnus-uu-decode-uu-view
  "\C-d" gnus-summary-enter-digest-group
  "\M-\C-d" gnus-summary-read-document
  "\M-\C-e" gnus-summary-edit-parameters
  "\M-\C-a" gnus-summary-customize-parameters
  "\C-c\C-b" gnus-bug
  "*" gnus-cache-enter-article
  "\M-*" gnus-cache-remove-article
  "\M-&" gnus-summary-universal-argument
  "\C-l" gnus-recenter
  "I" gnus-summary-increase-score
  "L" gnus-summary-lower-score
  "\M-i" gnus-symbolic-argument
  "h" gnus-summary-select-article-buffer

  "b" gnus-article-view-part
  "\M-t" gnus-summary-toggle-display-buttonized

  "V" gnus-summary-score-map
  "X" gnus-uu-extract-map
  "S" gnus-summary-send-map)

;; Sort of orthogonal keymap
(gnus-define-keys (gnus-summary-mark-map "M" gnus-summary-mode-map)
  "t" gnus-summary-tick-article-forward
  "!" gnus-summary-tick-article-forward
  "d" gnus-summary-mark-as-read-forward
  "r" gnus-summary-mark-as-read-forward
  "c" gnus-summary-clear-mark-forward
  " " gnus-summary-clear-mark-forward
  "e" gnus-summary-mark-as-expirable
  "x" gnus-summary-mark-as-expirable
  "?" gnus-summary-mark-as-dormant
  "b" gnus-summary-set-bookmark
  "B" gnus-summary-remove-bookmark
  "#" gnus-summary-mark-as-processable
  "\M-#" gnus-summary-unmark-as-processable
  "S" gnus-summary-limit-include-expunged
  "C" gnus-summary-catchup
  "H" gnus-summary-catchup-to-here
  "h" gnus-summary-catchup-from-here
  "\C-c" gnus-summary-catchup-all
  "k" gnus-summary-kill-same-subject-and-select
  "K" gnus-summary-kill-same-subject
  "P" gnus-uu-mark-map)

(gnus-define-keys (gnus-summary-mscore-map "V" gnus-summary-mark-map)
  "c" gnus-summary-clear-above
  "u" gnus-summary-tick-above
  "m" gnus-summary-mark-above
  "k" gnus-summary-kill-below)

(gnus-define-keys (gnus-summary-limit-map "/" gnus-summary-mode-map)
  "/" gnus-summary-limit-to-subject
  "n" gnus-summary-limit-to-articles
  "b" gnus-summary-limit-to-bodies
  "h" gnus-summary-limit-to-headers
  "w" gnus-summary-pop-limit
  "s" gnus-summary-limit-to-subject
  "a" gnus-summary-limit-to-author
  "u" gnus-summary-limit-to-unread
  "m" gnus-summary-limit-to-marks
  "M" gnus-summary-limit-exclude-marks
  "v" gnus-summary-limit-to-score
  "*" gnus-summary-limit-include-cached
  "D" gnus-summary-limit-include-dormant
  "T" gnus-summary-limit-include-thread
  "d" gnus-summary-limit-exclude-dormant
  "t" gnus-summary-limit-to-age
  "." gnus-summary-limit-to-unseen
  "x" gnus-summary-limit-to-extra
  "p" gnus-summary-limit-to-display-predicate
  "E" gnus-summary-limit-include-expunged
  "c" gnus-summary-limit-exclude-childless-dormant
  "C" gnus-summary-limit-mark-excluded-as-read
  "o" gnus-summary-insert-old-articles
  "N" gnus-summary-insert-new-articles
  "S" gnus-summary-limit-to-singletons
  "r" gnus-summary-limit-to-replied
  "R" gnus-summary-limit-to-recipient
  "A" gnus-summary-limit-to-address)

(gnus-define-keys (gnus-summary-goto-map "G" gnus-summary-mode-map)
  "n" gnus-summary-next-unread-article
  "p" gnus-summary-prev-unread-article
  "N" gnus-summary-next-article
  "P" gnus-summary-prev-article
  "\C-n" gnus-summary-next-same-subject
  "\C-p" gnus-summary-prev-same-subject
  "\M-n" gnus-summary-next-unread-subject
  "\M-p" gnus-summary-prev-unread-subject
  "f" gnus-summary-first-unread-article
  "b" gnus-summary-best-unread-article
  "j" gnus-summary-goto-article
  "g" gnus-summary-goto-subject
  "l" gnus-summary-goto-last-article
  "o" gnus-summary-pop-article)

(gnus-define-keys (gnus-summary-thread-map "T" gnus-summary-mode-map)
  "k" gnus-summary-kill-thread
  "E" gnus-summary-expire-thread
  "l" gnus-summary-lower-thread
  "i" gnus-summary-raise-thread
  "T" gnus-summary-toggle-threads
  "t" gnus-summary-rethread-current
  "^" gnus-summary-reparent-thread
  "\M-^" gnus-summary-reparent-children
  "s" gnus-summary-show-thread
  "S" gnus-summary-show-all-threads
  "h" gnus-summary-hide-thread
  "H" gnus-summary-hide-all-threads
  "n" gnus-summary-next-thread
  "p" gnus-summary-prev-thread
  "u" gnus-summary-up-thread
  "o" gnus-summary-top-thread
  "d" gnus-summary-down-thread
  "#" gnus-uu-mark-thread
  "\M-#" gnus-uu-unmark-thread)

(gnus-define-keys (gnus-summary-buffer-map "Y" gnus-summary-mode-map)
  "g" gnus-summary-prepare
  "c" gnus-summary-insert-cached-articles
  "d" gnus-summary-insert-dormant-articles
  "t" gnus-summary-insert-ticked-articles)

(gnus-define-keys (gnus-summary-exit-map "Z" gnus-summary-mode-map)
  "c" gnus-summary-catchup-and-exit
  "C" gnus-summary-catchup-all-and-exit
  "E" gnus-summary-exit-no-update
  "Q" gnus-summary-exit
  "Z" gnus-summary-exit
  "n" gnus-summary-catchup-and-goto-next-group
  "p" gnus-summary-catchup-and-goto-prev-group
  "R" gnus-summary-reselect-current-group
  "G" gnus-summary-rescan-group
  "N" gnus-summary-next-group
  "s" gnus-summary-save-newsrc
  "P" gnus-summary-prev-group)

(gnus-define-keys (gnus-summary-article-map "A" gnus-summary-mode-map)
  " " gnus-summary-next-page
  "n" gnus-summary-next-page
  "\177" gnus-summary-prev-page
  [delete] gnus-summary-prev-page
  "p" gnus-summary-prev-page
  "\r" gnus-summary-scroll-up
  "\M-\r" gnus-summary-scroll-down
  "<" gnus-summary-beginning-of-article
  ">" gnus-summary-end-of-article
  "b" gnus-summary-beginning-of-article
  "e" gnus-summary-end-of-article
  "^" gnus-summary-refer-parent-article
  "r" gnus-summary-refer-parent-article
  "C" gnus-summary-show-complete-article
  "D" gnus-summary-enter-digest-group
  "R" gnus-summary-refer-references
  "T" gnus-summary-refer-thread
  "W" gnus-warp-to-article
  "g" gnus-summary-show-article
  "s" gnus-summary-isearch-article
  [tab] gnus-summary-widget-forward
  "P" gnus-summary-print-article
  "S" gnus-sticky-article
  "M" gnus-mailing-list-insinuate
  "t" gnus-article-babel)

(gnus-define-keys (gnus-summary-wash-map "W" gnus-summary-mode-map)
  "b" gnus-article-add-buttons
  "B" gnus-article-add-buttons-to-head
  "o" gnus-article-treat-overstrike
  "e" gnus-article-emphasize
  "w" gnus-article-fill-cited-article
  "Q" gnus-article-fill-long-lines
  "L" gnus-article-toggle-truncate-lines
  "C" gnus-article-capitalize-sentences
  "c" gnus-article-remove-cr
  "q" gnus-article-de-quoted-unreadable
  "6" gnus-article-de-base64-unreadable
  "Z" gnus-article-decode-HZ
  "A" gnus-article-treat-ansi-sequences
  "h" gnus-article-wash-html
  "u" gnus-article-unsplit-urls
  "s" gnus-summary-force-verify-and-decrypt
  "f" gnus-article-display-x-face
  "l" gnus-summary-stop-page-breaking
  "r" gnus-summary-caesar-message
  "m" gnus-summary-morse-message
  "t" gnus-summary-toggle-header
  "g" gnus-treat-smiley
  "v" gnus-summary-verbose-headers
  "a" gnus-article-strip-headers-in-body ;; mnemonic: wash archive
  "p" gnus-article-verify-x-pgp-sig
  "d" gnus-article-treat-dumbquotes
  "U" gnus-article-treat-non-ascii
  "i" gnus-summary-idna-message)

(gnus-define-keys (gnus-summary-wash-deuglify-map "Y" gnus-summary-wash-map)
  ;; mnemonic: deuglif*Y*
  "u" gnus-article-outlook-unwrap-lines
  "a" gnus-article-outlook-repair-attribution
  "c" gnus-article-outlook-rearrange-citation
  "f" gnus-article-outlook-deuglify-article) ;; mnemonic: full deuglify

(gnus-define-keys (gnus-summary-wash-hide-map "W" gnus-summary-wash-map)
  "a" gnus-article-hide
  "h" gnus-article-hide-headers
  "b" gnus-article-hide-boring-headers
  "s" gnus-article-hide-signature
  "c" gnus-article-hide-citation
  "C" gnus-article-hide-citation-in-followups
  "l" gnus-article-hide-list-identifiers
  "B" gnus-article-strip-banner
  "P" gnus-article-hide-pem
  "\C-c" gnus-article-hide-citation-maybe)

(gnus-define-keys (gnus-summary-wash-highlight-map "H" gnus-summary-wash-map)
  "a" gnus-article-highlight
  "h" gnus-article-highlight-headers
  "c" gnus-article-highlight-citation
  "s" gnus-article-highlight-signature)

(gnus-define-keys (gnus-summary-wash-header-map "G" gnus-summary-wash-map)
  "f" gnus-article-treat-fold-headers
  "u" gnus-article-treat-unfold-headers
  "n" gnus-article-treat-fold-newsgroups)

(gnus-define-keys (gnus-summary-wash-display-map "D" gnus-summary-wash-map)
  "x" gnus-article-display-x-face
  "d" gnus-article-display-face
  "s" gnus-treat-smiley
  "D" gnus-article-remove-images
  "W" gnus-article-show-images
  "f" gnus-treat-from-picon
  "m" gnus-treat-mail-picon
  "n" gnus-treat-newsgroups-picon
  "g" gnus-treat-from-gravatar
  "h" gnus-treat-mail-gravatar)

(gnus-define-keys (gnus-summary-wash-mime-map "M" gnus-summary-wash-map)
  "w" gnus-article-decode-mime-words
  "c" gnus-article-decode-charset
  "v" gnus-mime-view-all-parts
  "b" gnus-article-view-part)

(gnus-define-keys (gnus-summary-wash-time-map "T" gnus-summary-wash-map)
  "z" gnus-article-date-ut
  "u" gnus-article-date-ut
  "l" gnus-article-date-local
  "p" gnus-article-date-english
  "e" gnus-article-date-lapsed
  "o" gnus-article-date-original
  "i" gnus-article-date-iso8601
  "s" gnus-article-date-user)

(gnus-define-keys (gnus-summary-wash-empty-map "E" gnus-summary-wash-map)
  "t" gnus-article-remove-trailing-blank-lines
  "l" gnus-article-strip-leading-blank-lines
  "m" gnus-article-strip-multiple-blank-lines
  "a" gnus-article-strip-blank-lines
  "A" gnus-article-strip-all-blank-lines
  "s" gnus-article-strip-leading-space
  "e" gnus-article-strip-trailing-space
  "w" gnus-article-remove-leading-whitespace)

(gnus-define-keys (gnus-summary-help-map "H" gnus-summary-mode-map)
  "v" gnus-version
  "d" gnus-summary-describe-group
  "h" gnus-summary-describe-briefly
  "i" gnus-info-find-node)

(gnus-define-keys (gnus-summary-backend-map "B" gnus-summary-mode-map)
  "e" gnus-summary-expire-articles
  "\M-\C-e" gnus-summary-expire-articles-now
  "\177" gnus-summary-delete-article
  [delete] gnus-summary-delete-article
  [backspace] gnus-summary-delete-article
  "m" gnus-summary-move-article
  "r" gnus-summary-respool-article
  "w" gnus-summary-edit-article
  "c" gnus-summary-copy-article
  "B" gnus-summary-crosspost-article
  "q" gnus-summary-respool-query
  "t" gnus-summary-respool-trace
  "i" gnus-summary-import-article
  "I" gnus-summary-create-article
  "p" gnus-summary-article-posted-p)

(gnus-define-keys (gnus-summary-save-map "O" gnus-summary-mode-map)
  "o" gnus-summary-save-article
  "m" gnus-summary-save-article-mail
  "F" gnus-summary-write-article-file
  "r" gnus-summary-save-article-rmail
  "f" gnus-summary-save-article-file
  "b" gnus-summary-save-article-body-file
  "B" gnus-summary-write-article-body-file
  "h" gnus-summary-save-article-folder
  "v" gnus-summary-save-article-vm
  "p" gnus-summary-pipe-output
  "P" gnus-summary-muttprint)

(gnus-define-keys (gnus-summary-mime-map "K" gnus-summary-mode-map)
  "b" gnus-summary-display-buttonized
  "m" gnus-summary-repair-multipart
  "v" gnus-article-view-part
  "o" gnus-article-save-part
  "O" gnus-article-save-part-and-strip
  "r" gnus-article-replace-part
  "d" gnus-article-delete-part
  "t" gnus-article-view-part-as-type
  "j" gnus-article-jump-to-part
  "c" gnus-article-copy-part
  "C" gnus-article-view-part-as-charset
  "e" gnus-article-view-part-externally
  "H" gnus-article-browse-html-article
  "E" gnus-article-encrypt-body
  "i" gnus-article-inline-part
  "|" gnus-article-pipe-part)

(gnus-define-keys (gnus-uu-mark-map "P" gnus-summary-mark-map)
  "p" gnus-summary-mark-as-processable
  "u" gnus-summary-unmark-as-processable
  "U" gnus-summary-unmark-all-processable
  "v" gnus-uu-mark-over
  "s" gnus-uu-mark-series
  "r" gnus-uu-mark-region
  "g" gnus-uu-unmark-region
  "R" gnus-uu-mark-by-regexp
  "G" gnus-uu-unmark-by-regexp
  "t" gnus-uu-mark-thread
  "T" gnus-uu-unmark-thread
  "a" gnus-uu-mark-all
  "b" gnus-uu-mark-buffer
  "S" gnus-uu-mark-sparse
  "k" gnus-summary-kill-process-mark
  "y" gnus-summary-yank-process-mark
  "w" gnus-summary-save-process-mark
  "i" gnus-uu-invert-processable)

(gnus-define-keys (gnus-uu-extract-map "X" gnus-summary-mode-map)
  ;;"x" gnus-uu-extract-any
  "m" gnus-summary-save-parts
  "u" gnus-uu-decode-uu
  "U" gnus-uu-decode-uu-and-save
  "s" gnus-uu-decode-unshar
  "S" gnus-uu-decode-unshar-and-save
  "o" gnus-uu-decode-save
  "O" gnus-uu-decode-save
  "b" gnus-uu-decode-binhex
  "B" gnus-uu-decode-binhex
  "Y" gnus-uu-decode-yenc
  "p" gnus-uu-decode-postscript
  "P" gnus-uu-decode-postscript-and-save)

(gnus-define-keys
    (gnus-uu-extract-view-map "v" gnus-uu-extract-map)
  "u" gnus-uu-decode-uu-view
  "U" gnus-uu-decode-uu-and-save-view
  "s" gnus-uu-decode-unshar-view
  "S" gnus-uu-decode-unshar-and-save-view
  "o" gnus-uu-decode-save-view
  "O" gnus-uu-decode-save-view
  "b" gnus-uu-decode-binhex-view
  "B" gnus-uu-decode-binhex-view
  "p" gnus-uu-decode-postscript-view
  "P" gnus-uu-decode-postscript-and-save-view)

(defvar gnus-article-post-menu nil)

(defconst gnus-summary-menu-maxlen 20)

(defun gnus-summary-menu-split (menu)
  ;; If we have lots of elements, divide them into groups of 20
  ;; and make a pane (or submenu) for each one.
  (if (> (length menu) (/ (* gnus-summary-menu-maxlen 3) 2))
      (let ((menu menu) sublists next
	    (i 1))
	(while menu
	  ;; Pull off the next gnus-summary-menu-maxlen elements
	  ;; and make them the next element of sublist.
	  (setq next (nthcdr gnus-summary-menu-maxlen menu))
	  (if next
	      (setcdr (nthcdr (1- gnus-summary-menu-maxlen) menu)
		      nil))
	  (setq sublists (cons (cons (format "%s ... %s" (aref (car menu) 0)
					     (aref (car (last menu)) 0)) menu)
			       sublists))
	  (setq i (1+ i))
	  (setq menu next))
	(nreverse sublists))
    ;; Few elements--put them all in one pane.
    menu))

(defun gnus-summary-make-menu-bar ()
  (gnus-turn-off-edit-menu 'summary)

  (unless (boundp 'gnus-summary-misc-menu)

    (easy-menu-define
      gnus-summary-kill-menu gnus-summary-mode-map ""
      (cons
       "Score"
       (nconc
	(list
	 ["Customize" gnus-score-customize t])
	(gnus-make-score-map 'increase)
	(gnus-make-score-map 'lower)
	'(("Mark"
	   ["Kill below" gnus-summary-kill-below t]
	   ["Mark above" gnus-summary-mark-above t]
	   ["Tick above" gnus-summary-tick-above t]
	   ["Clear above" gnus-summary-clear-above t])
	  ["Current score" gnus-summary-current-score t]
	  ["Set score" gnus-summary-set-score t]
	  ["Switch current score file..." gnus-score-change-score-file t]
	  ["Set mark below..." gnus-score-set-mark-below t]
	  ["Set expunge below..." gnus-score-set-expunge-below t]
	  ["Edit current score file" gnus-score-edit-current-scores t]
	  ["Edit score file..." gnus-score-edit-file t]
	  ["Trace score" gnus-score-find-trace t]
	  ["Find words" gnus-score-find-favourite-words t]
	  ["Rescore buffer" gnus-summary-rescore t]
	  ["Increase score..." gnus-summary-increase-score t]
	  ["Lower score..." gnus-summary-lower-score t]))))

    ;; Define both the Article menu in the summary buffer and the
    ;; equivalent Commands menu in the article buffer here for
    ;; consistency.
    (let ((innards
	   `(("Hide"
	      ["All" gnus-article-hide t]
	      ["Headers" gnus-article-hide-headers t]
	      ["Signature" gnus-article-hide-signature t]
	      ["Citation" gnus-article-hide-citation t]
	      ["List identifiers" gnus-article-hide-list-identifiers t]
	      ["Banner" gnus-article-strip-banner t]
	      ["Boring headers" gnus-article-hide-boring-headers t])
	     ("Highlight"
	      ["All" gnus-article-highlight t]
	      ["Headers" gnus-article-highlight-headers t]
	      ["Signature" gnus-article-highlight-signature t]
	      ["Citation" gnus-article-highlight-citation t])
	     ("MIME"
	      ["Words" gnus-article-decode-mime-words t]
	      ["Charset" gnus-article-decode-charset t]
	      ["QP" gnus-article-de-quoted-unreadable t]
	      ["Base64" gnus-article-de-base64-unreadable t]
	      ["View MIME buttons" gnus-summary-display-buttonized t]
	      ["View all" gnus-mime-view-all-parts t]
	      ["Verify and Decrypt" gnus-summary-force-verify-and-decrypt t]
	      ["Encrypt body" gnus-article-encrypt-body
	       :active (not (gnus-group-read-only-p))
	       ,@(if (featurep 'xemacs) nil
		   '(:help "Encrypt the message body on disk"))]
	      ["Extract all parts..." gnus-summary-save-parts t]
	      ("Multipart"
	       ["Repair multipart" gnus-summary-repair-multipart t]
	       ["Pipe part..." gnus-article-pipe-part t]
	       ["Inline part" gnus-article-inline-part t]
	       ["View part as type..." gnus-article-view-part-as-type t]
	       ["Encrypt body" gnus-article-encrypt-body
		:active (not (gnus-group-read-only-p))
	       ,@(if (featurep 'xemacs) nil
		   '(:help "Encrypt the message body on disk"))]
	       ["View part externally" gnus-article-view-part-externally t]
	       ["View HTML parts in browser" gnus-article-browse-html-article t]
	       ["View part with charset..." gnus-article-view-part-as-charset t]
	       ["Copy part" gnus-article-copy-part t]
	       ["Save part..." gnus-article-save-part t]
	       ["View part" gnus-article-view-part t]))
	     ("Date"
	      ["Local" gnus-article-date-local t]
	      ["ISO8601" gnus-article-date-iso8601 t]
	      ["UT" gnus-article-date-ut t]
	      ["Original" gnus-article-date-original t]
	      ["Lapsed" gnus-article-date-lapsed t]
	      ["User-defined" gnus-article-date-user t])
	     ("Display"
	      ["Remove images" gnus-article-remove-images t]
	      ["Toggle smiley" gnus-treat-smiley t]
	      ["Show X-Face" gnus-article-display-x-face t]
	      ["Show picons in From" gnus-treat-from-picon t]
	      ["Show picons in mail headers" gnus-treat-mail-picon t]
	      ["Show picons in news headers" gnus-treat-newsgroups-picon t]
              ["Show Gravatars in From" gnus-treat-from-gravatar t]
	      ["Show Gravatars in mail headers" gnus-treat-mail-gravatar t]
	      ("View as different encoding"
	       ,@(gnus-summary-menu-split
		  (mapcar
		   (lambda (cs)
		     ;; Since easymenu under Emacs doesn't allow
		     ;; lambda forms for menu commands, we should
		     ;; provide intern'ed function symbols.
		     (let ((command (intern (format "\
gnus-summary-show-article-from-menu-as-charset-%s" cs))))
		       (fset command
			     `(lambda ()
				(interactive)
				(let ((gnus-summary-show-article-charset-alist
				       '((1 . ,cs))))
				  (gnus-summary-show-article 1))))
		       `[,(symbol-name cs) ,command t]))
		   (sort (if (fboundp 'coding-system-list)
			     (coding-system-list)
			   (mapcar 'car mm-mime-mule-charset-alist))
			 'string<)))))
	     ("Washing"
	      ("Remove Blanks"
	       ["Leading" gnus-article-strip-leading-blank-lines t]
	       ["Multiple" gnus-article-strip-multiple-blank-lines t]
	       ["Trailing" gnus-article-remove-trailing-blank-lines t]
	       ["All of the above" gnus-article-strip-blank-lines t]
	       ["All" gnus-article-strip-all-blank-lines t]
	       ["Leading space" gnus-article-strip-leading-space t]
	       ["Trailing space" gnus-article-strip-trailing-space t]
	       ["Leading space in headers"
		gnus-article-remove-leading-whitespace t])
	      ["Overstrike" gnus-article-treat-overstrike t]
	      ["Dumb quotes" gnus-article-treat-dumbquotes t]
	      ["Non-ASCII" gnus-article-treat-non-ascii t]
	      ["Emphasis" gnus-article-emphasize t]
	      ["Word wrap" gnus-article-fill-cited-article t]
	      ["Fill long lines" gnus-article-fill-long-lines t]
	      ["Toggle truncate long lines" gnus-article-toggle-truncate-lines t]
	      ["Capitalize sentences" gnus-article-capitalize-sentences t]
	      ["Remove CR" gnus-article-remove-cr t]
	      ["Quoted-Printable" gnus-article-de-quoted-unreadable t]
	      ["Base64" gnus-article-de-base64-unreadable t]
	      ["Rot 13" gnus-summary-caesar-message
	       ,@(if (featurep 'xemacs) '(t)
		   '(:help "\"Caesar rotate\" article by 13"))]
	      ["De-IDNA" gnus-summary-idna-message t]
	      ["Morse decode" gnus-summary-morse-message t]
	      ["Unix pipe..." gnus-summary-pipe-message t]
	      ["Add buttons" gnus-article-add-buttons t]
	      ["Add buttons to head" gnus-article-add-buttons-to-head t]
	      ["Stop page breaking" gnus-summary-stop-page-breaking t]
	      ["Verbose header" gnus-summary-verbose-headers t]
	      ["Toggle header" gnus-summary-toggle-header t]
	      ["Unfold headers" gnus-article-treat-unfold-headers t]
	      ["Fold newsgroups" gnus-article-treat-fold-newsgroups t]
	      ["Html" gnus-article-wash-html t]
	      ["Unsplit URLs" gnus-article-unsplit-urls t]
	      ["Verify X-PGP-Sig" gnus-article-verify-x-pgp-sig t]
	      ["Decode HZ" gnus-article-decode-HZ t]
	      ["ANSI sequences" gnus-article-treat-ansi-sequences t]
	      ("(Outlook) Deuglify"
	       ["Unwrap lines" gnus-article-outlook-unwrap-lines t]
	       ["Repair attribution" gnus-article-outlook-repair-attribution t]
	       ["Rearrange citation" gnus-article-outlook-rearrange-citation t]
	       ["Full (Outlook) deuglify"
		gnus-article-outlook-deuglify-article t])
	      )
	     ("Output"
	      ["Save in default format..." gnus-summary-save-article
	       ,@(if (featurep 'xemacs) '(t)
		   '(:help "Save article using default method"))]
	      ["Save in file..." gnus-summary-save-article-file
	       ,@(if (featurep 'xemacs) '(t)
		   '(:help "Save article in file"))]
	      ["Save in Unix mail format..." gnus-summary-save-article-mail t]
	      ["Save in MH folder..." gnus-summary-save-article-folder t]
	      ["Save in VM folder..." gnus-summary-save-article-vm t]
	      ["Save in RMAIL mbox..." gnus-summary-save-article-rmail t]
	      ["Save body in file..." gnus-summary-save-article-body-file t]
	      ["Pipe through a filter..." gnus-summary-pipe-output t]
	      ["Print with Muttprint..." gnus-summary-muttprint t]
	      ["Print" gnus-summary-print-article
	       ,@(if (featurep 'xemacs) '(t)
		   '(:help "Generate and print a PostScript image"))])
	     ("Copy, move,... (Backend)"
	      ,@(if (featurep 'xemacs) nil
		  '(:help "Copying, moving, expiring articles..."))
	      ["Respool article..." gnus-summary-respool-article t]
	      ["Move article..." gnus-summary-move-article
	       (gnus-check-backend-function
		'request-move-article gnus-newsgroup-name)]
	      ["Copy article..." gnus-summary-copy-article t]
	      ["Crosspost article..." gnus-summary-crosspost-article
	       (gnus-check-backend-function
		'request-replace-article gnus-newsgroup-name)]
	      ["Import file..." gnus-summary-import-article
	       (gnus-check-backend-function
		'request-accept-article gnus-newsgroup-name)]
	      ["Create article..." gnus-summary-create-article
	       (gnus-check-backend-function
		'request-accept-article gnus-newsgroup-name)]
	      ["Check if posted" gnus-summary-article-posted-p t]
	      ["Edit article" gnus-summary-edit-article
	       (not (gnus-group-read-only-p))]
	      ["Delete article" gnus-summary-delete-article
	       (gnus-check-backend-function
		'request-expire-articles gnus-newsgroup-name)]
	      ["Query respool" gnus-summary-respool-query t]
	      ["Trace respool" gnus-summary-respool-trace t]
	      ["Delete expirable articles" gnus-summary-expire-articles-now
	       (gnus-check-backend-function
		'request-expire-articles gnus-newsgroup-name)])
	     ("Extract"
	      ["Uudecode" gnus-uu-decode-uu
	       ,@(if (featurep 'xemacs) '(t)
		   '(:help "Decode uuencoded article(s)"))]
	      ["Uudecode and save" gnus-uu-decode-uu-and-save t]
	      ["Unshar" gnus-uu-decode-unshar t]
	      ["Unshar and save" gnus-uu-decode-unshar-and-save t]
	      ["Save" gnus-uu-decode-save t]
	      ["Binhex" gnus-uu-decode-binhex t]
	      ["PostScript" gnus-uu-decode-postscript t]
	      ["All MIME parts" gnus-summary-save-parts t])
	     ("Cache"
	      ["Enter article" gnus-cache-enter-article t]
	      ["Remove article" gnus-cache-remove-article t])
	     ["Translate" gnus-article-babel t]
	     ["Select article buffer" gnus-summary-select-article-buffer t]
	     ["Make article buffer sticky" gnus-sticky-article t]
	     ["Enter digest buffer" gnus-summary-enter-digest-group t]
	     ["Isearch article..." gnus-summary-isearch-article t]
	     ["Beginning of the article" gnus-summary-beginning-of-article t]
	     ["End of the article" gnus-summary-end-of-article t]
	     ["Fetch parent of article" gnus-summary-refer-parent-article t]
	     ["Fetch referenced articles" gnus-summary-refer-references t]
	     ["Fetch current thread" gnus-summary-refer-thread t]
	     ["Fetch article with id..." gnus-summary-refer-article t]
	     ["Setup Mailing List Params" gnus-mailing-list-insinuate t]
	     ["Redisplay" gnus-summary-show-article t]
	     ["Raw article" gnus-summary-show-raw-article :keys "C-u g"])))
      (easy-menu-define
	gnus-summary-article-menu gnus-summary-mode-map ""
	(cons "Article" innards))

      (if (not (keymapp gnus-summary-article-menu))
	  (easy-menu-define
	    gnus-article-commands-menu gnus-article-mode-map ""
	    (cons "Commands" innards))
	;; in Emacs, don't share menu.
	(setq gnus-article-commands-menu
	      (copy-keymap gnus-summary-article-menu))
	(define-key gnus-article-mode-map [menu-bar commands]
	  (cons "Commands" gnus-article-commands-menu))))

    (easy-menu-define
      gnus-summary-thread-menu gnus-summary-mode-map ""
      '("Threads"
	["Find all messages in thread" gnus-summary-refer-thread t]
	["Toggle threading" gnus-summary-toggle-threads t]
	["Hide threads" gnus-summary-hide-all-threads t]
	["Show threads" gnus-summary-show-all-threads t]
	["Hide thread" gnus-summary-hide-thread t]
	["Show thread" gnus-summary-show-thread t]
	["Go to next thread" gnus-summary-next-thread t]
	["Go to previous thread" gnus-summary-prev-thread t]
	["Go down thread" gnus-summary-down-thread t]
	["Go up thread" gnus-summary-up-thread t]
	["Top of thread" gnus-summary-top-thread t]
	["Mark thread as read" gnus-summary-kill-thread t]
	["Mark thread as expired" gnus-summary-expire-thread t]
	["Lower thread score" gnus-summary-lower-thread t]
	["Raise thread score" gnus-summary-raise-thread t]
	["Rethread current" gnus-summary-rethread-current t]))

    (easy-menu-define
      gnus-summary-post-menu gnus-summary-mode-map ""
      `("Post"
	["Send a message (mail or news)" gnus-summary-post-news
	 ,@(if (featurep 'xemacs) '(t)
	     '(:help "Compose a new message (mail or news)"))]
	["Followup" gnus-summary-followup
	 ,@(if (featurep 'xemacs) '(t)
	     '(:help "Post followup to this article"))]
	["Followup and yank" gnus-summary-followup-with-original
	 ,@(if (featurep 'xemacs) '(t)
	     '(:help "Post followup to this article, quoting its contents"))]
	["Supersede article" gnus-summary-supersede-article t]
	["Cancel article" gnus-summary-cancel-article
	 ,@(if (featurep 'xemacs) '(t)
	     '(:help "Cancel an article you posted"))]
	["Reply" gnus-summary-reply t]
	["Reply and yank" gnus-summary-reply-with-original t]
	["Wide reply" gnus-summary-wide-reply t]
	["Wide reply and yank" gnus-summary-wide-reply-with-original
	 ,@(if (featurep 'xemacs) '(t)
	     '(:help "Mail a reply, quoting this article"))]
	["Very wide reply" gnus-summary-very-wide-reply t]
	["Very wide reply and yank" gnus-summary-very-wide-reply-with-original
	 ,@(if (featurep 'xemacs) '(t)
	     '(:help "Mail a very wide reply, quoting this article"))]
	["Mail forward" gnus-summary-mail-forward t]
	["Post forward" gnus-summary-post-forward t]
	["Digest and mail" gnus-uu-digest-mail-forward t]
	["Digest and post" gnus-uu-digest-post-forward t]
	["Resend message" gnus-summary-resend-message t]
	["Resend message edit" gnus-summary-resend-message-edit t]
	["Send bounced mail" gnus-summary-resend-bounced-mail t]
	["Send a mail" gnus-summary-mail-other-window t]
	["Create a local message" gnus-summary-news-other-window t]
	["Uuencode and post" gnus-uu-post-news
	 ,@(if (featurep 'xemacs) '(t)
	     '(:help "Post a uuencoded article"))]
	["Followup via news" gnus-summary-followup-to-mail t]
	["Followup via news and yank"
	 gnus-summary-followup-to-mail-with-original t]
	["Strip signature on reply"
	 (lambda ()
	   (interactive)
	   (if (not (memq message-cite-function
			  '(message-cite-original-without-signature
			    message-cite-original)))
	       ;; Stupid workaround for XEmacs not honoring :visible.
	       (message "Can't toggle this value of `message-cite-function'")
	     (setq message-cite-function
		   (if (eq message-cite-function
			   'message-cite-original-without-signature)
		       'message-cite-original
		     'message-cite-original-without-signature))))
	 ;; XEmacs barfs on :visible.
	 ,@(if (featurep 'xemacs) nil
	     '(:visible (memq message-cite-function
			      '(message-cite-original-without-signature
				message-cite-original))))
	 :style toggle
	 :selected (eq message-cite-function
		       'message-cite-original-without-signature)
	 ,@(if (featurep 'xemacs) nil
	     '(:help "Strip signature from cited article when replying."))]
	;;("Draft"
	;;["Send" gnus-summary-send-draft t]
	;;["Send bounced" gnus-resend-bounced-mail t])
	))

    (cond
     ((not (keymapp gnus-summary-post-menu))
      (setq gnus-article-post-menu gnus-summary-post-menu))
     ((not gnus-article-post-menu)
      ;; Don't share post menu.
      (setq gnus-article-post-menu
	    (copy-keymap gnus-summary-post-menu))))
    (define-key gnus-article-mode-map [menu-bar post]
      (cons "Post" gnus-article-post-menu))

    (easy-menu-define
      gnus-summary-misc-menu gnus-summary-mode-map ""
      `("Gnus"
	("Mark Read"
	 ["Mark as read" gnus-summary-mark-as-read-forward t]
	 ["Mark same subject and select"
	  gnus-summary-kill-same-subject-and-select t]
	 ["Mark same subject" gnus-summary-kill-same-subject t]
	 ["Catchup" gnus-summary-catchup
	  ,@(if (featurep 'xemacs) '(t)
	      '(:help "Mark unread articles in this group as read"))]
	 ["Catchup all" gnus-summary-catchup-all t]
	 ["Catchup to here" gnus-summary-catchup-to-here t]
	 ["Catchup from here" gnus-summary-catchup-from-here t]
	 ["Catchup region" gnus-summary-mark-region-as-read
	  (gnus-mark-active-p)]
	 ["Mark excluded" gnus-summary-limit-mark-excluded-as-read t])
	("Mark Various"
	 ["Tick" gnus-summary-tick-article-forward t]
	 ["Mark as dormant" gnus-summary-mark-as-dormant t]
	 ["Remove marks" gnus-summary-clear-mark-forward t]
	 ["Set expirable mark" gnus-summary-mark-as-expirable t]
	 ["Set bookmark" gnus-summary-set-bookmark t]
	 ["Remove bookmark" gnus-summary-remove-bookmark t])
	("Limit to"
	 ["Marks..." gnus-summary-limit-to-marks t]
	 ["Subject..." gnus-summary-limit-to-subject t]
	 ["Author..." gnus-summary-limit-to-author t]
	 ["Recipient..." gnus-summary-limit-to-recipient t]
	 ["Address..." gnus-summary-limit-to-address t]
	 ["Age..." gnus-summary-limit-to-age t]
	 ["Extra..." gnus-summary-limit-to-extra t]
	 ["Score..." gnus-summary-limit-to-score t]
	 ["Display Predicate" gnus-summary-limit-to-display-predicate t]
	 ["Unread" gnus-summary-limit-to-unread t]
	 ["Unseen" gnus-summary-limit-to-unseen t]
	 ["Singletons" gnus-summary-limit-to-singletons t]
	 ["Replied" gnus-summary-limit-to-replied t]
	 ["Non-dormant" gnus-summary-limit-exclude-dormant t]
	 ["Next or process marked articles" gnus-summary-limit-to-articles t]
	 ["Pop limit" gnus-summary-pop-limit t]
	 ["Show dormant" gnus-summary-limit-include-dormant t]
	 ["Hide childless dormant"
	  gnus-summary-limit-exclude-childless-dormant t]
	 ;;["Hide thread" gnus-summary-limit-exclude-thread t]
	 ["Hide marked" gnus-summary-limit-exclude-marks t]
	 ["Show expunged" gnus-summary-limit-include-expunged t])
	("Process Mark"
	 ["Set mark" gnus-summary-mark-as-processable t]
	 ["Remove mark" gnus-summary-unmark-as-processable t]
	 ["Remove all marks" gnus-summary-unmark-all-processable t]
	 ["Invert marks" gnus-uu-invert-processable t]
	 ["Mark above" gnus-uu-mark-over t]
	 ["Mark series" gnus-uu-mark-series t]
	 ["Mark region" gnus-uu-mark-region (gnus-mark-active-p)]
	 ["Unmark region" gnus-uu-unmark-region (gnus-mark-active-p)]
	 ["Mark by regexp..." gnus-uu-mark-by-regexp t]
	 ["Unmark by regexp..." gnus-uu-unmark-by-regexp t]
	 ["Mark all" gnus-uu-mark-all t]
	 ["Mark buffer" gnus-uu-mark-buffer t]
	 ["Mark sparse" gnus-uu-mark-sparse t]
	 ["Mark thread" gnus-uu-mark-thread t]
	 ["Unmark thread" gnus-uu-unmark-thread t]
	 ("Process Mark Sets"
	  ["Kill" gnus-summary-kill-process-mark t]
	  ["Yank" gnus-summary-yank-process-mark
	   gnus-newsgroup-process-stack]
	  ["Save" gnus-summary-save-process-mark t]
	  ["Run command on marked..." gnus-summary-universal-argument t]))
	("Registry Marks")
	("Scroll article"
	 ["Page forward" gnus-summary-next-page
	  ,@(if (featurep 'xemacs) '(t)
	      '(:help "Show next page of article"))]
	 ["Page backward" gnus-summary-prev-page
	  ,@(if (featurep 'xemacs) '(t)
	      '(:help "Show previous page of article"))]
	 ["Line forward" gnus-summary-scroll-up t])
	("Move"
	 ["Next unread article" gnus-summary-next-unread-article t]
	 ["Previous unread article" gnus-summary-prev-unread-article t]
	 ["Next article" gnus-summary-next-article t]
	 ["Previous article" gnus-summary-prev-article t]
	 ["Next unread subject" gnus-summary-next-unread-subject t]
	 ["Previous unread subject" gnus-summary-prev-unread-subject t]
	 ["Next article same subject" gnus-summary-next-same-subject t]
	 ["Previous article same subject" gnus-summary-prev-same-subject t]
	 ["First unread article" gnus-summary-first-unread-article t]
	 ["Best unread article" gnus-summary-best-unread-article t]
	 ["Go to subject number..." gnus-summary-goto-subject t]
	 ["Go to article number..." gnus-summary-goto-article t]
	 ["Go to the last article" gnus-summary-goto-last-article t]
	 ["Pop article off history" gnus-summary-pop-article t])
	("Sort"
	 ["Sort by number" gnus-summary-sort-by-number t]
	 ["Sort by most recent number" gnus-summary-sort-by-most-recent-number t]
	 ["Sort by author" gnus-summary-sort-by-author t]
	 ["Sort by recipient" gnus-summary-sort-by-recipient t]
	 ["Sort by subject" gnus-summary-sort-by-subject t]
	 ["Sort by date" gnus-summary-sort-by-date t]
	 ["Sort by most recent date" gnus-summary-sort-by-most-recent-date t]
	 ["Sort by score" gnus-summary-sort-by-score t]
	 ["Sort by lines" gnus-summary-sort-by-lines t]
	 ["Sort by characters" gnus-summary-sort-by-chars t]
	 ["Randomize" gnus-summary-sort-by-random t]
	 ["Original sort" gnus-summary-sort-by-original t])
	("Help"
	 ["Describe group" gnus-summary-describe-group t]
	 ["Read manual" gnus-info-find-node t])
	("Modes"
	 ["Pick and read" gnus-pick-mode t]
	 ["Binary" gnus-binary-mode t])
	("Regeneration"
	 ["Regenerate" gnus-summary-prepare t]
	 ["Insert cached articles" gnus-summary-insert-cached-articles t]
	 ["Insert dormant articles" gnus-summary-insert-dormant-articles t]
	 ["Insert ticked articles" gnus-summary-insert-ticked-articles t]
	 ["Toggle threading" gnus-summary-toggle-threads t])
	["See old articles" gnus-summary-insert-old-articles t]
	["See new articles" gnus-summary-insert-new-articles t]
	["Filter articles..." gnus-summary-execute-command t]
	["Run command on articles..." gnus-summary-universal-argument t]
	["Search articles forward..." gnus-summary-search-article-forward t]
	["Search articles backward..." gnus-summary-search-article-backward t]
	["Toggle line truncation" gnus-summary-toggle-truncation t]
	["Expand window" gnus-summary-expand-window t]
	["Expire expirable articles" gnus-summary-expire-articles
	 (gnus-check-backend-function
	  'request-expire-articles gnus-newsgroup-name)]
	["Edit local kill file" gnus-summary-edit-local-kill t]
	["Edit main kill file" gnus-summary-edit-global-kill t]
	["Edit group parameters" gnus-summary-edit-parameters t]
	["Customize group parameters" gnus-summary-customize-parameters t]
	["Send a bug report" gnus-bug t]
	("Exit"
	 ["Catchup and exit" gnus-summary-catchup-and-exit
	  ,@(if (featurep 'xemacs) '(t)
	      '(:help "Mark unread articles in this group as read, then exit"))]
	 ["Catchup all and exit" gnus-summary-catchup-all-and-exit t]
	 ["Catchup and goto next" gnus-summary-catchup-and-goto-next-group t]
	 ["Catchup and goto prev" gnus-summary-catchup-and-goto-prev-group t]
	 ["Exit group" gnus-summary-exit
	  ,@(if (featurep 'xemacs) '(t)
	      '(:help "Exit current group, return to group selection mode"))]
	 ["Exit group without updating" gnus-summary-exit-no-update t]
	 ["Exit and goto next group" gnus-summary-next-group t]
	 ["Exit and goto prev group" gnus-summary-prev-group t]
	 ["Reselect group" gnus-summary-reselect-current-group t]
	 ["Rescan group" gnus-summary-rescan-group t]
	 ["Update dribble" gnus-summary-save-newsrc t])))

    (gnus-run-hooks 'gnus-summary-menu-hook)))

(defvar gnus-summary-tool-bar-map nil)

;; Note: The :set function in the `gnus-summary-tool-bar*' variables will only
;; affect _new_ message buffers.  We might add a function that walks thru all
;; summary-mode buffers and force the update.
(defun gnus-summary-tool-bar-update (&optional symbol value)
  "Update summary mode toolbar.
Setter function for custom variables."
  (setq-default gnus-summary-tool-bar-map nil)
  (when symbol
    ;; When used as ":set" function:
    (set-default symbol value))
  (when (gnus-buffer-live-p gnus-summary-buffer)
    (with-current-buffer gnus-summary-buffer
      (gnus-summary-make-tool-bar))))

(defcustom gnus-summary-tool-bar (if (eq gmm-tool-bar-style 'gnome)
				     'gnus-summary-tool-bar-gnome
				   'gnus-summary-tool-bar-retro)
  "Specifies the Gnus summary tool bar.

It can be either a list or a symbol referring to a list.  See
`gmm-tool-bar-from-list' for the format of the list.  The
default key map is `gnus-summary-mode-map'.

Pre-defined symbols include `gnus-summary-tool-bar-gnome' and
`gnus-summary-tool-bar-retro'."
  :type '(choice (const :tag "GNOME style" gnus-summary-tool-bar-gnome)
		 (const :tag "Retro look"  gnus-summary-tool-bar-retro)
		 (repeat :tag "User defined list" gmm-tool-bar-item)
		 (symbol))
  :version "23.1" ;; No Gnus
  :initialize 'custom-initialize-default
  :set 'gnus-summary-tool-bar-update
  :group 'gnus-summary)

(defcustom gnus-summary-tool-bar-gnome
  '((gnus-summary-post-news "mail/compose" nil)
    (gnus-summary-insert-new-articles "mail/inbox" nil
				      :visible (or (not gnus-agent)
						   gnus-plugged))
    (gnus-summary-reply-with-original "mail/reply")
    (gnus-summary-reply "mail/reply" nil :visible nil)
    (gnus-summary-followup-with-original "mail/reply-all")
    (gnus-summary-followup "mail/reply-all" nil :visible nil)
    (gnus-summary-mail-forward "mail/forward")
    (gnus-summary-save-article "mail/save")
    (gnus-summary-search-article-forward "search" nil :visible nil)
    (gnus-summary-print-article "print")
    (gnus-summary-tick-article-forward "flag-followup" nil :visible nil)
    ;; Some new commands that may need more suitable icons:
    (gnus-summary-save-newsrc "save" nil :visible nil)
    ;; (gnus-summary-show-article "stock_message-display" nil :visible nil)
    (gnus-summary-prev-article "left-arrow")
    (gnus-summary-next-article "right-arrow")
    (gnus-summary-next-page "next-page")
    ;; (gnus-summary-enter-digest-group "right_arrow" nil :visible nil)
    ;;
    ;; Maybe some sort-by-... could be added:
    ;; (gnus-summary-sort-by-author "sort-a-z" nil :visible nil)
    ;; (gnus-summary-sort-by-date "sort-1-9" nil :visible nil)
    (gnus-summary-mark-as-expirable
     "delete" nil
     :visible (gnus-check-backend-function 'request-expire-articles
					   gnus-newsgroup-name))
    (gnus-summary-mark-as-spam
     "mail/spam" t
     :visible (and (fboundp 'spam-group-ham-contents-p)
		   (spam-group-ham-contents-p gnus-newsgroup-name))
     :help "Mark as spam")
    (gnus-summary-mark-as-read-forward
     "mail/not-spam" nil
     :visible (and (fboundp 'spam-group-spam-contents-p)
		   (spam-group-spam-contents-p gnus-newsgroup-name)))
    ;;
    (gnus-summary-exit "exit")
    (gmm-customize-mode "preferences" t :help "Edit mode preferences")
    (gnus-info-find-node "help"))
  "List of functions for the summary tool bar (GNOME style).

See `gmm-tool-bar-from-list' for the format of the list."
  :type '(repeat gmm-tool-bar-item)
  :version "23.1" ;; No Gnus
  :initialize 'custom-initialize-default
  :set 'gnus-summary-tool-bar-update
  :group 'gnus-summary)

(defcustom gnus-summary-tool-bar-retro
  '((gnus-summary-prev-unread-article "gnus/prev-ur")
    (gnus-summary-next-unread-article "gnus/next-ur")
    (gnus-summary-post-news "gnus/post")
    (gnus-summary-followup-with-original "gnus/fuwo")
    (gnus-summary-followup "gnus/followup")
    (gnus-summary-reply-with-original "gnus/reply-wo")
    (gnus-summary-reply "gnus/reply")
    (gnus-summary-caesar-message "gnus/rot13")
    (gnus-uu-decode-uu "gnus/uu-decode")
    (gnus-summary-save-article-file "gnus/save-aif")
    (gnus-summary-save-article "gnus/save-art")
    (gnus-uu-post-news "gnus/uu-post")
    (gnus-summary-catchup "gnus/catchup")
    (gnus-summary-catchup-and-exit "gnus/cu-exit")
    (gnus-summary-exit "gnus/exit-summ")
    ;; Some new command that may need more suitable icons:
    (gnus-summary-print-article "gnus/print" nil :visible nil)
    (gnus-summary-mark-as-expirable "gnus/close" nil :visible nil)
    (gnus-summary-save-newsrc "gnus/save" nil :visible nil)
    ;; (gnus-summary-enter-digest-group "gnus/right_arrow" nil :visible nil)
    (gnus-summary-search-article-forward "gnus/search" nil :visible nil)
    ;; (gnus-summary-insert-new-articles "gnus/paste" nil :visible nil)
    ;; (gnus-summary-toggle-threads "gnus/open" nil :visible nil)
    ;;
    (gnus-info-find-node "gnus/help" nil :visible nil))
  "List of functions for the summary tool bar (retro look).

See `gmm-tool-bar-from-list' for the format of the list."
  :type '(repeat gmm-tool-bar-item)
  :version "23.1" ;; No Gnus
  :initialize 'custom-initialize-default
  :set 'gnus-summary-tool-bar-update
  :group 'gnus-summary)

(defcustom gnus-summary-tool-bar-zap-list t
  "List of icon items from the global tool bar.
These items are not displayed in the Gnus summary mode tool bar.

See `gmm-tool-bar-from-list' for the format of the list."
  :type 'gmm-tool-bar-zap-list
  :version "23.1" ;; No Gnus
  :initialize 'custom-initialize-default
  :set 'gnus-summary-tool-bar-update
  :group 'gnus-summary)

(defvar image-load-path)
(defvar tool-bar-map)

(defun gnus-summary-make-tool-bar (&optional force)
  "Make a summary mode tool bar from `gnus-summary-tool-bar'.
When FORCE, rebuild the tool bar."
  (when (and (not (featurep 'xemacs))
	     (boundp 'tool-bar-mode)
	     tool-bar-mode
	     (or (not gnus-summary-tool-bar-map) force))
    (let* ((load-path
	    (gmm-image-load-path-for-library "gnus"
					     "mail/save.xpm"
					     nil t))
           (image-load-path (cons (car load-path)
                                  (when (boundp 'image-load-path)
                                    image-load-path)))
	   (map (gmm-tool-bar-from-list gnus-summary-tool-bar
					gnus-summary-tool-bar-zap-list
					'gnus-summary-mode-map)))
      (when map
	;; Need to set `gnus-summary-tool-bar-map' because `gnus-article-mode'
	;; uses its value.
	(setq gnus-summary-tool-bar-map map))))
  (set (make-local-variable 'tool-bar-map) gnus-summary-tool-bar-map))

(defun gnus-score-set-default (var value)
  "A version of set that updates the GNU Emacs menu-bar."
  (set var value)
  ;; It is the message that forces the active status to be updated.
  (message ""))

(defun gnus-make-score-map (type)
  "Make a summary score map of type TYPE."
  (if t
      nil
    (let ((headers '(("author" "from" string)
		     ("subject" "subject" string)
		     ("article body" "body" string)
		     ("article head" "head" string)
		     ("xref" "xref" string)
		     ("extra header" "extra" string)
		     ("lines" "lines" number)
		     ("followups to author" "followup" string)))
	  (types '((number ("less than" <)
			   ("greater than" >)
			   ("equal" =))
		   (string ("substring" s)
			   ("exact string" e)
			   ("fuzzy string" f)
			   ("regexp" r))))
	  (perms '(("temporary" (current-time-string))
		   ("permanent" nil)
		   ("immediate" now)))
	  header)
      (list
       (apply
	'nconc
	(list
	 (if (eq type 'lower)
	     "Lower score"
	   "Increase score"))
	(let (outh)
	  (while headers
	    (setq header (car headers))
	    (setq outh
		  (cons
		   (apply
		    'nconc
		    (list (car header))
		    (let ((ts (cdr (assoc (nth 2 header) types)))
			  outt)
		      (while ts
			(setq outt
			      (cons
			       (apply
				'nconc
				(list (caar ts))
				(let ((ps perms)
				      outp)
				  (while ps
				    (setq outp
					  (cons
					   (vector
					    (caar ps)
					    (list
					     'gnus-summary-score-entry
					     (nth 1 header)
					     (if (or (string= (nth 1 header)
							      "head")
						     (string= (nth 1 header)
							      "body"))
						 ""
					       (list 'gnus-summary-header
						     (nth 1 header)))
					     (list 'quote (nth 1 (car ts)))
					     (list 'gnus-score-delta-default
						   nil)
					     (nth 1 (car ps))
					     t)
					    t)
					   outp))
				    (setq ps (cdr ps)))
				  (list (nreverse outp))))
			       outt))
			(setq ts (cdr ts)))
		      (list (nreverse outt))))
		   outh))
	    (setq headers (cdr headers)))
	  (list (nreverse outh))))))))


(declare-function turn-on-gnus-mailing-list-mode "gnus-ml" ())
(defvar bookmark-make-record-function)

(defvar bidi-paragraph-direction)

(defun gnus-summary-mode (&optional group)
  "Major mode for reading articles.

All normal editing commands are switched off.
\\<gnus-summary-mode-map>
Each line in this buffer represents one article.  To read an
article, you can, for instance, type `\\[gnus-summary-next-page]'.  To move forwards
and backwards while displaying articles, type `\\[gnus-summary-next-unread-article]' and `\\[gnus-summary-prev-unread-article]',
respectively.

You can also post articles and send mail from this buffer.  To
follow up an article, type `\\[gnus-summary-followup]'.  To mail a reply to the author
of an article, type `\\[gnus-summary-reply]'.

There are approx. one gazillion commands you can execute in this
buffer; read the info pages for more information (`\\[gnus-info-find-node]').

The following commands are available:

\\{gnus-summary-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (let ((gnus-summary-local-variables gnus-newsgroup-variables))
    (gnus-summary-make-local-variables))
  (gnus-summary-make-local-variables)
  (setq gnus-newsgroup-name group)
  (when (gnus-visual-p 'summary-menu 'menu)
    (gnus-summary-make-menu-bar)
    (gnus-summary-make-tool-bar))
  (gnus-make-thread-indent-array)
  (gnus-simplify-mode-line)
  (setq major-mode 'gnus-summary-mode)
  (setq mode-name "Summary")
  (use-local-map gnus-summary-mode-map)
  (buffer-disable-undo)
  (setq buffer-read-only t		;Disable modification
	show-trailing-whitespace nil)
  (setq truncate-lines t)
  ;; Force paragraph direction to be left-to-right.  Don't make it
  ;; bound globally in old Emacsen and XEmacsen.
  (set (make-local-variable 'bidi-paragraph-direction) 'left-to-right)
  (add-to-invisibility-spec '(gnus-sum . t))
  (gnus-summary-set-display-table)
  (gnus-set-default-directory)
  (make-local-variable 'gnus-summary-line-format)
  (make-local-variable 'gnus-summary-line-format-spec)
  (make-local-variable 'gnus-summary-dummy-line-format)
  (make-local-variable 'gnus-summary-dummy-line-format-spec)
  (make-local-variable 'gnus-summary-mark-positions)
  (gnus-make-local-hook 'pre-command-hook)
  (add-hook 'pre-command-hook 'gnus-set-global-variables nil t)
  (gnus-run-mode-hooks 'gnus-summary-mode-hook)
  (turn-on-gnus-mailing-list-mode)
  (mm-enable-multibyte)
  (set (make-local-variable 'bookmark-make-record-function)
       'gnus-summary-bookmark-make-record)
  (gnus-update-format-specifications nil 'summary 'summary-mode 'summary-dummy)
  (gnus-update-summary-mark-positions))

(defun gnus-summary-make-local-variables ()
  "Make all the local summary buffer variables."
  (let (global)
    (dolist (local gnus-summary-local-variables)
      (if (consp local)
	  (progn
	    (if (eq (cdr local) 'global)
		;; Copy the global value of the variable.
		(setq global (symbol-value (car local)))
	      ;; Use the value from the list.
	      (setq global (eval (cdr local))))
	    (set (make-local-variable (car local)) global))
	;; Simple nil-valued local variable.
	(set (make-local-variable local) nil)))))

;; Summary data functions.

(defmacro gnus-data-number (data)
  `(car ,data))

(defmacro gnus-data-set-number (data number)
  `(setcar ,data ,number))

(defmacro gnus-data-mark (data)
  `(nth 1 ,data))

(defmacro gnus-data-set-mark (data mark)
  `(setcar (nthcdr 1 ,data) ,mark))

(defmacro gnus-data-pos (data)
  `(nth 2 ,data))

(defmacro gnus-data-set-pos (data pos)
  `(setcar (nthcdr 2 ,data) ,pos))

(defmacro gnus-data-header (data)
  `(nth 3 ,data))

(defmacro gnus-data-set-header (data header)
  `(setf (nth 3 ,data) ,header))

(defmacro gnus-data-level (data)
  `(nth 4 ,data))

(defmacro gnus-data-unread-p (data)
  `(= (nth 1 ,data) gnus-unread-mark))

(defmacro gnus-data-read-p (data)
  `(/= (nth 1 ,data) gnus-unread-mark))

(defmacro gnus-data-pseudo-p (data)
  `(consp (nth 3 ,data)))

(defmacro gnus-data-find (number)
  `(assq ,number gnus-newsgroup-data))

(defmacro gnus-data-find-list (number &optional data)
  `(let ((bdata ,(or data 'gnus-newsgroup-data)))
     (memq (assq ,number bdata)
	   bdata)))

(defmacro gnus-data-make (number mark pos header level)
  `(list ,number ,mark ,pos ,header ,level))

(defun gnus-data-enter (after-article number mark pos header level offset)
  (let ((data (gnus-data-find-list after-article)))
    (unless data
      (error "No such article: %d" after-article))
    (setcdr data (cons (gnus-data-make number mark pos header level)
		       (cdr data)))
    (setq gnus-newsgroup-data-reverse nil)
    (gnus-data-update-list (cddr data) offset)))

(defun gnus-data-enter-list (after-article list &optional offset)
  (when list
    (let ((data (and after-article (gnus-data-find-list after-article)))
	  (ilist list))
      (if (not (or data
		   after-article))
	  (let ((odata gnus-newsgroup-data))
	    (setq gnus-newsgroup-data (nconc list gnus-newsgroup-data))
	    (when offset
	      (gnus-data-update-list odata offset)))
	;; Find the last element in the list to be spliced into the main
	;; list.
	(setq list (last list))
	(if (not data)
	    (progn
	      (setcdr list gnus-newsgroup-data)
	      (setq gnus-newsgroup-data ilist)
	      (when offset
		(gnus-data-update-list (cdr list) offset)))
	  (setcdr list (cdr data))
	  (setcdr data ilist)
	  (when offset
	    (gnus-data-update-list (cdr list) offset))))
      (setq gnus-newsgroup-data-reverse nil))))

(defun gnus-data-remove (article &optional offset)
  (let ((data gnus-newsgroup-data))
    (if (= (gnus-data-number (car data)) article)
	(progn
	  (setq gnus-newsgroup-data (cdr gnus-newsgroup-data)
		gnus-newsgroup-data-reverse nil)
	  (when offset
	    (gnus-data-update-list gnus-newsgroup-data offset)))
      (while (cdr data)
	(when (= (gnus-data-number (cadr data)) article)
	  (setcdr data (cddr data))
	  (when offset
	    (gnus-data-update-list (cdr data) offset))
	  (setq data nil
		gnus-newsgroup-data-reverse nil))
	(setq data (cdr data))))))

(defmacro gnus-data-list (backward)
  `(if ,backward
       (or gnus-newsgroup-data-reverse
	   (setq gnus-newsgroup-data-reverse
		 (reverse gnus-newsgroup-data)))
     gnus-newsgroup-data))

(defun gnus-data-update-list (data offset)
  "Add OFFSET to the POS of all data entries in DATA."
  (setq gnus-newsgroup-data-reverse nil)
  (while data
    (setcar (nthcdr 2 (car data)) (+ offset (nth 2 (car data))))
    (setq data (cdr data))))

(defun gnus-summary-article-pseudo-p (article)
  "Say whether this article is a pseudo article or not."
  (not (vectorp (gnus-data-header (gnus-data-find article)))))

(defmacro gnus-summary-article-sparse-p (article)
  "Say whether this article is a sparse article or not."
  `(memq ,article gnus-newsgroup-sparse))

(defmacro gnus-summary-article-ancient-p (article)
  "Say whether this article is a sparse article or not."
  `(memq ,article gnus-newsgroup-ancient))

(defun gnus-article-parent-p (number)
  "Say whether this article is a parent or not."
  (let ((data (gnus-data-find-list number)))
    (and (cdr data)              ; There has to be an article after...
	 (< (gnus-data-level (car data)) ; And it has to have a higher level.
	    (gnus-data-level (nth 1 data))))))

(defun gnus-article-children (number)
  "Return a list of all children to NUMBER."
  (let* ((data (gnus-data-find-list number))
	 (level (gnus-data-level (car data)))
	 children)
    (setq data (cdr data))
    (while (and data
		(= (gnus-data-level (car data)) (1+ level)))
      (push (gnus-data-number (car data)) children)
      (setq data (cdr data)))
    children))

(defmacro gnus-summary-skip-intangible ()
  "If the current article is intangible, then jump to a different article."
  '(let ((to (get-text-property (point) 'gnus-intangible)))
     (and to (gnus-summary-goto-subject to))))

(defmacro gnus-summary-article-intangible-p ()
  "Say whether this article is intangible or not."
  '(get-text-property (point) 'gnus-intangible))

(defun gnus-article-read-p (article)
  "Say whether ARTICLE is read or not."
  (not (or (memq article gnus-newsgroup-marked)
	   (memq article gnus-newsgroup-spam-marked)
	   (memq article gnus-newsgroup-unreads)
	   (memq article gnus-newsgroup-unselected)
	   (memq article gnus-newsgroup-dormant))))

;; Some summary mode macros.

(defmacro gnus-summary-article-number ()
  "The article number of the article on the current line.
If there isn't an article number here, then we return the current
article number."
  '(progn
     (gnus-summary-skip-intangible)
     (or (get-text-property (point) 'gnus-number)
	 (gnus-summary-last-subject))))

(defmacro gnus-summary-article-header (&optional number)
  "Return the header of article NUMBER."
  `(gnus-data-header (gnus-data-find
		      ,(or number '(gnus-summary-article-number)))))

(defmacro gnus-summary-thread-level (&optional number)
  "Return the level of thread that starts with article NUMBER."
  `(if (and (eq gnus-summary-make-false-root 'dummy)
	    (get-text-property (point) 'gnus-intangible))
       0
     (gnus-data-level (gnus-data-find
		       ,(or number '(gnus-summary-article-number))))))

(defmacro gnus-summary-article-mark (&optional number)
  "Return the mark of article NUMBER."
  `(gnus-data-mark (gnus-data-find
		    ,(or number '(gnus-summary-article-number)))))

(defmacro gnus-summary-article-pos (&optional number)
  "Return the position of the line of article NUMBER."
  `(gnus-data-pos (gnus-data-find
		   ,(or number '(gnus-summary-article-number)))))

(defalias 'gnus-summary-subject-string 'gnus-summary-article-subject)
(defmacro gnus-summary-article-subject (&optional number)
  "Return current subject string or nil if nothing."
  `(let ((headers
	  ,(if number
	       `(gnus-data-header (assq ,number gnus-newsgroup-data))
	     '(gnus-data-header (assq (gnus-summary-article-number)
				      gnus-newsgroup-data)))))
     (and headers
	  (vectorp headers)
	  (mail-header-subject headers))))

(defmacro gnus-summary-article-score (&optional number)
  "Return current article score."
  `(or (cdr (assq ,(or number '(gnus-summary-article-number))
		  gnus-newsgroup-scored))
       gnus-summary-default-score 0))

(defun gnus-summary-article-children (&optional number)
  "Return a list of article numbers that are children of article NUMBER."
  (let* ((data (gnus-data-find-list (or number (gnus-summary-article-number))))
	 (level (gnus-data-level (car data)))
	 l children)
    (while (and (setq data (cdr data))
		(> (setq l (gnus-data-level (car data))) level))
      (and (= (1+ level) l)
	   (push (gnus-data-number (car data))
		 children)))
    (nreverse children)))

(defun gnus-summary-article-parent (&optional number)
  "Return the article number of the parent of article NUMBER."
  (let* ((data (gnus-data-find-list (or number (gnus-summary-article-number))
				    (gnus-data-list t)))
	 (level (gnus-data-level (car data))))
    (if (zerop level)
	()				; This is a root.
      ;; We search until we find an article with a level less than
      ;; this one.  That function has to be the parent.
      (while (and (setq data (cdr data))
		  (not (< (gnus-data-level (car data)) level))))
      (and data (gnus-data-number (car data))))))

(defun gnus-unread-mark-p (mark)
  "Say whether MARK is the unread mark."
  (= mark gnus-unread-mark))

(defun gnus-read-mark-p (mark)
  "Say whether MARK is one of the marks that mark as read.
This is all marks except unread, ticked, dormant, and expirable."
  (not (or (= mark gnus-unread-mark)
	   (= mark gnus-ticked-mark)
	   (= mark gnus-spam-mark)
	   (= mark gnus-dormant-mark)
	   (= mark gnus-expirable-mark))))

(defmacro gnus-article-mark (number)
  "Return the MARK of article NUMBER.
This macro should only be used when computing the mark the \"first\"
time; i.e., when generating the summary lines.  After that,
`gnus-summary-article-mark' should be used to examine the
marks of articles."
  `(cond
    ((memq ,number gnus-newsgroup-unsendable) gnus-unsendable-mark)
    ((memq ,number gnus-newsgroup-downloadable) gnus-downloadable-mark)
    ((memq ,number gnus-newsgroup-unreads) gnus-unread-mark)
    ((memq ,number gnus-newsgroup-marked) gnus-ticked-mark)
    ((memq ,number gnus-newsgroup-spam-marked) gnus-spam-mark)
    ((memq ,number gnus-newsgroup-dormant) gnus-dormant-mark)
    ((memq ,number gnus-newsgroup-expirable) gnus-expirable-mark)
    (t (or (cdr (assq ,number gnus-newsgroup-reads))
	   gnus-ancient-mark))))

;; Saving hidden threads.

(defmacro gnus-save-hidden-threads (&rest forms)
  "Save hidden threads, eval FORMS, and restore the hidden threads."
  (let ((config (make-symbol "config")))
    `(let ((,config (gnus-hidden-threads-configuration)))
       (unwind-protect
	   (save-excursion
	     ,@forms)
	 (gnus-restore-hidden-threads-configuration ,config)))))
(put 'gnus-save-hidden-threads 'lisp-indent-function 0)
(put 'gnus-save-hidden-threads 'edebug-form-spec '(body))

(defun gnus-data-compute-positions ()
  "Compute the positions of all articles."
  (setq gnus-newsgroup-data-reverse nil)
  (let ((data gnus-newsgroup-data))
    (save-excursion
      (gnus-save-hidden-threads
	(gnus-summary-show-all-threads)
	(goto-char (point-min))
	(while data
	  (while (get-text-property (point) 'gnus-intangible)
	    (forward-line 1))
	  (gnus-data-set-pos (car data) (+ (point) 3))
	  (setq data (cdr data))
	  (forward-line 1))))))

(defun gnus-hidden-threads-configuration ()
  "Return the current hidden threads configuration."
  (save-excursion
    (let (config)
      (goto-char (point-min))
      (while (not (eobp))
        (when (eq (get-char-property (point-at-eol) 'invisible) 'gnus-sum)
          (push (save-excursion (forward-line 0) (point)) config))
        (forward-line 1))
      config)))

(defun gnus-restore-hidden-threads-configuration (config)
  "Restore hidden threads configuration from CONFIG."
  (save-excursion
    (let (point (inhibit-read-only t))
      (while (setq point (pop config))
        (goto-char point)
        (gnus-summary-hide-thread)))))

;; Various summary mode internalish functions.

(defun gnus-mouse-pick-article (e)
  (interactive "e")
  (mouse-set-point e)
  (gnus-summary-next-page nil t))

(defun gnus-summary-set-display-table ()
  "Change the display table.
Odd characters have a tendency to mess
up nicely formatted displays - we make all possible glyphs
display only a single character."

  ;; We start from the standard display table, if any.
  (let ((table (or (copy-sequence standard-display-table)
		   (make-display-table)))
	(i 32))
    ;; Nix out all the control chars...
    (while (>= (setq i (1- i)) 0)
      (gnus-put-display-table i [??] table))
   ;; ... but not newline and cr, of course.  (cr is necessary for the
    ;; selective display).
    (gnus-put-display-table ?\n nil table)
    (gnus-put-display-table ?\r nil table)
    ;; We keep TAB as well.
    (gnus-put-display-table ?\t nil table)
    ;; We nix out any glyphs 127 through 255, or 127 through 159 in
    ;; Emacs 23 (unicode), that are not set already.
    (let ((i (if (ignore-errors (= (make-char 'latin-iso8859-1 160) 160))
		 160
	       256)))
      (while (>= (setq i (1- i)) 127)
	;; Only modify if the entry is nil.
	(unless (gnus-get-display-table i table)
	  (gnus-put-display-table i [??] table))))
    (setq buffer-display-table table)))

(defun gnus-summary-set-article-display-arrow (pos)
  "Update the overlay arrow to point to line at position POS."
  (when gnus-summary-display-arrow
    (make-local-variable 'overlay-arrow-position)
    (make-local-variable 'overlay-arrow-string)
    (save-excursion
      (goto-char pos)
      (beginning-of-line)
      (unless overlay-arrow-position
	(setq overlay-arrow-position (make-marker)))
      (setq overlay-arrow-string "=>"
	    overlay-arrow-position (set-marker overlay-arrow-position
					       (point)
					       (current-buffer))))))

(defun gnus-summary-setup-buffer (group)
  "Initialize summary buffer.
If the setup was successful, non-nil is returned."
  (let ((buffer (gnus-summary-buffer-name group))
	(dead-name (concat "*Dead Summary "
			   (gnus-group-decoded-name group) "*")))
    ;; If a dead summary buffer exists, we kill it.
    (when (gnus-buffer-live-p dead-name)
      (gnus-kill-buffer dead-name))
    (if (get-buffer buffer)
	(progn
	  (set-buffer buffer)
	  (setq gnus-summary-buffer (current-buffer))
	  (not gnus-newsgroup-prepared))
      ;; Fix by Sudish Joseph <joseph@cis.ohio-state.edu>
      (setq gnus-summary-buffer (set-buffer (gnus-get-buffer-create buffer)))
      (gnus-summary-mode group)
      (when (gnus-group-quit-config group)
	(set (make-local-variable 'gnus-single-article-buffer) nil))
      (make-local-variable 'gnus-article-buffer)
      (make-local-variable 'gnus-article-current)
      (make-local-variable 'gnus-original-article-buffer)
      (setq gnus-newsgroup-name group)
      ;; Set any local variables in the group parameters.
      (gnus-summary-set-local-parameters gnus-newsgroup-name)
      t)))

(defun gnus-set-global-variables ()
  "Set the global equivalents of the buffer-local variables.
They are set to the latest values they had.  These reflect the summary
buffer that was in action when the last article was fetched."
  (when (eq major-mode 'gnus-summary-mode)
    (setq gnus-summary-buffer (current-buffer))
    (let ((name gnus-newsgroup-name)
	  (marked gnus-newsgroup-marked)
	  (spam gnus-newsgroup-spam-marked)
	  (unread gnus-newsgroup-unreads)
	  (headers gnus-current-headers)
	  (data gnus-newsgroup-data)
	  (summary gnus-summary-buffer)
	  (article-buffer gnus-article-buffer)
	  (original gnus-original-article-buffer)
	  (gac gnus-article-current)
	  (reffed gnus-reffed-article-number)
	  (score-file gnus-current-score-file)
	  (default-charset gnus-newsgroup-charset)
	  vlist)
      (let ((locals gnus-newsgroup-variables))
	(while locals
	  (if (consp (car locals))
	      (push (eval (caar locals)) vlist)
	    (push (eval (car locals)) vlist))
	  (setq locals (cdr locals)))
	(setq vlist (nreverse vlist)))
      (with-current-buffer gnus-group-buffer
	(setq gnus-newsgroup-name name
	      gnus-newsgroup-marked marked
	      gnus-newsgroup-spam-marked spam
	      gnus-newsgroup-unreads unread
	      gnus-current-headers headers
	      gnus-newsgroup-data data
	      gnus-article-current gac
	      gnus-summary-buffer summary
	      gnus-article-buffer article-buffer
	      gnus-original-article-buffer original
	      gnus-reffed-article-number reffed
	      gnus-current-score-file score-file
	      gnus-newsgroup-charset default-charset)
	(let ((locals gnus-newsgroup-variables))
	  (while locals
	    (if (consp (car locals))
		(set (caar locals) (pop vlist))
	      (set (car locals) (pop vlist)))
	    (setq locals (cdr locals))))
	;; The article buffer also has local variables.
	(when (gnus-buffer-live-p gnus-article-buffer)
	  (set-buffer gnus-article-buffer)
	  (setq gnus-summary-buffer summary))))))

(defun gnus-summary-article-unread-p (article)
  "Say whether ARTICLE is unread or not."
  (memq article gnus-newsgroup-unreads))

(defun gnus-summary-first-article-p (&optional article)
  "Return whether ARTICLE is the first article in the buffer."
  (if (not (setq article (or article (gnus-summary-article-number))))
      nil
    (eq article (caar gnus-newsgroup-data))))

(defun gnus-summary-last-article-p (&optional article)
  "Return whether ARTICLE is the last article in the buffer."
  (if (not (setq article (or article (gnus-summary-article-number))))
      ;; All non-existent numbers are the last article.  :-)
      t
    (not (cdr (gnus-data-find-list article)))))

(defun gnus-make-thread-indent-array (&optional n)
  (when (or n
	    (progn (setq n 200) nil)
	    (null gnus-thread-indent-array)
	    (/= gnus-thread-indent-level gnus-thread-indent-array-level))
    (setq gnus-thread-indent-array (make-vector (1+ n) "")
	  gnus-thread-indent-array-level gnus-thread-indent-level)
    (while (>= n 0)
      (aset gnus-thread-indent-array n
	    (make-string (* n gnus-thread-indent-level) ? ))
      (setq n (1- n)))))

(defun gnus-update-summary-mark-positions ()
  "Compute where the summary marks are to go."
  (save-excursion
    (when (gnus-buffer-exists-p gnus-summary-buffer)
      (set-buffer gnus-summary-buffer))
    (let ((spec gnus-summary-line-format-spec)
	  pos)
      (save-excursion
	(gnus-set-work-buffer)
	(let ((gnus-tmp-unread ?Z)
	      (gnus-replied-mark ?Z)
	      (gnus-score-below-mark ?Z)
	      (gnus-score-over-mark ?Z)
	      (gnus-undownloaded-mark ?Z)
	      (gnus-summary-line-format-spec spec)
	      (gnus-newsgroup-downloadable '(0))
	      (header [0 "" "" "05 Apr 2001 23:33:09 +0400" "" "" 0 0 "" nil])
	      case-fold-search ignores)
	  ;; Here, all marks are bound to Z.
	  (gnus-summary-insert-line header
				    0 nil t gnus-tmp-unread t nil "" nil 1)
	  (goto-char (point-min))
	  ;; Memorize the positions of the same characters as dummy marks.
	  (while (re-search-forward "[A-D]" nil t)
	    (push (point) ignores))
	  (erase-buffer)
	  ;; We use A-D as dummy marks in order to know column positions
	  ;; where marks should be inserted.
	  (setq gnus-tmp-unread ?A
		gnus-replied-mark ?B
		gnus-score-below-mark ?C
		gnus-score-over-mark ?C
		gnus-undownloaded-mark ?D)
	  (gnus-summary-insert-line header
				    0 nil t gnus-tmp-unread t nil "" nil 1)
	  ;; Ignore characters which aren't dummy marks.
	  (dolist (p ignores)
	    (delete-region (goto-char (1- p)) p)
	    (insert ?Z))
	  (goto-char (point-min))
	  (setq pos (list (cons 'unread
				(and (search-forward "A" nil t)
				     (- (point) (point-min) 1)))))
	  (goto-char (point-min))
	  (push (cons 'replied (and (search-forward "B" nil t)
				    (- (point) (point-min) 1)))
		pos)
	  (goto-char (point-min))
	  (push (cons 'score (and (search-forward "C" nil t)
				  (- (point) (point-min) 1)))
		pos)
	  (goto-char (point-min))
	  (push (cons 'download (and (search-forward "D" nil t)
				     (- (point) (point-min) 1)))
		pos)))
      (setq gnus-summary-mark-positions pos))))

(defun gnus-summary-insert-dummy-line (gnus-tmp-subject gnus-tmp-number)
  "Insert a dummy root in the summary buffer."
  (beginning-of-line)
  (gnus-add-text-properties
   (point) (progn (eval gnus-summary-dummy-line-format-spec) (point))
   (list 'gnus-number gnus-tmp-number 'gnus-intangible gnus-tmp-number)))

(defun gnus-summary-extract-address-component (from)
  (or (car (funcall gnus-extract-address-components from))
      from))

(defun gnus-summary-from-or-to-or-newsgroups (header gnus-tmp-from)
  (let ((mail-parse-charset gnus-newsgroup-charset)
	(ignored-from-addresses (gnus-ignored-from-addresses))
	; Is it really necessary to do this next part for each summary line?
	; Luckily, doesn't seem to slow things down much.
	(mail-parse-ignored-charsets
	 (with-current-buffer gnus-summary-buffer
	   gnus-newsgroup-ignored-charsets)))
    (or
     (and ignored-from-addresses
	  (string-match ignored-from-addresses gnus-tmp-from)
	  (let ((extra-headers (mail-header-extra header))
		to
		newsgroups)
	    (cond
	     ((setq to (cdr (assq 'To extra-headers)))
	      (concat gnus-summary-to-prefix
		      (inline
			(gnus-summary-extract-address-component
			 (funcall gnus-decode-encoded-address-function to)))))
	     ((setq newsgroups
		    (or
		     (cdr (assq 'Newsgroups extra-headers))
		     (and
		      (memq 'Newsgroups gnus-extra-headers)
                      (eq (car (gnus-find-method-for-group
                                gnus-newsgroup-name)) 'nntp)
		      (gnus-group-real-name gnus-newsgroup-name))))
	      (concat gnus-summary-newsgroup-prefix newsgroups)))))
     (gnus-string-mark-left-to-right
      (inline
       (gnus-summary-extract-address-component gnus-tmp-from))))))

(defun gnus-summary-insert-line (gnus-tmp-header
				 gnus-tmp-level gnus-tmp-current
				 undownloaded gnus-tmp-unread gnus-tmp-replied
				 gnus-tmp-expirable gnus-tmp-subject-or-nil
				 &optional gnus-tmp-dummy gnus-tmp-score
				 gnus-tmp-process)
  (if (>= gnus-tmp-level (length gnus-thread-indent-array))
      (gnus-make-thread-indent-array (max (* 2 (length gnus-thread-indent-array))
					  gnus-tmp-level)))
  (let* ((gnus-tmp-indentation (aref gnus-thread-indent-array gnus-tmp-level))
	 (gnus-tmp-lines (mail-header-lines gnus-tmp-header))
	 (gnus-tmp-score (or gnus-tmp-score gnus-summary-default-score 0))
	 (gnus-tmp-score-char
	  (if (or (null gnus-summary-default-score)
		  (<= (abs (- gnus-tmp-score gnus-summary-default-score))
		      gnus-summary-zcore-fuzz))
	      ?                         ;Whitespace
	    (if (< gnus-tmp-score gnus-summary-default-score)
		gnus-score-below-mark gnus-score-over-mark)))
	 (gnus-tmp-number (mail-header-number gnus-tmp-header))
	 (gnus-tmp-replied
	  (cond (gnus-tmp-process gnus-process-mark)
		((memq gnus-tmp-current gnus-newsgroup-cached)
		 gnus-cached-mark)
		(gnus-tmp-replied gnus-replied-mark)
		((memq gnus-tmp-current gnus-newsgroup-forwarded)
		 gnus-forwarded-mark)
		((memq gnus-tmp-current gnus-newsgroup-saved)
		 gnus-saved-mark)
		((memq gnus-tmp-number gnus-newsgroup-unseen)
		 gnus-unseen-mark)
		(t gnus-no-mark)))
	 (gnus-tmp-downloaded
	  (cond (undownloaded
                 gnus-undownloaded-mark)
                (gnus-newsgroup-agentized
                 gnus-downloaded-mark)
                (t
                 gnus-no-mark)))
	 (gnus-tmp-from (mail-header-from gnus-tmp-header))
	 (gnus-tmp-name
	  (cond
	   ((string-match "<[^>]+> *$" gnus-tmp-from)
	    (let ((beg (match-beginning 0)))
	      (or (and (string-match "^\".+\"" gnus-tmp-from)
		       (substring gnus-tmp-from 1 (1- (match-end 0))))
		  (substring gnus-tmp-from 0 beg))))
	   ((string-match "(.+)" gnus-tmp-from)
	    (substring gnus-tmp-from
		       (1+ (match-beginning 0)) (1- (match-end 0))))
	   (t gnus-tmp-from)))
	 (gnus-tmp-subject (mail-header-subject gnus-tmp-header))
	 (gnus-tmp-opening-bracket (if gnus-tmp-dummy ?\< ?\[))
	 (gnus-tmp-closing-bracket (if gnus-tmp-dummy ?\> ?\]))
	 (inhibit-read-only t))
    (when (string= gnus-tmp-name "")
      (setq gnus-tmp-name gnus-tmp-from))
    (unless (numberp gnus-tmp-lines)
      (setq gnus-tmp-lines -1))
    (if (= gnus-tmp-lines -1)
	(setq gnus-tmp-lines "?")
      (setq gnus-tmp-lines (number-to-string gnus-tmp-lines)))
    (condition-case ()
	(gnus-put-text-property
	 (point)
	 (progn (eval gnus-summary-line-format-spec) (point))
	 'gnus-number gnus-tmp-number)
      (error (gnus-message 5 "Error updating the summary line")))
    (when (gnus-visual-p 'summary-highlight 'highlight)
      (forward-line -1)
      (gnus-summary-highlight-line)
      (gnus-run-hooks 'gnus-summary-update-hook)
      (forward-line 1))))

(defun gnus-summary-update-line (&optional dont-update)
  "Update summary line after change."
  (when (and gnus-summary-default-score
	     (not gnus-summary-inhibit-highlight))
    (let* ((gnus-summary-inhibit-highlight t) ; Prevent recursion.
	   (article (gnus-summary-article-number))
	   (score (gnus-summary-article-score article)))
      (unless dont-update
	(if (and gnus-summary-mark-below
		 (< (gnus-summary-article-score)
		    gnus-summary-mark-below))
	    ;; This article has a low score, so we mark it as read.
	    (when (memq article gnus-newsgroup-unreads)
	      (gnus-summary-mark-article-as-read gnus-low-score-mark))
	  (when (eq (gnus-summary-article-mark) gnus-low-score-mark)
	    ;; This article was previously marked as read on account
	    ;; of a low score, but now it has risen, so we mark it as
	    ;; unread.
	    (gnus-summary-mark-article-as-unread gnus-unread-mark)))
	(gnus-summary-update-mark
	 (if (or (null gnus-summary-default-score)
		 (<= (abs (- score gnus-summary-default-score))
		     gnus-summary-zcore-fuzz))
	     ?                          ;Whitespace
	   (if (< score gnus-summary-default-score)
	       gnus-score-below-mark gnus-score-over-mark))
	 'score))
      ;; Do visual highlighting.
      (when (gnus-visual-p 'summary-highlight 'highlight)
	(gnus-summary-highlight-line)
	(gnus-run-hooks 'gnus-summary-update-hook)))))

(defvar gnus-tmp-new-adopts nil)

(defun gnus-summary-number-of-articles-in-thread (thread &optional level char)
  "Return the number of articles in THREAD.
This may be 0 in some cases -- if none of the articles in
the thread are to be displayed."
  (let* ((number
	 ;; Fix by Luc Van Eycken <Luc.VanEycken@esat.kuleuven.ac.be>.
	  (cond
	   ((not (listp thread))
	    1)
	   ((and (consp thread) (cdr thread))
	    (apply
	     '+ 1 (mapcar
		   'gnus-summary-number-of-articles-in-thread (cdr thread))))
	   ((null thread)
	    1)
	   ((memq (mail-header-number (car thread)) gnus-newsgroup-limit)
	    1)
	   (t 0))))
    (when (and level (zerop level) gnus-tmp-new-adopts)
      (incf number
	    (apply '+ (mapcar
		       'gnus-summary-number-of-articles-in-thread
		       gnus-tmp-new-adopts))))
    (if char
	(if (> number 1) gnus-not-empty-thread-mark
	  gnus-empty-thread-mark)
      number)))

(defsubst gnus-summary-line-message-size (head)
  "Return pretty-printed version of message size.
This function is intended to be used in
`gnus-summary-line-format-alist'."
  (let ((c (or (mail-header-chars head) -1)))
    (cond ((< c 0) "n/a")		; chars not available
	  ((< c (* 1000 10)) (format "%1.1fk" (/ c 1024.0)))
	  ((< c (* 1000 100)) (format "%dk" (/ c 1024.0)))
	  ((< c (* 1000 10000)) (format "%1.1fM" (/ c (* 1024.0 1024))))
	  (t (format "%dM" (/ c (* 1024.0 1024)))))))

(defcustom gnus-user-date-format-alist
  '(((gnus-seconds-today) . "Today, %H:%M")
    ((+ 86400 (gnus-seconds-today)) . "Yesterday, %H:%M")
    (604800 . "%A %H:%M")               ; That's one week
    ((gnus-seconds-month) . "%A %d")
    ((gnus-seconds-year) . "%B %d")
    (t . "%b %d %Y"))                   ; This one is used when no other
                                        ; does match
  "Specifies date format depending on age of article.
This is an alist of items (AGE . FORMAT).  AGE can be a number (of
seconds) or a Lisp expression evaluating to a number.  When the age of
the article is less than this number, then use `format-time-string'
with the corresponding FORMAT for displaying the date of the article.
If AGE is not a number or a Lisp expression evaluating to a
non-number, then the corresponding FORMAT is used as a default value.

Note that the list is processed from the beginning, so it should be
sorted by ascending AGE.  Also note that items following the first
non-number AGE will be ignored.

You can use the functions `gnus-seconds-today', `gnus-seconds-month'
and `gnus-seconds-year' in the AGE spec.  They return the number of
seconds passed since the start of today, of this month, of this year,
respectively."
  :version "24.1"
  :group 'gnus-summary-format
  :type '(alist :key-type sexp :value-type string))

(defun gnus-user-date (messy-date)
  "Format the messy-date according to `gnus-user-date-format-alist'.
Returns \"  ?  \" if there's bad input or if another error occurs.
Input should look like this: \"Sun, 14 Oct 2001 13:34:39 +0200\"."
  (condition-case ()
      (let* ((messy-date (gnus-float-time (gnus-date-get-time messy-date)))
	     (now (gnus-float-time))
	     ;;If we don't find something suitable we'll use this one
	     (my-format "%b %d '%y"))
	(let* ((difference (- now messy-date))
	       (templist gnus-user-date-format-alist)
	       (top (eval (caar templist))))
	  (while (if (numberp top) (< top difference) (not top))
	    (progn
	      (setq templist (cdr templist))
	      (setq top (eval (caar templist)))))
	  (if (stringp (cdr (car templist)))
	      (setq my-format (cdr (car templist)))))
	(format-time-string (eval my-format) (seconds-to-time messy-date)))
    (error "  ?   ")))

(defun gnus-summary-set-local-parameters (group)
  "Go through the local params of GROUP and set all variable specs in that list."
  (let ((vars '(quit-config active)))	; Ignore things that aren't
					; really variables.
    (dolist (elem (gnus-group-find-parameter group))
      (and (consp elem)			; Has to be a cons.
	   (consp (cdr elem))		; The cdr has to be a list.
	   (symbolp (car elem))		; Has to be a symbol in there.
	   (not (memq (car elem) vars))
	   (ignore-errors
	     (push (car elem) vars)
	     ;; Variables like `gnus-show-threads' that are globally
	     ;; bound, if used as group parameters, need to get to be
	     ;; buffer-local, whereas just parameters like `gcc-self',
	     ;; `timestamp', etc. should not be bound as variables.
	     (if (boundp (car elem))
		 (set (make-local-variable (car elem)) (eval (nth 1 elem)))
	       (eval (nth 1 elem))))))))

(defun gnus-summary-read-group (group &optional show-all no-article
				      kill-buffer no-display backward
				      select-articles)
  "Start reading news in newsgroup GROUP.
If SHOW-ALL is non-nil, already read articles are also listed.
If NO-ARTICLE is non-nil, no article is selected initially.
If NO-DISPLAY, don't generate the summary buffer contents.
If KILL-BUFFER, it should be a buffer that's killed once the new
summary buffer has been generated.
If BACKWARD, move point to the previous group in the group buffer
If SELECT-ARTICLES, only select those articles from GROUP."
  (let (result)
    (while (and group
		(null (setq result
			    (let ((gnus-auto-select-next nil))
			      (or (gnus-summary-read-group-1
				   group show-all no-article
				   kill-buffer no-display
				   select-articles)
				  (setq show-all nil
					select-articles nil)))))
		(eq gnus-auto-select-next 'quietly))
      (set-buffer gnus-group-buffer)
      ;; The entry function called above goes to the next
      ;; group automatically, so we go two groups back
      ;; if we are searching for the previous group.
      (when backward
	(gnus-group-prev-unread-group 2))
      (if (not (equal group (gnus-group-group-name)))
	  (setq group (gnus-group-group-name))
	(setq group nil)))
    result))

(defun gnus-summary-read-group-1 (group show-all no-article
					kill-buffer no-display
					&optional select-articles)
  ;; Killed foreign groups can't be entered.
  ;;  (when (and (not (gnus-group-native-p group))
  ;;	     (not (gnus-gethash group gnus-newsrc-hashtb)))
  ;;    (error "Dead non-native groups can't be entered"))
  (gnus-message 7 "Retrieving newsgroup: %s..."
		(gnus-group-decoded-name group))
  (let* ((new-group (gnus-summary-setup-buffer group))
	 (quit-config (gnus-group-quit-config group))
	 (did-select (and new-group (gnus-select-newsgroup
				     group show-all select-articles))))
    (cond
     ;; This summary buffer exists already, so we just select it.
     ((not new-group)
      (gnus-set-global-variables)
      (when kill-buffer
	(gnus-kill-or-deaden-summary kill-buffer))
      (gnus-configure-windows 'summary 'force)
      (gnus-set-mode-line 'summary)
      (gnus-summary-position-point)
      (message "")
      t)
     ;; We couldn't select this group.
     ((null did-select)
      (when (and (eq major-mode 'gnus-summary-mode)
		 (not (equal (current-buffer) kill-buffer)))
	(kill-buffer (current-buffer))
	(if (not quit-config)
	    (progn
	      ;; Update the info -- marks might need to be removed,
	      ;; for instance.
	      (gnus-summary-update-info)
	      (set-buffer gnus-group-buffer)
	      (gnus-group-jump-to-group group)
	      (gnus-group-next-unread-group 1))
	  (gnus-handle-ephemeral-exit quit-config)))
      (if (null (gnus-list-of-unread-articles group))
	  (gnus-message 3 "Group %s contains no messages" group)
	(gnus-message 3 "Can't select group"))
      nil)
     ;; The user did a `C-g' while prompting for number of articles,
     ;; so we exit this group.
     ((eq did-select 'quit)
      (and (eq major-mode 'gnus-summary-mode)
	   (not (equal (current-buffer) kill-buffer))
	   (kill-buffer (current-buffer)))
      (when kill-buffer
	(gnus-kill-or-deaden-summary kill-buffer))
      (if (not quit-config)
	  (progn
	    (set-buffer gnus-group-buffer)
	    (gnus-group-jump-to-group group)
	    (gnus-configure-windows 'group 'force))
	(gnus-handle-ephemeral-exit quit-config))
      ;; Finally signal the quit.
      (signal 'quit nil))
     ;; The group was successfully selected.
     (t
      (gnus-set-global-variables)
      ;; Save the active value in effect when the group was entered.
      (setq gnus-newsgroup-active
	    (gnus-copy-sequence
	     (gnus-active gnus-newsgroup-name)))
      (setq gnus-newsgroup-highest (cdr gnus-newsgroup-active))
      ;; You can change the summary buffer in some way with this hook.
      (gnus-run-hooks 'gnus-select-group-hook)
      (when (memq 'summary (gnus-update-format-specifications
			    nil 'summary 'summary-mode 'summary-dummy))
	;; The format specification for the summary line was updated,
	;; so we need to update the mark positions as well.
	(gnus-update-summary-mark-positions))
      ;; Do score processing.
      (when gnus-use-scoring
	(gnus-possibly-score-headers))
      ;; Check whether to fill in the gaps in the threads.
      (when gnus-build-sparse-threads
	(gnus-build-sparse-threads))
      ;; Find the initial limit.
      (if show-all
	  (let ((gnus-newsgroup-dormant nil))
	    (gnus-summary-initial-limit show-all))
	(gnus-summary-initial-limit show-all))
      ;; Generate the summary buffer.
      (unless no-display
	(gnus-summary-prepare))
      (when gnus-use-trees
	(gnus-tree-open group)
	(setq gnus-summary-highlight-line-function
	      'gnus-tree-highlight-article))
      ;; If the summary buffer is empty, but there are some low-scored
      ;; articles or some excluded dormants, we include these in the
      ;; buffer.
      (when (and (zerop (buffer-size))
		 (not no-display))
	(cond (gnus-newsgroup-dormant
	       (gnus-summary-limit-include-dormant))
	      ((and gnus-newsgroup-scored show-all)
	       (gnus-summary-limit-include-expunged t))))
      ;; Function `gnus-apply-kill-file' must be called in this hook.
      (gnus-run-hooks 'gnus-apply-kill-hook)
      (if (and (zerop (buffer-size))
	       (not no-display))
	  (progn
	    ;; This newsgroup is empty.
	    (gnus-summary-catchup-and-exit nil t)
	    (gnus-message 6 "No unread news")
	    (when kill-buffer
	      (gnus-kill-or-deaden-summary kill-buffer))
	    ;; Return nil from this function.
	    nil)
	;; Hide conversation thread subtrees.  We cannot do this in
	;; gnus-summary-prepare-hook since kill processing may not
	;; work with hidden articles.
	(gnus-summary-maybe-hide-threads)
	(gnus-configure-windows 'summary)
	(when kill-buffer
	  (gnus-kill-or-deaden-summary kill-buffer))
	(gnus-summary-auto-select-subject)
	;; Show first unread article if requested.
	(if (and (not no-article)
		 (not no-display)
		 gnus-newsgroup-unreads
		 gnus-auto-select-first)
	    (progn
	      (let ((art (gnus-summary-article-number)))
		(unless (and (not gnus-plugged)
			     (or (memq art gnus-newsgroup-undownloaded)
				 (memq art gnus-newsgroup-downloadable)))
		  (gnus-summary-goto-article art))))
	  ;; Don't select any articles.
	  (gnus-summary-position-point)
	  (gnus-configure-windows 'summary 'force)
	  (gnus-set-mode-line 'summary))
	(when (and gnus-auto-center-group
		   (get-buffer-window gnus-group-buffer t))
	  ;; Gotta use windows, because recenter does weird stuff if
	  ;; the current buffer ain't the displayed window.
	  (let ((owin (selected-window)))
	    (select-window (get-buffer-window gnus-group-buffer t))
	    (when (gnus-group-goto-group group)
	      (recenter))
	    (select-window owin)))
	;; Mark this buffer as "prepared".
	(setq gnus-newsgroup-prepared t)
	(gnus-run-hooks 'gnus-summary-prepared-hook)
	(unless (gnus-ephemeral-group-p group)
	  (gnus-group-update-group group nil t))
	t)))))

(defun gnus-summary-auto-select-subject ()
  "Select the subject line on initial group entry."
  (goto-char (point-min))
  (cond
   ((eq gnus-auto-select-subject 'best)
    (gnus-summary-best-unread-subject))
   ((eq gnus-auto-select-subject 'unread)
    (gnus-summary-first-unread-subject))
   ((eq gnus-auto-select-subject 'unseen)
    (gnus-summary-first-unseen-subject))
   ((eq gnus-auto-select-subject 'unseen-or-unread)
    (gnus-summary-first-unseen-or-unread-subject))
   ((eq gnus-auto-select-subject 'first)
    ;; Do nothing.
    )
   ((functionp gnus-auto-select-subject)
    (funcall gnus-auto-select-subject))))

(defun gnus-summary-prepare ()
  "Generate the summary buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq gnus-newsgroup-data nil
	  gnus-newsgroup-data-reverse nil)
    (gnus-run-hooks 'gnus-summary-generate-hook)
    ;; Generate the buffer, either with threads or without.
    (when gnus-newsgroup-headers
      (gnus-summary-prepare-threads
       (if gnus-show-threads
	   (gnus-sort-gathered-threads
	    (funcall gnus-summary-thread-gathering-function
		     (gnus-sort-threads
		      (gnus-cut-threads (gnus-make-threads)))))
	 ;; Unthreaded display.
	 (gnus-sort-articles gnus-newsgroup-headers))))
    (setq gnus-newsgroup-data (nreverse gnus-newsgroup-data))
    ;; Call hooks for modifying summary buffer.
    (goto-char (point-min))
    (gnus-run-hooks 'gnus-summary-prepare-hook)))

(defsubst gnus-general-simplify-subject (subject)
  "Simplify subject by the same rules as `gnus-gather-threads-by-subject'."
  (setq subject
	(cond
	 ;; Truncate the subject.
	 (gnus-simplify-subject-functions
	  (gnus-map-function gnus-simplify-subject-functions subject))
	 ((numberp gnus-summary-gather-subject-limit)
	  (setq subject (gnus-simplify-subject-re subject))
	  (if (> (length subject) gnus-summary-gather-subject-limit)
	      (substring subject 0 gnus-summary-gather-subject-limit)
	    subject))
	 ;; Fuzzily simplify it.
	 ((eq 'fuzzy gnus-summary-gather-subject-limit)
	  (gnus-simplify-subject-fuzzy subject))
	 ;; Just remove the leading "Re:".
	 (t
	  (gnus-simplify-subject-re subject))))

  (if (and gnus-summary-gather-exclude-subject
	   (string-match gnus-summary-gather-exclude-subject subject))
      nil                         ; This article shouldn't be gathered
    subject))

(defun gnus-summary-simplify-subject-query ()
  "Query where the respool algorithm would put this article."
  (interactive)
  (gnus-summary-select-article)
  (message "%s" (gnus-general-simplify-subject (gnus-summary-article-subject))))

(defun gnus-gather-threads-by-subject (threads)
  "Gather threads by looking at Subject headers."
  (if (not gnus-summary-make-false-root)
      threads
    (let ((hashtb (gnus-make-hashtable 1024))
	  (prev threads)
	  (result threads)
	  subject hthread whole-subject)
      (while threads
	(setq subject (gnus-general-simplify-subject
		       (setq whole-subject (mail-header-subject
					    (caar threads)))))
	(when subject
	  (if (setq hthread (gnus-gethash subject hashtb))
	      (progn
		;; We enter a dummy root into the thread, if we
		;; haven't done that already.
		(unless (stringp (caar hthread))
		  (setcar hthread (list whole-subject (car hthread))))
		;; We add this new gathered thread to this gathered
		;; thread.
		(setcdr (car hthread)
			(nconc (cdar hthread) (list (car threads))))
		;; Remove it from the list of threads.
		(setcdr prev (cdr threads))
		(setq threads prev))
	    ;; Enter this thread into the hash table.
	    (gnus-sethash subject
			  (if gnus-summary-make-false-root-always
			      (progn
				;; If you want a dummy root above all
				;; threads...
				(setcar threads (list whole-subject
						      (car threads)))
				threads)
			    threads)
			  hashtb)))
	(setq prev threads)
	(setq threads (cdr threads)))
      result)))

(defun gnus-gather-threads-by-references (threads)
  "Gather threads by looking at References headers."
  (let ((idhashtb (gnus-make-hashtable 1024))
	(thhashtb (gnus-make-hashtable 1024))
	(prev threads)
	(result threads)
	ids references id gthread gid entered ref)
    (while threads
      (when (setq references (mail-header-references (caar threads)))
	(setq id (mail-header-id (caar threads))
	      ids (inline (gnus-split-references references))
	      entered nil)
	(while (setq ref (pop ids))
	  (setq ids (delete ref ids))
	  (if (not (setq gid (gnus-gethash ref idhashtb)))
	      (progn
		(gnus-sethash ref id idhashtb)
		(gnus-sethash id threads thhashtb))
	    (setq gthread (gnus-gethash gid thhashtb))
	    (unless entered
	      ;; We enter a dummy root into the thread, if we
	      ;; haven't done that already.
	      (unless (stringp (caar gthread))
		(setcar gthread (list (mail-header-subject (caar gthread))
				      (car gthread))))
	      ;; We add this new gathered thread to this gathered
	      ;; thread.
	      (setcdr (car gthread)
		      (nconc (cdar gthread) (list (car threads)))))
	    ;; Add it into the thread hash table.
	    (gnus-sethash id gthread thhashtb)
	    (setq entered t)
	    ;; Remove it from the list of threads.
	    (setcdr prev (cdr threads))
	    (setq threads prev))))
      (setq prev threads)
      (setq threads (cdr threads)))
    result))

(defun gnus-sort-gathered-threads (threads)
  "Sort subthreads inside each gathered thread by `gnus-sort-gathered-threads-function'."
  (let ((result threads))
    (while threads
      (when (stringp (caar threads))
	(setcdr (car threads)
		(sort (cdar threads) gnus-sort-gathered-threads-function)))
      (setq threads (cdr threads)))
    result))

(defun gnus-thread-loop-p (root thread)
  "Say whether ROOT is in THREAD."
  (let ((stack (list thread))
	(infloop 0)
	th)
    (while (setq thread (pop stack))
      (setq th (cdr thread))
      (while (and th
		  (not (eq (caar th) root)))
	(pop th))
      (if th
	  ;; We have found a loop.
	  (let (ref-dep)
	    (setcdr thread (delq (car th) (cdr thread)))
	    (if (boundp (setq ref-dep (intern "none"
					      gnus-newsgroup-dependencies)))
		(setcdr (symbol-value ref-dep)
			(nconc (cdr (symbol-value ref-dep))
			       (list (car th))))
	      (set ref-dep (list nil (car th))))
	    (setq infloop 1
		  stack nil))
	;; Push all the subthreads onto the stack.
	(push (cdr thread) stack)))
    infloop))

(defun gnus-make-threads ()
  "Go through the dependency hashtb and find the roots.  Return all threads."
  (let (threads)
    (while (catch 'infloop
	     (mapatoms
	      (lambda (refs)
		;; Deal with self-referencing References loops.
		(when (and (car (symbol-value refs))
			   (not (zerop
				 (apply
				  '+
				  (mapcar
				   (lambda (thread)
				     (gnus-thread-loop-p
				      (car (symbol-value refs)) thread))
				   (cdr (symbol-value refs)))))))
		  (setq threads nil)
		  (throw 'infloop t))
		(unless (car (symbol-value refs))
		  ;; These threads do not refer back to any other
		  ;; articles, so they're roots.
		  (setq threads (append (cdr (symbol-value refs)) threads))))
	      gnus-newsgroup-dependencies)))
    threads))

;; Build the thread tree.
(defsubst gnus-dependencies-add-header (header dependencies force-new)
  "Enter HEADER into the DEPENDENCIES table if it is not already there.

If FORCE-NEW is not nil, enter HEADER into the DEPENDENCIES table even
if it was already present.

If `gnus-summary-ignore-duplicates' is nil then duplicate Message-IDs
will not be entered in the DEPENDENCIES table.  Otherwise duplicate
Message-IDs will be renamed to a unique Message-ID before being
entered.

Returns HEADER if it was entered in the DEPENDENCIES.  Returns nil otherwise."
  (let* ((id (mail-header-id header))
	 (id-dep (and id (intern id dependencies)))
	 parent-id ref ref-dep ref-header replaced)
    ;; Enter this `header' in the `dependencies' table.
    (cond
     ((not id-dep)
      (setq header nil))
     ;; The first two cases do the normal part: enter a new `header'
     ;; in the `dependencies' table.
     ((not (boundp id-dep))
      (set id-dep (list header)))
     ((null (car (symbol-value id-dep)))
      (setcar (symbol-value id-dep) header))

     ;; From here the `header' was already present in the
     ;; `dependencies' table.
     (force-new
      ;; Overrides an existing entry;
      ;; just set the header part of the entry.
      (setcar (symbol-value id-dep) header)
      (setq replaced t))

     ;; Renames the existing `header' to a unique Message-ID.
     ((not gnus-summary-ignore-duplicates)
      ;; An article with this Message-ID has already been seen.
      ;; We rename the Message-ID.
      (set (setq id-dep (intern (setq id (nnmail-message-id)) dependencies))
	   (list header))
      (mail-header-set-id header id))

     ;; The last case ignores an existing entry, except it adds any
     ;; additional Xrefs (in case the two articles came from different
     ;; servers.
     ;; Also sets `header' to `nil' meaning that the `dependencies'
     ;; table was *not* modified.
     (t
      (mail-header-set-xref
       (car (symbol-value id-dep))
       (concat (or (mail-header-xref (car (symbol-value id-dep)))
		   "")
	       (or (mail-header-xref header) "")))
      (setq header nil)))

    (when (and header (not replaced))
      ;; First check that we are not creating a References loop.
      (setq parent-id (gnus-parent-id (mail-header-references header)))
      (setq ref parent-id)
      (while (and ref
		  (setq ref-dep (intern-soft ref dependencies))
		  (boundp ref-dep)
		  (setq ref-header (car (symbol-value ref-dep))))
	(if (string= id ref)
	    ;; Yuk!  This is a reference loop.  Make the article be a
	    ;; root article.
	    (progn
	      (mail-header-set-references (car (symbol-value id-dep)) "none")
	      (setq ref nil)
	      (setq parent-id nil))
	  (setq ref (gnus-parent-id (mail-header-references ref-header)))))
      (setq ref-dep (intern (or parent-id "none") dependencies))
      (if (boundp ref-dep)
	  (setcdr (symbol-value ref-dep)
		  (nconc (cdr (symbol-value ref-dep))
			 (list (symbol-value id-dep))))
	(set ref-dep (list nil (symbol-value id-dep)))))
    header))

(defun gnus-extract-message-id-from-in-reply-to (string)
  (if (string-match "<[^>]+>" string)
      (substring string (match-beginning 0) (match-end 0))
    nil))

(defun gnus-build-sparse-threads ()
  (let ((headers gnus-newsgroup-headers)
	(mail-parse-charset gnus-newsgroup-charset)
	(gnus-summary-ignore-duplicates t)
	header references generation relations
	subject child end new-child date)
    ;; First we create an alist of generations/relations, where
    ;; generations is how much we trust the relation, and the relation
    ;; is parent/child.
    (gnus-message 7 "Making sparse threads...")
    (save-excursion
      (nnheader-set-temp-buffer " *gnus sparse threads*")
      (while (setq header (pop headers))
	(when (and (setq references (mail-header-references header))
		   (not (string= references "")))
	  (insert references)
	  (setq child (mail-header-id header)
		subject (mail-header-subject header)
		date (mail-header-date header)
		generation 0)
	  (while (search-backward ">" nil t)
	    (setq end (1+ (point)))
	    (when (search-backward "<" nil t)
	      (setq new-child (buffer-substring (point) end))
	      (push (list (incf generation)
			  child (setq child new-child)
			  subject date)
		    relations)))
	  (when child
	    (push (list (1+ generation) child nil subject) relations))
	  (erase-buffer)))
      (kill-buffer (current-buffer)))
    ;; Sort over trustworthiness.
    (dolist (relation (sort relations 'car-less-than-car))
      (when (gnus-dependencies-add-header
	     (make-full-mail-header
	      gnus-reffed-article-number
	      (nth 3 relation) "" (or (nth 4 relation) "")
	      (nth 1 relation)
	      (or (nth 2 relation) "") 0 0 "")
	     gnus-newsgroup-dependencies nil)
	(push gnus-reffed-article-number gnus-newsgroup-limit)
	(push gnus-reffed-article-number gnus-newsgroup-sparse)
	(push (cons gnus-reffed-article-number gnus-sparse-mark)
	      gnus-newsgroup-reads)
	(decf gnus-reffed-article-number)))
    (gnus-message 7 "Making sparse threads...done")))

(defun gnus-build-old-threads ()
  ;; Look at all the articles that refer back to old articles, and
  ;; fetch the headers for the articles that aren't there.  This will
  ;; build complete threads - if the roots haven't been expired by the
  ;; server, that is.
  (let ((mail-parse-charset gnus-newsgroup-charset)
	id heads)
    (mapatoms
     (lambda (refs)
       (when (not (car (symbol-value refs)))
	 (setq heads (cdr (symbol-value refs)))
	 (while heads
	   (if (memq (mail-header-number (caar heads))
		     gnus-newsgroup-dormant)
	       (setq heads (cdr heads))
	     (setq id (symbol-name refs))
	     (while (and (setq id (gnus-build-get-header id))
			 (not (car (gnus-id-to-thread id)))))
	     (setq heads nil)))))
     gnus-newsgroup-dependencies)))

(defsubst gnus-remove-odd-characters (string)
  "Translate STRING into something that doesn't contain weird characters."
  (mm-subst-char-in-string
   ?\r ?\-
   (mm-subst-char-in-string ?\n ?\- string t) t))

;; This function has to be called with point after the article number
;; on the beginning of the line.
(defsubst gnus-nov-parse-line (number dependencies &optional force-new)
  (let ((eol (point-at-eol))
	(buffer (current-buffer))
	header references in-reply-to)

    ;; overview: [num subject from date id refs chars lines misc]
    (unwind-protect
	(let (x)
	  (narrow-to-region (point) eol)
	  (unless (eobp)
	    (forward-char))

	  (setq header
		(make-full-mail-header
		 number			; number
		 (condition-case ()	; subject
		     (gnus-remove-odd-characters
		      (funcall gnus-decode-encoded-word-function
			       (setq x (nnheader-nov-field))))
		   (error x))
		 (condition-case ()	; from
		     (gnus-remove-odd-characters
		      (funcall gnus-decode-encoded-address-function
			       (setq x (nnheader-nov-field))))
		   (error x))
		 (nnheader-nov-field)	; date
		 (nnheader-nov-read-message-id number)	; id
		 (setq references (nnheader-nov-field))	; refs
		 (nnheader-nov-read-integer) ; chars
		 (nnheader-nov-read-integer) ; lines
		 (unless (eobp)
		   (if (looking-at "Xref: ")
		       (goto-char (match-end 0)))
		   (nnheader-nov-field)) ; Xref
		 (nnheader-nov-parse-extra)))) ; extra

      (widen))

    (when (and (string= references "")
	       (setq in-reply-to (mail-header-extra header))
	       (setq in-reply-to (cdr (assq 'In-Reply-To in-reply-to))))
      (mail-header-set-references
       header (gnus-extract-message-id-from-in-reply-to in-reply-to)))

    (when gnus-alter-header-function
      (funcall gnus-alter-header-function header))
    (gnus-dependencies-add-header header dependencies force-new)))

(defun gnus-build-get-header (id)
  "Look through the buffer of NOV lines and find the header to ID.
Enter this line into the dependencies hash table, and return
the id of the parent article (if any)."
  (let ((deps gnus-newsgroup-dependencies)
	found header)
    (prog1
	(with-current-buffer nntp-server-buffer
	  (let ((case-fold-search nil))
	    (goto-char (point-min))
	    (while (and (not found)
			(search-forward id nil t))
	      (beginning-of-line)
	      (setq found (looking-at
			   (format "^[^\t]*\t[^\t]*\t[^\t]*\t[^\t]*\t%s"
				   (regexp-quote id))))
	      (or found (beginning-of-line 2)))
	    (when found
	      (beginning-of-line)
	      (and
	       (setq header (gnus-nov-parse-line
			     (read (current-buffer)) deps))
	       (gnus-parent-id (mail-header-references header))))))
      (when header
	(let ((number (mail-header-number header)))
	  (push number gnus-newsgroup-limit)
	  (push header gnus-newsgroup-headers)
	  (if (memq number gnus-newsgroup-unselected)
	      (progn
		(setq gnus-newsgroup-unreads
		      (gnus-add-to-sorted-list gnus-newsgroup-unreads
					       number))
		(setq gnus-newsgroup-unselected
		      (delq number gnus-newsgroup-unselected)))
	    (push number gnus-newsgroup-ancient)))))))

(defun gnus-build-all-threads ()
  "Read all the headers."
  (let ((gnus-summary-ignore-duplicates t)
	(mail-parse-charset gnus-newsgroup-charset)
	(dependencies gnus-newsgroup-dependencies)
	header article)
    (with-current-buffer nntp-server-buffer
      (let ((case-fold-search nil))
	(goto-char (point-min))
	(while (not (eobp))
	  (ignore-errors
	    (setq article (read (current-buffer))
		  header (gnus-nov-parse-line article dependencies t)))
	  (when header
	    (with-current-buffer gnus-summary-buffer
	      (push header gnus-newsgroup-headers)
	      (if (memq (setq article (mail-header-number header))
			gnus-newsgroup-unselected)
		  (progn
		    (setq gnus-newsgroup-unreads
			  (gnus-add-to-sorted-list
			   gnus-newsgroup-unreads article))
		    (setq gnus-newsgroup-unselected
			  (delq article gnus-newsgroup-unselected)))
		(push article gnus-newsgroup-ancient)))
	    (forward-line 1)))))))

(defun gnus-summary-update-article-line (article header)
  "Update the line for ARTICLE using HEADER."
  (let* ((id (mail-header-id header))
	 (thread (gnus-id-to-thread id)))
    (unless thread
      (error "Article in no thread"))
    ;; Update the thread.
    (setcar thread header)
    (gnus-summary-goto-subject article)
    (let* ((datal (gnus-data-find-list article))
	   (data (car datal))
	   (inhibit-read-only t)
	   (level (gnus-summary-thread-level)))
      (gnus-delete-line)
      (let ((inserted (- (point)
                         (progn
                           (gnus-summary-insert-line
                            header level nil
                            (memq article gnus-newsgroup-undownloaded)
                            (gnus-article-mark article)
                            (memq article gnus-newsgroup-replied)
                            (memq article gnus-newsgroup-expirable)
                            ;; Only insert the Subject string when it's different
                            ;; from the previous Subject string.
                            (if (and
                                 gnus-show-threads
                                 (gnus-subject-equal
                                  (condition-case ()
                                      (mail-header-subject
                                       (gnus-data-header
                                        (cadr
                                         (gnus-data-find-list
                                          article
                                          (gnus-data-list t)))))
                                    ;; Error on the side of excessive subjects.
                                    (error ""))
                                  (mail-header-subject header)))
                                ""
                              (mail-header-subject header))
                            nil (cdr (assq article gnus-newsgroup-scored))
                            (memq article gnus-newsgroup-processable))
                           (point)))))
        (when (cdr datal)
          (gnus-data-update-list
           (cdr datal)
           (- (gnus-data-pos data) (gnus-data-pos (cadr datal)) inserted)))))))

(defun gnus-summary-update-article (article &optional iheader)
  "Update ARTICLE in the summary buffer."
  (set-buffer gnus-summary-buffer)
  (let* ((header (gnus-summary-article-header article))
	 (id (mail-header-id header))
	 (data (gnus-data-find article))
	 (thread (gnus-id-to-thread id))
	 (references (mail-header-references header))
	 (parent
	  (gnus-id-to-thread
	   (or (gnus-parent-id
		(when (and references
			   (not (equal "" references)))
		  references))
	       "none")))
	 (inhibit-read-only t)
	 (old (car thread)))
    (when thread
      (unless iheader
	(setcar thread nil)
	(when parent
	  (delq thread parent)))
      (if (gnus-summary-insert-subject id header)
	  ;; Set the (possibly) new article number in the data structure.
	  (gnus-data-set-number data (gnus-id-to-article id))
	(setcar thread old)
	nil))))

(defun gnus-rebuild-thread (id &optional line)
  "Rebuild the thread containing ID.
If LINE, insert the rebuilt thread starting on line LINE."
  (let ((inhibit-read-only t)
	old-pos current thread data)
    (if (not gnus-show-threads)
	(setq thread (list (car (gnus-id-to-thread id))))
      ;; Get the thread this article is part of.
      (setq thread (gnus-remove-thread id)))
    (setq old-pos (point-at-bol))
    (setq current (save-excursion
		    (and (re-search-backward "[\r\n]" nil t)
			 (gnus-summary-article-number))))
    ;; If this is a gathered thread, we have to go some re-gathering.
    (when (stringp (car thread))
      (let ((subject (car thread))
	    roots thr)
	(setq thread (cdr thread))
	(while thread
	  (unless (memq (setq thr (gnus-id-to-thread
				   (gnus-root-id
				    (mail-header-id (caar thread)))))
			roots)
	    (push thr roots))
	  (setq thread (cdr thread)))
	;; We now have all (unique) roots.
	(if (= (length roots) 1)
	    ;; All the loose roots are now one solid root.
	    (setq thread (car roots))
	  (setq thread (cons subject (gnus-sort-threads roots))))))
    (let (threads)
      ;; We then insert this thread into the summary buffer.
      (when line
	(goto-char (point-min))
	(forward-line (1- line)))
      (let (gnus-newsgroup-data gnus-newsgroup-threads)
	(if gnus-show-threads
	    (gnus-summary-prepare-threads (gnus-cut-threads (list thread)))
	  (gnus-summary-prepare-unthreaded thread))
	(setq data (nreverse gnus-newsgroup-data))
	(setq threads gnus-newsgroup-threads))
      ;; We splice the new data into the data structure.
      ;;!!! This is kinda bogus.  We assume that in LINE is non-nil,
      ;;!!! then we want to insert at the beginning of the buffer.
      ;;!!! That happens to be true with Gnus now, but that may
      ;;!!! change in the future.  Perhaps.
      (gnus-data-enter-list
       (if line nil current) data (- (point) old-pos))
      (setq gnus-newsgroup-threads
	    (nconc threads gnus-newsgroup-threads))
      (gnus-data-compute-positions))))

(defun gnus-number-to-header (number)
  "Return the header for article NUMBER."
  (let ((headers gnus-newsgroup-headers))
    (while (and headers
		(not (= number (mail-header-number (car headers)))))
      (pop headers))
    (when headers
      (car headers))))

(defun gnus-parent-headers (in-headers &optional generation)
  "Return the headers of the GENERATIONth parent of HEADERS."
  (unless generation
    (setq generation 1))
  (let ((parent t)
	(headers in-headers)
	references)
    (while (and parent
		(not (zerop generation))
		(setq references (mail-header-references headers)))
      (setq headers (if (and references
			     (setq parent (gnus-parent-id references)))
			(car (gnus-id-to-thread parent))
		      nil))
      (decf generation))
    (and (not (eq headers in-headers))
	 headers)))

(defun gnus-id-to-thread (id)
  "Return the (sub-)thread where ID appears."
  (gnus-gethash id gnus-newsgroup-dependencies))

(defun gnus-id-to-article (id)
  "Return the article number of ID."
  (let ((thread (gnus-id-to-thread id)))
    (when (and thread
	       (car thread))
      (mail-header-number (car thread)))))

(defun gnus-id-to-header (id)
  "Return the article headers of ID."
  (car (gnus-id-to-thread id)))

(defun gnus-article-displayed-root-p (article)
  "Say whether ARTICLE is a root(ish) article."
  (let ((level (gnus-summary-thread-level article))
	(refs (mail-header-references  (gnus-summary-article-header article)))
	particle)
    (cond
     ((null level) nil)
     ((zerop level) t)
     ((null refs) t)
     ((null (gnus-parent-id refs)) t)
     ((and (= 1 level)
	   (null (setq particle (gnus-id-to-article
				 (gnus-parent-id refs))))
	   (null (gnus-summary-thread-level particle)))))))

(defun gnus-root-id (id)
  "Return the id of the root of the thread where ID appears."
  (let (last-id prev)
    (while (and id (setq prev (car (gnus-id-to-thread id))))
      (setq last-id id
	    id (gnus-parent-id (mail-header-references prev))))
    last-id))

(defun gnus-articles-in-thread (thread)
  "Return the list of articles in THREAD."
  (cons (mail-header-number (car thread))
	(apply 'nconc (mapcar 'gnus-articles-in-thread (cdr thread)))))

(defun gnus-remove-thread (id &optional dont-remove)
  "Remove the thread that has ID in it."
  (let (headers thread last-id)
    ;; First go up in this thread until we find the root.
    (setq last-id (gnus-root-id id)
	  headers (message-flatten-list (gnus-id-to-thread last-id)))
    ;; We have now found the real root of this thread.  It might have
    ;; been gathered into some loose thread, so we have to search
    ;; through the threads to find the thread we wanted.
    (let ((threads gnus-newsgroup-threads)
	  sub)
      (while threads
	(setq sub (car threads))
	(if (stringp (car sub))
	    ;; This is a gathered thread, so we look at the roots
	    ;; below it to find whether this article is in this
	    ;; gathered root.
	    (progn
	      (setq sub (cdr sub))
	      (while sub
		(when (member (caar sub) headers)
		  (setq thread (car threads)
			threads nil
			sub nil))
		(setq sub (cdr sub))))
	  ;; It's an ordinary thread, so we check it.
	  (when (eq (car sub) (car headers))
	    (setq thread sub
		  threads nil)))
	(setq threads (cdr threads)))
      ;; If this article is in no thread, then it's a root.
      (if thread
	  (unless dont-remove
	    (setq gnus-newsgroup-threads (delq thread gnus-newsgroup-threads)))
	(setq thread (gnus-id-to-thread last-id)))
      (when thread
	(prog1
	    thread			; We return this thread.
	  (unless dont-remove
	    (if (stringp (car thread))
		(progn
		  ;; If we use dummy roots, then we have to remove the
		  ;; dummy root as well.
		  (when (eq gnus-summary-make-false-root 'dummy)
		    ;; We go to the dummy root by going to
		    ;; the first sub-"thread", and then one line up.
		    (gnus-summary-goto-article
		     (mail-header-number (caadr thread)))
		    (forward-line -1)
		    (gnus-delete-line)
		    (gnus-data-compute-positions))
		  (setq thread (cdr thread))
		  (while thread
		    (gnus-remove-thread-1 (car thread))
		    (setq thread (cdr thread))))
	      (gnus-remove-thread-1 thread))))))))

(defun gnus-remove-thread-1 (thread)
  "Remove the thread THREAD recursively."
  (let ((number (mail-header-number (pop thread)))
	d)
    (setq thread (reverse thread))
    (while thread
      (gnus-remove-thread-1 (pop thread)))
    (when (setq d (gnus-data-find number))
      (goto-char (gnus-data-pos d))
      (gnus-summary-show-thread)
      (gnus-data-remove
       number
       (- (point-at-bol)
	  (prog1
	      (1+ (point-at-eol))
	    (gnus-delete-line)))))))

(defun gnus-sort-threads-recursive (threads func)
  (sort (mapcar (lambda (thread)
		  (cons (car thread)
			(and (cdr thread)
			     (gnus-sort-threads-recursive (cdr thread) func))))
		threads) func))

(defun gnus-sort-threads-loop (threads func)
  (let* ((superthread (cons nil threads))
  	 (stack (list (cons superthread threads)))
  	 remaining-threads thread)
    (while stack
      (setq remaining-threads (cdr (car stack)))
      (if remaining-threads
  	  (progn (setq thread (car remaining-threads))
  		 (setcdr (car stack) (cdr remaining-threads))
  		 (if (cdr thread)
  		     (push (cons thread (cdr thread)) stack)))
  	(setq thread (caar stack))
  	(setcdr thread (sort (cdr thread) func))
  	(pop stack)))
    (cdr superthread)))

(defun gnus-sort-threads (threads)
  "Sort THREADS."
  (if (not gnus-thread-sort-functions)
      threads
    (gnus-message 8 "Sorting threads...")
    (prog1
	(condition-case nil
	    (let ((max-lisp-eval-depth (max max-lisp-eval-depth 5000)))
	      (gnus-sort-threads-recursive
	       threads (gnus-make-sort-function gnus-thread-sort-functions)))
	  ;; Even after binding max-lisp-eval-depth, the recursive
	  ;; sorter might fail for very long threads.  In that case,
	  ;; try using a (less well-tested) non-recursive sorter.
	  (error (gnus-message 9 "Sorting threads with loop...")
		 (gnus-sort-threads-loop
		  threads (gnus-make-sort-function
			   gnus-thread-sort-functions))))
      (gnus-message 8 "Sorting threads...done"))))

(defun gnus-sort-articles (articles)
  "Sort ARTICLES."
  (when gnus-article-sort-functions
    (gnus-message 7 "Sorting articles...")
    (prog1
	(setq gnus-newsgroup-headers
	      (sort articles (gnus-make-sort-function
			      gnus-article-sort-functions)))
      (gnus-message 7 "Sorting articles...done"))))

;; Written by Hallvard B Furuseth <h.b.furuseth@usit.uio.no>.
(defmacro gnus-thread-header (thread)
  "Return header of first article in THREAD.
Note that THREAD must never, ever be anything else than a variable -
using some other form will lead to serious barfage."
  (or (symbolp thread) (signal 'wrong-type-argument '(symbolp thread)))
  ;; (8% speedup to gnus-summary-prepare, just for fun :-)
  (list 'byte-code "\10\211:\203\17\0\211@;\203\16\0A@@\207"
	(vector thread) 2))

(defsubst gnus-article-sort-by-number (h1 h2)
  "Sort articles by article number."
  (< (mail-header-number h1)
     (mail-header-number h2)))

(defun gnus-thread-sort-by-number (h1 h2)
  "Sort threads by root article number."
  (gnus-article-sort-by-number
   (gnus-thread-header h1) (gnus-thread-header h2)))

(defsubst gnus-article-sort-by-random (h1 h2)
  "Sort articles randomly."
  (zerop (random 2)))

(defun gnus-thread-sort-by-random (h1 h2)
  "Sort threads randomly."
  (gnus-article-sort-by-random
   (gnus-thread-header h1) (gnus-thread-header h2)))

(defsubst gnus-article-sort-by-lines (h1 h2)
  "Sort articles by article Lines header."
  (< (mail-header-lines h1)
     (mail-header-lines h2)))

(defun gnus-thread-sort-by-lines (h1 h2)
  "Sort threads by root article Lines header."
  (gnus-article-sort-by-lines
   (gnus-thread-header h1) (gnus-thread-header h2)))

(defsubst gnus-article-sort-by-chars (h1 h2)
  "Sort articles by octet length."
  (< (mail-header-chars h1)
     (mail-header-chars h2)))

(defun gnus-thread-sort-by-chars (h1 h2)
  "Sort threads by root article octet length."
  (gnus-article-sort-by-chars
   (gnus-thread-header h1) (gnus-thread-header h2)))

(defsubst gnus-article-sort-by-author (h1 h2)
  "Sort articles by root author."
  (gnus-string<
   (let ((extract (funcall
		   gnus-extract-address-components
		   (mail-header-from h1))))
     (or (car extract) (cadr extract) ""))
   (let ((extract (funcall
		   gnus-extract-address-components
		   (mail-header-from h2))))
     (or (car extract) (cadr extract) ""))))

(defun gnus-thread-sort-by-author (h1 h2)
  "Sort threads by root author."
  (gnus-article-sort-by-author
   (gnus-thread-header h1)  (gnus-thread-header h2)))

(defsubst gnus-article-sort-by-recipient (h1 h2)
  "Sort articles by recipient."
  (gnus-string<
   (let ((extract (funcall
		   gnus-extract-address-components
		   (or (cdr (assq 'To (mail-header-extra h1))) ""))))
     (or (car extract) (cadr extract)))
   (let ((extract (funcall
		   gnus-extract-address-components
		   (or (cdr (assq 'To (mail-header-extra h2))) ""))))
     (or (car extract) (cadr extract)))))

(defun gnus-thread-sort-by-recipient (h1 h2)
  "Sort threads by root recipient."
  (gnus-article-sort-by-recipient
   (gnus-thread-header h1) (gnus-thread-header h2)))

(defsubst gnus-article-sort-by-subject (h1 h2)
  "Sort articles by root subject."
  (gnus-string<
   (downcase (gnus-simplify-subject-re (mail-header-subject h1)))
   (downcase (gnus-simplify-subject-re (mail-header-subject h2)))))

(defun gnus-thread-sort-by-subject (h1 h2)
  "Sort threads by root subject."
  (gnus-article-sort-by-subject
   (gnus-thread-header h1) (gnus-thread-header h2)))

(defsubst gnus-article-sort-by-date (h1 h2)
  "Sort articles by root article date."
  (time-less-p
   (gnus-date-get-time (mail-header-date h1))
   (gnus-date-get-time (mail-header-date h2))))

(defun gnus-thread-sort-by-date (h1 h2)
  "Sort threads by root article date."
  (gnus-article-sort-by-date
   (gnus-thread-header h1) (gnus-thread-header h2)))

(defsubst gnus-article-sort-by-score (h1 h2)
  "Sort articles by root article score.
Unscored articles will be counted as having a score of zero."
  (> (or (cdr (assq (mail-header-number h1)
		    gnus-newsgroup-scored))
	 gnus-summary-default-score 0)
     (or (cdr (assq (mail-header-number h2)
		    gnus-newsgroup-scored))
	 gnus-summary-default-score 0)))

(defun gnus-thread-sort-by-score (h1 h2)
  "Sort threads by root article score."
  (gnus-article-sort-by-score
   (gnus-thread-header h1) (gnus-thread-header h2)))

(defun gnus-thread-sort-by-total-score (h1 h2)
  "Sort threads by the sum of all scores in the thread.
Unscored articles will be counted as having a score of zero."
  (> (gnus-thread-total-score h1) (gnus-thread-total-score h2)))

(defun gnus-thread-total-score (thread)
  ;; This function find the total score of THREAD.
  (cond
   ((null thread)
    0)
   ((consp thread)
    (if (stringp (car thread))
	(apply gnus-thread-score-function 0
	       (mapcar 'gnus-thread-total-score-1 (cdr thread)))
      (gnus-thread-total-score-1 thread)))
   (t
    (gnus-thread-total-score-1 (list thread)))))

(defun gnus-article-sort-by-most-recent-number (h1 h2)
  "Sort articles by number."
  (gnus-article-sort-by-number h1 h2))

(defun gnus-thread-sort-by-most-recent-number (h1 h2)
  "Sort threads such that the thread with the most recently arrived article comes first."
  (> (gnus-thread-highest-number h1) (gnus-thread-highest-number h2)))

(defun gnus-thread-highest-number (thread)
  "Return the highest article number in THREAD."
  (apply 'max (mapcar (lambda (header)
			(mail-header-number header))
		      (message-flatten-list thread))))

(defun gnus-article-sort-by-most-recent-date (h1 h2)
  "Sort articles by number."
  (gnus-article-sort-by-date h1 h2))

(defun gnus-thread-sort-by-most-recent-date (h1 h2)
  "Sort threads such that the thread with the most recently dated article comes first."
  (> (gnus-thread-latest-date h1) (gnus-thread-latest-date h2)))

; Since this is called not only to sort the top-level threads, but
; also in recursive sorts to order the articles within a thread, each
; article will be processed many times.  Thus it speeds things up
; quite a bit to use gnus-date-get-time, which caches the time value.
(defun gnus-thread-latest-date (thread)
  "Return the highest article date in THREAD."
  (apply 'max
	 (mapcar (lambda (header) (gnus-float-time
				   (gnus-date-get-time
				    (mail-header-date header))))
		 (message-flatten-list thread))))

(defun gnus-thread-total-score-1 (root)
  ;; This function find the total score of the thread below ROOT.
  (setq root (car root))
  (apply gnus-thread-score-function
	 (or (append
	      (mapcar 'gnus-thread-total-score
		      (cdr (gnus-id-to-thread (mail-header-id root))))
	      (when (> (mail-header-number root) 0)
		(list (or (cdr (assq (mail-header-number root)
				     gnus-newsgroup-scored))
			  gnus-summary-default-score 0))))
	     (list gnus-summary-default-score)
	     '(0))))

;; Added by Per Abrahamsen <amanda@iesd.auc.dk>.
(defvar gnus-tmp-prev-subject nil)
(defvar gnus-tmp-false-parent nil)
(defvar gnus-tmp-root-expunged nil)
(defvar gnus-tmp-dummy-line nil)

(defun gnus-extra-header (type &optional header)
  "Return the extra header of TYPE."
  (or (cdr (assq type (mail-header-extra (or header gnus-tmp-header))))
      ""))

(defvar gnus-tmp-thread-tree-header-string "")

(defcustom gnus-sum-thread-tree-root "> "
  "With %B spec, used for the root of a thread.
If nil, use subject instead."
  :version "22.1"
  :type '(radio (const :format "%v  " nil) string)
  :group 'gnus-thread)

(defcustom gnus-sum-thread-tree-false-root "> "
  "With %B spec, used for a false root of a thread.
If nil, use subject instead."
  :version "22.1"
  :type '(radio (const :format "%v  " nil) string)
  :group 'gnus-thread)

(defcustom gnus-sum-thread-tree-single-indent ""
  "With %B spec, used for a thread with just one message.
If nil, use subject instead."
  :version "22.1"
  :type '(radio (const :format "%v  " nil) string)
  :group 'gnus-thread)

(defcustom gnus-sum-thread-tree-vertical "| "
  "With %B spec, used for drawing a vertical line."
  :version "22.1"
  :type 'string
  :group 'gnus-thread)

(defcustom gnus-sum-thread-tree-indent "  "
  "With %B spec, used for indenting."
  :version "22.1"
  :type 'string
  :group 'gnus-thread)

(defcustom gnus-sum-thread-tree-leaf-with-other "+-> "
  "With %B spec, used for a leaf with brothers."
  :version "22.1"
  :type 'string
  :group 'gnus-thread)

(defcustom gnus-sum-thread-tree-single-leaf "\\-> "
  "With %B spec, used for a leaf without brothers."
  :version "22.1"
  :type 'string
  :group 'gnus-thread)

(defcustom gnus-summary-display-while-building nil
  "If non-nil, show and update the summary buffer as it's being built.
If the value is t, update the buffer after every line is inserted.  If
the value is an integer (N), update the display every N lines."
  :version "22.1"
  :group 'gnus-thread
  :type '(choice (const :tag "off" nil)
		 number
		 (const :tag "frequently" t)))

(defun gnus-summary-prepare-threads (threads)
  "Prepare summary buffer from THREADS and indentation LEVEL.
THREADS is either a list of `(PARENT [(CHILD1 [(GRANDCHILD ...]...) ...])'
or a straight list of headers."
  (gnus-message 7 "Generating summary...")

  (setq gnus-newsgroup-threads threads)
  (beginning-of-line)

  (let ((gnus-tmp-level 0)
	(default-score (or gnus-summary-default-score 0))
	(gnus-visual-p (gnus-visual-p 'summary-highlight 'highlight))
	(building-line-count gnus-summary-display-while-building)
	(building-count (integerp gnus-summary-display-while-building))
	thread number subject stack state gnus-tmp-gathered beg-match
	new-roots gnus-tmp-new-adopts thread-end simp-subject
	gnus-tmp-header gnus-tmp-unread gnus-tmp-downloaded
	gnus-tmp-replied gnus-tmp-subject-or-nil
	gnus-tmp-dummy gnus-tmp-indentation gnus-tmp-lines gnus-tmp-score
	gnus-tmp-score-char gnus-tmp-from gnus-tmp-name
	gnus-tmp-number gnus-tmp-opening-bracket gnus-tmp-closing-bracket
	tree-stack)

    (setq gnus-tmp-prev-subject nil
          gnus-tmp-thread-tree-header-string "")

    (if (vectorp (car threads))
	;; If this is a straight (sic) list of headers, then a
	;; threaded summary display isn't required, so we just create
	;; an unthreaded one.
	(gnus-summary-prepare-unthreaded threads)

      ;; Do the threaded display.

      (if gnus-summary-display-while-building
	  (switch-to-buffer (buffer-name)))
      (while (or threads stack gnus-tmp-new-adopts new-roots)

	(if (and (= gnus-tmp-level 0)
		 (or (not stack)
		     (= (caar stack) 0))
		 (not gnus-tmp-false-parent)
		 (or gnus-tmp-new-adopts new-roots))
	    (if gnus-tmp-new-adopts
		(setq gnus-tmp-level (if gnus-tmp-root-expunged 0 1)
		      thread (list (car gnus-tmp-new-adopts))
		      gnus-tmp-header (caar thread)
		      gnus-tmp-new-adopts (cdr gnus-tmp-new-adopts))
	      (when new-roots
		(setq thread (list (car new-roots))
		      gnus-tmp-header (caar thread)
		      new-roots (cdr new-roots))))

	  (if threads
	      ;; If there are some threads, we do them before the
	      ;; threads on the stack.
	      (setq thread threads
		    gnus-tmp-header (caar thread))
	    ;; There were no current threads, so we pop something off
	    ;; the stack.
	    (setq state (car stack)
		  gnus-tmp-level (car state)
		  tree-stack (cadr state)
		  thread (caddr state)
		  stack (cdr stack)
		  gnus-tmp-header (caar thread))))

	(setq gnus-tmp-false-parent nil)
	(setq gnus-tmp-root-expunged nil)
	(setq thread-end nil)

	(if (stringp gnus-tmp-header)
	    ;; The header is a dummy root.
	    (cond
	     ((eq gnus-summary-make-false-root 'adopt)
	      ;; We let the first article adopt the rest.
	      (setq gnus-tmp-new-adopts (nconc gnus-tmp-new-adopts
					       (cddar thread)))
	      (setq gnus-tmp-gathered
		    (nconc (mapcar
			    (lambda (h) (mail-header-number (car h)))
			    (cddar thread))
			   gnus-tmp-gathered))
	      (setq thread (cons (list (caar thread)
				       (cadar thread))
				 (cdr thread)))
	      (setq gnus-tmp-level -1
		    gnus-tmp-false-parent t))
	     ((eq gnus-summary-make-false-root 'empty)
	      ;; We print adopted articles with empty subject fields.
	      (setq gnus-tmp-gathered
		    (nconc (mapcar
			    (lambda (h) (mail-header-number (car h)))
			    (cddar thread))
			   gnus-tmp-gathered))
	      (setq gnus-tmp-level -1))
	     ((eq gnus-summary-make-false-root 'dummy)
	      ;; We remember that we probably want to output a dummy
	      ;; root.
	      (setq gnus-tmp-dummy-line gnus-tmp-header)
	      (setq gnus-tmp-prev-subject gnus-tmp-header))
	     (t
	      ;; We do not make a root for the gathered
	      ;; sub-threads at all.
	      (setq gnus-tmp-level -1)))

	  (setq number (mail-header-number gnus-tmp-header)
		subject (mail-header-subject gnus-tmp-header)
		simp-subject (gnus-simplify-subject-fully subject))

	  (cond
	   ;; If the thread has changed subject, we might want to make
	   ;; this subthread into a root.
	   ((and (null gnus-thread-ignore-subject)
		 (not (zerop gnus-tmp-level))
		 gnus-tmp-prev-subject
		 (not (string= gnus-tmp-prev-subject simp-subject)))
	    (setq new-roots (nconc new-roots (list (car thread)))
		  thread-end t
		  gnus-tmp-header nil))
	   ;; If the article lies outside the current limit,
	   ;; then we do not display it.
	   ((not (memq number gnus-newsgroup-limit))
	    (setq gnus-tmp-gathered
		  (nconc (mapcar
			  (lambda (h) (mail-header-number (car h)))
			  (cdar thread))
			 gnus-tmp-gathered))
	    (setq gnus-tmp-new-adopts (if (cdar thread)
					  (append gnus-tmp-new-adopts
						  (cdar thread))
					gnus-tmp-new-adopts)
		  thread-end t
		  gnus-tmp-header nil)
	    (when (zerop gnus-tmp-level)
	      (setq gnus-tmp-root-expunged t)))
	   ;; Perhaps this article is to be marked as read?
	   ((and gnus-summary-mark-below
		 (< (or (cdr (assq number gnus-newsgroup-scored))
			default-score)
		    gnus-summary-mark-below)
		 ;; Don't touch sparse articles.
		 (not (gnus-summary-article-sparse-p number))
		 (not (gnus-summary-article-ancient-p number)))
	    (setq gnus-newsgroup-unreads
		  (delq number gnus-newsgroup-unreads))
	    (if gnus-newsgroup-auto-expire
		(setq gnus-newsgroup-expirable
		      (gnus-add-to-sorted-list
		       gnus-newsgroup-expirable number))
	      (push (cons number gnus-low-score-mark)
		    gnus-newsgroup-reads))))

	  (when gnus-tmp-header
	    ;; We may have an old dummy line to output before this
	    ;; article.
	    (when (and gnus-tmp-dummy-line
		       (gnus-subject-equal
			gnus-tmp-dummy-line
			(mail-header-subject gnus-tmp-header)))
	      (gnus-summary-insert-dummy-line
	       gnus-tmp-dummy-line (mail-header-number gnus-tmp-header))
	      (setq gnus-tmp-dummy-line nil))

	    ;; Compute the mark.
	    (setq gnus-tmp-unread (gnus-article-mark number))

	    (push (gnus-data-make number gnus-tmp-unread (1+ (point))
				  gnus-tmp-header gnus-tmp-level)
		  gnus-newsgroup-data)

	    ;; Actually insert the line.
	    (setq
	     gnus-tmp-subject-or-nil
	     (cond
	      ((and gnus-thread-ignore-subject
		    gnus-tmp-prev-subject
		    (not (string= gnus-tmp-prev-subject simp-subject)))
	       subject)
	      ((zerop gnus-tmp-level)
	       (if (and (eq gnus-summary-make-false-root 'empty)
			(memq number gnus-tmp-gathered)
			gnus-tmp-prev-subject
			(string= gnus-tmp-prev-subject simp-subject))
		   gnus-summary-same-subject
		 subject))
	      (t gnus-summary-same-subject)))
	    (if (and (eq gnus-summary-make-false-root 'adopt)
		     (= gnus-tmp-level 1)
		     (memq number gnus-tmp-gathered))
		(setq gnus-tmp-opening-bracket ?\<
		      gnus-tmp-closing-bracket ?\>)
	      (setq gnus-tmp-opening-bracket ?\[
		    gnus-tmp-closing-bracket ?\]))
	    (if (>= gnus-tmp-level (length gnus-thread-indent-array))
		(gnus-make-thread-indent-array
		 (max (* 2 (length gnus-thread-indent-array))
		      gnus-tmp-level)))
	    (setq
	     gnus-tmp-indentation
	     (aref gnus-thread-indent-array gnus-tmp-level)
	     gnus-tmp-lines (mail-header-lines gnus-tmp-header)
	     gnus-tmp-score (or (cdr (assq number gnus-newsgroup-scored))
				gnus-summary-default-score 0)
	     gnus-tmp-score-char
	     (if (or (null gnus-summary-default-score)
		     (<= (abs (- gnus-tmp-score gnus-summary-default-score))
			 gnus-summary-zcore-fuzz))
		 ?                      ;Whitespace
	       (if (< gnus-tmp-score gnus-summary-default-score)
		   gnus-score-below-mark gnus-score-over-mark))
	     gnus-tmp-replied
	     (cond ((memq number gnus-newsgroup-processable)
		    gnus-process-mark)
		   ((memq number gnus-newsgroup-cached)
		    gnus-cached-mark)
		   ((memq number gnus-newsgroup-replied)
		    gnus-replied-mark)
		   ((memq number gnus-newsgroup-forwarded)
		    gnus-forwarded-mark)
		   ((memq number gnus-newsgroup-saved)
		    gnus-saved-mark)
		   ((memq number gnus-newsgroup-unseen)
		    gnus-unseen-mark)
		   (t gnus-no-mark))
	     gnus-tmp-downloaded
             (cond ((memq number gnus-newsgroup-undownloaded)
                    gnus-undownloaded-mark)
                   (gnus-newsgroup-agentized
                    gnus-downloaded-mark)
                   (t
                    gnus-no-mark))
	     gnus-tmp-from (mail-header-from gnus-tmp-header)
	     gnus-tmp-name
	     (cond
	      ((string-match "<[^>]+> *$" gnus-tmp-from)
	       (setq beg-match (match-beginning 0))
	       (or (and (string-match "^\".+\"" gnus-tmp-from)
			(substring gnus-tmp-from 1 (1- (match-end 0))))
		   (substring gnus-tmp-from 0 beg-match)))
	      ((string-match "(.+)" gnus-tmp-from)
	       (substring gnus-tmp-from
			  (1+ (match-beginning 0)) (1- (match-end 0))))
	      (t gnus-tmp-from))

	     ;; Do the %B string
	     gnus-tmp-thread-tree-header-string
	     (cond
	      ((not gnus-show-threads) "")
	      ((zerop gnus-tmp-level)
	       (cond ((cdar thread)
		      (or gnus-sum-thread-tree-root subject))
		     (gnus-tmp-new-adopts
		      (or gnus-sum-thread-tree-false-root subject))
		     (t
		      (or gnus-sum-thread-tree-single-indent subject))))
	      (t
	       (concat (apply 'concat
			      (mapcar (lambda (item)
					(if (= item 1)
					    gnus-sum-thread-tree-vertical
					  gnus-sum-thread-tree-indent))
				      (cdr (reverse tree-stack))))
		       (if (nth 1 thread)
			   gnus-sum-thread-tree-leaf-with-other
			 gnus-sum-thread-tree-single-leaf)))))
	    (when (string= gnus-tmp-name "")
	      (setq gnus-tmp-name gnus-tmp-from))
	    (unless (numberp gnus-tmp-lines)
	      (setq gnus-tmp-lines -1))
	    (if (= gnus-tmp-lines -1)
		(setq gnus-tmp-lines "?")
	      (setq gnus-tmp-lines (number-to-string gnus-tmp-lines)))
	    (gnus-put-text-property
	     (point)
	     (progn (eval gnus-summary-line-format-spec) (point))
	     'gnus-number number)
	    (when gnus-visual-p
	      (forward-line -1)
	      (gnus-summary-highlight-line)
	      (when gnus-summary-update-hook
		(gnus-run-hooks 'gnus-summary-update-hook))
	      (forward-line 1))

	    (setq gnus-tmp-prev-subject simp-subject)))

	(when (nth 1 thread)
	  (push (list (max 0 gnus-tmp-level)
		      (copy-sequence tree-stack)
		      (nthcdr 1 thread))
		stack))
	(push (if (nth 1 thread) 1 0) tree-stack)
	(incf gnus-tmp-level)
	(setq threads (if thread-end nil (cdar thread)))
	(if gnus-summary-display-while-building
	    (if building-count
		(progn
		  ;; use a set frequency
		  (setq building-line-count (1- building-line-count))
		  (when (= building-line-count 0)
		    (sit-for 0)
		    (setq building-line-count
			  gnus-summary-display-while-building)))
	      ;; always
	      (sit-for 0)))
	(unless threads
	  (setq gnus-tmp-level 0)))))
  (gnus-message 7 "Generating summary...done"))

(defun gnus-summary-prepare-unthreaded (headers)
  "Generate an unthreaded summary buffer based on HEADERS."
  (let (header number mark)

    (beginning-of-line)

    (while headers
      ;; We may have to root out some bad articles...
      (when (memq (setq number (mail-header-number
				(setq header (pop headers))))
		  gnus-newsgroup-limit)
	;; Mark article as read when it has a low score.
	(when (and gnus-summary-mark-below
		   (< (or (cdr (assq number gnus-newsgroup-scored))
			  gnus-summary-default-score 0)
		      gnus-summary-mark-below)
		   (not (gnus-summary-article-ancient-p number)))
	  (setq gnus-newsgroup-unreads
		(delq number gnus-newsgroup-unreads))
	  (if gnus-newsgroup-auto-expire
	      (push number gnus-newsgroup-expirable)
	    (push (cons number gnus-low-score-mark)
		  gnus-newsgroup-reads)))

	(setq mark (gnus-article-mark number))
	(push (gnus-data-make number mark (1+ (point)) header 0)
	      gnus-newsgroup-data)
	(gnus-summary-insert-line
	 header 0 number
	 (memq number gnus-newsgroup-undownloaded)
	 mark (memq number gnus-newsgroup-replied)
	 (memq number gnus-newsgroup-expirable)
	 (mail-header-subject header) nil
	 (cdr (assq number gnus-newsgroup-scored))
	 (memq number gnus-newsgroup-processable))))))

(defun gnus-group-get-list-identifiers (group)
  "Get list identifier regexp for GROUP."
  (or (gnus-parameter-list-identifier group)
      (if (consp gnus-list-identifiers)
          (mapconcat 'identity gnus-list-identifiers " *\\|")
        gnus-list-identifiers)))

(defun gnus-summary-remove-list-identifiers ()
  "Remove list identifiers in `gnus-list-identifiers' from articles in the current group."
  (let ((regexp (gnus-group-get-list-identifiers gnus-newsgroup-name))
        changed subject)
    (when regexp
      (setq regexp (concat "^\\(?:R[Ee]: +\\)*\\(" regexp " *\\)"))
      (dolist (header gnus-newsgroup-headers)
	(setq subject (mail-header-subject header)
	      changed nil)
	(while (string-match regexp subject)
	  (setq subject
		(concat (substring subject 0 (match-beginning 1))
			(substring subject (match-end 0)))
		changed t))
	(when changed
	  (when (string-match "^\\(\\(?:R[Ee]: +\\)+\\)R[Ee]: +" subject)
	    (setq subject
		  (concat (substring subject 0 (match-beginning 1))
			  (substring subject (match-end 1)))))
	  (mail-header-set-subject header subject))))))

(defun gnus-fetch-headers (articles &optional limit force-new dependencies)
  "Fetch headers of ARTICLES."
  (let ((name (gnus-group-decoded-name gnus-newsgroup-name)))
    (gnus-message 7 "Fetching headers for %s..." name)
    (prog1
	(if (eq 'nov
		(setq gnus-headers-retrieved-by
		      (gnus-retrieve-headers
		       articles gnus-newsgroup-name
		       (or limit
			   ;; We might want to fetch old headers, but
			   ;; not if there is only 1 article.
			   (and (or (and
				     (not (eq gnus-fetch-old-headers 'some))
				     (not (numberp gnus-fetch-old-headers)))
				    (> (length articles) 1))
				gnus-fetch-old-headers)))))
	    (gnus-get-newsgroup-headers-xover
	     articles force-new dependencies gnus-newsgroup-name t)
	  (gnus-get-newsgroup-headers dependencies force-new))
      (gnus-message 7 "Fetching headers for %s...done" name))))

(defun gnus-select-newsgroup (group &optional read-all select-articles)
  "Select newsgroup GROUP.
If READ-ALL is non-nil, all articles in the group are selected.
If SELECT-ARTICLES, only select those articles from GROUP."
  (let* ((entry (gnus-group-entry group))
	 ;;!!! Dirty hack; should be removed.
	 (gnus-summary-ignore-duplicates
	  (if (eq (car (gnus-find-method-for-group group)) 'nnvirtual)
	      t
	    gnus-summary-ignore-duplicates))
	 (info (nth 2 entry))
	 charset articles fetched-articles cached)

    (unless (gnus-check-server
	     (set (make-local-variable 'gnus-current-select-method)
		  (gnus-find-method-for-group group)))
      (error "Couldn't open server"))
    (setq charset (gnus-group-name-charset gnus-current-select-method group))

    (or (and entry (not (eq (car entry) t))) ; Either it's active...
	(gnus-activate-group group)	; Or we can activate it...
	(progn				; Or we bug out.
	  (when (equal major-mode 'gnus-summary-mode)
	    (gnus-kill-buffer (current-buffer)))
	  (error
	   "Couldn't activate group %s: %s"
	   (mm-decode-coding-string group charset)
	   (mm-decode-coding-string (gnus-status-message group) charset))))

    (unless (gnus-request-group group t)
      (when (equal major-mode 'gnus-summary-mode)
	(gnus-kill-buffer (current-buffer)))
      (error "Couldn't request group %s: %s"
	     (mm-decode-coding-string group charset)
	     (mm-decode-coding-string (gnus-status-message group) charset)))

    (when (and gnus-agent
	       (gnus-active group))
      (gnus-agent-possibly-alter-active group (gnus-active group) info)

      (setq gnus-summary-use-undownloaded-faces
	    (gnus-agent-find-parameter
	     group
	     'agent-enable-undownloaded-faces)))

    (setq gnus-newsgroup-name group
	  gnus-newsgroup-unselected nil
	  gnus-newsgroup-unreads (gnus-list-of-unread-articles group))

    (let ((display (gnus-group-find-parameter group 'display)))
      (setq gnus-newsgroup-display
	    (cond
	     ((not (zerop (or (car-safe read-all) 0)))
	      ;; The user entered the group with C-u SPC/RET, let's show
	      ;; all articles.
	      'gnus-not-ignore)
	     ((eq display 'all)
	      'gnus-not-ignore)
	     ((arrayp display)
	      (gnus-summary-display-make-predicate (mapcar 'identity display)))
	     ((numberp display)
	      ;; The following is probably the "correct" solution, but
	      ;; it makes Gnus fetch all headers and then limit the
	      ;; articles (which is slow), so instead we hack the
	      ;; select-articles parameter instead. -- Simon Josefsson
	      ;; <jas@kth.se>
	      ;;
	      ;; (gnus-byte-compile
	      ;;  `(lambda () (> number ,(- (cdr (gnus-active group))
	      ;; 			 display)))))
	      (setq select-articles
		    (gnus-uncompress-range
		     (cons (let ((tmp (- (cdr (gnus-active group)) display)))
			     (if (> tmp 0)
				 tmp
			       1))
			   (cdr (gnus-active group)))))
	      nil)
	     (t
	      nil))))

    (gnus-summary-setup-default-charset)

    ;; Kludge to avoid having cached articles nixed out in virtual groups.
    (when (gnus-virtual-group-p group)
      (setq cached gnus-newsgroup-cached))

    (setq gnus-newsgroup-unreads
	  (gnus-sorted-ndifference
	   (gnus-sorted-ndifference gnus-newsgroup-unreads
				    gnus-newsgroup-marked)
	   gnus-newsgroup-dormant))

    (setq gnus-newsgroup-processable nil)

    (gnus-update-read-articles group gnus-newsgroup-unreads t)

    ;; Adjust and set lists of article marks.
    (when info
      (gnus-adjust-marked-articles info))
    (if (setq articles select-articles)
	(setq gnus-newsgroup-unselected
	      (gnus-sorted-difference gnus-newsgroup-unreads articles))
      (setq articles (gnus-articles-to-read group read-all)))

    (cond
     ((null articles)
      ;;(gnus-message 3 "Couldn't select newsgroup -- no articles to display")
      'quit)
     ((eq articles 0) nil)
     (t
      ;; Init the dependencies hash table.
      (setq gnus-newsgroup-dependencies
	    (gnus-make-hashtable (length articles)))
      (gnus-set-global-variables)
      ;; Retrieve the headers and read them in.

      (setq gnus-newsgroup-headers (gnus-fetch-headers articles))

      ;; Kludge to avoid having cached articles nixed out in virtual groups.
      (when cached
	(setq gnus-newsgroup-cached cached))

      ;; Suppress duplicates?
      (when gnus-suppress-duplicates
	(gnus-dup-suppress-articles))

      ;; Set the initial limit.
      (setq gnus-newsgroup-limit (copy-sequence articles))
      ;; Remove canceled articles from the list of unread articles.
      (setq fetched-articles
	    (mapcar (lambda (headers) (mail-header-number headers))
		    gnus-newsgroup-headers))
      (setq gnus-newsgroup-articles fetched-articles)
      (setq gnus-newsgroup-unreads
	    (gnus-sorted-nintersection
	     gnus-newsgroup-unreads fetched-articles))
      (gnus-compute-unseen-list)

      ;; Removed marked articles that do not exist.
      (gnus-update-missing-marks
       (gnus-sorted-difference articles fetched-articles))
      ;; We might want to build some more threads first.
      (when (and gnus-fetch-old-headers
		 (eq gnus-headers-retrieved-by 'nov))
	(if (eq gnus-fetch-old-headers 'invisible)
	    (gnus-build-all-threads)
	  (gnus-build-old-threads)))
      ;; Let the Gnus agent mark articles as read.
      (when gnus-agent
	(gnus-agent-get-undownloaded-list))
      ;; Remove list identifiers from subject
      (gnus-summary-remove-list-identifiers)
      ;; Check whether auto-expire is to be done in this group.
      (setq gnus-newsgroup-auto-expire
	    (and (gnus-group-auto-expirable-p group)
		 (not (gnus-group-read-only-p group))))
      ;; Set up the article buffer now, if necessary.
      (unless (and gnus-single-article-buffer
		   (equal gnus-article-buffer "*Article*"))
	(gnus-article-setup-buffer))
      ;; First and last article in this newsgroup.
      (when gnus-newsgroup-headers
	(setq gnus-newsgroup-begin
	      (mail-header-number (car gnus-newsgroup-headers))
	      gnus-newsgroup-end
	      (mail-header-number
	       (gnus-last-element gnus-newsgroup-headers))))
      ;; GROUP is successfully selected.
      (or gnus-newsgroup-headers t)))))

(defun gnus-compute-unseen-list ()
  ;; The `seen' marks are treated specially.
  (if (not gnus-newsgroup-seen)
      (setq gnus-newsgroup-unseen gnus-newsgroup-articles)
    (setq gnus-newsgroup-unseen
	  (gnus-inverse-list-range-intersection
	   gnus-newsgroup-articles gnus-newsgroup-seen))))

(declare-function gnus-get-predicate "gnus-agent" (predicate))

(defun gnus-summary-display-make-predicate (display)
  (require 'gnus-agent)
  (when (= (length display) 1)
    (setq display (car display)))
  (unless gnus-summary-display-cache
    (dolist (elem (append '((unread . unread)
			    (read . read)
			    (unseen . unseen))
			  gnus-article-mark-lists))
      (push (cons (cdr elem)
		  (gnus-byte-compile    ;Why bother?
		   `(lambda () (gnus-article-marked-p ',(cdr elem)))))
	    gnus-summary-display-cache)))
  (let ((gnus-category-predicate-alist gnus-summary-display-cache)
	(gnus-category-predicate-cache gnus-summary-display-cache))
    (gnus-get-predicate display)))

;; Uses the dynamically bound `gnus-number' variable.
(defvar gnus-number)
(defun gnus-article-marked-p (type &optional article)
  (let ((article (or article gnus-number)))
    (cond
     ((eq type 'tick)
      (memq article gnus-newsgroup-marked))
     ((eq type 'spam)
      (memq article gnus-newsgroup-spam-marked))
     ((eq type 'unsend)
      (memq article gnus-newsgroup-unsendable))
     ((eq type 'undownload)
      (memq article gnus-newsgroup-undownloaded))
     ((eq type 'download)
      (memq article gnus-newsgroup-downloadable))
     ((eq type 'unread)
      (memq article gnus-newsgroup-unreads))
     ((eq type 'read)
      (memq article gnus-newsgroup-reads))
     ((eq type 'dormant)
      (memq article gnus-newsgroup-dormant) )
     ((eq type 'expire)
      (memq article gnus-newsgroup-expirable))
     ((eq type 'reply)
      (memq article gnus-newsgroup-replied))
     ((eq type 'killed)
      (memq article gnus-newsgroup-killed))
     ((eq type 'bookmark)
      (assq article gnus-newsgroup-bookmarks))
     ((eq type 'score)
      (assq article gnus-newsgroup-scored))
     ((eq type 'save)
      (memq article gnus-newsgroup-saved))
     ((eq type 'cache)
      (memq article gnus-newsgroup-cached))
     ((eq type 'forward)
      (memq article gnus-newsgroup-forwarded))
     ((eq type 'seen)
      (not (memq article gnus-newsgroup-unseen)))
     (t t))))

(defun gnus-articles-to-read (group &optional read-all)
  "Find out what articles the user wants to read."
  (let* ((only-read-p t)
	 (articles
	  ;; Select all articles if `read-all' is non-nil, or if there
	  ;; are no unread articles.
	  (if (or read-all
		  (and (zerop (length gnus-newsgroup-marked))
		       (zerop (length gnus-newsgroup-unreads)))
		  ;; Fetch all if the predicate is non-nil.
		  gnus-newsgroup-display)
	      ;; We want to select the headers for all the articles in
	      ;; the group, so we select either all the active
	      ;; articles in the group, or (if that's nil), the
	      ;; articles in the cache.
	      (or
	       (if gnus-newsgroup-maximum-articles
		   (let ((active (gnus-active group)))
		     (gnus-uncompress-range
		      (cons (max (car active)
				 (- (cdr active)
				    gnus-newsgroup-maximum-articles
				    -1))
			    (cdr active))))
		 (gnus-uncompress-range (gnus-active group)))
	       (gnus-cache-articles-in-group group))
	    ;; Select only the "normal" subset of articles.
	    (setq only-read-p nil)
	    (gnus-sorted-nunion
	     (gnus-sorted-union gnus-newsgroup-dormant gnus-newsgroup-marked)
	     gnus-newsgroup-unreads)))
	 (scored-list (gnus-killed-articles gnus-newsgroup-killed articles))
	 (scored (length scored-list))
	 (number (length articles))
	 (marked (+ (length gnus-newsgroup-marked)
		    (length gnus-newsgroup-dormant)))
	 (select
	  (cond
	   ((numberp read-all)
	    read-all)
	   ((numberp gnus-newsgroup-display)
	    gnus-newsgroup-display)
	   (t
	    (condition-case ()
		(cond
		 ((and (or (<= scored marked) (= scored number))
		       (numberp gnus-large-newsgroup)
		       (> number gnus-large-newsgroup))
		  (let* ((cursor-in-echo-area nil)
			 (initial (gnus-parameter-large-newsgroup-initial
				   gnus-newsgroup-name))
			 (default (if only-read-p
				      (or initial gnus-large-newsgroup)
				    number))
			 (input
			  (read-string
			   (if only-read-p
			       (format
				"How many articles from %s (available %d, default %d): "
				(gnus-group-decoded-name
				 (gnus-group-real-name gnus-newsgroup-name))
				number default)
			     (format
			      "How many articles from %s (%d default): "
			      (gnus-group-decoded-name
			       (gnus-group-real-name gnus-newsgroup-name))
			      default))
			   nil
			   nil
			   (number-to-string default))))
		    (if (string-match "^[ \t]*$" input) number input)))
		 ((and (> scored marked) (< scored number)
		       (> (- scored number) 20))
		  (let ((input
			 (read-string
			  (format "%s %s (%d scored, %d total): "
				  "How many articles from"
				  (gnus-group-decoded-name
				   (gnus-group-real-name gnus-newsgroup-name))
				  scored number))))
		    (if (string-match "^[ \t]*$" input)
			number input)))
		 (t number))
	      (quit
	       (message "Quit getting the articles to read")
	       nil))))))
    (setq select (if (stringp select) (string-to-number select) select))
    (if (or (null select) (zerop select))
	select
      (if (and (not (zerop scored)) (<= (abs select) scored))
	  (progn
	    (setq articles (sort scored-list '<))
	    (setq number (length articles)))
	(setq articles (copy-sequence articles)))

      (when (< (abs select) number)
	(if (< select 0)
	    ;; Select the N oldest articles.
	    (setcdr (nthcdr (1- (abs select)) articles) nil)
	  ;; Select the N most recent articles.
	  (setq articles (nthcdr (- number select) articles))))
      (setq gnus-newsgroup-unselected
	    (gnus-sorted-difference gnus-newsgroup-unreads articles))
      (when gnus-alter-articles-to-read-function
	(setq articles
	      (sort
	       (funcall gnus-alter-articles-to-read-function
			gnus-newsgroup-name articles)
	       '<)))
      articles)))

(defun gnus-killed-articles (killed articles)
  (let (out)
    (while articles
      (when (inline (gnus-member-of-range (car articles) killed))
	(push (car articles) out))
      (setq articles (cdr articles)))
    out))

(defun gnus-uncompress-marks (marks)
  "Uncompress the mark ranges in MARKS."
  (let ((uncompressed '(score bookmark))
	out)
    (while marks
      (if (memq (caar marks) uncompressed)
	  (push (car marks) out)
	(push (cons (caar marks) (gnus-uncompress-range (cdar marks))) out))
      (setq marks (cdr marks)))
    out))

(defun gnus-article-mark-to-type (mark)
  "Return the type of MARK."
  (or (cadr (assq mark gnus-article-special-mark-lists))
      'list))

(defun gnus-article-unpropagatable-p (mark)
  "Return whether MARK should be propagated to back end."
  (memq mark gnus-article-unpropagated-mark-lists))

(defun gnus-adjust-marked-articles (info)
  "Set all article lists and remove all marks that are no longer valid."
  (let* ((marked-lists (gnus-info-marks info))
	 (active (gnus-active (gnus-info-group info)))
	 (min (car active))
	 (max (cdr active))
	 (types gnus-article-mark-lists)
	 marks var articles article mark mark-type
         bgn end)
    ;; Hack to avoid adjusting marks for imap.
    (when (eq (car (gnus-find-method-for-group (gnus-info-group info)))
	      'nnimap)
      (setq min 1))

    (dolist (marks marked-lists)
      (setq mark (car marks)
	    mark-type (gnus-article-mark-to-type mark)
	    var (intern (format "gnus-newsgroup-%s" (car (rassq mark types)))))

      ;; We set the variable according to the type of the marks list,
      ;; and then adjust the marks to a subset of the active articles.
      (cond
       ;; Adjust "simple" lists - compressed yet unsorted
       ((eq mark-type 'list)
        ;; Simultaneously uncompress and clip to active range
        ;; See gnus-uncompress-range for a description of possible marks
        (let (l lh)
          (if (not (cadr marks))
              (set var nil)
            (setq articles (if (numberp (cddr marks))
                               (list (cdr marks))
                             (cdr marks))
                  lh (cons nil nil)
                  l lh)

            (while (setq article (pop articles))
              (cond ((consp article)
                     (setq bgn (max (car article) min)
                           end (min (cdr article) max))
                     (while (<= bgn end)
                       (setq l (setcdr l (cons bgn nil))
                             bgn (1+ bgn))))
                    ((and (<= min article)
                          (>= max article))
                     (setq l (setcdr l (cons article nil))))))
            (set var (cdr lh)))))
       ;; Adjust assocs.
       ((eq mark-type 'tuple)
	(set var (setq articles (cdr marks)))
	(when (not (listp (cdr (symbol-value var))))
	  (set var (list (symbol-value var))))
	(when (not (listp (cdr articles)))
	  (setq articles (list articles)))
	(while articles
	  (when (or (not (consp (setq article (pop articles))))
		    (< (car article) min)
		    (> (car article) max))
	    (set var (delq article (symbol-value var))))))
       ;; Adjust ranges (sloppily).
       ((eq mark-type 'range)
	(cond
	 ((eq mark 'seen)
	  ;; Fix the record for `seen' if it looks like (seen NUM1 . NUM2).
	  ;; It should be (seen (NUM1 . NUM2)).
	  (when (numberp (cddr marks))
	    (setcdr marks (list (cdr marks))))
	  (setq articles (cdr marks))
	  (while (and articles
		      (or (and (consp (car articles))
			       (> min (cdar articles)))
			  (and (numberp (car articles))
			       (> min (car articles)))))
	    (pop articles))
	  (set var articles))))))))

(defun gnus-update-missing-marks (missing)
  "Go through the list of MISSING articles and remove them from the mark lists."
  (when missing
    (let (var m)
      ;; Go through all types.
      (dolist (elem gnus-article-mark-lists)
	(when (eq (gnus-article-mark-to-type (cdr elem)) 'list)
	  (setq var (intern (format "gnus-newsgroup-%s" (car elem))))
	  (when (symbol-value var)
	    ;; This list has articles.  So we delete all missing
	    ;; articles from it.
	    (setq m missing)
	    (while m
	      (set var (delq (pop m) (symbol-value var))))))))))

(defun gnus-update-marks ()
  "Enter the various lists of marked articles into the newsgroup info list."
  (let ((types gnus-article-mark-lists)
	(info (gnus-get-info gnus-newsgroup-name))
	type list newmarked symbol delta-marks)
    (when info
      ;; Add all marks lists to the list of marks lists.
      (while (setq type (pop types))
	(setq list (symbol-value
		    (setq symbol
			  (intern (format "gnus-newsgroup-%s" (car type))))))

	(when list
	  ;; Get rid of the entries of the articles that have the
	  ;; default score.
	  (when (and (eq (cdr type) 'score)
		     gnus-save-score
		     list)
	    (let* ((arts list)
		   (prev (cons nil list))
		   (all prev))
	      (while arts
		(if (or (not (consp (car arts)))
			(= (cdar arts) gnus-summary-default-score))
		    (setcdr prev (cdr arts))
		  (setq prev arts))
		(setq arts (cdr arts)))
	      (setq list (cdr all)))))

	(when (eq (cdr type) 'seen)
	  (setq list (gnus-range-add list gnus-newsgroup-unseen)))

	(when (eq (gnus-article-mark-to-type (cdr type)) 'list)
	  (setq list (gnus-compress-sequence (set symbol (sort list '<)) t)))

	(when (and (gnus-check-backend-function
		    'request-set-mark gnus-newsgroup-name)
		   (or gnus-propagate-marks
		       (gnus-method-option-p
			(gnus-find-method-for-group gnus-newsgroup-name)
			'server-marks))
		   (not (gnus-article-unpropagatable-p (cdr type))))
	  (let* ((old (cdr (assq (cdr type) (gnus-info-marks info))))
		 ;; Don't do anything about marks for articles we
		 ;; didn't actually get any headers for.
		 (del
		  (gnus-list-range-intersection
		   gnus-newsgroup-articles
		   (gnus-remove-from-range (gnus-copy-sequence old) list)))
		 (add
		  (gnus-list-range-intersection
		   gnus-newsgroup-articles
		   (gnus-remove-from-range
		    (gnus-copy-sequence list) old))))
	    (when add
	      (push (list add 'add (list (cdr type))) delta-marks))
	    (when del
	      ;; Don't delete marks from outside the active range.
	      ;; This shouldn't happen, but is a sanity check.
	      (setq del (gnus-sorted-range-intersection
			 (gnus-active gnus-newsgroup-name) del))
	      (push (list del 'del (list (cdr type))) delta-marks))))

	(when list
	  (push (cons (cdr type) list) newmarked)))

      (when delta-marks
	(unless (gnus-check-group gnus-newsgroup-name)
	  (error "Can't open server for %s" gnus-newsgroup-name))
	(gnus-request-set-mark gnus-newsgroup-name delta-marks))

      ;; Enter these new marks into the info of the group.
      (if (nthcdr 3 info)
	  (setcar (nthcdr 3 info) newmarked)
	;; Add the marks lists to the end of the info.
	(when newmarked
	  (setcdr (nthcdr 2 info) (list newmarked))))

      ;; Cut off the end of the info if there's nothing else there.
      (let ((i 5))
	(while (and (> i 2)
		    (not (nth i info)))
	  (when (nthcdr (decf i) info)
	    (setcdr (nthcdr i info) nil)))))))

(defun gnus-set-mode-line (where)
  "Set the mode line of the article or summary buffers.
If WHERE is `summary', the summary mode line format will be used."
  ;; Is this mode line one we keep updated?
  (when (and (memq where gnus-updated-mode-lines)
	     (symbol-value
	      (intern (format "gnus-%s-mode-line-format-spec" where))))
    (let (mode-string)
      ;; We evaluate this in the summary buffer since these
      ;; variables are buffer-local to that buffer.
      (with-current-buffer gnus-summary-buffer
        ;; We bind all these variables that are used in the `eval' form
	;; below.
	(let* ((mformat (symbol-value
			 (intern
			  (format "gnus-%s-mode-line-format-spec" where))))
	       (gnus-tmp-group-name (gnus-mode-string-quote
				     (gnus-group-decoded-name
				      gnus-newsgroup-name)))
	       (gnus-tmp-article-number (or gnus-current-article 0))
	       (gnus-tmp-unread gnus-newsgroup-unreads)
	       (gnus-tmp-unread-and-unticked (length gnus-newsgroup-unreads))
	       (gnus-tmp-unselected (length gnus-newsgroup-unselected))
	       (gnus-tmp-unread-and-unselected
		(cond ((and (zerop gnus-tmp-unread-and-unticked)
			    (zerop gnus-tmp-unselected))
		       "")
		      ((zerop gnus-tmp-unselected)
		       (format "{%d more}" gnus-tmp-unread-and-unticked))
		      (t (format "{%d(+%d) more}"
				 gnus-tmp-unread-and-unticked
				 gnus-tmp-unselected))))
	       (gnus-tmp-subject
		(if (and gnus-current-headers
			 (vectorp gnus-current-headers))
		    (gnus-mode-string-quote
		     (mail-header-subject gnus-current-headers))
		  ""))
	       bufname-length max-len
	       gnus-tmp-header)	;; passed as argument to any user-format-funcs
	  (setq mode-string (eval mformat))
	  (setq bufname-length (if (string-match "%b" mode-string)
				   (- (length
				       (buffer-name
					(if (eq where 'summary)
					    nil
					  (get-buffer gnus-article-buffer))))
				      2)
				 0))
	  (setq max-len (max 4 (if gnus-mode-non-string-length
				   (- (window-width)
				      gnus-mode-non-string-length
				      bufname-length)
				 (length mode-string))))
	  ;; We might have to chop a bit of the string off...
	  (when (> (length mode-string) max-len)
	    (setq mode-string
		  (concat (truncate-string-to-width mode-string (- max-len 3))
			  "...")))))
      ;; Update the mode line.
      (setq mode-line-buffer-identification
	    (gnus-mode-line-buffer-identification (list mode-string)))
      (set-buffer-modified-p t))))

(defun gnus-create-xref-hashtb (from-newsgroup headers unreads)
  "Go through the HEADERS list and add all Xrefs to a hash table.
The resulting hash table is returned, or nil if no Xrefs were found."
  (let* ((virtual (gnus-virtual-group-p from-newsgroup))
	 (prefix (if virtual "" (gnus-group-real-prefix from-newsgroup)))
	 (xref-hashtb (gnus-make-hashtable))
	 start group entry number xrefs header)
    (while headers
      (setq header (pop headers))
      (when (and (setq xrefs (mail-header-xref header))
		 (not (memq (setq number (mail-header-number header))
			    unreads)))
	(setq start 0)
	(while (string-match "\\([^ ]+\\)[:/]\\([0-9]+\\)" xrefs start)
	  (setq start (match-end 0))
	  (setq group (if prefix
			  (concat prefix (substring xrefs (match-beginning 1)
						    (match-end 1)))
			(substring xrefs (match-beginning 1) (match-end 1))))
	  (setq number
		(string-to-number (substring xrefs (match-beginning 2)
					  (match-end 2))))
	  (if (setq entry (gnus-gethash group xref-hashtb))
	      (setcdr entry (cons number (cdr entry)))
	    (gnus-sethash group (cons number nil) xref-hashtb)))))
    (and start xref-hashtb)))

(defun gnus-mark-xrefs-as-read (from-newsgroup headers unreads)
  "Look through all the headers and mark the Xrefs as read."
  (let ((virtual (gnus-virtual-group-p from-newsgroup))
	name info xref-hashtb idlist method nth4)
    (with-current-buffer gnus-group-buffer
      (when (setq xref-hashtb
		  (gnus-create-xref-hashtb from-newsgroup headers unreads))
	(mapatoms
	 (lambda (group)
	   (unless (string= from-newsgroup (setq name (symbol-name group)))
	     (setq idlist (symbol-value group))
	     ;; Dead groups are not updated.
	     (and (prog1
		      (setq info (gnus-get-info name))
		    (when (stringp (setq nth4 (gnus-info-method info)))
		      (setq nth4 (gnus-server-to-method nth4))))
		  ;; Only do the xrefs if the group has the same
		  ;; select method as the group we have just read.
		  (or (gnus-methods-equal-p
		       nth4 (gnus-find-method-for-group from-newsgroup))
		      virtual
		      (equal nth4 (setq method (gnus-find-method-for-group
						from-newsgroup)))
		      (and (equal (car nth4) (car method))
			   (equal (nth 1 nth4) (nth 1 method))))
		  gnus-use-cross-reference
		  (or (not (eq gnus-use-cross-reference t))
		      virtual
		      ;; Only do cross-references on subscribed
		      ;; groups, if that is what is wanted.
		      (<= (gnus-info-level info) gnus-level-subscribed))
		  (gnus-group-make-articles-read name idlist))))
	 xref-hashtb)))))

(defun gnus-compute-read-articles (group articles)
  (let* ((entry (gnus-group-entry group))
	 (info (nth 2 entry))
	 (active (gnus-active group))
	 ninfo)
    (when entry
      ;; First peel off all invalid article numbers.
      (when active
	(let ((ids articles)
	      id first)
	  (while (setq id (pop ids))
	    (when (and first (> id (cdr active)))
	      ;; We'll end up in this situation in one particular
	      ;; obscure situation.  If you re-scan a group and get
	      ;; a new article that is cross-posted to a different
	      ;; group that has not been re-scanned, you might get
	      ;; crossposted article that has a higher number than
	      ;; Gnus believes possible.  So we re-activate this
	      ;; group as well.  This might mean doing the
	      ;; crossposting thingy will *increase* the number
	      ;; of articles in some groups.  Tsk, tsk.
	      (setq active (or (gnus-activate-group group) active)))
	    (when (or (> id (cdr active))
		      (< id (car active)))
	      (setq articles (delq id articles))))))
      ;; If the read list is nil, we init it.
      (if (and active
	       (null (gnus-info-read info))
	       (> (car active) 1))
	  (setq ninfo (cons 1 (1- (car active))))
	(setq ninfo (gnus-info-read info)))
      ;; Then we add the read articles to the range.
      (gnus-add-to-range
       ninfo (setq articles (sort articles '<))))))

(defun gnus-group-make-articles-read (group articles)
  "Update the info of GROUP to say that ARTICLES are read."
  (let* ((num 0)
	 (entry (gnus-group-entry group))
	 (info (nth 2 entry))
	 (active (gnus-active group))
	 (set-marks
	  (or gnus-propagate-marks
	      (gnus-method-option-p
	       (gnus-find-method-for-group group)
	       'server-marks)))
	 range)
    (if (not entry)
	;; Group that Gnus doesn't know exists, but still allow the
	;; backend to set marks.
	(when set-marks
	  (gnus-request-set-mark
	   group (list (list (gnus-compress-sequence (sort articles #'<))
			     'add '(read)))))
      ;; Normal, subscribed groups.
      (setq range (gnus-compute-read-articles group articles))
      (with-current-buffer gnus-group-buffer
	(gnus-undo-register
	  `(progn
	     (gnus-info-set-marks ',info ',(gnus-info-marks info) t)
	     (gnus-info-set-read ',info ',(gnus-info-read info))
	     (gnus-get-unread-articles-in-group ',info (gnus-active ,group))
	     (when ,set-marks
	       (gnus-request-set-mark
		,group (list (list ',range 'del '(read)))))
	     (gnus-group-update-group ,group t))))
      ;; Add the read articles to the range.
      (gnus-info-set-read info range)
      (when set-marks
	(gnus-request-set-mark group (list (list range 'add '(read)))))
      ;; Then we have to re-compute how many unread
      ;; articles there are in this group.
      (when active
	(cond
	 ((not range)
	  (setq num (- (1+ (cdr active)) (car active))))
	 ((not (listp (cdr range)))
	  (setq num (- (cdr active) (- (1+ (cdr range))
				       (car range)))))
	 (t
	  (while range
	    (if (numberp (car range))
		(setq num (1+ num))
	      (setq num (+ num (- (1+ (cdar range)) (caar range)))))
	    (setq range (cdr range)))
	  (setq num (- (cdr active) num))))
	;; Update the number of unread articles.
	(setcar entry num)
	;; Update the group buffer.
	(unless (gnus-ephemeral-group-p group)
	  (gnus-group-update-group group t))))))

(defun gnus-get-newsgroup-headers (&optional dependencies force-new)
  (let ((cur nntp-server-buffer)
	(dependencies
	 (or dependencies
	     (with-current-buffer gnus-summary-buffer
	       gnus-newsgroup-dependencies)))
	headers id end ref number
	(mail-parse-charset gnus-newsgroup-charset)
	(mail-parse-ignored-charsets
	 (save-current-buffer (condition-case nil
                                  (set-buffer gnus-summary-buffer)
                                (error))
                              gnus-newsgroup-ignored-charsets)))
    (with-current-buffer nntp-server-buffer
      ;; Translate all TAB characters into SPACE characters.
      (subst-char-in-region (point-min) (point-max) ?\t ?  t)
      (subst-char-in-region (point-min) (point-max) ?\r ?  t)
      (ietf-drums-unfold-fws)
      (gnus-run-hooks 'gnus-parse-headers-hook)
      (let ((case-fold-search t)
	    in-reply-to header p lines chars)
	(goto-char (point-min))
	;; Search to the beginning of the next header.  Error messages
	;; do not begin with 2 or 3.
	(while (re-search-forward "^[23][0-9]+ " nil t)
	  (setq id nil
		ref nil)
	  ;; This implementation of this function, with nine
	  ;; search-forwards instead of the one re-search-forward and
	  ;; a case (which basically was the old function) is actually
	  ;; about twice as fast, even though it looks messier.  You
	  ;; can't have everything, I guess.  Speed and elegance
	  ;; doesn't always go hand in hand.
	  (setq
	   header
	   (vector
	    ;; Number.
	    (prog1
		(setq number (read cur))
	      (end-of-line)
	      (setq p (point))
	      (narrow-to-region (point)
				(or (and (search-forward "\n.\n" nil t)
					 (- (point) 2))
				    (point))))
	    ;; Subject.
	    (progn
	      (goto-char p)
	      (if (search-forward "\nsubject:" nil t)
		  (funcall gnus-decode-encoded-word-function
			   (nnheader-header-value))
		"(none)"))
	    ;; From.
	    (progn
	      (goto-char p)
	      (if (search-forward "\nfrom:" nil t)
		  (funcall gnus-decode-encoded-address-function
			   (nnheader-header-value))
		"(nobody)"))
	    ;; Date.
	    (progn
	      (goto-char p)
	      (if (search-forward "\ndate:" nil t)
		  (nnheader-header-value) ""))
	    ;; Message-ID.
	    (progn
	      (goto-char p)
	      (setq id (if (re-search-forward
			    "^message-id: *\\(<[^\n\t> ]+>\\)" nil t)
			   ;; We do it this way to make sure the Message-ID
			   ;; is (somewhat) syntactically valid.
			   (buffer-substring (match-beginning 1)
					     (match-end 1))
			 ;; If there was no message-id, we just fake one
			 ;; to make subsequent routines simpler.
			 (nnheader-generate-fake-message-id number))))
	    ;; References.
	    (progn
	      (goto-char p)
	      (if (search-forward "\nreferences:" nil t)
		  (progn
		    (setq end (point))
		    (prog1
			(nnheader-header-value)
		      (setq ref
			    (buffer-substring
			     (progn
			       (end-of-line)
			       (search-backward ">" end t)
			       (1+ (point)))
			     (progn
			       (search-backward "<" end t)
			       (point))))))
		;; Get the references from the in-reply-to header if there
		;; were no references and the in-reply-to header looks
		;; promising.
		(if (and (search-forward "\nin-reply-to:" nil t)
			 (setq in-reply-to (nnheader-header-value))
			 (string-match "<[^>]+>" in-reply-to))
		    (let (ref2)
		      (setq ref (substring in-reply-to (match-beginning 0)
					   (match-end 0)))
		      (while (string-match "<[^>]+>" in-reply-to (match-end 0))
			(setq ref2 (substring in-reply-to (match-beginning 0)
					      (match-end 0)))
			(when (> (length ref2) (length ref))
			  (setq ref ref2)))
		      ref)
		  (setq ref nil))))
	    ;; Chars.
	    (progn
	      (goto-char p)
	      (if (search-forward "\nchars: " nil t)
		  (if (numberp (setq chars (ignore-errors (read cur))))
		      chars -1)
		-1))
	    ;; Lines.
	    (progn
	      (goto-char p)
	      (if (search-forward "\nlines: " nil t)
		  (if (numberp (setq lines (ignore-errors (read cur))))
		      lines -1)
		-1))
	    ;; Xref.
	    (progn
	      (goto-char p)
	      (and (search-forward "\nxref:" nil t)
		   (nnheader-header-value)))
	    ;; Extra.
	    (when gnus-extra-headers
	      (let ((extra gnus-extra-headers)
		    out)
		(while extra
		  (goto-char p)
		  (when (search-forward
			 (concat "\n" (symbol-name (car extra)) ":") nil t)
		    (push (cons (car extra) (nnheader-header-value))
			  out))
		  (pop extra))
		out))))
	  (when (equal id ref)
	    (setq ref nil))

	  (when gnus-alter-header-function
	    (funcall gnus-alter-header-function header)
	    (setq id (mail-header-id header)
		  ref (gnus-parent-id (mail-header-references header))))

	  (when (setq header
		      (gnus-dependencies-add-header
		       header dependencies force-new))
	    (push header headers))
	  (goto-char (point-max))
	  (widen))
	(nreverse headers)))))

;; Goes through the xover lines and returns a list of vectors
(defun gnus-get-newsgroup-headers-xover (sequence &optional
						  force-new dependencies
						  group also-fetch-heads)
  "Parse the news overview data in the server buffer.
Return a list of headers that match SEQUENCE (see
`nntp-retrieve-headers')."
  ;; Get the Xref when the users reads the articles since most/some
  ;; NNTP servers do not include Xrefs when using XOVER.
  (setq gnus-article-internal-prepare-hook '(gnus-article-get-xrefs))
  (let ((mail-parse-charset gnus-newsgroup-charset)
	(mail-parse-ignored-charsets gnus-newsgroup-ignored-charsets)
	(cur nntp-server-buffer)
	(dependencies (or dependencies gnus-newsgroup-dependencies))
	(allp (cond
	       ((eq gnus-read-all-available-headers t)
		t)
	       ((and (stringp gnus-read-all-available-headers)
		     group)
		(string-match gnus-read-all-available-headers group))
	       (t
		nil)))
	number headers header)
    (with-current-buffer nntp-server-buffer
      (subst-char-in-region (point-min) (point-max) ?\r ?  t)
      ;; Allow the user to mangle the headers before parsing them.
      (gnus-run-hooks 'gnus-parse-headers-hook)
      (goto-char (point-min))
      (gnus-parse-without-error
	(while (and (or sequence allp)
		    (not (eobp)))
	  (setq number (read cur))
	  (when (not allp)
	    (while (and sequence
			(< (car sequence) number))
	      (setq sequence (cdr sequence))))
	  (when (and (or allp
			 (and sequence
			      (eq number (car sequence))))
		     (progn
		       (setq sequence (cdr sequence))
		       (setq header (inline
				      (gnus-nov-parse-line
				       number dependencies force-new)))))
	    (push header headers))
	  (forward-line 1)))
      ;; A common bug in inn is that if you have posted an article and
      ;; then retrieves the active file, it will answer correctly --
      ;; the new article is included.  However, a NOV entry for the
      ;; article may not have been generated yet, so this may fail.
      ;; We work around this problem by retrieving the last few
      ;; headers using HEAD.
      (if (or (not also-fetch-heads)
	      (not sequence))
	  ;; We (probably) got all the headers.
	  (nreverse headers)
	(let ((gnus-nov-is-evil t))
	  (nconc
	   (nreverse headers)
	   (when (eq (gnus-retrieve-headers sequence group) 'headers)
	     (gnus-get-newsgroup-headers))))))))

(defun gnus-article-get-xrefs ()
  "Fill in the Xref value in `gnus-current-headers', if necessary.
This is meant to be called in `gnus-article-internal-prepare-hook'."
  (let ((headers (with-current-buffer gnus-summary-buffer
		   gnus-current-headers)))
    (or (not gnus-use-cross-reference)
	(not headers)
	(and (mail-header-xref headers)
	     (not (string= (mail-header-xref headers) "")))
	(let ((case-fold-search t)
	      xref)
	  (save-restriction
	    (nnheader-narrow-to-headers)
	    (goto-char (point-min))
	    (when (or (and (not (eobp))
			   (eq (downcase (char-after)) ?x)
			   (looking-at "Xref:"))
		      (search-forward "\nXref:" nil t))
	      (goto-char (1+ (match-end 0)))
	      (setq xref (buffer-substring (point) (point-at-eol)))
	      (mail-header-set-xref headers xref)))))))

(defun gnus-summary-insert-subject (id &optional old-header use-old-header)
  "Find article ID and insert the summary line for that article.
OLD-HEADER can either be a header or a line number to insert
the subject line on.
If USE-OLD-HEADER is non-nil, then OLD-HEADER should be a header,
and OLD-HEADER will be used when the summary line is inserted,
too, instead of trying to fetch new headers."
  (let* ((line (and (numberp old-header) old-header))
	 (old-header (and (vectorp old-header) old-header))
	 (header (cond ((and old-header use-old-header)
			old-header)
		       ((and (numberp id)
			     (gnus-number-to-header id))
			(gnus-number-to-header id))
		       (t
			(gnus-read-header id))))
	 (number (and (numberp id) id))
	 d)
    (when header
      ;; Rebuild the thread that this article is part of and go to the
      ;; article we have fetched.
      (when (and (not gnus-show-threads)
		 old-header)
	(when (and number
		   (setq d (gnus-data-find (mail-header-number old-header))))
	  (goto-char (gnus-data-pos d))
	  (gnus-data-remove
	   number
	   (- (point-at-bol)
	      (prog1
		  (1+ (point-at-eol))
		(gnus-delete-line))))))
      ;; Remove list identifiers from subject.
      (let ((gnus-newsgroup-headers (list header)))
        (gnus-summary-remove-list-identifiers))
      (when old-header
	(mail-header-set-number header (mail-header-number old-header)))
      (setq gnus-newsgroup-sparse
	    (delq (setq number (mail-header-number header))
		  gnus-newsgroup-sparse))
      (setq gnus-newsgroup-ancient (delq number gnus-newsgroup-ancient))
      (push number gnus-newsgroup-limit)
      (gnus-rebuild-thread (mail-header-id header) line)
      (gnus-summary-goto-subject number nil t))
    (when (and (numberp number)
	       (> number 0))
      ;; We have to update the boundaries even if we can't fetch the
      ;; article if ID is a number -- so that the next `P' or `N'
      ;; command will fetch the previous (or next) article even
      ;; if the one we tried to fetch this time has been canceled.
      (when (> number gnus-newsgroup-end)
	(setq gnus-newsgroup-end number))
      (when (< number gnus-newsgroup-begin)
	(setq gnus-newsgroup-begin number))
      (setq gnus-newsgroup-unselected
	    (delq number gnus-newsgroup-unselected)))
    ;; Report back a success?
    (and header (mail-header-number header))))

;;; Process/prefix in the summary buffer

(defun gnus-summary-work-articles (n)
  "Return a list of articles to be worked upon.
The prefix argument, the list of process marked articles, and the
current article will be taken into consideration."
  (with-current-buffer gnus-summary-buffer
    (cond
     (n
      ;; A numerical prefix has been given.
      (setq n (prefix-numeric-value n))
      (let ((backward (< n 0))
	    (n (abs (prefix-numeric-value n)))
	    articles article)
	(save-excursion
	  (while
	      (and (> n 0)
		   (push (setq article (gnus-summary-article-number))
			 articles)
		   (if backward
		       (gnus-summary-find-prev nil article)
		     (gnus-summary-find-next nil article)))
	    (decf n)))
	(nreverse articles)))
     ((and (gnus-region-active-p) (mark))
      (message "region active")
      ;; Work on the region between point and mark.
      (let ((max (max (point) (mark)))
	    articles article)
	(save-excursion
	  (goto-char (min (point) (mark)))
	  (while
	      (and
	       (push (setq article (gnus-summary-article-number)) articles)
	       (gnus-summary-find-next nil article)
	       (< (point) max)))
	  (nreverse articles))))
     (gnus-newsgroup-processable
      ;; There are process-marked articles present.
      ;; Save current state.
      (gnus-summary-save-process-mark)
      ;; Return the list.
      (reverse gnus-newsgroup-processable))
     (t
      ;; Just return the current article.
      (list (gnus-summary-article-number))))))

(defmacro gnus-summary-iterate (arg &rest forms)
  "Iterate over the process/prefixed articles and do FORMS.
ARG is the interactive prefix given to the command.  FORMS will be
executed with point over the summary line of the articles."
  (let ((articles (make-symbol "gnus-summary-iterate-articles")))
    `(let ((,articles (gnus-summary-work-articles ,arg)))
       (while ,articles
	 (gnus-summary-goto-subject (car ,articles))
	 ,@forms
	 (pop ,articles)))))

(put 'gnus-summary-iterate 'lisp-indent-function 1)
(put 'gnus-summary-iterate 'edebug-form-spec '(form body))

(defun gnus-summary-save-process-mark ()
  "Push the current set of process marked articles on the stack."
  (interactive)
  (push (copy-sequence gnus-newsgroup-processable)
	gnus-newsgroup-process-stack))

(defun gnus-summary-kill-process-mark ()
  "Push the current set of process marked articles on the stack and unmark."
  (interactive)
  (gnus-summary-save-process-mark)
  (gnus-summary-unmark-all-processable))

(defun gnus-summary-yank-process-mark ()
  "Pop the last process mark state off the stack and restore it."
  (interactive)
  (unless gnus-newsgroup-process-stack
    (error "Empty mark stack"))
  (gnus-summary-process-mark-set (pop gnus-newsgroup-process-stack)))

(defun gnus-summary-process-mark-set (set)
  "Make SET into the current process marked articles."
  (gnus-summary-unmark-all-processable)
  (mapc 'gnus-summary-set-process-mark set))

;;; Searching and stuff

(defun gnus-summary-search-group (&optional backward use-level)
  "Search for next unread newsgroup.
If optional argument BACKWARD is non-nil, search backward instead."
  (with-current-buffer gnus-group-buffer
    (when (gnus-group-search-forward
	   backward nil (if use-level (gnus-group-group-level) nil))
      (gnus-group-group-name))))

(defun gnus-summary-best-group (&optional exclude-group)
  "Find the name of the best unread group.
If EXCLUDE-GROUP, do not go to this group."
  (with-current-buffer gnus-group-buffer
    (save-excursion
      (gnus-group-best-unread-group exclude-group))))

(defun gnus-summary-find-next (&optional unread article backward)
  (if backward
      (gnus-summary-find-prev unread article)
    (let* ((dummy (gnus-summary-article-intangible-p))
	   (article (or article (gnus-summary-article-number)))
	   (data (gnus-data-find-list article))
	   result)
      (when (and (not dummy)
		 (or (not gnus-summary-check-current)
		     (not unread)
		     (not (gnus-data-unread-p (car data)))))
	(setq data (cdr data)))
      (when (setq result
		  (if unread
		      (progn
			(while data
                          (unless (memq (gnus-data-number (car data))
                                        (cond
					 ((eq gnus-auto-goto-ignores
					      'always-undownloaded)
					  gnus-newsgroup-undownloaded)
					 (gnus-plugged
					  nil)
					 ((eq gnus-auto-goto-ignores
					      'unfetched)
					  gnus-newsgroup-unfetched)
					 ((eq gnus-auto-goto-ignores
					      'undownloaded)
					  gnus-newsgroup-undownloaded)))
                            (when (gnus-data-unread-p (car data))
                              (setq result (car data)
                                    data nil)))
			  (setq data (cdr data)))
			result)
		    (car data)))
	(goto-char (gnus-data-pos result))
	(gnus-data-number result)))))

(defun gnus-summary-find-prev (&optional unread article)
  (let* ((eobp (eobp))
	 (article (or article (gnus-summary-article-number)))
	 (data (gnus-data-find-list article (gnus-data-list 'rev)))
	 result)
    (when (and (not eobp)
	       (or (not gnus-summary-check-current)
		   (not unread)
		   (not (gnus-data-unread-p (car data)))))
      (setq data (cdr data)))
    (when (setq result
		(if unread
		    (progn
		      (while data
                        (unless (memq (gnus-data-number (car data))
                                      (cond
				       ((eq gnus-auto-goto-ignores
					    'always-undownloaded)
					gnus-newsgroup-undownloaded)
				       (gnus-plugged
					nil)
				       ((eq gnus-auto-goto-ignores
					    'unfetched)
					gnus-newsgroup-unfetched)
				       ((eq gnus-auto-goto-ignores
					    'undownloaded)
					gnus-newsgroup-undownloaded)))
                          (when (gnus-data-unread-p (car data))
                            (setq result (car data)
                                  data nil)))
			(setq data (cdr data)))
		      result)
		  (car data)))
      (goto-char (gnus-data-pos result))
      (gnus-data-number result))))

(defun gnus-summary-find-subject (subject &optional unread backward article)
  (let* ((simp-subject (gnus-simplify-subject-fully subject))
	 (article (or article (gnus-summary-article-number)))
	 (articles (gnus-data-list backward))
	 (arts (gnus-data-find-list article articles))
	 result)
    (when (or (not gnus-summary-check-current)
	      (not unread)
	      (not (gnus-data-unread-p (car arts))))
      (setq arts (cdr arts)))
    (while arts
      (and (or (not unread)
	       (gnus-data-unread-p (car arts)))
	   (vectorp (gnus-data-header (car arts)))
	   (gnus-subject-equal
	    simp-subject (mail-header-subject (gnus-data-header (car arts))) t)
	   (setq result (car arts)
		 arts nil))
      (setq arts (cdr arts)))
    (and result
	 (goto-char (gnus-data-pos result))
	 (gnus-data-number result))))

(defun gnus-summary-search-forward (&optional unread subject backward)
  "Search forward for an article.
If UNREAD, look for unread articles.  If SUBJECT, look for
articles with that subject.  If BACKWARD, search backward instead."
  (cond (subject (gnus-summary-find-subject subject unread backward))
	(backward (gnus-summary-find-prev unread))
	(t (gnus-summary-find-next unread))))

(defun gnus-recenter (&optional n)
  "Center point in window and redisplay frame.
Also do horizontal recentering."
  (interactive "P")
  (when (and gnus-auto-center-summary
	     (not (eq gnus-auto-center-summary 'vertical)))
    (gnus-horizontal-recenter))
  (if (fboundp 'recenter-top-bottom)
      (recenter-top-bottom n)
    (recenter n)))

(put 'gnus-recenter 'isearch-scroll t)

(defun gnus-forward-line-ignore-invisible (n)
  "Move N lines forward (backward if N is negative).
Like forward-line, but skip over (and don't count) invisible lines."
  (let (done)
    (while (and (> n 0) (not done))
      ;; If the following character is currently invisible,
      ;; skip all characters with that same `invisible' property value.
      (while (gnus-invisible-p (point))
	(goto-char (gnus-next-char-property-change (point))))
      (forward-line 1)
      (if (eobp)
	  (setq done t)
	(setq n (1- n))))
    (while (and (< n 0) (not done))
      (forward-line -1)
      (if (bobp) (setq done t)
	(setq n (1+ n))
	(while (and (not (bobp)) (gnus-invisible-p (1- (point))))
	  (goto-char (gnus-previous-char-property-change (point))))))))

(defun gnus-summary-recenter ()
  "Center point in the summary window.
If `gnus-auto-center-summary' is nil, or the article buffer isn't
displayed, no centering will be performed."
  ;; Suggested by earle@mahendo.JPL.NASA.GOV (Greg Earle).
  ;; Recenter only when requested.  Suggested by popovich@park.cs.columbia.edu.
  (interactive)
  ;; The user has to want it.
  (when gnus-auto-center-summary
    (let* ((top (cond ((< (window-height) 4) 0)
		      ((< (window-height) 7) 1)
		      (t (if (numberp gnus-auto-center-summary)
			     gnus-auto-center-summary
                           (/ (1- (window-height)) 2)))))
	   (height (1- (window-height)))
	   (bottom (save-excursion
		     (goto-char (point-max))
		     (gnus-forward-line-ignore-invisible (- height))
		     (point)))
	   (window (get-buffer-window (current-buffer))))
      (when (get-buffer-window gnus-article-buffer)
	;; Only do recentering when the article buffer is displayed,
	;; Set the window start to either `bottom', which is the biggest
	;; possible valid number, or the second line from the top,
	;; whichever is the least.
	(let ((top-pos (save-excursion
			 (gnus-forward-line-ignore-invisible (- top))
			 (point))))
	  (if (> bottom top-pos)
	      ;; Keep the second line from the top visible
	      (set-window-start window top-pos)
	    ;; Try to keep the bottom line visible; if it's partially
	    ;; obscured, either scroll one more line to make it fully
	    ;; visible, or revert to using TOP-POS.
	    (save-excursion
	      (goto-char (point-max))
	      (gnus-forward-line-ignore-invisible -1)
	      (let ((last-line-start (point)))
		(goto-char bottom)
		(set-window-start window (point) t)
		(when (not (pos-visible-in-window-p last-line-start window))
		  (gnus-forward-line-ignore-invisible 1)
		  (set-window-start window (min (point) top-pos) t)))))))
      ;; Do horizontal recentering while we're at it.
      (when (and (get-buffer-window (current-buffer) t)
		 (not (eq gnus-auto-center-summary 'vertical)))
	(let ((selected (selected-window)))
	  (select-window (get-buffer-window (current-buffer) t))
	  (gnus-summary-position-point)
	  (gnus-horizontal-recenter)
	  (select-window selected))))))

(defun gnus-summary-jump-to-group (newsgroup)
  "Move point to NEWSGROUP in group mode buffer."
  ;; Keep update point of group mode buffer if visible.
  (if (eq (current-buffer) (get-buffer gnus-group-buffer))
      (save-window-excursion
	;; Take care of tree window mode.
	(when (get-buffer-window gnus-group-buffer)
	  (pop-to-buffer gnus-group-buffer))
	(gnus-group-jump-to-group newsgroup))
    (save-excursion
      ;; Take care of tree window mode.
      (if (get-buffer-window gnus-group-buffer 0)
	  (pop-to-buffer gnus-group-buffer)
	(set-buffer gnus-group-buffer))
      (gnus-group-jump-to-group newsgroup))))

;; This function returns a list of article numbers based on the
;; difference between the ranges of read articles in this group and
;; the range of active articles.
(defun gnus-list-of-unread-articles (group)
  (let* ((read (gnus-info-read (gnus-get-info group)))
	 (active (or (gnus-active group) (gnus-activate-group group)))
	 (last (or (cdr active)
		   (error "Group %s couldn't be activated " group)))
	 (bottom (if gnus-newsgroup-maximum-articles
		     (max (car active)
			  (- last gnus-newsgroup-maximum-articles -1))
		   (car active)))
	 first nlast unread)
    ;; If none are read, then all are unread.
    (if (not read)
	(setq first bottom)
      ;; If the range of read articles is a single range, then the
      ;; first unread article is the article after the last read
      ;; article.  Sounds logical, doesn't it?
      (if (and (not (listp (cdr read)))
	       (or (< (car read) bottom)
		   (progn (setq read (list read))
			  nil)))
	  (setq first (max bottom (1+ (cdr read))))
	;; `read' is a list of ranges.
	(when (/= (setq nlast (or (and (numberp (car read)) (car read))
				  (caar read)))
		  1)
	  (setq first bottom))
	(while read
	  (when first
	    (while (< first nlast)
	      (setq unread (cons first unread)
                    first (1+ first))))
	  (setq first (1+ (if (atom (car read)) (car read) (cdar read))))
	  (setq nlast (if (atom (cadr read)) (cadr read) (caadr read)))
	  (setq read (cdr read)))))
    ;; And add the last unread articles.
    (while (<= first last)
      (setq unread (cons first unread)
            first (1+ first)))
    ;; Return the list of unread articles.
    (delq 0 (nreverse unread))))

(defun gnus-list-of-read-articles (group)
  "Return a list of unread, unticked and non-dormant articles."
  (let* ((info (gnus-get-info group))
	 (marked (gnus-info-marks info))
	 (active (gnus-active group)))
    (and info active
	 (gnus-list-range-difference
	  (gnus-list-range-difference
	   (gnus-sorted-complement
	    (gnus-uncompress-range
	     (if gnus-newsgroup-maximum-articles
		 (cons (max (car active)
			    (- (cdr active)
			       gnus-newsgroup-maximum-articles
			       -1))
		       (cdr active))
	       active))
	    (gnus-list-of-unread-articles group))
	   (cdr (assq 'dormant marked)))
	  (cdr (assq 'tick marked))))))

;; This function returns a sequence of article numbers based on the
;; difference between the ranges of read articles in this group and
;; the range of active articles.
(defun gnus-sequence-of-unread-articles (group)
  (let* ((read (gnus-info-read (gnus-get-info group)))
	 (active (or (gnus-active group) (gnus-activate-group group)))
	 (last (cdr active))
	 (bottom (if gnus-newsgroup-maximum-articles
		     (max (car active)
			  (- last gnus-newsgroup-maximum-articles -1))
		   (car active)))
	 first nlast unread)
    ;; If none are read, then all are unread.
    (if (not read)
	(setq first bottom)
      ;; If the range of read articles is a single range, then the
      ;; first unread article is the article after the last read
      ;; article.  Sounds logical, doesn't it?
      (if (and (not (listp (cdr read)))
	       (or (< (car read) bottom)
		   (progn (setq read (list read))
			  nil)))
	  (setq first (max bottom (1+ (cdr read))))
	;; `read' is a list of ranges.
	(when (/= (setq nlast (or (and (numberp (car read)) (car read))
				  (caar read)))
		  1)
	  (setq first bottom))
	(while read
	  (when first
            (push (cons first nlast) unread))
	  (setq first (1+ (if (atom (car read)) (car read) (cdar read))))
	  (setq nlast (if (atom (cadr read)) (cadr read) (caadr read)))
	  (setq read (cdr read)))))
    ;; And add the last unread articles.
    (cond ((not (and first last))
	   nil)
	  ((< first last)
	   (push (cons first last) unread))
	  ((= first last)
	   (push first unread)))
    ;; Return the sequence of unread articles.
    (delq 0 (nreverse unread))))

;; Various summary commands

(defun gnus-summary-select-article-buffer ()
  "Reconfigure windows to show the article buffer.
If `gnus-widen-article-window' is set, show only the article
buffer."
  (interactive)
  (if (not (gnus-buffer-live-p gnus-article-buffer))
      (error "There is no article buffer for this summary buffer")
    (unless (get-buffer-window gnus-article-buffer)
      (gnus-summary-show-article))
    (gnus-configure-windows
     (if gnus-widen-article-window
	 'only-article
       'article)
     t)
    (select-window (get-buffer-window gnus-article-buffer))))

(defun gnus-summary-universal-argument (arg)
  "Perform any operation on all articles that are process/prefixed."
  (interactive "P")
  (let ((articles (gnus-summary-work-articles arg))
	func article)
    (if (eq
	 (setq
	  func
	  (key-binding
	   (read-key-sequence
	    (substitute-command-keys
	     "\\<gnus-summary-mode-map>\\[gnus-summary-universal-argument]"))))
	 'undefined)
	(gnus-error 1 "Undefined key")
      (save-excursion
	(while articles
	  (gnus-summary-goto-subject (setq article (pop articles)))
	  (let (gnus-newsgroup-processable)
	    (command-execute func))
	  (gnus-summary-remove-process-mark article)))))
  (gnus-summary-position-point))

(defun gnus-summary-toggle-truncation (&optional arg)
  "Toggle truncation of summary lines.
With ARG, turn line truncation on if ARG is positive."
  (interactive "P")
  (setq truncate-lines
	(if (null arg) (not truncate-lines)
	  (> (prefix-numeric-value arg) 0)))
  (redraw-display))

(defun gnus-summary-find-for-reselect ()
  "Return the number of an article to stay on across a reselect.
The current article is considered, then following articles, then previous
articles.  An article is sought which is not canceled and isn't a temporary
insertion from another group.  If there's no such then return a dummy 0."
  (let (found)
    (dolist (rev '(nil t))
      (unless found      ; don't demand the reverse list if we don't need it
        (let ((data (gnus-data-find-list
                     (gnus-summary-article-number) (gnus-data-list rev))))
          (while (and data (not found))
            (if (and (< 0 (gnus-data-number (car data)))
                     (not (eq gnus-canceled-mark (gnus-data-mark (car data)))))
                (setq found (gnus-data-number (car data))))
            (setq data (cdr data))))))
    (or found 0)))

(defun gnus-summary-reselect-current-group (&optional all rescan)
  "Exit and then reselect the current newsgroup.
The prefix argument ALL means to select all articles."
  (interactive "P")
  (when (gnus-ephemeral-group-p gnus-newsgroup-name)
    (error "Ephemeral groups can't be reselected"))
  (let ((current-subject (gnus-summary-find-for-reselect))
	(group gnus-newsgroup-name))
    (setq gnus-newsgroup-begin nil)
    (gnus-summary-exit nil 'leave-hidden)
    ;; We have to adjust the point of group mode buffer because
    ;; point was moved to the next unread newsgroup by exiting.
    (gnus-summary-jump-to-group group)
    (when rescan
      (save-excursion
	(gnus-group-get-new-news-this-group 1)))
    (gnus-group-read-group all t)
    (gnus-summary-goto-subject current-subject nil t)))

(defun gnus-summary-rescan-group (&optional all)
  "Exit the newsgroup, ask for new articles, and select the newsgroup."
  (interactive "P")
  (let ((config gnus-current-window-configuration))
    (gnus-summary-reselect-current-group all t)
    (gnus-configure-windows config)
    (when (eq config 'article)
      (gnus-summary-select-article))))

(defun gnus-summary-update-info (&optional non-destructive)
  (save-excursion
    (let ((group gnus-newsgroup-name))
      (when group
	(when gnus-newsgroup-kill-headers
	  (setq gnus-newsgroup-killed
		(gnus-compress-sequence
		 (gnus-sorted-union
		  (gnus-list-range-intersection
		   gnus-newsgroup-unselected gnus-newsgroup-killed)
		  gnus-newsgroup-unreads)
		 t)))
	(unless (listp (cdr gnus-newsgroup-killed))
	  (setq gnus-newsgroup-killed (list gnus-newsgroup-killed)))
	(let ((headers gnus-newsgroup-headers)
	      (ephemeral-p (gnus-ephemeral-group-p group))
	      info)
	  (unless ephemeral-p
	    (setq info (copy-sequence (gnus-get-info group))
		  info (delq (gnus-info-params info) info)))
	  ;; Set the new ranges of read articles.
	  (with-current-buffer gnus-group-buffer
	    (gnus-undo-force-boundary))
	  (gnus-update-read-articles
	   group (gnus-sorted-union
		  gnus-newsgroup-unreads gnus-newsgroup-unselected))
	  ;; Set the current article marks.
	  (let ((gnus-newsgroup-scored
		 (if (and (not gnus-save-score)
			  (not non-destructive))
		     nil
		   gnus-newsgroup-scored)))
	    (save-excursion
	      (gnus-update-marks)))
	  ;; Do the cross-ref thing.
	  (when gnus-use-cross-reference
	    (gnus-mark-xrefs-as-read group headers gnus-newsgroup-unreads))
	  ;; Do not switch windows but change the buffer to work.
	  (set-buffer gnus-group-buffer)
	  (unless ephemeral-p
	    (gnus-group-update-group
	     group nil
	     (equal info
		    (setq info (copy-sequence (gnus-get-info group))
			  info (delq (gnus-info-params info) info))))))))))

(defun gnus-summary-save-newsrc (&optional force)
  "Save the current number of read/marked articles in the dribble buffer.
The dribble buffer will then be saved.
If FORCE (the prefix), also save the .newsrc file(s)."
  (interactive "P")
  (gnus-summary-update-info t)
  (if force
      (gnus-save-newsrc-file)
    (gnus-dribble-save)))

(declare-function gnus-cache-write-active "gnus-cache" (&optional force))

(defun gnus-summary-exit (&optional temporary leave-hidden)
  "Exit reading current newsgroup, and then return to group selection mode.
`gnus-exit-group-hook' is called with no arguments if that value is non-nil."
  (interactive)
  (gnus-set-global-variables)
  (when (gnus-buffer-live-p gnus-article-buffer)
    (with-current-buffer gnus-article-buffer
      (mm-destroy-parts gnus-article-mime-handles)
      ;; Set it to nil for safety reason.
      (setq gnus-article-mime-handle-alist nil)
      (setq gnus-article-mime-handles nil)))
  (gnus-kill-save-kill-buffer)
  (gnus-async-halt-prefetch)
  (let* ((group gnus-newsgroup-name)
	 (quit-config (gnus-group-quit-config gnus-newsgroup-name))
	 (gnus-group-is-exiting-p t)
	 (article-buffer gnus-article-buffer)
	 (original-article-buffer gnus-original-article-buffer)
	 (mode major-mode)
	 (group-point nil)
	 (buf (current-buffer))
	 ;; `gnus-single-article-buffer' is nil buffer-locally in
	 ;; ephemeral group of which summary buffer will be killed,
	 ;; but the global value may be non-nil.
	 (single-article-buffer gnus-single-article-buffer))
    (unless quit-config
      ;; Do adaptive scoring, and possibly save score files.
      (when gnus-newsgroup-adaptive
	(gnus-score-adaptive))
      (when gnus-use-scoring
	(gnus-score-save)))
    (gnus-run-hooks 'gnus-summary-prepare-exit-hook)
    (when gnus-use-cache
      (gnus-cache-possibly-remove-articles)
      (gnus-cache-save-buffers))
    (gnus-async-prefetch-remove-group group)
    (when gnus-suppress-duplicates
      (gnus-dup-enter-articles))
    (when gnus-use-trees
      (gnus-tree-close group))
    (when gnus-use-cache
      (gnus-cache-write-active))
    ;; Remove entries for this group.
    (nnmail-purge-split-history (gnus-group-real-name group))
    ;; Make all changes in this group permanent.
    (unless quit-config
      (gnus-run-hooks 'gnus-exit-group-hook)
      (gnus-summary-update-info))
    (gnus-close-group group)
    ;; Make sure where we were, and go to next newsgroup.
    (set-buffer gnus-group-buffer)
    (unless quit-config
      (gnus-group-jump-to-group group))
    (gnus-run-hooks 'gnus-summary-exit-hook)
    (unless (or quit-config
		(not gnus-summary-next-group-on-exit)
		;; If this group has disappeared from the summary
		;; buffer, don't skip forwards.
		(not (string= group (gnus-group-group-name))))
      (gnus-group-next-unread-group 1))
    (setq group-point (point))
    (if temporary
	nil				;Nothing to do.
      (set-buffer buf)
      (if (not gnus-kill-summary-on-exit)
	  (progn
	    (gnus-deaden-summary)
	    (setq mode nil))
	(when (get-buffer gnus-article-buffer)
	  (bury-buffer gnus-article-buffer))
	;; Return to group mode buffer.
	(when (eq mode 'gnus-summary-mode)
	  (gnus-kill-buffer buf)))

      (setq gnus-current-select-method gnus-select-method)
      (set-buffer gnus-group-buffer)
      (if quit-config
	  (gnus-handle-ephemeral-exit quit-config)
	(goto-char group-point)
	;; If gnus-group-buffer is already displayed, make sure we also move
	;; the cursor in the window that displays it.
	(let ((win (get-buffer-window (current-buffer) 0)))
	  (if win (set-window-point win (point))))
	(unless leave-hidden
	  (gnus-configure-windows 'group 'force)))

      ;; If we have several article buffers, we kill them at exit.
      (unless single-article-buffer
	(when (gnus-buffer-live-p article-buffer)
	  (with-current-buffer article-buffer
	    ;; Don't kill sticky article buffers
	    (unless (eq major-mode 'gnus-sticky-article-mode)
	      (gnus-kill-buffer article-buffer)
	      (setq gnus-article-current nil))))
	(gnus-kill-buffer original-article-buffer))

      ;; Clear the current group name.
      (unless quit-config
	(setq gnus-newsgroup-name nil)))))

(defalias 'gnus-summary-quit 'gnus-summary-exit-no-update)
(defun gnus-summary-exit-no-update (&optional no-questions)
  "Quit reading current newsgroup without updating read article info."
  (interactive)
  (let* ((group gnus-newsgroup-name)
	 (gnus-group-is-exiting-p t)
	 (gnus-group-is-exiting-without-update-p t)
	 (quit-config (gnus-group-quit-config group)))
    (when (or no-questions
	      gnus-expert-user
	      (gnus-y-or-n-p "Discard changes to this group and exit? "))
      (gnus-async-halt-prefetch)
      (run-hooks 'gnus-summary-prepare-exit-hook)
      (when (gnus-buffer-live-p gnus-article-buffer)
	(with-current-buffer gnus-article-buffer
	  (gnus-article-stop-animations)
	  (gnus-stop-downloads)
	  (mm-destroy-parts gnus-article-mime-handles)
	  ;; Set it to nil for safety reason.
	  (setq gnus-article-mime-handle-alist nil)
	  (setq gnus-article-mime-handles nil)))
      ;; If we have several article buffers, we kill them at exit.
      (unless gnus-single-article-buffer
	(gnus-kill-buffer gnus-article-buffer)
	(gnus-kill-buffer gnus-original-article-buffer)
	(setq gnus-article-current nil))
      ;; Return to the group buffer.
      (if (not gnus-kill-summary-on-exit)
	  (progn
	    (gnus-deaden-summary)
	    (gnus-configure-windows 'group 'force))
	(gnus-configure-windows 'group 'force)
	(gnus-close-group group)
	(gnus-kill-buffer gnus-summary-buffer))
      (unless gnus-single-article-buffer
	(setq gnus-article-current nil))
      (when gnus-use-trees
	(gnus-tree-close group))
      (gnus-async-prefetch-remove-group group)
      (when (get-buffer gnus-article-buffer)
	(bury-buffer gnus-article-buffer))
      ;; Clear the current group name.
      (setq gnus-newsgroup-name nil)
      (unless (gnus-ephemeral-group-p group)
	(gnus-group-update-group group nil t))
      (when (equal (gnus-group-group-name) group)
	(gnus-group-next-unread-group 1))
      (when quit-config
	(gnus-handle-ephemeral-exit quit-config)))))

(defun gnus-handle-ephemeral-exit (quit-config)
  "Handle movement when leaving an ephemeral group.
The state which existed when entering the ephemeral is reset."
  (if (not (buffer-live-p (car quit-config)))
      (gnus-configure-windows 'group 'force)
    (set-buffer (car quit-config))
    (unless (eq (cdr quit-config) 'group)
      (setq gnus-current-select-method
	    (gnus-find-method-for-group gnus-newsgroup-name)))
    (cond ((eq major-mode 'gnus-summary-mode)
	   (gnus-set-global-variables))
	  ((eq major-mode 'gnus-article-mode)
	   (save-current-buffer
	     ;; The `gnus-summary-buffer' variable may point
	     ;; to the old summary buffer when using a single
	     ;; article buffer.
	     (unless (gnus-buffer-live-p gnus-summary-buffer)
	       (set-buffer gnus-group-buffer))
	     (set-buffer gnus-summary-buffer)
	     (gnus-set-global-variables))))
    (if (or (eq (cdr quit-config) 'article)
	    (eq (cdr quit-config) 'pick))
	(if (and (boundp 'gnus-pick-mode) (symbol-value 'gnus-pick-mode))
	    (gnus-configure-windows 'pick 'force)
	  (gnus-configure-windows (cdr quit-config) 'force))
      (gnus-configure-windows (cdr quit-config) 'force))
    (when (eq major-mode 'gnus-summary-mode)
      (if (memq gnus-auto-select-on-ephemeral-exit '(next-noselect
						     next-unread-noselect))
	  (when (zerop (cond ((eq gnus-auto-select-on-ephemeral-exit
				  'next-noselect)
			      (gnus-summary-next-subject 1 nil t))
			     ((eq gnus-auto-select-on-ephemeral-exit
				  'next-unread-noselect)
			      (gnus-summary-next-subject 1 t t))))
	    ;; Hide the article buffer which displays the article different
	    ;; from the one that the cursor points to in the summary buffer.
	    (gnus-configure-windows 'summary 'force))
	(cond ((eq gnus-auto-select-on-ephemeral-exit 'next)
	       (gnus-summary-next-subject 1))
	      ((eq gnus-auto-select-on-ephemeral-exit 'next-unread)
	       (gnus-summary-next-subject 1 t))))
      (gnus-summary-recenter)
      (gnus-summary-position-point))))

;;; Dead summaries.

(defvar gnus-dead-summary-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (substitute-key-definition 'undefined 'gnus-summary-wake-up-the-dead map)
    (dolist (key '("\C-d" "\r" "\177" [delete]))
      (define-key map key 'gnus-summary-wake-up-the-dead))
    (dolist (key '("q" "Q"))
      (define-key map key 'bury-buffer))
    map))

(define-minor-mode gnus-dead-summary-mode
  "Minor mode for Gnus summary buffers."
  :lighter " Dead" :keymap gnus-dead-summary-mode-map
  (unless (derived-mode-p 'gnus-summary-mode)
    (setq gnus-dead-summary-mode nil)))

(defun gnus-deaden-summary ()
  "Make the current summary buffer into a dead summary buffer."
  ;; Kill any previous dead summary buffer.
  (when (and gnus-dead-summary
	     (buffer-name gnus-dead-summary))
    (with-current-buffer gnus-dead-summary
      (when gnus-dead-summary-mode
	(kill-buffer (current-buffer)))))
  ;; Make this the current dead summary.
  (setq gnus-dead-summary (current-buffer))
  (gnus-dead-summary-mode 1)
  (let ((name (buffer-name)))
    (when (string-match "Summary" name)
      (rename-buffer
       (concat (substring name 0 (match-beginning 0)) "Dead "
	       (substring name (match-beginning 0)))
       t)
      (bury-buffer))))

(defun gnus-kill-or-deaden-summary (buffer)
  "Kill or deaden the summary BUFFER."
  (save-excursion
    (when (and (buffer-name buffer)
	       (not gnus-single-article-buffer))
      (with-current-buffer buffer
	(gnus-kill-buffer gnus-article-buffer)
	(gnus-kill-buffer gnus-original-article-buffer)))
    (cond
     ;; Kill the buffer.
     (gnus-kill-summary-on-exit
      (when (and gnus-use-trees
		 (gnus-buffer-exists-p buffer))
	(with-current-buffer buffer
	  (gnus-tree-close gnus-newsgroup-name)))
      (gnus-kill-buffer buffer))
     ;; Deaden the buffer.
     ((gnus-buffer-exists-p buffer)
      (with-current-buffer buffer
	(gnus-deaden-summary))))))

(defun gnus-summary-wake-up-the-dead (&rest args)
  "Wake up the dead summary buffer."
  (interactive)
  (gnus-dead-summary-mode -1)
  (let ((name (buffer-name)))
    (when (string-match "Dead " name)
      (rename-buffer
       (concat (substring name 0 (match-beginning 0))
	       (substring name (match-end 0)))
       t)))
  (gnus-message 3 "This dead summary is now alive again"))

;; Suggested by Per Abrahamsen <amanda@iesd.auc.dk>.
(defun gnus-summary-describe-group (&optional force)
  "Describe the current newsgroup."
  (interactive "P")
  (gnus-group-describe-group force gnus-newsgroup-name))

(defun gnus-summary-describe-briefly ()
  "Describe summary mode commands briefly."
  (interactive)
  (gnus-message 6 "%s" (substitute-command-keys "\\<gnus-summary-mode-map>\\[gnus-summary-next-page]:Select  \\[gnus-summary-next-unread-article]:Forward  \\[gnus-summary-prev-unread-article]:Backward  \\[gnus-summary-exit]:Exit  \\[gnus-info-find-node]:Run Info	 \\[gnus-summary-describe-briefly]:This help")))

;; Walking around group mode buffer from summary mode.

(defun gnus-summary-next-group (&optional no-article target-group backward)
  "Exit current newsgroup and then select next unread newsgroup.
If prefix argument NO-ARTICLE is non-nil, no article is selected
initially.  If TARGET-GROUP, go to this group.  If BACKWARD, go to
previous group instead."
  (interactive "P")
  ;; Stop pre-fetching.
  (gnus-async-halt-prefetch)
  (let ((current-group gnus-newsgroup-name)
	(current-buffer (current-buffer))
	entered)
    ;; First we semi-exit this group to update Xrefs and all variables.
    ;; We can't do a real exit, because the window conf must remain
    ;; the same in case the user is prompted for info, and we don't
    ;; want the window conf to change before that...
    (gnus-summary-exit t)
    (while (not entered)
      ;; Then we find what group we are supposed to enter.
      (set-buffer gnus-group-buffer)
      (gnus-group-jump-to-group current-group)
      (setq target-group
	    (or target-group
		(if (eq gnus-keep-same-level 'best)
		    (gnus-summary-best-group gnus-newsgroup-name)
		  (gnus-summary-search-group backward gnus-keep-same-level))))
      (if (not target-group)
	  ;; There are no further groups, so we return to the group
	  ;; buffer.
	  (progn
	    (gnus-message 5 "Returning to the group buffer")
	    (setq entered t)
	    (when (gnus-buffer-live-p current-buffer)
	      (set-buffer current-buffer)
	      (gnus-summary-exit))
	    (gnus-run-hooks 'gnus-group-no-more-groups-hook))
	;; We try to enter the target group.
	(gnus-group-jump-to-group target-group)
	(let ((unreads (gnus-group-group-unread)))
	  (if (and (or (eq t unreads)
		       (and unreads (not (zerop unreads))))
 		   (gnus-summary-read-group
 		    target-group nil no-article
 		    (and (buffer-name current-buffer) current-buffer)
 		    nil backward))
	      (setq entered t)
	    (setq current-group target-group
		  target-group nil)))))))

(defun gnus-summary-prev-group (&optional no-article)
  "Exit current newsgroup and then select previous unread newsgroup.
If prefix argument NO-ARTICLE is non-nil, no article is selected initially."
  (interactive "P")
  (gnus-summary-next-group no-article nil t))

;; Walking around summary lines.

(defun gnus-summary-first-subject (&optional unread undownloaded unseen)
  "Go to the first subject satisfying any non-nil constraint.
If UNREAD is non-nil, the article should be unread.
If UNDOWNLOADED is non-nil, the article should be undownloaded.
If UNSEEN is non-nil, the article should be unseen as well as unread.
Returns the article selected or nil if there are no matching articles."
  (interactive "P")
  (cond
   ;; Empty summary.
   ((null gnus-newsgroup-data)
    (gnus-message 3 "No articles in the group")
    nil)
   ;; Pick the first article.
   ((not (or unread undownloaded unseen))
    (goto-char (gnus-data-pos (car gnus-newsgroup-data)))
    (gnus-data-number (car gnus-newsgroup-data)))
   ;; Find the first unread article.
   (t
    (let ((data gnus-newsgroup-data))
      (while (and data
                  (let ((num (gnus-data-number (car data))))
                    (or (memq num gnus-newsgroup-unfetched)
                        (not (or (and unread
                                      (memq num gnus-newsgroup-unreads))
                                 (and undownloaded
                                      (memq num gnus-newsgroup-undownloaded))
                                 (and unseen
                                      (memq num gnus-newsgroup-unseen)
				      (memq num gnus-newsgroup-unreads)))))))
        (setq data (cdr data)))
      (prog1
          (if data
              (progn
                (goto-char (gnus-data-pos (car data)))
                (gnus-data-number (car data)))
            (gnus-message 3 "No more%s articles"
                          (let* ((r (when unread " unread"))
                                 (d (when undownloaded " undownloaded"))
                                 (s (when unseen " unseen"))
                                 (l (delq nil (list r d s))))
                            (cond ((= 3 (length l))
                                   (concat r "," d ", or" s))
                                  ((= 2 (length l))
                                   (concat (car l) ", or" (cadr l)))
                                  ((= 1 (length l))
                                   (car l))
                                  (t
                                   ""))))
            nil
            )
        (gnus-summary-position-point))))))

(defun gnus-summary-next-subject (n &optional unread dont-display)
  "Go to next N'th summary line.
If N is negative, go to the previous N'th subject line.
If UNREAD is non-nil, only unread articles are selected.
The difference between N and the actual number of steps taken is
returned."
  (interactive "p")
  (let ((backward (< n 0))
	(n (abs n)))
    (while (and (> n 0)
		(if backward
		    (gnus-summary-find-prev unread)
		  (gnus-summary-find-next unread)))
      (unless (zerop (setq n (1- n)))
	(gnus-summary-show-thread)))
    (when (/= 0 n)
      (gnus-message 7 "No more%s articles"
		    (if unread " unread" "")))
    (unless dont-display
      (gnus-summary-recenter)
      (gnus-summary-position-point))
    n))

(defun gnus-summary-next-unread-subject (n)
  "Go to next N'th unread summary line."
  (interactive "p")
  (gnus-summary-next-subject n t))

(defun gnus-summary-prev-subject (n &optional unread)
  "Go to previous N'th summary line.
If optional argument UNREAD is non-nil, only unread article is selected."
  (interactive "p")
  (gnus-summary-next-subject (- n) unread))

(defun gnus-summary-prev-unread-subject (n)
  "Go to previous N'th unread summary line."
  (interactive "p")
  (gnus-summary-next-subject (- n) t))

(defun gnus-summary-goto-subjects (articles)
  "Insert the subject header for ARTICLES in the current buffer."
  (save-excursion
    (dolist (article articles)
      (gnus-summary-goto-subject article t)))
  (gnus-summary-limit (append articles gnus-newsgroup-limit))
  (gnus-summary-position-point))

(defun gnus-summary-goto-subject (article &optional force silent)
  "Go to the subject line of ARTICLE.
If FORCE, also allow jumping to articles not currently shown."
  (interactive "nArticle number: ")
  (unless (numberp article)
    (error "Article %s is not a number" article))
  (let ((b (point))
	(data (gnus-data-find article)))
    ;; We read in the article if we have to.
    (and (not data)
	 force
	 (gnus-summary-insert-subject
	  article
	  (if (or (numberp force) (vectorp force)) force)
	  t)
	 (setq data (gnus-data-find article)))
    (goto-char b)
    (if (not data)
	(progn
	  (unless silent
	    (gnus-message 3 "Can't find article %d" article))
	  nil)
      (let ((pt (gnus-data-pos data)))
	(goto-char pt)
	(gnus-summary-set-article-display-arrow pt))
      (gnus-summary-position-point)
      article)))

;; Walking around summary lines with displaying articles.

(defun gnus-summary-expand-window (&optional arg)
  "Make the summary buffer take up the entire Emacs frame.
Given a prefix, will force an `article' buffer configuration."
  (interactive "P")
  (if arg
      (gnus-configure-windows 'article 'force)
    (gnus-configure-windows 'summary 'force)))

(defun gnus-summary-display-article (article &optional all-header)
  "Display ARTICLE in article buffer."
  (unless (and (gnus-buffer-live-p gnus-article-buffer)
	       (with-current-buffer gnus-article-buffer
		 (eq major-mode 'gnus-article-mode)))
    (gnus-article-setup-buffer))
  (gnus-set-global-variables)
  (with-current-buffer gnus-article-buffer
    (setq gnus-article-charset gnus-newsgroup-charset)
    (setq gnus-article-ignored-charsets gnus-newsgroup-ignored-charsets)
    (mm-enable-multibyte))
  (if (null article)
      nil
    (prog1
	(if gnus-summary-display-article-function
	    (funcall gnus-summary-display-article-function article all-header)
	  (gnus-article-prepare article all-header))
      (gnus-run-hooks 'gnus-select-article-hook)
      (when (and gnus-current-article
		 (not (zerop gnus-current-article)))
	(gnus-summary-goto-subject gnus-current-article))
      (gnus-summary-recenter)
      (when (and gnus-use-trees gnus-show-threads)
	(gnus-possibly-generate-tree article)
	(gnus-highlight-selected-tree article))
      ;; Successfully display article.
      (gnus-article-set-window-start
       (cdr (assq article gnus-newsgroup-bookmarks))))))

(defun gnus-summary-select-article (&optional all-headers force pseudo article)
  "Select the current article.
If ALL-HEADERS is non-nil, show all header fields.  If FORCE is
non-nil, the article will be re-fetched even if it already present in
the article buffer.  If PSEUDO is non-nil, pseudo-articles will also
be displayed."
  ;; Make sure we are in the summary buffer to work around bbdb bug.
  (unless (eq major-mode 'gnus-summary-mode)
    (set-buffer gnus-summary-buffer))
  (let ((article (or article (gnus-summary-article-number)))
	(all-headers (not (not all-headers))) ;Must be t or nil.
	gnus-summary-display-article-function)
    (and (not pseudo)
	 (gnus-summary-article-pseudo-p article)
	 (error "This is a pseudo-article"))
    (with-current-buffer gnus-summary-buffer
      (if (or (and gnus-single-article-buffer
		   (or (null gnus-current-article)
		       (null gnus-article-current)
		       (null (get-buffer gnus-article-buffer))
		       (not (eq article (cdr gnus-article-current)))
		       (not (equal (car gnus-article-current)
				   gnus-newsgroup-name))
		       (not (get-buffer gnus-original-article-buffer))))
	      (and (not gnus-single-article-buffer)
		   (or (null gnus-current-article)
		       (not (get-buffer gnus-original-article-buffer))
		       (not (eq gnus-current-article article))))
	      force)
	  ;; The requested article is different from the current article.
	  (progn
	    (gnus-summary-display-article article all-headers)
	    (when (gnus-buffer-live-p gnus-article-buffer)
	      (with-current-buffer gnus-article-buffer
		(if (not gnus-article-decoded-p) ;; a local variable
		    (mm-disable-multibyte))))
	    (gnus-article-set-window-start
	     (cdr (assq article gnus-newsgroup-bookmarks)))
	    article)
	'old))))

(defun gnus-summary-force-verify-and-decrypt ()
  "Display buttons for signed/encrypted parts and verify/decrypt them."
  (interactive)
  (let ((mm-verify-option 'known)
	(mm-decrypt-option 'known)
	(gnus-article-emulate-mime t)
	(gnus-buttonized-mime-types (append (list "multipart/signed"
						  "multipart/encrypted")
					    gnus-buttonized-mime-types)))
    (gnus-summary-select-article nil 'force)))

(defun gnus-summary-set-current-mark (&optional current-mark)
  "Obsolete function."
  nil)

(defun gnus-summary-next-article (&optional unread subject backward push)
  "Select the next article.
If UNREAD, only unread articles are selected.
If SUBJECT, only articles with SUBJECT are selected.
If BACKWARD, the previous article is selected instead of the next."
  (interactive "P")
  ;; Make sure we are in the summary buffer.
  (unless (eq major-mode 'gnus-summary-mode)
    (set-buffer gnus-summary-buffer))
  (cond
   ;; Is there such an article?
   ((and (gnus-summary-search-forward unread subject backward)
	 (or (gnus-summary-display-article (gnus-summary-article-number))
	     (eq (gnus-summary-article-mark) gnus-canceled-mark)))
    (gnus-summary-position-point))
   ;; If not, we try the first unread, if that is wanted.
   ((and subject
	 gnus-auto-select-same
	 (gnus-summary-first-unread-article))
    (gnus-summary-position-point)
    (gnus-message 6 "Wrapped"))
   ;; Try to get next/previous article not displayed in this group.
   ((and gnus-auto-extend-newsgroup
	 (not unread) (not subject))
    (gnus-summary-goto-article
     (if backward (1- gnus-newsgroup-begin) (1+ gnus-newsgroup-end))
     nil (count-lines (point-min) (point))))
   ;; Go to next/previous group.
   (t
    (unless (gnus-ephemeral-group-p gnus-newsgroup-name)
      (gnus-summary-jump-to-group gnus-newsgroup-name))
    (let ((cmd (if (featurep 'xemacs)
		   last-command-char
		 last-command-event))
	  (point
	   (with-current-buffer gnus-group-buffer
	     (point)))
	  (current-summary (current-buffer))
	  (group
	   (if (eq gnus-keep-same-level 'best)
	       (gnus-summary-best-group gnus-newsgroup-name)
	     (gnus-summary-search-group backward gnus-keep-same-level))))
      ;; Select next unread newsgroup automagically.
      (cond
       ((or (not gnus-auto-select-next)
	    (not cmd))
	(gnus-message 7 "No more%s articles" (if unread " unread" "")))
       ((or (eq gnus-auto-select-next 'quietly)
	    (and (eq gnus-auto-select-next 'slightly-quietly)
		 push)
	    (and (eq gnus-auto-select-next 'almost-quietly)
		 (gnus-summary-last-article-p)))
	;; Select quietly.
	(if (gnus-ephemeral-group-p gnus-newsgroup-name)
	    (gnus-summary-exit)
	  (gnus-message 7 "No more%s articles (%s)..."
			(if unread " unread" "")
			(if group (concat "selecting " group)
			  "exiting"))
	  (gnus-summary-next-group nil group backward)))
       (t
	(when (gnus-key-press-event-p last-input-event)
	  ;; Somehow or other, we may now have selected a different
	  ;; window.  Make point go back to the summary buffer.
	  (when (eq current-summary (current-buffer))
            ;; FIXME: This burps when get-buffer-window returns nil.
	    (select-window (get-buffer-window current-summary 0)))
	  (gnus-summary-walk-group-buffer
	   gnus-newsgroup-name cmd unread backward point))))))))

(defun gnus-summary-walk-group-buffer (from-group cmd unread backward start)
  (let ((keystrokes '((?\C-n (gnus-group-next-unread-group 1))
		      (?\C-p (gnus-group-prev-unread-group 1))))
	(cursor-in-echo-area t)
	keve key group ended prompt)
    (with-current-buffer gnus-group-buffer
      (goto-char start)
      (setq group
	    (if (eq gnus-keep-same-level 'best)
		(gnus-summary-best-group gnus-newsgroup-name)
	      (gnus-summary-search-group backward gnus-keep-same-level))))
    (while (not ended)
      (setq prompt
	    (format
	     "No more%s articles%s " (if unread " unread" "")
	     (if (and group
		      (not (gnus-ephemeral-group-p gnus-newsgroup-name)))
		 (format " (Type %s for %s [%s])"
			 (single-key-description cmd)
			 (gnus-group-decoded-name group)
			 (gnus-group-unread group))
	       (format " (Type %s to exit %s)"
		       (single-key-description cmd)
		       (gnus-group-decoded-name gnus-newsgroup-name)))))
      ;; Confirm auto selection.
      (setq key (car (setq keve (gnus-read-event-char prompt)))
	    ended t)
      (cond
       ((assq key keystrokes)
	(let ((obuf (current-buffer)))
	  (switch-to-buffer gnus-group-buffer)
	  (when group
	    (gnus-group-jump-to-group group))
	  (eval (cadr (assq key keystrokes)))
	  (setq group (gnus-group-group-name))
	  (switch-to-buffer obuf))
	(setq ended nil))
       ((equal key cmd)
	(if (or (not group)
		(gnus-ephemeral-group-p gnus-newsgroup-name))
	    (gnus-summary-exit)
	  (gnus-summary-next-group nil group backward)))
       (t
	(push (cdr keve) unread-command-events))))))

(defun gnus-summary-next-unread-article ()
  "Select unread article after current one."
  (interactive)
  (gnus-summary-next-article
   (or (not (eq gnus-summary-goto-unread 'never))
       (gnus-summary-last-article-p (gnus-summary-article-number)))
   (and gnus-auto-select-same
	(gnus-summary-article-subject))))

(defun gnus-summary-prev-article (&optional unread subject)
  "Select the article before the current one.
If UNREAD is non-nil, only unread articles are selected."
  (interactive "P")
  (gnus-summary-next-article unread subject t))

(defun gnus-summary-prev-unread-article ()
  "Select unread article before current one."
  (interactive)
  (gnus-summary-prev-article
   (or (not (eq gnus-summary-goto-unread 'never))
       (gnus-summary-first-article-p (gnus-summary-article-number)))
   (and gnus-auto-select-same
	(gnus-summary-article-subject))))

(defun gnus-summary-next-page (&optional lines circular stop)
  "Show next page of the selected article.
If at the end of the current article, select the next article.
LINES says how many lines should be scrolled up.

If CIRCULAR is non-nil, go to the start of the article instead of
selecting the next article when reaching the end of the current
article.

If STOP is non-nil, just stop when reaching the end of the message.

Also see the variable `gnus-article-skip-boring'."
  (interactive "P")
  (setq gnus-summary-buffer (current-buffer))
  (gnus-set-global-variables)
  (let ((article (gnus-summary-article-number))
	(article-window (get-buffer-window gnus-article-buffer t))
	endp)
    ;; If the buffer is empty, we have no article.
    (unless article
      (error "No article to select"))
    (gnus-configure-windows 'article)
    (if (eq (cdr (assq article gnus-newsgroup-reads)) gnus-canceled-mark)
	(if (and (eq gnus-summary-goto-unread 'never)
		 (not (gnus-summary-last-article-p article)))
	    (gnus-summary-next-article)
	  (gnus-summary-next-unread-article))
      (if (or (null gnus-current-article)
	      (null gnus-article-current)
	      (/= article (cdr gnus-article-current))
	      (not (equal (car gnus-article-current) gnus-newsgroup-name)))
	  ;; Selected subject is different from current article's.
	  (gnus-summary-display-article article)
	(when article-window
	  (gnus-eval-in-buffer-window gnus-article-buffer
	    (setq endp (or (gnus-article-next-page lines)
			   (gnus-article-only-boring-p))))
	  (when endp
	    (cond ((or stop gnus-summary-stop-at-end-of-message)
		   (gnus-message 3 "End of message"))
		  (circular
		   (gnus-summary-beginning-of-article))
		  (lines
		   (gnus-message 3 "End of message"))
		  ((null lines)
		   (if (and (eq gnus-summary-goto-unread 'never)
			    (not (gnus-summary-last-article-p article)))
		       (gnus-summary-next-article)
		     (gnus-summary-next-unread-article))))))))
    (gnus-summary-recenter)
    (gnus-summary-position-point)))

(defun gnus-summary-prev-page (&optional lines move)
  "Show previous page of selected article.
Argument LINES specifies lines to be scrolled down.
If MOVE, move to the previous unread article if point is at
the beginning of the buffer."
  (interactive "P")
  (let ((article (gnus-summary-article-number))
	(article-window (get-buffer-window gnus-article-buffer t))
	endp)
    (gnus-configure-windows 'article)
    (if (or (null gnus-current-article)
	    (null gnus-article-current)
	    (/= article (cdr gnus-article-current))
	    (not (equal (car gnus-article-current) gnus-newsgroup-name)))
	;; Selected subject is different from current article's.
	(gnus-summary-display-article article)
      (gnus-summary-recenter)
      (when article-window
	(gnus-eval-in-buffer-window gnus-article-buffer
	  (setq endp (gnus-article-prev-page lines)))
	(when (and move endp)
	  (cond (lines
		 (gnus-message 3 "Beginning of message"))
		((null lines)
		 (if (and (eq gnus-summary-goto-unread 'never)
			  (not (gnus-summary-first-article-p article)))
		     (gnus-summary-prev-article)
		   (gnus-summary-prev-unread-article))))))))
  (gnus-summary-position-point))

(defun gnus-summary-prev-page-or-article (&optional lines)
  "Show previous page of selected article.
Argument LINES specifies lines to be scrolled down.
If at the beginning of the article, go to the next article."
  (interactive "P")
  (gnus-summary-prev-page lines t))

(defun gnus-summary-scroll-up (lines)
  "Scroll up (or down) one line current article.
Argument LINES specifies lines to be scrolled up (or down if negative).
If no article is selected, then the current article will be selected first."
  (interactive "p")
  (gnus-configure-windows 'article)
  (gnus-summary-show-thread)
  (when (eq (gnus-summary-select-article nil nil 'pseudo) 'old)
    (gnus-eval-in-buffer-window gnus-article-buffer
      (cond ((> lines 0)
	     (when (gnus-article-next-page lines)
	       (gnus-message 3 "End of message")))
	    ((< lines 0)
	     (gnus-article-prev-page (- lines))))))
  (gnus-summary-recenter)
  (gnus-summary-position-point))

(defun gnus-summary-scroll-down (lines)
  "Scroll down (or up) one line current article.
Argument LINES specifies lines to be scrolled down (or up if negative).
If no article is selected, then the current article will be selected first."
  (interactive "p")
  (gnus-summary-scroll-up (- lines)))

(defun gnus-summary-next-same-subject ()
  "Select next article which has the same subject as current one."
  (interactive)
  (gnus-summary-next-article nil (gnus-summary-article-subject)))

(defun gnus-summary-prev-same-subject ()
  "Select previous article which has the same subject as current one."
  (interactive)
  (gnus-summary-prev-article nil (gnus-summary-article-subject)))

(defun gnus-summary-next-unread-same-subject ()
  "Select next unread article which has the same subject as current one."
  (interactive)
  (gnus-summary-next-article t (gnus-summary-article-subject)))

(defun gnus-summary-prev-unread-same-subject ()
  "Select previous unread article which has the same subject as current one."
  (interactive)
  (gnus-summary-prev-article t (gnus-summary-article-subject)))

(defun gnus-summary-first-unread-article ()
  "Select the first unread article.
Return nil if there are no unread articles."
  (interactive)
  (prog1
      (when (gnus-summary-first-subject t)
	(gnus-summary-show-thread)
	(gnus-summary-first-subject t)
	(gnus-summary-display-article (gnus-summary-article-number)))
    (gnus-summary-position-point)))

(defun gnus-summary-first-unread-subject ()
  "Place the point on the subject line of the first unread article.
Return nil if there are no unread articles."
  (interactive)
  (prog1
      (when (gnus-summary-first-subject t)
	(gnus-summary-show-thread)
	(gnus-summary-first-subject t))
    (gnus-summary-position-point)))

(defun gnus-summary-first-unseen-subject ()
  "Place the point on the subject line of the first unseen article.
Return nil if there are no unseen articles."
  (interactive)
  (prog1
      (when (gnus-summary-first-subject nil nil t)
	(gnus-summary-show-thread)
	(gnus-summary-first-subject nil nil t))
    (gnus-summary-position-point)))

(defun gnus-summary-first-unseen-or-unread-subject ()
  "Place the point on the subject line of the first unseen and unread article.
If all article have been seen, on the subject line of the first unread
article."
  (interactive)
  (prog1
      (unless (when (gnus-summary-first-subject nil nil t)
		(gnus-summary-show-thread)
		(gnus-summary-first-subject nil nil t))
	(when (gnus-summary-first-subject t)
	  (gnus-summary-show-thread)
	  (gnus-summary-first-subject t)))
    (gnus-summary-position-point)))

(defun gnus-summary-first-article ()
  "Select the first article.
Return nil if there are no articles."
  (interactive)
  (prog1
      (when (gnus-summary-first-subject)
	(gnus-summary-show-thread)
	(gnus-summary-first-subject)
	(gnus-summary-display-article (gnus-summary-article-number)))
    (gnus-summary-position-point)))

(defun gnus-summary-best-unread-article (&optional arg)
  "Select the unread article with the highest score.
If given a prefix argument, select the next unread article that has a
score higher than the default score."
  (interactive "P")
  (let ((article (if arg
		     (gnus-summary-better-unread-subject)
		   (gnus-summary-best-unread-subject))))
    (if article
	(gnus-summary-goto-article article)
      (error "No unread articles"))))

(defun gnus-summary-best-unread-subject ()
  "Select the unread subject with the highest score."
  (interactive)
  (let ((best -1000000)
	(data gnus-newsgroup-data)
	article score)
    (while data
      (and (gnus-data-unread-p (car data))
	   (> (setq score
		    (gnus-summary-article-score (gnus-data-number (car data))))
	      best)
	   (setq best score
		 article (gnus-data-number (car data))))
      (setq data (cdr data)))
    (when article
      (gnus-summary-goto-subject article))
    (gnus-summary-position-point)
    article))

(defun gnus-summary-better-unread-subject ()
  "Select the first unread subject that has a score over the default score."
  (interactive)
  (let ((data gnus-newsgroup-data)
	article score)
    (while (and (setq article (gnus-data-number (car data)))
		(or (gnus-data-read-p (car data))
		    (not (> (gnus-summary-article-score article)
			    gnus-summary-default-score))))
      (setq data (cdr data)))
    (when article
      (gnus-summary-goto-subject article))
    (gnus-summary-position-point)
    article))

(defun gnus-summary-last-subject ()
  "Go to the last displayed subject line in the group."
  (let ((article (gnus-data-number (car (gnus-data-list t)))))
    (when article
      (gnus-summary-goto-subject article))))

(defun gnus-summary-goto-article (article &optional all-headers force)
  "Fetch ARTICLE (article number or Message-ID) and display it if it exists.
If ALL-HEADERS is non-nil, no header lines are hidden.
If FORCE, go to the article even if it isn't displayed.  If FORCE
is a number, it is the line the article is to be displayed on."
  (interactive
   (list
    (gnus-completing-read
     "Article number or Message-ID"
     (mapcar 'int-to-string gnus-newsgroup-limit))
    current-prefix-arg
    t))
  (prog1
      (if (and (stringp article)
	       (string-match "@\\|%40" article))
	  (gnus-summary-refer-article article)
	(when (stringp article)
	  (setq article (string-to-number article)))
	(if (gnus-summary-goto-subject article force)
	    (gnus-summary-display-article article all-headers)
	  (gnus-message 4 "Couldn't go to article %s" article) nil))
    (gnus-summary-position-point)))

(defun gnus-summary-goto-last-article ()
  "Go to the previously read article."
  (interactive)
  (prog1
      (when gnus-last-article
	(gnus-summary-goto-article gnus-last-article nil t))
    (gnus-summary-position-point)))

(defun gnus-summary-pop-article (number)
  "Pop one article off the history and go to the previous.
NUMBER articles will be popped off."
  (interactive "p")
  (let (to)
    (setq gnus-newsgroup-history
	  (cdr (setq to (nthcdr number gnus-newsgroup-history))))
    (if to
	(gnus-summary-goto-article (car to) nil t)
      (error "Article history empty")))
  (gnus-summary-position-point))

;; Summary commands and functions for limiting the summary buffer.

(defun gnus-summary-limit-to-articles (n)
  "Limit the summary buffer to the next N articles.
If not given a prefix, use the process marked articles instead."
  (interactive "P")
  (prog1
      (let ((articles (gnus-summary-work-articles n)))
	(setq gnus-newsgroup-processable nil)
	(gnus-summary-limit articles))
    (gnus-summary-position-point)))

(defun gnus-summary-pop-limit (&optional total)
  "Restore the previous limit.
If given a prefix, remove all limits."
  (interactive "P")
  (when total
    (setq gnus-newsgroup-limits
	  (list (mapcar (lambda (h) (mail-header-number h))
			gnus-newsgroup-headers))))
  (unless gnus-newsgroup-limits
    (error "No limit to pop"))
  (prog1
      (gnus-summary-limit nil 'pop)
    (gnus-summary-position-point)))

(defun gnus-summary-limit-to-subject (subject &optional header not-matching)
  "Limit the summary buffer to articles that have subjects that match a regexp.
If NOT-MATCHING, excluding articles that have subjects that match a regexp."
  (interactive
   (list (read-string (if current-prefix-arg
			  "Exclude subject (regexp): "
			"Limit to subject (regexp): "))
	 nil current-prefix-arg))
  (unless header
    (setq header "subject"))
  (when (not (equal "" subject))
    (prog1
	(let ((articles (gnus-summary-find-matching
			 (or header "subject") subject 'all nil nil
			 not-matching)))
	  (unless articles
	    (error "Found no matches for \"%s\"" subject))
	  (gnus-summary-limit articles))
      (gnus-summary-position-point))))

(defun gnus-summary-limit-to-author (from &optional not-matching)
  "Limit the summary buffer to articles that have authors that match a regexp.
If NOT-MATCHING, excluding articles that have authors that match a regexp."
  (interactive
   (list (read-string (if current-prefix-arg
			  "Exclude author (regexp): "
			"Limit to author (regexp): "))
	 current-prefix-arg))
  (gnus-summary-limit-to-subject from "from" not-matching))

(defun gnus-summary-limit-to-recipient (recipient &optional not-matching)
  "Limit the summary buffer to articles with the given RECIPIENT.

If NOT-MATCHING, exclude RECIPIENT.

To and Cc headers are checked.  You need to include them in
`nnmail-extra-headers'."
  ;; Unlike `rmail-summary-by-recipients', doesn't include From.
  (interactive
   (list (read-string (format "%s recipient (regexp): "
			      (if current-prefix-arg "Exclude" "Limit to")))
	 current-prefix-arg))
  (when (not (equal "" recipient))
    (prog1 (let* ((to
		   (if (memq 'To nnmail-extra-headers)
		       (gnus-summary-find-matching
			(cons 'extra 'To) recipient 'all nil nil
			not-matching)
		     (gnus-message
		      1 "`To' isn't present in `nnmail-extra-headers'")
		     (sit-for 1)
		     nil))
		  (cc
		   (if (memq 'Cc nnmail-extra-headers)
		       (gnus-summary-find-matching
			(cons 'extra 'Cc) recipient 'all nil nil
			not-matching)
		     (gnus-message
		      1 "`Cc' isn't present in `nnmail-extra-headers'")
		     (sit-for 1)
		     nil))
		  (articles
		   (if not-matching
		       ;; We need the numbers that are in both lists:
		       (mapcar (lambda (a)
				 (and (memq a to) a))
			       cc)
		     (nconc to cc))))
	     (unless articles
	       (error "Found no matches for \"%s\"" recipient))
	     (gnus-summary-limit articles))
      (gnus-summary-position-point))))

(defun gnus-summary-limit-to-address (address &optional not-matching)
  "Limit the summary buffer to articles with the given ADDRESS.

If NOT-MATCHING, exclude ADDRESS.

To, Cc and From headers are checked.  You need to include `To' and `Cc'
in `nnmail-extra-headers'."
  (interactive
   (list (read-string (format "%s address (regexp): "
			      (if current-prefix-arg "Exclude" "Limit to")))
	 current-prefix-arg))
  (when (not (equal "" address))
    (prog1 (let* ((to
		   (if (memq 'To nnmail-extra-headers)
		       (gnus-summary-find-matching
			(cons 'extra 'To) address 'all nil nil
			not-matching)
		     (gnus-message
		      1 "`To' isn't present in `nnmail-extra-headers'")
		     (sit-for 1)
		     t))
		  (cc
		   (if (memq 'Cc nnmail-extra-headers)
		       (gnus-summary-find-matching
			(cons 'extra 'Cc) address 'all nil nil
			not-matching)
		     (gnus-message
		      1 "`Cc' isn't present in `nnmail-extra-headers'")
		     (sit-for 1)
		     t))
		  (from
		   (gnus-summary-find-matching "from" address
					       'all nil nil not-matching))
		  (articles
		   (if not-matching
		       ;; We need the numbers that are in all lists:
		       (if (eq cc t)
			   (if (eq to t)
			       from
			     (mapcar (lambda (a) (car (memq a from))) to))
			 (if (eq to t)
			     (mapcar (lambda (a) (car (memq a from))) cc)
			   (mapcar (lambda (a) (car (memq a from)))
				   (mapcar (lambda (a) (car (memq a to)))
					   cc))))
		     (nconc (if (eq to t) nil to)
			    (if (eq cc t) nil cc)
			    from))))
	     (unless articles
	       (error "Found no matches for \"%s\"" address))
	     (gnus-summary-limit articles))
      (gnus-summary-position-point))))

(defun gnus-summary-limit-strange-charsets-predicate (header)
  (when (fboundp 'char-charset)
    (let ((string (concat (mail-header-subject header)
			  (mail-header-from header)))
	  charset found)
      (dotimes (i (1- (length string)))
	(setq charset (format "%s" (char-charset (aref string (1+ i)))))
	(when (string-match "unicode\\|big\\|japanese" charset)
	  (setq found t)))
      found)))

(defun gnus-summary-limit-to-predicate (predicate)
  "Limit to articles where PREDICATE returns non-nil.
PREDICATE will be called with the header structures of the
articles."
  (let ((articles nil)
	(case-fold-search t))
    (dolist (header gnus-newsgroup-headers)
      (when (funcall predicate header)
	(push (mail-header-number header) articles)))
    (gnus-summary-limit (nreverse articles))))

(defun gnus-summary-limit-to-age (age &optional younger-p)
  "Limit the summary buffer to articles that are older than (or equal) AGE days.
If YOUNGER-P (the prefix) is non-nil, limit the summary buffer to
articles that are younger than AGE days."
  (interactive
   (let ((younger current-prefix-arg)
	 (days-got nil)
	 days)
     (while (not days-got)
       (setq days (if younger
		      (read-string "Limit to articles younger than (in days, older when negative): ")
		    (read-string
		     "Limit to articles older than (in days, younger when negative): ")))
       (when (> (length days) 0)
	 (setq days (read days)))
       (if (numberp days)
	   (progn
	     (setq days-got t)
	     (when (< days 0)
	       (setq younger (not younger))
	       (setq days (* days -1))))
	 (message "Please enter a number.")
	 (sleep-for 1)))
     (list days younger)))
  (prog1
      (let ((data gnus-newsgroup-data)
	    (cutoff (days-to-time age))
	    articles d date is-younger)
	(while (setq d (pop data))
	  (when (and (vectorp (gnus-data-header d))
		     (setq date (mail-header-date (gnus-data-header d))))
	    (setq is-younger (time-less-p
			      (time-since (gnus-date-get-time date))
			      cutoff))
	    (when (if younger-p
		      is-younger
		    (not is-younger))
	      (push (gnus-data-number d) articles))))
	(gnus-summary-limit (nreverse articles)))
    (gnus-summary-position-point)))

(defun gnus-summary-limit-to-extra (header regexp &optional not-matching)
  "Limit the summary buffer to articles that match an 'extra' header."
  (interactive
   (let ((header
	  (intern
	   (gnus-completing-read
	    (if current-prefix-arg
		"Exclude extra header"
	      "Limit extra header")
	    (mapcar 'symbol-name gnus-extra-headers)
	    t nil nil
            (symbol-name (car gnus-extra-headers))))))
     (list header
	   (read-string (format "%s header %s (regexp): "
				(if current-prefix-arg "Exclude" "Limit to")
				header))
	   current-prefix-arg)))
  (when (not (equal "" regexp))
    (prog1
	(let ((articles (gnus-summary-find-matching
			 (cons 'extra header) regexp 'all nil nil
			 not-matching)))
	  (unless articles
	    (error "Found no matches for \"%s\"" regexp))
	  (gnus-summary-limit articles))
      (gnus-summary-position-point))))

(defun gnus-summary-limit-to-display-predicate ()
  "Limit the summary buffer to the predicated in the `display' group parameter."
  (interactive)
  (unless gnus-newsgroup-display
    (error "There is no `display' group parameter"))
  (let (articles)
    (dolist (gnus-number gnus-newsgroup-articles)
      (when (funcall gnus-newsgroup-display)
	(push gnus-number articles)))
    (gnus-summary-limit articles))
  (gnus-summary-position-point))

(defun gnus-summary-limit-to-unread (&optional all)
  "Limit the summary buffer to articles that are not marked as read.
If ALL is non-nil, limit strictly to unread articles."
  (interactive "P")
  (if all
      (gnus-summary-limit-to-marks (char-to-string gnus-unread-mark))
    (gnus-summary-limit-to-marks
     ;; Concat all the marks that say that an article is read and have
     ;; those removed.
     (list gnus-del-mark gnus-read-mark gnus-ancient-mark
	   gnus-killed-mark gnus-spam-mark gnus-kill-file-mark
	   gnus-low-score-mark gnus-expirable-mark
	   gnus-canceled-mark gnus-catchup-mark gnus-sparse-mark
	   gnus-duplicate-mark)
     'reverse)))

(defun gnus-summary-limit-to-headers (match &optional reverse)
  "Limit the summary buffer to articles that have headers that match MATCH.
If REVERSE (the prefix), limit to articles that don't match."
  (interactive "sMatch headers (regexp): \nP")
  (gnus-summary-limit-to-bodies match reverse t))

(defun gnus-summary-limit-to-bodies (match &optional reverse headersp)
  "Limit the summary buffer to articles that have bodies that match MATCH.
If REVERSE (the prefix), limit to articles that don't match."
  (interactive "sMatch body (regexp): \nP")
  (let ((articles nil)
	(gnus-select-article-hook nil)	;Disable hook.
	(gnus-article-prepare-hook nil)
	(gnus-use-article-prefetch nil)
	(gnus-keep-backlog nil)
	(gnus-break-pages nil)
	(gnus-summary-display-arrow nil)
	(gnus-updated-mode-lines nil)
	(gnus-auto-center-summary nil)
	(gnus-display-mime-function nil))
    (dolist (data gnus-newsgroup-data)
      (let (gnus-mark-article-hook)
	(gnus-summary-select-article t t nil (gnus-data-number data)))
      (with-current-buffer gnus-article-buffer
	(article-goto-body)
	(let* ((case-fold-search t)
	       (found (if headersp
			  (re-search-backward match nil t)
			(re-search-forward match nil t))))
	  (when (or (and found
			 (not reverse))
		    (and (not found)
			 reverse))
	    (push (gnus-data-number data) articles)))))
    (if (not articles)
	(message "No messages matched")
      (gnus-summary-limit articles)))
  (gnus-summary-position-point))

(defun gnus-summary-limit-to-singletons (&optional threadsp)
  "Limit the summary buffer to articles that aren't part on any thread.
If THREADSP (the prefix), limit to articles that are in threads."
  (interactive "P")
  (let ((articles nil)
	thread-articles
	threads)
    (dolist (thread gnus-newsgroup-threads)
      (if (stringp (car thread))
	  (dolist (thread (cdr thread))
	    (push thread threads))
	(push thread threads)))
    (dolist (thread threads)
      (setq thread-articles (gnus-articles-in-thread thread))
      (when (or (and threadsp
		     (> (length thread-articles) 1))
		(and (not threadsp)
		     (= (length thread-articles) 1)))
	(setq articles (nconc thread-articles articles))))
    (if (not articles)
	(message "No messages matched")
      (gnus-summary-limit articles))
    (gnus-summary-position-point)))

(defun gnus-summary-limit-to-replied (&optional unreplied)
  "Limit the summary buffer to replied articles.
If UNREPLIED (the prefix), limit to unreplied articles."
  (interactive "P")
  (if unreplied
      (gnus-summary-limit
       (gnus-set-difference gnus-newsgroup-articles
	gnus-newsgroup-replied))
    (gnus-summary-limit gnus-newsgroup-replied))
  (gnus-summary-position-point))

(defun gnus-summary-limit-exclude-marks (marks &optional reverse)
  "Exclude articles that are marked with MARKS (e.g. \"DK\").
If REVERSE, limit the summary buffer to articles that are marked
with MARKS.  MARKS can either be a string of marks or a list of marks.
Returns how many articles were removed."
  (interactive "sMarks: ")
  (gnus-summary-limit-to-marks marks t))

(defun gnus-summary-limit-to-marks (marks &optional reverse)
  "Limit the summary buffer to articles that are marked with MARKS (e.g. \"DK\").
If REVERSE (the prefix), limit the summary buffer to articles that are
not marked with MARKS.  MARKS can either be a string of marks or a
list of marks.
Returns how many articles were removed."
  (interactive "sMarks: \nP")
  (prog1
      (let ((data gnus-newsgroup-data)
	    (marks (if (listp marks) marks
		     (append marks nil))) ; Transform to list.
	    articles)
	(while data
	  (when (if reverse (not (memq (gnus-data-mark (car data)) marks))
		  (memq (gnus-data-mark (car data)) marks))
	    (push (gnus-data-number (car data)) articles))
	  (setq data (cdr data)))
	(gnus-summary-limit articles))
    (gnus-summary-position-point)))

(defun gnus-summary-limit-to-score (score)
  "Limit to articles with score at or above SCORE."
  (interactive "NLimit to articles with score of at least: ")
  (let ((data gnus-newsgroup-data)
	articles)
    (while data
      (when (>= (gnus-summary-article-score (gnus-data-number (car data)))
		score)
	(push (gnus-data-number (car data)) articles))
      (setq data (cdr data)))
    (prog1
	(gnus-summary-limit articles)
      (gnus-summary-position-point))))

(defun gnus-summary-limit-to-unseen ()
  "Limit to unseen articles."
  (interactive)
  (prog1
      (gnus-summary-limit gnus-newsgroup-unseen)
    (gnus-summary-position-point)))

(defun gnus-summary-limit-include-thread (id)
  "Display all the hidden articles that is in the thread with ID in it.
When called interactively, ID is the Message-ID of the current
article."
  (interactive (list (mail-header-id (gnus-summary-article-header))))
  (let ((articles (gnus-articles-in-thread
		   (gnus-id-to-thread (gnus-root-id id))))
	;;we REALLY want the whole thread---this prevents cut-threads
	;;from removing the thread we want to include.
	(gnus-fetch-old-headers nil)
	(gnus-build-sparse-threads nil))
    (prog1
	(gnus-summary-limit (nconc articles gnus-newsgroup-limit))
      (gnus-summary-limit-include-matching-articles
       "subject"
       (regexp-quote (gnus-simplify-subject-re
		      (mail-header-subject (gnus-id-to-header id)))))
      (gnus-summary-position-point))))

(defun gnus-summary-limit-include-matching-articles (header regexp)
  "Display all the hidden articles that have HEADERs that match REGEXP."
  (interactive (list (read-string "Match on header: ")
		     (read-string "Regexp: ")))
  (let ((articles (gnus-find-matching-articles header regexp)))
    (prog1
	(gnus-summary-limit (nconc articles gnus-newsgroup-limit))
      (gnus-summary-position-point))))

(defun gnus-summary-insert-dormant-articles ()
  "Insert all the dormant articles for this group into the current buffer."
  (interactive)
  (let ((gnus-verbose (max 6 gnus-verbose)))
    (if (not gnus-newsgroup-dormant)
	(gnus-message 3 "No dormant articles for this group")
      (gnus-summary-goto-subjects gnus-newsgroup-dormant))))

(defun gnus-summary-insert-ticked-articles ()
  "Insert ticked articles for this group into the current buffer."
  (interactive)
  (let ((gnus-verbose (max 6 gnus-verbose)))
    (if (not gnus-newsgroup-marked)
	(gnus-message 3 "No ticked articles for this group")
      (gnus-summary-goto-subjects gnus-newsgroup-marked))))

(defun gnus-summary-limit-include-dormant ()
  "Display all the hidden articles that are marked as dormant.
Note that this command only works on a subset of the articles currently
fetched for this group."
  (interactive)
  (unless gnus-newsgroup-dormant
    (error "There are no dormant articles in this group"))
  (prog1
      (gnus-summary-limit (append gnus-newsgroup-dormant gnus-newsgroup-limit))
    (gnus-summary-position-point)))

(defun gnus-summary-include-articles (articles)
  "Fetch the headers for ARTICLES and then display the summary lines."
  (let ((gnus-inhibit-demon t)
	(gnus-agent nil)
	(gnus-read-all-available-headers t))
    (setq gnus-newsgroup-headers
	  (gnus-merge
	   'list gnus-newsgroup-headers
	   (gnus-fetch-headers articles nil t)
	   'gnus-article-sort-by-number))
    (setq gnus-newsgroup-articles
	  (gnus-sorted-nunion gnus-newsgroup-articles articles))
    (gnus-summary-limit (append articles gnus-newsgroup-limit))))

(defun gnus-summary-limit-exclude-dormant ()
  "Hide all dormant articles."
  (interactive)
  (prog1
      (gnus-summary-limit-to-marks (list gnus-dormant-mark) 'reverse)
    (gnus-summary-position-point)))

(defun gnus-summary-limit-exclude-childless-dormant ()
  "Hide all dormant articles that have no children."
  (interactive)
  (let ((data (gnus-data-list t))
	articles d children)
    ;; Find all articles that are either not dormant or have
    ;; children.
    (while (setq d (pop data))
      (when (or (not (= (gnus-data-mark d) gnus-dormant-mark))
		(and (setq children
			   (gnus-article-children (gnus-data-number d)))
		     (let (found)
		       (while children
			 (when (memq (car children) articles)
			   (setq children nil
				 found t))
			 (pop children))
		       found)))
	(push (gnus-data-number d) articles)))
    ;; Do the limiting.
    (prog1
	(gnus-summary-limit articles)
      (gnus-summary-position-point))))

(defun gnus-summary-limit-mark-excluded-as-read (&optional all)
  "Mark all unread excluded articles as read.
If ALL, mark even excluded ticked and dormants as read."
  (interactive "P")
  (setq gnus-newsgroup-limit (sort gnus-newsgroup-limit '<))
  (let ((articles (gnus-sorted-ndifference
		   (sort
		    (mapcar (lambda (h) (mail-header-number h))
			    gnus-newsgroup-headers)
		    '<)
		   gnus-newsgroup-limit))
	article)
    (setq gnus-newsgroup-unreads
	  (gnus-sorted-intersection gnus-newsgroup-unreads
				    gnus-newsgroup-limit))
    (if all
	(setq gnus-newsgroup-dormant nil
	      gnus-newsgroup-marked nil
	      gnus-newsgroup-reads
	      (nconc
	       (mapcar (lambda (n) (cons n gnus-catchup-mark)) articles)
	       gnus-newsgroup-reads))
      (while (setq article (pop articles))
	(unless (or (memq article gnus-newsgroup-dormant)
		    (memq article gnus-newsgroup-marked))
	  (push (cons article gnus-catchup-mark) gnus-newsgroup-reads))))))

(defun gnus-summary-limit (articles &optional pop)
  (if pop
      ;; We pop the previous limit off the stack and use that.
      (setq articles (car gnus-newsgroup-limits)
	    gnus-newsgroup-limits (cdr gnus-newsgroup-limits))
    ;; We use the new limit, so we push the old limit on the stack.
    (push gnus-newsgroup-limit gnus-newsgroup-limits))
  ;; Set the limit.
  (setq gnus-newsgroup-limit articles)
  (let ((total (length gnus-newsgroup-data))
	(data (gnus-data-find-list (gnus-summary-article-number)))
	(gnus-summary-mark-below nil)	; Inhibit this.
	found)
    ;; This will do all the work of generating the new summary buffer
    ;; according to the new limit.
    (gnus-summary-prepare)
    ;; Hide any threads, possibly.
    (gnus-summary-maybe-hide-threads)
    ;; Try to return to the article you were at, or one in the
    ;; neighborhood.
    (when data
      ;; We try to find some article after the current one.
      (while data
	(when (gnus-summary-goto-subject (gnus-data-number (car data)) nil t)
	  (setq data nil
		found t))
	(setq data (cdr data))))
    (unless found
      ;; If there is no data, that means that we were after the last
      ;; article.  The same goes when we can't find any articles
      ;; after the current one.
      (goto-char (point-max))
      (gnus-summary-find-prev))
    (gnus-set-mode-line 'summary)
    ;; We return how many articles were removed from the summary
    ;; buffer as a result of the new limit.
    (- total (length gnus-newsgroup-data))))

(defsubst gnus-invisible-cut-children (threads)
  (let ((num 0))
    (while threads
      (when (memq (mail-header-number (caar threads)) gnus-newsgroup-limit)
	(incf num))
      (pop threads))
    (< num 2)))

(defsubst gnus-cut-thread (thread)
  "Go forwards in the thread until we find an article that we want to display."
  (when (or (eq gnus-fetch-old-headers 'some)
	    (eq gnus-fetch-old-headers 'invisible)
	    (numberp gnus-fetch-old-headers)
	    (eq gnus-build-sparse-threads 'some)
	    (eq gnus-build-sparse-threads 'more))
    ;; Deal with old-fetched headers and sparse threads.
    (while (and
	    thread
	    (or
	     (gnus-summary-article-sparse-p (mail-header-number (car thread)))
	     (gnus-summary-article-ancient-p
	      (mail-header-number (car thread))))
	    (if (or (<= (length (cdr thread)) 1)
		    (eq gnus-fetch-old-headers 'invisible))
		(setq gnus-newsgroup-limit
		      (delq (mail-header-number (car thread))
			    gnus-newsgroup-limit)
		      thread (cadr thread))
	      (when (gnus-invisible-cut-children (cdr thread))
		(let ((th (cdr thread)))
		  (while th
		    (if (memq (mail-header-number (caar th))
			      gnus-newsgroup-limit)
			(setq thread (car th)
			      th nil)
		      (setq th (cdr th))))))))))
  thread)

(defun gnus-cut-threads (threads)
  "Cut off all uninteresting articles from the beginning of THREADS."
  (when (or (eq gnus-fetch-old-headers 'some)
	    (eq gnus-fetch-old-headers 'invisible)
	    (numberp gnus-fetch-old-headers)
	    (eq gnus-build-sparse-threads 'some)
	    (eq gnus-build-sparse-threads 'more))
    (let ((th threads))
      (while th
	(setcar th (gnus-cut-thread (car th)))
	(setq th (cdr th)))))
  ;; Remove nixed out threads.
  (delq nil threads))

(defun gnus-summary-initial-limit (&optional show-if-empty)
  "Figure out what the initial limit is supposed to be on group entry.
This entails weeding out unwanted dormants, low-scored articles,
fetch-old-headers verbiage, and so on."
  ;; Most groups have nothing to remove.
  (unless (or gnus-inhibit-limiting
	      (and (null gnus-newsgroup-dormant)
		   (eq gnus-newsgroup-display 'gnus-not-ignore)
		   (not (eq gnus-fetch-old-headers 'some))
		   (not (numberp gnus-fetch-old-headers))
		   (not (eq gnus-fetch-old-headers 'invisible))
		   (null gnus-summary-expunge-below)
		   (not (eq gnus-build-sparse-threads 'some))
		   (not (eq gnus-build-sparse-threads 'more))
		   (null gnus-thread-expunge-below)))
    (push gnus-newsgroup-limit gnus-newsgroup-limits)
    (setq gnus-newsgroup-limit nil)
    (mapatoms
     (lambda (node)
       (unless (car (symbol-value node))
	 ;; These threads have no parents -- they are roots.
	 (let ((nodes (cdr (symbol-value node)))
	       thread)
	   (while nodes
	     (if (and gnus-thread-expunge-below
		      (< (gnus-thread-total-score (car nodes))
			 gnus-thread-expunge-below))
		 (gnus-expunge-thread (pop nodes))
	       (setq thread (pop nodes))
	       (gnus-summary-limit-children thread))))))
     gnus-newsgroup-dependencies)
    ;; If this limitation resulted in an empty group, we might
    ;; pop the previous limit and use it instead.
    (when (and (not gnus-newsgroup-limit)
	       show-if-empty)
      (setq gnus-newsgroup-limit (pop gnus-newsgroup-limits)))
    gnus-newsgroup-limit))

(defun gnus-summary-limit-children (thread)
  "Return 1 if this subthread is visible and 0 if it is not."
  ;; First we get the number of visible children to this thread.  This
  ;; is done by recursing down the thread using this function, so this
  ;; will really go down to a leaf article first, before slowly
  ;; working its way up towards the root.
  (when thread
    (let* ((max-lisp-eval-depth (max 5000 max-lisp-eval-depth))
	   (children
	   (if (cdr thread)
	       (apply '+ (mapcar 'gnus-summary-limit-children
				 (cdr thread)))
	     0))
	   (number (mail-header-number (car thread)))
	   score)
      (if (and
	   (not (memq number gnus-newsgroup-marked))
	   (or
	    ;; If this article is dormant and has absolutely no visible
	    ;; children, then this article isn't visible.
	    (and (memq number gnus-newsgroup-dormant)
		 (zerop children))
	    ;; If this is "fetch-old-headered" and there is no
	    ;; visible children, then we don't want this article.
	    (and (or (eq gnus-fetch-old-headers 'some)
		     (numberp gnus-fetch-old-headers))
		 (gnus-summary-article-ancient-p number)
		 (zerop children))
	    ;; If this is "fetch-old-headered" and `invisible', then
	    ;; we don't want this article.
	    (and (eq gnus-fetch-old-headers 'invisible)
		 (gnus-summary-article-ancient-p number))
	    ;; If this is a sparsely inserted article with no children,
	    ;; we don't want it.
	    (and (eq gnus-build-sparse-threads 'some)
		 (gnus-summary-article-sparse-p number)
		 (zerop children))
	    ;; If we use expunging, and this article is really
	    ;; low-scored, then we don't want this article.
	    (when (and gnus-summary-expunge-below
		       (< (setq score
				(or (cdr (assq number gnus-newsgroup-scored))
				    gnus-summary-default-score))
			  gnus-summary-expunge-below))
	      ;; We increase the expunge-tally here, but that has
	      ;; nothing to do with the limits, really.
	      (incf gnus-newsgroup-expunged-tally)
	      ;; We also mark as read here, if that's wanted.
	      (when (and gnus-summary-mark-below
			 (< score gnus-summary-mark-below))
		(setq gnus-newsgroup-unreads
		      (delq number gnus-newsgroup-unreads))
		(if gnus-newsgroup-auto-expire
		    (push number gnus-newsgroup-expirable)
		  (push (cons number gnus-low-score-mark)
			gnus-newsgroup-reads)))
	      t)
	    ;; Do the `display' group parameter.
	    (and gnus-newsgroup-display
		 (let ((gnus-number number))
		   (not (funcall gnus-newsgroup-display))))))
	  ;; Nope, invisible article.
	  0
	;; Ok, this article is to be visible, so we add it to the limit
	;; and return 1.
	(push number gnus-newsgroup-limit)
	1))))

(defun gnus-expunge-thread (thread)
  "Mark all articles in THREAD as read."
  (let* ((number (mail-header-number (car thread))))
    (incf gnus-newsgroup-expunged-tally)
    ;; We also mark as read here, if that's wanted.
    (setq gnus-newsgroup-unreads
	  (delq number gnus-newsgroup-unreads))
    (if gnus-newsgroup-auto-expire
	(push number gnus-newsgroup-expirable)
      (push (cons number gnus-low-score-mark)
	    gnus-newsgroup-reads)))
  ;; Go recursively through all subthreads.
  (mapcar 'gnus-expunge-thread (cdr thread)))

;; Summary article oriented commands

(defun gnus-summary-refer-parent-article (n)
  "Refer parent article N times.
If N is negative, go to ancestor -N instead.
The difference between N and the number of articles fetched is returned."
  (interactive "p")
  (let ((skip 1)
	error header ref)
    (when (not (natnump n))
      (setq skip (abs n)
	    n 1))
    (while (and (> n 0)
		(not error))
      (setq header (gnus-summary-article-header))
      (if (and (eq (mail-header-number header)
		   (cdr gnus-article-current))
	       (equal gnus-newsgroup-name
		      (car gnus-article-current)))
	  ;; If we try to find the parent of the currently
	  ;; displayed article, then we take a look at the actual
	  ;; References header, since this is slightly more
	  ;; reliable than the References field we got from the
	  ;; server.
	  (with-current-buffer gnus-original-article-buffer
	    (nnheader-narrow-to-headers)
	    (unless (setq ref (message-fetch-field "references"))
	      (when (setq ref (message-fetch-field "in-reply-to"))
		(setq ref (gnus-extract-message-id-from-in-reply-to ref))))
	    (widen))
	(setq ref
	      ;; It's not the current article, so we take a bet on
	      ;; the value we got from the server.
	      (mail-header-references header)))
      (if (and ref
	       (not (equal ref "")))
	  (unless (gnus-summary-refer-article (gnus-parent-id ref skip))
	    (gnus-message 1 "Couldn't find parent"))
	(gnus-message 1 "No references in article %d"
		      (gnus-summary-article-number))
	(setq error t))
      (decf n))
    (gnus-summary-position-point)
    n))

(defun gnus-summary-refer-references ()
  "Fetch all articles mentioned in the References header.
Return the number of articles fetched."
  (interactive)
  (let ((ref (mail-header-references (gnus-summary-article-header)))
	(current (gnus-summary-article-number))
	(n 0))
    (if (or (not ref)
	    (equal ref ""))
	(error "No References in the current article")
      ;; For each Message-ID in the References header...
      (while (string-match "<[^>]*>" ref)
	(incf n)
	;; ... fetch that article.
	(gnus-summary-refer-article
	 (prog1 (match-string 0 ref)
	   (setq ref (substring ref (match-end 0))))))
      (gnus-summary-goto-subject current)
      (gnus-summary-position-point)
      n)))

(defun gnus-delete-duplicate-headers (headers)
  ;; First remove leading duplicates.
  (while (and (> (length headers) 1)
	      (= (mail-header-number (car headers))
		 (mail-header-number (cadr headers))))
    (pop headers))
  ;; Then the rest.
  (let ((result headers))
    (while (> (length headers) 1)
      (if (= (mail-header-number (car headers))
	     (mail-header-number (cadr headers)))
	  (setcdr headers (cddr headers))
	(pop headers)))
    result))

(defun gnus-summary-refer-thread (&optional limit)
  "Fetch all articles in the current thread. For backends that
know how to search for threads (currently only 'nnimap) a
non-numeric prefix arg will use nnir to search the entire
server; without a prefix arg only the current group is
searched. If the variable `gnus-refer-thread-use-nnir' is
non-nil the prefix arg has the reverse meaning. If no
backend-specific 'request-thread function is available fetch
LIMIT (the numerical prefix) old headers. If LIMIT is
non-numeric or nil fetch the number specified by the
`gnus-refer-thread-limit' variable."
  (interactive "P")
  (gnus-warp-to-article)
  (let* ((header (gnus-summary-article-header))
	 (id (mail-header-id header))
	 (gnus-inhibit-demon t)
	 (gnus-summary-ignore-duplicates t)
	 (gnus-read-all-available-headers t)
	 (gnus-refer-thread-use-nnir
	  (if (and (not (null limit)) (listp limit))
	      (not gnus-refer-thread-use-nnir) gnus-refer-thread-use-nnir))
	 (new-headers
	  (if (gnus-check-backend-function
	       'request-thread gnus-newsgroup-name)
	      (gnus-request-thread header gnus-newsgroup-name)
	    (let* ((limit (if (numberp limit) (prefix-numeric-value limit)
			    gnus-refer-thread-limit))
		   (last (if (numberp limit)
			     (min (+ (mail-header-number header)
				     limit)
				  gnus-newsgroup-highest)
			   gnus-newsgroup-highest))
		   (subject (gnus-simplify-subject
			     (mail-header-subject header)))
		   (refs (split-string (or (mail-header-references header)
					   "")))
		   (gnus-parse-headers-hook
		    `(lambda () (goto-char (point-min))
		      (keep-lines
		       (regexp-opt ',(append refs (list id subject)))))))
	      (gnus-fetch-headers (list last) (if (numberp limit)
						  (* 2 limit) limit) t))))
	 article-ids)
    (when (listp new-headers)
      (dolist (header new-headers)
	(push (mail-header-number header) article-ids)
	(when (member (mail-header-number header) gnus-newsgroup-unselected)
          (push (mail-header-number header) gnus-newsgroup-unreads)
          (setq gnus-newsgroup-unselected
                (delete (mail-header-number header)
			gnus-newsgroup-unselected))))
      (setq gnus-newsgroup-headers
            (gnus-delete-duplicate-headers
             (gnus-merge
              'list gnus-newsgroup-headers new-headers
              'gnus-article-sort-by-number)))
      (setq gnus-newsgroup-articles
      	    (gnus-sorted-nunion gnus-newsgroup-articles (nreverse article-ids)))
      (gnus-summary-limit-include-thread id)))
  (gnus-summary-show-thread))

(defun gnus-summary-refer-article (message-id)
  "Fetch an article specified by MESSAGE-ID."
  (interactive "sMessage-ID: ")
  (gnus-warp-to-article)
  (when (and (stringp message-id)
	     (not (zerop (length message-id))))
    (setq message-id (gnus-replace-in-string message-id " " ""))
    ;; Construct the correct Message-ID if necessary.
    ;; Suggested by tale@pawl.rpi.edu.
    (unless (string-match "^<" message-id)
      (setq message-id (concat "<" message-id)))
    (unless (string-match ">$" message-id)
      (setq message-id (concat message-id ">")))
    ;; People often post MIDs from URLs, so unhex it:
    (unless (string-match "@" message-id)
      (setq message-id (gnus-url-unhex-string message-id)))
    (let* ((header (gnus-id-to-header message-id))
	   (sparse (and header
			(gnus-summary-article-sparse-p
			 (mail-header-number header))
			(memq (mail-header-number header)
			      gnus-newsgroup-limit)))
	   number)
      (cond
       ;; If the article is present in the buffer we just go to it.
       ((and header
	     (or (not (gnus-summary-article-sparse-p
		       (mail-header-number header)))
		 sparse))
	(prog1
	    (gnus-summary-goto-article
	     (mail-header-number header) nil t)
	  (when sparse
	    (gnus-summary-update-article (mail-header-number header)))))
       (t
	;; We fetch the article.
	(catch 'found
	  (dolist (gnus-override-method (gnus-refer-article-methods))
	    (when (and (gnus-check-server gnus-override-method)
		       ;; Fetch the header,
		       (setq number (gnus-summary-insert-subject message-id)))
	      ;; and display the article.
	      (gnus-summary-select-article nil nil nil number)
	      (throw 'found t)))
	  (gnus-message 3 "Couldn't fetch article %s" message-id)))))))

(defun gnus-refer-article-methods ()
  "Return a list of referable methods."
  (cond
   ;; No method, so we default to current and native.
   ((null gnus-refer-article-method)
    (list gnus-current-select-method gnus-select-method))
   ;; Current.
   ((eq 'current gnus-refer-article-method)
    (list gnus-current-select-method))
   ;; List of select methods.
   ((not (and (symbolp (car gnus-refer-article-method))
	      (assq (car gnus-refer-article-method) nnoo-definition-alist)))
    (let (out)
      (dolist (method gnus-refer-article-method)
	(push (if (eq 'current method)
		  gnus-current-select-method
		(if (eq 'nnir (car method))
		    (list
		     'nnir
		     (or (cadr method)
			 (gnus-method-to-server gnus-current-select-method)))
		  method))
	      out))
      (nreverse out)))
   ;; One single select method.
   (t
    (list gnus-refer-article-method))))

(defun gnus-summary-edit-parameters ()
  "Edit the group parameters of the current group."
  (interactive)
  (gnus-group-edit-group gnus-newsgroup-name 'params))

(defun gnus-summary-customize-parameters ()
  "Customize the group parameters of the current group."
  (interactive)
  (gnus-group-customize gnus-newsgroup-name))

(defun gnus-summary-enter-digest-group (&optional force)
  "Enter an nndoc group based on the current article.
If FORCE, force a digest interpretation.  If not, try to guess
what the document format is.

To control what happens when you exit the group, see the
`gnus-auto-select-on-ephemeral-exit' variable."
  (interactive "P")
  (let ((conf gnus-current-window-configuration))
    (save-window-excursion
      (save-excursion
	(let (gnus-article-prepare-hook
	      gnus-display-mime-function
	      gnus-break-pages)
	  (gnus-summary-select-article))))
    (setq gnus-current-window-configuration conf)
    (let* ((name (format "%s-%d"
			 (gnus-group-prefixed-name
			  gnus-newsgroup-name (list 'nndoc ""))
			 (with-current-buffer gnus-summary-buffer
			   gnus-current-article)))
	   (ogroup gnus-newsgroup-name)
	   (params (append (gnus-info-params (gnus-get-info ogroup))
			   (list (cons 'to-group ogroup))
			   (list (cons 'parent-group ogroup))
			   (list (cons 'save-article-group ogroup))))
	   (case-fold-search t)
	   (buf (current-buffer))
	   dig to-address)
      (with-current-buffer gnus-original-article-buffer
	;; Have the digest group inherit the main mail address of
	;; the parent article.
	(when (setq to-address (or (gnus-fetch-field "reply-to")
				   (gnus-fetch-field "from")))
	  (setq params
		(append
		 (list (cons 'to-address
			     (funcall gnus-decode-encoded-address-function
				      to-address))))))
	(setq dig (nnheader-set-temp-buffer " *gnus digest buffer*"))
	(insert-buffer-substring gnus-original-article-buffer)
	;; Remove lines that may lead nndoc to misinterpret the
	;; document type.
	(narrow-to-region
	 (goto-char (point-min))
	 (or (search-forward "\n\n" nil t) (point)))
	(goto-char (point-min))
	(delete-matching-lines "^Path:\\|^From ")
	(widen))
      (unwind-protect
	  (if (let ((gnus-newsgroup-ephemeral-charset gnus-newsgroup-charset)
		    (gnus-newsgroup-ephemeral-ignored-charsets
		     gnus-newsgroup-ignored-charsets))
		(gnus-group-read-ephemeral-group
		 name `(nndoc ,name (nndoc-address ,(get-buffer dig))
			      (nndoc-article-type
			       ,(if force 'mbox 'guess)))
		 t nil nil nil
		 `((adapt-file . ,(gnus-score-file-name gnus-newsgroup-name
							"ADAPT")))))
	      ;; Make all postings to this group go to the parent group.
	      (nconc (gnus-info-params (gnus-get-info name))
		     params)
	    ;; Couldn't select this doc group.
	    (switch-to-buffer buf)
	    (gnus-set-global-variables)
	    (gnus-configure-windows 'summary)
	    (gnus-message 3 "Article couldn't be entered?"))
	(kill-buffer dig)))))

(defun gnus-summary-read-document (n)
  "Open a new group based on the current article(s).
This will allow you to read digests and other similar
documents as newsgroups.
Obeys the standard process/prefix convention."
  (interactive "P")
  (let* ((ogroup gnus-newsgroup-name)
	 (params (append (gnus-info-params (gnus-get-info ogroup))
			 (list (cons 'to-group ogroup))))
	 group egroup groups vgroup)
    (dolist (article (gnus-summary-work-articles n))
      (setq group (format "%s-%d" gnus-newsgroup-name article))
      (gnus-summary-remove-process-mark article)
      (when (gnus-summary-display-article article)
	(save-excursion ;;What for?
	  (with-temp-buffer
	    (insert-buffer-substring gnus-original-article-buffer)
	    ;; Remove some headers that may lead nndoc to make
	    ;; the wrong guess.
	    (message-narrow-to-head)
	    (goto-char (point-min))
	    (delete-matching-lines "^Path:\\|^From ")
	    (widen)
	    (if (setq egroup
		      (gnus-group-read-ephemeral-group
		       group `(nndoc ,group (nndoc-address ,(current-buffer))
				     (nndoc-article-type guess))
		       t nil t))
		(progn
                  ;; Make all postings to this group go to the parent group.
		  (nconc (gnus-info-params (gnus-get-info egroup))
			 params)
		  (push egroup groups))
	      ;; Couldn't select this doc group.
	      (gnus-error 3 "Article couldn't be entered"))))))
    ;; Now we have selected all the documents.
    (cond
     ((not groups)
      (error "None of the articles could be interpreted as documents"))
     ((gnus-group-read-ephemeral-group
       (setq vgroup (format
		     "nnvirtual:%s-%s" gnus-newsgroup-name
		     (format-time-string "%Y%m%dT%H%M%S" (current-time))))
       `(nnvirtual ,vgroup (nnvirtual-component-groups ,groups))
       t
       (cons (current-buffer) 'summary)))
     (t
      (error "Couldn't select virtual nndoc group")))))

(defun gnus-summary-widget-forward (arg)
  "Move point to the next field or button in the article.
With optional ARG, move across that many fields."
  (interactive "p")
  (gnus-summary-select-article)
  (gnus-configure-windows 'article)
  (select-window (gnus-get-buffer-window gnus-article-buffer))
  (widget-forward arg))

(defun gnus-summary-isearch-article (&optional regexp-p)
  "Do incremental search forward on the current article.
If REGEXP-P (the prefix) is non-nil, do regexp isearch."
  (interactive "P")
  (gnus-summary-select-article)
  (gnus-configure-windows 'article)
  (gnus-eval-in-buffer-window gnus-article-buffer
    (save-restriction
      (widen)
      (isearch-forward regexp-p))))

(defun gnus-summary-repeat-search-article-forward ()
  "Repeat the previous search forwards."
  (interactive)
  (unless gnus-last-search-regexp
    (error "No previous search"))
  (gnus-summary-search-article-forward gnus-last-search-regexp))

(defun gnus-summary-repeat-search-article-backward ()
  "Repeat the previous search backwards."
  (interactive)
  (unless gnus-last-search-regexp
    (error "No previous search"))
  (gnus-summary-search-article-forward gnus-last-search-regexp t))

(defun gnus-summary-search-article-forward (regexp &optional backward)
  "Search for an article containing REGEXP forward.
If BACKWARD, search backward instead."
  (interactive
   (list (read-string
	  (format "Search article %s (regexp%s): "
		  (if current-prefix-arg "backward" "forward")
		  (if gnus-last-search-regexp
		      (concat ", default " gnus-last-search-regexp)
		    "")))
	 current-prefix-arg))
  (if (string-equal regexp "")
      (setq regexp (or gnus-last-search-regexp ""))
    (setq gnus-last-search-regexp regexp)
    (setq gnus-article-before-search gnus-current-article))
  ;; Intentionally set gnus-last-article.
  (setq gnus-last-article gnus-article-before-search)
  (let ((gnus-last-article gnus-last-article))
    (if (gnus-summary-search-article regexp backward)
	(gnus-summary-show-thread)
      (signal 'search-failed (list regexp)))))

(defun gnus-summary-search-article-backward (regexp)
  "Search for an article containing REGEXP backward."
  (interactive
   (list (read-string
	  (format "Search article backward (regexp%s): "
		  (if gnus-last-search-regexp
		      (concat ", default " gnus-last-search-regexp)
		    "")))))
  (gnus-summary-search-article-forward regexp 'backward))

(defun gnus-summary-search-article (regexp &optional backward)
  "Search for an article containing REGEXP.
Optional argument BACKWARD means do search for backward.
`gnus-select-article-hook' is not called during the search."
  ;; We have to require this here to make sure that the following
  ;; dynamic binding isn't shadowed by autoloading.
  (require 'gnus-async)
  (require 'gnus-art)
  (let ((gnus-select-article-hook nil)	;Disable hook.
	(gnus-article-prepare-hook nil)
	(gnus-mark-article-hook nil)	;Inhibit marking as read.
	(gnus-use-article-prefetch nil)
	(gnus-xmas-force-redisplay nil)	;Inhibit XEmacs redisplay.
	(gnus-use-trees nil)		;Inhibit updating tree buffer.
	(gnus-visual nil)
	(gnus-keep-backlog nil)
	(gnus-break-pages nil)
	(gnus-summary-display-arrow nil)
	(gnus-updated-mode-lines nil)
	(gnus-auto-center-summary nil)
	(sum (current-buffer))
	(gnus-display-mime-function nil)
	(found nil)
	point)
    (gnus-save-hidden-threads
      (gnus-summary-select-article)
      (set-buffer gnus-article-buffer)
      (goto-char (window-point (get-buffer-window (current-buffer))))
      (when backward
	(forward-line -1))
      (while (not found)
	(gnus-message 7 "Searching article: %d..." (cdr gnus-article-current))
	(if (if backward
		(re-search-backward regexp nil t)
	      (re-search-forward regexp nil t))
	    ;; We found the regexp.
	    (progn
	      (setq found 'found)
	      (beginning-of-line)
	      (set-window-start
	       (get-buffer-window (current-buffer))
	       (point))
	      (forward-line 1)
	      (set-window-point
	       (get-buffer-window (current-buffer))
	       (point))
	      (set-buffer sum)
	      (setq point (point)))
	  ;; We didn't find it, so we go to the next article.
	  (set-buffer sum)
	  (setq found 'not)
	  (while (eq found 'not)
	    (if (not (if backward (gnus-summary-find-prev)
		       (gnus-summary-find-next)))
		;; No more articles.
		(setq found t)
	      ;; Select the next article and adjust point.
	      (unless (gnus-summary-article-sparse-p
		       (gnus-summary-article-number))
		(setq found nil)
		(gnus-summary-select-article)
		(set-buffer gnus-article-buffer)
		(widen)
		(goto-char (if backward (point-max) (point-min))))))))
      (gnus-message 7 ""))
    ;; Return whether we found the regexp.
    (when (eq found 'found)
      (goto-char point)
      (gnus-summary-show-thread)
      (gnus-summary-goto-subject gnus-current-article)
      (gnus-summary-position-point)
      t)))

(defun gnus-find-matching-articles (header regexp)
  "Return a list of all articles that match REGEXP on HEADER.
This search includes all articles in the current group that Gnus has
fetched headers for, whether they are displayed or not."
  (let ((articles nil)
	;; Can't eta-reduce because it's a macro.
	(func `(lambda (h) (,(intern (concat "mail-header-" header)) h)))
	(case-fold-search t))
    (dolist (header gnus-newsgroup-headers)
      (when (string-match regexp (funcall func header))
	(push (mail-header-number header) articles)))
    (nreverse articles)))

(defun gnus-summary-find-matching (header regexp &optional backward unread
					  not-case-fold not-matching)
  "Return a list of all articles that match REGEXP on HEADER.
The search stars on the current article and goes forwards unless
BACKWARD is non-nil.  If BACKWARD is `all', do all articles.
If UNREAD is non-nil, only unread articles will
be taken into consideration.  If NOT-CASE-FOLD, case won't be folded
in the comparisons. If NOT-MATCHING, return a list of all articles that
not match REGEXP on HEADER."
  (let ((case-fold-search (not not-case-fold))
	articles d func)
    (if (consp header)
	(if (eq (car header) 'extra)
	    (setq func
		  `(lambda (h)
		     (or (cdr (assq ',(cdr header) (mail-header-extra h)))
			 "")))
	  (error "%s is an invalid header" header))
      (unless (fboundp (intern (concat "mail-header-" header)))
	(error "%s is not a valid header" header))
      (setq func `(lambda (h) (,(intern (concat "mail-header-" header)) h))))
    (dolist (d (if (eq backward 'all)
		   gnus-newsgroup-data
		 (gnus-data-find-list
		  (gnus-summary-article-number)
		  (gnus-data-list backward))))
      (when (and (or (not unread)	; We want all articles...
		     (gnus-data-unread-p d)) ; Or just unreads.
		 (vectorp (gnus-data-header d)) ; It's not a pseudo.
		 (if not-matching
		     (not (string-match
			   regexp
			   (funcall func (gnus-data-header d))))
		   (string-match regexp
				 (funcall func (gnus-data-header d)))))
	(push (gnus-data-number d) articles))) ; Success!
    (nreverse articles)))

(defun gnus-summary-execute-command (header regexp command &optional backward)
  "Search forward for an article whose HEADER matches REGEXP and execute COMMAND.
If HEADER is an empty string (or nil), the match is done on the entire
article.  If BACKWARD (the prefix) is non-nil, search backward instead."
  (interactive
   (list (let ((completion-ignore-case t))
	   (gnus-completing-read
	    "Header name"
	    (mapcar 'symbol-name
		    (append
		     '(Number Subject From Lines Date
		       Message-ID Xref References Body)
		     gnus-extra-headers))
	    'require-match))
	 (read-string "Regexp: ")
	 (read-key-sequence "Command: ")
	 current-prefix-arg))
  (when (equal header "Body")
    (setq header ""))
  ;; Hidden thread subtrees must be searched as well.
  (gnus-summary-show-all-threads)
  ;; We don't want to change current point nor window configuration.
  (save-excursion
    (save-window-excursion
      (let (gnus-visual
	    gnus-treat-strip-trailing-blank-lines
	    gnus-treat-strip-leading-blank-lines
	    gnus-treat-strip-multiple-blank-lines
	    gnus-treat-hide-boring-headers
	    gnus-treat-fold-newsgroups
	    gnus-article-prepare-hook)
	(gnus-message 6 "Executing %s..." (key-description command))
	;; We'd like to execute COMMAND interactively so as to give arguments.
	(gnus-execute header regexp
		      `(call-interactively ',(key-binding command))
		      backward)
	(gnus-message 6 "Executing %s...done" (key-description command))))))

(defun gnus-summary-beginning-of-article ()
  "Scroll the article back to the beginning."
  (interactive)
  (gnus-summary-select-article)
  (gnus-configure-windows 'article)
  (gnus-eval-in-buffer-window gnus-article-buffer
    (widen)
    (goto-char (point-min))
    (when gnus-break-pages
      (gnus-narrow-to-page))))

(defun gnus-summary-end-of-article ()
  "Scroll to the end of the article."
  (interactive)
  (gnus-summary-select-article)
  (gnus-configure-windows 'article)
  (gnus-eval-in-buffer-window gnus-article-buffer
    (widen)
    (goto-char (point-max))
    (recenter -3)
    (when gnus-break-pages
      (gnus-narrow-to-page))))

(defun gnus-summary-print-truncate-and-quote (string &optional len)
  "Truncate to LEN and quote all \"(\"'s in STRING."
  (gnus-replace-in-string (if (and len (> (length string) len))
			      (substring string 0 len)
			    string)
			  "[()]" "\\\\\\&"))

(defun gnus-summary-print-article (&optional filename n)
  "Generate and print a PostScript image of the process-marked (mail) articles.

If used interactively, print the current article if none are
process-marked.  With prefix arg, prompt the user for the name of the
file to save in.

When used from Lisp, accept two optional args FILENAME and N.  N means
to print the next N articles.  If N is negative, print the N previous
articles.  If N is nil and articles have been marked with the process
mark, print these instead.

If the optional first argument FILENAME is nil, send the image to the
printer.  If FILENAME is a string, save the PostScript image in a file with
that name.  If FILENAME is a number, prompt the user for the name of the file
to save in."
  (interactive (list (ps-print-preprint current-prefix-arg)))
  (dolist (article (gnus-summary-work-articles n))
    (gnus-summary-select-article nil nil 'pseudo article)
    (gnus-eval-in-buffer-window gnus-article-buffer
      (gnus-print-buffer))
    (gnus-summary-remove-process-mark article))
  (ps-despool filename))

(defun gnus-print-buffer ()
  (let ((ps-left-header
	 (list
	  (concat "("
		  (gnus-summary-print-truncate-and-quote
		   (mail-header-subject gnus-current-headers)
		   66) ")")
	  (concat "("
		  (gnus-summary-print-truncate-and-quote
		   (mail-header-from gnus-current-headers)
		   45) ")")))
	(ps-right-header
	 (list
	  "/pagenumberstring load"
	  (concat "("
		  (mail-header-date gnus-current-headers) ")"))))
    (gnus-run-hooks 'gnus-ps-print-hook)
    (save-excursion
      (if ps-print-color-p
	  (ps-spool-buffer-with-faces)
	(ps-spool-buffer)))))

(defun gnus-summary-show-complete-article ()
  "Show a complete version of the current article.
This is only useful if you're looking at a partial version of the
article currently."
  (interactive)
  (let ((gnus-keep-backlog nil)
	(gnus-use-cache nil)
	(gnus-agent nil)
	(variable (intern
		   (format "%s-fetch-partial-articles"
			   (car (gnus-find-method-for-group
				 gnus-newsgroup-name)))
		   obarray))
	old-val)
    (unwind-protect
	(progn
	  (setq old-val (symbol-value variable))
	  (set variable nil)
	  (gnus-flush-original-article-buffer)
	  (gnus-summary-show-article))
      (set variable old-val))))

(defun gnus-summary-show-article (&optional arg)
  "Force redisplaying of the current article.
If ARG (the prefix) is a number, show the article with the charset
defined in `gnus-summary-show-article-charset-alist', or the charset
input.
If ARG (the prefix) is non-nil and not a number, show the article,
but without running any of the article treatment functions
article.  Normally, the keystroke is `C-u g'.  When using `C-u
C-u g', show the raw article."
  (interactive "P")
  (cond
   ((numberp arg)
    (gnus-summary-show-article t)
    (let ((gnus-newsgroup-charset
	   (or (cdr (assq arg gnus-summary-show-article-charset-alist))
	       (mm-read-coding-system
		"View as charset: " ;; actually it is coding system.
		(with-current-buffer gnus-article-buffer
		  (mm-detect-coding-region (point) (point-max))))))
	  (gnus-newsgroup-ignored-charsets 'gnus-all))
      (gnus-summary-select-article nil 'force)
      (let ((deps gnus-newsgroup-dependencies)
	    head header lines)
	(with-current-buffer gnus-original-article-buffer
	  (save-restriction
	    (message-narrow-to-head)
	    (setq head (buffer-string))
	    (goto-char (point-min))
	    (unless (re-search-forward "^lines:[ \t]\\([0-9]+\\)" nil t)
	      (goto-char (point-max))
	      (widen)
	      (setq lines (1- (count-lines (point) (point-max))))))
	  (with-temp-buffer
	    (insert (format "211 %d Article retrieved.\n"
			    (cdr gnus-article-current)))
	    (insert head)
	    (if lines (insert (format "Lines: %d\n" lines)))
	    (insert ".\n")
	    (let ((nntp-server-buffer (current-buffer)))
	      (setq header (car (gnus-get-newsgroup-headers deps t))))))
	(gnus-data-set-header
	 (gnus-data-find (cdr gnus-article-current))
	 header)
	(gnus-summary-update-article-line
	 (cdr gnus-article-current) header)
	(when (gnus-summary-goto-subject (cdr gnus-article-current) nil t)
	  (gnus-summary-update-secondary-mark (cdr gnus-article-current))))))
   ((not arg)
    ;; Select the article the normal way.
    (if (eq mm-text-html-renderer 'shr)
	(progn
	  (require 'shr)
	  (let ((shr-ignore-cache t))
	    (gnus-summary-select-article nil 'force)))
      (gnus-summary-select-article nil 'force)))
   ((equal arg '(16))
    ;; C-u C-u g
    (let ((gnus-inhibit-article-treatments t))
      (gnus-summary-select-article nil 'force)))
   (t
    ;; We have to require this here to make sure that the following
    ;; dynamic binding isn't shadowed by autoloading.
    (require 'gnus-async)
    (require 'gnus-art)
    ;; Bind the article treatment functions to nil.
    (let ((gnus-have-all-headers t)
	  gnus-article-prepare-hook
	  gnus-article-decode-hook
	  gnus-display-mime-function
	  gnus-break-pages)
      ;; Destroy any MIME parts.
      (when (gnus-buffer-live-p gnus-article-buffer)
	(with-current-buffer gnus-article-buffer
	  (gnus-article-stop-animations)
	  (gnus-stop-downloads)
	  (mm-destroy-parts gnus-article-mime-handles)
	  ;; Set it to nil for safety reason.
	  (setq gnus-article-mime-handle-alist nil)
	  (setq gnus-article-mime-handles nil)))
      (gnus-summary-select-article nil 'force))))
  (gnus-summary-goto-subject gnus-current-article)
  (gnus-summary-position-point))

(defun gnus-summary-show-raw-article ()
  "Show the raw article without any article massaging functions being run."
  (interactive)
  (gnus-summary-show-article t))

(defun gnus-summary-verbose-headers (&optional arg)
  "Toggle permanent full header display.
If ARG is a positive number, turn header display on.
If ARG is a negative number, turn header display off."
  (interactive "P")
  (setq gnus-show-all-headers
	(cond ((or (not (numberp arg))
		   (zerop arg))
	       (not gnus-show-all-headers))
	      ((natnump arg)
	       t)))
  (gnus-summary-show-article))

(defun gnus-summary-toggle-header (&optional arg)
  "Show the headers if they are hidden, or hide them if they are shown.
If ARG is a positive number, show the entire header.
If ARG is a negative number, hide the unwanted header lines."
  (interactive "P")
  (let ((window (and (gnus-buffer-live-p gnus-article-buffer)
		     (get-buffer-window gnus-article-buffer t))))
    (with-current-buffer gnus-article-buffer
      (widen)
      (article-narrow-to-head)
      (let* ((inhibit-read-only t)
	     (inhibit-point-motion-hooks t)
	     (hidden (if (numberp arg)
			 (>= arg 0)
		       (or (not (looking-at "[^ \t\n]+:"))
			   (gnus-article-hidden-text-p 'headers))))
	     s e)
	(delete-region (point-min) (point-max))
	(with-current-buffer gnus-original-article-buffer
	  (goto-char (setq s (point-min)))
	  (setq e (if (search-forward "\n\n" nil t)
		      (1- (point))
		    (point-max))))
	(insert-buffer-substring gnus-original-article-buffer s e)
	(run-hooks 'gnus-article-decode-hook)
	(if hidden
	    (let ((gnus-treat-hide-headers nil)
		  (gnus-treat-hide-boring-headers nil))
	      (gnus-delete-wash-type 'headers)
	      (gnus-treat-article 'head))
	  (gnus-treat-article 'head))
	(widen)
	(if window
	    (set-window-start window (goto-char (point-min))))
	(if gnus-break-pages
	    (gnus-narrow-to-page)
	  (when (gnus-visual-p 'page-marker)
	    (let ((inhibit-read-only t))
	      (gnus-remove-text-with-property 'gnus-prev)
	      (gnus-remove-text-with-property 'gnus-next))))
	(gnus-set-mode-line 'article)))))

(defun gnus-summary-show-all-headers ()
  "Make all header lines visible."
  (interactive)
  (gnus-summary-toggle-header 1))

(defun gnus-summary-caesar-message (&optional arg)
  "Caesar rotate the current article by 13.
With a non-numerical prefix, also rotate headers.  A numerical
prefix specifies how many places to rotate each letter forward."
  (interactive "P")
  (gnus-summary-select-article)
  (let ((mail-header-separator ""))
    (gnus-eval-in-buffer-window gnus-article-buffer
      (save-restriction
	(widen)
	(let ((start (window-start))
	      (inhibit-read-only t))
	  (if (equal arg '(4))
	      (message-caesar-buffer-body nil t)
	    (message-caesar-buffer-body arg))
	  (set-window-start (get-buffer-window (current-buffer)) start)))))
  ;; Create buttons and stuff...
  (gnus-treat-article nil))

(declare-function idna-to-unicode "ext:idna" (str))

(defun gnus-summary-idna-message (&optional arg)
  "Decode IDNA encoded domain names in the current articles.
IDNA encoded domain names looks like `xn--bar'.  If a string
remain unencoded after running this function, it is likely an
invalid IDNA string (`xn--bar' is invalid).

You must have GNU Libidn (URL `http://www.gnu.org/software/libidn/')
installed for this command to work."
  (interactive "P")
  (if (not (and (condition-case nil (require 'idna)
		  (file-error))
		(mm-coding-system-p 'utf-8)
		(executable-find (symbol-value 'idna-program))))
      (gnus-message
       5 "GNU Libidn not installed properly (`idn' or `idna.el' missing)")
    (gnus-summary-select-article)
    (let ((mail-header-separator ""))
      (gnus-eval-in-buffer-window gnus-article-buffer
	(save-restriction
	  (widen)
	  (let ((start (window-start))
		buffer-read-only)
	    (while (re-search-forward "\\(xn--[-0-9a-z]+\\)" nil t)
	      (replace-match (idna-to-unicode (match-string 1))))
	    (set-window-start (get-buffer-window (current-buffer)) start)))))))

(defun gnus-summary-morse-message (&optional arg)
  "Morse decode the current article."
  (interactive "P")
  (gnus-summary-select-article)
  (let ((mail-header-separator ""))
    (gnus-eval-in-buffer-window gnus-article-buffer
      (save-excursion
	(save-restriction
	  (widen)
	  (let ((pos (window-start))
		(inhibit-read-only t))
	    (goto-char (point-min))
	    (when (message-goto-body)
	      (gnus-narrow-to-body))
	    (goto-char (point-min))
	    (while (search-forward "·" (point-max) t)
	      (replace-match "."))
	    (unmorse-region (point-min) (point-max))
	    (widen)
	    (set-window-start (get-buffer-window (current-buffer)) pos)))))))

(defun gnus-summary-stop-page-breaking ()
  "Stop page breaking in the current article."
  (interactive)
  (gnus-summary-select-article)
  (gnus-eval-in-buffer-window gnus-article-buffer
    (widen)
    (when (gnus-visual-p 'page-marker)
      (let ((inhibit-read-only t))
	(gnus-remove-text-with-property 'gnus-prev)
	(gnus-remove-text-with-property 'gnus-next))
      (setq gnus-page-broken nil))))

(defun gnus-summary-move-article (&optional n to-newsgroup
					    select-method action)
  "Move the current article to a different newsgroup.
If N is a positive number, move the N next articles.
If N is a negative number, move the N previous articles.
If N is nil and any articles have been marked with the process mark,
move those articles instead.
If TO-NEWSGROUP is string, do not prompt for a newsgroup to move to.
If SELECT-METHOD is non-nil, do not move to a specific newsgroup, but
re-spool using this method.

When called interactively with TO-NEWSGROUP being nil, the value of
the variable `gnus-move-split-methods' is used for finding a default
for the target newsgroup.

For this function to work, both the current newsgroup and the
newsgroup that you want to move to have to support the `request-move'
and `request-accept' functions.

ACTION can be either `move' (the default), `crosspost' or `copy'."
  (interactive "P")
  (unless action
    (setq action 'move))
  ;; Check whether the source group supports the required functions.
  (cond ((and (eq action 'move)
	      (not (gnus-check-backend-function
		    'request-move-article gnus-newsgroup-name)))
	 (error "The current group does not support article moving"))
	((and (eq action 'crosspost)
	      (not (gnus-check-backend-function
		    'request-replace-article gnus-newsgroup-name)))
	 (error "The current group does not support article editing")))
  (let ((articles (gnus-summary-work-articles n))
	(prefix (if (gnus-check-backend-function
		     'request-move-article gnus-newsgroup-name)
		    (funcall gnus-move-group-prefix-function
			     gnus-newsgroup-name)
		  ""))
	(names '((move "Move" "Moving")
		 (copy "Copy" "Copying")
		 (crosspost "Crosspost" "Crossposting")))
	(copy-buf (save-excursion
		    (nnheader-set-temp-buffer " *copy article*")))
	art-group to-method new-xref article to-groups
	articles-to-update-marks encoded)
    (unless (assq action names)
      (error "Unknown action %s" action))
    ;; Read the newsgroup name.
    (when (and (not to-newsgroup)
	       (not select-method))
      (if (and gnus-move-split-methods
	       (not
		(and (memq gnus-current-article articles)
		     (gnus-buffer-live-p gnus-original-article-buffer))))
	  ;; When `gnus-move-split-methods' is non-nil, we have to
	  ;; select an article to give `gnus-read-move-group-name' an
	  ;; opportunity to suggest an appropriate default.  However,
	  ;; we needn't render or mark the article.
	  (let ((gnus-display-mime-function nil)
		(gnus-article-prepare-hook nil)
		(gnus-mark-article-hook nil))
	    (gnus-summary-select-article nil nil nil (car articles))))
      (setq to-newsgroup (gnus-read-move-group-name
			  (cadr (assq action names))
			  (symbol-value
			   (intern (format "gnus-current-%s-group" action)))
			  articles prefix)
	    encoded to-newsgroup
	    to-method (gnus-server-to-method (gnus-group-method to-newsgroup)))
      (set (intern (format "gnus-current-%s-group" action))
	   (mm-decode-coding-string
	    to-newsgroup
	    (gnus-group-name-charset to-method to-newsgroup))))
    (unless to-method
      (setq to-method (or select-method
			  (gnus-server-to-method
			   (gnus-group-method to-newsgroup)))))
    (setq to-newsgroup
	  (or encoded
	      (and to-newsgroup
		   (mm-encode-coding-string
		    to-newsgroup
		    (gnus-group-name-charset to-method to-newsgroup)))))
    ;; Check the method we are to move this article to...
    (unless (gnus-check-backend-function
	     'request-accept-article (car to-method))
      (error "%s does not support article copying" (car to-method)))
    (unless (gnus-check-server to-method)
      (error "Can't open server %s" (car to-method)))
    (gnus-message 6 "%s to %s: %s..."
		  (caddr (assq action names))
		  (or (car select-method)
		      (gnus-group-decoded-name to-newsgroup))
		  articles)
    (while articles
      (setq article (pop articles))
      ;; Set any marks that may have changed in the summary buffer.
      (when gnus-preserve-marks
	(gnus-summary-push-marks-to-backend article))
      (setq
       art-group
       (cond
	;; Move the article.
	((eq action 'move)
	 ;; Remove this article from future suppression.
	 (gnus-dup-unsuppress-article article)
	 (let* ((from-method (gnus-find-method-for-group
			      gnus-newsgroup-name))
		(to-method (or select-method
			       (gnus-find-method-for-group to-newsgroup)))
		(move-is-internal (gnus-server-equal from-method to-method)))
	   (gnus-request-move-article
	    article			; Article to move
	    gnus-newsgroup-name         ; From newsgroup
	    (nth 1 (gnus-find-method-for-group
		    gnus-newsgroup-name)) ; Server
	    (list 'gnus-request-accept-article
		  to-newsgroup (list 'quote select-method)
		  (not articles) t)	; Accept form
	    (not articles)		; Only save nov last time
	    (and move-is-internal
		 to-newsgroup		; Not respooling
					; Is this move internal?
		 (gnus-group-real-name to-newsgroup)))))
	;; Copy the article.
	((eq action 'copy)
	 (with-current-buffer copy-buf
	   (when (gnus-request-article-this-buffer article
						   gnus-newsgroup-name)
	     (save-restriction
	       (nnheader-narrow-to-headers)
	       (dolist (hdr gnus-copy-article-ignored-headers)
		 (message-remove-header hdr t)))
	     (gnus-request-accept-article
	      to-newsgroup select-method (not articles) t))))
	;; Crosspost the article.
	((eq action 'crosspost)
	 (let ((xref (message-tokenize-header
		      (mail-header-xref (gnus-summary-article-header
					 article))
		      " ")))
	   (setq new-xref (concat (gnus-group-real-name gnus-newsgroup-name)
				  ":" (number-to-string article)))
	   (unless xref
	     (setq xref (list (system-name))))
	   (setq new-xref
		 (concat
		  (mapconcat 'identity
			     (delete "Xref:" (delete new-xref xref))
			     " ")
		  " " new-xref))
	   (with-current-buffer copy-buf
	     ;; First put the article in the destination group.
	     (gnus-request-article-this-buffer article gnus-newsgroup-name)
	     (when (consp (setq art-group
				(gnus-request-accept-article
				 to-newsgroup select-method (not articles)
				 t)))
	       (setq new-xref (concat new-xref " " (car art-group)
				      ":"
				      (number-to-string (cdr art-group))))
	       ;; Now we have the new Xrefs header, so we insert
	       ;; it and replace the new article.
	       (nnheader-replace-header "Xref" new-xref)
	       (gnus-request-replace-article
		(cdr art-group) to-newsgroup (current-buffer) t)
	       art-group))))))
      (cond
       ((not art-group)
	(gnus-message 1 "Couldn't %s article %s: %s"
		      (cadr (assq action names)) article
		      (nnheader-get-report (car to-method))))
       ((eq art-group 'junk)
	(when (eq action 'move)
	  (gnus-summary-mark-article article gnus-canceled-mark)
	  (gnus-message 4 "Deleted article %s" article)
	  ;; run the delete hook
	  (run-hook-with-args 'gnus-summary-article-delete-hook
			      action
			      (gnus-data-header
			       (assoc article (gnus-data-list nil)))
			      gnus-newsgroup-name nil
			      select-method)))
       (t
	(let* ((pto-group (gnus-group-prefixed-name
			   (car art-group) to-method))
	       (info (gnus-get-info pto-group))
	       (to-group (gnus-info-group info))
	       to-marks)
	  ;; Update the group that has been moved to.
	  (when (and info
		     (memq action '(move copy)))
	    (unless (member to-group to-groups)
	      (push to-group to-groups))

	    (when (and (not (memq article gnus-newsgroup-unreads))
		       (cdr art-group))
	      (push 'read to-marks)
	      (gnus-info-set-read
	       info (gnus-add-to-range (gnus-info-read info)
				       (list (cdr art-group)))))

	    ;; See whether the article is to be put in the cache.
	    (let* ((expirable (gnus-group-auto-expirable-p to-group))
		   (marks (if expirable
			      gnus-article-mark-lists
			    (delete '(expirable . expire)
				    (copy-sequence
				     gnus-article-mark-lists))))
		   (to-article (cdr art-group)))

	      ;; Enter the article into the cache in the new group,
	      ;; if that is required.
	      (when (and to-article
			 gnus-use-cache)
		(gnus-cache-possibly-enter-article
		 to-group to-article
		 (memq article gnus-newsgroup-marked)
		 (memq article gnus-newsgroup-dormant)
		 (memq article gnus-newsgroup-unreads)))

	      (when (and gnus-preserve-marks
			 to-article)
		;; Copy any marks over to the new group.
		(when (and (equal to-group gnus-newsgroup-name)
			   (not (memq article gnus-newsgroup-unreads)))
		  ;; Mark this article as read in this group.
		  (push (cons to-article gnus-read-mark)
			gnus-newsgroup-reads)
		  ;; Increase the active status of this group.
		  (setcdr (gnus-active to-group) to-article)
		  (setcdr gnus-newsgroup-active to-article))

		(while marks
		  (when (eq (gnus-article-mark-to-type (cdar marks)) 'list)
		    (when (memq article (symbol-value
					 (intern (format "gnus-newsgroup-%s"
							 (caar marks)))))
		      (push (cdar marks) to-marks)
		      ;; If the other group is the same as this group,
		      ;; then we have to add the mark to the list.
		      (when (equal to-group gnus-newsgroup-name)
			(set (intern (format "gnus-newsgroup-%s"
					     (caar marks)))
			     (cons to-article
				   (symbol-value
				    (intern (format "gnus-newsgroup-%s"
						    (caar marks)))))))
		      ;; Copy the marks to other group.
		      (gnus-add-marked-articles
		       to-group (cdar marks) (list to-article) info)))
		  (setq marks (cdr marks)))

		(when (and expirable
			   gnus-mark-copied-or-moved-articles-as-expirable
			   (not (memq 'expire to-marks)))
		  ;; Mark this article as expirable.
		  (push 'expire to-marks)
		  (when (equal to-group gnus-newsgroup-name)
		    (push to-article gnus-newsgroup-expirable))
		  ;; Copy the expirable mark to other group.
		  (gnus-add-marked-articles
		   to-group 'expire (list to-article) info))

		(when (and to-marks
			   (or gnus-propagate-marks
			       (gnus-method-option-p
				(gnus-find-method-for-group to-group)
				'server-marks)))
		  (gnus-request-set-mark
		   to-group (list (list (list to-article) 'add to-marks)))))

	      (gnus-dribble-enter
	       (concat "(gnus-group-set-info '"
		       (gnus-prin1-to-string (gnus-get-info to-group))
		       ")")
	       (concat "^(gnus-group-set-info '(\""
		       (regexp-quote to-group) "\""))))

	  ;; Update the Xref header in this article to point to
	  ;; the new crossposted article we have just created.
	  (when (eq action 'crosspost)
	    (with-current-buffer copy-buf
	      (gnus-request-article-this-buffer article gnus-newsgroup-name)
	      (nnheader-replace-header "Xref" new-xref)
	      (gnus-request-replace-article
	       article gnus-newsgroup-name (current-buffer) t)))

	  ;; run the move/copy/crosspost/respool hook
	  (run-hook-with-args 'gnus-summary-article-move-hook
			      action
			      (gnus-data-header
			       (assoc article (gnus-data-list nil)))
			      gnus-newsgroup-name
			      to-newsgroup
			      select-method))

        ;;;!!!Why is this necessary?
	(set-buffer gnus-summary-buffer)

	(when (eq action 'move)
	  (save-excursion
	    (gnus-summary-goto-subject article)
	    (gnus-summary-mark-article article gnus-canceled-mark)))))
      (push article articles-to-update-marks))

    (save-excursion
      (apply 'gnus-summary-remove-process-mark articles-to-update-marks))
    ;; Re-activate all groups that have been moved to.
    (with-current-buffer gnus-group-buffer
      (let ((gnus-group-marked to-groups))
	(gnus-group-get-new-news-this-group nil t)))

    (gnus-kill-buffer copy-buf)
    (gnus-summary-position-point)
    (gnus-set-mode-line 'summary)))

(defun gnus-summary-push-marks-to-backend (article)
  (let ((set nil)
	(marks gnus-article-mark-lists))
    (unless (memq article gnus-newsgroup-unreads)
      (push 'read set))
    (while marks
      (when (and (eq (gnus-article-mark-to-type (cdar marks)) 'list)
		 (memq article (symbol-value
				(intern (format "gnus-newsgroup-%s"
						(caar marks))))))
	(push (cdar marks) set))
      (pop marks))
    (gnus-request-set-mark gnus-newsgroup-name `(((,article) set ,set)))))

(defun gnus-summary-copy-article (&optional n to-newsgroup select-method)
  "Copy the current article to some other group.
If TO-NEWSGROUP is string, do not prompt for a newsgroup to copy to.
When called interactively, if TO-NEWSGROUP is nil, use the value of
the variable `gnus-move-split-methods' for finding a default target
newsgroup.
If SELECT-METHOD is non-nil, do not move to a specific newsgroup, but
re-spool using this method."
  (interactive "P")
  (gnus-summary-move-article n to-newsgroup select-method 'copy))

(defun gnus-summary-crosspost-article (&optional n)
  "Crosspost the current article to some other group."
  (interactive "P")
  (gnus-summary-move-article n nil nil 'crosspost))

(defcustom gnus-summary-respool-default-method nil
  "Default method type for respooling an article.
If nil, use to the current newsgroup method."
  :type 'symbol
  :group 'gnus-summary-mail)

(defun gnus-summary-respool-article (&optional n method)
  "Respool the current article.
The article will be squeezed through the mail spooling process again,
which means that it will be put in some mail newsgroup or other
depending on `nnmail-split-methods'.
If N is a positive number, respool the N next articles.
If N is a negative number, respool the N previous articles.
If N is nil and any articles have been marked with the process mark,
respool those articles instead.

Respooling can be done both from mail groups and \"real\" newsgroups.
In the former case, the articles in question will be moved from the
current group into whatever groups they are destined to.  In the
latter case, they will be copied into the relevant groups."
  (interactive
   (list current-prefix-arg
	 (let* ((methods (mapcar #'car (gnus-methods-using 'respool)))
		(methname
		 (symbol-name (or gnus-summary-respool-default-method
				  (car (gnus-find-method-for-group
					gnus-newsgroup-name)))))
		(method
		 (gnus-completing-read
		  "Backend to use when respooling"
		  methods t nil 'gnus-mail-method-history methname))
		ms)
	   (cond
	    ((zerop (length (setq ms (gnus-servers-using-backend
				      (intern method)))))
	     (list (intern method) ""))
	    ((= 1 (length ms))
	     (car ms))
	    (t
	     (let ((ms-alist (mapcar (lambda (m) (cons (cadr m) m)) ms)))
	       (cdr (assoc (gnus-completing-read "Server name" ms-alist t)
			   ms-alist))))))))
  (unless method
    (error "No method given for respooling"))
  (if (assoc (symbol-name
	      (car (gnus-find-method-for-group gnus-newsgroup-name)))
	     (gnus-methods-using 'respool))
      (gnus-summary-move-article n nil method)
    (gnus-summary-copy-article n nil method)))

(defun gnus-summary-import-article (file &optional edit)
  "Import an arbitrary file into a mail newsgroup."
  (interactive "fImport file: \nP")
  (let ((group gnus-newsgroup-name)
	(now (current-time))
	atts lines group-art)
    (unless (gnus-check-backend-function 'request-accept-article group)
      (error "%s does not support article importing" group))
    (or (file-readable-p file)
	(not (file-regular-p file))
	(error "Can't read %s" file))
    (with-current-buffer (gnus-get-buffer-create " *import file*")
      (erase-buffer)
      (nnheader-insert-file-contents file)
      (goto-char (point-min))
      (if (nnheader-article-p)
	  (save-restriction
	    (goto-char (point-min))
	    (search-forward "\n\n" nil t)
	    (narrow-to-region (point-min) (1- (point)))
	    (goto-char (point-min))
	    (unless (re-search-forward "^date:" nil t)
	      (goto-char (point-max))
	      (insert "Date: " (message-make-date (nth 5 atts)) "\n")))
       ;; This doesn't look like an article, so we fudge some headers.
	(setq atts (file-attributes file)
	      lines (count-lines (point-min) (point-max)))
	(insert "From: " (read-string "From: ") "\n"
		"Subject: " (read-string "Subject: ") "\n"
		"Date: " (message-make-date (nth 5 atts)) "\n"
		"Message-ID: " (message-make-message-id) "\n"
		"Lines: " (int-to-string lines) "\n"
		"Chars: " (int-to-string (nth 7 atts)) "\n\n"))
      (setq group-art (gnus-request-accept-article group nil t))
      (kill-buffer (current-buffer)))
    (setq gnus-newsgroup-active (gnus-activate-group group))
    (forward-line 1)
    (gnus-summary-goto-article (cdr group-art) nil t)
    (when edit
      (gnus-summary-edit-article))))

(defun gnus-summary-create-article ()
  "Create an article in a mail newsgroup."
  (interactive)
  (let ((group gnus-newsgroup-name)
	(now (current-time))
	group-art)
    (unless (gnus-check-backend-function 'request-accept-article group)
      (error "%s does not support article importing" group))
    (with-current-buffer (gnus-get-buffer-create " *import file*")
      (erase-buffer)
      (goto-char (point-min))
      ;; This doesn't look like an article, so we fudge some headers.
      (insert "From: " (read-string "From: ") "\n"
	      "Subject: " (read-string "Subject: ") "\n"
	      "Date: " (message-make-date now) "\n"
	      "Message-ID: " (message-make-message-id) "\n")
      (setq group-art (gnus-request-accept-article group nil t))
      (kill-buffer (current-buffer)))
    (setq gnus-newsgroup-active (gnus-activate-group group))
    (forward-line 1)
    (gnus-summary-goto-article (cdr group-art) nil t)
    (gnus-summary-edit-article)))

(defun gnus-summary-article-posted-p ()
  "Say whether the current (mail) article is available from news as well.
This will be the case if the article has both been mailed and posted."
  (interactive)
  (let ((id (mail-header-references (gnus-summary-article-header)))
	(gnus-override-method (car (gnus-refer-article-methods))))
    (if (gnus-request-head id "")
	(gnus-message 2 "The current message was found on %s"
		      gnus-override-method)
      (gnus-message 2 "The current message couldn't be found on %s"
		    gnus-override-method)
      nil)))

(defun gnus-summary-expire-articles (&optional now)
  "Expire all articles that are marked as expirable in the current group."
  (interactive)
  (when (and (not gnus-group-is-exiting-without-update-p)
	     (gnus-check-backend-function
	      'request-expire-articles gnus-newsgroup-name))
    ;; This backend supports expiry.
    (let* ((total (gnus-group-total-expirable-p gnus-newsgroup-name))
	   (expirable (if total
			  (progn
			    ;; We need to update the info for
			    ;; this group for `gnus-list-of-read-articles'
			    ;; to give us the right answer.
			    (gnus-run-hooks 'gnus-exit-group-hook)
			    (gnus-summary-update-info)
			    (gnus-list-of-read-articles gnus-newsgroup-name))
			(setq gnus-newsgroup-expirable
			      (sort gnus-newsgroup-expirable '<))))
	   (expiry-wait (if now 'immediate
			  (gnus-group-find-parameter
			   gnus-newsgroup-name 'expiry-wait)))
	   (nnmail-expiry-target
	    (or (gnus-group-find-parameter gnus-newsgroup-name 'expiry-target)
		nnmail-expiry-target))
	   es)
      (when expirable
	;; There are expirable articles in this group, so we run them
	;; through the expiry process.
	(gnus-message 6 "Expiring articles...")
	(when (gnus-check-group gnus-newsgroup-name)
	  ;; The list of articles that weren't expired is returned.
	  (save-excursion
	    (if expiry-wait
		(let ((nnmail-expiry-wait-function nil)
		      (nnmail-expiry-wait expiry-wait))
		  (setq es (gnus-request-expire-articles
			    expirable gnus-newsgroup-name)))
	      (setq es (gnus-request-expire-articles
			expirable gnus-newsgroup-name)))
	    (unless total
	      (setq gnus-newsgroup-expirable es))
	    ;; We go through the old list of expirable, and mark all
	    ;; really expired articles as nonexistent.
	    (unless (eq es expirable) ;If nothing was expired, we don't mark.
	      (let ((gnus-use-cache nil))
		(dolist (article expirable)
		  (when (and (not (memq article es))
			     (gnus-data-find article))
		    (gnus-summary-mark-article article gnus-canceled-mark)
		    (run-hook-with-args 'gnus-summary-article-expire-hook
					'delete
					(gnus-data-header
					 (assoc article (gnus-data-list nil)))
					gnus-newsgroup-name
					nil
					nil)))))))
	(gnus-message 6 "Expiring articles...done")))))

(defun gnus-summary-expire-articles-now ()
  "Expunge all expirable articles in the current group.
This means that *all* articles that are marked as expirable will be
deleted forever, right now."
  (interactive)
  (or gnus-expert-user
      (gnus-yes-or-no-p
       "Are you really, really sure you want to delete all expirable messages? ")
      (error "Phew!"))
  (gnus-summary-expire-articles t))

;; Suggested by Jack Vinson <vinson@unagi.cis.upenn.edu>.
(defun gnus-summary-delete-article (&optional n)
  "Delete the N next (mail) articles.
This command actually deletes articles.  This is not a marking
command.  The article will disappear forever from your life, never to
return.

If N is negative, delete backwards.
If N is nil and articles have been marked with the process mark,
delete these instead.

If `gnus-novice-user' is non-nil you will be asked for
confirmation before the articles are deleted."
  (interactive "P")
  (unless (gnus-check-backend-function 'request-expire-articles
				       gnus-newsgroup-name)
    (error "The current newsgroup does not support article deletion"))
  (unless (gnus-check-server (gnus-find-method-for-group gnus-newsgroup-name))
    (error "Couldn't open server"))
  ;; Compute the list of articles to delete.
  (let ((articles (sort (copy-sequence (gnus-summary-work-articles n)) '<))
	(nnmail-expiry-target 'delete)
	not-deleted)
    (if (and gnus-novice-user
	     (not (gnus-yes-or-no-p
		   (format "Do you really want to delete %s forever? "
			   (if (> (length articles) 1)
			       (format "these %s articles" (length articles))
			     "this article")))))
	()
      ;; Delete the articles.
      (setq not-deleted (gnus-request-expire-articles
			 articles gnus-newsgroup-name 'force))
      (save-excursion
	(while articles
	  (gnus-summary-remove-process-mark (car articles))
	  ;; The backend might not have been able to delete the article
	  ;; after all.
	  (unless (memq (car articles) not-deleted)
	    (gnus-summary-mark-article (car articles) gnus-canceled-mark)
	    (let* ((article (car articles))
		   (ghead  (gnus-data-header
			    (assoc article (gnus-data-list nil)))))
	      (run-hook-with-args 'gnus-summary-article-delete-hook
				  'delete ghead gnus-newsgroup-name nil
				  nil)))
	  (setq articles (cdr articles))))
      (when not-deleted
	(gnus-message 4 "Couldn't delete articles %s" not-deleted)))
    (gnus-summary-position-point)
    (gnus-set-mode-line 'summary)
    not-deleted))

(defun gnus-summary-edit-article (&optional arg)
  "Edit the current article.
This will have permanent effect only in mail groups.
If ARG is nil, edit the decoded articles.
If ARG is 1, edit the raw articles.
If ARG is 2, edit the raw articles even in read-only groups.
If ARG is 3, edit the articles with the current handles.
Otherwise, allow editing of articles even in read-only
groups."
  (interactive "P")
  (let (force raw current-handles)
    (cond
     ((null arg))
     ((eq arg 1)
      (setq raw t))
     ((eq arg 2)
      (setq raw t
	    force t))
     ((eq arg 3)
      (setq current-handles
	    (and (gnus-buffer-live-p gnus-article-buffer)
		 (with-current-buffer gnus-article-buffer
		   (prog1
		       gnus-article-mime-handles
		     (setq gnus-article-mime-handles nil))))))
     (t
      (setq force t)))
    (when (and raw (not force)
	       (member gnus-newsgroup-name '("nndraft:delayed"
					     "nndraft:drafts"
					     "nndraft:queue")))
      (error "Can't edit the raw article in group %s"
	     gnus-newsgroup-name))
    (with-current-buffer gnus-summary-buffer
      (let ((mail-parse-charset gnus-newsgroup-charset)
	    (mail-parse-ignored-charsets gnus-newsgroup-ignored-charsets))
	(gnus-set-global-variables)
	(when (and (not force)
		   (gnus-group-read-only-p))
	  (error "The current newsgroup does not support article editing"))
	(gnus-summary-show-article t)
	(when (and (not raw) (gnus-buffer-live-p gnus-article-buffer))
	  (with-current-buffer gnus-article-buffer
	    (mm-enable-multibyte)))
	(if (member gnus-newsgroup-name '("nndraft:delayed" "nndraft:drafts"))
	    (setq raw t))
	(gnus-article-edit-article
	 (if raw 'ignore
	   `(lambda ()
	      (let ((mbl mml-buffer-list))
		(setq mml-buffer-list nil)
		(let ((rfc2047-quote-decoded-words-containing-tspecials t))
		  (mime-to-mml ,'current-handles))
		(let ((mbl1 mml-buffer-list))
		  (setq mml-buffer-list mbl)
		  (set (make-local-variable 'mml-buffer-list) mbl1))
		(gnus-make-local-hook 'kill-buffer-hook)
		(add-hook 'kill-buffer-hook 'mml-destroy-buffers t t))))
	 `(lambda (no-highlight)
	    (let ((mail-parse-charset ',gnus-newsgroup-charset)
		  (message-options message-options)
		  (message-options-set-recipient)
		  (mail-parse-ignored-charsets
		   ',gnus-newsgroup-ignored-charsets)
		  (rfc2047-header-encoding-alist
		   ',(let ((charset (gnus-group-name-charset
				     (gnus-find-method-for-group
				      gnus-newsgroup-name)
				     gnus-newsgroup-name)))
		       (append (list (cons "Newsgroups" charset)
				     (cons "Followup-To" charset)
				     (cons "Xref" charset))
			       rfc2047-header-encoding-alist))))
	      ,(if (not raw) '(progn
				(mml-to-mime)
				(mml-destroy-buffers)
				(remove-hook 'kill-buffer-hook
					     'mml-destroy-buffers t)
				(kill-local-variable 'mml-buffer-list)))
	      (gnus-summary-edit-article-done
	       ,(or (mail-header-references gnus-current-headers) "")
	       ,(gnus-group-read-only-p)
	       ,gnus-summary-buffer no-highlight))))))))

(defalias 'gnus-summary-edit-article-postpone 'gnus-article-edit-exit)

(defun gnus-summary-edit-article-done (&optional references read-only buffer
						 no-highlight)
  "Make edits to the current article permanent."
  (interactive)
  (save-excursion
    ;; The buffer restriction contains the entire article if it exists.
    (when (article-goto-body)
      (let ((lines (count-lines (point) (point-max)))
	    (length (- (point-max) (point)))
	    (case-fold-search t)
	    (body (copy-marker (point))))
	(goto-char (point-min))
	(when (re-search-forward "^content-length:[ \t]\\([0-9]+\\)" body t)
	  (delete-region (match-beginning 1) (match-end 1))
	  (insert (number-to-string length)))
	(goto-char (point-min))
	(when (re-search-forward
	       "^x-content-length:[ \t]\\([0-9]+\\)" body t)
	  (delete-region (match-beginning 1) (match-end 1))
	  (insert (number-to-string length)))
	(goto-char (point-min))
	(when (re-search-forward "^lines:[ \t]\\([0-9]+\\)" body t)
	  (delete-region (match-beginning 1) (match-end 1))
	  (insert (number-to-string lines))))))
  ;; Replace the article.
  (let ((buf (current-buffer))
	(article (cdr gnus-article-current))
	replace-result)
    (with-temp-buffer
      (insert-buffer-substring buf)
      (if (and (not read-only)
	       (not (setq replace-result
			  (gnus-request-replace-article
			   article (car gnus-article-current)
			   (current-buffer) t))))
	  (error "Couldn't replace article")
	;; If we got a number back, then that's the new article number
	;; for this article.  Otherwise, the article number didn't change.
	(when (numberp replace-result)
	  (with-current-buffer gnus-summary-buffer
	    (setq gnus-newsgroup-limit (delq article gnus-newsgroup-limit))
	    (gnus-summary-limit gnus-newsgroup-limit)
	    (setq article replace-result)
	    (gnus-summary-goto-subject article t)))
	;; Update the summary buffer.
	(if (and references
		 (equal (message-tokenize-header references " ")
			(message-tokenize-header
			 (or (message-fetch-field "references") "") " ")))
	    ;; We only have to update this line.
	    (save-excursion
	      (save-restriction
		(message-narrow-to-head)
		(let ((head (buffer-substring-no-properties
			     (point-min) (point-max)))
		      header)
		  (with-temp-buffer
		    (insert (format "211 %d Article retrieved.\n" article))
		    (insert head)
		    (insert ".\n")
		    (let ((nntp-server-buffer (current-buffer)))
		      (setq header (car (gnus-get-newsgroup-headers nil t))))
		    (with-current-buffer gnus-summary-buffer
		      (gnus-data-set-header (gnus-data-find article) header)
		      (gnus-summary-update-article-line article header)
		      (if (gnus-summary-goto-subject article nil t)
			  (gnus-summary-update-secondary-mark article)))))))
	  ;; Update threads.
	  (set-buffer (or buffer gnus-summary-buffer))
	  (gnus-summary-update-article article)
	  (if (gnus-summary-goto-subject article nil t)
	      (gnus-summary-update-secondary-mark article)))
	;; Prettify the article buffer again.
	(unless no-highlight
	  (with-current-buffer gnus-article-buffer
	    ;;!!! Fix this -- article should be rehighlighted.
	    ;;(gnus-run-hooks 'gnus-article-display-hook)
	    (set-buffer gnus-original-article-buffer)
	    (gnus-request-article
	     article (car gnus-article-current) (current-buffer))))
	;; Prettify the summary buffer line.
	(when (gnus-visual-p 'summary-highlight 'highlight)
	  (gnus-run-hooks 'gnus-visual-mark-article-hook))))))

(defun gnus-summary-edit-wash (key)
  "Perform editing command KEY in the article buffer."
  (interactive
   (list
    (progn
      (message "%s" (concat (this-command-keys) "- "))
      (read-char))))
  (message "")
  (gnus-summary-edit-article)
  (execute-kbd-macro (concat (this-command-keys) key))
  (gnus-article-edit-done))

;;; Respooling

(defun gnus-summary-respool-query (&optional silent trace)
  "Query where the respool algorithm would put this article."
  (interactive)
  (let (gnus-mark-article-hook)
    (gnus-summary-select-article)
    (with-current-buffer gnus-original-article-buffer
      (let ((groups (nnmail-article-group 'identity trace)))
	(unless silent
	  (if groups
	      (message "This message would go to %s"
		       (mapconcat 'car groups ", "))
	    (message "This message would go to no groups"))
	  groups)))))

(defun gnus-summary-respool-trace ()
  "Trace where the respool algorithm would put this article.
Display a buffer showing all fancy splitting patterns which matched."
  (interactive)
  (gnus-summary-respool-query nil t))

;; Summary marking commands.

(defun gnus-summary-kill-same-subject-and-select (&optional unmark)
  "Mark articles which has the same subject as read, and then select the next.
If UNMARK is positive, remove any kind of mark.
If UNMARK is negative, tick articles."
  (interactive "P")
  (when unmark
    (setq unmark (prefix-numeric-value unmark)))
  (let ((count
	 (gnus-summary-mark-same-subject
	  (gnus-summary-article-subject) unmark)))
    ;; Select next unread article.  If auto-select-same mode, should
    ;; select the first unread article.
    (gnus-summary-next-article t (and gnus-auto-select-same
				      (gnus-summary-article-subject)))
    (gnus-message 7 "%d article%s marked as %s"
		  count (if (= count 1) " is" "s are")
		  (if unmark "unread" "read"))))

(defun gnus-summary-kill-same-subject (&optional unmark)
  "Mark articles which has the same subject as read.
If UNMARK is positive, remove any kind of mark.
If UNMARK is negative, tick articles."
  (interactive "P")
  (when unmark
    (setq unmark (prefix-numeric-value unmark)))
  (let ((count
	 (gnus-summary-mark-same-subject
	  (gnus-summary-article-subject) unmark)))
    ;; If marked as read, go to next unread subject.
    (when (null unmark)
      ;; Go to next unread subject.
      (gnus-summary-next-subject 1 t))
    (gnus-message 7 "%d articles are marked as %s"
		  count (if unmark "unread" "read"))))

(defun gnus-summary-mark-same-subject (subject &optional unmark)
  "Mark articles with same SUBJECT as read, and return marked number.
If optional argument UNMARK is positive, remove any kinds of marks.
If optional argument UNMARK is negative, mark articles as unread instead."
  (let ((count 1))
    (save-excursion
      (cond
       ((null unmark)			; Mark as read.
	(while (and
		(progn
		  (gnus-summary-mark-article-as-read gnus-killed-mark)
		  (gnus-summary-show-thread) t)
		(gnus-summary-find-subject subject))
	  (setq count (1+ count))))
       ((> unmark 0)			; Tick.
	(while (and
		(progn
		  (gnus-summary-mark-article-as-unread gnus-ticked-mark)
		  (gnus-summary-show-thread) t)
		(gnus-summary-find-subject subject))
	  (setq count (1+ count))))
       (t				; Mark as unread.
	(while (and
		(progn
		  (gnus-summary-mark-article-as-unread gnus-unread-mark)
		  (gnus-summary-show-thread) t)
		(gnus-summary-find-subject subject))
	  (setq count (1+ count)))))
      (gnus-set-mode-line 'summary)
      ;; Return the number of marked articles.
      count)))

(defun gnus-summary-mark-as-processable (n &optional unmark)
  "Set the process mark on the next N articles.
If N is negative, mark backward instead.  If UNMARK is non-nil, remove
the process mark instead.  The difference between N and the actual
number of articles marked is returned."
  (interactive "P")
  (if (and (null n) (gnus-region-active-p))
      (gnus-uu-mark-region (region-beginning) (region-end) unmark)
    (setq n (prefix-numeric-value n))
    (let ((backward (< n 0))
	  (n (abs n)))
      (while (and
	      (> n 0)
	      (if unmark
		  (gnus-summary-remove-process-mark
		   (gnus-summary-article-number))
		(gnus-summary-set-process-mark (gnus-summary-article-number)))
	      (zerop (gnus-summary-next-subject (if backward -1 1) nil t)))
	(setq n (1- n)))
      (when (/= 0 n)
	(gnus-message 7 "No more articles"))
      (gnus-summary-recenter)
      (gnus-summary-position-point)
      n)))

(defun gnus-summary-unmark-as-processable (n)
  "Remove the process mark from the next N articles.
If N is negative, unmark backward instead.  The difference between N and
the actual number of articles unmarked is returned."
  (interactive "P")
  (gnus-summary-mark-as-processable n t))

(defun gnus-summary-unmark-all-processable ()
  "Remove the process mark from all articles."
  (interactive)
  (save-excursion
    (while gnus-newsgroup-processable
      (gnus-summary-remove-process-mark (car gnus-newsgroup-processable))))
  (gnus-summary-position-point))

(defun gnus-summary-add-mark (article type)
  "Mark ARTICLE with a mark of TYPE."
  (let ((vtype (car (assq type gnus-article-mark-lists)))
	var)
    (if (not vtype)
	(error "No such mark type: %s" type)
      (setq var (intern (format "gnus-newsgroup-%s" type)))
      (set var (cons article (symbol-value var)))
      (if (memq type '(processable cached replied forwarded recent saved))
	  (gnus-summary-update-secondary-mark article)
	;;; !!! This is bogus.  We should find out what primary
	;;; !!! mark we want to set.
	(gnus-summary-update-mark gnus-del-mark 'unread)))))

(defun gnus-summary-mark-as-expirable (n)
  "Mark N articles forward as expirable.
If N is negative, mark backward instead.  The difference between N and
the actual number of articles marked is returned."
  (interactive "p")
  (gnus-summary-mark-forward n gnus-expirable-mark))

(defun gnus-summary-mark-as-spam (n)
  "Mark N articles forward as spam.
If N is negative, mark backward instead.  The difference between N and
the actual number of articles marked is returned."
  (interactive "p")
  (gnus-summary-mark-forward n gnus-spam-mark))

(defun gnus-summary-mark-article-as-replied (article)
  "Mark ARTICLE as replied to and update the summary line.
ARTICLE can also be a list of articles."
  (interactive (list (gnus-summary-article-number)))
  (let ((articles (if (listp article) article (list article))))
    (dolist (article articles)
      (unless (numberp article)
	(error "%s is not a number" article))
      (push article gnus-newsgroup-replied)
      (let ((inhibit-read-only t))
	(when (gnus-summary-goto-subject article nil t)
	  (gnus-summary-update-secondary-mark article))))))

(defun gnus-summary-mark-article-as-forwarded (article)
  "Mark ARTICLE as forwarded and update the summary line.
ARTICLE can also be a list of articles."
  (let ((articles (if (listp article) article (list article))))
    (dolist (article articles)
      (push article gnus-newsgroup-forwarded)
      (let ((inhibit-read-only t))
	(when (gnus-summary-goto-subject article nil t)
	  (gnus-summary-update-secondary-mark article))))))

(defun gnus-summary-set-bookmark (article)
  "Set a bookmark in current article."
  (interactive (list (gnus-summary-article-number)))
  (when (or (not (get-buffer gnus-article-buffer))
	    (not gnus-current-article)
	    (not gnus-article-current)
	    (not (equal gnus-newsgroup-name (car gnus-article-current))))
    (error "No current article selected"))
  ;; Remove old bookmark, if one exists.
  (gnus-alist-pull article gnus-newsgroup-bookmarks)
  ;; Set the new bookmark, which is on the form
  ;; (article-number . line-number-in-body).
  (push
   (cons article
	 (with-current-buffer gnus-article-buffer
	   (count-lines
	    (min (point)
		 (save-excursion
		   (article-goto-body)
		   (point)))
	    (point))))
   gnus-newsgroup-bookmarks)
  (gnus-message 6 "A bookmark has been added to the current article."))

(defun gnus-summary-remove-bookmark (article)
  "Remove the bookmark from the current article."
  (interactive (list (gnus-summary-article-number)))
  ;; Remove old bookmark, if one exists.
  (if (not (assq article gnus-newsgroup-bookmarks))
      (gnus-message 6 "No bookmark in current article.")
    (gnus-alist-pull article gnus-newsgroup-bookmarks)
    (gnus-message 6 "Removed bookmark.")))

;; Suggested by Daniel Quinlan <quinlan@best.com>.
(defun gnus-summary-mark-as-dormant (n)
  "Mark N articles forward as dormant.
If N is negative, mark backward instead.  The difference between N and
the actual number of articles marked is returned."
  (interactive "p")
  (gnus-summary-mark-forward n gnus-dormant-mark))

(defun gnus-summary-set-process-mark (article)
  "Set the process mark on ARTICLE and update the summary line."
  (setq gnus-newsgroup-processable
	(cons article
	      (delq article gnus-newsgroup-processable)))
  (when (gnus-summary-goto-subject article)
    (gnus-summary-show-thread)
    (gnus-summary-goto-subject article)
    (gnus-summary-update-secondary-mark article)))

(defun gnus-summary-remove-process-mark (&rest articles)
  "Remove the process mark from ARTICLES and update the summary line."
  (dolist (article articles)
    (setq gnus-newsgroup-processable (delq article gnus-newsgroup-processable))
    (when (gnus-summary-goto-subject article)
      (gnus-summary-show-thread)
      (gnus-summary-goto-subject article)
      (gnus-summary-update-secondary-mark article)))
  t)

(defun gnus-summary-set-saved-mark (article)
  "Set the process mark on ARTICLE and update the summary line."
  (push article gnus-newsgroup-saved)
  (when (gnus-summary-goto-subject article)
    (gnus-summary-update-secondary-mark article)))

(defun gnus-summary-mark-forward (n &optional mark no-expire)
  "Mark N articles as read forwards.
If N is negative, mark backwards instead.  Mark with MARK, ?r by default.
The difference between N and the actual number of articles marked is
returned.
If NO-EXPIRE, auto-expiry will be inhibited."
  (interactive "p")
  (gnus-summary-show-thread)
  (let ((backward (< n 0))
	(gnus-summary-goto-unread
	 (and gnus-summary-goto-unread
	      (not (eq gnus-summary-goto-unread 'never))
	      (not (memq mark (list gnus-unread-mark gnus-spam-mark
				    gnus-ticked-mark gnus-dormant-mark)))))
	(n (abs n))
	(mark (or mark gnus-del-mark)))
    (while (and (> n 0)
		(gnus-summary-mark-article nil mark no-expire)
		(zerop (gnus-summary-next-subject
			(if backward -1 1)
			(and gnus-summary-goto-unread
			     (not (eq gnus-summary-goto-unread 'never)))
			t)))
      (setq n (1- n)))
    (when (/= 0 n)
      (gnus-message 7 "No more %sarticles" (if mark "" "unread ")))
    (gnus-summary-recenter)
    (gnus-summary-position-point)
    (gnus-set-mode-line 'summary)
    n))

(defun gnus-summary-mark-article-as-read (mark)
  "Mark the current article quickly as read with MARK."
  (let ((article (gnus-summary-article-number)))
    (setq gnus-newsgroup-unreads (delq article gnus-newsgroup-unreads))
    (setq gnus-newsgroup-marked (delq article gnus-newsgroup-marked))
    (setq gnus-newsgroup-spam-marked (delq article gnus-newsgroup-spam-marked))
    (setq gnus-newsgroup-dormant (delq article gnus-newsgroup-dormant))
    (push (cons article mark) gnus-newsgroup-reads)
    ;; Possibly remove from cache, if that is used.
    (when gnus-use-cache
      (gnus-cache-enter-remove-article article))
    ;; Allow the backend to change the mark.
    (setq mark (gnus-request-update-mark gnus-newsgroup-name article mark))
    ;; Check for auto-expiry.
    (when (and gnus-newsgroup-auto-expire
	       (memq mark gnus-auto-expirable-marks))
      (setq mark gnus-expirable-mark)
      ;; Let the backend know about the mark change.
      (setq mark (gnus-request-update-mark gnus-newsgroup-name article mark))
      (push article gnus-newsgroup-expirable))
    ;; Set the mark in the buffer.
    (gnus-summary-update-mark mark 'unread)
    t))

(defun gnus-summary-mark-article-as-unread (mark)
  "Mark the current article quickly as unread with MARK."
  (let* ((article (gnus-summary-article-number))
	 (old-mark (gnus-summary-article-mark article)))
    ;; Allow the backend to change the mark.
    (setq mark (gnus-request-update-mark gnus-newsgroup-name article mark))
    (if (eq mark old-mark)
	t
      (if (<= article 0)
	  (progn
	    (gnus-error 1 "Can't mark negative article numbers")
	    nil)
	(setq gnus-newsgroup-marked (delq article gnus-newsgroup-marked))
	(setq gnus-newsgroup-spam-marked
	      (delq article gnus-newsgroup-spam-marked))
	(setq gnus-newsgroup-dormant (delq article gnus-newsgroup-dormant))
	(setq gnus-newsgroup-expirable (delq article gnus-newsgroup-expirable))
	(setq gnus-newsgroup-reads (delq article gnus-newsgroup-reads))
	(setq gnus-newsgroup-unreads (delq article gnus-newsgroup-unreads))
	(cond ((= mark gnus-ticked-mark)
	       (setq gnus-newsgroup-marked
		     (gnus-add-to-sorted-list gnus-newsgroup-marked
					      article)))
	      ((= mark gnus-spam-mark)
	       (setq gnus-newsgroup-spam-marked
		     (gnus-add-to-sorted-list gnus-newsgroup-spam-marked
					      article)))
	      ((= mark gnus-dormant-mark)
	       (setq gnus-newsgroup-dormant
		     (gnus-add-to-sorted-list gnus-newsgroup-dormant
					      article)))
	      (t
	       (setq gnus-newsgroup-unreads
		     (gnus-add-to-sorted-list gnus-newsgroup-unreads
					      article))))
	(gnus-alist-pull article gnus-newsgroup-reads)

	;; See whether the article is to be put in the cache.
	(and gnus-use-cache
	     (vectorp (gnus-summary-article-header article))
	     (save-excursion
	       (gnus-cache-possibly-enter-article
		gnus-newsgroup-name article
		(= mark gnus-ticked-mark)
		(= mark gnus-dormant-mark) (= mark gnus-unread-mark))))

	;; Fix the mark.
	(gnus-summary-update-mark mark 'unread)
	t))))

(defun gnus-summary-mark-article (&optional article mark no-expire)
  "Mark ARTICLE with MARK.  MARK can be any character.
Four MARK strings are reserved: `? ' (unread), `?!' (ticked),
`??' (dormant) and `?E' (expirable).
If MARK is nil, then the default character `?r' is used.
If ARTICLE is nil, then the article on the current line will be
marked.
If NO-EXPIRE, auto-expiry will be inhibited."
  ;; The mark might be a string.
  (when (stringp mark)
    (setq mark (aref mark 0)))
  ;; If no mark is given, then we check auto-expiring.
  (when (null mark)
    (setq mark gnus-del-mark))
  (when (and (not no-expire)
	     gnus-newsgroup-auto-expire
	     (memq mark gnus-auto-expirable-marks))
    (setq mark gnus-expirable-mark))
  (let ((article (or article (gnus-summary-article-number)))
	(old-mark (gnus-summary-article-mark article)))
    ;; Allow the backend to change the mark.
    (setq mark (gnus-request-update-mark gnus-newsgroup-name article mark))
    (if (eq mark old-mark)
	t
      (unless article
	(error "No article on current line"))
      (if (not (if (or (= mark gnus-unread-mark)
		       (= mark gnus-ticked-mark)
		       (= mark gnus-spam-mark)
		       (= mark gnus-dormant-mark))
		   (gnus-mark-article-as-unread article mark)
		 (gnus-mark-article-as-read article mark)))
	  t
	;; See whether the article is to be put in the cache.
	(and gnus-use-cache
	     (not (= mark gnus-canceled-mark))
	     (vectorp (gnus-summary-article-header article))
	     (save-excursion
	       (gnus-cache-possibly-enter-article
		gnus-newsgroup-name article
		(= mark gnus-ticked-mark)
		(= mark gnus-dormant-mark) (= mark gnus-unread-mark))))

	(when (gnus-summary-goto-subject article nil t)
	  (let ((inhibit-read-only t))
	    (gnus-summary-show-thread)
	    ;; Fix the mark.
	    (gnus-summary-update-mark mark 'unread)
	    t))))))

(defun gnus-summary-update-secondary-mark (article)
  "Update the secondary (read, process, cache) mark."
  (gnus-summary-update-mark
   (cond ((memq article gnus-newsgroup-processable)
	  gnus-process-mark)
	 ((memq article gnus-newsgroup-cached)
	  gnus-cached-mark)
	 ((memq article gnus-newsgroup-replied)
	  gnus-replied-mark)
	 ((memq article gnus-newsgroup-forwarded)
	  gnus-forwarded-mark)
	 ((memq article gnus-newsgroup-saved)
	  gnus-saved-mark)
	 ((memq article gnus-newsgroup-unseen)
	  gnus-unseen-mark)
	 (t gnus-no-mark))
   'replied)
  (when (gnus-visual-p 'summary-highlight 'highlight)
    (gnus-summary-highlight-line)
    (gnus-run-hooks 'gnus-summary-update-hook))
  t)

(defun gnus-summary-update-download-mark (article)
  "Update the download mark."
  (gnus-summary-update-mark
   (cond ((memq article gnus-newsgroup-undownloaded)
          gnus-undownloaded-mark)
         (gnus-newsgroup-agentized
          gnus-downloaded-mark)
         (t
          gnus-no-mark))
   'download)
  (gnus-summary-update-line t)
  t)

(defun gnus-summary-update-mark (mark type)
  (let ((forward (cdr (assq type gnus-summary-mark-positions)))
	(inhibit-read-only t))
    (re-search-backward "[\n\r]" (point-at-bol) 'move-to-limit)
    (when forward
      (when (looking-at "\r")
	(incf forward))
      (when (<= (+ forward (point)) (point-max))
	;; Go to the right position on the line.
	(goto-char (+ forward (point)))
	;; Replace the old mark with the new mark.
        (let ((to-insert
               (mm-subst-char-in-string
		(char-after) mark
		(buffer-substring (point) (1+ (point))))))
          (delete-region (point) (1+ (point)))
          (insert to-insert))
	;; Optionally update the marks by some user rule.
	(when (eq type 'unread)
	  (gnus-data-set-mark
	   (gnus-data-find (gnus-summary-article-number)) mark)
	  (gnus-summary-update-line (eq mark gnus-unread-mark)))))))

(defun gnus-mark-article-as-read (article &optional mark)
  "Enter ARTICLE in the pertinent lists and remove it from others."
  ;; Make the article expirable.
  (let ((mark (or mark gnus-del-mark)))
    (setq gnus-newsgroup-expirable
	  (if (= mark gnus-expirable-mark)
	      (gnus-add-to-sorted-list gnus-newsgroup-expirable article)
	    (delq article gnus-newsgroup-expirable)))
    ;; Remove from unread and marked lists.
    (setq gnus-newsgroup-unreads (delq article gnus-newsgroup-unreads))
    (setq gnus-newsgroup-marked (delq article gnus-newsgroup-marked))
    (setq gnus-newsgroup-spam-marked (delq article gnus-newsgroup-spam-marked))
    (setq gnus-newsgroup-dormant (delq article gnus-newsgroup-dormant))
    (push (cons article mark) gnus-newsgroup-reads)
    ;; Possibly remove from cache, if that is used.
    (when gnus-use-cache
      (gnus-cache-enter-remove-article article))
    t))

(defun gnus-mark-article-as-unread (article &optional mark)
  "Enter ARTICLE in the pertinent lists and remove it from others."
  (let ((mark (or mark gnus-ticked-mark)))
    (if (<= article 0)
	(progn
	  (gnus-error 1 "Can't mark negative article numbers")
	  nil)
      (setq gnus-newsgroup-marked (delq article gnus-newsgroup-marked)
	    gnus-newsgroup-spam-marked (delq article gnus-newsgroup-spam-marked)
	    gnus-newsgroup-dormant (delq article gnus-newsgroup-dormant)
	    gnus-newsgroup-expirable (delq article gnus-newsgroup-expirable)
	    gnus-newsgroup-unreads (delq article gnus-newsgroup-unreads))

      ;; Unsuppress duplicates?
      (when gnus-suppress-duplicates
	(gnus-dup-unsuppress-article article))

      (cond ((= mark gnus-ticked-mark)
	     (setq gnus-newsgroup-marked
		   (gnus-add-to-sorted-list gnus-newsgroup-marked article)))
	    ((= mark gnus-spam-mark)
	     (setq gnus-newsgroup-spam-marked
		   (gnus-add-to-sorted-list gnus-newsgroup-spam-marked
					    article)))
	    ((= mark gnus-dormant-mark)
	     (setq gnus-newsgroup-dormant
		   (gnus-add-to-sorted-list gnus-newsgroup-dormant article)))
	    (t
	     (setq gnus-newsgroup-unreads
		   (gnus-add-to-sorted-list gnus-newsgroup-unreads article))))
      (gnus-alist-pull article gnus-newsgroup-reads)
      t)))

(defun gnus-summary-tick-article-forward (n)
  "Tick N articles forwards.
If N is negative, tick backwards instead.
The difference between N and the number of articles ticked is returned."
  (interactive "p")
  (gnus-summary-mark-forward n gnus-ticked-mark))

(defun gnus-summary-tick-article-backward (n)
  "Tick N articles backwards.
The difference between N and the number of articles ticked is returned."
  (interactive "p")
  (gnus-summary-mark-forward (- n) gnus-ticked-mark))

(defun gnus-summary-tick-article (&optional article clear-mark)
  "Mark current article as unread.
Optional 1st argument ARTICLE specifies article number to be marked as unread.
Optional 2nd argument CLEAR-MARK remove any kinds of mark."
  (interactive)
  (gnus-summary-mark-article article (if clear-mark gnus-unread-mark
				       gnus-ticked-mark)))

(defun gnus-summary-mark-as-read-forward (n)
  "Mark N articles as read forwards.
If N is negative, mark backwards instead.
The difference between N and the actual number of articles marked is
returned."
  (interactive "p")
  (gnus-summary-mark-forward n gnus-del-mark gnus-inhibit-user-auto-expire))

(defun gnus-summary-mark-as-read-backward (n)
  "Mark the N articles as read backwards.
The difference between N and the actual number of articles marked is
returned."
  (interactive "p")
  (gnus-summary-mark-forward
   (- n) gnus-del-mark gnus-inhibit-user-auto-expire))

(defun gnus-summary-mark-as-read (&optional article mark)
  "Mark current article as read.
ARTICLE specifies the article to be marked as read.
MARK specifies a string to be inserted at the beginning of the line."
  (gnus-summary-mark-article article mark))

(defun gnus-summary-clear-mark-forward (n)
  "Clear marks from N articles forward.
If N is negative, clear backward instead.
The difference between N and the number of marks cleared is returned."
  (interactive "p")
  (gnus-summary-mark-forward n gnus-unread-mark))

(defun gnus-summary-clear-mark-backward (n)
  "Clear marks from N articles backward.
The difference between N and the number of marks cleared is returned."
  (interactive "p")
  (gnus-summary-mark-forward (- n) gnus-unread-mark))

(defun gnus-summary-mark-unread-as-read ()
  "Intended to be used by `gnus-mark-article-hook'."
  (when (memq gnus-current-article gnus-newsgroup-unreads)
    (gnus-summary-mark-article gnus-current-article gnus-read-mark)))

(defun gnus-summary-mark-read-and-unread-as-read (&optional new-mark)
  "Intended to be used by `gnus-mark-article-hook'."
  (let ((mark (gnus-summary-article-mark)))
    (when (or (gnus-unread-mark-p mark)
	      (gnus-read-mark-p mark))
      (gnus-summary-mark-article gnus-current-article
				 (or new-mark gnus-read-mark)))))

(defun gnus-summary-mark-current-read-and-unread-as-read (&optional new-mark)
  "Intended to be used by `gnus-mark-article-hook'."
  (let ((mark (gnus-summary-article-mark)))
    (when (or (gnus-unread-mark-p mark)
	      (gnus-read-mark-p mark))
      (gnus-summary-mark-article (gnus-summary-article-number)
				 (or new-mark gnus-read-mark)))))

(defun gnus-summary-mark-unread-as-ticked ()
  "Intended to be used by `gnus-mark-article-hook'."
  (when (memq gnus-current-article gnus-newsgroup-unreads)
    (gnus-summary-mark-article gnus-current-article gnus-ticked-mark)))

(defun gnus-summary-mark-region-as-read (point mark all)
  "Mark all unread articles between point and mark as read.
If given a prefix, mark all articles between point and mark as read,
even ticked and dormant ones."
  (interactive "r\nP")
  (save-excursion
    (let (article)
      (goto-char point)
      (beginning-of-line)
      (while (and
	      (< (point) mark)
	      (progn
		(when (or all
			  (memq (setq article (gnus-summary-article-number))
				gnus-newsgroup-unreads))
		  (gnus-summary-mark-article article gnus-del-mark))
		t)
	      (gnus-summary-find-next))))))

(defun gnus-summary-mark-below (score mark)
  "Mark articles with score less than SCORE with MARK."
  (interactive "P\ncMark: ")
  (setq score (if score
		  (prefix-numeric-value score)
		(or gnus-summary-default-score 0)))
  (with-current-buffer gnus-summary-buffer
    (goto-char (point-min))
    (while
	(progn
	  (and (< (gnus-summary-article-score) score)
	       (gnus-summary-mark-article nil mark))
	  (gnus-summary-find-next)))))

(defun gnus-summary-kill-below (&optional score)
  "Mark articles with score below SCORE as read."
  (interactive "P")
  (gnus-summary-mark-below score gnus-killed-mark))

(defun gnus-summary-clear-above (&optional score)
  "Clear all marks from articles with score above SCORE."
  (interactive "P")
  (gnus-summary-mark-above score gnus-unread-mark))

(defun gnus-summary-tick-above (&optional score)
  "Tick all articles with score above SCORE."
  (interactive "P")
  (gnus-summary-mark-above score gnus-ticked-mark))

(defun gnus-summary-mark-above (score mark)
  "Mark articles with score over SCORE with MARK."
  (interactive "P\ncMark: ")
  (setq score (if score
		  (prefix-numeric-value score)
		(or gnus-summary-default-score 0)))
  (with-current-buffer gnus-summary-buffer
    (goto-char (point-min))
    (while (and (progn
		  (when (> (gnus-summary-article-score) score)
		    (gnus-summary-mark-article nil mark))
		  t)
		(gnus-summary-find-next)))))

;; Suggested by Daniel Quinlan <quinlan@best.com>.
(defalias 'gnus-summary-show-all-expunged 'gnus-summary-limit-include-expunged)
(defun gnus-summary-limit-include-expunged (&optional no-error)
  "Display all the hidden articles that were expunged for low scores."
  (interactive)
  (let ((inhibit-read-only t))
    (let ((scored gnus-newsgroup-scored)
	  headers h)
      (while scored
	(unless (gnus-summary-article-header (caar scored))
	  (and (setq h (gnus-number-to-header (caar scored)))
	       (< (cdar scored) gnus-summary-expunge-below)
	       (push h headers)))
	(setq scored (cdr scored)))
      (if (not headers)
	  (when (not no-error)
	    (error "No expunged articles hidden"))
	(goto-char (point-min))
	(push gnus-newsgroup-limit gnus-newsgroup-limits)
	(setq gnus-newsgroup-limit (copy-sequence gnus-newsgroup-limit))
	(dolist (x headers)
	  (push (mail-header-number x) gnus-newsgroup-limit))
	(gnus-summary-prepare-unthreaded (nreverse headers))
	(goto-char (point-min))
	(gnus-summary-position-point)
	t))))

(defun gnus-summary-catchup (&optional all quietly to-here not-mark reverse)
  "Mark all unread articles in this newsgroup as read.
If prefix argument ALL is non-nil, ticked and dormant articles will
also be marked as read.
If QUIETLY is non-nil, no questions will be asked.

If TO-HERE is non-nil, it should be a point in the buffer.  All
articles before (after, if REVERSE is set) this point will be marked
as read.

Note that this function will only catch up the unread article
in the current summary buffer limitation.

The number of articles marked as read is returned."
  (interactive "P")
  (prog1
      (save-excursion
	(when (or quietly
		  (not gnus-interactive-catchup) ;Without confirmation?
		  gnus-expert-user
		  (gnus-y-or-n-p
		   (if all
		       "Mark absolutely all articles as read? "
		     "Mark all unread articles as read? ")))
	  (if (and not-mark
		   (not gnus-newsgroup-adaptive)
		   (not gnus-newsgroup-auto-expire)
		   (not gnus-suppress-duplicates)
		   (or (not gnus-use-cache)
		       (eq gnus-use-cache 'passive)))
	      (progn
		(when all
		  (setq gnus-newsgroup-marked nil
			gnus-newsgroup-spam-marked nil
			gnus-newsgroup-dormant nil))
		(setq gnus-newsgroup-unreads
		      (gnus-sorted-nunion
                       (gnus-sorted-intersection gnus-newsgroup-unreads
						 gnus-newsgroup-downloadable)
		       (gnus-sorted-difference gnus-newsgroup-unfetched
					       gnus-newsgroup-cached))))
	    ;; We actually mark all articles as canceled, which we
	    ;; have to do when using auto-expiry or adaptive scoring.
	    (gnus-summary-show-all-threads)
	    (if (and to-here reverse)
		(progn
		  (goto-char to-here)
		  (gnus-summary-mark-current-read-and-unread-as-read
		   gnus-catchup-mark)
		  (while (gnus-summary-find-next (not all))
		    (gnus-summary-mark-article-as-read gnus-catchup-mark)))
	      (when (gnus-summary-first-subject (not all))
		(while (and
			(if to-here (< (point) to-here) t)
			(gnus-summary-mark-article-as-read gnus-catchup-mark)
			(gnus-summary-find-next (not all))))))
	    (gnus-set-mode-line 'summary))
	  t))
    (gnus-summary-position-point)))

(defun gnus-summary-catchup-to-here (&optional all)
  "Mark all unticked articles before the current one as read.
If ALL is non-nil, also mark ticked and dormant articles as read."
  (interactive "P")
  (save-excursion
    (gnus-save-hidden-threads
      (let ((beg (point)))
	;; We check that there are unread articles.
	(when (or all (gnus-summary-find-prev))
	  (gnus-summary-catchup all t beg)))))
  (gnus-summary-position-point))

(defun gnus-summary-catchup-from-here (&optional all)
  "Mark all unticked articles after (and including) the current one as read.
If ALL is non-nil, also mark ticked and dormant articles as read."
  (interactive "P")
  (save-excursion
    (gnus-save-hidden-threads
      (let ((beg (point)))
	;; We check that there are unread articles.
	(when (or all (gnus-summary-find-next))
	  (gnus-summary-catchup all t beg nil t)))))
  (gnus-summary-position-point))

(defun gnus-summary-catchup-all (&optional quietly)
  "Mark all articles in this newsgroup as read.
This command is dangerous.  Normally, you want \\[gnus-summary-catchup]
instead, which marks only unread articles as read."
  (interactive "P")
  (gnus-summary-catchup t quietly))

(defun gnus-summary-catchup-and-exit (&optional all quietly)
  "Mark all unread articles in this group as read, then exit.
If prefix argument ALL is non-nil, all articles are marked as read.
If QUIETLY is non-nil, no questions will be asked."
  (interactive "P")
  (when (gnus-summary-catchup all quietly nil 'fast)
    ;; Select next newsgroup or exit.
    (if (and (not (gnus-group-quit-config gnus-newsgroup-name))
	     (eq gnus-auto-select-next 'quietly))
	(gnus-summary-next-group nil)
      (gnus-summary-exit))))

(defun gnus-summary-catchup-all-and-exit (&optional quietly)
  "Mark all articles in this newsgroup as read, and then exit.
This command is dangerous.  Normally, you want \\[gnus-summary-catchup-and-exit]
instead, which marks only unread articles as read."
  (interactive "P")
  (gnus-summary-catchup-and-exit t quietly))

(defun gnus-summary-catchup-and-goto-next-group (&optional all)
  "Mark all articles in this group as read and select the next group.
If given a prefix, mark all articles, unread as well as ticked, as
read."
  (interactive "P")
  (save-excursion
    (gnus-summary-catchup all))
  (gnus-summary-next-group))

(defun gnus-summary-catchup-and-goto-prev-group (&optional all)
  "Mark all articles in this group as read and select the previous group.
If given a prefix, mark all articles, unread as well as ticked, as
read."
  (interactive "P")
  (save-excursion
    (gnus-summary-catchup all))
  (gnus-summary-next-group nil nil t))

;;;
;;; with article
;;;

(defmacro gnus-with-article (article &rest forms)
  "Select ARTICLE and perform FORMS in the original article buffer.
Then replace the article with the result."
  `(progn
     ;; We don't want the article to be marked as read.
     (let (gnus-mark-article-hook)
       (gnus-summary-select-article t t nil ,article))
     (set-buffer gnus-original-article-buffer)
     ,@forms
     (if (not (gnus-check-backend-function
	       'request-replace-article (car gnus-article-current)))
	 (gnus-message 5 "Read-only group; not replacing")
       (unless (gnus-request-replace-article
		,article (car gnus-article-current)
		(current-buffer) t)
	 (error "Couldn't replace article")))
     ;; The cache and backlog have to be flushed somewhat.
     (when gnus-keep-backlog
       (gnus-backlog-remove-article
	(car gnus-article-current) (cdr gnus-article-current)))
     (when gnus-use-cache
       (gnus-cache-update-article
	(car gnus-article-current) (cdr gnus-article-current)))))

(put 'gnus-with-article 'lisp-indent-function 1)
(put 'gnus-with-article 'edebug-form-spec '(form body))

;; Thread-based commands.

(defun gnus-summary-articles-in-thread (&optional article)
  "Return a list of all articles in the current thread.
If ARTICLE is non-nil, return all articles in the thread that starts
with that article."
  (let* ((article (or article (gnus-summary-article-number)))
	 (data (gnus-data-find-list article))
	 (top-level (gnus-data-level (car data)))
	 (top-subject
	  (cond ((null gnus-thread-operation-ignore-subject)
		 (gnus-simplify-subject-re
		  (mail-header-subject (gnus-data-header (car data)))))
		((eq gnus-thread-operation-ignore-subject 'fuzzy)
		 (gnus-simplify-subject-fuzzy
		  (mail-header-subject (gnus-data-header (car data)))))
		(t nil)))
	 (end-point (save-excursion
		      (goto-char (gnus-data-pos (car data)))
		      (if (gnus-summary-go-to-next-thread)
			  (point) (point-max))))
	 articles)
    (while (and data
		(< (gnus-data-pos (car data)) end-point))
      (when (or (not top-subject)
		(string= top-subject
			 (if (eq gnus-thread-operation-ignore-subject 'fuzzy)
			     (gnus-simplify-subject-fuzzy
			      (mail-header-subject
			       (gnus-data-header (car data))))
			   (gnus-simplify-subject-re
			    (mail-header-subject
			     (gnus-data-header (car data)))))))
	(push (gnus-data-number (car data)) articles))
      (unless (and (setq data (cdr data))
		   (> (gnus-data-level (car data)) top-level))
	(setq data nil)))
    ;; Return the list of articles.
    (nreverse articles)))

(defun gnus-summary-rethread-current ()
  "Rethread the thread the current article is part of."
  (interactive)
  (let* ((gnus-show-threads t)
	 (article (gnus-summary-article-number))
	 (id (mail-header-id (gnus-summary-article-header)))
	 (gnus-newsgroup-threads (list (gnus-id-to-thread (gnus-root-id id)))))
    (unless id
      (error "No article on the current line"))
    (gnus-rebuild-thread id)
    (gnus-summary-goto-subject article)))

(defun gnus-summary-reparent-thread ()
  "Make the current article child of the marked (or previous) article.

Note that the re-threading will only work if `gnus-thread-ignore-subject'
is non-nil or the Subject: of both articles are the same."
  (interactive)
  (unless (not (gnus-group-read-only-p))
    (error "The current newsgroup does not support article editing"))
  (unless (<= (length gnus-newsgroup-processable) 1)
    (error "No more than one article may be marked"))
  (let ((child (gnus-summary-article-number))
	;; First grab the marked article, otherwise one line up.
	(parent (if (not (null gnus-newsgroup-processable))
		    (car gnus-newsgroup-processable)
		  (save-excursion
		    (if (eq (forward-line -1) 0)
			(gnus-summary-article-number)
		      (error "Beginning of summary buffer"))))))
    (gnus-summary-reparent-children parent (list child))))

(defun gnus-summary-reparent-children (parent children)
  "Make PARENT the parent of CHILDREN.
When called interactively, PARENT is the current article and CHILDREN
are the process-marked articles."
  (interactive
   (list (gnus-summary-article-number)
	 (gnus-summary-work-articles nil)))
  (dolist (child children)
    (save-window-excursion
      (let ((gnus-article-buffer " *reparent*"))
	(unless (not (eq parent child))
	  (error "An article may not be self-referential"))
	(let ((message-id (mail-header-id
			   (gnus-summary-article-header parent))))
	  (unless (and message-id (not (equal message-id "")))
	    (error "No message-id in desired parent"))
	  (gnus-with-article child
	    (save-restriction
	      (goto-char (point-min))
	      (message-narrow-to-head)
	      (if (re-search-forward "^References: " nil t)
		  (progn
		    (re-search-forward "^[^ \t]" nil t)
		    (forward-line -1)
		    (end-of-line)
		    (insert " " message-id))
		(insert "References: " message-id "\n"))))
	  (set-buffer gnus-summary-buffer)
	  (gnus-summary-unmark-all-processable)
	  (gnus-summary-update-article child)
	  (when (gnus-summary-goto-subject (cdr gnus-article-current) nil t)
	    (gnus-summary-update-secondary-mark (cdr gnus-article-current)))
	  (gnus-summary-rethread-current)
	  (gnus-message 3 "Article %d is now the child of article %d"
			child parent))))))

(defun gnus-summary-toggle-threads (&optional arg)
  "Toggle showing conversation threads.
If ARG is positive number, turn showing conversation threads on."
  (interactive "P")
  (let ((current (or (gnus-summary-article-number) gnus-newsgroup-end)))
    (setq gnus-show-threads
	  (if (null arg) (not gnus-show-threads)
	    (> (prefix-numeric-value arg) 0)))
    (gnus-summary-prepare)
    (gnus-summary-goto-subject current)
    (gnus-message 6 "Threading is now %s" (if gnus-show-threads "on" "off"))
    (gnus-summary-position-point)))

(eval-and-compile
  (if (fboundp 'remove-overlays)
      (defalias 'gnus-remove-overlays 'remove-overlays)
    (defun gnus-remove-overlays (beg end name val)
      "Clear BEG and END of overlays whose property NAME has value VAL.
For compatibility with XEmacs."
      (dolist (ov (gnus-overlays-in beg end))
	(when (eq (gnus-overlay-get ov name) val)
	  (gnus-delete-overlay ov))))))

(defun gnus-summary-show-all-threads ()
  "Show all threads."
  (interactive)
  (gnus-remove-overlays (point-min) (point-max) 'invisible 'gnus-sum)
  (gnus-summary-position-point))

(defsubst gnus-summary--inv (p)
  (and (eq (get-char-property p 'invisible) 'gnus-sum) p))

(defun gnus-summary-show-thread ()
  "Show thread subtrees.
Returns nil if no thread was there to be shown."
  (interactive)
  (let* ((orig (point))
	 (end (point-at-eol))
         (end (or (gnus-summary--inv end) (gnus-summary--inv (1- end))))
	 ;; Leave point at bol
	 (beg (progn (beginning-of-line) (if (bobp) (point) (1- (point)))))
	 (eoi (when end
		(if (fboundp 'next-single-char-property-change)
		    ;; Note: XEmacs version of n-s-c-p-c may return nil
		    (or (next-single-char-property-change end 'invisible)
			(point-max))
		  (while (progn
			   (end-of-line 2)
			   (and (not (eobp))
				(eq (get-char-property (point) 'invisible)
				    'gnus-sum))))
		  (point)))))
    (when eoi
      (gnus-remove-overlays beg eoi 'invisible 'gnus-sum)
      (goto-char orig)
      (gnus-summary-position-point)
      eoi)))

(defun gnus-summary-maybe-hide-threads ()
  "If requested, hide the threads that should be hidden."
  (when (and gnus-show-threads
	     gnus-thread-hide-subtree)
    (gnus-summary-hide-all-threads
     (if (or (consp gnus-thread-hide-subtree)
	     (functionp gnus-thread-hide-subtree))
	 (gnus-make-predicate gnus-thread-hide-subtree)
       nil))))

;;; Hiding predicates.

(defun gnus-article-unread-p (header)
  (memq (mail-header-number header) gnus-newsgroup-unreads))

(defun gnus-article-unseen-p (header)
  (memq (mail-header-number header) gnus-newsgroup-unseen))

(defun gnus-map-articles (predicate articles)
  "Map PREDICATE over ARTICLES and return non-nil if any predicate is non-nil."
  (apply 'gnus-or (mapcar predicate
			  (mapcar (lambda (number)
				    (gnus-summary-article-header number))
				  articles))))

(defun gnus-summary-hide-all-threads (&optional predicate)
  "Hide all thread subtrees.
If PREDICATE is supplied, threads that satisfy this predicate
will not be hidden."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((end nil)
          (count 0))
      (while (not end)
        (incf count)
        (when (zerop (mod count 1000))
          (message "Hiding all threads... %d" count))
	(when (or (not predicate)
		  (gnus-map-articles
		   predicate (gnus-summary-article-children)))
	    (gnus-summary-hide-thread))
	(setq end (not (zerop (gnus-summary-next-thread 1 t)))))))
  (gnus-summary-position-point))

(defun gnus-summary-hide-thread ()
  "Hide thread subtrees.
If PREDICATE is supplied, threads that satisfy this predicate
will not be hidden.
Returns nil if no threads were there to be hidden."
  (interactive)
  (let ((start (point))
	(starteol (line-end-position))
	(article (gnus-summary-article-number)))
    (goto-char start)
    ;; Go forward until either the buffer ends or the subthread ends.
    (when (and (not (eobp))
	       (or (zerop (gnus-summary-next-thread 1 t))
		   (goto-char (point-max))))
      (if (and (> (point) start)
	       ;; FIXME: this should actually search for a non-invisible \n.
	       (search-backward "\n" start t))
	  (progn
	    (when (> (point) starteol)
	      (gnus-remove-overlays starteol (point) 'invisible 'gnus-sum)
	      (let ((ol (gnus-make-overlay starteol (point) nil t nil)))
		(gnus-overlay-put ol 'invisible 'gnus-sum)
		(gnus-overlay-put ol 'evaporate t)))
	    (gnus-summary-goto-subject article)
            (when (> start (point))
              (message "Hiding the thread moved us backwards, aborting!")
              (goto-char (point-max))))
	(goto-char start)
	nil))))

(defun gnus-summary-go-to-next-thread (&optional previous)
  "Go to the same level (or less) next thread.
If PREVIOUS is non-nil, go to previous thread instead.
Return the article number moved to, or nil if moving was impossible."
  (let ((level (gnus-summary-thread-level))
	(way (if previous -1 1))
	(beg (point)))
    (forward-line way)
    (while (and (not (eobp))
		(< level (gnus-summary-thread-level)))
      (forward-line way))
    (if (eobp)
	(progn
	  (goto-char beg)
	  nil)
      (setq beg (point))
      (prog1
	  (gnus-summary-article-number)
	(goto-char beg)))))

(defun gnus-summary-next-thread (n &optional silent)
  "Go to the same level next N'th thread.
If N is negative, search backward instead.
Returns the difference between N and the number of skips actually
done.

If SILENT, don't output messages."
  (interactive "p")
  (let ((backward (< n 0))
	(n (abs n)))
    (while (and (> n 0)
		(gnus-summary-go-to-next-thread backward))
      (decf n))
    (unless silent
      (gnus-summary-position-point))
    (when (and (not silent) (/= 0 n))
      (gnus-message 7 "No more threads"))
    n))

(defun gnus-summary-prev-thread (n)
  "Go to the same level previous N'th thread.
Returns the difference between N and the number of skips actually
done."
  (interactive "p")
  (gnus-summary-next-thread (- n)))

(defun gnus-summary-go-down-thread ()
  "Go down one level in the current thread."
  (let ((children (gnus-summary-article-children)))
    (when children
      (gnus-summary-goto-subject (car children)))))

(defun gnus-summary-go-up-thread ()
  "Go up one level in the current thread."
  (let ((parent (gnus-summary-article-parent)))
    (when parent
      (gnus-summary-goto-subject parent))))

(defun gnus-summary-down-thread (n)
  "Go down thread N steps.
If N is negative, go up instead.
Returns the difference between N and how many steps down that were
taken."
  (interactive "p")
  (let ((up (< n 0))
	(n (abs n)))
    (while (and (> n 0)
		(if up (gnus-summary-go-up-thread)
		  (gnus-summary-go-down-thread)))
      (setq n (1- n)))
    (gnus-summary-position-point)
    (when (/= 0 n)
      (gnus-message 7 "Can't go further"))
    n))

(defun gnus-summary-up-thread (n)
  "Go up thread N steps.
If N is negative, go down instead.
Returns the difference between N and how many steps down that were
taken."
  (interactive "p")
  (gnus-summary-down-thread (- n)))

(defun gnus-summary-top-thread ()
  "Go to the top of the thread."
  (interactive)
  (while (gnus-summary-go-up-thread))
  (gnus-summary-article-number))

(defun gnus-summary-expire-thread ()
  "Mark articles under current thread as expired."
  (interactive)
  (gnus-summary-kill-thread 0))

(defun gnus-summary-kill-thread (&optional unmark)
  "Mark articles under current thread as read.
If the prefix argument is positive, remove any kinds of marks.
If the prefix argument is zero, mark thread as expired.
If the prefix argument is negative, tick articles instead."
  (interactive "P")
  (when unmark
    (setq unmark (prefix-numeric-value unmark)))
  (let ((articles (gnus-summary-articles-in-thread))
	(hide (or (null unmark) (= unmark 0))))
    (save-excursion
      ;; Expand the thread.
      (gnus-summary-show-thread)
      ;; Mark all the articles.
      (while articles
	(gnus-summary-goto-subject (car articles))
	(cond ((null unmark)
	       (gnus-summary-mark-article-as-read gnus-killed-mark))
	      ((> unmark 0)
	       (gnus-summary-mark-article-as-unread gnus-unread-mark))
	      ((= unmark 0)
	       (gnus-summary-mark-article nil gnus-expirable-mark))
	      (t
	       (gnus-summary-mark-article-as-unread gnus-ticked-mark)))
	(setq articles (cdr articles))))
    ;; Hide killed subtrees when hide is true.
    (and hide
	 gnus-thread-hide-killed
	 (gnus-summary-hide-thread))
    ;; If hide is t, go to next unread subject.
    (when hide
      ;; Go to next unread subject.
      (gnus-summary-next-subject 1 t)))
  (gnus-set-mode-line 'summary))

;; Summary sorting commands

(defun gnus-summary-sort-by-number (&optional reverse)
  "Sort the summary buffer by article number.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-summary-sort 'number reverse))

(defun gnus-summary-sort-by-most-recent-number (&optional reverse)
  "Sort the summary buffer by most recent article number.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-summary-sort 'most-recent-number reverse))

(defun gnus-summary-sort-by-random (&optional reverse)
  "Randomize the order in the summary buffer.
Argument REVERSE means to randomize in reverse order."
  (interactive "P")
  (gnus-summary-sort 'random reverse))

(defun gnus-summary-sort-by-author (&optional reverse)
  "Sort the summary buffer by author name alphabetically.
If `case-fold-search' is non-nil, case of letters is ignored.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-summary-sort 'author reverse))

(defun gnus-summary-sort-by-recipient (&optional reverse)
  "Sort the summary buffer by recipient name alphabetically.
If `case-fold-search' is non-nil, case of letters is ignored.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-summary-sort 'recipient reverse))

(defun gnus-summary-sort-by-subject (&optional reverse)
  "Sort the summary buffer by subject alphabetically.  `Re:'s are ignored.
If `case-fold-search' is non-nil, case of letters is ignored.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-summary-sort 'subject reverse))

(defun gnus-summary-sort-by-date (&optional reverse)
  "Sort the summary buffer by date.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-summary-sort 'date reverse))

(defun gnus-summary-sort-by-most-recent-date (&optional reverse)
  "Sort the summary buffer by most recent date.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-summary-sort 'most-recent-date reverse))

(defun gnus-summary-sort-by-score (&optional reverse)
  "Sort the summary buffer by score.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-summary-sort 'score reverse))

(defun gnus-summary-sort-by-lines (&optional reverse)
  "Sort the summary buffer by the number of lines.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-summary-sort 'lines reverse))

(defun gnus-summary-sort-by-chars (&optional reverse)
  "Sort the summary buffer by article length.
Argument REVERSE means reverse order."
  (interactive "P")
  (gnus-summary-sort 'chars reverse))

(defun gnus-summary-sort-by-original (&optional reverse)
  "Sort the summary buffer using the default sorting method.
Argument REVERSE means reverse order."
  (interactive "P")
  (let* ((inhibit-read-only t)
	 (gnus-summary-prepare-hook nil))
    ;; We do the sorting by regenerating the threads.
    (gnus-summary-prepare)
    ;; Hide subthreads if needed.
    (gnus-summary-maybe-hide-threads)))

(defun gnus-summary-sort (predicate reverse)
  "Sort summary buffer by PREDICATE.  REVERSE means reverse order."
  (let* ((thread (intern (format "gnus-thread-sort-by-%s" predicate)))
	 (article (intern (format "gnus-article-sort-by-%s" predicate)))
	 (gnus-thread-sort-functions
	  (if (not reverse)
	      thread
	    `(lambda (t1 t2)
	       (,thread t2 t1))))
	 (gnus-sort-gathered-threads-function
	  gnus-thread-sort-functions)
	 (gnus-article-sort-functions
	  (if (not reverse)
	      article
	    `(lambda (t1 t2)
	       (,article t2 t1))))
	 (inhibit-read-only t)
	 (gnus-summary-prepare-hook nil))
    ;; We do the sorting by regenerating the threads.
    (gnus-summary-prepare)
    ;; Hide subthreads if needed.
    (gnus-summary-maybe-hide-threads)))

;; Summary saving commands.

(defun gnus-summary-save-article (&optional n not-saved)
  "Save the current article using the default saver function.
If N is a positive number, save the N next articles.
If N is a negative number, save the N previous articles.
If N is nil and any articles have been marked with the process mark,
save those articles instead.
The variable `gnus-default-article-saver' specifies the saver function.

If the optional second argument NOT-SAVED is non-nil, articles saved
will not be marked as saved."
  (interactive "P")
  (require 'gnus-art)
  (let* ((articles (gnus-summary-work-articles n))
	 (save-buffer (save-excursion
			(nnheader-set-temp-buffer " *Gnus Save*")))
	 (num (length articles))
	 ;; Whether to save decoded articles or raw articles.
	 (decode (when gnus-article-save-coding-system
		   (get gnus-default-article-saver :decode)))
	 ;; When saving many articles in a single file, use the other
	 ;; function to save articles other than the first one.
	 (saver2 (get gnus-default-article-saver :function))
	 (gnus-prompt-before-saving (if saver2
					t
				      gnus-prompt-before-saving))
	 (gnus-default-article-saver gnus-default-article-saver)
	 header file)
    (dolist (article articles)
      (setq header (gnus-summary-article-header article))
      (if (not (vectorp header))
	  ;; This is a pseudo-article.
	  (if (assq 'name header)
	      (gnus-copy-file (cdr (assq 'name header)))
	    (gnus-message 1 "Article %d is unsavable" article))
	;; This is a real article.
	(save-window-excursion
	  (gnus-summary-select-article decode decode nil article)
	  (gnus-summary-goto-subject article))
	(with-current-buffer save-buffer
	  (erase-buffer)
	  (insert-buffer-substring (if decode
				       gnus-article-buffer
				     gnus-original-article-buffer)))
	(setq file (gnus-article-save save-buffer file num))
	(gnus-summary-remove-process-mark article)
	(unless not-saved
	  (gnus-summary-set-saved-mark article)))
      (when saver2
	(setq gnus-default-article-saver saver2
	      saver2 nil)))
    (gnus-kill-buffer save-buffer)
    (gnus-summary-position-point)
    (gnus-set-mode-line 'summary)
    n))

(defun gnus-summary-pipe-output (&optional n sym)
  "Pipe the current article to a subprocess.
If N is a positive number, pipe the N next articles.
If N is a negative number, pipe the N previous articles.
If N is nil and any articles have been marked with the process mark,
pipe those articles instead.
The default command to which articles are piped is specified by the
variable `gnus-summary-pipe-output-default-command'; if it is nil, you
will be prompted for the command.

The properties `:decode' and `:headers' that are put to the function
symbol `gnus-summary-save-in-pipe' control whether this function
decodes articles and what headers to keep (see the doc string for the
`gnus-default-article-saver' variable).  If SYM (the symbolic prefix)
is neither omitted nor the symbol `r', force including all headers
regardless of the `:headers' property.  If it is the symbol `r',
articles that are not decoded and include all headers will be piped
no matter what the properties `:decode' and `:headers' are."
  (interactive (gnus-interactive "P\ny"))
  (require 'gnus-art)
  (let* ((articles (gnus-summary-work-articles n))
	 (result-buffer "*Shell Command Output*")
	 (all-headers (not (memq sym '(nil r))))
	 (gnus-save-all-headers (or all-headers gnus-save-all-headers))
	 (raw (eq sym 'r))
	 (headers (get 'gnus-summary-save-in-pipe :headers))
	 command result)
    (unless (numberp (car articles))
      (error "No article to pipe"))
    (setq command (gnus-read-shell-command
		   (concat "Shell command on "
			   (if (cdr articles)
			       (format "these %d articles" (length articles))
			     "this article")
			   ": ")
		   gnus-summary-pipe-output-default-command))
    (when (string-equal command "")
      (error "A command is required"))
    (when all-headers
      (put 'gnus-summary-save-in-pipe :headers nil))
    (unwind-protect
	(while articles
	  (gnus-summary-goto-subject (pop articles))
	  (save-window-excursion (gnus-summary-save-in-pipe command raw))
	  (when (and (get-buffer result-buffer)
		     (not (zerop (buffer-size (get-buffer result-buffer)))))
	    (setq result (concat result (with-current-buffer result-buffer
					  (buffer-string))))))
      (put 'gnus-summary-save-in-pipe :headers headers))
    (unless (zerop (length result))
      (if (with-current-buffer (get-buffer-create result-buffer)
	    (erase-buffer)
	    (insert result)
	    (prog1
		(and (= (count-lines (point-min) (point)) 1)
		     (progn
		       (end-of-line 0)
		       (<= (current-column)
			   (window-width (minibuffer-window)))))
	      (goto-char (point-min))))
	  (message "%s" (substring result 0 -1))
	(message nil)
	(gnus-configure-windows 'pipe)))))

(defun gnus-summary-save-article-mail (&optional arg)
  "Append the current article to a Unix mail box file.
If N is a positive number, save the N next articles.
If N is a negative number, save the N previous articles.
If N is nil and any articles have been marked with the process mark,
save those articles instead."
  (interactive "P")
  (require 'gnus-art)
  (let ((gnus-default-article-saver 'gnus-summary-save-in-mail))
    (gnus-summary-save-article arg)))

(defun gnus-summary-save-article-rmail (&optional arg)
  "Append the current article to an rmail file.
If N is a positive number, save the N next articles.
If N is a negative number, save the N previous articles.
If N is nil and any articles have been marked with the process mark,
save those articles instead."
  (interactive "P")
  (require 'gnus-art)
  (let ((gnus-default-article-saver 'gnus-summary-save-in-rmail))
    (gnus-summary-save-article arg)))

(defun gnus-summary-save-article-file (&optional arg)
  "Append the current article to a file.
If N is a positive number, save the N next articles.
If N is a negative number, save the N previous articles.
If N is nil and any articles have been marked with the process mark,
save those articles instead."
  (interactive "P")
  (require 'gnus-art)
  (let ((gnus-default-article-saver 'gnus-summary-save-in-file))
    (gnus-summary-save-article arg)))

(defun gnus-summary-write-article-file (&optional arg)
  "Write the current article to a file, deleting the previous file.
If N is a positive number, save the N next articles.
If N is a negative number, save the N previous articles.
If N is nil and any articles have been marked with the process mark,
save those articles instead."
  (interactive "P")
  (require 'gnus-art)
  (let ((gnus-default-article-saver 'gnus-summary-write-to-file))
    (gnus-summary-save-article arg)))

(defun gnus-summary-save-article-body-file (&optional arg)
  "Append the current article body to a file.
If N is a positive number, save the N next articles.
If N is a negative number, save the N previous articles.
If N is nil and any articles have been marked with the process mark,
save those articles instead."
  (interactive "P")
  (require 'gnus-art)
  (let ((gnus-default-article-saver 'gnus-summary-save-body-in-file))
    (gnus-summary-save-article arg)))

(defun gnus-summary-write-article-body-file (&optional arg)
  "Write the current article body to a file, deleting the previous file.
If N is a positive number, save the N next articles.
If N is a negative number, save the N previous articles.
If N is nil and any articles have been marked with the process mark,
save those articles instead."
  (interactive "P")
  (require 'gnus-art)
  (let ((gnus-default-article-saver 'gnus-summary-write-body-to-file))
    (gnus-summary-save-article arg)))

(defun gnus-summary-muttprint (&optional arg)
  "Print the current article using Muttprint.
If N is a positive number, save the N next articles.
If N is a negative number, save the N previous articles.
If N is nil and any articles have been marked with the process mark,
save those articles instead."
  (interactive "P")
  (require 'gnus-art)
  (let ((gnus-default-article-saver 'gnus-summary-pipe-to-muttprint))
    (gnus-summary-save-article arg t)))

(defun gnus-summary-pipe-message (program)
  "Pipe the current article through PROGRAM."
  (interactive "sProgram: ")
  (gnus-summary-select-article)
  (let ((mail-header-separator ""))
    (gnus-eval-in-buffer-window gnus-article-buffer
      (save-restriction
	(widen)
	(let ((start (window-start))
	      (inhibit-read-only t))
	  (message-pipe-buffer-body program)
	  (set-window-start (get-buffer-window (current-buffer)) start))))))

(defun gnus-get-split-value (methods)
  "Return a value based on the split METHODS."
  (let (split-name method result match)
    (when methods
      (with-current-buffer gnus-original-article-buffer
	(save-restriction
	  (nnheader-narrow-to-headers)
	  (while (and methods (not split-name))
	    (goto-char (point-min))
	    (setq method (pop methods))
	    (setq match (car method))
	    (when (cond
		   ((stringp match)
		    ;; Regular expression.
		    (ignore-errors
		      (re-search-forward match nil t)))
		   ((functionp match)
		    ;; Function.
		    (save-restriction
		      (widen)
		      (setq result (funcall match gnus-newsgroup-name))))
		   ((consp match)
		    ;; Form.
		    (save-restriction
		      (widen)
		      (setq result (eval match)))))
	      (setq split-name (cdr method))
	      (cond ((stringp result)
		     (push (expand-file-name
			    result gnus-article-save-directory)
			   split-name))
		    ((consp result)
		     (setq split-name (append result split-name)))))))))
    (nreverse split-name)))

(defun gnus-valid-move-group-p (group)
  (and (symbolp group)
       (boundp group)
       (symbol-name group)
       (symbol-value group)
       (gnus-get-function (gnus-find-method-for-group
			   (symbol-name group)) 'request-accept-article t)))

(defun gnus-read-move-group-name (prompt default articles prefix)
  "Read a group name."
  (let* ((split-name (gnus-get-split-value gnus-move-split-methods))
	 (minibuffer-confirm-incomplete nil) ; XEmacs
	 (prom
	  (format "%s %s to"
		  prompt
		  (if (> (length articles) 1)
		      (format "these %d articles" (length articles))
		    "this article")))
	 (to-newsgroup
          (cond
           ((null split-name)
            (gnus-group-completing-read
             prom
             (gnus-remove-if-not 'gnus-valid-move-group-p gnus-active-hashtb t)
             nil prefix nil default))
           ((= 1 (length split-name))
            (gnus-group-completing-read
             prom
	     (gnus-remove-if-not 'gnus-valid-move-group-p gnus-active-hashtb t)
             nil prefix 'gnus-group-history (car split-name)))
           (t
            (gnus-completing-read
             prom (nreverse split-name) nil nil 'gnus-group-history))))
         (to-method (gnus-server-to-method (gnus-group-method to-newsgroup)))
	 encoded)
    (when to-newsgroup
      (if (or (string= to-newsgroup "")
	      (string= to-newsgroup prefix))
	  (setq to-newsgroup default))
      (unless to-newsgroup
	(error "No group name entered"))
      (setq encoded (mm-encode-coding-string
		     to-newsgroup
		     (gnus-group-name-charset to-method to-newsgroup)))
      (or (gnus-active encoded)
	  (gnus-activate-group encoded nil nil to-method)
	  (if (gnus-y-or-n-p (format "No such group: %s.  Create it? "
				     to-newsgroup))
	      (or (and (gnus-request-create-group encoded to-method)
		       (gnus-activate-group encoded nil nil to-method)
		       (gnus-subscribe-group encoded))
		  (error "Couldn't create group %s" to-newsgroup)))
	  (error "No such group: %s" to-newsgroup))
      encoded)))

(defvar gnus-summary-save-parts-counter)
(declare-function mm-uu-dissect "mm-uu" (&optional noheader mime-type))

(defun gnus-summary-save-parts (type dir n &optional reverse)
  "Save parts matching TYPE to DIR.
If REVERSE, save parts that do not match TYPE."
  (interactive
   (list (read-string "Save parts of type: "
		      (or (car gnus-summary-save-parts-type-history)
			  gnus-summary-save-parts-default-mime)
		      'gnus-summary-save-parts-type-history)
	 (setq gnus-summary-save-parts-last-directory
	       (read-directory-name "Save to directory: "
                                    gnus-summary-save-parts-last-directory
                                    nil t))
	 current-prefix-arg))
  (gnus-summary-iterate n
    (let ((gnus-display-mime-function nil)
	  gnus-article-prepare-hook
	  gnus-article-decode-hook
	  gnus-display-mime-function
	  gnus-break-pages
	  (gnus-inhibit-treatment t))
      (gnus-summary-select-article))
    (with-current-buffer gnus-article-buffer
      (let ((handles (or gnus-article-mime-handles
			 (mm-dissect-buffer nil gnus-article-loose-mime)
			 (and gnus-article-emulate-mime
			      (mm-uu-dissect))))
	    (gnus-summary-save-parts-counter 1))
	(when handles
	  (gnus-summary-save-parts-1 type dir handles reverse)
	  (unless gnus-article-mime-handles ;; Don't destroy this case.
	    (mm-destroy-parts handles)))))))

(defun gnus-summary-save-parts-1 (type dir handle reverse)
  (if (stringp (car handle))
      (mapcar (lambda (h) (gnus-summary-save-parts-1 type dir h reverse))
	      (cdr handle))
    (when (if reverse
	      (not (string-match type (mm-handle-media-type handle)))
	    (string-match type (mm-handle-media-type handle)))
      (let ((file (expand-file-name
		   (gnus-map-function
		    mm-file-name-rewrite-functions
		    (file-name-nondirectory
		     (or
                      (mm-handle-filename handle)
		      (format "%s.%d.%d" gnus-newsgroup-name
			      (cdr gnus-article-current)
			      gnus-summary-save-parts-counter))))
		   dir)))
	(incf gnus-summary-save-parts-counter)
	(unless (file-exists-p file)
	  (mm-save-part-to-file handle file))))))

;; Summary extract commands

(defun gnus-summary-insert-pseudos (pslist &optional not-view)
  (let ((inhibit-read-only t)
	(article (gnus-summary-article-number))
	after-article b e)
    (unless (gnus-summary-goto-subject article)
      (error "No such article: %d" article))
    (gnus-summary-position-point)
    ;; If all commands are to be bunched up on one line, we collect
    ;; them here.
    (unless gnus-view-pseudos-separately
      (let ((ps (setq pslist (sort pslist 'gnus-pseudos<)))
	    files action)
	(while ps
	  (setq action (cdr (assq 'action (car ps))))
	  (setq files (list (cdr (assq 'name (car ps)))))
	  (while (and ps (cdr ps)
		      (string= (or action "1")
			       (or (cdr (assq 'action (cadr ps))) "2")))
	    (push (cdr (assq 'name (cadr ps))) files)
	    (setcdr ps (cddr ps)))
	  (when files
	    (when (not (string-match "%s" action))
	      (push " " files))
	    (push " " files)
	    (when (assq 'execute (car ps))
	      (setcdr (assq 'execute (car ps))
		      (funcall (if (string-match "%s" action)
				   'format 'concat)
			       action
			       (mapconcat
				(lambda (f)
				  (if (equal f " ")
				      f
				    (shell-quote-argument f)))
				files " ")))))
	  (setq ps (cdr ps)))))
    (if (and gnus-view-pseudos (not not-view))
	(while pslist
	  (when (assq 'execute (car pslist))
	    (gnus-execute-command (cdr (assq 'execute (car pslist)))
				  (eq gnus-view-pseudos 'not-confirm)))
	  (setq pslist (cdr pslist)))
      (save-excursion
	(while pslist
	  (setq after-article (or (cdr (assq 'article (car pslist)))
				  (gnus-summary-article-number)))
	  (gnus-summary-goto-subject after-article)
	  (forward-line 1)
	  (setq b (point))
	  (insert "    " (file-name-nondirectory
			  (cdr (assq 'name (car pslist))))
		  ": " (or (cdr (assq 'execute (car pslist))) "") "\n")
	  (setq e (point))
	  (forward-line -1)		; back to `b'
	  (gnus-add-text-properties
	   b (1- e) (list 'gnus-number gnus-reffed-article-number
			  gnus-mouse-face-prop gnus-mouse-face))
	  (gnus-data-enter
	   after-article gnus-reffed-article-number
	   gnus-unread-mark b (car pslist) 0 (- e b))
	  (setq gnus-newsgroup-unreads
		(gnus-add-to-sorted-list gnus-newsgroup-unreads
					 gnus-reffed-article-number))
	  (setq gnus-reffed-article-number (1- gnus-reffed-article-number))
	  (setq pslist (cdr pslist)))))))

(defun gnus-pseudos< (p1 p2)
  (let ((c1 (cdr (assq 'action p1)))
	(c2 (cdr (assq 'action p2))))
    (and c1 c2 (string< c1 c2))))

(defun gnus-request-pseudo-article (props)
  (cond ((assq 'execute props)
	 (gnus-execute-command (cdr (assq 'execute props)))))
  (let ((gnus-current-article (gnus-summary-article-number)))
    (gnus-run-hooks 'gnus-mark-article-hook)))

(defun gnus-execute-command (command &optional automatic)
  (save-excursion
    (gnus-article-setup-buffer)
    (set-buffer gnus-article-buffer)
    (setq buffer-read-only nil)
    (let ((command (if automatic command
		     (read-string "Command: " (cons command 0)))))
      (erase-buffer)
      (insert "$ " command "\n\n")
      (if gnus-view-pseudo-asynchronously
	  (start-process "gnus-execute" (current-buffer) shell-file-name
			 shell-command-switch command)
	(call-process shell-file-name nil t nil
		      shell-command-switch command)))))

;; Summary kill commands.

(defun gnus-summary-edit-global-kill (article)
  "Edit the \"global\" kill file."
  (interactive (list (gnus-summary-article-number)))
  (gnus-group-edit-global-kill article))

(defun gnus-summary-edit-local-kill ()
  "Edit a local kill file applied to the current newsgroup."
  (interactive)
  (setq gnus-current-headers (gnus-summary-article-header))
  (gnus-group-edit-local-kill
   (gnus-summary-article-number) gnus-newsgroup-name))

;;; Header reading.

(defun gnus-read-header (id &optional header)
  "Read the headers of article ID and enter them into the Gnus system."
  (let ((group gnus-newsgroup-name)
	(gnus-override-method
	 (or
	  gnus-override-method
	  (and (gnus-news-group-p gnus-newsgroup-name)
	       (car (gnus-refer-article-methods)))))
	where)
    ;; First we check to see whether the header in question is already
    ;; fetched.
    (if (stringp id)
	;; This is a Message-ID.
	(setq header (or header (gnus-id-to-header id)))
      ;; This is an article number.
      (setq header (or header (gnus-summary-article-header id))))
    (if (and header
	     (not (gnus-summary-article-sparse-p (mail-header-number header))))
	;; We have found the header.
	header
      ;; We have to really fetch the header to this article.
      (with-current-buffer nntp-server-buffer
	(when (setq where (gnus-request-head id group))
	  (nnheader-fold-continuation-lines)
	  (goto-char (point-max))
	  (insert ".\n")
	  (goto-char (point-min))
	  (insert "211 ")
	  (princ (cond
		  ((numberp id) id)
		  ((cdr where) (cdr where))
		  (header (mail-header-number header))
		  (t gnus-reffed-article-number))
		 (current-buffer))
	  (insert " Article retrieved.\n"))
	(if (or (not where)
		(not (setq header (car (gnus-get-newsgroup-headers nil t)))))
	    ()				; Malformed head.
	  (unless (gnus-summary-article-sparse-p (mail-header-number header))
	    (when (and (stringp id)
		       (or
			(not (string= (gnus-group-real-name group)
				      (car where)))
			(not (gnus-server-equal gnus-override-method
						(gnus-group-method group)))))
	      ;; If we fetched by Message-ID and the article came from
	      ;; a different group (or server), we fudge some bogus
	      ;; article numbers for this article.
	      (mail-header-set-number header gnus-reffed-article-number))
	    (with-current-buffer gnus-summary-buffer
	      (decf gnus-reffed-article-number)
	      (gnus-remove-header (mail-header-number header))
	      (push header gnus-newsgroup-headers)
	      (setq gnus-current-headers header)
	      (push (mail-header-number header) gnus-newsgroup-limit)))
	  header)))))

(defun gnus-remove-header (number)
  "Remove header NUMBER from `gnus-newsgroup-headers'."
  (if (and gnus-newsgroup-headers
	   (= number (mail-header-number (car gnus-newsgroup-headers))))
      (pop gnus-newsgroup-headers)
    (let ((headers gnus-newsgroup-headers))
      (while (and (cdr headers)
		  (not (= number (mail-header-number (cadr headers)))))
	(pop headers))
      (when (cdr headers)
	(setcdr headers (cddr headers))))))

;;;
;;; summary highlights
;;;

(defun gnus-highlight-selected-summary ()
  "Highlight selected article in summary buffer."
  ;; Added by Per Abrahamsen <amanda@iesd.auc.dk>.
  (when gnus-summary-selected-face
    (save-excursion
      (let* ((beg (point-at-bol))
	     (end (point-at-eol))
	     ;; Fix by Mike Dugan <dugan@bucrf16.bu.edu>.
	     (from (if (get-text-property beg gnus-mouse-face-prop)
		       beg
		     (or (next-single-property-change
			  beg gnus-mouse-face-prop nil end)
			 beg)))
	     (to
	      (if (= from end)
		  (- from 2)
		(or (next-single-property-change
		     from gnus-mouse-face-prop nil end)
		    end))))
	;; If no mouse-face prop on line we will have to = from = end,
	;; so we highlight the entire line instead.
	(when (= (+ to 2) from)
	  (setq from beg)
	  (setq to end))
	(if gnus-newsgroup-selected-overlay
	    ;; Move old overlay.
	    (gnus-move-overlay
	     gnus-newsgroup-selected-overlay from to (current-buffer))
	  ;; Create new overlay.
	  (gnus-overlay-put
	   (setq gnus-newsgroup-selected-overlay (gnus-make-overlay from to))
	   'face gnus-summary-selected-face))))))

(defvar gnus-summary-highlight-line-cached nil)
(defvar gnus-summary-highlight-line-trigger nil)

(defun gnus-summary-highlight-line-0 ()
  (if (and (eq gnus-summary-highlight-line-trigger
               gnus-summary-highlight)
           gnus-summary-highlight-line-cached)
      gnus-summary-highlight-line-cached
    (setq gnus-summary-highlight-line-trigger gnus-summary-highlight
          gnus-summary-highlight-line-cached
          (let* ((cond (list 'cond))
                 (c cond)
                 (list gnus-summary-highlight))
            (while list
              (setcdr c (cons (list (caar list) (list 'quote (cdar list)))
			      nil))
              (setq c (cdr c)
                    list (cdr list)))
            (gnus-byte-compile (list 'lambda nil cond))))))

(defun gnus-summary-highlight-line ()
  "Highlight current line according to `gnus-summary-highlight'."
  (let* ((beg (point-at-bol))
	 (article (or (gnus-summary-article-number) gnus-current-article))
	 (score (or (cdr (assq article
			       gnus-newsgroup-scored))
		    gnus-summary-default-score 0))
	 (mark (or (gnus-summary-article-mark) gnus-unread-mark))
	 (inhibit-read-only t)
	 (default gnus-summary-default-score)
	 (default-high gnus-summary-default-high-score)
	 (default-low gnus-summary-default-low-score)
	 (uncached (and gnus-summary-use-undownloaded-faces
                        (memq article gnus-newsgroup-undownloaded)
                        (not (memq article gnus-newsgroup-cached)))))
    (let ((face (funcall (gnus-summary-highlight-line-0))))
      (unless (eq face (get-text-property beg 'face))
	(gnus-put-text-property-excluding-characters-with-faces
	 beg (point-at-eol) 'face
	 (setq face (if (boundp face) (symbol-value face) face)))
	(when gnus-summary-highlight-line-function
	  (funcall gnus-summary-highlight-line-function article face))))))

(defun gnus-update-read-articles (group unread &optional compute)
  "Update the list of read articles in GROUP.
UNREAD is a sorted list."
  (let ((active (or gnus-newsgroup-active (gnus-active group)))
	(info (gnus-get-info group))
	(prev 1)
	read)
    (if (or (not info) (not active))
	;; There is no info on this group if it was, in fact,
	;; killed.  Gnus stores no information on killed groups, so
	;; there's nothing to be done.
	;; One could store the information somewhere temporarily,
	;; perhaps...  Hmmm...
	()
      ;; Remove any negative articles numbers.
      (while (and unread (< (car unread) 0))
	(setq unread (cdr unread)))
      ;; Remove any expired article numbers
      (while (and unread (< (car unread) (car active)))
	(setq unread (cdr unread)))
      ;; Compute the ranges of read articles by looking at the list of
      ;; unread articles.
      (while unread
	(when (/= (car unread) prev)
	  (push (if (= prev (1- (car unread))) prev
		  (cons prev (1- (car unread))))
		read))
	(setq prev (1+ (car unread)))
	(setq unread (cdr unread)))
      (when (<= prev (cdr active))
	(push (cons prev (cdr active)) read))
      (setq read (if (> (length read) 1) (nreverse read) read))
      (if compute
	  read
	(save-excursion
	  (let (setmarkundo)
	    ;; Propagate the read marks to the backend.
	    (when (and (or gnus-propagate-marks
			   (gnus-method-option-p
			    (gnus-find-method-for-group group)
			    'server-marks))
		       (gnus-check-backend-function 'request-set-mark group))
	      (let ((del (gnus-remove-from-range (gnus-info-read info) read))
		    (add (gnus-remove-from-range read (gnus-info-read info))))
		(when (or add del)
		  (unless (gnus-check-group group)
		    (error "Can't open server for %s" group))
		  (gnus-request-set-mark
		   group (delq nil (list (if add (list add 'add '(read)))
					 (if del (list del 'del '(read))))))
		  (setq setmarkundo
			`(gnus-request-set-mark
			  ,group
			  ',(delq nil (list
				       (if del (list del 'add '(read)))
				       (if add (list add 'del '(read))))))))))
	    (set-buffer gnus-group-buffer)
	    (gnus-undo-register
	      `(progn
		 (gnus-info-set-marks ',info ',(gnus-info-marks info) t)
		 (gnus-info-set-read ',info ',(gnus-info-read info))
		 (gnus-get-unread-articles-in-group ',info
						    (gnus-active ,group))
		 (gnus-group-update-group ,group t)
		 ,setmarkundo))))
	;; Enter this list into the group info.
	(gnus-info-set-read info read)
	;; Set the number of unread articles in gnus-newsrc-hashtb.
	(gnus-get-unread-articles-in-group info (gnus-active group))
	t))))

(defun gnus-offer-save-summaries ()
  "Offer to save all active summary buffers."
  (let (buffers)
    ;; Go through all buffers and find all summaries.
    (dolist (buffer (buffer-list))
      (when (and (setq buffer (buffer-name buffer))
		 (string-match "Summary" buffer)
		 (with-current-buffer buffer
		   ;; We check that this is, indeed, a summary buffer.
		   (and (eq major-mode 'gnus-summary-mode)
			;; Also make sure this isn't bogus.
			gnus-newsgroup-prepared
			;; Also make sure that this isn't a
			;; dead summary buffer.
			(not gnus-dead-summary-mode))))
	(push buffer buffers)))
    ;; Go through all these summary buffers and offer to save them.
    (when buffers
      (save-excursion
	(if (eq gnus-interactive-exit 'quiet)
	    (dolist (buffer buffers)
	      (switch-to-buffer buffer)
	      (gnus-summary-exit))
	  (map-y-or-n-p
	   "Update summary buffer %s? "
	   (lambda (buf)
	     (switch-to-buffer buf)
	     (gnus-summary-exit))
	   buffers))))))

(defun gnus-summary-setup-default-charset ()
  "Setup newsgroup default charset."
  (if (member gnus-newsgroup-name '("nndraft:delayed" "nndraft:drafts"))
      (setq gnus-newsgroup-charset nil)
    (let* ((ignored-charsets
	    (or gnus-newsgroup-ephemeral-ignored-charsets
		(append
		 (and gnus-newsgroup-name
		      (gnus-parameter-ignored-charsets gnus-newsgroup-name))
		 gnus-newsgroup-ignored-charsets))))
      (setq gnus-newsgroup-charset
	    (or gnus-newsgroup-ephemeral-charset
		(and gnus-newsgroup-name
		     (gnus-parameter-charset gnus-newsgroup-name))
		gnus-default-charset))
      (set (make-local-variable 'gnus-newsgroup-ignored-charsets)
	   ignored-charsets))))

;;;
;;; Mime Commands
;;;

(defun gnus-summary-display-buttonized (&optional show-all-parts)
  "Display the current article buffer fully MIME-buttonized.
If SHOW-ALL-PARTS (the prefix) is non-nil, all multipart/* parts are
treated as multipart/mixed."
  (interactive "P")
  (require 'gnus-art)
  (let ((gnus-unbuttonized-mime-types nil)
	(gnus-mime-display-multipart-as-mixed show-all-parts))
    (gnus-summary-show-article)))

(defun gnus-summary-repair-multipart (article)
  "Add a Content-Type header to a multipart article without one."
  (interactive (list (gnus-summary-article-number)))
  (gnus-with-article article
    (message-narrow-to-head)
    (message-remove-header "Mime-Version")
    (goto-char (point-max))
    (insert "Mime-Version: 1.0\n")
    (widen)
    (when (search-forward "\n--" nil t)
      (let ((separator (buffer-substring (point) (point-at-eol))))
	(message-narrow-to-head)
	(message-remove-header "Content-Type")
	(goto-char (point-max))
	(insert (format "Content-Type: multipart/mixed; boundary=\"%s\"\n"
			separator))
	(widen))))
  (let (gnus-mark-article-hook)
    (gnus-summary-select-article t t nil article)))

(defun gnus-summary-toggle-display-buttonized ()
  "Toggle the buttonizing of the article buffer."
  (interactive)
  (require 'gnus-art)
  (if (setq gnus-inhibit-mime-unbuttonizing
	    (not gnus-inhibit-mime-unbuttonizing))
      (let ((gnus-unbuttonized-mime-types nil))
	(gnus-summary-show-article))
    (gnus-summary-show-article)))

;;;
;;; Generic summary marking commands
;;;

(defvar gnus-summary-marking-alist
  '((read gnus-del-mark "d")
    (unread gnus-unread-mark "u")
    (ticked gnus-ticked-mark "!")
    (dormant gnus-dormant-mark "?")
    (expirable gnus-expirable-mark "e"))
  "An alist of names/marks/keystrokes.")

(defvar gnus-summary-generic-mark-map (make-sparse-keymap))
(defvar gnus-summary-mark-map)

(defun gnus-summary-make-all-marking-commands ()
  (define-key gnus-summary-mark-map "M" gnus-summary-generic-mark-map)
  (dolist (elem gnus-summary-marking-alist)
    (apply 'gnus-summary-make-marking-command elem)))

(defun gnus-summary-make-marking-command (name mark keystroke)
  (let ((map (make-sparse-keymap)))
    (define-key gnus-summary-generic-mark-map keystroke map)
    (dolist (lway `((next "next" next nil "n")
		    (next-unread "next unread" next t "N")
		    (prev "previous" prev nil "p")
		    (prev-unread "previous unread" prev t "P")
		    (nomove "" nil nil ,keystroke)))
      (let ((func (gnus-summary-make-marking-command-1
		   mark (car lway) lway name)))
	(setq func (eval func))
	(define-key map (nth 4 lway) func)))))

(defun gnus-summary-make-marking-command-1 (mark way lway name)
  `(defun ,(intern
	    (format "gnus-summary-put-mark-as-%s%s"
		    name (if (eq way 'nomove)
			     ""
			   (concat "-" (symbol-name way)))))
     (n)
     ,(format
       "Mark the current article as %s%s.
If N, the prefix, then repeat N times.
If N is negative, move in reverse order.
The difference between N and the actual number of articles marked is
returned."
       name (cadr lway))
     (interactive "p")
     (gnus-summary-generic-mark n ,mark ',(nth 2 lway) ,(nth 3 lway))))

(defun gnus-summary-generic-mark (n mark move unread)
  "Mark N articles with MARK."
  (unless (eq major-mode 'gnus-summary-mode)
    (error "This command can only be used in the summary buffer"))
  (gnus-summary-show-thread)
  (let ((nummove
	 (cond
	  ((eq move 'next) 1)
	  ((eq move 'prev) -1)
	  (t 0))))
    (if (zerop nummove)
	(setq n 1)
      (when (< n 0)
	(setq n (abs n)
	      nummove (* -1 nummove))))
    (while (and (> n 0)
		(gnus-summary-mark-article nil mark)
		(zerop (gnus-summary-next-subject nummove unread t)))
      (setq n (1- n)))
    (when (/= 0 n)
      (gnus-message 7 "No more %sarticles" (if mark "" "unread ")))
    (gnus-summary-recenter)
    (gnus-summary-position-point)
    (gnus-set-mode-line 'summary)
    n))

(defun gnus-summary-insert-articles (articles)
  (when (setq articles
	      (gnus-sorted-difference articles
				      (mapcar (lambda (h)
						(mail-header-number h))
					      gnus-newsgroup-headers)))
    (setq gnus-newsgroup-headers
	  (gnus-merge 'list
		      gnus-newsgroup-headers
		      (gnus-fetch-headers articles)
		      'gnus-article-sort-by-number))
    (setq gnus-newsgroup-articles
	  (gnus-sorted-nunion gnus-newsgroup-articles articles))
    ;; Suppress duplicates?
    (when gnus-suppress-duplicates
      (gnus-dup-suppress-articles))

    (if (and gnus-fetch-old-headers
	     (eq gnus-headers-retrieved-by 'nov))
	;; We might want to build some more threads first.
	(if (eq gnus-fetch-old-headers 'invisible)
	    (gnus-build-all-threads)
	  (gnus-build-old-threads))
      ;; Mark the inserted articles that are unread as unread.
      (setq gnus-newsgroup-unreads
	    (gnus-sorted-nunion
	     gnus-newsgroup-unreads
	     (gnus-sorted-nintersection
	      (gnus-list-of-unread-articles gnus-newsgroup-name)
	      articles)))
      ;; Mark the inserted articles as selected so that the information
      ;; of the marks having been changed by a user may be updated when
      ;; exiting this group.  See `gnus-summary-update-info'.
      (dolist (art articles)
	(setq gnus-newsgroup-unselected (delq art gnus-newsgroup-unselected))))
    ;; Let the Gnus agent mark articles as read.
    (when gnus-agent
      (gnus-agent-get-undownloaded-list))
    ;; Remove list identifiers from subject
    (gnus-summary-remove-list-identifiers)
    ;; First and last article in this newsgroup.
    (when gnus-newsgroup-headers
      (setq gnus-newsgroup-begin
	    (mail-header-number (car gnus-newsgroup-headers))
	    gnus-newsgroup-end
	    (mail-header-number
	     (gnus-last-element gnus-newsgroup-headers))))
    (when gnus-use-scoring
      (gnus-possibly-score-headers))))

(defun gnus-summary-insert-old-articles (&optional all)
  "Insert all old articles in this group.
If ALL is non-nil, already read articles become readable.
If ALL is a number, fetch this number of articles."
  (interactive "P")
  (prog1
      (let ((old (sort (mapcar 'car gnus-newsgroup-data) '<))
	    older len)
	(setq older
	      ;; Some nntp servers lie about their active range.  When
	      ;; this happens, the active range can be in the millions.
	      ;; Use a compressed range to avoid creating a huge list.
	      (gnus-range-difference (list gnus-newsgroup-active) old))
	(setq len (gnus-range-length older))
	(cond
	 ((null older) nil)
	 ((numberp all)
	  (if (< all len)
	      (let ((older-range (nreverse older)))
		(setq older nil)

		(while (> all 0)
		  (let* ((r (pop older-range))
			 (min (if (numberp r) r (car r)))
			 (max (if (numberp r) r (cdr r))))
		    (while (and (<= min max)
				(> all 0))
		      (push max older)
		      (setq all (1- all)
			    max (1- max))))))
	    (setq older (gnus-uncompress-range older))))
	 (all
	  (setq older (gnus-uncompress-range older)))
	 (t
	  (when (and (numberp gnus-large-newsgroup)
		   (> len gnus-large-newsgroup))
	      (let* ((cursor-in-echo-area nil)
		     (initial (gnus-parameter-large-newsgroup-initial
			       gnus-newsgroup-name))
		     (input
		      (read-string
		       (format
			"How many articles from %s (%s %d): "
			(gnus-group-decoded-name gnus-newsgroup-name)
			(if initial "max" "default")
			len)
		       nil nil
		       (and initial
			    (number-to-string initial)))))
		(unless (string-match "^[ \t]*$" input)
		  (setq all (string-to-number input))
		  (if (< all len)
		      (let ((older-range (nreverse older)))
			(setq older nil)

			(while (> all 0)
			  (let* ((r (pop older-range))
				 (min (if (numberp r) r (car r)))
				 (max (if (numberp r) r (cdr r))))
			    (while (and (<= min max)
					(> all 0))
			      (push max older)
			      (setq all (1- all)
				    max (1- max))))))))))
	  (setq older (gnus-uncompress-range older))))
	(if (not older)
	    (message "No old news.")
	  (gnus-summary-insert-articles older)
	  (gnus-summary-limit (gnus-sorted-nunion old older))))
    (gnus-summary-position-point)))

(defun gnus-summary-insert-new-articles ()
  "Insert all new articles in this group."
  (interactive)
  (let ((old (sort (mapcar 'car gnus-newsgroup-data) '<))
	(old-high gnus-newsgroup-highest)
	(nnmail-fetched-sources (list t))
	(new-active (gnus-activate-group gnus-newsgroup-name 'scan))
	i new)
    (unless new-active
      (error "Couldn't fetch new data"))
    (setq gnus-newsgroup-active (gnus-copy-sequence new-active))
    (setq i (cdr gnus-newsgroup-active)
	  gnus-newsgroup-highest i)
    (while (> i old-high)
      (push i new)
      (decf i))
    (if (not new)
	(message "No gnus is bad news")
      (gnus-summary-insert-articles new)
      (setq gnus-newsgroup-unreads
	    (gnus-sorted-nunion gnus-newsgroup-unreads new))
      (gnus-summary-limit (gnus-sorted-nunion old new))))
  (gnus-summary-position-point))

;;; Bookmark support for Gnus.
(declare-function bookmark-make-record-default
                  "bookmark" (&optional no-file no-context posn))
(declare-function bookmark-prop-get "bookmark" (bookmark prop))
(declare-function bookmark-default-handler "bookmark" (bmk))
(declare-function bookmark-get-bookmark-record "bookmark" (bmk))
(defvar bookmark-yank-point)
(defvar bookmark-current-buffer)

(defun gnus-summary-bookmark-make-record ()
  "Make a bookmark entry for a Gnus summary buffer."
  (let (pos buf)
    (unless (and (derived-mode-p 'gnus-summary-mode) gnus-article-current)
      (save-restriction              ; FIXME is it necessary to widen?
        (widen) (setq pos (point))) ; Set position in gnus-article buffer.
      (setq buf "art") ; We are recording bookmark from article buffer.
      (setq bookmark-yank-point (point))
      (setq bookmark-current-buffer (current-buffer))
      (gnus-article-show-summary))      ; Go back in summary buffer.
    ;; We are now recording bookmark from summary buffer.
    (unless buf (setq buf "sum"))
    (let* ((subject (elt (gnus-summary-article-header) 1))
           (grp     (car gnus-article-current))
           (art     (cdr gnus-article-current))
           (head    (gnus-summary-article-header art))
           (id      (mail-header-id head)))
      `(,subject
	,@(condition-case nil
	      (bookmark-make-record-default 'no-file 'no-context pos)
	    (wrong-number-of-arguments
	     (bookmark-make-record-default 'point-only)))
        (location . ,(format "Gnus-%s %s:%d:%s" buf grp art id))
        (group . ,grp) (article . ,art)
        (message-id . ,id) (handler . gnus-summary-bookmark-jump)))))

;;;###autoload
(defun gnus-summary-bookmark-jump (bookmark)
  "Handler function for record returned by `gnus-summary-bookmark-make-record'.
BOOKMARK is a bookmark name or a bookmark record."
  (let ((group    (bookmark-prop-get bookmark 'group))
        (article  (bookmark-prop-get bookmark 'article))
        (id       (bookmark-prop-get bookmark 'message-id))
        (buf      (car (split-string (bookmark-prop-get bookmark 'location)))))
    (gnus-fetch-group group (list article))
    (gnus-summary-insert-cached-articles)
    (gnus-summary-goto-article id nil 'force)
    ;; FIXME we have to wait article buffer is ready (only large buffer)
    ;; Is there a better solution to know that?
    ;; If we don't wait `bookmark-default-handler' will have no chance
    ;; to set position. However there is no error, just wrong pos.
    (sit-for 1)
    (when (string= buf "Gnus-art")
      (other-window 1))
    (bookmark-default-handler
     `(""
       (buffer . ,(current-buffer))
       . ,(bookmark-get-bookmark-record bookmark)))))

(gnus-summary-make-all-marking-commands)

(gnus-ems-redefine)

(provide 'gnus-sum)

(run-hooks 'gnus-sum-load-hook)

;; Local Variables:
;; coding: iso-8859-1
;; End:

;;; gnus-sum.el ends here
