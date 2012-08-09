;;; allout.el --- extensive outline mode for use alone and with other modes

;; Copyright (C) 1992-1994, 2001-2012  Free Software Foundation, Inc.

;; Author: Ken Manheimer <ken dot manheimer at gmail...>
;; Maintainer: Ken Manheimer <ken dot manheimer at gmail...>
;; Created: Dec 1991 -- first release to usenet
;; Version: 2.3
;; Keywords: outlines, wp, languages, PGP, GnuPG
;; Website: http://myriadicity.net/software-and-systems/craft/emacs-allout

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

;; Allout outline minor mode provides extensive outline formatting and
;; and manipulation beyond standard emacs outline mode.  Some features:
;;
;;  - Classic outline-mode topic-oriented navigation and exposure adjustment
;;  - Topic-oriented editing including coherent topic and subtopic
;;    creation, promotion, demotion, cut/paste across depths, etc.
;;  - Incremental search with dynamic exposure and reconcealment of text
;;  - Customizable bullet format -- enables programming-language specific
;;    outlining, for code-folding editing.  (Allout code itself is to try it;
;;    formatted as an outline -- do ESC-x eval-buffer in allout.el; but
;;    emacs local file variables need to be enabled when the
;;    file was visited -- see `enable-local-variables'.)
;;  - Configurable per-file initial exposure settings
;;  - Symmetric-key and key-pair topic encryption.  Encryption is via the
;;    Emacs 'epg' library.  See allout-toggle-current-subtree-encryption
;;    docstring.
;;  - Automatic topic-number maintenance
;;  - "Hot-spot" operation, for single-keystroke maneuvering and
;;    exposure control (see the allout-mode docstring)
;;  - Easy rendering of exposed portions into numbered, latex, indented, etc
;;    outline styles
;;  - Careful attention to whitespace -- enabling blank lines between items
;;    and maintenance of hanging indentation (in paragraph auto-fill and
;;    across topic promotion and demotion) of topic bodies consistent with
;;    indentation of their topic header.
;;
;; and more.
;;
;; See the `allout-mode' function's docstring for an introduction to the
;; mode.
;;
;; Directions to the latest development version and helpful notes are
;; available at http://myriadicity.net/Sundry/EmacsAllout .
;;
;; The outline menubar additions provide quick reference to many of the
;; features.  See the docstring of the variables `allout-layout' and
;; `allout-auto-activation' for details on automatic activation of
;; `allout-mode' as a minor mode.  (`allout-init' is deprecated in favor of
;; a purely customization-based method.)
;;
;; Note -- the lines beginning with `;;;_' are outline topic headers.
;;        Customize `allout-auto-activation' to enable, then revisit this
;;        buffer to give it a whirl.

;; ken manheimer (ken dot manheimer at gmail dot com)

;;; Code:

;;;_* Dependency loads
(require 'overlay)
(eval-when-compile
  ;; Most of the requires here are for stuff covered by autoloads, which
  ;; byte-compiling doesn't trigger.
  (require 'epg)
  (require 'epa)
  (require 'overlay)
  ;; `cl' is required for `assert'.  `assert' is not covered by a standard
  ;; autoload, but it is a macro, so that eval-when-compile is sufficient
  ;; to byte-compile it in, or to do the require when the buffer evalled.
  (require 'cl)
  )

;;;_* USER CUSTOMIZATION VARIABLES:

;;;_ > defgroup allout, allout-keybindings
(defgroup allout nil
  "Extensive outline minor-mode, for use stand-alone and with other modes.

See Allout Auto Activation for automatic activation."
  :prefix "allout-"
  :group 'outlines)
(defgroup allout-keybindings nil
  "Allout outline mode keyboard bindings configuration."
  :group 'allout)

;;;_ + Layout, Mode, and Topic Header Configuration

(defvar allout-command-prefix)          ; defined below

;;;_  > allout-keybindings incidentals:
;;;_   : internal key binding stuff - in this section for load-order.
;;;_    = allout-mode-map
(defvar allout-mode-map 'allout-mode-map
  "Keybindings place-holder for (allout) outline minor mode.

Do NOT set the value of this variable.  Instead, customize
`allout-command-prefix', `allout-prefixed-keybindings', and
`allout-unprefixed-keybindings'.")
;;;_    = allout-mode-map-value
(defvar allout-mode-map-value nil
  "Keymap for allout outline minor mode.

Do NOT set the value of this variable.  Instead, customize
`allout-command-prefix', `allout-prefixed-keybindings', and
`allout-unprefixed-keybindings'.")
;;;_    = make allout-mode-map-value an alias for allout-mode-map:
;; this needs to be revised when the value is changed, sigh.
(defalias 'allout-mode-map allout-mode-map-value)
;;;_   > allout-compose-and-institute-keymap (&optional varname value)
(defun allout-compose-and-institute-keymap (&optional varname value)
  "Create the allout keymap according to the keybinding specs, and set it.

Useful standalone or to effect customizations of the
respective allout-mode keybinding variables, `allout-command-prefix',
`allout-prefixed-keybindings', and `allout-unprefixed-keybindings'"
  ;; Set the customization variable, if any:
  (when varname
    (set-default varname value))
  (let ((map (make-sparse-keymap)))
    (when (boundp 'allout-prefixed-keybindings)
      ;; tolerate first definitions of the variables:
      (dolist (entry allout-prefixed-keybindings)
        (define-key map
          ;; XXX vector vs non-vector key descriptions?
          (vconcat allout-command-prefix
                   (car (read-from-string (car entry))))
          (cadr entry))))
    (when (boundp 'allout-unprefixed-keybindings)
      (dolist (entry allout-unprefixed-keybindings)
        (define-key map (car (read-from-string (car entry))) (cadr entry))))
    (substitute-key-definition 'beginning-of-line 'allout-beginning-of-line
                               map global-map)
    (substitute-key-definition 'move-beginning-of-line 'allout-beginning-of-line
                               map global-map)
    (substitute-key-definition 'end-of-line 'allout-end-of-line
                               map global-map)
    (substitute-key-definition 'move-end-of-line 'allout-end-of-line
                               map global-map)
    (allout-institute-keymap map)))
;;;_  > allout-institute-keymap (map)
(defun allout-institute-keymap (map)
  "Associate allout-mode bindings with allout as a minor mode."
  ;; Architecture:
  ;; allout-mode-map var is a keymap by virtue of being a defalias for
  ;; allout-mode-map-value, which has the actual keymap value.
  ;; allout-mode-map's symbol value is just 'allout-mode-map, so it can be
  ;; used in minor-mode-map-alist to indirect to the actual
  ;; allout-mode-map-var value, which can be adjusted and reassigned.

  ;; allout-mode-map-value for keymap reference in various places:
  (setq allout-mode-map-value map)
  ;; the function value keymap of allout-mode-map is used in
  ;; minor-mode-map-alist - update it:
  (fset allout-mode-map allout-mode-map-value))
;;;_  * initialize the mode map:
;; ensure that allout-mode-map has some setting even if allout-mode hasn't
;; been invoked:
(allout-compose-and-institute-keymap)
;;;_   = allout-command-prefix
(defcustom allout-command-prefix "\C-c "
  "Key sequence to be used as prefix for outline mode command key bindings.

Default is '\C-c<space>'; just '\C-c' is more short-and-sweet, if you're
willing to let allout use a bunch of \C-c keybindings."
  :type 'string
  :group 'allout-keybindings
  :set 'allout-compose-and-institute-keymap)
;;;_   = allout-keybindings-binding
(define-widget 'allout-keybindings-binding 'lazy
  "Structure of allout keybindings customization items."
  :type '(repeat
          (list (string :tag "Key" :value "[(meta control shift ?f)]")
                (function :tag "Function name"
                          :value allout-forward-current-level))))
;;;_   = allout-prefixed-keybindings
(defcustom allout-prefixed-keybindings
  '(("[(control ?n)]" allout-next-visible-heading)
    ("[(control ?p)]" allout-previous-visible-heading)
    ("[(control ?u)]" allout-up-current-level)
    ("[(control ?f)]" allout-forward-current-level)
    ("[(control ?b)]" allout-backward-current-level)
    ("[(control ?a)]" allout-beginning-of-current-entry)
    ("[(control ?e)]" allout-end-of-entry)
    ("[(control ?i)]" allout-show-children)
    ("[(control ?s)]" allout-show-current-subtree)
    ("[(control ?t)]" allout-toggle-current-subtree-exposure)
;; Let user customize if they want to preempt describe-prefix-bindings ^h use.
;;    ("[(control ?h)]" allout-hide-current-subtree)
    ("[?h]" allout-hide-current-subtree)
    ("[(control ?o)]" allout-show-current-entry)
    ("[?!]" allout-show-all)
    ("[?x]" allout-toggle-current-subtree-encryption)
    ("[? ]" allout-open-sibtopic)
    ("[?.]" allout-open-subtopic)
    ("[?,]" allout-open-supertopic)
    ("[?']" allout-shift-in)
    ("[?>]" allout-shift-in)
    ("[?<]" allout-shift-out)
    ("[(control ?m)]" allout-rebullet-topic)
    ("[?*]" allout-rebullet-current-heading)
    ("[?#]" allout-number-siblings)
    ("[(control ?k)]" allout-kill-topic)
    ("[(meta ?k)]" allout-copy-topic-as-kill)
    ("[?@]" allout-resolve-xref)
    ("[?=?c]" allout-copy-exposed-to-buffer)
    ("[?=?i]" allout-indented-exposed-to-buffer)
    ("[?=?t]" allout-latexify-exposed)
    ("[?=?p]" allout-flatten-exposed-to-buffer)
    )
  "Allout-mode key bindings that are prefixed with `allout-command-prefix'.

See `allout-unprefixed-keybindings' for the list of keybindings
that are not prefixed.

Use vector format for the keys:
  - put literal keys after a '?' question mark, eg: '?a', '?.'
  - enclose control, shift, or meta-modified keys as sequences within
    parentheses, with the literal key, as above, preceded by the name(s)
    of the modifiers, eg: [(control ?a)]
See the existing keys for examples.

Functions can be bound to multiple keys, but binding keys to
multiple functions will not work - the last binding for a key
prevails."
  :version "24.1"
  :type 'allout-keybindings-binding
  :group 'allout-keybindings
  :set 'allout-compose-and-institute-keymap
 )
;;;_   = allout-unprefixed-keybindings
(defcustom allout-unprefixed-keybindings
  '(("[(control ?k)]" allout-kill-line)
    ("[(meta ?k)]" allout-copy-line-as-kill)
    ("[(control ?y)]" allout-yank)
    ("[(meta ?y)]" allout-yank-pop)
    )
  "Allout-mode functions bound to keys without any added prefix.

This is in contrast to the majority of allout-mode bindings on
`allout-prefixed-bindings', whose bindings are created with a
preceding command key.

Use vector format for the keys:
  - put literal keys after a '?' question mark, eg: '?a', '?.'
  - enclose control, shift, or meta-modified keys as sequences within
    parentheses, with the literal key, as above, preceded by the name(s)
    of the modifiers, eg: [(control ?a)]
See the existing keys for examples."
  :version "24.1"
  :type 'allout-keybindings-binding
  :group 'allout-keybindings
  :set 'allout-compose-and-institute-keymap
  )

;;;_  > allout-auto-activation-helper (var value)
;;;###autoload
(defun allout-auto-activation-helper (var value)
  "Institute `allout-auto-activation'.

Intended to be used as the `allout-auto-activation' :set function."
  (set-default var value)
  (allout-setup))
;;;_  > allout-setup ()
;;;###autoload
(defun allout-setup ()
  "Do fundamental Emacs session for allout auto-activation.

Establishes allout processing as part of visiting a file if
`allout-auto-activation' is non-nil, or removes it otherwise.

The proper way to use this is through customizing the setting of
`allout-auto-activation'."
  (if (not allout-auto-activation)
      (remove-hook 'find-file-hook 'allout-find-file-hook)
      (add-hook 'find-file-hook 'allout-find-file-hook)))
;;;_  = allout-auto-activation
;;;###autoload
(defcustom allout-auto-activation nil
  "Configure allout outline mode auto-activation.

Control whether and how allout outline mode is automatically
activated when files are visited with non-nil buffer-specific
file variable `allout-layout'.

When allout-auto-activation is \"On\" (t), allout mode is
activated in buffers with non-nil `allout-layout', and the
specified layout is applied.

With value \"ask\", auto-mode-activation is enabled, and endorsement for
performing auto-layout is asked of the user each time.

With value \"activate\", only auto-mode-activation is enabled.
Auto-layout is not.

With value nil, inhibit any automatic allout-mode activation."
  :set 'allout-auto-activation-helper
  ;; FIXME: Using strings here is unusual and less efficient than symbols.
  :type '(choice (const :tag "On" t)
                (const :tag "Ask about layout" "ask")
                (const :tag "Mode only" "activate")
                (const :tag "Off" nil))
  :group 'allout)
(allout-setup)
;;;_  = allout-default-layout
(defcustom allout-default-layout '(-2 : 0)
  "Default allout outline layout specification.

This setting specifies the outline exposure to use when
`allout-layout' has the local value `t'.  This docstring describes the
layout specifications.

A list value specifies a default layout for the current buffer,
to be applied upon activation of `allout-mode'.  Any non-nil
value will automatically trigger `allout-mode', provided
`allout-auto-activation' has been customized to enable it.

The types of elements in the layout specification are:

 INTEGER -- dictate the relative depth to open the corresponding topic(s),
            where:
         -- negative numbers force the topic to be closed before opening
            to the absolute value of the number, so all siblings are open
            only to that level.
         -- positive numbers open to the relative depth indicated by the
            number, but do not force already opened subtopics to be closed.
         -- 0 means to close topic -- hide all subitems.
 :   -- repeat spec -- apply the preceding element to all siblings at
        current level, *up to* those siblings that would be covered by specs
        following the `:' on the list.  Ie, apply to all topics at level but
        trailing ones accounted for by trailing specs.  (Only the first of
        multiple colons at the same level is honored -- later ones are ignored.)
 *   -- completely exposes the topic, including bodies
 +   -- exposes all subtopics, but not the bodies
 -   -- exposes the body of the corresponding topic, but not subtopics
 LIST -- a nested layout spec, to be applied intricately to its
        corresponding item(s)

Examples:
 (-2 : 0)
	Collapse the top-level topics to show their children and
        grandchildren, but completely collapse the final top-level topic.
 (-1 () : 1 0)
	Close the first topic so only the immediate subtopics are shown,
        leave the subsequent topics exposed as they are until the second
	second to last topic, which is exposed at least one level, and
        completely close the last topic.
 (-2 : -1 *)
        Expose children and grandchildren of all topics at current
	level except the last two; expose children of the second to
	last and completely expose the last one, including its subtopics.

See `allout-expose-topic' for more about the exposure process.

Also, allout's mode-specific provisions will make topic prefixes default
to the comment-start string, if any, of the language of the file.  This
is modulo the setting of `allout-use-mode-specific-leader', which see."
  :type 'allout-layout-type
  :group 'allout)
;;;_  : allout-layout-type
(define-widget 'allout-layout-type 'lazy
  "Allout layout format customization basic building blocks."
  :type '(repeat
          (choice (integer :tag "integer (<= zero is strict)")
                  (const :tag ": (repeat prior)" :)
                  (const :tag "* (completely expose)" *)
                  (const :tag "+ (expose all offspring, headlines only)" +)
                  (const :tag "- (expose topic body but not offspring)" -)
                  (allout-layout-type :tag "<Nested layout>"))))

;;;_  = allout-inhibit-auto-fill
(defcustom allout-inhibit-auto-fill nil
  "If non-nil, auto-fill will be inhibited in the allout buffers.

You can customize this setting to set it for all allout buffers, or set it
in individual buffers if you want to inhibit auto-fill only in particular
buffers.  (You could use a function on `allout-mode-hook' to inhibit
auto-fill according, eg, to the major mode.)

If you don't set this and auto-fill-mode is enabled, allout will use the
value that `normal-auto-fill-function', if any, when allout mode starts, or
else allout's special hanging-indent maintaining auto-fill function,
`allout-auto-fill'."
  :type 'boolean
  :group 'allout)
(make-variable-buffer-local 'allout-inhibit-auto-fill)
;;;_  = allout-inhibit-auto-fill-on-headline
(defcustom allout-inhibit-auto-fill-on-headline nil
  "If non-nil, auto-fill will be inhibited while on topic's header line."
  :version "24.1"
  :type 'boolean
  :group 'allout)
(make-variable-buffer-local 'allout-inhibit-auto-fill-on-headline)
;;;_  = allout-use-hanging-indents
(defcustom allout-use-hanging-indents t
  "If non-nil, topic body text auto-indent defaults to indent of the header.
Ie, it is indented to be just past the header prefix.  This is
relevant mostly for use with `indented-text-mode', or other situations
where auto-fill occurs."
  :type 'boolean
  :group 'allout)
(make-variable-buffer-local 'allout-use-hanging-indents)
;;;###autoload
(put 'allout-use-hanging-indents 'safe-local-variable
     (if (fboundp 'booleanp) 'booleanp (lambda (x) (member x '(t nil)))))
;;;_  = allout-reindent-bodies
(defcustom allout-reindent-bodies (if allout-use-hanging-indents
				    'text)
  "Non-nil enables auto-adjust of topic body hanging indent with depth shifts.

When active, topic body lines that are indented even with or beyond
their topic header are reindented to correspond with depth shifts of
the header.

A value of t enables reindent in non-programming-code buffers, ie
those that do not have the variable `comment-start' set.  A value of
`force' enables reindent whether or not `comment-start' is set."
  :type '(choice (const nil) (const t) (const text) (const force))
  :group 'allout)

(make-variable-buffer-local 'allout-reindent-bodies)
;;;###autoload
(put 'allout-reindent-bodies 'safe-local-variable
     (lambda (x) (memq x '(nil t text force))))

;;;_  = allout-show-bodies
(defcustom allout-show-bodies nil
  "If non-nil, show entire body when exposing a topic, rather than
just the header."
  :type 'boolean
  :group 'allout)
(make-variable-buffer-local 'allout-show-bodies)
;;;###autoload
(put 'allout-show-bodies 'safe-local-variable
     (if (fboundp 'booleanp) 'booleanp (lambda (x) (member x '(t nil)))))

;;;_  = allout-beginning-of-line-cycles
(defcustom allout-beginning-of-line-cycles t
  "If non-nil, \\[allout-beginning-of-line] will cycle through smart-placement options.

Cycling only happens on when the command is repeated, not when it
follows a different command.

Smart-placement means that repeated calls to this function will
advance as follows:

 - if the cursor is on a non-headline body line and not on the first column:
   then it goes to the first column
 - if the cursor is on the first column of a non-headline body line:
   then it goes to the start of the headline within the item body
 - if the cursor is on the headline and not the start of the headline:
   then it goes to the start of the headline
 - if the cursor is on the start of the headline:
   then it goes to the bullet character (for hotspot navigation)
 - if the cursor is on the bullet character:
   then it goes to the first column of that line (the headline)
 - if the cursor is on the first column of the headline:
   then it goes to the start of the headline within the item body.

In this fashion, you can use the beginning-of-line command to do
its normal job and then, when repeated, advance through the
entry, cycling back to start.

If this configuration variable is nil, then the cursor is just
advanced to the beginning of the line and remains there on
repeated calls."
  :type 'boolean :group 'allout)
;;;_  = allout-end-of-line-cycles
(defcustom allout-end-of-line-cycles t
  "If non-nil, \\[allout-end-of-line] will cycle through smart-placement options.

Cycling only happens on when the command is repeated, not when it
follows a different command.

Smart placement means that repeated calls to this function will
advance as follows:

 - if the cursor is not on the end-of-line,
   then it goes to the end-of-line
 - if the cursor is on the end-of-line but not the end-of-entry,
   then it goes to the end-of-entry, exposing it if necessary
 - if the cursor is on the end-of-entry,
   then it goes to the end of the head line

In this fashion, you can use the end-of-line command to do its
normal job and then, when repeated, advance through the entry,
cycling back to start.

If this configuration variable is nil, then the cursor is just
advanced to the end of the line and remains there on repeated
calls."
  :type 'boolean :group 'allout)

;;;_  = allout-header-prefix
(defcustom allout-header-prefix "."
;; this string is treated as literal match.  it will be `regexp-quote'd, so
;; one cannot use regular expressions to match varying header prefixes.
  "Leading string which helps distinguish topic headers.

Outline topic header lines are identified by a leading topic
header prefix, which mostly have the value of this var at their front.
Level 1 topics are exceptions.  They consist of only a single
character, which is typically set to the `allout-primary-bullet'."
  :type 'string
  :group 'allout)
(make-variable-buffer-local 'allout-header-prefix)
;;;###autoload
(put 'allout-header-prefix 'safe-local-variable 'stringp)
;;;_  = allout-primary-bullet
(defcustom allout-primary-bullet "*"
  "Bullet used for top-level outline topics.

Outline topic header lines are identified by a leading topic header
prefix, which is concluded by bullets that includes the value of this
var and the respective allout-*-bullets-string vars.

The value of an asterisk (`*') provides for backwards compatibility
with the original Emacs outline mode.  See `allout-plain-bullets-string'
and `allout-distinctive-bullets-string' for the range of available
bullets."
  :type 'string
  :group 'allout)
(make-variable-buffer-local 'allout-primary-bullet)
;;;###autoload
(put 'allout-primary-bullet 'safe-local-variable 'stringp)
;;;_  = allout-plain-bullets-string
(defcustom allout-plain-bullets-string ".,"
  "The bullets normally used in outline topic prefixes.

See `allout-distinctive-bullets-string' for the other kind of
bullets.

DO NOT include the close-square-bracket, `]', as a bullet.

Outline mode has to be reactivated in order for changes to the value
of this var to take effect."
  :type 'string
  :group 'allout)
(make-variable-buffer-local 'allout-plain-bullets-string)
;;;###autoload
(put 'allout-plain-bullets-string 'safe-local-variable 'stringp)
;;;_  = allout-distinctive-bullets-string
(defcustom allout-distinctive-bullets-string "*+-=>()[{}&!?#%\"X@$~_\\:;^"
  "Persistent outline header bullets used to distinguish special topics.

These bullets are distinguish topics with particular character.
They are not used by default in the topic creation routines, but
are offered as options when you modify topic creation with a
universal argument (\\[universal-argument]), or during rebulleting (\\[allout-rebullet-current-heading]).

Distinctive bullets are not cycled when topics are shifted or
otherwise automatically rebulleted, so their marking is
persistent until deliberately changed.  Their significance is
purely by convention, however.  Some conventions suggest
themselves:

 `(' - open paren -- an aside or incidental point
 `?' - question mark -- uncertain or outright question
 `!' - exclamation point/bang -- emphatic
 `[' - open square bracket -- meta-note, about item instead of item's subject
 `\"' - double quote -- a quotation or other citation
 `=' - equal sign -- an assignment, some kind of definition
 `^' - carat -- relates to something above

Some are more elusive, but their rationale may be recognizable:

 `+' - plus -- pending consideration, completion
 `_' - underscore -- done, completed
 `&' - ampersand -- addendum, furthermore

\(Some other non-plain bullets have special meaning to the
software.  By default:

 `~' marks encryptable topics -- see `allout-topic-encryption-bullet'
 `#' marks auto-numbered bullets -- see `allout-numbered-bullet'.)

See `allout-plain-bullets-string' for the standard, alternating
bullets.

You must run `set-allout-regexp' in order for outline mode to
adopt changes of this value.

DO NOT include the close-square-bracket, `]', on either of the bullet
strings."
  :type 'string
  :group 'allout)
(make-variable-buffer-local 'allout-distinctive-bullets-string)
;;;###autoload
(put 'allout-distinctive-bullets-string 'safe-local-variable 'stringp)

;;;_  = allout-use-mode-specific-leader
(defcustom allout-use-mode-specific-leader t
  "When non-nil, use mode-specific topic-header prefixes.

Allout outline mode will use the mode-specific `allout-mode-leaders' or
comment-start string, if any, to lead the topic prefix string, so topic
headers look like comments in the programming language.  It will also use
the comment-start string, with an '_' appended, for `allout-primary-bullet'.

String values are used as literals, not regular expressions, so
do not escape any regular-expression characters.

Value t means to first check for assoc value in `allout-mode-leaders'
alist, then use comment-start string, if any, then use default (`.').
\(See note about use of comment-start strings, below.)

Set to the symbol for either of `allout-mode-leaders' or
`comment-start' to use only one of them, respectively.

Value nil means to always use the default (`.') and leave
`allout-primary-bullet' unaltered.

comment-start strings that do not end in spaces are tripled in
the header-prefix, and an `_' underscore is tacked on the end, to
distinguish them from regular comment strings.  comment-start
strings that do end in spaces are not tripled, but an underscore
is substituted for the space.  [This presumes that the space is
for appearance, not comment syntax.  You can use
`allout-mode-leaders' to override this behavior, when
undesired.]"
  :type '(choice (const t) (const nil) string
		 (const allout-mode-leaders)
		 (const comment-start))
  :group 'allout)
;;;###autoload
(put 'allout-use-mode-specific-leader 'safe-local-variable
     (lambda (x) (or (memq x '(t nil allout-mode-leaders comment-start))
                      (stringp x))))
;;;_  = allout-mode-leaders
(defvar allout-mode-leaders '()
  "Specific allout-prefix leading strings per major modes.

Use this if the mode's comment-start string isn't what you
prefer, or if the mode lacks a comment-start string.  See
`allout-use-mode-specific-leader' for more details.

If you're constructing a string that will comment-out outline
structuring so it can be included in program code, append an extra
character, like an \"_\" underscore, to distinguish the lead string
from regular comments that start at the beginning-of-line.")

;;;_  = allout-old-style-prefixes
(defcustom allout-old-style-prefixes nil
  "When non-nil, use only old-and-crusty `outline-mode' `*' topic prefixes.

Non-nil restricts the topic creation and modification
functions to asterix-padded prefixes, so they look exactly
like the original Emacs-outline style prefixes.

Whatever the setting of this variable, both old and new style prefixes
are always respected by the topic maneuvering functions."
  :type 'boolean
  :group 'allout)
(make-variable-buffer-local 'allout-old-style-prefixes)
;;;###autoload
(put 'allout-old-style-prefixes 'safe-local-variable
     (if (fboundp 'booleanp) 'booleanp (lambda (x) (member x '(t nil)))))
;;;_  = allout-stylish-prefixes -- alternating bullets
(defcustom allout-stylish-prefixes t
  "Do fancy stuff with topic prefix bullets according to level, etc.

Non-nil enables topic creation, modification, and repositioning
functions to vary the topic bullet char (the char that marks the topic
depth) just preceding the start of the topic text) according to level.
Otherwise, only asterisks (`*') and distinctive bullets are used.

This is how an outline can look (but sans indentation) with stylish
prefixes:

    * Top level
    .* A topic
    . + One level 3 subtopic
    .  . One level 4 subtopic
    .  . A second 4 subtopic
    . + Another level 3 subtopic
    .  #1 A numbered level 4 subtopic
    .  #2 Another
    .  ! Another level 4 subtopic with a different distinctive bullet
    .  #4 And another numbered level 4 subtopic

This would be an outline with stylish prefixes inhibited (but the
numbered and other distinctive bullets retained):

    * Top level
    .* A topic
    . * One level 3 subtopic
    .  * One level 4 subtopic
    .  * A second 4 subtopic
    . * Another level 3 subtopic
    .  #1 A numbered level 4 subtopic
    .  #2 Another
    .  ! Another level 4 subtopic with a different distinctive bullet
    .  #4 And another numbered level 4 subtopic

Stylish and constant prefixes (as well as old-style prefixes) are
always respected by the topic maneuvering functions, regardless of
this variable setting.

The setting of this var is not relevant when `allout-old-style-prefixes'
is non-nil."
  :type 'boolean
  :group 'allout)
(make-variable-buffer-local 'allout-stylish-prefixes)
;;;###autoload
(put 'allout-stylish-prefixes 'safe-local-variable
     (if (fboundp 'booleanp) 'booleanp (lambda (x) (member x '(t nil)))))

;;;_  = allout-numbered-bullet
(defcustom allout-numbered-bullet "#"
  "String designating bullet of topics that have auto-numbering; nil for none.

Topics having this bullet have automatic maintenance of a sibling
sequence-number tacked on, just after the bullet.  Conventionally set
to \"#\", you can set it to a bullet of your choice.  A nil value
disables numbering maintenance."
  :type '(choice (const nil) string)
  :group 'allout)
(make-variable-buffer-local 'allout-numbered-bullet)
;;;###autoload
(put 'allout-numbered-bullet 'safe-local-variable
     (if (fboundp 'string-or-null-p)
         'string-or-null-p
       (lambda (x) (or (stringp x) (null x)))))
;;;_  = allout-file-xref-bullet
(defcustom allout-file-xref-bullet "@"
  "Bullet signifying file cross-references, for `allout-resolve-xref'.

Set this var to the bullet you want to use for file cross-references."
  :type '(choice (const nil) string)
  :group 'allout)
;;;###autoload
(put 'allout-file-xref-bullet 'safe-local-variable
     (if (fboundp 'string-or-null-p)
         'string-or-null-p
       (lambda (x) (or (stringp x) (null x)))))
;;;_  = allout-presentation-padding
(defcustom allout-presentation-padding 2
  "Presentation-format white-space padding factor, for greater indent."
  :type 'integer
  :group 'allout)

(make-variable-buffer-local 'allout-presentation-padding)
;;;###autoload
(put 'allout-presentation-padding 'safe-local-variable 'integerp)

;;;_  = allout-flattened-numbering-abbreviation
(define-obsolete-variable-alias 'allout-abbreviate-flattened-numbering
  'allout-flattened-numbering-abbreviation "24.1")
(defcustom allout-flattened-numbering-abbreviation nil
  "If non-nil, `allout-flatten-exposed-to-buffer' abbreviates topic
numbers to minimal amount with some context.  Otherwise, entire
numbers are always used."
  :version "24.1"
  :type 'boolean
  :group 'allout)

;;;_ + LaTeX formatting
;;;_  - allout-number-pages
(defcustom allout-number-pages nil
  "Non-nil turns on page numbering for LaTeX formatting of an outline."
  :type 'boolean
  :group 'allout)
;;;_  - allout-label-style
(defcustom allout-label-style "\\large\\bf"
  "Font and size of labels for LaTeX formatting of an outline."
  :type 'string
  :group 'allout)
;;;_  - allout-head-line-style
(defcustom allout-head-line-style "\\large\\sl "
  "Font and size of entries for LaTeX formatting of an outline."
  :type 'string
  :group 'allout)
;;;_  - allout-body-line-style
(defcustom allout-body-line-style " "
  "Font and size of entries for LaTeX formatting of an outline."
  :type 'string
  :group 'allout)
;;;_  - allout-title-style
(defcustom allout-title-style "\\Large\\bf"
  "Font and size of titles for LaTeX formatting of an outline."
  :type 'string
  :group 'allout)
;;;_  - allout-title
(defcustom allout-title '(or buffer-file-name (buffer-name))
  "Expression to be evaluated to determine the title for LaTeX
formatted copy."
  :type 'sexp
  :group 'allout)
;;;_  - allout-line-skip
(defcustom allout-line-skip ".05cm"
  "Space between lines for LaTeX formatting of an outline."
  :type 'string
  :group 'allout)
;;;_  - allout-indent
(defcustom allout-indent ".3cm"
  "LaTeX formatted depth-indent spacing."
  :type 'string
  :group 'allout)

;;;_ + Topic encryption
;;;_  = allout-encryption group
(defgroup allout-encryption nil
  "Settings for topic encryption features of allout outliner."
  :group 'allout)
;;;_  = allout-topic-encryption-bullet
(defcustom allout-topic-encryption-bullet "~"
  "Bullet signifying encryption of the entry's body."
  :type '(choice (const nil) string)
  :version "22.1"
  :group 'allout-encryption)
;;;_  = allout-encrypt-unencrypted-on-saves
(defcustom allout-encrypt-unencrypted-on-saves t
  "If non-nil, topics pending encryption are encrypted during buffer saves.

This prevents file-system exposure of un-encrypted contents of
items marked for encryption.

When non-nil, if the topic currently being edited is decrypted,
it will be encrypted for saving but automatically decrypted
before any subsequent user interaction, so it is once again clear
text for editing though the file system copy is encrypted.

\(Auto-saves are handled differently.  Buffers with plain-text
exposed encrypted topics are exempted from auto saves until all
such topics are encrypted.)"

  :type 'boolean
  :version "23.1"
  :group 'allout-encryption)
(make-variable-buffer-local 'allout-encrypt-unencrypted-on-saves)
(defvar allout-auto-save-temporarily-disabled nil
  "True while topic encryption is pending and auto-saving was active.

The value of `buffer-saved-size' at the time of decryption is used,
for restoring when all encryptions are established.")
(defvar allout-just-did-undo nil
  "True just after undo commands, until allout-post-command-business.")
(make-variable-buffer-local 'allout-just-did-undo)

;;;_ + Developer
;;;_  = allout-developer group
(defgroup allout-developer nil
  "Allout settings developers care about, including topic encryption and more."
  :group 'allout)
;;;_  = allout-run-unit-tests-on-load
(defcustom allout-run-unit-tests-on-load nil
  "When non-nil, unit tests will be run at end of loading the allout module.

Generally, allout code developers are the only ones who'll want to set this.

\(If set, this makes it an even better practice to exercise changes by
doing byte-compilation with a repeat count, so the file is loaded after
compilation.)

See `allout-run-unit-tests' to see what's run."
  :type 'boolean
  :group 'allout-developer)

;;;_ + Miscellaneous customization

;;;_  = allout-enable-file-variable-adjustment
(defcustom allout-enable-file-variable-adjustment t
  "If non-nil, some allout outline actions edit Emacs local file var text.

This can range from changes to existing entries, addition of new ones,
and creation of a new local variables section when necessary.

Emacs file variables adjustments are also inhibited if `enable-local-variables'
is nil.

Operations potentially causing edits include allout encryption routines.
For details, see `allout-toggle-current-subtree-encryption's docstring."
  :type 'boolean
  :group 'allout)
(make-variable-buffer-local 'allout-enable-file-variable-adjustment)

;;;_* CODE -- no user customizations below.

;;;_ #1 Internal Outline Formatting and Configuration
;;;_  : Version
;;;_   = allout-version
(defvar allout-version "2.3"
  "Version of currently loaded outline package.  (allout.el)")
;;;_   > allout-version
(defun allout-version (&optional here)
  "Return string describing the loaded outline version."
  (interactive "P")
  (let ((msg (concat "Allout Outline Mode v " allout-version)))
    (if here (insert msg))
    (message "%s" msg)
    msg))
;;;_  : Mode activation (defined here because it's referenced early)
;;;_   = allout-mode
(defvar allout-mode nil "Allout outline mode minor-mode flag.")
(make-variable-buffer-local 'allout-mode)
;;;_   = allout-layout nil
(defvar allout-layout nil            ; LEAVE GLOBAL VALUE NIL -- see docstring.
  "Buffer-specific setting for allout layout.

In buffers where this is non-nil (and if `allout-auto-activation'
has been customized to enable this behavior), `allout-mode' will be
automatically activated.  The layout dictated by the value will be used to
set the initial exposure when `allout-mode' is activated.

\*You should not setq-default this variable non-nil unless you want every
visited file to be treated as an allout file.*

The value would typically be set by a file local variable.  For
example, the following lines at the bottom of an Emacs Lisp file:

;;;Local variables:
;;;allout-layout: (0 : -1 -1 0)
;;;End:

dictate activation of `allout-mode' mode when the file is visited
\(presuming proper `allout-auto-activation' customization),
followed by the equivalent of `(allout-expose-topic 0 : -1 -1 0)'.
\(This is the layout used for the allout.el source file.)

`allout-default-layout' describes the specification format.
`allout-layout' can additionally have the value `t', in which
case the value of `allout-default-layout' is used.")
(make-variable-buffer-local 'allout-layout)
;;;###autoload
(put 'allout-layout 'safe-local-variable
     (lambda (x) (or (numberp x) (listp x) (memq x '(: * + -)))))

;;;_  : Topic header format
;;;_   = allout-regexp
(defvar allout-regexp ""
  "*Regular expression to match the beginning of a heading line.

Any line whose beginning matches this regexp is considered a
heading.  This var is set according to the user configuration vars
by `set-allout-regexp'.")
(make-variable-buffer-local 'allout-regexp)
;;;_   = allout-bullets-string
(defvar allout-bullets-string ""
  "A string dictating the valid set of outline topic bullets.

This var should *not* be set by the user -- it is set by `set-allout-regexp',
and is produced from the elements of `allout-plain-bullets-string'
and `allout-distinctive-bullets-string'.")
(make-variable-buffer-local 'allout-bullets-string)
;;;_   = allout-bullets-string-len
(defvar allout-bullets-string-len 0
  "Length of current buffers' `allout-plain-bullets-string'.")
(make-variable-buffer-local 'allout-bullets-string-len)
;;;_   = allout-depth-specific-regexp
(defvar allout-depth-specific-regexp ""
  "*Regular expression to match a heading line prefix for a particular depth.

This expression is used to search for depth-specific topic
headers at depth 2 and greater.  Use `allout-depth-one-regexp'
for to seek topics at depth one.

This var is set according to the user configuration vars by
`set-allout-regexp'.  It is prepared with format strings for two
decimal numbers, which should each be one less than the depth of the
topic prefix to be matched.")
(make-variable-buffer-local 'allout-depth-specific-regexp)
;;;_   = allout-depth-one-regexp
(defvar allout-depth-one-regexp ""
  "*Regular expression to match a heading line prefix for depth one.

This var is set according to the user configuration vars by
`set-allout-regexp'.  It is prepared with format strings for two
decimal numbers, which should each be one less than the depth of the
topic prefix to be matched.")
(make-variable-buffer-local 'allout-depth-one-regexp)
;;;_   = allout-line-boundary-regexp
(defvar allout-line-boundary-regexp ()
  "`allout-regexp' prepended with a newline for the search target.

This is properly set by `set-allout-regexp'.")
(make-variable-buffer-local 'allout-line-boundary-regexp)
;;;_   = allout-bob-regexp
(defvar allout-bob-regexp ()
  "Like `allout-line-boundary-regexp', for headers at beginning of buffer.")
(make-variable-buffer-local 'allout-bob-regexp)
;;;_   = allout-header-subtraction
(defvar allout-header-subtraction (1- (length allout-header-prefix))
  "Allout-header prefix length to subtract when computing topic depth.")
(make-variable-buffer-local 'allout-header-subtraction)
;;;_   = allout-plain-bullets-string-len
(defvar allout-plain-bullets-string-len (length allout-plain-bullets-string)
  "Length of `allout-plain-bullets-string', updated by `set-allout-regexp'.")
(make-variable-buffer-local 'allout-plain-bullets-string-len)

;;;_   = allout-doublecheck-at-and-shallower
(defconst allout-doublecheck-at-and-shallower 3
  "Validate apparent topics of this depth and shallower as being non-aberrant.

Verified with `allout-aberrant-container-p'.  The usefulness of
this check is limited to shallow depths, because the
determination of aberrance is according to the mistaken item
being followed by a legitimate item of excessively greater depth.

The classic example of a mistaken item, for a standard allout
outline configuration, is a body line that begins with an '...'
ellipsis.  This happens to contain a legitimate depth-2 header
prefix, constituted by two '..' dots at the beginning of the
line.  The only thing that can distinguish it *in principle* from
a legitimate one is if the following real header is at a depth
that is discontinuous from the depth of 2 implied by the
ellipsis, ie depth 4 or more.  As the depth being tested gets
greater, the likelihood of this kind of disqualification is
lower, and the usefulness of this test is lower.

Extending the depth of the doublecheck increases the amount it is
applied, increasing the cost of the test - on casual estimation,
for outlines with many deep topics, geometrically (O(n)?).
Taken together with decreasing likelihood that the test will be
useful at greater depths, more modest doublecheck limits are more
suitably economical.")
;;;_   X allout-reset-header-lead (header-lead)
(defun allout-reset-header-lead (header-lead)
  "Reset the leading string used to identify topic headers."
  (interactive "sNew lead string: ")
  (setq allout-header-prefix header-lead)
  (setq allout-header-subtraction (1- (length allout-header-prefix)))
  (set-allout-regexp))
;;;_   X allout-lead-with-comment-string (header-lead)
(defun allout-lead-with-comment-string (&optional header-lead)
  "Set the topic-header leading string to specified string.

Useful for encapsulating outline structure in programming
language comments.  Returns the leading string."

  (interactive "P")
  (if (not (stringp header-lead))
      (setq header-lead (read-string
                         "String prefix for topic headers: ")))
  (setq allout-reindent-bodies nil)
  (allout-reset-header-lead header-lead)
  header-lead)
;;;_   > allout-infer-header-lead-and-primary-bullet ()
(defun allout-infer-header-lead-and-primary-bullet ()
  "Determine appropriate `allout-header-prefix' and `allout-primary-bullet'.

Works according to settings of:

       `comment-start'
       `allout-header-prefix' (default)
       `allout-use-mode-specific-leader'
and    `allout-mode-leaders'.

Apply this via (re)activation of `allout-mode', rather than
invoking it directly."
  (let* ((use-leader (and (boundp 'allout-use-mode-specific-leader)
			  (if (or (stringp allout-use-mode-specific-leader)
				  (memq allout-use-mode-specific-leader
					'(allout-mode-leaders
					  comment-start
					  t)))
			      allout-use-mode-specific-leader
			    ;; Oops -- garbled value, equate with effect of t:
			    t)))
	 (leader
	  (cond
	   ((not use-leader) nil)
	   ;; Use the explicitly designated leader:
	   ((stringp use-leader) use-leader)
	   (t (or (and (memq use-leader '(t allout-mode-leaders))
		       ;; Get it from outline mode leaders?
		       (cdr (assq major-mode allout-mode-leaders)))
		  ;; ... didn't get from allout-mode-leaders...
		  (and (memq use-leader '(t comment-start))
		       comment-start
		       ;; Use comment-start, maybe tripled, and with
		       ;; underscore:
		       (concat
			(if (string= " "
				     (substring comment-start
						(1- (length comment-start))))
			    ;; Use comment-start, sans trailing space:
			    (substring comment-start 0 -1)
			  (concat comment-start comment-start comment-start))
			;; ... and append underscore, whichever:
			"_")))))))
    (if (not leader)
	nil
      (setq allout-header-prefix leader)
      (if (not allout-old-style-prefixes)
          ;; setting allout-primary-bullet makes the top level topics use --
          ;; actually, be -- the special prefix:
          (setq allout-primary-bullet leader))
      allout-header-prefix)))
(defalias 'allout-infer-header-lead
  'allout-infer-header-lead-and-primary-bullet)
;;;_   > allout-infer-body-reindent ()
(defun allout-infer-body-reindent ()
  "Determine proper setting for `allout-reindent-bodies'.

Depends on default setting of `allout-reindent-bodies' (which see)
and presence of setting for `comment-start', to tell whether the
file is programming code."
  (if (and allout-reindent-bodies
	   comment-start
	   (not (eq 'force allout-reindent-bodies)))
      (setq allout-reindent-bodies nil)))
;;;_   > set-allout-regexp ()
(defun set-allout-regexp ()
  "Generate proper topic-header regexp form for outline functions.

Works with respect to `allout-plain-bullets-string' and
`allout-distinctive-bullets-string'.

Also refresh various data structures that hinge on the regexp."

  (interactive)
  ;; Derive allout-bullets-string from user configured components:
  (setq allout-bullets-string "")
  (let ((strings (list 'allout-plain-bullets-string
                       'allout-distinctive-bullets-string
                       'allout-primary-bullet))
        cur-string
        cur-len
        cur-char
        index)
    (while strings
      (setq index 0)
      (setq cur-len (length (setq cur-string (symbol-value (car strings)))))
      (while (< index cur-len)
        (setq cur-char (aref cur-string index))
        (setq allout-bullets-string
              (concat allout-bullets-string
                      (cond
                                        ; Single dash would denote a
                                        ; sequence, repeated denotes
                                        ; a dash:
                       ((eq cur-char ?-) "--")
                                        ; literal close-square-bracket
                                        ; doesn't work right in the
                                        ; expr, exclude it:
                       ((eq cur-char ?\]) "")
                       (t (regexp-quote  (char-to-string cur-char))))))
        (setq index (1+ index)))
      (setq strings (cdr strings)))
    )
  ;; Derive next for repeated use in allout-pending-bullet:
  (setq allout-plain-bullets-string-len (length allout-plain-bullets-string))
  (setq allout-header-subtraction (1- (length allout-header-prefix)))

  (let (new-part old-part formfeed-part)
    (setq new-part (concat "\\("
                           (regexp-quote allout-header-prefix)
                           "[ \t]*"
                           ;; already regexp-quoted in a custom way:
                           "[" allout-bullets-string "]"
                           "\\)")
          old-part (concat "\\("
                           (regexp-quote allout-primary-bullet)
                           "\\|"
                           (regexp-quote allout-header-prefix)
                           "\\)"
                           "+"
                           " ?[^" allout-primary-bullet "]")
          formfeed-part "\\(\^L\\)"

          allout-regexp (concat new-part
                                "\\|"
                                old-part
                                "\\|"
                                formfeed-part)

          allout-line-boundary-regexp (concat "\n" new-part
                                              "\\|"
                                              "\n" old-part
                                              "\\|"
                                              "\n" formfeed-part)

          allout-bob-regexp (concat "\\`" new-part
                                    "\\|"
                                    "\\`" old-part
                                    "\\|"
                                    "\\`" formfeed-part
                                    ))

    (setq allout-depth-specific-regexp
          (concat "\\(^\\|\\`\\)"
                  "\\("

                  ;; new-style spacers-then-bullet string:
                  "\\("
                  (allout-format-quote (regexp-quote allout-header-prefix))
                  " \\{%s\\}"
                  "[" (allout-format-quote allout-bullets-string) "]"
                  "\\)"

                  ;; old-style all-bullets string, if primary not multi-char:
                  (if (< 0 allout-header-subtraction)
                      ""
                    (concat "\\|\\("
                            (allout-format-quote
                             (regexp-quote allout-primary-bullet))
                            (allout-format-quote
                             (regexp-quote allout-primary-bullet))
                            (allout-format-quote
                             (regexp-quote allout-primary-bullet))
                            "\\{%s\\}"
                            ;; disqualify greater depths:
                            "[^"
                            (allout-format-quote allout-primary-bullet)
                            "]\\)"
                            ))
                  "\\)"
                  ))
    (setq allout-depth-one-regexp
          (concat "\\(^\\|\\`\\)"
                  "\\("

                  "\\("
                  (regexp-quote allout-header-prefix)
                  ;; disqualify any bullet char following any amount of
                  ;; intervening whitespace:
                  " *"
                  (concat "[^ " allout-bullets-string "]")
                  "\\)"
                  (if (< 0 allout-header-subtraction)
                      ;; Need not support anything like the old
                      ;; bullet style if the prefix is multi-char.
                      ""
                    (concat "\\|"
                            (regexp-quote allout-primary-bullet)
                            ;; disqualify deeper primary-bullet sequences:
                            "[^" allout-primary-bullet "]"))
                  "\\)"
                  ))))
;;;_  : Menu bar
(defvar allout-mode-exposure-menu)
(defvar allout-mode-editing-menu)
(defvar allout-mode-navigation-menu)
(defvar allout-mode-misc-menu)
(defun produce-allout-mode-menubar-entries ()
  (require 'easymenu)
  (easy-menu-define allout-mode-exposure-menu
		    allout-mode-map-value
		    "Allout outline exposure menu."
		    '("Exposure"
		      ["Show Entry" allout-show-current-entry t]
		      ["Show Children" allout-show-children t]
		      ["Show Subtree" allout-show-current-subtree t]
		      ["Hide Subtree" allout-hide-current-subtree t]
		      ["Hide Leaves" allout-hide-current-leaves t]
		      "----"
		      ["Show All" allout-show-all t]))
  (easy-menu-define allout-mode-editing-menu
		    allout-mode-map-value
		    "Allout outline editing menu."
		    '("Headings"
		      ["Open Sibling" allout-open-sibtopic t]
		      ["Open Subtopic" allout-open-subtopic t]
		      ["Open Supertopic" allout-open-supertopic t]
		      "----"
		      ["Shift Topic In" allout-shift-in t]
		      ["Shift Topic Out" allout-shift-out t]
		      ["Rebullet Topic" allout-rebullet-topic t]
		      ["Rebullet Heading" allout-rebullet-current-heading t]
		      ["Number Siblings" allout-number-siblings t]
		      "----"
                      ["Toggle Topic Encryption"
                       allout-toggle-current-subtree-encryption
                       (> (allout-current-depth) 1)]))
  (easy-menu-define allout-mode-navigation-menu
		    allout-mode-map-value
		    "Allout outline navigation menu."
		    '("Navigation"
		      ["Next Visible Heading" allout-next-visible-heading t]
		      ["Previous Visible Heading"
		       allout-previous-visible-heading t]
		      "----"
		      ["Up Level" allout-up-current-level t]
		      ["Forward Current Level" allout-forward-current-level t]
		      ["Backward Current Level"
		       allout-backward-current-level t]
		      "----"
		      ["Beginning of Entry"
		       allout-beginning-of-current-entry t]
		      ["End of Entry" allout-end-of-entry t]
		      ["End of Subtree" allout-end-of-current-subtree t]))
  (easy-menu-define allout-mode-misc-menu
		    allout-mode-map-value
		    "Allout outlines miscellaneous bindings."
		    '("Misc"
		      ["Version" allout-version t]
		      "----"
		      ["Duplicate Exposed" allout-copy-exposed-to-buffer t]
		      ["Duplicate Exposed, numbered"
		       allout-flatten-exposed-to-buffer t]
		      ["Duplicate Exposed, indented"
		       allout-indented-exposed-to-buffer t]
		      "----"
		      ["Set Header Lead" allout-reset-header-lead t]
		      ["Set New Exposure" allout-expose-topic t])))
;;;_  : Allout Modal-Variables Utilities
;;;_   = allout-mode-prior-settings
(defvar allout-mode-prior-settings nil
  "Internal `allout-mode' use; settings to be resumed on mode deactivation.

See `allout-add-resumptions' and `allout-do-resumptions'.")
(make-variable-buffer-local 'allout-mode-prior-settings)
;;;_   > allout-add-resumptions (&rest pairs)
(defun allout-add-resumptions (&rest pairs)
  "Set name/value PAIRS.

Old settings are preserved for later resumption using `allout-do-resumptions'.

The new values are set as a buffer local.  On resumption, the prior buffer
scope of the variable is restored along with its value.  If it was a void
buffer-local value, then it is left as nil on resumption.

The pairs are lists whose car is the name of the variable and car of the
cdr is the new value: '(some-var some-value)'.  The pairs can actually be
triples, where the third element qualifies the disposition of the setting,
as described further below.

If the optional third element is the symbol 'extend, then the new value
created by `cons'ing the second element of the pair onto the front of the
existing value.

If the optional third element is the symbol 'append, then the new value is
extended from the existing one by `append'ing a list containing the second
element of the pair onto the end of the existing value.

Extension, and resumptions in general, should not be used for hook
functions -- use the 'local mode of `add-hook' for that, instead.

The settings are stored on `allout-mode-prior-settings'."
  (while pairs
    (let* ((pair (pop pairs))
           (name (car pair))
           (value (cadr pair))
           (qualifier (if (> (length pair) 2)
                          (caddr pair)))
           prior-value)
      (if (not (symbolp name))
          (error "Pair's name, %S, must be a symbol, not %s"
                 name (type-of name)))
      (setq prior-value (condition-case nil
                            (symbol-value name)
                          (void-variable nil)))
      (when (not (assoc name allout-mode-prior-settings))
        ;; Not already added as a resumption, create the prior setting entry.
        (if (local-variable-p name (current-buffer))
            ;; is already local variable -- preserve the prior value:
            (push (list name prior-value) allout-mode-prior-settings)
          ;; wasn't local variable, indicate so for resumption by killing
          ;; local value, and make it local:
          (push (list name) allout-mode-prior-settings)
          (make-local-variable name)))
      (if qualifier
          (cond ((eq qualifier 'extend)
                 (if (not (listp prior-value))
                     (error "extension of non-list prior value attempted")
                   (set name (cons value prior-value))))
                ((eq qualifier 'append)
                 (if (not (listp prior-value))
                     (error "appending of non-list prior value attempted")
                   (set name (append prior-value (list value)))))
                (t (error "unrecognized setting qualifier `%s' encountered"
                          qualifier)))
        (set name value)))))
;;;_   > allout-do-resumptions ()
(defun allout-do-resumptions ()
  "Resume all name/value settings registered by `allout-add-resumptions'.

This is used when concluding allout-mode, to resume selected variables to
their settings before allout-mode was started."

    (while allout-mode-prior-settings
      (let* ((pair (pop allout-mode-prior-settings))
             (name (car pair))
             (value-cell (cdr pair)))
        (if (not value-cell)
            ;; Prior value was global:
            (kill-local-variable name)
          ;; Prior value was explicit:
          (set name (car value-cell))))))
;;;_  : Mode-specific incidentals
;;;_   > allout-unprotected (expr)
(defmacro allout-unprotected (expr)
  "Enable internal outline operations to alter invisible text."
  `(let ((inhibit-read-only (if (not buffer-read-only) t))
         (inhibit-field-text-motion t))
     ,expr))
;;;_   = allout-mode-hook
(defvar allout-mode-hook nil
  "*Hook that's run when allout mode starts.")
;;;_   = allout-mode-deactivate-hook
(defvar allout-mode-deactivate-hook nil
  "*Hook that's run when allout mode ends.")
(define-obsolete-variable-alias 'allout-mode-deactivate-hook
  'allout-mode-off-hook "24.1")
;;;_   = allout-exposure-category
(defvar allout-exposure-category nil
  "Symbol for use as allout invisible-text overlay category.")
;;;_   = allout-exposure-change-hook
(defvar allout-exposure-change-hook nil
  "*Hook that's run after allout outline subtree exposure changes.

It is run at the conclusion of `allout-flag-region'.

Functions on the hook must take three arguments:

 - FROM -- integer indicating the point at the start of the change.
 - TO -- integer indicating the point of the end of the change.
 - FLAG -- change mode: nil for exposure, otherwise concealment.

This hook might be invoked multiple times by a single command.")
;;;_   = allout-structure-added-hook
(defvar allout-structure-added-hook nil
  "*Hook that's run after addition of items to the outline.

Functions on the hook should take two arguments:

 - NEW-START -- integer indicating position of start of the first new item.
 - NEW-END -- integer indicating position of end of the last new item.

This hook might be invoked multiple times by a single command.")
;;;_   = allout-structure-deleted-hook
(defvar allout-structure-deleted-hook nil
  "*Hook that's run after disciplined deletion of subtrees from the outline.

Functions on the hook must take two arguments:

 - DEPTH -- integer indicating the depth of the subtree that was deleted.
 - REMOVED-FROM -- integer indicating the point where the subtree was removed.

Some edits that remove or invalidate items may be missed by this hook:
specifically edits that native allout routines do not control.

This hook might be invoked multiple times by a single command.")
;;;_   = allout-structure-shifted-hook
(defvar allout-structure-shifted-hook nil
  "*Hook that's run after shifting of items in the outline.

Functions on the hook should take two arguments:

 - DEPTH-CHANGE -- integer indicating depth increase, negative for decrease
 - START -- integer indicating the start point of the shifted parent item.

Some edits that shift items can be missed by this hook: specifically edits
that native allout routines do not control.

This hook might be invoked multiple times by a single command.")
;;;_   = allout-after-copy-or-kill-hook
(defvar allout-after-copy-or-kill-hook nil
  "*Hook that's run after copying outline text.

Functions on the hook should not require any arguments.")
;;;_   = allout-post-undo-hook
(defvar allout-post-undo-hook nil
  "*Hook that's run after undo activity.

The item that's current when the hook is run *may* be the one
that was affected by the undo.

Functions on the hook should not require any arguments.")
;;;_   = allout-outside-normal-auto-fill-function
(defvar allout-outside-normal-auto-fill-function nil
  "Value of `normal-auto-fill-function' outside of allout mode.

Used by `allout-auto-fill' to do the mandated `normal-auto-fill-function'
wrapped within allout's automatic `fill-prefix' setting.")
(make-variable-buffer-local 'allout-outside-normal-auto-fill-function)
;;;_   = prevent redundant activation by desktop mode:
(add-to-list 'desktop-minor-mode-handlers '(allout-mode . nil))
;;;_   = allout-passphrase-verifier-string
(defvar allout-passphrase-verifier-string nil
  "Setting used to test solicited encryption passphrases against the one
already associated with a file.

It consists of an encrypted random string useful only to verify that a
passphrase entered by the user is effective for decryption.  The passphrase
itself is \*not* recorded in the file anywhere, and the encrypted contents
are random binary characters to avoid exposing greater susceptibility to
search attacks.

The verifier string is retained as an Emacs file variable, as well as in
the Emacs buffer state, if file variable adjustments are enabled.  See
`allout-enable-file-variable-adjustment' for details about that.")
(make-variable-buffer-local 'allout-passphrase-verifier-string)
(make-obsolete 'allout-passphrase-verifier-string
               'allout-passphrase-verifier-string "23.3")
;;;###autoload
(put 'allout-passphrase-verifier-string 'safe-local-variable 'stringp)
;;;_   = allout-passphrase-hint-string
(defvar allout-passphrase-hint-string ""
  "Variable used to retain reminder string for file's encryption passphrase.

See the description of `allout-passphrase-hint-handling' for details about how
the reminder is deployed.

The hint is retained as an Emacs file variable, as well as in the Emacs buffer
state, if file variable adjustments are enabled.  See
`allout-enable-file-variable-adjustment' for details about that.")
(make-variable-buffer-local 'allout-passphrase-hint-string)
(setq-default allout-passphrase-hint-string "")
(make-obsolete 'allout-passphrase-hint-string
               'allout-passphrase-hint-string "23.3")
;;;###autoload
(put 'allout-passphrase-hint-string 'safe-local-variable 'stringp)
;;;_   = allout-after-save-decrypt
(defvar allout-after-save-decrypt nil
  "Internal variable, is nil or has the value of two points:

 - the location of a topic to be decrypted after saving is done
 - where to situate the cursor after the decryption is performed

This is used to decrypt the topic that was currently being edited, if it
was encrypted automatically as part of a file write or autosave.")
(make-variable-buffer-local 'allout-after-save-decrypt)
;;;_   = allout-encryption-plaintext-sanitization-regexps
(defvar allout-encryption-plaintext-sanitization-regexps nil
  "List of regexps whose matches are removed from plaintext before encryption.

This is for the sake of removing artifacts, like escapes, that are added on
and not actually part of the original plaintext.  The removal is done just
prior to encryption.

Entries must be symbols that are bound to the desired values.

Each value can be a regexp or a list with a regexp followed by a
substitution string.  If it's just a regexp, all its matches are removed
before the text is encrypted.  If it's a regexp and a substitution, the
substitution is used against the regexp matches, a la `replace-match'.")
(make-variable-buffer-local 'allout-encryption-text-removal-regexps)
;;;_   = allout-encryption-ciphertext-rejection-regexps
(defvar allout-encryption-ciphertext-rejection-regexps nil
  "Variable for regexps matching plaintext to remove before encryption.

This is used to detect strings in encryption results that would
register as allout mode structural elements, for example, as a
topic prefix.

Entries must be symbols that are bound to the desired regexp values.

Encryptions that result in matches will be retried, up to
`allout-encryption-ciphertext-rejection-limit' times, after which
an error is raised.")

(make-variable-buffer-local 'allout-encryption-ciphertext-rejection-regexps)
;;;_   = allout-encryption-ciphertext-rejection-ceiling
(defvar allout-encryption-ciphertext-rejection-ceiling 5
  "Limit on number of times encryption ciphertext is rejected.

See `allout-encryption-ciphertext-rejection-regexps' for rejection reasons.")
(make-variable-buffer-local 'allout-encryption-ciphertext-rejection-ceiling)
;;;_   > allout-mode-p ()
;; Must define this macro above any uses, or byte compilation will lack
;; proper def, if file isn't loaded -- eg, during emacs build!
;;;###autoload
(defmacro allout-mode-p ()
  "Return t if `allout-mode' is active in current buffer."
  'allout-mode)
;;;_   > allout-write-contents-hook-handler ()
(defun allout-write-contents-hook-handler ()
  "Implement `allout-encrypt-unencrypted-on-saves' for file writes

Return nil if all goes smoothly, or else return an informative
message if an error is encountered.  The message will serve as a
non-nil return on `write-contents-functions' to prevent saving of
the buffer while it has decrypted content.

This behavior depends on Emacs versions that implement the
`write-contents-functions' hook."

  (if (or (not (allout-mode-p))
          (not (boundp 'allout-encrypt-unencrypted-on-saves))
          (not allout-encrypt-unencrypted-on-saves))
      nil
    (if (save-excursion (goto-char (point-min))
                        (allout-next-topic-pending-encryption))
        (progn
          (message "auto-encrypting pending topics")
          (sit-for 0)
          (condition-case failure
              (progn
                (setq allout-after-save-decrypt
                      (allout-encrypt-decrypted))
                ;; aok - return nil:
                nil)
            (error
             ;; whoops - probably some still-decrypted items, return non-nil:
             (let ((text (format (concat "%s contents write inhibited due to"
                                         " encrypted topic encryption error:"
                                         " %s")
                                 (buffer-name (current-buffer))
                                 failure)))
               (message text)(sit-for 2)
               text)))))
    ))
;;;_   > allout-after-saves-handler ()
(defun allout-after-saves-handler ()
  "Decrypt topic encrypted for save, if it's currently being edited.

Ie, if it was pending encryption and contained the point in its body before
the save.

We use values stored in `allout-after-save-decrypt' to locate the topic
and the place for the cursor after the decryption is done."
  (if (not (and (allout-mode-p)
                (boundp 'allout-after-save-decrypt)
                allout-after-save-decrypt))
      t
    (goto-char (car allout-after-save-decrypt))
    (let ((was-modified (buffer-modified-p)))
      (allout-toggle-subtree-encryption)
      (if (not was-modified)
          (set-buffer-modified-p nil)))
    (goto-char (cadr allout-after-save-decrypt))
    (setq allout-after-save-decrypt nil))
  )
;;;_   > allout-called-interactively-p ()
(defmacro allout-called-interactively-p ()
  "A version of `called-interactively-p' independent of Emacs version."
  ;; ... to ease maintenance of allout without betraying deprecation.
  (if (equal (subr-arity (symbol-function 'called-interactively-p))
             '(0 . 0))
      '(called-interactively-p)
    '(called-interactively-p 'interactive)))
;;;_   = allout-inhibit-aberrance-doublecheck nil
;; In some exceptional moments, disparate topic depths need to be allowed
;; momentarily, eg when one topic is being yanked into another and they're
;; about to be reconciled.  let-binding allout-inhibit-aberrance-doublecheck
;; prevents the aberrance doublecheck to allow, eg, the reconciliation
;; processing to happen in the presence of such discrepancies.  It should
;; almost never be needed, however.
(defvar allout-inhibit-aberrance-doublecheck nil
  "Internal state, for momentarily inhibits aberrance doublecheck.

This should only be momentarily let-bound non-nil, not set
non-nil in a lasting way.")

;;;_ #2 Mode environment and activation
;;;_  = allout-explicitly-deactivated
(defvar allout-explicitly-deactivated nil
  "If t, `allout-mode's last deactivation was deliberate.
So `allout-post-command-business' should not reactivate it...")
(make-variable-buffer-local 'allout-explicitly-deactivated)
;;;_  > allout-init (mode)
(defun allout-init (mode)
  "DEPRECATED - configure allout activation by customizing
`allout-auto-activation'.  This function remains around, limited
from what it did before, for backwards compatibility.

MODE is the activation mode - see `allout-auto-activation' for
valid values."

  (custom-set-variables (list 'allout-auto-activation (format "%s" mode)))
  (format "%s" mode))
(make-obsolete 'allout-init
               "customize 'allout-auto-activation' instead." "23.3")
;;;_  > allout-setup-menubar ()
(defun allout-setup-menubar ()
  "Populate the current buffer's menubar with `allout-mode' stuff."
  (let ((menus (list allout-mode-exposure-menu
		     allout-mode-editing-menu
		     allout-mode-navigation-menu
		     allout-mode-misc-menu))
	cur)
    (while menus
      (setq cur (car menus)
	    menus (cdr menus))
      (easy-menu-add cur))))
;;;_  > allout-overlay-preparations
(defun allout-overlay-preparations ()
  "Set the properties of the allout invisible-text overlay and others."
  (setplist 'allout-exposure-category nil)
  (put 'allout-exposure-category 'invisible 'allout)
  (put 'allout-exposure-category 'evaporate t)
  ;; ??? We use isearch-open-invisible *and* isearch-mode-end-hook.  The
  ;; latter would be sufficient, but it seems that a separate behavior --
  ;; the _transient_ opening of invisible text during isearch -- is keyed to
  ;; presence of the isearch-open-invisible property -- even though this
  ;; property controls the isearch _arrival_ behavior.  This is the case at
  ;; least in emacs 21, 22.1, and xemacs 21.4.
  (put 'allout-exposure-category 'isearch-open-invisible
       'allout-isearch-end-handler)
  (if (featurep 'xemacs)
      (put 'allout-exposure-category 'start-open t)
    (put 'allout-exposure-category 'insert-in-front-hooks
         '(allout-overlay-insert-in-front-handler)))
  (put 'allout-exposure-category 'modification-hooks
       '(allout-overlay-interior-modification-handler)))
;;;_  > define-minor-mode allout-mode
;;;_   : Defun:
;;;###autoload
(define-minor-mode allout-mode
;;;_    . Doc string:
  "Toggle Allout outline mode.
With a prefix argument ARG, enable Allout outline mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

\\<allout-mode-map-value>
Allout outline mode is a minor mode that provides extensive
outline oriented formatting and manipulation.  It enables
structural editing of outlines, as well as navigation and
exposure.  It also is specifically aimed at accommodating
syntax-sensitive text like programming languages.  (For example,
see the allout code itself, which is organized as an allout
outline.)

In addition to typical outline navigation and exposure, allout includes:

 - topic-oriented authoring, including keystroke-based topic creation,
   repositioning, promotion/demotion, cut, and paste
 - incremental search with dynamic exposure and reconcealment of hidden text
 - adjustable format, so programming code can be developed in outline-structure
 - easy topic encryption and decryption, symmetric or key-pair
 - \"Hot-spot\" operation, for single-keystroke maneuvering and exposure control
 - integral outline layout, for automatic initial exposure when visiting a file
 - independent extensibility, using comprehensive exposure and authoring hooks

and many other features.

Below is a description of the key bindings, and then description
of special `allout-mode' features and terminology.  See also the
outline menubar additions for quick reference to many of the
features.  Customize `allout-auto-activation' to prepare your
Emacs session for automatic activation of `allout-mode'.

The bindings are those listed in `allout-prefixed-keybindings'
and `allout-unprefixed-keybindings'.  We recommend customizing
`allout-command-prefix' to use just `\\C-c' as the command
prefix, if the allout bindings don't conflict with any personal
bindings you have on \\C-c.  In any case, outline structure
navigation and authoring is simplified by positioning the cursor
on an item's bullet character, the \"hot-spot\" -- then you can
invoke allout commands with just the un-prefixed,
un-control-shifted command letters.  This is described further in
the HOT-SPOT Operation section.

        Exposure Control:
        ----------------
\\[allout-hide-current-subtree]   `allout-hide-current-subtree'
\\[allout-show-children] `allout-show-children'
\\[allout-show-current-subtree] `allout-show-current-subtree'
\\[allout-show-current-entry] `allout-show-current-entry'
\\[allout-show-all]   `allout-show-all'

        Navigation:
        ----------
\\[allout-next-visible-heading] `allout-next-visible-heading'
\\[allout-previous-visible-heading] `allout-previous-visible-heading'
\\[allout-up-current-level] `allout-up-current-level'
\\[allout-forward-current-level] `allout-forward-current-level'
\\[allout-backward-current-level] `allout-backward-current-level'
\\[allout-end-of-entry] `allout-end-of-entry'
\\[allout-beginning-of-current-entry] `allout-beginning-of-current-entry' (alternately, goes to hot-spot)
\\[allout-beginning-of-line]  `allout-beginning-of-line' -- like regular beginning-of-line, but
     if immediately repeated cycles to the beginning of the current item
     and then to the hot-spot (if `allout-beginning-of-line-cycles' is set).


        Topic Header Production:
        -----------------------
\\[allout-open-sibtopic] `allout-open-sibtopic' Create a new sibling after current topic.
\\[allout-open-subtopic]   `allout-open-subtopic' ... an offspring of current topic.
\\[allout-open-supertopic] `allout-open-supertopic' ... a sibling of the current topic's parent.

        Topic Level and Prefix Adjustment:
        ---------------------------------
\\[allout-shift-in] `allout-shift-in'   Shift current topic and all offspring deeper
\\[allout-shift-out] `allout-shift-out' ... less deep
\\[allout-rebullet-current-heading] `allout-rebullet-current-heading' Prompt for alternate bullet for
            current topic
\\[allout-rebullet-topic] `allout-rebullet-topic'   Reconcile bullets of topic and
            its offspring -- distinctive bullets are not changed, others
            are alternated according to nesting depth.
\\[allout-number-siblings] `allout-number-siblings'  Number bullets of topic and siblings --
           the offspring are not affected.
           With repeat count, revoke numbering.

        Topic-oriented Killing and Yanking:
        ----------------------------------
\\[allout-kill-topic] `allout-kill-topic'   Kill current topic, including offspring.
\\[allout-copy-topic-as-kill] `allout-copy-topic-as-kill' Copy current topic, including offspring.
\\[allout-kill-line]     `allout-kill-line'    Kill line, attending to outline structure.
\\[allout-copy-line-as-kill]     `allout-copy-line-as-kill' Copy line but don't delete it.
\\[allout-yank] `allout-yank'        Yank, adjusting depth of yanked topic to
                             depth of heading if yanking into bare topic
                             heading (ie, prefix sans text).
\\[allout-yank-pop]     `allout-yank-pop'       Is to `allout-yank' as `yank-pop' is to `yank'.

        Topic-oriented Encryption:
        -------------------------
\\[allout-toggle-current-subtree-encryption] `allout-toggle-current-subtree-encryption'
          Encrypt/Decrypt topic content

        Misc commands:
        -------------
M-x outlineify-sticky       Activate outline mode for current buffer,
                            and establish a default file-var setting
                            for `allout-layout'.
\\[allout-mark-topic]       `allout-mark-topic'
\\[allout-copy-exposed-to-buffer] `allout-copy-exposed-to-buffer'
                            Duplicate outline, sans concealed text, to
                            buffer with name derived from derived from that
                            of current buffer -- \"*BUFFERNAME exposed*\".
\\[allout-flatten-exposed-to-buffer] `allout-flatten-exposed-to-buffer'
                            Like above 'copy-exposed', but convert topic
                            prefixes to section.subsection... numeric
                            format.
\\[customize-variable] allout-auto-activation
                            Prepare Emacs session for allout outline mode
                            auto-activation.

                  Topic Encryption

Outline mode supports gpg encryption of topics, with support for
symmetric and key-pair modes, and auto-encryption of topics
pending encryption on save.

Topics pending encryption are, by default, automatically
encrypted during file saves, including checkpoint saves, to avoid
exposing the plain text of encrypted topics in the file system.
If the content of the topic containing the cursor was encrypted
for a save, it is automatically decrypted for continued editing.

NOTE: A few GnuPG v2 versions improperly preserve incorrect
symmetric decryption keys, preventing entry of the correct key on
subsequent decryption attempts until the cache times-out.  That
can take several minutes.  (Decryption of other entries is not
affected.)  Upgrade your EasyPG version, if you can, and you can
deliberately clear your gpg-agent's cache by sending it a '-HUP'
signal.

See `allout-toggle-current-subtree-encryption' function docstring
and `allout-encrypt-unencrypted-on-saves' customization variable
for details.

                 HOT-SPOT Operation

Hot-spot operation provides a means for easy, single-keystroke outline
navigation and exposure control.

When the text cursor is positioned directly on the bullet character of
a topic, regular characters (a to z) invoke the commands of the
corresponding allout-mode keymap control chars.  For example, \"f\"
would invoke the command typically bound to \"C-c<space>C-f\"
\(\\[allout-forward-current-level] `allout-forward-current-level').

Thus, by positioning the cursor on a topic bullet, you can
execute the outline navigation and manipulation commands with a
single keystroke.  Regular navigation keys (eg, \\[forward-char], \\[next-line]) don't get
this special translation, so you can use them to get out of the
hot-spot and back to normal editing operation.

In allout-mode, the normal beginning-of-line command (\\[allout-beginning-of-line]) is
replaced with one that makes it easy to get to the hot-spot.  If you
repeat it immediately it cycles (if `allout-beginning-of-line-cycles'
is set) to the beginning of the item and then, if you hit it again
immediately, to the hot-spot.  Similarly, `allout-beginning-of-current-entry'
\(\\[allout-beginning-of-current-entry]) moves to the hot-spot when the cursor is already located
at the beginning of the current entry.

                             Extending Allout

Allout exposure and authoring activities all have associated
hooks, by which independent code can cooperate with allout
without changes to the allout core.  Here are key ones:

`allout-mode-hook'
`allout-mode-deactivate-hook' (deprecated)
`allout-mode-off-hook'
`allout-exposure-change-hook'
`allout-structure-added-hook'
`allout-structure-deleted-hook'
`allout-structure-shifted-hook'
`allout-after-copy-or-kill-hook'
`allout-post-undo-hook'

                            Terminology

Topic hierarchy constituents -- TOPICS and SUBTOPICS:

ITEM:   A unitary outline element, including the HEADER and ENTRY text.
TOPIC:  An ITEM and any ITEMs contained within it, ie having greater DEPTH
        and with no intervening items of lower DEPTH than the container.
CURRENT ITEM:
        The visible ITEM most immediately containing the cursor.
DEPTH:  The degree of nesting of an ITEM; it increases with containment.
        The DEPTH is determined by the HEADER PREFIX.  The DEPTH is also
        called the:
LEVEL:  The same as DEPTH.

ANCESTORS:
        Those ITEMs whose TOPICs contain an ITEM.
PARENT: An ITEM's immediate ANCESTOR.  It has a DEPTH one less than that
        of the ITEM.
OFFSPRING:
        The ITEMs contained within an ITEM's TOPIC.
SUBTOPIC:
        An OFFSPRING of its ANCESTOR TOPICs.
CHILD:
        An immediate SUBTOPIC of its PARENT.
SIBLINGS:
        TOPICs having the same PARENT and DEPTH.

Topic text constituents:

HEADER: The first line of an ITEM, include the ITEM PREFIX and HEADER
        text.
ENTRY:  The text content of an ITEM, before any OFFSPRING, but including
        the HEADER text and distinct from the ITEM PREFIX.
BODY:   Same as ENTRY.
PREFIX: The leading text of an ITEM which distinguishes it from normal
        ENTRY text.  Allout recognizes the outline structure according
        to the strict PREFIX format.  It consists of a PREFIX-LEAD string,
        PREFIX-PADDING, and a BULLET.  The BULLET might be followed by a
        number, indicating the ordinal number of the topic among its
        siblings, or an asterisk indicating encryption, plus an optional
        space.  After that is the ITEM HEADER text, which is not part of
        the PREFIX.

        The relative length of the PREFIX determines the nesting DEPTH
        of the ITEM.
PREFIX-LEAD:
        The string at the beginning of a HEADER PREFIX, by default a `.'.
        It can be customized by changing the setting of
        `allout-header-prefix' and then reinitializing `allout-mode'.

        When the PREFIX-LEAD is set to the comment-string of a
        programming language, outline structuring can be embedded in
        program code without interfering with processing of the text
        (by Emacs or the language processor) as program code.  This
        setting happens automatically when allout mode is used in
        programming-mode buffers.  See `allout-use-mode-specific-leader'
        docstring for more detail.
PREFIX-PADDING:
        Spaces or asterisks which separate the PREFIX-LEAD and the
        bullet, determining the ITEM's DEPTH.
BULLET: A character at the end of the ITEM PREFIX, it must be one of
        the characters listed on `allout-plain-bullets-string' or
        `allout-distinctive-bullets-string'.  When creating a TOPIC,
        plain BULLETs are by default used, according to the DEPTH of the
        TOPIC.  Choice among the distinctive BULLETs is offered when you
        provide a universal argument (\\[universal-argument]) to the
        TOPIC creation command, or when explicitly rebulleting a TOPIC.  The
        significance of the various distinctive bullets is purely by
        convention.  See the documentation for the above bullet strings for
        more details.
EXPOSURE:
        The state of a TOPIC which determines the on-screen visibility
        of its OFFSPRING and contained ENTRY text.
CONCEALED:
        TOPICs and ENTRY text whose EXPOSURE is inhibited.  Concealed
        text is represented by \"...\" ellipses.

        CONCEALED TOPICs are effectively collapsed within an ANCESTOR.
CLOSED: A TOPIC whose immediate OFFSPRING and body-text is CONCEALED.
OPEN:	A TOPIC that is not CLOSED, though its OFFSPRING or BODY may be."
;;;_    . Code
  :lighter " Allout"
  :keymap 'allout-mode-map

  (let ((use-layout (if (listp allout-layout)
                        allout-layout
                      allout-default-layout)))

    (if (not (allout-mode-p))
        (progn
          ;; Deactivation:

                                        ; Activation not explicitly
                                        ; requested, and either in
                                        ; active state or *de*activation
                                        ; specifically requested:
          (allout-do-resumptions)

          (remove-from-invisibility-spec '(allout . t))
          (remove-hook 'pre-command-hook 'allout-pre-command-business t)
          (remove-hook 'post-command-hook 'allout-post-command-business t)
          (remove-hook 'before-change-functions 'allout-before-change-handler t)
          (remove-hook 'isearch-mode-end-hook 'allout-isearch-end-handler t)
          (remove-hook 'write-contents-functions
                       'allout-write-contents-hook-handler t)

          (remove-overlays (point-min) (point-max)
                           'category 'allout-exposure-category))

      ;; Activating:
      (if allout-old-style-prefixes
          ;; Inhibit all the fancy formatting:
          (allout-add-resumptions '(allout-primary-bullet "*")))

      (allout-overlay-preparations)     ; Doesn't hurt to redo this.

      (allout-infer-header-lead-and-primary-bullet)
      (allout-infer-body-reindent)

      (set-allout-regexp)
      (allout-add-resumptions '(allout-encryption-ciphertext-rejection-regexps
                                allout-line-boundary-regexp
                                extend)
                              '(allout-encryption-ciphertext-rejection-regexps
                                allout-bob-regexp
                                extend))

      (allout-compose-and-institute-keymap)
      (produce-allout-mode-menubar-entries)

      (add-to-invisibility-spec '(allout . t))

      (allout-add-resumptions '(line-move-ignore-invisible t))
      (add-hook 'pre-command-hook 'allout-pre-command-business nil t)
      (add-hook 'post-command-hook 'allout-post-command-business nil t)
      (add-hook 'before-change-functions 'allout-before-change-handler nil t)
      (add-hook 'isearch-mode-end-hook 'allout-isearch-end-handler nil t)
      (add-hook 'write-contents-functions 'allout-write-contents-hook-handler
                nil t)

      ;; Stash auto-fill settings and adjust so custom allout auto-fill
      ;; func will be used if auto-fill is active or activated.  (The
      ;; custom func respects topic headline, maintains hanging-indents,
      ;; etc.)
      (allout-add-resumptions (list 'allout-former-auto-filler
                                    auto-fill-function)
                              ;; Register allout-auto-fill to be used if
                              ;; filling is active:
                              (list 'allout-outside-normal-auto-fill-function
                                    normal-auto-fill-function)
                              '(normal-auto-fill-function allout-auto-fill)
                              ;; Paragraphs are broken by topic headlines.
                              (list 'paragraph-start
                                    (concat paragraph-start "\\|^\\("
                                            allout-regexp "\\)"))
                              (list 'paragraph-separate
                                    (concat paragraph-separate "\\|^\\("
                                            allout-regexp "\\)")))
      (if (and auto-fill-function (not allout-inhibit-auto-fill))
          ;; allout-auto-fill will use the stashed values and so forth.
          (allout-add-resumptions '(auto-fill-function allout-auto-fill)))

      (allout-setup-menubar)

      ;; Do auto layout if warranted:
      (when (and allout-layout
                 allout-auto-activation
                 use-layout
                 (and (not (string= allout-auto-activation "activate"))
                      (if (string= allout-auto-activation "ask")
                          (if (y-or-n-p (format "Expose %s with layout '%s'? "
                                                (buffer-name)
                                                use-layout))
                              t
                            (message "Skipped %s layout." (buffer-name))
                            nil)
                        t)))
        (save-excursion
          (message "Adjusting '%s' exposure..." (buffer-name))
          (goto-char 0)
          (allout-this-or-next-heading)
          (condition-case err
              (progn
                (apply 'allout-expose-topic (list use-layout))
                (message "Adjusting '%s' exposure... done."
                         (buffer-name)))
            ;; Problem applying exposure -- notify user, but don't
            ;; interrupt, eg, file visit:
            (error (message "%s" (car (cdr err)))
                   (sit-for 1))))
        )                               ; when allout-layout
      )					; if (allout-mode-p)
    )                                   ; let (())
  )  					; define-minor-mode
;;;_  > allout-minor-mode alias
(defalias 'allout-minor-mode 'allout-mode)
;;;_  > allout-unload-function
(defun allout-unload-function ()
  "Unload the allout outline library."
  (save-current-buffer
    (dolist (buffer (buffer-list))
      (set-buffer buffer)
      (when (allout-mode-p) (allout-mode -1))))
  ;; continue standard unloading
  nil)

;;;_  - Position Assessment
;;;_   > allout-hidden-p (&optional pos)
(defsubst allout-hidden-p (&optional pos)
  "Non-nil if the character after point was made invisible by allout."
  (eq (get-char-property (or pos (point)) 'invisible) 'allout))

;;;_  > allout-overlay-insert-in-front-handler (ol after beg end
;;;                                                &optional prelen)
(defun allout-overlay-insert-in-front-handler (ol after beg end
                                                  &optional prelen)
  "Shift the overlay so stuff inserted in front of it is excluded."
  (if after
      ;; ??? Shouldn't moving the overlay should be unnecessary, if overlay
      ;;     front-advance on the overlay worked as expected?
      (move-overlay ol (1+ beg) (overlay-end ol))))
;;;_  > allout-overlay-interior-modification-handler (ol after beg end
;;;                                                      &optional prelen)
(defun allout-overlay-interior-modification-handler (ol after beg end
                                                        &optional prelen)
  "Get confirmation before making arbitrary changes to invisible text.

We expose the invisible text and ask for confirmation.  Refusal or
`keyboard-quit' abandons the changes, with keyboard-quit additionally
reclosing the opened text.

No confirmation is necessary when `inhibit-read-only' is set -- eg, allout
internal functions use this feature cohesively bunch changes."

  (when (and (not inhibit-read-only) (not after))
    (let ((start (point))
          (ol-start (overlay-start ol))
          (ol-end (overlay-end ol))
          first)
      (goto-char beg)
      (while (< (point) end)
        (when (allout-hidden-p)
          (allout-show-to-offshoot)
          (if (allout-hidden-p)
              (save-excursion (forward-char 1)
                              (allout-show-to-offshoot)))
          (when (not first)
            (setq first (point))))
        (goto-char (if (featurep 'xemacs)
                       (next-property-change (1+ (point)) nil end)
                     (next-char-property-change (1+ (point)) end))))
      (when first
        (goto-char first)
        (condition-case nil
            (if (not
                 (yes-or-no-p
                  (substitute-command-keys
                   (concat "Modify concealed text?  (\"no\" just aborts,"
                           " \\[keyboard-quit] also reconceals) "))))
                (progn (goto-char start)
                       (error "Concealed-text change refused")))
          (quit (allout-flag-region ol-start ol-end nil)
                (allout-flag-region ol-start ol-end t)
                (error "Concealed-text change abandoned, text reconcealed"))))
      (goto-char start))))
;;;_  > allout-before-change-handler (beg end)
(defun allout-before-change-handler (beg end)
  "Protect against changes to invisible text.

See `allout-overlay-interior-modification-handler' for details."

  (when (and (allout-mode-p) undo-in-progress)
    (setq allout-just-did-undo t)
    (if (allout-hidden-p)
        (allout-show-children)))

  ;; allout-overlay-interior-modification-handler on an overlay handles
  ;; this in other emacs, via `allout-exposure-category's 'modification-hooks.
  (when (and (featurep 'xemacs) (allout-mode-p))
    ;; process all of the pending overlays:
    (save-excursion
      (goto-char beg)
      (let ((overlay (allout-get-invisibility-overlay)))
        (if overlay
            (allout-overlay-interior-modification-handler
             overlay nil beg end nil))))))
;;;_  > allout-isearch-end-handler (&optional overlay)
(defun allout-isearch-end-handler (&optional overlay)
  "Reconcile allout outline exposure on arriving in hidden text after isearch.

Optional OVERLAY parameter is for when this function is used by
`isearch-open-invisible' overlay property.  It is otherwise unused, so this
function can also be used as an `isearch-mode-end-hook'."

  (if (and (allout-mode-p) (allout-hidden-p))
      (allout-show-to-offshoot)))

;;;_ #3 Internal Position State-Tracking -- "allout-recent-*" funcs
;; All the basic outline functions that directly do string matches to
;; evaluate heading prefix location set the variables
;; `allout-recent-prefix-beginning'  and `allout-recent-prefix-end'
;; when successful.  Functions starting with `allout-recent-' all
;; use this state, providing the means to avoid redundant searches
;; for just-established data.  This optimization can provide
;; significant speed improvement, but it must be employed carefully.
;;;_  = allout-recent-prefix-beginning
(defvar allout-recent-prefix-beginning 0
  "Buffer point of the start of the last topic prefix encountered.")
(make-variable-buffer-local 'allout-recent-prefix-beginning)
;;;_  = allout-recent-prefix-end
(defvar allout-recent-prefix-end 0
  "Buffer point of the end of the last topic prefix encountered.")
(make-variable-buffer-local 'allout-recent-prefix-end)
;;;_  = allout-recent-depth
(defvar allout-recent-depth 0
  "Depth of the last topic prefix encountered.")
(make-variable-buffer-local 'allout-recent-depth)
;;;_  = allout-recent-end-of-subtree
(defvar allout-recent-end-of-subtree 0
  "Buffer point last returned by `allout-end-of-current-subtree'.")
(make-variable-buffer-local 'allout-recent-end-of-subtree)
;;;_  > allout-prefix-data ()
(defsubst allout-prefix-data ()
  "Register allout-prefix state data.

For reference by `allout-recent' funcs.  Return
the new value of `allout-recent-prefix-beginning'."
  (setq allout-recent-prefix-end (or (match-end 1) (match-end 2) (match-end 3))
        allout-recent-prefix-beginning (or (match-beginning 1)
                                           (match-beginning 2)
                                           (match-beginning 3))
        allout-recent-depth (max 1 (- allout-recent-prefix-end
                                      allout-recent-prefix-beginning
                                      allout-header-subtraction)))
  allout-recent-prefix-beginning)
;;;_  > nullify-allout-prefix-data ()
(defsubst nullify-allout-prefix-data ()
  "Mark allout prefix data as being uninformative."
  (setq allout-recent-prefix-end (point)
        allout-recent-prefix-beginning (point)
        allout-recent-depth 0)
  allout-recent-prefix-beginning)
;;;_  > allout-recent-depth ()
(defsubst allout-recent-depth ()
  "Return depth of last heading encountered by an outline maneuvering function.

All outline functions which directly do string matches to assess
headings set the variables `allout-recent-prefix-beginning' and
`allout-recent-prefix-end' if successful.  This function uses those settings
to return the current depth."

  allout-recent-depth)
;;;_  > allout-recent-prefix ()
(defsubst allout-recent-prefix ()
  "Like `allout-recent-depth', but returns text of last encountered prefix.

All outline functions which directly do string matches to assess
headings set the variables `allout-recent-prefix-beginning' and
`allout-recent-prefix-end' if successful.  This function uses those settings
to return the current prefix."
  (buffer-substring-no-properties allout-recent-prefix-beginning
                                  allout-recent-prefix-end))
;;;_  > allout-recent-bullet ()
(defmacro allout-recent-bullet ()
  "Like `allout-recent-prefix', but returns bullet of last encountered prefix.

All outline functions which directly do string matches to assess
headings set the variables `allout-recent-prefix-beginning' and
`allout-recent-prefix-end' if successful.  This function uses those settings
to return the current depth of the most recently matched topic."
  '(buffer-substring-no-properties (1- allout-recent-prefix-end)
                                   allout-recent-prefix-end))

;;;_ #4 Navigation

;;;_  - Position Assessment
;;;_   : Location Predicates
;;;_    > allout-do-doublecheck ()
(defsubst allout-do-doublecheck ()
  "True if current item conditions qualify for checking on topic aberrance."
  (and
   ;; presume integrity of outline and yanked content during yank -- necessary
   ;; to allow for level disparity of yank location and yanked text:
   (not allout-inhibit-aberrance-doublecheck)
   ;; allout-doublecheck-at-and-shallower is ceiling for doublecheck:
   (<= allout-recent-depth allout-doublecheck-at-and-shallower)))
;;;_     > allout-aberrant-container-p ()
(defun allout-aberrant-container-p ()
  "True if topic, or next sibling with children, contains them discontinuously.

Discontinuous means an immediate offspring that is nested more
than one level deeper than the topic.

If topic has no offspring, then the next sibling with offspring will
determine whether or not this one is determined to be aberrant.

If true, then the allout-recent-* settings are calibrated on the
offspring that qualifies it as aberrant, ie with depth that
exceeds the topic by more than one."

  ;; This is most clearly understood when considering standard-prefix-leader
  ;; low-level topics, which can all too easily match text not intended as
  ;; headers.  For example, any line with a leading '.' or '*' and lacking a
  ;; following bullet qualifies without this protection.  (A sequence of
  ;; them can occur naturally, eg a typical textual bullet list.)  We
  ;; disqualify such low-level sequences when they are followed by a
  ;; discontinuously contained child, inferring that the sequences are not
  ;; actually connected with their prospective context.

  (let ((depth (allout-depth))
        (start-point (point))
        done aberrant)
    (save-match-data
      (save-excursion
        (while (and (not done)
                    (re-search-forward allout-line-boundary-regexp nil 0))
          (allout-prefix-data)
          (goto-char allout-recent-prefix-beginning)
          (cond
           ;; sibling -- continue:
           ((eq allout-recent-depth depth))
           ;; first offspring is excessive -- aberrant:
           ((> allout-recent-depth (1+ depth))
            (setq done t aberrant t))
           ;; next non-sibling is lower-depth -- not aberrant:
           (t (setq done t))))))
    (if aberrant
        aberrant
      (goto-char start-point)
      ;; recalibrate allout-recent-*
      (allout-depth)
      nil)))
;;;_    > allout-on-current-heading-p ()
(defun allout-on-current-heading-p ()
  "Return non-nil if point is on current visible topics' header line.

Actually, returns prefix beginning point."
  (save-excursion
    (allout-beginning-of-current-line)
    (save-match-data
      (and (looking-at allout-regexp)
           (allout-prefix-data)
           (or (not (allout-do-doublecheck))
               (not (allout-aberrant-container-p)))))))
;;;_    > allout-on-heading-p ()
(defalias 'allout-on-heading-p 'allout-on-current-heading-p)
;;;_    > allout-e-o-prefix-p ()
(defun allout-e-o-prefix-p ()
  "True if point is located where current topic prefix ends, heading begins."
  (and (save-match-data
        (save-excursion (let ((inhibit-field-text-motion t))
                          (beginning-of-line))
                        (looking-at allout-regexp))
       (= (point) (save-excursion (allout-end-of-prefix)(point))))))
;;;_   : Location attributes
;;;_    > allout-depth ()
(defun allout-depth ()
  "Return depth of topic most immediately containing point.

Does not do doublecheck for aberrant topic header.

Return zero if point is not within any topic.

Like `allout-current-depth', but respects hidden as well as visible topics."
  (save-excursion
    (let ((start-point (point)))
      (if (and (allout-goto-prefix)
               (not (< start-point (point))))
          allout-recent-depth
        (progn
          ;; Oops, no prefix, nullify it:
          (nullify-allout-prefix-data)
          ;; ... and return 0:
          0)))))
;;;_    > allout-current-depth ()
(defun allout-current-depth ()
  "Return depth of visible topic most immediately containing point.

Return zero if point is not within any topic."
  (save-excursion
    (if (allout-back-to-current-heading)
        (max 1
             (- allout-recent-prefix-end
                allout-recent-prefix-beginning
                allout-header-subtraction))
      0)))
;;;_    > allout-get-current-prefix ()
(defun allout-get-current-prefix ()
  "Topic prefix of the current topic."
  (save-excursion
    (if (allout-goto-prefix)
	(allout-recent-prefix))))
;;;_    > allout-get-bullet ()
(defun allout-get-bullet ()
  "Return bullet of containing topic (visible or not)."
  (save-excursion
    (and (allout-goto-prefix)
	 (allout-recent-bullet))))
;;;_    > allout-current-bullet ()
(defun allout-current-bullet ()
  "Return bullet of current (visible) topic heading, or none if none found."
  (condition-case nil
      (save-excursion
	(allout-back-to-current-heading)
	(buffer-substring-no-properties (- allout-recent-prefix-end 1)
                                        allout-recent-prefix-end))
    ;; Quick and dirty provision, ostensibly for missing bullet:
    (args-out-of-range nil))
  )
;;;_    > allout-get-prefix-bullet (prefix)
(defun allout-get-prefix-bullet (prefix)
  "Return the bullet of the header prefix string PREFIX."
  ;; Doesn't make sense if we're old-style prefixes, but this just
  ;; oughtn't be called then, so forget about it...
  (if (string-match allout-regexp prefix)
      (substring prefix (1- (match-end 2)) (match-end 2))))
;;;_    > allout-sibling-index (&optional depth)
(defun allout-sibling-index (&optional depth)
  "Item number of this prospective topic among its siblings.

If optional arg DEPTH is greater than current depth, then we're
opening a new level, and return 0.

If less than this depth, ascend to that depth and count..."

  (save-excursion
    (cond ((and depth (<= depth 0) 0))
          ((or (null depth) (= depth (allout-depth)))
           (let ((index 1))
             (while (allout-previous-sibling allout-recent-depth nil)
	       (setq index (1+ index)))
             index))
          ((< depth allout-recent-depth)
           (allout-ascend-to-depth depth)
           (allout-sibling-index))
          (0))))
;;;_    > allout-topic-flat-index ()
(defun allout-topic-flat-index ()
  "Return a list indicating point's numeric section.subsect.subsubsect...
Outermost is first."
  (let* ((depth (allout-depth))
	 (next-index (allout-sibling-index depth))
	 (rev-sibls nil))
    (while (> next-index 0)
      (setq rev-sibls (cons next-index rev-sibls))
      (setq depth (1- depth))
      (setq next-index (allout-sibling-index depth)))
    rev-sibls)
  )

;;;_  - Navigation routines
;;;_   > allout-beginning-of-current-line ()
(defun allout-beginning-of-current-line ()
  "Like beginning of line, but to visible text."

  ;; This combination of move-beginning-of-line and beginning-of-line is
  ;; deliberate, but the (beginning-of-line) may now be superfluous.
  (let ((inhibit-field-text-motion t))
    (move-beginning-of-line 1)
    (beginning-of-line)
    (while (and (not (bobp)) (or (not (bolp)) (allout-hidden-p)))
      (beginning-of-line)
      (if (or (allout-hidden-p) (not (bolp)))
          (forward-char -1)))))
;;;_   > allout-end-of-current-line ()
(defun allout-end-of-current-line ()
  "Move to the end of line, past concealed text if any."
  ;; This is for symmetry with `allout-beginning-of-current-line' --
  ;; `move-end-of-line' doesn't suffer the same problem as
  ;; `move-beginning-of-line'.
  (let ((inhibit-field-text-motion t))
    (end-of-line)
    (while (allout-hidden-p)
      (end-of-line)
      (if (allout-hidden-p) (forward-char 1)))))
;;;_   > allout-beginning-of-line ()
(defun allout-beginning-of-line ()
  "Beginning-of-line with `allout-beginning-of-line-cycles' behavior, if set."

  (interactive)

  (if (or (not allout-beginning-of-line-cycles)
          (not (equal last-command this-command)))
      (progn
        (if (and (not (bolp))
                 (allout-hidden-p (1- (point))))
            (goto-char (allout-previous-single-char-property-change
                        (1- (point)) 'invisible)))
        (move-beginning-of-line 1))
    (allout-depth)
    (let ((beginning-of-body
           (save-excursion
             (while (and (allout-do-doublecheck)
                         (allout-aberrant-container-p)
                         (allout-previous-visible-heading 1)))
             (allout-beginning-of-current-entry)
             (point))))
      (cond ((= (current-column) 0)
             (goto-char beginning-of-body))
            ((< (point) beginning-of-body)
             (allout-beginning-of-current-line))
            ((= (point) beginning-of-body)
             (goto-char (allout-current-bullet-pos)))
            (t (allout-beginning-of-current-line)
               (if (< (point) beginning-of-body)
                   ;; we were on the headline after its start:
                   (goto-char beginning-of-body)))))))
;;;_   > allout-end-of-line ()
(defun allout-end-of-line ()
  "End-of-line with `allout-end-of-line-cycles' behavior, if set."

  (interactive)

  (if (or (not allout-end-of-line-cycles)
          (not (equal last-command this-command)))
      (allout-end-of-current-line)
    (let ((end-of-entry (save-excursion
                          (allout-end-of-entry)
                          (point))))
      (cond ((not (eolp))
             (allout-end-of-current-line))
            ((or (allout-hidden-p) (save-excursion
                                     (forward-char -1)
                                     (allout-hidden-p)))
             (allout-back-to-current-heading)
             (allout-show-current-entry)
             (allout-show-children)
             (allout-end-of-entry))
            ((>= (point) end-of-entry)
             (allout-back-to-current-heading)
             (allout-end-of-current-line))
            (t
             (if (not (allout-mark-active-p))
                 (push-mark))
             (allout-end-of-entry))))))
;;;_   > allout-mark-active-p ()
(defun allout-mark-active-p ()
  "True if the mark is currently or always active."
  ;; `(cond (boundp...))' (or `(if ...)') invokes special byte-compiler
  ;; provisions, at least in GNU Emacs to prevent warnings about lack of,
  ;; eg, region-active-p.
  (cond ((boundp 'mark-active)
         mark-active)
        ((fboundp 'region-active-p)
         (region-active-p))
        (t)))
;;;_   > allout-next-heading ()
(defsubst allout-next-heading ()
  "Move to the heading for the topic (possibly invisible) after this one.

Returns the location of the heading, or nil if none found.

We skip anomalous low-level topics, a la `allout-aberrant-container-p'."
  (save-match-data

    (if (looking-at allout-regexp)
        (forward-char 1))

    (when (re-search-forward allout-line-boundary-regexp nil 0)
      (allout-prefix-data)
      (goto-char allout-recent-prefix-beginning)
      (while (not (bolp))
        (forward-char -1))
      (and (allout-do-doublecheck)
           ;; this will set allout-recent-* on the first non-aberrant topic,
           ;; whether it's the current one or one that disqualifies it:
           (allout-aberrant-container-p))
      ;; this may or may not be the same as above depending on doublecheck:
      (goto-char allout-recent-prefix-beginning))))
;;;_   > allout-this-or-next-heading
(defun allout-this-or-next-heading ()
  "Position cursor on current or next heading."
  ;; A throwaway non-macro that is defined after allout-next-heading
  ;; and usable by allout-mode.
  (if (not (allout-goto-prefix-doublechecked)) (allout-next-heading)))
;;;_   > allout-previous-heading ()
(defun allout-previous-heading ()
  "Move to the prior (possibly invisible) heading line.

Return the location of the beginning of the heading, or nil if not found.

We skip anomalous low-level topics, a la `allout-aberrant-container-p'."

  (if (bobp)
      nil
    (let ((start-point (point)))
      ;; allout-goto-prefix-doublechecked calls us, so we can't use it here.
      (allout-goto-prefix)
      (save-match-data
        (when (or (re-search-backward allout-line-boundary-regexp nil 0)
                  (looking-at allout-bob-regexp))
          (goto-char (allout-prefix-data))
          (if (and (allout-do-doublecheck)
                   (allout-aberrant-container-p))
              (or (allout-previous-heading)
                  (and (goto-char start-point)
                       ;; recalibrate allout-recent-*:
                       (allout-depth)
                       nil))
            (point)))))))
;;;_   > allout-get-invisibility-overlay ()
(defun allout-get-invisibility-overlay ()
  "Return the overlay at point that dictates allout invisibility."
  (let ((overlays (overlays-at (point)))
        got)
    (while (and overlays (not got))
      (if (equal (overlay-get (car overlays) 'invisible) 'allout)
          (setq got (car overlays))
        (pop overlays)))
    got))
;;;_   > allout-back-to-visible-text ()
(defun allout-back-to-visible-text ()
  "Move to most recent prior character that is visible, and return point."
    (if (allout-hidden-p)
      (goto-char (overlay-start (allout-get-invisibility-overlay))))
    (point))

;;;_  - Subtree Charting
;;;_   " These routines either produce or assess charts, which are
;;; nested lists of the locations of topics within a subtree.
;;;
;;; Charts enable efficient subtree navigation by providing a reusable basis
;;; for elaborate, compound assessment and adjustment of a subtree.

;;;_   > allout-chart-subtree (&optional levels visible orig-depth prev-depth)
(defun allout-chart-subtree (&optional levels visible orig-depth prev-depth)
  "Produce a location \"chart\" of subtopics of the containing topic.

Optional argument LEVELS specifies a depth limit (relative to start
depth) for the chart.  Null LEVELS means no limit.

When optional argument VISIBLE is non-nil, the chart includes
only the visible subelements of the charted subjects.

The remaining optional args are for internal use by the function.

Point is left at the end of the subtree.

Charts are used to capture outline structure, so that outline-altering
routines need to assess the structure only once, and then use the chart
for their elaborate manipulations.

The chart entries for the topics are in reverse order, so the
last topic is listed first.  The entry for each topic consists of
an integer indicating the point at the beginning of the topic
prefix.  Charts for offspring consist of a list containing,
recursively, the charts for the respective subtopics.  The chart
for a topics' offspring precedes the entry for the topic itself.

The other function parameters are for internal recursion, and should
not be specified by external callers.  ORIG-DEPTH is depth of topic at
starting point, and PREV-DEPTH is depth of prior topic."

  (let ((original (not orig-depth))	; `orig-depth' set only in recursion.
	chart curr-depth)

    (if original			; Just starting?
					; Register initial settings and
					; position to first offspring:
	(progn (setq orig-depth (allout-depth))
	       (or prev-depth (setq prev-depth (1+ orig-depth)))
               (if visible
                   (allout-next-visible-heading 1)
                 (allout-next-heading))))

    ;; Loop over the current levels' siblings.  Besides being more
    ;; efficient than tail-recursing over a level, it avoids exceeding
    ;; the typically quite constrained Emacs max-lisp-eval-depth.
    ;;
    ;; Probably would speed things up to implement loop-based stack
    ;; operation rather than recursing for lower levels.  Bah.

    (while (and (not (eobp))
					; Still within original topic?
		(< orig-depth (setq curr-depth allout-recent-depth))
		(cond ((= prev-depth curr-depth)
		       ;; Register this one and move on:
		       (setq chart (cons allout-recent-prefix-beginning chart))
		       (if (and levels (<= levels 1))
			   ;; At depth limit -- skip sublevels:
			   (or (allout-next-sibling curr-depth)
			       ;; or no more siblings -- proceed to
			       ;; next heading at lesser depth:
			       (while (and (<= curr-depth
					       allout-recent-depth)
                                           (if visible
                                               (allout-next-visible-heading 1)
                                             (allout-next-heading)))))
                         (if visible
                             (allout-next-visible-heading 1)
                           (allout-next-heading))))

		      ((and (< prev-depth curr-depth)
			    (or (not levels)
				(> levels 0)))
		       ;; Recurse on deeper level of curr topic:
		       (setq chart
			     (cons (allout-chart-subtree (and levels
							       (1- levels))
                                                         visible
                                                         orig-depth
                                                         curr-depth)
				   chart))
		       ;; ... then continue with this one.
		       )

		      ;; ... else nil if we've ascended back to prev-depth.

		      )))

    (if original			; We're at the last sibling on
					; the original level.  Position
					; to the end of it:
	(progn (and (not (eobp)) (forward-char -1))
	       (and (= (preceding-char) ?\n)
		    (= (aref (buffer-substring (max 1 (- (point) 3))
                                               (point))
                             1)
                       ?\n)
		    (forward-char -1))
	       (setq allout-recent-end-of-subtree (point))))

    chart				; (nreverse chart) not necessary,
					; and maybe not preferable.
    ))
;;;_   > allout-chart-siblings (&optional start end)
(defun allout-chart-siblings (&optional start end)
  "Produce a list of locations of this and succeeding sibling topics.
Effectively a top-level chart of siblings.  See `allout-chart-subtree'
for an explanation of charts."
  (save-excursion
    (when (allout-goto-prefix-doublechecked)
      (let ((chart (list (point))))
        (while (allout-next-sibling)
          (setq chart (cons (point) chart)))
        (if chart (setq chart (nreverse chart)))))))
;;;_   > allout-chart-to-reveal (chart depth)
(defun allout-chart-to-reveal (chart depth)

  "Return a flat list of hidden points in subtree CHART, up to DEPTH.

If DEPTH is nil, include hidden points at any depth.

Note that point can be left at any of the points on chart, or at the
start point."

  (let (result here)
    (while (and (or (null depth) (> depth 0))
		chart)
      (setq here (car chart))
      (if (listp here)
	  (let ((further (allout-chart-to-reveal here (if (null depth)
                                                          depth
                                                        (1- depth)))))
	    ;; We're on the start of a subtree -- recurse with it, if there's
	    ;; more depth to go:
	    (if further (setq result (append further result)))
	    (setq chart (cdr chart)))
	(goto-char here)
        (if (allout-hidden-p)
	    (setq result (cons here result)))
	(setq chart (cdr chart))))
    result))
;;;_   X allout-chart-spec (chart spec &optional exposing)
;; (defun allout-chart-spec (chart spec &optional exposing)
;;   "Not yet (if ever) implemented.

;; Produce exposure directives given topic/subtree CHART and an exposure SPEC.

;; Exposure spec indicates the locations to be exposed and the prescribed
;; exposure status.  Optional arg EXPOSING is an integer, with 0
;; indicating pending concealment, anything higher indicating depth to
;; which subtopic headers should be exposed, and negative numbers
;; indicating (negative of) the depth to which subtopic headers and
;; bodies should be exposed.

;; The produced list can have two types of entries.  Bare numbers
;; indicate points in the buffer where topic headers that should be
;; exposed reside.

;;  - bare negative numbers indicates that the topic starting at the
;;    point which is the negative of the number should be opened,
;;    including their entries.
;;  - bare positive values indicate that this topic header should be
;;    opened.
;;  - Lists signify the beginning and end points of regions that should
;;    be flagged, and the flag to employ.  (For concealment: `(\?r)', and
;;    exposure:"
;;   (while spec
;;     (cond ((listp spec)
;; 	   )
;; 	  )
;;     (setq spec (cdr spec)))
;;   )

;;;_  - Within Topic
;;;_   > allout-goto-prefix ()
(defun allout-goto-prefix ()
  "Put point at beginning of immediately containing outline topic.

Goes to most immediate subsequent topic if none immediately containing.

Not sensitive to topic visibility.

Returns the point at the beginning of the prefix, or nil if none."

  (save-match-data
    (let (done)
      (while (and (not done)
                  (search-backward "\n" nil 1))
        (forward-char 1)
        (if (looking-at allout-regexp)
            (setq done (allout-prefix-data))
          (forward-char -1)))
      (if (bobp)
          (cond ((looking-at allout-regexp)
                 (allout-prefix-data))
                ((allout-next-heading))
                (done))
        done))))
;;;_   > allout-goto-prefix-doublechecked ()
(defun allout-goto-prefix-doublechecked ()
  "Put point at beginning of immediately containing outline topic.

Like `allout-goto-prefix', but shallow topics (according to
`allout-doublecheck-at-and-shallower') are checked and
disqualified for child containment discontinuity, according to
`allout-aberrant-container-p'."
  (if (allout-goto-prefix)
      (if (and (allout-do-doublecheck)
               (allout-aberrant-container-p))
          (allout-previous-heading)
        (point))))

;;;_   > allout-end-of-prefix ()
(defun allout-end-of-prefix (&optional ignore-decorations)
  "Position cursor at beginning of header text.

If optional IGNORE-DECORATIONS is non-nil, put just after bullet,
otherwise skip white space between bullet and ensuing text."

  (if (not (allout-goto-prefix-doublechecked))
      nil
    (goto-char allout-recent-prefix-end)
    (save-match-data
      (if ignore-decorations
          t
        (while (looking-at "[0-9]") (forward-char 1))
        (if (and (not (eolp)) (looking-at "\\s-")) (forward-char 1))))
    ;; Reestablish where we are:
    (allout-current-depth)))
;;;_   > allout-current-bullet-pos ()
(defun allout-current-bullet-pos ()
  "Return position of current (visible) topic's bullet."

  (if (not (allout-current-depth))
      nil
    (1- allout-recent-prefix-end)))
;;;_   > allout-back-to-current-heading (&optional interactive)
(defun allout-back-to-current-heading (&optional interactive)
  "Move to heading line of current topic, or beginning if not in a topic.

If interactive, we position at the end of the prefix.

Return value of resulting point, unless we started outside
of (before any) topics, in which case we return nil."

  (interactive "p")

  (allout-beginning-of-current-line)
  (let ((bol-point (point)))
    (when (allout-goto-prefix-doublechecked)
      (if (<= (point) bol-point)
          (progn
            (setq bol-point (point))
            (allout-beginning-of-current-line)
            (if (not (= bol-point (point)))
                (if (looking-at allout-regexp)
                    (allout-prefix-data)))
            (if interactive
                (allout-end-of-prefix)
              (point)))
        (goto-char (point-min))
        nil))))
;;;_   > allout-back-to-heading ()
(defalias 'allout-back-to-heading 'allout-back-to-current-heading)
;;;_   > allout-pre-next-prefix ()
(defun allout-pre-next-prefix ()
  "Skip forward to just before the next heading line.

Returns that character position."

  (if (allout-next-heading)
      (goto-char (1- allout-recent-prefix-beginning))))
;;;_   > allout-end-of-subtree (&optional current include-trailing-blank)
(defun allout-end-of-subtree (&optional current include-trailing-blank)
  "Put point at the end of the last leaf in the containing topic.

Optional CURRENT means put point at the end of the containing
visible topic.

Optional INCLUDE-TRAILING-BLANK means include a trailing blank line, if
any, as part of the subtree.  Otherwise, that trailing blank will be
excluded as delimiting whitespace between topics.

Returns the value of point."
  (interactive "P")
  (if current
      (allout-back-to-current-heading)
    (allout-goto-prefix-doublechecked))
  (let ((level allout-recent-depth))
    (allout-next-heading)
    (while (and (not (eobp))
                (> allout-recent-depth level))
      (allout-next-heading))
    (if (eobp)
        (allout-end-of-entry)
      (forward-char -1))
    (if (and (not include-trailing-blank) (= ?\n (preceding-char)))
         (forward-char -1))
    (setq allout-recent-end-of-subtree (point))))
;;;_   > allout-end-of-current-subtree (&optional include-trailing-blank)
(defun allout-end-of-current-subtree (&optional include-trailing-blank)

  "Put point at end of last leaf in currently visible containing topic.

Optional INCLUDE-TRAILING-BLANK means include a trailing blank line, if
any, as part of the subtree.  Otherwise, that trailing blank will be
excluded as delimiting whitespace between topics.

Returns the value of point."
  (interactive)
  (allout-end-of-subtree t include-trailing-blank))
;;;_   > allout-beginning-of-current-entry (&optional interactive)
(defun allout-beginning-of-current-entry (&optional interactive)
  "When not already there, position point at beginning of current topic header.

If already there, move cursor to bullet for hot-spot operation.
\(See `allout-mode' doc string for details of hot-spot operation.)"
  (interactive "p")
  (let ((start-point (point)))
    (move-beginning-of-line 1)
    (if (< 0 (allout-current-depth))
        (goto-char allout-recent-prefix-end)
      (goto-char (point-min)))
    (allout-end-of-prefix)
    (if (and interactive
	     (= (point) start-point))
	(goto-char (allout-current-bullet-pos)))))
;;;_   > allout-end-of-entry (&optional inclusive)
(defun allout-end-of-entry (&optional inclusive)
  "Position the point at the end of the current topics' entry.

Optional INCLUSIVE means also include trailing empty line, if any.  When
unset, whitespace between items separates them even when the items are
collapsed."
  (interactive)
  (allout-pre-next-prefix)
  (if (and (not inclusive) (not (bobp)) (= ?\n (preceding-char)))
      (forward-char -1))
  (point))
;;;_   > allout-end-of-current-heading ()
(defun allout-end-of-current-heading ()
  (interactive)
  (allout-beginning-of-current-entry)
  (search-forward "\n" nil t)
  (forward-char -1))
(defalias 'allout-end-of-heading 'allout-end-of-current-heading)
;;;_   > allout-get-body-text ()
(defun allout-get-body-text ()
  "Return the unmangled body text of the topic immediately containing point."
  (save-excursion
    (allout-end-of-prefix)
    (if (not (search-forward "\n" nil t))
        nil
      (backward-char 1)
      (let ((pre-body (point)))
        (if (not pre-body)
            nil
          (allout-end-of-entry t)
          (if (not (= pre-body (point)))
              (buffer-substring-no-properties (1+ pre-body) (point))))
        )
      )
    )
  )

;;;_  - Depth-wise
;;;_   > allout-ascend-to-depth (depth)
(defun allout-ascend-to-depth (depth)
  "Ascend to depth DEPTH, returning depth if successful, nil if not."
  (if (and (> depth 0)(<= depth (allout-depth)))
      (let (last-ascended)
        (while (and (< depth allout-recent-depth)
                    (setq last-ascended (allout-ascend))))
        (goto-char allout-recent-prefix-beginning)
        (if (allout-called-interactively-p) (allout-end-of-prefix))
        (and last-ascended allout-recent-depth))))
;;;_   > allout-ascend (&optional dont-move-if-unsuccessful)
(defun allout-ascend (&optional dont-move-if-unsuccessful)
  "Ascend one level, returning resulting depth if successful, nil if not.

Point is left at the beginning of the level whether or not
successful, unless optional DONT-MOVE-IF-UNSUCCESSFUL is set, in
which case point is returned to its original starting location."
  (if dont-move-if-unsuccessful
      (setq dont-move-if-unsuccessful (point)))
  (prog1
      (if (allout-beginning-of-level)
          (let ((bolevel (point))
                (bolevel-depth allout-recent-depth))
            (allout-previous-heading)
            (cond ((< allout-recent-depth bolevel-depth)
                   allout-recent-depth)
                  ((= allout-recent-depth bolevel-depth)
                   (if dont-move-if-unsuccessful
                       (goto-char dont-move-if-unsuccessful))
                   (allout-depth)
                   nil)
                  (t
                   ;; some topic after very first is lower depth than first:
                   (goto-char bolevel)
                   (allout-depth)
                   nil))))
    (if (allout-called-interactively-p) (allout-end-of-prefix))))
;;;_   > allout-descend-to-depth (depth)
(defun allout-descend-to-depth (depth)
  "Descend to depth DEPTH within current topic.

Returning depth if successful, nil if not."
  (let ((start-point (point))
        (start-depth (allout-depth)))
    (while
        (and (> (allout-depth) 0)
             (not (= depth allout-recent-depth)) ; ... not there yet
             (allout-next-heading)     ; ... go further
             (< start-depth allout-recent-depth))) ; ... still in topic
    (if (and (> (allout-depth) 0)
             (= allout-recent-depth depth))
        depth
      (goto-char start-point)
      nil))
  )
;;;_   > allout-up-current-level (arg)
(defun allout-up-current-level (arg)
  "Move out ARG levels from current visible topic."
  (interactive "p")
  (let ((start-point (point)))
    (allout-back-to-current-heading)
    (if (not (allout-ascend))
        (progn (goto-char start-point)
               (error "Can't ascend past outermost level"))
      (if (allout-called-interactively-p) (allout-end-of-prefix))
      allout-recent-prefix-beginning)))

;;;_  - Linear
;;;_   > allout-next-sibling (&optional depth backward)
(defun allout-next-sibling (&optional depth backward)
  "Like `allout-forward-current-level', but respects invisible topics.

Traverse at optional DEPTH, or current depth if none specified.

Go backward if optional arg BACKWARD is non-nil.

Return the start point of the new topic if successful, nil otherwise."

  (if (if backward (bobp) (eobp))
      nil
    (let ((target-depth (or depth (allout-depth)))
          (start-point (point))
          (start-prefix-beginning allout-recent-prefix-beginning)
          (count 0)
          leaping
	  last-depth)
      (while (and
              ;; done too few single steps to resort to the leap routine:
              (not leaping)
              ;; not at limit:
              (not (if backward (bobp) (eobp)))
              ;; still traversable:
              (if backward (allout-previous-heading) (allout-next-heading))
              ;; we're below the target depth
              (> (setq last-depth allout-recent-depth) target-depth))
        (setq count (1+ count))
        (if (> count 7)                 ; lists are commonly 7 +- 2, right?-)
            (setq leaping t)))
      (cond (leaping
             (or (allout-next-sibling-leap target-depth backward)
                 (progn
                   (goto-char start-point)
                   (if depth (allout-depth) target-depth)
                   nil)))
            ((and (not (eobp))
                  (and (> (or last-depth (allout-depth)) 0)
                       (= allout-recent-depth target-depth))
                  (not (= start-prefix-beginning
                          allout-recent-prefix-beginning)))
             allout-recent-prefix-beginning)
            (t
             (goto-char start-point)
             (if depth (allout-depth) target-depth)
             nil)))))
;;;_   > allout-next-sibling-leap (&optional depth backward)
(defun allout-next-sibling-leap (&optional depth backward)
  "Like `allout-next-sibling', but by direct search for topic at depth.

Traverse at optional DEPTH, or current depth if none specified.

Go backward if optional arg BACKWARD is non-nil.

Return the start point of the new topic if successful, nil otherwise.

Costs more than regular `allout-next-sibling' for short traversals:

 - we have to check the prior (next, if traveling backwards)
   item to confirm connectivity with the prior topic, and
 - if confirmed, we have to reestablish the allout-recent-* settings with
   some extra navigation
 - if confirmation fails, we have to do more work to recover

It is an increasingly big win when there are many intervening
offspring before the next sibling, however, so
`allout-next-sibling' resorts to this if it finds itself in that
situation."

  (if (if backward (bobp) (eobp))
      nil
    (let* ((start-point (point))
           (target-depth (or depth (allout-depth)))
           (search-whitespace-regexp nil)
           (depth-biased (- target-depth 2))
           (expression (if (<= target-depth 1)
                           allout-depth-one-regexp
                         (format allout-depth-specific-regexp
                                 depth-biased depth-biased)))
           found
           done)
      (while (not done)
        (setq found (save-match-data
                      (if backward
                          (re-search-backward expression nil 'to-limit)
                        (forward-char 1)
                        (re-search-forward expression nil 'to-limit))))
        (if (and found (allout-aberrant-container-p))
            (setq found nil))
        (setq done (or found (if backward (bobp) (eobp)))))
      (if (not found)
          (progn (goto-char start-point)
                 nil)
        ;; rationale: if any intervening items were at a lower depth, we
        ;; would now be on the first offspring at the target depth -- ie,
        ;; the preceding item (per the search direction) must be at a
        ;; lesser depth.  that's all we need to check.
        (if backward (allout-next-heading) (allout-previous-heading))
        (if (< allout-recent-depth target-depth)
            ;; return to start and reestablish allout-recent-*:
            (progn
              (goto-char start-point)
              (allout-depth)
              nil)
          (goto-char found)
          ;; locate cursor and set allout-recent-*:
          (allout-goto-prefix))))))
;;;_   > allout-previous-sibling (&optional depth backward)
(defun allout-previous-sibling (&optional depth backward)
  "Like `allout-forward-current-level' backwards, respecting invisible topics.

Optional DEPTH specifies depth to traverse, default current depth.

Optional BACKWARD reverses direction.

Return depth if successful, nil otherwise."
  (allout-next-sibling depth (not backward))
  )
;;;_   > allout-snug-back ()
(defun allout-snug-back ()
  "Position cursor at end of previous topic.

Presumes point is at the start of a topic prefix."
 (if (or (bobp) (eobp))
     nil
   (forward-char -1))
 (if (or (bobp) (not (= ?\n (preceding-char))))
     nil
   (forward-char -1))
 (point))
;;;_   > allout-beginning-of-level ()
(defun allout-beginning-of-level ()
  "Go back to the first sibling at this level, visible or not."
  (allout-end-of-level 'backward))
;;;_   > allout-end-of-level (&optional backward)
(defun allout-end-of-level (&optional backward)
  "Go to the last sibling at this level, visible or not."

  (let ((depth (allout-depth)))
    (while (allout-previous-sibling depth nil))
    (prog1 allout-recent-depth
      (if (allout-called-interactively-p) (allout-end-of-prefix)))))
;;;_   > allout-next-visible-heading (arg)
(defun allout-next-visible-heading (arg)
  "Move to the next ARGth visible heading line, backward if ARG is negative.

Move to buffer limit in indicated direction if headings are exhausted."

  (interactive "p")
  (let* ((inhibit-field-text-motion t)
         (backward (if (< arg 0) (setq arg (* -1 arg))))
	 (step (if backward -1 1))
         (progress (allout-current-bullet-pos))
	 prev got)

    (while (> arg 0)
      (while (and
              ;; Boundary condition:
              (not (if backward (bobp)(eobp)))
              ;; Move, skipping over all concealed lines in one fell swoop:
              (prog1 (condition-case nil (or (line-move step) t)
                       (error nil))
                (allout-beginning-of-current-line)
                ;; line-move can wind up on the same line if long.
                ;; when moving forward, that would yield no-progress
                (when (and (not backward)
                           (<= (point) progress))
                  ;; ensure progress by doing line-move from end-of-line:
                  (end-of-line)
                  (condition-case nil (or (line-move step) t)
                    (error nil))
                  (allout-beginning-of-current-line)
                  (setq progress (point))))
              ;; Deal with apparent header line:
              (save-match-data
                (if (not (looking-at allout-regexp))
                    ;; not a header line, keep looking:
                    t
                  (allout-prefix-data)
                  (if (and (allout-do-doublecheck)
                           (allout-aberrant-container-p))
                      ;; skip this aberrant prospective header line:
                      t
                    ;; this prospective headerline qualifies -- register:
                    (setq got allout-recent-prefix-beginning)
                    ;; and break the loop:
                    nil)))))
      ;; Register this got, it may be the last:
      (if got (setq prev got))
      (setq arg (1- arg)))
    (cond (got				; Last move was to a prefix:
           (allout-end-of-prefix))
	  (prev				; Last move wasn't, but prev was:
           (goto-char prev)
           (allout-end-of-prefix))
	  ((not backward) (end-of-line) nil))))
;;;_   > allout-previous-visible-heading (arg)
(defun allout-previous-visible-heading (arg)
  "Move to the previous heading line.

With argument, repeats or can move forward if negative.
A heading line is one that starts with a `*' (or that `allout-regexp'
matches)."
  (interactive "p")
  (prog1 (allout-next-visible-heading (- arg))
    (if (allout-called-interactively-p) (allout-end-of-prefix))))
;;;_   > allout-forward-current-level (arg)
(defun allout-forward-current-level (arg)
  "Position point at the next heading of the same level.

Takes optional repeat-count, goes backward if count is negative.

Returns resulting position, else nil if none found."
  (interactive "p")
  (let ((start-depth (allout-current-depth))
	(start-arg arg)
	(backward (> 0 arg)))
    (if (= 0 start-depth)
	(error "No siblings, not in a topic..."))
    (if backward (setq arg (* -1 arg)))
    (allout-back-to-current-heading)
    (while (and (not (zerop arg))
                (if backward
                    (allout-previous-sibling)
                  (allout-next-sibling)))
      (setq arg (1- arg)))
    (if (not (allout-called-interactively-p))
        nil
      (allout-end-of-prefix)
      (if (not (zerop arg))
          (error "Hit %s level %d topic, traversed %d of %d requested"
                 (if backward "first" "last")
                 allout-recent-depth
                 (- (abs start-arg) arg)
                 (abs start-arg))))))
;;;_   > allout-backward-current-level (arg)
(defun allout-backward-current-level (arg)
  "Inverse of `allout-forward-current-level'."
  (interactive "p")
  (if (allout-called-interactively-p)
      (let ((current-prefix-arg (* -1 arg)))
	(call-interactively 'allout-forward-current-level))
    (allout-forward-current-level (* -1 arg))))

;;;_ #5 Alteration

;;;_  - Fundamental
;;;_   = allout-post-goto-bullet
(defvar allout-post-goto-bullet nil
  "Outline internal var, for `allout-pre-command-business' hot-spot operation.

When set, tells post-processing to reposition on topic bullet, and
then unset it.  Set by `allout-pre-command-business' when implementing
hot-spot operation, where literal characters typed over a topic bullet
are mapped to the command of the corresponding control-key on the
`allout-mode-map-value'.")
(make-variable-buffer-local 'allout-post-goto-bullet)
;;;_   = allout-command-counter
(defvar allout-command-counter 0
  "Counter that monotonically increases in allout-mode buffers.

Set by `allout-pre-command-business', to support allout addons in
coordinating with allout activity.")
(make-variable-buffer-local 'allout-command-counter)
;;;_   = allout-this-command-hid-text
(defvar allout-this-command-hid-text nil
  "True if the most recent allout-mode command hid any text.")
(make-variable-buffer-local 'allout-this-command-hid-text)
;;;_   > allout-post-command-business ()
(defun allout-post-command-business ()
  "Outline `post-command-hook' function.

- Implement (and clear) `allout-post-goto-bullet', for hot-spot
  outline commands.

- Move the cursor to the beginning of the entry if it is hidden
  and collapsing activity just happened.

- If the command we're following was an undo, check for change in
  the status of encrypted items and adjust auto-save inhibitions
  accordingly.

- Decrypt topic currently being edited if it was encrypted for a save."

  (if (not (allout-mode-p))		; In allout-mode.
      nil

    (when allout-just-did-undo
      (setq allout-just-did-undo nil)
      (run-hooks 'allout-post-undo-hook)
      (cond ((and (= buffer-saved-size -1)
                  allout-auto-save-temporarily-disabled)
             ;; user possibly undid a decryption, disinhibit auto-save:
             (allout-maybe-resume-auto-save-info-after-encryption))
            ((save-excursion
               (save-restriction
                 (widen)
                 (goto-char (point-min))
                 (not (allout-next-topic-pending-encryption))))
             ;; plain-text encrypted items are present, inhibit auto-save:
             (allout-inhibit-auto-save-info-for-decryption (buffer-size)))))

    (if (and (boundp 'allout-after-save-decrypt)
             allout-after-save-decrypt)
        (allout-after-saves-handler))

    ;; Implement allout-post-goto-bullet, if set:
    (if (and allout-post-goto-bullet
	     (allout-current-bullet-pos))
	(progn (goto-char (allout-current-bullet-pos))
	       (setq allout-post-goto-bullet nil))
      (when (and (allout-hidden-p) allout-this-command-hid-text)
        (allout-beginning-of-current-entry)))))
;;;_   > allout-pre-command-business ()
(defun allout-pre-command-business ()
  "Outline `pre-command-hook' function for outline buffers.

Among other things, implements special behavior when the cursor is on the
topic bullet character.

When the cursor is on the bullet character, self-insert
characters are reinterpreted as the corresponding
control-character in the `allout-mode-map-value'.  The
`allout-mode' `post-command-hook' insures that the cursor which
has moved as a result of such reinterpretation is positioned on
the bullet character of the destination topic.

The upshot is that you can get easy, single (ie, unmodified) key
outline maneuvering operations by positioning the cursor on the bullet
char.  When in this mode you can use regular cursor-positioning
command/keystrokes to relocate the cursor off of a bullet character to
return to regular interpretation of self-insert characters."

  (if (not (allout-mode-p))
      nil
    (setq allout-command-counter (1+ allout-command-counter))
    (setq allout-this-command-hid-text nil)
    ;; Do hot-spot navigation.
    (if (and (eq this-command 'self-insert-command)
	     (eq (point)(allout-current-bullet-pos)))
        (allout-hotspot-key-handler))))
;;;_   > allout-hotspot-key-handler ()
(defun allout-hotspot-key-handler ()
  "Catchall handling of key bindings in hot-spots.

Translates unmodified keystrokes to corresponding allout commands, when
they would qualify if prefixed with the `allout-command-prefix', and sets
`this-command' accordingly.

Returns the qualifying command, if any, else nil."
  (interactive)
  (let* ((modified (event-modifiers last-command-event))
         (key-num (cond ((numberp last-command-event) last-command-event)
                        ;; for XEmacs character type:
                        ((and (fboundp 'characterp)
                              (apply 'characterp (list last-command-event)))
                         (apply 'char-to-int (list last-command-event)))
                        (t 0)))
         mapped-binding)

    (if (zerop key-num)
        nil

      (if (and
           ;; exclude control chars and escape:
           (not modified)
           (<= 33 key-num)
           (setq mapped-binding
                 (or
                  ;; try control-modified versions of keys:
                  (key-binding (vconcat allout-command-prefix
                                        (vector
                                         (if (and (<= 97 key-num) ; "a"
                                                  (>= 122 key-num)) ; "z"
                                             (- key-num 96) key-num)))
                               t)
                  ;; try non-modified versions of keys:
                  (key-binding (vconcat allout-command-prefix
                                        (vector key-num))
                               t))))
          ;; Qualified as an allout command -- do hot-spot operation.
          (setq allout-post-goto-bullet t)
        ;; accept-defaults nil, or else we get allout-item-icon-key-handler.
        (setq mapped-binding (key-binding (vector key-num))))

      (while (keymapp mapped-binding)
        (setq mapped-binding
              (lookup-key mapped-binding (vector (read-char)))))

      (when mapped-binding
        (setq this-command mapped-binding)))))

;;;_   > allout-find-file-hook ()
(defun allout-find-file-hook ()
  "Activate `allout-mode' on non-nil `allout-auto-activation', `allout-layout'.

See `allout-auto-activation' for setup instructions."
  (if (and allout-auto-activation
	   (not (allout-mode-p))
	   allout-layout)
      (allout-mode)))

;;;_  - Topic Format Assessment
;;;_   > allout-solicit-alternate-bullet (depth &optional current-bullet)
(defun allout-solicit-alternate-bullet (depth &optional current-bullet)

  "Prompt for and return a bullet char as an alternative to the current one.

Offer one suitable for current depth DEPTH as default."

  (let* ((default-bullet (or (and (stringp current-bullet) current-bullet)
                             (allout-bullet-for-depth depth)))
	 (sans-escapes (regexp-sans-escapes allout-bullets-string))
	 choice)
    (save-excursion
      (goto-char (allout-current-bullet-pos))
      (setq choice (solicit-char-in-string
                    (format "Select bullet: %s ('%s' default): "
                            sans-escapes
                            (allout-substring-no-properties default-bullet))
                    sans-escapes
                    t)))
    (message "")
    (if (string= choice "") default-bullet choice))
  )
;;;_   > allout-distinctive-bullet (bullet)
(defun allout-distinctive-bullet (bullet)
  "True if BULLET is one of those on `allout-distinctive-bullets-string'."
  (string-match (regexp-quote bullet) allout-distinctive-bullets-string))
;;;_   > allout-numbered-type-prefix (&optional prefix)
(defun allout-numbered-type-prefix (&optional prefix)
  "True if current header prefix bullet is numbered bullet."
  (and allout-numbered-bullet
        (string= allout-numbered-bullet
                 (if prefix
                     (allout-get-prefix-bullet prefix)
                   (allout-get-bullet)))))
;;;_   > allout-encrypted-type-prefix (&optional prefix)
(defun allout-encrypted-type-prefix (&optional prefix)
  "True if current header prefix bullet is for an encrypted entry (body)."
  (and allout-topic-encryption-bullet
        (string= allout-topic-encryption-bullet
                 (if prefix
                     (allout-get-prefix-bullet prefix)
                   (allout-get-bullet)))))
;;;_   > allout-bullet-for-depth (&optional depth)
(defun allout-bullet-for-depth (&optional depth)
  "Return outline topic bullet suited to optional DEPTH, or current depth."
  ;; Find bullet in plain-bullets-string modulo DEPTH.
  (if allout-stylish-prefixes
      (char-to-string (aref allout-plain-bullets-string
                            (% (max 0 (- depth 2))
                               allout-plain-bullets-string-len)))
    allout-primary-bullet)
  )

;;;_  - Topic Production
;;;_   > allout-make-topic-prefix (&optional prior-bullet
(defun allout-make-topic-prefix (&optional prior-bullet
                                            new
                                            depth
                                            instead
                                            number-control
                                            index)
  ;; Depth null means use current depth, non-null means we're either
  ;; opening a new topic after current topic, lower or higher, or we're
  ;; changing level of current topic.
  ;; Instead dominates specified bullet-char.
;;;_    . Doc string:
  "Generate a topic prefix suitable for optional arg DEPTH, or current depth.

All the arguments are optional.

PRIOR-BULLET indicates the bullet of the prefix being changed, or
nil if none.  This bullet may be preserved (other options
notwithstanding) if it is on the `allout-distinctive-bullets-string',
for instance.

Second arg NEW indicates that a new topic is being opened after the
topic at point, if non-nil.  Default bullet for new topics, eg, may
be set (contingent to other args) to numbered bullets if previous
sibling is one.  The implication otherwise is that the current topic
is being adjusted -- shifted or rebulleted -- and we don't consider
bullet or previous sibling.

Third arg DEPTH forces the topic prefix to that depth, regardless of
the current topics' depth.

If INSTEAD is:

- nil, then the bullet char for the context is used, per distinction or depth
- a (numeric) character, then character's string representation is used
- a string, then the user is asked for bullet with the first char as default
- anything else, the user is solicited with bullet char per context as default

\(INSTEAD overrides other options, including, eg, a distinctive
PRIOR-BULLET.)

Fifth arg, NUMBER-CONTROL, matters only if `allout-numbered-bullet'
is non-nil *and* no specific INSTEAD was specified.  Then
NUMBER-CONTROL non-nil forces prefix to either numbered or
unnumbered format, depending on the value of the sixth arg, INDEX.

\(Note that NUMBER-CONTROL does *not* apply to level 1 topics.  Sorry...)

If NUMBER-CONTROL is non-nil and sixth arg INDEX is non-nil then
the prefix of the topic is forced to be numbered.  Non-nil
NUMBER-CONTROL and nil INDEX forces non-numbered format on the
bullet.  Non-nil NUMBER-CONTROL and non-nil, non-number INDEX means
that the index for the numbered prefix will be derived, by counting
siblings back to start of level.  If INDEX is a number, then that
number is used as the index for the numbered prefix (allowing, eg,
sequential renumbering to not require this function counting back the
index for each successive sibling)."
;;;_    . Code:
  ;; The options are ordered in likely frequency of use, most common
  ;; highest, least lowest.  Ie, more likely to be doing prefix
  ;; adjustments than soliciting, and yet more than numbering.
  ;; Current prefix is least dominant, but most likely to be commonly
  ;; specified...

  (let* (body
         numbering
         denumbering
         (depth (or depth (allout-depth)))
         (header-lead allout-header-prefix)
         (bullet-char

          ;; Getting value for bullet char is practically the whole job:

          (cond
                                        ; Simplest situation -- level 1:
           ((<= depth 1) (setq header-lead "") allout-primary-bullet)
                                        ; Simple, too: all asterisks:
           (allout-old-style-prefixes
            ;; Cheat -- make body the whole thing, null out header-lead and
            ;; bullet-char:
            (setq body (make-string depth
                                    (string-to-char allout-primary-bullet)))
            (setq header-lead "")
            "")

           ;; (Neither level 1 nor old-style, so we're space padding.
           ;; Sneak it in the condition of the next case, whatever it is.)

           ;; Solicitation overrides numbering and other cases:
           ((progn (setq body (make-string (- depth 2) ?\ ))
                   ;; The actual condition:
                   instead)
            (let ((got (cond ((stringp instead)
                              (if (> (length instead) 0)
                                  (allout-solicit-alternate-bullet
                                   depth (substring instead 0 1))))
                             ((characterp instead) (char-to-string instead))
                             (t (allout-solicit-alternate-bullet depth)))))
              ;; Gotta check whether we're numbering and got a numbered bullet:
              (setq numbering (and allout-numbered-bullet
                                   (not (and number-control (not index)))
                                   (string= got allout-numbered-bullet)))
              ;; Now return what we got, regardless:
              got))

           ;; Numbering invoked through args:
           ((and allout-numbered-bullet number-control)
            (if (setq numbering (not (setq denumbering (not index))))
                allout-numbered-bullet
              (if (and prior-bullet
                       (not (string= allout-numbered-bullet
                                     prior-bullet)))
                  prior-bullet
                (allout-bullet-for-depth depth))))

          ;;; Neither soliciting nor controlled numbering ;;;
             ;;; (may be controlled denumbering, tho) ;;;

           ;; Check wrt previous sibling:
           ((and new				  ; only check for new prefixes
                 (<= depth (allout-depth))
                 allout-numbered-bullet	      ; ... & numbering enabled
                 (not denumbering)
                 (let ((sibling-bullet
                        (save-excursion
                          ;; Locate correct sibling:
                          (or (>= depth (allout-depth))
                              (allout-ascend-to-depth depth))
                          (allout-get-bullet))))
                   (if (and sibling-bullet
                            (string= allout-numbered-bullet sibling-bullet))
                       (setq numbering sibling-bullet)))))

           ;; Distinctive prior bullet?
           ((and prior-bullet
                 (allout-distinctive-bullet prior-bullet)
                 ;; Either non-numbered:
                 (or (not (and allout-numbered-bullet
                               (string= prior-bullet allout-numbered-bullet)))
                     ;; or numbered, and not denumbering:
                     (setq numbering (not denumbering)))
                 ;; Here 'tis:
                 prior-bullet))

           ;; Else, standard bullet per depth:
           ((allout-bullet-for-depth depth)))))

    (concat header-lead
            body
            bullet-char
            (if numbering
                (format "%d" (cond ((and index (numberp index)) index)
                                   (new (1+ (allout-sibling-index depth)))
                                   ((allout-sibling-index))))))
    )
  )
;;;_   > allout-open-topic (relative-depth &optional before offer-recent-bullet)
(defun allout-open-topic (relative-depth &optional before offer-recent-bullet)
  "Open a new topic at depth DEPTH.

New topic is situated after current one, unless optional flag BEFORE
is non-nil, or unless current line is completely empty -- lacking even
whitespace -- in which case open is done on the current line.

When adding an offspring, it will be added immediately after the parent if
the other offspring are exposed, or after the last child if the offspring
are hidden.  (The intervening offspring will be exposed in the latter
case.)

If OFFER-RECENT-BULLET is true, offer to use the bullet of the prior sibling.

Nuances:

- Creation of new topics is with respect to the visible topic
  containing the cursor, regardless of intervening concealed ones.

- New headers are generally created after/before the body of a
  topic.  However, they are created right at cursor location if the
  cursor is on a blank line, even if that breaks the current topic
  body.  This is intentional, to provide a simple means for
  deliberately dividing topic bodies.

- Double spacing of topic lists is preserved.  Also, the first
  level two topic is created double-spaced (and so would be
  subsequent siblings, if that's left intact).  Otherwise,
  single-spacing is used.

- Creation of sibling or nested topics is with respect to the topic
  you're starting from, even when creating backwards.  This way you
  can easily create a sibling in front of the current topic without
  having to go to its preceding sibling, and then open forward
  from there."

  (allout-beginning-of-current-line)
  (save-match-data
    (let* ((inhibit-field-text-motion t)
           (depth (+ (allout-current-depth) relative-depth))
           (opening-on-blank (if (looking-at "^\$")
                                 (not (setq before nil))))
           ;; bunch o vars set while computing ref-topic
           opening-numbered
           ref-depth
           ref-bullet
           (ref-topic (save-excursion
                        (cond ((< relative-depth 0)
                               (allout-ascend-to-depth depth))
                              ((>= relative-depth 1) nil)
                              (t (allout-back-to-current-heading)))
                        (setq ref-depth allout-recent-depth)
                        (setq ref-bullet
                              (if (> allout-recent-prefix-end 1)
                                  (allout-recent-bullet)
                                ""))
                        (setq opening-numbered
                              (save-excursion
                                (and allout-numbered-bullet
                                     (or (<= relative-depth 0)
                                         (allout-descend-to-depth depth))
                                     (if (allout-numbered-type-prefix)
                                         allout-numbered-bullet))))
                        (point)))
           dbl-space
           doing-beginning
           start end)

      (if (not opening-on-blank)
                                        ; Positioning and vertical
                                        ; padding -- only if not
                                        ; opening-on-blank:
          (progn
            (goto-char ref-topic)
            (setq dbl-space             ; Determine double space action:
                  (or (and (<= relative-depth 0) ; not descending;
                           (save-excursion
                             ;; at b-o-b or preceded by a blank line?
                             (or (> 0 (forward-line -1))
                                 (looking-at "^\\s-*$")
                                 (bobp)))
                           (save-excursion
                             ;; succeeded by a blank line?
                             (allout-end-of-current-subtree)
                             (looking-at "\n\n")))
                      (and (= ref-depth 1)
                           (or before
                               (= depth 1)
                               (save-excursion
                                 ;; Don't already have following
                                 ;; vertical padding:
                                 (not (allout-pre-next-prefix)))))))

            ;; Position to prior heading, if inserting backwards, and not
            ;; going outwards:
            (if (and before (>= relative-depth 0))
                (progn (allout-back-to-current-heading)
                       (setq doing-beginning (bobp))
                       (if (not (bobp))
                           (allout-previous-heading)))
              (if (and before (bobp))
                  (open-line 1)))

            (if (<= relative-depth 0)
                ;; Not going inwards, don't snug up:
                (if doing-beginning
                    (if (not dbl-space)
                        (open-line 1)
                      (open-line 2))
                  (if before
                      (progn (end-of-line)
                             (allout-pre-next-prefix)
                             (while (and (= ?\n (following-char))
                                         (save-excursion
                                           (forward-char 1)
                                           (allout-hidden-p)))
                               (forward-char 1))
                             (if (not (looking-at "^$"))
                                 (open-line 1)))
                    (allout-end-of-current-subtree)
                    (if (looking-at "\n\n") (forward-char 1))))
              ;; Going inwards -- double-space if first offspring is
              ;; double-spaced, otherwise snug up.
              (allout-end-of-entry)
              (if (eobp)
                  (newline 1)
                (line-move 1))
              (allout-beginning-of-current-line)
              (backward-char 1)
              (if (bolp)
                  ;; Blank lines between current header body and next
                  ;; header -- get to last substantive (non-white-space)
                  ;; line in body:
                  (progn (setq dbl-space t)
                         (re-search-backward "[^ \t\n]" nil t)))
              (if (looking-at "\n\n")
                  (setq dbl-space t))
              (if (save-excursion
                    (allout-next-heading)
                    (when (> allout-recent-depth ref-depth)
                      ;; This is an offspring.
                      (forward-line -1)
                      (looking-at "^\\s-*$")))
                  (progn (forward-line 1)
                         (open-line 1)
                         (forward-line 1)))
              (allout-end-of-current-line))

            ;;(if doing-beginning (goto-char doing-beginning))
            (if (not (bobp))
                ;; We insert a newline char rather than using open-line to
                ;; avoid rear-stickiness inheritance of read-only property.
                (progn (if (and (not (> depth ref-depth))
                                (not before))
                           (open-line 1)
                         (if (and (not dbl-space) (> depth ref-depth))
                             (newline 1)
                           (if dbl-space
                               (open-line 1)
                             (if (not before)
                                 (newline 1)))))
                       (if (and dbl-space (not (> relative-depth 0)))
                           (newline 1))
                       (if (and (not (eobp))
                                (or (not (bolp))
                                    (and (not (bobp))
                                         ;; bolp doesn't detect concealed
                                         ;; trailing newlines, compensate:
                                         (save-excursion
                                           (forward-char -1)
                                           (allout-hidden-p)))))
                           (forward-char 1))))
            ))
      (setq start (point))
      (insert (concat (allout-make-topic-prefix opening-numbered t depth)
                      " "))
      (setq end (1+ (point)))

      (allout-rebullet-heading (and offer-recent-bullet ref-bullet)
                               depth nil nil t)
      (if (> relative-depth 0)
          (save-excursion (goto-char ref-topic)
                          (allout-show-children)))
      (end-of-line)

      (run-hook-with-args 'allout-structure-added-hook start end)
      )
    )
  )
;;;_   > allout-open-subtopic (arg)
(defun allout-open-subtopic (arg)
  "Open new topic header at deeper level than the current one.

Negative universal ARG means to open deeper, but place the new topic
prior to the current one."
  (interactive "p")
  (allout-open-topic 1 (> 0 arg) (< 1 arg)))
;;;_   > allout-open-sibtopic (arg)
(defun allout-open-sibtopic (arg)
  "Open new topic header at same level as the current one.

Positive universal ARG means to use the bullet of the prior sibling.

Negative universal ARG means to place the new topic prior to the current
one."
  (interactive "p")
  (allout-open-topic 0 (> 0 arg) (not (= 1 arg))))
;;;_   > allout-open-supertopic (arg)
(defun allout-open-supertopic (arg)
  "Open new topic header at shallower level than the current one.

Negative universal ARG means to open shallower, but place the new
topic prior to the current one."

  (interactive "p")
  (allout-open-topic -1 (> 0 arg) (< 1 arg)))

;;;_  - Outline Alteration
;;;_   : Topic Modification
;;;_    = allout-former-auto-filler
(defvar allout-former-auto-filler nil
  "Name of modal fill function being wrapped by `allout-auto-fill'.")
;;;_    > allout-auto-fill ()
(defun allout-auto-fill ()
  "`allout-mode' autofill function.

Maintains outline hanging topic indentation if
`allout-use-hanging-indents' is set."

  (when (and (not allout-inhibit-auto-fill)
             (or (not allout-inhibit-auto-fill-on-headline)
                 (not (allout-on-current-heading-p))))
    (let ((fill-prefix (if allout-use-hanging-indents
                           ;; Check for topic header indentation:
                           (save-match-data
                             (save-excursion
                               (beginning-of-line)
                               (if (looking-at allout-regexp)
                                   ;; ... construct indentation to account for
                                   ;; length of topic prefix:
                                   (make-string (progn (allout-end-of-prefix)
                                                       (current-column))
                                                ?\ ))))))
          (use-auto-fill-function
           (if (and (eq allout-outside-normal-auto-fill-function
                        'allout-auto-fill)
                    (eq auto-fill-function 'allout-auto-fill))
               'do-auto-fill
             (or allout-outside-normal-auto-fill-function
                 auto-fill-function))))
      (if (or allout-former-auto-filler allout-use-hanging-indents)
          (funcall use-auto-fill-function)))))
;;;_    > allout-reindent-body (old-depth new-depth &optional number)
(defun allout-reindent-body (old-depth new-depth &optional number)
  "Reindent body lines which were indented at OLD-DEPTH to NEW-DEPTH.

Optional arg NUMBER indicates numbering is being added, and it must
be accommodated.

Note that refill of indented paragraphs is not done."

  (save-excursion
    (allout-end-of-prefix)
    (let* ((new-margin (current-column))
	   excess old-indent-begin old-indent-end
	   ;; We want the column where the header-prefix text started
	   ;; *before* the prefix was changed, so we infer it relative
	   ;; to the new margin and the shift in depth:
	   (old-margin (+ old-depth (- new-margin new-depth))))

      ;; Process lines up to (but excluding) next topic header:
      (allout-unprotected
       (save-match-data
         (while
	     (and (re-search-forward "\n\\(\\s-*\\)"
				     nil
				     t)
		  ;; Register the indent data, before we reset the
		  ;; match data with a subsequent `looking-at':
		  (setq old-indent-begin (match-beginning 1)
			old-indent-end (match-end 1))
		  (not (looking-at allout-regexp)))
	   (if (> 0 (setq excess (- (- old-indent-end old-indent-begin)
                                    old-margin)))
	       ;; Text starts left of old margin -- don't adjust:
	       nil
	     ;; Text was hanging at or right of old left margin --
	     ;; reindent it, preserving its existing indentation
	     ;; beyond the old margin:
	     (delete-region old-indent-begin old-indent-end)
             (indent-to (+ new-margin excess (current-column))))))))))
;;;_    > allout-rebullet-current-heading (arg)
(defun allout-rebullet-current-heading (arg)
  "Solicit new bullet for current visible heading."
  (interactive "p")
  (let ((initial-col (current-column))
	(on-bullet (eq (point)(allout-current-bullet-pos)))
        from to
	(backwards (if (< arg 0)
		       (setq arg (* arg -1)))))
    (while (> arg 0)
      (save-excursion (allout-back-to-current-heading)
		      (allout-end-of-prefix)
                      (setq from allout-recent-prefix-beginning
                            to allout-recent-prefix-end)
		      (allout-rebullet-heading t	;;; instead
						nil	;;; depth
						nil	;;; number-control
						nil	;;; index
						t)	;;; do-successors
                      (run-hook-with-args 'allout-exposure-change-hook
                                          from to t))
      (setq arg (1- arg))
      (if (<= arg 0)
	  nil
	(setq initial-col nil)		; Override positioning back to init col
	(if (not backwards)
	    (allout-next-visible-heading 1)
	  (allout-goto-prefix-doublechecked)
	  (allout-next-visible-heading -1))))
    (message "Done.")
    (cond (on-bullet (goto-char (allout-current-bullet-pos)))
	  (initial-col (move-to-column initial-col)))))
;;;_    > allout-rebullet-heading (&optional instead ...)
(defun allout-rebullet-heading (&optional instead
                                           new-depth
                                           number-control
                                           index
                                           do-successors)

  "Adjust bullet of current topic prefix.

All args are optional.

If INSTEAD is:
- nil, then the bullet char for the context is used, per distinction or depth
- a (numeric) character, then character's string representation is used
- a string, then the user is asked for bullet with the first char as default
- anything else, the user is solicited with bullet char per context as default

Second arg DEPTH forces the topic prefix to that depth, regardless
of the topic's current depth.

Third arg NUMBER-CONTROL can force the prefix to or away from
numbered form.  It has effect only if `allout-numbered-bullet' is
non-nil and soliciting was not explicitly invoked (via first arg).
Its effect, numbering or denumbering, then depends on the setting
of the fourth arg, INDEX.

If NUMBER-CONTROL is non-nil and fourth arg INDEX is nil, then the
prefix of the topic is forced to be non-numbered.  Null index and
non-nil NUMBER-CONTROL forces denumbering.  Non-nil INDEX (and
non-nil NUMBER-CONTROL) forces a numbered-prefix form.  If non-nil
INDEX is a number, then that number is used for the numbered
prefix.  Non-nil and non-number means that the index for the
numbered prefix will be derived by allout-make-topic-prefix.

Fifth arg DO-SUCCESSORS t means re-resolve count on succeeding
siblings.

Cf vars `allout-stylish-prefixes', `allout-old-style-prefixes',
and `allout-numbered-bullet', which all affect the behavior of
this function."

  (let* ((current-depth (allout-depth))
         (new-depth (or new-depth current-depth))
         (mb allout-recent-prefix-beginning)
         (me allout-recent-prefix-end)
         (current-bullet (buffer-substring-no-properties (- me 1) me))
         (has-annotation (get-text-property mb 'allout-was-hidden))
         (new-prefix (allout-make-topic-prefix current-bullet
                                                nil
                                                new-depth
                                                instead
                                                number-control
                                                index)))

    ;; Is new one identical to old?
    (if (and (= current-depth new-depth)
             (string= current-bullet
                      (substring new-prefix (1- (length new-prefix)))))
	;; Nothing to do:
        t

      ;; New prefix probably different from old:
					; get rid of old one:
      (allout-unprotected (delete-region mb me))
      (goto-char mb)
					; Dispense with number if
					; numbered-bullet prefix:
      (save-match-data
        (if (and allout-numbered-bullet
                 (string= allout-numbered-bullet current-bullet)
                 (looking-at "[0-9]+"))
            (allout-unprotected
             (delete-region (match-beginning 0)(match-end 0)))))

      ;; convey 'allout-was-hidden annotation, if original had it:
      (if has-annotation
          (put-text-property 0 (length new-prefix) 'allout-was-hidden t
                             new-prefix))

					; Put in new prefix:
      (allout-unprotected (insert new-prefix))

      ;; Reindent the body if elected, margin changed, and not encrypted body:
      (if (and allout-reindent-bodies
	       (not (= new-depth current-depth))
               (not (allout-encrypted-topic-p)))
	  (allout-reindent-body current-depth new-depth))

      (run-hook-with-args 'allout-exposure-change-hook mb me nil)

      ;; Recursively rectify successive siblings of orig topic if
      ;; caller elected for it:
      (if do-successors
	  (save-excursion
	    (while (allout-next-sibling new-depth nil)
	      (setq index
		    (cond ((numberp index) (1+ index))
			  ((not number-control)  (allout-sibling-index))))
	      (if (allout-numbered-type-prefix)
		  (allout-rebullet-heading nil		;;; instead
					    new-depth	;;; new-depth
					    number-control;;; number-control
					    index	;;; index
					    nil)))))	;;;(dont!)do-successors
      )	; (if (and (= current-depth new-depth)...))
    ) ; let* ((current-depth (allout-depth))...)
  ) ; defun
;;;_    > allout-rebullet-topic (arg)
(defun allout-rebullet-topic (arg &optional sans-offspring)
  "Rebullet the visible topic containing point and all contained subtopics.

Descends into invisible as well as visible topics, however.

When optional SANS-OFFSPRING is non-nil, subtopics are not
shifted.  (Shifting a topic outwards without shifting its
offspring is disallowed, since this would create a \"containment
discontinuity\", where the depth difference between a topic and
its immediate offspring is greater than one.)

With repeat count, shift topic depth by that amount."
  (interactive "P")
  (let ((start-col (current-column)))
    (save-excursion
      ;; Normalize arg:
      (cond ((null arg) (setq arg 0))
            ((listp arg) (setq arg (car arg))))
      ;; Fill the user in, in case we're shifting a big topic:
      (if (not (zerop arg)) (message "Shifting..."))
      (allout-back-to-current-heading)
      (if (<= (+ allout-recent-depth arg) 0)
          (error "Attempt to shift topic below level 1"))
      (allout-rebullet-topic-grunt arg nil nil nil nil sans-offspring)
      (if (not (zerop arg)) (message "Shifting... done.")))
    (move-to-column (max 0 (+ start-col arg)))))
;;;_    > allout-rebullet-topic-grunt (&optional relative-depth ...)
(defun allout-rebullet-topic-grunt (&optional relative-depth
                                               starting-depth
                                               starting-point
                                               index
                                               do-successors
                                               sans-offspring)
  "Like `allout-rebullet-topic', but on nearest containing topic
\(visible or not).

See `allout-rebullet-heading' for rebulleting behavior.

All arguments are optional.

First arg RELATIVE-DEPTH means to shift the depth of the entire
topic that amount.

Several subsequent args are for internal recursive use by the function
itself: STARTING-DEPTH, STARTING-POINT, and INDEX.

Finally, if optional SANS-OFFSPRING is non-nil then the offspring
are not shifted.  (Shifting a topic outwards without shifting
its offspring is disallowed, since this would create a
\"containment discontinuity\", where the depth difference between
a topic and its immediate offspring is greater than one.)"

  ;; XXX the recursion here is peculiar, and in general the routine may
  ;; need simplification with refactoring.

  (if (and sans-offspring
           relative-depth
           (< relative-depth 0))
      (error (concat "Attempt to shift topic outwards without offspring,"
                     " would cause containment discontinuity.")))

  (let* ((relative-depth (or relative-depth 0))
         (new-depth (allout-depth))
         (starting-depth (or starting-depth new-depth))
         (on-starting-call  (null starting-point))
         (index (or index
                    ;; Leave index null on starting call, so rebullet-heading
                    ;; calculates it at what might be new depth:
                    (and (or (zerop relative-depth)
                             (not on-starting-call))
                         (allout-sibling-index))))
         (starting-index index)
         (moving-outwards (< 0 relative-depth))
         (starting-point (or starting-point (point)))
         (local-point (point)))

    ;; Sanity check for excessive promotion done only on starting call:
    (and on-starting-call
         moving-outwards
         (> 0 (+ starting-depth relative-depth))
         (error "Attempt to shift topic out beyond level 1"))

    (cond ((= starting-depth new-depth)
           ;; We're at depth to work on this one.

           ;; When shifting out we work on the children before working on
           ;; the parent to avoid interim `allout-aberrant-container-p'
           ;; aberrancy, and vice-versa when shifting in:
           (if (>= relative-depth 0)
               (allout-rebullet-heading nil
                                        (+ starting-depth relative-depth)
                                        nil		;;; number
                                        index
                                        nil)) ;;; do-successors
           (when (not sans-offspring)
             ;; ... and work on subsequent ones which are at greater depth:
             (setq index 0)
             (allout-next-heading)
             (while (and (not (eobp))
                         (< starting-depth (allout-depth)))
               (setq index (1+ index))
               (allout-rebullet-topic-grunt relative-depth
                                            (1+ starting-depth)
                                            starting-point
                                            index)))
           (when (< relative-depth 0)
             (save-excursion
               (goto-char local-point)
               (allout-rebullet-heading nil               ;;; instead
                                        (+ starting-depth relative-depth)
                                        nil		;;; number
                                        starting-index
                                        nil)))) ;;; do-successors

          ((< starting-depth new-depth)
           ;; Rare case -- subtopic more than one level deeper than parent.
           ;; Treat this one at an even deeper level:
           (allout-rebullet-topic-grunt relative-depth
                                         new-depth
                                         starting-point
                                         index
                                         sans-offspring)))

    (if on-starting-call
        (progn
          ;; Rectify numbering of former siblings of the adjusted topic,
          ;; if topic has changed depth
          (if (or do-successors
                  (and (not (zerop relative-depth))
                       (or (= allout-recent-depth starting-depth)
                           (= allout-recent-depth (+ starting-depth
                                                        relative-depth)))))
              (allout-rebullet-heading nil nil nil nil t))
          ;; Now rectify numbering of new siblings of the adjusted topic,
          ;; if depth has been changed:
          (progn (goto-char starting-point)
                 (if (not (zerop relative-depth))
                     (allout-rebullet-heading nil nil nil nil t)))))
    )
  )
;;;_    > allout-renumber-to-depth (&optional depth)
(defun allout-renumber-to-depth (&optional depth)
  "Renumber siblings at current depth.

Affects superior topics if optional arg DEPTH is less than current depth.

Returns final depth."

  ;; Proceed by level, processing subsequent siblings on each,
  ;; ascending until we get shallower than the start depth:

  (let ((ascender (allout-depth))
	was-eobp)
    (while (and (not (eobp))
		(allout-depth)
                (>= allout-recent-depth depth)
                (>= ascender depth))
                                        ; Skip over all topics at
                                        ; lesser depths, which can not
                                        ; have been disturbed:
      (while (and (not (setq was-eobp (eobp)))
		  (> allout-recent-depth ascender))
        (allout-next-heading))
                                        ; Prime ascender for ascension:
      (setq ascender (1- allout-recent-depth))
      (if (>= allout-recent-depth depth)
          (allout-rebullet-heading nil	;;; instead
                                    nil	;;; depth
                                    nil	;;; number-control
                                    nil	;;; index
                                    t)) ;;; do-successors
      (if was-eobp (goto-char (point-max)))))
  allout-recent-depth)
;;;_    > allout-number-siblings (&optional denumber)
(defun allout-number-siblings (&optional denumber)
  "Assign numbered topic prefix to this topic and its siblings.

With universal argument, denumber -- assign default bullet to this
topic and its siblings.

With repeated universal argument (`^U^U'), solicit bullet for each
rebulleting each topic at this level."

  (interactive "P")

  (save-excursion
    (allout-back-to-current-heading)
    (allout-beginning-of-level)
    (let ((depth allout-recent-depth)
	  (index (if (not denumber) 1))
          (use-bullet (equal '(16) denumber))
          (more t))
      (while more
        (allout-rebullet-heading use-bullet		;;; instead
                                  depth			;;; depth
                                  t			;;; number-control
                                  index			;;; index
                                  nil)			;;; do-successors
        (if index (setq index (1+ index)))
        (setq more (allout-next-sibling depth nil))))))
;;;_    > allout-shift-in (arg)
(defun allout-shift-in (arg)
  "Increase depth of current heading and any items collapsed within it.

With a negative argument, the item is shifted out using
`allout-shift-out', instead.

With an argument greater than one, shift-in the item but not its
offspring, making the item into a sibling of its former children,
and a child of sibling that formerly preceded it.

You are not allowed to shift the first offspring of a topic
inwards, because that would yield a \"containment
discontinuity\", where the depth difference between a topic and
its immediate offspring is greater than one.  The first topic in
the file can be adjusted to any positive depth, however."

  (interactive "p")
  (if (< arg 0)
      (allout-shift-out (* arg -1))
    ;; refuse to create a containment discontinuity:
    (save-excursion
      (allout-back-to-current-heading)
      (if (not (bobp))
          (let* ((current-depth allout-recent-depth)
                 (start-point (point))
                 (predecessor-depth (progn
                                      (forward-char -1)
                                      (allout-goto-prefix-doublechecked)
                                      (if (< (point) start-point)
                                          allout-recent-depth
                                        0))))
            (if (and (> predecessor-depth 0)
                     (> (1+ current-depth)
                        (1+ predecessor-depth)))
                (error (concat "Disallowed shift deeper than"
                               " containing topic's children."))
              (allout-back-to-current-heading)
              (if (< allout-recent-depth (1+ current-depth))
                  (allout-show-children))))))
    (let ((where (point)))
      (allout-rebullet-topic 1 (and (> arg 1) 'sans-offspring))
      (run-hook-with-args 'allout-structure-shifted-hook arg where))))
;;;_    > allout-shift-out (arg)
(defun allout-shift-out (arg)
  "Decrease depth of current heading and any topics collapsed within it.
This will make the item a sibling of its former container.

With a negative argument, the item is shifted in using
`allout-shift-in', instead.

With an argument greater than one, shift-out the item's offspring
but not the item itself, making the former children siblings of
the item.

With an argument greater than 1, the item's offspring are shifted
out without shifting the item.  This will make the immediate
subtopics into siblings of the item."
  (interactive "p")
  (if (< arg 0)
      (allout-shift-in (* arg -1))
    ;; Get proper exposure in this area:
    (save-excursion (if (allout-ascend)
                        (allout-show-children)))
    ;; Show collapsed children if there's a successor which will become
    ;; their sibling:
    (if (and (allout-current-topic-collapsed-p)
             (save-excursion (allout-next-sibling)))
        (allout-show-children))
    (let ((where (and (allout-depth) allout-recent-prefix-beginning)))
      (save-excursion
        (if (> arg 1)
            ;; Shift the offspring but not the topic:
            (let ((children-chart (allout-chart-subtree 1)))
              (if (listp (car children-chart))
                  ;; whoops:
                  (setq children-chart (allout-flatten children-chart)))
              (save-excursion
                (dolist (child-point children-chart)
                  (goto-char child-point)
                  (allout-shift-out 1))))
          (allout-rebullet-topic (* arg -1))))
      (run-hook-with-args 'allout-structure-shifted-hook (* arg -1) where))))
;;;_   : Surgery (kill-ring) functions with special provisions for outlines:
;;;_    > allout-kill-line (&optional arg)
(defun allout-kill-line (&optional arg)
  "Kill line, adjusting subsequent lines suitably for outline mode."

  (interactive "*P")

  (if (or (not (allout-mode-p))
          (not (bolp))
          (not (save-match-data (looking-at allout-regexp))))
      ;; Just do a regular kill:
      (kill-line arg)
    ;; Ah, have to watch out for adjustments:
    (let* ((beg (point))
           end
           (beg-hidden (allout-hidden-p))
           (end-hidden (save-excursion (allout-end-of-current-line)
                                       (setq end (point))
                                       (allout-hidden-p)))
           (depth (allout-depth)))

      (allout-annotate-hidden beg end)
      (unwind-protect
          (if (and (not beg-hidden) (not end-hidden))
              (allout-unprotected (kill-line arg))
            (kill-line arg))
        (run-hooks 'allout-after-copy-or-kill-hook)
        (allout-deannotate-hidden beg end)

        (if allout-numbered-bullet
            (save-excursion         ; Renumber subsequent topics if needed:
              (if (not (save-match-data (looking-at allout-regexp)))
                  (allout-next-heading))
              (allout-renumber-to-depth depth)))
        (run-hook-with-args 'allout-structure-deleted-hook depth (point))))))
;;;_    > allout-copy-line-as-kill ()
(defun allout-copy-line-as-kill ()
  "Like `allout-kill-topic', but save to kill ring instead of deleting."
  (interactive)
  (let ((buffer-read-only t))
    (condition-case nil
        (allout-kill-line)
      (buffer-read-only nil))))
;;;_    > allout-kill-topic ()
(defun allout-kill-topic ()
  "Kill topic together with subtopics.

Trailing whitespace is killed with a topic if that whitespace:

 - would separate the topic from a subsequent sibling
 - would separate the topic from the end of buffer
 - would not be added to whitespace already separating the topic from the
   previous one.

Topic exposure is marked with text-properties, to be used by
`allout-yank-processing' for exposure recovery."

  (interactive)
  (let* ((inhibit-field-text-motion t)
         (beg (prog1 (allout-back-to-current-heading) (beginning-of-line)))
         end
         (depth allout-recent-depth))
    (allout-end-of-current-subtree)
    (if (and (/= (current-column) 0) (not (eobp)))
        (forward-char 1))
    (if (not (eobp))
	(if (and (save-match-data (looking-at "\n"))
                 (or (save-excursion
                       (or (not (allout-next-heading))
                           (= depth allout-recent-depth)))
                     (and (> (- beg (point-min)) 3)
                          (string= (buffer-substring (- beg 2) beg) "\n\n"))))
	    (forward-char 1)))

    (allout-annotate-hidden beg (setq end (point)))
    (unwind-protect                     ; for possible barf-if-buffer-read-only.
        (allout-unprotected (kill-region beg end))
      (allout-deannotate-hidden beg end)
      (run-hooks 'allout-after-copy-or-kill-hook)

      (save-excursion
        (allout-renumber-to-depth depth))
      (run-hook-with-args 'allout-structure-deleted-hook depth (point)))))
;;;_    > allout-copy-topic-as-kill ()
(defun allout-copy-topic-as-kill ()
  "Like `allout-kill-topic', but save to kill ring instead of deleting."
  (interactive)
  (let ((buffer-read-only t))
    (condition-case nil
        (allout-kill-topic)
      (buffer-read-only (message "Topic copied...")))))
;;;_    > allout-annotate-hidden (begin end)
(defun allout-annotate-hidden (begin end)
  "Qualify text with properties to indicate exposure status."

  (let ((was-modified (buffer-modified-p))
        (buffer-read-only nil))
    (allout-deannotate-hidden begin end)
    (save-excursion
      (goto-char begin)
      (let (done next prev overlay)
        (while (not done)
          ;; at or advance to start of next hidden region:
          (if (not (allout-hidden-p))
              (setq next
                    (max (1+ (point))
                         (allout-next-single-char-property-change (point)
                                                                  'invisible
                                                                  nil end))))
          (if (or (not next) (eq prev next))
              ;; still not at start of hidden area -- must not be any left.
              (setq done t)
            (goto-char next)
            (setq prev next)
            (if (not (allout-hidden-p))
                ;; still not at start of hidden area.
                (setq done t)
              (setq overlay (allout-get-invisibility-overlay))
              (setq next (overlay-end overlay)
                    prev next)
              ;; advance to end of this hidden area:
              (when next
                (goto-char next)
                (allout-unprotected
                 (let ((buffer-undo-list t))
                   (put-text-property (overlay-start overlay) next
                                      'allout-was-hidden t)))))))))
    (set-buffer-modified-p was-modified)))
;;;_    > allout-deannotate-hidden (begin end)
(defun allout-deannotate-hidden (begin end)
  "Remove allout hidden-text annotation between BEGIN and END."

  (allout-unprotected
   (let ((inhibit-read-only t)
         (buffer-undo-list t))
     (remove-text-properties begin (min end (point-max))
                             '(allout-was-hidden t)))))
;;;_    > allout-hide-by-annotation (begin end)
(defun allout-hide-by-annotation (begin end)
  "Translate text properties indicating exposure status into actual exposure."
  (save-excursion
    (goto-char begin)
    (let ((was-modified (buffer-modified-p))
          done next prev)
      (while (not done)
        ;; at or advance to start of next annotation:
        (if (not (get-text-property (point) 'allout-was-hidden))
            (setq next (allout-next-single-char-property-change
                        (point) 'allout-was-hidden nil end)))
        (if (or (not next) (eq prev next))
            ;; no more or not advancing -- must not be any left.
            (setq done t)
          (goto-char next)
          (setq prev next)
          (if (not (get-text-property (point) 'allout-was-hidden))
              ;; still not at start of annotation.
              (setq done t)
            ;; advance to just after end of this annotation:
            (setq next (allout-next-single-char-property-change
                        (point) 'allout-was-hidden nil end))
            (let ((o (make-overlay prev next nil 'front-advance)))
              (overlay-put o 'category 'allout-exposure-category)
              (overlay-put o 'evaporate t))
            (allout-deannotate-hidden prev next)
            (setq prev next)
            (if next (goto-char next)))))
      (set-buffer-modified-p was-modified))))
;;;_    > allout-yank-processing ()
(defun allout-yank-processing (&optional arg)

  "Incidental allout-specific business to be done just after text yanks.

Does depth adjustment of yanked topics, when:

1 the stuff being yanked starts with a valid outline header prefix, and
2 it is being yanked at the end of a line which consists of only a valid
     topic prefix.

Also, adjusts numbering of subsequent siblings when appropriate.

Depth adjustment alters the depth of all the topics being yanked
the amount it takes to make the first topic have the depth of the
header into which it's being yanked.

The point is left in front of yanked, adjusted topics, rather than
at the end (and vice-versa with the mark).  Non-adjusted yanks,
however, are left exactly like normal, non-allout-specific yanks."

  (interactive "*P")
					; Get to beginning, leaving
					; region around subject:
  (if (< (allout-mark-marker t) (point))
      (exchange-point-and-mark))
  (save-match-data
    (let* ((subj-beg (point))
           (into-bol (bolp))
           (subj-end (allout-mark-marker t))
           ;; 'resituate' if yanking an entire topic into topic header:
           (resituate (and (let ((allout-inhibit-aberrance-doublecheck t))
                             (allout-e-o-prefix-p))
                           (looking-at allout-regexp)
                           (allout-prefix-data)))
           ;; `rectify-numbering' if resituating (where several topics may
           ;; be resituating) or yanking a topic into a topic slot (bol):
           (rectify-numbering (or resituate
                                  (and into-bol (looking-at allout-regexp)))))
      (if resituate
          ;; Yanking a topic into the start of a topic -- reconcile to fit:
          (let* ((inhibit-field-text-motion t)
                 (prefix-len (if (not (match-end 1))
                                 1
                               (- (match-end 1) subj-beg)))
                 (subj-depth allout-recent-depth)
                 (prefix-bullet (allout-recent-bullet))
                 (adjust-to-depth
                  ;; Nil if adjustment unnecessary, otherwise depth to which
                  ;; adjustment should be made:
                  (save-excursion
                    (and (goto-char subj-end)
                         (eolp)
                         (goto-char subj-beg)
                         (and (looking-at allout-regexp)
                              (progn
                                (beginning-of-line)
                                (not (= (point) subj-beg)))
                              (looking-at allout-regexp)
                              (allout-prefix-data))
                         allout-recent-depth)))
                 (more t))
            (setq rectify-numbering allout-numbered-bullet)
            (if adjust-to-depth
                                        ; Do the adjustment:
                (progn
                  (save-restriction
                    (narrow-to-region subj-beg subj-end)
                                        ; Trim off excessive blank
                                        ; line at end, if any:
                    (goto-char (point-max))
                    (if (looking-at "^$")
                        (allout-unprotected (delete-char -1)))
                                        ; Work backwards, with each
                                        ; shallowest level,
                                        ; successively excluding the
                                        ; last processed topic from
                                        ; the narrow region:
                    (while more
                      (allout-back-to-current-heading)
                                        ; go as high as we can in each bunch:
                      (while (allout-ascend t))
                      (save-excursion
                        (allout-unprotected
                         (allout-rebullet-topic-grunt (- adjust-to-depth
                                                         subj-depth)))
                        (allout-depth))
                      (if (setq more (not (bobp)))
                          (progn (widen)
                                 (forward-char -1)
                                 (narrow-to-region subj-beg (point))))))
                  ;; Remove new heading prefix:
                  (allout-unprotected
                   (progn
                     (delete-region (point) (+ (point)
                                               prefix-len
                                               (- adjust-to-depth
                                                  subj-depth)))
                                        ; and delete residual subj
                                        ; prefix digits and space:
                     (while (looking-at "[0-9]") (delete-char 1))
                     (delete-char -1)
                     (if (not (eolp))
                         (forward-char))))
                  ;; Assert new topic's bullet - minimal effort if unchanged:
                  (allout-rebullet-heading (string-to-char prefix-bullet)))
              (exchange-point-and-mark))))
      (if rectify-numbering
          (progn
            (save-excursion
                                        ; Give some preliminary feedback:
              (message "... reconciling numbers")
                                        ; ... and renumber, in case necessary:
              (goto-char subj-beg)
              (if (allout-goto-prefix-doublechecked)
                  (allout-unprotected
                   (allout-rebullet-heading nil          ;;; instead
                                            (allout-depth) ;;; depth
                                            nil ;;; number-control
                                            nil ;;; index
                                            t)))
              (message ""))))
      (if (or into-bol resituate)
          (allout-hide-by-annotation (point) (allout-mark-marker t))
        (allout-deannotate-hidden (allout-mark-marker t) (point)))
      (if (not resituate)
          (exchange-point-and-mark))
      (run-hook-with-args 'allout-structure-added-hook subj-beg subj-end))))
;;;_    > allout-yank (&optional arg)
(defun allout-yank (&optional arg)
  "`allout-mode' yank, with depth and numbering adjustment of yanked topics.

Non-topic yanks work no differently than normal yanks.

If a topic is being yanked into a bare topic prefix, the depth of the
yanked topic is adjusted to the depth of the topic prefix.

  1 we're yanking in an `allout-mode' buffer
  2 the stuff being yanked starts with a valid outline header prefix, and
  3 it is being yanked at the end of a line which consists of only a valid
    topic prefix.

If these conditions hold then the depth of the yanked topics are all
adjusted the amount it takes to make the first one at the depth of the
header into which it's being yanked.

The point is left in front of yanked, adjusted topics, rather than
at the end (and vice-versa with the mark).  Non-adjusted yanks,
however, (ones that don't qualify for adjustment) are handled
exactly like normal yanks.

Numbering of yanked topics, and the successive siblings at the depth
into which they're being yanked, is adjusted.

`allout-yank-pop' works with `allout-yank' just like normal `yank-pop'
works with normal `yank' in non-outline buffers."

  (interactive "*P")
  (setq this-command 'yank)
  (allout-unprotected
   (yank arg))
  (if (allout-mode-p)
      (allout-yank-processing)))
;;;_    > allout-yank-pop (&optional arg)
(defun allout-yank-pop (&optional arg)
  "Yank-pop like `allout-yank' when popping to bare outline prefixes.

Adapts level of popped topics to level of fresh prefix.

Note -- prefix changes to distinctive bullets will stick, if followed
by pops to non-distinctive yanks.  Bug..."

  (interactive "*p")
  (setq this-command 'yank)
  (yank-pop arg)
  (if (allout-mode-p)
      (allout-yank-processing)))

;;;_  - Specialty bullet functions
;;;_   : File Cross references
;;;_    > allout-resolve-xref ()
(defun allout-resolve-xref ()
  "Pop to file associated with current heading, if it has an xref bullet.

\(Works according to setting of `allout-file-xref-bullet')."
  (interactive)
  (if (not allout-file-xref-bullet)
      (error
       "Outline cross references disabled -- no `allout-file-xref-bullet'")
    (if (not (string= (allout-current-bullet) allout-file-xref-bullet))
        (error "Current heading lacks cross-reference bullet `%s'"
               allout-file-xref-bullet)
      (let ((inhibit-field-text-motion t)
            file-name)
        (save-match-data
          (save-excursion
            (let* ((text-start allout-recent-prefix-end)
                   (heading-end (point-at-eol)))
              (goto-char text-start)
              (setq file-name
                    (if (re-search-forward "\\s-\\(\\S-*\\)" heading-end t)
                        (buffer-substring (match-beginning 1)
                                          (match-end 1)))))))
        (setq file-name (expand-file-name file-name))
        (if (or (file-exists-p file-name)
                (if (file-writable-p file-name)
                    (y-or-n-p (format "%s not there, create one? "
                                      file-name))
                  (error "%s not found and can't be created" file-name)))
            (condition-case failure
                (find-file-other-window file-name)
              (error failure))
          (error "%s not found" file-name))
        )
      )
    )
  )

;;;_ #6 Exposure Control

;;;_  - Fundamental
;;;_   > allout-flag-region (from to flag)
(defun allout-flag-region (from to flag)
  "Conceal text between FROM and TO if FLAG is non-nil, else reveal it.

Exposure-change hook `allout-exposure-change-hook' is run with the same
arguments as this function, after the exposure changes are made."

  ;; We use outline invisibility spec.
  (remove-overlays from to 'category 'allout-exposure-category)
  (when flag
    (let ((o (make-overlay from to nil 'front-advance)))
      (overlay-put o 'category 'allout-exposure-category)
      (overlay-put o 'evaporate t)
      (when (featurep 'xemacs)
        (let ((props (symbol-plist 'allout-exposure-category)))
          (while props
            (condition-case nil
                ;; as of 2008-02-27, xemacs lacks modification-hooks
                (overlay-put o (pop props) (pop props))
              (error nil))))))
    (setq allout-this-command-hid-text t))
  (run-hook-with-args 'allout-exposure-change-hook from to flag))
;;;_   > allout-flag-current-subtree (flag)
(defun allout-flag-current-subtree (flag)
  "Conceal currently-visible topic's subtree if FLAG non-nil, else reveal it."

  (save-excursion
    (allout-back-to-current-heading)
    (let ((inhibit-field-text-motion t))
      (end-of-line))
    (allout-flag-region (point)
                        ;; Exposing must not leave trailing blanks hidden,
                        ;; but can leave them exposed when hiding, so we
                        ;; can use flag's inverse as the
                        ;; include-trailing-blank cue:
                        (allout-end-of-current-subtree (not flag))
                        flag)))

;;;_  - Topic-specific
;;;_   > allout-show-entry ()
(defun allout-show-entry ()
  "Like `allout-show-current-entry', but reveals entries in hidden topics.

This is a way to give restricted peek at a concealed locality without the
expense of exposing its context, but can leave the outline with aberrant
exposure.  `allout-show-offshoot' should be used after the peek to rectify
the exposure."

  (interactive)
  (save-excursion
    (let (beg end)
      (allout-goto-prefix-doublechecked)
      (setq beg (if (allout-hidden-p) (1- (point)) (point)))
      (setq end (allout-pre-next-prefix))
      (allout-flag-region beg end nil)
      (list beg end))))
;;;_   > allout-show-children (&optional level strict)
(defun allout-show-children (&optional level strict)

  "If point is visible, show all direct subheadings of this heading.

Otherwise, do `allout-show-to-offshoot', and then show subheadings.

Optional LEVEL specifies how many levels below the current level
should be shown, or all levels if t.  Default is 1.

Optional STRICT means don't resort to -show-to-offshoot, no matter
what.  This is basically so -show-to-offshoot, which is called by
this function, can employ the pure offspring-revealing capabilities of
it.

Returns point at end of subtree that was opened, if any.  (May get a
point of non-opened subtree?)"

  (interactive "p")
  (let ((start-point (point)))
    (if (and (not strict)
             (allout-hidden-p))

        (progn (allout-show-to-offshoot) ; Point's concealed, open to
                                        ; expose it.
               ;; Then recurse, but with "strict" set so we don't
               ;; infinite regress:
               (allout-show-children level t))

      (save-excursion
        (allout-beginning-of-current-line)
        (save-restriction
          (let* (depth
                 ;; translate the level spec for this routine to the ones
                 ;; used by -chart-subtree and -chart-to-reveal:
                 (chart-level (cond ((not level) 1)
                                    ((eq level t) nil)
                                    (t level)))
                 (chart (allout-chart-subtree chart-level))
                 (to-reveal (or (allout-chart-to-reveal chart chart-level)
                                ;; interactive, show discontinuous children:
                                (and chart
                                     (allout-called-interactively-p)
                                     (save-excursion
                                       (allout-back-to-current-heading)
                                       (setq depth (allout-current-depth))
                                       (and (allout-next-heading)
                                            (> allout-recent-depth
                                               (1+ depth))))
                                     (message
                                      "Discontinuous offspring; use `%s %s'%s."
                                      (substitute-command-keys
                                       "\\[universal-argument]")
                                      (substitute-command-keys
                                       "\\[allout-shift-out]")
                                      " to elevate them.")
                                     (allout-chart-to-reveal
                                      chart (- allout-recent-depth depth))))))
            (goto-char start-point)
            (when (and strict (allout-hidden-p))
              ;; Concealed root would already have been taken care of,
              ;; unless strict was set.
              (allout-flag-region (point) (allout-snug-back) nil)
              (when allout-show-bodies
                (goto-char (car to-reveal))
                (allout-show-current-entry)))
            (while to-reveal
              (goto-char (car to-reveal))
              (allout-flag-region (save-excursion (allout-snug-back) (point))
                                  (progn (search-forward "\n" nil t)
                                         (1- (point)))
                                  nil)
              (when allout-show-bodies
                (goto-char (car to-reveal))
                (allout-show-current-entry))
              (setq to-reveal (cdr to-reveal)))))))
    ;; Compensate for `save-excursion's maintenance of point
    ;; within invisible text:
    (goto-char start-point)))
;;;_   > allout-show-to-offshoot ()
(defun allout-show-to-offshoot ()
  "Like `allout-show-entry', but reveals all concealed ancestors, as well.

Useful for coherently exposing to a random point in a hidden region."
  (interactive)
  (save-excursion
    (let ((inhibit-field-text-motion t)
          (orig-pt (point))
	  (orig-pref (allout-goto-prefix-doublechecked))
	  (last-at (point))
	  (bag-it 0))
      (while (or (> bag-it 1) (allout-hidden-p))
        (while (allout-hidden-p)
          (move-beginning-of-line 1)
          (if (allout-hidden-p) (forward-char -1)))
	(if (= last-at (setq last-at (point)))
	    ;; Oops, we're not making any progress!  Show the current topic
	    ;; completely, and try one more time here, if we haven't already.
	    (progn (beginning-of-line)
		   (allout-show-current-subtree)
		   (goto-char orig-pt)
		   (setq bag-it (1+ bag-it))
                   (if (> bag-it 1)
                       (error "allout-show-to-offshoot: %s"
                              "Stumped by aberrant nesting.")))
          (if (> bag-it 0) (setq bag-it 0))
          (allout-show-children)
          (goto-char orig-pref)))
      (goto-char orig-pt)))
  (if (allout-hidden-p)
      (allout-show-entry)))
;;;_   > allout-hide-current-entry ()
(defun allout-hide-current-entry ()
  "Hide the body directly following this heading."
  (interactive)
  (allout-back-to-current-heading)
  (save-excursion
    (let ((inhibit-field-text-motion t))
      (end-of-line))
    (allout-flag-region (point)
                        (progn (allout-end-of-entry) (point))
                        t)))
;;;_   > allout-show-current-entry (&optional arg)
(defun allout-show-current-entry (&optional arg)
  "Show body following current heading, or hide entry with universal argument."

  (interactive "P")
  (if arg
      (allout-hide-current-entry)
    (save-excursion (allout-show-to-offshoot))
    (save-excursion
      (allout-flag-region (point)
                          (progn (allout-end-of-entry t) (point))
                          nil)
      )))
;;;_   > allout-show-current-subtree (&optional arg)
(defun allout-show-current-subtree (&optional arg)
  "Show everything within the current topic.
With a repeat-count, expose this topic and its siblings."
  (interactive "P")
  (save-excursion
    (if (<= (allout-current-depth) 0)
	;; Outside any topics -- try to get to the first:
	(if (not (allout-next-heading))
	    (error "No topics")
	  ;; got to first, outermost topic -- set to expose it and siblings:
	  (message "Above outermost topic -- exposing all.")
	  (allout-flag-region (point-min)(point-max) nil))
      (allout-beginning-of-current-line)
      (if (not arg)
	  (allout-flag-current-subtree nil)
	(allout-beginning-of-level)
	(allout-expose-topic '(* :))))))
;;;_   > allout-current-topic-collapsed-p (&optional include-single-liners)
(defun allout-current-topic-collapsed-p (&optional include-single-liners)
  "True if the currently visible containing topic is already collapsed.

Single line topics intrinsically can be considered as being both
collapsed and uncollapsed.  If optional INCLUDE-SINGLE-LINERS is
true, then single-line topics are considered to be collapsed.  By
default, they are treated as being uncollapsed."
  (save-match-data
    (save-excursion
      (and
       ;; Is the topic all on one line (allowing for trailing blank line)?
       (>= (progn (allout-back-to-current-heading)
                  (let ((inhibit-field-text-motion t))
                    (move-end-of-line 1))
                  (point))
           (allout-end-of-current-subtree (not (looking-at "\n\n"))))

       (or include-single-liners
           (progn (backward-char 1) (allout-hidden-p)))))))
;;;_   > allout-hide-current-subtree (&optional just-close)
(defun allout-hide-current-subtree (&optional just-close)
  "Close the current topic, or containing topic if this one is already closed.

If this topic is closed and it's a top level topic, close this topic
and its siblings.

If optional arg JUST-CLOSE is non-nil, do not close the parent or
siblings, even if the target topic is already closed."

  (interactive)
  (let* ((from (point))
         (sibs-msg "Top-level topic already closed -- closing siblings...")
         (current-exposed (not (allout-current-topic-collapsed-p t))))
    (cond (current-exposed (allout-flag-current-subtree t))
          (just-close nil)
          ((allout-ascend) (allout-hide-current-subtree))
          (t (goto-char 0)
             (message sibs-msg)
             (allout-goto-prefix-doublechecked)
             (allout-expose-topic '(0 :))
             (message (concat sibs-msg "  Done."))))
    (goto-char from)))
;;;_   > allout-toggle-current-subtree-exposure
(defun allout-toggle-current-subtree-exposure ()
  "Show or hide the current subtree depending on its current state."
  ;; thanks to tassilo for suggesting this.
  (interactive)
  (save-excursion
    (allout-back-to-heading)
    (if (allout-hidden-p (point-at-eol))
        (allout-show-current-subtree)
      (allout-hide-current-subtree))))
;;;_   > allout-show-current-branches ()
(defun allout-show-current-branches ()
  "Show all subheadings of this heading, but not their bodies."
  (interactive)
  (let ((inhibit-field-text-motion t))
    (beginning-of-line))
  (allout-show-children t))
;;;_   > allout-hide-current-leaves ()
(defun allout-hide-current-leaves ()
  "Hide the bodies of the current topic and all its offspring."
  (interactive)
  (allout-back-to-current-heading)
  (allout-hide-region-body (point) (progn (allout-end-of-current-subtree)
                                           (point))))

;;;_  - Region and beyond
;;;_   > allout-show-all ()
(defun allout-show-all ()
  "Show all of the text in the buffer."
  (interactive)
  (message "Exposing entire buffer...")
  (allout-flag-region (point-min) (point-max) nil)
  (message "Exposing entire buffer...  Done."))
;;;_   > allout-hide-bodies ()
(defun allout-hide-bodies ()
  "Hide all of buffer except headings."
  (interactive)
  (allout-hide-region-body (point-min) (point-max)))
;;;_   > allout-hide-region-body (start end)
(defun allout-hide-region-body (start end)
  "Hide all body lines in the region, but not headings."
  (save-match-data
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (let ((inhibit-field-text-motion t))
          (while (not (eobp))
            (end-of-line)
            (allout-flag-region (point) (allout-end-of-entry) t)
            (if (not (eobp))
                (forward-char
                 (if (looking-at "\n\n")
                     2 1)))))))))

;;;_   > allout-expose-topic (spec)
(defun allout-expose-topic (spec)
  "Apply exposure specs to successive outline topic items.

Use the more convenient frontend, `allout-new-exposure', if you don't
need evaluation of the arguments, or even better, the `allout-layout'
variable-keyed mode-activation/auto-exposure feature of allout outline
mode.  See the respective documentation strings for more details.

Cursor is left at start position.

SPEC is either a number or a list.

Successive specs on a list are applied to successive sibling topics.

A simple spec (either a number, one of a few symbols, or the null
list) dictates the exposure for the corresponding topic.

Non-null lists recursively designate exposure specs for respective
subtopics of the current topic.

The `:' repeat spec is used to specify exposure for any number of
successive siblings, up to the trailing ones for which there are
explicit specs following the `:'.

Simple (numeric and null-list) specs are interpreted as follows:

 Numbers indicate the relative depth to open the corresponding topic.
     - negative numbers force the topic to be closed before opening to the
       absolute value of the number, so all siblings are open only to
       that level.
     - positive numbers open to the relative depth indicated by the
       number, but do not force already opened subtopics to be closed.
     - 0 means to close topic -- hide all offspring.
  :  - `repeat'
       apply prior element to all siblings at current level, *up to*
       those siblings that would be covered by specs following the `:'
       on the list.  Ie, apply to all topics at level but the last
       ones.  (Only first of multiple colons at same level is
       respected -- subsequent ones are discarded.)
  *  - completely opens the topic, including bodies.
  +  - shows all the sub headers, but not the bodies
  -  - exposes the body of the corresponding topic.

Examples:
\(allout-expose-topic '(-1 : 0))
	Close this and all following topics at current level, exposing
	only their immediate children, but close down the last topic
	at this current level completely.
\(allout-expose-topic '(-1 () : 1 0))
	Close current topic so only the immediate subtopics are shown;
	show the children in the second to last topic, and completely
	close the last one.
\(allout-expose-topic '(-2 : -1 *))
        Expose children and grandchildren of all topics at current
	level except the last two; expose children of the second to
	last and completely open the last one."

  (interactive "xExposure spec: ")
  (if (not (listp spec))
      nil
    (let ((depth (allout-depth))
	  (max-pos 0)
	  prev-elem curr-elem
	  stay)
      (while spec
	(setq prev-elem curr-elem
	      curr-elem (car spec)
	      spec (cdr spec))
	(cond				; Do current element:
	 ((null curr-elem) nil)
	 ((symbolp curr-elem)
	  (cond ((eq curr-elem '*) (allout-show-current-subtree)
		 (if (> allout-recent-end-of-subtree max-pos)
		     (setq max-pos allout-recent-end-of-subtree)))
                ((eq curr-elem '+)
                 (if (not (allout-hidden-p))
                     (save-excursion (allout-hide-current-subtree t)))
                 (allout-show-current-branches)
		 (if (> allout-recent-end-of-subtree max-pos)
		     (setq max-pos allout-recent-end-of-subtree)))
		((eq curr-elem '-) (allout-show-current-entry))
		((eq curr-elem ':)
		 (setq stay t)
		 ;; Expand the `repeat' spec to an explicit version,
		 ;; w.r.t. remaining siblings:
		 (let ((residue	   ; = # of sibs not covered by remaining spec
			;; Dang, could be nice to make use of the chart, sigh:
			(- (length (allout-chart-siblings))
			   (length spec))))
		   (if (< 0 residue)
		       ;; Some residue -- cover it with prev-elem:
		       (setq spec (append (make-list residue prev-elem)
					  spec)))))))
	 ((numberp curr-elem)
	  (if (and (>= 0 curr-elem) (not (allout-hidden-p)))
	      (save-excursion (allout-hide-current-subtree t)
			      (if (> 0 curr-elem)
				  nil
				(if (> allout-recent-end-of-subtree max-pos)
				    (setq max-pos
					  allout-recent-end-of-subtree)))))
	  (if (> (abs curr-elem) 0)
	      (progn (allout-show-children (abs curr-elem))
		     (if (> allout-recent-end-of-subtree max-pos)
			 (setq max-pos allout-recent-end-of-subtree)))))
	  ((listp curr-elem)
	   (if (allout-descend-to-depth (1+ depth))
	       (let ((got (allout-expose-topic curr-elem)))
		 (if (and got (> got max-pos)) (setq max-pos got))))))
	(cond (stay (setq stay nil))
	      ((listp (car spec)) nil)
	      ((> max-pos (point))
	       ;; Capitalize on max-pos state to get us nearer next sibling:
	       (progn (goto-char (min (point-max) max-pos))
		      (allout-next-heading)))
	      ((allout-next-sibling depth))))
      max-pos)))
;;;_   > allout-old-expose-topic (spec &rest followers)
(defun allout-old-expose-topic (spec &rest followers)

  "Deprecated.  Use `allout-expose-topic' (with different schema
format) instead.

Dictate wholesale exposure scheme for current topic, according to SPEC.

SPEC is either a number or a list.  Optional successive args
dictate exposure for subsequent siblings of current topic.

A simple spec (either a number, a special symbol, or the null list)
dictates the overall exposure for a topic.  Non null lists are
composite specs whose first element dictates the overall exposure for
a topic, with the subsequent elements in the list interpreted as specs
that dictate the exposure for the successive offspring of the topic.

Simple (numeric and null-list) specs are interpreted as follows:

 - Numbers indicate the relative depth to open the corresponding topic:
  - negative numbers force the topic to be close before opening to the
    absolute value of the number.
  - positive numbers just open to the relative depth indicated by the number.
  - 0 just closes
 - `*' completely opens the topic, including bodies.
 - `+' shows all the sub headers, but not the bodies
 - `-' exposes the body and immediate offspring of the corresponding topic.

If the spec is a list, the first element must be a number, which
dictates the exposure depth of the topic as a whole.  Subsequent
elements of the list are nested SPECs, dictating the specific exposure
for the corresponding offspring of the topic.

Optional FOLLOWERS arguments dictate exposure for succeeding siblings."

  (interactive "xExposure spec: ")
  (let ((inhibit-field-text-motion t)
        (depth (allout-current-depth))
	max-pos)
    (cond ((null spec) nil)
	  ((symbolp spec)
	   (if (eq spec '*) (allout-show-current-subtree))
	   (if (eq spec '+) (allout-show-current-branches))
	   (if (eq spec '-) (allout-show-current-entry)))
	  ((numberp spec)
	   (if (>= 0 spec)
	       (save-excursion (allout-hide-current-subtree t)
			       (end-of-line)
			       (if (or (not max-pos)
				       (> (point) max-pos))
				   (setq max-pos (point)))
			       (if (> 0 spec)
				   (setq spec (* -1 spec)))))
	   (if (> spec 0)
	     (allout-show-children spec)))
	  ((listp spec)
	   ;(let ((got (allout-old-expose-topic (car spec))))
	   ;  (if (and got (or (not max-pos) (> got max-pos)))
	   ;	 (setq max-pos got)))
	   (let ((new-depth  (+ (allout-current-depth) 1))
		 got)
	     (setq max-pos (allout-old-expose-topic (car spec)))
	     (setq spec (cdr spec))
	     (if (and spec
		      (allout-descend-to-depth new-depth)
		      (not (allout-hidden-p)))
		 (progn (setq got (apply 'allout-old-expose-topic spec))
			(if (and got (or (not max-pos) (> got max-pos)))
			    (setq max-pos got)))))))
    (while (and followers
		(progn (if (and max-pos (< (point) max-pos))
			   (progn (goto-char max-pos)
				  (setq max-pos nil)))
		       (end-of-line)
		       (allout-next-sibling depth)))
      (allout-old-expose-topic (car followers))
      (setq followers (cdr followers)))
    max-pos))
;;;_   > allout-new-exposure '()
(defmacro allout-new-exposure (&rest spec)
  "Literal frontend for `allout-expose-topic', doesn't evaluate arguments.
Some arguments that would need to be quoted in `allout-expose-topic'
need not be quoted in `allout-new-exposure'.

Cursor is left at start position.

Use this instead of obsolete `allout-exposure'.

Examples:
\(allout-new-exposure (-1 () () () 1) 0)
	Close current topic at current level so only the immediate
	subtopics are shown, except also show the children of the
	third subtopic; and close the next topic at the current level.
\(allout-new-exposure : -1 0)
	Close all topics at current level to expose only their
	immediate children, except for the last topic at the current
	level, in which even its immediate children are hidden.
\(allout-new-exposure -2 : -1 *)
        Expose children and grandchildren of first topic at current
	level, and expose children of subsequent topics at current
	level *except* for the last, which should be opened completely."
  (list 'save-excursion
	'(if (not (or (allout-goto-prefix-doublechecked)
		      (allout-next-heading)))
	     (error "allout-new-exposure: Can't find any outline topics"))
	(list 'allout-expose-topic (list 'quote spec))))

;;;_ #7 Systematic outline presentation -- copying, printing, flattening

;;;_  - Mapping and processing of topics
;;;_   ( See also Subtree Charting, in Navigation code.)
;;;_   > allout-stringify-flat-index (flat-index)
(defun allout-stringify-flat-index (flat-index &optional context)
  "Convert list representing section/subsection/... to document string.

Optional arg CONTEXT indicates interior levels to include."
  (let ((delim ".")
	result
	numstr
	(context-depth (or (and context 2) 1)))
    ;; Take care of the explicit context:
    (while (> context-depth 0)
      (setq numstr (int-to-string (car flat-index))
	    flat-index (cdr flat-index)
	    result (if flat-index
		       (cons delim (cons numstr result))
		       (cons numstr result))
	    context-depth (if flat-index (1- context-depth) 0)))
    (setq delim " ")
    ;; Take care of the indentation:
    (if flat-index
	(progn
	  (while flat-index
	    (setq result
		  (cons delim
			(cons (make-string
			       (1+ (truncate (if (zerop (car flat-index))
						 1
					       (log10 (car flat-index)))))
			       ? )
			      result)))
	    (setq flat-index (cdr flat-index)))
	  ;; Dispose of single extra delim:
	  (setq result (cdr result))))
    (apply 'concat result)))
;;;_   > allout-stringify-flat-index-plain (flat-index)
(defun allout-stringify-flat-index-plain (flat-index)
  "Convert list representing section/subsection/... to document string."
  (let ((delim ".")
	result)
	(while flat-index
	  (setq result (cons (int-to-string (car flat-index))
			     (if result
				 (cons delim result))))
	  (setq flat-index (cdr flat-index)))
    (apply 'concat result)))
;;;_   > allout-stringify-flat-index-indented (flat-index)
(defun allout-stringify-flat-index-indented (flat-index)
  "Convert list representing section/subsection/... to document string."
  (let ((delim ".")
	result
	numstr)
    ;; Take care of the explicit context:
    (setq numstr (int-to-string (car flat-index))
	  flat-index (cdr flat-index)
	  result (if flat-index
		     (cons delim (cons numstr result))
		   (cons numstr result)))
    (setq delim " ")
    ;; Take care of the indentation:
    (if flat-index
	(progn
	  (while flat-index
	    (setq result
		  (cons delim
			(cons (make-string
			       (1+ (truncate (if (zerop (car flat-index))
						 1
					       (log10 (car flat-index)))))
			       ? )
			      result)))
	    (setq flat-index (cdr flat-index)))
	  ;; Dispose of single extra delim:
	  (setq result (cdr result))))
    (apply 'concat result)))
;;;_   > allout-listify-exposed (&optional start end format)
(defun allout-listify-exposed (&optional start end format)

  "Produce a list representing exposed topics in current region.

This list can then be used by `allout-process-exposed' to manipulate
the subject region.

Optional START and END indicate bounds of region.

Optional arg, FORMAT, designates an alternate presentation form for
the prefix:

 list -- Present prefix as numeric section.subsection..., starting with
	section indicated by the list, innermost nesting first.
 `indent' (symbol) --  Convert header prefixes to all white space,
		       except for distinctive bullets.

The elements of the list produced are lists that represents a topic
header and body.  The elements of that list are:

 - a number representing the depth of the topic,
 - a string representing the header-prefix, including trailing whitespace and
   bullet.
 - a string representing the bullet character,
 - and a series of strings, each containing one line of the exposed
   portion of the topic entry."

  (interactive "r")
  (save-excursion
    (let*
        ((inhibit-field-text-motion t)
         ;; state vars:
         strings prefix result depth new-depth out gone-out bullet beg
		 next done)

      (goto-char start)
      (beginning-of-line)
      ;; Goto initial topic, and register preceding stuff, if any:
      (if (> (allout-goto-prefix-doublechecked) start)
	  ;; First topic follows beginning point -- register preliminary stuff:
	  (setq result
                (list (list 0 "" nil
                            (buffer-substring-no-properties start
                                                            (1- (point)))))))
      (while (and (not done)
		  (not (eobp))		; Loop until we've covered the region.
		  (not (> (point) end)))
	(setq depth allout-recent-depth 	; Current topics depth,
	      bullet (allout-recent-bullet)	; ... bullet,
	      prefix (allout-recent-prefix)
	      beg (progn (allout-end-of-prefix t) (point))) ; and beginning.
	(setq done			; The boundary for the current topic:
	      (not (allout-next-visible-heading 1)))
	(setq new-depth allout-recent-depth)
	(setq gone-out out
	      out (< new-depth depth))
	(beginning-of-line)
	(setq next (point))
	(goto-char beg)
	(setq strings nil)
	(while (> next (point))		; Get all the exposed text in
	  (setq strings
		(cons (buffer-substring-no-properties
		       beg
					;To hidden text or end of line:
		       (progn
                         (end-of-line)
                         (allout-back-to-visible-text)))
		      strings))
	  (when (< (point) next)      ; Resume from after hid text, if any.
            (line-move 1)
            (beginning-of-line))
	  (setq beg (point)))
	;; Accumulate list for this topic:
	(setq strings (nreverse strings))
	(setq result
	      (cons
	       (if format
		   (let ((special (if (string-match
				       (regexp-quote bullet)
				       allout-distinctive-bullets-string)
				      bullet)))
		     (cond ((listp format)
			    (list depth
				  (if allout-flattened-numbering-abbreviation
				      (allout-stringify-flat-index format
								    gone-out)
				      (allout-stringify-flat-index-plain
				       format))
				  strings
				  special))
			   ((eq format 'indent)
			    (if special
				(list depth
				      (concat (make-string (1+ depth) ? )
					      (substring prefix -1))
				      strings)
			      (list depth
				    (make-string depth ? )
				    strings)))
			   (t (error "allout-listify-exposed: %s %s"
				     "invalid format" format))))
		 (list depth prefix strings))
		    result))
	;; Reassess format, if any:
	(if (and format (listp format))
	    (cond ((= new-depth depth)
		   (setq format (cons (1+ (car format))
					  (cdr format))))
		  ((> new-depth depth)	; descending -- assume by 1:
		   (setq format (cons 1 format)))
		  (t
					; Pop the residue:
		   (while (< new-depth depth)
		       (setq format (cdr format))
		       (setq depth (1- depth)))
					; And increment the current one:
		     (setq format
			   (cons (1+ (or (car format)
					 -1))
				 (cdr format)))))))
      ;; Put the list with first at front, to last at back:
      (nreverse result))))
;;;_   > allout-region-active-p ()
(defmacro allout-region-active-p ()
  (cond ((fboundp 'use-region-p) '(use-region-p))
        ((fboundp 'region-active-p) '(region-active-p))
        (t 'mark-active)))
;;_   > allout-process-exposed (&optional func from to frombuf
;;;					    tobuf format)
(defun allout-process-exposed (&optional func from to frombuf tobuf
					  format start-num)
  "Map function on exposed parts of current topic; results to another buffer.

All args are options; default values itemized below.

Apply FUNCTION to exposed portions FROM position TO position in buffer
FROMBUF to buffer TOBUF.  Sixth optional arg, FORMAT, designates an
alternate presentation form:

 `flat' -- Present prefix as numeric section.subsection..., starting with
	 section indicated by the START-NUM, innermost nesting first.
 X`flat-indented' -- Prefix is like `flat' for first topic at each
 X		   level, but subsequent topics have only leaf topic
 X		   number, padded with blanks to line up with first.
 `indent' (symbol) --  Convert header prefixes to all white space,
		       except for distinctive bullets.

Defaults:
  FUNCTION:	`allout-insert-listified'
  FROM:		region start, if region active, else start of buffer
  TO:		region end, if region active, else end of buffer
  FROMBUF:	current buffer
  TOBUF:	buffer name derived: \"*current-buffer-name exposed*\"
  FORMAT:	nil"

					; Resolve arguments,
					; defaulting if necessary:
  (if (not func) (setq func 'allout-insert-listified))
  (if (not (and from to))
      (if (allout-region-active-p)
	  (setq from (region-beginning) to (region-end))
	(setq from (point-min) to (point-max))))
  (if frombuf
      (if (not (bufferp frombuf))
	  ;; Specified but not a buffer -- get it:
	  (let ((got (get-buffer frombuf)))
	    (if (not got)
		(error (concat "allout-process-exposed: source buffer "
			       frombuf
			       " not found."))
	      (setq frombuf got))))
    ;; not specified -- default it:
    (setq frombuf (current-buffer)))
  (if tobuf
      (if (not (bufferp tobuf))
	  (setq tobuf (get-buffer-create tobuf)))
    ;; not specified -- default it:
    (setq tobuf (concat "*" (buffer-name frombuf) " exposed*")))
  (if (listp format)
      (nreverse format))

  (let* ((listified
	  (progn (set-buffer frombuf)
		 (allout-listify-exposed from to format))))
    (set-buffer tobuf)
    (mapc func listified)
    (pop-to-buffer tobuf)))

;;;_  - Copy exposed
;;;_   > allout-insert-listified (listified)
(defun allout-insert-listified (listified)
  "Insert contents of listified outline portion in current buffer.

LISTIFIED is a list representing each topic header and body:

 \`(depth prefix text)'

or \`(depth prefix text bullet-plus)'

If `bullet-plus' is specified, it is inserted just after the entire prefix."
  (setq listified (cdr listified))
  (let ((prefix (prog1
		    (car listified)
		  (setq listified (cdr listified))))
	(text (prog1
		  (car listified)
		(setq listified (cdr listified))))
	(bullet-plus (car listified)))
    (insert prefix)
    (if bullet-plus (insert (concat " " bullet-plus)))
    (while text
      (insert (car text))
      (if (setq text (cdr text))
	  (insert "\n")))
    (insert "\n")))
;;;_   > allout-copy-exposed-to-buffer (&optional arg tobuf format)
(defun allout-copy-exposed-to-buffer (&optional arg tobuf format)
  "Duplicate exposed portions of current outline to another buffer.

Other buffer has current buffers name with \" exposed\" appended to it.

With repeat count, copy the exposed parts of only the current topic.

Optional second arg TOBUF is target buffer name.

Optional third arg FORMAT, if non-nil, symbolically designates an
alternate presentation format for the outline:

 `flat'   - Convert topic header prefixes to numeric
	    section.subsection... identifiers.
 `indent' - Convert header prefixes to all white space, except for
	    distinctive bullets.
 `indent-flat' - The best of both - only the first of each level has
		 the full path, the rest have only the section number
		 of the leaf, preceded by the right amount of indentation."

  (interactive "P")
  (if (not tobuf)
      (setq tobuf (get-buffer-create (concat "*" (buffer-name) " exposed*"))))
  (let* ((start-pt (point))
	 (beg (if arg (allout-back-to-current-heading) (point-min)))
	 (end (if arg (allout-end-of-current-subtree) (point-max)))
	 (buf (current-buffer))
	 (start-list ()))
    (if (eq format 'flat)
	(setq format (if arg (save-excursion
				   (goto-char beg)
				   (allout-topic-flat-index))
			   '(1))))
    (with-current-buffer tobuf (erase-buffer))
    (allout-process-exposed 'allout-insert-listified
			     beg
			     end
			     (current-buffer)
			     tobuf
			     format start-list)
    (goto-char (point-min))
    (pop-to-buffer buf)
    (goto-char start-pt)))
;;;_   > allout-flatten-exposed-to-buffer (&optional arg tobuf)
(defun allout-flatten-exposed-to-buffer (&optional arg tobuf)
  "Present numeric outline of outline's exposed portions in another buffer.

The resulting outline is not compatible with outline mode -- use
`allout-copy-exposed-to-buffer' if you want that.

Use `allout-indented-exposed-to-buffer' for indented presentation.

With repeat count, copy the exposed portions of only current topic.

Other buffer has current buffer's name with \" exposed\" appended to
it, unless optional second arg TOBUF is specified, in which case it is
used verbatim."
  (interactive "P")
  (allout-copy-exposed-to-buffer arg tobuf 'flat))
;;;_   > allout-indented-exposed-to-buffer (&optional arg tobuf)
(defun allout-indented-exposed-to-buffer (&optional arg tobuf)
  "Present indented outline of outline's exposed portions in another buffer.

The resulting outline is not compatible with outline mode -- use
`allout-copy-exposed-to-buffer' if you want that.

Use `allout-flatten-exposed-to-buffer' for numeric sectional presentation.

With repeat count, copy the exposed portions of only current topic.

Other buffer has current buffer's name with \" exposed\" appended to
it, unless optional second arg TOBUF is specified, in which case it is
used verbatim."
  (interactive "P")
  (allout-copy-exposed-to-buffer arg tobuf 'indent))

;;;_  - LaTeX formatting
;;;_   > allout-latex-verb-quote (string &optional flow)
(defun allout-latex-verb-quote (string &optional flow)
  "Return copy of STRING for literal reproduction across LaTeX processing.
Expresses the original characters (including carriage returns) of the
string across LaTeX processing."
  (mapconcat (function
	      (lambda (char)
		(cond ((memq char '(?\\ ?$ ?% ?# ?& ?{ ?} ?_ ?^ ?- ?*))
		       (concat "\\char" (number-to-string char) "{}"))
		      ((= char ?\n) "\\\\")
		      (t (char-to-string char)))))
	     string
	     ""))
;;;_   > allout-latex-verbatim-quote-curr-line ()
(defun allout-latex-verbatim-quote-curr-line ()
  "Express line for exact (literal) representation across LaTeX processing.

Adjust line contents so it is unaltered (from the original line)
across LaTeX processing, within the context of a `verbatim'
environment.  Leaves point at the end of the line."
  (let ((inhibit-field-text-motion t))
    (beginning-of-line)
    (let ((beg (point))
          (end (point-at-eol)))
      (save-match-data
        (while (re-search-forward "\\\\"
  ;;"\\\\\\|\\{\\|\\}\\|\\_\\|\\$\\|\\\"\\|\\&\\|\\^\\|\\-\\|\\*\\|#"
                                  end	; bounded by end-of-line
                                  1) ; no matches, move to end & return nil
          (goto-char (match-beginning 2))
          (insert "\\")
          (setq end (1+ end))
          (goto-char (1+ (match-end 2))))))))
;;;_   > allout-insert-latex-header (buffer)
(defun allout-insert-latex-header (buffer)
  "Insert initial LaTeX commands at point in BUFFER."
  ;; Much of this is being derived from the stuff in appendix of E in
  ;; the TeXBook, pg 421.
  (set-buffer buffer)
  (let ((doc-style (format "\n\\documentstyle{%s}\n"
			   "report"))
	(page-numbering (if allout-number-pages
			    "\\pagestyle{empty}\n"
			  ""))
	(titlecmd (format "\\newcommand{\\titlecmd}[1]{{%s #1}}\n"
			  allout-title-style))
	(labelcmd (format "\\newcommand{\\labelcmd}[1]{{%s #1}}\n"
			  allout-label-style))
	(headlinecmd (format "\\newcommand{\\headlinecmd}[1]{{%s #1}}\n"
			     allout-head-line-style))
	(bodylinecmd (format "\\newcommand{\\bodylinecmd}[1]{{%s #1}}\n"
			     allout-body-line-style))
	(setlength (format "%s%s%s%s"
			   "\\newlength{\\stepsize}\n"
			   "\\setlength{\\stepsize}{"
			   allout-indent
			   "}\n"))
	(oneheadline (format "%s%s%s%s%s%s%s"
			     "\\newcommand{\\OneHeadLine}[3]{%\n"
			     "\\noindent%\n"
			     "\\hspace*{#2\\stepsize}%\n"
			     "\\labelcmd{#1}\\hspace*{.2cm}"
			     "\\headlinecmd{#3}\\\\["
			     allout-line-skip
			     "]\n}\n"))
	(onebodyline (format "%s%s%s%s%s%s"
			       "\\newcommand{\\OneBodyLine}[2]{%\n"
			       "\\noindent%\n"
			       "\\hspace*{#1\\stepsize}%\n"
			       "\\bodylinecmd{#2}\\\\["
			       allout-line-skip
			       "]\n}\n"))
	(begindoc "\\begin{document}\n\\begin{center}\n")
	(title (format "%s%s%s%s"
		       "\\titlecmd{"
		       (allout-latex-verb-quote (if allout-title
						(condition-case nil
						    (eval allout-title)
						  (error "<unnamed buffer>"))
					      "Unnamed Outline"))
		       "}\n"
		       "\\end{center}\n\n"))
	(hsize "\\hsize = 7.5 true in\n")
	(hoffset "\\hoffset = -1.5 true in\n")
	(vspace "\\vspace{.1cm}\n\n"))
    (insert (concat doc-style
		    page-numbering
		    titlecmd
		    labelcmd
		    headlinecmd
		    bodylinecmd
		    setlength
		    oneheadline
		    onebodyline
		    begindoc
		    title
		    hsize
		    hoffset
		    vspace)
	    )))
;;;_   > allout-insert-latex-trailer (buffer)
(defun allout-insert-latex-trailer (buffer)
  "Insert concluding LaTeX commands at point in BUFFER."
  (set-buffer buffer)
  (insert "\n\\end{document}\n"))
;;;_   > allout-latexify-one-item (depth prefix bullet text)
(defun allout-latexify-one-item (depth prefix bullet text)
  "Insert LaTeX commands for formatting one outline item.

Args are the topics numeric DEPTH, the header PREFIX lead string, the
BULLET string, and a list of TEXT strings for the body."
  (let* ((head-line (if text (car text)))
	 (body-lines (cdr text))
	 (curr-line)
	 body-content bop)
					; Do the head line:
    (insert (concat "\\OneHeadLine{\\verb\1 "
                    (allout-latex-verb-quote bullet)
                    "\1}{"
                    depth
                    "}{\\verb\1 "
                    (if head-line
                        (allout-latex-verb-quote head-line)
                      "")
                    "\1}\n"))
    (if (not body-lines)
	nil
      ;;(insert "\\beginlines\n")
      (insert "\\begin{verbatim}\n")
      (while body-lines
	(setq curr-line (car body-lines))
	(if (and (not body-content)
		 (not (string-match "^\\s-*$" curr-line)))
	    (setq body-content t))
					; Mangle any occurrences of
					; "\end{verbatim}" in text,
					; it's special:
	(if (and body-content
		 (setq bop (string-match "\\end{verbatim}" curr-line)))
	    (setq curr-line (concat (substring curr-line 0 bop)
				    ">"
				    (substring curr-line bop))))
	;;(insert "|" (car body-lines) "|")
	(insert curr-line)
	(allout-latex-verbatim-quote-curr-line)
	(insert "\n")
	(setq body-lines (cdr body-lines)))
      (if body-content
	  (setq body-content nil)
	(forward-char -1)
	(insert "\\ ")
	(forward-char 1))
      ;;(insert "\\endlines\n")
      (insert "\\end{verbatim}\n")
      )))
;;;_   > allout-latexify-exposed (arg &optional tobuf)
(defun allout-latexify-exposed (arg &optional tobuf)
  "Format current topics exposed portions to TOBUF for LaTeX processing.
TOBUF defaults to a buffer named the same as the current buffer, but
with \"*\" prepended and \" latex-formed*\" appended.

With repeat count, copy the exposed portions of entire buffer."

  (interactive "P")
  (if (not tobuf)
      (setq tobuf
	    (get-buffer-create (concat "*" (buffer-name) " latexified*"))))
  (let* ((start-pt (point))
	 (beg (if arg (point-min) (allout-back-to-current-heading)))
	 (end (if arg (point-max) (allout-end-of-current-subtree)))
	 (buf (current-buffer)))
    (set-buffer tobuf)
    (erase-buffer)
    (allout-insert-latex-header tobuf)
    (goto-char (point-max))
    (allout-process-exposed 'allout-latexify-one-item
			     beg
			     end
			     buf
			     tobuf)
    (goto-char (point-max))
    (allout-insert-latex-trailer tobuf)
    (goto-char (point-min))
    (pop-to-buffer buf)
    (goto-char start-pt)))

;;;_ #8 Encryption
;;;_  > allout-toggle-current-subtree-encryption (&optional keymode-cue)
(defun allout-toggle-current-subtree-encryption (&optional keymode-cue)
  "Encrypt clear or decrypt encoded topic text.

Allout uses Emacs 'epg' library to perform encryption.  Symmetric
and keypair encryption are supported.  All encryption is ascii
armored.

Entry encryption defaults to symmetric key mode unless keypair
recipients are associated with the file (see
`epa-file-encrypt-to') or the function is invoked with a
\(KEYMODE-CUE) universal argument greater than 1.

When encrypting, KEYMODE-CUE universal argument greater than 1
causes prompting for recipients for public-key keypair
encryption.  Selecting no recipients results in symmetric key
encryption.

Further, encrypting with a KEYMODE-CUE universal argument greater
than 4 - eg, preceded by a doubled Ctrl-U - causes association of
the specified recipients with the file, replacing those currently
associated with it.  This can be used to dissociate any
recipients with the file, by selecting no recipients in the
dialog.

Encrypted topic's bullets are set to a `~' to signal that the
contents of the topic (body and subtopics, but not heading) is
pending encryption or encrypted.  `*' asterisk immediately after
the bullet signals that the body is encrypted, its absence means
the topic is meant to be encrypted but is not currently.  When a
file with topics pending encryption is saved, topics pending
encryption are encrypted.  See `allout-encrypt-unencrypted-on-saves'
for auto-encryption specifics.

\*NOTE WELL* that automatic encryption that happens during saves will
default to symmetric encryption -- you must deliberately (re)encrypt key-pair
encrypted topics if you want them to continue to use the key-pair cipher.

Level-one topics, with prefix consisting solely of an `*' asterisk, cannot be
encrypted.  If you want to encrypt the contents of a top-level topic, use
\\[allout-shift-in] to increase its depth."
  (interactive "P")
  (save-excursion
    (allout-back-to-current-heading)
    (allout-toggle-subtree-encryption keymode-cue)))
;;;_  > allout-toggle-subtree-encryption (&optional keymode-cue)
(defun allout-toggle-subtree-encryption (&optional keymode-cue)
  "Encrypt clear text or decrypt encoded topic contents (body and subtopics.)

Entry encryption defaults to symmetric key mode unless keypair
recipients are associated with the file (see
`epa-file-encrypt-to') or the function is invoked with a
\(KEYMODE-CUE) universal argument greater than 1.

When encrypting, KEYMODE-CUE universal argument greater than 1
causes prompting for recipients for public-key keypair
encryption.  Selecting no recipients results in symmetric key
encryption.

Further, encrypting with a KEYMODE-CUE universal argument greater
than 4 - eg, preceded by a doubled Ctrl-U - causes association of
the specified recipients with the file, replacing those currently
associated with it.  This can be used to dissociate any
recipients with the file, by selecting no recipients in the
dialog.

Encryption and decryption uses the Emacs 'epg' library.

Encrypted text will be ascii-armored.

See `allout-toggle-current-subtree-encryption' for more details."

  (interactive "P")
  (save-excursion
    (allout-end-of-prefix t)

    (if (= allout-recent-depth 1)
        (error (concat "Cannot encrypt or decrypt level 1 topics -"
                       " shift it in to make it encryptable")))

    (let* ((allout-buffer (current-buffer))
           ;; for use with allout-auto-save-temporarily-disabled, if necessary:
           (was-buffer-saved-size buffer-saved-size)
           ;; Assess location:
           (bullet-pos allout-recent-prefix-beginning)
           (after-bullet-pos (point))
           (was-encrypted
            (progn (if (= (point-max) after-bullet-pos)
                       (error "no body to encrypt"))
                   (allout-encrypted-topic-p)))
           (was-collapsed (if (not (search-forward "\n" nil t))
                              nil
                            (backward-char 1)
                            (allout-hidden-p)))
           (subtree-beg (1+ (point)))
           (subtree-end (allout-end-of-subtree))
           (subject-text (buffer-substring-no-properties subtree-beg
                                                         subtree-end))
           (subtree-end-char (char-after (1- subtree-end)))
           (subtree-trailing-char (char-after subtree-end))
           ;; kluge -- result-text needs to be nil, but we also want to
           ;;         check for the error condition
           (result-text (if (or (string= "" subject-text)
                                (string= "\n" subject-text))
                            (error "No topic contents to %scrypt"
                                   (if was-encrypted "de" "en"))
                          nil))
           ;; Assess key parameters:
           (was-coding-system buffer-file-coding-system))

      (when (not was-encrypted)
        ;; ensure that non-ascii chars pending encryption are noticed before
        ;; they're encrypted, so the coding system is set to accommodate
        ;; them.
        (setq buffer-file-coding-system
              (allout-select-safe-coding-system subtree-beg subtree-end))
        ;; if the coding system for the text being encrypted is different
        ;; than that prevailing, then there a real risk that the coding
        ;; system can't be noticed by emacs when the file is visited.  to
        ;; mitigate that, offer to preserve the coding system using a file
        ;; local variable.
        (if (and (not (equal buffer-file-coding-system
                             was-coding-system))
                 (yes-or-no-p
                  (format (concat "Register coding system %s as file local"
                                  " var?  Necessary when only encrypted text"
                                  " is in that coding system. ")
                          buffer-file-coding-system)))
            (allout-adjust-file-variable "buffer-file-coding-system"
                                         buffer-file-coding-system)))

      (setq result-text
            (allout-encrypt-string subject-text was-encrypted
                                   (current-buffer) keymode-cue))

       ;; Replace the subtree with the processed product.
      (allout-unprotected
       (progn
         (set-buffer allout-buffer)
         (delete-region subtree-beg subtree-end)
         (insert result-text)
         (if was-collapsed
             (allout-flag-region (1- subtree-beg) (point) t))
         ;; adjust trailing-blank-lines to preserve topic spacing:
         (if (not was-encrypted)
             (if (and (= subtree-end-char ?\n)
                      (= subtree-trailing-char ?\n))
                 (insert subtree-trailing-char)))
         ;; Ensure that the item has an encrypted-entry bullet:
         (if (not (string= (buffer-substring-no-properties
                            (1- after-bullet-pos) after-bullet-pos)
                           allout-topic-encryption-bullet))
             (progn (goto-char (1- after-bullet-pos))
                    (delete-char 1)
                    (insert allout-topic-encryption-bullet)))
         (if was-encrypted
             ;; Remove the is-encrypted bullet qualifier:
             (progn (goto-char after-bullet-pos)
                    (delete-char 1))
           ;; Add the is-encrypted bullet qualifier:
           (goto-char after-bullet-pos)
           (insert "*"))))

      ;; adjust buffer's auto-save eligibility:
      (if was-encrypted
          (allout-inhibit-auto-save-info-for-decryption was-buffer-saved-size)
        (allout-maybe-resume-auto-save-info-after-encryption))

      (run-hook-with-args 'allout-structure-added-hook
                          bullet-pos subtree-end))))
;;;_  > allout-encrypt-string (text decrypt allout-buffer keymode-cue
;;;                                 &optional rejected)
(defun allout-encrypt-string (text decrypt allout-buffer keymode-cue
                                   &optional rejected)
  "Encrypt or decrypt message TEXT.

Returns the resulting string, or nil if the transformation fails.

If DECRYPT is true (default false), then decrypt instead of encrypt.

ALLOUT-BUFFER identifies the buffer containing the text.

Entry encryption defaults to symmetric key mode unless keypair
recipients are associated with the file (see
`epa-file-encrypt-to') or the function is invoked with a
\(KEYMODE-CUE) universal argument greater than 1.

When encrypting, KEYMODE-CUE universal argument greater than 1
causes prompting for recipients for public-key keypair
encryption.  Selecting no recipients results in symmetric key
encryption.

Further, encrypting with a KEYMODE-CUE universal argument greater
than 4 - eg, preceded by a doubled Ctrl-U - causes association of
the specified recipients with the file, replacing those currently
associated with it.  This can be used to dissociate any
recipients with the file, by selecting no recipients in the
dialog.

Optional REJECTED is for internal use, to convey the number of
rejections due to matches against
`allout-encryption-ciphertext-rejection-regexps', as limited by
`allout-encryption-ciphertext-rejection-ceiling'.

NOTE: A few GnuPG v2 versions improperly preserve incorrect
symmetric decryption keys, preventing entry of the correct key on
subsequent decryption attempts until the cache times-out.  That
can take several minutes.  (Decryption of other entries is not
affected.)  Upgrade your EasyPG version, if you can, and you can
deliberately clear your gpg-agent's cache by sending it a '-HUP'
signal."

  (require 'epg)
  (require 'epa)

  (let* ((epg-context (let* ((context (epg-make-context nil t)))
                        (epg-context-set-passphrase-callback
                         context #'epa-passphrase-callback-function)
                        context))

         (encoding (with-current-buffer allout-buffer
                     buffer-file-coding-system))
         (multibyte (with-current-buffer allout-buffer
                      enable-multibyte-characters))
         ;; "sanitization" avoids encryption results that are outline structure.
         (sani-regexps 'allout-encryption-plaintext-sanitization-regexps)
         (strip-plaintext-regexps (if (not decrypt)
                                      (allout-get-configvar-values
                                       sani-regexps)))
         (rejection-regexps 'allout-encryption-ciphertext-rejection-regexps)
         (reject-ciphertext-regexps (if (not decrypt)
                                        (allout-get-configvar-values
                                         rejection-regexps)))
         (rejected (or rejected 0))
         (rejections-left (- allout-encryption-ciphertext-rejection-ceiling
                             rejected))
         (keypair-mode (cond (decrypt 'decrypting)
                             ((<= (prefix-numeric-value keymode-cue) 1)
                              'default)
                             ((<= (prefix-numeric-value keymode-cue) 4)
                              'prompt)
                             ((> (prefix-numeric-value keymode-cue) 4)
                              'prompt-save)))
         (keypair-message (concat "Select encryption recipients.\n"
                                  "Symmetric encryption is done if no"
                                  " recipients are selected.  "))
         (encrypt-to (and (boundp 'epa-file-encrypt-to) epa-file-encrypt-to))
         recipients
         massaged-text
         result-text
         )

    ;; Massage the subject text for encoding and filtering.
    (with-temp-buffer
      (insert text)
      ;; convey the text characteristics of the original buffer:
      (set-buffer-multibyte multibyte)
      (when encoding
        (set-buffer-file-coding-system encoding)
        (if (not decrypt)
            (encode-coding-region (point-min) (point-max) encoding)))

      ;; remove sanitization regexps matches before encrypting:
      (when (and strip-plaintext-regexps (not decrypt))
        (dolist (re strip-plaintext-regexps)
          (let ((re (if (listp re) (car re) re))
                (replacement (if (listp re) (cadr re) "")))
            (goto-char (point-min))
            (save-match-data
              (while (re-search-forward re nil t)
                (replace-match replacement nil nil))))))
      (setq massaged-text (buffer-substring-no-properties (point-min)
                                                          (point-max))))
    ;; determine key mode and, if keypair, recipients:
    (setq recipients
          (case keypair-mode

            (decrypting nil)

            (default (if encrypt-to (epg-list-keys epg-context encrypt-to)))

            ((prompt prompt-save)
             (save-window-excursion
               (epa-select-keys epg-context keypair-message)))))

    (setq result-text
          (if decrypt
              (condition-case err
                  (epg-decrypt-string epg-context
                                      (encode-coding-string massaged-text
                                                            (or encoding 'utf-8)))
                (epg-error
                 (signal 'egp-error
                         (cons (concat (cadr err) " - gpg version problem?")
                               (cddr err)))))
            (replace-regexp-in-string "\n$" ""
             (epg-encrypt-string epg-context
                                 (encode-coding-string massaged-text
                                                       (or encoding 'utf-8))
                                 recipients))))

    ;; validate result -- non-empty
    (if (not result-text)
        (error "%scryption failed." (if decrypt "De" "En")))


    (when (eq keypair-mode 'prompt-save)
      ;; set epa-file-encrypt-to in the buffer:
      (setq epa-file-encrypt-to (mapcar (lambda (key)
                                          (epg-user-id-string
                                           (car (epg-key-user-id-list key))))
                                        recipients))
      ;; change the file variable:
      (allout-adjust-file-variable "epa-file-encrypt-to" epa-file-encrypt-to))

    (cond
     ;; Retry (within limit) if ciphertext contains rejections:
     ((and (not decrypt)
           ;; Check for disqualification of this ciphertext:
           (let ((regexps reject-ciphertext-regexps)
                 reject-it)
             (while (and regexps (not reject-it))
               (setq reject-it (string-match (car regexps) result-text))
               (pop regexps))
             reject-it))
      (setq rejections-left (1- rejections-left))
      (if (<= rejections-left 0)
          (error (concat "Ciphertext rejected too many times"
                         " (%s), per `%s'")
                 allout-encryption-ciphertext-rejection-ceiling
                 'allout-encryption-ciphertext-rejection-regexps)
        ;; try again (gpg-agent may have the key cached):
        (allout-encrypt-string text decrypt allout-buffer keypair-mode
                               (1+ rejected))))

     ;; Barf if encryption yields extraordinary control chars:
     ((and (not decrypt)
           (string-match "[\C-a\C-k\C-o-\C-z\C-@]"
                         result-text))
      (error (concat "Encryption produced non-armored text, which"
                     "conflicts with allout mode -- reconfigure!")))
     (t result-text))))
;;;_  > allout-inhibit-auto-save-info-for-decryption
(defun allout-inhibit-auto-save-info-for-decryption (was-buffer-saved-size)
  "Temporarily prevent auto-saves in this buffer when an item is decrypted.

WAS-BUFFER-SAVED-SIZE is the value of `buffer-saved-size' *before*
the decryption."
  (when (not (or (= buffer-saved-size -1) (= was-buffer-saved-size -1)))
    (setq allout-auto-save-temporarily-disabled was-buffer-saved-size
          buffer-saved-size -1)))
;;;_  > allout-maybe-resume-auto-save-info-after-encryption ()
(defun allout-maybe-resume-auto-save-info-after-encryption ()
  "Restore auto-save info, *if* there are no topics pending encryption."
  (when (and allout-auto-save-temporarily-disabled
             (= buffer-saved-size -1)
             (save-excursion
               (save-restriction
                 (widen)
                 (goto-char (point-min))
                 (not (allout-next-topic-pending-encryption)))))
    (setq buffer-saved-size allout-auto-save-temporarily-disabled
          allout-auto-save-temporarily-disabled nil)))

;;;_  > allout-encrypted-topic-p ()
(defun allout-encrypted-topic-p ()
  "True if the current topic is encryptable and encrypted."
  (save-excursion
    (allout-end-of-prefix t)
    (and (string= (buffer-substring-no-properties (1- (point)) (point))
                  allout-topic-encryption-bullet)
         (save-match-data (looking-at "\\*")))
    )
  )
;;;_  > allout-next-topic-pending-encryption ()
(defun allout-next-topic-pending-encryption ()
  "Return the point of the next topic pending encryption, or nil if none.

Such a topic has the `allout-topic-encryption-bullet' without an
immediately following '*' that would mark the topic as being encrypted.
It must also have content."
  (let (done got content-beg)
    (save-match-data
      (while (not done)

        (if (not (re-search-forward
                  (format "\\(\\`\\|\n\\)%s *%s[^*]"
                          (regexp-quote allout-header-prefix)
                          (regexp-quote allout-topic-encryption-bullet))
                  nil t))
            (setq got nil
                  done t)
          (goto-char (setq got (match-beginning 0)))
          (if (save-match-data (looking-at "\n"))
              (forward-char 1))
          (setq got (point)))

        (cond ((not got)
               (setq done t))

              ((not (search-forward "\n"))
               (setq got nil
                     done t))

              ((eobp)
               (setq got nil
                     done t))

              (t
               (setq content-beg (point))
               (backward-char 1)
               (allout-end-of-subtree)
               (if (<= (point) content-beg)
                   ;; Continue looking
                   (setq got nil)
                 ;; Got it!
                 (setq done t)))
              )
        )
      (if got
          (goto-char got))
      )
    )
  )
;;;_  > allout-encrypt-decrypted ()
(defun allout-encrypt-decrypted ()
  "Encrypt topics pending encryption except those containing exemption point.

If a topic that is currently being edited was encrypted, we return a list
containing the location of the topic and the location of the cursor just
before the topic was encrypted.  This can be used, eg, to decrypt the topic
and exactly resituate the cursor if this is being done as part of a file
save.  See `allout-encrypt-unencrypted-on-saves' for more info."

  (interactive "p")
  (save-match-data
    (save-excursion
      (let* ((current-mark (point-marker))
             (current-mark-position (marker-position current-mark))
             was-modified
             bo-subtree
             editing-topic editing-point)
        (goto-char (point-min))
        (while (allout-next-topic-pending-encryption)
          (setq was-modified (buffer-modified-p))
          (when (save-excursion
                  (and (boundp 'allout-encrypt-unencrypted-on-saves)
                       allout-encrypt-unencrypted-on-saves
                       (setq bo-subtree (re-search-forward "$"))
                       (not (allout-hidden-p))
                       (>= current-mark (point))
                       (allout-end-of-current-subtree)
                       (<= current-mark (point))))
            (setq editing-topic (point)
                  ;; we had to wait for this 'til now so prior topics are
                  ;; encrypted, any relevant text shifts are in place:
                  editing-point (- current-mark-position
                                   (count-trailing-whitespace-region
                                    bo-subtree current-mark-position))))
          (allout-toggle-subtree-encryption)
          (if (not was-modified)
              (set-buffer-modified-p nil))
          )
        (if (not was-modified)
            (set-buffer-modified-p nil))
        (if editing-topic (list editing-topic editing-point))
        )
      )
    )
  )

;;;_ #9 miscellaneous
;;;_  : Mode:
;;;_   > outlineify-sticky ()
;; outlinify-sticky is correct spelling; provide this alias for sticklers:
;;;###autoload
(defalias 'outlinify-sticky 'outlineify-sticky)
;;;###autoload
(defun outlineify-sticky (&optional arg)
  "Activate outline mode and establish file var so it is started subsequently.

See `allout-layout' and customization of `allout-auto-activation'
for details on preparing Emacs for automatic allout activation."

  (interactive "P")

  (if (allout-mode-p) (allout-mode))    ; deactivate so we can re-activate...
  (allout-mode)

  (save-excursion
    (goto-char (point-min))
    (if (allout-goto-prefix)
	t
      (allout-open-topic 2)
      (insert (concat "Dummy outline topic header -- see"
                      "`allout-mode' docstring: `^Hm'."))
      (allout-adjust-file-variable
       "allout-layout" (or allout-layout '(-1 : 0))))))
;;;_   > allout-file-vars-section-data ()
(defun allout-file-vars-section-data ()
  "Return data identifying the file-vars section, or nil if none.

Returns a list of the form (BEGINNING-POINT PREFIX-STRING SUFFIX-STRING)."
  ;; minimally gleaned from emacs 21.4 files.el hack-local-variables function.
  (let (beg prefix suffix)
    (save-excursion
      (goto-char (point-max))
      (search-backward "\n\^L" (max (- (point-max) 3000) (point-min)) 'move)
      (if (let ((case-fold-search t))
	    (not (search-forward "Local Variables:" nil t)))
          nil
        (setq beg (- (point) 16))
        (setq suffix (buffer-substring-no-properties
                      (point)
                      (progn (if (search-forward "\n" nil t)
                                 (forward-char -1))
                             (point))))
        (setq prefix (buffer-substring-no-properties
                      (progn (if (search-backward "\n" nil t)
                                 (forward-char 1))
                             (point))
                      beg))
        (list beg prefix suffix))
      )
    )
  )
;;;_   > allout-adjust-file-variable (varname value)
(defun allout-adjust-file-variable (varname value)
  "Adjust the setting of an Emacs file variable named VARNAME to VALUE.

This activity is inhibited if either `enable-local-variables' or
`allout-enable-file-variable-adjustment' are nil.

When enabled, an entry for the variable is created if not already present,
or changed if established with a different value.  The section for the file
variables, itself, is created if not already present.  When created, the
section lines (including the section line) exist as second-level topics in
a top-level topic at the end of the file.

`enable-local-variables' must be true for any of this to happen."
  (if (not (and enable-local-variables
                allout-enable-file-variable-adjustment))
      nil
    (save-excursion
      (let ((inhibit-field-text-motion t)
            (section-data (allout-file-vars-section-data))
            beg prefix suffix)
        (if section-data
            (setq beg (car section-data)
                  prefix (cadr section-data)
                  suffix (car (cddr section-data)))
          ;; create the section
          (goto-char (point-max))
          (open-line 1)
          (allout-open-topic 0)
          (end-of-line)
          (insert "Local emacs vars.\n")
          (allout-open-topic 1)
          (setq beg (point)
                suffix ""
                prefix (buffer-substring-no-properties (progn
                                                         (beginning-of-line)
                                                         (point))
                                                       beg))
          (goto-char beg)
          (insert "Local variables:\n")
          (allout-open-topic 0)
          (insert "End:\n")
          )
        ;; look for existing entry or create one, leaving point for insertion
        ;; of new value:
        (goto-char beg)
        (allout-show-to-offshoot)
        (if (search-forward (concat "\n" prefix varname ":") nil t)
            (let* ((value-beg (point))
                   (line-end (progn (if (search-forward "\n" nil t)
                                        (forward-char -1))
                                    (point)))
                   (value-end (- line-end (length suffix))))
              (if (> value-end value-beg)
                  (delete-region value-beg value-end)))
          (end-of-line)
          (open-line 1)
          (forward-line 1)
          (insert (concat prefix varname ":")))
        (insert (format " %S%s" value suffix))
        )
      )
    )
  )
;;;_   > allout-get-configvar-values (varname)
(defun allout-get-configvar-values (configvar-name)
  "Return a list of values of the symbols in list bound to CONFIGVAR-NAME.

The user is prompted for removal of symbols that are unbound, and they
otherwise are ignored.

CONFIGVAR-NAME should be the name of the configuration variable,
not its value."

  (let ((configvar-value (symbol-value configvar-name))
        got)
    (dolist (sym configvar-value)
      (if (not (boundp sym))
          (if (yes-or-no-p (format "%s entry `%s' is unbound -- remove it? "
                                   configvar-name sym))
              (delq sym (symbol-value configvar-name)))
        (push (symbol-value sym) got)))
    (reverse got)))
;;;_  : Topics:
;;;_   > allout-mark-topic ()
(defun allout-mark-topic ()
  "Put the region around topic currently containing point."
  (interactive)
  (let ((inhibit-field-text-motion t))
    (beginning-of-line))
  (allout-goto-prefix-doublechecked)
  (push-mark (point))
  (allout-end-of-current-subtree)
  (exchange-point-and-mark))
;;;_  : UI:
;;;_   > solicit-char-in-string (prompt string &optional do-defaulting)
(defun solicit-char-in-string (prompt string &optional do-defaulting)
  "Solicit (with first arg PROMPT) choice of a character from string STRING.

Optional arg DO-DEFAULTING indicates to accept empty input (CR)."

  (let ((new-prompt prompt)
        got)

    (while (not got)
      (message "%s" new-prompt)

      ;; We do our own reading here, so we can circumvent, eg, special
      ;; treatment for `?' character.  (Oughta use minibuffer keymap instead.)
      (setq got
            (char-to-string (let ((cursor-in-echo-area nil)) (read-char))))

      (setq got
	    (cond ((string-match (regexp-quote got) string) got)
		  ((and do-defaulting (string= got "\r"))
		   ;; Return empty string to default:
		   "")
		  ((string= got "\C-g") (signal 'quit nil))
		  (t
		   (setq new-prompt (concat prompt
					    got
					    " ...pick from: "
					    string
					    ""))
		   nil))))
      ;; got something out of loop -- return it:
      got)
  )
;;;_  : Strings:
;;;_   > regexp-sans-escapes (string)
(defun regexp-sans-escapes (regexp &optional successive-backslashes)
  "Return a copy of REGEXP with all character escapes stripped out.

Representations of actual backslashes -- '\\\\\\\\' -- are left as a
single backslash.

Optional arg SUCCESSIVE-BACKSLASHES is used internally for recursion."

  (if (string= regexp "")
      ""
    ;; Set successive-backslashes to number if current char is
    ;; backslash, or else to nil:
    (setq successive-backslashes
	  (if (= (aref regexp 0) ?\\)
	      (if successive-backslashes (1+ successive-backslashes) 1)
	    nil))
    (if (or (not successive-backslashes) (= 2 successive-backslashes))
	;; Include first char:
	(concat (substring regexp 0 1)
		(regexp-sans-escapes (substring regexp 1)))
      ;; Exclude first char, but maintain count:
      (regexp-sans-escapes (substring regexp 1) successive-backslashes))))
;;;_   > count-trailing-whitespace-region (beg end)
(defun count-trailing-whitespace-region (beg end)
  "Return number of trailing whitespace chars between BEG and END.

If BEG is bigger than END we return 0."
  (if (> beg end)
      0
    (save-match-data
      (save-excursion
        (goto-char beg)
        (let ((count 0))
          (while (re-search-forward "[ 	][ 	]*$" end t)
            (goto-char (1+ (match-beginning 2)))
            (setq count (1+ count)))
          count)))))
;;;_   > allout-format-quote (string)
(defun allout-format-quote (string)
  "Return a copy of string with all \"%\" characters doubled."
  (apply 'concat
         (mapcar (lambda (char) (if (= char ?%) "%%" (char-to-string char)))
                 string)))
;;;_  : lists
;;;_   > allout-flatten (list)
(defun allout-flatten (list)
  "Return a list of all atoms in list."
  ;; classic.
  (cond ((null list) nil)
        ((atom (car list)) (cons (car list) (allout-flatten (cdr list))))
        (t (append (allout-flatten (car list)) (allout-flatten (cdr list))))))
;;;_  : Compatibility:
;;;_   : xemacs undo-in-progress provision:
(unless (boundp 'undo-in-progress)
  (defvar undo-in-progress nil
    "Placeholder defvar for XEmacs compatibility from allout.el.")
  (defadvice undo-more (around allout activate)
    ;; This defadvice used only in emacs that lack undo-in-progress, eg xemacs.
    (let ((undo-in-progress t)) ad-do-it)))

;;;_   > allout-mark-marker to accommodate divergent emacsen:
(defun allout-mark-marker (&optional force buffer)
  "Accommodate the different signature for `mark-marker' across Emacsen.

XEmacs takes two optional args, while Emacs does not,
so pass them along when appropriate."
  (if (featurep 'xemacs)
      (apply 'mark-marker force buffer)
    (mark-marker)))
;;;_   > subst-char-in-string if necessary
(if (not (fboundp 'subst-char-in-string))
    (defun subst-char-in-string (fromchar tochar string &optional inplace)
      "Replace FROMCHAR with TOCHAR in STRING each time it occurs.
Unless optional argument INPLACE is non-nil, return a new string."
      (let ((i (length string))
            (newstr (if inplace string (copy-sequence string))))
        (while (> i 0)
          (setq i (1- i))
          (if (eq (aref newstr i) fromchar)
              (aset newstr i tochar)))
        newstr)))
;;;_   > wholenump if necessary
(if (not (fboundp 'wholenump))
    (defalias 'wholenump 'natnump))
;;;_   > remove-overlays if necessary
(if (not (fboundp 'remove-overlays))
    (defun remove-overlays (&optional beg end name val)
      "Clear BEG and END of overlays whose property NAME has value VAL.
Overlays might be moved and/or split.
BEG and END default respectively to the beginning and end of buffer."
      (unless beg (setq beg (point-min)))
      (unless end (setq end (point-max)))
      (if (< end beg)
          (setq beg (prog1 end (setq end beg))))
      (save-excursion
        (dolist (o (overlays-in beg end))
          (when (eq (overlay-get o name) val)
            ;; Either push this overlay outside beg...end
            ;; or split it to exclude beg...end
            ;; or delete it entirely (if it is contained in beg...end).
            (if (< (overlay-start o) beg)
                (if (> (overlay-end o) end)
                    (progn
                      (move-overlay (copy-overlay o)
                                    (overlay-start o) beg)
                      (move-overlay o end (overlay-end o)))
                  (move-overlay o (overlay-start o) beg))
              (if (> (overlay-end o) end)
                  (move-overlay o end (overlay-end o))
                (delete-overlay o)))))))
  )
;;;_   > copy-overlay if necessary -- xemacs ~ 21.4
(if (not (fboundp 'copy-overlay))
    (defun copy-overlay (o)
      "Return a copy of overlay O."
      (let ((o1 (make-overlay (overlay-start o) (overlay-end o)
                              ;; FIXME: there's no easy way to find the
                              ;; insertion-type of the two markers.
                              (overlay-buffer o)))
            (props (overlay-properties o)))
        (while props
          (overlay-put o1 (pop props) (pop props)))
        o1)))
;;;_   > add-to-invisibility-spec if necessary -- xemacs ~ 21.4
(if (not (fboundp 'add-to-invisibility-spec))
    (defun add-to-invisibility-spec (element)
      "Add ELEMENT to `buffer-invisibility-spec'.
See documentation for `buffer-invisibility-spec' for the kind of elements
that can be added."
      (if (eq buffer-invisibility-spec t)
          (setq buffer-invisibility-spec (list t)))
      (setq buffer-invisibility-spec
            (cons element buffer-invisibility-spec))))
;;;_   > remove-from-invisibility-spec if necessary -- xemacs ~ 21.4
(if (not (fboundp 'remove-from-invisibility-spec))
    (defun remove-from-invisibility-spec (element)
      "Remove ELEMENT from `buffer-invisibility-spec'."
      (if (consp buffer-invisibility-spec)
          (setq buffer-invisibility-spec (delete element
                                                 buffer-invisibility-spec)))))
;;;_   > move-beginning-of-line if necessary -- older emacs, xemacs
(if (not (fboundp 'move-beginning-of-line))
    (defun move-beginning-of-line (arg)
      "Move point to beginning of current line as displayed.
\(This disregards invisible newlines such as those
which are part of the text that an image rests on.)

With argument ARG not nil or 1, move forward ARG - 1 lines first.
If point reaches the beginning or end of buffer, it stops there.
To ignore intangibility, bind `inhibit-point-motion-hooks' to t."
      (interactive "p")
      (or arg (setq arg 1))
      (if (/= arg 1)
          (condition-case nil (line-move (1- arg)) (error nil)))

      ;; Move to beginning-of-line, ignoring fields and invisible text.
      (skip-chars-backward "^\n")
      (while (and (not (bobp))
                  (let ((prop
                          (get-char-property (1- (point)) 'invisible)))
                    (if (eq buffer-invisibility-spec t)
                        prop
                      (or (memq prop buffer-invisibility-spec)
                          (assq prop buffer-invisibility-spec)))))
        (goto-char (if (featurep 'xemacs)
                       (previous-property-change (point))
                     (previous-char-property-change (point))))
        (skip-chars-backward "^\n"))
      (vertical-motion 0))
)
;;;_   > move-end-of-line if necessary -- Emacs < 22.1, xemacs
(if (not (fboundp 'move-end-of-line))
    (defun move-end-of-line (arg)
      "Move point to end of current line as displayed.
\(This disregards invisible newlines such as those
which are part of the text that an image rests on.)

With argument ARG not nil or 1, move forward ARG - 1 lines first.
If point reaches the beginning or end of buffer, it stops there.
To ignore intangibility, bind `inhibit-point-motion-hooks' to t."
      (interactive "p")
      (or arg (setq arg 1))
      (let (done)
        (while (not done)
          (let ((newpos
                 (save-excursion
                   (let ((goal-column 0))
                     (and (condition-case nil
                              (or (line-move arg) t)
                            (error nil))
                          (not (bobp))
                          (progn
                            (while
                                (and
                                 (not (bobp))
                                 (let ((prop
                                        (get-char-property (1- (point))
                                                           'invisible)))
                                   (if (eq buffer-invisibility-spec t)
                                       prop
                                     (or (memq prop
                                               buffer-invisibility-spec)
                                         (assq prop
                                               buffer-invisibility-spec)))))
                              (goto-char
                               (previous-char-property-change (point))))
                            (backward-char 1)))
                     (point)))))
            (goto-char newpos)
            (if (and (> (point) newpos)
                     (eq (preceding-char) ?\n))
                (backward-char 1)
              (if (and (> (point) newpos) (not (eobp))
                       (not (eq (following-char) ?\n)))
                  ;; If we skipped something intangible
                  ;; and now we're not really at eol,
                  ;; keep going.
                  (setq arg 1)
                (setq done t)))))))
  )
;;;_   > allout-next-single-char-property-change -- alias unless lacking
(defalias 'allout-next-single-char-property-change
  (if (fboundp 'next-single-char-property-change)
      'next-single-char-property-change
    'next-single-property-change)
  ;; No docstring because xemacs defalias doesn't support it.
  )
;;;_   > allout-previous-single-char-property-change -- alias unless lacking
(defalias 'allout-previous-single-char-property-change
  (if (fboundp 'previous-single-char-property-change)
      'previous-single-char-property-change
    'previous-single-property-change)
  ;; No docstring because xemacs defalias doesn't support it.
  )
;;;_   > allout-select-safe-coding-system
(defalias 'allout-select-safe-coding-system
  (if (fboundp 'select-safe-coding-system)
      'select-safe-coding-system
    'detect-coding-region)
 )
;;;_   > allout-substring-no-properties
;; define as alias first, so byte compiler is happy.
(defalias 'allout-substring-no-properties 'substring-no-properties)
;; then supplant with definition if underlying alias absent.
(if (not (fboundp 'substring-no-properties))
  (defun allout-substring-no-properties (string &optional start end)
    (substring string (or start 0) end))
  )

;;;_ #10 Unfinished
;;;_  > allout-bullet-isearch (&optional bullet)
(defun allout-bullet-isearch (&optional bullet)
  "Isearch (regexp) for topic with bullet BULLET."
  (interactive)
  (if (not bullet)
      (setq bullet (solicit-char-in-string
		    "ISearch for topic with bullet: "
		    (regexp-sans-escapes allout-bullets-string))))

  (let ((isearch-regexp t)
	(isearch-string (concat "^"
				allout-header-prefix
				"[ \t]*"
				bullet)))
    (isearch-repeat 'forward)
    (isearch-mode t)))

;;;_ #11 Unit tests -- this should be last item before "Provide"
;;;_  > allout-run-unit-tests ()
(defun allout-run-unit-tests ()
  "Run the various allout unit tests."
  (message "Running allout tests...")
  (allout-test-resumptions)
  (message "Running allout tests...  Done.")
  (sit-for .5))
;;;_  : test resumptions:
;;;_   > allout-tests-obliterate-variable (name)
(defun allout-tests-obliterate-variable (name)
  "Completely unbind variable with NAME."
  (if (local-variable-p name (current-buffer)) (kill-local-variable name))
  (while (boundp name) (makunbound name)))
;;;_   > allout-test-resumptions ()
(defvar allout-tests-globally-unbound nil
  "Fodder for allout resumptions tests -- defvar just for byte compiler.")
(defvar allout-tests-globally-true nil
  "Fodder for allout resumptions tests -- defvar just for byte compiler.")
(defvar allout-tests-locally-true nil
  "Fodder for allout resumptions tests -- defvar just for byte compiler.")
(defun allout-test-resumptions ()
  "Exercise allout resumptions."
  ;; for each resumption case, we also test that the right local/global
  ;; scopes are affected during resumption effects:

  ;; ensure that previously unbound variables return to the unbound state.
  (with-temp-buffer
    (allout-tests-obliterate-variable 'allout-tests-globally-unbound)
    (allout-add-resumptions '(allout-tests-globally-unbound t))
    (assert (not (default-boundp 'allout-tests-globally-unbound)))
    (assert (local-variable-p 'allout-tests-globally-unbound (current-buffer)))
    (assert (boundp 'allout-tests-globally-unbound))
    (assert (equal allout-tests-globally-unbound t))
    (allout-do-resumptions)
    (assert (not (local-variable-p 'allout-tests-globally-unbound
                                   (current-buffer))))
    (assert (not (boundp 'allout-tests-globally-unbound))))

  ;; ensure that variable with prior global value is resumed
  (with-temp-buffer
    (allout-tests-obliterate-variable 'allout-tests-globally-true)
    (setq allout-tests-globally-true t)
    (allout-add-resumptions '(allout-tests-globally-true nil))
    (assert (equal (default-value 'allout-tests-globally-true) t))
    (assert (local-variable-p 'allout-tests-globally-true (current-buffer)))
    (assert (equal allout-tests-globally-true nil))
    (allout-do-resumptions)
    (assert (not (local-variable-p 'allout-tests-globally-true
                                   (current-buffer))))
    (assert (boundp 'allout-tests-globally-true))
    (assert (equal allout-tests-globally-true t)))

  ;; ensure that prior local value is resumed
  (with-temp-buffer
    (allout-tests-obliterate-variable 'allout-tests-locally-true)
    (set (make-local-variable 'allout-tests-locally-true) t)
    (assert (not (default-boundp 'allout-tests-locally-true))
            nil (concat "Test setup mistake -- variable supposed to"
                        " not have global binding, but it does."))
    (assert (local-variable-p 'allout-tests-locally-true (current-buffer))
            nil (concat "Test setup mistake -- variable supposed to have"
                        " local binding, but it lacks one."))
    (allout-add-resumptions '(allout-tests-locally-true nil))
    (assert (not (default-boundp 'allout-tests-locally-true)))
    (assert (local-variable-p 'allout-tests-locally-true (current-buffer)))
    (assert (equal allout-tests-locally-true nil))
    (allout-do-resumptions)
    (assert (boundp 'allout-tests-locally-true))
    (assert (local-variable-p 'allout-tests-locally-true (current-buffer)))
    (assert (equal allout-tests-locally-true t))
    (assert (not (default-boundp 'allout-tests-locally-true))))

  ;; ensure that last of multiple resumptions holds, for various scopes.
  (with-temp-buffer
    (allout-tests-obliterate-variable 'allout-tests-globally-unbound)
    (allout-tests-obliterate-variable 'allout-tests-globally-true)
    (setq allout-tests-globally-true t)
    (allout-tests-obliterate-variable 'allout-tests-locally-true)
    (set (make-local-variable 'allout-tests-locally-true) t)
    (allout-add-resumptions '(allout-tests-globally-unbound t)
                            '(allout-tests-globally-true nil)
                            '(allout-tests-locally-true nil))
    (allout-add-resumptions '(allout-tests-globally-unbound 2)
                            '(allout-tests-globally-true 3)
                            '(allout-tests-locally-true 4))
    ;; reestablish many of the basic conditions are maintained after re-add:
    (assert (not (default-boundp 'allout-tests-globally-unbound)))
    (assert (local-variable-p 'allout-tests-globally-unbound (current-buffer)))
    (assert (equal allout-tests-globally-unbound 2))
    (assert (default-boundp 'allout-tests-globally-true))
    (assert (local-variable-p 'allout-tests-globally-true (current-buffer)))
    (assert (equal allout-tests-globally-true 3))
    (assert (not (default-boundp 'allout-tests-locally-true)))
    (assert (local-variable-p 'allout-tests-locally-true (current-buffer)))
    (assert (equal allout-tests-locally-true 4))
    (allout-do-resumptions)
    (assert (not (local-variable-p 'allout-tests-globally-unbound
                                   (current-buffer))))
    (assert (not (boundp 'allout-tests-globally-unbound)))
    (assert (not (local-variable-p 'allout-tests-globally-true
                                   (current-buffer))))
    (assert (boundp 'allout-tests-globally-true))
    (assert (equal allout-tests-globally-true t))
    (assert (boundp 'allout-tests-locally-true))
    (assert (local-variable-p 'allout-tests-locally-true (current-buffer)))
    (assert (equal allout-tests-locally-true t))
    (assert (not (default-boundp 'allout-tests-locally-true))))

  ;; ensure that deliberately unbinding registered variables doesn't foul things
  (with-temp-buffer
    (allout-tests-obliterate-variable 'allout-tests-globally-unbound)
    (allout-tests-obliterate-variable 'allout-tests-globally-true)
    (setq allout-tests-globally-true t)
    (allout-tests-obliterate-variable 'allout-tests-locally-true)
    (set (make-local-variable 'allout-tests-locally-true) t)
    (allout-add-resumptions '(allout-tests-globally-unbound t)
                            '(allout-tests-globally-true nil)
                            '(allout-tests-locally-true nil))
    (allout-tests-obliterate-variable 'allout-tests-globally-unbound)
    (allout-tests-obliterate-variable 'allout-tests-globally-true)
    (allout-tests-obliterate-variable 'allout-tests-locally-true)
    (allout-do-resumptions))
  )
;;;_  % Run unit tests if `allout-run-unit-tests-after-load' is true:
(when allout-run-unit-tests-on-load
  (allout-run-unit-tests))

;;;_ #12 Provide
(provide 'allout)

;;;_* Local emacs vars.
;; The following `allout-layout' local variable setting:
;;  - closes all topics from the first topic to just before the third-to-last,
;;  - shows the children of the third to last (config vars)
;;  - and the second to last (code section),
;;  - and closes the last topic (this local-variables section).
;;Local variables:
;;allout-layout: (0 : -1 -1 0)
;;End:

;;; allout.el ends here
