;; allout-widgets.el --- Visually highlight allout outline structure.

;; Copyright (C) 2005-2012 Free Software Foundation, Inc.

;; Author: Ken Manheimer <ken dot manheimer at gmail...>
;; Maintainer: Ken Manheimer <ken dot manheimer at gmail...>
;; Version: 1.0
;; Created: Dec 2005
;; Keywords: outlines
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

;; This is an allout outline-mode add-on that highlights outline structure
;; with graphical widgets.
;;
;; To activate, customize `allout-widgets-auto-activation'.  You can also
;; invoke allout-widgets-mode in a particular allout buffer.  When
;; auto-enabled, you can inhibit widget operation in particular allout
;; buffers by setting the variable `allout-widgets-mode-inhibit' non-nil in
;; that file's buffer.  Use emacs *file local variables* to generally
;; inhibit for a file.
;;
;; See the `allout-widgets-mode' docstring for more details.
;;
;; Info about allout and allout-widgets development are available at
;; http://myriadicity.net/Sundry/EmacsAllout
;;
;; The graphics include:
;;
;; - icons for item bullets, varying to distinguish whether the item either
;;   lacks any subitems, the subitems are currently collapsed within the
;;   item, or the item is currently expanded.
;;
;; - guide lines connecting item bullet-icons with those of their subitems.
;;
;; - cue area between the bullet-icon and the start of the body headline,
;;   for item numbering, encryption indicator, and distinctive bullets.
;;
;; The bullet-icon and guide line graphics provide keybindings and mouse
;; bindings for easy outline navigation and exposure control, extending
;; outline hot-spot navigation (see `allout-mode' docstring for details).
;;
;; Developers note: Our use of emacs widgets is unconventional.  We
;; decorate existing text rather than substituting for it, to
;; piggy-back on existing allout operation.  This employs the C-coded
;; efficiencies of widget-apply, widget-get, and widget-put, along
;; with the basic object-oriented organization of widget-create, to
;; systematically couple overlays, graphics, and other features with
;; allout-governed text.

;;; Code:

;;;_ : General Environment
(require 'allout)
(require 'widget)
(require 'wid-edit)

(eval-when-compile
  (progn
    (require 'overlay)
    (require 'cl)
    ))

;;;_ : internal variables needed before user-customization variables
;;; In order to enable activation of allout-widgets-mode via customization,
;;; allout-widgets-auto-activation uses a setting function.  That function
;;; is invoked when the customization variable definition is evaluated,
;;; during file load, so the involved code must reside above that
;;; definition in the file.
;;;_  = allout-widgets-mode
(defvar allout-widgets-mode nil
  "Allout mode enhanced with graphical widgets.")
(make-variable-buffer-local 'allout-widgets-mode)

;;;_ : USER CUSTOMIZATION VARIABLES and incidental functions:
;;;_  > defgroup allout-widgets
;;;###autoload
(defgroup allout-widgets nil
  "Allout extension that highlights outline structure graphically.

Customize `allout-widgets-auto-activation' to activate allout-widgets
with allout-mode."
  :group 'allout)
;;;_  > defgroup allout-widgets-developer
(defgroup allout-widgets-developer nil
  "Settings for development of allout widgets extension."
  :group 'allout-widgets)
;;;_  ; some functions a bit early, for allout-auto-activation dependency:
;;;_  > allout-widgets-mode-enable
(defun allout-widgets-mode-enable ()
  "Enable allout-widgets-mode in allout-mode buffers.

See `allout-widgets-mode-inhibit' for per-file/per-buffer
inhibition of allout-widgets-mode."
  (add-hook 'allout-mode-off-hook 'allout-widgets-mode-off)
  (add-hook 'allout-mode-on-hook 'allout-widgets-mode-on)
  t)
;;;_  > allout-widgets-mode-disable
(defun allout-widgets-mode-disable ()
  "Disable allout-widgets-mode in allout-mode buffers.

See `allout-widgets-mode-inhibit' for per-file/per-buffer
inhibition of allout-widgets-mode."
  (remove-hook 'allout-mode-off-hook 'allout-widgets-mode-off)
  (remove-hook 'allout-mode-on-hook 'allout-widgets-mode-on)
  t)
;;;_  > allout-widgets-setup (varname value)
;;;###autoload
(defun allout-widgets-setup (varname value)
  "Commission or decommission allout-widgets-mode along with allout-mode.

Meant to be used by customization of `allout-widgets-auto-activation'."
  (set-default varname value)
  (if allout-widgets-auto-activation
      (allout-widgets-mode-enable)
    (allout-widgets-mode-disable)))
;;;_  = allout-widgets-auto-activation
;;;###autoload
(defcustom allout-widgets-auto-activation nil
  "Activate to enable allout icon graphics wherever allout mode is active.

Also enable `allout-auto-activation' for this to take effect upon
visiting an outline.

When this is set you can disable allout widgets in select files
by setting `allout-widgets-mode-inhibit'

Instead of setting `allout-widgets-auto-activation' you can
explicitly invoke `allout-widgets-mode' in allout buffers where
you want allout widgets operation.

See `allout-widgets-mode' for allout widgets mode features."
  :version "24.1"
  :type 'boolean
  :group 'allout-widgets
  :set 'allout-widgets-setup
 )
;; ;;;_  = allout-widgets-allow-unruly-edits
;; (defcustom allout-widgets-allow-unruly-edits nil
;;   "*Control whether manual edits are restricted to maintain outline integrity.

;; When nil, manual edits must either be within an item's body or encompass
;; one or more items completely - eg, killing topics as entities, rather than
;; deleting from the middle of one to the middle of another.

;; If you only occasionally need to make unrestricted change, you can set this
;; variable in the specific buffer using set-variable, or just deactivate
;; `allout-mode' temporarily.  You can customize this to always allow unruly
;; edits, but you will be able to create outlines that are unnavigable in
;; principle, and not just for allout's navigation and exposure mechanisms."
;;   :type 'boolean
;;   :group allout-widgets)
;; (make-variable-buffer-local 'allout-widgets-allow-unruly-edits)
;;;_  = allout-widgets-auto-activation - below, for eval-order dependencies
;;;_  = allout-widgets-icons-dark-subdir
(defcustom allout-widgets-icons-dark-subdir "icons/allout-widgets/dark-bg/"
  "Directory on `image-load-path' holding allout icons for dark backgrounds."
  :version "24.1"
  :type 'string
  :group 'allout-widgets)
;;;_  = allout-widgets-icons-light-subdir
(defcustom allout-widgets-icons-light-subdir "icons/allout-widgets/light-bg/"
  "Directory on `image-load-path' holding allout icons for light backgrounds."
  :version "24.1"
  :type 'string
  :group 'allout-widgets)
;;;_  = allout-widgets-icon-types
(defcustom allout-widgets-icon-types '(xpm png)
  "File extensions for the icon graphic format types, in order of preference."
  :version "24.1"
  :type '(repeat symbol)
  :group 'allout-widgets)

;;;_  . Decoration format
;;;_   = allout-widgets-theme-dark-background
(defcustom allout-widgets-theme-dark-background "allout-dark-bg"
  "Identify the outline's icon theme to use with a dark background."
  :version "24.1"
  :type '(string)
  :group 'allout-widgets)
;;;_   = allout-widgets-theme-light-background
(defcustom allout-widgets-theme-light-background "allout-light-bg"
  "Identify the outline's icon theme to use with a light background."
  :version "24.1"
  :type '(string)
  :group 'allout-widgets)
;;;_   = allout-widgets-item-image-properties-emacs
(defcustom allout-widgets-item-image-properties-emacs
  '(:ascent center :mask (heuristic t))
  "*Default properties item widget images in mainline Emacs."
  :version "24.1"
  :type 'plist
  :group 'allout-widgets)
;;;_   = allout-widgets-item-image-properties-xemacs
(defcustom allout-widgets-item-image-properties-xemacs
  nil
  "*Default properties item widget images in XEmacs."
  :version "24.1"
  :type 'plist
  :group 'allout-widgets)
;;;_  . Developer
;;;_   = allout-widgets-run-unit-tests-on-load
(defcustom allout-widgets-run-unit-tests-on-load nil
  "*When non-nil, unit tests will be run at end of loading allout-widgets.

Generally, allout widgets code developers are the only ones who'll want to
set this.

\(If set, this makes it an even better practice to exercise changes by
doing byte-compilation with a repeat count, so the file is loaded after
compilation.)

See `allout-widgets-run-unit-tests' to see what's run."
  :version "24.1"
  :type 'boolean
  :group 'allout-widgets-developer)
;;;_   = allout-widgets-time-decoration-activity
(defcustom allout-widgets-time-decoration-activity nil
  "*Retain timing info of the last cooperative redecoration.

The details are retained as the value of
`allout-widgets-last-decoration-timing'.

Generally, allout widgets code developers are the only ones who'll want to
set this."
  :version "24.1"
  :type 'boolean
  :group 'allout-widgets-developer)
;;;_   = allout-widgets-hook-error-post-time 0
(defcustom allout-widgets-hook-error-post-time 0
  "*Amount of time to sit showing hook error messages.

0 is minimal, or nil to not post to the message area.

This is for debugging purposes."
  :version "24.1"
  :type 'integer
  :group 'allout-widgets-developer)
;;;_   = allout-widgets-maintain-tally nil
(defcustom allout-widgets-maintain-tally nil
  "*If non-nil, maintain a collection of widgets, `allout-widgets-tally'.

This is for debugging purposes.

The tally shows the total number of item widgets in the current
buffer, and tracking increases as new widgets are added and
decreases as obsolete widgets are garbage collected."
  :version "24.1"
  :type 'boolean
  :group 'allout-widgets-developer)
(defvar allout-widgets-tally nil
  "Hash-table of existing allout widgets, for debugging.

Table is maintained iff `allout-widgets-maintain-tally' is non-nil.

The table contents will be out of sync if any widgets are created
or deleted while this variable is nil.")
(make-variable-buffer-local 'allout-widgets-tally)
(defvar allout-widgets-mode-inhibit)    ; defined below
;;;_   > allout-widgets-tally-string
(defun allout-widgets-tally-string ()
  "Return a string giving the number of tracked widgets, or empty string if not tracking.

The string is formed for appending to the allout-mode mode-line lighter.

An empty string is also returned if tracking is inhibited or
widgets are locally inhibited.

The number varies according to the evanescence of objects on a
 hash table with weak keys, so tracking of widget erasures is often delayed."
  (when (and allout-widgets-maintain-tally
             (not allout-widgets-mode-inhibit)
             allout-widgets-tally)
    (format ":%s" (hash-table-count allout-widgets-tally))))
;;;_   = allout-widgets-track-decoration nil
(defcustom allout-widgets-track-decoration nil
  "*If non-nil, show cursor position of each item decoration.

This is for debugging purposes, and generally set at need in a
buffer rather than as a prevailing configuration \(but it's handy
to publicize it by making it a customization variable\)."
  :version "24.1"
  :type 'boolean
  :group 'allout-widgets-developer)
(make-variable-buffer-local 'allout-widgets-track-decoration)

;;;_ : Mode context - variables, hookup, and hooks
;;;_  . internal mode variables
;;;_   , Mode activation and environment
;;;_    = allout-widgets-version
(defvar allout-widgets-version "1.0"
  "Version of currently loaded allout-widgets extension.")
;;;_    > allout-widgets-version
(defun allout-widgets-version (&optional here)
  "Return string describing the loaded outline version."
  (interactive "P")
  (let ((msg (concat "Allout Outline Widgets Extension v "
                     allout-widgets-version)))
    (if here (insert msg))
    (message "%s" msg)
    msg))
;;;_    = allout-widgets-mode-inhibit
(defvar allout-widgets-mode-inhibit nil
  "Inhibit `allout-widgets-mode' from activating widgets.

This also inhibits automatic adjustment of widgets to track allout outline
changes.

You can use this as a file local variable setting to disable
allout widgets enhancements in selected buffers while generally
enabling widgets by customizing `allout-widgets-auto-activation'.

In addition, you can invoked `allout-widgets-mode' allout-mode
buffers where this is set to enable and disable widget
enhancements, directly.")
;;;###autoload
(put 'allout-widgets-mode-inhibit 'safe-local-variable
     (if (fboundp 'booleanp) 'booleanp (lambda (x) (member x '(t nil)))))
(make-variable-buffer-local 'allout-widgets-mode-inhibit)
;;;_    = allout-inhibit-body-modification-hook
(defvar allout-inhibit-body-modification-hook nil
  "Override de-escaping of text-prefixes in item bodies during specific changes.

This is used by `allout-buffer-modification-handler' to signal such changes
to `allout-body-modification-handler', and is always reset by
`allout-post-command-business'.")
(make-variable-buffer-local 'allout-inhibit-body-modification-hook)
;;;_    = allout-widgets-icons-cache
(defvar allout-widgets-icons-cache nil
  "Cache allout icon images, as an association list.

`allout-fetch-icon-image' uses this cache transparently, keying
images with lists containing the name of the icon directory \(as
found on the `load-path') and the icon name.

Set this variable to `nil' to empty the cache, and have it replenish from the
filesystem.")
;;;_    = allout-widgets-unset-inhibit-read-only
(defvar allout-widgets-unset-inhibit-read-only nil
  "Tell `allout-widgets-post-command-business' to unset `inhibit-read-only'.

Used by `allout-graphics-modification-handler'")
;;;_    = allout-widgets-reenable-before-change-handler
(defvar allout-widgets-reenable-before-change-handler nil
  "Tell `allout-widgets-post-command-business' to reequip the handler.

Necessary because the handler sometimes deliberately raises an
error, causing it to be disabled.")
;;;_   , State for hooks
;;;_    = allout-unresolved-body-mod-workroster
(defvar allout-unresolved-body-mod-workroster (make-hash-table :size 16)
  "List of body-overlays that did before-change business but not after-change.

See `allout-post-command-business' and `allout-body-modification-handler'.")
;;;_    = allout-structure-unruly-deletion-message
(defvar allout-structure-unruly-deletion-message
  "Unruly edit prevented --
To change the bullet character: \\[allout-rebullet-current-heading]
To promote this item: \\[allout-shift-out]
To demote it: \\[allout-shift-in]
To delete it and offspring: \\[allout-kill-topic]
See \\[describe-mode] for many more options."
  "Informative message presented on improper editing of outline structure.

The structure includes the guides lines, bullet, and bullet cue.")
;;;_    = allout-widgets-changes-record
(defvar allout-widgets-changes-record nil
  "Record outline changes for processing by post-command hook.

Entries on the list are lists whose first element is a symbol indicating
the change type and subsequent elements are data specific to that change
type.  Specifically:

 'exposure `allout-exposure-from' `allout-exposure-to' `allout-exposure-flag'

The changes are recorded in reverse order, with new values pushed
onto the front.")
(make-variable-buffer-local 'allout-widgets-changes-record)
;;;_    = allout-widgets-undo-exposure-record
(defvar allout-widgets-undo-exposure-record nil
  "Record outline undo traces for processing by post-command hook.

The changes are recorded in reverse order, with new values pushed
onto the front.")
(make-variable-buffer-local 'allout-widgets-undo-exposure-record)
;;;_    = allout-widgets-last-hook-error
(defvar allout-widgets-last-hook-error nil
  "String holding last error string, for debugging purposes.")
;;;_    = allout-widgets-adjust-message-length-threshold 100
(defvar allout-widgets-adjust-message-length-threshold 100
  "Display \"Adjusting widgets\" message above this number of pending changes."
 )
;;;_    = allout-widgets-adjust-message-size-threshold 10000
(defvar allout-widgets-adjust-message-size-threshold 10000
  "Display \"Adjusting widgets\" message above this size of pending changes."
 )
;;;_    = allout-doing-exposure-undo-processor nil
(defvar allout-undo-exposure-in-progress nil
  "Maintained true during `allout-widgets-exposure-undo-processor'")
;;;_   , Widget-specific outline text format
;;;_    = allout-escaped-prefix-regexp
(defvar allout-escaped-prefix-regexp ""
  "*Regular expression for body text that would look like an item prefix if
not altered with an escape sequence.")
(make-variable-buffer-local 'allout-escaped-prefix-regexp)
;;;_   , Widget element formatting
;;;_    = allout-item-icon-keymap
(defvar allout-item-icon-keymap
  (let ((km (make-sparse-keymap)))
    (dolist (digit '("0" "1" "2" "3"
                     "4" "5" "6" "7" "8" "9"))
      (define-key km digit 'digit-argument))
    (define-key km "-" 'negative-argument)
;;    (define-key km [(return)] 'allout-tree-expand-command)
;;    (define-key km [(meta return)] 'allout-toggle-torso-command)
;;    (define-key km [(down-mouse-1)] 'allout-item-button-click)
;;    (define-key km [(down-mouse-2)] 'allout-toggle-torso-event-command)
    ;; Override underlying mouse-1 and mouse-2 bindings in icon territory:
    (define-key km [(mouse-1)] (lambda () (interactive) nil))
    (define-key km [(mouse-2)] (lambda () (interactive) nil))

    ;; Catchall, handles actual keybindings, dynamically doing keymap lookups:
    (define-key km [t] 'allout-item-icon-key-handler)

    km)
  "General tree-node key bindings.")
;;;_    = allout-item-body-keymap
(defvar allout-item-body-keymap
  (let ((km (make-sparse-keymap))
        (local-map (current-local-map)))
;;    (define-key km [(control return)] 'allout-tree-expand-command)
;;    (define-key km [(meta return)] 'allout-toggle-torso-command)
    ;; XXX We need to reset this per buffer's mode; we do so in
    ;; allout-widgets-mode.
    (if local-map
        (set-keymap-parent km local-map))

    km)
  "General key bindings for the text content of outline items.")
(make-variable-buffer-local 'allout-item-body-keymap)
;;;_    = allout-body-span-category
(defvar allout-body-span-category nil
  "Symbol carrying allout body-text overlay properties.")
;;;_    = allout-cue-span-keymap
(defvar allout-cue-span-keymap
  (let ((km (make-sparse-keymap)))
    (set-keymap-parent km allout-item-icon-keymap)
    km)
  "Keymap used in the item cue area - the space between the icon and headline.")
;;;_    = allout-escapes-category
(defvar allout-escapes-category nil
  "Symbol for category of text property used to hide escapes of prefix-like
text in allout item bodies.")
;;;_    = allout-guides-category
(defvar allout-guides-category nil
  "Symbol carrying allout icon-guides overlay properties.")
;;;_    = allout-guides-span-category
(defvar allout-guides-span-category nil
  "Symbol carrying allout icon and guide lines overlay properties.")
;;;_    = allout-icon-span-category
(defvar allout-icon-span-category nil
  "Symbol carrying allout icon and guide lines overlay properties.")
;;;_    = allout-cue-span-category
(defvar allout-cue-span-category nil
  "Symbol carrying common properties of the space following the outline icon.

\(That space is used to convey selected cues indicating body qualities,
including things like:
 - encryption '~'
 - numbering '#'
 - indirect reference '@'
 - distinctive bullets - see `allout-distinctive-bullets-string'.\)")
;;;_    = allout-span-to-category
(defvar allout-span-to-category
  '((:guides-span . allout-guides-span-category)
    (:cue-span . allout-cue-span-category)
    (:icon-span . allout-icon-span-category)
    (:body-span . allout-body-span-category))
  "Association list mapping span identifier to category identifier.")
;;;_    = allout-trailing-category
(defvar allout-trailing-category nil
  "Symbol carrying common properties of an overlay's trailing newline.")
;;;_   , Developer
(defvar allout-widgets-last-decoration-timing nil
  "Timing details for the last cooperative decoration action.

This is maintained when `allout-widgets-time-decoration-activity' is set.

The value is a list containing two elements:
  - the elapsed time as a number of seconds
  - the list of changes processed, a la `allout-widgets-changes-record'.

When active, the value is revised each time automatic decoration activity
happens in the buffer.")
(make-variable-buffer-local 'allout-widgets-last-decoration-timing)
;;;_  . mode hookup
;;;_   > define-minor-mode allout-widgets-mode (arg)
;;;###autoload
(define-minor-mode allout-widgets-mode
  "Toggle Allout Widgets mode.
With a prefix argument ARG, enable Allout Widgets mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

Allout Widgets mode is an extension of Allout mode that provides
graphical decoration of outline structure.  It is meant to
operate along with `allout-mode', via `allout-mode-hook'.

The graphics include:

- guide lines connecting item bullet-icons with those of their subitems.

- icons for item bullets, varying to indicate whether or not the item
  has subitems, and if so, whether or not the item is expanded.

- cue area between the bullet-icon and the start of the body headline,
  for item numbering, encryption indicator, and distinctive bullets.

The bullet-icon and guide line graphics provide keybindings and mouse
bindings for easy outline navigation and exposure control, extending
outline hot-spot navigation \(see `allout-mode')."

  :lighter nil
  :keymap nil

  ;; define-minor-mode handles any provided argument according to emacs
  ;; minor-mode conventions - '(elisp) Minor Mode Conventions' - and sets
  ;; allout-widgets-mode accordingly *before* running the body code, so we
  ;; cue on that.
  (if allout-widgets-mode
      ;; Activating:
      (progn
        (allout-add-resumptions
         ;; XXX user may need say in line-truncation/hscrolling - an option
         ;;     that abstracts mode.
         ;; truncate text lines to keep guide lines intact:
         '(truncate-lines t)
         ;; and enable autoscrolling to ease view of text
         '(auto-hscroll-mode t)
         '(line-move-ignore-fields t)
         '(widget-push-button-prefix "")
         '(widget-push-button-suffix "")
         ;; allout-escaped-prefix-regexp depends on allout-regexp:
         (list 'allout-escaped-prefix-regexp (concat "\\(\\\\\\)"
                                                     "\\(" allout-regexp "\\)")))
        (allout-add-resumptions
         (list 'allout-widgets-tally allout-widgets-tally)
         (list 'allout-widgets-escapes-sanitization-regexp-pair
               (list (concat "\\(\n\\|\\`\\)"
                             allout-escaped-prefix-regexp
                             )
                     ;; Include everything but the escape symbol.
                     "\\1\\3"))
         )

        (add-hook 'after-change-functions 'allout-widgets-after-change-handler
                  nil t)

        (allout-setup-text-properties)
        (add-to-invisibility-spec '(allout-torso . t))
        (add-to-invisibility-spec 'allout-escapes)

        (if (current-local-map)
            (set-keymap-parent allout-item-body-keymap (current-local-map)))

        (add-hook 'allout-exposure-change-hook
                  'allout-widgets-exposure-change-recorder nil 'local)
        (add-hook 'allout-structure-added-hook
                  'allout-widgets-additions-recorder nil 'local)
        (add-hook 'allout-structure-deleted-hook
                  'allout-widgets-deletions-recorder nil 'local)
        (add-hook 'allout-structure-shifted-hook
                  'allout-widgets-shifts-recorder nil 'local)
        (add-hook 'allout-after-copy-or-kill-hook
                  'allout-widgets-after-copy-or-kill-function nil 'local)
        (add-hook 'allout-post-undo-hook
                  'allout-widgets-after-undo-function nil 'local)

        (add-hook 'before-change-functions 'allout-widgets-before-change-handler
                  nil 'local)
        (add-hook 'post-command-hook 'allout-widgets-post-command-business
                  nil 'local)
        (add-hook 'pre-command-hook 'allout-widgets-pre-command-business
                  nil 'local)

        ;; init the widgets tally for debugging:
        (if (not allout-widgets-tally)
            (setq allout-widgets-tally (make-hash-table
                                        :test 'eq :weakness 'key)))
        ;; add tally count display on minor-mode-alist just after
        ;; allout-mode entry.
        ;; (we use ternary condition form to keep condition simple for deletion.)
        (let* ((mode-line-entry '(allout-widgets-mode-inhibit ""
                                                              (:eval (allout-widgets-tally-string))))
               (associated (assoc (car mode-line-entry) minor-mode-alist))
               ;; need location for it only if not already present:
               (after (and (not associated)
                           (memq (assq 'allout-mode minor-mode-alist) minor-mode-alist))))
          (if after
              (rplacd after (cons mode-line-entry (cdr after)))))
        (allout-widgets-prepopulate-buffer)
        t)
    ;; Deactivating:
    (let ((inhibit-read-only t)
          (was-modified (buffer-modified-p)))

      (allout-widgets-undecorate-region (point-min)(point-max))
      (remove-from-invisibility-spec '(allout-torso . t))
      (remove-from-invisibility-spec 'allout-escapes)

      (remove-hook 'after-change-functions
                   'allout-widgets-after-change-handler 'local)
      (remove-hook 'allout-exposure-change-hook
                   'allout-widgets-exposure-change-recorder 'local)
      (remove-hook 'allout-structure-added-hook
                   'allout-widgets-additions-recorder 'local)
      (remove-hook 'allout-structure-deleted-hook
                   'allout-widgets-deletions-recorder 'local)
      (remove-hook 'allout-structure-shifted-hook
                   'allout-widgets-shifts-recorder 'local)
      (remove-hook 'allout-after-copy-or-kill-hook
                   'allout-widgets-after-copy-or-kill-function 'local)
      (remove-hook 'before-change-functions
                   'allout-widgets-before-change-handler 'local)
      (remove-hook 'post-command-hook
                   'allout-widgets-post-command-business 'local)
      (remove-hook 'pre-command-hook
                   'allout-widgets-pre-command-business 'local)
      (assq-delete-all 'allout-widgets-mode-inhibit minor-mode-alist)
      (set-buffer-modified-p was-modified))))
;;;_   > allout-widgets-mode-off
(defun allout-widgets-mode-off ()
  "Explicitly disable allout-widgets-mode."
  (allout-widgets-mode -1))
;;;_   > allout-widgets-mode-off
(defun allout-widgets-mode-on ()
  "Explicitly disable allout-widgets-mode."
  (allout-widgets-mode 1))
;;;_   > allout-setup-text-properties ()
(defun allout-setup-text-properties ()
  "Configure category and literal text properties."

  ;; XXX body - before-change, entry, keymap

  (setplist 'allout-guides-span-category nil)
  (put 'allout-guides-span-category
       'modification-hooks '(allout-graphics-modification-handler))
  (put 'allout-guides-span-category 'local-map allout-item-icon-keymap)
  (put 'allout-guides-span-category 'mouse-face widget-button-face)
  (put 'allout-guides-span-category 'field 'structure)
;;  (put 'allout-guides-span-category 'face 'widget-button)

  (setplist 'allout-icon-span-category
            (allout-widgets-copy-list (symbol-plist
                                       'allout-guides-span-category)))
  (put 'allout-icon-span-category 'field 'structure)

  ;; XXX for body text we're instead going to use the buffer-wide
  ;;     resources, like before/after-change-functions hooks and the
  ;;     buffer's key map.  that way we won't have to do painful provisions
  ;;     to fixup things after edits, catch outlier interstitial
  ;;     characters, like newline and empty lines after hidden subitems,
  ;;     etc.
  (setplist 'allout-body-span-category nil)
  (put 'allout-body-span-category 'evaporate t)
  (put 'allout-body-span-category 'local-map allout-item-body-keymap)
  ;;(put 'allout-body-span-category
  ;;     'modification-hooks '(allout-body-modification-handler))
  ;;(put 'allout-body-span-category 'field 'body)

  (setplist 'allout-cue-span-category nil)
  (put 'allout-cue-span-category 'evaporate t)
  (put 'allout-cue-span-category
       'modification-hooks '(allout-body-modification-handler))
  (put 'allout-cue-span-category 'local-map allout-cue-span-keymap)
  (put 'allout-cue-span-category 'mouse-face widget-button-face)
  (put 'allout-cue-span-category 'pointer 'arrow)
  (put 'allout-cue-span-category 'field 'structure)

  (setplist 'allout-trailing-category nil)
  (put 'allout-trailing-category 'evaporate t)
  (put 'allout-trailing-category 'local-map allout-item-body-keymap)

  (setplist 'allout-escapes-category nil)
  (put 'allout-escapes-category 'invisible 'allout-escapes)
  (put 'allout-escapes-category 'evaporate t))
;;;_   > allout-widgets-prepopulate-buffer ()
(defun allout-widgets-prepopulate-buffer ()
  "Step over the current buffers exposed items to do initial widgetizing."
  (if (not allout-widgets-mode-inhibit)
      (save-excursion
        (goto-char (point-min))
        (while (allout-next-visible-heading 1)
          (when (not (widget-at (point)))
            (allout-get-or-create-item-widget))))))
;;;_  . settings context
;;;_   = allout-container-item
(defvar allout-container-item-widget nil
  "A widget for the current outline's overarching container as an item.

The item has settings \(of the file/connection\) and maybe a body, but no
icon/bullet.")
(make-variable-buffer-local 'allout-container-item-widget)
;;;_  . Hooks and hook helpers
;;;_   , major command-loop business:
;;;_    > allout-widgets-pre-command-business (&optional recursing)
(defun allout-widgets-pre-command-business (&optional recursing)
  "Handle actions pending before allout-mode activity."
)
;;;_    > allout-widgets-post-command-business (&optional recursing)
(defun allout-widgets-post-command-business (&optional recursing)
  "Handle actions pending after any allout-mode commands.

Optional RECURSING is for internal use, to limit recursion."
  ;; - check changed text for nesting discontinuities and escape anything
  ;;   that's: (1) asterisks at bol or (2) excessively nested.
  (condition-case failure

      (when (and (boundp 'allout-mode) allout-mode)

        (if allout-widgets-unset-inhibit-read-only
            (setq inhibit-read-only nil
                  allout-widgets-unset-inhibit-read-only nil))

        (when allout-widgets-reenable-before-change-handler
          (add-hook 'before-change-functions
                    'allout-widgets-before-change-handler
                    nil 'local)
          (setq allout-widgets-reenable-before-change-handler nil))

        (when (or allout-widgets-undo-exposure-record
                  allout-widgets-changes-record)
          (let* ((debug-on-signal t)
                 (debug-on-error t)
                 ;; inhibit recording new undo records when processing
                 ;; effects of undo-exposure:
                 (debugger 'allout-widgets-hook-error-handler)
                 (adjusting-message " Adjusting widgets...")
                 (replaced-message (allout-widgets-adjusting-message
                                    adjusting-message))
                 (start-time (current-time)))

            (if allout-widgets-undo-exposure-record
                ;; inhibit undo recording iff undoing exposure stuff.
                ;; XXX we might need to inhibit per respective
                ;;     change-record, rather than assuming that some undo
                ;;     activity during a command is all undo activity.
                (let ((buffer-undo-list t))
                  (allout-widgets-exposure-undo-processor)
                  (allout-widgets-changes-dispatcher))
              (allout-widgets-exposure-undo-processor)
              (allout-widgets-changes-dispatcher))

            (if allout-widgets-time-decoration-activity
                (setq allout-widgets-last-decoration-timing
                      (list (allout-elapsed-time-seconds (current-time)
                                                         start-time)
                            allout-widgets-changes-record)))

            (setq allout-widgets-changes-record nil)

            (if replaced-message
                (if (stringp replaced-message)
                    (message replaced-message)
                  (message "")))))

        ;; alas, decorated intermediate matches are not easily undecorated
        ;; when they're automatically rehidden by isearch, so we're
        ;; dropping this nicety.
        ;; ;; Detect undecorated items, eg during isearch into previously
        ;; ;; unexposed topics, and decorate "economically".  Some
        ;; ;; undecorated stuff is often exposed, to reduce lag, but the
        ;; ;; item containing the cursor is decorated.  We constrain
        ;; ;; recursion to avoid being trapped by unexpectedly undecoratable
        ;; ;; items.
        ;; (when (and (not recursing)
        ;;            (not (allout-current-decorated-p))
        ;;            (or (not (equal (allout-depth) 0))
        ;;                (not allout-container-item-widget)))
        ;;   (let ((buffer-undo-list t))
        ;;     (allout-widgets-exposure-change-recorder
        ;;      allout-recent-prefix-beginning allout-recent-prefix-end nil)
        ;;     (allout-widgets-post-command-business 'recursing)))

        ;; Detect and rectify fouled outline structure - decorated item
        ;; not at beginning of line.
        (let ((this-widget (or (widget-at (point))
                               ;; XXX we really should be checking across
                               ;; edited span, not just point and point+1
                               (and (not (eq (point) (point-max)))
                                    (widget-at (1+ (point))))))
              inserted-at)
          (save-excursion
            (if (and this-widget
                     (goto-char (widget-get this-widget :from))
                     (not (bolp)))
                (if (not
                     (condition-case err
                         (yes-or-no-p
                          (concat "Misplaced item won't be recognizable "
                                  " as part of outline - rectify? "))
                       (quit nil)))
                    (progn
                      (if (allout-hidden-p (max (1- (point)) 1))
                          (save-excursion
                            (goto-char (max (1- (point)) 1))
                            (allout-show-to-offshoot)))
                      (allout-widgets-undecorate-item this-widget))
                  ;; expose any hidden intervening items, so resulting
                  ;; position is clear:
                  (setq inserted-at (point))
                  (allout-unprotected (insert-before-markers "\n"))
                  (forward-char -1)
                  ;; ensure the inserted newline is visible:
                  (allout-flag-region inserted-at (1+ inserted-at) nil)
                  (allout-widgets-post-command-business 'recursing)
                  (message (concat "outline structure corrected - item"
                                   " moved to beginning of new line"))
                  ;; preserve cursor position in some cases:
                  (if (and inserted-at
                           (> (point) inserted-at))
                      (forward-char -1)))))))

    (error
     ;; zero work list so we don't get stuck futilely retrying.
     ;; error recording done by allout-widgets-hook-error-handler.
     (setq allout-widgets-changes-record nil))))
;;;_   , major change handlers:
;;;_    > allout-widgets-before-change-handler
(defun allout-widgets-before-change-handler (beg end)
  "Business to be done before changes in a widgetized allout outline."
  ;; protect against unruly edits to structure:
  (cond
   (undo-in-progress (when (eq (get-text-property beg 'category)
                               'allout-icon-span-category)
                       (save-excursion
                         (goto-char beg)
                         (let* ((item-widget (allout-get-item-widget)))
                           (if item-widget
                               (allout-widgets-exposure-undo-recorder
                                item-widget))))))
   (inhibit-read-only t)
   ((not  (and (boundp 'allout-mode) allout-mode)) t)
   ((equal this-command 'quoted-insert) t)
   ((not (text-property-any beg (if (equal end beg)
                                    (min (1+ beg) (point-max))
                                  end)
                            'field 'structure))
    t)
   ((yes-or-no-p "Unruly edit of outline structure - allow? ")
    (setq allout-widgets-unset-inhibit-read-only (not inhibit-read-only)
          inhibit-read-only t))
   (t
    ;; tell the allout-widgets-post-command-business to reestablish the hook:
    (setq allout-widgets-reenable-before-change-handler t)
    ;; and raise an error to prevent the edit (and disable the hook):
    (error
     (substitute-command-keys allout-structure-unruly-deletion-message)))))
;;;_    > allout-widgets-after-change-handler
(defun allout-widgets-after-change-handler (beg end prelength)
  "Reconcile what needs to be reconciled for allout widgets after edits."
  )
;;;_   > allout-current-decorated-p ()
(defun allout-current-decorated-p ()
  "True if the current item is not decorated"
  (save-excursion
    (if (allout-back-to-current-heading)
        (if (> allout-recent-depth 0)
            (and (allout-get-item-widget) t)
          allout-container-item-widget))))

;;;_   > allout-widgets-hook-error-handler
(defun allout-widgets-hook-error-handler (mode args)
  "Process errors which occurred in the course of command hook operation.

We store a backtrace of the error information in the variable,
`allout-widgets-last-hook-error', unset the error handlers, and
reraise the error, so that processing continues to the
encompassing condition-case."
  ;; first deconstruct special error environment so errors here propagate
  ;; to encompassing condition-case:
  (setq debugger 'debug
        debug-on-error nil
        debug-on-signal nil)
  (let* ((bt (with-output-to-string (backtrace)))
         (this "allout-widgets-hook-error-handler")
         (header
          (format "allout-widgets-last-hook-error stored, %s/%s %s %s"
                  this mode args
                  (format-time-string "%e-%b-%Y %r" (current-time)))))
    ;; post to *Messages* then immediately replace with more compact notice:
    (message "%s" (setq allout-widgets-last-hook-error
                        (format "%s:\n%s" header bt)))
    (message header) (sit-for allout-widgets-hook-error-post-time)
    ;; reraise the error, or one concerning this function if unexpected:
    (if (equal mode 'error)
        (apply 'signal args)
      (error "%s: unexpected mode, %s %s" this mode args))))
;;;_   > allout-widgets-changes-exceed-threshold-p ()
(defun allout-widgets-adjusting-message (message)
  "Post MESSAGE when pending are likely to make a big enough delay.

If posting of the MESSAGE is warranted and there already is a
`current-message' in the minibuffer, the MESSAGE is appended to
the current one, and the previously pending `current-message' is
returned for later posting on completion.

If posting of the MESSAGE is warranted, but no `current-message'
is pending, then t is returned to indicate that case.

If posting of the MESSAGE is not warranted, then nil is returned.

See `allout-widgets-adjust-message-length-threshold',
`allout-widgets-adjust-message-size-threshold' for message
posting threshold criteria."
  (if (or (> (length allout-widgets-changes-record)
             allout-widgets-adjust-message-length-threshold)
          ;; for size, use distance from start of first to end of last:
          (let ((min (point-max))
                (max 0)
                first second)
            (mapc (function (lambda (entry)
                              (if (eq :undone-exposure (car entry))
                                  nil
                                (setq first (cadr entry)
                                      second (caddr entry))
                                (if (< (min first second) min)
                                    (setq min (min first second)))
                                (if (> (max first second) max)
                                    (setq max (max first second))))))
                    allout-widgets-changes-record)
            (> (- max min) allout-widgets-adjust-message-size-threshold)))
      (let ((prior (current-message)))
        (message (if prior (concat prior " - " message) message))
        (or prior t))))
;;;_   > allout-widgets-changes-dispatcher ()
(defun allout-widgets-changes-dispatcher ()
  "Dispatch CHANGES-RECORD items to respective widgets change processors."

  (if (not allout-widgets-mode-inhibit)
      (let* ((changes-record allout-widgets-changes-record)
             (changes-pending (and changes-record t))
             entry
             exposures
             additions
             deletions
             shifts)

        (when changes-pending
          (while changes-record
            (setq entry (pop changes-record))
            (case (car entry)
              (:exposed (push entry exposures))
              (:added (push entry additions))
              (:deleted (push entry deletions))
              (:shifted (push entry shifts))))

          (if exposures
              (allout-widgets-exposure-change-processor exposures))
          (if additions
              (allout-widgets-additions-processor additions))
          (if deletions
              (allout-widgets-deletions-processor deletions))
          (if shifts
              (allout-widgets-shifts-processor shifts))))
    (when (not (equal allout-widgets-mode-inhibit 'undecorated))
      (allout-widgets-undecorate-region (point-min)(point-max))
      (setq allout-widgets-mode-inhibit 'undecorated))))
;;;_   > allout-widgets-exposure-change-recorder (from to flag)
(defun allout-widgets-exposure-change-recorder (from to flag)
  "Record allout exposure changes for tracking during post-command processing.

Records changes in `allout-widgets-changes-record'."
  (push (list :exposed from to flag) allout-widgets-changes-record))
;;;_   > allout-widgets-exposure-change-processor (changes)
(defun allout-widgets-exposure-change-processor (changes)
  "Widgetize and adjust item widgets tracking allout outline exposure changes.

Generally invoked via `allout-exposure-change-hook'."

  (let ((changes (sort changes (function (lambda (this next)
                                           (< (cadr this) (cadr next))))))
        ;; have to distinguish between concealing and exposing so that, eg,
        ;; `allout-expose-topic's mix is handled properly.
        handled-expose
        handled-conceal
        covered
        deactivate-mark)

    (dolist (change changes)
      (let (handling
            (from (cadr change))
            bucket got
            (to (caddr change))
            (flag (cadddr change))
            parent)

        ;; swap from and to:
        (if (< to from) (setq bucket to
                              to from
                              from bucket))

        ;; have we already handled exposure changes in this region?
        (setq handling (if flag 'handled-conceal 'handled-expose)
              got (allout-range-overlaps from to (symbol-value handling))
              covered (car got))
        (set handling (cadr got))

        (when (not covered)
          (save-excursion
            (goto-char from)
            (cond

             ;; collapsing:
             (flag
              (allout-widgets-undecorate-region from to)
              (allout-beginning-of-current-line)
              (let ((widget (allout-get-item-widget)))
                (if (not widget)
                    (allout-get-or-create-item-widget)
                  (widget-apply widget :redecorate))))

             ;; expanding:
             (t
              (while (< (point) to)
                (allout-beginning-of-current-line)
                (setq parent (allout-get-item-widget))
                (if (not parent)
                    (setq parent (allout-get-or-create-item-widget))
                  (widget-apply parent :redecorate))
                (allout-next-visible-heading 1)
                (if (widget-get parent :has-subitems)
                    (allout-redecorate-visible-subtree parent))
                (if (> (point) to)
                    ;; subtree may be well beyond to - incorporate in ranges:
                    (setq handled-expose
                          (allout-range-overlaps from (point) handled-expose)
                          covered (car handled-expose)
                          handled-expose (cadr handled-expose)))
                (allout-next-visible-heading 1))))))))))

;;;_   > allout-widgets-additions-recorder (from to)
(defun allout-widgets-additions-recorder (from to)
  "Record allout item additions for tracking during post-command processing.

Intended for use on `allout-structure-added-hook'.

FROM point at the start of the first new item and TO is point at the start
of the last one.

Records changes in `allout-widgets-changes-record'."
  (push (list :added from to) allout-widgets-changes-record))
;;;_   > allout-widgets-additions-processor (changes)
(defun allout-widgets-additions-processor (changes)
  "Widgetize and adjust items tracking allout outline structure additions.

Dispatched by `allout-widgets-post-command-business' in response to
:added entries recorded by `allout-widgets-additions-recorder'."
  (save-excursion
    (let (handled
          covered)
      (dolist (change changes)
        (let ((from (cadr change))
              bucket
              (to (caddr change)))
          (if (< to from) (setq bucket to to from from bucket))
          ;; have we already handled exposure changes in this region?
          (setq handled (allout-range-overlaps from to handled)
                covered (car handled)
                handled (cadr handled))
          (when (not covered)
            (goto-char from)
            ;; Prior sibling and parent can both be affected.
            (if (allout-ascend)
                (allout-redecorate-visible-subtree
                 (allout-get-or-create-item-widget 'redecorate)))
            (if (< (point) from)
                (goto-char from))
            (while (and (< (point) to) (not (eobp)))
              (allout-beginning-of-current-line)
              (allout-redecorate-visible-subtree
               (allout-get-or-create-item-widget))
              (allout-next-visible-heading 1))
            (if (> (point) to)
                ;; subtree may be well beyond to - incorporate in ranges:
                (setq handled (allout-range-overlaps from (point) handled)
                      covered (car handled)
                      handled (cadr handled)))))))))

;;;_   > allout-widgets-deletions-recorder (depth from)
(defun allout-widgets-deletions-recorder (depth from)
  "Record allout item deletions for tracking during post-command processing.

Intended for use on `allout-structure-deleted-hook'.

DEPTH is the depth of the deleted subtree, and FROM is the point from which
the subtree was deleted.

Records changes in `allout-widgets-changes-record'."
  (push (list :deleted depth from) allout-widgets-changes-record))
;;;_   > allout-widgets-deletions-processor (changes)
(defun allout-widgets-deletions-processor (changes)
  "Adjust items tracking allout outline structure deletions.

Dispatched by `allout-widgets-post-command-business' in response to
:deleted entries recorded by `allout-widgets-deletions-recorder'."
  (save-excursion
    (dolist (change changes)
      (let ((depth (cadr change))
            (from (caddr change)))
        (goto-char from)
        (when (allout-previous-visible-heading 1)
          (if (> depth 1)
              (allout-ascend-to-depth (1- depth)))
          (allout-redecorate-visible-subtree
           (allout-get-or-create-item-widget 'redecorate)))))))

;;;_   > allout-widgets-shifts-recorder (shifted-amount at)
(defun allout-widgets-shifts-recorder (shifted-amount at)
  "Record outline subtree shifts for tracking during post-command processing.

Intended for use on `allout-structure-shifted-hook'.

SHIFTED-AMOUNT is the depth change and AT is the point at the start of the
subtree that's been shifted.

Records changes in `allout-widgets-changes-record'."
  (push (list :shifted shifted-amount at) allout-widgets-changes-record))
;;;_   > allout-widgets-shifts-processor (changes)
(defun allout-widgets-shifts-processor (changes)
  "Widgetize and adjust items tracking allout outline structure additions.

Dispatched by `allout-widgets-post-command-business' in response to
:shifted entries recorded by `allout-widgets-shifts-recorder'."
  (save-excursion
    (dolist (change changes)
      (goto-char (caddr change))
      (allout-ascend)
      (allout-redecorate-visible-subtree))))
;;;_   > allout-widgets-after-copy-or-kill-function ()
(defun allout-widgets-after-copy-or-kill-function ()
  "Do allout-widgets processing of text just placed in the kill ring.

Intended for use on allout-after-copy-or-kill-hook."
  (if (car kill-ring)
      (setcar kill-ring (allout-widgets-undecorate-text (car kill-ring)))))
;;;_   > allout-widgets-after-undo-function ()
(defun allout-widgets-after-undo-function ()
  "Do allout-widgets processing of text after an undo.

Intended for use on allout-post-undo-hook."
  (save-excursion
    (if (allout-goto-prefix)
        (allout-redecorate-item (allout-get-or-create-item-widget)))))

;;;_   > allout-widgets-exposure-undo-recorder (widget from-state)
(defun allout-widgets-exposure-undo-recorder (widget)
  "Record outline exposure undo for tracking during post-command processing.

Intended for use by `allout-graphics-modification-handler'.

WIDGET is the widget being changed.

Records changes in `allout-widgets-changes-record'."
  ;; disregard the events if we're currently processing them.
  (if (not allout-undo-exposure-in-progress)
      (push widget allout-widgets-undo-exposure-record)))
;;;_   > allout-widgets-exposure-undo-processor ()
(defun allout-widgets-exposure-undo-processor ()
  "Adjust items tracking undo of allout outline structure exposure.

Dispatched by `allout-widgets-post-command-business' in response to
:undone-exposure entries recorded by `allout-widgets-exposure-undo-recorder'."
  (let* ((allout-undo-exposure-in-progress t)
         ;; inhibit undo recording while twiddling exposure to track undo:
         (widgets allout-widgets-undo-exposure-record)
         widget widget-start-marker widget-end-marker
         from-state icon-start-point to-state
         handled covered)
    (setq allout-widgets-undo-exposure-record nil)
    (save-excursion
      (dolist (widget widgets)
        (setq widget-start-marker (widget-get widget :from)
              widget-end-marker (widget-get widget :to)
              from-state (widget-get widget :icon-state)
              icon-start-point (widget-apply widget :actual-position
                                             :icon-start)
              to-state (get-text-property icon-start-point
                                          :icon-state))
        (setq handled (allout-range-overlaps widget-start-marker
                                             widget-end-marker
                                             handled)
              covered (car handled)
              handled (cadr handled))
        (when (not covered)
          (goto-char (widget-get widget :from))
          (when (not (allout-hidden-p))
            ;; adjust actual exposure to that of to-state viz from-state
            (cond ((and (eq to-state 'closed) (eq from-state 'opened))
                   (allout-hide-current-subtree)
                   (allout-decorate-item-and-context widget))
                  ((and (eq to-state 'opened) (eq from-state 'closed))
                   (save-excursion
                     (dolist
                         (expose-to (allout-chart-exposure-contour-by-icon))
                       (goto-char expose-to)
                       (allout-show-to-offshoot)))))))))))
;;;_   > allout-chart-exposure-contour-by-icon (&optional from-depth)
(defun allout-chart-exposure-contour-by-icon (&optional from-depth)
  "Return points of subtree items to which exposure should be extended.

The qualifying items are ones with a widget icon that is in the closed or
empty state, or items with undecorated subitems.

The resulting list of points is in reverse order.

Optional FROM-DEPTH is for internal use."
  ;; During internal recursion, we return a pair: (at-end . result)
  ;; Otherwise we just return the result.
  (let ((from-depth from-depth)
        start-point
        at-end level-depth
        this-widget
        got subgot)
    (if from-depth
        (setq level-depth (allout-depth))
      ;; at containing item:
      (setq start-point (point))
      (setq from-depth (allout-depth))
      (setq at-end (not (allout-next-heading))
            level-depth allout-recent-depth))

    ;; traverse the level, recursing on deeper levels:
    (while (and (not at-end)
                (> allout-recent-depth from-depth)
                (setq this-widget (allout-get-item-widget)))
      (if (< level-depth allout-recent-depth)
          ;; recurse:
          (progn
            (setq subgot (allout-chart-exposure-contour-by-icon level-depth)
                  at-end (car subgot)
                  subgot (cdr subgot))
            (if subgot (setq got (append subgot got))))
        ;; progress at this level:
        (when (memq (widget-get this-widget :icon-state) '(closed empty))
          (push (point) got)
          (allout-end-of-subtree))
        (setq at-end (not (allout-next-heading)))))

    ;; tailor result depending on whether or not we're a recursion:
    (if (not start-point)
        (cons at-end got)
      (goto-char start-point)
      got)))
;;;_   > allout-range-overlaps (from to ranges)
(defun allout-range-overlaps (from to ranges)
  "Return a pair indicating overlap of FROM and TO subtree range in RANGES.

First element of result indicates whether candidate range FROM, TO
overlapped any of the existing ranges.

Second element of result is a new version of RANGES incorporating the
candidate range with overlaps consolidated.

FROM and TO must be in increasing order, as must be the pairs in RANGES."
  ;; to append to the end: (rplacd next-to-last-cdr (list 'f))
  (let (new-ranges
        entry
        ;; the start of the range that includes the candidate from:
        included-from
        ;; the end of the range that includes the candidate to:
        included-to
        ;; the candidates were inserted:
        done)
    (while (and ranges (not done))
      (setq entry (car ranges)
            ranges (cdr ranges))

      (cond

       (included-from
        ;; some entry included the candidate from.
        (cond ((> (car entry) to)
               ;; current entry exceeds end of candidate range - done.
               (push (list included-from to) new-ranges)
               (push entry new-ranges)
               (setq included-to to
                     done t))
              ((>= (cadr entry) to)
               ;; current entry includes end of candidate range - done.
               (push (list included-from (cadr entry)) new-ranges)
               (setq included-to (cadr entry)
                     done t))
               ;; current entry contained in candidate range - ditch, continue:
              (t nil)))

       ((> (car entry) to)
        ;; current entry start exceeds candidate end - done, placed as new entry
        (push (list from to) new-ranges)
        (push entry new-ranges)
        (setq included-to to
              done t))

       ((>= (car entry) from)
        ;; current entry start is above candidate start, but not above
        ;; candidate end (by prior case).
        (setq included-from from)
        ;; now we have to check on whether this entry contains to, or continue:
        (when (>= (cadr entry) to)
          ;; current entry contains only candidate end - done:
          (push (list included-from (cadr entry)) new-ranges)
          (setq included-to (cadr entry)
                done t))
        ;; otherwise, we will continue to look for placement of candidate end.
        )

       ((>= (cadr entry) to)
        ;; current entry properly contains candidate range.
        (push entry new-ranges)
        (setq included-from (car entry)
              included-to (cadr entry)
              done t))

       ((>= (cadr entry) from)
        ;; current entry contains start of candidate range.
        (setq included-from (car entry)))

       (t
        ;; current entry is below the candidate range.
        (push entry new-ranges))))

    (cond ((and included-from included-to)
           ;; candidates placed.
           nil)
          ((not (or included-from included-to))
           ;; candidates found no place, must be at the end:
           (push (list from to) new-ranges))
          (included-from
           ;; candidate start placed but end not:
           (push (list included-from to) new-ranges))
          ;; might be included-to and not included-from, indicating new entry.
          )
    (setq new-ranges (nreverse new-ranges))
    (if ranges (setq new-ranges (append new-ranges ranges)))
    (list (if included-from t) new-ranges)))
;;;_   > allout-test-range-overlaps ()
(defun allout-test-range-overlaps ()
  "allout-range-overlaps unit tests."
  (let* (ranges
         got
         (try (lambda (from to)
                (setq got (allout-range-overlaps from to ranges))
                (setq ranges (cadr got))
                got)))
;;     ;; biggie:
;;     (setq ranges nil)
;;     ;; ~ .02 to .1 seconds for just repeated listing args instead of funcall
;;     ;; ~ 13 seconds for doing repeated funcall
;;     (message "time-trial: %s, resulting size %s"
;;              (time-trial
;;               '(let ((size 10000)
;;                      doing)
;;                  (random t)
;;                  (dotimes (count size)
;;                    (setq doing (random size))
;;                    (funcall try doing (+ doing (random 5)))
;;                    ;;(list doing (+ doing (random 5)))
;;                    )))
;;              (length ranges))
;;     (sit-for 2)

    ;; fresh:
    (setq ranges nil)
    (assert (equal (funcall try 3 5) '(nil ((3 5)))))
    ;; add range at end:
    (assert (equal (funcall try 10 12) '(nil ((3 5) (10 12)))))
    ;; add range at beginning:
    (assert (equal (funcall try 1 2) '(nil ((1 2) (3 5) (10 12)))))
    ;; insert range somewhere in the middle:
    (assert (equal (funcall try 7 9) '(nil ((1 2) (3 5) (7 9) (10 12)))))
    ;; consolidate some:
    (assert (equal (funcall try 5 8) '(t ((1 2) (3 9) (10 12)))))
    ;; add more:
    (assert (equal (funcall try 15 17) '(nil ((1 2) (3 9) (10 12) (15 17)))))
    ;; add more:
    (assert (equal (funcall try 20 22)
                   '(nil ((1 2) (3 9) (10 12) (15 17) (20 22)))))
    ;; encompass more:
    (assert (equal (funcall try 4 11) '(t ((1 2) (3 12) (15 17) (20 22)))))
    ;; encompass all:
    (assert (equal (funcall try 2 25) '(t ((1 25)))))

    ;; fresh slate:
    (setq ranges nil)
    (assert (equal (funcall try 20 25) '(nil ((20 25)))))
    (assert (equal (funcall try 30 35) '(nil ((20 25) (30 35)))))
    (assert (equal (funcall try 26 28) '(nil ((20 25) (26 28) (30 35)))))
    (assert (equal (funcall try 15 20) '(t ((15 25) (26 28) (30 35)))))
    (assert (equal (funcall try 10 30) '(t ((10 35)))))
    (assert (equal (funcall try 5 6) '(nil ((5 6) (10 35)))))
    (assert (equal (funcall try 2 100) '(t ((2 100)))))

    (setq ranges nil)
    ))
;;;_   > allout-widgetize-buffer (&optional doing)
(defun allout-widgetize-buffer (&optional doing)
  "EXAMPLE FUNCTION.  Widgetize items in buffer using allout-chart-subtree.

We economize by just focusing on the first of local-maximum depth siblings.

Optional DOING is for internal use - a chart of the current level, for
recursive operation."

  (interactive)
  (if (not doing)

      (save-excursion
        (goto-char (point-min))
        ;; Construct the chart by scanning the siblings:
        (dolist (top-level-sibling (allout-chart-siblings))
          (goto-char top-level-sibling)
          (let ((subchart (allout-chart-subtree)))
            (if subchart
                (allout-widgetize-buffer subchart)))))

    ;; save-excursion was done on recursion entry, not necessary here.
    (let (have-sublists)
      (dolist (sibling doing)
        (when (listp sibling)
          (setq have-sublists t)
          (allout-widgetize-buffer sibling)))
      (when (and (not have-sublists) (not (widget-at (car doing))))
        (goto-char (car doing))
        (allout-get-or-create-item-widget)))))

;;;_ : Item widget and constructors

;;;_  $ allout-item-widget
(define-widget 'allout-item-widget 'default
  "A widget presenting an allout outline item."

  'button        nil
  ;; widget-field-at respects this to get item if 'field is unused.
  ;; we don't use field to avoid collision with end-of-line, etc, on which
  ;; allout depends.
  'real-field    nil

  ;; data fields:


  ;; tailor the widget for a specific item
  :create         'allout-decorate-item-and-context
  :value-delete   'allout-widgets-undecorate-item
  ;; Not Yet Converted (from original, tree-widget stab)
  :expander       'allout-tree-event-dispatcher ; get children when nil :args
  :expander-p     'identity              ; always engage the :expander
  :action         'allout-tree-widget-action
  ;; :notify      "when item changes"

  ;; force decoration of item but not context, unless already done this tick:
  :redecorate     'allout-redecorate-item
  :last-decorated-tick nil
  ;; recognize the actual situation of the item's text:
  :parse-item           'allout-parse-item-at-point
  ;; decorate the entirety of the item, sans offspring:
  :decorate-item-span  'allout-decorate-item-span
  ;; decorate the various item elements:
  :decorate-guides     'allout-decorate-item-guides
  :decorate-icon       'allout-decorate-item-icon
  :decorate-cue        'allout-decorate-item-cue
  :decorate-body       'allout-decorate-item-body
  :actual-position     'allout-item-actual-position

  ;; Layout parameters:
  :is-container   nil   ; is this actually the encompassing file/connection?

  :from           nil                   ; item beginning - marker
  :to             nil                   ; item end - marker
  :span-overlay   nil   ; overlay by which actual position is determined

  ;; also serves as guide-end:
  :icon-start     nil
  :icon-end       nil
  :distinctive-start   nil
  ;; also serves as cue-start:
  :distinctive-end     nil
  ;; also serves as cue-end:
  :body-start     nil
  :body-end       nil
  :depth          nil
  :has-subitems   nil
  :was-has-subitems   'init
  :expanded       nil
  :was-expanded   'init
  :brief          nil
  :was-brief      'init

  :does-encrypt   nil           ; pending encryption when :is-encrypted false.
  :is-encrypted   nil

  ;; the actual location of the item text:
  :location       'allout-item-location

  :button-keymap  allout-item-icon-keymap ; XEmacs
  :keymap         allout-item-icon-keymap        ; Emacs

  ;; Element regions:
  :guides-span         nil
  :icon-span           nil
  :cue-span            nil
  :bullet              nil
  :was-bullet          nil
  :body-span           nil

  :body-brevity-p 'allout-body-brevity-p

  ;; :guide-column-flags indicate (in reverse order) whether or not the
  ;; item's ancestor at the depth corresponding to the column has a
  ;; subsequent sibling - ie, whether or not the corresponding column needs
  ;; a descender line to connect that ancestor with its sibling.
  :guide-column-flags  nil
  :was-guide-column-flags  'init

  ;; ie, has subitems:
  :populous-p    'allout-item-populous-p
  :help-echo     'allout-tree-widget-help-echo
  )
;;;_  > allout-new-item-widget ()
(defsubst allout-new-item-widget ()
  "create a new item widget, not yet situated anywhere."
  (if allout-widgets-maintain-tally
      ;; all the extra overhead is incurred only when doing the
      ;; maintenance, except the condition, which can't be avoided.
      (let ((widget (widget-convert 'allout-item-widget)))
        (puthash widget nil allout-widgets-tally)
        widget)
    (widget-convert 'allout-item-widget)))
;;;_  : Item decoration
;;;_   > allout-decorate-item-and-context (item-widget &optional redecorate
;;;                                                   blank-container parent)
(defun allout-decorate-item-and-context (item-widget &optional redecorate
                                                     blank-container parent)
  "Create or adjust widget decorations for ITEM-WIDGET and neighbors at point.

The neighbors include its siblings and parent.

ITEM-WIDGET can be a created or converted allout-item-widget.

If you're only trying to get or create a widget for an item, use
`allout-get-or-create-item-widget'.  If you have the item-widget, applying
:redecorate will do the right thing.

Optional BLANK-CONTAINER is for internal use.  It is used to fabricate a
container widget for an empty-bodied container, in the course of decorating
a proper \(non-container\) item which starts at the beginning of the file.

Optional REDECORATE causes redecoration of the item-widget and
its siblings, even if already decorated in this cycle of the command loop.

Optional PARENT, when provided, bypasses some navigation and computation
necessary to obtain the parent of the items being processed.

We return the item-widget corresponding to the item at point."

  (when (or redecorate
            (not (equal (widget-get item-widget :last-decorated-tick)
                        allout-command-counter)))
    (let* ((allout-inhibit-body-modification-hook t)
           (was-modified (buffer-modified-p))
           (was-point (point))
           prefix-start
           (is-container (or blank-container
                             (not (setq prefix-start (allout-goto-prefix)))
                             (< was-point prefix-start)))
           ;; steady-point (set in two steps) is reliable across parent
           ;; widget-creation.
           (steady-point (progn (if is-container (goto-char 1))
                                (point-marker)))
           (steady-point (progn (set-marker-insertion-type steady-point t)
                                steady-point))
           (parent (and (not is-container)
                        (allout-get-or-create-parent-widget)))
           parent-flags parent-depth
           successor-sibling
           body
           doing-item
           sub-item-widget
           depth
           reverse-siblings-chart
           (buffer-undo-list t))

      ;; At this point the parent is decorated and parent-flags indicate
      ;; its guide lines.  We will iterate over the siblings according to a
      ;; chart we create at the start, and going from last to first so we
      ;; don't have to worry about text displacement caused by widgetizing.

      (if is-container
          (progn (widget-put item-widget :is-container t)
                 (setq reverse-siblings-chart (list 1)))
        (goto-char (widget-apply parent :actual-position :from))
        (if (widget-get parent :is-container)
            ;; `allout-goto-prefix' will go to first non-container item:
            (allout-goto-prefix)
          (allout-next-heading))
        (setq depth (allout-recent-depth))
        (setq reverse-siblings-chart (list allout-recent-prefix-beginning))
        (while (allout-next-sibling)
          (push allout-recent-prefix-beginning reverse-siblings-chart)))

      (dolist (doing-at reverse-siblings-chart)
        (goto-char doing-at)
        (when allout-widgets-track-decoration
          (sit-for 0))

        (setq doing-item (if (= doing-at steady-point)
                             item-widget
                           (or (allout-get-item-widget)
                               (allout-new-item-widget))))

        (when (or redecorate (not (equal (widget-get doing-item
                                                     :last-decorated-tick)
                                         allout-command-counter)))
          (widget-apply doing-item :parse-item t blank-container)
          (widget-apply doing-item :decorate-item-span)

          (widget-apply doing-item :decorate-guides
                        parent (and successor-sibling t))
          (widget-apply doing-item :decorate-icon)
          (widget-apply doing-item :decorate-cue)
          (widget-apply doing-item :decorate-body)

          (widget-put doing-item :last-decorated-tick allout-command-counter))

        (setq successor-sibling doing-at))

      (set-buffer-modified-p was-modified)
      (goto-char steady-point)
      ;; must null the marker or the buffer gets clogged with impedance:
      (set-marker steady-point nil)

      item-widget)))
;;;_   > allout-redecorate-item (item)
(defun allout-redecorate-item (item-widget)
  "Resituate ITEM-WIDGET decorations, disregarding context.

Use this to redecorate only the item, when you know that its
situation with respect to siblings, parent, and offspring is
unchanged from its last decoration.  Use
`allout-decorate-item-and-context' instead to reassess and adjust
relevant context, when suitable."
  (if (not (equal (widget-get item-widget :last-decorated-tick)
                  allout-command-counter))
      (let ((was-modified (buffer-modified-p))
            (buffer-undo-list t))
        (widget-apply item-widget :parse-item)
        (widget-apply item-widget :decorate-guides)
        (widget-apply item-widget :decorate-icon)
        (widget-apply item-widget :decorate-cue)
        (widget-apply item-widget :decorate-body)
        (set-buffer-modified-p was-modified))))
;;;_   > allout-redecorate-visible-subtree (&optional parent-widget
;;;                                                  depth chart)
(defun allout-redecorate-visible-subtree (&optional parent-widget depth chart)
  "Redecorate all visible items in subtree at point.

Optional PARENT-WIDGET is for optimization, when the parent
widget is already available.

Optional DEPTH restricts the excursion depth of covered.

Optional CHART is for internal recursion, to carry a chart of the
target items.

Point is left at the last sibling in the visible subtree."
  ;; using a treatment that takes care of all the siblings on a level, we
  ;; only need apply it to the first sibling on the level, and we can
  ;; collect and pass the parent of the lower levels to recursive calls as
  ;; we go.
  (let ((parent-widget
         (if (and parent-widget (widget-apply parent-widget
                                              :actual-position :from))
             (progn (goto-char (widget-apply parent-widget
                                             :actual-position :from))
                    parent-widget)
           (let ((got (allout-get-item-widget)))
             (if got
                 (allout-decorate-item-and-context got 'redecorate)
               (allout-get-or-create-item-widget 'redecorate)))))
        (pending-chart (or chart (allout-chart-subtree nil 'visible)))
        item-widget
        previous-sibling-point
        previous-sibling
        recent-sibling-point)
    (setq pending-chart (nreverse pending-chart))
    (dolist (sibling-point pending-chart)
      (cond ((integerp sibling-point)
             (when (not previous-sibling-point)
               (goto-char sibling-point)
               (if (setq item-widget (allout-get-item-widget nil))
                   (allout-decorate-item-and-context item-widget 'redecorate
                                                     nil parent-widget)
                 (allout-get-or-create-item-widget)))
             (setq previous-sibling-point sibling-point
                   recent-sibling-point sibling-point))
            ((listp sibling-point)
             (if (or (not depth)
                     (> depth 1))
                 (allout-redecorate-visible-subtree
                  (if (not previous-sibling-point)
                      ;; containment discontinuity - sigh
                      parent-widget
                    (allout-get-or-create-item-widget 'redecorate))
                  (if depth (1- depth))
                  sibling-point)))))
    (if (and recent-sibling-point (< (point) recent-sibling-point))
        (goto-char recent-sibling-point))))
;;;_   > allout-parse-item-at-point (item-widget &optional at-beginning
;;;                                                       blank-container)
(defun allout-parse-item-at-point (item-widget &optional at-beginning
                                                         blank-container)
  "Set widget ITEM-WIDGET layout parameters per item-at-point's actual layout.

If optional AT-BEGINNING is t, then point is assumed to be at the start of
the item prefix.

If optional BLANK-CONTAINER is true, then the parameters of a container
which has an empty body are set.  \(Though the body is blank, the object
may have subitems.\)"

  ;; Uncomment this sit-for to notice where decoration is happening:
;;  (sit-for .1)
  (let* ((depth (allout-depth))
         (depth (if blank-container 0 depth))
         (is-container (or blank-container (zerop depth)))

         (does-encrypt (and (not is-container)
                            (allout-encrypted-type-prefix)))
         (is-encrypted (and does-encrypt (allout-encrypted-topic-p)))
         (icon-end allout-recent-prefix-end)
         (icon-start (1- icon-end))
         body-start
         body-end
         bullet
         has-subitems
         (contents-depth (1+ depth))
         )
    (widget-put item-widget :depth depth)
    (if is-container

        (progn
          (widget-put item-widget :from (allout-set-boundary-marker
                                         :from (point-min)
                                         (widget-get item-widget :from)))
          (widget-put item-widget :icon-end nil)
          (widget-put item-widget :icon-start nil)
          (setq body-start (widget-put item-widget :body-start 1)))

      ;; not container:

      (widget-put item-widget :from (allout-set-boundary-marker
                                     :from (if at-beginning
                                               (point)
                                             allout-recent-prefix-beginning)
                                     (widget-get item-widget :from)))
      (widget-put item-widget :icon-start icon-start)
      (widget-put item-widget :icon-end icon-end)
      (when does-encrypt
        (widget-put item-widget :does-encrypt t)
        (widget-put item-widget :is-encrypted is-encrypted))

      ;; cue area:
      (setq body-start icon-end)
      (widget-put item-widget :bullet (setq bullet (allout-get-bullet)))
      (if (equal (char-after body-start) ? )
          (setq body-start (1+ body-start)))
      (widget-put item-widget :body-start body-start)
      )

    ;; Both container and regular items:

    ;; :body-end (doesn't include a trailing blank line, if any) -
    (widget-put item-widget :body-end (setq body-end
                                            (if blank-container
                                                1
                                              (allout-end-of-entry))))

    (widget-put item-widget :to (allout-set-boundary-marker
                                 :to (if blank-container
                                         (point-min)
                                       (or (allout-pre-next-prefix)
                                           (goto-char (point-max))))
                                 (widget-get item-widget :to)))
    (widget-put item-widget :has-subitems
                (setq has-subitems
                      (and
                       ;; has a subsequent item:
                       (not (= body-end (point-max)))
                       ;; subsequent item is deeper:
                       (< depth (setq contents-depth (allout-recent-depth))))))
    ;; note :expanded - true if widget item's content is currently visible?
    (widget-put item-widget :expanded
                (and has-subitems
                     ;; subsequent item is or isn't visible:
                     (save-excursion
                       (goto-char allout-recent-prefix-beginning)
                       (not (allout-hidden-p)))))))
;;;_   > allout-set-boundary-marker (boundary position &optional current-marker)
(defun allout-set-boundary-marker (boundary position &optional current-marker)
  "Set or create item widget BOUNDARY type marker at POSITION.

Optional CURRENT-MARKER is the marker currently being used for
the boundary, if any.

BOUNDARY type is either :from or :to, determining the marker insertion type."
  (if (not position) (setq position (point)))
  (if current-marker
      (set-marker current-marker position)
    (let ((marker (make-marker)))
      ;; XXX dang - would like for :from boundary to advance after inserted
      ;;     text, but that would omit new header prefixes when allout
      ;;     relevels, etc.  this competes with ad-hoc edits, which would
      ;;     better be omitted
      (set-marker-insertion-type marker nil)
      (set-marker marker position))))
;;;_   > allout-decorate-item-span (item-widget)
(defun allout-decorate-item-span (item-widget)
  "Equip the item with a span, as an entirety.

This span is implemented so it can be used to detect displacement
of the widget in absolute terms, and provides an offset bias for
the various element spans."

  (if (and (widget-get item-widget :is-container)
           ;; the only case where the span could be empty.
           (eq (widget-get item-widget :from)
               (widget-get item-widget :to)))
      nil
    (allout-item-span item-widget
                      (widget-get item-widget :from)
                      (widget-get item-widget :to))))
;;;_   > allout-decorate-item-guides (item-widget
;;;                                  &optional parent-widget has-successor)
(defun allout-decorate-item-guides (item-widget
                                    &optional parent-widget has-successor)
  "Add ITEM-WIDGET guide icon-prefix descender and connector text properties.

Optional arguments provide context for deriving the guides.  In
their absence, the current guide column flags are used.

Optional PARENT-WIDGET is the widget for the item's parent item.

Optional HAS-SUCCESSOR is true iff the item is followed by a sibling.

We also hide the header-prefix string.

Guides are established according to the item-widget's :guide-column-flags,
when different than :was-guide-column-flags.  Changing that property and
reapplying this method will rectify the glyphs."

  (when (not (widget-get item-widget :is-container))
    (let* ((depth (widget-get item-widget :depth))
           (parent-depth (and parent-widget
                              (widget-get parent-widget :depth)))
           (parent-flags (and parent-widget
                              (widget-get parent-widget :guide-column-flags)))
           (parent-flags-depth (length parent-flags))
           (extender-length (- depth (+ parent-flags-depth 2)))
           (flags (or (and (> depth 1)
                           parent-widget
                           (widget-put item-widget :guide-column-flags
                                       (append (list has-successor)
                                               (if (< 0 extender-length)
                                                   (make-list extender-length
                                                              '-))
                                               parent-flags)))
                      (widget-get item-widget :guide-column-flags)))
           (was-flags (widget-get item-widget :was-guide-column-flags))
           (guides-start (widget-get item-widget :from))
           (guides-end (widget-get item-widget :icon-start))
           (position guides-start)
           (increment (length allout-header-prefix))
           reverse-flags
           guide-name
           extenders paint-extenders
           (inhibit-read-only t))

      (when (not (equal was-flags flags))

        (setq reverse-flags (reverse flags))
        (while reverse-flags
          (setq guide-name
                (cond ((null (cdr reverse-flags))
                       (if (car reverse-flags)
                           'mid-connector
                         'end-connector))
                      ((eq (car reverse-flags) '-)
                       ;; accumulate extenders tally, to be painted on next
                       ;; non-extender flag, according to the flag type.
                       (setq extenders (1+ (or extenders 0)))
                       nil)
                      ((car reverse-flags)
                       'through-descender)
                      (t 'skip-descender)))
          (when guide-name
            (put-text-property position (setq position (+ position increment))
                               'display (allout-fetch-icon-image guide-name))
            (if (> increment 1) (setq increment 1))
            (when extenders
              ;; paint extenders after a connector, else leave spaces.
              (dotimes (i extenders)
                (put-text-property
                 position (setq position (1+ position))
                 'display (allout-fetch-icon-image
                           (if (memq guide-name '(mid-connector end-connector))
                               'extender-connector
                             'skip-descender))))
              (setq extenders nil)))
          (setq reverse-flags (cdr reverse-flags)))
        (widget-put item-widget :was-guide-column-flags flags))

      (allout-item-element-span-is item-widget :guides-span
                                guides-start guides-end))))
;;;_   > allout-decorate-item-icon (item-widget)
(defun allout-decorate-item-icon (item-widget)
  "Add item icon glyph and distinctive bullet text properties to ITEM-WIDGET."

  (when (not (widget-get item-widget :is-container))
    (let* ((icon-start (widget-get item-widget :icon-start))
           (icon-end (widget-get item-widget :icon-end))
           (bullet (widget-get item-widget :bullet))
           (use-bullet bullet)
           (was-bullet (widget-get item-widget :was-bullet))
           (distinctive (allout-distinctive-bullet bullet))
           (distinctive-start (widget-get item-widget :distinctive-start))
           (distinctive-end (widget-get item-widget :distinctive-end))
           (does-encrypt (widget-get item-widget :does-encrypt))
           (is-encrypted (and does-encrypt (widget-get item-widget
                                                       :is-encrypted)))
           (expanded (widget-get item-widget :expanded))
           (has-subitems (widget-get item-widget :has-subitems))
           (inhibit-read-only t)
           icon-state)

      (when (not (and (equal (widget-get item-widget :was-expanded) expanded)
                      (equal (widget-get item-widget :was-has-subitems)
                             has-subitems)
                      (equal (widget-get item-widget :was-does-encrypt)
                             does-encrypt)
                      (equal (widget-get item-widget :was-is-encrypted)
                             is-encrypted)))

        (setq icon-state
              (cond (does-encrypt (if is-encrypted
                                      'locked-encrypted
                                    'unlocked-encrypted))
                    (expanded 'opened)
                    (has-subitems 'closed)
                    (t 'empty)))
        (put-text-property icon-start (1+ icon-start)
                           'display (allout-fetch-icon-image icon-state))
        (widget-put item-widget :was-expanded expanded)
        (widget-put item-widget :was-has-subitems has-subitems)
        (widget-put item-widget :was-does-encrypt does-encrypt)
        (widget-put item-widget :was-is-encrypted is-encrypted)
        ;; preserve as a widget property to track last known:
        (widget-put item-widget :icon-state icon-state)
        ;; preserve as a text property to track undo:
        (put-text-property icon-start icon-end :icon-state icon-state))
      (allout-item-element-span-is item-widget :icon-span
                                   icon-start icon-end)
      (when (not (string= was-bullet bullet))
        (cond ((not distinctive)
               ;; XXX we strip the prior properties without even checking if
               ;;     the prior bullet was distinctive, because the widget
               ;;     provisions to convey that info is disappearing, sigh.
               (remove-text-properties icon-end (1+ icon-end) '(display))
               (setq distinctive-start icon-end distinctive-end icon-end)
               (widget-put item-widget :distinctive-start distinctive-start)
               (widget-put item-widget :distinctive-end distinctive-end))

              ((not (string= bullet allout-numbered-bullet))
               (setq distinctive-start icon-end distinctive-end (+ icon-end 1)))

              (does-encrypt
               (setq distinctive-start icon-end distinctive-end (+ icon-end 1)))

              (t
               (goto-char icon-end)
               (looking-at "[0-9]+")
               (setq use-bullet (buffer-substring icon-end (match-end 0)))
               (setq distinctive-start icon-end
                     distinctive-end (match-end 0))))
          (put-text-property distinctive-start distinctive-end 'display
                             use-bullet)
          (widget-put item-widget :was-bullet bullet)
          (widget-put item-widget :distinctive-start distinctive-start)
          (widget-put item-widget :distinctive-end distinctive-end)))))
;;;_   > allout-decorate-item-cue (item-widget)
(defun allout-decorate-item-cue (item-widget)
  "Incorporate space between bullet icon and body to the ITEM-WIDGET."
  ;; NOTE: most of the cue-area

  (when (not (widget-get item-widget :is-container))
    (let* ((cue-start (or (widget-get item-widget :distinctive-end)
                          (widget-get item-widget :icon-end)))
           (body-start (widget-get item-widget :body-start))
           (expanded (widget-get item-widget :expanded))
           (has-subitems (widget-get item-widget :has-subitems))
           (inhibit-read-only t))

      (allout-item-element-span-is item-widget :cue-span cue-start body-start)
      (put-text-property (1- body-start) body-start 'rear-nonsticky t))))
;;;_   > allout-decorate-item-body (item-widget &optional force)
(defun allout-decorate-item-body (item-widget &optional force)
  "Incorporate item body text as part the ITEM-WIDGET.

Optional FORCE means force reassignment of the region property."

  (let* ((allout-inhibit-body-modification-hook t)
         (body-start (widget-get item-widget :body-start))
         (body-end (widget-get item-widget :body-end))
         (body-text-end body-end)
         (inhibit-read-only t))

    (allout-item-element-span-is item-widget :body-span
                              body-start (min (1+ body-end) (point-max))
                              force)))
;;;_   > allout-item-actual-position (item-widget field)
(defun allout-item-actual-position (item-widget field)
  "Return ITEM-WIDGET FIELD position taking item displacement into account."

  ;; The item's sub-element positions (:icon-end, :body-start, etc) are
  ;; accurate when the item is parsed, but some offsets from the start
  ;; drift with text added in the body.
  ;;
  ;; Rather than reparse an item with every change (inefficient), or derive
  ;; every position from a distinct field marker/overlay (prohibitive as
  ;; the number of items grows), we use the displacement tracking of the
  ;; :span-overlay's markers, against the registered :from or :body-end
  ;; (depending on whether the requested field value is before or after the
  ;; item body), to bias the registered values.
  ;;
  ;; This is not necessary/useful when the item is being decorated, because
  ;; that always must be preceded by a fresh item parse.

  (if (not (eq field :body-end))
      (widget-get item-widget :from)

    (let* ((span-overlay (widget-get item-widget :span-overlay))
           (body-end-position (widget-get item-widget :body-end))
           (ref-marker-position (and span-overlay
                                     (overlay-end span-overlay)))
           (offset (and body-end-position span-overlay
                        (- (or ref-marker-position 0)
                           body-end-position))))
      (+ (widget-get item-widget field) (or offset 0)))))
;;;_  : Item undecoration
;;;_   > allout-widgets-undecorate-region (start end)
(defun allout-widgets-undecorate-region (start end)
  "Eliminate widgets and decorations for all items in region from START to END."
  (let ((next start)
        widget)
    (save-excursion
      (goto-char start)
      (while (<  (setq next (next-single-char-property-change next
                                                              'display
                                                              (current-buffer)
                                                              end))
                 end)
        (goto-char next)
        (when (setq widget (allout-get-item-widget))
          ;; if the next-property/overly progression got us to a widget:
          (allout-widgets-undecorate-item widget t))))))
;;;_   > allout-widgets-undecorate-text (text)
(defun allout-widgets-undecorate-text (text)
  "Eliminate widgets and decorations for all items in TEXT."
  (remove-text-properties 0 (length text)
                          '(display nil :icon-state nil rear-nonsticky nil
                            category nil button nil field nil)
                          text)
  text)
;;;_   > allout-widgets-undecorate-item (item-widget &optional no-expose)
(defun allout-widgets-undecorate-item (item-widget &optional no-expose)
  "Remove widget decorations from ITEM-WIDGET.

Any concealed content head lines and item body is exposed, unless
optional NO-EXPOSE is non-nil."
  (let ((from (widget-get item-widget :from))
        (to (widget-get item-widget :to))
        (text-properties-to-remove '(display nil
                                     :icon-state nil
                                     rear-nonsticky nil
                                     category nil
                                     button nil
                                     field nil))
        (span-overlay (widget-get item-widget :span-overlay))
        (button-overlay (widget-get item-widget :button))
        (was-modified (buffer-modified-p))
        (buffer-undo-list t)
	(inhibit-read-only t))
    (if (not no-expose)
        (allout-flag-region from to nil))
    (allout-unprotected
     (remove-text-properties from to text-properties-to-remove))
    (when span-overlay
      (delete-overlay span-overlay) (widget-put item-widget :span-overlay nil))
    (when button-overlay
      (delete-overlay button-overlay) (widget-put item-widget :button nil))
    (set-marker from nil)
    (set-marker to nil)
    (if (not was-modified)
        (set-buffer-modified-p nil))))

;;;_  : Item decoration support
;;;_   > allout-item-span (item-widget &optional start end)
(defun allout-item-span (item-widget &optional start end)
  "Return or register the location of an ITEM-WIDGET's actual START and END.

If START and END are not passed in, return either a dotted pair
of the current span, if established, or nil if not yet set.

When the START and END are passed, return the distance that the
start of the item moved.  We return 0 if the span was not
previously established or is not moved."
  (let ((overlay (widget-get item-widget :span-overlay))
        was-start was-end
        changed)
    (cond ((not overlay) (when start
                           (setq overlay (make-overlay start end nil t nil))
                           (overlay-put overlay 'button item-widget)
                           (overlay-put overlay 'evaporate t)
                           (widget-put item-widget :span-overlay overlay)
                           t))
          ;; report:
          ((not start) (cons (overlay-start overlay) (overlay-end overlay)))
          ;; move:
          ((or (not (equal (overlay-start overlay) start))
               (not (equal (overlay-end overlay) end)))
           (move-overlay overlay start end)
           t)
          ;; specified span already set:
          (t nil))))
;;;_   > allout-item-element-span-is (item-widget element
;;;                               &optional start end force)
(defun allout-item-element-span-is (item-widget element
                                             &optional start end force)
  "Return or register the location of the indicated ITEM-WIDGET ELEMENT.

ELEMENT is one of :guides-span, :icon-span, :cue-span, or :body-span.

When optional START is specified, optional END must also be.

START and END are the actual bounds of the region, if provided.

If START and END are not passed in, we return either a dotted
pair of the current span, if established, or nil if not yet set.

When the START and END are passed, we return t if the region
changed or nil if not.

Optional FORCE means force assignment of the region's text
property, even if it's already set."
  (let ((span (widget-get item-widget element)))
    (cond ((or (not span) force)
           (when start
             (widget-put item-widget element (cons start end))
             (put-text-property start end 'category
                                (cdr (assoc element
                                            allout-span-to-category)))
             t))
          ;; report:
          ((not start) span)
          ;; move if necessary:
          ((not (and (eq (car span) start)
                     (eq (cdr span) end)))
           (widget-put item-widget element span)
           t)
          ;; specified span already set:
          (t nil))))
;;;_  : Item widget retrieval (/ high-level creation):
;;;_   > allout-get-item-widget (&optional container)
(defun allout-get-item-widget (&optional container)
  "Return the widget for the item at point, or nil if no widget yet exists.

Point must be situated *before* the start of the target item's
body, so we don't get an existing containing item when we're in
the process of creating an item in the middle of another.

Optional CONTAINER is used to obtain the container item."
  (if (or container (zerop (allout-depth)))
      allout-container-item-widget
    ;; allout-recent-* are calibrated by (allout-depth) if we got here.
    (let ((got (widget-at allout-recent-prefix-beginning)))
      (if (and got (listp got))
          (if (marker-position (widget-get got :from))
              (and
               (>= (point) (widget-apply got :actual-position :from))
               (<= (point) (widget-apply got :actual-position :body-start))
               got)
            ;; a wacky residual item - undecorate and disregard:
            (allout-widgets-undecorate-item got)
            nil)))))
;;;_   > allout-get-or-create-item-widget (&optional redecorate blank-container)
(defun allout-get-or-create-item-widget (&optional redecorate blank-container)
  "Return a widget for the item at point, creating the widget if necessary.

When creating a widget, we assume there has been a context change
and decorate its siblings and parent, as well.

Optional BLANK-CONTAINER is for internal use, to fabricate a
meta-container item with an empty body when the first proper
\(non-container\) item starts at the beginning of the file.

Optional REDECORATE, if non-nil, means to redecorate the widget
if it already exists."
  (let ((widget (allout-get-item-widget blank-container))
        (buffer-undo-list t))
    (cond (widget (if redecorate
                      (allout-redecorate-item widget))
                  widget)
          ((or blank-container (zerop (allout-depth)))
           (or allout-container-item-widget
               (setq allout-container-item-widget
                     (allout-decorate-item-and-context
                      (widget-convert 'allout-item-widget)
                      nil blank-container))))
          ;; create a widget for a regular/non-container item:
          (t (allout-decorate-item-and-context (widget-convert
                                                'allout-item-widget))))))
;;;_   > allout-get-or-create-parent-widget (&optional redecorate)
(defun allout-get-or-create-parent-widget (&optional redecorate)
  "Return widget for parent of item at point, decorating it if necessary.

We return the container widget if we're above the first proper item in the
file.

Optional REDECORATE, if non-nil, means to redecorate the widget if it
already exists.

Point will wind up positioned on the beginning of the parent or beginning
of the buffer."
  ;; use existing widget, if there, else establish it
  (if (or (bobp) (and (not (allout-ascend))
                      (looking-at allout-regexp)))
      (allout-get-or-create-item-widget redecorate 'blank-container)
    (allout-get-or-create-item-widget redecorate)))
;;;_  : X- Item ancillaries
;;;_   >X allout-body-modification-handler (beg end)
(defun allout-body-modification-handler (beg end)
  "Do routine processing of body text before and after modification.

Operation is inhibited by `allout-inhibit-body-modification-handler'."

;; The primary duties are:
;;
;; - marking of escaped prefix-like text for delayed cleanup of escapes
;; - removal and replacement of the settings
;; - maintenance of beginning-of-line guide lines
;;
;; ?? Escapes removal \(before changes\) is not done when edits span multiple
;; items, recognizing that item structure is being preserved, including
;; escaping of item-prefix-like text within bodies.  See
;; `allout-before-modification-handler' and
;; `allout-inhibit-body-modification-handler'.
;;
;; Adds the overlay to the `allout-unresolved-body-mod-workhash' during
;; before-change operation, and removes from that list during after-change
;; operation.
  (cond (allout-inhibit-body-modification-hook nil)))
;;;_   >X allout-graphics-modification-handler (beg end)
(defun allout-graphics-modification-handler (beg end)
  "Protect against incoherent deletion of decoration graphics.

Deletes allowed only when inhibit-read-only is t."
  (cond
   (undo-in-progress (when (eq (get-text-property beg 'category)
                               'allout-icon-span-category)
                       (save-excursion
                         (goto-char beg)
                         (let* ((item-widget (allout-get-item-widget)))
                           (if item-widget
                               (allout-widgets-exposure-undo-recorder
                                item-widget))))))
   (inhibit-read-only t)
   ((not  (and (boundp 'allout-mode) allout-mode)) t)
   ((equal this-command 'quoted-insert) t)
   ((yes-or-no-p "Unruly edit of outline structure - allow? ")
    (setq allout-widgets-unset-inhibit-read-only (not inhibit-read-only)
          inhibit-read-only t))
   (t (error
       (substitute-command-keys allout-structure-unruly-deletion-message)))))
;;;_   > allout-item-icon-key-handler ()
(defun allout-item-icon-key-handler ()
  "Catchall handling of key bindings in item icon/cue hot-spots.

Applies `allout-hotspot-key-handler' and calls the result, if any, as an
interactive command."

  (interactive)
  (let* ((mapped-binding (allout-hotspot-key-handler)))
    (when mapped-binding
      (call-interactively mapped-binding))))

;;;_ : Status
;;;_  . allout-item-location (item-widget)
(defun allout-item-location (item-widget)
  "Location of the start of the item's text."
  (overlay-start (widget-get item-widget :span-overlay)))

;;;_ : Icon management
;;;_  > allout-fetch-icon-image (name)
(defun allout-fetch-icon-image (name)
  "Fetch allout icon for symbol NAME.

We use a caching strategy, so the caller doesn't need to do so."
  (let* ((types allout-widgets-icon-types)
         (use-dir (if (equal (allout-frame-property nil 'background-mode)
                             'light)
                      allout-widgets-icons-light-subdir
                    allout-widgets-icons-dark-subdir))
         (key (list name use-dir))
         (got (assoc key allout-widgets-icons-cache)))
    (if got
        ;; display system shows only first of subsequent adjacent
        ;; `eq'-identical repeats - use copies to avoid this problem.
        (allout-widgets-copy-list (cadr got))
      (while (and types (not got))
        (setq got
              (allout-find-image
               (list (append (list :type (car types)
                                   :file (concat use-dir
                                                 (symbol-name name)
                                                 "." (symbol-name
                                                      (car types))))
                             (if (featurep 'xemacs)
                                 allout-widgets-item-image-properties-xemacs
                               allout-widgets-item-image-properties-emacs)
                             ))))
        (setq types (cdr types)))
      (if got
          (push (list key got) allout-widgets-icons-cache))
      got)))

;;;_ : Miscellaneous
;;;_  > allout-elapsed-time-seconds (triple)
(defun allout-elapsed-time-seconds (end start)
  "Return seconds between `current-time' style time START/END triples."
  (let ((elapsed (time-subtract end start)))
    (float-time elapsed)))
;;;_  > allout-frame-property (frame property)
(defalias 'allout-frame-property
  (cond ((fboundp 'frame-parameter)
         'frame-parameter)
        ((fboundp 'frame-property)
         'frame-property)
        (t nil)))
;;;_  > allout-find-image (specs)
(defalias 'allout-find-image
  (if (fboundp 'find-image)
      'find-image
    nil)                                ; aka, not-yet-implemented for xemacs.
)
;;;_  > allout-widgets-copy-list (list)
(defun allout-widgets-copy-list (list)
  ;; duplicated from cl.el 'copy-list' as of 2008-08-17
  "Return a copy of LIST, which may be a dotted list.
The elements of LIST are not copied, just the list structure itself."
  (if (consp list)
      (let ((res nil))
	(while (consp list) (push (pop list) res))
	(prog1 (nreverse res) (setcdr res list)))
    (car list)))
;;;_  . allout-widgets-count-buttons-in-region (start end)
(defun allout-widgets-count-buttons-in-region (start end)
  "Debugging/diagnostic tool - count overlays with 'button' property in region."
  (interactive "r")
  (setq start (or start (point-min))
        end (or end (point-max)))
  (if (> start end) (let ((interim start)) (setq start end end interim)))
  (let ((button-overlays (delq nil
                               (mapcar (function (lambda (o)
                                                   (if (overlay-get o 'button)
                                                       o)))
                                       (overlays-in start end)))))
    (length button-overlays)))

;;;_ : Run unit tests:
(defun allout-widgets-run-unit-tests ()
  (message "Running allout-widget tests...")

  (allout-test-range-overlaps)

  (message "Running allout-widget tests...  Done.")
  (sit-for .5))

(when allout-widgets-run-unit-tests-on-load
  (allout-widgets-run-unit-tests))

;;;_ : provide
(provide 'allout-widgets)

;;;_. Local emacs vars.
;;;_ , Local variables:
;;;_ , allout-layout: (-1 : 0)
;;;_ , End:
