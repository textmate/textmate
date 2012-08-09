;;; viper.el --- A full-featured Vi emulator for Emacs and XEmacs,
;;		 a VI Plan for Emacs Rescue,
;;		 and a venomous VI PERil.
;;		 Viper Is also a Package for Emacs Rebels.

;; Copyright (C) 1994-2012 Free Software Foundation, Inc.

;; Author: Michael Kifer <kifer@cs.stonybrook.edu>
;; Keywords: emulations
;; Version: 3.14.1

;; Yoni Rabkin <yoni@rabkins.net> contacted the maintainer of this
;; file on 20/3/2008, and the maintainer agreed that when a bug is
;; filed in the Emacs bug reporting system against this file, a copy
;; of the bug report be sent to the maintainer's email address.

(defconst viper-version "3.14.1 of August 15, 2009"
  "The current version of Viper")

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

;; Viper is a full-featured Vi emulator for Emacs and XEmacs.  It emulates and
;; improves upon the standard features of Vi and, at the same time, allows
;; full access to all Emacs facilities.  Viper supports multiple undo,
;; file name completion, command, file, and search history and it extends
;; Vi in many other ways.  Viper is highly customizable through the various
;; hooks, user variables, and keymaps.  It is implemented as a collection
;; of minor modes and it is designed to provide full access to all Emacs
;; major and minor modes.
;;
;;; History:
;;
;; Viper is a new name for a package formerly known as VIP-19,
;; which was a successor of VIP version 3.5 by Masahiko Sato
;; <ms@sail.stanford.edu> and VIP version 4.2 by Aamod Sane
;; <sane@cs.uiuc.edu>.  Some ideas from vip 4.4.2 by Aamod Sane
;; were also shamelessly plagiarized.
;;
;; Viper maintains some degree of compatibility with these older
;; packages.  See the documentation for customization.
;;
;; The main difference between Viper and these older packages are:
;;
;; 1. Viper emulates Vi at several levels, from almost complete conformity
;;    to a rather loose Vi-compliance.
;;
;; 2. Viper provides full access to all major and minor modes of Emacs
;;    without the need to type extra keys.
;;    The older versions of VIP (and other Vi emulators) do not work with
;;    some major and minor modes.
;;
;; 3. Viper supports vi-style undo.
;;
;; 4. Viper fully emulates (and improves upon) vi's replacement mode.
;;
;; 5. Viper has a better interface to ex, including command, variable, and
;;    file name completion.
;;
;; 6. Viper uses native Emacs history and completion features; it doesn't
;;    rely on other packages (such as gmhist.el and completer.el) to provide
;;    these features.
;;
;; 7. Viper supports Vi-style editing in the minibuffer, by allowing the
;;    user to switch from Insert state to Vi state to Replace state, etc.
;;
;; 8. Viper keeps history of recently inserted pieces of text and recently
;;    executed Vi-style destructive commands, such as `i', `d', etc.
;;    These pieces of text can be inserted in later insertion commands;
;;    the previous destructive commands can be re-executed.
;;
;; 9. Viper has Vi-style keyboard macros, which enhances the similar
;;    facility in the original Vi.
;;    First, one can execute any Emacs command while defining a
;;    macro, not just the Vi commands.  Second, macros are defined in a
;;    WYSYWYG mode, using an interface to Emacs's WYSIWYG style of defining
;;    macros.  Third, in Viper, one can define macros that are specific to
;;    a given buffer, a given major mode, or macros defined for all buffers.
;;    The same macro name can have several different definitions:
;;    one global, several definitions for various major modes, and
;;    definitions for specific buffers.
;;    Buffer-specific definitions override mode-specific
;;    definitions, which, in turn, override global definitions.
;;
;;
;;; Installation:
;;  -------------
;;
;;  (require 'viper)
;;

;;; Acknowledgements:
;;  -----------------
;;  Bug reports and ideas contributed by many users have helped
;;  improve Viper and the various versions of VIP.
;;  See the on-line manual for a complete list of contributors.
;;
;;
;;; Notes:
;;
;; 1. Major modes.
;; In most cases, Viper handles major modes correctly, i.e., they come up
;; in the right state (either  vi-state or emacs-state).  For instance, text
;; files come up in vi-state, while, say, Dired appears in emacs-state by
;; default.
;; However, some modes do not appear in the right mode in the beginning,
;; usually because they neglect to follow Emacs conventions (e.g., they don't
;; use kill-all-local-variables when they start).  Some major modes
;; may fail to come up in emacs-state if they call hooks, such as
;; text-hook, for no good reason.
;;
;; As an immediate solution, you can hit C-z to bring about the right mode.
;; An interim solution is to add an appropriate hook to the mode like this:
;;
;;     (add-hook 'your-favorite-mode 'viper-mode)
;; or
;;     (add-hook 'your-favorite-mode 'viper-change-state-to-emacs)
;;
;; whichever applies.  The right thing to do, however, is to complain to the
;; author of the respective package. (Sometimes they also neglect to equip
;; their  modes with hooks, which is one more reason for complaining.)
;;
;; 2. Keymap handling
;;    Each Viper state (insert, vi, replace) is implemented as a collection of
;;    several minor modes, each with its own keymap.
;;
;; Viper's  Vi state consists of seven minor modes:
;;
;;  viper-vi-intercept-minor-mode
;;  viper-vi-local-user-minor-mode
;;  viper-vi-global-user-minor-mode
;;  viper-vi-kbd-minor-mode
;;  viper-vi-state-modifier-minor-mode
;;  viper-vi-diehard-minor-mode
;;  viper-vi-basic-minor-mode
;;
;;  Bindings done to the keymap of the first mode overshadow those done to
;;  the second, which, in turn, overshadows those done to the third, etc.
;;
;;  The last viper-vi-basic-minor-mode contains most of the usual Vi bindings
;;  in its edit mode.  This mode provides access to all Emacs facilities.
;;  Novice users, however, may want to set their viper-expert-level to 1
;;  in their .viper file.  This will enable viper-vi-diehard-minor-mode.  This
;;  minor mode's bindings make Viper simulate the usual Vi very closely.
;;  For instance,  C-c will not have its standard Emacs binding
;;  and so many of the goodies of Emacs are not available.
;;
;;  A skilled user should set viper-expert-level to at least 3.  This will
;;  enable `C-c' and many Emacs facilities will become available.
;;  In this case, viper-vi-diehard-minor-mode is inactive.
;;
;;  Viper gurus should have at least
;;      (setq viper-expert-level 4)
;;  in their ~/.viper files.  This will unsuppress all Emacs keys that are not
;;  essential for VI-style editing.
;;  Pick-and-choose users may want to put
;;      (setq viper-expert-level 5)
;;  in ~/.viper.  Viper will then leave it up to the user to set the variables
;;  viper-want-*  See viper-set-expert-level for details.
;;
;;  The very first minor mode, viper-vi-intercept-minor-mode, is of no
;;  concern for the user.  It is needed to bind Viper's vital keys, such as
;;  ESC and C-z.
;;
;;  The second mode,  viper-vi-local-user-minor-mode, usually has an
;;  empty keymap.  However, the user can set bindings in this keymap, which
;;  will overshadow the corresponding bindings in the other two minor
;;  modes.  This is useful, for example, for setting up ZZ in gnus,
;;  rmail, mh-e, etc., to send  message instead of saving it in a file.
;;  Likewise, in Dired mode, you may want to bind ZN and ZP to commands
;;  that would visit the next or the previous file in the Dired buffer.
;;  Setting local keys is tricky, so don't do it directly.  Instead, use
;;  viper-add-local-keys function (see its doc).
;;
;;  The third minor mode, viper-vi-global-user-minor-mode, is also intended
;;  for the users but, unlike viper-vi-local-user-minor-mode, its key
;;  bindings are seen in all Viper buffers.  This mode keys can be done
;;  with define-key command.
;;
;;  The fourth minor mode, viper-vi-kbd-minor-mode, is used by keyboard
;;  macros.  Users are NOT supposed to modify this keymap directly.
;;
;;  The fifth mode, viper-vi-state-modifier-minor-mode, can be used to set
;;  key bindings that are visible in some major modes but not in others.
;;
;;  Users are allowed to modify keymaps that belong to
;;  viper-vi-local-user-minor-mode, viper-vi-global-user-minor-mode,
;;  and viper-vi-state-modifier-minor-mode only.
;;
;;  Viper's Insert state also has seven minor modes:
;;
;;      viper-insert-intercept-minor-mode
;;  	viper-insert-local-user-minor-mode
;;  	viper-insert-global-user-minor-mode
;;  	viper-insert-kbd-minor-mode
;;      viper-insert-state-modifier-minor-mode
;;  	viper-insert-diehard-minor-mode
;;  	viper-insert-basic-minor-mode
;;
;;  As with VI's editing modes, the first mode,
;;  viper-insert-intercept-minor-mode is used to bind vital keys that are not
;;  to be changed by the user.
;;
;;  The next mode, viper-insert-local-user-minor-mode, is used to customize
;;  bindings in the insert state of Viper.  The third mode,
;;  viper-insert-global-user-minor-mode is like
;;  viper-insert-local-user-minor-mode, except that its bindings are seen in
;;  all Viper buffers.  As with viper-vi-local-user-minor-mode, its bindings
;;  should be done via the function viper-add-local-keys.  Bindings for
;;  viper-insert-global-user-minor-mode can be set with the define-key command.
;;
;;  The next minor mode, viper-insert-kbd-minor-mode,
;;  is used for keyboard VI-style macros defined with :map!.
;;
;;  The fifth minor mode, viper-insert-state-modifier-minor-mode, is like
;;  viper-vi-state-modifier-minor-mode, except that it is used in the Insert
;;  state; it can be used to modify keys in a mode-specific fashion.
;;
;;  The minor mode viper-insert-diehard-minor-mode is in effect when
;;  the user wants a high degree of Vi compatibility (a bad idea, really!).
;;  The last minor mode, viper-insert-basic-minor-mode, is always in effect
;;  when Viper is in insert state.  It binds a small number of keys needed for
;;  Viper's operation.
;;
;;  Finally, Viper provides minor modes for overriding bindings set by Emacs
;;  modes when Viper is in Emacs state:
;;
;; 	viper-emacs-local-user-minor-mode
;;  	viper-emacs-global-user-minor-mode
;;      viper-emacs-kbd-minor-mode
;;      viper-emacs-state-modifier-minor-mode
;;
;;  These minor modes are in effect when Viper is in Emacs state.  The keymap
;;  associated with viper-emacs-global-user-minor-mode,
;;  viper-emacs-global-user-map, overrides the global and local keymaps as
;;  well as the minor mode keymaps set by other modes.  The keymap of
;;  viper-emacs-local-user-minor-mode, viper-emacs-local-user-map, overrides
;;  everything, but it is used on a per buffer basis.
;;  The keymap associated with viper-emacs-state-modifier-minor-mode
;;  overrides keys on a per-major-mode basis.  The mode
;;  viper-emacs-kbd-minor-mode is used to define Vi-style macros in Emacs
;;  state.
;;
;;  3. There is also one minor mode that is used when Viper is in its
;;     replace-state (used for commands like cw, C, etc.).  This mode is
;;     called
;;
;;       viper-replace-minor-mode
;;
;;     and its keymap is viper-replace-map.  Replace minor mode is always
;;     used in conjunction with the minor modes for insert-state, and its
;;     keymap overshadows the keymaps for insert minor modes.
;;
;;  4. Defining buffer-local bindings in Vi and Insert modes.
;;  As mentioned before, sometimes, it is convenient to have
;;  buffer-specific of mode-specific key bindings in Vi and insert modes.
;;  Viper provides a special function, viper-add-local-keys, to do precisely
;;  this.  For instance, is you need to add couple of mode-specific bindings
;;  to Insert mode, you can put
;;
;;       (viper-add-local-keys 'insert-state '((key1 . func1) (key2 .func2)))
;;
;;  somewhere in a hook of this major mode.  If you put something like this
;;  in your own elisp function, this will define bindings specific to the
;;  buffer that was current at the time of the call to viper-add-local-keys.
;;  The only thing to make sure here is that the major mode of this buffer
;;  is written according to Emacs conventions, which includes a call to
;;  (kill-all-local-variables).  See viper-add-local-keys for more details.
;;
;;
;;  TO DO (volunteers?):
;;
;; 1. Some of the code that is inherited from VIP-3.5 is rather
;;    convoluted.  Instead of viper-command-argument, keymaps should bind the
;;    actual commands.  E.g., "dw" should be bound to a generic command
;;    viper-delete that will delete things based on the value of
;;    last-command-event.  This would greatly simplify the logic and the code.
;;
;; 2. Somebody should venture to write a customization package a la
;;    options.el that would allow the user to change values of variables
;;    that meet certain specs (e.g., match a regexp) and whose doc string
;;    starts with a '*'.  Then, the user should be offered to save
;;    variables that were changed.  This will make user's customization job
;;    much easier.
;;

;;; Code:

;; compiler pacifier
(defvar mark-even-if-inactive)
(defvar quail-mode)
(defvar viper-expert-level)
(defvar viper-mode-string)
(defvar viper-major-mode-modifier-list)
;; end pacifier

(require 'advice)
(require 'viper-init)
(require 'viper-keym)

;; better be defined before Viper custom group.
(defvar viper-custom-file-name (convert-standard-filename "~/.viper")
  "Viper customization file.
If set by the user, this must be done _before_ Viper is loaded in `~/.emacs'.")

(defgroup viper nil
  "Vi emulation within Emacs.
NOTE: Viper customization should be saved in `viper-custom-file-name', which
defaults to `~/.viper'."
  :prefix "viper-"
  :group 'emulations)

(require 'viper-cmd)

(defgroup viper-misc nil
  "Miscellaneous Viper customization."
  :prefix "viper-"
  :group 'viper)


(defcustom viper-always t
  "Non-nil means, arrange for vi-state to be a default when appropriate.
This is different from `viper-mode' variable in that `viper-mode' determines
whether to use Viper in the first place, while `viper-always', if nil, lets
user decide when to invoke Viper in a major mode."
  :type 'boolean
  :tag "Always Invoke Viper"
  :group 'viper-misc)

;; Non-viper variables that need to be saved in case the user decides to
;; de-viperize emacs.
(defvar viper-saved-non-viper-variables nil)

(defcustom viper-mode (cond (noninteractive nil)
			    (t 'ask))
  "To Viperize or not to Viperize.
If t, viperize Emacs.  If nil -- don't.  If `ask', ask the user.
This variable is used primarily when Viper is being loaded.

Must be set in `~/.emacs' before Viper is loaded.
DO NOT set this variable interactively, unless you are using the customization
widget."
  :type '(choice (const nil) (const t) (const ask))
  :tag "Set Viper Mode on Loading"
  :group 'viper-misc)

(defcustom viper-vi-state-mode-list
  '(fundamental-mode
    makefile-mode

    awk-mode
    m4-mode
    xrdb-mode
    winmgr-mode
    autoconf-mode
    cvs-edit-mode

    html-mode html-helper-mode
    emacs-lisp-mode lisp-mode lisp-interaction-mode

    jde-mode java-mode
    cc-mode c-mode c++-mode objc-mode
    fortran-mode f90-mode
    basic-mode
    bat-mode
    asm-mode
    prolog-mode
    flora-mode
    sql-mode

    text-mode indented-text-mode
    tex-mode latex-mode bibtex-mode
    ps-mode

    ;; completion-list-mode
    diff-mode
    idl-mode

    perl-mode
    cperl-mode
    javascript-mode
    tcl-mode
    python-mode

    sh-mode ksh-mode csh-mode

    gnus-article-mode
    mh-show-mode
    )
  "Major modes that require Vi command state."
  :type '(repeat symbol)
  :group 'viper-misc)

(defcustom viper-emacs-state-mode-list
  '(Custom-mode

    dired-mode
    efs-mode
    tar-mode

    browse-kill-ring-mode
    recentf-mode
    recentf-dialog-mode
    occur-mode

    mh-folder-mode
    gnus-group-mode
    gnus-summary-mode

    completion-list-mode
    help-mode

    Info-mode
    Buffer-menu-mode
    compilation-mode

    rcirc-mode

    jde-javadoc-checker-report-mode

    view-mode
    vm-mode
    vm-summary-mode)
  "*A list of major modes that should come up in Emacs state.
Normally, Viper would bring buffers up in Emacs state, unless the corresponding
major mode has been placed on `viper-vi-state-mode-list' or
`viper-insert-state-mode-list'.  So, don't place a new mode on this list,
unless it is coming up in a wrong Viper state."
  :type '(repeat symbol)
  :group 'viper-misc)

(defcustom viper-insert-state-mode-list
  '(internal-ange-ftp-mode
    comint-mode
    gud-mode
    inferior-emacs-lisp-mode
    erc-mode
    eshell-mode
    shell-mode)
  "*A list of major modes that should come up in Vi Insert state."
  :type '(repeat symbol)
  :group 'viper-misc)


;; used to set viper-major-mode-modifier-list in defcustom
(defun viper-apply-major-mode-modifiers (&optional symbol value)
  (if symbol
      (set symbol value))
  (mapcar (lambda (triple)
	    (viper-modify-major-mode
	     (nth 0 triple) (nth 1 triple) (eval (nth 2 triple))))
	  viper-major-mode-modifier-list))

;; We change standard bindings in some major modes, making them slightly
;; different than in "normal" vi/insert/emacs states
(defcustom viper-major-mode-modifier-list
  '((help-mode emacs-state viper-slash-and-colon-map)
    (comint-mode insert-state viper-comint-mode-modifier-map)
    (comint-mode vi-state viper-comint-mode-modifier-map)
    (gud-mode insert-state viper-comint-mode-modifier-map)
    (shell-mode insert-state viper-comint-mode-modifier-map)
    (inferior-emacs-lisp-mode insert-state viper-comint-mode-modifier-map)
    (shell-mode vi-state viper-comint-mode-modifier-map)
    (ange-ftp-shell-mode insert-state viper-comint-mode-modifier-map)
    (ange-ftp-shell-mode vi-state viper-comint-mode-modifier-map)
    (internal-ange-ftp-mode insert-state viper-comint-mode-modifier-map)
    (internal-ange-ftp-mode vi-state viper-comint-mode-modifier-map)
    (dired-mode emacs-state viper-dired-modifier-map)
    (tar-mode emacs-state viper-slash-and-colon-map)
    (mh-folder-mode emacs-state viper-slash-and-colon-map)
    (gnus-group-mode emacs-state viper-gnus-modifier-map)
    (gnus-summary-mode emacs-state viper-gnus-modifier-map)
    (Info-mode emacs-state viper-slash-and-colon-map)
    (Buffer-menu-mode emacs-state viper-slash-and-colon-map)
    (erc-mode insert-state viper-comint-mode-modifier-map)
    (erc-mode vi-state viper-comint-mode-modifier-map)
    )
  "List specifying how to modify the various major modes to enable some Viperisms.
The list has the structure: ((mode viper-state keymap) (mode viper-state
keymap) ...).  If `mode' is on the list, the `keymap' will be made active (on
the minor-mode-map-alist) in the specified viper state.
If you change this list, have to restart Emacs for the change to take effect.
However, if you did the change through the customization widget, then Emacs
needs to be restarted only if you deleted a triple mode-state-keymap from the
list.  No need to restart Emacs in case of insertion or modification of an
existing triple."
  :type '(repeat
	  (list symbol
		(choice (const emacs-state)
			(const vi-state)
			(const insert-state))
		symbol))
  :set 'viper-apply-major-mode-modifiers
  :group 'viper-misc)





;;;###autoload
(defun toggle-viper-mode ()
  "Toggle Viper on/off.
If Viper is enabled, turn it off.  Otherwise, turn it on."
  (interactive)
  (if (eq viper-mode t)
      (viper-go-away)
    (setq viper-mode nil)
    (viper-mode)))

;;;###autoload
(defun viper-mode ()
  "Turn on Viper emulation of Vi in Emacs. See Info node `(viper)Top'."
  (interactive)
  (if (not noninteractive)
      (progn
	;; if the user requested viper-mode explicitly
	(if viper-mode
	    ()
	  (setq viper-mode t)
	  (load-library "viper"))

	(if viper-first-time ; Important check.  Prevents mix-up of startup
	    (progn	     ; and expert-level msgs when viper-mode recurses
	      (setq viper-first-time nil)
	      (if (not viper-inhibit-startup-message)
		  (save-window-excursion
		    (setq viper-inhibit-startup-message t)
		    (delete-other-windows)
		    (switch-to-buffer "Viper Startup Message")
		    (erase-buffer)
		    (insert
		     (substitute-command-keys
		      "Viper Is a Package for Emacs Rebels,
a VI Plan for Emacs Rescue, and a venomous VI PERil.

Incidentally, Viper emulates Vi under Emacs/XEmacs 20.
It supports all of what is good in Vi and Ex, while extending
and improving upon much of it.

   1. Viper supports Vi at several levels.  Level 1 is the closest to Vi,
      level 5 provides the most flexibility to depart from many Vi conventions.

      You will be asked to specify your user level in a following screen.

      If you select user level 1 then the keys ^X, ^C, ^Z, and ^G will behave
      as in VI, to smooth transition to Viper for the beginners.  However, to
      use Emacs productively, you are advised to reach user level 3 or higher.

      At user level 2 or higher, ^X and ^C have Emacs, not Vi, bindings;
      ^Z toggles Vi/Emacs states; ^G is Emacs's keyboard-quit (like ^C in Vi).

   2. Vi exit functions (e.g., :wq, ZZ) work on INDIVIDUAL files -- they
      do not cause Emacs to quit, except at user level 1 (for a novice).
   3. ^X^C EXITS EMACS.
   4. Viper supports multiple undo: `u' will undo.  Typing `.' will repeat
      undo.  Another `u' changes direction.

   6. Emacs Meta key is `C-\\' (in all modes) or `\\ ESC' (in Vi command mode).
      On a window system, the best way is to use the Meta-key on your keyboard.
   7. Try \\[keyboard-quit] and \\[abort-recursive-edit] repeatedly,if
      something funny happens.  This would abort the current editing command.

For more information on Viper:

   a. Type `:help' in Vi command mode
   b. Print Viper manual, found in ./etc/viper.dvi
   c. Print the Quick Reference, found in ./etc/viperCard.dvi

To submit a bug report or to contact the author, type :submitReport in Vi
command mode.  To shoo Viper away and return to pure Emacs (horror!), type:

   M-x viper-go-away

This startup message appears whenever you load Viper, unless you type `y' now."
		      ))
		    (goto-char (point-min))
		    (if (y-or-n-p "Inhibit Viper startup message? ")
			(viper-save-setting
			 'viper-inhibit-startup-message
			 "Viper startup message inhibited"
			 viper-custom-file-name t))
		    ;;(kill-buffer (current-buffer))
		    (message
		     "The last message is in buffer `Viper Startup Message'")
		    (sit-for 4)
		    ))
	      (viper-set-expert-level 'dont-change-unless)))

	(or (memq major-mode viper-emacs-state-mode-list) ; don't switch to Vi
	    (memq major-mode viper-insert-state-mode-list) ; don't switch
	    (viper-change-state-to-vi))
	))

  (if (eq major-mode 'viper-mode)
      (setq major-mode 'fundamental-mode))
  )


;; Apply a little heuristic to invoke vi state on major-modes
;; that are not listed in viper-vi-state-mode-list
(defun this-major-mode-requires-vi-state (mode)
  (cond ((memq mode viper-vi-state-mode-list) t)
	((memq mode viper-emacs-state-mode-list) nil)
	((memq mode viper-insert-state-mode-list) nil)
	(t (and (eq (key-binding "a") 'self-insert-command)
		(eq (key-binding " ") 'self-insert-command)))))


;; This hook designed to enable Vi-style editing in comint-based modes."
(defun viper-comint-mode-hook ()
  (set (make-local-variable 'require-final-newline) nil)
  (setq viper-ex-style-editing nil
	viper-ex-style-motion nil)
  (viper-change-state-to-insert))


;; remove viper hooks from SYMBOL
(defun viper-remove-hooks (symbol)
  (cond ((not (boundp symbol)) nil)
	((not (listp (eval symbol))) nil)
	((string-match "-hook" (symbol-name symbol))
	 (remove-hook symbol 'viper-mode)
	 (remove-hook symbol 'viper-change-state-to-emacs)
	 (remove-hook symbol 'viper-change-state-to-insert)
	 (remove-hook symbol 'viper-change-state-to-vi)
	 (remove-hook symbol 'viper-minibuffer-post-command-hook)
	 (remove-hook symbol 'viper-minibuffer-setup-sentinel)
	 (remove-hook symbol 'viper-major-mode-change-sentinel)
	 (remove-hook symbol 'set-viper-state-in-major-mode)
	 (remove-hook symbol 'viper-post-command-sentinel)
	 )))

;; Remove local value in all existing buffers
;; This doesn't delocalize vars (which would have been desirable)
(defun viper-delocalize-var (symbol)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (kill-local-variable symbol))))


(defun viper-go-away ()
  "De-Viperize Emacs.
This function tries to do as good a job as possible.  However, it may undo some
user customization, unrelated to Viper.  For instance, if the user advised
`read-file-name', `describe-key', and some others, then this advice will be
undone.
It also can't undo some Viper settings."
  (interactive)

  ;; restore non-viper vars
  (setq-default
   next-line-add-newlines
   (viper-standard-value
    'next-line-add-newlines viper-saved-non-viper-variables)
   require-final-newline
   (viper-standard-value
    'require-final-newline viper-saved-non-viper-variables)
   scroll-step
   (viper-standard-value 'scroll-step viper-saved-non-viper-variables)
   mode-line-buffer-identification
   (viper-standard-value
    'mode-line-buffer-identification viper-saved-non-viper-variables)
   global-mode-string
   (delq 'viper-mode-string global-mode-string))

  (setq-default major-mode
                (viper-standard-value 'default-major-mode
                                      viper-saved-non-viper-variables))

  (if (featurep 'emacs)
      (setq-default
       mark-even-if-inactive
       (viper-standard-value
	'mark-even-if-inactive viper-saved-non-viper-variables)))

  ;; Ideally, we would like to be able to de-localize local variables
  (unless
      (and (fboundp 'add-to-ordered-list) (boundp 'emulation-mode-map-alists))
    (viper-delocalize-var 'minor-mode-map-alist))
  (viper-delocalize-var 'require-final-newline)
  (if (featurep 'xemacs) (viper-delocalize-var 'bar-cursor))


  ;; deactivate all advices done by Viper.
  (ad-deactivate-regexp "viper-")

  (setq viper-mode nil)

  (when (and (fboundp 'add-to-ordered-list) (boundp 'emulation-mode-map-alists))
    (setq emulation-mode-map-alists
	  (delq 'viper--intercept-key-maps
		(delq 'viper--key-maps emulation-mode-map-alists))
	  ))

  (viper-delocalize-var 'viper-vi-minibuffer-minor-mode)
  (viper-delocalize-var 'viper-insert-minibuffer-minor-mode)
  (viper-delocalize-var 'viper-vi-intercept-minor-mode)
  (viper-delocalize-var 'viper-insert-intercept-minor-mode)

  (viper-delocalize-var 'viper-vi-local-user-minor-mode)
  (viper-delocalize-var 'viper-vi-kbd-minor-mode)
  (viper-delocalize-var 'viper-vi-global-user-minor-mode)
  (viper-delocalize-var 'viper-vi-state-modifier-minor-mode)
  (viper-delocalize-var 'viper-vi-diehard-minor-mode)
  (viper-delocalize-var 'viper-vi-basic-minor-mode)

  (viper-delocalize-var 'viper-replace-minor-mode)

  (viper-delocalize-var 'viper-insert-local-user-minor-mode)
  (viper-delocalize-var 'viper-insert-kbd-minor-mode)
  (viper-delocalize-var 'viper-insert-global-user-minor-mode)
  (viper-delocalize-var 'viper-insert-state-modifier-minor-mode)
  (viper-delocalize-var 'viper-insert-diehard-minor-mode)
  (viper-delocalize-var 'viper-insert-basic-minor-mode)

  (viper-delocalize-var 'viper-emacs-intercept-minor-mode)
  (viper-delocalize-var 'viper-emacs-local-user-minor-mode)
  (viper-delocalize-var 'viper-emacs-kbd-minor-mode)
  (viper-delocalize-var 'viper-emacs-global-user-minor-mode)
  (viper-delocalize-var 'viper-emacs-state-modifier-minor-mode)

  (viper-delocalize-var 'viper-current-state)
  (viper-delocalize-var 'viper-mode-string)

  (setq-default viper-vi-minibuffer-minor-mode	       nil
		viper-insert-minibuffer-minor-mode     nil
		viper-vi-intercept-minor-mode	       nil
		viper-insert-intercept-minor-mode      nil

		viper-vi-local-user-minor-mode         nil
		viper-vi-kbd-minor-mode        	       nil
		viper-vi-global-user-minor-mode        nil
		viper-vi-state-modifier-minor-mode     nil
		viper-vi-diehard-minor-mode            nil
		viper-vi-basic-minor-mode              nil

		viper-replace-minor-mode 	       nil

		viper-insert-local-user-minor-mode     nil
		viper-insert-kbd-minor-mode            nil
		viper-insert-global-user-minor-mode    nil
		viper-insert-state-modifier-minor-mode nil
		viper-insert-diehard-minor-mode        nil
		viper-insert-basic-minor-mode          nil

		viper-emacs-intercept-minor-mode       nil
		viper-emacs-local-user-minor-mode      nil
		viper-emacs-kbd-minor-mode             nil
		viper-emacs-global-user-minor-mode     nil
		viper-emacs-state-modifier-minor-mode  nil

		viper-current-state		       'emacs-state
		viper-mode-string		       viper-emacs-state-id
		)

  ;; remove all hooks set by viper
  (mapatoms 'viper-remove-hooks)
  (remove-hook 'comint-mode-hook 'viper-comint-mode-hook)
  (remove-hook 'erc-mode-hook 'viper-comint-mode-hook)
  (remove-hook 'change-major-mode-hook 'viper-major-mode-change-sentinel)

  ;; unbind Viper mouse bindings
  (viper-unbind-mouse-search-key)
  (viper-unbind-mouse-insert-key)
  ;; In emacs, we have to advice handle-switch-frame
  ;; This advice is undone earlier, when all advices matching "viper-" are
  ;; deactivated.
  (if (featurep 'xemacs)
      (remove-hook 'mouse-leave-frame-hook 'viper-remember-current-frame))
  ) ; end viper-go-away


;; list of buffers that just changed their major mode
;; used in a hack that triggers vi command mode whenever needed
(defvar viper-new-major-mode-buffer-list nil)

;; set appropriate Viper state in buffers that changed major mode
(defun set-viper-state-in-major-mode ()
  (mapc
   (lambda (buf)
     (if (viper-buffer-live-p buf)
	 (with-current-buffer buf
	   (cond ((and (this-major-mode-requires-vi-state major-mode)
		       (eq viper-current-state 'emacs-state))
		  (viper-mode))
		 ((memq major-mode viper-emacs-state-mode-list)
		  ;; not checking (eq viper-current-state 'emacs-state)
		  ;; because viper-current-state could have gotten it by
		  ;; default.  We need viper-change-state-to-emacs here to have
		  ;; the keymaps take effect.
		  (viper-change-state-to-emacs))
		 ((and (memq major-mode viper-insert-state-mode-list)
		       (not (eq viper-current-state 'insert-state)))
		  (viper-change-state-to-insert))
		 )) ; with-current-buffer
       )) ; function
   viper-new-major-mode-buffer-list)
  ;; clear the list of bufs that changed major mode
  (setq viper-new-major-mode-buffer-list nil)
  ;; change the global value of hook
  (remove-hook 'viper-post-command-hooks 'set-viper-state-in-major-mode))

;; sets up post-command-hook to turn viper-mode, if the current mode is
;; fundamental
(defun viper-major-mode-change-sentinel ()
  (save-match-data
    (or (string-match "\*Minibuf-" (buffer-name))
	(setq viper-new-major-mode-buffer-list
	      (cons (current-buffer) viper-new-major-mode-buffer-list))))
  ;; change the global value of hook
  (add-hook 'viper-post-command-hooks 'set-viper-state-in-major-mode t))



;; This sets major mode hooks to make them come up in vi-state.
(defun viper-set-hooks ()
  ;; It is of course a misnomer to call viper-mode a `major mode'.
  ;; However, this has the effect that if the user didn't specify the
  ;; default mode, new buffers that fall back on the default will come up
  ;; in Fundamental Mode and Vi state.
  ;; When viper-mode is executed in such a case, it will set the major mode
  ;; back to fundamental-mode.
  (if (eq (default-value 'major-mode) 'fundamental-mode)
      (setq-default major-mode 'viper-mode))

  (add-hook 'change-major-mode-hook 'viper-major-mode-change-sentinel)
  (add-hook 'find-file-hooks 'set-viper-state-in-major-mode)

  ;; keep this because many modes we don't know about use this hook
  (defvar text-mode-hook)
  (add-hook 'text-mode-hook 'viper-mode)

  (defvar emerge-startup-hook)
  (add-hook 'emerge-startup-hook 'viper-change-state-to-emacs)

  ;; Zap bad bindings in flyspell-mouse-map, which prevent ESC from working
  ;; over misspelled words (due to the overlay keymaps)
  (defvar flyspell-mode-hook)
  (defvar flyspell-mouse-map)
  (add-hook 'flyspell-mode-hook
	    (lambda ()
              (define-key flyspell-mouse-map viper-ESC-key nil)))
  ;; if viper is started from .emacs, it might be impossible to get certain
  ;; info about the display and windows until emacs initialization is complete
  ;; So do it via the window-setup-hook
  (add-hook 'window-setup-hook
  	    (lambda ()
              (modify-frame-parameters
               (selected-frame)
               (list (cons 'viper-vi-state-cursor-color
                           (viper-get-cursor-color))))
              (setq viper-vi-state-cursor-color (viper-get-cursor-color))
              ))

  ;; Tell vc-diff to put *vc* in Vi mode
  (eval-after-load
      "vc"
    '(defadvice vc-diff (after viper-vc-ad activate)
       "Force Vi state in VC diff buffer."
       (viper-change-state-to-vi)))

  (eval-after-load
   "emerge"
   '(defadvice emerge-quit (after viper-emerge-advice activate)
      "Run `viper-change-state-to-vi' after quitting emerge."
      (viper-change-state-to-vi)))

  ;; passwd.el sets up its own buffer, which turns up in Vi mode,
  ;; thus overriding the local map.  We don't need Vi mode here.
  (eval-after-load
   "passwd"
   '(defadvice read-passwd-1 (before viper-passwd-ad activate)
      "Switch to Emacs state while reading password."
      (viper-change-state-to-emacs)))

  (defadvice self-insert-command (around viper-self-insert-ad activate)
    "Ignore all self-inserting keys in the vi-state."
    (if (and (eq viper-current-state 'vi-state)
	     ;; Do not use called-interactively-p here. XEmacs does not have it
	     ;; and interactive-p is just fine.
	     ;; (called-interactively-p 'interactive))
	     (interactive-p))
	(beep 1)
      ad-do-it
      ))

  (defadvice set-cursor-color (after viper-set-cursor-color-ad activate)
    "Change cursor color in VI state."
    (modify-frame-parameters
	(selected-frame)
	(list (cons 'viper-vi-state-cursor-color (ad-get-arg 0))))
    (setq viper-vi-state-cursor-color (ad-get-arg 0))
    )

  (when (and (fboundp 'add-to-ordered-list) (boundp 'emulation-mode-map-alists))
    ;; needs to be as early as possible
    (add-to-ordered-list
     'emulation-mode-map-alists 'viper--intercept-key-maps 100)
    ;; needs to be after cua-mode
    (add-to-ordered-list 'emulation-mode-map-alists 'viper--key-maps 500)
    )

  ;; Emacs shell, ange-ftp, and comint-based modes
  (add-hook 'comint-mode-hook 'viper-comint-mode-hook) ; comint
  (add-hook 'erc-mode-hook 'viper-comint-mode-hook) ; ERC

  (add-hook 'eshell-mode-hook
	    (lambda () (setq viper-auto-indent nil)))

  (viper-set-emacs-state-searchstyle-macros nil 'dired-mode) ; dired
  (viper-set-emacs-state-searchstyle-macros nil 'tar-mode) ; tar
  (viper-set-emacs-state-searchstyle-macros nil 'mh-folder-mode) ; mhe
  (viper-set-emacs-state-searchstyle-macros nil 'gnus-group-mode) ; gnus
  (viper-set-emacs-state-searchstyle-macros nil 'gnus-summary-mode)
  (viper-set-emacs-state-searchstyle-macros nil 'Info-mode) ; info
  (viper-set-emacs-state-searchstyle-macros nil 'Buffer-menu-mode) ;buffer-menu

  ;; Modify major modes according to viper-major-mode-modifier-list
  (viper-apply-major-mode-modifiers)

  ;; For RMAIL users.
  ;; Put buf in Emacs state after edit.
  (eval-after-load
   "rmailedit"
   '(defadvice rmail-cease-edit (after viper-rmail-advice activate)
      "Switch to Emacs state when done editing message."
      (viper-change-state-to-emacs)))

  ;; ISO accents
  ;; Need to do it after loading iso-acc, or else this loading will wipe out
  ;; the advice.
  (eval-after-load
   "iso-acc"
   '(defadvice iso-accents-mode (around viper-iso-accents-advice activate)
      "Set viper-automatic-iso-accents to iso-accents-mode."
      (let ((arg (ad-get-arg 0)))
	ad-do-it
	(setq viper-automatic-iso-accents
	      (if (eq viper-current-state 'vi-state)
		  (if arg
		      ;; if iso-accents-mode was called with positive arg, turn
		      ;; accents on
		      (> (prefix-numeric-value arg) 0)
		    ;; else: toggle viper-automatic-iso-accents
		    (not viper-automatic-iso-accents))
		;; other states: accept what iso-accents-mode has done
		iso-accents-mode))
	;; turn off ISO accents in vi-state
	(if (eq viper-current-state 'vi-state)
	    (viper-set-iso-accents-mode nil))
	(if (memq viper-current-state '(vi-state insert-state replace-state))
	    (message "Viper ISO accents mode: %s"
		     (if viper-automatic-iso-accents "on" "off")))
	)))

  ;; International input methods
  (if (featurep 'emacs)
      (eval-after-load "mule-cmds"
	'(progn
	   (defadvice inactivate-input-method (after viper-mule-advice activate)
	     "Set viper-special-input-method to disable intl. input methods."
	     (viper-inactivate-input-method-action))
	   (defadvice activate-input-method (after viper-mule-advice activate)
	     "Set viper-special-input-method to enable intl. input methods."
	     (viper-activate-input-method-action))
	   ))
    ;; XEmacs Although these hooks exist in Emacs, they don't seem to be always
    ;; called on input-method activation/deactivation, so we the above advise
    ;; functions instead.
    (eval-after-load "mule-cmds"
      '(progn
	 (add-hook 'input-method-activate-hook
		   'viper-activate-input-method-action t)
	 (add-hook 'input-method-inactivate-hook
		   'viper-inactivate-input-method-action t)))
    )
  (eval-after-load "mule-cmds"
    '(defadvice toggle-input-method (around viper-mule-advice activate)
       "Adjust input-method toggling in vi-state."
       (if (and viper-special-input-method (eq viper-current-state 'vi-state))
	   (viper-inactivate-input-method)
	 ad-do-it)))

  ) ; viper-set-hooks


;; these are primarily advices and Vi-ish variable settings
(defun viper-non-hook-settings ()

  ;;;; Viper changes the default mode-line-buffer-identification
  ;;(setq-default mode-line-buffer-identification '(" %b"))

  ;; setup emacs-supported vi-style feel
  (setq next-line-add-newlines nil
	require-final-newline t)

  ;; don't bark when mark is inactive
  (if (featurep 'emacs)
      (setq mark-even-if-inactive t))

  (setq scroll-step 1)

  ;; Variable displaying the current Viper state in the mode line.
  (or (memq 'viper-mode-string global-mode-string)
      (setq global-mode-string
	    (append '("" viper-mode-string) (cdr global-mode-string))))

  (if (featurep 'xemacs)
      ;; XEmacs
      (defadvice describe-key (before viper-describe-key-ad protect activate)
	"Force to read key via `viper-read-key-sequence'."
	(interactive (list (viper-read-key-sequence "Describe key: "))))
    ;; Emacs
    (defadvice describe-key (before viper-describe-key-ad protect activate)
      "Force to read key via `viper-read-key-sequence'."
      (interactive (let (key)
		     (setq key (viper-read-key-sequence
				"Describe key (or click or menu item): "))
		     (list key
			   (prefix-numeric-value current-prefix-arg)
			   ;; If KEY is a down-event, read also the
			   ;; corresponding up-event.
			   (and (vectorp key)
				(let ((last-idx (1- (length key))))
				  (and (eventp (aref key last-idx))
				       (memq 'down (event-modifiers
						    (aref key last-idx)))))
				(or (and (eventp (aref key 0))
					 (memq 'down (event-modifiers
						      (aref key 0)))
					 ;; For the C-down-mouse-2 popup menu,
					 ;; there is no subsequent up-event
					 (= (length key) 1))
				    (and (> (length key) 1)
					 (eventp (aref key 1))
					 (memq 'down (event-modifiers (aref key 1)))))
				(read-event))))))
    ) ; (if (featurep 'xemacs)

  (if (featurep 'xemacs)
      ;; XEmacs
      (defadvice describe-key-briefly
	(before viper-describe-key-briefly-ad protect activate)
	"Force to read key via `viper-read-key-sequence'."
	(interactive (list (viper-read-key-sequence "Describe key briefly: "))))
    ;; Emacs
    (defadvice describe-key-briefly
      (before viper-describe-key-briefly-ad protect activate)
      "Force to read key via `viper-read-key-sequence'."
      (interactive (let (key)
		     (setq key (viper-read-key-sequence
				"Describe key (or click or menu item): "))
		     ;; If KEY is a down-event, read and discard the
		     ;; corresponding up-event.
		     (and (vectorp key)
			  (let ((last-idx (1- (length key))))
			    (and (eventp (aref key last-idx))
				 (memq 'down (event-modifiers (aref key last-idx)))))
			  (read-event))
		     (list key
			   (if current-prefix-arg
			       (prefix-numeric-value current-prefix-arg))
			   1))))
    ) ; (if (featurep 'xemacs)

  (defadvice find-file (before viper-add-suffix-advice activate)
    "Use `read-file-name' for reading arguments."
    (interactive (cons (read-file-name "Find file: " nil default-directory)
		       ;; XEmacs: if Mule & prefix arg, ask for coding system
		       (cond ((and (featurep 'xemacs) (featurep 'mule))
			      (list
			       (and current-prefix-arg
				    (read-coding-system "Coding-system: "))))
			     ;; Emacs: do wildcards
			     ((and (featurep 'emacs) (boundp 'find-file-wildcards))
				   (list find-file-wildcards))))
		 ))

  (defadvice find-file-other-window (before viper-add-suffix-advice activate)
    "Use `read-file-name' for reading arguments."
    (interactive (cons (read-file-name "Find file in other window: "
				       nil default-directory)
		       ;; XEmacs: if Mule & prefix arg, ask for coding system
		       (cond ((and (featurep 'xemacs) (featurep 'mule))
			      (list
			       (and current-prefix-arg
				    (read-coding-system "Coding-system: "))))
			     ;; Emacs: do wildcards
			     ((and (featurep 'emacs) (boundp 'find-file-wildcards))
			      (list find-file-wildcards))))
		 ))


  (defadvice find-file-other-frame (before viper-add-suffix-advice activate)
    "Use `read-file-name' for reading arguments."
    (interactive (cons (read-file-name "Find file in other frame: "
				       nil default-directory)
		       ;; XEmacs: if Mule & prefix arg, ask for coding system
		       (cond ((and (featurep 'xemacs) (featurep 'mule))
			      (list
			       (and current-prefix-arg
				    (read-coding-system "Coding-system: "))))
			     ;; Emacs: do wildcards
			     ((and (featurep 'emacs) (boundp 'find-file-wildcards))
			      (list find-file-wildcards))))
		 ))


  (defadvice read-file-name (around viper-suffix-advice activate)
    "Tell `exit-minibuffer' to run `viper-file-add-suffix' as a hook."
    (let ((viper-minibuffer-exit-hook
	   (append viper-minibuffer-exit-hook
		   '(viper-minibuffer-trim-tail viper-file-add-suffix))))
      ad-do-it))

  (defadvice start-kbd-macro (after viper-kbd-advice activate)
    "Remove Viper's intercepting bindings for C-x ).
  This may be needed if the previous `:map' command terminated abnormally."
    (define-key viper-vi-intercept-map "\C-x)" nil)
    (define-key viper-insert-intercept-map "\C-x)" nil)
    (define-key viper-emacs-intercept-map "\C-x)" nil))

  (defadvice add-minor-mode (after
			     viper-advice-add-minor-mode
			     (toggle name &optional keymap after toggle-fun)
			     activate)
    "Run viper-normalize-minor-mode-map-alist after adding a minor mode."
    (viper-normalize-minor-mode-map-alist)
    (unless
	(and (fboundp 'add-to-ordered-list) (boundp 'emulation-mode-map-alists))
      (setq-default minor-mode-map-alist minor-mode-map-alist)))

  ;; catch frame switching event
  (if (viper-window-display-p)
      (if (featurep 'xemacs)
	     (add-hook 'mouse-leave-frame-hook
		       'viper-remember-current-frame)
	   (defadvice handle-switch-frame (before viper-frame-advice activate)
	     "Remember the selected frame before the switch-frame event."
	     (viper-remember-current-frame (selected-frame)))) )

  ) ; end viper-non-hook-settings



;; Ask only if this-command/last-command are nil, i.e., when loading
(cond ((and (eq viper-mode 'ask) (null this-command) (null last-command))
       (save-window-excursion
	 (with-output-to-temp-buffer " *viper-info*"
	   (princ "
You have loaded Viper, and are about to Viperize your Emacs!

Viper is a Package for Emacs Rebels and a venomous VI PERil,

It's time to decide: to Viperize or not to Viperize...

If you wish to Viperize AND make this your way of life, please put

	(setq viper-mode t)
	(require 'viper)

in your .emacs file (preferably, close to the top).
These two lines must come in the order given.

** Viper users:
	**** The startup file name has been changed from .vip to .viper
	**** All vip-* style names have been converted to viper-* style."))
	 (if (y-or-n-p "Viperize? ")
	     (setq viper-mode t)
	   (setq viper-mode nil))
	 (message "")
	 (kill-buffer " *viper-info*")))

      ;; If viper-mode is t, then just continue.  Viper will kick in.
      ((eq viper-mode t))
      ;; Otherwise, it was asking Viper was not loaded through .emacs
      ;; In this case, it was either through M-x viper-mode or via something
      ;; else, like the custom widget.  If Viper was loaded through
      ;; M-x viper-mode, then viper will kick in anyway.
      (t (setq viper-mode nil)))

(defun viper-load-custom-file ()
  (if (and (file-exists-p viper-custom-file-name)
	   (not noninteractive))
      (load viper-custom-file-name)))






;; save non-viper vars that Viper might change
(if (null viper-saved-non-viper-variables)
    (setq viper-saved-non-viper-variables
	  (list
	   (cons 'default-major-mode (list (default-value 'major-mode)))
	   (cons 'next-line-add-newlines (list next-line-add-newlines))
	   (cons 'require-final-newline (list require-final-newline))
	   (cons 'scroll-step (list scroll-step))
	   (cons 'mode-line-buffer-identification
		 (list (default-value 'mode-line-buffer-identification)))
	   (cons 'global-mode-string (list global-mode-string))
	   (if (featurep 'emacs)
	       (cons 'mark-even-if-inactive (list mark-even-if-inactive)))
	   )))


;; Set some useful macros, advices
;; These must be BEFORE ~/.viper is loaded,
;; so the user can unrecord them in ~/.viper.
(if viper-mode
    (progn
      ;; set advices and some variables that give emacs Vi look.
      (viper-non-hook-settings)

      ;; repeat the 2nd previous command without rotating the command history
      (viper-record-kbd-macro
       (vector viper-repeat-from-history-key '\1) 'vi-state
       [(meta x) v i p e r - r e p e a t - f r o m - h i s t o r y return] 't)
      ;; repeat the 3d previous command without rotating the command history
      (viper-record-kbd-macro
       (vector viper-repeat-from-history-key '\2) 'vi-state
       [(meta x) v i p e r - r e p e a t - f r o m - h i s t o r y return] 't)

      ;; set macros for toggling case sensitivity and regexp search
      (viper-set-searchstyle-toggling-macros nil)
      ;; Make %%% toggle parsing comments for matching parentheses
      (viper-set-parsing-style-toggling-macro nil)

      ;; ~/.viper is loaded if exists
      (viper-load-custom-file)

      ;; should be after loading custom file to avoid the pesky msg that
      ;; mouse-search/insert keys are already bound
      (viper-bind-mouse-search-key)
      (viper-bind-mouse-insert-key)
      ))



;; Applying Viper customization -- runs after (load .viper)

;; Save user settings or Viper defaults for vars controlled by
;; viper-expert-level
(if (null viper-saved-user-settings)
    (setq viper-saved-user-settings
	  (list (cons 'viper-want-ctl-h-help (list viper-want-ctl-h-help))
		(cons 'viper-always (list viper-always))
		(cons 'viper-no-multiple-ESC (list viper-no-multiple-ESC))
		(cons 'viper-ex-style-motion (list viper-ex-style-motion))
		(cons 'viper-ex-style-editing
		      (list viper-ex-style-editing))
		(cons 'viper-want-emacs-keys-in-vi
		      (list viper-want-emacs-keys-in-vi))
		(cons 'viper-electric-mode (list viper-electric-mode))
		(cons 'viper-want-emacs-keys-in-insert
		      (list viper-want-emacs-keys-in-insert))
		(cons 'viper-re-search (list viper-re-search)))))


(if viper-mode
    (progn
      (viper-set-minibuffer-style)
      (if viper-buffer-search-char
	  (viper-buffer-search-enable))
      (viper-update-syntax-classes 'set-default)
      ))


;;; Familiarize Viper with some minor modes that have their own keymaps
(if viper-mode
    (progn
      (viper-harness-minor-mode "compile")
      (viper-harness-minor-mode "outline")
      (viper-harness-minor-mode "allout")
      (viper-harness-minor-mode "xref")
      (viper-harness-minor-mode "lmenu")
      (viper-harness-minor-mode "vc")
      (viper-harness-minor-mode "ltx-math") ; LaTeX-math-mode in AUC-TeX, which
      (viper-harness-minor-mode "latex")    ; sits in one of these two files
      (viper-harness-minor-mode "cyrillic")
      (viper-harness-minor-mode "russian")
      (viper-harness-minor-mode "view-less")
      (viper-harness-minor-mode "view")
      (viper-harness-minor-mode "reftex")
      (viper-harness-minor-mode "flyspell")
      ))


;; Intercept maps could go in viper-keym.el
;; We keep them here in case someone redefines them in ~/.viper

(define-key viper-vi-intercept-map viper-ESC-key 'viper-intercept-ESC-key)
(define-key viper-insert-intercept-map viper-ESC-key 'viper-intercept-ESC-key)

;; This is taken care of by viper-insert-global-user-map.
;;(define-key viper-replace-map viper-ESC-key 'viper-intercept-ESC-key)


;; The default viper-toggle-key is \C-z; for the novice, it suspends or
;; iconifies Emacs
(define-key viper-vi-intercept-map viper-toggle-key 'viper-toggle-key-action)
(define-key
  viper-emacs-intercept-map viper-toggle-key 'viper-change-state-to-vi)

;;; Removed to avoid bad interaction with cua-mode.
;;; Escape from Emacs and Insert modes to Vi for one command
;;(define-key viper-emacs-intercept-map "\C-c\\" 'viper-escape-to-vi)
;;(define-key viper-insert-intercept-map "\C-c\\" 'viper-escape-to-vi)

(if viper-mode
    (setq-default viper-emacs-intercept-minor-mode t
		  viper-emacs-local-user-minor-mode t
		  viper-emacs-global-user-minor-mode t
		  viper-emacs-kbd-minor-mode t
		  viper-emacs-state-modifier-minor-mode t))
(if (and viper-mode (eq viper-current-state 'emacs-state))
    (setq viper-emacs-intercept-minor-mode t
	  viper-emacs-local-user-minor-mode t
	  viper-emacs-global-user-minor-mode t
	  viper-emacs-kbd-minor-mode t
	  viper-emacs-state-modifier-minor-mode t))


(if (and viper-mode
	 (or viper-always
	     (and (< viper-expert-level 5) (> viper-expert-level 0))))
    (viper-set-hooks))

;; Let all minor modes take effect after loading.
;; This may not be enough, so we also set default minor-mode-alist.
;; Without setting the default, new buffers that come up in emacs mode have
;; minor-mode-map-alist = nil, unless we call viper-change-state-*
(if (and viper-mode (eq viper-current-state 'emacs-state))
    (progn
      (viper-change-state-to-emacs)
      (unless
	  (and (fboundp 'add-to-ordered-list)
	       (boundp 'emulation-mode-map-alists))
	(setq-default minor-mode-map-alist minor-mode-map-alist))
      ))

(if (and viper-mode (this-major-mode-requires-vi-state major-mode))
    (viper-mode))

(if viper-mode
    (setq initial-major-mode
	  `(lambda ()
	     (funcall (quote ,initial-major-mode))
	     (set-viper-state-in-major-mode))
	  ))



(run-hooks 'viper-load-hook) ; the last chance to change something

(provide 'viper)


;; Local Variables:
;; eval: (put 'viper-deflocalvar 'lisp-indent-hook 'defun)
;; End:

;;; viper.el ends here
