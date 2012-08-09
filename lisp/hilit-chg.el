;;; hilit-chg.el --- minor mode displaying buffer changes with special face

;; Copyright (C) 1998, 2000-2012  Free Software Foundation, Inc.

;; Author: Richard Sharman <rsharman@pobox.com>
;; Keywords: faces

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

;; A minor mode: "Highlight Changes mode".

;; When Highlight Changes mode is enabled changes to the buffer are
;; recorded with a text property.  Normally these ranges of text are
;; displayed in a distinctive face.  However, sometimes it is
;; desirable to temporarily not see these changes.  Instead of
;; disabling Highlight Changes mode (which would remove the text property)
;; use the command highlight-changes-visible-mode.

;; Two faces are supported: one for changed or inserted text and
;; another for the first character after text has been deleted.

;; When Highlight Changes mode is on (even if changes are not visible)
;; you can go to the next or previous change with
;; `highlight-changes-next-change' or `highlight-changes-previous-change'.

;; Command highlight-compare-with-file shows changes in this file
;; compared with another file (by default the previous version of the
;; file).
;;
;; The command highlight-compare-buffers compares two buffers by
;; highlighting their differences.

;; You can "age" different sets of changes by using
;; `highlight-changes-rotate-faces'.  This rotates through a series
;; of different faces, so you can distinguish "new" changes from "older"
;; changes.  You can customize these "rotated" faces in two ways.  You can
;; either explicitly define each face by customizing
;; `highlight-changes-face-list'.  If, however, the faces differ from
;; `highlight-changes-face' only in the foreground color, you can simply set
;; `highlight-changes-colors'.  If `highlight-changes-face-list' is nil when
;; the faces are required they will be constructed from
;; `highlight-changes-colors'.

;; You can automatically rotate faces when the buffer is saved;
;; see function `highlight-changes-rotate-faces' for how to do this.

;; The hook `highlight-changes-mode-hook' is called when
;; Highlight Changes mode is turned on or off.
;; When it called, variable `highlight-changes-mode' has been updated
;; to the new value.
;;
;; Example usage:
;; (defun my-highlight-changes-mode-hook ()
;;   (if highlight-changes-mode
;;       (add-hook 'write-file-functions 'highlight-changes-rotate-faces nil t)
;;     (remove-hook 'write-file-functions 'highlight-changes-rotate-faces t)
;;     ))


;;           Automatically enabling Highlight Changes mode
;;

;; Normally, Highlight Changes mode is turned on explicitly in a buffer.
;;
;; If you prefer to have it automatically invoked you can do it as
;; follows.

;; 1. Most modes have a major-hook, typically called MODE-hook.  You
;; can use `add-hook' to call `highlight-changes-mode'.

;;   Example:
;;	(add-hook 'c-mode-hook 'highlight-changes-mode)

;; However, this cannot be done for Fundamental mode for there is no
;; such hook.

;; 2. You can use the function `global-highlight-changes-mode'
;;
;; This function, which is fashioned after the way `global-font-lock' works,
;; toggles on or off global Highlight Changes mode.  When activated, it turns
;; on Highlight Changes mode in all "suitable" existing buffers and will turn
;; it on in new "suitable" buffers to be created.

;; A buffer's "suitability" is determined by variable
;; `highlight-changes-global-modes', as follows.  If it is
;; * nil  -- then no buffers are suitable;
;; * a function -- this function is called and the result is used.  As
;;   an example, if the value is `buffer-file-name' then all buffers
;;   who are visiting files are suitable, but others (like dired
;;   buffers) are not;
;; * a list -- then the buffer is suitable if and only if its mode is in the
;;   list, except if the first element is `not', in which case the test
;;   is reversed (i.e. it is a list of unsuitable modes).
;; * Otherwise, the buffer is suitable if its name does not begin with
;;   ` ' or `*' and if `buffer-file-name' returns true.

;; To enable it for future sessions put this in your ~/.emacs file:
;;		(global-highlight-changes-mode t)


;;     Possible bindings:
;; (global-set-key '[C-right] 'highlight-changes-next-change)
;; (global-set-key '[C-left]  'highlight-changes-previous-change)
;;
;;     Other interactive functions (that could be bound if desired):
;; highlight-changes-mode
;; highlight-changes-toggle-visibility
;; highlight-changes-remove-highlight
;; highlight-compare-with-file
;; highlight-compare-buffers
;; highlight-changes-rotate-faces


;;; Bugs:

;; - the next-change and previous-change functions are too literal;
;;   they should find the next "real" change, in other words treat
;;   consecutive changes as one.


;;; To do (maybe), notes, ...

;; - having different faces for deletion and non-deletion: is it
;;   really worth the hassle?
;; - highlight-compare-with-file should allow RCS files - e.g. nice to be
;;   able to say show changes compared with version 2.1.


;;; History:

;; R Sharman (rsharman@pobox.com) Feb 1998:
;; - initial release as change-mode.
;; Jari Aalto <jari.aalto@ntc.nokia.com> Mar 1998
;; - fixes for byte compile errors
;; - use eval-and-compile for autoload
;; Marijn Ros <J.M.Ros@fys.ruu.nl> Mar 98
;; - suggested turning it on by default
;; Eric Ludlam <zappo@gnu.org> Suggested using overlays.
;; July 98
;; - global mode and various stuff added
;; - Changed to use overlays
;; August 98
;; - renamed to Highlight Changes mode.
;; Dec 2003
;; - Use require for ediff stuff
;; - Added highlight-compare-buffers
;; Mar 2008
;; - Made highlight-changes-mode like other modes (toggle on/off)
;; - Added new command highlight-changes-visible-mode to replace the
;;   previous active/passive aspect of highlight-changes-mode.
;; - Removed highlight-changes-toggle-hook
;; - Put back eval-and-compile inadvertently dropped
;; May 2008
;; - Removed highlight-changes-disable-hook and highlight-changes-enable-hook
;;   because highlight-changes-mode-hook can do both.

;;; Code:

(require 'wid-edit)

;; ====================== Customization =======================
(defgroup highlight-changes nil
  "Highlight Changes mode."
  :version "20.4"
  :group 'faces)


;; Face information: How the changes appear.

;; Defaults for face: red foreground, no change to background,
;; and underlined if a change is because of a deletion.
;; Note: underlining is helpful in that it shows up changes in white space.
;; However, having it set for non-delete changes can be annoying because all
;; indentation on inserts gets underlined (which can look pretty ugly!).

(defface highlight-changes
  '((((min-colors 88) (class color)) (:foreground "red1"))
    (((class color)) (:foreground "red" ))
    (t (:inverse-video t)))
  "Face used for highlighting changes."
  :group 'highlight-changes)
(define-obsolete-face-alias 'highlight-changes-face
  'highlight-changes "22.1")

;; This looks pretty ugly, actually.  Maybe the underline should be removed.
(defface highlight-changes-delete
  '((((min-colors 88) (class color)) (:foreground "red1" :underline t))
    (((class color)) (:foreground "red" :underline t))
    (t (:inverse-video t)))
  "Face used for highlighting deletions."
  :group 'highlight-changes)
(define-obsolete-face-alias 'highlight-changes-delete-face
  'highlight-changes-delete "22.1")


;; A (not very good) default list of colors to rotate through.
(define-obsolete-variable-alias 'highlight-changes-colours
                                'highlight-changes-colors "22.1")

(defcustom highlight-changes-colors
  (if (eq (frame-parameter nil 'background-mode) 'light)
      ;; defaults for light background:
      '( "magenta" "blue" "darkgreen" "chocolate" "sienna4" "NavyBlue")
      ;; defaults for dark background:
    '("yellow" "magenta" "blue" "maroon" "firebrick" "green4" "DarkOrchid"))
  "Colors used by `highlight-changes-rotate-faces'.
The newest rotated change will be displayed in the first element of this list,
the next older will be in the second element etc.

This list is used if `highlight-changes-face-list' is nil, otherwise that
variable overrides this list.  If you only care about foreground
colors then use this, if you want fancier faces then set
`highlight-changes-face-list'."
  :type '(repeat color)
  :group 'highlight-changes)

;; When you invoke highlight-changes-mode, should highlight-changes-visible-mode
;; be on or off?

(define-obsolete-variable-alias 'highlight-changes-initial-state
  'highlight-changes-visibility-initial-state "23.1")

(defcustom highlight-changes-visibility-initial-state t
  "Controls whether changes are initially visible in Highlight Changes mode.

This controls the initial value of `highlight-changes-visible-mode'.
When a buffer is in Highlight Changes mode the function
`highlight-changes-visible-mode' is used to toggle the mode on or off."
  :type 'boolean
  :group 'highlight-changes)

;; highlight-changes-global-initial-state has been removed



;; These are the strings displayed in the mode-line for the minor mode:
(define-obsolete-variable-alias 'highlight-changes-active-string
  'highlight-changes-visible-string "23.1")

(defcustom highlight-changes-visible-string " +Chg"
  "The string used when in Highlight Changes mode and changes are visible.
This should be set to nil if no indication is desired, or to
a string with a leading space."
  :type '(choice string
		 (const :tag "None"  nil))
  :group 'highlight-changes)

(define-obsolete-variable-alias 'highlight-changes-passive-string
  'highlight-changes-invisible-string "23.1")

(defcustom highlight-changes-invisible-string " -Chg"
  "The string used when in Highlight Changes mode and changes are hidden.
This should be set to nil if no indication is desired, or to
a string with a leading space."
  :type '(choice string
		 (const :tag "None"  nil))
  :group 'highlight-changes)

(defcustom highlight-changes-global-modes t
  "Determine whether a buffer is suitable for global Highlight Changes mode.

A function means call that function to decide: if it returns non-nil,
the buffer is suitable.

A list means the elements are major modes suitable for Highlight
Changes mode, or a list whose first element is `not' followed by major
modes which are not suitable.

A value of t means the buffer is suitable if it is visiting a file and
its name does not begin with ` ' or `*'.

A value of nil means no buffers are suitable for `global-highlight-changes-mode'
\(effectively disabling the mode).

Example:
	(c-mode c++-mode)
means that Highlight Changes mode is turned on for buffers in C and C++
modes only."
  :type '(choice
	  (const :tag "all non-special buffers visiting files" t)
	  (set :menu-tag "specific modes" :tag "modes"
	       :value (not)
	       (const :tag "All except these" not)
	       (repeat :tag "Modes" :inline t (symbol :tag "mode")))
	  (function :menu-tag "determined by function"
			   :value buffer-file-name)
	  (const :tag "none" nil)
	  )
  :group 'highlight-changes)

(defcustom highlight-changes-global-changes-existing-buffers nil
  "If non-nil, toggling global Highlight Changes mode affects existing buffers.
Normally, `global-highlight-changes' affects only new buffers (to be
created).  However, if `highlight-changes-global-changes-existing-buffers'
is non-nil, then turning on `global-highlight-changes' will turn on
Highlight Changes mode in suitable buffers, and turning the mode off will
remove it from existing buffers."
  :type 'boolean
  :group 'highlight-changes)

;; These are for internal use.

(defvar hilit-chg-list nil)
(defvar hilit-chg-string " ??")

(make-variable-buffer-local 'hilit-chg-string)



;;; Functions...

;;;###autoload
(define-minor-mode highlight-changes-mode
  "Toggle highlighting changes in this buffer (Highlight Changes mode).
With a prefix argument ARG, enable Highlight Changes mode if ARG
is positive, and disable it otherwise.  If called from Lisp,
enable the mode if ARG is omitted or nil.

When Highlight Changes is enabled, changes are marked with a text
property.  Normally they are displayed in a distinctive face, but
command \\[highlight-changes-visible-mode] can be used to toggles
this on and off.

Other functions for buffers in this mode include:
\\[highlight-changes-next-change] - move point to beginning of next change
\\[highlight-changes-previous-change] - move to beginning of previous change
\\[highlight-changes-remove-highlight] - remove the change face from the region
\\[highlight-changes-rotate-faces] - rotate different \"ages\" of changes
through	various faces.
\\[highlight-compare-with-file] - mark text as changed by comparing this
buffer with the contents of a file
\\[highlight-compare-buffers] highlights differences between two buffers."
  nil			;; init-value
  hilit-chg-string	;; lighter
  nil			;; keymap
  (if (or (display-color-p)
	  (and (fboundp 'x-display-grayscale-p) (x-display-grayscale-p)))
      (progn
	(if (and (eq this-command 'global-highlight-changes-mode)
		 (not highlight-changes-global-changes-existing-buffers))
	    ;; The global mode has toggled the value of the mode variable,
	    ;; but not other changes have been mode, so we are safe
	    ;; to retoggle it.
	    (setq highlight-changes-mode (not highlight-changes-mode)))
	(if highlight-changes-mode
	    ;; it is being turned on
	    (hilit-chg-set)
	  ;; mode is turned off
	  (hilit-chg-clear)))
    (message "Highlight Changes mode requires color or grayscale display")))


;;;###autoload
(define-minor-mode highlight-changes-visible-mode
  "Toggle visibility of highlighting due to Highlight Changes mode.
With a prefix argument ARG, enable Highlight Changes Visible mode
if ARG is positive, and disable it otherwise.  If called from
Lisp, enable the mode if ARG is omitted or nil.

Highlight Changes Visible mode only has an effect when Highlight
Changes mode is on.  When enabled, the changed text is displayed
in a distinctive face.

The default value can be customized with variable
`highlight-changes-visibility-initial-state'.

This command does not itself set highlight-changes mode."

  t		;; init-value
  nil		;; lighter
  nil		;; keymap

  (hilit-chg-update)
  )


(defun hilit-chg-cust-fix-changes-face-list (w _wc &optional event)
  ;; When customization function `highlight-changes-face-list' inserts a new
  ;; face it uses the default face.  We don't want the user to modify this
  ;; face, so we rename the faces in the list on an insert.  The rename is
  ;; actually done by copying the faces so user-defined faces still remain
  ;; in the same order.
  ;; The notifying the parent is needed because without it changes to the
  ;; faces are saved but not to the actual list itself.
  (let ((old-list (widget-value w)))
    (if (member 'default old-list)
	(let
	    ((p (reverse old-list))
	     (n (length old-list))
	     new-name old-name
	     (new-list nil)
	     )
	  (while p
	    (setq old-name (car p))
	    (setq new-name (intern (format "highlight-changes-%d" n)))
	    (if (eq old-name new-name)
		nil
	      ;; A new face has been inserted: we don't want to modify the
	      ;; default face so copy it.  Better, though, (I think) is to
	      ;; make a new face have the same attributes as
	      ;; the `highlight-changes' face.
	      (if (eq old-name 'default)
		  (copy-face 'highlight-changes new-name)
		(copy-face old-name new-name)
		))
	    (setq new-list (append (list new-name) new-list))
	    (setq n (1- n))
	    (setq p (cdr p)))
	  (if (equal new-list (widget-value w))
	      nil ;; (message "notify: no change!")
	    (widget-value-set w new-list)
	    (widget-setup)
	    )
	  )
      ;; (message "notify: no default here!")
      ))
  (let ((parent (widget-get w :parent)))
    (when parent
      (widget-apply parent :notify w event))))


(defcustom highlight-changes-face-list nil
  "A list of faces used when rotating changes.
Normally the variable is initialized to nil and the list is created from
`highlight-changes-colors' when needed.  However, you can set this variable
to any list of faces.  You will have to do this if you want faces which
don't just differ from the `highlight-changes' face by the foreground color.
Otherwise, this list will be constructed when needed from
`highlight-changes-colors'."
  :type '(choice
	  (repeat
	    :notify hilit-chg-cust-fix-changes-face-list
	    face  )
	  (const :tag "Derive from highlight-changes-colors"  nil)
	  )
  :group 'highlight-changes)


(defun hilit-chg-map-changes (func &optional start-position end-position)
  "Call function FUNC for each region used by Highlight Changes mode.
If START-POSITION is nil, (point-min) is used.
If END-POSITION is nil, (point-max) is used.
FUNC is called with 3 params: PROPERTY START STOP."
  (let ((start (or start-position (point-min)))
	(limit (or end-position (point-max)))
	prop end)
    (while (and start (< start limit))
      (setq prop (get-text-property start 'hilit-chg))
      (setq end (text-property-not-all start limit 'hilit-chg prop))
      (if prop
	  (funcall func prop start (or end limit)))
      (setq start end))))


(defun hilit-chg-display-changes (&optional beg end)
  "Display face information for Highlight Changes mode.

An overlay from BEG to END containing a change face is added from the
information in the text property of type `hilit-chg'.

This is the opposite of `hilit-chg-hide-changes'."
  (hilit-chg-map-changes 'hilit-chg-make-ov beg end))


(defun hilit-chg-make-ov (prop start end)
  (or prop
      (error "hilit-chg-make-ov: prop is nil"))
  ;; For the region create overlays with a distinctive face
  ;; and the text property 'hilit-chg.
  (let ((ov (make-overlay start end))
	(face (if (eq prop 'hilit-chg-delete)
                  'highlight-changes-delete
                (nth 1 (member prop hilit-chg-list)))))
    (if face
	(progn
	  ;; We must mark the face, that is the purpose of the overlay.
	  (overlay-put ov 'face face)
	  ;; I don't think we need to set evaporate since we should
	  ;; be controlling them!
	  (overlay-put ov 'evaporate t)
	  ;; We set the change property so we can tell this is one
	  ;; of our overlays (so we don't delete someone else's).
	  (overlay-put ov 'hilit-chg t)
	  )
      (error "hilit-chg-make-ov: no face for prop: %s" prop))))

(defun hilit-chg-hide-changes (&optional beg end)
  "Remove face information for Highlight Changes mode.

The overlay containing the face is removed, but the text property
containing the change information is retained.

This is the opposite of `hilit-chg-display-changes'."
  (let ((start (or beg (point-min)))
	(limit (or end (point-max))))
    (dolist (p (overlays-in start limit))
      ;; don't delete the overlay if it isn't ours!
      (if (overlay-get p 'hilit-chg)
	  (delete-overlay p)))))


(defun hilit-chg-fixup (beg end)
  "Fix change overlays in region between BEG and END.

Ensure the overlays agree with the changes as determined from
the text properties of type `hilit-chg'."
  ;; Remove or alter overlays in region beg..end
  (remove-overlays beg end 'hilit-chg t)
  (hilit-chg-display-changes beg end))

;; Inspired by font-lock.  Something like this should be moved to subr.el.
(defmacro highlight-save-buffer-state (&rest body)
  "Bind variables according to VARLIST and eval BODY restoring buffer state."
  (declare (indent 0) (debug t))
  (let ((modified (make-symbol "modified")))
    `(let* ((,modified (buffer-modified-p))
            (inhibit-modification-hooks t)
            deactivate-mark
            ;; So we don't check the file's mtime.
            buffer-file-name
            buffer-file-truename)
       (progn
         ,@body)
       (unless ,modified
         (restore-buffer-modified-p nil)))))

;;;###autoload
(defun highlight-changes-remove-highlight (beg end)
  "Remove the change face from the region between BEG and END.
This allows you to manually remove highlighting from uninteresting changes."
  (interactive "r")
  (highlight-save-buffer-state
    (remove-text-properties beg end '(hilit-chg nil))
    (hilit-chg-fixup beg end)))

(defun hilit-chg-set-face-on-change (beg end leng-before
					 &optional no-property-change)
  "Record changes and optionally display them in a distinctive face.
`hilit-chg-set' adds this function to the `after-change-functions' hook."
  ;;
  ;; This function is called by the `after-change-functions' hook, which
  ;; is how we are notified when text is changed.
  ;; It is also called from `highlight-compare-with-file'.
  ;;
  ;; We do NOT want to simply do this if this is an undo command, because
  ;; otherwise an undone change shows up as changed.  While the properties
  ;; are automatically restored by undo, we must fix up the overlay.
  (save-match-data
    (let (;;(beg-decr 1)
          (end-incr 1)
	  (type 'hilit-chg))
      (if undo-in-progress
	  (if (and highlight-changes-mode
		   highlight-changes-visible-mode)
	      (hilit-chg-fixup beg end))
        (highlight-save-buffer-state
          (if (and (= beg end) (> leng-before 0))
              ;; deletion
              (progn
                ;; The eolp and bolp tests are a kludge!  But they prevent
                ;; rather nasty looking displays when deleting text at the end
                ;; of line, such as normal corrections as one is typing and
                ;; immediately makes a correction, and when deleting first
                ;; character of a line.
                ;; (if (= leng-before 1)
                ;;     (if (eolp)
                ;;         (setq beg-decr 0 end-incr 0)
                ;;       (if (bolp)
                ;;   	(setq beg-decr 0))))
                ;; (setq beg (max (- beg beg-decr) (point-min)))
                (setq end (min (+ end end-incr) (point-max)))
                (setq type 'hilit-chg-delete))
            ;; Not a deletion.
            ;; Most of the time the following is not necessary, but
            ;; if the current text was marked as a deletion then
            ;; the old overlay is still in effect, so if we add some
            ;; text then remove the deletion marking, but set it to
	  ;; changed otherwise its highlighting disappears.
	  (if (eq (get-text-property end 'hilit-chg) 'hilit-chg-delete)
	      (progn
		(put-text-property end (+ end 1) 'hilit-chg 'hilit-chg)
		(if highlight-changes-visible-mode
		    (hilit-chg-fixup beg (+ end 1))))))
          (unless no-property-change
            (put-text-property beg end 'hilit-chg type))
          (if (or highlight-changes-visible-mode no-property-change)
              (hilit-chg-make-ov type beg end)))))))

(defun hilit-chg-update ()
  "Update a buffer's highlight changes when visibility changed."
  (if highlight-changes-visible-mode
      ;; changes are visible
      (progn
	(setq hilit-chg-string highlight-changes-visible-string)
	(or buffer-read-only
	    (hilit-chg-display-changes)))
    ;; changes are invisible
    (setq hilit-chg-string highlight-changes-invisible-string)
    (or buffer-read-only
	(hilit-chg-hide-changes))))

(defun hilit-chg-set ()
  "Turn on Highlight Changes mode for this buffer."
  (remove-hook 'after-change-functions 'hilit-chg-set-face-on-change t)
  (hilit-chg-make-list)
  (setq highlight-changes-mode t)
  (setq highlight-changes-visible-mode highlight-changes-visibility-initial-state)
  (hilit-chg-update)
  (force-mode-line-update)
  (add-hook 'after-change-functions 'hilit-chg-set-face-on-change nil t))

(defun hilit-chg-clear ()
  "Remove Highlight Changes mode for this buffer.
This removes all saved change information."
  (if buffer-read-only
      ;; We print the buffer name because this function could be called
      ;; on many buffers from `global-highlight-changes-mode'.
      (message "Cannot remove highlighting from read-only mode buffer %s"
	       (buffer-name))
    (remove-hook 'after-change-functions 'hilit-chg-set-face-on-change t)
    (highlight-save-buffer-state
      (hilit-chg-hide-changes)
      (hilit-chg-map-changes
       (lambda (_prop start stop)
         (remove-text-properties start stop '(hilit-chg nil)))))
    (setq highlight-changes-mode nil)
    (force-mode-line-update)))


;;;###autoload
(defun highlight-changes-next-change ()
  "Move to the beginning of the next change, if in Highlight Changes mode."
  (interactive)
  (if highlight-changes-mode
      (let ((start (point))
	    prop)
	(setq prop (get-text-property (point) 'hilit-chg))
	(if prop
	    ;; we are in a change
	    (setq start (next-single-property-change (point) 'hilit-chg)))
	(if start
	    (setq start (next-single-property-change start 'hilit-chg)))
	(if start
	    (goto-char start)
	  (message "no next change")))
    (message "This buffer is not in Highlight Changes mode.")))


;;;###autoload
(defun highlight-changes-previous-change ()
  "Move to the beginning of the previous change, if in Highlight Changes mode."
  (interactive)
  (if highlight-changes-mode
      (let ( (start (point)) (prop nil) )
	(or (bobp)
	    (setq prop (get-text-property (1- (point)) 'hilit-chg)))
	(if prop
	    ;; we are in a change
	    (setq start (previous-single-property-change (point) 'hilit-chg)))
	(if start
	    (setq start (previous-single-property-change start 'hilit-chg)))
	;; special handling for the case where (point-min) is a change
	(if start
	    (setq start (or (previous-single-property-change start 'hilit-chg)
			    (if (get-text-property (point-min) 'hilit-chg)
				(point-min)))))
	(if start
	    (goto-char start)
	  (message "no previous change")))
    (message "This buffer is not in Highlight Changes mode.")))

;; ========================================================================

(defun hilit-chg-make-list (&optional force)
  "Construct `hilit-chg-list' and `highlight-changes-face-list'."
  ;; Constructs highlight-changes-face-list if necessary,
  ;; and hilit-chg-list always:
  ;; Maybe this should always be called when rotating a face
  ;; so we pick up any changes?
  (if (or (null highlight-changes-face-list)  ; Don't do it if it
	  force) ; already exists unless FORCE non-nil.
      (let ((p highlight-changes-colors)
	    (n 1) name)
	(setq highlight-changes-face-list nil)
	(while p
	  (setq name (intern (format "highlight-changes-%d" n)))
	  (copy-face 'highlight-changes name)
	  (set-face-foreground name (car p))
	  (setq highlight-changes-face-list
		(append highlight-changes-face-list (list name)))
	  (setq p (cdr p))
	  (setq n (1+ n)))))
  (setq hilit-chg-list (list 'hilit-chg 'highlight-changes))
  (let ((p highlight-changes-face-list)
	(n 1)
	last-category last-face)
    (while p
      (setq last-category (intern (format "change-%d" n)))
      ;; (setq last-face (intern (format "highlight-changes-%d" n)))
      (setq last-face (car p))
      (setq hilit-chg-list
	    (append hilit-chg-list
		    (list last-category last-face)))
      (setq p (cdr p))
      (setq n (1+ n)))
    (setq hilit-chg-list
	  (append hilit-chg-list
		  (list last-category last-face)))))

(defun hilit-chg-bump-change (prop start end)
  "Increment (age) the Highlight Changes mode text property."
  (let ( new-prop )
    (if (eq prop 'hilit-chg-delete)
	(setq new-prop (nth 2 hilit-chg-list))
      (setq new-prop (nth 2 (member prop hilit-chg-list))))
    (if prop
	(put-text-property start end 'hilit-chg new-prop)
      (message "%d-%d unknown property %s not changed" start end prop))))

;;;###autoload
(defun highlight-changes-rotate-faces ()
  "Rotate the faces if in Highlight Changes mode and the changes are visible.

Current changes are displayed in the face described by the first element
of `highlight-changes-face-list', one level older changes are shown in
face described by the second element, and so on.  Very old changes remain
shown in the last face in the list.

You can automatically rotate colors when the buffer is saved by adding
this function to `write-file-functions' as a buffer-local value.  To do
this, eval the following in the buffer to be saved:

  \(add-hook 'write-file-functions 'highlight-changes-rotate-faces nil t)"
  (interactive)
  (when (and highlight-changes-mode highlight-changes-visible-mode)
    (let ((modified (buffer-modified-p))
	  (inhibit-modification-hooks t))
      ;; The `modified' related code tries to combine two goals: (1) Record the
      ;; rotation in `buffer-undo-list' and (2) avoid setting the modified flag
      ;; of the current buffer due to the rotation.  We do this by inserting (in
      ;; `buffer-undo-list') entries restoring buffer-modified-p to nil before
      ;; and after the entry for the rotation.
      ;; FIXME: this is no good: we need to test the `modified' state at the
      ;; time of the undo, not at the time of the "do", otherwise the undo
      ;; may erroneously clear the modified flag.  --Stef
      ;; (unless modified
      ;;   ;; Install the "before" entry.
      ;;   (push '(apply restore-buffer-modified-p nil) buffer-undo-list))
      (unwind-protect
	  (progn
	    ;; ensure hilit-chg-list is made and up to date
	    (hilit-chg-make-list)
	    ;; remove our existing overlays
	    (hilit-chg-hide-changes)
	    ;; for each change text property, increment it
	    (hilit-chg-map-changes 'hilit-chg-bump-change)
	    ;; and display them
	    (hilit-chg-display-changes))
	(unless modified
	  ;; Install the "after" entry.  FIXME: See above.
	  ;; (push '(apply restore-buffer-modified-p nil) buffer-undo-list)

	  (restore-buffer-modified-p nil)))))
  ;; This always returns nil so it is safe to use in write-file-functions
  nil)

;; ========================================================================
;; Comparing buffers/files
;; These use ediff to find the differences.

(defun highlight-markup-buffers
  (buf-a file-a buf-b file-b &optional markup-a-only)
  "Get differences between two buffers and set highlight changes.
Both buffers are done unless optional parameter MARKUP-A-ONLY
is non-nil."
  (eval-and-compile
    (require 'ediff-util))
  (save-window-excursion
    (let* (change-info
	   change-a change-b
	   a-start a-end len-a
	   b-start b-end len-b
	   (bufa-modified (buffer-modified-p buf-a))
	   (bufb-modified (buffer-modified-p buf-b))
	   (buf-a-read-only (with-current-buffer buf-a buffer-read-only))
	   (buf-b-read-only (with-current-buffer buf-b buffer-read-only))
	   temp-a temp-b)
      (if (and file-a bufa-modified)
	  (if (y-or-n-p (format "Save buffer %s?  " buf-a))
	      (with-current-buffer buf-a
		(save-buffer)
		(setq bufa-modified (buffer-modified-p buf-a)))
	    (setq file-a nil)))
      (or file-a
	  (setq temp-a (setq file-a (ediff-make-temp-file buf-a nil))))

      (if (and file-b bufb-modified)
	  (if (y-or-n-p (format "Save buffer %s?  " buf-b))
	      (with-current-buffer buf-b
		(save-buffer)
		(setq bufb-modified (buffer-modified-p buf-b)))
	    (setq file-b nil)))
      (or file-b
	  (setq temp-b (setq file-b (ediff-make-temp-file buf-b nil))))
      (set-buffer buf-a)
      (highlight-changes-mode 1)
      (or markup-a-only (with-current-buffer buf-b
			  (highlight-changes-mode 1)))
      (setq change-info (hilit-chg-get-diff-info buf-a file-a buf-b file-b))


      (setq change-a (car change-info))
      (setq change-b (car (cdr change-info)))

      (hilit-chg-make-list)
      (while change-a
	(setq a-start (nth 0 (car change-a)))
	(setq a-end (nth 1 (car change-a)))
	(setq b-start (nth 0 (car change-b)))
	(setq b-end (nth 1 (car change-b)))
	(setq len-a (- a-end a-start))
	(setq len-b (- b-end b-start))
	(set-buffer buf-a)
	(hilit-chg-set-face-on-change a-start a-end len-b buf-a-read-only)
	(or markup-a-only
	    (with-current-buffer buf-b
	      (hilit-chg-set-face-on-change b-start b-end len-a
					    buf-b-read-only)
	      ))
	(setq change-a (cdr change-a))
	(setq change-b (cdr change-b)))
      (or bufa-modified
	  (with-current-buffer buf-a (set-buffer-modified-p nil)))
      (or bufb-modified
	  (with-current-buffer buf-b (set-buffer-modified-p nil)))
      (if temp-a
	  (delete-file temp-a))
      (if temp-b
	  (delete-file temp-b)))
    ))

;;;###autoload
(defun highlight-compare-buffers (buf-a buf-b)
"Compare two buffers and highlight the differences.

The default is the current buffer and the one in the next window.

If either buffer is modified and is visiting a file, you are prompted
to save the file.

Unless the buffer is unmodified and visiting a file, the buffer is
written to a temporary file for comparison.

If a buffer is read-only, differences will be highlighted but no property
changes are made, so \\[highlight-changes-next-change] and
\\[highlight-changes-previous-change] will not work."
  (interactive
   (list
    (get-buffer (read-buffer "buffer-a " (current-buffer) t))
    (get-buffer
     (read-buffer "buffer-b "
		  (window-buffer (next-window (selected-window))) t))))
  (let ((file-a (buffer-file-name buf-a))
	(file-b (buffer-file-name buf-b)))
    (highlight-markup-buffers buf-a file-a buf-b file-b)
    ))

;;;###autoload
(defun highlight-compare-with-file (file-b)
  "Compare this buffer with a file, and highlight differences.

If the buffer has a backup filename, it is used as the default when
this function is called interactively.

If the current buffer is visiting the file being compared against, it
also will have its differences highlighted.  Otherwise, the file is
read in temporarily but the buffer is deleted.

If the buffer is read-only, differences will be highlighted but no property
changes are made, so \\[highlight-changes-next-change] and
\\[highlight-changes-previous-change] will not work."
  (interactive
   (let ((file buffer-file-name)
	 (file-name nil)
	 (file-dir nil))
     (and file
	  (setq file-name (file-name-nondirectory file)
		file-dir (file-name-directory file)))
     (setq file-name (make-backup-file-name file-name))
     (unless (file-exists-p file-name)
	     (setq file-name nil))
     (list (read-file-name
	    "Find to compare with: "	;; prompt
	    file-dir			;; directory
	    nil				;; default
	    nil				;; existing
	    file-name)			;; initial
	   )))
  (let* ((buf-a (current-buffer))
	 (file-a (buffer-file-name))
	 (existing-buf (get-file-buffer file-b))
	 (buf-b (or existing-buf
		    (find-file-noselect file-b))))
    (highlight-markup-buffers buf-a file-a buf-b file-b (not existing-buf))
    (unless existing-buf
      (kill-buffer buf-b))
    ))


(defun hilit-chg-get-diff-info (buf-a file-a buf-b file-b)
   ;; hilit-e,x,y are set by function hilit-chg-get-diff-list-hk.
  (let (hilit-e hilit-x hilit-y)
    (ediff-setup buf-a file-a buf-b file-b
	       nil nil   ; buf-c file-C
	       'hilit-chg-get-diff-list-hk
	       (list (cons 'ediff-job-name 'something))
	       )
    (ediff-with-current-buffer hilit-e (ediff-really-quit nil))
    (list hilit-x hilit-y)))


(defun hilit-chg-get-diff-list-hk ()
  ;; hilit-e/x/y are dynamically bound by hilit-chg-get-diff-info
  ;; which calls this function as a hook.
  (defvar hilit-x)                      ; placate the byte-compiler
  (defvar hilit-y)
  (defvar hilit-e)
  (setq hilit-e (current-buffer))
  (let ((n 0) extent p va vb a b)
    (setq hilit-x nil hilit-y nil)
    (while (< n ediff-number-of-differences)
      (ediff-make-fine-diffs n)
      (setq va (ediff-get-fine-diff-vector n 'A))
      ;; va is a vector if there are fine differences
      (if va
	  (setq a (append va nil))
	;; if not, get the unrefined difference
	(setq va (ediff-get-difference n 'A))
	(setq a (list (elt va 0))))
      ;; a list a list
      (setq p a)
      (while p
	(setq extent (list (overlay-start (car p))
			   (overlay-end (car p))))
	(setq p (cdr p))
	(setq hilit-x (append hilit-x (list extent) )));; while p
      ;;
      (setq vb (ediff-get-fine-diff-vector n 'B))
      ;; vb is a vector
      (if vb
	  (setq b (append vb nil))
	;; if not, get the unrefined difference
	(setq vb (ediff-get-difference n 'B))
	(setq b (list (elt vb 0))))
      ;; b list a list
      (setq p b)
      (while p
	(setq extent (list (overlay-start (car p))
			   (overlay-end (car p))))
	(setq p (cdr p))
	(setq hilit-y (append hilit-y (list extent) )))
      (setq n (1+ n)));; while
    ;; ediff-quit doesn't work here.
    ;; No point in returning a value, since this is a hook function.
    ))

;; ======================= global-highlight-changes-mode ==============

;;;###autoload
(define-globalized-minor-mode global-highlight-changes-mode
  highlight-changes-mode highlight-changes-mode-turn-on)

(define-obsolete-function-alias
 'global-highlight-changes
  'global-highlight-changes-mode "23.1")

(defun highlight-changes-mode-turn-on ()
  "See if Highlight Changes mode should be turned on for this buffer.
This is called when `global-highlight-changes-mode' is turned on."
  (or highlight-changes-mode			; do nothing if already on
      (if
	  (cond
	   ((null highlight-changes-global-modes)
	    nil)
	   ((functionp highlight-changes-global-modes)
	    (funcall highlight-changes-global-modes))
	    ((listp highlight-changes-global-modes)
	     (if (eq (car-safe highlight-changes-global-modes) 'not)
		 (not (memq major-mode (cdr highlight-changes-global-modes)))
	       (memq major-mode highlight-changes-global-modes)))
	    (t
	     (and
	      (not (string-match "^[ *]" (buffer-name)))
	      (buffer-file-name))))
	  (highlight-changes-mode 1))
	))


;;;; Desktop support.

;; Called by `desktop-create-buffer' to restore `highlight-changes-mode'.
(defun hilit-chg-desktop-restore (desktop-buffer-locals)
  (highlight-changes-mode
   (or (cdr (assq 'highlight-changes-mode desktop-buffer-locals)) 1)))

(add-to-list 'desktop-minor-mode-handlers
             '(highlight-changes-mode . hilit-chg-desktop-restore))

(add-to-list 'desktop-locals-to-save 'highlight-changes-mode)

;; ===================== debug ==================
;; For debug & test use:
;;
;; (defun hilit-chg-debug-show (&optional beg end)
;;   (interactive)
;;   (message "--- hilit-chg-debug-show ---")
;;   (hilit-chg-map-changes (lambda (prop start end)
;; 			     (message "%d-%d: %s" start end prop))
;; 			   beg end
;; 			   ))
;;
;; ================== end of debug ===============

(provide 'hilit-chg)

;;; hilit-chg.el ends here
