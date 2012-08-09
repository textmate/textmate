;;; viper-cmd.el --- Vi command support for Viper

;; Copyright (C) 1997-2012  Free Software Foundation, Inc.

;; Author: Michael Kifer <kifer@cs.stonybrook.edu>
;; Package: viper

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

(provide 'viper-cmd)

;; Compiler pacifier
(defvar viper-minibuffer-current-face)
(defvar viper-minibuffer-insert-face)
(defvar viper-minibuffer-vi-face)
(defvar viper-minibuffer-emacs-face)
(defvar viper-always)
(defvar viper-mode-string)
(defvar viper-custom-file-name)
(defvar viper--key-maps)
(defvar viper--intercept-key-maps)
(defvar iso-accents-mode)
(defvar quail-mode)
(defvar quail-current-str)
(defvar mark-even-if-inactive)
(defvar init-message)
(defvar viper-initial)
(defvar undo-beg-posn)
(defvar undo-end-posn)

(eval-and-compile
  (unless (fboundp 'declare-function) (defmacro declare-function (&rest  r))))
;; end pacifier


(require 'viper-util)
(require 'viper-keym)
(require 'viper-mous)
(require 'viper-macs)
(require 'viper-ex)



;; Generic predicates

;; These test functions are shamelessly lifted from vip 4.4.2 by Aamod Sane

;; generate test functions
;; given symbol foo, foo-p is the test function, foos is the set of
;; Viper command keys
;; (macroexpand '(viper-test-com-defun foo))
;; (defun foo-p (com) (consp (memq com foos)))

(defmacro viper-test-com-defun (name)
  (let* ((snm (symbol-name name))
	 (nm-p (intern (concat snm "-p")))
	 (nms (intern (concat snm "s"))))
    `(defun ,nm-p (com)
       (consp (viper-memq-char com ,nms)
	      ))))

;; Variables for defining VI commands

;; Modifying commands that can be prefixes to movement commands
(defvar viper-prefix-commands '(?c ?d ?y ?! ?= ?# ?< ?> ?\"))
;; define viper-prefix-command-p
(viper-test-com-defun viper-prefix-command)

;; Commands that are pairs eg. dd. r and R here are a hack
(defconst viper-charpair-commands '(?c ?d ?y ?! ?= ?< ?> ?r ?R))
;; define viper-charpair-command-p
(viper-test-com-defun viper-charpair-command)

(defconst viper-movement-commands '(?b ?B ?e ?E ?f ?F ?G ?h ?j ?k ?l
				     ?H ?M ?L ?n ?t ?T ?w ?W ?$ ?%
				     ?^ ?( ?) ?- ?+ ?| ?{ ?} ?[ ?] ?' ?`
				     ?\; ?, ?0 ?? ?/ ?\  ?\C-m
				     space return
				     delete backspace
				     )
				     "Movement commands")
;; define viper-movement-command-p
(viper-test-com-defun viper-movement-command)

;; Vi digit commands
(defconst viper-digit-commands '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))

;; define viper-digit-command-p
(viper-test-com-defun viper-digit-command)

;; Commands that can be repeated by . (dotted)
(defconst viper-dotable-commands '(?c ?d ?C ?s ?S ?D ?> ?<))
;; define viper-dotable-command-p
(viper-test-com-defun viper-dotable-command)

;; Commands that can follow a #
(defconst viper-hash-commands '(?c ?C ?g ?q ?s))
;; define viper-hash-command-p
(viper-test-com-defun viper-hash-command)

;; Commands that may have registers as prefix
(defconst viper-regsuffix-commands '(?d ?y ?Y ?D ?p ?P ?x ?X))
;; define viper-regsuffix-command-p
(viper-test-com-defun viper-regsuffix-command)

(defconst viper-vi-commands (append viper-movement-commands
				  viper-digit-commands
				  viper-dotable-commands
				  viper-charpair-commands
				  viper-hash-commands
				  viper-prefix-commands
				  viper-regsuffix-commands)
  "The list of all commands in Vi-state.")
;; define viper-vi-command-p
(viper-test-com-defun viper-vi-command)

;; Where viper saves mark. This mark is resurrected by m^
(defvar viper-saved-mark nil)

;; Contains user settings for vars affected by viper-set-expert-level function.
;; Not a user option.
(defvar viper-saved-user-settings nil)



;;; CODE

;; sentinels

;; Runs viper-after-change-functions inside after-change-functions
(defun viper-after-change-sentinel (beg end len)
  (run-hook-with-args 'viper-after-change-functions beg end len))

;; Runs viper-before-change-functions inside before-change-functions
(defun viper-before-change-sentinel (beg end)
  (run-hook-with-args 'viper-before-change-functions beg end))

(defsubst viper-post-command-sentinel ()
  (condition-case conds
      (run-hooks 'viper-post-command-hooks)
    (error (viper-message-conditions conds)))
  (if (eq viper-current-state 'vi-state)
      (viper-restore-cursor-color 'after-insert-mode)))

(defsubst viper-pre-command-sentinel ()
  (run-hooks 'viper-pre-command-hooks))

;; Needed so that Viper will be able to figure the last inserted
;; chunk of text with reasonable accuracy.
(defsubst viper-insert-state-post-command-sentinel ()
  (if (and (memq viper-current-state '(insert-state replace-state))
	   viper-insert-point
	   (>= (point) viper-insert-point))
      (setq viper-last-posn-while-in-insert-state (point-marker)))
  (or (viper-overlay-p viper-replace-overlay)
      (progn
	(viper-set-replace-overlay (point-min) (point-min))
	(viper-hide-replace-overlay)))
  (if (eq viper-current-state 'insert-state)
      (let ((icolor (viper-frame-value viper-insert-state-cursor-color)))
	(or (stringp (viper-get-saved-cursor-color-in-insert-mode))
	    (string= (viper-get-cursor-color) icolor)
	    ;; save current color, if not already saved
	    (viper-save-cursor-color 'before-insert-mode))
	;; set insert mode cursor color
	(viper-change-cursor-color icolor)))
  (let ((ecolor (viper-frame-value viper-emacs-state-cursor-color)))
    (when (and ecolor (eq viper-current-state 'emacs-state))
      (or (stringp (viper-get-saved-cursor-color-in-emacs-mode))
	  (string= (viper-get-cursor-color) ecolor)
	  ;; save current color, if not already saved
	  (viper-save-cursor-color 'before-emacs-mode))
      ;; set emacs mode cursor color
      (viper-change-cursor-color ecolor)))

  (if (and (memq this-command '(dabbrev-expand hippie-expand))
	   (integerp viper-pre-command-point)
	   (markerp viper-insert-point)
	   (marker-position viper-insert-point)
	   (> viper-insert-point viper-pre-command-point))
      (viper-move-marker-locally viper-insert-point viper-pre-command-point)))

(defsubst viper-preserve-cursor-color ()
  (or (memq this-command '(self-insert-command
			   viper-del-backward-char-in-insert
			   viper-del-backward-char-in-replace
			   viper-delete-backward-char
			   viper-join-lines
			   viper-delete-char))
      (memq (viper-event-key last-command-event)
	    '(up down left right (meta f) (meta b)
		 (control n) (control p) (control f) (control b)))))

(defsubst viper-insert-state-pre-command-sentinel ()
  (or (viper-preserve-cursor-color)
      (viper-restore-cursor-color 'after-insert-mode))
  (if (and (memq this-command '(dabbrev-expand hippie-expand))
	   (markerp viper-insert-point)
	   (marker-position viper-insert-point))
      (setq viper-pre-command-point (marker-position viper-insert-point))))

(defun viper-R-state-post-command-sentinel ()
  ;; Restoring cursor color is needed despite
  ;; viper-replace-state-pre-command-sentinel: When you jump to another buffer
  ;; in another frame, the pre-command hook won't change cursor color to
  ;; default in that other frame.  So, if the second frame cursor was red and
  ;; we set the point outside the replacement region, then the cursor color
  ;; will remain red.  Restoring the default, below, prevents this.
  (if (and (<= (viper-replace-start) (point))
	   (<=  (point) (viper-replace-end)))
      (viper-change-cursor-color
       (viper-frame-value viper-replace-overlay-cursor-color))
    (viper-restore-cursor-color 'after-replace-mode)))

;; to speed up, don't change cursor color before self-insert
;; and common move commands
(defsubst viper-replace-state-pre-command-sentinel ()
  (or (viper-preserve-cursor-color)
      (viper-restore-cursor-color 'after-replace-mode)))


;; Make sure we don't delete more than needed.
;; This is executed at viper-last-posn-in-replace-region
(defsubst viper-trim-replace-chars-to-delete-if-necessary ()
  (setq viper-replace-chars-to-delete
	(max 0
	     (min viper-replace-chars-to-delete
		  ;; Don't delete more than to the end of repl overlay
		  (viper-chars-in-region
		   (viper-replace-end) viper-last-posn-in-replace-region)
		  ;; point is viper-last-posn-in-replace-region now
		  ;; So, this limits deletion to the end of line
		  (viper-chars-in-region (point) (viper-line-pos 'end))
		  ))))


(defun viper-replace-state-post-command-sentinel ()
  ;; Restoring cursor color is needed despite
  ;; viper-replace-state-pre-command-sentinel: When one jumps to another buffer
  ;; in another frame, the pre-command hook won't change cursor color to
  ;; default in that other frame.  So, if the second frame cursor was red and
  ;; we set the point outside the replacement region, then the cursor color
  ;; will remain red.  Restoring the default, below, fixes this problem.
  ;;
  ;; We optimize for some commands, like self-insert-command,
  ;; viper-delete-backward-char, etc., since they either don't change
  ;; cursor color or, if they terminate replace mode, the color will be changed
  ;; in viper-finish-change
  (or (viper-preserve-cursor-color)
      (viper-restore-cursor-color 'after-replace-mode))
  (cond
   ((eq viper-current-state 'replace-state)
    ;; delete characters to compensate for inserted chars.
    (let ((replace-boundary (viper-replace-end)))
      (save-excursion
	(goto-char viper-last-posn-in-replace-region)
	(viper-trim-replace-chars-to-delete-if-necessary)
	(delete-char viper-replace-chars-to-delete)
	(setq viper-replace-chars-to-delete 0)
	;; terminate replace mode if reached replace limit
	(if (= viper-last-posn-in-replace-region (viper-replace-end))
	    (viper-finish-change)))

      (when (viper-pos-within-region
	     (point) (viper-replace-start) replace-boundary)
	;; the state may have changed in viper-finish-change above
	(if (eq viper-current-state 'replace-state)
	    (viper-change-cursor-color
	     (viper-frame-value viper-replace-overlay-cursor-color)))
	(setq viper-last-posn-in-replace-region (point-marker)))))
   ;; terminate replace mode if changed Viper states.
   (t (viper-finish-change))))


;; changing mode

;; Change state to NEW-STATE---either emacs-state, vi-state, or insert-state.
(defun viper-change-state (new-state)
  ;; Keep viper-post/pre-command-hooks fresh.
  ;; We remove then add viper-post/pre-command-sentinel since it is very
  ;; desirable that viper-pre-command-sentinel is the last hook and
  ;; viper-post-command-sentinel is the first hook.

  (when (featurep 'xemacs)
    (make-local-hook 'viper-after-change-functions)
    (make-local-hook 'viper-before-change-functions)
    (make-local-hook 'viper-post-command-hooks)
    (make-local-hook 'viper-pre-command-hooks))

  (remove-hook 'post-command-hook 'viper-post-command-sentinel)
  (add-hook 'post-command-hook 'viper-post-command-sentinel)
  (remove-hook 'pre-command-hook 'viper-pre-command-sentinel)
  (add-hook 'pre-command-hook 'viper-pre-command-sentinel t)
  ;; These hooks will be added back if switching to insert/replace mode
  (remove-hook 'viper-post-command-hooks
	       'viper-insert-state-post-command-sentinel 'local)
  (remove-hook 'viper-pre-command-hooks
	       'viper-insert-state-pre-command-sentinel 'local)
  (setq viper-intermediate-command nil)
  (cond ((eq new-state 'vi-state)
	 (cond ((member viper-current-state '(insert-state replace-state))

		;; move viper-last-posn-while-in-insert-state
		;; This is a normal hook that is executed in insert/replace
		;; states after each command.  In Vi/Emacs state, it does
		;; nothing.  We need to execute it here to make sure that
		;; the last posn was recorded when we hit ESC.
		;; It may be left unrecorded if the last thing done in
		;; insert/repl state was dabbrev-expansion or abbrev
		;; expansion caused by hitting ESC
		(viper-insert-state-post-command-sentinel)

		(condition-case conds
		    (progn
		      (viper-save-last-insertion
		       viper-insert-point
		       viper-last-posn-while-in-insert-state)
		      (if viper-began-as-replace
			  (setq viper-began-as-replace nil)
			;; repeat insert commands if numerical arg > 1
			(save-excursion
			  (viper-repeat-insert-command))))
		  (error
		   (viper-message-conditions conds)))

		(if (> (length viper-last-insertion) 0)
		    (viper-push-onto-ring viper-last-insertion
					  'viper-insertion-ring))

		(if viper-ESC-moves-cursor-back
		    (or (bolp) (viper-beginning-of-field) (backward-char 1))))
	       ))

	;; insert or replace
	((memq new-state '(insert-state replace-state))
	 (if (memq viper-current-state '(emacs-state vi-state))
	     (viper-move-marker-locally 'viper-insert-point (point)))
	 (viper-move-marker-locally
	  'viper-last-posn-while-in-insert-state (point))
	 (add-hook 'viper-post-command-hooks
		   'viper-insert-state-post-command-sentinel t 'local)
	 (add-hook 'viper-pre-command-hooks
		   'viper-insert-state-pre-command-sentinel t 'local))
	) ; outermost cond

  ;; Nothing needs to be done to switch to emacs mode! Just set some
  ;; variables, which is already done in viper-change-state-to-emacs!

  ;; ISO accents
  ;; always turn off iso-accents-mode in vi-state, or else we won't be able to
  ;; use the keys `,',^ , as they will do accents instead of Vi actions.
  (cond ((eq new-state 'vi-state) (viper-set-iso-accents-mode nil));accents off
	(viper-automatic-iso-accents (viper-set-iso-accents-mode t));accents on
	(t (viper-set-iso-accents-mode nil)))
  ;; Always turn off quail mode in vi state
  (cond ((eq new-state 'vi-state) (viper-set-input-method nil)) ;intl input off
	(viper-special-input-method (viper-set-input-method t)) ;intl input on
	(t (viper-set-input-method nil)))

  (setq viper-current-state new-state)

  (viper-update-syntax-classes)
  (viper-normalize-minor-mode-map-alist)
  (viper-adjust-keys-for new-state)
  (viper-set-mode-vars-for new-state)
  (viper-refresh-mode-line)
  )


(defun viper-adjust-keys-for (state)
  "Make necessary adjustments to keymaps before entering STATE."
  (cond ((memq state '(insert-state replace-state))
	 (if viper-auto-indent
	     (progn
	       (define-key viper-insert-basic-map "\C-m" 'viper-autoindent)
	       (if viper-want-emacs-keys-in-insert
		   ;; expert
		   (define-key viper-insert-basic-map "\C-j" nil)
		 ;; novice
		 (define-key viper-insert-basic-map "\C-j" 'viper-autoindent)))
	   (define-key viper-insert-basic-map "\C-m" nil)
	   (define-key viper-insert-basic-map "\C-j" nil))

	 (setq viper-insert-diehard-minor-mode
	       (not viper-want-emacs-keys-in-insert))

	 (if viper-want-ctl-h-help
	     (progn
	       (define-key viper-insert-basic-map "\C-h" 'help-command)
	       (define-key viper-replace-map "\C-h" 'help-command))
	   (define-key viper-insert-basic-map
	     "\C-h" 'viper-del-backward-char-in-insert)
	   (define-key viper-replace-map
	     "\C-h" 'viper-del-backward-char-in-replace))
	 ;; In XEmacs, C-h overrides backspace, so we make sure it doesn't.
	 (define-key viper-insert-basic-map
	   [backspace] 'viper-del-backward-char-in-insert)
	 (define-key viper-replace-map
	   [backspace] 'viper-del-backward-char-in-replace)
	 ) ; end insert/replace case
	(t ; Vi state
	 (setq viper-vi-diehard-minor-mode (not viper-want-emacs-keys-in-vi))
	 (if viper-want-ctl-h-help
	     (define-key viper-vi-basic-map "\C-h" 'help-command)
	   (define-key viper-vi-basic-map "\C-h" 'viper-backward-char))
	 ;; In XEmacs, C-h overrides backspace, so we make sure it doesn't.
	 (define-key viper-vi-basic-map [backspace] 'viper-backward-char))
	))


;; Normalizes minor-mode-map-alist by putting Viper keymaps first.
;; This ensures that Viper bindings are in effect, regardless of which minor
;; modes were turned on by the user or by other packages.
(defun viper-normalize-minor-mode-map-alist ()
  (setq viper--intercept-key-maps
	(list
	 (cons 'viper-vi-intercept-minor-mode viper-vi-intercept-map)
	 (cons 'viper-insert-intercept-minor-mode viper-insert-intercept-map)
	 (cons 'viper-emacs-intercept-minor-mode viper-emacs-intercept-map)
	 ))
  (setq viper--key-maps
	(list (cons 'viper-vi-minibuffer-minor-mode viper-minibuffer-map)
	      (cons 'viper-vi-local-user-minor-mode viper-vi-local-user-map)
	      (cons 'viper-vi-kbd-minor-mode viper-vi-kbd-map)
	      (cons 'viper-vi-global-user-minor-mode viper-vi-global-user-map)
	      (cons 'viper-vi-state-modifier-minor-mode
		    (if (keymapp
			 (cdr (assoc major-mode viper-vi-state-modifier-alist)))
			(cdr (assoc major-mode viper-vi-state-modifier-alist))
		      viper-empty-keymap))
	      (cons 'viper-vi-diehard-minor-mode  viper-vi-diehard-map)
	      (cons 'viper-vi-basic-minor-mode     viper-vi-basic-map)
	       (cons 'viper-replace-minor-mode  viper-replace-map)
	       ;; viper-insert-minibuffer-minor-mode must come after
	       ;; viper-replace-minor-mode
	       (cons 'viper-insert-minibuffer-minor-mode
		     viper-minibuffer-map)
	       (cons 'viper-insert-local-user-minor-mode
		     viper-insert-local-user-map)
	       (cons 'viper-insert-kbd-minor-mode viper-insert-kbd-map)
	       (cons 'viper-insert-global-user-minor-mode
		     viper-insert-global-user-map)
	       (cons 'viper-insert-state-modifier-minor-mode
		     (if (keymapp
			  (cdr (assoc major-mode
				      viper-insert-state-modifier-alist)))
			 (cdr (assoc major-mode
				     viper-insert-state-modifier-alist))
		       viper-empty-keymap))
	       (cons 'viper-insert-diehard-minor-mode viper-insert-diehard-map)
	       (cons 'viper-insert-basic-minor-mode viper-insert-basic-map)
	       (cons 'viper-emacs-local-user-minor-mode
		     viper-emacs-local-user-map)
	       (cons 'viper-emacs-kbd-minor-mode viper-emacs-kbd-map)
	       (cons 'viper-emacs-global-user-minor-mode
		     viper-emacs-global-user-map)
	       (cons 'viper-emacs-state-modifier-minor-mode
		     (if (keymapp
			  (cdr
			   (assoc major-mode viper-emacs-state-modifier-alist)))
			 (cdr
			  (assoc major-mode viper-emacs-state-modifier-alist))
		       viper-empty-keymap))
	       ))

  ;; This var is not local in Emacs, so we make it local.  It must be local
  ;; because although the stack of minor modes can be the same for all buffers,
  ;; the associated *keymaps* can be different.  In Viper,
  ;; viper-vi-local-user-map, viper-insert-local-user-map, and others can have
  ;; different keymaps for different buffers.  Also, the keymaps associated
  ;; with viper-vi/insert-state-modifier-minor-mode can be different.
  ;; ***This is needed only in case emulation-mode-map-alists is not defined.
  ;; In emacs with emulation-mode-map-alists, nothing needs to be done
  (unless
      (and (fboundp 'add-to-ordered-list) (boundp 'emulation-mode-map-alists))
    (set (make-local-variable 'minor-mode-map-alist)
         (viper-append-filter-alist
          (append viper--intercept-key-maps viper--key-maps)
          minor-mode-map-alist)))
  )



;; Viper mode-changing commands and utilities

;; Modifies mode-line-buffer-identification.
(defun viper-refresh-mode-line ()
  (set (make-local-variable 'viper-mode-string)
	(cond ((eq viper-current-state 'emacs-state) viper-emacs-state-id)
	      ((eq viper-current-state 'vi-state) viper-vi-state-id)
	      ((eq viper-current-state 'replace-state) viper-replace-state-id)
	      ((eq viper-current-state 'insert-state) viper-insert-state-id)))

  ;; Sets Viper mode string in global-mode-string
  (force-mode-line-update))


;; Switch from Insert state to Vi state.
(defun viper-exit-insert-state ()
  (interactive)
  (viper-change-state-to-vi))

(defun viper-set-mode-vars-for (state)
  "Sets Viper minor mode variables to put Viper's state STATE in effect."

  ;; Emacs state
  (setq viper-vi-minibuffer-minor-mode	     nil
        viper-insert-minibuffer-minor-mode   nil
	viper-vi-intercept-minor-mode	     nil
	viper-insert-intercept-minor-mode    nil

	viper-vi-local-user-minor-mode       nil
	viper-vi-kbd-minor-mode        	     nil
	viper-vi-global-user-minor-mode      nil
	viper-vi-state-modifier-minor-mode   nil
	viper-vi-diehard-minor-mode          nil
        viper-vi-basic-minor-mode            nil

	viper-replace-minor-mode 	       nil

	viper-insert-local-user-minor-mode     nil
	viper-insert-kbd-minor-mode            nil
	viper-insert-global-user-minor-mode    nil
	viper-insert-state-modifier-minor-mode nil
	viper-insert-diehard-minor-mode        nil
	viper-insert-basic-minor-mode          nil
	viper-emacs-intercept-minor-mode       t
	viper-emacs-local-user-minor-mode      t
	viper-emacs-kbd-minor-mode             (not (viper-is-in-minibuffer))
	viper-emacs-global-user-minor-mode     t
	viper-emacs-state-modifier-minor-mode  t
	)

  ;; Vi state
  (if (eq state 'vi-state) ; adjust for vi-state
      (setq
       viper-vi-intercept-minor-mode	   t
       viper-vi-minibuffer-minor-mode	   (viper-is-in-minibuffer)
       viper-vi-local-user-minor-mode	   t
       viper-vi-kbd-minor-mode        	   (not (viper-is-in-minibuffer))
       viper-vi-global-user-minor-mode	   t
       viper-vi-state-modifier-minor-mode    t
       ;; don't let the diehard keymap block command completion
       ;; and other things in the minibuffer
       viper-vi-diehard-minor-mode    	   (not
					    (or viper-want-emacs-keys-in-vi
						(viper-is-in-minibuffer)))
       viper-vi-basic-minor-mode      	      t
       viper-emacs-intercept-minor-mode       nil
       viper-emacs-local-user-minor-mode      nil
       viper-emacs-kbd-minor-mode     	      nil
       viper-emacs-global-user-minor-mode     nil
       viper-emacs-state-modifier-minor-mode  nil
       ))

  ;; Insert and Replace states
  (if (member state '(insert-state replace-state))
      (setq
       viper-insert-intercept-minor-mode      t
       viper-replace-minor-mode	     	      (eq state 'replace-state)
       viper-insert-minibuffer-minor-mode     (viper-is-in-minibuffer)
       viper-insert-local-user-minor-mode     t
       viper-insert-kbd-minor-mode     	      (not (viper-is-in-minibuffer))
       viper-insert-global-user-minor-mode     t
       viper-insert-state-modifier-minor-mode  t
       ;; don't let the diehard keymap block command completion
       ;; and other things in the minibuffer
       viper-insert-diehard-minor-mode 	      (not
					       (or
					        viper-want-emacs-keys-in-insert
					        (viper-is-in-minibuffer)))
       viper-insert-basic-minor-mode   	      t
       viper-emacs-intercept-minor-mode       nil
       viper-emacs-local-user-minor-mode      nil
       viper-emacs-kbd-minor-mode     	      nil
       viper-emacs-global-user-minor-mode     nil
       viper-emacs-state-modifier-minor-mode  nil
       ))

  ;; minibuffer faces
  (if (viper-has-face-support-p)
      (setq viper-minibuffer-current-face
	    (cond ((eq state 'emacs-state) viper-minibuffer-emacs-face)
		  ((eq state 'vi-state) viper-minibuffer-vi-face)
		  ((memq state '(insert-state replace-state))
		   viper-minibuffer-insert-face))))

  (if (viper-is-in-minibuffer)
      (viper-set-minibuffer-overlay))
  )

;; This also takes care of the annoying incomplete lines in files.
;; Also, this fixes `undo' to work vi-style for complex commands.
(defun viper-change-state-to-vi ()
  "Change Viper state to Vi."
  (interactive)
  (if (and viper-first-time (not (viper-is-in-minibuffer)))
      (viper-mode)
    (if overwrite-mode (overwrite-mode -1))
    (or (viper-overlay-p viper-replace-overlay)
      (viper-set-replace-overlay (point-min) (point-min)))
    (viper-hide-replace-overlay)
    ;; Expand abbrevs iff the previous character has word syntax.
    (and abbrev-mode
	 (eq (char-syntax (preceding-char)) ?w)
	 (expand-abbrev))
    (if (and auto-fill-function (> (current-column) fill-column))
	(funcall auto-fill-function))
    ;; don't leave whitespace lines around
    (if (and (memq last-command
		   '(viper-autoindent
		     viper-open-line viper-Open-line
		     viper-replace-state-exit-cmd))
	     (viper-over-whitespace-line))
	(indent-to-left-margin))
    (viper-add-newline-at-eob-if-necessary)
    (viper-adjust-undo)

    (if (eq viper-current-state 'emacs-state)
	(viper-restore-cursor-color 'after-emacs-mode)
      (viper-restore-cursor-color 'after-insert-mode))

    (viper-change-state 'vi-state)

    ;; Protect against user errors in hooks
    (condition-case conds
	(run-hooks 'viper-vi-state-hook)
      (error
       (viper-message-conditions conds)))))

(defun viper-change-state-to-insert ()
  "Change Viper state to Insert."
  (interactive)
  (viper-change-state 'insert-state)

  (or (viper-overlay-p viper-replace-overlay)
      (viper-set-replace-overlay (point-min) (point-min)))
  (viper-hide-replace-overlay)

  (let ((icolor (viper-frame-value viper-insert-state-cursor-color)))
    (or (stringp (viper-get-saved-cursor-color-in-insert-mode))
	(string= (viper-get-cursor-color) icolor)
	(viper-save-cursor-color 'before-insert-mode))
    (viper-change-cursor-color icolor))

  ;; Protect against user errors in hooks
  (condition-case conds
      (run-hooks 'viper-insert-state-hook)
    (error
     (viper-message-conditions conds))))

(defsubst viper-downgrade-to-insert ()
  ;; Protect against user errors in hooks
  (condition-case conds
      (run-hooks 'viper-insert-state-hook)
    (error
     (viper-message-conditions conds)))
  (setq viper-current-state 'insert-state
	viper-replace-minor-mode nil))



;; Change to replace state.  When the end of replacement region is reached,
;; replace state changes to insert state.
(defun viper-change-state-to-replace (&optional non-R-cmd)
  (viper-change-state 'replace-state)
  ;; Run insert-state-hook
  (condition-case conds
      (run-hooks 'viper-insert-state-hook 'viper-replace-state-hook)
    (error
     (viper-message-conditions conds)))

  (if non-R-cmd
      (viper-start-replace)
    ;; 'R' is implemented using Emacs's overwrite-mode
    (viper-start-R-mode))
  )


(defun viper-change-state-to-emacs ()
  "Change Viper state to Emacs."
  (interactive)
  (or (viper-overlay-p viper-replace-overlay)
      (viper-set-replace-overlay (point-min) (point-min)))
  (viper-hide-replace-overlay)

  (let ((ecolor (viper-frame-value viper-emacs-state-cursor-color)))
    (when ecolor
      (or (stringp (viper-get-saved-cursor-color-in-emacs-mode))
	  (string= (viper-get-cursor-color) ecolor)
	  (viper-save-cursor-color 'before-emacs-mode))
      (viper-change-cursor-color ecolor)))

  (viper-change-state 'emacs-state)

  ;; Protect against user errors in hooks
  (condition-case conds
      (run-hooks 'viper-emacs-state-hook)
    (error
     (viper-message-conditions conds))))

;; escape to emacs mode temporarily
(defun viper-escape-to-emacs (arg &optional events)
  "Escape to Emacs state from Vi state for one Emacs command.
ARG is used as the prefix value for the executed command.  If
EVENTS is a list of events, which become the beginning of the command."
  (interactive "P")
  (if (viper= (viper-last-command-char) ?\\)
      (message "Switched to EMACS state for the next command..."))
  (viper-escape-to-state arg events 'emacs-state))

;; escape to Vi mode temporarily
(defun viper-escape-to-vi (arg)
  "Escape from Emacs state to Vi state for one Vi 1-character command.
If the Vi command that the user types has a prefix argument, e.g., `d2w', then
Vi's prefix argument will be used.  Otherwise, the prefix argument passed to
`viper-escape-to-vi' is used."
  (interactive "P")
  (message "Switched to VI state for the next command...")
  (viper-escape-to-state arg nil 'vi-state))

;; Escape to STATE mode for one Emacs command.
(defun viper-escape-to-state (arg events state)
  ;;(let (com key prefix-arg)
  (let (com key)
    ;; this temporarily turns off Viper's minor mode keymaps
    (viper-set-mode-vars-for state)
    (viper-normalize-minor-mode-map-alist)
    (if events (viper-set-unread-command-events events))

    ;; protect against keyboard quit and other errors
    (condition-case nil
	(let (viper-vi-kbd-minor-mode
	      viper-insert-kbd-minor-mode
	      viper-emacs-kbd-minor-mode)
	  (unwind-protect
	      (progn
		(setq com
		      (key-binding (setq key (viper-read-key-sequence nil))))
		;; In case of binding indirection--chase definitions.
		;; Have to do it here because we execute this command under
		;; different keymaps, so command-execute may not do the
		;; right thing there
		(while (vectorp com) (setq com (key-binding com))))
	    nil)
	  ;; Execute command com in the original Viper state, not in state
	  ;; `state'.  Otherwise, if we switch buffers while executing the
	  ;; escaped to command, Viper's mode vars will remain those of
	  ;; `state'.  When we return to the orig buffer, the bindings will be
	  ;; screwed up.
	  (viper-set-mode-vars-for viper-current-state)

	  ;; this-command, last-command-char, last-command-event
	  (setq this-command com)
	  (if (featurep 'xemacs)
	      ;; XEmacs represents key sequences as vectors
	      (setq last-command-event
		    (viper-copy-event (viper-seq-last-elt key))
		    last-command-char (event-to-character last-command-event))
	    ;; Emacs represents them as sequences (str or vec)
	    (setq last-command-event
		  (viper-copy-event (viper-seq-last-elt key))))

	  (if (commandp com)
	      ;; pretend that current state is the state we escaped to
	      (let ((viper-current-state state))
		(setq prefix-arg (or prefix-arg arg))
		(command-execute com)))
	  )
      (quit (ding))
      (error (beep 1))))
  ;; set state in the new buffer
  (viper-set-mode-vars-for viper-current-state))

;; This is used in order to allow reading characters according to the input
;; method. The character is read in emacs and inserted into the buffer.
;; If an input method is in effect, this might
;; cause several characters to be combined into one.
;; Also takes care of the iso-accents mode
(defun viper-special-read-and-insert-char ()
  (viper-set-mode-vars-for 'emacs-state)
  (viper-normalize-minor-mode-map-alist)
  (if viper-special-input-method
      (viper-set-input-method t))
  (if viper-automatic-iso-accents
      (viper-set-iso-accents-mode t))
  (condition-case nil
      (let (viper-vi-kbd-minor-mode
	    viper-insert-kbd-minor-mode
	    viper-emacs-kbd-minor-mode
	    ch)
	(cond ((and viper-special-input-method
		    (featurep 'emacs)
		    (fboundp 'quail-input-method))
	       ;; (let ...) is used to restore unread-command-events to the
	       ;; original state. We don't want anything left in there after
	       ;; key translation. (Such left-overs are possible if the user
	       ;; types a regular key.)
	       (let (unread-command-events)
		 ;; The next cmd  and viper-set-unread-command-events
		 ;; are intended to prevent the input method
		 ;; from swallowing ^M, ^Q and other special characters
		 (setq ch (read-char-exclusive))
		 ;; replace ^M with the newline
		 (if (eq ch ?\C-m) (setq ch ?\n))
		 ;; Make sure ^V and ^Q work as quotation chars
		 (if (memq ch '(?\C-v ?\C-q))
		     (setq ch (read-char-exclusive)))
		 (viper-set-unread-command-events ch)
		 (quail-input-method nil)

		 (if (and ch (string= quail-current-str ""))
		     (insert ch)
		   (insert quail-current-str))
		 (setq ch (or ch
			      (aref quail-current-str
				    (1- (length quail-current-str)))))
		 ))
	      ((and viper-special-input-method
		    (featurep 'xemacs)
		    (fboundp 'quail-start-translation))
	       ;; same as above but for XEmacs, which doesn't have
	       ;; quail-input-method
	       (let (unread-command-events)
		 (setq ch (read-char-exclusive))
		 ;; replace ^M with the newline
		 (if (eq ch ?\C-m) (setq ch ?\n))
		 ;; Make sure ^V and ^Q work as quotation chars
		 (if (memq ch '(?\C-v ?\C-q))
		     (setq ch (read-char-exclusive)))
		 (viper-set-unread-command-events ch)
		 (quail-start-translation nil)

		 (if (and ch (string= quail-current-str ""))
		     (insert ch)
		   (insert quail-current-str))
		 (setq ch (or ch
			      (aref quail-current-str
				    (1- (length quail-current-str)))))
		 ))
	      ((and (boundp 'iso-accents-mode) iso-accents-mode)
	       (setq ch (aref (read-key-sequence nil) 0))
	       ;; replace ^M with the newline
	       (if (eq ch ?\C-m) (setq ch ?\n))
	       ;; Make sure ^V and ^Q work as quotation chars
	       (if (memq ch '(?\C-v ?\C-q))
		   (setq ch (aref (read-key-sequence nil) 0)))
	       (insert ch))
	      (t
	       ;;(setq ch (read-char-exclusive))
	       (setq ch (aref (read-key-sequence nil) 0))
	       (if (featurep 'xemacs)
		   (setq ch (event-to-character ch)))
	       ;; replace ^M with the newline
	       (if (eq ch ?\C-m) (setq ch ?\n))
	       ;; Make sure ^V and ^Q work as quotation chars
	       (if (memq ch '(?\C-v ?\C-q))
		   (progn
		     ;;(setq ch (read-char-exclusive))
		     (setq ch (aref (read-key-sequence nil) 0))
		     (if (featurep 'xemacs)
			 (setq ch (event-to-character ch))))
		 )
	       (insert ch))
	      )
	(setq last-command-event
	      (viper-copy-event (if (featurep 'xemacs)
				    (character-to-event ch) ch)))
	) ; let
    (error nil)
    ) ; condition-case

  (viper-set-input-method nil)
  (viper-set-iso-accents-mode nil)
  (viper-set-mode-vars-for viper-current-state)
  )


(defun viper-exec-form-in-vi  (form)
  "Execute FORM in Vi state, regardless of the current Vi state."
  (let ((buff (current-buffer))
	result)
    (viper-set-mode-vars-for 'vi-state)

    (condition-case nil
	(let (viper-vi-kbd-minor-mode) ; execute without kbd macros
	  (setq result (eval form)))
      (error
       (signal 'quit nil)))

    (if (not (equal buff (current-buffer))) ; cmd switched buffer
	(with-current-buffer buff
	  (viper-set-mode-vars-for viper-current-state)))
    (viper-set-mode-vars-for viper-current-state)
    result))

(defun viper-exec-form-in-emacs  (form)
  "Execute FORM in Emacs, temporarily disabling Viper's minor modes.
Similar to `viper-escape-to-emacs', but accepts forms rather than keystrokes."
  (let ((buff (current-buffer))
	result)
    (viper-set-mode-vars-for 'emacs-state)
    (setq result (eval form))
    (if (not (equal buff (current-buffer))) ; cmd switched buffer
	(with-current-buffer buff
	  (viper-set-mode-vars-for viper-current-state)))
    (viper-set-mode-vars-for viper-current-state)
    result))

;; This executes the last kbd event in emacs mode. Is used when we want to
;; interpret certain keys directly in emacs (as, for example, in comint mode).
(defun viper-exec-key-in-emacs (arg)
  (interactive "P")
  (viper-escape-to-emacs arg last-command-event))


;; This is needed because minor modes sometimes override essential Viper
;; bindings.  By letting Viper know which files these modes are in, it will
;; arrange to reorganize minor-mode-map-alist so that things will work right.
(defun viper-harness-minor-mode (load-file)
  "Familiarize Viper with a minor mode defined in LOAD-FILE.
Minor modes that have their own keymaps may overshadow Viper keymaps.
This function is designed to make Viper aware of the packages that define
such minor modes.
Usage:
    (viper-harness-minor-mode load-file)

LOAD-FILE is the name of the file where the specific minor mode is defined.
Suffixes such as .el or .elc should be stripped."

  (interactive "sEnter name of the load file: ")

  (eval-after-load load-file '(viper-normalize-minor-mode-map-alist))

  ;; Change the default for minor-mode-map-alist each time a harnessed minor
  ;; mode adds its own keymap to the a-list.
  (unless
      (and (fboundp 'add-to-ordered-list) (boundp 'emulation-mode-map-alists))
    (eval-after-load
	load-file '(setq-default minor-mode-map-alist minor-mode-map-alist)))
  )


(defun viper-ESC (arg)
  "Emulate ESC key in Emacs.
Prevents multiple escape keystrokes if viper-no-multiple-ESC is true.
If viper-no-multiple-ESC is 'twice double ESC would ding in vi-state.
Other ESC sequences are emulated via the current Emacs's major mode
keymap.  This is more convenient on TTYs, since this won't block
function keys such as up, down, etc.  ESC will also will also work as
a Meta key in this case.  When viper-no-multiple-ESC is nil, ESC works
as a Meta key and any number of multiple escapes are allowed."
  (interactive "P")
  (let (char)
    (cond ((and (not viper-no-multiple-ESC) (eq viper-current-state 'vi-state))
	   (setq char (viper-read-char-exclusive))
	   (viper-escape-to-emacs arg (list ?\e char) ))
	  ((and (eq viper-no-multiple-ESC 'twice)
		(eq viper-current-state 'vi-state))
	   (setq char (viper-read-char-exclusive))
	   (if (= char (string-to-char viper-ESC-key))
	       (ding)
	     (viper-escape-to-emacs arg (list ?\e char) )))
	  (t (ding)))
    ))

(defun viper-alternate-Meta-key (arg)
  "Simulate Emacs Meta key."
  (interactive "P")
  (sit-for 1) (message "ESC-")
  (viper-escape-to-emacs arg '(?\e)))

(defun viper-toggle-key-action ()
  "Action bound to `viper-toggle-key'."
  (interactive)
  (if (and (< viper-expert-level 2) (equal viper-toggle-key "\C-z"))
      (if (viper-window-display-p)
	  (viper-iconify)
	(suspend-emacs))
    (viper-change-state-to-emacs)))


;; Intercept ESC sequences on dumb terminals.
;; Based on the idea contributed by Marcelino Veiga Tuimil <mveiga@dit.upm.es>

;; Check if last key was ESC and if so try to reread it as a function key.
;; But only if there are characters to read during a very short time.
;; Returns the last event, if any.
(defun viper-envelop-ESC-key ()
  (let ((event last-input-event)
	(keyseq [nil])
	(inhibit-quit t))
    (if (viper-ESC-event-p event)
	(progn
	  ;; Some versions of Emacs (eg., 22.50.8 have a bug, which makes even
	  ;; a single ESC into ;; a fast keyseq. To guard against this, we
	  ;; added a check if there are other events as well. Keep the next
	  ;; line for the next time the bug reappears, so that will remember to
	  ;; report it.
	  ;;(if (and (viper-fast-keysequence-p) unread-command-events)
	  (if (viper-fast-keysequence-p) ;; for Emacsen without the above bug
	      (progn
		(let (minor-mode-map-alist emulation-mode-map-alists)
		  (viper-set-unread-command-events event)
		  (setq keyseq (read-key-sequence nil 'continue-echo))
		  ) ; let
		;; If keyseq translates into something that still has ESC
		;; at the beginning, separate ESC from the rest of the seq.
		;; In XEmacs we check for events that are keypress meta-key
		;; and convert them into [escape key]
		;;
		;; This is needed for the following reason:
		;; If ESC is the first symbol, we interpret it as if the
		;; user typed ESC and then quickly some other symbols.
		;; If ESC is not the first one, then the key sequence
		;; entered was apparently translated into a function key or
		;; something (e.g., one may have
		;; (define-key function-key-map "\e[192z" [f11])
		;; which would translate the escape-sequence generated by
		;; f11 in an xterm window into the symbolic key f11.
		;;
		;; If `first-key' is not an ESC event, we make it into the
		;; last-command-event in order to pretend that this key was
		;; pressed.  This is needed to allow arrow keys to be bound to
		;; macros.  Otherwise, viper-exec-mapped-kbd-macro will think
		;; that the last event was ESC and so it'll execute whatever is
		;; bound to ESC. (Viper macros can't be bound to
		;; ESC-sequences).
		(let* ((first-key (elt keyseq 0))
		       (key-mod (event-modifiers first-key)))
		  (cond ((and (viper-ESC-event-p first-key)
			      (not (viper-translate-all-ESC-keysequences)))
			 ;; put keys following ESC on the unread list
			 ;; and return ESC as the key-sequence
			 (viper-set-unread-command-events (viper-subseq keyseq 1))
			 (setq last-input-event event
			       keyseq (if (featurep 'emacs)
					  "\e"
					(vector (character-to-event ?\e)))))
			((and (featurep 'xemacs)
			      (key-press-event-p first-key)
			      (equal '(meta) key-mod))
			 (viper-set-unread-command-events
			  (vconcat (vector
				    (character-to-event (event-key first-key)))
				   (viper-subseq keyseq 1)))
			 (setq last-input-event event
			       keyseq (vector (character-to-event ?\e))))
			((eventp first-key)
			 (setq last-command-event
			       (viper-copy-event first-key)))
			))
		) ; end progn

	    ;; this is escape event with nothing after it
	    ;; put in unread-command-event and then re-read
	    (viper-set-unread-command-events event)
	    (setq keyseq (read-key-sequence nil))
	    ))
      ;; not an escape event
      (setq keyseq (vector event)))
    keyseq))



;; Listen to ESC key.
;; If a sequence of keys starting with ESC is issued with very short delays,
;; interpret these keys in Emacs mode, so ESC won't be interpreted as a Vi key.
(defun viper-intercept-ESC-key ()
  "Function that implements ESC key in Viper emulation of Vi."
  (interactive)
  ;; `key-binding' needs to be called in a context where Viper's
  ;; minor-mode map(s) have been temporarily disabled so the ESC
  ;; binding to viper-intercept-ESC-key doesn't hide the binding we're
  ;; looking for (Bug#9146):
  (let* ((event (viper-envelop-ESC-key))
	 (cmd (cond ((equal event viper-ESC-key)
		     'viper-intercept-ESC-key)
		    ((let ((emulation-mode-map-alists nil))
		       (key-binding event)))
		    (t
		     (error "Viper bell")))))

    ;; call the actual function to execute ESC (if no other symbols followed)
    ;; or the key bound to the ESC sequence (if the sequence was issued
    ;; with very short delay between characters).
    (if (eq cmd 'viper-intercept-ESC-key)
	(setq cmd
	      (cond ((eq viper-current-state 'vi-state)
		     'viper-ESC)
		    ((eq viper-current-state 'insert-state)
		     'viper-exit-insert-state)
		    ((eq viper-current-state 'replace-state)
		     'viper-replace-state-exit-cmd)
		    (t 'viper-change-state-to-vi)
		    )))
    (call-interactively cmd)))




;; prefix argument for Vi mode

;; In Vi mode, prefix argument is a dotted pair (NUM . COM) where NUM
;; represents the numeric value of the prefix argument and COM represents
;; command prefix such as "c", "d", "m" and "y".

;; Get value part of prefix-argument ARG.
(defsubst viper-p-val (arg)
  (cond ((null arg) 1)
	((consp arg)
	 (if (or (null (car arg)) (equal (car arg) '(nil)))
	     1 (car arg)))
	(t arg)))

;; Get raw value part of prefix-argument ARG.
(defsubst viper-P-val (arg)
  (cond ((consp arg) (car arg))
	(t arg)))

;; Get com part of prefix-argument ARG.
(defsubst viper-getcom (arg)
  (cond ((null arg) nil)
	((consp arg) (cdr arg))
	(t nil)))

;; Get com part of prefix-argument ARG and modify it.
(defun viper-getCom (arg)
  (let ((com (viper-getcom arg)))
    (cond ((viper= com ?c) ?c)
	  ;; Previously, ?c was being converted to ?C, but this prevented
	  ;; multiline replace regions.
	  ;;((viper= com ?c) ?C)
	  ((viper= com ?d) ?D)
	  ((viper= com ?y) ?Y)
	  (t com))))


;; Compute numeric prefix arg value.
;; Invoked by EVENT-CHAR.  COM is the command part obtained so far.
(defun viper-prefix-arg-value (event-char com)
  (let ((viper-intermediate-command 'viper-digit-argument)
	value func)
    ;; read while number
    (while (and (viper-characterp event-char)
		(>= event-char ?0) (<= event-char ?9))
      (setq value (+ (* (if (integerp value) value 0) 10) (- event-char ?0)))
      (setq event-char (viper-read-event-convert-to-char)))

    (setq prefix-arg value)
    (if com (setq prefix-arg (cons prefix-arg com)))
    (while (eq event-char ?U)
      (viper-describe-arg prefix-arg)
      (setq event-char (viper-read-event-convert-to-char)))

    (if (or com (and (not (eq viper-current-state 'vi-state))
		     ;; make sure it is a Vi command
		     (viper-characterp event-char)
		     (viper-vi-command-p event-char)
		     ))
	;; If appears to be one of the vi commands,
	;; then execute it with funcall and clear prefix-arg in order to not
	;; confuse subsequent commands
	(progn
	  ;; last-command-event is the char we want emacs to think was typed
	  ;; last.  If com is not nil, the viper-digit-argument command was
	  ;; called from within viper-prefix-arg command, such as `d', `w',
	  ;; etc., i.e., the user typed, say, d2.  In this case, `com' would be
	  ;; `d', `w', etc.  If viper-digit-argument was invoked by
	  ;; viper-escape-to-vi (which is indicated by the fact that the
	  ;; current state is not vi-state), then `event-char' represents the
	  ;; vi command to be executed (e.g., `d', `w', etc).  Again,
	  ;; last-command-event must make emacs believe that this is the command
	  ;; we typed.
	  (cond ((eq event-char 'return) (setq event-char ?\C-m))
		((eq event-char 'delete) (setq event-char ?\C-?))
		((eq event-char 'backspace) (setq event-char ?\C-h))
		((eq event-char 'space) (setq event-char ?\ )))
	  (setq last-command-event
		(if (featurep 'xemacs)
		    (character-to-event (or com event-char))
		  (or com event-char)))
	  (setq func (viper-exec-form-in-vi
		      `(key-binding (char-to-string ,event-char))))
	  (funcall func prefix-arg)
	  (setq prefix-arg nil))
      ;; some other command -- let emacs do it in its own way
      (viper-set-unread-command-events event-char))
    ))


;; Vi operator as prefix argument."
(defun viper-prefix-arg-com (char value com)
  (let ((cont t)
	cmd-info
	cmd-to-exec-at-end)
    (while (and cont
		(viper-memq-char char
				 (list ?c ?d ?y ?! ?< ?> ?= ?# ?r ?R ?\"
				       viper-buffer-search-char)))
      (if com
	  ;; this means that we already have a command character, so we
	  ;; construct a com list and exit while.  however, if char is "
	  ;; it is an error.
	  (progn
	    ;; new com is (CHAR . OLDCOM)
	    (if (viper-memq-char char '(?# ?\")) (error "Viper bell"))
	    (setq com (cons char com))
	    (setq cont nil))
	;; If com is nil we set com as char, and read more.  Again, if char is
	;; ", we read the name of register and store it in viper-use-register.
	;; if char is !, =, or #, a complete com is formed so we exit the while
	;; loop.
	(cond ((viper-memq-char char '(?! ?=))
	       (setq com char)
	       (setq char (read-char))
	       (setq cont nil))
	      ((viper= char ?#)
	       ;; read a char and encode it as com
	       (setq com (+ 128 (read-char)))
	       (setq char (read-char)))
	      ((viper= char ?\")
	       (let ((reg (read-char)))
		 (if (viper-valid-register reg)
		     (setq viper-use-register reg)
		   (error "Viper bell"))
		 (setq char (read-char))))
	      (t
	       (setq com char)
	       (setq char (read-char))))))

    (if (atom com)
	;; `com' is a single char, so we construct the command argument
	;; and if `char' is `?', we describe the arg; otherwise
	;; we prepare the command that will be executed at the end.
	(progn
	  (setq cmd-info (cons value com))
	  (while (viper= char ?U)
	    (viper-describe-arg cmd-info)
	    (setq char (read-char)))
	  ;; `char' is a movement cmd, a digit arg cmd, or a register cmd---so
	  ;; we execute it at the very end
	  (or (viper-movement-command-p char)
	      (viper-digit-command-p char)
	      (viper-regsuffix-command-p char)
	      (viper= char ?!) ; bang command
	      (viper= char ?g) ; the gg command (like G0)
	      (error "Viper bell"))
	  (setq cmd-to-exec-at-end
		(viper-exec-form-in-vi
		 `(key-binding (char-to-string ,char)))))

      ;; as com is non-nil, this means that we have a command to execute
      (if (viper-memq-char (car com) '(?r ?R))
	  ;; execute appropriate region command.
	  (let ((char (car com)) (com (cdr com)))
	    (setq prefix-arg (cons value com))
	    (if (viper= char ?r)
		(viper-region prefix-arg)
	      (viper-Region prefix-arg))
	    ;; reset prefix-arg
	    (setq prefix-arg nil))
	;; otherwise, reset prefix arg and call appropriate command
	(setq value (if (null value) 1 value))
	(setq prefix-arg nil)
	(cond
	 ;; If we change ?C to ?c here, then cc will enter replacement mode
	 ;; rather than deleting lines.  However, it will affect 1 less line
	 ;; than normal.  We decided to not use replacement mode here and
	 ;; follow Vi, since replacement mode on n full lines can be achieved
	 ;; with nC.
	 ((equal com '(?c . ?c)) (viper-line (cons value ?C)))
	 ((equal com '(?d . ?d)) (viper-line (cons value ?D)))
	 ((equal com '(?d . ?y)) (viper-yank-defun))
	 ((equal com '(?y . ?y)) (viper-line (cons value ?Y)))
	 ((equal com '(?< . ?<)) (viper-line (cons value ?<)))
	 ((equal com '(?> . ?>)) (viper-line (cons value ?>)))
	 ((equal com '(?! . ?!)) (viper-line (cons value ?!)))
	 ((equal com '(?= . ?=)) (viper-line (cons value ?=)))
	 ;; gg  acts as G0
	 ((equal (car com) ?g)   (viper-goto-line 0))
	 (t (error "Viper bell")))))

    (if cmd-to-exec-at-end
	(progn
	  (setq last-command-event
		(viper-copy-event
		 (if (featurep 'xemacs) (character-to-event char) char)))
	  (condition-case err
	      (funcall cmd-to-exec-at-end cmd-info)
	    (error
	     (error "%s" (error-message-string err))))))
    ))

(defun viper-describe-arg (arg)
  (let (val com)
    (setq val (viper-P-val arg)
	  com (viper-getcom arg))
    (if (null val)
	(if (null com)
	    (message "Value is nil, and command is nil")
	  (message "Value is nil, and command is `%c'" com))
      (if (null com)
	  (message "Value is `%d', and command is nil" val)
	(message "Value is `%d', and command is `%c'" val com)))))

(defun viper-digit-argument (arg)
  "Begin numeric argument for the next command."
  (interactive "P")
  (viper-leave-region-active)
  (viper-prefix-arg-value
   (viper-last-command-char) (if (consp arg) (cdr arg) nil)))

(defun viper-command-argument (arg)
  "Accept a motion command as an argument."
  (interactive "P")
  (let ((viper-intermediate-command 'viper-command-argument))
    (condition-case nil
	(viper-prefix-arg-com
	 (viper-last-command-char)
	 (cond ((null arg) nil)
	       ((consp arg) (car arg))
	       ((integerp arg) arg)
	       (t (error viper-InvalidCommandArgument)))
	 (cond ((null arg) nil)
	       ((consp arg) (cdr arg))
	       ((integerp arg) nil)
	       (t (error viper-InvalidCommandArgument))))
      (quit (setq viper-use-register nil)
	    (signal 'quit nil)))
    (viper-deactivate-mark)))


;; repeat last destructive command

;; Append region to text in register REG.
;; START and END are buffer positions indicating what to append.
(defsubst viper-append-to-register (reg start end)
  (set-register reg (concat (if (stringp (get-register reg))
				(get-register reg) "")
			    (buffer-substring start end))))

;; Saves last inserted text for possible use by viper-repeat command.
(defun viper-save-last-insertion (beg end)
  (condition-case nil
      (setq viper-last-insertion (buffer-substring beg end))
    (error
     ;; beg or end marker are somehow screwed up
     (setq viper-last-insertion nil)))
  (setq viper-last-insertion (buffer-substring beg end))
  (or (< (length viper-d-com) 5)
      (setcar (nthcdr 4 viper-d-com) viper-last-insertion))
  (or (null viper-command-ring)
      (ring-empty-p viper-command-ring)
      (progn
	(setcar (nthcdr 4 (viper-current-ring-item viper-command-ring))
		viper-last-insertion)
	;; del most recent elt, if identical to the second most-recent
	(viper-cleanup-ring viper-command-ring)))
  )

(defsubst viper-yank-last-insertion ()
  "Inserts the text saved by the previous viper-save-last-insertion command."
  (condition-case nil
      (insert viper-last-insertion)
    (error nil)))


;; define functions to be executed

;; invoked by the `C' command
(defun viper-exec-change (m-com com)
  (or (and (markerp viper-com-point) (marker-position viper-com-point))
      (set-marker viper-com-point (point) (current-buffer)))
  ;; handle C cmd at the eol and at eob.
  (if (or (and (eolp) (= viper-com-point (point)))
	  (= viper-com-point (point-max)))
      (progn
	(insert " ")(backward-char 1)))
  (if (= viper-com-point (point))
      (viper-forward-char-carefully))
  (push-mark viper-com-point)
  (if (eq m-com 'viper-next-line-at-bol)
      (viper-enlarge-region (mark t) (point)))
  (if (< (point) (mark t))
      (exchange-point-and-mark))
  (if (eq (preceding-char) ?\n)
      (viper-backward-char-carefully)) ; give back the newline
  (if (eq viper-intermediate-command 'viper-repeat)
      (viper-change-subr (mark t) (point))
    (viper-change (mark t) (point))))

;; this is invoked by viper-substitute-line
(defun viper-exec-Change (m-com com)
  (save-excursion
    (set-mark viper-com-point)
    (viper-enlarge-region (mark t) (point))
    (if viper-use-register
	(progn
	  (cond ((viper-valid-register viper-use-register '(letter digit))
		 (copy-to-register
		  viper-use-register (mark t) (point) nil))
		((viper-valid-register viper-use-register '(Letter))
		 (viper-append-to-register
		  (downcase viper-use-register) (mark t) (point)))
		(t (setq viper-use-register nil)
		   (error viper-InvalidRegister viper-use-register)))
	  (setq viper-use-register nil)))
    (delete-region (mark t) (point)))
  (open-line 1)
  (if (eq viper-intermediate-command 'viper-repeat)
      (viper-yank-last-insertion)
    (viper-change-state-to-insert)
    ))

(defun viper-exec-delete (m-com com)
  (or (and (markerp viper-com-point) (marker-position viper-com-point))
      (set-marker viper-com-point (point) (current-buffer)))
  (let (chars-deleted)
    (if viper-use-register
	(progn
	  (cond ((viper-valid-register viper-use-register '(letter digit))
		 (copy-to-register
		  viper-use-register viper-com-point (point) nil))
		((viper-valid-register viper-use-register '(Letter))
		 (viper-append-to-register
		  (downcase viper-use-register) viper-com-point (point)))
		(t (setq viper-use-register nil)
		 (error viper-InvalidRegister viper-use-register)))
	  (setq viper-use-register nil)))
    (setq last-command
	  (if (eq last-command 'd-command) 'kill-region nil))
    (setq chars-deleted (abs (- (point) viper-com-point)))
    (if (> chars-deleted viper-change-notification-threshold)
	(unless (viper-is-in-minibuffer)
	  (message "Deleted %d characters" chars-deleted)))
    (kill-region viper-com-point (point))
    (setq this-command 'd-command)
    (if viper-ex-style-motion
	(if (and (eolp) (not (bolp))) (backward-char 1)))))

(defun viper-exec-Delete (m-com com)
  (save-excursion
    (set-mark viper-com-point)
    (viper-enlarge-region (mark t) (point))
    (let (lines-deleted)
      (if viper-use-register
	  (progn
	    (cond ((viper-valid-register viper-use-register '(letter digit))
		   (copy-to-register
		    viper-use-register (mark t) (point) nil))
		  ((viper-valid-register viper-use-register '(Letter))
		   (viper-append-to-register
		    (downcase viper-use-register) (mark t) (point)))
		  (t (setq viper-use-register nil)
		     (error viper-InvalidRegister viper-use-register)))
	    (setq viper-use-register nil)))
      (setq last-command
	    (if (eq last-command 'D-command) 'kill-region nil))
      (setq lines-deleted (count-lines (point) viper-com-point))
      (if (> lines-deleted viper-change-notification-threshold)
	  (unless (viper-is-in-minibuffer)
	    (message "Deleted %d lines" lines-deleted)))
      (kill-region (mark t) (point))
      (if (eq m-com 'viper-line) (setq this-command 'D-command)))
    (back-to-indentation)))

;; save region
(defun viper-exec-yank (m-com com)
  (or (and (markerp viper-com-point) (marker-position viper-com-point))
      (set-marker viper-com-point (point) (current-buffer)))
  (let (chars-saved)
    (if viper-use-register
	(progn
	  (cond ((viper-valid-register viper-use-register '(letter digit))
		 (copy-to-register
		  viper-use-register viper-com-point (point) nil))
		((viper-valid-register viper-use-register '(Letter))
		 (viper-append-to-register
		  (downcase viper-use-register) viper-com-point (point)))
		(t (setq viper-use-register nil)
		   (error viper-InvalidRegister viper-use-register)))
	  (setq viper-use-register nil)))
    (setq last-command nil)
    (copy-region-as-kill viper-com-point (point))
    (setq chars-saved (abs (- (point) viper-com-point)))
    (if (> chars-saved viper-change-notification-threshold)
	(unless (viper-is-in-minibuffer)
	  (message "Saved %d characters" chars-saved)))
    (goto-char viper-com-point)))

;; save lines
(defun viper-exec-Yank (m-com com)
  (save-excursion
    (set-mark viper-com-point)
    (viper-enlarge-region (mark t) (point))
    (let (lines-saved)
      (if viper-use-register
	  (progn
	    (cond ((viper-valid-register viper-use-register '(letter digit))
		   (copy-to-register
		    viper-use-register (mark t) (point) nil))
		  ((viper-valid-register viper-use-register '(Letter))
		   (viper-append-to-register
		    (downcase viper-use-register) (mark t) (point)))
		  (t (setq viper-use-register nil)
		     (error viper-InvalidRegister  viper-use-register)))
	    (setq viper-use-register nil)))
      (setq last-command nil)
      (copy-region-as-kill (mark t) (point))
      (setq lines-saved (count-lines (mark t) (point)))
      (if (> lines-saved viper-change-notification-threshold)
	  (unless (viper-is-in-minibuffer)
	    (message "Saved %d lines" lines-saved)))))
  (viper-deactivate-mark)
  (goto-char viper-com-point))

(defun viper-exec-bang (m-com com)
  (save-excursion
    (set-mark viper-com-point)
    (viper-enlarge-region (mark t) (point))
    (exchange-point-and-mark)
    (shell-command-on-region
     (mark t) (point)
     (if (viper= com ?!)
	 (setq viper-last-shell-com
	       (viper-read-string-with-history
		"!"
		nil
		'viper-shell-history
		(car viper-shell-history)
		))
       viper-last-shell-com)
     t)))

(defun viper-exec-equals (m-com com)
  (save-excursion
    (set-mark viper-com-point)
    (viper-enlarge-region (mark t) (point))
    (if (> (mark t) (point)) (exchange-point-and-mark))
    (indent-region (mark t) (point) nil)))

(defun viper-exec-shift (m-com com)
  (save-excursion
    (set-mark viper-com-point)
    (viper-enlarge-region (mark t) (point))
    (if (> (mark t) (point)) (exchange-point-and-mark))
    (indent-rigidly (mark t) (point)
		    (if (viper= com ?>)
			viper-shift-width
		      (- viper-shift-width))))
  ;; return point to where it was before shift
  (goto-char viper-com-point))

;; this is needed because some commands fake com by setting it to ?r, which
;; denotes repeated insert command.
(defsubst viper-exec-dummy (m-com com)
  nil)

(defun viper-exec-buffer-search (m-com com)
  (setq viper-s-string
	(regexp-quote (buffer-substring (point) viper-com-point)))
  (setq viper-s-forward t)
  (setq viper-search-history (cons viper-s-string viper-search-history))
  (setq viper-intermediate-command 'viper-exec-buffer-search)
  (viper-search viper-s-string viper-s-forward 1))

(defvar viper-exec-array (make-vector 128 nil))

;; Using a dispatch array allows adding functions like buffer search
;; without affecting other functions.  Buffer search can now be bound
;; to any character.

(aset viper-exec-array ?c 'viper-exec-change)
(aset viper-exec-array ?C 'viper-exec-Change)
(aset viper-exec-array ?d 'viper-exec-delete)
(aset viper-exec-array ?D 'viper-exec-Delete)
(aset viper-exec-array ?y 'viper-exec-yank)
(aset viper-exec-array ?Y 'viper-exec-Yank)
(aset viper-exec-array ?r 'viper-exec-dummy)
(aset viper-exec-array ?! 'viper-exec-bang)
(aset viper-exec-array ?< 'viper-exec-shift)
(aset viper-exec-array ?> 'viper-exec-shift)
(aset viper-exec-array ?= 'viper-exec-equals)



;; This function is called by various movement commands to execute a
;; destructive command on the region specified by the movement command.  For
;; instance, if the user types cw, then the command viper-forward-word will
;; call viper-execute-com to execute viper-exec-change, which eventually will
;; call viper-change to invoke the replace mode on the region.
;;
;; The var viper-d-com is set to (M-COM VAL COM REG INSERTED-TEXT COMMAND-KEYS)
;; via a call to viper-set-destructive-command, for later use by viper-repeat.
(defun viper-execute-com (m-com val com)
  (let ((reg viper-use-register))
    ;; this is the special command `#'
    (if (> com 128)
	(viper-special-prefix-com (- com 128))
      (let ((fn (aref viper-exec-array com)))
	(if (null fn)
	    (error "%c: %s" com viper-InvalidViCommand)
	  (funcall fn m-com com))))
    (if (viper-dotable-command-p com)
	(viper-set-destructive-command
	 (list m-com val com reg nil nil)))
    ))


(defun viper-repeat (arg)
  "Re-execute last destructive command.
Use the info in viper-d-com, which has the form
\(com val ch reg inserted-text command-keys\),
where `com' is the command to be re-executed, `val' is the
argument to `com', `ch' is a flag for repeat, and `reg' is optional;
if it exists, it is the name of the register for `com'.
If the prefix argument ARG is non-nil, it is used instead of `val'."
  (interactive "P")
  (let ((save-point (point)) ; save point before repeating prev cmd
	;; Pass along that we are repeating a destructive command
	;; This tells viper-set-destructive-command not to update
	;; viper-command-ring
	(viper-intermediate-command 'viper-repeat))
    (if (eq last-command 'viper-undo)
	;; if the last command was viper-undo, then undo-more
	(viper-undo-more)
      ;; otherwise execute the command stored in viper-d-com.  if arg is
      ;; non-nil its prefix value is used as new prefix value for the command.
      (let ((m-com (car viper-d-com))
	    (val (viper-P-val arg))
	    (com (nth 2 viper-d-com))
	    (reg (nth 3 viper-d-com)))
        (if (null val) (setq val (nth 1 viper-d-com)))
        (if (null m-com) (error "No previous command to repeat"))
        (setq viper-use-register reg)
	(if (nth 4 viper-d-com) ; text inserted by command
	    (setq viper-last-insertion (nth 4 viper-d-com)
		  viper-d-char (nth 4 viper-d-com)))
        (funcall m-com (cons val com))
        (cond ((and (< save-point (point)) viper-keep-point-on-repeat)
	       (goto-char save-point)) ; go back to before repeat.
	      ((and (< save-point (point)) viper-ex-style-editing)
	       (or (bolp) (backward-char 1))))
	(if (and (eolp) (not (bolp)))
	    (backward-char 1))
     ))
  (viper-adjust-undo) ; take care of undo
  ;; If the prev cmd was rotating the command ring, this means that `.' has
  ;; just executed a command from that ring.  So, push it on the ring again.
  ;; If we are just executing previous command , then don't push viper-d-com
  ;; because viper-d-com is not fully constructed in this case (its keys and
  ;; the inserted text may be nil).  Besides, in this case, the command
  ;; executed by `.' is already on the ring.
  (if (eq last-command 'viper-display-current-destructive-command)
      (viper-push-onto-ring viper-d-com 'viper-command-ring))
  (viper-deactivate-mark)
  ))

(defun viper-repeat-from-history ()
  "Repeat a destructive command from history.
Doesn't change viper-command-ring in any way, so `.' will work as before
executing this command.
This command is supposed to be bound to a two-character Vi macro where
the second character is a digit 0 to 9.  The digit indicates which
history command to execute. `<char>0' is equivalent to `.', `<char>1'
invokes the command before that, etc."
  (interactive)
  (let* ((viper-intermediate-command 'repeating-display-destructive-command)
	 (idx (cond (viper-this-kbd-macro
		      (string-to-number
		       (symbol-name (elt viper-this-kbd-macro 1))))
		    (t 0)))
	 (num idx)
	 (viper-d-com viper-d-com))

    (or (and (numberp num) (<= 0 num) (<= num 9))
	(progn
	  (setq idx 0
		num 0)
	  (message
	   "`viper-repeat-from-history' must be invoked as a Vi macro bound to `<key><digit>'")))
    (while (< 0 num)
      (setq viper-d-com (viper-special-ring-rotate1 viper-command-ring -1))
      (setq num (1- num)))
    (viper-repeat nil)
    (while (> idx num)
      (viper-special-ring-rotate1 viper-command-ring 1)
      (setq num (1+ num)))
    ))


;; The hash-command.  It is invoked interactively by the key sequence #<char>.
;; The chars that can follow `#' are determined by viper-hash-command-p
(defun viper-special-prefix-com (char)
  (cond ((viper= char ?c)
	 (downcase-region (min viper-com-point (point))
			  (max viper-com-point (point))))
	((viper= char ?C)
	 (upcase-region (min viper-com-point (point))
			(max viper-com-point (point))))
	((viper= char ?g)
	 (push-mark viper-com-point t)
	 ;; execute the last emacs kbd macro on each line of the region
	 (viper-global-execute))
	((viper= char ?q)
	 (push-mark viper-com-point t)
	 (viper-quote-region))
	((viper= char ?s)
	 (funcall viper-spell-function viper-com-point (point)))
	(t (error "#%c: %s" char viper-InvalidViCommand))))


;; undoing

;; hook used inside undo
(defvar viper-undo-functions nil)

;; Runs viper-before-change-functions inside before-change-functions
(defun viper-undo-sentinel (beg end length)
  (run-hook-with-args 'viper-undo-functions beg end length))

(add-hook 'after-change-functions 'viper-undo-sentinel)

;; Hook used in viper-undo
(defun viper-after-change-undo-hook (beg end len)
  (if (and (boundp 'undo-in-progress) undo-in-progress)
      (setq undo-beg-posn beg
	    undo-end-posn (or end beg))
    ;; some other hooks may be changing various text properties in
    ;; the buffer in response to 'undo'; so remove this hook to avoid
    ;; its repeated invocation
    (remove-hook 'viper-undo-functions 'viper-after-change-undo-hook 'local)
  ))

(defun viper-undo ()
  "Undo previous change."
  (interactive)
  (message "undo!")
  (let ((modified (buffer-modified-p))
        (before-undo-pt (point-marker))
	undo-beg-posn undo-end-posn)

    ;; the viper-after-change-undo-hook removes itself after the 1st invocation
    (add-hook 'viper-undo-functions 'viper-after-change-undo-hook nil 'local)

    (undo-start)
    (undo-more 2)
    ;;(setq undo-beg-posn (or undo-beg-posn (point))
    ;;    undo-end-posn (or undo-end-posn (point)))
    ;;(setq undo-beg-posn (or undo-beg-posn before-undo-pt)
    ;;      undo-end-posn (or undo-end-posn undo-beg-posn))

    (if (and undo-beg-posn undo-end-posn)
	(progn
	  (goto-char undo-beg-posn)
	  (sit-for 0)
	  (if (and viper-keep-point-on-undo
		   (pos-visible-in-window-p before-undo-pt))
	      (progn
		(push-mark (point-marker) t)
		(viper-sit-for-short 300)
		(goto-char undo-end-posn)
		(viper-sit-for-short 300)
		(if (pos-visible-in-window-p undo-beg-posn)
		    (goto-char before-undo-pt)
		  (goto-char undo-beg-posn)))
	    (push-mark before-undo-pt t))
	  ))

    (if (and (eolp) (not (bolp))) (backward-char 1))
    )
  (setq this-command 'viper-undo))

;; Continue undoing previous changes.
(defun viper-undo-more ()
  (message "undo more!")
  (condition-case nil
      (undo-more 1)
    (error (beep)
	   (message "No further undo information in this buffer")))
  (if (and (eolp) (not (bolp))) (backward-char 1))
  (setq this-command 'viper-undo))

;; The following two functions are used to set up undo properly.
;; In VI, unlike Emacs, if you open a line, say, and add a bunch of lines,
;; they are undone all at once.
(defun viper-adjust-undo ()
  (if viper-undo-needs-adjustment
      (let ((inhibit-quit t)
	    tmp tmp2)
	(setq viper-undo-needs-adjustment nil)
	(if (listp buffer-undo-list)
	    (if (setq tmp (memq viper-buffer-undo-list-mark buffer-undo-list))
		(progn
		  (setq tmp2 (cdr tmp)) ; the part after mark

		  ;; cut tail from buffer-undo-list temporarily by direct
		  ;; manipulation with pointers in buffer-undo-list
		  (setcdr tmp nil)

		  (setq buffer-undo-list (delq nil buffer-undo-list))
		  (setq buffer-undo-list
			(delq viper-buffer-undo-list-mark buffer-undo-list))
		  ;; restore tail of buffer-undo-list
		  (setq buffer-undo-list (nconc buffer-undo-list tmp2)))
	      (setq buffer-undo-list (delq nil buffer-undo-list)))))
    ))


(defun viper-set-complex-command-for-undo ()
  (if (listp buffer-undo-list)
      (if (not viper-undo-needs-adjustment)
	  (let ((inhibit-quit t))
	    (setq buffer-undo-list
		  (cons viper-buffer-undo-list-mark buffer-undo-list))
	    (setq viper-undo-needs-adjustment t)))))


;;; Viper's destructive Command ring utilities

(defun viper-display-current-destructive-command ()
  (let ((text (nth 4 viper-d-com))
	(keys (nth 5 viper-d-com))
	(max-text-len 30))

    (setq this-command 'viper-display-current-destructive-command)

    (message " `.' runs  %s%s"
	     (concat "`" (viper-array-to-string keys) "'")
	     (viper-abbreviate-string
	      (if (featurep 'xemacs)
		  (replace-in-string ; xemacs
		   (cond ((characterp text) (char-to-string text))
			 ((stringp text) text)
			 (t ""))
		   "\n" "^J")
		text ; emacs
		)
	      max-text-len
	      "  inserting  `" "'" "    ......."))
    ))


;; don't change viper-d-com if it was viper-repeat command invoked with `.'
;; or in some other way (non-interactively).
(defun viper-set-destructive-command (list)
  (or (eq viper-intermediate-command 'viper-repeat)
      (progn
	(setq viper-d-com list)
	(setcar (nthcdr 5 viper-d-com)
		(viper-array-to-string (if (arrayp viper-this-command-keys)
					   viper-this-command-keys
					 (this-command-keys))))
	(viper-push-onto-ring viper-d-com 'viper-command-ring)))
  (setq viper-this-command-keys nil))


(defun viper-prev-destructive-command (next)
  "Find previous destructive command in the history of destructive commands.
With prefix argument, find next destructive command."
  (interactive "P")
  (let (cmd viper-intermediate-command)
    (if (eq last-command 'viper-display-current-destructive-command)
	;; repeated search through command history
	(setq viper-intermediate-command
	      'repeating-display-destructive-command)
      ;; first search through command history--set temp ring
      (setq viper-temp-command-ring (ring-copy viper-command-ring)))
    (setq cmd (if next
		  (viper-special-ring-rotate1 viper-temp-command-ring 1)
		(viper-special-ring-rotate1 viper-temp-command-ring -1)))
    (if (null cmd)
	()
      (setq viper-d-com cmd))
    (viper-display-current-destructive-command)))


(defun viper-next-destructive-command ()
  "Find next destructive command in the history of destructive commands."
  (interactive)
  (viper-prev-destructive-command 'next))


(defun viper-insert-prev-from-insertion-ring (arg)
  "Cycle through insertion ring in the direction of older insertions.
Undoes previous insertion and inserts new.
With prefix argument, cycles in the direction of newer elements.
In minibuffer, this command executes whatever the invocation key is bound
to in the global map, instead of cycling through the insertion ring."
  (interactive "P")
  (let (viper-intermediate-command)
    (if (eq last-command 'viper-insert-from-insertion-ring)
	(progn  ; repeated search through insertion history
	  (setq viper-intermediate-command 'repeating-insertion-from-ring)
	  (if (eq viper-current-state 'replace-state)
	      (undo 1)
	    (if viper-last-inserted-string-from-insertion-ring
		(backward-delete-char
		 (length viper-last-inserted-string-from-insertion-ring))))
	  )
      ;;first search through insertion history
      (setq viper-temp-insertion-ring (ring-copy viper-insertion-ring)))
    (setq this-command 'viper-insert-from-insertion-ring)
    ;; so that things will be undone properly
    (setq buffer-undo-list (cons nil buffer-undo-list))
    (setq viper-last-inserted-string-from-insertion-ring
	  (viper-special-ring-rotate1 viper-temp-insertion-ring (if arg 1 -1)))

    ;; this change of viper-intermediate-command must come after
    ;; viper-special-ring-rotate1, so that the ring will rotate, but before the
    ;; insertion.
    (setq viper-intermediate-command nil)
    (if viper-last-inserted-string-from-insertion-ring
	(insert viper-last-inserted-string-from-insertion-ring))
    ))

(defun viper-insert-next-from-insertion-ring ()
  "Cycle through insertion ring in the direction of older insertions.
Undo previous insertion and inserts new."
  (interactive)
  (viper-insert-prev-from-insertion-ring 'next))



;; some region utilities

;; If at the last line of buffer, add \\n before eob, if newline is missing.
(defun viper-add-newline-at-eob-if-necessary ()
  (save-excursion
      (end-of-line)
      ;; make sure all lines end with newline, unless in the minibuffer or
      ;; when requested otherwise (require-final-newline is nil)
      (save-restriction
	(widen)
	(if (and (eobp)
		 (not (bolp))
		 require-final-newline
		 ;; add newline only if we actually edited buffer. otherwise it
		 ;; might unintentionally modify binary buffers
		 (buffer-modified-p)
		 (not (viper-is-in-minibuffer))
		 (not buffer-read-only))
	    ;; text property may be read-only
	    (condition-case nil
		(insert "\n")
	      (error nil))
	  ))
      ))

(defun viper-yank-defun ()
  (mark-defun)
  (copy-region-as-kill (point) (mark t)))

;; Enlarge region between BEG and END.
(defun viper-enlarge-region (beg end)
  (or beg (setq beg end)) ; if beg is nil, set to end
  (or end (setq end beg)) ; if end is nil, set to beg

  (if (< beg end)
      (progn (goto-char beg) (set-mark end))
    (goto-char end)
    (set-mark beg))
  (beginning-of-line)
  (exchange-point-and-mark)
  (if (or (not (eobp)) (not (bolp))) (forward-line 1))
  (if (not (eobp)) (beginning-of-line))
  (if (> beg end) (exchange-point-and-mark)))


;; Quote region by each line with a user supplied string.
(defun viper-quote-region ()
  (let ((quote-str viper-quote-string)
	(donot-change-default t))
    (setq quote-str
	  (viper-read-string-with-history
	   "Quote string: "
	   nil
	   'viper-quote-region-history
	   (cond ((string-match "tex.*-mode" (symbol-name major-mode)) "%%")
		 ((string-match "java.*-mode" (symbol-name major-mode)) "//")
		 ((string-match "perl.*-mode" (symbol-name major-mode)) "#")
		 ((string-match "lisp.*-mode" (symbol-name major-mode)) ";;")
		 ((memq major-mode '(c-mode cc-mode c++-mode)) "//")
		 ((memq major-mode '(sh-mode shell-mode)) "#")
		 (t (setq donot-change-default nil)
		    quote-str))))
    (or donot-change-default
	(setq viper-quote-string quote-str))
    (viper-enlarge-region (point) (mark t))
    (if (> (point) (mark t)) (exchange-point-and-mark))
    (insert quote-str)
    (beginning-of-line)
    (forward-line 1)
    (while (and (< (point) (mark t)) (bolp))
      (insert quote-str)
      (beginning-of-line)
      (forward-line 1))))

;;  Tells whether BEG is on the same line as END.
;;  If one of the args is nil, it'll return nil.
(defun viper-same-line (beg end)
   (let ((selective-display nil)
	 (incr 0)
	 temp)
     (if (and beg end (> beg end))
	 (setq temp beg
	       beg end
	       end temp))
     (if (and beg end)
	 (cond ((or (> beg (point-max)) (> end (point-max))) ; out of range
		nil)
	       (t
		;; This 'if' is needed because Emacs treats the next empty line
		;; as part of the previous line.
		(if (= (viper-line-pos 'start) end)
		    (setq incr 1))
		(<= (+ incr (count-lines beg end)) 1))))
     ))


;; Check if the string ends with a newline.
(defun viper-end-with-a-newline-p (string)
  (or (string= string "")
      (= (viper-seq-last-elt string) ?\n)))

(defun viper-tmp-insert-at-eob (msg)
  (let ((savemax (point-max)))
      (goto-char savemax)
      (insert msg)
      (sit-for 2)
      (goto-char savemax) (delete-region (point) (point-max))
      ))



;;; Minibuffer business

(defsubst viper-set-minibuffer-style ()
  (add-hook 'minibuffer-setup-hook 'viper-minibuffer-setup-sentinel)
  (add-hook 'post-command-hook 'viper-minibuffer-post-command-hook))


(defun viper-minibuffer-setup-sentinel ()
  (let ((hook (if viper-vi-style-in-minibuffer
		  'viper-change-state-to-insert
		'viper-change-state-to-emacs)))
    ;; making buffer-local variables so that normal buffers won't affect the
    ;; minibuffer and vice versa. Otherwise, command arguments will affect
    ;; minibuffer ops and insertions from the minibuffer will change those in
    ;; the normal buffers
    (make-local-variable 'viper-d-com)
    (make-local-variable 'viper-last-insertion)
    (make-local-variable 'viper-command-ring)
    (setq viper-d-com nil
	  viper-last-insertion nil
	  viper-command-ring nil)
    (funcall hook)
    ))

;; This is a temp hook that uses free variables init-message and viper-initial.
;; A dirty feature, but it is the simplest way to have it do the right thing.
;; The INIT-MESSAGE and VIPER-INITIAL vars come from the scope set by
;; viper-read-string-with-history
(defun viper-minibuffer-standard-hook ()
  (if (stringp init-message)
      (viper-tmp-insert-at-eob init-message))
  (when (stringp viper-initial)
    ;; don't wait if we have unread events or in kbd macro
    (or unread-command-events
	executing-kbd-macro
	(sit-for 840))
    (if (fboundp 'minibuffer-prompt-end)
	(delete-region (minibuffer-prompt-end) (point-max))
      (erase-buffer))
    (insert viper-initial)))

(defsubst viper-minibuffer-real-start ()
  (if (fboundp 'minibuffer-prompt-end)
      (minibuffer-prompt-end)
    (point-min)))

(defun viper-minibuffer-post-command-hook()
  (when (active-minibuffer-window)
    (when (< (point) (viper-minibuffer-real-start))
      (goto-char (viper-minibuffer-real-start)))))


;; Interpret last event in the local map first; if fails, use exit-minibuffer.
;; Run viper-minibuffer-exit-hook before exiting.
(defun viper-exit-minibuffer ()
  "Exit minibuffer Viper way."
  (interactive)
  (let (command)
    (setq command (local-key-binding (char-to-string (viper-last-command-char))))
    (run-hooks 'viper-minibuffer-exit-hook)
    (if command
	(command-execute command)
      (exit-minibuffer))))


(defcustom viper-smart-suffix-list
  '("" "tex" "c" "cc" "C" "java" "el" "html" "htm" "xml"
    "pl" "flr" "P" "p" "h" "H")
  "*List of suffixes that Viper tries to append to filenames ending with a `.'.
This is useful when the current directory contains files with the same
prefix and many different suffixes.  Usually, only one of the suffixes
represents an editable file.  However, file completion will stop at the `.'
The smart suffix feature lets you hit RET in such a case, and Viper will
select the appropriate suffix.

Suffixes are tried in the order given and the first suffix for which a
corresponding file exists is selected.  If no file exists for any of the
suffixes, the user is asked to confirm.

To turn this feature off, set this variable to nil."
  :type '(repeat string)
  :group 'viper-misc)


;; Try to add a suitable suffix to files whose name ends with a `.'
;; Useful when the user hits RET on a non-completed file name.
;; Used as a minibuffer exit hook in read-file-name
(defun viper-file-add-suffix ()
  (let ((count 0)
	(len (length viper-smart-suffix-list))
	(file (buffer-substring-no-properties
	       (viper-minibuffer-real-start) (point-max)))
	found key cmd suff)
    (goto-char (point-max))
    (if (and viper-smart-suffix-list (string-match "\\.$" file))
	(progn
	  (while (and (not found) (< count len))
	    (setq suff (nth count viper-smart-suffix-list)
		  count (1+ count))
	    (if (file-exists-p
		 (format "%s%s" (substitute-in-file-name file) suff))
		(progn
		  (setq found t)
		  (insert suff))))

	  (if found
	      ()
	    (viper-tmp-insert-at-eob " [Please complete file name]")
	    (unwind-protect
		(while (not (memq cmd
				  '(exit-minibuffer viper-exit-minibuffer)))
		  (setq cmd
			(key-binding (setq key (read-key-sequence nil))))
		  (cond ((eq cmd 'self-insert-command)
			 (if (featurep 'xemacs)
			     (insert (events-to-keys key)) ; xemacs
			   (insert key) ; emacs
			   ))
			((memq cmd '(exit-minibuffer viper-exit-minibuffer))
			 nil)
			(t (command-execute cmd)))
		  )))
	  ))))


(defun viper-minibuffer-trim-tail ()
  "Delete junk at the end of the first line of the minibuffer input.
Remove this function from `viper-minibuffer-exit-hook', if this causes
problems."
  (if (viper-is-in-minibuffer)
      (let ((inhibit-field-text-motion t))
	(goto-char (viper-minibuffer-real-start))
	(end-of-line)
	(delete-region (point) (point-max)))))


;;; Reading string with history

(defun viper-read-string-with-history (prompt &optional viper-initial
					      history-var default keymap
					      init-message)
  ;; Read string, prompting with PROMPT and inserting the VIPER-INITIAL
  ;; value.  Uses HISTORY-VAR.  DEFAULT is the default value to accept if the
  ;; input is an empty string.
  ;; Default value is displayed until the user types something in the
  ;; minibuffer.
  ;; KEYMAP is used, if given, instead of minibuffer-local-map.
  ;; INIT-MESSAGE is the message temporarily displayed after entering the
  ;; minibuffer.
  (let ((minibuffer-setup-hook
	 ;; stolen from add-hook
	 (let ((old
		(if (boundp 'minibuffer-setup-hook)
		    minibuffer-setup-hook
		  nil)))
	   (cons
	    'viper-minibuffer-standard-hook
	    (if (or (not (listp old)) (eq (car old) 'lambda))
		(list old) old))))
	(val "")
	(padding "")
	temp-msg)

    (setq keymap (or keymap minibuffer-local-map)
	  viper-initial (or viper-initial "")
	  temp-msg (if default
		       (format "(default %s) " default)
		     ""))

    (setq viper-incomplete-ex-cmd nil)
    (setq val (read-from-minibuffer prompt
				    (concat temp-msg viper-initial val padding)
				    keymap nil history-var))
    (setq minibuffer-setup-hook nil
	  padding (viper-array-to-string (this-command-keys))
	  temp-msg "")
    ;; the following tries to be smart about what to put in history
    (if (not (string= val (car (eval history-var))))
	(set history-var (cons val (eval history-var))))
    (if (or (string= (nth 0 (eval history-var)) (nth 1 (eval history-var)))
	    (string= (nth 0 (eval history-var)) ""))
	(set history-var (cdr (eval history-var))))
    ;; If the user enters nothing but the prev cmd wasn't viper-ex,
    ;; viper-command-argument, or `! shell-command', this probably means
    ;; that the user typed something then erased.  Return "" in this case, not
    ;; the default---the default is too confusing in this case.
    (cond ((and (string= val "")
		(not (string= prompt "!")) ; was a `! shell-command'
		(not (memq last-command
			   '(viper-ex
			     viper-command-argument
			     t)
			   )))
	   "")
	  ((string= val "") (or default ""))
	  (t val))
    ))



;; insertion commands

;; Called when state changes from Insert Vi command mode.
;; Repeats the insertion command if Insert state was entered with prefix
;; argument > 1.
(defun viper-repeat-insert-command ()
  (let ((i-com (car viper-d-com))
	(val   (nth 1 viper-d-com))
	(char  (nth 2 viper-d-com)))
    (if (and val (> val 1)) ; first check that val is non-nil
	(progn
	  (setq viper-d-com (list i-com (1- val) ?r nil nil nil))
	  (viper-repeat nil)
	  (setq viper-d-com (list i-com val char nil nil nil))
	  ))))

(defun viper-insert (arg)
  "Insert before point."
  (interactive "P")
  (viper-set-complex-command-for-undo)
  (let ((val (viper-p-val arg))
	;;(com (viper-getcom arg))
	)
    (viper-set-destructive-command (list 'viper-insert val ?r nil nil nil))
    (if (eq viper-intermediate-command 'viper-repeat)
	(viper-loop val (viper-yank-last-insertion))
      (viper-change-state-to-insert))))

(defun viper-append (arg)
  "Append after point."
  (interactive "P")
  (viper-set-complex-command-for-undo)
  (let ((val (viper-p-val arg))
	;;(com (viper-getcom arg))
	)
    (viper-set-destructive-command (list 'viper-append val ?r nil nil nil))
    (if (not (eolp)) (forward-char))
    (if (eq viper-intermediate-command 'viper-repeat)
	(viper-loop val (viper-yank-last-insertion))
      (viper-change-state-to-insert))))

(defun viper-Append (arg)
  "Append at end of line."
  (interactive "P")
  (viper-set-complex-command-for-undo)
  (let ((val (viper-p-val arg))
	;;(com (viper-getcom arg))
	)
    (viper-set-destructive-command (list 'viper-Append val ?r nil nil nil))
    (end-of-line)
    (if (eq viper-intermediate-command 'viper-repeat)
	(viper-loop val (viper-yank-last-insertion))
      (viper-change-state-to-insert))))

(defun viper-Insert (arg)
  "Insert before first non-white."
  (interactive "P")
  (viper-set-complex-command-for-undo)
  (let ((val (viper-p-val arg))
	;;(com (viper-getcom arg))
	)
    (viper-set-destructive-command (list 'viper-Insert val ?r nil nil nil))
    (back-to-indentation)
    (if (eq viper-intermediate-command 'viper-repeat)
	(viper-loop val (viper-yank-last-insertion))
      (viper-change-state-to-insert))))

(defun viper-open-line (arg)
  "Open line below."
  (interactive "P")
  (viper-set-complex-command-for-undo)
  (let ((val (viper-p-val arg))
	;;(com (viper-getcom arg))
	)
    (viper-set-destructive-command (list 'viper-open-line val ?r nil nil nil))
    (let ((col (current-indentation)))
      (if (eq viper-intermediate-command 'viper-repeat)
	  (viper-loop val
		      (end-of-line)
		      (newline 1)
		      (viper-indent-line col)
		      (viper-yank-last-insertion))
	(end-of-line)
	(newline 1)
	(viper-indent-line col)
	(viper-change-state-to-insert)))))

(defun viper-Open-line (arg)
  "Open line above."
  (interactive "P")
  (viper-set-complex-command-for-undo)
  (let ((val (viper-p-val arg))
	;;(com (viper-getcom arg))
	)
    (viper-set-destructive-command (list 'viper-Open-line val ?r nil nil nil))
    (let ((col (current-indentation)))
      (if (eq viper-intermediate-command 'viper-repeat)
	  (viper-loop val
		      (beginning-of-line)
		      (open-line 1)
		      (viper-indent-line col)
		      (viper-yank-last-insertion))
	(beginning-of-line)
	(open-line 1)
	(viper-indent-line col)
	(viper-change-state-to-insert)))))

(defun viper-open-line-at-point (arg)
  "Open line at point."
  (interactive "P")
  (viper-set-complex-command-for-undo)
  (let ((val (viper-p-val arg))
	;;(com (viper-getcom arg))
	)
    (viper-set-destructive-command
     (list 'viper-open-line-at-point val ?r nil nil nil))
    (if (eq viper-intermediate-command 'viper-repeat)
	(viper-loop val
		    (open-line 1)
		    (viper-yank-last-insertion))
      (open-line 1)
      (viper-change-state-to-insert))))

;; bound to s
(defun viper-substitute (arg)
  "Substitute characters."
  (interactive "P")
  (let ((val (viper-p-val arg))
	;;(com (viper-getcom arg))
	)
    (push-mark nil t)
    (forward-char val)
    (if (eq viper-intermediate-command 'viper-repeat)
	(viper-change-subr (mark t) (point))
      (viper-change (mark t) (point)))
    ;; com is set to ?r when we repeat this command with dot
    (viper-set-destructive-command (list 'viper-substitute val ?r nil nil nil))
    ))

;; Command bound to S
(defun viper-substitute-line (arg)
  "Substitute lines."
  (interactive "p")
  (viper-set-complex-command-for-undo)
  (viper-line (cons arg ?C)))

;; Prepare for replace
(defun viper-start-replace ()
  (setq viper-began-as-replace t
	viper-sitting-in-replace t
	viper-replace-chars-to-delete 0)
  (add-hook
   'viper-after-change-functions 'viper-replace-mode-spy-after t 'local)
  (add-hook
   'viper-before-change-functions 'viper-replace-mode-spy-before t 'local)
  ;; this will get added repeatedly, but no harm
  (add-hook 'after-change-functions 'viper-after-change-sentinel t)
  (add-hook 'before-change-functions 'viper-before-change-sentinel t)
  (viper-move-marker-locally
   'viper-last-posn-in-replace-region (viper-replace-start))
  (add-hook
   'viper-post-command-hooks 'viper-replace-state-post-command-sentinel
   t 'local)
  (add-hook
   'viper-pre-command-hooks 'viper-replace-state-pre-command-sentinel t 'local)
  ;; guard against a smarty who switched from R-replace to normal replace
  (remove-hook
   'viper-post-command-hooks 'viper-R-state-post-command-sentinel 'local)
  (if overwrite-mode (overwrite-mode -1))
  )


(defun viper-replace-mode-spy-before (beg end)
  (setq viper-replace-region-chars-deleted (viper-chars-in-region beg end))
  )

;; Invoked as an after-change-function to calculate how many chars have to be
;; deleted.  This function may be called several times within a single command,
;; if this command performs several separate buffer changes.  Therefore, if
;; adds up the number of chars inserted and subtracts the number of chars
;; deleted.
(defun viper-replace-mode-spy-after (beg end length)
  (if (memq viper-intermediate-command
	    '(dabbrev-expand hippie-expand repeating-insertion-from-ring))
      ;; Take special care of text insertion from insertion ring inside
      ;; replacement overlays.
      (progn
	(setq viper-replace-chars-to-delete 0)
	(viper-move-marker-locally
	 'viper-last-posn-in-replace-region (point)))

    (let* ((real-end (min end (viper-replace-end)))
	   (column-shift (- (save-excursion (goto-char real-end)
					    (current-column))
			    (save-excursion (goto-char beg)
					    (current-column))))
	   (chars-deleted 0))

      (if (> length 0)
	  (setq chars-deleted viper-replace-region-chars-deleted))
      (setq viper-replace-region-chars-deleted 0)
      (setq viper-replace-chars-to-delete
	    (+ viper-replace-chars-to-delete
	       (-
		;; if column shift is bigger, due to a TAB insertion, take
		;; column-shift instead of the number of inserted chars
		(max (viper-chars-in-region beg real-end)
		     ;; This test accounts for Chinese/Japanese/...  chars,
		     ;; which occupy 2 columns instead of one.  If we use
		     ;; column-shift here, we may delete two chars instead of
		     ;; one when the user types one Chinese character.
		     ;; Deleting two would be OK, if they were European chars,
		     ;; but it is not OK if they are Chinese chars.
		     ;; Since it is hard to
		     ;; figure out which characters are being deleted in any
		     ;; given region, we decided to treat Eastern and European
		     ;; characters equally, even though Eastern chars may
		     ;; occupy more columns.
		     (if (memq this-command '(self-insert-command
					      quoted-insert viper-insert-tab))
			 column-shift
		       0))
		;; the number of deleted chars
		chars-deleted)))

      (viper-move-marker-locally
       'viper-last-posn-in-replace-region
       (max (if (> end (viper-replace-end)) (viper-replace-end) end)
	    (or (marker-position viper-last-posn-in-replace-region)
		(viper-replace-start))
	    ))

      )))


;; Delete stuff between viper-last-posn-in-replace-region and the end of
;; viper-replace-overlay-marker, if viper-last-posn-in-replace-region is within
;; the overlay and current point is before the end of the overlay.
;; Don't delete anything if current point is past the end of the overlay.
(defun viper-finish-change ()
  (remove-hook
   'viper-after-change-functions 'viper-replace-mode-spy-after 'local)
  (remove-hook
   'viper-before-change-functions 'viper-replace-mode-spy-before 'local)
  (remove-hook
   'viper-post-command-hooks 'viper-replace-state-post-command-sentinel 'local)
  (remove-hook
   'viper-pre-command-hooks 'viper-replace-state-pre-command-sentinel 'local)
  (viper-restore-cursor-color 'after-replace-mode)
  (setq viper-sitting-in-replace nil) ; just in case we'll need to know it
  (save-excursion
    (if (and viper-replace-overlay
	     (viper-pos-within-region viper-last-posn-in-replace-region
				      (viper-replace-start)
				      (viper-replace-end))
	     (< (point) (viper-replace-end)))
	(delete-region
	 viper-last-posn-in-replace-region (viper-replace-end))))

  (if (eq viper-current-state 'replace-state)
      (viper-downgrade-to-insert))
  ;; replace mode ended => nullify viper-last-posn-in-replace-region
  (viper-move-marker-locally 'viper-last-posn-in-replace-region nil)
  (viper-hide-replace-overlay)
  (viper-refresh-mode-line)
  (viper-put-string-on-kill-ring viper-last-replace-region)
  )

;; Make STRING be the first element of the kill ring.
(defun viper-put-string-on-kill-ring (string)
  (setq kill-ring (cons string kill-ring))
  (if (> (length kill-ring) kill-ring-max)
      (setcdr (nthcdr (1- kill-ring-max) kill-ring) nil))
  (setq kill-ring-yank-pointer kill-ring))

(defun viper-finish-R-mode ()
  (remove-hook
   'viper-post-command-hooks 'viper-R-state-post-command-sentinel 'local)
  (remove-hook
   'viper-pre-command-hooks 'viper-replace-state-pre-command-sentinel 'local)
  (viper-downgrade-to-insert))

(defun viper-start-R-mode ()
  ;; Leave arg as 1, not t: XEmacs insists that it must be a pos number
  (overwrite-mode 1)
  (add-hook
   'viper-post-command-hooks 'viper-R-state-post-command-sentinel t 'local)
  (add-hook
   'viper-pre-command-hooks 'viper-replace-state-pre-command-sentinel t 'local)
  ;; guard against a smarty who switched from R-replace to normal replace
  (remove-hook
   'viper-post-command-hooks 'viper-replace-state-post-command-sentinel 'local)
  )



(defun viper-replace-state-exit-cmd ()
  "Binding for keys that cause Replace state to switch to Vi or to Insert.
These keys are ESC, RET, and LineFeed."
  (interactive)
  (if overwrite-mode   ; if in replace mode invoked via 'R'
      (viper-finish-R-mode)
    (viper-finish-change))
  (let (com)
    (if (eq this-command 'viper-intercept-ESC-key)
	(setq com 'viper-exit-insert-state)
      (viper-set-unread-command-events last-input-event)
      (setq com (key-binding (viper-read-key-sequence nil))))

    (condition-case conds
	(command-execute com)
      (error
       (viper-message-conditions conds)))
    )
  (viper-hide-replace-overlay))


(defun viper-replace-state-carriage-return ()
  "Carriage return in Viper replace state."
  (interactive)
  ;; If Emacs start supporting overlay maps, as it currently supports
  ;; text-property maps, we could do away with viper-replace-minor-mode and
  ;; just have keymap attached to replace overlay.  Then the "if part" of this
  ;; statement can be deleted.
  (if (or (< (point) (viper-replace-start))
	  (> (point) (viper-replace-end)))
      (let (viper-replace-minor-mode com)
	(viper-set-unread-command-events last-input-event)
	(setq com (key-binding (read-key-sequence nil)))
	(condition-case conds
	    (command-execute com)
	  (error
	   (viper-message-conditions conds))))
    (if (not viper-allow-multiline-replace-regions)
	(viper-replace-state-exit-cmd)
      (if (viper-same-line (point) (viper-replace-end))
	  (viper-replace-state-exit-cmd)
	;; delete the rest of line
	(delete-region (point) (viper-line-pos 'end))
	(save-excursion
	  (end-of-line)
	  (if (eobp) (error "Last line in buffer")))
	;; skip to the next line
	(forward-line 1)
	(back-to-indentation)
	))))


;; This is the function bound to 'R'---unlimited replace.
;; Similar to Emacs's own overwrite-mode.
(defun viper-overwrite (arg)
  "Begin overwrite mode."
  (interactive "P")
  (let ((val (viper-p-val arg))
	;;(com (viper-getcom arg))
	(len))
    (viper-set-destructive-command (list 'viper-overwrite val ?r nil nil nil))
    (if (eq viper-intermediate-command 'viper-repeat)
	(progn
	  ;; Viper saves inserted text in viper-last-insertion
	  (setq len (length viper-last-insertion))
	  (delete-char (min len (- (point-max) (point) 1)))
	  (viper-loop val (viper-yank-last-insertion)))
      (setq last-command 'viper-overwrite)
      (viper-set-complex-command-for-undo)
      (viper-set-replace-overlay (point) (viper-line-pos 'end))
      (viper-change-state-to-replace)
      )))


;; line commands

(defun viper-line (arg)
  (let ((val (car arg))
	(com (cdr arg)))
    (viper-move-marker-locally 'viper-com-point (point))
    (if (not (eobp))
	(viper-next-line-carefully (1- val)))
    ;; the following ensures that dd, cc, D, yy will do the right thing on the
    ;; last line of buffer when this line has no \n.
    (viper-add-newline-at-eob-if-necessary)
    (viper-execute-com 'viper-line val com))
  (if (and (eobp) (bolp) (not (bobp))) (forward-line -1))
  )

(defun viper-yank-line (arg)
  "Yank ARG lines (in Vi's sense)."
  (interactive "P")
  (let ((val (viper-p-val arg)))
    (viper-line (cons val ?Y))))


;; region commands

(defun viper-region (arg)
  "Execute command on a region."
  (interactive "P")
  (let ((val (viper-P-val arg))
	(com (viper-getcom arg)))
    (viper-move-marker-locally 'viper-com-point (point))
    (exchange-point-and-mark)
    (viper-execute-com 'viper-region val com)))

(defun viper-Region (arg)
  "Execute command on a Region."
  (interactive "P")
  (let ((val (viper-P-val arg))
	(com (viper-getCom arg)))
    (viper-move-marker-locally 'viper-com-point (point))
    (exchange-point-and-mark)
    (viper-execute-com 'viper-Region val com)))

(defun viper-replace-char (arg)
  "Replace the following ARG chars by the character read."
  (interactive "P")
  (if (and (eolp) (bolp)) (error "No character to replace here"))
  (let ((val (viper-p-val arg))
	(com (viper-getcom arg)))
    (viper-replace-char-subr com val)
    (if (and (eolp) (not (bolp))) (forward-char 1))
    (setq viper-this-command-keys
	  (format "%sr" (if (integerp arg) arg "")))
    (viper-set-destructive-command
     (list 'viper-replace-char val ?r nil viper-d-char nil))
  ))

(defun viper-replace-char-subr (com arg)
  (let ((inhibit-quit t)
	char)
    (viper-set-complex-command-for-undo)
    (or (eq viper-intermediate-command 'viper-repeat)
	(viper-special-read-and-insert-char))

    (delete-char 1 t)
    (setq char (if com viper-d-char (viper-char-at-pos 'backward)))

    (if com (insert char))

    (setq viper-d-char char)

    (viper-loop (1- (if (> arg 0) arg (- arg)))
		(delete-char 1 t)
		(insert char))

    (viper-adjust-undo)
    (backward-char arg)
    ))


;; basic cursor movement.  j, k, l, h commands.

(defun viper-forward-char (arg)
  "Move point right ARG characters (left if ARG negative).
On reaching end of line, stop and signal error."
  (interactive "P")
  (viper-leave-region-active)
  (let ((val (viper-p-val arg))
	(com (viper-getcom arg)))
    (if com (viper-move-marker-locally 'viper-com-point (point)))
    (if viper-ex-style-motion
	(progn
	  ;; the boundary condition check gets weird here because
	  ;; forward-char may be the parameter of a delete, and 'dl' works
	  ;; just like 'x' for the last char on a line, so we have to allow
	  ;; the forward motion before the 'viper-execute-com', but, of
	  ;; course, 'dl' doesn't work on an empty line, so we have to
	  ;; catch that condition before 'viper-execute-com'
	  (if (and (eolp) (bolp)) (error "Viper bell") (forward-char val))
	  (if com (viper-execute-com 'viper-forward-char val com))
	  (if (eolp) (progn (backward-char 1) (error "Viper bell"))))
      (forward-char val)
      (if com (viper-execute-com 'viper-forward-char val com)))))


(defun viper-backward-char (arg)
  "Move point left ARG characters (right if ARG negative).
On reaching beginning of line, stop and signal error."
  (interactive "P")
  (viper-leave-region-active)
  (let ((val (viper-p-val arg))
	(com (viper-getcom arg)))
    (if com (viper-move-marker-locally 'viper-com-point (point)))
    (if viper-ex-style-motion
	(progn
	  (if (bolp) (error "Viper bell") (backward-char val))
	  (if com (viper-execute-com 'viper-backward-char val com)))
      (backward-char val)
      (if com (viper-execute-com 'viper-backward-char val com)))))


;; Like forward-char, but doesn't move at end of buffer.
;; Returns distance traveled
;; (positive or 0, if arg positive; negative if arg negative).
(defun viper-forward-char-carefully (&optional arg)
  (setq arg (or arg 1))
  (let ((pt (point)))
    (condition-case nil
	(forward-char arg)
      (error nil))
    (if (< (point) pt) ; arg was negative
	(- (viper-chars-in-region pt (point)))
      (viper-chars-in-region pt (point)))))


;; Like backward-char, but doesn't move at beg of buffer.
;; Returns distance traveled
;; (negative or 0, if arg positive; positive if arg negative).
(defun viper-backward-char-carefully (&optional arg)
  (setq arg (or arg 1))
  (let ((pt (point)))
    (condition-case nil
	(backward-char arg)
      (error nil))
    (if (> (point) pt) ; arg was negative
	(viper-chars-in-region pt (point))
      (- (viper-chars-in-region pt (point))))))

(defun viper-next-line-carefully (arg)
  (condition-case nil
      ;; do not use forward-line! need to keep column
      (let ((line-move-visual nil))
	(if (featurep 'emacs)
	    (with-no-warnings (next-line arg))
	  (next-line arg)))
    (error nil)))



;;; Word command

;; Words are formed from alpha's and nonalphas - <sp>,\t\n are separators for
;; word movement.  When executed with a destructive command, \n is usually left
;; untouched for the last word.  Viper uses syntax table to determine what is a
;; word and what is a separator.  However, \n is always a separator.  Also, if
;; viper-syntax-preference is 'vi, then `_' is part of the word.

;; skip only one \n
(defun viper-skip-separators (forward)
  (if forward
      (progn
	(viper-skip-all-separators-forward 'within-line)
	(if (looking-at "\n")
	    (progn
	      (forward-char)
	      (viper-skip-all-separators-forward  'within-line))))
    ;; check for eob and white space before it.  move off of eob
    (if (and (eobp) (save-excursion
		      (viper-backward-char-carefully)
		      (viper-looking-at-separator)))
	(viper-backward-char-carefully))
    (viper-skip-all-separators-backward 'within-line)
    (viper-backward-char-carefully)
    (if (looking-at "\n")
	(viper-skip-all-separators-backward 'within-line)
      (or (viper-looking-at-separator) (forward-char)))))


(defun viper-forward-word-kernel (val)
  (while (> val 0)
    (cond ((viper-looking-at-alpha)
	   (viper-skip-alpha-forward "_")
	   (viper-skip-separators t))
	  ((viper-looking-at-separator)
	   (viper-skip-separators t))
	  ((not (viper-looking-at-alphasep))
	   (viper-skip-nonalphasep-forward)
	   (viper-skip-separators t)))
    (setq val (1- val))))

;; first skip non-newline separators backward, then skip \n.  Then, if TWICE is
;; non-nil, skip non-\n back again, but don't overshoot the limit LIM.
(defun viper-separator-skipback-special (twice lim)
  (let ((prev-char (viper-char-at-pos 'backward))
	(saved-point (point)))
    ;; skip non-newline separators backward
    (while (and (not (viper-memq-char prev-char '(nil \n)))
		(< lim (point))
		;; must be non-newline separator
		(if (eq viper-syntax-preference 'strict-vi)
		    (viper-memq-char prev-char '(?\  ?\t))
		  (viper-memq-char (char-syntax prev-char) '(?\  ?-))))
      (viper-backward-char-carefully)
      (setq prev-char (viper-char-at-pos 'backward)))

    (if (and (< lim (point)) (eq prev-char ?\n))
	(backward-char)
      ;; If we skipped to the next word and the prefix of this line doesn't
      ;; consist of separators preceded by a newline, then don't skip backwards
      ;; at all.
      (goto-char saved-point))
    (setq prev-char (viper-char-at-pos 'backward))

    ;; skip again, but make sure we don't overshoot the limit
    (if twice
	(while (and (not (viper-memq-char prev-char '(nil \n)))
		    (< lim (point))
		    ;; must be non-newline separator
		    (if (eq viper-syntax-preference 'strict-vi)
			(viper-memq-char prev-char '(?\  ?\t))
		      (viper-memq-char (char-syntax prev-char) '(?\  ?-))))
	  (viper-backward-char-carefully)
	  (setq prev-char (viper-char-at-pos 'backward))))

    (if (= (point) lim)
	(viper-forward-char-carefully))
    ))


(defun viper-forward-word (arg)
  "Forward word."
  (interactive "P")
  (viper-leave-region-active)
  (let ((val (viper-p-val arg))
	(com (viper-getcom arg)))
    (if com (viper-move-marker-locally 'viper-com-point (point)))
    (viper-forward-word-kernel val)
    (if com
	(progn
	  (cond ((viper-char-equal com ?c)
		 (viper-separator-skipback-special 'twice viper-com-point))
		;; Yank words including the whitespace, but not newline
		((viper-char-equal com ?y)
		 (viper-separator-skipback-special nil viper-com-point))
		((viper-dotable-command-p com)
		 (viper-separator-skipback-special nil viper-com-point)))
	  (viper-execute-com 'viper-forward-word val com)))
    ))


(defun viper-forward-Word (arg)
  "Forward word delimited by white characters."
  (interactive "P")
  (viper-leave-region-active)
  (let ((val (viper-p-val arg))
	(com (viper-getcom arg)))
    (if com (viper-move-marker-locally 'viper-com-point (point)))
    (viper-loop val
		(viper-skip-nonseparators 'forward)
		(viper-skip-separators t))
    (if com (progn
	      (cond ((viper-char-equal com ?c)
		     (viper-separator-skipback-special 'twice viper-com-point))
		    ;; Yank words including the whitespace, but not newline
		    ((viper-char-equal com ?y)
		     (viper-separator-skipback-special nil viper-com-point))
		    ((viper-dotable-command-p com)
		     (viper-separator-skipback-special nil viper-com-point)))
	      (viper-execute-com 'viper-forward-Word val com)))))


;; this is a bit different from Vi, but Vi's end of word
;; makes no sense whatsoever
(defun viper-end-of-word-kernel ()
  (if (viper-end-of-word-p) (forward-char))
  (if (viper-looking-at-separator)
      (viper-skip-all-separators-forward))

  (cond ((viper-looking-at-alpha) (viper-skip-alpha-forward "_"))
	((not (viper-looking-at-alphasep)) (viper-skip-nonalphasep-forward)))
  (viper-backward-char-carefully))

(defun viper-end-of-word-p ()
  (or (eobp)
      (save-excursion
	(cond ((viper-looking-at-alpha)
	       (forward-char)
	       (not (viper-looking-at-alpha)))
	      ((not (viper-looking-at-alphasep))
	       (forward-char)
	       (viper-looking-at-alphasep))))))


(defun viper-end-of-word (arg &optional careful)
  "Move point to end of current word."
  (interactive "P")
  (viper-leave-region-active)
  (let ((val (viper-p-val arg))
	(com (viper-getcom arg)))
    (if com (viper-move-marker-locally 'viper-com-point (point)))
    (viper-loop val (viper-end-of-word-kernel))
    (if com
	(progn
	  (forward-char)
	  (viper-execute-com 'viper-end-of-word val com)))))

(defun viper-end-of-Word (arg)
  "Forward to end of word delimited by white character."
  (interactive "P")
  (viper-leave-region-active)
  (let ((val (viper-p-val arg))
	(com (viper-getcom arg)))
    (if com (viper-move-marker-locally 'viper-com-point (point)))
    (viper-loop val
		(viper-end-of-word-kernel)
		(viper-skip-nonseparators 'forward)
		(backward-char))
    (if com
	(progn
	  (forward-char)
	  (viper-execute-com 'viper-end-of-Word val com)))))

(defun viper-backward-word-kernel (val)
  (while (> val 0)
    (viper-backward-char-carefully)
    (cond ((viper-looking-at-alpha)
	   (viper-skip-alpha-backward "_"))
	  ((viper-looking-at-separator)
	   (forward-char)
	   (viper-skip-separators nil)
	   (viper-backward-char-carefully)
	   (cond ((viper-looking-at-alpha)
		  (viper-skip-alpha-backward "_"))
		 ((not (viper-looking-at-alphasep))
		  (viper-skip-nonalphasep-backward))
		 ((bobp)) ; could still be at separator, but at beg of buffer
		 (t (forward-char))))
	  ((not (viper-looking-at-alphasep))
	   (viper-skip-nonalphasep-backward)))
    (setq val (1- val))))

(defun viper-backward-word (arg)
  "Backward word."
  (interactive "P")
  (viper-leave-region-active)
  (let ((val (viper-p-val arg))
	(com (viper-getcom arg)))
    (if com
	(let (i)
	  (if (setq i (save-excursion (backward-char) (looking-at "\n")))
	      (backward-char))
	  (viper-move-marker-locally 'viper-com-point (point))
	  (if i (forward-char))))
    (viper-backward-word-kernel val)
    (if com (viper-execute-com 'viper-backward-word val com))))

(defun viper-backward-Word (arg)
  "Backward word delimited by white character."
  (interactive "P")
  (viper-leave-region-active)
  (let ((val (viper-p-val arg))
	(com (viper-getcom arg)))
    (if com
	(let (i)
	  (if (setq i (save-excursion (backward-char) (looking-at "\n")))
	      (backward-char))
	  (viper-move-marker-locally 'viper-com-point (point))
	  (if i (forward-char))))
    (viper-loop val
		(viper-skip-separators nil) ; nil means backward here
		(viper-skip-nonseparators 'backward))
    (if com (viper-execute-com 'viper-backward-Word val com))))



;; line commands

(defun viper-beginning-of-line (arg)
  "Go to beginning of line."
  (interactive "P")
  (viper-leave-region-active)
  (let ((val (viper-p-val arg))
	(com (viper-getcom arg)))
    (if com (viper-move-marker-locally 'viper-com-point (point)))
    (beginning-of-line val)
    (if com (viper-execute-com 'viper-beginning-of-line val com))))

(defun viper-bol-and-skip-white (arg)
  "Beginning of line at first non-white character."
  (interactive "P")
  (viper-leave-region-active)
  (let ((val (viper-p-val arg))
	(com (viper-getcom arg)))
    (if com (viper-move-marker-locally 'viper-com-point (point)))
    (forward-to-indentation (1- val))
    (if com (viper-execute-com 'viper-bol-and-skip-white val com))))

(defun viper-goto-eol (arg)
  "Go to end of line."
  (interactive "P")
  (viper-leave-region-active)
  (let ((val (viper-p-val arg))
	(com (viper-getcom arg)))
    (if com (viper-move-marker-locally 'viper-com-point (point)))
    (end-of-line val)
    (if com (viper-execute-com 'viper-goto-eol val com))
    (if viper-ex-style-motion
	(if (and (eolp) (not (bolp))
		 ;; a fix for viper-change-to-eol
		 (not (equal viper-current-state 'insert-state)))
	    (backward-char 1)
    ))))


(defun viper-goto-col (arg)
  "Go to ARG's column."
  (interactive "P")
  (viper-leave-region-active)
  (let ((val (viper-p-val arg))
	(com (viper-getcom arg))
	line-len)
    (setq line-len
	  (viper-chars-in-region
	   (viper-line-pos 'start) (viper-line-pos 'end)))
    (if com (viper-move-marker-locally 'viper-com-point (point)))
    (beginning-of-line)
    (forward-char (1- (min line-len val)))
    (while (> (current-column) (1- val))
      (backward-char 1))
    (if com (viper-execute-com 'viper-goto-col val com))
    (save-excursion
      (end-of-line)
      (if (> val (current-column)) (error "Viper bell")))
    ))


(defun viper-next-line (arg)
  "Go to next line."
  (interactive "P")
  (viper-leave-region-active)
  (let ((val (viper-p-val arg))
	(com (viper-getCom arg)))
    (if com (viper-move-marker-locally 'viper-com-point (point)))
    ;; do not use forward-line! need to keep column
    (let ((line-move-visual nil))
      (if (featurep 'emacs)
	  (with-no-warnings (next-line val))
	(next-line val)))
    (if viper-ex-style-motion
	(if (and (eolp) (not (bolp))) (backward-char 1)))
    (setq this-command 'next-line)
    (if com (viper-execute-com 'viper-next-line val com))))

(declare-function widget-type "wid-edit" (widget))
(declare-function widget-button-press "wid-edit" (pos &optional event))
(declare-function viper-set-hooks "viper" ())

(defun viper-next-line-at-bol (arg)
  "Next line at beginning of line.
If point is on a widget or a button, simulate clicking on that widget/button."
  (interactive "P")
  (let* ((field (get-char-property (point) 'field))
	 (button (get-char-property (point) 'button))
	 (doc (get-char-property (point) 'widget-doc))
	 (widget (or field button doc)))
    (if (and widget
             (if (symbolp widget)
                 (get widget 'widget-type)
               (and (consp widget)
                    (get (widget-type widget) 'widget-type))))
        (widget-button-press (point))
      (if (and (fboundp 'button-at) (fboundp 'push-button) (button-at (point)))
          (push-button)
	;; not a widget or a button
        (viper-leave-region-active)
        (save-excursion
          (end-of-line)
          (if (eobp) (error "Last line in buffer")))
        (let ((val (viper-p-val arg))
              (com (viper-getCom arg)))
          (if com (viper-move-marker-locally 'viper-com-point (point)))
          (forward-line val)
          (back-to-indentation)
          (if com (viper-execute-com 'viper-next-line-at-bol val com)))))))


(defun viper-previous-line (arg)
  "Go to previous line."
  (interactive "P")
  (viper-leave-region-active)
  (let ((val (viper-p-val arg))
	(com (viper-getCom arg)))
    (if com (viper-move-marker-locally 'viper-com-point (point)))
    ;; do not use forward-line! need to keep column
    (let ((line-move-visual nil))
      (if (featurep 'emacs)
	  (with-no-warnings (previous-line val))
	(previous-line val)))
    (if viper-ex-style-motion
	(if (and (eolp) (not (bolp))) (backward-char 1)))
    (setq this-command 'previous-line)
    (if com (viper-execute-com 'viper-previous-line val com))))


(defun viper-previous-line-at-bol (arg)
  "Previous line at beginning of line."
  (interactive "P")
  (viper-leave-region-active)
  (save-excursion
    (beginning-of-line)
    (if (bobp) (error "First line in buffer")))
  (let ((val (viper-p-val arg))
	(com (viper-getCom arg)))
    (if com (viper-move-marker-locally 'viper-com-point (point)))
    (forward-line (- val))
    (back-to-indentation)
    (if com (viper-execute-com 'viper-previous-line val com))))

(defun viper-change-to-eol (arg)
  "Change to end of line."
  (interactive "P")
  (viper-goto-eol (cons arg ?c)))

(defun viper-kill-line (arg)
  "Delete line."
  (interactive "P")
  (viper-goto-eol (cons arg ?d)))

(defun viper-erase-line (arg)
  "Erase line."
  (interactive "P")
  (viper-beginning-of-line (cons arg ?d)))


;;; Moving around

(defun viper-goto-line (arg)
  "Go to ARG's line.  Without ARG go to end of buffer."
  (interactive "P")
  (let ((val (viper-P-val arg))
	(com (viper-getCom arg)))
    (viper-move-marker-locally 'viper-com-point (point))
    (viper-deactivate-mark)
    (push-mark nil t)
    (if (null val)
	(goto-char (point-max))
      (goto-char (point-min))
      (forward-line (1- val)))

    ;; positioning is done twice: before and after command execution
    (if (and (eobp) (bolp) (not (bobp))) (forward-line -1))
    (back-to-indentation)

    (if com (viper-execute-com 'viper-goto-line val com))

    (if (and (eobp) (bolp) (not (bobp))) (forward-line -1))
    (back-to-indentation)
    ))

;; Find ARG's occurrence of CHAR on the current line.
;; If FORWARD then search is forward, otherwise backward.  OFFSET is used to
;; adjust point after search.
(defun viper-find-char (arg char forward offset)
  (or (char-or-string-p char) (error "Viper bell"))
  (let ((arg (if forward arg (- arg)))
	(cmd (if (eq viper-intermediate-command 'viper-repeat)
		 (nth 5 viper-d-com)
	       (viper-array-to-string (this-command-keys))))
	point region-beg region-end)
    (save-excursion
      (save-restriction
	(if (> arg 0) ; forward
	    (progn
	      (setq region-beg (point))
	      (if viper-allow-multiline-replace-regions
		  (viper-forward-paragraph 1)
		(end-of-line))
	      (setq region-end (point)))
	  (setq region-end (point))
	      (if viper-allow-multiline-replace-regions
		  (viper-backward-paragraph 1)
		(beginning-of-line))
	      (setq region-beg (point)))
	(if (or (and (< arg 0)
		     (< (- region-end region-beg)
			(if viper-allow-multiline-replace-regions
			    2 1))
		     (bolp))
		(and (> arg 0)
		     (< (- region-end region-beg)
			(if viper-allow-multiline-replace-regions
			    3 2))
		     (eolp)))
	    (error "Command `%s':  At %s of %s"
		   cmd
		   (if (> arg 0) "end" "beginning")
		   (if viper-allow-multiline-replace-regions
		       "paragraph" "line")))
	(narrow-to-region region-beg region-end)
	;; if arg > 0, point is forwarded before search.
	(if (> arg 0) (goto-char (1+ (point-min)))
	  (goto-char (point-max)))
	(if (let ((case-fold-search nil))
	      (search-forward (char-to-string char) nil 0 arg))
	    (setq point (point))
	  (error "Command `%s':  `%c' not found" cmd char))))
    (goto-char point)
    (if (> arg 0)
	(backward-char (if offset 2 1))
      (forward-char (if offset 1 0)))))

(defun viper-find-char-forward (arg)
  "Find char on the line.
If called interactively read the char to find from the terminal, and if
called from viper-repeat, the char last used is used.  This behavior is
controlled by the sign of prefix numeric value."
  (interactive "P")
  (let ((val (viper-p-val arg))
	(com (viper-getcom arg))
	(cmd-representation (nth 5 viper-d-com)))
    (if (> val 0)
	;; this means that the function was called interactively
	(setq viper-f-char (read-char)
	      viper-f-forward t
	      viper-f-offset nil)
      ;; viper-repeat --- set viper-F-char from command-keys
      (setq viper-F-char (if (stringp cmd-representation)
			   (viper-seq-last-elt cmd-representation)
			 viper-F-char)
	    viper-f-char viper-F-char)
      (setq val (- val)))
    (if com (viper-move-marker-locally 'viper-com-point (point)))
    (viper-find-char
     val (if (> (viper-p-val arg) 0) viper-f-char viper-F-char) t nil)
    (setq val (- val))
    (if com
	(progn
	  (setq viper-F-char viper-f-char) ; set new viper-F-char
	  (forward-char)
	  (viper-execute-com 'viper-find-char-forward val com)))))

(defun viper-goto-char-forward (arg)
  "Go up to char ARG forward on line."
  (interactive "P")
  (let ((val (viper-p-val arg))
	(com (viper-getcom arg))
	(cmd-representation (nth 5 viper-d-com)))
    (if (> val 0)
	;; this means that the function was called interactively
	(setq viper-f-char (read-char)
	      viper-f-forward t
	      viper-f-offset t)
      ;; viper-repeat --- set viper-F-char from command-keys
      (setq viper-F-char (if (stringp cmd-representation)
			     (viper-seq-last-elt cmd-representation)
			   viper-F-char)
	    viper-f-char viper-F-char)
      (setq val (- val)))
    (if com (viper-move-marker-locally 'viper-com-point (point)))
    (viper-find-char
     val (if (> (viper-p-val arg) 0) viper-f-char viper-F-char) t t)
    (setq val (- val))
    (if com
	(progn
	  (setq viper-F-char viper-f-char) ; set new viper-F-char
	  (forward-char)
	  (viper-execute-com 'viper-goto-char-forward val com)))))

(defun viper-find-char-backward (arg)
  "Find char ARG on line backward."
  (interactive "P")
  (let ((val (viper-p-val arg))
	(com (viper-getcom arg))
	(cmd-representation (nth 5 viper-d-com)))
    (if (> val 0)
	;; this means that the function was called interactively
	(setq viper-f-char (read-char)
	      viper-f-forward nil
	      viper-f-offset nil)
      ;; viper-repeat --- set viper-F-char from command-keys
      (setq viper-F-char (if (stringp cmd-representation)
			   (viper-seq-last-elt cmd-representation)
			 viper-F-char)
	    viper-f-char viper-F-char)
      (setq val (- val)))
    (if com (viper-move-marker-locally 'viper-com-point (point)))
    (viper-find-char
     val (if (> (viper-p-val arg) 0) viper-f-char viper-F-char) nil nil)
    (setq val (- val))
    (if com
	(progn
	  (setq viper-F-char viper-f-char) ; set new viper-F-char
	  (viper-execute-com 'viper-find-char-backward val com)))))

(defun viper-goto-char-backward (arg)
  "Go up to char ARG backward on line."
  (interactive "P")
  (let ((val (viper-p-val arg))
	(com (viper-getcom arg))
	(cmd-representation (nth 5 viper-d-com)))
    (if (> val 0)
	;; this means that the function was called interactively
	(setq viper-f-char (read-char)
	      viper-f-forward nil
	      viper-f-offset t)
      ;; viper-repeat --- set viper-F-char from command-keys
      (setq viper-F-char (if (stringp cmd-representation)
			   (viper-seq-last-elt cmd-representation)
			 viper-F-char)
	    viper-f-char viper-F-char)
      (setq val (- val)))
    (if com (viper-move-marker-locally 'viper-com-point (point)))
    (viper-find-char
     val (if (> (viper-p-val arg) 0) viper-f-char viper-F-char) nil t)
    (setq val (- val))
    (if com
	(progn
	  (setq viper-F-char viper-f-char) ; set new viper-F-char
	  (viper-execute-com 'viper-goto-char-backward val com)))))

(defun viper-repeat-find (arg)
  "Repeat previous find command."
  (interactive "P")
  (let ((val (viper-p-val arg))
	(com (viper-getcom arg)))
    (viper-deactivate-mark)
    (if com (viper-move-marker-locally 'viper-com-point (point)))
    (viper-find-char val viper-f-char viper-f-forward viper-f-offset)
    (if com
	(progn
	  (if viper-f-forward (forward-char))
	  (viper-execute-com 'viper-repeat-find val com)))))

(defun viper-repeat-find-opposite (arg)
  "Repeat previous find command in the opposite direction."
  (interactive "P")
  (let ((val (viper-p-val arg))
	(com (viper-getcom arg)))
    (viper-deactivate-mark)
    (if com (viper-move-marker-locally 'viper-com-point (point)))
    (viper-find-char val viper-f-char (not viper-f-forward) viper-f-offset)
    (if com
	(progn
	  (if viper-f-forward (forward-char))
	  (viper-execute-com 'viper-repeat-find-opposite val com)))))


;; window scrolling etc.

(defun viper-window-top (arg)
  "Go to home window line."
  (interactive "P")
  (let ((val (viper-p-val arg))
	(com (viper-getCom arg)))
    (viper-leave-region-active)
    (if com (viper-move-marker-locally 'viper-com-point (point)))
    (push-mark nil t)
    (move-to-window-line (1- val))

    ;; positioning is done twice: before and after command execution
    (if (and (eobp) (bolp) (not (bobp))) (forward-line -1))
    (back-to-indentation)

    (if com (viper-execute-com 'viper-window-top val com))

    (if (and (eobp) (bolp) (not (bobp))) (forward-line -1))
    (back-to-indentation)
    ))

(defun viper-window-middle (arg)
  "Go to middle window line."
  (interactive "P")
  (let ((val (viper-p-val arg))
	(com (viper-getCom arg)))
    (viper-leave-region-active)
    (if com (viper-move-marker-locally 'viper-com-point (point)))
    (push-mark nil t)
    (move-to-window-line (+ (/ (1- (window-height)) 2) (1- val)))

    ;; positioning is done twice: before and after command execution
    (if (and (eobp) (bolp) (not (bobp))) (forward-line -1))
    (back-to-indentation)

    (if com (viper-execute-com 'viper-window-middle val com))

    (if (and (eobp) (bolp) (not (bobp))) (forward-line -1))
    (back-to-indentation)
    ))

(defun viper-window-bottom (arg)
  "Go to last window line."
  (interactive "P")
  (let ((val (viper-p-val arg))
	(com (viper-getCom arg)))
    (viper-leave-region-active)
    (if com (viper-move-marker-locally 'viper-com-point (point)))
    (push-mark nil t)
    (move-to-window-line (- val))

    ;; positioning is done twice: before and after command execution
    (if (and (eobp) (bolp) (not (bobp))) (forward-line -1))
    (back-to-indentation)

    (if com (viper-execute-com 'viper-window-bottom val com))

    (if (and (eobp) (bolp) (not (bobp))) (forward-line -1))
    (back-to-indentation)
    ))

(defun viper-line-to-top (arg)
  "Put current line on the home line."
  (interactive "p")
  (recenter (1- arg)))

(defun viper-line-to-middle (arg)
  "Put current line on the middle line."
  (interactive "p")
  (recenter (+ (1- arg) (/ (1- (window-height)) 2))))

(defun viper-line-to-bottom (arg)
  "Put current line on the last line."
  (interactive "p")
  (recenter (- (window-height) (1+ arg))))

;; If point is within viper-search-scroll-threshold of window top or bottom,
;; scroll up or down 1/7 of window height, depending on whether we are at the
;; bottom or at the top of the  window.  This function is called by viper-search
;; (which is called from viper-search-forward/backward/next).  If the value of
;; viper-search-scroll-threshold is negative - don't scroll.
(defun viper-adjust-window ()
  (let ((win-height (if (featurep 'xemacs)
			(window-displayed-height)
		      (1- (window-height)))) ; adjust for modeline
	(pt (point))
	at-top-p at-bottom-p
	min-scroll direction)
    (save-excursion
      (move-to-window-line 0) ; top
      (setq at-top-p
	    (<= (count-lines pt (point))
		viper-search-scroll-threshold))
      (move-to-window-line -1) ; bottom
      (setq at-bottom-p
	    (<= (count-lines pt (point)) viper-search-scroll-threshold)))
    (cond (at-top-p (setq min-scroll (1- viper-search-scroll-threshold)
			  direction  1))
	  (at-bottom-p (setq min-scroll (1+ viper-search-scroll-threshold)
			     direction -1)))
    (if min-scroll
	(recenter
	 (* (max min-scroll (/ win-height 7)) direction)))
    ))


;; paren match
;; must correct this to only match ( to ) etc.  On the other hand
;; it is good that paren match gets confused, because that way you
;; catch _all_ imbalances.

(defun viper-paren-match (arg)
  "Go to the matching parenthesis."
  (interactive "P")
  (viper-leave-region-active)
  (let ((com (viper-getcom arg))
	(parse-sexp-ignore-comments viper-parse-sexp-ignore-comments)
	anchor-point)
    (if (integerp arg)
	(if (or (> arg 99) (< arg 1))
	    (error "Prefix must be between 1 and 99")
	  (goto-char
	   (if (> (point-max) 80000)
	       (* (/ (point-max) 100) arg)
	     (/ (* (point-max) arg) 100)))
	  (back-to-indentation))
      (let (beg-lim end-lim)
	(if (and (eolp) (not (bolp))) (forward-char -1))
	(if (not (looking-at "[][(){}]"))
	    (setq anchor-point (point)))
	(setq beg-lim (point-at-bol)
	      end-lim (point-at-eol))
	(cond ((re-search-forward "[][(){}]" end-lim t)
	       (backward-char) )
	      ((re-search-backward "[][(){}]" beg-lim t))
	      (t
	       (error "No matching character on line"))))
      (cond ((looking-at "[\(\[{]")
	     (if com (viper-move-marker-locally 'viper-com-point (point)))
	     (forward-sexp 1)
	     (if com
		 (viper-execute-com 'viper-paren-match nil com)
	       (backward-char)))
	    (anchor-point
	     (if com
		 (progn
		   (viper-move-marker-locally 'viper-com-point anchor-point)
		   (forward-char 1)
		   (viper-execute-com 'viper-paren-match nil com)
		   )))
	    ((looking-at "[])}]")
	     (forward-char)
	     (if com (viper-move-marker-locally 'viper-com-point (point)))
	     (backward-sexp 1)
	     (if com (viper-execute-com 'viper-paren-match nil com)))
	    (t (error "Viper bell"))))))

(defun viper-toggle-parse-sexp-ignore-comments ()
  (interactive)
  (setq viper-parse-sexp-ignore-comments
	(not viper-parse-sexp-ignore-comments))
  (princ (format
	  "From now on, `%%' will %signore parentheses inside comment fields"
	  (if viper-parse-sexp-ignore-comments "" "NOT "))))


;; sentence, paragraph and heading

(defun viper-forward-sentence (arg)
  "Forward sentence."
  (interactive "P")
  (or (eq last-command this-command)
      (push-mark nil t))
  (let ((val (viper-p-val arg))
	(com (viper-getcom arg)))
    (if com (viper-move-marker-locally 'viper-com-point (point)))
    (forward-sentence val)
    (if com (viper-execute-com 'viper-forward-sentence nil com))))

(defun viper-backward-sentence (arg)
  "Backward sentence."
  (interactive "P")
  (or (eq last-command this-command)
      (push-mark nil t))
  (let ((val (viper-p-val arg))
	(com (viper-getcom arg)))
    (if com (viper-move-marker-locally 'viper-com-point (point)))
    (backward-sentence val)
    (if com (viper-execute-com 'viper-backward-sentence nil com))))

(defun viper-forward-paragraph (arg)
  "Forward paragraph."
  (interactive "P")
  (or (eq last-command this-command)
      (push-mark nil t))
  (let ((val (viper-p-val arg))
	;; if you want d} operate on whole lines, change viper-getcom to
	;; viper-getCom below
	(com (viper-getcom arg)))
    (if com (viper-move-marker-locally 'viper-com-point (point)))
    (forward-paragraph val)
    (if com
	(progn
	  (backward-char 1)
	  (viper-execute-com 'viper-forward-paragraph nil com)))))

(defun viper-backward-paragraph (arg)
  "Backward paragraph."
  (interactive "P")
  (or (eq last-command this-command)
      (push-mark nil t))
  (let ((val (viper-p-val arg))
	;; if you want d{ operate on whole lines, change viper-getcom to
	;; viper-getCom below
	(com (viper-getcom arg)))
    (if com (viper-move-marker-locally 'viper-com-point (point)))
    (backward-paragraph val)
    (if com
	(progn
	  (forward-char 1)
	  (viper-execute-com 'viper-backward-paragraph nil com)
	  (backward-char 1)))))

;; should be mode-specific
(defun viper-prev-heading (arg)
  (interactive "P")
  (let ((val (viper-p-val arg))
	(com (viper-getCom arg)))
    (if com (viper-move-marker-locally 'viper-com-point (point)))
    (re-search-backward viper-heading-start nil t val)
    (goto-char (match-beginning 0))
    (if com (viper-execute-com 'viper-prev-heading nil com))))

(defun viper-heading-end (arg)
  (interactive "P")
  (let ((val (viper-p-val arg))
	(com (viper-getCom arg)))
    (if com (viper-move-marker-locally 'viper-com-point (point)))
    (re-search-forward viper-heading-end nil t val)
    (goto-char (match-beginning 0))
    (if com (viper-execute-com 'viper-heading-end nil com))))

(defun viper-next-heading (arg)
  (interactive "P")
  (let ((val (viper-p-val arg))
	(com (viper-getCom arg)))
    (if com (viper-move-marker-locally 'viper-com-point (point)))
    (end-of-line)
    (re-search-forward viper-heading-start nil t val)
    (goto-char (match-beginning 0))
    (if com (viper-execute-com 'viper-next-heading nil com))))


;; scrolling

(defun viper-scroll-screen (arg)
  "Scroll to next screen."
  (interactive "p")
  (condition-case nil
      (if (> arg 0)
	  (while (> arg 0)
	    (scroll-up)
	    (setq arg (1- arg)))
	(while (> 0 arg)
	  (scroll-down)
	  (setq arg (1+ arg))))
    (error (beep 1)
	   (if (> arg 0)
	       (progn
		 (message "End of buffer")
		 (goto-char (point-max)))
	     (message "Beginning of buffer")
	     (goto-char (point-min))))
    ))

(defun viper-scroll-screen-back (arg)
  "Scroll to previous screen."
  (interactive "p")
  (viper-scroll-screen (- arg)))

(defun viper-scroll-down (arg)
  "Pull down half screen."
  (interactive "P")
  (condition-case nil
      (if (null arg)
	  (scroll-down (/ (window-height) 2))
	(scroll-down arg))
    (error (beep 1)
	   (message "Beginning of buffer")
	   (goto-char (point-min)))))

(defun viper-scroll-down-one (arg)
  "Scroll up one line."
  (interactive "p")
  (scroll-down arg))

(defun viper-scroll-up (arg)
  "Pull up half screen."
  (interactive "P")
  (condition-case nil
      (if (null arg)
	  (scroll-up (/ (window-height) 2))
	(scroll-up arg))
    (error (beep 1)
	   (message "End of buffer")
	   (goto-char (point-max)))))

(defun viper-scroll-up-one (arg)
  "Scroll down one line."
  (interactive "p")
  (scroll-up arg))


;; searching

(defun viper-insert-isearch-string ()
  "Insert `isearch' last search string."
  (interactive)
  (when isearch-string (insert isearch-string)))

(defun viper-if-string (prompt)
  (if (memq viper-intermediate-command
	    '(viper-command-argument viper-digit-argument viper-repeat))
      (setq viper-this-command-keys (this-command-keys)))
  (let* ((keymap (let ((keymap (copy-keymap minibuffer-local-map)))
                   (define-key keymap [(control ?s)] 'viper-insert-isearch-string)
                   keymap))
         (s (viper-read-string-with-history
	    prompt
	    nil ; no initial
	    'viper-search-history
             (car viper-search-history)
             keymap)))
    (if (not (string= s ""))
	(setq viper-s-string s))))


(defun viper-toggle-search-style (arg)
  "Toggle the value of viper-case-fold-search/viper-re-search.
Without prefix argument, will ask which search style to toggle.  With prefix
arg 1, toggles viper-case-fold-search; with arg 2 toggles viper-re-search.

Although this function is bound to \\[viper-toggle-search-style], the most
convenient way to use it is to bind `//' to the macro
`1 M-x viper-toggle-search-style' and `///' to
`2 M-x viper-toggle-search-style'.  In this way, hitting `//' quickly will
toggle case-fold-search and hitting `/' three times with toggle regexp
search.  Macros are more convenient in this case because they don't affect
the Emacs binding of `/'."
  (interactive "P")
  (let (msg)
    (cond ((or (eq arg 1)
	       (and (null arg)
		    (y-or-n-p (format "Search style: '%s'.  Want '%s'? "
				      (if viper-case-fold-search
					  "case-insensitive" "case-sensitive")
				      (if viper-case-fold-search
					  "case-sensitive"
					"case-insensitive")))))
	   (setq viper-case-fold-search (null viper-case-fold-search))
	   (if viper-case-fold-search
	       (setq msg "Search becomes case-insensitive")
	     (setq msg "Search becomes case-sensitive")))
	  ((or (eq arg 2)
	       (and (null arg)
		    (y-or-n-p (format "Search style: '%s'.  Want '%s'? "
				      (if viper-re-search
					  "regexp-search" "vanilla-search")
				      (if viper-re-search
					  "vanilla-search"
					"regexp-search")))))
	   (setq viper-re-search (null viper-re-search))
	   (if viper-re-search
	       (setq msg "Search becomes regexp-style")
	     (setq msg "Search becomes vanilla-style")))
	  (t
	   (setq msg "Search style remains unchanged")))
    (princ msg t)))

(defun viper-set-searchstyle-toggling-macros (unset &optional major-mode)
  "Set the macros for toggling the search style in Viper's vi-state.
The macro that toggles case sensitivity is bound to `//', and the one that
toggles regexp search is bound to `///'.
With a prefix argument, this function unsets the macros.
If MAJOR-MODE is set, set the macros only in that major mode."
  (interactive "P")
  (let (scope)
    (if (and major-mode (symbolp major-mode))
	(setq scope major-mode)
      (setq scope 't))
    (or noninteractive
	(if (not unset)
	    (progn
	      ;; toggle case sensitivity in search
	      (viper-record-kbd-macro
	       "//" 'vi-state
	       [1 (meta x) v i p e r - t o g g l e - s e a r c h - s t y l e return]
	       scope)
	      ;; toggle regexp/vanilla search
	      (viper-record-kbd-macro
	       "///" 'vi-state
	       [2 (meta x) v i p e r - t o g g l e - s e a r c h - s t y l e return]
	       scope)
	      ;; XEmacs has no called-interactively-p
	      ;; (if (called-interactively-p 'interactive)
	      (if (interactive-p)
		  (message
		   "// and /// now toggle case-sensitivity and regexp search")))
	  (viper-unrecord-kbd-macro "//" 'vi-state)
	  (sit-for 2)
	  (viper-unrecord-kbd-macro "///" 'vi-state)))
    ))


(defun viper-set-parsing-style-toggling-macro (unset)
  "Set `%%%' to be a macro that toggles whether comment fields should be parsed for matching parentheses.
This is used in conjunction with the `%' command.

With a prefix argument, unsets the macro."
  (interactive "P")
  (or noninteractive
      (if (not unset)
	  (progn
	    ;; Make %%% toggle parsing comments for matching parentheses
	    (viper-record-kbd-macro
	     "%%%" 'vi-state
	     [(meta x) v i p e r - t o g g l e - p a r s e - s e x p - i g n o r e - c o m m e n t s return]
	     't)
	    ;; XEmacs has no called-interactively-p. And interactive-p
	    ;; works fine here.
	    ;; (if (called-interactively-p 'interactive)
	    (if (interactive-p)
		(message
		 "%%%%%% now toggles whether comments should be parsed for matching parentheses")))
	(viper-unrecord-kbd-macro "%%%" 'vi-state))))


(defun viper-set-emacs-state-searchstyle-macros (unset &optional arg-majormode)
  "Set the macros for toggling the search style in Viper's emacs-state.
The macro that toggles case sensitivity is bound to `//', and the one that
toggles regexp search is bound to `///'.
With a prefix argument, this function unsets the macros.
If the optional prefix argument is non-nil and specifies a valid major mode,
this sets the macros only in the macros in that major mode.  Otherwise,
the macros are set in the current major mode.
\(When unsetting the macros, the second argument has no effect.\)"
  (interactive "P")
  (or noninteractive
      (if (not unset)
	  (progn
	    ;; toggle case sensitivity in search
	    (viper-record-kbd-macro
	     "//" 'emacs-state
	     [1 (meta x) v i p e r - t o g g l e - s e a r c h - s t y l e return]
	     (or arg-majormode major-mode))
	    ;; toggle regexp/vanilla search
	    (viper-record-kbd-macro
	     "///" 'emacs-state
	     [2 (meta x) v i p e r - t o g g l e - s e a r c h - s t y l e return]
	     (or arg-majormode major-mode))
	    ;; called-interactively-p does not work for
	    ;; XEmacs. interactive-p is ok here.
	    ;; (if (called-interactively-p 'interactive)
	    (if (interactive-p)
		(message
		 "// and /// now toggle case-sensitivity and regexp search.")))
	(viper-unrecord-kbd-macro "//" 'emacs-state)
	(sit-for 2)
	(viper-unrecord-kbd-macro "///" 'emacs-state))))


(defun viper-search-forward (arg)
  "Search a string forward.
ARG is used to find the ARG's occurrence of the string.
Null string will repeat previous search."
  (interactive "P")
  (let ((val (viper-P-val arg))
	(com (viper-getcom arg))
	(old-str viper-s-string)
	debug-on-error)
    (setq viper-s-forward t)
    (viper-if-string "/")
    ;; this is not used at present, but may be used later
    (if (or (not (equal old-str viper-s-string))
	    (not (markerp viper-local-search-start-marker))
	    (not (marker-buffer viper-local-search-start-marker)))
	(setq viper-local-search-start-marker (point-marker)))
    (viper-search viper-s-string t val)
    (if com
	(progn
	  (viper-move-marker-locally 'viper-com-point (mark t))
	  (viper-execute-com 'viper-search-next val com)))
    ))

(defun viper-search-backward (arg)
  "Search a string backward.
ARG is used to find the ARG's occurrence of the string.
Null string will repeat previous search."
  (interactive "P")
  (let ((val (viper-P-val arg))
	(com (viper-getcom arg))
	(old-str viper-s-string)
	debug-on-error)
    (setq viper-s-forward nil)
    (viper-if-string "?")
    ;; this is not used at present, but may be used later
    (if (or (not (equal old-str viper-s-string))
	    (not (markerp viper-local-search-start-marker))
	    (not (marker-buffer viper-local-search-start-marker)))
	(setq viper-local-search-start-marker (point-marker)))
    (viper-search viper-s-string nil val)
    (if com
	(progn
	  (viper-move-marker-locally 'viper-com-point (mark t))
	  (viper-execute-com 'viper-search-next val com)))))


;; Search for COUNT's occurrence of STRING.
;; Search is forward if FORWARD is non-nil, otherwise backward.
;; INIT-POINT is the position where search is to start.
;; Arguments:
;;   (STRING FORW COUNT &optional NO-OFFSET INIT-POINT LIMIT FAIL-IF-NOT-FOUND)
(defun viper-search (string forward arg
			    &optional no-offset init-point fail-if-not-found)
  (if (not (equal string ""))
    (let ((val (viper-p-val arg))
	  (com (viper-getcom arg))
	  (offset (not no-offset))
	  (case-fold-search viper-case-fold-search)
	  (start-point (or init-point (point))))
      (viper-deactivate-mark)
      (if forward
	  (condition-case nil
	      (progn
	        (if offset (viper-forward-char-carefully))
	        (if viper-re-search
		    (progn
		      (re-search-forward string nil nil val)
		      (re-search-backward string))
		  (search-forward string nil nil val)
		  (search-backward string))
		(if (not (equal start-point (point)))
		    (push-mark start-point t)))
	    (search-failed
	     (if (and (not fail-if-not-found) viper-search-wrap-around)
	         (progn
		   (message "Search wrapped around BOTTOM of buffer")
		   (goto-char (point-min))
		   (viper-search string forward (cons 1 com) t start-point 'fail)
		   ;; don't wait in macros
		   (or executing-kbd-macro
		       (memq viper-intermediate-command
			     '(viper-repeat
			       viper-digit-argument
			       viper-command-argument))
		       (sit-for 2))
		   ;; delete the wrap-around message
		   (message "")
		   )
	       (goto-char start-point)
	       (error "`%s': %s not found"
		      string
		      (if viper-re-search "Pattern" "String"))
	       )))
	;; backward
        (condition-case nil
	    (progn
	      (if viper-re-search
		  (re-search-backward string nil nil val)
	        (search-backward string nil nil val))
	      (if (not (equal start-point (point)))
		  (push-mark start-point t)))
	  (search-failed
	   (if (and (not fail-if-not-found) viper-search-wrap-around)
	       (progn
		 (message "Search wrapped around TOP of buffer")
	         (goto-char (point-max))
	         (viper-search string forward (cons 1 com) t start-point 'fail)
		 ;; don't wait in macros
		 (or executing-kbd-macro
		     (memq viper-intermediate-command
			   '(viper-repeat
			     viper-digit-argument
			     viper-command-argument))
		     (sit-for 2))
		 ;; delete the wrap-around message
		 (message "")
		 )
	     (goto-char start-point)
	     (error "`%s': %s not found"
		    string
		    (if viper-re-search "Pattern" "String"))
	     ))))
      ;; pull up or down if at top/bottom of window
      (viper-adjust-window)
      ;; highlight the result of search
      ;; don't wait and don't highlight in macros
      (or executing-kbd-macro
	  (memq viper-intermediate-command
		'(viper-repeat viper-digit-argument viper-command-argument))
	  (viper-flash-search-pattern))
      )))

(defun viper-search-next (arg)
  "Repeat previous search."
  (interactive "P")
  (let ((val (viper-p-val arg))
	(com (viper-getcom arg))
	debug-on-error)
    (if (or (null viper-s-string) (string= viper-s-string ""))
	(error viper-NoPrevSearch))
    (viper-search viper-s-string viper-s-forward arg)
    (if com
	(progn
	  (viper-move-marker-locally 'viper-com-point (mark t))
	  (viper-execute-com 'viper-search-next val com)))))

(defun viper-search-Next (arg)
  "Repeat previous search in the reverse direction."
  (interactive "P")
  (let ((val (viper-p-val arg))
	(com (viper-getcom arg))
	debug-on-error)
    (if (null viper-s-string) (error viper-NoPrevSearch))
    (viper-search viper-s-string (not viper-s-forward) arg)
    (if com
	(progn
	  (viper-move-marker-locally 'viper-com-point (mark t))
	  (viper-execute-com 'viper-search-Next val com)))))


;; Search contents of buffer defined by one of Viper's motion commands.
;; Repeatable via `n' and `N'.
(defun viper-buffer-search-enable (&optional c)
  (cond (c (setq viper-buffer-search-char c))
	((null viper-buffer-search-char)
	 ;; ?g acts as a default value for viper-buffer-search-char
	 (setq viper-buffer-search-char ?g)))
  (define-key viper-vi-basic-map
    (cond ((viper-characterp viper-buffer-search-char)
	   (char-to-string viper-buffer-search-char))
	  (t (error "viper-buffer-search-char: wrong value type, %S"
		    viper-buffer-search-char)))
    'viper-command-argument)
  (aset viper-exec-array viper-buffer-search-char 'viper-exec-buffer-search)
  (setq viper-prefix-commands
	(cons viper-buffer-search-char viper-prefix-commands)))

;; This is a Viper wrapper for isearch-forward.
(defun viper-isearch-forward (arg)
  "Do incremental search forward."
  (interactive "P")
  ;; emacs bug workaround
  (if (listp arg) (setq arg (car arg)))
  (viper-exec-form-in-emacs (list 'isearch-forward arg)))

;; This is a Viper wrapper for isearch-backward."
(defun viper-isearch-backward (arg)
  "Do incremental search backward."
  (interactive "P")
  ;; emacs bug workaround
  (if (listp arg) (setq arg (car arg)))
  (viper-exec-form-in-emacs (list 'isearch-backward arg)))


;; visiting and killing files, buffers

(defun viper-switch-to-buffer ()
  "Switch to buffer in the current window."
  (interactive)
  (let ((other-buffer (other-buffer (current-buffer)))
	buffer)
    (setq buffer
	  (funcall viper-read-buffer-function
		   "Switch to buffer in this window: " other-buffer))
    (switch-to-buffer buffer)))

(defun viper-switch-to-buffer-other-window ()
  "Switch to buffer in another window."
  (interactive)
  (let ((other-buffer (other-buffer (current-buffer)))
	buffer)
    (setq buffer
	  (funcall viper-read-buffer-function
		   "Switch to buffer in another window: " other-buffer))
    (switch-to-buffer-other-window buffer)))

(defun viper-kill-buffer ()
  "Kill a buffer."
  (interactive)
  (let (buffer buffer-name)
    (setq buffer-name
	  (funcall viper-read-buffer-function
		   (format "Kill buffer \(%s\): "
			   (buffer-name (current-buffer)))))
    (setq buffer
	  (if (null buffer-name)
	      (current-buffer)
	    (get-buffer buffer-name)))
    (if (null buffer) (error "`%s': No such buffer" buffer-name))
    (if (or (not (buffer-modified-p buffer))
	    (y-or-n-p
	     (format
	      "Buffer `%s' is modified, are you sure you want to kill it? "
	      buffer-name)))
	(kill-buffer buffer)
      (error "Buffer not killed"))))



;; yank and pop

(defsubst viper-yank (text)
  "Yank TEXT silently.  This works correctly with Emacs's yank-pop command."
    (insert text)
    (setq this-command 'yank))

(defun viper-put-back (arg)
  "Put back after point/below line."
  (interactive "P")
  (let ((val (viper-p-val arg))
	(text (if viper-use-register
		  (cond ((viper-valid-register viper-use-register '(digit))
			 (current-kill
			  (- viper-use-register ?1) 'do-not-rotate))
			((viper-valid-register viper-use-register)
			 (get-register (downcase viper-use-register)))
			(t (error viper-InvalidRegister viper-use-register)))
		(current-kill 0)))
	sv-point chars-inserted lines-inserted)
    (if (null text)
	(if viper-use-register
	    (let ((reg viper-use-register))
	      (setq viper-use-register nil)
	      (error viper-EmptyRegister reg))
	  (error "Viper bell")))
    (setq viper-use-register nil)
    (if (viper-end-with-a-newline-p text)
	(progn
	  (end-of-line)
	  (if (eobp)
	      (insert "\n")
	    (forward-line 1))
	  (beginning-of-line))
      (if (not (eolp)) (viper-forward-char-carefully)))
    (set-marker (viper-mark-marker) (point) (current-buffer))
    (viper-set-destructive-command
     (list 'viper-put-back val nil viper-use-register nil nil))
    (setq sv-point (point))
    (viper-loop val (viper-yank text))
    (setq chars-inserted (abs (- (point) sv-point))
	  lines-inserted (abs (count-lines (point) sv-point)))
    (if (or (> chars-inserted viper-change-notification-threshold)
	    (> lines-inserted viper-change-notification-threshold))
	(unless (viper-is-in-minibuffer)
	  (message "Inserted %d character(s), %d line(s)"
		   chars-inserted lines-inserted))))
  ;; Vi puts cursor on the last char when the yanked text doesn't contain a
  ;; newline; it leaves the cursor at the beginning when the text contains
  ;; a newline
  (if (viper-same-line (point) (mark))
      (or (= (point) (mark)) (viper-backward-char-carefully))
    (exchange-point-and-mark)
    (if (bolp)
	(back-to-indentation)))
  (viper-deactivate-mark))

(defun viper-Put-back (arg)
  "Put back at point/above line."
  (interactive "P")
  (let ((val (viper-p-val arg))
	(text (if viper-use-register
		  (cond ((viper-valid-register viper-use-register '(digit))
			 (current-kill
			  (- viper-use-register ?1) 'do-not-rotate))
			((viper-valid-register viper-use-register)
			 (get-register (downcase viper-use-register)))
			(t (error viper-InvalidRegister viper-use-register)))
		(current-kill 0)))
	sv-point chars-inserted lines-inserted)
    (if (null text)
	(if viper-use-register
	    (let ((reg viper-use-register))
	      (setq viper-use-register nil)
	      (error viper-EmptyRegister reg))
	  (error "Viper bell")))
    (setq viper-use-register nil)
    (if (viper-end-with-a-newline-p text) (beginning-of-line))
    (viper-set-destructive-command
     (list 'viper-Put-back val nil viper-use-register nil nil))
    (set-marker (viper-mark-marker) (point) (current-buffer))
    (setq sv-point (point))
    (viper-loop val (viper-yank text))
    (setq chars-inserted (abs (- (point) sv-point))
	  lines-inserted (abs (count-lines (point) sv-point)))
    (if (or (> chars-inserted viper-change-notification-threshold)
	    (> lines-inserted viper-change-notification-threshold))
	(unless (viper-is-in-minibuffer)
	  (message "Inserted %d character(s), %d line(s)"
		   chars-inserted lines-inserted))))
  ;; Vi puts cursor on the last char when the yanked text doesn't contain a
  ;; newline; it leaves the cursor at the beginning when the text contains
  ;; a newline
  (if (viper-same-line (point) (mark))
      (or (= (point) (mark)) (viper-backward-char-carefully))
    (exchange-point-and-mark)
    (if (bolp)
	(back-to-indentation)))
  (viper-deactivate-mark))


;; Copy region to kill-ring.
;; If BEG and END do not belong to the same buffer, copy empty region.
(defun viper-copy-region-as-kill (beg end)
  (condition-case nil
      (copy-region-as-kill beg end)
    (error (copy-region-as-kill beg beg))))


(defun viper-delete-char (arg)
  "Delete next character."
  (interactive "P")
  (let ((val (viper-p-val arg))
	end-del-pos)
    (viper-set-destructive-command
     (list 'viper-delete-char val nil nil nil nil))
    (if (and viper-ex-style-editing
	     (> val (viper-chars-in-region (point) (viper-line-pos 'end))))
	(setq val (viper-chars-in-region (point) (viper-line-pos 'end))))
    (if (and viper-ex-style-motion (eolp))
	(if (bolp) (error "Viper bell") (setq val 0))) ; not bol---simply back 1 ch
    (save-excursion
      (viper-forward-char-carefully val)
      (setq end-del-pos (point)))
    (if viper-use-register
	(progn
	  (cond ((viper-valid-register viper-use-register '((Letter)))
		 (viper-append-to-register
		  (downcase viper-use-register) (point) end-del-pos))
		((viper-valid-register viper-use-register)
		 (copy-to-register
		  viper-use-register (point) end-del-pos nil))
		(t (error viper-InvalidRegister viper-use-register)))
	  (setq viper-use-register nil)))

    (delete-char val t)
    (if viper-ex-style-motion
	(if (and (eolp) (not (bolp))) (backward-char 1)))
    ))

(defun viper-delete-backward-char (arg)
  "Delete previous character.  On reaching beginning of line, stop and beep."
  (interactive "P")
  (let ((val (viper-p-val arg))
	end-del-pos)
    (viper-set-destructive-command
     (list 'viper-delete-backward-char val nil nil nil nil))
    (if (and
	 viper-ex-style-editing
	 (> val (viper-chars-in-region (viper-line-pos 'start) (point))))
	(setq val (viper-chars-in-region (viper-line-pos 'start) (point))))
    (save-excursion
      (viper-backward-char-carefully val)
      (setq end-del-pos (point)))
    (if viper-use-register
	(progn
	  (cond ((viper-valid-register viper-use-register '(Letter))
		 (viper-append-to-register
		  (downcase viper-use-register) end-del-pos (point)))
		((viper-valid-register viper-use-register)
		 (copy-to-register
		  viper-use-register end-del-pos (point) nil))
		(t (error viper-InvalidRegister viper-use-register)))
	  (setq viper-use-register nil)))
    (if (and (bolp) viper-ex-style-editing)
	(ding))
    (delete-char (- val) t)))


(defun viper-del-backward-char-in-insert ()
  "Delete 1 char backwards while in insert mode."
  (interactive)
  (if (and viper-ex-style-editing (bolp))
      (beep 1)
    ;; don't put on kill ring
    (delete-char -1 nil)))


(defun viper-del-backward-char-in-replace ()
  "Delete one character in replace mode.
If `viper-delete-backwards-in-replace' is t, then DEL key actually deletes
characters.  If it is nil, then the cursor just moves backwards, similarly
to Vi.  The variable `viper-ex-style-editing', if t, doesn't let the
cursor move past the beginning of line."
  (interactive)
  (cond (viper-delete-backwards-in-replace
	 (cond ((not (bolp))
		;; don't put on kill ring
		(delete-char -1 nil))
	       (viper-ex-style-editing
		(beep 1))
	       ((bobp)
		(beep 1))
	       (t
		;; don't put on kill ring
		(delete-char -1 nil))))
	(viper-ex-style-editing
	 (if (bolp)
	     (beep 1)
	   (backward-char 1)))
	(t
	 (backward-char 1))))



;; join lines.

(defun viper-join-lines (arg)
  "Join this line to next, if ARG is nil.  Otherwise, join ARG lines."
  (interactive "*P")
  (let ((val (viper-P-val arg)))
    (viper-set-destructive-command
     (list 'viper-join-lines val nil nil nil nil))
    (viper-loop (if (null val) 1 (1- val))
		(end-of-line)
		(if (not (eobp))
		    (progn
		      (forward-line 1)
		      (delete-region (point) (1- (point)))
		      (fixup-whitespace)
		      ;; fixup-whitespace sometimes does not leave space
		      ;; between objects, so we insert it as in Vi
		      (or (looking-at " ")
			  (insert " ")
			  (backward-char 1))
		      )))))


;; Replace state

(defun viper-change (beg end)
  (if (markerp beg) (setq beg (marker-position beg)))
  (if (markerp end) (setq end (marker-position end)))
  ;; beg is sometimes (mark t), which may be nil
  (or beg (setq beg end))

  (viper-set-complex-command-for-undo)
  (if viper-use-register
      (progn
	(copy-to-register viper-use-register beg end nil)
	(setq viper-use-register nil)))
  (viper-set-replace-overlay beg end)
  (setq last-command nil) ; separate repl text from prev kills

  (if (= (viper-replace-start) (point-max))
      (error "End of buffer"))

  (setq viper-last-replace-region
	(buffer-substring (viper-replace-start)
			  (viper-replace-end)))

  ;; protect against error while inserting "@" and other disasters
  ;; (e.g., read-only buff)
  (condition-case conds
      (if (or viper-allow-multiline-replace-regions
	      (viper-same-line (viper-replace-start)
			       (viper-replace-end)))
	  (progn
	    ;; tabs cause problems in replace, so untabify
	    (goto-char (viper-replace-end))
	    (insert-before-markers "@") ; put placeholder after the TAB
	    (untabify (viper-replace-start) (point))
	    ;; del @, don't put on kill ring
	    (delete-char -1)

	    (viper-set-replace-overlay-glyphs
	     viper-replace-region-start-delimiter
	     viper-replace-region-end-delimiter)
	    ;; this move takes care of the last posn in the overlay, which
	    ;; has to be shifted because of insert.  We can't simply insert
	    ;; "$" before-markers because then overlay-start will shift the
	    ;; beginning of the overlay in case we are replacing a single
	    ;; character.  This fixes the bug with `s' and `cl' commands.
	    (viper-move-replace-overlay (viper-replace-start) (point))
	    (goto-char (viper-replace-start))
	    (viper-change-state-to-replace t))
	(kill-region (viper-replace-start)
		     (viper-replace-end))
	(viper-hide-replace-overlay)
	(viper-change-state-to-insert))
    (error ;; make sure that the overlay doesn't stay.
           ;; go back to the original point
     (goto-char (viper-replace-start))
     (viper-hide-replace-overlay)
     (viper-message-conditions conds))))


(defun viper-change-subr (beg end)
  ;; beg is sometimes (mark t), which may be nil
  (or beg (setq beg end))
  (if viper-use-register
      (progn
	(copy-to-register viper-use-register beg end nil)
	(setq viper-use-register nil)))
  (kill-region beg end)
  (setq this-command 'viper-change)
  (viper-yank-last-insertion))

(defun viper-toggle-case (arg)
  "Toggle character case."
  (interactive "P")
  (let ((val (viper-p-val arg)) (c))
    (viper-set-destructive-command
     (list 'viper-toggle-case val nil nil nil nil))
    (while (> val 0)
      (setq c (following-char))
      (delete-char 1 nil)
      (if (eq c (upcase c))
	  (insert-char (downcase c) 1)
	(insert-char (upcase c) 1))
      (if (eolp) (backward-char 1))
      (setq val (1- val)))))


;; query replace

(defun viper-query-replace ()
  "Query replace.
If a null string is supplied as the string to be replaced,
the query replace mode will toggle between string replace
and regexp replace."
  (interactive)
  (let (str)
    (setq str (viper-read-string-with-history
	       (if viper-re-query-replace "Query replace regexp: "
		 "Query replace: ")
	       nil  ; no initial
	       'viper-replace1-history
	       (car viper-replace1-history) ; default
	       ))
    (if (string= str "")
	(progn
	  (setq viper-re-query-replace (not viper-re-query-replace))
	  (message "Query replace mode changed to %s"
		   (if viper-re-query-replace "regexp replace"
		     "string replace")))
      (if viper-re-query-replace
	  (query-replace-regexp
	   str
	   (viper-read-string-with-history
	    (format "Query replace regexp `%s' with: " str)
	    nil  ; no initial
	    'viper-replace1-history
	    (car viper-replace1-history) ; default
	    ))
	(query-replace
	 str
	 (viper-read-string-with-history
	  (format "Query replace `%s' with: " str)
	  nil  ; no initial
	  'viper-replace1-history
	  (car viper-replace1-history) ; default
	  ))))))


;; marking

(defun viper-mark-beginning-of-buffer ()
  "Mark beginning of buffer."
  (interactive)
  (push-mark (point))
  (goto-char (point-min))
  (exchange-point-and-mark)
  (message "Mark set at the beginning of buffer"))

(defun viper-mark-end-of-buffer ()
  "Mark end of buffer."
  (interactive)
  (push-mark (point))
  (goto-char (point-max))
  (exchange-point-and-mark)
  (message "Mark set at the end of buffer"))

(defun viper-mark-point ()
  "Set mark at point of buffer."
  (interactive)
  (let ((char (read-char)))
    (cond ((and (<= ?a char) (<= char ?z))
	   (point-to-register (viper-int-to-char (1+ (- char ?a)))))
	  ((viper= char ?<) (viper-mark-beginning-of-buffer))
	  ((viper= char ?>) (viper-mark-end-of-buffer))
	  ((viper= char ?.) (viper-set-mark-if-necessary))
	  ((viper= char ?,) (viper-cycle-through-mark-ring))
	  ((viper= char ?^) (push-mark viper-saved-mark t t))
	  ((viper= char ?D) (mark-defun))
	  (t (error "Viper bell"))
	  )))

;; Algorithm: If first invocation of this command save mark on ring, goto
;; mark, M0, and pop the most recent elt from the mark ring into mark,
;; making it into the new mark, M1.
;; Push this mark back and set mark to the original point position, p1.
;; So, if you hit '' or `` then you can return to p1.
;;
;; If repeated command, pop top elt from the ring into mark and
;; jump there.  This forgets the position, p1, and puts M1 back into mark.
;; Then we save the current pos, which is M0, jump to M1 and pop M2 from
;; the ring into mark.  Push M2 back on the ring and set mark to M0.
;; etc.
(defun viper-cycle-through-mark-ring ()
  "Visit previous locations on the mark ring.
One can use `` and '' to temporarily jump 1 step back."
  (let* ((sv-pt (point)))
       ;; if repeated `m,' command, pop the previously saved mark.
       ;; Prev saved mark is actually prev saved point.  It is used if the
       ;; user types `` or '' and is discarded
       ;; from the mark ring by the next `m,' command.
       ;; In any case, go to the previous or previously saved mark.
       ;; Then push the current mark (popped off the ring) and set current
       ;; point to be the mark.  Current pt as mark is discarded by the next
       ;; m, command.
       (if (eq last-command 'viper-cycle-through-mark-ring)
	   ()
	 ;; save current mark if the first iteration
	 (setq mark-ring (delete (viper-mark-marker) mark-ring))
	 (if (mark t)
	     (push-mark (mark t) t)) )
       (pop-mark)
       (set-mark-command 1)
       ;; don't duplicate mark on the ring
       (setq mark-ring (delete (viper-mark-marker) mark-ring))
       (push-mark sv-pt t)
       (viper-deactivate-mark)
       (setq this-command 'viper-cycle-through-mark-ring)
       ))


(defun viper-goto-mark (arg)
  "Go to mark."
  (interactive "P")
  (let ((char (read-char))
	(com (viper-getcom arg)))
    (viper-goto-mark-subr char com nil)))

(defun viper-goto-mark-and-skip-white (arg)
  "Go to mark and skip to first non-white character on line."
  (interactive "P")
  (let ((char (read-char))
	(com (viper-getCom arg)))
    (viper-goto-mark-subr char com t)))

(defun viper-goto-mark-subr (char com skip-white)
  (if (eobp)
      (if (bobp)
	  (error "Empty buffer")
	(backward-char 1)))
  (cond ((viper-valid-register char '(letter))
	 (let* ((buff (current-buffer))
	        (reg (viper-int-to-char (1+ (- char ?a))))
	        (text-marker (get-register reg)))
	   ;; If marker points to file that had markers set (and those markers
	   ;; were saved (as e.g., in session.el), then restore those markers
	   (if (and (consp text-marker)
 		    (eq (car text-marker) 'file-query)
 		    (or (find-buffer-visiting (nth 1 text-marker))
 			(y-or-n-p (format "Visit file %s again? "
 					  (nth 1 text-marker)))))
 	       (save-excursion
 		 (find-file (nth 1 text-marker))
 		 (when (and (<= (nth 2 text-marker) (point-max))
 			    (<= (point-min) (nth 2 text-marker)))
 		   (setq text-marker (copy-marker (nth 2 text-marker)))
 		   (set-register reg text-marker))))
	   (if com (viper-move-marker-locally 'viper-com-point (point)))
	   (if (not (viper-valid-marker text-marker))
	       (error viper-EmptyTextmarker char))
	   (if (and (viper-same-line (point) viper-last-jump)
		    (= (point) viper-last-jump-ignore))
	       (push-mark viper-last-jump t)
	     (push-mark nil t)) ; no msg
	   (viper-register-to-point reg)
	   (setq viper-last-jump (point-marker))
	   (cond (skip-white
		  (back-to-indentation)
		  (setq viper-last-jump-ignore (point))))
	   (if com
	       (if (equal buff (current-buffer))
		   (viper-execute-com (if skip-white
					  'viper-goto-mark-and-skip-white
					'viper-goto-mark)
				    nil com)
		 (switch-to-buffer buff)
		 (goto-char viper-com-point)
		 (viper-change-state-to-vi)
		 (error "Viper bell")))))
	((and (not skip-white) (viper= char ?`))
	 (if com (viper-move-marker-locally 'viper-com-point (point)))
	 (if (and (viper-same-line (point) viper-last-jump)
		  (= (point) viper-last-jump-ignore))
	     (goto-char viper-last-jump))
	 (if (null (mark t)) (error "Mark is not set in this buffer"))
	 (if (= (point) (mark t)) (pop-mark))
	 (exchange-point-and-mark)
	 (setq viper-last-jump (point-marker)
	       viper-last-jump-ignore 0)
	 (if com (viper-execute-com 'viper-goto-mark nil com)))
	((and skip-white (viper= char ?'))
	 (if com (viper-move-marker-locally 'viper-com-point (point)))
	 (if (and (viper-same-line (point) viper-last-jump)
		  (= (point) viper-last-jump-ignore))
	     (goto-char viper-last-jump))
	 (if (= (point) (mark t)) (pop-mark))
	 (exchange-point-and-mark)
	 (setq viper-last-jump (point))
	 (back-to-indentation)
	 (setq viper-last-jump-ignore (point))
	 (if com (viper-execute-com 'viper-goto-mark-and-skip-white nil com)))
	(t (error viper-InvalidTextmarker char))))

(defun viper-insert-tab ()
  (interactive)
  (insert-tab))

(defun viper-exchange-point-and-mark ()
  (interactive)
  (exchange-point-and-mark)
  (back-to-indentation))

;; Input Mode Indentation

;; Returns t, if the string before point matches the regexp STR.
(defsubst viper-looking-back (str)
  (and (save-excursion (re-search-backward str nil t))
       (= (point) (match-end 0))))


(defun viper-forward-indent ()
  "Indent forward -- `C-t' in Vi."
  (interactive)
  (setq viper-cted t)
  (indent-to (+ (current-column) viper-shift-width)))

(defun viper-backward-indent ()
  "Backtab, `C-d' in Vi."
  (interactive)
  (if viper-cted
      (let ((p (point)) (c (current-column)) bol (indent t))
	(if (viper-looking-back "[0^]")
	    (progn
	      (if (eq ?^ (preceding-char))
		  (setq viper-preserve-indent t))
	      (delete-char -1)
	      (setq p (point))
	      (setq indent nil)))
	(setq bol (point-at-bol))
	(if (re-search-backward "[^ \t]" bol 1) (forward-char))
	(delete-region (point) p)
	(if indent
	    (indent-to (- c viper-shift-width)))
	(if (or (bolp) (viper-looking-back "[^ \t]"))
	    (setq viper-cted nil)))))

;; do smart indent
(defun viper-indent-line (col)
  (if viper-auto-indent
      (progn
	(setq viper-cted t)
	(if (and viper-electric-mode
		 (not (memq major-mode '(fundamental-mode
					 text-mode
					 paragraph-indent-text-mode))))
	    (indent-according-to-mode)
	  (indent-to col)))))


(defun viper-autoindent ()
  "Auto Indentation, Vi-style."
  (interactive)
  (let ((col (current-indentation)))
    (if abbrev-mode (expand-abbrev))
    (if viper-preserve-indent
	(setq viper-preserve-indent nil)
      (setq viper-current-indent col))
    ;; don't leave whitespace lines around
    (if (memq last-command
	      '(viper-autoindent
		viper-open-line viper-Open-line
		viper-replace-state-exit-cmd))
	(indent-to-left-margin))
    ;; use \n instead of newline, or else <Return> will move the insert point
    ;;(newline 1)
    (insert "\n")
    (viper-indent-line viper-current-indent)
    ))


;; Viewing registers

(defun viper-ket-function (arg)
  "Function called by \], the ket.  View registers and call \]\]."
  (interactive "P")
  (let ((reg (read-char)))
    (cond ((viper-valid-register reg '(letter Letter))
	   (view-register (downcase reg)))
	  ((viper-valid-register reg '(digit))
	   (let ((text (current-kill (- reg ?1) 'do-not-rotate)))
	     (with-output-to-temp-buffer " *viper-info*"
	       (princ (format "Register %c contains the string:\n" reg))
	       (princ text))
	     ))
	  ((viper= ?\] reg)
	   (viper-next-heading arg))
	  (t (error
	      viper-InvalidRegister reg)))))

(defun viper-brac-function (arg)
  "Function called by \[, the brac.  View textmarkers and call \[\[."
  (interactive "P")
  (let ((reg (read-char)))
    (cond ((viper= ?\[ reg)
	   (viper-prev-heading arg))
	  ((viper= ?\] reg)
	   (viper-heading-end arg))
	  ((viper-valid-register reg '(letter))
	   (let* ((val (get-register (viper-int-to-char (1+ (- reg ?a)))))
		  (buf (if (not (markerp val))
			   (error viper-EmptyTextmarker reg)
			 (marker-buffer val)))
		  (pos (marker-position val))
		  line-no text (s pos) (e pos))
	     (with-output-to-temp-buffer " *viper-info*"
	       (if (and buf pos)
		   (progn
		     (with-current-buffer buf
		       (setq line-no (1+ (count-lines (point-min) val)))
		       (goto-char pos)
		       (beginning-of-line)
		       (if (re-search-backward "[^ \t]" nil t)
			   (setq s (point-at-bol)))
		       (goto-char pos)
		       (forward-line 1)
		       (if (re-search-forward "[^ \t]" nil t)
			   (progn
			     (end-of-line)
			     (setq e (point))))
		       (setq text (buffer-substring s e))
		       (setq text (format "%s<%c>%s"
					  (substring text 0 (- pos s))
					  reg (substring text (- pos s)))))
		     (princ
		      (format
		       "Textmarker `%c' is in buffer `%s' at line %d.\n"
				     reg (buffer-name buf) line-no))
		     (princ (format "Here is some text around %c:\n\n %s"
				     reg text)))
		 (princ (format viper-EmptyTextmarker reg))))
	     ))
	  (t (error viper-InvalidTextmarker reg)))))



(defun viper-delete-backward-word (arg)
  "Delete previous word."
  (interactive "p")
  (save-excursion
    (push-mark nil t)
    (backward-word arg)
    (delete-region (point) (mark t))
    (pop-mark)))



;; Get viper standard value of SYMBOL.  If symbol is customized, get its
;; standard value.  Otherwise, get the value saved in the alist STORAGE.  If
;; STORAGE is nil, use viper-saved-user-settings.
(defun viper-standard-value (symbol &optional storage)
  (or (eval (car (get symbol 'customized-value)))
      (eval (car (get symbol 'saved-value)))
      (nth 1 (assoc symbol (or storage viper-saved-user-settings)))))



(defun viper-set-expert-level (&optional dont-change-unless)
  "Sets the expert level for a Viper user.
Can be called interactively to change (temporarily or permanently) the
current expert level.

The optional argument DONT-CHANGE-UNLESS, if not nil, says that
the level should not be changed, unless its current value is
meaningless (i.e., not one of 1,2,3,4,5).

User level determines the setting of Viper variables that are most
sensitive for VI-style look-and-feel."

  (interactive)

  (if (not (natnump viper-expert-level)) (setq viper-expert-level 0))

  (save-window-excursion
    (delete-other-windows)
    ;; if 0 < viper-expert-level < viper-max-expert-level
    ;;    & dont-change-unless = t -- use it; else ask
    (viper-ask-level dont-change-unless))

  (setq viper-always          	    		t
	viper-ex-style-motion 	    		t
	viper-ex-style-editing			t
	viper-want-ctl-h-help nil)

  (cond ((eq viper-expert-level 1) ; novice or beginner
	 (global-set-key   ; in emacs-state
	  viper-toggle-key
	  (if (viper-window-display-p) 'viper-iconify 'suspend-emacs))
	 (setq viper-no-multiple-ESC	       t
	       viper-re-search	    	       t
	       viper-vi-style-in-minibuffer    t
	       viper-search-wrap-around        t
	       viper-electric-mode	       nil
	       viper-want-emacs-keys-in-vi     nil
	       viper-want-emacs-keys-in-insert nil))

	((and (> viper-expert-level 1) (< viper-expert-level 5))
	 ;; intermediate to guru
	 (setq viper-no-multiple-ESC           (if (viper-window-display-p)
						   t 'twice)
	       viper-electric-mode	       t
	       viper-want-emacs-keys-in-vi     t
	       viper-want-emacs-keys-in-insert (> viper-expert-level 2))

	 (if (eq viper-expert-level 4)  ; respect user's ex-style motion
					; and viper-no-multiple-ESC
	     (progn
	       (setq-default
		viper-ex-style-editing
		(viper-standard-value 'viper-ex-style-editing)
		viper-ex-style-motion
		(viper-standard-value 'viper-ex-style-motion))
	       (setq viper-ex-style-motion
		     (viper-standard-value 'viper-ex-style-motion)
		     viper-ex-style-editing
		     (viper-standard-value 'viper-ex-style-editing)
		     viper-re-search
		     (viper-standard-value 'viper-re-search)
		     viper-no-multiple-ESC
		     (viper-standard-value 'viper-no-multiple-ESC)))))

	;; A wizard!!
	;; Ideally, if 5 is selected, a buffer should pop up to let the
	;; user toggle the values of variables.
	(t (setq-default viper-ex-style-editing
			 (viper-standard-value 'viper-ex-style-editing)
			 viper-ex-style-motion
			 (viper-standard-value 'viper-ex-style-motion))
	   (setq  viper-want-ctl-h-help
		  (viper-standard-value 'viper-want-ctl-h-help)
		  viper-always
		  (viper-standard-value 'viper-always)
		  viper-no-multiple-ESC
		  (viper-standard-value 'viper-no-multiple-ESC)
		  viper-ex-style-motion
		  (viper-standard-value 'viper-ex-style-motion)
		  viper-ex-style-editing
		  (viper-standard-value 'viper-ex-style-editing)
		  viper-re-search
		  (viper-standard-value 'viper-re-search)
		  viper-electric-mode
		  (viper-standard-value 'viper-electric-mode)
		  viper-want-emacs-keys-in-vi
		  (viper-standard-value 'viper-want-emacs-keys-in-vi)
		  viper-want-emacs-keys-in-insert
		  (viper-standard-value 'viper-want-emacs-keys-in-insert))))

  (viper-set-mode-vars-for viper-current-state)
  (if (or viper-always
	  (and (> viper-expert-level 0) (> 5 viper-expert-level)))
      (viper-set-hooks)))


;; Ask user expert level.
(defun viper-ask-level (dont-change-unless)
  (let ((ask-buffer " *viper-ask-level*")
	level-changed repeated)
    (save-window-excursion
      (switch-to-buffer ask-buffer)

      (while (or (> viper-expert-level viper-max-expert-level)
		 (< viper-expert-level 1)
		 (null dont-change-unless))
	(erase-buffer)
	(if repeated
	    (progn
	      (message "Invalid user level")
	      (beep 1))
	  (setq repeated t))
	(setq dont-change-unless t
	      level-changed t)
	(insert "
Please specify your level of familiarity with the venomous VI PERil
\(and the VI Plan for Emacs Rescue).
You can change it at any time by typing `M-x viper-set-expert-level RET'

 1 -- BEGINNER: Almost all Emacs features are suppressed.
       Feels almost like straight Vi.  File name completion and
       command history in the minibuffer are thrown in as a bonus.
       To use Emacs productively, you must reach level 3 or higher.
 2 -- MASTER: C-c now has its standard Emacs meaning in Vi command state,
       so most Emacs commands can be used when Viper is in Vi state.
       Good progress---you are well on the way to level 3!
 3 -- GRAND MASTER: Like 2, but most Emacs commands are available also
       in Viper's insert state.
 4 -- GURU: Like 3, but user settings are respected for viper-no-multiple-ESC,
       viper-ex-style-motion, viper-ex-style-editing, and
       viper-re-search variables.  Adjust these settings to your taste.
 5 -- WIZARD: Like 4, but user settings are also respected for viper-always,
       viper-electric-mode, viper-want-ctl-h-help, viper-want-emacs-keys-in-vi,
       and viper-want-emacs-keys-in-insert.  Adjust these to your taste.

Please, specify your level now: ")

	(setq viper-expert-level (- (viper-read-char-exclusive) ?0))
	) ; end while

      ;; tell the user if level was changed
      (and level-changed
	   (progn
	     (insert
	      (format "\n\n\n\n\n\t\tYou have selected user level %d"
		      viper-expert-level))
	     (if (y-or-n-p "Do you wish to make this change permanent? ")
		 ;; save the setting for viper-expert-level
		 (viper-save-setting
		  'viper-expert-level
		  (format "Saving user level %d ..." viper-expert-level)
		  viper-custom-file-name))
	     ))
      (bury-buffer) ; remove ask-buffer from screen
      (message "")
      )))


(defun viper-nil ()
  (interactive)
  (beep 1))


;; if ENFORCE-BUFFER is not nil, error if CHAR is a marker in another buffer
(defun viper-register-to-point (char &optional enforce-buffer)
  "Like `jump-to-register', but switches to another buffer in another window."
  (interactive "cViper register to point: ")
  (let ((val (get-register char)))
    (cond
     ((and (fboundp 'frame-configuration-p)
	   (frame-configuration-p val))
      (set-frame-configuration val))
     ((window-configuration-p val)
      (set-window-configuration val))
     ((viper-valid-marker val)
      (if (and enforce-buffer
	       (not (equal (current-buffer) (marker-buffer val))))
	  (error (concat viper-EmptyTextmarker " in this buffer")
		 (viper-int-to-char (1- (+ char ?a)))))
      (pop-to-buffer  (marker-buffer val))
      (goto-char val))
     ((and (consp val) (eq (car val) 'file))
      (find-file (cdr val)))
     (t
      (error viper-EmptyTextmarker (viper-int-to-char (1- (+ char ?a))))))))


(defun viper-save-kill-buffer ()
  "Save then kill current buffer."
  (interactive)
  (if (< viper-expert-level 2)
      (save-buffers-kill-emacs)
    (save-buffer)
    (kill-buffer (current-buffer))))



;;; Bug Report

(defun viper-submit-report ()
  "Submit bug report on Viper."
  (interactive)
  (let ((reporter-prompt-for-summary-p t)
	(viper-device-type (viper-device-type))
	color-display-p frame-parameters
	minibuffer-emacs-face minibuffer-vi-face minibuffer-insert-face
	varlist salutation window-config)

    ;; If mode info is needed, add variable to `let' and then set it below,
    ;; like we did with color-display-p.
    (setq color-display-p (if (viper-window-display-p)
			      (viper-color-display-p)
			    'non-x)
	  minibuffer-vi-face (if (viper-has-face-support-p)
				 (viper-get-face viper-minibuffer-vi-face)
			       'non-x)
	  minibuffer-insert-face (if (viper-has-face-support-p)
				     (viper-get-face
				      viper-minibuffer-insert-face)
				   'non-x)
	  minibuffer-emacs-face (if (viper-has-face-support-p)
				    (viper-get-face
				     viper-minibuffer-emacs-face)
				  'non-x)
	  frame-parameters (if (fboundp 'frame-parameters)
			       (frame-parameters (selected-frame))))

    (setq varlist (list 'viper-vi-minibuffer-minor-mode
		        'viper-insert-minibuffer-minor-mode
		        'viper-vi-intercept-minor-mode
		        'viper-vi-local-user-minor-mode
		        'viper-vi-kbd-minor-mode
		        'viper-vi-global-user-minor-mode
		        'viper-vi-state-modifier-minor-mode
		        'viper-vi-diehard-minor-mode
		        'viper-vi-basic-minor-mode
		        'viper-replace-minor-mode
		        'viper-insert-intercept-minor-mode
		        'viper-insert-local-user-minor-mode
		        'viper-insert-kbd-minor-mode
		        'viper-insert-global-user-minor-mode
		        'viper-insert-state-modifier-minor-mode
		        'viper-insert-diehard-minor-mode
		        'viper-insert-basic-minor-mode
		        'viper-emacs-intercept-minor-mode
		        'viper-emacs-local-user-minor-mode
		        'viper-emacs-kbd-minor-mode
		        'viper-emacs-global-user-minor-mode
		        'viper-emacs-state-modifier-minor-mode
		        'viper-automatic-iso-accents
			'viper-special-input-method
		        'viper-want-emacs-keys-in-insert
		        'viper-want-emacs-keys-in-vi
		        'viper-keep-point-on-undo
		        'viper-no-multiple-ESC
		        'viper-electric-mode
		        'viper-ESC-key
		        'viper-want-ctl-h-help
		        'viper-ex-style-editing
		        'viper-delete-backwards-in-replace
		        'viper-vi-style-in-minibuffer
		        'viper-vi-state-hook
		        'viper-insert-state-hook
		        'viper-replace-state-hook
		        'viper-emacs-state-hook
		        'ex-cycle-other-window
		        'ex-cycle-through-non-files
		        'viper-expert-level
		        'major-mode
		        'viper-device-type
			'color-display-p
			'frame-parameters
			'minibuffer-vi-face
			'minibuffer-insert-face
			'minibuffer-emacs-face
			))
	  (setq salutation "
Congratulations! You may have unearthed a bug in Viper!
Please mail a concise, accurate summary of the problem to the address above.

-------------------------------------------------------------------")
	  (setq window-config (current-window-configuration))
	  (with-output-to-temp-buffer " *viper-info*"
	    (switch-to-buffer " *viper-info*")
	    (delete-other-windows)
	    (princ "
PLEASE FOLLOW THESE PROCEDURES
------------------------------

Before reporting a bug, please verify that it is related to Viper, and is
not caused by other packages you are using.

Don't report compilation warnings, unless you are certain that there is a
problem.  These warnings are normal and unavoidable.

Please note that users should not modify variables and keymaps other than
those advertised in the manual.  Such `customization' is likely to crash
Viper, as it would any other improperly customized Emacs package.

If you are reporting an error message received while executing one of the
Viper commands, type:

    M-x set-variable <Return> debug-on-error <Return> t <Return>

Then reproduce the error.  The above command will cause Emacs to produce a
back trace of the execution that leads to the error.  Please include this
trace in your bug report.

If you believe that one of Viper's commands goes into an infinite loop
\(e.g., Emacs freezes\), type:

    M-x set-variable <Return> debug-on-quit <Return> t <Return>

Then reproduce the problem.  Wait for a few seconds, then type C-g to abort
the current command.  Include the resulting back trace in the bug report.

Mail anyway (y or n)? ")
	    (if (y-or-n-p "Mail anyway? ")
		()
	      (set-window-configuration window-config)
	      (error "Bug report aborted")))

	  (require 'reporter)
	  (set-window-configuration window-config)

	  (reporter-submit-bug-report "kifer@cs.stonybrook.edu"
				      (viper-version)
				      varlist
				      nil 'delete-other-windows
				      salutation)
	  ))




;;; viper-cmd.el ends here
